//! 構文木から CPS ノードのもとになる命令列を生成する処理

use super::{name_resolution::NLoopData, NName};
use crate::cps::*;
use crate::parse::*;
use crate::{
    front::NameResolution,
    logs::Logger,
    token::{HaveLocation, Location, TokenData, TokenKind},
};
use log::{error, trace};
use std::{
    iter::once,
    mem::{replace, take},
};

#[derive(Clone)]
struct KLoopData {
    break_label: KLabel,
    continue_label: KLabel,
}

/// Code generation context.
#[derive(Default)]
struct Gx {
    outlines: KOutlines,
    current_commands: Vec<KCommand>,
    current_locals: Vec<KLocalData>,
    current_loops: Vec<KLoopData>,
    current_labels: Vec<KLabelData>,
    fns: Vec<KFnData>,
    fn_loops: Vec<Vec<NLoopData>>,
    extern_fns: Vec<KExternFnData>,
    logger: Logger,
}

impl Gx {
    fn new(logger: Logger) -> Self {
        Self {
            logger,
            ..Self::default()
        }
    }

    fn fresh_symbol(&mut self, hint: &str, location: Location) -> KSymbol {
        let name = hint.to_string();
        let ty = KTy::default();

        let local = KLocal::new(self.current_locals.len());
        self.current_locals.push(KLocalData {
            name: name.clone(),
            ty: ty.clone(),
            is_alive: true,
        });

        KSymbol { local, location }
    }

    fn fresh_label(&mut self, hint: &str, _location: Location) -> KLabel {
        let name = hint.to_string();

        let id = self.current_labels.len();
        let label = KLabel::new(id);
        self.current_labels.push(KLabelData {
            name,
            params: vec![],
            body: KNode::default(),
        });
        label
    }

    fn push(&mut self, command: KCommand) {
        self.current_commands.push(command);
    }

    fn push_label(&mut self, label: KLabel, params: Vec<KSymbol>) {
        self.push(KCommand::Label { label, params })
    }

    // ちょうど1つの継続を持つプリミティブノードを生成する。
    fn push_prim_1(&mut self, prim: KPrim, args: Vec<KTerm>, result: KSymbol) {
        let location = result.location.clone();
        self.push(KCommand::Node {
            prim,
            tys: vec![],
            args,
            result_opt: Some(result),
            cont_count: 1,
            location,
        });
    }

    fn do_push_jump(
        &mut self,
        label: KLabel,
        args: impl IntoIterator<Item = KTerm>,
        cont_count: usize,
    ) {
        self.push(KCommand::Node {
            prim: KPrim::Jump,
            tys: vec![],
            args: once(KTerm::Label(label)).chain(args).collect(),
            result_opt: None,
            cont_count,
            // FIXME: location を持たせる
            location: Default::default(),
        });
    }

    fn push_jump(&mut self, label: KLabel, args: impl IntoIterator<Item = KTerm>) {
        self.do_push_jump(label, args, 0);
    }

    fn push_jump_with_cont(&mut self, label: KLabel, args: impl IntoIterator<Item = KTerm>) {
        self.do_push_jump(label, args, 1);
    }
}

fn new_false_term(location: Location) -> KTerm {
    KTerm::False(TokenData::new(
        TokenKind::False,
        "false".to_string(),
        location,
    ))
}

fn new_true_term(location: Location) -> KTerm {
    KTerm::True(TokenData::new(
        TokenKind::True,
        "true".to_string(),
        location,
    ))
}

fn new_unit_term(location: Location) -> KTerm {
    KTerm::Unit { location }
}

fn new_never_term(location: Location) -> KTerm {
    // FIXME: the type is ! (never)
    KTerm::Unit { location }
}

fn emit_unary_op(
    prim: KPrim,
    arg_opt: Option<Box<PExpr>>,
    location: Location,
    gx: &mut Gx,
) -> KTerm {
    let result = gx.fresh_symbol(&prim.hint_str(), location);

    let arg = gen_expr(*arg_opt.unwrap(), gx);

    gx.push_prim_1(prim, vec![arg], result.clone());

    KTerm::Name(result)
}

fn emit_compound_assign(
    prim: KPrim,
    left: PExpr,
    right_opt: Option<Box<PExpr>>,
    location: Location,
    gx: &mut Gx,
) -> KTerm {
    let left = gen_expr_lval(left, KMut::Mut, location.clone(), gx);
    let right = gen_expr(*right_opt.unwrap(), gx);

    gx.push(KCommand::Node {
        prim,
        tys: vec![],
        args: vec![left, right],
        result_opt: None,
        cont_count: 1,
        location: location.clone(),
    });

    new_unit_term(location)
}

fn emit_binary_op(
    prim: KPrim,
    left: PExpr,
    right_opt: Option<Box<PExpr>>,
    location: Location,
    gx: &mut Gx,
) -> KTerm {
    let result = gx.fresh_symbol(&prim.hint_str(), location);

    let left = gen_expr(left, gx);
    let right = gen_expr(*right_opt.unwrap(), gx);

    gx.push_prim_1(prim, vec![left, right], result.clone());

    KTerm::Name(result)
}

fn emit_if(
    cond: PExpr,
    gen_body: impl FnOnce(&mut Gx) -> KTerm,
    gen_alt: impl FnOnce(&mut Gx) -> KTerm,
    location: Location,
    gx: &mut Gx,
) -> KTerm {
    let result = gx.fresh_symbol("if_result", location.clone());
    let next_label = gx.fresh_label("next", location.clone());

    let k_cond = gen_expr(cond, gx);

    gx.push(KCommand::Node {
        prim: KPrim::If,
        tys: vec![],
        args: vec![k_cond],
        result_opt: None,
        cont_count: 2,
        location,
    });

    // body
    {
        let body = gen_body(gx);
        gx.push_jump(next_label, vec![body]);
    }

    // alt
    {
        let alt = gen_alt(gx);
        gx.push_jump(next_label, vec![alt]);
    }

    gx.push_label(next_label, vec![result.clone()]);

    KTerm::Name(result)
}

fn gen_mut(p_mut: Option<&PMut>, _gx: &mut Gx) -> KMut {
    match p_mut {
        Some(&(k_mut, _)) => k_mut,
        None => KMut::Const,
    }
}

fn gen_ty_name(ty_name: PNameTy, _gx: &mut Gx) -> KTy {
    let mut name = ty_name.0;
    let n_name = take(&mut name.info_opt).unwrap();

    match n_name {
        NName::I32 => KTy::I32,
        NName::I64 => KTy::I64,
        NName::Usize => KTy::Usize,
        NName::F64 => KTy::F64,
        NName::C8 => KTy::C8,
        NName::Bool => KTy::Bool,
        NName::Enum(enum_id) => KTy::Enum(KEnum::new(enum_id)),
        NName::Struct(struct_id) => KTy::Struct(KStruct::new(struct_id)),
        _ => unreachable!("expected type name but {:?}", name),
    }
}

fn gen_ty(ty: PTy, gx: &mut Gx) -> KTy {
    match ty {
        PTy::Name(name) => gen_ty_name(name, gx),
        PTy::Never(_) => KTy::Never,
        PTy::Unit(_) => KTy::Unit,
        PTy::Ptr(PPtrTy {
            ty_opt, mut_opt, ..
        }) => {
            let k_mut = gen_mut(mut_opt.as_ref(), gx);
            gen_ty(*ty_opt.unwrap(), gx).into_ptr(k_mut)
        }
    }
}

fn gen_name(mut name: PName, gx: &mut Gx) -> KSymbolExt {
    let n_name = take(&mut name.info_opt).unwrap();
    let (name, location) = name.decompose();

    match n_name {
        NName::LocalVar(local_var_id) => {
            assert!(local_var_id < gx.current_locals.len());
            let local = KLocal::new(local_var_id);

            let local_data = &mut gx.current_locals[local_var_id];
            local_data.name = name;
            local_data.is_alive = true;

            KSymbolExt::Symbol(KSymbol { local, location })
        }
        NName::Fn(fn_id) => {
            assert!(fn_id < gx.fns.len());
            KSymbolExt::Fn(KFn::new(fn_id))
        }
        NName::ExternFn(extern_fn_id) => {
            assert!(extern_fn_id < gx.extern_fns.len());
            KSymbolExt::ExternFn(KExternFn::new(extern_fn_id))
        }
        NName::Const(const_id) => {
            assert!(const_id < gx.outlines.consts.len());
            KSymbolExt::Const(KConst::new(const_id))
        }
        NName::StaticVar(static_var_id) => {
            assert!(static_var_id < gx.outlines.static_vars.len());
            KSymbolExt::StaticVar(KStaticVar::new(static_var_id))
        }
        NName::Struct(struct_id)
            if KStruct::new(struct_id)
                .fields(&gx.outlines.structs)
                .is_empty() =>
        {
            KSymbolExt::UnitLikeStruct {
                k_struct: KStruct::new(struct_id),
                location,
            }
        }
        NName::I32
        | NName::I64
        | NName::Usize
        | NName::F64
        | NName::C8
        | NName::Bool
        | NName::Enum(_)
        | NName::Struct(_) => {
            gx.logger.error(&location, "型の名前です。");
            // FIXME: 適切にハンドル？
            KSymbolExt::Symbol(gx.fresh_symbol(&name, location))
        }
        NName::Unresolved => {
            // Unresolved ならエラーなのでコード生成には来ないはず。
            unreachable!("{:?}", (&name, &location, n_name))
        }
    }
}

fn gen_param(param: PParam, gx: &mut Gx) -> KSymbol {
    let ty = match param.ty_opt {
        Some(ty) => gen_ty(ty, gx),
        None => {
            gx.logger.error(&param.name, "param type is mandatory");
            KTy::Never
        }
    };

    let symbol = gen_name(param.name, gx).expect_symbol();

    let old_ty = replace(&mut gx.current_locals[symbol.local.id()].ty, ty);
    assert!(old_ty.is_unresolved());

    symbol
}

struct GenFnSigResult {
    fn_name: String,
    params: Vec<KSymbol>,
    param_tys: Vec<KTy>,
    result_ty: KTy,
}

fn gen_fn_sig(
    n_name: NName,
    name_opt: Option<PName>,
    param_list_opt: Option<PParamList>,
    result_ty_opt: Option<PTy>,
    gx: &mut Gx,
) -> GenFnSigResult {
    let fn_name = {
        let mut name = name_opt.unwrap();

        // assert
        match (n_name, take(&mut name.info_opt)) {
            (NName::Fn(expected), Some(NName::Fn(actual)))
            | (NName::ExternFn(expected), Some(NName::ExternFn(actual))) => {
                assert_eq!(expected, actual);
            }
            _ => unreachable!(),
        }

        name.token.into_text()
    };
    let params = param_list_opt
        .unwrap()
        .params
        .into_iter()
        .map(|param| gen_param(param, gx))
        .collect::<Vec<_>>();
    let param_tys = params
        .iter()
        .map(|param| param.ty(&gx.current_locals))
        .collect();
    let result_ty = match result_ty_opt {
        Some(ty) => gen_ty(ty, gx),
        None => KTy::Unit,
    };
    GenFnSigResult {
        fn_name,
        params,
        param_tys,
        result_ty,
    }
}

fn gen_constant(expr: PExpr, gx: &mut Gx) -> Option<KConstValue> {
    fn strip_suffix<'a>(s: &'a str, suffix: &'static str) -> Option<&'a str> {
        if s.ends_with(suffix) {
            Some(&s[..s.len() - suffix.len()])
        } else {
            None
        }
    }

    match expr {
        PExpr::Int(PIntExpr { token }) => {
            let text = &token.into_text().replace("_", "");
            strip_suffix(text, "i32")
                .and_then(|text| text.parse::<i32>().ok().map(KConstValue::I32))
                .or_else(|| {
                    strip_suffix(text, "i64")
                        .and_then(|text| text.parse::<i64>().ok().map(KConstValue::I64))
                })
                .or_else(|| {
                    strip_suffix(text, "usize")
                        .and_then(|text| text.parse::<usize>().ok().map(KConstValue::Usize))
                })
                .or_else(|| text.parse::<i32>().ok().map(KConstValue::I32))
        }
        PExpr::Float(PFloatExpr { token }) => token
            .into_text()
            .replace("_", "")
            .parse::<f64>()
            .ok()
            .map(KConstValue::F64),
        PExpr::True(_) => Some(KConstValue::Bool(true)),
        PExpr::False(_) => Some(KConstValue::Bool(false)),
        PExpr::Name(PNameExpr(name)) => match gen_name(name, gx) {
            KSymbolExt::Const(k_const) => k_const.value_opt(&gx.outlines.consts).cloned(),
            _ => None,
        },
        _ => unimplemented!(),
    }
}

fn gen_const_variant(
    decl: PConstVariantDecl,
    enum_ty_opt: Option<KTy>,
    value_slot: &mut usize,
    gx: &mut Gx,
) -> KConst {
    let PConstVariantDecl {
        mut name,
        value_opt,
        ..
    } = decl;

    let (name, k_const) = {
        let k_const = match take(&mut name.info_opt) {
            Some(NName::Const(const_id)) => KConst::new(const_id),
            _ => unreachable!(),
        };
        let (name, _) = name.decompose();
        (name, k_const)
    };

    if let Some(value) = value_opt {
        let location = value.location();
        match gen_constant(*value, gx) {
            Some(value) => *value_slot = value.cast_as_usize(),
            None => gx.logger.error(&location, "invalid constant expression"),
        }
    }

    gx.outlines.consts[k_const.id()] = KConstData {
        name,
        ty: enum_ty_opt.unwrap_or(KTy::Usize),
        value_opt: Some(KConstValue::Usize(*value_slot)),
    };
    k_const
}

fn gen_record_variant(decl: PRecordVariantDecl, enum_ty_opt: Option<KTy>, gx: &mut Gx) -> KStruct {
    let PRecordVariantDecl {
        mut name, fields, ..
    } = decl;

    let k_struct = match take(&mut name.info_opt) {
        Some(NName::Struct(struct_id)) => KStruct::new(struct_id),
        n_name_opt => unreachable!("{:?}", n_name_opt),
    };
    let (name, location) = name.decompose();

    let fields = fields
        .into_iter()
        .map(|field| {
            let k_field = KField::new(field.field_id_opt.unwrap());
            let (name, location) = field.name.decompose();
            let field_ty = field.ty_opt.map_or(KTy::Unresolved, |ty| gen_ty(ty, gx));
            gx.outlines.fields[k_field.id()] = KFieldOutline {
                name,
                ty: field_ty,
                location,
            };

            k_field
        })
        .collect();

    gx.outlines.structs[k_struct.id()] = KStructOutline {
        name,
        ty: enum_ty_opt.unwrap_or(KTy::Struct(k_struct)),
        fields,
        location,
    };
    k_struct
}

fn gen_variant(
    decl: PVariantDecl,
    enum_ty_opt: Option<KTy>,
    value_slot: &mut usize,
    gx: &mut Gx,
) -> KVariant {
    match decl {
        PVariantDecl::Const(decl) => {
            KVariant::Const(gen_const_variant(decl, enum_ty_opt, value_slot, gx))
        }
        PVariantDecl::Record(decl) => KVariant::Record(gen_record_variant(decl, enum_ty_opt, gx)),
    }
}

/// 式を左辺値とみなして変換する。(結果として、ポインタ型の項を期待している。)
fn gen_expr_lval(expr: PExpr, k_mut: KMut, location: Location, gx: &mut Gx) -> KTerm {
    match expr {
        PExpr::Tuple(PTupleExpr { mut arg_list }) if !arg_list.is_tuple() => {
            let args = take(&mut arg_list.args);
            let mut arg = args.into_iter().next().unwrap();
            gen_expr_lval(take(&mut arg.expr), k_mut, location, gx)
        }
        PExpr::UnaryOp(PUnaryOpExpr {
            op: PUnaryOp::Deref,
            arg_opt: Some(arg),
            ..
        }) => {
            // &*x ==> x
            gen_expr(*arg, gx)
        }
        PExpr::DotField(PDotFieldExpr { left, name_opt, .. }) => {
            // x.foo は `&(&x)->foo` のような形にコンパイルする。
            let (_, name, location) = name_opt.unwrap().decompose();
            let result = gx.fresh_symbol(&format!("{}_ptr", name), location.clone());

            let left = gen_expr_lval(*left, k_mut, location.clone(), gx);
            let field = KTerm::FieldTag(KFieldTag { name, location });

            let get_field_prim = match k_mut {
                KMut::Const => KPrim::GetField,
                KMut::Mut => KPrim::GetFieldMut,
            };
            gx.push_prim_1(get_field_prim, vec![left, field], result.clone());

            KTerm::Name(result)
        }
        expr => {
            let prim = match k_mut {
                KMut::Const => KPrim::Ref,
                KMut::Mut => KPrim::RefMut,
            };
            emit_unary_op(prim, Some(Box::new(expr)), location, gx)
        }
    }
}

fn gen_expr(expr: PExpr, gx: &mut Gx) -> KTerm {
    match expr {
        PExpr::Int(PIntExpr { token }) => KTerm::Int(token, KTy::Unresolved),
        PExpr::Float(PFloatExpr { token }) => KTerm::Float(token),
        PExpr::Char(PCharExpr { token }) => KTerm::Char(token),
        PExpr::Str(PStrExpr { token }) => KTerm::Str(token),
        PExpr::True(PTrueExpr(token)) => KTerm::True(token),
        PExpr::False(PFalseExpr(token)) => KTerm::False(token),
        PExpr::Name(PNameExpr(name)) => match gen_name(name, gx) {
            KSymbolExt::Symbol(symbol) => KTerm::Name(symbol),
            KSymbolExt::Const(k_const) => KTerm::Const(k_const),
            KSymbolExt::StaticVar(static_var) => KTerm::StaticVar(static_var),
            KSymbolExt::Fn(k_fn) => KTerm::Fn(k_fn),
            KSymbolExt::ExternFn(extern_fn) => KTerm::ExternFn(extern_fn),
            KSymbolExt::UnitLikeStruct { k_struct, location } => {
                let ty = KTy::Struct(k_struct);
                let name = k_struct.name(&gx.outlines.structs).to_string();
                let result = gx.fresh_symbol(&name, location.clone());
                gx.push(KCommand::Node {
                    prim: KPrim::Record,
                    tys: vec![ty],
                    args: vec![],
                    result_opt: Some(result.clone()),
                    cont_count: 1,
                    location,
                });
                KTerm::Name(result)
            }
        },
        PExpr::Record(PRecordExpr {
            mut name,
            left_brace,
            mut fields,
            ..
        }) => {
            let k_struct = match take(&mut name.0.info_opt) {
                Some(NName::Const(_)) => todo!(),
                Some(NName::Struct(struct_id)) => KStruct::new(struct_id),
                n_name => unreachable!("{:?}", n_name),
            };

            let (name, location) = name.0.decompose();
            let result = gx.fresh_symbol(&name, location.clone());

            let field_count = k_struct.fields(&gx.outlines.structs).len();
            let mut args = vec![KTerm::default(); field_count];
            let mut arg_freq = vec![0_u8; field_count];

            for field in &mut fields {
                let term = gen_expr(field.value_opt.take().unwrap(), gx);

                match (0..field_count).find(|&i| {
                    field.name.text()
                        == k_struct.fields(&gx.outlines.structs)[i].name(&gx.outlines.fields)
                }) {
                    Some(i) => {
                        arg_freq[i] += 1;
                        args[i] = term;
                    }
                    None => gx.logger.error(
                        field,
                        format!("not field of {}", k_struct.name(&gx.outlines.structs)),
                    ),
                }
            }

            // フィールドへの割り当ての過不足を計算する。
            let mut missed = vec![];
            let mut duped = vec![];
            for (freq, k_field) in arg_freq.iter().zip(k_struct.fields(&gx.outlines.structs)) {
                match freq {
                    0 => missed.push(k_field.name(&gx.outlines.fields)),
                    1 => {}
                    _ => duped.push(k_field.name(&gx.outlines.fields)),
                }
            }

            if !duped.is_empty() {
                for field_expr in &fields {
                    if duped.contains(&field_expr.name.text()) {
                        gx.logger.error(field_expr, "duplicated");
                    }
                }
            }

            if !missed.is_empty() {
                gx.logger.error(
                    &left_brace,
                    format!("missed some fields: '{}'", missed.join("', '")),
                );
            }

            gx.push(KCommand::Node {
                prim: KPrim::Record,
                tys: vec![KTy::Struct(k_struct)],
                args,
                result_opt: Some(result.clone()),
                cont_count: 1,
                location,
            });

            KTerm::Name(result)
        }
        PExpr::Tuple(PTupleExpr { mut arg_list }) => {
            let is_tuple = arg_list.is_tuple();
            match arg_list.args.as_mut_slice() {
                [] => {
                    let location = arg_list.location();
                    new_unit_term(location)
                }
                [arg] if !is_tuple => gen_expr(take(&mut arg.expr), gx),
                _ => unimplemented!("tuple literal is not supported yet"),
            }
        }
        PExpr::DotField(..) => {
            let (_, text, location) = match &expr {
                PExpr::DotField(PDotFieldExpr {
                    name_opt: Some(name),
                    ..
                }) => name.clone().decompose(),
                _ => unimplemented!(),
            };

            // FIXME: location
            let result1 = gen_expr_lval(expr, KMut::Const, location.clone(), gx);
            let result2 = gx.fresh_symbol(&text, location);

            gx.push_prim_1(KPrim::Deref, vec![result1], result2.clone());

            KTerm::Name(result2)
        }
        PExpr::Call(PCallExpr { left, arg_list }) => {
            let location = arg_list.left_paren.into_location();
            let result = gx.fresh_symbol("call_result", location.clone());

            let k_left = gen_expr(*left, gx);

            let mut k_args = vec![k_left];
            for p_arg in arg_list.args {
                let k_arg = gen_expr(p_arg.expr, gx);
                k_args.push(k_arg);
            }

            gx.push_prim_1(KPrim::CallDirect, k_args, result.clone());

            KTerm::Name(result)
        }
        PExpr::Index(PIndexExpr { left, arg_list }) => {
            // a[i] ==> *(a + i)

            let location = arg_list.left_paren.into_location();
            let indexed_ptr = gx.fresh_symbol("indexed_ptr", location.clone());
            let result = gx.fresh_symbol("index_result", location.clone());

            if arg_list.args.len() != 1 {
                gx.logger.error(
                    &location,
                    "zero or multiple arguments of indexing is unimplemented",
                );
                return new_never_term(location);
            }

            let k_left = gen_expr(*left, gx);
            let k_arg = gen_expr(arg_list.args.into_iter().next().unwrap().expr, gx);
            gx.push_prim_1(KPrim::Add, vec![k_left, k_arg], indexed_ptr.clone());
            gx.push_prim_1(KPrim::Deref, vec![KTerm::Name(indexed_ptr)], result.clone());

            KTerm::Name(result)
        }
        PExpr::As(PAsExpr {
            left,
            keyword,
            ty_opt,
            ..
        }) => {
            let location = keyword.location();
            let result = gx.fresh_symbol("cast", location.clone());

            let left = gen_expr(*left, gx);
            let ty = gen_ty(ty_opt.unwrap(), gx);

            gx.push(KCommand::Node {
                prim: KPrim::Cast,
                tys: vec![ty],
                args: vec![left],
                result_opt: Some(result.clone()),
                cont_count: 1,
                location,
            });

            KTerm::Name(result)
        }
        PExpr::UnaryOp(PUnaryOpExpr {
            op,
            mut_opt,
            arg_opt,
            location,
        }) => match op {
            PUnaryOp::Deref => emit_unary_op(KPrim::Deref, arg_opt, location, gx),
            PUnaryOp::Ref => {
                let k_mut = gen_mut(mut_opt.as_ref(), gx);
                gen_expr_lval(*arg_opt.unwrap(), k_mut, location, gx)
            }
            PUnaryOp::Minus => emit_unary_op(KPrim::Minus, arg_opt, location, gx),
            PUnaryOp::Not => emit_unary_op(KPrim::Not, arg_opt, location, gx),
        },
        PExpr::BinaryOp(PBinaryOpExpr {
            op,
            left,
            right_opt,
            location,
        }) => match op {
            PBinaryOp::Assign => {
                let left = gen_expr_lval(*left, KMut::Mut, location.clone(), gx);
                let right = gen_expr(*right_opt.unwrap(), gx);

                gx.push(KCommand::Node {
                    prim: KPrim::Assign,
                    tys: vec![],
                    args: vec![left, right],
                    result_opt: None,
                    cont_count: 1,
                    location: location.clone(),
                });

                new_unit_term(location)
            }
            PBinaryOp::AddAssign => {
                emit_compound_assign(KPrim::AddAssign, *left, right_opt, location, gx)
            }
            PBinaryOp::SubAssign => {
                emit_compound_assign(KPrim::SubAssign, *left, right_opt, location, gx)
            }
            PBinaryOp::MulAssign => {
                emit_compound_assign(KPrim::MulAssign, *left, right_opt, location, gx)
            }
            PBinaryOp::DivAssign => {
                emit_compound_assign(KPrim::DivAssign, *left, right_opt, location, gx)
            }
            PBinaryOp::ModuloAssign => {
                emit_compound_assign(KPrim::ModuloAssign, *left, right_opt, location, gx)
            }
            PBinaryOp::BitAndAssign => {
                emit_compound_assign(KPrim::BitAndAssign, *left, right_opt, location, gx)
            }
            PBinaryOp::BitOrAssign => {
                emit_compound_assign(KPrim::BitOrAssign, *left, right_opt, location, gx)
            }
            PBinaryOp::BitXorAssign => {
                emit_compound_assign(KPrim::BitXorAssign, *left, right_opt, location, gx)
            }
            PBinaryOp::LeftShiftAssign => {
                emit_compound_assign(KPrim::LeftShiftAssign, *left, right_opt, location, gx)
            }
            PBinaryOp::RightShiftAssign => {
                emit_compound_assign(KPrim::RightShiftAssign, *left, right_opt, location, gx)
            }
            PBinaryOp::Add => emit_binary_op(KPrim::Add, *left, right_opt, location, gx),
            PBinaryOp::Sub => emit_binary_op(KPrim::Sub, *left, right_opt, location, gx),
            PBinaryOp::Mul => emit_binary_op(KPrim::Mul, *left, right_opt, location, gx),
            PBinaryOp::Div => emit_binary_op(KPrim::Div, *left, right_opt, location, gx),
            PBinaryOp::Modulo => emit_binary_op(KPrim::Modulo, *left, right_opt, location, gx),
            PBinaryOp::BitAnd => emit_binary_op(KPrim::BitAnd, *left, right_opt, location, gx),
            PBinaryOp::BitOr => emit_binary_op(KPrim::BitOr, *left, right_opt, location, gx),
            PBinaryOp::BitXor => emit_binary_op(KPrim::BitXor, *left, right_opt, location, gx),
            PBinaryOp::LeftShift => {
                emit_binary_op(KPrim::LeftShift, *left, right_opt, location, gx)
            }
            PBinaryOp::RightShift => {
                emit_binary_op(KPrim::RightShift, *left, right_opt, location, gx)
            }
            PBinaryOp::Eq => emit_binary_op(KPrim::Eq, *left, right_opt, location, gx),
            PBinaryOp::Ne => emit_binary_op(KPrim::Ne, *left, right_opt, location, gx),
            PBinaryOp::Lt => emit_binary_op(KPrim::Lt, *left, right_opt, location, gx),
            PBinaryOp::Le => emit_binary_op(KPrim::Le, *left, right_opt, location, gx),
            PBinaryOp::Gt => emit_binary_op(KPrim::Gt, *left, right_opt, location, gx),
            PBinaryOp::Ge => emit_binary_op(KPrim::Ge, *left, right_opt, location, gx),
            PBinaryOp::LogAnd => {
                let false_term = new_false_term(location.clone());
                emit_if(
                    *left,
                    |gx| gen_expr(*right_opt.unwrap(), gx),
                    move |_| false_term,
                    location,
                    gx,
                )
            }
            PBinaryOp::LogOr => {
                let true_term = new_true_term(location.clone());
                emit_if(
                    *left,
                    move |_| true_term,
                    |gx| gen_expr(*right_opt.unwrap(), gx),
                    location,
                    gx,
                )
            }
        },
        PExpr::Pipe(PPipeExpr {
            left: arg,
            pipe,
            right_opt,
        }) => match right_opt.map(|b| *b) {
            Some(PExpr::Call(PCallExpr { left, arg_list })) => {
                // FIXME: call expr の生成と共通化
                let result = {
                    let location = pipe.into_location();
                    gx.fresh_symbol("call_result", location.clone())
                };

                let k_arg = gen_expr(*arg, gx);
                let k_left = gen_expr(*left, gx);

                let mut k_args = vec![k_left, k_arg];
                for p_arg in arg_list.args {
                    let k_arg = gen_expr(p_arg.expr, gx);
                    k_args.push(k_arg);
                }

                gx.push_prim_1(KPrim::CallDirect, k_args, result.clone());
                KTerm::Name(result)
            }
            _ => unimplemented!(),
        },
        PExpr::Block(PBlockExpr(block)) => gen_block(block, gx),
        PExpr::Break(PBreakExpr {
            keyword,
            arg_opt,
            loop_id_opt,
        }) => {
            let location = keyword.into_location();

            let label = gx.current_loops[loop_id_opt.unwrap()].break_label;
            let arg = match arg_opt {
                Some(arg) => gen_expr(*arg, gx),
                None => new_unit_term(location.clone()),
            };
            gx.push_jump_with_cont(label, vec![arg]);

            new_never_term(location)
        }
        PExpr::Continue(PContinueExpr {
            keyword,
            loop_id_opt,
        }) => {
            let location = keyword.into_location();

            let label = gx.current_loops[loop_id_opt.unwrap()].continue_label;
            gx.push_jump_with_cont(label, vec![]);

            new_never_term(location)
        }
        PExpr::Return(PReturnExpr {
            keyword,
            arg_opt,
            fn_id_opt,
        }) => {
            let location = keyword.into_location();
            let k_fn = KFn::new(fn_id_opt.unwrap());

            let args = {
                let return_term = KTerm::Return(k_fn);
                let arg = match arg_opt {
                    Some(arg) => gen_expr(*arg, gx),
                    None => new_unit_term(location.clone()),
                };
                vec![return_term, arg]
            };
            gx.push(KCommand::Node {
                prim: KPrim::Jump,
                tys: vec![],
                args,
                result_opt: None,
                cont_count: 1,
                location: location.clone(),
            });

            new_never_term(location)
        }
        PExpr::If(PIfExpr {
            keyword,
            cond_opt,
            body_opt,
            alt_opt,
            ..
        }) => {
            let location = keyword.into_location();
            let location1 = location.clone();
            emit_if(
                *cond_opt.unwrap(),
                |gx| gen_block(body_opt.unwrap(), gx),
                move |gx| match alt_opt {
                    Some(alt) => gen_expr(*alt, gx),
                    None => new_unit_term(location),
                },
                location1,
                gx,
            )
        }
        PExpr::Match(PMatchExpr {
            keyword,
            cond_opt,
            arms,
            ..
        }) => {
            let location = keyword.into_location();
            let k_cond = gen_expr(*cond_opt.unwrap(), gx);
            if arms.is_empty() {
                return new_never_term(location).clone();
            }

            let result = gx.fresh_symbol("match_result", location.clone());
            let next_label = gx.fresh_label("match_next", location.clone());

            let args = once(k_cond.clone())
                .chain(arms.iter().map(|arm| match &arm.pat {
                    PPat::Name(name) => match name.info_opt.as_ref().unwrap() {
                        NName::Const(const_id) => KTerm::Const(KConst::new(*const_id)),
                        NName::LocalVar(_) => {
                            let symbol = gen_name(name.clone(), gx).expect_symbol();
                            KTerm::Name(symbol)
                        }
                        _ => {
                            error!("unimplemented pat {:?}", arm);
                            KTerm::Unit {
                                location: arm.location().clone(),
                            }
                        }
                    },
                    PPat::Record(PRecordPat { name, .. }) => {
                        let k_struct = name
                            .info_opt
                            .unwrap()
                            .as_struct()
                            .map(KStruct::new)
                            .unwrap();
                        KTerm::RecordTag(k_struct)
                    }
                }))
                .collect();
            let cont_count = arms.len();

            gx.push(KCommand::Node {
                prim: KPrim::Switch,
                tys: vec![],
                args,
                result_opt: None,
                cont_count,
                location,
            });

            for arm in arms {
                match arm.pat {
                    PPat::Name(name) => match name.info_opt.as_ref().unwrap() {
                        NName::Const(_) => {}
                        NName::LocalVar(_) => {
                            let name = gen_name(name.clone(), gx).expect_symbol();
                            gx.push_prim_1(KPrim::Let, vec![k_cond.clone()], name);
                        }
                        _ => error!("unimplemented pat {:?}", name),
                    },
                    PPat::Record(_) => {}
                }

                let body = gen_expr(*arm.body_opt.unwrap(), gx);
                gx.push_jump(next_label, vec![body]);
            }

            gx.push_label(next_label, vec![result.clone()]);
            KTerm::Name(result)
        }
        PExpr::While(PWhileExpr {
            keyword,
            cond_opt,
            body_opt,
            loop_id_opt,
            ..
        }) => {
            let location = keyword.into_location();
            let result = gx.fresh_symbol("while_result", location.clone());
            let unit_term = new_unit_term(location.clone());
            let KLoopData {
                break_label: next_label,
                continue_label,
            } = gx.current_loops[loop_id_opt.unwrap()].clone();

            gx.push_jump(continue_label, vec![]);

            gx.push_label(continue_label, vec![]);

            let k_cond = gen_expr(*cond_opt.unwrap(), gx);

            gx.push(KCommand::Node {
                prim: KPrim::If,
                tys: vec![],
                args: vec![k_cond],
                result_opt: None,
                cont_count: 2,
                location,
            });

            // body:
            gen_block(body_opt.unwrap(), gx);

            gx.push_jump(continue_label, vec![]);

            // alt:
            gx.push_jump(next_label, vec![unit_term.clone()]);

            // next:
            gx.push_label(next_label, vec![result.clone()]);

            unit_term
        }
        PExpr::Loop(PLoopExpr {
            keyword,
            body_opt,
            loop_id_opt,
        }) => {
            let location = keyword.into_location();
            let result = gx.fresh_symbol("loop_result", location.clone());
            let KLoopData {
                break_label: next_label,
                continue_label,
            } = gx.current_loops[loop_id_opt.unwrap()].clone();

            gx.push_jump(continue_label.clone(), vec![]);

            gx.push_label(continue_label.clone(), vec![]);

            // body:
            gen_block(body_opt.unwrap(), gx);

            gx.push_jump(continue_label.clone(), vec![]);

            // next:
            gx.push_label(next_label, vec![result.clone()]);

            KTerm::Name(result)
        }
    }
}

fn gen_decl(decl: PDecl, gx: &mut Gx) {
    match decl {
        PDecl::Expr(PExprDecl { expr, .. }) => {
            gen_expr(expr, gx);
        }
        PDecl::Let(PLetDecl {
            name_opt,
            ty_opt,
            init_opt,
            ..
        }) => {
            let ty = ty_opt.map_or(KTy::Unresolved, |ty| gen_ty(ty, gx));

            let k_init = gen_expr(init_opt.unwrap(), gx);
            let mut result = gen_name(name_opt.unwrap(), gx).expect_symbol();
            *result.ty_mut(&mut gx.current_locals) = ty;

            gx.push_prim_1(KPrim::Let, vec![k_init], result);
        }
        PDecl::Const(PConstDecl {
            keyword,
            name_opt,
            ty_opt,
            init_opt,
            ..
        }) => {
            let (name, k_const) = {
                let mut name = name_opt.unwrap();
                let k_const = match take(&mut name.info_opt) {
                    Some(NName::Const(const_id)) => KConst::new(const_id),
                    _ => unreachable!(),
                };
                let (name, _) = name.decompose();
                (name, k_const)
            };
            let ty = gen_ty(ty_opt.unwrap(), gx);

            if !ty.is_primitive() {
                gx.logger
                    .error(&keyword, "constant must be of primitive type");
            }

            let init = init_opt.unwrap();
            let init_location = init.location();
            let value_opt = match gen_constant(init, gx) {
                Some(value) => Some(value),
                None => {
                    gx.logger
                        .error(&init_location, "invalid constant expression");
                    None
                }
            };

            gx.outlines.consts[k_const.id()] = KConstData {
                name,
                ty,
                value_opt,
            };
        }
        PDecl::Static(PStaticDecl {
            keyword,
            name_opt,
            ty_opt,
            init_opt,
            ..
        }) => {
            let (name, static_var) = {
                let mut name = name_opt.unwrap();
                let static_var = match take(&mut name.info_opt) {
                    Some(NName::StaticVar(static_var_id)) => KStaticVar::new(static_var_id),
                    _ => unreachable!(),
                };
                let (name, _) = name.decompose();
                (name, static_var)
            };
            let ty = gen_ty(ty_opt.unwrap(), gx);

            if !ty.is_primitive() {
                gx.logger
                    .error(&keyword, "static var must be of primitive type");
            }

            let init = init_opt.unwrap();
            let init_location = init.location();
            let value_opt = match gen_constant(init, gx) {
                Some(value) => Some(value),
                None => {
                    gx.logger
                        .error(&init_location, "invalid constant expression");
                    None
                }
            };

            gx.outlines.static_vars[static_var.id()] = KStaticVarData {
                name,
                ty,
                value_opt,
            };
        }

        PDecl::Fn(PFnDecl {
            vis_opt,
            keyword,
            name_opt,
            param_list_opt,
            result_ty_opt,
            block_opt,
            fn_id_opt,
            ..
        }) => {
            let location = keyword.into_location();
            let k_fn = KFn::new(fn_id_opt.unwrap());

            let vis_opt = vis_opt.map(|(vis, _)| vis);

            let parent_local_vars = {
                let local_vars = take(&mut gx.fns[k_fn.id()].locals);
                replace(&mut gx.current_locals, local_vars)
            };

            let GenFnSigResult {
                fn_name,
                params,
                param_tys,
                result_ty,
            } = gen_fn_sig(
                NName::Fn(k_fn.id()),
                name_opt,
                param_list_opt,
                result_ty_opt,
                gx,
            );

            let (commands, labels) = {
                let parent_commands = take(&mut gx.current_commands);
                let parent_labels = take(&mut gx.current_labels);
                let parent_loops = take(&mut gx.current_loops);

                gx.current_loops = take(&mut gx.fn_loops[k_fn.id()])
                    .into_iter()
                    .map(|loop_data| {
                        let location = loop_data.location;
                        let continue_label = gx.fresh_label("continue_", location.clone());
                        let break_label = gx.fresh_label("next", location);
                        KLoopData {
                            break_label,
                            continue_label,
                        }
                    })
                    .collect();

                let result = gen_block(block_opt.unwrap(), gx);
                gx.push(KCommand::Node {
                    prim: KPrim::Jump,
                    tys: vec![],
                    args: vec![KTerm::Return(k_fn), result],
                    result_opt: None,
                    cont_count: 1,
                    location: location.clone(),
                });

                let commands = replace(&mut gx.current_commands, parent_commands);
                let labels = replace(&mut gx.current_labels, parent_labels);
                gx.current_loops = parent_loops;
                (commands, labels)
            };

            let (body, labels) = fold_block(commands, labels);

            let locals = replace(&mut gx.current_locals, parent_local_vars);

            gx.outlines.fns[k_fn.id()] = KFnOutline {
                name: fn_name,
                vis_opt,
                param_tys,
                result_ty,
                location,
            };
            gx.fns[k_fn.id()] = KFnData {
                params,
                body,
                locals,
                labels,
                label_sigs: vec![],
                ty_env: Default::default(),
            };
        }
        PDecl::ExternFn(PExternFnDecl {
            extern_keyword,
            fn_keyword,
            name_opt,
            param_list_opt,
            result_ty_opt,
            extern_fn_id_opt,
            ..
        }) => {
            let extern_fn = KExternFn::new(extern_fn_id_opt.unwrap());

            let parent_local_vars = {
                let local_vars = take(&mut gx.extern_fns[extern_fn.id()].locals);
                replace(&mut gx.current_locals, local_vars)
            };

            let location = extern_keyword.location().unite(&fn_keyword.location());
            let GenFnSigResult {
                fn_name,
                params,
                param_tys,
                result_ty,
            } = gen_fn_sig(
                NName::ExternFn(extern_fn.id()),
                name_opt,
                param_list_opt,
                result_ty_opt,
                gx,
            );

            let locals = replace(&mut gx.current_locals, parent_local_vars);

            gx.outlines.extern_fns[extern_fn.id()] = KExternFnOutline {
                name: fn_name,
                param_tys,
                result_ty,
                location,
            };
            gx.extern_fns[extern_fn.id()] = KExternFnData { params, locals };
        }
        PDecl::Enum(PEnumDecl {
            name_opt, variants, ..
        }) => {
            let mut name = name_opt.unwrap();
            let k_enum = match take(&mut name.info_opt) {
                Some(NName::Enum(enum_id)) => KEnum::new(enum_id),
                n_name_opt => unreachable!("{:?}", n_name_opt),
            };
            let (name, location) = name.decompose();

            let mut next_value = 0_usize;
            let k_variants = variants
                .into_iter()
                .map(|variant_decl| {
                    let k_variant =
                        gen_variant(variant_decl, Some(KTy::Enum(k_enum)), &mut next_value, gx);
                    next_value += 1;
                    k_variant
                })
                .collect::<Vec<_>>();

            let repr = KEnumRepr::determine(&k_variants, &gx.outlines.consts);

            gx.outlines.enums[k_enum.id()] = KEnumOutline {
                name,
                variants: k_variants,
                repr,
                location,
            };
        }
        PDecl::Struct(PStructDecl { variant_opt, .. }) => {
            if let Some(variant) = variant_opt {
                gen_variant(variant, None, &mut 0, gx);
            }
        }
    }
}

fn gen_block(block: PBlock, gx: &mut Gx) -> KTerm {
    for decl in block.decls {
        gen_decl(decl, gx);
    }

    match block.last_opt {
        Some(last) => gen_expr(*last, gx),
        None => {
            let location = block.left_brace.into_location();
            new_unit_term(location)
        }
    }
}

fn gen_root(root: PRoot, gx: &mut Gx) {
    for decl in root.decls {
        gen_decl(decl, gx);
    }
}

pub(crate) fn cps_conversion(
    p_root: PRoot,
    name_resolution: NameResolution,
    logger: Logger,
) -> KRoot {
    let k_root = {
        let const_count = name_resolution.consts.len();

        let static_var_count = name_resolution.static_vars.len();

        // 関数の ID, 名前ID の対応表を構築する。
        let mut n_fns = name_resolution.fns;
        let fn_count = n_fns.len();
        let fn_loops = n_fns
            .iter_mut()
            .map(|fn_data| take(&mut fn_data.loops))
            .collect::<Vec<_>>();
        let fns = n_fns
            .iter()
            .map(|fn_data| KFnData {
                locals: vec![KLocalData::default(); fn_data.local_vars.len()],
                ..KFnData::default()
            })
            .collect();

        // 外部関数の ID, 名前ID の対応表を構築する。
        let n_extern_fns = name_resolution.extern_fns;
        let extern_fn_count = n_extern_fns.len();
        let k_extern_fns = n_extern_fns
            .iter()
            .map(|extern_fn_data| KExternFnData {
                locals: vec![KLocalData::default(); extern_fn_data.local_vars.len()],
                ..KExternFnData::default()
            })
            .collect();

        let enum_count = name_resolution.enums.len();

        let struct_count = name_resolution.structs.len();

        let field_count = name_resolution.fields.len();

        let mut gx = Gx::new(logger.clone());
        gx.outlines
            .consts
            .resize_with(const_count, Default::default);

        gx.outlines
            .static_vars
            .resize_with(static_var_count, Default::default);

        gx.fns = fns;
        gx.fn_loops = fn_loops;
        gx.outlines.fns.resize_with(fn_count, Default::default);

        gx.extern_fns = k_extern_fns;
        gx.outlines
            .extern_fns
            .resize_with(extern_fn_count, Default::default);

        gx.outlines.enums.resize_with(enum_count, Default::default);

        gx.outlines
            .structs
            .resize_with(struct_count, Default::default);

        gx.outlines
            .fields
            .resize_with(field_count, Default::default);

        gen_root(p_root, &mut gx);

        KRoot {
            outlines: gx.outlines,
            extern_fns: gx.extern_fns,
            fns: gx.fns,
        }
    };

    trace!("k_root (untyped) = {:#?}\n", k_root);

    k_root
}
