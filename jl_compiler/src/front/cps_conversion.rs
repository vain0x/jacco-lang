//! 構文木から CPS ノードのもとになる命令列を生成する処理

use super::{
    name_resolution::{NLoop, NLoopArena, NLoopTag},
    NName,
};
use crate::cps::*;
use crate::parse::*;
use crate::{
    front::NameResolution,
    logs::Logger,
    token::{HaveLocation, Location, TokenData, TokenKind, TokenSource},
    utils::VecArena,
};
use log::{error, trace};
use std::{
    iter::once,
    mem::{replace, take},
    rc::Rc,
};

#[derive(Clone)]
struct KLoopData {
    break_label: KLabel,
    continue_label: KLabel,
}

/// Code generation context.
#[derive(Default)]
struct Gx {
    tokens: Rc<PTokens>,
    outlines: KOutlines,
    current_commands: Vec<KCommand>,
    current_locals: KLocalArena,
    current_loops: VecArena<NLoopTag, KLoopData>,
    current_labels: KLabelArena,
    fns: KFnArena,
    fn_loops: VecArena<KFnTag, NLoopArena>,
    extern_fns: KExternFnArena,
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
        let local = self.current_locals.alloc(KLocalData {
            name: hint.to_string(),
            ty: KTy::Unresolved,
            location,
            is_alive: true,
        });

        KSymbol { local, location }
    }

    fn fresh_label(&mut self, hint: &str, _location: Location) -> KLabel {
        let name = hint.to_string();

        self.current_labels.alloc(KLabelData {
            name,
            params: vec![],
            body: KNode::default(),
        })
    }

    fn push(&mut self, command: KCommand) {
        self.current_commands.push(command);
    }

    fn push_label(&mut self, label: KLabel, params: Vec<KSymbol>) {
        self.push(KCommand::Label { label, params })
    }

    // ちょうど1つの継続を持つプリミティブノードを生成する。
    fn push_prim_1(&mut self, prim: KPrim, args: Vec<KTerm>, result: KSymbol) {
        let location = result.location;
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
        // FIXME: location を持たせる
        let location = Location::new(TokenSource::Special("<do_push_jump>"), Default::default());

        self.push(KCommand::Node {
            prim: KPrim::Jump,
            tys: vec![],
            args: once(KTerm::Label { label, location }).chain(args).collect(),
            result_opt: None,
            cont_count,
            location,
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

fn emit_unary_op(prim: KPrim, arg_opt: Option<&PExpr>, location: Location, gx: &mut Gx) -> KTerm {
    let result = gx.fresh_symbol(&prim.hint_str(), location);

    let arg = gen_expr(arg_opt.unwrap(), gx);

    gx.push_prim_1(prim, vec![arg], result.clone());

    KTerm::Name(result)
}

fn emit_compound_assign(
    prim: KPrim,
    left: &PExpr,
    right_opt: Option<&PExpr>,
    location: Location,
    gx: &mut Gx,
) -> KTerm {
    let left = gen_expr_lval(left, KMut::Mut, location, gx);
    let right = gen_expr(right_opt.unwrap(), gx);

    gx.push(KCommand::Node {
        prim,
        tys: vec![],
        args: vec![left, right],
        result_opt: None,
        cont_count: 1,
        location,
    });

    new_unit_term(location)
}

fn emit_binary_op(
    prim: KPrim,
    left: &PExpr,
    right_opt: Option<&PExpr>,
    location: Location,
    gx: &mut Gx,
) -> KTerm {
    let result = gx.fresh_symbol(&prim.hint_str(), location);

    let left = gen_expr(left, gx);
    let right = gen_expr(right_opt.unwrap(), gx);

    gx.push_prim_1(prim, vec![left, right], result.clone());

    KTerm::Name(result)
}

fn emit_if(
    cond: &PExpr,
    gen_body: impl FnOnce(&mut Gx) -> KTerm,
    gen_alt: impl FnOnce(&mut Gx) -> KTerm,
    location: Location,
    gx: &mut Gx,
) -> KTerm {
    let result = gx.fresh_symbol("if_result", location);
    let next_label = gx.fresh_label("next", location);

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

fn gen_mut(p_mut: Option<&PMut>) -> KMut {
    match p_mut {
        Some(&(k_mut, _)) => k_mut,
        None => KMut::Const,
    }
}

// ローカル型環境を引数に取るかもしれない
pub(crate) fn gen_ty_name(p_name: &PName) -> KTy {
    let n_name = p_name.info_opt.clone().unwrap();

    match n_name {
        NName::I8 => KTy::I8,
        NName::I16 => KTy::I16,
        NName::I32 => KTy::I32,
        NName::I64 => KTy::I64,
        NName::Isize => KTy::Isize,
        NName::U8 => KTy::U8,
        NName::U16 => KTy::U16,
        NName::U32 => KTy::U32,
        NName::U64 => KTy::U64,
        NName::Usize => KTy::Usize,
        NName::F32 => KTy::F32,
        NName::F64 => KTy::F64,
        NName::C8 => KTy::C8,
        NName::C16 => KTy::C16,
        NName::C32 => KTy::C32,
        NName::Bool => KTy::Bool,
        NName::Enum(n_enum) => KTy::Enum(n_enum),
        NName::Struct(n_struct) => KTy::Struct(n_struct),
        _ => unreachable!("expected type name but {:?}", p_name),
    }
}

pub(crate) fn gen_ty(ty: &PTy) -> KTy {
    match ty {
        PTy::Name(name) => gen_ty_name(name),
        PTy::Never(_) => KTy::Never,
        PTy::Unit(_) => KTy::Unit,
        PTy::Ptr(PPtrTy {
            ty_opt, mut_opt, ..
        }) => {
            let k_mut = gen_mut(mut_opt.as_ref());
            gen_ty(ty_opt.as_deref().unwrap()).into_ptr(k_mut)
        }
    }
}

fn gen_name(name: &PName, gx: &mut Gx) -> KSymbolExt {
    let n_name = name.info_opt.unwrap();
    let (name, location) = (name.text(&gx.tokens).to_string(), name.location());

    match n_name {
        NName::LocalVar(local) => {
            let local_data = &mut gx.current_locals[local];
            local_data.name = name.to_string();
            local_data.is_alive = true;

            KSymbolExt::Symbol(KSymbol { local, location })
        }
        NName::Fn(k_fn) => KSymbolExt::Fn(k_fn),
        NName::ExternFn(extern_fn) => KSymbolExt::ExternFn(extern_fn),
        NName::Const(n_const) => KSymbolExt::Const(n_const),
        NName::StaticVar(n_static_var) => KSymbolExt::StaticVar(n_static_var),
        NName::Struct(n_struct) if n_struct.fields(&gx.outlines.structs).is_empty() => {
            KSymbolExt::UnitLikeStruct {
                k_struct: n_struct,
                location,
            }
        }
        NName::I8
        | NName::I16
        | NName::I32
        | NName::I64
        | NName::Isize
        | NName::U8
        | NName::U16
        | NName::U32
        | NName::U64
        | NName::Usize
        | NName::F32
        | NName::F64
        | NName::C8
        | NName::C16
        | NName::C32
        | NName::Bool
        | NName::Enum(_)
        | NName::Struct(_) => {
            gx.logger.error(location, "型の名前です。");
            // FIXME: 適切にハンドル？
            KSymbolExt::Symbol(gx.fresh_symbol(&name, location))
        }
        NName::Unresolved => {
            // Unresolved ならエラーなのでコード生成には来ないはず。
            unreachable!("{:?}", (&name, location, n_name))
        }
    }
}

fn gen_constant(expr: &PExpr, gx: &mut Gx) -> Option<KConstValue> {
    fn strip_suffix<'a>(s: &'a str, suffix: &'static str) -> Option<&'a str> {
        if s.ends_with(suffix) {
            Some(&s[..s.len() - suffix.len()])
        } else {
            None
        }
    }

    match expr {
        PExpr::Int(PIntExpr { token }) => {
            let text = &token.text(&gx.tokens).replace("_", "");
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
            .text(&gx.tokens)
            .replace("_", "")
            .parse::<f64>()
            .ok()
            .map(KConstValue::F64),
        PExpr::True(_) => Some(KConstValue::Bool(true)),
        PExpr::False(_) => Some(KConstValue::Bool(false)),
        PExpr::Name(name) => match gen_name(name, gx) {
            KSymbolExt::Const(k_const) => k_const.of(&gx.outlines.consts).value_opt.clone(),
            _ => None,
        },
        _ => {
            // FIXME: 実装
            gx.logger.error(expr, "unimplemented");
            None
        }
    }
}

fn gen_const_variant(decl: &PConstVariantDecl, value_slot: &mut usize, gx: &mut Gx) -> KConst {
    let PConstVariantDecl {
        name, value_opt, ..
    } = decl;

    let k_const = match name.info_opt {
        Some(NName::Const(n_const)) => n_const,
        _ => unreachable!(),
    };
    assert_eq!(name.text(&gx.tokens), k_const.of(&gx.outlines.consts).name);

    if let Some(value) = value_opt.as_deref() {
        let location = value.location();
        match gen_constant(value, gx) {
            Some(value) => {
                // FIXME: 値を設定できるのはすべてのバリアントが const なときだけ
                *value_slot = value.cast_as_usize()
            }
            None => gx.logger.error(location, "invalid constant expression"),
        }
    }

    k_const.of_mut(&mut gx.outlines.consts).value_opt = Some(KConstValue::Usize(*value_slot));

    k_const
}

fn gen_record_variant(decl: &PRecordVariantDecl, gx: &mut Gx) -> KStruct {
    let PRecordVariantDecl { name, .. } = decl;

    let k_struct = match name.info_opt {
        Some(NName::Struct(n_struct)) => n_struct,
        n_name_opt => unreachable!("{:?}", n_name_opt),
    };
    assert_eq!(name.text(&gx.tokens), k_struct.name(&gx.outlines.structs));

    k_struct
}

fn gen_variant(decl: &PVariantDecl, value_slot: &mut usize, gx: &mut Gx) -> KVariant {
    match decl {
        PVariantDecl::Const(decl) => KVariant::Const(gen_const_variant(decl, value_slot, gx)),
        PVariantDecl::Record(decl) => KVariant::Record(gen_record_variant(decl, gx)),
    }
}

/// 式を左辺値とみなして変換する。(結果として、ポインタ型の項を期待している。)
fn gen_expr_lval(expr: &PExpr, k_mut: KMut, location: Location, gx: &mut Gx) -> KTerm {
    match expr {
        PExpr::Tuple(PTupleExpr { arg_list }) if !arg_list.is_tuple() => {
            let arg = &arg_list.args[0];
            gen_expr_lval(&arg.expr, k_mut, location, gx)
        }
        PExpr::UnaryOp(PUnaryOpExpr {
            op: PUnaryOp::Deref,
            arg_opt: Some(arg),
            ..
        }) => {
            // &*x ==> x
            gen_expr(arg.as_ref(), gx)
        }
        PExpr::DotField(PDotFieldExpr { left, name_opt, .. }) => {
            // x.foo は `&(&x)->foo` のような形にコンパイルする。
            let (name, location) = name_opt
                .as_ref()
                .map(|name| (name.text(&gx.tokens).to_string(), name.location(&gx.tokens)))
                .unwrap();
            let result = gx.fresh_symbol(&format!("{}_ptr", name), location);

            let left = gen_expr_lval(left.as_ref(), k_mut, location, gx);
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
            emit_unary_op(prim, Some(expr), location, gx)
        }
    }
}

fn gen_expr(expr: &PExpr, gx: &mut Gx) -> KTerm {
    match expr {
        PExpr::Int(PIntExpr { token }) => KTerm::Int(token.get(&gx.tokens), KTy::Unresolved),
        PExpr::Float(PFloatExpr { token }) => KTerm::Float(token.get(&gx.tokens)),
        PExpr::Char(PCharExpr { token }) => KTerm::Char(token.get(&gx.tokens)),
        PExpr::Str(PStrExpr { token }) => KTerm::Str(token.get(&gx.tokens)),
        PExpr::True(PTrueExpr(token)) => KTerm::True(token.get(&gx.tokens)),
        PExpr::False(PFalseExpr(token)) => KTerm::False(token.get(&gx.tokens)),
        PExpr::Name(name) => {
            let location = name.location();
            match gen_name(name, gx) {
                KSymbolExt::Symbol(symbol) => KTerm::Name(symbol),
                KSymbolExt::Const(k_const) => KTerm::Const { k_const, location },
                KSymbolExt::StaticVar(static_var) => KTerm::StaticVar {
                    static_var,
                    location,
                },
                KSymbolExt::Fn(k_fn) => KTerm::Fn { k_fn, location },
                KSymbolExt::ExternFn(extern_fn) => KTerm::ExternFn {
                    extern_fn,
                    location,
                },
                KSymbolExt::UnitLikeStruct { k_struct, location } => {
                    let ty = KTy::Struct(k_struct);
                    let name = k_struct.name(&gx.outlines.structs).to_string();
                    let result = gx.fresh_symbol(&name, location);
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
            }
        }
        PExpr::Record(PRecordExpr {
            name,
            left_brace,
            fields,
            ..
        }) => {
            let k_struct = match name.info_opt {
                Some(NName::Const(_)) => todo!(),
                Some(NName::Struct(n_struct)) => n_struct,
                n_name => unreachable!("{:?}", n_name),
            };

            let (name, location) = (name.text(&gx.tokens).to_string(), name.location());
            let result = gx.fresh_symbol(&name, location);

            let field_count = k_struct.fields(&gx.outlines.structs).len();
            let mut args = vec![KTerm::default(); field_count];
            let mut arg_freq = vec![0_u8; field_count];

            for field in fields {
                let term = gen_expr(field.value_opt.as_ref().unwrap(), gx);

                match (0..field_count).find(|&i| {
                    field.name.text(&gx.tokens).to_string()
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
                for field_expr in fields {
                    if duped.contains(&field_expr.name.text(&gx.tokens)) {
                        gx.logger.error(field_expr, "duplicated");
                    }
                }
            }

            if !missed.is_empty() {
                gx.logger.error(
                    left_brace.location(&gx.tokens),
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
        PExpr::Tuple(PTupleExpr { arg_list }) => match arg_list.args.as_slice() {
            [] => new_unit_term(arg_list.location()),
            [arg] if !arg_list.is_tuple() => gen_expr(&arg.expr, gx),
            _ => {
                gx.logger.error(arg_list, "tuple literal is unimplemented");
                new_never_term(arg_list.location())
            }
        },
        PExpr::DotField(..) => {
            let (text, location) = match expr {
                PExpr::DotField(PDotFieldExpr {
                    name_opt: Some(name),
                    ..
                }) => (name.text(&gx.tokens).to_string(), name.location(&gx.tokens)),
                _ => unreachable!(),
            };

            // FIXME: Location
            let result1 = gen_expr_lval(expr, KMut::Const, location, gx);
            let result2 = gx.fresh_symbol(&text, location);

            gx.push_prim_1(KPrim::Deref, vec![result1], result2.clone());

            KTerm::Name(result2)
        }
        PExpr::Call(PCallExpr { left, arg_list }) => {
            let location = arg_list.left_paren.location(&gx.tokens);
            let result = gx.fresh_symbol("call_result", location);

            let k_left = gen_expr(&left, gx);

            let mut k_args = vec![k_left];
            for p_arg in &arg_list.args {
                let k_arg = gen_expr(&p_arg.expr, gx);
                k_args.push(k_arg);
            }

            gx.push_prim_1(KPrim::CallDirect, k_args, result.clone());

            KTerm::Name(result)
        }
        PExpr::Index(PIndexExpr { left, arg_list }) => {
            // a[i] ==> *(a + i)

            let location = arg_list.left_paren.location(&gx.tokens);
            let indexed_ptr = gx.fresh_symbol("indexed_ptr", location);
            let result = gx.fresh_symbol("index_result", location);

            if arg_list.args.len() != 1 {
                gx.logger.error(
                    location,
                    "zero or multiple arguments of indexing is unimplemented",
                );
                return new_never_term(location);
            }

            let k_left = gen_expr(&left, gx);
            let k_arg = gen_expr(&arg_list.args[0].expr, gx);
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
            let location = keyword.location(&gx.tokens);
            let result = gx.fresh_symbol("cast", location);

            let left = gen_expr(&left, gx);
            let ty = gen_ty(ty_opt.as_ref().unwrap());

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
            PUnaryOp::Deref => emit_unary_op(KPrim::Deref, arg_opt.as_deref(), *location, gx),
            PUnaryOp::DerefDeref => {
                let deref = emit_unary_op(KPrim::Deref, arg_opt.as_deref(), *location, gx);

                let result = gx.fresh_symbol("deref", *location);
                gx.push_prim_1(KPrim::Deref, vec![deref], result.clone());
                KTerm::Name(result)
            }
            PUnaryOp::Ref => {
                let k_mut = gen_mut(mut_opt.as_ref());
                gen_expr_lval(arg_opt.as_ref().unwrap(), k_mut, *location, gx)
            }
            PUnaryOp::Minus => emit_unary_op(KPrim::Minus, arg_opt.as_deref(), *location, gx),
            PUnaryOp::Not => emit_unary_op(KPrim::Not, arg_opt.as_deref(), *location, gx),
        },
        PExpr::BinaryOp(PBinaryOpExpr {
            op,
            left,
            right_opt,
            location,
        }) => match op {
            PBinaryOp::Assign => {
                let left = gen_expr_lval(&left, KMut::Mut, *location, gx);
                let right = gen_expr(right_opt.as_ref().unwrap(), gx);

                gx.push(KCommand::Node {
                    prim: KPrim::Assign,
                    tys: vec![],
                    args: vec![left, right],
                    result_opt: None,
                    cont_count: 1,
                    location: *location,
                });

                new_unit_term(*location)
            }
            PBinaryOp::AddAssign => {
                emit_compound_assign(KPrim::AddAssign, left, right_opt.as_deref(), *location, gx)
            }
            PBinaryOp::SubAssign => {
                emit_compound_assign(KPrim::SubAssign, left, right_opt.as_deref(), *location, gx)
            }
            PBinaryOp::MulAssign => {
                emit_compound_assign(KPrim::MulAssign, left, right_opt.as_deref(), *location, gx)
            }
            PBinaryOp::DivAssign => {
                emit_compound_assign(KPrim::DivAssign, left, right_opt.as_deref(), *location, gx)
            }
            PBinaryOp::ModuloAssign => emit_compound_assign(
                KPrim::ModuloAssign,
                left,
                right_opt.as_deref(),
                *location,
                gx,
            ),
            PBinaryOp::BitAndAssign => emit_compound_assign(
                KPrim::BitAndAssign,
                left,
                right_opt.as_deref(),
                *location,
                gx,
            ),
            PBinaryOp::BitOrAssign => emit_compound_assign(
                KPrim::BitOrAssign,
                left,
                right_opt.as_deref(),
                *location,
                gx,
            ),
            PBinaryOp::BitXorAssign => emit_compound_assign(
                KPrim::BitXorAssign,
                left,
                right_opt.as_deref(),
                *location,
                gx,
            ),
            PBinaryOp::LeftShiftAssign => emit_compound_assign(
                KPrim::LeftShiftAssign,
                left,
                right_opt.as_deref(),
                *location,
                gx,
            ),
            PBinaryOp::RightShiftAssign => emit_compound_assign(
                KPrim::RightShiftAssign,
                left,
                right_opt.as_deref(),
                *location,
                gx,
            ),
            PBinaryOp::Add => emit_binary_op(KPrim::Add, left, right_opt.as_deref(), *location, gx),
            PBinaryOp::Sub => emit_binary_op(KPrim::Sub, left, right_opt.as_deref(), *location, gx),
            PBinaryOp::Mul => emit_binary_op(KPrim::Mul, left, right_opt.as_deref(), *location, gx),
            PBinaryOp::Div => emit_binary_op(KPrim::Div, left, right_opt.as_deref(), *location, gx),
            PBinaryOp::Modulo => {
                emit_binary_op(KPrim::Modulo, left, right_opt.as_deref(), *location, gx)
            }
            PBinaryOp::BitAnd => {
                emit_binary_op(KPrim::BitAnd, left, right_opt.as_deref(), *location, gx)
            }
            PBinaryOp::BitOr => {
                emit_binary_op(KPrim::BitOr, left, right_opt.as_deref(), *location, gx)
            }
            PBinaryOp::BitXor => {
                emit_binary_op(KPrim::BitXor, left, right_opt.as_deref(), *location, gx)
            }
            PBinaryOp::LeftShift => {
                emit_binary_op(KPrim::LeftShift, left, right_opt.as_deref(), *location, gx)
            }
            PBinaryOp::RightShift => {
                emit_binary_op(KPrim::RightShift, left, right_opt.as_deref(), *location, gx)
            }
            PBinaryOp::Equal => {
                emit_binary_op(KPrim::Equal, left, right_opt.as_deref(), *location, gx)
            }
            PBinaryOp::NotEqual => {
                emit_binary_op(KPrim::NotEqual, left, right_opt.as_deref(), *location, gx)
            }
            PBinaryOp::LessThan => {
                emit_binary_op(KPrim::LessThan, left, right_opt.as_deref(), *location, gx)
            }
            PBinaryOp::LessEqual => {
                emit_binary_op(KPrim::LessEqual, left, right_opt.as_deref(), *location, gx)
            }
            PBinaryOp::GreaterThan => emit_binary_op(
                KPrim::GreaterThan,
                left,
                right_opt.as_deref(),
                *location,
                gx,
            ),
            PBinaryOp::GreaterEqual => emit_binary_op(
                KPrim::GreaterEqual,
                left,
                right_opt.as_deref(),
                *location,
                gx,
            ),
            PBinaryOp::LogAnd => {
                let false_term = new_false_term(*location);
                emit_if(
                    &left,
                    |gx| gen_expr(right_opt.as_deref().unwrap(), gx),
                    move |_| false_term,
                    *location,
                    gx,
                )
            }
            PBinaryOp::LogOr => {
                let true_term = new_true_term(*location);
                emit_if(
                    &left,
                    move |_| true_term,
                    |gx| gen_expr(right_opt.as_deref().unwrap(), gx),
                    *location,
                    gx,
                )
            }
        },
        PExpr::Pipe(PPipeExpr {
            left: arg,
            pipe,
            right_opt,
        }) => match right_opt.as_deref() {
            Some(PExpr::Call(PCallExpr { left, arg_list })) => {
                // FIXME: call expr の生成と共通化
                let result = {
                    let location = pipe.location(&gx.tokens);
                    gx.fresh_symbol("call_result", location)
                };

                let k_arg = gen_expr(&arg, gx);
                let k_left = gen_expr(&left, gx);

                let mut k_args = vec![k_left, k_arg];
                for p_arg in &arg_list.args {
                    let k_arg = gen_expr(&p_arg.expr, gx);
                    k_args.push(k_arg);
                }

                gx.push_prim_1(KPrim::CallDirect, k_args, result.clone());
                KTerm::Name(result)
            }
            _ => {
                let location = pipe.location(&gx.tokens);
                gx.logger
                    .error(location, "|> の右辺は関数呼び出しでなければいけません");
                new_never_term(location)
            }
        },
        PExpr::Block(PBlockExpr(block)) => gen_block(block, gx),
        PExpr::Break(PBreakExpr {
            keyword,
            arg_opt,
            loop_id_opt,
        }) => {
            let location = keyword.location(&gx.tokens);
            let n_loop = NLoop::from_index(loop_id_opt.unwrap());

            let label = gx.current_loops[n_loop].break_label;
            let arg = match arg_opt {
                Some(arg) => gen_expr(&arg, gx),
                None => new_unit_term(location),
            };
            gx.push_jump_with_cont(label, vec![arg]);

            new_never_term(location)
        }
        PExpr::Continue(PContinueExpr {
            keyword,
            loop_id_opt,
        }) => {
            let location = keyword.location(&gx.tokens);
            let n_loop = NLoop::from_index(loop_id_opt.unwrap());

            let label = gx.current_loops[n_loop].continue_label;
            gx.push_jump_with_cont(label, vec![]);

            new_never_term(location)
        }
        PExpr::Return(PReturnExpr {
            keyword,
            arg_opt,
            fn_id_opt,
        }) => {
            let location = keyword.location(&gx.tokens);
            let k_fn = KFn::from_index(fn_id_opt.unwrap());

            let args = {
                let return_term = KTerm::Return { k_fn, location };
                let arg = match arg_opt {
                    Some(arg) => gen_expr(&arg, gx),
                    None => new_unit_term(location),
                };
                vec![return_term, arg]
            };
            gx.push(KCommand::Node {
                prim: KPrim::Jump,
                tys: vec![],
                args,
                result_opt: None,
                cont_count: 1,
                location,
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
            let location = keyword.location(&gx.tokens);
            emit_if(
                &cond_opt.as_ref().unwrap(),
                |gx| gen_block(body_opt.as_ref().unwrap(), gx),
                move |gx| match alt_opt {
                    Some(alt) => gen_expr(&alt, gx),
                    None => new_unit_term(location),
                },
                location,
                gx,
            )
        }
        PExpr::Match(PMatchExpr {
            keyword,
            cond_opt,
            arms,
            ..
        }) => {
            let location = keyword.location(&gx.tokens);
            let k_cond = gen_expr(cond_opt.as_ref().unwrap(), gx);
            if arms.is_empty() {
                return new_never_term(location).clone();
            }

            let result = gx.fresh_symbol("match_result", location);
            let next_label = gx.fresh_label("match_next", location);

            let args = once(k_cond.clone())
                .chain(arms.iter().map(|arm| match &arm.pat {
                    PPat::Name(name) => {
                        let location = name.location();
                        match name.info_opt.clone().unwrap() {
                            NName::Const(k_const) => KTerm::Const { k_const, location },
                            NName::LocalVar(_) => {
                                let symbol = gen_name(name, gx).expect_symbol();
                                KTerm::Name(symbol)
                            }
                            _ => {
                                error!("unimplemented pat {:?}", arm);
                                KTerm::Unit { location }
                            }
                        }
                    }
                    PPat::Record(PRecordPat { name, .. }) => {
                        let location = name.location();
                        let k_struct = name.info_opt.unwrap().as_struct().unwrap();
                        KTerm::RecordTag { k_struct, location }
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
                match &arm.pat {
                    PPat::Name(name) => match name.info_opt.as_ref().unwrap() {
                        NName::Const(_) => {}
                        NName::LocalVar(_) => {
                            let name = gen_name(name, gx).expect_symbol();
                            gx.push_prim_1(KPrim::Let, vec![k_cond.clone()], name);
                        }
                        _ => error!("unimplemented pat {:?}", name),
                    },
                    PPat::Record(_) => {}
                }

                let body = gen_expr(arm.body_opt.as_deref().unwrap(), gx);
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
            let location = keyword.location(&gx.tokens);
            let n_loop = NLoop::from_index(loop_id_opt.unwrap());
            let result = gx.fresh_symbol("while_result", location);
            let unit_term = new_unit_term(location);
            let KLoopData {
                break_label: next_label,
                continue_label,
            } = gx.current_loops[n_loop].clone();

            gx.push_jump(continue_label, vec![]);

            gx.push_label(continue_label, vec![]);

            let k_cond = gen_expr(cond_opt.as_ref().unwrap(), gx);

            gx.push(KCommand::Node {
                prim: KPrim::If,
                tys: vec![],
                args: vec![k_cond],
                result_opt: None,
                cont_count: 2,
                location,
            });

            // body:
            gen_block(body_opt.as_ref().unwrap(), gx);

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
            let location = keyword.location(&gx.tokens);
            let n_loop = NLoop::from_index(loop_id_opt.unwrap());
            let result = gx.fresh_symbol("loop_result", location);
            let KLoopData {
                break_label: next_label,
                continue_label,
            } = gx.current_loops[n_loop].clone();

            gx.push_jump(continue_label.clone(), vec![]);

            gx.push_label(continue_label.clone(), vec![]);

            // body:
            gen_block(body_opt.as_ref().unwrap(), gx);

            gx.push_jump(continue_label.clone(), vec![]);

            // next:
            gx.push_label(next_label, vec![result.clone()]);

            KTerm::Name(result)
        }
    }
}

fn gen_decl(decl: &PDecl, gx: &mut Gx) {
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
            let ty = ty_opt.as_ref().map_or(KTy::Unresolved, gen_ty);

            let k_init = gen_expr(init_opt.as_ref().unwrap(), gx);
            let mut result = gen_name(name_opt.as_ref().unwrap(), gx).expect_symbol();
            *result.ty_mut(&mut gx.current_locals) = ty;

            gx.push_prim_1(KPrim::Let, vec![k_init], result);
        }
        PDecl::Const(PConstDecl {
            name_opt, init_opt, ..
        }) => {
            let name = name_opt.clone().unwrap();
            let k_const = match name.info_opt {
                Some(NName::Const(n_const)) => n_const,
                _ => unreachable!(),
            };
            assert_eq!(name.text(&gx.tokens), k_const.of(&gx.outlines.consts).name);

            let init = init_opt.as_ref().unwrap();
            let init_location = init.location();
            let value_opt = match gen_constant(init, gx) {
                Some(value) => Some(value),
                None => {
                    gx.logger
                        .error(&init_location, "invalid constant expression");
                    None
                }
            };

            k_const.of_mut(&mut gx.outlines.consts).value_opt = value_opt;
        }
        PDecl::Static(PStaticDecl {
            name_opt, init_opt, ..
        }) => {
            let name = name_opt.clone().unwrap();
            let static_var = match name.info_opt {
                Some(NName::StaticVar(n_static_var)) => n_static_var,
                _ => unreachable!(),
            };
            assert_eq!(
                name.text(&gx.tokens),
                static_var.name(&gx.outlines.static_vars)
            );

            let init = init_opt.as_ref().unwrap();
            let init_location = init.location();
            let value_opt = match gen_constant(init, gx) {
                Some(value) => Some(value),
                None => {
                    gx.logger
                        .error(&init_location, "invalid constant expression");
                    None
                }
            };

            static_var.of_mut(&mut gx.outlines.static_vars).value_opt = value_opt;
        }

        PDecl::Fn(PFnDecl {
            keyword,
            block_opt,
            fn_id_opt,
            ..
        }) => {
            let location = keyword.location(&gx.tokens);
            let k_fn = KFn::from_index(fn_id_opt.unwrap());

            let parent_local_vars = {
                let local_vars = take(&mut gx.fns[k_fn].locals);
                replace(&mut gx.current_locals, local_vars)
            };

            let (commands, labels) = {
                let parent_commands = take(&mut gx.current_commands);
                let parent_labels = take(&mut gx.current_labels);
                let parent_loops = take(&mut gx.current_loops);

                gx.current_loops = VecArena::from_vec(
                    take(&mut gx.fn_loops[k_fn])
                        .iter()
                        .map(|loop_data| {
                            let location = loop_data.location;
                            let continue_label = gx.fresh_label("continue_", location);
                            let break_label = gx.fresh_label("next", location);
                            KLoopData {
                                break_label,
                                continue_label,
                            }
                        })
                        .collect(),
                );

                let result = gen_block(block_opt.as_ref().unwrap(), gx);
                gx.push(KCommand::Node {
                    prim: KPrim::Jump,
                    tys: vec![],
                    args: vec![KTerm::Return { k_fn, location }, result],
                    result_opt: None,
                    cont_count: 1,
                    location,
                });

                let commands = replace(&mut gx.current_commands, parent_commands);
                let labels = replace(&mut gx.current_labels, parent_labels);
                gx.current_loops = parent_loops;
                (commands, labels)
            };

            let (body, labels) = fold_block(commands, labels);

            let locals = replace(&mut gx.current_locals, parent_local_vars);

            let mut data = &mut gx.fns[k_fn];
            data.body = body;
            data.locals = locals;
            data.labels = labels;
        }
        PDecl::ExternFn(_) => {}
        PDecl::Enum(PEnumDecl {
            name_opt, variants, ..
        }) => {
            let name = name_opt.as_ref().unwrap();
            let k_enum = match name.info_opt {
                Some(NName::Enum(n_enum)) => n_enum,
                n_name_opt => unreachable!("{:?}", n_name_opt),
            };
            assert_eq!(name.text(&gx.tokens), k_enum.name(&gx.outlines.enums));

            let mut next_value = 0_usize;
            let k_variants = variants
                .into_iter()
                .map(|variant_decl| {
                    let k_variant = gen_variant(variant_decl, &mut next_value, gx);
                    next_value += 1;
                    k_variant
                })
                .collect::<Vec<_>>();

            let repr = KEnumRepr::determine(&k_variants, &gx.outlines.consts);
            k_enum.of_mut(&mut gx.outlines.enums).repr = repr;
        }
        PDecl::Struct(PStructDecl { variant_opt, .. }) => {
            if let Some(variant) = variant_opt {
                gen_variant(variant, &mut 0, gx);
            }
        }
    }
}

fn gen_block(block: &PBlock, gx: &mut Gx) -> KTerm {
    for decl in &block.decls {
        gen_decl(decl, gx);
    }

    match &block.last_opt {
        Some(last) => gen_expr(last, gx),
        None => {
            let location = block.left_brace.location(&gx.tokens);
            new_unit_term(location)
        }
    }
}

fn gen_root(root: &PRoot, gx: &mut Gx) {
    for decl in &root.decls {
        gen_decl(decl, gx);
    }
}

pub(crate) fn cps_conversion(
    p_root: &PRoot,
    name_resolution: &NameResolution,
    logger: Logger,
) -> KRoot {
    let k_root = {
        let k_consts = name_resolution
            .consts
            .iter()
            .map(|n_const_data| KConstData {
                name: n_const_data.name.to_string(),
                value_ty: n_const_data.value_ty.clone(),
                value_opt: {
                    // FIXME: calc here?
                    None
                },
                parent_opt: n_const_data.parent_opt,
                location: n_const_data.location,
            })
            .collect();

        let static_vars = name_resolution
            .static_vars
            .iter()
            .map(|n_static_var_data| KStaticVarData {
                name: n_static_var_data.name.to_string(),
                ty: n_static_var_data.ty.clone(),
                value_opt: {
                    // FIXME: calc here?
                    None
                },
                location: n_static_var_data.location,
            })
            .collect();

        // 関数の ID, 名前ID の対応表を構築する。
        let n_fns = &name_resolution.fns;
        let fn_loops = n_fns
            .iter()
            .map(|fn_data| fn_data.loops.clone())
            .collect::<Vec<_>>();
        let fn_outlines = n_fns
            .iter()
            .map(|data| KFnOutline {
                name: data.name.to_string(),
                vis_opt: data.vis_opt,
                param_tys: data
                    .params
                    .iter()
                    .map(|symbol| symbol.local.of(&data.local_vars).ty.clone())
                    .collect(),
                result_ty: data.result_ty.clone(),
                location: data.location,
            })
            .collect();
        let fns = n_fns
            .iter()
            .map(|fn_data| KFnData {
                params: fn_data.params.clone(),
                locals: KLocalArena::from_vec(
                    fn_data
                        .local_vars
                        .iter()
                        .map(|n_local_var_data| KLocalData {
                            name: n_local_var_data.name.to_string(),
                            ty: n_local_var_data.ty.clone(),
                            location: n_local_var_data.location,
                            is_alive: true,
                        })
                        .collect(),
                ),
                body: Default::default(),
                labels: Default::default(),
                label_sigs: Default::default(),
                ty_env: Default::default(),
            })
            .collect();

        // 外部関数の ID, 名前ID の対応表を構築する。
        let n_extern_fns = &name_resolution.extern_fns;
        let extern_fn_outlines = n_extern_fns
            .iter()
            .map(|data| KExternFnOutline {
                name: data.name.to_string(),
                param_tys: data
                    .params
                    .iter()
                    .map(|symbol| symbol.local.of(&data.local_vars).ty.clone())
                    .collect(),
                result_ty: data.result_ty.clone(),
                location: data.location,
            })
            .collect();
        let k_extern_fns = n_extern_fns
            .iter()
            .map(|extern_fn_data| KExternFnData {
                params: extern_fn_data.params.clone(),
                locals: KLocalArena::from_vec(
                    extern_fn_data
                        .local_vars
                        .iter()
                        .map(|n_local_var_data| KLocalData {
                            name: n_local_var_data.name.to_string(),
                            ty: n_local_var_data.ty.clone(),
                            location: n_local_var_data.location,
                            is_alive: true,
                        })
                        .collect(),
                ),
            })
            .collect();

        let k_enums = name_resolution
            .enums
            .iter()
            .map(|n_enum_data| KEnumOutline {
                name: n_enum_data.name.to_string(),
                variants: n_enum_data.variants.clone(),
                repr: {
                    // FIXME: determine?
                    KEnumRepr::Never
                },
                location: n_enum_data.location,
            })
            .collect();

        let k_structs = name_resolution
            .structs
            .iter()
            .map(|n_struct_data| KStructOutline {
                name: n_struct_data.name.to_string(),
                fields: n_struct_data.fields.clone(),
                parent_opt: n_struct_data.parent_opt.map(KStructParent::new),
                location: n_struct_data.location,
            })
            .collect();

        let k_fields = name_resolution
            .fields
            .iter()
            .map(|n_field_data| KFieldOutline {
                name: n_field_data.name.to_string(),
                ty: n_field_data.ty.clone(),
                location: n_field_data.location,
            })
            .collect();

        let mut gx = Gx::new(logger.clone());
        gx.tokens = Rc::new(p_root.tokens.clone());

        gx.outlines.consts = VecArena::from_vec(k_consts);

        gx.outlines.static_vars = VecArena::from_vec(static_vars);

        gx.fns = VecArena::from_vec(fns);
        gx.fn_loops = VecArena::from_vec(fn_loops);
        gx.outlines.fns = VecArena::from_vec(fn_outlines);

        gx.extern_fns = VecArena::from_vec(k_extern_fns);
        gx.outlines.extern_fns = VecArena::from_vec(extern_fn_outlines);

        gx.outlines.enums = VecArena::from_vec(k_enums);

        gx.outlines.structs = VecArena::from_vec(k_structs);

        gx.outlines.fields = VecArena::from_vec(k_fields);

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
