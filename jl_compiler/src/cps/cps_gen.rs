//! 構文木から CPS ノードのもとになる命令列を生成する処理

use super::cps_fold::fold_block;
use super::*;
use crate::parse::*;
use crate::{front::NameResolution, token::TokenKind};
use std::collections::HashMap;
use std::iter::once;
use std::mem::{replace, take};
use std::rc::Rc;

struct KLoopData {
    break_label: KLabel,
    continue_label: KLabel,
}

/// Code generation context.
#[derive(Default)]
struct Gx {
    outlines: KOutlines,
    loop_map: HashMap<usize, KLoopData>,
    local_map: HashMap<PNameId, KLocal>,
    fn_map: HashMap<PNameId, KFn>,
    extern_fn_map: HashMap<PNameId, KExternFn>,
    struct_map: HashMap<PNameId, KStruct>,
    current_commands: Vec<KCommand>,
    current_locals: Vec<KLocalData>,
    current_labels: Vec<KLabelData>,
    extern_fns: Vec<KExternFnData>,
    structs: Vec<KStruct>,
    fns: Vec<KFnData>,
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

    fn get_break_label(&self, loop_id_opt: Option<usize>) -> Option<KLabel> {
        self.loop_map
            .get(&loop_id_opt?)
            .map(|data| data.break_label)
    }

    fn get_continue_label(&self, loop_id_opt: Option<usize>) -> Option<KLabel> {
        self.loop_map
            .get(&loop_id_opt?)
            .map(|data| data.continue_label)
    }

    fn push(&mut self, command: KCommand) {
        self.current_commands.push(command);
    }

    fn push_label(&mut self, label: KLabel, params: Vec<KSymbol>) {
        self.push(KCommand::Label { label, params })
    }

    // ちょうど1つの継続を持つプリミティブノードを生成する。
    fn push_prim_1(&mut self, prim: KPrim, args: Vec<KTerm>, result: KSymbol) {
        self.push(KCommand::Node {
            prim,
            tys: vec![],
            args,
            result_opt: Some(result),
            cont_count: 1,
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
        });
    }

    fn push_jump(&mut self, label: KLabel, args: impl IntoIterator<Item = KTerm>) {
        self.do_push_jump(label, args, 0);
    }

    fn push_jump_with_cont(&mut self, label: KLabel, args: impl IntoIterator<Item = KTerm>) {
        self.do_push_jump(label, args, 1);
    }
}

fn new_int_term(value: i64, location: Location) -> KTerm {
    KTerm::Int(TokenData::new(TokenKind::Int, value.to_string(), location))
}

fn new_false_term(location: Location) -> KTerm {
    new_int_term(0, location)
}

fn new_true_term(location: Location) -> KTerm {
    new_int_term(1, location)
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

fn gen_ty_name(ty_name: PNameTy, gx: &mut Gx) -> KTy {
    let mut name = ty_name.0;
    let name_info = take(&mut name.info_opt).unwrap();

    match name_info.kind() {
        PNameKind::I32 => KTy::I32,
        PNameKind::Struct => {
            let k_struct = *gx.struct_map.get(&name_info.id()).unwrap();
            KTy::Struct(k_struct)
        }
        _ => unreachable!("expected type name but {:?}", name),
    }
}

fn gen_ty(ty: PTy, gx: &mut Gx) -> KTy {
    match ty {
        PTy::Name(name) => gen_ty_name(name, gx),
        PTy::Never(_) => KTy::Never,
        PTy::Unit(_) => KTy::Unit,
        PTy::Ptr(_) => unimplemented!(),
    }
}

fn gen_name_with_ty(mut name: PName, ty: KTy, gx: &mut Gx) -> KSymbolExt {
    let name_info = take(&mut name.info_opt).unwrap();
    let (name, location) = name.decompose();

    match name_info.kind() {
        PNameKind::Fn => match gx.fn_map.get(&name_info.id()) {
            Some(&k_fn) => KSymbolExt::Fn(k_fn),
            None => unimplemented!("maybe recursive fn reference?"),
        },
        PNameKind::ExternFn => match gx.extern_fn_map.get(&name_info.id()) {
            Some(&extern_fn) => KSymbolExt::ExternFn(extern_fn),
            None => unimplemented!("maybe recursive extern fn reference?"),
        },
        PNameKind::Local => {
            let local = match gx.local_map.get(&name_info.id()) {
                Some(&local) => local,
                None => {
                    let local = KLocal::new(gx.current_locals.len());
                    gx.current_locals.push(KLocalData {
                        name: name.clone(),
                        ty: ty.clone(),
                        is_alive: true,
                    });
                    gx.local_map.insert(name_info.id(), local);
                    local
                }
            };

            KSymbolExt::Symbol(KSymbol { local, location })
        }
        PNameKind::I32 | PNameKind::Struct => {
            // FIXME: Unit-like 構造体なら OK
            gx.logger.error(&location, "型の名前です。");
            // FIXME: 適切にハンドル？
            KSymbolExt::Symbol(gx.fresh_symbol(&name, location))
        }
        PNameKind::Unresolved | PNameKind::Field => {
            // Unresolved ならエラーなのでコード生成には来ないはず。
            // フィールド名は環境に導入されないはず。
            unreachable!("{:?}", (&name, &location, name_info))
        }
    }
}

fn gen_name(name: PName, gx: &mut Gx) -> KSymbolExt {
    gen_name_with_ty(name, KTy::default(), gx)
}

fn gen_param(param: PParam, gx: &mut Gx) -> KSymbol {
    let ty = match param.ty_opt {
        Some(ty) => gen_ty(ty, gx),
        None => {
            gx.logger.error(&param.name, "param type is mandatory");
            KTy::Never
        }
    };

    gen_name_with_ty(param.name, ty, gx).expect_symbol()
}

/// 式を左辺値とみなして変換する。(結果として、ポインタ型の項を期待している。)
fn gen_expr_lval(expr: PExpr, location: Location, gx: &mut Gx) -> KTerm {
    match expr {
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

            let left = gen_expr_lval(*left, location.clone(), gx);
            let field = KTerm::FieldTag(KFieldTag { name, location });

            gx.push_prim_1(KPrim::GetField, vec![left, field], result.clone());

            KTerm::Name(result)
        }
        expr => emit_unary_op(KPrim::Ref, Some(Box::new(expr)), location, gx),
    }
}

fn gen_expr(expr: PExpr, gx: &mut Gx) -> KTerm {
    match expr {
        PExpr::Int(PIntExpr { token }) => KTerm::Int(token),
        PExpr::Str(..) => unimplemented!(),
        PExpr::Name(PNameExpr(name)) => match gen_name(name, gx) {
            KSymbolExt::Symbol(symbol) => KTerm::Name(symbol),
            KSymbolExt::Fn(k_fn) => KTerm::Fn(k_fn),
            KSymbolExt::ExternFn(extern_fn) => KTerm::ExternFn(extern_fn),
        },
        PExpr::Struct(PStructExpr {
            mut name, fields, ..
        }) => {
            let name_info = take(&mut name.0.info_opt).unwrap();
            assert_eq!(name_info.kind(), PNameKind::Struct);

            let (name, location) = name.0.decompose();
            let result = gx.fresh_symbol(&name, location.clone());

            let ty = match gx.struct_map.get(&name_info.id()) {
                Some(def) => KTy::Struct(def.clone().into()),
                None => {
                    error!("struct {:?} should be found here", name);
                    return new_never_term(location);
                }
            };

            // FIXME: フィールドに過不足がある、定義時と順番が異なるケースなどに対処
            let args = fields
                .into_iter()
                .map(|field| gen_expr(field.value_opt.unwrap(), gx))
                .collect();

            gx.push(KCommand::Node {
                prim: KPrim::Struct,
                tys: vec![ty],
                args,
                result_opt: Some(result.clone()),
                cont_count: 1,
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
            let result1 = gen_expr_lval(expr, location.clone(), gx);
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
        PExpr::UnaryOp(PUnaryOpExpr {
            op,
            arg_opt,
            location,
        }) => match op {
            PUnaryOp::Deref => emit_unary_op(KPrim::Deref, arg_opt, location, gx),
            PUnaryOp::Ref => gen_expr_lval(*arg_opt.unwrap(), location, gx),
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
                let left = gen_expr_lval(*left, location.clone(), gx);
                let right = gen_expr(*right_opt.unwrap(), gx);

                gx.push(KCommand::Node {
                    prim: KPrim::Assign,
                    tys: vec![],
                    args: vec![left, right],
                    result_opt: None,
                    cont_count: 1,
                });

                new_unit_term(location)
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
        PExpr::Block(PBlockExpr(block)) => gen_block(block, gx),
        PExpr::Break(PBreakExpr {
            keyword,
            arg_opt,
            loop_id_opt,
        }) => {
            let location = keyword.into_location();

            let label = gx.get_break_label(loop_id_opt).unwrap().clone();
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

            let label = gx.get_continue_label(loop_id_opt).unwrap().clone();
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
        PExpr::While(PWhileExpr {
            keyword,
            cond_opt,
            body_opt,
            loop_id_opt,
            ..
        }) => {
            let location = keyword.into_location();
            let result = gx.fresh_symbol("while_result", location.clone());
            let continue_label = gx.fresh_label("continue_", location.clone());
            let next_label = gx.fresh_label("next", location.clone());
            let unit_term = new_unit_term(location);

            gx.loop_map.insert(
                loop_id_opt.unwrap(),
                KLoopData {
                    break_label: next_label,
                    continue_label,
                },
            );

            gx.push_jump(continue_label, vec![]);

            gx.push_label(continue_label, vec![]);

            let k_cond = gen_expr(*cond_opt.unwrap(), gx);

            gx.push(KCommand::Node {
                prim: KPrim::If,
                tys: vec![],
                args: vec![k_cond],
                result_opt: None,
                cont_count: 2,
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
            let continue_label = gx.fresh_label("continue_", location.clone());
            let next_label = gx.fresh_label("next", location.clone());

            gx.loop_map.insert(
                loop_id_opt.unwrap(),
                KLoopData {
                    break_label: next_label,
                    continue_label,
                },
            );

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
            name_opt, init_opt, ..
        }) => {
            let result = gen_name(name_opt.unwrap(), gx).expect_symbol();
            let k_init = gen_expr(init_opt.unwrap(), gx);

            gx.push_prim_1(KPrim::Let, vec![k_init], result);
        }
        PDecl::Fn(PFnDecl {
            keyword,
            name_opt,
            block_opt,
            fn_id_opt,
            ..
        }) => {
            let location = keyword.into_location();
            let k_fn = KFn::new(fn_id_opt.unwrap());

            let parent_locals = take(&mut gx.current_locals);

            let name = {
                let mut name = name_opt.unwrap();
                let name_info = take(&mut name.info_opt).unwrap();
                let (name, _) = name.decompose();

                assert_eq!(name_info.kind(), PNameKind::Fn);
                assert_eq!(gx.fn_map.get(&name_info.id()).copied(), Some(k_fn));

                name
            };

            let (commands, labels) = {
                let parent_commands = take(&mut gx.current_commands);
                let parent_labels = take(&mut gx.current_labels);

                let result = gen_block(block_opt.unwrap(), gx);
                gx.push(KCommand::Node {
                    prim: KPrim::Jump,
                    tys: vec![],
                    args: vec![KTerm::Return(k_fn), result],
                    result_opt: None,
                    cont_count: 1,
                });

                let commands = replace(&mut gx.current_commands, parent_commands);
                let labels = replace(&mut gx.current_labels, parent_labels);
                (commands, labels)
            };

            let (node, labels) = fold_block(commands, labels);

            let locals = replace(&mut gx.current_locals, parent_locals);

            // FIXME: generate params
            *gx.outlines.fn_get_mut(k_fn) = KFnOutline {
                name,
                param_tys: vec![],
                result_ty: KTy::Never,
                location,
            };
            gx.fns[k_fn.id()] = KFnData {
                params: vec![],
                body: node,
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
            ..
        }) => {
            let parent_locals = take(&mut gx.current_locals);

            let location = extern_keyword.location().unite(&fn_keyword.location());
            let (fn_name, fn_name_id) = {
                let mut name = name_opt.unwrap();
                let name_info = take(&mut name.info_opt).unwrap();
                assert_eq!(name_info.kind(), PNameKind::ExternFn);
                let (name, _) = name.decompose();
                (name, name_info.id())
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

            let locals = replace(&mut gx.current_locals, parent_locals);

            let extern_fn = gx.outlines.extern_fn_new(KExternFnOutline {
                name: fn_name,
                param_tys,
                result_ty,
                location,
            });
            gx.extern_fns
                .insert(extern_fn.id(), KExternFnData { params, locals });
            gx.extern_fn_map.insert(fn_name_id, extern_fn);
        }
        PDecl::Struct(PStructDecl {
            name_opt,
            variant_opt,
            ..
        }) => {
            let mut name = name_opt.unwrap();
            let name_info = take(&mut name.info_opt).unwrap();
            assert_eq!(name_info.kind(), PNameKind::Struct);
            let (name, location) = name.decompose();

            let fields = match variant_opt {
                Some(PVariantDecl::Struct(PStructVariantDecl { fields, .. })) => fields
                    .into_iter()
                    .map(|field| {
                        // NOTE: フィールド名は型推論中に解決されるので、PName::info_opt は使わない。(sizeof(K::foo) のようにフィールドを直接指す構文があれば必要になる。)
                        let (text, location) = field.name.decompose();
                        let field_ty = field.ty_opt.map_or(KTy::Unresolved, |ty| gen_ty(ty, gx));
                        gx.outlines
                            .field_new(KFieldOutline::new(text, field_ty, location))
                    })
                    .collect(),
                None => vec![],
            };

            let k_struct = gx.outlines.struct_new(KStructOutline {
                name,
                fields,
                location,
            });
            gx.struct_map.insert(name_info.id(), k_struct.clone());
            gx.structs.push(k_struct);
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

// FIXME: 結果を構造体に入れる
pub(crate) fn cps_conversion(
    p_root: PRoot,
    name_resolution: NameResolution,
    logger: Logger,
) -> (KRoot, Rc<KOutlines>) {
    let (mut k_root, outlines) = {
        // 関数IDと関数名の名前IDの対応表を構築する。
        let n_fns = name_resolution.fns();
        let fn_count = n_fns.len();
        let fns = vec![KFnData::default(); fn_count];
        let fn_name_to_fn_map = n_fns
            .iter()
            .enumerate()
            .filter_map(|(i, n_fn)| {
                n_fn.fn_name_id_opt()
                    .map(|fn_name_id| (fn_name_id, KFn::new(i)))
            })
            .collect();

        let mut gx = Gx::new(logger.clone());
        gx.fns = fns;
        gx.fn_map = fn_name_to_fn_map;
        gx.outlines.fns.resize_with(fn_count, Default::default);

        gen_root(p_root, &mut gx);
        (
            KRoot {
                extern_fns: gx.extern_fns,
                fns: gx.fns,
            },
            Rc::new(gx.outlines),
        )
    };

    trace!("k_root (untyped) = {:#?}\n", k_root);

    type_resolution::resolve_types(&mut k_root, outlines.clone(), logger.clone());

    (k_root, outlines)
}
