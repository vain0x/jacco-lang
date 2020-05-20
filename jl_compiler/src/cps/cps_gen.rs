//! 構文木から CPS ノードのもとになる命令列を生成する処理

use super::cps_fold::fold_block;
use super::*;
use crate::parse::*;
use crate::token::TokenKind;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

struct FnConstruction {
    return_label: KSymbol,
}

struct LoopConstruction {
    break_label: KSymbol,
    continue_label: KSymbol,
}

/// Code generation context.
#[derive(Default)]
struct Gx {
    symbols: HashMap<usize, Rc<KVarDef>>,
    struct_map: HashMap<usize, Rc<RefCell<KStructData>>>,
    current: Vec<XCommand>,
    parent_loop: Option<LoopConstruction>,
    current_fn: Option<FnConstruction>,
    extern_fns: Vec<KExternFn>,
    structs: Vec<KStruct>,
    fns: Vec<KFn>,
    logger: Logger,
}

impl Gx {
    fn new(logger: Logger) -> Self {
        Self {
            symbols: vec![(
                0,
                Rc::new(KVarDef {
                    id_opt: RefCell::new(Some(0)),
                    ty: RefCell::new(KTy::Never),
                }),
            )]
            .into_iter()
            .collect(),
            logger,
            ..Self::default()
        }
    }

    fn fresh_symbol(&mut self, hint: &str, location: Location) -> KSymbol {
        let text = hint.to_string();

        KSymbol {
            text,
            ty: KTy::default(),
            location,
            def: Rc::default(),
        }
    }

    fn current_break_label(&self) -> Option<&KSymbol> {
        self.parent_loop.as_ref().map(|l| &l.break_label)
    }

    fn current_continue_label(&self) -> Option<&KSymbol> {
        self.parent_loop.as_ref().map(|l| &l.continue_label)
    }

    fn current_return_label(&self) -> Option<&KSymbol> {
        self.current_fn.as_ref().map(|f| &f.return_label)
    }

    fn push(&mut self, command: XCommand) {
        self.current.push(command);
    }

    fn do_push_jump(
        &mut self,
        label: KSymbol,
        args: impl IntoIterator<Item = KTerm>,
        cont_count: usize,
    ) {
        self.push(XCommand::Prim {
            prim: KPrim::Jump,
            tys: vec![],
            args: std::iter::once(KTerm::Name(label)).chain(args).collect(),
            result_opt: None,
            cont_count,
        });
    }

    fn push_jump(&mut self, label: KSymbol, args: impl IntoIterator<Item = KTerm>) {
        self.do_push_jump(label, args, 0);
    }

    fn push_jump_with_cont(&mut self, label: KSymbol, args: impl IntoIterator<Item = KTerm>) {
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

    gx.push(XCommand::Prim {
        prim,
        tys: vec![],
        args: vec![arg],
        result_opt: Some(result.clone()),
        cont_count: 1,
    });

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

    gx.push(XCommand::Prim {
        prim,
        tys: vec![],
        args: vec![left, right],
        result_opt: Some(result.clone()),
        cont_count: 1,
    });

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
    let next_label = gx.fresh_symbol("next", location.clone());

    let k_cond = gen_expr(cond, gx);

    gx.push(XCommand::Prim {
        prim: KPrim::If,
        tys: vec![],
        args: vec![k_cond],
        cont_count: 2,
        result_opt: None,
    });

    // body
    {
        let body = gen_body(gx);
        gx.push_jump(next_label.clone(), vec![body]);
    }

    // alt
    {
        let alt = gen_alt(gx);
        gx.push_jump(next_label.clone(), vec![alt]);
    }

    gx.push(XCommand::Label {
        label: next_label,
        params: vec![result.clone()],
    });

    KTerm::Name(result)
}

fn gen_ty_name(ty_name: PNameTy, gx: &mut Gx) -> KTy {
    let name = ty_name.0;
    match name.text() {
        "i32" => KTy::I32,
        _ => {
            if let Some(def) = gx.symbols.get(&name.name_id) {
                assert!(def.ty.borrow().is_symbol());
                return def.ty.borrow().clone();
            }

            // FIXME: location info
            gx.logger.error(
                Location::default(),
                format!("undefined type name {:?}", name.text()),
            );
            KTy::Unresolved
        }
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

fn gen_name_with_ty(name: PName, ty: KTy, gx: &mut Gx) -> KSymbol {
    let def = match gx.symbols.get(&name.name_id) {
        Some(def) => def.clone(),
        None => {
            let def = Rc::new(KVarDef::new_with_ty(ty.clone()));
            gx.symbols.insert(name.name_id, def.clone());
            def
        }
    };

    let (text, location) = name.decompose();

    KSymbol {
        text,
        ty,
        location,
        def,
    }
}

fn gen_name(name: PName, gx: &mut Gx) -> KSymbol {
    gen_name_with_ty(name, KTy::default(), gx)
}

fn gen_param(param: PParam, gx: &mut Gx) -> KSymbol {
    let ty = match param.ty_opt {
        Some(ty) => gen_ty(ty, gx),
        None => {
            gx.logger
                .error(param.name.location().clone(), "param type is mandatory");
            KTy::Never
        }
    };

    gen_name_with_ty(param.name, ty, gx)
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
            let (_, text, location) = name_opt.unwrap().decompose();
            let result = gx.fresh_symbol(&format!("{}_ptr", text), location.clone());

            let left = gen_expr_lval(*left, location.clone(), gx);
            let field = KTerm::Field { text, location };

            gx.push(XCommand::Prim {
                prim: KPrim::GetField,
                tys: vec![],
                args: vec![left, field],
                result_opt: Some(result.clone()),
                cont_count: 1,
            });

            KTerm::Name(result)
        }
        expr => emit_unary_op(KPrim::Ref, Some(Box::new(expr)), location, gx),
    }
}

fn gen_expr(expr: PExpr, gx: &mut Gx) -> KTerm {
    match expr {
        PExpr::Int(PIntExpr { token }) => KTerm::Int(token),
        PExpr::Str(..) => unimplemented!(),
        PExpr::Name(PNameExpr(name)) => KTerm::Name(gen_name(name, gx)),
        PExpr::Struct(PStructExpr { name, fields, .. }) => {
            let result = gx.fresh_symbol(name.0.text(), name.location());
            let ty = match gx.struct_map.get(&name.0.name_id) {
                Some(def) => KTy::Symbol { def: def.clone() },
                None => {
                    error!("struct {:?} should be found here", name.0.text());
                    return new_never_term(name.location());
                }
            };

            // FIXME: フィールドに過不足がある、定義時と順番が異なるケースなどに対処
            let args = fields
                .into_iter()
                .map(|field| gen_expr(field.value_opt.unwrap(), gx))
                .collect();

            gx.push(XCommand::Prim {
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
                    let location = arg_list.left.into_location();
                    new_unit_term(location)
                }
                [arg] if !is_tuple => gen_expr(std::mem::take(&mut arg.expr), gx),
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

            gx.push(XCommand::Prim {
                prim: KPrim::Deref,
                tys: vec![],
                args: vec![result1],
                result_opt: Some(result2.clone()),
                cont_count: 1,
            });

            KTerm::Name(result2)
        }
        PExpr::Call(PCallExpr { callee, arg_list }) => {
            let location = arg_list.left.into_location();
            let result = gx.fresh_symbol("call_result", location.clone());

            let k_callee = gen_expr(*callee, gx);

            let mut k_args = vec![k_callee];
            for p_arg in arg_list.args {
                let k_arg = gen_expr(p_arg.expr, gx);
                k_args.push(k_arg);
            }

            gx.push(XCommand::Prim {
                prim: KPrim::CallDirect,
                tys: vec![],
                args: k_args,
                result_opt: Some(result.clone()),
                cont_count: 1,
            });

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
            PUnaryOp::Negate => emit_unary_op(KPrim::Negate, arg_opt, location, gx),
        },
        PExpr::BinaryOp(PBinaryOpExpr {
            op,
            left,
            right_opt,
            location,
        }) => match op {
            PBinaryOp::Assign => {
                let result = gx.fresh_symbol("unit", location.clone());

                let left = gen_expr_lval(*left, location, gx);
                let right = gen_expr(*right_opt.unwrap(), gx);

                gx.push(XCommand::Prim {
                    prim: KPrim::Assign,
                    tys: vec![],
                    args: vec![left, right],
                    result_opt: Some(result.clone()),
                    cont_count: 1,
                });

                KTerm::Name(result)
            }
            PBinaryOp::Add => emit_binary_op(KPrim::Add, *left, right_opt, location, gx),
            PBinaryOp::Sub => emit_binary_op(KPrim::Sub, *left, right_opt, location, gx),
            PBinaryOp::Mul => emit_binary_op(KPrim::Mul, *left, right_opt, location, gx),
            PBinaryOp::Div => emit_binary_op(KPrim::Div, *left, right_opt, location, gx),
            PBinaryOp::Mod => emit_binary_op(KPrim::Mod, *left, right_opt, location, gx),
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
        PExpr::Break(PBreakExpr { keyword, arg_opt }) => {
            let location = keyword.into_location();

            let label = gx.current_break_label().expect("out of loop").clone();
            let arg = match arg_opt {
                Some(arg) => gen_expr(*arg, gx),
                None => new_unit_term(location.clone()),
            };
            gx.push_jump_with_cont(label, vec![arg]);

            new_never_term(location)
        }
        PExpr::Continue(PContinueExpr { keyword, .. }) => {
            let location = keyword.into_location();

            let label = gx.current_continue_label().expect("out of loop").clone();
            gx.push_jump_with_cont(label, vec![]);

            new_never_term(location)
        }
        PExpr::Return(PReturnExpr { keyword, arg_opt }) => {
            let location = keyword.into_location();

            let label = gx.current_return_label().expect("out of fn").clone();
            let arg = match arg_opt {
                Some(arg) => gen_expr(*arg, gx),
                None => new_unit_term(location.clone()),
            };
            gx.push_jump_with_cont(label, vec![arg]);

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
            ..
        }) => {
            let location = keyword.into_location();
            let result = gx.fresh_symbol("while_result", location.clone());
            let continue_label = gx.fresh_symbol("continue_", location.clone());
            let next_label = gx.fresh_symbol("next", location.clone());

            gx.push_jump(continue_label.clone(), vec![]);

            gx.push(XCommand::Label {
                label: continue_label.clone(),
                params: vec![],
            });

            let k_cond = gen_expr(*cond_opt.unwrap(), gx);

            gx.push(XCommand::Prim {
                prim: KPrim::If,
                tys: vec![],
                args: vec![k_cond],
                result_opt: None,
                cont_count: 2,
            });

            let parent_loop = std::mem::replace(
                &mut gx.parent_loop,
                Some(LoopConstruction {
                    break_label: next_label.clone(),
                    continue_label: continue_label.clone(),
                }),
            );

            // body:
            gen_block(body_opt.unwrap(), gx);

            gx.push_jump(continue_label.clone(), vec![]);

            // alt:
            gx.push_jump(next_label.clone(), vec![new_unit_term(location)]);

            gx.parent_loop = parent_loop;

            // next:
            gx.push(XCommand::Label {
                label: next_label,
                params: vec![result.clone()],
            });

            KTerm::Name(result)
        }
        PExpr::Loop(PLoopExpr { keyword, body_opt }) => {
            let location = keyword.into_location();
            let result = gx.fresh_symbol("loop_result", location.clone());
            let continue_label = gx.fresh_symbol("continue_", location.clone());
            let next_label = gx.fresh_symbol("next", location.clone());

            gx.push_jump(continue_label.clone(), vec![]);

            gx.push(XCommand::Label {
                label: continue_label.clone(),
                params: vec![],
            });

            let parent_loop = std::mem::replace(
                &mut gx.parent_loop,
                Some(LoopConstruction {
                    break_label: next_label.clone(),
                    continue_label: continue_label.clone(),
                }),
            );

            // body:
            gen_block(body_opt.unwrap(), gx);

            gx.push_jump(continue_label.clone(), vec![]);

            gx.parent_loop = parent_loop;

            // next:
            gx.push(XCommand::Label {
                label: next_label,
                params: vec![result.clone()],
            });

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
            let result = gen_name(name_opt.unwrap(), gx);
            let k_init = gen_expr(init_opt.unwrap(), gx);

            gx.push(XCommand::Prim {
                prim: KPrim::Let,
                tys: vec![],
                args: vec![k_init],
                result_opt: Some(result),
                cont_count: 1,
            });
        }
        PDecl::Fn(PFnDecl {
            keyword,
            name_opt,
            block_opt,
            ..
        }) => {
            let location = keyword.into_location();
            let fn_name = gen_name(name_opt.unwrap(), gx);
            let return_label = gx.fresh_symbol("return", location.clone());
            let fn_construction = FnConstruction {
                return_label: return_label.clone(),
            };

            let commands = {
                let prev_fn = std::mem::replace(&mut gx.current_fn, Some(fn_construction));
                let previous = std::mem::take(&mut gx.current);
                let parent_loop = std::mem::take(&mut gx.parent_loop);

                let k_result = gen_block(block_opt.unwrap(), gx);

                gx.push_jump(return_label.clone(), vec![k_result]);

                gx.current_fn = prev_fn;
                gx.parent_loop = parent_loop;
                std::mem::replace(&mut gx.current, previous)
            };

            let (node, labels) = fold_block(commands);

            let k_fn = KFn {
                name: fn_name,
                params: vec![return_label],
                body: node,
                labels,
            };

            gx.fns.push(k_fn);
        }
        PDecl::ExternFn(PExternFnDecl {
            name_opt,
            param_list_opt,
            result_opt,
            ..
        }) => {
            let name = gen_name(name_opt.unwrap(), gx);
            let params = param_list_opt
                .unwrap()
                .params
                .into_iter()
                .map(|param| gen_param(param, gx))
                .collect();
            let result = match result_opt {
                Some(ty) => gen_ty(ty, gx),
                None => KTy::Unit,
            };

            let extern_fn = KExternFn {
                name,
                params,
                result,
            };
            gx.extern_fns.push(extern_fn);
        }
        PDecl::Struct(PStructDecl {
            name_opt,
            variant_opt,
            ..
        }) => {
            let name = name_opt.unwrap();
            let name_id = name.name_id;
            let name = gen_name(name, gx);

            let fields = match variant_opt {
                Some(PVariantDecl::Struct(PStructVariantDecl { fields, .. })) => fields
                    .into_iter()
                    .map(|field| {
                        let name = gen_name(field.name, gx);
                        KFieldDef { name }
                    })
                    .collect(),
                None => vec![],
            };

            let def = Rc::new(RefCell::new(KStructData { name, fields }));
            gx.struct_map.insert(name_id, def.clone());
            gx.structs.push(KStruct { def });
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
            let location = block.left.into_location();
            new_unit_term(location)
        }
    }
}

fn gen_root(root: PRoot, gx: &mut Gx) {
    for decl in root.decls {
        gen_decl(decl, gx);
    }
}

pub(crate) fn cps_conversion(p_root: PRoot, logger: Logger) -> KRoot {
    let mut k_root = {
        let mut gx = Gx::new(logger.clone());
        gen_root(p_root, &mut gx);
        KRoot {
            extern_fns: gx.extern_fns,
            fns: gx.fns,
            structs: gx.structs,
        }
    };

    trace!("k_root (untyped) = {:#?}\n", k_root);

    type_resolution::resolve_types(&mut k_root, logger.clone());

    k_root
}
