//! 名前解決の処理

use super::*;
use crate::logs::Logger;
use log::trace;
use std::{
    collections::HashMap,
    mem::{replace, take},
};

#[derive(Default)]
pub(crate) struct NLoopData {
    pub(crate) location: Location,
}

pub(crate) struct NLocalVarData;

pub(crate) struct NConstData;

pub(crate) struct NStaticVarData;

pub(crate) struct NFnData {
    // FIXME: クロージャのように関数境界を超えるローカル変数があると困るかもしれない
    pub(crate) local_vars: Vec<NLocalVarData>,
    pub(crate) loops: Vec<NLoopData>,
}

pub(crate) struct NExternFnData {
    pub(crate) local_vars: Vec<NLocalVarData>,
}

pub(crate) struct NEnumData;

pub(crate) struct NStructData;

pub(crate) struct NFieldData;

/// 名前解決の結果。
#[derive(Default)]
pub(crate) struct NameResolution {
    pub(crate) consts: Vec<NConstData>,
    pub(crate) static_vars: Vec<NStaticVarData>,
    pub(crate) fns: Vec<NFnData>,
    pub(crate) extern_fns: Vec<NExternFnData>,
    pub(crate) enums: Vec<NEnumData>,
    pub(crate) structs: Vec<NStructData>,
    pub(crate) fields: Vec<NFieldData>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum NName {
    Unresolved,
    /// ローカル変数や仮引数
    LocalVar(usize),
    Const(usize),
    StaticVar(usize),
    Fn(usize),
    ExternFn(usize),
    Enum(usize),
    Struct(usize),
    Bool,
    I32,
    I64,
    Usize,
    F64,
    C8,
}

impl NName {
    pub(crate) fn as_struct(self) -> Option<usize> {
        match self {
            NName::Struct(struct_id) => Some(struct_id),
            _ => None,
        }
    }
}

impl Default for NName {
    fn default() -> Self {
        NName::Unresolved
    }
}

/// Naming context. 名前解決処理の状態を持ち運ぶもの
#[derive(Default)]
struct Nx {
    local_env: HashMap<String, NName>,
    // global_env: HashMap<String, NName>,
    parent_loop: Option<usize>,
    parent_fn: Option<usize>,
    parent_local_vars: Vec<NLocalVarData>,
    parent_loops: Vec<NLoopData>,
    res: NameResolution,
    logger: Logger,
}

impl Nx {
    fn new(logger: Logger) -> Self {
        Self {
            logger,
            ..Self::default()
        }
    }

    // fn import_global(&mut self, name: String, n_name: NName) {
    //     trace!("global {} => {:?}", name, n_name);
    //     self.global_env.insert(name, n_name);
    // }

    fn import_local(&mut self, name: String, n_name: NName) {
        trace!(
            "local {} => {:?} (parent={:?})",
            name,
            n_name,
            self.parent_fn
        );
        self.local_env.insert(name, n_name);
    }

    fn enter_scope(&mut self, do_resolve: impl FnOnce(&mut Nx)) {
        // FIXME: 効率化
        let parent_env = self.local_env.clone();

        do_resolve(self);

        self.local_env = parent_env;
    }

    fn enter_loop(&mut self, location: Location, do_resolve: impl FnOnce(&mut Nx, usize)) {
        let loop_id = self.parent_loops.len();
        self.parent_loops.push(NLoopData { location });

        let parent_loop = replace(&mut self.parent_loop, Some(loop_id));

        do_resolve(self, loop_id);

        self.parent_loop = parent_loop;
    }

    fn enter_fn(&mut self, fn_id: usize, do_resolve: impl FnOnce(&mut Nx)) {
        let parent_loop = take(&mut self.parent_loop);
        let parent_fn = replace(&mut self.parent_fn, Some(fn_id));
        let parent_local_vars = take(&mut self.parent_local_vars);
        let parent_loops = take(&mut self.parent_loops);

        do_resolve(self);

        self.parent_loop = parent_loop;
        self.parent_fn = parent_fn;
        let local_vars = replace(&mut self.parent_local_vars, parent_local_vars);
        let loops = replace(&mut self.parent_loops, parent_loops);

        let mut fn_data = &mut self.res.fns[fn_id];
        fn_data.local_vars = local_vars;
        fn_data.loops = loops;
    }
}

fn parse_known_ty_name(s: &str) -> Option<NName> {
    let n_name = match s {
        "i32" => NName::I32,
        "i64" => NName::I64,
        "usize" => NName::Usize,
        "f64" => NName::F64,
        "c8" => NName::C8,
        "bool" => NName::Bool,
        _ => return None,
    };
    Some(n_name)
}

fn find_value_name(name: &str, nx: &Nx) -> Option<NName> {
    nx.local_env
        .get(name)
        // .or_else(|| nx.global_env.get(name))
        .cloned()
}

fn resolve_name_use(name: &mut PName, nx: &mut Nx) {
    let name_info = find_value_name(&name.full_name(), nx).unwrap_or_else(|| {
        nx.logger.error(name, "undefined value");
        NName::Unresolved
    });

    name.info_opt = Some(name_info);
}

fn resolve_name_def(p_name: &mut PName, n_name: NName, nx: &mut Nx) {
    p_name.info_opt = Some(n_name);

    if !p_name.is_underscore() {
        nx.import_local(p_name.full_name(), n_name);
    }
}

fn resolve_qualified_name_def(
    p_name: &mut PName,
    parent_name_opt: Option<&str>,
    n_name: NName,
    nx: &mut Nx,
) {
    p_name.info_opt = Some(n_name);

    if !p_name.is_underscore() {
        let full_name = match parent_name_opt {
            Some(parent_name) => {
                // PName::full_name と同じエンコーディング
                format!("{}::{}", parent_name, p_name.full_name())
            }
            _ => p_name.full_name(),
        };
        nx.import_local(full_name, n_name);
    }
}

fn resolve_local_var_def(name: &mut PName, nx: &mut Nx) {
    // alloc local
    let local_var_id = nx.parent_local_vars.len();
    nx.parent_local_vars.push(NLocalVarData);

    resolve_name_def(name, NName::LocalVar(local_var_id), nx);
}

fn resolve_ty_name(ty_name: &mut PNameTy, nx: &mut Nx) {
    let name = &mut ty_name.0;

    // 環境から探して、なければ組み込み型の名前とみなす。
    let name_info = find_value_name(&name.full_name(), nx)
        .or_else(|| parse_known_ty_name(name.text()))
        .unwrap_or_else(|| {
            nx.logger.error(name, "undefined type");
            NName::Unresolved
        });

    name.info_opt = Some(name_info);
}

fn resolve_ty(ty: &mut PTy, nx: &mut Nx) {
    match ty {
        PTy::Name(name) => resolve_ty_name(name, nx),
        PTy::Never(_) | PTy::Unit(_) => {}
        PTy::Ptr(PPtrTy { ty_opt, .. }) => {
            resolve_ty_opt(ty_opt.as_deref_mut(), nx);
        }
    }
}

fn resolve_ty_opt(ty_opt: Option<&mut PTy>, nx: &mut Nx) {
    if let Some(ty) = ty_opt {
        resolve_ty(ty, nx);
    }
}

fn resolve_pat(pat: &mut PPat, nx: &mut Nx) {
    match pat {
        PPat::Name(name) => match find_value_name(&name.full_name(), nx) {
            Some(NName::Const(_)) => {
                resolve_name_use(name, nx);
            }
            _ => resolve_local_var_def(name, nx),
        },
        PPat::Record(PRecordPat { name, .. }) => resolve_name_use(name, nx),
    }
}

#[allow(unused)]
fn resolve_pat_opt(pat_opt: Option<&mut PPat>, nx: &mut Nx) {
    if let Some(pat) = pat_opt {
        resolve_pat(pat, nx);
    }
}

fn resolve_expr(expr: &mut PExpr, nx: &mut Nx) {
    match expr {
        PExpr::Int(_)
        | PExpr::Float(_)
        | PExpr::Char(_)
        | PExpr::Str(_)
        | PExpr::True(_)
        | PExpr::False(_) => {}
        PExpr::Name(PNameExpr(name)) => {
            resolve_name_use(name, nx);
        }
        PExpr::Record(PRecordExpr { name, fields, .. }) => {
            resolve_ty_name(name, nx);

            for field in fields {
                resolve_expr_opt(field.value_opt.as_mut(), nx);
            }
        }
        PExpr::Tuple(PTupleExpr { arg_list }) => {
            for arg in &mut arg_list.args {
                resolve_expr(&mut arg.expr, nx);
            }
        }
        PExpr::DotField(PDotFieldExpr { left, .. }) => {
            // NOTE: フィールド名は型検査が終わるまで解決できない。
            resolve_expr(left, nx);
        }
        PExpr::Call(PCallExpr { left, arg_list }) | PExpr::Index(PIndexExpr { left, arg_list }) => {
            resolve_expr(left, nx);

            for arg in &mut arg_list.args {
                resolve_expr(&mut arg.expr, nx);
            }
        }
        PExpr::As(PAsExpr { left, ty_opt, .. }) => {
            resolve_expr(left, nx);
            resolve_ty_opt(ty_opt.as_mut(), nx);
        }
        PExpr::UnaryOp(PUnaryOpExpr { arg_opt, .. }) => {
            resolve_expr_opt(arg_opt.as_deref_mut(), nx);
        }
        PExpr::BinaryOp(PBinaryOpExpr {
            left, right_opt, ..
        }) => {
            resolve_expr(left, nx);
            resolve_expr_opt(right_opt.as_deref_mut(), nx);
        }
        PExpr::Pipe(PPipeExpr {
            left, right_opt, ..
        }) => {
            resolve_expr(left, nx);
            resolve_expr_opt(right_opt.as_deref_mut(), nx);
        }
        PExpr::Block(PBlockExpr(block)) => {
            resolve_block(block, nx);
        }
        PExpr::Break(PBreakExpr {
            keyword,
            arg_opt,
            loop_id_opt,
        }) => {
            resolve_expr_opt(arg_opt.as_deref_mut(), nx);

            *loop_id_opt = nx.parent_loop;
            if loop_id_opt.is_none() {
                nx.logger.error(keyword, "break out of loop");
            }
        }
        PExpr::Continue(PContinueExpr {
            keyword,
            loop_id_opt,
        }) => {
            *loop_id_opt = nx.parent_loop;
            if loop_id_opt.is_none() {
                nx.logger.error(keyword, "continue out of loop");
            }
        }
        PExpr::Return(PReturnExpr {
            keyword,
            arg_opt,
            fn_id_opt,
        }) => {
            resolve_expr_opt(arg_opt.as_deref_mut(), nx);

            *fn_id_opt = nx.parent_fn;
            if fn_id_opt.is_none() {
                nx.logger.error(keyword, "return out of function");
            }
        }
        PExpr::If(PIfExpr {
            cond_opt,
            body_opt,
            alt_opt,
            ..
        }) => {
            resolve_expr_opt(cond_opt.as_deref_mut(), nx);
            resolve_block_opt(body_opt.as_mut(), nx);
            resolve_expr_opt(alt_opt.as_deref_mut(), nx);
        }
        PExpr::Match(PMatchExpr { cond_opt, arms, .. }) => {
            resolve_expr_opt(cond_opt.as_deref_mut(), nx);

            for arm in arms {
                nx.enter_scope(|nx| {
                    resolve_pat(&mut arm.pat, nx);
                    resolve_expr_opt(arm.body_opt.as_deref_mut(), nx);
                });
            }
        }
        PExpr::While(PWhileExpr {
            keyword,
            cond_opt,
            body_opt,
            loop_id_opt,
            ..
        }) => {
            let location = keyword.location();

            resolve_expr_opt(cond_opt.as_deref_mut(), nx);

            nx.enter_loop(location, |nx, loop_id| {
                *loop_id_opt = Some(loop_id);
                resolve_block_opt(body_opt.as_mut(), nx);
            });
        }
        PExpr::Loop(PLoopExpr {
            keyword,
            body_opt,
            loop_id_opt,
            ..
        }) => {
            let location = keyword.location();

            nx.enter_loop(location, |nx, loop_id| {
                *loop_id_opt = Some(loop_id);
                resolve_block_opt(body_opt.as_mut(), nx);
            });
        }
    }
}

fn resolve_param_list_opt(param_list_opt: Option<&mut PParamList>, nx: &mut Nx) {
    let params = param_list_opt
        .into_iter()
        .flat_map(|param_list| param_list.params.iter_mut());
    for param in params {
        resolve_local_var_def(&mut param.name, nx);
        resolve_ty_opt(param.ty_opt.as_mut(), nx);
    }
}

fn resolve_variant(variant: &mut PVariantDecl, parent_name_opt: Option<&str>, nx: &mut Nx) {
    match variant {
        PVariantDecl::Const(PConstVariantDecl {
            name, value_opt, ..
        }) => {
            // alloc const
            let k_const = {
                let const_id = nx.res.consts.len();
                nx.res.consts.push(NConstData);
                NName::Const(const_id)
            };

            resolve_qualified_name_def(name, parent_name_opt, k_const, nx);
            resolve_expr_opt(value_opt.as_deref_mut(), nx);
        }
        PVariantDecl::Record(PRecordVariantDecl { name, fields, .. }) => {
            // alloc struct
            let k_struct = {
                let struct_id = nx.res.structs.len();
                nx.res.structs.push(NStructData);
                NName::Struct(struct_id)
            };

            resolve_qualified_name_def(name, parent_name_opt, k_struct, nx);

            for field in fields {
                // alloc field
                let field_id = nx.res.fields.len();
                nx.res.fields.push(NFieldData);
                field.field_id_opt = Some(field_id);

                // resolve_name_def(&mut field.name, PNameKind::Field, nx);
                resolve_ty_opt(field.ty_opt.as_mut(), nx);
            }
        }
    }
}

fn resolve_decls(decls: &mut [PDecl], nx: &mut Nx) {
    // 再帰的に定義される宣言を先に環境に加える。
    // FIXME: 同じスコープに同じ名前の再帰的な宣言があったらコンパイルエラー
    for decl in decls.iter_mut() {
        match decl {
            PDecl::Fn(PFnDecl {
                name_opt,
                fn_id_opt,
                ..
            }) => {
                // alloc fn
                let fn_id = nx.res.fns.len();
                *fn_id_opt = Some(fn_id);
                nx.res.fns.push(NFnData {
                    local_vars: vec![],
                    loops: vec![],
                });

                if let Some(name) = name_opt {
                    resolve_name_def(name, NName::Fn(fn_id), nx);
                }
            }
            PDecl::ExternFn(PExternFnDecl {
                name_opt,
                extern_fn_id_opt,
                ..
            }) => {
                // alloc extern fn
                let extern_fn_id = nx.res.extern_fns.len();
                *extern_fn_id_opt = Some(extern_fn_id);
                nx.res.extern_fns.push(NExternFnData { local_vars: vec![] });

                if let Some(name) = name_opt.as_mut() {
                    resolve_name_def(name, NName::ExternFn(extern_fn_id), nx);
                }
            }
            PDecl::Enum(PEnumDecl {
                name_opt, variants, ..
            }) => {
                // alloc enum
                let k_enum = {
                    let enum_id = nx.res.enums.len();
                    nx.res.enums.push(NEnumData);
                    NName::Enum(enum_id)
                };

                let mut parent_name = "__anonymous_enum";

                if let Some(name) = name_opt {
                    resolve_name_def(name, k_enum, nx);
                    parent_name = name.text();
                }

                for variant in variants {
                    resolve_variant(variant, Some(parent_name), nx);
                }
            }
            PDecl::Struct(PStructDecl { variant_opt, .. }) => {
                if let Some(variant) = variant_opt {
                    resolve_variant(variant, None, nx);
                }
            }
            PDecl::Expr(_) | PDecl::Let(_) | PDecl::Const(_) | PDecl::Static(_) => {}
        }
    }

    for decl in decls.iter_mut() {
        resolve_decl(decl, nx);
    }
}

fn resolve_decl(decl: &mut PDecl, nx: &mut Nx) {
    match decl {
        PDecl::Expr(PExprDecl { expr, .. }) => {
            resolve_expr(expr, nx);
        }
        PDecl::Let(PLetDecl {
            name_opt,
            ty_opt,
            init_opt,
            ..
        }) => {
            resolve_ty_opt(ty_opt.as_mut(), nx);
            resolve_expr_opt(init_opt.as_mut(), nx);

            if let Some(name) = name_opt.as_mut() {
                resolve_local_var_def(name, nx);
            }
        }
        PDecl::Const(PConstDecl {
            name_opt,
            ty_opt,
            init_opt,
            ..
        }) => {
            // alloc const
            let const_id = nx.res.consts.len();
            nx.res.consts.push(NConstData);

            resolve_ty_opt(ty_opt.as_mut(), nx);
            resolve_expr_opt(init_opt.as_mut(), nx);

            if let Some(name) = name_opt {
                resolve_name_def(name, NName::Const(const_id), nx);
            }
        }
        PDecl::Static(PStaticDecl {
            name_opt,
            ty_opt,
            init_opt,
            ..
        }) => {
            // alloc static var
            let static_var_id = nx.res.static_vars.len();
            nx.res.static_vars.push(NStaticVarData);

            resolve_ty_opt(ty_opt.as_mut(), nx);
            resolve_expr_opt(init_opt.as_mut(), nx);

            if let Some(name) = name_opt {
                resolve_name_def(name, NName::StaticVar(static_var_id), nx);
            }
        }
        PDecl::Fn(PFnDecl {
            param_list_opt,
            result_ty_opt,
            block_opt,
            fn_id_opt,
            ..
        }) => {
            let fn_id = fn_id_opt.unwrap();

            nx.enter_fn(fn_id, |nx| {
                nx.enter_scope(|nx| {
                    resolve_param_list_opt(param_list_opt.as_mut(), nx);
                    resolve_ty_opt(result_ty_opt.as_mut(), nx);
                    resolve_block_opt(block_opt.as_mut(), nx);
                });
            });
        }
        PDecl::ExternFn(PExternFnDecl {
            param_list_opt,
            result_ty_opt,
            extern_fn_id_opt,
            ..
        }) => {
            let parent_local_vars = take(&mut nx.parent_local_vars);

            nx.enter_scope(|nx| {
                resolve_param_list_opt(param_list_opt.as_mut(), nx);
                resolve_ty_opt(result_ty_opt.as_mut(), nx);
            });

            let local_vars = replace(&mut nx.parent_local_vars, parent_local_vars);

            nx.res.extern_fns[extern_fn_id_opt.unwrap()].local_vars = local_vars;
        }
        PDecl::Enum(_) | PDecl::Struct(_) => {}
    }
}

fn resolve_expr_opt(expr_opt: Option<&mut PExpr>, nx: &mut Nx) {
    if let Some(expr) = expr_opt {
        resolve_expr(expr, nx);
    }
}

fn resolve_block(block: &mut PBlock, nx: &mut Nx) {
    nx.enter_scope(|nx| {
        resolve_decls(&mut block.decls, nx);

        if let Some(last) = &mut block.last_opt {
            resolve_expr(last, nx);
        }
    });
}

fn resolve_block_opt(block_opt: Option<&mut PBlock>, nx: &mut Nx) {
    if let Some(block) = block_opt {
        resolve_block(block, nx);
    }
}

pub(crate) fn resolve_name(p_root: &mut PRoot, logger: Logger) -> NameResolution {
    let mut nx = Nx::new(logger);

    resolve_decls(&mut p_root.decls, &mut nx);

    nx.res
}
