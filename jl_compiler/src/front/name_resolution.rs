//! 名前解決の処理

use super::*;
use crate::{
    cps::{
        KAlias, KAliasArena, KAliasOutline, KConst, KConstTag, KEnum, KEnumTag, KExternFn,
        KExternFnTag, KField, KFieldTag, KFn, KFnTag, KLocal, KLocalTag, KStaticVar, KStaticVarTag,
        KStruct, KStructTag, KSymbol, KTy, KVariant, KVis,
    },
    logs::DocLogger,
    utils::{VecArena, VecArenaId},
};
use cps_conversion::gen_ty;
use std::{
    collections::HashMap,
    fmt::Debug,
    mem::{replace, take},
    rc::Rc,
};

pub(crate) struct NLoopTag;

pub(crate) type NLoop = VecArenaId<NLoopTag>;

pub(crate) type NLoopArena = VecArena<NLoopTag, NLoopData>;

#[derive(Clone)]
pub(crate) struct NLoopData {
    pub(crate) location: Location,
}

pub(crate) struct NLocalVarData {
    pub(crate) name: String,
    pub(crate) ty: KTy,
    pub(crate) location: Location,
}

pub(crate) type NLocalVarArena = VecArena<KLocalTag, NLocalVarData>;

pub(crate) type NConstArena = VecArena<KConstTag, NConstData>;

pub(crate) struct NConstData {
    pub(crate) name: String,
    pub(crate) value_ty: KTy,
    pub(crate) parent_opt: Option<KEnum>,
    pub(crate) location: Location,
}

pub(crate) type NStaticVarArena = VecArena<KStaticVarTag, NStaticVarData>;

pub(crate) struct NStaticVarData {
    pub(crate) name: String,
    pub(crate) ty: KTy,
    pub(crate) location: Location,
}

pub(crate) type NFnArena = VecArena<KFnTag, NFnData>;

pub(crate) struct NFnData {
    pub(crate) name: String,
    pub(crate) vis_opt: Option<KVis>,
    pub(crate) params: Vec<KSymbol>,
    pub(crate) result_ty: KTy,
    pub(crate) location: Location,
    // FIXME: クロージャのように関数境界を超えるローカル変数があると困るかもしれない
    pub(crate) local_vars: NLocalVarArena,
    pub(crate) loops: NLoopArena,
}

pub(crate) type NExternFnArena = VecArena<KExternFnTag, NExternFnData>;

pub(crate) struct NExternFnData {
    pub(crate) name: String,
    pub(crate) params: Vec<KSymbol>,
    pub(crate) result_ty: KTy,
    pub(crate) location: Location,
    pub(crate) local_vars: NLocalVarArena,
}

pub(crate) type NEnumArena = VecArena<KEnumTag, NEnumData>;

pub(crate) struct NEnumData {
    pub(crate) name: String,
    pub(crate) variants: Vec<KVariant>,
    pub(crate) location: Location,
}

pub(crate) type NStructArena = VecArena<KStructTag, NStructData>;

pub(crate) struct NStructData {
    pub(crate) name: String,
    pub(crate) fields: Vec<KField>,
    pub(crate) parent_opt: Option<KEnum>,
    pub(crate) location: Location,
}

pub(crate) type NFieldArena = VecArena<KFieldTag, NFieldData>;

pub(crate) struct NFieldData {
    pub(crate) name: String,
    pub(crate) ty: KTy,
    pub(crate) location: Location,
}

#[derive(Copy, Clone)]
pub(crate) enum DefOrUse {
    Def,
    Use,
}

/// 名前解決の結果。
#[derive(Default)]
pub(crate) struct NameResolution {
    pub(crate) names: VecArena<PNameTag, NName>,
    pub(crate) occurrences: VecArena<PNameTag, (NAbsName, DefOrUse)>,
    pub(crate) aliases: KAliasArena,
    pub(crate) consts: NConstArena,
    pub(crate) static_vars: NStaticVarArena,
    pub(crate) fns: NFnArena,
    pub(crate) extern_fns: NExternFnArena,
    pub(crate) enums: NEnumArena,
    pub(crate) structs: NStructArena,
    pub(crate) fields: NFieldArena,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum NName {
    Unresolved,
    Alias(KAlias),
    LocalVar(KLocal),
    Const(KConst),
    StaticVar(KStaticVar),
    Fn(KFn),
    ExternFn(KExternFn),
    Enum(KEnum),
    Struct(KStruct),
    Bool,
    I8,
    I16,
    I32,
    I64,
    Isize,
    U8,
    U16,
    U32,
    U64,
    Usize,
    F32,
    F64,
    C8,
    C16,
    C32,
}

impl NName {
    #[allow(unused)]
    pub(crate) fn as_enum(self) -> Option<KEnum> {
        match self {
            NName::Enum(k_enum) => Some(k_enum),
            _ => None,
        }
    }

    pub(crate) fn as_struct(self) -> Option<KStruct> {
        match self {
            NName::Struct(n_struct) => Some(n_struct),
            _ => None,
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum NParentFn {
    Fn(KFn),
    ExternFn(KExternFn),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum NAbsName {
    Unresolved,
    LocalVar {
        parent_fn: NParentFn,
        local: KLocal,
    },
    /// Unresolved, LocalVar 以外
    Other(NName),
}

impl NAbsName {
    pub(crate) fn new(parent_fn_opt: Option<NParentFn>, n_name: NName) -> Self {
        match n_name {
            NName::Unresolved => NAbsName::Unresolved,
            NName::LocalVar(local) => NAbsName::LocalVar {
                parent_fn: parent_fn_opt.unwrap(),
                local,
            },
            _ => NAbsName::Other(n_name),
        }
    }
}

impl From<NAbsName> for NName {
    fn from(name: NAbsName) -> Self {
        match name {
            NAbsName::Unresolved => NName::Unresolved,
            NAbsName::LocalVar { local, .. } => NName::LocalVar(local),
            NAbsName::Other(name) => name,
        }
    }
}

/// Naming context. 名前解決処理の状態を持ち運ぶもの
#[derive(Default)]
struct Nx {
    tokens: Rc<PTokens>,
    names: PNameArena,
    local_env: HashMap<String, NAbsName>,
    // global_env: HashMap<String, NName>,
    parent_loop: Option<NLoop>,
    parent_fn: Option<NParentFn>,
    parent_local_vars: NLocalVarArena,
    parent_loops: NLoopArena,
    res: NameResolution,
    logger: DocLogger,
}

impl Nx {
    fn new(tokens: Rc<PTokens>, names: PNameArena, logger: DocLogger) -> Self {
        Self {
            tokens,
            names,
            logger,
            ..Self::default()
        }
    }

    fn tokens(&self) -> &PTokens {
        &self.tokens
    }

    // fn import_global(&mut self, name: String, n_name: NName) {
    //     trace!("global {} => {:?}", name, n_name);
    //     self.global_env.insert(name, n_name);
    // }

    fn import_local(&mut self, name: String, n_name: NAbsName) {
        self.local_env.insert(name, n_name);
    }

    fn enter_scope(&mut self, do_resolve: impl FnOnce(&mut Nx)) {
        // FIXME: 効率化
        let parent_env = self.local_env.clone();

        do_resolve(self);

        self.local_env = parent_env;
    }

    fn enter_loop(&mut self, location: Location, do_resolve: impl FnOnce(&mut Nx, NLoop)) {
        let n_loop = self.parent_loops.alloc(NLoopData { location });

        let parent_loop = replace(&mut self.parent_loop, Some(n_loop));

        do_resolve(self, n_loop);

        self.parent_loop = parent_loop;
    }

    fn enter_fn(&mut self, k_fn: KFn, do_resolve: impl FnOnce(&mut Nx)) {
        let parent_loop = take(&mut self.parent_loop);
        let parent_fn = replace(&mut self.parent_fn, Some(NParentFn::Fn(k_fn)));
        let parent_local_vars = take(&mut self.parent_local_vars);
        let parent_loops = take(&mut self.parent_loops);

        do_resolve(self);

        self.parent_loop = parent_loop;
        self.parent_fn = parent_fn;
        let local_vars = replace(&mut self.parent_local_vars, parent_local_vars);
        let loops = replace(&mut self.parent_loops, parent_loops);

        let mut fn_data = &mut self.res.fns[k_fn];
        fn_data.local_vars = local_vars;
        fn_data.loops = loops;
    }
}

fn error_on_token(token: PToken, message: impl Into<String>, nx: &Nx) {
    let loc = PLoc::new(token);
    nx.logger.error(loc, message);
}

fn error_on_name(name: PName, message: impl Into<String>, nx: &Nx) {
    let loc = PLoc::new(name.of(&nx.names).token);
    nx.logger.error(loc, message);
}

fn parse_known_ty_name(s: &str) -> Option<NName> {
    let n_name = match s {
        "i8" => NName::I8,
        "i16" => NName::I16,
        "i32" => NName::I32,
        "i64" => NName::I64,
        "isize" => NName::Isize,
        "u8" => NName::U8,
        "u16" => NName::U16,
        "u32" => NName::U32,
        "u64" => NName::U64,
        "usize" => NName::Usize,
        "f32" => NName::F32,
        "f64" => NName::F64,
        "c8" => NName::C8,
        "c16" => NName::C16,
        "c32" => NName::C32,
        "bool" => NName::Bool,
        _ => return None,
    };
    Some(n_name)
}

fn find_value_name(name: &str, nx: &Nx) -> Option<NAbsName> {
    nx.local_env
        .get(name)
        // .or_else(|| nx.global_env.get(name))
        .cloned()
}

fn resolve_name_use(p_name: PName, nx: &mut Nx) {
    let n_name = find_value_name(&p_name.full_name(&nx.names), nx).unwrap_or_else(|| {
        error_on_name(p_name, "undefined value", nx);
        NAbsName::Unresolved
    });

    nx.res.names[p_name] = n_name.into();
    nx.res.occurrences[p_name] = (n_name, DefOrUse::Use);
}

fn resolve_name_def(p_name: PName, n_name: NName, nx: &mut Nx) {
    let abs_name = NAbsName::new(nx.parent_fn, n_name);
    nx.res.names[p_name] = n_name;
    nx.res.occurrences[p_name] = (abs_name, DefOrUse::Def);

    if !p_name.is_underscore(&nx.names) {
        nx.import_local(p_name.full_name(&nx.names), abs_name);
    }
}

fn resolve_qualified_name_def(
    p_name: PName,
    parent_name_opt: Option<&str>,
    n_name: NName,
    nx: &mut Nx,
) {
    let abs_name = NAbsName::new(nx.parent_fn, n_name);
    nx.res.names[p_name] = n_name;
    nx.res.occurrences[p_name] = (abs_name, DefOrUse::Def);

    if !p_name.is_underscore(&nx.names) {
        let full_name = match parent_name_opt {
            Some(parent_name) => {
                // PName::full_name と同じエンコーディング
                format!("{}::{}", parent_name, p_name.full_name(&nx.names))
            }
            _ => p_name.full_name(&nx.names),
        };
        nx.import_local(full_name, abs_name);
    }
}

fn resolve_local_var_def(name: PName, nx: &mut Nx) -> KLocal {
    // alloc local
    let local_var = nx.parent_local_vars.alloc(NLocalVarData {
        name: name.text(&nx.names).to_string(),
        ty: KTy::Unresolved,
        location: name.location(),
    });

    resolve_name_def(name, NName::LocalVar(local_var), nx);
    local_var
}

fn resolve_ty_name(p_name: PName, nx: &mut Nx) {
    // 環境から探して、なければ組み込み型の名前とみなす。
    let abs_name = find_value_name(&p_name.full_name(&nx.names), nx)
        .or_else(|| parse_known_ty_name(p_name.text(&nx.names)).map(NAbsName::Other))
        .unwrap_or_else(|| {
            error_on_name(p_name, "undefined type", nx);
            NAbsName::Unresolved
        });

    nx.res.names[p_name] = abs_name.into();
    nx.res.occurrences[p_name] = (abs_name, DefOrUse::Use);
}

fn resolve_ty(ty: &mut PTy, nx: &mut Nx) {
    match ty {
        PTy::Name(name) => resolve_ty_name(*name, nx),
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
        PPat::Char(_) => {}
        PPat::Name(name) => match find_value_name(&name.full_name(&nx.names), nx) {
            Some(NAbsName::Other(NName::Const(_))) => {
                resolve_name_use(*name, nx);
            }
            _ => {
                resolve_local_var_def(*name, nx);
            }
        },
        PPat::Record(PRecordPat { name, .. }) => resolve_name_use(*name, nx),
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
        PExpr::Name(name) => {
            resolve_name_use(*name, nx);
        }
        PExpr::Record(PRecordExpr { name, fields, .. }) => {
            resolve_ty_name(*name, nx);

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

            *loop_id_opt = nx.parent_loop.map(NLoop::to_index);
            if loop_id_opt.is_none() {
                error_on_token(*keyword, "break out of loop", nx);
            }
        }
        PExpr::Continue(PContinueExpr {
            keyword,
            loop_id_opt,
        }) => {
            *loop_id_opt = nx.parent_loop.map(NLoop::to_index);
            if loop_id_opt.is_none() {
                error_on_token(*keyword, "continue out of loop", nx);
            }
        }
        PExpr::Return(PReturnExpr {
            keyword,
            arg_opt,
            fn_id_opt,
        }) => {
            resolve_expr_opt(arg_opt.as_deref_mut(), nx);

            *fn_id_opt = match nx.parent_fn {
                Some(NParentFn::Fn(k_fn)) => Some(k_fn.to_index()),
                _ => unreachable!(),
            };
            if fn_id_opt.is_none() {
                error_on_token(*keyword, "return out of loop", nx);
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
            let location = keyword.location(nx.tokens());

            resolve_expr_opt(cond_opt.as_deref_mut(), nx);

            nx.enter_loop(location, |nx, n_loop| {
                *loop_id_opt = Some(n_loop.to_index());
                resolve_block_opt(body_opt.as_mut(), nx);
            });
        }
        PExpr::Loop(PLoopExpr {
            keyword,
            body_opt,
            loop_id_opt,
            ..
        }) => {
            let location = keyword.location(nx.tokens());

            nx.enter_loop(location, |nx, n_loop| {
                *loop_id_opt = Some(n_loop.to_index());
                resolve_block_opt(body_opt.as_mut(), nx);
            });
        }
    }
}

fn resolve_param_list_opt(
    param_list_opt: Option<&mut PParamList>,
    k_params: &mut Vec<KSymbol>,
    nx: &mut Nx,
) {
    let params = param_list_opt
        .into_iter()
        .flat_map(|param_list| param_list.params.iter_mut());
    for param in params {
        let local = resolve_local_var_def(param.name, nx);
        resolve_ty_opt(param.ty_opt.as_mut(), nx);

        let ty = match &param.ty_opt {
            Some(ty) => gen_ty(ty, &nx.res.names),
            None => {
                error_on_name(param.name, "param type is mandatory", nx);
                KTy::Unresolved
            }
        };
        nx.parent_local_vars[local].ty = ty;

        k_params.push(KSymbol {
            local,
            location: param.name.location(),
        });
    }
}

fn resolve_variant(
    variant: &mut PVariantDecl,
    parent_opt: Option<KEnum>,
    parent_name_opt: Option<&str>,
    nx: &mut Nx,
) -> KVariant {
    match variant {
        PVariantDecl::Const(PConstVariantDecl {
            name, value_opt, ..
        }) => {
            // alloc const
            let n_const = nx.res.consts.alloc(NConstData {
                name: name.text(&nx.names).to_string(),
                value_ty: {
                    // FIXME: 値を見て型を決める？
                    KTy::Usize
                },
                parent_opt,
                location: name.location(),
            });

            resolve_qualified_name_def(*name, parent_name_opt, NName::Const(n_const), nx);
            resolve_expr_opt(value_opt.as_deref_mut(), nx);

            KVariant::Const(n_const)
        }
        PVariantDecl::Record(PRecordVariantDecl { name, fields, .. }) => {
            let mut n_fields = Vec::with_capacity(fields.len());

            // alloc struct
            let n_struct = nx.res.structs.alloc(NStructData {
                name: name.text(&nx.names).to_string(),
                fields: vec![],
                parent_opt,
                location: name.location(),
            });

            resolve_qualified_name_def(*name, parent_name_opt, NName::Struct(n_struct), nx);

            for field in fields {
                // 型は後ろにある宣言を見た後に解決する。
                // alloc field
                let n_field = nx.res.fields.alloc(NFieldData {
                    name: field.name.text(&nx.names).to_string(),
                    ty: KTy::Unresolved,
                    location: name.location(),
                });
                field.field_id_opt = Some(n_field.to_index());
                n_fields.push(n_field);

                // resolve_name_def(&mut field.name, PNameKind::Field, nx);
                resolve_ty_opt(field.ty_opt.as_mut(), nx);
            }

            nx.res.structs[n_struct].fields = n_fields;
            KVariant::Record(n_struct)
        }
    }
}

fn resolve_variant2(variant: &PVariantDecl, nx: &mut Nx) {
    match variant {
        PVariantDecl::Const(_) => {}
        PVariantDecl::Record(PRecordVariantDecl { name, fields, .. }) => {
            let k_struct = name.of(&nx.res.names).as_struct().unwrap();
            for (k_field, p_field) in nx.res.structs[k_struct].fields.clone().iter().zip(fields) {
                let ty = p_field
                    .ty_opt
                    .as_ref()
                    .map_or(KTy::Unresolved, |ty| gen_ty(ty, &nx.res.names));
                k_field.of_mut(&mut nx.res.fields).ty = ty;
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
                keyword,
                name_opt,
                fn_id_opt,
                ..
            }) => {
                let location = keyword.location(nx.tokens());

                // alloc fn
                let k_fn = nx.res.fns.alloc(NFnData {
                    name: Default::default(),
                    vis_opt: Default::default(),
                    params: Default::default(),
                    result_ty: Default::default(),
                    location,
                    local_vars: Default::default(),
                    loops: Default::default(),
                });
                *fn_id_opt = Some(k_fn.to_index());

                if let Some(name) = name_opt {
                    resolve_name_def(*name, NName::Fn(k_fn), nx);

                    nx.res.fns[k_fn].name = name.text(&nx.names).to_string();
                }
            }
            PDecl::ExternFn(PExternFnDecl {
                extern_keyword,
                fn_keyword,
                name_opt,
                extern_fn_id_opt,
                ..
            }) => {
                let location = extern_keyword
                    .location(nx.tokens())
                    .unite(fn_keyword.location(nx.tokens()));

                // alloc extern fn
                let extern_fn = nx.res.extern_fns.alloc(NExternFnData {
                    name: Default::default(),
                    params: Default::default(),
                    result_ty: Default::default(),
                    location,
                    local_vars: Default::default(),
                });
                *extern_fn_id_opt = Some(extern_fn.to_index());

                if let Some(name) = name_opt.as_mut() {
                    resolve_name_def(*name, NName::ExternFn(extern_fn), nx);

                    nx.res.extern_fns[extern_fn].name = name.text(&nx.names).to_string();
                }
            }
            PDecl::Enum(PEnumDecl {
                keyword,
                name_opt,
                variants,
                ..
            }) => {
                // alloc enum
                let n_enum = nx.res.enums.alloc(NEnumData {
                    name: Default::default(),
                    variants: Default::default(),
                    location: keyword.location(&nx.tokens()),
                });

                let mut enum_name = String::new();
                if let Some(name) = name_opt {
                    resolve_name_def(*name, NName::Enum(n_enum), nx);

                    enum_name = name.text(&nx.names).to_string();
                }

                let n_variants = variants
                    .iter_mut()
                    .map(|p_variant| resolve_variant(p_variant, Some(n_enum), Some(&enum_name), nx))
                    .collect();

                let enum_data = n_enum.of_mut(&mut nx.res.enums);
                enum_data.name = enum_name;
                enum_data.variants = n_variants;
            }
            PDecl::Struct(PStructDecl { variant_opt, .. }) => {
                if let Some(variant) = variant_opt {
                    resolve_variant(variant, None, None, nx);
                }
            }
            PDecl::Use(PUseDecl { name_opt, .. }) => {
                if let Some(p_name) = name_opt {
                    // alloc alias
                    let alias = {
                        let location = p_name.of(&nx.names).token.of(&nx.tokens).location();
                        let path = p_name.name_path(&nx.names, &nx.tokens);
                        let name = p_name.text(&nx.names).to_string();

                        nx.res
                            .aliases
                            .alloc(KAliasOutline::new(name, path, location))
                    };

                    resolve_name_def(*p_name, NName::Alias(alias), nx);

                    // resolve_name_def では full_name (foo:bar) の方しか登録されない。
                    nx.import_local(
                        p_name.text(&nx.names).to_string(),
                        NAbsName::Other(NName::Alias(alias)),
                    );
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
                resolve_local_var_def(*name, nx);
            }
        }
        PDecl::Const(PConstDecl {
            keyword,
            name_opt,
            ty_opt,
            init_opt,
            ..
        }) => {
            // alloc const
            let n_const = nx.res.consts.alloc(NConstData {
                name: String::new(),
                value_ty: KTy::Unresolved,
                parent_opt: None,
                location: keyword.location(&nx.tokens()),
            });

            resolve_ty_opt(ty_opt.as_mut(), nx);
            resolve_expr_opt(init_opt.as_mut(), nx);

            if let Some(name) = name_opt {
                resolve_name_def(*name, NName::Const(n_const), nx);

                nx.res.consts[n_const].name = name.text(&nx.names).to_string();
            }

            let value_ty = match ty_opt {
                Some(p_ty) => {
                    let ty = gen_ty(p_ty, &nx.res.names);
                    if !ty.is_primitive() {
                        error_on_token(*keyword, "定数はプリミティブ型でなければいけません", nx);
                    }
                    ty
                }
                None => KTy::Unresolved,
            };
            n_const.of_mut(&mut nx.res.consts).value_ty = value_ty;
        }
        PDecl::Static(PStaticDecl {
            keyword,
            name_opt,
            ty_opt,
            init_opt,
            ..
        }) => {
            // alloc static var
            let n_static_var = nx.res.static_vars.alloc(NStaticVarData {
                name: String::new(),
                ty: KTy::Unresolved,
                location: keyword.location(&nx.tokens()),
            });

            resolve_ty_opt(ty_opt.as_mut(), nx);
            resolve_expr_opt(init_opt.as_mut(), nx);

            if let Some(name) = name_opt {
                resolve_name_def(*name, NName::StaticVar(n_static_var), nx);

                nx.res.static_vars[n_static_var].name = name.text(&nx.names).to_string();
            }

            let ty = match ty_opt {
                Some(p_ty) => {
                    let ty = gen_ty(p_ty, &nx.res.names);
                    if !ty.is_primitive() {
                        error_on_token(*keyword, "定数はプリミティブ型でなければいけません", nx);
                    }
                    ty
                }
                None => KTy::Unresolved,
            };
            n_static_var.of_mut(&mut nx.res.static_vars).ty = ty;
        }
        PDecl::Fn(PFnDecl {
            vis_opt,
            param_list_opt,
            result_ty_opt,
            block_opt,
            fn_id_opt,
            ..
        }) => {
            let k_fn = KFn::from_index(fn_id_opt.unwrap());

            let vis_opt = vis_opt.as_ref().map(|(vis, _)| *vis);
            let mut params = vec![];
            let mut result_ty = KTy::Unresolved;

            nx.enter_fn(k_fn, |nx| {
                nx.enter_scope(|nx| {
                    resolve_param_list_opt(param_list_opt.as_mut(), &mut params, nx);
                    resolve_ty_opt(result_ty_opt.as_mut(), nx);
                    result_ty = result_ty_opt
                        .as_ref()
                        .map_or(KTy::Unit, |ty| gen_ty(ty, &nx.res.names));

                    resolve_block_opt(block_opt.as_mut(), nx);
                });
            });

            let fn_data = &mut nx.res.fns[k_fn];
            fn_data.vis_opt = vis_opt;
            fn_data.params = params;
            fn_data.result_ty = result_ty;
        }
        PDecl::ExternFn(PExternFnDecl {
            param_list_opt,
            result_ty_opt,
            extern_fn_id_opt,
            ..
        }) => {
            let extern_fn = KExternFn::from_index(extern_fn_id_opt.unwrap());
            let parent_fn = replace(&mut nx.parent_fn, Some(NParentFn::ExternFn(extern_fn)));
            let parent_local_vars = take(&mut nx.parent_local_vars);
            let mut params = vec![];
            let mut result_ty = KTy::Unresolved;

            nx.enter_scope(|nx| {
                resolve_param_list_opt(param_list_opt.as_mut(), &mut params, nx);
                resolve_ty_opt(result_ty_opt.as_mut(), nx);

                result_ty = result_ty_opt
                    .as_ref()
                    .map_or(KTy::Unit, |ty| gen_ty(ty, &nx.res.names));
            });

            nx.parent_fn = parent_fn;
            let local_vars = replace(&mut nx.parent_local_vars, parent_local_vars);

            let mut extern_fn_data = &mut nx.res.extern_fns[extern_fn];
            extern_fn_data.params = params;
            extern_fn_data.result_ty = result_ty;
            extern_fn_data.local_vars = local_vars;
        }
        PDecl::Enum(PEnumDecl { variants, .. }) => {
            for variant in variants {
                resolve_variant2(variant, nx);
            }
        }
        PDecl::Struct(PStructDecl { variant_opt, .. }) => {
            if let Some(variant) = variant_opt {
                resolve_variant2(variant, nx);
            }
        }
        PDecl::Use(_) => {}
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

pub(crate) fn resolve_name(p_root: &mut PRoot, logger: DocLogger) -> NameResolution {
    let mut nx = {
        // FIXME: clone しない
        let tokens = Rc::new(p_root.tokens.clone());
        let names = p_root.names.clone();
        Nx::new(tokens, names, logger)
    };

    nx.res.names = VecArena::from_iter(nx.names.iter().map(|_| NName::Unresolved));
    nx.res.occurrences = VecArena::from_iter(
        nx.names
            .iter()
            .map(|_| (NAbsName::Unresolved, DefOrUse::Use)),
    );

    resolve_decls(&mut p_root.decls, &mut nx);

    nx.res
}
