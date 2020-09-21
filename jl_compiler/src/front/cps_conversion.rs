//! 構文木から CPS ノードのもとになる命令列を生成する処理

use crate::{
    cps::*,
    front::name_resolution::*,
    logs::DocLogger,
    parse::*,
    source::{Doc, Loc},
    token::{eval_number, LitErr},
    utils::VecArena,
};
use std::{
    collections::HashMap,
    collections::HashSet,
    iter::once,
    mem::{swap, take},
};

struct KLoopData {
    break_label: KLabel,
    continue_label: KLabel,
}

type AfterLval = KTerm;
type AfterRval = KTerm;
type AfterJump = KTerm;

/// 式の型に与えられる制約。
#[allow(unused)]
#[derive(Copy, Clone)]
enum TyExpect<'a> {
    /// 未実装部分
    Todo,
    Exact(&'a KTy2),
}

impl<'a> TyExpect<'a> {
    pub(crate) fn from(ty: &'a KTy2) -> Self {
        if ty.is_unresolved() {
            Self::Todo
        } else {
            TyExpect::Exact(ty)
        }
    }
}

fn new_unit_term(loc: Loc) -> AfterRval {
    KTerm::Unit { loc }
}

fn new_never_term(loc: Loc) -> AfterRval {
    // FIXME: the type is ! (never)
    KTerm::Unit { loc }
}

fn ty_expect_as_number(ty_expect: TyExpect) -> Option<KNumberTy> {
    match ty_expect {
        TyExpect::Exact(KTy2::Number(it)) => Some(*it),
        _ => None,
    }
}

fn convert_number_lit(
    token: PToken,
    ty_expect: TyExpect,
    tokens: &PTokens,
    doc: Doc,
    logger: &DocLogger,
) -> AfterRval {
    let text = token.text(tokens).to_string();
    let cause = KTermCause::Token(doc, token);
    let loc = cause.loc();

    match eval_number(&text) {
        Ok((_, number_ty)) => {
            let ty = KTy2::Number(number_ty);
            match number_ty {
                KNumberTy::F32 | KNumberTy::F64 | KNumberTy::FNN => KTerm::Float { text, ty, loc },
                KNumberTy::C8 | KNumberTy::C16 | KNumberTy::C32 | KNumberTy::CNN => {
                    KTerm::Char { text, ty, loc }
                }
                KNumberTy::UNN => {
                    if let Some(ty) = ty_expect_as_number(ty_expect) {
                        return KTerm::Int {
                            text,
                            ty: KTy2::Number(ty),
                            cause,
                        };
                    }

                    // FIXME: 後続のパスが UNN をうまく処理できなくて、unsigned long long になってしまう。いまのところ、ここで i32 にしておく
                    KTerm::Int {
                        text,
                        ty: KTy2::I32,
                        cause,
                    }
                }
                _ => KTerm::Int { text, ty, cause },
            }
        }
        Err(err) => {
            let message = match err {
                LitErr::Flow => "不正な値です",
                LitErr::UnknownSuffix => "不正なサフィックスです",
            };
            logger.error(PLoc::new(token), message);
            new_never_term(cause.loc())
        }
    }
}

// =============================================================================
// V2
// =============================================================================

#[derive(Default)]
struct KLabelConstruction {
    name: String,
    params: Vec<KVarTerm>,
    body: Vec<KNode>,
}

/// CPS 変換の文脈
struct Xx<'a> {
    // state
    mod_data: KModData,
    label: KLabel,
    nodes: Vec<KNode>,
    local_vars: KLocalVarArena,
    labels: VecArena<KLabelTag, KLabelConstruction>,
    ty_env: KTyEnv,
    /// return のターゲットとなる関数
    fn_opt: Option<KFn>,
    /// break/continue のターゲットとなるループ
    loop_opt: Option<KLoopData>,

    // read:
    doc: Doc,
    tokens: &'a PTokens,
    ast: &'a ATree,
    name_referents: &'a NameReferents,
    name_symbols: &'a mut NameSymbols,
    mod_outline: &'a KModOutline,
    logger: &'a DocLogger,
}

impl<'a> Xx<'a> {
    fn new(
        doc: Doc,
        tokens: &'a PTokens,
        ast: &'a ATree,
        name_referents: &'a NameReferents,
        name_symbols: &'a mut NameSymbols,
        mod_outline: &'a KModOutline,
        logger: &'a DocLogger,
    ) -> Self {
        let mut labels = VecArena::new();

        let toplevel = labels.alloc(KLabelConstruction {
            name: String::new(),
            params: vec![],
            body: vec![],
        });

        Self {
            label: toplevel,
            nodes: vec![],
            local_vars: KLocalVarArena::new(),
            labels,
            ty_env: KTyEnv::new(),
            fn_opt: None,
            loop_opt: None,
            mod_data: KModData::default(),
            // read:
            doc,
            tokens,
            ast,
            name_referents,
            name_symbols,
            mod_outline,
            logger,
        }
    }

    fn do_out_fn<A>(&mut self, f: impl FnOnce(&mut Xx) -> A) -> A {
        let label = self.label;
        let nodes = take(&mut self.nodes);
        let ty_env = take(&mut self.ty_env);
        let labels = take(&mut self.labels);
        let local_vars = take(&mut self.local_vars);
        let fn_opt = self.fn_opt.take();
        let loop_opt = self.loop_opt.take();

        let result = f(self);

        self.label = label;
        self.nodes = nodes;
        self.ty_env = ty_env;
        self.labels = labels;
        self.local_vars = local_vars;
        self.fn_opt = fn_opt;
        self.loop_opt = loop_opt;

        result
    }
}

fn path_resolution_context<'a>(xx: &'a mut Xx) -> PathResolutionContext<'a> {
    PathResolutionContext {
        tokens: xx.tokens,
        ast: xx.ast,
        name_referents: xx.name_referents,
        name_symbols: xx.name_symbols,
        mod_outline: xx.mod_outline,
    }
}

#[cfg(skip)]
fn local_context<'a>(xx: &'a Xx) -> (&'a KModOutline, Option<(&'a KLocalArena, &'a KLabelArena)>) {
    (
        xx.mod_outline,
        // FIXME: ラベルがない。xx.fx_opt.as_ref().map(|fx| (&xx.local_vars, &fx.labels)),
        None,
    )
}

fn error_unresolved_ty(loc: PLoc, logger: &DocLogger) {
    logger.error(loc, "これは型の名前だと思いますが、定義が見つかりません。")
}

fn error_unresolved_value(loc: PLoc, logger: &DocLogger) {
    logger.error(loc, "これは値の名前だと思いますが、定義が見つかりません。");
}

fn error_unsupported_path_ty(loc: PLoc, logger: &DocLogger) {
    logger.error(loc, "パスによる型の指定は未実装");
}

fn error_ty_arg_arity(loc: PLoc, logger: &DocLogger) {
    logger.error(loc, "型引数の個数が一致しません。");
}

fn error_invalid_ty_args(loc: PLoc, logger: &DocLogger) {
    logger.error(loc, "型引数は指定できません。");
}

fn error_expected_record_ty(loc: PLoc, logger: &DocLogger) {
    logger.error(loc, "これはレコードでなければいけません。");
}

fn error_rval_used_as_lval(loc: PLoc, logger: &DocLogger) {
    logger.error(loc, "この式は左辺値ではありません。参照元や代入先は、変数や配列の要素など、左辺値でなければいけません。");
}

fn error_no_such_field(loc: PLoc, logger: &DocLogger) {
    logger.error(loc, "この名前のフィールドはありません。");
}

fn error_redundant_field(loc: PLoc, logger: &DocLogger) {
    logger.error(loc, "このフィールドはすでに指定されています。");
}

fn error_missed_fields<'a>(names: impl Iterator<Item = &'a str>, loc: PLoc, logger: &DocLogger) {
    logger.error(
        loc,
        format!(
            "フィールドへの割り当てが不足しています: '{}'",
            names.collect::<Vec<_>>().join("', '")
        ),
    );
}

fn error_break_out_of_loop(loc: PLoc, logger: &DocLogger) {
    logger.error(loc, "ループの外では break を使えません。");
}

fn error_continue_out_of_loop(loc: PLoc, logger: &DocLogger) {
    logger.error(loc, "ループの外では continue を使えません。");
}

fn error_return_out_of_fn(loc: PLoc, logger: &DocLogger) {
    logger.error(loc, "関数の外では return を使えません。");
}

fn error_empty_match(loc: PLoc, logger: &DocLogger) {
    logger.error(loc, "空の match は未実装です。");
}

// -----------------------------------------------
// 型
// -----------------------------------------------

pub(crate) struct TyResolver<'a> {
    pub(crate) ast: &'a ATree,
    pub(crate) name_referents: &'a NameReferents,
    pub(crate) name_symbols: &'a NameSymbols,
    pub(crate) logger: &'a DocLogger,
}

fn new_ty_resolver<'a>(xx: &'a Xx<'_>) -> TyResolver<'a> {
    TyResolver {
        ast: xx.ast,
        name_referents: xx.name_referents,
        name_symbols: xx.name_symbols,
        logger: xx.logger,
    }
}

fn do_convert_name_ty(ty_id: ATyId, name: ANameId, xx: &TyResolver) -> KTy {
    if name.of(xx.ast.names()).is_qualified() {
        error_unsupported_path_ty(name.loc(), xx.logger);
    }

    match resolve_ty_name(name, xx.name_referents, xx.name_symbols) {
        Some(ty) => ty,
        None => {
            error_unresolved_ty(name.loc(), xx.logger);
            KTy::Unresolved {
                cause: KTyCause::NameUnresolved(ty_id),
            }
        }
    }
}

fn do_convert_ty(ty_id: ATyId, ty: &ATy, xx: &TyResolver) -> KTy {
    match ty {
        ATy::Name(name) => do_convert_name_ty(ty_id, *name, xx),
        ATy::App(name, ty_args) => {
            let ty = Box::new(do_convert_name_ty(ty_id, *name, xx));
            let ty_args = ty_args.iter().map(|ty| convert_ty(ty, xx)).collect();
            KTy::App { ty, ty_args }
        }
        ATy::InferTy => {
            // FIXME: メタ変数にする。シグネチャだったらエラーにする。
            error_unresolved_ty(ty_id.loc(), xx.logger);
            KTy::Unresolved {
                cause: KTyCause::InferTy(ty_id),
            }
        }
        ATy::Never => KTy::Never,
        ATy::Unit => KTy::Unit,
        ATy::Ptr(APtrTy { mut_opt, ty_opt }) => {
            let k_mut = mut_opt.unwrap_or(KMut::Const);
            let base_ty = convert_ty_opt(*ty_opt, xx);
            base_ty.into_ptr(k_mut)
        }
        ATy::Fn(AFnTy {
            param_tys,
            result_ty_opt,
        }) => {
            let param_tys = param_tys.iter().map(|&ty| convert_ty(ty, xx)).collect();
            let result_ty = Box::new(convert_ty_or_unit(*result_ty_opt, xx));
            KTy::Fn {
                ty_params: vec![],
                param_tys,
                result_ty,
            }
        }
    }
}

pub(crate) fn convert_ty(ty_id: ATyId, xx: &TyResolver) -> KTy {
    let ty = ty_id.of(xx.ast.tys());
    do_convert_ty(ty_id, ty, xx)
}

pub(crate) fn convert_ty_opt(ty_opt: Option<ATyId>, xx: &TyResolver) -> KTy {
    match ty_opt {
        Some(ty) => convert_ty(ty, xx),
        None => KTy::Unresolved {
            cause: KTyCause::Miss,
        },
    }
}

pub(crate) fn convert_ty_or_unit(ty_opt: Option<ATyId>, xx: &TyResolver) -> KTy {
    match ty_opt {
        Some(ty) => convert_ty(ty, xx),
        None => KTy::Unit,
    }
}

// -----------------------------------------------
// パターン
// -----------------------------------------------

// 入れ子のパターンはまだコンパイルできない
enum Branch {
    Case(KTerm),
    Default(KVarTerm),
}

fn convert_wildcard_pat_as_cond(token: PToken, xx: &mut Xx) -> Branch {
    let term = {
        let cause = KVarTermCause::WildcardPat(xx.doc, token);
        fresh_var("_", cause, xx)
    };
    Branch::Default(term)
}

fn emit_default_branch(name: ANameId, xx: &mut Xx) -> Branch {
    if name.of(xx.ast.names()).is_qualified() {
        error_unresolved_value(name.loc(), &xx.logger);
    }

    let term = {
        let cause = KVarTermCause::NameDef(xx.doc, name);
        fresh_var(&name.of(xx.ast.names()).text, cause, xx)
    };
    Branch::Default(term)
}

fn convert_name_pat_as_cond(name: ANameId, xx: &mut Xx) -> Branch {
    let loc = name.loc().to_loc(xx.doc);

    let value = match resolve_value_path(name, path_resolution_context(xx)) {
        Some(it) => it,
        None => return emit_default_branch(name, xx),
    };

    match value {
        KLocalValue::Alias(alias) => {
            // FIXME: エイリアスが const などを指している可能性があるので、shadowing とはみなせない。Rust と挙動が異なる
            Branch::Case(KTerm::Alias { alias, loc })
        }
        KLocalValue::Const(k_const) => Branch::Case(KTerm::Const { k_const, loc }),
        KLocalValue::UnitLikeStruct(k_struct) => Branch::Case(KTerm::RecordTag { k_struct, loc }),
        _ => emit_default_branch(name, xx),
    }
}

fn convert_name_pat_as_assign(name_id: ANameId, cond: &KTerm, term: KTerm, xx: &mut Xx) {
    let symbol = match term {
        KTerm::Name(symbol) if symbol.local_var.name(&xx.local_vars) == "_" => return,
        KTerm::Name(it) => it,
        _ => return,
    };

    xx.name_symbols
        .insert(name_id, NameSymbol::LocalVar(symbol.local_var));

    let loc = name_id.loc().to_loc(xx.doc);
    xx.nodes
        .push(new_let_node(cond.clone(), symbol, new_cont(), loc));
}

fn convert_record_pat_as_cond(pat: &ARecordPat, loc: Loc, xx: &mut Xx) -> AfterRval {
    let k_struct = match resolve_ty_path(pat.left, path_resolution_context(xx)) {
        Some(KTy2::Struct(k_struct)) => k_struct,
        _ => {
            error_expected_record_ty(PLoc::from_loc(loc), xx.logger);
            return new_error_term(loc);
        }
    };
    KTerm::RecordTag { k_struct, loc }
}

fn do_convert_pat_as_cond(pat_id: APatId, pat: &APat, xx: &mut Xx) -> Branch {
    let loc = pat_id.loc().to_loc(xx.doc);
    let term = match pat {
        APat::Unit => KTerm::Unit { loc },
        APat::True(_) => KTerm::True { loc },
        APat::False(_) => KTerm::False { loc },
        APat::Char(token) => convert_char_expr(*token, xx.doc, xx.tokens),
        APat::Number(token) => {
            convert_number_lit(*token, TyExpect::Todo, xx.tokens, xx.doc, xx.logger)
        }
        APat::Str(token) => convert_str_expr(*token, xx.doc, xx.tokens),
        APat::Wildcard(token) => return convert_wildcard_pat_as_cond(*token, xx),
        APat::Name(name) => return convert_name_pat_as_cond(*name, xx),
        APat::Record(record_pat) => convert_record_pat_as_cond(record_pat, loc, xx),
    };
    Branch::Case(term)
}

fn do_convert_pat_as_assign(pat: &APat, cond: &KTerm, term: KTerm, xx: &mut Xx) {
    match pat {
        APat::Name(name) => convert_name_pat_as_assign(*name, cond, term, xx),
        _ => {}
    }
}

fn convert_pat_as_cond(pat_id: APatId, xx: &mut Xx) -> Branch {
    let pat = pat_id.of(xx.ast.pats());
    do_convert_pat_as_cond(pat_id, pat, xx)
}

fn convert_pat_opt_as_cond(pat_opt: Option<APatId>, loc: PLoc, xx: &mut Xx) -> Branch {
    match pat_opt {
        Some(pat) => convert_pat_as_cond(pat, xx),
        None => Branch::Case(new_error_term(Loc::new(xx.doc, loc))),
    }
}

fn convert_pat_as_assign(pat_id: APatId, cond: &KTerm, term: KTerm, xx: &mut Xx) {
    let pat = pat_id.of(xx.ast.pats());
    do_convert_pat_as_assign(pat, cond, term, xx)
}

fn convert_pat_opt_as_assign(pat_opt: Option<APatId>, cond: &KTerm, term: KTerm, xx: &mut Xx) {
    if let Some(pat) = pat_opt {
        convert_pat_as_assign(pat, cond, term, xx);
    }
}

// -----------------------------------------------
// 式 (項とノード)
// -----------------------------------------------

fn new_error_term(loc: Loc) -> AfterRval {
    KTerm::Unit { loc }
}

fn new_error_node(loc: Loc) -> KNode {
    KNode {
        prim: KPrim::Stuck,
        tys: vec![],
        args: vec![],
        results: vec![],
        conts: vec![new_cont()],
        loc,
    }
}

fn new_cont() -> KNode {
    KNode::default()
}

fn fold_nodes(mut nodes: Vec<KNode>) -> KNode {
    let mut stack = vec![];

    while let Some(mut node) = nodes.pop() {
        // スタックに積まれているノードを継続として持たせる。
        let n = node.conts.len();
        for (slot, cont) in node
            .conts
            .iter_mut()
            .zip(stack.drain(stack.len() - n..).rev())
        {
            *slot = cont;
        }

        stack.push(node);
    }

    stack.pop().unwrap_or_default()
}

/// break した後や条件分岐から抜けた後を表すラベル。引数を1個持つ。
struct BreakLabel {
    label: KLabel,
    result: KVarTerm,
}

/// continue で戻ってくる先の、ループの先頭を表すラベル。引数を持たない。
struct ContinueLabel {
    label: KLabel,
}

fn new_break_label(hint: &str, result: KVarTerm, xx: &mut Xx) -> BreakLabel {
    let label = xx.labels.alloc(KLabelConstruction {
        name: hint.to_string(),
        params: vec![result],
        body: vec![],
    });
    BreakLabel { label, result }
}

fn new_continue_label(hint: &str, xx: &mut Xx) -> ContinueLabel {
    let label = xx.labels.alloc(KLabelConstruction {
        name: hint.to_string(),
        params: vec![],
        body: vec![],
    });
    ContinueLabel { label }
}

/// 現在のラベルの変換を一時中断する。
///
/// 設計: これを直接使って変換を記述すると、コードの構造がとても分かりにくくなる。
///         代わりに、構造化された API である do_with_break/do_with_continue を使う。
fn commit_label(xx: &mut Xx) {
    let nodes = take(&mut xx.nodes);
    xx.label.of_mut(&mut xx.labels).body.extend(nodes);
}

/// break のような進行方向へのジャンプを含む変換を行う。
///
/// loop からの break だけでなく、if/match から下に抜けるときのジャンプにも使える。
/// 変換の評価値は `result` に束縛すること。
fn do_with_break(
    break_label: BreakLabel,
    xx: &mut Xx,
    f: impl FnOnce(&mut Xx, KLabel),
) -> AfterRval {
    f(xx, break_label.label);
    commit_label(xx);

    xx.label = break_label.label;
    KTerm::Name(break_label.result)
}

/// continue のような進行方向と逆行するジャンプを含む変換を行う。
fn do_with_continue(
    continue_label: ContinueLabel,
    loc: Loc,
    xx: &mut Xx,
    f: impl FnOnce(&mut Xx, KLabel),
) {
    xx.nodes
        .push(new_jump_tail(continue_label.label, vec![], loc));
    commit_label(xx);

    xx.label = continue_label.label;
    f(xx, continue_label.label)
}

/// 分岐の CPS 変換を行う。
///
/// 変換前後でラベルを戻す必要がある。
fn do_in_branch(xx: &mut Xx, f: impl FnOnce(&mut Xx)) {
    let label = xx.label;
    commit_label(xx);

    f(xx);
    commit_label(xx);

    xx.label = label;
    assert_eq!(xx.nodes.len(), 0);
}

/// ループの本体の変換を行う。
///
/// ループの結果を束縛する一時変数 `result` と
/// break/continue のジャンプ先のラベルが生成される。
fn do_in_loop(
    hint: &str,
    loc: Loc,
    xx: &mut Xx,
    f: impl FnOnce(&mut Xx, KVarTerm, KLabel, KLabel),
) -> AfterRval {
    let result = fresh_var(hint, loc, xx);

    // continue → break の順で生成しないと型検査の順番がおかしくなる。
    let continue_label = new_continue_label("continue_", xx);
    let break_label = new_break_label("next", result, xx);

    do_with_break(break_label, xx, |xx, break_label| {
        do_with_continue(continue_label, loc, xx, |xx, continue_label| {
            let parent_loop_opt = xx.loop_opt.replace(KLoopData {
                break_label,
                continue_label,
            });

            f(xx, result, break_label, continue_label);

            xx.loop_opt = parent_loop_opt;
        });
    })
}

fn convert_char_expr(token: PToken, doc: Doc, tokens: &PTokens) -> AfterRval {
    KTerm::Char {
        text: token.text(tokens).to_string(),
        ty: KTy2::C8,
        loc: KTermCause::Token(doc, token).loc(),
    }
}

fn convert_str_expr(token: PToken, doc: Doc, tokens: &PTokens) -> AfterRval {
    KTerm::Str {
        text: token.text(tokens).to_string(),
        loc: KTermCause::Token(doc, token).loc(),
    }
}

fn emit_unit_like_struct(
    k_struct: KStruct,
    result: KVarTerm,
    loc: Loc,
    nodes: &mut Vec<KNode>,
) -> AfterRval {
    let ty = KTy2::Struct(k_struct);

    nodes.push(new_record_node(ty, vec![], result, new_cont(), loc));
    KTerm::Name(result)
}

fn fresh_var(hint: &str, cause: impl Into<KVarTermCause>, xx: &mut Xx) -> KVarTerm {
    let cause = cause.into();
    let loc = cause.loc();
    let local_var = xx
        .local_vars
        .alloc(KLocalVarData::new(hint.to_string(), loc));
    KVarTerm { local_var, cause }
}

fn convert_name_expr(name: ANameId, xx: &mut Xx) -> AfterRval {
    let loc = name.loc().to_loc(xx.doc);
    let cause = KVarTermCause::NameUse(xx.doc, name);

    let value = match resolve_value_path(name, path_resolution_context(xx)) {
        Some(it) => it,
        None => {
            error_unresolved_value(name.loc(), xx.logger);
            return new_error_term(loc);
        }
    };

    match value {
        KLocalValue::Alias(alias) => KTerm::Alias { alias, loc },
        KLocalValue::LocalVar(local_var) => KTerm::Name(KVarTerm { local_var, cause }),
        KLocalValue::Const(k_const) => KTerm::Const { k_const, loc },
        KLocalValue::StaticVar(static_var) => KTerm::StaticVar { static_var, loc },
        KLocalValue::Fn(k_fn) => KTerm::Fn {
            k_fn,
            ty: k_fn
                .ty(&xx.mod_outline.fns)
                .to_ty2(xx.mod_outline, &mut xx.ty_env),
            loc,
        },
        KLocalValue::ExternFn(extern_fn) => KTerm::ExternFn { extern_fn, loc },
        KLocalValue::UnitLikeStruct(k_struct) => {
            let name = k_struct.name(&xx.mod_outline.structs);
            let result = fresh_var(name, cause, xx);
            emit_unit_like_struct(k_struct, result, loc, &mut xx.nodes)
        }
    }
}

fn convert_name_lval(name: ANameId, k_mut: KMut, xx: &mut Xx) -> AfterLval {
    let loc = name.loc().to_loc(xx.doc);
    let cause = KVarTermCause::NameUse(xx.doc, name);

    let value = match resolve_value_path(name, path_resolution_context(xx)) {
        Some(it) => it,
        None => {
            error_unresolved_value(name.loc(), xx.logger);
            return new_error_term(loc);
        }
    };

    let term = match value {
        KLocalValue::Alias(alias) => KTerm::Alias { alias, loc },
        KLocalValue::LocalVar(local_var) => KTerm::Name(KVarTerm { local_var, cause }),
        KLocalValue::StaticVar(static_var) => KTerm::StaticVar { static_var, loc },
        KLocalValue::Const(_)
        | KLocalValue::Fn(_)
        | KLocalValue::ExternFn(_)
        | KLocalValue::UnitLikeStruct(_) => {
            error_rval_used_as_lval(name.loc(), xx.logger);
            return new_error_term(loc);
        }
    };

    match k_mut {
        KMut::Const => {
            let result = fresh_var("ref", cause, xx);
            xx.nodes.push(new_ref_node(term, result, new_cont(), loc));
            KTerm::Name(result)
        }
        KMut::Mut => {
            let result = fresh_var("refmut", cause, xx);
            xx.nodes
                .push(new_ref_mut_node(term, result, new_cont(), loc));
            KTerm::Name(result)
        }
    }
}

fn convert_ty_app_expr(expr: &ATyAppExpr, xx: &mut Xx) -> AfterRval {
    let name = expr.left;
    let loc = name.loc().to_loc(xx.doc);

    // __align_of/__size_of
    if let Some(kind) = KTyProperty::from_str(name.of(xx.ast.names()).text()) {
        if expr.ty_args.len() != 1 {
            error_ty_arg_arity(name.loc(), xx.logger);
            return new_error_term(loc);
        }

        let ty = convert_ty(expr.ty_args.iter().next().unwrap(), &new_ty_resolver(xx));
        return KTerm::TyProperty { kind, ty, loc };
    }

    let value = match resolve_value_path(name, path_resolution_context(xx)) {
        Some(it) => it,
        None => {
            error_unresolved_value(name.loc(), xx.logger);
            return new_error_term(loc);
        }
    };

    match value {
        KLocalValue::Fn(k_fn) => {
            let fn_data = k_fn.of(&xx.mod_outline.fns);
            if fn_data.ty_params.is_empty() {
                error_invalid_ty_args(name.loc(), xx.logger);
                return new_error_term(loc);
            }

            if expr.ty_args.len() != fn_data.ty_params.len() {
                error_ty_arg_arity(name.loc(), xx.logger);
                return new_error_term(loc);
            }

            let ty_args = fn_data
                .ty_params
                .iter()
                .zip(expr.ty_args.iter())
                .map(|(ty_param, ty_arg)| {
                    let ty_param = ty_param.name.to_string();
                    let ty_arg = convert_ty(ty_arg, &new_ty_resolver(xx))
                        .to_ty2(xx.mod_outline, &mut xx.ty_env);
                    (ty_param, ty_arg)
                })
                .collect::<HashMap<_, _>>();
            let ty = fn_data.ty().substitute(&xx.mod_outline, &ty_args);

            KTerm::Fn { k_fn, ty, loc }
        }
        _ => {
            xx.logger
                .error(name.loc(), "ジェネリックな関数以外の型適用式は未実装です");
            new_error_term(loc)
        }
    }
}

enum FieldExhaustivityError {
    NoSuchField {
        #[allow(unused)]
        item_index: usize,
    },
    Redundant {
        #[allow(unused)]
        item_index: usize,
    },
    Missed(KField),
}

fn calculate_field_ordering<'a, T>(
    items: &[T],
    fields: &[KField],
    field_arena: &'a KFieldArena,
    name_fn: impl Fn(&T) -> &'a str,
) -> Result<Vec<usize>, Vec<FieldExhaustivityError>> {
    // perm[item_index] = Some(field_index) or None
    let mut perm = vec![0; items.len()];
    // inverse[field_index] = Some(item_index) or None
    let mut inverse = vec![None; fields.len()];
    let mut errors = vec![];

    for (item_index, item) in items.iter().enumerate() {
        let name = name_fn(item);
        let i = match fields
            .iter()
            .position(|field| field.name(field_arena) == name)
        {
            Some(it) => it,
            None => {
                errors.push(FieldExhaustivityError::NoSuchField { item_index });
                continue;
            }
        };

        if let Some(first) = inverse[i] {
            errors.push(FieldExhaustivityError::Redundant { item_index: first });
            continue;
        }

        perm[item_index] = i;
        inverse[i] = Some(item_index);
    }

    for (item_index_opt, &field) in inverse.iter().zip(fields.iter()) {
        if item_index_opt.is_none() {
            errors.push(FieldExhaustivityError::Missed(field));
        }
    }

    if !errors.is_empty() {
        return Err(errors);
    }

    debug_assert_eq!(
        perm.iter()
            .filter(|&&i| i < fields.len())
            .collect::<HashSet<_>>()
            .len(),
        fields.len()
    );
    Ok(perm)
}

fn report_record_expr_errors(
    fields: &[KField],
    errors: &[FieldExhaustivityError],
    loc: Loc,
    mod_outline: &KModOutline,
    logger: &DocLogger,
) {
    // FIXME: フィールドの位置情報を取る。
    let mut missed = vec![];

    for error in errors {
        match error {
            FieldExhaustivityError::NoSuchField { .. } => {
                error_no_such_field(PLoc::from_loc(loc), logger);
            }
            FieldExhaustivityError::Redundant { .. } => {
                error_redundant_field(PLoc::from_loc(loc), logger);
            }
            FieldExhaustivityError::Missed(field) => missed.push(field),
        }
    }

    if !missed.is_empty() {
        error_missed_fields(
            fields.iter().map(|field| field.name(&mod_outline.fields)),
            PLoc::from_loc(loc),
            logger,
        );
    }
}

fn do_convert_record_expr(expr: &ARecordExpr, loc: Loc, xx: &mut Xx) -> Option<KVarTerm> {
    let k_struct = match resolve_ty_path(expr.left, path_resolution_context(xx)) {
        Some(KTy2::Struct(k_struct)) => k_struct,
        _ => {
            error_expected_record_ty(PLoc::from_loc(loc), xx.logger);
            return None;
        }
    };

    let mut ty_is_generic = false;
    let loc_ty_args = PLoc::TokenBehind(expr.left.of(xx.ast.names()).token);
    let ty = match &k_struct.of(&xx.mod_outline.structs).parent {
        KStructParent::Struct { ty_params } if !ty_params.is_empty() => {
            ty_is_generic = true;

            let mut ty_args_opt = None;
            if let Some(ty_args) = expr.ty_args_opt.as_ref() {
                if ty_args.len() != ty_params.len() {
                    error_ty_arg_arity(loc_ty_args, xx.logger);
                } else {
                    ty_args_opt = Some(
                        ty_params
                            .iter()
                            .zip(ty_args.iter())
                            .map(|(ty_param, ty_arg)| {
                                let ty_param = ty_param.name.to_string();
                                let ty_arg = convert_ty(ty_arg, &new_ty_resolver(xx))
                                    .to_ty2_poly(xx.mod_outline);
                                (ty_param, ty_arg)
                            })
                            .collect::<HashMap<_, _>>(),
                    );
                }
            }

            let ty_args = ty_args_opt.unwrap_or_else(|| {
                ty_params
                    .iter()
                    .map(|ty_param| {
                        let meta_ty = xx.ty_env.alloc(KMetaTyData::new_fresh(ty_param.loc));
                        (ty_param.name.to_string(), KTy2::Meta(meta_ty))
                    })
                    .collect::<HashMap<_, _>>()
            });

            KTy2::App { k_struct, ty_args }
        }
        _ => KTy2::Struct(k_struct),
    };

    if expr.ty_args_opt.is_some() && !ty_is_generic {
        error_invalid_ty_args(loc_ty_args, xx.logger);
    }

    let fields = &k_struct.of(&xx.mod_outline.structs).fields;
    let perm =
        match calculate_field_ordering(&expr.fields, fields, &xx.mod_outline.fields, |field_expr| {
            &field_expr.field_name.of(xx.ast.names()).text
        }) {
            Ok(it) => it,
            Err(errors) => {
                report_record_expr_errors(fields, &errors, loc, xx.mod_outline, xx.logger);
                return None;
            }
        };

    let mut args = vec![KTerm::Unit { loc }; fields.len()];
    for (i, field_expr) in expr.fields.iter().enumerate() {
        args[perm[i]] = convert_expr_opt(field_expr.value_opt, TyExpect::Todo, loc, xx);
    }

    let result = fresh_var(&k_struct.of(&xx.mod_outline.structs).name, loc, xx);
    xx.nodes
        .push(new_record_node(ty, args, result, new_cont(), loc));
    Some(result)
}

fn convert_record_expr(expr: &ARecordExpr, loc: Loc, xx: &mut Xx) -> AfterRval {
    match do_convert_record_expr(expr, loc, xx) {
        Some(result) => KTerm::Name(result),
        None => new_error_term(loc),
    }
}

// `&A { .. }`
fn convert_record_lval(expr: &ARecordExpr, loc: Loc, xx: &mut Xx) -> AfterLval {
    let arg = match do_convert_record_expr(expr, loc, xx) {
        Some(it) => it,
        None => return new_error_term(loc),
    };

    let result = fresh_var("ref", loc, xx);
    xx.nodes
        .push(new_ref_node(KTerm::Name(arg), result, new_cont(), loc));
    KTerm::Name(result)
}

// `x.field` ==> `*(&x)->field`
fn convert_field_expr(expr: &AFieldExpr, loc: Loc, xx: &mut Xx) -> AfterRval {
    let result = {
        let name = match expr.field_opt {
            Some(token) => token.text(xx.tokens),
            None => "_",
        };
        fresh_var(&name, loc, xx)
    };

    let field_ptr = convert_field_lval(expr, KMut::Const, loc, xx);
    xx.nodes
        .push(new_deref_node(field_ptr, result, new_cont(), loc));
    KTerm::Name(result)
}

// `&x.field` ==> `&(&x)->field`
fn convert_field_lval(expr: &AFieldExpr, k_mut: KMut, loc: Loc, xx: &mut Xx) -> AfterLval {
    let (name, field_loc) = match expr.field_opt {
        Some(token) => {
            let name = token.text(xx.tokens).to_string();
            let loc = Loc::new(xx.doc, PLoc::Token(token));
            (name, loc)
        }
        None => ("_".to_string(), loc),
    };
    let result = fresh_var(&format!("{}_ptr", name), loc, xx);

    let left = convert_lval(expr.left, k_mut, TyExpect::Todo, xx);
    xx.nodes.push(new_field_node(
        left,
        name,
        field_loc,
        k_mut,
        result,
        new_cont(),
        loc,
    ));
    KTerm::Name(result)
}

fn convert_call_expr(
    call_expr: &ACallLikeExpr,
    _ty_expect: TyExpect,
    loc: Loc,
    xx: &mut Xx,
) -> AfterRval {
    let result = fresh_var("call_result", loc, xx);
    let left = convert_expr(call_expr.left, TyExpect::Todo, xx);

    let mut args = Vec::with_capacity(call_expr.args.len() + 1);
    args.push(left);
    args.extend(
        call_expr
            .args
            .iter()
            .map(|arg| convert_expr(arg, TyExpect::Todo, xx)),
    );

    xx.nodes.push(new_call_node(args, result, new_cont(), loc));
    KTerm::Name(result)
}

// `a[i]` ==> `*(a + i)`
fn convert_index_expr(
    expr: &ACallLikeExpr,
    _ty_expect: TyExpect,
    loc: Loc,
    xx: &mut Xx,
) -> AfterRval {
    let ptr = convert_index_lval(expr, TyExpect::Todo, loc, xx);

    let result = fresh_var("index_result", loc, xx);
    xx.nodes.push(new_deref_node(ptr, result, new_cont(), loc));
    KTerm::Name(result)
}

// `&a[i]` ==> `a + i`
fn convert_index_lval(
    expr: &ACallLikeExpr,
    _ty_expect: TyExpect,
    loc: Loc,
    xx: &mut Xx,
) -> AfterLval {
    let left = convert_expr(expr.left, TyExpect::Todo, xx);
    let right = if expr.args.len() == 1 {
        let right = expr.args.iter().next().unwrap();
        convert_expr(right, TyExpect::Todo, xx)
    } else {
        new_error_term(loc)
    };

    let result = fresh_var("indexed_ptr", loc, xx);
    xx.nodes
        .push(new_add_node(left, right, result, new_cont(), loc));
    KTerm::Name(result)
}

fn convert_cast_expr(expr: &ACastExpr, _ty_expect: TyExpect, loc: Loc, xx: &mut Xx) -> AfterRval {
    let ty = convert_ty_opt(expr.ty_opt, &new_ty_resolver(xx)).to_ty2_poly(xx.mod_outline);
    let arg = convert_expr(expr.left, TyExpect::from(&ty), xx);

    let result = fresh_var("cast", loc, xx);
    xx.nodes
        .push(new_cast_node(ty, arg, result, new_cont(), loc));
    KTerm::Name(result)
}

fn convert_unary_op_expr(
    expr: &AUnaryOpExpr,
    _ty_expect: TyExpect,
    loc: Loc,
    xx: &mut Xx,
) -> AfterRval {
    match expr.op {
        PUnaryOp::Deref => {
            let arg = convert_expr_opt(expr.arg_opt, TyExpect::Todo, loc, xx);
            let result = fresh_var("deref", loc, xx);
            xx.nodes.push(new_deref_node(arg, result, new_cont(), loc));
            KTerm::Name(result)
        }
        PUnaryOp::Ref => {
            let k_mut = expr.mut_opt.unwrap_or(KMut::Const);
            convert_lval_opt(expr.arg_opt, k_mut, TyExpect::Todo, loc, xx)
        }
        PUnaryOp::Minus => {
            let arg = convert_expr_opt(expr.arg_opt, TyExpect::Todo, loc, xx);
            let result = fresh_var("minus", loc, xx);
            xx.nodes.push(new_minus_node(arg, result, new_cont(), loc));
            KTerm::Name(result)
        }
        PUnaryOp::Not => {
            let arg = convert_expr_opt(expr.arg_opt, TyExpect::Todo, loc, xx);
            let result = fresh_var("not", loc, xx);
            xx.nodes.push(new_not_node(arg, result, new_cont(), loc));
            KTerm::Name(result)
        }
    }
}

fn convert_unary_op_lval(
    expr: &AUnaryOpExpr,
    _ty_expect: TyExpect,
    loc: Loc,
    xx: &mut Xx,
) -> AfterLval {
    match expr.op {
        PUnaryOp::Deref => {
            // `&*p` ==> `p`
            convert_expr_opt(expr.arg_opt, TyExpect::Todo, loc, xx)
        }
        PUnaryOp::Ref | PUnaryOp::Minus | PUnaryOp::Not => {
            error_rval_used_as_lval(PLoc::from_loc(loc), xx.logger);
            new_error_term(loc)
        }
    }
}

fn do_convert_assignment_expr(
    prim: KPrim,
    expr: &ABinaryOpExpr,
    loc: Loc,
    xx: &mut Xx,
) -> AfterRval {
    let left = convert_lval(expr.left, KMut::Mut, TyExpect::Todo, xx);
    let right = convert_expr_opt(expr.right_opt, TyExpect::Todo, loc, xx);

    xx.nodes
        .push(new_assignment_node(prim, left, right, new_cont(), loc));
    new_unit_term(loc)
}

fn do_convert_basic_binary_op_expr(
    prim: KPrim,
    expr: &ABinaryOpExpr,
    loc: Loc,
    xx: &mut Xx,
) -> AfterRval {
    let left = convert_expr(expr.left, TyExpect::Todo, xx);
    let right = convert_expr_opt(expr.right_opt, TyExpect::Todo, loc, xx);

    let result = fresh_var(&prim.hint_str(), loc, xx);
    xx.nodes.push(new_basic_binary_op_node(
        prim,
        left,
        right,
        result,
        new_cont(),
        loc,
    ));
    KTerm::Name(result)
}

// `p && q` ==> `if p { q } else { false }`
fn do_convert_log_and_expr(
    expr: &ABinaryOpExpr,
    ty_expect: TyExpect,
    loc: Loc,
    xx: &mut Xx,
) -> AfterRval {
    do_convert_if_expr(
        |xx| convert_expr(expr.left, TyExpect::Todo, xx),
        |xx| convert_expr_opt(expr.right_opt, TyExpect::Todo, loc, xx),
        |_| KTerm::False { loc },
        ty_expect,
        loc,
        xx,
    )
}

// `p || q` ==> `if p { true } else { q }`
fn do_convert_log_or_expr(
    expr: &ABinaryOpExpr,
    ty_expect: TyExpect,
    loc: Loc,
    xx: &mut Xx,
) -> AfterRval {
    do_convert_if_expr(
        |xx| convert_expr(expr.left, TyExpect::Todo, xx),
        |_| KTerm::True { loc },
        |xx| convert_expr_opt(expr.right_opt, TyExpect::Todo, loc, xx),
        ty_expect,
        loc,
        xx,
    )
}

fn convert_binary_op_expr(
    expr: &ABinaryOpExpr,
    ty_expect: TyExpect,
    loc: Loc,
    xx: &mut Xx,
) -> AfterRval {
    let on_assign = |prim: KPrim, xx: &mut Xx| do_convert_assignment_expr(prim, expr, loc, xx);
    let on_basic = |prim: KPrim, xx: &mut Xx| do_convert_basic_binary_op_expr(prim, expr, loc, xx);
    let on_bit = on_basic;
    let on_comparison = on_basic;

    match expr.op {
        PBinaryOp::Assign => on_assign(KPrim::Assign, xx),
        PBinaryOp::AddAssign => on_assign(KPrim::AddAssign, xx),
        PBinaryOp::SubAssign => on_assign(KPrim::SubAssign, xx),
        PBinaryOp::MulAssign => on_assign(KPrim::MulAssign, xx),
        PBinaryOp::DivAssign => on_assign(KPrim::DivAssign, xx),
        PBinaryOp::ModuloAssign => on_assign(KPrim::ModuloAssign, xx),
        PBinaryOp::BitAndAssign => on_assign(KPrim::BitAndAssign, xx),
        PBinaryOp::BitOrAssign => on_assign(KPrim::BitOrAssign, xx),
        PBinaryOp::BitXorAssign => on_assign(KPrim::BitXorAssign, xx),
        PBinaryOp::LeftShiftAssign => on_assign(KPrim::LeftShiftAssign, xx),
        PBinaryOp::RightShiftAssign => on_assign(KPrim::RightShiftAssign, xx),
        PBinaryOp::Add => on_basic(KPrim::Add, xx),
        PBinaryOp::Sub => on_basic(KPrim::Sub, xx),
        PBinaryOp::Mul => on_basic(KPrim::Mul, xx),
        PBinaryOp::Div => on_basic(KPrim::Div, xx),
        PBinaryOp::Modulo => on_basic(KPrim::Modulo, xx),
        PBinaryOp::BitAnd => on_bit(KPrim::BitAnd, xx),
        PBinaryOp::BitOr => on_bit(KPrim::BitOr, xx),
        PBinaryOp::BitXor => on_bit(KPrim::BitXor, xx),
        PBinaryOp::LeftShift => on_bit(KPrim::LeftShift, xx),
        PBinaryOp::RightShift => on_bit(KPrim::RightShift, xx),
        PBinaryOp::Equal => on_comparison(KPrim::Equal, xx),
        PBinaryOp::NotEqual => on_comparison(KPrim::NotEqual, xx),
        PBinaryOp::LessThan => on_comparison(KPrim::LessThan, xx),
        PBinaryOp::LessEqual => on_comparison(KPrim::LessEqual, xx),
        PBinaryOp::GreaterThan => on_comparison(KPrim::GreaterThan, xx),
        PBinaryOp::GreaterEqual => on_comparison(KPrim::GreaterEqual, xx),
        PBinaryOp::LogAnd => do_convert_log_and_expr(expr, ty_expect, loc, xx),
        PBinaryOp::LogOr => do_convert_log_or_expr(expr, ty_expect, loc, xx),
    }
}

fn convert_block_expr(decls: ADeclIds, _ty_expect: TyExpect, loc: Loc, xx: &mut Xx) -> AfterRval {
    convert_decls(decls.clone(), xx).unwrap_or_else(|| KTerm::Unit { loc })
}

fn convert_break_expr(expr: &AJumpExpr, _ty_expect: TyExpect, loc: Loc, xx: &mut Xx) -> AfterJump {
    let label_opt = xx.loop_opt.as_ref().map(|data| data.break_label);
    let label = match label_opt {
        Some(it) => it,
        None => {
            error_break_out_of_loop(PLoc::from_loc(loc), xx.logger);
            return new_error_term(loc);
        }
    };

    let arg = convert_expr_opt(expr.arg_opt, TyExpect::Todo, loc, xx);
    xx.nodes
        .push(new_jump_node(label, vec![arg], new_cont(), loc));
    new_never_term(loc)
}

fn convert_continue_expr(_ty_expect: TyExpect, loc: Loc, xx: &mut Xx) -> AfterJump {
    let label_opt = xx.loop_opt.as_ref().map(|data| data.continue_label);
    let label = match label_opt {
        Some(it) => it,
        None => {
            error_continue_out_of_loop(PLoc::from_loc(loc), xx.logger);
            return new_error_term(loc);
        }
    };

    xx.nodes.push(new_jump_node(
        label,
        vec![new_unit_term(loc)],
        new_cont(),
        loc,
    ));
    new_never_term(loc)
}

fn convert_return_expr(expr: &AJumpExpr, _ty_expect: TyExpect, loc: Loc, xx: &mut Xx) -> AfterJump {
    let arg = convert_expr_opt(expr.arg_opt, TyExpect::Todo, loc, xx);

    let node = match xx.fn_opt {
        Some(k_fn) => new_return_node(k_fn, arg, loc),
        None => {
            error_return_out_of_fn(PLoc::from_loc(loc), xx.logger);
            new_error_node(loc)
        }
    };

    xx.nodes.push(node);
    new_never_term(loc)
}

fn do_convert_if_expr(
    cond_fn: impl FnOnce(&mut Xx) -> AfterRval,
    body_fn: impl FnOnce(&mut Xx) -> AfterRval,
    alt_fn: impl FnOnce(&mut Xx) -> AfterRval,
    _ty_expect: TyExpect,
    loc: Loc,
    xx: &mut Xx,
) -> AfterRval {
    let result = fresh_var("if_result", loc, xx);
    // FIXME: next → if_next
    let next = new_break_label("next", result, xx);

    do_with_break(next, xx, |xx, break_label| {
        let cond = cond_fn(xx);
        xx.nodes
            .push(new_if_node(cond, new_cont(), new_cont(), loc));

        do_in_branch(xx, |xx| {
            let body = body_fn(xx);
            xx.nodes.push(new_jump_tail(break_label, once(body), loc));
        });

        do_in_branch(xx, |xx| {
            let alt = alt_fn(xx);
            xx.nodes.push(new_jump_tail(break_label, once(alt), loc));
        });
    })
}

fn convert_if_expr(expr: &AIfExpr, ty_expect: TyExpect, loc: Loc, xx: &mut Xx) -> AfterRval {
    do_convert_if_expr(
        |xx| convert_expr_opt(expr.cond_opt, TyExpect::Todo, loc, xx),
        |xx| convert_expr_opt(expr.body_opt, TyExpect::Todo, loc, xx),
        |xx| convert_expr_opt(expr.alt_opt, TyExpect::Todo, loc, xx),
        ty_expect,
        loc,
        xx,
    )
}

fn convert_match_expr(expr: &AMatchExpr, _ty_expect: TyExpect, loc: Loc, xx: &mut Xx) -> AfterRval {
    let cond = convert_expr_opt(expr.cond_opt, TyExpect::Todo, loc, xx);
    if expr.arms.is_empty() {
        error_empty_match(PLoc::from_loc(loc), xx.logger);
        return new_error_term(loc);
    }

    let result = fresh_var("match_result", loc, xx);
    let next = new_break_label("match_next", result, xx);

    do_with_break(next, xx, |xx, break_label| {
        let arms = expr
            .arms
            .iter()
            .map(|arm| {
                let term = match convert_pat_opt_as_cond(arm.pat_opt, arm.loc, xx) {
                    Branch::Case(term) => term,
                    Branch::Default(term) => KTerm::Name(term),
                };
                (arm, term)
            })
            .collect::<Vec<_>>();

        let switch_node = {
            let args = once(cond.clone())
                .chain(arms.iter().map(|(_, term)| term.clone()))
                .collect();
            let cont_count = expr.arms.len();
            new_switch_tail(args, vec![new_cont(); cont_count], loc)
        };
        xx.nodes.push(switch_node);

        for (arm, term) in arms {
            do_in_branch(xx, |xx| {
                convert_pat_opt_as_assign(arm.pat_opt, &cond, term, xx);
                let body = convert_expr_opt(arm.body_opt, TyExpect::Todo, loc, xx);
                let node = new_jump_tail(break_label, vec![body], loc);
                xx.nodes.push(node);
            });
        }
    })
}

// `while cond { body }` ==> `loop { if cond { body } else { break } }`
fn convert_while_expr(expr: &AWhileExpr, _ty_expect: TyExpect, loc: Loc, xx: &mut Xx) -> AfterRval {
    let unit_term = new_unit_term(loc);

    do_in_loop(
        "while_result",
        loc,
        xx,
        |xx, _, break_label, continue_label| {
            let cond = convert_expr_opt(expr.cond_opt, TyExpect::Todo, loc, xx);
            xx.nodes
                .push(new_if_node(cond, new_cont(), new_cont(), loc));

            // body
            do_in_branch(xx, |xx| {
                let node = {
                    let _term = convert_expr_opt(expr.body_opt, TyExpect::Todo, loc, xx);
                    new_jump_tail(continue_label, vec![], loc)
                };
                xx.nodes.push(node);
            });

            // alt
            do_in_branch(xx, |xx| {
                xx.nodes
                    .push(new_jump_tail(break_label, vec![unit_term.clone()], loc));
            });
        },
    )
}

fn convert_loop_expr(expr: &ALoopExpr, _ty_expect: TyExpect, loc: Loc, xx: &mut Xx) -> AfterRval {
    do_in_loop("loop_result", loc, xx, |xx, _, _, continue_label| {
        do_in_branch(xx, |xx| {
            let node = {
                let _term = convert_expr_opt(expr.body_opt, TyExpect::Todo, loc, xx);
                new_jump_tail(continue_label, vec![], loc)
            };
            xx.nodes.push(node);
        });
    })
}

fn do_convert_expr(expr_id: AExprId, expr: &AExpr, ty_expect: TyExpect, xx: &mut Xx) -> AfterRval {
    let loc = Loc::new(xx.doc, PLoc::Expr(expr_id));

    match expr {
        AExpr::Unit => KTerm::Unit { loc },
        AExpr::True => KTerm::True { loc },
        AExpr::False => KTerm::False { loc },
        AExpr::Number(token) => convert_number_lit(*token, ty_expect, xx.tokens, xx.doc, xx.logger),
        AExpr::Char(token) => convert_char_expr(*token, xx.doc, xx.tokens),
        AExpr::Str(token) => convert_str_expr(*token, xx.doc, xx.tokens),
        AExpr::Name(name) => convert_name_expr(*name, xx),
        AExpr::TyApp(expr) => convert_ty_app_expr(expr, xx),
        AExpr::Record(record_expr) => convert_record_expr(record_expr, loc, xx),
        AExpr::Field(field_expr) => convert_field_expr(field_expr, loc, xx),
        AExpr::Call(call_expr) => convert_call_expr(call_expr, ty_expect, loc, xx),
        AExpr::Index(index_expr) => convert_index_expr(index_expr, ty_expect, loc, xx),
        AExpr::Cast(cast_expr) => convert_cast_expr(cast_expr, ty_expect, loc, xx),
        AExpr::UnaryOp(unary_op_expr) => convert_unary_op_expr(unary_op_expr, ty_expect, loc, xx),
        AExpr::BinaryOp(binary_op_expr) => {
            convert_binary_op_expr(binary_op_expr, ty_expect, loc, xx)
        }
        AExpr::Block(ABlockExpr { decls }) => convert_block_expr(decls.clone(), ty_expect, loc, xx),
        AExpr::Break(break_expr) => convert_break_expr(break_expr, ty_expect, loc, xx),
        AExpr::Continue => convert_continue_expr(ty_expect, loc, xx),
        AExpr::Return(return_expr) => convert_return_expr(return_expr, ty_expect, loc, xx),
        AExpr::If(if_expr) => convert_if_expr(if_expr, ty_expect, loc, xx),
        AExpr::Match(match_expr) => convert_match_expr(match_expr, ty_expect, loc, xx),
        AExpr::While(while_expr) => convert_while_expr(while_expr, ty_expect, loc, xx),
        AExpr::Loop(loop_expr) => convert_loop_expr(loop_expr, ty_expect, loc, xx),
    }
}

/// `&expr` を生成する。
fn do_convert_lval(
    expr_id: AExprId,
    expr: &AExpr,
    k_mut: KMut,
    ty_expect: TyExpect,
    xx: &mut Xx,
) -> AfterLval {
    let loc = Loc::new(xx.doc, PLoc::Expr(expr_id));

    match expr {
        AExpr::Name(name) => convert_name_lval(*name, k_mut, xx),
        AExpr::Record(expr) => convert_record_lval(expr, loc, xx),
        AExpr::Field(field_expr) => convert_field_lval(field_expr, k_mut, loc, xx),
        AExpr::Index(index_expr) => convert_index_lval(index_expr, ty_expect, loc, xx),
        AExpr::UnaryOp(unary_op_expr) => convert_unary_op_lval(unary_op_expr, ty_expect, loc, xx),
        _ => {
            // break や if など、左辺値と解釈可能な式は他にもある。いまのところ実装する必要はない

            let term = match convert_expr(expr_id, ty_expect, xx) {
                KTerm::Name(it) => it,
                term => {
                    // FIXME: リテラルなら static を導入してそのアドレスを取る。

                    let temp = fresh_var("lval", loc, xx);
                    xx.nodes.push(new_let_node(term, temp, new_cont(), loc));
                    temp
                }
            };
            let result = fresh_var("ref", loc, xx);
            xx.nodes
                .push(new_ref_node(KTerm::Name(term), result, new_cont(), loc));
            KTerm::Name(result)
        }
    }
}

fn convert_expr(expr_id: AExprId, ty_expect: TyExpect, xx: &mut Xx) -> AfterRval {
    let expr = expr_id.of(xx.ast.exprs());
    do_convert_expr(expr_id, expr, ty_expect, xx)
}

fn convert_lval(expr_id: AExprId, k_mut: KMut, ty_expect: TyExpect, xx: &mut Xx) -> AfterLval {
    let expr = expr_id.of(xx.ast.exprs());
    do_convert_lval(expr_id, expr, k_mut, ty_expect, xx)
}

fn convert_expr_opt(
    expr_id_opt: Option<AExprId>,
    ty_expect: TyExpect,
    loc: Loc,
    xx: &mut Xx,
) -> AfterRval {
    match expr_id_opt {
        Some(expr_id) => convert_expr(expr_id, ty_expect, xx),
        None => new_error_term(loc),
    }
}

fn convert_lval_opt(
    expr_id_opt: Option<AExprId>,
    k_mut: KMut,
    ty_expect: TyExpect,
    loc: Loc,
    xx: &mut Xx,
) -> AfterLval {
    match expr_id_opt {
        Some(expr_id) => convert_lval(expr_id, k_mut, ty_expect, xx),
        None => new_error_term(loc),
    }
}

/// 式を右辺値として評価する。型注釈があれば ty_opt に渡される。
fn evaluate_rval(
    expr_opt: Option<AExprId>,
    ty_opt: Option<ATyId>,
    loc: Loc,
    xx: &mut Xx,
) -> (AfterRval, KTy2) {
    let value = convert_expr_opt(expr_opt, TyExpect::Todo, loc, xx);
    let ty = convert_ty_opt(ty_opt, &new_ty_resolver(xx)).to_ty2_poly(xx.mod_outline);
    (value, ty)
}

fn convert_let_decl(_decl_id: ADeclId, decl: &AFieldLikeDecl, loc: Loc, xx: &mut Xx) {
    let (init_term, ty) = evaluate_rval(decl.value_opt, decl.ty_opt, loc, xx);

    let name = match decl.name_opt {
        Some(it) => it,
        None => {
            // 名前がなくても、型検査のため、一時変数に束縛する必要がある。
            let local_var = xx
                .local_vars
                .alloc(KLocalVarData::new("_".to_string(), loc).with_ty(ty));
            let var_term = KVarTerm {
                local_var,
                cause: KVarTermCause::Loc(loc),
            };
            xx.nodes
                .push(new_let_node(init_term, var_term, new_cont(), loc));
            return;
        }
    };

    let local_var = xx.local_vars.alloc(
        KLocalVarData::new(
            name.of(xx.ast.names()).text().to_string(),
            name.loc().to_loc(xx.doc),
        )
        .with_ty(ty),
    );
    let var_term = KVarTerm {
        local_var,
        cause: KVarTermCause::NameDef(xx.doc, name),
    };
    xx.nodes
        .push(new_let_node(init_term, var_term, new_cont(), loc));
    xx.name_symbols
        .insert(name, NameSymbol::LocalVar(local_var));
}

fn convert_const_decl(k_const: KConst, decl: &AFieldLikeDecl, loc: Loc, xx: &mut Xx) {
    let (node, term) = {
        let mut nodes = take(&mut xx.nodes);
        let mut term_opt = None;
        xx.do_out_fn(|xx| {
            term_opt = Some(convert_expr_opt(decl.value_opt, TyExpect::Todo, loc, xx));
        });
        swap(&mut xx.nodes, &mut nodes);

        (fold_nodes(nodes), term_opt.unwrap())
    };

    #[cfg(skip)]
    log::trace!(
        "{} init = {:?}",
        k_const.of(&xx.mod_outline.consts).name,
        DebugWith::new(&term, &local_context(xx)),
    );
    *k_const.of_mut(&mut xx.mod_data.consts) = KConstInit {
        init_opt: Some((node, term)),
    };
}

fn convert_static_decl(static_var: KStaticVar, decl: &AFieldLikeDecl, loc: Loc, xx: &mut Xx) {
    let (node, term) = {
        let mut nodes = take(&mut xx.nodes);
        let mut term_opt = None;
        xx.do_out_fn(|xx| {
            term_opt = Some(convert_expr_opt(decl.value_opt, TyExpect::Todo, loc, xx));
        });
        swap(&mut xx.nodes, &mut nodes);

        (fold_nodes(nodes), term_opt.unwrap())
    };

    *static_var.of_mut(&mut xx.mod_data.static_vars) = KStaticVarInit {
        init_opt: Some((node, term)),
    };
}

fn convert_param_decls(
    param_decls: &[AParamDecl],
    param_tys: &[KTy],
    doc: Doc,
    ast: &ATree,
    local_vars: &mut KLocalVarArena,
    name_symbols: &mut NameSymbols,
) -> Vec<KVarTerm> {
    assert_eq!(param_decls.len(), param_tys.len());

    param_decls
        .iter()
        .zip(param_tys)
        .map(|(param_decl, _param_ty)| {
            let loc = param_decl.name.loc().to_loc(doc);
            let name = param_decl.name.of(ast.names()).text.to_string();
            let local_var = local_vars.alloc(KLocalVarData::new(name.to_string(), loc));
            name_symbols.insert(param_decl.name, NameSymbol::LocalVar(local_var));
            KVarTerm {
                local_var,
                cause: KVarTermCause::NameDef(doc, param_decl.name),
            }
        })
        .collect()
}

fn convert_fn_decl(k_fn: KFn, fn_decl: &AFnLikeDecl, loc: Loc, xx: &mut Xx) {
    let mut local_vars = KLocalVarArena::new();

    let params = convert_param_decls(
        &fn_decl.params,
        k_fn.param_tys(&xx.mod_outline.fns),
        xx.doc,
        xx.ast,
        &mut local_vars,
        xx.name_symbols,
    );

    let fn_data = xx.do_out_fn(|xx| {
        xx.fn_opt = Some(k_fn);
        xx.local_vars = local_vars;

        // 関数の本体を格納しておくラベル
        xx.label = xx.labels.alloc(KLabelConstruction::default());

        let term = convert_expr_opt(fn_decl.body_opt, TyExpect::Todo, loc, xx);
        emit_return(term, loc, xx);
        commit_label(xx);

        #[cfg(skip)]
        for label in xx.labels.iter() {
            log::trace!(
                "label {}({:#?}) size={} = {:#?}",
                &label.name,
                DebugWith::new(&label.params, &xx.local_vars),
                label.body.len(),
                DebugWith::new(&label.body, &local_context(xx))
            );
        }

        let local_vars = take(&mut xx.local_vars);
        let labels =
            KLabelArena::from_iter(take(&mut xx.labels).into_vec().into_iter().map(|label| {
                let name = label.name;
                let params = label.params;
                let body = fold_nodes(label.body);
                KLabelData { name, params, body }
            }));
        let ty_env = take(&mut xx.ty_env);

        KFnData::new(params, local_vars, labels, ty_env)
    });

    *k_fn.of_mut(&mut xx.mod_data.fns) = fn_data;
}

fn emit_return(term: KTerm, loc: Loc, xx: &mut Xx) {
    let k_fn = xx.fn_opt.unwrap();
    xx.nodes.push(new_return_tail(k_fn, term, loc));
}

fn convert_extern_fn_decl(extern_fn: KExternFn, extern_fn_decl: &AFnLikeDecl, xx: &mut Xx) {
    let mut local_vars = KLocalVarArena::new();

    let params = convert_param_decls(
        &extern_fn_decl.params,
        extern_fn.param_tys(&xx.mod_outline.extern_fns),
        xx.doc,
        xx.ast,
        &mut local_vars,
        xx.name_symbols,
    );

    *extern_fn.of_mut(&mut xx.mod_data.extern_fns) = KExternFnData { params, local_vars };
}

fn convert_const_enum_decl(const_enum: KConstEnum, decl: &AEnumDecl, loc: Loc, xx: &mut Xx) {
    for (decl, k_const) in decl
        .variants
        .iter()
        .zip(const_enum.variants(&xx.mod_outline.const_enums).iter())
    {
        let decl = decl.as_const().unwrap();
        convert_const_decl(k_const, decl, loc, xx);
    }
}

fn convert_struct_enum_decl(_enum: KStructEnum, _decl: &AEnumDecl, _loc: Loc, _xx: &mut Xx) {
    // pass
}

fn do_convert_decl(decl_id: ADeclId, decl: &ADecl, term_opt: &mut Option<KTerm>, xx: &mut Xx) {
    let symbol_opt = decl
        .name_opt()
        .and_then(|name| xx.name_symbols.get(&name).cloned());
    let loc = Loc::new(xx.doc, PLoc::Decl(decl_id));

    match decl {
        ADecl::Attr => {}
        ADecl::Expr(expr) => {
            *term_opt = Some(convert_expr(*expr, TyExpect::Todo, xx));
        }
        ADecl::Let(decl) => {
            assert_eq!(symbol_opt, None);
            convert_let_decl(decl_id, decl, loc, xx);
        }
        ADecl::Const(decl) => {
            let k_const = match symbol_opt {
                Some(NameSymbol::ModSymbol(KModSymbol::Const(it))) => it,
                _ => return,
            };
            convert_const_decl(k_const, decl, loc, xx)
        }
        ADecl::Static(decl) => {
            let static_var = match symbol_opt {
                Some(NameSymbol::ModSymbol(KModSymbol::StaticVar(it))) => it,
                _ => return,
            };
            convert_static_decl(static_var, decl, loc, xx)
        }
        ADecl::Fn(fn_decl) => {
            let k_fn = match symbol_opt {
                Some(NameSymbol::ModSymbol(KModSymbol::Fn(it))) => it,
                _ => return,
            };
            convert_fn_decl(k_fn, fn_decl, loc, xx);
        }
        ADecl::ExternFn(extern_fn_decl) => {
            let extern_fn = match symbol_opt {
                Some(NameSymbol::ModSymbol(KModSymbol::ExternFn(it))) => it,
                _ => return,
            };
            convert_extern_fn_decl(extern_fn, extern_fn_decl, xx);
        }
        ADecl::Enum(enum_decl) => match symbol_opt {
            Some(NameSymbol::ModSymbol(KModSymbol::ConstEnum(const_enum))) => {
                convert_const_enum_decl(const_enum, enum_decl, loc, xx)
            }
            Some(NameSymbol::ModSymbol(KModSymbol::StructEnum(struct_enum))) => {
                convert_struct_enum_decl(struct_enum, enum_decl, loc, xx)
            }
            _ => return,
        },
        ADecl::Struct(_) | ADecl::Use(_) => {}
    }
}

fn convert_decls(decls: ADeclIds, xx: &mut Xx) -> Option<KTerm> {
    let mut last_opt = None;
    for (decl_id, decl) in decls.enumerate(xx.ast.decls()) {
        let mut term_opt = None;

        do_convert_decl(decl_id, decl, &mut term_opt, xx);

        last_opt = term_opt;
    }
    last_opt
}

pub(crate) fn convert_to_cps(
    doc: Doc,
    tree: &PTree,
    name_symbols: &mut NameSymbols,
    mod_outline: &KModOutline,
    mod_data: &mut KModData,
    logger: &DocLogger,
) {
    let mut xx = Xx::new(
        doc,
        &tree.tokens,
        &tree.ast,
        &tree.name_referents,
        name_symbols,
        mod_outline,
        logger,
    );

    {
        let KModData {
            consts,
            static_vars,
            extern_fns,
            fns,
        } = mod_data;
        consts.extend_with(mod_outline.consts.len(), KConstInit::new_empty);
        static_vars.extend_with(mod_outline.static_vars.len(), KStaticVarInit::new_empty);
        extern_fns.extend_with(mod_outline.extern_fns.len(), Default::default);
        fns.extend_with(mod_outline.fns.len(), Default::default);
    }

    {
        xx.mod_data = take(mod_data);
        convert_decls(tree.ast.root_decls(), &mut xx);
        *mod_data = xx.mod_data;
    }
}
