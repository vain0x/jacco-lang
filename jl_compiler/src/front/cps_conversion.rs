//! 構文木から CPS ノードのもとになる命令列を生成する処理

use crate::{
    cps::*,
    front::{env::Env, name_resolution::*},
    logs::DocLogger,
    parse::*,
    source::{Doc, Loc},
    token::{eval_number, LitErr},
    utils::VecArena,
};
use std::{
    collections::HashSet,
    iter::once,
    mem::{swap, take},
};

struct KLoopData {
    break_label: KLabel,
    continue_label: KLabel,
}

fn new_unit_term(loc: Loc) -> KTerm {
    KTerm::Unit { loc }
}

fn new_never_term(loc: Loc) -> KTerm {
    // FIXME: the type is ! (never)
    KTerm::Unit { loc }
}

fn convert_number_lit(token: PToken, tokens: &PTokens, doc: Doc, logger: &DocLogger) -> KTerm {
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
    params: Vec<KSymbol>,
    body: Vec<KNode>,
}

/// CPS 変換の文脈
struct Xx<'a> {
    // state
    env: Env,
    mod_data: KModData,
    label: KLabel,
    nodes: Vec<KNode>,
    local_vars: KLocalArena,
    labels: VecArena<KLabelTag, KLabelConstruction>,
    /// return のターゲットとなる関数
    fn_opt: Option<KFn>,
    /// break/continue のターゲットとなるループ
    loop_opt: Option<KLoopData>,

    // read:
    doc: Doc,
    k_mod: KMod,
    tokens: &'a PTokens,
    ast: &'a ATree,
    decl_symbols: &'a DeclSymbols,
    mod_outline: &'a KModOutline,
    #[allow(unused)]
    mod_outlines: &'a KModOutlines,
    listener: &'a mut dyn NameResolutionListener,
    logger: &'a DocLogger,
}

impl<'a> Xx<'a> {
    fn new(
        doc: Doc,
        k_mod: KMod,
        tokens: &'a PTokens,
        ast: &'a ATree,
        decl_symbols: &'a DeclSymbols,
        mod_outline: &'a KModOutline,
        mod_outlines: &'a KModOutlines,
        listener: &'a mut dyn NameResolutionListener,
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
            labels,
            local_vars: KLocalArena::new(),
            fn_opt: None,
            loop_opt: None,
            env: Env::new(),
            mod_data: KModData::default(),
            // read:
            doc,
            k_mod,
            tokens,
            ast,
            decl_symbols,
            mod_outline,
            mod_outlines,
            listener,
            logger,
        }
    }

    fn do_out_fn<A>(&mut self, f: impl FnOnce(&mut Xx) -> A) -> A {
        let label = self.label;
        let nodes = take(&mut self.nodes);
        let labels = take(&mut self.labels);
        let local_vars = take(&mut self.local_vars);
        let fn_opt = self.fn_opt.take();
        let loop_opt = self.loop_opt.take();

        let result = f(self);

        self.label = label;
        self.nodes = nodes;
        self.labels = labels;
        self.local_vars = local_vars;
        self.fn_opt = fn_opt;
        self.loop_opt = loop_opt;

        result
    }

    fn do_in_scope(&mut self, f: impl FnOnce(&mut Xx)) {
        self.env.enter_scope();
        f(self);
        self.env.leave_scope();
    }
}

fn path_resolution_context<'a>(xx: &'a mut Xx) -> PathResolutionContext<'a> {
    PathResolutionContext {
        tokens: xx.tokens,
        mod_outline: xx.mod_outline,
        env: &xx.env,
        listener: &mut *xx.listener,
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
    pub(crate) env: &'a Env,
    pub(crate) ast: &'a ATree,
    pub(crate) listener: &'a mut dyn NameResolutionListener,
    pub(crate) logger: &'a DocLogger,
}

fn new_ty_resolver<'a>(xx: &'a mut Xx<'_>) -> TyResolver<'a> {
    TyResolver {
        env: &xx.env,
        ast: xx.ast,
        listener: xx.listener,
        logger: xx.logger,
    }
}

fn do_convert_ty(ty_id: ATyId, ty: &ATy, xx: &mut TyResolver) -> KTy {
    let key = ANameKey::Ty(ty_id);
    let loc = PLoc::Name(key);

    match ty {
        ATy::Name(name) => {
            if name.is_qualified() {
                error_unsupported_path_ty(loc, xx.logger);
            }

            match resolve_ty_name(name.text(), key, &xx.env, xx.listener) {
                Some(ty) => ty,
                None => {
                    error_unresolved_ty(loc, xx.logger);
                    KTy::Unresolved {
                        cause: KTyCause::NameUnresolved(ty_id),
                    }
                }
            }
        }
        ATy::InferTy => {
            // FIXME: メタ変数にする。シグネチャだったらエラーにする。
            error_unresolved_ty(loc, xx.logger);
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
    }
}

pub(crate) fn convert_ty(ty_id: ATyId, xx: &mut TyResolver) -> KTy {
    let ty = ty_id.of(xx.ast.tys());
    do_convert_ty(ty_id, ty, xx)
}

pub(crate) fn convert_ty_opt(ty_opt: Option<ATyId>, xx: &mut TyResolver) -> KTy {
    match ty_opt {
        Some(ty) => convert_ty(ty, xx),
        None => KTy::Unresolved {
            cause: KTyCause::Miss,
        },
    }
}

// -----------------------------------------------
// パターン
// -----------------------------------------------

// 入れ子のパターンはまだコンパイルできない
enum Branch {
    Case(KTerm),
    Default(KSymbol),
}

fn convert_wildcard_pat_as_cond(token: PToken, xx: &mut Xx) -> Branch {
    let symbol = {
        let cause = KSymbolCause::WildcardPat(xx.doc, token);
        fresh_symbol("_", cause, xx)
    };
    Branch::Default(symbol)
}

fn convert_name_pat_as_cond(name: &AName, key: ANameKey, xx: &mut Xx) -> Branch {
    let loc = Loc::new(xx.doc, PLoc::Name(key));
    match resolve_value_path(&name, key, path_resolution_context(xx)) {
        Some(KLocalValue::Const(k_const)) => Branch::Case(KTerm::Const { k_const, loc }),
        Some(KLocalValue::UnitLikeStruct(k_struct)) => {
            Branch::Case(KTerm::RecordTag { k_struct, loc })
        }
        Some(KLocalValue::Alias(alias)) => {
            // FIXME: エイリアスが const などを指している可能性があるので、shadowing とはみなせない。Rust と挙動が異なる
            Branch::Case(KTerm::Alias { alias, loc })
        }
        _ => {
            if name.is_qualified() {
                error_unresolved_value(PLoc::Name(key), &xx.logger);
            }

            let symbol = {
                let cause = KSymbolCause::NameDef(xx.doc, key);
                fresh_symbol(&name.text, cause, xx)
            };
            Branch::Default(symbol)
        }
    }
}

fn convert_name_pat_as_assign(cond: &KTerm, term: KTerm, loc: Loc, xx: &mut Xx) {
    let symbol = match term {
        KTerm::Name(symbol) if symbol.local.name(&xx.local_vars) == "_" => return,
        KTerm::Name(it) => it,
        _ => return,
    };

    let name = symbol.local.name(&xx.local_vars).to_string();
    xx.env
        .insert_value(name, KLocalValue::LocalVar(symbol.local));

    xx.nodes
        .push(new_let_node(cond.clone(), symbol, new_cont(), loc));
}

fn convert_record_pat_as_cond(pat_id: APatId, pat: &ARecordPat, loc: Loc, xx: &mut Xx) -> KTerm {
    let key = ANameKey::Pat(pat_id);
    let k_struct = match resolve_ty_path(&pat.left, key, path_resolution_context(xx)) {
        Some(KTy::Struct(it)) => it,
        _ => {
            error_expected_record_ty(PLoc::from_loc(loc), xx.logger);
            return new_error_term(loc);
        }
    };
    KTerm::RecordTag { k_struct, loc }
}

fn do_convert_pat_as_cond(pat_id: APatId, pat: &APat, loc: Loc, xx: &mut Xx) -> Branch {
    let term = match pat {
        APat::Unit => KTerm::Unit { loc },
        APat::True(_) => KTerm::True { loc },
        APat::False(_) => KTerm::False { loc },
        APat::Char(token) => convert_char_expr(*token, xx.doc, xx.tokens),
        APat::Number(token) => convert_number_lit(*token, xx.tokens, xx.doc, xx.logger),
        APat::Str(token) => convert_str_expr(*token, xx.doc, xx.tokens),
        APat::Wildcard(token) => return convert_wildcard_pat_as_cond(*token, xx),
        APat::Name(name) => return convert_name_pat_as_cond(name, ANameKey::Pat(pat_id), xx),
        APat::Record(record_pat) => convert_record_pat_as_cond(pat_id, record_pat, loc, xx),
    };
    Branch::Case(term)
}

fn do_convert_pat_as_assign(pat: &APat, cond: &KTerm, term: KTerm, loc: Loc, xx: &mut Xx) {
    match pat {
        APat::Name(_) => convert_name_pat_as_assign(cond, term, loc, xx),
        _ => {}
    }
}

fn convert_pat_as_cond(pat_id: APatId, xx: &mut Xx) -> Branch {
    let pat = pat_id.of(xx.ast.pats());
    let loc = Loc::new(xx.doc, PLoc::Pat(pat_id));
    do_convert_pat_as_cond(pat_id, pat, loc, xx)
}

fn convert_pat_as_assign(pat_id: APatId, cond: &KTerm, term: KTerm, xx: &mut Xx) {
    let pat = pat_id.of(xx.ast.pats());
    let loc = Loc::new(xx.doc, PLoc::Pat(pat_id));
    do_convert_pat_as_assign(pat, cond, term, loc, xx)
}

// -----------------------------------------------
// 式 (項とノード)
// -----------------------------------------------

fn new_error_term(loc: Loc) -> KTerm {
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
    result: KSymbol,
}

/// continue で戻ってくる先の、ループの先頭を表すラベル。引数を持たない。
struct ContinueLabel {
    label: KLabel,
}

fn new_break_label(hint: &str, result: KSymbol, xx: &mut Xx) -> BreakLabel {
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
fn do_with_break(break_label: BreakLabel, xx: &mut Xx, f: impl FnOnce(&mut Xx, KLabel)) -> KTerm {
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
    f: impl FnOnce(&mut Xx, KSymbol, KLabel, KLabel),
) -> KTerm {
    let result = fresh_symbol(hint, loc, xx);

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

fn convert_char_expr(token: PToken, doc: Doc, tokens: &PTokens) -> KTerm {
    KTerm::Char {
        text: token.text(tokens).to_string(),
        ty: KTy2::C8,
        loc: KTermCause::Token(doc, token).loc(),
    }
}

fn convert_str_expr(token: PToken, doc: Doc, tokens: &PTokens) -> KTerm {
    KTerm::Str {
        text: token.text(tokens).to_string(),
        loc: KTermCause::Token(doc, token).loc(),
    }
}

fn emit_unit_like_struct(
    k_struct: KStruct,
    result: KSymbol,
    loc: Loc,
    nodes: &mut Vec<KNode>,
) -> KTerm {
    let ty = KTy::Struct(k_struct);

    nodes.push(new_record_node(ty, vec![], result, new_cont(), loc));
    KTerm::Name(result)
}

fn fresh_symbol(hint: &str, cause: impl Into<KSymbolCause>, xx: &mut Xx) -> KSymbol {
    let cause = cause.into();
    let loc = cause.loc();
    let local = xx.local_vars.alloc(KLocalData::new(hint.to_string(), loc));
    KSymbol { local, cause }
}

fn convert_name_expr(name: &AName, key: ANameKey, xx: &mut Xx) -> KTerm {
    let loc = Loc::new(xx.doc, PLoc::Name(key));
    let cause = KSymbolCause::NameUse(xx.doc, key);

    let value = match resolve_value_path(name, key, path_resolution_context(xx)) {
        Some(it) => it,
        None => {
            error_unresolved_value(PLoc::from_loc(loc), xx.logger);
            return new_error_term(loc);
        }
    };

    match value {
        KLocalValue::LocalVar(local_var) => KTerm::Name(KSymbol {
            local: local_var,
            cause,
        }),
        KLocalValue::Const(k_const) => KTerm::Const { k_const, loc },
        KLocalValue::StaticVar(static_var) => KTerm::StaticVar { static_var, loc },
        KLocalValue::Fn(k_fn) => KTerm::Fn { k_fn, loc },
        KLocalValue::ExternFn(extern_fn) => KTerm::ExternFn { extern_fn, loc },
        KLocalValue::UnitLikeStruct(k_struct) => {
            let name = k_struct.name(&xx.mod_outline.structs);
            let result = fresh_symbol(name, cause, xx);
            emit_unit_like_struct(k_struct, result, loc, &mut xx.nodes)
        }
        KLocalValue::Alias(alias) => KTerm::Alias { alias, loc },
    }
}

fn convert_name_lval(name: &AName, k_mut: KMut, key: ANameKey, xx: &mut Xx) -> KTerm {
    let loc = Loc::new(xx.doc, PLoc::Name(key));
    let cause = KSymbolCause::NameUse(xx.doc, key);

    let value = match resolve_value_path(name, key, path_resolution_context(xx)) {
        Some(it) => it,
        None => {
            error_unresolved_value(PLoc::Name(key), xx.logger);
            return new_error_term(loc);
        }
    };

    let term = match value {
        KLocalValue::LocalVar(local) => KTerm::Name(KSymbol { local, cause }),
        KLocalValue::StaticVar(static_var) => KTerm::StaticVar { static_var, loc },
        KLocalValue::Alias(alias) => KTerm::Alias { alias, loc },
        KLocalValue::Const(_)
        | KLocalValue::Fn(_)
        | KLocalValue::ExternFn(_)
        | KLocalValue::UnitLikeStruct(_) => {
            error_rval_used_as_lval(PLoc::Name(key), xx.logger);
            return new_error_term(loc);
        }
    };

    match k_mut {
        KMut::Const => {
            let result = fresh_symbol("ref", cause, xx);
            xx.nodes.push(new_ref_node(term, result, new_cont(), loc));
            KTerm::Name(result)
        }
        KMut::Mut => {
            let result = fresh_symbol("refmut", cause, xx);
            xx.nodes
                .push(new_ref_mut_node(term, result, new_cont(), loc));
            KTerm::Name(result)
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

fn calculate_field_ordering<T>(
    items: &[T],
    fields: &[KField],
    field_arena: &KFieldArena,
    name_fn: impl Fn(&T) -> &str,
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

fn do_convert_record_expr(
    expr_id: AExprId,
    expr: &ARecordExpr,
    loc: Loc,
    xx: &mut Xx,
) -> Option<KSymbol> {
    let key = ANameKey::Expr(expr_id);
    let k_struct = match resolve_ty_path(&expr.left, key, path_resolution_context(xx)) {
        Some(KTy::Struct(k_struct)) => k_struct,
        _ => {
            // FIXME: エイリアス
            error_expected_record_ty(PLoc::from_loc(loc), xx.logger);
            return None;
        }
    };
    let ty = KTy::Struct(k_struct);

    let fields = k_struct.fields(&xx.mod_outline.structs);
    let perm =
        match calculate_field_ordering(&expr.fields, fields, &xx.mod_outline.fields, |field_expr| {
            &field_expr.field_name.text
        }) {
            Ok(it) => it,
            Err(errors) => {
                report_record_expr_errors(fields, &errors, loc, xx.mod_outline, xx.logger);
                return None;
            }
        };

    let mut args = vec![KTerm::Unit { loc }; fields.len()];
    for (i, field_expr) in expr.fields.iter().enumerate() {
        args[perm[i]] = convert_expr_opt(field_expr.value_opt, loc, xx);
    }

    let result = fresh_symbol(k_struct.name(&xx.mod_outline.structs), loc, xx);
    xx.nodes
        .push(new_record_node(ty, args, result, new_cont(), loc));
    Some(result)
}

fn convert_record_expr(expr_id: AExprId, expr: &ARecordExpr, loc: Loc, xx: &mut Xx) -> KTerm {
    match do_convert_record_expr(expr_id, expr, loc, xx) {
        Some(result) => KTerm::Name(result),
        None => new_error_term(loc),
    }
}

// `&A { .. }`
fn convert_record_lval(expr_id: AExprId, expr: &ARecordExpr, loc: Loc, xx: &mut Xx) -> KTerm {
    let arg = match do_convert_record_expr(expr_id, expr, loc, xx) {
        Some(it) => it,
        None => return new_error_term(loc),
    };

    let result = fresh_symbol("ref", loc, xx);
    xx.nodes
        .push(new_ref_node(KTerm::Name(arg), result, new_cont(), loc));
    KTerm::Name(result)
}

// `x.field` ==> `*(&x)->field`
fn convert_field_expr(expr: &AFieldExpr, loc: Loc, xx: &mut Xx) -> KTerm {
    let result = {
        let name = match expr.field_opt {
            Some(token) => token.text(xx.tokens),
            None => "_",
        };
        fresh_symbol(&name, loc, xx)
    };

    let field_ptr = convert_field_lval(expr, KMut::Const, loc, xx);
    xx.nodes
        .push(new_deref_node(field_ptr, result, new_cont(), loc));
    KTerm::Name(result)
}

// `&x.field` ==> `&(&x)->field`
fn convert_field_lval(expr: &AFieldExpr, k_mut: KMut, loc: Loc, xx: &mut Xx) -> KTerm {
    let (name, field_loc) = match expr.field_opt {
        Some(token) => {
            let name = token.text(xx.tokens).to_string();
            let loc = Loc::new(xx.doc, PLoc::Token(token));
            (name, loc)
        }
        None => ("_".to_string(), loc),
    };
    let result = fresh_symbol(&format!("{}_ptr", name), loc, xx);

    let left = convert_lval(expr.left, k_mut, xx);
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

fn convert_call_expr(call_expr: &ACallLikeExpr, loc: Loc, xx: &mut Xx) -> KTerm {
    let result = fresh_symbol("call_result", loc, xx);
    let left = convert_expr(call_expr.left, xx);

    let mut args = Vec::with_capacity(call_expr.args.len() + 1);
    args.push(left);
    args.extend(call_expr.args.iter().map(|arg| convert_expr(arg, xx)));

    xx.nodes.push(new_call_node(args, result, new_cont(), loc));
    KTerm::Name(result)
}

// `a[i]` ==> `*(a + i)`
fn convert_index_expr(expr: &ACallLikeExpr, loc: Loc, xx: &mut Xx) -> KTerm {
    let ptr = convert_index_lval(expr, loc, xx);

    let result = fresh_symbol("index_result", loc, xx);
    xx.nodes.push(new_deref_node(ptr, result, new_cont(), loc));
    KTerm::Name(result)
}

// `&a[i]` ==> `a + i`
fn convert_index_lval(expr: &ACallLikeExpr, loc: Loc, xx: &mut Xx) -> KTerm {
    let left = convert_expr(expr.left, xx);
    let right = if expr.args.len() == 1 {
        let right = expr.args.iter().next().unwrap();
        convert_expr(right, xx)
    } else {
        new_error_term(loc)
    };

    let result = fresh_symbol("indexed_ptr", loc, xx);
    xx.nodes
        .push(new_add_node(left, right, result, new_cont(), loc));
    KTerm::Name(result)
}

fn convert_as_expr(expr: &AAsExpr, loc: Loc, xx: &mut Xx) -> KTerm {
    let arg = convert_expr(expr.left, xx);
    let ty = convert_ty_opt(expr.ty_opt, &mut new_ty_resolver(xx));

    let result = fresh_symbol("cast", loc, xx);
    xx.nodes
        .push(new_cast_node(ty, arg, result, new_cont(), loc));
    KTerm::Name(result)
}

fn convert_unary_op_expr(expr: &AUnaryOpExpr, loc: Loc, xx: &mut Xx) -> KTerm {
    match expr.op {
        PUnaryOp::Deref => {
            let arg = convert_expr_opt(expr.arg_opt, loc, xx);
            let result = fresh_symbol("deref", loc, xx);
            xx.nodes.push(new_deref_node(arg, result, new_cont(), loc));
            KTerm::Name(result)
        }
        PUnaryOp::Ref => {
            let k_mut = expr.mut_opt.unwrap_or(KMut::Const);
            convert_lval_opt(expr.arg_opt, k_mut, loc, xx)
        }
        PUnaryOp::Minus => {
            let arg = convert_expr_opt(expr.arg_opt, loc, xx);
            let result = fresh_symbol("minus", loc, xx);
            xx.nodes.push(new_minus_node(arg, result, new_cont(), loc));
            KTerm::Name(result)
        }
        PUnaryOp::Not => {
            let arg = convert_expr_opt(expr.arg_opt, loc, xx);
            let result = fresh_symbol("not", loc, xx);
            xx.nodes.push(new_not_node(arg, result, new_cont(), loc));
            KTerm::Name(result)
        }
    }
}

fn convert_unary_op_lval(expr: &AUnaryOpExpr, loc: Loc, xx: &mut Xx) -> KTerm {
    match expr.op {
        PUnaryOp::Deref => {
            // `&*p` ==> `p`
            convert_expr_opt(expr.arg_opt, loc, xx)
        }
        PUnaryOp::Ref | PUnaryOp::Minus | PUnaryOp::Not => {
            error_rval_used_as_lval(PLoc::from_loc(loc), xx.logger);
            new_error_term(loc)
        }
    }
}

fn do_convert_assignment_expr(prim: KPrim, expr: &ABinaryOpExpr, loc: Loc, xx: &mut Xx) -> KTerm {
    let left = convert_lval(expr.left, KMut::Mut, xx);
    let right = convert_expr_opt(expr.right_opt, loc, xx);

    xx.nodes
        .push(new_assignment_node(prim, left, right, new_cont(), loc));
    new_unit_term(loc)
}

fn do_convert_basic_binary_op_expr(
    prim: KPrim,
    expr: &ABinaryOpExpr,
    loc: Loc,
    xx: &mut Xx,
) -> KTerm {
    let left = convert_expr(expr.left, xx);
    let right = convert_expr_opt(expr.right_opt, loc, xx);

    let result = fresh_symbol(&prim.hint_str(), loc, xx);
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
fn do_convert_log_and_expr(expr: &ABinaryOpExpr, loc: Loc, xx: &mut Xx) -> KTerm {
    do_convert_if_expr(
        |xx| convert_expr(expr.left, xx),
        |xx| convert_expr_opt(expr.right_opt, loc, xx),
        |_| KTerm::False { loc },
        loc,
        xx,
    )
}

// `p || q` ==> `if p { true } else { q }`
fn do_convert_log_or_expr(expr: &ABinaryOpExpr, loc: Loc, xx: &mut Xx) -> KTerm {
    do_convert_if_expr(
        |xx| convert_expr(expr.left, xx),
        |_| KTerm::True { loc },
        |xx| convert_expr_opt(expr.right_opt, loc, xx),
        loc,
        xx,
    )
}

fn convert_binary_op_expr(expr: &ABinaryOpExpr, loc: Loc, xx: &mut Xx) -> KTerm {
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
        PBinaryOp::LogAnd => do_convert_log_and_expr(expr, loc, xx),
        PBinaryOp::LogOr => do_convert_log_or_expr(expr, loc, xx),
    }
}

fn convert_block_expr(decls: ADeclIds, loc: Loc, xx: &mut Xx) -> KTerm {
    let mut last_opt = None;

    xx.do_in_scope(|xx| {
        last_opt = convert_decls(decls.clone(), xx);
    });

    last_opt.unwrap_or(KTerm::Unit { loc })
}

fn convert_break_expr(expr: &AJumpExpr, loc: Loc, xx: &mut Xx) -> KTerm {
    let label_opt = xx.loop_opt.as_ref().map(|data| data.break_label);
    let label = match label_opt {
        Some(it) => it,
        None => {
            error_break_out_of_loop(PLoc::from_loc(loc), xx.logger);
            return new_error_term(loc);
        }
    };

    let arg = convert_expr_opt(expr.arg_opt, loc, xx);
    xx.nodes
        .push(new_jump_node(label, vec![arg], new_cont(), loc));
    new_never_term(loc)
}

fn convert_continue_expr(loc: Loc, xx: &mut Xx) -> KTerm {
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

fn convert_return_expr(expr: &AJumpExpr, loc: Loc, xx: &mut Xx) -> KTerm {
    let arg = convert_expr_opt(expr.arg_opt, loc, xx);

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
    cond_fn: impl FnOnce(&mut Xx) -> KTerm,
    body_fn: impl FnOnce(&mut Xx) -> KTerm,
    alt_fn: impl FnOnce(&mut Xx) -> KTerm,
    loc: Loc,
    xx: &mut Xx,
) -> KTerm {
    let result = fresh_symbol("if_result", loc, xx);
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

fn convert_if_expr(expr: &AIfExpr, loc: Loc, xx: &mut Xx) -> KTerm {
    do_convert_if_expr(
        |xx| convert_expr_opt(expr.cond_opt, loc, xx),
        |xx| convert_expr_opt(expr.body_opt, loc, xx),
        |xx| convert_expr_opt(expr.alt_opt, loc, xx),
        loc,
        xx,
    )
}

fn convert_match_expr(expr: &AMatchExpr, loc: Loc, xx: &mut Xx) -> KTerm {
    let cond = convert_expr_opt(expr.cond_opt, loc, xx);
    if expr.arms.is_empty() {
        error_empty_match(PLoc::from_loc(loc), xx.logger);
        return new_error_term(loc);
    }

    let result = fresh_symbol("match_result", loc, xx);
    let next = new_break_label("match_next", result, xx);

    do_with_break(next, xx, |xx, break_label| {
        let arms = expr
            .arms
            .iter()
            .map(|arm| {
                let term = match convert_pat_as_cond(arm.pat, xx) {
                    Branch::Case(term) => term,
                    Branch::Default(symbol) => KTerm::Name(symbol),
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
                xx.do_in_scope(|xx| {
                    convert_pat_as_assign(arm.pat, &cond, term, xx);
                    let body = convert_expr_opt(arm.body_opt, loc, xx);
                    let node = new_jump_tail(break_label, vec![body], loc);
                    xx.nodes.push(node);
                });
            });
        }
    })
}

// `while cond { body }` ==> `loop { if cond { body } else { break } }`
fn convert_while_expr(expr: &AWhileExpr, loc: Loc, xx: &mut Xx) -> KTerm {
    let unit_term = new_unit_term(loc);

    do_in_loop(
        "while_result",
        loc,
        xx,
        |xx, _, break_label, continue_label| {
            let cond = convert_expr_opt(expr.cond_opt, loc, xx);
            xx.nodes
                .push(new_if_node(cond, new_cont(), new_cont(), loc));

            // body
            do_in_branch(xx, |xx| {
                let node = {
                    let _term = convert_expr_opt(expr.body_opt, loc, xx);
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

fn convert_loop_expr(expr: &ALoopExpr, loc: Loc, xx: &mut Xx) -> KTerm {
    do_in_loop("loop_result", loc, xx, |xx, _, _, continue_label| {
        do_in_branch(xx, |xx| {
            let node = {
                let _term = convert_expr_opt(expr.body_opt, loc, xx);
                new_jump_tail(continue_label, vec![], loc)
            };
            xx.nodes.push(node);
        });
    })
}

fn do_convert_expr(expr_id: AExprId, expr: &AExpr, xx: &mut Xx) -> KTerm {
    let loc = Loc::new(xx.doc, PLoc::Expr(expr_id));

    match expr {
        AExpr::Unit => KTerm::Unit { loc },
        AExpr::True => KTerm::True { loc },
        AExpr::False => KTerm::False { loc },
        AExpr::Number(token) => convert_number_lit(*token, xx.tokens, xx.doc, xx.logger),
        AExpr::Char(token) => convert_char_expr(*token, xx.doc, xx.tokens),
        AExpr::Str(token) => convert_str_expr(*token, xx.doc, xx.tokens),
        AExpr::Name(name) => convert_name_expr(name, ANameKey::Expr(expr_id), xx),
        AExpr::Record(record_expr) => convert_record_expr(expr_id, record_expr, loc, xx),
        AExpr::Field(field_expr) => convert_field_expr(field_expr, loc, xx),
        AExpr::Call(call_expr) => convert_call_expr(call_expr, loc, xx),
        AExpr::Index(index_expr) => convert_index_expr(index_expr, loc, xx),
        AExpr::As(as_expr) => convert_as_expr(as_expr, loc, xx),
        AExpr::UnaryOp(unary_op_expr) => convert_unary_op_expr(unary_op_expr, loc, xx),
        AExpr::BinaryOp(binary_op_expr) => convert_binary_op_expr(binary_op_expr, loc, xx),
        AExpr::Block(ABlockExpr { decls }) => convert_block_expr(decls.clone(), loc, xx),
        AExpr::Break(break_expr) => convert_break_expr(break_expr, loc, xx),
        AExpr::Continue => convert_continue_expr(loc, xx),
        AExpr::Return(return_expr) => convert_return_expr(return_expr, loc, xx),
        AExpr::If(if_expr) => convert_if_expr(if_expr, loc, xx),
        AExpr::Match(match_expr) => convert_match_expr(match_expr, loc, xx),
        AExpr::While(while_expr) => convert_while_expr(while_expr, loc, xx),
        AExpr::Loop(loop_expr) => convert_loop_expr(loop_expr, loc, xx),
    }
}

/// `&expr` を生成する。
fn do_convert_lval(expr_id: AExprId, expr: &AExpr, k_mut: KMut, xx: &mut Xx) -> KTerm {
    let loc = Loc::new(xx.doc, PLoc::Expr(expr_id));

    match expr {
        AExpr::Name(name) => convert_name_lval(name, k_mut, ANameKey::Expr(expr_id), xx),
        AExpr::Record(expr) => convert_record_lval(expr_id, expr, loc, xx),
        AExpr::Field(field_expr) => convert_field_lval(field_expr, k_mut, loc, xx),
        AExpr::Index(index_expr) => convert_index_lval(index_expr, loc, xx),
        AExpr::UnaryOp(unary_op_expr) => convert_unary_op_lval(unary_op_expr, loc, xx),
        _ => {
            // break や if など、左辺値と解釈可能な式は他にもある。いまのところ実装する必要はない

            let symbol = match convert_expr(expr_id, xx) {
                KTerm::Name(it) => it,
                term => {
                    // FIXME: リテラルなら static を導入してそのアドレスを取る。

                    let symbol = fresh_symbol("lval", loc, xx);
                    xx.nodes.push(new_let_node(term, symbol, new_cont(), loc));
                    symbol
                }
            };
            let result = fresh_symbol("ref", loc, xx);
            xx.nodes
                .push(new_ref_node(KTerm::Name(symbol), result, new_cont(), loc));
            KTerm::Name(result)
        }
    }
}

fn convert_expr(expr_id: AExprId, xx: &mut Xx) -> KTerm {
    let expr = expr_id.of(xx.ast.exprs());
    do_convert_expr(expr_id, expr, xx)
}

fn convert_lval(expr_id: AExprId, k_mut: KMut, xx: &mut Xx) -> KTerm {
    let expr = expr_id.of(xx.ast.exprs());
    do_convert_lval(expr_id, expr, k_mut, xx)
}

fn convert_expr_opt(expr_id_opt: Option<AExprId>, loc: Loc, xx: &mut Xx) -> KTerm {
    match expr_id_opt {
        Some(expr_id) => convert_expr(expr_id, xx),
        None => new_error_term(loc),
    }
}

fn convert_lval_opt(expr_id_opt: Option<AExprId>, k_mut: KMut, loc: Loc, xx: &mut Xx) -> KTerm {
    match expr_id_opt {
        Some(expr_id) => convert_lval(expr_id, k_mut, xx),
        None => new_error_term(loc),
    }
}

fn convert_let_decl(decl_id: ADeclId, decl: &AFieldLikeDecl, loc: Loc, xx: &mut Xx) {
    let cause = KSymbolCause::NameDef(xx.doc, ANameKey::Decl(decl_id));

    let name_opt = decl.name_opt.as_ref().map(|name| name.text.to_string());

    let value = convert_expr_opt(decl.value_opt, loc, xx);
    let ty = convert_ty_opt(decl.ty_opt, &mut new_ty_resolver(xx)).to_ty2(xx.k_mod);

    let local = xx.local_vars.alloc(
        KLocalData::new(
            name_opt.clone().unwrap_or_else(|| "_".to_string()),
            cause.loc(),
        )
        .with_ty(ty),
    );
    let symbol = KSymbol { local, cause };

    xx.nodes.push(new_let_node(value, symbol, new_cont(), loc));

    if let Some(name) = name_opt {
        xx.env.insert_value(name, KLocalValue::LocalVar(local));
    }
}

fn convert_const_decl(k_const: KConst, decl: &AFieldLikeDecl, loc: Loc, xx: &mut Xx) {
    let (node, term) = {
        let mut nodes = take(&mut xx.nodes);
        let mut term_opt = None;
        xx.do_out_fn(|xx| {
            term_opt = Some(convert_expr_opt(decl.value_opt, loc, xx));
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
            term_opt = Some(convert_expr_opt(decl.value_opt, loc, xx));
        });
        swap(&mut xx.nodes, &mut nodes);

        (fold_nodes(nodes), term_opt.unwrap())
    };

    *static_var.of_mut(&mut xx.mod_data.static_vars) = KStaticVarInit {
        init_opt: Some((node, term)),
    };
}

fn new_param_name_key(decl_id: ADeclId, index: usize) -> ANameKey {
    ANameKey::Param(AParamDeclKey::new(decl_id, index))
}

fn convert_param_decls(
    param_decls: &[AParamDecl],
    param_tys: &[KTy],
    doc: Doc,
    decl_id: ADeclId,
    locals: &mut KLocalArena,
    env: &mut Env,
) -> Vec<KSymbol> {
    assert_eq!(param_decls.len(), param_tys.len());

    param_decls
        .iter()
        .enumerate()
        .zip(param_tys)
        .map(|((index, param_decl), _param_ty)| {
            let name_key = new_param_name_key(decl_id, index);
            let loc = Loc::new(doc, PLoc::Name(name_key));
            let name = param_decl.name.text.to_string();
            let local = locals.alloc(KLocalData::new(name.to_string(), loc));
            env.insert_value(name, KLocalValue::LocalVar(local));
            KSymbol {
                local,
                cause: KSymbolCause::NameDef(doc, name_key),
            }
        })
        .collect()
}

fn convert_fn_decl(decl_id: ADeclId, k_fn: KFn, fn_decl: &AFnLikeDecl, loc: Loc, xx: &mut Xx) {
    let mut locals = KLocalArena::new();

    let params = convert_param_decls(
        &fn_decl.params,
        k_fn.param_tys(&xx.mod_outline.fns),
        xx.doc,
        decl_id,
        &mut locals,
        &mut xx.env,
    );

    let fn_data = xx.do_out_fn(|xx| {
        xx.fn_opt = Some(k_fn);
        xx.local_vars = locals;

        // 関数の本体を格納しておくラベル
        xx.label = xx.labels.alloc(KLabelConstruction::default());

        let term = convert_expr_opt(fn_decl.body_opt, loc, xx);
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

        let locals = take(&mut xx.local_vars);
        let labels =
            KLabelArena::from_iter(take(&mut xx.labels).into_vec().into_iter().map(|label| {
                let name = label.name;
                let params = label.params;
                let body = fold_nodes(label.body);
                KLabelData { name, params, body }
            }));

        KFnData::new(params, locals, labels)
    });

    *k_fn.of_mut(&mut xx.mod_data.fns) = fn_data;
}

fn emit_return(term: KTerm, loc: Loc, xx: &mut Xx) {
    let k_fn = xx.fn_opt.unwrap();
    xx.nodes.push(new_return_tail(k_fn, term, loc));
}

fn convert_extern_fn_decl(
    decl_id: ADeclId,
    extern_fn: KExternFn,
    extern_fn_decl: &AFnLikeDecl,
    xx: &mut Xx,
) {
    let mut locals = KLocalArena::new();

    let params = convert_param_decls(
        &extern_fn_decl.params,
        extern_fn.param_tys(&xx.mod_outline.extern_fns),
        xx.doc,
        decl_id,
        &mut locals,
        &mut xx.env,
    );

    *extern_fn.of_mut(&mut xx.mod_data.extern_fns) = KExternFnData { params, locals };
}

fn convert_enum_decl(k_enum: KEnum, decl: &AEnumDecl, loc: Loc, xx: &mut Xx) {
    for (variant_decl, variant) in decl
        .variants
        .iter()
        .zip(k_enum.variants(&xx.mod_outline.enums).iter())
    {
        match variant_decl {
            AVariantDecl::Const(const_variant_decl) => {
                let k_const = variant.as_const().unwrap();
                convert_const_decl(k_const, const_variant_decl, loc, xx);
            }
            AVariantDecl::Record(_) => {}
        }
    }
}

fn do_convert_decl(decl_id: ADeclId, decl: &ADecl, term_opt: &mut Option<KTerm>, xx: &mut Xx) {
    let symbol_opt = *decl_id.of(xx.decl_symbols);
    let loc = Loc::new(xx.doc, PLoc::Decl(decl_id));

    match decl {
        ADecl::Attr => {}
        ADecl::Expr(expr) => {
            *term_opt = Some(convert_expr(*expr, xx));
        }
        ADecl::Let(decl) => {
            assert_eq!(symbol_opt, None);
            convert_let_decl(decl_id, decl, loc, xx);
        }
        ADecl::Const(decl) => {
            let k_const = match symbol_opt {
                Some(KModLocalSymbol::Const(it)) => it,
                _ => return,
            };
            convert_const_decl(k_const, decl, loc, xx)
        }
        ADecl::Static(decl) => {
            let static_var = match symbol_opt {
                Some(KModLocalSymbol::StaticVar(it)) => it,
                _ => return,
            };
            convert_static_decl(static_var, decl, loc, xx)
        }
        ADecl::Fn(fn_decl) => {
            let k_fn = match symbol_opt {
                Some(KModLocalSymbol::Fn(it)) => it,
                _ => return,
            };
            convert_fn_decl(decl_id, k_fn, fn_decl, loc, xx);
        }
        ADecl::ExternFn(extern_fn_decl) => {
            let extern_fn = match symbol_opt {
                Some(KModLocalSymbol::ExternFn(it)) => it,
                _ => return,
            };
            convert_extern_fn_decl(decl_id, extern_fn, extern_fn_decl, xx);
        }
        ADecl::Enum(enum_decl) => {
            let k_enum = match symbol_opt {
                Some(KModLocalSymbol::Enum(it)) => it,
                _ => return,
            };
            convert_enum_decl(k_enum, enum_decl, loc, xx);
        }
        ADecl::Struct(_) | ADecl::Use(_) => {}
    }
}

fn convert_decls(decls: ADeclIds, xx: &mut Xx) -> Option<KTerm> {
    for (decl_id, decl) in decls.enumerate(xx.ast.decls()) {
        if decl_allows_forward_reference(decl) {
            add_decl_to_local_env(decl_id, decl, xx.decl_symbols, xx.mod_outline, &mut xx.env);
        }
    }

    let mut last_opt = None;
    for (decl_id, decl) in decls.enumerate(xx.ast.decls()) {
        let mut term_opt = None;

        do_convert_decl(decl_id, decl, &mut term_opt, xx);

        if !decl_allows_forward_reference(decl) {
            add_decl_to_local_env(decl_id, decl, xx.decl_symbols, xx.mod_outline, &mut xx.env);
        }
        last_opt = term_opt;
    }
    last_opt
}

pub(crate) fn convert_to_cps(
    doc: Doc,
    k_mod: KMod,
    tree: &PTree,
    decl_symbols: &DeclSymbols,
    mod_outline: &KModOutline,
    mod_outlines: &KModOutlines,
    listener: &mut dyn NameResolutionListener,
    logger: &DocLogger,
) -> KModData {
    let mut xx = Xx::new(
        doc,
        k_mod,
        &tree.tokens,
        &tree.ast,
        decl_symbols,
        mod_outline,
        mod_outlines,
        listener,
        logger,
    );

    xx.mod_data = KModData {
        consts: mod_outline.consts.slice().map_with(KConstInit::new_empty),
        static_vars: mod_outline
            .static_vars
            .slice()
            .map_with(KStaticVarInit::new_empty),
        extern_fns: mod_outline.extern_fns.slice().map_with(Default::default),
        fns: mod_outline.fns.slice().map_with(Default::default),
    };

    xx.do_in_scope(|xx| {
        convert_decls(tree.ast.root_decls(), xx);
    });

    xx.mod_data
}
