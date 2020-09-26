use crate::{cps::*, front::name_resolution::*, logs::*, parse::*, source::*, utils::*};
use std::mem::take;

/// CPS 変換の文脈
pub(super) struct Xx<'a> {
    // state
    pub(crate) mod_data: KModData,
    pub(crate) label: KLabel,
    pub(crate) nodes: Vec<KNode>,
    pub(crate) local_vars: KLocalVarArena,
    pub(crate) labels: VecArena<KLabelTag, KLabelConstruction>,
    pub(crate) ty_env: KTyEnv,
    /// return のターゲットとなる関数
    pub(crate) fn_opt: Option<KFn>,
    /// break/continue のターゲットとなるループ
    pub(crate) loop_opt: Option<KLoopData>,

    // read:
    pub(crate) doc: Doc,
    pub(crate) tokens: &'a PTokens,
    pub(crate) ast: &'a ATree,
    pub(crate) name_referents: &'a NameReferents,
    pub(crate) name_symbols: &'a mut NameSymbols,
    pub(crate) mod_outline: &'a KModOutline,
    pub(crate) logger: &'a DocLogger,
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

    pub(crate) fn do_out_fn<A>(&mut self, f: impl FnOnce(&mut Xx) -> A) -> A {
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

    pub(crate) fn path_resolution_context(&mut self) -> PathResolutionContext {
        PathResolutionContext {
            tokens: self.tokens,
            ast: self.ast,
            name_referents: self.name_referents,
            name_symbols: self.name_symbols,
            mod_outline: self.mod_outline,
        }
    }

    pub(crate) fn fresh_var(&mut self, hint: &str, cause: impl Into<KVarTermCause>) -> KVarTerm {
        fresh_var(hint, cause, self)
    }
}

#[derive(Default)]
pub(crate) struct KLabelConstruction {
    pub(crate) name: String,
    pub(crate) params: Vec<KVarTerm>,
    pub(crate) body: Vec<KNode>,
}

pub(crate) struct KLoopData {
    pub(crate) break_label: KLabel,
    pub(crate) continue_label: KLabel,
}

/// break した後や条件分岐から抜けた後を表すラベル。引数を1個持つ。
pub(crate) struct BreakLabel {
    pub(crate) label: KLabel,
    pub(crate) result: KVarTerm,
}

/// continue で戻ってくる先の、ループの先頭を表すラベル。引数を持たない。
pub(crate) struct ContinueLabel {
    pub(crate) label: KLabel,
}

// 入れ子のパターンはまだコンパイルできない
pub(crate) enum Branch {
    Case(KTerm),
    Default(KVarTerm),
}

pub(crate) type AfterLval = KTerm;
pub(crate) type AfterRval = KTerm;
pub(crate) type AfterJump = KTerm;

// FIXME: KTerm::Never を追加。型は never になるべき
pub(crate) fn new_never_term(loc: Loc) -> AfterRval {
    KTerm::Unit { loc }
}

// FIXME: KTerm::Unit に置き換える
pub(crate) fn new_unit_term(loc: Loc) -> AfterRval {
    KTerm::Unit { loc }
}

pub(crate) fn new_error_term(loc: Loc) -> AfterRval {
    KTerm::Unit { loc }
}

pub(crate) fn new_cont() -> KNode {
    KNode::default()
}

pub(crate) fn new_error_node(loc: Loc) -> KNode {
    KNode {
        prim: KPrim::Stuck,
        tys: vec![],
        args: vec![],
        results: vec![],
        conts: vec![new_cont()],
        loc,
    }
}

pub(crate) fn fold_nodes(mut nodes: Vec<KNode>) -> KNode {
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

pub(super) fn fresh_var(hint: &str, cause: impl Into<KVarTermCause>, xx: &mut Xx) -> KVarTerm {
    let cause = cause.into();
    let loc = cause.loc();
    let local_var = xx
        .local_vars
        .alloc(KLocalVarData::new(hint.to_string(), loc));
    KVarTerm { local_var, cause }
}

// ===============================================
// エラー
// ===============================================

pub(crate) fn error_unresolved_ty(loc: PLoc, logger: &DocLogger) {
    logger.error(loc, "これは型の名前だと思いますが、定義が見つかりません。")
}

pub(crate) fn error_unresolved_value(loc: PLoc, logger: &DocLogger) {
    logger.error(loc, "これは値の名前だと思いますが、定義が見つかりません。");
}

pub(crate) fn error_unsupported_path_ty(loc: PLoc, logger: &DocLogger) {
    logger.error(loc, "パスによる型の指定は未実装");
}

pub(crate) fn error_ty_arg_arity(loc: PLoc, logger: &DocLogger) {
    logger.error(loc, "型引数の個数が一致しません。");
}

pub(crate) fn error_invalid_ty_args(loc: PLoc, logger: &DocLogger) {
    logger.error(loc, "型引数は指定できません。");
}

pub(crate) fn error_expected_record_ty(loc: PLoc, logger: &DocLogger) {
    logger.error(loc, "これはレコードでなければいけません。");
}

pub(crate) fn error_rval_used_as_lval(loc: PLoc, logger: &DocLogger) {
    logger.error(loc, "この式は左辺値ではありません。参照元や代入先は、変数や配列の要素など、左辺値でなければいけません。");
}

pub(crate) fn error_no_such_field(loc: PLoc, logger: &DocLogger) {
    logger.error(loc, "この名前のフィールドはありません。");
}

pub(crate) fn error_redundant_field(loc: PLoc, logger: &DocLogger) {
    logger.error(loc, "このフィールドはすでに指定されています。");
}

pub(crate) fn error_missed_fields<'a>(
    names: impl Iterator<Item = &'a str>,
    loc: PLoc,
    logger: &DocLogger,
) {
    logger.error(
        loc,
        format!(
            "フィールドへの割り当てが不足しています: '{}'",
            names.collect::<Vec<_>>().join("', '")
        ),
    );
}

pub(crate) fn error_break_out_of_loop(loc: PLoc, logger: &DocLogger) {
    logger.error(loc, "ループの外では break を使えません。");
}

pub(crate) fn error_continue_out_of_loop(loc: PLoc, logger: &DocLogger) {
    logger.error(loc, "ループの外では continue を使えません。");
}

pub(crate) fn error_return_out_of_fn(loc: PLoc, logger: &DocLogger) {
    logger.error(loc, "関数の外では return を使えません。");
}

pub(crate) fn error_empty_match(loc: PLoc, logger: &DocLogger) {
    logger.error(loc, "空の match は未実装です。");
}

// ===============================================
// インターフェイス
// ===============================================

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
        xx.convert_decls(tree.ast.root_decls(), TyExpect::unit());
        *mod_data = xx.mod_data;
    }
}
