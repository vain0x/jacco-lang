use super::{KFn, KNode, KSymbol, KTerm, KTy};
use crate::source::Loc;

/// CPS 中間表現のプリミティブの種類
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum KPrim {
    /// 行き詰まり
    Stuck,
    Jump,
    CallDirect,
    Record,
    /// 構造体へのポインタから特定のフィールドへのポインタを取得する。(`&p->field`)
    GetField,
    GetFieldMut,
    If,
    Switch,
    Let,
    Deref,
    Ref,
    RefMut,
    Minus,
    Not,
    Add,
    Sub,
    Mul,
    Div,
    Modulo,
    BitAnd,
    BitOr,
    BitXor,
    LeftShift,
    RightShift,
    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    Cast,
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModuloAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
    LeftShiftAssign,
    RightShiftAssign,
}

impl KPrim {
    pub(crate) fn hint_str(self) -> String {
        format!("{:?}", self).to_lowercase()
    }
}

pub(crate) fn new_call_node(args: Vec<KTerm>, result: KSymbol, cont: KNode, loc: Loc) -> KNode {
    KNode {
        prim: KPrim::CallDirect,
        tys: vec![],
        args,
        results: vec![result],
        conts: vec![cont],
        loc,
    }
}

/// 末尾ではない `return`
pub(crate) fn new_return_node(k_fn: KFn, arg: KTerm, loc: Loc) -> KNode {
    KNode {
        prim: KPrim::Jump,
        tys: vec![],
        args: vec![KTerm::Return { k_fn, loc }, arg],
        results: vec![],
        conts: vec![KNode::default()],
        loc,
    }
}

/// 末尾の `return`
pub(crate) fn new_return_tail(k_fn: KFn, arg: KTerm, loc: Loc) -> KNode {
    KNode {
        prim: KPrim::Jump,
        tys: vec![],
        args: vec![KTerm::Return { k_fn, loc }, arg],
        results: vec![],
        conts: vec![],
        loc,
    }
}

pub(crate) fn new_record_node(ty: KTy, result: KSymbol, cont: KNode, loc: Loc) -> KNode {
    KNode {
        prim: KPrim::Record,
        tys: vec![ty],
        args: vec![],
        results: vec![result.clone()],
        conts: vec![cont],
        loc,
    }
}

pub(crate) fn new_let_node(init: KTerm, result: KSymbol, loc: Loc, cont: KNode) -> KNode {
    KNode {
        prim: KPrim::Let,
        tys: vec![],
        args: vec![init],
        results: vec![result],
        conts: vec![cont],
        loc,
    }
}

pub(crate) fn new_deref_node(arg: KTerm, result: KSymbol, cont: KNode, loc: Loc) -> KNode {
    KNode {
        prim: KPrim::Deref,
        tys: vec![],
        args: vec![arg],
        results: vec![result],
        conts: vec![cont],
        loc,
    }
}

pub(crate) fn new_minus_node(arg: KTerm, result: KSymbol, cont: KNode, loc: Loc) -> KNode {
    KNode {
        prim: KPrim::Minus,
        tys: vec![],
        args: vec![arg],
        results: vec![result],
        conts: vec![cont],
        loc,
    }
}

pub(crate) fn new_not_node(arg: KTerm, result: KSymbol, cont: KNode, loc: Loc) -> KNode {
    KNode {
        prim: KPrim::Not,
        tys: vec![],
        args: vec![arg],
        results: vec![result],
        conts: vec![cont],
        loc,
    }
}
