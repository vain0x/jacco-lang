use super::{KFieldTag, KFn, KLabel, KMut, KNode, KSymbol, KTerm, KTy};
use crate::source::Loc;
use std::iter::once;

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

// 末尾でない `jump`
#[allow(unused)]
pub(crate) fn new_jump_node(
    label: KLabel,
    args: impl IntoIterator<Item = KTerm>,
    cont: KNode,
    loc: Loc,
) -> KNode {
    KNode {
        prim: KPrim::Jump,
        tys: vec![],
        args: once(KTerm::Label { label, loc }).chain(args).collect(),
        results: vec![],
        conts: vec![cont],
        loc,
    }
}

// 末尾の `jump`
pub(crate) fn new_jump_tail(
    label: KLabel,
    args: impl IntoIterator<Item = KTerm>,
    loc: Loc,
) -> KNode {
    KNode {
        prim: KPrim::Jump,
        tys: vec![],
        args: once(KTerm::Label { label, loc }).chain(args).collect(),
        results: vec![],
        conts: vec![],
        loc,
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

pub(crate) fn new_record_node(
    ty: KTy,
    args: Vec<KTerm>,
    result: KSymbol,
    cont: KNode,
    loc: Loc,
) -> KNode {
    KNode {
        prim: KPrim::Record,
        tys: vec![ty],
        args,
        results: vec![result.clone()],
        conts: vec![cont],
        loc,
    }
}

pub(crate) fn new_field_node(
    left: KTerm,
    right: String,
    right_loc: Loc,
    k_mut: KMut,
    result: KSymbol,
    cont: KNode,
    loc: Loc,
) -> KNode {
    let prim = match k_mut {
        KMut::Const => KPrim::GetField,
        KMut::Mut => KPrim::GetFieldMut,
    };
    KNode {
        prim,
        tys: vec![],
        args: vec![
            left,
            KTerm::FieldTag(KFieldTag {
                name: right,
                loc: right_loc,
            }),
        ],
        results: vec![result],
        conts: vec![cont],
        loc,
    }
}

pub(crate) fn new_if_node(cond: KTerm, body_cont: KNode, alt_cont: KNode, loc: Loc) -> KNode {
    KNode {
        prim: KPrim::If,
        tys: vec![],
        args: vec![cond],
        results: vec![],
        conts: vec![body_cont, alt_cont],
        loc,
    }
}

pub(crate) fn new_switch_tail(args: Vec<KTerm>, conts: Vec<KNode>, loc: Loc) -> KNode {
    KNode {
        prim: KPrim::Switch,
        tys: vec![],
        args,
        results: vec![],
        conts,
        loc,
    }
}

pub(crate) fn new_let_node(init: KTerm, result: KSymbol, cont: KNode, loc: Loc) -> KNode {
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

pub(crate) fn new_ref_node(arg: KTerm, result: KSymbol, cont: KNode, loc: Loc) -> KNode {
    KNode {
        prim: KPrim::Ref,
        tys: vec![],
        args: vec![arg],
        results: vec![result],
        conts: vec![cont],
        loc,
    }
}

pub(crate) fn new_ref_mut_node(arg: KTerm, result: KSymbol, cont: KNode, loc: Loc) -> KNode {
    KNode {
        prim: KPrim::RefMut,
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

pub(crate) fn new_assignment_node(
    prim: KPrim,
    left: KTerm,
    right: KTerm,
    cont: KNode,
    loc: Loc,
) -> KNode {
    KNode {
        prim,
        tys: vec![],
        args: vec![left, right],
        results: vec![],
        conts: vec![cont],
        loc,
    }
}

pub(crate) fn new_basic_binary_op_node(
    prim: KPrim,
    left: KTerm,
    right: KTerm,
    result: KSymbol,
    cont: KNode,
    loc: Loc,
) -> KNode {
    KNode {
        prim,
        tys: vec![],
        args: vec![left, right],
        results: vec![result],
        conts: vec![cont],
        loc,
    }
}

pub(crate) fn new_add_node(
    left: KTerm,
    right: KTerm,
    result: KSymbol,
    cont: KNode,
    loc: Loc,
) -> KNode {
    new_basic_binary_op_node(KPrim::Add, left, right, result, cont, loc)
}

pub(crate) fn new_cast_node(ty: KTy, arg: KTerm, result: KSymbol, cont: KNode, loc: Loc) -> KNode {
    KNode {
        prim: KPrim::Cast,
        tys: vec![ty],
        args: vec![arg],
        results: vec![result],
        conts: vec![cont],
        loc,
    }
}
