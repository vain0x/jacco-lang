use super::*;

#[derive(Clone, Debug)]
pub(crate) enum KNode {
    Entry,
    Abort,
    Prim {
        prim: KPrim,
        args: Vec<KTerm>,
        results: Vec<String>,
        conts: Vec<KNode>,
    },
    Jump {
        label: String,
        args: Vec<KTerm>,
    },
}

#[derive(Debug)]
pub(crate) enum KElement {
    Term(KTerm),
    Node(KNode),
}

#[derive(Clone, Debug)]
pub(crate) struct KFn {
    pub(crate) name: String,
    pub(crate) params: Vec<String>,
    pub(crate) body: KNode,
}

#[derive(Clone, Debug)]
pub(crate) struct KRoot {
    pub(crate) fns: Vec<KFn>,
}
