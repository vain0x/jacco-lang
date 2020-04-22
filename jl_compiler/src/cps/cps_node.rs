use super::*;
use std::fmt;

#[derive(Clone)]
pub(crate) struct KSymbol {
    pub(crate) id: usize,
    pub(crate) text: String,
    pub(crate) location: Location,
}

impl KSymbol {
    pub(crate) fn with_location(self, location: Location) -> Self {
        Self { location, ..self }
    }

    pub(crate) fn unique_name(&self) -> String {
        format!("{}_{}", self.text, self.id)
    }
}

impl Default for KSymbol {
    fn default() -> Self {
        KSymbol {
            id: usize::default(),
            text: String::default(),
            location: Location::new_dummy(),
        }
    }
}

impl fmt::Debug for KSymbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}_{}", self.text, self.id)
    }
}

#[derive(Clone, Debug)]
pub(crate) enum KNode {
    Abort,
    Prim {
        prim: KPrim,
        args: Vec<KTerm>,
        results: Vec<KSymbol>,
        conts: Vec<KNode>,
    },
    Jump {
        label: KSymbol,
        args: Vec<KTerm>,
    },
}

#[derive(Clone, Debug)]
pub(crate) enum KElement {
    Term(KTerm),
    Node(KNode),
}

#[derive(Clone, Debug)]
pub(crate) struct KFn {
    pub(crate) name: KSymbol,
    pub(crate) params: Vec<KSymbol>,
    pub(crate) body: KNode,
}

#[derive(Clone, Debug)]
pub(crate) struct KRoot {
    pub(crate) fns: Vec<KFn>,
}
