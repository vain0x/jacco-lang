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
        write!(f, "{}_{}", self.text, self.id)
    }
}

#[derive(Clone)]
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

impl fmt::Debug for KNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            KNode::Abort => write!(f, "(abort)"),
            KNode::Prim {
                prim,
                args,
                results,
                conts,
            } => {
                write!(f, "({:?} ", prim)?;

                {
                    let mut list = f.debug_list();
                    for arg in args {
                        list.entry(arg);
                    }
                    list.finish()?;
                }

                write!(f, " ")?;

                {
                    let mut list = f.debug_list();
                    for result in results {
                        list.entry(result);
                    }
                    list.finish()?;
                }

                if conts.len() == 1 {
                    write!(f, ") => ")?;

                    for cont in conts {
                        fmt::Debug::fmt(cont, f)?;
                    }

                    Ok(())
                } else {
                    write!(f, " ")?;

                    let mut list = f.debug_list();
                    for cont in conts {
                        list.entry(cont);
                    }
                    list.finish()?;

                    write!(f, ")")
                }
            }
            KNode::Jump { label, args } => {
                write!(f, "(jump ")?;
                fmt::Debug::fmt(label, f)?;
                write!(f, " ")?;

                {
                    let mut list = f.debug_list();
                    for arg in args {
                        list.entry(arg);
                    }
                    list.finish()?;
                }

                write!(f, ")")
            }
        }
    }
}

#[derive(Clone)]
pub(crate) enum KElement {
    Term(KTerm),
    Node(KNode),
}

impl fmt::Debug for KElement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            KElement::Term(term) => fmt::Debug::fmt(term, f),
            KElement::Node(node) => fmt::Debug::fmt(node, f),
        }
    }
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
