//! CPS 中間表現や、CPS 中間表現のもとになる命令列の定義

use super::*;
use std::fmt;

#[derive(Debug)]
pub(crate) enum XCommand {
    Prim {
        prim: KPrim,
        args: Vec<KTerm>,
        result_opt: Option<KSymbol>,
        cont_count: usize,
    },
    Label {
        label: KSymbol,
        params: Vec<KSymbol>,
    },
}

#[derive(Clone, Debug)]
pub(crate) enum KTy {
    Unit,
    I32,
}

#[derive(Clone)]
pub(crate) struct KSymbol {
    pub(crate) id: usize,
    pub(crate) text: String,
    pub(crate) location: Location,
}

impl KSymbol {
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
pub(crate) struct KNode {
    pub(crate) prim: KPrim,
    pub(crate) args: Vec<KTerm>,
    pub(crate) results: Vec<KSymbol>,
    pub(crate) conts: Vec<KNode>,
}

impl Default for KNode {
    fn default() -> Self {
        KNode {
            prim: KPrim::Stuck,
            args: vec![],
            results: vec![],
            conts: vec![],
        }
    }
}

impl fmt::Debug for KNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({:?} ", self.prim)?;

        {
            let mut list = f.debug_list();
            for arg in &self.args {
                list.entry(arg);
            }
            list.finish()?;
        }

        write!(f, " ")?;

        {
            let mut list = f.debug_list();
            for result in &self.results {
                list.entry(result);
            }
            list.finish()?;
        }

        if self.conts.len() == 1 {
            write!(f, ") => ")?;

            for cont in &self.conts {
                fmt::Debug::fmt(cont, f)?;
            }

            Ok(())
        } else {
            write!(f, " ")?;

            let mut list = f.debug_list();
            for cont in &self.conts {
                list.entry(cont);
            }
            list.finish()?;

            write!(f, ")")
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct KFn {
    pub(crate) name: KSymbol,
    pub(crate) params: Vec<KSymbol>,
    pub(crate) body: KNode,
    pub(crate) labels: Vec<KFn>,
}

#[derive(Clone, Debug)]
pub(crate) struct KExternFn {
    pub(crate) name: KSymbol,
    pub(crate) params: Vec<(KSymbol, KTy)>,
    pub(crate) result: KTy,
}

#[derive(Clone, Debug)]
pub(crate) struct KRoot {
    pub(crate) extern_fns: Vec<KExternFn>,
    pub(crate) fns: Vec<KFn>,
}
