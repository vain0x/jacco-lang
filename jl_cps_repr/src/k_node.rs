use super::{KPrim, KSymbol, KTerm, KTy};
use crate::source::{HaveLocation, Location};
use std::fmt::{self, Debug, Formatter};
use std::mem::take;

#[derive(Clone, Default)]
pub struct KNode {
    pub(crate) prim: KPrim,
    pub(crate) tys: Vec<KTy>,
    pub(crate) args: Vec<KTerm>,
    pub(crate) results: Vec<KSymbol>,
    pub(crate) conts: Vec<KNode>,
    pub(crate) location: Location,
}

impl KNode {
    pub fn decompose(
        self,
    ) -> (
        KPrim,
        Vec<KTy>,
        Vec<KTerm>,
        Vec<KSymbol>,
        Vec<KNode>,
        Location,
    ) {
        (
            self.prim,
            self.tys,
            self.args,
            self.results,
            self.conts,
            self.location,
        )
    }

    pub fn take_tys(&mut self) -> Vec<KTy> {
        take(&mut self.tys)
    }

    pub fn take_args(&mut self) -> Vec<KTerm> {
        take(&mut self.args)
    }

    pub fn take_results(&mut self) -> Vec<KSymbol> {
        take(&mut self.results)
    }

    pub fn take_conts(&mut self) -> Vec<KNode> {
        take(&mut self.conts)
    }
}

impl Debug for KNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.tys.is_empty() {
            write!(f, "({:?}", self.prim)?;
        } else {
            write!(f, "({:?}::", self.prim)?;

            let mut list = f.debug_list();
            for ty in &self.tys {
                list.entry(ty);
            }
            list.finish()?;
        }

        write!(f, " ")?;

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
                Debug::fmt(cont, f)?;
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

impl HaveLocation for KNode {
    fn location(&self) -> Location {
        self.location.clone()
    }
}
