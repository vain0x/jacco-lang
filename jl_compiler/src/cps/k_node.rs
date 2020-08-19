use super::{KLabelArena, KLocalArena, KModOutline, KPrim, KSymbol, KTerm, KTy2};
use crate::{
    source::{HaveLoc, Loc},
    utils::{DebugWith, DebugWithContext},
};
use std::fmt::{self, Debug, Formatter};

#[derive(Clone)]
pub(crate) struct KNode {
    pub(crate) prim: KPrim,
    pub(crate) tys: Vec<KTy2>,
    pub(crate) args: Vec<KTerm>,
    pub(crate) results: Vec<KSymbol>,
    pub(crate) conts: Vec<KNode>,
    pub(crate) loc: Loc,
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
            for _arg in &self.args {
                // FIXME: DebugWithContext を使う
                list.entry(&"<arg>");
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

impl<'a> DebugWithContext<(&'a KModOutline, Option<(&'a KLocalArena, &'a KLabelArena)>)> for KNode {
    fn fmt(
        &self,
        context: &(&'a KModOutline, Option<(&'a KLocalArena, &'a KLabelArena)>),
        f: &mut Formatter<'_>,
    ) -> fmt::Result {
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
                list.entry(&DebugWith::new(arg, context));
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
                DebugWithContext::fmt(cont, context, f)?;
            }

            Ok(())
        } else {
            write!(f, " ")?;

            let mut list = f.debug_list();
            for cont in &self.conts {
                list.entry(&DebugWith::new(cont, context));
            }
            list.finish()?;

            write!(f, ")")
        }
    }
}

impl Default for KNode {
    fn default() -> Self {
        KNode {
            prim: KPrim::Stuck,
            tys: Default::default(),
            args: Default::default(),
            results: Default::default(),
            conts: Default::default(),
            loc: Loc::new_unknown("<KNode::default>"),
        }
    }
}

impl HaveLoc for KNode {
    fn loc(&self) -> Loc {
        self.loc
    }
}
