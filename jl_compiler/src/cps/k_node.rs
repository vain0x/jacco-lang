use super::*;
use crate::{
    source::{HaveLoc, Loc},
    utils::{DebugWith, DebugWithContext},
};
use std::fmt::{self, Formatter};

#[derive(Clone)]
pub(crate) struct KNode {
    pub(crate) prim: KPrim,
    pub(crate) tys: Vec<KTy2>,
    pub(crate) args: Vec<KTerm>,
    pub(crate) results: Vec<KVarTerm>,
    pub(crate) conts: Vec<KNode>,
    pub(crate) loc: Loc,
}

impl KNode {
    #[allow(unused)]
    pub(crate) fn with_debug<'a, A>(
        &'a self,
        mod_outline: &'a KModOutline,
        mod_data: &'a KModData,
        k_fn: KFn,
        f: impl FnOnce(&dyn fmt::Debug) -> A,
    ) -> A {
        let fn_data = k_fn.of(&mod_data.fns);
        f(&DebugWith::new(
            self,
            &(mod_outline, Some((&fn_data.local_vars, &fn_data.labels))),
        ))
    }
}

impl<'a>
    DebugWithContext<(
        &'a KModOutline,
        Option<(&'a KLocalVarArena, &'a KLabelArena)>,
    )> for KNode
{
    fn fmt(
        &self,
        context: &(
            &'a KModOutline,
            Option<(&'a KLocalVarArena, &'a KLabelArena)>,
        ),
        f: &mut Formatter<'_>,
    ) -> fmt::Result {
        if self.tys.is_empty() {
            write!(f, "({:?}", self.prim)?;
        } else {
            write!(f, "({:?}::", self.prim)?;

            let mut list = f.debug_list();
            for _ty in &self.tys {
                // FIXME: DebugWithContext を使う
                list.entry(&"<ty>");
            }
            list.finish()?;
        }

        write!(f, " ")?;

        {
            let mut list = f.debug_list();
            for _arg in &self.args {
                // FIXME: DebugWithContext を使う
                // list.entry(&DebugWith::new(arg, context));
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
