use super::{KLabel, KPrim, KSymbol, KTerm, KTy};
use crate::source::Loc;

#[derive(Debug)]
pub(crate) enum KCommand {
    Node {
        prim: KPrim,
        tys: Vec<KTy>,
        args: Vec<KTerm>,
        result_opt: Option<KSymbol>,
        cont_count: usize,
        location: Loc,
    },
    Label {
        label: KLabel,
        params: Vec<KSymbol>,
    },
}
