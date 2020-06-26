use super::{KLabel, KPrim, KSymbol, KTerm, KTy};
use crate::token::Location;

#[derive(Debug)]
pub(crate) enum KCommand {
    Node {
        prim: KPrim,
        tys: Vec<KTy>,
        args: Vec<KTerm>,
        result_opt: Option<KSymbol>,
        cont_count: usize,
        location: Location,
    },
    Label {
        label: KLabel,
        params: Vec<KSymbol>,
    },
}
