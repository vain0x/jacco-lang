use super::{source::Location, KLabel, KPrim, KSymbol, KTerm, KTy};

#[derive(Debug)]
pub enum KCommand {
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
