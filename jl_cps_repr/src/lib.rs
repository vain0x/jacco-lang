mod cps_fold;
mod k_command;
mod k_const;
mod k_extern_fn;
mod k_field;
mod k_fn;
mod k_label;
mod k_local;
mod k_meta_ty;
mod k_node;
mod k_outlines;
mod k_prim;
mod k_root;
mod k_static_var;
mod k_struct;
mod k_symbol;
mod k_term;
mod k_ty;
mod k_ty_env;

pub mod source {
    mod location;
    mod logs;
    mod position;
    mod range;
    mod source_file;
    mod token_source;

    pub use location::{HaveLocation, Location};
    pub use logs::Logger;
    pub use position::Position;
    pub use range::Range;
    pub use source_file::SourceFile;
    pub use token_source::TokenSource;

    pub type SourceCode = String;
}

pub mod utils {
    mod id_provider;

    pub use id_provider::IdProvider;
}

pub use k_const::{KConst, KConstData, KConstValue};
pub use k_extern_fn::{KExternFn, KExternFnData, KExternFnOutline};
pub use k_field::{KField, KFieldOutline, KFieldTag};
pub use k_fn::{KFn, KFnData, KFnOutline};
pub use k_label::{KLabel, KLabelData, KLabelSig};
pub use k_local::{KLocal, KLocalData};
pub use k_meta_ty::{KMetaTy, KMetaTyData};
pub use k_node::KNode;
pub use k_outlines::KOutlines;
pub use k_prim::KPrim;
pub use k_root::KRoot;
pub use k_static_var::{KStaticVar, KStaticVarData};
pub use k_struct::{KStruct, KStructOutline};
pub use k_symbol::{KSymbol, KSymbolExt};
pub use k_term::KTerm;
pub use k_ty::KTy;
pub use k_ty_env::KTyEnv;

use crate::{
    k_command::KCommand,
    source::{logs::Logger, Location, Position, Range, SourceCode, SourceFile},
};
use log::{error, trace};
