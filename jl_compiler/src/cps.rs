//! CPS 中間表現

mod cps_fold;
mod cps_gen;
mod k_command;
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
mod k_struct;
mod k_symbol;
mod k_term;
mod k_ty;
mod k_ty_env;
mod type_resolution;

pub(crate) use cps_gen::cps_conversion;
pub(crate) use k_command::KCommand;
pub(crate) use k_extern_fn::{KExternFn, KExternFnData, KExternFnOutline};
pub(crate) use k_field::{KField, KFieldOutline, KFieldTag};
pub(crate) use k_fn::{KFn, KFnData, KFnOutline};
pub(crate) use k_label::{KLabel, KLabelData, KLabelSig};
pub(crate) use k_local::{KLocal, KLocalData};
pub(crate) use k_meta_ty::{KMetaTy, KMetaTyData};
pub(crate) use k_node::KNode;
pub(crate) use k_outlines::KOutlines;
pub(crate) use k_prim::KPrim;
pub(crate) use k_root::KRoot;
pub(crate) use k_struct::{KStruct, KStructOutline};
pub(crate) use k_symbol::{KSymbol, KSymbolExt};
pub(crate) use k_term::KTerm;
pub(crate) use k_ty::KTy;
pub(crate) use k_ty_env::KTyEnv;

use crate::logs::Logger;
use crate::token::{HaveLocation, Location, TokenData};
use log::{error, trace};
