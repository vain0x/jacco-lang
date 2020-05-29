//! CPS 中間表現

mod cps_fold;
mod cps_gen;
mod cps_node;
mod cps_outline;
mod cps_prim;
mod cps_term;
mod type_resolution;

pub(crate) use cps_gen::cps_conversion;
pub(crate) use cps_node::*;
pub(crate) use cps_outline::*;
pub(crate) use cps_prim::KPrim;
pub(crate) use cps_term::KTerm;

use crate::logs::Logger;
use crate::token::{HaveLocation, Location, TokenData};
use log::{error, trace};
