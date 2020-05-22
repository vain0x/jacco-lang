//! 構文木上の処理

mod name_resolution;
mod syntax_validation;

pub(crate) use name_resolution::resolve_name;
pub(crate) use syntax_validation::validate_syntax;

use crate::logs::Logger;
use crate::parse::*;
use crate::token::{HaveLocation, Location, TokenData};
