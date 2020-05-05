//! 構文木上の処理

mod name_resolution;

pub(crate) use name_resolution::resolve_name;

use crate::parse::*;
