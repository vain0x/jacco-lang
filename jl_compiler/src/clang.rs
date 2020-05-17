//! C言語を生成する機能

mod clang_dump;
mod clang_gen;
mod clang_node;

pub(crate) use clang_dump::clang_dump;
pub(crate) use clang_node::*;

use crate::cps::*;
use crate::id_provider::IdProvider;
use log::error;
