//! 構文木・構文解析

mod binary_op;
mod parse_context;
mod parse_decl;
mod parse_expr;
mod parse_tree;

pub(crate) use binary_op::BinaryOp;
pub(crate) use parse_decl::parse_tokens;
pub(crate) use parse_tree::*;

use crate::logs::Logger;
use crate::token::{Location, TokenData, TokenKind};
use parse_context::Px;
use parse_decl::parse_semi;
use parse_expr::{parse_block, parse_expr, parse_name};

/// 関数の中か外か。
/// FIXME: より適切な名前？
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum Placement {
    /// 関数の内
    Local,

    /// 関数の外 (モジュールの直下)
    Global,
}
