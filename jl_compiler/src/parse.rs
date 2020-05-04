mod binary_op;
mod parse_context;
mod parse_stmt;
mod parse_term;
mod parse_tree;

pub(crate) use binary_op::BinaryOp;
pub(crate) use parse_stmt::parse_tokens;
pub(crate) use parse_tree::*;

use crate::logs::Logger;
use crate::token::{Location, TokenData, TokenKind};
use parse_stmt::parse_semi;
use parse_term::{eat_block, eat_name, eat_term, parse_block, parse_name, parse_term};

type Px = parse_context::ParseContext;

/// 関数の中か外か。
/// QUESTION: より適切な名前？
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum Placement {
    /// 関数の内
    Local,

    /// 関数の外
    Global,
}
