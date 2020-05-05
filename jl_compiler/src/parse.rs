mod binary_op;
mod parse_context;
mod parse_decl;
mod parse_expr;
mod parse_term;
mod parse_tree;

pub(crate) use binary_op::BinaryOp;
pub(crate) use parse_decl::parse_tokens;
pub(crate) use parse_tree::*;

use crate::logs::Logger;
use crate::token::{Location, TokenData, TokenKind};
use parse_context::Px;
use parse_decl::parse_semi;
use parse_expr::{parse_args, parse_block, parse_expr};
use parse_term::{parse_cond, parse_name, parse_term};

/// 関数の中か外か。
/// QUESTION: より適切な名前？
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum Placement {
    /// 関数の内
    Local,

    /// 関数の外
    Global,
}
