use crate::{front::name_resolution::BuiltInTy, parse::ANameId};

/// 識別子やパスの先頭が構文的に指しているもの。
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum LexicalReferent {
    Unresolved,

    /// 名前が定義箇所として出現していることを表す。
    Def,

    /// 名前が使用箇所として出現していることを表す。(参照先は構文木内の定義箇所)
    Name(ANameId),

    BuiltInTy(BuiltInTy),
}
