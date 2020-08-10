use crate::{
    parse::*,
    source::{Doc, Loc, TRange},
};
use std::fmt::{self, Debug, Formatter};

/// 構文木上の位置
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub(crate) enum PLoc {
    #[allow(unused)]
    Unknown(&'static str),
    Range(TRange),
    Token(PToken),
    TokenBehind(PToken),
    #[allow(unused)]
    TokenRange {
        first: PToken,
        last: PToken,
    },
    #[allow(unused)]
    Element(PElement),
    // KTy に位置情報が含まれないので未使用
    #[allow(unused)]
    Ty(ATyId),
    Pat(APatId),
    Expr(AExprId),
    Decl(ADeclId),
    Name(ANameKey),
    #[allow(unused)]
    ParamDecl(AParamDeclKey),
    #[allow(unused)]
    FieldDecl(AFieldDeclKey),
    #[allow(unused)]
    VariantDecl(AVariantDeclKey),
}

impl PLoc {
    pub(crate) fn new(token: PToken) -> Self {
        PLoc::Token(token)
    }

    #[allow(unused)]
    pub(crate) fn behind(self) -> Self {
        match self {
            PLoc::Token(token) => PLoc::TokenBehind(token),
            _ => self,
        }
    }

    pub(crate) fn range(self, tree: &PTree) -> Result<TRange, &'static str> {
        let range = match self {
            PLoc::Unknown(hint) => return Err(hint),
            PLoc::Range(range) => range,
            PLoc::Token(token) => token.range(&tree.tokens),
            PLoc::TokenBehind(token) => token.range(&tree.tokens).to_end(),
            PLoc::TokenRange { first, last } => {
                first.range(&tree.tokens).join(last.range(&tree.tokens))
            }
            PLoc::Element(element) => element.range(tree)?,
            PLoc::Ty(ty_id) => ty_id.element(tree).range(tree)?,
            PLoc::Pat(pat_id) => pat_id.element(tree).range(tree)?,
            PLoc::Expr(expr_id) => expr_id.element(tree).range(tree)?,
            PLoc::Decl(decl_id) => decl_id.element(tree).range(tree)?,
            PLoc::Name(key) => key.element(tree).range(tree)?,
            PLoc::ParamDecl(key) => key.element(tree).range(tree)?,
            PLoc::FieldDecl(key) => key.element(tree).range(tree)?,
            PLoc::VariantDecl(key) => key.element(tree).range(tree)?,
        };
        Ok(range)
    }

    pub(crate) fn from_loc(loc: Loc) -> Self {
        match loc.inner() {
            Ok((_, loc)) => loc,
            Err(hint) => PLoc::Unknown(hint),
        }
    }

    #[allow(unused)]
    pub(crate) fn to_loc(self, doc: Doc) -> Loc {
        Loc::new(doc, self)
    }
}

impl Debug for PLoc {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let (token, suffix) = match self {
            PLoc::Unknown(hint) => return write!(f, "{}", hint),
            PLoc::Range(range) => return write!(f, "{}", range),
            PLoc::Token(token) => (token, ""),
            PLoc::TokenBehind(token) => (token, ":behind"),
            PLoc::TokenRange { first, last } => {
                return write!(f, "token#({}..{})", first.to_index(), last.to_index());
            }
            PLoc::Element(element) => return write!(f, "element#{}", element.to_index()),
            PLoc::Ty(ty_id) => return write!(f, "ty#{}", ty_id.to_index()),
            PLoc::Pat(pat_id) => return write!(f, "pat#{}", pat_id.to_index()),
            PLoc::Expr(expr_id) => return write!(f, "expr#{}", expr_id.to_index()),
            PLoc::Decl(decl_id) => return write!(f, "decl#{}", decl_id.to_index()),
            PLoc::Name(_) => return write!(f, "Name"),
            PLoc::ParamDecl(_) => return write!(f, "ParamDecl"),
            PLoc::FieldDecl(_) => return write!(f, "FieldDecl"),
            PLoc::VariantDecl(_) => return write!(f, "VariantDecl"),
        };

        Debug::fmt(&token, f)?;
        write!(f, "{}", suffix)
    }
}
