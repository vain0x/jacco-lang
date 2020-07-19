mod cli;
mod logs;

/// API for Rust.
pub mod rust_api {
    pub use super::cli::{compile, Project};
    pub use super::lang_service::lang_service::LangService;
    pub use super::source::{doc::Doc, t_pos::TPos, t_pos16::TPos16, t_range::TRange};
}

mod clang {
    //! C言語を生成する機能

    mod c_binary_op;
    mod c_block;
    mod c_expr;
    mod c_root;
    mod c_stmt;
    mod c_ty;
    mod c_unary_op;
    mod clang_dump;
    mod clang_gen;

    pub(crate) use c_binary_op::CBinaryOp;
    pub(crate) use c_block::CBlock;
    pub(crate) use c_expr::CExpr;
    pub(crate) use c_root::CRoot;
    pub(crate) use c_stmt::CStmt;
    pub(crate) use c_ty::CTy;
    pub(crate) use c_unary_op::CUnaryOp;
    pub(crate) use clang_dump::clang_dump;

    use crate::cps::*;
    use crate::utils::IdProvider;
    use log::error;
}

mod cps {
    //! CPS 中間表現

    mod cps_fold;
    mod eliminate_unit;
    mod k_alias;
    mod k_command;
    mod k_const;
    mod k_enum;
    mod k_extern_fn;
    mod k_field;
    mod k_fn;
    mod k_label;
    mod k_local;
    mod k_meta_ty;
    mod k_mod;
    mod k_mut;
    mod k_node;
    mod k_prim;
    mod k_static_var;
    mod k_struct;
    mod k_symbol;
    mod k_term;
    mod k_ty;
    mod k_vis;
    mod type_resolution;

    pub(crate) use cps_fold::fold_block;
    pub(crate) use eliminate_unit::eliminate_unit;
    pub(crate) use k_alias::{KAlias, KAliasArena, KAliasOutline};
    pub(crate) use k_command::KCommand;
    pub(crate) use k_const::{KConst, KConstData, KConstTag, KConstValue, KNumber};
    pub(crate) use k_enum::{KEnum, KEnumOutline, KEnumRepr, KEnumReprs, KEnumTag, KVariant};
    pub(crate) use k_extern_fn::{
        KExternFn, KExternFnArena, KExternFnData, KExternFnOutline, KExternFnTag,
    };
    pub(crate) use k_field::{KField, KFieldArena, KFieldOutline, KFieldTag};
    pub(crate) use k_fn::{KFn, KFnArena, KFnData, KFnOutline, KFnTag};
    pub(crate) use k_label::{
        KLabel, KLabelArena, KLabelData, KLabelSig, KLabelSigArena, KLabelTag,
    };
    pub(crate) use k_local::{KLocal, KLocalArena, KLocalData, KLocalTag};
    pub(crate) use k_meta_ty::{KMetaTy, KTyEnv};
    pub(crate) use k_mod::{
        resolve_aliases, KMod, KModArena, KModData, KModLocalSymbol, KModOutline, KModOutlines,
        KModTag, KProjectSymbol,
    };
    pub(crate) use k_mut::KMut;
    pub(crate) use k_node::KNode;
    pub(crate) use k_prim::KPrim;
    pub(crate) use k_static_var::{KStaticVar, KStaticVarData, KStaticVarTag};
    pub(crate) use k_struct::{KStruct, KStructArena, KStructOutline, KStructParent, KStructTag};
    pub(crate) use k_symbol::{KSymbol, KSymbolExt};
    pub(crate) use k_term::KTerm;
    pub(crate) use k_ty::{KNumberTy, KTy, KTy2};
    pub(crate) use k_vis::KVis;
    pub(crate) use type_resolution::resolve_types;

    use crate::logs::Logger;
    use log::{error, trace};

    /// 符号付き？
    #[allow(unused)]
    #[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
    pub(crate) enum KIsSigned {
        Signed,
        Unsigned,
    }

    /// ビット幅
    #[allow(unused)]
    #[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
    pub(crate) enum KIntBits {
        /// 8-bit
        B8,
        B16,
        B32,
        B64,
        B128,
        /// ptr-sized
        Ptr,
    }

    #[allow(unused)]
    #[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
    pub(crate) struct KIntFlavor {
        is_signed: KIsSigned,
        bits: KIntBits,
    }

    #[allow(unused)]
    #[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
    pub(crate) enum KFloatBits {
        B16,
        B32,
        B64,
        B128,
    }

    #[allow(unused)]
    #[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
    pub(crate) enum KCharBits {
        B8,
        B16,
        B32,
    }
}

mod front {
    //! 構文木上の処理

    mod cps_conversion;
    mod name_resolution;
    mod occurrence_collection;
    mod syntax_validation;

    pub(crate) use cps_conversion::cps_conversion;
    pub(crate) use name_resolution::{resolve_name, NAbsName, NName, NParentFn, NameResolution};
    pub(crate) use occurrence_collection::{collect_occurrences, Occurrences};
    pub(crate) use syntax_validation::validate_syntax;

    use crate::parse::*;
}

mod lang_service {
    //! 入力支援機能 (LSP サーバーの内部実装)

    pub(crate) mod lang_service;
}

mod parse {
    //! 構文木・構文解析

    mod p_binary_op;
    mod p_token;
    mod p_unary_op;
    mod parse_context;
    mod parse_decl;
    mod parse_expr;
    mod parse_pat;
    mod parse_tree;
    mod parse_ty;

    pub(crate) use p_binary_op::PBinaryOp;
    pub(crate) use p_token::{PToken, PTokens};
    pub(crate) use p_unary_op::PUnaryOp;
    pub(crate) use parse_decl::parse_tokens;
    pub(crate) use parse_tree::*;

    use crate::logs::Logger;
    use crate::{
        cps::{KMut, KVis},
        token::{TokenData, TokenKind},
    };
    use parse_context::Px;
    use parse_decl::parse_semi;
    use parse_expr::{parse_block, parse_expr, parse_name};
    use parse_ty::{parse_ty, parse_ty_ascription};

    pub(crate) type PMut = (KMut, PToken);

    pub(crate) type PVis = (KVis, PToken);

    /// 関数の中か外か。
    /// FIXME: より適切な名前？
    #[derive(Clone, Copy, PartialEq, Eq)]
    pub(crate) enum Placement {
        /// 関数の内
        Local,

        /// 関数の外 (モジュールの直下)
        Global,
    }
}

mod source {
    //! ソースファイル

    pub(crate) mod doc;
    pub(crate) mod loc;
    pub(crate) mod t_pos;
    pub(crate) mod t_pos16;
    pub(crate) mod t_range;

    #[cfg(test)]
    pub(crate) mod cursor_text;

    pub(crate) use doc::Doc;
    pub(crate) use loc::{HaveLocation, Loc};
    pub(crate) use t_pos::TPos;
    pub(crate) use t_pos16::TPos16;
    pub(crate) use t_range::TRange;

    #[allow(unused)]
    #[cfg(test)]
    pub(crate) use cursor_text::CursorText;

    pub(crate) type SourceCode = String;
}

mod token {
    //! 字句・字句解析

    mod keyword;
    mod lit_binary;
    mod lit_decimal;
    mod lit_hex;
    mod punctuation;
    mod token_data;
    mod token_kind;
    mod token_source;
    mod tokenize_context;
    mod tokenize_rules;

    pub(crate) use keyword::Keyword;
    pub(crate) use lit_decimal::{eval_number, LitErr};
    pub(crate) use token_data::TokenData;
    pub(crate) use token_kind::TokenKind;
    pub(crate) use token_source::TokenSource;
    pub(crate) use tokenize_rules::tokenize;
}

mod utils {
    mod id_provider;
    mod take_out;

    #[macro_use]
    mod vec_arena;

    pub(crate) use id_provider::IdProvider;
    pub(crate) use take_out::TakeOut;

    pub(crate) use vec_arena::{RawId, VecArena, VecArenaId};
}
