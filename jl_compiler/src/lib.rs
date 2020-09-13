mod cli;
mod logs;
mod tests;

/// API for Rust.
pub mod rust_api {
    pub use super::cli::{compile_v2, Project};
    pub use super::lang_service::lang_service::{Content, LangService};
    pub use super::source::{doc::Doc, TPos, TPos16, TRange};
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
    use log::error;
}

mod cps {
    //! CPS 中間表現

    mod eliminate_unit;
    mod eval;
    mod k_alias;
    mod k_const;
    mod k_const_enum;
    mod k_extern_fn;
    mod k_field;
    mod k_fn;
    mod k_label;
    mod k_local_var;
    mod k_meta_ty;
    mod k_mod;
    mod k_mut;
    mod k_node;
    mod k_number;
    mod k_number_ty;
    mod k_prim;
    mod k_static_var;
    mod k_struct;
    mod k_struct_enum;
    mod k_symbol;
    mod k_term;
    mod k_ty;
    mod k_value;
    mod k_vis;
    mod type_resolution;

    pub(crate) use eliminate_unit::eliminate_unit;
    pub(crate) use eval::eval_cps;
    pub(crate) use k_alias::*;
    pub(crate) use k_const::*;
    pub(crate) use k_const_enum::*;
    pub(crate) use k_extern_fn::*;
    pub(crate) use k_field::{KField, KFieldArena, KFieldOutline, KFieldTag};
    pub(crate) use k_fn::*;
    pub(crate) use k_label::{
        KLabel, KLabelArena, KLabelData, KLabelSig, KLabelSigArena, KLabelTag,
    };
    pub(crate) use k_local_var::*;
    pub(crate) use k_meta_ty::*;
    pub(crate) use k_mod::*;
    pub(crate) use k_mut::KMut;
    pub(crate) use k_node::KNode;
    pub(crate) use k_number::KNumber;
    pub(crate) use k_number_ty::KNumberTy;
    pub(crate) use k_prim::*;
    pub(crate) use k_static_var::*;
    pub(crate) use k_struct::*;
    pub(crate) use k_struct_enum::*;
    pub(crate) use k_symbol::{KSymbol, KSymbolCause};
    pub(crate) use k_term::{KTerm, KTermCause};
    pub(crate) use k_ty::*;
    pub(crate) use k_value::{KLocalValue, KProjectValue};
    pub(crate) use k_vis::KVis;
    pub(crate) use type_resolution::resolve_types;

    use crate::logs::Logger;
    use log::error;
}

mod front {
    //! 構文木上の処理

    mod cps_conversion;
    mod mod_outline_gen;
    pub(crate) mod name_resolution;

    pub(crate) use cps_conversion::convert_to_cps;
    pub(crate) use mod_outline_gen::generate_outline;

    use crate::parse::*;
}

mod lang_service {
    //! 入力支援機能 (LSP サーバーの内部実装)

    mod doc_analysis;
    mod actions {
        mod completion;
        mod definitions;
        mod document_highlight;
        mod hover;
        mod references;
        mod rename;
        mod validate;

        pub(super) use completion::completion;
        pub(super) use definitions::definitions;
        pub(super) use document_highlight::document_highlight;
        pub(super) use hover::hover;
        pub(super) use references::references;
        pub(super) use rename::{prepare_rename, rename};
        pub(super) use validate::validate;

        use crate::lang_service::lang_service::{
            collect_def_sites, collect_use_sites, hit_test, LangService, Location,
        };
        use crate::source::{Doc, TPos16, TRange};
    }

    pub(crate) mod lang_service;
}

mod parse {
    //! 構文木・構文解析

    mod ast;
    mod ast_gen;
    mod p_binary_op;
    mod p_element;
    mod p_loc;
    mod p_node;
    mod p_token;
    mod p_tree;
    mod p_tree_builder;
    mod p_unary_op;
    mod parse_context;
    mod parse_decl;
    mod parse_expr;
    mod parse_pat;
    mod parse_ty;
    mod syntax_error;

    pub(crate) use ast::*;
    pub(crate) use p_binary_op::PBinaryOp;
    pub(crate) use p_element::*;
    pub(crate) use p_loc::PLoc;
    pub(crate) use p_node::*;
    pub(crate) use p_token::{PToken, PTokens};
    pub(crate) use p_tree::PTree;
    pub(crate) use p_tree_builder::*;
    pub(crate) use p_unary_op::PUnaryOp;
    pub(crate) use parse_decl::parse_tokens;

    use crate::{
        cps::{KMut, KVis},
        token::{TokenData, TokenKind},
    };
    use ast_gen::*;
    use parse_context::Px;
    use parse_decl::parse_semi;
    use parse_expr::{parse_block, parse_expr, parse_qualifiable_name};
    use parse_ty::{parse_ty, parse_ty_ascription};

    pub(crate) type PMut = (KMut, PToken);

    pub(crate) type PVis = (KVis, PToken);
}

mod scope {
    pub(crate) mod lexical_referent;
    pub(crate) mod scope_system;
    pub(crate) mod scope_walker;
    pub(crate) mod syntax_scopes;
}

mod source {
    //! ソースファイル

    pub(crate) mod doc;
    pub(crate) mod loc;

    #[cfg(test)]
    pub(crate) mod cursor_text;

    pub(crate) use doc::Doc;
    pub(crate) use loc::{HaveLoc, Loc};

    #[allow(unused)]
    #[cfg(test)]
    pub(crate) use cursor_text::CursorText;

    pub(crate) type SourceCode = String;

    pub use text_position_rs::CompositePosition as TPos;
    pub use text_position_rs::Utf16Position as TPos16;

    pub type TRange = text_position_rs::TextRange<TPos>;
    pub type TRange16 = text_position_rs::TextRange<TPos16>;
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
    mod tokenize_context;
    mod tokenize_rules;

    pub(crate) use keyword::Keyword;
    pub(crate) use lit_decimal::{eval_number, LitErr};
    pub(crate) use token_data::TokenData;
    pub(crate) use token_kind::TokenKind;
    pub(crate) use tokenize_rules::tokenize;
}

mod utils {
    mod debug_with;
    mod id_provider;
    mod map_stack;
    mod take_out;

    #[macro_use]
    mod vec_arena;

    pub(crate) use debug_with::{DebugWith, DebugWithContext};
    pub(crate) use id_provider::IdProvider;
    pub(crate) use map_stack::MapStack;
    pub(crate) use vec_arena::{RawId, VecArena, VecArenaId, VecArenaSlice};

    #[allow(unused)]
    pub(crate) use bumpalo::{
        boxed::Box as BumpBox,
        collections::{String as BumpString, Vec as BumpVec},
        vec as bump_vec, Bump,
    };
}
