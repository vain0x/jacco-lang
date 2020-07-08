mod logs;

/// API for Rust.
pub mod rust_api {
    pub use super::lang_service::lang_service::LangService;
    pub use super::source::{doc::Doc, pos::Pos, range::Range};
}

/// ワークスペースのルートからの相対パスを計算する。
pub(crate) fn make_path_relative_to_manifest_dir(path: &std::path::Path) -> std::path::PathBuf {
    use std::{
        io,
        path::{Component, Path, PathBuf},
    };

    fn segments(path: &Path) -> io::Result<Vec<String>> {
        Ok(path
            .canonicalize()?
            .components()
            .filter_map(|c| match c {
                Component::Normal(name) => Some(name.to_string_lossy().to_string()),
                _ => None,
            })
            .collect())
    }

    fn make_relative_path(dest_path: &Path, base_path: &Path) -> io::Result<PathBuf> {
        let dest = segments(dest_path)?;
        let base = segments(base_path)?;

        let common_prefix_len = dest
            .iter()
            .zip(base.iter())
            .take_while(|(dest_name, base_name)| dest_name == base_name)
            .count();

        let mut out = PathBuf::new();
        for _ in 0..base.len() - common_prefix_len {
            out.push("..".to_string());
        }
        for name in &dest[common_prefix_len..] {
            out.push(name);
        }
        Ok(out)
    }

    let manifest_dir: &str = env!("CARGO_MANIFEST_DIR");

    let mut base_dir = PathBuf::from(manifest_dir);
    base_dir.pop();

    make_relative_path(path, &base_dir).unwrap_or_default()
}

pub fn compile(source_path: &std::path::Path, source_code: &str) -> String {
    use crate::{
        clang::clang_dump,
        cps::{eliminate_unit, resolve_types, KEnumOutline},
        front::{cps_conversion, resolve_name, validate_syntax},
        logs::Logs,
        parse::parse_tokens,
        source::Doc,
        token::{tokenize, TokenSource},
    };
    use log::{error, trace};
    use std::{process, rc::Rc};

    trace!("source_path = {:?}", source_path);

    let logs = Logs::new();

    let doc = Doc::new(1);
    let source_path = make_path_relative_to_manifest_dir(source_path);
    Doc::set_path(doc, &source_path);

    let token_source = TokenSource::File(doc);
    let source_code = Rc::new(source_code.to_string());
    let tokens = tokenize(token_source, source_code);
    trace!("tokens = {:#?}\n", tokens);

    let mut p_root = parse_tokens(tokens, logs.logger());
    validate_syntax(&p_root, logs.logger());
    let name_resolution = resolve_name(&mut p_root, logs.logger());
    trace!("p_root = {:#?}\n", p_root);

    if logs.is_fatal() {
        for item in logs.finish() {
            error!("{:?} {}", item.location, item.message);
        }
        process::exit(1);
    }

    let mut k_root = cps_conversion(&p_root, &name_resolution, logs.logger());
    resolve_types(&mut k_root, logs.logger());
    trace!("k_root (gen) = {:#?}\n", k_root);

    eliminate_unit(&mut k_root);
    trace!("k_root (elim) = {:#?}\n", k_root);

    for item in logs.finish() {
        error!("{:?} {}", item.location, item.message);
    }

    KEnumOutline::determine_tags(
        &mut k_root.outlines.consts,
        &mut k_root.outlines.enums,
        &mut k_root.outlines.structs,
    );

    clang_dump(&k_root)
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
    mod k_command;
    mod k_const;
    mod k_enum;
    mod k_extern_fn;
    mod k_field;
    mod k_fn;
    mod k_label;
    mod k_local;
    mod k_meta_ty;
    mod k_mut;
    mod k_node;
    mod k_outlines;
    mod k_prim;
    mod k_root;
    mod k_static_var;
    mod k_struct;
    mod k_symbol;
    mod k_term;
    mod k_ty;
    mod k_ty_env;
    mod k_vis;
    mod type_resolution;

    pub(crate) use cps_fold::fold_block;
    pub(crate) use eliminate_unit::eliminate_unit;
    pub(crate) use k_command::KCommand;
    pub(crate) use k_const::{KConst, KConstData, KConstValue};
    pub(crate) use k_enum::{KEnum, KEnumOutline, KEnumRepr, KVariant};
    pub(crate) use k_extern_fn::{KExternFn, KExternFnData, KExternFnOutline};
    pub(crate) use k_field::{KField, KFieldOutline, KFieldTag};
    pub(crate) use k_fn::{KFn, KFnData, KFnOutline};
    pub(crate) use k_label::{KLabel, KLabelData, KLabelSig};
    pub(crate) use k_local::{KLocal, KLocalData};
    pub(crate) use k_meta_ty::{KMetaTy, KMetaTyData};
    pub(crate) use k_mut::KMut;
    pub(crate) use k_node::KNode;
    pub(crate) use k_outlines::KOutlines;
    pub(crate) use k_prim::KPrim;
    pub(crate) use k_root::KRoot;
    pub(crate) use k_static_var::{KStaticVar, KStaticVarData};
    pub(crate) use k_struct::{KStruct, KStructOutline, KStructParent};
    pub(crate) use k_symbol::{KSymbol, KSymbolExt};
    pub(crate) use k_term::KTerm;
    pub(crate) use k_ty::KTy;
    pub(crate) use k_ty_env::KTyEnv;
    pub(crate) use k_vis::KVis;
    pub(crate) use type_resolution::resolve_types;

    use crate::logs::Logger;
    use crate::token::{HaveLocation, Location, TokenData};
    use log::{error, trace};

    /// 符号付き？
    #[allow(unused)]
    #[derive(Copy, Clone)]
    pub(crate) enum KIsSigned {
        Signed,
        Unsigned,
    }

    /// ビット幅
    #[allow(unused)]
    #[derive(Copy, Clone)]
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
    #[derive(Copy, Clone)]
    pub(crate) struct KIntFlavor {
        is_signed: KIsSigned,
        bits: KIntBits,
    }

    #[allow(unused)]
    #[derive(Copy, Clone)]
    pub(crate) enum KFloatBits {
        B16,
        B32,
        B64,
        B128,
    }

    #[allow(unused)]
    #[derive(Copy, Clone)]
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
    pub(crate) use name_resolution::{resolve_name, NName, NameResolution};
    pub(crate) use occurrence_collection::{collect_occurrences, Occurrences};
    pub(crate) use syntax_validation::validate_syntax;

    use crate::logs::Logger;
    use crate::parse::*;
    use crate::token::{HaveLocation, Location};
}

mod lang_service {
    //! 入力支援機能 (LSP サーバーの内部実装)

    #![allow(unused)]

    pub(crate) mod lang_service;
}

mod parse {
    //! 構文木・構文解析

    mod p_binary_op;
    mod p_token;
    mod p_unary_op;
    mod p_vis;
    mod parse_context;
    mod parse_decl;
    mod parse_expr;
    mod parse_pat;
    mod parse_tree;
    mod parse_ty;

    pub(crate) use p_binary_op::PBinaryOp;
    pub(crate) use p_token::{PToken, PTokens};
    pub(crate) use p_unary_op::PUnaryOp;
    pub(crate) use p_vis::PVis;
    pub(crate) use parse_decl::parse_tokens;
    pub(crate) use parse_tree::*;

    use crate::logs::Logger;
    use crate::{
        cps::KMut,
        token::{HaveLocation, Location, TokenData, TokenKind},
    };
    use parse_context::Px;
    use parse_decl::parse_semi;
    use parse_expr::{parse_block, parse_expr, parse_name};
    use parse_ty::{parse_ty, parse_ty_ascription};

    pub(crate) type PMut = (KMut, PToken);

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
    pub(crate) mod pos;
    pub(crate) mod range;
    pub(crate) mod t_pos;
    pub(crate) mod t_range;

    pub(crate) use doc::Doc;
    pub(crate) use pos::Pos;
    pub(crate) use range::Range;

    #[allow(unused)]
    pub(crate) use t_pos::TPos;
    #[allow(unused)]
    pub(crate) use t_range::TRange;

    pub(crate) type SourceCode = String;
}

mod token {
    //! 字句・字句解析

    mod keyword;
    mod location;
    mod punctuation;
    mod token_data;
    mod token_kind;
    mod token_source;
    mod tokenize_context;
    mod tokenize_rules;

    pub(crate) use keyword::Keyword;
    pub(crate) use location::{HaveLocation, Location};
    pub(crate) use token_data::TokenData;
    pub(crate) use token_kind::TokenKind;
    pub(crate) use token_source::TokenSource;
    pub(crate) use tokenize_rules::tokenize;

    use crate::source::{Pos, Range, SourceCode};
}

mod utils {
    mod id_provider;
    mod take_out;

    pub(crate) use id_provider::IdProvider;
    pub(crate) use take_out::TakeOut;
}
