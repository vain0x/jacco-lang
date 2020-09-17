use super::{Doc, LangService, TRange};
use crate::lang_service::doc_analysis::DocSymbolAnalysisMut;

pub(crate) fn validate(doc: Doc, ls: &mut LangService) -> (Option<i64>, Vec<(TRange, String)>) {
    let version_opt = ls.docs.get(&doc).map(|analysis| analysis.version());

    // 構文解析などを行ってエラーを収集する。エラーが発生したフェイズで止める。
    let mut errors = vec![];
    if let Some(syntax) = ls.request_syntax(doc) {
        errors.extend(syntax.errors.clone());
    }

    if errors.is_empty() {
        if let Some(analysis) = ls.request_symbols(doc) {
            errors.extend_from_slice(&analysis.symbols.errors);
        }
    }

    if errors.is_empty() {
        if let Some(analysis) = ls.request_cps(doc) {
            errors.extend_from_slice(&analysis.cps.errors);
        }
    }

    if errors.is_empty() {
        if let Some(analysis) = ls.request_types_for(doc) {
            errors.extend_from_slice(&analysis.cps.errors);
        }
    }

    (version_opt, errors)
}
