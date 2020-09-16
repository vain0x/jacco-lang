use super::{Doc, LangService, TRange};

pub(crate) fn validate(doc: Doc, ls: &mut LangService) -> (Option<i64>, Vec<(TRange, String)>) {
    // let (version_opt, mut errors) = ls
    //     .docs
    //     .get_mut(&doc)
    //     .map(|analysis| {
    //         let version_opt = Some(analysis.version());

    //         let mut errors = analysis.request_syntax().errors.clone();
    //         if errors.is_empty() {
    //             errors.extend(
    //                 analysis
    //                     .request_symbols(&mut ls.mod_outlines)
    //                     .symbols
    //                     .errors
    //                     .clone(),
    //             );
    //         }

    //         (version_opt, errors)
    //     })
    //     .unwrap_or((None, vec![]));

    // // CPS 変換のエラーを報告する。
    // if errors.is_empty() {
    //     if let Some(analysis) = ls.request_cps(doc) {
    //         errors.extend(analysis.cps.errors.clone());
    //     }
    // }

    // // 型エラーを報告する。
    // if errors.is_empty() {
    //     if let Some(analysis) = ls.request_types_for(doc) {
    //         errors.extend_from_slice(&analysis.cps.errors);
    //     }
    // }

    // (version_opt, errors)
    (None, vec![])
}
