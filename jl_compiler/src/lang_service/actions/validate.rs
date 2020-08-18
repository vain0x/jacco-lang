use super::{Doc, LangService, TRange};

pub(crate) fn validate(doc: Doc, ls: &mut LangService) -> (Option<i64>, Vec<(TRange, String)>) {
    let (version_opt, mut errors) = ls
        .docs
        .get_mut(&doc)
        .map(|analysis| {
            let version_opt = Some(analysis.version());

            let mut errors = analysis.request_syntax().errors.clone();
            if errors.is_empty() {
                errors.extend(analysis.request_symbols().symbols.errors.clone());
            }

            (version_opt, errors)
        })
        .unwrap_or((None, vec![]));

    // CPS 変換のエラーを報告する。
    if errors.is_empty() {
        if let Some(analysis) = ls.request_cps(doc) {
            errors.extend(analysis.cps.errors.clone());
        }
    }

    // 型エラーを報告する。
    if errors.is_empty() {
        let logs = ls.request_types().to_owned();
        if let Some(syntax) = ls.request_syntax(doc) {
            for item in logs {
                let loc = match item.loc().inner() {
                    Ok((the_doc, _)) if the_doc != doc => continue,
                    Ok((_, loc)) => loc,
                    Err(_) => continue,
                };

                let range = match loc.range(&syntax.tree) {
                    Ok(it) => it,
                    Err(_) => continue,
                };
                let message = item.message().to_string();

                errors.push((range, message));
            }
        }
    }

    (version_opt, errors)
}
