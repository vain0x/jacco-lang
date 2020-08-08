use super::{Doc, LangService, TRange};

pub(crate) fn validate(doc: Doc, ls: &mut LangService) -> (Option<i64>, Vec<(TRange, String)>) {
    ls.docs
        .get_mut(&doc)
        .map(|analysis| {
            let version_opt = Some(analysis.version());

            let mut errors = analysis.request_syntax().errors.clone();
            if errors.is_empty() {
                errors.extend(analysis.request_symbols().symbols.errors.clone());
            }
            // 頻繁にクラッシュするので無効化
            // if errors.is_empty() {
            //     errors.extend(analysis.request_cps().errors.clone());
            // }

            (version_opt, errors)
        })
        .unwrap_or((None, vec![]))
}
