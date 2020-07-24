use std::{env::current_dir, path::Path};
use url::Url;

/// 正規化された URI
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Uri {
    inner: Url,
}

impl Uri {
    #[allow(unused)]
    pub fn from_file_path(path: &Path) -> Option<Self> {
        let to_uri = |path: &Path| Url::from_file_path(path).ok().map(|url| Uri { inner: url });

        // canonicalize に成功するなら、これを正規形と信じて使う。
        if let Ok(canonical_path) = path.canonicalize() {
            return to_uri(&canonical_path);
        }

        // 絶対パスでなければ、カレントディレクトリと繋ぐ。
        // 念のため再び canonicalize を試してから、絶対パスを URI にする。
        if !path.is_absolute() {
            if let Some(path) = current_dir().ok().map(|current_dir| current_dir.join(path)) {
                if let Ok(canonical_path) = path.canonicalize() {
                    return to_uri(&canonical_path);
                }

                return to_uri(&path);
            }
        }

        // canonicalize できない絶対パスというのはよく分からないが、
        // 例えばファイルパスを取得した直後にファイルが削除された場合などに発生しうる。
        // (存在しないファイルパスは canonicalize に失敗するはず。)
        // 正規形ではないかもしれないが、そのまま URI として使う。
        to_uri(path)
    }

    pub fn from_url(url: &Url) -> Self {
        let url = match url
            .to_file_path()
            .ok()
            .and_then(|file_path| file_path.canonicalize().ok())
            .and_then(|canonical_path| Url::from_file_path(canonical_path).ok())
        {
            Some(url) => url,
            None => url.to_owned(),
        };
        Uri { inner: url }
    }

    pub fn into_url(self) -> Url {
        self.inner
    }
}
