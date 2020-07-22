pub(crate) struct PackageInfo {
    pub(crate) name: &'static str,
    pub(crate) version: &'static str,
}

impl PackageInfo {
    pub(crate) fn from_env() -> Self {
        Self {
            name: env!("CARGO_PKG_NAME"),
            version: env!("CARGO_PKG_VERSION"),
        }
    }
}
