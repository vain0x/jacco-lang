use crate::util::package_info::PackageInfo;

pub(crate) fn exec_version_cmd() {
    println!("{}", PackageInfo::from_env().version);
}
