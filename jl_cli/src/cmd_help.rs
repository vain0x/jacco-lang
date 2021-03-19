use crate::util::package_info::PackageInfo;

pub(crate) fn exec_help_cmd() {
    let package_info = PackageInfo::from_env();
    print!(
        include_str!("cmd_help.txt"),
        command = package_info.name,
        version = package_info.version,
    );
}
