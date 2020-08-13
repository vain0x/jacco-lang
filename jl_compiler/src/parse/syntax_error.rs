//! 構文エラーについて書く場所

use super::*;

// -----------------------------------------------
// 型
// -----------------------------------------------

pub(crate) fn validate_ptr_ty(
    star: PToken,
    _mut_opt: Option<PMut>,
    ty_opt: Option<&AfterTy>,
    px: &Px,
) {
    if ty_opt.is_none() {
        px.logger().error(
            PLoc::TokenBehind(star),
            "ポインタ型の * の後ろに型が必要です。",
        );
    }
}
