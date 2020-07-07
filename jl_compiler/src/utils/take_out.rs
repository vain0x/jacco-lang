use std::mem::take;

/// 可変参照からオブジェクトを取り出す。
///
/// `std::mem::take` と同様、オブジェクトからデータを効率よく抜き去るのに使う。
/// ただし、take_out されたオブジェクトが `default` になるとは限らない。
///
/// 例えば、オブジェクトを take_out されたことを示す特殊な状態にする、という実装がありうる。
/// あるいは、オブジェクトのデータのコピーが軽量な場合、単に clone で実装される可能性もある。
pub(crate) trait TakeOut<Output = Self> {
    fn take_out(&mut self) -> Output;
}

impl TakeOut for String {
    fn take_out(&mut self) -> Self {
        take(self)
    }
}

impl<T> TakeOut for Option<T> {
    fn take_out(&mut self) -> Self {
        take(self)
    }
}

impl<T> TakeOut for Vec<T> {
    fn take_out(&mut self) -> Self {
        take(self)
    }
}
