//! ソースファイル

mod position;
mod range;
mod source_file;

pub(crate) use position::Position;
pub(crate) use range::Range;
pub(crate) use source_file::SourceFile;

pub(crate) type SourceCode = String;
