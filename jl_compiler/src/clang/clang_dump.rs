//! C言語の構文木を文字列に変換する処理

use super::*;
use c_stmt::CStorageModifier;
use std::io::{self, Write};

/// Dump context.
pub(crate) struct Dx<W> {
    w: W,
    indent: usize,
}

impl<W: Write> Dx<W> {
    pub(crate) fn new(w: W) -> Self {
        Self { w, indent: 0 }
    }

    pub(crate) fn finish(self) -> W {
        self.w
    }

    pub(crate) fn do_with_indent(
        &mut self,
        write_fn: impl FnOnce(&mut Self) -> io::Result<()>,
    ) -> io::Result<()> {
        self.indent += 1;
        write_fn(self)?;
        self.indent -= 1;
        Ok(())
    }

    pub(crate) fn do_with_unindent(
        &mut self,
        write_fn: impl FnOnce(&mut Self) -> io::Result<()>,
    ) -> io::Result<()> {
        let n = self.indent.min(1);
        self.indent -= n;
        write_fn(self)?;
        self.indent += n;
        Ok(())
    }
}

impl<W: Write> Write for Dx<W> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.w.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.w.flush()
    }
}

fn write_indent(dx: &mut Dx<impl Write>) -> io::Result<()> {
    for _ in 0..dx.indent {
        write!(dx, "    ")?;
    }
    Ok(())
}

fn write_ty(ty: &CTy, dx: &mut Dx<impl Write>) -> io::Result<()> {
    match ty {
        CTy::Other(text) => write!(dx, "{}", text),
        CTy::Void => write!(dx, "void"),
        CTy::Int => write!(dx, "int"),
        CTy::LongLong => write!(dx, "long long"),
        CTy::UnsignedLongLong => write!(dx, "unsigned long long"),
        CTy::UnsignedChar => write!(dx, "unsigned char"),
        CTy::Double => write!(dx, "double"),
        CTy::Ptr { ty } => {
            write_ty(&ty, dx)?;
            write!(dx, "*")
        }
        CTy::Struct(name) => write!(dx, "struct {}", name),
    }
}

/// 変数と型を書き込む。(`int foo` や `void (*f)()` など。)
fn write_var_with_ty(name: &str, ty: &CTy, dx: &mut Dx<impl Write>) -> io::Result<()> {
    write_ty(ty, dx)?;
    write!(dx, " {}", name)
}

fn write_param_list(params: &[(String, CTy)], dx: &mut Dx<impl Write>) -> io::Result<()> {
    write!(dx, "(")?;

    for (i, (name, ty)) in params.iter().enumerate() {
        if i != 0 {
            write!(dx, ", ")?;
        }

        write_var_with_ty(name, ty, dx)?;
    }

    write!(dx, ")")
}

fn write_expr(expr: &CExpr, dx: &mut Dx<impl Write>) -> io::Result<()> {
    match expr {
        CExpr::IntLit(value) => write!(dx, "{}", value),
        CExpr::LongLongLit(value) => write!(dx, "{}LL", value),
        CExpr::UnsignedLongLongLit(value) => write!(dx, "{}ULL", value),
        CExpr::DoubleLit(value) => write!(dx, "{}", value),
        CExpr::CharLit(value) => write!(dx, "{}", value),
        CExpr::StrLit(value) => write!(dx, "{}", value),
        CExpr::Name(name) => write!(dx, "{}", name),
        CExpr::Dot { left, field } => {
            write_expr(left, dx)?;
            write!(dx, ".{}", field)
        }
        CExpr::Arrow { left, field } => {
            write_expr(left, dx)?;
            write!(dx, "->{}", field)
        }
        CExpr::Call { left, args } => {
            write_expr(left, dx)?;
            write!(dx, "(")?;

            for (i, arg) in args.iter().enumerate() {
                if i != 0 {
                    write!(dx, ", ")?;
                }

                write_expr(arg, dx)?;
            }

            write!(dx, ")")
        }
        CExpr::Cast { ty, arg } => {
            write!(dx, "(")?;
            write_ty(ty, dx)?;
            write!(dx, ")")?;
            write_expr(arg, dx)
        }
        CExpr::UnaryOp { op, arg } => {
            write!(dx, "{}", op.as_str())?;
            write_expr(arg, dx)
        }
        CExpr::BinaryOp { op, left, right } => {
            write_expr(left, dx)?;
            write!(dx, " {} ", op.as_str())?;
            write_expr(right, dx)
        }
    }
}

fn write_stmt(stmt: &CStmt, dx: &mut Dx<impl Write>) -> io::Result<()> {
    match stmt {
        CStmt::Comment(text) => {
            for line in text.split_terminator('\n') {
                write!(dx, "// {}", line)?;
            }
            Ok(())
        }
        CStmt::Expr(body) => {
            write_expr(body, dx)?;
            write!(dx, ";")
        }
        CStmt::Block(body) => write_block(body, dx),
        CStmt::Goto { label } => write!(dx, "goto {};", label),
        CStmt::Return(None) => write!(dx, "return;"),
        CStmt::Return(Some(arg)) => {
            write!(dx, "return ")?;
            write_expr(arg, dx)?;
            write!(dx, ";")
        }
        CStmt::If { cond, body, alt } => {
            write!(dx, "if (")?;
            write_expr(cond, dx)?;
            write!(dx, ") ")?;
            write_stmt(body, dx)?;
            write!(dx, " else ")?;
            write_stmt(alt, dx)
        }
        CStmt::VarDecl {
            storage_modifier_opt,
            name,
            ty,
            init_opt,
        } => {
            if let &Some(CStorageModifier::Static) = storage_modifier_opt {
                write!(dx, "static ")?;
            }

            write_var_with_ty(name, ty, dx)?;

            if let Some(init) = init_opt {
                write!(dx, " = ")?;
                write_expr(init, dx)?;
            }

            write!(dx, ";")
        }
        CStmt::FnDecl {
            name,
            params,
            result_ty,
            body_opt,
        } => {
            write_ty(result_ty, dx)?;
            write!(dx, " {}", name)?;
            write_param_list(&params, dx)?;

            match body_opt {
                Some(body) => {
                    write!(dx, " ")?;
                    write_block(body, dx)
                }
                None => write!(dx, ";"),
            }
        }
        CStmt::ExternFnDecl {
            name,
            params,
            result_ty,
        } => {
            write_ty(result_ty, dx)?;
            write!(dx, " {}", name)?;
            write_param_list(&params, dx)?;
            write!(dx, ";")
        }
        CStmt::StructDecl { name, fields } => {
            write!(dx, "struct {} {{\n", name)?;

            for (name, ty) in fields {
                dx.do_with_indent(|dx| {
                    write_indent(dx)?;
                    write_var_with_ty(&name, &ty, dx)?;
                    write!(dx, ";")?;
                    write!(dx, "\n")
                })?;
            }

            write_indent(dx)?;
            write!(dx, "}};")
        }
        CStmt::Label { .. } => unreachable!(),
    }
}

fn write_stmt_with_indent(stmt: &CStmt, dx: &mut Dx<impl Write>) -> io::Result<()> {
    if let CStmt::Label { label } = stmt {
        return dx.do_with_unindent(|dx| {
            write!(dx, "\n")?;
            write_indent(dx)?;
            write!(dx, "{}:;", label)
        });
    }

    write_indent(dx)?;
    write_stmt(stmt, dx)
}

fn write_block(block: &CBlock, dx: &mut Dx<impl Write>) -> io::Result<()> {
    let stmts = &block.stmts;

    if stmts.is_empty() {
        write!(dx, "{{}}")
    } else {
        write!(dx, "{{\n")?;

        for stmt in stmts {
            dx.do_with_indent(|dx| write_stmt_with_indent(stmt, dx))?;
            write!(dx, "\n")?;
        }

        write_indent(dx)?;
        write!(dx, "}}")
    }
}

fn write_root(root: &CRoot, dx: &mut Dx<impl Write>) -> io::Result<()> {
    for (i, decl) in root.decls.iter().enumerate() {
        if i != 0 {
            write!(dx, "\n")?;
        }

        write_stmt_with_indent(decl, dx)?;
        write!(dx, "\n")?;
    }
    Ok(())
}

pub(crate) fn clang_dump(k_root: KRoot) -> String {
    let c_root = clang_gen::gen(k_root);

    let text = {
        let out = {
            let mut dx = Dx::new(vec![]);
            super::clang_dump::write_root(&c_root, &mut dx).unwrap();
            dx.finish()
        };

        String::from_utf8_lossy(&out).to_string()
    };

    text
}
