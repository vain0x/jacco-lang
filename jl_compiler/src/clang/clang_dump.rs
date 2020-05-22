//! C言語の構文木を文字列に変換する処理

use super::*;
use std::io::{self, Write};

impl CUnaryOp {
    fn as_str(&self) -> &'static str {
        match self {
            CUnaryOp::Deref => "*",
            CUnaryOp::Ref => "&",
            CUnaryOp::Minus => "-",
            CUnaryOp::Not => "!",
        }
    }
}

impl CBinaryOp {
    fn as_str(&self) -> &'static str {
        match self {
            CBinaryOp::Assign => "=",
            CBinaryOp::Add => "+",
            CBinaryOp::Sub => "-",
            CBinaryOp::Mul => "*",
            CBinaryOp::Div => "/",
            CBinaryOp::Modulo => "%",
            CBinaryOp::BitAnd => "&",
            CBinaryOp::BitOr => "|",
            CBinaryOp::BitXor => "^",
            CBinaryOp::LeftShift => "<<",
            CBinaryOp::RightShift => ">>",
            CBinaryOp::Eq => "==",
            CBinaryOp::Ne => "!=",
            CBinaryOp::Lt => "<",
            CBinaryOp::Le => "<=",
            CBinaryOp::Gt => ">",
            CBinaryOp::Ge => ">=",
        }
    }
}

fn write_indent(indent: usize, out: &mut Vec<u8>) -> io::Result<()> {
    for _ in 0..indent {
        write!(out, "    ")?;
    }
    Ok(())
}

fn write_ty(ty: &CTy, indent: usize, out: &mut Vec<u8>) -> io::Result<()> {
    match ty {
        CTy::Other(text) => write!(out, "{}", text),
        CTy::Void => write!(out, "void"),
        CTy::Int => write!(out, "int"),
        CTy::Ptr { ty } => {
            write_ty(&ty, indent, out)?;
            write!(out, "*")
        }
        CTy::Struct(name) => write!(out, "struct {}", name),
    }
}

/// 変数と型を書き込む。(`int foo` や `void (*f)()` など。)
fn write_var_with_ty(name: &str, ty: &CTy, indent: usize, out: &mut Vec<u8>) -> io::Result<()> {
    write_ty(ty, indent, out)?;
    write!(out, " {}", name)
}

fn write_param_list(params: &[(String, CTy)], indent: usize, out: &mut Vec<u8>) -> io::Result<()> {
    write!(out, "(")?;

    for (i, (name, ty)) in params.iter().enumerate() {
        if i != 0 {
            write!(out, ", ")?;
        }

        write_var_with_ty(name, ty, indent, out)?;
    }

    write!(out, ")")
}

fn write_expr(expr: &CExpr, indent: usize, out: &mut Vec<u8>) -> io::Result<()> {
    match expr {
        CExpr::IntLit(value) => write!(out, "{}", value),
        CExpr::Name(name) => write!(out, "{}", name),
        CExpr::Dot { left, field } => {
            write_expr(left, indent, out)?;
            write!(out, ".{}", field)
        }
        CExpr::Arrow { left, field } => {
            write_expr(left, indent, out)?;
            write!(out, "->{}", field)
        }
        CExpr::Call { left, args } => {
            write_expr(left, indent, out)?;
            write!(out, "(")?;

            for (i, arg) in args.iter().enumerate() {
                if i != 0 {
                    write!(out, ", ")?;
                }

                write_expr(arg, indent, out)?;
            }

            write!(out, ")")
        }
        CExpr::UnaryOp { op, arg } => {
            write!(out, "{}", op.as_str())?;
            write_expr(arg, indent, out)
        }
        CExpr::BinaryOp { op, left, right } => {
            write_expr(left, indent, out)?;
            write!(out, " {} ", op.as_str())?;
            write_expr(right, indent, out)
        }
    }
}

fn write_stmt(stmt: &CStmt, indent: usize, out: &mut Vec<u8>) -> io::Result<()> {
    match stmt {
        CStmt::Expr(body) => {
            write_expr(body, indent, out)?;
            write!(out, ";")
        }
        CStmt::Block(body) => write_block(body, indent, out),
        CStmt::Goto { label } => write!(out, "goto {};", label),
        CStmt::Return(None) => write!(out, "return;"),
        CStmt::Return(Some(arg)) => {
            write!(out, "return ")?;
            write_expr(arg, indent, out)?;
            write!(out, ";")
        }
        CStmt::If { cond, body, alt } => {
            write!(out, "if (")?;
            write_expr(cond, indent, out)?;
            write!(out, ") ")?;
            write_stmt(body, indent, out)?;
            write!(out, " else ")?;
            write_stmt(alt, indent, out)
        }
        CStmt::VarDecl { name, ty, init_opt } => {
            write_var_with_ty(name, ty, indent, out)?;

            if let Some(init) = init_opt {
                write!(out, " = ")?;
                write_expr(init, indent, out)?;
            }

            write!(out, ";")
        }
        CStmt::FnDecl {
            name,
            params,
            result_ty,
            body,
        } => {
            write_ty(result_ty, indent, out)?;
            write!(out, " {}", name)?;
            write_param_list(&params, indent, out)?;
            write!(out, " ")?;
            write_block(body, indent, out)
        }
        CStmt::ExternFnDecl {
            name,
            params,
            result_ty,
        } => {
            write_ty(result_ty, indent, out)?;
            write!(out, " {}", name)?;
            write_param_list(&params, indent, out)?;
            write!(out, ";")
        }
        CStmt::StructDecl { name, fields } => {
            write!(out, "struct {} {{\n", name)?;

            for (name, ty) in fields {
                let indent = indent + 1;
                write_indent(indent, out)?;
                write_var_with_ty(&name, &ty, indent, out)?;
                write!(out, ";")?;
                write!(out, "\n")?;
            }

            write_indent(indent, out)?;
            write!(out, "}};")
        }
        CStmt::Label { .. } => unreachable!(),
    }
}

fn write_stmt_with_indent(stmt: &CStmt, indent: usize, out: &mut Vec<u8>) -> io::Result<()> {
    if let CStmt::Label { label } = stmt {
        write!(out, "\n")?;
        write_indent(indent.saturating_sub(1), out)?;
        return write!(out, "{}:;", label);
    }

    write_indent(indent, out)?;
    write_stmt(stmt, indent, out)
}

fn write_block(block: &CBlock, indent: usize, out: &mut Vec<u8>) -> io::Result<()> {
    let body = &block.body;

    if body.is_empty() {
        write!(out, "{{}}")
    } else {
        write!(out, "{{\n")?;

        for stmt in body {
            write_stmt_with_indent(stmt, indent + 1, out)?;
            write!(out, "\n")?;
        }

        write_indent(indent, out)?;
        write!(out, "}}")
    }
}

fn write_root(root: &CRoot, indent: usize, out: &mut Vec<u8>) -> io::Result<()> {
    for (i, decl) in root.body.iter().enumerate() {
        if i != 0 {
            write!(out, "\n")?;
        }

        write_stmt_with_indent(decl, indent, out)?;
        write!(out, "\n")?;
    }
    Ok(())
}

pub(crate) fn clang_dump(k_root: KRoot) -> String {
    let c_root = clang_gen::gen(k_root);

    let text = {
        let mut out = vec![];
        super::clang_dump::write_root(&c_root, 0, &mut out).unwrap();
        String::from_utf8_lossy(&out).to_string()
    };

    text
}
