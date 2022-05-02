use crate::{
    ast::{BinaryExpr, Expr, LiteralExpression, Stmt, UnaryExpr},
    scanner::TokenKind,
};

#[derive(Clone, Debug)]
pub enum RoxValue {
    String(String),
    Number(f64),
    Nil,
    Boolean(bool),
}

impl std::fmt::Display for RoxValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            RoxValue::String(s) => write!(f, "{s}"),
            RoxValue::Number(s) => write!(f, "{s}"),
            RoxValue::Boolean(s) => write!(f, "{s}"),
            RoxValue::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug)]
pub struct RuntimeError {
    kind: RuntimeErrorKind,
}
#[derive(Debug)]
pub enum RuntimeErrorKind {
    TypeError(String),
}

type EvaluationResult = Result<RoxValue, RuntimeError>;

pub fn evaluate_stmt(stmt: Stmt) -> Result<(), RuntimeError> {
    match stmt {
        Stmt::Print(expr) => evaluate_expr(expr).map(|c| println!("{c}")),
        Stmt::ExprStmt(expr) => evaluate_expr(expr).map(|_| ()),
    }
}

pub fn evaluate_expr(expr: Expr) -> EvaluationResult {
    match expr {
        Expr::Literal(l) => Ok(evaluate_literal(l)),
        Expr::Unary(l) => evalutate_unary(*l),
        Expr::Binary(l) => evaluate_binary(*l),
        Expr::Grouping(l) => evaluate_expr(*l),
    }
}

fn evaluate_literal(expr: LiteralExpression) -> RoxValue {
    match expr {
        LiteralExpression::Boolean(n) => RoxValue::Boolean(n),
        LiteralExpression::Number(n) => RoxValue::Number(n),
        LiteralExpression::String(n) => RoxValue::String(n),
        LiteralExpression::Nil => RoxValue::Nil,
    }
}

fn evalutate_unary(expr: UnaryExpr) -> EvaluationResult {
    let right = evaluate_expr(expr.right)?;
    match expr.operator.kind {
        TokenKind::Minus => convert_to_number(right).map(|n| RoxValue::Number(-n)),
        TokenKind::Bang => Ok(RoxValue::Boolean(!is_truthy(right))),
        _ => unimplemented!(),
    }
}

fn evaluate_binary(expr: BinaryExpr) -> EvaluationResult {
    let left = evaluate_expr(expr.left)?;
    let right = evaluate_expr(expr.right)?;

    match expr.operator.kind {
        TokenKind::Plus => {
            if let Ok(left_number) = convert_to_number(left.clone()) {
                if let Ok(right_number) = convert_to_number(right) {
                    Ok(RoxValue::Number(left_number + right_number))
                } else {
                    Err(RuntimeError {
                        kind: RuntimeErrorKind::TypeError(
                            "Both operands of plus must be string or number".into(),
                        ),
                    })
                }
            } else if let Ok(mut left_string) = convert_to_string(left) {
                if let Ok(right_string) = convert_to_string(right) {
                    left_string.push_str(&right_string);
                    Ok(RoxValue::String(left_string))
                } else {
                    Err(RuntimeError {
                        kind: RuntimeErrorKind::TypeError(
                            "Both operands of plus must be string or number".into(),
                        ),
                    })
                }
            } else {
                Err(RuntimeError {
                    kind: RuntimeErrorKind::TypeError(
                        "Both operands of plus must be string or number".into(),
                    ),
                })
            }
        }
        TokenKind::Minus => {
            let left_number = convert_to_number(left)?;
            let right_number = convert_to_number(right)?;
            Ok(RoxValue::Number(left_number - right_number))
        }

        TokenKind::Star => {
            let left_number = convert_to_number(left)?;
            let right_number = convert_to_number(right)?;
            Ok(RoxValue::Number(left_number * right_number))
        }
        TokenKind::Slash => {
            let left_number = convert_to_number(left)?;
            let right_number = convert_to_number(right)?;
            Ok(RoxValue::Number(left_number / right_number))
        }
        _ => unimplemented!(),
    }
}

fn convert_to_number(eval_expr: RoxValue) -> Result<f64, RuntimeError> {
    if let RoxValue::Number(n) = eval_expr {
        Ok(n)
    } else {
        Err(RuntimeError {
            kind: RuntimeErrorKind::TypeError(format!("Expected number got {:?}", eval_expr)),
        })
    }
}

fn convert_to_string(eval_expr: RoxValue) -> Result<String, RuntimeError> {
    if let RoxValue::String(s) = eval_expr {
        Ok(s)
    } else {
        Err(RuntimeError {
            kind: RuntimeErrorKind::TypeError(format!("Expected string got {:?}", eval_expr)),
        })
    }
}

fn is_truthy(eval_expr: RoxValue) -> bool {
    match eval_expr {
        RoxValue::Nil => false,
        RoxValue::Boolean(false) => false,
        _ => true,
    }
}
