use std::{cell::RefCell, collections::HashMap, rc::Rc};
use thiserror::Error;

use crate::{
    ast::{BinaryExpr, Expr, LiteralExpr, Stmt, UnaryExpr},
    scanner::TokenKind,
};

#[derive(Clone, Debug)]
pub enum RoxValue {
    String(String),
    Number(f64),
    Nil,
    Boolean(bool),
}

impl RoxValue {
    fn to_rox_number(self: RoxValue) -> Result<f64, RuntimeError> {
        if let RoxValue::Number(n) = self {
            Ok(n)
        } else {
            Err(RuntimeError {
                kind: RuntimeErrorKind::TypeError(format!("Expected number got {:?}", self)),
            })
        }
    }

    fn to_rox_string(self) -> Result<String, RuntimeError> {
        if let RoxValue::String(s) = self {
            Ok(s)
        } else {
            Err(RuntimeError {
                kind: RuntimeErrorKind::TypeError(format!("Expected string got {:?}", self)),
            })
        }
    }

    fn is_truthy(self: RoxValue) -> bool {
        match self {
            RoxValue::Nil => false,
            RoxValue::Boolean(false) => false,
            _ => true,
        }
    }
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

#[derive(Error, Debug)]
#[error("Runtime Exception: {kind}")]
pub struct RuntimeError {
    kind: RuntimeErrorKind,
}

#[derive(Error, Debug)]
pub enum RuntimeErrorKind {
    #[error("Type error {0}")]
    TypeError(String),
    #[error("variable {0} is not defined")]
    UndefinedVariable(String),
}

type EvaluationResult = Result<RoxValue, RuntimeError>;

pub struct Environment {
    variables: HashMap<String, RoxValue>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new(enclosing: Option<Rc<RefCell<Environment>>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Environment {
            variables: HashMap::new(),
            enclosing,
        }))
    }

    fn declare(&mut self, name: String, value: RoxValue) {
        self.variables.insert(name, value);
    }

    fn get(&self, name: String) -> EvaluationResult {
        let mut variable = self
            .variables
            .get(&name)
            .map(|t| t.clone())
            .ok_or(RuntimeError {
                kind: RuntimeErrorKind::UndefinedVariable(name.clone()),
            });
        if variable.is_err() && self.enclosing.is_some() {
            variable = self.enclosing.as_ref().unwrap().borrow().get(name);
        }

        variable
    }

    fn assign(&mut self, name: String, value: RoxValue) -> Result<(), RuntimeError> {
        if self.variables.contains_key(&name) {
            self.variables.insert(name, value.clone());
            Ok(())
        } else if let Some(enclosing) = self.enclosing.as_ref() {
            enclosing.borrow_mut().assign(name, value)
        } else {
            Err(RuntimeError {
                kind: RuntimeErrorKind::UndefinedVariable(name.clone()),
            })
        }
    }
}

pub trait Evaluate<T = RoxValue> {
    fn evaluate<'a, 'b>(self, env: Rc<RefCell<Environment>>) -> Result<T, RuntimeError>;
}

impl Evaluate<()> for Stmt {
    fn evaluate<'a, 'b>(self, env: Rc<RefCell<Environment>>) -> Result<(), RuntimeError> {
        match self {
            Stmt::Block(statements) => {
                let new_env = Environment::new(Some(env.clone()));

                for statement in statements {
                    statement.evaluate(new_env.clone())?
                }
                Ok(())
            }
            Stmt::Print(expr) => expr.evaluate(env).map(|c| println!("{c}")),
            Stmt::ExprStmt(expr) => expr.evaluate(env).map(|_| ()),
            Stmt::VarDecl { name, init } => {
                let init = if let Some(expr) = init {
                    expr.evaluate(env.clone())?
                } else {
                    RoxValue::Nil
                };
                env.borrow_mut().declare(name, init);
                Ok(())
            }
        }
    }
}

impl Evaluate for Expr {
    fn evaluate(self, env: Rc<RefCell<Environment>>) -> EvaluationResult {
        match self {
            Expr::Literal(l) => l.evaluate(env),
            Expr::Unary(l) => l.evaluate(env),
            Expr::Binary(l) => l.evaluate(env),
            Expr::Grouping(l) => l.evaluate(env),
            Expr::Assign { name, value } => {
                let value = value.evaluate(env.clone())?;
                env.borrow_mut().assign(name, value.clone())?;
                return Ok(value);
            }
        }
    }
}

impl Evaluate for LiteralExpr {
    fn evaluate(self, env: Rc<RefCell<Environment>>) -> EvaluationResult {
        Ok(match self {
            LiteralExpr::Boolean(n) => RoxValue::Boolean(n),
            LiteralExpr::Number(n) => RoxValue::Number(n),
            LiteralExpr::String(n) => RoxValue::String(n),
            LiteralExpr::Nil => RoxValue::Nil,
            LiteralExpr::Variable(name) => env.borrow().get(name)?,
        })
    }
}

impl Evaluate for UnaryExpr {
    fn evaluate(self, env: Rc<RefCell<Environment>>) -> EvaluationResult {
        let right = self.right.evaluate(env)?;
        match self.operator.kind {
            TokenKind::Minus => right.to_rox_number().map(|n| RoxValue::Number(-n)),
            TokenKind::Bang => Ok(RoxValue::Boolean(!right.is_truthy())),
            _ => unreachable!(),
        }
    }
}
impl Evaluate for BinaryExpr {
    fn evaluate(self, env: Rc<RefCell<Environment>>) -> EvaluationResult {
        let left = self.left.evaluate(env.clone())?;
        let right = self.right.evaluate(env.clone())?;

        match self.operator.kind {
            TokenKind::Plus => {
                if let Ok(left_number) = left.clone().to_rox_number() {
                    if let Ok(right_number) = right.to_rox_number() {
                        Ok(RoxValue::Number(left_number + right_number))
                    } else {
                        Err(RuntimeError {
                            kind: RuntimeErrorKind::TypeError(
                                "Both operands of plus must be string or number".into(),
                            ),
                        })
                    }
                } else if let Ok(mut left_string) = left.to_rox_string() {
                    if let Ok(right_string) = right.to_rox_string() {
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
                let left_number = left.to_rox_number()?;
                let right_number = right.to_rox_number()?;
                Ok(RoxValue::Number(left_number - right_number))
            }

            TokenKind::Star => {
                let left_number = left.to_rox_number()?;
                let right_number = right.to_rox_number()?;
                Ok(RoxValue::Number(left_number * right_number))
            }
            TokenKind::Slash => {
                let left_number = left.to_rox_number()?;
                let right_number = right.to_rox_number()?;
                Ok(RoxValue::Number(left_number / right_number))
            }
            _ => unreachable!(),
        }
    }
}
