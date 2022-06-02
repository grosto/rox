use std::{cell::RefCell, collections::HashMap, rc::Rc};
use thiserror::Error;

use crate::{
    ast::{BinaryExpr, Expr, LiteralExpr, LogicalExpr, Stmt, UnaryExpr},
    globals::{CLOCK_NATIVE_FN, READ_FILE_NATIVE_FN},
    scanner::{Token, TokenKind},
};

pub type WrappedEnvironment = Rc<RefCell<Environment>>;

#[derive(Clone, Debug)]
pub enum RoxValue {
    String(String),
    Number(f64),
    Nil,
    Boolean(bool),
    Function(FnValue),
    NativeFn(NativeFnValue),
}

#[derive(Clone, Debug)]
pub struct NativeFnValue {
    pub name: &'static str,
    pub arity: usize,
    pub native_fn: fn(Vec<RoxValue>) -> EvaluationResult,
}

#[derive(Clone, Debug)]
pub struct FnValue {
    name: String,
    params: Vec<Token>,
    body: Vec<Stmt>,
}

pub trait Callable: std::fmt::Debug {
    fn arity(&self) -> usize;
    fn call(&self, env: WrappedEnvironment, arguments: Vec<RoxValue>) -> EvaluationResult;
    fn name(&self) -> String;
}

impl Callable for FnValue {
    fn arity(&self) -> usize {
        self.params.len()
    }
    fn call(&self, env: WrappedEnvironment, arguments: Vec<RoxValue>) -> EvaluationResult {
        let new_env = Environment::new(Some(env.clone()));
        for (index, token) in self.params.iter().enumerate() {
            new_env
                .borrow_mut()
                .declare(token.lexem.clone(), arguments[index].clone())
        }

        for statement in self.body.clone() {
            statement.evaluate(new_env.clone())?
        }
        return Ok(RoxValue::Nil);
    }
    fn name(&self) -> String {
        self.name.clone()
    }
}

impl Callable for NativeFnValue {
    fn arity(&self) -> usize {
        self.arity
    }
    fn call(&self, _env: WrappedEnvironment, arguments: Vec<RoxValue>) -> EvaluationResult {
        (self.native_fn)(arguments)
    }
    fn name(&self) -> String {
        self.name.into()
    }
}

impl RoxValue {
    pub fn to_rox_number(self: RoxValue) -> Result<f64, RuntimeError> {
        if let RoxValue::Number(n) = self {
            Ok(n)
        } else {
            Err(RuntimeError {
                kind: RuntimeErrorKind::TypeError(format!("Expected number got {:?}", self)),
            })
        }
    }

    pub fn to_rox_string(self) -> Result<String, RuntimeError> {
        if let RoxValue::String(s) = self {
            Ok(s)
        } else {
            Err(RuntimeError {
                kind: RuntimeErrorKind::TypeError(format!("Expected string got {:?}", self)),
            })
        }
    }

    pub fn is_truthy(self: &RoxValue) -> bool {
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
            RoxValue::NativeFn(s) => write!(f, "native function: {}", s.name),
            RoxValue::Function(s) => write!(f, "function: {}", s.name),
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
    #[error("expression is not callable")]
    CalleeIsNotCallable,
    #[error("Invalid argument count expected: {param_len} but got {arguments_len}.")]
    MismatchedArgumentsAndParameters {
        arguments_len: usize,
        param_len: usize,
    },
}

pub type EvaluationResult = Result<RoxValue, RuntimeError>;

pub struct Environment {
    variables: HashMap<String, RoxValue>,
    enclosing: Option<WrappedEnvironment>,
}

impl Environment {
    pub fn global() -> Rc<RefCell<Self>> {
        let mut global_variables = HashMap::new();
        global_variables.insert("clock".into(), RoxValue::NativeFn(CLOCK_NATIVE_FN));
        global_variables.insert("read_file".into(), RoxValue::NativeFn(READ_FILE_NATIVE_FN));
        Rc::new(RefCell::new(Environment {
            variables: global_variables,
            enclosing: None,
        }))
    }

    pub fn new(enclosing: Option<WrappedEnvironment>) -> Rc<RefCell<Self>> {
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
    fn evaluate<'a, 'b>(self, env: WrappedEnvironment) -> Result<T, RuntimeError>;
}

impl Evaluate<()> for Stmt {
    fn evaluate<'a, 'b>(self, env: WrappedEnvironment) -> Result<(), RuntimeError> {
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
            Stmt::FunDecl { name, body, params } => {
                env.borrow_mut().declare(
                    name.clone(),
                    RoxValue::Function(FnValue { name, params, body }),
                );
                Ok(())
            }
            Stmt::IfElse {
                pred,
                if_branch,
                else_branch,
            } => {
                if pred.evaluate(env.clone())?.is_truthy() {
                    if_branch.evaluate(env.clone())?;
                } else {
                    if let Some(s) = else_branch {
                        s.evaluate(env.clone())?;
                    }
                }
                Ok(())
            }
            Stmt::WhileLoop { pred, body } => {
                while pred.clone().evaluate(env.clone())?.is_truthy() {
                    body.clone().evaluate(env.clone())?
                }
                Ok(())
            }
        }
    }
}

impl Evaluate for Expr {
    fn evaluate(self, env: WrappedEnvironment) -> EvaluationResult {
        match self {
            Expr::Literal(l) => l.evaluate(env),
            Expr::Unary(l) => l.evaluate(env),
            Expr::Binary(l) => l.evaluate(env),
            Expr::Grouping(l) => l.evaluate(env),
            Expr::Logical(t) => t.evaluate(env),
            Expr::Assign { name, value } => {
                let value = value.evaluate(env.clone())?;
                env.borrow_mut().assign(name, value.clone())?;
                return Ok(value);
            }
            Expr::Call { expr, arguments } => {
                let callee = expr.evaluate(env.clone())?;

                match callee {
                    RoxValue::NativeFn(function) => {
                        if arguments.len() != function.arity() {
                            return Err(RuntimeError {
                                kind: RuntimeErrorKind::MismatchedArgumentsAndParameters {
                                    arguments_len: arguments.len(),
                                    param_len: function.arity(),
                                },
                            });
                        }
                        let mut evaluated_arguments = Vec::with_capacity(arguments.len());
                        for argument in arguments {
                            evaluated_arguments.push(argument.evaluate(env.clone())?)
                        }

                        function.call(env.clone(), evaluated_arguments)
                    }
                    RoxValue::Function(function) => {
                        if arguments.len() != function.arity() {
                            return Err(RuntimeError {
                                kind: RuntimeErrorKind::MismatchedArgumentsAndParameters {
                                    arguments_len: arguments.len(),
                                    param_len: function.arity(),
                                },
                            });
                        }
                        let mut evaluated_arguments = Vec::with_capacity(arguments.len());
                        for argument in arguments {
                            evaluated_arguments.push(argument.evaluate(env.clone())?)
                        }

                        function.call(env.clone(), evaluated_arguments)
                    }
                    _ => Err(RuntimeError {
                        kind: RuntimeErrorKind::CalleeIsNotCallable,
                    }),
                }
            }
        }
    }
}

impl Evaluate for LiteralExpr {
    fn evaluate(self, env: WrappedEnvironment) -> EvaluationResult {
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
    fn evaluate(self, env: WrappedEnvironment) -> EvaluationResult {
        let right = self.right.evaluate(env)?;
        match self.operator.kind {
            TokenKind::Minus => right.to_rox_number().map(|n| RoxValue::Number(-n)),
            TokenKind::Bang => Ok(RoxValue::Boolean(!right.is_truthy())),
            _ => unreachable!(),
        }
    }
}

impl Evaluate for BinaryExpr {
    fn evaluate(self, env: WrappedEnvironment) -> EvaluationResult {
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
            TokenKind::Greater => {
                let left_number = left.to_rox_number()?;
                let right_number = right.to_rox_number()?;
                Ok(RoxValue::Boolean(left_number > right_number))
            }
            TokenKind::Less => {
                let left_number = left.to_rox_number()?;
                let right_number = right.to_rox_number()?;
                Ok(RoxValue::Boolean(left_number < right_number))
            }
            _ => unreachable!(),
        }
    }
}

impl Evaluate for LogicalExpr {
    fn evaluate(self, env: WrappedEnvironment) -> EvaluationResult {
        let left = self.left.evaluate(env.clone())?;
        match self.operator.kind {
            TokenKind::And => {
                if left.is_truthy() {
                    self.right.evaluate(env)
                } else {
                    Ok(left)
                }
            }
            TokenKind::Or => {
                if left.is_truthy() {
                    Ok(left)
                } else {
                    self.right.evaluate(env.clone())
                }
            }
            _ => unreachable!(),
        }
    }
}

pub fn evaluate_statements(
    statements: Vec<Stmt>,
    env: WrappedEnvironment,
) -> Result<(), RuntimeError> {
    for statement in statements {
        statement.evaluate(env.clone())?
    }

    Ok(())
}
