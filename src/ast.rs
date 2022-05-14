use crate::scanner::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Print(Expr),
    ExprStmt(Expr),
    VarDecl { name: String, init: Option<Expr> },
    Block(Vec<Stmt>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Binary(Box<BinaryExpr>),
    Grouping(Box<Expr>),
    Literal(LiteralExpr),
    Unary(Box<UnaryExpr>),
    Assign { name: String, value: Box<Expr> },
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralExpr {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
    Variable(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub operator: Token,
    pub right: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    pub left: Expr,
    pub operator: Token,
    pub right: Expr,
}
