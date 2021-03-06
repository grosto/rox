use crate::scanner::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Print(Expr),
    ExprStmt(Expr),
    VarDecl {
        name: String,
        init: Option<Expr>,
    },
    FunDecl {
        name: String,
        params: Vec<Token>,
        body: Vec<Stmt>,
    },
    Block(Vec<Stmt>),
    IfElse {
        pred: Expr,
        if_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    WhileLoop {
        pred: Expr,
        body: Box<Stmt>,
    },
    Return(Option<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Binary(Box<BinaryExpr>),
    Grouping(Box<Expr>),
    Literal(LiteralExpr),
    Unary(Box<UnaryExpr>),
    Logical(Box<LogicalExpr>),
    Call {
        expr: Box<Expr>,
        arguments: Vec<Expr>,
    },
    Assign {
        name: String,
        value: Box<Expr>,
    },
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

#[derive(Debug, Clone, PartialEq)]
pub struct LogicalExpr {
    pub operator: Token,
    pub left: Expr,
    pub right: Expr,
}
