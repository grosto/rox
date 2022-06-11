use crate::{
    ast::{BinaryExpr, Expr, LiteralExpr, LogicalExpr, Stmt, UnaryExpr},
    scanner::{LiteralToken, Scanner, Token, TokenKind},
};

use std::iter::Peekable;
use thiserror::Error;

pub fn parse_source_code(source_code: &str) -> Result<Vec<Stmt>, ParseError> {
    Parser::new(Scanner::new(&source_code)).parse()
}

pub struct Parser<I>
where
    I: Iterator<Item = Token>,
{
    tokens: Peekable<I>,
}

#[derive(Debug, Error)]
#[error("Parse error on line {line}: {message}")]
pub struct ParseError {
    message: String,
    line: u32,
}

impl<I: Iterator<Item = Token>> Parser<I> {
    pub fn new(tokens: I) -> Self {
        Parser {
            tokens: tokens.peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = vec![];

        while self.tokens.peek().is_some() {
            statements.push(self.declaration()?)
        }
        Ok(statements)
    }

    fn declaration(&mut self) -> Result<Stmt, ParseError> {
        let next_token = self
            .tokens
            .peek()
            .expect("caller of declrations must ensure there are tokens");

        match next_token.kind {
            TokenKind::Var => {
                self.tokens.next();
                self.var_decl()
            }
            TokenKind::Fun => {
                self.tokens.next();
                self.function()
            }
            _ => self.statement(),
        }
    }

    fn var_decl(&mut self) -> Result<Stmt, ParseError> {
        let token = self.ensure_next_token(TokenKind::Literal {
            kind: LiteralToken::Identifier,
        })?;
        let stmt = if self.advance_on_match(&[TokenKind::Equal]).is_some() {
            let init = self.expression()?;
            Stmt::VarDecl {
                name: token.lexem,
                init: Some(init),
            }
        } else {
            Stmt::VarDecl {
                name: token.lexem,
                init: None,
            }
        };

        self.ensure_next_token(TokenKind::Semicolon).map(|_| stmt)
    }

    fn function(&mut self) -> Result<Stmt, ParseError> {
        let identifer = self.ensure_next_token(TokenKind::Literal {
            kind: LiteralToken::Identifier,
        })?;
        self.ensure_next_token(TokenKind::LeftParen)?;
        let mut params = vec![];
        let closing_paren = self.advance_on_match(&[TokenKind::RightParen]);
        if closing_paren.is_none() {
            params.push(self.ensure_next_token(TokenKind::Literal {
                kind: LiteralToken::Identifier,
            })?);
            while self.advance_on_match(&[TokenKind::Comma]).is_some() {
                params.push(self.ensure_next_token(TokenKind::Literal {
                    kind: LiteralToken::Identifier,
                })?);
            }
            self.ensure_next_token(TokenKind::RightParen)?;
        }

        self.ensure_next_token(TokenKind::LeftBrace)?;
        let body = self.block_statement()?;

        Ok(Stmt::FunDecl {
            name: identifer.lexem,
            params: params,
            body,
        })
    }

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        let next_token = self
            .tokens
            .peek()
            .expect("caller of statement should ensure there are more tokens");

        let stmt = match next_token.kind {
            TokenKind::Print => {
                self.tokens.next();
                let stmt = Stmt::Print(self.expression()?);
                self.ensure_next_token(TokenKind::Semicolon)?;
                stmt
            }
            TokenKind::LeftBrace => {
                self.tokens.next();
                Stmt::Block(self.block_statement()?)
            }
            TokenKind::If => {
                self.tokens.next();
                self.if_else_statement()?
            }
            TokenKind::While => {
                self.tokens.next();
                self.while_loop()?
            }
            TokenKind::For => {
                self.tokens.next();
                self.for_loop()?
            }
            TokenKind::Return => {
                self.tokens.next();
                self.return_statement()?
            }
            _ => self.expr_statement()?,
        };

        Ok(stmt)
    }

    fn return_statement(&mut self) -> Result<Stmt, ParseError> {
        let expr = if self.advance_on_match(&[TokenKind::Semicolon]).is_none() {
            let expr = Some(self.expression()?);
            self.ensure_next_token(TokenKind::Semicolon)?;
            expr
        } else {
            None
        };

        Ok(Stmt::Return(expr))
    }
    fn expr_statement(&mut self) -> Result<Stmt, ParseError> {
        let expr = Ok(Stmt::ExprStmt(self.expression()?));
        self.ensure_next_token(TokenKind::Semicolon)?;
        expr
    }

    fn if_else_statement(&mut self) -> Result<Stmt, ParseError> {
        self.ensure_next_token(TokenKind::LeftParen)?;
        let pred = self.expression()?;
        self.ensure_next_token(TokenKind::RightParen)?;
        let left = Box::new(self.statement()?);
        let right = if let Some(_) = self.advance_on_match(&[TokenKind::Else]) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };
        Ok(Stmt::IfElse {
            pred,
            if_branch: left,
            else_branch: right,
        })
    }

    fn while_loop(&mut self) -> Result<Stmt, ParseError> {
        self.ensure_next_token(TokenKind::LeftParen)?;
        let pred = self.expression()?;
        self.ensure_next_token(TokenKind::RightParen)?;
        let body = Box::new(self.statement()?);

        Ok(Stmt::WhileLoop { pred, body })
    }

    fn for_loop(&mut self) -> Result<Stmt, ParseError> {
        self.ensure_next_token(TokenKind::LeftParen)?;

        let init = if self.advance_on_match(&[TokenKind::Semicolon]).is_none() {
            if self.advance_on_match(&[TokenKind::Var]).is_some() {
                Some(self.var_decl()?)
            } else {
                Some(self.expr_statement()?)
            }
        } else {
            None
        };

        let condition = if self.advance_on_match(&[TokenKind::Semicolon]).is_none() {
            let expr = Some(self.expression()?);
            self.ensure_next_token(TokenKind::Semicolon)?;
            expr
        } else {
            None
        };

        let increment = if self.advance_on_match(&[TokenKind::Semicolon]).is_none() {
            Some(self.expression()?)
        } else {
            None
        };

        self.ensure_next_token(TokenKind::RightParen)?;
        let body = self.statement()?;
        let body = if let Some(expr) = increment {
            Stmt::Block(vec![body, Stmt::ExprStmt(expr)])
        } else {
            body
        };

        let condition = condition.unwrap_or(Expr::Literal(LiteralExpr::Boolean(true)));
        let while_loop = Stmt::WhileLoop {
            pred: condition,
            body: Box::new(body),
        };

        let block = if let Some(init) = init {
            Stmt::Block(vec![init, while_loop])
        } else {
            while_loop
        };

        Ok(block)
    }

    fn block_statement(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = vec![];

        loop {
            if self.tokens.peek().is_none() {
                return Err(ParseError {
                    message: "Expected '}' after block".into(),
                    line: 0,
                });
            }

            if self.advance_on_match(&[TokenKind::RightBrace]).is_some() {
                return Ok(statements);
            }

            statements.push(self.declaration()?);
        }
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.logical_or()?;

        while self.advance_on_match(&[TokenKind::Equal]).is_some() {
            let value = self.assignment()?;

            if let Expr::Literal(LiteralExpr::Variable(name)) = expr {
                return Ok(Expr::Assign {
                    name,
                    value: Box::new(value),
                });
            }

            return Err(ParseError {
                line: 0,
                message: "Invalid assigment target".into(),
            });
        }
        Ok(expr)
    }

    fn logical_or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.logical_and()?;

        while let Some(operator) = self.advance_on_match(&[TokenKind::Or]) {
            let right = self.comparison()?;
            expr = Expr::Logical(Box::new(LogicalExpr {
                left: expr,
                right,
                operator,
            }))
        }

        Ok(expr)
    }

    fn logical_and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.equality()?;

        while let Some(operator) = self.advance_on_match(&[TokenKind::And]) {
            let right = self.comparison()?;
            expr = Expr::Logical(Box::new(LogicalExpr {
                left: expr,
                right,
                operator,
            }))
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.comparison()?;
        while let Some(operator) =
            self.advance_on_match(&[TokenKind::BangEqual, TokenKind::EqualEqual])
        {
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(BinaryExpr {
                left: expr,
                right,
                operator,
            }))
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.term()?;
        while let Some(operator) = self.advance_on_match(&[
            TokenKind::Greater,
            TokenKind::GreaterEqual,
            TokenKind::LessEqual,
            TokenKind::Less,
        ]) {
            let right = self.term()?;
            expr = Expr::Binary(Box::new(BinaryExpr {
                left: expr,
                right,
                operator,
            }))
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.factor()?;
        while let Some(operator) = self.advance_on_match(&[TokenKind::Plus, TokenKind::Minus]) {
            let right = self.factor()?;
            expr = Expr::Binary(Box::new(BinaryExpr {
                left: expr,
                right,
                operator,
            }))
        }

        Ok(expr)
    }
    fn factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.unary()?;

        while let Some(operator) = self.advance_on_match(&[TokenKind::Star, TokenKind::Slash]) {
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(BinaryExpr {
                left: expr,
                right,
                operator,
            }))
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if let Some(operator) = self.advance_on_match(&[TokenKind::Bang, TokenKind::Minus]) {
            let expr = self.unary()?;
            return Ok(Expr::Unary(Box::new(UnaryExpr {
                operator,
                right: expr,
            })));
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.primary()?;

        loop {
            if self.advance_on_match(&[TokenKind::LeftParen]).is_some() {
                let mut arguments = vec![];
                let closing_paren = self.advance_on_match(&[TokenKind::RightParen]);
                if let Some(_) = closing_paren {
                    expr = Expr::Call {
                        expr: Box::new(expr),
                        arguments,
                    };
                } else {
                    arguments.push(self.expression()?);
                    while self.advance_on_match(&[TokenKind::Comma]).is_some() {
                        arguments.push(self.expression()?);
                    }
                    self.ensure_next_token(TokenKind::RightParen)?;
                    expr = Expr::Call {
                        expr: Box::new(expr),

                        arguments,
                    };
                }
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        let token = self.tokens.next().unwrap();
        let expr = match token.kind {
            TokenKind::False => Expr::Literal(LiteralExpr::Boolean(false)),
            TokenKind::True => Expr::Literal(LiteralExpr::Boolean(true)),
            TokenKind::Nil => Expr::Literal(LiteralExpr::Nil),
            TokenKind::Literal { kind } => match kind {
                LiteralToken::Number(n) => Expr::Literal(LiteralExpr::Number(n)),
                LiteralToken::String => Expr::Literal(LiteralExpr::String(token.lexem)),
                LiteralToken::Identifier => Expr::Literal(LiteralExpr::Variable(token.lexem)),
            },
            TokenKind::LeftParen => {
                let expr = self.expression()?;
                self.ensure_next_token(TokenKind::RightParen)?;
                Expr::Grouping(Box::new(expr))
            }
            _ => panic!("whoa what is this token {:?}", token.kind),
        };
        Ok(expr)
    }

    fn ensure_next_token(&mut self, token_kind: TokenKind) -> Result<Token, ParseError> {
        if let Some(token) = self.tokens.next() {
            if token.kind == token_kind {
                Ok(token)
            } else {
                Err(ParseError {
                    message: format!(
                        "expected next token to be {:?} but found {:?}",
                        token_kind, token.kind
                    ),
                    line: token.line.clone(),
                })
            }
        } else {
            Err(ParseError {
                message: format!(
                    "expected next token to be {:?} but found the end of file",
                    token_kind
                ),
                line: 0,
            })
        }
    }

    fn advance_on_match(&mut self, token_types: &[TokenKind]) -> Option<Token> {
        self.tokens
            .next_if(|token| token_types.contains(&token.kind))
    }
}
