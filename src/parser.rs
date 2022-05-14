use crate::{
    ast::{BinaryExpr, Expr, LiteralExpr, Stmt, UnaryExpr},
    scanner::{LiteralToken, Token, TokenKind},
};

use std::iter::Peekable;
use thiserror::Error;
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

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        let next_token = self
            .tokens
            .peek()
            .expect("caller of statement should ensure there are more tokens");

        let stmt = match next_token.kind {
            TokenKind::Print => {
                self.tokens.next();
                Stmt::Print(self.expression()?)
            }
            TokenKind::LeftBrace => {
                self.tokens.next();
                Stmt::Block(self.block_statement()?)
            }
            _ => Stmt::ExprStmt(self.expression()?),
        };
        match stmt {
            Stmt::Block(_) => {}
            _ => {
                self.ensure_next_token(TokenKind::Semicolon)?;
            }
        };

        Ok(stmt)
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
        let expr = self.equality()?;

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
            self.primary()
        }
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
