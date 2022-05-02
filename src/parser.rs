use crate::{
    ast::{BinaryExpr, Expr, LiteralExpression, Stmt, UnaryExpr},
    scanner::{LiteralToken, Token, TokenKind},
};

use std::iter::Peekable;
pub struct Parser<I>
where
    I: Iterator<Item = Token>,
{
    tokens: Peekable<I>,
}

#[derive(Debug)]
pub struct ParseError {}

impl<I: Iterator<Item = Token>> Parser<I> {
    pub fn new(tokens: I) -> Self {
        Parser {
            tokens: tokens.peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = vec![];
        while self.tokens.peek().is_some() {
            statements.push(self.statement()?)
        }
        Ok(statements)
    }

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        let next_token = self
            .tokens
            .peek()
            .expect("caller of statement should ensure there are more tokens");
        println!("{next_token:?}");
        let stmt = match next_token.kind {
            TokenKind::Print => {
                self.tokens.next();
                Stmt::Print(self.expression()?)
            }
            _ => Stmt::ExprStmt(self.expression()?),
        };
        self.ensure_next_token(TokenKind::Semicolon).map(|_| stmt)
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.equality()
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
            TokenKind::False => Expr::Literal(LiteralExpression::Boolean(false)),
            TokenKind::True => Expr::Literal(LiteralExpression::Boolean(true)),
            TokenKind::Nil => Expr::Literal(LiteralExpression::Nil),
            TokenKind::Literal { kind } => match kind {
                LiteralToken::Number(n) => Expr::Literal(LiteralExpression::Number(n)),
                LiteralToken::String => Expr::Literal(LiteralExpression::String(token.lexem)),
                LiteralToken::Identifier => return Err(ParseError {}),
            },
            TokenKind::LeftParen => {
                let expr = self.expression()?;
                self.ensure_next_token(TokenKind::RightParen)?;
                Expr::Grouping(Box::new(expr))
            }
            _ => panic!("whoa what is this token"),
        };
        Ok(expr)
    }

    fn ensure_next_token(&mut self, token_kind: TokenKind) -> Result<(), ParseError> {
        if let Some(token) = self.tokens.next() {
            if token.kind == token_kind {
                return Ok(());
            }
        }
        Err(ParseError {})
    }

    fn advance_on_match(&mut self, token_types: &[TokenKind]) -> Option<Token> {
        self.tokens
            .next_if(|token| token_types.contains(&token.kind))
    }
}
