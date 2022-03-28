use std::{
    iter::{Peekable, Scan},
    str::Chars,
};

pub(crate) const EOF_CHAR: char = '\0';

pub struct Scanner<'a> {
    chars: Chars<'a>,
    line: u32,
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        self.scan_token()
    }
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            chars: source.chars(),
            line: 1,
        }
    }

    fn scan_token(&mut self) -> Option<Token> {
        let c = self.advance()?;
        match c {
            ' ' => return self.scan_token(),
            '\n' => {
                self.line += 1;
                self.advance();
                return self.scan_token();
            }
            '(' => self.create_token(TokenKind::LeftParen, c.into()),
            ')' => self.create_token(TokenKind::RightParen, c.into()),
            '+' => self.create_token(TokenKind::Plus, c.into()),
            '/' => {
                let next_char = self.peek_first();
                println!("next char is {next_char}");
                match next_char {
                    '/' => {
                        self.advance_until('\n');
                        return self.scan_token();
                    }
                    _ => self.create_token(TokenKind::Slash, c.into()),
                }
            }

            c @ '0'..='9' => self.number(c),
            _ => self.create_token(TokenKind::Unknown, "".into()),
        }
        .into()
    }

    fn create_token(&self, kind: TokenKind, lexem: String) -> Token {
        Token {
            line: self.line,
            lexem,
            kind,
        }
    }

    fn number(&mut self, c: char) -> Token {
        let mut number = c.to_string();
        while let c2 @ '0'..='9' = self.peek_first() {
            number.push(c2.clone());
            self.advance();
        }

        if self.peek_first() == '.' {
            if let '0'..='9' = self.peek_second() {
                number.push('.');
                self.advance();
            };
        };

        while let c2 @ '0'..='9' = self.peek_first() {
            number.push(c2.clone());
            self.advance();
        }

        self.create_token(
            TokenKind::Literal {
                kind: LiteralKind::Number(number.parse().expect("Oopsie")),
            },
            number,
        )
    }

    fn advance_until(&mut self, predicate_char: char) {
        while self.peek_first() != predicate_char {
            self.advance();
        }
    }

    fn peek_first(&self) -> char {
        self.chars.clone().next().unwrap_or(EOF_CHAR)
    }

    fn peek_second(&self) -> char {
        let mut iter = self.chars.clone();
        iter.next();
        iter.next().unwrap_or(EOF_CHAR)
    }

    fn advance(&mut self) -> Option<char> {
        self.chars.next()
    }
}

#[derive(Debug)]
pub struct Token {
    kind: TokenKind,
    line: u32,
    lexem: String,
}

#[derive(Clone, Copy, Debug)]
enum TokenKind {
    // Single character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literal
    Literal { kind: LiteralKind },

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,

    Unknown,
}

#[derive(Clone, Copy, Debug)]
enum LiteralKind {
    Identifier,
    String,
    Number(i64),
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn ignores_comments() {
        let source = "// dasdassasadsad 
        2 + 2";
        let mut s = Scanner::new(source.into());

        let tokens = s.collect::<Vec<Token>>();
        println!("{tokens:#?}");
    }
}
