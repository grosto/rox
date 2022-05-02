use std::{collections::HashMap, str::Chars};

pub struct Scanner<'a> {
    chars: Chars<'a>,
    line: u32,
    keywords: HashMap<String, TokenKind>,
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        self.scan_token()
    }
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        let keywords: HashMap<String, TokenKind> = HashMap::from_iter([
            ("and".into(), TokenKind::And),
            ("class".into(), TokenKind::Class),
            ("else".into(), TokenKind::Else),
            ("false".into(), TokenKind::False),
            ("for".into(), TokenKind::For),
            ("fun".into(), TokenKind::Fun),
            ("if".into(), TokenKind::If),
            ("nil".into(), TokenKind::Nil),
            ("or".into(), TokenKind::Or),
            ("print".into(), TokenKind::Print),
            ("return".into(), TokenKind::Return),
            ("super".into(), TokenKind::Super),
            ("this".into(), TokenKind::This),
            ("true".into(), TokenKind::True),
            ("var".into(), TokenKind::Var),
            ("while".into(), TokenKind::While),
        ]);

        Self {
            chars: source.chars(),
            line: 1,
            keywords,
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
            '(' => self.create_token(TokenKind::LeftParen, c),
            ')' => self.create_token(TokenKind::RightParen, c),
            '+' => self.create_token(TokenKind::Plus, c),
            '-' => self.create_token(TokenKind::Minus, c),
            '*' => self.create_token(TokenKind::Star, c),
            '[' => self.create_token(TokenKind::LeftBrace, c),
            ']' => self.create_token(TokenKind::RightBrace, c),
            ',' => self.create_token(TokenKind::Comma, c),
            '.' => self.create_token(TokenKind::Dot, c),
            ';' => self.create_token(TokenKind::Semicolon, c),
            '!' => match self.peek_first()? {
                '=' => {
                    self.advance();
                    self.create_token(TokenKind::BangEqual, "!=")
                }
                _ => self.create_token(TokenKind::Bang, c),
            },
            '=' => match self.peek_first()? {
                '=' => {
                    self.advance();
                    self.create_token(TokenKind::EqualEqual, "==")
                }
                _ => self.create_token(TokenKind::Equal, "="),
            },
            '>' => match self.peek_first()? {
                '=' => {
                    self.advance();
                    self.create_token(TokenKind::GreaterEqual, ">=")
                }
                _ => self.create_token(TokenKind::Greater, c),
            },
            '<' => match self.peek_first()? {
                '=' => {
                    self.advance();
                    self.create_token(TokenKind::LessEqual, ">=")
                }
                _ => self.create_token(TokenKind::Less, c),
            },
            '/' => match self.peek_first()? {
                '/' => {
                    self.advance_until('\n');
                    return self.scan_token();
                }
                _ => self.create_token(TokenKind::Slash, c),
            },
            '"' => {
                let mut string = String::new();
                while self.peek_first()? != '"' {
                    string.push(self.advance().expect("Unexpected EOF"))
                }

                let closing = self.advance();
                // we reached eof without closing string
                if closing.is_none() {
                    self.create_token(TokenKind::Unknown, "")
                } else {
                    self.create_token(
                        TokenKind::Literal {
                            kind: LiteralToken::String,
                        },
                        string,
                    )
                }
            }
            '0'..='9' => self.number(c),
            c if Self::is_alpha(&c) => {
                let mut identifier = String::from(c);
                let mut c = self.peek_first()?;
                while Self::is_alpha(&c) {
                    identifier.push(c);
                    self.advance();
                    c = self.peek_first()?;
                }
                if let Some(token) = self.keywords.get(identifier.as_str()) {
                    self.create_token(*token, identifier)
                } else {
                    self.create_token(
                        TokenKind::Literal {
                            kind: LiteralToken::Identifier,
                        },
                        identifier,
                    )
                }
            }
            _ => self.create_token(TokenKind::Unknown, ""),
        }
        .into()
    }

    fn is_alpha(c: &char) -> bool {
        c.is_ascii_alphanumeric() || c == &'_'
    }

    fn create_token<T: Into<String>>(&self, kind: TokenKind, lexem: T) -> Token {
        Token {
            line: self.line,
            lexem: lexem.into(),
            kind,
        }
    }

    fn number(&mut self, c: char) -> Token {
        let mut number = c.to_string();
        while let Some(c2 @ '0'..='9') = self.peek_first() {
            number.push(c2.clone());
            self.advance();
        }

        if self.peek_first() == Some('.') {
            if let Some('0'..='9') = self.peek_second() {
                number.push('.');
                self.advance();
            };
        };

        while let Some(c2 @ '0'..='9') = self.peek_first() {
            number.push(c2.clone());
            self.advance();
        }

        self.create_token(
            TokenKind::Literal {
                kind: LiteralToken::Number(number.parse().expect("Oopsie")),
            },
            number,
        )
    }

    fn advance_until(&mut self, predicate_char: char) {
        while self.peek_first() != Some(predicate_char) {
            self.advance();
        }
    }

    fn peek_first(&self) -> Option<char> {
        self.chars.clone().next()
    }

    fn peek_second(&self) -> Option<char> {
        let mut iter = self.chars.clone();
        iter.next();
        iter.next()
    }

    fn advance(&mut self) -> Option<char> {
        self.chars.next()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub line: u32,
    pub lexem: String,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TokenKind {
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
    Literal { kind: LiteralToken },

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

    Unknown,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LiteralToken {
    Identifier,
    String,
    Number(f64),
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn ignores_comments() {
        let source = "// dasdassasadsad 
        2 + 2";
        let s = Scanner::new(source.into());

        let tokens = s.collect::<Vec<Token>>();
        println!("{tokens:#?}");
    }
    #[test]
    fn string() {
        let source = r#"alex"#;
        println!("{source:?}");
        let s = Scanner::new(source.into());

        let tokens = s.collect::<Vec<Token>>();
        println!("{tokens:#?}");
    }
}
