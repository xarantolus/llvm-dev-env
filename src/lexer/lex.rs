use core::fmt;
use std::{error::Error, fmt::Display, num::ParseIntError};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    // Imports
    Need,
    Include,

    // Procedures
    Proc,

    Identifier(String),

    LBrace,
    RBrace,

    LBracket,
    RBracket,

    LParen,
    RParen,

    Minus,
    Plus,
    Star,
    Slash,
    Percent,

    // =
    Assign,

    // ==
    Equality,
    // <>
    NotEqual,
    // <
    Less,
    // >
    Greater,
    // <=
    LessEqual,
    // >=
    GreaterEqual,
    // and
    And,
    // or
    Or,
    // not
    Not,

    Semicolon,
    Dot,
    DotDot,
    // ->
    Arrow,

    Question,
    Colon,
    DoubleColon,
    Comma,

    At,

    StringLiteral(String),
    IntLiteral(i64),
    BoolLiteral(bool),

    EOF,
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Need => write!(f, "need"),
            Token::Include => write!(f, "include"),
            Token::Proc => write!(f, "proc"),
            Token::Identifier(s) => write!(f, "{}", s),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::Minus => write!(f, "-"),
            Token::Plus => write!(f, "+"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Percent => write!(f, "%"),
            Token::Assign => write!(f, "="),
            Token::Equality => write!(f, "=="),
            Token::NotEqual => write!(f, "<>"),
            Token::Less => write!(f, "<"),
            Token::Greater => write!(f, ">"),
            Token::LessEqual => write!(f, "<="),
            Token::GreaterEqual => write!(f, ">="),
            Token::And => write!(f, "and"),
            Token::Or => write!(f, "or"),
            Token::Not => write!(f, "not"),
            Token::Semicolon => write!(f, ";"),
            Token::Dot => write!(f, "."),
            Token::DotDot => write!(f, ".."),
            Token::Arrow => write!(f, "->"),
            Token::Question => write!(f, "?"),
            Token::Colon => write!(f, ":"),
            Token::DoubleColon => write!(f, "::"),
            Token::Comma => write!(f, ","),
            Token::At => write!(f, "@"),
            Token::StringLiteral(s) => write!(f, "{:#?}", s),
            Token::IntLiteral(n) => write!(f, "{}", n),
            Token::BoolLiteral(b) => write!(f, "{}", b),
            Token::EOF => write!(f, ""),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum LexErrorType {
    InvalidToken(String),
    InvalidNumber(ParseIntError),
    InvalidStringEscape(char),
    UnexpectedEOF(Token),
}

#[derive(Debug, PartialEq, Clone)]
pub struct LexError {
    pub error_type: LexErrorType,
    pub line: usize,
    pub column: usize,
}

impl LexError {
    fn new(error_type: LexErrorType, input: &str, pos: usize) -> Self {
        let mut line = 1;
        let mut column = 0;

        for (i, c) in input.chars().enumerate() {
            if i == pos {
                break;
            }

            if c == '\n' {
                line += 1;
                column = 0;
            } else {
                column += 1;
            }
        }

        Self {
            error_type,
            line,
            column,
        }
    }
}

impl Error for LexError {}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.error_type {
            LexErrorType::InvalidToken(ref token) => write!(f, "Invalid token: {:#?}", token),
            LexErrorType::InvalidStringEscape(c) => {
                write!(f, "Invalid string escape: {:#?} must not be escaped", c)
            }
            LexErrorType::UnexpectedEOF(ref token) => {
                write!(f, "Unexpected EOF while parsing {} token", token)
            }
            LexErrorType::InvalidNumber(ref err) => write!(f, "Invalid number: {}", err),
        }
    }
}

impl Token {
    fn lex_single_char(c: char, next_c: char) -> Option<(Token, usize)> {
        Some(match c {
            '(' => (Token::LParen, 1),
            ')' => (Token::RParen, 1),
            '[' => (Token::LBracket, 1),
            ']' => (Token::RBracket, 1),
            '{' => (Token::LBrace, 1),
            '}' => (Token::RBrace, 1),
            '-' => {
                if next_c == '>' {
                    (Token::Arrow, 2)
                } else {
                    (Token::Minus, 1)
                }
            }
            '+' => (Token::Plus, 1),
            '*' => (Token::Star, 1),
            '/' => (Token::Slash, 1),
            '%' => (Token::Percent, 1),
            '=' => {
                if next_c == '=' {
                    (Token::Equality, 2)
                } else {
                    (Token::Assign, 1)
                }
            }
            '<' => {
                if next_c == '>' {
                    (Token::NotEqual, 2)
                } else if next_c == '=' {
                    (Token::LessEqual, 2)
                } else {
                    (Token::Less, 1)
                }
            }
            '>' => {
                if next_c == '=' {
                    (Token::GreaterEqual, 2)
                } else {
                    (Token::Greater, 1)
                }
            }
            ';' => (Token::Semicolon, 1),
            '.' => {
                if next_c == '.' {
                    (Token::DotDot, 2)
                } else {
                    (Token::Dot, 1)
                }
            }
            '?' => (Token::Question, 1),
            ':' => {
                if next_c == ':' {
                    (Token::DoubleColon, 2)
                } else {
                    (Token::Colon, 1)
                }
            }
            ',' => (Token::Comma, 1),
            '@' => (Token::At, 1),
            _ => return None,
        })
    }

    pub fn lex(input: &str, start: usize) -> Result<(Token, usize), LexError> {
        let input = &input[start..];
        if input.len() == 0 {
            return Ok((Token::EOF, start));
        }

        let mut offset = 0;
        // Skip leading whitespace in input
        while offset < input.len() && input.chars().nth(offset).unwrap().is_whitespace() {
            offset += 1;
        }
        if offset >= input.len() {
            return Ok((Token::EOF, start));
        }

        // Check if token is a single character
        let c = input.chars().nth(offset).unwrap();
        let next_c = if offset + 1 < input.len() {
            input.chars().nth(offset + 1).unwrap()
        } else {
            '\0'
        };
        if let Some(token) = Token::lex_single_char(c, next_c) {
            return Ok((token.0, start + offset + token.1));
        }

        // Check if token is a string
        if c == '"' {
            let mut string = String::new();
            offset += 1;

            // Read the entire string and allow for escape characters
            let mut escape = false;
            loop {
                if offset >= input.len() {
                    return Err(LexError::new(
                        LexErrorType::UnexpectedEOF(Token::StringLiteral(string)),
                        input,
                        start + offset,
                    ));
                }
                let c = input.chars().nth(offset).unwrap();
                if escape {
                    match c {
                        'n' => string.push('\n'),
                        't' => string.push('\t'),
                        '\\' => string.push('\\'),
                        '"' => string.push('"'),
                        _ => {
                            return Err(LexError::new(
                                LexErrorType::InvalidStringEscape(c),
                                input,
                                start,
                            ));
                        }
                    }
                    escape = false;
                } else if c == '"' {
                    break;
                } else if c == '\\' {
                    escape = true;
                } else {
                    string.push(c);
                }
                offset += 1;
            }

            return Ok((Token::StringLiteral(string), start + offset + 1));
        }

        // Check if token is a number
        if c.is_digit(10) {
            let mut number = String::new();

            while offset < input.len() {
                let c = input.chars().nth(offset).unwrap();
                // basically allow digits and an 'x' for hex numbers 0x1234
                if !(c.is_digit(16) || c == 'x') {
                    break;
                }
                number.push(c);
                offset += 1;
            }

            // Now parse it. We allow numbers with 0x prefix to be hex
            if number.starts_with("0x") {
                return match i64::from_str_radix(&number[2..], 16) {
                    Ok(number) => Ok((Token::IntLiteral(number), start + offset)),
                    Err(e) => Err(LexError::new(LexErrorType::InvalidNumber(e), input, start)),
                };
            }
            return match i64::from_str_radix(&number, 10) {
                Ok(number) => Ok((Token::IntLiteral(number), start + offset)),
                Err(e) => Err(LexError::new(LexErrorType::InvalidNumber(e), input, start)),
            };
        }

        // Otherwise we consume until the next whitespace OR occurence of a single character token
        let mut iden = String::new();
        while offset < input.len() {
            let c = input.chars().nth(offset).unwrap();
            if c.is_whitespace() || Token::lex_single_char(c, '\0').is_some() {
                break;
            }
            iden.push(c);
            offset += 1;
        }

        match iden.as_str() {
            "proc" => Ok((Token::Proc, start + offset)),
            "need" => Ok((Token::Need, start + offset)),
            "include" => Ok((Token::Include, start + offset)),
            "and" => Ok((Token::And, start + offset)),
            "or" => Ok((Token::Or, start + offset)),
            "not" => Ok((Token::Not, start + offset)),
            "true" => Ok((Token::BoolLiteral(true), start + offset)),
            "false" => Ok((Token::BoolLiteral(false), start + offset)),
            _ => Ok((Token::Identifier(iden), start + offset)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::vec;

    fn assert_token_stream(s: &str, tokens: Vec<Token>) {
        let mut str_pos = 0;
        let mut tok_pos = 0;

        while tok_pos < tokens.len() {
            let (token, next) = Token::lex(s, str_pos).expect("Failed to lex token");

            assert_eq!(
                token, tokens[tok_pos],
                "Expected {:?} but got {:?} at position {} as token {}",
                tokens[tok_pos], token, str_pos, tok_pos
            );
            str_pos = next;
            tok_pos += 1;
        }

        assert_eq!(tok_pos, tokens.len());

        // Make sure the last thing is an EOF
        assert_eq!(Token::lex(s, str_pos).unwrap().0, Token::EOF);
    }

    #[test]
    fn simple_math() {
        assert_token_stream(
            "1+5",
            vec![Token::IntLiteral(1), Token::Plus, Token::IntLiteral(5)],
        );
        assert_token_stream(
            "1 + 5",
            vec![Token::IntLiteral(1), Token::Plus, Token::IntLiteral(5)],
        );
        assert_token_stream(
            "1 + 5 - 10",
            vec![
                Token::IntLiteral(1),
                Token::Plus,
                Token::IntLiteral(5),
                Token::Minus,
                Token::IntLiteral(10),
            ],
        );

        assert_token_stream(
            "(1+5) * 3",
            vec![
                Token::LParen,
                Token::IntLiteral(1),
                Token::Plus,
                Token::IntLiteral(5),
                Token::RParen,
                Token::Star,
                Token::IntLiteral(3),
            ],
        );
    }

    #[test]
    fn lex_simple_program() {
        let program = include_str!("../../testdata/example-program.l").trim();

        let mut pos = 0;
        loop {
            let (token, next) = Token::lex(program, pos).expect("Failed to lex token");
            if token == Token::EOF {
                assert!(next == program.len(), "Didn't lex entire program");
                break;
            }
            pos = next;
        }
    }

    #[test]
    fn strings() {
        assert_token_stream("\"test\"", vec![Token::StringLiteral("test".to_string())]);

        assert_token_stream(
            "\"test\" + \"test\"",
            vec![
                Token::StringLiteral("test".to_string()),
                Token::Plus,
                Token::StringLiteral("test".to_string()),
            ],
        );

        // Escape sequences: \n, \t, \", \\
        assert_token_stream(
            "\"\\n\\\"\\t\\\\\"",
            vec![Token::StringLiteral("\n\"\t\\".to_string())],
        );
    }

    #[test]
    fn conditionals() {
        assert_token_stream(
            r#"? n == 0 {
		1
	} : {
		n * factorial(n-1)
	}"#,
            vec![
                Token::Question,
                Token::Identifier("n".to_string()),
                Token::Equality,
                Token::IntLiteral(0),
                Token::LBrace,
                Token::IntLiteral(1),
                Token::RBrace,
                Token::Colon,
                Token::LBrace,
                Token::Identifier("n".to_string()),
                Token::Star,
                Token::Identifier("factorial".to_string()),
                Token::LParen,
                Token::Identifier("n".to_string()),
                Token::Minus,
                Token::IntLiteral(1),
                Token::RParen,
                Token::RBrace,
            ],
        );
    }

    #[test]
    fn procedure() {
        assert_token_stream(
            r#"proc max(int.. arr) -> int {
	arr -> reduce acc, elem {
		? acc > elem {
			 acc
		} : {
			elem
		}
	}
}
"#,
            vec![
                Token::Proc,
                Token::Identifier("max".to_string()),
                Token::LParen,
                Token::Identifier("int".to_string()),
                Token::DotDot,
                Token::Identifier("arr".to_string()),
                Token::RParen,
                Token::Arrow,
                Token::Identifier("int".to_string()),
                Token::LBrace,
                Token::Identifier("arr".to_string()),
                Token::Arrow,
                Token::Identifier("reduce".to_string()),
                Token::Identifier("acc".to_string()),
                Token::Comma,
                Token::Identifier("elem".to_string()),
                Token::LBrace,
                Token::Question,
                Token::Identifier("acc".to_string()),
                Token::Greater,
                Token::Identifier("elem".to_string()),
                Token::LBrace,
                Token::Identifier("acc".to_string()),
                Token::RBrace,
                Token::Colon,
                Token::LBrace,
                Token::Identifier("elem".to_string()),
                Token::RBrace,
                Token::RBrace,
                Token::RBrace,
            ],
        );
    }

    #[test]
    fn arrays() {
        assert_token_stream(
            "int.. a = [1, 2, 3]",
            vec![
                Token::Identifier("int".to_string()),
                Token::DotDot,
                Token::Identifier("a".to_string()),
                Token::Assign,
                Token::LBracket,
                Token::IntLiteral(1),
                Token::Comma,
                Token::IntLiteral(2),
                Token::Comma,
                Token::IntLiteral(3),
                Token::RBracket,
            ],
        );
    }

    #[test]
    fn multi_conditions() {
        assert_token_stream(
            r#"? n == 0 or n == 1"#,
            vec![
                Token::Question,
                Token::Identifier("n".to_string()),
                Token::Equality,
                Token::IntLiteral(0),
                Token::Or,
                Token::Identifier("n".to_string()),
                Token::Equality,
                Token::IntLiteral(1),
            ],
        );

        assert_token_stream(
            r#"? n >= 0 and m <> 1 {"#,
            vec![
                Token::Question,
                Token::Identifier("n".to_string()),
                Token::GreaterEqual,
                Token::IntLiteral(0),
                Token::And,
                Token::Identifier("m".to_string()),
                Token::NotEqual,
                Token::IntLiteral(1),
                Token::LBrace,
            ],
        );
    }

    #[test]
    fn bools() {
        assert_token_stream(
            r#"? true and false"#,
            vec![
                Token::Question,
                Token::BoolLiteral(true),
                Token::And,
                Token::BoolLiteral(false),
            ],
        );
    }
}
