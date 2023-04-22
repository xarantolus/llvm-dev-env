use core::fmt;
use std::error::Error;

use crate::lexer::lex::{LexError, Token};

#[derive(Debug, PartialEq, Clone)]
pub enum ParseErrorType {
    InvalidToken(Token, Vec<Token>),
    LexError(LexError),
}

impl From<LexError> for ParseError {
    fn from(error: LexError) -> Self {
        let line = error.line;
        let column = error.column;
        Self {
            error_type: ParseErrorType::LexError(error),
            line,
            column,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ParseError {
    pub error_type: ParseErrorType,
    pub line: usize,
    pub column: usize,
}

impl ParseError {
    fn new(error_type: ParseErrorType, input: &str, pos: usize) -> Self {
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

impl Error for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.error_type {
            ParseErrorType::InvalidToken(ref token, ref expected) => write!(
                f,
                "Invalid token {:?} at line {} column {}. Expected one of {:?}",
                token, self.line, self.column, expected
            ),
            ParseErrorType::LexError(ref error) => write!(
                f,
                "Lex error {:?} at line {} column {}",
                error, self.line, self.column
            ),
        }
    }
}

pub struct Parser {
    input_file: String,
    current_position: usize,
}

impl Parser {
    pub fn new(input: String) -> Parser {
        Self {
            input_file: input,
            current_position: 0,
        }
    }

    pub fn next_token(&mut self, expected_tokens: Vec<Token>) -> Result<Token, ParseError> {
        let (token, pos) = Token::lex(&self.input_file, self.current_position)?;

        // Make sure the Token is one of the expected ones. If expected_tokens includes something like a number, we ignore the numerical value
        let valid_token = expected_tokens.iter().any(|t| {
            *t == token
                || match t.clone() {
                    Token::Number(_) => matches!(token, Token::Number(_)),
                    Token::String(_) => matches!(token, Token::String(_)),
                    Token::Identifier(_) => matches!(token, Token::Identifier(_)),
                    _ => false,
                }
        });

        if !valid_token {
            // Skip whitespace the same way the lexer does -- otherwise we would get weird positions in the error message
            let mut column = self.current_position;
			// FIXME: this is super inefficient, especially for larger files (as we re-parse the unicode chars every iteration)
            while column < self.input_file.len() && self.input_file.chars().nth(column).unwrap().is_whitespace() {
                column += 1;
            }

            return Err(ParseError::new(
                ParseErrorType::InvalidToken(token.clone(), expected_tokens),
                &self.input_file,
                column,
            ));
        }

        self.current_position = pos;

        Ok(token)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn next_token() {
        let mut parser = Parser::new("1 + 2".to_string());

        assert_eq!(
            parser.next_token(vec![Token::Number(0)]),
            Ok(Token::Number(1))
        );
        assert_eq!(
            parser.next_token(vec![Token::Minus, Token::Plus]),
            Ok(Token::Plus)
        );
        assert_eq!(
            parser.next_token(vec![Token::Number(0)]),
            Ok(Token::Number(2))
        );
    }

    #[test]
    fn next_token_fail() {
        let mut parser = Parser::new("1 + 2".to_string());

        assert_eq!(
            parser.next_token(vec![Token::Plus]),
            Err(ParseError::new(
                ParseErrorType::InvalidToken(Token::Number(1), vec![Token::Plus]),
                "1 + 2",
                0
            ))
        );

        parser = Parser::new("1 + 2".to_string());

        assert_eq!(
            parser.next_token(vec![Token::Number(0)]),
            Ok(Token::Number(1))
        );
        assert_eq!(
            parser.next_token(vec![Token::Minus]),
            Err(ParseError::new(
                ParseErrorType::InvalidToken(Token::Plus, vec![Token::Minus]),
                "1 + 2",
                2
            ))
        );
    }
}
