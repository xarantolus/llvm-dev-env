use core::fmt;
use std::{error::Error, fmt::Debug};

use crate::lexer::lex::{LexError, Token};

use super::util;

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

#[derive(PartialEq, Clone)]
pub struct ParseError {
    pub error_type: ParseErrorType,
    pub line: usize,
    pub column: usize,
}

impl ParseError {
    pub(crate) fn new(error_type: ParseErrorType, input: &str, pos: usize) -> Self {
        let (line, column) = util::get_line_and_column(input, pos);
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

impl Debug for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

pub struct Parser {
    input_file: String,
    pub(crate) current_position: usize,
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
        let mut valid_token = expected_tokens.is_empty();
        if !valid_token {
            for t in expected_tokens.iter() {
                if *t == token
                    || match t.clone() {
                        Token::IntLiteral(_) => matches!(token, Token::IntLiteral(_)),
                        Token::StringLiteral(_) => matches!(token, Token::StringLiteral(_)),
                        Token::BoolLiteral(_) => matches!(token, Token::BoolLiteral(_)),
                        Token::Identifier(_) => matches!(token, Token::Identifier(_)),
                        _ => false,
                    }
                {
                    valid_token = true;
                    break;
                }
            }
        }

        if !valid_token {
            // Skip whitespace the same way the lexer does -- otherwise we would get weird positions in the error message
            let mut pos_in_file = self.current_position;
            // FIXME: this is super inefficient, especially for larger files (as we re-parse the unicode chars every iteration)
            while pos_in_file < self.input_file.len()
                && self
                    .input_file
                    .chars()
                    .nth(pos_in_file)
                    .unwrap()
                    .is_whitespace()
            {
                pos_in_file += 1;
            }

            return Err(ParseError::new(
                ParseErrorType::InvalidToken(token.clone(), expected_tokens),
                &self.input_file,
                pos_in_file,
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
            parser.next_token(vec![Token::IntLiteral(0)]),
            Ok(Token::IntLiteral(1))
        );
        assert_eq!(
            parser.next_token(vec![Token::Minus, Token::Plus]),
            Ok(Token::Plus)
        );
        assert_eq!(
            parser.next_token(vec![Token::IntLiteral(0)]),
            Ok(Token::IntLiteral(2))
        );
    }

    #[test]
    fn next_token_fail() {
        let mut parser = Parser::new("1 + 2".to_string());

        assert_eq!(
            parser.next_token(vec![Token::Plus]),
            Err(ParseError::new(
                ParseErrorType::InvalidToken(Token::IntLiteral(1), vec![Token::Plus]),
                "1 + 2",
                0
            ))
        );

        parser = Parser::new("1 + 2".to_string());

        assert_eq!(
            parser.next_token(vec![Token::IntLiteral(0)]),
            Ok(Token::IntLiteral(1))
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
