use std::fmt::Debug;
use std::{error::Error, fmt, num::ParseIntError};

use crate::parser::ast::Expr;

use super::{token::Token, util};

#[derive(Debug, PartialEq, Clone)]
pub enum ParseErrorType {
    UnknownPrefix {
        prefix: Expr,
        current: Token,
        expected: Vec<Expr>,
    },
    StatementAfterExpressionInBlock {
        prefix: Expr,
    },
    ExpectedType {
        prefix: Expr,
    },
    ExpectedAssignableExpression {
        prefix: Option<Expr>,
    },
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
            ParseErrorType::UnknownPrefix {
                ref prefix,
                ref current,
                ref expected,
            } => write!(
                f,
                "Unknown prefix {:?} at line {} column {} for current token {:?}. Expected one of {:?}",
                prefix, self.line, self.column, current, expected
            ),
            ParseErrorType::ExpectedAssignableExpression { ref prefix } => write!(
                f,
                "Expected assignable expression at line {} column {}, but got {:?}",
                self.line, self.column, prefix
            ),
            ParseErrorType::StatementAfterExpressionInBlock { ref prefix } => write!(
                f,
                "Expression {:?} ends block, but found further statements at line {} column {}",
                prefix, self.line, self.column
            ),
            ParseErrorType::ExpectedType { ref prefix } => write!(
                f,
                "Expected type at line {} column {}, but got {:?}",
                self.line, self.column, prefix
            ),
        }
    }
}

impl Debug for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
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
    pub fn new(error_type: LexErrorType, input: &str, pos: usize) -> Self {
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
