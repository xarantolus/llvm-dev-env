use super::parse::{ParseError, ParseErrorType, Parser};
use crate::{lexer::lex::Token, parser::util};
use std::{fs::File, vec};

#[derive(Debug, Clone, PartialEq)]
pub enum BuiltinType {
    Int,
    String,
    Bool,
    Void,
    Array(Box<BuiltinType>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Bracketed(Box<Expr>),

    StringLiteral(String),
    IntLiteral(i64),
    BoolLiteral(bool),
    ArrayLiteral(Vec<Expr>),

    ArrayAccess(Box<Expr>, Box<Expr>),
    VariableAccess(String),
    FunctionCall(String, Vec<Expr>),

    Plus(Box<Expr>, Box<Expr>),
    Minus(Box<Expr>, Box<Expr>),
    Multiply(Box<Expr>, Box<Expr>),
    Divide(Box<Expr>, Box<Expr>),
    Modulo(Box<Expr>, Box<Expr>),

    If(Box<Expr>, Box<Expr>, Box<Expr>),

    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Not(Box<Expr>),

    Equal(Box<Expr>, Box<Expr>),
    NotEqual(Box<Expr>, Box<Expr>),
    LessThan(Box<Expr>, Box<Expr>),
    LessThanOrEqual(Box<Expr>, Box<Expr>),
    GreaterThan(Box<Expr>, Box<Expr>),
    GreaterThanOrEqual(Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    ExprStmt(Expr),
    VariableDeclaration(BuiltinType, String, Expr),
    VariableAssignment(String, Expr),
    BlockStmt(Vec<Stmt>),
    // An expression that returns a value, but is not assigned to a variable
    Expression(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub return_type: BuiltinType,
    pub parameters: Vec<(BuiltinType, String)>,

    // Invariant: the last statement must be an expression returning a value of return_type
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GlobalVariable {
    pub name: String,
    // Must be a constant expression, so a literal
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    // Functions must include a main function and function names must be unique
    // TODO: Maybe make functions names non-unique, but only function signatures?
    // This would allow for something like str::from(array | bool | ...)
    pub functions: Vec<Function>,
    pub global_variables: Vec<GlobalVariable>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            global_variables: Vec::new(),
        }
    }

    pub fn parse(file: String) -> Result<Program, ParseError> {
        let mut parser = Parser::new(file.clone());

        let top_lvl_tokens = vec![Token::Identifier("".to_string()), Token::Proc];

        loop {
            // basically parse top-level statements
            // Currently only proc and global variables are supported, "need" and "include" are not
            match parser.next_token(top_lvl_tokens.clone())? {
                Token::Identifier(name) => {
                    // Basically this must be a type assignment, so parse that...
                    todo!("Parse assignment");
                }
                Token::Proc => {
                    // Parse a function
                    todo!("Parse function");
                }
                other => {
                    // next_token wrong implementation
                    panic!("Unexpected token: {:?}", other);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hello_world() {
        let file = include_str!("../../testdata/hello-world.l").to_string();
        let program = Program::parse(file).expect("Failed to parse program");
        println!("{:#?}", program);
    }
}
