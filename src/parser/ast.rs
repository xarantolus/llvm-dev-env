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

impl TryFrom<&str> for BuiltinType {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "int" => Ok(Self::Int),
            "string" => Ok(Self::String),
            "bool" => Ok(Self::Bool),
            "void" => Ok(Self::Void),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,

    And,
    Or,

    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

impl TryFrom<&Token> for BinOp {
    type Error = ();

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value {
            Token::Plus => Ok(Self::Plus),
            Token::Minus => Ok(Self::Minus),
            Token::Star => Ok(Self::Multiply),
            Token::Slash => Ok(Self::Divide),
            Token::Percent => Ok(Self::Modulo),

            Token::And => Ok(Self::And),
            Token::Or => Ok(Self::Or),

            Token::Equality => Ok(Self::Equal),
            Token::NotEqual => Ok(Self::NotEqual),
            Token::Less => Ok(Self::LessThan),
            Token::LessEqual => Ok(Self::LessThanOrEqual),
            Token::Greater => Ok(Self::GreaterThan),
            Token::GreaterEqual => Ok(Self::GreaterThanOrEqual),
            _ => Err(()),
        }
    }
}

impl BinOp {
    pub fn precedence(&self) -> u8 {
        match self {
            Self::Or => 1,
            Self::And => 2,
            Self::Equal | Self::NotEqual => 3,
            Self::LessThan
            | Self::LessThanOrEqual
            | Self::GreaterThan
            | Self::GreaterThanOrEqual => 4,
            Self::Plus | Self::Minus => 5,
            Self::Multiply | Self::Divide | Self::Modulo => 6,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    StringLiteral(String),
    IntLiteral(i64),
    BoolLiteral(bool),
    ArrayLiteral(Vec<Expr>),

    ArrayAccess(Box<Expr>, Box<Expr>),
    VariableAccess(String),
    FunctionCall(String, Vec<Expr>),

    BinOp(Box<Expr>, BinOp, Box<Expr>),

    If(Box<Expr>, Box<Expr>, Box<Expr>),
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
    pub return_type: String,
    pub parameters: Vec<Parameter>,

    // Invariant: the last statement must be an expression returning a value of return_type
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub type_name: String,
    pub is_array: bool,
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

const TOK_IDENTIFIER: Token = Token::Identifier(String::new());
const TOK_INT: Token = Token::IntLiteral(0);
const TOK_STRING: Token = Token::StringLiteral(String::new());
const TOK_BOOL: Token = Token::BoolLiteral(false);

impl Program {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            global_variables: Vec::new(),
        }
    }

    pub fn parse(&mut self, file: String) -> Result<Program, ParseError> {
        let mut parser = Parser::new(file.clone());

        let top_lvl_tokens = vec![Token::Identifier("".to_string()), Token::Proc];

        loop {
            // basically parse top-level statements
            // Currently only proc and global variables are supported, "need" and "include" are not
            match parser.peek_token(top_lvl_tokens.clone())? {
                Token::Identifier(_) => {
                    // Basically this must be a type assignment, so parse that...
                    todo!("Parse assignment");
                }
                Token::Proc => {
                    // Parse function
                    let function = Self::parse_function(&mut parser)?;
                    self.functions.push(function);
                }
                other => {
                    // next_token wrong implementation
                    panic!("Unexpected token: {:?}", other);
                }
            }
        }
    }

    fn parse_function(parser: &mut Parser) -> Result<Function, ParseError> {
        // First is always a "proc" token
        parser.skip_token(Token::Proc)?;

        let Token::Identifier(function_name) = parser.next_token(vec![TOK_IDENTIFIER])? else {
            panic!("next_token returned something other than an identifier");
        };

        parser.skip_token(Token::LParen)?;

        // Parse parameters
        let mut parameters: Vec<Parameter> = Vec::new();
        loop {
            let Token::Identifier(param_type) = parser.next_token(vec![TOK_IDENTIFIER, Token::RParen])? else {
                // Parameter list was closed
                break;
            };
            match parser.next_token(vec![TOK_IDENTIFIER, Token::DotDot])? {
                Token::Identifier(param_name) => {
                    parameters.push(Parameter {
                        name: param_name,
                        type_name: param_type,
                        is_array: false,
                    });
                }
                Token::DotDot => {
                    let Token::Identifier(param_name) = parser.next_token(vec![TOK_IDENTIFIER])? else {
                        panic!("next_token returned something other than an identifier");
                    };
                    parameters.push(Parameter {
                        name: param_name,
                        type_name: param_type,
                        is_array: true,
                    });
                }
                _ => {
                    panic!("next_token returned something other than an identifier or dotdot");
                }
            }
        }

        // Now maybe an arrow + return type OR an opening brace that starts the body
        let return_type = {
            if let Token::Arrow = parser.next_token(vec![Token::Arrow, Token::LBrace])? {
                let Token::Identifier(return_type) = parser.next_token(vec![TOK_IDENTIFIER])? else {
                    panic!("next_token returned something other than an identifier");
                };
                parser.skip_token(Token::LBrace)?;

                Some(return_type)
            } else {
                None
            }
        };

        let body = Self::parse_block(parser)?;

        Ok(Function {
            name: function_name,
            return_type: return_type.unwrap_or("void".to_string()),
            parameters,
            body,
        })
    }

    fn parse_block(parser: &mut Parser) -> Result<Vec<Stmt>, ParseError> {
        let mut statements: Vec<Stmt> = Vec::new();

        // TODO: Parse statements. Note that the last part of a block may be an expression

        todo!("Parse block");
    }

    fn parse_expression(parser: &mut Parser) -> Result<Expr, ParseError> {
        let first_expr = Program::parse_simple_expression(parser)?;

        // Maybe we have some binary or post operator after this expression
        let binop_tokens = vec![
            Token::LParen,
            Token::At,
            Token::Plus,
            Token::Minus,
            Token::Star,
            Token::Slash,
            Token::Percent,
            Token::And,
            Token::Or,
            Token::Equality,
            Token::NotEqual,
            Token::Less,
            Token::LessEqual,
            Token::Greater,
            Token::GreaterEqual,
        ];
        match parser.next_token(binop_tokens.clone()) {
            Err(_) => {
                // Following stuff, so just return the expression
                return Ok(first_expr);
            }
            Ok(Token::LParen) => {
                // Function calls
                let Expr::VariableAccess(function_name) = first_expr else {
                    return Err(ParseError::new(
                        ParseErrorType::UnknownPrefix {
                            prefix: first_expr,
                            current: Token::LParen,
                            expected: vec![Expr::VariableAccess(String::new())],
                        },
                        &parser.input_file,
                        parser.current_position,
                    ));
                };

                // Parse argument expressions separated by comma
                let mut arguments: Vec<Expr> = Vec::new();
                loop {
                    let argument = Self::parse_expression(parser)?;
                    arguments.push(argument);

                    match parser.next_token(vec![Token::Comma, Token::RParen])? {
                        Token::Comma => {
                            continue;
                        }
                        Token::RParen => {
                            // End of argument list
                            break;
                        }
                        _ => {
                            // next_token wrong implementation
                            panic!("Unexpected token");
                        }
                    }
                }

                Ok(Expr::FunctionCall(function_name, arguments))
            }
            Ok(Token::At) => {
                let index = Self::parse_expression(parser)?;

                Ok(Expr::ArrayAccess(Box::new(first_expr), Box::new(index)))
            }
            Ok(binary_operator_token) => {
                // Binary operators
                let Ok(first_binop_type) = BinOp::try_from(&binary_operator_token) else {
                    // Apparently not a binop
                    return Err(ParseError::new(
                        ParseErrorType::InvalidToken(binary_operator_token, binop_tokens),
                        &parser.input_file,
                        parser.current_position,
                    ));
                };

                // Now parse the other part of the expression
                let second_expr = Self::parse_expression(parser)?;

                // Now we have a problem: The second expression could also be a binary operator
                // but have different precedence. So we need to reorganize the tree
                // This e.g. happens for 1 * 2 + 3

                if let Expr::BinOp(second_left, second_binop_type, second_right) =
                    second_expr.clone()
                {
                    // It's actually a correct binop type
                    if let Ok(second_binop) = BinOp::try_from(second_binop_type.clone()) {
                        // And the first binop has higher precedence than the second one
                        if first_binop_type.precedence() > second_binop.precedence() {
                            // So we need to reorganize the tree
                            return Ok(Expr::BinOp(
                                Box::new(Expr::BinOp(
                                    Box::new(first_expr),
                                    first_binop_type,
                                    second_left,
                                )),
                                second_binop_type,
                                second_right,
                            ));
                        }
                    }
                }

                Ok(Expr::BinOp(
                    Box::new(first_expr),
                    first_binop_type,
                    Box::new(second_expr),
                ))
            }
            _ => {
                // next_token wrong implementation
                panic!("Unexpected token");
            }
        }
    }

    fn parse_simple_expression(parser: &mut Parser) -> Result<Expr, ParseError> {
        Ok(
            match parser.next_token(vec![
                TOK_INT,
                TOK_STRING,
                TOK_BOOL,
                TOK_IDENTIFIER,
                Token::LParen,
                Token::LBracket,
                Token::LBrace,
            ])? {
                Token::IntLiteral(value) => Expr::IntLiteral(value),
                Token::StringLiteral(value) => Expr::StringLiteral(value),
                Token::BoolLiteral(value) => Expr::BoolLiteral(value),
                Token::Identifier(name) => Expr::VariableAccess(name),
                Token::LParen => {
                    let expr = Self::parse_expression(parser)?;
                    parser.skip_token(Token::RParen)?;
                    expr
                }
                Token::LBrace => {
                    let expr = Self::parse_expression(parser)?;
                    parser.skip_token(Token::RBrace)?;
                    expr
                }
                Token::LBracket => {
                    // Array literal
                    let mut elements: Vec<Expr> = Vec::new();
                    loop {
                        elements.push(Self::parse_expression(parser)?);
                        match parser.next_token(vec![Token::Comma, Token::RBracket])? {
                            Token::Comma => {}
                            Token::RBracket => {
                                break;
                            }
                            _ => {
                                panic!(
                                    "next_token returned something other than a comma or rbracket"
                                );
                            }
                        }
                    }

                    Expr::ArrayLiteral(elements)
                }
                tok => {
                    // next_token wrong implementation
                    panic!("Unexpected token {:?}", tok);
                }
            },
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hello_world() {
        let file = include_str!("../../testdata/hello-world.l").to_string();
        let mut program = Program::new();
        program.parse(file).expect("Failed to parse program");
        println!("{:#?}", program);
    }

    fn assert_ast(file: &str, expected: Program) {
        let mut program = Program::new();
        program
            .parse(file.to_string())
            .expect("Failed to parse program");
        assert_eq!(
            program, expected,
            "Expected AST:\n{:#?}\nGot AST: {:#?}",
            expected, program
        );
    }

    #[test]
    fn simple_function() {
        assert_ast(
            r#"proc main() -> int {
                0
            }"#,
            Program {
                functions: vec![Function {
                    name: "main".to_string(),
                    return_type: "int".to_string(),
                    parameters: vec![],
                    body: vec![Stmt::Expression(Expr::IntLiteral(0))],
                }],
                global_variables: vec![],
            },
        )
    }

    #[test]
    fn expr() {
        let mut program = Parser::new(r#"f(1, x)"#.to_string());
        let expr = Program::parse_expression(&mut program).unwrap();
        assert_eq!(
            expr,
            Expr::FunctionCall(
                "f".to_string(),
                vec![Expr::IntLiteral(1), Expr::VariableAccess("x".to_string())]
            )
        );

        let mut program = Parser::new(r#"1 + 2"#.to_string());
        let expr = Program::parse_expression(&mut program).unwrap();
        assert_eq!(
            expr,
            Expr::BinOp(
                Box::new(Expr::IntLiteral(1)),
                BinOp::Plus,
                Box::new(Expr::IntLiteral(2))
            )
        );

        // Check binop order
        let mut program = Parser::new(r#"1 + 2 * 3"#.to_string());
        let expr = Program::parse_expression(&mut program).unwrap();
        assert_eq!(
            expr,
            Expr::BinOp(
                Box::new(Expr::IntLiteral(1)),
                BinOp::Plus,
                Box::new(Expr::BinOp(
                    Box::new(Expr::IntLiteral(2)),
                    BinOp::Multiply,
                    Box::new(Expr::IntLiteral(3))
                ))
            )
        );

        // Same for other direction
        let mut program = Parser::new(r#"1 * 2 + 3"#.to_string());
        let expr = Program::parse_expression(&mut program).unwrap();
        assert_eq!(
            expr,
            Expr::BinOp(
                Box::new(Expr::BinOp(
                    Box::new(Expr::IntLiteral(1)),
                    BinOp::Multiply,
                    Box::new(Expr::IntLiteral(2))
                )),
                BinOp::Plus,
                Box::new(Expr::IntLiteral(3))
            )
        );

        // Array indexing
        let mut program = Parser::new(r#"arr @ 0"#.to_string());
        let expr = Program::parse_expression(&mut program).unwrap();
        assert_eq!(
            expr,
            Expr::ArrayAccess(
                Box::new(Expr::VariableAccess("arr".to_string())),
                Box::new(Expr::IntLiteral(0))
            )
        );

        // Array indexing with expression
        let mut program = Parser::new(r#"arr @ 0 + 1"#.to_string());
        let expr = Program::parse_expression(&mut program).unwrap();
        assert_eq!(
            expr,
            Expr::ArrayAccess(
                Box::new(Expr::VariableAccess("arr".to_string())),
                Box::new(Expr::BinOp(
                    Box::new(Expr::IntLiteral(0)),
                    BinOp::Plus,
                    Box::new(Expr::IntLiteral(1))
                ))
            )
        );

        let mut program = Parser::new(r#"arr @ arr @ 0"#.to_string());
        let expr = Program::parse_expression(&mut program).unwrap();
        assert_eq!(
            expr,
            Expr::ArrayAccess(
                Box::new(Expr::VariableAccess("arr".to_string())),
                Box::new(Expr::ArrayAccess(
                    Box::new(Expr::VariableAccess("arr".to_string())),
                    Box::new(Expr::IntLiteral(0))
                ))
            )
        );

        let mut program = Parser::new(r#"1 <= 2"#.to_string());
        let expr = Program::parse_expression(&mut program).unwrap();
        assert_eq!(
            expr,
            Expr::BinOp(
                Box::new(Expr::IntLiteral(1)),
                BinOp::LessThanOrEqual,
                Box::new(Expr::IntLiteral(2))
            )
        );
    }
}
