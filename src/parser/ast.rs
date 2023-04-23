use super::parse::{ParseError, ParseErrorType, Parser};
use crate::lexer::lex::Token;
use std::vec;

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
            _ => {
                if value.ends_with("..") {
                    let inner_type = value[..value.len() - 2].try_into()?;
                    Ok(Self::Array(Box::new(inner_type)))
                } else {
                    Err(())
                }
            }
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

    If(Box<Expr>, Stmt, Stmt),
}

impl Expr {
    fn is_assignable(&self) -> bool {
        match self {
            Expr::VariableAccess(_) => true,
            Expr::ArrayAccess(_, _) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    VariableDeclaration {
        type_name: String,
        var_name: String,
        expr: Box<Expr>,
    },
    Assignment {
        to: Box<Expr>,
        expr: Box<Expr>,
    },
    Block(Vec<Stmt>),
    // ExprStmt are only allowed as the last statement in a block
    ExprStmt(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub return_type: String,
    pub parameters: Vec<Parameter>,

    // Invariant: the last statement must be an expression returning a value of return_type
    pub body: Stmt,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub type_name: String,
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

        let top_lvl_tokens = vec![Token::Proc, Token::EOF];

        loop {
            // basically parse top-level statements
            // Currently only proc is supported, "need" and "include" are not
            match parser.peek_token(top_lvl_tokens.clone())? {
                Token::Proc => {
                    // Parse function
                    let function = Self::parse_function(&mut parser)?;
                    self.functions.push(function);
                }
                Token::EOF => {
                    // End of file
                    break;
                }
                _ => {
                    panic!("peek_token returned something other than an identifier, proc or EOF")
                }
            }
        }

        Ok(self.clone())
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
            match parser.next_token(vec![TOK_IDENTIFIER])? {
                Token::Identifier(param_name) => {
                    parameters.push(Parameter {
                        name: param_name,
                        type_name: param_type,
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
                Some(return_type)
            } else {
                None
            }
        };

        let body = Self::parse_block(parser, false)?;

        Ok(Function {
            name: function_name,
            return_type: return_type.unwrap_or("void".to_string()),
            parameters,
            body,
        })
    }

    fn parse_block(parser: &mut Parser, in_opening_brace: bool) -> Result<Stmt, ParseError> {
        let mut statements: Vec<Stmt> = Vec::new();

        if !in_opening_brace {
            parser.skip_token(Token::LBrace)?;
        }

        loop {
            // Speculatively see if we have an expression statement -- this would be last thing in the block
            let previous_pos = parser.current_position;
            let mut had_expr = false;
            if let Ok(expr) = Program::parse_expression(parser) {
                statements.push(Stmt::ExprStmt(Box::new(expr)));

                // Skip a semicolon if it's there.
                // If there's no semicolon, we might reuse the expression or leave it as the last thing in the block
                match parser.skip_token(Token::Semicolon) {
                    Err(_) => {
                        // No semicolon, so we might reuse the expression (e.g. for assignments a bit further down)
                        had_expr = true;
                    }
                    Ok(_) => {
                        continue;
                    }
                }
            } else {
                parser.rewind(previous_pos)
            };

            match parser.next_token(vec![
                // identifier for variable declaration e.g. "int x = 5"
                // OR variable assignment, like "x = 5"
                TOK_IDENTIFIER,
                Token::Assign,
                Token::RBrace,
                Token::LBrace,
            ])? {
                // Normal variable assignment - the variable is the previous expression
                Token::Assign => {
                    if !had_expr {
                        return Err(ParseError::new(
                            ParseErrorType::ExpectedAssignableExpression { prefix: None },
                            &parser.input_file,
                            parser.current_position,
                        ));
                    }

                    // Make sure the last expression is assignable
                    let Some(Stmt::ExprStmt(left_expr)) = statements.pop() else {
                        panic!("Expected last statement to be an expression statement");
                    };
                    if !left_expr.is_assignable() {
                        return Err(ParseError::new(
                            ParseErrorType::ExpectedAssignableExpression {
                                prefix: Some(*left_expr),
                            },
                            &parser.input_file,
                            parser.current_position,
                        ));
                    }

                    // OK, so this works. Now parse the expression on the right side of the assignment
                    parser.skip_token(Token::Assign)?;
                    let right_expr = Program::parse_expression(parser)?;
                    statements.push(Stmt::Assignment {
                        to: left_expr,
                        expr: Box::new(right_expr),
                    });

                    // Make sure the next token is a semicolon
                    parser.skip_token(Token::Semicolon)?;
                }
                // Variable declaration -- previous expr is the type, then name, assignment etc.
                Token::Identifier(var_name) => {
                    let Some(Stmt::ExprStmt(type_expr)) = statements.pop() else {
                            panic!("Expected last statement to be an expression statement");
                    };
                    if !had_expr {
                        return Err(ParseError::new(
                            ParseErrorType::ExpectedType { prefix: *type_expr },
                            &parser.input_file,
                            parser.current_position,
                        ));
                    }
                    let Expr::VariableAccess(type_name) = *type_expr  else {
                        return Err(ParseError::new(
                            ParseErrorType::ExpectedType { prefix: *type_expr },
                            &parser.input_file,
                            parser.current_position,
                        ));
                    };

                    parser.skip_token(Token::Assign)?;

                    let right_expr = Program::parse_expression(parser)?;
                    statements.push(Stmt::VariableDeclaration {
                        type_name,
                        var_name,
                        expr: Box::new(right_expr),
                    });

                    // Make sure the next token is a semicolon
                    parser.skip_token(Token::Semicolon)?;
                }
                Token::RBrace => {
                    // End of block
                    break;
                }
                Token::LBrace => {
                    // Nested block
                    statements.push(Self::parse_block(parser, false)?);
                }
                _ => {
                    todo!()
                }
            }
        }

        // Make sure the last statement is an expression statement

        Ok(Stmt::Block(statements))
    }

    fn parse_expression(parser: &mut Parser) -> Result<Expr, ParseError> {
        let first_expr = Program::parse_simple_expression(parser)?;

        // Maybe we have some binary or post operator after this expression
        let binop_tokens = vec![
            Token::LParen,
            Token::Colon,
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
                Token::Question,
            ])? {
                Token::IntLiteral(value) => Expr::IntLiteral(value),
                Token::StringLiteral(value) => Expr::StringLiteral(value),
                Token::BoolLiteral(value) => Expr::BoolLiteral(value),
                Token::Identifier(name) => {
                    let expr = Expr::VariableAccess(name);
                    if let Ok(_) = parser.peek_token(vec![Token::At]) {
                        // Array access
                        parser.skip_token(Token::At)?;
                        let index = Self::parse_expression(parser)?;

                        Expr::ArrayAccess(Box::new(expr), Box::new(index))
                    } else {
                        expr
                    }
                }
                Token::LParen => {
                    let expr = Self::parse_expression(parser)?;
                    parser.skip_token(Token::RParen)?;
                    expr
                }
                Token::Question => {
                    let condition = Self::parse_expression(parser)?;
                    let true_branch = Self::parse_block(parser, false)?;
                    parser.skip_token(Token::Colon)?;
                    let false_branch = Self::parse_block(parser, false)?;
                    Expr::If(Box::new(condition), true_branch, false_branch)
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
                    body: Stmt::Block(vec![Stmt::ExprStmt(Box::new(Expr::IntLiteral(0)))]),
                }],
                global_variables: vec![],
            },
        )
    }

    #[test]
    fn function_call() {
        let mut program = Program::new();
        let stmt = program
            .parse(
                r#"
            proc main() -> int {
                print("Hello World");
            }"#
                .to_string(),
            )
            .unwrap();

        assert_eq!(
            stmt,
            Program {
                functions: vec![Function {
                    name: "main".to_string(),
                    return_type: "int".to_string(),
                    parameters: vec![],
                    body: Stmt::Block(vec![Stmt::ExprStmt(Box::new(Expr::FunctionCall(
                        "print".to_string(),
                        vec![Expr::StringLiteral("Hello World".to_string())]
                    )))]),
                }],
                global_variables: vec![],
            }
        );
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

        let mut program = Parser::new(
            r#"? a == b {
            1
        } : {
            2
        }"#
            .to_string(),
        );
        let expr = Program::parse_expression(&mut program).unwrap();
        assert_eq!(
            expr,
            Expr::If(
                Box::new(Expr::BinOp(
                    Box::new(Expr::VariableAccess("a".to_string())),
                    BinOp::Equal,
                    Box::new(Expr::VariableAccess("b".to_string()))
                )),
                Stmt::Block(vec![Stmt::ExprStmt(Box::new(Expr::IntLiteral(1)))]),
                Stmt::Block(vec![Stmt::ExprStmt(Box::new(Expr::IntLiteral(2)))])
            )
        );
    }

    #[test]
    fn blocks() {
        let mut program = Parser::new(r#"{ int a = 5; a }"#.to_string());
        let block = Program::parse_block(&mut program, false).unwrap();
        assert_eq!(
            block,
            Stmt::Block(vec![
                Stmt::VariableDeclaration {
                    var_name: "a".to_string(),
                    type_name: "int".to_string(),
                    expr: Box::new(Expr::IntLiteral(5)),
                },
                Stmt::ExprStmt(Box::new(Expr::VariableAccess("a".to_string())))
            ])
        );

        let mut program = Parser::new(r#"{ int.. a = [5]; a }"#.to_string());
        let block = Program::parse_block(&mut program, false).unwrap();
        assert_eq!(
            block,
            Stmt::Block(vec![
                Stmt::VariableDeclaration {
                    var_name: "a".to_string(),
                    type_name: "int..".to_string(),
                    expr: Box::new(Expr::ArrayLiteral(vec![Expr::IntLiteral(5)])),
                },
                Stmt::ExprStmt(Box::new(Expr::VariableAccess("a".to_string())))
            ])
        );

        let mut program = Parser::new(r#"{ int a = 5; int.. b = [7]; (b @ 0) + a }"#.to_string());
        let block = Program::parse_block(&mut program, false).unwrap();
        assert_eq!(
            block,
            Stmt::Block(vec![
                Stmt::VariableDeclaration {
                    var_name: "a".to_string(),
                    type_name: "int".to_string(),
                    expr: Box::new(Expr::IntLiteral(5)),
                },
                Stmt::VariableDeclaration {
                    var_name: "b".to_string(),
                    type_name: "int..".to_string(),
                    expr: Box::new(Expr::ArrayLiteral(vec![Expr::IntLiteral(7)])),
                },
                Stmt::ExprStmt(Box::new(Expr::BinOp(
                    Box::new(Expr::ArrayAccess(
                        Box::new(Expr::VariableAccess("b".to_string())),
                        Box::new(Expr::IntLiteral(0))
                    )),
                    BinOp::Plus,
                    Box::new(Expr::VariableAccess("a".to_string()))
                )))
            ])
        );
    }
}
