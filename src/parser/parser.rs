use std::vec;

use crate::lexer::{
    error::{ParseError, ParseErrorType},
    lexer::Lexer,
    token::Token,
};

use super::ast::{BinOp, Expr, Function, Parameter, Stmt};

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    // Functions must include a main function and function names must be unique
    // TODO: Maybe make functions names non-unique, but only function signatures?
    // This would allow for something like str::from(array | bool | ...)
    pub functions: Vec<Function>,
}

const TOK_IDENTIFIER: Token = Token::Identifier(String::new());
const TOK_INT: Token = Token::IntLiteral(0);
const TOK_STRING: Token = Token::StringLiteral(String::new());
const TOK_BOOL: Token = Token::BoolLiteral(false);

impl Program {
    fn new() -> Self {
        Self {
            functions: Vec::new(),
        }
    }

    pub fn parse(file: &str) -> Result<Program, ParseError> {
        let mut program = Self::new();

        let mut parser = Lexer::new(file);

        let top_lvl_tokens = vec![Token::Proc, Token::EOF];

        loop {
            // basically parse top-level statements
            // Currently only proc is supported, "need" and "include" are not
            match parser.peek_token(top_lvl_tokens.clone())? {
                Token::Proc => {
                    // Parse function
                    let function = Self::parse_function(&mut parser)?;
                    program.functions.push(function);
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

        Ok(program)
    }

    fn parse_function(parser: &mut Lexer) -> Result<Function, ParseError> {
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
            if let Token::Arrow = parser.peek_token(vec![Token::Arrow, Token::LBrace])? {
                parser.skip_token(Token::Arrow)?;
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

    fn parse_block(parser: &mut Lexer, in_opening_brace: bool) -> Result<Stmt, ParseError> {
        let mut statements: Vec<Stmt> = Vec::new();

        if !in_opening_brace {
            parser.skip_token(Token::LBrace)?;
        }

        loop {
            // Speculatively see if we have an expression statement -- this would be last thing in the block
            let previous_pos = parser.current_position;
            let mut had_expr = false;
            if let Ok(expr) = Program::parse_expression(parser) {
                statements.push(Stmt::Expr(Box::new(expr)));

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
                    let Some(Stmt::Expr(left_expr)) = statements.pop() else {
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
                    let Some(Stmt::Expr(type_expr)) = statements.pop() else {
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

    fn parse_expression(parser: &mut Lexer) -> Result<Expr, ParseError> {
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

                Ok(Expr::FunctionCall {
                    name: function_name,
                    arguments,
                })
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

                // TODO: This might be wrong - see "a" test

                let new_expr = Expr::BinOp {
                    left: Box::new(first_expr),
                    op: first_binop_type,
                    right: Box::new(second_expr),
                    parens: false,
                };

                Ok(Expr::reorder_binops(new_expr))
            }
        }
    }

    fn parse_simple_expression(parser: &mut Lexer) -> Result<Expr, ParseError> {
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

                        Expr::ArrayAccess {
                            array: Box::new(expr),
                            index: Box::new(index),
                        }
                    } else {
                        expr
                    }
                }
                // () and {} are pretty similar for expressions, so we can handle them the same way
                tok @ (Token::LParen | Token::LBrace) => {
                    let mut expr = Self::parse_expression(parser)?;
                    if let Expr::BinOp {
                        left, op, right, ..
                    } = expr
                    {
                        // We have a parenthesized expression
                        expr = Expr::BinOp {
                            left,
                            op,
                            right,
                            parens: true,
                        }
                    }
                    parser.skip_token(if tok == Token::LParen {
                        Token::RParen
                    } else {
                        Token::RBrace
                    })?;
                    expr
                }
                Token::Question => {
                    let condition = Self::parse_expression(parser)?;
                    let true_branch = Self::parse_block(parser, false)?;
                    parser.skip_token(Token::Colon)?;
                    let false_branch = Self::parse_block(parser, false)?;
                    Expr::If {
                        condition: Box::new(condition),
                        true_block: true_branch,
                        false_block: false_branch,
                    }
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
    use pretty_assertions::assert_eq;

    #[test]
    fn hello_world() {
        let file = include_str!("../../testdata/hello-world.l");
        let program = Program::parse(file).expect("Failed to parse program");
        println!("{:#?}", program);
    }

    fn assert_ast(file: &str, expected: Program) {
        let program = Program::parse(file).expect("Failed to parse program");
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
                    body: Stmt::Block(vec![Stmt::Expr(Box::new(Expr::IntLiteral(0)))]),
                }],
            },
        )
    }

    #[test]
    fn function_call() {
        let stmt = Program::parse(
            r#"
            proc main() -> int {
                print("Hello World");
            }"#,
        )
        .unwrap();

        assert_eq!(
            stmt,
            Program {
                functions: vec![Function {
                    name: "main".to_string(),
                    return_type: "int".to_string(),
                    parameters: vec![],
                    body: Stmt::Block(vec![Stmt::Expr(Box::new(Expr::FunctionCall {
                        name: "print".to_string(),
                        arguments: vec![Expr::StringLiteral("Hello World".to_string())]
                    }))]),
                }],
            }
        );
    }

    #[test]
    fn fn_call() {
        let mut program = Lexer::new(r#"f(1, x)"#);
        let expr = Program::parse_expression(&mut program).unwrap();
        assert_eq!(
            expr,
            Expr::FunctionCall {
                name: "f".to_string(),
                arguments: vec![Expr::IntLiteral(1), Expr::VariableAccess("x".to_string())]
            }
        );
    }
    #[test]
    fn addition() {
        let mut program = Lexer::new(r#"1 + 2"#);
        let expr = Program::parse_expression(&mut program).unwrap();
        assert_eq!(
            expr,
            Expr::BinOp {
                left: Box::new(Expr::IntLiteral(1)),
                op: BinOp::Plus,
                right: Box::new(Expr::IntLiteral(2)),
                parens: false
            }
        );
    }

    #[test]
    fn binop_order_mul() {
        // Check binop order
        let mut program = Lexer::new(r#"1 + 2 * 3"#);
        let expr = Program::parse_expression(&mut program).unwrap();
        dbg!(&expr);
        assert_eq!(
            expr,
            Expr::BinOp {
                left: Box::new(Expr::IntLiteral(1)),
                op: BinOp::Plus,
                right: Box::new(Expr::BinOp {
                    left: Box::new(Expr::IntLiteral(2)),
                    op: BinOp::Multiply,
                    right: Box::new(Expr::IntLiteral(3)),
                    parens: false
                }),
                parens: false
            }
        );
    }

    #[test]
    fn binop_order_mul2() {
        // Same for other direction
        let mut program = Lexer::new(r#"1 * 2 + 3"#);
        let expr = Program::parse_expression(&mut program).unwrap();
        dbg!(&expr);
        assert_eq!(
            expr,
            Expr::BinOp {
                left: Box::new(Expr::BinOp {
                    left: Box::new(Expr::IntLiteral(1)),
                    op: BinOp::Multiply,
                    right: Box::new(Expr::IntLiteral(2)),
                    parens: false
                }),
                op: BinOp::Plus,
                right: Box::new(Expr::IntLiteral(3)),
                parens: false
            }
        );
    }

    #[test]
    fn arr_index() {
        // Array indexing
        let mut program = Lexer::new(r#"arr @ 0"#);
        let expr = Program::parse_expression(&mut program).unwrap();
        assert_eq!(
            expr,
            Expr::ArrayAccess {
                array: Box::new(Expr::VariableAccess("arr".to_string())),
                index: Box::new(Expr::IntLiteral(0))
            }
        );
    }

    #[test]
    fn arr_index_expr() {
        // Array indexing with expression
        let mut program = Lexer::new(r#"arr @ 0 + 1"#);
        let expr = Program::parse_expression(&mut program).unwrap();
        assert_eq!(
            expr,
            Expr::ArrayAccess {
                array: Box::new(Expr::VariableAccess("arr".to_string())),
                index: Box::new(Expr::BinOp {
                    left: Box::new(Expr::IntLiteral(0)),
                    op: BinOp::Plus,
                    right: Box::new(Expr::IntLiteral(1)),
                    parens: false
                })
            }
        );
    }

    #[test]
    fn arr_double_index() {
        let mut program = Lexer::new(r#"arr @ arr @ 0"#);
        let expr = Program::parse_expression(&mut program).unwrap();
        assert_eq!(
            expr,
            Expr::ArrayAccess {
                array: Box::new(Expr::VariableAccess("arr".to_string())),
                index: Box::new(Expr::ArrayAccess {
                    array: Box::new(Expr::VariableAccess("arr".to_string())),
                    index: Box::new(Expr::IntLiteral(0))
                })
            }
        );
    }

    #[test]
    fn binop_order_leq() {
        let mut program = Lexer::new(r#"1 <= 2"#);
        let expr = Program::parse_expression(&mut program).unwrap();
        assert_eq!(
            expr,
            Expr::BinOp {
                left: Box::new(Expr::IntLiteral(1)),
                op: BinOp::LessThanOrEqual,
                right: Box::new(Expr::IntLiteral(2)),
                parens: false
            }
        );
    }

    #[test]
    fn if_equals() {
        let mut program = Lexer::new(
            r#"? a == b {
            1
        } : {
            2
        }"#,
        );
        let expr = Program::parse_expression(&mut program).unwrap();
        assert_eq!(
            expr,
            Expr::If {
                condition: Box::new(Expr::BinOp {
                    left: Box::new(Expr::VariableAccess("a".to_string())),
                    op: BinOp::Equal,
                    right: Box::new(Expr::VariableAccess("b".to_string())),
                    parens: false
                }),
                true_block: Stmt::Block(vec![Stmt::Expr(Box::new(Expr::IntLiteral(1)))]),
                false_block: Stmt::Block(vec![Stmt::Expr(Box::new(Expr::IntLiteral(2)))])
            }
        );
    }

    #[test]
    fn blocks() {
        let mut program = Lexer::new(r#"{ int a = 5; a }"#);
        let block = Program::parse_block(&mut program, false).unwrap();
        assert_eq!(
            block,
            Stmt::Block(vec![
                Stmt::VariableDeclaration {
                    var_name: "a".to_string(),
                    type_name: "int".to_string(),
                    expr: Box::new(Expr::IntLiteral(5)),
                },
                Stmt::Expr(Box::new(Expr::VariableAccess("a".to_string())))
            ])
        );

        let mut program = Lexer::new(r#"{ int.. a = [5]; a }"#);
        let block = Program::parse_block(&mut program, false).unwrap();
        assert_eq!(
            block,
            Stmt::Block(vec![
                Stmt::VariableDeclaration {
                    var_name: "a".to_string(),
                    type_name: "int..".to_string(),
                    expr: Box::new(Expr::ArrayLiteral(vec![Expr::IntLiteral(5)])),
                },
                Stmt::Expr(Box::new(Expr::VariableAccess("a".to_string())))
            ])
        );

        let mut program = Lexer::new(r#"{ int a = 5; int.. b = [7]; (b @ 0) + a }"#);
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
                Stmt::Expr(Box::new(Expr::BinOp {
                    left: Box::new(Expr::ArrayAccess {
                        array: Box::new(Expr::VariableAccess("b".to_string())),
                        index: Box::new(Expr::IntLiteral(0))
                    }),
                    op: BinOp::Plus,
                    right: Box::new(Expr::VariableAccess("a".to_string())),
                    parens: false
                }))
            ])
        );
    }
}
