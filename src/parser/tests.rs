#[cfg(test)]
mod tests {
    use crate::parser::{
        ast::{BinOp, Expr, Function, Parameter, Stmt},
        parser::Program,
    };
    use pretty_assertions::assert_eq;

    fn assert_program_equals(program: &str, expected: Program) {
        let actual = Program::parse(program).expect("Failed to parse program");
        dbg!("{:#?}", &actual);
        assert_eq!(actual, expected);
    }

    #[test]
    fn function_void_return() {
        let tree = Program {
            functions: vec![Function {
                name: "main".to_string(),
                parameters: vec![Parameter {
                    name: "args".to_string(),
                    type_name: "str..".to_string(),
                }],
                body: Stmt::Block(vec![Stmt::Expr(Box::new(Expr::IntLiteral(0)))]),
                return_type: "void".to_string(),
            }],
        };
        assert_program_equals("proc main(str.. args) { 0 }", tree.clone());
        assert_program_equals("proc main(str.. args) { 0; }", tree.clone());
        assert_program_equals("proc main(str.. args) -> void { 0 }", tree.clone());
        assert_program_equals("proc main(str.. args) -> void { 0; }", tree.clone());
    }

    #[test]
    fn expression_blocks() {
        assert_program_equals(
            r#"proc main(str.. args)  -> int {
            int a = {
                (
                    0
                )
            };

            a
         }"#,
            Program {
                functions: vec![Function {
                    name: "main".to_string(),
                    parameters: vec![Parameter {
                        name: "args".to_string(),
                        type_name: "str..".to_string(),
                    }],
                    body: Stmt::Block(vec![
                        Stmt::VariableDeclaration {
                            var_name: "a".to_string(),
                            type_name: "int".to_string(),
                            expr: Box::new(Expr::IntLiteral(0)),
                        },
                        Stmt::Expr(Box::new(Expr::VariableAccess("a".to_string()))),
                    ]),
                    return_type: "int".to_string(),
                }],
            },
        );
    }

    #[test]
    fn math_order_of_operations() {
        assert_program_equals(
            r#"proc main() -> int {
                int a = 3 * (1 + 2);
                a
             }"#,
            Program {
                functions: vec![Function {
                    name: "main".to_string(),
                    parameters: vec![],
                    body: Stmt::Block(vec![
                        Stmt::VariableDeclaration {
                            var_name: "a".to_string(),
                            type_name: "int".to_string(),
                            expr: Box::new(Expr::BinOp {
                                left: Box::new(Expr::IntLiteral(3)),
                                op: BinOp::Multiply,
                                right: Box::new(Expr::BinOp {
                                    left: Box::new(Expr::IntLiteral(1)),
                                    op: BinOp::Plus,
                                    right: Box::new(Expr::IntLiteral(2)),
                                    parens: true,
                                }),
                                parens: false,
                            }),
                        },
                        Stmt::Expr(Box::new(Expr::VariableAccess("a".to_string()))),
                    ]),
                    return_type: "int".to_string(),
                }],
            },
        );

        assert_program_equals(
            r#"proc main() -> int {
                int a = (1 + 2) * 3;
                a
         }"#,
            Program {
                functions: vec![Function {
                    name: "main".to_string(),
                    parameters: vec![],
                    body: Stmt::Block(vec![
                        Stmt::VariableDeclaration {
                            var_name: "a".to_string(),
                            type_name: "int".to_string(),
                            expr: Box::new(Expr::BinOp {
                                left: Box::new(Expr::BinOp {
                                    left: Box::new(Expr::IntLiteral(1)),
                                    op: BinOp::Plus,
                                    right: Box::new(Expr::IntLiteral(2)),
                                    parens: true,
                                }),
                                op: BinOp::Multiply,
                                right: Box::new(Expr::IntLiteral(3)),
                                parens: false,
                            }),
                        },
                        Stmt::Expr(Box::new(Expr::VariableAccess("a".to_string()))),
                    ]),
                    return_type: "int".to_string(),
                }],
            },
        );

        assert_program_equals(
            r#"proc main() -> int {
                ? 1593 * (3 * 3) - 3 == 15 {
                    1
                } : {
                    0
                }
         }"#,
            Program {
                functions: vec![Function {
                    name: "main".to_string(),
                    return_type: "int".to_string(),
                    parameters: vec![],
                    body: Stmt::Block(vec![Stmt::Expr(Box::new(Expr::If {
                        condition: Box::new(Expr::BinOp {
                            left: Box::new(Expr::BinOp {
                                left: Box::new(Expr::IntLiteral(1593)),
                                op: BinOp::Multiply,
                                right: Box::new(Expr::BinOp {
                                    left: Box::new(Expr::IntLiteral(3)),
                                    op: BinOp::Multiply,
                                    right: Box::new(Expr::IntLiteral(3)),
                                    parens: true,
                                }),
                                parens: false,
                            }),
                            op: BinOp::Minus,
                            right: Box::new(Expr::IntLiteral(3)),
                            parens: false,
                        }),
                        true_block: Stmt::Block(vec![Stmt::Expr(Box::new(Expr::IntLiteral(1)))]),
                        false_block: Stmt::Block(vec![Stmt::Expr(Box::new(Expr::IntLiteral(0)))]),
                    }))]),
                }],
            },
        );
    }
}
