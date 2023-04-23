#[cfg(test)]
mod tests {
    use crate::parser::{
        ast::{Expr, Function, Parameter, Stmt},
        parser::Program,
    };

    fn assert_program_equals(program: &str, expected: Program) {
        let actual = Program::parse(program).expect("Failed to parse program");
        assert_eq!(
            actual, expected,
            "Expected {:?}, got {:?}",
            expected, actual
        );
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
                body: Stmt::Block(vec![Stmt::ExprStmt(Box::new(Expr::IntLiteral(0)))]),
                return_type: "void".to_string(),
            }],
        };
        assert_program_equals("proc main(str.. args) { 0 }", tree.clone());
        assert_program_equals("proc main(str.. args) { 0; }", tree.clone());
        assert_program_equals("proc main(str.. args) -> void { 0 }", tree.clone());
        assert_program_equals("proc main(str.. args) -> void { 0; }", tree.clone());
    }
}
