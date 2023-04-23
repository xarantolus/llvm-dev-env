#[cfg(test)]
mod tests {
    use crate::parser::parser::Program;

    fn assert_program_equals(program: &str, expected: Program) {
        let actual = Program::parse(program).expect("Failed to parse program");
        assert_eq!(
            actual, expected,
            "Expected {:?}, got {:?}",
            expected, actual
        );
    }

    #[test]
    fn test_parser() {}
}
