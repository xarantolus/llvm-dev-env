use core::fmt;
use std::fmt::Display;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    /// Imports
    Need,
    /// Includes
    Include,

    /// Procedures
    Proc,

    /// Identifiers
    Identifier(String),

    /// {
    LBrace,
    /// }
    RBrace,

    /// [
    LBracket,
    /// ]
    RBracket,

    /// (
    LParen,
    /// )
    RParen,

    /// -
    Minus,
    /// +
    Plus,
    /// *
    Star,
    /// /
    Slash,
    /// %
    Percent,

    /// =
    Assign,

    /// ==
    Equality,
    /// <>
    NotEqual,
    /// <
    Less,
    /// >
    Greater,
    /// <=
    LessEqual,
    /// >=
    GreaterEqual,
    /// and
    And,
    /// or
    Or,
    /// not
    Not,

    /// ;
    Semicolon,
    /// ..
    DotDot,
    /// ->
    Arrow,

    /// ?
    Question,
    /// :
    Colon,
    /// ::
    DoubleColon,
    /// ,
    Comma,

    /// @
    At,

    /// "string"
    StringLiteral(String),
    /// 123
    IntLiteral(i64),
    /// true
    BoolLiteral(bool),

    EOF,
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Need => write!(f, "need"),
            Token::Include => write!(f, "include"),
            Token::Proc => write!(f, "proc"),
            Token::Identifier(s) => write!(f, "{}", s),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::Minus => write!(f, "-"),
            Token::Plus => write!(f, "+"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Percent => write!(f, "%"),
            Token::Assign => write!(f, "="),
            Token::Equality => write!(f, "=="),
            Token::NotEqual => write!(f, "<>"),
            Token::Less => write!(f, "<"),
            Token::Greater => write!(f, ">"),
            Token::LessEqual => write!(f, "<="),
            Token::GreaterEqual => write!(f, ">="),
            Token::And => write!(f, "and"),
            Token::Or => write!(f, "or"),
            Token::Not => write!(f, "not"),
            Token::Semicolon => write!(f, ";"),
            // Token::Dot => write!(f, "."),
            Token::DotDot => write!(f, ".."),
            Token::Arrow => write!(f, "->"),
            Token::Question => write!(f, "?"),
            Token::Colon => write!(f, ":"),
            Token::DoubleColon => write!(f, "::"),
            Token::Comma => write!(f, ","),
            Token::At => write!(f, "@"),
            Token::StringLiteral(s) => write!(f, "{:#?}", s),
            Token::IntLiteral(n) => write!(f, "{}", n),
            Token::BoolLiteral(b) => write!(f, "{}", b),
            Token::EOF => write!(f, ""),
        }
    }
}

impl Token {
    pub const IDEN: Token = Token::Identifier(String::new());

    // Some convenience functions for creating empty tokens
    pub const fn iden() -> Token {
        Token::Identifier(String::new())
    }

    pub const fn int() -> Token {
        Token::IntLiteral(0)
    }

    pub const fn bool() -> Token {
        Token::BoolLiteral(false)
    }

    pub const fn string() -> Token {
        Token::StringLiteral(String::new())
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::lexer::Lexer;
    use pretty_assertions::assert_eq;

    use super::*;
    use std::vec;

    fn assert_token_stream(s: &str, tokens: Vec<Token>) {
        let mut str_pos = 0;
        let mut tok_pos = 0;

        while tok_pos < tokens.len() {
            let (token, next) = Lexer::lex(s, str_pos).expect("Failed to lex token");

            assert_eq!(
                token, tokens[tok_pos],
                "Expected {:?} but got {:?} at position {} as token {}",
                tokens[tok_pos], token, str_pos, tok_pos
            );
            str_pos = next;
            tok_pos += 1;
        }

        assert_eq!(tok_pos, tokens.len());

        // Make sure the last thing is an EOF
        assert_eq!(Lexer::lex(s, str_pos).unwrap().0, Token::EOF);
    }

    #[test]
    fn simple_math() {
        assert_token_stream(
            "1+5",
            vec![Token::IntLiteral(1), Token::Plus, Token::IntLiteral(5)],
        );
        assert_token_stream(
            "1 + 5",
            vec![Token::IntLiteral(1), Token::Plus, Token::IntLiteral(5)],
        );
        assert_token_stream(
            "1 + 5 - 10",
            vec![
                Token::IntLiteral(1),
                Token::Plus,
                Token::IntLiteral(5),
                Token::Minus,
                Token::IntLiteral(10),
            ],
        );

        assert_token_stream(
            "(1+5) * 3",
            vec![
                Token::LParen,
                Token::IntLiteral(1),
                Token::Plus,
                Token::IntLiteral(5),
                Token::RParen,
                Token::Star,
                Token::IntLiteral(3),
            ],
        );
    }

    #[test]
    fn lex_simple_program() {
        let program = include_str!("../../testdata/example-program.l").trim();

        let mut pos = 0;
        loop {
            let (token, next) = Lexer::lex(program, pos).expect("Failed to lex token");
            if token == Token::EOF {
                assert!(next == program.len(), "Didn't lex entire program");
                break;
            }
            pos = next;
        }
    }

    #[test]
    fn strings() {
        assert_token_stream("\"test\"", vec![Token::StringLiteral("test".to_string())]);

        assert_token_stream(
            "\"test\" + \"test\"",
            vec![
                Token::StringLiteral("test".to_string()),
                Token::Plus,
                Token::StringLiteral("test".to_string()),
            ],
        );

        // Escape sequences: \n, \t, \", \\
        assert_token_stream(
            "\"\\n\\\"\\t\\\\\"",
            vec![Token::StringLiteral("\n\"\t\\".to_string())],
        );
    }

    #[test]
    fn conditionals() {
        assert_token_stream(
            r#"? n == 0 {
		1
	} : {
		n * factorial(n-1)
	}"#,
            vec![
                Token::Question,
                Token::Identifier("n".to_string()),
                Token::Equality,
                Token::IntLiteral(0),
                Token::LBrace,
                Token::IntLiteral(1),
                Token::RBrace,
                Token::Colon,
                Token::LBrace,
                Token::Identifier("n".to_string()),
                Token::Star,
                Token::Identifier("factorial".to_string()),
                Token::LParen,
                Token::Identifier("n".to_string()),
                Token::Minus,
                Token::IntLiteral(1),
                Token::RParen,
                Token::RBrace,
            ],
        );
    }

    #[test]
    fn procedure() {
        assert_token_stream(
            r#"proc max(int.. arr) -> int {
	arr -> reduce acc, elem {
		? acc > elem {
			 acc
		} : {
			elem
		}
	}
}
"#,
            vec![
                Token::Proc,
                Token::Identifier("max".to_string()),
                Token::LParen,
                Token::Identifier("int..".to_string()),
                Token::Identifier("arr".to_string()),
                Token::RParen,
                Token::Arrow,
                Token::Identifier("int".to_string()),
                Token::LBrace,
                Token::Identifier("arr".to_string()),
                Token::Arrow,
                Token::Identifier("reduce".to_string()),
                Token::Identifier("acc".to_string()),
                Token::Comma,
                Token::Identifier("elem".to_string()),
                Token::LBrace,
                Token::Question,
                Token::Identifier("acc".to_string()),
                Token::Greater,
                Token::Identifier("elem".to_string()),
                Token::LBrace,
                Token::Identifier("acc".to_string()),
                Token::RBrace,
                Token::Colon,
                Token::LBrace,
                Token::Identifier("elem".to_string()),
                Token::RBrace,
                Token::RBrace,
                Token::RBrace,
            ],
        );
    }

    #[test]
    fn arrays() {
        assert_token_stream(
            "int.. a = [1, 2, 3]",
            vec![
                Token::Identifier("int..".to_string()),
                Token::Identifier("a".to_string()),
                Token::Assign,
                Token::LBracket,
                Token::IntLiteral(1),
                Token::Comma,
                Token::IntLiteral(2),
                Token::Comma,
                Token::IntLiteral(3),
                Token::RBracket,
            ],
        );
    }

    #[test]
    fn multi_conditions() {
        assert_token_stream(
            r#"? n == 0 or n == 1"#,
            vec![
                Token::Question,
                Token::Identifier("n".to_string()),
                Token::Equality,
                Token::IntLiteral(0),
                Token::Or,
                Token::Identifier("n".to_string()),
                Token::Equality,
                Token::IntLiteral(1),
            ],
        );

        assert_token_stream(
            r#"? n >= 0 and m <> 1 {"#,
            vec![
                Token::Question,
                Token::Identifier("n".to_string()),
                Token::GreaterEqual,
                Token::IntLiteral(0),
                Token::And,
                Token::Identifier("m".to_string()),
                Token::NotEqual,
                Token::IntLiteral(1),
                Token::LBrace,
            ],
        );
    }

    #[test]
    fn bools() {
        assert_token_stream(
            r#"? true and false"#,
            vec![
                Token::Question,
                Token::BoolLiteral(true),
                Token::And,
                Token::BoolLiteral(false),
            ],
        );
    }
}
