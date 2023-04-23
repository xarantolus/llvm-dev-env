use super::{
    error::{LexError, LexErrorType, ParseError, ParseErrorType},
    token::Token,
};

pub struct Lexer {
    pub(crate) input_file: String,
    pub(crate) current_position: usize,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        Self {
            input_file: input,
            current_position: 0,
        }
    }

    pub fn rewind(&mut self, pos: usize) {
        self.current_position = pos;
    }

    fn peek_next_impl(
        &mut self,
        expected_tokens: Vec<Token>,
    ) -> Result<(Token, usize), ParseError> {
        let (token, pos) = Lexer::lex(&self.input_file, self.current_position)?;

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

        Ok((token, pos))
    }

    /// Peeks at the next token without advancing the parser
    pub fn peek_token(&mut self, expected_tokens: Vec<Token>) -> Result<Token, ParseError> {
        let (token, _) = self.peek_next_impl(expected_tokens)?;
        Ok(token)
    }

    /// Advances to the next token and checks that they are of the specified type
    pub fn next_token(&mut self, expected_tokens: Vec<Token>) -> Result<Token, ParseError> {
        let (token, pos) = self.peek_next_impl(expected_tokens)?;
        self.current_position = pos;
        Ok(token)
    }

    /// Skip a token of the specified type, returning an error if the next token is not of the specified type
    pub fn skip_token(&mut self, token: Token) -> Result<(), ParseError> {
        let (_token, pos) = self.peek_next_impl(vec![token])?;
        self.current_position = pos;
        Ok(())
    }

    fn lex_single_char(c: char, next_c: char) -> Option<(Token, usize)> {
        Some(match c {
            '(' => (Token::LParen, 1),
            ')' => (Token::RParen, 1),
            '[' => (Token::LBracket, 1),
            ']' => (Token::RBracket, 1),
            '{' => (Token::LBrace, 1),
            '}' => (Token::RBrace, 1),
            '-' => {
                if next_c == '>' {
                    (Token::Arrow, 2)
                } else {
                    (Token::Minus, 1)
                }
            }
            '+' => (Token::Plus, 1),
            '*' => (Token::Star, 1),
            '/' => (Token::Slash, 1),
            '%' => (Token::Percent, 1),
            '=' => {
                if next_c == '=' {
                    (Token::Equality, 2)
                } else {
                    (Token::Assign, 1)
                }
            }
            '<' => {
                if next_c == '>' {
                    (Token::NotEqual, 2)
                } else if next_c == '=' {
                    (Token::LessEqual, 2)
                } else {
                    (Token::Less, 1)
                }
            }
            '>' => {
                if next_c == '=' {
                    (Token::GreaterEqual, 2)
                } else {
                    (Token::Greater, 1)
                }
            }
            ';' => (Token::Semicolon, 1),
            '.' => {
                if next_c == '.' {
                    (Token::DotDot, 2)
                } else {
                    return None;
                    // (Token::Dot, 1)
                }
            }
            '?' => (Token::Question, 1),
            ':' => {
                if next_c == ':' {
                    (Token::DoubleColon, 2)
                } else {
                    (Token::Colon, 1)
                }
            }
            ',' => (Token::Comma, 1),
            '@' => (Token::At, 1),
            _ => return None,
        })
    }

    pub fn lex(input: &str, start: usize) -> Result<(Token, usize), LexError> {
        let input = &input[start..];
        if input.len() == 0 {
            return Ok((Token::EOF, start));
        }

        let mut offset = 0;
        // Skip leading whitespace in input
        while offset < input.len() && input.chars().nth(offset).unwrap().is_whitespace() {
            offset += 1;
        }
        if offset >= input.len() {
            return Ok((Token::EOF, start));
        }

        // Check if token is a single character
        let c = input.chars().nth(offset).unwrap();
        let next_c = if offset + 1 < input.len() {
            input.chars().nth(offset + 1).unwrap()
        } else {
            '\0'
        };
        if let Some(token) = Lexer::lex_single_char(c, next_c) {
            return Ok((token.0, start + offset + token.1));
        }

        // Check if token is a string
        if c == '"' {
            let mut string = String::new();
            offset += 1;

            // Read the entire string and allow for escape characters
            let mut escape = false;
            loop {
                if offset >= input.len() {
                    return Err(LexError::new(
                        LexErrorType::UnexpectedEOF(Token::StringLiteral(string)),
                        input,
                        start + offset,
                    ));
                }
                let c = input.chars().nth(offset).unwrap();
                if escape {
                    match c {
                        'n' => string.push('\n'),
                        't' => string.push('\t'),
                        '\\' => string.push('\\'),
                        '"' => string.push('"'),
                        _ => {
                            return Err(LexError::new(
                                LexErrorType::InvalidStringEscape(c),
                                input,
                                start,
                            ));
                        }
                    }
                    escape = false;
                } else if c == '"' {
                    break;
                } else if c == '\\' {
                    escape = true;
                } else {
                    string.push(c);
                }
                offset += 1;
            }

            return Ok((Token::StringLiteral(string), start + offset + 1));
        }

        // Check if token is a number
        if c.is_digit(10) {
            let mut number = String::new();

            while offset < input.len() {
                let c = input.chars().nth(offset).unwrap();
                // basically allow digits and an 'x' for hex numbers 0x1234
                if !(c.is_digit(16) || c == 'x') {
                    break;
                }
                number.push(c);
                offset += 1;
            }

            // Now parse it. We allow numbers with 0x prefix to be hex
            if number.starts_with("0x") {
                return match i64::from_str_radix(&number[2..], 16) {
                    Ok(number) => Ok((Token::IntLiteral(number), start + offset)),
                    Err(e) => Err(LexError::new(LexErrorType::InvalidNumber(e), input, start)),
                };
            }
            return match i64::from_str_radix(&number, 10) {
                Ok(number) => Ok((Token::IntLiteral(number), start + offset)),
                Err(e) => Err(LexError::new(LexErrorType::InvalidNumber(e), input, start)),
            };
        }

        // Otherwise we consume until the next whitespace OR occurence of a single character token
        let mut iden = String::new();
        while offset < input.len() {
            let c = input.chars().nth(offset).unwrap();
            if c.is_whitespace() || Lexer::lex_single_char(c, '\0').is_some() {
                break;
            }
            iden.push(c);
            offset += 1;
        }

        match iden.as_str() {
            "proc" => Ok((Token::Proc, start + offset)),
            "need" => Ok((Token::Need, start + offset)),
            "include" => Ok((Token::Include, start + offset)),
            "and" => Ok((Token::And, start + offset)),
            "or" => Ok((Token::Or, start + offset)),
            "not" => Ok((Token::Not, start + offset)),
            "true" => Ok((Token::BoolLiteral(true), start + offset)),
            "false" => Ok((Token::BoolLiteral(false), start + offset)),
            _ => Ok((Token::Identifier(iden), start + offset)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn next_token() {
        let mut parser = Lexer::new("1 + 2".to_string());

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
        let mut parser = Lexer::new("1 + 2".to_string());

        assert_eq!(
            parser.next_token(vec![Token::Plus]),
            Err(ParseError::new(
                ParseErrorType::InvalidToken(Token::IntLiteral(1), vec![Token::Plus]),
                "1 + 2",
                0
            ))
        );

        parser = Lexer::new("1 + 2".to_string());

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
