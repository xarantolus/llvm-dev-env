use crate::lexer::token::Token;

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

    ArrayAccess {
        array: Box<Expr>,
        index: Box<Expr>,
    },
    VariableAccess(String),
    FunctionCall {
        name: String,
        arguments: Vec<Expr>,
    },
    BinOp {
        left: Box<Expr>,
        op: BinOp,
        right: Box<Expr>,
        parens: bool,
    },
    If {
        condition: Box<Expr>,
        true_block: Stmt,
        false_block: Stmt,
    },
}

impl Expr {
    pub fn is_assignable(&self) -> bool {
        match self {
            Expr::VariableAccess(_) => true,
            Expr::ArrayAccess { .. } => true,
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
    Expr(Box<Expr>),
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
