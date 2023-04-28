use std::fmt::{Display, Formatter};

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

impl Display for BinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match *self {
            BinOp::Plus => write!(f, "+"),
            BinOp::Minus => write!(f, "-"),
            BinOp::Multiply => write!(f, "*"),
            BinOp::Divide => write!(f, "/"),
            BinOp::Modulo => write!(f, "%"),

            BinOp::And => write!(f, "and"),
            BinOp::Or => write!(f, "or"),

            BinOp::Equal => write!(f, "=="),
            BinOp::NotEqual => write!(f, "!="),
            BinOp::LessThan => write!(f, "<"),
            BinOp::LessThanOrEqual => write!(f, "<="),
            BinOp::GreaterThan => write!(f, ">"),
            BinOp::GreaterThanOrEqual => write!(f, ">="),
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

    pub fn precedence(&self) -> u8 {
        match self {
            Expr::BinOp { op, .. } => op.precedence(),
            _ => u8::MAX,
        }
    }

    pub fn reorder_binops(self: Expr) -> Expr {
        // An expression tree is not necessarily correct after parsing, because the parser
        // does not take precedence into account. This function reorders the tree
        let Expr::BinOp { left: c_left, op: c_op, right: c_right, parens: c_parens } = self.clone() else {
            return self;
        };

        // Center is a binop. If on the left or on the right there are BinOps, we have to check if we need to
        // reorder them.
        match (*c_left.clone(), *c_right.clone()) {
            (
                Expr::BinOp {
                    left: l_left,
                    op: l_op,
                    right: l_right,
                    parens: l_parens,
                },
                Expr::BinOp {
                    left: r_left,
                    op: r_op,
                    right: r_right,
                    parens: r_parens,
                },
            ) => {
                // E.g. "a+b*c+d", "a*b+c*d", "a+b*c+d*e", "a*b+c*d+e"
                if c_op.precedence() > l_op.precedence() {
                    // E.g. "a+b*c+d", "a+b*c+d*e"
                    Expr::BinOp {
                        left: Box::new(Expr::BinOp {
                            left: l_left,
                            op: l_op,
                            right: l_right,
                            parens: l_parens,
                        }),
                        op: c_op,
                        right: c_right,
                        parens: c_parens,
                    }
                } else if c_op.precedence() > r_op.precedence() {
                    // E.g. "a*b+c*d", "a*b+c*d+e"
                    Expr::BinOp {
                        left: c_left,
                        op: c_op,
                        right: Box::new(Expr::BinOp {
                            left: r_left,
                            op: r_op,
                            right: r_right,
                            parens: r_parens,
                        }),
                        parens: c_parens,
                    }
                } else {
                    // E.g. "a+b*c+d", "a*b+c*d", "a+b*c+d*e", "a*b+c*d+e"
                    Expr::BinOp {
                        left: Box::new(Expr::BinOp {
                            left: l_left,
                            op: l_op,
                            right: l_right,
                            parens: l_parens,
                        }),
                        op: c_op,
                        right: Box::new(Expr::BinOp {
                            left: r_left,
                            op: r_op,
                            right: r_right,
                            parens: r_parens,
                        }),
                        parens: c_parens,
                    }
                }
            }
            (c_left @ Expr::BinOp { .. }, _) => {
                let c_left = c_left.reorder_binops();
                // E.g. "a+b*c" or "a*b+c"
                let has_parens = match c_left {
                    Expr::BinOp { parens, .. } => parens,
                    _ => false,
                };
                if c_op.precedence() > c_left.precedence() && !has_parens {
                    let Expr::BinOp { left: l_left, op: l_op, right: l_right, parens: l_parens } = c_left else {
                        panic!("This should never happen");
                    };

                    // E.g. "1 + 2 * 3" would previously be "(1 + 2) * 3", but we reorder it to "1 + (2 * 3)"
                    Expr::BinOp {
                        left: Box::new(Expr::BinOp {
                            left: l_left,
                            op: l_op,
                            right: l_right,
                            parens: l_parens,
                        }),
                        op: c_op,
                        right: c_right,
                        parens: c_parens,
                    }
                } else {
                    // E.g. "a*b+c"
                    Expr::BinOp {
                        left: Box::new(c_left),
                        op: c_op,
                        right: c_right,
                        parens: c_parens,
                    }
                }
            }
            (_, c_right @ Expr::BinOp { .. }) => {
                let c_right = c_right.reorder_binops();

                // E.g. "a+b*c" or "a*b+c"
                let has_parens = match c_right {
                    Expr::BinOp { parens, .. } => parens,
                    _ => false,
                };
                if c_op.precedence() > c_right.precedence() && !has_parens {
                    let Expr::BinOp { left: r_left, op: r_op, right: r_right, parens: r_parens } = c_right else {
                        panic!("This should never happen");
                    };

                    // E.g. "1 * 2 + 3" would previously be "1 * (2 + 3)", but we reorder it to "(1 * 2) + 3"
                    Expr::BinOp {
                        left: Box::new(Expr::BinOp {
                            left: c_left,
                            op: c_op,
                            right: r_left,
                            parens: c_parens,
                        }),
                        op: r_op,
                        right: r_right,
                        parens: c_parens,
                    }
                } else {
                    // E.g. "a*b+c"
                    Expr::BinOp {
                        left: c_left,
                        op: c_op,
                        right: Box::new(c_right),
                        parens: c_parens,
                    }
                }
            }
            _ => {
                // Neither left nor right are BinOps. We don't need to reorder anything
                return self;
            }
        }
    }

    // Adds parentheses around all BinOps in this expression
    pub fn add_parens(&mut self) {
        match self {
            Expr::BinOp {
                left,
                op: _,
                right,
                parens,
            } => {
                left.add_parens();
                right.add_parens();
                *parens = true;
            }
            _ => {}
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

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::VariableDeclaration {
                type_name,
                var_name,
                expr,
            } => {
                write!(f, "{} {} = {}", type_name, var_name, expr)
            }
            Stmt::Assignment { to, expr } => write!(f, "{} = {}", to, expr),
            Stmt::Block(stmts) => {
                write!(f, "{{")?;
                for stmt in stmts {
                    write!(f, "{}\n", stmt)?;
                }
                write!(f, "}}")
            }
            Stmt::Expr(expr) => write!(f, "{}", expr),
        }
    }
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

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::StringLiteral(value) => write!(f, "{:?}", value),
            Expr::IntLiteral(value) => write!(f, "{}", value),
            Expr::BoolLiteral(value) => write!(f, "{}", value),
            Expr::ArrayLiteral(values) => {
                write!(f, "[")?;
                for (i, value) in values.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", value)?;
                }
                write!(f, "]")
            }
            Expr::ArrayAccess { array, index } => write!(f, "{} @ {}", array, index),
            Expr::VariableAccess(name) => write!(f, "{}", name),
            Expr::FunctionCall { name, arguments } => {
                write!(f, "{}(", name)?;
                for (i, arg) in arguments.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            Expr::BinOp {
                left,
                op,
                right,
                parens,
            } => {
                if *parens {
                    write!(f, "(")?;
                }
                write!(f, "{} {} {}", left, op, right)?;
                if *parens {
                    write!(f, ")")?;
                }
                Ok(())
            }
            Expr::If {
                condition,
                true_block,
                false_block,
            } => {
                write!(
                    f,
                    "if {} {{ {} }} else {{ {} }}",
                    condition, true_block, false_block
                )
            }
        }
    }
}
