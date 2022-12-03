use combine::parser::char::{string};
use combine::{EasyParser, Parser, ParseError, Stream};
use combine::{between, choice, parser};
use combine::parser::combinator::attempt;
use std::fmt;
use std::str::FromStr;
use thiserror::Error;

/// A parser for types that are not composite
fn basic_type_parser<Input>() -> impl Parser<Input, Output = Type>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    choice!(
        string("int").map(|_| Type::PrimInteger),
        string("boolean").map(|_| Type::PrimBoolean),
        string("string").map(|_| Type::PrimString),
        string("Expr").map(|_| Type::Expr),
        string("Function").map(|_| Type::Function),
        string("Method").map(|_| Type::Method),
        string("Parameter").map(|_| Type::Parameter),
        string("Type").map(|_| Type::Type),
        attempt(string("Class").map(|_| Type::Class)),
        attempt(string("Callable").map(|_| Type::Callable)),
        attempt(string("Call").map(|_| Type::Call)),
        attempt(string("Regex").map(|_| Type::Regex))
    )
}

fn aggregate_type_parser<Input>() -> impl Parser<Input, Output = Type>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    choice!(
        (string("List"), between(string("<"), string(">"), type_parser())).map(|(_, ty)| Type::List(Box::new(ty))),
        (string("Relational"), between(string("<"), string(">"), type_parser())).map(|(_, ty)| Type::Relational(Box::new(ty)))
    )
}

fn type_parser<Input>() -> impl Parser<Input, Output = Type>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    type_parser_()
}

// Note that this internal parser helper is required to break the recursive loop
// with the other parsing functions. The rust compiler will not infer impl types
// for functions in recursive cycles. This separate function generated with the
// parser! macro breaks the cycle by providing a concrete type (that is too hard
// to write by hand)
parser! {
    fn type_parser_[Input]()(Input) -> Type
    where [ Input: Stream<Token = char> ]
    {
        basic_type_parser().or(aggregate_type_parser())
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Type {
    PrimInteger,
    PrimBoolean,
    PrimString,
    Regex,
    Type,
    Class,
    Function,
    Method,
    Callable,
    Parameter,
    Call,
    Expr,
    Field,
    /// Values that appear in a relational context (and might be evaluated as a list or as a logic expression)
    Relational(Box<Type>),
    /// A list of values
    List(Box<Type>)
}

#[derive(Error, Debug, PartialEq, Eq)]
pub enum TypeParseError {
    #[error("Error parsing type")]
    TypeParseError
}

impl FromStr for Type {
    type Err = TypeParseError;
    fn from_str(s : &str) -> Result<Self, Self::Err> {
        type_parser()
            .easy_parse(combine::stream::position::Stream::new(s))
            .map(|res| res.0)
            .map_err(|_| TypeParseError::TypeParseError)
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::PrimInteger => write!(f, "int"),
            Type::PrimBoolean => write!(f, "boolean"),
            Type::PrimString => write!(f, "string"),
            Type::Function => write!(f, "Function"),
            Type::Method => write!(f, "Method"),
            Type::Callable => write!(f, "Callable"),
            Type::Field => write!(f, "Field"),
            Type::Parameter => write!(f, "Parameter"),
            Type::Type => write!(f, "Type"),
            Type::Class => write!(f, "Class"),
            Type::Call => write!(f, "Call"),
            Type::Expr => write!(f, "Expr"),
            Type::Regex => write!(f, "Regex"),
            Type::List(ty) => {
                write!(f, "List<")?;
                ty.fmt(f)?;
                write!(f, ">")
            },
            Type::Relational(ty) => {
                write!(f, "Relational<")?;
                ty.fmt(f)?;
                write!(f, ">")
            },
        }
    }
}

impl Type {
    pub fn is_callable(&self) -> bool {
        match self {
            Type::Function => true,
            Type::Method => true,
            Type::Callable => true,
            Type::Parameter => false,
            Type::Field => false,
            Type::Class => false,
            Type::Type => false,
            Type::Regex => false,
            Type::Call => false,
            Type::Expr => false,
            Type::PrimString => false,
            Type::PrimInteger => false,
            Type::PrimBoolean => false,
            Type::List(_) => false,
            Type::Relational(_) => false,
        }
    }

    /// If this type is Relational<T>, return T. Otherwise just return this type.
    ///
    /// We only want to treat Relational as special, as we want to support
    /// methods on List types.
    pub fn base_if_relational(&self) -> Type {
        match self {
            Type::Relational(inner_ty) => inner_ty.as_ref().clone(),
            Type::List(_) |
            Type::PrimInteger |
            Type::PrimBoolean |
            Type::PrimString |
            Type::Regex |
            Type::Type |
            Type::Class |
            Type::Function |
            Type::Method |
            Type::Callable |
            Type::Parameter |
            Type::Call |
            Type::Expr |
            Type::Field => self.clone(),
        }
    }
}

#[test]
fn test_parse_prim() {
    assert!(Type::from_str("int") == Ok(Type::PrimInteger));
}

#[test]
fn test_parse_relational() {
    assert!(Type::from_str("Relational<Parameter>") == Ok(Type::Relational(Box::new(Type::Parameter))));
}

#[test]
fn test_parse_callable() {
    assert!(Type::from_str("Callable") == Ok(Type::Callable));
}

#[test]
fn test_parse_call() {
    assert!(Type::from_str("Call") == Ok(Type::Call));
}
