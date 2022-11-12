/// Abstract the representation of expressions so that the same core IR can be
/// used at multiple translation stages
pub trait Repr
where
    <Self as Repr>::Type: Eq,
    <Self as Repr>::Type: PartialEq,
    <Self as Repr>::Type: Clone,
    <Self as Repr>::Type: std::fmt::Debug
{
    type Type;
    type Evaluator<T>;
}

/// The raw syntax before type checking
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Syntax;

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub struct Untyped;

impl Repr for Syntax {
    type Type = Untyped;
    type Evaluator<T> = ();
}

/// The IR after type checking
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Typed;

impl Repr for Typed {
    type Type = Type;
    type Evaluator<T> = ();
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Constant {
    Boolean(bool),
    Integer(i32),
    String_(String)
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum Type {
    PrimInteger,
    PrimBoolean,
    PrimString,
    Type,
    Class,
    Function,
    Method,
    Callable,
    Parameter,
    Field
}

impl Type {
    pub fn from_str(s : &str) -> Option<Type> {
        match s {
            "Function" => Some(Type::Function),
            "Method" => Some(Type::Method),
            "Class" => Some(Type::Class),
            "Parameter" => Some(Type::Parameter),
            "Callable" => Some(Type::Callable),
            "Field" => Some(Type::Field),
            "Type" => Some(Type::Type),
            "int" => Some(Type::PrimInteger),
            "boolean" => Some(Type::PrimBoolean),
            "string" => Some(Type::PrimString),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VarDecl {
    pub type_: Type,
    pub name: String,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum CompOp {
    LT,
    LE,
    GT,
    GE,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum EqualityOp {
    EQ,
    NE
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum AggregateOp {
    Count
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr_<R: Repr> {
    QualifiedAccess(Box<Expr<R>>, String),
    Aggregate(AggregateOp, Vec<AsExpr<R>>),
    RelationalComparison(Box<Expr<R>>, CompOp, Box<Expr<R>>),
    EqualityComparison(Box<Expr<R>>, EqualityOp, Box<Expr<R>>),
    VarRef(String),
    ConstantExpr(Constant)
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Expr<R: Repr> {
    pub expr: Expr_<R>,
    pub type_: R::Type,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AsExpr<R: Repr> {
    pub expr: Expr<R>,
    pub ident: Option<String>
}

/// A top-level select statement
#[derive(Debug, Eq, PartialEq)]
pub struct Select<R: Repr> {
    /// The expression to evaluate and return
    pub select_exprs: Vec<AsExpr<R>>,
    /// The where clause, if any
    pub where_formula: Option<Expr<R>>,
    /// A possibly empty list of variable declarations
    pub var_decls: Vec<VarDecl>
}
