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
}

/// The raw syntax before type checking
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Syntax;

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub struct Untyped;

impl Repr for Syntax {
    type Type = Untyped;
}

/// The IR after type checking
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Typed;

impl Repr for Typed {
    type Type = Type;
}

/// A compiled regular expression with its original string
///
/// The original string is used for comparisons, which are not supported on regular expressions.
#[derive(Debug, Clone)]
pub struct CachedRegex(pub String, pub regex::Regex);

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Constant {
    Boolean(bool),
    Integer(i32),
    String_(String),
    Regex(CachedRegex)
}

impl PartialEq for CachedRegex {
    fn eq(&self, other : &CachedRegex) -> bool {
        self.0 == other.0
    }
}

impl Eq for CachedRegex {}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
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
    Field
}

impl Type {
    pub fn from_string(s : &str) -> Option<Type> {
        match s {
            "Function" => Some(Type::Function),
            "Method" => Some(Type::Method),
            "Class" => Some(Type::Class),
            "Parameter" => Some(Type::Parameter),
            "Callable" => Some(Type::Callable),
            "Field" => Some(Type::Field),
            "Type" => Some(Type::Type),
            "Regex" => Some(Type::Regex),
            "int" => Some(Type::PrimInteger),
            "boolean" => Some(Type::PrimBoolean),
            "string" => Some(Type::PrimString),
            _ => None,
        }
    }

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
            Type::PrimString => false,
            Type::PrimInteger => false,
            Type::PrimBoolean => false
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
    ConstantExpr(Constant),
    RelationalComparison(Box<Expr<R>>, CompOp, Box<Expr<R>>),
    EqualityComparison(Box<Expr<R>>, EqualityOp, Box<Expr<R>>),
    VarRef(String),
    LogicalConjunction(Box<Expr<R>>, Box<Expr<R>>),
    LogicalDisjunction(Box<Expr<R>>, Box<Expr<R>>),
    /// A method called in a base object, with a list of arguments
    QualifiedAccess(Box<Expr<R>>, String, Vec<Expr<R>>),
    // FIXME: Make this explicit and eliminate the aggregate constructor
    //
    // Count(Box<Expr<R>>),
    Aggregate(AggregateOp, Vec<AsExpr<R>>),
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
