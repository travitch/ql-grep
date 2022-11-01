#[derive(Debug, Clone, Eq, PartialEq)]
pub enum QLValue {
    QLBoolean(bool),
    QLInteger(i32),
    QLString(String),
    QLTuple(Vec<Box<QLValue>>)
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Type {
    Class,
    Function,
    Method,
    Parameter,
    Field
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VarDecl {
    pub type_: Type,
    pub name: String
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum CompOp {
    LT,
    LE,
    GT,
    GE,
    EQ,
    NE
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum AggregateOp {
    Count
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
    QualifiedAccess(Box<Expr>, String),
    Aggregate(AggregateOp, Vec<AsExpr>),
    Comparison(Box<Expr>, CompOp, Box<Expr>),
    Equals(Box<Expr>, Box<Expr>),
    VarRef(String),
    ValueExpr(QLValue)
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AsExpr {
    pub expr: Expr,
    pub ident: Option<String>
}

/// A top-level select statement
#[derive(Debug, Eq, PartialEq)]
pub struct Select {
    /// The expression to evaluate and return
    pub select_exprs: Vec<AsExpr>,
    /// The where clause, if any
    pub where_formula: Option<Expr>,
    /// A possibly empty list of variable declarations
    pub var_decls: Vec<VarDecl>
}
