use pretty::RcDoc;
use std::fmt;
use std::fmt::Display;

use crate::query::val_type::Type;

/// Abstract the representation of expressions so that the same core IR can be
/// used at multiple translation stages
pub trait Repr
where
    <Self as Repr>::Type: Eq,
    <Self as Repr>::Type: PartialEq,
    <Self as Repr>::Type: Clone,
    <Self as Repr>::Type: std::fmt::Debug,
{
    /// The type (of types) assigned to each expression
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
    Regex(CachedRegex),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
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
    NE,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum AggregateOp {
    Count,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr_<R: Repr> {
    /// A constant value appearing in a query
    ConstantExpr(Constant),
    /// References to previously-declared variables
    VarRef(String),
    /// An expression node that binds a variable to one of a number of different
    /// relational values (provided by the expression)
    ///
    /// This is not generated by the parser. Instead, it is introduced by the
    /// query planner whenever an equality expression acts as a binder (rather
    /// than a simple scalar assertion). Separating this out makes the
    /// compiler's job a bit easier, as this node represents explicit iteration
    /// over relations.
    Bind {
        /// The variable bound in this expression
        bound_var: VarDecl,
        /// The relational expression that provides the values to bind
        relation_expr: Box<Expr<R>>,
        /// The expression to be evaluated under each variable binding
        evaluated_expr: Box<Expr<R>>,
    },
    /// A comparison between two expressions
    ///
    /// This represents the Ord operations; Eq is handled by
    /// `EqualityComparison`.  This is supported for integer types.
    RelationalComparison {
        lhs: Box<Expr<R>>,
        op: CompOp,
        rhs: Box<Expr<R>>,
    },
    /// Equality between terms
    ///
    /// This is supported for all scalars
    EqualityComparison {
        lhs: Box<Expr<R>>,
        op: EqualityOp,
        rhs: Box<Expr<R>>,
    },
    /// The negation of a boolean expression
    LogicalNegation {
        predicate: Box<Expr<R>>,
    },
    /// A conjunction of two boolean expressions
    LogicalConjunction {
        lhs: Box<Expr<R>>,
        rhs: Box<Expr<R>>,
    },
    /// A disjunction of two boolean expressions
    LogicalDisjunction {
        lhs: Box<Expr<R>>,
        rhs: Box<Expr<R>>,
    },
    /// A method called in a base object, with a list of arguments
    QualifiedAccess {
        base: Box<Expr<R>>,
        method_name: String,
        operands: Vec<Expr<R>>,
    },
    // FIXME: Make this explicit and eliminate the aggregate constructor
    //
    // Count(Box<Expr<R>>),
    Aggregate {
        op: AggregateOp,
        operands: Vec<AsExpr<R>>,
    },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Expr<R: Repr> {
    pub expr: Expr_<R>,
    pub type_: R::Type,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AsExpr<R: Repr> {
    pub expr: Expr<R>,
    pub ident: Option<String>,
}

/// A top-level select statement
#[derive(Debug, Eq, PartialEq)]
pub struct Select<R: Repr> {
    /// The expression to evaluate and return
    pub select_exprs: Vec<AsExpr<R>>,
    /// The where clause, if any
    pub where_formula: Expr<R>,
    /// A possibly empty list of variable declarations
    pub var_decls: Vec<VarDecl>,
}

impl PartialEq for CachedRegex {
    fn eq(&self, other: &CachedRegex) -> bool {
        self.0 == other.0
    }
}

impl Eq for CachedRegex {}

impl Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constant::Boolean(b) => write!(f, "{b}"),
            Constant::Integer(i) => write!(f, "{i}"),
            Constant::String_(s) => write!(f, "\"{s}\""),
            Constant::Regex(cr) => {
                let s = &cr.0;
                write!(f, "/{s}/")
            }
        }
    }
}

impl Display for CompOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompOp::LT => write!(f, "<"),
            CompOp::LE => write!(f, "<="),
            CompOp::GT => write!(f, ">"),
            CompOp::GE => write!(f, ">="),
        }
    }
}

impl Display for EqualityOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EqualityOp::EQ => write!(f, "="),
            EqualityOp::NE => write!(f, "!="),
        }
    }
}

impl Display for AggregateOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AggregateOp::Count => write!(f, "count"),
        }
    }
}

impl Display for Untyped {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "")
    }
}

impl<R: Repr> Expr_<R>
where
    <R as Repr>::Type: Display,
{
    pub fn to_doc(&self) -> RcDoc<()> {
        match self {
            Expr_::ConstantExpr(c) => RcDoc::as_string(c),
            Expr_::VarRef(s) => parens(RcDoc::text("var-ref ").append(RcDoc::text(s))),
            Expr_::Bind {
                bound_var,
                relation_expr,
                evaluated_expr,
            } => RcDoc::text("(bind")
                .append(RcDoc::hardline())
                .append(
                    RcDoc::text("(= ")
                        .append(RcDoc::as_string(&bound_var.name))
                        .append(RcDoc::space())
                        .append(relation_expr.to_doc())
                        .append(RcDoc::text(")"))
                        .append(RcDoc::hardline())
                        .append(evaluated_expr.to_doc())
                        .append(RcDoc::hardline())
                        .nest(2),
                )
                .append(RcDoc::text(")")),
            Expr_::RelationalComparison { lhs, op, rhs } => parens(
                RcDoc::as_string(op)
                    .append(RcDoc::space())
                    .append(lhs.to_doc())
                    .append(RcDoc::space())
                    .append(rhs.to_doc())
                    .append(RcDoc::text(")")),
            ),
            Expr_::EqualityComparison { lhs, op, rhs } => parens(
                RcDoc::as_string(op)
                    .append(RcDoc::space())
                    .append(lhs.to_doc())
                    .append(RcDoc::space())
                    .append(rhs.to_doc())
                    .append(RcDoc::text(")")),
            ),
            Expr_::LogicalNegation { predicate } => parens(
                RcDoc::text("not").append(RcDoc::hardline()).append(
                    predicate.to_doc().nest(2)
                ),
            ),
            Expr_::LogicalConjunction { lhs, rhs } => parens(
                RcDoc::text("and").append(RcDoc::hardline()).append(
                    lhs.to_doc()
                        .append(RcDoc::hardline())
                        .append(rhs.to_doc())
                        .nest(2),
                ),
            ),
            Expr_::LogicalDisjunction { lhs, rhs } => parens(
                RcDoc::text("or").append(RcDoc::hardline()).append(
                    lhs.to_doc()
                        .append(RcDoc::hardline())
                        .append(rhs.to_doc())
                        .nest(2),
                ),
            ),
            Expr_::QualifiedAccess {
                base,
                method_name,
                operands,
            } => base
                .to_doc()
                .append(RcDoc::text("."))
                .append(RcDoc::as_string(method_name))
                .append(RcDoc::text("("))
                .append(RcDoc::intersperse(
                    operands.iter().map(|o| o.to_doc()),
                    RcDoc::text(","),
                ))
                .append(RcDoc::text(")")),
            Expr_::Aggregate { op, operands } => parens(
                RcDoc::as_string(op)
                    .append(RcDoc::intersperse(
                        operands.iter().map(|o| o.expr.to_doc()),
                        RcDoc::text(","),
                    ))
                    .append(RcDoc::text(")")),
            ),
        }
    }
}

impl<R: Repr> Expr<R>
where
    <R as Repr>::Type: Display,
{
    pub fn to_doc(&self) -> RcDoc<()> {
        self.expr.to_doc()
    }

    pub fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl<R: Repr> Select<R>
where
    <R as Repr>::Type: Display,
{
    pub fn to_doc(&self) -> RcDoc<()> {
        let from = RcDoc::text("from ").append(RcDoc::intersperse(
            self.var_decls.iter().map(|v| {
                let doc = RcDoc::as_string(&v.type_)
                    .append(RcDoc::space())
                    .append(RcDoc::as_string(&v.name));
                parens(doc)
            }),
            RcDoc::space(),
        ));
        let select = RcDoc::text("select ").append(RcDoc::intersperse(
            self.select_exprs.iter().map(|e| e.expr.to_doc()),
            RcDoc::space(),
        ));
        parens(
            RcDoc::text("query").append(RcDoc::hardline()).append(
                parens(from)
                    .append(RcDoc::hardline())
                    .append(self.where_formula.to_doc())
                    .append(RcDoc::hardline())
                    .append(parens(select))
                    .nest(2),
            ),
        )
    }

    pub fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

fn parens(d: RcDoc<()>) -> RcDoc<()> {
    RcDoc::text("(").append(d).append(RcDoc::text(")"))
}
