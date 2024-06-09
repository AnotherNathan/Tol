use std::ops::Range;

use super::lexer::Literal;

pub struct Package<'a> {
    pub files: Vec<File<'a>>,
}

pub struct File<'a> {
    pub path: String,
    pub top_level_elements: Vec<TopLevelElement<'a>>,
}

pub enum TopLevelElement<'a> {
    Import(&'a str),
    Proc(Proc<'a>),
    Struct,
    Enum,
}

pub struct Proc<'a> {
    pub ident: &'a str,
    pub arguments: Vec<Argument<'a>>,
    pub returns: Vec<Argument<'a>>,
    pub body: Vec<Statement<'a>>,
    pub span: Range<usize>,
}

pub struct Argument<'a> {
    pub ident: &'a str,
    pub data_type: DataType<'a>,
    pub span: Range<usize>,
}

pub enum DataType<'a> {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    String,
    Slice(Box<Self>),
    Array { size: u64, element_type: Box<Self> },
    Custom(CompoundDataType<'a>),
}

pub struct CompoundDataType<'a> {
    pub ident: &'a str,
    pub package: &'a str,
}

pub enum Statement<'a> {
    Assignment(Assignment<'a>),
    Expression(Expression<'a>),
    Loop,
}

pub struct Assignment<'a> {
    pub left: &'a str,
    pub right: Expression<'a>,
}

pub enum Expression<'a> {
    IfElse(IfElse<'a>),
    And(AndExpression<'a>),
    Or(OrExpression<'a>),
    Not(NotExpression<'a>),
    Add(AddExpression<'a>),
    Subtract(SubtractExpression<'a>),
    Divide(DivideExpression<'a>),
    Multiply(MultiplyExpression<'a>),
    Unary(UnaryExpression<'a>),
    Literal(Literal<'a>),
    Variable(Variable<'a>),
    ProcCall(CallArgument<'a>),
}

pub struct IfElse<'a> {
    pub condition: Box<Expression<'a>>,
    pub body: Vec<Statement<'a>>,
    pub else_if: Option<Box<Self>>,
}

pub struct AndExpression<'a> {
    pub left: Box<Expression<'a>>,
    pub right: Box<Expression<'a>>,
}

pub struct OrExpression<'a> {
    pub left: Box<Expression<'a>>,
    pub right: Box<Expression<'a>>,
}

pub struct NotExpression<'a> {
    pub expr: Box<Expression<'a>>,
}

pub struct AddExpression<'a> {
    pub left: Box<Expression<'a>>,
    pub right: Box<Expression<'a>>,
}

pub struct SubtractExpression<'a> {
    pub left: Box<Expression<'a>>,
    pub right: Box<Expression<'a>>,
}

pub struct DivideExpression<'a> {
    pub left: Box<Expression<'a>>,
    pub right: Box<Expression<'a>>,
}

pub struct MultiplyExpression<'a> {
    pub left: Box<Expression<'a>>,
    pub right: Box<Expression<'a>>,
}

pub struct UnaryExpression<'a> {
    pub expr: Box<Expression<'a>>,
}

pub struct Variable<'a> {
    pub ident: &'a str,
}

pub struct CallArgument<'a> {
    pub expr: Box<Expression<'a>>,
}
