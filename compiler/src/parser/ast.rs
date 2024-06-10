use std::ops::Range;

use super::{lexer::Literal, span::Span};

pub struct Package {
    pub files: Vec<File>,
}

pub struct File {
    pub path: String,
    pub source: String,
    pub top_level_elements: Vec<TopLevelElement>,
}

pub enum TopLevelElement {
    Import,
    Proc(Proc),
    Struct,
    Enum,
}

pub struct Proc {
    pub ident: Span,
    pub arguments: Vec<Argument>,
    pub returns: Vec<Argument>,
    pub body: Block,
    pub span: Range<usize>,
}

pub struct Argument {
    pub ident: Span,
    pub data_type: DataType,
    pub span: Range<usize>,
}

pub enum DataType {
    Builtin(BuiltinDataType),
    Slice(Box<Self>),
    Array {
        size: Box<Expression>,
        element_type: Box<Self>,
    },
    Other(PunctuatedPath),
}

pub enum BuiltinDataType {
    UPtr,
    U8,
    U16,
    U32,
    U64,
    IPtr,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    String,
}

pub enum Statement {
    Assignment(Assignment),
    AddAssignment(Assignment),
    SubtractAssignment(Assignment),
    MultiplyAssignment(Assignment),
    DivideAssignment(Assignment),
    OrAssignment(Assignment),
    AndAssignment(Assignment),
    Expression(Expression),
    Loop,
}

pub struct Assignment {
    pub left: Span,
    pub right: Expression,
}

pub enum Expression {
    IfElse(IfElse),
    And(AndExpression),
    Or(OrExpression),
    Not(NotExpression),
    Add(AddExpression),
    Subtract(SubtractExpression),
    Divide(DivideExpression),
    Multiply(MultiplyExpression),
    Unary(UnaryExpression),
    Literal(Literal),
    ProcCall(CallArgument),
    PunctuatedPath(PunctuatedPath),
    Block(Block),
}

pub struct Block {
    pub statements: Vec<Statement>,
}

pub struct IfElse {
    pub condition: Box<Expression>,
    pub body: Vec<Statement>,
    pub else_if: Option<Box<Self>>,
}

pub struct AndExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

pub struct OrExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

pub struct NotExpression {
    pub expr: Box<Expression>,
}

pub struct AddExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

pub struct SubtractExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

pub struct DivideExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

pub struct MultiplyExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

pub struct UnaryExpression {
    pub expr: Box<Expression>,
}

pub struct CallArgument {
    pub expr: Box<Expression>,
}

pub struct PunctuatedPath {
    pub components: Vec<Span>,
}
