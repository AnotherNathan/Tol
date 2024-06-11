use std::ops::Range;

use super::{lexer::Literal, span::Span};

#[derive(Debug, Clone, PartialEq)]
pub struct Package {
    pub files: Vec<File>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct File {
    pub path: String,
    pub source: String,
    pub top_level_elements: Vec<TopLevelElement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TopLevelElement {
    Import,
    Proc(Proc),
    Struct,
    Enum,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Proc {
    pub ident: Span,
    pub arguments: Vec<Argument>,
    pub returns: Vec<Argument>,
    pub body: Block,
    pub span: Range<usize>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
    pub ident: Span,
    pub data_type: DataType,
    pub span: Range<usize>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DataType {
    Builtin(BuiltinDataType),
    Slice(Box<Self>),
    Array {
        size: Box<Expression>,
        element_type: Box<Self>,
    },
    Other(PunctuatedPath),
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Assignment(Assignment),
    DeclarationAssignment(DeclarationAssignment),
    OperatorAssignment(OperatorAssignment),
    Expression(Expression),
    Loop,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DeclarationAssignment {
    pub left: Expression,
    pub right: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub left: Expression,
    pub right: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct OperatorAssignment {
    pub operator: CompoundOperator,
    pub left: Expression,
    pub right: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    IfElse(IfElse),
    Unary(UnaryExpression),
    Compound(CompoundExpression),
    Literal(Literal),
    ProcCall(ProcCall),
    PunctuatedPath(PunctuatedPath),
    Block(Block),
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Negate,
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpression {
    pub operator: UnaryOperator,
    pub expr: Box<Expression>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CompoundOperator {
    Add,
    Subtract,
    Multiply,
    Divide,

    And,
    Or,

    Equal,
    NotEqual,
    Greater,
    Smaller,
    GreaterEqual,
    SmallerEqual,
}

impl CompoundOperator {
    pub fn precedence(self) -> u32 {
        match self {
            CompoundOperator::Add | CompoundOperator::Subtract => 6,
            CompoundOperator::Multiply | CompoundOperator::Divide => 7,
            CompoundOperator::And => 4,
            CompoundOperator::Or => 3,
            CompoundOperator::Equal
            | CompoundOperator::NotEqual
            | CompoundOperator::Greater
            | CompoundOperator::Smaller
            | CompoundOperator::GreaterEqual
            | CompoundOperator::SmallerEqual => 5,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompoundExpression {
    pub operator: CompoundOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfElse {
    pub condition: Box<Expression>,
    pub body: Vec<Statement>,
    pub else_if: Option<Box<Self>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AndExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct OrExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NotExpression {
    pub expr: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AddExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SubtractExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DivideExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MultiplyExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProcCall {
    pub name: PunctuatedPath,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PunctuatedPath {
    pub components: Vec<Span>,
}
