use anyhow::{anyhow, Error, Result};
use ast::Package;

pub mod ast;
mod keyword;
mod lexer;
mod span;

use ast::*;
use keyword::Keyword;
use lexer::*;
use span::Span;

pub fn parse(package_directory: &str) -> Result<Package> {
    std::fs::read_dir(package_directory)?;
    todo!()
}

fn parse_file(file: &str) -> Result<File> {
    let source = std::fs::read_to_string(file)?;
    let tokens: Vec<_> = tokenize(&source).collect();
    let mut pos = 0;
    let mut errors = Vec::<Error>::new();
    while pos < tokens.len() {
        let start_token = &tokens[pos..];
    }
    todo!()
}

fn try_parse_proc(tokens: &[Token], mut pos: usize, source: &str) -> Option<(usize, Proc)> {
    let identifier = match_identifier(tokens, &mut pos)?;
    match_token(tokens, &mut pos, TokenKind::Colon)?;
    match_keyword(tokens, &mut pos, Keyword::Proc, source)?;
    match_token(tokens, &mut pos, TokenKind::LParen)?;

    todo!()
}

fn try_parse_argument(tokens: &[Token], mut pos: usize) -> Option<(usize, Argument)> {
    let identifier = match_identifier(tokens, &mut pos)?;
    match_token(tokens, &mut pos, TokenKind::Colon)?;
    todo!()
}

fn try_parse_data_type(tokens: &[Token], pos: usize) -> Option<(usize, DataType)> {
    if let Some((pos, data_type)) = try_parse_punctuated_path(tokens, pos) {
        return Some((pos, DataType::Other(data_type)));
    }

    todo!()
}

fn try_parse_slice(tokens: &[Token], mut pos: usize) -> Option<(usize, DataType)> {
    match_token(tokens, &mut pos, TokenKind::LBracket)?;
    match_token(tokens, &mut pos, TokenKind::RBracket)?;
    let (pos, element_type) = try_parse_data_type(tokens, pos)?;
    return Some((pos, DataType::Slice(Box::new(element_type))));
}

fn try_parse_array(tokens: &[Token], pos: usize) -> Option<(usize, DataType)> {
    todo!()
}

fn try_parse_punctuated_path(tokens: &[Token], mut pos: usize) -> Option<(usize, PunctuatedPath)> {
    let mut components = Vec::new();
    while let Some(ident) = match_identifier(tokens, &mut pos) {
        components.push(ident.clone());
        if match_token(tokens, &mut pos, TokenKind::Dot).is_none() {
            break;
        }
    }

    if components.len() == 0 {
        return None;
    }

    Some((pos, PunctuatedPath { components }))
}

fn try_parse_expression(tokens: &[Token], pos: usize) -> Option<(usize, Expression)> {
    if let Some((pos, compound_expression)) = try_parse_compound_expression(tokens, pos) {
        return Some((pos, Expression::Compound(compound_expression)));
    }

    try_parse_non_compound_expression(tokens, pos)
}

fn try_parse_non_compound_expression(
    tokens: &[Token],
    mut pos: usize,
) -> Option<(usize, Expression)> {
    if match_token(tokens, &mut pos, TokenKind::LParen).is_some() {
        let expr = try_parse_expression(tokens, pos)?;
        match_token(tokens, &mut pos, TokenKind::RParen)?;
        return Some(expr);
    }

    if let Some((pos, unary_expression)) = try_parse_unary_expression(tokens, pos) {
        return Some((pos, Expression::Unary(unary_expression)));
    }

    if let Some(literal) = match_literal(tokens, &mut pos) {
        return Some((pos, Expression::Literal(*literal)));
    }

    if let Some((pos, proc_call)) = try_parse_proc_call(tokens, pos) {
        return Some((pos, Expression::ProcCall(proc_call)));
    }

    if let Some((pos, punctuated_path)) = try_parse_punctuated_path(tokens, pos) {
        return Some((pos, Expression::PunctuatedPath(punctuated_path)));
    }

    None
}

fn try_parse_unary_expression(
    tokens: &[Token],
    mut pos: usize,
) -> Option<(usize, UnaryExpression)> {
    let operator = match_unary_operator(tokens, &mut pos)?;
    let (pos, expr) = try_parse_expression(tokens, pos)?;
    let unary_expression = UnaryExpression {
        expr: Box::new(expr),
        operator,
    };
    Some((pos, unary_expression))
}

fn try_parse_compound_expression(
    tokens: &[Token],
    pos: usize,
) -> Option<(usize, CompoundExpression)> {
    let (mut pos, left) = try_parse_non_compound_expression(tokens, pos)?;
    let operator = match_compound_operator(tokens, &mut pos)?;
    let (pos, right) = try_parse_expression(tokens, pos)?;

    match right {
        Expression::Compound(mut right_compound)
            if right_compound.operator.precedence() < operator.precedence() =>
        {
            let compound_expression = CompoundExpression {
                left: Box::new(left),
                operator,
                right: right_compound.left,
            };
            right_compound.left = Box::new(Expression::Compound(compound_expression));
            Some((pos, right_compound))
        }
        right => {
            let compound_expression = CompoundExpression {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            };
            Some((pos, compound_expression))
        }
    }
}

fn try_parse_proc_call(tokens: &[Token], pos: usize) -> Option<(usize, ProcCall)> {
    let (mut pos, name) = try_parse_punctuated_path(tokens, pos)?;

    match_token(tokens, &mut pos, TokenKind::LParen)?;
    let mut arguments = Vec::new();
    loop {
        if let Some((new_pos, expr)) = try_parse_expression(tokens, pos) {
            arguments.push(expr);
            pos = new_pos;
        }

        if match_token(tokens, &mut pos, TokenKind::Comma).is_none() {
            break;
        }
    }

    match_token(tokens, &mut pos, TokenKind::RParen)?;

    let proc_call = ProcCall { arguments, name };
    Some((pos, proc_call))
}

fn try_parse_block(tokens: &[Token], mut pos: usize) -> Option<(usize, Block)> {
    match_token(tokens, &mut pos, TokenKind::LBrace)?;
    let mut statements = Vec::new();
    while let Some((new_pos, statement)) = try_parse_statement(tokens, pos) {
        pos = new_pos;
        statements.push(statement);

        while match_token(tokens, &mut pos, TokenKind::NewLine).is_some() {}
    }
    match_token(tokens, &mut pos, TokenKind::RBrace)?;
    Some((pos, Block { statements }))
}

fn try_parse_statement(tokens: &[Token], pos: usize) -> Option<(usize, Statement)> {
    let (mut pos, statement) = if let Some((pos, assignment)) = try_parse_assignment(tokens, pos) {
        (pos, Statement::Assignment(assignment))
    } else if let Some((pos, declaration_assignment)) =
        try_parse_declaration_assignment(tokens, pos)
    {
        (
            pos,
            Statement::DeclarationAssignment(declaration_assignment),
        )
    } else if let Some((pos, operator_assignment)) = try_parse_operator_assignment(tokens, pos) {
        (pos, Statement::OperatorAssignment(operator_assignment))
    } else if let Some((pos, expression)) = try_parse_expression(tokens, pos) {
        (pos, Statement::Expression(expression))
    } else {
        return None;
    };

    match_token(tokens, &mut pos, TokenKind::NewLine)?;
    Some((pos, statement))
}

fn try_parse_assignment(tokens: &[Token], pos: usize) -> Option<(usize, Assignment)> {
    let (mut pos, left) = try_parse_expression(tokens, pos)?;
    match_token(tokens, &mut pos, TokenKind::Equal)?;
    let (pos, right) = try_parse_expression(tokens, pos)?;
    Some((pos, Assignment { left, right }))
}

fn try_parse_declaration_assignment(
    tokens: &[Token],
    pos: usize,
) -> Option<(usize, DeclarationAssignment)> {
    let (mut pos, left) = try_parse_expression(tokens, pos)?;
    match_token(tokens, &mut pos, TokenKind::Colon)?;
    match_token(tokens, &mut pos, TokenKind::Equal)?;
    let (pos, right) = try_parse_expression(tokens, pos)?;
    Some((pos, DeclarationAssignment { left, right }))
}

fn try_parse_operator_assignment(
    tokens: &[Token],
    pos: usize,
) -> Option<(usize, OperatorAssignment)> {
    let (mut pos, left) = try_parse_expression(tokens, pos)?;
    let operator = match_compound_operator(tokens, &mut pos)?;
    match_token(tokens, &mut pos, TokenKind::Equal)?;
    let (pos, right) = try_parse_expression(tokens, pos)?;
    Some((
        pos,
        OperatorAssignment {
            left,
            right,
            operator,
        },
    ))
}

fn match_unary_operator(tokens: &[Token], pos: &mut usize) -> Option<UnaryOperator> {
    if match_token(tokens, pos, TokenKind::Dash).is_some() {
        return Some(UnaryOperator::Negate);
    }
    if match_token(tokens, pos, TokenKind::Exclamation).is_some() {
        return Some(UnaryOperator::Not);
    }

    None
}

fn match_compound_operator(tokens: &[Token], pos: &mut usize) -> Option<CompoundOperator> {
    if match_token(tokens, pos, TokenKind::Plus).is_some() {
        return Some(CompoundOperator::Add);
    }
    if match_token(tokens, pos, TokenKind::Dash).is_some() {
        return Some(CompoundOperator::Subtract);
    }
    if match_token(tokens, pos, TokenKind::Asterisk).is_some() {
        return Some(CompoundOperator::Multiply);
    }
    if match_token(tokens, pos, TokenKind::Slash).is_some() {
        return Some(CompoundOperator::Divide);
    }

    if match_tokens(tokens, pos, &[TokenKind::Ampersand, TokenKind::Ampersand]).is_some() {
        return Some(CompoundOperator::And);
    }
    if match_tokens(tokens, pos, &[TokenKind::Bar, TokenKind::Bar]).is_some() {
        return Some(CompoundOperator::Or);
    }

    if match_tokens(tokens, pos, &[TokenKind::Smaller, TokenKind::Equal]).is_some() {
        return Some(CompoundOperator::SmallerEqual);
    }
    if match_tokens(tokens, pos, &[TokenKind::Greater, TokenKind::Equal]).is_some() {
        return Some(CompoundOperator::GreaterEqual);
    }
    if match_tokens(tokens, pos, &[TokenKind::Equal, TokenKind::Equal]).is_some() {
        return Some(CompoundOperator::Equal);
    }
    if match_tokens(tokens, pos, &[TokenKind::Exclamation, TokenKind::Equal]).is_some() {
        return Some(CompoundOperator::NotEqual);
    }
    if match_token(tokens, pos, TokenKind::Smaller).is_some() {
        return Some(CompoundOperator::Smaller);
    }
    if match_token(tokens, pos, TokenKind::Greater).is_some() {
        return Some(CompoundOperator::Greater);
    }

    None
}

fn match_tokens(tokens: &[Token], pos: &mut usize, expected_tokens: &[TokenKind]) -> Option<()> {
    let mut local_pos = *pos;
    for expected_token in expected_tokens {
        match_token(tokens, &mut local_pos, *expected_token)?;
    }

    *pos = local_pos;
    Some(())
}
fn match_token(tokens: &[Token], pos: &mut usize, expected_token: TokenKind) -> Option<()> {
    if tokens.get(*pos)?.kind != expected_token {
        return None;
    }

    *pos += 1;
    Some(())
}

fn match_keyword(
    tokens: &[Token],
    pos: &mut usize,
    expected_keyword: Keyword,
    source: &str,
) -> Option<()> {
    let keyword = match tokens.get(*pos) {
        Some(Token {
            kind: TokenKind::Identifier,
            span,
        }) => span,
        _ => return None,
    };
    let keyword = source[keyword.file_pos.clone()].parse::<Keyword>().ok()?;
    if keyword != expected_keyword {
        return None;
    }

    *pos += 1;

    Some(())
}

fn match_identifier<'a>(tokens: &'a [Token], pos: &mut usize) -> Option<&'a Span> {
    match tokens.get(*pos) {
        Some(Token {
            kind: TokenKind::Identifier,
            span,
        }) => {
            *pos += 1;
            Some(span)
        }
        _ => None,
    }
}

fn match_literal<'a>(tokens: &'a [Token], pos: &mut usize) -> Option<&'a Literal> {
    match tokens.get(*pos) {
        Some(Token {
            kind: TokenKind::Literal(literal),
            ..
        }) => {
            *pos += 1;
            Some(literal)
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn expressions() {
        let expr = "a + b";
        let expected = Expression::Compound(CompoundExpression {
            operator: CompoundOperator::Add,
            left: ident_expr(&expr, "a"),
            right: ident_expr(&expr, "b"),
        });
        run_expr_test(expr, expected);

        let expr = "a && b || c";
        let expected = Expression::Compound(CompoundExpression {
            operator: CompoundOperator::Or,
            left: Box::new(Expression::Compound(CompoundExpression {
                operator: CompoundOperator::And,
                left: ident_expr(&expr, "a"),
                right: ident_expr(&expr, "b"),
            })),
            right: ident_expr(&expr, "c"),
        });
        run_expr_test(expr, expected);
    }

    fn run_expr_test(expr: &str, expected_ast: Expression) {
        let tokens: Vec<_> = tokenize(expr).collect();
        let actual_ast = try_parse_expression(&tokens, 0).unwrap().1;
        pretty_assertions::assert_eq!(expected_ast, actual_ast);
    }

    fn span(source: &str, span: &str) -> Span {
        let pos = source.find(span).unwrap();
        let line_start = source[0..pos].lines().count();
        let line_count = span.lines().count();
        Span {
            file_pos: pos..(pos + span.len()),
            line: line_start..(line_start + line_count),
            column: 0..0,
        }
    }

    fn ident_expr(source: &str, ident: &str) -> Box<Expression> {
        Box::new(Expression::PunctuatedPath(PunctuatedPath {
            components: vec![span(source, ident)],
        }))
    }
}
