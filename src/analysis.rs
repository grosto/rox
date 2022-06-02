use crate::ast::{Expr, Stmt};

#[derive(thiserror::Error, Debug)]
#[error("Semantic Error: {kind}")]
pub struct SemanticError {
    pub kind: SemanticErrorKind,
}

#[derive(thiserror::Error, Debug)]
pub enum SemanticErrorKind {
    #[error("Too many arguments. Max arguments size is 255, got: {0}")]
    TooManyArguments(usize),
    #[error("Too many parameters. Max arguments size is 255, got: {0}")]
    TooManyParameters(usize),
}

trait Analyze {
    fn analyze(&self) -> Option<Vec<SemanticError>>;
}

impl Analyze for Stmt {
    fn analyze(&self) -> Option<Vec<SemanticError>> {
        match &self {
            Stmt::Block(stmts) => analyze_statements(stmts),
            Stmt::FunDecl {
                name: _name,
                params,
                body: _body,
            } if params.len() > 125 => Some(vec![SemanticError {
                kind: SemanticErrorKind::TooManyParameters(params.len()),
            }]),
            Stmt::ExprStmt(expr) => expr.analyze(),
            _ => None,
        }
    }
}

impl Analyze for Expr {
    fn analyze(&self) -> Option<Vec<SemanticError>> {
        match self {
            Expr::Call {
                expr: _expr,
                arguments,
            } if arguments.len() > 125 => Some(vec![SemanticError {
                kind: SemanticErrorKind::TooManyArguments(arguments.len()),
            }]),
            _ => None,
        }
    }
}

pub fn analyze_statements(statments: &[Stmt]) -> Option<Vec<SemanticError>> {
    let vec: Vec<SemanticError> = statments
        .iter()
        .map(|stmt| stmt.analyze())
        .flatten()
        .flatten()
        .collect();

    if vec.len() > 0 {
        Some(vec)
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use crate::{analysis::analyze_statements, parse_source_code};

    #[test]
    fn does_not_error_on_max_parameters() {
        let repeated_a = std::iter::repeat(String::from("a"))
            .take(125)
            .collect::<Vec<String>>()
            .join(",");

        let mut call = String::from("fun b(");
        call.push_str(&repeated_a);
        call.push_str(") {}");

        let parse_result = parse_source_code(&call);

        assert!(analyze_statements(&parse_result.unwrap()).is_none())
    }

    #[test]
    fn does_not_error_on_max_arguments() {
        let repeated_a = std::iter::repeat(String::from("a"))
            .take(125)
            .collect::<Vec<String>>()
            .join(",");

        let mut call = String::from("b(");
        call.push_str(&repeated_a);
        call.push_str(");");

        let parse_result = parse_source_code(&call);

        assert!(analyze_statements(&parse_result.unwrap()).is_none())
    }

    #[test]
    fn errors_on_more_than_max_parameters() {
        let repeated_a = std::iter::repeat(String::from("a"))
            .take(126)
            .collect::<Vec<String>>()
            .join(",");

        let mut call = String::from("fun b(");
        call.push_str(&repeated_a);
        call.push_str(") {}");

        let parse_result = parse_source_code(&call);

        assert!(analyze_statements(&parse_result.unwrap()).is_some())
    }

    #[test]
    fn errors_on_more_than_max_arguments() {
        let repeated_a = std::iter::repeat(String::from("a"))
            .take(126)
            .collect::<Vec<String>>()
            .join(",");

        let mut call = String::from("b(");
        call.push_str(&repeated_a);
        call.push_str(");");

        let parse_result = parse_source_code(&call);

        assert!(analyze_statements(&parse_result.unwrap()).is_some())
    }
}
