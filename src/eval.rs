
//! eval.rs: A basic interpreter for MiniML expressions.
//! Largely useful for testing.

use crate::parser;
use parser::Expression;
use parser::Expression::*;
use parser::Variable;
use std::collections::HashMap;

/// Contexts represent value environments.
/// e.g. those introduced by `let x = e1 in e2`, or by applications.
type Context = HashMap<Variable, Expression>;

/// Evaluate under an empty context.
fn eval(e: &Expression) -> Result<Expression, &'static str> {
    return eval_under(e, &Context::new());
}

/// Evaluate with a given context.
fn eval_under(e: &Expression, ctx: &Context) -> Result<Expression, &'static str> {
    match e {
        True => Ok(True),
        False => Ok(False),
        Num(n) => Ok(e.clone()),
        Var(v) => {
            // IMPROVE(akiss-xyz, 2024-06-11): Lazy semantics for let will affect this.
            ctx.get(v).ok_or("Variable not defined.").clone().cloned()
        }
        Nil => Ok(Nil),
        Let(var, bound_expression, body) => {
            // let x = e1 in e2
            // Evaluate e1 in the current context,
            let bound_value = eval_under(&*bound_expression, ctx)?;
            // Add x = e1 into a new inner context,
            let mut inner_ctx = ctx.clone();
            inner_ctx.insert(var.clone(), bound_value);
            // And evaluate e2!
            eval_under(body, &inner_ctx)
        }
        Not(e) => match eval_under(e, ctx)? {
            True => Ok(False),
            False => Ok(True),
            _ => Err("Not applied to non-boolean.")
        },
        If(cond, yes, no) => match eval_under(cond, ctx)? {
            True => eval_under(yes, ctx),
            False => eval_under(no, ctx),
            _ => Err("If applied to non-boolean condition.")
        },

        // ... TODO(akiss-xyz, 2024-06-11):

        And(left, right) => match eval_under(left, ctx)? {
            True => eval_under(right, ctx),
            False => Ok(False),
            _ => Err("And applied to non-boolean condition.")
        }

        _ => todo!()
    }
}

// -- Tests --

fn bTrue() -> Box<Expression> { Box::new(True) }
fn bFalse() -> Box<Expression>  { Box::new(False) }

#[test]
fn test_eval_basic_bools() {
    assert_eq!(eval(&False), Ok(False));
    assert_eq!(eval(&True), Ok(True));
}

#[test]
fn test_eval_basic_and() {
    let expr = And(bFalse(), bFalse());
    assert_eq!(eval(&expr), Ok(False));

    let expr = And(bFalse(), bTrue());
    assert_eq!(eval(&expr), Ok(False));

    let expr = And(bTrue(), bFalse());
    assert_eq!(eval(&expr), Ok(False));

    let expr = And(bTrue(), bTrue());
    assert_eq!(eval(&expr), Ok(True));
}

#[test]
fn test_eval_basic_not() {
    let expr = Not(bFalse());
    assert_eq!(eval(&expr), Ok(True));

    let expr = Not(bTrue());
    assert_eq!(eval(&expr), Ok(False));
}

#[test]
fn test_eval_basic_if() {
    let expr = If(bFalse(), bFalse(), bFalse());
    assert_eq!(eval(&expr), Ok(False));

    let expr = If(bFalse(), bFalse(), bTrue());
    assert_eq!(eval(&expr), Ok(True));

    let expr = If(bFalse(), bTrue(), bFalse());
    assert_eq!(eval(&expr), Ok(False));

    let expr = If(bFalse(), bTrue(), bTrue());
    assert_eq!(eval(&expr), Ok(True));

    let expr = If(bTrue(), bFalse(), bFalse());
    assert_eq!(eval(&expr), Ok(False));

    let expr = If(bTrue(), bFalse(), bTrue());
    assert_eq!(eval(&expr), Ok(False));

    let expr = If(bTrue(), bTrue(), bFalse());
    assert_eq!(eval(&expr), Ok(True));

    let expr = If(bTrue(), bTrue(), bTrue());
    assert_eq!(eval(&expr), Ok(True));
}

// Box a not from a boxed expression.
fn bNot(b: Box<Expression>) -> Box<Expression> { Box::new(Not(b)) }

// Test exprs like if not (X) then Y else Z.
#[test]
fn test_eval_basic_if_not() {
    let expr = If(bNot(bFalse()), bFalse(), bFalse());
    assert_eq!(eval(&expr), Ok(False));

    let expr = If(bNot(bFalse()), bFalse(), bTrue());
    assert_eq!(eval(&expr), Ok(False));

    let expr = If(bNot(bFalse()), bTrue(), bFalse());
    assert_eq!(eval(&expr), Ok(True));

    let expr = If(bNot(bFalse()), bTrue(), bTrue());
    assert_eq!(eval(&expr), Ok(True));

    let expr = If(bNot(bTrue()), bFalse(), bFalse());
    assert_eq!(eval(&expr), Ok(False));

    let expr = If(bNot(bTrue()), bFalse(), bTrue());
    assert_eq!(eval(&expr), Ok(True));

    let expr = If(bNot(bTrue()), bTrue(), bFalse());
    assert_eq!(eval(&expr), Ok(False));

    let expr = If(bNot(bTrue()), bTrue(), bTrue());
    assert_eq!(eval(&expr), Ok(True));
}

fn bVariable(s: &str) -> Box<Variable> {
    Box::new(Variable{ident: s.to_string()})
}

fn bVar(s: &str) -> Box<Expression> {
    Box::new(Var(Variable{ident: s.to_string()}))
}

#[test]
fn test_eval_basic_let() {
    let expr = Let(*bVariable("x"),
                   bTrue(),
                   bVar("x"));
    assert_eq!(eval(&expr), Ok(True));

    let expr = Let(*bVariable("x"),
                   bTrue(),
                   bNot(bVar("x")));
    assert_eq!(eval(&expr), Ok(False));
}
