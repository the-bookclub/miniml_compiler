//! eval.rs: A basic interpreter for MiniML expressions.
//! Largely useful for testing.

use crate::{parser, pprint};
use pprint::pprint;
use parser::Expression;
use parser::Expression::*;
use parser::Variable;
use std::collections::HashMap;

/// Contexts represent value environments.
/// e.g. those introduced by `let x = e1 in e2`, or by applications.
type Context = HashMap<Variable, Expression>;

fn pprint_ctx(ctx: &Context) -> String {
    let mut s = String::new();
    for (k,v) in ctx.iter()  {
        s.push_str(format!(" - {}:  {}\n", k.ident, pprint(v)).as_str());
    }
    s
}

#[derive(Clone, PartialEq, Debug)]
pub enum EvaluationOrder {
    Normal,
    Applicative,
}

const EVALUATION_ORDER: EvaluationOrder = EvaluationOrder::Normal;

/// Evaluate under an empty context.
pub fn eval(e: &Expression) -> Result<Expression, &'static str> {
    return eval_under(e, &Context::new());
}

/// Evaluate with a given context.
pub fn eval_under(e: &Expression, ctx: &Context) -> Result<Expression, &'static str> {
    match e {
        True => Ok(True),
        False => Ok(False),
        Num(n) => Ok(e.clone()),
        Var(v) =>
            match ctx.get(v) {
                Some(bound_e) => eval_under(bound_e, ctx),
                None => Ok(e.clone())
            }
        Nil => Ok(Nil),
        Not(e) => match eval_under(e, ctx)? {
            True => Ok(False),
            False => Ok(True),
            _ => Err("Not applied to non-boolean.")
        },
        If(cond, yes, no) => match eval_under(cond, ctx)? {
            True => eval_under(yes, ctx),
            False => eval_under(no, ctx),
            // If the condition doesn't (yet) evaluate to a normal form, DO NOT
            // simplify the yes and no branches - they may diverge, even if the
            // program is well-formed.
            cond_n => Ok(If(Box::new(cond_n), yes.clone(), no.clone()))
            //_ => Err("If applied to non-boolean condition.")
        },
        And(left, right) => match eval_under(left, ctx)? {
            True => eval_under(right, ctx),
            False => Ok(False),
            _ => Err("And applied to non-boolean condition.")
        }

        Succ(e) => match eval_under(e, ctx)? {
            Num(n) =>
                if n == u32::MAX {
                    Ok(Num(n))
                } else {
                    Ok(Num(n+1))
                }
            _ => Err("Succ applied to non-integer parameter.")
        }
        Pred(e) => match eval_under(e, ctx)? {
            Num(n) =>
                if n == 0 {
                    Ok(Num(0))
                } else {
                    Ok(Num(n-1))
                }
            _ => Err("Pred applied to non-integer parameter.")
        }
        Add(left, right) =>
            match (eval_under(left, ctx)?, eval_under(right, ctx)?) {
                (Num(l), Num(r)) => Ok(Num(l+r)),
                (l, r) => Ok(Add(Box::new(l), Box::new(r)))
            }
        Eq(left, right) =>
            match (eval_under(left, ctx)?, eval_under(right, ctx)?) {
                (Num(l), Num(r)) => Ok(true_or_false(l == r)),
                (True, True) => Ok(True),
                (True, False) => Ok(False),
                (False, True) => Ok(False),
                (False, False) => Ok(True),
                (l, r) => Ok(Eq(Box::new(l), Box::new(r)))
            }

        // - Normal form
        Fn(var, body) => {
            // If var is already in the context above this abstraction, remove it.
            let mut inner_ctx = ctx.clone();
            inner_ctx.remove(var);
            let body_normal = eval_under(body, &inner_ctx)?;
            Ok(Fn(var.clone(), Box::new(body_normal)))
        },

        // Applications
        Apply(l, r) => eval_apply(l, r, ctx),

        Let(var, bound_expression, body) => {
            // Evaluate e1 in the current context,
            let bound_value = eval_under(&*bound_expression, ctx)?;
            // Add x = e1 into a new inner context,
            let mut inner_ctx = ctx.clone();
            inner_ctx.insert(var.clone(), bound_value);
            // And evaluate e2!
            eval_under(body, &inner_ctx)
        }

        _ => todo!()
    }
}

fn eval_apply(l: &Expression, r: &Expression, ctx: &Context)
                     -> Result<Expression, &'static str> {
    let l_normal = eval_under(l, ctx)?;
    match l_normal {
        Fn(var, body) => { // Beta reduction.
            // Replace every occurrence of var in body with r.
            let body_replaced = replace_var_in_expr_with_r(&var, &*body, r);
            eval_under(&*body_replaced, ctx)
        }
        _ => {
            // - Not too eager...
            Ok(Apply(Box::new(l_normal), Box::new(r.clone())))
            // - Too eager?
            //Ok(Apply(Box::new(l_normal),
            //         Box::new(eval_under_dbg(r, ctx, idbg)?)))
        }
    }
}


fn replace_var_in_expr_with_r(var: &Variable, body: &Expression, r: &Expression)
                              -> Box<Expression> {
    let b = body.clone();
    Box::new(
    match body {
        True => b,
        False => b,
        Num(_n) => b,
        Var(_v) => if *_v == *var {
            r.clone()
        } else {
            b
        },
        Nil => b,
        Let(_var, _bound_expr, _body) => {
            let new_body =
                if *_var == *var {
                    _body.clone()
                } else {
                    replace_var_in_expr_with_r(var, &*_body, r)
                };
            let new_binding =
                replace_var_in_expr_with_r(var, &_bound_expr, r);
            Let(_var.clone(), new_binding, new_body)
        },
        Not(_e) => Not(replace_var_in_expr_with_r(var, &*_e, r)),
        If(_cond, _yes, _no) =>
            If(replace_var_in_expr_with_r(var, &*_cond, r),
               replace_var_in_expr_with_r(var, &*_yes, r),
               replace_var_in_expr_with_r(var, &*_no, r)),
        Succ(_e) =>
            Succ(replace_var_in_expr_with_r(var, &*_e, r)),
        Pred(_e) =>
            Pred(replace_var_in_expr_with_r(var, &*_e, r)),
        Fst(_e) =>
            Fst(replace_var_in_expr_with_r(var, &*_e, r)),
        Snd(_e) =>
            Snd(replace_var_in_expr_with_r(var, &*_e, r)),
        Hd(_e) =>
            Hd(replace_var_in_expr_with_r(var, &*_e, r)),
        Tl(_e) =>
            Tl(replace_var_in_expr_with_r(var, &*_e, r)),
        Pair(_e1, _e2) =>
            Pair(replace_var_in_expr_with_r(var, &*_e1, r),
                 replace_var_in_expr_with_r(var, &*_e2, r)),
        Fn(_var, _body) => {
            let new_body =
                if *_var == *var {
                    _body.clone()
                } else {
                    replace_var_in_expr_with_r(var, &*_body, r)
                };
            Fn(_var.clone(), new_body)
        },
        Eq(_e1, _e2) =>
            Eq(replace_var_in_expr_with_r(var, &*_e1, r),
                 replace_var_in_expr_with_r(var, &*_e2, r)),
        Cons(_e1, _e2) =>
            Cons(replace_var_in_expr_with_r(var, &*_e1, r),
                 replace_var_in_expr_with_r(var, &*_e2, r)),
        And(_e1, _e2) =>
            And(replace_var_in_expr_with_r(var, &*_e1, r),
                 replace_var_in_expr_with_r(var, &*_e2, r)),
        Add(_e1, _e2) =>
            Add(replace_var_in_expr_with_r(var, &*_e1, r),
                 replace_var_in_expr_with_r(var, &*_e2, r)),
        Apply(_e1, _e2) =>
            Apply(replace_var_in_expr_with_r(var, &*_e1, r),
                 replace_var_in_expr_with_r(var, &*_e2, r)),
    })
}

fn true_or_false(b: bool) -> Expression {
    if b {
        True
    } else {
        False
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::exprs;
    use exprs::*;

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

    #[test]
    fn test_eval_basic_add() {
        let expr = bAdd(bNum(0), bNum(0));
        assert_eq!(eval(&*expr), Ok(Num(0)));

        let expr = bAdd(bNum(1), bNum(0));
        assert_eq!(eval(&*expr), Ok(Num(1)));

        let expr = bAdd(bNum(0), bNum(1));
        assert_eq!(eval(&*expr), Ok(Num(1)));

        let expr = bAdd(bNum(2), bNum(2));
        assert_eq!(eval(&*expr), Ok(Num(4)));

        let expr = bAdd(bNum(3), bNum(1));
        assert_eq!(eval(&*expr), Ok(Num(4)));
    }

    #[test]
    fn test_eval_basic_succ() {
        let expr = bSucc(bNum(0));
        assert_eq!(eval(&*expr), Ok(Num(1)));

        let expr = bSucc(bNum(1));
        assert_eq!(eval(&*expr), Ok(Num(2)));

        let expr = bSucc(bNum(2));
        assert_eq!(eval(&*expr), Ok(Num(3)));

        let expr = bSucc(bNum(3));
        assert_eq!(eval(&*expr), Ok(Num(4)));

        // Addition is saturating.
        let expr = bSucc(bNum(u32::MAX));
        assert_eq!(eval(&*expr), Ok(Num(u32::MAX)));
    }


    #[test]
    fn test_eval_basic_pred() {
        // Subtraction is saturating.
        let expr = bPred(bNum(0));
        assert_eq!(eval(&*expr), Ok(Num(0)));

        let expr = bPred(bNum(1));
        assert_eq!(eval(&*expr), Ok(Num(0)));

        let expr = bPred(bNum(2));
        assert_eq!(eval(&*expr), Ok(Num(1)));

        let expr = bPred(bNum(3));
        assert_eq!(eval(&*expr), Ok(Num(2)));

        let expr = bPred(bNum(u32::MAX));
        assert_eq!(eval(&*expr), Ok(Num(u32::MAX-1)));
    }

    #[test]
    fn test_eval_basic_fn_apply() {
        let add_two = bFn("a", bAdd(bNum(2), bVar("a")));
        let expr = bApply(add_two,
                        bNum(4));
        assert_eq!(eval(&expr), Ok(Num(6)));
    }

    fn rust_fib(n: u32) -> u32 {
        match n {
            0 => 0,
            1 => 1,
            n => rust_fib(n-1) + rust_fib(n-2)
        }
    }


    #[test]
    fn test_eval_basic_y_comb() {
        // Curry's Y combinator
        let V = bFn("Vy",
                    bApply(bVar("Yx"),
                        bApply(bVar("Vy"), bVar("Vy"))
                    ));
        let Y = bFn("Yx",
                    bApply(V.clone(), V));

        let mut COMBINATORS = Context::new();
        COMBINATORS.insert(*bVariable("Y"), *Y);

        let fib = bApply(bVar("Y"),
                        bFn("fib", bFn("x",
                            bIf(bEq(bVar("x"), bNum(0)), bNum(0), // if x == 0, 0
                                bIf(bEq(bVar("x"), bNum(1)), bNum(1), // if x == 1, 1
                                    // x > 1
                                    bLet(bVariable("px"), bPred(bVar("x")),
                                        bAdd( // fib px + fib (pred px)
                                            bApply(bVar("fib"), bVar("px")),
                                            bApply(bVar("fib"), bPred(bVar("px"))),
                                        ))
                                ))
                        ))
            );
        let mml_fib = |x| eval_under(&bApply(fib.clone(), bNum(x)), &COMBINATORS);

        for n in 0..10 {
            let expected = rust_fib(n);
            if let Ok(Num(got)) = mml_fib(n) {
                assert_eq!(got, expected);
            } else {
                assert!(false);
            }
        }
    }
}
