//! exprs.rs: Handy functions for expressions.

use crate::parser;
use parser::Expression;
use parser::Expression::*;
use parser::Variable;

// region Boxed constructors for convenient building of expressions.
pub fn bTrue() -> Box<Expression> {
    Box::new(True)
}
pub fn bFalse() -> Box<Expression> {
    Box::new(False)
}
pub fn bNot(b: Box<Expression>) -> Box<Expression> {
    Box::new(Not(b))
}
pub fn bIf(c: Box<Expression>, yes: Box<Expression>, no: Box<Expression>) -> Box<Expression> {
    Box::new(If(c, yes, no))
}

pub fn bVariable(s: &str) -> Box<Variable> {
    Box::new(newVariable(s))
}
pub fn bVar(s: &str) -> Box<Expression> {
    Box::new(Var(newVariable(s)))
}

pub fn newVariable(s: &str) -> Variable {
    Variable {
        ident: s.to_string(),
    }
}

pub fn bAdd(l: Box<Expression>, r: Box<Expression>) -> Box<Expression> {
    Box::new(Add(l, r))
}
pub fn bNum(n: u32) -> Box<Expression> {
    Box::new(Num(n))
}
pub fn bSucc(e: Box<Expression>) -> Box<Expression> {
    Box::new(Succ(e))
}
pub fn bPred(e: Box<Expression>) -> Box<Expression> {
    Box::new(Pred(e))
}

pub fn bEq(l: Box<Expression>, r: Box<Expression>) -> Box<Expression> {
    Box::new(Eq(l, r))
}

pub fn bLet(v: Box<Variable>, def_expr: Box<Expression>, body: Box<Expression>) -> Box<Expression> {
    Box::new(Let(*v, def_expr, body))
}
pub fn bFn(s: &str, body: Box<Expression>) -> Box<Expression> {
    Box::new(Fn(
        newVariable(s),
        body,
    ))
}
pub fn bApply(l: Box<Expression>, r: Box<Expression>) -> Box<Expression> {
    Box::new(Apply(l, r))
}

pub fn bPair(l: Box<Expression>, r: Box<Expression>) -> Box<Expression> {
    Box::new(Pair(l, r))
}
pub fn bFst(p: Box<Expression>) -> Box<Expression> {
    Box::new(Fst(p))
}
pub fn bSnd(p: Box<Expression>) -> Box<Expression> {
    Box::new(Snd(p))
}

pub fn bHd(e: Box<Expression>) -> Box<Expression> {
    Box::new(Hd(e))
}
pub fn bTl(e: Box<Expression>) -> Box<Expression> {
    Box::new(Tl(e))
}
pub fn bCons(l: Box<Expression>, r: Box<Expression>) -> Box<Expression> {
    Box::new(Cons(l, r))
}
pub fn bNil() -> Box<Expression> {
    Box::new(Nil)
}
// endregion

/// The number of subexpressions in e.
pub fn expression_arity(e: &Expression) -> u32 {
    match e {
        True => 0,
        False => 0,
        Num(_n) => 0,
        Var(_v) => 0,
        Nil => 0,
        Let(_var, _bound_expr, _body) => 3,
        Not(_e) => 1,
        If(_cond, _yes, _no) => 3,
        Succ(_e) => 1,
        Pred(_e) => 1,
        Fst(_e) => 1,
        Snd(_e) => 1,
        Hd(_e) => 1,
        Tl(_e) => 1,
        Pair(_e1, _e2) => 2,
        Fn(v, _e) => 2,
        Eq(_e1, _e2) => 2,
        Cons(_e1, _e2) => 2,
        And(_e1, _e2) => 2,
        Add(_e1, _e2) => 2,
        Apply(_e1, _e2) => 2,
    }
}

pub fn expr_needs_paren(e: &Expression) -> bool {
    match e {
        True => false,
        False => false,
        Num(_n) => false,
        Var(_v) => false,
        Nil => false,
        Let(_var, _bound_expr, _body) => true,
        Not(_e) => true,
        If(_cond, _yes, _no) => true,
        Succ(_e) => true,
        Pred(_e) => true,
        Fst(_e) => true,
        Snd(_e) => true,
        Hd(_e) => true,
        Tl(_e) => true,
        Pair(_e1, _e2) => false,
        Fn(v, _e) => true,
        Eq(_e1, _e2) => true,
        Cons(_e1, _e2) => true,
        And(_e1, _e2) => true,
        Add(_e1, _e2) => true,
        Apply(_e1, _e2) => true,
    }
}

/// Does this expression have subexpressions?
pub fn is_simple(e: &Expression) -> bool {
    expression_arity(e) == 0
}
