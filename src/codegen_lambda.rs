
use std::fmt::format;

use crate::parser::Expression;
use crate::parser::Expression::*;

static lambda: &'static str  = "λ";

/// Given an expression, generate a Lambda calculus representation for it.
///
/// NOTE: Could define more recursively, building expressions to codegen instead
/// NOTE: of format strings. However, this is super ugly without a nicer tree
/// NOTE: builder syntax
pub fn codegen (e: &Expression) -> String {
    match e {
        True => format!(
            "{lambda}{0}.{lambda}{1}.{0}",
            uniqvar("x"),
            uniqvar("y")
        ),
        False => format!(
            "{lambda}{0}.{lambda}{1}.{1}",
            uniqvar("x"),
            uniqvar("y")
        ),
        Num(n) => {
            let f = uniqvar("f");
            let fs = std::iter::repeat(format!("{f} ")).take(*n as usize).collect::<String>();
            format!("{lambda}{0}.{lambda}{1}.{fs}{1}", f, uniqvar("x"))
        }
        Var(v) => v.ident.to_owned(),

        Let(v, d, e) => codegen(
            &Expression::Apply(
                Box::new(Expression::Fn(v.to_owned(), e.to_owned())), 
                d.to_owned()
            )
        ),
        Fn(v, e) => format!(
            "{lambda}{0}.{1}",
            v.ident,
            codegen(e)
        ),
        Apply(e1, e2) => format!(
            ""
        ),

        Not(e) => format!(
            "{lambda}{0}.{lambda}{1}.{lambda}{2}.{0}{2}{1}",
            uniqvar("p"),
            uniqvar("a"),
            uniqvar("b")
        ),
        And(l, r) => format!(
            "{lambda}{0}.{lambda}{1}.{0}{1}{0}",
            uniqvar("p"),
            uniqvar("q")
        ),
        If(p, x, y) => format!(
            "({lambda}{0}.{lambda}{1}.{lambda}{2}.{0}{1}{2}) {3} {4} {5}",
            uniqvar("p"),
            uniqvar("x"),
            uniqvar("y"),
            codegen(p), codegen(x), codegen(y)
        ),

        // succ = \n.\f.\x f(n f x)
        Succ(e) => format!(
            "({lambda}{0}.{lambda}{1}.{lambda}{2}.{1}({0} {1} {2})) {3}",
            uniqvar("n"),
            uniqvar("f"),
            uniqvar("x"),
            codegen(e),
        ),
        
        // pred = \n.\f.\x.n (\g.\h.h(g f)) (\u.x) (\u.u)
        Pred(e) => format!(
            ""
        ),
        // plus = \m.\n.\f.\x.m f (n f x)
        Add(l, r) => format!(
            "({lambda}{0}.{lambda}{1}.{lambda}{2}.{lambda}{3}.{0} {2} ({1} {2} {3})) {4} {5}",
            uniqvar("m"),
            uniqvar("n"),
            uniqvar("f"),
            uniqvar("x"),
            codegen(l),
            codegen(r),
        ),
        // Iszero = \n.n (\x.false) true
        // minus = ...
        // LEQ = \m.\n. Iszero(minus m n)
        // EQ = \m.\n. AND (LEQ m n) (LEQ n m)
        Eq(l, r) => format!(
            ""
        ),

        Fst(e) => format!(
            ""
        ),
        Snd(e) => format!(
            ""
        ),
        Pair(l, r) => format!(
            ""
        ),

        Hd(e) => format!(
            ""
        ),
        Tl(e) => format!(
            ""
        ),
        Nil => format!(
            "{lambda}{0}.{0}",
            uniqvar("x")
        ),
        Cons(i, l) => format!(
            ""
        ),
    }
    
}

/// TODO: Guarantee no name clashes in bound variables
/// NOTE: Only needs to be used when introducing new temporaries (not on user variabless)
fn uniqvar(v: &str) -> String {
    v.to_string()
}

#[test]
fn test_true() {
    let result = codegen(
        &Expression::True
    );
    assert_eq!(result, format!("{lambda}x.{lambda}y.x"));
}

#[test]
fn test_false() {
    let result = codegen(
        &Expression::False
    );
    assert_eq!(result, format!("{lambda}x.{lambda}y.y"));
}

#[test]
fn test_num_zero() {
    let result = codegen(
        &Expression::Num(0)
    );
    assert_eq!(result, format!("{lambda}f.{lambda}x.x"));
}

#[test]
fn test_num_positive() {
    for n in 1..10{
        let result = codegen(
            &Expression::Num(n)
        );
        assert_eq!(result.chars().filter(|c| *c == 'f').count(), (n+1) as usize);
    }
}