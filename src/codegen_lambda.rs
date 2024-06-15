
use std::fmt::format;

use crate::parser;
use parser::{Expression, Variable};
use parser::Expression::*;

static lambda: &'static str  = "λ";

/// Given an expression, generate a Lambda calculus representation for it.
///
/// NOTE: Could define more recursively, building expressions to codegen instead
/// NOTE: of format strings. However, this is super ugly without a nicer tree
/// NOTE: builder syntax
pub fn codegen (e: &Expression) -> String {
    match e {
        // -- Literals
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


        // -- Binders
        Let(v, d, e) => codegen(
            &Apply(
                Box::new(Fn(v.to_owned(), e.to_owned())), 
                d.to_owned()
            )
        ),
        Fn(v, e) => format!(
            "{lambda}{0}.{1}",
            v.ident,
            codegen(e)
        ),


        // -- Application
        Apply(e1, e2) => format!(
            "({0}) ({1})",
            codegen(e1),
            codegen(e2),
        ),

        // -- Boolean fundamental ops
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

        // -- Numeric fundamental ops
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
            "({lambda}{0}.{lambda}{1}.{lambda}{2}.{0} ({lambda}{3}.{lambda}{4}.{4} ({3} {1})) ({lambda}{5}.{2}) ({lambda}{5}.{5})) {6}",
            uniqvar("n"),
            uniqvar("f"),
            uniqvar("x"),
            uniqvar("g"),
            uniqvar("h"),
            uniqvar("u"),
            codegen(e),
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

        // -- Pairs and Lists
        Fst(e) | Hd(e) => format!(
            "{lambda}{0}.{lambda}{1}.{0}",
            uniqvar("x"),
            uniqvar("y"),
        ),
        Snd(e) | Tl(e) => format!(
            "{lambda}{0}.{lambda}{1}.{1}",
            uniqvar("x"),
            uniqvar("y"),
        ),
        Pair(l, r) | Cons(l, r) => format!(
            "{lambda}{0}.{lambda}{1}.{lambda}{2}.{2}{0}{1}",
            uniqvar("x"),
            uniqvar("y"),
            uniqvar("z"),
        ),
        Nil => format!(
            "{lambda}{0}.{0}",
            uniqvar("x")
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
    assert_eq!(codegen(&True), format!("{lambda}x.{lambda}y.x"));
}

#[test]
fn test_false() {
    assert_eq!(codegen(&False), format!("{lambda}x.{lambda}y.y"));
}

#[test]
fn test_num_zero() {
    assert_eq!(codegen(&Num(0)), format!("{lambda}f.{lambda}x.x"));
}

#[test]
fn test_num_positive() {
    for n in 1..10{
        let result = codegen(&Num(n));
        assert_eq!(result.chars().filter(|c| *c == 'f').count(), (n+1) as usize);
    }
}

#[test]
fn test_var() {
    assert_eq!(codegen(&Var(Variable { ident: "foo".to_string() })), "foo");
}

#[test]
fn test_let() {
    let parameter = Variable { ident: "x".to_string() };
    let expr = Let(
        parameter.clone(),
        Box::new(Num(1)),
        Box::new(Var(parameter.clone())),
    );
    assert_eq!(codegen(&expr), format!("({lambda}x.x) ({lambda}f.{lambda}x.f x)"));
}

#[test]
fn test_fn() {
    let parameter = Variable { ident: "foo".to_string() };
    let body = Box::new(Var(parameter.clone()));
    assert_eq!(codegen(&Fn(parameter, body)), format!("{lambda}foo.foo"));
}

#[test]
fn test_apply() {
    let parameter = Variable { ident: "x".to_string() };
    let expr = Apply(
        Box::new(Var(parameter.clone())),
        Box::new(Var(parameter))
    );
    assert_eq!(codegen(&expr), format!("(x) (x)"))
}

// #[test]
// fn test_() {
//     assert_eq!();
// }