
# MiniML Compiler 

## Glossary

- MiniML:
  MiniML is a tiny subset of the [ML language](https://en.wikipedia.org/wiki/ML_(programming_language)) (of Standard ML and OCaml fame).
  Different authors include/exclude different features. 
  When we say MiniML, we mean the version as used in [Warwick's POPL](https://warwick.ac.uk/fac/sci/dcs/teaching/modules/cs349/).
  
## Colloquially, the MiniML grammar

### Term grammar
The term grammar is the set of valid MiniML expressions. Everything is an
expression in MiniML: there are no statements.

Whitespace is never significant, except for comments which stretch until newline.

Comments are specified by two dashes `--`.

```bnf
e :=
  -- Literals
  x                        -- Variables.
  | True | False           -- Booleans.
  | 0 | 1 | 2 | ...        -- Numerics.

  -- Binders
  | let x = e1 in e2       -- Let bindings.
  | fn x . e               -- Functions (a.k.a. abstractions)

  -- Boolean fundamental ops
  | e1 and e2              -- Conjunction
  | not(e1)                -- Negation
  | if e0 then e1 else e2  -- Conditional
  
  -- Numeric fundamental ops
  | succ(e) | pred(e)      -- +1, -1
  | e1 + e2                -- Sum
  | e1 == e2               -- Equal?
  | zero?(e)               -- Is zero?

  -- Pairs
  | <e1,  e2>              -- A pair of values (possibly different types)
  | fst(e) | snd(e)        -- Element-wise access to pairs

  -- Lists
  | nil                    -- The empty list
  | e1 :: e2               -- Cons, or list join
  | hd(e) | tl(e)          -- The first entry of the list & the remainder of the list
```

### Type grammar
The type grammar specifies the list of valid types.

```bnf
t := num
  | bool
  | t1 -> t2               -- The type of functions
  | t1 * t2                -- The type of pairs
  | t list                 -- The type of lists
  | y                      -- Type variables
```

### Type schemes
Type schemes cannot be entered or used directly by the programmer, they are
purely a tool of the type system.

Type schemes occur when implicit polymorphism is required by `let` bindings.

```bnf
s := forall a . t
```


## Stretch goals

### Recursion 
In a general enough implementation, recursion would 'fall out' using the Y combinator.

However, it might be best (simpler, more performant, more ergonomic) to introduce the `mu` binder.

Based on the `mu` or Y binder [in
PCF](https://en.wikipedia.org/wiki/Programming_Computable_Functions#Semantics),
`mu` is akin to `fn`, but introduces an abstraction that, when called, recurses
into the body of the `mu` binding.

See [the sample definition of recursive Fibonacci](./examples/fib.ml).

### Recursive bindings with letrec 

## Yacc grammar

```c
%{ 
// Auxililary declarations (in C)


%}

// YACC declarations section

%token EOF
// %token SEMISEMI
%token LET
%token EQUAL
%token MINUS
%token PLUS
%token TIMES
%token LESS
%token IF
%token THEN
%token ELSE
%token FUN
%token LPAREN
%token COLON
%token RPAREN
%token IS
%token TRUE
%token FALSE
%token TINT
%token TBOOL
%token TARROW

%token SEMICOLON
%token DIGIT
%token ALPHA

%%

// Rules section

file = EOF // produces []
     | (e: expr) EOF // produces [Expr e]
     | (e: expr) SEMISMI (lst: file) // produces Expr e :: lst
     | (ds: nonempty_list(def)) SEMISEMI (lst: file) // produces ds @ lst
     | (ds: nonempty_list(def)) EOF // produces ds

toplevel = (d: def) SEMISEMI // produces d
         | (e: expr) SEMISEMI // produces Expr e

def = LET (x: VAR) EQUAL (e: expr) // produces Def (x, e)

expr = mark_position(plain_expr)

plain_expr = (e: plain_app_expr) // Produces e
           | MINUS (n: INT) // Produces Int (-n)
           | (e1: expr) PLUS (e2: expr) // Produces Plus(e1, e2)
           | (e1: expr) MINUS (e2: expr) // Produces Minus(e1, e2)
           | (e1: expr) TIMES (e2: expr) // Produces Times(e1, e2)
           | (e1: expr) EQUAL (e2: expr) // Produces Equal(e1, e2)
           | (e1: expr) LESS (e2: expr) // Produces Less(e1, e2)
           | IF (e1: expr) THEN (e2: expr) ELSE (e3: expr) // Produces If (e1, e2, e3)
           | FUN (x: VAR) LPAREN (f: VAR) COLON (t1: ty) RPAREN COLON (t2: y) IS (e: expr) // Produces Fun(x, f, t1, t2, e)
        
app_expr =  mark_position(plain_app_expr)

plain_app_expr = (e: plain_simple_expr) // Produces e
               | (e1: app_expr) (e2: simple_expr) // Produces Apply(e1, e2)

simple_expr = mark_position(plain_simpl_expr)

plain_simple_expr = (x: VAR) // Produces Var x
                  | TRUE // Produces Bool true
                  | FALSE // Produces Bool false
                  | (n: INT) // Produces Int n
                  | LPAREN (e: plain_expr) RPAREN // Produces e

ty = TBOOL // Produces TBool
   | TINT // Produces TInt
   | (t1: ty) TARROW (t2: ty) // Produces TArrow(t1, t2)
   | LPAREN (t: ty) RPAREN // Produces t

SEMISEMI = SEMICOLON SEMICOLON
INT = DIGIT+

%%

// Auxiliary functions

yyerror(char const *s) {
    printf("yyerror %s", s);
}

yylex() {
    char c;
    c = getchar();

    // Single Character Tokens
    //token EQUAL
    if (c == '=') {
        return EQUAL;
    }
    //token MINUS
    if (c == '-') {
        return MINUS;
    }
    //token PLUS
    if (c == '+') {
        return PLUS;
    }
    //token TIMES
    if (c == '*') {
        return TIMES;
    }
    //token LESS
    if (c == '<') {
        return LESS;
    }
    //token LPAREN
    if (c == '(') {
        return LPAREN;
    }
    //token COLON
    if (c == ':') {
        return COLON;
    }
    //token RPAREN
    if (c == ')') {
        return RPAREN;
    }
    //token EOF
    if (c == '\0') {
        return EOF;
    }
    //token SEMICOLON
    if (c == ';') {
        return SEMICOLON;
    }

    //token LET
    //token IF
    //token THEN
    //token ELSE
    //token FUN
    //token IS
    //token TRUE
    //token FALSE
    //token TINT
    //token TBOOL
    //token TARROW

    //token DIGIT
    if(isdigit(c)) {
        yylval = c - '0';
        return DIGIT;
    }

    //token ALPHA
    if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) {
        yylval = c;
        return ALPHA;
    }

    // Skip whitespace or new lines
    if (c == ' ' || c == '\n' || c == '\r') {
        yylex();
    }


    return c;
}

main() {
    yyparse();
    return 1;
}
```
