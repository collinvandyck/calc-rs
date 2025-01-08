use anyhow::{Context, Result, bail};
use itertools::Itertools;
use std::rc::Rc;

fn main() {
    macro_rules! assert_eval {
        ($e:expr, $ex: expr) => {
            assert_eq!(eval($e).unwrap().must_f64(), $ex);
        };
    }
    assert_eval!("3", 3.0);
    assert_eval!("((((3))))", 3.0);
    assert_eval!("(3) * 4", 12.0);
    assert_eval!("3 * ((4/2) + (3 + 1 - 1))", 15.0);
    assert_eval!("3+4", 7.0);
    assert_eval!("3+4 + 3", 10.0);
    assert_eval!("3*(4 + 3)", 21.0);
}

type SharedExpr = Rc<Expr>;

#[derive(Clone, Debug)]
enum Expr {
    Group(Rc<GroupExpr>),
    Math(Rc<MathExpr>),
    Value(Rc<Value>),
}

impl Expr {
    fn visit<V: ExprVisitor>(&self, v: &mut V) -> Result<V::Item> {
        match self {
            Expr::Group(rc) => v.visit_group(rc.clone()),
            Expr::Math(rc) => v.visit_math(rc.clone()),
            Expr::Value(rc) => v.visit_value(rc.clone()),
        }
    }
}

#[derive(Clone, Debug)]
struct MathExpr {
    op: TokTyp,
    left: SharedExpr,
    right: SharedExpr,
}

#[derive(Debug, Clone)]
struct GroupExpr {
    expr: SharedExpr,
}

#[derive(Clone, Debug)]
struct Tok {
    lexeme: String,
    typ: TokTyp,
    val: Option<Value>,
}

impl std::fmt::Display for Tok {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Tok { lexeme, val, .. } = self;
        if let Some(val) = val {
            write!(f, "{val}")
        } else {
            write!(f, "{lexeme}")
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum TokTyp {
    LeftParen,
    RightParen,
    Plus,
    Star,
    Slash,
    Minus,
    Number,
}

impl std::fmt::Debug for TokTyp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            TokTyp::LeftParen => "(",
            TokTyp::RightParen => ")",
            TokTyp::Plus => "+",
            TokTyp::Star => "*",
            TokTyp::Slash => "/",
            TokTyp::Minus => "-",
            TokTyp::Number => "NUM",
        };
        write!(f, "{s}")
    }
}

#[derive(Clone, Debug, PartialEq)]
enum Value {
    F64(f64),
}

impl Value {
    fn must_f64(&self) -> f64 {
        match self {
            Value::F64(v) => *v,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::F64(v) => write!(f, "{v}"),
        }
    }
}

fn eval(s: &str) -> Result<Value> {
    let expr = parse(s)?;
    let mut inter = Interpreter::new(expr);
    inter.interpret()
}

fn parse(s: &str) -> Result<SharedExpr> {
    let toks = lex(s)?;
    Parser::new(toks).parse()
}

trait ExprVisitor {
    type Item;
    fn visit_group(&mut self, group: Rc<GroupExpr>) -> Result<Self::Item>;
    fn visit_math(&mut self, add: Rc<MathExpr>) -> Result<Self::Item>;
    fn visit_value(&mut self, val: Rc<Value>) -> Result<Self::Item>;
}

struct Interpreter {
    expr: SharedExpr,
}

impl Interpreter {
    fn new(expr: SharedExpr) -> Self {
        Self { expr }
    }
    fn interpret(&mut self) -> Result<Value> {
        self.expr.clone().visit(self)
    }
}

impl ExprVisitor for Interpreter {
    type Item = Value;

    fn visit_group(&mut self, group: Rc<GroupExpr>) -> Result<Self::Item> {
        group.expr.visit(self)
    }

    fn visit_math(&mut self, add: Rc<MathExpr>) -> Result<Self::Item> {
        let MathExpr { left, right, op } = add.as_ref();
        let left = left.visit(self)?;
        let right = right.visit(self)?;
        match (left, right, *op) {
            (Value::F64(v1), Value::F64(v2), op) => {
                match op {
                    TokTyp::Plus => Ok(Value::F64(v1 + v2)),
                    TokTyp::Minus => Ok(Value::F64(v1 - v2)),
                    TokTyp::Star => Ok(Value::F64(v1 * v2)),
                    TokTyp::Slash => Ok(Value::F64(v1 / v2)),
                    _ => bail!("invalid op: {op:?}"),
                }
            }
        }
    }

    fn visit_value(&mut self, val: Rc<Value>) -> Result<Self::Item> {
        Ok(val.as_ref().clone())
    }
}

struct Parser {
    toks: Vec<Tok>,
    pos: usize,
}

impl Parser {
    fn new(toks: Vec<Tok>) -> Self {
        Self { toks, pos: 0 }
    }
    fn parse(&mut self) -> Result<SharedExpr> {
        let res = self.expr()?;
        if !self.eof() {
            bail!("parse: trailing input: {}", self.rest_str());
        }
        Ok(res)
    }
    fn expr(&mut self) -> Result<SharedExpr> {
        self.math()
    }
    fn math(&mut self) -> Result<SharedExpr> {
        let mut left = self.group()?;
        loop {
            if self.matches([TokTyp::Plus, TokTyp::Minus, TokTyp::Slash, TokTyp::Star]) {
                let op = self.prev().typ;
                let right = self.expr()?;
                left = Rc::new(Expr::Math(Rc::new(MathExpr { left, right, op })));
            } else {
                break;
            }
        }
        Ok(left)
    }
    fn group(&mut self) -> Result<SharedExpr> {
        if self.matches([TokTyp::LeftParen]) {
            let expr = self.expr()?;
            self.consume(TokTyp::RightParen)?;
            return Ok(Rc::new(Expr::Group(Rc::new(GroupExpr { expr }))));
        }
        self.value()
    }
    fn value(&mut self) -> Result<SharedExpr> {
        if self.matches([TokTyp::Number]) {
            let prev = self.prev();
            let val = prev.val.context("expected value for number")?;
            let val = Expr::Value(Rc::new(val));
            return Ok(Rc::new(val));
        }
        bail!("expected value");
    }
    fn prev(&self) -> Tok {
        self.toks[self.pos - 1].clone()
    }
    fn consume(&mut self, typ: TokTyp) -> Result<()> {
        if !self.matches([typ]) {
            bail!("expected {typ:?}");
        }
        Ok(())
    }
    fn matches(&mut self, typ: impl IntoIterator<Item = TokTyp>) -> bool {
        if !self.eof() {
            for t in typ.into_iter() {
                if self.cur().typ == t {
                    self.advance();
                    return true;
                }
            }
        }
        false
    }
    #[allow(unused)]
    fn debug(&mut self, op: &str) {
        println!("{op} {}", self.rest_str());
    }
    fn rest_str(&self) -> String {
        self.rest()
            .iter()
            .map(ToString::to_string)
            .join(", ")
    }
    fn rest(&self) -> &[Tok] {
        &self.toks[self.pos..]
    }
    fn eof(&self) -> bool {
        self.pos >= self.toks.len()
    }
    fn advance(&mut self) {
        self.pos += 1;
    }
    fn cur(&self) -> &Tok {
        &self.toks[self.pos]
    }
}

struct Lexer {
    chars: Vec<char>,
    toks: Vec<Tok>,
    pos: usize,
}

impl Lexer {
    fn new(s: &str) -> Self {
        Self {
            chars: s.chars().collect(),
            pos: 0,
            toks: Vec::new(),
        }
    }
    fn lex(&mut self) -> Result<Vec<Tok>> {
        while !self.eof() {
            let ch = self.cur();
            match ch {
                '(' => self.tok(TokTyp::LeftParen),
                ')' => self.tok(TokTyp::RightParen),
                '+' => self.tok(TokTyp::Plus),
                '*' => self.tok(TokTyp::Star),
                '/' => self.tok(TokTyp::Slash),
                '-' => self.tok(TokTyp::Minus),
                '0'..'9' => self.number()?,
                ' ' => self.advance(),
                _ => panic!("unexpected token: {ch:?}"),
            }
        }
        Ok(self.toks.clone())
    }
    fn number(&mut self) -> Result<()> {
        let cur = self.pos;
        let mut val = 0.0;
        while !self.eof() && self.cur().is_numeric() {
            val = val * 10.0 + self.cur().to_digit(10).unwrap() as f64;
            self.advance();
        }
        self.toks.push(Tok {
            lexeme: self.chars[cur..self.pos].iter().collect(),
            typ: TokTyp::Number,
            val: Some(Value::F64(val)),
        });
        Ok(())
    }
    fn tok(&mut self, typ: TokTyp) {
        let lexeme = String::from(self.cur());
        let tok = Tok { lexeme, typ, val: None };
        self.toks.push(tok);
        self.advance();
    }
    fn cur(&self) -> char {
        self.chars[self.pos]
    }
    fn advance(&mut self) {
        self.pos += 1;
    }
    fn eof(&self) -> bool {
        self.pos >= self.chars.len()
    }
}

fn lex(s: &str) -> Result<Vec<Tok>> {
    Lexer::new(s).lex()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        main();
    }
}
