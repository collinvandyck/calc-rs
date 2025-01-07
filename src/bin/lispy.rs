use anyhow::{Context, Result, bail};
use core::panic;
use std::rc::Rc;

fn main() {
    assert_eq!(eval("(+ 3 8)").unwrap().to_f64(), Some(11.0));
    assert_eq!(eval("(+ 3 (* 8 4))").unwrap().to_f64(), Some(35.0));
}

type SharedExpr = Rc<Expr>;

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum TokTyp {
    LeftParen,
    RightParen,
    Plus,
    Star,
    Slash,
    Minus,
    Number,
}

#[derive(Clone, Debug, PartialEq)]
enum Value {
    F64(f64),
}

impl Value {
    fn to_f64(&self) -> Option<f64> {
        match self {
            Value::F64(v) => Some(*v),
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
            (Value::F64(v1), Value::F64(v2), TokTyp::Plus) => Ok(Value::F64(v1 + v2)),
            (Value::F64(v1), Value::F64(v2), TokTyp::Minus) => Ok(Value::F64(v1 - v2)),
            (Value::F64(v1), Value::F64(v2), TokTyp::Star) => Ok(Value::F64(v1 * v2)),
            (Value::F64(v1), Value::F64(v2), TokTyp::Slash) => Ok(Value::F64(v1 / v2)),
            _ => bail!("invalid math: {add:?}"),
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
        self.group()
    }
    fn group(&mut self) -> Result<SharedExpr> {
        if self.matches([TokTyp::LeftParen]) {
            let next = self.math()?;
            self.consume(TokTyp::RightParen)?;
            return Ok(Rc::new(Expr::Group(Rc::new(GroupExpr { expr: next }))));
        }
        self.math()
    }
    fn math(&mut self) -> Result<SharedExpr> {
        if self.matches([TokTyp::Plus, TokTyp::Minus, TokTyp::Slash, TokTyp::Star]) {
            let op = self.prev().typ;
            let left = self.parse()?;
            let right = self.parse()?;
            let add = Expr::Math(Rc::new(MathExpr { left, right, op }));
            return Ok(Rc::new(add));
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
        for t in typ.into_iter() {
            if self.cur().typ == t {
                self.advance();
                return true;
            }
        }
        false
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
        while !self.eof() && self.cur().is_numeric() {
            self.advance();
        }
        let s = self.chars[cur..self.pos]
            .iter()
            .collect::<String>();
        let val = s.parse().context("parse f64")?;
        self.toks.push(Tok {
            lexeme: s,
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
