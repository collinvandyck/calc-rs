use anyhow::{Result, bail};
use itertools::Itertools;

fn main() {
    assert_eq!(eval("3").unwrap(), 3.0);
    assert_eq!(eval("3 + 4").unwrap(), 7.0);
    assert_eq!(eval("3 + 4 + 3").unwrap(), 10.0);
    assert_eq!(eval("3 * (4 + 3)").unwrap(), 21.0);
    assert_eq!(eval("3 * ((4/2) + (3))").unwrap(), 15.0);
}

#[derive(Debug, Clone)]
enum Tok {
    Num(f64),
    Op(char),
}

struct Stack(Vec<Vec<Tok>>);

impl Stack {
    fn new() -> Self {
        Self(vec![vec![]])
    }
    fn push_grp(&mut self) {
        self.0.push(vec![]);
    }
    fn pop_grp(&mut self) -> Vec<Tok> {
        self.0.pop().unwrap()
    }
    fn push(&mut self, tok: Tok) {
        self.0.last_mut().unwrap().push(tok);
    }
    fn len(&self) -> usize {
        self.0.last().unwrap().len()
    }
    fn drain(&mut self, v: usize) -> Vec<Tok> {
        self.0
            .last_mut()
            .unwrap()
            .drain(0..v)
            .collect_vec()
    }
}

fn eval(s: &str) -> Result<f64> {
    let chars: Vec<char> = s.chars().collect_vec();
    let mut pos = 0;
    let mut stack = Stack::new();
    while pos < chars.len() {
        let ch = chars[pos];
        match ch {
            '(' => {
                stack.push_grp();
                pos += 1;
            }
            ')' => {
                match stack.pop_grp().as_slice() {
                    [tok] => {
                        stack.push(tok.clone());
                        pos += 1;
                    }
                    xs => bail!("bad group expr: {xs:?}"),
                }
            }
            '0'..'9' => {
                let cur = pos;
                while pos < chars.len() && chars[pos].is_numeric() {
                    pos += 1;
                }
                stack.push(Tok::Num(
                    chars[cur..pos]
                        .iter()
                        .collect::<String>()
                        .parse()?,
                ));
            }
            '+' | '-' | '*' | '/' => {
                stack.push(Tok::Op(ch));
                pos += 1;
            }
            ch if ch.is_whitespace() => pos += 1,
            _ => bail!("bad ch: {ch:?}"),
        }
        if stack.len() >= 3 {
            match stack.drain(3).as_slice() {
                [Tok::Num(v1), Tok::Op(op), Tok::Num(v2)] => {
                    let num = match op {
                        '+' => v1 + v2,
                        '-' => v1 - v2,
                        '*' => v1 * v2,
                        '/' => v1 / v2,
                        _ => bail!("invalid op: {op:?}"),
                    };
                    stack.push(Tok::Num(num));
                }
                rest => bail!("invalid stack state: {rest:?}"),
            }
        }
    }
    match stack.pop_grp().as_slice() {
        [Tok::Num(v)] => Ok(*v),
        xs => bail!("invalid state: {xs:?}"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eval() {
        main();
    }
}
