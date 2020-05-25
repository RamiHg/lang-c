use ast::*;
use span::{Node, Span};

#[cfg_attr(test, derive(Debug, PartialEq, Clone))]
pub enum Operation {
    Member(Node<MemberOperator>, Node<Identifier>),
    Unary(Node<UnaryOperator>),
    Binary(Node<BinaryOperator>, Node<Expression>),
    Call(Vec<Node<Expression>>),
}

fn apply_op(a: Node<Expression>, op: Node<Operation>) -> Node<Expression> {
    let span = Span::span(a.span.start, op.span.end);
    let expr = match op.node {
        Operation::Member(op, id) => Expression::Member(Box::new(Node::new(
            MemberExpression {
                operator: op,
                expression: Box::new(a),
                identifier: id,
            },
            span,
        ))),
        Operation::Unary(op) => Expression::UnaryOperator(Box::new(Node::new(
            UnaryOperatorExpression {
                operator: op,
                operand: Box::new(a),
            },
            span,
        ))),
        Operation::Binary(op, b) => Expression::BinaryOperator(Box::new(Node::new(
            BinaryOperatorExpression {
                operator: op,
                lhs: Box::new(a),
                rhs: Box::new(b),
            },
            span,
        ))),
        Operation::Call(args) => Expression::Call(Box::new(Node::new(
            CallExpression {
                callee: Box::new(a),
                arguments: args,
            },
            span,
        ))),
    };

    Node::new(expr, span)
}

pub fn apply_ops(ops: Vec<Node<Operation>>, expr: Node<Expression>) -> Node<Expression> {
    ops.into_iter().fold(expr, apply_op)
}

pub fn concat<T>(mut a: Vec<T>, b: Vec<T>) -> Vec<T> {
    a.extend(b);
    a
}

pub fn infix(
    node: Node<()>,
    op: BinaryOperator,
    lhs: Node<Expression>,
    rhs: Node<Expression>,
) -> Node<Expression> {
    let span = Span::span(lhs.span.start, rhs.span.end);
    Node::new(
        Expression::BinaryOperator(Box::new(Node::new(
            BinaryOperatorExpression {
                operator: Node::new(op, node.span),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            span,
        ))),
        span,
    )
}

pub fn with_ext(mut d: Node<Declarator>, e: Option<Vec<Node<Extension>>>) -> Node<Declarator> {
    if let Some(e) = e {
        d.node.extensions.extend(e);
    }
    d
}

pub fn ts18661_float(binary: bool, width: usize, extended: bool) -> TS18661FloatType {
    TS18661FloatType {
        format: match (binary, extended) {
            (true, false) => TS18661FloatFormat::BinaryInterchange,
            (true, true) => TS18661FloatFormat::BinaryExtended,
            (false, false) => TS18661FloatFormat::DecimalInterchange,
            (false, true) => TS18661FloatFormat::DecimalExtended,
        },
        width: width,
    }
}

pub fn int_suffix(mut s: &str) -> Result<IntegerSuffix, &'static str> {
    let mut l = IntegerSize::Int;
    let mut u = false;
    let mut i = false;

    while s.len() > 0 {
        if l == IntegerSize::Int && (s.starts_with("ll") || s.starts_with("LL")) {
            l = IntegerSize::LongLong;
            s = &s[2..];
        } else if l == IntegerSize::Int && (s.starts_with("l") || s.starts_with("L")) {
            l = IntegerSize::Long;
            s = &s[1..];
        } else if !u && (s.starts_with("u") || s.starts_with("U")) {
            u = true;
            s = &s[1..];
        } else if !i
            && (s.starts_with("i")
                || s.starts_with("I")
                || s.starts_with("j")
                || s.starts_with("J"))
        {
            i = true;
            s = &s[1..];
        } else {
            return Err("integer suffix");
        }
    }

    Ok(IntegerSuffix {
        size: l,
        unsigned: u,
        imaginary: i,
    })
}

pub trait Concat<T> {
    fn concat(self) -> Vec<T>;
}

impl<T> Concat<T> for (T, Vec<T>, Vec<T>) {
    fn concat(self) -> Vec<T> {
        let mut result = Vec::with_capacity(1 + self.1.len() + self.2.len());
        result.push(self.0);
        result.extend(self.1);
        result.extend(self.2);
        result
    }
}

impl<T> Concat<T> for (Vec<T>, T, Vec<T>) {
    fn concat(mut self) -> Vec<T> {
        self.0.reserve(1 + self.2.len());
        self.0.push(self.1);
        self.0.extend(self.2);
        self.0
    }
}

impl<T> Concat<T> for (Vec<T>, Vec<T>, T) {
    fn concat(mut self) -> Vec<T> {
        self.0.reserve(1 + self.1.len());
        self.0.extend(self.1);
        self.0.push(self.2);
        self.0
    }
}

impl<T> Concat<T> for (T, Vec<T>, T) {
    fn concat(self) -> Vec<T> {
        let mut result = Vec::with_capacity(2 + self.1.len());
        result.push(self.0);
        result.extend(self.1);
        result.push(self.2);
        result
    }
}

impl<T> Concat<T> for (Vec<T>, Vec<T>, Vec<T>) {
    fn concat(mut self) -> Vec<T> {
        self.0.reserve(self.1.len() + self.2.len());
        self.0.extend(self.1);
        self.0.extend(self.2);
        self.0
    }
}
