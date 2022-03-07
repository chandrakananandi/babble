//! The AST defining a 3D CAD language.

use babble::{
    ast_node::{Arity, AstNode, Expr},
    learn::{LibId, ParseLibIdError},
    teachable::{BindingExpr, DeBruijnIndex, Teachable},
};
use egg::{Rewrite, RecExpr};
use ref_cast::RefCast;
use serde::{Serialize, Deserialize};
use std::{
    fmt::{self, Display, Formatter},
    num::ParseIntError,
    str::FromStr,
};
use lazy_static::lazy_static;


#[derive(Serialize, Deserialize)]
pub(crate) struct CADJson {
    pub(crate) programs: Vec<String>
}

/// The operations/AST nodes of the "Smiley" language.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum CAD {
    Int(i32), // for now, but we will need floats for actual eval.
    Cube,
    Sphere,
    Empty,
    Cuboid,
    Cylinder,
    Translate,
    Scale,
    Rotate,
    Union,
    Diff,
    Lambda,
    Lib(LibId),
    LibVar(LibId),
    Apply,
    Var(DeBruijnIndex),
    Shift,
    Combine
}

impl Arity for CAD {
    fn min_arity(&self) -> usize {
        match self {
            CAD::Int(_) => 0,
            CAD::Cube => 0,
            CAD::Empty => 0,
            CAD::Cuboid => 0,
            CAD::Cylinder => 0,
            CAD::Sphere => 0,
            CAD::Var(_) => 0,
            CAD::Translate => 4,
            CAD::Scale => 4,
            CAD::Rotate => 4,
            CAD::Union => 2,
            CAD::Diff => 2,
            CAD::Lambda => 1,
            CAD::Lib(_) => 2,
            CAD::LibVar(_) => 0,
            CAD::Apply => 2,
            CAD::Shift => 1,
            CAD::Combine => 1,
        }
    }

    fn max_arity(&self) -> Option<usize> {
        match self {
            CAD::Combine => None,
            x => Some(x.min_arity()),
        }
    }
}

impl Display for CAD {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            CAD::Int(n) => n.fmt(f),
            CAD::Cube => f.write_str("cube"),
            CAD::Empty => f.write_str("empty"),
            CAD::Cuboid => f.write_str("cuboid"),
            CAD::Cylinder => f.write_str("cylinder"),
            CAD::Sphere => f.write_str("sphere"),
            CAD::Translate => f.write_str("translate"),
            CAD::Scale => f.write_str("scale"),
            CAD::Rotate => f.write_str("rotate"),
            CAD::Union => f.write_str("union"),
            CAD::Diff => f.write_str("diff"),
            CAD::Lambda => f.write_str("λ"),
            CAD::Lib(ix) => write!(f, "lib {}", ix),
            CAD::LibVar(ix) => write!(f, "{}", ix),
            CAD::Apply => f.write_str("@"),
            CAD::Var(i) => write!(f, "{}", i),
            CAD::Shift => f.write_str("shift"),
            CAD::Combine => f.write_str("combine"),
        }
    }
}


impl FromStr for CAD {
    type Err = ParseIntError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let cad = match s {
            "empty" => Self::Empty,
            "cube" => Self::Cube,
            "cuboid" => Self::Cuboid,
            "cylinder" => Self::Cylinder,
            "sphere" => Self::Sphere,
            "translate" => Self::Translate,
            "rotate" => Self::Rotate,
            "scale" => Self::Scale,
            "union" => Self::Union,
            "diff" => Self::Diff,
            "apply" | "@" => Self::Apply,
            "shift" => Self::Shift,
            "lambda" | "λ" => Self::Lambda,
            _ => {
                if let Ok(index) = s.parse::<DeBruijnIndex>() {
                    Self::Var(index)
                } else if let Ok(lv) = s.parse::<LibId>() {
                    Self::LibVar(lv)
                } else if let Ok(lv) = s
                    .strip_prefix("lib ")
                    .ok_or(ParseLibIdError::NoLeadingL)
                    .and_then(|x| x.parse())
                {
                    Self::Lib(lv)
                } else if let Ok(n) = s.parse::<i32>() {
                    Self::Int(n)
                } else {
                    panic!("Cannot parse {}", s)
                }
            }
        };
        Ok(cad)
    }
}

impl Teachable for CAD {
    fn from_binding_expr<T>(binding_expr: BindingExpr<T>) -> AstNode<Self, T> {
        match binding_expr {
            BindingExpr::Lambda(body) => AstNode::new(Self::Lambda, [body]),
            BindingExpr::Apply(fun, arg) => AstNode::new(Self::Apply, [fun, arg]),
            BindingExpr::Var(index) => AstNode::leaf(Self::Var(DeBruijnIndex(index))),
            BindingExpr::LibVar(ix) => AstNode::leaf(Self::LibVar(ix)),
            BindingExpr::Let(ix, bound_value, body) => {
                AstNode::new(Self::Lib(ix), [bound_value, body])
            }
            BindingExpr::Shift(body) => AstNode::new(Self::Shift, [body]),
        }
    }

    fn as_binding_expr<T>(node: &AstNode<Self, T>) -> Option<BindingExpr<&T>> {
        let binding_expr = match node.as_parts() {
            (Self::Lambda, [body]) => BindingExpr::Lambda(body),
            (Self::Apply, [fun, arg]) => BindingExpr::Apply(fun, arg),
            (&Self::Var(DeBruijnIndex(index)), []) => BindingExpr::Var(index),
            (Self::Lib(ix), [bound_value, body]) => BindingExpr::Let(*ix, bound_value, body),
            (Self::Shift, [body]) => BindingExpr::Shift(body),
            _ => return None,
        };
        Some(binding_expr)
    }
}


// TODO: add CAD rewrites from Szalinski
lazy_static! {
    pub(crate) static ref LIFT_LIB_REWRITES: &'static [Rewrite<AstNode<CAD>, ()>] = vec![ ]
    .leak();
}
