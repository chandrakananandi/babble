//! The JSON interface to Dream&shy;Coder.

use super::{expr::DcExpr, types::Type};
use serde::{Deserialize, Serialize};

/// The input format of the `compression` tool.
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, PartialOrd, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CompressionInput {
    // #[serde(rename = "CPUs")]
    // pub cpus: u32,
    // pub arity: u32,
    // pub verbose: bool,
    // #[serde(rename = "collect_data")]
    // pub collect_data: bool,
    // pub bs: u32,
    // pub aic: f64,
    // pub structure_penalty: f64,
    // pub top_k: u32,
    #[serde(rename = "DSL")]
    pub dsl: Grammar,
    pub frontiers: Vec<Frontier>,
}

/// The output format of the `compression` tool.
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, PartialOrd, Default, Serialize, Deserialize)]
pub struct CompressionOutput {
    #[serde(rename = "DSL")]
    pub dsl: Grammar,
    pub frontiers: Vec<Frontier>,
}

/// The primitives and learned functions for the language.
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, PartialOrd, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Grammar {
    pub log_variable: f64,
    pub productions: Vec<Production>,
}

/// A primitive or learned function.
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Production {
    pub log_probability: f64,
    pub expression: DcExpr,
}

/// A particular task for `compression` to examine.
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct Frontier {
    pub task: Option<String>,
    pub request: Type,
    pub programs: Vec<Program>,
}

/// A particular program that `compression` will try to compress.
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Program {
    pub log_likelihood: f64,
    pub program: DcExpr,
}

#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, PartialOrd, Default, Serialize, Deserialize)]
pub struct Info {
    pub iteration: usize,
    pub num_learned: usize,
    pub new_grammar: Grammar,
}
