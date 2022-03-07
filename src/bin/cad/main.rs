#![warn(
    clippy::all,
    clippy::pedantic,
    anonymous_parameters,
    elided_lifetimes_in_paths,
    missing_copy_implementations,
    missing_debug_implementations,
    single_use_lifetimes,
    trivial_casts,
    unreachable_pub,
    unused_lifetimes
)]
#![allow(clippy::non_ascii_literal)]

use babble::{
    ast_node::{AstNode, Expr},
    extract::{
        beam::{less_dumb_extractor, PartialLibCost},
        lift_libs, true_cost,
    },
    learn::LearnedLibrary,
    sexp::{self, Sexp},
};
use clap::Clap;
use egg::{AstSize, CostFunction, EGraph, LanguageChildren, RecExpr, Rewrite, Runner};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use serde::__private::de::IdentifierDeserializer;
use serde_json::Value;
use std::{
    convert::{self, TryInto, TryFrom},
    fs::{self, File},
    io::{self, Read},
    path::PathBuf,
    time::{Duration, Instant},
};

use crate::lang::{CADJson, CAD};
mod lang;

#[allow(clippy::struct_excessive_bools)]
#[derive(Clap)]
#[clap(version, author, about)]
struct Opts {
    /// The input file. If no file is specified, reads from stdin.
    #[clap(parse(from_os_str))]
    file: Option<PathBuf>,

    /// Enables pretty-printing of JSON output.
    #[clap(long)]
    pretty: bool,

    /// The maximum number of programs to anti-unify.
    #[clap(long)]
    limit: Option<usize>,
}

fn main() {
    env_logger::init();
    let opts: Opts = Opts::parse();

    let input = opts
        .file
        .map_or_else(
            || {
                let mut buf = String::new();
                io::stdin().read_to_string(&mut buf).map(|_| buf)
            },
            fs::read_to_string,
        )
        .expect("Error reading input");

    let input: CADJson = serde_json::from_str(&input).expect("Error parsing JSON input");
    let mut wtr = csv::Writer::from_path("target/cad-res.csv").unwrap();

    let run_beam_exp = |limit: usize, final_beams, inter_beams, wtr: &mut csv::Writer<fs::File>| {
        if final_beams > inter_beams {
            return;
        }

        println!(
            "limit: {}, final_beams: {}, inter_beams: {}",
            limit, final_beams, inter_beams
        );

        let start_time = Instant::now();
        let timeout = Duration::from_secs(60 * 100000);

        let mut aeg = EGraph::new(PartialLibCost::new(final_beams, inter_beams));
        let programs: Vec<Expr<CAD>> = input
            .programs
            .clone()
            .into_iter()
            .map(|p| Expr::try_from(Sexp::parse(&p).unwrap()).unwrap())
            .take(limit)
            .collect();
        let initial_cost: usize = programs.iter().map(Expr::len).sum();
        let mut roots = Vec::with_capacity(programs.len());
        
        for expr in programs.iter().cloned().map(RecExpr::from) {
            let root = aeg.add_expr(&expr);
            roots.push(root);
        }
        aeg.rebuild();

        let runner = Runner::<_, _, ()>::new(PartialLibCost::new(final_beams, inter_beams))
            .with_egraph(aeg)
            .with_time_limit(timeout.saturating_sub(start_time.elapsed()));

        let aeg = runner.egraph;
        println!("Compressing {} programs", roots.len());
        println!("Starting cost: {}", initial_cost);

        let learned_lib = LearnedLibrary::from(&aeg);
        let lib_rewrites: Vec<_> = learned_lib.rewrites().collect();

        println!("Found {} antiunifications", lib_rewrites.len());

        println!("Anti-unifying");
        let runner = Runner::<_, _, ()>::new(PartialLibCost::new(final_beams, inter_beams))
            .with_egraph(aeg.clone())
            .with_iter_limit(1)
            .with_time_limit(timeout.saturating_sub(start_time.elapsed()))
            .with_node_limit(1_000_000)
            .run(lib_rewrites.iter());

        println!("Stop reason: {:?}", runner.stop_reason.unwrap());

        let mut egraph = runner.egraph;
        println!("Number of nodes: {}", egraph.total_size());

        // Add the root combine node.
        let root = egraph.add(AstNode::new(CAD::Combine, roots.iter().copied()));

        let mut cs = egraph[egraph.find(root)].data.clone();
        cs.set.sort_unstable_by_key(|elem| elem.full_cost);

        println!("upper bound ('full') cost: {}", cs.set[0].full_cost);
        println!();

        println!("extracting (final, lifted libs)");
        let (lifted, final_cost) = cs
            .set
            .par_iter()
            .map(|ls| {
                // Add the root combine node again
                let mut fin = Runner::<_, _, ()>::new(PartialLibCost::new(0, 0))
                    .with_egraph(aeg.clone())
                    .with_iter_limit(1)
                    .run(
                        lib_rewrites
                            .iter()
                            .enumerate()
                            .filter(|(i, _)| {
                                ls.libs.iter().any(|x| *i == x.0 .0)
                            })
                            .map(|x| x.1),
                    )
                    .egraph;

                let root = fin.add(AstNode::new(CAD::Combine, roots.iter().copied()));
                let best = less_dumb_extractor(&fin, root);

                let lifted = lift_libs(best);
                let final_cost = true_cost(lifted.clone()) - 1;

                (lifted, final_cost)
            })
            .min_by_key(|x: &(RecExpr<AstNode<CAD>>, usize)| x.1)
            .unwrap();

        println!("{}", lifted.pretty(100));
        println!("final cost: {}", final_cost);
        println!();

        wtr.serialize((
            limit,
            "beam",
            timeout.as_secs(),
            final_beams,
            inter_beams,
            final_cost,
            start_time.elapsed().as_secs_f64(),
        ))
        .unwrap();
        wtr.flush().unwrap();
    };

    run_beam_exp(20, 250, 250, &mut wtr);
}
