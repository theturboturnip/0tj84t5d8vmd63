use std::error::Error;

use clap::Parser;
use csv::WriterBuilder;

use rand::{rngs::StdRng, seq::SliceRandom, thread_rng, SeedableRng};
use rust_caps_testgen::{cap_initial_len_pow2s, gen_edge, gen_in_category, variant_edge_cases, CapTestCaseSerDe, CapVariant, RandomCategory};


#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// The filename to output generated tests into
    #[arg(short)]
    out_file: Option<String>,

    #[arg(long)]
    seed: Option<u64>,

    /// Number of capabilities to generate for each category.
    #[arg(short, long, default_value_t=10)]
    n_caps_per_category: usize,

    #[arg(long, default_value="cap2024-11")]
    cap: CapVariant,

    #[arg(long, default_value_t=false)]
    gen_0cav: bool,
    #[arg(long, default_value_t=false)]
    gen_1cav: bool,
    #[arg(long, default_value_t=false)]
    gen_2cav: bool,
    #[arg(long, default_value_t=false)]
    gen_all_edge_cases: bool,
    #[arg(long, num_args=0..)]
    gen_edge: Vec<usize>,

    #[arg(long, default_value_t=false)]
    list_edge_cases: bool,

    #[arg(long, default_value_t=false)]
    shuffle: bool,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    if args.list_edge_cases {
        for (i, edge) in variant_edge_cases(args.cap).into_iter().enumerate() {
            println!("{}: {}", i, edge.to_str().unwrap());
        }
        return Ok(())
    }

    let valid_categories = {
        let mut cs = vec![];
        if args.gen_0cav {
            cs.push(RandomCategory::Valid0Cav);
        }
        if args.gen_1cav {
            cs.push(RandomCategory::Valid1Cav);
        }
        if args.gen_2cav {
            cs.push(RandomCategory::Valid2Cav);
        }
        cs
    };
    let edge_categories = {
        let mut es = vec![];
        if args.gen_all_edge_cases {
            for i in 0..variant_edge_cases(args.cap).len() {
                es.push(i);
            }
        } else {
            for e in args.gen_edge {
                es.push(e);
            }
        }
        es
    };

    if valid_categories.is_empty() && edge_categories.is_empty() {
        panic!("Please select at least one capability category to generate using cmdline options (see -h)");
    }

    let mut rng = match args.seed {
        Some(seed) => StdRng::seed_from_u64(seed),
        None => StdRng::from_rng(thread_rng()).unwrap(),
    };

    // Generate many valid capabilities for each of the entries
    let mut tests = vec![];

    let mut n_made: usize = 0;
    let n_expected = (cap_initial_len_pow2s(args.cap).len() * valid_categories.len() + edge_categories.len()) * args.n_caps_per_category;

    for initial_len_pow2 in cap_initial_len_pow2s(args.cap) {
        for category in &valid_categories {
            for _ in 0..args.n_caps_per_category {
                tests.push(gen_in_category(&mut rng, args.cap, *category, Some(initial_len_pow2)));
                n_made += 1;
                print!("{n_made}/{n_expected}\r");
            }
        }
    }
    for e in &edge_categories {
        for _ in 0..args.n_caps_per_category {
            tests.push(gen_edge(&mut rng, args.cap, *e));
            n_made += 1;
            print!("{n_made}/{n_expected}\r");
        }
    }
    
    let is_dumping_to_null = if cfg!(unix) {
        args.out_file.as_ref().map(|path| path.as_str() == "/dev/null") == Some(true)
    } else {
        false
    };
    if !is_dumping_to_null {
        let io_wtr: Box<dyn std::io::Write> = match args.out_file {
            Some(real_file) => Box::new(std::fs::File::create(real_file)?),
            None => Box::new(std::io::stdout()),
        };
        let mut wtr = WriterBuilder::new().delimiter(b':').from_writer(io_wtr);
        if args.shuffle {
            tests.shuffle(&mut rng);
        }
        for test in tests {
            let serde_test: CapTestCaseSerDe = test.into();
            wtr.serialize(serde_test)?;
        }
        wtr.flush()?;
    }

    Ok(())
}