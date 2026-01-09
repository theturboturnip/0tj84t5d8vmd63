use clap::Parser;
use rust_caps::capability::v2024_02::CapBitfield;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    hex: String
}

fn main() {
    let args = Args::parse();
    let text = u128::from_str_radix(&args.hex, 16).unwrap();
    println!("{:?}", CapBitfield::new(text));
}