use std::path::PathBuf;

use structopt::clap::arg_enum;
use structopt::StructOpt;


arg_enum! {
    #[derive(Debug)]
    enum Grepper {
        Grep,
        GitGrep,
        Ag,
        RipGrep
    }
}

#[derive(Debug, StructOpt)]
#[structopt(name = "fnr")]
/// Look for things, optionally replace them.
struct Opts {
    #[structopt(short, long)]
    interactive: bool,

    /// Program to use for searching
    #[structopt(short, long, possible_values = &Grepper::variants(), case_insensitive = true, default_value = "grep")]
    grep: Grepper,

    #[structopt(short, long)]
    prompt: bool,

    /// What to search for.
    #[structopt(name = "FIND")]
    find: String,

    /// What to replace it with. Required unless `prompt` is set.
    #[structopt(name = "REPLACE")]
    replace: Option<String>,

    /// Locations to search. Current directory if not specified.
    #[structopt(name = "PATH", parse(from_os_str))]
    paths: Vec<PathBuf>,
}


#[derive(Debug)]
struct Search{}

fn main() {
    let opts: Opts = Opts::from_args();

    println!("Parsed out: {:?}", opts);
}
