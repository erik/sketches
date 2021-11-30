use std::collections::HashMap;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};

use grep::matcher::Matcher;
use grep::regex::RegexMatcherBuilder;
use grep::searcher::sinks::UTF8;
use grep::searcher::{Searcher, SearcherBuilder, Sink, SinkContext, SinkContextKind, SinkMatch};

use ignore::Walk;

use structopt::clap::arg_enum;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(name = "fnr")]
/// Look for things, optionally replace them.
struct Opts {
    /// Run search in case insensitive mode
    #[structopt(short, long)]
    insensitive: bool,

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
struct SearchMatch {
    line_number: u64,
    line: String,

    pre_context: Vec<String>,
    post_context: Vec<String>,
}

impl SearchMatch {
    fn empty() -> SearchMatch {
        return SearchMatch {
            line_number: 0,
            line: "".to_string(),
            pre_context: Vec::new(),
            post_context: Vec::new(),
        };
    }
}

struct SearchResults {
    // TODO: Group by file
    matches: Vec<SearchMatch>,
}

impl SearchResults {
    fn new() -> SearchResults {
        return SearchResults {
            matches: Vec::new(),
        };
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum MatchState {
    Before,
    Match,
    After,
}

struct SearchMatchSink {
    state: MatchState,
    current_match: SearchMatch,
}

impl SearchMatchSink {
    fn new() -> SearchMatchSink {
        return SearchMatchSink {
            state: MatchState::Before,
            current_match: SearchMatch::empty(),
        };
    }

    fn reset(&mut self) {
        self.state = MatchState::Before;
        self.current_match = SearchMatch::empty();
    }

    #[inline]
    fn transition(&mut self, next: MatchState) {
        match (self.state, next) {
            (x, y) if x == y => {
                // No transition, nothing to do
            }

            (MatchState::After, _any) => {
                // Finished, emit record
                println!("-- {:?}", self.current_match);
                self.reset();
            }

            (_prev, next) => {
                self.state = next;
            }
        }
    }
}

impl Sink for SearchMatchSink {
    type Error = std::io::Error;

    fn matched(
        &mut self,
        _searcher: &Searcher,
        mat: &SinkMatch<'_>,
    ) -> Result<bool, std::io::Error> {
        self.transition(MatchState::Match);

        let line = std::str::from_utf8(mat.bytes()).unwrap();
        let line_number = mat.line_number().unwrap();

        self.current_match.line = line.to_string();
        self.current_match.line_number = line_number;
        Ok(true)
    }

    fn context(
        &mut self,
        _searcher: &Searcher,
        ctx: &SinkContext<'_>,
    ) -> Result<bool, std::io::Error> {
        let line = std::str::from_utf8(ctx.bytes()).unwrap();

        // TODO: Use line number
        let line_number = ctx.line_number().unwrap();

        match ctx.kind() {
            &SinkContextKind::Before => {
                self.current_match.pre_context.push(line.to_string());
                self.transition(MatchState::Before);
            }
            &SinkContextKind::After => {
                self.current_match.post_context.push(line.to_string());
                self.transition(MatchState::After);
            }
            &SinkContextKind::Other => {}
        }

        Ok(true)
    }
}

fn main() {
    let opts: Opts = Opts::from_args();
    println!("Parsed opts: {:?}", opts);

    let matcher = RegexMatcherBuilder::new()
        .case_insensitive(opts.insensitive)
        .build(&opts.find)
        .expect("bad pattern");
    let mut searcher = SearcherBuilder::new()
        .line_number(true)
        .before_context(3)
        .after_context(3)
        .build();

    let mut sink = SearchMatchSink::new();

    for path in Walk::new("../") {
        let path = path.unwrap();
        if !path.path().is_file() {
            continue;
        }

        searcher
            .search_path(&matcher, path.path(), &mut sink)
            .expect("search failed");
    }
}
