use std::path::{Path, PathBuf};

use grep::regex::{RegexMatcher, RegexMatcherBuilder};
use grep::searcher::{Searcher, SearcherBuilder, Sink, SinkContext, SinkContextKind, SinkMatch};

use ignore::Walk;

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

#[derive(Debug, Clone)]
struct Line(u64, String);

#[derive(Debug)]
struct SearchMatch {
    line: Line,
    context_pre: Vec<Line>,
    context_post: Vec<Line>,
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum MatchState {
    Init,
    Before,
    Match,
    After,
}

struct SearchMatchCollector {
    state: MatchState,

    cur_context_pre: Vec<Line>,
    cur_context_post: Vec<Line>,
    cur_match_line: Option<Line>,
}

impl SearchMatchCollector {
    fn new() -> SearchMatchCollector {
        return SearchMatchCollector {
            state: MatchState::Init,
            cur_match_line: None,
            cur_context_pre: Vec::new(),
            cur_context_post: Vec::new(),
        };
    }

    fn maybe_emit(&mut self) {
        if let Some(line) = &self.cur_match_line {
            for ctx in &self.cur_context_pre {
                println!("-- {:?}", ctx);
            }
            println!("-- {:?}", line);
            for ctx in &self.cur_context_post {
                println!("-- {:?}", ctx);
            }
            println!("---");

            self.reset();
        }
    }

    fn reset(&mut self) {
        self.state = MatchState::Init;

        self.cur_match_line = None;
        self.cur_context_pre.clear();
        self.cur_context_post.clear();
    }

    #[inline]
    fn transition(&mut self, next: MatchState) {
        match (self.state, next) {
            // No transition, nothing to do
            (cur, next) if cur == next => {}

            // Beginning a new match or ending a previous one
            (_, MatchState::Before) | (MatchState::After, _) => {
                self.maybe_emit();
            }

            (_prev, next) => {
                self.state = next;
            }
        }
    }
}

impl Drop for SearchMatchCollector {
    fn drop(&mut self) {
        self.maybe_emit();
    }
}

impl Sink for SearchMatchCollector {
    type Error = std::io::Error;

    fn matched(
        &mut self,
        _searcher: &Searcher,
        mat: &SinkMatch<'_>,
    ) -> Result<bool, std::io::Error> {
        self.transition(MatchState::Match);

        // TODO: handle errors
        let line = Line(
            mat.line_number().expect("grab line number"),
            String::from_utf8_lossy(mat.bytes()).to_string(),
        );

        self.cur_match_line = Some(line);

        Ok(true)
    }

    fn context(
        &mut self,
        _searcher: &Searcher,
        ctx: &SinkContext<'_>,
    ) -> Result<bool, std::io::Error> {
        // TODO: handle errors.
        // TODO: can we avoid the additional .to_string() by storing a COW instead?
        let line = Line(
            ctx.line_number().expect("grab line number"),
            String::from_utf8_lossy(ctx.bytes()).to_string(),
        );

        match ctx.kind() {
            &SinkContextKind::Before => {
                self.transition(MatchState::Before);
                self.cur_context_pre.push(line);
            }
            &SinkContextKind::After => {
                self.transition(MatchState::After);
                self.cur_context_post.push(line);
            }
            &SinkContextKind::Other => {}
        }

        Ok(true)
    }
}

fn handle_path(path: &Path, searcher: &mut Searcher, matcher: &RegexMatcher) {
    let mut sink = SearchMatchCollector::new();

    searcher
        .search_path(&matcher, path, &mut sink)
        .expect("search failed");
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

    for dir_entry in Walk::new("../") {
        let dir_entry = dir_entry.unwrap();
        if !dir_entry.path().is_file() {
            continue;
        }

        handle_path(dir_entry.path(), &mut searcher, &matcher);
    }
}
