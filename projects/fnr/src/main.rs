use std::path::PathBuf;

use grep::regex::RegexMatcherBuilder;
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

struct SearchMatchCollector<'a> {
    proc: &'a mut SearchMatchProcessor,
    state: MatchState,

    cur_context_pre: Vec<Line>,
    cur_context_post: Vec<Line>,
    cur_match_line: Option<Line>,
}

impl<'a> SearchMatchCollector<'a> {
    fn new(proc: &'a mut SearchMatchProcessor) -> SearchMatchCollector<'a> {
        return SearchMatchCollector {
            proc: proc,
            state: MatchState::Init,
            cur_match_line: None,
            cur_context_pre: Vec::new(),
            cur_context_post: Vec::new(),
        };
    }

    fn maybe_emit(&mut self) {
        if let Some(line) = &self.cur_match_line {
            // TODO: clones are wasteful here.
            let m = SearchMatch {
                line: line.clone(),
                context_pre: self.cur_context_pre.clone(),
                context_post: self.cur_context_post.clone(),
            };

            self.proc.handle(m);
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

impl<'a> Drop for SearchMatchCollector<'a> {
    fn drop(&mut self) {
        self.maybe_emit();
    }
}

impl<'a> Sink for SearchMatchCollector<'a> {
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

struct SearchMatchProcessor {}
impl SearchMatchProcessor {
    fn handle(&self, m: SearchMatch) {
        println!("---");

        for line in m.context_pre {
            print!("{}: {}", line.0, line.1);
        }
        print!("{}: {}", m.line.0, m.line.1);
        for line in m.context_post {
            print!("{}: {}", line.0, line.1);
        }
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

    let mut processor = SearchMatchProcessor {};

    for dir_entry in Walk::new("../") {
        let dir_entry = dir_entry.unwrap();
        if !dir_entry.path().is_file() {
            continue;
        }

        let mut sink = SearchMatchCollector::new(&mut processor);

        searcher
            .search_path(&matcher, dir_entry.path(), &mut sink)
            .expect("search failed");
    }
}
