use std::path::{Path, PathBuf};

use grep::matcher::{Captures, Matcher};
use grep::regex::{RegexMatcher, RegexMatcherBuilder};
use grep::searcher::{Searcher, SearcherBuilder, Sink, SinkContext, SinkContextKind, SinkMatch};
use ignore::Walk;
use lazy_static::lazy_static;
use regex::Regex;
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

    matches: Vec<SearchMatch>,
}

impl SearchMatchCollector {
    fn new() -> SearchMatchCollector {
        return SearchMatchCollector {
            state: MatchState::Init,
            cur_match_line: None,
            cur_context_pre: Vec::new(),
            cur_context_post: Vec::new(),
            matches: Vec::new(),
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

            self.matches.push(m);
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

    fn collect(&mut self) -> &Vec<SearchMatch> {
        self.maybe_emit();
        return &self.matches;
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

trait Replacer {
    fn replace(&self, input: &str) -> Option<String>;
}

struct RegexReplacer<'a> {
    matcher: &'a RegexMatcher,
    template: &'a str,
}

#[derive(Debug)]
enum TemplatePart<'a> {
    Literal(&'a str),
    Capture(u64),
}

type Template<'a> = Vec<TemplatePart<'a>>;

// TODO: Clean this up
fn parse_template<'a>(t: &'a str) -> Template<'a> {
    lazy_static! {
        static ref re: Regex = Regex::new(r"\$(\d+)").unwrap();
    }

    let capture_positions = re.captures_iter(t).map(|cap| {
        let num = cap[1].parse::<u64>().expect("invalid capture number");
        let mat = cap.get(1).unwrap();
        (mat.start() - 1, mat.end(), num)
    });
    let mut pos = 0;
    let mut template = Vec::new();

    for (start, end, value) in capture_positions {
        if start > pos {
            template.push(TemplatePart::Literal(&t[pos..start]));
        }
        template.push(TemplatePart::Capture(value));
        pos = end;
    }

    if pos < t.len() {
        template.push(TemplatePart::Literal(&t[pos..t.len()]));
    }

    template
}

impl<'a> Replacer for RegexReplacer<'a> {
    fn replace(&self, input: &str) -> Option<String> {
        let mut caps = self.matcher.new_captures().unwrap();
        let mut dst = vec![];

        self.matcher.replace_with_captures(input.as_bytes(), &mut caps, &mut dst, |caps, dst| {
            caps.interpolate(
                |name| self.matcher.capture_index(name),
                input.as_bytes(),
                self.template.as_bytes(),
                dst,
            );
            false
        })
            .unwrap();

        Some(
            String::from_utf8_lossy(&dst).to_string()
        )
    }
}


struct SearchMatchProcessor<'a> {
    replacer: &'a dyn Replacer
}

impl <'a> SearchMatchProcessor<'a> {
    fn handle_path(&self, path: &Path, matches: &Vec<SearchMatch>) {
        if matches.is_empty() { return }
        println!("--- {}: {} matches", path.display(), matches.len());

        for m in matches {
            self.handle(m);
        }

        println!("");
    }

    fn handle(&self, m: &SearchMatch) {
        for line in &m.context_pre {
            print!("    {}: {}", line.0, line.1);
        }

        // TODO: Multiple matches on same line
        print!("-   {}: {}", m.line.0, m.line.1);
        if let Some(replacement) = self.replacer.replace(&m.line.1) {
            print!("+   {}: {}", m.line.0, replacement);
        }

        // TODO: print gap between non-consecutive lines
        for line in &m.context_post {
            print!("    {}: {}", line.0, line.1);
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
        .before_context(1)
        .after_context(1)
        .build();

    // TODO: Confirm that template does not reference more capture groups than exist.
    let template_str = &opts.replace.unwrap();
    let replacer = RegexReplacer {
        matcher: &matcher,
        template: template_str,
    };

    let proc = SearchMatchProcessor {
        replacer: &replacer,
    };

    for dir_entry in Walk::new("../") {
        let dir_entry = dir_entry.unwrap();
        let path = dir_entry.path();
        if !path.is_file() {
            continue;
        }

        let mut sink = SearchMatchCollector::new();

        searcher
            .search_path(&matcher, dir_entry.path(), &mut sink)
            .expect("search failed");

        let matches = sink.collect();
        proc.handle_path(path, matches);
    }
}
