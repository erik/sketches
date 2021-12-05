use std::cmp::max;
use std::fs::File;
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::path::{Path, PathBuf};

use grep::matcher::{Captures, Matcher};
use grep::regex::{RegexMatcher, RegexMatcherBuilder};
use grep::searcher::{
    BinaryDetection, Searcher, SearcherBuilder, Sink, SinkContext, SinkContextKind, SinkMatch,
};
use ignore::Walk;
use structopt::StructOpt;
use text_io::read;

#[derive(Debug, StructOpt)]
#[structopt(name = "fnr")]
/// Look for things, optionally replace them.
struct Opts {
    /// Run search in case insensitive mode
    #[structopt(short, long)]
    insensitive: bool,

    /// Print out replacements without actually performing them
    #[structopt(short, long)]
    dry_run: bool,

    #[structopt(short = "A", long, default_value = "0")]
    after: usize,
    #[structopt(short = "B", long, default_value = "0")]
    before: usize,
    #[structopt(short = "C", long, default_value = "0")]
    context: usize,

    #[structopt(short, long)]
    prompt: bool,

    /// What to search for.
    #[structopt(name = "FIND")]
    find: String,

    /// What to replace it with.
    #[structopt(name = "REPLACE")]
    replace: String,

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

impl SearchMatch {
    fn as_change(&self, line: String) -> Change {
        return Change {
            line_number: self.line.0,
            new_line: line,
        };
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum MatchState {
    Init,
    Before,
    Match,
    After,
}

#[derive(Debug)]
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
            // Beginning a new match or ending a previous one
            (MatchState::Match, MatchState::Before)       // Have before context, no after context
            | (MatchState::Match, MatchState::Match)      // No before context, no after context
            | (MatchState::After, MatchState::Before)     // Have before context, have after context
            | (MatchState::After, MatchState::Match) => { // Have after context, no before context
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

impl<'a> Replacer for RegexReplacer<'a> {
    fn replace(&self, input: &str) -> Option<String> {
        let mut caps = self.matcher.new_captures().unwrap();
        let mut dst = vec![];

        self.matcher
            .replace_with_captures(input.as_bytes(), &mut caps, &mut dst, |caps, dst| {
                caps.interpolate(
                    |name| self.matcher.capture_index(name),
                    input.as_bytes(),
                    self.template.as_bytes(),
                    dst,
                );
                false
            })
            .unwrap();

        Some(String::from_utf8_lossy(&dst).to_string())
    }
}

type MatchDecider = dyn Fn(&SearchMatch) -> ReplacementDecision;

#[derive(Debug)]
struct Change {
    line_number: u64,
    // TODO: use slice here
    new_line: String,
}

struct SearchMatchProcessor<'a> {
    replacer: &'a dyn Replacer,
    acceptor: &'a MatchDecider,

    decide_all: Option<bool>,
}

impl<'a> SearchMatchProcessor<'a> {
    fn new(replacer: &'a dyn Replacer, acceptor: &'a MatchDecider) -> SearchMatchProcessor<'a> {
        return SearchMatchProcessor {
            replacer,
            acceptor,
            decide_all: None,
        };
    }

    fn handle_path(&mut self, path: &Path, matches: &Vec<SearchMatch>) {
        if matches.is_empty() || Some(false) == self.decide_all {
            return;
        }
        println!(
            "--- {}: {} match{}",
            path.display(),
            matches.len(),
            if matches.len() == 1 { "" } else { "es" }
        );
        let mut accept_remaining = false;

        let mut change_list = vec![];
        for m in matches {
            let replacement = if let Some(r) = self.replacer.replace(&m.line.1) {
                r
            } else {
                println!("TODO: replacer failed on line");
                continue;
            };
            self.display(m, &replacement);

            let change = if accept_remaining || Some(true) == self.decide_all {
                m.as_change(replacement)
            } else {
                match (self.acceptor)(m) {
                    ReplacementDecision::IgnoreThis => continue,
                    ReplacementDecision::IgnoreFile => break,
                    ReplacementDecision::IgnoreRest => {
                        self.decide_all = Some(false);
                        break;
                    }
                    ReplacementDecision::AcceptThis => m.as_change(replacement),
                    ReplacementDecision::AcceptFile => {
                        accept_remaining = true;
                        m.as_change(replacement)
                    }
                    ReplacementDecision::EditThis => {
                        print!("Replace with [^D to skip] ");
                        std::io::stdout().flush().unwrap();
                        let mut line: String = read!("{}\n");
                        if line == "" {
                            println!("... skipped ...");
                            continue;
                        }

                        line.push('\n');
                        self.display(m, &line);
                        println!("--");
                        m.as_change(line)
                    }
                }
            };

            change_list.push(change);
        }

        // For --prompt, confirm before applying here
        if !change_list.is_empty() {
            self.apply_changes(path, &mut change_list);
        }

        println!("");
    }

    fn display(&self, m: &SearchMatch, replacement: &str) {
        // TODO: only show this gap if non-consecutive
        println!("    ...");
        for line in &m.context_pre {
            print!("    {}: {}", line.0, line.1);
        }

        // TODO: Multiple matches on same line
        // TODO: Disable colors when not atty
        // TODO: Align line number with padding.
        print!("\x1B[31m-   {}: {}\x1B[0m", m.line.0, m.line.1);
        print!("\x1B[32m+   {}: {}\x1B[0m", m.line.0, replacement);

        for line in &m.context_post {
            print!("    {}: {}", line.0, line.1);
        }
    }

    fn apply_changes(&self, path: &Path, mut changes: &[Change]) {
        let dst_path = path.with_extension("~");
        let src = File::open(path).expect("couldn't open file");
        let dst = File::create(&dst_path).expect("couldn't create file");

        let mut reader = BufReader::new(src);
        let mut writer = BufWriter::new(dst);

        let mut line_num = 0;
        loop {
            let mut line = String::new();
            let bytes_read = reader.read_line(&mut line).expect("read line");
            // EOF reached
            if bytes_read == 0 {
                break;
            }

            line_num += 1;

            if !changes.is_empty() && changes[0].line_number == line_num {
                writer.write(changes[0].new_line.as_bytes()).unwrap();
                changes = &changes[1..];
            } else {
                writer.write(line.as_bytes()).unwrap();
            }
        }

        std::fs::rename(dst_path, path).expect("rename file");
    }
}

#[derive(Debug)]
enum ReplacementDecision {
    IgnoreThis,
    IgnoreFile,
    IgnoreRest,
    AcceptThis,
    AcceptFile,
    EditThis,
}

fn prompt_for_decision() -> ReplacementDecision {
    loop {
        print!("Stage this replacement [Y,n,q,a,e,d,?] ");
        std::io::stdout().flush().unwrap();
        let line: String = read!("{}\n");
        // TODO: ^D should not result in acceptance

        return match line.as_str() {
            "y" | "Y" => ReplacementDecision::AcceptThis,
            "n" => ReplacementDecision::IgnoreThis,
            "q" => ReplacementDecision::IgnoreRest,
            "a" => ReplacementDecision::AcceptFile,
            "d" => ReplacementDecision::IgnoreFile,
            "e" => ReplacementDecision::EditThis,

            "?" | _ => {
                println!(
                    "\x1B[31m
Y - replace this line
n - do not replace this line
q - quit; do not replace this line or any remaining ones
a - replace this line and all remaining ones in this file
d - do not replace this line nor any remaining ones in this file
e - edit this replacement
? - show help
\x1B[0m"
                );
                continue;
            }
        };
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
        .binary_detection(BinaryDetection::quit(0x00))
        .line_number(true)
        .before_context(max(opts.context, opts.before))
        .after_context(max(opts.context, opts.after))
        .build();

    // TODO: Confirm that template does not reference more capture groups than exist.
    let replacer = RegexReplacer {
        matcher: &matcher,
        template: &opts.replace,
    };

    if opts.dry_run && opts.prompt {
        println!("WARN: --prompt does not make sense with --dry-run, skipping");
    }

    let acceptor = if opts.dry_run {
        |_: &SearchMatch| ReplacementDecision::IgnoreThis
    } else if opts.prompt {
        |_: &SearchMatch| prompt_for_decision()
    } else {
        |_: &SearchMatch| ReplacementDecision::AcceptThis
    };
    let mut proc = SearchMatchProcessor::new(&replacer, &acceptor);

    let paths = if opts.paths.is_empty() {
        vec![Path::new("./").to_owned()]
    } else {
        opts.paths
    };

    for path in paths {
        // TODO: Support files as well as directories here
        for dir_entry in Walk::new(path) {
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

    if opts.dry_run {
        println!("--dry-run enabled, no changes applied.");
    }

    println!("All done.");
}
