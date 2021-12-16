use std::fs::File;
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::path::{Path, PathBuf};

use atty::Stream;
use grep::matcher::{Captures, Matcher};
use grep::regex::{RegexMatcher, RegexMatcherBuilder};
use grep::searcher::{
    BinaryDetection, Searcher, SearcherBuilder, Sink, SinkContext, SinkContextKind, SinkMatch,
};
use ignore::WalkBuilder;
use structopt::StructOpt;
use text_io::read;

#[derive(Debug, StructOpt)]
#[structopt(name = "fnr")]
/// Look for things, optionally replace them.
struct Opts {
    /// Run case insensitive search
    #[structopt(short = "i", long)]
    ignore_case: bool,

    /// Modify files in place.
    #[structopt(short, long)]
    write: bool,

    /// Confirm each modification before making it.
    #[structopt(short, long)]
    prompt: bool,

    /// Print lines after matches.
    #[structopt(short = "A", long)]
    after: Option<usize>,

    /// Print lines before matches.
    #[structopt(short = "B", long)]
    before: Option<usize>,

    /// Print lines before and after matches.
    #[structopt(short = "C", long)]
    context: Option<usize>,

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

    fn display_change(&self, replacement_line: &str) {
        for line in &self.context_pre {
            print!("    {}: {}", line.0, line.1);
        }

        // TODO: Multiple matches on same line
        // TODO: Disable colors when not atty
        // TODO: Align line number with padding.
        print!("\x1B[31m-   {}: {}\x1B[0m", self.line.0, self.line.1);
        print!("\x1B[32m+   {}: {}\x1B[0m", self.line.0, replacement_line);

        for line in &self.context_post {
            print!("    {}: {}", line.0, line.1);
        }
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
                true
            })
            .unwrap();

        Some(String::from_utf8_lossy(&dst).to_string())
    }
}

#[derive(Debug)]
struct Change {
    line_number: u64,
    // TODO: use slice here
    new_line: String,
}

struct SearchMatchProcessor<'a> {
    replacer: &'a dyn Replacer,
    replacement_decider: ReplacementDecider,

    total_matches: usize,
    total_replacements: usize,
}

impl<'a> SearchMatchProcessor<'a> {
    fn new(
        replacer: &'a dyn Replacer,
        replacement_decider: ReplacementDecider,
    ) -> SearchMatchProcessor<'a> {
        return SearchMatchProcessor {
            replacer,
            replacement_decider,

            total_matches: 0,
            total_replacements: 0,
        };
    }

    fn handle_path(
        &mut self,
        path: &Path,
        matches: &Vec<SearchMatch>,
    ) -> Result<(), std::io::Error> {
        if matches.is_empty() {
            return Ok(());
        }
        println!(
            "--- {}: {} matching line{}",
            path.display(),
            matches.len(),
            if matches.len() == 1 { "" } else { "s" }
        );
        self.total_matches += matches.len();
        self.replacement_decider.reset_local_decision();

        let mut change_list = vec![];
        for m in matches {
            let replacement = if let Some(r) = self.replacer.replace(&m.line.1) {
                r
            } else {
                println!("TODO: replacer failed on line");
                continue;
            };

            let decision = self.replacement_decider.decide(m, &replacement);
            let change = match decision {
                ReplacementDecision::Accept => m.as_change(replacement),
                ReplacementDecision::Ignore => continue,
                ReplacementDecision::Edit => {
                    let mut line = read_input("Replace with [^D to skip] ")?;
                    if line == "" {
                        println!("... skipped ...");
                        continue;
                    }

                    line.push('\n');
                    m.display_change(&line);
                    println!("--");
                    m.as_change(line)
                }
            };

            change_list.push(change);
            self.total_replacements += 1;
        }

        if !change_list.is_empty() {
            self.apply_changes(path, &mut change_list)?;
        }

        Ok(())
    }

    fn apply_changes(&self, path: &Path, mut changes: &[Change]) -> Result<(), std::io::Error> {
        /*println!(
            "{}: {} change{}",
            path.display(),
            changes.len(),
            if changes.len() != 1 { "s" } else { "" }
        );*/
        let dst_path = path.with_extension("~");
        let src = File::open(path)?;
        let dst = File::create(&dst_path)?;

        let mut reader = BufReader::new(src);
        let mut writer = BufWriter::new(dst);

        let mut line_num = 0;
        loop {
            let mut line = String::new();
            let bytes_read = reader.read_line(&mut line)?;
            // EOF reached
            if bytes_read == 0 {
                break;
            }

            line_num += 1;

            if !changes.is_empty() && changes[0].line_number == line_num {
                writer.write(changes[0].new_line.as_bytes())?;
                changes = &changes[1..];
            } else {
                writer.write(line.as_bytes())?;
            }
        }

        std::fs::rename(dst_path, path)?;
        return Ok(());
    }

    fn finalize(&self) {
        println!(
            "All done. Replaced {} of {} matches",
            self.total_replacements, self.total_matches
        );
    }
}

#[derive(Debug, Copy, Clone)]
enum ReplacementDecision {
    Accept,
    Ignore,
    Edit,
}

fn read_input(prompt: &str) -> Result<String, std::io::Error> {
    print!("{}", prompt);
    std::io::stdout().flush()?;

    Ok(read!("{}\n"))
}

struct ReplacementDecider {
    should_prompt: bool,
    global_decision: Option<ReplacementDecision>,
    local_decision: Option<ReplacementDecision>,
}

impl ReplacementDecider {
    fn with_prompt() -> ReplacementDecider {
        return ReplacementDecider {
            should_prompt: true,
            global_decision: None,
            local_decision: None,
        };
    }

    fn constantly(decision: ReplacementDecision) -> ReplacementDecider {
        return ReplacementDecider {
            should_prompt: false,
            global_decision: Some(decision),
            local_decision: None,
        };
    }

    fn reset_local_decision(&mut self) {
        self.local_decision = None;
    }

    fn decide(&mut self, match_: &SearchMatch, replacement: &str) -> ReplacementDecision {
        // TODO: Should we display this always?
        match_.display_change(replacement);

        if let Some(decision) = self.global_decision {
            return decision;
        } else if let Some(decision) = self.local_decision {
            return decision;
        }

        if !self.should_prompt {
            panic!("invalid state: no decision, but should not prompt");
        }

        return self.prompt_for_decision();
    }

    fn prompt_for_decision(&mut self) -> ReplacementDecision {
        loop {
            let line = read_input("Stage this replacement [y,n,q,a,e,d,?] ").unwrap();

            return match line.as_str() {
                "y" => ReplacementDecision::Accept,
                "n" => ReplacementDecision::Ignore,
                "q" => {
                    self.global_decision = Some(ReplacementDecision::Ignore);
                    ReplacementDecision::Ignore
                }
                "a" => {
                    self.local_decision = Some(ReplacementDecision::Accept);
                    ReplacementDecision::Accept
                }
                "d" => {
                    self.local_decision = Some(ReplacementDecision::Ignore);
                    ReplacementDecision::Ignore
                }
                "e" => ReplacementDecision::Edit,

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
}

fn main() {
    let opts: Opts = Opts::from_args();
    println!("Parsed opts: {:?}", opts);

    let matcher = RegexMatcherBuilder::new()
        .case_insensitive(opts.ignore_case)
        .build(&opts.find)
        .expect("bad pattern");

    let mut searcher = SearcherBuilder::new()
        .binary_detection(BinaryDetection::quit(0x00))
        .line_number(true)
        .before_context(opts.before.or(opts.context).unwrap_or(2))
        .after_context(opts.after.or(opts.context).unwrap_or(2))
        .build();

    // TODO: Confirm that template does not reference more capture groups than exist.
    let replacer = RegexReplacer {
        matcher: &matcher,
        template: &opts.replace,
    };

    if opts.write && opts.prompt {
        eprintln!("both --write and --prompt given, ignore --write.");
    }

    let replacement_decider = if opts.prompt {
        ReplacementDecider::with_prompt()
    } else if opts.write {
        ReplacementDecider::constantly(ReplacementDecision::Accept)
    } else {
        ReplacementDecider::constantly(ReplacementDecision::Ignore)
    };

    let mut proc = SearchMatchProcessor::new(&replacer, replacement_decider);

    let paths = if opts.paths.is_empty() {
        if !atty::is(Stream::Stdin) {
            if opts.prompt {
                panic!("Cannot use --prompt when reading files from stdin!");
            }
            let mut paths = vec![];
            for line in std::io::stdin().lock().lines() {
                paths.push(Path::new(&line.unwrap()).to_owned());
            }
            paths
        } else {
            vec![Path::new(".").to_owned()]
        }
    } else {
        opts.paths
    };

    // TODO: Add path exclusions
    let mut file_walker = WalkBuilder::new(paths[0].clone());
    for path in &paths[1..] {
        file_walker.add(path);
    }

    // TODO: These settings can be given by command line args.
    file_walker
        .ignore(true)
        .git_ignore(true)
        .git_exclude(true)
        .parents(true);

    // TODO: There exists a parallel file walker
    for dir_entry in file_walker.build() {
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
        proc.handle_path(path, matches).expect("handle path");
    }

    if !opts.write {
        println!("Use -w, --write to modify files in place.");
    }

    proc.finalize();
}
