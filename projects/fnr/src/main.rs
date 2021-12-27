use std::fs::File;
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::path::{Path, PathBuf};

use anyhow::{ensure, Context, Result};
use atty::Stream;
use grep::matcher::{Captures, Matcher};
use grep::regex::{RegexMatcher, RegexMatcherBuilder};
use grep::searcher::{
    BinaryDetection, Searcher, SearcherBuilder, Sink, SinkContext, SinkContextKind, SinkMatch,
};
use ignore::WalkBuilder;
use regex;
use structopt::StructOpt;
use text_io::read;

#[derive(Debug, StructOpt)]
#[structopt(name = "fnr")]
/// Recursively find and replace. Like sed, but memorable.
// TODO: Potential features:
//
// /// Disable printing matches
// #[structopt(short, long)]
// quiet: bool,
//
// /// Search files with the given file extensions.
// #[structopt(short = "T", long, multiple = true, conflicts_with = "include")]
// file_type: Option<String>,
//
// /// Control whether terminal output is in color.
// #[structopt(long, allow_values = "always, auto, never", default_value = "auto")]
// color: String,
//
// /// Save changes as a .patch file rather than modifying in place.
// #[structopt(long)]
// write_patch: bool
struct Config {
    /// Match case insensitively.
    #[structopt(short = "i", long, conflicts_with = "case_sensitive, smart_case")]
    ignore_case: bool,

    /// Match case sensitively.
    #[structopt(short = "s", long, conflicts_with = "ignore_case, smart_case")]
    case_sensitive: bool,

    /// Match case sensitively if FIND has uppercase characters,
    /// insensitively otherwise. [default: true].
    #[structopt(
        short = "S",
        long,
        conflicts_with = "ignore_case, case_sensitive",
        takes_value = false
    )]
    smart_case: Option<bool>,

    /// Modify files in place.
    #[structopt(short = "W", long, conflicts_with = "prompt")]
    write: bool,

    /// Match FIND only at word boundary.
    #[structopt(short, long)]
    word: bool,

    /// Treat FIND as a string rather than a regular expression.
    #[structopt(long)]
    literal: bool,

    /// Search ALL files in given paths for matches.
    #[structopt(short, long)]
    all_files: bool,

    /// Find replacements in hidden files and directories.
    #[structopt(short = "H", long)]
    hidden: bool,

    /// Confirm each modification before making it. Implies --write.
    #[structopt(short, long, conflicts_with = "write")]
    prompt: bool,

    /// Print lines after matches.
    #[structopt(short = "A", long)]
    after: Option<usize>,

    /// Print lines before matches.
    #[structopt(short = "B", long)]
    before: Option<usize>,

    /// Print lines before and after matches.
    #[structopt(short = "C", long, conflicts_with = "after, before")]
    context: Option<usize>,

    /// Include only files or directories matching pattern.
    #[structopt(short = "I", long)]
    include: Option<Vec<String>>,

    /// Exclude files or directories matching pattern.
    #[structopt(short = "E", long)]
    exclude: Vec<String>,

    /// What to search for. Literal string or regular expression.
    ///
    /// For supported regular expression syntax, see:
    /// https://docs.rs/regex/latest/regex/#syntax
    #[structopt(name = "FIND", required = true)]
    find: String,

    /// What to replace it with.
    ///
    /// May contain numbered references to capture groups given in
    /// FIND in the form $1, $2, etc.
    #[structopt(name = "REPLACE", required = true)]
    replace: String,

    /// Locations to search. Current directory if not given.
    ///
    /// Paths may also be provided through standard input, e.g.
    ///
    /// $ fd .rs | fnr 'old_fn' 'new_fn'
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

    fn collect(&mut self) -> Vec<SearchMatch> {
        self.maybe_emit();

        let mut matches = vec![];
        std::mem::swap(&mut self.matches, &mut matches);

        return matches;
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

struct RegexReplacer {
    matcher: RegexMatcher,
    template: String,
}

impl RegexReplacer {
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

struct SearchProcessor {
    searcher: Searcher,
    matcher: RegexMatcher,
}

impl SearchProcessor {
    fn search_path<'a>(&mut self, path: &'a Path) -> Result<Vec<SearchMatch>> {
        let mut collector = SearchMatchCollector::new();

        self.searcher
            .search_path(&self.matcher, path, &mut collector)?;

        let matches = collector.collect();
        Ok(matches)
    }
}

struct MatchProcessor {
    replacer: RegexReplacer,
    replacement_decider: ReplacementDecider,

    total_matches: usize,
    total_replacements: usize,
}

impl MatchProcessor {
    fn new(replacer: RegexReplacer, replacement_decider: ReplacementDecider) -> MatchProcessor {
        return MatchProcessor {
            replacer,
            replacement_decider,

            total_matches: 0,
            total_replacements: 0,
        };
    }

    fn handle_path(&mut self, path: &Path, matches: &Vec<SearchMatch>) -> Result<()> {
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

    fn apply_changes(&self, path: &Path, mut changes: &[Change]) -> Result<()> {
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
            panic!("[bug] invalid state: no decision, but should not prompt");
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

struct FindAndReplacer {
    file_walker: WalkBuilder,
    included_paths: Option<regex::RegexSet>,
    excluded_paths: regex::RegexSet,
    match_processor: MatchProcessor,
    search_processor: SearchProcessor,
}

const DEFAULT_BEFORE_CONTEXT_LINES: usize = 2;
const DEFAULT_AFTER_CONTEXT_LINES: usize = 2;

impl FindAndReplacer {
    fn from_config(config: Config) -> Result<FindAndReplacer> {
        let pattern = if config.literal {
            regex::escape(&config.find)
        } else {
            config.find
        };

        let mut matcher_builder = RegexMatcherBuilder::new();
        matcher_builder
            .case_insensitive(!config.case_sensitive && config.ignore_case)
            .case_smart(!config.case_sensitive && config.smart_case.unwrap_or(true));

        let searcher = SearcherBuilder::new()
            .binary_detection(BinaryDetection::quit(0x00))
            .line_number(true)
            .before_context(
                config
                    .before
                    .or(config.context)
                    .unwrap_or(DEFAULT_BEFORE_CONTEXT_LINES),
            )
            .after_context(
                config
                    .after
                    .or(config.context)
                    .unwrap_or(DEFAULT_AFTER_CONTEXT_LINES),
            )
            .build();

        let pattern_matcher = matcher_builder
            .build(&pattern)
            .with_context(|| format!("Failed to parse pattern '{}'", pattern))?;

        // TODO: Confirm that template does not reference more capture groups than exist.
        let replacer = RegexReplacer {
            matcher: pattern_matcher.clone(),
            template: config.replace,
        };

        let replacement_decider = if config.prompt {
            ReplacementDecider::with_prompt()
        } else if config.write {
            ReplacementDecider::constantly(ReplacementDecision::Accept)
        } else {
            ReplacementDecider::constantly(ReplacementDecision::Ignore)
        };

        let match_processor = MatchProcessor::new(replacer, replacement_decider);
        let search_processor = SearchProcessor {
            matcher: pattern_matcher,
            searcher: searcher,
        };

        let paths = if config.paths.is_empty() {
            // Read paths from standard in if none are specified and
            // there's input piped to the process.
            if !atty::is(Stream::Stdin) {
                ensure!(
                    config.prompt == false,
                    "cannot use --prompt when reading files from stdin"
                );
                let mut paths = vec![];
                for line in std::io::stdin().lock().lines() {
                    paths.push(Path::new(&line.unwrap()).to_owned());
                }
                paths
            } else {
                vec![Path::new(".").to_owned()]
            }
        } else {
            config.paths
        };

        let mut file_walker = WalkBuilder::new(paths[0].clone());
        for path in &paths[1..] {
            file_walker.add(path);
        }

        let should_ignore = !config.all_files;
        let should_show_hidden = config.hidden || config.all_files;
        file_walker
            .hidden(!should_show_hidden)
            .ignore(should_ignore)
            .git_ignore(should_ignore)
            .git_exclude(should_ignore)
            .parents(should_ignore);

        let included_paths = config.include.map(|included_paths| {
            let escaped = included_paths.iter().map(|p| regex::escape(p));
            regex::RegexSet::new(escaped).unwrap()
        });

        let excluded_paths = {
            let escaped = config.exclude.iter().map(|p| regex::escape(p));
            regex::RegexSet::new(escaped)?
        };

        Ok(FindAndReplacer {
            file_walker: file_walker,
            included_paths: included_paths,
            excluded_paths: excluded_paths,
            search_processor: search_processor,
            match_processor: match_processor,
        })
    }

    fn run(&mut self) -> Result<()> {
        // TODO: There exists a parallel file walker
        let file_walker = self.file_walker.build();
        for dir_entry in file_walker {
            let dir_entry = dir_entry?;
            let path = dir_entry.path();
            if !path.is_file() {
                continue;
            }

            let path_str = path.to_str().with_context(|| {
                format!("Failed to interpret path name as UTF-8 string: {:?}", path)
            })?;

            if let Some(ref included_paths) = self.included_paths {
                if !included_paths.is_match(path_str) {
                    continue;
                }
            }
            if self.excluded_paths.is_match(path_str) {
                continue;
            }

            let matches = self.search_processor.search_path(path)?;
            self.match_processor.handle_path(path, &matches)?;
        }

        // if !self.config.write {
        //     println!("Use -w, --write to modify files in place.");
        // }

        self.match_processor.finalize();
        Ok(())
    }
}

// Main entry point
fn run_find_and_replace() -> Result<()> {
    let config = Config::from_args();
    let mut find_and_replacer = FindAndReplacer::from_config(config)?;

    return find_and_replacer.run();
}

fn main() {
    let exit_code = match run_find_and_replace() {
        Err(e) => {
            eprintln!("{:?}", e);
            1
        }
        Ok(()) => 0,
    };

    std::process::exit(exit_code);
}
