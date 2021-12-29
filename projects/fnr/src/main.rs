use std::borrow::Cow;
use std::fs::File;
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::path::{Path, PathBuf};
use std::sync::mpsc::channel;
use std::sync::Arc;

use anyhow::{ensure, Context, Result};
use atty::Stream;
use crossbeam::thread;
use grep::matcher::{Captures, Matcher};
use grep::regex::{RegexMatcher, RegexMatcherBuilder};
use grep::searcher::{
    BinaryDetection, Searcher, SearcherBuilder, Sink, SinkContext, SinkContextKind, SinkMatch,
};
use ignore::{WalkBuilder, WalkState};
use regex::RegexSet;
use structopt::StructOpt;
use text_io::read;

#[derive(Debug, StructOpt)]
#[structopt(name = "fnr")]
/// Recursively find and replace. Like sed, but memorable.
// TODO: Potential features:
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

    /// Disable printing matches
    #[structopt(short, long, conflicts_with = "prompt")]
    quiet: bool,

    /// Display compacted output format
    #[structopt(short, long, conflicts_with = "prompt, quiet")]
    compact: bool,

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
    #[structopt(short, long, conflicts_with = "hidden")]
    all_files: bool,

    /// Find replacements in hidden files and directories.
    #[structopt(short = "H", long, conflicts_with = "all_files")]
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

enum MatchPrintMode {
    Silent,
    Compact,
    Full,
}

struct MatchFormatter {
    print_mode: MatchPrintMode,
    writes_enabled: bool,
    last_line_num: Option<u64>,
}

impl MatchFormatter {
    fn from_config(cfg: &Config) -> MatchFormatter {
        MatchFormatter {
            print_mode: if cfg.quiet {
                MatchPrintMode::Silent
            } else if cfg.compact {
                MatchPrintMode::Compact
            } else {
                MatchPrintMode::Full
            },
            writes_enabled: cfg.write || cfg.prompt,
            last_line_num: None,
        }
    }

    fn display_header(&mut self, path: &Path, num_matches: usize) {
        match self.print_mode {
            MatchPrintMode::Silent => {}
            MatchPrintMode::Compact => {}
            MatchPrintMode::Full => self.display_header_full(path, num_matches),
        }
    }

    #[inline]
    fn display_header_full(&mut self, path: &Path, num_matches: usize) {
        println!(
            "\x1B[4m{}\x1B[0m {} match{}",
            path.display(),
            num_matches,
            if num_matches == 1 { "" } else { "es" }
        );

        self.last_line_num = None;
    }

    fn display_match(&mut self, path: &Path, search_match: &SearchMatch, replacement: &str) {
        match self.print_mode {
            MatchPrintMode::Silent => {}
            MatchPrintMode::Compact => self.display_match_compact(path, search_match, replacement),
            MatchPrintMode::Full => self.display_match_full(search_match, replacement),
        }
    }

    #[inline]
    fn display_match_compact(
        &mut self,
        path: &Path,
        search_match: &SearchMatch,
        replacement: &str,
    ) {
        let path = path.display();

        for line in &search_match.context_pre {
            print!("{}:{}:{}", path, line.0, line.1);
        }

        print!(
            "\x1B[31m{}:{}-{}\x1B[0m",
            path, search_match.line.0, search_match.line.1
        );
        print!(
            "\x1B[32m{}:{}+{}\x1B[0m",
            path, search_match.line.0, replacement
        );

        for line in &search_match.context_post {
            print!("{}:{}:{}", path, line.0, line.1);
        }
    }

    #[inline]
    fn display_match_full(&mut self, m: &SearchMatch, replacement: &str) {
        let has_line_break = self
            .last_line_num
            .map(|last_line_num| {
                if !m.context_pre.is_empty() {
                    m.context_pre[0].0 > last_line_num + 1
                } else {
                    m.line.0 > last_line_num + 1
                }
            })
            .unwrap_or(false);

        if has_line_break {
            println!("  ---");
        }

        for line in &m.context_pre {
            print!(" {:4} {}", line.0, line.1);
        }

        // TODO: Highlight matching part of line
        // TODO: Disable colors when not atty
        print!("\x1B[31m-{:4} {}\x1B[0m", m.line.0, m.line.1);
        print!("\x1B[32m+{:4} {}\x1B[0m", m.line.0, replacement);

        for line in &m.context_post {
            print!(" {:4} {}", line.0, line.1);
            self.last_line_num = Some(line.0);
        }
    }

    fn display_footer(&self, total_replacements: usize, total_matches: usize) {
        match self.print_mode {
            MatchPrintMode::Silent => {}
            MatchPrintMode::Compact => {}
            MatchPrintMode::Full => self.display_footer_full(total_replacements, total_matches),
        }
    }

    #[inline]
    fn display_footer_full(&self, total_replacements: usize, total_matches: usize) {
        println!(
            "All done. Replaced {} of {} matches",
            total_replacements, total_matches
        );

        if !self.writes_enabled {
            println!("Use -w, --write to modify files in place.");
        }
    }
}

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
        SearchMatchCollector {
            state: MatchState::Init,
            cur_match_line: None,
            cur_context_pre: Vec::new(),
            cur_context_post: Vec::new(),
            matches: Vec::new(),
        }
    }

    fn maybe_emit(&mut self) {
        let mut cur_match_line = None;
        std::mem::swap(&mut cur_match_line, &mut self.cur_match_line);

        if let Some(line) = cur_match_line {
            let mut context_pre = vec![];
            let mut context_post = vec![];
            std::mem::swap(&mut context_pre, &mut self.cur_context_pre);
            std::mem::swap(&mut context_post, &mut self.cur_context_post);

            let search_match = SearchMatch {
                line,
                context_pre,
                context_post,
            };

            self.matches.push(search_match);
            self.state = MatchState::Init;
        }
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

        matches
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
            mat.line_number().unwrap(),
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

        match *ctx.kind() {
            SinkContextKind::Before => {
                self.transition(MatchState::Before);
                self.cur_context_pre.push(line);
            }
            SinkContextKind::After => {
                self.transition(MatchState::After);
                self.cur_context_post.push(line);
            }
            SinkContextKind::Other => {}
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

struct MatchReplacement<'a> {
    search_match: SearchMatch,
    replacement: Cow<'a, str>,
}

#[derive(Clone)]
struct SearchProcessor {
    searcher: Searcher,
    matcher: Arc<RegexMatcher>,
}

impl SearchProcessor {
    fn search_path(&mut self, path: &'_ Path) -> Result<Vec<SearchMatch>> {
        let mut collector = SearchMatchCollector::new();

        self.searcher
            .search_path(self.matcher.as_ref(), path, &mut collector)?;

        let matches = collector.collect();
        Ok(matches)
    }
}

struct MatchProcessor {
    replacer: RegexReplacer,
    replacement_decider: ReplacementDecider,
    match_formatter: MatchFormatter,

    total_matches: usize,
    total_replacements: usize,
}

impl MatchProcessor {
    fn new(
        replacer: RegexReplacer,
        replacement_decider: ReplacementDecider,
        match_formatter: MatchFormatter,
    ) -> MatchProcessor {
        MatchProcessor {
            replacer,
            replacement_decider,
            match_formatter,

            total_matches: 0,
            total_replacements: 0,
        }
    }

    fn consume_matches(&mut self, path: &Path, matches: &mut Vec<SearchMatch>) -> Result<()> {
        if matches.is_empty() {
            return Ok(());
        }

        self.match_formatter.display_header(path, matches.len());
        self.total_matches += matches.len();
        self.replacement_decider.reset_local_decision();

        let mut replacement_list = vec![];
        for m in matches.drain(0..) {
            let replacement = if let Some(r) = self.replacer.replace(&m.line.1) {
                r
            } else {
                println!("TODO: replacer failed on line");
                continue;
            };

            self.match_formatter.display_match(path, &m, &replacement);
            let match_replacement = match self.replacement_decider.decide() {
                ReplacementDecision::Accept => MatchReplacement {
                    search_match: m,
                    replacement: replacement.into(),
                },
                ReplacementDecision::Ignore => continue,
                ReplacementDecision::Edit => {
                    let mut line = read_input("Replace with [^D to skip] ")?;
                    if line.is_empty() {
                        println!("... skipped ...");
                        continue;
                    }

                    line.push('\n');
                    self.match_formatter.display_match(path, &m, &line);
                    println!("--");
                    MatchReplacement {
                        search_match: m,
                        replacement: line.into(),
                    }
                }
            };

            replacement_list.push(match_replacement);
            self.total_replacements += 1;
        }

        if !replacement_list.is_empty() {
            self.apply_replacements(path, &replacement_list)?;
        }

        Ok(())
    }

    fn apply_replacements(&self, path: &Path, mut replacements: &[MatchReplacement]) -> Result<()> {
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

            if !replacements.is_empty() && replacements[0].search_match.line.0 == line_num {
                writer.write_all(replacements[0].replacement.as_bytes())?;
                replacements = &replacements[1..];
            } else {
                writer.write_all(line.as_bytes())?;
            }
        }

        std::fs::rename(dst_path, path)?;
        Ok(())
    }

    fn finalize(&self) {
        self.match_formatter
            .display_footer(self.total_replacements, self.total_matches);
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
        ReplacementDecider {
            should_prompt: true,
            global_decision: None,
            local_decision: None,
        }
    }

    fn constantly(decision: ReplacementDecision) -> ReplacementDecider {
        ReplacementDecider {
            should_prompt: false,
            global_decision: Some(decision),
            local_decision: None,
        }
    }

    fn reset_local_decision(&mut self) {
        self.local_decision = None;
    }

    fn decide(&mut self) -> ReplacementDecision {
        if let Some(global_decision) = self.global_decision {
            return global_decision;
        } else if let Some(local_decision) = self.local_decision {
            return local_decision;
        }

        if !self.should_prompt {
            panic!("[bug] invalid state: no decision, but should not prompt");
        }

        self.prompt_for_decision()
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

                _ => {
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
    path_matcher: PathMatcher,
    match_processor: MatchProcessor,
    searcher_factory: Box<dyn Fn() -> SearchProcessor + Sync>,
}

const DEFAULT_BEFORE_CONTEXT_LINES: usize = 2;
const DEFAULT_AFTER_CONTEXT_LINES: usize = 2;

impl FindAndReplacer {
    fn from_config(config: Config) -> Result<FindAndReplacer> {
        let pattern = if config.literal {
            regex::escape(&config.find)
        } else {
            config.find.to_owned()
        };

        let pattern_matcher = RegexMatcherBuilder::new()
            .case_insensitive(!config.case_sensitive && config.ignore_case)
            .case_smart(!config.case_sensitive && config.smart_case.unwrap_or(true))
            .build(&pattern)
            .with_context(|| format!("Failed to parse pattern '{}'", pattern))?;

        let mut searcher_builder = SearcherBuilder::new();
        searcher_builder
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
            );

        // TODO: Confirm that template does not reference more capture groups than exist.
        let replacer = RegexReplacer {
            matcher: pattern_matcher.clone(),
            template: config.replace.to_owned(),
        };

        let replacement_decider = if config.prompt {
            ReplacementDecider::with_prompt()
        } else if config.write {
            ReplacementDecider::constantly(ReplacementDecision::Accept)
        } else {
            ReplacementDecider::constantly(ReplacementDecision::Ignore)
        };

        let match_formatter = MatchFormatter::from_config(&config);
        let match_processor = MatchProcessor::new(replacer, replacement_decider, match_formatter);

        let searcher_factory = {
            let matcher = Arc::new(pattern_matcher);

            Box::new(move || SearchProcessor {
                matcher: matcher.clone(),
                searcher: searcher_builder.build(),
            })
        };

        let paths = if config.paths.is_empty() {
            // Read paths from standard in if none are specified and
            // there's input piped to the process.
            if !atty::is(Stream::Stdin) {
                ensure!(
                    !config.prompt,
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

        let mut file_walker = WalkBuilder::new(&paths[0]);
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
            RegexSet::new(escaped).unwrap()
        });
        let excluded_paths = {
            let escaped = config.exclude.iter().map(|p| regex::escape(p));
            RegexSet::new(escaped)?
        };

        let path_matcher = PathMatcher {
            included_paths,
            excluded_paths,
        };

        Ok(FindAndReplacer {
            file_walker,
            path_matcher,
            searcher_factory,
            match_processor,
        })
    }

    fn run(&mut self) -> Result<()> {
        thread::scope(|thread_scope| {
            let (tx, rx) = channel();
            let file_walker = self.file_walker.build_parallel();
            let searcher_factory = &self.searcher_factory;
            let path_matcher = &self.path_matcher;
            thread_scope.spawn(move |_| {
                file_walker.run(|| {
                    let tx = tx.clone();

                    Box::new(move |dir_entry| {
                        let dir_entry = dir_entry.unwrap();
                        if !dir_entry.file_type().unwrap().is_file() {
                            return WalkState::Continue;
                        }

                        let path = dir_entry.path();
                        if !path_matcher.is_match(path) {
                            return WalkState::Continue;
                        }

                        let mut searcher = (searcher_factory)();
                        let matches = searcher.search_path(path).unwrap();
                        tx.send((path.to_owned(), matches)).unwrap();

                        WalkState::Continue
                    })
                });

                // Close the channel
                drop(tx);
            });

            for (path, mut matches) in rx.iter() {
                self.match_processor
                    .consume_matches(&path, &mut matches)
                    .unwrap();
            }
        })
        .unwrap();

        self.match_processor.finalize();

        Ok(())
    }
}

struct PathMatcher {
    included_paths: Option<RegexSet>,
    excluded_paths: RegexSet,
}

impl PathMatcher {
    fn is_match(&self, path: &Path) -> bool {
        let path_str = path
            .to_str()
            .with_context(|| format!("Failed to interpret path name as UTF-8 string: {:?}", path))
            .unwrap();

        if let Some(included_paths) = &self.included_paths {
            return included_paths.is_match(path_str);
        }

        !self.excluded_paths.is_match(path_str)
    }
}

// Main entry point
fn run_find_and_replace() -> Result<()> {
    let config = Config::from_args();
    let mut find_and_replacer = FindAndReplacer::from_config(config)?;

    find_and_replacer.run()
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

#[cfg(test)]
mod tests {
    use super::*;
    mod path_matcher {
        use super::*;

        fn as_regex_set(v: Vec<&str>) -> RegexSet {
            let escaped = v.iter().map(|r| regex::escape(r));
            RegexSet::new(escaped).unwrap()
        }

        #[test]
        fn test_empty_included_set() {
            let disallow_list: Vec<&str> = vec![];

            let matcher = PathMatcher {
                included_paths: None,
                excluded_paths: as_regex_set(disallow_list),
            };

            assert_eq!(matcher.is_match(&Path::new("foo")), true);
        }

        #[test]
        fn test_included_set() {
            let allow_list: Vec<&str> = vec!["foo", "bar"];
            let disallow_list: Vec<&str> = vec![];

            let matcher = PathMatcher {
                included_paths: Some(as_regex_set(allow_list)),
                excluded_paths: as_regex_set(disallow_list),
            };

            assert_eq!(matcher.is_match(&Path::new("foo.rs")), true);
            assert_eq!(matcher.is_match(&Path::new("bar.rs")), true);
            assert_eq!(matcher.is_match(&Path::new("baz.rs")), false);
        }

        #[test]
        fn test_excluded_set() {
            let disallow_list = vec!["foo", "bar"];
            let matcher = PathMatcher {
                included_paths: None,
                excluded_paths: as_regex_set(disallow_list),
            };

            assert_eq!(matcher.is_match(&Path::new("foo.rs")), false);
            assert_eq!(matcher.is_match(&Path::new("bar.rs")), false);
            assert_eq!(matcher.is_match(&Path::new("baz.rs")), true);
        }

        // Inclusion should take precedence
        #[test]
        fn test_included_and_excluded_set() {
            let allow_list = vec!["foo", "bar"];
            let disallow_list = vec!["foo", "bar"];

            let matcher = PathMatcher {
                included_paths: Some(as_regex_set(allow_list)),
                excluded_paths: as_regex_set(disallow_list),
            };

            assert_eq!(matcher.is_match(&Path::new("foo.rs")), true);
            assert_eq!(matcher.is_match(&Path::new("bar.rs")), true);
            assert_eq!(matcher.is_match(&Path::new("baz.rs")), false);
        }
    }
}
