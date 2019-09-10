extern crate serde_derive;

use std::collections::{BTreeMap, HashSet};
use std::error::Error;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};

use chrono::{DateTime, Utc};
use clap::{App, Arg, ArgMatches, SubCommand};
use glob::glob;
use serde_derive::{Deserialize, Serialize};

#[derive(Deserialize, Debug, Clone)]
struct RepoConfig {
    path: String,
    #[serde(default = "default_false")]
    commit: bool,
    #[serde(default = "default_false")]
    sync_updates: bool,
}

fn default_false() -> bool {
    false
}

#[derive(Deserialize, Debug)]
struct Supfile {
    repo: RepoConfig,
    editor: Option<String>,
}

impl Supfile {
    pub fn from(str: &str) -> Result<Self, toml::de::Error> {
        toml::from_str(&str)
    }

    pub fn find() -> PathBuf {
        // TODO: pull in XDG crate for paths
        let default_dir = "~/.config/sup/";

        ["./", default_dir]
            .iter()
            .map(|dir| Path::new(dir).join("Supfile"))
            .find(|p| p.exists())
            .unwrap_or_else(|| {
                // TODO: offer to create path somehow
                panic!("can't find supfile")
            })
    }
}

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
enum TaskState {
    Todo,
    InProgress,
    Complete,
    Canceled,
    Deferred,
}

impl TaskState {
    fn is_open(self) -> bool {
        match self {
            TaskState::Todo | TaskState::InProgress | TaskState::Deferred => true,
            _ => false,
        }
    }
}

impl std::fmt::Display for TaskState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let value = match self {
            TaskState::Todo => " ",
            TaskState::InProgress => "•",
            TaskState::Complete => "✔",
            TaskState::Canceled => "-",
            TaskState::Deferred => "⟫",
        };
        write!(f, "{}", value)
    }
}

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
enum Action {
    Create {
        task: String,
        #[serde(default)]
        notes: Option<String>,
        #[serde(default)]
        tags: Vec<String>,
    },
    EditTags {
        add: Vec<String>,
        remove: Vec<String>,
    },
    EditTask(String),
    EditNotes(Option<String>),
    EditState(TaskState),
}

#[derive(Deserialize, Serialize)]
struct TaskUpdate {
    id: String,
    timestamp: DateTime<Utc>,
    action: Action,
}

impl TaskUpdate {
    fn from_str(line: &str) -> serde_json::Result<Self> {
        serde_json::from_str(line).map_err(|e| {
            println!("nuts: {:?}", e);
            e
        })
    }
}

#[derive(Clone, Debug)]
struct Task {
    id: String,
    created_at: DateTime<Utc>,
    completed_at: Option<DateTime<Utc>>,
    state: TaskState,
    task: String,
    notes: Option<String>,
    tags: HashSet<String>,
}

impl Task {
    fn apply(&mut self, update: &TaskUpdate) {
        match &update.action {
            Action::Create { task, notes, tags } => {
                self.task = task.clone();
                self.notes = notes.clone();
                for tag in tags.iter() {
                    self.tags.insert(tag.clone());
                }
            }
            Action::EditTags { add, remove } => {
                for tag in remove.iter() {
                    self.tags.remove(tag);
                }

                for tag in add.iter() {
                    self.tags.insert(tag.clone());
                }
            }
            Action::EditTask(task) => self.task = task.clone(),
            Action::EditNotes(notes) => self.notes = notes.clone(),
            Action::EditState(state) => {
                self.state = state.clone();
                if state == &TaskState::Complete {
                    self.completed_at = Some(update.timestamp);
                }
            }
        }
    }

    fn summary(&self) -> String {
        format!("{} [{}] {}", self.id, self.state, self.task)
    }
}

impl From<&TaskUpdate> for Task {
    fn from(update: &TaskUpdate) -> Self {
        let mut task = Task {
            id: update.id.clone(),
            created_at: update.timestamp.clone(),
            completed_at: None,
            state: TaskState::Todo,
            task: "untitled".to_string(),
            notes: None,
            tags: HashSet::new(),
        };

        task.apply(update);
        return task;
    }
}

#[derive(Debug)]
struct SupdateLog {
    name: String,
    file: BufReader<File>,
}

impl SupdateLog {
    fn new(path: &Path) -> Self {
        let name = path
            .file_name()
            .expect("need file, not directory")
            .to_string_lossy()
            .into();

        let file = File::open(path).expect("could not open file");
        // TODO: Support versioning in line

        SupdateLog {
            name,
            file: BufReader::new(file),
        }
    }
}

impl Iterator for SupdateLog {
    type Item = TaskUpdate;

    fn next(&mut self) -> Option<TaskUpdate> {
        let mut line = String::new();
        return match self.file.read_line(&mut line) {
            // End of file
            Ok(0) => None,

            Ok(_) => {
                //println!("line = {:?}", line);
                // TODO: alert when we don't succeed at parsing
                TaskUpdate::from_str(&line).ok()
            }

            Err(e) => {
                println!("Failed to read line: {:?}", e);
                None
            }
        };
    }
}

/**
 * A Repository represents a directory containing SupdateLogs.
 *
 * The directory structure is something like this:
 *
 * repo/
 *   2019/
 *     08/
 *       20190801_235959.sup
 *       20190802_003030.sup
 *       20190802_123030.sup
 *
 * TODO: Support concept of `rollup` files to put an upper limit on
 * how far we need to look back to fully reconstruct the state of
 * everything.
 */

struct Repository {
    config: RepoConfig,
}

impl Repository {
    fn new(cfg: RepoConfig) -> Self {
        // TODO: Handle error, return Result
        if !Path::new(&cfg.path).exists() {
            panic!("given repository does not exist")
        }

        Repository { config: cfg }
    }

    fn open_journal(&self) -> Journal {
        let updates = self.read_updates();
        self.build_journal(updates)
    }

    fn build_journal(&self, updates: Vec<SupdateLog>) -> Journal {
        let mut journal = Journal::new();

        // Reverse ordering so that we consume oldest elements first.
        for log in updates.into_iter().rev() {
            println!("reading: {:?}", log);

            for ref update in log {
                journal.add(update);
            }
        }

        journal
    }

    fn read_updates(&self) -> Vec<SupdateLog> {
        let path = Path::new(&self.config.path).join("**/*.sup");

        // TODO: This is really wasteful. Make this an iterator
        // Get list of all files in the repo
        let mut updates = glob(path.to_str().unwrap())
            .expect("glob failed")
            .filter_map(|f| f.map(|p| SupdateLog::new(&p)).ok())
            .collect::<Vec<SupdateLog>>();

        // We want sorted (chronological, desc) order, based on file name.
        updates.sort_by(|a, b| b.name.cmp(&a.name));
        return updates;
    }
}

/**
 * A collection of Updates.
 *
 * Represents the cumulative, "rolled up" set of task updates.
 */
#[derive(Debug)]
struct Journal {
    /// id -> task
    entries: BTreeMap<String, Task>,
}

impl Journal {
    fn new() -> Self {
        return Self {
            entries: BTreeMap::new(),
        };
    }

    fn add(&mut self, update: &TaskUpdate) {
        let task = self.entries.get_mut(&update.id);

        match task {
            Some(t) => t.apply(update),
            None => {
                self.entries.insert(update.id.clone(), Task::from(update));
            }
        }
    }

    // TODO: Hack until a custom iterator is implemented.
    fn for_each(&self, f: &Fn(&Task) -> ()) {
        self.entries.values().for_each(f)
    }
}

struct SupCli {
    matches: ArgMatches<'static>,
}

impl SupCli {
    fn new() -> Self {
        Self {
            matches: Self::parse_args(),
        }
    }

    fn parse_args<'a>() -> ArgMatches<'a> {
        App::new("sup")
            .version("0.0.0")
            .about("Keep track of what's up.")
            .arg(
                Arg::with_name("config")
                    .short("c")
                    .long("config")
                    .value_name("FILE")
                    .takes_value(true)
                    .help("Set custom location for Supfile"),
            )
            .subcommand(
                SubCommand::with_name("todo")
                    .about("create a new todo item")
                    .arg(Arg::with_name("TASK").takes_value(true).multiple(true)),
            )
            .subcommand(
                SubCommand::with_name("show")
                    .about("asdf")
                    .arg(Arg::with_name("DATE").takes_value(true).help("asdf")),
            )
            .subcommand(
                SubCommand::with_name("serve")
                    .about("launch web interface")
                    .arg(
                        Arg::with_name("port")
                            .short("p")
                            .long("port")
                            .takes_value(true),
                    ),
            )
            .get_matches()
    }

    fn load_config(&self) -> Result<Supfile, Box<dyn Error>> {
        let path = self
            .matches
            .value_of("config")
            .map(|p| Path::new(p).to_path_buf())
            .unwrap_or_else(Supfile::find);

        let bytes = std::fs::read_to_string(path.as_os_str())?;
        Ok(Supfile::from(&bytes)?)
    }

    fn run(&self) -> Result<(), Box<dyn Error>> {
        let cfg = self.load_config().expect("error in config file");
        let app = SupApp::new(&cfg);

        match self.matches.subcommand() {
            ("show", Some(m)) => println!("TODO: show {:?}", m),
            ("serve", Some(m)) => println!("TODO: serve {:?}", m),
            ("todo", Some(m)) => {
                println!("here's the todo: {:?}", m);
                app.run_add_update()?;
            }
            _ => {
                println!("add update");
                app.run_show_updates()?;
            }
        };

        Ok(())
    }
}

struct SupApp {
    repo: Repository,
    journal: Journal,
}

impl SupApp {
    fn new(cfg: &Supfile) -> Self {
        let repo = Repository::new(cfg.repo.clone());
        let journal = repo.open_journal();

        Self { repo, journal }
    }

    fn run_add_update(&self) -> Result<(), Box<dyn Error>> {
        println!("journal: {:?}", self.journal);
        Ok(())
    }

    fn run_show_updates(&self) -> Result<(), Box<dyn Error>> {
        self.journal.for_each(&|task| {
            println!("{}", task.summary());
        });

        Ok(())
    }
}

// TODO: Investiage this instead of Box<Error>
// https://github.com/rust-lang-nursery/failure
fn main() {
    let app = SupCli::new();
    match app.run() {
        Err(e) => println!("oh no! {:?}", e),
        Ok(_) => (),
    };
}
