extern crate serde_derive;

use std::collections::{BTreeMap, HashSet};
use std::error::Error;
use std::fs::{File, OpenOptions};
use std::io::{BufRead, BufReader, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};

use chrono::offset::TimeZone;
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

    /// Create a basic TaskUpdate. The `id` is left unset and must be set before being written.
    fn new_todo(id: String, task: String, notes: Option<String>, tags: Vec<String>) -> Self {
        Self {
            id: id,
            timestamp: Utc::now(),
            action: Action::Create { task, notes, tags },
        }
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
    path: String,

    timestamp: DateTime<Utc>,
}

impl SupdateLog {
    fn new(path: &Path) -> Self {
        let name: String = path
            .file_name()
            .expect("need file, not directory")
            .to_string_lossy()
            .into();

        let timestamp = Utc
            .datetime_from_str(&name, "%Y%m%d_%H%M%S.sup")
            .unwrap_or_else(|_| {
                println!("{:?} appears to be improperly named!", path);
                Utc.timestamp(0, 0)
            });

        // TODO: Support versioning in line
        SupdateLog {
            name,
            path: path.to_string_lossy().into(),
            timestamp,
        }
    }

    fn write_update(&self, task: &TaskUpdate) -> std::io::Result<()> {
        let mut file = self.open_file()?;
        file.seek(SeekFrom::End(0)).expect("could not seek to end");

        serde_json::ser::to_writer(&file, task)?;
        file.write_all(b"\n")?;

        Ok(())
    }

    fn iter(&self) -> SupdateIterator {
        let mut file = self.open_file().expect("could not open file");
        file.seek(SeekFrom::Start(0)).expect("could not rewind");

        SupdateIterator {
            reader: BufReader::new(file),
        }
    }

    /// Open the log file or create it if it doesn't exist.
    fn open_file(&self) -> std::io::Result<File> {
        OpenOptions::new()
            .read(true)
            .append(true)
            .create(true)
            .open(&self.path)
    }
}

struct SupdateIterator {
    reader: BufReader<File>,
}

impl Iterator for SupdateIterator {
    type Item = TaskUpdate;

    fn next(&mut self) -> Option<TaskUpdate> {
        let mut line = String::new();
        return match self.reader.read_line(&mut line) {
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
        let updates = self.list_update_logs();
        self.build_journal(updates)
    }

    fn build_journal(&self, updates: Vec<SupdateLog>) -> Journal {
        let mut journal = Journal::new();

        // Reverse ordering so that we consume oldest elements first.
        for log in updates.into_iter().rev() {
            for ref update in log.iter() {
                journal.add(update);
            }
        }

        journal
    }

    fn list_update_logs(&self) -> Vec<SupdateLog> {
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

    fn active_update_log(&self) -> SupdateLog {
        let now = Utc::now();
        let mut logs = self.list_update_logs();

        let last_update = if !logs.is_empty() {
            Some(logs.remove(0))
        } else {
            None
        };

        // Use the last update if it's still the same day, otherwise create a new one.
        last_update
            .filter(|log| log.timestamp.date() == now.date())
            .unwrap_or_else(|| {
                let log_path = now.format("%Y/%m/%Y%m%d_%H%M%S.sup").to_string();
                let abs_path = Path::new(&self.config.path).join(log_path);

                let dir = abs_path.parent().unwrap();

                std::fs::create_dir_all(dir).expect("create directory failed");

                SupdateLog::new(&abs_path)
            })
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
    tasks: BTreeMap<String, Task>,
    last_id: u64,
}

impl Journal {
    fn new() -> Self {
        return Self {
            tasks: BTreeMap::new(),
            last_id: 0,
        };
    }

    fn add(&mut self, update: &TaskUpdate) {
        let id = &update.id;

        // Keep track of higest id value we've seen.
        let id_int = id.parse::<u64>().unwrap();
        self.last_id = std::cmp::max(self.last_id, id_int);

        let task = self.tasks.get_mut(id);

        match task {
            Some(t) => t.apply(update),
            None => {
                self.tasks.insert(id.clone(), Task::from(update));
            }
        }
    }

    fn get_task(&self, id: &String) -> Option<&Task> {
        self.tasks.get(id)
    }

    // TODO: Hack until a custom iterator is implemented.
    fn for_each(&self, f: &Fn(&Task) -> ()) {
        self.tasks.values().for_each(f)
    }

    fn next_id(&mut self) -> String {
        self.last_id += 1;

        format!("{:04}", self.last_id)
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
                    .about("show full details of task")
                    .arg(
                        Arg::with_name("TASK_ID")
                            .required(true)
                            .takes_value(true)
                            .help("asdf"),
                    ),
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
        let mut app = SupApp::new(&cfg);

        match self.matches.subcommand() {
            ("show", Some(m)) => {
                let id = m.value_of("TASK_ID").unwrap();

                app.run_show_task(id.to_string())
            }
            ("todo", Some(m)) => {
                let task = m.values_of("TASK").map(|v| {
                    let vec = v.collect::<Vec<_>>();
                    vec.join(" ")
                });

                app.run_add_task(task)
            }

            _ => app.run_show_updates(),
        }
    }
}

struct SupApp {
    repo: Repository,
}

impl SupApp {
    fn new(cfg: &Supfile) -> Self {
        let repo = Repository::new(cfg.repo.clone());

        Self { repo }
    }

    fn run_add_task(&mut self, task: Option<String>) -> Result<(), Box<dyn Error>> {
        let mut journal = self.repo.open_journal();

        let task = task.expect("TODO: ui task creation");
        let id = journal.next_id();

        let update = TaskUpdate::new_todo(id, task, None, vec![]);
        self.repo.active_update_log().write_update(&update)?;

        self.run_show_updates()
    }

    fn run_show_task(&self, id: String) -> Result<(), Box<dyn Error>> {
        let journal = self.repo.open_journal();

        let task = journal.get_task(&id).expect("unknown task id");

        println!(
            "{} {}

{}

tags: {}
created: {}
completed: {:?}",
            task.state,
            task.task,
            &task.notes.clone().unwrap_or("[no details]".to_string()), // TODO: try
            "".to_string(), // task.tags.iter().collect::<Vec<_>>().join(", "),
            task.created_at,
            task.completed_at
        );

        Ok(())
    }

    fn run_show_updates(&self) -> Result<(), Box<dyn Error>> {
        let journal = self.repo.open_journal();
        journal.for_each(&|task| {
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
