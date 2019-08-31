extern crate serde_derive;

use std::error::Error;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};

use chrono::{DateTime, Utc};
use clap::{App, Arg, ArgMatches, SubCommand};
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

#[derive(Deserialize, Serialize)]
enum TaskState {
    Todo,
    Partial,
    Complete,
    Canceled,
    Deferred { until: DateTime<Utc> },
}

#[derive(Deserialize, Serialize)]
enum UpdateKind {
    Note(String),
    Task(TaskState, String),
}

/// A Sup update is a supdate.
///
/// I hate this.
#[derive(Deserialize, Serialize)]
struct Supdate {
    id: String,
    timestamp: DateTime<Utc>,
    kind: UpdateKind,
    tags: Vec<String>,
}

impl Supdate {
    fn new(line: &str) -> Self {
        unimplemented!()
    }
}

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

    fn is_rollup(&self) -> bool {
        self.name.ends_with("0000_rollup.sup")
    }
}

impl Iterator for SupdateLog {
    type Item = Supdate;

    fn next(&mut self) -> Option<Supdate> {
        let mut line = String::new();
        return match self.file.read_line(&mut line) {
            // End of file
            Ok(0) => None,

            Ok(_) => {
                println!("line = {:?}", line);
                Some(Supdate::new(&line))
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
 *       20190800_rollup.sup
 *       20190801_235959.sup
 *       20190802_003030.sup
 *       20190802_123030.sup
 *
 * Rollups are special SupdateLogs which contain the collapsed content
 * of earlier entries, to put a bound on the number of entries which
 * must be read in order to construct the current state.
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

    fn read_updates(&self) -> Vec<Supdate> {
        let file = File::open(&self.config.path).expect("");

        vec![]
    }
}

struct SupApp {
    matches: ArgMatches<'static>,
}

impl SupApp {
    fn new() -> Self {
        SupApp {
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
        let cfg = self.load_config()?;
        let repo = Repository::new(cfg.repo.clone());

        match self.matches.subcommand() {
            ("show", Some(m)) => println!("TODO: show {:?}", m),
            ("serve", Some(m)) => println!("TODO: serve {:?}", m),
            _ => {
                println!("add update");

                run_add_update(&cfg)?;
            }
        };

        Ok(())
    }
}

fn run_add_update(_cfg: &Supfile) -> Result<(), Box<dyn Error>> {
    Ok(())
}

// TODO: Investiage this instead of Box<Error>
// https://github.com/rust-lang-nursery/failure
fn main() {
    let app = SupApp::new();
    match app.run() {
        Err(e) => println!("oh no! {:?}", e),
        Ok(_) => (),
    };
}
