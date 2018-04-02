extern crate clap;
extern crate termion;

use std::collections::HashMap;
use std::error::Error;
use std::fs::File;
use std::ffi::OsString;
use std::io::{stdin, stdout, BufRead, BufReader, Write};
use std::option::Option;
use std::path::Path;
use std::process::{Command, Stdio};
use std::sync::mpsc::{channel, Receiver, Sender};
use std::thread;
use std::vec::Vec;

use clap::{App, Arg};
use termion::event::{Event, Key, MouseEvent};
use termion::input::{MouseTerminal, TermRead};
use termion::raw::IntoRawMode;

const VERT_SEPARATOR: &'static str = "│";
const HORIZ_SEPARATOR: &'static str = "─";

enum ChanMessage {
    SearchResult(ListEntry),
    TerminalEvent(Event),
}

#[derive(Debug, Clone)]
struct ListEntry {
    directory: String,
    file_name: String,
    line_number: u32,
    text: String,
}

impl ListEntry {
    // format is: path/to/file.rs:123:line of text
    fn from_line(line: &str) -> Option<ListEntry> {
        let parts: Vec<&str> = line.splitn(3, ':').collect();

        if parts.len() != 3 {
            return None;
        }

        let path = Path::new(parts[0]);

        // FIXME: This cannot possibly be right. wtf.
        Some(ListEntry {
            directory: String::from(path.parent().unwrap().to_str().unwrap()),
            file_name: String::from(path.file_name().unwrap().to_str().unwrap()),
            line_number: parts[1].parse().unwrap(),
            text: String::from(parts[2]),
        })
    }
}

#[derive(Debug, Clone)]
struct FileList {
    entries: Vec<ListEntry>,
    current_index: Option<u32>,
}

impl FileList {
    fn new() -> FileList {
        FileList {
            entries: vec![],
            current_index: None,
        }
    }

    fn add_entry(&mut self, entry: ListEntry) {
        // Insert the new element into it's sorted position
        let insert_pos = {
            let new_key = (&entry.directory, &entry.file_name, &entry.line_number);
            let result = self.entries.binary_search_by(|other| {
                let k = (&other.directory, &other.file_name, &other.line_number);
                return k.cmp(&new_key);
            });

            match result {
                Ok(pos) | Err(pos) => pos,
            }
        };

        self.entries.insert(insert_pos, entry);

        if self.current_index.is_none() {
            self.current_index = Some(0);
        }
    }
}

#[derive(Debug)]
struct UI {
    list: FileList,
    file_cache: HashMap<(String, String), File>,
}

fn main() {
    let matches = App::new("dredge")
        .about("Search interactively")
        .arg(
            Arg::with_name("program")
                .long("program")
                .short("p")
                .value_name("PROGRAM")
                .default_value("ag")
                .help("grep or ag or w/e"),
        )
        .arg(
            Arg::with_name("query")
                .help("what to search for")
                .required(true),
        )
        .arg(
            Arg::with_name("directories")
                .help("where to search for it")
                .multiple(true)
                .default_value("."),
        )
        .arg(
            Arg::with_name("options")
                .help("options to pass directly to search program.")
                .multiple(true)
                .last(true),
        )
        .get_matches();

    let (tx, rx) = channel();

    let cmd_tx = tx.clone();
    let term_tx = tx.clone();

    thread::spawn(move || {
        run_command(cmd_tx).expect("failed to run command");
    });

    thread::spawn(move || {
        let stdin = stdin();

        for c in stdin.events() {
            term_tx
                .send(ChanMessage::TerminalEvent(c.unwrap()))
                .unwrap();
        }
    });

    ui_loop(rx);
}

fn run_command(sender: Sender<ChanMessage>) -> Result<(), Box<std::error::Error>> {
    let cmd = Command::new("ag")
        .args([".", "src"].iter())
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .spawn()?;

    let output = cmd.stdout.unwrap();
    let reader = BufReader::new(output);

    for line in reader.lines() {
        if let Some(entry) = ListEntry::from_line(&line.unwrap()) {
            sender.send(ChanMessage::SearchResult(entry))?;
        }
    }

    Ok(())
}

fn ui_loop(receiver: Receiver<ChanMessage>) -> Result<(), Box<std::error::Error>> {
    let mut file_list = FileList::new();

    let mut stdout = MouseTerminal::from(stdout().into_raw_mode()?);
    write!(
        stdout,
        "{}{}",
        termion::clear::All,
        termion::cursor::Goto(1, 1)
    )?;

    for entry in receiver.iter() {
        let mut redraw = false;

        match entry {
            ChanMessage::TerminalEvent(evt) => match evt {
                Event::Key(Key::Ctrl('c')) | Event::Key(Key::Char('q')) => break,
                Event::Mouse(me) => match me {
                    MouseEvent::Press(_, x, y) => {
                        write!(stdout, "{}x", termion::cursor::Goto(x, y))?;
                        stdout.flush()?;
                    }

                    _ => (),
                },

                _ => {}
            },

            ChanMessage::SearchResult(entry) => {
                redraw = true;
                file_list.add_entry(entry);
            }
        }

        if redraw {
            for (i, entry) in file_list.entries.iter().enumerate() {
                write!(
                    stdout,
                    "{}{}",
                    termion::cursor::Goto(1, 1 + i as u16),
                    entry.file_name
                )?;
                stdout.flush()?;
            }
        }
    }

    Ok(())
}
