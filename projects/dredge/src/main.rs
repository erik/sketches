extern crate clap;
extern crate termion;

use std::collections::HashMap;
use std::cmp::min;
use std::fs::File;
use std::io::{stdin, stdout, BufRead, BufReader, Write};
use std::option::Option;
use std::path::Path;
use std::process::{Command, Stdio};
use std::sync::mpsc::{channel, Receiver, Sender};
use std::thread;
use std::vec::Vec;

use clap::{App, Arg};
use termion::color;
use termion::event::{Event, Key, MouseEvent};
use termion::input::{MouseTerminal, TermRead};
use termion::raw::IntoRawMode;

const VERT_SEPARATOR: &'static str = "│";
const LIST_WIDTH: u16 = 32;

enum ChanMessage {
    SearchResult(ListEntry),
    SearchComplete,
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
    current_index: Option<usize>,
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

    fn scroll_down(&mut self) {
        self.current_index = self.current_index
            .map(|prev| min(self.entries.len() - 1, prev + 1))
    }

    fn scroll_up(&mut self) {
        self.current_index = self.current_index
            .map(|prev| if prev == 0 { prev } else { prev - 1 })
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
        // .arg(
        //     Arg::with_name("directories")
        //         .help("where to search for it")
        //         .multiple(true)
        //         .default_value("."),
        // )
        // .arg(
        //     Arg::with_name("options")
        //         .help("options to pass directly to search program.")
        //         .multiple(true)
        //         .default_value("")
        //         .last(true),
        // )
        .get_matches();

    let (tx, rx) = channel();

    let cmd_tx = tx.clone();
    let term_tx = tx.clone();

    thread::spawn(move || {
        run_command(matches, cmd_tx).expect("failed to run command");
    });

    thread::spawn(move || {
        let stdin = stdin();

        for c in stdin.events() {
            term_tx
                .send(ChanMessage::TerminalEvent(c.unwrap()))
                .unwrap();
        }
    });

    ui_loop(rx).unwrap();
}

fn run_command(
    config: clap::ArgMatches,
    sender: Sender<ChanMessage>,
) -> Result<(), Box<std::error::Error>> {
    let cmd = Command::new("ag")
        .arg(config.value_of("query").unwrap())
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

    sender.send(ChanMessage::SearchComplete)?;

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
            ChanMessage::SearchComplete => {
                if file_list.entries.is_empty() {
                    println!("No matches");
                    break;
                }
            }

            ChanMessage::TerminalEvent(evt) => match evt {
                Event::Key(Key::Ctrl('c')) | Event::Key(Key::Char('q')) => break,

                Event::Key(Key::Char('j')) => {
                    redraw = true;
                    file_list.scroll_down();
                }

                Event::Key(Key::Char('k')) => {
                    redraw = true;
                    file_list.scroll_up();
                }

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

                if file_list.current_index.is_none() {
                    file_list.current_index = Some(0)
                }
            }
        }

        if redraw {
            let (width, height) = termion::terminal_size()?;
            let offset = file_list.current_index.unwrap_or(0);

            for row in 0..height {
                let index = (offset + row as usize) as usize;

                if index < file_list.entries.len() {
                    let ref entry = &file_list.entries[index];
                    let (fg, bg): (&color::Color, &color::Color) =
                        if Some(index) == file_list.current_index {
                            (&color::Black, &color::White)
                        } else {
                            (&color::Reset, &color::Reset)
                        };

                    write!(
                        stdout,
                        "{}{}{}{}/{}:{}{}{}",
                        termion::cursor::Goto(1, 1 + row as u16),
                        color::Fg(fg),
                        color::Bg(bg),
                        entry.directory,
                        entry.file_name,
                        entry.line_number,
                        color::Fg(color::Reset),
                        color::Bg(color::Reset),
                    )?;
                } else {
                    write!(
                        stdout,
                        "{}{}",
                        termion::cursor::Goto(1, 1 + row as u16),
                        "                     "
                    )?;
                }

                write!(
                    stdout,
                    "{}{}",
                    termion::cursor::Goto(LIST_WIDTH, 1 + row),
                    VERT_SEPARATOR
                )?;
            }

            write!(stdout, "{}", termion::cursor::Hide)?;
            stdout.flush()?;
        }
    }

    Ok(())
}
