BEGIN;

CREATE TABLE IF NOT EXISTS networks (
    name        TEXT PRIMARY KEY
  , address     TEXT NOT NULL     -- `host:port`
  , nick        TEXT NOT NULL     -- Initial nick
  , server_pass TEXT              -- Credentials to send in PASS command on startup
  , client_pass TEXT NOT NULL     -- Encoding `user:crypt(password)` for client logins
);

CREATE TABLE IF NOT EXISTS cursors (
    id            TEXT    NOT NULL
  , network_name  TEXT    NOT NULL
  , channel       TEXT    NOT NULL
  , position      INTEGER NOT NULL

  , UNIQUE(id, network_name, channel)
  , FOREIGN KEY (network_name) REFERENCES networks (name)
);

CREATE TABLE IF NOT EXISTS logs (
    id            INTEGER PRIMARY KEY AUTOINCREMENT
  , network_name  TEXT    NOT NULL
  , timestamp     INTEGER NOT NULL
  , channel       TEXT    NOT NULL
  , raw_line      TEXT    NOT NULL

  , FOREIGN KEY (network_name) REFERENCES networks (name)
);

COMMIT;
