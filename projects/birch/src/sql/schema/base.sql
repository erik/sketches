BEGIN;

CREATE TABLE IF NOT EXISTS networks (
    name        TEXT PRIMARY KEY
  , address     TEXT               -- `host:port`
  , nick        TEXT               -- Initial nick
  , server_pass TEXT               -- Credentials to send in PASS command on startup
  , client_pass TEXT               -- Encoding `user:crypt(password)` for client logins
);

CREATE TABLE IF NOT EXISTS cursors (
    id            TEXT
  , network_name  TEXT
  , channel       TEXT
  , position      INTEGER

  , UNIQUE(id, network_name, channel)
  , FOREIGN KEY (network_name) REFERENCES networks (name)
);

CREATE TABLE IF NOT EXISTS logs (
    id            INTEGER PRIMARY KEY AUTOINCREMENT
  , network_name  TEXT
  , timestamp     INTEGER
  , channel       TEXT
  , raw_line      TEXT

  , FOREIGN KEY (network_name) REFERENCES networks (name)
);

COMMIT;
