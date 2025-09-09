import json
import os
import sqlite3
import threading
import sys
from pathlib import Path

from dotenv import load_dotenv
from flask import Flask, render_template
import flask
from telegram import Update, Video, VideoNote, Voice
import telegram
from telegram.ext import (
    ApplicationBuilder,
    CommandHandler,
    CallbackContext,
    MessageHandler,
    filters,
)


app = Flask(__name__)
thread_local = threading.local()


def get_db() -> sqlite3.Connection:
    if not hasattr(thread_local, "_database"):
        thread_local._database = sqlite3.connect(os.environ["SQLITE_DB_PATH"])
        thread_local._database.row_factory = sqlite3.Row
    return thread_local._database


def get_media_storage_path() -> Path:
    """Get the media storage directory from config, create if it doesn't exist"""
    media_path = Path(os.environ.get("MEDIA_STORAGE_PATH", "./media"))
    media_path.mkdir(parents=True, exist_ok=True)
    return media_path


@app.before_request
def prefer_cdn():
    # HACK: if we're deployed, point to CDN
    if "localhost" not in flask.request.host and flask.request.host != "sorelegs.club":
        return flask.redirect(
            f"https://sorelegs.club{flask.request.full_path}", code=301
        )


@app.get("/")
@app.get("/<username>")
def view_feed(username: str | None = None):
    # If we allow `/xmlrpc.php` etc. the bots get excited and try all sorts of things.
    if username and not username.startswith("@"):
        return flask.abort(404)

    conn = get_db()

    posts = conn.execute(
        f"""
SELECT
  msg_id,
  media_group_id,
  message,
  lat,
  lng,
  created,
  updated,
  display_name,
  username,
  avatar_url
FROM posts
INNER JOIN users ON posts.user_id=users.telegram_id
WHERE deleted IS NULL {"AND users.username=?" if username else ""}
ORDER BY created DESC
    """,
        [username] if username else [],
    ).fetchall()

    group_ids = [p["media_group_id"] for p in posts]
    params = ",".join(["?"] * len(group_ids)) if group_ids else ""

    media = []
    if group_ids:
        media = conn.execute(
            f"""
SELECT
  msg_id,
  group_id,
  media_id,
  type,
  content_type,
  width,
  height
FROM media
WHERE deleted IS NULL AND group_id IN ({params})""",
            group_ids,
        ).fetchall()

    # Convert sqlite3.Row to dict:
    posts = [dict(p) for p in posts]

    for post in posts:
        post["media"] = [m for m in media if m["group_id"] == post["media_group_id"]]

    return render_template("index.html", posts=posts)


@app.get("/map")
def get_map_data(username: str | None = None):
    """Return check-ins as a GeoJSON FeatureCollection"""

    conn = get_db()

    posts = conn.execute(
        f"""
SELECT
  msg_id,
  media_group_id,
  message,
  lat,
  lng,
  created,
  updated,
  display_name,
  username,
  avatar_url
FROM posts
INNER JOIN users ON posts.user_id=users.telegram_id
WHERE deleted IS NULL
  AND lat IS NOT NULL
  AND lng IS NOT NULL
  {"AND users.username=?" if username else ""}
ORDER BY created ASC
    """,
        [username] if username else [],
    ).fetchall()

    # Get media for posts with location data
    if posts:
        group_ids = [p["media_group_id"] for p in posts]
        params = ",".join(["?"] * len(group_ids))

        media = conn.execute(
            f"""
SELECT
  msg_id,
  group_id,
  media_id,
  type,
  content_type,
  width,
  height
FROM media
WHERE deleted IS NULL AND group_id IN ({params})""",
            group_ids,
        ).fetchall()

        # Convert to dict and attach media
        posts_dict = [dict(p) for p in posts]
        for post in posts_dict:
            post["media"] = [dict(m) for m in media if m["group_id"] == post["media_group_id"]]
    else:
        posts_dict = []

    features = []
    for post in posts_dict:
        feature = {
            "type": "Feature",
            "geometry": {
                "type": "Point",
                "coordinates": [post["lng"], post["lat"]]
            },
            "properties": {
                "msg_id": post["msg_id"],
                "media_group_id": post["media_group_id"],
                "message": post["message"],
                "created": post["created"],
                "updated": post["updated"],
                "display_name": post["display_name"],
                "username": post["username"],
                "avatar_url": post["avatar_url"],
                "media": post["media"],
                "has_media": len(post["media"]) > 0
            }
        }
        features.append(feature)

    geojson = {
        "type": "FeatureCollection",
        "features": features
    }

    return render_template("map.html", geojson=json.dumps(geojson))


@app.get("/media/<media_id>")
def view_media(media_id):
    conn = get_db()

    cur = conn.execute(
        """
        SELECT media_id, content_type, file_path
        FROM media WHERE media_id=?
        """,
        [media_id],
    )
    media = cur.fetchone()
    if not media or not media["file_path"]:
        return "not found", 404

    # TODO: If media['file_path'] is none then migrate the blob to disk and update the file_path
    media_storage_path = get_media_storage_path()
    file_path = media_storage_path / media["file_path"]

    if not file_path.exists():
        return "file not found", 404

    return flask.send_file(
        file_path,
        mimetype=media["content_type"],
        max_age=60 * 60 * 24 * 365,
    )


SCHEMA = """
CREATE TABLE IF NOT EXISTS users (
    telegram_id    TEXT PRIMARY KEY,
    username       TEXT NOT NULL UNIQUE,
    display_name   TEXT NOT NULL,
    avatar_url     TEXT
);

CREATE TABLE IF NOT EXISTS posts (
    id             INTEGER PRIMARY KEY AUTOINCREMENT,
    msg_id         TEXT UNIQUE NOT NULL,
    media_group_id TEXT REFERENCES media(group_id) ON DELETE CASCADE,
    user_id        TEXT REFERENCES users(telegram_id) ON DELETE CASCADE,

    message  TEXT,
    lat      REAL,
    lng      REAL,

    created  TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated  TIMESTAMP DEFAULT NULL,
    deleted  TIMESTAMP DEFAULT NULL
);

CREATE TABLE IF NOT EXISTS media (
    id           INTEGER PRIMARY KEY AUTOINCREMENT,
    msg_id       TEXT REFERENCES posts(msg_id) ON DELETE CASCADE,
    media_id     TEXT UNIQUE NOT NULL,
    group_id     TEXT NOT NULL,

    type         TEXT CHECK(type IN ('photo', 'video', 'audio')),

    content_type TEXT,
    content_size INTEGER,
    content      BLOB,

    caption TEXT,
    width   INTEGER,
    height  INTEGER,

    created      TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated      TIMESTAMP DEFAULT NULL,
    deleted      TIMESTAMP DEFAULT NULL
);

INSERT OR REPLACE INTO users (telegram_id, username, display_name, avatar_url)
VALUES ('6525743351', '@susu', 'susu', 'https://user-images.githubusercontent.com/188935/259557976-0e622245-5970-4199-8a9f-ff20e3115043.png')
     , ('1031477684', '@erik', 'erik', 'https://avatars.githubusercontent.com/u/188935?v=4');
"""

def migrate_blobs_to_filesystem(conn):
    media_storage_path = get_media_storage_path()

    cursor = conn.cursor()
    cursor.execute("SELECT id, media_id, content_type, content FROM media")

    while True:
        row = cursor.fetchone()
        if row is None:
            break

        file_path = generate_file_path(row['media_id'], row['content_type'])
        print(f"Moving blob {row['id']} to {file_path}")
        with open(media_storage_path / file_path, 'wb') as f:
            f.write(row['content'])

        conn.execute("UPDATE media SET file_path=? WHERE id=?", (file_path, row['id']))
    cursor.close()


MIGRATIONS = [
    ("""
    CREATE TABLE IF NOT EXISTS migrations (
        id           INTEGER PRIMARY KEY AUTOINCREMENT,
        applied_at   TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    );
    """, None),

    ("ALTER TABLE media ADD COLUMN file_path TEXT;", migrate_blobs_to_filesystem)
]


def apply_migrations(conn):
    try:
        max_migration_id = conn.execute("SELECT MAX(id) FROM migrations").fetchone()[0]
    except:
        max_migration_id = 0

    for (migration, migration_func) in MIGRATIONS[max_migration_id:]:
        print(f"Applying migration {migration}")
        conn.executescript(migration)
        if migration_func:
            migration_func(conn)
        conn.execute("INSERT INTO migrations (id) VALUES (?)", (len(MIGRATIONS),))

    conn.commit()


def init_db():
    print("initializing db")
    conn = get_db()
    conn.executescript(SCHEMA)
    conn.commit()

    apply_migrations(conn)
    print("db initialized")


def generate_file_path(media_id: str, content_type: str) -> str:
    """Generate a filesystem path for storing media"""
    # Get file extension from content type
    ext_map = {
        "image/jpeg": ".jpg",
        "image/png": ".png",
        "image/gif": ".gif",
        "image/webp": ".webp",
        "video/mp4": ".mp4",
        "video/webm": ".webm",
        "video/quicktime": ".mov",
        "audio/ogg": ".ogg",
        "audio/mpeg": ".mp3",
        "audio/wav": ".wav",
    }

    ext = ext_map.get(content_type, "")
    return f"{media_id}{ext}"


async def on_start(update: Update, _context: CallbackContext) -> None:
    """Send a message when the command /ping is issued."""
    await update.message.reply_text("pong!")


async def dispatch_new_message(update: Update, ctx: CallbackContext) -> None:
    print(f"dispatching message: {update}")
    if not get_user_id(update):
        print("ignoring unknown user")

    if update.effective_message.text or update.effective_message.caption:
        await handle_text_message(update, ctx)

    if update.effective_message.effective_attachment:
        await handle_media(update, ctx)


async def dispatch_reply_message(update: Update, _context: CallbackContext) -> None:
    print(f"dispatching reply message: {update}")
    msg = update.effective_message
    conn = get_db()
    if not get_user_id(update):
        print("ignoring unknown user")

    if msg.location:
        print(f"Handling location: {update}")
        return await handle_location_message(update, _context)

    text = msg.text or msg.caption

    if text.lower() == "delete":
        print(f"Deleting message: {update, text}")
        conn.execute(
            """
            UPDATE posts SET deleted=CURRENT_TIMESTAMP WHERE msg_id=?
            """,
            [get_post_id(update, msg.reply_to_message.id)],
        )
        conn.commit()
        await ack_message(msg)


def get_post_id(update: Update, msg_id: int = None) -> str:
    msg = update.effective_message
    return f"{msg.chat_id}:{msg_id or msg.id}"


def get_user_id(update: Update) -> str | None:
    conn = get_db()
    user = conn.execute(
        "SELECT telegram_id FROM users WHERE telegram_id=?",
        [update.effective_message.from_user.id],
    ).fetchone()

    return user[0] if user else None


async def ack_message(msg: telegram.Message):
    await msg.set_reaction(reaction=[telegram.ReactionTypeEmoji(telegram.constants.ReactionEmoji.SQUARED_COOL)])


async def handle_text_message(update: Update, context: CallbackContext) -> None:
    conn = get_db()
    msg = update.effective_message
    print(f"Handling text: {msg.caption or msg.text}. Have media? {msg.media_group_id}")
    msg_id = get_post_id(update)
    conn.execute(
        """
        INSERT INTO posts(msg_id, user_id, media_group_id, message) VALUES(?, ?, ?, ?)
        ON CONFLICT(msg_id) DO UPDATE
            SET message=COALESCE(excluded.message, message),
                media_group_id=COALESCE(excluded.media_group_id, media_group_id),
                updated=CURRENT_TIMESTAMP
        ;
        """,
        (
            msg_id,
            get_user_id(update),
            msg.media_group_id or msg_id,
            msg.caption or msg.text or "No description.",
        ),
    )
    conn.commit()
    await ack_message(msg)


async def handle_location_message(update: Update, context: CallbackContext) -> None:
    conn = get_db()
    msg = update.effective_message
    location = msg.location
    msg_id = (
        msg.reply_to_message.id if msg.reply_to_message else update.effective_message.id
    )

    conn.execute(
        "UPDATE posts SET lat=?, lng=?, updated=CURRENT_TIMESTAMP WHERE msg_id=?;",
        (
            location.latitude,
            location.longitude,
            get_post_id(update, msg_id),
        ),
    )
    conn.commit()

    await ack_message(msg)


async def handle_media(update: Update, context: CallbackContext) -> None:
    conn = get_db()
    msg = update.effective_message
    att = update.effective_message.effective_attachment
    print(f"Handling media: {att}")

    # Images are sent as a list of sizes, we only want the largest
    if isinstance(att, tuple):
        att = sorted(att, key=lambda a: a.width)[-1]
        kind = "photo"
    elif isinstance(att, Voice):
        kind = "audio"
    elif isinstance(att, Video) or isinstance(att, VideoNote):
        kind = "video"
    else:
        print(f"Unknown attachment type: {att}")
        return

    # Need to do this or we won't get the upload when the caption is missing.
    if kind in {"video", "audio"}:
        await handle_text_message(update, context)

    msg_id = get_post_id(update)
    unique_id = att.file_unique_id
    group_id = msg.media_group_id or msg_id

    # Check if we've already seen this file
    cur = conn.execute("SELECT 1 FROM media WHERE media_id=?", (unique_id,))
    if cur.fetchone():
        print(f"File already exists: {unique_id}")
        return

    # Get content type
    content_type = getattr(
        att,
        "mime_type",
        {
            "photo": "image/jpeg",
            "video": "video/mp4",
            "audio": "audio/ogg",
        }[kind],
    )

    file_path = generate_file_path(unique_id, content_type)
    media_storage_path = get_media_storage_path()
    file = await update.get_bot().get_file(att.file_id)
    await file.download_to_drive(str(media_storage_path / file_path))

    conn.execute(
        """
        INSERT INTO media(
            media_id,
            msg_id,
            group_id,
            type,
            content_type,
            content_size,
            file_path,
            width,
            height
        )
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
        """,
        (
            unique_id,
            msg_id,
            group_id,
            kind,
            content_type,
            att.file_size,
            file_path,
            att.width if kind == "photo" else None,
            att.height if kind == "photo" else None,
        ),
    )

    conn.commit()
    print(f"Downloaded file: {unique_id} -> {file_path}")
    await ack_message(msg)


def application():
    """Application factory for uWSGI"""
    load_dotenv()
    init_db()

    return app


def run_debug_server():
    app = application()
    app.config["TEMPLATES_AUTO_RELOAD"] = True
    app.run(
        host="0.0.0.0",
        port=8080,
        debug=True,
    )


def run_telegram_bot():
    """Create Telegram bot"""
    load_dotenv()
    init_db()
    bot = ApplicationBuilder().token(os.environ["TG_BOT_TOKEN"]).build()
    bot.add_handler(CommandHandler("start", on_start))
    bot.add_handler(CommandHandler("delete", on_start))
    bot.add_handler(
        MessageHandler(filters.ALL & ~filters.REPLY, dispatch_new_message)
    )
    bot.add_handler(
        MessageHandler(filters.ALL & filters.REPLY, dispatch_reply_message)
    )

    bot.run_polling(allowed_updates=Update.ALL_TYPES)


if __name__ == "__main__":
    if sys.argv[1:] == ["bot"]:
        run_telegram_bot()

    elif sys.argv[1:] == ["server"]:
        run_debug_server()

    else:
        print("Usage: python bot.py [bot|server]")
        sys.exit(1)
