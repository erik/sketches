import io
import os
import sqlite3
import threading
import sys

from dotenv import load_dotenv
from flask import Flask, render_template
import flask
from telegram import Update, Video, VideoNote, Voice
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
    return thread_local._database


@app.get("/")
@app.get("/<username>")
def view_feed(username: str | None = None):
    conn = get_db()

    post_cols = [
        "msg_id",
        "media_group_id",
        "message",
        "lat",
        "lng",
        "created",
        "updated",
        "display_name",
        "username",
        "avatar_url",
    ]
    posts = [
        dict(zip(post_cols, row))
        for row in conn.execute(
            f"""
SELECT {", ".join(post_cols)}
FROM posts
INNER JOIN users ON posts.user_id=users.telegram_id
WHERE deleted IS NULL {"AND users.username=?" if username else ""}
ORDER BY created DESC
    """,
            [username] if username else [],
        ).fetchall()
    ]

    group_ids = [p["media_group_id"] for p in posts]
    params = ",".join(["?"] * len(group_ids))

    media_cols = [
        "msg_id",
        "group_id",
        "media_id",
        "type",
        "content_type",
        "width",
        "height",
    ]
    media = [
        dict(zip(media_cols, row))
        for row in conn.execute(
            f"""SELECT {", ".join(media_cols)} 
                FROM media
                WHERE deleted IS NULL AND group_id IN ({params})
            """,
            group_ids,
        ).fetchall()
    ]

    for post in posts:
        post["media"] = [m for m in media if m["group_id"] == post["media_group_id"]]

    return render_template("index.html", posts=posts)


@app.get("/media/<media_id>")
def view_media(media_id):
    conn = get_db()

    cur = conn.execute(
        """
        SELECT rowid, content_type
        FROM media WHERE media_id=?
        """,
        [media_id],
    )
    media = cur.fetchone()
    if not media:
        return "not found", 404

    row_number = media[0]
    with conn.blobopen("media", "content", row_number, readonly=True) as blob:
        # TODO: this should be done directly without reading into memory...
        f = io.BytesIO(blob.read())
        return flask.send_file(
            f,
            mimetype=media[1],
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


def init_db():
    print("initializing db")
    conn = get_db()
    conn.executescript(SCHEMA)
    conn.commit()
    print("db initialized")


async def on_start(update: Update, _context: CallbackContext) -> None:
    """Send a message when the command /ping is issued."""
    await update.message.reply_text("pong! (wtf)")


async def dispatch_new_message(update: Update, _context: CallbackContext) -> None:
    print(f"dispatching message: {update}")
    if not get_user_id(update):
        print("ignoring unknown user")

    if update.effective_message.text or update.effective_message.caption:
        await handle_text_message(update, _context)

    if update.effective_message.effective_attachment:
        await handle_media(update, _context)


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

    # Download the file
    file = await update.get_bot().get_file(att.file_id)
    cur = conn.execute(
        """
        INSERT INTO media(
            media_id, 
            msg_id, 
            group_id,
            type, 
            content_type, 
            content_size, 
            width,
            height,
            content
        )
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, zeroblob(?))
        """,
        (
            unique_id,
            msg_id,
            group_id,
            kind,
            # Not present for photos or video notes just guess...
            getattr(
                att,
                "mime_type",
                {
                    "photo": "image/jpeg",
                    "video": "video/mp4",
                    "audio": "audio/ogg",
                }[kind],
            ),
            att.file_size,
            att.width if kind == "photo" else None,
            att.height if kind == "photo" else None,
            att.file_size,
        ),
    )

    with conn.blobopen("media", "content", cur.lastrowid) as blob:
        await file.download_to_memory(blob)

    conn.commit()
    print(f"Downloaded file: {unique_id}")


if __name__ == "__main__":
    load_dotenv()
    init_db()

    if sys.argv[1:] == ["bot"]:
        print(f"Start bot creation: {os.environ['TG_BOT_TOKEN']}")
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

    elif sys.argv[1:] == ["server"]:
        print("Starting server...")

        app.config["TEMPLATES_AUTO_RELOAD"] = True
        app.run(
            host="0.0.0.0",
            port=8080,
        )

    else:
        print("Usage: python bot.py [bot|server]")
        sys.exit(1)
