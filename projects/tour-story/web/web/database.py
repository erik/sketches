import json

from flask_sqlalchemy import SQLAlchemy
from passlib import pwd
from passlib.hash import bcrypt


db = SQLAlchemy()


class User(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    api_key = db.Column(db.String(128), unique=True)
    email = db.Column(db.String(128), unique=True)
    name = db.Column(db.String(128))
    pw_hash = db.Column(db.String(128))

    @staticmethod
    def login(email, password):
        pw_hash = bcrypt.hash(password)
        return User.query.filter_by(email=email, pw_hash=pw_hash).first()

    @staticmethod
    def create(user_schema):
        password = user_schema.pop('password')

        # FIXME: real API key.
        user = User(**{
            'api_key': pwd.genword(),
            'pw_hash': bcrypt.hash(password),
            **user_schema
        })

        db.session.add(user)
        db.session.commit()
        return user


class Story(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    started_at = db.Column(db.DateTime)
    finished_at = db.Column(db.DateTime)
    updated_at = db.Column(db.DateTime)

    # Serialized JSON
    content = db.Column(db.String)

    author_id = db.Column(db.Integer, db.ForeignKey("user.id"))

    @staticmethod
    def create(user, story_schema):
        story_schema.update({
            'author_id': user.id,
            'content': json.dumps(story_schema['content'])
        })
        story = database.Story(**data)

        db.session.add(story)
        db.session.commit()
        return story


class Post(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    posted_at = db.Column(db.DateTime)

    # Serialized JSON
    content = db.Column(db.String)

    author_id = db.Column(db.Integer, db.ForeignKey("user.id"))
    story_id = db.Column(db.Integer, db.ForeignKey("story.id"))

    @staticmethod
    def create(story_id, user, post_schema):
        post = database.Post({
            'author_id': user.id,
            'story_id': story_id,
            'posted_at': datetime.utcnow(),
            'content': json.dumps(post_schema),
            **data
        })

        db.session.add(post)
        db.session.commit()

        return post


class Image(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    posted_at = db.Column(db.DateTime)
    url = db.Column(db.String)
    title = db.Column(db.String)
    caption = db.Column(db.String)

    author_id = db.Column(db.Integer, db.ForeignKey("user.id"))


def init_app(app):
    db.app = app

    db.init_app(app)

    # For development, nuke the database:
    # db.drop_all()

    db.create_all()

    return app
