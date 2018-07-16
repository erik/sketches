from datetime import datetime as dt

from flask_sqlalchemy import SQLAlchemy
from werkzeug.security import generate_password_hash, \
    check_password_hash


db = SQLAlchemy()


class User(db.Model):
    __tablename__ = 'users'

    id = db.Column(db.Integer, primary_key=True)

    name = db.Column(db.String())
    password_hash = db.Column(db.String(128))

    created_at = db.Column(db.DateTime, default=dt.utcnow)
    deleted_at = db.Column(db.DateTime())

    @staticmethod
    def create(name, password):
        hash = generate_password_hash(password)

        user = User(name=name, password_hash=hash)

        db.session.add(user)
        db.session.commit()

        return user

    @staticmethod
    def by_name_and_password(name, password):
        user = User.query.filter_by(name=name).first()

        if user and check_password_hash(
                user.password_hash, password):
            return user

    @staticmethod
    def by_id(user_id):
        return User.query.get(user_id)


class Trip(db.Model):
    __tablename__ = 'trips'

    id = db.Column(db.Integer, primary_key=True)

    title = db.Column(db.String)
    description = db.Column(db.String)

    created_at = db.Column(db.DateTime, default=dt.utcnow)
    updated_at = db.Column(db.DateTime())
    ended_at = db.Column(db.DateTime())

    user_id = db.Column(db.Integer, db.ForeignKey('users.id'))

    @staticmethod
    def create(title, description, user_id):
        trip = Trip(title=title, description=description, user_id=user_id)

        db.session.add(trip)
        db.session.commit()

        return trip

    @staticmethod
    def by_user_id(user_id, page=0, per_page=100):
        return Trip                     \
            .query                      \
            .filter_by(user_id=user_id) \
            .pagination(page, per_page) \
            .all()


class Status(db.Model):
    __tablename__ = 'statuses'

    id = db.Column(db.Integer, primary_key=True)

    title = db.Column(db.String())
    body = db.Column(db.String())

    created_at = db.Column(db.DateTime, default=dt.utcnow)
    updated_at = db.Column(db.DateTime, default=dt.utcnow)
    deleted_at = db.Column(db.DateTime)

    user_id = db.Column(db.Integer, db.ForeignKey('users.id'))
    trip_id = db.Column(db.Integer, db.ForeignKey('trips.id'))

    @staticmethod
    def create(title, body, user_id, trip_id):
        status = Status(title=title, body=body, user_id=user_id,
                        trip_id=trip_id)

        db.session.add(status)
        db.session.commit()

        return status

    @staticmethod
    def by_trip_id(trip_id, page=0, per_page=100):
        return Status                                \
            .query                                   \
            .filter_by(trip_id=trip_id)              \
            .pagination(page, per_page)              \
            .all()