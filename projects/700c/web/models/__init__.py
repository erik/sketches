from datetime import datetime as dt

from flask_sqlalchemy import SQLAlchemy, Model
from werkzeug.security import generate_password_hash, \
    check_password_hash


db = SQLAlchemy()


class CommonMixin(Model):
    '''Reduce boilerplate'''

    @classmethod
    def create(cls, **kwargs):
        inst = cls(**kwargs)

        db.session.add(inst)
        db.session.commit()

        return inst

    @classmethod
    def by_id(cls, pk_id):
        return cls.query.get(pk_id)


class User(CommonMixin, db.Model):
    __tablename__ = 'users'

    id = db.Column(db.Integer, primary_key=True)

    name = db.Column(db.String())
    password_hash = db.Column(db.String(128))

    created_at = db.Column(db.DateTime, default=dt.utcnow)
    deleted_at = db.Column(db.DateTime())

    @classmethod
    def create(cls, name, password):
        hash = generate_password_hash(password)
        return super().create(name=name, password_hash=hash)

    @staticmethod
    def by_name_and_password(name, password):
        user = User.query.filter_by(name=name).first()

        if user and check_password_hash(
                user.password_hash, password):
            return user


class Trip(CommonMixin, db.Model):
    __tablename__ = 'trips'

    id = db.Column(db.Integer, primary_key=True)

    title = db.Column(db.String)
    description = db.Column(db.String)

    created_at = db.Column(db.DateTime, default=dt.utcnow)
    updated_at = db.Column(db.DateTime())
    ended_at = db.Column(db.DateTime())

    user_id = db.Column(db.Integer, db.ForeignKey('users.id'))

    @classmethod
    def create(cls, title, description, user_id):
        return super().create(
            title=title, description=description, user_id=user_id)

    @classmethod
    def get_active(cls, user_id):
        return cls                                     \
            .query                                     \
            .filter_by(user_id=user_id, ended_at=None) \
            .first()

    @classmethod
    def by_user_id(cls, user_id, page=0, per_page=100):
        return cls                      \
            .query                      \
            .filter_by(user_id=user_id) \
            .pagination(page, per_page, error_out=False)


class Status(CommonMixin, db.Model):
    __tablename__ = 'statuses'

    id = db.Column(db.Integer, primary_key=True)

    title = db.Column(db.String())
    body = db.Column(db.String())

    created_at = db.Column(db.DateTime, default=dt.utcnow)
    updated_at = db.Column(db.DateTime, default=dt.utcnow)
    deleted_at = db.Column(db.DateTime)

    user_id = db.Column(db.Integer, db.ForeignKey('users.id'))
    trip_id = db.Column(db.Integer, db.ForeignKey('trips.id'))

    @classmethod
    def create(cls, title, body, user_id, trip_id):
        return super().create(
            title=title, body=body, user_id=user_id, trip_id=trip_id)

    @staticmethod
    def by_trip_id(trip_id, page=0, per_page=100):
        return Status                   \
            .query                      \
            .filter_by(trip_id=trip_id) \
            .paginate(page, per_page, error_out=False)

    @staticmethod
    def get_recent(user_id=None, page=0, per_page=100):
        query = Status                          \
            .query                              \
            .order_by(Status.created_at.desc()) \

        if user_id is not None:
            query = query.filter_by(user_id=user_id)

        return query.paginate(page, per_page, error_out=False)
