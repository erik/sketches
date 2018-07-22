import functools

import flask

from web.models import User


def requires_login(f):
    @functools.wraps(f)
    def decorated(*args, **kwargs):
        if not flask.g.logged_in:
            flask.flash('Must be logged in for that!')
            return flask.redirect(flask.url_for('general.login'))

        return f(*args, **kwargs)
    return decorated


def lookup_request_user():
    user = None

    if 'uid' in flask.session:
        user = User.by_id(flask.session['uid'])

        if user is None:
            flask.session.pop('uid')

    flask.g.logged_in = user is not None
    flask.g.user = user
