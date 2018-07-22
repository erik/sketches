import json

import flask
from flask import request

from web.models import Location, Status
from web.views import requires_login, lookup_request_user


mod = flask.Blueprint('status', __name__)
mod.before_request(lookup_request_user)


@mod.route('/create', methods=['POST'])
@requires_login
def create():
    # TODO: real form parsing
    location = request.form['location']
    if location != '':
        location = Location(**json.loads(location))
    else:
        location = None

    Status.create(
        title=request.form['title'],
        body=request.form['body'],
        location=location,
        user_id=flask.g.user.id,
        trip_id=0)

    return flask.redirect(flask.url_for('general.index'))


@mod.route('/<int:id>/edit', methods=['POST'])
def edit():
    pass
