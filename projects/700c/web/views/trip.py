import flask
from flask import request

from web.models import Status, Trip
from web.views.util import requires_login, lookup_request_user


mod = flask.Blueprint('trip', __name__)
mod.before_request(lookup_request_user)


@mod.route('/')
@requires_login
def index(methods=['GET']):
    return flask.render_template('views/create_trip.html')


@mod.route('/create', methods=['POST'])
@requires_login
def create():
    # TODO: real form parsing

    Trip.create(
        title=request.form['title'],
        description=request.form['description'],
        user_id=flask.g.user.id)

    return flask.redirect(flask.url_for('general.index'))


@mod.route('/<int:id>', methods=['GET'])
def view(id):
    trip = Trip.by_id(id, raise_on_not_found=True)
    return flask.render_template('views/trip.html', **{
        'trip': trip,
        'statuses': Status.by_trip_id(trip.id)
    })


@mod.route('/<int:id>/edit', methods=['POST'])
def edit(id):
    values = {
        k: request.form[k]
        for k in ['title', 'description']
        if k in request.form
    }

    Trip.update(where=(Trip.ended_at is None), values=values)

    return flask.redirect(flask.url_for('trip.view', id))
