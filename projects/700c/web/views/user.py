import flask
from flask import request

from web.models import Trip, User
from web.views import requires_login, lookup_request_user


mod = flask.Blueprint('user', __name__)
mod.before_request(lookup_request_user)


@mod.route('/<int:id>', methods=['GET'])
def view(id):
    return flask.render_template('views/user.html', **{
        'user': User.by_id(id, raise_on_not_found=True),
        'trips': Trip.by_user_id(id)
    })


@mod.route('/<int:id>/edit', methods=['POST'])
def edit(id):
    raise NotImplementedError('no')
