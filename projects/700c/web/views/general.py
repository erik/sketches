import flask

from web.models import User, Status, Trip
from web.views.util import requires_login, lookup_request_user


mod = flask.Blueprint('general', __name__)

mod.before_request(lookup_request_user)


@mod.route('/', methods=['GET'])
@requires_login
def index():
    active_trip = Trip.get_active(flask.g.user.id)

    if active_trip is None:
        return flask.redirect(flask.url_for('trip.index'))

    return flask.render_template('views/timeline.html', **{
        'active_trip': active_trip,
        'statuses': Status.get_recent(flask.g.user.id)
    })


@mod.route('/login', methods=['POST'])
def try_login():
    form = flask.request.form
    user = User.by_name_and_password(form['user'], form['password'])

    if user is not None:
        flask.session['uid'] = user.id
        return flask.redirect('/')

    return flask.redirect('/login')


@mod.route('/login', methods=['GET'])
def login():
    return flask.render_template('views/login.html')


@mod.route('/logout', methods=['GET'])
def logout():
    flask.session.pop('uid')
    flask.flash('Logged out!')
    return flask.redirect(flask.url_for('general.login'))


@mod.route('/register', methods=['GET'])
def register():
    raise NotImplementedError("no")
