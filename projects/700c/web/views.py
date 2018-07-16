import flask

from web.models import User


# User facing
html = flask.Blueprint('html', __name__)


@html.before_request
def lookup_request_user():
    user = None
    if 'uid' in flask.session:
        user = User.by_id(flask.session['uid'])

    flask.g.user = user


@html.route('/', methods=['GET'])
def index():
    return f'Logged in: {flask.g.user is not None}'


@html.route('/login', methods=['POST'])
def try_login():
    form = flask.request.form
    user = User.by_name_and_password(form['user'], form['password'])

    if user is not None:
        flask.session['uid'] = user.id
        return flask.redirect('/')

    return flask.redirect('/login')


@html.route('/login', methods=['GET'])
def login():
    return flask.render_template('login.html')


# JSON API
api = flask.Blueprint('api', __name__)
