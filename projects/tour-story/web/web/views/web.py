import functools

import flask

import database
import schema


view = flask.Blueprint('web', __name__, template_folder='templates')


def require_auth(f):
    @functools.wraps(f)
    def wrapper(*args, **kwargs):
        cookie = flask.session.get('uid')
        user = cookie and database.User.query.get(cookie)

        if not user:
            return flask.redirect('/login')

        kwargs['user'] = user

        return f(*args, **kwargs)
    return wrapper


@view.route('/')
@require_auth
def index(user):
    return flask.render_template('index.html', user=user)


@view.route('/register', methods=['POST'])
def register():
    errors = schema.user_schema.validate(flask.request.form)

    if errors:
        return flask.jsonify({'errors': errors}), 422

    user = database.User.create(flask.request.form.to_dict())
    flask.session['uid'] = user.id

    return flask.redirect('/')


@view.route('/login', methods=['GET', 'POST'])
def login():
    if flask.request.method == 'GET':
        return flask.render_template('login.html')

    email = flask.request.form['email']
    pw = flask.request.form['password']
    user = database.User.login(email, pw)

    if not user:
        return flask.redirect('/login')

    flask.session['uid'] = user.id
    return flask.redirect('/')
