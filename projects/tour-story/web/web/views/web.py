import functools
import json

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
    query = database.Story.query \
                          .filter_by(author_id=user.id) \
                          .order_by(database.Story.updated_at.desc()) \
                          .all()

    stories = []
    for story in query:
        story.content = json.loads(story.content)
        stories.append(schema.story_schema.dump(story).data)

    return flask.render_template('index.html', user=user, stories=stories)


@view.route('/register', methods=['POST'])
def register():
    errors = schema.user_schema.validate(flask.request.form)

    if errors:
        return flask.jsonify({'errors': errors}), 422

    user = database.User.create(flask.request.form.to_dict())
    flask.session['uid'] = user.id

    return flask.redirect('/')


@view.route('/logout', methods=['GET'])
def logout():
    flask.session.pop('uid')
    return flask.redirect('/login')


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


@view.route('/story/<story_id>', methods=['GET'])
def view_story(story_id):
    query = database.Post.query \
                         .filter_by(story_id=story_id) \
                         .order_by(database.Post.posted_at.desc()) \
                         .all()
    posts = []
    for post in query:
        post.content = json.loads(post.content)
        posts.append(schema.post_schema.dump(post).data)

    return flask.render_template('view_story.html', posts=posts)
