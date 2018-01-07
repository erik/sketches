from datetime import datetime
import functools
import json

import flask

import database
import schema


view = flask.Blueprint('api', __name__)


def require_api_auth(f):
    @functools.wraps(f)
    def wrapper(*args, **kwargs):
        key = flask.request.args.get('api_key')
        user = key and database.User.query.filter_by(api_key=key).first()

        if not user:
            flask.abort(401)

        kwargs['user'] = user

        return f(*args, **kwargs)
    return wrapper


def require_schema(schema):
    def wrapper(f):
        @functools.wraps(f)
        def inner(*args, **kwargs):
            blob = flask.request.get_json(force=True)
            data, errors = schema.load(blob)

            if errors or data is None:
                return flask.jsonify({'errors': errors}), 422

            kwargs['data'] = data
            return f(*args, **kwargs)
        return inner
    return wrapper


# /user
@view.route('/api/user', methods=['POST'])
@require_schema(schema.user_schema)
def POST_user(data):
    user = database.User.create(data)
    return flask.jsonify({
        'id': user.id,
        'api_key': user.api_key,
    })


# /story/
@view.route('/api/story', methods=['POST'])
@require_api_auth
@require_schema(schema.story_schema)
def POST_story(user, data):
    story = database.Story.create(user, data)
    return flask.jsonify({
        'id': story.id
    })


@view.route('/api/story/<story_id>', methods=['GET'])
def GET_story(story_id):
    story = database.Story.query.get(story_id)
    if story is None:
        return flask.jsonify({'error': 'unknown story_id'}), 404

    return schema.story_schema.dumps(story)


@view.route('/api/story/<story_id>', methods=['PUT'])
@require_api_auth
@require_schema(schema.story_schema)
def PUT_story(user, story_id):
    pass


# /post/
@view.route('/api/story/<story_id>/post', methods=['POST'])
@require_api_auth
@require_schema(schema.post_content_schema)
def POST_post(story_id, user, data):
    post = database.Post.create(story_id, user, data)
    return flask.jsonify({
        'id': post.id
    })


@view.route('/api/story/<story_id>/post/<post_id>', methods=['GET'])
def GET_post(_story_id, post_id):
    post = database.Post.query.get(post_id)
    if post is None:
        return flask.jsonify({'error': 'unknown post_id'}), 404

    return schema.post_schema(post)


@view.route('/api/story/<story_id>/post/<post_id>', methods=['PUT'])
@require_api_auth
@require_schema(schema.post_schema)
def PUT_post(_story_id, post_id, data):
    pass
