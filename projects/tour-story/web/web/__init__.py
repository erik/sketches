import os
import json

import flask

import database
from config import config
from views import register_routes


def load_config():
    env = os.getenv('ENV', 'dev')

    with open('config/{}.json'.format(env)) as f:
        parsed = json.load(f)
        config.update(parsed)

    print('running with config: %s' % config)


def create_app():
    app = flask.Flask(__name__)

    app.config.update({
        'SQLALCHEMY_DATABASE_URI': config['db_url'],
        'SQLALCHEMY_TRACK_MODIFICATIONS': True,
        'SECRET_KEY': config['flask_secret'],
    })

    database.init_app(app)
    register_routes(app)

    return app


if __name__ == '__main__':
    load_config()

    app = create_app()
    app.run(debug=True)
