import flask

from web import views
from web.models import db, User


def setup_app():
    db.create_all()

    # FIXME: for dev only
    User.create('dev', 'dev')


def create_app(config_file):
    app = flask.Flask(__name__)

    app.config.from_pyfile(config_file)

    app.register_blueprint(views.html)
    app.register_blueprint(views.api, url_prefix='/api/')

    with app.app_context():
        db.init_app(app)
        setup_app()

    return app
