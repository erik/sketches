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

    app.register_blueprint(views.blueprints.general)
    app.register_blueprint(views.blueprints.api, url_prefix='/api/')
    app.register_blueprint(views.blueprints.status, url_prefix='/status/')
    app.register_blueprint(views.blueprints.trip, url_prefix='/trip/')
    app.register_blueprint(views.blueprints.user, url_prefix='/user/')

    with app.app_context():
        db.init_app(app)
        setup_app()

    return app
