import arrow
import flask

from web import views
from web.views import util
from web.models import db, User


def setup_app(app):
    db.init_app(app)
    db.create_all()

    app.template_filter('humanize_date')(util.humanize_date)

    # FIXME: for dev only
    User.create('dev', 'dev')


def create_app(config_file):
    app = flask.Flask(__name__)

    app.config.from_pyfile(config_file)

    app.register_blueprint(views.general)
    app.register_blueprint(views.api, url_prefix='/api/')
    app.register_blueprint(views.status, url_prefix='/status/')
    app.register_blueprint(views.trip, url_prefix='/trip/')
    app.register_blueprint(views.user, url_prefix='/user/')

    with app.app_context():
        setup_app(app)

    return app
