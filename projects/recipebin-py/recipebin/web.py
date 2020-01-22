import flask
from flask_sqlalchemy import SQLAlchemy

from . import extractor


api = flask.Blueprint("api", __name__)
web = flask.Blueprint("web", __name__)


def create_app() -> flask.Flask:
    app = flask.Flask(__name__)

    app.register_blueprint(web)
    app.register_blueprint(api, url_prefix="/api/")

    return app


@web.route("/", methods=["GET"])
def index() -> str:
    return "Hello, world."


@api.route("/recipe/scrape", methods=["POST"])
def trigger_scrape():
    pass
