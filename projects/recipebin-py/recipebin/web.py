from functools import wraps

from flask_sqlalchemy import SQLAlchemy
from werkzeug.exceptions import BadRequest
import flask
import requests

from . import extractor


api = flask.Blueprint("api", __name__)
web = flask.Blueprint("web", __name__)


def create_app() -> flask.Flask:
    app = flask.Flask(__name__)

    app.register_blueprint(web)
    app.register_blueprint(api, url_prefix="/api/")

    return app


def validate_json(fn):
    @wraps(fn)
    def wrapper(*args, **kwargs):
        try:
            if not flask.request.get_json(force=True):
                raise BadRequest("asdf")
            return fn(*args, **kwargs)
        except BadRequest:
            return flask.jsonify({"error": "must be valid json"}), 400

    return wrapper


class OptimisiticDict(dict):
    def __getitem__(self, k):
        if k not in self:
            flask.abort(400)
        return super().__getitem__(k)


@web.route("/", methods=["GET"])
def index() -> str:
    return "Hello, world."


@api.route("/recipe/scrape", methods=["POST"])
@validate_json
def trigger_scrape():
    obj = OptimisiticDict(flask.request.get_json(force=True))

    req = requests.get(obj['url'])
    recipes = extractor.extract_recipes(req.text)
    return flask.jsonify({'recipes': recipes})
