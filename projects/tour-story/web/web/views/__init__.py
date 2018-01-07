from views import api
from views import web


def register_routes(app):
    app.register_blueprint(api.view)
    app.register_blueprint(web.view)
