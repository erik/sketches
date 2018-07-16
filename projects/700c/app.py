from web import create_app


if __name__ == '__main__':
    app = create_app('../config/dev.cfg')
    app.run()
