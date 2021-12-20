# app.py

from flask import Flask
# from flask import request, render_template
# from flask.ext.sqlalchemy import SQLAlchemy
# from config import BaseConfig

app = Flask(__name__)
# app.config.from_object(BaseConfig)
# db = SQLAlchemy(app)

# from models import *


@app.route('/hello')
def hello_route():
    return 'hello, this is a flask app!'


# uncommenting this after the app is running will cause it to be reloaded
# @app.route('/goodbye')
# def goodbye_route():
#     return 'bye! this was a flask app!'

if __name__ == '__main__':
    app.run()
