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
def submit():
    return 'hello, this is a flask app!'

if __name__ == '__main__':
    app.run()
