# all the imports
import sqlite3, os, subprocess, logging
from flask import Flask, request, session, g, redirect, url_for, \
    abort, render_template, flash
from contextlib import closing
from logging.handlers import RotatingFileHandler
from logging import Formatter

# configuration
DATABASE   = 'db/flaskr.db'
DEBUG      = True
SECRET_KEY = """2a(c=$n6"/'-kp/:`'n_;l<en"""
USERNAME   = 'admin'
PASSWORD   = 'banana17'
LOGGING    = {
    'filename': 'logs/flaskr.log'
}

# create our little application :)
app = Flask(__name__)
app.config.from_object(__name__)
# Allow FLASKR_SETTINGS to override defaults
app.config.from_envvar('FLASKR_SETTINGS', silent=True)

def ensure_dir_for_file(fname):
  dname = os.path.dirname(fname)
  if not os.path.isdir(dname):
    os.mkdir(dname)

ensure_dir_for_file(app.config['DATABASE'])

def init_db():
  with closing(connect_db()) as db:
    with app.open_resource('schema.sql', mode='r') as f:
      db.cursor().executescript(f.read())
      db.commit()

def connect_db():
  return sqlite3.connect(app.config['DATABASE'])

# # init the db
if not os.path.isfile(DATABASE):
  init_db()

def init_logging():
  ensure_dir_for_file(app.config['LOGGING']['filename'])
  logging_handler = RotatingFileHandler(app.config['LOGGING']['filename'], maxBytes=1024*1024*10, backupCount=5)
  logging_handler.setLevel(logging.INFO)
  if app.config['DEBUG']:
    logging_handler.setLevel(logging.DEBUG)
  app.logger.addHandler(logging_handler)
  logging_handler.setFormatter(Formatter(
    '%(asctime)s %(levelname)s: %(pathname)s:%(lineno)d: %(message)s '
  ))
  app.logger.info("logging initialized")

@app.before_request
def before_request():
    g.db = connect_db()

@app.teardown_request
def teardown_request(exception):
    db = getattr(g, 'db', None)
    if db is not None:
        db.close()

@app.route('/', methods=['GET'])
def show_entries():
  cursor = g.db.execute('SELECT title, text FROM entries ORDER BY id DESC LIMIT 10')
  entries = [dict(title=row[0], text=row[1]) for row in cursor.fetchall()]
  return render_template('show_entries.html', entries=entries)

@app.route('/add', methods=['POST'])
def add_entry():
  if not session.get('logged_in'):
    abort(401)
  g.db.execute('insert into entries (title, text) values (?, ?)',
               [request.form['title'], request.form['text']])
  g.db.commit()
  flash('New entry was successfully posted')
  return redirect(url_for('show_entries'))


@app.route('/login', methods=['GET', 'POST'])
def login():
    error = None
    if request.method == 'POST':
        if request.form['username'] != app.config['USERNAME']:
            error = 'Invalid username'
        elif request.form['password'] != app.config['PASSWORD']:
            error = 'Invalid password'
        else:
            session['logged_in'] = True
            flash('You were logged in')
            return redirect(url_for('show_entries'))
    # on GET, render the form
    return render_template('login.html', error=error)

@app.route('/logout', methods=['GET','POST','DELETE'])
def logout():
    session.pop('logged_in', None)
    flash('You were logged out')
    return redirect(url_for('show_entries'))

if __name__ == '__main__':
  init_logging()
  app.run()
