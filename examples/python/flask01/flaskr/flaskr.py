# all the imports
import sqlite3, os, subprocess
from flask import Flask, request, session, g, redirect, url_for, \
    abort, render_template, flash

# configuration
DATABASE   = 'db/flaskr.db'
DEBUG      = True
SECRET_KEY = """2a(c=$n6"/'-kp/:`'n_;l<en"""
USERNAME   = 'admin'
PASSWORD   = 'default'

# create our little application :)
app = Flask(__name__)
# Allow FLASKR_SETTINGS to override defaults
app.config.from_envvar('FLASKR_SETTINGS', silent=True)




dbdir = os.path.dirname(DATABASE)
if not os.path.isdir(dbdir):
  os.mkdir(dbdir)

if not os.path.isfile(DATABASE):
  with open('schema.sql', 'r') as input_fh:
    p = subprocess.Popen( ['sqlite3', DATABASE], stdout=subprocess.PIPE, stdin=input_fh)
    p.wait()
    #output=p.stdout.read()

def connect_db():
  return sqlite3.connect(app.config['DATABASE'])

if __name__ == '__main__':
  app.run()
