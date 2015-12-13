import os, flaskr, unittest, tempfile, logging

class BaseTest(unittest.TestCase):
  def setUp(self):
    self.db_fd, flaskr.app.config['DATABASE'] = tempfile.mkstemp()
    flaskr.app.config['TESTING'] = True
    self.app = flaskr.app.test_client()
    flaskr.init_db()

  def tearDown(self):
    os.close(self.db_fd)
    os.unlink(flaskr.app.config['DATABASE'])


  def login(self, username, password):
    return self.app.post('/login', data=dict(
        username=username,
        password=password
    ), follow_redirects=True)
  
  def logout(self):
    #return self.app.get('/logout', follow_redirects=True)
    return self.app.delete('/logout', follow_redirects=True)

class LoggedInTest(BaseTest):
  def setUp(self):
    super(LoggedInTest, self).setUp()
    self.login(flaskr.app.config['USERNAME'], flaskr.app.config['PASSWORD'])

  def tearDown(self):
    self.logout()
    super(LoggedInTest, self).tearDown()

class FlaskrTestCase(BaseTest):

  def test_empty_db(self):
    rv = self.app.get('/')
    assert 'No entries here so far' in rv.data

  def test_login_logout(self):
    # rv = self.login('admin', 'default')
    rv = self.login(flaskr.app.config['USERNAME'], flaskr.app.config['PASSWORD'])
    assert 'You were logged in' in rv.data
    rv = self.logout()
    assert 'You were logged out' in rv.data
    rv = self.login('invaliduname', flaskr.app.config['PASSWORD'])
    assert 'Invalid username' in rv.data
    rv = self.login(flaskr.app.config['USERNAME'], 'invalidpass')
    assert 'Invalid password' in rv.data

class EntriesTest(LoggedInTest):
  def test_messages(self):
    rv = self.app.post('/add', data=dict(
        title='<Hello>',
        text='<strong>HTML</strong> allowed here'
    ), follow_redirects=True)
    assert 'No entries here so far' not in rv.data
    assert '&lt;Hello&gt;' in rv.data
    assert '<strong>HTML</strong> allowed here' in rv.data

if __name__ == '__main__':
  unittest.main()