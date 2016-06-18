http://www.chesnok.com/daily/2013/07/02/a-practical-guide-to-using-alembic/

```
$ alemibc init alembic
$ docker-compose -f dev-docker-compose.yml up -d
$ docker-compose -f dev-docker-compose.yml down
$ bake migration-new create table users
```
