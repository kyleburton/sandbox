#!/usr/bin/env bash 

SIMPLEMIGRATE_CONFIG="${SIMPLEMIGRATE_CONFIG:-}"

function simplemigrate_config () {
  if [ -n "$SIMPLEMIGRATE_CONFIG" ]; then
    echo "$SIMPLEMIGRATE_CONFIG"
    return 0
  fi

  if [ -e local.env ]; then
    echo "local.env"
    return 0
  fi

  return 1
}

source "$(simplemigrate_config)"

bake_task dbshell
function dbshell () {
  local dbname="${1:-}"

  if [ -z "$dbname" ]; then
    dbname="$DBNAME"
  fi

  if [ -n "$dbname" ]; then
    mysql --password="$DBPASS" -u "$DBUSER" -h "$DBHOST" "$dbname"
  else
    # no dbname provided
    mysql --password="$DBPASS" -u "$DBUSER" -h "$DBHOST"
  fi
}

bake_task createdb
function createdb () {
  local dbname="$1"
  mysql --password="$DBPASS" -u "$DBUSER" -h "$DBHOST" mysql -e "CREATE DATABASE IF NOT EXISTS $dbname"
}

bake_task exec_sql
function exec_sql () {
  local sqlfile="${1:-}"
  if [ -z "$sqlfile" ]; then
    mysql --password="$DBPASS" -u "$DBUSER" -h "$DBHOST" "$DBNAME"
  else
    mysql --password="$DBPASS" -u "$DBUSER" -h "$DBHOST" "$DBNAME" < $sqlfile
  fi
}

bake_task bootstrap
function bootstrap () {
 createdb "$DBNAME"
 exec_sql << END
CREATE TABLE IF NOT EXISTS simplemigrate (
  created_at TIMESTAMP    DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  revision   VARCHAR(255) NOT NULL,
  PRIMARY KEY (revision)
)
END
}

function applied_migrations () {
  echo "select revision from simplemigrate order by revision desc" | exec_sql
}

bake_task show_tables
function show_tables () {
  echo "show tables" | exec_sql
}

bake_task pending
function pending () {
  bake_echo_red "implement this: show pending migrations"
}

bake_task up
function up () {
  bake_echo_red "implement this: upgrade one revision"
}

bake_task down
function down () {
  bake_echo_red "implement this: downgrade one revision"
}

bake_task migrate
function migrate () {
  bake_echo_red "implement this: apply all unmigrated migrations"
}

bake_task status
function status () {
  for migration in $(applied_migrations); do
    echo "applied: $migration"
  done
}


bake_task new
function new () {
  raw_name="$@"
  migration_name="$(echo "$raw_name" | tr -c '[[:alnum:]]' _ | sed 's/__*/_/' | sed 's/_$//')"
  echo "raw_name='$raw_name'"
  echo "migration_name='$migration_name'"
}

