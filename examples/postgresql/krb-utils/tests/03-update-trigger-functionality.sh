#!/usr/bin/env
set -eu

# set -x

function ensure-test-data () {
bake postgres psql <<END
SELECT krb_utils.uninstall_updated_at_tracker('krb_test', 'users', 'updated_at');
DELETE FROM krb_test.users WHERE uname='this';
INSERT INTO krb_test.users (uname) VALUES ('this');
END
}

function updated_at_is_not_tracked_before_install () {
  local preval="$(echo "SELECT updated_at FROM krb_test.users WHERE uname='this'" | bake postgres psql -t)"
  echo 'UPDATE krb_test.users SET id=id' | bake postgres psql
  local postval="$(echo "SELECT updated_at FROM krb_test.users WHERE uname='this'" | bake postgres psql -t)"
  if [ "$preval" != "$postval" ]; then
      echo "ERROR: before trigger installed, expected updated_at not to be tracked: $preval != $postval"
      return 1
  fi
}

function updated_at_is_tracked_after_install () {
  local preval="$(echo "SELECT updated_at FROM krb_test.users WHERE uname='this'" | bake postgres psql -t)"
  echo 'UPDATE krb_test.users SET id=id' | bake postgres psql
  local postval="$(echo "SELECT updated_at FROM krb_test.users WHERE uname='this'" | bake postgres psql -t)"
  if [ "$preval" == "$postval" ]; then
      echo "ERROR: after trigger installed, expected updated_at to be tracked: $preval == $postval"
      return 1
  fi
}

function try_install_trigger () {
bake postgres psql <<END
\set ON_ERROR_STOP on
BEGIN;
  SELECT krb_utils.install_updated_at_tracker(
  'krb_test',
  'users',
  'updated_at'
  );
COMMIT;
END
}

ensure-test-data
updated_at_is_not_tracked_before_install
try_install_trigger
updated_at_is_tracked_after_install
