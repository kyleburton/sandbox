#!/usr/bin/env
set -eu

set -x

function try_install_trigger () {
bake postgres psql <<END
\set ON_ERROR_STOP on
BEGIN;
  SELECT krb_utils.install_created_at_tracker(
  'krb_test',
  'users',
  'updated_at'
  );
COMMIT;
END
}

try_install_trigger
