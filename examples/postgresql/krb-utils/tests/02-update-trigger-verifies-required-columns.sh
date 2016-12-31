#!/usr/bin/env
set -eu

set -x

function try_install_trigger () {
bake postgres psql <<END
\set ON_ERROR_STOP on
BEGIN;
  SELECT krb_utils.install_updated_at_tracker(
  'krb_test',
  'missing_required_columns',
  'updated_at'
  );
END
}

if try_install_trigger; then
    # echo "$0: unexpected success"
    exit 1
else
    # echo "$0: expected failure"
    exit 0
fi

