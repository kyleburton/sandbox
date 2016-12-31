#!/usr/bin/env
set -eu
bake postgres psql <<END
BEGIN;

CREATE SCHEMA IF NOT EXISTS krb_test;

CREATE TABLE IF NOT EXISTS krb_test.missing_required_columns (
  id          bigserial primary key,
  uname       varchar(255),
  fname       varchar(255),
  lname       varchar(255)
);

CREATE TABLE IF NOT EXISTS krb_test.users (
  id          bigserial primary key,
  created_at  timestamp with time zone default now() NOT NULL,
  updated_at  timestamp with time zone default now() NOT NULL,
  uname       varchar(255),
  fname       varchar(255),
  lname       varchar(255)
);

COMMIT;
END
