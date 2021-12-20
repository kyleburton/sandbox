BEGIN;

CREATE SCHEMA IF NOT EXISTS krb_utils;

CREATE OR REPLACE FUNCTION krb_utils.track_updated_at()
  RETURNS TRIGGER AS
$$
-- DELCARE
BEGIN
  IF TG_OP = 'UPDATE' THEN
    NEW.updated_at := now();
  ELSIF TG_OP = 'INSERT' THEN
    NEW.updated_at := now();
  END IF;

  RETURN NEW;
END
$$
LANGUAGE 'plpgsql' -- IMMUTABLE -- NB: not sure what IMMUTABLE means .. probably don't want it for installing the stored proc
SECURITY DEFINER;

CREATE OR REPLACE FUNCTION krb_utils.install_updated_at_tracker(sname varchar, tname varchar, colname varchar)
  RETURNS void AS
$$
DECLARE
  stmt text;
  has_colname boolean;
BEGIN
  EXECUTE 'SELECT true FROM information_schema.columns WHERE table_schema=''' || sname || ''''
          || ' AND table_name=''' || tname || ''''
          || ' AND column_name=''' || colname || ''''
        ';'
    INTO has_colname;

  IF has_colname IS NULL THEN
    RAISE EXCEPTION 'krb_utils.install_updated_at_tracker: Error: %.% has no ''%'' column, unable to install trigger',
      sname, tname, colname;
  END IF;

  -- RAISE NOTICE 'krb_utils.install_updated_at_tracker has_colname=%',
  --   has_colname;

  stmt := 'DROP TRIGGER IF EXISTS ' || tname || '_updt ON ' || sname || '.' || tname || ';';
  EXECUTE stmt;

  stmt := 'CREATE TRIGGER ' || tname || '_updt BEFORE INSERT OR UPDATE ON ' || sname || '.' || tname ||
    ' FOR EACH ROW EXECUTE PROCEDURE krb_utils.track_updated_at()';
  EXECUTE stmt;

  RAISE NOTICE 'krb_utils.install_updated_at_tracker installed trigger on %.%(%)',
    sname, tname, colname;
END;
$$
LANGUAGE 'plpgsql' -- IMMUTABLE -- NB: not sure what IMMUTABLE means .. probably don't want it for installing the stored proc
SECURITY DEFINER;


CREATE OR REPLACE FUNCTION krb_utils.uninstall_updated_at_tracker(sname varchar, tname varchar, colname varchar)
  RETURNS void AS
$$
DECLARE
  stmt text;
  has_colname boolean;
BEGIN
  stmt := 'DROP TRIGGER IF EXISTS ' || tname || '_updt ON ' || sname || '.' || tname || ';';
  EXECUTE stmt;
END;
$$
LANGUAGE 'plpgsql' -- IMMUTABLE -- NB: not sure what IMMUTABLE means .. probably don't want it for installing the stored proc
SECURITY DEFINER;


-- TODO: implement a version of install_updated_at_tracker that defaults the colname to 'updated_at'::regclass

-- TODO: implement install_created_at_tracker()
-- TODO: implement a version of install_created_at_tracker that defaults the colname to 'updated_at'::regclass

COMMIT;
