CREATE SCHEMA rowcounts;

GRANT ALL ON SCHEMA rowcounts TO public;
COMMENT ON SCHEMA rowcounts IS 'Out-of-table row counting, tables and trigger functions';

CREATE TABLE rowcounts.counts (
  id bigserial   primary key,
  schema_name    text   not null,
  table_name     text   not null,
  num_rows       bigint not null default 0
);

GRANT ALL ON rowcounts.counts TO public;

COMMENT ON TABLE rowcounts.counts IS 'Row Counts by table, from rowcounts.track_rowcount_func()';

CREATE INDEX rowcounts_schema_name_table_name_idx ON rowcounts.counts (schema_name,table_name);


CREATE TABLE rowcounts.count_metrics (
  id bigserial   primary key,
  schema_name    text   not null,
  table_name     text   not null,
  period         text not null,
  num_inserts    bigint not null default 0,
  num_deletes    bigint not null default 0
);

GRANT ALL ON rowcounts.count_metrics TO public;

COMMENT ON TABLE rowcounts.count_metrics IS 'Row Counts by table, from rowcounts.track_rowcount_func()';

CREATE INDEX rowcount_metrics_schema_name_table_name_idx ON rowcounts.count_metrics (schema_name,table_name,period);

CREATE OR REPLACE FUNCTION rowcounts.track_rowcount_metrics_func(in_table_schema text, in_table_name text, in_period text, incrby integer) RETURNS void AS $body$
DECLARE
  _q_txt  text;
  _n_inserts integer;
  _n_deletes integer;
BEGIN
  IF incrby < 0 THEN -- an update
    _n_inserts = 0;
    _n_deletes = 1;
  ELSE               -- an insert
    _n_inserts = 1;
    _n_deletes = 0;
  END IF;

  LOOP
    UPDATE rowcounts.count_metrics 
       SET num_inserts = num_inserts + _n_inserts,
           num_deletes = num_deletes + _n_deletes
     WHERE schema_name = in_table_schema 
       AND table_name = in_table_name 
       AND period = in_period;
    IF found THEN
      RETURN;
    END IF;

    BEGIN
      INSERT INTO rowcounts.count_metrics 
             (schema_name,table_name,period,num_inserts, num_deletes)
      VALUES (in_table_schema,in_table_name,in_period,_n_inserts,_n_deletes);
      RETURN;
    EXCEPTION WHEN unique_violation THEN
      -- do nothing, make the update
    END;
  END LOOP;
END;
$body$
LANGUAGE plpgsql
SECURITY DEFINER
SET search_path = pg_catalog, public;

CREATE OR REPLACE FUNCTION rowcounts.track_rowcount_func() RETURNS TRIGGER AS $body$
DECLARE
  _q_txt  text;
  _m_year text;
  _m_mon  text;
  _m_day  text;
  _m_hour text;
  _m_min  text;
BEGIN
  _m_year = lpad(extract(year  from current_timestamp)::text, 4, '0');
  _m_mon  = lpad(extract(month from current_timestamp)::text, 2, '0');
  _m_day  = lpad(extract(day   from current_timestamp)::text, 2, '0');
  _m_hour = lpad(extract(hour  from current_timestamp)::text, 2, '0');
  _m_min  = lpad(extract(min   from current_timestamp)::text, 2, '0');

  IF TG_OP = 'INSERT' THEN
    _q_txt = 'UPDATE rowcounts.counts SET num_rows = num_rows+1 WHERE schema_name = $1 AND table_name = $2';
    EXECUTE _q_txt USING TG_TABLE_SCHEMA, TG_TABLE_NAME;
    PERFORM rowcounts.track_rowcount_metrics_func(TG_TABLE_SCHEMA, TG_TABLE_NAME, _m_year, 1);
    PERFORM rowcounts.track_rowcount_metrics_func(TG_TABLE_SCHEMA, TG_TABLE_NAME, _m_year || '.' || _m_mon, 1);
    PERFORM rowcounts.track_rowcount_metrics_func(TG_TABLE_SCHEMA, TG_TABLE_NAME, _m_year || '.' || _m_mon || '.' || _m_day, 1);
    PERFORM rowcounts.track_rowcount_metrics_func(TG_TABLE_SCHEMA, TG_TABLE_NAME, _m_year || '.' || _m_mon || '.' || _m_day || '.' || _m_hour, 1);
    PERFORM rowcounts.track_rowcount_metrics_func(TG_TABLE_SCHEMA, TG_TABLE_NAME, _m_year || '.' || _m_mon || '.' || _m_day || '.' || _m_hour || '.' || _m_min, 1);

    RETURN NEW;
  ELSIF TG_OP = 'DELETE' THEN
    _q_txt = 'UPDATE rowcounts.counts SET num_rows = num_rows-1 WHERE schema_name = $1 AND table_name = $2';
    EXECUTE _q_txt USING TG_TABLE_SCHEMA, TG_TABLE_NAME;
    PERFORM rowcounts.track_rowcount_metrics_func(TG_TABLE_SCHEMA, TG_TABLE_NAME, _m_year, -1);
    PERFORM rowcounts.track_rowcount_metrics_func(TG_TABLE_SCHEMA, TG_TABLE_NAME, _m_year || '.' || _m_mon, -1);
    PERFORM rowcounts.track_rowcount_metrics_func(TG_TABLE_SCHEMA, TG_TABLE_NAME, _m_year || '.' || _m_mon || '.' || _m_day, -1);
    PERFORM rowcounts.track_rowcount_metrics_func(TG_TABLE_SCHEMA, TG_TABLE_NAME, _m_year || '.' || _m_mon || '.' || _m_day || '.' || _m_hour, -1);
    PERFORM rowcounts.track_rowcount_metrics_func(TG_TABLE_SCHEMA, TG_TABLE_NAME, _m_year || '.' || _m_mon || '.' || _m_day || '.' || _m_hour || '.' || _m_min, -1);

    RETURN OLD;
  END IF;
END;
$body$
LANGUAGE plpgsql
SECURITY DEFINER
SET search_path = pg_catalog, public;

CREATE OR REPLACE FUNCTION rowcounts.track_rowcount_on_truncate_func() RETURNS TRIGGER AS $body$
DECLARE
BEGIN
    PERFORM 'UPDATE rowcounts.counts SET num_rows = 0 WHERE schema_name = $1 AND table_name = $2',
      TG_TABLE_SCHEMA, TG_TABLE_NAME;
    RETURN NULL;
END;
$body$
LANGUAGE plpgsql
SECURITY DEFINER
SET search_path = pg_catalog, public;


CREATE OR REPLACE FUNCTION rowcounts.track_rowcount_on(in_table_schema text, in_table_name text) RETURNS void AS $body$
DECLARE
  target_table regclass;
  _row_trigger_name text;
  _truncate_trigger_name text;
  _q_txt text;
BEGIN
  target_table = (in_table_schema || '.' || in_table_name)::regclass;

  _row_trigger_name      = 'rowcounts_row_trigger';
  _truncate_trigger_name = 'rowcounts_truncate_trigger';
  EXECUTE 'DROP TRIGGER IF EXISTS ' || _row_trigger_name || ' ON ' || quote_ident(target_table::text);
  _q_txt = 'CREATE TRIGGER ' || _row_trigger_name || ' AFTER INSERT OR DELETE ON ' ||
           quote_ident(target_table::text) ||
           ' FOR EACH ROW EXECUTE PROCEDURE rowcounts.track_rowcount_func();';
  RAISE NOTICE '%', _q_txt;
  EXECUTE _q_txt;

  EXECUTE 'DROP TRIGGER IF EXISTS ' || _truncate_trigger_name || ' ON ' || quote_ident(target_table::text);
  _q_txt = 'CREATE TRIGGER ' || _truncate_trigger_name || ' AFTER TRUNCATE ON ' ||
           quote_ident(target_table::text) ||
           ' FOR EACH STATEMENT EXECUTE PROCEDURE rowcounts.track_rowcount_on_truncate_func();';
  RAISE NOTICE '%', _q_txt;
  EXECUTE _q_txt;

  -- Lock the table and insert the initial count
  LOCK TABLE rowcounts.counts IN EXCLUSIVE MODE;
  DELETE FROM rowcounts.counts WHERE schema_name = in_table_schema AND table_name = in_table_name;
  _q_txt = 'INSERT INTO rowcounts.counts ' ||
                       '(schema_name,table_name,num_rows) ' ||
          'VALUES (''' || in_table_schema || ''', ' ||
          '       ''' || in_table_name || ''',  ' ||
          '       (SELECT count(''x'') FROM ' || target_table || '));';
  RAISE NOTICE '%', _q_txt;
  EXECUTE _q_txt;

END;
$body$
LANGUAGE plpgsql
SECURITY DEFINER
SET search_path = pg_catalog, public;

