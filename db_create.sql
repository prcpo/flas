--
-- FLAS - Free/libre accounting system
--
-- Creating database
--
-- PostgreSQL version 9.1.1
-- Required ltree and plpgsql contrib modules
-- 

SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

---------

CREATE SCHEMA app;
CREATE SCHEMA dic;
CREATE SCHEMA sec;
CREATE SCHEMA test;

---------

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;
COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';

CREATE EXTENSION IF NOT EXISTS ltree WITH SCHEMA public;
COMMENT ON EXTENSION ltree IS 'data type for hierarchical tree-like structures';

---------

SET search_path = app, pg_catalog;

CREATE FUNCTION modes_node_children_get(_node_code text DEFAULT ''::text) RETURNS text
    LANGUAGE sql
    AS $_$select array_to_json(
	array(
		select 
			json_element(
				tree::text,
				json_row(
					json_element('Function', 'table_show(\"' || tree::text || '\")'),
					json_element('Text', lbl),
					json_element('Type', 'Mode'::text)
				)
			)
			from
			app.modes
			where
			tree <@ text2ltree(COALESCE($1,''))
			and 
			nlevel(tree) = nlevel(text2ltree(COALESCE($1,'')))+1
	)
);
$_$;


SET search_path = dic, pg_catalog;


CREATE FUNCTION param_def_add(VARIADIC arr text[]) RETURNS void
    LANGUAGE plpgsql
    AS $$begin
  perform dic.param_def_add(text2ltree(arr[1]), VARIADIC arr[2:3]);
end;$$;


CREATE FUNCTION param_def_add(_tree public.ltree, VARIADIC arr text[]) RETURNS void
    LANGUAGE plpgsql
    AS $$begin
	insert into dic.param_def
		(tree, lbl, note)
	values
		(_tree, coalesce(arr[1],ltree2text(_tree)), arr[2]);
end;$$;


CREATE FUNCTION param_value_get(_param public.ltree, _usr text DEFAULT "current_user"(), _cmp integer DEFAULT NULL::integer, _dt date DEFAULT NULL::date) RETURNS text
    LANGUAGE plpgsql
    AS $$
declare _res text;
begin
	select val into _res
	from dic.param_values 
	where param = _param
	and coalesce(usr,'') = coalesce(_usr,'')
	and coalesce(cmp,-1) = coalesce(_cmp,-1)
	and dt_b <= COALESCE(_dt,current_date);

	return _res;
end
$$;


CREATE FUNCTION param_value_get(_param text, _usr text DEFAULT "current_user"(), _cmp integer DEFAULT NULL::integer, _dt date DEFAULT '-infinity'::date) RETURNS text
    LANGUAGE plpgsql
    AS $$begin
	return dic.param_value_get(text2ltree(_param), _usr, _cmp, _dt);
end;$$;


CREATE FUNCTION param_value_set(_param public.ltree, _val anyelement, _usr text DEFAULT "current_user"(), _cmp integer DEFAULT NULL::integer, _dt date DEFAULT '-infinity'::date) RETURNS void
    LANGUAGE plpgsql
    AS $$begin
	-- если нет такого параметра, добавляем
	if not exists (
			select 'x' 
			from dic.param_def 
			where tree = _param
			) then
		insert into dic.param_def(tree)
		values (_param);
	end if;
	-- если значение параметра есть, заменяем  на новое
	if exists (
			select 'x' 
			from dic.param_values 
			where param = _param
			and coalesce(usr,'') = coalesce(_usr,'')
			and coalesce(cmp,-1) = coalesce(_cmp,-1)
			and dt_b = _dt
			) then
		update dic.param_values 
		set val = _val::text
		where param = _param
		and coalesce(usr,'') = coalesce(_usr,'')
		and coalesce(cmp,-1) = coalesce(_cmp,-1)
		and dt_b = _dt;
  -- если значения параметра нет, добавляем
	else
		insert into dic.param_values 
		(usr, cmp, param, val, dt_b)
		values
		(_usr, _cmp, _param, _val::text, _dt);
	end if;
end;$$;


CREATE FUNCTION param_value_set(_param text, _val anyelement, _usr text DEFAULT "current_user"(), _cmp integer DEFAULT NULL::integer, _dt date DEFAULT '-infinity'::date) RETURNS void
    LANGUAGE plpgsql
    AS $$begin
	perform dic.param_value_set(text2ltree(_param), _val, _usr, _cmp, _dt);
end;$$;


SET search_path = public, pg_catalog;


CREATE FUNCTION array_to_json(anyarray) RETURNS text
    LANGUAGE sql
    AS $_$select '{' || array_to_string($1, ', ') || '}'$_$;


CREATE FUNCTION json_element(text, anyelement) RETURNS text
    LANGUAGE sql
    AS $_$select '"' || $1 || '": ' || regexp_replace($2,'^([^\{]*[^\}])$',E'"\\1"','g');$_$;

CREATE FUNCTION json_row(VARIADIC text[]) RETURNS text
    LANGUAGE sql
    AS $_$select array_to_json($1)$_$;


CREATE FUNCTION modes_node_children_get(_node_code text DEFAULT ''::text) RETURNS text
    LANGUAGE sql
    AS $_$select app.modes_node_children_get($1);
$_$;

CREATE FUNCTION param_value_get(_param text) RETURNS text
    LANGUAGE sql
    AS $_$select dic.param_value_get($1)$_$;


SET search_path = test, pg_catalog;

CREATE FUNCTION do_test(_command text, _expect text, _user text DEFAULT "current_user"()) RETURNS boolean
    LANGUAGE plpgsql
    AS $$declare
	_id	integer;
  _res  text;
  _exec	text;
begin
  
	insert into test.results(code) values (_command) returning id into _id;

  _exec = 'select ' || _command; 
	raise notice 'Try: %', _exec;

  begin
		execute _exec into _res;
	end;

	raise notice 'Result: %', _res;
  
	update test.results
	set result = (COALESCE(_res = _expect, false))::text
	where id = _id;
	return COALESCE(_res = _expect, false);
end;$$;


CREATE FUNCTION test_all() RETURNS text
    LANGUAGE plpgsql
    AS $$declare 
	_res boolean;
	_cur	cursor for 
		select code, res 
		from test.tests 
		order by id;
	_rec	record;
begin
  delete from test.results;
	_res = true;
	for _rec in _cur loop
		_res = _res AND test.do_test(_rec.code,_rec.res);
  end loop;
  return _res;
end;$$;

---------

SET search_path = app, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

CREATE TABLE modes (
    tree public.ltree NOT NULL,
    lbl text
);


SET search_path = dic, pg_catalog;

CREATE TABLE param_def (
    tree public.ltree NOT NULL,
    lbl text,
    note text
);


CREATE TABLE param_values (
    usr text DEFAULT "current_user"() NOT NULL,
    cmp integer,
    param public.ltree NOT NULL,
    val text,
    dt_b date DEFAULT '-infinity'::date NOT NULL,
    dt_e date DEFAULT 'infinity'::date
);


SET search_path = sec, pg_catalog;

CREATE TABLE companies (
    id integer NOT NULL,
    nm text
);


CREATE TABLE users (
    user_name text DEFAULT "current_user"() NOT NULL,
    company integer NOT NULL
);


SET search_path = public, pg_catalog;

CREATE VIEW companies AS
    SELECT companies.id, companies.nm FROM sec.companies, sec.users WHERE ((users.company = companies.id) AND (users.user_name = ("current_user"())::text));

---------

SET search_path = sec, pg_catalog;

CREATE SEQUENCE companies_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


SELECT pg_catalog.setval('companies_id_seq', 1, false);


SET search_path = test, pg_catalog;

CREATE TABLE results (
    id integer NOT NULL,
    dt timestamp with time zone DEFAULT now(),
    code text,
    result text
);


CREATE SEQUENCE results_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE results_id_seq OWNED BY results.id;

SELECT pg_catalog.setval('results_id_seq', 1, true);

CREATE TABLE tests (
    code text,
    res text,
    id integer NOT NULL
);

CREATE SEQUENCE tests_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE tests_id_seq OWNED BY tests.id;


SELECT pg_catalog.setval('tests_id_seq', 1, true);


SET search_path = sec, pg_catalog;


ALTER TABLE companies ALTER COLUMN id SET DEFAULT nextval('companies_id_seq'::regclass);


SET search_path = test, pg_catalog;

ALTER TABLE results ALTER COLUMN id SET DEFAULT nextval('results_id_seq'::regclass);


ALTER TABLE tests ALTER COLUMN id SET DEFAULT nextval('tests_id_seq'::regclass);


---------

SET search_path = app, pg_catalog;

ALTER TABLE ONLY modes
    ADD CONSTRAINT pk_modes PRIMARY KEY (tree);


SET search_path = dic, pg_catalog;


ALTER TABLE ONLY param_def
    ADD CONSTRAINT pk_param_def PRIMARY KEY (tree);



ALTER TABLE ONLY param_values
    ADD CONSTRAINT pk_param_values PRIMARY KEY (usr, param, dt_b);


SET search_path = sec, pg_catalog;


ALTER TABLE ONLY companies
    ADD CONSTRAINT pk_companies PRIMARY KEY (id);



ALTER TABLE ONLY users
    ADD CONSTRAINT pk_users PRIMARY KEY (user_name, company);


SET search_path = test, pg_catalog;


ALTER TABLE ONLY results
    ADD CONSTRAINT pk_results PRIMARY KEY (id);



ALTER TABLE ONLY tests
    ADD CONSTRAINT pk_tests PRIMARY KEY (id);


SET search_path = dic, pg_catalog;



CREATE INDEX fki_param_values_param ON param_values USING btree (param);


SET search_path = sec, pg_catalog;


CREATE INDEX fki_users_company ON users USING btree (company);


SET search_path = dic, pg_catalog;


ALTER TABLE ONLY param_values
    ADD CONSTRAINT fk_param_values_param FOREIGN KEY (param) REFERENCES param_def(tree) ON UPDATE CASCADE ON DELETE CASCADE;


SET search_path = sec, pg_catalog;


ALTER TABLE ONLY users
    ADD CONSTRAINT fk_users_company FOREIGN KEY (company) REFERENCES companies(id) ON UPDATE CASCADE ON DELETE CASCADE;

--------

REVOKE ALL ON SCHEMA public FROM PUBLIC;

GRANT ALL ON SCHEMA public TO PUBLIC;


