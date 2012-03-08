--
-- PostgreSQL database dump
--

-- Dumped from database version 9.1.3
-- Dumped by pg_dump version 9.1.3
-- Started on 2012-03-08 23:16:58 MSK

SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- TOC entry 6 (class 2615 OID 28563)
-- Name: app; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA app;


--
-- TOC entry 2180 (class 0 OID 0)
-- Dependencies: 6
-- Name: SCHEMA app; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON SCHEMA app IS 'Данные и функции, устанавливающие правила поведения клиентской части (GUI).
Здесь:
- Список доступных режимов работы
- Правила отображения таблиц и форм документов
- Доступные пользователю действия
и т.д.';


--
-- TOC entry 7 (class 2615 OID 28564)
-- Name: dic; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA dic;


--
-- TOC entry 2181 (class 0 OID 0)
-- Dependencies: 7
-- Name: SCHEMA dic; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON SCHEMA dic IS 'Справочники';


--
-- TOC entry 8 (class 2615 OID 28565)
-- Name: ext; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA ext;


--
-- TOC entry 2182 (class 0 OID 0)
-- Dependencies: 8
-- Name: SCHEMA ext; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON SCHEMA ext IS 'Сюда устанавливаем расширения, чтобы не захламлять public.
Для корректной работы требуется установить переменную базы данных:

ALTER DATABASE {database} SET search_path TO public, ext;';


--
-- TOC entry 9 (class 2615 OID 28566)
-- Name: json; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA json;


--
-- TOC entry 2183 (class 0 OID 0)
-- Dependencies: 9
-- Name: SCHEMA json; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON SCHEMA json IS 'Функции для работы с JSON';


--
-- TOC entry 10 (class 2615 OID 28567)
-- Name: obj; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA obj;


--
-- TOC entry 2184 (class 0 OID 0)
-- Dependencies: 10
-- Name: SCHEMA obj; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON SCHEMA obj IS 'Структура хранения объектов приложения';


--
-- TOC entry 11 (class 2615 OID 28568)
-- Name: sec; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA sec;


--
-- TOC entry 2186 (class 0 OID 0)
-- Dependencies: 11
-- Name: SCHEMA sec; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON SCHEMA sec IS 'Обеспечение безопасности, разграничения прав доступа';


--
-- TOC entry 12 (class 2615 OID 28569)
-- Name: test; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA test;


--
-- TOC entry 2187 (class 0 OID 0)
-- Dependencies: 12
-- Name: SCHEMA test; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON SCHEMA test IS 'Тесты';


--
-- TOC entry 183 (class 3079 OID 11685)
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- TOC entry 2188 (class 0 OID 0)
-- Dependencies: 183
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


--
-- TOC entry 184 (class 3079 OID 28570)
-- Dependencies: 8
-- Name: ltree; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS ltree WITH SCHEMA ext;


--
-- TOC entry 2189 (class 0 OID 0)
-- Dependencies: 184
-- Name: EXTENSION ltree; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION ltree IS 'data type for hierarchical tree-like structures';


SET search_path = app, pg_catalog;

--
-- TOC entry 266 (class 1255 OID 28745)
-- Dependencies: 601 6
-- Name: actions_get(ext.ltree); Type: FUNCTION; Schema: app; Owner: -
--

CREATE FUNCTION actions_get(ext.ltree) RETURNS text
    LANGUAGE sql
    AS $_$select json.get(
	array(
		select 
			json.element(
			"action",
				json.elements(
					json.element('Text',lbl),
					json.element('Function',func)
				)
			)
		from app.actions
		where tree=$1
	)
)$_$;


--
-- TOC entry 267 (class 1255 OID 28746)
-- Dependencies: 6
-- Name: modes_node_children_get(text); Type: FUNCTION; Schema: app; Owner: -
--

CREATE FUNCTION modes_node_children_get(_node_code text DEFAULT ''::text) RETURNS text
    LANGUAGE sql
    AS $_$select json.get(
	array(
		select 
			json.element(
				tree::text,
				json.elements(
					json.element('Text', a.lbl),
					json.element('Type', 'Mode'::text),
					json.element('Children',
						exists (
							select 'x' 
							from app.modes c 
							where 
							c.tree <@ a.tree 
							and not c.tree = a.tree
						)
					),			
					json.element('Actions', app.actions_get(a.tree))
				)
			)
			from
			app.modes a
			where
			a.tree <@ ext.text2ltree(COALESCE($1,''))
			and 
			nlevel(a.tree) = nlevel(ext.text2ltree(COALESCE($1,'')))+1

	)
);
$_$;


SET search_path = dic, pg_catalog;

--
-- TOC entry 268 (class 1255 OID 28747)
-- Dependencies: 664 7
-- Name: param_def_add(text[]); Type: FUNCTION; Schema: dic; Owner: -
--

CREATE FUNCTION param_def_add(VARIADIC arr text[]) RETURNS void
    LANGUAGE plpgsql
    AS $$begin
  perform dic.param_def_add(ext.text2ltree(arr[1]), VARIADIC arr[2:3]);
end;$$;


--
-- TOC entry 269 (class 1255 OID 28748)
-- Dependencies: 664 7 601
-- Name: param_def_add(ext.ltree, text[]); Type: FUNCTION; Schema: dic; Owner: -
--

CREATE FUNCTION param_def_add(_tree ext.ltree, VARIADIC arr text[]) RETURNS void
    LANGUAGE plpgsql
    AS $$begin
	insert into dic.param_def
		(tree, lbl, note)
	values
		(_tree, coalesce(arr[1],ltree2text(_tree)), arr[2]);
end;$$;


--
-- TOC entry 270 (class 1255 OID 28749)
-- Dependencies: 664 601 7
-- Name: param_value_get(ext.ltree, text, integer, date); Type: FUNCTION; Schema: dic; Owner: -
--

CREATE FUNCTION param_value_get(_param ext.ltree, _usr text DEFAULT "current_user"(), _cmp integer DEFAULT NULL::integer, _dt date DEFAULT NULL::date) RETURNS text
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


--
-- TOC entry 271 (class 1255 OID 28750)
-- Dependencies: 7 664
-- Name: param_value_get(text, text, integer, date); Type: FUNCTION; Schema: dic; Owner: -
--

CREATE FUNCTION param_value_get(_param text, _usr text DEFAULT "current_user"(), _cmp integer DEFAULT NULL::integer, _dt date DEFAULT '-infinity'::date) RETURNS text
    LANGUAGE plpgsql
    AS $$begin
	return dic.param_value_get(ext.text2ltree(_param), _usr, _cmp, _dt);
end;$$;


--
-- TOC entry 272 (class 1255 OID 28751)
-- Dependencies: 601 7 664
-- Name: param_value_set(ext.ltree, anyelement, text, integer, date); Type: FUNCTION; Schema: dic; Owner: -
--

CREATE FUNCTION param_value_set(_param ext.ltree, _val anyelement, _usr text DEFAULT "current_user"(), _cmp integer DEFAULT NULL::integer, _dt date DEFAULT '-infinity'::date) RETURNS void
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


--
-- TOC entry 273 (class 1255 OID 28752)
-- Dependencies: 664 7
-- Name: param_value_set(text, anyelement, text, integer, date); Type: FUNCTION; Schema: dic; Owner: -
--

CREATE FUNCTION param_value_set(_param text, _val anyelement, _usr text DEFAULT "current_user"(), _cmp integer DEFAULT NULL::integer, _dt date DEFAULT '-infinity'::date) RETURNS void
    LANGUAGE plpgsql
    AS $$begin
	perform dic.param_value_set(ext.text2ltree(_param), _val, _usr, _cmp, _dt);
end;$$;


SET search_path = json, pg_catalog;

--
-- TOC entry 275 (class 1255 OID 28758)
-- Dependencies: 9
-- Name: element(text, anyelement); Type: FUNCTION; Schema: json; Owner: -
--

CREATE FUNCTION element(text, anyelement) RETURNS text
    LANGUAGE sql
    AS $_$select '"' || $1 || '": ' || json.value($2);$_$;


--
-- TOC entry 276 (class 1255 OID 28759)
-- Dependencies: 9
-- Name: elements(text[]); Type: FUNCTION; Schema: json; Owner: -
--

CREATE FUNCTION elements(VARIADIC text[]) RETURNS text
    LANGUAGE sql
    AS $_$select json.get($1)$_$;


--
-- TOC entry 274 (class 1255 OID 28757)
-- Dependencies: 9
-- Name: get(anyarray); Type: FUNCTION; Schema: json; Owner: -
--

CREATE FUNCTION get(anyarray) RETURNS text
    LANGUAGE sql
    AS $_$select '{' || array_to_string($1, ', ') || '}'$_$;


--
-- TOC entry 277 (class 1255 OID 28760)
-- Dependencies: 9
-- Name: value(text); Type: FUNCTION; Schema: json; Owner: -
--

CREATE FUNCTION value(text) RETURNS text
    LANGUAGE sql
    AS $_$select regexp_replace($1,'^([^\{]*[^\}])$',E'"\\1"','g')$_$;


--
-- TOC entry 278 (class 1255 OID 28761)
-- Dependencies: 9
-- Name: value(boolean); Type: FUNCTION; Schema: json; Owner: -
--

CREATE FUNCTION value(boolean) RETURNS text
    LANGUAGE sql
    AS $_$select $1::text$_$;


--
-- TOC entry 279 (class 1255 OID 28762)
-- Dependencies: 9
-- Name: value(integer); Type: FUNCTION; Schema: json; Owner: -
--

CREATE FUNCTION value(integer) RETURNS text
    LANGUAGE sql
    AS $_$select $1::text$_$;


SET search_path = obj, pg_catalog;

--
-- TOC entry 288 (class 1255 OID 28942)
-- Dependencies: 601 10
-- Name: add(ext.ltree, bigint); Type: FUNCTION; Schema: obj; Owner: -
--

CREATE FUNCTION add(_code ext.ltree, _parent bigint DEFAULT NULL::bigint) RETURNS bigint
    LANGUAGE sql
    AS $_$insert into obj.objects (code, parent)
values ($1, $2)
returning id;$_$;


--
-- TOC entry 289 (class 1255 OID 28944)
-- Dependencies: 10
-- Name: add(text, bigint); Type: FUNCTION; Schema: obj; Owner: -
--

CREATE FUNCTION add(_code text, _parent bigint DEFAULT NULL::bigint) RETURNS bigint
    LANGUAGE sql
    AS $_$select obj.add($1::text, $2);$_$;


--
-- TOC entry 284 (class 1255 OID 28902)
-- Dependencies: 10
-- Name: get(bigint); Type: FUNCTION; Schema: obj; Owner: -
--

CREATE FUNCTION get(_id bigint) RETURNS text
    LANGUAGE sql
    AS $_$select 
  json.get(
    array(
      select 
        json.element( 
          id::text,
          json.get(
            json.element('Type', o.code::text) ||
            array(
              select obj.req_get(id)
              from obj.req
              where obj = o.id
            ) 
          )
        )
      from obj.objects o
      where o.id = $1
    )
  )$_$;


--
-- TOC entry 287 (class 1255 OID 28938)
-- Dependencies: 601 664 10
-- Name: req_add(bigint, ext.ltree, text, bigint); Type: FUNCTION; Schema: obj; Owner: -
--

CREATE FUNCTION req_add(_obj bigint, _code ext.ltree, _val text, _parent bigint DEFAULT NULL::bigint) RETURNS bigint
    LANGUAGE plpgsql
    AS $$declare
  _id Bigint;
begin
  insert into obj.req (obj, code, val, parent)
    values (_obj, _code, _val, _parent) returning id into _id;
  return _id;
end;$$;


--
-- TOC entry 285 (class 1255 OID 28939)
-- Dependencies: 10
-- Name: req_add(bigint, text, text, bigint); Type: FUNCTION; Schema: obj; Owner: -
--

CREATE FUNCTION req_add(_obj bigint, _code text, _val text, _parent bigint DEFAULT NULL::bigint) RETURNS bigint
    LANGUAGE sql
    AS $_$select obj.req_add($1, ext.text2ltree($2), $3, $4)$_$;


--
-- TOC entry 286 (class 1255 OID 28937)
-- Dependencies: 10 664
-- Name: req_get(bigint); Type: FUNCTION; Schema: obj; Owner: -
--

CREATE FUNCTION req_get(_id bigint) RETURNS text
    LANGUAGE plpgsql
    AS $$declare
  _res text;
  _name text;
begin
  select 
    code::text,
    val 
  into _name, _res
  from obj.req 
  where id = _id;

  if exists (select 'x' from obj.req where parent = _id or obj = _id) then
    _res := json.get(
        array(
          select obj.req_get(id)
          from obj.req
          where parent = _id or obj = _id
        )
      );
  end if;
  return json.element(_name, _res);
end;$$;


SET search_path = public, pg_catalog;

--
-- TOC entry 280 (class 1255 OID 28763)
-- Dependencies: 13
-- Name: modes_node_children_get(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION modes_node_children_get(_node_code text DEFAULT ''::text) RETURNS text
    LANGUAGE sql
    AS $_$select app.modes_node_children_get($1);
$_$;


--
-- TOC entry 290 (class 1255 OID 28945)
-- Dependencies: 13
-- Name: object_add(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION object_add(_code text) RETURNS bigint
    LANGUAGE sql
    AS $_$select obj.add($1)$_$;


--
-- TOC entry 2190 (class 0 OID 0)
-- Dependencies: 290
-- Name: FUNCTION object_add(_code text); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION object_add(_code text) IS 'Создаёт новый объект (документ, элемент справочника).
Параметр - код объекта, например:
''doc.pp'' - документ "Платёжное поручение"
''dic.org'' - организация в справочнике организаций
Возвращает ID созданного объекта.';


--
-- TOC entry 283 (class 1255 OID 28903)
-- Dependencies: 13
-- Name: object_get(bigint); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION object_get(_id bigint) RETURNS text
    LANGUAGE sql
    AS $_$select obj.get($1)$_$;


--
-- TOC entry 2191 (class 0 OID 0)
-- Dependencies: 283
-- Name: FUNCTION object_get(_id bigint); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION object_get(_id bigint) IS 'Возвращает объект (документ, элемент справочника) в формате JSON.
Параметр - ID объекта';


--
-- TOC entry 291 (class 1255 OID 28947)
-- Dependencies: 13
-- Name: object_req_add(bigint, text, text, bigint); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION object_req_add(_obj bigint, _code text, _val text, _parent bigint DEFAULT NULL::bigint) RETURNS bigint
    LANGUAGE sql
    AS $_$select obj.req_add($1, $2, $3, $4)$_$;


--
-- TOC entry 2192 (class 0 OID 0)
-- Dependencies: 291
-- Name: FUNCTION object_req_add(_obj bigint, _code text, _val text, _parent bigint); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION object_req_add(_obj bigint, _code text, _val text, _parent bigint) IS 'Добавляет реквизит к объекту. Например, назначение платежа к документу "Платёжное поручение".
Параметры:
1) ID объекта
2) код реквизита
3) значение реквизита
4) вышестоящий по иерархии ревизит (необязательно)';


SET search_path = test, pg_catalog;

--
-- TOC entry 281 (class 1255 OID 28764)
-- Dependencies: 12 664
-- Name: do_test(text, text, text); Type: FUNCTION; Schema: test; Owner: -
--

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


--
-- TOC entry 282 (class 1255 OID 28765)
-- Dependencies: 664 12
-- Name: test_all(); Type: FUNCTION; Schema: test; Owner: -
--

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


SET search_path = app, pg_catalog;

SET default_with_oids = false;

--
-- TOC entry 168 (class 1259 OID 28766)
-- Dependencies: 6 601
-- Name: actions; Type: TABLE; Schema: app; Owner: -
--

CREATE TABLE actions (
    tree ext.ltree NOT NULL,
    action text NOT NULL,
    lbl text,
    func text
);


--
-- TOC entry 169 (class 1259 OID 28772)
-- Dependencies: 6 601
-- Name: modes; Type: TABLE; Schema: app; Owner: -
--

CREATE TABLE modes (
    tree ext.ltree NOT NULL,
    lbl text
);


SET search_path = dic, pg_catalog;

--
-- TOC entry 170 (class 1259 OID 28778)
-- Dependencies: 601 7
-- Name: param_def; Type: TABLE; Schema: dic; Owner: -
--

CREATE TABLE param_def (
    tree ext.ltree NOT NULL,
    lbl text,
    note text
);


--
-- TOC entry 171 (class 1259 OID 28784)
-- Dependencies: 2124 2125 2126 7 601
-- Name: param_values; Type: TABLE; Schema: dic; Owner: -
--

CREATE TABLE param_values (
    usr text DEFAULT "current_user"() NOT NULL,
    cmp integer,
    param ext.ltree NOT NULL,
    val text,
    dt_b date DEFAULT '-infinity'::date NOT NULL,
    dt_e date DEFAULT 'infinity'::date
);


SET search_path = obj, pg_catalog;

--
-- TOC entry 172 (class 1259 OID 28793)
-- Dependencies: 10 601
-- Name: objects; Type: TABLE; Schema: obj; Owner: -
--

CREATE TABLE objects (
    id bigint NOT NULL,
    cmp integer,
    parent bigint,
    code ext.ltree
);


--
-- TOC entry 173 (class 1259 OID 28796)
-- Dependencies: 10 172
-- Name: objects_id_seq; Type: SEQUENCE; Schema: obj; Owner: -
--

CREATE SEQUENCE objects_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- TOC entry 2193 (class 0 OID 0)
-- Dependencies: 173
-- Name: objects_id_seq; Type: SEQUENCE OWNED BY; Schema: obj; Owner: -
--

ALTER SEQUENCE objects_id_seq OWNED BY objects.id;


--
-- TOC entry 2194 (class 0 OID 0)
-- Dependencies: 173
-- Name: objects_id_seq; Type: SEQUENCE SET; Schema: obj; Owner: -
--

SELECT pg_catalog.setval('objects_id_seq', 1, true);


--
-- TOC entry 182 (class 1259 OID 28912)
-- Dependencies: 2133 10 601
-- Name: req; Type: TABLE; Schema: obj; Owner: -
--

CREATE TABLE req (
    id bigint DEFAULT nextval('objects_id_seq'::regclass) NOT NULL,
    obj bigint NOT NULL,
    parent bigint,
    code ext.ltree,
    val text
);


SET search_path = sec, pg_catalog;

--
-- TOC entry 174 (class 1259 OID 28804)
-- Dependencies: 11
-- Name: companies_id_seq; Type: SEQUENCE; Schema: sec; Owner: -
--

CREATE SEQUENCE companies_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- TOC entry 2195 (class 0 OID 0)
-- Dependencies: 174
-- Name: companies_id_seq; Type: SEQUENCE SET; Schema: sec; Owner: -
--

SELECT pg_catalog.setval('companies_id_seq', 1, false);


--
-- TOC entry 175 (class 1259 OID 28806)
-- Dependencies: 2128 11
-- Name: companies; Type: TABLE; Schema: sec; Owner: -
--

CREATE TABLE companies (
    id integer DEFAULT nextval('companies_id_seq'::regclass) NOT NULL,
    nm text
);


--
-- TOC entry 176 (class 1259 OID 28813)
-- Dependencies: 2129 11
-- Name: users; Type: TABLE; Schema: sec; Owner: -
--

CREATE TABLE users (
    user_name text DEFAULT "current_user"() NOT NULL,
    company integer NOT NULL
);


SET search_path = public, pg_catalog;

--
-- TOC entry 177 (class 1259 OID 28820)
-- Dependencies: 2123 13
-- Name: companies; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW companies AS
    SELECT companies.id, companies.nm FROM sec.companies, sec.users WHERE ((users.company = companies.id) AND (users.user_name = ("current_user"())::text));


SET search_path = test, pg_catalog;

--
-- TOC entry 178 (class 1259 OID 28824)
-- Dependencies: 2130 12
-- Name: results; Type: TABLE; Schema: test; Owner: -
--

CREATE TABLE results (
    id integer NOT NULL,
    dt timestamp with time zone DEFAULT now(),
    code text,
    result text
);


--
-- TOC entry 179 (class 1259 OID 28831)
-- Dependencies: 178 12
-- Name: results_id_seq; Type: SEQUENCE; Schema: test; Owner: -
--

CREATE SEQUENCE results_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- TOC entry 2196 (class 0 OID 0)
-- Dependencies: 179
-- Name: results_id_seq; Type: SEQUENCE OWNED BY; Schema: test; Owner: -
--

ALTER SEQUENCE results_id_seq OWNED BY results.id;


--
-- TOC entry 2197 (class 0 OID 0)
-- Dependencies: 179
-- Name: results_id_seq; Type: SEQUENCE SET; Schema: test; Owner: -
--

SELECT pg_catalog.setval('results_id_seq', 1, true);


--
-- TOC entry 180 (class 1259 OID 28833)
-- Dependencies: 12
-- Name: tests; Type: TABLE; Schema: test; Owner: -
--

CREATE TABLE tests (
    code text,
    res text,
    id integer NOT NULL
);


--
-- TOC entry 181 (class 1259 OID 28839)
-- Dependencies: 180 12
-- Name: tests_id_seq; Type: SEQUENCE; Schema: test; Owner: -
--

CREATE SEQUENCE tests_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- TOC entry 2198 (class 0 OID 0)
-- Dependencies: 181
-- Name: tests_id_seq; Type: SEQUENCE OWNED BY; Schema: test; Owner: -
--

ALTER SEQUENCE tests_id_seq OWNED BY tests.id;


--
-- TOC entry 2199 (class 0 OID 0)
-- Dependencies: 181
-- Name: tests_id_seq; Type: SEQUENCE SET; Schema: test; Owner: -
--

SELECT pg_catalog.setval('tests_id_seq', 2, true);


SET search_path = obj, pg_catalog;

--
-- TOC entry 2127 (class 2604 OID 28841)
-- Dependencies: 173 172
-- Name: id; Type: DEFAULT; Schema: obj; Owner: -
--

ALTER TABLE ONLY objects ALTER COLUMN id SET DEFAULT nextval('objects_id_seq'::regclass);


SET search_path = test, pg_catalog;

--
-- TOC entry 2131 (class 2604 OID 28843)
-- Dependencies: 179 178
-- Name: id; Type: DEFAULT; Schema: test; Owner: -
--

ALTER TABLE ONLY results ALTER COLUMN id SET DEFAULT nextval('results_id_seq'::regclass);


--
-- TOC entry 2132 (class 2604 OID 28844)
-- Dependencies: 181 180
-- Name: id; Type: DEFAULT; Schema: test; Owner: -
--

ALTER TABLE ONLY tests ALTER COLUMN id SET DEFAULT nextval('tests_id_seq'::regclass);


SET search_path = app, pg_catalog;

--
-- TOC entry 2167 (class 0 OID 28766)
-- Dependencies: 168
-- Data for Name: actions; Type: TABLE DATA; Schema: app; Owner: -
--

INSERT INTO actions (tree, action, lbl, func) VALUES ('doc.pay', 'open', '*Список', 'doc_list');


--
-- TOC entry 2168 (class 0 OID 28772)
-- Dependencies: 169
-- Data for Name: modes; Type: TABLE DATA; Schema: app; Owner: -
--

INSERT INTO modes (tree, lbl) VALUES ('doc', 'Документы');
INSERT INTO modes (tree, lbl) VALUES ('dic', 'Справочники');
INSERT INTO modes (tree, lbl) VALUES ('doc.pay', 'Платёжные поручения');
INSERT INTO modes (tree, lbl) VALUES ('dic.companies', 'Организации');


SET search_path = dic, pg_catalog;

--
-- TOC entry 2169 (class 0 OID 28778)
-- Dependencies: 170
-- Data for Name: param_def; Type: TABLE DATA; Schema: dic; Owner: -
--

INSERT INTO param_def (tree, lbl, note) VALUES ('defaults', 'Настройки по-умолчанию', NULL);
INSERT INTO param_def (tree, lbl, note) VALUES ('defaults.company_name', 'Наименование организации по-умолчанию', NULL);
INSERT INTO param_def (tree, lbl, note) VALUES ('session', 'Параметры текущего сеанса', NULL);
INSERT INTO param_def (tree, lbl, note) VALUES ('session.work_date', NULL, NULL);
INSERT INTO param_def (tree, lbl, note) VALUES ('session.active_company', NULL, NULL);


--
-- TOC entry 2170 (class 0 OID 28784)
-- Dependencies: 171
-- Data for Name: param_values; Type: TABLE DATA; Schema: dic; Owner: -
--

INSERT INTO param_values (usr, cmp, param, val, dt_b, dt_e) VALUES ('', NULL, 'defaults.company_name', 'Моя организация', '-infinity', 'infinity');


SET search_path = obj, pg_catalog;

--
-- TOC entry 2171 (class 0 OID 28793)
-- Dependencies: 172
-- Data for Name: objects; Type: TABLE DATA; Schema: obj; Owner: -
--



--
-- TOC entry 2176 (class 0 OID 28912)
-- Dependencies: 182
-- Data for Name: req; Type: TABLE DATA; Schema: obj; Owner: -
--



SET search_path = sec, pg_catalog;

--
-- TOC entry 2172 (class 0 OID 28806)
-- Dependencies: 175
-- Data for Name: companies; Type: TABLE DATA; Schema: sec; Owner: -
--



--
-- TOC entry 2173 (class 0 OID 28813)
-- Dependencies: 176
-- Data for Name: users; Type: TABLE DATA; Schema: sec; Owner: -
--



SET search_path = test, pg_catalog;

--
-- TOC entry 2174 (class 0 OID 28824)
-- Dependencies: 178
-- Data for Name: results; Type: TABLE DATA; Schema: test; Owner: -
--



--
-- TOC entry 2175 (class 0 OID 28833)
-- Dependencies: 180
-- Data for Name: tests; Type: TABLE DATA; Schema: test; Owner: -
--

INSERT INTO tests (code, res, id) VALUES ('param_value_get(''defaults.company_name'')', 'Моя организация', 2);


SET search_path = app, pg_catalog;

--
-- TOC entry 2135 (class 2606 OID 28846)
-- Dependencies: 168 168 168
-- Name: pk_actions; Type: CONSTRAINT; Schema: app; Owner: -
--

ALTER TABLE ONLY actions
    ADD CONSTRAINT pk_actions PRIMARY KEY (tree, action);


--
-- TOC entry 2137 (class 2606 OID 28848)
-- Dependencies: 169 169
-- Name: pk_modes; Type: CONSTRAINT; Schema: app; Owner: -
--

ALTER TABLE ONLY modes
    ADD CONSTRAINT pk_modes PRIMARY KEY (tree);


SET search_path = dic, pg_catalog;

--
-- TOC entry 2139 (class 2606 OID 28850)
-- Dependencies: 170 170
-- Name: pk_param_def; Type: CONSTRAINT; Schema: dic; Owner: -
--

ALTER TABLE ONLY param_def
    ADD CONSTRAINT pk_param_def PRIMARY KEY (tree);


--
-- TOC entry 2142 (class 2606 OID 28852)
-- Dependencies: 171 171 171 171
-- Name: pk_param_values; Type: CONSTRAINT; Schema: dic; Owner: -
--

ALTER TABLE ONLY param_values
    ADD CONSTRAINT pk_param_values PRIMARY KEY (usr, param, dt_b);


SET search_path = obj, pg_catalog;

--
-- TOC entry 2146 (class 2606 OID 28854)
-- Dependencies: 172 172
-- Name: pk_objects; Type: CONSTRAINT; Schema: obj; Owner: -
--

ALTER TABLE ONLY objects
    ADD CONSTRAINT pk_objects PRIMARY KEY (id);


--
-- TOC entry 2159 (class 2606 OID 28920)
-- Dependencies: 182 182
-- Name: pk_req; Type: CONSTRAINT; Schema: obj; Owner: -
--

ALTER TABLE ONLY req
    ADD CONSTRAINT pk_req PRIMARY KEY (id);


SET search_path = sec, pg_catalog;

--
-- TOC entry 2148 (class 2606 OID 28858)
-- Dependencies: 175 175
-- Name: pk_companies; Type: CONSTRAINT; Schema: sec; Owner: -
--

ALTER TABLE ONLY companies
    ADD CONSTRAINT pk_companies PRIMARY KEY (id);


--
-- TOC entry 2151 (class 2606 OID 28860)
-- Dependencies: 176 176 176
-- Name: pk_users; Type: CONSTRAINT; Schema: sec; Owner: -
--

ALTER TABLE ONLY users
    ADD CONSTRAINT pk_users PRIMARY KEY (user_name, company);


SET search_path = test, pg_catalog;

--
-- TOC entry 2153 (class 2606 OID 28862)
-- Dependencies: 178 178
-- Name: pk_results; Type: CONSTRAINT; Schema: test; Owner: -
--

ALTER TABLE ONLY results
    ADD CONSTRAINT pk_results PRIMARY KEY (id);


--
-- TOC entry 2155 (class 2606 OID 28864)
-- Dependencies: 180 180
-- Name: pk_tests; Type: CONSTRAINT; Schema: test; Owner: -
--

ALTER TABLE ONLY tests
    ADD CONSTRAINT pk_tests PRIMARY KEY (id);


SET search_path = dic, pg_catalog;

--
-- TOC entry 2140 (class 1259 OID 28865)
-- Dependencies: 1533 171
-- Name: fki_param_values_param; Type: INDEX; Schema: dic; Owner: -
--

CREATE INDEX fki_param_values_param ON param_values USING btree (param);


SET search_path = obj, pg_catalog;

--
-- TOC entry 2143 (class 1259 OID 28866)
-- Dependencies: 172
-- Name: fki_objects_cmp; Type: INDEX; Schema: obj; Owner: -
--

CREATE INDEX fki_objects_cmp ON objects USING btree (cmp);


--
-- TOC entry 2144 (class 1259 OID 28867)
-- Dependencies: 172
-- Name: fki_objects_parent; Type: INDEX; Schema: obj; Owner: -
--

CREATE INDEX fki_objects_parent ON objects USING btree (parent);


--
-- TOC entry 2156 (class 1259 OID 28926)
-- Dependencies: 182
-- Name: fki_req_obj; Type: INDEX; Schema: obj; Owner: -
--

CREATE INDEX fki_req_obj ON req USING btree (obj);


--
-- TOC entry 2157 (class 1259 OID 28932)
-- Dependencies: 182
-- Name: fki_req_parent; Type: INDEX; Schema: obj; Owner: -
--

CREATE INDEX fki_req_parent ON req USING btree (parent);


SET search_path = sec, pg_catalog;

--
-- TOC entry 2149 (class 1259 OID 28868)
-- Dependencies: 176
-- Name: fki_users_company; Type: INDEX; Schema: sec; Owner: -
--

CREATE INDEX fki_users_company ON users USING btree (company);


SET search_path = app, pg_catalog;

--
-- TOC entry 2160 (class 2606 OID 28869)
-- Dependencies: 169 1375 168 2136
-- Name: fk_actions_tree; Type: FK CONSTRAINT; Schema: app; Owner: -
--

ALTER TABLE ONLY actions
    ADD CONSTRAINT fk_actions_tree FOREIGN KEY (tree) REFERENCES modes(tree) ON UPDATE CASCADE ON DELETE CASCADE;


SET search_path = dic, pg_catalog;

--
-- TOC entry 2161 (class 2606 OID 28874)
-- Dependencies: 2138 171 170 1375
-- Name: fk_param_values_param; Type: FK CONSTRAINT; Schema: dic; Owner: -
--

ALTER TABLE ONLY param_values
    ADD CONSTRAINT fk_param_values_param FOREIGN KEY (param) REFERENCES param_def(tree) ON UPDATE CASCADE ON DELETE CASCADE;


SET search_path = obj, pg_catalog;

--
-- TOC entry 2162 (class 2606 OID 28879)
-- Dependencies: 2147 175 172
-- Name: fk_objects_cmp; Type: FK CONSTRAINT; Schema: obj; Owner: -
--

ALTER TABLE ONLY objects
    ADD CONSTRAINT fk_objects_cmp FOREIGN KEY (cmp) REFERENCES sec.companies(id) ON UPDATE CASCADE ON DELETE CASCADE DEFERRABLE;


--
-- TOC entry 2163 (class 2606 OID 28884)
-- Dependencies: 172 2145 172
-- Name: fk_objects_parent; Type: FK CONSTRAINT; Schema: obj; Owner: -
--

ALTER TABLE ONLY objects
    ADD CONSTRAINT fk_objects_parent FOREIGN KEY (parent) REFERENCES objects(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- TOC entry 2165 (class 2606 OID 28921)
-- Dependencies: 172 2145 182
-- Name: fk_req_obj; Type: FK CONSTRAINT; Schema: obj; Owner: -
--

ALTER TABLE ONLY req
    ADD CONSTRAINT fk_req_obj FOREIGN KEY (obj) REFERENCES objects(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- TOC entry 2166 (class 2606 OID 28927)
-- Dependencies: 182 182 2158
-- Name: fk_req_parent; Type: FK CONSTRAINT; Schema: obj; Owner: -
--

ALTER TABLE ONLY req
    ADD CONSTRAINT fk_req_parent FOREIGN KEY (parent) REFERENCES req(id) ON UPDATE CASCADE ON DELETE CASCADE;


SET search_path = sec, pg_catalog;

--
-- TOC entry 2164 (class 2606 OID 28894)
-- Dependencies: 2147 175 176
-- Name: fk_users_company; Type: FK CONSTRAINT; Schema: sec; Owner: -
--

ALTER TABLE ONLY users
    ADD CONSTRAINT fk_users_company FOREIGN KEY (company) REFERENCES companies(id) ON UPDATE CASCADE ON DELETE CASCADE;


-- Completed on 2012-03-08 23:16:58 MSK

--
-- PostgreSQL database dump complete
--

