-- Function: obj.req_get(bigint)

-- DROP FUNCTION obj.req_get(bigint);

CREATE OR REPLACE FUNCTION obj.req_get(_id bigint)
  RETURNS text AS
$BODY$declare
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
        json.element('UID', _id::text) ||
        array(
          select obj.req_get(id)
          from obj.req
          where parent = _id or obj = _id
        )
      );
  end if;
  return json.element(_name, _res);
end;$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;
ALTER FUNCTION obj.req_get(bigint)
  OWNER TO postgres;
