-- Function: obj.get(bigint)

-- DROP FUNCTION obj.get(bigint);

CREATE OR REPLACE FUNCTION obj.get(_id bigint)
  RETURNS text AS
$BODY$select
  json.get(
    json.element('Type', o.code::text) ||
    (json.element('UID', o.id::text) ||
    array(
              select obj.req_get(id)
              from obj.req
              where obj = o.id
              and parent is null
    ))
  )
from obj.objects o
where o.id = $1$BODY$
  LANGUAGE sql VOLATILE
  COST 100;
ALTER FUNCTION obj.get(bigint)
  OWNER TO postgres;
