-- Function: test_doc_table(text)

-- DROP FUNCTION test_doc_table(text);

CREATE OR REPLACE FUNCTION test_doc_table(text)
  RETURNS text AS
$BODY$select 
	json_row(
		json_element('Text','Платёжные поручения'::text),
		json_element('Table',
			json_row(
				json_element('num',
					json_row(
						json_element('Type','integer'::text),
						json_element('Text','Номер'::text)
					)
				),
				json_element('date',
					json_row(
						json_element('Type','date'::text),
						json_element('Text','Дата'::text)
					)
				),
				json_element('sum',
					json_row(
						json_element('Type','sum'::text),
						json_element('Text','Сумма'::text)
					)
				)
			)
		)
	)

					
				$BODY$
  LANGUAGE sql VOLATILE
  COST 100;
ALTER FUNCTION test_doc_table(text)
  OWNER TO postgres;
