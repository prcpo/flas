-- Function: test.py()
/* Тест создания документа из PostgreSQL
требуется:
- установить в PostgreSQL аддон pl/python (plpythou)
- в ОС установить LibreOffice
- в ОС установить Python
- в Python установить модуль uno
- запустить LibreOffice в фоновом режиме с прослушкой 2002 порта

пример для Ubuntu здесь:
http://blog.swlogic.eu/2011/06/03/rabota-s-openoffice-libreoffice-iz-python/

Пример по обращению к базе данных из pl/python в официальной документации на postgresql.org или здесь:
http://postgresql.ru.net/manual/plpython-database.html#AEN57491

*/


CREATE OR REPLACE FUNCTION test.py()
  RETURNS text AS
$BODY$try:
	import sys, uno

	local = uno.getComponentContext()
	resolver = local.ServiceManager.createInstanceWithContext("com.sun.star.bridge.UnoUrlResolver", local)
	context = resolver.resolve("uno:socket,host=localhost,port=2002;urp;StarOffice.ComponentContext")
	desktop = context.ServiceManager.createInstanceWithContext("com.sun.star.frame.Desktop", context)

	url = "private:factory/swriter"
	document = desktop.loadComponentFromURL(url, "_blank", 0, ())
	cursor = document.Text.createTextCursor()

	text = "This text is being added to openoffice using python and uno package.\n\n"
	document.Text.insertString(cursor, text, 0)

	document.storeAsURL("file:///home/user/tmp/002.odt",())
	document.dispose()

#  sys.exit()



	return "File is OK"
except Exception, e:
	import traceback
	plpy.info(traceback.format_exc())
	return traceback.format_exc()$BODY$
  LANGUAGE plpythonu VOLATILE
  COST 100;
ALTER FUNCTION test.py()
  OWNER TO postgres;
