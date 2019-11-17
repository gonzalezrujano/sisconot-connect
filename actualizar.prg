? 'Epa'
CREATE CONNECTION prueba
? SQLCONNECT('sisconot', 'leonardo', 'leo8721208', prueba)

IF faltanMarcasTiempo() THEN
	rellenarMarcasRestantes()
ENDIF

FUNCTION rellenarMarcasRestantes()
	REPLACE update_at WITH DATETIME() WHILE EMPTY(update_at)
ENDFUNC

* Verificar si todos los registros tienen marca de tiempo
FUNCTION faltanMarcasTiempo()
	USE datos
	LOCATE FOR EMPTY(update_at)
	RETURN FOUND()
ENDFUNC