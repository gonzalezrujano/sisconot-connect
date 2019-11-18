* Definir variables de entorno para conexion (nombre de ODBC)
* Definir env OR file config de tablas a actualizar
* Documentar
* ----------------------------------------------------
* - Verificar existencia de campo marca de tiempo en las tablas (Local)
* - Verificar marcas de tiempo vacias en tablas
* - Copiar registros inexistentes en tabla remota

*CREATE CURSOR tablaLocal

*CREATE CURSOR tablaRemota

*
* Numero de tablas a actualizar
*
* var (Numerico)
*
nCantidadTablas = 5

*
* Tablas a actualizar
*
* var (Array)
*
DECLARE aTablas (nCantidadTablas)

*
* Identificador de conexion remota
*
* var (Numerico)
*
nConexion = 0

establecerConexion()

* Verificar conexion remota
IF (nConexion <= 0) THEN
	= MESSAGEBOX('No se puede conectar con el servidor', 16, 'Error en conexión')
ENDIF

obtenerTablas()

IF NOT existenLasTablas() THEN
	= MESSAGEBOX('Verifique que todas las tablas existan en los entornos correspondientes', 16, 'Error en configuración')
	salir()
ENDIF

&& CONTINUAR AQUI

salir()

*
* Establecer conexion remota
*
* Retorno (void)
*
FUNCTION establecerConexion
	nConexion = SQLCONNECT('sisconot')
ENDFUNC

*
* Obtener tablas a Actualizar
*
* Retorno (void)
*
FUNCTION obtenerTablas
	aTablas(1) = 'datos'
	aTablas(2) = 'record'
ENDFUNC

*
* Verificar existencia de tablas
*
* Retorno (boolean)
*
FUNCTION existenLasTablas

  	FOR EACH tabla IN aTablas
		IF NOT (EMPTY(tabla)) THEN
		 	IF NOT existeEnLocal(tabla) OR NOT existeEnRemoto(tabla) THEN
			 	RETURN .F.
			ENDIF
		ENDIF
	ENDFOR
	
	RETURN .T.
ENDFUNC

*
* Verificar existencia de tabla en entorno Local
*
* Retorno (boolean)
*
FUNCTION existeEnLocal
	LPARAMETERS tabla
	
	RETURN INDBC(tabla, 'TABLE')
ENDFUNC

*
* Verificar existencia de tabla en entorno Remoto
*
* Retorno (boolean)
*
FUNCTION existeEnRemoto
	LPARAMETERS tabla
	
	RETURN NOT (SQLCOLUMNS(nConexion, tabla) = -1)
ENDFUNC

*
* Verificar si hay marcas de tiempo vacias en la tabla
*
* Retorno (void)
*
FUNCTION verificarMarcasVacias
	IF faltanMarcasTiempo() THEN
		rellenarMarcasRestantes()
	ENDIF
ENDFUNC

*
* Verificar si existen registros sin marca de tiempo
*
* Retorno (boolean)
*
FUNCTION faltanMarcasTiempo
	LOCATE FOR EMPTY(update_at)
	RETURN FOUND()
ENDFUNC

*
* Rellenar marcas de tiempo vacias (En registros)
*
* Retorno (void)
*
FUNCTION rellenarMarcasRestantes
	REPLACE update_at WITH DATETIME() WHILE EMPTY(update_at)
ENDFUNC

*
* Terminar proceso
*
* Retorno (void)
*
FUNCTION salir
	SQLDISCONNECT(nConexion)
	CANCEL
ENDFUNC