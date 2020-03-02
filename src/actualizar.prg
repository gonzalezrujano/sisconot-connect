*
* Tablas a actualizacion frecuente
*
* var (Array)
*
DECLARE aTablas (3)

*
* Campos de relacion de las tablas
*
* var (Array)
*
DECLARE aCamposRelacion (3)

*
* Tablas de configuracion
*
* var (Array)
*
DECLARE aTablasConfiguracion(2)


*
* Numero de columnas a actualizar
*
* var (Numerico)
*
nCantidadColumnas = 50

*
* Columnas a actualizar
*
* var (Array)
*
DECLARE aColumnas (nCantidadColumnas)

*
* Registro nuevo a insertar
*
* var (Cadena)
*
cRegistro = ''

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

* Actualizar tablas de configuracion
IF .F. THEN
	desplegarTablasDeConfiguracion()
* Actualizar tablas de registros
ELSE
	desplegarNuevosRegistros()
	actualizarRegistrosRemotos()
ENDIF

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

	aTablasConfiguracion(1) = 'seccion'
	aTablasConfiguracion(2) = 'asignatu'
	
	aTablas(1) = 'datos'
	aTablas(2) = 'profesor'
	aTablas(3) = obtenerNombreDeMatriculaActual()
	
	aCamposRelacion(1) = 'cedula'
	aCamposRelacion(2) = 'cedula'
	aCamposRelacion(3) = 'ced'
	
ENDFUNC

*
* Obtener nombre de la tabla de matricula del regimen
*
* Retorno (caracter)
*
FUNCTION obtenerNombreDeMatriculaActual

	* DEFINIR variable de entorno e identificacion de regimen actual *
	esRegimenDeAdultos = .T.
	
	IF (esRegimenDeAdultos) THEN
		cTabla = 'MA' + obtenerLapso()
	ENDIF
	
	RETURN cTabla
	
ENDFUNC

*
* Obtener numero de lapso del periodo (01/02)
*
* Retorno (caracter)
*
FUNCTION obtenerLapso()

	nMesActual = MONTH(DATE())
	
	* Se encuentra en el Lapso 2 (Entre Marzo y Agosto)
	IF (nMesActual >= 3 AND nMesActual <= 7) THEN
		cPeriodo = '02'
		cAnio    = LTRIM(STR((YEAR(DATE()) - 1)))
	* Se encuentra en el Lapso 1 (Entre Septiembre y Febrero)
	ELSE
		cPeriodo = '01'
		cAnio    = LTRIM(STR(YEAR(DATE())))
	ENDIF
	
	cLapso = cPeriodo + cAnio
	
	RETURN '012017'
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
* Subir registros de configuracion
*
* Retorno (void)
*
FUNCTION desplegarTablasDeConfiguracion()

	* SQLSETPROP(nConexion, "Transactions", 2)
	
	FOR EACH tablaLocal IN aTablasConfiguracion
		
		obtenerColumnas(tablaLocal)
		
		* Eliminar registros remotos
		SQLEXEC(nConexion, 'DELETE FROM ' + tablaLocal)
		
		* Insertar registros locales
		SELECT * FROM (tablaLocal)
		
		* Ingresar registros en la tabla remota
		DO WHILE !EOF()
		
			obtenerDatosDelRegistros()
		
			* Construir consulta de actualizacion SQL
			cConsulta = 'INSERT INTO ' + tablaLocal + ' VALUES(' + cRegistro + ')'
			
			* Enviar y guardar los cambios
			SQLEXEC(nConexion, cConsulta)
			
			SKIP
			* EXIT
			
		ENDDO
		
		SQLCOMMIT(nConexion)
		
	ENDFOR
	
	* SQLSETPROP(nConexion, "Transactions", 1)
	
ENDFUNC

*
* Subir registros nuevos de la tabla local a la remota
*
* Retorno (void)
*
FUNCTION desplegarNuevosRegistros

	nCont = 1

	FOR EACH tablaLocal IN aTablas
	
		obtenerColumnas(tablaLocal)
		
		* Consultar registros externos
		SQLEXEC(nConexion, 'SELECT * FROM ' + tablaLocal, 'tablaRemota')
		
		cCampoRelacion = aCamposRelacion(nCont)
		
		* Obtener registros en la tabla local que no existan en la remota
		SELECT * FROM (tablaLocal); 
		WHERE NOT EXISTS (SELECT tablaRemota.&cCampoRelacion FROM tablaRemota;
		WHERE tablaRemota.&cCampoRelacion = &tablaLocal..&cCampoRelacion)
		
		* Ingresar registros en la tabla remota
		DO WHILE !EOF()
		
			obtenerDatosDelRegistros()
		
			* Construir consulta de actualizacion SQL
			cConsulta = 'INSERT INTO ' + tablaLocal + ' VALUES(' + cRegistro + ')'
			
			* Enviar y guardar los cambios
			SQLEXEC(nConexion, cConsulta)
			SQLCOMMIT(nConexion)
			
			SKIP
			
		ENDDO
		
		nCont = nCont + 1
		
	ENDFOR
	
ENDFUNC

*
* Actualizar registros desactualizados en tabla remota
*
* Retorno (void)
*
FUNCTION actualizarRegistrosRemotos

	nCont = 1

	FOR EACH tablaLocal IN aTablas
		
		obtenerColumnas(tablaLocal)
		
		* Consultar registros externos
		SQLEXEC(nConexion, 'SELECT * FROM ' + tablaLocal, 'tablaRemota')
		
		cCampoRelacion = aCamposRelacion(nCont)
		
		* Obtener registros desactualizados
		SELECT &tablaLocal..* FROM (tablaLocal), tablaRemota;
			WHERE &tablaLocal..&cCampoRelacion = tablaRemota.&cCampoRelacion;
			AND &tablaLocal..update_at > tablaRemota.update_at
			
		* Actualizar registros en la tabla remota
		DO WHILE !EOF()
		
			* Iterar en columnas de la tabla
			FOR gnContador = 2 TO nCantidadColumnas
			
				columna = aColumnas(gnContador)
				
				* Salir al final de lista de columnas
				IF EMPTY(columna)
					EXIT
				ENDIF
				
				* Convertir columna de tipo DateTime
				IF TYPE(columna) = 'T'
					nuevoValor = TTOC(&columna, 2)
				ELSE
					nuevoValor = RTRIM(&columna)
				ENDIF
				
				* Construir consulta de actualizacion SQL
				cConsulta = 'UPDATE ' + tablaLocal + ' SET ' + columna + ' = "' + nuevoValor + '" WHERE cedula = "' + cedula + '"'
				
				* Enviar y guardar los cambios
				SQLEXEC(nConexion, cConsulta)
				SQLCOMMIT(nConexion)
				
			ENDFOR
			
			SKIP
			
		ENDDO
		
		nCont = nCont + 1
		
	ENDFOR
	
ENDFUNC

*
* Extraer columnas de la tabla remota
*
* Retorno (void)
*
FUNCTION obtenerColumnas
	LPARAMETERS tabla
	
	* Reestablecer variable de columnas
	FOR gnContador = 1 TO nCantidadColumnas
		aColumnas(gnContador) = .F.
	ENDFOR
	
	* Obtener columnas del origen de datos	
	SQLCOLUMNS(nConexion, tabla, 'FOXPRO', 'columna')
	
	* Guardar columnas
	nContador = 1
	DO WHILE !EOF()
		aColumnas(nContador) = columna.Field_name
		nContador = nContador + 1
		SKIP
	ENDDO

ENDFUNC

*
* Construir cadena CSV con los valores de las columnas requeridas
*
* Retorno (void)
*
FUNCTION obtenerDatosDelRegistros()

	* Reestablecer variable de registro
	cRegistro = 'NULL, '
	
	* Iterar en columnas de la tabla
	FOR gnContador = 2 TO nCantidadColumnas
	
		* Obtener los nombres de la columna
		columna = aColumnas(gnContador)
		siguienteColumna = aColumnas(gnContador + 1)
		
		* Convertir el valor a cadena
		DO CASE
			CASE TYPE(columna) = 'T'
				valor = '"' + DTOC(&columna) + '"'
			CASE TYPE(columna) = 'N'
				valor = LTRIM(STR(&columna))
			CASE (TYPE(columna) = 'C')
				valor = '"' + RTRIM(&columna) + '"'
			OTHERWISE
				valor = 'NULL'
		ENDCASE
		
		* Concatenar valor del registro
		IF EMPTY(siguienteColumna)
			cRegistro = cRegistro + valor
			EXIT
		ELSE
			cRegistro = cRegistro + valor + ','
		ENDIF
		
	ENDFOR
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