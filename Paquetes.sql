--Liz Castiblanco
--Faber Navia
--Sofia Moreno
--Juan Latorre
--Yeimy Joaqui



-------------------------------------------------------------
-------------------------------------------------------------
-- INDICES
-------------------------------------------------------------
-------------------------------------------------------------


-------------------------------------------------------------
--  INDICES PARA LA TABLA PRESTAMO
-------------------------------------------------------------

--PRESTAMO
--indexado por estado, optimiza las consultas frecuentes al buscar prestamos y 
--filtrarlos por estados (activos/incativos)
CREATE BITMAP INDEX idx_estado_prestamo ON Empeno(estadoPrestamo);
--comprueban la fecha de vencimiento del prestamo
CREATE INDEX idx_fecha_fin ON Empeno(fechaFin);
--consulta indices creados
SELECT index_name FROM user_indexes WHERE table_name = 'EMPENIO';


--ARTICULO
-- Índice para acelerar búsquedas por estado
CREATE INDEX idx_articulo_estado ON Articulo(estadoArticulo);

--Consulto el indice creado
SELECT index_name FROM user_indexes WHERE table_name = 'ARTICULO';


--CLIENTE
-- Índice para acelerar búsquedas por tipo de identidad
CREATE INDEX idx_buscar_Cliente_tipoIdentidad ON Cliente(tipoIdentidad);

--Consulto el indice creado
SELECT index_name FROM user_indexes WHERE table_name = 'CLIENTE';


--ADMINISTADOR
CREATE INDEX idx_poliza_administrador
ON Poliza (numeroIdentidadAdministrador);


-- DEVOLUCION
SET SERVEROUTPUT ON
-- Índice compuesto que mejora ambas condiciones del WHERE
CREATE INDEX idx_devolucion_cliente_fecha
ON Devolucion(numeroIdentidadCliente, fechaDevolucion);

SELECT 
    numeroIdentidadCliente,
    idArticulo,
    numConvenio,
    fechaDevolucion
FROM 
    Devolucion
WHERE 
    numeroIdentidadCliente = 87654321
    AND fechaDevolucion BETWEEN TO_DATE('2025-01-01', 'YYYY-MM-DD') 
                            AND TO_DATE('2025-05-19', 'YYYY-MM-DD')
ORDER BY 
    fechaDevolucion DESC;
/

----------------------------------------------------------------------------
----------------------------------------------------------------------------
--------PAQUETES ORACLE - SISTEMA GESTOR DE EMPENIO
----------------------------------------------------------------------------
----------------------------------------------------------------------------



--Crea un paquete PL/SQL llamado pkg_articulo que contenga lo siguiente:
--Un procedimiento llamado listar_articulos_por_estado, que reciba como 
--parámetro un estado ('optimo', 'funcionable' o 'defectuoso') 
--y muestre por consola los artículos que se encuentran en ese
--estado, mostrando su id, nombre y valor.
--Y una función llamada contar_articulos_por_estado, que reciba un estado como 
--parámetro y retorne la cantidad total de artículos que tienen ese estado.


CREATE OR REPLACE PACKAGE pkg_articulo AS

  PROCEDURE listar_articulos_por_estado(p_estado IN VARCHAR2);

  FUNCTION contar_articulos_por_estado(p_estado IN VARCHAR2) RETURN NUMBER;
  
END pkg_articulo;
/


CREATE OR REPLACE PACKAGE BODY pkg_articulo AS

  PROCEDURE listar_articulos_por_estado(p_estado IN VARCHAR2) IS
    CURSOR c_articulos IS
      SELECT id, nombre, valor FROM Articulo WHERE estadoArticulo = p_estado;
    v_id Articulo.id%TYPE;
    v_nombre Articulo.nombre%TYPE;
    v_valor Articulo.valor%TYPE;
  BEGIN
    OPEN c_articulos;
    LOOP
      FETCH c_articulos INTO v_id, v_nombre, v_valor;
      EXIT WHEN c_articulos%NOTFOUND;
      DBMS_OUTPUT.PUT_LINE('ID: ' || v_id || ', Nombre: ' || v_nombre || ', Valor: ' || v_valor);
    END LOOP;
    CLOSE c_articulos;
  END;

  FUNCTION contar_articulos_por_estado(p_estado IN VARCHAR2) RETURN NUMBER IS
    v_total NUMBER := 0;
  BEGIN
    SELECT COUNT(*) INTO v_total FROM Articulo WHERE estadoArticulo = p_estado;
    RETURN v_total;
  END;

END pkg_articulo;
/


-- SET SERVEROUTPUT ON

-- --listar articulos ('óptimo', 'funcionable' o 'defectuoso')
-- BEGIN
--   pkg_articulo.listar_articulos_por_estado('optimo');
-- END;


-- -- Contar artículos defectuosos
-- DECLARE
--   v_cantidad NUMBER;
-- BEGIN
--   v_cantidad := pkg_articulo.contar_articulos_por_estado('defectuoso');
--   DBMS_OUTPUT.PUT_LINE('Total defectuosos: ' || v_cantidad);
-- END;

















-- Paquete para registrar devoluciones.
-- Verifica el empeño, inserta la devolución y actualiza el estado del préstamo.
SET SERVEROUTPUT ON
-- ESPECIFICACIÓN DEL PAQUETE
CREATE OR REPLACE PACKAGE pkg_devoluciones
AS
PROCEDURE registrarDevolucion(
    p_numIdentidadCliente IN Devolucion.numeroIdentidadCliente%TYPE,
    p_idArticulo          IN Devolucion.idArticulo%TYPE,
    p_numConvenio         IN Devolucion.numConvenio%TYPE,
    p_fechaDevolucion     IN Devolucion.fechaDevolucion%TYPE
);
END pkg_devoluciones;
/

-- CUERPO DEL PAQUETE
CREATE OR REPLACE PACKAGE BODY pkg_devoluciones
AS
PROCEDURE registrarDevolucion(
    p_numIdentidadCliente IN Devolucion.numeroIdentidadCliente%TYPE,
    p_idArticulo          IN Devolucion.idArticulo%TYPE,
    p_numConvenio         IN Devolucion.numConvenio%TYPE,
    p_fechaDevolucion     IN Devolucion.fechaDevolucion%TYPE
)
IS
v_count NUMBER;
BEGIN
-- 1. Verificar que el empeño exista
    SELECT COUNT(*)
    INTO v_count
    FROM Empeno
    WHERE numeroIdentidadCliente = p_numIdentidadCliente
    AND idArticulo = p_idArticulo;

    IF v_count = 0 THEN
        RAISE_APPLICATION_ERROR(-20001, 'No existe un empeño para este cliente y artículo.');
    END IF;
    
-- 2. Insertar en Devolucion
    INSERT INTO Devolucion (numeroIdentidadCliente, idArticulo, numConvenio, fechaDevolucion)
    VALUES (p_numIdentidadCliente, p_idArticulo, p_numConvenio, p_fechaDevolucion);
    
-- 3. Actualizar estado del préstamo a 'devuelto'
    UPDATE Empeno
    SET estadoPrestamo = 'devuelto'
    WHERE numeroIdentidadCliente = p_numIdentidadCliente
    AND idArticulo = p_idArticulo;
    COMMIT;
    EXCEPTION
        WHEN DUP_VAL_ON_INDEX THEN
            RAISE_APPLICATION_ERROR(-20002, 'Ya existe una devolución registrada con ese convenio para ese cliente y artículo.');
        WHEN OTHERS THEN
            ROLLBACK;
            RAISE;
    END registrarDevolucion;
END pkg_devoluciones;
/

-----------------------------------------------------------------------------------------------------
--EJECUCIÓN
-----------------------------------------------------------------------------------------------------
-- BEGIN
--     pkg_devoluciones.registrarDevolucion(12345678,1,9001,TO_DATE('2025-05-14', 'YYYY-MM-DD'));
-- END;

-- DELETE FROM Devolucion
-- WHERE numeroIdentidadCliente = 12345678
--   AND idArticulo = 1
--   AND numConvenio = 9001;
























--------------------------------------------------------------------------------
--PAQUETE PARA GENERAR ESTADISTICAS DE EMPENIO----------------------------------
--------------------------------------------------------------------------------
--proposito: Proporcionar estadísticas sobre los empeños para apoyar decisiones.


------------------------------------------------------------------
-----------ESPECIFICACION DEL PAQUETE-----------------------------
------------------------------------------------------------------
CREATE OR REPLACE PACKAGE pkg_estadisticas_empeno AS
    /*
    * Propósito del paquete:
    * Ofrecer funciones y procedimientos que permitan consultar estadísticas clave 
    * del sistema de empeños, tales como intereses promedio, porcentaje de
    * empeños activos, duración promedio de empeños, y estadísticas generales. 
    */
    
    -- Definición de excepciones personalizadas
    e_no_data_found EXCEPTION;
    e_invalid_parameter EXCEPTION;
    e_process_error EXCEPTION;
    
    -- Definición de tipos para retornar datos
    TYPE rec_interes_promedio IS RECORD (
        estado_prestamo VARCHAR2(20),
        interes_promedio NUMBER
    );
    
    TYPE tbl_interes_promedio IS TABLE OF rec_interes_promedio;
    
    TYPE rec_cliente_empeno IS RECORD (
        numero_identidad INTEGER,
        nombre VARCHAR2(100),
        cantidad_empenos INTEGER
    );
    
    TYPE tbl_cliente_empeno IS TABLE OF rec_cliente_empeno;
    
    -- Definición de funciones
    FUNCTION fn_porcentaje_empenos_activos RETURN NUMBER;
    FUNCTION fn_promedio_duracion_empenos RETURN NUMBER;
    
    -- Definición de procedimientos
    PROCEDURE sp_mostrar_estadisticas_generales(
        p_total_empenos OUT NUMBER,
        p_total_activos OUT NUMBER,
        p_total_inactivos OUT NUMBER,
        p_total_devueltos OUT NUMBER,
        p_porcentaje_activos OUT NUMBER,
        p_interes_promedio OUT NUMBER,
        p_cliente_mas_empenos OUT VARCHAR2
    );
    
    PROCEDURE sp_listar_top_n_clientes(
        p_top_n IN NUMBER,
        p_resultado OUT tbl_cliente_empeno
    );
    
    PROCEDURE sp_listar_interes_promedio_por_estado(
        p_resultado OUT tbl_interes_promedio
    );
    
END pkg_estadisticas_empeno;
/

------------------------------------------------------------------
-----------CUERPO DEL PAQUETE-------------------------------------
------------------------------------------------------------------
CREATE OR REPLACE PACKAGE BODY pkg_estadisticas_empeno AS
    
    /*
    * Función que calcula el porcentaje de empeños activos respecto al total
    * Retorna: NUMBER - Porcentaje (de 0 a 100)
    */
    FUNCTION fn_porcentaje_empenos_activos RETURN NUMBER IS
        v_total NUMBER := 0;
        v_activos NUMBER := 0;
        v_porcentaje NUMBER := 0;
    BEGIN
        -- Obtenemos el total de empeños
        SELECT COUNT(*) INTO v_total FROM Empeno;
        
        -- Validamos si hay empeños registrados
        IF v_total = 0 THEN
            RAISE e_no_data_found;
        END IF;
        
        -- Obtenemos el total de empeños activos
        SELECT COUNT(*) INTO v_activos 
        FROM Empeno 
        WHERE estadoPrestamo = 'activo';
        
        -- Calculamos el porcentaje
        v_porcentaje := ROUND((v_activos / v_total) * 100, 2);
        
        RETURN v_porcentaje;
    EXCEPTION
        WHEN e_no_data_found THEN
            -- Si no hay datos, retornamos 0 como porcentaje
            RETURN 0;
        WHEN OTHERS THEN
            -- Registramos el error y relanzamos la excepción
            DBMS_OUTPUT.PUT_LINE('Error en fn_porcentaje_empenos_activos: ' || SQLERRM);
            RAISE e_process_error;
    END fn_porcentaje_empenos_activos;
    
 
    /*
    * Función que calcula el promedio de duración de empeños en días
    * Retorna: NUMBER - Promedio de días
    */
    FUNCTION fn_promedio_duracion_empenos RETURN NUMBER IS
        v_promedio_dias NUMBER := 0;
    BEGIN
        -- Calculamos el promedio de días entre fechaInicio y fechaFin
        SELECT NVL(ROUND(AVG(fechaFin - fechaInicio), 2), 0) INTO v_promedio_dias
        FROM Empeno
        WHERE fechaFin IS NOT NULL;
        
        RETURN v_promedio_dias;
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            -- Si no hay datos, retornamos 0
            RETURN 0;
        WHEN OTHERS THEN
            -- Registramos el error y relanzamos la excepción
            DBMS_OUTPUT.PUT_LINE('Error en fn_promedio_duracion_empenos: ' || SQLERRM);
            RAISE e_process_error;
    END fn_promedio_duracion_empenos;
    
    /*
    * Procedimiento que muestra estadísticas generales del sistema de empeños
    * Utilizando cursores básicos
    */
    PROCEDURE sp_mostrar_estadisticas_generales(
        p_total_empenos OUT NUMBER,
        p_total_activos OUT NUMBER,
        p_total_inactivos OUT NUMBER,
        p_total_devueltos OUT NUMBER,
        p_porcentaje_activos OUT NUMBER,
        p_interes_promedio OUT NUMBER,
        p_cliente_mas_empenos OUT VARCHAR2
    ) IS
        -- Cursor para obtener el total de empeños por estado
        CURSOR c_empenos_por_estado IS
            SELECT estadoPrestamo, COUNT(*) as cantidad
            FROM Empeno
            GROUP BY estadoPrestamo;
        
        -- Cursor para obtener el cliente con más empeños
        CURSOR c_cliente_mas_empenos IS
            SELECT c.nombre
            FROM Cliente c
            JOIN Empeno e ON c.numeroIdentidad = e.numeroIdentidadCliente
            GROUP BY c.numeroIdentidad, c.nombre
            ORDER BY COUNT(e.numeroIdentidadCliente) DESC
            FETCH FIRST 1 ROW ONLY;
        
        -- Variables locales
        v_estado VARCHAR2(20);
        v_cantidad NUMBER;
    BEGIN
        -- Inicializamos valores por defecto
        p_total_empenos := 0;
        p_total_activos := 0;
        p_total_inactivos := 0;
        p_total_devueltos := 0;
        p_porcentaje_activos := 0;
        p_interes_promedio := 0;
        p_cliente_mas_empenos := 'Sin datos';
        
        -- Obtenemos el total de empeños
        SELECT COUNT(*) INTO p_total_empenos FROM Empeno;
        
        -- Si no hay empeños, terminamos el procedimiento
        IF p_total_empenos = 0 THEN
            RETURN;
        END IF;
        
        -- Obtenemos el total de empeños por estado usando el cursor
        FOR r_estado IN c_empenos_por_estado LOOP
            CASE r_estado.estadoPrestamo
                WHEN 'activo' THEN p_total_activos := r_estado.cantidad;
                WHEN 'inactivo' THEN p_total_inactivos := r_estado.cantidad;
                WHEN 'devuelto' THEN p_total_devueltos := r_estado.cantidad;
                ELSE NULL;
            END CASE;
        END LOOP;
        
        -- Calculamos el porcentaje de empeños activos
        p_porcentaje_activos := fn_porcentaje_empenos_activos();
        
        -- Calculamos el interés promedio
        SELECT NVL(ROUND(AVG(interes), 2), 0) INTO p_interes_promedio
        FROM Empeno;
        
        
        -- Obtenemos el cliente con más empeños usando el cursor
        OPEN c_cliente_mas_empenos;
        FETCH c_cliente_mas_empenos INTO p_cliente_mas_empenos;
        IF c_cliente_mas_empenos%NOTFOUND THEN
            p_cliente_mas_empenos := 'Sin datos';
        END IF;
        CLOSE c_cliente_mas_empenos;
        
    EXCEPTION
        WHEN OTHERS THEN
            IF c_cliente_mas_empenos%ISOPEN THEN
                CLOSE c_cliente_mas_empenos;
            END IF;
            
            -- Registramos el error y relanzamos la excepción
            DBMS_OUTPUT.PUT_LINE('Error en sp_mostrar_estadisticas_generales: ' || SQLERRM);
            RAISE e_process_error;
    END sp_mostrar_estadisticas_generales;
    
    /*
    * Procedimiento que retorna los N clientes con más empeños
    * Usando un cursor parametrizado
    */
    PROCEDURE sp_listar_top_n_clientes(
        p_top_n IN NUMBER,
        p_resultado OUT tbl_cliente_empeno
    ) IS
        -- Cursor parametrizado para obtener los N clientes con más empeños
        CURSOR c_top_clientes(cp_top_n NUMBER) IS
            SELECT 
                c.numeroIdentidad as numero_identidad,
                c.nombre,
                COUNT(e.numeroIdentidadCliente) as cantidad_empenos
            FROM 
                Cliente c
            JOIN 
                Empeno e ON c.numeroIdentidad = e.numeroIdentidadCliente
            GROUP BY 
                c.numeroIdentidad, c.nombre
            ORDER BY 
                COUNT(e.numeroIdentidadCliente) DESC
            FETCH FIRST cp_top_n ROWS ONLY;
        
        -- Contador para la posición en la colección
        v_contador NUMBER := 0;
    BEGIN
        -- Validamos el parámetro de entrada
        IF p_top_n <= 0 THEN
            RAISE e_invalid_parameter;
        END IF;
        
        -- Inicializamos la tabla de resultados
        p_resultado := tbl_cliente_empeno();
        
        -- Recorremos el cursor y llenamos la colección
        FOR r_cliente IN c_top_clientes(p_top_n) LOOP
            -- Incrementamos el contador
            v_contador := v_contador + 1;
            
            -- Extendemos la colección
            p_resultado.EXTEND;
            
            -- Asignamos los valores
            p_resultado(v_contador) := rec_cliente_empeno(
                r_cliente.numero_identidad,
                r_cliente.nombre,
                r_cliente.cantidad_empenos
            );
        END LOOP;
        
        -- Verificamos si se encontraron resultados
        IF v_contador = 0 THEN
            RAISE e_no_data_found;
        END IF;
        
    EXCEPTION
        WHEN e_invalid_parameter THEN
            -- Lanzamos una excepción personalizada
            RAISE_APPLICATION_ERROR(-20001, 'El parámetro p_top_n debe ser mayor que cero.');
        WHEN e_no_data_found THEN
            -- No se encontraron datos, dejamos la colección vacía
            NULL;
        WHEN OTHERS THEN
            -- Registramos el error y relanzamos la excepción
            DBMS_OUTPUT.PUT_LINE('Error en sp_listar_top_n_clientes: ' || SQLERRM);
            RAISE e_process_error;
    END sp_listar_top_n_clientes;
    
    /*
    * Procedimiento que retorna el interés promedio por estado de préstamo
    * Usando un cursor básico
    */
    PROCEDURE sp_listar_interes_promedio_por_estado(
        p_resultado OUT tbl_interes_promedio
    ) IS
        -- Cursor básico para obtener el interés promedio por estado
        CURSOR c_interes_promedio IS
            SELECT 
                estadoPrestamo as estado_prestamo,
                ROUND(AVG(interes), 2) as interes_promedio
            FROM 
                Empeno
            GROUP BY 
                estadoPrestamo;
        
        -- Contador para la posición en la colección
        v_contador NUMBER := 0;
    BEGIN
        -- Inicializamos la tabla de resultados
        p_resultado := tbl_interes_promedio();
        
        -- Recorremos el cursor y llenamos la colección
        FOR r_interes IN c_interes_promedio LOOP
            -- Incrementamos el contador
            v_contador := v_contador + 1;
            
            -- Extendemos la colección
            p_resultado.EXTEND;
            
            -- Asignamos los valores
            p_resultado(v_contador) := rec_interes_promedio(
                r_interes.estado_prestamo,
                r_interes.interes_promedio
            );
        END LOOP;
        
        -- Verificamos si se encontraron resultados
        IF v_contador = 0 THEN
            RAISE e_no_data_found;
        END IF;
        
    EXCEPTION
        WHEN e_no_data_found THEN
            -- No se encontraron datos, dejamos la colección vacía
            NULL;
        WHEN OTHERS THEN
            -- Registramos el error y relanzamos la excepción
            DBMS_OUTPUT.PUT_LINE('Error en sp_listar_interes_promedio_por_estado: ' || SQLERRM);
            RAISE e_process_error;
    END sp_listar_interes_promedio_por_estado;
    
END pkg_estadisticas_empeno;
/




-- ------------------------------------------------
-- ------PRUEBAS Y USO DEL PAQUETE----------------
-- ------------------------------------------------


-- -- Bloque PL/SQL para probar el paquete pkg_estadisticas_empeno
-- SET SERVEROUTPUT ON;

-- DECLARE
--     -- Variables para probar funciones individuales
--     v_porcentaje_activos NUMBER;
--     v_duracion_promedio NUMBER;
    
--     -- Variables para probar sp_mostrar_estadisticas_generales
--     v_total_empenos NUMBER;
--     v_total_activos NUMBER;
--     v_total_inactivos NUMBER;
--     v_total_devueltos NUMBER;
--     v_porcentaje_activos_sp NUMBER;
--     v_interes_promedio NUMBER;
--     v_cliente_mas_empenos VARCHAR2(100);
    
--     -- Variables para probar sp_listar_top_n_clientes
--     v_top_clientes pkg_estadisticas_empeno.tbl_cliente_empeno;
--     v_cantidad_top NUMBER := 5; -- Mostrar los 5 clientes con más empeños
    
--     -- Variables para probar sp_listar_interes_promedio_por_estado
--     v_interes_por_estado pkg_estadisticas_empeno.tbl_interes_promedio;
    
-- BEGIN
--     DBMS_OUTPUT.PUT_LINE('=== PRUEBA DE PAQUETE pkg_estadisticas_empeno ===');
--     DBMS_OUTPUT.PUT_LINE('');
    
--     -- 1. Probar funciones individuales
--     DBMS_OUTPUT.PUT_LINE('1. PRUEBA DE FUNCIONES INDIVIDUALES:');
--     DBMS_OUTPUT.PUT_LINE('---------------------------------------');
    
--     -- Probar fn_porcentaje_empenos_activos
--     BEGIN
--         v_porcentaje_activos := pkg_estadisticas_empeno.fn_porcentaje_empenos_activos();
--         DBMS_OUTPUT.PUT_LINE('Porcentaje de empeños activos: ' || v_porcentaje_activos || '%');
--     EXCEPTION
--         WHEN OTHERS THEN
--             DBMS_OUTPUT.PUT_LINE('Error al ejecutar fn_porcentaje_empenos_activos: ' || SQLERRM);
--     END;
    
--     -- Probar fn_promedio_duracion_empenos
--     BEGIN
--         v_duracion_promedio := pkg_estadisticas_empeno.fn_promedio_duracion_empenos();
--         DBMS_OUTPUT.PUT_LINE('Duración promedio de empeños: ' || v_duracion_promedio || ' días');
--     EXCEPTION
--         WHEN OTHERS THEN
--             DBMS_OUTPUT.PUT_LINE('Error al ejecutar fn_promedio_duracion_empenos: ' || SQLERRM);
--     END;
    
--     DBMS_OUTPUT.PUT_LINE('');
    
--     -- 2. Probar sp_mostrar_estadisticas_generales
--     DBMS_OUTPUT.PUT_LINE('2. PRUEBA DE sp_mostrar_estadisticas_generales:');
--     DBMS_OUTPUT.PUT_LINE('---------------------------------------');
    
--     BEGIN
--         pkg_estadisticas_empeno.sp_mostrar_estadisticas_generales(
--             p_total_empenos => v_total_empenos,
--             p_total_activos => v_total_activos,
--             p_total_inactivos => v_total_inactivos,
--             p_total_devueltos => v_total_devueltos,
--             p_porcentaje_activos => v_porcentaje_activos_sp,
--             p_interes_promedio => v_interes_promedio,
--             p_cliente_mas_empenos => v_cliente_mas_empenos
--         );
        
--         DBMS_OUTPUT.PUT_LINE('Total de empeños: ' || v_total_empenos);
--         DBMS_OUTPUT.PUT_LINE('Total de empeños activos: ' || v_total_activos);
--         DBMS_OUTPUT.PUT_LINE('Total de empeños inactivos: ' || v_total_inactivos);
--         DBMS_OUTPUT.PUT_LINE('Total de empeños devueltos: ' || v_total_devueltos);
--         DBMS_OUTPUT.PUT_LINE('Porcentaje de empeños activos: ' || v_porcentaje_activos_sp || '%');
--         DBMS_OUTPUT.PUT_LINE('Interés promedio: ' || v_interes_promedio || '%');
--         DBMS_OUTPUT.PUT_LINE('Cliente con más empeños: ' || v_cliente_mas_empenos);
--     EXCEPTION
--         WHEN OTHERS THEN
--             DBMS_OUTPUT.PUT_LINE('Error al ejecutar sp_mostrar_estadisticas_generales: ' || SQLERRM);
--     END;
    
--     DBMS_OUTPUT.PUT_LINE('');
    
--     -- 3. Probar sp_listar_top_n_clientes
--     DBMS_OUTPUT.PUT_LINE('3. PRUEBA DE sp_listar_top_n_clientes:');
--     DBMS_OUTPUT.PUT_LINE('---------------------------------------');
    
--     BEGIN
--         pkg_estadisticas_empeno.sp_listar_top_n_clientes(
--             p_top_n => v_cantidad_top,
--             p_resultado => v_top_clientes
--         );
        
--         IF v_top_clientes.COUNT = 0 THEN
--             DBMS_OUTPUT.PUT_LINE('No se encontraron datos de clientes.');
--         ELSE
--             DBMS_OUTPUT.PUT_LINE('Top ' || v_cantidad_top || ' clientes con más empeños:');
--             DBMS_OUTPUT.PUT_LINE('ID       | Nombre                            | Cantidad Empeños');
--             DBMS_OUTPUT.PUT_LINE('---------|-----------------------------------|------------------');
            
--             FOR i IN 1..v_top_clientes.COUNT LOOP
--                 DBMS_OUTPUT.PUT_LINE(
--                     RPAD(v_top_clientes(i).numero_identidad, 9) || '| ' ||
--                     RPAD(v_top_clientes(i).nombre, 34) || '| ' ||
--                     RPAD(v_top_clientes(i).cantidad_empenos, 10)
--                 );
--             END LOOP;
--         END IF;
--     EXCEPTION
--         WHEN OTHERS THEN
--             DBMS_OUTPUT.PUT_LINE('Error al ejecutar sp_listar_top_n_clientes: ' || SQLERRM);
--     END;
    
--     DBMS_OUTPUT.PUT_LINE('');
    
--     -- 4. Probar sp_listar_interes_promedio_por_estado
--     DBMS_OUTPUT.PUT_LINE('4. PRUEBA DE sp_listar_interes_promedio_por_estado:');
--     DBMS_OUTPUT.PUT_LINE('---------------------------------------');
    
--     BEGIN
--         pkg_estadisticas_empeno.sp_listar_interes_promedio_por_estado(
--             p_resultado => v_interes_por_estado
--         );
        
--         IF v_interes_por_estado.COUNT = 0 THEN
--             DBMS_OUTPUT.PUT_LINE('No se encontraron datos de interés por estado.');
--         ELSE
--             DBMS_OUTPUT.PUT_LINE('Interés promedio por estado de préstamo:');
--             DBMS_OUTPUT.PUT_LINE('Estado              | Interés Promedio (%)');
--             DBMS_OUTPUT.PUT_LINE('--------------------|---------------------');
            
--             FOR i IN 1..v_interes_por_estado.COUNT LOOP
--                 DBMS_OUTPUT.PUT_LINE(
--                     RPAD(v_interes_por_estado(i).estado_prestamo, 20) || '| ' ||
--                     RPAD(v_interes_por_estado(i).interes_promedio, 10)
--                 );
--             END LOOP;
--         END IF;
--     EXCEPTION
--         WHEN OTHERS THEN
--             DBMS_OUTPUT.PUT_LINE('Error al ejecutar sp_listar_interes_promedio_por_estado: ' || SQLERRM);
--     END;
    
--     DBMS_OUTPUT.PUT_LINE('');
--     DBMS_OUTPUT.PUT_LINE('=== FIN DE PRUEBAS ===');
    
-- EXCEPTION
--     WHEN OTHERS THEN
--         DBMS_OUTPUT.PUT_LINE('Error general en el bloque de prueba: ' || SQLERRM);
-- END;
-- /
























-- Descripción del paquete pkg_gestion_clientes:
-- Este paquete agrupa procedimientos y funciones para la gestión de clientes en una base de datos. 
-- Permite insertar y actualizar registros de clientes, contar clientes según su tipo de identidad 
-- y listar clientes de forma general o filtrada. Utiliza cursores (básicos y parametrizados) 
-- para recorrer y mostrar datos de clientes, facilitando operaciones administrativas y consultas dentro del sistema.
-----------PAQUETE CON CURSORES---------------------
CREATE OR REPLACE PACKAGE pkg_gestion_clientes AS

    PROCEDURE insertar_cliente(
        p_numeroIdentidad IN INT,
        p_nombre IN VARCHAR2,
        p_tipoIdentidad IN VARCHAR2,
        p_numeroIdentidadAdministrador IN INT
    );

    PROCEDURE actualizar_cliente(
        p_numeroIdentidad IN INT,
        p_nombre IN VARCHAR2,
        p_tipoIdentidad IN VARCHAR2
    );

    FUNCTION contar_clientes_por_tipo(p_tipoIdentidad VARCHAR2) RETURN INT;

    PROCEDURE listar_todos_clientes;

    PROCEDURE listar_clientes_por_tipo(p_tipoIdentidad VARCHAR2);

END pkg_gestion_clientes;
/

CREATE OR REPLACE PACKAGE BODY pkg_gestion_clientes AS

    -- Cursor básico que retorna todos los clientes
    CURSOR cur_clientes IS
        SELECT numeroIdentidad, nombre, tipoIdentidad, numeroIdentidadAdministrador FROM Cliente;

    -- Cursor parametrizado que retorna clientes por tipo de identidad
    CURSOR cur_clientes_por_tipo(p_tipoIdentidad VARCHAR2) IS
        SELECT numeroIdentidad, nombre, tipoIdentidad, numeroIdentidadAdministrador
        FROM Cliente
        WHERE tipoIdentidad = p_tipoIdentidad;

    PROCEDURE insertar_cliente(
        p_numeroIdentidad IN INT,
        p_nombre IN VARCHAR2,
        p_tipoIdentidad IN VARCHAR2,
        p_numeroIdentidadAdministrador IN INT
    ) IS
    BEGIN
        INSERT INTO Cliente(numeroIdentidad, nombre, tipoIdentidad, numeroIdentidadAdministrador)
        VALUES(p_numeroIdentidad, p_nombre, p_tipoIdentidad, p_numeroIdentidadAdministrador);
    EXCEPTION
        WHEN DUP_VAL_ON_INDEX THEN
            RAISE_APPLICATION_ERROR(-20010, 'Ya existe un cliente con ese número de identidad.');
    END insertar_cliente;

    PROCEDURE actualizar_cliente(
        p_numeroIdentidad IN INT,
        p_nombre IN VARCHAR2,
        p_tipoIdentidad IN VARCHAR2
    ) IS
    BEGIN
        UPDATE Cliente
        SET nombre = p_nombre,
            tipoIdentidad = p_tipoIdentidad
        WHERE numeroIdentidad = p_numeroIdentidad;

        IF SQL%ROWCOUNT = 0 THEN
            RAISE_APPLICATION_ERROR(-20011, 'Cliente no encontrado para actualizar.');
        END IF;
    END actualizar_cliente;

    FUNCTION contar_clientes_por_tipo(p_tipoIdentidad VARCHAR2) RETURN INT IS
        v_count INT;
    BEGIN
        SELECT COUNT(*) INTO v_count FROM Cliente WHERE tipoIdentidad = p_tipoIdentidad;
        RETURN v_count;
    END contar_clientes_por_tipo;

    PROCEDURE listar_todos_clientes IS
    BEGIN
        FOR rec IN cur_clientes LOOP
            DBMS_OUTPUT.PUT_LINE('Cliente: ' || rec.numeroIdentidad || ' - ' || rec.nombre);
        END LOOP;
    END listar_todos_clientes;

    PROCEDURE listar_clientes_por_tipo(p_tipoIdentidad VARCHAR2) IS
    BEGIN
        FOR rec IN cur_clientes_por_tipo(p_tipoIdentidad) LOOP
            DBMS_OUTPUT.PUT_LINE('Cliente tipo ' || p_tipoIdentidad || ': ' || rec.numeroIdentidad || ' - ' || rec.nombre);
        END LOOP;
    END listar_clientes_por_tipo;

END pkg_gestion_clientes;
/



-- --------prueba del paquete--------
-- BEGIN
--     pkg_gestion_clientes.insertar_cliente(401, 'Laura Gómez', 'C.C.', NULL);
--     pkg_gestion_clientes.insertar_cliente(402, 'Andrés Martínez', 'T.I.', NULL);
--     pkg_gestion_clientes.insertar_cliente(403, 'María López', 'C.E.', NULL);
--     DBMS_OUTPUT.PUT_LINE('Clientes insertados correctamente.');
-- EXCEPTION
--     WHEN OTHERS THEN
--         DBMS_OUTPUT.PUT_LINE('Error al insertar cliente: ' || SQLERRM);
-- END;


-- BEGIN
--     pkg_gestion_clientes.actualizar_cliente(401, 'Laura Gómez Actualizada', 'C.C.');
--     DBMS_OUTPUT.PUT_LINE('Cliente actualizado correctamente.');
-- EXCEPTION
--     WHEN OTHERS THEN
--         DBMS_OUTPUT.PUT_LINE('Error al actualizar cliente: ' || SQLERRM);
-- END;


-- BEGIN
--     pkg_gestion_clientes.listar_todos_clientes;
-- END;

-- BEGIN
--     pkg_gestion_clientes.listar_clientes_por_tipo('C.C.');
-- END;


























-- Paquete para consultar pólizas y asociaciones con artículos (posee).
-- Permite listar y filtrar datos por tipo o cliente.

CREATE OR REPLACE PACKAGE pkg_poliza_posee AS
  PROCEDURE listar_polizas;

  FUNCTION obtener_polizas_por_tipo(p_tipo VARCHAR2) RETURN SYS_REFCURSOR;

  PROCEDURE listar_posee;

  FUNCTION obtener_posee_por_cliente(p_cliente_id NUMBER) RETURN SYS_REFCURSOR;
END pkg_poliza_posee;
/




CREATE OR REPLACE PACKAGE BODY pkg_poliza_posee AS

  PROCEDURE listar_polizas IS
    CURSOR c_polizas IS
      SELECT nit, tipo, costo, numeroidentidadadministrador FROM poliza;
  BEGIN
    FOR r IN c_polizas LOOP
      DBMS_OUTPUT.PUT_LINE('NIT: ' || r.nit || ', Tipo: ' || r.tipo ||
                           ', Costo: ' || r.costo || ', Admin ID: ' || r.numeroidentidadadministrador);
    END LOOP;
  END listar_polizas;

  FUNCTION obtener_polizas_por_tipo(p_tipo VARCHAR2) RETURN SYS_REFCURSOR IS
    c_resultado SYS_REFCURSOR;
  BEGIN
    OPEN c_resultado FOR
      SELECT * FROM poliza WHERE tipo = p_tipo;
    RETURN c_resultado;
  END obtener_polizas_por_tipo;

  PROCEDURE listar_posee IS
    CURSOR c_posee IS
      SELECT numeroidentidadcliente, idarticulo FROM posee;
  BEGIN
    FOR r IN c_posee LOOP
      DBMS_OUTPUT.PUT_LINE('Cliente: ' || r.numeroidentidadcliente || ', Artículo: ' || r.idarticulo);
    END LOOP;
  END listar_posee;

  FUNCTION obtener_posee_por_cliente(p_cliente_id NUMBER) RETURN SYS_REFCURSOR IS
    c_resultado SYS_REFCURSOR;
  BEGIN
    OPEN c_resultado FOR
      SELECT * FROM posee WHERE numeroidentidadcliente = p_cliente_id;
    RETURN c_resultado;
  END obtener_posee_por_cliente;

END pkg_poliza_posee;
/

--------------------------------------------------- DATOS PARA PROBAR----------------------------------------------------------------------------
-- SET SERVEROUTPUT ON;

-- -- Insertar datos en POLIZA
-- DELETE FROM poliza WHERE nit = 555001;
-- DELETE FROM poliza WHERE nit = 555002;

-- INSERT INTO poliza (nit, tipo, costo, numeroidentidadadministrador)
-- VALUES (555001, 'Vida', 500.00, 101);

-- INSERT INTO poliza (nit, tipo, costo, numeroidentidadadministrador)
-- VALUES (555002, 'Auto', 800.00, 102);

-- -- Insertar datos en POSEE
-- DELETE FROM posee WHERE numeroidentidadcliente = 12345678;
-- DELETE FROM posee WHERE numeroidentidadcliente = 87654321;

-- INSERT INTO posee (numeroidentidadcliente, idarticulo)
-- VALUES (12345678, 1);

-- INSERT INTO posee (numeroidentidadcliente, idarticulo)
-- VALUES (87654321, 2);

-- -- Probar Paquete
-- BEGIN
--   pkg_poliza_posee.listar_polizas;
-- END;

-- DECLARE
--   v_cursor SYS_REFCURSOR;
--   v_nit poliza.nit%TYPE;
--   v_tipo poliza.tipo%TYPE;
--   v_costo poliza.costo%TYPE;
--   v_admin poliza.numeroidentidadadministrador%TYPE;
-- BEGIN
--   v_cursor := pkg_poliza_posee.obtener_polizas_por_tipo('Auto');
--   LOOP
--     FETCH v_cursor INTO v_nit, v_tipo, v_costo, v_admin;
--     EXIT WHEN v_cursor%NOTFOUND;
--     DBMS_OUTPUT.PUT_LINE('NIT: ' || v_nit || ', Tipo: ' || v_tipo || ', Costo: ' || v_costo || ', Admin: ' || v_admin);
--   END LOOP;
--   CLOSE v_cursor;
-- END;

-- BEGIN
--   pkg_poliza_posee.listar_posee;
-- END;

-- DECLARE
--   v_cursor SYS_REFCURSOR;
--   v_idcliente posee.numeroidentidadcliente%TYPE;
--   v_idarticulo posee.idarticulo%TYPE;
-- BEGIN
--   v_cursor := pkg_poliza_posee.obtener_posee_por_cliente(12345678);
--   LOOP
--     FETCH v_cursor INTO v_idcliente, v_idarticulo;
--     EXIT WHEN v_cursor%NOTFOUND;
--     DBMS_OUTPUT.PUT_LINE('Cliente: ' || v_idcliente || ', Artículo: ' || v_idarticulo);
--   END LOOP;
--   CLOSE v_cursor;
-- END;
