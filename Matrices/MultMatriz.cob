      *----------------------------------------------------------
      * MULTIPLICACION DE MATRICES CON DIMENSIONES POR TECLADO
      * Lee las dimensiones (limites) y los valores desde el teclado,
      * valida que columnas de A = filas de B, multiplica y pinta el
      * resultado en pantalla. Maximo 10 x 10.
      *----------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MULTMATRIZ.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FILAS-A   PIC 9(2) VALUE 0.
       01  COLS-A    PIC 9(2) VALUE 0.
       01  FILAS-B   PIC 9(2) VALUE 0.
       01  COLS-B    PIC 9(2) VALUE 0.

       01  MAT-A.
           05 A-FILA OCCURS 10 TIMES.
              10 A-COL OCCURS 10 TIMES PIC 9(4).
       01  MAT-B.
           05 B-FILA OCCURS 10 TIMES.
              10 B-COL OCCURS 10 TIMES PIC 9(4).
       01  MAT-C.
           05 C-FILA OCCURS 10 TIMES.
              10 C-COL OCCURS 10 TIMES PIC 9(8).

       01  WS-I      PIC 9(2) VALUE 0.
       01  WS-J      PIC 9(2) VALUE 0.
       01  WS-K      PIC 9(2) VALUE 0.
       01  WS-ACUM   PIC 9(8) VALUE 0.

       01  LINEA-OUT.
           05 LO-COL OCCURS 10 TIMES.
              10 LO-NUM PIC ZZZZZZZ9.
              10 FILLER PIC X.

       PROCEDURE DIVISION.
       PRINCIPAL.
           PERFORM LEER-LIMITES.
           IF FILAS-A > 10 OR COLS-A > 10 OR FILAS-B > 10
                   OR COLS-B > 10
               DISPLAY "ERROR: maximo 10 x 10"
           ELSE
               IF COLS-A NOT = FILAS-B
                   DISPLAY "ERROR: columnas de A deben ser igual a "
                           "filas de B"
               ELSE
                   PERFORM LEER-A
                   PERFORM LEER-B
                   PERFORM MULTIPLICAR
                   PERFORM MOSTRAR.
           STOP RUN.

       LEER-LIMITES.
           DISPLAY "Filas de A:".
           ACCEPT FILAS-A.
           DISPLAY "Columnas de A:".
           ACCEPT COLS-A.
           DISPLAY "Filas de B:".
           ACCEPT FILAS-B.
           DISPLAY "Columnas de B:".
           ACCEPT COLS-B.

       LEER-A.
           DISPLAY "Valores de A:".
           PERFORM LEER-A-CELDA
               VARYING WS-I FROM 1 BY 1 UNTIL WS-I > FILAS-A
               AFTER   WS-J FROM 1 BY 1 UNTIL WS-J > COLS-A.

       LEER-A-CELDA.
           DISPLAY "A(" WS-I "," WS-J "):".
           ACCEPT A-COL (WS-I, WS-J).

       LEER-B.
           DISPLAY "Valores de B:".
           PERFORM LEER-B-CELDA
               VARYING WS-I FROM 1 BY 1 UNTIL WS-I > FILAS-B
               AFTER   WS-J FROM 1 BY 1 UNTIL WS-J > COLS-B.

       LEER-B-CELDA.
           DISPLAY "B(" WS-I "," WS-J "):".
           ACCEPT B-COL (WS-I, WS-J).

      * Resultado = FILAS-A x COLS-B ; cada celda es fila x columna
       MULTIPLICAR.
           PERFORM MULT-CELDA
               VARYING WS-I FROM 1 BY 1 UNTIL WS-I > FILAS-A
               AFTER   WS-J FROM 1 BY 1 UNTIL WS-J > COLS-B.

       MULT-CELDA.
           MOVE 0 TO WS-ACUM.
           PERFORM MULT-K VARYING WS-K FROM 1 BY 1 UNTIL WS-K > COLS-A.
           MOVE WS-ACUM TO C-COL (WS-I, WS-J).

       MULT-K.
           COMPUTE WS-ACUM =
                   WS-ACUM + A-COL (WS-I, WS-K) * B-COL (WS-K, WS-J).

       MOSTRAR.
           DISPLAY " ".
           DISPLAY "MATRIZ RESULTADO:".
           PERFORM MOSTRAR-FILA
               VARYING WS-I FROM 1 BY 1 UNTIL WS-I > FILAS-A.

       MOSTRAR-FILA.
           MOVE SPACES TO LINEA-OUT.
           PERFORM MOSTRAR-CELDA
               VARYING WS-J FROM 1 BY 1 UNTIL WS-J > COLS-B.
           DISPLAY LINEA-OUT.

       MOSTRAR-CELDA.
           MOVE C-COL (WS-I, WS-J) TO LO-NUM (WS-J).
