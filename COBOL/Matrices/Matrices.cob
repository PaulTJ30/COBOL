      *----------------------------------------------------------
      * OPERACION DE MATRICES 2x2
      * Dos matrices hardcodeadas: saca la SUMA y la MULTIPLICACION
      * Una matriz = tabla de dos dimensiones (OCCURS dentro de OCCURS)
      *----------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MATRICES.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  MAT-A.
           05 A-FILA OCCURS 2 TIMES.
              10 A-COL OCCURS 2 TIMES PIC 9(6).
       01  MAT-B.
           05 B-FILA OCCURS 2 TIMES.
              10 B-COL OCCURS 2 TIMES PIC 9(6).
       01  MAT-S.
           05 S-FILA OCCURS 2 TIMES.
              10 S-COL OCCURS 2 TIMES PIC 9(6).
       01  MAT-M.
           05 M-FILA OCCURS 2 TIMES.
              10 M-COL OCCURS 2 TIMES PIC 9(6).

       01  WS-I      PIC 9 VALUE 0.
       01  WS-J      PIC 9 VALUE 0.
       01  WS-K      PIC 9 VALUE 0.
       01  WS-ACUM   PIC 9(6) VALUE 0.
       01  ED-1      PIC ZZZZZ9.
       01  ED-2      PIC ZZZZZ9.

       PROCEDURE DIVISION.
       PRINCIPAL.
           PERFORM INICIAR.
           PERFORM SUMAR.
           PERFORM MULTIPLICAR.
           PERFORM MOSTRAR.
           STOP RUN.

      * ---- Hardcodear las dos matrices ----
       INICIAR.
           MOVE 1 TO A-COL (1, 1).
           MOVE 2 TO A-COL (1, 2).
           MOVE 3 TO A-COL (2, 1).
           MOVE 4 TO A-COL (2, 2).
           MOVE 5 TO B-COL (1, 1).
           MOVE 6 TO B-COL (1, 2).
           MOVE 7 TO B-COL (2, 1).
           MOVE 8 TO B-COL (2, 2).

      * ---- Suma: elemento con elemento ----
       SUMAR.
           PERFORM SUMA-CELDA
               VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 2
               AFTER   WS-J FROM 1 BY 1 UNTIL WS-J > 2.

       SUMA-CELDA.
           COMPUTE S-COL (WS-I, WS-J) =
                   A-COL (WS-I, WS-J) + B-COL (WS-I, WS-J).

      * ---- Multiplicacion: fila por columna ----
       MULTIPLICAR.
           PERFORM MULT-CELDA
               VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 2
               AFTER   WS-J FROM 1 BY 1 UNTIL WS-J > 2.

       MULT-CELDA.
           MOVE 0 TO WS-ACUM.
           PERFORM MULT-K VARYING WS-K FROM 1 BY 1 UNTIL WS-K > 2.
           MOVE WS-ACUM TO M-COL (WS-I, WS-J).

       MULT-K.
           COMPUTE WS-ACUM =
                   WS-ACUM + A-COL (WS-I, WS-K) * B-COL (WS-K, WS-J).

      * ---- Mostrar resultados ----
       MOSTRAR.
           DISPLAY "MATRIZ A".
           PERFORM FILA-A VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 2.
           DISPLAY "MATRIZ B".
           PERFORM FILA-B VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 2.
           DISPLAY "SUMA A + B".
           PERFORM FILA-S VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 2.
           DISPLAY "MULTIPLICACION A * B".
           PERFORM FILA-M VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 2.

       FILA-A.
           MOVE A-COL (WS-I, 1) TO ED-1.
           MOVE A-COL (WS-I, 2) TO ED-2.
           DISPLAY ED-1 " " ED-2.

       FILA-B.
           MOVE B-COL (WS-I, 1) TO ED-1.
           MOVE B-COL (WS-I, 2) TO ED-2.
           DISPLAY ED-1 " " ED-2.

       FILA-S.
           MOVE S-COL (WS-I, 1) TO ED-1.
           MOVE S-COL (WS-I, 2) TO ED-2.
           DISPLAY ED-1 " " ED-2.

       FILA-M.
           MOVE M-COL (WS-I, 1) TO ED-1.
           MOVE M-COL (WS-I, 2) TO ED-2.
           DISPLAY ED-1 " " ED-2.
