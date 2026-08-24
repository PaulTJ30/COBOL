      *----------------------------------------------------------
      * REPORTE DE SUELDOS POR EMPLEADO
      * Carga EMPLEADOS.TXT en una tabla, busca cada sueldo por ID
      * y GRABA el reporte en el archivo REPORTE.TXT
      *----------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPORTE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMP-FILE ASSIGN TO "EMPLEADOS.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SUE-FILE ASSIGN TO "SUELDOS.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT REP-FILE ASSIGN TO "REPORTE.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  EMP-FILE.
       01  EMP-REG.
           05 E-ID      PIC 9(4).
           05 E-NOMBRE  PIC X(20).
           05 E-TEL     PIC X(10).
           05 E-RFC     PIC X(13).

       FD  SUE-FILE.
       01  SUE-REG.
           05 S-ID      PIC 9(4).
           05 S-SUELDO  PIC 9(6).

       FD  REP-FILE.
       01  REP-REG      PIC X(70).

       WORKING-STORAGE SECTION.
       01  WS-FIN-EMP    PIC X VALUE "N".
           88 FIN-EMP    VALUE "S".
       01  WS-FIN-SUE    PIC X VALUE "N".
           88 FIN-SUE    VALUE "S".
       01  WS-NUM-EMP    PIC 9(3) VALUE 0.
       01  WS-I          PIC 9(3) VALUE 0.
       01  WS-POS        PIC 9(3) VALUE 0.
       01  WS-ENCON      PIC X VALUE "N".
           88 ENCONTRADO VALUE "S".

       01  TABLA-EMP.
           05 T-EMP OCCURS 50 TIMES.
              10 T-ID      PIC 9(4).
              10 T-NOMBRE  PIC X(20).
              10 T-TEL     PIC X(10).
              10 T-RFC     PIC X(13).

      * Estructura de un renglon del reporte (asi se alinean columnas)
       01  LINEA-REP.
           05 L-ID      PIC 9(4).
           05 FILLER    PIC X VALUE SPACE.
           05 L-NOMBRE  PIC X(20).
           05 FILLER    PIC X VALUE SPACE.
           05 L-TEL     PIC X(10).
           05 FILLER    PIC X VALUE SPACE.
           05 L-RFC     PIC X(13).
           05 FILLER    PIC X VALUE SPACE.
           05 L-SUELDO  PIC $$$,$$9.

       01  ENC-LINEA.
           05 FILLER PIC X(5)  VALUE "ID".
           05 FILLER PIC X(21) VALUE "NOMBRE".
           05 FILLER PIC X(11) VALUE "TELEFONO".
           05 FILLER PIC X(14) VALUE "RFC".
           05 FILLER PIC X(6)  VALUE "SUELDO".

       01  ENC-RAYA.
           05 FILLER PIC X(58) VALUE ALL "-".

       PROCEDURE DIVISION.
       PRINCIPAL.
           PERFORM CARGAR-EMPLEADOS.
           PERFORM PROCESAR-SUELDOS.
           DISPLAY "REPORTE GENERADO EN REPORTE.TXT".
           STOP RUN.

       CARGAR-EMPLEADOS.
           OPEN INPUT EMP-FILE.
           READ EMP-FILE AT END MOVE "S" TO WS-FIN-EMP.
           PERFORM GUARDAR-EMP UNTIL FIN-EMP.
           CLOSE EMP-FILE.

       GUARDAR-EMP.
           ADD 1 TO WS-NUM-EMP.
           MOVE E-ID     TO T-ID (WS-NUM-EMP).
           MOVE E-NOMBRE TO T-NOMBRE (WS-NUM-EMP).
           MOVE E-TEL    TO T-TEL (WS-NUM-EMP).
           MOVE E-RFC    TO T-RFC (WS-NUM-EMP).
           READ EMP-FILE AT END MOVE "S" TO WS-FIN-EMP.

       PROCESAR-SUELDOS.
           OPEN INPUT SUE-FILE.
           OPEN OUTPUT REP-FILE.
           PERFORM ENCABEZADO.
           READ SUE-FILE AT END MOVE "S" TO WS-FIN-SUE.
           PERFORM UN-SUELDO UNTIL FIN-SUE.
           CLOSE SUE-FILE.
           CLOSE REP-FILE.

       ENCABEZADO.
           WRITE REP-REG FROM ENC-LINEA.
           WRITE REP-REG FROM ENC-RAYA.

       UN-SUELDO.
           PERFORM BUSCAR-EMPLEADO.
           IF ENCONTRADO
               MOVE S-ID              TO L-ID
               MOVE T-NOMBRE (WS-POS) TO L-NOMBRE
               MOVE T-TEL (WS-POS)    TO L-TEL
               MOVE T-RFC (WS-POS)    TO L-RFC
               MOVE S-SUELDO          TO L-SUELDO
               WRITE REP-REG FROM LINEA-REP
           ELSE
               MOVE SPACES TO LINEA-REP
               MOVE S-ID TO L-ID
               MOVE "EMPLEADO NO ENCONTRADO" TO L-NOMBRE
               WRITE REP-REG FROM LINEA-REP.
           READ SUE-FILE AT END MOVE "S" TO WS-FIN-SUE.

       BUSCAR-EMPLEADO.
           MOVE "N" TO WS-ENCON.
           MOVE 1 TO WS-I.
           PERFORM COMPARA UNTIL WS-I > WS-NUM-EMP OR ENCONTRADO.

       COMPARA.
           IF S-ID = T-ID (WS-I)
               MOVE "S" TO WS-ENCON
               MOVE WS-I TO WS-POS.
           ADD 1 TO WS-I.
