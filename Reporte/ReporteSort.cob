      *----------------------------------------------------------
      * REPORTE DE SUELDOS
      * 1) Ordena EMPLEADOS y SUELDOS por su llave con SORT
      * 2) Coteja los dos archivos ordenados por la llave (NO-EMPL)
      * 3) Graba el reporte en REPORTE.TXT
      *----------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPORTE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLEADOS ASSIGN TO DISK "EMPLEADOS.TXT"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.
           SELECT EMPL-TEMP ASSIGN TO DISK "Empleados_Temp.txt"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.
           SELECT EMPL-ORDS ASSIGN TO DISK "Empleados_Ords.txt"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.
           SELECT SUELDOS ASSIGN TO DISK "SUELDOS.TXT"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.
           SELECT SUE-TEMP ASSIGN TO DISK "Sueldos_Temp.txt"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.
           SELECT SUELDOS-ORD ASSIGN TO DISK "Sueldos_Ords.txt"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.
           SELECT REP-FILE ASSIGN TO DISK "REPORTE.TXT"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  EMPLEADOS.
       01  REG-EMPLEADO.
           05 NO-EMPLEADO     PIC X(06).
           05 NOMBRE          PIC X(20).
           05 APATERNO        PIC X(20).
           05 AMATERNO        PIC X(20).
           05 DOMICILIO       PIC X(30).
           05 NO-EXTERIOR     PIC X(05).
           05 NO-INTERIOR     PIC X(05).
           05 COLONIA         PIC X(30).
           05 MUNI-ALCALDIA   PIC X(20).
           05 CP              PIC X(05).
           05 PAIS            PIC X(10).

       SD  EMPL-TEMP.
       01  REG-EMPL-TEMP.
           05 TMP-NO-EMPL     PIC X(06).
           05 TMP-RESTO       PIC X(165).

       FD  EMPL-ORDS.
       01  REG-EMPL-ORD.
           05 ORD-NO-EMPL     PIC X(06).
           05 ORD-NOMBRE      PIC X(20).
           05 ORD-APATERNO    PIC X(20).
           05 ORD-AMATERNO    PIC X(20).
           05 ORD-RESTO       PIC X(105).

       FD  SUELDOS.
       01  REG-SUELDO.
           05 SU-NO-EMPL      PIC X(06).
           05 SU-SUELDO       PIC 9(06).

       SD  SUE-TEMP.
       01  REG-SUE-TEMP.
           05 TMPS-NO-EMPL    PIC X(06).
           05 TMPS-SUELDO     PIC 9(06).

       FD  SUELDOS-ORD.
       01  REG-SUE-ORD.
           05 SUO-NO-EMPL     PIC X(06).
           05 SUO-SUELDO      PIC 9(06).

       FD  REP-FILE.
       01  REP-REG            PIC X(90).

       WORKING-STORAGE SECTION.
       01  WS-FIN-EMPL   PIC X VALUE "N".
           88 FIN-EMPL   VALUE "S".
       01  WS-FIN-SUE    PIC X VALUE "N".
           88 FIN-SUE    VALUE "S".

       01  LINEA-REP.
           05 L-NO       PIC X(06).
           05 FILLER     PIC X VALUE SPACE.
           05 L-NOMBRE   PIC X(20).
           05 FILLER     PIC X VALUE SPACE.
           05 L-APATERNO PIC X(20).
           05 FILLER     PIC X VALUE SPACE.
           05 L-AMATERNO PIC X(20).
           05 FILLER     PIC X VALUE SPACE.
           05 L-SUELDO   PIC $$$,$$9.

       01  ENC-LINEA.
           05 FILLER PIC X(07) VALUE "NO-EMP".
           05 FILLER PIC X(21) VALUE "NOMBRE".
           05 FILLER PIC X(21) VALUE "A-PATERNO".
           05 FILLER PIC X(21) VALUE "A-MATERNO".
           05 FILLER PIC X(08) VALUE "SUELDO".

       01  ENC-RAYA.
           05 FILLER PIC X(80) VALUE ALL "-".

       PROCEDURE DIVISION.
       PRINCIPAL.
           SORT EMPL-TEMP ON ASCENDING KEY TMP-NO-EMPL
               USING EMPLEADOS GIVING EMPL-ORDS.
           SORT SUE-TEMP ON ASCENDING KEY TMPS-NO-EMPL
               USING SUELDOS GIVING SUELDOS-ORD.
           PERFORM PROCESO-COTEJO.
           DISPLAY "REPORTE GENERADO EN REPORTE.TXT".
           STOP RUN.

       PROCESO-COTEJO.
           OPEN INPUT EMPL-ORDS.
           OPEN INPUT SUELDOS-ORD.
           OPEN OUTPUT REP-FILE.
           WRITE REP-REG FROM ENC-LINEA.
           WRITE REP-REG FROM ENC-RAYA.
           PERFORM LEER-EMPL.
           PERFORM LEER-SUE.
           PERFORM COTEJAR UNTIL FIN-EMPL AND FIN-SUE.
           CLOSE EMPL-ORDS.
           CLOSE SUELDOS-ORD.
           CLOSE REP-FILE.

       LEER-EMPL.
           READ EMPL-ORDS AT END MOVE HIGH-VALUES TO ORD-NO-EMPL
                                  MOVE "S" TO WS-FIN-EMPL.

       LEER-SUE.
           READ SUELDOS-ORD AT END MOVE HIGH-VALUES TO SUO-NO-EMPL
                                    MOVE "S" TO WS-FIN-SUE.

       COTEJAR.
           IF ORD-NO-EMPL = SUO-NO-EMPL
               PERFORM ESCRIBIR-LINEA
               PERFORM LEER-EMPL
               PERFORM LEER-SUE
           ELSE
               IF ORD-NO-EMPL < SUO-NO-EMPL
                   PERFORM LEER-EMPL
               ELSE
                   PERFORM LEER-SUE.

       ESCRIBIR-LINEA.
           MOVE ORD-NO-EMPL   TO L-NO.
           MOVE ORD-NOMBRE    TO L-NOMBRE.
           MOVE ORD-APATERNO  TO L-APATERNO.
           MOVE ORD-AMATERNO  TO L-AMATERNO.
           MOVE SUO-SUELDO    TO L-SUELDO.
           WRITE REP-REG FROM LINEA-REP.
