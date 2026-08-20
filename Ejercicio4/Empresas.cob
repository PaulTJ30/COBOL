      *----------------------------------------------------------
      * RELACION MUCHOS A MUCHOS: EMPLEADOS <-> EMPRESAS
      * 1) SORT empleados por la llave compuesta (NO-EMPR + NO-EMPL)
      * 2) SORT empresas  por la misma llave compuesta
      * 3) Coteja los dos por la llave; los que no casan van SIN MATCH
      * 4) SORT del resultado por NOMBRE de empresa
      * 5) Escribe el reporte formateado
      *----------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPRESAS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLEADOS ASSIGN TO DISK "EMPLEADOS.TXT"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.
           SELECT EMP-SORT ASSIGN TO DISK "Emp_Sort.txt"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.
           SELECT EMP-ORDS ASSIGN TO DISK "Emp_Ords.txt"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.
           SELECT EMPRESAS ASSIGN TO DISK "EMPRESAS.TXT"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.
           SELECT EPR-SORT ASSIGN TO DISK "Epr_Sort.txt"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.
           SELECT EPR-ORDS ASSIGN TO DISK "Epr_Ords.txt"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.
           SELECT MATCH-FILE ASSIGN TO DISK "Match.txt"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.
           SELECT MATCH-SORT ASSIGN TO DISK "Match_Sort.txt"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.
           SELECT MATCH-ORDS ASSIGN TO DISK "Match_Ords.txt"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.
           SELECT REP-FILE ASSIGN TO DISK "REPORTE.TXT"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  EMPLEADOS.
       01  REG-EMP.
           05 EM-NO-EMPR   PIC X(04).
           05 EM-NO-EMPL   PIC X(06).
           05 EM-NOMBRE    PIC X(20).
           05 EM-APATERNO  PIC X(20).
           05 EM-AMATERNO  PIC X(20).

       SD  EMP-SORT.
       01  REG-EMP-S.
           05 SE-LLAVE.
              10 SE-NO-EMPR PIC X(04).
              10 SE-NO-EMPL PIC X(06).
           05 SE-RESTO     PIC X(60).

       FD  EMP-ORDS.
       01  REG-EMP-O.
           05 EO-LLAVE.
              10 EO-NO-EMPR PIC X(04).
              10 EO-NO-EMPL PIC X(06).
           05 EO-NOMBRE    PIC X(20).
           05 EO-APATERNO  PIC X(20).
           05 EO-AMATERNO  PIC X(20).

       FD  EMPRESAS.
       01  REG-EPR.
           05 EP-NO-EMPR   PIC X(04).
           05 EP-NO-EMPL   PIC X(06).
           05 EP-NOMBRE    PIC X(30).
           05 EP-FECHA     PIC X(08).
           05 EP-SALARIO   PIC 9(06)V9(02).

       SD  EPR-SORT.
       01  REG-EPR-S.
           05 SP-LLAVE.
              10 SP-NO-EMPR PIC X(04).
              10 SP-NO-EMPL PIC X(06).
           05 SP-RESTO     PIC X(46).

       FD  EPR-ORDS.
       01  REG-EPR-O.
           05 EPO-LLAVE.
              10 EPO-NO-EMPR PIC X(04).
              10 EPO-NO-EMPL PIC X(06).
           05 EPO-NOMBRE   PIC X(30).
           05 EPO-FECHA    PIC X(08).
           05 EPO-SALARIO  PIC 9(06)V9(02).

       FD  MATCH-FILE.
       01  REG-MATCH.
           05 MT-EMPR-NOMBRE PIC X(30).
           05 MT-NO-EMPR   PIC X(04).
           05 MT-NO-EMPL   PIC X(06).
           05 MT-NOMBRE    PIC X(20).
           05 MT-APATERNO  PIC X(20).
           05 MT-AMATERNO  PIC X(20).
           05 MT-SALARIO   PIC 9(06)V9(02).
           05 MT-FECHA     PIC X(08).
           05 MT-ESTADO    PIC X(09).

       SD  MATCH-SORT.
       01  REG-MATCH-S.
           05 MS-EMPR-NOMBRE PIC X(30).
           05 MS-RESTO     PIC X(95).

       FD  MATCH-ORDS.
       01  REG-MATCH-O.
           05 MO-EMPR-NOMBRE PIC X(30).
           05 MO-NO-EMPR   PIC X(04).
           05 MO-NO-EMPL   PIC X(06).
           05 MO-NOMBRE    PIC X(20).
           05 MO-APATERNO  PIC X(20).
           05 MO-AMATERNO  PIC X(20).
           05 MO-SALARIO   PIC 9(06)V9(02).
           05 MO-FECHA     PIC X(08).
           05 MO-ESTADO    PIC X(09).

       FD  REP-FILE.
       01  REP-REG         PIC X(130).

       WORKING-STORAGE SECTION.
       01  WS-FIN-EMP    PIC X VALUE "N".
           88 FIN-EMP    VALUE "S".
       01  WS-FIN-EPR    PIC X VALUE "N".
           88 FIN-EPR    VALUE "S".
       01  WS-FIN-MAT    PIC X VALUE "N".
           88 FIN-MAT    VALUE "S".

       01  LINEA-REP.
           05 R-EMPRESA   PIC X(30).
           05 FILLER      PIC X.
           05 R-NOMBRE    PIC X(20).
           05 FILLER      PIC X.
           05 R-APATERNO  PIC X(20).
           05 FILLER      PIC X.
           05 R-AMATERNO  PIC X(20).
           05 FILLER      PIC X.
           05 R-SALARIO   PIC $ZZZ,ZZ9.99.
           05 FILLER      PIC X.
           05 R-FECHA     PIC X(08).
           05 FILLER      PIC X.
           05 R-ESTADO    PIC X(09).

       01  ENC-LINEA.
           05 FILLER PIC X(31) VALUE "EMPRESA".
           05 FILLER PIC X(21) VALUE "NOMBRE".
           05 FILLER PIC X(21) VALUE "A-PATERNO".
           05 FILLER PIC X(21) VALUE "A-MATERNO".
           05 FILLER PIC X(12) VALUE "SALARIO".
           05 FILLER PIC X(09) VALUE "FECHA".
           05 FILLER PIC X(09) VALUE "ESTADO".

       01  ENC-RAYA.
           05 FILLER PIC X(120) VALUE ALL "-".

       PROCEDURE DIVISION.
       PRINCIPAL.
           SORT EMP-SORT ON ASCENDING KEY SE-LLAVE
               USING EMPLEADOS GIVING EMP-ORDS.
           SORT EPR-SORT ON ASCENDING KEY SP-LLAVE
               USING EMPRESAS GIVING EPR-ORDS.
           PERFORM COTEJO.
           SORT MATCH-SORT ON ASCENDING KEY MS-EMPR-NOMBRE
               USING MATCH-FILE GIVING MATCH-ORDS.
           PERFORM ESCRIBIR-REPORTE.
           DISPLAY "REPORTE GENERADO EN REPORTE.TXT".
           STOP RUN.

      * ---- Paso 3: cotejar los dos archivos ordenados por llave ----
       COTEJO.
           OPEN INPUT EMP-ORDS.
           OPEN INPUT EPR-ORDS.
           OPEN OUTPUT MATCH-FILE.
           PERFORM LEER-EMP.
           PERFORM LEER-EPR.
           PERFORM COTEJAR UNTIL FIN-EMP AND FIN-EPR.
           CLOSE EMP-ORDS.
           CLOSE EPR-ORDS.
           CLOSE MATCH-FILE.

       LEER-EMP.
           READ EMP-ORDS AT END MOVE HIGH-VALUES TO EO-LLAVE
                                 MOVE "S" TO WS-FIN-EMP.

       LEER-EPR.
           READ EPR-ORDS AT END MOVE HIGH-VALUES TO EPO-LLAVE
                                 MOVE "S" TO WS-FIN-EPR.

       COTEJAR.
           IF EO-LLAVE = EPO-LLAVE
               PERFORM ESCRIBIR-MATCH
               PERFORM LEER-EMP
               PERFORM LEER-EPR
           ELSE
               IF EO-LLAVE < EPO-LLAVE
                   PERFORM ESCRIBIR-EMP-SOLO
                   PERFORM LEER-EMP
               ELSE
                   PERFORM ESCRIBIR-EPR-SOLO
                   PERFORM LEER-EPR.

       ESCRIBIR-MATCH.
           MOVE SPACES TO REG-MATCH.
           MOVE EPO-NOMBRE  TO MT-EMPR-NOMBRE.
           MOVE EO-NO-EMPR  TO MT-NO-EMPR.
           MOVE EO-NO-EMPL  TO MT-NO-EMPL.
           MOVE EO-NOMBRE   TO MT-NOMBRE.
           MOVE EO-APATERNO TO MT-APATERNO.
           MOVE EO-AMATERNO TO MT-AMATERNO.
           MOVE EPO-SALARIO TO MT-SALARIO.
           MOVE EPO-FECHA   TO MT-FECHA.
           MOVE "OK"        TO MT-ESTADO.
           WRITE REG-MATCH.

       ESCRIBIR-EMP-SOLO.
           MOVE SPACES TO REG-MATCH.
           MOVE EO-NO-EMPR  TO MT-NO-EMPR.
           MOVE EO-NO-EMPL  TO MT-NO-EMPL.
           MOVE EO-NOMBRE   TO MT-NOMBRE.
           MOVE EO-APATERNO TO MT-APATERNO.
           MOVE EO-AMATERNO TO MT-AMATERNO.
           MOVE ZEROS       TO MT-SALARIO.
           MOVE "SIN MATCH" TO MT-ESTADO.
           WRITE REG-MATCH.

       ESCRIBIR-EPR-SOLO.
           MOVE SPACES TO REG-MATCH.
           MOVE EPO-NOMBRE  TO MT-EMPR-NOMBRE.
           MOVE EPO-NO-EMPR TO MT-NO-EMPR.
           MOVE EPO-NO-EMPL TO MT-NO-EMPL.
           MOVE EPO-SALARIO TO MT-SALARIO.
           MOVE EPO-FECHA   TO MT-FECHA.
           MOVE "SIN MATCH" TO MT-ESTADO.
           WRITE REG-MATCH.

      * ---- Paso 5: escribir el reporte ya ordenado por empresa ----
       ESCRIBIR-REPORTE.
           OPEN INPUT MATCH-ORDS.
           OPEN OUTPUT REP-FILE.
           WRITE REP-REG FROM ENC-LINEA.
           WRITE REP-REG FROM ENC-RAYA.
           PERFORM LEER-MAT.
           PERFORM UNA-LINEA UNTIL FIN-MAT.
           CLOSE MATCH-ORDS.
           CLOSE REP-FILE.

       LEER-MAT.
           READ MATCH-ORDS AT END MOVE "S" TO WS-FIN-MAT.

       UNA-LINEA.
           MOVE SPACES TO LINEA-REP.
           MOVE MO-EMPR-NOMBRE TO R-EMPRESA.
           MOVE MO-NOMBRE      TO R-NOMBRE.
           MOVE MO-APATERNO    TO R-APATERNO.
           MOVE MO-AMATERNO    TO R-AMATERNO.
           MOVE MO-SALARIO     TO R-SALARIO.
           MOVE MO-FECHA       TO R-FECHA.
           MOVE MO-ESTADO      TO R-ESTADO.
           WRITE REP-REG FROM LINEA-REP.
           PERFORM LEER-MAT.
