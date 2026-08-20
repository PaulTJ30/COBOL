       IDENTIFICATION DIVISION.
       PROGRAM-ID. RFCMATCH.

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
           SELECT REP-FILE ASSIGN TO DISK "REPORTE.TXT"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  EMPLEADOS.
       01  REG-EMP.
           05 EM-RFC-EMPL  PIC X(13).
           05 EM-RFC-EMPR  PIC X(12).
           05 EM-NOMBRE    PIC X(20).
           05 EM-APATERNO  PIC X(20).
           05 EM-AMATERNO  PIC X(20).

       SD  EMP-SORT.
       01  REG-EMP-S.
           05 SE-LLAVE.
              10 SE-RFC-EMPL PIC X(13).
              10 SE-RFC-EMPR PIC X(12).
           05 SE-RESTO     PIC X(60).

       FD  EMP-ORDS.
       01  REG-EMP-O.
           05 EO-LLAVE.
              10 EO-RFC-EMPL PIC X(13).
              10 EO-RFC-EMPR PIC X(12).
           05 EO-NOMBRE    PIC X(20).
           05 EO-APATERNO  PIC X(20).
           05 EO-AMATERNO  PIC X(20).

       FD  EMPRESAS.
       01  REG-EPR.
           05 EP-RFC-EMPL  PIC X(13).
           05 EP-RFC-EMPR  PIC X(12).
           05 EP-NOMBRE    PIC X(30).
           05 EP-FECHA     PIC X(08).
           05 EP-SALARIO   PIC 9(06)V9(02).

       SD  EPR-SORT.
       01  REG-EPR-S.
           05 SP-LLAVE.
              10 SP-RFC-EMPL PIC X(13).
              10 SP-RFC-EMPR PIC X(12).
           05 SP-RESTO     PIC X(46).

       FD  EPR-ORDS.
       01  REG-EPR-O.
           05 EPO-LLAVE.
              10 EPO-RFC-EMPL PIC X(13).
              10 EPO-RFC-EMPR PIC X(12).
           05 EPO-NOMBRE   PIC X(30).
           05 EPO-FECHA    PIC X(08).
           05 EPO-SALARIO  PIC 9(06)V9(02).

       FD  MATCH-FILE.
       01  REG-MATCH.
           05 MT-RFC-EMPL  PIC X(13).
           05 MT-NOMBRE    PIC X(20).
           05 MT-APATERNO  PIC X(20).
           05 MT-AMATERNO  PIC X(20).
           05 MT-RFC-EMPR  PIC X(12).
           05 MT-EMPR-NOMBRE PIC X(30).
           05 MT-SALARIO   PIC 9(06)V9(02).

       FD  REP-FILE.
       01  REP-REG         PIC X(360).

       WORKING-STORAGE SECTION.
       01  WS-FIN-EMP   PIC X VALUE "N".
           88 FIN-EMP   VALUE "S".
       01  WS-FIN-EPR   PIC X VALUE "N".
           88 FIN-EPR   VALUE "S".
       01  WS-FIN-MAT   PIC X VALUE "N".
           88 FIN-MAT   VALUE "S".

       01  WS-RFC-ACT   PIC X(13).
       01  WS-NOMBRE    PIC X(20).
       01  WS-APATERNO  PIC X(20).
       01  WS-AMATERNO  PIC X(20).
       01  WS-NUM-EMPR  PIC 9(02) VALUE 0.
       01  WS-J         PIC 9(02) VALUE 0.


       01  ARR-EMPR.
           05 A-EMPR OCCURS 5 TIMES.
              10 A-RFC PIC X(12).
              10 A-NOM PIC X(30).
              10 A-SAL PIC 9(06)V9(02).

       01  LINEA-REP.
           05 L-RFC-EMPL  PIC X(13).
           05 FILLER      PIC X.
           05 L-NOMBRE    PIC X(20).
           05 FILLER      PIC X.
           05 L-APATERNO  PIC X(20).
           05 FILLER      PIC X.
           05 L-AMATERNO  PIC X(20).
           05 FILLER      PIC X.
           05 L-GRUPO OCCURS 5 TIMES.
              10 L-RFC   PIC X(12).
              10 FILLER  PIC X.
              10 L-NOM   PIC X(30).
              10 FILLER  PIC X.
              10 L-SAL   PIC $ZZZ,ZZ9.99.
              10 FILLER  PIC X.

       01  ENC-LINEA.
           05 FILLER PIC X(14) VALUE "RFC-EMPLEADO".
           05 FILLER PIC X(21) VALUE "NOMBRE".
           05 FILLER PIC X(21) VALUE "A-PATERNO".
           05 FILLER PIC X(21) VALUE "A-MATERNO".
           05 FILLER PIC X(33) VALUE "EMPRESAS (RFC/NOMBRE/SALARIO)".

       01  ENC-RAYA.
           05 FILLER PIC X(120) VALUE ALL "-".

       PROCEDURE DIVISION.
       PRINCIPAL.
           SORT EMP-SORT ON ASCENDING KEY SE-LLAVE
               USING EMPLEADOS GIVING EMP-ORDS.
           SORT EPR-SORT ON ASCENDING KEY SP-LLAVE
               USING EMPRESAS GIVING EPR-ORDS.
           PERFORM COTEJO.
           PERFORM REPORTE.
           DISPLAY "REPORTE GENERADO EN REPORTE.TXT".
           STOP RUN.

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
                   PERFORM LEER-EMP
               ELSE
                   PERFORM LEER-EPR.

       ESCRIBIR-MATCH.
           MOVE SPACES TO REG-MATCH.
           MOVE EO-RFC-EMPL TO MT-RFC-EMPL.
           MOVE EO-NOMBRE   TO MT-NOMBRE.
           MOVE EO-APATERNO TO MT-APATERNO.
           MOVE EO-AMATERNO TO MT-AMATERNO.
           MOVE EPO-RFC-EMPR  TO MT-RFC-EMPR.
           MOVE EPO-NOMBRE    TO MT-EMPR-NOMBRE.
           MOVE EPO-SALARIO   TO MT-SALARIO.
           WRITE REG-MATCH.


       REPORTE.
           OPEN INPUT MATCH-FILE.
           OPEN OUTPUT REP-FILE.
           WRITE REP-REG FROM ENC-LINEA.
           WRITE REP-REG FROM ENC-RAYA.
           PERFORM LEER-MAT.
           PERFORM UN-EMPLEADO UNTIL FIN-MAT.
           CLOSE MATCH-FILE.
           CLOSE REP-FILE.

       LEER-MAT.
           READ MATCH-FILE AT END MOVE "S" TO WS-FIN-MAT.


       UN-EMPLEADO.
           MOVE MT-RFC-EMPL TO WS-RFC-ACT.
           MOVE MT-NOMBRE   TO WS-NOMBRE.
           MOVE MT-APATERNO TO WS-APATERNO.
           MOVE MT-AMATERNO TO WS-AMATERNO.
           MOVE 0 TO WS-NUM-EMPR.
           PERFORM JUNTAR-EMPR
               UNTIL FIN-MAT OR MT-RFC-EMPL NOT = WS-RFC-ACT.
           PERFORM IMPRIMIR.

       JUNTAR-EMPR.
           ADD 1 TO WS-NUM-EMPR.
           MOVE MT-RFC-EMPR    TO A-RFC (WS-NUM-EMPR).
           MOVE MT-EMPR-NOMBRE TO A-NOM (WS-NUM-EMPR).
           MOVE MT-SALARIO     TO A-SAL (WS-NUM-EMPR).
           PERFORM LEER-MAT.

       IMPRIMIR.
           MOVE SPACES TO LINEA-REP.
           MOVE WS-RFC-ACT  TO L-RFC-EMPL.
           MOVE WS-NOMBRE   TO L-NOMBRE.
           MOVE WS-APATERNO TO L-APATERNO.
           MOVE WS-AMATERNO TO L-AMATERNO.
           MOVE 1 TO WS-J.
           PERFORM PONER-EMPR UNTIL WS-J > WS-NUM-EMPR.
           WRITE REP-REG FROM LINEA-REP.

       PONER-EMPR.
           MOVE A-RFC (WS-J) TO L-RFC (WS-J).
           MOVE A-NOM (WS-J) TO L-NOM (WS-J).
           MOVE A-SAL (WS-J) TO L-SAL (WS-J).
           ADD 1 TO WS-J.
