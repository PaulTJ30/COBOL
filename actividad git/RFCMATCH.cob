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
           SELECT LOG-FILE ASSIGN TO DISK "LOG.TXT"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.

      *    -- MODIFICACION PUNTO (a): EMPRESAS ahora es INDEXADO -----
      *    Ya no se usa SORT/EPR-SORT/EPR-ORDS: se lee directo por llave
           SELECT EMPRESAS-IDX ASSIGN TO DISK "EMPRESAS.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS EP-LLAVE
                  FILE STATUS IS WS-STATUS-EPR.

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

      *    -- MODIFICACION PUNTO (a): FD del archivo indexado ---------
       FD  EMPRESAS-IDX.
       01  REG-EPR-IDX.
           05 EP-LLAVE.
              10 EP-RFC-EMPL PIC X(13).
              10 EP-RFC-EMPR PIC X(12).
           05 EP-NOMBRE      PIC X(30).
           05 EP-FECHA       PIC X(08).
           05 EP-SALARIO     PIC 9(06)V9(02).

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

       FD LOG-FILE.
       01  REG-LOG.
           05 LOG-RFC-EMPL PIC X(13).
           05 FILLER       PIC X(2) VALUE "-".
           05 LOG-RFC-EMPR PIC X(12).
           05 FILLER       PIC X(2) VALUE "-".
           05 LOG-MENSAJE  PIC X(30) VALUE "Empleado NO encontrado:)".

       WORKING-STORAGE SECTION.
       01  WS-FIN-EMP   PIC X VALUE "N".
           88 FIN-EMP   VALUE "S".
       01  WS-FIN-MAT   PIC X VALUE "N".
           88 FIN-MAT   VALUE "S".
       01  WS-STATUS-EPR PIC XX.

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



       01  WS-FECHA-SIS.
           05 WS-ANIO   PIC 9(04).
           05 WS-MES    PIC 9(02).
           05 WS-DIA    PIC 9(02).

       01  WS-TOT-EMPLEADOS  PIC 9(05) VALUE 0.
       01  WS-TOT-RELACIONES PIC 9(05) VALUE 0.

       01  ENC-TITULO.
           05 FILLER PIC X(40) VALUE "REPORTE DE EMPLEADOS Y EMPRESAS".

       01  ENC-FECHA.
           05 FILLER    PIC X(10) VALUE "FECHA: ".
           05 EF-DIA    PIC 99.
           05 FILLER    PIC X VALUE "/".
           05 EF-MES    PIC 99.
           05 FILLER    PIC X VALUE "/".
           05 EF-ANIO   PIC 9999.

       01  LINEA-BLANCO PIC X(1) VALUE SPACES.

       01  PIE-LINEA.
           05 FILLER     PIC X(29) VALUE "TOTAL EMPLEADOS PROCESADOS: ".
           05 PL-TOT-EMP  PIC ZZZZ9.
           05 FILLER      PIC X(5)  VALUE SPACES.
           05 FILLER      PIC X(19) VALUE "TOTAL RELACIONES: ".
           05 PL-TOT-REL  PIC ZZZZ9.


       PROCEDURE DIVISION.
       PRINCIPAL.
           SORT EMP-SORT ON ASCENDING KEY SE-LLAVE
               USING EMPLEADOS GIVING EMP-ORDS.
      *    -- MODIFICACION PUNTO (a): ya no se ordena EMPRESAS --------
           PERFORM COTEJO.
           PERFORM REPORTE.
           DISPLAY "REPORTE GENERADO EN REPORTE.TXT".
           STOP RUN.

      *    -- MODIFICACION PUNTO (a): COTEJO ahora busca por llave ----
      *    en vez de comparar dos archivos ordenados (merge-join),
      *    se hace un READ directo al archivo indexado por cada
      *    empleado (acceso por llave en lugar de busqueda secuencial)
       COTEJO.
           OPEN INPUT EMP-ORDS.
           OPEN INPUT EMPRESAS-IDX.
           OPEN OUTPUT MATCH-FILE.
           OPEN OUTPUT LOG-FILE.
           PERFORM LEER-EMP.
           PERFORM UNTIL FIN-EMP
               PERFORM BUSCAR-EMPRESA
               PERFORM LEER-EMP
           END-PERFORM.
           CLOSE EMP-ORDS.
           CLOSE EMPRESAS-IDX.
           CLOSE MATCH-FILE.
           CLOSE LOG-FILE.

       LEER-EMP.
           READ EMP-ORDS AT END MOVE HIGH-VALUES TO EO-LLAVE
                                 MOVE "S" TO WS-FIN-EMP.

      *    -- NUEVO: busqueda directa por llave (reemplaza LEER-EPR y
      *    COTEJAR del programa original) -----------------------------
       BUSCAR-EMPRESA.
           IF NOT FIN-EMP
               MOVE EO-RFC-EMPL TO EP-RFC-EMPL
               MOVE EO-RFC-EMPR TO EP-RFC-EMPR
               READ EMPRESAS-IDX
                   INVALID KEY
                       PERFORM ESCRIBIR-LOG
                   NOT INVALID KEY
                       PERFORM ESCRIBIR-MATCH
               END-READ
           END-IF.

       ESCRIBIR-LOG.
           MOVE SPACES TO REG-LOG.
           MOVE EO-RFC-EMPL TO LOG-RFC-EMPL.
           MOVE EO-RFC-EMPR TO LOG-RFC-EMPR.
           MOVE "Empleado NO encontrado" TO LOG-MENSAJE.
           WRITE REG-LOG.

       ESCRIBIR-MATCH.
           MOVE SPACES TO REG-MATCH.
           MOVE EO-RFC-EMPL TO MT-RFC-EMPL.
           MOVE EO-NOMBRE   TO MT-NOMBRE.
           MOVE EO-APATERNO TO MT-APATERNO.
           MOVE EO-AMATERNO TO MT-AMATERNO.
           MOVE EP-RFC-EMPR TO MT-RFC-EMPR.
           MOVE EP-NOMBRE   TO MT-EMPR-NOMBRE.
           MOVE EP-SALARIO  TO MT-SALARIO.
           WRITE REG-MATCH.


       REPORTE.
           OPEN INPUT MATCH-FILE.
           OPEN OUTPUT REP-FILE.
           PERFORM ARMAR-ENCABEZADO.
           PERFORM LEER-MAT.
           PERFORM UN-EMPLEADO UNTIL FIN-MAT.
           PERFORM ARMAR-PIE.
           CLOSE MATCH-FILE.
           CLOSE REP-FILE.

       ARMAR-ENCABEZADO.
           ACCEPT WS-FECHA-SIS FROM DATE YYYYMMDD.
           MOVE WS-ANIO TO EF-ANIO.
           MOVE WS-MES  TO EF-MES.
           MOVE WS-DIA  TO EF-DIA.
           WRITE REP-REG FROM ENC-TITULO.
           WRITE REP-REG FROM ENC-FECHA.
           WRITE REP-REG FROM LINEA-BLANCO.
           WRITE REP-REG FROM ENC-LINEA.
           WRITE REP-REG FROM ENC-RAYA.

       ARMAR-PIE.
           MOVE WS-TOT-EMPLEADOS  TO PL-TOT-EMP.
           MOVE WS-TOT-RELACIONES TO PL-TOT-REL.
           WRITE REP-REG FROM ENC-RAYA.
           WRITE REP-REG FROM PIE-LINEA.


       LEER-MAT.
           READ MATCH-FILE AT END MOVE "S" TO WS-FIN-MAT.


       UN-EMPLEADO.
           MOVE MT-RFC-EMPL TO WS-RFC-ACT.
           MOVE MT-NOMBRE   TO WS-NOMBRE.
           MOVE MT-APATERNO TO WS-APATERNO.
           MOVE MT-AMATERNO TO WS-AMATERNO.
           MOVE 0 TO WS-NUM-EMPR.
           ADD 1 TO WS-TOT-EMPLEADOS.
           PERFORM JUNTAR-EMPR
               UNTIL FIN-MAT OR MT-RFC-EMPL NOT = WS-RFC-ACT.
           PERFORM IMPRIMIR.

       JUNTAR-EMPR.
           ADD 1 TO WS-NUM-EMPR.
           ADD 1 TO WS-TOT-RELACIONES.
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
