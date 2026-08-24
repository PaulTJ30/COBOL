      *----------------------------------------------------------
      * RELACION 1 A MUCHOS: UN EMPLEADO CON VARIOS TELEFONOS
      * 1) Ordena EMPLEADOS y TELEFONOS por su llave con SORT
      * 2) Junta todos los telefonos de cada empleado en un arreglo
      * 3) Imprime UN registro por empleado con todos sus telefonos
      *----------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TELEFONOS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLEADOS ASSIGN TO DISK "EMPLEADOS.TXT"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.
           SELECT EMP-TEMP ASSIGN TO DISK "Emp_Temp.txt"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.
           SELECT EMP-ORDS ASSIGN TO DISK "Emp_Ords.txt"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.
           SELECT TELEFONOS ASSIGN TO DISK "TELEFONOS.TXT"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.
           SELECT TEL-TEMP ASSIGN TO DISK "Tel_Temp.txt"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.
           SELECT TEL-ORDS ASSIGN TO DISK "Tel_Ords.txt"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.
           SELECT REP-FILE ASSIGN TO DISK "REPORTE.TXT"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  EMPLEADOS.
       01  REG-EMP.
           05 EM-ID    PIC X(06).
           05 EM-NOM   PIC X(20).
           05 EM-APE   PIC X(20).

       SD  EMP-TEMP.
       01  REG-EMP-T.
           05 TE-ID    PIC X(06).
           05 TE-RESTO PIC X(40).

       FD  EMP-ORDS.
       01  REG-EMP-O.
           05 EO-ID    PIC X(06).
           05 EO-NOM   PIC X(20).
           05 EO-APE   PIC X(20).

       FD  TELEFONOS.
       01  REG-TEL.
           05 TL-ID    PIC X(06).
           05 TL-NUM   PIC X(10).

       SD  TEL-TEMP.
       01  REG-TEL-T.
           05 TT-ID    PIC X(06).
           05 TT-NUM   PIC X(10).

       FD  TEL-ORDS.
       01  REG-TEL-O.
           05 TO-ID    PIC X(06).
           05 TO-NUM   PIC X(10).

       FD  REP-FILE.
       01  REP-REG     PIC X(160).

       WORKING-STORAGE SECTION.
       01  WS-FIN-EMP  PIC X VALUE "N".
           88 FIN-EMP  VALUE "S".
       01  WS-FIN-TEL  PIC X VALUE "N".
           88 FIN-TEL  VALUE "S".
       01  WS-NUM-TEL  PIC 9(02) VALUE 0.
       01  WS-J        PIC 9(02) VALUE 0.

      * Arreglo temporal donde se juntan los telefonos del empleado
       01  ARR-TELS.
           05 A-TEL OCCURS 10 TIMES PIC X(10).

      * Registro final: empleado + todos sus telefonos en una linea
       01  LINEA.
           05 L-ID    PIC X(06).
           05 FILLER  PIC X.
           05 L-NOM   PIC X(20).
           05 FILLER  PIC X.
           05 L-APE   PIC X(20).
           05 FILLER  PIC X.
           05 L-GRUPO OCCURS 10 TIMES.
              10 L-TEL   PIC X(10).
              10 FILLER  PIC X.

       01  ENC-LINEA.
           05 FILLER PIC X(07) VALUE "NO-EMP".
           05 FILLER PIC X(21) VALUE "NOMBRE".
           05 FILLER PIC X(21) VALUE "APELLIDO".
           05 FILLER PIC X(11) VALUE "TELEFONOS".

       01  ENC-RAYA.
           05 FILLER PIC X(90) VALUE ALL "-".

       PROCEDURE DIVISION.
       PRINCIPAL.
           SORT EMP-TEMP ON ASCENDING KEY TE-ID
               USING EMPLEADOS GIVING EMP-ORDS.
           SORT TEL-TEMP ON ASCENDING KEY TT-ID
               USING TELEFONOS GIVING TEL-ORDS.
           PERFORM PROCESO.
           DISPLAY "REPORTE GENERADO EN REPORTE.TXT".
           STOP RUN.

       PROCESO.
           OPEN INPUT EMP-ORDS.
           OPEN INPUT TEL-ORDS.
           OPEN OUTPUT REP-FILE.
           WRITE REP-REG FROM ENC-LINEA.
           WRITE REP-REG FROM ENC-RAYA.
           PERFORM LEER-EMP.
           PERFORM LEER-TEL.
           PERFORM ARMAR-REGISTRO UNTIL FIN-EMP.
           CLOSE EMP-ORDS.
           CLOSE TEL-ORDS.
           CLOSE REP-FILE.

       LEER-EMP.
           READ EMP-ORDS AT END MOVE "S" TO WS-FIN-EMP.

       LEER-TEL.
           READ TEL-ORDS AT END MOVE HIGH-VALUES TO TO-ID
                                 MOVE "S" TO WS-FIN-TEL.

      * Por cada empleado: junta sus telefonos y arma su registro
       ARMAR-REGISTRO.
           MOVE 0 TO WS-NUM-TEL.
           PERFORM JUNTAR-TELS UNTIL TO-ID NOT = EO-ID.
           PERFORM IMPRIMIR.
           PERFORM LEER-EMP.

      * Mientras el telefono sea del mismo empleado, al arreglo
       JUNTAR-TELS.
           ADD 1 TO WS-NUM-TEL.
           MOVE TO-NUM TO A-TEL (WS-NUM-TEL).
           PERFORM LEER-TEL.

       IMPRIMIR.
           MOVE SPACES TO LINEA.
           MOVE EO-ID  TO L-ID.
           MOVE EO-NOM TO L-NOM.
           MOVE EO-APE TO L-APE.
           MOVE 1 TO WS-J.
           PERFORM PONER-TEL UNTIL WS-J > WS-NUM-TEL.
           WRITE REP-REG FROM LINEA.

       PONER-TEL.
           MOVE A-TEL (WS-J) TO L-TEL (WS-J).
           ADD 1 TO WS-J.
