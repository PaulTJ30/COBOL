      ******************************************************************
      * PROGRAMA   : CARGAEPR
      * PROPOSITO  : Cargar el archivo secuencial EMPRESAS.TXT hacia el
      *              archivo INDEXADO EMPRESAS.DAT que usara RFCMATCH.
      *              Se corre UNA VEZ (o cuando cambie EMPRESAS.TXT)
      *              antes de ejecutar RFCMATCH.
      * LLAVE      : EP-RFC-EMPL + EP-RFC-EMPR (igual a SP-LLAVE de
      *              RFCMATCH, para que las lecturas por llave calcen)
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CARGAEPR.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPRESAS ASSIGN TO DISK "EMPRESAS.TXT"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.
           SELECT EMPRESAS-IDX ASSIGN TO DISK "EMPRESAS.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS SEQUENTIAL
                  RECORD KEY IS EP-LLAVE
                  FILE STATUS IS WS-STATUS-EPR.

       DATA DIVISION.
       FILE SECTION.
       FD  EMPRESAS.
       01  REG-EPR.
           05 EP-RFC-EMPL-S  PIC X(13).
           05 EP-RFC-EMPR-S  PIC X(12).
           05 EP-NOMBRE-S    PIC X(30).
           05 EP-FECHA-S     PIC X(08).
           05 EP-SALARIO-S   PIC 9(06)V9(02).

       FD  EMPRESAS-IDX.
       01  REG-EPR-IDX.
           05 EP-LLAVE.
              10 EP-RFC-EMPL PIC X(13).
              10 EP-RFC-EMPR PIC X(12).
           05 EP-NOMBRE      PIC X(30).
           05 EP-FECHA       PIC X(08).
           05 EP-SALARIO     PIC 9(06)V9(02).

       WORKING-STORAGE SECTION.
       01  WS-STATUS-EPR      PIC XX.
       01  WS-FIN             PIC X VALUE "N".
           88 FIN-ARCHIVO     VALUE "S".
       01  WS-LEIDOS          PIC 9(05) VALUE 0.
       01  WS-GRABADOS        PIC 9(05) VALUE 0.
       01  WS-RECHAZADOS      PIC 9(05) VALUE 0.

       PROCEDURE DIVISION.
       PRINCIPAL.
           OPEN INPUT  EMPRESAS.
           OPEN OUTPUT EMPRESAS-IDX.

           PERFORM UNTIL FIN-ARCHIVO
               READ EMPRESAS
                   AT END
                       MOVE "S" TO WS-FIN
                   NOT AT END
                       ADD 1 TO WS-LEIDOS
                       PERFORM CARGAR-REGISTRO
               END-READ
           END-PERFORM.

           CLOSE EMPRESAS.
           CLOSE EMPRESAS-IDX.

           DISPLAY "------------------------------------------".
           DISPLAY "Registros leidos    : " WS-LEIDOS.
           DISPLAY "Registros grabados  : " WS-GRABADOS.
           DISPLAY "Registros rechazados: " WS-RECHAZADOS.
           DISPLAY "EMPRESAS.DAT generado.".
           DISPLAY "------------------------------------------".
           STOP RUN.

       CARGAR-REGISTRO.
           MOVE EP-RFC-EMPL-S TO EP-RFC-EMPL.
           MOVE EP-RFC-EMPR-S TO EP-RFC-EMPR.
           MOVE EP-NOMBRE-S   TO EP-NOMBRE.
           MOVE EP-FECHA-S    TO EP-FECHA.
           MOVE EP-SALARIO-S  TO EP-SALARIO.

           WRITE REG-EPR-IDX
               INVALID KEY
                   ADD 1 TO WS-RECHAZADOS
                   DISPLAY "Llave duplicada, se omite: " EP-LLAVE
               NOT INVALID KEY
                   ADD 1 TO WS-GRABADOS
           END-WRITE.
