      *----------------------------------------------------------
      * ACTUALIZA EL INDEXADO CON UN ARCHIVO SECUENCIAL
      * Lee el secuencial, busca la llave (CODPRODU) en el indexado
      * y si existe actualiza (REWRITE) los campos que trae.
      * Estadistica: leidos, actualizados, no encontrados.
      *----------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACTUALIZA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN TO DISK "PRODUCT_19082026.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.
           SELECT PROD-FILE ASSIGN TO DISK "PRODUCTOS.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CODPRODU.

       DATA DIVISION.
       FILE SECTION.
       FD  SEQ-FILE.
       01  REG-SEQ.
           04 S-CODPRODU  PIC X(30).
           04 S-FECPAGO   PIC X(10).
           04 S-BASECALC  PIC X(20).
           04 S-FRECPAG   PIC X(20).
           04 S-TXTCUPON  PIC X(50).
           04 S-MULTFASE  PIC XX.
           04 S-FILLER    PIC XX.
           04 S-TIPOPCIO  PIC X(8).
           04 S-FECHOPCI  PIC X(20).
           04 S-PERPETUA  PIC XX.
           04 S-PORCENCU  PIC S9(4)V9(3) SIGN IS LEADING SEPARATE.
           04 S-PORCENTI  PIC S9(4)V9(3) SIGN IS LEADING SEPARATE.

       FD  PROD-FILE.
       01  PRODUCTO.
           04 CODPRODU   PIC X(30).
           04 FECINI     PIC X(10).
           04 HORINI     PIC X(8).
           04 HORFIN     PIC X(8).
           04 FECPAGO    PIC X(10).
           04 IMPMINSU   PIC X(50).
           04 IMPMAXSU   PIC X(50).
           04 TIPCUPON   PIC XX.
           04 BASECALC   PIC X(20).
           04 FRECPAG    PIC X(20).
           04 CURVAFLO   PIC X(25).
           04 DESTEMI    PIC X(150).
           04 TXTCUPON   PIC X(50).
           04 ORDIRREV   PIC XX.
           04 MULTFASE   PIC XX.
           04 NUMFASE    PIC XX.
           04 TIPOPCIO   PIC X(8).
           04 FECHOPCI   PIC X(20).
           04 PERPETUA   PIC XX.
           04 MAKWHOLE   PIC XX.
           04 FECMAWHO   PIC X(20).
           04 PORCENCU   PIC S9(4)V9(3) DISPLAY.
           04 PORCENTI   PIC S9(4)V9(3) DISPLAY.

       WORKING-STORAGE SECTION.
       01  WS-FIN     PIC X VALUE "N".
           88 FIN-SEQ VALUE "S".
       01  WS-NOENC   PIC X VALUE "N".
       01  WS-LEIDOS  PIC 9(4) VALUE 0.
       01  WS-ACT     PIC 9(4) VALUE 0.
       01  WS-NENC    PIC 9(4) VALUE 0.

       PROCEDURE DIVISION.
       PRINCIPAL.
           OPEN INPUT SEQ-FILE.
           OPEN I-O PROD-FILE.
           READ SEQ-FILE AT END MOVE "S" TO WS-FIN.
           PERFORM PROCESAR UNTIL FIN-SEQ.
           CLOSE SEQ-FILE.
           CLOSE PROD-FILE.
           PERFORM ESTADISTICA.
           STOP RUN.

       PROCESAR.
           ADD 1 TO WS-LEIDOS.
           MOVE S-CODPRODU TO CODPRODU.
           MOVE "N" TO WS-NOENC.
           READ PROD-FILE INVALID KEY MOVE "S" TO WS-NOENC.
           IF WS-NOENC = "S"
               ADD 1 TO WS-NENC
           ELSE
               PERFORM ACTUALIZAR.
           READ SEQ-FILE AT END MOVE "S" TO WS-FIN.

       ACTUALIZAR.
           MOVE S-FECPAGO   TO FECPAGO.
           MOVE S-BASECALC  TO BASECALC.
           MOVE S-FRECPAG   TO FRECPAG.
           MOVE S-TXTCUPON  TO TXTCUPON.
           MOVE S-MULTFASE  TO MULTFASE.
           MOVE S-TIPOPCIO  TO TIPOPCIO.
           MOVE S-FECHOPCI  TO FECHOPCI.
           MOVE S-PERPETUA  TO PERPETUA.
           MOVE S-PORCENCU  TO PORCENCU.
           MOVE S-PORCENTI  TO PORCENTI.
           REWRITE PRODUCTO INVALID KEY DISPLAY "ERROR REWRITE".
           ADD 1 TO WS-ACT.

       ESTADISTICA.
           DISPLAY " ".
           DISPLAY "===== ESTADISTICA =====".
           DISPLAY "REGISTROS LEIDOS      : " WS-LEIDOS.
           DISPLAY "REGISTROS ACTUALIZADOS: " WS-ACT.
           DISPLAY "REGISTROS NO ENCONTRADOS: " WS-NENC.
