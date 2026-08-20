      *----------------------------------------------------------
      * SIEMBRA EL ARCHIVO INDEXADO CON 12 REGISTROS
      * (borra lo anterior; las llaves salen del secuencial)
      * TXTCUPON = "ORIGINAL" para ver luego que se actualiza.
      *----------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEMILLA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PROD-FILE ASSIGN TO DISK "PRODUCTOS.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CODPRODU.

       DATA DIVISION.
       FILE SECTION.
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
       01  TABLA-KEYS.
           05 FILLER PIC X(30) VALUE "TASA_FLOT_02".
           05 FILLER PIC X(30) VALUE "ESTRUCT_03".
           05 FILLER PIC X(30) VALUE "NOTA_CAP_01".
           05 FILLER PIC X(30) VALUE "BONO_GUB_34".
           05 FILLER PIC X(30) VALUE "BONO_CORP_B".
           05 FILLER PIC X(30) VALUE "CERT_DEPOSITO".
           05 FILLER PIC X(30) VALUE "BONDE_F_01".
           05 FILLER PIC X(30) VALUE "CUPON_VAR_02".
           05 FILLER PIC X(30) VALUE "BONO_UDIS_01".
           05 FILLER PIC X(30) VALUE "SWAP_TASA_02".
           05 FILLER PIC X(30) VALUE "PAGARE_BANC".
           05 FILLER PIC X(30) VALUE "BONO_GUB_28".
       01  LISTA-KEYS REDEFINES TABLA-KEYS.
           05 K-ITEM OCCURS 12 TIMES PIC X(30).

       01  WS-I  PIC 9(2) VALUE 0.

       PROCEDURE DIVISION.
       PRINCIPAL.
           OPEN OUTPUT PROD-FILE.
           PERFORM SEMBRAR VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 12.
           CLOSE PROD-FILE.
           DISPLAY "INDEXADO SEMBRADO CON 12 REGISTROS.".
           STOP RUN.

       SEMBRAR.
           MOVE SPACES TO PRODUCTO.
           MOVE 0 TO PORCENCU.
           MOVE 0 TO PORCENTI.
           MOVE K-ITEM (WS-I) TO CODPRODU.
           MOVE "ORIGINAL" TO TXTCUPON.
           WRITE PRODUCTO INVALID KEY DISPLAY "DUP: " CODPRODU.
