      *----------------------------------------------------------
      * CREA UN ARCHIVO INDEXADO DE PRODUCTOS Y CAPTURA REGISTROS
      * Llave = CODPRODU. Los datos se teclean desde el teclado.
      *----------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRODUCTO.

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
       01  WS-SIGUE  PIC X VALUE "S".

       PROCEDURE DIVISION.
       PRINCIPAL.
           OPEN OUTPUT PROD-FILE.
           PERFORM CAPTURAR UNTIL WS-SIGUE = "N".
           CLOSE PROD-FILE.
           DISPLAY "ARCHIVO INDEXADO CREADO.".
           STOP RUN.

       CAPTURAR.
           MOVE SPACES TO PRODUCTO.
           MOVE 0 TO PORCENCU.
           MOVE 0 TO PORCENTI.
           DISPLAY "CODIGO DE PRODUCTO:".
           ACCEPT CODPRODU.
           DISPLAY "FECHA INICIO:".
           ACCEPT FECINI.
           DISPLAY "HORA INICIO:".
           ACCEPT HORINI.
           DISPLAY "HORA FIN:".
           ACCEPT HORFIN.
           DISPLAY "FECHA PAGO CUPON:".
           ACCEPT FECPAGO.
           DISPLAY "IMPORTE MINIMO SUSCRIPCION/IMPORTE EMISION:".
           ACCEPT IMPMINSU.
           DISPLAY "IMPORTE MAXIMO SUSCRIPCION:".
           ACCEPT IMPMAXSU.
           DISPLAY "TIPO CUPON:".
           ACCEPT TIPCUPON.
           DISPLAY "BASE DE CALCULO:".
           ACCEPT BASECALC.
           DISPLAY "FRECUENCIA PAGO CUPON:".
           ACCEPT FRECPAG.
           DISPLAY "CURVA FLOTANTE:".
           ACCEPT CURVAFLO.
           DISPLAY "DESTINATARIO DE LA EMISION:".
           ACCEPT DESTEMI.
           DISPLAY "TEXTO PAGO CUPON / DESCRIPCION PRODUCTO:".
           ACCEPT TXTCUPON.
           DISPLAY "ORDEN IRREVOCABLE:".
           ACCEPT ORDIRREV.
           DISPLAY "MULTIFASE:".
           ACCEPT MULTFASE.
           DISPLAY "NUMERO DE FASE BONO MULTIFASE:".
           ACCEPT NUMFASE.
           DISPLAY "TIPO DE OPCION:".
           ACCEPT TIPOPCIO.
           DISPLAY "FECHA DE OPCION:".
           ACCEPT FECHOPCI.
           DISPLAY "PERPETUA:".
           ACCEPT PERPETUA.
           DISPLAY "MAKE WHOLE:".
           ACCEPT MAKWHOLE.
           DISPLAY "FECHA MAKE WHOLE:".
           ACCEPT FECMAWHO.
           DISPLAY "PORCENTAJE CUPON BLOQUE CA-CARACTERISTICAS:".
           ACCEPT PORCENCU.
           DISPLAY "PORCENTAJE TIR BLOQUE TI-TIR:".
           ACCEPT PORCENTI.
           WRITE PRODUCTO INVALID KEY DISPLAY "CODIGO DUPLICADO".
           DISPLAY "OTRO REGISTRO? (S/N):".
           ACCEPT WS-SIGUE.
