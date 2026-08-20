      ******************************************************************
      * Author: Paul Tristan
      * Date: 05/08/2026
      * Purpose: Registrar 3 invitados usando PERFORM y PERFORM VARYING
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACTIVIDADES.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC.
       OBJECT-COMPUTER. PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 INVITADO  PIC X(20).
       01 I         PIC 9.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM REGISTRO
            STOP RUN.
       REGISTRO.
            DISPLAY "LISTA DE INVITADOS"
            PERFORM PEDIR VARYING I FROM 1 BY 1 UNTIL I > 3.
       PEDIR.
            DISPLAY "Invitado numero " I ": "
            ACCEPT INVITADO
            DISPLAY "Registrado: " INVITADO.
       END PROGRAM ACTIVIDADES.
