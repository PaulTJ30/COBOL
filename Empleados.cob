      ******************************************************************
      * Author: Paul Tristan
      * Date: 06/08/2026
      * Purpose: Menu que lee empleados de un archivo TXT externo,
      *          calcula el sueldo y genera un reporte con estadisticas
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLEADOS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMP-FILE ASSIGN TO "D:\Ntt\COBOL\EMPLEADOS.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  EMP-FILE.
       01  EMP-REG.
           05 E-NUM     PIC 9(4).
           05 FILLER    PIC X.
           05 E-NOMBRE  PIC X(12).
           05 FILLER    PIC X.
           05 E-APEPAT  PIC X(12).
           05 FILLER    PIC X.
           05 E-APEMAT  PIC X(12).
           05 FILLER    PIC X.
           05 E-EDAD    PIC 9(2).
           05 FILLER    PIC X.
           05 E-SEXO    PIC X.
           05 FILLER    PIC X.
           05 E-UNIV    PIC X(14).
           05 FILLER    PIC X.
           05 E-PROF    PIC X(14).
           05 FILLER    PIC X.
           05 E-EXPER   PIC 9(2).
           05 FILLER    PIC X.
           05 E-SALDIA  PIC 9(4).
           05 FILLER    PIC X.
           05 E-DIAS    PIC 9(2).
       WORKING-STORAGE SECTION.
       01 WS-FIN        PIC X VALUE "N".
          88 FIN-ARCHIVO      VALUE "S".
       01 WS-OPCION     PIC 9 VALUE 0.
       01 WS-SUELDO     PIC 9(7).
       01 WS-LEIDOS     PIC 9(3).
       01 WS-PROCESADOS PIC 9(3).
       01 R-SALDIA      PIC $$,$$9.
       01 R-SUELDO      PIC $$,$$$,$$9.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM MENU UNTIL WS-OPCION = 3
            DISPLAY "FIN"
            STOP RUN.
       MENU.
            DISPLAY " "
            DISPLAY "1. Agregar un registro"
            DISPLAY "2. Leer lista de registros"
            DISPLAY "3. Salir"
            DISPLAY "Opcion: "
            ACCEPT WS-OPCION.
            IF WS-OPCION = 1
                PERFORM AGREGAR.
            IF WS-OPCION = 2
                PERFORM LEER-ARCHIVO.
       AGREGAR.
            MOVE SPACES TO EMP-REG
            DISPLAY "Numero: "
            ACCEPT E-NUM
            DISPLAY "Nombre(s): "
            ACCEPT E-NOMBRE
            DISPLAY "Apellido paterno: "
            ACCEPT E-APEPAT
            DISPLAY "Apellido materno: "
            ACCEPT E-APEMAT
            DISPLAY "Edad: "
            ACCEPT E-EDAD
            DISPLAY "Sexo (M/F): "
            ACCEPT E-SEXO
            DISPLAY "Universidad: "
            ACCEPT E-UNIV
            DISPLAY "Profesion: "
            ACCEPT E-PROF
            DISPLAY "Anios de experiencia: "
            ACCEPT E-EXPER
            DISPLAY "Salario diario: "
            ACCEPT E-SALDIA
            DISPLAY "Dias trabajados: "
            ACCEPT E-DIAS
            OPEN EXTEND EMP-FILE
            WRITE EMP-REG
            CLOSE EMP-FILE
            DISPLAY "Registro agregado.".
       LEER-ARCHIVO.
            MOVE 0 TO WS-LEIDOS
            MOVE 0 TO WS-PROCESADOS
            MOVE "N" TO WS-FIN
            OPEN INPUT EMP-FILE
            PERFORM ENCABEZADO
            READ EMP-FILE
                AT END MOVE "S" TO WS-FIN.
            PERFORM PROCESAR UNTIL FIN-ARCHIVO
            CLOSE EMP-FILE
            PERFORM ESTADISTICAS.
       ENCABEZADO.
            DISPLAY "NOMBRE COMPLETO | UNIVERSIDAD | "
                    "PROFESION | EXP SALDIA DIAS | SUELDO".
       PROCESAR.
            ADD 1 TO WS-LEIDOS.
            IF E-DIAS > 0
                PERFORM CALCULAR.
            READ EMP-FILE
                AT END MOVE "S" TO WS-FIN.
       CALCULAR.
            COMPUTE WS-SUELDO = E-SALDIA * E-DIAS
            MOVE E-SALDIA TO R-SALDIA
            MOVE WS-SUELDO TO R-SUELDO
            ADD 1 TO WS-PROCESADOS
            DISPLAY E-NOMBRE " " E-APEPAT " " E-APEMAT " "
                    E-UNIV " " E-PROF " " E-EXPER "  "
                    R-SALDIA "  " E-DIAS "  " R-SUELDO.
       ESTADISTICAS.
            DISPLAY " "
            DISPLAY "Registros leidos: " WS-LEIDOS
            DISPLAY "Registros procesados: " WS-PROCESADOS.
       END PROGRAM EMPLEADOS.
