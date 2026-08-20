      ******************************************************************
      * COBOL SHER
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Tarea4.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT EMPLEADOS-ARCHIVO
       ASSIGN TO "C://Users//shgonzal//Downloads//empleados.txt"
       ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD EMPLEADOS-ARCHIVO.
       01 EMPLEADOS-REGISTRO.
           05 EMP-ID    PIC X(6).
           05 EMP-NOM   PIC X(20).
           05 EMP-APT   PIC X(10).
           05 EMP-AMT   PIC X(10).
           05 EMP-EDAD  PIC 9(2).
           05 EMP-SEXO  PIC X.
           05 EMP-UNI   PIC X(5).
           05 EMP-PROF  PIC X(20).
           05 EMP-EXP   PIC 9(2).
           05 EMP-SALD  PIC 9(6)V99.
           05 EMP-NDT   PIC 9(2).

       WORKING-STORAGE SECTION.
       77 F-ARCHIVO PIC X VALUE "N".
       77 T-SUELDO PIC 9(7)V99 VALUE ZERO.
       77 R-LEIDOS PIC 9(3) VALUE ZERO.
       77 R-PROCESADOS PIC 9(3) VALUE ZERO.
       77 R-SALARIO PIC $$,$$$,$$9.99.
       77 R-SUELDO PIC $$,$$$,$$9.99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
        OPEN INPUT EMPLEADOS-ARCHIVO.
        DISPLAY "NOMBRE COMPLETO                           "
                "UNIV  PROFESION            EXP SALARIO"
                "        DIAS SUELDO".
       PERFORM LECTURA-REGISTRO.
       PERFORM UNTIL F-ARCHIVO = "S"
           PERFORM PROCESO-REGISTRO
           PERFORM LECTURA-REGISTRO
       END-PERFORM.

       CLOSE EMPLEADOS-ARCHIVO.
        DISPLAY "Registros procesados: " R-PROCESADOS.
        DISPLAY "Registros leidos: " R-LEIDOS.
        STOP RUN.

       LECTURA-REGISTRO.
       READ EMPLEADOS-ARCHIVO
           AT END
               MOVE "S" TO F-ARCHIVO
           NOT AT END
               ADD 1 TO R-LEIDOS
       END-READ.

       PROCESO-REGISTRO.
           COMPUTE T-SUELDO = EMP-SALD * EMP-NDT.
           ADD 1 TO R-PROCESADOS.
           MOVE EMP-SALD TO R-SALARIO.
           MOVE T-SUELDO TO R-SUELDO.

           DISPLAY EMP-NOM " " EMP-APT " " EMP-AMT " "
                   EMP-UNI " " EMP-PROF " " EMP-EXP " "
                   R-SALARIO " " EMP-NDT " " R-SUELDO.

       END PROGRAM Tarea4.
