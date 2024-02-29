       IDENTIFICATION DIVISION.
       PROGRAM-ID. sub_program.

       ENVIRONMENT DIVISION.
       *> -m pour le subprogram et pas -x
       DATA DIVISION.
       LINKAGE SECTION.
       01  LS-MONTANT PIC 9(5).
      
       PROCEDURE DIVISION USING LS-MONTANT.
           DISPLAY "Sub_program: Montant envoyé par le subprogram", 
                   LS-MONTANT.
           STOP RUN.

           EXIT PROGRAM.
       END PROGRAM sub_program.
