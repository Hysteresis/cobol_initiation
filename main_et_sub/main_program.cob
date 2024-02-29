       IDENTIFICATION DIVISION.
       PROGRAM-ID. main_program.


       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 Montant PIC 9(5) VALUE 500.

       
       PROCEDURE DIVISION.
           DISPLAY "Main program: Montant reçu par le subprogram", 
                   montant.
               CALL 'sub_program' USING Montant
      
               STOP RUN.
           