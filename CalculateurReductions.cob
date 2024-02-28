       IDENTIFICATION DIVISION.
       PROGRAM-ID. CalculReductions.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  montant_total USAGE COMP-1 .
       01  reduction PIC 9(2)V99 COMP VALUE ZERO.
       01  montant_final PIC 9(3)V99 COMP.
       01  pourcentage_reduction PIC 9(2)V99.
       

       PROCEDURE DIVISION.

           PERFORM SAISIR_MONTANT.
           PERFORM REDUCTION_MONTANT.

           STOP RUN.
           
       SAISIR_MONTANT.
           DISPLAY "Saisir montant total :".
           ACCEPT montant_total.
           DISPLAY "Le montant total est ",montant_total, " €".

       REDUCTION_MONTANT.
           IF montant_total > 500 THEN
                   MOVE 0.1 TO reduction
           ELSE IF montant_total >= 100  THEN
                   MOVE 0.05 TO reduction

           END-IF.
           COMPUTE pourcentage_reduction = REDUCTION*100


           DISPLAY "Réduction : ", reduction
           COMPUTE montant_final = MONTANT_TOTAL -
                                    (MONTANT_TOTAL * REDUCTION)
           DISPLAY "montant final : ", montant_final, 
                               " Euros avec une Reduction de ", 
                                       pourcentage_reduction, " %".
