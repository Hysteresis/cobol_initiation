       IDENTIFICATION DIVISION.
       PROGRAM-ID. transaction.
       
       DATA DIVISION.
       LINKAGE SECTION.
       01 ls_operation PIC X.
       01 ls_montant PIC 9(4)V99 COMP.
       
       WORKING-STORAGE SECTION.
       01 date_heure_transaction PIC X(14).
       01 espace_transaction PIC X(2).
       01 action_transaction PIC X(10).
       01 montant_transaction PIC 9(4)V99 COMP.
       
       PROCEDURE DIVISION USING ls_operation, ls_montant.
           MOVE FUNCTION CURRENT-DATE TO date_heure_transaction
           MOVE SPACES TO espace_transaction
       
           EVALUATE ls_operation
               WHEN "D" PERFORM DEPOT-TRANSACTION
               WHEN "R" PERFORM RETRAIT-TRANSACTION
           END-EVALUATE
       
           STOP RUN.
       
       DEPOT-TRANSACTION.
           MOVE "Depot" TO action_transaction
           MOVE ls_montant TO montant_transaction
           PERFORM HISTORIQUE.
       
       RETRAIT-TRANSACTION.
           MOVE "Retrait" TO action_transaction
           MOVE ls_montant TO montant_transaction
           PERFORM HISTORIQUE.
       
       HISTORIQUE.
           DISPLAY "Enregistrement dans l'historique..."
           DISPLAY "Date et heure : " date_heure_transaction
           DISPLAY "Action        : " action_transaction
           DISPLAY "Montant       : " montant_transaction.
       