       IDENTIFICATION DIVISION.
       PROGRAM-ID. retrait.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       
       FILE-CONTROL.
           SELECT historique ASSIGN TO 'historique' 
           ORGANIZATION  IS LINE SEQUENTIAL.
       *> -m pour le subprogram et pas -x
       DATA DIVISION.
       FILE SECTION.

       FD historique.
       01 transactions.
           05  date_heure_transaction PIC X(14) VALUES SPACES.
           05  espace_transaction PIC X(2) .
           05  action_transaction PIC X(10) VALUES SPACES.
           05  montant_transaction PIC 9(4)V99.
       WORKING-STORAGE SECTION.     
       
       01  transaction.
           02 montant PIC 9(5)V99.
           02 action PIC X(20).
           02 date_heure_trans PIC X(20).
       
       77  etoiles PIC X(50) VALUE 
           "****************************".
       77  les_plus PIC X(50) VALUE 
           "+++++++++++++++++++++++++++++++".
       77  saut_ligne PIC X(3) VALUE
           " ".
       77  tiret_menu PIC X(20) VALUE
           "-------------- ".
       77  espace PIC X(20) VALUE
           "  ".
       LINKAGE SECTION.
       01  ls_operation PIC X.
       01  ls_MONTANT_RETRAIT PIC 9(4)V99 COMP.
       01  ls_solde USAGE COMP-1.
       
       PROCEDURE DIVISION USING  ls_operation, ls_MONTANT_RETRAIT, 
                                   ls_solde.
           EVALUATE ls_operation
           WHEN "R" PERFORM RETRAIT
           END-EVALUATE.
          
           STOP RUN.
          
       RETRAIT.
           DISPLAY tiret_menu.
           DISPLAY "-->  Retrait :"
           DISPLAY tiret_menu.
           ACCEPT ls_MONTANT_RETRAIT
           IF ls_solde < ls_MONTANT_RETRAIT THEN
                  DISPLAY "Pas assez de solde"
           ELSE
                  COMPUTE ls_solde = ls_solde - ls_MONTANT_RETRAIT
                  DISPLAY "Le RETRAIT est ", ls_MONTANT_RETRAIT, " €"
                  PERFORM HISTORIQUE_RETRAIT
           END-IF.
       
           DISPLAY "Gate retrait".
       
       HISTORIQUE_RETRAIT.
           OPEN EXTEND historique.
           MOVE "Retrait :" TO action_transaction.
           MOVE ls_MONTANT_RETRAIT TO montant_transaction.  
           MOVE FUNCTION CURRENT-DATE TO date_heure_trans.         
           MOVE date_heure_trans TO date_heure_transaction.
       
           MOVE ESPACE TO ESPACE_TRANSACTION.
           WRITE transactions
           END-WRITE
           CLOSE historique.
       
       
           EXIT PROGRAM.
       END PROGRAM retrait.
       