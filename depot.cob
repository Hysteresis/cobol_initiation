       IDENTIFICATION DIVISION.
       PROGRAM-ID. depot.
       
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
       01  ws-file-status pic XX.
       01  solde USAGE COMP-1 VALUE 100 .
       01  montant_depot PIC 9(4)V99 COMP VALUE ZERO.
       
       01 date_heure. 
           05 annee   PIC X(4).
           05 mois    PIC X(2).
           05 jour    PIC X(2).
           05 heure   PIC X(2).
           05 minute  PIC X(2).
           05 seconde PIC X(2).
       
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
       77  myDisplayMessage pic X(100).
       LINKAGE SECTION.
       01  ls_operation PIC X.
       01  ls_MONTANT_DEPOT PIC 9(4)V99 COMP.
       01  ls_solde USAGE COMP-1.

       PROCEDURE DIVISION USING ls_operation, LS_MONTANT_DEPOT, 
                                   ls_solde.
           EVALUATE ls_operation
           WHEN "D" PERFORM DEPOT
           END-EVALUATE.
       
           STOP RUN.
       
       DEPOT.
           DISPLAY tiret_menu.
           DISPLAY "---->  depot :".
           DISPLAY tiret_menu.
           ACCEPT ls_MONTANT_DEPOT.
           COMPUTE ls_solde = ls_solde + LS_MONTANT_DEPOT.
           PERFORM HISTORIQUE_DEPOT
           
           DISPLAY "Gate depot".
       
       
       HISTORIQUE_DEPOT.
           OPEN EXTEND historique.
           MOVE "Depot :" TO action_transaction.
           MOVE LS_MONTANT_DEPOT TO montant_transaction.
       
           MOVE FUNCTION CURRENT-DATE TO date_heure_trans.         
           MOVE date_heure_trans TO date_heure_transaction.
       
           MOVE ESPACE TO ESPACE_TRANSACTION.
           WRITE transactions
       
           END-WRITE
           CLOSE historique.
       
           EXIT PROGRAM.
       END PROGRAM depot.
       