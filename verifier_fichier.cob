       IDENTIFICATION DIVISION.
       PROGRAM-ID. verifier_fichier.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT le_solde ASSIGN TO 'le_solde' 
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-FILE-STATUS.
       *> -m pour le subprogram et pas -x
       DATA DIVISION.
       FILE SECTION.
       FD le_solde.
       01 soldes_file.
           05  date_heure_solde.
               10 annee_solde   PIC X(5).
               10 mois_solde     PIC X(3).
               10 jour_solde     PIC X(3).
               10 heure_solde    PIC X(3).
               10 minute_solde   PIC X(3).
               10 seconde_solde PIC X(3).
           05  espace_solde PIC X(2)  VALUES SPACES.
           05  label_solde PIC X(7) VALUES "Solde :" .
           05  montant_solde PIC 9(4)V99.
       01  str_solde PIC X(20).  
       WORKING-STORAGE SECTION.
       01  ws-file-status pic XX.
   

       LINKAGE SECTION.
       01  ls_operation PIC X.

       PROCEDURE DIVISION USING ls_operation.
           EVALUATE ls_operation
           WHEN "V" PERFORM VERIFIER-FICHIER
           END-EVALUATE.
           
           STOP RUN.          
           
       VERIFIER-FICHIER.
           OPEN INPUT le_solde 
           IF ws-file-status = '35'
                CLOSE le_solde
                  OPEN OUTPUT le_solde
                  CLOSE le_solde
           END-IF.
           CLOSE le_solde.
    
           EXIT PROGRAM.
       END PROGRAM verifier_fichier.
