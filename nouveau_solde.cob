       IDENTIFICATION DIVISION.
       PROGRAM-ID. nouveau_solde.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       
       FILE-CONTROL.
           SELECT le_solde ASSIGN TO 'le_solde' 
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-FILE-STATUS.
       
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
       01  solde_b USAGE COMP-1 VALUE 200 .
       01  compte_b PIC 9(4) VALUE 0545.
       01  compte_x PIC 9(4) VALUE 0545.
       01  montant_depot PIC 9(4)V99 COMP VALUE ZERO.
       01  montant_retrait PIC 9(4)V99 COMP.
       01  montant_virement PIC 9(4)V99.
       01  somme_a_virer PIC 9(4)V99.
       01  choix_menu PIC X.
       01  continuer PIC X.
       
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
       01  ls_solde USAGE COMP-1.
       
       PROCEDURE DIVISION USING ls_operation, ls_solde.
           EVALUATE ls_operation
           WHEN "N" PERFORM SOLDE_NOUVEAU
           END-EVALUATE.
       
           STOP RUN.      
 
       SOLDE_NOUVEAU.    
           DISPLAY saut_ligne.
           DISPLAY etoiles.
           DISPLAY " Votre nouveau solde : ",ls_solde, " € "
           DISPLAY etoiles.
           DISPLAY saut_ligne.
       
           MOVE FUNCTION CURRENT-DATE TO date_heure.
           MOVE annee TO annee_solde.
           MOVE mois TO mois_solde
           MOVE jour TO jour_solde
           MOVE heure TO heure_solde
           MOVE minute TO minute_solde
           MOVE seconde TO seconde_solde
       
           STRING annee DELIMITED BY SPACE 
                      '/' DELIMITED BY SPACE
                      mois DELIMITED BY SPACE 
                      '/' DELIMITED BY SPACE
                      jour DELIMITED BY SPACE 
                      '/' DELIMITED BY SPACE
                      heure DELIMITED BY SPACE 
                      ':'  DELIMITED BY SPACE
                      minute DELIMITED BY SPACE 
                       '.'  DELIMITED BY SPACE
                      seconde DELIMITED BY SPACE 
           INTO date_heure_solde
           
           DISPLAY date_heure_solde
           
           OPEN EXTEND le_solde.
           MOVE ls_solde TO montant_solde.     
           MOVE ESPACE TO espace_solde. 
           MOVE "SOLDE : " TO label_solde
           WRITE soldes_file
           END-WRITE
           CLOSE le_solde.
        
           EXIT PROGRAM.
       END PROGRAM nouveau_solde.
      