       IDENTIFICATION DIVISION.
       PROGRAM-ID. banque.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT le_solde ASSIGN TO 'le_solde' 
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT historique ASSIGN TO 'historique' 
           ORGANIZATION  IS LINE SEQUENTIAL.
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
       FD historique.
       01 transactions.
           05  date_heure_transaction PIC X(14) VALUES SPACES.
           05  espace_transaction PIC X(2) .
           05  action_transaction PIC X(10) VALUES SPACES.
           05  montant_transaction PIC 9(4)V99.
           
       WORKING-STORAGE SECTION.
       01  le_montant PIC 9(5) VALUE 500.
       01  ws-file-status PIC XX.
       01  solde USAGE COMP-1 VALUE 100 .
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
       77  saut_ligne PIC X(3) VALUE " ".
       77  tiret_menu PIC X(20) VALUE  "-------------- ".
       77  espace PIC X(20) VALUE  "  ".
       77  myDisplayMessage pic X(100).


       PROCEDURE DIVISION.
           CALL 'verifier_fichier' USING 'V' 

           PERFORM UNTIL continuer = 'y'
               DISPLAY tiret_menu
               DISPLAY "     Menu "
               DISPLAY tiret_menu
               DISPLAY "Depot : 1"
               DISPLAY "Retrait : 2"
               DISPLAY "Virement : 3"
               DISPLAY "Mon solde : 4"
               DISPLAY tiret_menu
               DISPLAY "Quitter : 0"
               ACCEPT choix_menu
               EVALUATE choix_menu
                   WHEN '0'
                       DISPLAY "Merci de votre visite."
                       GOBACK
                   WHEN '1'
                       CALL 'depot' USING 'D' montant_depot, solde
                       CALL 'nouveau_solde' USING 'N' solde

                   WHEN '2'
                       CALL 'retrait' USING 'R' montant_retrait, solde
                       CALL 'nouveau_solde' USING 'N' solde 
                   WHEN '3'
                       CALL 'virement' USING 'V' montant_virement, solde
                       CALL 'nouveau_solde' USING 'N' solde 
                   WHEN '4'
                       CALL 'mon_solde' USING 'S' solde
                    WHEN OTHER
                        DISPLAY "/!\ Choix non reconnu"
               END-EVALUATE
            END-PERFORM
            GOBACK.
               