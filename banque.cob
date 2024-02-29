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
       01 ws-file-status PIC XX.
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
       77  saut_ligne PIC X(3) VALUE
           " ".
       77  tiret_menu PIC X(20) VALUE
           "-------------- ".
       77  espace PIC X(20) VALUE
           "  ".
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
                       CALL 'depot' USING 'D' montant_depot

                   WHEN '2'
                       CALL 'retrait' USING 'D' MONTANT_RETRAIT
                   WHEN '3'
                       PERFORM VIREMENT
                   WHEN '4'
                       PERFORM MON_SOLDE
                    WHEN OTHER
                        DISPLAY "/!\ Choix non reconnu"
               END-EVALUATE
            END-PERFORM
            GOBACK.

       HISTORIQUE_DEPOT.
           OPEN EXTEND historique.
           MOVE "Depot :" TO action_transaction.
           MOVE montant_depot TO montant_transaction.
           MOVE FUNCTION CURRENT-DATE TO date_heure_trans.
           
           MOVE date_heure_trans TO date_heure_transaction.
           MOVE ESPACE TO ESPACE_TRANSACTION.
           WRITE transactions
           END-WRITE
           CLOSE historique.

       HISTORIQUE_RETRAIT.
           OPEN EXTEND historique.
           MOVE "Retrait :" TO action_transaction.
           MOVE montant_retrait TO montant_transaction.        
           WRITE transactions
           END-WRITE
           CLOSE historique.

       HISTORIQUE_VIREMENT.
           OPEN EXTEND historique.
           MOVE "Virement :" TO action_transaction.
           MOVE SOMME_A_VIRER TO montant_transaction.        
           WRITE transactions
           END-WRITE
           CLOSE historique.
           
      *>RETRAIT.
      *>    PERFORM MON_SOLDE
      *>    DISPLAY tiret_menu.
      *>    DISPLAY "-->  Retrait :"
      *>    DISPLAY tiret_menu.
      *>    ACCEPT montant_retrait
      *>    IF solde < montant_retrait THEN
      *>        DISPLAY "Pas assez de solde"
      *>    ELSE
      *>        COMPUTE solde = solde - montant_retrait
      *>        DISPLAY "Le RETRAIT est ", montant_retrait, " €"
      *>        PERFORM NOUVEAU_SOLDE
      *>    END-IF.
      *>    PERFORM HISTORIQUE_RETRAIT.

       VIREMENT.
           PERFORM MON_SOLDE
           DISPLAY "Saisir le compte à solder:".
      *>   saisir le compte a solder : 0545    
           ACCEPT compte_x.
           if compte_x =  COMPTE_B THEN
               DISPLAY "Saisir la somme à virer:"
               ACCEPT somme_a_virer
               if SOMME_A_VIRER < solde THEN
                   COMPUTE SOLDE_B = SOLDE_B + SOMME_A_VIRER
                   COMPUTE SOLDE = SOLDE - SOMME_A_VIRER
                   DISPLAY "--> Le virement de ", 
                               SOMME_A_VIRER , 
                               " € a bien été effectué"
                        
                   PERFORM NOUVEAU_SOLDE
                   PERFORM HISTORIQUE_VIREMENT
                ELSE 
                   DISPLAY "Votre solde ne permet pas de virer ", 
                               SOMME_A_VIRER " €, car Votre SOLDE :", 
                               solde 
           ELSE 
               DISPLAY " Numero de compte erroné"
           END-IF.
      
       MON_SOLDE.
           DISPLAY saut_ligne.
           DISPLAY etoiles.
           DISPLAY "     Mon solde : ",solde, " € ".
           DISPLAY etoiles.
           DISPLAY saut_ligne.

       NOUVEAU_SOLDE.
           DISPLAY saut_ligne.
           DISPLAY etoiles.
           DISPLAY "     Nouveau solde : ",solde, " € "
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
           
           DISPLAY "STR date_heure_solde :", date_heure_solde
           
           OPEN EXTEND le_solde.
           MOVE solde TO montant_solde.     
           MOVE ESPACE TO espace_solde. 
           MOVE "SOLDE : " TO label_solde
           WRITE soldes_file
           END-WRITE
           CLOSE le_solde.
         