       IDENTIFICATION DIVISION.
       PROGRAM-ID. banque.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.       
           
       WORKING-STORAGE SECTION.
       01  solde USAGE COMP-1 VALUE 100 .
       01  solde_b USAGE COMP-1 VALUE 200 .
       01  montant_depot PIC 9(4)V99 COMP VALUE ZERO.
       01  montant_retrait PIC 9(4)V99 COMP.
       01  montant_virement PIC 9(4)V99.
       01  choix_menu PIC X.
       01  continuer PIC X.

       01  transaction.
           02 montant PIC 9(5)V99.
           02 action PIC X(20).
           02 date_heure_trans PIC X(20).      

       77  tiret_menu PIC X(20) VALUE  "------------------ ".
    

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
               