       IDENTIFICATION DIVISION.
       PROGRAM-ID. tableau.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  livres.
           05 livre OCCURS 5 TIMES INDEXED BY i.
               10 titre PIC X(30).
               10 auteur PIC X(30).
               10 annee-publication PIC 9(4).
               10 nombre-copies PIC 9(2).
       01 index-livres PIC 9(2) VALUE 1.

       PROCEDURE DIVISION.
           DISPLAY index-livres
           ADD 1 TO index-livres
           MOVE "1984" TO titre(index-livres).
           MOVE "George Orwell" TO auteur(index-livres).
           MOVE 1949 TO annee-publication(index-livres).
           MOVE 5 TO nombre-copies(index-livres).
           

           MOVE "Le Petit Prince" TO titre(index-livres).
           MOVE "Antoine de Saint-Exupéry" TO auteur(index-livres).
           MOVE 1943 TO annee-publication(index-livres).
           MOVE 3 TO nombre-copies(index-livres).
           ADD 1 TO index-livres    

           MOVE "Le Seigneur des Anneaux" TO titre(index-livres).
           MOVE "J.R.R. Tolkien" TO auteur(index-livres).
           MOVE 1954 TO annee-publication(index-livres).
           MOVE 7 TO nombre-copies(index-livres).
           ADD 1 TO index-livres

           MOVE "Fondation" TO titre(index-livres).
           MOVE "Isaac Asimov" TO auteur(index-livres).
           MOVE 1951 TO annee-publication(index-livres).
           MOVE 4 TO nombre-copies(index-livres).
           ADD 1 TO index-livres

           MOVE "Dune" TO titre(index-livres).
           MOVE "Frank Herbert" TO auteur(index-livres).
           MOVE 1965 TO annee-publication(index-livres).
           MOVE 6 TO nombre-copies(index-livres).
           ADD 1 TO index-livres
           DISPLAY index-livres
           

           PERFORM AFFICHER-LIVRES.
           PERFORM SAISIR-VALEURS.
           PERFORM AFFICHER-LIVRES.
           STOP RUN.

       AFFICHER-LIVRES.
           PERFORM VARYING i FROM 1 BY 1 UNTIL i > index-livres
               DISPLAY "titre : ", titre(i)
               DISPLAY "auteur : ", auteur(i)
               DISPLAY "annee-publication : ", annee-publication(i)
               DISPLAY "nombre-copies : ", nombre-copies(i)
               DISPLAY " "
      
           END-PERFORM.

       SAISIR-VALEURS.
           ADD 1 TO index-livres.
           DISPLAY INDEX-LIVRES
           DISPLAY index-livres
           DISPLAY "Saisir titre :".
           ACCEPT titre(index-livres)
           DISPLAY "Saisir auteur :".
           ACCEPT auteur(index-livres)
           DISPLAY "Saisir annee-publication :".
           ACCEPT annee-publication(index-livres)
           DISPLAY "Saisir nombre-copies :".
           ACCEPT nombre-copies(index-livres).
           
           
       