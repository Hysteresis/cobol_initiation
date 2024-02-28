       IDENTIFICATION DIVISION.
       PROGRAM-ID. tableau.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  livres.
           05 livre OCCURS 50 TIMES INDEXED BY i.
               10 titre PIC X(30).
               10 auteur PIC X(30).
               10 annee-publication PIC 9(4).
               10 nombre-copies PIC 9(2).
       01 nb-livres PIC 9(2) VALUE 1.
       01 index-livres PIC 9(2).
       01 max_livres PIC 9(1) VALUE 5.
       01 display_index PIC Z(3).

       PROCEDURE DIVISION.

           PERFORM NB_LIVRES.
           PERFORM SAISIR-LIVRES.
           PERFORM AFFICHER-LIVRES.

           STOP RUN.

       NB_LIVRES.
           DISPLAY "Combien de livres à saisir : (maximum : 5)".
           ACCEPT nb-livres.
           IF nb-livres > max_livres THEN              
               MOVE MAX_LIVRES TO NB-LIVRES
               DISPLAY "nb livres = ", NB-LIVRES
           ELSE
               DISPLAY "nb livres = ", NB-LIVRES
           END-IF.

       SAISIR-LIVRES.
           PERFORM VARYING i FROM 1 BY 1 UNTIL i > nb-livres
               MOVE i to DISPLAY_INDEX
               DISPLAY "Livre numéro " DISPLAY_INDEX
               PERFORM SAISIR-LIVRE
           END-PERFORM.

       SAISIR-LIVRE.
           DISPLAY "Saisir titre :".
           ACCEPT titre(i)
           DISPLAY "Saisir auteur :".
           ACCEPT auteur(i)
           DISPLAY "Saisir annee-publication :".
           ACCEPT annee-publication(i)
           DISPLAY "Saisir nombre-copies :".
           ACCEPT nombre-copies(i).

       AFFICHER-LIVRES.
           PERFORM VARYING i FROM 1 BY 1 UNTIL i > nb-livres
                DISPLAY "---------------------------------------------"
                DISPLAY "Livre **", i, "**"
                DISPLAY "titre : ", titre(i)
                DISPLAY "auteur : ", auteur(i)
                DISPLAY "annee-publication : ", annee-publication(i)
                DISPLAY "nombre-copies : ", nombre-copies(i)
                DISPLAY " "
                DISPLAY "---------------------------------------------"
           END-PERFORM.
