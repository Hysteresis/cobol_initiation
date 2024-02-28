       IDENTIFICATION DIVISION.
       PROGRAM-ID. manip_fichier.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT monfichier ASSIGN TO 'lefichier' ORGANIZATION 
                                                   IS LINE SEQUENTIAL.
           
       DATA DIVISION.
       FILE SECTION.
       FD monfichier.
       01 personne.
           05 nom PIC X(20) VALUES SPACES.
           05 age PIC 9(2) VALUE ZEROES.
       01 profession PIC X(20).

       WORKING-STORAGE SECTION.
       01  solde USAGE COMP-1 VALUE 100 .
       01  endl PIC X VALUE X'0A'.


       PROCEDURE DIVISION.
       *>***************ECRITURE DANS UN FICHIER**************
       *>    OPEN OUTPUT monfichier : remplace toutes les lignes
       *>    OPEN EXTEND monfichier : ajoute les lignes
           OPEN EXTEND monfichier.
           
           MOVE "PIERRE" TO nom
           MOVE 41 TO age
           WRITE personne
           MOVE "Developeur" TO profession
           
           WRITE profession

           END-WRITE
           CLOSE monfichier.
           OPEN INPUT monfichier.
           PERFORM UNTIL nom = SPACES
               READ monfichier INTO personne
               DISPLAY personne
           END-PERFORM
           CLOSE monfichier.

           STOP RUN.


       