IDENTIFICATION DIVISION.
PROGRAM-ID. CalculPerimetre.
ENVIRONMENT DIVISION.
DATA DIVISION.
WORKING-STORAGE SECTION.
      01 longueur PIC 9(2).
      01 largeur PIC 9(2).
      01 perimetre PIC 9(4).

PROCEDURE DIVISION.
      PERFORM SAISIR-VALEURS.
      PERFORM CALCULER-PERIMETRE.
      DISPLAY "Le perimete est ",perimetre, " m".
          STOP RUN.

CALCULER-PERIMETRE.
      ADD longueur TO largeur.
      MOVE largeur TO perimetre.
      MULTIPLY 2 BY perimetre.

SAISIR-VALEURS.
      DISPLAY "Saisir longueur :".
      ACCEPT longueur.
      DISPLAY "Saisir largeur :".
      ACCEPT largeur.

