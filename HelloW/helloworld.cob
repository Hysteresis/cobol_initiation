IDENTIFICATION DIVISION.
PROGRAM-ID. helloworld.
ENVIRONMENT DIVISION.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 premierevariable PIC 9 VALUES ZERO.
PROCEDURE DIVISION.
      *> ecrire mes paragraphes
       A-PARA.
           *> jouer 
          PERFORM DISPLAY 'A-PARA'
      *>    fin de la boucle
          END-PERFORM.
          PERFORM C-PARA THRU E-PARA.
       
       B-PARA.
          DISPLAY 'B-PARA'.
          STOP RUN.
       
       C-PARA.
           DISPLAY 'C-PARA'.
       
       D-PARA.
           DISPLAY 'D-PARA'.
       
       E-PARA.
           DISPLAY 'E-PARA'.
