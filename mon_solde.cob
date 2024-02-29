       IDENTIFICATION DIVISION.
       PROGRAM-ID. mon_solde.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.
      
       WORKING-STORAGE SECTION.      
       77  etoiles PIC X(50) VALUE 
           "****************************".
       77  les_plus PIC X(50) VALUE 
           "+++++++++++++++++++++++++++++++".
       77  saut_ligne PIC X(3) VALUE " ".
       77  tiret_menu PIC X(20) VALUE "-------------- ".
       77  espace PIC X(20) VALUE "  ".

       LINKAGE SECTION.
       01  ls_operation PIC X.
       01  ls_MONTANT_DEPOT PIC 9(4)V99 COMP.
       01  ls_solde USAGE COMP-1.
       
       PROCEDURE DIVISION USING ls_operation, ls_solde.
           EVALUATE ls_operation
           WHEN "S" PERFORM MON_SOLDE
           END-EVALUATE.
       
           STOP RUN.
             
       MON_SOLDE.
           DISPLAY saut_ligne.
           DISPLAY etoiles.
           DISPLAY "     Mon solde : ",ls_solde, " € ".
           DISPLAY etoiles.
           DISPLAY saut_ligne.
          
           EXIT PROGRAM.
       END PROGRAM mon_solde.
      