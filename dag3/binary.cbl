       IDENTIFICATION DIVISION.

       PROGRAM-ID. BINARY.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
          DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

          SELECT BINARYFIL ASSIGN "input.txt"
          ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS IND1-FILESTATUS.

       DATA DIVISION.
       FILE SECTION.

       FD BINARYFIL.
       01 WS-INPUT PIC 9(12).

       WORKING-STORAGE SECTION.

       01 NUMBERS-S.
          05 NUMBER-X PIC 9 OCCURS 12 TIMES.

       01 NUMBERS-SUM.
          05 SUM-NUMBER-X PIC 9(3) OCCURS 12 TIMES.

       01 END-OF-FILE-SW PIC 9.
          88 END-OF-FILE VALUE 1.

       01 W-FILESTATUSES.
          05 IND1-FILESTATUS PIC XX.

       01 REKNARE.
          05 REKNARE-1 PIC 9(4).
          05 REKNARE-2 PIC 9(4).
          05 INDEX-1 PIC 99.

       01 RESULTAT.
          05 GAMMA PIC 9(4).
          05 EPSILON PIC 9(4).
          05 TOT-RESULTAT PIC 9(20).

       PROCEDURE DIVISION.

       A-MAIN SECTION.

          PERFORM B-INIT
          PERFORM C-CALCULATE
          PERFORM N-AVSLUTA
          .
       B-INIT SECTION.

          INITIALIZE NUMBERS-S
          INITIALIZE NUMBERS-SUM
          INITIALIZE REKNARE-1

          OPEN INPUT BINARYFIL

          READ BINARYFIL
             AT END
                SET END-OF-FILE TO TRUE
          END-READ
          .

       C-CALCULATE SECTION.

          PERFORM UNTIL END-OF-FILE

             MOVE WS-INPUT TO NUMBERS-S

             ADD NUMBER-X(1) TO SUM-NUMBER-X(1)
             ADD NUMBER-X(2) TO SUM-NUMBER-X(2)
             ADD NUMBER-X(3) TO SUM-NUMBER-X(3)
             ADD NUMBER-X(4) TO SUM-NUMBER-X(4)
             ADD NUMBER-X(5) TO SUM-NUMBER-X(5)
             ADD NUMBER-X(6) TO SUM-NUMBER-X(6)
             ADD NUMBER-X(7) TO SUM-NUMBER-X(7)
             ADD NUMBER-X(8) TO SUM-NUMBER-X(8)
             ADD NUMBER-X(9) TO SUM-NUMBER-X(9)
             ADD NUMBER-X(10) TO SUM-NUMBER-X(10)
             ADD NUMBER-X(11) TO SUM-NUMBER-X(11)
             ADD NUMBER-X(12) TO SUM-NUMBER-X(12)

             ADD 1 TO REKNARE-1

             READ BINARYFIL
             AT END
                SET END-OF-FILE TO TRUE
             END-READ

          END-PERFORM

          COMPUTE REKNARE-2 = REKNARE-1 / 2

          PERFORM VARYING INDEX-1 FROM 1 BY 1
                UNTIL INDEX-1 > 12
             IF SUM-NUMBER-X(INDEX-1) >= REKNARE-2
                IF INDEX-1 = 12
                   COMPUTE GAMMA = GAMMA + NUMBER-X(INDEX-1)
                ELSE
                   COMPUTE GAMMA = GAMMA +
                         (2 ** (12 - INDEX-1))
                END-IF
             ELSE
                 IF INDEX-1 = 12
                    COMPUTE EPSILON = EPSILON + NUMBER-X(INDEX-1)
                 ELSE
                    COMPUTE EPSILON = EPSILON +
                       (2 ** (12 - INDEX-1))
                 END-IF
             END-IF
          END-PERFORM

          COMPUTE TOT-RESULTAT = GAMMA * EPSILON

          DISPLAY "Gamma: " GAMMA
          DISPLAY "Epsilon: " EPSILON
          DISPLAY "Totalt: " TOT-RESULTAT
          .
       N-AVSLUTA SECTION.

          CLOSE BINARYFIL

          STOP RUN
          .
