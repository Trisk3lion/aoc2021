       IDENTIFICATION DIVISION.

       PROGRAM-ID. BINARY2.

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
          05 NUMBERS-X PIC 9 OCCURS 12 TIMES.

       01 NUMBER-SINGLE PIC 9.

       01 NUMBERS-SUM.
          05 SUM-NUMBER-X PIC 9(3) OCCURS 12 TIMES.

       01 NUMBERS-MOST.
          05 NUMBER-MOST-X PIC 9 OCCURS 12 TIMES.

       01 NUMBERS-LEAST.
          05 NUMBER-LEAST-X PIC 9 OCCURS 12 TIMES.

       01 NUMBERS-TABLE.
          05 NUMBERS-ROW OCCURS 0 TO 1000 TIMES
                                        DEPENDING ON ANTAL-RADER
                                        INDEXED BY INDEX-1.
             10 NUMBERS-ROW-1 PIC 9.
             10 NUMBERS-ROW-2 PIC 9.
             10 NUMBERS-ROW-3 PIC 9.
             10 NUMBERS-ROW-4 PIC 9.
             10 NUMBERS-ROW-5 PIC 9.
             10 NUMBERS-ROW-6 PIC 9.
             10 NUMBERS-ROW-7 PIC 9.
             10 NUMBERS-ROW-8 PIC 9.
             10 NUMBERS-ROW-9 PIC 9.
             10 NUMBERS-ROW-10 PIC 9.
             10 NUMBERS-ROW-11 PIC 9.
             10 NUMBERS-ROW-12 PIC 9.

       01 INDEX-2 PIC 999.

       01 NUMBER-SUMMA PIC 999.


       01 END-OF-FILE-SW PIC 9 VALUE ZERO.
          88 END-OF-FILE VALUE 1.

       01 FOUND-SW PIC 9 VALUE ZERO.
          88 FOUND VALUE 1.

       01 W-FILESTATUSES.
          05 IND1-FILESTATUS PIC XX.

       01 ANTAL-ETTOR-NOLLOR.
          05 ANTAL-ETTOR PIC 999.
          05 ANTAL-NOLLER PIC 999.

       01 REKNARE.
          05 ANTAL-RADER PIC 9(4).
          05 ANTAL-NYA-RADER PIC 9(4).
          05 REKNARE-2 PIC 9(4).
          05 REKNARE-3 PIC 999.
          05 REKNARE-4 PIC 999.
          05 MIN-ROW PIC 9(4).
          05 MAX-ROW PIC 9(4).
          05 ROW-DIFF PIC 9(4).
      *    05 INDEX-1 PIC 99.

       01 RESULTAT.
          05 SUM-COL-X PIC 9(5).
          05 OXYGEN PIC 9(6).
          05 CARBODIOXIDE PIC 9(6).
          05 TOT-RESULTAT PIC 9(16).

       01 COMMON-ONE-OR-ZERO PIC X.
          88 COMMON-ONE VALUE "1".
          88 COMMON-ZERO VALUE "0".

       01 ODD-ONE-OR-ZERO PIC X.
          88 ODD-ONE VALUE "1".
          88 ODD-ZERO VALUE "0".


       PROCEDURE DIVISION.

       A-MAIN SECTION.

           PERFORM B-INIT
           PERFORM C-SUMMERA
           PERFORM D-HITTA
           PERFORM N-AVSLUTA
           .
       B-INIT SECTION.

           INITIALIZE NUMBERS-S
           INITIALIZE NUMBERS-SUM
           INITIALIZE ANTAL-RADER

           OPEN INPUT BINARYFIL

           READ BINARYFIL
              AT END
                SET END-OF-FILE TO TRUE
           END-READ
           .

       C-SUMMERA SECTION.

           PERFORM UNTIL END-OF-FILE

             ADD 1 TO ANTAL-RADER
             SET INDEX-1 TO ANTAL-RADER

             MOVE WS-INPUT TO NUMBERS-ROW(INDEX-1)

             READ BINARYFIL
             AT END
                SET END-OF-FILE TO TRUE
             END-READ

          END-PERFORM

           DISPLAY "Antal rader: " ANTAL-RADER

           CLOSE BINARYFIL

           SORT NUMBERS-ROW ON DESCENDING KEY
             NUMBERS-ROW-1
             NUMBERS-ROW-2
             NUMBERS-ROW-3
             NUMBERS-ROW-4
             NUMBERS-ROW-5
             NUMBERS-ROW-6
             NUMBERS-ROW-7
             NUMBERS-ROW-8
             NUMBERS-ROW-9
             NUMBERS-ROW-10
             NUMBERS-ROW-11
             NUMBERS-ROW-12

      *    perform varying index-1 from 1 by 1
      *          until index-1 > ANTAL-RADER
      *       display numbers-row(index-1)
      *       end-perform
         .

       D-HITTA SECTION.

           PERFORM DA-HITTA-OXYGEN

           PERFORM DB-HITTA-CARBONDIOXIDE

           DISPLAY "Oxide:" OXYGEN
           DISPLAY "Carbodioxide: " CARBODIOXIDE

           COMPUTE TOT-RESULTAT = OXYGEN * CARBODIOXIDE

           DISPLAY "Resultat: " TOT-RESULTAT

          .

       DA-HITTA-OXYGEN SECTION.

           MOVE ANTAL-RADER TO MAX-ROW
           MOVE 1 TO MIN-ROW
           INITIALIZE FOUND-SW

           PERFORM VARYING INDEX-2 FROM 1 BY 1
                UNTIL INDEX-2 > 12 OR FOUND

             INITIALIZE REKNARE-4
             INITIALIZE ANTAL-ETTOR-NOLLOR
             INITIALIZE COMMON-ONE-OR-ZERO

             PERFORM VARYING INDEX-1 FROM MIN-ROW BY 1
                   UNTIL INDEX-1 > MAX-ROW
                IF NUMBERS-ROW(INDEX-1) (INDEX-2:1) = 1
                   ADD 1 TO ANTAL-ETTOR
                ELSE
                   ADD 1 TO ANTAL-NOLLER
                END-IF
             END-PERFORM


             IF ANTAL-ETTOR >= ANTAL-NOLLER
                SET COMMON-ONE TO TRUE
                DISPLAY "Etta"
             ELSE
                SET COMMON-ZERO TO TRUE
                DISPLAY "nolla"
             END-IF

             IF COMMON-ONE-OR-ZERO = "1"
                PERFORM VARYING INDEX-1 FROM MIN-ROW BY 1
                      UNTIL NUMBERS-ROW(INDEX-1) (INDEX-2:1) NOT = 1
                   ADD 1 TO REKNARE-4
                END-PERFORM

                COMPUTE MAX-ROW = MIN-ROW + REKNARE-4 - 1

                DISPLAY "Max-row: " MAX-ROW  "iter: " INDEX-2
                DISPLAY "Nummer: " NUMBERS-ROW(MAX-ROW)
             ELSE
                PERFORM VARYING INDEX-1 FROM MAX-ROW BY -1
                      UNTIL NUMBERS-ROW(INDEX-1) (INDEX-2:1) NOT = 0
                      ADD 1 TO REKNARE-4
                END-PERFORM

                COMPUTE MIN-ROW = MAX-ROW - REKNARE-4 + 1

                DISPLAY "Max-row: " MIN-ROW  "iter: " INDEX-2
                DISPLAY "Nummer: " NUMBERS-ROW(MIN-ROW)

             END-IF

             DISPLAY "max row: " MAX-ROW
             DISPLAY "min-row: " MIN-ROW

             COMPUTE ROW-DIFF = (MAX-ROW - MIN-ROW)

             DISPLAY "Rad-skillnad. " ROW-DIFF
             IF ROW-DIFF <= 2

                COMPUTE MIN-ROW = MIN-ROW + 1
                SET INDEX-1 TO MIN-ROW
                MOVE NUMBERS-ROW(INDEX-1) TO NUMBERS-S
                DISPLAY "Oxygen: " NUMBERS-S

                SET FOUND TO TRUE
             END-IF

           END-PERFORM

           PERFORM VARYING REKNARE-3 FROM 1 BY 1
                UNTIL REKNARE-3 > 12
             IF NUMBERS-X(REKNARE-3) = 1
                COMPUTE OXYGEN = OXYGEN +
                   (2 ** (12 - REKNARE-3))
             END-IF
          END-PERFORM

          .

       DB-HITTA-CARBONDIOXIDE SECTION.


           MOVE ANTAL-RADER TO MAX-ROW
           MOVE 1 TO MIN-ROW
           INITIALIZE FOUND-SW

           PERFORM VARYING INDEX-2 FROM 1 BY 1
                UNTIL INDEX-2 > 12 OR FOUND

             INITIALIZE REKNARE-4
             INITIALIZE ROW-DIFF
             INITIALIZE ANTAL-ETTOR-NOLLOR
             INITIALIZE ODD-ONE-OR-ZERO

              PERFORM VARYING INDEX-1 FROM MIN-ROW BY 1
                   UNTIL INDEX-1 > MAX-ROW
                IF NUMBERS-ROW(INDEX-1) (INDEX-2:1) = 1
                   ADD 1 TO ANTAL-ETTOR
                ELSE
                   ADD 1 TO ANTAL-NOLLER
                END-IF
             END-PERFORM

             IF ANTAL-ETTOR >= ANTAL-NOLLER
                SET ODD-ZERO TO TRUE
             ELSE
                SET ODD-ONE TO TRUE
             END-IF

             IF ODD-ONE-OR-ZERO = "1"
                PERFORM VARYING INDEX-1 FROM MIN-ROW BY 1
                      UNTIL NUMBERS-ROW(INDEX-1) (INDEX-2:1) NOT = 1
                   ADD 1 TO REKNARE-4
                END-PERFORM

                COMPUTE MAX-ROW = MIN-ROW + REKNARE-4 - 1

                DISPLAY "Max-row: " MAX-ROW  "iter: " INDEX-2
                DISPLAY "Nummer: " NUMBERS-ROW(MAX-ROW)

             ELSE
                PERFORM VARYING INDEX-1 FROM MAX-ROW BY -1
                      UNTIL NUMBERS-ROW(INDEX-1) (INDEX-2:1) NOT = 0
                   ADD 1 TO REKNARE-4
                END-PERFORM

                COMPUTE MIN-ROW = MAX-ROW - REKNARE-4 + 1

                DISPLAY "Max-row: " MIN-ROW  "iter: " INDEX-2
                DISPLAY "Nummer: " NUMBERS-ROW(MIN-ROW)
             END-IF


             COMPUTE ROW-DIFF = (MAX-ROW - MIN-ROW)

             DISPLAY "max row: " MAX-ROW
             DISPLAY "min-row: " MIN-ROW

            DISPLAY "Rad-skillnad. " ROW-DIFF

            IF ROW-DIFF <= 2

               COMPUTE MIN-ROW = MIN-ROW + 1
                SET INDEX-1 TO MIN-ROW
                MOVE NUMBERS-ROW(INDEX-1) TO NUMBERS-S

                DISPLAY "Carbo: " NUMBERS-S

                SET FOUND TO TRUE
             END-IF

           END-PERFORM

           PERFORM VARYING REKNARE-3 FROM 1 BY 1
                UNTIL REKNARE-3 > 12
             IF NUMBERS-X(REKNARE-3) = 1
                COMPUTE CARBODIOXIDE = CARBODIOXIDE +
                   (2 ** (12 - REKNARE-3))
             END-IF
           END-PERFORM
           .

       N-AVSLUTA SECTION.

           STOP RUN
           .
