       IDENTIFICATION DIVISION.

       PROGRAM-ID. BINGO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT BINGOFIL ASSIGN "input.txt"
           ORGANIZATION IS LINE SEQUENTIAL
             FILE STATUS IS IND1-FILESTATUS.

       DATA DIVISION.
       FILE SECTION.

       FD BINGOFIL.
       01 WS-INPUT PIC X(300).

       WORKING-STORAGE SECTION.

       01 BINGO-NUMMER PIC X(300).

       01 NUMMER-GRUPP.
          05 ANTAL-NUMMER PIC 9.
          05 NUMMER-MATCH PIC 9.
          05 VINNANDE-TABELL PIC 9(3).
          05 VINNANDE-RAD PIC 9.
          05 VINNANDE-KOLUMN PIC 9.
          05 VINNANDE-NUMMER PIC 99.
          05 REKNE-NUMMER PIC 99.
          05 ANTAL-MATCHAR PIC 9.

       01 SENASTE-NUMMER-SPACE.
           05 SENASTE-NUMMER PIC X(2) JUSTIFIED RIGHT.
           05 FILLER PIC X VALUE SPACE.

       01 SENASTE-NUMMER-X.
           05 SENASTE-NUMMER-BYTE PIC X(2) JUSTIFIED RIGHT.
           05 FILLER VALUE "X".

       01 TABELL.
          05 BINGO-TABELL OCCURS 0 TO 100 TIMES
                                        DEPENDING ON ANTAL-TABELLER
                                        INDEXED BY TABELL-INDEX.
             10 BINGO-RAD OCCURS 5 TIMES INDEXED BY RAD-INDEX.
                 15 SIFFROR OCCURS 5 TIMES INDEXED BY SIFFER-INDEX.
                     20 RAD-SIFFRA PIC XX JUSTIFIED RIGHT.
                     20 RAD-SIFFRA-X   PIC X VALUE SPACE.

       01 ANTAL-TABELLER PIC 9(3).


       01 TABBELL2.
           05 BINGO-KOLUMN OCCURS 5 TIMES INDEXED BY KOLUMN-INDEX.
               10 KOLUMN-SIFFROR OCCURS 5 TIMES
                                        INDEXED BY KOLUMN-SIFFER-INDEX.
                     20 KOLUMN-SIFFRA PIC XX JUSTIFIED RIGHT.
                     20 KOLUMN-SIFFRA-X   PIC X VALUE SPACE.



       01 INDEX-2 PIC 999.

       01 NUMBER-SUMMA PIC 999.


       01 FIL-SLUT-SW PIC 9 VALUE ZERO.
          88 FIL-SLUT VALUE 1.

       01 HITTAT-SW PIC 9 VALUE ZERO.
          88 HITTAT VALUE 1.

       01 BINGO-SW PIC 9 VALUE ZERO.
          88 BINGO VALUE 1.

       01 SLUT-PA-SIFFROR-SW PIC 9 VALUE ZERO.
          88 SLUT-PA-SIFFROR VALUE 1.

       01 W-FILESTATUSES.
          05 IND1-FILESTATUS PIC XX.

       01 REKNARE.
          05 PEKARE PIC 9(3).
          05 REKNARE-2 PIC 9(4).
          05 REKNARE-3 PIC 999.
          05 REKNARE-4 PIC 999.

       01 RESULTAT.
           05 TOT-RAD-KOLUMN PIC 9(4).
           05 TOT-OMARKERADE PIC 9(5).
          05 TOT-RESULTAT PIC 9(16).

       PROCEDURE DIVISION.

       A-MAIN SECTION.

           PERFORM B-INIT
           PERFORM C-BINGO-NUMMER
           PERFORM D-BINGO-TABELL
           PERFORM E-SPELA-BINGO UNTIL BINGO OR SLUT-PA-SIFFROR
           PERFORM D-BEREKNA-POENG
           PERFORM N-AVSLUTA
           .
       B-INIT SECTION.

           INITIALIZE BINGO-SW
           INITIALIZE ANTAL-TABELLER

           MOVE 001 TO PEKARE

           OPEN INPUT BINGOFIL

      *>     Read in bingo-numbers
           READ BINGOFIL
               AT END
                   SET FIL-SLUT TO TRUE
           END-READ

           DISPLAY 'Filestatus: ' IND1-FILESTATUS
           .

       C-BINGO-NUMMER SECTION.

           MOVE FUNCTION TRIM(WS-INPUT,TRAILING) TO BINGO-NUMMER

           DISPLAY 'Bingo-nummer: ' BINGO-NUMMER

           READ BINGOFIL
              AT END
                SET FIL-SLUT TO TRUE
           END-READ
           .
Q
       D-BINGO-TABELL SECTION.

           PERFORM VARYING TABELL-INDEX FROM 1 BY 1
                   UNTIL FIL-SLUT

               ADD 1 TO ANTAL-TABELLER

               PERFORM VARYING RAD-INDEX FROM 1 BY 1
                       UNTIL RAD-INDEX > 5
                   READ BINGOFIL
                       AT END
                           SET FIL-SLUT TO TRUE
                   END-READ

                   MOVE FUNCTION TRIM(WS-INPUT,TRAILING) TO
                       BINGO-RAD(TABELL-INDEX,RAD-INDEX)
               END-PERFORM

      *>         DISPLAY 'Tabell ' ANTAL-TABELLER ': '
      *>                 BINGO-TABELL(ANTAL-TABELLER)

               READ BINGOFIL
                   AT END
                       SET FIL-SLUT TO TRUE
               END-READ
           END-PERFORM
           .

       E-SPELA-BINGO SECTION.


           PERFORM EB-NESTA-NUMMER
           PERFORM EC-MARKERA-TABELL
           PERFORM ED-KONTROLLERA-TABELL
          .

       EB-NESTA-NUMMER SECTION.

           UNSTRING BINGO-NUMMER
               DELIMITED BY ','
               INTO SENASTE-NUMMER
               COUNT IN ANTAL-NUMMER
               WITH POINTER PEKARE
           END-UNSTRING

      *>     DISPLAY 'Antal nummer: ' ANTAL-NUMMER
      *>     DISPLAY 'Pekare: ' PEKARE

           MOVE SENASTE-NUMMER TO SENASTE-NUMMER-BYTE

      *>     DISPLAY 'Senaste nummer: ' SENASTE-NUMMER-SPACE
      *>     DISPLAY 'Ers??tts med: ' SENASTE-NUMMER-X

           IF ANTAL-NUMMER = 0
               SET SLUT-PA-SIFFROR TO TRUE
           END-IF
           .

       EC-MARKERA-TABELL SECTION.

           INSPECT TABELL
               REPLACING ALL SENASTE-NUMMER-SPACE
               BY SENASTE-NUMMER-X
           .

       ED-KONTROLLERA-TABELL SECTION.

           PERFORM VARYING TABELL-INDEX FROM 1 BY 1
             UNTIL (TABELL-INDEX > ANTAL-TABELLER) OR BINGO

             *> Kontrollera rader efter bingo
               PERFORM VARYING RAD-INDEX FROM 1 BY 1
                 UNTIL (RAD-INDEX > 5) OR BINGO
                   MOVE ZERO TO ANTAL-MATCHAR

                   SET KOLUMN-SIFFER-INDEX TO RAD-INDEX

                   PERFORM VARYING SIFFER-INDEX FROM 1 BY 1
                     UNTIL SIFFER-INDEX > 5 OR BINGO
                       SET KOLUMN-INDEX TO SIFFER-INDEX
                       MOVE RAD-SIFFRA
                           (TABELL-INDEX,RAD-INDEX,SIFFER-INDEX) TO
                           KOLUMN-SIFFRA
                           (KOLUMN-INDEX,KOLUMN-SIFFER-INDEX)

                       MOVE RAD-SIFFRA-X
                           (TABELL-INDEX,RAD-INDEX,SIFFER-INDEX) TO
                           KOLUMN-SIFFRA-X
                           (KOLUMN-INDEX,KOLUMN-SIFFER-INDEX)
                   END-PERFORM

                   INSPECT BINGO-RAD(TABELL-INDEX,RAD-INDEX)
                       TALLYING ANTAL-MATCHAR
                       FOR ALL "X"

      *>             DISPLAY 'Antal matchar: ' ANTAL-MATCHAR

                   IF ANTAL-MATCHAR = 5
                       SET BINGO TO TRUE
                       MOVE TABELL-INDEX TO VINNANDE-TABELL
                       MOVE RAD-INDEX TO VINNANDE-RAD
                       MOVE FUNCTION NUMVAL(SENASTE-NUMMER)
                                               TO VINNANDE-NUMMER

                       DISPLAY 'BINGO!'
                       DISPLAY 'Tabell: ' TABELL-INDEX
                       DISPLAY 'Rad: ' RAD-INDEX
                   END-IF
               END-PERFORM

      *> Kontrollera kolumner efter bingo
               PERFORM VARYING KOLUMN-INDEX FROM 1 BY 1
                 UNTIL KOLUMN-INDEX > 5 OR BINGO
                   INITIALIZE ANTAL-MATCHAR

                   INSPECT BINGO-KOLUMN(KOLUMN-INDEX)
                       TALLYING ANTAL-MATCHAR
                       FOR ALL "X"

                   IF ANTAL-MATCHAR = 5
                       SET BINGO TO TRUE
                       MOVE TABELL-INDEX TO VINNANDE-TABELL
                       MOVE KOLUMN-INDEX TO VINNANDE-KOLUMN
                       MOVE FUNCTION NUMVAL(SENASTE-NUMMER)
                                               TO VINNANDE-NUMMER

                       DISPLAY 'BINGO!'
                       DISPLAY 'Tabell: ' TABELL-INDEX
                       DISPLAY 'Kolumn: ' KOLUMN-INDEX
                   END-IF

               END-PERFORM

           END-PERFORM
           .

       D-BEREKNA-POENG SECTION.

           SET TABELL-INDEX TO VINNANDE-TABELL

           PERFORM VARYING RAD-INDEX FROM 1 BY 1
                   UNTIL RAD-INDEX > 5
               PERFORM VARYING SIFFER-INDEX FROM 1 BY 1
                         UNTIL SIFFER-INDEX > 5
                   IF RAD-SIFFRA-X(TABELL-INDEX,
                                     RAD-INDEX,SIFFER-INDEX) = SPACE
                           MOVE FUNCTION NUMVAL(RAD-SIFFRA
                               (TABELL-INDEX,RAD-INDEX,SIFFER-INDEX))
                           TO REKNE-NUMMER
                           COMPUTE TOT-OMARKERADE = TOT-OMARKERADE
                           + REKNE-NUMMER
                   END-IF
              END-PERFORM
           END-PERFORM


           DISPLAY 'Totalt omarkerade: ' TOT-OMARKERADE
           DISPLAY 'Vinnande nummer: ' VINNANDE-NUMMER

           COMPUTE TOT-RESULTAT = VINNANDE-NUMMER * TOT-OMARKERADE

           DISPLAY 'Resultat: ' TOT-RESULTAT
           .

       DA-BEREKNA-POENG-FEL SECTION.

           SET TABELL-INDEX TO VINNANDE-TABELL

           IF VINNANDE-RAD > 0
               PERFORM VARYING RAD-INDEX FROM 1 BY 1
                   UNTIL RAD-INDEX > 5
                   IF RAD-INDEX = VINNANDE-RAD
                       PERFORM VARYING SIFFER-INDEX FROM 1 BY 1
                         UNTIL SIFFER-INDEX > 5
                           MOVE FUNCTION NUMVAL(RAD-SIFFRA
                               (TABELL-INDEX,RAD-INDEX,SIFFER-INDEX))
                           TO REKNE-NUMMER
                           COMPUTE TOT-RAD-KOLUMN = TOT-RAD-KOLUMN
                           + REKNE-NUMMER
                       END-PERFORM

                   ELSE

                       PERFORM VARYING SIFFER-INDEX FROM 1 BY 1
                         UNTIL SIFFER-INDEX > 5
                           MOVE FUNCTION NUMVAL(RAD-SIFFRA
                               (TABELL-INDEX,RAD-INDEX,SIFFER-INDEX))
                           TO REKNE-NUMMER
                           COMPUTE TOT-OMARKERADE = TOT-OMARKERADE
                           + REKNE-NUMMER
                       END-PERFORM

                   END-IF
               END-PERFORM
           ELSE
               PERFORM VARYING SIFFER-INDEX FROM 1 BY 1
                   UNTIL SIFFER-INDEX > 5
                   IF SIFFER-INDEX = VINNANDE-KOLUMN
                       PERFORM VARYING RAD-INDEX FROM 1 BY 1
                         UNTIL RAD-INDEX > 5
                           MOVE FUNCTION NUMVAL(RAD-SIFFRA
                               (TABELL-INDEX,RAD-INDEX,SIFFER-INDEX))
                           TO REKNE-NUMMER
                           COMPUTE TOT-RAD-KOLUMN = TOT-RAD-KOLUMN
                           + REKNE-NUMMER
                       END-PERFORM

                   ELSE

                       PERFORM VARYING RAD-INDEX FROM 1 BY 1
                         UNTIL RAD-INDEX > 5
                           MOVE FUNCTION NUMVAL(RAD-SIFFRA
                               (TABELL-INDEX,RAD-INDEX,SIFFER-INDEX))
                           TO REKNE-NUMMER
                           COMPUTE TOT-OMARKERADE = TOT-OMARKERADE
                           + REKNE-NUMMER
                       END-PERFORM

                   END-IF
               END-PERFORM
           END-IF

           DISPLAY 'Vinnande tabell: ' BINGO-TABELL(VINNANDE-TABELL)

           DISPLAY 'Totalt bingo rad/kolumn: ' TOT-RAD-KOLUMN
           DISPLAY 'Totalt ??vrigt: ' TOT-OMARKERADE

           COMPUTE TOT-RESULTAT = TOT-RAD-KOLUMN * TOT-OMARKERADE

           DISPLAY 'Resultat: ' TOT-RESULTAT
           .

       N-AVSLUTA SECTION.

           CLOSE BINGOFIL

           STOP RUN
           .
