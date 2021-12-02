       IDENTIFICATION DIVISION.

       PROGRAM-ID. SONAR3.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
          DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

          SELECT SONARFIL ASSIGN TO
             "input.txt"
          ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS IND1-FILESTATUS.

       DATA DIVISION.
       FILE SECTION.

       FD SONARFIL.
       01 SIFFRA PIC 9(5).

       WORKING-STORAGE SECTION.

       01 WS-VARIABLER.
          05 SIFFRA1 PIC 9(5).
          05 SIFFRA2 PIC 9(5).
          05 SIFFRA3 PIC 9(5).
          05 SUMMA1 PIC 9(9).
          05 SUMMA2 PIC 9(9).
          05 REKNARE PIC  9(4).

       01 END-OF-FILE-SW PIC 9.
          88 END-OF-FILE VALUE 1.

       01 W-FILESTATUSES.
          05 IND1-FILESTATUS PIC XX.

       PROCEDURE DIVISION.

       A-MAIN SECTION.

          PERFORM B-INIT
          PERFORM C-COUNT
          PERFORM N-AVSLUTA
          .
       B-INIT SECTION.

          INITIALIZE WS-VARIABLER

          OPEN INPUT SONARFIL

          READ SONARFIL
                AT END
                   SET END-OF-FILE TO TRUE
          END-READ

          MOVE SIFFRA TO SIFFRA1

          READ SONARFIL
                AT END
                   SET END-OF-FILE TO TRUE
          END-READ

          MOVE SIFFRA TO SIFFRA2

          READ SONARFIL
                AT END
                   SET END-OF-FILE TO TRUE
          END-READ

          MOVE SIFFRA TO SIFFRA3

          COMPUTE SUMMA1 = SIFFRA1 + SIFFRA2 + SIFFRA3

          READ SONARFIL
                AT END
                   SET END-OF-FILE TO TRUE
          END-READ
          .

       C-COUNT SECTION.

          PERFORM UNTIL END-OF-FILE

             MOVE SIFFRA2 TO SIFFRA1
             MOVE SIFFRA3 TO SIFFRA2
             MOVE SIFFRA TO SIFFRA3

             COMPUTE SUMMA2 = SIFFRA1 + SIFFRA2 + SIFFRA3

             IF SUMMA2 > SUMMA1
                ADD 1 TO REKNARE
             END-IF

             MOVE SUMMA2 TO SUMMA1

             READ SONARFIL
                AT END
                   SET END-OF-FILE TO TRUE
             END-READ

          END-PERFORM

          DISPLAY REKNARE
          .

       N-AVSLUTA SECTION.

          CLOSE SONARFIL

          STOP RUN
          .
