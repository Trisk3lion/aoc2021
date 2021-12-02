       IDENTIFICATION DIVISION.

       PROGRAM-ID.SONAR3.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
          DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

          SELECT SONARFIL ASSIGN INDAG1
           FILE STATUS IS INDGA1-FILESTATUS.

       DATA DIVISION.
       FILE SECTION.

       FD SONARFIL RECORDING MODE V.
       01 SIFFRA 9(4).

       WORKING-STORAGE SECTION.

       01 WS-VARIABLER.
          05 SIFFRA1 9(4).
          05 SIFFRA2 9(4),
          05 SIFFRA3 9(4).
          05 SUMMA1 9(4).
          05 SUMMA2 9(4).
          05 REKNARE 9(4).

       01 END-OF-FILE-SW PIC 9.
          88 END-OF-FILE VALUE 1.

       PROCEDURE DIVISION.

       A-MAIN SECTION.

          PERFORM B-INIT
          PERFORM C-COUNT
          PERFORM N-AVSLUTA
          .
       B-INIT SECTION.

          INITIALIZE WS-VARIABLER

          OPEN OUTPUT SONARFIL

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

             READ SONARFIL
                AT END
                   SET END-OF-FILE TO TRUE
             END-READ

             MOVE SUMMA2 TO SUMMA1

          END-PERFORM

          DISPLAY REKNARE
          .

       N-AVSLUTA SECTION.

          STOP RUN
          .
