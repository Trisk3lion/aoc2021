       IDENTIFICATION DIVISION.

       PROGRAM-ID.SONAR.

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
          05 PREV-SIFFRA 9(4),
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

          MOVE SIFFRA TO PREV-SIFFRA

          READ SONARFIL
                AT END
                   SET END-OF-FILE TO TRUE
          END-READ
          .

       C-COUNT SECTION.

          PERFORM UNTIL END-OF-FILE

             IF SIFFRA > PREV-SIFFRA
                ADD 1 TO REKNARE
             END-IF

             READ SONARFIL
                AT END
                   SET END-OF-FILE TO TRUE
             END-READ

          END-PERFORM

          DISPLAY REKNARE

          call 'sonar3'

          .
       N-AVSLUTA SECTION.

          STOP RUN
          .
