       IDENTIFICATION DIVISION.

       PROGRAM-ID. SONAR.

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
       01 SIFFRA PIC 9(4).

       WORKING-STORAGE SECTION.

       01 WS-VARIABLER.
      *    05 SIFFRA PIC 9(4).
          05 PREV-SIFFRA PIC 9(4).
          05 REKNARE PIC 9(4).

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

          DISPLAY "Filestatus is: " IND1-FILESTATUS

          DISPLAY "Forsta input: " SIFFRA

          MOVE SIFFRA TO PREV-SIFFRA

          READ SONARFIL
                AT END
                   SET END-OF-FILE TO TRUE
          END-READ

          DISPLAY "Andra input: " SIFFRA

                 .

       C-COUNT SECTION.

          PERFORM UNTIL END-OF-FILE

             IF SIFFRA > PREV-SIFFRA
                ADD 1 TO REKNARE
             END-IF

             MOVE SIFFRA TO PREV-SIFFRA

             READ SONARFIL
                AT END
                   SET END-OF-FILE TO TRUE
             END-READ
          END-PERFORM

          display "Sista Siffra: " SIFFRA
          display "sista Prev siffra: " prev-siffra

          DISPLAY "Antal: " REKNARE
          .

       N-AVSLUTA SECTION.

          CLOSE SONARFIL

          STOP RUN
          .
