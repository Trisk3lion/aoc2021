       IDENTIFICATION DIVISION.

       PROGRAM-ID. SONAR2.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
          DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

          SELECT SONARFIL ASSIGN "input.txt"
          ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS IND1-FILESTATUS.

       DATA DIVISION.
       FILE SECTION.

       FD SONARFIL.
       01 WS-INPUT PIC X(10).

       WORKING-STORAGE SECTION.

       01 WS-VARIABLER.
          05 DJUP PIC 9(6).
          05 AIM PIC 9(6).
          05 FRAMDRIFT PIC 9(6).
          05 RIKTNING PIC X(10).
          05 SIFFRA PIC 99.
          05 SUMMA PIC 9(12).
          05 REKNARE1 PIC 9(3).
          05 REKNARE2 PIC 9(3).

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
          .

       C-COUNT SECTION.

          PERFORM UNTIL END-OF-FILE

             INSPECT WS-INPUT TALLYING
                REKNARE1 FOR CHARACTERS BEFORE SPACE
                REKNARE2 FOR CHARACTERS AFTER SPACE

             MOVE WS-INPUT(1:REKNARE1) TO RIKTNING
             MOVE WS-INPUT(REKNARE1 + 1:REKNARE2) TO SIFFRA

             EVALUATE RIKTNING
                WHEN 'down'
                   COMPUTE AIM = AIM + SIFFRA
                WHEN 'up'
                   COMPUTE AIM = AIM - SIFFRA
                WHEN 'forward'
                   COMPUTE FRAMDRIFT = FRAMDRIFT + SIFFRA
                   COMPUTE DJUP = DJUP + (AIM * SIFFRA)
             END-EVALUATE

             READ SONARFIL
                AT END
                   SET END-OF-FILE TO TRUE
             END-READ

             INITIALIZE REKNARE1
             INITIALIZE REKNARE2

          END-PERFORM

          COMPUTE SUMMA = FRAMDRIFT * DJUP

          DISPLAY SUMMA
          .

       N-AVSLUTA SECTION.

          CLOSE SONARFIL

          STOP RUN
          .
