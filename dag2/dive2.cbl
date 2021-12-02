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
       01 WS-INPUT X(10).

       WORKING-STORAGE SECTION.

       01 WS-VARIABLER.
          05 DJUP 9(4),
          05 AIM 9(4).
          05 FRAMDRIFT 9(4).
          05 RIKTNING X(10).
          05 SUMMA 9(9).
          05 REKNARE1 PIC 9(3).
          05 REKNARE2 PIC 9(3).

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
          .

       C-COUNT SECTION.

          PERFORM UNTIL END-OF-FILE

             INSPECT WS-INPUT TALLYING
                REKNARE1 FOR CHARACTERS BEFORE SPACE
                REKNARE2 FOR CHARACTERS AFTER SPACE

             MOVE WS-INPUT(1:REKNARE1) TO RIKTNING
             MOVE WS-INPUT(1 + REKNARE1:REKNARE2) TO SIFFRA

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

          END-PERFORM

          COMPUTE SUMMA = FRAMDRIFT * DJUP

          DISPLAY SUMMA
          .

       N-AVSLUTA SECTION.

          STOP RUN
          .
