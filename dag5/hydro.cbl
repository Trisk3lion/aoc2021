       IDENTIFICATION DIVISION.

       PROGRAM-ID. HYDRO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT HYDROFIL ASSIGN "input.txt"
           ORGANIZATION IS LINE SEQUENTIAL
             FILE STATUS IS IND1-FILESTATUS.

       DATA DIVISION.
       FILE SECTION.

       FD HYDROFIL.
       01 WS-INPUT PIC X(20).

       WORKING-STORAGE SECTION.

       01 KORDINATER.
           05 X1 PIC 9(3).
           05 Y1 PIC 9(3).
           05 X2 PIC 9(3).
           05 Y2 PIC 9(3).

       01 KOMP-KORDINATER
           05 K-X1 PIC 9(3).
           05 K-Y1 PIC 9(3).
           05 K-X2 PIC 9(3).
           05 K-Y2 PIC 9(3).

       01 VEKTOR-X-TABELL.
           05 X-LIKA-TABELL OCCURS 1 TO 100 TIMES
                          DEPENDING ON ANTAL-X-VEKTORER
                          INDEXED BY X-INDEX.
           10 X-X1 PIC 9(3).
           10 X-Y1 PIC 9(3).
           10 X-X2 PIC 9(3).
           10 X Y2 PIC 9(3).

       01 VEKTOR-Y-TABELL.
           05 Y-LIKA-TABELL OCCURS 1 TO 100 TIMES
                          DEPENDING ON ANTAL-Y-VEKTORER
                          INDEXED BY Y-INDEX.
           10 Y-X1 PIC 9(3).
           10 Y-Y1 PIC 9(3).
           10 Y-X2 PIC 9(3).
           10 Y Y2 PIC 9(3).


       PROCEDURE DIVISION.

       A-MAIN SECTION.

           PERFORM B-INIT
           PERFORM C-TA-INPUT UNTIL FIL-SLUT
           PERFORM N-AVSLUTA
           .

       B-INIT SECTION.

           OPEN INPUT HYDROFIL

           READ HYDROFIL
           AT END
               SET FIL-SLUT TO TRUE
           END-READ

           IF X1 = X2
               ADD 1 TO ANTAL-X-VEKTORER
               MOVE X1 TO X-X1
               MOVE X2 TO X-X2
               MOVE Y1 TO X-Y1
               MOVE Y2 TO X-Y2
           END-IF

           IF Y1 = Y2
               ADD 1 TO ANTAL-Y-VEKTORER
               MOVE X1 TO Y-X1
               MOVE X2 TO Y-X2
               MOVE Y1 TO Y-Y1
               MOVE Y2 TO Y-Y2
           END-IF

           READ HYDROFIL
           AT END
               SET FIL-SLUT TO TRUE
           END-READ
           .

       C-TA-INPUT SECTION.

           UNSTRING WS-INPUT
           DELIMITED BY ',' OR SPACE
           INTO X1 Y1 FILLER X2 Y2

           IF X1 = X2

               PERFORM CA-JEMNFORA-MOT-Y
               ADD 1 TO ANTAL-X-VEKTORER
               MOVE X1 TO X-X1
               MOVE X2 TO X-X2
               MOVE Y1 TO X-Y1
               MOVE Y2 TO X-Y2
           END-IF

           IF Y1 = Y2

               PERFORM CB-JEMNFORA-MOT-X
               ADD 1 TO ANTAL-Y-VEKTORER
               MOVE X1 TO Y-X1
               MOVE X2 TO Y-X2
               MOVE Y1 TO Y-Y1
               MOVE Y2 TO Y-Y2
           END-IF
           .

       CA-JEMNFORA-MOT-Y SECTION.

           PERFORM VARYING Y-INDEX FROM 1 BY 1
               UNTIL Y-INDEX > ANTAL-Y-VEKTORER
               MOVE Y-LIKA-TABELL(Y-INDEX) TO KOMP-KORDINATER
               PERFORM D-JEMNFORA-VEKTORER
               END-PERFORM
           .


       CB-JEMNFORA-MOT-X SECTION.

           PERFORM VARYING X-INDEX FROM 1 BY 1
               UNTIL X-INDEX > ANTAL-X-VEKTORER
               MOVE X-LIKA-TABELL(X-INDEX) TO KOMP-KORDINATER
               PERFORM D-JEMNFORA-VEKTORER
               END-PERFORM



           .

       D-JEMNFORA-VEKTORER SECTION.



           .


       N-AVSLUTA SECTION.

           CLOSE HYDROFIL

           STOP RUN.
           .
