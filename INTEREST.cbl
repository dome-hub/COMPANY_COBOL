      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO "ACCOUNT.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ACCOUNT-FILE-NEW ASSIGN TO "ACCOUNTS_NEW.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       01  ACCOUNT-RECORD.
           05 ACCOUNT-NO       PIC X(10).
           05 PIN              PIC X(4).
           05 ACCOUNT-NAME     PIC X(20).
           05 BALANCE          PIC 9(8)V99.

       FD  ACCOUNT-FILE-NEW.
       01  ACCOUNT-RECORD-NEW.
           05 ACCOUNT-NO-NEW       PIC X(10).
           05 PIN-NEW              PIC X(4).
           05 ACCOUNT-NAME-NEW    PIC X(20).
           05 BALANCE-NEW          PIC 9(8)V99.

       WORKING-STORAGE SECTION.
       01  WS-EOF-FLAG      PIC X VALUE 'N'.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "CALCULATING INTEREST ..."

            OPEN INPUT ACCOUNT-FILE
            OPEN OUTPUT ACCOUNT-FILE-NEW

            PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ ACCOUNT-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                   MOVE ACCOUNT-NO TO ACCOUNT-NO-NEW
                   MOVE PIN TO PIN-NEW
                   MOVE ACCOUNT-NAME TO ACCOUNT-NAME-NEW
                   COMPUTE BALANCE-NEW = BALANCE *1.015
                   WRITE ACCOUNT-RECORD-NEW
               END-READ
            END-PERFORM

            CLOSE ACCOUNT-FILE
            CLOSE ACCOUNT-FILE-NEW

            DISPLAY "CALCULATING COMPLETE "

            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
