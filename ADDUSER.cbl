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
       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       01  ACCOUNT-RECORD.
           05 ACCOUNT-NO       PIC X(10).
           05 PIN              PIC X(4).
           05 ACCOUNT-NAME     PIC X(20).
           05 BALANCE          PIC 9(8)V99.

       WORKING-STORAGE SECTION.
       01  WS-EOF-FLAG      PIC X VALUE 'N'.

       01  WS-NEW-USER.
           05 WS-NEW-USER-ACCOUNT-NO      PIC X(10).
           05 WS-NEW-USER-PIN             PIC X(4).
           05 WS-NEW-USER-ACCOUNT-NAME    PIC X(20).
           05 WS-NEW-USER-BALANCE         PIC 9(8)V99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "ENTER NEW USER ACCOUNT : "
            ACCEPT WS-NEW-USER-ACCOUNT-NO
            DISPLAY "ENTER NEW USER PIN : "
            ACCEPT WS-NEW-USER-PIN
            DISPLAY "ENTER NEW USER NAME : "
            ACCEPT WS-NEW-USER-ACCOUNT-NAME
            DISPLAY "ENTER START MONEY : "
            ACCEPT WS-NEW-USER-BALANCE

            OPEN EXTEND ACCOUNT-FILE
            WRITE ACCOUNT-RECORD FROM WS-NEW-USER
            CLOSE ACCOUNT-FILE
            DISPLAY "ADDED NEW USER"

            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
