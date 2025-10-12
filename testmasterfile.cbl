******************************************************************
      * Author:      Test Master File Display
      * Date:        2025
      * Purpose:     Display all employees from master file
      * Tectonics:   cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTMASTERFILE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-MASTER-FILE ASSIGN TO "EMPLOYEE.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS EMP-ID.

       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE-MASTER-FILE.
       01 EMPLOYEE-MASTER-FILE-RECORD.
           05 EMP-ID      PIC 9(5).
           05 EMP-NAME    PIC X(30).
           05 EMP-DEPART  PIC X(10).
           05 EMP-SALARY  PIC 9(7)V99.

       WORKING-STORAGE SECTION.
       01 WS-EOF-FLAG     PIC X VALUE 'N'.
       01 WS-INDEX        PIC 9(3) VALUE 0.
       01 WS-IND-FROM     PIC ZZ9.
       01 WS-SALA-FROM    PIC ZZ,ZZZ,ZZ9.99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT EMPLOYEE-MASTER-FILE.
           MOVE 'N' TO WS-EOF-FLAG.
           MOVE 0 TO WS-INDEX.

           DISPLAY "===================================================".
           DISPLAY "         EMPLOYEE MASTER FILE DISPLAY              ".
           DISPLAY "===================================================".
           DISPLAY " ".

           PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ EMPLOYEE-MASTER-FILE NEXT RECORD
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       ADD 1 TO WS-INDEX
                       MOVE WS-INDEX TO WS-IND-FROM
                       MOVE EMP-SALARY TO WS-SALA-FROM
                       DISPLAY WS-IND-FROM ". ID: " EMP-ID
                           ", Name: " EMP-NAME
                           ", Department: " EMP-DEPART
                           ", Salary: " WS-SALA-FROM
               END-READ
           END-PERFORM.

           DISPLAY " ".
           DISPLAY "===================================================".
           DISPLAY "Total Employees: " WS-INDEX.
           DISPLAY "===================================================".

           CLOSE EMPLOYEE-MASTER-FILE.
           STOP RUN.
       END PROGRAM TESTMASTERFILE.
