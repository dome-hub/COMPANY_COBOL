      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATM-PROJECT.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO "ACCOUNT.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ACCOUNT-TMP ASSIGN TO "accountTMP.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRANSLOG-FILE ASSIGN TO "TRANSLOG.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       01  ACCOUNT-RECORD.
           05 ACCOUNT-NO       PIC X(10).
           05 PIN              PIC X(4).
           05 ACCOUNT-NAME     PIC X(20).
           05 BALANCE          PIC 9(8)V99.

       FD  ACCOUNT-TMP.
       01  ACCOUNT-TMP-RECORD.
           05 TEMP-ACCOUNT-NO       PIC X(10).
           05 TEMP-PIN              PIC X(4).
           05 TEMP-ACCOUNT-NAME     PIC X(20).
           05 TEMP-BALANCE          PIC 9(8)V99.

       FD  TRANSLOG-FILE.
       01  TRANSLOG-RECORD.
           05 TRANSLOG-ACCOUNT-NO       PIC X(10).
           05 TRANSLOG-DATE             PIC 9(8).
           05 TRANSLOG-TIME             PIC 9(6).
           05 TRANSLOG-TYPE             PIC X(1).
           05 TRANSLOG-BALANCE          PIC 9(8)V99.

       WORKING-STORAGE SECTION.
       01  WS-EOF-FLAG      PIC X VALUE 'N'.
       01  WS-EOF2-FLAG     PIC X VALUE 'N'.
       01  WS-FOUND-FLAG    PIC X VALUE 'N'.
       01  WS-TYPE          PIC X(1).

       01  WS-CHOICE        PIC X(1).

       01  WS-NEW-BALANCE   PIC 9(8)V99 VALUE 0.
       01  ACCOUNT-DATA.
           05 ACCOUNT-NUMBER  PIC X(11).
           05 ACCOUNT-PIN     PIC X(5).

       01  WS-CURRENT-BALANCE PIC 9(8)V99 VALUE 0.
       01  CURRENT-DATE-TIME.
           05  CURRENT-YEAR    PIC 9(4).
           05  CURRENT-MONTH   PIC 9(2).
           05  CURRENT-DAY     PIC 9(2).
           05  CURRENT-HOUR    PIC 9(2).
           05  CURRENT-MINUTE  PIC 9(2).
           05  CURRENT-SECOND  PIC 9(2).
           05  FILLER          PIC X(10).
       01  BANK-DATE           PIC X(19).

       01  TRANSLOG-CURRENT-DATE-TIME.
           05  TRANSLOG-CURRENT-DATE  PIC 9(8).
           05  TRANSLOG-CURRENT-TIME   PIC 9(6).
       01  TRANSLOG-BANK-DATE           PIC X(14).

      *>  deposit บัญชีคนอื่น
       01  WS-TARGET-ACCNO   PIC X(10).
       01  WS-TARGET-FOUND   PIC X VALUE 'N'.
       01  TARGET-CURR-BAL   PIC 9(8)V99 VALUE 0.
       01  TARGET-BALANCE    PIC 9(8)V99 VALUE 0.
       01  WS-SAVED-ACCNO    PIC X(11).
       01  WS-SAVED-BAL      PIC 9(8)V99.
       01  WS-SAVED-ACCNO-FILE PIC X(10).
       01  WS-SAVED-NAME       PIC X(20).
       01  WS-SAVED-PIN        PIC X(4).

       01  WS-AMOUNT       PIC 9(8)V99 VALUE 0.
       01  WS-AMOUNT-DISP  PIC ZZ,ZZZ,ZZZ.ZZ.
       01  WS-ANSWER            PIC X(1)     VALUE SPACE.
       01  ANSWER-RECEIPT      PIC X(1).
       01  WS-RECEIVER-NO PIC X(11).
       01  ANS             PIC A(1).
       01  CURRENT-ACCOUNT.
           05 CURRENT-ACCOUNT-NO    PIC X(10).
           05 CURRENT-PIN           PIC X(4).
           05 CURRENT-NAME          PIC X(20).
           05 CURRENT-BALANCE       PIC 9(8)V99.

       01  TRANSFER-DATA.
           05 TRANSFER-TO-ACCOUNT   PIC X(10).
           05 TRANSFER-AMOUNT       PIC 9(8)V99.
           05 TRANSFER-TO-NAME      PIC X(20).
           05 TRANSFER-TO-BALANCE   PIC 9(8)V99.
           05 TRANSFER-FOUND        PIC X VALUE 'N'.

       01  DISPLAY-FIELD.
           05 DISPLAY-BALANCE   PIC ZZ,ZZZ,ZZZ.ZZ.
           05 DISPLAY-ACC      PIC X(20).
           05 DISPLAY-ACC-NO  PIC X(10).
           05 DISPLAY-ACC-BALANCE  PIC 9(8)V99.

       01  DUMMY PIC X(1).

       PROCEDURE DIVISION.


       MAIN-PROCEDURE.
           PERFORM LOGIN.

           STOP RUN.
       LOGIN.
            DISPLAY "ENTER YOUR ACCOUNT NUMBER : "
            ACCEPT ACCOUNT-NUMBER.
            IF FUNCTION LENGTH(FUNCTION TRIM(ACCOUNT-NUMBER)) NOT = 10
               DISPLAY "ACCOUNT NUMBER must be exactly 10 digits!"
               GO TO LOGIN
            END-IF
            DISPLAY "ENTER YOUR PIN : "
            ACCEPT ACCOUNT-PIN.
            IF FUNCTION LENGTH(FUNCTION TRIM(ACCOUNT-PIN)) NOT = 4
               DISPLAY "PIN must be exactly 4 digits!"
               GO TO LOGIN
            END-IF
            OPEN INPUT ACCOUNT-FILE.
            PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ ACCOUNT-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       IF FUNCTION TRIM(ACCOUNT-NO) =
                           FUNCTION TRIM(ACCOUNT-NUMBER)
                       AND FUNCTION TRIM(PIN) =
                           FUNCTION TRIM(ACCOUNT-PIN)
                          THEN
                             MOVE 'Y' TO WS-FOUND-FLAG
                             MOVE 'Y' TO WS-EOF-FLAG
                       END-IF
               END-READ
            END-PERFORM.
            CLOSE ACCOUNT-FILE.

            IF WS-FOUND-FLAG = 'Y'
                DISPLAY "LOGIN SUCCESSFUL!"
                DISPLAY "----------------------------------------"
                DISPLAY "         WELCOME " ACCOUNT-NAME"        "
                DISPLAY "----------------------------------------"
                MOVE BALANCE TO DISPLAY-BALANCE
                PERFORM MENU
            ELSE
                DISPLAY "INVALID ACCOUNT OR PIN."
                MOVE 'N' TO WS-FOUND-FLAG
                MOVE 'N' TO WS-EOF-FLAG
                GO TO LOGIN
            END-IF.
      *>  MENU
       MENU.
            DISPLAY "PLEASE SELECT MENU"
            DISPLAY " 1. CHECK BALANCE".
            DISPLAY " 2. WITHDRAW "
            DISPLAY " 3. DEPOSIT"
            DISPLAY " 4. TRANSFER"
            DISPLAY " 5. EXIT".
            ACCEPT WS-CHOICE.
           EVALUATE WS-CHOICE
             WHEN 1  PERFORM CHECK-BALANCE
             WHEN 2  PERFORM WITHDRAW
             WHEN 3  PERFORM DEPOSIT
             WHEN 4  PERFORM TRANSFER
             WHEN 5  PERFORM EXIT-PROGRAM
             WHEN OTHER DISPLAY "Invalid choice "
             ",Press Enter To Try Again !"
           END-EVALUATE.

       CHECK-BALANCE.
           DISPLAY "========================================"
            DISPLAY "          COBOL BANK ATM SYSTEM.        "
            DISPLAY "========================================"
            DISPLAY SPACE.
            DISPLAY "BANK : THREE LITTLE PIGS BANK".
            DISPLAY "NAME : " ACCOUNT-NAME.
            DISPLAY "ACCOUNT NUMBER : "ACCOUNT-NO.
            DISPLAY SPACE.
            DISPLAY "----------------------------------------".
            DISPLAY " Available Balance : " DISPLAY-BALANCE " Bath".
            DISPLAY "----------------------------------------".
      *>       DISPLAY " Available Balance : "
            PERFORM FORMAT-DATE-TIME.
            PERFORM MENU.

       FORMAT-DATE-TIME.
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-TIME

           STRING CURRENT-YEAR DELIMITED BY SIZE
                  "-" DELIMITED BY SIZE
                  CURRENT-MONTH DELIMITED BY SIZE
                  "-" DELIMITED BY SIZE
                  CURRENT-DAY DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  CURRENT-HOUR DELIMITED BY SIZE
                  ":" DELIMITED BY SIZE
                  CURRENT-MINUTE DELIMITED BY SIZE
                  ":" DELIMITED BY SIZE
                  CURRENT-SECOND DELIMITED BY SIZE
                  INTO BANK-DATE
           END-STRING

           DISPLAY "DATE & TIME : " BANK-DATE.

       WITHDRAW.
           DISPLAY "----------------------------------------".
           DISPLAY "---------WELCOME " ACCOUNT-NAME"--------".
           DISPLAY "---------------WITHDRAW-----------------".
           DISPLAY "ENTER AMOUNT TO WITHDRAW : ".
           ACCEPT WS-AMOUNT.
           IF WS-AMOUNT <=0
               DISPLAY "INVALID AMOUNT MUSE BE > 0 "
               DISPLAY "PRESS ENTER TO RETURN MENU..."
               ACCEPT DUMMY
               PERFORM MENU
           END-IF

           IF WS-AMOUNT > BALANCE
               DISPLAY "NOT ENOUGHT MONEY TO WITHDRAW "
               MOVE BALANCE TO DISPLAY-BALANCE
               DISPLAY "CURRNE BALACNCE : " DISPLAY-BALANCE
               DISPLAY "PRESS ENTER TO RETURN MENU..."
               ACCEPT DUMMY
               PERFORM MENU EXIT PARAGRAPH

           END-IF

           MOVE WS-AMOUNT TO WS-AMOUNT-DISP.
           DISPLAY "COMFIRM WITHDRAW " WS-AMOUNT-DISP "? (Y/N) : "
               ACCEPT WS-ANSWER
           IF WS-ANSWER NOT = 'Y' AND WS-ANSWER NOT = 'y'
               DISPLAY "CANCELLED WITHDRAW"
               DISPLAY "PRESS ENTER TO RETURN MENU..."
               ACCEPT DUMMY
               PERFORM MENU EXIT PARAGRAPH
           END-IF

           SUBTRACT WS-AMOUNT FROM BALANCE


           MOVE WS-AMOUNT        TO WS-AMOUNT-DISP
           MOVE BALANCE          TO DISPLAY-BALANCE
           DISPLAY "----------------------------------------"
           DISPLAY "WITHDRAWN             : " WS-AMOUNT-DISP  "Bath"
           DISPLAY "REMANINING BALANCE    : " DISPLAY-BALANCE  "Bath"
           DISPLAY "STATUS                : WITHDRAW SUCCESSFUL"
           PERFORM FORMAT-DATE-TIME


           MOVE 'W' TO WS-TYPE.
           PERFORM TRANSLOG.
           PERFORM UPDATE-ACCOUNT-FILE

           DISPLAY " ".
           DISPLAY "DO YOU WANT RECIEPT ? (Y/N)"
           ACCEPT ANSWER-RECEIPT
           IF ANSWER-RECEIPT = 'Y' AND ANSWER-RECEIPT NOT = 'y'
                PERFORM PRINT-DEPOSIT-RECEIPT

           END-IF.
           DISPLAY "PRESS ENTER TO RETURN MENU..."
           ACCEPT DUMMY
           PERFORM MENU.

       DEPOSIT.
           DISPLAY "----------------------------------------".
           DISPLAY "---------WELCOME " ACCOUNT-NAME"--------".
           DISPLAY "---------------DEPOSIT-----------------".
           DISPLAY "WHICH ACCOUNT DO YOU WANT TO DEOSIT "
           DISPLAY "1. MY BANK ACCOUNT "
           DISPLAY "2. OTHER BANK ACCOUNT "
           ACCEPT WS-CHOICE.
           EVALUATE WS-CHOICE
             WHEN 1  PERFORM DEPOSIT-MY-ACCOUNT
             WHEN 2  PERFORM DEPOSIT-OTHERACCOUNT
             WHEN 3  PERFORM WITHDRAW
             WHEN 4  PERFORM DEPOSIT
             WHEN OTHER DISPLAY "Invalid choice "
             ",Press Enter To Try Again !"
           END-EVALUATE.

       DEPOSIT-MY-ACCOUNT.
           DISPLAY "ENTER AMOUNT TO DEPOSIT : ".
           ACCEPT WS-AMOUNT.

           IF WS-AMOUNT <=0
               DISPLAY "INVALID AMOUNT MUSE BE > 0 "
               DISPLAY "PRESS ENTER TO RETURN MENU..."
               ACCEPT DUMMY
               PERFORM MENU
           END-IF

           MOVE WS-AMOUNT TO WS-AMOUNT-DISP.
           DISPLAY "CONFIRM DEPOSIT " WS-AMOUNT-DISP " ? (Y/N): "
           ACCEPT WS-ANSWER

           IF WS-ANSWER NOT = 'Y' AND WS-ANSWER NOT = 'y'
               DISPLAY "CANCELLED DEPOSIT"
               DISPLAY "PRESS ENTER TO RETURN MENU..."
               ACCEPT DUMMY
               PERFORM MENU EXIT PARAGRAPH
           END-IF

           ADD WS-AMOUNT TO BALANCE
           MOVE WS-AMOUNT        TO WS-AMOUNT-DISP
           MOVE BALANCE          TO DISPLAY-BALANCE
           DISPLAY "----------------------------------------"
           DISPLAY "DEPOSITED        : " WS-AMOUNT-DISP "Bath"
           DISPLAY "NEW BALANCE      : " DISPLAY-BALANCE "Bath"

           DISPLAY "STATUS           : DEPOSIT SUCCESSFUL"
           PERFORM FORMAT-DATE-TIME

           PERFORM UPDATE-ACCOUNT-FILE

           MOVE 'D' TO WS-TYPE.
           PERFORM TRANSLOG.

           DISPLAY " ".
           DISPLAY "DO YOU WANT RECIEPT ? (Y/N)"
           ACCEPT ANSWER-RECEIPT
           IF ANSWER-RECEIPT = 'Y' AND ANSWER-RECEIPT NOT = 'y'
               PERFORM PRINT-DEPOSIT-RECEIPT
           END-IF.
           DISPLAY "PRESS ENTER TO RETURN MENU..." WITH NO ADVANCING
           ACCEPT DUMMY
           PERFORM MENU.

       DEPOSIT-OTHERACCOUNT.
           MOVE ACCOUNT-NO     TO WS-SAVED-ACCNO-FILE
           MOVE ACCOUNT-NAME   TO WS-SAVED-NAME
           MOVE PIN            TO WS-SAVED-PIN
           MOVE BALANCE        TO WS-SAVED-BAL
           MOVE ACCOUNT-NUMBER TO WS-SAVED-ACCNO

           DISPLAY "ENTER OTHER ACCOUNT NUMBER : "
           ACCEPT WS-TARGET-ACCNO
           IF FUNCTION LENGTH(FUNCTION TRIM(WS-TARGET-ACCNO)) NOT = 10
               DISPLAY "ACCOUNT NUMBER must be exactly 10 digits!"
               DISPLAY "PRESS ENTER TO RETURN MENU..."
               ACCEPT DUMMY
               PERFORM MENU
               EXIT PARAGRAPH
           END-IF

           DISPLAY "ENTER AMOUNT TO DEPOSIT : "
           ACCEPT WS-AMOUNT
           IF WS-AMOUNT <= 0
               DISPLAY "INVALID AMOUNT MUST BE > 0"
               ACCEPT DUMMY
               PERFORM RESTORE-LOGIN-RECORD
               PERFORM MENU
               EXIT PARAGRAPH
           END-IF

           MOVE 'N' TO WS-TARGET-FOUND
           MOVE 'N' TO WS-EOF-FLAG
           OPEN INPUT ACCOUNT-FILE
           PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ ACCOUNT-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       IF FUNCTION TRIM(ACCOUNT-NO) =
                          FUNCTION TRIM(WS-TARGET-ACCNO)
                          MOVE 'Y'     TO WS-TARGET-FOUND
                          MOVE BALANCE TO TARGET-CURR-BAL
                          EXIT PERFORM
                       END-IF
               END-READ
           END-PERFORM
           CLOSE ACCOUNT-FILE

           IF WS-TARGET-FOUND NOT = 'Y'
               DISPLAY "DESTINATION ACCOUNT NOT FOUND."
               DISPLAY "PRESS ENTER TO RETURN MENU..."
               ACCEPT DUMMY
               PERFORM RESTORE-LOGIN-RECORD
               PERFORM MENU
               EXIT PARAGRAPH
           END-IF

           COMPUTE TARGET-BALANCE = TARGET-CURR-BAL + WS-AMOUNT

           MOVE WS-AMOUNT TO WS-AMOUNT-DISP
           DISPLAY "CONFIRM DEPOSIT " WS-AMOUNT-DISP
                   " TO " WS-TARGET-ACCNO " ? (Y/N): "
           ACCEPT WS-ANSWER
           IF WS-ANSWER NOT = 'Y' AND WS-ANSWER NOT = 'y'
               DISPLAY "CANCELLED DEPOSIT"
               DISPLAY "PRESS ENTER TO RETURN MENU..."
               ACCEPT DUMMY
               PERFORM RESTORE-LOGIN-RECORD
               PERFORM MENU
               EXIT PARAGRAPH
           END-IF
           MOVE ACCOUNT-NUMBER TO WS-SAVED-ACCNO
           MOVE BALANCE        TO WS-SAVED-BAL


           MOVE FUNCTION TRIM(WS-TARGET-ACCNO) TO ACCOUNT-NUMBER
           *> ทำให้ BALANCE มี "ยอดใหม่ของบัญชีคนที่เราฝาก"
           MOVE TARGET-BALANCE TO BALANCE

           PERFORM UPDATE-ACCOUNT-FILE

           MOVE 'D' TO WS-TYPE.
           PERFORM TRANSLOG.

           *> คืนค่ากลับเป็นของผู้ล็อกอิน
           MOVE WS-SAVED-ACCNO TO ACCOUNT-NUMBER
           MOVE WS-SAVED-BAL   TO BALANCE

           DISPLAY "----------------------------------------"
           DISPLAY "DEPOSITED TO     : " WS-TARGET-ACCNO
           DISPLAY "AMOUNT           : " WS-AMOUNT-DISP "Bath"
           DISPLAY "STATUS           : SUCCESSFUL"
           PERFORM FORMAT-DATE-TIME

           MOVE 'D' TO WS-TYPE.


           DISPLAY "PRESS ENTER TO RETURN MENU..."
           ACCEPT DUMMY
           PERFORM MENU.
       RESTORE-LOGIN-RECORD.
           MOVE WS-SAVED-ACCNO-FILE TO ACCOUNT-NO
           MOVE WS-SAVED-NAME       TO ACCOUNT-NAME
           MOVE WS-SAVED-PIN        TO PIN
           MOVE WS-SAVED-BAL        TO BALANCE
           MOVE WS-SAVED-ACCNO      TO ACCOUNT-NUMBER
           MOVE BALANCE             TO DISPLAY-BALANCE
           EXIT PARAGRAPH.
       TRANSLOG.

           OPEN EXTEND TRANSLOG-FILE
               MOVE ACCOUNT-NUMBER TO TRANSLOG-ACCOUNT-NO

               PERFORM TRANSLOG-FORMAT-DATE-TIME

               MOVE WS-TYPE TO TRANSLOG-TYPE
               MOVE DISPLAY-BALANCE TO TRANSLOG-BALANCE
               WRITE TRANSLOG-RECORD
           CLOSE TRANSLOG-FILE.

      *>      แสตมเวลา log
       TRANSLOG-FORMAT-DATE-TIME.
           MOVE FUNCTION CURRENT-DATE TO TRANSLOG-CURRENT-DATE-TIME
           MOVE TRANSLOG-CURRENT-DATE TO TRANSLOG-DATE
           MOVE TRANSLOG-CURRENT-TIME TO TRANSLOG-TIME.

       UPDATE-ACCOUNT-FILE.
           MOVE BALANCE TO WS-NEW-BALANCE

           OPEN INPUT  ACCOUNT-FILE
           OPEN OUTPUT ACCOUNT-TMP
           MOVE 'N' TO WS-EOF2-FLAG
           PERFORM UNTIL WS-EOF2-FLAG = 'Y'
               READ ACCOUNT-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF2-FLAG
                   NOT AT END

                       MOVE ACCOUNT-NO       TO TEMP-ACCOUNT-NO
                       MOVE PIN              TO TEMP-PIN
                       MOVE ACCOUNT-NAME     TO TEMP-ACCOUNT-NAME


                       IF ACCOUNT-NO = ACCOUNT-NUMBER
                          MOVE WS-NEW-BALANCE TO TEMP-BALANCE
                       ELSE
                          MOVE BALANCE        TO TEMP-BALANCE
                       END-IF

                       WRITE ACCOUNT-TMP-RECORD
               END-READ
              END-PERFORM
           CLOSE ACCOUNT-FILE
           CLOSE ACCOUNT-TMP

           OPEN INPUT  ACCOUNT-TMP
           OPEN OUTPUT ACCOUNT-FILE
           MOVE 'N' TO WS-EOF2-FLAG
           PERFORM UNTIL WS-EOF2-FLAG = 'Y'
               READ ACCOUNT-TMP
                   AT END
                       MOVE 'Y' TO WS-EOF2-FLAG
                   NOT AT END
                       MOVE TEMP-ACCOUNT-NO     TO ACCOUNT-NO
                       MOVE TEMP-PIN            TO PIN
                       MOVE TEMP-ACCOUNT-NAME   TO ACCOUNT-NAME
                       MOVE TEMP-BALANCE        TO BALANCE
                       WRITE ACCOUNT-RECORD
               END-READ
           END-PERFORM

           CLOSE ACCOUNT-TMP
           CLOSE ACCOUNT-FILE

           EXIT PARAGRAPH.

           CLOSE ACCOUNT-FILE
           CLOSE ACCOUNT-TMP.


       TRANSFER.
           DISPLAY "----------------------------------------".
           DISPLAY "---------WELCOME " ACCOUNT-NAME "--------".
           DISPLAY "---------------TRANSFER----------------".

               DISPLAY "ENTER RECEIVER ACCOUNT NUMBER : ".
           ACCEPT WS-RECEIVER-NO.

               IF FUNCTION LENGTH(FUNCTION TRIM(WS-RECEIVER-NO)) NOT = 10
               DISPLAY "ACCOUNT NUMBER must be exactly 10 digits!"
               DISPLAY "PRESS ENTER TO RETURN MENU..."
               ACCEPT DUMMY
               PERFORM MENU
           END-IF.

               DISPLAY "ENTER AMOUNT TO TRANSFER : ".
           ACCEPT WS-AMOUNT.

               IF WS-AMOUNT <= 0
               DISPLAY "INVALID AMOUNT MUST BE > 0"
               DISPLAY "PRESS ENTER TO RETURN MENU..."
               ACCEPT DUMMY
               PERFORM MENU
           END-IF.

               IF WS-AMOUNT > BALANCE
               DISPLAY "NOT ENOUGH MONEY TO TRANSFER"
               DISPLAY "CURRENT BALANCE : " DISPLAY-BALANCE
               DISPLAY "PRESS ENTER TO RETURN MENU..."
               ACCEPT DUMMY
               PERFORM MENU
           END-IF.

               *> Confirm Transfer
           MOVE WS-AMOUNT TO WS-AMOUNT-DISP
           DISPLAY "CONFIRM TRANSFER "
           WS-AMOUNT-DISP " TO " WS-RECEIVER-NO
                   " ? (Y/N) : "
           ACCEPT WS-ANSWER

               IF WS-ANSWER NOT = 'Y' AND WS-ANSWER NOT = 'y'
               DISPLAY "CANCELLED TRANSFER"
               DISPLAY "PRESS ENTER TO RETURN MENU..."
               ACCEPT DUMMY
               PERFORM MENU
           END-IF.

               PERFORM UPDATE-TRANSFER-ACCOUNT-FILE

               *> Update display balance
           MOVE BALANCE TO DISPLAY-BALANCE
           DISPLAY "----------------------------------------"
           DISPLAY "TRANSFER SUCCESSFUL"
           DISPLAY "TRANSFERRED AMOUNT   : " WS-AMOUNT-DISP " Bath"
           DISPLAY "NEW BALANCE          : " DISPLAY-BALANCE " Bath"
           PERFORM FORMAT-DATE-TIME

               DISPLAY "PRESS ENTER TO RETURN MENU..."
           ACCEPT DUMMY
       PERFORM MENU.

       UPDATE-TRANSFER-ACCOUNT-FILE.
       OPEN INPUT ACCOUNT-FILE
           OPEN OUTPUT ACCOUNT-TMP
           MOVE 'N' TO WS-EOF2-FLAG

           PERFORM UNTIL WS-EOF2-FLAG = 'Y'
               READ ACCOUNT-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF2-FLAG
                   NOT AT END
                       MOVE ACCOUNT-NO TO TEMP-ACCOUNT-NO
                       MOVE PIN TO TEMP-PIN
                       MOVE ACCOUNT-NAME TO TEMP-ACCOUNT-NAME
                       MOVE BALANCE TO TEMP-BALANCE

                       IF ACCOUNT-NO = ACCOUNT-NUMBER
                           SUBTRACT WS-AMOUNT FROM TEMP-BALANCE
                       END-IF

                       IF ACCOUNT-NO = WS-RECEIVER-NO
                           ADD WS-AMOUNT TO TEMP-BALANCE
                       END-IF

                       WRITE ACCOUNT-TMP-RECORD
               END-READ
           END-PERFORM
           CLOSE ACCOUNT-FILE
           CLOSE ACCOUNT-TMP

           OPEN INPUT ACCOUNT-TMP
           OPEN OUTPUT ACCOUNT-FILE
           MOVE 'N' TO WS-EOF2-FLAG

           PERFORM UNTIL WS-EOF2-FLAG = 'Y'
               READ ACCOUNT-TMP
                   AT END
                       MOVE 'Y' TO WS-EOF2-FLAG
                   NOT AT END
                       MOVE TEMP-ACCOUNT-NO TO ACCOUNT-NO
                       MOVE TEMP-PIN TO PIN
                       MOVE TEMP-ACCOUNT-NAME TO ACCOUNT-NAME
                       MOVE TEMP-BALANCE TO BALANCE
                       WRITE ACCOUNT-RECORD
               END-READ
           END-PERFORM
           CLOSE ACCOUNT-TMP
           CLOSE ACCOUNT-FILE
           EXIT PARAGRAPH.

       PRINT-WITHDRAW-RECEIPT.
           DISPLAY "========================================"
           DISPLAY "           WITHDRAWAL RECEIPT           "
           DISPLAY "========================================"
           DISPLAY "BANK: THREE LITTLE PIGS BANK"
           DISPLAY "NAME: " DISPLAY-ACC
           DISPLAY "ACCOUNT: " DISPLAY-ACC-NO
           DISPLAY "----------------------------------------"
           MOVE WS-AMOUNT TO WS-AMOUNT-DISP
           MOVE WS-CURRENT-BALANCE TO DISPLAY-BALANCE
           DISPLAY "WITHDRAWN: " WS-AMOUNT-DISP " BATH"
           DISPLAY "REMAINING: " DISPLAY-BALANCE " BATH"
           PERFORM FORMAT-DATE-TIME
           DISPLAY "========================================"
           DISPLAY "Would you like to make another transaction?(Y/N)"
           ACCEPT ANS
           IF ANS = 'Y' OR ANS = 'y'
               PERFORM MENU
           ELSE IF ANS = 'N' OR ANs = 'n'
               STOP RUN
           END-IF.

       PRINT-DEPOSIT-OTHER-RECEIPT.
           DISPLAY "----------------------------------------"
           DISPLAY "           DEPOSIT RECEIPT (OTHER)"
           DISPLAY "----------------------------------------"
           DISPLAY "FROM ACCOUNT NO : " DISPLAY-ACC-NO
           DISPLAY "ACCOUNT NAME    : " DISPLAY-ACC
           MOVE WS-AMOUNT TO WS-AMOUNT-DISP
           DISPLAY "DEPOSIT AMOUNT  : " WS-AMOUNT-DISP " Bath"
           DISPLAY "TO ACCOUNT NO   : " TRANSFER-TO-ACCOUNT
           DISPLAY "ACCOUNT NAME    : " TRANSFER-TO-NAME
           MOVE TRANSFER-AMOUNT TO WS-AMOUNT-DISP
           DISPLAY "CREDITED AMOUNT : " WS-AMOUNT-DISP " Bath"
           MOVE WS-CURRENT-BALANCE TO DISPLAY-BALANCE
           DISPLAY "NEW BALANCE     : " DISPLAY-BALANCE " Bath"
           DISPLAY "----------------------------------------"
           DISPLAY "Would you like to make another transaction?(Y/N)"
           ACCEPT ANS
           IF ANS = 'Y' OR ANS = 'y'
               PERFORM MENU
           ELSE IF ANS = 'N' OR ANs = 'n'
               STOP RUN
           END-IF.

       PRINT-DEPOSIT-RECEIPT.
           DISPLAY "========================================"
           DISPLAY "            DEPOSIT RECEIPT             "
           DISPLAY "========================================"
           DISPLAY "BANK: THREE LITTLE PIGS BANK"
           DISPLAY "NAME: " DISPLAY-ACC
           DISPLAY "ACCOUNT: " DISPLAY-ACC-NO
           DISPLAY "----------------------------------------"
           MOVE WS-AMOUNT TO WS-AMOUNT-DISP
           MOVE WS-CURRENT-BALANCE TO DISPLAY-BALANCE
           DISPLAY "DEPOSITED: " WS-AMOUNT-DISP " BATH"
           DISPLAY "NEW BALANCE: " DISPLAY-BALANCE " BATH"
           PERFORM FORMAT-DATE-TIME
           DISPLAY "========================================"
           DISPLAY "Would you like to make another transaction?(Y/N)"
           ACCEPT ANS
           IF ANS = 'Y' OR ANS = 'y'
               PERFORM MENU
           ELSE IF ANS = 'N' OR ANs = 'n'
               STOP RUN
           END-IF.

       PRINT-TRANSFER-RECEIPT.
           DISPLAY "========================================"
           DISPLAY "           TRANSFER RECEIPT             "
           DISPLAY "========================================"
           DISPLAY "BANK: THREE LITTLE PIGS BANK"
           DISPLAY "FROM: " DISPLAY-ACC " (" DISPLAY-ACC-NO ")"
           DISPLAY "TO: " TRANSFER-TO-NAME " (" TRANSFER-TO-ACCOUNT ")"
           DISPLAY "----------------------------------------"
           MOVE TRANSFER-AMOUNT TO WS-AMOUNT-DISP
           MOVE WS-CURRENT-BALANCE TO DISPLAY-BALANCE
           DISPLAY "TRANSFERRED: " WS-AMOUNT-DISP " BATH"
           DISPLAY "REMAINING: " DISPLAY-BALANCE " BATH"
           PERFORM FORMAT-DATE-TIME
           DISPLAY "========================================"
           DISPLAY "Would you like to make another transaction?(Y/N)"
           ACCEPT ANS
           IF ANS = 'Y' OR ANS = 'y'
               PERFORM MENU
           ELSE IF ANS = 'N' OR ANs = 'n'
               STOP RUN
           END-IF.
       EXIT-PROGRAM.
           DISPLAY "==== THANK YOU FOR USING OUR SERVICES ====".
           STOP RUN.
       END PROGRAM ATM-PROJECT.
