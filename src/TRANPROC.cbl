      *================================================================*
      * Program:    TRANPROC                                           *
      * Purpose:    Transaction Processing Subprogram                  *
      * Concepts:   Sequential file I/O, CORRESPONDING,               *
      *             nested programs, COMPUTE with complex expressions, *
      *             ON SIZE ERROR, ROUNDED, arithmetic verbs,          *
      *             multiple file handling, SORT, MERGE concepts,      *
      *             USE AFTER EXCEPTION/ERROR (declaratives),          *
      *             reference modification, STRING/UNSTRING            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRANPROC.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE
               ASSIGN TO 'data/ACCOUNTS.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AF-ACCT-NUMBER
               FILE STATUS IS WS-ACCT-FILE-STATUS.

           SELECT TRANSACTION-FILE
               ASSIGN TO 'data/TRANSLOG.dat'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-TRAN-FILE-STATUS.

       DATA DIVISION.

       FILE SECTION.
       FD  ACCOUNT-FILE.
       01  ACCOUNT-FILE-RECORD.
           05  AF-ACCT-NUMBER          PIC 9(10).
           05  AF-ACCT-TYPE            PIC X(1).
           05  AF-ACCT-STATUS          PIC X(1).
           05  AF-HOLDER-LAST-NAME     PIC X(25).
           05  AF-HOLDER-FIRST-NAME    PIC X(20).
           05  AF-HOLDER-MI            PIC X(1).
           05  AF-HOLDER-SSN           PIC 9(9).
           05  AF-HOLDER-DOB           PIC 9(8).
           05  AF-HOLDER-PHONE         PIC 9(10).
           05  AF-ADDR-STREET          PIC X(30).
           05  AF-ADDR-CITY            PIC X(20).
           05  AF-ADDR-STATE           PIC X(2).
           05  AF-ADDR-ZIP             PIC 9(5).
           05  AF-CURRENT-BALANCE      PIC S9(11)V99.
           05  AF-AVAILABLE-BALANCE    PIC S9(11)V99.
           05  AF-INTEREST-RATE        PIC 9V9(4).
           05  AF-ACCRUED-INTEREST     PIC S9(9)V99.
           05  AF-OVERDRAFT-LIMIT      PIC 9(7)V99.
           05  AF-MIN-BALANCE          PIC 9(7)V99.
           05  AF-DATE-OPENED          PIC 9(8).
           05  AF-DATE-CLOSED          PIC 9(8).
           05  AF-LAST-TRANS-DATE      PIC 9(8).
           05  AF-LAST-STMT-DATE       PIC 9(8).
           05  AF-MONTHLY-TRANS-CT     PIC 9(5).
           05  AF-YTD-DEPOSITS         PIC S9(11)V99.
           05  AF-YTD-WITHDRAWALS      PIC S9(11)V99.
           05  AF-FILLER               PIC X(20).

       FD  TRANSACTION-FILE.
       01  TRANSACTION-FILE-RECORD.
           05  TF-TRANS-ID             PIC 9(12).
           05  TF-TRANS-DATE           PIC 9(8).
           05  TF-TRANS-TIME           PIC 9(6).
           05  TF-TRANS-TYPE           PIC X(2).
           05  TF-TRANS-ACCT-FROM      PIC 9(10).
           05  TF-TRANS-ACCT-TO        PIC 9(10).
           05  TF-TRANS-AMOUNT         PIC S9(11)V99.
           05  TF-TRANS-BALANCE-AFTER  PIC S9(11)V99.
           05  TF-TRANS-STATUS         PIC X(1).
           05  TF-TRANS-DESCRIPTION    PIC X(40).
           05  TF-TRANS-REF-NUMBER     PIC X(15).
           05  TF-TRANS-TELLER-ID      PIC X(6).
           05  TF-FILLER               PIC X(10).

       WORKING-STORAGE SECTION.

       01  WS-ACCT-FILE-STATUS         PIC X(2).
       01  WS-TRAN-FILE-STATUS         PIC X(2).
       01  WS-ACCT-FILE-OPEN           PIC X VALUE 'N'.
       01  WS-TRAN-FILE-OPEN           PIC X VALUE 'N'.

      *----------------------------------------------------------------*
      * Transaction processing fields                                  *
      *----------------------------------------------------------------*
       01  WS-PROCESS-FIELDS.
           05  WS-INPUT-ACCT-NUM       PIC 9(10).
           05  WS-INPUT-ACCT-TO        PIC 9(10).
           05  WS-INPUT-AMOUNT         PIC S9(11)V99.
           05  WS-INPUT-CONFIRM        PIC X.
           05  WS-INPUT-DESC           PIC X(40).
           05  WS-NEW-BALANCE          PIC S9(11)V99.
           05  WS-OVERDRAFT-USED       PIC S9(7)V99.
           05  WS-FEE-AMOUNT           PIC S9(5)V99.

      *----------------------------------------------------------------*
      * Transaction ID generation (demonstrates COMPUTE)               *
      *----------------------------------------------------------------*
       01  WS-TRANS-COUNTER            PIC 9(6) VALUE 0.
       01  WS-GENERATED-TRANS-ID       PIC 9(12).

      *----------------------------------------------------------------*
      * Daily limits and fee schedule (demonstrates VALUE tables)      *
      *----------------------------------------------------------------*
       01  WS-DAILY-LIMITS.
           05  WS-MAX-WITHDRAWAL       PIC 9(7)V99 VALUE 10000.00.
           05  WS-MAX-TRANSFER         PIC 9(7)V99 VALUE 50000.00.
           05  WS-DAILY-WDRW-TOTAL     PIC 9(7)V99 VALUE 0.

       01  WS-FEE-SCHEDULE.
           05  WS-OVERDRAFT-FEE        PIC 9(3)V99 VALUE 035.00.
           05  WS-BELOW-MIN-FEE        PIC 9(3)V99 VALUE 015.00.
           05  WS-EXCESS-TRANS-FEE     PIC 9(3)V99 VALUE 002.50.
           05  WS-WIRE-TRANSFER-FEE    PIC 9(3)V99 VALUE 025.00.
           05  WS-FREE-TRANS-LIMIT     PIC 9(3) VALUE 006.

      *----------------------------------------------------------------*
      * Display fields                                                 *
      *----------------------------------------------------------------*
       01  WS-DISPLAY-BALANCE          PIC $ZZZ,ZZZ,ZZ9.99-.
       01  WS-DISPLAY-AMOUNT           PIC $ZZZ,ZZZ,ZZ9.99-.
       01  WS-DISPLAY-FEE              PIC $ZZ9.99.
       01  WS-SEPARATOR                PIC X(60) VALUE ALL '-'.

      *----------------------------------------------------------------*
      * Transaction history table (for in-memory display)              *
      *----------------------------------------------------------------*
       01  WS-HISTORY-TABLE.
           05  WS-HIST-COUNT           PIC 9(3) VALUE 0.
           05  WS-HIST-ENTRY OCCURS 50 TIMES
               INDEXED BY WS-HIST-IDX.
               10  WS-HIST-DATE        PIC 9(8).
               10  WS-HIST-TYPE        PIC X(2).
               10  WS-HIST-AMOUNT      PIC S9(11)V99.
               10  WS-HIST-BALANCE     PIC S9(11)V99.
               10  WS-HIST-DESC        PIC X(40).
               10  WS-HIST-STATUS      PIC X(1).

       01  WS-TEMP-DATE                PIC 9(8).
       01  WS-TEMP-TIME                PIC 9(6).

      *================================================================*
      * LINKAGE SECTION                                                *
      *================================================================*
       LINKAGE SECTION.

       01  LS-FUNCTION-CODE           PIC X(2).

       01  LS-ACCOUNT-RECORD.
           05  LS-ACCT-NUMBER          PIC 9(10).
           05  LS-ACCT-TYPE            PIC X(1).
           05  LS-ACCT-STATUS          PIC X(1).
           05  LS-ACCT-HOLDER-INFO.
               10  LS-HOLDER-LAST-NAME   PIC X(25).
               10  LS-HOLDER-FIRST-NAME  PIC X(20).
               10  LS-HOLDER-MI          PIC X(1).
               10  LS-HOLDER-SSN         PIC 9(9).
               10  LS-HOLDER-DOB         PIC 9(8).
               10  LS-HOLDER-PHONE       PIC 9(10).
           05  LS-ACCT-ADDRESS.
               10  LS-ADDR-STREET       PIC X(30).
               10  LS-ADDR-CITY         PIC X(20).
               10  LS-ADDR-STATE        PIC X(2).
               10  LS-ADDR-ZIP          PIC 9(5).
           05  LS-ACCT-FINANCIALS.
               10  LS-CURRENT-BALANCE   PIC S9(11)V99.
               10  LS-AVAILABLE-BALANCE PIC S9(11)V99.
               10  LS-INTEREST-RATE     PIC 9V9(4).
               10  LS-ACCRUED-INTEREST  PIC S9(9)V99.
               10  LS-OVERDRAFT-LIMIT   PIC 9(7)V99.
               10  LS-MIN-BALANCE       PIC 9(7)V99.
           05  LS-ACCT-DATES.
               10  LS-DATE-OPENED       PIC 9(8).
               10  LS-DATE-CLOSED       PIC 9(8).
               10  LS-LAST-TRANS-DATE   PIC 9(8).
               10  LS-LAST-STMT-DATE    PIC 9(8).
           05  LS-ACCT-COUNTERS.
               10  LS-MONTHLY-TRANS-CT  PIC 9(5).
               10  LS-YTD-DEPOSITS      PIC S9(11)V99.
               10  LS-YTD-WITHDRAWALS   PIC S9(11)V99.
           05  FILLER                    PIC X(20).

       01  LS-TRANSACTION-RECORD.
           05  LS-TRANS-ID             PIC 9(12).
           05  LS-TRANS-DATE           PIC 9(8).
           05  LS-TRANS-TIME           PIC 9(6).
           05  LS-TRANS-TYPE           PIC X(2).
           05  LS-TRANS-ACCT-FROM      PIC 9(10).
           05  LS-TRANS-ACCT-TO        PIC 9(10).
           05  LS-TRANS-AMOUNT         PIC S9(11)V99.
           05  LS-TRANS-BALANCE-AFTER  PIC S9(11)V99.
           05  LS-TRANS-STATUS         PIC X(1).
           05  LS-TRANS-DESCRIPTION    PIC X(40).
           05  LS-TRANS-REF-NUMBER     PIC X(15).
           05  LS-TRANS-TELLER-ID      PIC X(6).
           05  FILLER                   PIC X(10).

       01  LS-ERROR-HANDLING.
           05  LS-ERROR-CODE           PIC 9(4).
           05  LS-ERROR-MESSAGE        PIC X(60).
           05  LS-ERROR-SEVERITY       PIC 9.
           05  LS-ERROR-MODULE         PIC X(8).
           05  LS-ERROR-PARAGRAPH      PIC X(20).

       01  LS-TELLER-INFO.
           05  LS-TELLER-ID            PIC X(6).
           05  LS-TELLER-NAME          PIC X(30).
           05  LS-TELLER-LEVEL         PIC 9.
           05  LS-TELLER-FULL-ID       PIC X(40).

       PROCEDURE DIVISION USING
           LS-FUNCTION-CODE
           LS-ACCOUNT-RECORD
           LS-TRANSACTION-RECORD
           LS-ERROR-HANDLING
           LS-TELLER-INFO.

       0000-MAIN-ENTRY.
           MOVE 0000 TO LS-ERROR-CODE
           MOVE SPACES TO LS-ERROR-MESSAGE
           MOVE 'TRANPROC' TO LS-ERROR-MODULE

           PERFORM 0100-OPEN-FILES

           IF WS-ACCT-FILE-OPEN = 'Y'
               EVALUATE LS-FUNCTION-CODE
                   WHEN 'DP'
                       PERFORM 1000-PROCESS-DEPOSIT
                   WHEN 'WD'
                       PERFORM 2000-PROCESS-WITHDRAWAL
                   WHEN 'TR'
                       PERFORM 3000-PROCESS-TRANSFER
                   WHEN 'BQ'
                       PERFORM 4000-BALANCE-INQUIRY
                   WHEN 'TH'
                       PERFORM 5000-TRANSACTION-HISTORY
                   WHEN OTHER
                       MOVE 9002 TO LS-ERROR-CODE
                       MOVE 'Invalid transaction function'
                           TO LS-ERROR-MESSAGE
                       MOVE 2 TO LS-ERROR-SEVERITY
               END-EVALUATE

               PERFORM 0200-CLOSE-FILES
           END-IF

           GOBACK
           .

      *================================================================*
      * FILE OPERATIONS                                                *
      *================================================================*
       0100-OPEN-FILES.
           OPEN I-O ACCOUNT-FILE
           IF WS-ACCT-FILE-STATUS = '00'
               MOVE 'Y' TO WS-ACCT-FILE-OPEN
           ELSE
               IF WS-ACCT-FILE-STATUS = '35'
                   OPEN OUTPUT ACCOUNT-FILE
                   CLOSE ACCOUNT-FILE
                   OPEN I-O ACCOUNT-FILE
                   IF WS-ACCT-FILE-STATUS = '00'
                       MOVE 'Y' TO WS-ACCT-FILE-OPEN
                   END-IF
               END-IF
               IF WS-ACCT-FILE-OPEN NOT = 'Y'
                   MOVE 9001 TO LS-ERROR-CODE
                   MOVE 'Cannot open account file'
                       TO LS-ERROR-MESSAGE
                   MOVE 3 TO LS-ERROR-SEVERITY
               END-IF
           END-IF

           IF WS-ACCT-FILE-OPEN = 'Y'
               OPEN EXTEND TRANSACTION-FILE
               IF WS-TRAN-FILE-STATUS = '00'
                   MOVE 'Y' TO WS-TRAN-FILE-OPEN
               ELSE
                   IF WS-TRAN-FILE-STATUS = '35'
                       OPEN OUTPUT TRANSACTION-FILE
                       IF WS-TRAN-FILE-STATUS = '00'
                           MOVE 'Y' TO WS-TRAN-FILE-OPEN
                       END-IF
                   END-IF
               END-IF
           END-IF
           .

       0200-CLOSE-FILES.
           IF WS-ACCT-FILE-OPEN = 'Y'
               CLOSE ACCOUNT-FILE
               MOVE 'N' TO WS-ACCT-FILE-OPEN
           END-IF
           IF WS-TRAN-FILE-OPEN = 'Y'
               CLOSE TRANSACTION-FILE
               MOVE 'N' TO WS-TRAN-FILE-OPEN
           END-IF
           .

      *----------------------------------------------------------------*
      * Read account from indexed file                                 *
      *----------------------------------------------------------------*
       0300-READ-ACCOUNT.
           MOVE WS-INPUT-ACCT-NUM TO AF-ACCT-NUMBER
           READ ACCOUNT-FILE
               KEY IS AF-ACCT-NUMBER
           END-READ

           IF WS-ACCT-FILE-STATUS = '00'
               MOVE AF-ACCT-NUMBER      TO LS-ACCT-NUMBER
               MOVE AF-ACCT-TYPE        TO LS-ACCT-TYPE
               MOVE AF-ACCT-STATUS      TO LS-ACCT-STATUS
               MOVE AF-HOLDER-LAST-NAME TO LS-HOLDER-LAST-NAME
               MOVE AF-HOLDER-FIRST-NAME TO LS-HOLDER-FIRST-NAME
               MOVE AF-CURRENT-BALANCE  TO LS-CURRENT-BALANCE
               MOVE AF-AVAILABLE-BALANCE TO LS-AVAILABLE-BALANCE
               MOVE AF-INTEREST-RATE    TO LS-INTEREST-RATE
               MOVE AF-OVERDRAFT-LIMIT  TO LS-OVERDRAFT-LIMIT
               MOVE AF-MIN-BALANCE      TO LS-MIN-BALANCE
               MOVE AF-MONTHLY-TRANS-CT TO LS-MONTHLY-TRANS-CT
               MOVE AF-YTD-DEPOSITS     TO LS-YTD-DEPOSITS
               MOVE AF-YTD-WITHDRAWALS  TO LS-YTD-WITHDRAWALS
               MOVE AF-DATE-OPENED      TO LS-DATE-OPENED
               MOVE AF-LAST-TRANS-DATE  TO LS-LAST-TRANS-DATE
           ELSE
               IF WS-ACCT-FILE-STATUS = '23'
                   MOVE 1001 TO LS-ERROR-CODE
                   MOVE 'Account not found'
                       TO LS-ERROR-MESSAGE
                   MOVE 2 TO LS-ERROR-SEVERITY
               ELSE
                   MOVE 9001 TO LS-ERROR-CODE
                   STRING 'Account read error: '
                       DELIMITED SIZE
                       WS-ACCT-FILE-STATUS DELIMITED SIZE
                       INTO LS-ERROR-MESSAGE
                   END-STRING
                   MOVE 3 TO LS-ERROR-SEVERITY
               END-IF
           END-IF
           .

      *----------------------------------------------------------------*
      * Update account in indexed file                                 *
      *----------------------------------------------------------------*
       0400-UPDATE-ACCOUNT.
           MOVE LS-ACCT-NUMBER      TO AF-ACCT-NUMBER
           MOVE LS-CURRENT-BALANCE  TO AF-CURRENT-BALANCE
           MOVE LS-AVAILABLE-BALANCE TO AF-AVAILABLE-BALANCE
           MOVE LS-MONTHLY-TRANS-CT TO AF-MONTHLY-TRANS-CT
           MOVE LS-YTD-DEPOSITS     TO AF-YTD-DEPOSITS
           MOVE LS-YTD-WITHDRAWALS  TO AF-YTD-WITHDRAWALS
           MOVE FUNCTION CURRENT-DATE(1:8)
               TO AF-LAST-TRANS-DATE

           REWRITE ACCOUNT-FILE-RECORD
           IF WS-ACCT-FILE-STATUS NOT = '00'
               MOVE 9001 TO LS-ERROR-CODE
               MOVE 'Failed to update account'
                   TO LS-ERROR-MESSAGE
               MOVE 3 TO LS-ERROR-SEVERITY
           END-IF
           .

      *----------------------------------------------------------------*
      * Generate unique transaction ID                                 *
      *----------------------------------------------------------------*
       0500-GENERATE-TRANS-ID.
           ADD 1 TO WS-TRANS-COUNTER
           MOVE FUNCTION CURRENT-DATE(1:8)
               TO WS-TEMP-DATE
           MOVE FUNCTION CURRENT-DATE(9:6)
               TO WS-TEMP-TIME

           COMPUTE WS-GENERATED-TRANS-ID =
               WS-TEMP-DATE * 10000 + WS-TRANS-COUNTER
               ON SIZE ERROR
                   COMPUTE WS-GENERATED-TRANS-ID =
                       FUNCTION RANDOM * 999999999999
               NOT ON SIZE ERROR
                   CONTINUE
           END-COMPUTE

           MOVE WS-GENERATED-TRANS-ID TO LS-TRANS-ID
           MOVE WS-TEMP-DATE TO LS-TRANS-DATE
           MOVE WS-TEMP-TIME TO LS-TRANS-TIME
           .

      *----------------------------------------------------------------*
      * Write transaction log entry                                    *
      *----------------------------------------------------------------*
       0600-WRITE-TRANS-LOG.
           IF WS-TRAN-FILE-OPEN = 'Y'
               MOVE LS-TRANS-ID       TO TF-TRANS-ID
               MOVE LS-TRANS-DATE     TO TF-TRANS-DATE
               MOVE LS-TRANS-TIME     TO TF-TRANS-TIME
               MOVE LS-TRANS-TYPE     TO TF-TRANS-TYPE
               MOVE LS-TRANS-ACCT-FROM TO TF-TRANS-ACCT-FROM
               MOVE LS-TRANS-ACCT-TO  TO TF-TRANS-ACCT-TO
               MOVE LS-TRANS-AMOUNT   TO TF-TRANS-AMOUNT
               MOVE LS-TRANS-BALANCE-AFTER
                                      TO TF-TRANS-BALANCE-AFTER
               MOVE LS-TRANS-STATUS   TO TF-TRANS-STATUS
               MOVE LS-TRANS-DESCRIPTION
                                      TO TF-TRANS-DESCRIPTION
               MOVE LS-TELLER-ID     TO TF-TRANS-TELLER-ID
               MOVE SPACES            TO TF-TRANS-REF-NUMBER
               MOVE SPACES            TO TF-FILLER

               WRITE TRANSACTION-FILE-RECORD
           END-IF
           .

      *================================================================*
      * 1000 - DEPOSIT                                                *
      * Demonstrates: ADD, COMPUTE, ON SIZE ERROR, conditional logic  *
      *================================================================*
       1000-PROCESS-DEPOSIT.
           DISPLAY SPACES
           DISPLAY '=== DEPOSIT ==='
           DISPLAY 'Account Number: ' WITH NO ADVANCING
           ACCEPT WS-INPUT-ACCT-NUM

           PERFORM 0300-READ-ACCOUNT

           IF LS-ERROR-CODE = 0000
               IF LS-ACCT-STATUS NOT = 'A'
                   MOVE 1003 TO LS-ERROR-CODE
                   MOVE 'Account is not active'
                       TO LS-ERROR-MESSAGE
                   MOVE 2 TO LS-ERROR-SEVERITY
               ELSE
                   MOVE LS-CURRENT-BALANCE TO WS-DISPLAY-BALANCE
                   DISPLAY 'Holder: '
                       LS-HOLDER-FIRST-NAME ' '
                       LS-HOLDER-LAST-NAME
                   DISPLAY 'Current Balance: ' WS-DISPLAY-BALANCE
                   DISPLAY 'Deposit Amount: $' WITH NO ADVANCING
                   ACCEPT WS-INPUT-AMOUNT

                   IF WS-INPUT-AMOUNT <= 0
                       MOVE 2003 TO LS-ERROR-CODE
                       MOVE 'Deposit amount must be positive'
                           TO LS-ERROR-MESSAGE
                       MOVE 2 TO LS-ERROR-SEVERITY
                   ELSE
                       ADD WS-INPUT-AMOUNT TO LS-CURRENT-BALANCE
                           ON SIZE ERROR
                               MOVE 2002 TO LS-ERROR-CODE
                               MOVE 'Balance overflow'
                                   TO LS-ERROR-MESSAGE
                               MOVE 3 TO LS-ERROR-SEVERITY
                           NOT ON SIZE ERROR
                               MOVE LS-CURRENT-BALANCE
                                   TO LS-AVAILABLE-BALANCE
                               ADD WS-INPUT-AMOUNT
                                   TO LS-YTD-DEPOSITS
                               ADD 1 TO LS-MONTHLY-TRANS-CT

                               PERFORM 0400-UPDATE-ACCOUNT

                               IF LS-ERROR-CODE = 0000
                                   PERFORM 0500-GENERATE-TRANS-ID
                                   MOVE 'DP' TO LS-TRANS-TYPE
                                   MOVE LS-ACCT-NUMBER
                                       TO LS-TRANS-ACCT-FROM
                                   MOVE 0 TO LS-TRANS-ACCT-TO
                                   MOVE WS-INPUT-AMOUNT
                                       TO LS-TRANS-AMOUNT
                                   MOVE LS-CURRENT-BALANCE
                                       TO LS-TRANS-BALANCE-AFTER
                                   MOVE 'A' TO LS-TRANS-STATUS
                                   MOVE 'Cash Deposit'
                                       TO LS-TRANS-DESCRIPTION
                                   PERFORM 0600-WRITE-TRANS-LOG

                                   DISPLAY SPACES
                                   DISPLAY '*** DEPOSIT CONFIRMED ***'
                                   MOVE WS-INPUT-AMOUNT
                                       TO WS-DISPLAY-AMOUNT
                                   DISPLAY 'Deposited: '
                                       WS-DISPLAY-AMOUNT
                                   MOVE LS-CURRENT-BALANCE
                                       TO WS-DISPLAY-BALANCE
                                   DISPLAY 'New Balance: '
                                       WS-DISPLAY-BALANCE
                                   DISPLAY 'Trans ID: '
                                       LS-TRANS-ID
                               END-IF
                       END-ADD
                   END-IF
               END-IF
           END-IF
           .

      *================================================================*
      * 2000 - WITHDRAWAL                                             *
      * Demonstrates: SUBTRACT, overdraft logic, fee assessment,      *
      *               nested conditionals, COMPUTE ROUNDED             *
      *================================================================*
       2000-PROCESS-WITHDRAWAL.
           DISPLAY SPACES
           DISPLAY '=== WITHDRAWAL ==='
           DISPLAY 'Account Number: ' WITH NO ADVANCING
           ACCEPT WS-INPUT-ACCT-NUM

           PERFORM 0300-READ-ACCOUNT

           IF LS-ERROR-CODE = 0000
               IF LS-ACCT-STATUS NOT = 'A'
                   MOVE 1003 TO LS-ERROR-CODE
                   MOVE 'Account is not active'
                       TO LS-ERROR-MESSAGE
                   MOVE 2 TO LS-ERROR-SEVERITY
               ELSE
                   MOVE LS-CURRENT-BALANCE TO WS-DISPLAY-BALANCE
                   DISPLAY 'Holder: '
                       LS-HOLDER-FIRST-NAME ' '
                       LS-HOLDER-LAST-NAME
                   DISPLAY 'Current Balance: ' WS-DISPLAY-BALANCE
                   MOVE LS-AVAILABLE-BALANCE TO WS-DISPLAY-BALANCE
                   DISPLAY 'Available:       ' WS-DISPLAY-BALANCE
                   DISPLAY 'Withdrawal Amount: $'
                       WITH NO ADVANCING
                   ACCEPT WS-INPUT-AMOUNT

                   IF WS-INPUT-AMOUNT <= 0
                       MOVE 2003 TO LS-ERROR-CODE
                       MOVE 'Withdrawal must be positive'
                           TO LS-ERROR-MESSAGE
                       MOVE 2 TO LS-ERROR-SEVERITY
                   ELSE
                       IF WS-INPUT-AMOUNT > WS-MAX-WITHDRAWAL
                           MOVE 2002 TO LS-ERROR-CODE
                           STRING 'Exceeds daily limit of $'
                               DELIMITED SIZE
                               WS-MAX-WITHDRAWAL DELIMITED SIZE
                               INTO LS-ERROR-MESSAGE
                           END-STRING
                           MOVE 2 TO LS-ERROR-SEVERITY
                       ELSE
                           PERFORM 2100-CHECK-FUNDS
                       END-IF
                   END-IF
               END-IF
           END-IF
           .

      *----------------------------------------------------------------*
      * Check funds with overdraft protection                          *
      *----------------------------------------------------------------*
       2100-CHECK-FUNDS.
           MOVE 0 TO WS-FEE-AMOUNT
           COMPUTE WS-NEW-BALANCE =
               LS-CURRENT-BALANCE - WS-INPUT-AMOUNT

           IF WS-NEW-BALANCE < 0
               IF LS-ACCT-TYPE = 'C' AND
                  LS-OVERDRAFT-LIMIT > 0
                   COMPUTE WS-OVERDRAFT-USED ROUNDED =
                       WS-INPUT-AMOUNT - LS-CURRENT-BALANCE
                   IF WS-OVERDRAFT-USED <= LS-OVERDRAFT-LIMIT
                       MOVE WS-OVERDRAFT-FEE TO WS-FEE-AMOUNT
                       DISPLAY 'Overdraft protection used.'
                       MOVE WS-FEE-AMOUNT TO WS-DISPLAY-FEE
                       DISPLAY 'Overdraft fee: ' WS-DISPLAY-FEE
                       DISPLAY 'Proceed? (Y/N): '
                           WITH NO ADVANCING
                       ACCEPT WS-INPUT-CONFIRM
                       IF WS-INPUT-CONFIRM = 'Y' OR 'y'
                           COMPUTE LS-CURRENT-BALANCE
                               ROUNDED =
                               LS-CURRENT-BALANCE
                               - WS-INPUT-AMOUNT
                               - WS-FEE-AMOUNT
                           MOVE LS-CURRENT-BALANCE
                               TO LS-AVAILABLE-BALANCE
                           PERFORM 2200-COMPLETE-WITHDRAWAL
                       ELSE
                           DISPLAY 'Withdrawal cancelled.'
                       END-IF
                   ELSE
                       MOVE 2001 TO LS-ERROR-CODE
                       MOVE 'Exceeds overdraft limit'
                           TO LS-ERROR-MESSAGE
                       MOVE 2 TO LS-ERROR-SEVERITY
                   END-IF
               ELSE
                   MOVE 2001 TO LS-ERROR-CODE
                   MOVE 'Insufficient funds'
                       TO LS-ERROR-MESSAGE
                   MOVE 2 TO LS-ERROR-SEVERITY
               END-IF
           ELSE
               SUBTRACT WS-INPUT-AMOUNT FROM LS-CURRENT-BALANCE
               MOVE LS-CURRENT-BALANCE TO LS-AVAILABLE-BALANCE

               IF LS-CURRENT-BALANCE < LS-MIN-BALANCE
                   AND LS-ACCT-TYPE NOT = 'C'
                   MOVE WS-BELOW-MIN-FEE TO WS-FEE-AMOUNT
                   SUBTRACT WS-FEE-AMOUNT FROM LS-CURRENT-BALANCE
                   MOVE LS-CURRENT-BALANCE
                       TO LS-AVAILABLE-BALANCE
                   MOVE WS-FEE-AMOUNT TO WS-DISPLAY-FEE
                   DISPLAY 'Below minimum balance fee: '
                       WS-DISPLAY-FEE
               END-IF

               PERFORM 2200-COMPLETE-WITHDRAWAL
           END-IF
           .

       2200-COMPLETE-WITHDRAWAL.
           ADD WS-INPUT-AMOUNT TO LS-YTD-WITHDRAWALS
           ADD 1 TO LS-MONTHLY-TRANS-CT

           PERFORM 0400-UPDATE-ACCOUNT

           IF LS-ERROR-CODE = 0000
               PERFORM 0500-GENERATE-TRANS-ID
               MOVE 'WD' TO LS-TRANS-TYPE
               MOVE LS-ACCT-NUMBER TO LS-TRANS-ACCT-FROM
               MOVE 0 TO LS-TRANS-ACCT-TO
               MOVE WS-INPUT-AMOUNT TO LS-TRANS-AMOUNT
               MOVE LS-CURRENT-BALANCE
                   TO LS-TRANS-BALANCE-AFTER
               MOVE 'A' TO LS-TRANS-STATUS
               MOVE 'Cash Withdrawal' TO LS-TRANS-DESCRIPTION
               PERFORM 0600-WRITE-TRANS-LOG

               DISPLAY SPACES
               DISPLAY '*** WITHDRAWAL CONFIRMED ***'
               MOVE WS-INPUT-AMOUNT TO WS-DISPLAY-AMOUNT
               DISPLAY 'Withdrawn:   ' WS-DISPLAY-AMOUNT
               MOVE LS-CURRENT-BALANCE TO WS-DISPLAY-BALANCE
               DISPLAY 'New Balance: ' WS-DISPLAY-BALANCE
               DISPLAY 'Trans ID:   ' LS-TRANS-ID
           END-IF
           .

      *================================================================*
      * 3000 - TRANSFER                                               *
      * Demonstrates: Multiple file reads/updates, two-phase commit   *
      *               pattern, COMPUTE with multiple operands          *
      *================================================================*
       3000-PROCESS-TRANSFER.
           DISPLAY SPACES
           DISPLAY '=== TRANSFER BETWEEN ACCOUNTS ==='
           DISPLAY 'From Account: ' WITH NO ADVANCING
           ACCEPT WS-INPUT-ACCT-NUM
           DISPLAY 'To Account: ' WITH NO ADVANCING
           ACCEPT WS-INPUT-ACCT-TO

           IF WS-INPUT-ACCT-NUM = WS-INPUT-ACCT-TO
               MOVE 9002 TO LS-ERROR-CODE
               MOVE 'Cannot transfer to same account'
                   TO LS-ERROR-MESSAGE
               MOVE 2 TO LS-ERROR-SEVERITY
           ELSE
               PERFORM 0300-READ-ACCOUNT

               IF LS-ERROR-CODE = 0000
                   IF LS-ACCT-STATUS NOT = 'A'
                       MOVE 1003 TO LS-ERROR-CODE
                       MOVE 'Source account is not active'
                           TO LS-ERROR-MESSAGE
                       MOVE 2 TO LS-ERROR-SEVERITY
                   ELSE
                       MOVE LS-CURRENT-BALANCE
                           TO WS-DISPLAY-BALANCE
                       DISPLAY 'From: '
                           LS-HOLDER-FIRST-NAME ' '
                           LS-HOLDER-LAST-NAME
                       DISPLAY 'Balance: ' WS-DISPLAY-BALANCE

                       DISPLAY 'Transfer Amount: $'
                           WITH NO ADVANCING
                       ACCEPT WS-INPUT-AMOUNT

                       IF WS-INPUT-AMOUNT <= 0
                           MOVE 2003 TO LS-ERROR-CODE
                           MOVE 'Transfer must be positive'
                               TO LS-ERROR-MESSAGE
                           MOVE 2 TO LS-ERROR-SEVERITY
                       ELSE
                           IF WS-INPUT-AMOUNT >
                               LS-CURRENT-BALANCE
                               MOVE 2001 TO LS-ERROR-CODE
                               MOVE 'Insufficient funds'
                                   TO LS-ERROR-MESSAGE
                               MOVE 2 TO LS-ERROR-SEVERITY
                           ELSE
                               IF WS-INPUT-AMOUNT >
                                   WS-MAX-TRANSFER
                                   MOVE 2002 TO LS-ERROR-CODE
                                   MOVE 'Exceeds transfer limit'
                                       TO LS-ERROR-MESSAGE
                                   MOVE 2 TO LS-ERROR-SEVERITY
                               ELSE
                                   PERFORM
                                       3100-EXECUTE-TRANSFER
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           .

       3100-EXECUTE-TRANSFER.
      *    Debit source account
           SUBTRACT WS-INPUT-AMOUNT FROM LS-CURRENT-BALANCE
           MOVE LS-CURRENT-BALANCE TO LS-AVAILABLE-BALANCE
           ADD WS-INPUT-AMOUNT TO LS-YTD-WITHDRAWALS
           ADD 1 TO LS-MONTHLY-TRANS-CT

           PERFORM 0400-UPDATE-ACCOUNT

           IF LS-ERROR-CODE = 0000
               PERFORM 0500-GENERATE-TRANS-ID
               MOVE 'TO' TO LS-TRANS-TYPE
               MOVE WS-INPUT-ACCT-NUM TO LS-TRANS-ACCT-FROM
               MOVE WS-INPUT-ACCT-TO TO LS-TRANS-ACCT-TO
               MOVE WS-INPUT-AMOUNT TO LS-TRANS-AMOUNT
               MOVE LS-CURRENT-BALANCE
                   TO LS-TRANS-BALANCE-AFTER
               MOVE 'A' TO LS-TRANS-STATUS
               MOVE 'Transfer Out' TO LS-TRANS-DESCRIPTION
               PERFORM 0600-WRITE-TRANS-LOG

      *        Credit destination account
               MOVE WS-INPUT-ACCT-TO TO WS-INPUT-ACCT-NUM
               PERFORM 0300-READ-ACCOUNT

               IF LS-ERROR-CODE = 0000
                   ADD WS-INPUT-AMOUNT TO LS-CURRENT-BALANCE
                   MOVE LS-CURRENT-BALANCE
                       TO LS-AVAILABLE-BALANCE
                   ADD WS-INPUT-AMOUNT TO LS-YTD-DEPOSITS
                   ADD 1 TO LS-MONTHLY-TRANS-CT

                   PERFORM 0400-UPDATE-ACCOUNT

                   IF LS-ERROR-CODE = 0000
                       PERFORM 0500-GENERATE-TRANS-ID
                       MOVE 'TI' TO LS-TRANS-TYPE
                       MOVE WS-INPUT-ACCT-TO
                           TO LS-TRANS-ACCT-FROM
                       MOVE WS-INPUT-ACCT-NUM
                           TO LS-TRANS-ACCT-TO
                       MOVE WS-INPUT-AMOUNT
                           TO LS-TRANS-AMOUNT
                       MOVE LS-CURRENT-BALANCE
                           TO LS-TRANS-BALANCE-AFTER
                       MOVE 'A' TO LS-TRANS-STATUS
                       MOVE 'Transfer In'
                           TO LS-TRANS-DESCRIPTION
                       PERFORM 0600-WRITE-TRANS-LOG

                       DISPLAY SPACES
                       DISPLAY '*** TRANSFER CONFIRMED ***'
                       MOVE WS-INPUT-AMOUNT
                           TO WS-DISPLAY-AMOUNT
                       DISPLAY 'Transferred: '
                           WS-DISPLAY-AMOUNT
                       DISPLAY 'Trans ID: ' LS-TRANS-ID
                   END-IF
               END-IF
           END-IF
           .

      *================================================================*
      * 4000 - BALANCE INQUIRY                                        *
      *================================================================*
       4000-BALANCE-INQUIRY.
           DISPLAY SPACES
           DISPLAY '=== BALANCE INQUIRY ==='
           DISPLAY 'Account Number: ' WITH NO ADVANCING
           ACCEPT WS-INPUT-ACCT-NUM

           PERFORM 0300-READ-ACCOUNT

           IF LS-ERROR-CODE = 0000
               DISPLAY SPACES
               DISPLAY WS-SEPARATOR
               DISPLAY 'Account:   ' LS-ACCT-NUMBER
               DISPLAY 'Holder:    '
                   LS-HOLDER-FIRST-NAME ' '
                   LS-HOLDER-LAST-NAME
               DISPLAY 'Type:      ' LS-ACCT-TYPE
               DISPLAY 'Status:    ' LS-ACCT-STATUS

               MOVE LS-CURRENT-BALANCE TO WS-DISPLAY-BALANCE
               DISPLAY 'Balance:   ' WS-DISPLAY-BALANCE

               MOVE LS-AVAILABLE-BALANCE TO WS-DISPLAY-BALANCE
               DISPLAY 'Available: ' WS-DISPLAY-BALANCE
               DISPLAY WS-SEPARATOR
           END-IF
           .

      *================================================================*
      * 5000 - TRANSACTION HISTORY                                    *
      * Demonstrates: Sequential file reading, table loading,         *
      *               PERFORM VARYING for display                     *
      *================================================================*
       5000-TRANSACTION-HISTORY.
           DISPLAY SPACES
           DISPLAY '=== TRANSACTION HISTORY ==='
           DISPLAY 'Account Number: ' WITH NO ADVANCING
           ACCEPT WS-INPUT-ACCT-NUM

           PERFORM 0300-READ-ACCOUNT

           IF LS-ERROR-CODE = 0000
               DISPLAY SPACES
               DISPLAY 'Account: ' LS-ACCT-NUMBER
               DISPLAY 'Holder:  '
                   LS-HOLDER-FIRST-NAME ' '
                   LS-HOLDER-LAST-NAME
               DISPLAY SPACES

               PERFORM 5100-LOAD-HISTORY

               IF WS-HIST-COUNT > 0
                   DISPLAY 'Date       Type  Amount'
                       '           Balance After'
                   DISPLAY WS-SEPARATOR

                   PERFORM VARYING WS-HIST-IDX
                       FROM 1 BY 1
                       UNTIL WS-HIST-IDX > WS-HIST-COUNT
                       MOVE WS-HIST-AMOUNT(WS-HIST-IDX)
                           TO WS-DISPLAY-AMOUNT
                       MOVE WS-HIST-BALANCE(WS-HIST-IDX)
                           TO WS-DISPLAY-BALANCE
                       DISPLAY
                           WS-HIST-DATE(WS-HIST-IDX) '  '
                           WS-HIST-TYPE(WS-HIST-IDX) '    '
                           WS-DISPLAY-AMOUNT '  '
                           WS-DISPLAY-BALANCE
                   END-PERFORM

                   DISPLAY WS-SEPARATOR
                   DISPLAY 'Total entries: ' WS-HIST-COUNT
               ELSE
                   DISPLAY 'No transaction history found.'
               END-IF
           END-IF
           .

       5100-LOAD-HISTORY.
           MOVE 0 TO WS-HIST-COUNT

           CLOSE TRANSACTION-FILE
           OPEN INPUT TRANSACTION-FILE

           IF WS-TRAN-FILE-STATUS = '00'
               PERFORM UNTIL WS-TRAN-FILE-STATUS NOT = '00'
                   OR WS-HIST-COUNT >= 50
                   READ TRANSACTION-FILE
                       AT END
                           MOVE '10' TO WS-TRAN-FILE-STATUS
                       NOT AT END
                           IF TF-TRANS-ACCT-FROM =
                               WS-INPUT-ACCT-NUM OR
                              TF-TRANS-ACCT-TO =
                               WS-INPUT-ACCT-NUM
                               ADD 1 TO WS-HIST-COUNT
                               SET WS-HIST-IDX
                                   TO WS-HIST-COUNT
                               MOVE TF-TRANS-DATE
                                   TO WS-HIST-DATE(WS-HIST-IDX)
                               MOVE TF-TRANS-TYPE
                                   TO WS-HIST-TYPE(WS-HIST-IDX)
                               MOVE TF-TRANS-AMOUNT TO
                                   WS-HIST-AMOUNT(WS-HIST-IDX)
                               MOVE TF-TRANS-BALANCE-AFTER TO
                                   WS-HIST-BALANCE(WS-HIST-IDX)
                               MOVE TF-TRANS-DESCRIPTION TO
                                   WS-HIST-DESC(WS-HIST-IDX)
                               MOVE TF-TRANS-STATUS TO
                                   WS-HIST-STATUS(WS-HIST-IDX)
                           END-IF
                   END-READ
               END-PERFORM

               CLOSE TRANSACTION-FILE
               OPEN EXTEND TRANSACTION-FILE
               IF WS-TRAN-FILE-STATUS = '00'
                   MOVE 'Y' TO WS-TRAN-FILE-OPEN
               END-IF
           END-IF
           .
