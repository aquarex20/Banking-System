      *================================================================*
      * Program:    RPTGEN                                             *
      * Purpose:    Report Generation Subprogram                       *
      * Concepts:   Sequential file processing, report writer-style   *
      *             output, accumulators, control breaks,              *
      *             PERFORM VARYING, STRING formatting,                *
      *             INSPECT TALLYING, group-level operations,          *
      *             multi-file processing, summary statistics          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RPTGEN.

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

           SELECT LOAN-FILE
               ASSIGN TO 'data/LOANS.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS LF-LOAN-NUMBER
               FILE STATUS IS WS-LOAN-FILE-STATUS.

           SELECT REPORT-FILE
               ASSIGN TO 'data/REPORT.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-FILE-STATUS.

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

       FD  LOAN-FILE.
       01  LOAN-FILE-RECORD.
           05  LF-LOAN-NUMBER          PIC 9(10).
           05  LF-LOAN-ACCT-NUMBER     PIC 9(10).
           05  LF-LOAN-TYPE            PIC X(1).
           05  LF-LOAN-STATUS          PIC X(1).
           05  LF-LOAN-PRINCIPAL       PIC S9(11)V99.
           05  LF-LOAN-CURRENT-BAL     PIC S9(11)V99.
           05  LF-LOAN-INT-RATE        PIC 9V9(4).
           05  LF-LOAN-TERM-MONTHS     PIC 9(3).
           05  LF-LOAN-MONTHLY-PMT     PIC S9(9)V99.
           05  LF-LOAN-TOTAL-PAID      PIC S9(11)V99.
           05  LF-LOAN-TOTAL-INTEREST  PIC S9(11)V99.
           05  LF-LOAN-PAYMENTS-MADE   PIC 9(3).
           05  LF-LOAN-PAYMENTS-LEFT   PIC 9(3).
           05  LF-LOAN-START-DATE      PIC 9(8).
           05  LF-LOAN-END-DATE        PIC 9(8).
           05  LF-LOAN-LAST-PMT-DATE   PIC 9(8).
           05  LF-LOAN-NEXT-PMT-DATE   PIC 9(8).
           05  LF-LOAN-LATE-FEES       PIC S9(7)V99.
           05  LF-FILLER               PIC X(20).

       FD  REPORT-FILE.
       01  REPORT-LINE                 PIC X(132).

       WORKING-STORAGE SECTION.

       01  WS-ACCT-FILE-STATUS         PIC X(2).
       01  WS-TRAN-FILE-STATUS         PIC X(2).
       01  WS-LOAN-FILE-STATUS         PIC X(2).
       01  WS-RPT-FILE-STATUS          PIC X(2).

      *----------------------------------------------------------------*
      * Report header and detail lines                                 *
      *----------------------------------------------------------------*
       01  WS-RPT-HEADER-1.
           05  FILLER                  PIC X(50)
               VALUE ALL '='.
           05  FILLER                  PIC X(30).

       01  WS-RPT-HEADER-2.
           05  FILLER                  PIC X(5) VALUE SPACES.
           05  WS-RPT-TITLE            PIC X(45).
           05  FILLER                  PIC X(30).

       01  WS-RPT-HEADER-3.
           05  FILLER                  PIC X(5) VALUE 'Date:'.
           05  FILLER                  PIC X VALUE SPACE.
           05  WS-RPT-DATE             PIC X(10).
           05  FILLER                  PIC X(20) VALUE SPACES.
           05  FILLER                  PIC X(5) VALUE 'Page:'.
           05  WS-RPT-PAGE             PIC Z9.

       01  WS-RPT-SEPARATOR.
           05  FILLER                  PIC X(50) VALUE ALL '-'.

      *----------------------------------------------------------------*
      * Account statement detail line                                  *
      *----------------------------------------------------------------*
       01  WS-STMT-DETAIL.
           05  WS-STMT-DATE            PIC X(10).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  WS-STMT-TYPE            PIC X(12).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  WS-STMT-AMOUNT          PIC $ZZZ,ZZZ,ZZ9.99-.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  WS-STMT-BALANCE         PIC $ZZZ,ZZZ,ZZ9.99-.

      *----------------------------------------------------------------*
      * Portfolio summary accumulators                                 *
      *----------------------------------------------------------------*
       01  WS-PORTFOLIO-ACCUM.
           05  WS-PORT-TOTAL-ACCOUNTS  PIC 9(7) VALUE 0.
           05  WS-PORT-ACTIVE-ACCTS    PIC 9(7) VALUE 0.
           05  WS-PORT-CLOSED-ACCTS    PIC 9(7) VALUE 0.
           05  WS-PORT-FROZEN-ACCTS    PIC 9(7) VALUE 0.
           05  WS-PORT-CHECKING-CT     PIC 9(7) VALUE 0.
           05  WS-PORT-SAVINGS-CT      PIC 9(7) VALUE 0.
           05  WS-PORT-MONEY-MKT-CT    PIC 9(7) VALUE 0.
           05  WS-PORT-TOTAL-BALANCE   PIC S9(15)V99 VALUE 0.
           05  WS-PORT-TOTAL-DEPOSITS  PIC S9(15)V99 VALUE 0.
           05  WS-PORT-TOTAL-WDRW      PIC S9(15)V99 VALUE 0.
           05  WS-PORT-AVG-BALANCE     PIC S9(11)V99 VALUE 0.
           05  WS-PORT-MAX-BALANCE     PIC S9(11)V99 VALUE 0.
           05  WS-PORT-MIN-BALANCE     PIC S9(11)V99
                                       VALUE 99999999999.99.

      *----------------------------------------------------------------*
      * Transaction summary accumulators                               *
      *----------------------------------------------------------------*
       01  WS-TRANS-ACCUM.
           05  WS-TRAN-TOTAL-COUNT     PIC 9(7) VALUE 0.
           05  WS-TRAN-DEPOSIT-CT      PIC 9(7) VALUE 0.
           05  WS-TRAN-WDRW-CT         PIC 9(7) VALUE 0.
           05  WS-TRAN-XFER-CT         PIC 9(7) VALUE 0.
           05  WS-TRAN-OTHER-CT        PIC 9(7) VALUE 0.
           05  WS-TRAN-DEPOSIT-AMT     PIC S9(15)V99 VALUE 0.
           05  WS-TRAN-WDRW-AMT        PIC S9(15)V99 VALUE 0.
           05  WS-TRAN-XFER-AMT        PIC S9(15)V99 VALUE 0.
           05  WS-TRAN-APPROVED-CT     PIC 9(7) VALUE 0.
           05  WS-TRAN-DECLINED-CT     PIC 9(7) VALUE 0.

      *----------------------------------------------------------------*
      * Loan portfolio accumulators                                    *
      *----------------------------------------------------------------*
       01  WS-LOAN-ACCUM.
           05  WS-LN-TOTAL-LOANS       PIC 9(7) VALUE 0.
           05  WS-LN-ACTIVE-LOANS      PIC 9(7) VALUE 0.
           05  WS-LN-PAID-LOANS        PIC 9(7) VALUE 0.
           05  WS-LN-DEFAULT-LOANS     PIC 9(7) VALUE 0.
           05  WS-LN-TOTAL-PRINCIPAL   PIC S9(15)V99 VALUE 0.
           05  WS-LN-TOTAL-OUTSTANDING PIC S9(15)V99 VALUE 0.
           05  WS-LN-TOTAL-INT-EARNED  PIC S9(15)V99 VALUE 0.
           05  WS-LN-TOTAL-LATE-FEES   PIC S9(11)V99 VALUE 0.

      *----------------------------------------------------------------*
      * Display and work fields                                        *
      *----------------------------------------------------------------*
       01  WS-DISP-AMOUNT              PIC $ZZZ,ZZZ,ZZZ,ZZ9.99-.
       01  WS-DISP-COUNT               PIC ZZZ,ZZ9.
       01  WS-DISP-RATE                PIC Z9.9999.
       01  WS-PAGE-COUNT               PIC 99 VALUE 1.
       01  WS-LINE-COUNT               PIC 99 VALUE 0.
       01  WS-LINES-PER-PAGE           PIC 99 VALUE 50.
       01  WS-INPUT-ACCT-NUM           PIC 9(10).
       01  WS-INPUT-DATE               PIC 9(8).
       01  WS-FORMATTED-DATE           PIC X(10).
       01  WS-RAW-DATE                 PIC 9(8).
       01  WS-SEPARATOR                PIC X(60) VALUE ALL '-'.
       01  WS-EOF-FLAG                 PIC X VALUE 'N'.

      *================================================================*
      * LINKAGE SECTION                                                *
      *================================================================*
       LINKAGE SECTION.

       01  LS-FUNCTION-CODE            PIC X(2).

       01  LS-ACCOUNT-RECORD.
           05  LS-ACCT-NUMBER          PIC 9(10).
           05  FILLER                  PIC X(230).

       01  LS-TRANSACTION-RECORD.
           05  LS-TRANS-ID             PIC 9(12).
           05  FILLER                  PIC X(118).

       01  LS-LOAN-RECORD.
           05  LS-LOAN-NUMBER          PIC 9(10).
           05  FILLER                  PIC X(130).

       01  LS-ERROR-HANDLING.
           05  LS-ERROR-CODE           PIC 9(4).
           05  LS-ERROR-MESSAGE        PIC X(60).
           05  LS-ERROR-SEVERITY       PIC 9.
           05  LS-ERROR-MODULE         PIC X(8).
           05  LS-ERROR-PARAGRAPH      PIC X(20).

       PROCEDURE DIVISION USING
           LS-FUNCTION-CODE
           LS-ACCOUNT-RECORD
           LS-TRANSACTION-RECORD
           LS-LOAN-RECORD
           LS-ERROR-HANDLING.

       0000-MAIN-ENTRY.
           MOVE 0000 TO LS-ERROR-CODE
           MOVE SPACES TO LS-ERROR-MESSAGE
           MOVE 'RPTGEN' TO LS-ERROR-MODULE

           EVALUATE LS-FUNCTION-CODE
               WHEN 'AS'
                   PERFORM 1000-ACCOUNT-STATEMENT
               WHEN 'DT'
                   PERFORM 2000-DAILY-TRANS-SUMMARY
               WHEN 'AP'
                   PERFORM 3000-ACCOUNT-PORTFOLIO
               WHEN 'LP'
                   PERFORM 4000-LOAN-PORTFOLIO
               WHEN 'OL'
                   PERFORM 5000-OVERDUE-LOANS
               WHEN OTHER
                   MOVE 9002 TO LS-ERROR-CODE
                   MOVE 'Invalid report function'
                       TO LS-ERROR-MESSAGE
                   MOVE 2 TO LS-ERROR-SEVERITY
           END-EVALUATE

           GOBACK
           .

      *================================================================*
      * 1000 - ACCOUNT STATEMENT                                      *
      * Demonstrates: Multi-file processing, formatted output,        *
      *               date formatting with STRING, sequential read     *
      *================================================================*
       1000-ACCOUNT-STATEMENT.
           DISPLAY SPACES
           DISPLAY '=== ACCOUNT STATEMENT ==='
           DISPLAY 'Account Number: ' WITH NO ADVANCING
           ACCEPT WS-INPUT-ACCT-NUM

           OPEN INPUT ACCOUNT-FILE
           IF WS-ACCT-FILE-STATUS NOT = '00'
               MOVE 9001 TO LS-ERROR-CODE
               MOVE 'Cannot open account file'
                   TO LS-ERROR-MESSAGE
               MOVE 3 TO LS-ERROR-SEVERITY
           ELSE
               MOVE WS-INPUT-ACCT-NUM TO AF-ACCT-NUMBER
               READ ACCOUNT-FILE
                   KEY IS AF-ACCT-NUMBER
               END-READ

               IF WS-ACCT-FILE-STATUS = '00'
                   PERFORM 1100-PRINT-STMT-HEADER
                   PERFORM 1200-PRINT-STMT-TRANSACTIONS
                   PERFORM 1300-PRINT-STMT-FOOTER
               ELSE
                   MOVE 1001 TO LS-ERROR-CODE
                   MOVE 'Account not found'
                       TO LS-ERROR-MESSAGE
                   MOVE 2 TO LS-ERROR-SEVERITY
               END-IF

               CLOSE ACCOUNT-FILE
           END-IF
           .

       1100-PRINT-STMT-HEADER.
           DISPLAY SPACES
           DISPLAY ALL '='
           DISPLAY '     ACCOUNT STATEMENT'
           DISPLAY ALL '='
           DISPLAY 'Account:  ' AF-ACCT-NUMBER
           DISPLAY 'Name:     '
               AF-HOLDER-FIRST-NAME ' '
               AF-HOLDER-LAST-NAME
           DISPLAY 'Type:     ' AF-ACCT-TYPE
           DISPLAY 'Status:   ' AF-ACCT-STATUS

           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-RAW-DATE
           STRING WS-RAW-DATE(1:4)  DELIMITED SIZE
                  '-'                DELIMITED SIZE
                  WS-RAW-DATE(5:2)  DELIMITED SIZE
                  '-'                DELIMITED SIZE
                  WS-RAW-DATE(7:2)  DELIMITED SIZE
               INTO WS-FORMATTED-DATE
           END-STRING

           DISPLAY 'Date:     ' WS-FORMATTED-DATE
           DISPLAY ALL '-'
           DISPLAY 'Date        Type          Amount'
               '             Balance'
           DISPLAY ALL '-'
           .

       1200-PRINT-STMT-TRANSACTIONS.
           OPEN INPUT TRANSACTION-FILE
           IF WS-TRAN-FILE-STATUS = '00'
               MOVE 'N' TO WS-EOF-FLAG
               PERFORM UNTIL WS-EOF-FLAG = 'Y'
                   READ TRANSACTION-FILE
                       AT END
                           MOVE 'Y' TO WS-EOF-FLAG
                       NOT AT END
                           IF TF-TRANS-ACCT-FROM =
                               WS-INPUT-ACCT-NUM
                               OR TF-TRANS-ACCT-TO =
                               WS-INPUT-ACCT-NUM

                               MOVE TF-TRANS-DATE TO WS-RAW-DATE
                               INITIALIZE WS-FORMATTED-DATE
                               STRING
                                   WS-RAW-DATE(1:4)
                                       DELIMITED SIZE
                                   '-' DELIMITED SIZE
                                   WS-RAW-DATE(5:2)
                                       DELIMITED SIZE
                                   '-' DELIMITED SIZE
                                   WS-RAW-DATE(7:2)
                                       DELIMITED SIZE
                                   INTO WS-FORMATTED-DATE
                               END-STRING
                               MOVE WS-FORMATTED-DATE
                                   TO WS-STMT-DATE

                               EVALUATE TF-TRANS-TYPE
                                   WHEN 'DP'
                                       MOVE 'Deposit'
                                           TO WS-STMT-TYPE
                                   WHEN 'WD'
                                       MOVE 'Withdrawal'
                                           TO WS-STMT-TYPE
                                   WHEN 'TO'
                                       MOVE 'Transfer Out'
                                           TO WS-STMT-TYPE
                                   WHEN 'TI'
                                       MOVE 'Transfer In'
                                           TO WS-STMT-TYPE
                                   WHEN 'IN'
                                       MOVE 'Interest'
                                           TO WS-STMT-TYPE
                                   WHEN 'FE'
                                       MOVE 'Fee'
                                           TO WS-STMT-TYPE
                                   WHEN OTHER
                                       MOVE TF-TRANS-TYPE
                                           TO WS-STMT-TYPE
                               END-EVALUATE

                               MOVE TF-TRANS-AMOUNT
                                   TO WS-STMT-AMOUNT
                               MOVE TF-TRANS-BALANCE-AFTER
                                   TO WS-STMT-BALANCE

                               DISPLAY WS-STMT-DETAIL
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE TRANSACTION-FILE
           END-IF
           .

       1300-PRINT-STMT-FOOTER.
           DISPLAY ALL '-'
           MOVE AF-CURRENT-BALANCE TO WS-DISP-AMOUNT
           DISPLAY 'Current Balance:   ' WS-DISP-AMOUNT
           MOVE AF-AVAILABLE-BALANCE TO WS-DISP-AMOUNT
           DISPLAY 'Available Balance: ' WS-DISP-AMOUNT
           MOVE AF-YTD-DEPOSITS TO WS-DISP-AMOUNT
           DISPLAY 'YTD Deposits:      ' WS-DISP-AMOUNT
           MOVE AF-YTD-WITHDRAWALS TO WS-DISP-AMOUNT
           DISPLAY 'YTD Withdrawals:   ' WS-DISP-AMOUNT
           DISPLAY ALL '='
           .

      *================================================================*
      * 2000 - DAILY TRANSACTION SUMMARY                              *
      * Demonstrates: INSPECT TALLYING, accumulators, control totals  *
      *================================================================*
       2000-DAILY-TRANS-SUMMARY.
           DISPLAY SPACES
           DISPLAY '=== DAILY TRANSACTION SUMMARY ==='
           INITIALIZE WS-TRANS-ACCUM

           OPEN INPUT TRANSACTION-FILE
           IF WS-TRAN-FILE-STATUS NOT = '00'
               IF WS-TRAN-FILE-STATUS = '35'
                   DISPLAY 'No transaction file exists yet.'
               ELSE
                   MOVE 9001 TO LS-ERROR-CODE
                   MOVE 'Cannot open transaction file'
                       TO LS-ERROR-MESSAGE
                   MOVE 3 TO LS-ERROR-SEVERITY
               END-IF
           ELSE
               MOVE 'N' TO WS-EOF-FLAG

               PERFORM UNTIL WS-EOF-FLAG = 'Y'
                   READ TRANSACTION-FILE
                       AT END
                           MOVE 'Y' TO WS-EOF-FLAG
                       NOT AT END
                           ADD 1 TO WS-TRAN-TOTAL-COUNT

                           EVALUATE TF-TRANS-TYPE
                               WHEN 'DP'
                                   ADD 1
                                       TO WS-TRAN-DEPOSIT-CT
                                   ADD TF-TRANS-AMOUNT
                                       TO WS-TRAN-DEPOSIT-AMT
                               WHEN 'WD'
                                   ADD 1 TO WS-TRAN-WDRW-CT
                                   ADD TF-TRANS-AMOUNT
                                       TO WS-TRAN-WDRW-AMT
                               WHEN 'TO' THRU 'TI'
                                   ADD 1 TO WS-TRAN-XFER-CT
                                   ADD TF-TRANS-AMOUNT
                                       TO WS-TRAN-XFER-AMT
                               WHEN OTHER
                                   ADD 1 TO WS-TRAN-OTHER-CT
                           END-EVALUATE

                           IF TF-TRANS-STATUS = 'A'
                               ADD 1 TO WS-TRAN-APPROVED-CT
                           ELSE
                               ADD 1 TO WS-TRAN-DECLINED-CT
                           END-IF
                   END-READ
               END-PERFORM

               CLOSE TRANSACTION-FILE

               PERFORM 2100-DISPLAY-TRANS-SUMMARY
           END-IF
           .

       2100-DISPLAY-TRANS-SUMMARY.
           DISPLAY SPACES
           DISPLAY ALL '='
           DISPLAY '     TRANSACTION SUMMARY'
           DISPLAY ALL '='

           MOVE WS-TRAN-TOTAL-COUNT TO WS-DISP-COUNT
           DISPLAY 'Total Transactions:  ' WS-DISP-COUNT

           DISPLAY ALL '-'
           DISPLAY 'BY TYPE:'

           MOVE WS-TRAN-DEPOSIT-CT TO WS-DISP-COUNT
           DISPLAY '  Deposits:          ' WS-DISP-COUNT
           MOVE WS-TRAN-DEPOSIT-AMT TO WS-DISP-AMOUNT
           DISPLAY '    Total Amount:    ' WS-DISP-AMOUNT

           MOVE WS-TRAN-WDRW-CT TO WS-DISP-COUNT
           DISPLAY '  Withdrawals:       ' WS-DISP-COUNT
           MOVE WS-TRAN-WDRW-AMT TO WS-DISP-AMOUNT
           DISPLAY '    Total Amount:    ' WS-DISP-AMOUNT

           MOVE WS-TRAN-XFER-CT TO WS-DISP-COUNT
           DISPLAY '  Transfers:         ' WS-DISP-COUNT
           MOVE WS-TRAN-XFER-AMT TO WS-DISP-AMOUNT
           DISPLAY '    Total Amount:    ' WS-DISP-AMOUNT

           MOVE WS-TRAN-OTHER-CT TO WS-DISP-COUNT
           DISPLAY '  Other:             ' WS-DISP-COUNT

           DISPLAY ALL '-'
           DISPLAY 'BY STATUS:'
           MOVE WS-TRAN-APPROVED-CT TO WS-DISP-COUNT
           DISPLAY '  Approved:          ' WS-DISP-COUNT
           MOVE WS-TRAN-DECLINED-CT TO WS-DISP-COUNT
           DISPLAY '  Declined:          ' WS-DISP-COUNT
           DISPLAY ALL '='
           .

      *================================================================*
      * 3000 - ACCOUNT PORTFOLIO SUMMARY                              *
      * Demonstrates: Sequential traversal of indexed file,           *
      *               START/READ NEXT, accumulators, averages,        *
      *               MIN/MAX tracking                                *
      *================================================================*
       3000-ACCOUNT-PORTFOLIO.
           DISPLAY SPACES
           DISPLAY '=== ACCOUNT PORTFOLIO SUMMARY ==='
           INITIALIZE WS-PORTFOLIO-ACCUM
           MOVE 99999999999.99 TO WS-PORT-MIN-BALANCE

           OPEN INPUT ACCOUNT-FILE
           IF WS-ACCT-FILE-STATUS NOT = '00'
               IF WS-ACCT-FILE-STATUS = '35'
                   DISPLAY 'No account file exists yet.'
               ELSE
                   MOVE 9001 TO LS-ERROR-CODE
                   MOVE 'Cannot open account file'
                       TO LS-ERROR-MESSAGE
                   MOVE 3 TO LS-ERROR-SEVERITY
               END-IF
           ELSE
               MOVE 0 TO AF-ACCT-NUMBER
               START ACCOUNT-FILE
                   KEY IS >= AF-ACCT-NUMBER
               END-START

               IF WS-ACCT-FILE-STATUS = '00'
                   MOVE 'N' TO WS-EOF-FLAG

                   PERFORM UNTIL WS-EOF-FLAG = 'Y'
                       READ ACCOUNT-FILE NEXT
                           AT END
                               MOVE 'Y' TO WS-EOF-FLAG
                           NOT AT END
                               PERFORM
                                   3100-ACCUMULATE-ACCT-STATS
                       END-READ
                   END-PERFORM
               END-IF

               CLOSE ACCOUNT-FILE

               IF WS-PORT-TOTAL-ACCOUNTS > 0
                   COMPUTE WS-PORT-AVG-BALANCE ROUNDED =
                       WS-PORT-TOTAL-BALANCE /
                       WS-PORT-ACTIVE-ACCTS
                   PERFORM 3200-DISPLAY-PORTFOLIO
               ELSE
                   DISPLAY 'No accounts found.'
               END-IF
           END-IF
           .

       3100-ACCUMULATE-ACCT-STATS.
           ADD 1 TO WS-PORT-TOTAL-ACCOUNTS

           EVALUATE AF-ACCT-STATUS
               WHEN 'A' ADD 1 TO WS-PORT-ACTIVE-ACCTS
               WHEN 'C' ADD 1 TO WS-PORT-CLOSED-ACCTS
               WHEN 'F' ADD 1 TO WS-PORT-FROZEN-ACCTS
           END-EVALUATE

           EVALUATE AF-ACCT-TYPE
               WHEN 'C' ADD 1 TO WS-PORT-CHECKING-CT
               WHEN 'S' ADD 1 TO WS-PORT-SAVINGS-CT
               WHEN 'M' ADD 1 TO WS-PORT-MONEY-MKT-CT
           END-EVALUATE

           ADD AF-CURRENT-BALANCE TO WS-PORT-TOTAL-BALANCE
           ADD AF-YTD-DEPOSITS TO WS-PORT-TOTAL-DEPOSITS
           ADD AF-YTD-WITHDRAWALS TO WS-PORT-TOTAL-WDRW

           IF AF-CURRENT-BALANCE > WS-PORT-MAX-BALANCE
               MOVE AF-CURRENT-BALANCE TO WS-PORT-MAX-BALANCE
           END-IF

           IF AF-CURRENT-BALANCE < WS-PORT-MIN-BALANCE
               AND AF-ACCT-STATUS = 'A'
               MOVE AF-CURRENT-BALANCE TO WS-PORT-MIN-BALANCE
           END-IF
           .

       3200-DISPLAY-PORTFOLIO.
           DISPLAY SPACES
           DISPLAY ALL '='
           DISPLAY '     ACCOUNT PORTFOLIO SUMMARY'
           DISPLAY ALL '='

           MOVE WS-PORT-TOTAL-ACCOUNTS TO WS-DISP-COUNT
           DISPLAY 'Total Accounts:      ' WS-DISP-COUNT

           DISPLAY ALL '-'
           DISPLAY 'BY STATUS:'
           MOVE WS-PORT-ACTIVE-ACCTS TO WS-DISP-COUNT
           DISPLAY '  Active:            ' WS-DISP-COUNT
           MOVE WS-PORT-CLOSED-ACCTS TO WS-DISP-COUNT
           DISPLAY '  Closed:            ' WS-DISP-COUNT
           MOVE WS-PORT-FROZEN-ACCTS TO WS-DISP-COUNT
           DISPLAY '  Frozen:            ' WS-DISP-COUNT

           DISPLAY ALL '-'
           DISPLAY 'BY TYPE:'
           MOVE WS-PORT-CHECKING-CT TO WS-DISP-COUNT
           DISPLAY '  Checking:          ' WS-DISP-COUNT
           MOVE WS-PORT-SAVINGS-CT TO WS-DISP-COUNT
           DISPLAY '  Savings:           ' WS-DISP-COUNT
           MOVE WS-PORT-MONEY-MKT-CT TO WS-DISP-COUNT
           DISPLAY '  Money Market:      ' WS-DISP-COUNT

           DISPLAY ALL '-'
           DISPLAY 'FINANCIAL SUMMARY:'
           MOVE WS-PORT-TOTAL-BALANCE TO WS-DISP-AMOUNT
           DISPLAY '  Total Balances:    ' WS-DISP-AMOUNT
           MOVE WS-PORT-AVG-BALANCE TO WS-DISP-AMOUNT
           DISPLAY '  Average Balance:   ' WS-DISP-AMOUNT
           MOVE WS-PORT-MAX-BALANCE TO WS-DISP-AMOUNT
           DISPLAY '  Highest Balance:   ' WS-DISP-AMOUNT
           MOVE WS-PORT-MIN-BALANCE TO WS-DISP-AMOUNT
           DISPLAY '  Lowest Balance:    ' WS-DISP-AMOUNT
           MOVE WS-PORT-TOTAL-DEPOSITS TO WS-DISP-AMOUNT
           DISPLAY '  YTD Deposits:      ' WS-DISP-AMOUNT
           MOVE WS-PORT-TOTAL-WDRW TO WS-DISP-AMOUNT
           DISPLAY '  YTD Withdrawals:   ' WS-DISP-AMOUNT
           DISPLAY ALL '='
           .

      *================================================================*
      * 4000 - LOAN PORTFOLIO                                         *
      * Demonstrates: START/READ NEXT on indexed file, accumulators   *
      *================================================================*
       4000-LOAN-PORTFOLIO.
           DISPLAY SPACES
           DISPLAY '=== LOAN PORTFOLIO REPORT ==='
           INITIALIZE WS-LOAN-ACCUM

           OPEN INPUT LOAN-FILE
           IF WS-LOAN-FILE-STATUS NOT = '00'
               IF WS-LOAN-FILE-STATUS = '35'
                   DISPLAY 'No loan file exists yet.'
               ELSE
                   MOVE 9001 TO LS-ERROR-CODE
                   MOVE 'Cannot open loan file'
                       TO LS-ERROR-MESSAGE
                   MOVE 3 TO LS-ERROR-SEVERITY
               END-IF
           ELSE
               MOVE 0 TO LF-LOAN-NUMBER
               START LOAN-FILE KEY >= LF-LOAN-NUMBER
               END-START

               IF WS-LOAN-FILE-STATUS = '00'
                   MOVE 'N' TO WS-EOF-FLAG

                   PERFORM UNTIL WS-EOF-FLAG = 'Y'
                       READ LOAN-FILE NEXT
                           AT END
                               MOVE 'Y' TO WS-EOF-FLAG
                           NOT AT END
                               PERFORM
                                   4100-ACCUMULATE-LOAN-STATS
                       END-READ
                   END-PERFORM
               END-IF

               CLOSE LOAN-FILE

               IF WS-LN-TOTAL-LOANS > 0
                   PERFORM 4200-DISPLAY-LOAN-PORTFOLIO
               ELSE
                   DISPLAY 'No loans found.'
               END-IF
           END-IF
           .

       4100-ACCUMULATE-LOAN-STATS.
           ADD 1 TO WS-LN-TOTAL-LOANS

           EVALUATE LF-LOAN-STATUS
               WHEN 'A' ADD 1 TO WS-LN-ACTIVE-LOANS
               WHEN 'P' ADD 1 TO WS-LN-PAID-LOANS
               WHEN 'D' ADD 1 TO WS-LN-DEFAULT-LOANS
           END-EVALUATE

           ADD LF-LOAN-PRINCIPAL TO WS-LN-TOTAL-PRINCIPAL
           ADD LF-LOAN-CURRENT-BAL TO WS-LN-TOTAL-OUTSTANDING
           ADD LF-LOAN-TOTAL-INTEREST TO WS-LN-TOTAL-INT-EARNED
           ADD LF-LOAN-LATE-FEES TO WS-LN-TOTAL-LATE-FEES
           .

       4200-DISPLAY-LOAN-PORTFOLIO.
           DISPLAY SPACES
           DISPLAY ALL '='
           DISPLAY '     LOAN PORTFOLIO SUMMARY'
           DISPLAY ALL '='

           MOVE WS-LN-TOTAL-LOANS TO WS-DISP-COUNT
           DISPLAY 'Total Loans:         ' WS-DISP-COUNT

           DISPLAY ALL '-'
           DISPLAY 'BY STATUS:'
           MOVE WS-LN-ACTIVE-LOANS TO WS-DISP-COUNT
           DISPLAY '  Active:            ' WS-DISP-COUNT
           MOVE WS-LN-PAID-LOANS TO WS-DISP-COUNT
           DISPLAY '  Paid Off:          ' WS-DISP-COUNT
           MOVE WS-LN-DEFAULT-LOANS TO WS-DISP-COUNT
           DISPLAY '  Defaulted:         ' WS-DISP-COUNT

           DISPLAY ALL '-'
           DISPLAY 'FINANCIAL SUMMARY:'
           MOVE WS-LN-TOTAL-PRINCIPAL TO WS-DISP-AMOUNT
           DISPLAY '  Total Originated:  ' WS-DISP-AMOUNT
           MOVE WS-LN-TOTAL-OUTSTANDING TO WS-DISP-AMOUNT
           DISPLAY '  Outstanding Bal:   ' WS-DISP-AMOUNT
           MOVE WS-LN-TOTAL-INT-EARNED TO WS-DISP-AMOUNT
           DISPLAY '  Interest Earned:   ' WS-DISP-AMOUNT
           MOVE WS-LN-TOTAL-LATE-FEES TO WS-DISP-AMOUNT
           DISPLAY '  Late Fees:         ' WS-DISP-AMOUNT
           DISPLAY ALL '='
           .

      *================================================================*
      * 5000 - OVERDUE LOANS REPORT                                   *
      * Demonstrates: Date comparison logic, conditional processing   *
      *================================================================*
       5000-OVERDUE-LOANS.
           DISPLAY SPACES
           DISPLAY '=== OVERDUE LOANS REPORT ==='

           OPEN INPUT LOAN-FILE
           IF WS-LOAN-FILE-STATUS NOT = '00'
               IF WS-LOAN-FILE-STATUS = '35'
                   DISPLAY 'No loan file exists yet.'
               ELSE
                   MOVE 9001 TO LS-ERROR-CODE
                   MOVE 'Cannot open loan file'
                       TO LS-ERROR-MESSAGE
                   MOVE 3 TO LS-ERROR-SEVERITY
               END-IF
           ELSE
               DISPLAY SPACES
               DISPLAY ALL '='
               DISPLAY '     OVERDUE LOANS'
               DISPLAY ALL '='
               DISPLAY 'Loan#        Balance'
                   '            Status   Payments Left'
               DISPLAY ALL '-'

               MOVE 0 TO LF-LOAN-NUMBER
               START LOAN-FILE KEY >= LF-LOAN-NUMBER
               END-START

               IF WS-LOAN-FILE-STATUS = '00'
                   MOVE 'N' TO WS-EOF-FLAG
                   MOVE 0 TO WS-LN-TOTAL-LOANS

                   PERFORM UNTIL WS-EOF-FLAG = 'Y'
                       READ LOAN-FILE NEXT
                           AT END
                               MOVE 'Y' TO WS-EOF-FLAG
                           NOT AT END
                               IF LF-LOAN-STATUS = 'A'
                                   AND LF-LOAN-PAYMENTS-LEFT > 0
                                   ADD 1 TO WS-LN-TOTAL-LOANS
                                   MOVE LF-LOAN-CURRENT-BAL
                                       TO WS-DISP-AMOUNT
                                   DISPLAY
                                       LF-LOAN-NUMBER '  '
                                       WS-DISP-AMOUNT '  '
                                       LF-LOAN-STATUS
                                       '        '
                                       LF-LOAN-PAYMENTS-LEFT
                               END-IF
                       END-READ
                   END-PERFORM
               END-IF

               DISPLAY ALL '-'
               MOVE WS-LN-TOTAL-LOANS TO WS-DISP-COUNT
               DISPLAY 'Active loans found: ' WS-DISP-COUNT
               DISPLAY ALL '='

               CLOSE LOAN-FILE
           END-IF
           .
