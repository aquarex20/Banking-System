      *================================================================*
      * Program:    BANKMAIN                                           *
      * Purpose:    Main Banking System - Menu-Driven Controller       *
      * Concepts:   CALL statements, subprogram linkage,               *
      *             PERFORM VARYING, STRING/UNSTRING, INSPECT,         *
      *             88-level conditions, nested IF, EVALUATE,          *
      *             reference modification, inline PERFORM,            *
      *             ACCEPT/DISPLAY screen I/O, COPY/REPLACE            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANKMAIN.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *----------------------------------------------------------------*
      * System control fields                                          *
      *----------------------------------------------------------------*
       01  WS-SYSTEM-CONTROL.
           05  WS-MAIN-MENU-CHOICE     PIC 9      VALUE 0.
               88  MENU-ACCT-MGMT      VALUE 1.
               88  MENU-TRANSACTIONS    VALUE 2.
               88  MENU-LOANS           VALUE 3.
               88  MENU-REPORTS         VALUE 4.
               88  MENU-ADMIN           VALUE 5.
               88  MENU-EXIT            VALUE 9.
           05  WS-SUB-MENU-CHOICE      PIC 9      VALUE 0.
           05  WS-CONTINUE-FLAG        PIC X      VALUE 'Y'.
               88  WS-CONTINUE         VALUE 'Y' 'y'.
               88  WS-STOP             VALUE 'N' 'n'.
           05  WS-RETURN-CODE          PIC S9(4)  VALUE 0.

      *----------------------------------------------------------------*
      * Date/Time fields demonstrating intrinsic functions             *
      *----------------------------------------------------------------*
       01  WS-DATE-TIME.
           05  WS-CURRENT-DATE-FULL    PIC X(21).
           05  WS-CURRENT-DATE-R REDEFINES WS-CURRENT-DATE-FULL.
               10  WS-CUR-YEAR         PIC 9(4).
               10  WS-CUR-MONTH        PIC 9(2).
               10  WS-CUR-DAY          PIC 9(2).
               10  WS-CUR-HOUR         PIC 9(2).
               10  WS-CUR-MINUTE       PIC 9(2).
               10  WS-CUR-SECOND       PIC 9(2).
               10  WS-CUR-HUND-SEC     PIC 9(2).
               10  WS-UTC-SIGN         PIC X.
               10  WS-UTC-HOURS        PIC 9(2).
               10  WS-UTC-MINUTES      PIC 9(2).
           05  WS-FORMATTED-DATE       PIC X(10).
           05  WS-FORMATTED-TIME       PIC X(8).
           05  WS-INTEGER-DATE         PIC 9(8).

      *----------------------------------------------------------------*
      * Session tracking (demonstrates tables/arrays)                  *
      *----------------------------------------------------------------*
       01  WS-SESSION-LOG.
           05  WS-SESSION-ID           PIC 9(8).
           05  WS-LOGIN-TIME           PIC X(8).
           05  WS-ACTION-COUNT         PIC 9(4)   VALUE 0.
           05  WS-ACTION-LOG.
               10  WS-ACTION-ENTRY OCCURS 100 TIMES
                   INDEXED BY WS-ACTION-IDX.
                   15  WS-ACTION-TIMESTAMP PIC X(8).
                   15  WS-ACTION-CODE      PIC X(4).
                   15  WS-ACTION-DESC      PIC X(30).

      *----------------------------------------------------------------*
      * Display formatting fields                                      *
      *----------------------------------------------------------------*
       01  WS-DISPLAY-FIELDS.
           05  WS-BANNER-LINE          PIC X(60).
           05  WS-DETAIL-LINE          PIC X(60).
           05  WS-SEPARATOR            PIC X(60)  VALUE ALL '='.
           05  WS-DASH-LINE            PIC X(60)  VALUE ALL '-'.
           05  WS-BLANK-LINE           PIC X(60)  VALUE SPACES.

      *----------------------------------------------------------------*
      * Subprogram linkage areas                                       *
      *----------------------------------------------------------------*
       01  WS-CALL-FUNCTION            PIC X(2).
       01  WS-CALL-RESULT              PIC S9(4).

      *----------------------------------------------------------------*
      * Teller authentication (demonstrates STRING/UNSTRING)           *
      *----------------------------------------------------------------*
       01  WS-TELLER-INFO.
           05  WS-TELLER-ID            PIC X(6).
           05  WS-TELLER-NAME          PIC X(30).
           05  WS-TELLER-LEVEL         PIC 9.
               88  TELLER-BASIC        VALUE 1.
               88  TELLER-SENIOR       VALUE 2.
               88  TELLER-SUPERVISOR   VALUE 3.
           05  WS-TELLER-FULL-ID       PIC X(40).

      *----------------------------------------------------------------*
      * Counters for session summary                                   *
      *----------------------------------------------------------------*
       01  WS-SESSION-COUNTERS.
           05  WS-TOTAL-DEPOSITS       PIC 9(5) VALUE 0.
           05  WS-TOTAL-WITHDRAWALS    PIC 9(5) VALUE 0.
           05  WS-TOTAL-TRANSFERS      PIC 9(5) VALUE 0.
           05  WS-TOTAL-INQUIRIES      PIC 9(5) VALUE 0.
           05  WS-TOTAL-ERRORS         PIC 9(5) VALUE 0.

      *----------------------------------------------------------------*
      * Include shared copybooks                                       *
      *----------------------------------------------------------------*
       COPY ACCTREC.
       COPY TRANREC.
       COPY LOANREC.
       COPY ERRHAND.

       PROCEDURE DIVISION.

      *================================================================*
      * MAIN-PROGRAM: Top-level control flow                           *
      *================================================================*
       0000-MAIN-PROGRAM.
           PERFORM 1000-INITIALIZE-SYSTEM
           PERFORM 1100-LOGIN-TELLER

           PERFORM 2000-MAIN-MENU-LOOP
               UNTIL WS-STOP

           PERFORM 9000-SESSION-SUMMARY
           PERFORM 9900-TERMINATE-SYSTEM
           STOP RUN
           .

      *================================================================*
      * INITIALIZATION                                                 *
      *================================================================*
       1000-INITIALIZE-SYSTEM.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FULL

           STRING WS-CUR-YEAR   DELIMITED SIZE
                  '-'            DELIMITED SIZE
                  WS-CUR-MONTH  DELIMITED SIZE
                  '-'            DELIMITED SIZE
                  WS-CUR-DAY    DELIMITED SIZE
               INTO WS-FORMATTED-DATE
           END-STRING

           STRING WS-CUR-HOUR   DELIMITED SIZE
                  ':'            DELIMITED SIZE
                  WS-CUR-MINUTE DELIMITED SIZE
                  ':'            DELIMITED SIZE
                  WS-CUR-SECOND DELIMITED SIZE
               INTO WS-FORMATTED-TIME
           END-STRING

           COMPUTE WS-SESSION-ID =
               FUNCTION RANDOM * 89999999 + 10000000

           MOVE WS-FORMATTED-TIME TO WS-LOGIN-TIME
           INITIALIZE WS-SESSION-COUNTERS
           SET ERR-NONE TO TRUE

           DISPLAY SPACES
           DISPLAY WS-SEPARATOR
           DISPLAY '   COBOL BANKING SYSTEM v2.0'
           DISPLAY '   Session: ' WS-SESSION-ID
           DISPLAY '   Date:    ' WS-FORMATTED-DATE
           DISPLAY '   Time:    ' WS-FORMATTED-TIME
           DISPLAY WS-SEPARATOR
           DISPLAY SPACES
           .

      *================================================================*
      * TELLER LOGIN - demonstrates STRING and INSPECT                 *
      *================================================================*
       1100-LOGIN-TELLER.
           DISPLAY 'Enter Teller ID (6 chars): '
               WITH NO ADVANCING
           ACCEPT WS-TELLER-ID

           INSPECT WS-TELLER-ID
               REPLACING ALL SPACES BY ZEROS

           DISPLAY 'Enter Teller Name: '
               WITH NO ADVANCING
           ACCEPT WS-TELLER-NAME

           MOVE 1 TO WS-TELLER-LEVEL

           STRING WS-TELLER-ID   DELIMITED SIZE
                  ' - '           DELIMITED SIZE
                  WS-TELLER-NAME DELIMITED '  '
               INTO WS-TELLER-FULL-ID
           END-STRING

           DISPLAY SPACES
           DISPLAY 'Logged in as: ' WS-TELLER-FULL-ID
           DISPLAY WS-DASH-LINE
           .

      *================================================================*
      * MAIN MENU LOOP - demonstrates EVALUATE (COBOL CASE statement) *
      *================================================================*
       2000-MAIN-MENU-LOOP.
           PERFORM 2100-DISPLAY-MAIN-MENU
           ACCEPT WS-MAIN-MENU-CHOICE

           EVALUATE TRUE
               WHEN MENU-ACCT-MGMT
                   PERFORM 3000-ACCOUNT-MANAGEMENT
               WHEN MENU-TRANSACTIONS
                   PERFORM 4000-TRANSACTION-PROCESSING
               WHEN MENU-LOANS
                   PERFORM 5000-LOAN-PROCESSING
               WHEN MENU-REPORTS
                   PERFORM 6000-REPORT-GENERATION
               WHEN MENU-ADMIN
                   PERFORM 7000-ADMINISTRATION
               WHEN MENU-EXIT
                   SET WS-STOP TO TRUE
               WHEN OTHER
                   DISPLAY '*** Invalid selection. Try again. ***'
                   ADD 1 TO WS-TOTAL-ERRORS
           END-EVALUATE

           IF NOT WS-STOP
               PERFORM 2200-LOG-ACTION
           END-IF
           .

       2100-DISPLAY-MAIN-MENU.
           DISPLAY SPACES
           DISPLAY WS-SEPARATOR
           DISPLAY '           MAIN MENU'
           DISPLAY WS-SEPARATOR
           DISPLAY '  1. Account Management'
           DISPLAY '  2. Transaction Processing'
           DISPLAY '  3. Loan Processing'
           DISPLAY '  4. Reports & Statements'
           DISPLAY '  5. Administration'
           DISPLAY WS-DASH-LINE
           DISPLAY '  9. Exit System'
           DISPLAY WS-SEPARATOR
           DISPLAY 'Selection: ' WITH NO ADVANCING
           .

       2200-LOG-ACTION.
           IF WS-ACTION-COUNT < 100
               ADD 1 TO WS-ACTION-COUNT
               SET WS-ACTION-IDX TO WS-ACTION-COUNT

               MOVE FUNCTION CURRENT-DATE(9:6)
                   TO WS-ACTION-TIMESTAMP(WS-ACTION-IDX)

               EVALUATE TRUE
                   WHEN MENU-ACCT-MGMT
                       MOVE 'ACCT'
                           TO WS-ACTION-CODE(WS-ACTION-IDX)
                       MOVE 'Account Management'
                           TO WS-ACTION-DESC(WS-ACTION-IDX)
                   WHEN MENU-TRANSACTIONS
                       MOVE 'TRAN'
                           TO WS-ACTION-CODE(WS-ACTION-IDX)
                       MOVE 'Transaction Processing'
                           TO WS-ACTION-DESC(WS-ACTION-IDX)
                   WHEN MENU-LOANS
                       MOVE 'LOAN'
                           TO WS-ACTION-CODE(WS-ACTION-IDX)
                       MOVE 'Loan Processing'
                           TO WS-ACTION-DESC(WS-ACTION-IDX)
                   WHEN MENU-REPORTS
                       MOVE 'RPTS'
                           TO WS-ACTION-CODE(WS-ACTION-IDX)
                       MOVE 'Reports Generation'
                           TO WS-ACTION-DESC(WS-ACTION-IDX)
                   WHEN MENU-ADMIN
                       MOVE 'ADMN'
                           TO WS-ACTION-CODE(WS-ACTION-IDX)
                       MOVE 'Administration'
                           TO WS-ACTION-DESC(WS-ACTION-IDX)
               END-EVALUATE
           END-IF
           .

      *================================================================*
      * ACCOUNT MANAGEMENT - CALLs subprogram ACCTMGMT                *
      *================================================================*
       3000-ACCOUNT-MANAGEMENT.
           DISPLAY SPACES
           DISPLAY WS-DASH-LINE
           DISPLAY '     ACCOUNT MANAGEMENT'
           DISPLAY WS-DASH-LINE
           DISPLAY '  1. Open New Account'
           DISPLAY '  2. Close Account'
           DISPLAY '  3. Account Inquiry'
           DISPLAY '  4. Update Account Info'
           DISPLAY '  5. Freeze/Unfreeze Account'
           DISPLAY '  0. Return to Main Menu'
           DISPLAY WS-DASH-LINE
           DISPLAY 'Selection: ' WITH NO ADVANCING
           ACCEPT WS-SUB-MENU-CHOICE

           IF WS-SUB-MENU-CHOICE NOT = 0
               EVALUATE WS-SUB-MENU-CHOICE
                   WHEN 1 MOVE 'OP' TO WS-CALL-FUNCTION
                   WHEN 2 MOVE 'CL' TO WS-CALL-FUNCTION
                   WHEN 3 MOVE 'IQ' TO WS-CALL-FUNCTION
                   WHEN 4 MOVE 'UP' TO WS-CALL-FUNCTION
                   WHEN 5 MOVE 'FZ' TO WS-CALL-FUNCTION
                   WHEN OTHER
                       DISPLAY '*** Invalid selection ***'
                       MOVE 'XX' TO WS-CALL-FUNCTION
               END-EVALUATE

               IF WS-CALL-FUNCTION NOT = 'XX'
                   CALL 'ACCTMGMT' USING
                       WS-CALL-FUNCTION
                       WS-ACCOUNT-RECORD
                       WS-ERROR-HANDLING
                       WS-TELLER-INFO
                   END-CALL
                   ADD 1 TO WS-TOTAL-INQUIRIES
                   PERFORM 8000-CHECK-ERRORS
               END-IF
           END-IF
           .

      *================================================================*
      * TRANSACTION PROCESSING - CALLs subprogram TRANPROC             *
      *================================================================*
       4000-TRANSACTION-PROCESSING.
           DISPLAY SPACES
           DISPLAY WS-DASH-LINE
           DISPLAY '     TRANSACTION PROCESSING'
           DISPLAY WS-DASH-LINE
           DISPLAY '  1. Deposit'
           DISPLAY '  2. Withdrawal'
           DISPLAY '  3. Transfer Between Accounts'
           DISPLAY '  4. Balance Inquiry'
           DISPLAY '  5. Transaction History'
           DISPLAY '  0. Return to Main Menu'
           DISPLAY WS-DASH-LINE
           DISPLAY 'Selection: ' WITH NO ADVANCING
           ACCEPT WS-SUB-MENU-CHOICE

           IF WS-SUB-MENU-CHOICE NOT = 0
               EVALUATE WS-SUB-MENU-CHOICE
                   WHEN 1 MOVE 'DP' TO WS-CALL-FUNCTION
                   WHEN 2 MOVE 'WD' TO WS-CALL-FUNCTION
                   WHEN 3 MOVE 'TR' TO WS-CALL-FUNCTION
                   WHEN 4 MOVE 'BQ' TO WS-CALL-FUNCTION
                   WHEN 5 MOVE 'TH' TO WS-CALL-FUNCTION
                   WHEN OTHER
                       DISPLAY '*** Invalid selection ***'
                       MOVE 'XX' TO WS-CALL-FUNCTION
               END-EVALUATE

               IF WS-CALL-FUNCTION NOT = 'XX'
                   CALL 'TRANPROC' USING
                       WS-CALL-FUNCTION
                       WS-ACCOUNT-RECORD
                       WS-TRANSACTION-RECORD
                       WS-ERROR-HANDLING
                       WS-TELLER-INFO
                   END-CALL

                   EVALUATE WS-CALL-FUNCTION
                       WHEN 'DP'
                           ADD 1 TO WS-TOTAL-DEPOSITS
                       WHEN 'WD'
                           ADD 1 TO WS-TOTAL-WITHDRAWALS
                       WHEN 'TR'
                           ADD 1 TO WS-TOTAL-TRANSFERS
                   END-EVALUATE

                   PERFORM 8000-CHECK-ERRORS
               END-IF
           END-IF
           .

      *================================================================*
      * LOAN PROCESSING - CALLs subprogram LOANPROC                   *
      *================================================================*
       5000-LOAN-PROCESSING.
           DISPLAY SPACES
           DISPLAY WS-DASH-LINE
           DISPLAY '     LOAN PROCESSING'
           DISPLAY WS-DASH-LINE
           DISPLAY '  1. Loan Application'
           DISPLAY '  2. Loan Payment'
           DISPLAY '  3. Loan Inquiry'
           DISPLAY '  4. Amortization Schedule'
           DISPLAY '  5. Payoff Quote'
           DISPLAY '  0. Return to Main Menu'
           DISPLAY WS-DASH-LINE
           DISPLAY 'Selection: ' WITH NO ADVANCING
           ACCEPT WS-SUB-MENU-CHOICE

           IF WS-SUB-MENU-CHOICE NOT = 0
               EVALUATE WS-SUB-MENU-CHOICE
                   WHEN 1 MOVE 'AP' TO WS-CALL-FUNCTION
                   WHEN 2 MOVE 'PM' TO WS-CALL-FUNCTION
                   WHEN 3 MOVE 'IQ' TO WS-CALL-FUNCTION
                   WHEN 4 MOVE 'AM' TO WS-CALL-FUNCTION
                   WHEN 5 MOVE 'PO' TO WS-CALL-FUNCTION
                   WHEN OTHER
                       DISPLAY '*** Invalid selection ***'
                       MOVE 'XX' TO WS-CALL-FUNCTION
               END-EVALUATE

               IF WS-CALL-FUNCTION NOT = 'XX'
                   CALL 'LOANPROC' USING
                       WS-CALL-FUNCTION
                       WS-LOAN-RECORD
                       WS-ACCOUNT-RECORD
                       WS-ERROR-HANDLING
                   END-CALL
                   PERFORM 8000-CHECK-ERRORS
               END-IF
           END-IF
           .

      *================================================================*
      * REPORT GENERATION - CALLs subprogram RPTGEN                   *
      *================================================================*
       6000-REPORT-GENERATION.
           DISPLAY SPACES
           DISPLAY WS-DASH-LINE
           DISPLAY '     REPORTS & STATEMENTS'
           DISPLAY WS-DASH-LINE
           DISPLAY '  1. Account Statement'
           DISPLAY '  2. Daily Transaction Summary'
           DISPLAY '  3. Account Portfolio Summary'
           DISPLAY '  4. Loan Portfolio Report'
           DISPLAY '  5. Overdue Loans Report'
           DISPLAY '  0. Return to Main Menu'
           DISPLAY WS-DASH-LINE
           DISPLAY 'Selection: ' WITH NO ADVANCING
           ACCEPT WS-SUB-MENU-CHOICE

           IF WS-SUB-MENU-CHOICE NOT = 0
               EVALUATE WS-SUB-MENU-CHOICE
                   WHEN 1 MOVE 'AS' TO WS-CALL-FUNCTION
                   WHEN 2 MOVE 'DT' TO WS-CALL-FUNCTION
                   WHEN 3 MOVE 'AP' TO WS-CALL-FUNCTION
                   WHEN 4 MOVE 'LP' TO WS-CALL-FUNCTION
                   WHEN 5 MOVE 'OL' TO WS-CALL-FUNCTION
                   WHEN OTHER
                       DISPLAY '*** Invalid selection ***'
                       MOVE 'XX' TO WS-CALL-FUNCTION
               END-EVALUATE

               IF WS-CALL-FUNCTION NOT = 'XX'
                   CALL 'RPTGEN' USING
                       WS-CALL-FUNCTION
                       WS-ACCOUNT-RECORD
                       WS-TRANSACTION-RECORD
                       WS-LOAN-RECORD
                       WS-ERROR-HANDLING
                   END-CALL
                   PERFORM 8000-CHECK-ERRORS
               END-IF
           END-IF
           .

      *================================================================*
      * ADMINISTRATION - demonstrates advanced inline logic            *
      *================================================================*
       7000-ADMINISTRATION.
           DISPLAY SPACES
           DISPLAY WS-DASH-LINE
           DISPLAY '     ADMINISTRATION'
           DISPLAY WS-DASH-LINE
           DISPLAY '  1. View Session Log'
           DISPLAY '  2. System Status'
           DISPLAY '  3. Interest Rate Update'
           DISPLAY '  0. Return to Main Menu'
           DISPLAY WS-DASH-LINE
           DISPLAY 'Selection: ' WITH NO ADVANCING
           ACCEPT WS-SUB-MENU-CHOICE

           EVALUATE WS-SUB-MENU-CHOICE
               WHEN 1
                   PERFORM 7100-VIEW-SESSION-LOG
               WHEN 2
                   PERFORM 7200-SYSTEM-STATUS
               WHEN 3
                   PERFORM 7300-INTEREST-RATE-UPDATE
               WHEN 0
                   CONTINUE
               WHEN OTHER
                   DISPLAY '*** Invalid selection ***'
           END-EVALUATE
           .

      *----------------------------------------------------------------*
      * View session log - demonstrates PERFORM VARYING with tables    *
      *----------------------------------------------------------------*
       7100-VIEW-SESSION-LOG.
           DISPLAY SPACES
           DISPLAY '--- SESSION ACTIVITY LOG ---'
           DISPLAY 'Time     Code  Description'
           DISPLAY WS-DASH-LINE

           IF WS-ACTION-COUNT = 0
               DISPLAY 'No actions recorded yet.'
           ELSE
               PERFORM VARYING WS-ACTION-IDX
                   FROM 1 BY 1
                   UNTIL WS-ACTION-IDX > WS-ACTION-COUNT
                   DISPLAY
                       WS-ACTION-TIMESTAMP(WS-ACTION-IDX) '  '
                       WS-ACTION-CODE(WS-ACTION-IDX) '  '
                       WS-ACTION-DESC(WS-ACTION-IDX)
               END-PERFORM
           END-IF

           DISPLAY WS-DASH-LINE
           DISPLAY 'Total actions: ' WS-ACTION-COUNT
           .

      *----------------------------------------------------------------*
      * System status - demonstrates COMPUTE and formatted output      *
      *----------------------------------------------------------------*
       7200-SYSTEM-STATUS.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FULL

           DISPLAY SPACES
           DISPLAY '--- SYSTEM STATUS ---'
           DISPLAY 'Session ID:     ' WS-SESSION-ID
           DISPLAY 'Teller:         ' WS-TELLER-FULL-ID
           DISPLAY 'Login Time:     ' WS-LOGIN-TIME
           DISPLAY 'Current Time:   '
               WS-CUR-HOUR ':' WS-CUR-MINUTE ':' WS-CUR-SECOND
           DISPLAY WS-DASH-LINE
           DISPLAY 'Deposits:       ' WS-TOTAL-DEPOSITS
           DISPLAY 'Withdrawals:    ' WS-TOTAL-WITHDRAWALS
           DISPLAY 'Transfers:      ' WS-TOTAL-TRANSFERS
           DISPLAY 'Inquiries:      ' WS-TOTAL-INQUIRIES
           DISPLAY 'Errors:         ' WS-TOTAL-ERRORS
           DISPLAY WS-DASH-LINE
           .

      *----------------------------------------------------------------*
      * Interest rate update - demonstrates numeric editing            *
      *----------------------------------------------------------------*
       7300-INTEREST-RATE-UPDATE.
           DISPLAY SPACES
           DISPLAY '--- INTEREST RATE UPDATE ---'
           DISPLAY 'Feature available to supervisors only.'

           IF TELLER-SUPERVISOR
               DISPLAY 'Enter new savings rate (e.g., 0.0325): '
                   WITH NO ADVANCING
               ACCEPT WS-INTEREST-RATE
               DISPLAY 'Rate updated to: ' WS-INTEREST-RATE
           ELSE
               DISPLAY '*** Access denied. Supervisor level required.'
           END-IF
           .

      *================================================================*
      * ERROR CHECKING - common error handler                          *
      *================================================================*
       8000-CHECK-ERRORS.
           IF NOT ERR-NONE
               ADD 1 TO WS-TOTAL-ERRORS
               DISPLAY SPACES
               DISPLAY '!!! ERROR ' WS-ERROR-CODE ' !!!'
               DISPLAY 'Module:  ' WS-ERROR-MODULE
               DISPLAY 'Message: ' WS-ERROR-MESSAGE

               EVALUATE TRUE
                   WHEN SEV-INFO
                       DISPLAY 'Severity: INFORMATIONAL'
                   WHEN SEV-WARNING
                       DISPLAY 'Severity: WARNING'
                   WHEN SEV-ERROR
                       DISPLAY 'Severity: ERROR'
                   WHEN SEV-CRITICAL
                       DISPLAY 'Severity: CRITICAL - Contact Support'
               END-EVALUATE

               SET ERR-NONE TO TRUE
           END-IF
           .

      *================================================================*
      * SESSION SUMMARY - end-of-session report                        *
      *================================================================*
       9000-SESSION-SUMMARY.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FULL

           DISPLAY SPACES
           DISPLAY WS-SEPARATOR
           DISPLAY '       END OF SESSION SUMMARY'
           DISPLAY WS-SEPARATOR
           DISPLAY 'Session ID:     ' WS-SESSION-ID
           DISPLAY 'Teller:         ' WS-TELLER-FULL-ID
           DISPLAY 'Login Time:     ' WS-LOGIN-TIME
           DISPLAY 'Logout Time:    '
               WS-CUR-HOUR ':' WS-CUR-MINUTE ':' WS-CUR-SECOND
           DISPLAY WS-DASH-LINE
           DISPLAY '  Deposits Made:      ' WS-TOTAL-DEPOSITS
           DISPLAY '  Withdrawals Made:   ' WS-TOTAL-WITHDRAWALS
           DISPLAY '  Transfers Made:     ' WS-TOTAL-TRANSFERS
           DISPLAY '  Inquiries:          ' WS-TOTAL-INQUIRIES
           DISPLAY '  Errors Encountered: ' WS-TOTAL-ERRORS
           DISPLAY '  Total Actions:      ' WS-ACTION-COUNT
           DISPLAY WS-DASH-LINE
           DISPLAY 'Session log entries:'
           PERFORM VARYING WS-ACTION-IDX
               FROM 1 BY 1
               UNTIL WS-ACTION-IDX > WS-ACTION-COUNT
               DISPLAY '  ' WS-ACTION-CODE(WS-ACTION-IDX) ' - '
                   WS-ACTION-DESC(WS-ACTION-IDX)
           END-PERFORM
           DISPLAY WS-SEPARATOR
           .

      *================================================================*
      * TERMINATION                                                    *
      *================================================================*
       9900-TERMINATE-SYSTEM.
           DISPLAY SPACES
           DISPLAY 'Thank you for using COBOL Banking System.'
           DISPLAY 'Goodbye!'
           DISPLAY SPACES
           .
