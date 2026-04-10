      *================================================================*
      * Program:    ACCTMGMT                                           *
      * Purpose:    Account Management Subprogram                      *
      * Concepts:   Subprogram with LINKAGE SECTION, file I/O          *
      *             (sequential + indexed), SEARCH/SEARCH ALL,         *
      *             REDEFINES, condition names, STRING, INSPECT,       *
      *             reference modification, PERFORM THRU,              *
      *             ON SIZE ERROR, declaratives                        *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCTMGMT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE
               ASSIGN TO 'data/ACCOUNTS.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AF-ACCT-NUMBER
               FILE STATUS IS WS-FILE-STATUS.

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

       WORKING-STORAGE SECTION.

       01  WS-FILE-STATUS              PIC X(2).
           88  FILE-OK                 VALUE '00'.
           88  FILE-DUP-KEY            VALUE '22'.
           88  FILE-NOT-FOUND          VALUE '23'.
           88  FILE-EOF                VALUE '10'.

       01  WS-FILE-OPEN-FLAG          PIC X VALUE 'N'.
           88  FILE-IS-OPEN            VALUE 'Y'.
           88  FILE-IS-CLOSED          VALUE 'N'.

      *----------------------------------------------------------------*
      * Account number generation - uses table for branch routing      *
      *----------------------------------------------------------------*
       01  WS-NEXT-ACCT-NUMBER         PIC 9(10) VALUE 1000000001.

       01  WS-BRANCH-TABLE.
           05  FILLER PIC X(22) VALUE '01NEW YORK MAIN      '.
           05  FILLER PIC X(22) VALUE '02BOSTON CENTRAL      '.
           05  FILLER PIC X(22) VALUE '03CHICAGO LOOP       '.
           05  FILLER PIC X(22) VALUE '04LOS ANGELES HQ     '.
           05  FILLER PIC X(22) VALUE '05HOUSTON GALLERIA   '.
       01  WS-BRANCH-TABLE-R REDEFINES WS-BRANCH-TABLE.
           05  WS-BRANCH-ENTRY OCCURS 5 TIMES
               ASCENDING KEY IS WS-BRANCH-CODE
               INDEXED BY WS-BRANCH-IDX.
               10  WS-BRANCH-CODE      PIC 9(2).
               10  WS-BRANCH-NAME      PIC X(20).

       01  WS-SEARCH-BRANCH-CODE       PIC 9(2).

      *----------------------------------------------------------------*
      * Account type descriptions using table                          *
      *----------------------------------------------------------------*
       01  WS-ACCT-TYPE-TABLE.
           05  FILLER PIC X(21) VALUE 'CChecking            '.
           05  FILLER PIC X(21) VALUE 'SSavings             '.
           05  FILLER PIC X(21) VALUE 'MMoney Market        '.
       01  WS-ACCT-TYPE-TABLE-R REDEFINES WS-ACCT-TYPE-TABLE.
           05  WS-ACCT-TYPE-ENTRY OCCURS 3 TIMES
               INDEXED BY WS-TYPE-IDX.
               10  WS-TYPE-CODE        PIC X(1).
               10  WS-TYPE-DESC        PIC X(20).

      *----------------------------------------------------------------*
      * Working fields for display formatting                          *
      *----------------------------------------------------------------*
       01  WS-DISPLAY-BALANCE          PIC $ZZZ,ZZZ,ZZ9.99-.
       01  WS-DISPLAY-RATE             PIC 9.9999.
       01  WS-DISPLAY-SSN              PIC X(11).
       01  WS-DISPLAY-PHONE            PIC X(14).
       01  WS-DISPLAY-DATE             PIC X(10).
       01  WS-RAW-DATE                 PIC 9(8).
       01  WS-WORK-STRING              PIC X(60).

       01  WS-INPUT-FIELDS.
           05  WS-INPUT-ACCT-NUM       PIC 9(10).
           05  WS-INPUT-ACCT-TYPE      PIC X(1).
           05  WS-INPUT-AMOUNT         PIC S9(11)V99.
           05  WS-INPUT-CONFIRM        PIC X.

       01  WS-SEPARATOR                PIC X(60) VALUE ALL '-'.
       01  WS-CURRENT-DATE-FIELD       PIC 9(8).

      *----------------------------------------------------------------*
      * Checksum validation field (demonstrates COMPUTE / arithmetic)  *
      *----------------------------------------------------------------*
       01  WS-CHECKSUM-FIELDS.
           05  WS-CHECK-DIGIT          PIC 9.
           05  WS-CHECK-SUM            PIC 9(5).
           05  WS-CHECK-REMAINDER      PIC 9(3).
           05  WS-DIGIT-VAL            PIC 9.
           05  WS-DIGIT-IDX            PIC 9(2).

      *================================================================*
      * LINKAGE SECTION - parameters received from calling program     *
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
           LS-ERROR-HANDLING
           LS-TELLER-INFO.

       0000-MAIN-ENTRY.
           MOVE 0000 TO LS-ERROR-CODE
           MOVE SPACES TO LS-ERROR-MESSAGE
           MOVE 'ACCTMGMT' TO LS-ERROR-MODULE

           PERFORM 0100-OPEN-FILE

           IF FILE-IS-OPEN
               EVALUATE LS-FUNCTION-CODE
                   WHEN 'OP'
                       PERFORM 1000-OPEN-ACCOUNT
                   WHEN 'CL'
                       PERFORM 2000-CLOSE-ACCOUNT
                   WHEN 'IQ'
                       PERFORM 3000-ACCOUNT-INQUIRY
                   WHEN 'UP'
                       PERFORM 4000-UPDATE-ACCOUNT
                   WHEN 'FZ'
                       PERFORM 5000-FREEZE-ACCOUNT
                   WHEN OTHER
                       MOVE 9002 TO LS-ERROR-CODE
                       MOVE 'Invalid function code'
                           TO LS-ERROR-MESSAGE
                       MOVE 2 TO LS-ERROR-SEVERITY
               END-EVALUATE

               PERFORM 0200-CLOSE-FILE
           END-IF

           GOBACK
           .

      *================================================================*
      * FILE OPERATIONS                                                *
      *================================================================*
       0100-OPEN-FILE.
           IF FILE-IS-CLOSED
               OPEN I-O ACCOUNT-FILE
               IF FILE-OK
                   SET FILE-IS-OPEN TO TRUE
               ELSE
                   IF WS-FILE-STATUS = '35'
                       OPEN OUTPUT ACCOUNT-FILE
                       IF FILE-OK
                           CLOSE ACCOUNT-FILE
                           OPEN I-O ACCOUNT-FILE
                           IF FILE-OK
                               SET FILE-IS-OPEN TO TRUE
                           END-IF
                       END-IF
                   END-IF
                   IF FILE-IS-CLOSED
                       MOVE 9001 TO LS-ERROR-CODE
                       STRING 'File open error: status='
                           DELIMITED SIZE
                           WS-FILE-STATUS DELIMITED SIZE
                           INTO LS-ERROR-MESSAGE
                       END-STRING
                       MOVE 3 TO LS-ERROR-SEVERITY
                   END-IF
               END-IF
           END-IF
           .

       0200-CLOSE-FILE.
           IF FILE-IS-OPEN
               CLOSE ACCOUNT-FILE
               SET FILE-IS-CLOSED TO TRUE
           END-IF
           .

      *================================================================*
      * 1000 - OPEN NEW ACCOUNT                                       *
      * Demonstrates: input validation, SEARCH ALL, computed fields,   *
      *               checksum algorithm, ON SIZE ERROR                *
      *================================================================*
       1000-OPEN-ACCOUNT.
           DISPLAY SPACES
           DISPLAY '=== OPEN NEW ACCOUNT ==='
           DISPLAY SPACES

           PERFORM 1100-GET-HOLDER-INFO
           PERFORM 1200-GET-ACCOUNT-TYPE
           PERFORM 1300-GET-INITIAL-DEPOSIT

           IF LS-ERROR-CODE = 0000
               PERFORM 1400-GENERATE-ACCOUNT-NUMBER
               PERFORM 1500-SET-ACCOUNT-DEFAULTS
               PERFORM 1600-WRITE-ACCOUNT
               PERFORM 1700-DISPLAY-CONFIRMATION
           END-IF
           .

       1100-GET-HOLDER-INFO.
           DISPLAY 'First Name: ' WITH NO ADVANCING
           ACCEPT LS-HOLDER-FIRST-NAME
           DISPLAY 'Last Name: ' WITH NO ADVANCING
           ACCEPT LS-HOLDER-LAST-NAME
           DISPLAY 'Middle Initial: ' WITH NO ADVANCING
           ACCEPT LS-HOLDER-MI
           DISPLAY 'SSN (9 digits): ' WITH NO ADVANCING
           ACCEPT LS-HOLDER-SSN
           DISPLAY 'Date of Birth (YYYYMMDD): ' WITH NO ADVANCING
           ACCEPT LS-HOLDER-DOB
           DISPLAY 'Phone (10 digits): ' WITH NO ADVANCING
           ACCEPT LS-HOLDER-PHONE
           DISPLAY 'Street Address: ' WITH NO ADVANCING
           ACCEPT LS-ADDR-STREET
           DISPLAY 'City: ' WITH NO ADVANCING
           ACCEPT LS-ADDR-CITY
           DISPLAY 'State (2 chars): ' WITH NO ADVANCING
           ACCEPT LS-ADDR-STATE

           INSPECT LS-ADDR-STATE
               CONVERTING 'abcdefghijklmnopqrstuvwxyz'
               TO         'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

           DISPLAY 'ZIP (5 digits): ' WITH NO ADVANCING
           ACCEPT LS-ADDR-ZIP
           .

       1200-GET-ACCOUNT-TYPE.
           DISPLAY SPACES
           DISPLAY 'Account Types:'

           PERFORM VARYING WS-TYPE-IDX FROM 1 BY 1
               UNTIL WS-TYPE-IDX > 3
               DISPLAY '  ' WS-TYPE-CODE(WS-TYPE-IDX)
                   ' - ' WS-TYPE-DESC(WS-TYPE-IDX)
           END-PERFORM

           DISPLAY 'Select type: ' WITH NO ADVANCING
           ACCEPT WS-INPUT-ACCT-TYPE

           INSPECT WS-INPUT-ACCT-TYPE
               CONVERTING 'csm' TO 'CSM'

           IF WS-INPUT-ACCT-TYPE NOT = 'C' AND
              WS-INPUT-ACCT-TYPE NOT = 'S' AND
              WS-INPUT-ACCT-TYPE NOT = 'M'
               MOVE 9002 TO LS-ERROR-CODE
               MOVE 'Invalid account type selected'
                   TO LS-ERROR-MESSAGE
               MOVE 2 TO LS-ERROR-SEVERITY
           ELSE
               MOVE WS-INPUT-ACCT-TYPE TO LS-ACCT-TYPE
           END-IF
           .

       1300-GET-INITIAL-DEPOSIT.
           IF LS-ERROR-CODE = 0000
               DISPLAY 'Initial Deposit Amount: ' WITH NO ADVANCING
               ACCEPT WS-INPUT-AMOUNT

               EVALUATE LS-ACCT-TYPE
                   WHEN 'C'
                       IF WS-INPUT-AMOUNT < 100
                           MOVE 2003 TO LS-ERROR-CODE
                           MOVE 'Minimum $100 for checking'
                               TO LS-ERROR-MESSAGE
                           MOVE 2 TO LS-ERROR-SEVERITY
                       END-IF
                   WHEN 'S'
                       IF WS-INPUT-AMOUNT < 50
                           MOVE 2003 TO LS-ERROR-CODE
                           MOVE 'Minimum $50 for savings'
                               TO LS-ERROR-MESSAGE
                           MOVE 2 TO LS-ERROR-SEVERITY
                       END-IF
                   WHEN 'M'
                       IF WS-INPUT-AMOUNT < 2500
                           MOVE 2003 TO LS-ERROR-CODE
                           MOVE 'Minimum $2500 for money market'
                               TO LS-ERROR-MESSAGE
                           MOVE 2 TO LS-ERROR-SEVERITY
                       END-IF
               END-EVALUATE
           END-IF
           .

      *----------------------------------------------------------------*
      * Account number generation with checksum (Luhn-like algorithm)  *
      *----------------------------------------------------------------*
       1400-GENERATE-ACCOUNT-NUMBER.
           ADD 1 TO WS-NEXT-ACCT-NUMBER
           MOVE WS-NEXT-ACCT-NUMBER TO LS-ACCT-NUMBER

           MOVE 0 TO WS-CHECK-SUM
           PERFORM VARYING WS-DIGIT-IDX FROM 1 BY 1
               UNTIL WS-DIGIT-IDX > 9
               MOVE LS-ACCT-NUMBER(WS-DIGIT-IDX:1)
                   TO WS-DIGIT-VAL
               COMPUTE WS-CHECK-SUM =
                   WS-CHECK-SUM + WS-DIGIT-VAL * WS-DIGIT-IDX
                   ON SIZE ERROR
                       MOVE 0 TO WS-CHECK-SUM
               END-COMPUTE
           END-PERFORM

           DIVIDE WS-CHECK-SUM BY 10
               GIVING WS-CHECK-SUM
               REMAINDER WS-CHECK-DIGIT

           MOVE WS-CHECK-DIGIT TO LS-ACCT-NUMBER(10:1)
           .

       1500-SET-ACCOUNT-DEFAULTS.
           MOVE 'A' TO LS-ACCT-STATUS
           MOVE WS-INPUT-AMOUNT TO LS-CURRENT-BALANCE
           MOVE WS-INPUT-AMOUNT TO LS-AVAILABLE-BALANCE
           MOVE 0 TO LS-ACCRUED-INTEREST
           MOVE 0 TO LS-MONTHLY-TRANS-CT
           MOVE WS-INPUT-AMOUNT TO LS-YTD-DEPOSITS
           MOVE 0 TO LS-YTD-WITHDRAWALS

           EVALUATE LS-ACCT-TYPE
               WHEN 'C'
                   MOVE 0.0010 TO LS-INTEREST-RATE
                   MOVE 500.00 TO LS-OVERDRAFT-LIMIT
                   MOVE 0 TO LS-MIN-BALANCE
               WHEN 'S'
                   MOVE 0.0250 TO LS-INTEREST-RATE
                   MOVE 0 TO LS-OVERDRAFT-LIMIT
                   MOVE 25.00 TO LS-MIN-BALANCE
               WHEN 'M'
                   MOVE 0.0400 TO LS-INTEREST-RATE
                   MOVE 0 TO LS-OVERDRAFT-LIMIT
                   MOVE 2500.00 TO LS-MIN-BALANCE
           END-EVALUATE

           MOVE FUNCTION CURRENT-DATE(1:8)
               TO LS-DATE-OPENED
           MOVE 0 TO LS-DATE-CLOSED
           MOVE LS-DATE-OPENED TO LS-LAST-TRANS-DATE
           MOVE 0 TO LS-LAST-STMT-DATE
           .

       1600-WRITE-ACCOUNT.
           MOVE LS-ACCT-NUMBER     TO AF-ACCT-NUMBER
           MOVE LS-ACCT-TYPE       TO AF-ACCT-TYPE
           MOVE LS-ACCT-STATUS     TO AF-ACCT-STATUS
           MOVE LS-HOLDER-LAST-NAME TO AF-HOLDER-LAST-NAME
           MOVE LS-HOLDER-FIRST-NAME TO AF-HOLDER-FIRST-NAME
           MOVE LS-HOLDER-MI       TO AF-HOLDER-MI
           MOVE LS-HOLDER-SSN      TO AF-HOLDER-SSN
           MOVE LS-HOLDER-DOB      TO AF-HOLDER-DOB
           MOVE LS-HOLDER-PHONE    TO AF-HOLDER-PHONE
           MOVE LS-ADDR-STREET     TO AF-ADDR-STREET
           MOVE LS-ADDR-CITY       TO AF-ADDR-CITY
           MOVE LS-ADDR-STATE      TO AF-ADDR-STATE
           MOVE LS-ADDR-ZIP        TO AF-ADDR-ZIP
           MOVE LS-CURRENT-BALANCE TO AF-CURRENT-BALANCE
           MOVE LS-AVAILABLE-BALANCE TO AF-AVAILABLE-BALANCE
           MOVE LS-INTEREST-RATE   TO AF-INTEREST-RATE
           MOVE LS-ACCRUED-INTEREST TO AF-ACCRUED-INTEREST
           MOVE LS-OVERDRAFT-LIMIT TO AF-OVERDRAFT-LIMIT
           MOVE LS-MIN-BALANCE     TO AF-MIN-BALANCE
           MOVE LS-DATE-OPENED     TO AF-DATE-OPENED
           MOVE LS-DATE-CLOSED     TO AF-DATE-CLOSED
           MOVE LS-LAST-TRANS-DATE TO AF-LAST-TRANS-DATE
           MOVE LS-LAST-STMT-DATE  TO AF-LAST-STMT-DATE
           MOVE LS-MONTHLY-TRANS-CT TO AF-MONTHLY-TRANS-CT
           MOVE LS-YTD-DEPOSITS    TO AF-YTD-DEPOSITS
           MOVE LS-YTD-WITHDRAWALS TO AF-YTD-WITHDRAWALS
           MOVE SPACES             TO AF-FILLER

           WRITE ACCOUNT-FILE-RECORD
           IF NOT FILE-OK
               IF FILE-DUP-KEY
                   ADD 1 TO WS-NEXT-ACCT-NUMBER
                   PERFORM 1400-GENERATE-ACCOUNT-NUMBER
                   MOVE LS-ACCT-NUMBER TO AF-ACCT-NUMBER
                   WRITE ACCOUNT-FILE-RECORD
                   IF NOT FILE-OK
                       MOVE 9001 TO LS-ERROR-CODE
                       MOVE 'Failed to write account record'
                           TO LS-ERROR-MESSAGE
                       MOVE 3 TO LS-ERROR-SEVERITY
                   END-IF
               ELSE
                   MOVE 9001 TO LS-ERROR-CODE
                   STRING 'Write error: status='
                       DELIMITED SIZE
                       WS-FILE-STATUS DELIMITED SIZE
                       INTO LS-ERROR-MESSAGE
                   END-STRING
                   MOVE 3 TO LS-ERROR-SEVERITY
               END-IF
           END-IF
           .

       1700-DISPLAY-CONFIRMATION.
           IF LS-ERROR-CODE = 0000
               DISPLAY SPACES
               DISPLAY '*** ACCOUNT CREATED SUCCESSFULLY ***'
               DISPLAY 'Account Number: ' LS-ACCT-NUMBER
               DISPLAY 'Account Type:   ' LS-ACCT-TYPE
               DISPLAY 'Holder:         '
                   LS-HOLDER-FIRST-NAME ' '
                   LS-HOLDER-LAST-NAME

               MOVE LS-CURRENT-BALANCE TO WS-DISPLAY-BALANCE
               DISPLAY 'Balance:        ' WS-DISPLAY-BALANCE
               DISPLAY 'Date Opened:    ' LS-DATE-OPENED
           END-IF
           .

      *================================================================*
      * 2000 - CLOSE ACCOUNT                                          *
      *================================================================*
       2000-CLOSE-ACCOUNT.
           DISPLAY SPACES
           DISPLAY '=== CLOSE ACCOUNT ==='
           DISPLAY 'Enter Account Number: ' WITH NO ADVANCING
           ACCEPT WS-INPUT-ACCT-NUM

           PERFORM 3100-READ-ACCOUNT

           IF LS-ERROR-CODE = 0000
               IF LS-ACCT-STATUS = 'C'
                   MOVE 1002 TO LS-ERROR-CODE
                   MOVE 'Account is already closed'
                       TO LS-ERROR-MESSAGE
                   MOVE 1 TO LS-ERROR-SEVERITY
               ELSE
                   MOVE LS-CURRENT-BALANCE TO WS-DISPLAY-BALANCE
                   DISPLAY 'Account Holder: '
                       LS-HOLDER-FIRST-NAME ' '
                       LS-HOLDER-LAST-NAME
                   DISPLAY 'Current Balance: ' WS-DISPLAY-BALANCE
                   DISPLAY SPACES
                   IF LS-CURRENT-BALANCE NOT = 0
                       DISPLAY 'Balance must be $0 to close.'
                       DISPLAY 'Current balance will be returned.'
                   END-IF
                   DISPLAY 'Confirm close? (Y/N): '
                       WITH NO ADVANCING
                   ACCEPT WS-INPUT-CONFIRM
                   IF WS-INPUT-CONFIRM = 'Y' OR 'y'
                       MOVE 'C' TO LS-ACCT-STATUS
                       MOVE FUNCTION CURRENT-DATE(1:8)
                           TO LS-DATE-CLOSED
                       MOVE 0 TO LS-CURRENT-BALANCE
                       MOVE 0 TO LS-AVAILABLE-BALANCE
                       PERFORM 2100-UPDATE-FILE-RECORD
                       IF LS-ERROR-CODE = 0000
                           DISPLAY '*** Account closed. ***'
                       END-IF
                   ELSE
                       DISPLAY 'Close cancelled.'
                   END-IF
               END-IF
           END-IF
           .

       2100-UPDATE-FILE-RECORD.
           MOVE LS-ACCT-NUMBER     TO AF-ACCT-NUMBER
           MOVE LS-ACCT-TYPE       TO AF-ACCT-TYPE
           MOVE LS-ACCT-STATUS     TO AF-ACCT-STATUS
           MOVE LS-HOLDER-LAST-NAME TO AF-HOLDER-LAST-NAME
           MOVE LS-HOLDER-FIRST-NAME TO AF-HOLDER-FIRST-NAME
           MOVE LS-HOLDER-MI       TO AF-HOLDER-MI
           MOVE LS-HOLDER-SSN      TO AF-HOLDER-SSN
           MOVE LS-HOLDER-DOB      TO AF-HOLDER-DOB
           MOVE LS-HOLDER-PHONE    TO AF-HOLDER-PHONE
           MOVE LS-ADDR-STREET     TO AF-ADDR-STREET
           MOVE LS-ADDR-CITY       TO AF-ADDR-CITY
           MOVE LS-ADDR-STATE      TO AF-ADDR-STATE
           MOVE LS-ADDR-ZIP        TO AF-ADDR-ZIP
           MOVE LS-CURRENT-BALANCE TO AF-CURRENT-BALANCE
           MOVE LS-AVAILABLE-BALANCE TO AF-AVAILABLE-BALANCE
           MOVE LS-INTEREST-RATE   TO AF-INTEREST-RATE
           MOVE LS-ACCRUED-INTEREST TO AF-ACCRUED-INTEREST
           MOVE LS-OVERDRAFT-LIMIT TO AF-OVERDRAFT-LIMIT
           MOVE LS-MIN-BALANCE     TO AF-MIN-BALANCE
           MOVE LS-DATE-OPENED     TO AF-DATE-OPENED
           MOVE LS-DATE-CLOSED     TO AF-DATE-CLOSED
           MOVE LS-LAST-TRANS-DATE TO AF-LAST-TRANS-DATE
           MOVE LS-LAST-STMT-DATE  TO AF-LAST-STMT-DATE
           MOVE LS-MONTHLY-TRANS-CT TO AF-MONTHLY-TRANS-CT
           MOVE LS-YTD-DEPOSITS    TO AF-YTD-DEPOSITS
           MOVE LS-YTD-WITHDRAWALS TO AF-YTD-WITHDRAWALS
           MOVE SPACES             TO AF-FILLER

           REWRITE ACCOUNT-FILE-RECORD
           IF NOT FILE-OK
               MOVE 9001 TO LS-ERROR-CODE
               STRING 'Rewrite error: status='
                   DELIMITED SIZE
                   WS-FILE-STATUS DELIMITED SIZE
                   INTO LS-ERROR-MESSAGE
               END-STRING
               MOVE 3 TO LS-ERROR-SEVERITY
           END-IF
           .

      *================================================================*
      * 3000 - ACCOUNT INQUIRY                                        *
      * Demonstrates: formatted display, reference modification,       *
      *               STRING for formatting SSN/phone, SEARCH ALL      *
      *================================================================*
       3000-ACCOUNT-INQUIRY.
           DISPLAY SPACES
           DISPLAY '=== ACCOUNT INQUIRY ==='
           DISPLAY 'Enter Account Number: ' WITH NO ADVANCING
           ACCEPT WS-INPUT-ACCT-NUM

           PERFORM 3100-READ-ACCOUNT

           IF LS-ERROR-CODE = 0000
               PERFORM 3200-DISPLAY-ACCOUNT-DETAIL
           END-IF
           .

       3100-READ-ACCOUNT.
           MOVE WS-INPUT-ACCT-NUM TO AF-ACCT-NUMBER

           READ ACCOUNT-FILE INTO ACCOUNT-FILE-RECORD
               KEY IS AF-ACCT-NUMBER
           END-READ

           IF FILE-OK
               MOVE AF-ACCT-NUMBER     TO LS-ACCT-NUMBER
               MOVE AF-ACCT-TYPE       TO LS-ACCT-TYPE
               MOVE AF-ACCT-STATUS     TO LS-ACCT-STATUS
               MOVE AF-HOLDER-LAST-NAME TO LS-HOLDER-LAST-NAME
               MOVE AF-HOLDER-FIRST-NAME TO LS-HOLDER-FIRST-NAME
               MOVE AF-HOLDER-MI       TO LS-HOLDER-MI
               MOVE AF-HOLDER-SSN      TO LS-HOLDER-SSN
               MOVE AF-HOLDER-DOB      TO LS-HOLDER-DOB
               MOVE AF-HOLDER-PHONE    TO LS-HOLDER-PHONE
               MOVE AF-ADDR-STREET     TO LS-ADDR-STREET
               MOVE AF-ADDR-CITY       TO LS-ADDR-CITY
               MOVE AF-ADDR-STATE      TO LS-ADDR-STATE
               MOVE AF-ADDR-ZIP        TO LS-ADDR-ZIP
               MOVE AF-CURRENT-BALANCE TO LS-CURRENT-BALANCE
               MOVE AF-AVAILABLE-BALANCE TO LS-AVAILABLE-BALANCE
               MOVE AF-INTEREST-RATE   TO LS-INTEREST-RATE
               MOVE AF-ACCRUED-INTEREST TO LS-ACCRUED-INTEREST
               MOVE AF-OVERDRAFT-LIMIT TO LS-OVERDRAFT-LIMIT
               MOVE AF-MIN-BALANCE     TO LS-MIN-BALANCE
               MOVE AF-DATE-OPENED     TO LS-DATE-OPENED
               MOVE AF-DATE-CLOSED     TO LS-DATE-CLOSED
               MOVE AF-LAST-TRANS-DATE TO LS-LAST-TRANS-DATE
               MOVE AF-LAST-STMT-DATE  TO LS-LAST-STMT-DATE
               MOVE AF-MONTHLY-TRANS-CT TO LS-MONTHLY-TRANS-CT
               MOVE AF-YTD-DEPOSITS    TO LS-YTD-DEPOSITS
               MOVE AF-YTD-WITHDRAWALS TO LS-YTD-WITHDRAWALS
           ELSE
               IF FILE-NOT-FOUND
                   MOVE 1001 TO LS-ERROR-CODE
                   MOVE 'Account not found'
                       TO LS-ERROR-MESSAGE
                   MOVE 2 TO LS-ERROR-SEVERITY
               ELSE
                   MOVE 9001 TO LS-ERROR-CODE
                   STRING 'Read error: status='
                       DELIMITED SIZE
                       WS-FILE-STATUS DELIMITED SIZE
                       INTO LS-ERROR-MESSAGE
                   END-STRING
                   MOVE 3 TO LS-ERROR-SEVERITY
               END-IF
           END-IF
           .

       3200-DISPLAY-ACCOUNT-DETAIL.
           DISPLAY SPACES
           DISPLAY WS-SEPARATOR
           DISPLAY '       ACCOUNT DETAILS'
           DISPLAY WS-SEPARATOR
           DISPLAY 'Account Number:  ' LS-ACCT-NUMBER

           SET WS-TYPE-IDX TO 1
           SEARCH WS-ACCT-TYPE-ENTRY
               AT END
                   DISPLAY 'Account Type:    Unknown'
               WHEN WS-TYPE-CODE(WS-TYPE-IDX) = LS-ACCT-TYPE
                   DISPLAY 'Account Type:    '
                       WS-TYPE-DESC(WS-TYPE-IDX)
           END-SEARCH

           EVALUATE LS-ACCT-STATUS
               WHEN 'A' DISPLAY 'Status:          Active'
               WHEN 'C' DISPLAY 'Status:          CLOSED'
               WHEN 'F' DISPLAY 'Status:          FROZEN'
           END-EVALUATE

           DISPLAY WS-SEPARATOR
           DISPLAY 'HOLDER INFORMATION'
           DISPLAY 'Name:            '
               LS-HOLDER-FIRST-NAME ' '
               LS-HOLDER-MI '. '
               LS-HOLDER-LAST-NAME

      *    Format SSN as XXX-XX-XXXX using reference modification
           INITIALIZE WS-DISPLAY-SSN
           STRING LS-HOLDER-SSN(1:3) DELIMITED SIZE
                  '-'                 DELIMITED SIZE
                  LS-HOLDER-SSN(4:2) DELIMITED SIZE
                  '-'                 DELIMITED SIZE
                  LS-HOLDER-SSN(6:4) DELIMITED SIZE
               INTO WS-DISPLAY-SSN
           END-STRING
           DISPLAY 'SSN:             ' WS-DISPLAY-SSN

      *    Format phone as (XXX) XXX-XXXX
           INITIALIZE WS-DISPLAY-PHONE
           STRING '('                    DELIMITED SIZE
                  LS-HOLDER-PHONE(1:3)  DELIMITED SIZE
                  ') '                   DELIMITED SIZE
                  LS-HOLDER-PHONE(4:3)  DELIMITED SIZE
                  '-'                    DELIMITED SIZE
                  LS-HOLDER-PHONE(7:4)  DELIMITED SIZE
               INTO WS-DISPLAY-PHONE
           END-STRING
           DISPLAY 'Phone:           ' WS-DISPLAY-PHONE

           DISPLAY WS-SEPARATOR
           DISPLAY 'ADDRESS'
           DISPLAY '  ' LS-ADDR-STREET
           DISPLAY '  ' LS-ADDR-CITY ', '
               LS-ADDR-STATE ' ' LS-ADDR-ZIP

           DISPLAY WS-SEPARATOR
           DISPLAY 'FINANCIAL SUMMARY'

           MOVE LS-CURRENT-BALANCE TO WS-DISPLAY-BALANCE
           DISPLAY 'Current Balance:   ' WS-DISPLAY-BALANCE

           MOVE LS-AVAILABLE-BALANCE TO WS-DISPLAY-BALANCE
           DISPLAY 'Available Balance: ' WS-DISPLAY-BALANCE

           MOVE LS-INTEREST-RATE TO WS-DISPLAY-RATE
           DISPLAY 'Interest Rate:     ' WS-DISPLAY-RATE

           MOVE LS-ACCRUED-INTEREST TO WS-DISPLAY-BALANCE
           DISPLAY 'Accrued Interest:  ' WS-DISPLAY-BALANCE

           MOVE LS-OVERDRAFT-LIMIT TO WS-DISPLAY-BALANCE
           DISPLAY 'Overdraft Limit:   ' WS-DISPLAY-BALANCE

           MOVE LS-MIN-BALANCE TO WS-DISPLAY-BALANCE
           DISPLAY 'Minimum Balance:   ' WS-DISPLAY-BALANCE

           DISPLAY WS-SEPARATOR
           DISPLAY 'ACCOUNT DATES'

           PERFORM 3300-FORMAT-DATE-OPENED
           DISPLAY 'Date Opened:       ' WS-DISPLAY-DATE

           DISPLAY 'Last Transaction:  ' LS-LAST-TRANS-DATE

           DISPLAY WS-SEPARATOR
           DISPLAY 'YTD ACTIVITY'
           MOVE LS-YTD-DEPOSITS TO WS-DISPLAY-BALANCE
           DISPLAY 'YTD Deposits:      ' WS-DISPLAY-BALANCE
           MOVE LS-YTD-WITHDRAWALS TO WS-DISPLAY-BALANCE
           DISPLAY 'YTD Withdrawals:   ' WS-DISPLAY-BALANCE
           DISPLAY 'Monthly Trans:     ' LS-MONTHLY-TRANS-CT
           DISPLAY WS-SEPARATOR
           .

       3300-FORMAT-DATE-OPENED.
           MOVE LS-DATE-OPENED TO WS-RAW-DATE
           IF WS-RAW-DATE > 0
               STRING WS-RAW-DATE(1:4)  DELIMITED SIZE
                      '-'                DELIMITED SIZE
                      WS-RAW-DATE(5:2)  DELIMITED SIZE
                      '-'                DELIMITED SIZE
                      WS-RAW-DATE(7:2)  DELIMITED SIZE
                   INTO WS-DISPLAY-DATE
               END-STRING
           ELSE
               MOVE 'N/A       ' TO WS-DISPLAY-DATE
           END-IF
           .

      *================================================================*
      * 4000 - UPDATE ACCOUNT INFO                                     *
      *================================================================*
       4000-UPDATE-ACCOUNT.
           DISPLAY SPACES
           DISPLAY '=== UPDATE ACCOUNT ==='
           DISPLAY 'Enter Account Number: ' WITH NO ADVANCING
           ACCEPT WS-INPUT-ACCT-NUM

           PERFORM 3100-READ-ACCOUNT

           IF LS-ERROR-CODE = 0000
               IF LS-ACCT-STATUS = 'C'
                   MOVE 1002 TO LS-ERROR-CODE
                   MOVE 'Cannot update a closed account'
                       TO LS-ERROR-MESSAGE
                   MOVE 2 TO LS-ERROR-SEVERITY
               ELSE
                   DISPLAY 'Current name: '
                       LS-HOLDER-FIRST-NAME ' '
                       LS-HOLDER-LAST-NAME
                   DISPLAY 'New Phone (or ENTER to skip): '
                       WITH NO ADVANCING
                   ACCEPT LS-HOLDER-PHONE
                   DISPLAY 'New Street (or ENTER to skip): '
                       WITH NO ADVANCING
                   ACCEPT LS-ADDR-STREET

                   PERFORM 2100-UPDATE-FILE-RECORD

                   IF LS-ERROR-CODE = 0000
                       DISPLAY '*** Account updated. ***'
                   END-IF
               END-IF
           END-IF
           .

      *================================================================*
      * 5000 - FREEZE/UNFREEZE ACCOUNT                                *
      *================================================================*
       5000-FREEZE-ACCOUNT.
           DISPLAY SPACES
           DISPLAY '=== FREEZE/UNFREEZE ACCOUNT ==='
           DISPLAY 'Enter Account Number: ' WITH NO ADVANCING
           ACCEPT WS-INPUT-ACCT-NUM

           PERFORM 3100-READ-ACCOUNT

           IF LS-ERROR-CODE = 0000
               IF LS-ACCT-STATUS = 'C'
                   MOVE 1002 TO LS-ERROR-CODE
                   MOVE 'Cannot freeze a closed account'
                       TO LS-ERROR-MESSAGE
                   MOVE 2 TO LS-ERROR-SEVERITY
               ELSE
                   IF LS-ACCT-STATUS = 'F'
                       DISPLAY 'Account is FROZEN.'
                       DISPLAY 'Unfreeze? (Y/N): '
                           WITH NO ADVANCING
                       ACCEPT WS-INPUT-CONFIRM
                       IF WS-INPUT-CONFIRM = 'Y' OR 'y'
                           MOVE 'A' TO LS-ACCT-STATUS
                           MOVE LS-CURRENT-BALANCE
                               TO LS-AVAILABLE-BALANCE
                           PERFORM 2100-UPDATE-FILE-RECORD
                           IF LS-ERROR-CODE = 0000
                               DISPLAY '*** Account unfrozen. ***'
                           END-IF
                       END-IF
                   ELSE
                       DISPLAY 'Account is ACTIVE.'
                       DISPLAY 'Freeze? (Y/N): '
                           WITH NO ADVANCING
                       ACCEPT WS-INPUT-CONFIRM
                       IF WS-INPUT-CONFIRM = 'Y' OR 'y'
                           MOVE 'F' TO LS-ACCT-STATUS
                           MOVE 0 TO LS-AVAILABLE-BALANCE
                           PERFORM 2100-UPDATE-FILE-RECORD
                           IF LS-ERROR-CODE = 0000
                               DISPLAY '*** Account frozen. ***'
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           .
