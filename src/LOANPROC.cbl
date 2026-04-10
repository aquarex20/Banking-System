      *================================================================*
      * Program:    LOANPROC                                           *
      * Purpose:    Loan Processing Subprogram                         *
      * Concepts:   Complex COMPUTE with exponentiation (**),          *
      *             ROUNDED, ON SIZE ERROR, iteration for              *
      *             amortization schedule, PERFORM VARYING,            *
      *             table processing, formatted report output,         *
      *             intrinsic functions (ANNUITY), nested IF,          *
      *             arithmetic precision with V (implied decimal)      *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOANPROC.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOAN-FILE
               ASSIGN TO 'data/LOANS.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS LF-LOAN-NUMBER
               FILE STATUS IS WS-LOAN-FILE-STATUS.

       DATA DIVISION.

       FILE SECTION.
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

       WORKING-STORAGE SECTION.

       01  WS-LOAN-FILE-STATUS         PIC X(2).
       01  WS-LOAN-FILE-OPEN           PIC X VALUE 'N'.

      *----------------------------------------------------------------*
      * Loan calculation working fields                                *
      *----------------------------------------------------------------*
       01  WS-CALC-FIELDS.
           05  WS-MONTHLY-RATE         PIC 9V9(8).
           05  WS-NUM-PAYMENTS         PIC 9(3).
           05  WS-POWER-FACTOR         PIC 9(6)V9(8).
           05  WS-PMT-NUMERATOR        PIC S9(13)V9(4).
           05  WS-PMT-DENOMINATOR      PIC S9(13)V9(4).
           05  WS-CALCULATED-PMT       PIC S9(9)V99.
           05  WS-TOTAL-COST           PIC S9(13)V99.
           05  WS-TOTAL-INTEREST       PIC S9(13)V99.

      *----------------------------------------------------------------*
      * Amortization schedule table (max 360 months = 30 years)        *
      *----------------------------------------------------------------*
       01  WS-AMORT-TABLE.
           05  WS-AMORT-ENTRY OCCURS 360 TIMES
               INDEXED BY WS-AMORT-IDX.
               10  WS-AMORT-PMT-NUM    PIC 9(3).
               10  WS-AMORT-PAYMENT    PIC S9(9)V99.
               10  WS-AMORT-PRINCIPAL  PIC S9(9)V99.
               10  WS-AMORT-INTEREST   PIC S9(9)V99.
               10  WS-AMORT-BALANCE    PIC S9(11)V99.

      *----------------------------------------------------------------*
      * Running totals for amortization                                *
      *----------------------------------------------------------------*
       01  WS-RUNNING-TOTALS.
           05  WS-RUN-BALANCE          PIC S9(11)V99.
           05  WS-RUN-INTEREST         PIC S9(9)V99.
           05  WS-RUN-PRINCIPAL        PIC S9(9)V99.
           05  WS-RUN-TOTAL-INT        PIC S9(11)V99.
           05  WS-RUN-TOTAL-PRIN       PIC S9(11)V99.

      *----------------------------------------------------------------*
      * Loan rate table based on type and credit tier                  *
      *----------------------------------------------------------------*
       01  WS-RATE-TABLE.
           05  FILLER PIC X(7) VALUE 'P10.049'.
           05  FILLER PIC X(7) VALUE 'P20.069'.
           05  FILLER PIC X(7) VALUE 'P30.099'.
           05  FILLER PIC X(7) VALUE 'M10.035'.
           05  FILLER PIC X(7) VALUE 'M20.045'.
           05  FILLER PIC X(7) VALUE 'M30.065'.
           05  FILLER PIC X(7) VALUE 'A10.039'.
           05  FILLER PIC X(7) VALUE 'A20.055'.
           05  FILLER PIC X(7) VALUE 'A30.079'.
           05  FILLER PIC X(7) VALUE 'B10.059'.
           05  FILLER PIC X(7) VALUE 'B20.079'.
           05  FILLER PIC X(7) VALUE 'B30.109'.
       01  WS-RATE-TABLE-R REDEFINES WS-RATE-TABLE.
           05  WS-RATE-ENTRY OCCURS 12 TIMES
               INDEXED BY WS-RATE-IDX.
               10  WS-RATE-LOAN-TYPE   PIC X(1).
               10  WS-RATE-CREDIT-TIER PIC 9.
               10  WS-RATE-VALUE       PIC 9V9(3).

      *----------------------------------------------------------------*
      * Input and display fields                                       *
      *----------------------------------------------------------------*
       01  WS-INPUT-FIELDS.
           05  WS-INPUT-LOAN-NUM       PIC 9(10).
           05  WS-INPUT-ACCT-NUM       PIC 9(10).
           05  WS-INPUT-LOAN-TYPE      PIC X(1).
           05  WS-INPUT-PRINCIPAL      PIC S9(11)V99.
           05  WS-INPUT-TERM           PIC 9(3).
           05  WS-INPUT-CREDIT-TIER    PIC 9.
           05  WS-INPUT-CONFIRM        PIC X.
           05  WS-INPUT-PMT-AMOUNT     PIC S9(9)V99.

       01  WS-DISPLAY-FIELDS.
           05  WS-DISP-AMOUNT          PIC $ZZZ,ZZZ,ZZ9.99-.
           05  WS-DISP-BALANCE         PIC $ZZZ,ZZZ,ZZ9.99-.
           05  WS-DISP-RATE            PIC Z9.9999.
           05  WS-DISP-PAYMENT         PIC $ZZ,ZZ9.99.
           05  WS-DISP-INT             PIC $ZZ,ZZ9.99.
           05  WS-DISP-PRIN            PIC $ZZ,ZZ9.99.

       01  WS-SEPARATOR                PIC X(70) VALUE ALL '-'.
       01  WS-NEXT-LOAN-NUMBER         PIC 9(10) VALUE 5000000001.

      *================================================================*
      * LINKAGE SECTION                                                *
      *================================================================*
       LINKAGE SECTION.

       01  LS-FUNCTION-CODE           PIC X(2).

       01  LS-LOAN-RECORD.
           05  LS-LOAN-NUMBER          PIC 9(10).
           05  LS-LOAN-ACCT-NUMBER     PIC 9(10).
           05  LS-LOAN-TYPE            PIC X(1).
           05  LS-LOAN-STATUS          PIC X(1).
           05  LS-LOAN-PRINCIPAL       PIC S9(11)V99.
           05  LS-LOAN-CURRENT-BAL     PIC S9(11)V99.
           05  LS-LOAN-INT-RATE        PIC 9V9(4).
           05  LS-LOAN-TERM-MONTHS     PIC 9(3).
           05  LS-LOAN-MONTHLY-PMT     PIC S9(9)V99.
           05  LS-LOAN-TOTAL-PAID      PIC S9(11)V99.
           05  LS-LOAN-TOTAL-INTEREST  PIC S9(11)V99.
           05  LS-LOAN-PAYMENTS-MADE   PIC 9(3).
           05  LS-LOAN-PAYMENTS-LEFT   PIC 9(3).
           05  LS-LOAN-START-DATE      PIC 9(8).
           05  LS-LOAN-END-DATE        PIC 9(8).
           05  LS-LOAN-LAST-PMT-DATE   PIC 9(8).
           05  LS-LOAN-NEXT-PMT-DATE   PIC 9(8).
           05  LS-LOAN-LATE-FEES       PIC S9(7)V99.
           05  FILLER                   PIC X(20).

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

       PROCEDURE DIVISION USING
           LS-FUNCTION-CODE
           LS-LOAN-RECORD
           LS-ACCOUNT-RECORD
           LS-ERROR-HANDLING.

       0000-MAIN-ENTRY.
           MOVE 0000 TO LS-ERROR-CODE
           MOVE SPACES TO LS-ERROR-MESSAGE
           MOVE 'LOANPROC' TO LS-ERROR-MODULE

           PERFORM 0100-OPEN-FILE

           EVALUATE LS-FUNCTION-CODE
               WHEN 'AP'
                   PERFORM 1000-LOAN-APPLICATION
               WHEN 'PM'
                   PERFORM 2000-LOAN-PAYMENT
               WHEN 'IQ'
                   PERFORM 3000-LOAN-INQUIRY
               WHEN 'AM'
                   PERFORM 4000-AMORTIZATION-SCHEDULE
               WHEN 'PO'
                   PERFORM 5000-PAYOFF-QUOTE
               WHEN OTHER
                   MOVE 9002 TO LS-ERROR-CODE
                   MOVE 'Invalid loan function code'
                       TO LS-ERROR-MESSAGE
                   MOVE 2 TO LS-ERROR-SEVERITY
           END-EVALUATE

           PERFORM 0200-CLOSE-FILE
           GOBACK
           .

      *================================================================*
      * FILE OPERATIONS                                                *
      *================================================================*
       0100-OPEN-FILE.
           OPEN I-O LOAN-FILE
           IF WS-LOAN-FILE-STATUS = '00'
               MOVE 'Y' TO WS-LOAN-FILE-OPEN
           ELSE
               IF WS-LOAN-FILE-STATUS = '35'
                   OPEN OUTPUT LOAN-FILE
                   CLOSE LOAN-FILE
                   OPEN I-O LOAN-FILE
                   IF WS-LOAN-FILE-STATUS = '00'
                       MOVE 'Y' TO WS-LOAN-FILE-OPEN
                   END-IF
               END-IF
           END-IF
           .

       0200-CLOSE-FILE.
           IF WS-LOAN-FILE-OPEN = 'Y'
               CLOSE LOAN-FILE
               MOVE 'N' TO WS-LOAN-FILE-OPEN
           END-IF
           .

      *================================================================*
      * 1000 - LOAN APPLICATION                                       *
      * Demonstrates: Complex COMPUTE, rate table lookup, SEARCH,     *
      *               monthly payment formula with exponentiation,     *
      *               ON SIZE ERROR, ROUNDED                           *
      *================================================================*
       1000-LOAN-APPLICATION.
           DISPLAY SPACES
           DISPLAY '=========================================='
           DISPLAY '          LOAN APPLICATION'
           DISPLAY '=========================================='
           DISPLAY SPACES

           DISPLAY 'Loan Types:'
           DISPLAY '  P - Personal Loan'
           DISPLAY '  M - Mortgage'
           DISPLAY '  A - Auto Loan'
           DISPLAY '  B - Business Loan'
           DISPLAY 'Select Type: ' WITH NO ADVANCING
           ACCEPT WS-INPUT-LOAN-TYPE

           INSPECT WS-INPUT-LOAN-TYPE
               CONVERTING 'pmab' TO 'PMAB'

           IF WS-INPUT-LOAN-TYPE NOT = 'P' AND
              WS-INPUT-LOAN-TYPE NOT = 'M' AND
              WS-INPUT-LOAN-TYPE NOT = 'A' AND
              WS-INPUT-LOAN-TYPE NOT = 'B'
               MOVE 9002 TO LS-ERROR-CODE
               MOVE 'Invalid loan type' TO LS-ERROR-MESSAGE
               MOVE 2 TO LS-ERROR-SEVERITY
           ELSE
               DISPLAY 'Linked Account Number: '
                   WITH NO ADVANCING
               ACCEPT WS-INPUT-ACCT-NUM

               DISPLAY 'Loan Amount: $' WITH NO ADVANCING
               ACCEPT WS-INPUT-PRINCIPAL

               DISPLAY 'Term (months, max 360): '
                   WITH NO ADVANCING
               ACCEPT WS-INPUT-TERM

               IF WS-INPUT-TERM < 1 OR WS-INPUT-TERM > 360
                   MOVE 9002 TO LS-ERROR-CODE
                   MOVE 'Term must be 1-360 months'
                       TO LS-ERROR-MESSAGE
                   MOVE 2 TO LS-ERROR-SEVERITY
               ELSE
                   DISPLAY 'Credit Tier (1=Excellent,'
                       ' 2=Good, 3=Fair): '
                       WITH NO ADVANCING
                   ACCEPT WS-INPUT-CREDIT-TIER

                   PERFORM 1100-LOOKUP-RATE
                   PERFORM 1200-CALCULATE-PAYMENT

                   IF LS-ERROR-CODE = 0000
                       PERFORM 1300-DISPLAY-TERMS
                       DISPLAY SPACES
                       DISPLAY 'Approve loan? (Y/N): '
                           WITH NO ADVANCING
                       ACCEPT WS-INPUT-CONFIRM

                       IF WS-INPUT-CONFIRM = 'Y' OR 'y'
                           PERFORM 1400-CREATE-LOAN
                       ELSE
                           DISPLAY 'Loan application cancelled.'
                       END-IF
                   END-IF
               END-IF
           END-IF
           .

      *----------------------------------------------------------------*
      * Rate lookup using SEARCH on rate table                         *
      *----------------------------------------------------------------*
       1100-LOOKUP-RATE.
           SET WS-RATE-IDX TO 1

           SEARCH WS-RATE-ENTRY
               AT END
                   MOVE 0.0750 TO LS-LOAN-INT-RATE
                   DISPLAY 'Using default rate.'
               WHEN WS-RATE-LOAN-TYPE(WS-RATE-IDX) =
                       WS-INPUT-LOAN-TYPE
                   AND WS-RATE-CREDIT-TIER(WS-RATE-IDX) =
                       WS-INPUT-CREDIT-TIER
                   MOVE WS-RATE-VALUE(WS-RATE-IDX)
                       TO LS-LOAN-INT-RATE
           END-SEARCH
           .

      *----------------------------------------------------------------*
      * Monthly payment calculation                                    *
      * Formula: M = P * [r(1+r)^n] / [(1+r)^n - 1]                  *
      * Where: P=principal, r=monthly rate, n=number of payments       *
      *----------------------------------------------------------------*
       1200-CALCULATE-PAYMENT.
           COMPUTE WS-MONTHLY-RATE ROUNDED =
               LS-LOAN-INT-RATE / 12

           MOVE WS-INPUT-TERM TO WS-NUM-PAYMENTS

           IF WS-MONTHLY-RATE = 0
               COMPUTE WS-CALCULATED-PMT ROUNDED =
                   WS-INPUT-PRINCIPAL / WS-NUM-PAYMENTS
           ELSE
               COMPUTE WS-POWER-FACTOR =
                   (1 + WS-MONTHLY-RATE) ** WS-NUM-PAYMENTS
                   ON SIZE ERROR
                       MOVE 3002 TO LS-ERROR-CODE
                       MOVE 'Payment calculation overflow'
                           TO LS-ERROR-MESSAGE
                       MOVE 2 TO LS-ERROR-SEVERITY
               END-COMPUTE

               IF LS-ERROR-CODE = 0000
                   COMPUTE WS-PMT-NUMERATOR =
                       WS-INPUT-PRINCIPAL * WS-MONTHLY-RATE
                       * WS-POWER-FACTOR

                   COMPUTE WS-PMT-DENOMINATOR =
                       WS-POWER-FACTOR - 1

                   IF WS-PMT-DENOMINATOR = 0
                       MOVE 3002 TO LS-ERROR-CODE
                       MOVE 'Division by zero in payment calc'
                           TO LS-ERROR-MESSAGE
                       MOVE 2 TO LS-ERROR-SEVERITY
                   ELSE
                       COMPUTE WS-CALCULATED-PMT ROUNDED =
                           WS-PMT-NUMERATOR /
                           WS-PMT-DENOMINATOR
                           ON SIZE ERROR
                               MOVE 3002 TO LS-ERROR-CODE
                               MOVE 'Payment overflow'
                                   TO LS-ERROR-MESSAGE
                               MOVE 2 TO LS-ERROR-SEVERITY
                       END-COMPUTE
                   END-IF
               END-IF
           END-IF

           IF LS-ERROR-CODE = 0000
               COMPUTE WS-TOTAL-COST ROUNDED =
                   WS-CALCULATED-PMT * WS-NUM-PAYMENTS
               COMPUTE WS-TOTAL-INTEREST =
                   WS-TOTAL-COST - WS-INPUT-PRINCIPAL
           END-IF
           .

      *----------------------------------------------------------------*
      * Display proposed loan terms                                    *
      *----------------------------------------------------------------*
       1300-DISPLAY-TERMS.
           DISPLAY SPACES
           DISPLAY WS-SEPARATOR
           DISPLAY '        PROPOSED LOAN TERMS'
           DISPLAY WS-SEPARATOR

           EVALUATE WS-INPUT-LOAN-TYPE
               WHEN 'P' DISPLAY 'Loan Type:       Personal'
               WHEN 'M' DISPLAY 'Loan Type:       Mortgage'
               WHEN 'A' DISPLAY 'Loan Type:       Auto'
               WHEN 'B' DISPLAY 'Loan Type:       Business'
           END-EVALUATE

           MOVE WS-INPUT-PRINCIPAL TO WS-DISP-AMOUNT
           DISPLAY 'Principal:       ' WS-DISP-AMOUNT

           COMPUTE WS-DISP-RATE = LS-LOAN-INT-RATE * 100
           DISPLAY 'Annual Rate:     ' WS-DISP-RATE

           DISPLAY 'Term:            ' WS-INPUT-TERM ' months'

           MOVE WS-CALCULATED-PMT TO WS-DISP-PAYMENT
           DISPLAY 'Monthly Payment: ' WS-DISP-PAYMENT

           MOVE WS-TOTAL-COST TO WS-DISP-AMOUNT
           DISPLAY 'Total Cost:      ' WS-DISP-AMOUNT

           MOVE WS-TOTAL-INTEREST TO WS-DISP-AMOUNT
           DISPLAY 'Total Interest:  ' WS-DISP-AMOUNT
           DISPLAY WS-SEPARATOR
           .

      *----------------------------------------------------------------*
      * Create loan record and write to file                           *
      *----------------------------------------------------------------*
       1400-CREATE-LOAN.
           ADD 1 TO WS-NEXT-LOAN-NUMBER
           MOVE WS-NEXT-LOAN-NUMBER TO LS-LOAN-NUMBER
           MOVE WS-INPUT-ACCT-NUM TO LS-LOAN-ACCT-NUMBER
           MOVE WS-INPUT-LOAN-TYPE TO LS-LOAN-TYPE
           MOVE 'A' TO LS-LOAN-STATUS
           MOVE WS-INPUT-PRINCIPAL TO LS-LOAN-PRINCIPAL
           MOVE WS-INPUT-PRINCIPAL TO LS-LOAN-CURRENT-BAL
           MOVE WS-INPUT-TERM TO LS-LOAN-TERM-MONTHS
           MOVE WS-CALCULATED-PMT TO LS-LOAN-MONTHLY-PMT
           MOVE 0 TO LS-LOAN-TOTAL-PAID
           MOVE 0 TO LS-LOAN-TOTAL-INTEREST
           MOVE 0 TO LS-LOAN-PAYMENTS-MADE
           MOVE WS-INPUT-TERM TO LS-LOAN-PAYMENTS-LEFT
           MOVE FUNCTION CURRENT-DATE(1:8)
               TO LS-LOAN-START-DATE
           MOVE 0 TO LS-LOAN-END-DATE
           MOVE 0 TO LS-LOAN-LAST-PMT-DATE
           MOVE 0 TO LS-LOAN-NEXT-PMT-DATE
           MOVE 0 TO LS-LOAN-LATE-FEES

           PERFORM 1410-WRITE-LOAN-FILE

           IF LS-ERROR-CODE = 0000
               DISPLAY SPACES
               DISPLAY '*** LOAN APPROVED ***'
               DISPLAY 'Loan Number: ' LS-LOAN-NUMBER
               DISPLAY 'Linked Account: ' LS-LOAN-ACCT-NUMBER
           END-IF
           .

       1410-WRITE-LOAN-FILE.
           MOVE LS-LOAN-NUMBER       TO LF-LOAN-NUMBER
           MOVE LS-LOAN-ACCT-NUMBER  TO LF-LOAN-ACCT-NUMBER
           MOVE LS-LOAN-TYPE         TO LF-LOAN-TYPE
           MOVE LS-LOAN-STATUS       TO LF-LOAN-STATUS
           MOVE LS-LOAN-PRINCIPAL    TO LF-LOAN-PRINCIPAL
           MOVE LS-LOAN-CURRENT-BAL  TO LF-LOAN-CURRENT-BAL
           MOVE LS-LOAN-INT-RATE     TO LF-LOAN-INT-RATE
           MOVE LS-LOAN-TERM-MONTHS  TO LF-LOAN-TERM-MONTHS
           MOVE LS-LOAN-MONTHLY-PMT  TO LF-LOAN-MONTHLY-PMT
           MOVE LS-LOAN-TOTAL-PAID   TO LF-LOAN-TOTAL-PAID
           MOVE LS-LOAN-TOTAL-INTEREST
                                     TO LF-LOAN-TOTAL-INTEREST
           MOVE LS-LOAN-PAYMENTS-MADE
                                     TO LF-LOAN-PAYMENTS-MADE
           MOVE LS-LOAN-PAYMENTS-LEFT
                                     TO LF-LOAN-PAYMENTS-LEFT
           MOVE LS-LOAN-START-DATE   TO LF-LOAN-START-DATE
           MOVE LS-LOAN-END-DATE     TO LF-LOAN-END-DATE
           MOVE LS-LOAN-LAST-PMT-DATE
                                     TO LF-LOAN-LAST-PMT-DATE
           MOVE LS-LOAN-NEXT-PMT-DATE
                                     TO LF-LOAN-NEXT-PMT-DATE
           MOVE LS-LOAN-LATE-FEES    TO LF-LOAN-LATE-FEES
           MOVE SPACES               TO LF-FILLER

           WRITE LOAN-FILE-RECORD
           IF WS-LOAN-FILE-STATUS NOT = '00'
               MOVE 9001 TO LS-ERROR-CODE
               STRING 'Loan write error: '
                   DELIMITED SIZE
                   WS-LOAN-FILE-STATUS DELIMITED SIZE
                   INTO LS-ERROR-MESSAGE
               END-STRING
               MOVE 3 TO LS-ERROR-SEVERITY
           END-IF
           .

      *================================================================*
      * 2000 - LOAN PAYMENT                                           *
      * Demonstrates: interest/principal split calculation,            *
      *               SUBTRACT, multiple COMPUTE with ROUNDED          *
      *================================================================*
       2000-LOAN-PAYMENT.
           DISPLAY SPACES
           DISPLAY '=== LOAN PAYMENT ==='
           DISPLAY 'Loan Number: ' WITH NO ADVANCING
           ACCEPT WS-INPUT-LOAN-NUM

           PERFORM 2100-READ-LOAN

           IF LS-ERROR-CODE = 0000
               IF LS-LOAN-STATUS NOT = 'A'
                   MOVE 3001 TO LS-ERROR-CODE
                   MOVE 'Loan is not active'
                       TO LS-ERROR-MESSAGE
                   MOVE 2 TO LS-ERROR-SEVERITY
               ELSE
                   MOVE LS-LOAN-CURRENT-BAL TO WS-DISP-BALANCE
                   DISPLAY 'Remaining Balance: ' WS-DISP-BALANCE
                   MOVE LS-LOAN-MONTHLY-PMT TO WS-DISP-PAYMENT
                   DISPLAY 'Monthly Payment:   ' WS-DISP-PAYMENT
                   DISPLAY 'Payments Left:     '
                       LS-LOAN-PAYMENTS-LEFT
                   DISPLAY SPACES
                   DISPLAY 'Payment Amount (0 for standard): $'
                       WITH NO ADVANCING
                   ACCEPT WS-INPUT-PMT-AMOUNT

                   IF WS-INPUT-PMT-AMOUNT = 0
                       MOVE LS-LOAN-MONTHLY-PMT
                           TO WS-INPUT-PMT-AMOUNT
                   END-IF

                   PERFORM 2200-APPLY-PAYMENT
               END-IF
           END-IF
           .

       2100-READ-LOAN.
           MOVE WS-INPUT-LOAN-NUM TO LF-LOAN-NUMBER
           READ LOAN-FILE
               KEY IS LF-LOAN-NUMBER
           END-READ

           IF WS-LOAN-FILE-STATUS = '00'
               MOVE LF-LOAN-NUMBER       TO LS-LOAN-NUMBER
               MOVE LF-LOAN-ACCT-NUMBER  TO LS-LOAN-ACCT-NUMBER
               MOVE LF-LOAN-TYPE         TO LS-LOAN-TYPE
               MOVE LF-LOAN-STATUS       TO LS-LOAN-STATUS
               MOVE LF-LOAN-PRINCIPAL    TO LS-LOAN-PRINCIPAL
               MOVE LF-LOAN-CURRENT-BAL  TO LS-LOAN-CURRENT-BAL
               MOVE LF-LOAN-INT-RATE     TO LS-LOAN-INT-RATE
               MOVE LF-LOAN-TERM-MONTHS  TO LS-LOAN-TERM-MONTHS
               MOVE LF-LOAN-MONTHLY-PMT  TO LS-LOAN-MONTHLY-PMT
               MOVE LF-LOAN-TOTAL-PAID   TO LS-LOAN-TOTAL-PAID
               MOVE LF-LOAN-TOTAL-INTEREST
                                         TO LS-LOAN-TOTAL-INTEREST
               MOVE LF-LOAN-PAYMENTS-MADE
                                         TO LS-LOAN-PAYMENTS-MADE
               MOVE LF-LOAN-PAYMENTS-LEFT
                                         TO LS-LOAN-PAYMENTS-LEFT
               MOVE LF-LOAN-START-DATE   TO LS-LOAN-START-DATE
               MOVE LF-LOAN-LAST-PMT-DATE
                                         TO LS-LOAN-LAST-PMT-DATE
               MOVE LF-LOAN-LATE-FEES    TO LS-LOAN-LATE-FEES
           ELSE
               IF WS-LOAN-FILE-STATUS = '23'
                   MOVE 3001 TO LS-ERROR-CODE
                   MOVE 'Loan not found'
                       TO LS-ERROR-MESSAGE
                   MOVE 2 TO LS-ERROR-SEVERITY
               ELSE
                   MOVE 9001 TO LS-ERROR-CODE
                   STRING 'Loan read error: '
                       DELIMITED SIZE
                       WS-LOAN-FILE-STATUS DELIMITED SIZE
                       INTO LS-ERROR-MESSAGE
                   END-STRING
                   MOVE 3 TO LS-ERROR-SEVERITY
               END-IF
           END-IF
           .

      *----------------------------------------------------------------*
      * Apply payment - split between interest and principal           *
      *----------------------------------------------------------------*
       2200-APPLY-PAYMENT.
           COMPUTE WS-MONTHLY-RATE = LS-LOAN-INT-RATE / 12

           COMPUTE WS-RUN-INTEREST ROUNDED =
               LS-LOAN-CURRENT-BAL * WS-MONTHLY-RATE

           COMPUTE WS-RUN-PRINCIPAL ROUNDED =
               WS-INPUT-PMT-AMOUNT - WS-RUN-INTEREST

           IF WS-RUN-PRINCIPAL >= LS-LOAN-CURRENT-BAL
               MOVE LS-LOAN-CURRENT-BAL TO WS-RUN-PRINCIPAL
               COMPUTE WS-INPUT-PMT-AMOUNT =
                   WS-RUN-PRINCIPAL + WS-RUN-INTEREST
           END-IF

           SUBTRACT WS-RUN-PRINCIPAL FROM LS-LOAN-CURRENT-BAL
           ADD WS-INPUT-PMT-AMOUNT TO LS-LOAN-TOTAL-PAID
           ADD WS-RUN-INTEREST TO LS-LOAN-TOTAL-INTEREST
           ADD 1 TO LS-LOAN-PAYMENTS-MADE
           SUBTRACT 1 FROM LS-LOAN-PAYMENTS-LEFT
           MOVE FUNCTION CURRENT-DATE(1:8)
               TO LS-LOAN-LAST-PMT-DATE

           IF LS-LOAN-CURRENT-BAL <= 0
               MOVE 0 TO LS-LOAN-CURRENT-BAL
               MOVE 'P' TO LS-LOAN-STATUS
               MOVE FUNCTION CURRENT-DATE(1:8)
                   TO LS-LOAN-END-DATE
               MOVE 0 TO LS-LOAN-PAYMENTS-LEFT
           END-IF

           PERFORM 2300-UPDATE-LOAN-FILE

           IF LS-ERROR-CODE = 0000
               DISPLAY SPACES
               DISPLAY '*** PAYMENT APPLIED ***'
               MOVE WS-INPUT-PMT-AMOUNT TO WS-DISP-PAYMENT
               DISPLAY 'Payment Amount:  ' WS-DISP-PAYMENT
               MOVE WS-RUN-INTEREST TO WS-DISP-INT
               DISPLAY 'Interest Paid:   ' WS-DISP-INT
               MOVE WS-RUN-PRINCIPAL TO WS-DISP-PRIN
               DISPLAY 'Principal Paid:  ' WS-DISP-PRIN
               MOVE LS-LOAN-CURRENT-BAL TO WS-DISP-BALANCE
               DISPLAY 'Remaining Bal:   ' WS-DISP-BALANCE
               IF LS-LOAN-STATUS = 'P'
                   DISPLAY SPACES
                   DISPLAY '*** LOAN PAID IN FULL! ***'
               END-IF
           END-IF
           .

       2300-UPDATE-LOAN-FILE.
           MOVE LS-LOAN-NUMBER       TO LF-LOAN-NUMBER
           MOVE LS-LOAN-ACCT-NUMBER  TO LF-LOAN-ACCT-NUMBER
           MOVE LS-LOAN-TYPE         TO LF-LOAN-TYPE
           MOVE LS-LOAN-STATUS       TO LF-LOAN-STATUS
           MOVE LS-LOAN-PRINCIPAL    TO LF-LOAN-PRINCIPAL
           MOVE LS-LOAN-CURRENT-BAL  TO LF-LOAN-CURRENT-BAL
           MOVE LS-LOAN-INT-RATE     TO LF-LOAN-INT-RATE
           MOVE LS-LOAN-TERM-MONTHS  TO LF-LOAN-TERM-MONTHS
           MOVE LS-LOAN-MONTHLY-PMT  TO LF-LOAN-MONTHLY-PMT
           MOVE LS-LOAN-TOTAL-PAID   TO LF-LOAN-TOTAL-PAID
           MOVE LS-LOAN-TOTAL-INTEREST
                                     TO LF-LOAN-TOTAL-INTEREST
           MOVE LS-LOAN-PAYMENTS-MADE
                                     TO LF-LOAN-PAYMENTS-MADE
           MOVE LS-LOAN-PAYMENTS-LEFT
                                     TO LF-LOAN-PAYMENTS-LEFT
           MOVE LS-LOAN-START-DATE   TO LF-LOAN-START-DATE
           MOVE LS-LOAN-END-DATE     TO LF-LOAN-END-DATE
           MOVE LS-LOAN-LAST-PMT-DATE
                                     TO LF-LOAN-LAST-PMT-DATE
           MOVE LS-LOAN-NEXT-PMT-DATE
                                     TO LF-LOAN-NEXT-PMT-DATE
           MOVE LS-LOAN-LATE-FEES    TO LF-LOAN-LATE-FEES
           MOVE SPACES               TO LF-FILLER

           REWRITE LOAN-FILE-RECORD
           IF WS-LOAN-FILE-STATUS NOT = '00'
               MOVE 9001 TO LS-ERROR-CODE
               STRING 'Loan update error: '
                   DELIMITED SIZE
                   WS-LOAN-FILE-STATUS DELIMITED SIZE
                   INTO LS-ERROR-MESSAGE
               END-STRING
               MOVE 3 TO LS-ERROR-SEVERITY
           END-IF
           .

      *================================================================*
      * 3000 - LOAN INQUIRY                                           *
      *================================================================*
       3000-LOAN-INQUIRY.
           DISPLAY SPACES
           DISPLAY '=== LOAN INQUIRY ==='
           DISPLAY 'Loan Number: ' WITH NO ADVANCING
           ACCEPT WS-INPUT-LOAN-NUM

           PERFORM 2100-READ-LOAN

           IF LS-ERROR-CODE = 0000
               DISPLAY SPACES
               DISPLAY WS-SEPARATOR
               DISPLAY '          LOAN DETAILS'
               DISPLAY WS-SEPARATOR
               DISPLAY 'Loan Number:     ' LS-LOAN-NUMBER
               DISPLAY 'Linked Account:  ' LS-LOAN-ACCT-NUMBER

               EVALUATE LS-LOAN-TYPE
                   WHEN 'P' DISPLAY 'Type:            Personal'
                   WHEN 'M' DISPLAY 'Type:            Mortgage'
                   WHEN 'A' DISPLAY 'Type:            Auto'
                   WHEN 'B' DISPLAY 'Type:            Business'
               END-EVALUATE

               EVALUATE LS-LOAN-STATUS
                   WHEN 'A' DISPLAY 'Status:          Active'
                   WHEN 'P' DISPLAY 'Status:          Paid Off'
                   WHEN 'D' DISPLAY 'Status:          Defaulted'
                   WHEN 'V' DISPLAY 'Status:          Approved'
               END-EVALUATE

               DISPLAY WS-SEPARATOR

               MOVE LS-LOAN-PRINCIPAL TO WS-DISP-AMOUNT
               DISPLAY 'Original Principal:  ' WS-DISP-AMOUNT

               MOVE LS-LOAN-CURRENT-BAL TO WS-DISP-BALANCE
               DISPLAY 'Current Balance:     ' WS-DISP-BALANCE

               COMPUTE WS-DISP-RATE = LS-LOAN-INT-RATE * 100
               DISPLAY 'Interest Rate:       ' WS-DISP-RATE

               DISPLAY 'Term:                '
                   LS-LOAN-TERM-MONTHS ' months'

               MOVE LS-LOAN-MONTHLY-PMT TO WS-DISP-PAYMENT
               DISPLAY 'Monthly Payment:     ' WS-DISP-PAYMENT

               DISPLAY WS-SEPARATOR
               DISPLAY 'Payments Made:       '
                   LS-LOAN-PAYMENTS-MADE
               DISPLAY 'Payments Left:       '
                   LS-LOAN-PAYMENTS-LEFT

               MOVE LS-LOAN-TOTAL-PAID TO WS-DISP-AMOUNT
               DISPLAY 'Total Paid:          ' WS-DISP-AMOUNT

               MOVE LS-LOAN-TOTAL-INTEREST TO WS-DISP-AMOUNT
               DISPLAY 'Total Interest Paid: ' WS-DISP-AMOUNT

               MOVE LS-LOAN-LATE-FEES TO WS-DISP-AMOUNT
               DISPLAY 'Late Fees:           ' WS-DISP-AMOUNT

               DISPLAY WS-SEPARATOR
               DISPLAY 'Start Date:          ' LS-LOAN-START-DATE
               DISPLAY 'Last Payment:        '
                   LS-LOAN-LAST-PMT-DATE
               DISPLAY WS-SEPARATOR
           END-IF
           .

      *================================================================*
      * 4000 - AMORTIZATION SCHEDULE                                  *
      * Demonstrates: Complex iterative calculation with PERFORM       *
      *               VARYING, table building, formatted report output *
      *================================================================*
       4000-AMORTIZATION-SCHEDULE.
           DISPLAY SPACES
           DISPLAY '=== AMORTIZATION SCHEDULE ==='
           DISPLAY 'Loan Number: ' WITH NO ADVANCING
           ACCEPT WS-INPUT-LOAN-NUM

           PERFORM 2100-READ-LOAN

           IF LS-ERROR-CODE = 0000
               PERFORM 4100-BUILD-AMORT-TABLE
               PERFORM 4200-DISPLAY-AMORT-TABLE
           END-IF
           .

      *----------------------------------------------------------------*
      * Build complete amortization table                              *
      *----------------------------------------------------------------*
       4100-BUILD-AMORT-TABLE.
           COMPUTE WS-MONTHLY-RATE = LS-LOAN-INT-RATE / 12
           MOVE LS-LOAN-PRINCIPAL TO WS-RUN-BALANCE
           MOVE 0 TO WS-RUN-TOTAL-INT
           MOVE 0 TO WS-RUN-TOTAL-PRIN

           PERFORM VARYING WS-AMORT-IDX
               FROM 1 BY 1
               UNTIL WS-AMORT-IDX > LS-LOAN-TERM-MONTHS
                   OR WS-RUN-BALANCE <= 0

               MOVE WS-AMORT-IDX
                   TO WS-AMORT-PMT-NUM(WS-AMORT-IDX)

               COMPUTE WS-RUN-INTEREST ROUNDED =
                   WS-RUN-BALANCE * WS-MONTHLY-RATE

               COMPUTE WS-RUN-PRINCIPAL =
                   LS-LOAN-MONTHLY-PMT - WS-RUN-INTEREST

               IF WS-RUN-PRINCIPAL > WS-RUN-BALANCE
                   MOVE WS-RUN-BALANCE TO WS-RUN-PRINCIPAL
                   COMPUTE WS-AMORT-PAYMENT(WS-AMORT-IDX) =
                       WS-RUN-PRINCIPAL + WS-RUN-INTEREST
               ELSE
                   MOVE LS-LOAN-MONTHLY-PMT
                       TO WS-AMORT-PAYMENT(WS-AMORT-IDX)
               END-IF

               MOVE WS-RUN-INTEREST
                   TO WS-AMORT-INTEREST(WS-AMORT-IDX)
               MOVE WS-RUN-PRINCIPAL
                   TO WS-AMORT-PRINCIPAL(WS-AMORT-IDX)

               SUBTRACT WS-RUN-PRINCIPAL FROM WS-RUN-BALANCE
               MOVE WS-RUN-BALANCE
                   TO WS-AMORT-BALANCE(WS-AMORT-IDX)

               ADD WS-RUN-INTEREST TO WS-RUN-TOTAL-INT
               ADD WS-RUN-PRINCIPAL TO WS-RUN-TOTAL-PRIN
           END-PERFORM
           .

      *----------------------------------------------------------------*
      * Display amortization table with formatted columns              *
      *----------------------------------------------------------------*
       4200-DISPLAY-AMORT-TABLE.
           DISPLAY SPACES
           DISPLAY WS-SEPARATOR
           DISPLAY '  AMORTIZATION SCHEDULE - Loan '
               LS-LOAN-NUMBER
           DISPLAY WS-SEPARATOR
           DISPLAY ' Pmt#   Payment     Principal'
               '    Interest     Balance'
           DISPLAY WS-SEPARATOR

           PERFORM VARYING WS-AMORT-IDX
               FROM 1 BY 1
               UNTIL WS-AMORT-IDX > LS-LOAN-TERM-MONTHS
                   OR WS-AMORT-BALANCE(WS-AMORT-IDX) < 0

               MOVE WS-AMORT-PAYMENT(WS-AMORT-IDX)
                   TO WS-DISP-PAYMENT
               MOVE WS-AMORT-PRINCIPAL(WS-AMORT-IDX)
                   TO WS-DISP-PRIN
               MOVE WS-AMORT-INTEREST(WS-AMORT-IDX)
                   TO WS-DISP-INT
               MOVE WS-AMORT-BALANCE(WS-AMORT-IDX)
                   TO WS-DISP-BALANCE

               DISPLAY ' '
                   WS-AMORT-PMT-NUM(WS-AMORT-IDX)
                   '   ' WS-DISP-PAYMENT
                   '   ' WS-DISP-PRIN
                   '   ' WS-DISP-INT
                   '   ' WS-DISP-BALANCE
           END-PERFORM

           DISPLAY WS-SEPARATOR
           MOVE WS-RUN-TOTAL-PRIN TO WS-DISP-AMOUNT
           DISPLAY 'Total Principal: ' WS-DISP-AMOUNT
           MOVE WS-RUN-TOTAL-INT TO WS-DISP-AMOUNT
           DISPLAY 'Total Interest:  ' WS-DISP-AMOUNT
           COMPUTE WS-TOTAL-COST =
               WS-RUN-TOTAL-PRIN + WS-RUN-TOTAL-INT
           MOVE WS-TOTAL-COST TO WS-DISP-AMOUNT
           DISPLAY 'Total Cost:      ' WS-DISP-AMOUNT
           DISPLAY WS-SEPARATOR
           .

      *================================================================*
      * 5000 - PAYOFF QUOTE                                           *
      * Demonstrates: date arithmetic, penalty calculation             *
      *================================================================*
       5000-PAYOFF-QUOTE.
           DISPLAY SPACES
           DISPLAY '=== PAYOFF QUOTE ==='
           DISPLAY 'Loan Number: ' WITH NO ADVANCING
           ACCEPT WS-INPUT-LOAN-NUM

           PERFORM 2100-READ-LOAN

           IF LS-ERROR-CODE = 0000
               IF LS-LOAN-STATUS NOT = 'A'
                   DISPLAY 'Loan is not active.'
               ELSE
                   COMPUTE WS-MONTHLY-RATE =
                       LS-LOAN-INT-RATE / 12

                   COMPUTE WS-RUN-INTEREST ROUNDED =
                       LS-LOAN-CURRENT-BAL * WS-MONTHLY-RATE

                   DISPLAY SPACES
                   DISPLAY WS-SEPARATOR
                   DISPLAY '       PAYOFF QUOTE'
                   DISPLAY WS-SEPARATOR
                   DISPLAY 'Loan Number:       ' LS-LOAN-NUMBER

                   MOVE LS-LOAN-CURRENT-BAL TO WS-DISP-BALANCE
                   DISPLAY 'Principal Balance: ' WS-DISP-BALANCE

                   MOVE WS-RUN-INTEREST TO WS-DISP-INT
                   DISPLAY 'Accrued Interest:  ' WS-DISP-INT

                   MOVE LS-LOAN-LATE-FEES TO WS-DISP-AMOUNT
                   DISPLAY 'Outstanding Fees:  ' WS-DISP-AMOUNT

                   COMPUTE WS-TOTAL-COST =
                       LS-LOAN-CURRENT-BAL
                       + WS-RUN-INTEREST
                       + LS-LOAN-LATE-FEES
                   MOVE WS-TOTAL-COST TO WS-DISP-AMOUNT
                   DISPLAY WS-SEPARATOR
                   DISPLAY 'TOTAL PAYOFF:      ' WS-DISP-AMOUNT
                   DISPLAY WS-SEPARATOR
                   DISPLAY 'Quote valid for 10 business days.'
               END-IF
           END-IF
           .
