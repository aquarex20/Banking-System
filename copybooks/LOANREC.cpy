      *================================================================*
      * LOANREC.cpy - Loan Record Layout                               *
      * Shared copybook for the Banking System                         *
      *================================================================*

       01  WS-LOAN-RECORD.
           05  WS-LOAN-NUMBER          PIC 9(10).
           05  WS-LOAN-ACCT-NUMBER     PIC 9(10).
           05  WS-LOAN-TYPE            PIC X(1).
               88  LOAN-PERSONAL       VALUE 'P'.
               88  LOAN-MORTGAGE       VALUE 'M'.
               88  LOAN-AUTO           VALUE 'A'.
               88  LOAN-BUSINESS       VALUE 'B'.
           05  WS-LOAN-STATUS          PIC X(1).
               88  LOAN-ACTIVE         VALUE 'A'.
               88  LOAN-PAID-OFF       VALUE 'P'.
               88  LOAN-DEFAULTED      VALUE 'D'.
               88  LOAN-APPROVED       VALUE 'V'.
           05  WS-LOAN-PRINCIPAL       PIC S9(11)V99.
           05  WS-LOAN-CURRENT-BAL     PIC S9(11)V99.
           05  WS-LOAN-INT-RATE        PIC 9V9(4).
           05  WS-LOAN-TERM-MONTHS     PIC 9(3).
           05  WS-LOAN-MONTHLY-PMT     PIC S9(9)V99.
           05  WS-LOAN-TOTAL-PAID      PIC S9(11)V99.
           05  WS-LOAN-TOTAL-INTEREST  PIC S9(11)V99.
           05  WS-LOAN-PAYMENTS-MADE   PIC 9(3).
           05  WS-LOAN-PAYMENTS-LEFT   PIC 9(3).
           05  WS-LOAN-START-DATE      PIC 9(8).
           05  WS-LOAN-END-DATE        PIC 9(8).
           05  WS-LOAN-LAST-PMT-DATE   PIC 9(8).
           05  WS-LOAN-NEXT-PMT-DATE   PIC 9(8).
           05  WS-LOAN-LATE-FEES       PIC S9(7)V99.
           05  FILLER                   PIC X(20).
