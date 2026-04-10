      *================================================================*
      * ACCTREC.cpy - Account Master Record Layout                     *
      * Shared copybook for the Banking System                         *
      *================================================================*

       01  WS-ACCOUNT-RECORD.
           05  WS-ACCT-NUMBER          PIC 9(10).
           05  WS-ACCT-TYPE            PIC X(1).
               88  ACCT-CHECKING       VALUE 'C'.
               88  ACCT-SAVINGS        VALUE 'S'.
               88  ACCT-MONEY-MARKET   VALUE 'M'.
           05  WS-ACCT-STATUS          PIC X(1).
               88  ACCT-ACTIVE         VALUE 'A'.
               88  ACCT-CLOSED         VALUE 'C'.
               88  ACCT-FROZEN         VALUE 'F'.
               88  ACCT-VALID-STATUS   VALUE 'A' 'C' 'F'.
           05  WS-ACCT-HOLDER-INFO.
               10  WS-HOLDER-LAST-NAME   PIC X(25).
               10  WS-HOLDER-FIRST-NAME  PIC X(20).
               10  WS-HOLDER-MI          PIC X(1).
               10  WS-HOLDER-SSN         PIC 9(9).
               10  WS-HOLDER-DOB         PIC 9(8).
               10  WS-HOLDER-PHONE       PIC 9(10).
           05  WS-ACCT-ADDRESS.
               10  WS-ADDR-STREET       PIC X(30).
               10  WS-ADDR-CITY         PIC X(20).
               10  WS-ADDR-STATE        PIC X(2).
               10  WS-ADDR-ZIP          PIC 9(5).
           05  WS-ACCT-FINANCIALS.
               10  WS-CURRENT-BALANCE   PIC S9(11)V99.
               10  WS-AVAILABLE-BALANCE PIC S9(11)V99.
               10  WS-INTEREST-RATE     PIC 9V9(4).
               10  WS-ACCRUED-INTEREST  PIC S9(9)V99.
               10  WS-OVERDRAFT-LIMIT   PIC 9(7)V99.
               10  WS-MIN-BALANCE       PIC 9(7)V99.
           05  WS-ACCT-DATES.
               10  WS-DATE-OPENED       PIC 9(8).
               10  WS-DATE-CLOSED       PIC 9(8).
               10  WS-LAST-TRANS-DATE   PIC 9(8).
               10  WS-LAST-STMT-DATE    PIC 9(8).
           05  WS-ACCT-COUNTERS.
               10  WS-MONTHLY-TRANS-CT  PIC 9(5).
               10  WS-YTD-DEPOSITS      PIC S9(11)V99.
               10  WS-YTD-WITHDRAWALS   PIC S9(11)V99.
           05  FILLER                    PIC X(20).
