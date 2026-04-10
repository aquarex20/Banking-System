      *================================================================*
      * TRANREC.cpy - Transaction Record Layout                        *
      * Shared copybook for the Banking System                         *
      *================================================================*

       01  WS-TRANSACTION-RECORD.
           05  WS-TRANS-ID             PIC 9(12).
           05  WS-TRANS-DATE           PIC 9(8).
           05  WS-TRANS-TIME           PIC 9(6).
           05  WS-TRANS-TYPE           PIC X(2).
               88  TRANS-DEPOSIT       VALUE 'DP'.
               88  TRANS-WITHDRAWAL    VALUE 'WD'.
               88  TRANS-TRANSFER-OUT  VALUE 'TO'.
               88  TRANS-TRANSFER-IN   VALUE 'TI'.
               88  TRANS-INTEREST      VALUE 'IN'.
               88  TRANS-FEE           VALUE 'FE'.
               88  TRANS-ADJUSTMENT    VALUE 'AJ'.
               88  TRANS-VALID-TYPE    VALUE 'DP' 'WD' 'TO' 'TI'
                                             'IN' 'FE' 'AJ'.
           05  WS-TRANS-ACCT-FROM      PIC 9(10).
           05  WS-TRANS-ACCT-TO        PIC 9(10).
           05  WS-TRANS-AMOUNT         PIC S9(11)V99.
           05  WS-TRANS-BALANCE-AFTER  PIC S9(11)V99.
           05  WS-TRANS-STATUS         PIC X(1).
               88  TRANS-APPROVED      VALUE 'A'.
               88  TRANS-DECLINED      VALUE 'D'.
               88  TRANS-PENDING       VALUE 'P'.
               88  TRANS-REVERSED      VALUE 'R'.
           05  WS-TRANS-DESCRIPTION    PIC X(40).
           05  WS-TRANS-REF-NUMBER     PIC X(15).
           05  WS-TRANS-TELLER-ID      PIC X(6).
           05  FILLER                   PIC X(10).
