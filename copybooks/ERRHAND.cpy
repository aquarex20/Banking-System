      *================================================================*
      * ERRHAND.cpy - Error Handling Shared Structures                 *
      * Shared copybook for the Banking System                         *
      *================================================================*

       01  WS-ERROR-HANDLING.
           05  WS-ERROR-CODE           PIC 9(4).
               88  ERR-NONE            VALUE 0000.
               88  ERR-ACCT-NOT-FOUND  VALUE 1001.
               88  ERR-ACCT-CLOSED     VALUE 1002.
               88  ERR-ACCT-FROZEN     VALUE 1003.
               88  ERR-INSUF-FUNDS     VALUE 2001.
               88  ERR-OVER-LIMIT      VALUE 2002.
               88  ERR-INVALID-AMT     VALUE 2003.
               88  ERR-TRANS-FAILED    VALUE 2004.
               88  ERR-LOAN-NOT-FOUND  VALUE 3001.
               88  ERR-LOAN-DENIED     VALUE 3002.
               88  ERR-FILE-ERROR      VALUE 9001.
               88  ERR-INVALID-INPUT   VALUE 9002.
               88  ERR-SYSTEM          VALUE 9999.
           05  WS-ERROR-MESSAGE        PIC X(60).
           05  WS-ERROR-SEVERITY       PIC 9.
               88  SEV-INFO            VALUE 0.
               88  SEV-WARNING         VALUE 1.
               88  SEV-ERROR           VALUE 2.
               88  SEV-CRITICAL        VALUE 3.
           05  WS-ERROR-MODULE         PIC X(8).
           05  WS-ERROR-PARAGRAPH      PIC X(20).
