# COBOL Banking System

A comprehensive banking system written in COBOL demonstrating fundamental through advanced language concepts. The system features modular architecture with a main controller calling four subprograms for account management, transaction processing, loan handling, and report generation.

## Architecture

```
BANKMAIN (Main Controller)
├── ACCTMGMT  - Account Management (open, close, inquiry, update, freeze)
├── TRANPROC  - Transaction Processing (deposit, withdrawal, transfer, history)
├── LOANPROC  - Loan Processing (application, payment, amortization, payoff)
└── RPTGEN    - Report Generation (statements, summaries, portfolio reports)
```

## Project Structure

```
.
├── src/                    # COBOL source programs
│   ├── BANKMAIN.cbl       # Main menu-driven controller
│   ├── ACCTMGMT.cbl       # Account management subprogram
│   ├── TRANPROC.cbl       # Transaction processing subprogram
│   ├── LOANPROC.cbl       # Loan processing subprogram
│   └── RPTGEN.cbl         # Report generation subprogram
├── copybooks/              # Shared data structure definitions
│   ├── ACCTREC.cpy        # Account master record layout
│   ├── TRANREC.cpy        # Transaction record layout
│   ├── LOANREC.cpy        # Loan record layout
│   └── ERRHAND.cpy        # Error handling structures
├── data/                   # Runtime data files (created automatically)
├── scripts/                # Build and run scripts
│   ├── build.sh           # Compile all programs
│   ├── run.sh             # Build (if needed) and run
│   └── clean.sh           # Remove build artifacts
└── README.md
```

## Prerequisites

**GnuCOBOL** must be installed:

```bash
# macOS
brew install gnucobol

# Ubuntu/Debian
sudo apt install gnucobol

# Fedora
sudo dnf install gnucobol
```

## Build & Run

```bash
# Build everything
./scripts/build.sh

# Run the banking system
./scripts/run.sh

# Clean build artifacts
./scripts/clean.sh
```

## COBOL Concepts Demonstrated

### Fundamentals
| Concept | Where |
|---|---|
| **IDENTIFICATION / ENVIRONMENT / DATA / PROCEDURE divisions** | All programs |
| **PIC clauses** (numeric, alphanumeric, edited) | All copybooks and programs |
| **MOVE, ADD, SUBTRACT, MULTIPLY, DIVIDE, COMPUTE** | TRANPROC, LOANPROC |
| **DISPLAY / ACCEPT** (screen I/O) | BANKMAIN, all subprograms |
| **IF / ELSE / END-IF** (nested conditionals) | TRANPROC withdrawal logic |
| **EVALUATE** (COBOL's CASE/SWITCH) | BANKMAIN menu routing |
| **PERFORM** (paragraphs, THRU, VARYING, UNTIL) | All programs |
| **88-level condition names** | All copybooks, BANKMAIN |
| **Working-Storage vs Linkage sections** | ACCTMGMT, TRANPROC, LOANPROC |
| **COPY** (copybooks / shared includes) | BANKMAIN includes all 4 copybooks |

### Intermediate
| Concept | Where |
|---|---|
| **Subprogram CALL with USING** | BANKMAIN calling all 4 modules |
| **LINKAGE SECTION / PROCEDURE DIVISION USING** | ACCTMGMT, TRANPROC, LOANPROC, RPTGEN |
| **GOBACK** (subprogram return) | All subprograms |
| **Indexed file I/O** (OPEN, READ, WRITE, REWRITE, START, READ NEXT) | ACCTMGMT, TRANPROC, LOANPROC, RPTGEN |
| **Sequential file I/O** | TRANPROC (transaction log), RPTGEN |
| **FILE STATUS** checking | All file-handling programs |
| **STRING / UNSTRING** | BANKMAIN (date/time formatting), ACCTMGMT (SSN/phone formatting) |
| **INSPECT** (REPLACING, CONVERTING, TALLYING) | BANKMAIN (teller ID), ACCTMGMT (case conversion) |
| **Reference modification** `field(start:length)` | BANKMAIN, ACCTMGMT, TRANPROC |
| **REDEFINES** | BANKMAIN (date structure), ACCTMGMT (branch/type tables) |
| **Tables / arrays** (OCCURS, INDEXED BY) | BANKMAIN (session log), ACCTMGMT (branch table), LOANPROC (amortization) |
| **SEARCH** (linear search on tables) | ACCTMGMT (type lookup), LOANPROC (rate lookup) |

### Advanced
| Concept | Where |
|---|---|
| **ON SIZE ERROR / NOT ON SIZE ERROR** | TRANPROC (deposit), LOANPROC (payment calc) |
| **COMPUTE with ROUNDED** | TRANPROC (overdraft), LOANPROC (payment formula) |
| **Exponentiation** (`**` operator) | LOANPROC (amortization power factor) |
| **Intrinsic functions** (CURRENT-DATE, RANDOM) | BANKMAIN (session ID, timestamps) |
| **FUNCTION ALL INTRINSIC** (repository) | BANKMAIN |
| **PERFORM VARYING with tables** (building/traversing) | LOANPROC (amortization schedule), RPTGEN |
| **Multi-file processing** | TRANPROC (accounts + transactions), RPTGEN (all 3 files) |
| **Dynamic file access mode** | ACCTMGMT, TRANPROC, RPTGEN (random + sequential on same file) |
| **START/READ NEXT** (sequential traversal of indexed file) | RPTGEN (portfolio reports) |
| **Control break processing** | RPTGEN (transaction summary by type) |
| **Accumulators and summary statistics** | RPTGEN (portfolio: min/max/avg/totals) |
| **Two-phase commit pattern** | TRANPROC (transfer: debit source, credit destination) |
| **Checksum / Luhn-like algorithm** | ACCTMGMT (account number generation) |
| **Rate table lookup with compound key** | LOANPROC (loan type + credit tier) |
| **Amortization schedule generation** | LOANPROC (iterative interest/principal split) |
| **Report-writer style formatted output** | RPTGEN (columnar reports, headers, footers) |

## Banking Features

### Account Management
- Open new accounts (Checking, Savings, Money Market) with minimum balance enforcement
- Close accounts with balance validation
- Full account inquiry with formatted SSN, phone, and address display
- Update account contact information
- Freeze/unfreeze accounts (blocks transactions)

### Transaction Processing
- **Deposits** with balance overflow protection
- **Withdrawals** with overdraft protection, daily limits, and below-minimum fees
- **Transfers** between accounts with two-phase update
- **Balance inquiry** with available balance display
- **Transaction history** loaded from sequential log file

### Loan Processing
- **Loan application** with rate lookup by type and credit tier
- **Monthly payment calculation** using the standard amortization formula
- **Loan payments** with interest/principal split tracking
- **Full amortization schedule** generation (up to 30-year terms)
- **Payoff quotes** with accrued interest and fee calculation

### Reports
- Account statements with full transaction history
- Daily transaction summary with type/status breakdowns
- Account portfolio summary (totals, averages, min/max balances)
- Loan portfolio report with outstanding balance tracking
- Active/overdue loans listing
