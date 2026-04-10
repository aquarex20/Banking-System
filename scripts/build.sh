#!/bin/bash
#================================================================
# build.sh - Build script for COBOL Banking System
# Compiles all modules and links into executable
#================================================================

set -e

PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
SRC_DIR="$PROJECT_ROOT/src"
COPY_DIR="$PROJECT_ROOT/copybooks"
DATA_DIR="$PROJECT_ROOT/data"
BIN_DIR="$PROJECT_ROOT/bin"

echo "============================================"
echo "  COBOL Banking System - Build Script"
echo "============================================"
echo ""

# Check for GnuCOBOL
if ! command -v cobc &> /dev/null; then
    echo "ERROR: GnuCOBOL (cobc) not found."
    echo ""
    echo "Install with:"
    echo "  macOS:   brew install gnucobol"
    echo "  Ubuntu:  sudo apt install gnucobol"
    echo "  Fedora:  sudo dnf install gnucobol"
    echo ""
    exit 1
fi

echo "Using: $(cobc --version | head -1)"
echo ""

# Create directories
mkdir -p "$BIN_DIR"
mkdir -p "$DATA_DIR"

COBC_FLAGS="-x -std=cobol2014 -I $COPY_DIR"

echo "[1/5] Compiling ACCTMGMT (Account Management)..."
cobc -m -std=cobol2014 -I "$COPY_DIR" "$SRC_DIR/ACCTMGMT.cbl" -o "$BIN_DIR/ACCTMGMT.so"

echo "[2/5] Compiling TRANPROC (Transaction Processing)..."
cobc -m -std=cobol2014 -I "$COPY_DIR" "$SRC_DIR/TRANPROC.cbl" -o "$BIN_DIR/TRANPROC.so"

echo "[3/5] Compiling LOANPROC (Loan Processing)..."
cobc -m -std=cobol2014 -I "$COPY_DIR" "$SRC_DIR/LOANPROC.cbl" -o "$BIN_DIR/LOANPROC.so"

echo "[4/5] Compiling RPTGEN (Report Generation)..."
cobc -m -std=cobol2014 -I "$COPY_DIR" "$SRC_DIR/RPTGEN.cbl" -o "$BIN_DIR/RPTGEN.so"

echo "[5/5] Compiling & linking BANKMAIN (Main Program)..."
cobc $COBC_FLAGS "$SRC_DIR/BANKMAIN.cbl" -o "$BIN_DIR/BANKMAIN"

echo ""
echo "============================================"
echo "  Build completed successfully!"
echo "============================================"
echo ""
echo "Executables in: $BIN_DIR/"
ls -la "$BIN_DIR/"
echo ""
echo "Run with:"
echo "  cd $PROJECT_ROOT && COB_LIBRARY_PATH=$BIN_DIR $BIN_DIR/BANKMAIN"
echo ""
