#!/bin/bash
#================================================================
# clean.sh - Clean build artifacts and data files
#================================================================

PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"

echo "Cleaning build artifacts..."
rm -rf "$PROJECT_ROOT/bin"

echo "Clean data files too? (y/N): "
read -r answer
if [ "$answer" = "y" ] || [ "$answer" = "Y" ]; then
    rm -f "$PROJECT_ROOT/data/"*.dat
    rm -f "$PROJECT_ROOT/data/"*.txt
    echo "Data files removed."
fi

echo "Clean complete."
