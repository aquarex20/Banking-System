#!/bin/bash
#================================================================
# run.sh - Run the COBOL Banking System
#================================================================

PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BIN_DIR="$PROJECT_ROOT/bin"

if [ ! -f "$BIN_DIR/BANKMAIN" ]; then
    echo "Banking system not built yet. Running build first..."
    bash "$PROJECT_ROOT/scripts/build.sh"
fi

cd "$PROJECT_ROOT"
export COB_LIBRARY_PATH="$BIN_DIR"
"$BIN_DIR/BANKMAIN"
