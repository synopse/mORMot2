#!/bin/bash
# CLI Client - Compilation Script
# Usage:
#   ./prj/fpc/compile_cli.sh         # Remote mode (HTTP client)
#   ./prj/fpc/compile_cli.sh local   # Local mode (embedded SQLite)

# Determine the project root (two levels above this script)
ROOT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/../.." && pwd )"
cd "$ROOT_DIR"

mkdir -p bin
mkdir -p bin/units

if ! command -v fpc &> /dev/null; then
    echo "Error: Free Pascal Compiler (fpc) is not installed"
    exit 1
fi

DEFINE_FLAG=""
MODE="REMOTE"
if [ "$1" = "local" ]; then
    DEFINE_FLAG="-dLOCAL_MODE"
    MODE="LOCAL"
fi

echo "Compiling cli_client ($MODE mode)..."

fpc src/cli_client.pas \
    -obin/cli_client \
    -FUbin/units \
    -Fl"$ROOT_DIR/../../static/x86_64-linux" \
    -O1 -Mobjfpc \
    -Fi../../src \
    -Fusrc \
    -Fusrc/dom/tasks \
    -Fusrc/dom/tags \
    -Fusrc/infra/tasks \
    -Fusrc/infra/tags \
    -Fusrc/app/tasks \
    -Fusrc/app/tags \
    -Fu../../src/core \
    -Fu../../src/lib \
    -Fu../../src/crypt \
    -Fu../../src/orm \
    -Fu../../src/rest \
    -Fu../../src/db \
    -Fu../../src/net \
    -Fu../../src/soa \
    -Fu../../src/app \
    $DEFINE_FLAG

if [ $? -eq 0 ]; then
    echo ""
    echo "Compilation successful! Binary: bin/cli_client ($MODE mode)"
    echo ""
    echo "Usage:"
    echo "  ./bin/cli_client list [status]"
    echo "  ./bin/cli_client add <title> <description> <priority>"
    echo "  ./bin/cli_client get <id>"
    echo "  ./bin/cli_client complete <id>"
    echo "  ./bin/cli_client delete <id>"
    echo "  ./bin/cli_client search <term>"
    echo "  ./bin/cli_client comment <task_id> <content> <author>"
    echo "  ./bin/cli_client help"
else
    echo ""
    echo "Compilation failed!"
    exit 1
fi
