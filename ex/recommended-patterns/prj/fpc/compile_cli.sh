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
# The remote client only needs the service interfaces and DTOs. Domain,
# infrastructure and service implementations are on the search path in local
# mode only, where the client runs the services itself - so a remote build
# fails as soon as the client reaches into the domain.
SERVER_PATHS=""
if [ "$1" = "local" ]; then
    DEFINE_FLAG="-dLOCAL_MODE"
    MODE="LOCAL"
    SERVER_PATHS="-Fusrc/dom/tasks -Fusrc/dom/tags -Fusrc/infra/tasks -Fusrc/infra/tags -Fusrc/app/tasks/implementation -Fusrc/app/tags/implementation"
fi

echo "Compiling cli_client ($MODE mode)..."

fpc src/cli_client.pas \
    -obin/cli_client \
    -FUbin/units \
    -Fl"$ROOT_DIR/../../static/x86_64-linux" \
    -O1 -Mobjfpc \
    -Fi../../src \
    -Fusrc \
    -Fusrc/app/tasks/interface \
    -Fusrc/app/tags/interface \
    $SERVER_PATHS \
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
