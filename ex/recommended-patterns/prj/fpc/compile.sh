
#!/bin/bash
# mORMot2 Task Manager - Compilation Script

echo "mORMot2 Task Manager - Compilation Script"
echo "=========================================="
echo ""

# Determine the project root (two levels above this script)
ROOT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/../.." && pwd )"
cd "$ROOT_DIR"

mkdir -p bin
mkdir -p bin/units

# Check if FPC is installed
if ! command -v fpc &> /dev/null; then
    echo "Error: Free Pascal Compiler (fpc) is not installed"
    exit 1
fi

echo "Compiling task_manager..."
fpc src/task_manager.pas \
    -obin/task_manager \
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
    -Fusrc/serv/app \
    -Futests/tasks \
    -Futests/tags \
    -Fu../../src/core \
    -Fu../../src/lib \
    -Fu../../src/crypt \
    -Fu../../src/orm \
    -Fu../../src/rest \
    -Fu../../src/db \
    -Fu../../src/net \
    -Fu../../src/soa \
    -Fu../../src/app

if [ $? -eq 0 ]; then
    echo ""
    echo "Compilation successful!"
    echo "Binary: bin/task_manager"
    echo ""
    echo "To run the server:"
    echo "  cd $ROOT_DIR && ./bin/task_manager"
    echo ""
    echo "The server will be available at:"
    echo "  http://localhost:8080"
    echo ""
    echo "CQRS Services:"
    echo "  http://localhost:8080/taskmanager/TaskQuery"
    echo "  http://localhost:8080/taskmanager/TaskCommand"
    echo "  http://localhost:8080/taskmanager/TagQuery"
    echo "  http://localhost:8080/taskmanager/TagCommand"
else
    echo ""
    echo "Compilation failed!"
    exit 1
fi
