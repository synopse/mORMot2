
#!/bin/bash
# mORMot2 Task Manager - Run Script

echo "Starting mORMot2 Task Manager Server..."
echo "======================================="
echo ""

# Determine the project root (two levels above this script)
ROOT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/../.." && pwd )"
cd "$ROOT_DIR"

if [ ! -f "bin/task_manager" ]; then
    echo "Error: task_manager binary not found"
    echo "Please run ./src/fpc/compile.sh first"
    exit 1
fi

# Create data directory if it doesn't exist
mkdir -p data

# Run the server
./bin/task_manager
