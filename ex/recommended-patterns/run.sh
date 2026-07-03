
#!/bin/bash
# mORMot2 Task Manager - Run Script

echo "Starting mORMot2 Task Manager Server..."
echo "======================================="
echo ""

if [ ! -f "bin/task_manager" ]; then
    echo "Error: task_manager binary not found"
    echo "Please run ./compile.sh first"
    exit 1
fi

# Create data directory if it doesn't exist
mkdir -p data

# Run the server
./bin/task_manager
