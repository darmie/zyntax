#!/bin/bash
set -e

echo "=== Zyntax Haxe Compilation Test ==="
echo ""

# Step 1: Compile Haxe to TypedAST JSON
echo "[1/3] Compiling Haxe to TypedAST JSON..."
haxe build.hxml

# Check if output was generated
if [ ! -d "output" ]; then
    echo "Error: No output directory generated"
    exit 1
fi

JSON_COUNT=$(find output -name "*.json" | wc -l)
echo "      Generated $JSON_COUNT JSON file(s)"
echo ""

# Step 2: Build Zyntax CLI (if needed)
echo "[2/3] Building Zyntax compiler..."
cd ../../
cargo build --package zyntax_cli --release
cd reflaxe.zyntax/test
echo ""

# Step 3: Compile TypedAST JSON to native code
echo "[3/3] Compiling to native code..."
../../../target/release/zyntax compile output/*.json -o hello --run

echo ""
echo "=== Compilation Complete ==="
