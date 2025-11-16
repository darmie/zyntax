#!/bin/bash
set -e

ZYNTAX=/Users/amaterasu/Vibranium/zyntax/target/release/zyntax

echo "========================================"
echo "   Zyntax ZBC Pipeline - Test Suite"
echo "========================================"
echo ""

cd "$(dirname "$0")"

# Test 1: SimpleTrace
echo "TEST 1: SimpleTrace"
echo "----------------------------------------"
haxe test/build_simple_trace.hxml 2>&1 | tail -1
$ZYNTAX compile output/SimpleTrace.zbc --format bytecode --run 2>&1
echo ""

# Test 2: SimpleArithmetic
echo "TEST 2: SimpleArithmetic"
echo "----------------------------------------"
haxe test/build_simple_arithmetic.hxml 2>&1 | tail -1
$ZYNTAX compile output/SimpleArithmetic.zbc --format bytecode --run 2>&1
echo ""

# Test 3: ControlFlowTest
echo "TEST 3: ControlFlowTest"
echo "----------------------------------------"
haxe test/build_control_flow.hxml 2>&1 | tail -1
$ZYNTAX compile output/ControlFlowTest.zbc --format bytecode --run 2>&1
echo ""

# Test 4: ProofOfExecution
echo "TEST 4: ProofOfExecution"
echo "----------------------------------------"
haxe test/build_proof.hxml 2>&1 | tail -1
$ZYNTAX compile output/ProofOfExecution.zbc --format bytecode --run 2>&1
echo ""

echo "========================================"
echo "   All Tests Complete!"
echo "========================================"
