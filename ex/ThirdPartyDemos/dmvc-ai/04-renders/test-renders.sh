#!/bin/bash

# Test script for RendersSample
# This script tests all the different render types

PORT=8080
BASE_URL="http://localhost:$PORT/RendersSample"

echo "mORMot2 Renders Sample - Test Script"
echo "====================================="
echo ""
echo "Make sure RendersSample.exe is running on port $PORT"
echo ""
read -p "Press Enter to continue..."
echo ""

# Test 1: Get single person (JSON object)
echo "Test 1: Get Person (JSON object)"
echo "---------------------------------"
curl -s "$BASE_URL/GetPerson?id=1" | python3 -m json.tool
echo ""
echo ""

# Test 2: Get all people (JSON array)
echo "Test 2: Get People (JSON array)"
echo "--------------------------------"
curl -s "$BASE_URL/GetPeople" | python3 -m json.tool
echo ""
echo ""

# Test 3: Get single customer (JSON object)
echo "Test 3: Get Customer (JSON object)"
echo "-----------------------------------"
curl -s "$BASE_URL/GetCustomer?id=1" | python3 -m json.tool
echo ""
echo ""

# Test 4: Get all customers (JSON array)
echo "Test 4: Get Customers (JSON array)"
echo "-----------------------------------"
curl -s "$BASE_URL/GetCustomers" | python3 -m json.tool
echo ""
echo ""

# Test 5: Get people with metadata
echo "Test 5: Get People with Metadata"
echo "----------------------------------"
curl -s "$BASE_URL/GetPeopleWithMetadata" | python3 -m json.tool
echo ""
echo ""

# Test 6: Get person as text (plain text response)
echo "Test 6: Get Person As Text (plain text)"
echo "-----------------------------------------"
curl -s "$BASE_URL/GetPersonAsText?id=1"
echo ""
echo ""

# Test 7: Get people as CSV
echo "Test 7: Get People As CSV"
echo "--------------------------"
curl -s "$BASE_URL/GetPeopleAsCSV"
echo ""
echo ""

# Test 8: Get simple arrays
echo "Test 8: Get Simple Arrays (JSON with arrays)"
echo "----------------------------------------------"
curl -s "$BASE_URL/GetSimpleArrays" | python3 -m json.tool
echo ""
echo ""

echo "All tests completed!"
echo ""
echo "Manual tests you can run:"
echo "  curl $BASE_URL/GetPerson?id=1"
echo "  curl $BASE_URL/GetPeople"
echo "  curl $BASE_URL/GetPersonAsText?id=1"
echo "  curl $BASE_URL/GetPeopleAsCSV"
echo "  curl $BASE_URL/GetPeopleWithMetadata"
