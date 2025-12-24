#!/bin/bash

# Test script for 03-routing sample
# Demonstrates both RPC and RESTful endpoints

BASE_URL="http://localhost:8080"

echo "========================================="
echo "mORMot2 Routing Sample - Test Script"
echo "========================================="
echo ""
echo "Testing BOTH routing patterns:"
echo "1. RPC-Style (Interface-based)"
echo "2. RESTful (TUriRouter-based)"
echo ""
echo "Make sure RoutingSample.exe is running!"
echo ""
read -p "Press Enter to start testing..."

echo ""
echo "========================================="
echo "PATTERN 1: RPC-Style (Interface-Based)"
echo "========================================="
echo ""

echo "1. Get User by ID (RPC)"
echo "POST $BASE_URL/root/RoutingApi.GetUser"
curl -s -X POST "$BASE_URL/root/RoutingApi.GetUser" \
  -H "Content-Type: application/json" \
  -d '{"id":1}' | jq .
echo ""

echo "2. List Active Users (RPC)"
echo "POST $BASE_URL/root/RoutingApi.ListUsers"
curl -s -X POST "$BASE_URL/root/RoutingApi.ListUsers" \
  -H "Content-Type: application/json" \
  -d '{"filter":"active","limit":3}' | jq .
echo ""

echo "3. Search Users (RPC)"
echo "POST $BASE_URL/root/RoutingApi.Search"
curl -s -X POST "$BASE_URL/root/RoutingApi.Search" \
  -H "Content-Type: application/json" \
  -d '{"term":"user1"}' | jq .
echo ""

echo "4. Create User (RPC)"
echo "POST $BASE_URL/root/RoutingApi.CreateUser"
NEW_USER_ID_RPC=$(curl -s -X POST "$BASE_URL/root/RoutingApi.CreateUser" \
  -H "Content-Type: application/json" \
  -d '{"name":"Alice RPC","email":"alice.rpc@example.com"}' | jq -r '.result')
echo "Created user ID: $NEW_USER_ID_RPC"
echo ""

echo "5. Update User (RPC)"
echo "POST $BASE_URL/root/RoutingApi.UpdateUser"
curl -s -X POST "$BASE_URL/root/RoutingApi.UpdateUser" \
  -H "Content-Type: application/json" \
  -d "{\"id\":$NEW_USER_ID_RPC,\"name\":\"Alice Updated\",\"email\":\"alice.updated@example.com\"}" | jq .
echo ""

echo "6. Get Users by Status (RPC)"
echo "POST $BASE_URL/root/RoutingApi.GetUsersByStatus"
curl -s -X POST "$BASE_URL/root/RoutingApi.GetUsersByStatus" \
  -H "Content-Type: application/json" \
  -d '{"status":"active","page":1,"pageSize":3}' | jq .
echo ""

echo ""
echo "========================================="
echo "PATTERN 2: RESTful (TUriRouter-Based)"
echo "========================================="
echo ""

echo "1. Get User by ID (RESTful)"
echo "GET $BASE_URL/api/users/1"
curl -s -X GET "$BASE_URL/api/users/1" | jq .
echo ""

echo "2. List Active Users (RESTful)"
echo "GET $BASE_URL/api/users?filter=active&limit=3"
curl -s -X GET "$BASE_URL/api/users?filter=active&limit=3" | jq .
echo ""

echo "3. Search Users (RESTful)"
echo "GET $BASE_URL/api/search/user1"
curl -s -X GET "$BASE_URL/api/search/user1" | jq .
echo ""

echo "4. Create User (RESTful)"
echo "POST $BASE_URL/api/users"
NEW_USER_ID_REST=$(curl -s -X POST "$BASE_URL/api/users" \
  -H "Content-Type: application/json" \
  -d '{"name":"Bob REST","email":"bob.rest@example.com"}' | jq -r '.id')
echo "Created user ID: $NEW_USER_ID_REST"
echo ""

echo "5. Update User (RESTful)"
echo "PUT $BASE_URL/api/users/$NEW_USER_ID_REST"
curl -s -X PUT "$BASE_URL/api/users/$NEW_USER_ID_REST" \
  -H "Content-Type: application/json" \
  -d '{"name":"Bob Updated","email":"bob.updated@example.com"}' | jq .
echo ""

echo "6. Get Users by Status (RESTful)"
echo "GET $BASE_URL/api/users/status/active?page=1&pageSize=3"
curl -s -X GET "$BASE_URL/api/users/status/active?page=1&pageSize=3" | jq .
echo ""

echo "7. Delete User (RESTful)"
echo "DELETE $BASE_URL/api/users/$NEW_USER_ID_REST"
curl -s -X DELETE "$BASE_URL/api/users/$NEW_USER_ID_REST" | jq .
echo ""

echo ""
echo "========================================="
echo "COMPARISON SUMMARY"
echo "========================================="
echo ""
echo "RPC-Style (Interface-Based):"
echo "  ✓ POST with JSON body for all operations"
echo "  ✓ Automatic JSON serialization/deserialization"
echo "  ✓ Type-safe method signatures"
echo "  ✓ Best for internal APIs and Delphi-to-Delphi"
echo ""
echo "RESTful (TUriRouter-Based):"
echo "  ✓ Proper HTTP verbs (GET, POST, PUT, DELETE)"
echo "  ✓ URL path parameters (/api/users/123)"
echo "  ✓ Query parameters (?filter=active&limit=5)"
echo "  ✓ Best for public APIs and web clients"
echo ""
echo "Both patterns work simultaneously!"
echo "Both patterns share the same business logic!"
echo ""
echo "========================================="
echo "Tests Complete"
echo "========================================="
