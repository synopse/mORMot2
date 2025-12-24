#!/bin/bash
# Test script for RoutingSample endpoints
# Usage: ./test-endpoints.sh [port]

PORT=${1:-8080}
BASE_URL="http://localhost:$PORT/root"

echo "========================================"
echo "  Testing RoutingSample API Endpoints"
echo "========================================"
echo ""
echo "Server: $BASE_URL"
echo ""

# Test 1: Get User
echo "1. Testing GetUser (id=1)..."
curl -s -X POST "$BASE_URL/RoutingApi.GetUser" \
  -H "Content-Type: application/json" \
  -d '{"id":1}' | jq '.'
echo ""

# Test 2: List Users (active, limit 5)
echo "2. Testing ListUsers (filter=active, limit=5)..."
curl -s -X POST "$BASE_URL/RoutingApi.ListUsers" \
  -H "Content-Type: application/json" \
  -d '{"filter":"active","limit":5}' | jq '.'
echo ""

# Test 3: Search
echo "3. Testing Search (term=user1)..."
curl -s -X POST "$BASE_URL/RoutingApi.Search" \
  -H "Content-Type: application/json" \
  -d '{"term":"user1"}' | jq '.'
echo ""

# Test 4: Get User Details with stats
echo "4. Testing GetUserDetails (id=1, includeStats=true)..."
curl -s -X POST "$BASE_URL/RoutingApi.GetUserDetails" \
  -H "Content-Type: application/json" \
  -d '{"id":1,"includeStats":true}' | jq '.'
echo ""

# Test 5: Create User
echo "5. Testing CreateUser..."
NEW_ID=$(curl -s -X POST "$BASE_URL/RoutingApi.CreateUser" \
  -H "Content-Type: application/json" \
  -d '{"name":"Test User","email":"test@example.com"}')
echo "Created user ID: $NEW_ID"
echo ""

# Test 6: Update User
echo "6. Testing UpdateUser (id=$NEW_ID)..."
curl -s -X POST "$BASE_URL/RoutingApi.UpdateUser" \
  -H "Content-Type: application/json" \
  -d "{\"id\":$NEW_ID,\"name\":\"Updated Test User\",\"email\":\"updated@example.com\"}"
echo ""

# Test 7: Get Users by Status (pagination)
echo "7. Testing GetUsersByStatus (status=active, page=1, pageSize=3)..."
curl -s -X POST "$BASE_URL/RoutingApi.GetUsersByStatus" \
  -H "Content-Type: application/json" \
  -d '{"status":"active","page":1,"pageSize":3}' | jq '.'
echo ""

# Test 8: Delete User
echo "8. Testing DeleteUser (id=$NEW_ID)..."
curl -s -X POST "$BASE_URL/RoutingApi.DeleteUser" \
  -H "Content-Type: application/json" \
  -d "{\"id\":$NEW_ID}"
echo ""

# Test 9: Batch Delete (create some users first)
echo "9. Testing BatchDeleteUsers..."
ID1=$(curl -s -X POST "$BASE_URL/RoutingApi.CreateUser" \
  -H "Content-Type: application/json" \
  -d '{"name":"Batch User 1","email":"batch1@example.com"}')
ID2=$(curl -s -X POST "$BASE_URL/RoutingApi.CreateUser" \
  -H "Content-Type: application/json" \
  -d '{"name":"Batch User 2","email":"batch2@example.com"}')
ID3=$(curl -s -X POST "$BASE_URL/RoutingApi.CreateUser" \
  -H "Content-Type: application/json" \
  -d '{"name":"Batch User 3","email":"batch3@example.com"}')

DELETED=$(curl -s -X POST "$BASE_URL/RoutingApi.BatchDeleteUsers" \
  -H "Content-Type: application/json" \
  -d "{\"ids\":[$ID1,$ID2,$ID3]}")
echo "Deleted $DELETED users"
echo ""

echo "========================================"
echo "  All tests completed!"
echo "========================================"
