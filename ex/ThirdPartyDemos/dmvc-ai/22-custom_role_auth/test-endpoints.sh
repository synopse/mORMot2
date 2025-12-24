#!/bin/bash

# Test script for Custom Role-Based Authentication Sample
# Tests all users, all endpoints, and all role combinations

PORT="${1:-8080}"
BASE_URL="http://localhost:$PORT/root"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo ""
echo "================================================"
echo "  Custom Role-Based Authentication Tests"
echo "================================================"
echo ""

# Test counter
TOTAL=0
PASSED=0
FAILED=0

# Function to test an endpoint
test_endpoint() {
    local description="$1"
    local url="$2"
    local auth_header="$3"
    local data="$4"
    local expected_code="$5"

    TOTAL=$((TOTAL + 1))

    echo -n "Test $TOTAL: $description... "

    if [ -z "$auth_header" ]; then
        response=$(curl -s -w "\n%{http_code}" -X POST "$url" \
            -H "Content-Type: application/json" \
            -d "$data" 2>&1)
    else
        response=$(curl -s -w "\n%{http_code}" -X POST "$url" \
            -H "Content-Type: application/json" \
            -H "Authorization: Bearer $auth_header" \
            -d "$data" 2>&1)
    fi

    http_code=$(echo "$response" | tail -n1)
    body=$(echo "$response" | head -n-1)

    if [ "$http_code" = "$expected_code" ]; then
        echo -e "${GREEN}✓ PASS${NC} (HTTP $http_code)"
        PASSED=$((PASSED + 1))
        if [ "$expected_code" = "200" ]; then
            echo "   Response: $body"
        fi
    else
        echo -e "${RED}✗ FAIL${NC} (Expected HTTP $expected_code, got $http_code)"
        FAILED=$((FAILED + 1))
        echo "   Response: $body"
    fi
}

# Function to login and extract session
login_user() {
    local username="$1"
    local password="$2"

    response=$(curl -s -X POST "$BASE_URL/Auth" \
        -H "Content-Type: application/json" \
        -d "{\"userName\":\"$username\",\"password\":\"$password\"}")

    # Extract session ID and signature
    session_id=$(echo "$response" | grep -o '"result":[0-9]*' | grep -o '[0-9]*')
    signature=$(echo "$response" | grep -o '"session_signature":"[^"]*"' | cut -d'"' -f4)

    echo "${session_id}+${signature}"
}

echo "=== Public Endpoints (No Auth) ==="
echo ""

test_endpoint \
    "PublicAction (no auth)" \
    "$BASE_URL/PrivateApi.PublicAction" \
    "" \
    "{}" \
    "200"

echo ""
echo "=== Authentication Tests ==="
echo ""

# Test valid logins
echo -e "${BLUE}Logging in as admin...${NC}"
ADMIN_SESSION=$(login_user "admin" "adminpass")
echo "Admin session: $ADMIN_SESSION"
echo ""

echo -e "${BLUE}Logging in as user1 (role1)...${NC}"
USER1_SESSION=$(login_user "user1" "user1pass")
echo "User1 session: $USER1_SESSION"
echo ""

echo -e "${BLUE}Logging in as user2 (role2)...${NC}"
USER2_SESSION=$(login_user "user2" "user2pass")
echo "User2 session: $USER2_SESSION"
echo ""

echo -e "${BLUE}Logging in as user1_2 (role1+role2)...${NC}"
USER1_2_SESSION=$(login_user "user1_2" "user1_2pass")
echo "User1_2 session: $USER1_2_SESSION"
echo ""

echo -e "${BLUE}Logging in as user3 (role3)...${NC}"
USER3_SESSION=$(login_user "user3" "user3pass")
echo "User3 session: $USER3_SESSION"
echo ""

echo "=== Protected Endpoints - Index (any auth) ==="
echo ""

test_endpoint \
    "Index - admin" \
    "$BASE_URL/PrivateApi.Index" \
    "$ADMIN_SESSION" \
    "{}" \
    "200"

test_endpoint \
    "Index - user1" \
    "$BASE_URL/PrivateApi.Index" \
    "$USER1_SESSION" \
    "{}" \
    "200"

test_endpoint \
    "Index - user3" \
    "$BASE_URL/PrivateApi.Index" \
    "$USER3_SESSION" \
    "{}" \
    "200"

test_endpoint \
    "Index - no auth (should fail)" \
    "$BASE_URL/PrivateApi.Index" \
    "" \
    "{}" \
    "403"

echo ""
echo "=== Protected Endpoints - OnlyRole1 ==="
echo ""

test_endpoint \
    "OnlyRole1 - admin (has role1)" \
    "$BASE_URL/PrivateApi.OnlyRole1" \
    "$ADMIN_SESSION" \
    "{}" \
    "200"

test_endpoint \
    "OnlyRole1 - user1 (has role1)" \
    "$BASE_URL/PrivateApi.OnlyRole1" \
    "$USER1_SESSION" \
    "{}" \
    "200"

test_endpoint \
    "OnlyRole1 - user2 (no role1)" \
    "$BASE_URL/PrivateApi.OnlyRole1" \
    "$USER2_SESSION" \
    "{}" \
    "403"

test_endpoint \
    "OnlyRole1 - user3 (no role1)" \
    "$BASE_URL/PrivateApi.OnlyRole1" \
    "$USER3_SESSION" \
    "{}" \
    "403"

echo ""
echo "=== Protected Endpoints - OnlyRole2 ==="
echo ""

test_endpoint \
    "OnlyRole2 - admin (has role2)" \
    "$BASE_URL/PrivateApi.OnlyRole2" \
    "$ADMIN_SESSION" \
    "{}" \
    "200"

test_endpoint \
    "OnlyRole2 - user1 (no role2)" \
    "$BASE_URL/PrivateApi.OnlyRole2" \
    "$USER1_SESSION" \
    "{}" \
    "403"

test_endpoint \
    "OnlyRole2 - user2 (has role2)" \
    "$BASE_URL/PrivateApi.OnlyRole2" \
    "$USER2_SESSION" \
    "{}" \
    "200"

echo ""
echo "=== Protected Endpoints - OnlyRole1And2 (requires BOTH) ==="
echo ""

test_endpoint \
    "OnlyRole1And2 - admin (has both)" \
    "$BASE_URL/PrivateApi.OnlyRole1And2" \
    "$ADMIN_SESSION" \
    "{}" \
    "200"

test_endpoint \
    "OnlyRole1And2 - user1 (only role1)" \
    "$BASE_URL/PrivateApi.OnlyRole1And2" \
    "$USER1_SESSION" \
    "{}" \
    "403"

test_endpoint \
    "OnlyRole1And2 - user2 (only role2)" \
    "$BASE_URL/PrivateApi.OnlyRole1And2" \
    "$USER2_SESSION" \
    "{}" \
    "403"

test_endpoint \
    "OnlyRole1And2 - user1_2 (has both)" \
    "$BASE_URL/PrivateApi.OnlyRole1And2" \
    "$USER1_2_SESSION" \
    "{}" \
    "200"

test_endpoint \
    "OnlyRole1And2 - user3 (has neither)" \
    "$BASE_URL/PrivateApi.OnlyRole1And2" \
    "$USER3_SESSION" \
    "{}" \
    "403"

echo ""
echo "=== Protected Endpoints - OnlyRole1Or2 (requires EITHER) ==="
echo ""

test_endpoint \
    "OnlyRole1Or2 - admin (has both)" \
    "$BASE_URL/PrivateApi.OnlyRole1Or2" \
    "$ADMIN_SESSION" \
    "{}" \
    "200"

test_endpoint \
    "OnlyRole1Or2 - user1 (has role1)" \
    "$BASE_URL/PrivateApi.OnlyRole1Or2" \
    "$USER1_SESSION" \
    "{}" \
    "200"

test_endpoint \
    "OnlyRole1Or2 - user2 (has role2)" \
    "$BASE_URL/PrivateApi.OnlyRole1Or2" \
    "$USER2_SESSION" \
    "{}" \
    "200"

test_endpoint \
    "OnlyRole1Or2 - user1_2 (has both)" \
    "$BASE_URL/PrivateApi.OnlyRole1Or2" \
    "$USER1_2_SESSION" \
    "{}" \
    "200"

test_endpoint \
    "OnlyRole1Or2 - user3 (has neither)" \
    "$BASE_URL/PrivateApi.OnlyRole1Or2" \
    "$USER3_SESSION" \
    "{}" \
    "403"

echo ""
echo "=== Protected Endpoints - AccessByRole (dynamic) ==="
echo ""

test_endpoint \
    "AccessByRole - user1 with role1" \
    "$BASE_URL/PrivateApi.AccessByRole" \
    "$USER1_SESSION" \
    '{"role":"role1"}' \
    "200"

test_endpoint \
    "AccessByRole - user2 with role2" \
    "$BASE_URL/PrivateApi.AccessByRole" \
    "$USER2_SESSION" \
    '{"role":"role2"}' \
    "200"

echo ""
echo "================================================"
echo "  Test Summary"
echo "================================================"
echo -e "Total:  $TOTAL"
echo -e "${GREEN}Passed: $PASSED${NC}"
echo -e "${RED}Failed: $FAILED${NC}"
echo ""

if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}All tests passed! ✓${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed ✗${NC}"
    exit 1
fi
