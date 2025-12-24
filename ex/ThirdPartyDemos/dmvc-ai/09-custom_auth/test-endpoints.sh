#!/bin/bash
# Test script for Custom Authentication Sample
# Demonstrates authentication and role-based authorization

PORT=${1:-8080}
BASE_URL="http://localhost:$PORT/root"

echo "=========================================="
echo "Custom Authentication Sample - Test Suite"
echo "=========================================="
echo ""

# Color codes
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Test counter
TESTS_PASSED=0
TESTS_FAILED=0

# Function to test endpoint
test_endpoint() {
    local name="$1"
    local endpoint="$2"
    local data="$3"
    local auth_header="$4"
    local expected_status="$5"

    echo -n "Testing: $name ... "

    if [ -z "$auth_header" ]; then
        response=$(curl -s -w "\n%{http_code}" -X POST "$BASE_URL/$endpoint" \
            -H "Content-Type: application/json" \
            -d "$data" 2>/dev/null)
    else
        response=$(curl -s -w "\n%{http_code}" -X POST "$BASE_URL/$endpoint" \
            -H "Content-Type: application/json" \
            -H "$auth_header" \
            -d "$data" 2>/dev/null)
    fi

    status_code=$(echo "$response" | tail -n1)
    body=$(echo "$response" | head -n-1)

    if [ "$status_code" = "$expected_status" ]; then
        echo -e "${GREEN}PASS${NC} (HTTP $status_code)"
        ((TESTS_PASSED++))
        if [ -n "$body" ] && [ "$body" != "" ]; then
            echo "  Response: $body"
        fi
    else
        echo -e "${RED}FAIL${NC} (Expected HTTP $expected_status, got $status_code)"
        ((TESTS_FAILED++))
        if [ -n "$body" ] && [ "$body" != "" ]; then
            echo "  Response: $body"
        fi
    fi
    echo ""
}

# Function to authenticate and get session
authenticate() {
    local username="$1"
    local password="$2"

    echo "Authenticating as $username..."

    response=$(curl -s -X POST "$BASE_URL/Auth" \
        -H "Content-Type: application/json" \
        -d "{\"userName\":\"$username\",\"password\":\"$password\"}" 2>/dev/null)

    echo "  Response: $response"

    # Extract session_signature from JSON response
    # Format: {"result":12345,"data":{"session_signature":"ABC..."}}
    session_id=$(echo "$response" | grep -o '"result":[0-9]*' | grep -o '[0-9]*')
    session_sig=$(echo "$response" | grep -o '"session_signature":"[^"]*"' | cut -d'"' -f4)

    if [ -n "$session_id" ] && [ -n "$session_sig" ]; then
        echo -e "  ${GREEN}Success${NC}: Session ID=$session_id"
        echo "  Authorization: Bearer ${session_id}+${session_sig}"
        echo "$session_id+$session_sig"
    else
        echo -e "  ${RED}Failed${NC}: Could not extract session"
        echo ""
    fi
}

echo "=== Phase 1: Public Endpoints (No Auth) ==="
echo ""

test_endpoint \
    "PublicApi.Index" \
    "PublicApi.Index" \
    "{}" \
    "" \
    "200"

test_endpoint \
    "PrivateApi.PublicAction" \
    "PrivateApi.PublicAction" \
    "{}" \
    "" \
    "200"

echo "=== Phase 2: Protected Endpoints (Should Fail Without Auth) ==="
echo ""

test_endpoint \
    "PrivateApi.Index (no auth)" \
    "PrivateApi.Index" \
    "{}" \
    "" \
    "403"

test_endpoint \
    "PrivateApi.OnlyRole1 (no auth)" \
    "PrivateApi.OnlyRole1" \
    "{}" \
    "" \
    "403"

echo "=== Phase 3: Admin Authentication ==="
echo ""

ADMIN_SESSION=$(authenticate "admin" "adminpass")
echo ""

if [ -n "$ADMIN_SESSION" ]; then
    ADMIN_AUTH="Authorization: Bearer $ADMIN_SESSION"

    echo "=== Phase 4: Admin Access (Should Succeed) ==="
    echo ""

    test_endpoint \
        "PrivateApi.Index (admin)" \
        "PrivateApi.Index" \
        "{}" \
        "$ADMIN_AUTH" \
        "200"

    test_endpoint \
        "PrivateApi.OnlyRole1 (admin)" \
        "PrivateApi.OnlyRole1" \
        "{}" \
        "$ADMIN_AUTH" \
        "200"

    test_endpoint \
        "PrivateApi.OnlyRole2 (admin)" \
        "PrivateApi.OnlyRole2" \
        "{}" \
        "$ADMIN_AUTH" \
        "200"
fi

echo "=== Phase 5: User1 Authentication (role1) ==="
echo ""

USER1_SESSION=$(authenticate "user1" "user1pass")
echo ""

if [ -n "$USER1_SESSION" ]; then
    USER1_AUTH="Authorization: Bearer $USER1_SESSION"

    echo "=== Phase 6: User1 Access Tests ==="
    echo ""

    test_endpoint \
        "PrivateApi.Index (user1, should fail - needs admin)" \
        "PrivateApi.Index" \
        "{}" \
        "$USER1_AUTH" \
        "403"

    test_endpoint \
        "PrivateApi.OnlyRole1 (user1, should succeed)" \
        "PrivateApi.OnlyRole1" \
        "{}" \
        "$USER1_AUTH" \
        "200"

    test_endpoint \
        "PrivateApi.OnlyRole2 (user1, should fail - needs role2)" \
        "PrivateApi.OnlyRole2" \
        "{}" \
        "$USER1_AUTH" \
        "403"
fi

echo "=== Phase 7: User2 Authentication (role2) ==="
echo ""

USER2_SESSION=$(authenticate "user2" "user2pass")
echo ""

if [ -n "$USER2_SESSION" ]; then
    USER2_AUTH="Authorization: Bearer $USER2_SESSION"

    echo "=== Phase 8: User2 Access Tests ==="
    echo ""

    test_endpoint \
        "PrivateApi.Index (user2, should fail - needs admin)" \
        "PrivateApi.Index" \
        "{}" \
        "$USER2_AUTH" \
        "403"

    test_endpoint \
        "PrivateApi.OnlyRole1 (user2, should fail - needs role1)" \
        "PrivateApi.OnlyRole1" \
        "{}" \
        "$USER2_AUTH" \
        "403"

    test_endpoint \
        "PrivateApi.OnlyRole2 (user2, should succeed)" \
        "PrivateApi.OnlyRole2" \
        "{}" \
        "$USER2_AUTH" \
        "200"
fi

echo "=== Phase 9: Invalid Credentials ==="
echo ""

echo "Attempting invalid login..."
response=$(curl -s -X POST "$BASE_URL/Auth" \
    -H "Content-Type: application/json" \
    -d '{"userName":"invalid","password":"wrong"}' 2>/dev/null)
echo "  Response: $response"

if echo "$response" | grep -q "errorText"; then
    echo -e "  ${GREEN}PASS${NC}: Invalid credentials rejected"
    ((TESTS_PASSED++))
else
    echo -e "  ${RED}FAIL${NC}: Invalid credentials should be rejected"
    ((TESTS_FAILED++))
fi
echo ""

echo "=========================================="
echo "Test Results"
echo "=========================================="
echo -e "Passed: ${GREEN}$TESTS_PASSED${NC}"
echo -e "Failed: ${RED}$TESTS_FAILED${NC}"
echo ""

if [ $TESTS_FAILED -eq 0 ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed.${NC}"
    exit 1
fi
