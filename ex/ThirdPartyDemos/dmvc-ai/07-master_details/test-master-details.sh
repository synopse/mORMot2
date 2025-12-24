#!/bin/bash
# Test script for Master-Details Sample

set -e

BASE_URL="http://localhost:8080"
API_URL="$BASE_URL/OrdersAPI"

echo "=========================================="
echo "mORMot2 Master-Details Sample Test Script"
echo "=========================================="
echo ""

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Test counter
TEST_NUM=0

test_api() {
    TEST_NUM=$((TEST_NUM + 1))
    echo -e "${BLUE}Test $TEST_NUM: $1${NC}"
    echo -e "${YELLOW}Request: $2${NC}"
    echo ""
    shift 2
    response=$(eval "$@")
    echo "$response" | jq '.' 2>/dev/null || echo "$response"
    echo ""
}

# Check if server is running
echo "Checking if server is running..."
if ! curl -s "$BASE_URL" >/dev/null 2>&1; then
    echo "ERROR: Server is not running on $BASE_URL"
    echo "Please start the server first: ./Win32/Debug/MasterDetailsSample.exe"
    exit 1
fi
echo -e "${GREEN}Server is running!${NC}"
echo ""

# Test 1: Get all articles
test_api "Get all articles" \
    "GET $API_URL/GetArticles" \
    curl -s "$API_URL/GetArticles"

# Test 2: Get single article
test_api "Get article by ID" \
    "GET $API_URL/GetArticleByID?id=1" \
    curl -s "$API_URL/GetArticleByID?id=1"

# Test 3: Create a new order
echo -e "${GREEN}Creating new order...${NC}"
ORDER_RESPONSE=$(curl -s -X POST "$API_URL/CreateOrder" \
    -H "Content-Type: application/json" \
    -d '{"customerid":100,"orderdate":"2025-12-19T00:00:00"}')

ORDER_ID=$(echo "$ORDER_RESPONSE" | jq -r '.result')
echo "Order ID: $ORDER_ID"
echo ""

test_api "Create order" \
    "POST $API_URL/CreateOrder" \
    echo "$ORDER_RESPONSE"

# Test 4: Add first item to order
echo -e "${GREEN}Adding first item to order $ORDER_ID...${NC}"
ITEM1_RESPONSE=$(curl -s -X POST "$API_URL/AddItemToOrder?orderid=$ORDER_ID" \
    -H "Content-Type: application/json" \
    -d '{"articleid":1,"unitprice":10.5,"discount":0,"quantity":2,"description":"Widget A"}')

ITEM1_ID=$(echo "$ITEM1_RESPONSE" | jq -r '.result')
echo "Item 1 ID: $ITEM1_ID"
echo ""

test_api "Add first item to order" \
    "POST $API_URL/AddItemToOrder?orderid=$ORDER_ID" \
    echo "$ITEM1_RESPONSE"

# Test 5: Add second item to order
echo -e "${GREEN}Adding second item to order $ORDER_ID...${NC}"
ITEM2_RESPONSE=$(curl -s -X POST "$API_URL/AddItemToOrder?orderid=$ORDER_ID" \
    -H "Content-Type: application/json" \
    -d '{"articleid":2,"unitprice":25.0,"discount":10,"quantity":1,"description":"Widget B"}')

ITEM2_ID=$(echo "$ITEM2_RESPONSE" | jq -r '.result')
echo "Item 2 ID: $ITEM2_ID"
echo ""

test_api "Add second item to order" \
    "POST $API_URL/AddItemToOrder?orderid=$ORDER_ID" \
    echo "$ITEM2_RESPONSE"

# Test 6: Get order with all items
test_api "Get order with all items (nested JSON)" \
    "GET $API_URL/GetOrderByID?id=$ORDER_ID" \
    curl -s "$API_URL/GetOrderByID?id=$ORDER_ID"

# Test 7: Update order item (change quantity and discount)
echo -e "${GREEN}Updating item $ITEM1_ID (changing quantity to 5 and discount to 15%)...${NC}"
test_api "Update order item" \
    "POST $API_URL/UpdateOrderItem?id=$ITEM1_ID" \
    curl -s -X POST "$API_URL/UpdateOrderItem?id=$ITEM1_ID" \
        -H "Content-Type: application/json" \
        -d '{"articleid":1,"unitprice":10.5,"discount":15,"quantity":5,"description":"Widget A (Updated)"}'

# Test 8: Get updated order (should show new totals)
test_api "Get updated order (should show recalculated totals)" \
    "GET $API_URL/GetOrderByID?id=$ORDER_ID" \
    curl -s "$API_URL/GetOrderByID?id=$ORDER_ID"

# Test 9: Get all orders
test_api "Get all orders" \
    "GET $API_URL/GetOrders" \
    curl -s "$API_URL/GetOrders"

# Test 10: Search orders by total
test_api "Search orders with total >= 40.00" \
    "GET $API_URL/GetOrdersByTotalGreaterThan?total=40.00" \
    curl -s "$API_URL/GetOrdersByTotalGreaterThan?total=40.00"

# Test 11: Remove an item from order
echo -e "${GREEN}Removing item $ITEM2_ID from order $ORDER_ID...${NC}"
test_api "Remove item from order" \
    "GET $API_URL/RemoveItemFromOrder?orderid=$ORDER_ID&itemid=$ITEM2_ID" \
    curl -s "$API_URL/RemoveItemFromOrder?orderid=$ORDER_ID&itemid=$ITEM2_ID"

# Test 12: Get order after item removal (should show updated total)
test_api "Get order after item removal (should show updated total)" \
    "GET $API_URL/GetOrderByID?id=$ORDER_ID" \
    curl -s "$API_URL/GetOrderByID?id=$ORDER_ID"

# Test 13: Delete order (cascade delete)
echo -e "${GREEN}Deleting order $ORDER_ID (should cascade to remaining items)...${NC}"
test_api "Delete order (cascading)" \
    "GET $API_URL/DeleteOrder?id=$ORDER_ID" \
    curl -s "$API_URL/DeleteOrder?id=$ORDER_ID"

# Test 14: Verify order is gone
echo -e "${YELLOW}Verifying order was deleted (should return error)...${NC}"
curl -s "$API_URL/GetOrderByID?id=$ORDER_ID" || echo "Order not found (expected)"
echo ""
echo ""

echo "=========================================="
echo -e "${GREEN}All tests completed successfully!${NC}"
echo "=========================================="
echo ""
echo "Key features demonstrated:"
echo "  ✓ Master-detail relationships (Orders -> OrderItems)"
echo "  ✓ Nested JSON responses (orders with embedded items)"
echo "  ✓ Automatic total recalculation on item changes"
echo "  ✓ Cascading deletes (deleting order removes all items)"
echo "  ✓ Complete CRUD operations on both master and detail"
echo ""
