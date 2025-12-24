# Testing Guide for 08-basicauth

## Quick Start

1. **Compile the project**:
   ```bash
   cd /mnt/w/mORMot2/ex/dmvc/08-basicauth
   /mnt/w/Agentic-Coding/Tools/delphi-compiler.exe "W:\mORMot2\ex\dmvc\08-basicauth\08-basicauth.dproj"
   ```

2. **Run the server**:
   ```bash
   ./Win32/Debug/08-basicauth.exe
   ```

3. **Server should start on port 8080** and display available endpoints

## Test Scenarios

### Scenario 1: Public Endpoint (No Authentication)

**Expected**: Should work without credentials

```bash
curl http://localhost:8080/root/BasicAuthApi.PublicSection
```

**Expected Response**:
```
This is a public section
```

**Status Code**: 200 OK

---

### Scenario 2: Protected Endpoint Without Credentials

**Expected**: Should return 401 Unauthorized

```bash
curl -v http://localhost:8080/root/BasicAuthApi.OnlyRole1
```

**Expected Response**: Error message or 401 status

**Status Code**: 401 Unauthorized

---

### Scenario 3: Protected Endpoint With Valid Credentials (user1)

**Expected**: Should authenticate successfully and show user info

```bash
curl -u user1:user1 http://localhost:8080/root/BasicAuthApi.OnlyRole1
```

**Expected Response**:
```
Hey! Hello user1, now you are a logged user and this is a protected content!
As logged user you have the following roles:
role1
```

**Status Code**: 200 OK

---

### Scenario 4: Wrong Role (user1 accessing role2 endpoint)

**Expected**: Should return 403 Forbidden

```bash
curl -v -u user1:user1 http://localhost:8080/root/BasicAuthApi.OnlyRole2
```

**Expected Response**: Forbidden error

**Status Code**: 403 Forbidden

---

### Scenario 5: User with Multiple Roles (user3)

**Expected**: Should access both role1 and role2 endpoints

```bash
# Access role1 endpoint
curl -u user3:user3 http://localhost:8080/root/BasicAuthApi.OnlyRole1

# Access role2 endpoint
curl -u user3:user3 http://localhost:8080/root/BasicAuthApi.OnlyRole2
```

**Expected Response**: Both should succeed

**Status Code**: 200 OK for both

---

### Scenario 6: JSON Endpoint with Parameters

**Expected**: Should return JSON response with parameter

```bash
curl -u user1:user1 "http://localhost:8080/root/BasicAuthApi.OnlyRole1Json?par1=HelloWorld"
```

**Expected Response**:
```json
{
  "message": "This is protected content accessible only by role1",
  "parameter": "HelloWorld",
  "user": "user1"
}
```

**Status Code**: 200 OK

---

### Scenario 7: Invalid Credentials

**Expected**: Should return 401 Unauthorized

```bash
curl -v -u user1:wrongpassword http://localhost:8080/root/BasicAuthApi.OnlyRole1
```

**Expected Response**: Authentication failed

**Status Code**: 401 Unauthorized

---

### Scenario 8: Base64 Encoded Authorization Header (Manual)

**Test the Basic Auth parsing directly**

```bash
# user1:user1 in base64 = dXNlcjE6dXNlcjE=
curl -v -H "Authorization: Basic dXNlcjE6dXNlcjE=" http://localhost:8080/root/BasicAuthApi.OnlyRole1
```

**Expected Response**: Same as Scenario 3

**Status Code**: 200 OK

---

## Test Matrix

| User | Password | Roles | PublicSection | Index | OnlyRole1 | OnlyRole1Json | OnlyRole2 |
|------|----------|-------|---------------|-------|-----------|---------------|-----------|
| (none) | (none) | - | ✓ 200 | ✓ 200 | ✗ 401 | ✗ 401 | ✗ 401 |
| user1 | user1 | role1 | ✓ 200 | ✓ 200 | ✓ 200 | ✓ 200 | ✗ 403 |
| user2 | user2 | role2 | ✓ 200 | ✓ 200 | ✗ 403 | ✗ 403 | ✓ 200 |
| user3 | user3 | role1,role2 | ✓ 200 | ✓ 200 | ✓ 200 | ✓ 200 | ✓ 200 |
| user1 | wrong | - | ✓ 200 | ✓ 200 | ✗ 401 | ✗ 401 | ✗ 401 |

## Automated Test Script

Save as `test-auth.sh`:

```bash
#!/bin/bash

BASE_URL="http://localhost:8080/root/BasicAuthApi"

echo "=== Testing 08-basicauth ==="
echo

echo "1. Public endpoint (no auth):"
curl -s "$BASE_URL.PublicSection"
echo -e "\n"

echo "2. Protected endpoint without credentials (expect 401):"
curl -s -w "Status: %{http_code}\n" "$BASE_URL.OnlyRole1"
echo

echo "3. Protected endpoint with user1 credentials:"
curl -s -u user1:user1 "$BASE_URL.OnlyRole1"
echo -e "\n"

echo "4. user1 accessing role2 endpoint (expect 403):"
curl -s -w "Status: %{http_code}\n" -u user1:user1 "$BASE_URL.OnlyRole2"
echo

echo "5. user3 (all roles) accessing role1 endpoint:"
curl -s -u user3:user3 "$BASE_URL.OnlyRole1"
echo -e "\n"

echo "6. user3 (all roles) accessing role2 endpoint:"
curl -s -u user3:user3 "$BASE_URL.OnlyRole2"
echo -e "\n"

echo "7. JSON endpoint with parameter:"
curl -s -u user1:user1 "$BASE_URL.OnlyRole1Json?par1=TestParam"
echo -e "\n"

echo "8. Invalid credentials (expect 401):"
curl -s -w "Status: %{http_code}\n" -u user1:wrongpassword "$BASE_URL.OnlyRole1"
echo

echo "=== Tests Complete ==="
```

Run with:
```bash
chmod +x test-auth.sh
./test-auth.sh
```

## Browser Testing

1. Navigate to public endpoint:
   - http://localhost:8080/root/BasicAuthApi.PublicSection
   - Should display: "This is a public section"

2. Navigate to protected endpoint:
   - http://localhost:8080/root/BasicAuthApi.OnlyRole1
   - Browser should prompt for credentials
   - Enter: username=user1, password=user1
   - Should display welcome message with roles

3. Test wrong role:
   - Stay logged in as user1
   - Navigate to: http://localhost:8080/root/BasicAuthApi.OnlyRole2
   - Should see 403 Forbidden error

## Troubleshooting

### Server won't start
- Check if port 8080 is already in use
- Try a different port by modifying the .dpr file

### Authentication not working
- Check server logs (TSynLog output)
- Verify Authorization header format: `Basic <base64>`
- Ensure credentials are correctly base64 encoded

### All requests return 401
- Check if `RequiresAuthentication()` is working correctly
- Verify method name extraction in `OnBeforeBody`

### Compilation errors
- Ensure mORMot2 path is correct in dproj
- Check all source files are present in src/ folder

## Expected Log Output

Server should log authentication attempts:

```
Debug: Public endpoint, no auth required: PublicSection
Debug: No Authorization header for protected endpoint: OnlyRole1
Debug: User user1 authenticated with roles [role1] for method OnlyRole1
Debug: Authentication failed for user: user1
Debug: Authorization failed for user user1 on method OnlyRole2
```
