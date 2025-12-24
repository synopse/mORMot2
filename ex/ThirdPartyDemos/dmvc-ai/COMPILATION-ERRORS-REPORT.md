# COMPILATION ERRORS REPORT

**Date**: 2025-12-20
**Status**: 51 samples with API mismatches detected
**Root Cause**: Samples were generated with mORMot2 API assumptions that don't match actual library

---

## TIER 1: Core REST Samples (4/4 FAILED)

### 01-basicdemo_server
- **Errors**: 2 errors, 2 warnings
- **Main Issue**: `ServiceDefine` overload mismatch
  - Generated code calls: `ServiceDefine(TMyApi, [IMyApi], sicShared)`
  - mORMot2 actual signature differs in parameter count/types
- **Fix Required**: Check actual `TRestServerDB.ServiceDefine()` signature in mormot.orm.server

### 02-console_sample
- **Errors**: 2 errors
- **Main Issue**: `ToRecord` undeclared
  - Line 93: `ToRecord(...)` not found
  - Likely meant to be different API call
- **Fix Required**: Identify correct mORMot2 method for converting to records

### 03-routing
- **Errors**: 5 errors, 10 warnings
- **Main Issue**:
  - `TRestServerFullMemory` undeclared
  - `sicShared` undeclared (service mode)
- **Fix Required**: Check if these should use `TRestServerDB` instead, and actual service registration syntax

### 04-renders
- **Errors**: 10 errors
- **Main Issue**:
  - `RetrieveListObjArray` parameter mismatch
  - `FormatUtf8` overload issues
  - `CardinalToHexLower` undeclared
- **Fix Required**: Use correct mORMot2 function signatures

---

## TIER 2: Data Samples (3/3 FAILED)

### 05-datasets
- **Errors**: 10 errors
- **Main Issue**:
  - `THttpClientSocket.Create` signature mismatch
  - `DynArrayLoadJson` parameter types wrong
  - `Min` function undeclared (needs `System.Math`)
- **Fix Required**: Use correct HTTP client API, check DynArray functions

### 06-articles_crud_server
- **Errors**: 10 errors
- **Main Issue**:
  - Constructor signature mismatch with interface
  - `RetrieveListObjArray` wrong parameters
  - Multiple unsatisfied forward declarations
- **Fix Required**: Align implementation with interface, fix API calls

### 07-master_details
- **Errors**: 10 errors
- **Main Issue**:
  - `TOrmOrderItemObjArray` undeclared
  - `TOrmOrderObjArray` undeclared
  - Type incompatibilities
- **Fix Required**: Define correct ORM array types or use different approach

---

## TIER 3: Auth Samples (4/4 FAILED)

### 08-basicauth
- **Errors**: 4 errors
- **Main Issue**:
  - `ServiceDefine` too many parameters
  - `sicShared` undeclared
  - `OnBeforeBody` issues
- **Fix Required**: Check actual service registration and server event API

### 09-custom_auth
- **Errors**: Multiple undeclared identifiers
- **Main Issue**:
  - `TRestServerFullMemory` missing
  - `TOrmModel` construction issues
  - Missing authentication API methods
- **Fix Required**: Check authentication handler base class and methods

### 10-jsonwebtoken
- **Errors**: Multiple signature mismatches
- **Main Issue**:
  - `RetrieveSession` declaration differs
  - Parameter count issues
  - Missing auth constants
- **Fix Required**: Review actual JWT auth handler pattern in mORMot2

### 11-ssl_server
- **Errors**: Type incompatibility
- **Main Issue**:
  - HTTP server request handler signature mismatch
- **Fix Required**: Check `TOnHttpServerRequest` actual signature

---

## Remaining Samples (31/51)

Tiers 4-5 and Phase 2 samples likely have similar issues:
- Middleware registration API mismatches
- Service registration API changes
- ORM usage pattern differences
- Event handler signatures
- Type definition mismatches

---

## ROOT CAUSES IDENTIFIED

1. **API Version Mismatch**: Samples assume mORMot2 API different from actual
   - `ServiceDefine()` parameters
   - Service mode constants (`sicShared`, etc.)
   - ORM array types
   - Authentication handler base classes

2. **Missing Helper Functions**:
   - `ToRecord()`
   - `RetrieveListObjArray()` with correct params
   - `CardinalToHexLower()`
   - Proper ORM array type definitions

3. **Event Handler Signatures**:
   - `OnBeforeBody` signature
   - `OnRequest` handler signature
   - Authentication callback signatures

4. **Type Definitions**:
   - ORM entity array types (`TOrmXxxObjArray`)
   - Service mode constants
   - Authentication result types

---

## RECOMMENDED APPROACH

Instead of fixing 51 samples individually, we should:

1. **Reference Actual mORMot2 Examples**:
   - Check `/mnt/w/mORMot2/ex/tdd-service/`
   - Check `/mnt/w/mORMot2/ex/mvc-blog/`
   - Check `/mnt/w/mORMot2/ex/rest-websockets/`

2. **Extract Correct Patterns**:
   - How they register services
   - How they set up auth
   - How they use ORM
   - How they handle events

3. **Regenerate Samples**:
   - Use correct mORMot2 patterns from working examples
   - Simpler approach: fewer features but correct API
   - Focus on demonstrating each concept, not full-featured samples

4. **Test as We Go**:
   - Compile each sample before moving to next
   - Fix API issues immediately
   - Document working patterns

---

## NEXT STEPS

**Option 1**: Fix all 51 samples (high effort, complex)
- Requires deep investigation of mORMot2 APIs
- High risk of missing edge cases
- Time-consuming

**Option 2**: Create minimal working samples (recommended)
- Use patterns from actual mORMot2 examples
- One feature per sample, done correctly
- Much faster to complete
- Better educational value
- Higher success rate

**Recommendation**: Option 2 - Create minimal, working samples based on actual mORMot2 working examples.

---

## COMMANDS TO INVESTIGATE

```bash
# Check actual mORMot2 working examples
ls -la /mnt/w/mORMot2/ex/tdd-service/src/
ls -la /mnt/w/mORMot2/ex/mvc-blog/
ls -la /mnt/w/mORMot2/ex/rest-websockets/

# Review actual API signatures
grep -r "ServiceDefine" /mnt/w/mORMot2/src/ | head -5
grep -r "TRestServerDB.Create" /mnt/w/mORMot2/ex/ | head -5
```

---

**Status**: 🔴 **12/51 samples recompiled, all with API errors**
**Decision Required**: Which approach to fix remaining 39 samples?
