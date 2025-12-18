# 25. Testing and Logging

*Quality Assurance and Diagnostics*

mORMot provides comprehensive testing and logging capabilities through `mormot.core.test` and `mormot.core.log` units. These tools are essential for building reliable, maintainable applications.

---

## 25.1. Automated Testing

### 25.1.1. Why Test?

Testing ensures:
- Code correctness
- Regression prevention
- Documentation through examples
- Refactoring confidence
- Design quality (testable code is better code)

### 25.1.2. Test-Driven Development

The recommended approach:

1. Write a void implementation (interface only)
2. Write a test
3. Run test - it must **fail**
4. Implement the feature
5. Run test - it must **pass**
6. Refactor and repeat

---

## 25.2. Testing Classes

### 25.2.1. Class Hierarchy

```
TSynTest (abstract)
├── TSynTestCase    → Individual test case
└── TSynTests       → Test suite (runs multiple cases)
```

### 25.2.2. TSynTestCase

Defines individual tests in published methods:

```pascal
uses
  mormot.core.test;

type
  TTestMathOperations = class(TSynTestCase)
  published
    procedure TestAddition;
    procedure TestMultiplication;
    procedure TestDivision;
  end;

procedure TTestMathOperations.TestAddition;
begin
  Check(1 + 1 = 2, '1+1 should equal 2');
  Check(Add(10, 20) = 30, 'Add function failed');
  CheckEqual(Add(-5, 5), 0, 'Adding opposites');
end;
```

### 25.2.3. TSynTests

Runs a suite of test cases:

```pascal
type
  TMyTestSuite = class(TSynTests)
  published
    procedure AllTests;
  end;

procedure TMyTestSuite.AllTests;
begin
  AddCase([
    TTestMathOperations,
    TTestStringOperations,
    TTestDatabaseOperations
  ]);
end;

// Main program
begin
  with TMyTestSuite.Create('My Application Tests') do
  try
    Run;
    Readln;
  finally
    Free;
  end;
end.
```

---

## 25.3. Check Methods

### 25.3.1. Basic Assertions

```pascal
// Boolean check
Check(Value = Expected, 'Error message');

// Equality checks
CheckEqual(Actual, Expected, 'Error message');
CheckNotEqual(Actual, Unexpected, 'Error message');

// Floating-point comparison (with tolerance)
CheckSame(FloatValue, ExpectedFloat, 'Floating point error');

// UTF-8 string comparison
CheckUtf8(ActualStr, ExpectedStr, 'String mismatch');

// Hash comparison
CheckHash(ActualHash, ExpectedHash, 'Hash mismatch');
```

### 25.3.2. Exception Testing

```pascal
procedure TTestErrors.TestExceptionRaised;
begin
  // Verify exception is raised
  CheckRaised(
    procedure begin
      raise EInvalidOperation.Create('Test');
    end,
    EInvalidOperation,
    'Expected exception not raised'
  );
end;
```

### 25.3.3. Custom Checks

```pascal
procedure TTestCustomer.TestOrderValidation;
var
  Order: TOrder;
begin
  Order := TOrder.Create;
  try
    // Multiple checks
    Check(Order.Items.Count = 0, 'New order should be empty');
    Order.AddItem(1, 2, 10.00);
    CheckEqual(Order.Items.Count, 1, 'Should have one item');
    CheckSame(Order.TotalAmount, 20.00, 'Total should be 20.00');
  finally
    Order.Free;
  end;
end;
```

---

## 25.4. Test Output

### 25.4.1. Console Output

```pascal
var
  Suite: TMyTestSuite;
begin
  Suite := TMyTestSuite.Create('Test Suite');
  try
    Suite.Run;
  finally
    Suite.Free;
  end;
end;
```

Output example:
```
Test Suite

1. Math Operations
  - Addition: 3 assertions passed  12.5 us
  - Multiplication: 5 assertions passed  8.2 us
  - Division: 4 assertions passed  6.1 us

2. String Operations
  - Concatenation: 10 assertions passed  25.3 us
  - Parsing: 8 assertions passed  18.7 us

Total: 30 assertions passed in 2 test cases
```

### 25.4.2. Test Options

```pascal
type
  TSynTestOptions = set of (
    tcoLogEachCheck,        // Log each Check() call
    tcoLogInSubFolder,      // Put logs in ./log subfolder
    tcoLogVerboseRotate,    // Rotate large log files
    tcoLogNotHighResolution // Use plain ISO-8601 timestamps
  );

// Configure
Suite.Options := [tcoLogEachCheck, tcoLogInSubFolder];
```

---

## 25.5. Testing with Logging

### 25.5.1. TSynTestsLogged

Combines testing with logging:

```pascal
type
  TMyLoggedTests = class(TSynTestsLogged)
  published
    procedure AllTests;
  end;

begin
  with TMyLoggedTests.Create('Logged Tests') do
  try
    Run;
  finally
    Free;
  end;
end;
```

### 25.5.2. Log Levels in Tests

```pascal
procedure TTestWithLogging.TestDatabaseConnection;
begin
  TSynLog.Enter(self, 'TestDatabaseConnection');

  Log.Log(sllInfo, 'Connecting to database...');
  // Test code...
  Log.Log(sllDebug, 'Connection established');

  Check(Connected, 'Should be connected');
end;
```

---

## 25.6. Mocking and Stubbing

### 25.6.1. Interface Mocking

```pascal
uses
  mormot.core.interfaces;

type
  ICalculator = interface(IInvokable)
    ['{...}']
    function Add(A, B: Integer): Integer;
    function Multiply(A, B: Integer): Integer;
  end;

procedure TTestWithMocks.TestServiceWithMockedDependency;
var
  Mock: TInterfaceMock;
  Calculator: ICalculator;
begin
  // Create mock
  Mock := TInterfaceMock.Create(TypeInfo(ICalculator), Calculator, self);

  // Define behavior
  Mock.ExpectsCount('Add', qoEqualTo, 2);         // Expect 2 calls
  Mock.Returns('Add', [10, 20], 30);              // Return 30 for Add(10,20)
  Mock.Returns('Multiply', [], 100);              // Return 100 for any Multiply

  // Use mock
  CheckEqual(Calculator.Add(10, 20), 30);
  CheckEqual(Calculator.Multiply(5, 5), 100);

  // Verify expectations
  Mock.Verify;
end;
```

### 25.6.2. Stubbing

```pascal
procedure TTestStubs.TestWithStub;
var
  Stub: TInterfaceStub;
  Service: IMyService;
begin
  // Create stub (no verification)
  Stub := TInterfaceStub.Create(TypeInfo(IMyService), Service);

  // Define returns
  Stub.Returns('GetValue', [], 'stubbed value');

  // Use stub
  CheckEqual(Service.GetValue, 'stubbed value');
end;
```

### 25.6.3. Mock Options

```pascal
type
  TInterfaceMockOptions = set of (
    imoMockFailsWillPassTestCase,  // Failures don't fail test
    imoFakeInstanceCreation,       // Create fake objects
    imoLogMethodCallsAndResults    // Log all calls
  );

Mock.Options := [imoLogMethodCallsAndResults];
```

---

## 25.7. Logging with TSynLog

### 25.7.1. Logging Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    Logging Architecture                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌─────────────────┐                                            │
│  │ TSynLogFamily   │  Configuration (levels, rotation, etc.)    │
│  │ (per-class)     │                                            │
│  └────────┬────────┘                                            │
│           │                                                     │
│           ▼                                                     │
│  ┌─────────────────┐                                            │
│  │ TSynLog         │  Logger instance (per-thread)              │
│  │ (per-thread)    │                                            │
│  └────────┬────────┘                                            │
│           │                                                     │
│           ▼                                                     │
│  ┌─────────────────────────────────────────┐                    │
│  │              Log File                    │                   │
│  │  • Automatic rotation                    │                   │
│  │  • Stack traces on errors               │                    │
│  │  • Thread-safe writes                   │                    │
│  └─────────────────────────────────────────┘                    │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### 25.7.2. Basic Logging

```pascal
uses
  mormot.core.log;

// Simple logging
TSynLog.Add.Log(sllInfo, 'Application started');
TSynLog.Add.Log(sllDebug, 'Processing item %', [ItemID]);
TSynLog.Add.Log(sllError, 'Failed to connect: %', [ErrorMessage]);
```

### 25.7.3. Log Levels

```pascal
type
  TSynLogLevel = (
    sllNone,          // No logging
    sllInfo,          // Informational messages
    sllDebug,         // Debug information
    sllTrace,         // Detailed tracing
    sllWarning,       // Warnings
    sllError,         // Errors
    sllEnter,         // Method entry
    sllLeave,         // Method exit
    sllLastError,     // OS last error
    sllException,     // Exception caught
    sllExceptionOS,   // OS exception
    sllMemory,        // Memory allocation
    sllStackTrace,    // Stack trace
    sllFail,          // Test failure
    sllSQL,           // SQL statements
    sllCache,         // Cache operations
    sllResult,        // Method results
    sllDB,            // Database operations
    sllHTTP,          // HTTP traffic
    sllClient,        // Client operations
    sllServer,        // Server operations
    sllServiceCall,   // Service invocations
    sllServiceReturn, // Service returns
    sllUserAuth,      // User authentication
    sllCustom1..4,    // Custom levels
    sllNewRun,        // New run marker
    sllDDDError,      // DDD errors
    sllDDDInfo,       // DDD info
    sllMonitoring     // Monitoring data
  );
```

---

## 25.8. TSynLogFamily Configuration

### 25.8.1. Basic Configuration

```pascal
var
  LogFamily: TSynLogFamily;
begin
  LogFamily := TSynLog.Family;

  // Set log levels
  LogFamily.Level := LOG_VERBOSE;  // All levels

  // Or specific levels
  LogFamily.Level := [sllInfo, sllWarning, sllError, sllException];

  // File settings
  LogFamily.PerThreadLog := ptIdentifiedInOneFile;  // One file, thread IDs
  LogFamily.DestinationPath := 'C:\Logs\';
  LogFamily.FileExistsAction := acAppend;
end;
```

### 25.8.2. Log Rotation

```pascal
// Rotate by size
LogFamily.RotateFileCount := 5;           // Keep 5 files
LogFamily.RotateFileSizeKB := 10240;      // 10MB per file

// Rotate by time
LogFamily.RotateFileDailyAtHour := 0;     // Rotate at midnight

// Archive rotated logs
LogFamily.RotateFileArchiveCompression := acSynLz;  // Compress with SynLZ
```

### 25.8.3. Stack Traces

```pascal
// Enable stack traces for errors
LogFamily.LevelStackTrace := [sllError, sllException, sllExceptionOS];

// Requires .map or .mab file for readable stack traces
// Generate .mab from .map:
// mormot2tests.map -> mormot2tests.mab (much smaller)
```

---

## 25.9. Structured Logging

### 25.9.1. Method Enter/Leave

```pascal
procedure TMyClass.ProcessData(const Data: TData);
begin
  TSynLog.Enter(self, 'ProcessData');  // Logs entry with timestamp

  // Processing...
  TSynLog.Add.Log(sllDebug, 'Processing % bytes', [Length(Data)]);

  // Automatic leave logging on scope exit
end;
```

Output:
```
20230615 14:32:15.123  +    TMyClass.ProcessData
20230615 14:32:15.125       Processing 1024 bytes
20230615 14:32:15.130  -    00.007
```

### 25.9.2. Logging Objects

```pascal
// Log object as JSON
TSynLog.Add.Log(sllDebug, Customer);  // Serializes to JSON

// Log with context
TSynLog.Add.Log(sllInfo, 'Customer loaded: %', [Customer], TypeInfo(TCustomer));
```

### 25.9.3. SQL Logging

```pascal
// Enable SQL logging
LogFamily.Level := LogFamily.Level + [sllSQL, sllDB];

// SQL statements are automatically logged by ORM
// Output:
// 20230615 14:35:22.456  SQL   SELECT * FROM Customer WHERE ID=?
```

---

## 25.10. ISynLog Interface

### 25.10.1. Interface-Based Logging

```pascal
uses
  mormot.core.log;

procedure ProcessWithLogging;
var
  Log: ISynLog;
begin
  Log := TSynLog.Enter(nil, 'ProcessWithLogging');

  Log.Log(sllInfo, 'Starting process');

  try
    // Work...
    Log.Log(sllDebug, 'Step 1 complete');
  except
    on E: Exception do
    begin
      Log.Log(sllException, E);
      raise;
    end;
  end;
end;  // Automatic leave logged
```

### 25.10.2. Dependency Injection with Logging

```pascal
type
  IMyService = interface
    procedure DoWork;
  end;

  TMyService = class(TInterfacedObject, IMyService)
  private
    fLog: ISynLog;
  public
    constructor Create(const aLog: ISynLog);
    procedure DoWork;
  end;

procedure TMyService.DoWork;
begin
  fLog.Log(sllInfo, 'Starting work');
  // ...
end;
```

---

## 25.11. Debug Symbols

### 25.11.1. TDebugFile

For readable stack traces, provide debug symbols:

```pascal
// Delphi: Generate .map file (Project Options > Linker > Map File = Detailed)
// FPC: Compile with -gl flag, or use external .dbg file

// Convert .map to .mab (optimized format)
TDebugFile.Create('myapp.map', true);  // Creates myapp.mab
```

### 25.11.2. .mab File Benefits

| Format | Size (typical) | Load Time |
|--------|---------------|-----------|
| .map | 4-15 MB | Slow |
| .dbg | 10-50 MB | Slow |
| .mab | 200-500 KB | Fast |

---

## 25.12. Exception Logging

### 25.12.1. Global Exception Handler

```pascal
uses
  mormot.core.log;

begin
  // Install global exception handler
  TSynLog.Family.Level := LOG_VERBOSE + [sllExceptionOS];

  // All unhandled exceptions are logged with stack trace
end;
```

### 25.12.2. Manual Exception Logging

```pascal
procedure SafeProcess;
begin
  try
    RiskyOperation;
  except
    on E: Exception do
    begin
      TSynLog.Add.Log(sllException, E);
      // Or with additional context
      TSynLog.Add.Log(sllException, '% during % processing',
        [E.ClassName, OperationName], E);
      raise;
    end;
  end;
end;
```

---

## 25.13. Remote Logging

### 25.13.1. Log to Remote Server

```pascal
uses
  mormot.core.log,
  mormot.net.client;

var
  LogFamily: TSynLogFamily;
begin
  LogFamily := TSynLog.Family;

  // Enable remote logging
  LogFamily.EchoRemoteClient := THttpClientSocket.Create('logserver', '8080');
  LogFamily.EchoRemoteClientOwned := True;
end;
```

### 25.13.2. SysLog Support

```pascal
// Send to SysLog server (RFC 5424)
LogFamily.EchoToSysLog := True;
LogFamily.SysLogFacility := sfLocal0;
```

---

## 25.14. Log File Analysis

### 25.14.1. TSynLogFile

Read and analyze log files:

```pascal
uses
  mormot.core.log;

var
  LogFile: TSynLogFile;
  i: Integer;
begin
  LogFile := TSynLogFile.Create('app.log');
  try
    // Iterate events
    for i := 0 to LogFile.EventCount - 1 do
    begin
      Writeln(LogFile.EventDateTime[i], ': ',
              LogFile.EventLevel[i], ' - ',
              LogFile.EventText[i]);
    end;

    // Get specific level events
    Writeln('Errors: ', LogFile.EventCount(sllError));
  finally
    LogFile.Free;
  end;
end;
```

### 25.14.2. LogView Tool

mORMot provides a visual log viewer:
- Located in `ex/logview/`
- Features: filtering, search, statistics
- Supports all log formats

---

## 25.15. Performance Considerations

### 25.15.1. Conditional Logging

```pascal
// Use conditional to avoid string formatting overhead
if sllDebug in TSynLog.Family.Level then
  TSynLog.Add.Log(sllDebug, 'Complex: % + %', [ExpensiveCall1, ExpensiveCall2]);
```

### 25.15.2. Async Logging

```pascal
// Enable async writes (background thread)
LogFamily.BufferSize := 32768;  // 32KB buffer
LogFamily.NoFile := False;
```

### 25.15.3. Production Settings

```pascal
// Production: minimal overhead
LogFamily.Level := [sllWarning, sllError, sllException];
LogFamily.LevelStackTrace := [sllException];
LogFamily.RotateFileCount := 10;
LogFamily.RotateFileSizeKB := 20480;  // 20MB

// Development: verbose
LogFamily.Level := LOG_VERBOSE;
LogFamily.LevelStackTrace := [sllError, sllException, sllExceptionOS];
```

---

## 25.16. Summary

### 25.16.1. Testing Quick Reference

| Class | Purpose |
|-------|---------|
| `TSynTestCase` | Individual test case |
| `TSynTests` | Test suite runner |
| `TSynTestsLogged` | Suite with logging |
| `TInterfaceMock` | Interface mocking |
| `TInterfaceStub` | Interface stubbing |

| Method | Purpose |
|--------|---------|
| `Check()` | Boolean assertion |
| `CheckEqual()` | Equality assertion |
| `CheckSame()` | Float comparison |
| `CheckRaised()` | Exception testing |

### 25.16.2. Logging Quick Reference

| Class | Purpose |
|-------|---------|
| `TSynLog` | Logger instance |
| `TSynLogFamily` | Logger configuration |
| `ISynLog` | Logger interface |
| `TSynLogFile` | Log file reader |
| `TDebugFile` | Debug symbols |

| Level | Use For |
|-------|---------|
| `sllInfo` | Informational messages |
| `sllDebug` | Debug output |
| `sllWarning` | Warnings |
| `sllError` | Errors |
| `sllException` | Exceptions |
| `sllSQL` | SQL statements |
| `sllHTTP` | HTTP traffic |

### 25.16.3. Key Units

| Unit | Purpose |
|------|---------|
| `mormot.core.test` | Testing framework |
| `mormot.core.log` | Logging framework |
| `mormot.core.interfaces` | Mocking support |

---

*This concludes the mORMot2 SAD Guide. For additional information, consult the source code documentation and the official mORMot forum.*

---

## Navigation

| Previous | Index | Next |
|----------|-------|------|
| [Chapter 24: Domain-Driven Design](mORMot2-SAD-Chapter-24.md) | [Index](mORMot2-SAD-Index.md) | [Chapter 26: Source Code](mORMot2-SAD-Chapter-26.md) |
