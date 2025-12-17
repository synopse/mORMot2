# 15. Interfaces and SOLID Design

*The Foundation for Robust Architecture*

Before diving into interface-based services, we need to understand the fundamentals of interfaces in Delphi and the SOLID design principles that guide their effective use. This chapter establishes the theoretical foundation; Chapter 16 covers the practical implementation of SOA services.

---

## 15.1. Delphi and Interfaces

### 15.1.1. Declaring an Interface

In Delphi's OOP model, an `interface` defines a type comprising abstract virtual methods. It declares "what" is available, not "how" it's implemented — this is the **abstraction** benefit of interfaces.

```pascal
type
  ICalculator = interface(IInvokable)
    ['{9A60C8ED-CEB2-4E09-87D4-4A16F496E5FE}']
    /// add two signed 32-bit integers
    function Add(n1, n2: Integer): Integer;
  end;
```

Key characteristics:
- **Naming**: `ICalculator` starts with `I` (convention for interfaces, vs `T` for classes)
- **No visibility**: All methods are effectively public
- **No fields**: Only methods (fields are implementation details)
- **GUID**: Unique identifier (press `Ctrl+Shift+G` in the IDE to generate)
- **Inheritance**: From `IInvokable` for RTTI support

### 15.1.2. Implementing an Interface

```pascal
type
  TServiceCalculator = class(TInterfacedObject, ICalculator)
  public
    function Add(n1, n2: Integer): Integer;
  end;

function TServiceCalculator.Add(n1, n2: Integer): Integer;
begin
  Result := n1 + n2;
end;
```

Notes:
- The class inherits from `TInterfacedObject` and implements `ICalculator`
- Method visibility in the class doesn't affect interface usage
- Additional methods can exist in the class (not part of the interface)
- Multiple interfaces can be implemented: `class(TInterfacedObject, ICalculator, IAnotherInterface)`

### 15.1.3. Using an Interface

**Classic way** (explicit class instance):
```pascal
function MyAdd(a, b: Integer): Integer;
var
  Calculator: TServiceCalculator;
begin
  Calculator := TServiceCalculator.Create;
  try
    Result := Calculator.Add(a, b);
  finally
    Calculator.Free;
  end;
end;
```

**Interface way** (reference-counted):
```pascal
function MyAdd(a, b: Integer): Integer;
var
  Calculator: ICalculator;
begin
  Calculator := TServiceCalculator.Create;
  Result := Calculator.Add(a, b);
end; // Calculator automatically freed when out of scope
```

Key benefits:
- **Automatic memory management**: Reference counting handles cleanup
- **No try..finally needed**: Compiler generates hidden cleanup code
- **Minimal overhead**: Similar to virtual method call performance

### 15.1.4. Orthogonality and Polymorphism

Interfaces are orthogonal to class implementations:

```pascal
type
  TOtherCalculator = class(TInterfacedObject, ICalculator)
  public
    function Add(n1, n2: Integer): Integer;
  end;

function TOtherCalculator.Add(n1, n2: Integer): Integer;
begin
  Result := n2 + n1; // Different implementation, same interface
end;
```

The client code doesn't need to change:
```pascal
var
  Calculator: ICalculator;
begin
  Calculator := TOtherCalculator.Create; // Different class, same interface
  Result := Calculator.Add(a, b);
end;
```

### 15.1.5. The mORMot Magic

mORMot leverages interfaces for Client-Server communication:

- **Same interface** on both client and server
- **Client**: "Fake" implementation that serializes calls to JSON
- **Server**: Real implementation executes the logic
- **Transport**: JSON over HTTP (or WebSockets, named pipes, etc.)

This creates a seamless RPC experience with the elegance of local interface calls.

---

## 15.2. SOLID Design Principles

The SOLID acronym represents five principles for maintainable OOP design:

| Principle | Summary |
|-----------|---------|
| **S**ingle Responsibility | One reason to change per class |
| **O**pen/Closed | Open for extension, closed for modification |
| **L**iskov Substitution | Subtypes must be substitutable for base types |
| **I**nterface Segregation | Many specific interfaces over one general-purpose |
| **D**ependency Inversion | Depend on abstractions, not concretions |

These principles combat the three main code weaknesses:
- **Rigidity**: Hard to change (changes cascade everywhere)
- **Fragility**: Changes break unexpected parts
- **Immobility**: Hard to reuse in other contexts

### 15.2.1. Single Responsibility Principle

> "A class should have only one reason to change."

**Bad** — `TBarcodeScanner` handles both protocol and communication:
```pascal
type
  TBarcodeScanner = class
    function ReadFrame: TProtocolFrame;
    procedure WriteFrame(const Frame: TProtocolFrame);
    procedure SetComPort(const Port: string);  // Serial communication
    procedure SetUsbDevice(DeviceID: Integer);  // USB communication
  end;
```

**Good** — Separated responsibilities:
```pascal
type
  // Connection abstraction
  TAbstractBarcodeConnection = class
    function ReadChar: Byte; virtual; abstract;
    procedure WriteChar(aChar: Byte); virtual; abstract;
  end;

  // Protocol abstraction
  TAbstractBarcodeProtocol = class
  protected
    fConnection: TAbstractBarcodeConnection;
  public
    function ReadFrame: TProtocolFrame; virtual; abstract;
    procedure WriteFrame(const Frame: TProtocolFrame); virtual; abstract;
  end;

  // Composed scanner
  TBarcodeScanner = class
  protected
    fProtocol: TAbstractBarcodeProtocol;
    fConnection: TAbstractBarcodeConnection;
  public
    property Protocol: TAbstractBarcodeProtocol read fProtocol;
    property Connection: TAbstractBarcodeConnection read fConnection;
  end;
```

#### 15.2.1.1. Don't Mix UI and Logic

**Smell in uses clause**:
```pascal
unit MyDataModel;

uses
  Vcl.Forms,    // BAD: Couples data to GUI framework
  Windows,      // BAD: Couples to operating system
  mormot.orm.core;
```

Keep business logic units free of:
- GUI frameworks (VCL, FMX, LCL)
- Operating system specifics (`Windows`, `Posix`)
- Any visual form units

mORMot framework units follow this principle — `mormot.orm.core.pas` has no GUI dependencies.

### 15.2.2. Open/Closed Principle

> "Software entities should be open for extension, but closed for modification."

**Guidelines**:
- Define abstract classes/interfaces, implement via inheritance
- Members should be `protected` (for inheritance) or `private` (hidden)
- Avoid singletons and global variables
- Use RTTI sparingly and via framework abstractions

**Example — Your code extends mORMot without modifying it**:
```pascal
type
  TMyRestServer = class(TRestServerDB)
  published
    procedure MyCustomService(Ctxt: TRestServerUriContext);
  end;
```

You extend by inheritance, not by editing `mormot.rest.server.pas`.

### 15.2.3. Liskov Substitution Principle

> "Objects of a supertype should be replaceable with objects of any subtype."

**mORMot Example**:
```pascal
var
  Rest: TRest;  // Abstract parent type
begin
  // Either implementation works identically:
  Rest := TRestServerDB.Create(Model, 'mydata.db3');
  // OR
  Rest := TRestHttpClientSocket.Create('server', '8080', Model);

  // Same API regardless of implementation:
  Rest.Orm.Add(MyRecord);
end;
```

**Violations to avoid**:
```pascal
procedure TAbstractScanner.Process;
begin
  // BAD: Type checking breaks substitutability
  if Self is TSerialScanner then
    // Serial-specific code
  else if Self is TUsbScanner then
    // USB-specific code
end;
```

### 15.2.4. Interface Segregation Principle

> "Many client-specific interfaces are better than one general-purpose interface."

**Bad** — Fat interface:
```pascal
type
  IEverything = interface
    procedure DoThis;
    procedure DoThat;
    procedure DoSomethingElse;
    // ... 50 more methods
  end;
```

**Good** — Segregated interfaces:
```pascal
type
  IDoThis = interface
    procedure DoThis;
  end;

  IDoThat = interface
    procedure DoThat;
  end;
```

This is especially important in SOA: define small, focused service interfaces rather than monolithic ones.

### 15.2.5. Dependency Inversion Principle

> "Depend on abstractions, not concretions."

**Bad** — Direct dependency on implementation:
```pascal
type
  TOrderService = class
  private
    fDatabase: TSQLiteDatabase;  // Concrete class
  end;
```

**Good** — Dependency on abstraction:
```pascal
type
  TOrderService = class
  private
    fRepository: IOrderRepository;  // Interface abstraction
  public
    constructor Create(const aRepository: IOrderRepository);
  end;
```

This enables:
- **Testing**: Inject mock implementations
- **Flexibility**: Swap implementations without code changes
- **Decoupling**: No compile-time dependency on concrete classes

---

## 15.3. Circular References and Weak Pointers

### 15.3.1. The Problem

Interface reference counting can cause memory leaks with circular references:

```pascal
type
  IParent = interface
    procedure SetChild(const Value: IChild);
    function GetChild: IChild;
  end;

  IChild = interface
    procedure SetParent(const Value: IParent);
    function GetParent: IParent;
  end;
```

If `Parent.Child` references `Child`, and `Child.Parent` references `Parent`, neither will ever be freed — both maintain a reference count ≥ 1 indefinitely.

### 15.3.2. Weak Pointers

mORMot provides `SetWeak` to bypass reference counting:

```pascal
uses
  mormot.core.interfaces;

procedure TChild.SetParent(const Value: IParent);
begin
  SetWeak(@fParent, Value);  // No reference count increment
end;
```

The child holds a reference to parent, but doesn't prevent parent's destruction.

### 15.3.3. Zeroing Weak Pointers

For safer weak references that automatically become `nil` when the target is freed:

```pascal
procedure TChild.SetParent(const Value: IParent);
begin
  SetWeakZero(Self, @fParent, Value);  // Auto-nils when parent freed
end;
```

When `Parent` is destroyed:
- With `SetWeak`: `fParent` becomes a dangling pointer (dangerous!)
- With `SetWeakZero`: `fParent` automatically becomes `nil` (safe)

---

## 15.4. Dependency Injection in Practice

### 15.4.1. Constructor Injection

The simplest and most explicit form:

```pascal
type
  IUserRepository = interface(IInvokable)
    ['{B21E5B21-28F4-4874-8446-BD0B06DAA07F}']
    function GetUserByName(const Name: RawUtf8): TUser;
    procedure Save(const User: TUser);
  end;

  ISmsSender = interface(IInvokable)
    ['{8F87CB56-5E2F-437E-B2E6-B3020835DC61}']
    function Send(const Text, Number: RawUtf8): Boolean;
  end;

  TLoginController = class(TInterfacedObject, ILoginController)
  private
    fUserRepository: IUserRepository;
    fSmsSender: ISmsSender;
  public
    constructor Create(const aUserRepository: IUserRepository;
      const aSmsSender: ISmsSender);
    procedure ForgotMyPassword(const UserName: RawUtf8);
  end;

constructor TLoginController.Create(const aUserRepository: IUserRepository;
  const aSmsSender: ISmsSender);
begin
  fUserRepository := aUserRepository;
  fSmsSender := aSmsSender;
end;
```

### 15.4.2. TInjectableObject

For automatic resolution of dependencies, inherit from `TInjectableObject`:

```pascal
uses
  mormot.core.interfaces;

type
  TMyService = class(TInjectableObject, IMyService)
  private
    fCalculator: ICalculator;  // Auto-resolved
  published
    property Calculator: ICalculator read fCalculator;
  public
    function DoWork: Integer;
  end;
```

Published interface properties are automatically resolved when the object is created through the DI container.

### 15.4.3. Lazy Resolution

For on-demand resolution:

```pascal
procedure TMyService.DoSomething;
var
  Repository: IOrderRepository;
begin
  Resolve(IOrderRepository, Repository);  // Resolve when needed
  Repository.SaveOrder(Order);
end;
```

---

## 15.5. Stubs and Mocks for Testing

### 15.5.1. Terminology

| Type | Purpose |
|------|---------|
| **Stub** | Fake implementation returning pre-arranged responses |
| **Mock** | Fake that verifies interactions (method calls, parameters) |

**Rule**: One mock per test, multiple stubs as needed.

### 15.5.2. Creating Stubs

```pascal
uses
  mormot.core.interfaces;

procedure TMyTest.TestForgotPassword;
var
  SmsSender: ISmsSender;
  UserRepository: IUserRepository;
begin
  // Create stub that returns true for Send method
  TInterfaceStub.Create(TypeInfo(ISmsSender), SmsSender)
    .Returns('Send', [True]);

  // Create mock that expects Save to be called once
  TInterfaceMock.Create(TypeInfo(IUserRepository), UserRepository, Self)
    .ExpectsCount('Save', qoEqualTo, 1);

  // Run the test
  with TLoginController.Create(UserRepository, SmsSender) do
  try
    ForgotMyPassword('testuser');
  finally
    Free;
  end;
  // Verification happens automatically when UserRepository goes out of scope
end;
```

### 15.5.3. Stub Return Values

Simple returns:
```pascal
TInterfaceStub.Create(TypeInfo(ICalculator), Calc)
  .Returns('Add', [42]);  // Add always returns 42
```

Conditional returns:
```pascal
TInterfaceStub.Create(TypeInfo(ICalculator), Calc)
  .Returns('Add', [1, 2], [3])   // Add(1,2) returns 3
  .Returns('Add', [10, 20], [30]); // Add(10,20) returns 30
```

### 15.5.4. Stub with Custom Logic

Using a callback for complex behavior:

```pascal
procedure TMyTest.SubtractCallback(Ctxt: TOnInterfaceStubExecuteParamsVariant);
begin
  Ctxt['Result'] := Ctxt['n1'] - Ctxt['n2'];
end;

TInterfaceStub.Create(TypeInfo(ICalculator), Calc)
  .Executes('Subtract', SubtractCallback);
```

### 15.5.5. Mock Expectations

```pascal
TInterfaceMock.Create(TypeInfo(ICalculator), Calc, Self)
  // Expect Multiply to be called exactly twice
  .ExpectsCount('Multiply', qoEqualTo, 2)
  // Expect Add to be called at least once
  .ExpectsCount('Add', qoGreaterThan, 0)
  // Expect specific call sequence
  .ExpectsTrace('Add(10,20)=[30],Multiply(5,6)=[30]');
```

### 15.5.6. Test Spy Pattern

For "run then verify" testing:

```pascal
procedure TMyTest.TestCalculator;
var
  Calc: ICalculator;
  Spy: TInterfaceMockSpy;
begin
  Spy := TInterfaceMockSpy.Create(TypeInfo(ICalculator), Calc, Self);

  // Run code under test
  Calc.Add(10, 20);
  Calc.Multiply(5, 6);

  // Verify after execution
  Spy.Verify('Add');
  Spy.Verify('Multiply', [5, 6]);
end;
```

---

## 15.6. Interface Registration

### 15.6.1. Global Registration

Register interfaces at initialization for cleaner code:

```pascal
unit MyInterfaces;

interface

type
  ICalculator = interface(IInvokable)
    ['{9A60C8ED-CEB2-4E09-87D4-4A16F496E5FE}']
    function Add(n1, n2: Integer): Integer;
  end;

  IUserRepository = interface(IInvokable)
    ['{B21E5B21-28F4-4874-8446-BD0B06DAA07F}']
    function GetUserByName(const Name: RawUtf8): TUser;
  end;

implementation

uses
  mormot.core.interfaces;

initialization
  TInterfaceFactory.RegisterInterfaces([
    TypeInfo(ICalculator),
    TypeInfo(IUserRepository)
  ]);
end.
```

### 15.6.2. Using Registered Interfaces

After registration, you can use interface types directly (no `TypeInfo()`):

```pascal
// Instead of:
TInterfaceStub.Create(TypeInfo(ICalculator), Calc);

// You can write:
TInterfaceStub.Create(ICalculator, Calc);
```

---

## Summary

This chapter covered the foundations for interface-based development:

- **Interfaces** provide abstraction and automatic memory management
- **SOLID principles** guide maintainable architecture
- **Weak pointers** solve circular reference problems
- **Dependency injection** enables loose coupling and testability
- **Stubs and mocks** enable isolated unit testing

These concepts are essential for understanding the next chapter, which shows how mORMot uses interfaces to implement powerful SOA services with automatic client stub generation, contract validation, and multiple instance lifetime patterns.

---

## Navigation

| Previous | Index | Next |
|----------|-------|------|
| [Chapter 14: Client-Server Services via Methods](mORMot2-SAD-Chapter-14.md) | [Index](mORMot2-SAD-Index.md) | [Chapter 16: Client-Server Services via Interfaces](mORMot2-SAD-Chapter-16.md) |
