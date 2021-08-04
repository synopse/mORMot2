/// regression tests for RESTful SOA core process
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit test.soa.core;

interface

{$I ..\src\mormot.defines.inc}

uses
  sysutils,
  contnrs,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.unicode,
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.crypt.core,
  mormot.core.data,
  mormot.core.variants,
  mormot.core.json,
  mormot.core.log,
  mormot.core.perf,
  mormot.core.search,
  mormot.core.mustache,
  mormot.core.test,
  mormot.core.threads,
  mormot.core.interfaces,
  mormot.crypt.jwt,
  mormot.net.client,
  mormot.net.server,
  mormot.net.relay,
  mormot.net.ws.core,
  mormot.net.ws.client,
  mormot.net.ws.server,
  mormot.db.core,
  mormot.db.nosql.bson,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.orm.storage,
  mormot.orm.sqlite3,
  mormot.orm.client,
  mormot.orm.server,
  mormot.soa.core,
  mormot.soa.client,
  mormot.soa.server,
  mormot.rest.core,
  mormot.rest.client,
  mormot.rest.server,
  mormot.rest.memserver,
  mormot.rest.sqlite3,
  mormot.rest.http.client,
  mormot.rest.http.server,
  mormot.db.raw.sqlite3,
  mormot.db.raw.sqlite3.static,
  test.core.data,
  test.core.base,
  test.orm.core;

type
    /// a record used by IComplexCalculator.GetCustomer
  TCustomerData = packed record
    Id: Integer;
    AccountNum: RawUtf8;
    Name: RawUtf8;
    Address: RawUtf8;
  end;

  /// a test interface, used by TTestServiceOrientedArchitecture
  // - to test basic and high-level remote service calls
  ICalculator = interface(IInvokable)
    ['{9A60C8ED-CEB2-4E09-87D4-4A16F496E5FE}']
    /// add two signed 32 bit integers
    function Add(n1, n2: integer): integer;
    /// multiply two signed 64 bit integers
    function Multiply(n1, n2: Int64): Int64;
    /// substract two floating-point values
    function Subtract(n1, n2: double): double;
    /// convert a currency value into text
    procedure ToText(Value: Currency; var Result: RawUtf8);
    /// convert a floating-point value into text
    function ToTextFunc(Value: double): string;
    /// swap two by-reference floating-point values
    // - would validate pointer use instead of XMM1/XMM2 registers under Win64
    procedure Swap(var n1, n2: double);
    // test unaligned stack access
    function StackIntMultiply(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10: integer): Int64;
    // test float stack access
    function StackFloatMultiply(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10: double): Int64;
    /// do some work with strings, sets and enumerates parameters,
    // testing also var (in/out) parameters and set as a function result
    function SpecialCall(Txt: RawUtf8; var Int: integer; var Card: cardinal;
      field: TRttiParserComplexTypes; fields: TRttiParserComplexTypes;
      var options: TServiceInstanceImplementations): TRttiParserComplexTypes;
    /// test integer, strings and wide strings dynamic arrays, together with records
    function ComplexCall(const Ints: TIntegerDynArray;
      const Strs1: TRawUtf8DynArray; var Str2: TWideStringDynArray;
      const Rec1: TVirtualTableModuleProperties; var Rec2: TRestCacheEntryValue;
      Float1: double; var Float2: double): TRestCacheEntryValue;
    /// validates ArgsInputIsOctetStream raw binary upload
    function DirectCall(const Data: RawBlob): integer;
    /// validates huge RawJson/RawUtf8
    function RepeatJsonArray(const item: RawUtf8; count: integer): RawJson;
    function RepeatTextArray(const item: RawUtf8; count: integer): RawUtf8;
  end;

  /// a test interface, used by TTestServiceOrientedArchitecture
  // - to test remote service calls with objects as parameters (its published
  // properties will be serialized as standard Json objects)
  // - since it inherits from ICalculator interface, it will also test
  // the proper interface inheritance handling (i.e. it will test that
  // ICalculator methods are also available)
  IComplexCalculator = interface(ICalculator)
    ['{8D0F3839-056B-4488-A616-986CF8D4DEB7}']
    /// purpose of this method is to substract two complex numbers
    // - using class instances as parameters
    procedure Substract(n1, n2: TComplexNumber; out Result: TComplexNumber);
    /// purpose of this method is to check for boolean handling
    function IsNull(n: TComplexNumber): boolean;
    /// this will test the BLOB kind of remote answer
    function TestBlob(n: TComplexNumber): TServiceCustomAnswer;
    /// test variant kind of parameters
    function TestVariants(const Text: RawUtf8; V1: Variant;
      var V2: variant): variant;
    /// test in/out collections
    procedure Collections(Item: TCollTest; var List: TCollTestsI;
      out Copy: TCollTestsI);
    /// returns the thread ID running the method on server side
    function GetCurrentThreadID: PtrUInt;
    /// validate record transmission
    function GetCustomer(CustomerId: Integer;
      out CustomerData: TCustomerData): Boolean;
    //// validate TOrm transmission
    procedure FillPeople(var People: TOrmPeople);
    {$ifndef CPUAARCH64} // FPC doesn't follow the AARCH64 ABI -> fixme
    {$ifndef HASNOSTATICRTTI}
    /// validate simple record transmission
    // - older Delphi versions (e.g. 6-7-2009) do not allow records without
    // nested reference-counted types
    // - CPUAARCH64 has troubles with TConsultNav size and trigger GPF when
    // returned as function result -> Echo is an "out" parameter here
    function EchoRecord(const Nav: TConsultaNav): TConsultaNav;
    {$endif HASNOSTATICRTTI}
    {$endif CPUAARCH64}
  end;

  /// a test interface, used by TTestServiceOrientedArchitecture
  // - to test sicClientDriven implementation pattern: data will remain on
  // the server until the IComplexNumber instance is out of scope
  IComplexNumber = interface(IInvokable)
    ['{29D753B2-E7EF-41B3-B7C3-827FEB082DC1}']
    procedure Assign(aReal, aImaginary: double);
    function GetImaginary: double;
    function GetReal: double;
    procedure SetImaginary(const Value: double);
    procedure SetReal(const Value: double);
    procedure Add(aReal, aImaginary: double);
    property Real: double
      read GetReal write SetReal;
    property Imaginary: double
      read GetImaginary write SetImaginary;
  end;

  /// a test interface, used by TTestServiceOrientedArchitecture
  // - to test sicPerUser implementation pattern
  ITestUser = interface(IInvokable)
    ['{EABB42BF-FD08-444A-BF9C-6B73FA4C4788}']
    function GetContextSessionID: integer;
    function GetContextSessionUser: integer;
    function GetContextSessionGroup: integer;
  end;

  /// a test interface, used by TTestServiceOrientedArchitecture
  // - to test sicPerGroup implementation pattern
  ITestGroup = interface(ITestUser)
    ['{DCBA5A38-62CC-4A52-8639-E709B31DDCE1}']
  end;

  /// a test interface, used by TTestServiceOrientedArchitecture
  // - to test sicPerSession implementation pattern
  ITestSession = interface(ITestUser)
    ['{5237A687-C0B2-46BA-9F39-BEEA7C3AA6A9}']
  end;

  /// a test interface, used by TTestServiceOrientedArchitecture
  // - to test threading implementation pattern
  ITestPerThread = interface(IInvokable)
    ['{202B6C9F-FCCB-488D-A425-5472554FD9B1}']
    function GetContextServiceInstanceID: PtrUInt;
    function GetThreadIDAtCreation: PtrUInt;
    function GetCurrentThreadID: PtrUInt;
    function GetCurrentRunningThreadID: PtrUInt;
  end;

  /// a test value object, used by IUserRepository/ISmsSender interfaces
  // - to test stubing/mocking implementation pattern
  TUser = record
    Name: RawUtf8;
    Password: RawUtf8;
    MobilePhoneNumber: RawUtf8;
    ID: Integer;
  end;

  /// a test interface, used by TTestServiceOrientedArchitecture
  // - to test stubing/mocking implementation pattern
  IUserRepository = interface(IInvokable)
    ['{B21E5B21-28F4-4874-8446-BD0B06DAA07F}']
    function GetUserByName(const Name: RawUtf8): TUser;
    procedure Save(const User: TUser);
  end;

  /// a test interface, used by TTestServiceOrientedArchitecture
  // - to test stubing/mocking implementation pattern
  ISmsSender = interface(IInvokable)
    ['{8F87CB56-5E2F-437E-B2E6-B3020835DC61}']
    function Send(const Text, Number: RawUtf8): boolean;
  end;

const
  IID_ICalculator: TGUID = '{9A60C8ED-CEB2-4E09-87D4-4A16F496E5FE}';

type
  TTestServiceInstances = record
    I: ICalculator;
    CC: IComplexCalculator;
    CN: IComplexNumber;
    CU: ITestUser;
    CG: ITestGroup;
    CS: ITestSession;
    CT: ITestPerThread;
    ExpectedSessionID: integer;
    ExpectedUserID: integer;
    ExpectedGroupID: integer;
  end;

  /// a test case which will test the interface-based SOA implementation of
  // the mORMot framework
  TTestServiceOrientedArchitecture = class(TSynTestCase)
  protected
    fModel: TOrmModel;
    fClient: TRestClientDB;
    procedure Test(const Inst: TTestServiceInstances; Iterations: Cardinal = 700);
    procedure ClientTest(aRouting: TRestServerUriContextClass;
      aAsJsonObject: boolean; aRunInOtherThread: boolean = false;
      aOptions: TInterfaceMethodOptions = []);
    procedure ClientAlgo(algo: TRestAuthenticationSignedUriAlgo);
    class procedure CustomReader(var Context: TJsonParserContext; Data: pointer);
    class procedure CustomWriter(W: TTextWriter; Data: pointer;
      Options: TTextWriterWriteObjectOptions);
    procedure SetOptions(aAsJsonObject: boolean; aOptions: TInterfaceMethodOptions);
    procedure IntSubtractJson(Ctxt: TOnInterfaceStubExecuteParamsJson);
    procedure IntSubtractVariant(Ctxt: TOnInterfaceStubExecuteParamsVariant);
    procedure IntSubtractVariantVoid(Ctxt: TOnInterfaceStubExecuteParamsVariant);
    /// release used instances (e.g. http server) and memory
    procedure CleanUp; override;
  public
  published
    /// test the SetWeak/SetWeakZero weak interface functions
    procedure WeakInterfaces;
    /// initialize the SOA implementation
    procedure ServiceInitialization;
    /// test direct call to the class instance
    procedure DirectCall;
    /// test the server-side implementation
    procedure ServerSide;
    /// test the client-side implementation in RESTful mode
    procedure ClientSideREST;
    /// test the client-side in RESTful mode with values transmitted as Json objects
    procedure ClientSideRESTAsJsonObject;
    /// test the client-side in RESTful mode with full session statistics
    procedure ClientSideRESTSessionsStats;
    /// test the client-side implementation of optExecLockedPerInterface
    procedure ClientSideRESTLocked;
    /// test the client-side implementation of opt*InMainThread option
    procedure ClientSideRESTMainThread;
    /// test the client-side implementation of opt*InPerInterfaceThread option
    procedure ClientSideRESTBackgroundThread;
    /// test the client-side implementation with crc32c URI signature
    procedure ClientSideRESTSignWithCrc32c;
    /// test the client-side implementation with xxHash32 URI signature
    procedure ClientSideRESTSignWithXxhash;
    /// test the client-side implementation with MD5 URI signature
    procedure ClientSideRESTSignWithMd5;
    /// test the client-side implementation with SHA256 URI signature
    procedure ClientSideRESTSignWithSha256;
    /// test the client-side implementation with SHA512 URI signature
    procedure ClientSideRESTSignWithSha512;
    /// test the client-side implementation using TRestServerAuthenticationNone
    procedure ClientSideRESTWeakAuthentication;
    /// test the client-side implementation using TRestServerAuthenticationHttpBasic
    procedure ClientSideRESTBasicAuthentication;
    /// test the custom record Json serialization
    procedure ClientSideRESTCustomRecordLayout;
    /// test the client-side in RESTful mode with all calls logged in a table
    procedure ClientSideRESTServiceLogToDB;
    /// test the client-side implementation in Json-RPC mode
    procedure ClientSideJsonRPC;
    /// test REStful mode using HTTP client/server communication
    procedure TestOverHTTP;
    /// test the security features
    procedure Security;
    /// test interface stubbing / mocking
    procedure MocksAndStubs;
  end;



implementation

{ TServiceCalculator }

type
  TServiceCalculator = class(TInjectableObject, ICalculator)
  public
    function Add(n1, n2: integer): integer;
    function Subtract(n1, n2: double): double;
    procedure Swap(var n1, n2: double);
    function Multiply(n1, n2: Int64): Int64;
    procedure ToText(Value: Currency; var Result: RawUtf8);
    function ToTextFunc(Value: double): string;
    function StackIntMultiply(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10: integer): Int64;
    function StackFloatMultiply(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10: double): Int64;
    function SpecialCall(Txt: RawUtf8; var Int: integer; var Card: cardinal;
      field: TRttiParserComplexTypes; fields: TRttiParserComplexTypes;
      var options: TServiceInstanceImplementations): TRttiParserComplexTypes;
    function ComplexCall(const Ints: TIntegerDynArray;
      const Strs1: TRawUtf8DynArray; var Str2: TWideStringDynArray;
      const Rec1: TVirtualTableModuleProperties; var Rec2: TRestCacheEntryValue;
      Float1: double; var Float2: double): TRestCacheEntryValue;
    function DirectCall(const Data: RawBlob): integer;
    function RepeatJsonArray(const item: RawUtf8; count: integer): RawJson;
    function RepeatTextArray(const item: RawUtf8; count: integer): RawUtf8;
    function Test(A, B: Integer): RawUtf8;
  end;

  TServiceComplexCalculator = class(TServiceCalculator, IComplexCalculator)
  protected
    procedure EnsureInExpectedThread;
  public
    procedure Substract(n1, n2: TComplexNumber; out Result: TComplexNumber);
    function IsNull(n: TComplexNumber): boolean;
    function TestBlob(n: TComplexNumber): TServiceCustomAnswer;
    function TestVariants(const Text: RawUtf8;
      V1: Variant; var V2: variant): variant;
    procedure Collections(Item: TCollTest; var List: TCollTestsI;
      out Copy: TCollTestsI);
    destructor Destroy; override;
    function GetCurrentThreadID: PtrUInt;
    function EchoRecord(const Nav: TConsultaNav): TConsultaNav;
    function GetCustomer(CustomerId: Integer;
      out CustomerData: TCustomerData): Boolean;
    procedure FillPeople(var People: TOrmPeople);
  end;

  TServiceComplexNumber = class(TInterfacedObject, IComplexNumber)
  private
    fReal: double;
    fImaginary: double;
    function GetImaginary: double;
    function GetReal: double;
    procedure SetImaginary(const Value: double);
    procedure SetReal(const Value: double);
  public
    procedure Assign(aReal, aImaginary: double);
    procedure Add(aReal, aImaginary: double);
    property Real: double
      read GetReal write SetReal;
    property Imaginary: double
      read GetImaginary write SetImaginary;
  end;

  TServiceUserGroupSession = class(TInterfacedObject, ITestUser, ITestGroup, ITestSession)
  public
    function GetContextSessionID: integer;
    function GetContextSessionUser: integer;
    function GetContextSessionGroup: integer;
  end;

  TServicePerThread = class(TInterfacedObjectWithCustomCreate, ITestPerThread)
  protected
    fThreadIDAtCreation: PtrUInt; // TThreadID  = ^TThreadRec under BSD
  public
    constructor Create; override;
    function GetContextServiceInstanceID: PtrUInt;
    function GetThreadIDAtCreation: PtrUInt;
    function GetCurrentThreadID: PtrUInt;
    function GetCurrentRunningThreadID: PtrUInt;
  end;

function TServiceCalculator.Add(n1, n2: integer): integer;
begin
  result := n1 + n2;
end;

function TServiceCalculator.Multiply(n1, n2: Int64): Int64;
begin
  result := n1 * n2;
end;

function TServiceCalculator.StackIntMultiply(
  n1, n2, n3, n4, n5, n6, n7, n8, n9, n10: integer): Int64;
begin
  result := n1 * n2 * n3 * n4 * n5 * n6 * n7 * n8 * n9 * n10;
end;

function TServiceCalculator.StackFloatMultiply(
  n1, n2, n3, n4, n5, n6, n7, n8, n9, n10: double): Int64;
begin
  result := round(n1 * n2 * n3 * n4 * n5 * n6 * n7 * n8 * n9 * n10);
end;

function TServiceCalculator.SpecialCall(Txt: RawUtf8; var Int: integer;
  var Card: cardinal; field, fields: TRttiParserComplexTypes;
  var options: TServiceInstanceImplementations): TRttiParserComplexTypes;
var
  dummy: IComplexNumber;
begin
  TryResolve(TypeInfo(IComplexNumber), dummy);
  inc(Int, length(Txt));
  inc(Card);
  result := fields + field;
  Include(options, sicClientDriven);
  Exclude(options, sicSingle);
end;

function TServiceCalculator.Subtract(n1, n2: double): double;
begin
  result := n1 - n2;
end;

procedure TServiceCalculator.Swap(var n1, n2: double);
var
  tmp: double;
begin
  tmp := n2;
  n2 := n1;
  n1 := tmp;
end;

function TServiceCalculator.Test(A, B: Integer): RawUtf8;
begin
  result := Int32ToUtf8(A + B);
end;

procedure TServiceCalculator.ToText(Value: Currency; var Result: RawUtf8);
begin
  Result := Curr64ToStr(PInt64(@Value)^);
end;

function TServiceCalculator.ToTextFunc(Value: double): string;
begin
  Result := DoubleToString(Value);
end;

function TServiceCalculator.ComplexCall(const Ints: TIntegerDynArray;
  const Strs1: TRawUtf8DynArray; var Str2: TWideStringDynArray;
  const Rec1: TVirtualTableModuleProperties; var Rec2: TRestCacheEntryValue;
  Float1: double; var Float2: double): TRestCacheEntryValue;
var
  i: integer;
begin
  Result := Rec2;
  Result.Json := StringToUtf8(Rec1.FileExtension);
  i := length(Str2);
  SetLength(Str2, i + 1);
  Str2[i] := UTF8ToWideString(RawUtf8ArrayToCSV(Strs1));
  inc(Rec2.ID);
  dec(Rec2.Timestamp512);
  Rec2.Json := IntegerDynArrayToCSV(pointer(Ints), length(Ints));
  Float2 := Float1;
end;

function TServiceCalculator.DirectCall(const Data: RawBlob): integer;
var
  i: integer;
begin
  Result := length(Data);
  for i := 1 to Result do
    if Data[i] <> #1 then
      Result := 0;
end;

function TServiceCalculator.RepeatJsonArray(
  const item: RawUtf8; count: integer): RawJson;
var
  buf: array[word] of byte;
begin
  with TTextWriter.CreateOwnedStream(@buf, SizeOf(buf)) do
  try
    Add('[');
    while count > 0 do
    begin
      Add('"');
      AddJsonEscape(pointer(item));
      Add('"', ',');
      dec(count);
    end;
    CancelLastComma;
    Add(']');
    SetText(RawUtf8(Result));
  finally
    Free;
  end;
end;

function TServiceCalculator.RepeatTextArray(
  const item: RawUtf8; count: integer): RawUtf8;
var
  buf: array[word] of byte;
begin
  with TTextWriter.CreateOwnedStream(@buf, SizeOf(buf)) do
  try
    while count > 0 do
    begin
      AddJsonEscape(pointer(item));
      dec(count);
    end;
    SetText(Result);
  finally
    Free;
  end;
end;

var
  GlobalInterfaceTestMode: (
    itmDirect,
    itmClient,
    itmLocked,
    itmMainThread,
    itmPerInterfaceThread,
    itmHttp) = itmDirect;


{ TServiceComplexCalculator }

function TServiceComplexCalculator.IsNull(n: TComplexNumber): boolean;
begin
  result := (n.Real = 0) and (n.Imaginary = 0);
end;

procedure TServiceComplexCalculator.Substract(n1, n2: TComplexNumber;
  out Result: TComplexNumber);
begin
  {%H-}result.Real := n1.Real - n2.Real;
  result.Imaginary := n1.Imaginary - n2.Imaginary;
end;

function TServiceComplexCalculator.EchoRecord(const Nav: TConsultaNav): TConsultaNav;
begin
  result := Nav;
end;

function GetThreadID: PtrUInt;
begin // avoid name conflict with TServiceComplexCalculator.GetCurrentThreadID
  Result := PtrUInt(GetCurrentThreadId);
end;

procedure TServiceComplexCalculator.EnsureInExpectedThread;
begin
  case GlobalInterfaceTestMode of
    itmDirect, itmClient, itmMainThread:
      {$ifdef OSANDROID}
      // On Android, processes never run in the mainthread
      ;
      {$else}
      if GetThreadID <> PtrUInt(MainThreadID) then
        raise Exception.Create('Shall be in main thread');
      {$endif OSANDROID}
    itmPerInterfaceThread, itmHttp, itmLocked:
      if GetThreadID = PtrUInt(MainThreadID) then
        raise Exception.Create('Shall NOT be in main thread')
      else if ServiceRunningContext.RunningThread = nil then
        raise Exception.Create('Shall have a known RunningThread');
  end;
end;

function TServiceComplexCalculator.TestBlob(n: TComplexNumber): TServiceCustomAnswer;
begin
  EnsureInExpectedThread;
  Result.Header := TEXT_CONTENT_TYPE_HEADER;
  if n.Real = maxInt then
    Result.Content := StringOfChar(AnsiChar('-'), 600)
  else
    Result.Content := FormatUtf8('%,%', [n.Real, n.Imaginary]);
end;

function TServiceComplexCalculator.TestVariants(const Text: RawUtf8;
  V1: Variant; var V2: variant): variant;
begin
  V2 := V2 + V1;
  VariantLoadJson(Result, Text);
end;

function TServiceComplexCalculator.GetCurrentThreadID: PtrUInt;
begin
  Result := GetThreadID;
end;

function TServiceComplexCalculator.GetCustomer(CustomerId: Integer;
  out CustomerData: TCustomerData): Boolean;
begin
  CustomerData.Id := CustomerId;
  CustomerData.AccountNum := Int32ToUtf8(CustomerId);
  Result := True;
end;

procedure TServiceComplexCalculator.FillPeople(var People: TOrmPeople);
begin
  People.LastName := FormatUtf8('Last %', [People.ID]);
  People.FirstName := FormatUtf8('First %', [People.ID]);
end;

procedure TServiceComplexCalculator.Collections(Item: TCollTest;
  var List: TCollTestsI; out Copy: TCollTestsI);
begin
  CopyObject(Item, List.Add);
  CopyObject(List, Copy{%H-});
end;

destructor TServiceComplexCalculator.Destroy;
begin
  EnsureInExpectedThread;
  inherited;
end;


{ TServiceComplexNumber }

procedure TServiceComplexNumber.Add(aReal, aImaginary: double);
begin
  fReal := fReal + aReal;
  fImaginary := fImaginary + aImaginary;
end;

procedure TServiceComplexNumber.Assign(aReal, aImaginary: double);
begin
  fReal := aReal;
  fImaginary := aImaginary;
end;

function TServiceComplexNumber.GetImaginary: double;
begin
  Result := fImaginary;
end;

function TServiceComplexNumber.GetReal: double;
begin
  Result := fReal;
end;

procedure TServiceComplexNumber.SetImaginary(const Value: double);
begin
  fImaginary := Value;
end;

procedure TServiceComplexNumber.SetReal(const Value: double);
begin
  fReal := Value;
end;


{ TServiceUserGroupSession }

function TServiceUserGroupSession.GetContextSessionGroup: integer;
begin
  with ServiceRunningContext^ do
    if Request = nil then
      Result := 0
    else
      Result := Request.SessionGroup;
end;

function TServiceUserGroupSession.GetContextSessionID: integer;
begin
  with ServiceRunningContext^ do
    if Request = nil then
      Result := 0
    else
      Result := Request.Session;
end;

function TServiceUserGroupSession.GetContextSessionUser: integer;
begin
  with ServiceRunningContext^ do
    if Request = nil then
      Result := 0
    else
      Result := Request.SessionUser;
end;


{ TServicePerThread }

constructor TServicePerThread.Create;
begin
  inherited;
  fThreadIDAtCreation := PtrUInt(GetThreadID);
end;

function TServicePerThread.GetCurrentThreadID: PtrUInt;
begin
  Result := PtrUInt(GetThreadID);
  with ServiceRunningContext^ do
    if Request <> nil then
      if PtrUInt(Result) <> Request.ServiceInstanceID then
        raise Exception.Create('Unexpected ServiceInstanceID');
end;

function TServicePerThread.GetThreadIDAtCreation: PtrUInt;
begin
  Result := fThreadIDAtCreation;
end;

function TServicePerThread.GetContextServiceInstanceID: PtrUInt;
begin
  with ServiceRunningContext^ do
    if Request = nil then
      Result := 0
    else
    begin
      Result := Request.ServiceInstanceID;
      if Result <> PtrUInt(GetThreadID) then
        raise Exception.Create('Unexpected ThreadID');
    end;
end;

function TServicePerThread.GetCurrentRunningThreadID: PtrUInt;
var
  Thread: TThread;
begin
  Thread := ServiceRunningContext.RunningThread;
  if (Thread = nil) and
     (GlobalInterfaceTestMode = itmHttp) then
    raise Exception.Create('Unexpected Thread=nil');
  if Thread = nil then
    Result := 0
  else
  begin
    Result := PtrUInt(Thread.ThreadID);
    if Result <> PtrUInt(GetThreadID) then
      raise Exception.Create('Unexpected ThreadID');
  end;
end;


{ TTestServiceOrientedArchitecture }

procedure TTestServiceOrientedArchitecture.Test(const Inst:
  TTestServiceInstances; Iterations: Cardinal = 700);

  procedure TestCalculator(const I: ICalculator);
  var
    i1, i2: PtrInt;
    t, i3: integer;
    c: cardinal;
    cu: currency;
    n1, n2, s1, s2: double;
    o: TServiceInstanceImplementations;
    Ints: TIntegerDynArray;
    Strs1: TRawUtf8DynArray;
    Str2: TWideStringDynArray;
    Rec1: TVirtualTableModuleProperties;
    Rec2, RecRes: TRestCacheEntryValue;
    s: RawUtf8;
    r: string;
  begin
    Setlength(Ints, 2);
    CSVToRawUtf8DynArray('one,two,three', Strs1);
    for t := 1 to Iterations do
    begin
      i1 := Random(MaxInt) - Random(MaxInt);
      i2 := Random(MaxInt) - i1;
      Check(I.Add(i1, i2) = i1 + i2);
      Check(I.Multiply(i1, i2) = Int64(i1) * Int64(i2));
      n1 := Random * 1E-9 - Random * 1E-8;
      n2 := n1 * Random;
      CheckSame(I.Subtract(n1, n2), n1 - n2);
      s1 := n1;
      s2 := n2;
      CheckSame(s1, n1);
      CheckSame(s2, n2);
      I.Swap(s1, s2);
      CheckSame(s1, n2);
      CheckSame(s2, n1);
      cu := i1 * 0.01;
      I.ToText(cu, s);
      Check(s = Curr64ToStr(PInt64(@cu)^));
      r := I.ToTextFunc(n1);
      CheckSame(GetExtended(pointer(ToUtf8(r))), n1);
      o := [sicSingle, sicPerGroup];
      i3 := i1;
      c := cardinal(i2);
      Check(I.SpecialCall(s, i3, c, [pctNone], [pctModTime, pctCreateTime], o) =
        [pctModTime, pctCreateTime, pctNone]);
      Check(i3 = i1 + length(s));
      Check(c = cardinal(i2) + 1);
      Check(o = [sicClientDriven, sicPerGroup]);
      Ints[0] := i1;
      Ints[1] := i2;
      SetLength(Str2, 3);
      Str2[0] := 'ABC';
      Str2[1] := 'DEF';
      Str2[2] := 'GHIJK';
      FillCharFast(Rec1, sizeof(Rec1), 0);
      Rec1.Features := [vtTransaction, vtSavePoint];
      Rec1.FileExtension := Executable.ProgramFileName;
      Rec2.ID := i1;
      Rec2.Timestamp512 := c;
      Rec2.Json := 'abc';
      RecRes := I.ComplexCall(Ints, Strs1, Str2, Rec1, Rec2, n1, n2);
      Check(length(Str2) = 4);
      Check(Str2[0] = 'ABC');
      Check(Str2[1] = 'DEF');
      Check(Str2[2] = 'GHIJK');
      Check(Str2[3] = 'one,two,three');
      Check(Rec1.Features = [vtTransaction, vtSavePoint]);
      Check(Rec1.FileExtension = Executable.ProgramFileName);
      Check(Rec2.ID = i1 + 1);
      Check(Rec2.Timestamp512 = c - 1);
      Check(Rec2.Json = IntegerDynArrayToCSV(pointer(Ints), length(Ints)));
      Check(RecRes.ID = i1);
      Check(RecRes.Timestamp512 = c);
      Check(RecRes.Json = StringToUtf8(Rec1.FileExtension));
      CheckSame(n1, n2);
      Rec1.FileExtension := ''; // to avoid memory leak
    end;
    n1 := 0;
    RecRes := I.ComplexCall(Ints, nil, Str2, Rec1, Rec2, n1, n2);
    Check(length(Str2) = 5);
    Check(Str2[0] = 'ABC');
    Check(Str2[1] = 'DEF');
    Check(Str2[2] = 'GHIJK');
    Check(Str2[3] = 'one,two,three');
    Check(Str2[4] = '');
    s := StringToUtf8(StringOfChar(#1, 100));
    check(I.DirectCall(s) = 100);
    s := StringToUtf8(StringOfChar('-', 600));
    t := length(I.RepeatJsonArray(s, 100));
    checkutf8(t = 1 + 100 * 603, 'RawJson %', [KB(t)]);
    t := length(I.RepeatTextArray(s, 100));
    checkutf8(t = 100 * 600, 'RawUtf8 %', [KB(t)]);
  end;

var
  s: RawUtf8;
  data: TCustomerData;
  people: TOrmPeople;
  cust: TServiceCustomAnswer;
  c: cardinal;
  n1, n2: double;
  C1, C2, C3: TComplexNumber;
  Item: TCollTest;
  List, Copy: TCollTestsI;
  j: integer;
  x, y: PtrUInt; // TThreadID  = ^TThreadRec under BSD
  V1, V2, V3: variant;
  {$ifndef HASNOSTATICRTTI}
  Nav, Nav2: TConsultaNav;
  {$endif HASNOSTATICRTTI}
begin
  Check(Inst.I.Add(1, 2) = 3);
  Check(Inst.I.Multiply($1111333, $222266667) = $24693E8DB170B85);
  Check(Inst.I.StackIntMultiply(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) = 3628800);
  Check(Inst.I.StackFloatMultiply(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) = 3628800);
  CheckSame(Inst.I.Subtract(23, 20), 3);
  Inst.I.ToText(3.14, s);
  Check(s = '3.14');
  Check(Inst.I.ToTextFunc(777) = '777');
  x := Inst.CT.GetCurrentThreadID;
  if GlobalInterfaceTestMode <> itmHttp then
  begin
    y := Inst.CT.GetThreadIDAtCreation;
    Check(x = y);
  end;
  case GlobalInterfaceTestMode of
    itmMainThread:
      Check(Inst.CC.GetCurrentThreadID = PtrUInt(MainThreadID));
    itmPerInterfaceThread, itmLocked:
      Check(Inst.CC.GetCurrentThreadID <> PtrUInt(MainThreadID));
  end;
  TestCalculator(Inst.I);
  TestCalculator(Inst.CC); // test the fact that CC inherits from ICalculator
  C3 := TComplexNumber.Create(0, 0);
  C1 := TComplexNumber.Create(2, 3);
  C2 := TComplexNumber.Create(20, 30);
  List := TCollTestsI.Create;
  Copy := TCollTestsI.Create;
  Item := TCollTest.Create(nil);
  try
    Check(Inst.CC.IsNull(C3));
    for c := 0 to Iterations do
    begin
      Check(not Inst.CC.IsNull(C1));
      C3.Imaginary := 0;
      Inst.CC.Substract(C1, C2, C3);
      CheckSame(C3.Real, c - 18.0);
      CheckSame(C3.Imaginary, -27);
      cust := Inst.CC.TestBlob(C3);
      Check(PosEx(TEXT_CONTENT_TYPE_HEADER, cust.Header) > 0);
      Check(cust.Content = FormatUtf8('%,%', [C3.Real, C3.Imaginary]));
      V1 := C3.Real;
      V2 := c;
      case c mod 3 of
        0:
          s := DoubleToStr(C3.Real);
        1:
          s := Int32ToUtf8(c);
        2:
          s := QuotedStr(Int32ToUtf8(c), '"');
      end;
      V3 := Inst.CC.TestVariants(s, V1, V2);
      CheckSame(V1, C3.Real);
      CheckSame(V2, C3.Real + c);
      Check(VariantSaveJson(V3) = s);
      Check(Inst.CC.GetCustomer(c, data));
      Check(data.Id = integer(c));
      Check(GetCardinal(pointer(data.AccountNum)) = c);
      people := TOrmPeople.Create;
      try
        people.IDValue := c;
        Inst.CC.FillPeople(people);
        Check(people.ID = c);
        Check(people.LastName = FormatUtf8('Last %', [c]));
        Check(people.FirstName = FormatUtf8('First %', [c]));
      finally
        people.Free;
      end;
      {$ifndef CPUAARCH64} // FPC doesn't follow the AARCH64 ABI -> fixme
      {$ifndef HASNOSTATICRTTI}
      Nav.MaxRows := c;
      Nav.Row0 := c * 2;
      Nav.RowCount := c * 3;
      Nav.IsSQLUpdateBack := c and 1 = 0;
      Nav.EOF := c and 1 = 1;
      Nav2 := Inst.CC.EchoRecord(Nav);
      Check(Nav2.MaxRows = c);
      Check(Nav2.Row0 = c * 2);
      Check(Nav2.RowCount = c * 3);
      Check(Nav2.IsSQLUpdateBack = (c and 1 = 0));
      Check(Nav2.EOF = (c and 1 = 1));
      {$endif HASNOSTATICRTTI}
      {$endif CPUAARCH64}
      if c mod 10 = 1 then
      begin
        Item.Color := Item.Color + 1;
        Item.Length := Item.Color * 2;
        Item.Name := Int32ToUtf8(Item.Color);
        Inst.CC.Collections(Item, List, Copy);
      end;
      if not CheckFailed(List.Count = Item.Color) or
         not CheckFailed(Copy.Count = List.Count) then
        for j := 0 to List.Count - 1 do
        begin
          with TCollTest(List.Items[j]) do
          begin
            Check(Color = j + 1);
            Check(Length = Color * 2);
            Check(GetInteger(pointer(Name)) = Color);
          end;
          with TCollTest(Copy.Items[j]) do
          begin
            Check(Color = j + 1);
            Check(Length = Color * 2);
            Check(GetInteger(pointer(Name)) = Color);
          end;
        end;
      C1.Real := C1.Real + 1;
    end;
    C3.Real := maxInt; // magic value for huge content
    cust := Inst.CC.TestBlob(C3);
    j := length(cust.Content);
    checkutf8(j = 600, 'TestBlob len=%', [j]);
  finally
    C3.Free;
    C1.Free;
    C2.Free;
    Item.Free;
    List.Free;
    Copy.Free;
  end;
  n2 := Inst.CN.Imaginary;
  for c := 0 to Iterations shr 2 do
  begin
    CheckSame(Inst.CN.Imaginary, n2, 1E-9);
    n1 := Random * 1000;
    Inst.CN.Real := n1;
    CheckSame(Inst.CN.Real, n1);
    CheckSame(Inst.CN.Imaginary, n2, 1E-9);
    n2 := Random * 1000;
    Inst.CN.Imaginary := n2;
    CheckSame(Inst.CN.Real, n1);
    CheckSame(Inst.CN.Imaginary, n2, 1E-9);
    Inst.CN.Add(1, 2);
    CheckSame(Inst.CN.Real, n1 + 1, 1E-9);
    n2 := n2 + 2;
    CheckSame(Inst.CN.Imaginary, n2, 1E-9);
  end;
  Inst.CN.Assign(3.14, 1.05946);
  CheckSame(Inst.CN.Real, 3.14);
  CheckSame(Inst.CN.Imaginary, 1.05946);
  Check(Inst.CU.GetContextSessionID = Inst.ExpectedSessionID);
  Check(Inst.CG.GetContextSessionGroup = Inst.ExpectedGroupID);
  Check(Inst.CS.GetContextSessionUser = Inst.ExpectedUserID);
  x := Inst.CT.GetCurrentThreadID;
  y := Inst.CT.GetThreadIDAtCreation;
  case GlobalInterfaceTestMode of
    itmDirect:
      begin
        Check(x = y);
        Check(Inst.CT.GetCurrentRunningThreadID = 0);
        Check(Inst.CT.GetContextServiceInstanceID = 0);
      end;
    itmClient, itmPerInterfaceThread:
      begin
        Check(x = y);
        Check(Inst.CT.GetCurrentRunningThreadID = 0);
        Check(Inst.CT.GetContextServiceInstanceID <> 0);
      end;
    itmLocked, itmMainThread:
      begin
        Check(x = y);
        Check(Inst.CT.GetCurrentRunningThreadID <> 0);
        Check(Inst.CT.GetContextServiceInstanceID <> 0);
      end;
    itmHttp:
      begin
        Check(Inst.CT.GetCurrentRunningThreadID <> 0);
        Check(Inst.CT.GetCurrentThreadID <> PtrUInt(MainThreadID));
        Check(Inst.CT.GetContextServiceInstanceID <> 0);
      end;
  end;
end;

procedure TTestServiceOrientedArchitecture.SetOptions(aAsJsonObject: boolean;
  aOptions: TInterfaceMethodOptions);
var
  s: integer;
begin
  with fClient.Server.Services do
    for s := 0 to count - 1 do
      with Index(s) as TServiceFactoryServer do
      begin
        ResultAsJsonObject := aAsJsonObject;
        if InterfaceTypeInfo <> TypeInfo(ITestPerThread) then
          SetOptions([], aOptions);
      end;
end;

procedure TTestServiceOrientedArchitecture.ClientTest(aRouting:
  TRestServerUriContextClass; aAsJsonObject: boolean; aRunInOtherThread: boolean;
  aOptions: TInterfaceMethodOptions);
var
  Inst: TTestServiceInstances;
  O: TObject;
  sign: RawUtf8;
  stat: TSynMonitorInputOutput;
begin
 // exit;
  FillCharFast(Inst, sizeof(Inst), 0);
  GlobalInterfaceTestMode := itmClient;
  if aRunInOtherThread then
    if optExecLockedPerInterface in aOptions then
      GlobalInterfaceTestMode := itmLocked
    else if optExecInMainThread in aOptions then
      GlobalInterfaceTestMode := itmMainThread
    else if optExecInPerInterfaceThread in aOptions then
      GlobalInterfaceTestMode := itmPerInterfaceThread;
  (fClient.Services['Calculator'] as TServiceFactoryClient).
    ParamsAsJsonObject := aAsJsonObject;
  SetOptions(aAsJsonObject, aOptions);
  fClient.Server.ServicesRouting := aRouting;
  fClient.ServicesRouting := aRouting.ClientRouting;
  (fClient.Server.Services as TServiceContainerServer).PublishSignature := true;
  sign := fClient.Services['Calculator'].RetrieveSignature;
  Check(sign = fClient.Server.Services['Calculator'].RetrieveSignature);
  (fClient.Server.Services as TServiceContainerServer).PublishSignature := false;
  Check(fClient.Services['Calculator'].RetrieveSignature = '');
  // once registered, can be accessed by its GUID or URI
  if CheckFailed(
       fClient.Services.Info(TypeInfo(ICalculator)).Get(Inst.I)) or
     CheckFailed(
       fClient.Services.Info(TypeInfo(IComplexCalculator)).Get(Inst.CC)) or
     CheckFailed(
       fClient.Services.Info(TypeInfo(IComplexNumber)).Get(Inst.CN)) or
     CheckFailed(
       fClient.Services.Info(TypeInfo(ITestUser)).Get(Inst.CU)) or
     CheckFailed(
       fClient.Services.Info(TypeInfo(ITestSession)).Get(Inst.CS)) or
     CheckFailed(
       fClient.Services.Info(TypeInfo(ITestGroup)).Get(Inst.CG)) or
     CheckFailed(
       fClient.Services.Info(TypeInfo(ITestPerThread)).Get(Inst.CT)) then
    exit;
  O := ObjectFromInterface(Inst.I);
  Check((O <> nil) and
        (Copy(O.ClassName, 1, 21) = 'TInterfacedObjectFake'));
  Inst.ExpectedSessionID := fClient.SessionID;
  if CheckFailed(fClient.SessionUser <> nil) then
    exit;
  fClient.Orm.Retrieve('LogonName=?', [], [fClient.SessionUser.LogonName],
    fClient.SessionUser);
  Inst.ExpectedUserID := fClient.SessionUser.ID;
  Inst.ExpectedGroupID := fClient.SessionUser.GroupRights.ID;
  Test(Inst);
  Inst.I := nil;
  if CheckFailed(fClient.Services.Info(ICalculator).Get(Inst.I)) then
    exit;
  Test(Inst);
  Inst.I := nil;
  if CheckFailed(fClient.Services.Resolve(ICalculator, Inst.I)) then
    exit;
  Test(Inst);
  Finalize(Inst);
  if CheckFailed(fClient.Services['Calculator'].Get(Inst.I)) or
     CheckFailed(fClient.Services['ComplexCalculator'].Get(Inst.CC)) or
     CheckFailed(fClient.Services['ComplexNumber'].Get(Inst.CN)) or
     CheckFailed(fClient.Services['TestUser'].Get(Inst.CU)) or
     CheckFailed(fClient.Services['TestSession'].Get(Inst.CS)) or
     CheckFailed(fClient.Services['TestGroup'].Get(Inst.CG)) or
     CheckFailed(fClient.Services['testperthread'].Get(Inst.CT)) then
    exit;
  {$ifndef CPUARM}
  // The FPC arm optimizer ruins a return address at level -O2
  // So, disable this test until a suitable fix is found.
  Inst.CN.Imaginary;
  {$endif CPUARM}
  Test(Inst);
  SetOptions(false, []);
  stat := (fClient.Server.Services['Calculator'] as TServiceFactoryServer).stat['ToText'];
  Check(stat.TaskCount > 0);
end;

procedure TTestServiceOrientedArchitecture.DirectCall;
var
  Inst: TTestServiceInstances;
begin
  FillCharFast(Inst, sizeof(Inst), 0); // all Expected..ID=0
  Inst.I := TServiceCalculator.Create;
  Inst.CC := TServiceComplexCalculator.Create;
  Inst.CN := TServiceComplexNumber.Create;
  Inst.CS := TServiceUserGroupSession.Create;
  Inst.CG := TServiceUserGroupSession.Create;
  Inst.CU := TServiceUserGroupSession.Create;
  Inst.CT := TServicePerThread.Create;
  Test(Inst);
  Test(Inst);
  Test(Inst);
end;

procedure TTestServiceOrientedArchitecture.ServerSide;
var
  Inst: TTestServiceInstances;
begin
  FillCharFast(Inst, sizeof(Inst), 0); // all Expected..ID=0
  if CheckFailed(fModel <> nil) or
     CheckFailed(fClient <> nil) or
     CheckFailed(fClient.Server.Services.Count = 7) or
     CheckFailed(fClient.Server.Services.Index(0).Get(Inst.I)) or
     CheckFailed(Assigned(Inst.I)) or
     CheckFailed(fClient.Server.Services.Info(
       TypeInfo(ICalculator)).Get(Inst.I)) or
     CheckFailed(fClient.Server.Services.Info(
       TypeInfo(IComplexCalculator)).Get(Inst.CC)) or
     CheckFailed(fClient.Server.Services.Info(
       TypeInfo(IComplexNumber)).Get(Inst.CN)) or
     CheckFailed(fClient.Server.Services.Info(
       TypeInfo(ITestUser)).Get(Inst.CU)) or
     CheckFailed(fClient.Server.Services.Info(
       TypeInfo(ITestSession)).Get(Inst.CS)) or
     CheckFailed(fClient.Server.Services.Info(
       TypeInfo(ITestGroup)).Get(Inst.CG)) or
     CheckFailed(fClient.Server.Services.Info(
       TypeInfo(ITestPerThread)).Get(Inst.CT)) then
    exit;
  Test(Inst);
  Finalize(Inst);
  Check(Inst.I = nil);
  if CheckFailed(fClient.Server.Services['Calculator'].Get(Inst.I)) or
     CheckFailed(fClient.Server.Services['ComplexCalculator'].Get(Inst.CC)) or
     CheckFailed(fClient.Server.Services['ComplexNumber'].Get(Inst.CN)) or
     CheckFailed(fClient.Server.Services['TestUser'].Get(Inst.CU)) or
     CheckFailed(fClient.Server.Services['TestSession'].Get(Inst.CS)) or
     CheckFailed(fClient.Server.Services['TestGroup'].Get(Inst.CG)) or
     CheckFailed(fClient.Server.Services['TestPerThread'].Get(Inst.CT)) then
    exit;
  Test(Inst);
  Test(Inst);
end;

procedure TTestServiceOrientedArchitecture.ServiceInitialization;

  function Ask(Method, Params, ParamsURI, ParamsObj: RawUtf8;
    ExpectedResult: integer): RawUtf8;
  var
    resp, data, uriencoded, head: RawUtf8;
  begin
    Params := ' [ ' + Params + ' ]'; // add some ' ' to test real-world values
    uriencoded := '?' + UrlEncode(Params);
    if fClient.Server.ServicesRouting = TRestServerRoutingRest then
    begin
      SetString(data, PAnsiChar(pointer(Params)), length(Params)); // =UniqueString
      CheckEqual(fClient.URI(
        'root/calculator.' + Method, 'POST', @resp, nil, @data),
        ExpectedResult);
      if ExpectedResult = 200 then
      begin
        CheckEqual(fClient.URI(
          'root/CALCulator.' + Method + uriencoded, 'POST', @data),
          ExpectedResult);
        CheckEqual(data, resp, 'alternative URI-encoded-inlined parameters use');
        CheckEqual(fClient.URI(
          'root/Calculator.' + Method + '?' + ParamsURI, 'GET', @data),
          ExpectedResult);
        CheckEqual(data, resp,
          'alternative "param1=value1&param2=value2" URI-encoded scheme');
        CheckEqual(fClient.URI(
          'root/Calculator.' + Method + '/1234?' + ParamsURI, 'GET', @data),
          ExpectedResult);
        CheckEqual(data, resp, 'alternative URI-encoded scheme with ClientDrivenID');
        SetString(data, PAnsiChar(pointer(Params)), length(Params)); // =UniqueString
        CheckEqual(fClient.URI(
          'root/calculator/' + Method, 'POST', @data, nil, @data),
          ExpectedResult);
        CheckEqual(data, resp, 'interface/method routing');
        SetString(data, PAnsiChar(pointer(Params)), length(Params)); // =UniqueString
        CheckEqual(fClient.URI(
          'root/calculator/' + Method + '/123', 'POST', @data, nil, @Params),
          ExpectedResult);
        CheckEqual(data, resp, 'interface/method/clientdrivenID routing');
        CheckEqual(fClient.URI(
          'root/CALCulator/' + Method + uriencoded, 'POST', @data),
          ExpectedResult);
        CheckEqual(data, resp, 'alternative URI-encoded-inlined parameters use');
        CheckEqual(fClient.URI(
           'root/Calculator/' + Method + '?' + ParamsURI, 'GET', @data),
           ExpectedResult);
        CheckEqual(data, resp,
          'alternative "param1=value1&param2=value2" URI-encoded scheme');
        SetString(data, PAnsiChar(pointer(ParamsObj)), length(ParamsObj)); // =UniqueString
        CheckEqual(fClient.URI(
          'root/calculator/' + Method, 'POST', @data, nil, @data),
          ExpectedResult);
        CheckEqual(data, resp, 'alternative object-encoded-as-body parameters use');
        head := 'accept: application/xml';
        CheckEqual(fClient.URI(
          'root/Calculator/' + Method + '?' + ParamsURI, 'GET', @data, @head),
          ExpectedResult);
        Check(data <> resp, 'returned as XML');
        CheckEqual(head, XML_CONTENT_TYPE_HEADER);
        Check(IdemPChar(pointer(data), '<?XML'), 'returned as XML');
      end;
    end
    else if fClient.Server.ServicesRouting = TRestServerRoutingJsonRpc then
    begin
      data := '{"method":"' + Method + '", "params":' + Params + '}';
      CheckEqual(fClient.URI(
        'root/calculator', 'POST', @resp, nil, @data), ExpectedResult);
    end
    else
      raise Exception.Create('Invalid call');
    Result := JsonDecode(resp, 'result', nil, true);
    if IdemPChar(Pointer(Result), '{"RESULT"') then
      Result := JsonDecode(Result, 'result', nil, false)
    else
      Result := Copy(Result, 2, length(Result) - 2); // trim '[' + ']'
    if (Result <> '') and
       (Result[1] = '"') then
      Result := UnQuoteSQLString(Result); // '"777"' -> '777'
    if (ExpectedResult = 200) and
       (fClient.Server.ServicesRouting = TRestServerRoutingRest) then
    begin
      resp := XMLUTF8_HEADER + '<result><Result>' + Result + '</Result></result>';
      check(data = resp);
    end;
  end;

var
  S: TServiceFactory;
  i: integer;
  rout: integer;
  resp: RawUtf8;
const
  ROUTING: array[0..1] of TRestServerURIContextClass = (
    TRestServerRoutingRest, TRestServerRoutingJsonRpc);
const
  ExpectedURI: array[0..5] of RawUtf8 = (
    'Add', 'Multiply', 'Subtract', 'ToText', 'ToTextFunc', 'Swap');
  ExpectedParCount: array[0..5] of Integer = (
    4, 4, 4, 3, 3, 3);
  ExpectedArgs: array[0..5] of TInterfaceMethodValueTypes = (
    [imvSelf, imvInteger],
    [imvSelf, imvInt64],
    [imvSelf, imvDouble],
    [imvSelf, imvCurrency, imvRawUtf8],
    [imvSelf, imvDouble, imvString],
    [imvSelf, imvDouble]);
  ExpectedTypes: array[0..4] of string[10] = (
    'Integer', 'Int64', 'Double', 'Currency', 'Double');
  ExpectedType: array[0..5] of TInterfaceMethodValueType = (
    imvInteger, imvInt64, imvDouble, imvCurrency, imvDouble, imvDouble);
  ExpectedResult: array[0..2] of string[10] = (
    'Integer', 'Int64', 'Double');
begin
  if CheckFailed(fModel = nil) then
    exit; // should be called once
  // create model, client and server
  fModel := TOrmModel.Create([TOrmPeople, TAuthUser, TAuthGroup]);
  fClient := TRestClientDB.Create(fModel, nil, WorkDir + 'test.db3', TRestServerDB, true);
  fClient.Server.Server.CreateMissingTables; // if tests are run with no db
  Check(fClient.SetUser('User', 'synopse'), 'default user for Security tests');
  Check(fClient.Server.ServiceRegister(TServiceCalculator,
    [TypeInfo(ICalculator)], sicShared) <> nil,
    'register TServiceCalculator as the ICalculator implementation on the server');
  // verify ICalculator RTTI-generated details
  Check(fClient.Server.Services <> nil);
  if CheckFailed(fClient.Server.Services.Count = 1) then
    exit;
  S := fClient.Server.Services.Index(0);
  if CheckFailed(S <> nil) then
    exit;
  Check(S.InterfaceURI = 'Calculator');
  Check(S.InstanceCreation = sicShared);
  Check(S.InterfaceTypeInfo^.Kind = rkInterface);
  Check(S.InterfaceTypeInfo^.Name^ = 'ICalculator');
  Check(GUIDToString(S.InterfaceIID) = '{9A60C8ED-CEB2-4E09-87D4-4A16F496E5FE}');
  Check(GUIDToRawUtf8(S.InterfaceIID) = '{9A60C8ED-CEB2-4E09-87D4-4A16F496E5FE}');
  Check(S.InterfaceMangledURI = '7chgmrLOCU6H1EoW9Jbl_g');
  fClient.Server.Services.ExpectMangledURI := true;
  Check(fClient.Server.Services[S.InterfaceMangledURI] = S);
  fClient.Server.Services.ExpectMangledURI := false;
  Check(fClient.Server.Services['Calculator'] = S);
  Check(fClient.Server.Services['CALCULAtor'] = S);
  Check(fClient.Server.Services['CALCULAtors'] = nil);
  if CheckFailed(length(S.InterfaceFactory.Methods) = 13) then
    exit;
  //JsonReformatToFile(S.Contract, 'contract.json');
  //FileFromString(S.ContractHash, 'contract.hash');
  CheckEqual(S.ContractHash, '"BD1262EAFD23820E"');
  Check(TServiceCalculator(nil).Test(1, 2) = '3');
  Check(TServiceCalculator(nil).ToTextFunc(777) = '777');
  for i := 0 to high(ExpectedURI) do // SpecialCall interface not checked
    with S.InterfaceFactory.Methods[i] do
    begin
      Check(URI = ExpectedURI[i]);
      Check(length(Args) = ExpectedParCount[i]);
      Check(ArgsUsed = ExpectedArgs[i]);
      Check(Args[0].ParamName^ = 'Self');
      Check(Args[0].ValueDirection = imdConst);
      Check(Args[0].ValueType = imvSelf);
      Check(Args[0].ArgTypeName^ = 'ICalculator');
      Check(Args[1].ValueType = ExpectedType[i]);
      if i < 3 then
      begin
        // 0 function Add(n1,n2: integer): integer;
        // 1 function Multiply(n1,n2: Int64): Int64;
        // 2 function Subtract(n1,n2: double): double;
        Check(Args[1].ParamName^ = 'n1');
        Check(Args[1].ValueDirection = imdConst);
        Check(Args[2].ParamName^ = 'n2');
        Check(Args[2].ValueDirection = imdConst);
        Check(Args[2].ValueType = ExpectedType[i]);
        Check(IdemPropName(Args[3].ArgTypeName^, ExpectedTypes[i]),
          string(Args[3].ArgTypeName^));
        Check(Args[3].ValueDirection = imdResult);
        Check(Args[3].ValueType = ExpectedType[i]);
      end
      else if i < 5 then
      begin
        // 3 procedure ToText(Value: Currency; var Result: RawUtf8);
        // 4 function ToTextFunc(Value: double): string;
        Check(Args[1].ParamName^ = 'Value');
        Check(Args[1].ValueDirection = imdConst);
        Check(Args[2].ParamName^ = 'Result');
        if i < 4 then
          Check(Args[2].ValueDirection = imdVar)
        else
          Check(Args[2].ValueDirection = imdResult);
        if i < 4 then
          Check(Args[2].ValueType = imvRawUtf8)
        else
          Check(Args[2].ValueType = imvString);
      end
      else
      begin
        // 5 procedure Swap(var n1,n2: double);
        Check(Args[1].ParamName^ = 'n1');
        Check(Args[1].ValueDirection = imdVar);
        Check(Args[2].ParamName^ = 'n2');
        Check(Args[2].ValueDirection = imdVar);
      end;
    end;
  // IComplexCalculator + IComplexNumber services
  Check(fClient.Server.ServiceRegister(
    TServiceComplexCalculator, [TypeInfo(IComplexCalculator)], sicSingle) <> nil);
  Check(fClient.Server.ServiceRegister(
    TServiceComplexNumber, [TypeInfo(IComplexNumber)], sicClientDriven) <> nil);
  Check(fClient.Server.ServiceRegister(
    TServiceUserGroupSession, [TypeInfo(ITestSession)], sicPerSession) <> nil);
  Check(fClient.Server.ServiceRegister(
    TServiceUserGroupSession, [TypeInfo(ITestUser)], sicPerUser) <> nil);
  Check(fClient.Server.ServiceRegister(
    TServiceUserGroupSession, [TypeInfo(ITestGroup)], sicPerGroup) <> nil);
  Check(fClient.Server.ServiceRegister(
    TServicePerThread, [TypeInfo(ITestPerThread)], sicPerThread) <> nil);
  // Json-level access
  for rout := low(ROUTING) to high(ROUTING) do
  begin
    fClient.ServicesRouting := ROUTING[rout].ClientRouting;
    fClient.Server.ServicesRouting := ROUTING[rout];
    if rout = 0 then
      (fClient.Server.Services['Calculator'] as TServiceFactoryServer).
        ResultAsXMLObjectIfAcceptOnlyXML := true;
    CheckEqual(Ask('None', '1,2', 'one=1&two=2',
      '{one:1,two=2}', 400), '');
    CheckEqual(Ask('Add', '1,2', 'n1=1&n2=2',
      '{n1:1,n2:2}', 200), '3');
    CheckEqual(Ask('Add', '1,0', 'n2=1',
      '{n2:1}', 200), '1');
    CheckEqual(Ask('Multiply', '2,3', 'n1=2&n2=3',
      '{n0:"abc",n2:3,m:null,n1:2}', 200), '6');
    CheckEqual(Ask('Subtract', '23,20', 'n2=20&n1=23',
      '{n0:"abc",n2:20,n1:23}', 200), '3');
    CheckEqual(Ask('ToText', '777,"abc"', 'result=abc&value=777',
      '{result:"abc",value=777}', 200), '777');
    CheckEqual(Ask('ToTextFunc', '777', 'value=777',
      '{result:"abc",value=777}', 200), '777');
    if rout = 0 then
      CheckEqual(fClient.URI(
        'root/ComplexCalculator.GetCustomer?CustomerId=John%20Doe', 'POST',
          @resp, nil, nil), 406, 'incorrect input');
  end;
  fClient.ServicesRouting := TRestServerRoutingRest.ClientRouting; // back to default
  fClient.Server.ServicesRouting := TRestServerRoutingRest;
end;

procedure TTestServiceOrientedArchitecture.Security;

  procedure Test(Expected: TOrmFieldTables; const msg: string);

    function Ask(const Method, Params: RawUtf8): RawUtf8;
    var
      resp, data: RawUtf8;
    begin
      data := '{"method":"' + Method + '", "params": [ ' + Params + ' ]}';
      fClient.URI('root/calculator', 'POST', @resp, nil, @data);
      Result := JsonDecode(resp, 'result', nil, true);
    end;

  begin
    Check((Ask('None', '1,2') = ''), msg);
    CheckMatchAny(Ask('Add', '1,2'), ['[3]', '{"Result":3}'],
      true, (1 in Expected), msg);
    CheckMatchAny(Ask('Multiply', '2,3'), ['[6]', '{"Result":6}'],
      true, (2 in Expected), msg);
    CheckMatchAny(Ask('Subtract', '23,20'), ['[3]', '{"Result":3}'],
      true, (3 in Expected), msg);
    CheckMatchAny(Ask('ToText', '777,"abc"'), ['["777"]', '{"Result":"777"}'],
      true, (4 in Expected), msg);
    CheckMatchAny(Ask('ToTextFunc', '777'), ['["777"]', '{"Result":"777"}'],
      true, (5 in Expected), msg);
  end;

var
  S: TServiceFactoryServer;
  GroupID: TID;
  g: TIDDynArray;
begin
  fClient.ServicesRouting := TRestServerRoutingJsonRpc.ClientRouting;
  fClient.Server.ServicesRouting := TRestServerRoutingJsonRpc;
  GroupID := fClient.Orm.MainFieldID(TAuthGroup, 'User');
  Check(GroupID <> 0);
  Check(fClient.Orm.MainFieldIDs(TAuthGroup, ['User', 'Admin'], g));
  Check(length(g) = 2);
  Check((g[0] = GroupID) or (g[1] = GroupID));
  S := fClient.Server.Services['Calculator'] as TServiceFactoryServer;
  Test([1, 2, 3, 4, 5], 'by default, all methods are allowed');
  S.AllowAll;
  Test([1, 2, 3, 4, 5], 'AllowAll should change nothing');
  S.DenyAll;
  Test([], 'DenyAll will reset all settings');
  S.AllowAll;
  Test([1, 2, 3, 4, 5], 'back to full acccess for everybody');
  S.DenyAllByID([GroupID]);
  Test([], 'our current user shall be denied');
  S.AllowAll;
  Test([1, 2, 3, 4, 5], 'restore allowed for everybody');
  S.DenyAllByID([GroupID + 1]);
  Test([1, 2, 3, 4, 5], 'this group ID won''t affect the current user');
  S.DenyByID(['Add'], [GroupID]);
  Test([2, 3, 4, 5], 'exclude a specific method for the current user');
  S.DenyByID(['totext'], [GroupID]);
  Test([2, 3, 5], 'exclude another method for the current user');
  S.AllowByID(['Add'], [GroupID + 1]);
  Test([2, 3, 5], 'this group ID won''t affect the current user');
  S.AllowByID(['Add'], [GroupID]);
  Test([1, 2, 3, 5], 'allow a specific method for the current user');
  S.AllowAllByID([0]);
  Test([1, 2, 3, 5], 'invalid group ID won''t affect the current user');
  S.AllowAllByID([GroupID]);
  Test([1, 2, 3, 4, 5], 'restore allowed for the current user');
  Check(not fClient.SetUser('unknown', 'wrongpass'));
  Test([], 'no authentication -> access denied');
  Check(fClient.SetUser('Admin', 'synopse'));
  Test([1, 2, 3, 4, 5], 'authenticated user');
  S.DenyAll;
  Test([], 'DenyAll works even for admins');
  S.AllowAll;
  Test([1, 2, 3, 4, 5], 'restore allowed for everybody');
  S.AllowAllByName(['Supervisor']);
  Test([1, 2, 3, 4, 5], 'this group name won''t affect the current Admin user');
  S.DenyAllByName(['Supervisor']);
  Test([1, 2, 3, 4, 5], 'this group name won''t affect the current Admin user');
  S.DenyAllByName(['Supervisor', 'Admin']);
  Test([], 'Admin group user was explicitly denied access');
  S.AllowAllByName(['Admin']);
  Test([1, 2, 3, 4, 5], 'restore allowed for current Admin user');
  S.AllowAll;
  Check(fClient.SetUser('User', 'synopse'));
  Test([1, 2, 3, 4, 5], 'restore allowed for everybody');
end;

procedure TTestServiceOrientedArchitecture.ClientSideREST;
begin
  Check(fClient.ServiceRegister([TypeInfo(ICalculator)], sicShared));
  Check(fClient.ServiceRegister([TypeInfo(IComplexCalculator)], sicSingle));
  Check(fClient.ServiceRegister([TypeInfo(ITestSession)], sicPerSession));
  Check(fClient.ServiceRegister([TypeInfo(ITestUser)], sicPerUser));
  Check(fClient.ServiceRegister([TypeInfo(ITestGroup)], sicPerGroup));
  Check(fClient.ServiceRegister([TypeInfo(ITestPerThread)], sicPerThread));
  ClientTest(TRestServerRoutingRest, false);
end;

procedure TTestServiceOrientedArchitecture.ClientSideRESTServiceLogToDB;
var
  Log: TRestServerDB;
begin
  {$ifdef OSDARWIN}
  {$ifdef NOSQLITE3STATIC}
  // due to a very strange error during prepare_v2, this does not (yet) work on Darwin.
  // at least on Darwin with system sqlite 3.7.13
  // however, mORMots own static works perfect
  Check(1 = 0, 'Not (yet) supported on Darwin !!');
  exit;
  {$endif}
  {$endif OSDARWIN}
  DeleteFile(WorkDir + 'servicelog.db');
  Log := TRestServerDB.CreateWithOwnModel([TOrmServiceLog], WorkDir + 'servicelog.db');
  try
    Log.DB.Synchronous := smOff;
    Log.DB.LockingMode := lmExclusive;
    Log.Server.CreateMissingTables;
    (fClient.Server.ServiceContainer as TServiceContainerServer).
      SetServiceLog(Log.Orm);
    ClientTest(TRestServerRoutingRest, false);
  finally
    (fClient.Server.ServiceContainer as TServiceContainerServer).
      SetServiceLog(nil);
    Log.Free;
  end;
end;

procedure TTestServiceOrientedArchitecture.ClientSideRESTSessionsStats;
var
  stats: RawUtf8;
  store: TRestServerDB;
begin
  fClient.Server.StatLevels := SERVERDEFAULTMONITORLEVELS + [mlSessions];
  store := TRestServerDB.CreateWithOwnModel([TOrmMonitorUsage], WorkDir + 'servicestats.db3');
  try
    store.DB.Synchronous := smOff;
    store.DB.LockingMode := lmExclusive;
    store.Server.CreateMissingTables;
    fClient.Server.StatUsage := TSynMonitorUsageRest.Create(store.Orm, 1);
    ClientTest(TRestServerRoutingRest, false);
    fClient.CallBackGet('stat', ['withall', true], stats);
    JsonReformatToFile(stats, WorkDir + 'statsSessions.Json');
    fClient.Server.StatLevels := SERVERDEFAULTMONITORLEVELS;
    fClient.Server.StatUsage := nil;
  finally
    store.Free;
  end;
end;

procedure TTestServiceOrientedArchitecture.ClientSideJsonRPC;
begin
  ClientTest(TRestServerRoutingJsonRpc, false);
end;

procedure TTestServiceOrientedArchitecture.ClientSideRESTAsJsonObject;
begin
  ClientTest(TRestServerRoutingRest, true);
end;

procedure TTestServiceOrientedArchitecture.TestOverHTTP;
var
  HTTPServer: TRestHttpServer;
  HTTPClient: TRestHttpClient;
  Inst: TTestServiceInstances;
  Json: RawUtf8;
  i: integer;
  URI: TRestServerURIDynArray;
const
  SERVICES: array[0..4] of RawUtf8 = (
    'Calculator', 'ComplexCalculator',
    'TestUser', 'TestGroup', 'TestPerThread');
begin
  fClient.Server.ServicesRouting := TRestServerRoutingRest; // back to default
  GlobalInterfaceTestMode := itmHttp;
  HTTPServer := TRestHttpServer.Create(HTTP_DEFAULTPORT, [fClient.Server], '+',
    HTTP_DEFAULT_MODE, 8, secNone);
  try
    FillCharFast(Inst, sizeof(Inst), 0); // all Expected..ID=0
    HTTPClient := TRestHttpClient.Create('127.0.0.1', HTTP_DEFAULTPORT, fModel);
    try
      HTTPClient.ServicePublishOwnInterfaces :=
        fClient.Server.ServicesPublishedInterfaces;
      //HTTPClient.OnIdle := TLoginForm.OnIdleProcess; // from mORMotUILogin
      // HTTPClient.Compression := [hcSynShaAes]; // 350ms (300ms for [])
      Check(HTTPClient.SetUser('User', 'synopse'));
      // register services on the client side
      Check(HTTPClient.ServiceRegister([TypeInfo(ICalculator)], sicShared));
      Check(HTTPClient.ServiceRegister([TypeInfo(IComplexCalculator)], sicSingle));
      Check(HTTPClient.ServiceRegister([TypeInfo(ITestSession)], sicPerSession));
      Check(HTTPClient.ServiceRegister([TypeInfo(ITestUser)], sicPerUser));
      Check(HTTPClient.ServiceRegister([TypeInfo(ITestGroup)], sicPerGroup));
      Check(HTTPClient.ServiceRegister([TypeInfo(ITestPerThread)], sicPerThread));
      // retrieve service instances
      if CheckFailed(HTTPClient.Services.Info(TypeInfo(ICalculator)).
           Get(Inst.I)) or
         CheckFailed(HTTPClient.Services.Info(TypeInfo(IComplexCalculator)).
           Get(Inst.CC)) or
         CheckFailed(HTTPClient.Services.Info(TypeInfo(IComplexNumber)).
           Get(Inst.CN)) or
         CheckFailed(HTTPClient.Services.Info(TypeInfo(ITestUser)).
           Get(Inst.CU)) or
         CheckFailed(HTTPClient.Services.Info(TypeInfo(ITestSession)).
           Get(Inst.CS)) or
         CheckFailed(HTTPClient.Services.Info(TypeInfo(ITestGroup)).
           Get(Inst.CG)) or
         CheckFailed(HTTPClient.Services.Info(TypeInfo(ITestPerThread)).
           Get(Inst.CT)) then
        exit;
      Inst.ExpectedSessionID := HTTPClient.SessionID;
      HTTPClient.Orm.Retrieve('LogonName=?', [],
        [HTTPClient.SessionUser.LogonName], HTTPClient.SessionUser);
      Inst.ExpectedUserID := HTTPClient.SessionUser.ID;
      Inst.ExpectedGroupID := HTTPClient.SessionUser.GroupRights.ID;
      //SetOptions(false{$ifndef LVCL},true,[optExecInMainThread]{$endif});
      CheckEqual(HTTPClient.CallBackGet('stat', ['findservice', 'toto'], Json),
        HTTP_SUCCESS);
      CheckEqual(Json, '[]');
      for i := 0 to High(SERVICES) do
      begin
        CheckEqual(HTTPClient.CallBackGet(
          'stat', ['findservice', SERVICES[i]], Json), HTTP_SUCCESS, 'stat');
        Check(Json <> '[]');
        Check(HTTPClient.ServiceRetrieveAssociated(SERVICES[i], URI));
        Check(length(URI) = 1);
        Check(URI[0].Port = HTTP_DEFAULTPORT);
        Check(URI[0].Root = fClient.Model.Root);
      end;
      Check(HTTPClient.ServiceRetrieveAssociated(IComplexNumber, URI));
      Check(length(URI) = 1);
      Check(HTTPClient.ServiceRetrieveAssociated(ITestSession, URI));
      Check(length(URI) = 1);
      Test(Inst, 100);
      //SetOptions(false{$ifndef LVCL},true,[]{$endif});
    finally
      Finalize(Inst);
      HTTPClient.Free;
    end;
  finally
    HTTPServer.Free;
    GlobalInterfaceTestMode := itmClient;
  end;
end;

procedure TTestServiceOrientedArchitecture.ClientAlgo(algo:
  TRestAuthenticationSignedUriAlgo);
begin
  (fClient.Server.AuthenticationRegister(TRestServerAuthenticationDefault) as
    TRestServerAuthenticationDefault).Algorithm := algo;
  fClient.SetUser('User', 'synopse');
  ClientTest(TRestServerRoutingRest, false);
end;

procedure TTestServiceOrientedArchitecture.ClientSideRESTSignWithCRC32C;
begin
  ClientAlgo(suaCRC32C)
end;

procedure TTestServiceOrientedArchitecture.ClientSideRESTSignWithXXHASH;
begin
  ClientAlgo(suaXXHASH);
end;

procedure TTestServiceOrientedArchitecture.ClientSideRESTSignWithMD5;
begin
  ClientAlgo(suaMD5);
end;

procedure TTestServiceOrientedArchitecture.ClientSideRESTSignWithSHA256;
begin
  ClientAlgo(suaSHA256);
end;

procedure TTestServiceOrientedArchitecture.ClientSideRESTSignWithSHA512;
begin
  ClientAlgo(suaSHA512);
  (fClient.Server.AuthenticationRegister(TRestServerAuthenticationDefault) as
    TRestServerAuthenticationDefault).Algorithm := suaCRC32;
end;

procedure TTestServiceOrientedArchitecture.ClientSideRESTWeakAuthentication;
begin
  fClient.Server.ServicesRouting := TRestServerRoutingJsonRpc; // back to previous
  fClient.Server.AuthenticationUnregister([
    {$ifdef OSWINDOWS}
    TRestServerAuthenticationSspi,
    {$endif OSWINDOWS}
    TRestServerAuthenticationDefault]);
  fClient.Server.AuthenticationRegister(TRestServerAuthenticationNone);
  TRestClientAuthenticationNone.ClientSetUser(fClient, 'User', '');
  ClientTest(TRestServerRoutingRest, false);
  fClient.Server.AuthenticationUnregister(TRestServerAuthenticationNone);
end;

procedure TTestServiceOrientedArchitecture.ClientSideRESTBasicAuthentication;
begin
  fClient.SessionClose;
  fClient.Server.AuthenticationRegister(TRestServerAuthenticationHttpBasic);
  TRestClientAuthenticationHttpBasic.ClientSetUser(fClient, 'User', 'synopse');
  ClientTest(TRestServerRoutingRest, false);
  fClient.Server.AuthenticationUnregister(TRestServerAuthenticationHttpBasic);
  // restore default authentications
  fClient.Server.AuthenticationRegister([
    {$ifdef OSWINDOWS}
    TRestServerAuthenticationSspi,
    {$endif OSWINDOWS}
    TRestServerAuthenticationDefault]);
  fClient.SetUser('User', 'synopse');
end;

procedure TTestServiceOrientedArchitecture.ClientSideRESTCustomRecordLayout;
begin
  TRttiJson.RegisterCustomSerializer(TypeInfo(TRestCacheEntryValue),
    TTestServiceOrientedArchitecture.CustomReader,
    TTestServiceOrientedArchitecture.CustomWriter);
  try
    ClientTest(TRestServerRoutingRest, false);
  finally
    TRttiJson.UnRegisterCustomSerializer(TypeInfo(TRestCacheEntryValue));
  end;
end;

class procedure TTestServiceOrientedArchitecture.CustomReader(var Context:
  TJsonParserContext; Data: pointer);
var
  V: ^TRestCacheEntryValue absolute Data;
  Values: array[0..2] of TValuePUtf8Char;
begin
  // {"ID":1786554763,"Timestamp":323618765,"Json":"D:\\TestSQL3.exe"}
  if Context.ParseObject(['ID', 'Timestamp', 'Json'], @Values) then
  begin
    V.ID := GetInt64(Values[0].Value);
    V.Timestamp512 := Values[1].ToCardinal;
    Values[2].ToUtf8(V.Json);
  end;
end;

class procedure TTestServiceOrientedArchitecture.CustomWriter(W: TTextWriter;
  Data: pointer; Options: TTextWriterWriteObjectOptions);
var
  V: ^TRestCacheEntryValue absolute Data;
begin
  W.AddJsonEscape([
    'ID', V.ID,
    'Timestamp', Int64(V.Timestamp512),
    'Json', V.Json]);
end;

procedure TTestServiceOrientedArchitecture.Cleanup;
var
  stats: RawUtf8;
begin
  if fClient <> nil then
  begin
    fClient.CallBackGet('stat', [
      'withtables', true,
      'withsqlite3', true,
      'withmethods', true,
      'withinterfaces', true,
      'withsessions', true], stats);
    FileFromString(JsonReformat(stats), WorkDir + 'stats.Json');
  end;
  FreeAndNil(fClient);
  FreeAndNil(fModel);
end;


{ TTestThread }

type
  TTestThread = class(TSynThread)
  protected
    options: TInterfaceMethodOptions;
    procedure Execute; override;
  public
    Test: TTestServiceOrientedArchitecture;
  end;

procedure TTestThread.Execute;
begin
  try
    Test.fClient.Server.Run.BeginCurrentThread(self);
    Test.ClientTest(TRestServerRoutingRest, false, true, options);
    Test.fClient.Server.Run.EndCurrentThread(self);
  finally
    Test := nil; // mark tests finished
  end;
end;

procedure TTestServiceOrientedArchitecture.ClientSideRESTMainThread;
begin
  {$ifdef OSANDROID}
  // Tests on Android never run on MainThread
  exit;
  {$endif OSANDROID}
  with TTestThread.Create(true) do
  try
    Test := self;
    options := [optExecInMainThread, optFreeInMainThread];
    Start;
    while Test<>nil do
      if IsMultiThread and
         (GetCurrentThreadID = MainThreadID) then
        CheckSynchronize{$ifndef DELPHI6OROLDER}(1){$endif}
      else
        sleep(1);
  finally
    Free;
  end;
  fClient.Server.ServicesRouting := TRestServerRoutingJsonRpc; // back to previous
end;

procedure TTestServiceOrientedArchitecture.ClientSideRESTBackgroundThread;
begin
  ClientTest(TRestServerRoutingRest, false, true, [optExecInPerInterfaceThread,
    optFreeInPerInterfaceThread]);
  fClient.Server.ServicesRouting := TRestServerRoutingJsonRpc; // back to previous
end;

procedure TTestServiceOrientedArchitecture.ClientSideRESTLocked;
begin
  with TTestThread.Create(true) do
  try
    Test := self;
    options := [optExecLockedPerInterface];
    Start;
    while Test <> nil do
      if IsMultiThread and
         (GetCurrentThreadID=MainThreadID) then
        CheckSynchronize{$ifndef DELPHI6OROLDER}(1){$endif}
      else
        sleep(1);
  finally
    Free;
  end;
  fClient.Server.ServicesRouting := TRestServerRoutingJsonRpc; // back to previous
end;

type
  IChild = interface;

  IParent = interface
    procedure SetChild(const Value: IChild);
    function GetChild: IChild;
    function HasChild: boolean;
    property Child: IChild
      read GetChild write SetChild;
  end;

  IChild = interface
    procedure SetParent(const Value: IParent);
    function GetParent: IParent;
    property Parent: IParent
      read GetParent write SetParent;
  end;

  TParent = class(TInterfacedObject, IParent)
  private
    FChild: IChild;
    procedure SetChild(const Value: IChild);
    function GetChild: IChild;
  public
    destructor Destroy; override;
    function HasChild: boolean;
    property Child: IChild
      read GetChild write SetChild;
  end;

  TChild = class(TInterfacedObject, IChild)
  private
    FParent: IParent;
    procedure SetParent(const Value: IParent);
    function GetParent: IParent;
  public
    constructor Create(const AParent: IParent; SetChild: boolean);
    destructor Destroy; override;
    property Parent: IParent
      read GetParent write SetParent;
  end;

  TUseWeakRef = (direct, weakref, zeroing);

var
  ParentDestroyed, ChildDestroyed: boolean;
  UseWeakRef: TUseWeakRef;

procedure TTestServiceOrientedArchitecture.WeakInterfaces;
var
  Parent: IParent;
  Child, Child2: IChild;
  P: TParent;
  C: TChild;

  procedure Init(aWeakRef: TUseWeakRef);
  begin
    ParentDestroyed := false;
    ChildDestroyed := false;
    UseWeakRef := aWeakRef;
    Check(Parent = nil);
    Check(Child = nil);
    P := TParent.Create;
    Parent := P;
    Check(ObjectFromInterface(Parent) = P);
    C := TChild.Create(Parent, true);
    Child := C;
    Check(ObjectFromInterface(Child) = C);
    Parent.Child := Child;
  end;

  procedure WeakTest(aWeakRef: TUseWeakRef);
  var
    Child2: IChild;
  begin
    Init(aWeakRef);
    Check(ParentDestroyed = false);
    Check(ChildDestroyed = false);
    Child2 := Parent.Child;
    Child2 := nil; // otherwise memory leak, but it is OK
    Check(ChildDestroyed = false);
    Child := nil;
    Check(ChildDestroyed = true);
    Check(ParentDestroyed = false);
    Check(Parent.HasChild = (aWeakRef = weakref), 'ZEROed Weak');
    Parent := nil;
  end;

begin
  Init(direct);
  Parent := nil;
  Check(ParentDestroyed = false);
  Check(ChildDestroyed = false);
  Child := nil;
  Check(ParentDestroyed = false, 'Without weak reference: memory leak');
  Check(ChildDestroyed = false);
  {%H-}P._Release;
  Check(ParentDestroyed = true, 'Manual release');
  Check(ChildDestroyed = true);
  WeakTest(weakref);
  Init(zeroing);
  Check(ParentDestroyed = false);
  Check(ChildDestroyed = false);
  Child2 := Parent.Child;
  Child2 := nil;
  Check(ChildDestroyed = false);
  Parent := nil;
  Check(ParentDestroyed = false);
  Check(ChildDestroyed = false);
  Child := nil;
  Check(ParentDestroyed = true);
  Check(ChildDestroyed = true);
  WeakTest(zeroing);
  Init(zeroing);
  Check(Parent.HasChild);
  Child2 := TChild.Create(Parent, false);
  Check(Parent.HasChild);
  Parent.Child := Child2;
  Check(Parent.HasChild);
  Child2 := nil;
  Check(not Parent.HasChild);
  Check(ChildDestroyed = true);
  ChildDestroyed := false;
  Check(not Parent.HasChild);
  Child := nil;
  Check(ParentDestroyed = false);
  Check(ChildDestroyed = true);
  Check(not Parent.HasChild);
  ChildDestroyed := false;
  Parent := nil;
  Check(ParentDestroyed = true);
  Check(ChildDestroyed = false);
end;


{ TParent }

destructor TParent.Destroy;
begin
  ParentDestroyed := true;
  if UseWeakRef = weakref then
    SetWeak(@FChild, nil);
  inherited;
end;

function TParent.GetChild: IChild;
begin
  Result := FChild;
end;

function TParent.HasChild: boolean;
begin
  Result := FChild <> nil;
end;

procedure TParent.SetChild(const Value: IChild);
begin
  case UseWeakRef of
    direct:
      FChild := Value;
    weakref:
      SetWeak(@FChild, Value);
    zeroing:
      SetWeakZero(self, @FChild, Value);
  end;
end;

{ TChild }

constructor TChild.Create(const AParent: IParent; SetChild: boolean);
begin
  FParent := AParent;
  if SetChild then
    FParent.Child := self;
end;

destructor TChild.Destroy;
begin
  ChildDestroyed := true;
  inherited;
end;

function TChild.GetParent: IParent;
begin
  Result := FParent;
end;

procedure TChild.SetParent(const Value: IParent);
begin
  case UseWeakRef of
    direct:
      FParent := Value;
    weakref:
      SetWeak(@FParent, Value);
    zeroing:
      SetWeakZero(self, @FParent, Value);
  end;
end;

type
  TLoginController = class
  protected
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

procedure TLoginController.ForgotMyPassword(const UserName: RawUtf8);
var
  U: TUser;
begin
  U := fUserRepository.GetUserByName(UserName);
  Assert(U.Name = UserName, 'internal verification');
  U.Password := Int32ToUtf8(Random(MaxInt));
  U.MobilePhoneNumber := Int32ToUtf8(Random(MaxInt));
  if fSmsSender.Send('Your new password is ' + U.Password, U.MobilePhoneNumber) then
    fUserRepository.Save(U);
end;

procedure TTestServiceOrientedArchitecture.IntSubtractJson(
  Ctxt: TOnInterfaceStubExecuteParamsJson);
var
  P: PUtf8Char;
begin
  if Ctxt.Sender is TInterfaceMock then
    Ctxt.TestCase.Check(Ctxt.EventParams = 'toto');
  P := pointer(Ctxt.Params);
  Ctxt.Returns([GetNextItemDouble(P) - GetNextItemDouble(P)]);
  // Ctxt.Result := '['+DoubleToStr(GetNextItemDouble(P)-GetNextItemDouble(P))+']';
end;

procedure TTestServiceOrientedArchitecture.IntSubtractVariant(
  Ctxt: TOnInterfaceStubExecuteParamsVariant);
begin
  if Ctxt.Sender is TInterfaceMock then
    Ctxt.TestCase.Check(Ctxt.EventParams = 'toto');
  Ctxt['result'] := Ctxt['n1'] - Ctxt['n2'];
  // with Ctxt do Output[0] := Input[0]-Input[1];
end;

procedure TTestServiceOrientedArchitecture.IntSubtractVariantVoid(
  Ctxt: TOnInterfaceStubExecuteParamsVariant);
begin
end;

procedure TTestServiceOrientedArchitecture.MocksAndStubs;
var
  I: ICalculator;
  n: integer;
  UserRepository: IUserRepository;
  SmsSender: ISmsSender;
  U: TUser;
  log, UJson: RawUtf8;
  HashGetUserByNameToto: cardinal;
  Stub: TInterfaceStub;
  Mock: TInterfaceMockSpy;
begin
  Stub := TInterfaceStub.Create(TypeInfo(ICalculator), I).
    SetOptions([imoLogMethodCallsAndResults]);
  Check(I.Add(10, 20) = 0, 'Default result');
  log := Stub.LogAsText;
  Check(log = 'Add(10,20)=[0]');
  I := nil;
  Stub := TInterfaceStub.Create(TypeInfo(ICalculator), I).
    Returns('Add', '30').
    Returns('Multiply', [60]).
    Returns('Multiply', [2, 35], [70]).
    ExpectsCount('Multiply', ioEqualTo, 2).
    ExpectsCount('Subtract', ioGreaterThan, 0).
    ExpectsCount('ToTextFunc', ioLessThan, 2).
    ExpectsTrace('Add', Hash32('Add(10,30)=[30]')).
    ExpectsTrace('Multiply', 'Multiply(10,30)=[60],Multiply(2,35)=[70]').
    ExpectsTrace('Multiply', [10, 30], 'Multiply(10,30)=[60]').
    ExpectsTrace('Add(10,30)=[30],Multiply(10,30)=[60],' +
      'Multiply(2,35)=[70],Subtract(2.3,1.2)=[0],ToTextFunc(2.3)=["default"]').
    Returns('ToTextFunc', ['default']);
  Check(I.Add(10, 30) = 30);
  Check(I.Multiply(10, 30) = 60);
  Check(I.Multiply(2, 35) = 70);
  Check(I.Subtract(2.3, 1.2) = 0, 'Default result');
  Check(I.ToTextFunc(2.3) = 'default');
  Check(Stub.LogHash = $34FA7AAF);
  I := nil; // release Stub -> will check all expectations
  TInterfaceMock.Create(TypeInfo(ICalculator), I, self).
    Returns('Add', '30').
    Fails('Add', [1, 2], 'expected failure').
    SetOptions([imoMockFailsWillPassTestCase]). // -> Check(true)
    ExpectsCount('Add', ioEqualTo, 3).
    ExpectsCount('Add', [10, 30], ioNotEqualTo, 1).
    Executes('Subtract', IntSubtractJson, 'toto').
    Returns('Multiply', [60]).
    Returns('Multiply', [2, 35], [70]).
    Returns('ToTextFunc', [2.3], ['two point three']).
    Returns('ToTextFunc', ['default']);
  Check(I.ToTextFunc(2.3) = 'two point three');
  Check(I.ToTextFunc(2.4) = 'default');
  Check(I.Add(10, 30) = 30);
  n := Assertions;
  I.Add(1, 2); // will launch TInterfaceMock.InternalCheck -> Check(true)
  n := Assertions - n; // tricky code due to Check() inlined Assertions modif.
  Check(n = 1, 'test should have passed');
  Check(I.Multiply(10, 30) = 60);
  Check(I.Multiply(2, 35) = 70);
  for n := 1 to 10000 do
    CheckSame(I.Subtract(n * 10.5, n * 0.5), n * 10, 1E-9);
  n := Assertions;
  I := nil; // release TInterfaceMock -> will check all expectations
  n := Assertions - n;
  Check(n = 2, 'Add count<>3');
  TInterfaceStub.Create(TypeInfo(ISmsSender), SmsSender).
    Returns('Send', [true]);
  U.Name := 'toto';
  UJson := RecordSaveJson(U, TypeInfo(TUser));
  HashGetUserByNameToto := Hash32('GetUserByName("toto")=[' + UJson + ']');
  Mock := TInterfaceMockSpy.Create(
    TypeInfo(IUserRepository), UserRepository, self);
  Mock.Returns('GetUserByName', '"toto"', UJson).
    ExpectsCount('GetUserByName', ioEqualTo, 1).
    ExpectsCount('GetUserByName', ['toto'], ioEqualTo, 1).
    ExpectsCount('GetUserByName', '"tata"', ioEqualTo, 0).
    ExpectsTrace('GetUserByName', ['toto'], HashGetUserByNameToto).
    ExpectsTrace('GetUserByName', HashGetUserByNameToto).
    ExpectsCount('Save', ioEqualTo, 1);
  with TLoginController.Create(UserRepository, SmsSender) do
  try
    ForgotMyPassword('toto');
  finally
    Free;
  end;
  Mock.Verify('Save');
  Mock.Verify('GetUserByName', ['toto'], ioEqualTo, 1);
  Mock.Verify('GetUserByName', '"toto"', ioNotEqualTo, 2);
  Mock.Verify('GetUserByName', ['toto'], '[' + UJson + ']');
  UserRepository := nil; // will release TInterfaceMock and check Excepts*()
  SmsSender := nil;
  TInterfaceStub.Create(IID_ICalculator, I).
    Executes('Subtract', IntSubtractVariantVoid, 'titi');
  check(I.Subtract(10, 20) = 0);
  TInterfaceStub.Create(IID_ICalculator, I).
    Returns('Subtract', [10, 20], [3]).
    Executes('Subtract', IntSubtractVariant, 'toto').
    Fails('Add', 'expected exception').
    Raises('Add', [1, 2], ESynException, 'expected exception');
  for n := 1 to 10000 do
    CheckSame(I.Subtract(n * 10.5, n * 0.5), n * 10, 1E-9);
  Check(I.Subtract(10, 20) = 3, 'Explicit result');
  {$WARN SYMBOL_PLATFORM OFF}
  {$ifndef FPC}
  if DebugHook <> 0 then
  {$endif FPC}
    exit; // avoid exceptions in IDE
  {$WARN SYMBOL_PLATFORM ON}
  with TSynLog.Family.ExceptionIgnore do
  begin
    Add(EInterfaceFactory);
    Add(ESynException);
  end;
  try
    I.Add(0, 0);
    Check(false);
  except
    on E: EInterfaceFactory do
      Check(Pos('TInterfaceStub returned error: expected exception',
        E.Message) > 0, E.Message);
  end;
  try
    I.Add(1, 2);
    Check(false);
  except
    on E: ESynException do
      Check(E.Message = 'expected exception', E.Message);
  end;
  with TSynLog.Family.ExceptionIgnore do
  begin
    Delete(IndexOf(EInterfaceFactory));
    Delete(IndexOf(ESynException));
  end;
end;


end.

