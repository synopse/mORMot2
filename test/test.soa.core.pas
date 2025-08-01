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
  mormot.net.http,
  mormot.net.relay,
  mormot.net.ws.core,
  mormot.net.ws.client,
  mormot.net.ws.server,
  mormot.db.core,
  mormot.db.nosql.bson,
  mormot.orm.base,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.orm.storage,
  mormot.orm.sqlite3,
  mormot.orm.client,
  mormot.orm.server,
  mormot.soa.core,
  mormot.soa.client,
  mormot.soa.server,
  mormot.soa.codegen,
  mormot.rest.core,
  mormot.rest.client,
  mormot.rest.server,
  mormot.rest.memserver,
  mormot.rest.sqlite3,
  mormot.rest.http.client,
  mormot.rest.http.server,
  mormot.rest.mvc,
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

  /// a record which is an Homogeneous Floating-point Aggregate (HFA)
  TCoords = packed record
    X, Y: double;
  end;

  TClientSide = (
    csUndefined, csDirect, csServer,
    csMainThread, csBackground, csJsonObject, csSessions, csLocked,
    csCrc32, csCrc32c, csXxHash, csMd5, csSha1, csSha256, csSha512, csSha3,
    csWeak, csBasic, csDbLog, csJsonRpc, csHttp, csHttpLog, csCustomRtti);

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
    // - would validate pointer use instead of XMM1/XMM2 registers on x86-64
    // - also that /calculator/swap would be processed by ICalculator._Swap()
    procedure _Swap(var n1, n2: double);
    /// test unaligned stack access
    function StackIntMultiply(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10: integer): Int64;
    /// test float stack access
    function StackFloatMultiply(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10: double): Int64;
    /// do some work with strings, sets and enumerates parameters,
    // testing also var (in/out) parameters and set as a function result
    function SpecialCall(Txt: RawUtf8; var Int: integer; var Card: cardinal;
      field: TRttiParserComplexTypes; fields: TRttiParserComplexTypes;
      var options: TServiceInstanceImplementations): TRttiParserComplexTypes;
    /// test integer, strings and wide strings dynamic arrays, together with records
    function ComplexCall(const Ints: TIntegerDynArray;
      const Strs1: TRawUtf8DynArray; var Str2: TWideStringDynArray;
      const Rec1: TVirtualTableModuleProperties; var Rec2: TEntry;
      Float1: double; var Float2: double): TEntry;
    /// a variant is a TVarRec with mixed types so is a pointer even on the SysV ABI
    function VariantCall(const Value: variant): RawUtf8;
    {$ifndef HASNOSTATICRTTI} // Delphi 7/2007 raises "TGuid has no type info"
    /// test small TGuid record and HFA to be passed on registers on the SysV ABI
    function RecordCall(const Uuid: TGuid; const Pos: TCoords): RawJson;
    {$endif HASNOSTATICRTTI}
    /// validates ArgsInputIsOctetStream raw binary upload
    function DirectCall(const Data: RawBlob): integer;
    // validates huge RawJson/RawUtf8
    function RepeatJsonArray(const item: RawUtf8; count: integer): RawJson;
    function RepeatTextArray(const item: RawUtf8; count: integer): RawUtf8;
    // validates IDocList/IDocDict parameters - cannot be in result
    procedure TestDocList(var list: IDocList; const data: variant; out input: IDocList);
    procedure TestDocDict(var dict: IDocDict; const data: variant; out input: IDocDict);
  end;

  /// a test interface, used by TTestServiceOrientedArchitecture
  // - to test remote service calls with objects as parameters (its published
  // properties will be serialized as standard Json objects)
  // - since it inherits from ICalculator interface, it will also test
  // the proper interface inheritance handling (i.e. it will test that
  // ICalculator methods are also available)
  IComplexCalculator = interface(ICalculator)           
    ['{8D0F3839-056B-4488-A616-986CF8D4DEB7}']
    /// customize the server-side execution expectations for this interface
    /// purpose of this method is to substract two complex numbers
    // - using class instances as parameters
    procedure Substract(n1, n2: TComplexNumber; out Result: TComplexNumber);
    /// purpose of this method is to check for boolean handling
    function IsNull(n: TComplexNumber): boolean;
    /// this will test the BLOB kind of remote answer
    function TestBlob(n: TComplexNumber; cs: TClientSide): TServiceCustomAnswer;
    /// test variant kind of parameters
    function TestVariants(const Text: RawUtf8; V1: Variant;
      var V2: variant): variant;
    /// test (maybe huge) RawJson content
    function TestRawJson(len, value: integer; const j: RawJson): RawJson;
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
    /// validate array of TOrm transmission
    procedure FillPeoples(n: integer; out People: TOrmPeopleObjArray);
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
  IID_ICalculator: TGuid = '{9A60C8ED-CEB2-4E09-87D4-4A16F496E5FE}';

type
  TTestServiceInstances = record
    I: ICalculator;
    CC: IComplexCalculator;
    CN: IComplexNumber;
    CU: ITestUser;
    CG: ITestGroup;
    CS: ITestSession;
    CT: ITestPerThread;
    ClientSide: TClientSide;
    ExpectedSessionID: integer;
    ExpectedUserID: integer;
    ExpectedGroupID: integer;
  end;

  TRestClientDBNamed = class(TRestClientDB)
  public
    Name: RawUtf8;
    ClientSide: TClientSide;
  end;

  /// a test case which will test the interface-based SOA implementation of
  // the mORMot framework
  TTestServiceOrientedArchitecture = class(TSynTestCase)
  protected
    fMain: TRestClientDBNamed;
    procedure Test(const Inst: TTestServiceInstances; Iterations: Cardinal = 700);
    procedure TestHttp(aClient: TRestClientDBNamed; withlog: boolean; const port: RawUtf8);
    procedure ClientTest(aClient: TRestClientDBNamed; aRouting: TRestServerUriContextClass;
      aAsJsonObject: boolean; aRunInOtherThread: boolean = false;
      aOptions: TInterfaceMethodOptions = []);
    class procedure CustomReader(var Context: TJsonParserContext; Data: pointer);
    class procedure CustomWriter(W: TJsonWriter; Data: pointer;
      Options: TTextWriterWriteObjectOptions);
    procedure IntSubtractJson(Ctxt: TOnInterfaceStubExecuteParamsJson);
    procedure IntSubtractVariant(Ctxt: TOnInterfaceStubExecuteParamsVariant);
    procedure IntSubtractVariantVoid(Ctxt: TOnInterfaceStubExecuteParamsVariant);
  public
    { all threaded callbacks for validating all client side modes }
    /// test the client-side in RESTful mode with values transmitted as Json objects
    procedure ClientSideRESTAsJsonObject(Sender: TObject);
    /// test the client-side in RESTful mode with full SQlite3 session statistics
    procedure ClientSideRESTSessionsStats(Sender: TObject);
    /// test the client-side implementation with threading options
    procedure ClientSideRESTThread(Sender: TObject);
    /// test the client-side implementation with any hash URI signature
    procedure ClientSideRESTSign(Sender: TObject);
    /// test the client-side implementation using TRestServerAuthentication*
    procedure ClientSideRESTAuth(Sender: TObject);
    /// test the client-side in RESTful mode with all calls logged in a table
    procedure ClientSideRESTServiceLogToDB(Sender: TObject);
    /// test the client-side implementation in Json-RPC mode
    procedure ClientSideJsonRPC(Sender: TObject);
    /// test REStful mode using HTTP client/server communication
    procedure ClientSideOverHTTP(Sender: TObject);
    /// test the custom record Json serialization - could NOT be parallelized
    procedure ClientSideRESTCustomRecord(Client: TRestClientDBNamed);
    /// initialize a new REST server + REST client with SOA implementation
    function NewClient(aClientSide: TClientSide): TRestClientDBNamed;
  published
    /// test the SetWeak/SetWeakZero weak interface functions
    procedure WeakInterfaces;
    /// test direct call to the class instance
    procedure DirectCall;
    /// test the server-side implementation
    procedure ServerSide;
    /// test the security features
    procedure Security;
    /// multi-threaded tests of the client-side implementation in all mode
    procedure ClientSide;
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
    procedure _Swap(var n1, n2: double);
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
      const Rec1: TVirtualTableModuleProperties; var Rec2: TEntry;
      Float1: double; var Float2: double): TEntry;
    function DirectCall(const Data: RawBlob): integer; // not used on Delphi 7/2007
    function VariantCall(const Value: variant): RawUtf8;
    function RecordCall(const Uuid: TGuid; const Pos: TCoords): RawJson;
    function RepeatJsonArray(const item: RawUtf8; count: integer): RawJson;
    function RepeatTextArray(const item: RawUtf8; count: integer): RawUtf8;
    procedure TestDocList(var list: IDocList; const data: variant; out input: IDocList);
    procedure TestDocDict(var dict: IDocDict; const data: variant; out input: IDocDict);
    function Test(A, B: Integer): RawUtf8;
  end;

  TServiceComplexCalculator = class(TServiceCalculator, IComplexCalculator)
  protected
    fExpected: TClientSide;
    fMethodThread: PtrUInt;
    procedure EnsureInExpectedThread;
  public
    procedure Substract(n1, n2: TComplexNumber; out Result: TComplexNumber);
    function IsNull(n: TComplexNumber): boolean;
    function TestBlob(n: TComplexNumber; cs: TClientSide): TServiceCustomAnswer;
    function TestVariants(const Text: RawUtf8;
      V1: Variant; var V2: variant): variant;
    function TestRawJson(len, value: integer; const j: RawJson): RawJson;
    procedure Collections(Item: TCollTest; var List: TCollTestsI;
      out Copy: TCollTestsI);
    destructor Destroy; override;
    function GetCurrentThreadID: PtrUInt;
    function EchoRecord(const Nav: TConsultaNav): TConsultaNav;
    function GetCustomer(CustomerId: Integer;
      out CustomerData: TCustomerData): Boolean;
    procedure FillPeople(var People: TOrmPeople);
    procedure FillPeoples(n: integer; out People: TOrmPeopleObjArray);
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

  TServicePerThread = class(TInterfacedPersistent, ITestPerThread)
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

procedure TServiceCalculator._Swap(var n1, n2: double);
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
  const Rec1: TVirtualTableModuleProperties; var Rec2: TEntry;
  Float1: double; var Float2: double): TEntry;
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

function TServiceCalculator.VariantCall(const Value: variant): RawUtf8;
begin
  VariantToUtf8(Value, result);
end;

function TServiceCalculator.RecordCall(const Uuid: TGuid; const Pos: TCoords): RawJson;
begin
  result := FormatUtf8('["%",%,%]', [GuidToShort(Uuid), Pos.X, Pos.Y]);
end;

function TServiceCalculator.RepeatJsonArray(
  const item: RawUtf8; count: integer): RawJson;
var
  buf: TBuffer64K;
begin
  with TJsonWriter.CreateOwnedStream(@buf, SizeOf(buf)) do
  try
    Add('[');
    while count > 0 do
    begin
      Add('"');
      AddJsonEscape(pointer(item));
      Add('"', ',');
      dec(count);
    end;
    CancelLastComma(']');
    SetText(RawUtf8(Result));
  finally
    Free;
  end;
end;

function TServiceCalculator.RepeatTextArray(
  const item: RawUtf8; count: integer): RawUtf8;
var
  buf: TBuffer64K;
begin
  with TJsonWriter.CreateOwnedStream(@buf, SizeOf(buf)) do
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

procedure TServiceCalculator.TestDocList(var list: IDocList;
  const data: variant; out input: IDocList);
begin
  input := list;
  list := DocList([1, 2, 3, data]);
end;

procedure TServiceCalculator.TestDocDict(var dict: IDocDict;
  const data: variant; out input: IDocDict);
begin
  input := dict;
  dict := DocDict(['a', 1, 'b', 2, 'data', data]);
end;



{ TServiceComplexCalculator }

function GetThreadID: PtrUInt; {$ifdef HASINLINE} inline; {$endif}
begin // avoid name conflict with TServiceComplexCalculator.GetCurrentThreadID
  Result := PtrUInt(GetCurrentThreadId);
end;

function TServiceComplexCalculator.IsNull(n: TComplexNumber): boolean;
begin
  result := (n.Real = 0) and (n.Imaginary = 0);
end;

procedure TServiceComplexCalculator.Substract(n1, n2: TComplexNumber;
  out Result: TComplexNumber);
begin
  if fMethodThread = 0 then
    fMethodThread := GetThreadID;
  {%H-}result.Real := n1.Real - n2.Real;
  result.Imaginary := n1.Imaginary - n2.Imaginary;
end;

function TServiceComplexCalculator.EchoRecord(const Nav: TConsultaNav): TConsultaNav;
begin
  if fMethodThread = 0 then
    fMethodThread := GetThreadID;
  result := Nav;
end;

procedure TServiceComplexCalculator.EnsureInExpectedThread;
var
  name: PShortString;
  thrid: PtrUInt;
begin
  name := GetEnumName(TypeInfo(TClientSide), ord(fExpected));
  thrid := GetThreadID;
  if fMethodThread <> 0 then
    if fMethodThread <> thrid then
      ESynException.RaiseUtf8('%.EnsureInExpectedThread % in #% <> #%',
        [self, name^, thrid, fMethodThread]);
  case fExpected of
    csDirect,
    csServer,
    csMainThread:
      {$ifdef OSANDROID}
      // On Android, processes never run in the mainthread
      ;
      {$else}
      if thrid <> PtrUInt(MainThreadID) then
        ESynException.RaiseUtf8('% shall be in main thread', [name^]);
      {$endif OSANDROID}
    csBackground,
    csHttp,
    csHttpLog:
      if thrid = PtrUInt(MainThreadID) then
        ESynException.RaiseUtf8('% shall NOT be in main thread', [name^])
      else if ServiceRunningContext.RunningThread = nil then
        ESynException.RaiseUtf8('% shall have a known RunningThread', [name^]);
    // other TClientSide could be in main thread or background thread
  end;
end;

function TServiceComplexCalculator.TestBlob(n: TComplexNumber;
  cs: TClientSide): TServiceCustomAnswer;
begin
  if fMethodThread = 0 then
    fMethodThread := GetThreadID;
  fExpected := cs;
  EnsureInExpectedThread;
  Result.Header := TEXT_CONTENT_TYPE_HEADER;
  if n.Real = maxInt then
    Result.Content := RawUtf8OfChar('-', 600)
  else
    Result.Content := FormatUtf8('%,%', [n.Real, n.Imaginary]);
end;

function TServiceComplexCalculator.TestVariants(const Text: RawUtf8;
  V1: Variant; var V2: variant): variant;
begin
  if fMethodThread = 0 then
    fMethodThread := GetThreadID;
  V2 := V2 + V1;
  VariantLoadJson(Result, Text);
end;

const
  _TESTRAWJSON = '["toto"]';

function TServiceComplexCalculator.TestRawJson(
  len, value: integer; const j: RawJson): RawJson;
var
  p: PByteArray;
begin
  if fMethodThread = 0 then
    fMethodThread := GetThreadID;
  if len < 0 then
    len := 0;
  if j <> _TESTRAWJSON then
  begin
    result:= '';
    exit;
  end;
  p := FastSetString(RawUtf8(result), len + 2);
  p[0] := ord('"');
  FillcharFast(p[1], len, value);
  p[len + 1] := ord('"');
end;

function TServiceComplexCalculator.GetCurrentThreadID: PtrUInt;
begin
  Result := GetThreadID;
  fMethodThread := Result;
end;

function TServiceComplexCalculator.GetCustomer(CustomerId: Integer;
  out CustomerData: TCustomerData): Boolean;
begin
  if fMethodThread = 0 then
    fMethodThread := GetThreadID;
  CustomerData.Id := CustomerId;
  CustomerData.AccountNum := Int32ToUtf8(CustomerId);
  Result := True;
end;

procedure TServiceComplexCalculator.FillPeople(var People: TOrmPeople);
begin
  if fMethodThread = 0 then
    fMethodThread := GetThreadID;
  if People.ID = 0 then
    exit; // check transmission of LastName/FirstName as ""
  People.LastName  := FormatUtf8('Last %', [People.ID]);
  People.FirstName := FormatUtf8('First %', [People.ID]);
end;

procedure TServiceComplexCalculator.FillPeoples(
  n: integer; out People: TOrmPeopleObjArray);
var
  i: PtrInt;
  p: TOrmPeople;
begin
  if fMethodThread = 0 then
    fMethodThread := GetThreadID;
  SetLength(People, n);
  for i := 0 to n - 1 do
  begin
    p := TOrmPeople.Create;
    p.IDValue := i;
    p.FirstName := UInt32ToUtf8(i);
    p.LastName := 'Last';
    p.YearOfBirth := 1982 + i;
    p.YearOfDeath := 1992 + i;
    People[i] := p;
  end;
end;

procedure TServiceComplexCalculator.Collections(Item: TCollTest;
  var List: TCollTestsI; out Copy: TCollTestsI);
begin
  if fMethodThread = 0 then
    fMethodThread := GetThreadID;
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
  fThreadIDAtCreation := GetThreadID;
end;

function TServicePerThread.GetCurrentThreadID: PtrUInt;
begin
  result := GetThreadID;
  with ServiceRunningContext^ do
    if Request <> nil then
      if result <> PtrUInt(Request.ServiceInstanceID) then
        ESynException.RaiseUtf8('%.GetCurrentThreadID=%<>%',
          [self, result, Request.ServiceInstanceID]);
end;

function TServicePerThread.GetThreadIDAtCreation: PtrUInt;
begin
  result := fThreadIDAtCreation;
end;

function TServicePerThread.GetContextServiceInstanceID: PtrUInt;
begin
  with ServiceRunningContext^ do
    if Request = nil then
      result := 0
    else
    begin
      result := Request.ServiceInstanceID;
      if result <> GetThreadID then
        ESynException.RaiseUtf8('%.GetContextServiceInstanceID=%<>%',
          [self, result, GetThreadID]);
    end;
end;

function TServicePerThread.GetCurrentRunningThreadID: PtrUInt;
var
  Thread: TThread;
begin
  Thread := ServiceRunningContext.RunningThread;
  if Thread = nil then
    result := 0
  else
  begin
    result := PtrUInt(Thread.ThreadID);
    if result <> GetThreadID then
      ESynException.RaiseUtf8('%.GetCurrentRunningThreadID=%<>%',
        [self, result, GetThreadID]);
  end;
end;


{ TTestServiceOrientedArchitecture }

function TTestServiceOrientedArchitecture.NewClient(
  aClientSide: TClientSide): TRestClientDBNamed;

  function Ask(client: TRestClientDBNamed; Method, Params, ParamsURI, ParamsObj: RawUtf8;
    ExpectedResult: integer): RawUtf8;
  var
    resp, data, uriencoded, head: RawUtf8;
  begin
    Params := ' [ ' + Params + ' ]'; // add some ' ' to test real-world values
    uriencoded := '?' + UrlEncode(Params);
    if client.Server.ServicesRouting = TRestServerRoutingRest then
    begin
      FastSetString(data, pointer(Params), length(Params)); // =UniqueString
      CheckEqual(client.URI(
        'root/calculator.' + Method, 'POST', @resp, nil, @data),
        ExpectedResult);
      if ExpectedResult = HTTP_SUCCESS then
      begin
        CheckEqual(client.URI(
          'root/CALCulator.' + Method + uriencoded, 'POST', @data),
          ExpectedResult);
        CheckEqual(data, resp, 'alternative URI-encoded-inlined parameters use');
        CheckEqual(client.URI(
          'root/Calculator.' + Method + '?' + ParamsURI, 'GET', @data),
          ExpectedResult);
        CheckEqual(data, resp,
          'alternative "param1=value1&param2=value2" URI-encoded scheme');
        FastSetString(data, pointer(Params), length(Params)); // =UniqueString
        CheckEqual(client.URI(
          'root/calculator/' + Method, 'POST', @data, nil, @data),
          ExpectedResult);
        CheckEqual(data, resp, 'interface/method routing');
        FastSetString(data, pointer(Params), length(Params)); // =UniqueString
        CheckEqual(client.URI(
          'root/CALCulator/' + Method + uriencoded, 'POST', @data),
          ExpectedResult);
        CheckEqual(data, resp, 'alternative URI-encoded-inlined parameters use');
        CheckEqual(client.URI(
           'root/Calculator/' + Method + '?' + ParamsURI, 'GET', @data),
           ExpectedResult);
        CheckEqual(data, resp,
          'alternative "param1=value1&param2=value2" URI-encoded scheme');
        FastSetString(data, pointer(ParamsObj), length(ParamsObj)); // =UniqueString
        CheckEqual(client.URI(
          'root/calculator/' + Method, 'POST', @data, nil, @data),
          ExpectedResult);
        CheckEqual(data, resp, 'alternative object-encoded-as-body parameters use');
        head := 'accept: application/xml';
        CheckEqual(client.URI(
          'root/Calculator/' + Method + '?' + ParamsURI, 'GET', @data, @head),
          ExpectedResult);
        Check(data <> resp, 'returned as XML');
        CheckEqual(head, XML_CONTENT_TYPE_HEADER);
        Check(IdemPChar(pointer(data), '<?XML'), 'returned as XML');
      end;
    end
    else if client.Server.ServicesRouting = TRestServerRoutingJsonRpc then
    begin
      data := '{"method":"' + Method + '", "params":' + Params + '}';
      CheckEqual(client.URI(
        'root/calculator', 'POST', @resp, nil, @data), ExpectedResult);
    end
    else
      raise Exception.Create('Invalid call');
    result := JsonDecode(resp, 'result', nil, true);
    if IdemPChar(Pointer(result), '{"result"') then
      result := JsonDecode(result, 'result', nil, false)
    else
      TrimChars(result, 1, 1); // trim '[' + ']'
    if (result <> '') and
       (result[1] = '"') then
      result := UnQuoteSQLString(result); // '"777"' -> '777'
    if (ExpectedResult = HTTP_SUCCESS) and
       (client.Server.ServicesRouting = TRestServerRoutingRest) then
    begin
      resp := XMLUTF8_HEADER + '<result><Result>' + result + '</Result></result>';
      CheckEqual(data, resp, 'xml');
    end;
  end;

var
  S: TServiceFactory;
  i: integer;
  uid: TID;
  rout: integer;
  resp: RawUtf8;
const
  ROUTING: array[0..1] of TRestServerURIContextClass = (
    TRestServerRoutingRest, TRestServerRoutingJsonRpc);
const
  ExpectedURI: array[0..5] of RawUtf8 = (
    'Add', 'Multiply', 'Subtract', 'ToText', 'ToTextFunc', '_Swap');
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
  // create model, client and server
  result := TRestClientDBNamed.Create(
    TOrmModel.Create([TAuthUser, TAuthGroup]),
    nil, SQLITE_MEMORY_DATABASE_NAME, TRestServerDB, {useAuth=}true);
  result.Name := GetEnumNameTrimed(TypeInfo(TClientSide), ord(aClientSide));
  result.ClientSide := aClientSide;
  result.Model.Owner := result;
  result.Server.Server.CreateMissingTables; // if tests are run with no db
  uid := result.Server.Orm.MainFieldID(TAuthGroup, 'User');
  Check(uid <> 0, 'server orm');
  CheckEqual(result.Orm.MainFieldID(TAuthGroup, 'User'), 0, 'client orm');
  Check(result.SetUser('User', 'synopse'), 'default user for Security tests');
  Check(result.Server.ServiceRegister(TServiceCalculator,
    [TypeInfo(ICalculator)], sicShared) <> nil,
    'register TServiceCalculator as the ICalculator implementation on the server');
  // verify ICalculator RTTI-generated details
  Check(result.Server.Services <> nil);
  if CheckFailed(result.Server.Services.Count = 1) then
    exit;
  S := result.Server.Services.Index(0);
  if CheckFailed(S <> nil) then
    exit;
  Check(S.InterfaceURI = 'Calculator');
  Check(S.InstanceCreation = sicShared);
  Check(S.InterfaceTypeInfo^.Kind = rkInterface);
  Check(S.InterfaceTypeInfo^.Name^ = 'ICalculator');
  Check(GuidToString(S.InterfaceIID) = '{9A60C8ED-CEB2-4E09-87D4-4A16F496E5FE}');
  CheckEqual(GuidToRawUtf8(S.InterfaceIID), '{9A60C8ED-CEB2-4E09-87D4-4A16F496E5FE}');
  CheckEqual(S.InterfaceMangledURI, '7chgmrLOCU6H1EoW9Jbl_g');
  i := S.ServiceMethodIndex('swap');
  Check(i > 0);
  CheckEqual(S.ServiceMethodIndex('_swap'), i); // /calc/swap -> ICalc._Swap
  result.Server.Services.ExpectMangledURI := true;
  Check(result.Server.Services[S.InterfaceMangledURI] = S);
  result.Server.Services.ExpectMangledURI := false;
  Check(result.Server.Services['Calculator'] = S);
  Check(result.Server.Services['CALCULAtor'] = S);
  Check(result.Server.Services['CALCULAtors'] = nil);
  if not CheckEqual(length(S.InterfaceFactory.Methods),
     16 {$ifndef HASNOSTATICRTTI} + 1 {$endif}, 'methods') then
    exit;
  //FileFromString{JsonReformatToFile}(S.Contract, 'contract.json');
  //FileFromString(S.ContractHash, 'contract.hash');
  {$ifdef HASNOSTATICRTTI}
  CheckEqual(S.ContractHash, '"52FBB4DF85F3145E"');
  {$else}
  CheckEqual(S.ContractHash, '"A6BCBB7E50FD2CE3"');
  with S.InterfaceFactory.Methods[11] do
  begin // 11 function RecordCall(const Uuid: TGuid; const Pos: TCoords): RawJson;
    CheckEqual(URI, 'RecordCall');
    if CheckEqual(length(Args), 4) then
    begin
      CheckEqual(ArgsName[0], 'Self');
      Check(Args[0].ValueDirection = imdConst);
      Check(Args[0].ValueType = imvSelf);
      Check(Args[0].ArgTypeName^ = 'ICalculator');
      CheckEqualShort(Args[1].ParamName^, 'Uuid');
      CheckEqual(Args[1].ArgRtti.Name, 'TGuid');
      Check(not (vIsHFA in Args[1].ValueKindAsm), 'hfa1');
      CheckEqualShort(Args[2].ParamName^, 'Pos');
      CheckEqual(Args[2].ArgRtti.Name, 'TCoords');
      Check(vIsHFA in Args[2].ValueKindAsm, 'hfa2');
    end;
  end;
  {$endif HASNOSTATICRTTI}
  Check(TServiceCalculator(nil).Test(1, 2) = '3');
  Check(TServiceCalculator(nil).ToTextFunc(777) = '777');
  for i := 0 to high(ExpectedURI) do // SpecialCall interface not checked
    with S.InterfaceFactory.Methods[i] do
    begin
      CheckEqual(URI, ExpectedURI[i]);
      CheckEqual(length(Args), ExpectedParCount[i]);
      Check(ArgsUsed = ExpectedArgs[i], 'used');
      CheckEqualShort(Args[0].ParamName^, 'Self');
      CheckEqual(ArgsName[0], 'Self');
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
        CheckEqual(ArgsName[1], 'n1');
        Check(Args[1].ValueDirection = imdConst);
        Check(Args[2].ParamName^ = 'n2');
        CheckEqual(ArgsName[2], 'n2');
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
        CheckEqual(ArgsName[1], 'Value');
        Check(Args[1].ValueDirection = imdConst);
        Check(Args[2].ParamName^ = 'Result');
        CheckEqual(ArgsName[2], 'Result');
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
        CheckEqual(ArgsName[1], 'n1');
        Check(Args[1].ValueDirection = imdVar);
        Check(Args[2].ParamName^ = 'n2');
        CheckEqual(ArgsName[2], 'n2');
        Check(Args[2].ValueDirection = imdVar);
      end;
    end;
  // IComplexCalculator + IComplexNumber services
  Check(result.Server.ServiceRegister(
    TServiceComplexCalculator, [TypeInfo(IComplexCalculator)], sicSingle) <> nil);
  Check(result.Server.ServiceRegister(
    TServiceComplexNumber, [TypeInfo(IComplexNumber)], sicClientDriven) <> nil);
  Check(result.Server.ServiceRegister(
    TServiceUserGroupSession, [TypeInfo(ITestSession)], sicPerSession) <> nil);
  Check(result.Server.ServiceRegister(
    TServiceUserGroupSession, [TypeInfo(ITestUser)], sicPerUser) <> nil);
  Check(result.Server.ServiceRegister(
    TServiceUserGroupSession, [TypeInfo(ITestGroup)], sicPerGroup) <> nil);
  Check(result.Server.ServiceRegister(
    TServicePerThread, [TypeInfo(ITestPerThread)], sicPerThread) <> nil);
  // Json-level access
  for rout := low(ROUTING) to high(ROUTING) do
  begin
    result.ServicesRouting := ROUTING[rout].ClientRouting;
    result.Server.ServicesRouting := ROUTING[rout];
    if rout = 0 then
      (result.Server.Services['Calculator'] as TServiceFactoryServer).
        ResultAsXMLObjectIfAcceptOnlyXML := true;
    CheckEqual(Ask(result, 'None', '1,2', 'one=1&two=2',
      '{one:1,two=2}', HTTP_BADREQUEST), '');
    CheckEqual(Ask(result, 'Add', '1,2', 'n1=1&n2=2',
      '{n1:1,n2:2}', HTTP_SUCCESS), '3');
    CheckEqual(Ask(result, 'Add', '1,0', 'n2=1',
      '{n2:1}', HTTP_SUCCESS), '1');
    CheckEqual(Ask(result, 'Multiply', '2,3', 'n1=2&n2=3',
      '{n0:"abc",n2:3,m:null,n1:2}', HTTP_SUCCESS), '6');
    CheckEqual(Ask(result, 'Subtract', '23,20', 'n2=20&n1=23',
      '{n0:"abc",n2:20,n1:23}', HTTP_SUCCESS), '3');
    CheckEqual(Ask(result, 'ToText', '777,"abc"', 'result=abc&value=777',
      '{result:"abc",value=777}', HTTP_SUCCESS), '777');
    CheckEqual(Ask(result, 'ToTextFunc', '777', 'value=777',
      '{result:"abc",value=777}', HTTP_SUCCESS), '777');
    if rout = 0 then
      CheckEqual(result.URI(
        'root/ComplexCalculator.GetCustomer?CustomerId=John%20Doe', 'POST',
          @resp, nil, nil), 406, 'incorrect input');
  end;
  result.ServicesRouting := TRestServerRoutingRest.ClientRouting; // back to default
  result.Server.ServicesRouting := TRestServerRoutingRest;
end;

procedure TTestServiceOrientedArchitecture.Test(
  const Inst: TTestServiceInstances; Iterations: cardinal);

  procedure TestCalculator(const I: ICalculator);
  var
    i1, i2: PtrInt;
    n, t, i3: integer;
    c: cardinal;
    cu: currency;
    n1, n2, s1, s2: double;
    o: TServiceInstanceImplementations;
    Ints: TIntegerDynArray;
    Strs1: TRawUtf8DynArray;
    Str2: TWideStringDynArray;
    Rec1: TVirtualTableModuleProperties;
    Rec2, RecRes: TEntry;
    s, u: RawUtf8;
    p: PUtf8Char;
    r: string;
    l1, l2: IDocList;
    d1, d2: IDocDict;
  begin
    Setlength(Ints, 2);
    CsvToRawUtf8DynArray('one,two,three', Strs1);
    CheckEqual(length(strs1), 3);
    for t := 1 to Iterations do
    begin
      i1 := Random31 - Random31;
      i2 := Random31 - i1;
      Check(I.Add(i1, i2) = i1 + i2);
      Check(I.Multiply(i1, i2) = Int64(i1) * Int64(i2));
      n1 := RandomDouble * 1E-9 - RandomDouble * 1E-8;
      n2 := n1 * RandomDouble;
      CheckSame(I.Subtract(n1, n2), n1 - n2);
      s1 := n1;
      s2 := n2;
      CheckSame(s1, n1);
      CheckSame(s2, n2);
      I._Swap(s1, s2);
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
      CheckEqual(i3, i1 + length(s));
      Check(c = cardinal(i2) + 1);
      Check(o = [sicClientDriven, sicPerGroup]);
      Ints[0] := i1;
      Ints[1] := i2;
      SetLength(Str2, 3);
      Str2[0] := 'ABC';
      Str2[1] := 'DEF';
      Str2[2] := 'GHIJK';
      FillCharFast(Rec1, SizeOf(Rec1), 0);
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
      CheckEqual(Rec2.Json, IntegerDynArrayToCSV(pointer(Ints), length(Ints)));
      CheckEqual(RecRes.ID, i1);
      Check(RecRes.Timestamp512 = c);
      CheckEqual(RecRes.Json, StringToUtf8(Rec1.FileExtension));
      CheckSame(n1, n2);
      Rec1.FileExtension := ''; // to avoid memory leak
    end;
    i1 := Random32;
    i2 := Random32;
    l1 := DocList([i1, i2]);
    I.TestDocList(l1, i1, l2); // l2:=l1 & l1:=DocList([1,2,3,i1])
    CheckEqual(l1.Json, FormatUtf8('[1,2,3,%]', [i1]));
    CheckEqual(l2.Len, 2);
    CheckEqual(l2.I[0], i1);
    CheckEqual(l2.I[1], i2);
    d1 := DocDict(['a', i1]);
    I.TestDocDict(d1, i2, d2); // d2:=d1 & d1:=DocDict(['a',1,'b',2,'data',i2])
    CheckEqual(d1.Json, FormatUtf8('{"a":1,"b":2,"data":%}', [i2]));
    CheckEqual(d2.Len, 1);
    CheckEqual(d2.I['a'], i1);
    n1 := 0;
    RecRes := I.ComplexCall(Ints, nil, Str2, Rec1, Rec2, n1, n2);
    Check(length(Str2) = 5);
    Check(Str2[0] = 'ABC');
    Check(Str2[1] = 'DEF');
    Check(Str2[2] = 'GHIJK');
    Check(Str2[3] = 'one,two,three');
    Check(Str2[4] = '');
    s := RawUtf8OfChar(#1, 100);
    CheckEqual(I.DirectCall(s), 100);
    CheckEqual(I.VariantCall(100), '100');
    CheckEqual(I.VariantCall(100.0), '100');
    CheckEqual(JsonUnicodeUnEscape(I.VariantCall(s)), s);
    CheckEqual(I.VariantCall(_JsonFastFloat('{pi:3.14}')), '{"pi":3.14}');
    s := RandomUri(600);
    u := I.RepeatJsonArray(s, 100);
    t := length(u);
    checkutf8(t = 1 + 100 * 603, 'RawJson %', [KB(t)]);
    l1 := DocList(u);
    CheckEqual(l1.Len, 100, 'RJA');
    for i1 := 0 to l1.Len - 1 do
      CheckEqual(l1.U[i1], s, 'RJA');
    c := 1; // within the same process, no need to push this request
    n := 100;
    if Inst.ClientSide in [csHttp, csHttpLog] then
    begin
      c := 50; // >5000 for very agressive tests
      n := 1000; // generate a 600KB response (e.g. test IOCP background send)
    end;
    repeat
      u := I.RepeatTextArray(s, n);
      t := length(u);
      CheckEqual(t, n * 600, 'RepeatTextArray');
      p := pointer(u);
      repeat
        Check(CompareMem(p, pointer(s), 600), 'RTA');
        inc(p, 600);
        dec(t, 600)
      until t = 0;
      Check(p^ = #0, 'end RTA');
      dec(c);
    until c = 0;
  end;

var
  s: RawUtf8;
  data: TCustomerData;
  people: TOrmPeople;
  peoples: TOrmPeopleObjArray;
  cust: TServiceCustomAnswer;
  c: cardinal;
  n1, n2: double;
  C1, C2, C3: TComplexNumber;
  Item: TCollTest;
  List, Copy: TCollTestsI;
  n, j: integer;
  w, x, y, z: PtrUInt; // TThreadID  = ^TThreadRec under BSD
  V1, V2, V3: variant;
  {$ifndef HASNOSTATICRTTI}
  Nav, Nav2: TConsultaNav;
  {$endif HASNOSTATICRTTI}
begin
  CheckEqual(Inst.I.Add(1, 2), 3);
  Check(Inst.I.Multiply($1111333, $222266667) = $24693E8DB170B85, 'I.Mul');
  CheckEqual(Inst.I.StackIntMultiply(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 3628800, 'sm1');
  Check(Inst.I.StackFloatMultiply(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) = 3628800, 'sm2');
  CheckSame(Inst.I.Subtract(23, 20), 3, DOUBLE_SAME, 'substract');
  Inst.I.ToText(3.14, s);
  CheckEqual(s, '3.14');
  Check(Inst.I.ToTextFunc(777) = '777', '777');
  x := Inst.CT.GetCurrentThreadID;
  Check(x <> 0, 'x');
  if not (Inst.ClientSide in [csHttp, csHttpLog]) then
  begin
    y := Inst.CT.GetThreadIDAtCreation;
    Check(x = y, 'x=y');
  end;
  y := Inst.CC.GetCurrentThreadID;
  Check(y <> 0, 'y');
  case Inst.ClientSide of
    csDirect,
    csServer,
    csMainThread:
      Check(y = PtrUInt(MainThreadID), 'thrd1');
    csBackground,
    csHttp,
    csHttpLog:
      Check(y <> PtrUInt(MainThreadID), 'thrd2');
  end;
  TestCalculator(Inst.I);
  TestCalculator(Inst.CC); // test the fact that CC inherits from ICalculator
  n := 1000;
  s := Inst.CC.TestRawJson(n, 49, _TESTRAWJSON);
  Check(length(s) = n + 2);
  CheckEqual(Hash32(s), 4223609852); // n = 1000
  //CheckEqual(Hash32(s), 2508875362); // n = 100000000
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
      cust := Inst.CC.TestBlob(C3, Inst.ClientSide);
      Check(PosEx(TEXT_CONTENT_TYPE_HEADER, cust.Header) > 0);
      FormatUtf8('%,%', [C3.Real, C3.Imaginary], s);
      CheckEqual(cust.Content, s);
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
      s := Inst.CC.TestRawJson(c, c and 31 + 48, _TESTRAWJSON);
      Check(length(s) = integer(c + 2));
      Check(IsValidJson(s));
      if s <> '' then
      begin
        Check(s[1] = '"');
        if c > 0 then
          Check(ord(s[2]) = c and 31 + 48);
        for j := 3 to length(s) - 1 do
          Check(s[j] = s[2]);
        Check(s[length(s)] = '"');
      end;
      Check(Inst.CC.GetCustomer(c, data));
      Check(data.Id = integer(c));
      Check(GetCardinal(pointer(data.AccountNum)) = c);
      people := TOrmPeople.Create;
      try
        people.IDValue := c;
        Inst.CC.FillPeople(people);
        Check(people.ID = c);
        if c = 0 then
        begin
          Check(people.LastName = '');
          Check(people.FirstName = '');
        end
        else
        begin
          Check(people.LastName = FormatUtf8('Last %', [c]));
          Check(people.FirstName = FormatUtf8('First %', [c]));
        end;
      finally
        people.Free;
      end;
      n := c and 7; // not too much data
      Inst.CC.FillPeoples(n, peoples);
      Check(length(peoples) = n);
      for j := 0 to n - 1 do
        with peoples[j] do
        begin
          CheckEqual(IDValue, j);
          CheckEqual(FirstName, UInt32ToUtf8(j));
          CheckEqual(LastName, 'Last');
          CheckEqual(YearOfBirth, 1982 + j);
          CheckEqual(YearOfDeath, 1992 + j);
        end;
      ObjArrayClear(peoples);
      {$ifndef CPUAARCH64} // FPC doesn't follow the AARCH64 ABI -> fixme
      {$ifndef HASNOSTATICRTTI} // need RTTI for static records
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
    cust := Inst.CC.TestBlob(C3, Inst.ClientSide);
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
    n1 := RandomDouble * 1000;
    Inst.CN.Real := n1;
    CheckSame(Inst.CN.Real, n1);
    CheckSame(Inst.CN.Imaginary, n2, 1E-9);
    n2 := RandomDouble * 1000;
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
  CheckEqual(Inst.CU.GetContextSessionID, Inst.ExpectedSessionID, 'session');
  CheckEqual(Inst.CG.GetContextSessionGroup, Inst.ExpectedGroupID, 'group');
  CheckEqual(Inst.CS.GetContextSessionUser, Inst.ExpectedUserID, 'user');
  w := Inst.CT.GetContextServiceInstanceID;
  x := Inst.CT.GetCurrentThreadID;
  Check(x <> 0, 'x0');
  y := Inst.CT.GetThreadIDAtCreation;
  z := Inst.CT.GetCurrentRunningThreadID;
  case Inst.ClientSide of
    csServer,
    csDirect:
      begin
        Check(x = y);
        Check(x = PtrUInt(MainThreadID));
        Check(z = 0);
        Check(w = 0);
      end;
    csHttp,
    csHttpLog:
      begin
        Check(z <> 0);
        Check(x <> PtrUInt(MainThreadID));
        Check(y <> PtrUInt(MainThreadID));
        Check(w <> 0);
      end;
  // csLocked, csMainThread, csBackground does not apply to CC: ITestPerThread
  else
    begin
      Check(x = y);
      Check(z = 0);
      Check(w <> 0);
    end;
  end;
end;

procedure SetOptions(aClient: TRestClientDBNamed; aAsJsonObject: boolean;
  aOptions: TInterfaceMethodOptions);
var
  s: integer;
begin
  with aClient.Server.Services do
    for s := 0 to count - 1 do
      with Index(s) as TServiceFactoryServer do
      begin
        ResultAsJsonObject := aAsJsonObject;
        if InterfaceTypeInfo <> TypeInfo(ITestPerThread) then
          SetOptions([], aOptions);
      end;
end;

procedure TTestServiceOrientedArchitecture.ClientTest(aClient: TRestClientDBNamed;
  aRouting: TRestServerUriContextClass; aAsJsonObject: boolean;
  aRunInOtherThread: boolean; aOptions: TInterfaceMethodOptions);
var
  Inst: TTestServiceInstances;
  O: TObject;
  sign, sign2, ok: RawUtf8;
  stat: TSynMonitorInputOutput;
  timer: TPrecisionTimer;
begin
  if CheckFailed(aClient <> nil) then
    exit;
  timer.Start;
  FillCharFast(Inst, SizeOf(Inst), 0);
  Inst.ClientSide := aClient.ClientSide;
  ok := '!';
  try
    Check(aClient.ServiceRegister([TypeInfo(ICalculator)], sicShared));
    Check(aClient.ServiceRegister([TypeInfo(IComplexCalculator)], sicSingle));
    Check(aClient.ServiceRegister([TypeInfo(ITestSession)], sicPerSession));
    Check(aClient.ServiceRegister([TypeInfo(ITestUser)], sicPerUser));
    Check(aClient.ServiceRegister([TypeInfo(ITestGroup)], sicPerGroup));
    Check(aClient.ServiceRegister([TypeInfo(ITestPerThread)], sicPerThread));
    (aClient.Services['Calculator'] as TServiceFactoryClient).
      ParamsAsJsonObject := aAsJsonObject;
    SetOptions(aClient, aAsJsonObject, aOptions);
    aClient.Server.ServicesRouting := aRouting;
    aClient.ServicesRouting := aRouting.ClientRouting;
    (aClient.Server.Services as TServiceContainerServer).PublishSignature := true;
    sign := aClient.Services['Calculator'].RetrieveSignature;
    sign2 := aClient.Server.Services['Calculator'].RetrieveSignature;
    CheckEqual(sign, sign2, 'sign');
    (aClient.Server.Services as TServiceContainerServer).PublishSignature := false;
    CheckEqual(aClient.Services['Calculator'].RetrieveSignature, '');
    // once registered, can be accessed by its GUID or URI
    if CheckFailed(
         aClient.Services.Info(TypeInfo(ICalculator)).Get(Inst.I)) or
       CheckFailed(
         aClient.Services.Info(TypeInfo(IComplexCalculator)).Get(Inst.CC)) or
       CheckFailed(
         aClient.Services.Info(TypeInfo(IComplexNumber)).Get(Inst.CN)) or
       CheckFailed(
         aClient.Services.Info(TypeInfo(ITestUser)).Get(Inst.CU)) or
       CheckFailed(
         aClient.Services.Info(TypeInfo(ITestSession)).Get(Inst.CS)) or
       CheckFailed(
         aClient.Services.Info(TypeInfo(ITestGroup)).Get(Inst.CG)) or
       CheckFailed(
         aClient.Services.Info(TypeInfo(ITestPerThread)).Get(Inst.CT)) then
      exit;
    O := ObjectFromInterface(Inst.I);
    Check((O <> nil) and
          (Copy(O.ClassName, 1, 21) = 'TInterfacedObjectFake'));
    Inst.ExpectedSessionID := aClient.SessionID;
    if CheckFailed(aClient.SessionUser <> nil) then
      exit;
    aClient.Orm.Retrieve('LogonName=?', [], [aClient.SessionUser.LogonName],
      aClient.SessionUser);
    Inst.ExpectedUserID := aClient.SessionUser.ID;
    Inst.ExpectedGroupID := aClient.SessionUser.GroupRights.ID;
    Test(Inst);
    Inst.I := nil;
    if CheckFailed(aClient.Services.Info(ICalculator).Get(Inst.I)) then
      exit;
    Test(Inst);
    Inst.I := nil;
    if CheckFailed(aClient.Services.Resolve(ICalculator, Inst.I)) then
      exit;
    Test(Inst);
    Finalize(Inst);
    if CheckFailed(aClient.Services['Calculator'].Get(Inst.I)) or
       CheckFailed(aClient.Services['ComplexCalculator'].Get(Inst.CC)) or
       CheckFailed(aClient.Services['ComplexNumber'].Get(Inst.CN)) or
       CheckFailed(aClient.Services['TestUser'].Get(Inst.CU)) or
       CheckFailed(aClient.Services['TestSession'].Get(Inst.CS)) or
       CheckFailed(aClient.Services['TestGroup'].Get(Inst.CG)) or
       CheckFailed(aClient.Services['testperthread'].Get(Inst.CT)) then
      exit;
    {$ifndef CPUARM}
    // The FPC arm optimizer ruins a return address at level -O2
    // So, disable this test until a suitable fix is found.
    Inst.CN.Imaginary;
    {$endif CPUARM}
    Test(Inst);
    SetOptions(aClient, false, []);
    stat := (aClient.Server.Services['Calculator'] as TServiceFactoryServer).stat['ToText'];
    Check(stat.TaskCount > 0);
    ok := '';
  finally
    NotifyProgress([ok, aClient.Name, ' ', timer.Stop]);
  end;
end;

procedure TTestServiceOrientedArchitecture.DirectCall;
var
  Inst: TTestServiceInstances;
begin
  {$ifndef HASNOSTATICRTTI} // Delphi 7/2007 raises "TGuid has no type info"
  Rtti.RegisterFromText(TypeInfo(TCoords), 'x,y:double');
  {$endif HASNOSTATICRTTI}
  FillCharFast(Inst, SizeOf(Inst), 0); // all Expected..ID=0
  Inst.ClientSide := csDirect;
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
  fMain := NewClient(csServer);
  FillCharFast(Inst, SizeOf(Inst), 0); // all Expected..ID=0
  Inst.ClientSide := csServer;
  if CheckFailed(fMain <> nil) or
     CheckFailed(fMain.Server.Services.Count = 7) or
     CheckFailed(fMain.Server.Services.Index(0).Get(Inst.I)) or
     CheckFailed(Assigned(Inst.I)) or
     CheckFailed(fMain.Server.Services.Info(
       TypeInfo(ICalculator)).Get(Inst.I)) or
     CheckFailed(fMain.Server.Services.Info(
       TypeInfo(IComplexCalculator)).Get(Inst.CC)) or
     CheckFailed(fMain.Server.Services.Info(
       TypeInfo(IComplexNumber)).Get(Inst.CN)) or
     CheckFailed(fMain.Server.Services.Info(
       TypeInfo(ITestUser)).Get(Inst.CU)) or
     CheckFailed(fMain.Server.Services.Info(
       TypeInfo(ITestSession)).Get(Inst.CS)) or
     CheckFailed(fMain.Server.Services.Info(
       TypeInfo(ITestGroup)).Get(Inst.CG)) or
     CheckFailed(fMain.Server.Services.Info(
       TypeInfo(ITestPerThread)).Get(Inst.CT)) then
    exit;
  Test(Inst);
  Finalize(Inst);
  Check(Inst.I = nil);
  if CheckFailed(fMain.Server.Services['Calculator'].Get(Inst.I)) or
     CheckFailed(fMain.Server.Services['ComplexCalculator'].Get(Inst.CC)) or
     CheckFailed(fMain.Server.Services['ComplexNumber'].Get(Inst.CN)) or
     CheckFailed(fMain.Server.Services['TestUser'].Get(Inst.CU)) or
     CheckFailed(fMain.Server.Services['TestSession'].Get(Inst.CS)) or
     CheckFailed(fMain.Server.Services['TestGroup'].Get(Inst.CG)) or
     CheckFailed(fMain.Server.Services['TestPerThread'].Get(Inst.CT)) then
    exit;
  Test(Inst);
  Test(Inst);
end;

procedure TTestServiceOrientedArchitecture.Security;

  procedure Test(Expected: TOrmTableBits; const msg: string);

    function Ask(const Method, Params: RawUtf8): RawUtf8;
    var
      resp, data: RawUtf8;
    begin
      data := '{"method":"' + Method + '", "params": [ ' + Params + ' ]}';
      fMain.URI('root/calculator', 'POST', @resp, nil, @data);
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
  fMain.ServicesRouting := TRestServerRoutingJsonRpc.ClientRouting;
  fMain.Server.ServicesRouting := TRestServerRoutingJsonRpc;
  GroupID := fMain.Server.Orm.MainFieldID(TAuthGroup, 'User');
  Check(GroupID <> 0);
  Check(fMain.Server.Orm.MainFieldIDs(TAuthGroup, ['User', 'Admin'], g));
  if not CheckFailed(length(g) = 2) then
    Check((g[0] = GroupID) or (g[1] = GroupID));
  S := fMain.Server.Services['Calculator'] as TServiceFactoryServer;
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
  S.AllowAll;
  Test([1, 2, 3, 4, 5], 'restore allowed for the current user');
  Check(not fMain.SetUser('unknown', 'wrongpass'));
  Test([], 'no authentication -> access denied');
  Check(fMain.SetUser('Admin', 'synopse'));
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
  Check(fMain.SetUser('User', 'synopse'));
  Test([1, 2, 3, 4, 5], 'restore allowed for everybody');
  FreeAndNil(fMain);
end;

procedure TTestServiceOrientedArchitecture.ClientSide;

  procedure One(const Event: TNotifyEvent; cs: TClientSide);
  begin
    Run(Event, NewClient(cs), SmallUInt32Utf8[ord(cs)],
      {threaded=}true, {notify=}false, {forcedThreaded=}true);
  end;

begin
  // most client test cases would be run in their own thread (if possible)
  {$ifndef OSANDROID} // no "main" thread on Android?
  One(ClientSideRESTThread,           csMainThread); // should be threaded
  {$endif OSANDROID}
  One(ClientSideRESTThread,           csBackground); // (slowest first)
  One(ClientSideRESTAsJsonObject,     csJsonObject);
  One(ClientSideRESTSessionsStats,    csSessions);
  One(ClientSideRESTThread,           csLocked);
  One(ClientSideRESTSign,             csCrc32);
  One(ClientSideRESTSign,             csCrc32c);
  One(ClientSideRESTSign,             csXxhash);
  One(ClientSideRESTSign,             csMd5);
  One(ClientSideRESTSign,             csSha1);
  One(ClientSideRESTSign,             csSha256);
  One(ClientSideRESTSign,             csSha512);
  One(ClientSideRESTSign,             csSha3);
  One(ClientSideRESTAuth,             csWeak);
  One(ClientSideRESTAuth,             csBasic);
  One(ClientSideRESTServiceLogToDB,   csDblog);
  One(ClientSideJsonRPC,              csJsonrpc);
  One(ClientSideOverHTTP,             csHttp);
  One(ClientSideOverHTTP,             csHttplog);
  // wait for all multi-threaded background process to finish
  RunWait({notifyThreadCount=}false, {timeoutSec=}120, {callSynchronize=}true);
  // RTTI override could NOT be parallelized
  ClientSideRESTCustomRecord(NewClient(csCustomRtti));
end;

procedure TTestServiceOrientedArchitecture.ClientSideRESTServiceLogToDB(Sender: TObject);
var
  Log: TRestServerDB;
  c: TRestClientDBNamed absolute Sender;
begin
  {$ifdef OSDARWIN}
  {$ifdef NOSQLITE3STATIC}
  // due to a very strange error during prepare_v2, this does not (yet) work on Darwin.
  // at least on Darwin with system sqlite 3.7.13
  // however, mORMots own static works perfect
  Check(1 = 0, 'Not (yet) supported on Darwin !!');
  exit;
  {$endif NOSQLITE3STATIC}
  {$endif OSDARWIN}
  DeleteFile(WorkDir + 'servicelog.db');
  Log := TRestServerDB.CreateWithOwnModel([TOrmServiceLog], WorkDir + 'servicelog.db');
  try
    Log.DB.Synchronous := smOff;
    Log.DB.LockingMode := lmExclusive;
    Log.Server.CreateMissingTables;
    (c.Server.ServiceContainer as TServiceContainerServer).SetServiceLog(Log.Orm);
    ClientTest(c, TRestServerRoutingRest, false);
  finally
    (c.Server.ServiceContainer as TServiceContainerServer).SetServiceLog(nil);
    Log.Free;
  end;
  c.Free;
end;

procedure TTestServiceOrientedArchitecture.ClientSideRESTSessionsStats(Sender: TObject);
var
  stats: RawUtf8;
  store: TRestServerDB;
  c: TRestClientDBNamed absolute Sender;
begin
  store := TRestServerDB.CreateWithOwnModel(
    [TOrmMonitorUsage], WorkDir + 'servicestats.db3');
  try
    c.Server.StatLevels := SERVERDEFAULTMONITORLEVELS + [mlSessions];
    store.DB.Synchronous := smOff;
    store.DB.LockingMode := lmExclusive;
    store.Server.CreateMissingTables;
    c.Server.StatUsage := TSynMonitorUsageRest.Create(store.Orm, 1);
    ClientTest(c, TRestServerRoutingRest, false);
    c.CallBackGet('stat', ['withall', true], stats);
    JsonReformatToFile(stats, WorkDir + 'statsSessions.Json');
    c.Server.StatLevels := SERVERDEFAULTMONITORLEVELS;
    c.Server.StatUsage := nil;
  finally
    store.Free;
    c.Free;
  end;
end;

procedure TTestServiceOrientedArchitecture.ClientSideJsonRPC(Sender: TObject);
begin
  ClientTest(Sender as TRestClientDBNamed, TRestServerRoutingJsonRpc, false);
  Sender.Free;
end;

procedure TTestServiceOrientedArchitecture.ClientSideRESTAsJsonObject(Sender: TObject);
begin
  ClientTest(Sender as TRestClientDBNamed, TRestServerRoutingRest, true);
  Sender.Free;
end;

procedure TTestServiceOrientedArchitecture.ClientSideOverHTTP(Sender: TObject);
var
  c: TRestClientDBNamed absolute Sender;
begin
  TestHttp(c, c.ClientSide = csHttpLog,
    UInt32ToUtf8(GetInteger(HTTP_DEFAULTPORT) + ord(c.ClientSide = csHttpLog)));
  c.Free;
end;

procedure TTestServiceOrientedArchitecture.TestHttp(
  aClient: TRestClientDBNamed; withlog: boolean; const port: RawUtf8);
var
  srv: TRestHttpServer;
  clt: TRestHttpClient;
  Inst: TTestServiceInstances;
  Json: RawUtf8;
  i: integer;
  opt: TRestHttpServerOptions;
  URI: TRestServerUriDynArray;
const
  SERVICES: array[0..4] of RawUtf8 = (
    'Calculator',
    'ComplexCalculator',
    'TestUser',
    'TestGroup',
    'TestPerThread');
begin
  Check(aClient.Server.ServicesRouting = TRestServerRoutingRest);
  opt := HTTPSERVER_DEFAULT_OPTIONS;
  //opt := opt + [rsoLogVerbose];
  if withlog then
    opt := opt + [rsoEnableLogging, rsoTelemetryCsv, rsoTelemetryJson];
  srv := TRestHttpServer.Create(port, [aClient.Server], '+',
    useBidirAsync, // HTTP_DEFAULT_MODE,
    8, secNone, '', '', opt);
  try
    Check(srv.HttpServer <> nil);
    if withlog then
    begin
      Check(srv.HttpServer.Logger <> nil);
      Check(srv.HttpServer.Logger.Settings <> nil);
      srv.HttpServer.Logger.Settings.DefaultRotate := hrtAfter1MB;
    end
    else
      Check(srv.HttpServer.Logger = nil);
    FillCharFast(Inst, SizeOf(Inst), 0); // all Expected..ID=0
    Inst.ClientSide := aClient.ClientSide;
    clt := TRestHttpClient.Create('127.0.0.1', port, aClient.Model);
    try
      clt.ServicePublishOwnInterfaces :=
        aClient.Server.ServicesPublishedInterfaces;
      //clt.OnIdle := TLoginForm.OnIdleProcess; // from mORMotUILogin
      // clt.Compression := [hcSynShaAes]; // 350ms (300ms for [])
      Check(clt.SetUser('User', 'synopse'));
      // register services on the client side
      Check(clt.ServiceRegister([TypeInfo(ICalculator)], sicShared));
      Check(clt.ServiceRegister([TypeInfo(IComplexCalculator)], sicSingle));
      Check(clt.ServiceRegister([TypeInfo(ITestSession)], sicPerSession));
      Check(clt.ServiceRegister([TypeInfo(ITestUser)], sicPerUser));
      Check(clt.ServiceRegister([TypeInfo(ITestGroup)], sicPerGroup));
      Check(clt.ServiceRegister([TypeInfo(ITestPerThread)], sicPerThread));
      // retrieve service instances
      if CheckFailed(clt.Services.Info(TypeInfo(ICalculator)).
           Get(Inst.I)) or
         CheckFailed(clt.Services.Info(TypeInfo(IComplexCalculator)).
           Get(Inst.CC)) or
         CheckFailed(clt.Services.Info(TypeInfo(IComplexNumber)).
           Get(Inst.CN)) or
         CheckFailed(clt.Services.Info(TypeInfo(ITestUser)).
           Get(Inst.CU)) or
         CheckFailed(clt.Services.Info(TypeInfo(ITestSession)).
           Get(Inst.CS)) or
         CheckFailed(clt.Services.Info(TypeInfo(ITestGroup)).
           Get(Inst.CG)) or
         CheckFailed(clt.Services.Info(TypeInfo(ITestPerThread)).
           Get(Inst.CT)) then
        exit;
      Inst.ExpectedSessionID := clt.SessionID;
      clt.Orm.Retrieve('LogonName=?', [],
        [clt.SessionUser.LogonName], clt.SessionUser);
      Inst.ExpectedUserID := clt.SessionUser.ID;
      Inst.ExpectedGroupID := clt.SessionUser.GroupRights.ID;
      CheckEqual(
        clt.CallBackGet('stat', ['findservice', 'toto'], Json),
        HTTP_SUCCESS);
      CheckEqual(Json, '[]');
      for i := 0 to High(SERVICES) do
      begin
        CheckEqual(clt.CallBackGet(
          'stat', ['findservice', SERVICES[i]], Json), HTTP_SUCCESS, 'stat');
        Check(Json <> '[]');
        Check(clt.ServiceRetrieveAssociated(SERVICES[i], URI));
        Check(length(URI) = 1);
        Check(URI[0].Port = port);
        Check(URI[0].Root = aClient.Model.Root);
      end;
      Check(clt.ServiceRetrieveAssociated(IComplexNumber, URI));
      Check(length(URI) = 1);
      Check(clt.ServiceRetrieveAssociated(ITestSession, URI));
      Check(length(URI) = 1);
      Test(Inst, 100);
      NotifyProgress([aClient.Name]);
    finally
      Finalize(Inst);
      clt.Free;
    end;
  finally
    srv.Free;
  end;
end;

const
  CS_ALGO: array[csCrc32 .. csSha3] of TRestAuthenticationSignedUriAlgo = (
    suaCrc32, suaCrc32c, suaXxHash, suaMd5, suaSha1, suaSha256, suaSha512, suaSha3);

procedure TTestServiceOrientedArchitecture.ClientSideRESTSign(Sender: TObject);
var
  c: TRestClientDBNamed absolute Sender;
begin
  (c.Server.AuthenticationRegister(TRestServerAuthenticationDefault) as
    TRestServerAuthenticationDefault).Algorithm := CS_ALGO[c.ClientSide];
  c.SetUser('User', 'synopse');
  ClientTest(c, TRestServerRoutingRest, false);
  c.Free;
end;

procedure TTestServiceOrientedArchitecture.ClientSideRESTAuth(Sender: TObject);
var
  c: TRestClientDBNamed absolute Sender;
begin
  c.Server.AuthenticationUnregister([
    {$ifdef OSWINDOWS}
    TRestServerAuthenticationSspi,
    {$endif OSWINDOWS}
    TRestServerAuthenticationDefault]);
  case c.ClientSide of
    csWeak:
      begin
        c.Server.AuthenticationRegister(TRestServerAuthenticationNone);
        TRestClientAuthenticationNone.ClientSetUser(c, 'User', '');
      end;
    csBasic:
      begin
        c.Server.AuthenticationRegister(TRestServerAuthenticationHttpBasic);
        TRestClientAuthenticationHttpBasic.ClientSetUser(c, 'User', 'synopse');
      end;
  end;
  ClientTest(c, TRestServerRoutingRest, false);
  c.Free;
end;

procedure TTestServiceOrientedArchitecture.ClientSideRESTCustomRecord(
  Client: TRestClientDBNamed);
begin
  // warning: could NOT be parallelized before RTTI override is global
  TRttiJson.RegisterCustomSerializer(TypeInfo(TEntry),
    TTestServiceOrientedArchitecture.CustomReader,
    TTestServiceOrientedArchitecture.CustomWriter);
  try
    ClientTest(Client, TRestServerRoutingRest, false);
  finally
    TRttiJson.UnRegisterCustomSerializer(TypeInfo(TEntry));
    Client.Free;
  end;
end;

class procedure TTestServiceOrientedArchitecture.CustomReader(
  var Context: TJsonParserContext; Data: pointer);
var
  V: PEntry absolute Data;
  Values: array[0..2] of TValuePUtf8Char;
begin
  // {"ID":1786554763,"Timestamp":323618765,"Json":"D:\\TestSQL3.exe"}
  if Context.ParseObject(['ID', 'Timestamp', 'Json'], @Values) then
  begin
    V.ID := Values[0].ToInt64;
    V.Timestamp512 := Values[1].ToCardinal;
    Values[2].ToUtf8(V.Json);
  end;
end;

class procedure TTestServiceOrientedArchitecture.CustomWriter(
  W: TJsonWriter; Data: pointer; Options: TTextWriterWriteObjectOptions);
var
  V: PEntry absolute Data;
begin
  W.AddJsonEscape([
    'ID',        V.ID,
    'Timestamp', Int64(V.Timestamp512),
    'Json',      V.Json]);
end;

{procedure TTestServiceOrientedArchitecture.Cleanup;
var
  stats: RawUtf8;
begin
  if fClient = nil then
    exit;
  fClient.CallBackGet('stat', [
    'withtables',     true,
    'withsqlite3',    true,
    'withmethods',    true,
    'withinterfaces', true,
    'withsessions',   true], stats);
  FileFromString(JsonReformat(stats), WorkDir + 'stats.Json');
  FreeAndNil(fClient);
end;}

procedure TTestServiceOrientedArchitecture.ClientSideRESTThread(Sender: TObject);
var
  c: TRestClientDBNamed absolute Sender;
  opt: TInterfaceMethodOptions;
begin
  case c.ClientSide of
    csMainThread:
      opt := [optExecInMainThread, optFreeInMainThread];
    csBackground:
      opt := [optExecInPerInterfaceThread, optFreeInPerInterfaceThread];
    csLocked:
      opt := [optExecLockedPerInterface];
  end;
  ClientTest(c, TRestServerRoutingRest, false, true, opt);
  c.Free;
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

  TUseWeakRef = (
    direct,
    weakref
    {$ifndef NOPATCHVMT} , zeroing {$endif} );

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
  CheckEqual(SizeOf(TMvcAction), SizeOf(TServiceCustomAnswer));
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
  {$ifndef NOPATCHVMT}
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
  {$endif NOPATCHVMT}
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
    {$ifndef NOPATCHVMT}
    zeroing:
      SetWeakZero(self, @FChild, Value);
    {$endif NOPATCHVMT}
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
    {$ifndef NOPATCHVMT}
    zeroing:
      SetWeakZero(self, @FParent, Value);
    {$endif NOPATCHVMT}
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
  U.Password := UInt32ToUtf8(Random32);
  U.MobilePhoneNumber := UInt32ToUtf8(Random32);
  if fSmsSender.Send('Your new password is ' + U.Password, U.MobilePhoneNumber) then
    fUserRepository.Save(U);
end;

procedure TTestServiceOrientedArchitecture.IntSubtractJson(
  Ctxt: TOnInterfaceStubExecuteParamsJson);
var
  P: PUtf8Char;
begin
  if Ctxt.Sender is TInterfaceMock then
    TInterfaceMock(Ctxt.Sender).TestCase.Check(Ctxt.EventParams = 'toto');
  P := pointer(Ctxt.Params);
  Ctxt.Returns([GetNextItemDouble(P) - GetNextItemDouble(P)]);
  // Ctxt.Result := '['+DoubleToStr(GetNextItemDouble(P)-GetNextItemDouble(P))+']';
end;

procedure TTestServiceOrientedArchitecture.IntSubtractVariant(
  Ctxt: TOnInterfaceStubExecuteParamsVariant);
begin
  if Ctxt.Sender is TInterfaceMock then
    TInterfaceMock(Ctxt.Sender).TestCase.Check(Ctxt.EventParams = 'toto');
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
  CheckEqual(Stub.LogAsText, 'Add(10,30)=[30],Multiply(10,30)=[60],' +
    'Multiply(2,35)=[70],Subtract(2.3,1.2)=[0],ToTextFunc(2.3)=["default"]');
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
    Check(false, 'dead code EInterfaceFactory');
  except
    on E: EInterfaceFactory do
      Check(Pos('TInterfaceStub returned error: expected exception',
        E.Message) > 0, E.Message);
  end;
  try
    I.Add(1, 2);
    Check(false, 'dead code ESynException');
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

