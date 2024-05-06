/// Framework Core Unit and Regression Testing
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.test;

{
  *****************************************************************************

   Testing functions shared by all framework units
    - Unit-Testing classes and functions

  *****************************************************************************
}


interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  mormot.core.base,
  mormot.core.data,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.perf,
  mormot.core.log;


{ ************ Unit-Testing classes and functions }

type
  /// the prototype of an individual test
  // - to be used with TSynTest descendants
  TOnSynTest = procedure of object;

  /// allows to tune TSynTest process
  // - tcoLogEachCheck will log as sllCustom4 each non void Check() message
  // - tcoLogInSubFolder will log within a '[executable]\log\' sub-folder
  // - tcoLogVerboseRotate will force the log files to rotate - could be set if
  // you expect test logs to be huge, bigger than what LogView supports
  // - tcoLogNotHighResolution will log the current time as plain ISO-8601 text
  TSynTestOption = (
    tcoLogEachCheck,
    tcoLogInSubFolder,
    tcoLogVerboseRotate,
    tcoLogNotHighResolution);

  /// set of options to tune TSynTest process
  TSynTestOptions = set of TSynTestOption;

  TSynTest = class;

  /// how published method information is stored within TSynTest
  TSynTestMethodInfo = record
    /// the uncamelcased method name
    TestName: string;
    /// ready-to-be-displayed 'Ident - TestName' text, as UTF-8
    IdentTestName: RawUtf8;
    /// raw method name, as defined in pascal code (not uncamelcased)
    MethodName: RawUtf8;
    /// direct access to the method execution
    Method: TOnSynTest;
    /// the test case holding this method
    Test: TSynTest;
    /// the index of this method in the TSynTestCase
    MethodIndex: integer;
  end;
  /// pointer access to published method information

  PSynTestMethodInfo = ^TSynTestMethodInfo;

  /// abstract parent class for both tests suit (TSynTests) and cases (TSynTestCase)
  // - purpose of this ancestor is to have RTTI for its published methods,
  // and to handle a class text identifier, or uncamelcase its class name
  // if no identifier was defined
  // - sample code about how to use this test framework is available in
  // the "Sample\07 - SynTest" folder
  TSynTest = class(TSynPersistent)
  protected
    fTests: array of TSynTestMethodInfo;
    fIdent: string;
    fInternalTestsCount: integer;
    fOptions: TSynTestOptions;
    fWorkDir: TFileName;
    fRestrict: TRawUtf8DynArray;
    function GetCount: integer;
    function GetIdent: string;
    procedure SetWorkDir(const Folder: TFileName);
  public
    /// create the test instance
    // - if an identifier is not supplied, the class name is used, after
    // T[Syn][Test] left trim and un-camel-case
    // - this constructor will add all published methods to the internal
    // test list, accessible via the Count/TestName/TestMethod properties
    constructor Create(const Ident: string = ''); reintroduce; virtual;
    /// register a specified test to this class instance
    // - Create will register all published methods of this class, but
    // your code may initialize its own set of methods on need
    procedure Add(const aMethod: TOnSynTest;
      const aMethodName: RawUtf8; const aIdent: string);
    /// the test name
    // - either the Ident parameter supplied to the Create() method, either
    // a uncameled text from the class name
    property Ident: string
      read GetIdent;
    /// return the number of tests associated with this class
    // - i.e. the number of registered tests by the Register() method PLUS
    // the number of published methods defined within this class
    property Count: integer
      read GetCount;
    /// return the number of published methods defined within this class as tests
    // - i.e. the number of tests added by the Create() constructor from RTTI
    // - any TestName/TestMethod[] index higher or equal to this value has been
    // added by a specific call to the Add() method
    property InternalTestsCount: integer
      read fInternalTestsCount;
    /// allows to tune the test case process
    property Options: TSynTestOptions
      read fOptions write fOptions;
    /// folder name which can be used to store the temporary data during testing
    // - equals Executable.ProgramFilePath by default
    // - when set, will ensure it contains a trailing path delimiter (\ or /)
    property WorkDir: TFileName
      read fWorkDir write SetWorkDir;
    /// list of 'class.method' names to restrict the tests for Run
    // - as retrieved from "--test class.method" command line switch
    property Restrict: TRawUtf8DynArray
      read fRestrict write fRestrict;
  published
    { all published methods of the children will be run as individual tests
      - these methods must be declared as procedure with no parameter }
  end;

  /// callback signature as used by TSynTestCase.CheckRaised
  // - passed parameters can be converted e.g. using VarRecToUtf8/VarRecToInt64
  TOnTestCheck = procedure(const Params: array of const) of object;

  TSynTests = class;

  /// a class implementing a test case
  // - should handle a test unit, i.e. one or more tests
  // - individual tests are written in the published methods of this class
  TSynTestCase = class(TSynTest)
  protected
    fOwner: TSynTests;
    fAssertions: integer;
    fAssertionsFailed: integer;
    fAssertionsBeforeRun: integer;
    fAssertionsFailedBeforeRun: integer;
    /// any number not null assigned to this field will display a "../s" stat
    fRunConsoleOccurrenceNumber: cardinal;
    /// any number not null assigned to this field will display a "using .. MB" stat
    fRunConsoleMemoryUsed: Int64;
    /// any text assigned to this field will be displayed on console
    fRunConsole: string;
    fCheckLogTime: TPrecisionTimer;
    fCheckLastMsg: cardinal;
    fCheckLastTix: cardinal;
    /// called before all published properties are executed
    procedure Setup; virtual;
    /// called after all published properties are executed
    // - WARNING: this method should be re-entrant - so using FreeAndNil() is
    // a good idea in this method :)
    procedure CleanUp; virtual;
    /// called before each published properties execution
    procedure MethodSetup; virtual;
    /// called after each published properties execution
    procedure MethodCleanUp; virtual;
    procedure AddLog(condition: boolean; const msg: string);
    procedure DoCheckUtf8(condition: boolean; const msg: RawUtf8;
      const args: array of const);
  public
    /// create the test case instance
    // - must supply a test suit owner
    // - if an identifier is not supplied, the class name is used, after
    // T[Syn][Test] left trim and un-camel-case
    constructor Create(Owner: TSynTests; const Ident: string = ''); reintroduce; virtual;
    /// clean up the instance
    // - will call CleanUp, even if already done before
    destructor Destroy; override;
    /// used by the published methods to run a test assertion
    // - condition must equals TRUE to pass the test
    procedure Check(condition: boolean; const msg: string = '');
      {$ifdef HASSAFEINLINE}inline;{$endif} // Delphi 2007 has trouble inlining this
    /// used by the published methods to run a test assertion
    // - condition must equals TRUE to pass the test
    // - function return TRUE if the condition failed, in order to allow the
    // caller to stop testing with such code:
    // ! if CheckFailed(A=10) then exit;
    function CheckFailed(condition: boolean; const msg: string = ''): boolean;
      {$ifdef HASSAFEINLINE}inline;{$endif}
    /// used by the published methods to run a test assertion
    // - condition must equals FALSE to pass the test
    // - function return TRUE if the condition failed, in order to allow the
    // caller to stop testing with such code:
    // ! if CheckNot(A<>10) then exit;
    function CheckNot(condition: boolean; const msg: string = ''): boolean;
      {$ifdef HASSAFEINLINE}inline;{$endif}
    /// used by the published methods to run test assertion against integers
    // - if a<>b, will fail and include '#<>#' text before the supplied msg
    function CheckEqual(a, b: Int64; const msg: RawUtf8 = ''): boolean; overload;
      {$ifdef HASSAFEINLINE}inline;{$endif}
    /// used by the published methods to run test assertion against UTF-8 strings
    // - if a<>b, will fail and include '#<>#' text before the supplied msg
    function CheckEqual(const a, b: RawUtf8; const msg: RawUtf8 = ''): boolean; overload;
    /// used by the published methods to run test assertion against pointers/classes
    // - if a<>b, will fail and include '#<>#' text before the supplied msg
    function CheckEqual(a, b: pointer; const msg: RawUtf8 = ''): boolean; overload;
      {$ifdef HASSAFEINLINE}inline;{$endif}
    /// used by the published methods to run test assertion against integers
    // - if a=b, will fail and include '#=#' text before the supplied msg
    function CheckNotEqual(a, b: Int64; const msg: RawUtf8 = ''): boolean; overload;
      {$ifdef HASSAFEINLINE}inline;{$endif}
    /// used by the published methods to run test assertion against UTF-8 strings
    // - if a=b, will fail and include '#=#' text before the supplied msg
    function CheckNotEqual(const a, b: RawUtf8; const msg: RawUtf8 = ''): boolean; overload;
    /// used by the published methods to run test assertion against pointers/classes
    // - if a=b, will fail and include '#=#' text before the supplied msg
    function CheckNotEqual(a, b: pointer; const msg: RawUtf8 = ''): boolean; overload;
      {$ifdef HASSAFEINLINE}inline;{$endif}
    /// used by the published methods to run a test assertion about two double values
    // - includes some optional precision argument
    function CheckSame(const Value1, Value2: double;
      const Precision: double = DOUBLE_SAME; const msg: string = ''): boolean;
    /// used by the published methods to run a test assertion about two TDateTime values
    // - allows an error of up to 1 second between the values
    function CheckSameTime(const Value1, Value2: TDateTime; const msg: string = ''): boolean;
    /// used by the published methods to perform a string comparison with several values
    // - test passes if (Value=Values[0]) or (Value=Value[1]) or (Value=Values[2])...
    // and ExpectedResult=true
    function CheckMatchAny(const Value: RawUtf8; const Values: array of RawUtf8;
      CaseSentitive: boolean = true; ExpectedResult: boolean = true; const msg: string = ''): boolean;
    /// used by the published methods to run a test assertion, with an UTF-8 error message
    // - condition must equals TRUE to pass the test
    procedure CheckUtf8(condition: boolean; const msg: RawUtf8); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// used by the published methods to run a test assertion, with a error
    // message computed via FormatUtf8()
    // - condition must equals TRUE to pass the test
    procedure CheckUtf8(condition: boolean; const msg: RawUtf8;
      const args: array of const); overload;
    /// used by the published methods to execute a Method with the given
    // parameters, and ensure a (optionally specific) exception is raised
    function CheckRaised(const Method: TOnTestCheck; const Params: array of const;
      Raised: ExceptionClass = nil): boolean;
    /// used by published methods to start some timing on associated log
    // - call this once, before one or several consecutive CheckLogTime()
    // - warning: this method is not thread-safe
    procedure CheckLogTimeStart;
      {$ifdef HASINLINE}inline;{$endif}
    /// used by published methods to write some timing on associated log
    // - at least one CheckLogTimeStart method call should happen to reset the
    // internal timer
    // - condition must equals TRUE to pass the test
    // - the supplied message would be appended, with its timing
    // - warning: this method is not thread-safe
    procedure CheckLogTime(condition: boolean; const msg: RawUtf8;
      const args: array of const; level: TSynLogLevel = sllTrace);
    /// used by the published methods to run test assertion against a Hash32() constant
    procedure CheckHash(const data: RawByteString; expectedhash32: cardinal;
      const msg: RawUtf8 = '');
    /// create a temporary string random content, WinAnsi (code page 1252) content
    class function RandomString(CharCount: integer): WinAnsiString;
    /// create a temporary UTF-8 string random content, using WinAnsi
    // (code page 1252) content
    class function RandomUtf8(CharCount: integer): RawUtf8;
    /// create a temporary UTF-16 string random content, using WinAnsi
    // (code page 1252) content
    class function RandomUnicode(CharCount: integer): SynUnicode;
    /// create a temporary string random content, using ASCII 7-bit content
    class function RandomAnsi7(CharCount: integer): RawByteString;
    /// create a temporary string random content, using A..Z,_,0..9 chars only
    class function RandomIdentifier(CharCount: integer): RawByteString;
    /// create a temporary string random content, using uri-compatible chars only
    class function RandomUri(CharCount: integer): RawByteString;
    /// create a temporary string, containing some fake text, with paragraphs
    class function RandomTextParagraph(WordCount: integer; LastPunctuation: AnsiChar = '.';
      const RandomInclude: RawUtf8 = ''): RawUtf8;
    /// add containing some "bla bli blo blu" fake text, with paragraphs
    class procedure AddRandomTextParagraph(WR: TTextWriter; WordCount: integer;
      LastPunctuation: AnsiChar = '.'; const RandomInclude: RawUtf8 = '';
      NoLineFeed: boolean = false);
    /// this method is triggered internally - e.g. by Check() - when a test failed
    procedure TestFailed(const msg: string);
    /// will add to the console a message with a speed estimation
    // - speed is computed from the method start or supplied local Timer
    // - returns the number of microsec of the (may be specified) timer
    // - any ItemCount<0 would hide the trailing count and use abs(ItemCount)
    // - OnlyLog will compute and append the info to the log, but not on console
    // - warning: this method is thread-safe only if a local Timer is specified
    function NotifyTestSpeed(const ItemName: string; ItemCount: integer;
      SizeInBytes: QWord = 0; Timer: PPrecisionTimer = nil;
      OnlyLog: boolean = false): TSynMonitorOneMicroSec; overload;
    /// will add to the console a formatted message with a speed estimation
    function NotifyTestSpeed(
      const ItemNameFmt: RawUtf8; const ItemNameArgs: array of const;
      ItemCount: integer; SizeInBytes: QWord = 0; Timer: PPrecisionTimer = nil;
      OnlyLog: boolean = false): TSynMonitorOneMicroSec; overload;
    /// append some text to the current console
    // - OnlyLog will compute and append the info to the log, but not on the console
    procedure AddConsole(const msg: string; OnlyLog: boolean = false); overload;
    /// append some text to the current console
    procedure AddConsole(const Fmt: RawUtf8; const Args: array of const;
      OnlyLog: boolean = false); overload;
    /// append some text to the current console in real time, on the same line
    // - the information is flushed to the console immediately, whereas
    // AddConsole() append it into a buffer to be written once
    procedure NotifyProgress(const Args: array of const;
      Color: TConsoleColor = ccGreen);
    /// the test suit which owns this test case
    property Owner: TSynTests
      read fOwner;
    /// the human-readable test name
    // - either the Ident parameter supplied to the Create() method, either
    // an uncameled text from the class name
    property Ident: string
      read GetIdent;
    /// the number of assertions (i.e. Check() method call) for this test case
    property Assertions: integer
      read fAssertions;
    /// the number of failures (i.e. Check(false) method call) for this test case
    property AssertionsFailed: integer
      read fAssertionsFailed;
  published
    { all published methods of the children will be run as individual tests
      - these methods must be declared as procedure with no parameter
      - the method name will be used, after "uncamelcasing", for display }
  end;

  /// class-reference type (metaclass) of a test case
  TSynTestCaseClass = class of TSynTestCase;

  /// information about a failed test
  TSynTestFailed = record
    /// the contextual message associated with this failed test
    Error: string;
    /// the uncamelcased method name
    TestName: string;
    /// ready-to-be-displayed 'TestCaseIdent - TestName' text, as UTF-8
    IdentTestName: RawUtf8;
  end;

  TSynTestFaileds = array of TSynTestFailed;

  /// event signature for TSynTests.CustomOutput callback
  TOnSynTestOutput = procedure(const value: RawUtf8) of object;

  /// a class used to run a suit of test cases
  TSynTests = class(TSynTest)
  protected
    /// any number not null assigned to this field will display a "../sec" stat
    fRunConsoleOccurrenceNumber: cardinal;
    fTestCaseClass: array of TSynTestCaseClass;
    fAssertions: integer;
    fAssertionsFailed: integer;
    fCurrentMethodInfo: PSynTestMethodInfo;
    fSaveToFile: System.Text;
    fSafe: TSynLocker;
    fFailed: TSynTestFaileds;
    fFailedCount: integer;
    fNotifyProgressLineLen: integer;
    fNotifyProgress: RawUtf8;
    function GetFailedCount: integer;
    function GetFailed(Index: integer): TSynTestFailed;
    procedure CreateSaveToFile; virtual;
    /// low-level output on the console - use TSynTestCase.AddConsole instead
    procedure DoText(const value: RawUtf8); overload; virtual;
    /// low-level output on the console - use TSynTestCase.AddConsole instead
    procedure DoText(const values: array of const); overload;
    /// low-level output on the console - use TSynTestCase.AddConsole instead
    procedure DoTextLn(const values: array of const); overload;
    /// low-level set the console text color - use TSynTestCase.AddConsole instead
    procedure DoColor(aColor: TConsoleColor);
    /// low-level output on the console with automatic formatting
    // - use TSynTestCase.NotifyProgress() instead
    procedure DoNotifyProgress(const value: RawUtf8; cc: TConsoleColor);
    /// called when a test case failed: default is to add item to fFailed[]
    procedure AddFailed(const msg: string); virtual;
    /// this method is called before every run
    // - default implementation will just return nil
    // - can be overridden to implement a per-test case logging for instance
    function BeforeRun: IUnknown; virtual;
    /// this method is called during the run, after every testcase
    // - this implementation just report some minimal data to the console
    // by default, but may be overridden to update a real UI or reporting system
    // - method implementation can use fCurrentMethodInfo^ to get run context
    procedure AfterOneRun; virtual;
    /// could be overriden to add some custom command-line parameters
    class procedure DescribeCommandLine; virtual;
  public
    /// you can put here some text to be displayed at the end of the messages
    // - some internal versions, e.g.
    // - every line of text must explicitly BEGIN with CRLF
    CustomVersions: string;
    /// allow redirection to any kind of output
    // - will be called in addition to default console write()
    CustomOutput: TOnSynTestOutput;
    /// contains the run elapsed time
    RunTimer, TestTimer, TotalTimer: TPrecisionTimer;
    /// create the test suit
    // - if an identifier is not supplied, the class name is used, after
    // T[Syn][Test] left trim and un-camel-case
    // - this constructor will add all published methods to the internal
    // test list, accessible via the Count/TestName/TestMethod properties
    constructor Create(const Ident: string = ''); override;
    /// finalize the class instance
    // - release all registered Test case instance
    destructor Destroy; override;
    /// you can call this class method to perform all the tests on the Console
    // - it will create an instance of the corresponding class, with the
    // optional identifier to be supplied to its constructor
    // - if the executable was launched with a parameter, it will be used as
    // file name for the output - otherwise, tests information will be written
    // to the console
    // - it will optionally enable full logging during the process
    // - a typical use will first assign the same log class for the whole
    // framework - in such case, before calling RunAsConsole(), the caller
    // should execute:
    // ! TSynLogTestLog := TSqlLog;
    // ! TMyTestsClass.RunAsConsole('My Automated Tests',LOG_VERBOSE);
    class procedure RunAsConsole(const CustomIdent: string = '';
      withLogs: TSynLogLevels = [sllLastError, sllError, sllException, sllExceptionOS, sllFail];
      options: TSynTestOptions = []; const workdir: TFileName = ''); virtual;
    /// save the debug messages into an external file
    // - if no file name is specified, the current Ident is used
    procedure SaveToFile(const DestPath: TFileName; const FileName: TFileName = '');
    /// save the debug messages into an existing Text file
    procedure SaveToText(var aDest: System.Text);
    /// register a specified Test case from its class name
    // - an instance of the supplied class will be created during Run
    // - the published methods of the children must call this method in order
    // to add test cases
    // - example of use (code from a TSynTests published method):
    // !  AddCase(TOneTestCase);
    procedure AddCase(TestCase: TSynTestCaseClass); overload;
    /// register a specified Test case from its class name
    // - an instance of the supplied classes will be created during Run
    // - the published methods of the children must call this method in order
    // to add test cases
    // - example of use (code from a TSynTests published method):
    // !  AddCase([TOneTestCase]);
    procedure AddCase(const TestCase: array of TSynTestCaseClass); overload;
    /// call of this method will run all associated tests cases
    // - function will return TRUE if all test passed
    // - all failed test cases will be added to the Failed[] list - which is
    // cleared at the beginning of the run
    // - Assertions and AssertionsFailed counter properties are reset and
    // computed during the run
    // - you may override the DescribeCommandLine method to provide additional
    // information, e.g.
    // ! function TMySynTests.Run: boolean;
    // ! begin // need mormot.db.raw.sqlite3 unit in the uses clause
    // !   CustomVersions := format(CRLF + CRLF + '%s' + CRLF + '    %s' + CRLF +
    // !     'Using mORMot %s' + CRLF + '    %s %s', [OSVersionText, CpuInfoText,
    // !      SYNOPSE_FRAMEWORK_FULLVERSION, sqlite3.ClassName, sqlite3.Version]);
    // !   result := inherited Run;
    // ! end;
    function Run: boolean; virtual;
    /// could be overriden to redirect the content to proper TSynLog.Log()
    procedure DoLog(Level: TSynLogLevel; const TextFmt: RawUtf8;
      const TextArgs: array of const); virtual;
    /// number of failed tests after the last call to the Run method
    property FailedCount: integer
      read GetFailedCount;
    /// method information currently running
    // - is set by Run and available within TTestCase methods
    property CurrentMethodInfo: PSynTestMethodInfo
      read fCurrentMethodInfo;
    /// retrieve the information associated with a failure
    property Failed[Index: integer]: TSynTestFailed
      read GetFailed;
  published
    /// the number of assertions (i.e. Check() method call) in all tests
    // - this property is set by the Run method above
    property Assertions: integer
      read fAssertions;
    /// the number of assertions (i.e. Check() method call) which failed in all tests
    // - this property is set by the Run method above
    property AssertionsFailed: integer
      read fAssertionsFailed;
  published
    { all published methods of the children will be run as test cases registering
      - these methods must be declared as procedure with no parameter
      - every method should create a customized TSynTestCase instance,
        which will be registered with the AddCase() method, then automaticaly
        destroyed during the TSynTests destroy  }
  end;

  /// this overridden class will create a .log file in case of a test case failure
  // - inherits from TSynTestsLogged instead of TSynTests in order to add
  // logging to your test suite (via a dedicated TSynLogTest instance)
  TSynTestsLogged = class(TSynTests)
  protected
    fLogFile: TSynLog;
    fConsoleDup: RawUtf8;
    procedure CreateSaveToFile; override;
    /// called when a test case failed: log into the file
    procedure AddFailed(const msg: string); override;
    /// this method is called before every run
    // - overridden implementation to implement a per-test case logging
    function BeforeRun: IUnknown; override;
  public
    /// create the test instance and initialize associated LogFile instance
    // - this will allow logging of all exceptions to the LogFile
    constructor Create(const Ident: string = ''); override;
    /// release associated memory
    destructor Destroy; override;
    /// the .log file generator created if any test case failed
    property LogFile: TSynLog
      read fLogFile;
    /// a replicate of the text written to the console
    property ConsoleDup: RawUtf8
      read fConsoleDup;
  end;


const
  EQUAL_MSG = '%<>% %';
  NOTEQUAL_MSG = '%=% %';

var
  /// the kind of .log file generated by TSynTestsLogged
  TSynLogTestLog: TSynLogClass = TSynLog;


implementation

uses
  mormot.core.rtti,
  mormot.core.datetime;


{ ************ Unit-Testing classes and functions }

{ TSynTest }

constructor TSynTest.Create(const Ident: string);
var
  id: RawUtf8;
  s: string;
  methods: TPublishedMethodInfoDynArray;
  i: integer;
begin
  inherited Create; // may have been overriden
  if Ident <> '' then
    fIdent := Ident
  else
  begin
    ClassToText(ClassType, id);
    if IdemPChar(Pointer(id), 'TSYN') then
      if IdemPChar(Pointer(id), 'TSYNTEST') then
        Delete(id, 1, 8)
      else
        Delete(id, 1, 4)
    else if IdemPChar(Pointer(id), 'TTEST') then
      Delete(id, 1, 5)
    else if id[1] = 'T' then
      Delete(id, 1, 1);
    fIdent := string(UnCamelCase(id));
  end;
  fWorkDir := Executable.ProgramFilePath;
  for i := 0 to GetPublishedMethods(self, methods) - 1 do
    with methods[i] do
    begin
      inc(fInternalTestsCount);
      if Name[1] = '_' then
        s := Ansi7ToString(copy(Name, 2, 100))
      else
        s := Ansi7ToString(UnCamelCase(Name));
      Add(TOnSynTest(Method), Name, s);
    end;
end;

procedure TSynTest.Add(const aMethod: TOnSynTest; const aMethodName: RawUtf8;
  const aIdent: string);
var
  n: integer;
begin
  if self = nil then
    exit; // avoid GPF
  n := Length(fTests);
  SetLength(fTests, n + 1);
  with fTests[n] do
  begin
    TestName := aIdent;
    IdentTestName := StringToUtf8(fIdent + ' - ' + TestName);
    Method := aMethod;
    MethodName := aMethodName;
    Test := self;
    MethodIndex := n;
  end;
end;

function TSynTest.GetCount: integer;
begin
  if self = nil then
    result := 0
  else
    result := length(fTests);
end;

function TSynTest.GetIdent: string;
begin
  if self = nil then
    result := ''
  else
    result := fIdent;
end;

procedure TSynTest.SetWorkDir(const Folder: TFileName);
begin
  if Folder = '' then
    fWorkDir := Executable.ProgramFilePath
  else
    fWorkDir := EnsureDirectoryExists(Folder, ESynException);
end;


{ TSynTestCase }

constructor TSynTestCase.Create(Owner: TSynTests; const Ident: string);
begin
  inherited Create(Ident);
  fOwner := Owner;
  fOptions := Owner.Options;
end;

procedure TSynTestCase.Setup;
begin
  // do nothing by default
end;

procedure TSynTestCase.CleanUp;
begin
  // do nothing by default
end;

procedure TSynTestCase.MethodSetup;
begin
  // do nothing by default
end;

procedure TSynTestCase.MethodCleanUp;
begin
  // do nothing by default
end;

destructor TSynTestCase.Destroy;
begin
  CleanUp;
  inherited;
end;

procedure TSynTestCase.AddLog(condition: boolean; const msg: string);
const
  LEV: array[boolean] of TSynLogLevel = (
    sllFail, sllCustom4);
var
  tix, crc: cardinal; // use a crc since strings are not thread-safe
begin
  if condition then
  begin
    crc := Hash32(pointer(msg), length(msg) * SizeOf(msg[1]));
    if crc = fCheckLastMsg then
    begin
      // no need to be too much verbose
      tix := GetTickCount64 shr 8; // also avoid to use a lock
      if tix = fCheckLastTix then
        exit;
      fCheckLastTix := tix;
    end;
    fCheckLastMsg := crc;
  end
  else
    fCheckLastMsg := 0;
  if fOwner.fCurrentMethodInfo <> nil then
    fOwner.DoLog(LEV[condition], '% % [%]', [ClassType,
      fOwner.fCurrentMethodInfo^.TestName, msg]);
end;

procedure TSynTestCase.Check(condition: boolean; const msg: string);
begin
  if self = nil then
    exit;
  inc(fAssertions);
  if (msg <> '') and
     (tcoLogEachCheck in fOptions) then
    AddLog(condition, msg);
  if not condition then
    TestFailed(msg);
end;

function TSynTestCase.CheckFailed(condition: boolean; const msg: string): boolean;
begin
  if self = nil then
  begin
    result := false;
    exit;
  end;
  inc(fAssertions);
  if (msg <> '') and
     (tcoLogEachCheck in fOptions) then
    AddLog(condition, msg);
  if condition then
    result := false
  else
  begin
    TestFailed(msg);
    result := true;
  end;
end;

function TSynTestCase.CheckNot(condition: boolean; const msg: string): boolean;
begin
  result := CheckFailed(not condition, msg);
end;

procedure TSynTestCase.DoCheckUtf8(condition: boolean; const msg: RawUtf8;
  const args: array of const);
var
  str: string;
begin
  // inc(fAssertions) has been made by the caller
  if msg <> '' then
  begin
    FormatString(msg, args, str);
    if tcoLogEachCheck in fOptions then
      AddLog(condition, str);
  end;
  if not condition then
    TestFailed(str{%H-});
end;

procedure TSynTestCase.CheckUtf8(condition: boolean; const msg: RawUtf8;
  const args: array of const);
begin
  inc(fAssertions);
  if not condition or
     (tcoLogEachCheck in fOptions) then
    DoCheckUtf8(condition, msg, args);
end;

procedure TSynTestCase.CheckUtf8(condition: boolean; const msg: RawUtf8);
begin
  inc(fAssertions);
  if not condition or
     (tcoLogEachCheck in fOptions) then
    DoCheckUtf8(condition, '%', [msg]);
end;

function TSynTestCase.CheckEqual(a, b: Int64; const msg: RawUtf8): boolean;
begin
  inc(fAssertions);
  result := a = b;
  if not result or
     (tcoLogEachCheck in fOptions) then
    DoCheckUtf8(result, EQUAL_MSG, [a, b, msg]);
end;

function TSynTestCase.CheckEqual(const a, b, msg: RawUtf8): boolean;
begin
  inc(fAssertions);
  result := SortDynArrayRawByteString(a, b) = 0;
  if not result or
     (tcoLogEachCheck in fOptions) then
    DoCheckUtf8(result, EQUAL_MSG, [a, b, msg]);
end;

function TSynTestCase.CheckEqual(a, b: pointer; const msg: RawUtf8): boolean;
begin
  inc(fAssertions);
  result := a = b;
  if not result or
     (tcoLogEachCheck in fOptions) then
    DoCheckUtf8(result, EQUAL_MSG, [a, b, msg]);
end;

function TSynTestCase.CheckNotEqual(a, b: Int64; const msg: RawUtf8): boolean;
begin
  inc(fAssertions);
  result := a <> b;
  if not result or
     (tcoLogEachCheck in fOptions) then
    DoCheckUtf8(result, NOTEQUAL_MSG, [a, b, msg]);
end;

function TSynTestCase.CheckNotEqual(const a, b: RawUtf8; const msg: RawUtf8): boolean;
begin
  inc(fAssertions);
  result := SortDynArrayRawByteString(a, b) <> 0;
  if not result or
     (tcoLogEachCheck in fOptions) then
    DoCheckUtf8(result, NOTEQUAL_MSG, [a, b, msg]);
end;

function TSynTestCase.CheckNotEqual(a, b: pointer; const msg: RawUtf8): boolean;
begin
  inc(fAssertions);
  result := a <> b;
  if not result or
     (tcoLogEachCheck in fOptions) then
    DoCheckUtf8(result, NOTEQUAL_MSG, [a, b, msg]);
end;

function TSynTestCase.CheckSame(const Value1, Value2, Precision: double;
  const msg: string): boolean;
begin
  inc(fAssertions);
  result := SameValue(Value1, Value2, Precision);
  if not result or
     (tcoLogEachCheck in fOptions) then
    DoCheckUtf8(result, NOTEQUAL_MSG, [Value1, Value2, msg]);
end;

function TSynTestCase.CheckSameTime(const Value1, Value2: TDateTime;
  const msg: string): boolean;
begin
  result := CheckSame(Value1, Value2, 1 / SecsPerDay);
end;

function TSynTestCase.CheckMatchAny(const Value: RawUtf8; const Values: array of RawUtf8;
  CaseSentitive: boolean; ExpectedResult: boolean; const msg: string): boolean;
begin
  result := (FindRawUtf8(Values, Value, CaseSentitive) >= 0) = ExpectedResult;
  Check(result);
end;

function TSynTestCase.CheckRaised(const Method: TOnTestCheck;
  const Params: array of const; Raised: ExceptionClass): boolean;
var
  msg: string;
begin
  try
    Method(Params);
    result := false;
    if Raised = nil then
      Raised := Exception;
    FormatString('% missing', [Raised], msg);
  except
    on E: Exception do
    begin
      result := (Raised = nil) or
                (PClass(E)^ = Raised);
      if result then
        FormatString('% [%]', [E, E.Message], msg)
      else
        FormatString('% instead of %', [E, Raised], msg);
    end;
  end;
  Check(result, msg);
end;

procedure TSynTestCase.CheckLogTimeStart;
begin
  fCheckLogTime.Start;
end;

procedure TSynTestCase.CheckLogTime(condition: boolean; const msg: RawUtf8;
  const args: array of const; level: TSynLogLevel);
var
  str: string;
begin
  FormatString(msg, args, str);
  Check(condition, str);
  fOwner.DoLog(level, '% %', [str, fCheckLogTime.Stop]);
  fCheckLogTime.Start;
end;

procedure TSynTestCase.CheckHash(const data: RawByteString;
  expectedhash32: cardinal; const msg: RawUtf8);
var
  crc: cardinal;
begin
  crc := Hash32(data);
  //if crc <> expectedhash32 then ConsoleWrite(data);
  CheckUtf8(crc = expectedhash32, 'Hash32()=$% expected=$% %',
    [CardinalToHexShort(crc), CardinalToHexShort(expectedhash32), msg]);
end;

class function TSynTestCase.RandomString(CharCount: integer): WinAnsiString;
var
  i: PtrInt;
  R: PByteArray;
  tmp: TSynTempBuffer;
begin
  R := tmp.InitRandom(CharCount);
  FastSetStringCP(result, nil, CharCount, CP_WINANSI);
  for i := 0 to CharCount - 1 do
    PByteArray(result)[i] := 32 + R[i] and 127;
  tmp.Done;
end;

class function TSynTestCase.RandomAnsi7(CharCount: integer): RawByteString;
var
  i: PtrInt;
  R: PByteArray;
  tmp: TSynTempBuffer;
begin
  R := tmp.InitRandom(CharCount);
  FastSetString(RawUtf8(result), CharCount);
  for i := 0 to CharCount - 1 do
    PByteArray(result)[i] := 32 + R[i] mod 94;
  tmp.Done;
end;

procedure InitRandom64(chars64: PAnsiChar; count: integer; var result: RawByteString);
var
  i: PtrInt;
  R: PByteArray;
  tmp: TSynTempBuffer;
begin
  R := tmp.InitRandom(count);
  FastSetString(RawUtf8(result), count);
  for i := 0 to count - 1 do
    PByteArray(result)[i] := ord(chars64[PtrInt(R[i]) and 63]);
  tmp.Done;
end;

class function TSynTestCase.RandomIdentifier(CharCount: integer): RawByteString;
const
  IDENT_CHARS: array[0..63] of AnsiChar =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_ABCDEFGHIJKLMNOPQRSTUVWXYZ_';
begin
  InitRandom64(@IDENT_CHARS, CharCount, result);
end;

class function TSynTestCase.RandomUri(CharCount: integer): RawByteString;
const
  URL_CHARS: array[0..63] of AnsiChar =
    'abcdefghijklmnopqrstuvwxyz0123456789-ABCDEFGH.JKLMNOP-RSTUVWXYZ.';
begin
  InitRandom64(@URL_CHARS, CharCount, result);
end;

class function TSynTestCase.RandomUtf8(CharCount: integer): RawUtf8;
begin
  result := WinAnsiToUtf8(RandomString(CharCount));
end;

class function TSynTestCase.RandomUnicode(CharCount: integer): SynUnicode;
begin
  result := WinAnsiConvert.AnsiToUnicodeString(RandomString(CharCount));
end;

class function TSynTestCase.RandomTextParagraph(WordCount: integer;
  LastPunctuation: AnsiChar; const RandomInclude: RawUtf8): RawUtf8;
var
  tmp: TTextWriterStackBuffer;
  WR: TTextWriter;
begin
  WR := TTextWriter.CreateOwnedStream(tmp);
  try
    AddRandomTextParagraph(WR, WordCount, LastPunctuation, RandomInclude);
    WR.SetText(result);
  finally
    WR.Free;
  end;
end;

class procedure TSynTestCase.AddRandomTextParagraph(WR: TTextWriter;
  WordCount: integer; LastPunctuation: AnsiChar; const RandomInclude: RawUtf8;
  NoLineFeed: boolean);
type
  TKind = (
    space, comma, dot, question, paragraph);
const
  bla: array[0..7] of string[3] = (
    'bla', 'ble', 'bli', 'blo', 'blu', 'bla', 'bli', 'blo');
  endKind = [dot, paragraph, question];
var
  n: integer;
  s: string[3];
  last: TKind;
  rnd: cardinal;
  lec: PLecuyer;
begin
  lec := Lecuyer;
  last := paragraph;
  while WordCount > 0 do
  begin
    rnd := lec^.Next; // get 32 bits of randomness for up to 4 words per loop
    for n := 0 to rnd and 3 do
    begin
      // consume up to 4*5 = 20 bits from rnd
      rnd := rnd shr 2;
      s := bla[rnd and 7];
      rnd := rnd shr 3;
      if last in endKind then
      begin
        last := space;
        s[1] := NormToUpper[s[1]];
      end;
      WR.AddShorter(s);
      WR.Add(' ');
      dec(WordCount);
    end;
    WR.CancelLastChar(' ');
    case rnd and 127 of // consume 7 bits
      0..4:
        begin
          if RandomInclude <> '' then
          begin
            WR.Add(' ');
            WR.AddString(RandomInclude); // 5/128 = 4% chance of text inclusion
          end;
          last := space;
        end;
      5..65:
        last := space;
      66..90:
        last := comma;
      91..105:
        last := dot;
      106..115:
        last := question;
      116..127:
        if NoLineFeed then
          last := dot
        else
          last := paragraph;
    end;
    case last of
      space:
        WR.Add(' ');
      comma:
        WR.Add(',', ' ');
      dot:
        WR.Add('.', ' ');
      question:
        WR.Add('?', ' ');
      paragraph:
        WR.AddShorter('.'#13#10);
    end;
  end;
  if (LastPunctuation <> ' ') and
     not (last in endKind) then
  begin
    WR.AddShorter('bla');
    WR.Add(LastPunctuation);
  end;
end;

procedure TSynTestCase.TestFailed(const msg: string);
begin
  fOwner.fSafe.Lock; // protect when Check() is done from multiple threads
  try
    fOwner.DoLog(sllFail, '#% %', [fAssertions - fAssertionsBeforeRun, msg]);
    if Owner <> nil then // avoid GPF
      Owner.AddFailed(msg);
    inc(fAssertionsFailed);
  finally
    fOwner.fSafe.UnLock;
  end;
end;

procedure TSynTestCase.AddConsole(const msg: string; OnlyLog: boolean);
begin
  fOwner.DoLog(sllMonitoring, '% %', [self, msg]);
  if OnlyLog then
    exit;
  fOwner.fSafe.Lock;
  try
    if fRunConsole <> '' then
      fRunConsole := fRunConsole + (CRLF + '     ') + msg
    else
      fRunConsole := fRunConsole + msg;
  finally
    fOwner.fSafe.UnLock;
  end;
end;

procedure TSynTestCase.AddConsole(
  const Fmt: RawUtf8; const Args: array of const; OnlyLog: boolean);
var
  msg: string;
begin
  FormatString(Fmt, Args, msg);
  AddConsole(msg, OnlyLog);
end;

function TSynTestCase.NotifyTestSpeed(const ItemName: string; ItemCount: integer;
  SizeInBytes: QWord; Timer: PPrecisionTimer;
  OnlyLog: boolean): TSynMonitorOneMicroSec;
var
  Temp: TPrecisionTimer;
  msg: string;
begin
  if Timer = nil then
    Temp := Owner.TestTimer
  else
    Temp := Timer^;
  if ItemCount <= -1 then // -ItemCount to hide the trailing count
  begin
    ItemCount := -ItemCount;
    FormatString('% in % i.e. %/s',
      [ItemName, Temp.Stop, K(Temp.PerSec(ItemCount))], msg);
  end
  else if ItemCount <= 1 then
    FormatString('% in %', [ItemName, Temp.Stop], msg)
  else
    FormatString('% % in % i.e. %/s, aver. %',
      [ItemCount, ItemName, Temp.Stop, K(Temp.PerSec(ItemCount)),
       Temp.ByCount(ItemCount)], msg);
  if SizeInBytes > 0 then
    msg := FormatString('%, %/s', [msg, KB(Temp.PerSec(SizeInBytes))]);
  AddConsole(msg, OnlyLog);
  result := Temp.TimeInMicroSec;
end;

function TSynTestCase.NotifyTestSpeed(const ItemNameFmt: RawUtf8;
  const ItemNameArgs: array of const; ItemCount: integer; SizeInBytes: QWord;
  Timer: PPrecisionTimer; OnlyLog: boolean): TSynMonitorOneMicroSec;
var
  str: string;
begin
  FormatString(ItemNameFmt, ItemNameArgs, str);
  result := NotifyTestSpeed(str, ItemCount, SizeInBytes, Timer, OnlyLog);
end;

procedure TSynTestCase.NotifyProgress(const Args: array of const;
  Color: TConsoleColor);
var
  msg: RawUtf8;
begin
  msg := ' ';
  Append(msg, Args);
  fOwner.DoNotifyProgress(msg, Color);
end;


{ TSynTests }

procedure TSynTests.AddCase(const TestCase: array of TSynTestCaseClass);
var
  i: PtrInt;
begin
  for i := 0 to high(TestCase) do
    PtrArrayAdd(fTestCaseClass, TestCase[i]);
end;

procedure TSynTests.AddCase(TestCase: TSynTestCaseClass);
begin
  PtrArrayAdd(fTestCaseClass, TestCase);
end;

function TSynTests.BeforeRun: IUnknown;
begin
  result := nil;
end;

constructor TSynTests.Create(const Ident: string);
begin
  inherited Create(Ident);
  fSafe.Init;
end;

destructor TSynTests.Destroy;
begin
  if TTextRec(fSaveToFile).Handle <> 0 then
    Close(fSaveToFile);
  inherited Destroy;
  fSafe.Done;
end;

{$I-}

procedure TSynTests.DoColor(aColor: TConsoleColor);
begin
  if (StdOut <> 0) and
     (THandle(TTextRec(fSaveToFile).Handle) = StdOut) then
    TextColor(aColor);
end;

procedure TSynTests.DoText(const value: RawUtf8);
begin
  write(fSaveToFile, value);
  if Assigned(CustomOutput) then
    CustomOutput(value);
end;

procedure TSynTests.DoText(const values: array of const);
var
  i: PtrInt;
  s: RawUtf8;
begin
  for i := 0 to high(values) do
  begin
    VarRecToUtf8(values[i], s);
    DoText(s);
  end;
end;

procedure TSynTests.DoTextLn(const values: array of const);
begin
  DoText(values);
  DoText(CRLF);
end;

procedure TSynTests.DoNotifyProgress(const value: RawUtf8; cc: TConsoleColor);
var
  len: integer;
begin
  if fNotifyProgress = '' then
  begin
    DoColor(ccGreen);
    DoTextLn(['  - ', fCurrentMethodInfo^.TestName, ':']);
    DoText('     ');
    fNotifyProgressLineLen := 0;
  end;
  len := length(value);
  inc(fNotifyProgressLineLen, len);
  if (fNotifyProgress <> '') and
     (fNotifyProgressLineLen > 73) then
  begin
    DoText([CRLF + '     ']);
    fNotifyProgressLineLen := len;
  end;
  Append(fNotifyProgress, value);
  DoColor(cc);
  DoText(value);
  DoColor(ccLightGray);
end;

procedure TSynTests.DoLog(Level: TSynLogLevel; const TextFmt: RawUtf8;
  const TextArgs: array of const);
begin
  if Level = sllFail then
    TSynLogTestLog.DebuggerNotify(Level, TextFmt, TextArgs)
  else
    TSynLogTestLog.Add.Log(level, TextFmt, TextArgs, self);
end;

procedure TSynTests.CreateSaveToFile;
begin
  System.Assign(fSaveToFile, '');
  Rewrite(fSaveToFile);
  StdOut := TTextRec(fSaveToFile).Handle;
end;

procedure TSynTests.AddFailed(const msg: string);
begin
  if fFailedCount = length(fFailed) then
    SetLength(fFailed, NextGrow(fFailedCount));
  with fFailed[fFailedCount] do
  begin
    Error := msg;
    if fCurrentMethodInfo <> nil then
    begin
      TestName := fCurrentMethodInfo^.TestName;
      IdentTestName := fCurrentMethodInfo^.IdentTestName;
    end;
  end;
  inc(fFailedCount);
end;

function TSynTests.GetFailed(Index: integer): TSynTestFailed;
begin
  if (self = nil) or
     (cardinal(Index) >= cardinal(fFailedCount)) then
    Finalize(result)
  else
    result := fFailed[Index];
end;

function TSynTests.GetFailedCount: integer;
begin
  if self = nil then
    result := 0
  else
    result := fFailedCount;
end;

function TSynTests.Run: boolean;
var
  i, t, m: integer;
  Elapsed, Version: RawUtf8;
  dir: TFileName;
  err: string;
  C: TSynTestCase;
  started: boolean;
  {%H-}log: IUnknown;
begin
  if TTextRec(fSaveToFile).Handle = 0 then
    CreateSaveToFile;
  DoColor(ccLightCyan);
  DoTextLn([CRLF + '   ', Ident,
            CRLF + '  ', RawUtf8OfChar('-', length(Ident) + 2)]);
  RunTimer.Start;
  Randomize;
  fFailed := nil;
  fAssertions := 0;
  fAssertionsFailed := 0;
  dir := GetCurrentDir;
  for m := 0 to Count - 1 do
  try
    DoColor(ccWhite);
    DoTextLn([CRLF + CRLF, m + 1, '. ', fTests[m].TestName]);
    DoColor(ccLightGray);
    fTests[m].Method(); // call AddCase() to add instances into fTestCaseClass
    try
      for i := 0 to high(fTestCaseClass) do
      begin
        started := false;
        C := fTestCaseClass[i].Create(self); // add all published methods
        try
          for t := 0 to C.Count - 1 do
          try
            fCurrentMethodInfo := @C.fTests[t];
            if (fRestrict <> nil) and
              ((FindPropName(pointer(fRestrict),
                  FormatUtf8('%.%', [C, fCurrentMethodInfo^.MethodName]),
                  // e.g. --test TNetworkProtocols.DNSAndLDAP
                  length(fRestrict)) < 0) and
               (FindPropName(pointer(fRestrict),
                  // e.g. --test TNetworkProtocols
                  ToText(C.ClassType), length(fRestrict)) < 0)) then
              continue;
            if not started then
            begin
              C.fAssertions := 0; // reset assertions count
              C.fAssertionsFailed := 0;
              C.fWorkDir := fWorkDir;
              SetCurrentDir(fWorkDir);
              TotalTimer.Start;
              C.Setup;
              DoColor(ccWhite);
              DoTextLn([CRLF + ' ', m + 1, '.', i + 1, '. ', C.Ident, ': ']);
              DoColor(ccLightGray);
              started := true;
            end;
            C.fAssertionsBeforeRun := C.fAssertions;
            C.fAssertionsFailedBeforeRun := C.fAssertionsFailed;
            C.fRunConsoleOccurrenceNumber := fRunConsoleOccurrenceNumber;
            log := BeforeRun;
            TestTimer.Start;
            C.MethodSetup;
            try
              fCurrentMethodInfo^.Method(); // run tests + Check()
              AfterOneRun;
            finally
              C.MethodCleanUp;
            end;
            log := nil; // will trigger logging leave method e.g.
          except
            on E: Exception do
            begin
              DoColor(ccLightRed);
              AddFailed(E.ClassName + ': ' + E.Message);
              DoText(['! ', fCurrentMethodInfo^.IdentTestName]);
              if E.InheritsFrom(EControlC) then
                raise; // Control-C should just abort whole test
              DoTextLn([CRLF + '! ', GetLastExceptionText]); // with extended info
              DoColor(ccLightGray);
            end;
          end;
          if not started then
            continue;
          C.CleanUp; // should be done before Destroy call
          if C.AssertionsFailed = 0 then
            DoColor(ccLightGreen)
          else
            DoColor(ccLightRed);
          if C.fRunConsole <> '' then
          begin
            DoTextLn(['   ', C.fRunConsole]);
            C.fRunConsole := '';
          end;
          DoText(['  Total failed: ', IntToThousandString(C.AssertionsFailed),
            ' / ', IntToThousandString(C.Assertions), '  - ', C.Ident]);
          if C.AssertionsFailed = 0 then
            DoText(' PASSED')
          else
            DoText(' FAILED');
          DoTextLn(['  ', TotalTimer.Stop]);
          DoColor(ccLightGray);
          inc(fAssertions, C.fAssertions); // compute global assertions count
          inc(fAssertionsFailed, C.fAssertionsFailed);
        finally
          C.Free;
        end;
      end;
    finally
      fTestCaseClass := nil;
      fCurrentMethodInfo := nil;
    end;
  except
    on E: Exception do
    begin
      // assume any exception not intercepted above is a failure
      DoColor(ccLightRed);
      err := E.ClassName + ': ' + E.Message;
      AddFailed(err);
      DoText(['! ', err]);
    end;
  end;
  SetCurrentDir(dir);
  DoColor(ccLightCyan);
  result := (fFailedCount = 0);
  if Executable.Version.Major <> 0 then
    Version := FormatUtf8(CRLF +'Software version tested: % (%)',
      [Executable.Version.Detailed, Executable.Version.BuildDateTimeString]);
  FormatUtf8(CRLF + CRLF + 'Time elapsed for all tests: %' + CRLF +
    'Performed % by % on %',
    [RunTimer.Stop, NowToString, Executable.User, Executable.Host], Elapsed);
  DoTextLn([CRLF, Version, CustomVersions, CRLF +'Generated with: ',
    COMPILER_VERSION, ' ' + OS_TEXT + ' compiler', Elapsed]);
  if result then
    DoColor(ccWhite)
  else
    DoColor(ccLightRed);
  DoText([CRLF + 'Total assertions failed for all test suits:  ',
    IntToThousandString(AssertionsFailed), ' / ', IntToThousandString(Assertions)]);
  if result then
  begin
    DoColor(ccLightGreen);
    DoTextLn([CRLF + '! All tests passed successfully.']);
  end
  else
  begin
    DoTextLn([CRLF + '! Some tests FAILED: please correct the code.']);
    ExitCode := 1;
  end;
  DoColor(ccLightGray);
end;

procedure TSynTests.AfterOneRun;
var
  Run, Failed: integer;
  C: TSynTestCase;
begin
  if fCurrentMethodInfo = nil then
    exit;
  C := fCurrentMethodInfo^.Test as TSynTestCase;
  Run := C.Assertions - C.fAssertionsBeforeRun;
  Failed := C.AssertionsFailed - C.fAssertionsFailedBeforeRun;
  if fNotifyProgress <> '' then
  begin
    DoLog(sllMonitoring, '% %', [C, fNotifyProgress]);
    DoText(CRLF);
  end;
  if Failed = 0 then
  begin
    DoColor(ccGreen);
    if fNotifyProgress <> '' then
      DoText('        ')
    else
      DoText(['  - ', fCurrentMethodInfo^.TestName, ': ']);
    if Run = 0 then
      DoText('no assertion')
    else if Run = 1 then
      DoText('1 assertion passed')
    else
      DoText([IntToThousandString(Run), ' assertions passed']);
  end
  else
  begin
    DoColor(ccLightRed);   // ! to highlight the line
    DoText(['!  - ', fCurrentMethodInfo^.TestName, ': ', IntToThousandString(
      Failed), ' / ', IntToThousandString(Run), ' FAILED']);
  end;
  fNotifyProgress := '';
  DoText(['  ', TestTimer.Stop]);
  if C.fRunConsoleOccurrenceNumber > 0 then
    DoText(['  ', IntToThousandString(TestTimer.PerSec(
      C.fRunConsoleOccurrenceNumber)), '/s']);
  if C.fRunConsoleMemoryUsed > 0 then
  begin
    DoText(['  ', KB(C.fRunConsoleMemoryUsed)]);
    C.fRunConsoleMemoryUsed := 0; // display only once
  end;
  DoTextLn([]);
  if C.fRunConsole <> '' then
  begin
    DoTextLn(['     ', C.fRunConsole]);
    C.fRunConsole := '';
  end;
  DoColor(ccLightGray);
end;

class procedure TSynTests.DescribeCommandLine;
begin
  // do nothing by default - override with proper Executable.Command calls
end;

procedure TSynTests.SaveToFile(const DestPath: TFileName;
  const FileName: TFileName);
var
  FN: TFileName;
begin
  if TTextRec(fSaveToFile).Handle <> 0 then
    Close(fSaveToFile);
  if FileName = '' then
    FN := DestPath + Ident + '.txt'
  else
    FN := DestPath + FileName;
  if ExtractFilePath(FN) = '' then
    FN := Executable.ProgramFilePath + FN;
  system.assign(fSaveToFile, FN);
  rewrite(fSaveToFile);
  if IOResult <> 0 then
    FillCharFast(fSaveToFile, SizeOf(fSaveToFile), 0);
end;

procedure TSynTests.SaveToText(var aDest: System.Text);
begin
  if TTextRec(fSaveToFile).Handle <> 0 then
    Close(fSaveToFile);
  TTextRec(fSaveToFile) := TTextRec(aDest);
end;

{$I+}

class procedure TSynTests.RunAsConsole(const CustomIdent: string;
  withLogs: TSynLogLevels; options: TSynTestOptions; const workdir: TFileName);
var
  tests: TSynTests;
  redirect: TFileName;
  err: RawUtf8;
  restrict: TRawUtf8DynArray;
begin
  if self = TSynTests then
    raise ESynException.Create('You should inherit from TSynTests');
  // properly parse command line switches
  {$ifndef OSPOSIX}
  Executable.Command.Option('noenter', 'do not wait for ENTER key on exit');
  {$endif OSPOSIX}
  if Executable.Command.Arg(0, '#filename to redirect the console output') then
    Utf8ToFileName(Executable.Command.Args[0], redirect);
  Executable.Command.Get(['test'], restrict,
    'the #class.method name(s) to restrict the tests');
  DescribeCommandLine; // may be overriden to define additional parameters
  err := Executable.Command.DetectUnknown;
  if (err <> '') or
     Executable.Command.Option(['?', 'help'], 'display this message') or
     SameText(redirect, 'help') then
  begin
    ConsoleWrite(err);
    ConsoleWrite(Executable.Command.FullDescription);
    exit;
  end;
  // setup logs and console
  AllocConsole;
  RunFromSynTests := true; // set mormot.core.os.pas global flag
  with TSynLogTestLog.Family do
  begin
    Level := withLogs;
    PerThreadLog := ptIdentifiedInOneFile;
    HighResolutionTimestamp := not (tcoLogNotHighResolution in options);
    if (tcoLogVerboseRotate in options) and
       (Level = LOG_VERBOSE) then
    begin
      RotateFileCount := 10;
      RotateFileSizeKB := 100*1024; // rotate verbose logs by 100MB files
    end;
    if tcoLogInSubFolder in options then
      DestinationPath := EnsureDirectoryExists(Executable.ProgramFilePath + 'log');
  end;
  // testing is performed by some dedicated classes defined in the caller units
  tests := Create(CustomIdent);
  try
    if workdir <> '' then
      tests.WorkDir := workdir;
    tests.Options := options;
    tests.Restrict := restrict;
    if redirect <> '' then
    begin
      tests.SaveToFile(redirect); // export to file if named on command line
      {$I-} // minimal console output during blind regression tests
      Writeln(tests.Ident, CRLF + CRLF + ' Running tests... please wait');
      {$I+}
    end;
    tests.Run;
  finally
    tests.Free;
  end;
  {$ifndef OSPOSIX}
  if ParamCount = 0 then
  begin
    // direct exit if an external file was generated
    WriteLn(CRLF + 'Done - Press ENTER to Exit');
    ConsoleWaitForEnterKey;
  end;
  {$endif OSPOSIX}
end;



{ TSynTestsLogged }

function TSynTestsLogged.BeforeRun: IUnknown;
begin
  with fCurrentMethodInfo^ do
    result := TSynLogTestLog.Enter(Test, pointer(MethodName));
end;

constructor TSynTestsLogged.Create(const Ident: string);
begin
  inherited Create(Ident);
  with TSynLogTestLog.Family do
  begin
    if integer(Level) = 0 then // if no exception is set: at least main errors
      Level := [sllException, sllExceptionOS, sllFail];
    if AutoFlushTimeOut = 0 then
      // flush any pending text into .log file every 2 sec
      AutoFlushTimeOut := 2;
    fLogFile := Add;
  end;
end;

function SynTestsTextOut(var t: TTextRec): integer;
begin
  if t.BufPos = 0 then
    result := 0
  else
  begin
    if FileWrite(t.Handle, t.BufPtr^, t.BufPos) <> integer(t.BufPos) then
      result := GetLastError
    else
      result := 0;
    Append(PPRawUtf8(@t.UserData)^^, t.BufPtr, t.Bufpos);
    t.BufPos := 0;
  end;
end;

procedure TSynTestsLogged.CreateSaveToFile;
begin
  inherited;
  with TTextRec(fSaveToFile) do
  begin
    InOutFunc := @SynTestsTextOut;
    FlushFunc := @SynTestsTextOut;
    PPRawUtf8(@UserData)^ := @fConsoleDup;
  end;
end;

destructor TSynTestsLogged.Destroy;
begin
  if (fLogFile <> nil) and
     (fConsoleDup <> '') then
    fLogFile.LogLines(sllCustom1, pointer(fConsoleDup), nil, '  ----');
  fLogFile.Log(sllMemory, '', self);
  inherited Destroy;
end;

procedure TSynTestsLogged.AddFailed(const msg: string);
begin
  inherited AddFailed(msg);
  if fCurrentMethodInfo <> nil then
    with fCurrentMethodInfo^ do
      fLogFile.Log(sllFail, '% [%]', [IdentTestName, msg], Test)
  else
    fLogFile.Log(sllFail, 'no context', self)
end;


end.

