/// Framework Core Unit and Regression Testing
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.test;

{
  *****************************************************************************

   Testing functions shared by all framework units
    - Unit-Testing classes and functions
    - TInterfaceMock for Dependency Mocking

  *****************************************************************************
}


interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.data,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.perf,
  mormot.core.log,
  mormot.core.threads,
  mormot.core.interfaces,
  mormot.net.client;


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
    fAssertions: integer; // defined here for padding - used by TSynTestCase
    fAssertionsFailed: integer;
    fInternalTestsCount: integer;
    fOptions: TSynTestOptions;
    fTests: array of TSynTestMethodInfo;
    fIdent: string;
    fWorkDir: TFileName;
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
    fAssertionsBeforeRun: integer;
    fAssertionsFailedBeforeRun: integer;
    fBackgroundRun: TLoggedWorker;
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
    procedure OnBeforeEachBackgroundTask(Sender: TObject);
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
    // ! if CheckFailed(A = 10) then exit;
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
    /// used by the published methods to run test assertion against UTF-8/Ansi strings
    // - will ignore the a+b string codepages, and call SortDynArrayRawByteString()
    // - if a<>b, will fail and include '#<>#' text before the supplied msg
    function CheckEqual(const a, b: RawByteString; const msg: RawUtf8 = ''): boolean; overload;
    /// used by the published methods to run test assertion against UTF-8/Ansi strings
    // - will ignore the a+b string codepages, and call SortDynArrayRawByteString()
    // - if a<>b, will fail and include '#<>#' text before the supplied msg
    function CheckEqualShort(const a, b: shortstring; const msg: RawUtf8 = ''): boolean;
    /// used by the published methods to run test assertion against
    // - if BinToHexLower(a)<>b, will fail and include '#<>#' hexa before the supplied msg
    function CheckEqualHex(const a: RawByteString; const b: RawUtf8;
      const msg: RawUtf8 = ''): boolean;
    /// used by the published methods to run test assertion against UTF-8/Ansi strings
    // - if Trim(a)<>Trim(b), will fail and include '#<>#' text before the supplied msg
    function CheckEqualTrim(const a, b: RawByteString; const msg: RawUtf8 = ''): boolean;
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
    {$ifndef PUREMORMOT2}
    class function RandomString(CharCount: integer): WinAnsiString;
      {$ifdef HASINLINE}inline;{$endif}
    {$endif PUREMORMOT2}
    /// create a temporary string, containing some fake text, with paragraphs
    class function RandomTextParagraph(WordCount: integer; LastPunctuation: AnsiChar = '.';
      const RandomInclude: RawUtf8 = ''): RawUtf8;
    /// add containing some "bla bli blo blu" fake text, with paragraphs
    class procedure AddRandomTextParagraph(WR: TTextWriter; WordCount: integer;
      LastPunctuation: AnsiChar = '.'; const RandomInclude: RawUtf8 = '';
      NoLineFeed: boolean = false);
    /// safely download some reference material (e.g. from api.github.com)
    // - with proper retry if the server denies it, due to a rate limit
    function DownloadFile(const uri: RawUtf8; localfile: TFileName = '';
      retry: integer = 3): RawByteString;
    /// execute a method possibly in a dedicated TLoggedWorkThread
    // - OnTask() should take some time running, to be worth a thread execution
    // - won't create more background threads than currently available CPU cores,
    // to avoid resource exhaustion and unexpected timeouts on smaller computers,
    // unless ForcedThreaded is used and then an internal queue is used to
    // force all taks to be executed in their own thread
    procedure Run(const OnTask: TNotifyEvent; Sender: TObject;
      const TaskName: RawUtf8; Threaded: boolean = true; NotifyTask: boolean = true;
      ForcedThreaded: boolean = false);
    /// wait for background thread started by Run() to finish
    procedure RunWait(NotifyThreadCount: boolean = true; TimeoutSec: integer = 60;
      CallSynchronize: boolean = true);
    /// this method is triggered internally - e.g. by Check() - when a test failed
    procedure TestFailed(const msg: string); overload;
    /// this method can be triggered directly - e.g. after CheckFailed() = true
    procedure TestFailed(const msg: RawUtf8; const args: array of const); overload;
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
    fAssertionsFailed: integer;
    fSafe: TOSLock;
    fTestCaseClass: array of TSynTestCaseClass;
    /// any number not null assigned to this field will display a "../sec" stat
    fRunConsoleOccurrenceNumber: cardinal;
    fMultiThread: boolean;
    fFailed: TSynTestFaileds;
    fFailedCount: integer;
    fNotifyProgressLineLen: integer;
    fNotifyProgress: RawUtf8;
    fSaveToFileBeforeExternal: THandle;
    fRestrict: TRawUtf8DynArray;
    procedure EndSaveToFileExternal;
    function IsRestricted(const name: RawUtf8): boolean;
    function GetFailedCount: integer;
    function GetFailed(Index: integer): TSynTestFailed;
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
    // - method implementation can use CurrentMethodInfo^ to get run context
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
    // - will also redirect the main StdOut variable into the specified file
    procedure SaveToFile(const DestPath: TFileName; const FileName: TFileName = '');
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
    /// method information currently running
    // - is set by Run and available within TTestCase methods
    function CurrentMethodInfo: PSynTestMethodInfo;
    /// number of failed tests after the last call to the Run method
    property FailedCount: integer
      read GetFailedCount;
    /// retrieve the information associated with a failure
    property Failed[Index: integer]: TSynTestFailed
      read GetFailed;
    /// list of 'class.method' names to restrict the tests for Run
    // - as retrieved from "--test class.method" command line switch
    property Restrict: TRawUtf8DynArray
      read fRestrict write fRestrict;
    /// if the "--multithread" switch has been defined at command line
    property MultiThread: boolean
      read fMultiThread;
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
    procedure CustomConsoleOutput(const value: RawUtf8);
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


const // include some line feed for easier comparison/debugging in the console
  EQUAL_MSG    = '<>' + CRLF + '%' + CRLF + '% %';
  NOTEQUAL_MSG = '='  + CRLF + '%' + CRLF + '% %';

var
  /// the kind of .log file generated by TSynTestsLogged
  TSynLogTestLog: TSynLogClass = TSynLog;


{ ************ TInterfaceMock for Dependency Mocking }

type
  /// used to mock an interface implementation via expect-run-verify pattern
  // - TInterfaceStub will raise an exception on Fails(), ExpectsCount() or
  // ExpectsTrace() rule activation, but TInterfaceMock will call
  // TSynTestCase.Check() with no exception with such rules, as expected by
  // a mocked interface
  // - this class will follow the expect-run-verify pattern, i.e. expectations
  // are defined before running the test, and verification is performed
  // when the instance is released - use TInterfaceMockSpy if you prefer the
  // more explicit run-verify pattern
  TInterfaceMock = class(TInterfaceStub)
  protected
    fTestCase: TSynTestCase;
    function InternalCheck(aValid, aExpectationFailed: boolean;
      const aErrorMsgFmt: RawUtf8;
      const aErrorMsgArgs: array of const): boolean; override;
  public
    /// initialize an interface mock from TypeInfo(IMyInterface)
    // - aTestCase.Check() will be called in case of mocking failure
    // ! procedure TMyTestCase.OneTestCaseMethod;
    // ! var Persist: IPersistence;
    // ! ...
    // !   TInterfaceMock.Create(TypeInfo(IPersistence),Persist,self).
    // !     ExpectsCount('SaveItem',qoEqualTo,1)]);
    constructor Create(aInterface: PRttiInfo; out aMockedInterface;
      aTestCase: TSynTestCase); reintroduce; overload;
    /// initialize an interface mock from an interface TGuid
    // - aTestCase.Check() will be called during validation of all Expects*()
    // - you shall have registered the interface by a previous call to
    // ! TInterfaceFactory.RegisterInterfaces([TypeInfo(IPersistence),...])
    // - once registered, create and use the fake class instance as such:
    // !procedure TMyTestCase.OneTestCaseMethod;
    // !var
    // !  Persist: IPersistence;
    // ! ...
    // !   TInterfaceMock.Create(IPersistence,Persist,self).
    // !     ExpectsCount('SaveItem',qoEqualTo,1)]);
    // - if the supplied TGuid has not been previously registered, raise an Exception
    constructor Create(const aGuid: TGuid; out aMockedInterface;
      aTestCase: TSynTestCase); reintroduce; overload;
    /// initialize an interface mock from an interface name (e.g. 'IMyInterface')
    // - aTestCase.Check() will be called in case of mocking failure
    // - you shall have registered the interface by a previous call to
    // TInterfaceFactory.Get(TypeInfo(IMyInterface)) or RegisterInterfaces()
    // - if the supplied name has not been previously registered, raise an Exception
    constructor Create(const aInterfaceName: RawUtf8; out aMockedInterface;
      aTestCase: TSynTestCase); reintroduce; overload;
    /// initialize an interface mock from TypeInfo(IMyInterface) for later injection
    // - aTestCase.Check() will be called in case of mocking failure
    constructor Create(aInterface: PRttiInfo; aTestCase: TSynTestCase);
      reintroduce; overload;
    /// initialize an interface mock from TypeInfo(IMyInterface) for later injection
    // - aTestCase.Check() will be called in case of mocking failure
    constructor Create(const aGuid: TGuid; aTestCase: TSynTestCase);
      reintroduce; overload;
    /// the associated test case
    property TestCase: TSynTestCase
      read fTestCase;
  end;

  /// how TInterfaceMockSpy.Verify() shall generate the calls trace
  TInterfaceMockSpyCheck = (
    chkName,
    chkNameParams,
    chkNameParamsResults);

  /// used to mock an interface implementation via run-verify pattern
  // - this class will implement a so called "test-spy" mocking pattern, i.e.
  // no expectation is to be declared at first, but all calls are internally
  // logged (i.e. it force imoLogMethodCallsAndResults option to be defined),
  // and can afterwards been check via Verify() calls
  TInterfaceMockSpy = class(TInterfaceMock)
  protected
    procedure IntSetOptions(Options: TInterfaceStubOptions); override;
  public
    /// this will set and force imoLogMethodCallsAndResults option as needed
    // - you should not call this method, but the overloaded alternatives
    constructor Create(aFactory: TInterfaceFactory;
      const aInterfaceName: RawUtf8); override;
    /// check that a method has been called a specify number of times
    procedure Verify(const aMethodName: RawUtf8;
      aOperator: TInterfaceStubRuleOperator = ioGreaterThan;
      aCount: cardinal = 0); overload;
    /// check a method calls count with a set of parameters
    // - parameters shall be defined as a JSON array of values
    procedure Verify(const aMethodName, aParams: RawUtf8;
      aOperator: TInterfaceStubRuleOperator = ioGreaterThan;
      aCount: cardinal = 0); overload;
    /// check a method calls count with a set of parameters
    // - parameters shall be defined as a JSON array of values
    procedure Verify(const aMethodName: RawUtf8; const aParams: array of const;
      aOperator: TInterfaceStubRuleOperator = ioGreaterThan;
      aCount: cardinal = 0); overload;
    /// check an execution trace for the global interface
    // - text trace format shall follow method calls, e.g.
    // ! Verify('Multiply,Add',chkName);
    // or may include parameters:
    // ! Verify('Multiply(10,30),Add(2,35)',chkNameParams);
    // or include parameters and function results:
    // ! Verify('Multiply(10,30)=[300],Add(2,35)=[37]',chkNameParamsResults);
    procedure Verify(const aTrace: RawUtf8;
      aScope: TInterfaceMockSpyCheck); overload;
    /// check an execution trace for a specified method
    // - text trace format will follow specified scope, e.g.
    // ! Verify('Add','(10,30),(2,35)',chkNameParams);
    // or include parameters and function results:
    // ! Verify('Add','(10,30)=[300],(2,35)=[37]',chkNameParamsResults);
    // - if aMethodName does not exists or aScope=chkName, will raise an exception
    procedure Verify(const aMethodName, aTrace: RawUtf8;
      aScope: TInterfaceMockSpyCheck); overload;
    /// check an execution trace for a specified method and parameters
    // - text trace format shall contain only results, e.g.
    // ! Verify('Add','2,35','[37]');
    procedure Verify(const aMethodName, aParams, aTrace: RawUtf8); overload;
    /// check an execution trace for a specified method and parameters
    // - text trace format shall contain only results, e.g.
    // ! Verify('Add',[2,35],'[37]');
    procedure Verify(const aMethodName: RawUtf8; const aParams: array of const;
      const aTrace: RawUtf8); overload;
  end;

function ToText(c: TInterfaceMockSpyCheck): PShortString; overload;


implementation


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
    if IdemPChar(pointer(id), 'TSYN') then
      if IdemPChar(pointer(id), 'TSYNTEST') then
        Delete(id, 1, 8)
      else
        Delete(id, 1, 4)
    else if IdemPChar(pointer(id), 'TTEST') then
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
  fBackgroundRun.Free;
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
    crc := DefaultHasher(0, pointer(msg), length(msg) * SizeOf(msg[1]));
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
  fOwner.DoLog(LEV[condition], '%', [msg]);
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

function TSynTestCase.CheckEqual(const a, b: RawByteString; const msg: RawUtf8): boolean;
begin
  inc(fAssertions);
  result := SortDynArrayRawByteString(a, b) = 0;
  if not result or
     (tcoLogEachCheck in fOptions) then
    DoCheckUtf8(result, EQUAL_MSG, [a, b, msg]);
end;

function TSynTestCase.CheckEqualShort(const a, b: shortstring; const msg: RawUtf8): boolean;
begin
  inc(fAssertions);
  result := (a = b);
  if not result or
     (tcoLogEachCheck in fOptions) then
    DoCheckUtf8(result, EQUAL_MSG, [a, b, msg]);
end;

function TSynTestCase.CheckEqualHex(const a: RawByteString; const b, msg: RawUtf8): boolean;
var
  hex: RawUtf8;
begin
  BinToHexLower(pointer(a), length(a), hex);
  result := IdemPropNameU(hex, b);
  if not result or
     (tcoLogEachCheck in fOptions) then
    DoCheckUtf8(result, EQUAL_MSG, [hex, b, msg]);
end;

function TSynTestCase.CheckEqualTrim(const a, b: RawByteString; const msg: RawUtf8): boolean;
begin
  result := CheckEqual(TrimU(a), TrimU(b), msg);
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

{$ifndef PUREMORMOT2}
class function TSynTestCase.RandomString(CharCount: integer): WinAnsiString;
begin
  result := RandomWinAnsi(CharCount);
end;
{$endif PUREMORMOT2}

class function TSynTestCase.RandomTextParagraph(WordCount: integer;
  LastPunctuation: AnsiChar; const RandomInclude: RawUtf8): RawUtf8;
var
  tmp: TTextWriterStackBuffer; // 8KB work buffer on stack
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
  bla: array[0 .. 15] of TShort3 = (
    'bla', 'ble', 'bli', 'blo', 'blu', 'bla', 'bli', 'blo',
    'cha', 'che', 'chi', 'cho', 'chu', 'cha', 'chi', 'cho');
  endKind = [dot, paragraph, question];
var
  n: integer;
  s: TShort4;
  last: TKind;
  rnd: cardinal;
begin
  last := paragraph;
  while WordCount > 0 do
  begin
    rnd := Random32;  // get 32 bits of randomness for up to 5 words per loop
    n := (rnd and 3) + 2;  // n = 2..5
    rnd := rnd shr 2;      // consume 2 bits
    repeat
      PCardinal(@s)^ := PCardinal(@bla[rnd and 15])^;
      rnd := rnd shr 4;    // consume up to 5*4 = 20 bits from rnd
      s[0] := #4;
      s[4] := ' ';
      if last in endKind then
      begin
        last := space;
        s[1] := 'P';
      end;
      WR.AddShorter(s);
      dec(WordCount);
      if WordCount = 0 then
        break;
      dec(n);
    until n = 0;
    WR.CancelLastChar(' ');
    case rnd and 127 of // consume 7 bits from rnd (total up to 29 bits)
      0 .. 4:
        begin
          if RandomInclude <> '' then
          begin
            WR.AddDirect(' ');
            WR.AddString(RandomInclude); // 5/128 = 4% chance of text inclusion
          end;
          last := space;
        end;
      5 .. 50:
        last := space;
      51 .. 90:
        last := comma;
      91 .. 105:
        last := dot;
      106 .. 115:
        last := question;
      116 .. 127:
        if NoLineFeed then
          last := dot
        else
          last := paragraph;
    end;
    case last of
      space:
        WR.AddDirect(' ');
      comma:
        WR.AddDirect(',', ' ');
      dot:
        WR.AddDirect('.', ' ');
      question:
        WR.AddDirect('?', ' ');
      paragraph:
        WR.AddDirect('.', #13, #10);
    end;
  end;
  if (LastPunctuation <> ' ') and
     not (last in endKind) then
    WR.AddDirect('b', 'l', 'a', LastPunctuation);
end;

function TSynTestCase.DownloadFile(const uri: RawUtf8;
  localfile: TFileName; retry: integer): RawByteString;
var
  status: integer;
  info: string;
begin
  if localfile <> '' then
    if not IsExpandedPath(localfile) then
      localfile := WorkDir + localfile;
  repeat
    result := HttpGetWeak(uri, localfile, @status);
    FormatString('DownloadFile %=% retry=% [%]',
      [uri, status, retry, EscapeToShort(result)], info);
    fOwner.DoLog(sllTrace, '%', [info]);
    if status = HTTP_SUCCESS then
      exit;
    AddConsole(info); // notify something if not as expected (rate limit?)
    dec(retry);
    if retry <= 0 then
      exit;
    SleepHiRes(10);
  until false;
end;

threadvar
  _CurrentMethodInfo: PSynTestMethodInfo;

procedure TSynTestCase.OnBeforeEachBackgroundTask(Sender: TObject);
begin
  // executed in the context of the background thread
  _CurrentMethodInfo := fOwner.CurrentMethodInfo;
end;

procedure TSynTestCase.Run(const OnTask: TNotifyEvent; Sender: TObject;
  const TaskName: RawUtf8; Threaded, NotifyTask, ForcedThreaded: boolean);
begin
  if NotifyTask or
     not fOwner.fMultiThread or
     not Threaded then
    NotifyProgress([TaskName]);
  if not Assigned(OnTask) then
    exit;
  if not fOwner.fMultiThread or // avoid timeout e.g. on slow VMs
     not Threaded then
    OnTask(Sender) // run in main thread
  else
  begin
    if fBackgroundRun = nil then
    begin
      fBackgroundRun := TLoggedWorker.Create(TSynLogTestLog);
      fBackgroundRun.OnBeforeEachTask := OnBeforeEachBackgroundTask;
    end;
    fOwner.DoLog(sllDebug, 'Run(%,%) using %',
      [TaskName, ForcedThreaded, fBackgroundRun]);
    fBackgroundRun.Run(OnTask, Sender, TaskName, ForcedThreaded);
  end;
end;

procedure TSynTestCase.RunWait(NotifyThreadCount: boolean; TimeoutSec: integer;
  CallSynchronize: boolean);
var
  timer: TPrecisionTimer;
begin
  if not fBackgroundRun.Waiting then
    exit;
  if NotifyThreadCount then
  begin
    timer.Start;
    NotifyProgress(['waiting for ', Plural('thread', fBackgroundRun.Running), ':']);
  end;
  if not fBackgroundRun.RunWait(TimeoutSec, CallSynchronize) then
    TestFailed(' error: timeout after % sec' + CRLF, [TimeoutSec])
  else if NotifyThreadCount then
    NotifyProgress([timer.Stop]);
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

procedure TSynTestCase.TestFailed(const msg: RawUtf8; const args: array of const);
begin
  fOwner.DoLog(sllFail, msg, Args);
end;

procedure TSynTestCase.AddConsole(const msg: string; OnlyLog: boolean);
begin
  fOwner.DoLog(sllMonitoring, '%', [msg]);
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
    PtrArrayAddOnce(fTestCaseClass, TestCase[i]);
end;

procedure TSynTests.AddCase(TestCase: TSynTestCaseClass);
begin
  PtrArrayAddOnce(fTestCaseClass, TestCase);
end;

function TSynTests.BeforeRun: IUnknown;
begin
  result := nil;
end;

constructor TSynTests.Create(const Ident: string);
begin
  fSafe.Init;
  inherited Create(Ident);
end;

procedure TSynTests.EndSaveToFileExternal;
begin
  if fSaveToFileBeforeExternal = 0 then
    exit;
  FileClose(StdOut);
  StdOut := fSaveToFileBeforeExternal;
  fSaveToFileBeforeExternal := 0;
end;

destructor TSynTests.Destroy;
begin
  EndSaveToFileExternal;
  inherited Destroy;
  fSafe.Done;
end;

procedure TSynTests.DoColor(aColor: TConsoleColor);
begin
  if fSaveToFileBeforeExternal = 0 then
    TextColor(aColor);
end;

procedure TSynTests.DoText(const value: RawUtf8);
begin
  ConsoleWrite(value, ccLightGray, {nolf=}true, {nocolor=}true);
  if Assigned(CustomOutput) then
    CustomOutput(value);
end;

procedure TSynTests.DoText(const values: array of const);
var
  s: RawUtf8;
begin
  Make(values, s);
  DoText(s);
end;

procedure TSynTests.DoTextLn(const values: array of const);
var
  s: RawUtf8;
begin
  Make(values, s, {includelast=}CRLF);
  DoText(s);
end;

function TSynTests.CurrentMethodInfo: PSynTestMethodInfo;
begin
  result := _CurrentMethodInfo;
end;

procedure TSynTests.DoNotifyProgress(const value: RawUtf8; cc: TConsoleColor);
var
  len: integer;
  nfo: PSynTestMethodInfo;
begin
  if fNotifyProgress = '' then
  begin
    DoColor(ccGreen);
    nfo := _CurrentMethodInfo;
    if nfo <> nil then
      DoText(['  - ', nfo^.TestName, ':' + CRLF + '     '])
    else
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
var
  txt: RawUtf8;
begin
  if Level <> sllFail then
    if (TSynLogTestLog = nil) or
       not (Level in TSynLogTestLog.Family.Level) then
      exit;
  FormatUtf8(TextFmt, TextArgs, txt);
  if _CurrentMethodInfo <> nil then
    Prepend(txt, [_CurrentMethodInfo^.TestName, ': ']);
  if Level = sllFail then
    TSynLogTestLog.DebuggerNotify(Level, txt)
  else
    TSynLogTestLog.Add.Log(Level, txt)
end;

procedure TSynTests.AddFailed(const msg: string);
var
  nfo: PSynTestMethodInfo;
begin
  if fFailedCount = length(fFailed) then
    SetLength(fFailed, NextGrow(fFailedCount));
  with fFailed[fFailedCount] do
  begin
    Error := msg;
    nfo := _CurrentMethodInfo;
    if nfo <> nil then
    begin
      TestName := nfo^.TestName;
      IdentTestName := nfo^.IdentTestName;
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

function TSynTests.IsRestricted(const name: RawUtf8): boolean;
var
  i: PtrInt;
begin
  result := false;
  if (fRestrict = nil) or
     (FindPropName(pointer(fRestrict), name, length(fRestrict)) >= 0) then
    exit;
  for i := 0 to length(fRestrict) - 1 do
    if PosExI(fRestrict[i], name) <> 0 then
      exit;
  result := true;
end;

function TSynTests.Run: boolean;
var
  i, t, m: integer;
  Elapsed, Version, s: RawUtf8;
  methods: TRawUtf8DynArray;
  nfo: PSynTestMethodInfo;
  dir: TFileName;
  err: string;
  started, titledone: boolean;
  c: TSynTestCase;
  log: IUnknown;
begin
  result := true;
  if Executable.Command.Option('multithread') then
    fMultiThread := SystemInfo.dwNumberOfProcessors > 2; // enabled with 3 cores
  if Executable.Command.Option('&methods') then
  begin
    for m := 0 to Count - 1 do
      fTests[m].Method();
    for i := 0 to high(fTestCaseClass) do
      if not IsRestricted(ToText(fTestCaseClass[i])) then
      begin
        methods := GetPublishedMethodNames(fTestCaseClass[i]);
        for m := 0 to high(methods) do
          Append(s, [fTestCaseClass[i], '.', methods[m], CRLF]);
      end;
    DoText(s);
    exit;
  end
  else if Executable.Command.Option(['l', 'tests']) then
  begin
    for m := 0 to Count - 1 do
      fTests[m].Method();
    for i := 0 to high(fTestCaseClass) do
      Append(s, [fTestCaseClass[i], CRLF]);
    DoText(s);
    exit;
  end;
  // main loop processing all TSynTestCase instances
  DoColor(ccLightCyan);
  DoTextLn([CRLF + '   ', Ident,
            CRLF + '  ', RawUtf8OfChar('-', length(Ident) + 2)]);
  RunTimer.Start;
  fFailed := nil;
  fAssertions := 0;
  fAssertionsFailed := 0;
  dir := GetCurrentDir;
  repeat
    for m := 0 to Count - 1 do
    try
      titledone := false;
      fTests[m].Method(); // call AddCase() to add instances into fTestCaseClass
      try
        for i := 0 to high(fTestCaseClass) do
        begin
          started := false;
          c := fTestCaseClass[i].Create(self); // add all published methods
          try
            nfo := nil;
            for t := 0 to c.Count - 1 do
            try
              nfo := @c.fTests[t];
              _CurrentMethodInfo := nfo;
              // e.g. --test TNetworkProtocols.DNSAndLDAP or --test dns
              if IsRestricted(ToText(c.ClassType)) and
                 IsRestricted(FormatUtf8('%.%', [c, nfo^.MethodName])) then
                continue;
              if not titledone then
              begin
                titledone := true;
                DoColor(ccWhite);
                DoTextLn([CRLF + CRLF, m + 1, '. ', fTests[m].TestName]);
                DoColor(ccLightGray);
              end;
              if not started then
              begin
                c.fAssertions := 0; // reset assertions count
                c.fAssertionsFailed := 0;
                c.fWorkDir := fWorkDir;
                SetCurrentDir(fWorkDir);
                TotalTimer.Start;
                c.Setup;
                DoColor(ccWhite);
                DoTextLn([CRLF + ' ', m + 1, '.', i + 1, '. ', c.Ident, ': ']);
                DoColor(ccLightGray);
                started := true;
              end;
              c.fAssertionsBeforeRun := c.fAssertions;
              c.fAssertionsFailedBeforeRun := c.fAssertionsFailed;
              c.fRunConsoleOccurrenceNumber := fRunConsoleOccurrenceNumber;
              log := BeforeRun;
              TestTimer.Start;
              c.MethodSetup;
              try
                nfo^.Method(); // run tests + Check()
                AfterOneRun;
              finally
                c.MethodCleanUp;
                log := nil; // will trigger logging leave method e.g.
              end;
            except
              on E: Exception do
              begin
                DoColor(ccLightRed);
                AddFailed(E.ClassName + ': ' + E.Message);
                if nfo <> nil then
                  DoTextLn(['! ', nfo^.IdentTestName]);
                if E.InheritsFrom(EControlC) then
                  raise; // Control-C should just abort whole test
                {$ifndef NOEXCEPTIONINTERCEPT}
                DoTextLn(['! ', GetLastExceptionText]); // with extended info
                {$endif NOEXCEPTIONINTERCEPT}
                DoColor(ccLightGray);
              end;
            end;
            _CurrentMethodInfo := nil;
            if not started then
              continue;
            if c.fBackgroundRun.Waiting then
              c.RunWait({notify=}false, {timeout=}120, {synchronize=}true);
            c.CleanUp; // to be done before Destroy call and after RunWait()
            if c.AssertionsFailed = 0 then
              DoColor(ccLightGreen)
            else
              DoColor(ccLightRed);
            s := '';
            if c.fRunConsole <> '' then
            begin
              Make(['   ', c.fRunConsole, CRLF], s);
              c.fRunConsole := '';
            end;
            Append(s, ['  Total failed: ', IntToThousandString(c.AssertionsFailed),
              ' / ', IntToThousandString(c.Assertions), ' - ', c.Ident]);
            if c.AssertionsFailed = 0 then
              AppendShortToUtf8(' PASSED', s)
            else
              AppendShortToUtf8(' FAILED', s);
            Append(s, ['  ', TotalTimer.Stop, CRLF]);
            DoText(s); // write at once to the console output
            DoColor(ccLightGray);
            inc(fAssertions, c.fAssertions); // compute global assertions count
            inc(fAssertionsFailed, c.fAssertionsFailed);
          finally
            FreeAndNil(c);
          end;
        end;
      finally
        _CurrentMethodInfo := nil;
        fTestCaseClass := nil; // unregister the test classes once run
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
  until (fAssertionsFailed <> 0) or
        not Executable.Command.Option('loop');
  SetCurrentDir(dir);
  DoColor(ccLightCyan);
  result := (fFailedCount = 0);
  if Executable.Version.Major <> 0 then
    FormatUtf8(CRLF +'Software version tested: % (%)', [Executable.Version.Detailed,
      Executable.Version.BuildDateTimeString], Version);
  FormatUtf8(CRLF + CRLF + 'Time elapsed for all tests: %' + CRLF +
    'Performed % by % on %',
    [RunTimer.Stop, NowToHuman, Executable.User, Executable.Host], Elapsed);
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
  nfo: PSynTestMethodInfo;
  C: TSynTestCase;
  s: RawUtf8;
begin
  nfo := _CurrentMethodInfo;
  if nfo = nil then
    exit;
  C := nfo^.Test as TSynTestCase;
  Run := C.Assertions - C.fAssertionsBeforeRun;
  Failed := C.AssertionsFailed - C.fAssertionsFailedBeforeRun;
  if fNotifyProgress <> '' then
  begin
    DoLog(sllMonitoring, '% %', [C, fNotifyProgress]);
    s := CRLF;
  end;
  if Failed = 0 then
  begin
    DoColor(ccGreen);
    if fNotifyProgress <> '' then
      Append(s, '        ')
    else
      Append(s, ['  - ', nfo^.TestName, ': ']);
    if Run = 0 then
      Append(s, 'no assertion')
    else if Run = 1 then
      Append(s, '1 assertion passed')
    else
      Append(s, [IntToThousandString(Run), ' assertions passed']);
  end
  else
  begin
    DoColor(ccLightRed);   // ! to highlight the line
    Append(s, ['!  - ', nfo^.TestName, ': ', IntToThousandString(
      Failed), ' / ', IntToThousandString(Run), ' FAILED']);
  end;
  fNotifyProgress := '';
  Append(s, ['  ', TestTimer.Stop]);
  if C.fRunConsoleOccurrenceNumber > 0 then
    Append(s, ['  ', IntToThousandString(TestTimer.PerSec(
      C.fRunConsoleOccurrenceNumber)), '/s']);
  if C.fRunConsoleMemoryUsed > 0 then
  begin
    Append(s, ['  ', KB(C.fRunConsoleMemoryUsed)]);
    C.fRunConsoleMemoryUsed := 0; // display only once
  end;
  Append(s, CRLF);
  if C.fRunConsole <> '' then
  begin
    Append(s, ['     ', C.fRunConsole, CRLF]);
    C.fRunConsole := '';
  end;
  DoText(s); // append whole information at once to the console
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
  h: THandle;
begin
  EndSaveToFileExternal;
  if FileName = '' then
    if (Ident <> '') and
       SafeFileName(Ident) then
      FN := DestPath + Ident + '.txt'
    else
      FN := DestPath + Utf8ToString(Executable.ProgramName) + '.txt'
  else
    FN := DestPath + FileName;
  if ExtractFilePath(FN) = '' then
    FN := Executable.ProgramFilePath + FN;
  h := FileCreate(FN);
  if not ValidHandle(h) then
    exit;
  fSaveToFileBeforeExternal := StdOut; // backup
  StdOut := h;
end;

class procedure TSynTests.RunAsConsole(const CustomIdent: string;
  withLogs: TSynLogLevels; options: TSynTestOptions; const workdir: TFileName);
var
  tests: TSynTests;
  redirect: TFileName;
  err: RawUtf8;
  restrict: TRawUtf8DynArray;
begin
  if self = TSynTests then
    ESynException.RaiseU('RunAsConsole: you should inherit from TSynTests');
  // properly parse command line switches
  {$ifndef OSPOSIX}
  Executable.Command.Option('noenter', 'do not wait for ENTER key on exit');
  {$endif OSPOSIX}
  redirect := Executable.Command.ArgFile(0,
    '#filename to redirect the console output');
  Executable.Command.Get(['t', 'test'], restrict,
    'restrict the tests to a #class[.method] name(s)');
  Executable.Command.Option(['l', 'tests'],
    'list all class name(s) as expected by --test');
  Executable.Command.Option('&methods',
    'list all method name(s) of #class as specified to --test');
  Executable.Command.Option('multithread',
    'parallelize tests execution on multi-core CPU');
  if Executable.Command.Option('&verbose',
       'run logs in verbose mode: enabled only with --test') and
     (restrict <> nil) then
    withLogs := LOG_VERBOSE;
  Executable.Command.Option('loop',
    'loop the tests until some assertion failed');
  if options = [] then
    SetValueFromExecutableCommandLine(options, TypeInfo(TSynTestOptions),
      '&options', 'refine logs output content');
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
    PerThreadLog := ptIdentifiedInOneFile;
    HighResolutionTimestamp := not (tcoLogNotHighResolution in options);
    if (tcoLogVerboseRotate in options) and
       (Level = LOG_VERBOSE) then
    begin
      RotateFileCount := 10;
      RotateFileSizeKB := 100 shl 10; // rotate verbose logs by 100MB files
    end;
    if tcoLogInSubFolder in options then
      DestinationPath := EnsureDirectoryExists([Executable.ProgramFilePath, 'log']);
    Level := withLogs; // better be set last
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
      // minimal console output during blind regression tests
      tests.DoTextLn([tests.Ident, CRLF + CRLF + ' Running tests... please wait']);
      tests.SaveToFile(redirect); // export to file if named on command line
    end;
    tests.Run;
  finally
    tests.Free;
  end;
  {$ifndef OSPOSIX}
  if ParamCount = 0 then // Executable.Command.Option('noenter') not needed
  begin
    // direct exit if an external file was generated
    ConsoleWrite(CRLF + 'Done - Press ENTER to Exit');
    ConsoleWaitForEnterKey;
  end;
  {$endif OSPOSIX}
end;



{ TSynTestsLogged }

function TSynTestsLogged.BeforeRun: IUnknown;
begin
  with _CurrentMethodInfo^ do
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
  CustomOutput := CustomConsoleOutput; // redirect lines to the log
end;

procedure TSynTestsLogged.CustomConsoleOutput(const value: RawUtf8);
begin
  Append(fConsoleDup, value);
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
var
  nfo: PSynTestMethodInfo;
begin
  inherited AddFailed(msg);
  nfo := _CurrentMethodInfo;
  if nfo <> nil then
    fLogFile.Log(sllFail, '% [%]', [nfo^.IdentTestName, msg], nfo^.Test)
  else
    fLogFile.Log(sllFail, 'no context', self)
end;


{ ************ TInterfaceMock for Dependency Mocking }

{ TInterfaceMock }

constructor TInterfaceMock.Create(aInterface: PRttiInfo; out aMockedInterface;
  aTestCase: TSynTestCase);
begin
  inherited Create(aInterface, aMockedInterface);
  fTestCase := aTestCase;
end;

constructor TInterfaceMock.Create(const aGuid: TGuid; out aMockedInterface;
  aTestCase: TSynTestCase);
begin
  inherited Create(aGuid, aMockedInterface);
  fTestCase := aTestCase;
end;

constructor TInterfaceMock.Create(const aInterfaceName: RawUtf8;
  out aMockedInterface; aTestCase: TSynTestCase);
begin
  inherited Create(aInterfaceName, aMockedInterface);
  fTestCase := aTestCase;
end;

constructor TInterfaceMock.Create(aInterface: PRttiInfo; aTestCase: TSynTestCase);
begin
  inherited Create(aInterface);
  fTestCase := aTestCase;
end;

constructor TInterfaceMock.Create(const aGuid: TGuid; aTestCase: TSynTestCase);
begin
  inherited Create(aGuid);
  fTestCase := aTestCase;
end;

function TInterfaceMock.InternalCheck(aValid, aExpectationFailed: boolean;
  const aErrorMsgFmt: RawUtf8; const aErrorMsgArgs: array of const): boolean;
begin
  if fTestCase = nil then
    result := inherited InternalCheck(
      aValid, aExpectationFailed, aErrorMsgFmt, aErrorMsgArgs)
  else
  begin
    result := true; // do not raise any exception at this stage for TInterfaceMock
    if aValid xor (imoMockFailsWillPassTestCase in Options) then
      fTestCase.Check(true)
    else
      fTestCase.Check(false, Utf8ToString(FormatUtf8(aErrorMsgFmt, aErrorMsgArgs)));
  end;
end;


{ TInterfaceMockSpy }

constructor TInterfaceMockSpy.Create(aFactory: TInterfaceFactory;
  const aInterfaceName: RawUtf8);
begin
  inherited Create(aFactory, aInterfaceName);
  include(fOptions, imoLogMethodCallsAndResults);
end;

procedure TInterfaceMockSpy.IntSetOptions(Options: TInterfaceStubOptions);
begin
  include(Options, imoLogMethodCallsAndResults);
  inherited IntSetOptions(Options);
end;

procedure TInterfaceMockSpy.Verify(const aMethodName: RawUtf8;
  const aParams: array of const; aOperator: TInterfaceStubRuleOperator;
  aCount: cardinal);
begin
  Verify(aMethodName, JsonEncodeArray(aParams, true), aOperator, aCount);
end;

procedure TInterfaceMockSpy.Verify(const aMethodName: RawUtf8;
  const aParams: array of const; const aTrace: RawUtf8);
begin
  Verify(aMethodName, JsonEncodeArray(aParams, true), aTrace);
end;

procedure TInterfaceMockSpy.Verify(const aMethodName: RawUtf8;
  aOperator: TInterfaceStubRuleOperator; aCount: cardinal);
var
  m: integer;
begin
  m := fInterface.CheckMethodIndex(aMethodName);
  IntCheckCount(m, fRules[m].MethodPassCount, aOperator, aCount);
end;

procedure TInterfaceMockSpy.Verify(const aMethodName, aParams: RawUtf8;
  aOperator: TInterfaceStubRuleOperator; aCount: cardinal);
var
  asmndx, i: PtrInt;
  c: cardinal;
begin
  asmndx := fInterface.CheckMethodIndex(aMethodName) + RESERVED_VTABLE_SLOTS;
  if aParams = '' then
    c := fRules[asmndx - RESERVED_VTABLE_SLOTS].MethodPassCount
  else
  begin
    c := 0;
    for i := 0 to fLogCount - 1 do
      with fLogs[i] do
        if (method.ExecutionMethodIndex = asmndx) and
           (Params = aParams) then
          inc(c);
  end;
  IntCheckCount(asmndx - RESERVED_VTABLE_SLOTS, c, aOperator, aCount);
end;

procedure TInterfaceMockSpy.Verify(const aTrace: RawUtf8; aScope: TInterfaceMockSpyCheck);
const
  VERIFY_SCOPE: array[TInterfaceMockSpyCheck] of TInterfaceStubLogLayouts = (
    [wName], [wName, wParams], [wName, wParams, wResults]);
begin
  InternalCheck(IntGetLogAsText(0, '', VERIFY_SCOPE[aScope], ',') = aTrace,
    true, 'Verify(''%'',%) failed', [aTrace, ToText(aScope)^]);
end;

procedure TInterfaceMockSpy.Verify(const aMethodName, aParams, aTrace: RawUtf8);
var
  m: integer;
begin
  m := fInterface.CheckMethodIndex(aMethodName);
  InternalCheck(
    IntGetLogAsText(m + RESERVED_VTABLE_SLOTS, aParams, [wResults], ',') = aTrace,
    true, 'Verify(''%'',''%'',''%'') failed', [aMethodName, aParams, aTrace]);
end;

procedure TInterfaceMockSpy.Verify(const aMethodName, aTrace: RawUtf8;
  aScope: TInterfaceMockSpyCheck);
const
  VERIFY_SCOPE: array[TInterfaceMockSpyCheck] of TInterfaceStubLogLayouts = (
    [], [wParams], [wParams, wResults]);
var
  m: integer;
begin
  m := fInterface.CheckMethodIndex(aMethodName);
  if aScope = chkName then
    raise EInterfaceStub.Create(self, fInterface.Methods[m],
      'Invalid scope for Verify()');
  InternalCheck(
    IntGetLogAsText(m + RESERVED_VTABLE_SLOTS, '', VERIFY_SCOPE[aScope], ',') = aTrace,
    true, 'Verify(''%'',''%'',%) failed', [aMethodName, aTrace, ToText(aScope)^]);
end;


function ToText(c: TInterfaceMockSpyCheck): PShortString;
begin
  result := GetEnumName(TypeInfo(TInterfaceMockSpyCheck), ord(c));
end;

end.

