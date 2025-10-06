/// Framework Core Logging
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.log;

{
  *****************************************************************************

   Logging functions shared by all framework units
    - Debug Symbols Processing from Delphi .map or FPC/GDB DWARF
    - Logging via TSynLogFamily, TSynLog, ISynLog
    - High-Level Logs and Exception Related Features
    - Efficient .log File Access via TSynLogFile
    - SysLog Messages Support as defined by RFC 5424

  *****************************************************************************

}


interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.buffers,
  mormot.core.data,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime;


{ ************** Debug Symbols Processing from Delphi .map or FPC/GDB DWARF }

type
  /// a debugger symbol, as decoded by TDebugFile from a .map/.dbg file
  TDebugSymbol = packed record
    /// symbol internal name
    Name: RawUtf8;
    /// starting offset of this symbol in the executable
    Start: integer;
    /// end offset of this symbol in the executable
    Stop: integer;
  end;

  PDebugSymbol = ^TDebugSymbol;

  /// a dynamic array of symbols, as decoded by TDebugFile from a .map/.dbg file
  TDebugSymbolDynArray = array of TDebugSymbol;

  /// a debugger unit, as decoded by TDebugFile from a .map/.dbg file
  TDebugUnit = packed record
    /// Name, Start and Stop of this Unit
    Symbol: TDebugSymbol;
    /// associated source file name
    FileName: RawUtf8;
    /// list of all mapped source code lines of this unit
    Line: TIntegerDynArray;
    /// start code address of each source code line
    Addr: TIntegerDynArray;
  end;

  PDebugUnit = ^TDebugUnit;

  /// a dynamic array of units, as decoded by TDebugFile from a .map/.dbg file
  TDebugUnitDynArray = array of TDebugUnit;

  /// process a .map/.dbg file content, to be used e.g. with TSynLog to provide
  // additional debugging information for a given executable
  // - debug info can be saved as .mab file in a much more optimized format
  // (e.g. mormot2tests 4MB .map into a 280KB .mab, 13MB .dbg into a 290KB .mab)
  // - on FPC, DWARF symbols embedded to the executable can also be retrieved - but
  // you would better use an external .dbg file then convert it into a .mab
  // - on FPC, you don't need to specifly the -gl compiler switch
  // - location of a source code information from its address is below 10us
  TDebugFile = class(TSynPersistent)
  protected
    fDebugFile: TFileName;
    fSymbol: TDebugSymbolDynArray;
    fUnit: TDebugUnitDynArray;
    fSymbols, fUnits: TDynArray;
    fSymbolsCount, fUnitsCount: integer;
    fCodeOffset: PtrUInt;
    fHasDebugInfo: boolean;
    // called by Create() constructor
    procedure GenerateFromMapOrDbg(aDebugToConsole: boolean);
    function LoadMab(const aMabFile: TFileName): boolean;
  public
    /// get the available debugging information
    // - if aExeName is specified, will use it in its search for .map/.dbg/.mab
    // - if aExeName is not specified, will use the currently running .exe/.dll
    // - it will first search for a .map/.dbg matching the file name: if found,
    // will be read to retrieve all necessary debugging information - a .mab
    // file will be also created in the same directory (if MabCreate is TRUE)
    // - if .map/.dbg is not not available, will search for the .mab file
    // - if no .mab is available, will search for a .mab appended to the .exe/.dll
    // - if nothing is available, will log as hexadecimal pointers, without
    // debugging information
    constructor Create(const aExeName: TFileName = ''; MabCreate: boolean = true;
      DebugToConsole: boolean = false); reintroduce;
    /// save all debugging information in the .mab custom binary format
    // - if no file name is specified, it will be saved as ExeName.mab or DllName.mab
    // - this file content can be appended to the executable via SaveToExe method
    // - this function returns the created file name
    function SaveToFile(const aFileName: TFileName = ''): TFileName;
    /// save all debugging informat in our custom binary format
    procedure SaveToStream(aStream: TStream);
    /// append all debugging information to an executable (or library)
    // - the executable name must be specified, because it's impossible to
    // write to the executable of a running process
    // - this method will work for .exe and for .dll (or .ocx)
    procedure SaveToExe(const aExeName: TFileName);
    /// save all debugging information as JSON content
    // - may be useful from debugging purposes
    procedure SaveToJson(W: TTextWriter); overload;
    /// save all debugging information as a JSON file
    // - may be useful from debugging purposes
    procedure SaveToJson(const aJsonFile: TFileName;
      aJsonFormat: TTextWriterJsonFormat = jsonCompact); overload;
    /// add some debugging information about the supplied absolute memory address
    // - create a global TDebugFile instance for the current process, if needed
    // - if no debugging information is available (.map/.dbg/.mab), will write
    // the raw address pointer as hexadecimal
    class function Log(W: TTextWriter; aAddressAbsolute: PtrUInt;
      AllowNotCodeAddr: boolean; SymbolNameNotFilename: boolean = false): boolean;
    /// compute the relative memory address from its absolute (pointer) value
    function AbsoluteToOffset(aAddressAbsolute: PtrUInt): integer;
      {$ifdef HASINLINE}inline;{$endif}
    /// check if this memory address is part of the code segments
    function IsCode(aAddressAbsolute: PtrUInt): boolean;
    /// retrieve a symbol according to a relative code address
    // - use fast O(log n) binary search
    function FindSymbol(aAddressOffset: integer): PtrInt;
    /// retrieve an unit and source line, according to a relative code address
    // - use fast O(log n) binary search
    function FindUnit(aAddressOffset: integer; out LineNumber: integer): PtrInt; overload;
    /// retrieve an unit, according to a relative code address
    // - use fast O(log n) binary search
    function FindUnit(aAddressOffset: integer): PtrInt; overload;
    /// retrieve an unit information, according to the unit name
    // - will search within Units array
    function FindUnit(const aUnitName: RawUtf8): PtrInt; overload;
    /// return the symbol location according to the supplied absolute address
    // - filename, symbol name and line number (if any), as plain text, e.g.
    // '4cb765 ../src/core/mormot.core.base.pas statuscodeissuccess (11183)' on FPC
    // - returns only the hexadecimal value if no match is found in .map/.gdb info
    function FindLocation(aAddressAbsolute: PtrUInt): RawUtf8; overload;
    /// return the symbol location according to the supplied absolute address
    // - filename, symbol name and line number (if any), as plain text, e.g.
    // '4cb765 ../src/core/mormot.core.base.pas statuscodeissuccess (11183)' on FPC
    // - returns only the hexadecimal value if no match is found in .map/.gdb info
    // - won't allocate any heap memory during the text creation
    // - mormot.core.os.pas' GetExecutableLocation() redirects to this method
    function FindLocationShort(aAddressAbsolute: PtrUInt): ShortString;
    /// load .map/.gdb info and return the symbol location according
    // to the supplied ESynException
    // - i.e. unit name, symbol name and line number (if any), as plain text
    class function FindLocation(exc: ESynException): RawUtf8; overload;
    /// load .map/.gdb info and returns the file name of a given unit
    // - if unitname = '', returns the main file name of the current executable
    class function FindFileName(const unitname: RawUtf8): TFileName;
    {$ifdef FPC}
    /// load DWARF .gdb info and replace FPC RTL BacktraceStrFunc()
    // - uses much less disk space (e.g. 13MB .gdb into 284KB)
    // - is much faster: around 10us per call, whereas lnfodwrf is 20ms
    class function RegisterBacktraceStrFunc: boolean;
    {$endif FPC}
    /// all symbols, mainly function and method names and addresses
    property Symbols: TDebugSymbolDynArray
      read fSymbol;
    /// all units, including line numbers, associated to the executable
    property Units: TDebugUnitDynArray
      read fUnit;
  published
    /// the associated file name
    // - e.g. 'exec.map', 'exec.dbg' or even plain 'exec'/'exec.exe'
    property FileName: TFileName
      read fDebugFile;
    /// equals true if a .map/.dbg or .mab debugging information has been loaded
    property HasDebugInfo: boolean
      read fHasDebugInfo;
  end;

{$ifndef PUREMORMOT2}
  // backward compatibility type redirection
  TSynMapFile = TDebugFile;
{$endif PUREMORMOT2}


{ ************** Logging via TSynLogFamily, TSynLog, ISynLog }

type
  /// a list of lof events families, used to gather events by type
  TSynLogFilter = (
    lfNone,
    lfAll,
    lfErrors,
    lfExceptions,
    lfProfile,
    lfDatabase,
    lfClientServer,
    lfDebug,
    lfCustom,
    lfDDD);

  /// global increasing log levels as expected by most applications
  TAppLogLevel = (
    aplNone,
    aplCritical,
    aplError,
    aplWarning,
    aplInfo,
    aplDebug);

const
  /// up to 7 TSynLogFamily, i.e. TSynLog sub-classes can be defined at once
  MAX_SYNLOGFAMILY = 7;
  /// we store up to 53 recursion levels of Enter/Leave information
  // - above this limit, no error would be raised at runtime, but no associated
  // information would be stored - therefore logged (TSynLog.Enter returns nil)
  // - typical value of recursive calls number is below a dozen: indentation in
  // the log file would make any bigger value clearly unreadable
  // - this number has been also defined to keep TSynLogThreadInfo < 512 bytes
  MAX_SYNLOGRECURSION = 53;
  /// we handle up to 64K threads per TSynLog instance
  // - there is no technical reason to such limitation, but it would allow to
  // detect missing TSynLog.NotifyThreadEnded calls in your code logic
  MAX_SYNLOGTHREADS = 65500;

  /// constant with all TSynLogFamily.Level items, as set by LOG_VERBOSE
  LOG_ALL = [succ(sllNone) .. high(TSynLogLevel)];
  /// constant matching TSynLogFamily.Level items for regular aplCritical level
  LOG_CRI = [sllException, sllExceptionOS];
  /// constant matching TSynLogFamily.Level items for regular aplError level
  LOG_ERR = LOG_CRI + [sllLastError, sllError, sllDDDError];
  /// constant matching TSynLogFamily.Level items for regular aplWarning level
  LOG_WNG = LOG_ERR + [sllWarning, sllFail, sllStackTrace];
  /// constant matching TSynLogFamily.Level items for regular aplInfo level
  LOG_NFO = LOG_WNG + [sllInfo, sllDDDInfo, sllMonitoring, sllClient, sllServer, sllServiceCall];

  /// can be set to TSynLogFamily.Level in order to log all available events
  LOG_VERBOSE: TSynLogLevels = LOG_ALL;

  /// contains the logging levels for which stack trace should be dumped
  // - which are mainly exceptions or application errors
  LOG_STACKTRACE: TSynLogLevels = LOG_ERR;

  /// the text equivalency of each logging level, as written in the log file
  // - PCardinal(@LOG_LEVEL_TEXT[L][3])^ will be used for fast level matching
  // so text must be unique for characters [3..6] -> e.g. 'ust4'
  LOG_LEVEL_TEXT: array[TSynLogLevel] of string[7] = (
    '       ',  // sllNone
    ' info  ',  // sllInfo
    ' debug ',  // sllDebug
    ' trace ',  // sllTrace
    ' warn  ',  // sllWarning
    ' ERROR ',  // sllError
    '  +    ',  // sllEnter
    '  -    ',  // sllLeave
    ' OSERR ',  // sllLastError
    ' EXC   ',  // sllException
    ' EXCOS ',  // sllExceptionOS
    ' mem   ',  // sllMemory
    ' stack ',  // sllStackTrace
    ' fail  ',  // sllFail
    ' SQL   ',  // sllSQL
    ' cache ',  // sllCache
    ' res   ',  // sllResult
    ' DB    ',  // sllDB
    ' http  ',  // sllHTTP
    ' clnt  ',  // sllClient
    ' srvr  ',  // sllServer
    ' call  ',  // sllServiceCall
    ' ret   ',  // sllServiceReturn
    ' auth  ',  // sllUserAuth
    ' cust1 ',  // sllCustom1
    ' cust2 ',  // sllCustom2
    ' cust3 ',  // sllCustom3
    ' cust4 ',  // sllCustom4
    ' rotat ',  // sllNewRun
    ' dddER ',  // sllDDDError
    ' dddIN ',  // sllDDDInfo
    ' mon   '); // sllMonitoring

var
  /// RGB colors corresponding to each logging level
  // - matches the TColor values, as used by the VCL/LCL
  // - first array is for the background, second is for the text (black/white)
  // - is defined as var and not const to allow customization at runtime
  LOG_LEVEL_COLORS: array[boolean, TSynLogLevel] of integer = (
   ($ffffff,    // sllNone
    $dcc0c0,    // sllInfo
    $dcdcdc,    // sllDebug
    $c0c0c0,    // sllTrace
    $8080c0,    // sllWarning
    $8080ff,    // sllError
    $c0dcc0,    // sllEnter
    $dcdcc0,    // sllLeave
    $c0c0f0,    // sllLastError
    $c080ff,    // sllException
    $c080f0,    // sllExceptionOS
    $c080c0,    // sllMemory
    $c080c0,    // sllStackTrace
    $4040ff,    // sllFail
    $b08080,    // sllSQL
    $b0b080,    // sllCache
    $8080dc,    // sllResult
    $80dc80,    // sllDB
    $dc8080,    // sllHTTP
    $dcff00,    // sllClient
    $dcd000,    // sllServer
    $dcdc80,    // sllServiceCall
    $dc80dc,    // sllServiceReturn
    $dcdcdc,    // sllUserAuth
    $d0d0d0,    // sllCustom1
    $d0d0dc,    // sllCustom2
    $d0d0c0,    // sllCustom3
    $d0d0e0,    // sllCustom4
    $20e0d0,    // sllNewRun
    $8080ff,    // sllDDDError
    $dccdcd,    // sllDDDInfo
    $c0c0c0),   // sllMonitoring
    // black/white text corresponding to each colored background:
   ($000000,    // sllNone
    $000000,    // sllInfo
    $000000,    // sllDebug
    $000000,    // sllTrace
    $000000,    // sllWarning
    $ffffff,    // sllError
    $000000,    // sllEnter
    $000000,    // sllLeave
    $ffffff,    // sllLastError
    $ffffff,    // sllException
    $ffffff,    // sllExceptionOS
    $000000,    // sllMemory
    $000000,    // sllStackTrace
    $ffffff,    // sllFail
    $ffffff,    // sllSQL
    $000000,    // sllCache
    $ffffff,    // sllResult
    $000000,    // sllDB
    $000000,    // sllHTTP
    $000000,    // sllClient
    $000000,    // sllServer
    $000000,    // sllServiceCall
    $000000,    // sllServiceReturn
    $000000,    // sllUserAuth
    $000000,    // sllCustom1
    $000000,    // sllCustom2
    $000000,    // sllCustom3
    $000000,    // sllCustom4
    $000000,    // sllNewRun
    $ffffff,    // sllDDDError
    $000000,    // sllDDDInfo
    $000000));  // sllMonitoring

  /// console colors corresponding to each logging level
  // - to be used with mormot.core.os TextColor()
  // - is defined as var and not const to allow customization at runtime
  LOG_CONSOLE_COLORS: array[TSynLogLevel] of TConsoleColor = (
    ccLightGray,    // sllNone
    ccWhite,        // sllInfo
    ccLightGray,    // sllDebug
    ccLightBlue,    // sllTrace
    ccBrown,        // sllWarning
    ccLightRed,     // sllError
    ccGreen,        // sllEnter
    ccGreen,        // sllLeave
    ccLightRed,     // sllLastError
    ccLightRed,     // sllException
    ccLightRed,     // sllExceptionOS
    ccLightGray,    // sllMemory
    ccCyan,         // sllStackTrace
    ccLightRed,     // sllFail
    ccBrown,        // sllSQL
    ccBlue,         // sllCache
    ccLightCyan,    // sllResult
    ccMagenta,      // sllDB
    ccCyan,         // sllHTTP
    ccLightCyan,    // sllClient
    ccLightCyan,    // sllServer
    ccLightMagenta, // sllServiceCall
    ccLightMagenta, // sllServiceReturn
    ccMagenta,      // sllUserAuth
    ccLightGray,    // sllCustom1
    ccLightGray,    // sllCustom2
    ccLightGray,    // sllCustom3
    ccLightGray,    // sllCustom4
    ccLightMagenta, // sllNewRun
    ccLightRed,     // sllDDDError
    ccWhite,        // sllDDDInfo
    ccLightBlue);   // sllMonitoring

  /// how TLogFilter map TSynLogLevel events
  LOG_FILTER: array[TSynLogFilter] of TSynLogLevels = (
    [],                                                       // lfNone
    LOG_ALL,                                                  // lfAll
    [sllError, sllLastError, sllException, sllExceptionOS],   // lfErrors
    [sllException, sllExceptionOS],                           // lfExceptions
    [sllEnter, sllLeave],                                     // lfProfile
    [sllSQL, sllCache, sllDB],                                // lfDatabase
    [sllClient, sllServer, sllServiceCall, sllServiceReturn], // lfClientServer
    [sllDebug, sllTrace, sllEnter],                           // lfDebug
    [sllCustom1 .. sllCustom4],                               // lfCustom
    [sllDDDError, sllDDDInfo]);                               // lfDDD

  /// may be used to log as Debug or Error event, depending on an Error: boolean
  LOG_DEBUGERROR: array[boolean] of TSynLogLevel = (
    sllDebug,
    sllError);

  /// may be used to log as Trace or Error event, depending on an Error: boolean
  LOG_TRACEERROR: array[boolean] of TSynLogLevel = (
    sllTrace,
    sllError);

  /// may be used to log as Info or Warning event, depending on an Error: boolean
  LOG_INFOWARNING: array[boolean] of TSynLogLevel = (
    sllInfo,
    sllWarning);

  /// may be used to log as regular application-like levels
  LOG_APP: array[TAppLogLevel] of TSynLogLevels = (
    [],        // aplNone
    LOG_CRI,   // aplCritical (1)
    LOG_ERR,   // aplError    (2)
    LOG_WNG,   // aplWarning  (3)
    LOG_NFO,   // aplInfo     (4)
    LOG_ALL);  // aplDebug    (5)


/// returns the trimmed text value of a logging level
// - i.e. 'Warning' for sllWarning
function ToText(event: TSynLogLevel): RawUtf8; overload;

/// returns the trimmed text value of a logging levels set
function ToText(events: TSynLogLevels): ShortString; overload;

/// returns the ready-to-be displayed text of a TSynLogLevel value
function ToCaption(event: TSynLogLevel): string; overload;

/// returns the ready-to-be displayed text of a TSynLogFilter value
function ToCaption(filter: TSynLogFilter): string; overload;

/// returns a method event as text, using the .map/.dbg/.mab information if available
function ToText(const Event: TMethod): RawUtf8; overload;

/// returns the trimmed text value of an application-like logging level
// - i.e. 'Critical' for aplCritical
function ToText(apl: TAppLogLevel): RawUtf8; overload;

/// recognize TAppLogLevel common text like 'WARNING'
// - ignoring case and only checking the first 4 chars
// - would also recognize '1' .. '5' numbers as increasing aplCritical .. aplDebug
function ToAppLogLevel(const Text: RawUtf8): TAppLogLevel;

/// could be used to set TSynLogFamily.Levels e.g. from 'DEBUG' or 'CRITICAL' text
function FromAppLogLevel(const Text: RawUtf8): TSynLogLevels;

/// retrieve a one-line of text including detailed heap information
// - will use the RTL status entrypoint, or detect mormot.core.fpcx64mm
// and retrieve all its available information
// - as used by TSynLog.AddMemoryStats
function RetrieveMemoryManagerInfo: RawUtf8;

var
  /// low-level critical section used internally by this unit
  // - we use a process-wide giant lock to avoid proper multi-threading of logs
  // - most process (e.g. time retrieval) is done outside of the lock: only
  // actual log file writing is blocking the threads - slowest process like file
  // rotation/archival or console output will be executed in a background thread
  // - do not access this variable in your code: defined here for proper inlining
  GlobalThreadLock: TOSLock;

  /// is set to TRUE before ObjArrayClear(SynLogFile) in unit finalization
  // - defined here to avoid unexpected GPF at shutdown
  SynLogFileFreeing: boolean;

type
  /// class of Exceptions raised by this unit
  ESynLogException = class(ESynException);

  /// an exception which wouldn't be logged and intercepted by this unit
  // - only this exact class will be recognized by TSynLog: inheriting it
  // will trigger the interception, as any other regular exception
  // - you may consider also TSynLog.Family.ExceptionIgnore.Add()
  ESynLogSilent = class(ESynException);

  {$M+}
  TSynLog = class;

  /// class-reference type (metaclass) of a TSynLog family
  // - since TSynLog classes store their information per type, you usually
  // will store a reference to a logging family (i.e. logging settings) using
  // a TSynLogClass variable, whereas TSynLog would point to the active logging
  // instance
  TSynLogClass = class of TSynLog;

  TSynLogFamily = class;
  {$M-}

  /// a generic interface used for logging a method
  // - you should create one TSynLog instance at the beginning of a block code
  // using TSynLog.Enter: the ISynLog will be released automaticaly by the
  // compiler at the end of the method block, marking it's executation end
  // - all logging expect UTF-8 encoded text, i.e. usualy English text
  // - warning: NEVER use this ISynLog with TSynLog.Add or direclty from
  // a TSynLog instance: this interface requires the TSynLog.Enter class method:
  // ! var logger: ISynLog;
  // ! begin
  // !   logger := TSynLog.Enter(self,'MyMethod');
  // !   // do some stuff
  // !   if Assigned(logger) then // may be nil if sllEnter is not enabled
  // !     logger.Log(sllInfo,'method called');
  // ! end; // when logger is out-of-scope, will log the method leaving
  ISynLog = interface(IUnknown)
    ['{527AC81F-BC41-4717-B089-3F74DE56F1AE}']
    /// call this method to add some information to the log at a specified level
    // - will use TTextWriter.Add(...,twOnSameLine) to append its content
    // - % = #37 indicates a string, integer, floating-point, class parameter
    // to be appended as text (e.g. class name), any variant as JSON...
    // - note that cardinal values should be type-casted to Int64() (otherwise
    // the integer mapped value will be transmitted, therefore wrongly)
    // - if Instance is set, it will log the corresponding class name and address
    // (to be used if you didn't call TSynLog.Enter() method first)
    procedure Log(Level: TSynLogLevel; TextFmt: PUtf8Char;
      const TextArgs: array of const; Instance: TObject = nil); overload;
    /// call this method to add some information to the log at a specified level
    // - if Instance is set and Text is not '', it will log the corresponding
    // class name and address (to be used e.g. if you didn't call TSynLog.Enter()
    // method first)
    // - if Instance is set and Text is '', will behave the same as
    // Log(Level,Instance), i.e. write the Instance as JSON content
    procedure Log(Level: TSynLogLevel; const Text: RawUtf8;
      Instance: TObject = nil; TextTruncateAtLength: PtrInt = 0); overload;
    {$ifdef UNICODE}
    /// call this method to add some RTL string to the log at a specified level
    // - this overloaded version will avoid a call to StringToUtf8()
    procedure Log(Level: TSynLogLevel; const Text: string;
      Instance: TObject = nil); overload;
    {$endif UNICODE}
    /// call this method to add the content of an object to the log at a
    // specified level
    // - TSynLog will write the class and hexa address - TSqlLog will write the
    // object JSON content
    procedure Log(Level: TSynLogLevel; Instance: TObject); overload;
    /// call this method to add the content of most low-level types to the log
    // at a specified level
    // - TSynLog will handle enumerations and dynamic array; TSqlLog will be
    // able to write TObject/TOrm and sets content as JSON
    procedure Log(Level: TSynLogLevel; const aName: RawUtf8; aTypeInfo: PRttiInfo;
      const aValue; Instance: TObject); overload;
    /// call this method to add the caller address to the log at the specified level
    // - if the debugging info is available from TDebugFile, will log the
    // unit name, associated symbol and source code line
    procedure Log(Level: TSynLogLevel = sllTrace); overload;
    /// call this method to add the content of a PUtf8Char buffer
    // - is slightly more optimized than Log(RawUtf8) or LogText(Text,TextLen)
    procedure LogText(Level: TSynLogLevel; Text: PUtf8Char; Instance: TObject); overload;
    /// call this method to add the content of a PUtf8Char buffer and length
    procedure LogText(Level: TSynLogLevel; Text: PUtf8Char; TextLen: PtrInt;
      Instance: TObject; TextTruncateAtLength: PtrInt = 0); overload;
    /// call this method to add some multi-line information to the log at a
    // specified level
    // - LinesToLog content will be added, one line per one line, delimited
    // by #13#10 (CRLF)
    // - if a line starts with IgnoreWhenStartWith (already uppercase), it won't
    // be added to the log content (to be used e.g. with '--' for SQL statements)
    procedure LogLines(Level: TSynLogLevel; LinesToLog: PUtf8Char;
      aInstance: TObject = nil; const IgnoreWhenStartWith: PAnsiChar = nil);
    /// retrieve the associated logging instance
    // - warning: NEVER assign the returned instance to a ISynLog variable - use
    // the existing ISynLog, or call TSynLog.Enter/EnterLocal instead
    function Instance: TSynLog;
  end;

  /// this event can be set for a TSynLogFamily to archive any deprecated log
  // into a custom compressed format, i.e. compress and delete them
  // - called by TSynLogFamily.Destroy with files older than ArchiveAfterDays,
  // or by TSynLog.PerformRotation when some rotated files need to be deleted
  // - the aOldLogFileName will contain the .log file with full path
  // - the aDestinationPath parameter will contain 'ArchivePath\log\YYYYMM\'
  // - should return true on success, false on error
  // - example of matching event handler are EventArchiveDelete,
  // EventArchiveSynLZ, EventArchiveLizard or EventArchiveZip
  // - this event handler will be called one time per .log file to archive,
  // then one last time with aOldLogFileName='' in order to close any pending
  // archive (used e.g. by EventArchiveZip to open the .zip only once)
  TSynLogArchiveEvent = function(const aOldLogFileName,
    aDestinationPath: TFileName): boolean;

  /// this event can be set for a TSynLogFamily to customize the file rotation
  // - will be called by TSynLog.PerformRotation/ForceRotation
  // - should return TRUE if the function did process the file name
  // - should return FALSE if the function did not do anything, so that the
  // caller should perform the rotation as usual
  TSynLogRotateEvent = function(aLog: TSynLog; const aOldLogFileName: TFileName): boolean;

  /// how threading is handled by the TSynLogFamily
  // - proper threading expects the TSynLog.NotifyThreadEnded method to be called
  // when a thread is about to terminate, e.g. from TRest.EndCurrentThread
  // - by default, ptMergedInOneFile will indicate that all threads are logged
  // in the same file, in occurrence order
  // - if set to ptOneFilePerThread, it will create one .log file per thread
  // - if set to ptIdentifiedInOneFile, a new column will be added for each
  // log row, with the corresponding ThreadID - LogView tool will be able to
  // display per-thread logging, if needed - note that your application shall
  // always better use a thread pool (just like all mORMot servers classes do)
  // - if set to ptNoThreadProcess, no thread information is gathered, and all
  // Enter/Leave would be ignored - but it may be mandatory to use this option
  // if TSynLog.NotifyThreadEnded is not properly called (e.g. from legacy code)
  // and that your process has thread-related instability issues
  TSynLogPerThreadMode = (
    ptMergedInOneFile,
    ptOneFilePerThread,
    ptIdentifiedInOneFile,
    ptNoThreadProcess);

  /// how stack trace shall be computed during logging
  // - stOnlyAPI is the first (and default) value, since manual stack makes
  // unexpected detections, and was reported as very slow on Windows 11
  // - on FPC, these values are ignored, because RTL CaptureBacktrace() is used 
  TSynLogStackTraceUse = (
    stOnlyAPI,
    stManualAndAPI,
    stOnlyManual);

  /// how file existing shall be handled during logging
  TSynLogExistsAction = (
    acOverwrite,
    acAppend);

  {$ifndef NOEXCEPTIONINTERCEPT}
  /// callback signature used by TSynLogFamilly.OnBeforeException
  // - should return false to log the exception, or true to ignore it
  TOnBeforeException = function(const Context: TSynLogExceptionContext;
    const ThreadName: shortstring): boolean of object;
  {$endif NOEXCEPTIONINTERCEPT}

  /// available TSynLogThreadInfo.Flags definition
  // - tiExceptionIgnore store TSynLogFamily.ExceptionIgnoreCurrentThread
  // property (used only if NOEXCEPTIONINTERCEPT conditional is undefined)
  // - tiTemporaryDisable store TSynLogFamily.DisableCurrentThread property
  TSynLogThreadInfoFlag = (
    tiExceptionIgnore,
    tiTemporaryDisable);
  /// TSynLogThreadInfo.Flags property set type definition
  TSynLogThreadInfoFlags = set of TSynLogThreadInfoFlag;

  /// regroup several logs under an unique family name
  // - you should usualy use one family per application or per architectural
  // module: e.g. a server application may want to log in separate files the
  // low-level Communication, the DB access, and the high-level process
  // - initialize the family settings before using them, like in this code:
  // ! with TSynLogDB.Family do
  // ! begin
  // !   PerThreadLog := ptOneFilePerThread;
  // !   DestinationPath := 'C:\Logs';
  // !   Level := LOG_VERBOSE; // should better be set last
  // ! end;
  //- then use the logging system fropm this class, e.g. inside a method:
  // ! procedure TMyDB.MyMethod;
  // ! var logger: ISynLog;
  // ! begin
  // !   logger := TSynLogDB.Enter(self,'MyMethod');
  // !   // do some stuff
  // !   if Assigned(logger) then // may be nil if sllEnter is not enabled
  // !     logger.Log(sllInfo,'method called');
  // ! end; // when logger is out-of-scope, will log the method leaving
  TSynLogFamily = class
  protected
    fLevel, fLevelStackTrace, fLevelSysInfo: TSynLogLevels; // 3 * 32-bit
    fHighResolutionTimestamp: boolean;
    fLocalTimestamp: boolean;
    fZonedTimestamp: boolean;
    fIdent: byte;
    fGlobalLog: TSynLog;
    fSynLogClass: TSynLogClass;
    fPerThreadLog: TSynLogPerThreadMode;
    fWithUnitName: boolean;
    fWithInstancePointer: boolean;
    fStackTraceLevel: byte;
    fStackTraceUse: TSynLogStackTraceUse;
    fFileExistsAction: TSynLogExistsAction;
    {$ifdef OSWINDOWS}
    fNoEnvironmentVariable: boolean;
    {$endif OSWINDOWS}
    {$ifndef NOEXCEPTIONINTERCEPT}
    fHandleExceptions: boolean;
    fOnBeforeException: TOnBeforeException;
    {$endif NOEXCEPTIONINTERCEPT}
    fAutoFlushTimeOut: cardinal;
    fArchiveAfterDays: integer;
    fArchivePath: TFileName;
    fOnArchive: TSynLogArchiveEvent;
    fOnRotate: TSynLogRotateEvent;
    fCustomFileName: TFileName;
    fDestinationPath: TFileName;
    fDefaultExtension: TFileName;
    fExceptionIgnore: TSynList;
    fEchoToConsole: TSynLogLevels;
    fEchoCustom: TOnTextWriterEcho;
    fEchoRemoteClient: TObject;
    fEchoRemoteEvent: TOnTextWriterEcho;
    fEchoRemoteClientOwned: boolean;
    fEchoToConsoleUseJournal: boolean;
    fEchoToConsoleBackground: boolean;
    fEndOfLineCRLF: boolean;
    fIncludeComputerNameInFileName: boolean;
    fIncludeUserNameInFileName: boolean;
    fNoFile: boolean;
    fDestroying: boolean;
    fBufferSize: integer;
    fRotateFileCurrent: cardinal;
    fRotateFileCount: cardinal;
    fRotateFileSizeKB: cardinal;
    fRotateFileDailyAtHour: integer;
    function GetLog: TSynLog; // from inlined Add (calls CreateSynLog if needed)
    function CreateSynLog: TSynLog;
    procedure EnsureAutoFlushThreadRunning;
    procedure SetDestinationPath(const value: TFileName);
    procedure SetLevel(aLevel: TSynLogLevels);
    procedure SynLogFileListEcho(const aEvent: TOnTextWriterEcho; aEventAdd: boolean);
    procedure SetEchoToConsole(aEnabled: TSynLogLevels);
    procedure SetEchoToConsoleUseJournal(aValue: boolean);
    procedure SetEchoCustom(const aEvent: TOnTextWriterEcho);
    function GetSynLogClassName: string;
    function ArchiveAndDeleteFile(const aFileName: TFileName): boolean;
    function GetArchiveDestPath(age: TDateTime): TFileName;
    function GetCurrentThreadFlag(ti: TSynLogThreadInfoFlag): boolean;
    procedure SetCurrentThreadFlag(ti: TSynLogThreadInfoFlag; value: boolean);
  public
    /// intialize for a TSynLog class family
    // - add it in the global SynLogFileFamily[] list
    constructor Create(aSynLog: TSynLogClass);
    /// close any console echo, and release associated memory
    destructor Destroy; override;

    /// retrieve the corresponding log file of this thread and family
    // - calls GetLog if needed (e.g. at startup or if fGlobalLog is not set)
    // - warning: NEVER assign the returned instance to a ISynLog variable - use
    // TSynLog.Enter or TSynLog.EnterLocal if you want to have a ISynLog
    function Add: TSynLog;
      {$ifdef HASINLINE} inline; {$endif}
    /// register one object and one echo callback for remote logging
    // - aClient is typically a mORMot's TRestHttpClient or a TSynLogCallbacks
    // instance as defined in this unit
    // - if aClientOwnedByFamily is TRUE, its life time will be manage by this
    // TSynLogFamily: it will stay alive until this TSynLogFamily is destroyed,
    // or the EchoRemoteStop() method called
    // - aClientEvent should be able to send the log row to the remote server
    procedure EchoRemoteStart(aClient: TObject;
      const aClientEvent: TOnTextWriterEcho; aClientOwnedByFamily: boolean);
    /// stop echo remote logging
    // - will free the aClient instance supplied to EchoRemoteStart
    procedure EchoRemoteStop;
    /// can be used to retrieve up to a specified amount of KB of existing log
    // - expects a single file to be opened for this family
    // - will retrieve the log content for the current file, truncating the
    // text up to the specified number of KB (an up to 128 MB at most)
    function GetExistingLog(MaximumKB: cardinal): RawUtf8;
    /// callback to notify the current logger that its thread is finished
    // - method follows TOnNotifyThread signature, which can be assigned to
    // TSynBackgroundThreadAbstract.OnAfterExecute
    // - is called e.g. by TRest.EndCurrentThread
    // - just a wrapper around TSynLog.NotifyThreadEnded
    procedure OnThreadEnded(Sender: TThread);
    /// clean up *.log file by running OnArchive() on deprecated files
    // - will find and archive DestinationPath\*.log (or sourcePath\*.log)
    // files older than ArchiveAfterDays (or archiveDays), into the ArchivePath
    // (or destPath) folder
    // - was previously done in Destroy, but it makes better sense to run it
    // only when needed (least astonishment principle), and with customization
    procedure ArchiveOldFiles(sourcePath: TFileName = '';
      destPath: TFileName = ''; archiveDays: integer = -1);

    /// you can add some exceptions to be ignored to this list
    // - for instance, EConvertError may be added to the list, as such:
    // ! TSqlLog.Family.ExceptionIgnore.Add(EConvertError);
    // - you may also trigger ESynLogSilent exceptions for silent process
    // - see also ExceptionIgnoreCurrentThread property, if you want a per-thread
    // filtering of all exceptions
    property ExceptionIgnore: TSynList
      read fExceptionIgnore;
    {$ifndef NOEXCEPTIONINTERCEPT}
    /// allow to (temporarly) ignore exceptions in the current thread
    // - this property will affect all TSynLogFamily instances, for the
    // current thread
    // - may be used in a try...finally block e.g. when notifying the exception
    // to a third-party service, or during a particular process
    // - see also ExceptionIgnore property - which is also checked in addition
    // to this flag
    property ExceptionIgnoreCurrentThread: boolean
      index tiExceptionIgnore read GetCurrentThreadFlag write SetCurrentThreadFlag;
    /// allow to temporarly avoid logging in the current thread
    // - won't affect exceptions logging, as one would expect for safety reasons
    // - after setting true to this property, should eventually be reset to false:
    // ! TSynLog.Family.DisableCurrentThread := true;
    // ! try
    // !   ...
    // ! finally
    // !   TSynLog.Family.DisableCurrentThread := false;
    // ! end;
    property DisableCurrentThread: boolean
      index tiTemporaryDisable read GetCurrentThreadFlag write SetCurrentThreadFlag;
    /// you can let exceptions be ignored from a callback
    // - if set and returns true, the given exception won't be logged
    // - execution of this event handler is protected via the logs global lock
    // - may be handy e.g. when working with code triggerring a lot of
    // exceptions (e.g. Indy), where ExceptionIgnore could be refined
    property OnBeforeException: TOnBeforeException
      read fOnBeforeException write fOnBeforeException;
    {$endif NOEXCEPTIONINTERCEPT}
    /// event called to archive - i.e. compress and delete - .log files
    // - called by TSynLogFamily.Destroy with files older than ArchiveAfterDays,
    // or by TSynLog.PerformRotation when some rotated files need to be deleted
    // - set this property to EventArchiveDelete in order to delete deprecated
    // files, or EventArchiveSynLZ/EventArchiveLizard to archive the .log files
    // into our proprietary SynLZ/Lizard format: resulting file name will be
    // 'ArchivePath\log\YYYYMM\*.log.synlz/synliz' - use AlgoSynLZ.FileUnCompress
    // or AlgoLizard.FileUnCompress functions to uncompress them
    // - if you use EventArchiveZip from mormot.core.zip, the log files will be
    // archived in 'ArchivePath\log\YYYYMM.zip'
    // - this event handler will be called one time per .log file to archive,
    // then one last time with aOldLogFileName='' in order to close any pending
    // archive (used e.g. by EventArchiveZip to open the .zip only once)
    property OnArchive: TSynLogArchiveEvent
      read fOnArchive write fOnArchive;
    /// event called to perform a custom file rotation
    // - will be checked by TSynLog.PerformRotation to customize the rotation
    // process and do not perform the default step, if the callback returns TRUE
    property OnRotate: TSynLogRotateEvent
      read fOnRotate write fOnRotate;
    /// if the some kind of events shall be echoed to the console
    // - note that it will slow down the logging process a lot (console output
    // is slow by nature under Windows, but may be convenient for interactive
    // debugging of services, for instance) - see EchoToConsoleBackground
    // - this property shall be set before any actual logging, otherwise it
    // will have no effect
    // - can be set e.g. to LOG_VERBOSE in order to echo every kind of events
    // - EchoCustom or EchoToConsole can be activated separately
    property EchoToConsole: TSynLogLevels
      read fEchoToConsole write SetEchoToConsole;
    /// redirect all EchoToConsole logging into the Linux journald service
    // - do nothing on Windows or BSD systems
    // - such logs can be exported into a format which can be viewed by our
    // LogView tool using the following command (replacing UNIT with
    // your unit name and PROCESS with the executable name):
    // $ "journalctl -u UNIT --no-hostname -o short-iso-precise --since today | grep "PROCESS\[.*\]:  . " > todaysLog.log"
    property EchoToConsoleUseJournal: boolean
      read fEchoToConsoleUseJournal write SetEchoToConsoleUseJournal;
    /// EchoToConsole output is sent from the flush background thread
    // - enabled by default on Windows, since its console output is very slow
    property EchoToConsoleBackground: boolean
      read fEchoToConsoleBackground write fEchoToConsoleBackground;
    /// can be set to a callback which will be called for each log line
    // - could be used with a third-party logging system
    // - EchoToConsole or EchoCustom can be activated separately
    // - you may even disable the integrated file output, via NoFile := true
    property EchoCustom: TOnTextWriterEcho
      read fEchoCustom write SetEchoCustom;
    /// the associated TSynLog class
    property SynLogClass: TSynLogClass
      read fSynLogClass;
  published
    /// the associated TSynLog class
    property SynLogClassName: string
      read GetSynLogClassName;
    /// index in global SynLogFileFamily[] and PerThreadInfo.FileLookup[] lists
    // - value is always < MAX_SYNLOGFAMILY, i.e. in 0 .. 6 range
    property Ident: byte
      read fIdent;
    /// the current level of logging information for this family
    // - can be set e.g. to LOG_VERBOSE in order to log every kind of events
    property Level: TSynLogLevels
      read fLevel write SetLevel;
    /// the levels which will include a stack trace of the caller
    // - by default, contains sllStackTrace,sllException,sllExceptionOS plus
    // sllError,sllFail,sllLastError,sllDDDError
    // - exceptions will always trace the stack
    property LevelStackTrace: TSynLogLevels
      read fLevelStackTrace write fLevelStackTrace;
    /// the levels which will include some minimal system info
    // - by default, contains sllException, sllExceptionOS and sllLastError
    property LevelSysInfo: TSynLogLevels
      read fLevelSysInfo write fLevelSysInfo;
    /// the folder where the log must be stored
    // - by default, is in the executable folder
    property DestinationPath: TFileName
      read fDestinationPath write SetDestinationPath;
    /// the file extension to be used
    // - is '.log' by default
    property DefaultExtension: TFileName
      read fDefaultExtension write fDefaultExtension;
    /// if TRUE, the log file name will contain the Computer name
    // - as '(MyComputer)' or '(UserName@MyComputer)' patterns
    property IncludeComputerNameInFileName: boolean
      read fIncludeComputerNameInFileName write fIncludeComputerNameInFileName;
    /// if TRUE, the log file name will contain the User name
    // - as '(UserName)' or '(UserName@MyComputer)' patterns
    property IncludeUserNameInFileName: boolean
      read fIncludeUserNameInFileName write fIncludeUserNameInFileName;
    /// can be used to customized the default file name
    // - by default, the log file name is computed from the executable name
    // (and the computer/user name if IncludeComputerNameInFileName or
    // IncludeUserNameInFileName are true)
    // - you can specify your own file name here, to be used instead
    // - this file name should not contain any folder, nor file extension (which
    // are set by DestinationPath and DefaultExtension properties)
    property CustomFileName: TFileName
      read fCustomFileName write fCustomFileName;
    /// the folder where old log files must be compressed
    // - by default, is in the executable folder, i.e. the same as DestinationPath
    // - you can use a remote folder (e.g. on a file server) as backup target
    // - the 'log\' sub folder name will always be appended to this value
    // - will then be used by OnArchive event handler to produce, with the
    // current file date year and month, the final path (e.g.
    // 'ArchivePath\Log\YYYYMM\*.log.synlz' or 'ArchivePath\Log\YYYYMM.zip')
    property ArchivePath: TFileName
      read fArchivePath write fArchivePath;
    /// number of days before OnArchive event will be called to compress
    // or delete deprecated files
    // - will be set by default to 7 days
    // - will be used by Destroy to call OnArchive event handler on time
    property ArchiveAfterDays: integer
      read fArchiveAfterDays write fArchiveAfterDays;
    /// the internal in-memory buffer size, in bytes
    // - this is the number of bytes kept in memory before flushing to the hard
    // drive; you can call TSynLog.Flush method or set AutoFlushTimeOut > 0
    // in order to force the writing to disk
    // - is set to 8192 by default (4KB is the standard hard drive cluster size)
    property BufferSize: integer
      read fBufferSize write fBufferSize;
    /// define how thread will be identified during logging process
    // - by default, ptIdentifiedInOneFile will indicate that all threads are
    // logged in the same file with proper identification after the timestamp
    // - if RotateFileCount and RotateFileSizeKB/RotateFileDailyAtHour are set,
    // will be ignored (internal thread list shall be defined for one process)
    property PerThreadLog: TSynLogPerThreadMode
      read fPerThreadLog write fPerThreadLog;
    /// if TRUE, will log high-resolution time stamp instead of ISO 8601 date and time
    // - this is less human readable, but allows performance profiling of your
    // application on the customer side (using TSynLog.Enter methods)
    // - set to FALSE by default, or if RotateFileCount and RotateFileSizeKB /
    // RotateFileDailyAtHour are set (the high resolution frequency is set
    // in the log file header, so expects a single file)
    property HighResolutionTimestamp: boolean
      read fHighResolutionTimestamp write fHighResolutionTimestamp;
    /// by default, time logging will use error-safe UTC values as reference
    // - you may set this property to TRUE to store local time instead
    property LocalTimestamp: boolean
      read fLocalTimestamp write fLocalTimestamp;
    /// by default, UTC values have no time zone
    // - you may set this property to TRUE to append a Z after the timestamp
    property ZonedTimestamp: boolean
      read fZonedTimestamp write fZonedTimestamp;
    /// if TRUE, will log the unit name with an object instance if available
    // - unit name is available from RTTI if the class has published properties
    // - set to TRUE by default, for better debugging experience
    property WithUnitName: boolean
      read fWithUnitName write fWithUnitName;
    /// if TRUE, will log the pointer with an object instance class if available
    // - set to TRUE by default, for better debugging experience
    property WithInstancePointer: boolean
      read fWithInstancePointer write fWithInstancePointer;
    /// the time (in seconds) after which the log content must be written on
    // disk, whatever the current content size is
    // - equals 0 by default, so that the log file will be written for every 8KB
    // of log (see BufferSize property) - to ensure that the main application
    // won't be slow down during logging
    // - in order not to loose any log, e.g. on an idle server, a background
    // thread can be created and will be responsible of flushing all pending
    // log content every period of time (e.g. every 10 seconds)
    property AutoFlushTimeOut: cardinal
      read fAutoFlushTimeOut write fAutoFlushTimeOut;
    {$ifdef OSWINDOWS}
    /// force no environment variables to be written to the log file
    // - may be usefull if they contain some sensitive information
    property NoEnvironmentVariable: boolean
      read fNoEnvironmentVariable write fNoEnvironmentVariable;
    {$endif OSWINDOWS}
    /// force no log to be written to any file
    // - may be usefull in conjunction e.g. with EchoToConsole or any other
    // third-party logging component
    property NoFile: boolean
      read fNoFile write fNoFile;
    /// auto-rotation of logging files
    // - set to 0 by default, meaning no rotation
    // - can be set to a number of rotating files: rotation and compression will
    // happen, and main file size will be up to RotateFileSizeKB number of bytes,
    // or when RotateFileDailyAtHour time is reached
    // - if set to 1, no .synlz backup will be created, so the main log file will
    // be restarted from scratch when it reaches RotateFileSizeKB size or when
    // RotateFileDailyAtHour time is reached
    // - if set to a number > 1, some rotated files will be compressed using the
    // LogCompressAlgo algorithm (i.e. AlgoSynLZ by default but consider AlgoGZFast
    // from mormot.core.zip to generate .gz files), and will be named e.g. as
    // <MainLogFileName>.0.synlz .. <MainLogFileName>.7.synlz for RotateFileCount
    // as 9 (i.e. total count = 9, including 1 main log file and 8 .synlz files)
    property RotateFileCount: cardinal
      read fRotateFileCount write fRotateFileCount;
    /// maximum size of auto-rotated logging files, in kilo-bytes (per 1024 bytes)
    // - specify the maximum file size upon which .synlz rotation takes place
    // - is not used if RotateFileCount is left to its default 0
    // - note that the file size will be checked when flushing to disk, so
    // RotateFileSizeKB is meaningful only if bigger than BufferSize * 1024
    property RotateFileSizeKB: cardinal
      read fRotateFileSizeKB write fRotateFileSizeKB;
    /// local hour of the day where logging files rotation should be performed
    // - equals -1 by default, meaning no rotation
    // - you can set a time value between 0 and 23 to force the rotation at this
    // specified local/wallclock (not UTC) hour
    // - is not used if RotateFileCount is left to its default 0 value
    property RotateFileDailyAtHour: integer
      read fRotateFileDailyAtHour write fRotateFileDailyAtHour;
    /// the recursive depth of stack trace symbol to write
    // - used only if exceptions are handled, or by sllStackTrace level
    // - default value is 30, maximum is 255 (but API may never reach so high)
    property StackTraceLevel: byte
      read fStackTraceLevel write fStackTraceLevel;
    /// how the stack trace shall use only the Windows API
    // - default is stOnlyAPI, i.e. use RtlCaptureStackBackTrace() API with
    // no manual stack walk (which tends to report wrong calls)
    // - on FPC, this property is ignored in favor of RTL CaptureBacktrace() 
    property StackTraceUse: TSynLogStackTraceUse
      read fStackTraceUse write fStackTraceUse;
    /// how existing log file shall be handled
    property FileExistsAction: TSynLogExistsAction
      read fFileExistsAction write fFileExistsAction;
    /// define how the logger will emit its line feed
    // - by default (FALSE), a single LF (#10) char will be written, to save
    // storage space
    // - you can set this property to TRUE, so that CR+LF (#13#10) chars will
    // be appended instead
    // - TSynLogFile class and our LogView tool will handle both patterns
    property EndOfLineCRLF: boolean
      read fEndOfLineCRLF write fEndOfLineCRLF;
  end;

  /// thread-specific internal threadvar definition used for fast process
  // - consumes 484/512 bytes per thread on CPU32/CPU64
  TSynLogThreadInfo = packed record
    /// number of recursive calls currently stored in Recursion[]
    // - nothing logged above MAX_SYNLOGRECURSION (53) to keep this record small
    RecursionCount: byte;
    /// store per-thread behavior, e.g. to disable exceptions or whole logging
    Flags: TSynLogThreadInfoFlags;
    /// the internal number of this thread, stored as text using Int18ToChars3()
    // - is a value in [1..MAX_SYNLOGTHREADS=65500] range after InitThreadNumber
    // - see SynLogThreads.Ident[ThreadNumber - 1] for ptIdentifiedInOneFile
    // - raw value can be retrieved from TSynLog.ThreadIndex class method
    ThreadNumber: word;
    /// pre-computed "1 shl ((ThreadNumber - 1) and 31)" value
    // - equals 0 if InitThreadNumber() needs to be called
    ThreadBitLo: cardinal;
    /// pre-computed "(ThreadNumber - 1) shr 5" value
    ThreadBitHi: word;
    /// ready-to-be-written text timestamp, filled outside GlobalThreadLock
    // - ptIdentifiedInOneFile appends the ThreadNumber in Int18ToText() format
    // - store up to 19-20 chars - padded with previous fields as 32 bytes
    CurrentTimeAndThread: string[21];
    /// each thread can access to its own TSynLog instance
    // - implements TSynLogFamily.PerThreadLog = ptOneFilePerThread option
    FileLookup: array[0 .. MAX_SYNLOGFAMILY - 1] of TSynLog;
    /// used by TSynLog.Enter methods to handle recursive calls tracing
    // - stores ISynLog.RefCnt in lowest 8-bit, then Current Timestamp shl 8
    // (microseconds as 56-bit do cover 2285 years before overflow)
    // - allow thread-safe non-blocking ISynLog._AddRef/_Release process
    Recursion: array[0 .. MAX_SYNLOGRECURSION - 1] of Int64;
  end;
  PSynLogThreadInfo = ^TSynLogThreadInfo;

  /// a per-family and/or per-thread log file content
  // - you should create a sub class per kind of log file
  // ! TSynLogDB = class(TSynLog);
  // - the TSynLog instance won't be allocated in heap, but will share a
  // per-thread (if Family.PerThreadLog = ptOneFilePerThread) or global private
  // log file instance
  // - was very optimized for speed, if no logging is written, and even during
  // log write (using an internal TTextWriter)
  // - can use available debugging information via the TDebugFile class, for
  // stack trace logging for exceptions, sllStackTrace, and Enter/Leave labelling
  TSynLog = class(TObject, ISynLog)
  // note: don't inherit from TSynInterfacedObject to avoid a method call
  protected
    fFamily: TSynLogFamily;
    fWriter: TJsonWriter;
    fThreadInfo: PSynLogThreadInfo;
    fFlags: set of (logFileHeaderWritten, logInitDone, logAddThreadName);
    fPendingFlags: set of (pendingDisableRemoteLogLeave, pendingRotate);
    fThreadInfoBackup: TSynLogThreadInfoFlags;
    fISynLogOffset: integer;
    fStartTimestamp: Int64;
    fWriterEcho: TEchoWriter;
    fThreadNameLogged: TIntegerDynArray; // bits for ptIdentifiedInOneFile
    fWriterStream: TStream;
    fFileName: TFileName;
    fRotateBytes, fFlushTix32, fRotateDailyTix32: cardinal; // OnFlushToStream
    fStreamPositionAfterHeader: integer;
    fStartTimestampDateTime: TDateTime;
    fWriterClass: TJsonWriterClass;
    class function FamilyCreate: TSynLogFamily;
    // TInterfacedObject methods for fake per-thread RefCnt
    function QueryInterface({$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif}
      iid: TGuid; out obj): TIntQry;
      {$ifdef OSWINDOWS} stdcall {$else} cdecl {$endif};
    function _AddRef: TIntCnt;
      {$ifdef OSWINDOWS} stdcall {$else} cdecl {$endif};
    function _Release: TIntCnt;
      {$ifdef OSWINDOWS} stdcall {$else} cdecl {$endif};
    // internal methods
    function DoEnter: PSynLogThreadInfo; // returns nil if sllEnter is disabled
      {$ifdef FPC}inline;{$endif}
    procedure RaiseDoEnter;
    procedure LockAndPrepareEnter(nfo: PSynLogThreadInfo;
      microsecs: PInt64); // no profit inlining
    function LockAndDisableExceptions: boolean; // no profit inlining
    procedure LogEnter(nfo: PSynLogThreadInfo; inst: TObject; txt: PUtf8Char
      {$ifdef ISDELPHI} ; addr: PtrUInt = 0 {$endif});
    procedure LogEnterFmt(nfo: PSynLogThreadInfo; inst: TObject;
      fmt: PUtf8Char; args: PVarRec; argscount: PtrInt; microsecs: PInt64);
    procedure AddLogThreadName;
    procedure CreateLogWriter; virtual;
    procedure OnFlushToStream(Text: PUtf8Char; Len: PtrInt);
    procedure LogInternalFmt(Level: TSynLogLevel; Format: PUtf8Char;
      Values: PVarRec; ValuesCount: integer; Instance: TObject);
    procedure LogInternalText(Level: TSynLogLevel; Text: PUtf8Char;
      TextLen: PtrInt; Instance: TObject; TextTruncateAtLength: PtrInt);
    procedure LogInternalRtti(Level: TSynLogLevel; const aName: RawUtf8;
      aTypeInfo: PRttiInfo; const aValue; Instance: TObject);
    procedure LogHeader(const Level: TSynLogLevel; Instance: TObject);
      {$ifdef FPC}inline;{$endif}
    procedure LogTrailer(Level: TSynLogLevel);
      {$ifdef FPC}inline;{$endif}
    procedure FillInfo(nfo: PSynLogThreadInfo; MicroSec: PInt64); virtual;
    procedure LogFileInit(nfo: PSynLogThreadInfo);
    procedure LogFileHeader; virtual;
    procedure AddMemoryStats; virtual;
    procedure AddErrorMessage(Error: cardinal);
    procedure AddStackTrace(Stack: PPtrUInt);
    procedure AddSysInfo;
    procedure ComputeFileName; virtual;
    function GetFileSize: Int64; virtual;
    function GetThreadCount: integer;
    procedure PerformRotation(nfo: PSynLogThreadInfo); virtual;
    function Instance: TSynLog;
    function ConsoleEcho(Sender: TEchoWriter; Level: TSynLogLevel;
      const Text: RawUtf8): boolean; virtual;
  public
    /// intialize for a TSynLog class instance
    // - WARNING: not to be called directly! Use TSynLog.Enter or TSynLog.Add
    // class functions instead
    constructor Create(aFamily: TSynLogFamily = nil); virtual;
    /// release all memory and internal handles
    destructor Destroy; override;
    /// flush all log content to file
    // - if ForceDiskWrite is TRUE, will wait until written on disk (slow)
    procedure Flush(ForceDiskWrite: boolean = false);
    /// flush all log content to file and close the file
    procedure CloseLogFile;
    /// flush all log content to file, close the file, and release the instance
    // - you should never call the Free method directly, since the instance
    // is registered in a global TObjectList and an access violation may
    // occur at application closing: you can use this Release method if you
    // are sure that you won't need this TSynLog instance any more
    // - ensure there is no pending Leave element in a stack-allocated ISynLog
    // (see below)
    // - can be used e.g. to release the instance when finishing a thread when
    // Family.PerThreadLog is ptOneFilePerThread:
    // ! var
    // !   TThreadLogger : TSynLogClass = TSynLog;
    // !
    // ! procedure TMyThread.Execute;
    // ! var log : ISynLog;
    // ! begin
    // !   log := TThreadLogger.Enter(self);
    // ! ...
    // !   log := nil; // to force logging end of method
    // !   TThreadLogger.SynLog.Release;
    // ! end;
    procedure Release;
    /// to be called when a thread is ended, if SetThreadName() was also made
    // - should be called in the thread context which is about to terminate,
    // in a situation where no other logging may occur from this thread any more
    // - it will release all thread-specific resource used by this TSynLog
    // - called e.g. by TRest.EndCurrentThread, via TSynLogFamily.OnThreadEnded
    class procedure NotifyThreadEnded; {$ifdef HASINLINE} static; {$endif}
    /// handle generic method enter / auto-leave tracing
    // - returning a ISynLog interface will allow you to have an automated
    // sllLeave log created when the method is left (thanks to the hidden
    // try..finally block generated by the compiler to protect the ISynLog var)
    // - WARNING: due to a limitation (feature?) of the FPC compiler and
    // Delphi 10.4 and later, you NEED to hold the returned value into a
    // local ISynLog variable, as such:
    // ! procedure TMyDB.SQLFlush;
    // ! var log: ISynLog;
    // ! begin
    // !   log := TSynLogDB.Enter(self);
    // !   // do some stuff
    // ! end; // here log will be released, and method leaving will be logged
    // otherwise, the ISynLog instance would be released just after the Enter()
    // call, so the timing won't match the method execution
    // - as a benefit, it is convenient to define a local variable to store
    // the returned ISynLog and use it for any specific logging within
    // the method execution
    // - on Delphi earlier than 10.4 (but not FPC), you could just call Enter()
    // inside the method block, without any ISynLog interface variable - but
    // it is not very future-proof to write the following code:
    // ! procedure TMyDB.SQLFlush;
    // ! begin
    // !   TSynLogDB.Enter(self, 'SQLFlush');
    // !   // do some stuff
    // ! end;
    // - on Delphi, if no aMethodName is supplied, it will use the caller address,
    // and write it as hexa and with full unit and symbol name, if the debugging
    // information is available from TDebugFile, i.e. there is .map/.mab content
    // ! procedure TMyDB.SQLFlush;
    // ! var log: ISynLog;
    // ! begin
    // !   log := TSynLogDB.Enter(self);
    // !   // do some stuff
    // ! end;
    // - note that supplying aMethodName is faster than using the .map content,
    // and is what FPC requires, so it should be preferred for most projects
    // - if TSynLogFamily.HighResolutionTimestamp is TRUE, high-resolution
    // time stamp will be written instead of ISO 8601 date and time: this will
    // allow performance profiling of the application on the customer side
    // - Enter() will write the class name - and the unit name for classes with
    // published properties, if TSynLogFamily.WithUnitName is true:
    //  $ 20110325 19325801  +    MyDBUnit.TMyDB(004E11F4).SQLExecute
    //  $ 20110325 19325801 info   SQL=SELECT * FROM Table;
    //  $ 20110325 19325801  -    01.512.320
    // - may return nil if sllEnter is not enabled for the TSynLog class
    class function Enter(aInstance: TObject = nil;
      aMethodName: PUtf8Char = nil): ISynLog; overload;
      {$ifdef FPC} inline; {$endif}
    /// handle method enter / auto-leave tracing, with some custom text arguments
    // - this overloaded method would not write the method name, but the supplied
    // text content, after expanding the parameters like FormatUtf8()
    // - it will append the corresponding sllLeave log entry when the method ends
    // - warning: may return nil if sllEnter is not enabled for the TSynLog class
    class function Enter(TextFmt: PUtf8Char; const TextArgs: array of const;
      aInstance: TObject = nil): ISynLog; overload;
    /// handle method enter / auto-leave tracing, with some custom text arguments
    // - expects the ISynLog to be a void variable on stack
    // - slightly more efficient - especially on FPC - than plain Enter()
    // - optionally return the TSynLog instance (or nil) for direct call
    // - typical usage is the following, very close to TSynLog.Enter:
    // ! var logger: ISynLog;
    // ! begin
    // !   TSynLog.EnterLocal(logger, self, 'MyMethod');
    // !   // do some stuff
    // !   if Assigned(logger) then // may be nil if sllEnter is not enabled
    // !     logger.Log(sllInfo,'method called');
    // ! end; // when logger is out-of-scope, will log the method leaving
    class function EnterLocal(var Local: ISynLog; aInstance: TObject;
      aMethodName: PUtf8Char): TSynLog; overload;
    /// handle method enter / auto-leave tracing, with some custom text arguments
    // - expects the ISynLog to be a void variable on stack
    // - slightly more efficient - especially on FPC - than plain Enter()
    // - optionally return the TSynLog instance (or nil) for direct usage
    // - optionally return the TSynLog instance (or nil) for direct call
    // - typical usage is the following, very close to TSynLog.Enter:
    // ! var logger: ISynLog;
    // ! begin
    // !   TSynLog.EnterLocal(logger, 'MyMethodWithParams(%,%)', [a, b], self);
    // !   // do some stuff
    // !   if Assigned(logger) then // may be nil if sllEnter is not enabled
    // !     logger.Log(sllInfo,'method called');
    // ! end; // when logger is out-of-scope, will log the method leaving
    class function EnterLocal(var Local: ISynLog; TextFmt: PUtf8Char;
      const TextArgs: array of const; aInstance: TObject = nil): TSynLog; overload;
    /// handle method enter / auto-leave tracing, with some custom string arguments
    // - the logged text is supplied as generic string value, not RawUtf8/PUtf8Char
    // - expects the ISynLog to be a void variable on stack
    class function EnterLocalString(var Local: ISynLog; aInstance: TObject;
      const aMethodName: string): TSynLog;
    /// retrieve the current instance of this TSynLog class
    // - to be used for direct logging, without any Enter/Leave:
    // ! TSynLogDB.Add.Log(llError,'The % statement didn''t work',[SQL]);
    // - to be used for direct logging, without any Enter/Leave (one parameter
    // version - just the same as previous):
    // ! TSynLogDB.Add.Log(llError,'The % statement didn''t work',SQL);
    // - is just a wrapper around Family.SynLog - the same code will work:
    // ! TSynLogDB.Family.SynLog.Log(llError,'The % statement didn''t work',[SQL]);
    // - warning: NEVER assign the returned instance to a ISynLog variable - use
    // TSynLog.Enter or TSynLog.EnterLocal if you want to have a ISynLog
    class function Add: TSynLog;
      {$ifdef HASINLINE}inline;{$endif}
    /// retrieve the family of this TSynLog class type
    class function Family: TSynLogFamily; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// check some specific level(s) in the family of this TSynLog class type
    class function HasLevel(levels: TSynLogLevels): boolean;
      {$ifndef NOPATCHVMT} {$ifdef HASINLINE}inline;{$endif} {$endif}
    /// return a sequential 16-bit integer to identify the current thread
    // - as stored internally by TSynLogThreadInfo.ThreadNumber - 1
    // - by design, returns a value in range [0 .. MAX_SYNLOGTHREADS - 1]
    // - could be used as a sequential small alternative to GetCurrentThreadId
    // if you know that TSynLog.NotifyThreadEnded is properly called
    // - note that after TSynLog.NotifyThreadEnded call, a number/slot will be
    // reused so it could be a nice way of implementing per-thread resources
    // with automatic re-use between short-living threads, e.g. as it is
    // by TSqlDBConnectionPropertiesThreadSafe.ThreadSafeConnection
    class function ThreadIndex: PtrInt; {$ifdef HASINLINE} static; {$endif}
    /// returns a logging class which will never log anything
    // - i.e. a TSynLog sub-class with Family.Level := []
    class function Void: TSynLogClass;
    /// low-level method helper which can be called to make debugging easier
    // - log some warning message to the TSynLog family
    // - will force a manual breakpoint if tests are run from the Delphi IDE, or
    // will output the message to the current console
    class procedure DebuggerNotify(Level: TSynLogLevel; const Format: RawUtf8;
      const Args: array of const); overload;
    /// low-level method helper which can be called to make debugging easier
    class procedure DebuggerNotify(Level: TSynLogLevel; const Text: RawUtf8); overload;
    /// call this method to add some information to the log at the specified level
    // - will use TTextWriter.Add(...,twOnSameLine) to append its content
    // - % = #37 indicates a string, integer, floating-point, class parameter
    // to be appended as text (e.g. class name), any variant as JSON...
    // - note that cardinal values should be type-casted to Int64() (otherwise
    // the integer mapped value will be transmitted, therefore wrongly)
    procedure Log(Level: TSynLogLevel; Fmt: PUtf8Char;
      const Args: array of const; aInstance: TObject = nil); overload;
    /// call this method to add some information to the log at the specified level
    // - if Instance is set and Text is not '', it will log the corresponding
    // class name and address (to be used e.g. if you didn't call TSynLog.Enter()
    // method first) - for instance
    // ! TSqlLog.Add.Log(sllDebug,'GarbageCollector',GarbageCollector);
    // will append this line to the log:
    // $ 0000000000002DB9 debug TObjectList(00425E68) GarbageCollector
    // - if Instance is set and Text is '', will behave the same as
    // Log(Level,Instance), i.e. write the Instance as JSON content
    procedure Log(Level: TSynLogLevel; const Text: RawUtf8; aInstance: TObject = nil;
      TextTruncateAtLength: PtrInt = 0); overload;
      {$ifdef HASINLINE} inline; {$endif}
    {$ifdef UNICODE}
    /// call this method to add some RTL string to the log at a specified level
    // - this overloaded version will avoid a call to StringToUtf8()
    procedure Log(Level: TSynLogLevel; const Text: string;
      aInstance: TObject = nil); overload;
      {$ifdef HASINLINE} inline; {$endif}
    {$endif UNICODE}
    /// call this method to add the content of an object to the log at a
    // specified level
    // - this default implementation will just write the class name and its hexa
    // pointer value, and handle TList, TCollections and TStrings - for instance:
    // ! TSynLog.Add.Log(sllDebug,GarbageCollector);
    // will append this line to the log:
    // $ 20110330 10010005 debug {"TObjectList(00B1AD60)":["TObjectList(00B1AE20)","TObjectList(00B1AE80)"]}
    // - if aInstance is an Exception, it will handle its class name and Message:
    // $ 20110330 10010005 debug "EClassName(00C2129A)":"Exception message"
    procedure Log(Level: TSynLogLevel; aInstance: TObject); overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// call this method to add the content of most low-level types to the log
    // at a specified level
    // - this overridden implementation will write the value content,
    // written as human readable JSON: handle dynamic arrays and enumerations
    procedure Log(Level: TSynLogLevel; const aName: RawUtf8; aTypeInfo: PRttiInfo;
      const aValue; Instance: TObject); overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// call this method to add the caller address to the log at the specified level
    // - if the debugging info is available from TDebugFile, will log the
    // unit name, associated symbol and source code line
    procedure Log(Level: TSynLogLevel); overload;
    /// call this method to add the content of a PUtf8Char buffer
    // - is slightly more optimized than Log(RawUtf8) or LogText(Text,TextLen)
    procedure LogText(Level: TSynLogLevel; Text: PUtf8Char; Instance: TObject); overload;
    /// call this method to add the content of a PUtf8Char buffer and length
    procedure LogText(Level: TSynLogLevel; Text: PUtf8Char; TextLen: PtrInt;
      Instance: TObject; TextTruncateAtLength: PtrInt = 0); overload;
    /// call this method to add the content of a binary buffer with ASCII escape
    // - precompute up to TruncateLen (1024) bytes of output before writing with a
    // hardcoded limit of MAX_LOGESCAPE = 4KB text output for pre-rendering on stack
    procedure LogEscape(Level: TSynLogLevel;
      const ContextFmt: RawUtf8; const ContextArgs: array of const; Data: pointer;
      DataLen: PtrInt; Instance: TObject; TruncateLen: PtrInt = 1024);
    /// allows to identify the current thread with a textual representation
    // - redirect to SetThreadName/SetCurrentThreadName global function
    // - would append an sllInfo entry with "SetThreadName ThreadID=Name" text
    // - if Name='', will use CurrentThreadNameShort^ threadvar
    class procedure LogThreadName(const Name: RawUtf8);
      {$ifdef HASINLINE} static; {$endif}
    /// call this method to add some multi-line information to the log at a
    // specified level
    // - LinesToLog content will be added, one line per one line, delimited by
    // #13#10 (CRLF)
    // - if a line starts with IgnoreWhenStartWith (already uppercase), it won't
    // be added to the log content (to be used e.g. with '--' for SQL statements)
    procedure LogLines(Level: TSynLogLevel; LinesToLog: PUtf8Char; aInstance: TObject = nil;
      const IgnoreWhenStartWith: PAnsiChar = nil);
    /// manual low-level TSynLog.Enter execution without the ISynLog overhead
    // - may be used to log Enter/Leave stack from non-pascal code
    // - each call to ManualEnter should be followed by a matching ManualLeave
    procedure ManualEnter(aMethodName: PUtf8Char; aInstance: TObject = nil); overload;
    /// manual low-level TSynLog.Enter execution without the ISynLog overhead
    // - may be used to log Enter/Leave stack from non-pascal code
    // - each call to ManualEnter should be followed by a matching ManualLeave
    procedure ManualEnter(aInstance: TObject; TextFmt: PUtf8Char;
      const TextArgs: array of const; MicroSecs: PInt64 = nil); overload;
    /// manual low-level ISynLog release after TSynLog.Enter execution
    // - each call to ManualEnter should be followed by a matching ManualLeave
    procedure ManualLeave;
      {$ifdef HASINLINE}inline;{$endif}
    /// allow to temporary disable remote logging
    // - will enter the GlobalThreadLock - and is NOT reentrant
    // - to be used within a try ... finally section:
    // ! log.DisableRemoteLog(true);
    // ! try
    // !   log.Log(....); // won't be propagated to the remote log
    // ! finally
    // !   log.DisableRemoteLog(false);
    // ! end;
    procedure DisableRemoteLog(entervalue: boolean);
    /// the associated TSynLog class
    function LogClass: TSynLogClass;
      {$ifdef HASINLINE}inline;{$endif}
    /// low-level class method which can be assigned to TSynLogProc callback
    // signature, or used instead of Add.Log
    // - will flush the content to disk and avoid any memory reallocation
    // if Level is sllExceptionOS, e.g. on SIGABRT/SIGQUIT/SIGINT
    class procedure DoLog(Level: TSynLogLevel; Fmt: PUtf8Char;
      const Args: array of const; Instance: TObject = nil);
    /// low-level class method which can be assigned to a TOnInfoProgress callback
    // - as used e.g. by TStreamRedirect.OnInfoProgress or TZipAbstract.OnProgress
    class procedure ProgressInfo(Sender: TObject; Info: PProgressInfo);
    /// Force log rotation; Can be used for example inside SUGHUP signal handler
    procedure ForceRotation;
    /// direct access to the low-level writing content
    // - should usually not be used directly, unless you ensure it is safe
    property Writer: TJsonWriter
      read fWriter;
  published
    /// the associated file name containing the log
    // - this is accurate only with the default implementation of the class:
    // any child may override it with a custom logging mechanism
    property FileName: TFileName
      read fFileName;
    /// the current size, in bytes, of the associated file containing the log
    property FileSize: Int64
      read GetFileSize;
    /// the current number of thread contexts associated with this process
    // - doesn't match necessary the number of threads of the process, but the
    // threads which are still marked as active for any TSynLog
    // - a huge number may therefore not indicate a potential "out of memory"
    // error, but a broken logic with missing NotifyThreadEnded calls
    property ThreadCount: integer
      read GetThreadCount;
    /// the associated logging family
    property GenericFamily: TSynLogFamily
      read fFamily;
  end;

  TSynLogDynArray = array of TSynLog;

const
  /// maximum content size for TSynLog.LogEscape
  MAX_LOGESCAPE = 4096;

{$ifdef NOPATCHVMT}
var
  LastFamily: TSynLogFamily; // very likely to be a single class involved
{$endif NOPATCHVMT}

{$ifndef PUREMORMOT2}
const
  ptIdentifiedInOnFile = ptIdentifiedInOneFile;
{$endif PUREMORMOT2}


{ ************** High-Level Logs and Exception Related Features }

{$ifndef NOEXCEPTIONINTERCEPT}

type
  /// storage of the information associated with an intercepted exception
  // - as returned by GetLastException() function
  TSynLogExceptionInfo = record
    /// low-level calling context
    // - as used by TSynLogExceptionToStr callbacks
    Context: TSynLogExceptionContext;
    /// associated Exception.Message content (if any)
    Message: string;
  end;

  /// storage of information associated with one or several exceptions
  // - as returned by GetLastExceptions() function
  TSynLogExceptionInfoDynArray = array of TSynLogExceptionInfo;

/// makes a thread-safe copy of the latest intercepted exception
function GetLastException(out info: TSynLogExceptionInfo): boolean;

/// convert low-level exception information into some human-friendly text
function ToText(var info: TSynLogExceptionInfo): RawUtf8; overload;

/// returns some text about the latest intercepted exception
function GetLastExceptionText: RawUtf8;

/// makes a thread-safe copy of the latest intercepted exceptions
procedure GetLastExceptions(out result: TSynLogExceptionInfoDynArray;
  Depth: integer = 0); overload;

var
  /// a run-time alternative to the NOEXCEPTIONINTERCEPT global conditional
  // - this global variable affects TSynLogFamily.SetLevel() process
  SynLogNoExceptionIntercept: boolean;

{$endif NOEXCEPTIONINTERCEPT}


type
  /// a mORMot-SOA compatible callback definition
  // - used to notify a remote mORMot server via interface-based serivces
  // for any incoming event, using e.g. TSynLogCallbacks.Subscribe
  ISynLogCallback = interface(IInvokable)
    ['{9BC218CD-A7CD-47EC-9893-97B7392C37CF}']
    /// each line of the TTextWriter internal instance will trigger this method
    // - similar to TOnTextWriterEcho, as defined in mormot.core.text
    // - an initial call with Level=sllNone and the whole previous Text may be
    // transmitted, if ReceiveExistingKB is set for TSynLogCallbacks.Subscribe()
    procedure Log(Level: TSynLogLevel; const Text: RawUtf8);
  end;

  /// store a subscription to ISynLogCallback
  TSynLogCallback = record
    /// the log levels supplied to TSynLogCallbacks.Subscribe()
    Levels: TSynLogLevels;
    /// the callback interface supplied to TSynLogCallbacks.Subscribe()
    Callback: ISynLogCallback;
  end;

  /// store several subscribed ISynLogCallback
  TSynLogCallbackDynArray = array of TSynLogCallback;

  /// can manage a list of ISynLogCallback registrations
  TSynLogCallbacks = class(TObjectOSLock)
  protected
    fCount: integer;
    fCurrentlyEchoing: boolean;
  public
    /// direct access to the registration storage
    Registration: TSynLogCallbackDynArray;
    /// high-level access to the registration storage
    Registrations: TDynArray;
    /// the TSynLog family actually associated with those callbacks
    TrackedLog: TSynLogFamily;
    /// initialize the registration storage for a given TSynLogFamily instance
    constructor Create(aTrackedLog: TSynLogFamily); reintroduce;
    /// finalize the registration storage for a given TSynLogFamily instance
    destructor Destroy; override;
    /// register a callback for a given set of log levels
    // - you can specify a number of KB of existing log content to send to the
    // monitoring tool, before the actual real-time process
    function Subscribe(const Levels: TSynLogLevels; const Callback: ISynLogCallback;
      ReceiveExistingKB: cardinal = 0): integer; virtual;
    /// unregister a callback previously registered by Subscribe()
    procedure Unsubscribe(const Callback: ISynLogCallback); virtual;
    /// notify a given log event
    // - matches the TOnTextWriterEcho signature
    function OnEcho(Sender: TEchoWriter; Level: TSynLogLevel;
      const Text: RawUtf8): boolean;
  published
    /// how many registrations are currently defined
    property Count: integer
      read fCount;
  end;

  /// store simple log-related settings
  // - see also TDDDLogSettings in dddInfraSettings.pas and TSynDaemonSettings
  // in mORMotService.pas, which may be more integrated
  TSynLogSettings = class(TSynPersistent)
  protected
    fLevels: TSynLogLevels;
    fDestinationPath: TFileName;
    fRotateFileCount: integer;
    fLogClass: TSynLogClass;
  public
    /// set some default values
    constructor Create; override;
    /// define the log information into the supplied TSynLog class
    // - if you don't call this method, the logging won't be initiated
    procedure SetLog(aLogClass: TSynLogClass = nil);
    /// read-only access to the TSynLog class, if SetLog() has been called
    property LogClass: TSynLogClass
      read fLogClass;
  published
    /// the log levels to be used for the log file
    // - i.e. a combination of none or several logging event
    // - if "*" is serialized, unneeded sllNone won't be part of the set
    // - default is LOG_STACKTRACE
    property Levels: TSynLogLevels
      read fLevels write fLevels;
    /// allow to customize where the logs should be written
    // - default is the system log folder (e.g. /var/log on Linux)
    property DestinationPath: TFileName
      read fDestinationPath write fDestinationPath;
    /// how many files will be rotated (default is 2)
    property RotateFileCount: integer
      read fRotateFileCount write fRotateFileCount;
  end;


/// a TSynLogArchiveEvent handler which will just delete older .log files
function EventArchiveDelete(
  const aOldLogFileName, aDestinationPath: TFileName): boolean;

/// a TSynLogArchiveEvent handler which will compress older .log files
// using our proprietary SynLZ format
// - resulting file will have the .synlz extension and will be located
// in the aDestinationPath directory, i.e. TSynLogFamily.ArchivePath+'\log\YYYYMM\'
// - use UnSynLZ.dpr tool to uncompress it into .log textual file
// - SynLZ is much faster than zip for compression content, but proprietary
function EventArchiveSynLZ(
  const aOldLogFileName, aDestinationPath: TFileName): boolean;


{ ************** Efficient .log File Access via TSynLogFile }

type
  /// used by TSynLogFile to refer to a method profiling in a .log file
  // - i.e. map a sllEnter/sllLeave event in the .log file
  TSynLogFileProc = record
    /// the index of the sllEnter event in the TSynLogFile.fLevels[] array
    Index: cardinal;
    /// the associated time elapsed in this method (in micro seconds)
    // - computed from the sllLeave time difference (high resolution timer)
    Time: cardinal;
    /// the time elapsed in this method and not in nested methods
    // - computed from Time property, minus the nested calls
    ProperTime: cardinal;
  end;
  PSynLogFileProc = ^TSynLogFileProc;

  /// used by TSynLogFile to refer to global method profiling in a .log file
  // - i.e. map all sllEnter/sllLeave event in the .log file
  TSynLogFileProcDynArray = array of TSynLogFileProc;

  TSynLogFileProcArray = array[0..(MaxInt div SizeOf(TSynLogFileProc)) - 1] of TSynLogFileProc;
  PSynLogFileProcArray = ^TSynLogFileProcArray;

  /// used by TSynLogFile.LogProcSort method
  TLogProcSortOrder = (
    soNone,
    soByName,
    soByOccurrence,
    soByTime,
    soByProperTime);

  /// used to parse a .log file, as created by TSynLog, into high-level data
  // - this particular TMemoryMapText class will retrieve only valid event lines
  // (i.e. will fill EventLevel[] for each line <> sllNone)
  // - Count is not the global text line numbers, but the number of valid events
  // within the file (LinePointers/Line/Strings will contain only event lines) -
  // it will not be a concern, since the .log header is parsed explicitly
  TSynLogFile = class(TMemoryMapText)
  protected
    /// map the events occurring in the .log file content
    fLevels: TSynLogLevelDynArray;
    fLineLevelOffset: byte;
    fLineTextOffset: byte;
    fLineHeaderCountToIgnore: byte;
    fThreadsCount: integer;
    fThreadMax: cardinal;
    fThreads: TWordDynArray; // = EventThread[] for each line
    fThreadInfo: array of record // by [thread]
      Rows: cardinal;
      SetThreadName: TPUtf8CharDynArray; // TSynLog.AddLogThreadName locations
    end;
    /// as extracted from the .log header
    fExeName, fExeVersion, fInstanceName: RawUtf8;
    fHost, fUser, fCPU, fOSDetailed, fFramework: RawUtf8;
    fExeDate: TDateTime;
    fOS: TWindowsVersion;
    fWow64, fWow64Emulated: boolean;
    fOSServicePack: integer;
    fStartDateTime: TDateTime;
    fDayCurrent: Int64; // as PInt64('20160607')^
    fDayChangeIndex: TIntegerDynArray;
    fDayCount: TIntegerDynArray;
    /// retrieve all used event levels
    fLevelUsed: TSynLogLevels;
    /// =0 if date time resolution, >0 if high-resolution time stamp
    fFreq: Int64;
    /// used by EventDateTime() to compute date from time stamp
    fFreqPerDay: double;
    /// custom headers, to be searched as .ini content
    fHeaderLinesCount: integer;
    fHeaders: RawUtf8;
    /// method profiling data
    fLogProcCurrentCount: integer;
    fLogProcNaturalCount: integer;
    fLogProcCurrent: PSynLogFileProcArray; // pointer(fLogProcNatural/fLogProcMerged)
    fLogProcNatural: TSynLogFileProcDynArray; // one item per sllEnter/sllLeave
    fLogProcMerged: TSynLogFileProcDynArray;  // merged by soByName
    fLogProcIsMerged: boolean;
    fLogProcSortInternalOrder: TLogProcSortOrder;
    fLogProcStack: array of TIntegerDynArray; // sllEnter stack by [thread]
    fLogProcStackCount: array of integer; // count of each fLogProcStack[thread]
    fLogProcSortInternalComp: function(A, B: PtrInt): PtrInt of object;
    /// used by ProcessOneLine/GetLogLevelTextMap
    fLogLevelsTextMap: array[TSynLogLevel] of cardinal;
    fIntelCPU: TIntelCpuFeatures;
    fArm32CPU: TArm32HwCaps;
    fArm64CPU: TArm64HwCaps;
    procedure SetLogProcMerged(const Value: boolean);
    function GetEventText(index: integer): RawUtf8;
    function GetLogLevelFromText(LineBeg: PUtf8Char): TSynLogLevel;
      {$ifdef HASINLINE} inline; {$endif}
    /// retrieve headers + fLevels[] + fLogProcNatural[], and delete invalid fLines[]
    procedure LoadFromMap(AverageLineLength: integer = 32); override;
    procedure CleanLevels;
    procedure RecomputeTime(p: PSynLogFileProc);
    function ComputeProperTime(start: PSynLogFileProc): PSynLogFileProc;
    /// compute fLevels[] + fLogProcNatural[] for each .log line during initial reading
    procedure ProcessOneLine(LineBeg, LineEnd: PUtf8Char); override;
    /// called by LogProcSort method
    function LogProcSortCompByName(A, B: PtrInt): PtrInt;
    function LogProcSortCompByOccurrence(A, B: PtrInt): PtrInt;
    function LogProcSortCompByTime(A, B: PtrInt): PtrInt;
    function LogProcSortCompByProperTime(A, B: PtrInt): PtrInt;
    function LogProcSortCompDefault(A, B: PtrInt): PtrInt;
    procedure LogProcSortInternal(L, R: PtrInt);
  public
    /// initialize internal structure
    constructor Create; override;
    /// returns TRUE if the supplied text is contained in the corresponding line
    function LineContains(const aUpperSearch: RawUtf8; aIndex: integer): boolean; override;
    /// retrieve the date and time of an event
    // - returns 0 in case of an invalid supplied index
    function EventDateTime(aIndex: integer): TDateTime;
    /// retrieve the description text of an event, as native RTL string
    // - returns '' if supplied index is out of range
    // - if the text is not truly UTF-8 encoded, would use the current system
    // codepage to create a valid string
    // - you may specify a text to replace all #9 characters occurrences
    // - is used e.g. in TMainLogView.ListDrawCell
    function EventString(index: integer; const replaceTabs: RawUtf8 = '';
      maxutf8len: integer = 0; includeFirstColumns: boolean = false): string;
    /// sort the LogProc[] array according to the supplied order
    procedure LogProcSort(Order: TLogProcSortOrder);
    /// return the number of matching events in the log
    function EventCount(const aSet: TSynLogLevels): integer;
    /// add a new line to the already parsed content
    // - overriden method which would identify the freq=%,%,% pseudo-header
    procedure AddInMemoryLine(const aNewLine: RawUtf8); override;
    /// returns the name of a given thread, according to the position in the log
    function ThreadName(ThreadID, CurrentLogIndex: integer): RawUtf8;
    /// returns the name of all threads, according to the position in the log
    // - result[0] stores the name of ThreadID = 1
    function ThreadNames(CurrentLogIndex: integer): TRawUtf8DynArray;
    /// returns all days of this log file
    // - only available for low-resolution timestamp, i.e. Freq=0
    procedure GetDays(out Days: TDateTimeDynArray);
    /// returns the number of occurrences of a given thread
    function ThreadRows(ThreadID: integer): cardinal;
    /// retrieve the level of an event
    // - is calculated by Create() constructor
    // - EventLevel[] array index is from 0 to Count-1
    property EventLevel: TSynLogLevelDynArray
      read fLevels;
    /// retrieve all used event levels
    // - is calculated by Create() constructor
    property EventLevelUsed: TSynLogLevels
      read fLevelUsed;
    /// retrieve the description text of an event
    // - returns '' if supplied index is out of range
    // - see also EventString() function, for direct UI display as RTL string
    property EventText[index: integer]: RawUtf8
      read GetEventText;
    /// retrieve all event thread IDs
    // - contains something if TSynLogFamily.PerThreadLog was ptIdentifiedInOneFile
    // - for ptMergedInOneFile (default) or ptOneFilePerThread logging process,
    // the array will be void (EventThread=nil)
    property EventThread: TWordDynArray
      read fThreads;
    /// the maximum recognized thread number
    // - some of the threads may have no event/row in this actual .log file
    property ThreadsCount: cardinal
      read fThreadMax;
    /// profiled methods information
    // - is calculated by Create() constructor
    // - will contain the sllEnter index, with the associated elapsed time
    // - number of items in the array is retrieved by the LogProcCount property
    property LogProc: PSynLogFileProcArray
      read fLogProcCurrent;
    /// the current sort order
    property LogProcOrder: TLogProcSortOrder
      read fLogProcSortInternalOrder;
    /// if the method information must be merged for the same method name
    property LogProcMerged: boolean
      read fLogProcIsMerged write SetLogProcMerged;
    /// all used event levels, as retrieved at log file content parsing
    property LevelUsed: TSynLogLevels
      read fLevelUsed;
    /// high-resolution time stamp frequence, as retrieved from log file header
    // - equals 0 if date time resolution, >0 if high-resolution time stamp
    property Freq: Int64
      read fFreq;
    /// the row indexes where the day changed
    // - only available for low-resolution timestamp, i.e. Freq=0
    // - if set, contains at least [0] if the whole log is over a single day
    property DayChangeIndex: TIntegerDynArray
      read fDayChangeIndex;
    /// the number of rows for each DayChangeIndex[] value
    property DayCount: TIntegerDynArray
      read fDayCount;
    /// custom headers, to be searched as .ini content
    property Headers: RawUtf8
      read fHeaders;
    /// the available Intel/AMD CPU features, as recognized at program startup
    // - is extracted from the last part of the CPU property text
    // - you could use the overloaded ToText() function to show it in an
    // human-friendly way
    property IntelCPU: TIntelCpuFeatures
      read fIntelCPU;
    /// the available 32-bit ARM CPU features, as recognized at program startup
    property Arm32CPU: TArm32HwCaps
      read fArm32CPU;
    /// the available 64-bit ARM CPU features, as recognized at program startup
    property Arm64CPU: TArm64HwCaps
      read fArm64CPU;
  published
    /// the associated executable name (with path)
    // - returns e.g. 'C:\Dev\lib\SQLite3\exe\TestSQL3.exe'
    property ExecutableName: RawUtf8
      read fExeName;
    /// the associated executable version
    // - returns e.g. '0.0.0.0'
    property ExecutableVersion: RawUtf8
      read fExeVersion;
    /// the associated executable build date and time
    property ExecutableDate: TDateTime
      read fExeDate;
    /// for a library, the associated instance name (with path)
    // - returns e.g. 'C:\Dev\lib\SQLite3\exe\TestLibrary.dll'
    // - for an executable, will be left void
    property InstanceName: RawUtf8
      read fInstanceName;
    /// the computer host name in which the process was running on
    property ComputerHost: RawUtf8
      read fHost;
    /// the computer user name who launched the process
    property RunningUser: RawUtf8
      read fUser;
    /// the computer CPU in which the process was running on
    // - returns e.g. '1*0-15-1027'
    property CPU: RawUtf8
      read fCPU;
    /// the computer Operating System in which the process was running on
    // - equals wUnknown on Linux or BSD - use DetailedOS instead
    property OS: TWindowsVersion
      read fOS;
    /// the Operating System Service Pack number
    // - not defined on Linux or BSD - use DetailedOS instead
    property ServicePack: integer
      read fOSServicePack;
    /// if the 32 bit process was running under WOW 64 virtual emulation
    property Wow64: boolean
      read fWow64;
    /// if the process was running under WOW 64 hardware emulation, e.g. Prism
    property Wow64Emulated: boolean
      read fWow64Emulated;
    /// the computer Operating System in which the process was running on
    // - returns e.g. '2.3=5.1.2600' for Windows XP
    // - under Linux, it will return the full system version, e.g.
    // 'Ubuntu=Linux-3.13.0-43-generic#72-Ubuntu-SMP-Mon-Dec-8-19:35:44-UTC-2014'
    property DetailedOS: RawUtf8
      read fOSDetailed;
    /// the associated framework information
    // - returns e.g. 'TSynLog 2.0.1 x64MMs'
    property Framework: RawUtf8
      read fFramework;
    /// the date and time at which the log file was started
    property StartDateTime: TDateTime
      read fStartDateTime;
    /// number of profiled methods in this .log file
    // - i.e. number of items in the LogProc[] array
    property LogProcCount: integer
      read fLogProcCurrentCount;
  end;

  /// used to parse a .log file and process into VCL/LCL/FMX
  // - would handle e.g. selection and search feature
  TSynLogFileView = class(TSynLogFile)
  protected
    fSelected: TIntegerDynArray;
    fSelectedCount: integer;
    fEvents: TSynLogLevels;
    fThreadSelected: TByteDynArray;
    fThreadSelectedMax: integer;
    procedure LoadFromMap(AverageLineLength: integer = 32); override;
    function GetThreads(thread: integer): boolean;
      {$ifdef HASINLINE} inline; {$endif}
    procedure SetThreads(thread: integer; value: boolean);
  public
    /// add a new line to the already parsed content
    // - overriden method would add the inserted index to Selected[]
    procedure AddInMemoryLine(const aNewLine: RawUtf8); override;
    /// search for the next matching TSynLogLevel, from the current row index
    // - returns -1 if no match was found
    function SearchNextEvent(aEvent: TSynLogLevel; aRow: integer): PtrInt;
    /// search for the next matching text, from the current row index
    // - returns -1 if no match was found
    function SearchNextText(const aPattern: RawUtf8; aRow, aDelta: integer): PtrInt;
    /// search for the previous matching text, from the current row index
    // - returns -1 if no match was found
    function SearchPreviousText(const aPattern: RawUtf8; aRow: integer): PtrInt;
    /// search for the matching Enter/Leave item, from the current row index
    // - returns -1 if no match was found
    function SearchEnterLeave(aRow: integer): PtrInt;
    /// search for the next specified thread, from the current row index
    // - returns -1 if no match was found
    function SearchThread(aThreadID: word; aRow: integer): PtrInt;
    /// search for the next diverse thread, from the current row index
    // - returns -1 if no match was found
    function SearchNextThread(aRow: integer): PtrInt;
    /// search for the next matching thread, from the current row index
    // - returns -1 if no match was found
    function SearchNextSameThread(aRow: integer): PtrInt;
    /// search for the next row index, appearing after the supplied item index
    // - returns -1 if no match was found
    function SearchNextSelected(aIndex: integer): PtrInt;
    /// search for the previous matching thread, from the current row index
    // - returns -1 if no match was found
    function SearchPreviousSameThread(aRow: integer): PtrInt;
    /// returns the ready-to-be text of a cell of the main TDrawGrid
    function GetCell(aCol, aRow: integer; out aLevel: TSynLogLevel): string;
    /// returns the ready-to-be displayed text of one or several selected rows
    function GetLineForMemo(aRow, aTop, aBottom: integer): string;
    /// returns the ready-to-be copied text of a selected row
    function GetLineForClipboard(aRow: integer): string;
    /// fill all rows matching Events and Threads[] properties in Selected[]
    // - you may specify the current selected row index, which would return
    // the closest one after the selection has been applied
    function Select(aRow: integer): integer; virtual;
    /// set all Threads[] to a specified value
    procedure SetAllThreads(enabled: boolean);
    /// define the current selection range, according to event kinds
    // - once you have set Events and Threads[], call Select() to fill Selected[]
    property Events: TSynLogLevels
      read fEvents write fEvents;
    /// define the current selection range, according to a thread ID
    // - here the supplied thread ID starts at 1
    // - once you have set Events and Threads[], call Select() to fill Selected[]
    property Threads[thread: integer]: boolean read
      GetThreads write SetThreads;
    /// the row indexes of the selected entries
    property Selected: TIntegerDynArray
      read fSelected;
    /// how many entries are currently stored in Selected[]
    property SelectedCount: integer
      read fSelectedCount;
  end;


{ **************  SysLog Messages Support as defined by RFC 5424 }

type
  /// syslog message facilities as defined by RFC 3164
  TSyslogFacility = (
    sfKern,
    sfUser,
    sfMail,
    sfDaemon,
    sfAuth,
    sfSyslog,
    sfLpr,
    sfNews,
    sfUucp,
    sfClock,
    sfAuthpriv,
    sfFtp,
    sfNtp,
    sfAudit,
    sfAlert,
    sfCron,
    sfLocal0,
    sfLocal1,
    sfLocal2,
    sfLocal3,
    sfLocal4,
    sfLocal5,
    sfLocal6,
    sfLocal7);

  /// syslog message severities as defined by RFC 5424
  TSyslogSeverity = (
    ssEmerg,
    ssAlert,
    ssCrit,
    ssErr,
    ssWarn,
    ssNotice,
    ssInfo,
    ssDebug);

const
  /// used to convert a TSynLog event level into a syslog message severity
  LOG_TO_SYSLOG: array[TSynLogLevel] of TSyslogSeverity = (
    ssDebug,   // sllNone
    ssInfo,    // sllInfo
    ssDebug,   // sllDebug
    ssDebug,   // sllTrace
    ssNotice,  // sllWarning
    ssWarn,    // sllError
    ssDebug,   // sllEnter
    ssDebug,   // sllLeave
    ssWarn,    // sllLastError
    ssErr,     // sllException
    ssErr,     // sllExceptionOS
    ssDebug,   // sllMemory
    ssDebug,   // sllStackTrace
    ssNotice,  // sllFail
    ssDebug,   // sllSQL
    ssDebug,   // sllCache
    ssDebug,   // sllResult
    ssDebug,   // sllDB
    ssDebug,   // sllHTTP
    ssDebug,   // sllClient
    ssDebug,   // sllServer
    ssDebug,   // sllServiceCall
    ssDebug,   // sllServiceReturn
    ssDebug,   // sllUserAuth
    ssDebug,   // sllCustom1
    ssDebug,   // sllCustom2
    ssDebug,   // sllCustom3
    ssDebug,   // sllCustom4
    ssNotice,  // sllNewRun
    ssWarn,    // sllDDDError
    ssInfo,    // sllDDDInfo
    ssDebug);  // sllMonitoring

/// append some information to a syslog message memory buffer
// - following https://tools.ietf.org/html/rfc5424 specifications
// - ready to be sent via UDP to a syslog remote server
// - returns the number of bytes written to destbuffer (which should have
// destsize > 127)
function SyslogMessage(facility: TSyslogFacility; severity: TSyslogSeverity;
  const msg, procid, msgid: RawUtf8; destbuffer: PUtf8Char; destsize: PtrInt;
  trimmsgfromlog: boolean): PtrInt;

{$ifdef OSLINUX}
/// send a TSynLog formatted text to the systemd library
// - expected input text should alread be in "20200615 08003008 xxxx" format
// - as used e.g. during TSynLogFamily.EchoToConsoleUseJournal process
procedure SystemdEcho(Level: TSynLogLevel; const Text: RawUtf8);
{$endif OSLINUX}


implementation

{$ifdef FPC}
uses
  exeinfo; // cross-platform executable raw access for GDB DWARF support
{$endif FPC}


{ ************** Debug Symbols Processing from Delphi .map or FPC/GDB DWARF }

var
  ExeInstanceDebugFile: TDebugFile;

function GetInstanceDebugFile: TDebugFile;
begin
  result := ExeInstanceDebugFile;
  if (result <> nil) or
     SynLogFileFreeing then // avoid GPF
    exit;
  GlobalThreadLock.Lock;
  try
    if ExeInstanceDebugFile = nil then
      ExeInstanceDebugFile := TDebugFile.Create;
    result := ExeInstanceDebugFile;
  finally
    GlobalThreadLock.UnLock;
  end;
end;


{ TDebugFile }

{$ifdef FPC}

{  FPC can export DWARF/GDB info on POSIX and Windows from the project options.

   Our TDebugFile is able to export the function names and line numbers into
   an optimized .mab binary, e.g. for our regression tests:

  -rwxrwxr-x 1 ab ab  6 541 672 Jan 31 16:10 mormot2tests*
  -rwxrwxr-x 1 ab ab 13 352 584 Jan 31 16:10 mormot2tests.dbg*
  -rw-rw-r-- 1 ab ab    291 057 Jan 31 16:10 mormot2tests.mab

   For a 6MB executable, raw DWARF/GDB was 13MB but our .mab is only 290KB...
   Then this .mab file can be distributed along the executable, or just
   appended to it after build.

   Code below is inspired - but highly rewritten - from RTL's linfodwrf.pp }

type
  TDwarfLineInfoHeader64 = packed record
    magic: cardinal; // $ffffffff matching TDwarfLineInfoHeader32.unit_length
    unit_length: QWord;
    version: word;
    length: QWord;
    minimum_instruction_length: byte;
    default_is_stmt: ByteBool;
    line_base: ShortInt;
    line_range: byte;
    opcode_base: byte;
  end;

  TDwarfLineInfoHeader32 = packed record
    unit_length: cardinal;
    version: word;
    length: cardinal;
    minimum_instruction_length: byte;
    default_is_stmt: ByteBool;
    line_base: ShortInt;
    line_range: byte;
    opcode_base: byte;
  end;

  TDwarfDebugInfoHeader64 = packed record
    magic: cardinal;
    unit_length: QWord;
    version: word;
    debug_abbrev_offset: QWord;
    address_size: byte;
  end;

  TDwarfDebugInfoHeader32 = packed record
    unit_length: cardinal;
    version: word;
    debug_abbrev_offset: cardinal;
    address_size: byte;
  end;

  TDwarfDebugAbbrev = record
    Tag: QWord;
    Attrs: array of record
      attr, form: cardinal;
    end;
    AttrsCount: integer;
    Child: byte;
  end;

  TDwarfMachineState = object
  public
    address: cardinal;
    line: cardinal;
    fileid: cardinal;
    column: cardinal;
    isstmt: boolean;
    basicblock: boolean;
    endsequence: boolean;
    prologueend: boolean;
    epiloguebegin: boolean;
    appendrow: boolean;
    isa: cardinal;
    procedure Init(aIs_Stmt: ByteBool);
  end;

  TDwarfReader = object
  public
    read: TFastReader;
    DebugLineSectionOffset, DebugLineSection_Size, // debug_line section
    DebugInfoSectionOffset, DebugInfoSection_Size, // debug_info section
    DebugAbbrevSectionOffset, DebugAbbrevSectionSize: integer; // debug_abbrev
    Abbrev: array of TDwarfDebugAbbrev; // debug_abbrev content
    Lines: TInt64DynArray;              // store TDebugUnit.Addr[]/Line[]
    dirs, files: TRawUtf8DynArray;
    filesdir: TIntegerDynArray;
    isdwarf64, debugtoconsole: boolean;
    debug: TDebugFile;
    Map: TMemoryMap;
    function FindSections(const filename: ShortString): boolean;
    procedure ReadInit(aBase, aLimit: Int64);
    function ReadLeb128: Int64;
    function ReadAddress(addr_size: PtrInt): QWord; inline;
    procedure ReadString(var s: ShortString);
    function SkipString: PtrInt;
    procedure SkipAttr(form: QWord; const header64: TDwarfDebugInfoHeader64);
    procedure ReadAbbrevTable(file_offset, file_size: QWord);
    function ParseCompilationUnits(file_offset, file_size: QWord): QWord;
    function ParseCompilationFunctions(file_offset, file_size: QWord): QWord;
  end;

procedure TDwarfMachineState.Init(aIs_Stmt: ByteBool);
begin
  address := 0;
  fileid := 1;
  line := 1;
  column := 0;
  isstmt := aIs_Stmt;
  basicblock := false;
  endsequence := false;
  prologueend := false;
  epiloguebegin := false;
  isa := 0;
  appendrow := false;
end;

{$I-} // for debugtoconsole

function TDwarfReader.FindSections(const filename: ShortString): boolean;
var
  dbgfn: ShortString;
  e: TExeFile; // use RTL's cross-OS exeinfo.pp unit
begin
  result := false;
  // open exe filename or follow '.gnu_debuglink' redirection
  if not OpenExeFile(e, filename) then
  begin
    DisplayError('OpenExeFile failed on  %s', [filename]);
    exit;
  end;
  if ReadDebugLink(e, dbgfn) then // is there an external .dbg file?
  begin
    CloseExeFile(e);
    if not OpenExeFile(e, dbgfn) then
    begin
      DisplayError('OpenExeFile failed on  %s', [dbgfn]);
      exit;
    end;
  end
  else
    dbgfn := filename;
  // locate debug_* sections
  if FindExeSection(e, '.debug_line',
       DebugLineSectionOffset, DebugLineSection_size) and
     FindExeSection(e, '.debug_info',
       DebugInfoSectionOffset, DebugInfoSection_size) and
     FindExeSection(e, '.debug_abbrev',
       DebugAbbrevSectionOffset, DebugAbbrevSectionSize) then
    result := Map.Map(dbgfn);
  CloseExeFile(e);
end;

procedure TDwarfReader.ReadInit(aBase, aLimit: Int64);
begin
  if aBase + aLimit > Int64(Map.Size) then
    read.ErrorOverflow;
  read.Init(Map.Buffer + aBase, aLimit);
end;

function TDwarfReader.SkipString: PtrInt;
begin
  result := 0; // return length
  while read.NextByte <> 0 do
    inc(result);
end;

function TDwarfReader.ReadLeb128: Int64;
var
  shift: byte;
  data: PtrInt;
  val: Int64;
begin
  data := read.NextByte;
  if data <= 127 then
    // optimize the most common case of -64..+63 range
    exit((not ((data and (Int64(1) shl 6)) - 1)) or data);
  result := 0;
  shift := 0;
  repeat
    val := data and $7f;
    result := result or (val shl shift);
    inc(shift, 7);
    if data <= 127 then
      break;
    data := read.NextByte;
  until false;
  // extend sign from current shifted bits - do not match FromVarInt64 encoding
  result := (not ((result and (Int64(1) shl (shift - 1))) - 1)) or result;
end;

function TDwarfReader.ReadAddress(addr_size: PtrInt): QWord;
begin
  result := 0;
  read.Copy(@result, addr_size);
end;

procedure TDwarfReader.ReadString(var s: ShortString);
var
  c: AnsiChar;
begin
  s[0] := #0;
  while read.NextByteSafe(@c) and
        ({%H-}c <> #0) do
    AppendShortCharSafe(c, @s);
end;

procedure TDwarfReader.ReadAbbrevTable(file_offset, file_size: QWord);
var
  nr, attr, form: PtrInt;
  prev: TFastReader;
begin
  prev := read;
  ReadInit(file_offset, file_size);
  repeat
    nr := read.VarUInt64;
    if nr = 0 then
      break;
    if nr > high(Abbrev) then
      SetLength(Abbrev, nr + 256);
    with Abbrev[nr] do
    begin
      Tag := read.VarUInt64;
      Child := read.NextByte;
      AttrsCount := 0;
      repeat
        attr := read.VarUInt32;
        form := read.VarUInt32;
        if attr = 0 then
          break;
        if AttrsCount >= length(Attrs) then
          SetLength(Attrs, AttrsCount + 32);
        Attrs[AttrsCount].attr := attr;
        Attrs[AttrsCount].form := form;
        inc(AttrsCount);
      until false;
    end;
  until false;
  read := prev;
end;

function CalculateAddressIncrement(opcode: PtrInt;
  const header: TDwarfLineInfoHeader64): PtrInt; inline;
begin
  result := PtrInt(opcode - header.opcode_base) div header.line_range *
    header.minimum_instruction_length;
end;

// DWARF 2 default opcodes
const
  DW_LNS_LNE = 0; // see DW_LNE_*
  DW_LNS_COPY = 1;
  DW_LNS_ADVANCE_PC = 2;
  DW_LNS_ADVANCE_LINE = 3;
  DW_LNS_SET_FILE = 4;
  DW_LNS_SET_COLUMN = 5;
  DW_LNS_NEGATE_STMT = 6;
  DW_LNS_SET_BASIC_BLOCK = 7;
  DW_LNS_CONST_ADD_PC = 8;
  DW_LNS_FIXED_ADVANCE_PC = 9;
  DW_LNS_SET_PROLOGUE_END = 10;
  DW_LNS_SET_EPILOGUE_BEGIN = 11;
  DW_LNS_SET_ISA = 12;

  DW_LNE_END_SEQUENCE = 1;
  DW_LNE_SET_ADDRESS = 2;
  DW_LNE_DEFINE_FILE = 3;

  DW_TAG_class_type = 2;      // map object pascal class or object
  DW_TAG_structure_type = 19; // map object pascal record
  DW_TAG_subprogram = 46;     // map object function or method

  DW_AT_name = $3;
  DW_AT_low_pc = $11;
  DW_AT_high_pc = $12;

  DW_FORM_addr = $1;
  DW_FORM_block2 = $3;
  DW_FORM_block4 = $4;
  DW_FORM_data2 = $5;
  DW_FORM_data4 = $6;
  DW_FORM_data8 = $7;
  DW_FORM_string = $8;
  DW_FORM_block = $9;
  DW_FORM_block1 = $a;
  DW_FORM_data1 = $b;
  DW_FORM_flag = $c;
  DW_FORM_sdata = $d;
  DW_FORM_strp = $e;
  DW_FORM_udata = $f;
  DW_FORM_ref_addr = $10;
  DW_FORM_ref1 = $11;
  DW_FORM_ref2 = $12;
  DW_FORM_ref4 = $13;
  DW_FORM_ref8 = $14;
  DW_FORM_ref_udata = $15;
  DW_FORM_indirect = $16;
  DW_FORM_sec_offset = $17;
  DW_FORM_exprloc = $18;
  DW_FORM_flag_present = $19;

procedure TDwarfReader.SkipAttr(form: QWord;
  const header64: TDwarfDebugInfoHeader64);
begin
  case form of
    DW_FORM_addr:
      read.Next(header64.address_size);
    DW_FORM_block2:
      read.Next(read.Next2);
    DW_FORM_block4:
      read.Next(read.Next4);
    DW_FORM_data2:
      read.Next2;
    DW_FORM_data4:
      read.Next4;
    DW_FORM_data8:
      read.Next8;
    DW_FORM_string:
      SkipString;
    DW_FORM_block,
    DW_FORM_exprloc:
      read.Next(read.VarUInt64);
    DW_FORM_block1:
      read.Next(read.NextByte);
    DW_FORM_data1,
    DW_FORM_flag:
      read.NextByte;
    DW_FORM_sdata:
      read.VarNextInt;
    DW_FORM_ref_addr:
      if header64.version > 2 then
        if isdwarf64 then
          read.Next8
        else
          read.Next4
      else if header64.address_size < 4 then
        read.Next4
      else
        read.Next(header64.address_size);
    DW_FORM_strp,
    DW_FORM_sec_offset:
      if isdwarf64 then
        read.Next8
      else
        read.Next4;
    DW_FORM_udata:
      read.VarUInt64;
    DW_FORM_ref1:
      read.NextByte;
    DW_FORM_ref2:
      read.Next2;
    DW_FORM_ref4:
      read.Next4;
    DW_FORM_ref8:
      read.Next8;
    DW_FORM_ref_udata:
      read.VarUInt64;
    DW_FORM_indirect:
      SkipAttr(read.VarUInt64, header64);
    DW_FORM_flag_present:
      ; // none
  else
    DisplayError('Internal error: unknown dwarf form: %x', [form]);
  end;
end;

procedure FinalizeLines(u: PDebugUnit; linesn: PtrInt; Lines: PInt64; unsorted: boolean);
var
  i: PtrInt;
begin
  if (u = nil) or
     (linesn = 0) then
    exit;
  if unsorted then
  begin
    QuickSortInt64(pointer(Lines), 0, linesn - 1); // sort by Addr (high 40-bit)
    u^.Symbol.Start := Lines^ shr 24; // set to unit first function Addr
  end;
  SetLength(u^.Addr, linesn);
  SetLength(u^.Line, linesn);
  for i := 0 to linesn - 1 do
  begin
    u^.Line[i] := Lines^ and $ffffff; // low 24-bit
    u^.Addr[i] := Lines^ shr 24;      // high 40-bit
    inc(Lines);
  end;
end;

function TDwarfReader.ParseCompilationUnits(file_offset, file_size: QWord): QWord;
var
  opcode, opcodeext, opcodeadjust, divlinerange,
  prevaddr, prevfile, prevline: cardinal;
  unitlen: QWord;
  opcodeextlen, headerlen: PtrInt;
  dirsn, filedirsn, filesn, linesn: integer;
  state: TDwarfMachineState;
  c: ansichar;
  unsorted: boolean;
  header64: TDwarfLineInfoHeader64;
  header32: TDwarfLineInfoHeader32;
  u: PDebugUnit;
  s: ShortString;
  numoptable: array[1..255] of byte;
begin
  // check if DWARF 32-bit or 64-bit format
  ReadInit(file_offset, file_size);
  header32.unit_length := read.Next4;
  isdwarf64 := header32.unit_length = $ffffffff;
  if isdwarf64 then
    unitlen := read.Next8 + SizeOf(header64.magic) + SizeOf(header64.unit_length)
  else
    unitlen := header32.unit_length + SizeOf(header32.unit_length);
  result := file_offset + unitlen;
  // process debug_line header
  ReadInit(file_offset, unitlen);
  if header32.unit_length <> $ffffffff then
  begin
    read.Copy(@header32, SizeOf(header32));
    header64.magic := $ffffffff;
    header64.unit_length := header32.unit_length;
    header64.version := header32.version;
    header64.length := header32.length;
    header64.minimum_instruction_length := header32.minimum_instruction_length;
    header64.default_is_stmt := header32.default_is_stmt;
    header64.line_base := header32.line_base;
    header64.line_range := header32.line_range;
    header64.opcode_base := header32.opcode_base;
    headerlen := SizeOf(header32.version) + SizeOf(header32.unit_length) +
      SizeOf(header32.length) + header32.length;
  end
  else
  begin
    read.Copy(@header64, SizeOf(header64));
    headerlen := SizeOf(header64.magic) + SizeOf(header64.unit_length) +
      SizeOf(header64.version) + SizeOf(header64.length) + header64.length;
  end;
  // read opcode parameter count table
  FillcharFast(numoptable, SizeOf(numoptable), 0);
  read.Copy(@numoptable, header64.opcode_base - 1);
  // read directory and file names
  dirsn := 0;
  repeat
    ReadString(s);
    if s[0] = #0 then
      break;
    c := PathDelim;
    if Pos('/', s) > 0 then
      c := '/'
    else if Pos('\', s) > 0 then
      c := '\';
    if s[ord(s[0])] <> c then
      AppendShortCharSafe(c, @s);
    AddRawUtf8(dirs, dirsn, ShortStringToUtf8(s));
  until false;
  filesn := 0;
  filedirsn := 0;
  repeat
    ReadString(s);
    if s[0] = #0 then
      break;
    AddRawUtf8(files, filesn, ShortStringToUtf8(s));
    AddInteger(filesdir, filedirsn, read.VarUInt32);
    read.VarNextInt(2); // we ignore the attributes
  until false;
  // main decoding loop
  ReadInit(file_offset + headerlen, unitlen - headerlen);
  state.Init(header64.default_is_stmt);
  unsorted := false;
  linesn := 0;
  prevfile := 0;
  prevline := 0;
  prevaddr := 0;
  opcode := 0;
  u := nil;
  while read.NextByteSafe(@opcode) do
  begin
    case opcode of
      DW_LNS_LNE:
        begin
          // extended opcode
          opcodeextlen := read.VarUInt32;
          opcodeext := read.NextByte;
          case opcodeext of
            DW_LNE_END_SEQUENCE:
              begin
                state.endsequence := true;
                state.appendrow := true;
              end;
            DW_LNE_SET_ADDRESS:
              state.address := ReadAddress(opcodeextlen - 1);
            DW_LNE_DEFINE_FILE:
              begin
                SkipString;
                read.VarNextInt(3);
              end;
          else
            // Unknown extended opcode
            read.Next(opcodeextlen - 1);
          end;
        end;
      DW_LNS_COPY:
        begin
          state.basicblock := false;
          state.prologueend := false;
          state.epiloguebegin := false;
          state.appendrow := true;
        end;
      DW_LNS_ADVANCE_PC:
        inc(state.address, read.VarUInt32 * header64.minimum_instruction_length);
      DW_LNS_ADVANCE_LINE:
        // most of the time, to decrease state.line
        state.line := Int64(state.line) + ReadLeb128;
      DW_LNS_SET_FILE:
        state.fileid := read.VarUInt64;
      DW_LNS_SET_COLUMN:
        state.column := read.VarUInt64;
      DW_LNS_NEGATE_STMT:
        state.isstmt := not state.isstmt;
      DW_LNS_SET_BASIC_BLOCK:
        state.basicblock := true;
      DW_LNS_CONST_ADD_PC:
        inc(state.address, CalculateAddressIncrement(255, header64));
      DW_LNS_FIXED_ADVANCE_PC:
        inc(state.address, read.Next2);
      DW_LNS_SET_PROLOGUE_END:
        state.prologueend := true;
      DW_LNS_SET_EPILOGUE_BEGIN:
        state.epiloguebegin := true;
      DW_LNS_SET_ISA:
        state.isa := read.VarUInt64;
    else
      if opcode < header64.opcode_base then
        // skip unsupported standard opcode
        read.VarNextInt(numoptable[opcode])
      else
      begin
        // non-standard opcodes are in fact line (and address) adjustments
        opcodeadjust := opcode - header64.opcode_base;
        if opcodeadjust >= header64.line_range then
        begin
          divlinerange := opcodeadjust div header64.line_range;
          inc(state.address, divlinerange * header64.minimum_instruction_length);
          inc(state.line, header64.line_base +
            integer(opcodeadjust - divlinerange * header64.line_range));
        end
        else
          // FPC set line_range=255 and prefer explicit DW_LNS_ADVANCE_PC
          inc(state.line, header64.line_base + PtrInt(opcodeadjust));
        state.basicblock := false;
        state.prologueend := false;
        state.epiloguebegin := false;
        state.appendrow := true;
      end;
    end;
    if state.appendrow then
    begin
      state.appendrow := false;
      if state.isstmt and
         (state.line <> prevline) and
         (state.address <> 0) and
         (state.line > 1) then
      begin
        prevline := state.line;
        if prevfile <> state.fileid then
        begin
          // handle new unit / file
          FinalizeLines(u, linesn, pointer(Lines), unsorted);
          linesn := 0; // reuse the same 64-bit Lines[] buffer for Addr[]+Line[]
          prevaddr := 0;
          prevfile := state.fileid - 1;
          if debugtoconsole then
            writeln('-------------- ', files[prevfile]);
          u := debug.fUnits.NewPtr;
          u^.Symbol.Name := StringToAnsi7(GetFileNameWithoutExt(
            Ansi7ToString(files[prevfile])));
          if filesdir[prevfile] > 0 then
            u^.FileName := dirs[filesdir[prevfile] - 1];
          u^.FileName := u^.FileName + files[prevfile];
          u^.Symbol.Start := state.address;
          inc(prevfile);
        end;
        if state.address < prevaddr then
          // not increasing: need to sort u^.Addr[]+Line[] and u^.Symbol.Start
          unsorted := true;
        prevaddr := state.address;
        AddInt64(Lines, linesn, QWord(state.address) shl 24 + state.line);
        if debugtoconsole then
          writeln(files[state.fileid - 1], ' ', state.line, ' ',
            CardinalToHexShort(state.address));
      end;
      if state.endsequence then
        state.Init(header64.default_is_stmt);
    end;
  end;
  FinalizeLines(u, linesn, pointer(Lines), unsorted);
end;

function TDwarfReader.ParseCompilationFunctions(file_offset, file_size: QWord): QWord;
var
  s: ^TDebugSymbol;
  header64: TDwarfDebugInfoHeader64;
  header32: TDwarfDebugInfoHeader32;
  unit_length, low_pc, high_pc: QWord;
  abbr, level: cardinal;
  i: PtrInt;
  name, typname: ShortString;
begin
  // check if DWARF 32-bit or 64-bit format
  ReadInit(file_offset, file_size);
  header32.unit_length := read.Next4;
  isdwarf64 := header32.unit_length = $ffffffff;
  if isdwarf64 then
    unit_length := read.Next8 + SizeOf(header64.magic) + SizeOf(header64.unit_length)
  else
    unit_length := header32.unit_length + SizeOf(header32.unit_length);
  result := file_offset + unit_length;
  ReadInit(file_offset, unit_length);
  // process debug_info header
  if not isdwarf64 then
  begin
    read.Copy(@header32, SizeOf(header32));
    header64.magic := $ffffffff;
    header64.unit_length := header32.unit_length;
    header64.version := header32.version;
    header64.debug_abbrev_offset := header32.debug_abbrev_offset;
    header64.address_size := header32.address_size;
  end
  else
    read.Copy(@header64, SizeOf(header64));
  // read the debug_abbrev section corresponding to this debug_info section
  ReadAbbrevTable(DebugAbbrevSectionOffset + header64.debug_abbrev_offset,
    DebugAbbrevSectionSize);
  // main decoding loop
  level := 0;
  abbr := read.VarUInt32;
  typname[0] := #0;
  while abbr <> 0 do
  begin
    with Abbrev[abbr] do
    begin
      if Child <> 0 then
        inc(level);
      if Tag = DW_TAG_subprogram then
      begin
        low_pc := 1;
        high_pc := 0;
        name := '';
        for i := 0 to AttrsCount - 1 do
          with Attrs[i] do
          begin
            if (attr = DW_AT_low_pc) and
               (form = DW_FORM_addr) then
              low_pc := ReadAddress(header64.address_size)
            else if (attr = DW_AT_high_pc) and
                    (form = DW_FORM_addr) then
              high_pc := ReadAddress(header64.address_size)
            else if (attr = DW_AT_name) and
                    (form = DW_FORM_string) then
              ReadString(name)
            else
              SkipAttr(form, header64);
          end;
        if low_pc < high_pc then
        begin
          s := debug.fSymbols.NewPtr;
          if (typname[0] <> #0) and
             (typname[ord(typname[0])] <> '.') then
            AppendShortCharSafe('.', @typname);
          // DWARF2 symbols are emitted as UPPER by FPC -> lower for esthetics
          if header64.version < 3 then
            ShortStringToAnsi7String(lowercase(typname + name), s^.name);
          s^.Start := low_pc;
          s^.Stop := high_pc - 1;
          if debugtoconsole then
            writeln(s^.name, ' ', CardinalToHexShort(low_pc), '-',
              CardinalToHexShort(high_pc));
        end;
      end
      else if (level = 2) and
              ((Tag = DW_TAG_class_type) or
               (Tag = DW_TAG_structure_type)) then
      begin
        typname[0] := #0;
        for i := 0 to AttrsCount - 1 do
          with Attrs[i] do
            if (attr = DW_AT_name) and
               (form = DW_FORM_string) then
              ReadString(typname)
            else
              SkipAttr(form, header64);
      end
      else
        for i := 0 to AttrsCount - 1 do
          SkipAttr(Attrs[i].form, header64);
    end;
    if read.EOF then
      exit;
    abbr := read.VarUInt32;
    while (level > 0) and
          (abbr = 0) do
    begin
      if level = 1 then
        typname[0] := #0;
      // skip entries signaling that no more child entries are following
      dec(level);
      if read.EOF then
        exit;
      abbr := read.VarUInt64;
    end;
  end;
end;

{$I+}

function SymbolSortByAddr(const A, B): integer;
begin
  result := CompareInteger(TDebugSymbol(A).Start, TDebugSymbol(B).Start);
end;

procedure TDebugFile.GenerateFromMapOrDbg(aDebugToConsole: boolean);
var
  dwarf: TDwarfReader;
  current_offset, end_offset: QWord;
begin
  FillCharFast(dwarf, SizeOf(dwarf), 0);
  dwarf.debugtoconsole := aDebugToConsole;
  if dwarf.FindSections(fDebugFile) then
  try
    // retrieve units name and line numbers
    dwarf.debug := self;
    current_offset := dwarf.DebugLineSectionOffset;
    end_offset := current_offset + dwarf.DebugLineSection_Size;
    while current_offset < end_offset do
      current_offset := dwarf.ParseCompilationUnits(
        current_offset, end_offset - current_offset);
    fUnits.Sort(SymbolSortByAddr);
    // retrieve function names
    current_offset := dwarf.DebugInfoSectionOffset;
    end_offset := current_offset + dwarf.DebugInfoSection_Size;
    while current_offset < end_offset do
      current_offset := dwarf.ParseCompilationFunctions(current_offset,
        end_offset - current_offset);
    fSymbols.Sort(SymbolSortByAddr);
  finally
    dwarf.Map.UnMap;
  end;
end;

function BacktraceStrFpc(Addr: CodePointer): ShortString;
begin
  result := GetInstanceDebugFile.FindLocationShort(PtrUInt(Addr));
end;

class function TDebugFile.RegisterBacktraceStrFunc: boolean;
begin
  result := GetInstanceDebugFile.HasDebugInfo;
  if result then
    BacktraceStrFunc := BacktraceStrFpc; // use our fast version from now on
end;

{$else}


{  Delphi can export detailed .map info from the project options.

   Our TDebugFile is able to export the function names and line numbers into
   an optimized .mab binary, e.g. for our regression tests:

   31/01/2021  16:10    5 380 096 mormot2tests.exe
   31/01/2021  16:10      286 931 mormot2tests.mab
   31/01/2021  16:10    4 339 623 mormot2tests.map

   For a 5MB executable, .map text was 4MB but our .mab is only 280KB...
   Then this .mab file can be distributed along the executable, or just
   appended to it after build. }


function MatchPattern(P, PEnd, Up: PUtf8Char; var Dest: PUtf8Char): boolean;
begin
  result := false;
  repeat
    if P^ in [#1..' '] then
      repeat
        inc(P)
      until not (P^ in [#1..' ']);
    while NormToUpperAnsi7[P^] = Up^ do
    begin
      inc(P);
      if P > PEnd then
        exit;
      inc(Up);
      if (Up^ = ' ') and
         (P^ in [#1..' ']) then
      begin
        // ignore multiple spaces in P^
        while (P < PEnd) and
              (P^ in [#1..' ']) do
          inc(P);
        inc(Up);
      end;
    end;
    if Up^ = #0 then // all chars of Up^ found in P^
      break
    else if Up^ <> ' ' then // P^ and Up^ didn't match
      exit;
    inc(Up);
  until false;
  while (P < PEnd) and
        (P^ = ' ') do
    inc(P); // ignore all spaces
  result := true;
  Dest := P;
end;

procedure TDebugFile.GenerateFromMapOrDbg(aDebugToConsole: boolean);
var
  P, PEnd: PUtf8Char;
  sections: TDebugUnitDynArray;

  procedure NextLine;
  begin
    while (P < PEnd) and
          (P^ >= ' ') do
      inc(P);
    if (P < PEnd) and
       (P^ = #13) then
      inc(P);
    if (P < PEnd) and
       (P^ = #10) then
      inc(P);
  end;

  function GetCode(var Ptr: integer): boolean;
  begin
    while (P < PEnd) and
          (P^ = ' ') do
      inc(P);
    result := false;
    if (P + 10 < PEnd) and
       (PInteger(P)^ = // 0001:## = function, 0002:## = const, 0005:##=pdata..
         ord('0') + ord('0') shl 8 + ord('0') shl 16 + ord('1') shl 24) and
       (P[4] = ':') then
    begin
      if not HexDisplayToCardinal(PAnsiChar(P) + 5, PCardinal(@Ptr)^) then
        exit;
      while (P < PEnd) and
            (P^ > ' ') do
        inc(P);
      while (P < PEnd) and
            (P^ = ' ') do
        inc(P);
      if P < PEnd then
        result := true; // and P points to symbol name
    end;
  end;

  procedure ReadSegments;
  var
    Beg: PAnsiChar;
    U: TDebugUnit;
  begin
    NextLine;
    NextLine;
    while (P < PEnd) and
          (P^ < ' ') do
      inc(P);
    while (P + 10 < PEnd) and
          (P^ >= ' ') do
    begin
      // we just need the unit names now for ReadSymbols to detect and trim them
      // final Unit[] will be filled in ReadLines with potential nested files
      if GetCode(U.Symbol.Start) and
         HexDisplayToCardinal(PAnsiChar(P), PCardinal(@U.Symbol.Stop)^) then
      begin
        while PWord(P)^ <> ord('M') + ord('=') shl 8 do
          if P + 10 > PEnd then
            exit
          else
            inc(P);
        Beg := pointer(P + 2);
        while (P < PEnd) and
              (P^ > ' ') do
          inc(P);
        FastSetString(U.Symbol.Name, Beg, P - Beg);
        inc(U.Symbol.Stop, U.Symbol.Start - 1);
        if (U.Symbol.Name <> '') and
           ((U.Symbol.Start <> 0) or
            (U.Symbol.Stop <> 0)) then
          fUnits.FindAndAddIfNotExisting(U);
      end;
      NextLine;
    end;
  end;

  procedure ReadSymbols;
  var
    Beg: PUtf8Char;
    Sym: TDebugSymbol;
    {$ifdef ISDELPHI2005ANDUP}
    l, u: PtrInt;
    LastUnitUp: RawUtf8; // e.g. 'MORMOT.CORE.DATA.'
    {$endif ISDELPHI2005ANDUP}
  begin
    Sym.Stop := 0;
    NextLine;
    NextLine;
    while (P + 10 < PEnd) and
          (P^ >= ' ') do
    begin
      if GetCode(Sym.Start) then
      begin
        while (P < PEnd) and
              (P^ = ' ') do
          inc(P);
        Beg := pointer(P);
        while (P < PEnd) and
              (P^ > ' ') do
          inc(P);
        {$ifdef ISDELPHI2005ANDUP}
        // trim left 'UnitName.' for each symbol (since Delphi 2005)
        if (LastUnitUp <> '') and
           IdemPChar(Beg, pointer(LastUnitUp)) then
          // most common case since symbols are grouped address, i.e. by unit
          inc(Beg, length(LastUnitUp))
        else
        begin
          // manual unit name search
          LastUnitUp := '';
          for u := 0 to fUnitsCount - 1 do
            with fUnit[u].Symbol do
            begin
              l := length(Name);
              if (Beg[l] = '.') and
                 (l > length(LastUnitUp)) and
                 IdemPropNameU(Name, Beg, l) then
                LastUnitUp := UpperCase(Name); // find longest match
            end;
          if LastUnitUp <> '' then
          begin
            l := length(LastUnitUp);
            SetLength(LastUnitUp, l + 1);
            LastUnitUp[l] := '.';
            inc(Beg, l + 1);
          end;
        end;
        {$endif ISDELPHI2005ANDUP}
        FastSetString(Sym.Name, Beg, P - Beg);
        if (Sym.Name <> '') and
           not (Sym.Name[1] in ['$', '?']) then
          fSymbols.Add(Sym);
      end;
      NextLine;
    end;
    sections := fUnit;
    SetLength(sections, fUnitsCount);
    fUnits.Clear; // ReadLines will repopulate all units :)
  end;

  procedure ReadLines;
  var
    Beg: PAnsiChar;
    n, capa: PtrInt;
    aName: RawUtf8;
    U: PDebugUnit;
  begin
    Beg := pointer(P);
    while P^ <> '(' do
      if P = PEnd then
        exit
      else
        inc(P);
    FastSetString(aName, Beg, P - Beg);
    if aName = '' then
      exit;
    inc(P);
    Beg := pointer(P);
    while P^ <> ')' do
      if P = PEnd then
        exit
      else
        inc(P);
    if not IdemPChar(P, ') SEGMENT .TEXT') then
      exit;
    U := fUnits.NewPtr; // always recreate all units due to nested .inc
    U^.Symbol.Name := aName;
    FastSetString(U^.FileName, Beg, P - Beg);
    NextLine;
    NextLine;
    capa := 0;
    n := 0;
    while (P + 10 < PEnd) and
          (P^ >= ' ') do
    begin
      while (P < PEnd) and
            (P^ = ' ') do
        inc(P);
      repeat
        if n = capa then
        begin
          capa := NextGrow(capa);
          SetLength(U^.Line, capa);
          SetLength(U^.Addr, capa);
        end;
        U^.Line[n] := GetNextItemCardinal(P, ' ');
        if not GetCode(U^.Addr[n]) then
          break;
        if U^.Addr[n] <> 0 then
          inc(n); // occurred with Delphi 2010 :(
      until (P >= PEnd) or
            (P^ < ' ');
      NextLine;
    end;
    if n > 0 then
      U^.Symbol.Start := U^.Addr[0];
    SetLength(U^.Line, n);
    SetLength(U^.Addr, n);
  end;

var
  i, j, l: PtrInt;
  mapcontent: RawUtf8;
begin
  mapcontent := StringFromFile(fDebugFile);
  P := pointer(mapcontent);
  l := length(mapcontent);
  if (P = nil) or
     (StrLen(P) <> l) then
    exit; // this is no .map file for sure
  PEnd := P + l;
  // parse .map/.dbg sections into fSymbol[] and fUnit[]
  fSymbols.Capacity := 8000;
  while P < PEnd do
    if MatchPattern(P, PEnd, 'DETAILED MAP OF SEGMENTS', P) then
      ReadSegments
    else if MatchPattern(P, PEnd, 'ADDRESS PUBLICS BY VALUE', P) then
      ReadSymbols
    else if MatchPattern(P, PEnd, 'LINE NUMBERS FOR', P) then
      ReadLines
    else
      NextLine;
  // now we should have read all .map/.dbg content
  for i := fUnitsCount - 1 downto 0 do
    with fUnit[i] do
      if (Symbol.Start = 0) and
         (Symbol.Stop = 0) then
        fUnits.Delete(i); // occurs with Delphi 2010 :(
  for i := 0 to fUnitsCount - 1 do
    with fUnit[i] do
      if Symbol.Stop = 0 then
      begin
        if i < fUnitsCount - 1 then
          Symbol.Stop := fUnit[i + 1].Symbol.Start - 1;
        for j := 0 to length(sections) - 1 do
          if sections[j].Symbol.Name = Symbol.Name then
          begin
            if (Symbol.Stop = 0) or
               (sections[j].Symbol.Stop < Symbol.Stop) then
              Symbol.Stop := sections[j].Symbol.Stop;
            break;
          end;
      end;
  for i := 0 to fSymbolsCount - 2 do
    fSymbol[i].Stop := fSymbol[i + 1].Start - 1;
  if fSymbolsCount > 0 then
    with fSymbol[fSymbolsCount - 1] do
      Stop := Start + 64; // wild guess
end;

{$endif FPC}

const
  // .mab layout changed with mORMot 2 -> magic changed too
  MAGIC_MAB = $A5A5A55A;

procedure ReadSymbol(var R: TFastReader; var A: TDynArray);
var
  i, n, L: PtrInt;
  S: PDebugSymbol;
  prev: cardinal;
  P: PByte;
begin
  n := R.VarUInt32;
  A.Count := n;
  P := pointer(R.P);
  if (n = 0) or
     (P = nil) then
    exit;
  S := A.Value^;
  prev := 0;
  for i := 1 to n do
  begin
    inc(prev, FromVarUInt32(P));
    S^.Start := prev;
    inc(prev, FromVarUInt32(P));
    S^.Stop := prev;
    inc(PByte(S), A.Info.Cache.ItemSize); // may be TDebugSymbol or TDebugUnit
  end;
  S := A.Value^;
  for i := 1 to n do
  begin
    L := FromVarUInt32(P); // inlined R.Read(S^.Name)
    FastSetString(S^.Name, P, L);
    inc(P, L);
    inc(PByte(S), A.Info.Cache.ItemSize);
  end;
  R.P := pointer(P);
end;

function TDebugFile.LoadMab(const aMabFile: TFileName): boolean;
var
  R: TFastReader;
  i: PtrInt;
  MS: TMemoryStream;
  u: PDebugUnit;
begin
  result := false;
  fDebugFile := aMabFile;
  if FileExists(aMabFile) then
  try
    // StreamUnCompress() will try from the end if aMabFile is an executable
    MS := AlgoSynLZ.StreamUnCompress(aMabFile, MAGIC_MAB, {hash32=}true);
    if MS <> nil then
    try
      R.Init(MS.Memory, MS.Size);
      ReadSymbol(R, fSymbols);
      ReadSymbol(R, fUnits);
      for i := 0 to fUnitsCount - 1 do
        R.VarUtf8(fUnit[i].FileName);
      u := pointer(fUnit);
      for i := 1 to fUnitsCount do
      begin
        R.ReadVarUInt32Array(u^.Line);
        R.ReadVarUInt32Array(u^.Addr);
        inc(u);
      end;
      result := true;
    finally
      MS.Free;
    end;
  except
    on Exception do
      ; // invalid file -> ignore any problem
  end;
end;

constructor TDebugFile.Create(const aExeName: TFileName;
  MabCreate, DebugToConsole: boolean);
var
  i: PtrInt;
  ExeFile, MabFile: TFileName;
  MapAge, MabAge: TUnixTime;
begin
  inherited Create; // may have been overriden
  fSymbols.InitSpecific(TypeInfo(TDebugSymbolDynArray), fSymbol, ptRawUtf8,
    @fSymbolsCount, true);
  fUnits.InitSpecific(TypeInfo(TDebugUnitDynArray), fUnit, ptRawUtf8,
    @fUnitsCount, true);
  if SynLogFileFreeing then // avoid GPF
    exit;
  // search for an external .map/.dbg file matching the running .exe/.dll name
  if aExeName = '' then
  begin
    // guess the debug information source for the current process
    {$ifdef OSWINDOWS}
    ExeFile := GetModuleName(hInstance);
    {$ifdef FPC}
    fDebugFile := ExeFile;
    {$else}
    fCodeOffset := GetModuleHandle(pointer(ExtractFileName(ExeFile))) +
      $1000; // fixed .map offset on Delphi Windows
    fDebugFile := ChangeFileExt(ExeFile, '.map');
    {$endif FPC}
    {$else}
    ExeFile := Executable.InstanceFileName;
    fDebugFile := ExeFile; // exeinfo's ReadDebugLink() would redirect to .dbg
    {$endif OSWINDOWS}
  end
  else
    // supplied e.g. 'exec.map', 'exec.dbg' or even plain 'exec'/'exec.exe'
    fDebugFile := aExeName;
  MabFile := ChangeFileExt(ExpandFileName(fDebugFile), '.mab');
  if not FileExists(MabFile) then
    if not IsDirectoryWritable(ExtractFilePath(MabFile)) then
      // (do not include [idwExcludeWinSys] because if we can as admin then fine)
      // read/only exe folder -> store .mab in local non roaming user folder
      MabFile := GetSystemPath(spUserData) + ExtractFileName(Mabfile);
  GlobalThreadLock.Lock;
  try
    MapAge := FileAgeToUnixTimeUtc(fDebugFile);
    MabAge := FileAgeToUnixTimeUtc(MabFile);
    if (MapAge > 0) and
       (MabAge < MapAge) then
      // recompute from .map/.dbg if no faster-to-load .mab available
      try
        GenerateFromMapOrDbg(DebugToConsole);
        fSymbols.Capacity := fSymbolsCount; // only consume the needed memory
        fUnits.Capacity := fUnitsCount;
        for i := 0 to fUnitsCount - 2 do
          if fUnit[i].Symbol.Stop = 0 then
            fUnit[i].Symbol.Stop := fUnit[i + 1].Symbol.Start - 1;
        if fUnitsCount <> 0 then // wild guess of the last unit end of code
          with fUnit[fUnitsCount - 1] do
            if Symbol.Stop = 0 then
              if Addr <> nil then
                // units may overlap with .inc -> use Addr[]
                Symbol.Stop := Addr[high(Addr)] + 64
              else
                Symbol.Stop := Symbol.Start;
      except
        fSymbols.ClearSafe;
        fUnits.ClearSafe;
      end;
    // search for a .mab file matching the running .exe/.dll name
    if (fSymbolsCount = 0) and
       (MabAge <> 0) then
      MabCreate := not LoadMab(MabFile);
    // search for an embedded compressed .mab file appended to the .exe/.dll
    if fSymbolsCount = 0 then
      if aExeName = '' then
        MabCreate := not LoadMab(ExeFile)
      else
        MabCreate := not LoadMab(aExeName);
    // verify available symbols
    if fSymbolsCount > 0 then
    begin
      // in DWARF, fSymbol[i].Start/Stop sometimes overlap -> ignore on FPC
      // GenerateFromMapOrDbg() did Sort() by Start so we can guess its enough
      {$ifdef ISDELPHI}
      for i := 1 to fSymbolsCount - 1 do
        if fSymbol[i].Start <= fSymbol[i - 1].Stop then
        begin
          // on Delphi, there should be no overlap
          fUnits.ClearSafe;
          fSymbols.ClearSafe;
          exit;
        end;
      {$endif ISDELPHI}
      if MabCreate then // just created from .map/.dbg -> create .mab file
        SaveToFile(MabFile);
      fHasDebugInfo := true;
    end
    else
      fDebugFile := '';
  finally
    GlobalThreadLock.UnLock;
  end;
end;

procedure WriteSymbol(var W: TBufferWriter; const A: TDynArray);
var
  i, n: integer;
  prev: integer;
  S: PDebugSymbol;
  P, Beg: PByte;
  tmp: RawByteString;
begin
  n := A.Count;
  W.WriteVarUInt32(n);
  if n = 0 then
    exit;
  S := A.Value^;
  P := pointer(W.DirectWritePrepare(n * 10, tmp));
  Beg := P;
  prev := 0;
  for i := 1 to n do
  begin
    P := ToVarUInt32(S^.Start - prev, P);
    P := ToVarUInt32(S^.Stop - S^.Start, P);
    prev := S^.Stop;
    inc(PByte(S), A.Info.Cache.ItemSize); // may be TDebugSymbol or TDebugUnit
  end;
  W.DirectWriteFlush(PtrUInt(P) - PtrUInt(Beg), tmp);
  S := A.Value^;
  for i := 1 to n do
  begin
    W.Write(S^.Name); // group for better compression
    inc(PByte(S), A.Info.Cache.ItemSize);
  end;
end;

procedure TDebugFile.SaveToStream(aStream: TStream);
var
  W: TBufferWriter;
  i: PtrInt;
  MS: TMemoryStream;
  u: PDebugUnit;
begin
  MS := TMemoryStream.Create;
  W := TBufferWriter.Create(MS, 1 shl 20); // 1 MB should be enough at first
  try
    WriteSymbol(W, fSymbols);
    WriteSymbol(W, fUnits);
    for i := 0 to high(fUnit) do
      W.Write(fUnit[i].FileName); // group for better compression
    u := pointer(fUnit);
    for i := 1 to length(fUnit) do
    begin
      // Line values are not always increasing -> wkOffsetI
      W.WriteVarUInt32Array(u^.Line, length(u^.Line), wkOffsetI);
      // Addr are sorted, so always increasing -> wkOffsetU
      W.WriteVarUInt32Array(u^.Addr, length(u^.Addr), wkOffsetU);
      inc(u);
    end;
    W.Flush; // now MS contains the uncompressed binary data
    AlgoSynLZ.StreamCompress(MS, aStream, MAGIC_MAB, {hash32=}true, {trailer=}true);
  finally
    W.Free;
    MS.Free;
  end;
end;

const
  _TDebugSymbol = 'Name:RawUtf8 Start,Stop:integer';
  _TDebugUnit ='Symbol:TDebugSymbol FileName:RawUtf8 Line,Addr:TIntegerDynArray';

procedure TDebugFile.SaveToJson(W: TTextWriter);
begin
  if Rtti.RegisterType(TypeInfo(TDebugSymbol)).Props.Count = 0 then
    Rtti.RegisterFromText([TypeInfo(TDebugSymbol), _TDebugSymbol,
                           TypeInfo(TDebugUnit), _TDebugUnit]);
  W.AddShort('{"Symbols":');
  fSymbols.SaveToJson(W, []);
  W.AddShort(',"Units":');
  fUnits.SaveToJson(W, []);
  W.Add('}');
end;

procedure TDebugFile.SaveToJson(const aJsonFile: TFileName;
  aJsonFormat: TTextWriterJsonFormat);
var
  W: TJsonWriter;
  json: RawUtf8;
begin
  W := TJsonWriter.CreateOwnedStream(65536);
  try
    SaveToJson(W);
    W.SetText(json, aJsonFormat);
    FileFromString(json, aJsonFile);
  finally
    W.Free;
  end;
end;

function TDebugFile.SaveToFile(const aFileName: TFileName): TFileName;
var
  F: TStream;
begin
  if aFileName = '' then
    result := ChangeFileExt(GetModuleName(hInstance), '.mab')
  else
    result := aFileName;
  DeleteFile(result);
  F := TFileStreamEx.Create(result, fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

procedure TDebugFile.SaveToExe(const aExeName: TFileName);
var
  mabfilename: TFileName;
  exe, mab: TMemoryStream;
  exesize, mabsize: PtrUInt;
begin
  if not FileExists(aExeName) then
    exit;
  mabfilename := SaveToFile(ChangeFileExt(aExeName, '.mab'));
  try
    exe := TMemoryStream.Create;
    mab := TMemoryStream.Create;
    try
      // load both files
      mab.LoadFromFile(mabfilename);
      mabsize := mab.Size;
      exe.LoadFromFile(aExeName);
      exesize := exe.Size;
      if exesize < 16 then
        exit;
      // trim existing mab content
      exesize := AlgoSynLZ.StreamComputeLen(exe.Memory, exesize, MAGIC_MAB);
      exe.Size := exesize + mabsize;
      // append mab content to exe
      MoveFast(mab.Memory^, PAnsiChar(exe.Memory)[exesize], mabsize);
      exe.SaveToFile(aExeName);
    finally
      mab.Free;
      exe.Free;
    end;
  finally
    DeleteFile(mabfilename);
  end;
end;

function TDebugFile.FindSymbol(aAddressOffset: integer): PtrInt;
var
  L, R: PtrInt;
  s: PDebugSymbol;
begin
  R := length(fSymbol) - 1;
  L := 0;
  if (R >= 0) and
     (aAddressOffset >= fSymbol[0].Start) and
     (aAddressOffset <= fSymbol[R].Stop) then
    repeat
      result := (L + R) shr 1;
      s := @fSymbol[result];
      if aAddressOffset < s^.Start then
        R := result - 1
      else if aAddressOffset > s^.Stop then
        L := result + 1
      else
        exit; // found
    until L > R;
  result := -1;
end;

function TDebugFile.FindUnit(aAddressOffset: integer): PtrInt;
var
  L, R: PtrInt;
  s: PDebugSymbol;
begin
  R := length(fUnit) - 1;
  L := 0;
  if (R >= 0) and
     (aAddressOffset >= fUnit[0].Symbol.Start) and
     (aAddressOffset <= fUnit[R].Symbol.Stop) then
    repeat
      result := (L + R) shr 1;
      s := @fUnit[result].Symbol;
      if aAddressOffset < s^.Start then
        R := result - 1
      else if aAddressOffset > s^.Stop then
        L := result + 1
      else
        exit;
    until L > R;
  result := -1;
end;

function TDebugFile.FindUnit(aAddressOffset: integer;
  out LineNumber: integer): PtrInt;
var
  L, R, n, max: PtrInt;
  u: PDebugUnit;
begin
  LineNumber := 0;
  result := FindUnit(aAddressOffset);
  if result < 0 then
    exit;
  // unit found -> search line number
  u := @fUnit[result];
  if u^.Addr = nil then
    exit;
  max := length(u^.Addr) - 1;
  L := 0;
  R := max;
  if R >= 0 then
    repeat
      n := (L + R) shr 1;
      if aAddressOffset < u^.Addr[n] then
        R := n - 1
      else if (n < max) and
              (aAddressOffset >= u^.Addr[n + 1]) then
        L := n + 1
      else
      begin
        LineNumber := u^.Line[n];
        exit;
      end;
    until L > R;
end;

function TDebugFile.AbsoluteToOffset(aAddressAbsolute: PtrUInt): integer;
begin
  if (self = nil) or
     (aAddressAbsolute = 0) then
    result := 0
  else
    result := PtrInt(aAddressAbsolute) - PtrInt(fCodeOffset);
end;

function TDebugFile.IsCode(aAddressAbsolute: PtrUInt): boolean;
var
  offset: integer;
begin
  offset := AbsoluteToOffset(aAddressAbsolute);
  result := (offset <> 0) and
            HasDebugInfo and
            (((fUnit <> nil) and
              (offset >= fUnit[0].Symbol.Start) and
              (offset <= fUnit[length(fUnit) - 1].Symbol.Stop)) or
             ((fSymbol <> nil) and
              (offset >= fSymbol[0].Start) and
              (offset <= fSymbol[length(fSymbol) - 1].Stop)));
end;

class function TDebugFile.Log(W: TTextWriter; aAddressAbsolute: PtrUInt;
  AllowNotCodeAddr, SymbolNameNotFilename: boolean): boolean;
var
  u, s, Line, offset: integer;
  debug: TDebugFile;

  procedure AddHex;
  begin
    if not AllowNotCodeAddr then
      exit;
    W.AddPointer(aAddressAbsolute);
    W.AddDirect(' ');
  end;

begin
  result := false;
  if (W <> nil) and
     (aAddressAbsolute <> 0) then
  try
    debug := ExeInstanceDebugFile;
    if debug = nil then
      debug := GetInstanceDebugFile;
    if (debug = nil) or
       not debug.HasDebugInfo then
    begin
      AddHex;
      exit;
    end;
    offset := debug.AbsoluteToOffset(aAddressAbsolute);
    s := debug.FindSymbol(offset);
    u := debug.FindUnit(offset, Line);
    if (s < 0) and
       (u < 0) then
    begin
      AddHex;
      exit;
    end;
    {$ifdef ISDELPHI}
    if (s >= 0) and
       not AllowNotCodeAddr and
       (FindPropName(['SynRtlUnwind', '@HandleAnyException',  'LogExcept',
         '@HandleOnException', 'ThreadWrapper', 'ThreadProc'],
         debug.Symbols[s].Name) >= 0) then
      // no stack trace within the Delphi exception interception functions
      exit;
    {$endif ISDELPHI}
    AddHex;
    if u >= 0 then
    begin
      if SymbolNameNotFilename then
        W.AddString(debug.Units[u].Symbol.Name)
      else
        W.AddString(debug.Units[u].FileName);
      W.AddDirect(' ');
    end;
    if s >= 0 then
      W.AddString(debug.Symbols[s].Name);
    W.AddDirect(' ');
    if Line > 0 then
    begin
      W.AddDirect('(');
      W.AddU(Line);
      W.AddDirect(')', ' ');
    end;
    result := true;
  except
    result := false;
  end;
end;

function TDebugFile.FindLocation(aAddressAbsolute: PtrUInt): RawUtf8;
begin
  ShortStringToAnsi7String(FindLocationShort(aAddressAbsolute), result);
end;

function TDebugFile.FindLocationShort(aAddressAbsolute: PtrUInt): ShortString;
var
  u, s, line, offset: integer;
begin
  result := PointerToHexShort(pointer(aAddressAbsolute));
  if (self = nil) or
     (aAddressAbsolute = 0) or
     not HasDebugInfo then
    exit;
  offset := AbsoluteToOffset(aAddressAbsolute);
  s := FindSymbol(offset);
  u := FindUnit(offset, line);
  if (s < 0) and
     (u < 0) then
     exit;
  AppendShortChar(' ', @result);
  if u >= 0 then
  begin
    AppendShortAnsi7String(Units[u].FileName, result);
    AppendShortCharSafe(' ', @result);
  end
  else
    result[0] := #0;
  if s >= 0 then
    AppendShortAnsi7String(Symbols[s].Name, result);
  if line > 0 then
  begin
    AppendShortTwoChars(ord(' ') + ord('(') shl 8, @result);
    AppendShortCardinal(line, result);
    AppendShortCharSafe(')', @result);
  end;
end;

class function TDebugFile.FindLocation(exc: ESynException): RawUtf8;
begin
  if (exc = nil) or
     (exc.RaisedAt = nil) then
    result := ''
  else
    result := GetInstanceDebugFile.FindLocation(PtrUInt(exc.RaisedAt));
end;

function _GetExecutableLocation(aAddress: pointer): ShortString;
begin
  result := GetInstanceDebugFile.FindLocationShort(PtrUInt(aAddress));
end;

function TDebugFile.FindUnit(const aUnitName: RawUtf8): PtrInt;
begin
  if (self <> nil) and
     (aUnitName <> '') then
    for result := 0 to high(fUnit) do
      if IdemPropNameU(fUnit[result].Symbol.Name, aUnitName) then // inlined
        exit;
  result := -1;
end;

class function TDebugFile.FindFileName(const unitname: RawUtf8): TFileName;
var
  map: TDebugFile;
  name: RawUtf8;
  u: integer;
begin
  result := '';
  map := GetInstanceDebugFile;
  if map = nil then
    exit;
  if unitname = '' then
    name := Executable.ProgramName
  else
    name := unitname;
  u := map.FindUnit(name);
  if u >= 0 then
    Utf8ToFileName(map.fUnit[u].FileName, result);
end;


{ ************** Logging via TSynLogFamily, TSynLog, ISynLog }

var
  _LogInfoText:    array[TSynLogLevel] of RawUtf8;
  _LogInfoCaption: array[TSynLogLevel] of string;
  _LogAppText:     array[TAppLogLevel] of RawUtf8;

function ToText(event: TSynLogLevel): RawUtf8;
begin
  result := _LogInfoText[event];
end;

function ToText(events: TSynLogLevels): ShortString;
begin
  GetSetNameShort(TypeInfo(TSynLogLevels), events, result, {trimleft=}true);
end;

function ToCaption(event: TSynLogLevel): string;
begin
  result := _LogInfoCaption[event];
end;

function ToCaption(filter: TSynLogFilter): string;
begin
  result := GetCaptionFromEnum(TypeInfo(TSynLogFilter), Ord(filter))
end;

function ToText(const Event: TMethod): RawUtf8;
begin
  FormatUtf8('% using %(%)', [
    GetInstanceDebugFile.FindLocationShort(PtrUInt(Event.Code)),
    TObject(Event.Data), Event.Data], result);
end;

function ToText(apl: TAppLogLevel): RawUtf8;
begin
  result := _LogAppText[apl];
end;

function ToAppLogLevel(const Text: RawUtf8): TAppLogLevel;
begin
  if Text <> '' then
    case PCardinal(Text)^ and $dfdfdfdf of
      ord('C') + ord('R') shl 8 + ord('I') shl 16 + ord('T') shl 24:
        result := aplCritical;
      ord('E') + ord('R') shl 8 + ord('R') shl 16 + ord('O') shl 24:
        result := aplError;
      ord('W') + ord('A') shl 8 + ord('R') shl 16 + ord('N') shl 24:
        result := aplWarning;
      ord('I') + ord('N') shl 8 + ord('F') shl 16 + ord('O') shl 24:
        result := aplInfo;
      ord('D') + ord('E') shl 8 + ord('B') shl 16 + ord('U') shl 24:
        result := aplDebug;
    else if cardinal(PWord(Text)^) in [ord('1') .. ord('5')] then
      result := TAppLogLevel(PByte(Text)^ - ord('0'))
    else
      result := aplNone;
    end
  else
    result := aplNone;
end;

function FromAppLogLevel(const Text: RawUtf8): TSynLogLevels;
begin
  result := LOG_APP[ToAppLogLevel(Text)];
end;

{$ifdef FPC}
type
  THeapInfo = function: RawUtf8;

function RetrieveMemoryManagerInfo: RawUtf8;
begin
  {$ifdef CPUX64}
  // detect and include mormot.core.fpcx64mm raw information
  with GetHeapStatus do
    if PShortString(@TotalAddrSpace)^ = 'fpcx64mm' then // magic marker
    try
      result := StringReplaceAll(THeapInfo(PPointer(@Unused)^)(), '  ', ' ');
      exit;
    except
    end;
  {$endif CPUX64}
  // standard FPC memory manager
  with GetFPCHeapStatus do
    FormatUtf8(' - Heap: Current: used=% size=% free=%   Max: size=% used=%',
      [KBNoSpace(CurrHeapUsed), KBNoSpace(CurrHeapSize), KBNoSpace(CurrHeapFree),
       KBNoSpace(MaxHeapSize),  KBNoSpace(MaxHeapUsed)], result);
end;
{$else}
function RetrieveMemoryManagerInfo: RawUtf8;
begin
  // standard Delphi memory manager
  with GetHeapStatus do
    if TotalAddrSpace <> 0 then
      FormatUtf8(' - Heap: AddrSpace=% Uncommitted=% Committed=% Allocated=% '+
         'Free=% FreeSmall=% FreeBig=% Unused=% Overheap=% ',
        [KBNoSpace(TotalAddrSpace), KBNoSpace(TotalUncommitted),
         KBNoSpace(TotalCommitted), KBNoSpace(TotalAllocated),
         KBNoSpace(TotalFree),      KBNoSpace(FreeSmall),
         KBNoSpace(FreeBig),        KBNoSpace(Unused),
         KBNoSpace(Overhead)], result)
    else
      result := '';
end;
{$endif FPC}


var
  /// internal list of registered TSynLogFamily instances
  // - up to MAX_SYNLOGFAMILY TSynLog sub-classes may be defined
  // - protected by GlobalThreadLock
  SynLogFamily: array of TSynLogFamily;

  /// internal list of created TSynLog instances, one per each log file on disk
  // - also used by AutoFlushProc() to get a global list of TSynLog instances
  // - protected by GlobalThreadLock
  SynLogFile: TSynLogDynArray;


type
  // RRD of last 128 lines to be sent to console (no need of older data)
  TAutoFlushThreadToConsole = record
    Next, Count: integer;
    Text:  array[0..127] of RawUtf8; // must be a power-of-two length
    Color: array[0..127] of TConsoleColor;
  end;

  // cross-platform / cross-compiler TThread-based flush
  TAutoFlushThread = class(TThread)
  protected
    fToConsoleSafe: TLightLock; // topmost to ensure aarch64 alignment
    fEvent: TSynEvent;
    fToCompress: TFileName;
    fToConsole: TAutoFlushThreadToConsole;
    procedure Execute; override;
    procedure AddToConsole(const s: RawUtf8; c: TConsoleColor);
    procedure FlushConsole;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

var
  AutoFlushThread: TAutoFlushThread;

constructor TAutoFlushThread.Create;
begin
  fEvent := TSynEvent.Create;
  inherited Create(false);
end;

destructor TAutoFlushThread.Destroy;
begin
  inherited Destroy;
  fEvent.Free;
end;

procedure TAutoFlushThread.AddToConsole(const s: RawUtf8; c: TConsoleColor);
var
  i: PtrInt;
begin
  fToConsoleSafe.Lock;
  try
    with fToConsole do
    begin
      i := Next;
      Text[i] := s;
      Color[i] := c;
      Next := (i + 1) and high(Text); // simple round-robin data buffer
      inc(Count);
    end;
  finally
    fToConsoleSafe.UnLock;
  end;
end;

procedure TAutoFlushThread.FlushConsole;
var
  i: PtrInt;
  c: TAutoFlushThreadToConsole;
begin
  if fToConsole.Count = 0 then
    exit;
  fToConsoleSafe.Lock;
  try
    MoveFast(fToConsole, c, SizeOf(c)); // thread-safe local copy
    FillCharFast(fToConsole, SizeOf(fToConsole), 0); // copy with no refcount
  finally
    fToConsoleSafe.UnLock;
  end;
  if c.Count >= length(c.Text) then
  begin
    ConsoleWrite('... (truncated) ...', ccBlue);
    for i := c.Next to high(c.Text) do
      if c.Count <> 0 then
      begin
        ConsoleWrite(c.Text[i], c.Color[i]);
        dec(c.Count);
      end;
  end;
  for i := 0 to c.Next - 1 do
    if c.Count <> 0 then
    begin
      ConsoleWrite(c.Text[i], c.Color[i]);
      dec(c.Count);
    end;
  TextColor(ccLightGray);
end;

procedure TAutoFlushThread.Execute;
var
  i: PtrInt;
  tmp: TFileName;
  waitms, tix32, lasttix32: cardinal;
  log: TSynLog;
  files: TSynLogDynArray;
begin
  waitms := MilliSecsPerSec;
  lasttix32 := 0;
  repeat
    fEvent.WaitFor(waitms);
    if Terminated then
      break;
    try
      // 1. try background (SynLZ) compression after TSynLog.PerformRotation
      if fToCompress <> '' then
      begin
        tmp := fToCompress + '.tmp';
        RenameFile(fToCompress, tmp);
        LogCompressAlgo.FileCompress(tmp, fToCompress, LOG_MAGIC, true);
        DeleteFile(tmp);
        fToCompress := '';
        if Terminated then
          break;
      end;
      // 2. try background output to the console (by default on Windows)
      if fToConsole.Count <> 0 then
      begin
        FlushConsole;
        waitms := 111; // make the console a bit more reactive
      end
      else if waitms = 111 then
        waitms := 500;
      // 3. regularly flush (and maybe rotate) log content on disk
      tix32 := GetTickSec;
      if lasttix32 = tix32 then
        continue; // checking once per second is enough
      if Terminated or
         SynLogFileFreeing then
        break;
      GlobalThreadLock.Lock;
      try
        if Terminated or
           SynLogFileFreeing then
          break;
        files := copy(SynLogFile); // don't slow down main logging process
      finally
        GlobalThreadLock.UnLock;
      end;
      for i := 0 to high(files) do
      begin
        if Terminated or
           SynLogFileFreeing then // avoid GPF
          break;
        log := files[i];
        if (log.fFlushTix32 <> 0) and
           (tix32 >= log.fFlushTix32) and
           (log.fWriter <> nil) and
           (log.fWriter.PendingBytes > 1) then
          // write pending data after TSynLogFamily.AutoFlushTimeOut seconds
          log.Flush({forcediskwrite=}false); // may also set pendingRotate flag
      end;
      lasttix32 := tix32;
    except
      // on stability issue, start identifying this thread
      if not Terminated then
        try
          SetCurrentThreadName('TAutoFlushThread');
        except
          break;
        end;
    end;
  until Terminated;
  // Terminated is set: eventually display delayed console ouput
  try
    FlushConsole;
  except
    ; // ignore any exception at shutdown
  end;
end;

threadvar // do not publish for compilation within Delphi packages
  PerThreadInfo: TSynLogThreadInfo;

{$ifndef NOEXCEPTIONINTERCEPT}
// this is the main entry point for all intercepted exceptions
procedure SynLogException(const Ctxt: TSynLogExceptionContext); forward;

var
  // local cache containing TSynLogFamily with fHandleExceptions = true
  HandleExceptionFamily: TSynLogFamily;

function SearchHandleException(f: PPointer): TSynLogFamily;
var
  n: integer;
begin
  if f <> nil then
  begin
    n := PDALen(PAnsiChar(f) - _DALEN)^ + _DAOFF;
    repeat
      result := f^;
      if result.fHandleExceptions then // main log is the first
        exit;
      inc(f);
      dec(n);
    until n = 0;
  end;
  result := nil;
end;
{$endif NOEXCEPTIONINTERCEPT}

type
  TSynLogThreads = record
    Safe: TLightLock;       // topmost to ensure aarch64 alignment
    Name: TRawUtf8DynArray; // Name[ThreadNumber - 1] for ptIdentifiedInOneFile
    Count: integer;         // as returned by TSynLog.ThreadCount
    IndexReleasedCount: integer;
    IndexReleased: TWordDynArray; // reuse TSynLogThreadInfo.ThreadNumber
  end;
  PSynLogThreads = ^TSynLogThreads;

var
  // threads information shared by all TSynLog, protected by its own TLightLock
  SynLogThreads: TSynLogThreads;

procedure InitThreadNumber(nfo: PSynLogThreadInfo);
var
  thd: PSynLogThreads;
  num: cardinal; // in [1..MAX_SYNLOGTHREADS=65500] range
begin
  // compute the thread number - reusing any pre-existing closed thread number
  thd := @SynLogThreads;
  thd^.Safe.Lock;
  try
    if thd^.IndexReleasedCount <> 0 then // reuse NotifyThreadEnded() slot
    begin
      dec(thd^.IndexReleasedCount);
      num := thd^.IndexReleased[thd^.IndexReleasedCount];
    end
    else
    begin
      if thd^.Count >= MAX_SYNLOGTHREADS then
        ESynLogException.RaiseUtf8('Too many threads (%): ' +
          'check for missing TSynLog.NotifyThreadEnded', [thd^.Count]);
      inc(thd^.Count);    // new thread number
      num := thd^.Count;
    end;
  finally
    thd^.Safe.UnLock;
  end;
  nfo^.ThreadNumber := num;
  // pre-compute GetBitPtr() constants for SetThreadInfoAndThreadName()
  dec(num);
  nfo^.ThreadBitLo := 1 shl (num and 31); // 32-bit fThreadNameLogged[] value
  nfo^.ThreadBitHi := num shr 5;          // index in fThreadNameLogged[]
end;

function GetThreadInfo: PSynLogThreadInfo; {$ifdef HASINLINE} inline; {$endif}
begin
  result := @PerThreadInfo; // access the threadvar
  if result^.ThreadBitLo = 0 then
    InitThreadNumber(result); // initialized once per thread
end;


{ TSynLogFamily }

procedure TSynLogFamily.SetDestinationPath(const value: TFileName);
begin
  if value = '' then
    fDestinationPath := Executable.ProgramFilePath
  else
    fDestinationPath := IncludeTrailingPathDelimiter(value);
end;

procedure TSynLogFamily.SetLevel(aLevel: TSynLogLevels);
begin
  // ensure Leave has its matching Enter - but allow Enter without Leave
  if sllLeave in aLevel then
    include(aLevel, sllEnter);
  fLevel := aLevel;
  {$ifndef NOEXCEPTIONINTERCEPT}
  if SynLogNoExceptionIntercept then
    exit;
  // intercept exceptions, if necessary
  fHandleExceptions := (sllExceptionOS in aLevel) or
                       (sllException in aLevel);
  if fHandleExceptions then
  begin
    if HandleExceptionFamily = nil then
    begin
      HandleExceptionFamily := self;
      RawExceptionIntercept(SynLogException);
    end;
  end
  else if HandleExceptionFamily = self then // remove self and find next
    HandleExceptionFamily := SearchHandleException(pointer(SynLogFamily));
  {$endif NOEXCEPTIONINTERCEPT}
end;

procedure TSynLogFamily.SetEchoToConsole(aEnabled: TSynLogLevels);
begin
  if (self = nil) or
     (aEnabled = fEchoToConsole) then
    exit;
  fEchoToConsole := aEnabled;
end;

procedure TSynLogFamily.SetEchoToConsoleUseJournal(aValue: boolean);
begin
  if self <> nil then
    {$ifdef OSLINUX}
    if aValue and
       sd.IsAvailable then
      fEchoToConsoleUseJournal := true
    else
    {$endif OSLINUX}
      fEchoToConsoleUseJournal := false;
end;

function TSynLogFamily.GetSynLogClassName: string;
begin
  if (self = nil) or
     (fSynLogClass = nil) then
    result := ''
  else
    result := fSynLogClass.ClassName;
end;

constructor TSynLogFamily.Create(aSynLog: TSynLogClass);
begin
  fSynLogClass := aSynLog;
  if length(SynLogFamily) >= MAX_SYNLOGFAMILY then
    ESynLogException.RaiseUtf8('%.Create(%): too many classes', [self, aSynLog]);
  fIdent := ObjArrayAdd(SynLogFamily, self); // index of this TSynLogClass
  fDestinationPath := Executable.ProgramFilePath;
  // use .exe path by default - no [idwExcludeWinSys] needed here
  if not IsDirectoryWritable(fDestinationPath) then
    // fallback to a writable folder
    fDestinationPath := GetSystemPath(spLog);
  fDefaultExtension := '.log';
  fArchivePath := fDestinationPath;
  fArchiveAfterDays := 7;
  fRotateFileDailyAtHour := -1;
  fBufferSize := 8192;
  fStackTraceLevel := 30;
  fWithUnitName := true;
  fWithInstancePointer := true;
  {$ifdef OSWINDOWS}
  fEchoToConsoleBackground := true; // big speed-up on Windows
  {$endif OSWINDOWS}
  fExceptionIgnore := TSynList.Create;
  fPerThreadLog := ptIdentifiedInOneFile; // most convenient default layout
  fLevelStackTrace := [sllStackTrace, sllException, sllExceptionOS,
                       sllError, sllFail, sllLastError, sllDDDError];
  fLevelSysInfo := [sllException, sllExceptionOS, sllLastError, sllNewRun];
end;

function TSynLogFamily.GetCurrentThreadFlag(ti: TSynLogThreadInfoFlag): boolean;
begin
  result := ti in PerThreadInfo.Flags; // private threadvar access
end;

procedure TSynLogFamily.SetCurrentThreadFlag(ti: TSynLogThreadInfoFlag;
  value: boolean);
var
  flags: ^TSynLogThreadInfoFlags;
begin
  flags := @PerThreadInfo.Flags; // no need of GetThreadInfo/InitThreadNumber
  if value then
    include(flags^, ti)
  else
    exclude(flags^, ti);
end;

function TSynLogFamily.CreateSynLog: TSynLog;
begin
  result := nil;
  if SynLogFileFreeing then
    exit; // avoid GPF
  GlobalThreadLock.Lock;
  try
    result := fSynLogClass.Create(self);
    ObjArrayAdd(SynLogFile, result);
    if fPerThreadLog = ptOneFilePerThread then
      if (fRotateFileCount = 0) and
         (fRotateFileSizeKB = 0) and
         (fRotateFileDailyAtHour < 0) then
        PerThreadInfo.FileLookup[fIdent] := result // store TSynLog in threadvar
      else
      begin
        fPerThreadLog := ptIdentifiedInOneFile; // rotation requires one file
        fGlobalLog := result;
      end
    else
      fGlobalLog := result;
  finally
    GlobalThreadLock.UnLock;
  end;
end;

procedure TSynLogFamily.EnsureAutoFlushThreadRunning;
begin
  if (AutoFlushThread = nil) and
     not SynLogFileFreeing and
     (fAutoFlushTimeOut <> 0)
     {$ifdef ISDELPHI} and (DebugHook = 0) {$endif} then
    AutoFlushThread := TAutoFlushThread.Create;
end;

function TSynLogFamily.ArchiveAndDeleteFile(const aFileName: TFileName): boolean;
var
  age: TDateTime;
  dest: TFileName;
begin
  result := false;
  age := FileAgeToDateTime(aFileName);
  if age = 0 then
    exit; // not found
  if Assigned(OnArchive) then
  begin
    // we can ignore ArchiveAfterDays because the file is about to be deleted
    dest := GetArchiveDestPath(age);
    if dest <> '' then // the archive folder has been created
    try
      result := OnArchive(aFileName, dest); // archive and delete
    finally
      OnArchive('', dest); // always eventually close .zip
    end;
  end
  else
    result := DeleteFile(aFileName);
end;

function TSynLogFamily.GetArchiveDestPath(age: TDateTime): TFileName;
var
  dt: TSynSystemTime;
  tmp: string[7];
begin
  // returns 'ArchivePath\log\YYYYMM\'
  result := EnsureDirectoryExists([ArchivePath, 'log']);
  if result = '' then
    exit; // impossible to create the archive folder
  dt.FromDate(age); // faster than RTL DecodeDate()
  tmp[0] := #6;
  YearToPChar(dt.Year, @tmp[1]);
  PWord(@tmp[5])^ := TwoDigitLookupW[dt.Month];
  result := MakePath([result, tmp], {enddelim=}true);
end;

destructor TSynLogFamily.Destroy;
begin
  {$ifndef NOEXCEPTIONINTERCEPT}
  if HandleExceptionFamily = self then
    HandleExceptionFamily := nil;
  {$endif NOEXCEPTIONINTERCEPT}
  fDestroying := true;
  EchoRemoteStop;
  ExceptionIgnore.Free;
  inherited Destroy;
  fGlobalLog := nil; // paranoid
end;

procedure TSynLogFamily.ArchiveOldFiles(
  sourcePath, destPath: TFileName; archiveDays: integer);
var
  sr: TSearchRec;
  srName: TFileName;
  srTime, triggerTime: TDateTime;
begin
  if not Assigned(OnArchive) then
    exit;
  if sourcePath = '' then
    sourcePath := fDestinationPath;
  // search for logs older than ArchiveAfterDays to trigger OnArchive()
  if FindFirst(sourcePath + '*' + fDefaultExtension,
       faAnyFile - faDirectory, sr) = 0 then
  try
    triggerTime := 0;
    if archiveDays < 0 then
      archiveDays := ArchiveAfterDays;
    if archiveDays > 0 then
      triggerTime := NowUtc - archiveDays;
    repeat
      if not SearchRecValidFile(sr) then
        continue;
      srTime := SearchRecToDateTimeUtc(sr);
      if (srTime = 0) or
         (srTime > triggerTime) then
        continue;
      if srName = '' then
        if destPath = '' then
          destPath := GetArchiveDestPath(srTime)
        else
          destPath := EnsureDirectoryExists(destPath);
      srName := sourcePath + sr.Name;
      if sr.Size = 0 then
      begin
        DeleteFile(srName); // nothing to archive
        continue;
      end;
      if destPath = '' then
        break; // impossible to create the archive folder
      OnArchive(srName, destPath); // archive and delete
    until FindNext(sr) <> 0;
  finally
    FindClose(sr);
    if srName <> '' then     // if OnArchive() was called
      OnArchive('', destPath); // always eventually close .zip
  end;
end;

function TSynLogFamily.Add: TSynLog;
begin
  result := nil;
  if self = nil then
    exit;
  result := fGlobalLog;  // <>nil for ptMergedInOneFile/ptIdentifiedInOneFile
  if result = nil then
    result := GetLog; // call sub-proc for ptOneFilePerThread or once at startup
end;

function TSynLogFamily.GetLog: TSynLog;
begin
  if self <> nil then
  begin
    result := fGlobalLog;
    if result <> nil then
      // ptMergedInOneFile and ptIdentifiedInOneFile (most common case)
      exit;
    if (fPerThreadLog = ptOneFilePerThread) and
       (fRotateFileCount = 0) and
       (fRotateFileSizeKB = 0) and
       (fRotateFileDailyAtHour < 0) then
    begin
      // unrotated ptOneFilePerThread
      result := PerThreadInfo.FileLookup[fIdent];
      if result = nil then
        result := CreateSynLog;
    end
    else
      // new ptMergedInOneFile or ptIdentifiedInOneFile
      result := CreateSynLog;
  end
  else
    result := nil;
end;

procedure TSynLogFamily.SynLogFileListEcho(const aEvent: TOnTextWriterEcho;
  aEventAdd: boolean);
var
  i: PtrInt;
begin
  if (self = nil) or
     SynLogFileFreeing or
     (SynLogFile = nil) or
     (not Assigned(aEvent)) then
    exit;
  GlobalThreadLock.Lock;
  try
    for i := 0 to high(SynLogFile) do
      with SynLogFile[i] do
        if fFamily = self then
          if aEventAdd then
            fWriterEcho.EchoAdd(aEvent)
          else
            fWriterEcho.EchoRemove(aEvent);
  finally
    GlobalThreadLock.UnLock;
  end;
end;

procedure TSynLogFamily.SetEchoCustom(const aEvent: TOnTextWriterEcho);
begin
  if self = nil then
    exit;
  SynLogFileListEcho(fEchoCustom, {add=}false); // unsubscribe any previous
  fEchoCustom := aEvent;
  SynLogFileListEcho(aEvent, {add=}true); // subscribe new
end;

procedure TSynLogFamily.EchoRemoteStart(aClient: TObject;
  const aClientEvent: TOnTextWriterEcho; aClientOwnedByFamily: boolean);
begin
  EchoRemoteStop;
  fEchoRemoteClient := aClient;
  fEchoRemoteEvent := aClientEvent;
  fEchoRemoteClientOwned := aClientOwnedByFamily;
  SynLogFileListEcho(fEchoRemoteEvent, {add=}true); // subscribe
end;

procedure TSynLogFamily.EchoRemoteStop;
begin
  if fEchoRemoteClient = nil then
    exit;
  if fEchoRemoteClientOwned then
  try
    try
      fEchoRemoteEvent(nil, sllClient,
        FormatUtf8('%00%    Remote Client % Disconnected',
          [NowToString(false), LOG_LEVEL_TEXT[sllClient], self]));
    finally
      fEchoRemoteClient.Free;
    end;
  except
    on Exception do
      ;
  end;
  fEchoRemoteClient := nil;
  SynLogFileListEcho(fEchoRemoteEvent, {add=}false); // unsubscribe
  fEchoRemoteEvent := nil;
end;

function TSynLogFamily.GetExistingLog(MaximumKB: cardinal): RawUtf8;
const
  // a 128 MB RawUtf8 seems fair enough
  MAXPREVIOUSCONTENTSIZE = 128 shl 20;
var
  log: TSynLog;
  endpos, start: Int64;
  c: AnsiChar;
  i, len, read, total: integer;
  P: PAnsiChar;
begin
  result := '';
  if (SynLogFile = nil) or
     SynLogFileFreeing then
    exit;
  GlobalThreadLock.Lock;
  try
    for i := 0 to high(SynLogFile) do
    begin
      log := SynLogFile[i];
      if log.fFamily <> self then
        continue;
      log.Writer.FlushToStream;
      endpos := log.Writer.Stream.Position;
      try
        if endpos > MAXPREVIOUSCONTENTSIZE then
          len := MAXPREVIOUSCONTENTSIZE
        else
          len := MaximumKB shl 10;
        start := log.fStreamPositionAfterHeader;
        if (len <> 0) and
           (endpos - start > len) then
        begin
          start := endpos - len;
          log.Writer.Stream.Position := start;
          repeat
            inc(start)
          until (log.Writer.Stream.Read(c, 1) = 0) or
                (ord(c) in [10, 13]);
        end
        else
          log.Writer.Stream.Position := start;
        len := endpos - start;
        SetLength(result, len);
        P := pointer(result);
        total := 0;
        repeat
          read := log.Writer.Stream.Read(P^, len);
          if read <= 0 then
          begin
            if total <> len then
              FakeLength(result, total); // truncate on read error (paranoid)
            break;
          end;
          inc(P, read);
          dec(len, read);
          inc(total, read);
        until len = 0;
      finally
        log.Writer.Stream.Position := endpos;
      end;
      break;
    end;
  finally
    GlobalThreadLock.UnLock;
  end;
end;

procedure TSynLogFamily.OnThreadEnded(Sender: TThread);
begin
  TSynLog.NotifyThreadEnded;
end;


{ TSynLog }

class function TSynLog.Family: TSynLogFamily;
begin
  result := pointer(Self);
  if result = nil then
    exit;
  // inlined Rtti.Find(ClassType)
  {$ifdef NOPATCHVMT}
  result := LastFamily;
  if (result <> nil) and
     (result.SynLogClass = self) then
    exit; // most common case
  result := pointer(Rtti.FindType(PPointer(PAnsiChar(self) + vmtTypeInfo)^));
  {$else}
  result := PPointer(PAnsiChar(result) + vmtAutoTable)^;
  {$endif NOPATCHVMT}
  if result <> nil then
    // we know TRttiCustom is in the slot, and PrivateSlot as TSynLogFamily
    result := TRttiCustom(pointer(result)).PrivateSlot;
  if result = nil then
    // register the TSynLogFamily to the TRttiCustom.PrivateSlot field
    result := FamilyCreate
  {$ifdef NOPATCHVMT}
  else
    LastFamily := result;
  {$endif NOPATCHVMT}
end;

class function TSynLog.HasLevel(levels: TSynLogLevels): boolean;
begin
  result := (self <> nil) and
            (levels * Family.Level <> []);
end;

class function TSynLog.Add: TSynLog;
var
  lf: TSynLogFamily;
begin
  // inlined TSynLog.Family with direct fGlobalLog check and no FamilyCreate
  result := nil;
  if self = nil then
    exit;
  {$ifdef NOPATCHVMT}
  lf := LastFamily;
  if (lf = nil) or
     (lf.SynLogClass <> self) then
  begin
    lf := pointer(Rtti.FindType(PPointer(PAnsiChar(self) + vmtTypeInfo)^));
  {$else}
    lf := PPointer(PAnsiChar(self) + vmtAutoTable)^;
  {$endif NOPATCHVMT}
    if lf = nil then
      exit;
    // we know TRttiCustom is in the slot, and Private is TSynLogFamily
    lf := TRttiCustom(pointer(lf)).PrivateSlot;
    if lf = nil then
      exit; // FamilyCreate should have been called
  {$ifdef NOPATCHVMT}
    LastFamily := lf;
  end;
  {$endif NOPATCHVMT}
  // if we reached here, lf points to the expected TSynLogFamily
  result := lf.fGlobalLog;
  // <>nil for ptMergedInOneFile and ptIdentifiedInOneFile (most common case)
  if result = nil then
    result := lf.GetLog; // ptOneFilePerThread or at startup
end;

class function TSynLog.FamilyCreate: TSynLogFamily;
var
  rtticustom: TRttiCustom;
  {$ifndef NOPATCHVMT}
  vmt: TObject;
  {$endif NOPATCHVMT}
begin
  // private sub function called from inlined TSynLog.Family / TSynLog.Add
  if (self <> nil) and
     InheritsFrom(TSynLog) then // paranoid
  begin
    rtticustom := Rtti.RegisterClass(self);
    {$ifndef NOPATCHVMT}
    vmt := PPointer(PAnsiChar(self) + vmtAutoTable)^;
    if (rtticustom = nil) or
       (vmt <> rtticustom) then
      // TSynLog.Family / TSynLog.Add expect TRttiCustom in the first slot
      ESynLogException.RaiseUtf8(
        '%.FamilyCreate: vmtAutoTable=% not %', [self, vmt, rtticustom]);
    {$endif NOPATCHVMT}
    Rtti.RegisterSafe.Lock;
    try
      result := rtticustom.PrivateSlot;
      if Assigned(result) then
        if result.InheritsFrom(TSynLogFamily) then
          // registered by a background thread
          exit
        else
          ESynLogException.RaiseUtf8( // paranoid
            '%.FamilyCreate: PrivateSlot=%', [self, result]);
      // create the TSynLogFamily instance associated with this TSynLog class
      result := TSynLogFamily.Create(self); // stored in SynLogFamily[]
      rtticustom.PrivateSlot := result; // will be owned by this TRttiCustom
    finally
      Rtti.RegisterSafe.UnLock;
    end;
  end
  else
    result := nil;
end;

procedure TSynLog.LogHeader(const Level: TSynLogLevel; Instance: TObject);
var
  indent: PtrInt;
  P: PUtf8Char;
begin
  fWriter.AddShort(fThreadInfo^.CurrentTimeAndThread); // timestamp [+threadnum]
  P := fWriter.B + 1; // AddShort() reserved for 255 bytes
  PInt64(P)^ := PInt64(@LOG_LEVEL_TEXT[Level][1])^;
  inc(P, 7);
  indent := fThreadInfo^.RecursionCount;
  if Level = sllEnter then
    dec(indent);
  if indent > 0 then // ident <= MAX_SYNLOGRECURSION = 53 clearly within 255 bytes
  begin
    FillCharFast(P^, indent, 9); // inlined AddChars(#9, indent)
    inc(P, indent);
  end;
  if Instance <> nil then
  begin
    P := PointerToText(Instance, P, fFamily.WithUnitName, fFamily.WithInstancePointer);
    P^ := ' ';
  end
  else
    dec(P);
  fWriter.B := P;
  if Level = sllMemory then // handle additional information
    AddMemoryStats;
end;

procedure LogHeaderNoRecursion(WR: TJsonWriter; const Level: TSynLogLevel;
  TimeStampAndThreadNum: PShortString);
  {$ifdef HASINLINE} inline; {$endif}
begin
  WR.AddShort(TimeStampAndThreadNum^); // timestamp [+ threadnumber]
  PInt64(WR.B + 1)^ := PInt64(@LOG_LEVEL_TEXT[Level][1])^;
  inc(WR.B, 7); // include no recursive indentation nor any Instance
end;

procedure TSynLog.LogTrailer(Level: TSynLogLevel);
begin
  if Level in fFamily.fLevelStackTrace then
    AddStackTrace(nil);
  if Level in fFamily.fLevelSysInfo then
    AddSysInfo;
  fWriterEcho.AddEndOfLine(Level); // AddCR + any per-line echo suport
end;

procedure InternalSetCurrentThreadName(const Name: RawUtf8);
var
  ndx: PtrInt;
  thd: PSynLogThreads;
begin
  if SynLogFileFreeing then
    exit; // avoid GPF
  ndx := PtrInt(GetThreadInfo^.ThreadNumber) - 1; // may call InitThreadNumber()
  if ndx < 0 then
    exit; // paranoid
  thd := @SynLogThreads;
  thd^.Safe.Lock;
  try
    if ndx >= length(thd^.Name) then
      SetLength(thd^.Name, NextGrow(ndx + 32));
    thd^.Name[ndx] := Name;
  finally
    thd^.Safe.UnLock;
  end;
end;

class function TSynLog.ThreadIndex: PtrInt;
begin
  result := PtrInt(GetThreadInfo^.ThreadNumber) - 1;
  // warning: caller should ensure TSynLog.NotifyThreadEnded proper call
end;

class procedure TSynLog.NotifyThreadEnded;
var
  s: PShortString;
  nfo: PSynLogThreadInfo;
  thd: PSynLogThreads;
  num, i: PtrInt;
begin
  s := CurrentThreadNameShort;
  if s^[0] <> #0 then // avoid GPF if returned @NULCHAR
    s^[0] := #0; // reset TShort31 threadvar for consistency
  nfo := @PerThreadInfo; // no automatic InitThreadNumber()
  num := nfo^.ThreadNumber;
  if num = 0 then // not touched yet by TSynLog, or called twice
    exit;
  nfo^.ThreadBitLo := 0; // force InitThreadNumber on next thread access
  // reset global thread information
  if SynLogFileFreeing then
    exit; // inconsistent call at shutdown
  thd := @SynLogThreads;
  thd^.Safe.Lock;
  try
    // reset this thread name for ptIdentifiedInOneFile
    if num <= length(thd^.Name) then
      thd^.Name[num - 1] := '';
    // mark thread number to be recycled by InitThreadNumber
    AddWord(thd^.IndexReleased, thd^.IndexReleasedCount, num);
  finally
    thd^.Safe.UnLock;
  end;
  // reset this thread naming flag in each TSynLog
  dec(num);
  for i := 0 to length(SynLogFamily) - 1 do
    with SynLogFamily[i] do
      if (sllInfo in Level) and
         (PerThreadLog = ptIdentifiedInOneFile) and
         (fGlobalLog <> nil) and
         (num < (length(fGlobalLog.fThreadNameLogged) shl 5)) then
        UnSetBitPtr(fGlobalLog.fThreadNameLogged, num);
end;

function TSynLog.GetThreadCount: integer;
begin
  result := SynLogThreads.Count; // global counter for the process
end;

procedure TSynLog.AddLogThreadName; // once from SetThreadInfoAndThreadName()
var
  ndx, thdid: PtrInt;
begin
  // update fThreadNameLogged[] to ensure this method is called once per thread
  ndx := fThreadInfo.ThreadNumber - 1;
  if ndx < 0 then
    exit; // paranoid
  if ndx >= length(fThreadNameLogged) shl 5 then     // 32-bit array
    SetLength(fThreadNameLogged, (ndx shr 5)  + 32); // alloc per 1K threads
  SetBitPtr(fThreadNameLogged, ndx);
  // add the "SetThreadName" sllInfo line in the expected format
  // see TSynLogFile.ProcessOneLine() for the expected format
  LogHeaderNoRecursion(fWriter, sllInfo, @fThreadInfo^.CurrentTimeAndThread);
  fWriter.AddShort('SetThreadName ');
  fWriter.AddU(ndx + 1);  // human-friendly LogViewer number for this process
  fWriter.AddDirect(' ');
  thdid := PtrUInt(GetCurrentThreadId);
  fWriter.AddPointer(thdid);  // as hexadecimal (pthread pointer on POSIX)
  {$ifdef OSWINDOWS}
  fWriter.AddDirect(' ');
  fWriter.AddU(thdid);        // as decimal DWORD on Windows
  {$endif OSWINDOWS}
  fWriter.AddDirect('=');        // as expected by TSynLogFile.ThreadName()
  fWriter.AddOnSameLine(pointer(GetCurrentThreadName)); // human-readable text
  fWriterEcho.AddEndOfLine(sllInfo);
end;

procedure SetThreadInfoAndThreadName(log: TSynLog; nfo: PSynLogThreadInfo);
  {$ifdef HASINLINE} inline; {$endif}
var
  p: PIntegerArray;
  ndx: PtrUInt;
begin // caller just made GlobalThreadLock.Lock
  log.fThreadInfo := nfo;
  // quickly check if we need to rotate or write the "SetThreadName" line
  if pendingRotate in log.fPendingFlags then   // from OnFlushToStream
    log.PerformRotation(nfo);
  if not (logAddThreadName in log.fFlags) then 
    exit; // no sllInfo + ptIdentifiedInOneFile
  p := pointer(log.fThreadNameLogged); // threads bit-set of this TSynLog
  if p <> nil then
  begin
    ndx := nfo^.ThreadBitHi; // use pre-computed runtime constants
    if ndx <= PtrUInt(PDALen(PAnsiChar(p) - _DALEN)^ + (_DAOFF - 1)) then
      if p[ndx] and nfo^.ThreadBitLo <> 0 then // fast "if GetBitPtr() then"
        exit; // already done (most common case)
  end;
  // we need to append the "SetThreadName" line
  log.AddLogThreadName;
end;

function TSynLog.LockAndDisableExceptions: boolean;
var
  nfo: PSynLogThreadInfo;
begin
  nfo := @PerThreadInfo; // access the threadvar
  if not (tiTemporaryDisable in nfo^.Flags) then
  begin
    if nfo^.ThreadBitLo = 0 then
      InitThreadNumber(nfo); // first access - inlined GetThreadInfo
    if not (logInitDone in fFlags) then
      LogFileInit(nfo); // run once, to set start time and write headers
    FillInfo(nfo, nil); // syscall outside of GlobalThreadLock
    GlobalThreadLock.Lock;
    SetThreadInfoAndThreadName(self, nfo);
    {$ifndef NOEXCEPTIONINTERCEPT}
    // any exception within logging process will be ignored from now on
    fThreadInfoBackup := nfo^.Flags;
    // caller should always eventually perform in its finally ... end block:
    //    fThreadInfo^.Flags := fThreadInfoBackup;
    include(nfo^.Flags, tiExceptionIgnore);
    {$endif NOEXCEPTIONINTERCEPT}
    result := true; // normal process, with eventual fThreadInfoBackup + UnLock
  end
  else
    result := false; // TSynLogFamily.DisableCurrentThread=true for this thread
end;

function TSynLog.QueryInterface(
  {$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif} iid: TGuid;
  out obj): TIntQry;
begin
  result := E_NOINTERFACE; // never used
end;

function TSynLog._AddRef: TIntCnt; // efficient ISynLog per-thread refcount
var
  nfo: PSynLogThreadInfo;
  refcnt: PByte;
begin // self <> nil indicates sllEnter in fFamily.Level and nfo^.Recursion OK
  result := 1; // should never be 0 (would release TSynLog instance)
  nfo := @PerThreadInfo; // access the threadvar - InitThreadNumber() already done
  if nfo^.RecursionCount = 0 then
    exit; // paranoid - but could happen if ISynLog is used from TSynLog.Add
  refcnt := @nfo^.Recursion[nfo^.RecursionCount - 1];
  inc(refcnt^); // stores ISynLog.RefCnt in lowest 8-bit
  if refcnt^ = 0 then
    ESynLogException.RaiseUtf8('Too many %._AddRef', [self]);
end;

function TSynLog._Release: TIntCnt; // efficient ISynLog per-thread refcount
var
  nfo: PSynLogThreadInfo;
  ms: Int64;
  refcnt: PByte;
  rec: PtrInt;
begin // self <> nil indicates sllEnter in fFamily.Level and nfo^.Recursion OK
  result := 1; // should never be 0 (would release TSynLog instance)
  nfo := @PerThreadInfo; // threadvar access - InitThreadNumber() already done
  if nfo^.RecursionCount = 0 then
    exit; // paranoid - but could happen if ISynLog is used from TSynLog.Add
  refcnt := @nfo^.Recursion[nfo^.RecursionCount - 1];
  dec(refcnt^); // stores ISynLog.RefCnt in lowest 8-bit
  if refcnt^ <> 0 then
    exit;
  dec(nfo^.RecursionCount);
  if not (sllLeave in fFamily.Level) then
    exit;
  // reached refcnt=0 -> append e.g. 00000000001FFF23  %  -    02.096.658
  QueryPerformanceMicroSeconds(ms);
  dec(ms, fStartTimestamp);
  FillInfo(nfo, @ms); // timestamp [+ threadnumber]
  dec(ms, PInt64(refcnt)^ shr 8); // elapsed time since Enter
  GlobalThreadLock.Lock;
  {$ifdef HASFASTTRYFINALLY}
  try
  {$else}
  begin // direct AddMicroSec() output should not trigger any exception
  {$endif HASFASTTRYFINALLY}
    LogHeaderNoRecursion(fWriter, sllLeave, @nfo^.CurrentTimeAndThread);
    rec := nfo^.RecursionCount; // rec <= MAX_SYNLOGRECURSION = 53
    if rec <> 0 then // inlined AddChars(#9, rec)
    begin
      FillCharFast(fWriter.B[1], rec, 9); // LogHeaderNoRecursion did AddShort()
      inc(fWriter.B, rec);
    end;
    fWriter.AddMicroSec(ms);
    fWriterEcho.AddEndOfLine(sllLeave);
  {$ifdef HASFASTTRYFINALLY}
  finally
  {$endif HASFASTTRYFINALLY}
    GlobalThreadLock.UnLock;
  end;
end;

constructor TSynLog.Create(aFamily: TSynLogFamily);
var
  entry: PInterfaceEntry;
begin
  if aFamily = nil then
    aFamily := Family;
  fFamily := aFamily;
  entry := GetInterfaceEntry(ISynLog);
  if (entry = nil) or
     not InterfaceEntryIsStandard(entry) {$ifdef FPC} or
     (entry^.IOffset > high(fISynLogOffset)) {$endif FPC} then
    ESynLogException.RaiseUtf8('%.Create: unexpected ISynLog entry', [self]);
  fISynLogOffset := entry^.IOffset;
end;

destructor TSynLog.Destroy;
begin
  Flush({forcediskwrite=}true);
  fWriterEcho.Free;
  fWriter.Free;
  fWriterStream.Free;
  inherited;
end;

procedure TSynLog.CloseLogFile;
begin
  GlobalThreadLock.Lock;
  try
    if fWriter = nil then
      exit;
    fWriter.FlushFinal;
    FreeAndNilSafe(fWriterEcho);
    FreeAndNilSafe(fWriter);
    FreeAndNilSafe(fWriterStream);
  finally
    fFlags := [];
    exclude(fPendingFlags, pendingRotate); // reset it (after FlushFinal)
    GlobalThreadLock.UnLock;
  end;
end;

procedure TSynLog.Release;
begin
  GlobalThreadLock.Lock;
  try
    CloseLogFile;
    ObjArrayDelete(SynLogFile, self);
    if fFamily.fPerThreadLog = ptOneFilePerThread then
      PerThreadInfo.FileLookup[fFamily.fIdent] := nil;
  finally
    GlobalThreadLock.UnLock;
  end;
  Free;
end;

procedure TSynLog.Flush(ForceDiskWrite: boolean);
var
  diskflush: THandle;
begin
  if (self = nil) or
     (fWriter = nil) then
    exit;
  diskflush := 0;
  GlobalThreadLock.Lock;
  try
    if fWriter = nil then
      exit;
    fWriter.FlushToStream;
    if ForceDiskWrite and
       fWriterStream.InheritsFrom(THandleStream) then
      diskflush := THandleStream(fWriterStream).Handle;
  finally
    GlobalThreadLock.UnLock;
  end;
  if diskflush <> 0 then
    FlushFileBuffers(diskflush); // slow OS operation outside of the main lock
end;

procedure TSynLog.RaiseDoEnter;
begin
  ESynLogException.RaiseUtf8('Too many %.Enter', [self]);
end;

function TSynLog.DoEnter: PSynLogThreadInfo;
var
  ndx: byte;
begin
  result := nil;
  if (self = nil) or
     (not (sllEnter in fFamily.fLevel)) or // void operation
     (fFamily.fPerThreadLog = ptNoThreadProcess) then // don't mess with recursion
    exit;
  result := GetThreadInfo; // may call InitThreadNumber() if first access
  if not (tiTemporaryDisable in result^.Flags) then
  begin
    ndx := result^.RecursionCount;
    inc(ndx);
    if ndx = 0 then
      RaiseDoEnter;
    result^.RecursionCount := ndx;
    if ndx <= high(result^.Recursion) then
      exit; // fine
  end;
  result := nil; // logging disabled, or above MAX_SYNLOGRECURSION
end;

procedure TSynLog.LockAndPrepareEnter(nfo: PSynLogThreadInfo; microsecs: PInt64);
var
  ms, rec: Int64;
begin
  // prepare output file if not already done - and compute fStartTimestamp
  if not (logInitDone in fFlags) then
    LogFileInit(nfo);
  // setup recursive timing with RefCnt=1 like with _AddRef outside lock
  if sllLeave in fFamily.Level then
  begin
    QueryPerformanceMicroSeconds(ms);
    if microsecs <> nil then
      microsecs^ := ms;
    dec(ms, fStartTimestamp);
    FillInfo(nfo, @ms); // timestamp [+ threadnumber]
    rec := ms shl 8 + {RefCnt=}1;
  end
  else
  begin
    FillInfo(nfo, nil);
    if microsecs <> nil then
      microsecs^ := 0;
    rec := {RefCnt=}1; // no timestamp needed if no sllLeave
  end;
  nfo^.Recursion[nfo^.RecursionCount - 1] := rec; // with RefCnt = 1
  // prepare for the actual content logging
  GlobalThreadLock.Lock;
  SetThreadInfoAndThreadName(self, nfo);
end;

procedure TSynLog.LogEnter(nfo: PSynLogThreadInfo; inst: TObject; txt: PUtf8Char
  {$ifdef ISDELPHI} ; addr: PtrUInt {$endif});
begin
  LockAndPrepareEnter(nfo, nil);
  // append e.g. 00000000001FE4DC  !  +       TSqlDatabase(01039c0280).DBClose
  {$ifdef HASFASTTRYFINALLY}
  try
  {$else}
  begin // direct txt output should not trigger any exception
  {$endif HASFASTTRYFINALLY}
    LogHeader(sllEnter, inst);
    if txt <> nil then
      fWriter.AddOnSameLine(txt)
    {$ifdef ISDELPHI}
    else if addr <> 0 then
      // no method name specified -> try from map/mab symbols
      TDebugFile.Log(fWriter, addr, {notcode=}false, {symbol=}true)
    {$endif ISDELPHI};
    fWriterEcho.AddEndOfLine(sllEnter);
  {$ifdef HASFASTTRYFINALLY}
  finally
  {$endif HASFASTTRYFINALLY}
    GlobalThreadLock.UnLock;
  end;
end;

procedure TSynLog.LogEnterFmt(nfo: PSynLogThreadInfo; inst: TObject;
  fmt: PUtf8Char; args: PVarRec; argscount: PtrInt; microsecs: PInt64);
begin
  LockAndPrepareEnter(nfo, microsecs);
  fThreadInfoBackup := nfo^.Flags;
  try
    include(nfo^.Flags, tiExceptionIgnore);
    LogHeader(sllEnter, inst);
    fWriter.AddFmt(fmt, args, argscount, twOnSameLine,
      [woDontStoreDefault, woDontStoreVoid, woFullExpand]);
    fWriterEcho.AddEndOfLine(sllEnter);
  finally
    nfo^.Flags := fThreadInfoBackup;
    GlobalThreadLock.UnLock;
  end;
end;

{$ifdef ISDELPHI} // specific to Delphi: fast get the caller method name

{$STACKFRAMES ON} // we need a stack frame for ebp/RtlCaptureStackBackTrace
{$ifdef CPU64}
  {$define USERTLCAPTURESTACKBACKTRACE}
{$else}
  {$define USEASMX86STACKBACKTRACE}
{$endif CPU64}

class function TSynLog.Enter(aInstance: TObject; aMethodName: PUtf8Char): ISynLog;
var
  log: TSynLog;
  nfo: PSynLogThreadInfo;
  addr: PtrUInt;
begin
  result := nil;
  log := Add;
  nfo := log.DoEnter;
  if nfo = nil then
    exit; // nothing to log
  addr := 0;
  if aMethodName = nil then
  begin
    {$ifdef USERTLCAPTURESTACKBACKTRACE}
    if RtlCaptureStackBackTrace(1, 1, @addr, nil) = 0 then
      addr := 0;
    {$else}
    asm
      mov  eax, [ebp + 4] // retrieve caller EIP from push ebp; mov ebp,esp
      mov  addr, eax
    end;
    {$endif USERTLCAPTURESTACKBACKTRACE}
    if addr <> 0 then
      dec(addr, 5);
  end;
  log.LogEnter(nfo, aInstance, aMethodName, addr);
  pointer(result) := PAnsiChar(log) + log.fISynLogOffset; // result := self
end;

{$STACKFRAMES OFF} // back to {$W-} normal state, as in mormot.defines.inc

{$else}

class function TSynLog.Enter(aInstance: TObject; aMethodName: PUtf8Char): ISynLog;
begin
  result := nil;
  EnterLocal(result, aInstance, aMethodName);
end;

{$endif ISDELPHI}

class function TSynLog.Enter(TextFmt: PUtf8Char;
  const TextArgs: array of const; aInstance: TObject): ISynLog;
begin
  result := nil;
  EnterLocal(result, TextFmt, TextArgs, aInstance);
end;

class function TSynLog.EnterLocal(var Local: ISynLog; TextFmt: PUtf8Char;
  const TextArgs: array of const; aInstance: TObject): TSynLog;
var
  nfo: PSynLogThreadInfo;
begin // expects the caller to have set Local = nil
  result := Add;
  nfo := result.DoEnter;
  if nfo = nil then
    exit; // nothing to log
  result.LogEnterFmt(nfo, aInstance, TextFmt, @TextArgs[0], length(TextArgs), nil);
  pointer(Local) := PAnsiChar(result) + result.fISynLogOffset; // result := self
end;

class function TSynLog.EnterLocal(var Local: ISynLog; aInstance: TObject;
  aMethodName: PUtf8Char): TSynLog;
var
  nfo: PSynLogThreadInfo;
begin // expects the caller to have set Local = nil
  result := Add;
  nfo := result.DoEnter;
  if nfo = nil then
    exit; // nothing to log
  result.LogEnter(nfo, aInstance, aMethodName); // with refcnt = 1
  pointer(Local) := PAnsiChar(result) + result.fISynLogOffset; // result := self
end;

class function TSynLog.EnterLocalString(var Local: ISynLog; aInstance: TObject;
  const aMethodName: string): TSynLog;
var
  nfo: PSynLogThreadInfo;
begin // expects the caller to have set Local = nil
  result := Add;
  nfo := result.DoEnter;
  if nfo = nil then
    exit; // nothing to log
  result.LockAndPrepareEnter(nfo, nil); // inlined result.LogEnter()
  result.LogHeader(sllEnter, aInstance);
  if aMethodName <> '' then // direct string output with no temp conversion
    result.fWriter.AddOnSameLineString(aMethodName);
  result.fWriterEcho.AddEndOfLine(sllEnter);
  GlobalThreadLock.UnLock;
  pointer(Local) := PAnsiChar(result) + result.fISynLogOffset; // result := self
end;

procedure TSynLog.ManualEnter(aMethodName: PUtf8Char; aInstance: TObject);
var
  nfo: PSynLogThreadInfo;
begin
  nfo := DoEnter;
  if nfo <> nil then
    LogEnter(nfo, aInstance, aMethodName);
end;

procedure TSynLog.ManualEnter(aInstance: TObject; TextFmt: PUtf8Char;
  const TextArgs: array of const; MicroSecs: PInt64);
var
  nfo: PSynLogThreadInfo;
begin
  nfo := DoEnter;
  if nfo <> nil then
    LogEnterFmt(nfo, aInstance, TextFmt, @TextArgs[0], length(TextArgs), MicroSecs);
end;

procedure TSynLog.ManualLeave;
begin
  if self <> nil then
    _Release;
end;

type
  TSynLogVoid = class(TSynLog);

class function TSynLog.Void: TSynLogClass;
begin
  with TSynLogVoid.Family do
    if fLevel <> [] then
      SetLevel([]); // paranoid (if user did change the family settings)
  result := TSynLogVoid;
end;

function TSynLog.Instance: TSynLog;
begin
  result := self;
end;

{$ifdef OSLINUX}
procedure SystemdEcho(Level: TSynLogLevel; const Text: RawUtf8);
var
  priority: TShort16;
  mtmp: RawUtf8;
  jvec: array[0..1] of TIoVec;
const
  _MESSAGE: array[0..7] of AnsiChar = 'MESSAGE=';
begin
  if (length(Text) < 18) or
     not sd.IsAvailable then
    // should be at last "20200615 08003008  "
    exit;
  FormatShort16('PRIORITY=%', [LOG_TO_SYSLOG[Level]], priority);
  jvec[0].iov_base := @priority[1];
  jvec[0].iov_len := ord(priority[0]);
  // skip time "20200615 08003008  ." which should not be part of the jvec[]
  TrimCopy(Text, 18 - 8, Utf8TruncatedLength(Text, 1500) - (18 - 8 - 1), mtmp);
  // systemd truncates to LINE_MAX = 2048 anyway and expects valid UTF-8
  with jvec[1] do
  begin
    iov_base := pointer(mtmp);
    iov_len := length(mtmp);
    while (iov_len > 0) and
          (iov_base[8] <= ' ') do // trim left spaces
    begin
      inc(iov_base);
      dec(iov_len);
    end;
    PInt64(iov_base)^ := PInt64(@_MESSAGE)^;
  end;
  sd.journal_sendv(jvec[0], 2);
end;
{$endif OSLINUX}

function TSynLog.ConsoleEcho(Sender: TEchoWriter; Level: TSynLogLevel;
  const Text: RawUtf8): boolean;
begin
  result := true;
  if Level in fFamily.fEchoToConsole then
    {$ifdef OSLINUX}
    if Family.EchoToConsoleUseJournal then
      SystemdEcho(Level, Text)
    else
    {$endif OSLINUX}
    if fFamily.EchoToConsoleBackground and
       Assigned(AutoFlushThread) then
      AutoFlushThread.AddToConsole(Text, LOG_CONSOLE_COLORS[Level])
    else
    begin
      ConsoleWrite(Text, LOG_CONSOLE_COLORS[Level]);
      TextColor(ccLightGray);
    end;
end;

procedure TSynLog.Log(Level: TSynLogLevel; Fmt: PUtf8Char;
  const Args: array of const; aInstance: TObject);
begin
  if (self <> nil) and
     (Level in fFamily.fLevel) then
    LogInternalFmt(Level, Fmt, @Args[0], length(Args), aInstance);
end;

procedure TSynLog.Log(Level: TSynLogLevel; const Text: RawUtf8;
  aInstance: TObject; TextTruncateAtLength: PtrInt);
begin
  if (self <> nil) and
     (Level in fFamily.fLevel) then
    LogInternalText(Level, pointer(Text), length(Text), aInstance,
                    TextTruncateAtLength);
end;

{$ifdef UNICODE}
procedure TSynLog.Log(Level: TSynLogLevel; const Text: string; aInstance: TObject);
var
  vr: TVarRec;
begin
  if (self = nil) or
     not (Level in fFamily.fLevel) then
    exit;
  vr.VType := vtUnicodeString;
  vr.VUnicodeString := pointer(Text);
  LogInternalFmt(Level, '%', @vr, 1, aInstance);
end;
{$endif UNICODE}

procedure TSynLog.LogLines(Level: TSynLogLevel; LinesToLog: PUtf8Char;
  aInstance: TObject; const IgnoreWhenStartWith: PAnsiChar);

  procedure DoLog(LinesToLog: PUtf8Char); // sub-procedure for local RawUtf8
  var
    s: RawUtf8;
  begin
    repeat
      GetNextItemTrimedCRLF(LinesToLog, s);
      if s <> '' then
        if (IgnoreWhenStartWith = nil) or
           not IdemPChar(pointer(s), IgnoreWhenStartWith) then
          LogText(Level, pointer(s), aInstance);
    until LinesToLog = nil;
  end;

begin
  if (self <> nil) and
     (Level in fFamily.fLevel) and
     (LinesToLog <> nil) then
    DoLog(LinesToLog);
end;

procedure CleanThreadName(var name: RawUtf8);
var
  i: PtrInt;
begin
  for i := 1 to length(name) do
    if name[i] < ' ' then
      name[i] := ' '; // ensure on same line
  name := TrimU(StringReplaceAll(name, [
    'TSqlRest',        '',
    'TRest',           '',
    'TSql',            '',
    'TSQLRest',        '',
    'TSQL',            '',
    'TOrmRest',        '',
    'TOrm',            '',
    'TWebSocket',      'WS',
    'TServiceFactory', 'SF',
    'TSyn',            '',
    'Thread',          '',
    'Process',         '',
    'Background',      'Bgd',
    'WebSocket',       'WS',
    'Asynch',          'A',
    'Async',           'A',
    'Parallel',        'Prl',
    'Timer',           'Tmr',
    'Thread',          'Thd',
    'Database',        'DB',
    'Backup',          'Bak',
    'Server',          'Srv',
    'Client',          'Cli',
    'synopse',         'syn',
    'memory',          'mem',
    '  ',              ' '
    ]));
end;

procedure _SetThreadName(ThreadID: TThreadID; const Format: RawUtf8;
  const Args: array of const);
var
  name: RawUtf8;
  i: PtrInt;
  n: TShort31;
  ps: PShortString;
begin
  if SynLogFileFreeing then
    exit; // inconsistent call at shutdown
  n[0] := #0;
  if Format <> '' then
  begin
    // compute the full thread name
    FormatUtf8(Format, Args, name);
    if Format[1] = '=' then
      delete(name, 1, 1) // no need to clean this thread identifier
    else
      CleanThreadName(name); // clean e.g. class names or common identifiers
    // compute the shortened thread name as plain ASCII-7 identifier
    for i := 1 to length(name) do
      if name[i] in ['a'..'z', 'A'..'Z', '0'..'9', '.', ':'
        {$ifdef OSWINDOWS}, ' ', '-'{$endif}] then
      begin
        AppendShortChar(name[i], @n);
        if n[0] = #31 then
          break; // TShort31
      end;
  end;
  // set this process threadvar and notify the OS
  ps := nil;
  if ThreadID = GetCurrentThreadId then // from SetCurrentThreadName()
  begin
    ps := CurrentThreadNameShort;
    if ps^ = n then
      exit; // already set as such
    ps^ := n;
  end;
  RawSetThreadName(ThreadID, {$ifdef OSWINDOWS} name {$else} n {$endif});
  // store full name in global SynLogThreads.Name[]
  if ps <> nil then
    InternalSetCurrentThreadName(name);
end;

function _GetCurrentThreadName: RawUtf8;
var
  ndx: PtrInt;
  thd: PSynLogThreads;
begin
  result := '';
  if not SynLogFileFreeing then
  begin
    ndx := PerThreadInfo.ThreadNumber - 1; // no InitThreadNumber() call
    if ndx >= 0 then
    begin
      thd := @SynLogThreads;
      thd^.Safe.Lock;
      if ndx < length(thd^.Name) then
        result := thd^.Name[ndx]; // full thread name
      thd^.Safe.UnLock;
    end;
  end;
  if result = '' then // fallback to mormot.core.os default TShort21 behavior
    ShortStringToAnsi7String(CurrentThreadNameShort^, result);
end;

class procedure TSynLog.LogThreadName(const Name: RawUtf8);
var
  n: RawUtf8;
begin
  n := Name;
  if n = '' then
    ShortStringToAnsi7String(CurrentThreadNameShort^, n);
  SetCurrentThreadName(n); // redirect to _SetThreadName() above
end;

function TSynLog.LogClass: TSynLogClass;
begin
  if self = nil then
    result := nil
  else
    result := PPointer(self)^;
end;

class procedure TSynLog.DoLog(Level: TSynLogLevel; Fmt: PUtf8Char;
   const Args: array of const; Instance: TObject);
var
  log: TSynLog;
begin
  log := Add;
  if (log <> nil) and
     (Level in log.fFamily.fLevel) then
    log.LogInternalFmt(Level, Fmt, @Args[0], length(Args), Instance);
end;

class procedure TSynLog.ProgressInfo(Sender: TObject; Info: PProgressInfo);
var
  log: TSynLog;
begin
  log := Add;
  if (log <> nil) and
     (sllTrace in log.fFamily.fLevel) then
    log.Log(sllTrace, Info^.GetProgress, Sender);
end;

procedure TSynLog.ForceRotation;
begin
  GlobalThreadLock.Lock;
  try
    PerformRotation(nil);
  finally
    GlobalThreadLock.UnLock;
  end;
end;

procedure TSynLog.DisableRemoteLog(entervalue: boolean);
begin
  if not Assigned(fFamily.fEchoRemoteEvent) then
    exit;
  if entervalue then
  begin
    GlobalThreadLock.Lock;
    if pendingDisableRemoteLogLeave in fPendingFlags then
    begin
      GlobalThreadLock.UnLock;
      ESynLogException.RaiseUtf8('Nested %.DisableRotemoteLog', [self]);
    end;
    include(fPendingFlags, pendingDisableRemoteLogLeave);
  end
  else
  begin
    if not (pendingDisableRemoteLogLeave in fPendingFlags) then
      ESynLogException.RaiseUtf8('Missing %.DisableRotemoteLog(true)', [self]);
    // DisableRemoteLog(false) -> add to events, and quit the global mutex
    exclude(fPendingFlags, pendingDisableRemoteLogLeave);
    fWriterEcho.EchoAdd(fFamily.fEchoRemoteEvent);
    GlobalThreadLock.UnLock;
  end;
end;

procedure TSynLog.Log(Level: TSynLogLevel; aInstance: TObject);
begin
  if (self <> nil) and
     (Level in fFamily.fLevel) and
     (aInstance <> nil) then
    LogInternalText(Level, nil, 0, aInstance, 0);
end;

procedure TSynLog.Log(Level: TSynLogLevel; const aName: RawUtf8;
  aTypeInfo: PRttiInfo; const aValue; Instance: TObject);
begin
  if (self <> nil) and
     (Level in fFamily.fLevel) then
    LogInternalRtti(Level, aName, aTypeInfo, aValue, Instance);
end;

{$ifdef ISDELPHI}
  {$STACKFRAMES ON} // we need a stack frame for ebp/RtlCaptureStackBackTrace
{$endif ISDELPHI}

procedure TSynLog.Log(Level: TSynLogLevel);
var
  lasterror: integer;
  {$ifdef ISDELPHI}
  addr: PtrUInt;
  {$endif ISDELPHI}
begin
  if (self = nil) or
     not (Level in fFamily.fLevel) then
    exit;
  lasterror := 0;
  if Level = sllLastError then
    lasterror := GetLastError;
  if LockAndDisableExceptions then
  try
    LogHeader(Level, nil);
    if lasterror <> 0 then
      AddErrorMessage(lasterror);
    {$ifdef ISDELPHI}
    addr := 0;
    {$ifdef USERTLCAPTURESTACKBACKTRACE}
    if RtlCaptureStackBackTrace(1, 1, @addr, nil) = 0 then
      addr := 0;
    {$endif USERTLCAPTURESTACKBACKTRACE}
    {$ifdef USEASMX86STACKBACKTRACE}
    asm
      mov  eax, [ebp + 4]  // retrieve caller EIP from push ebp; mov ebp,esp
      mov  addr, eax
    end;
    {$endif USEASMX86STACKBACKTRACE}
    if addr <> 0 then
      TDebugFile.Log(fWriter, addr - 5, {notcode=}false, {symbol=}true);
    {$endif ISDELPHI}
    LogTrailer(Level);
  finally
    fThreadInfo^.Flags := fThreadInfoBackup;
    GlobalThreadLock.UnLock;
    if lasterror <> 0 then
      SetLastError(lasterror);
  end;
end;

procedure TSynLog.LogText(Level: TSynLogLevel; Text: PUtf8Char; Instance: TObject);
begin
  if (self = nil) or
     (Text = nil) or
     not (Level in fFamily.fLevel) then
    exit;
  if LockAndDisableExceptions then
  {$ifdef HASFASTTRYFINALLY}
  try
  {$else}
  begin // direct Text output should not trigger any exception
  {$endif HASFASTTRYFINALLY}
    LogHeader(Level, Instance);
    fWriter.AddOnSameLine(Text); // end with #0
    LogTrailer(Level);
  {$ifdef HASFASTTRYFINALLY}
  finally
  {$endif HASFASTTRYFINALLY}
    fThreadInfo^.Flags := fThreadInfoBackup;
    GlobalThreadLock.UnLock;
  end;
end;

procedure TSynLog.LogText(Level: TSynLogLevel; Text: PUtf8Char; TextLen: PtrInt;
  Instance: TObject; TextTruncateAtLength: PtrInt);
begin
  if (self <> nil) and
     (Level in fFamily.fLevel) then
    LogInternalText(Level, Text, TextLen, Instance, TextTruncateAtLength);
end;

procedure TSynLog.LogEscape(Level: TSynLogLevel; const ContextFmt: RawUtf8;
  const ContextArgs: array of const; Data: pointer; DataLen: PtrInt;
  Instance: TObject; TruncateLen: PtrInt);
var
  tmp: array[0 .. MAX_LOGESCAPE + 256] of AnsiChar; // pre-render on local buffer
  tmps: ShortString absolute tmp;
begin
  if (self = nil) or
     not (Level in fFamily.fLevel) then
    exit;
  tmps[0] := #0;
  if ContextFmt <> '' then
    FormatShort(ContextFmt, ContextArgs, tmps);
  AppendShort(' len=', tmps);
  AppendShortCardinal(DataLen, tmps);
  AppendShortChar(' ', @tmps);
  ContentAppend(Data, DataLen, ord(tmp[0]), MinPtrInt(high(tmp), TruncateLen), @tmp[1]);
  LogText(Level, @tmp[1], Instance); // this method with ending #0 is the fastest
end;

{$STACKFRAMES OFF} // back to {$W-} normal state, as in mormot.defines.inc

{$ifdef WIN64DELPHI} // Delphi Win64 has no 64-bit inline assembler
procedure DebugBreak;
asm
     .noframe
     int  3
end;
{$endif WIN64DELPHI}

class procedure TSynLog.DebuggerNotify(Level: TSynLogLevel; const Text: RawUtf8);
begin
  if Text = '' then
    exit;
  Add.LogInternalText(Level, pointer(Text), length(Text), nil, 16384);
  {$ifdef ISDELPHI} // Lazarus/fpdebug does not like "int 3" instructions
  {$ifdef OSWINDOWS}
  if IsDebuggerPresent then
    {$ifdef WIN64DELPHI}
    DebugBreak
    {$else}
    asm
      int  3
    end
    {$endif WIN64DELPHI}
  else
  {$endif OSWINDOWS}
  {$endif ISDELPHI}
    ConsoleWrite('%  ', [Text], LOG_CONSOLE_COLORS[Level], {noLF=}true);
end;

class procedure TSynLog.DebuggerNotify(Level: TSynLogLevel;
  const Format: RawUtf8; const Args: array of const);
var
  txt: RawUtf8;
begin
  if Format = '' then
    exit;
  FormatUtf8(Format, Args, txt);
  DebuggerNotify(Level, txt);
end;

procedure TSynLog.LogFileInit(nfo: PSynLogThreadInfo);
begin
  GlobalThreadLock.Lock;
  try
    fThreadInfo := nfo;
    if logInitDone in fFlags then // paranoid thread safety
      exit;
    // setup (once) proper timing for this log instance
    if fStartTimestamp = 0 then // don't reset after rotation
    begin
      QueryPerformanceMicroSeconds(fStartTimestamp);
      if fFamily.FileExistsAction = acAppend then
        fFamily.HighResolutionTimestamp := false; // file reuse = absolute time
      if fFamily.LocalTimestamp then
        fStartTimestampDateTime := Now
      else
        fStartTimestampDateTime := NowUtc;
    end;
    // check if we need to log the thread names in this new file
    if (sllInfo in fFamily.Level) and
       (fFamily.PerThreadLog = ptIdentifiedInOneFile) then
      include(fFlags, logAddThreadName);
    fThreadNameLogged := nil; // force re-notify
    // eventually mark this instance as initialized (i.e. fStartTimestamp set)
    include(fFlags, logInitDone);
    // initialize fWriter and its optional header - if needed
    if fWriter = nil then
      CreateLogWriter; // file creation should be thread-safe
    if not (logFileHeaderWritten in fFlags) then
      LogFileHeader; // executed once per file - not needed in acAppend mode
    // append a sllNewRun line at the log file (re)opening
    FillInfo(nfo, nil);
    LogHeaderNoRecursion(fWriter, sllNewRun, @nfo^.CurrentTimeAndThread);
    fWriter.AddString(Executable.ProgramName);
    fWriter.AddDirect(' ');
    if Executable.Version.Major <> 0 then
      fWriter.AddNoJsonEscapeString(Executable.Version.Detailed)
    else
      fWriter.AddDateTime(@Executable.Version.BuildDateTime, ' ');
    fWriter.AddDirect(' ');
    fWriter.AddShort(ClassNameShort(self)^);
    fWriter.AddShort(' ' + SYNOPSE_FRAMEWORK_VERSION);
    AddSysInfo;
    fWriterEcho.AddEndOfLine(sllNewRun);
  finally
    GlobalThreadLock.UnLock;
  end;
end;

procedure TSynLog.LogFileHeader;
var
  w: TJsonWriter;
  i: PtrInt;
  {$ifdef OSWINDOWS}
  Env: PWideChar;
  P: PWideChar;
  L: integer;
  {$endif OSWINDOWS}
begin
  include(fFlags, logFileHeaderWritten);
  w := fWriter;
  if w.WrittenBytes = 0 then // paranoid
  begin
    w.AddString(Executable.ProgramFullSpec);
    w.AddDirect(#10);
    w.AddShorter('Host=');
    w.AddString(Executable.Host);
    w.AddShorter(' User=');
    w.AddString(Executable.User);
    w.AddShort(' CPU='); // not AddShorter() for AddDirect(CpuInfoText) below
    if CpuInfoText = '' then
      w.Add(SystemInfo.dwNumberOfProcessors)
    else
      for i := 1 to length(CpuInfoText) do
        if not (ord(CpuInfoText[i]) in [1..32, ord(':')]) then
          w.AddDirect(CpuInfoText[i]);
    {$ifdef OSWINDOWS}
    w.AddDirect('*');
    w.Add(SystemInfo.wProcessorArchitecture);
    w.AddDirect('-');
    w.Add(SystemInfo.wProcessorLevel);
    w.AddDirect('-');
    w.Add(SystemInfo.wProcessorRevision);
    {$endif OSWINDOWS}
    {$ifdef CPUINTEL}
    w.AddDirect(':');
    w.AddBinToHexMinChars(@CpuFeatures, SizeOf(CpuFeatures), {lower=}true);
    {$endif CPUINTEL}
    {$ifdef CPUARM3264}
    w.Add(':', {$ifdef CPUARM} '-' {$else} '+' {$endif}); // ARM marker
    w.AddBinToHexMinChars(@CpuFeatures, SizeOf(CpuFeatures), {lower=}true);
    {$endif CPUARM3264}
    w.AddDirect(' ', 'O', 'S', '=');
    {$ifdef OSWINDOWS}
    w.AddB(ord(OSVersion));
    w.AddDirect('.');
    w.AddU(OSVersionInfo.wServicePackMajor);
    w.AddDirect('=');
    w.AddU(OSVersionInfo.dwMajorVersion);
    w.AddDirect('.');
    w.AddU(OSVersionInfo.dwMinorVersion);
    w.AddDirect('.');
    w.AddU(OSVersionInfo.dwBuildNumber);
    {$else}
    w.AddString(OS_NAME[OS_KIND]);
    w.AddDirect('=');
    w.AddTrimSpaces(pointer(SystemInfo.uts.sysname));
    w.AddDirect('-');
    w.AddTrimSpaces(pointer(SystemInfo.uts.release));
    w.AddReplace(pointer(SystemInfo.uts.version), ' ', '-');
    {$endif OSWINDOWS}
    if OSVersionInfoEx <> '' then
    begin
      w.AddDirect('/');
      w.AddTrimSpaces(OSVersionInfoEx);
    end;
    {$ifdef OSWINDOWS}
    w.AddShorter(' Wow64=');
    w.AddB(ord(IsWow64) + ord(IsWow64Emulation) shl 1); // 0, 1, 2 or 3
    {$else}
    w.AddShorter(' Wow64=0');
    {$endif OSWINDOWS}
    w.AddShort(' Freq=1000000'); // we use QueryPerformanceMicroSeconds()
    if IsLibrary then
    begin
      w.AddShort(' Instance=');
      w.AddNoJsonEscapeString(Executable.InstanceFileName);
    end;
    {$ifdef OSWINDOWS}
    if not fFamily.fNoEnvironmentVariable then
    begin
      w.AddDirect(#10);
      w.AddShort('Environment variables=');
      Env := GetEnvironmentStringsW;
      P := pointer(Env);
      while P^ <> #0 do
      begin
        L := StrLenW(P);
        if (L > 0) and
           (P^ <> '=') then
        begin
          w.AddNoJsonEscapeW(pointer(P));
          w.AddDirect(#9);
        end;
        inc(P, L + 1);
      end;
      FreeEnvironmentStringsW(Env);
      w.CancelLastChar(#9);
    end;
    {$endif OSWINDOWS}
    w.AddDirect(#10);
    w.AddClassName(self.ClassType);
    w.AddShort(' ' + SYNOPSE_FRAMEWORK_FULLVERSION + ' ');
    w.AddDateTime(fStartTimestampDateTime);
    w.AddDirect(#10, #10);
    w.FlushToStream;
    fWriterEcho.EchoReset; // header is not to be sent to console
  end;
  fStreamPositionAfterHeader := w.WrittenBytes;
end;

procedure TSynLog.AddMemoryStats;
var
  info: TMemoryInfo; // cross-compiler and cross-platform
begin
  if GetMemoryInfo(info, {withalloc=}true) then
    fWriter.Add(
      ' System: memtotal=% memfree=% filetotal=% filefree=% allocres=% allocused=% ',
      [KBNoSpace(info.memtotal),      KBNoSpace(info.memfree),
       KBNoSpace(info.filetotal),     KBNoSpace(info.filefree),
       KBNoSpace(info.allocreserved), KBNoSpace(info.allocused)]);
  // include mormot.core.fpcx64mm raw information if available
  fWriter.AddOnSameLine(pointer(RetrieveMemoryManagerInfo));
  fWriter.AddDirect(' ', ' ', ' ');
end;

procedure TSynLog.AddErrorMessage(Error: cardinal);
var
  msg: ShortString;
begin
  fWriter.AddDirect(' ', '"');
  GetErrorShortVar(Error, msg);
  fWriter.AddOnSameLine(@msg[0], ord(msg[0]));
  fWriter.AddDirect('"', ' ', '(');
  fWriter.AddU(Error);
  fWriter.AddDirect(')', ' ');
end;

procedure TSynLog.AddSysInfo;
var
  tmp: ShortString;
begin
  fWriter.AddDirect(' ', '{');
  RetrieveSysInfoText(tmp);
  fWriter.AddShort(tmp);
  fWriter.AddDirect('}');
end;

procedure TSynLog.FillInfo(nfo: PSynLogThreadInfo; MicroSec: PInt64);
var
  st: TSynSystemTime;
  ms: Int64 absolute st;
  p: PUtf8Char;
begin // set timestamp [+ threadnumber] - usually run outside GlobalThreadLock
  p := @nfo^.CurrentTimeAndThread;
  if fFamily.HighResolutionTimestamp then
  begin
    if MicroSec = nil then
    begin
      QueryPerformanceMicroSeconds(ms); // fast syscall or VDSO 
      dec(ms, fStartTimestamp);
      MicroSec := @ms;
    end;
    p[0] := #16; // 64-bit microseconds = 584704 years as 16 chars
    BinToHexDisplayLower(pointer(MicroSec), @p[1], SizeOf(ms));
  end
  else
  begin
    FromGlobalTime(st, fFamily.LocalTimestamp); // with 16ms cache
    p[0] := #17;
    st.ToLogTime(@p[1]); // '20110325 19241502' 17 chars
    if fFamily.ZonedTimestamp then
      AppendShortChar('Z', PAnsiChar(p));
  end;
  if fFamily.fPerThreadLog <> ptIdentifiedInOneFile then
    exit;
  Int18ToText(nfo^.ThreadNumber, @p[ord(p[0]) + 1]);
  inc(p[0], 3); // final length is 19-20 chars into string[21]
end;

procedure TSynLog.PerformRotation(nfo: PSynLogThreadInfo);
var
  currentMaxSynLZ: cardinal;
  bak: TSynLogThreadInfoFlags;
  i: PtrInt;
  ext: TFileName;
  FN: array of TFileName;
begin // caller made GlobalThreadLock.Lock
  exclude(fPendingFlags, pendingRotate);
  if nfo = nil then
    nfo := @PerThreadInfo; // from ForceRotation
  bak := nfo^.Flags;
  include(nfo^.Flags, tiExceptionIgnore); // avoid infinite locks
  try
    CloseLogFile;
    try
      if not (Assigned(fFamily.fOnRotate) and
              fFamily.fOnRotate(self, fFileName)) then
      begin
        if fFamily.fRotateFileCount > 1 then
        begin
          // rotate e.g. xxx.1.synlz ... xxx.9.synlz files
          ext := '.log';
          if LogCompressAlgo <> nil then
            ext := LogCompressAlgo.AlgoFileExt; // e.g. '.synlz' or '.gz'
          currentMaxSynLZ := 0;
          SetLength(FN, fFamily.fRotateFileCount - 1);
          for i := fFamily.fRotateFileCount - 1 downto 1 do
          begin
            FN[i - 1] := ChangeFileExt(fFileName, MakeString(['.', i, ext]));
            if (currentMaxSynLZ = 0) and
               FileExists(FN[i - 1]) then
              currentMaxSynLZ := i;
          end;
          if currentMaxSynLZ = fFamily.fRotateFileCount - 1 then
            // delete (and archive) xxx.9.synlz
            fFamily.ArchiveAndDeleteFile(FN[currentMaxSynLZ - 1]);
          for i := fFamily.fRotateFileCount - 2 downto 1 do
            // e.g. xxx.8.synlz -> xxx.9.synlz
            RenameFile(FN[i - 1], FN[i]);
          // compress the current FN[0] .log file into xxx.1.log/.synlz
          if LogCompressAlgo = nil then
            // no compression: quickly rename FN[0] into xxx.1.log
            RenameFile(fFileName, FN[0])
          else if (AutoFlushThread <> nil) and
                  (AutoFlushThread.fToCompress = '') and
                  RenameFile(fFileName, FN[0]) then
          begin
            // background compression of FN[0] into xxx.1.synlz
            AutoFlushThread.fToCompress := FN[0];
            AutoFlushThread.fEvent.SetEvent;
          end
          else
          begin
            // blocking compression in the main processing thread
            LogCompressAlgo.FileCompress(fFileName, FN[0], LOG_MAGIC, true);
            DeleteFile(fFileName);
          end;
        end
        else
          fFamily.ArchiveAndDeleteFile(fFileName);
      end;
    except
      // just ignore any problem during file rotation, and recreate the log file
    end;
    // initialize a brand new log file
    LogFileInit(GetThreadInfo);
  finally
    nfo^.Flags := bak;
  end;
end;

procedure TSynLog.LogInternalFmt(Level: TSynLogLevel; Format: PUtf8Char;
  Values: PVarRec; ValuesCount: integer; Instance: TObject);
var
  lasterror: cardinal;
begin
  lasterror := 0;
  if Level = sllLastError then
    lasterror := GetLastError;
  if LockAndDisableExceptions then
  try
    LogHeader(Level, Instance);
    fWriter.AddFmt(Format, Values, ValuesCount, twOnSameLine,
      [woDontStoreDefault, woDontStoreVoid, woFullExpand]);
    if lasterror <> 0 then
      AddErrorMessage(lasterror);
    LogTrailer(Level);
  finally
    fThreadInfo^.Flags := fThreadInfoBackup;
    GlobalThreadLock.UnLock;
    if lasterror <> 0 then
      SetLastError(lasterror);
  end;
end;

procedure TSynLog.LogInternalText(Level: TSynLogLevel; Text: PUtf8Char;
  TextLen: PtrInt; Instance: TObject; TextTruncateAtLength: PtrInt);
var
  lasterror, trunclen: PtrInt;
begin
  lasterror := 0;
  if Level = sllLastError then
    lasterror := GetLastError;
  if LockAndDisableExceptions then
  try
    LogHeader(Level, Instance);
    if Text = nil then
    begin
      if Instance <> nil then
        // by definition, a JSON object is serialized on the same line
        fWriter.WriteObject(Instance, [woFullExpand]);
    end
    else
    begin
      trunclen := TextLen;
      if (TextTruncateAtLength <> 0) and
         (TextLen > TextTruncateAtLength) then
        trunclen := Utf8TruncatedLength(pointer(Text), TextLen, TextTruncateAtLength);
      if IsValidUtf8Buffer(Text, trunclen) then // may use AVX2
        if trunclen <> TextLen then
        begin
          fWriter.AddOnSameLine(Text, trunclen);
          fWriter.AddShort('... (truncated) length=');
          fWriter.AddU(TextLen);
        end
        else
          fWriter.AddOnSameLine(Text, TextLen) // TextLen may be < length(Text)
      else // binary is written as escaped text and $xx binary
        fWriter.AddEscapeBuffer(Text, trunclen, TextTruncateAtLength);
    end;
    if lasterror <> 0 then
      AddErrorMessage(lasterror);
    LogTrailer(Level);
  finally
    fThreadInfo^.Flags := fThreadInfoBackup;
    GlobalThreadLock.UnLock;
    if lasterror <> 0 then
      SetLastError(lasterror);
  end;
end;

procedure TSynLog.LogInternalRtti(Level: TSynLogLevel; const aName: RawUtf8;
  aTypeInfo: PRttiInfo; const aValue; Instance: TObject);
begin
  if LockAndDisableExceptions then
  try
    LogHeader(Level, Instance);
    fWriter.AddOnSameLine(pointer(aName));
    fWriter.AddDirect('=');
    fWriter.AddTypedJson(@aValue, aTypeInfo, [woDontStoreVoid]);
    LogTrailer(Level);
  finally
    fThreadInfo^.Flags := fThreadInfoBackup;
    GlobalThreadLock.UnLock;
  end;
end;

procedure TSynLog.ComputeFileName;

  function SetName(Args: array of const): boolean;
  var
    i: PtrInt;
  begin
    fFileName := MakeString([fFamily.fDestinationPath, MakeString(Args),
                             fFamily.fDefaultExtension]);
    result := false;
    for i := 0 to high(SynLogFile) do
      if (SynLogFile[i] <> self) and
         (AnsiCompareFileName(SynLogFile[i].fFileName, fFileName) = 0) then
        exit; // happens with multiple TSynLog classes
    result := true;
  end;

var
  hourRotate, beforeRotate: TDateTime;
  dup: integer;
  tix32: cardinal;
  fn: TFileName;
  classn: RawUtf8;
begin
  fn := fFamily.fCustomFileName;
  if fn = '' then
    // compute the default filename as '<exename>(<user>@<host>)'
    with Executable do
      if fFamily.IncludeComputerNameInFileName then
        if fFamily.IncludeUserNameInFileName then
          fn := FormatString('%(%@%)', [ProgramName, User, Host])
        else
          fn := FormatString('%(%)', [ProgramName, Host])
      else if fFamily.IncludeUserNameInFileName then
        fn := FormatString('%(%)', [ProgramName, User])
      else
        Utf8ToFileName(ProgramName, fn);
  // prepare for any file flush or rotation - as checked in OnFlushToStream
  fRotateBytes := 0;
  fFlushTix32 := 0;
  fRotateDailyTix32 := 0;
  tix32 := GetTickSec;
  if fFamily.AutoFlushTimeOut <> 0 then
    fFlushTix32 := tix32 + fFamily.AutoFlushTimeOut;
  if fFamily.fRotateFileCount > 0 then
  begin
    if fFamily.fRotateFileSizeKB > 0 then
      fRotateBytes := fFamily.fRotateFileSizeKB shl 10; // size KB -> B
    if fFamily.fRotateFileDailyAtHour in [0..23] then
    begin
      hourRotate := EncodeTime(fFamily.fRotateFileDailyAtHour, 0, 0, 0);
      beforeRotate := hourRotate - Time; // use local time hour
      if beforeRotate <= 1 / MinsPerDay then // hour passed, or within 1 minute
        beforeRotate := beforeRotate + 1; // trigger tomorrow
      fRotateDailyTix32 := tix32 + trunc(beforeRotate * SecsPerDay);
    end;
  end;
  // file name should include current timestamp if no rotation is involved
  if (fRotateBytes = 0) and
     (fRotateDailyTix32 = 0) then
    fn := FormatString('% %',
      [fn, NowToFileShort(fFamily.LocalTimestamp)]);
  {$ifdef OSWINDOWS}
  // include library name
  if IsLibrary and
     (fFamily.fCustomFileName = '') then
    fn := fn + ' ' + ExtractFileName(GetModuleName(HInstance));
  {$else}
  // normalize file name to be more readable and usable on POSIX command line
  fn := StringReplace(fn, ' ', '-', [rfReplaceAll]);
  {$endif OSWINDOWS}
  // include thread ID in ptOneFilePerThread mode
  if fFamily.fPerThreadLog = ptOneFilePerThread then
    fn := FormatString('% %',
      [fn, PointerToHexShort({%H-}pointer(GetCurrentThreadId))]);
  // include inherited TSynLog class name as suffix
  if PClass(self)^ <> TSynLog then
  begin
    classn := ToText(PClass(self)^);
    if IdemPChar(pointer(classn), 'TSYNLOG') then
      delete(classn, 1, 7)  // TSynLogSecondary -> 'secondary'
    else if classn[1] = 'T' then
      delete(classn, 1, 1); // TCustomLog -> 'customlog'
    LowerCaseSelf(classn);
    if SetName([fn, '-', classn]) then
      exit; // exename-secondary.log is not yet active
  end;
  // ensure this file name is unique among all opened files
  if SetName([fn]) then
    exit; // exename.log is not already used
  for dup := 2 to MAX_SYNLOGFAMILY + 3 do // absolute max = MAX_SYNLOGFAMILY = 7
    if SetName([fn, '-', dup]) then
      exit; // exename-#.log does not exist
  ESynLogException.RaiseUtf8('Duplicated %.FileName=%', [self, fFileName]);
end;

procedure TSynLog.CreateLogWriter;
begin
  if fWriterStream = nil then // may be set by overriden CreateLogWriter method
  begin
    // create fWriterStream instance
    ComputeFileName;
    if not fFamily.NoFile then
      // open write access to the .log file
      try
        case fFamily.FileExistsAction of
          acOverwrite:
            begin
              DeleteFile(fFileName);
              fWriterStream := TFileStreamNoWriteError.Create(
                                 fFileName, fmCreate or fmShareRead);
              exclude(fFlags, logFileHeaderWritten); // header for new file
            end;
          acAppend:
            begin
              fWriterStream :=
                TFileStreamNoWriteError.CreateAndRenameIfLocked(fFileName);
              if fWriterStream.Seek(0, soEnd) <> 0 then
                include(fFlags, logFileHeaderWritten); // write headers once
            end;
        end;
      except
        // continue if file creation fails (e.g. R/O folder or disk full)
      end;
    if fWriterStream = nil then
      fWriterStream := TFakeWriterStream.Create; // don't write anything
  end;
  // create fWriter instance
  if fWriter = nil then
  begin
    if fWriterClass = nil then // may be overriden by an inherited class
      fWriterClass := TJsonWriter; // mormot.core.json.pas is linked
    fWriter := fWriterClass.Create(fWriterStream, fFamily.BufferSize);
    fWriter.CustomOptions :=
      [twoEnumSetsAsTextInRecord, // debug-friendly text output
       twoFullSetsAsStar,
       twoForceJsonExtended];
    fWriter.FlushToStreamNoAutoResize := true; // stick to BufferSize
    fWriter.NoWriteToStreamException := true;  // if TFileStreamNoWriteError is not set
  end;
  // create fWriterEcho instance
  if fWriterEcho = nil then
    fWriterEcho := TEchoWriter.Create(fWriter);
  fWriterEcho.EndOfLineCRLF := fFamily.EndOfLineCRLF;
  if integer(fFamily.EchoToConsole) <> 0 then
    fWriterEcho.EchoAdd(ConsoleEcho);
  if Assigned(fFamily.EchoCustom) then
    fWriterEcho.EchoAdd(fFamily.EchoCustom);
  if Assigned(fFamily.fEchoRemoteClient) then
    fWriterEcho.EchoAdd(fFamily.fEchoRemoteEvent);
  fWriter.OnFlushToStream := OnFlushToStream; // note: overwrites fWriterEcho
  // enable background writing in its own TAutoFlushThread
  if fFamily.AutoFlushTimeOut <> 0 then
    fFamily.EnsureAutoFlushThreadRunning;
end;

procedure TSynLog.OnFlushToStream(Text: PUtf8Char; Len: PtrInt);
var
  secs, tix32: cardinal;
  bytes: PtrInt;
begin
  // compute the next idle timestamp for the background TAutoFlushThread
  tix32 := 0;
  secs := fFamily.AutoFlushTimeOut;
  if secs <> 0 then
  begin
    tix32 := GetTickSec;
    fFlushTix32 := tix32 + secs;
  end;
  // check for any PerformRotation - delayed in SetThreadInfoAndThreadName
  if not (pendingRotate in fPendingFlags) then
  begin
    bytes := fRotateBytes;
    if (bytes > 0) and // reached size to rotate?
       (fWriter.WrittenBytes + Len > bytes) then
      include(fPendingFlags, pendingRotate)
    else
    begin
      secs := fRotateDailyTix32;
      if secs <> 0 then // reached time to rotate?
      begin
        if tix32 = 0 then
          tix32 := GetTickSec;
        if tix32 >= secs then
          include(fPendingFlags, pendingRotate);
          // PerformRotation will call ComputeFileName to recompute DailyTix32
      end;
    end;
  end;
  // chain to the fWriterEcho process (otherwise Text/Len buffer is lost)
  fWriterEcho.FlushToStream(Text, Len);
end;

function TSynLog.GetFileSize: Int64;
begin
  result := 0;
  if SynLogFileFreeing or
     (fWriterStream = nil) then
    exit;
  GlobalThreadLock.Lock;
  try
    if fWriterStream <> nil then
      result := fWriterStream.Size;
  finally
    GlobalThreadLock.UnLock;
  end;
end;

{$ifdef FPC}

procedure TSynLog.AddStackTrace(Stack: PPtrUInt);
var
  frames: array[0..61] of pointer; // on Win64, RtlCaptureStackBackTrace < 62
  i, depth: PtrInt;
begin
  depth := fFamily.StackTraceLevel;
  if depth <> 0 then
    try
      fWriter.AddDirect(' ');
      for i := 0 to CaptureBacktrace(2, length(frames), @frames[0]) - 1 do
        if (i = 0) or
           (frames[i] <> frames[i - 1]) then
          if TDebugFile.Log(fWriter, PtrUInt(frames[i]),
               {notcode=}false, {assymbol=}false) then
          begin
            dec(depth);
            if depth = 0 then
              break;
          end;
      fWriter.CancelLastChar(' ');
    except // don't let any unexpected GPF break the logging process
    end;
end;

{$else not FPC}

procedure TSynLog.AddStackTrace(Stack: PPtrUInt);

{$ifdef CPU64}

  procedure AddStackManual(Stack: PPtrUInt);
  begin
    // not implemented yet
  end;

{$else}

  procedure AddStackManual(Stack: PPtrUInt); 

    function CheckAsmX86(xret: PtrUInt): boolean; // naive detection
    var
      i: PtrUInt;
    begin
      result := true;
      try
        if PByte(xret - 5)^ = $E8 then
          exit;
        for i := 2 to 7 do
          if PWord(xret - i)^ and $38FF = $10FF then
            exit;
      except
        // ignore any GPF
      end;
      result := false;
    end;

  var
    st, max_stack, min_stack, depth: PtrUInt;
    debug: TDebugFile;
  begin
    asm
        mov     min_stack, ebp
        mov     eax, fs:[4]
        mov     max_stack, eax
    end;
    if Stack = nil then // if no Stack pointer set, retrieve current one
      Stack := pointer(min_stack)
    else if PtrUInt(Stack) < min_stack then
      exit;
    debug := GetInstanceDebugFile;
    if (debug <> nil) and
       not debug.HasDebugInfo then
      // slow SeemsRealPointer/VirtualQuery validation if no .map info
      debug := nil;
    fWriter.Add(' ');
    depth := fFamily.StackTraceLevel;
    try
      while PtrUInt(Stack) < max_stack do
      begin
        st := Stack^;
        inc(Stack);
        if ((st >= min_stack) and  // on-stack pointer is no code
            (st <= max_stack)) or
           ((debug <> nil) and // faster than SeemsRealPointer/VirtualQuery
            not debug.IsCode(st)) or
           ((debug = nil) and
            not SeemsRealPointer(pointer(st - 8))) then
          continue;
        if CheckAsmX86(st) then
          if TDebugFile.Log(fWriter, st, false) then
          begin
            dec(depth);
            if depth = 0 then
              break;
          end;
      end;
    except
      // just ignore any access violation here
    end;
  end;

{$endif CPU64}

var
  n, i, logged: integer;
  {$ifndef NOEXCEPTIONINTERCEPT}
  bak: TSynLogThreadInfoFlags; // paranoid precaution
  threadflags: ^TSynLogThreadInfoFlags;
  {$endif NOEXCEPTIONINTERCEPT}
  {$ifdef OSWINDOWS}
  BackTrace: array[byte] of PtrUInt;
  {$endif OSWINDOWS}
begin
  if fFamily.StackTraceLevel <= 0 then
    exit;
  {$ifndef NOEXCEPTIONINTERCEPT}
  threadflags := @PerThreadInfo.Flags;
  bak := threadflags^;
  include(threadflags^, tiExceptionIgnore);
  {$endif NOEXCEPTIONINTERCEPT}
  try
    {$ifdef OSWINDOWS}
    logged := 0;
    if fFamily.StackTraceUse <> stOnlyManual then
    begin
      n := RtlCaptureStackBackTrace(2, fFamily.StackTraceLevel, @BackTrace, nil);
      if n <> 0 then
      begin
        fWriter.AddDirect(' ');
        for i := 0 to n - 1 do
          if TDebugFile.Log(fWriter, BackTrace[i], false) then
            inc(logged);
      end;
    end;
    if (logged < 2) and
       (fFamily.StackTraceUse <> stOnlyAPI) then
      AddStackManual(stack);
    {$endif OSWINDOWS}
  except
    // just ignore any access violation here
  end;
  {$ifndef NOEXCEPTIONINTERCEPT}
  threadflags^ := bak;
  {$endif NOEXCEPTIONINTERCEPT}
end;

{$endif FPC}


{ ************** High-Level Logs and Exception Related Features }

{$ifndef NOEXCEPTIONINTERCEPT}

procedure DoLogException(Log: TSynLog; Info: PSynLogThreadInfo;
  const Ctxt: TSynLogExceptionContext);
begin // called by SynLogException() within its GlobalThreadLock.Lock
  if (Log = nil) or
     (Log.fWriter = nil) then
    exit; // this TSynLogFamily has no fGlobalLog or opened file (yet)
  Log.FillInfo(Info, nil); // timestamp [+ threadnumber]
  SetThreadInfoAndThreadName(Log, Info);
  LogHeaderNoRecursion(Log.fWriter, Ctxt.ELevel, @Info^.CurrentTimeAndThread);
  DefaultSynLogExceptionToStr(Log.fWriter, Ctxt, {addinfo=}false);
  // stack trace only in the main thread
  Log.fWriterEcho.AddEndOfLine(Ctxt.ELevel);
end;

const
  MAX_EXCEPTHISTORY = 15;

type
  TSynLogExceptionInfos = array[0 .. MAX_EXCEPTHISTORY] of TSynLogExceptionInfo;
  TLastException = record
    Index: integer;
    StackCount: integer;
    Infos: TSynLogExceptionInfos;
    Stack: array[0 .. MAX_EXCEPTHISTORY - 1] of PtrUInt;
  end;

var
  // some static information about the latest exceptions raised
  GlobalLastException: TLastException = (
    Index: -1{%H-});

// this is the main entry point for all intercepted exceptions
procedure SynLogException(const Ctxt: TSynLogExceptionContext);
var
  fam: TSynLogFamily;
  log: TSynLog;
  nfo: PSynLogThreadInfo;
  info: ^TSynLogExceptionInfo;
  thrdnam: PShortString;
  last: ^TLastException;
  i, n: PtrInt;
  {$ifdef FPC}
  curr, prev: PtrUInt;
  {$endif FPC}
label
  adr, fin;
begin
  if (HandleExceptionFamily = nil) or // no TSynLogFamily.fHandleExceptions set
     SynLogFileFreeing or             // inconsistent call at shutdown
     (Ctxt.EClass = ESynLogSilent) or
     HandleExceptionFamily.ExceptionIgnore.Exists(Ctxt.EClass) then
    exit;
  {$ifdef WIN64DELPHI} // Delphi<XE6 in System.pas to retrieve x64 dll exit code
  {$ifndef ISDELPHIXE6}
  if (Ctxt.EInstance <> nil) and // Ctxt.EClass is EExternalException
     (PShortString(PPointer(PPtrInt(Ctxt.EInstance)^ + vmtClassName)^)^ =
      '_TExitDllException') then
    exit;
  {$endif ISDELPHIXE6}
  {$endif WIN64DELPHI}
  nfo := @PerThreadInfo;
  if tiExceptionIgnore in nfo^.Flags then
    exit; // disabled for this thread (avoid nested call)
  log := HandleExceptionFamily.Add;
  if log = nil then
   exit;
  thrdnam := CurrentThreadNameShort;
  log.LockAndDisableExceptions; // ignore result = tiTemporaryDisable flag
  try
    try
      // ensure we need to log this
      if Assigned(log.fFamily.OnBeforeException) then
        if log.fFamily.OnBeforeException(Ctxt, thrdnam^) then
          exit; // intercepted by custom callback
      // memorize last exceptions into an internal round-robin static list
      last := @GlobalLastException;
      if last^.Index = high(last^.Infos) then
        last^.Index := 0
      else
        inc(last^.Index);
      info := @last^.Infos[last^.Index];
      info^.Context := Ctxt;
      info^.Message := '';
      if Ctxt.EStack = nil then
        last^.StackCount := 0
      else
      begin
        n := Ctxt.EStackCount;
        if n > high(last^.Stack) + 1 then
          n := high(last^.Stack) + 1;
        last^.StackCount := n;
        MoveFast(Ctxt.EStack[0], last^.Stack[0], n * SizeOf(PtrUInt));
      end;
      // actual exception log - with potential customization
      LogHeaderNoRecursion(log.fWriter, Ctxt.ELevel, @nfo^.CurrentTimeAndThread);
      if (Ctxt.ELevel = sllException) and
         (Ctxt.EInstance <> nil) then
      begin
        info^.Message := Ctxt.EInstance.Message;
        if Ctxt.EInstance.InheritsFrom(ESynException) then
        begin
          ESynException(Ctxt.EInstance).RaisedAt := pointer(Ctxt.EAddr);
          if ESynException(Ctxt.EInstance).CustomLog(log.fWriter, Ctxt) then
            goto fin;
          goto adr; // CustomLog() includes DefaultSynLogExceptionToStr()
        end;
      end;
      if DefaultSynLogExceptionToStr(log.fWriter, Ctxt, {addinfo=}true) then
        goto fin;
adr:  // regular exception context log with its stack trace
      log.fWriter.AddDirect(' ', '['); // fThreadContext^.ThreadName may be ''
      log.fWriter.AddShort(thrdnam^);
      log.fWriter.AddShorter('] at ');
      try
        TDebugFile.Log(log.fWriter, Ctxt.EAddr, {notcode=}true, {symbol=}false);
        {$ifdef FPC}
        prev := Ctxt.EAddr;
        // we rely on the stack trace supplied by the FPC RTL
        for i := 0 to Ctxt.EStackCount - 1 do
        begin
          curr := Ctxt.EStack[i];
          if curr = prev then
            continue; // don't log twice
          TDebugFile.Log(log.fWriter, curr, {notcode=}false, {symbol=}false);
          prev := curr;
        end;
        {$else}
        {$ifdef CPUX86}
        // stack frame OK only for RTLUnwindProc by now
        log.AddStackTrace(pointer(Ctxt.EStack));
        {$endif CPUX86}
        {$endif FPC}
      except // paranoid
      end;
fin:  if Ctxt.ELevel in log.fFamily.fLevelSysInfo then
        log.AddSysInfo;
      log.fWriterEcho.AddEndOfLine(Ctxt.ELevel);
      log.fWriter.FlushToStream; // exceptions available on disk ASAP
      // minimal exception logging to all other TSynLog files (to ease debug)
      for i := 0 to high(SynLogFamily) do
      begin
        fam := SynLogFamily[i];
        if (fam <> HandleExceptionFamily) and // if not already logged above
           (Ctxt.ELevel in fam.Level) then
        try
          DoLogException(fam.fGlobalLog, nfo, Ctxt);
        except
          // paranoid: don't try this family again (without SetLevel)
          fam.fLevel := fam.fLevel - [sllException, sllExceptionOS];
        end;
      end;
    except
      // any nested exception should never be propagated to the OS caller
    end;
  finally
    nfo^.Flags := log.fThreadInfoBackup;
    GlobalThreadLock.UnLock;
  end;
end;

function GetLastException(out info: TSynLogExceptionInfo): boolean;
begin
  result := false;
  if SynLogFileFreeing or
     (GlobalLastException.Index < 0) then
    exit; // no exception intercepted yet (or any more)
  GlobalThreadLock.Lock;
  try
    if GlobalLastException.Index < 0 then
      exit;
    info := GlobalLastException.Infos[GlobalLastException.Index]; // copy
  finally
    GlobalThreadLock.UnLock;
  end;
  info.Context.EInstance := nil; // avoid any GPF
  info.Context.EStack := @GlobalLastException.Stack;
  info.Context.EStackCount := GlobalLastException.StackCount;
  result := info.Context.ELevel <> sllNone;
end;

procedure GetLastExceptions(out result: TSynLogExceptionInfoDynArray;
  Depth: integer);
var
  infos: TSynLogExceptionInfos; // use thread-safe local copy of static array
  index, last, n, i: PtrInt;
begin
  // thread-safe retrieve last exceptions
  if SynLogFileFreeing or
     (GlobalLastException.Index < 0) then
    exit; // no exception intercepted yet (or any more)
  GlobalThreadLock.Lock;
  try
    infos := GlobalLastException.Infos;
    index := GlobalLastException.Index;
  finally
    GlobalThreadLock.UnLock;
  end;
  // generate an ordered array of exception infos
  n := MAX_EXCEPTHISTORY + 1;
  if (Depth > 0) and
     (n > Depth) then
    n := Depth;
  SetLength(result, n);
  last := MAX_EXCEPTHISTORY;
  for i := 0 to n - 1 do
  begin
    if i <= index then
      result[i] := infos[index - i]
    else
    begin
      result[i] := infos[last];
      dec(last);
    end;
    with result[i].Context do
      if ELevel = sllNone then
      begin
        SetLength(result, i); // truncate to latest available exception
        break;
      end
      else
      begin
        EInstance := nil; // avoid any GPF
        if i = 0 then
        begin
          EStack := @GlobalLastException.Stack; // static copy of last exception
          EStackCount := GlobalLastException.StackCount;
        end
        else
          EStack := nil; // avoid any GPF
      end;
  end;
end;

function ToText(var info: TSynLogExceptionInfo): RawUtf8;
var
  i: PtrInt;
begin
  with info.Context do
    if ELevel <> sllNone then
    begin
      FormatUtf8('% % at %: % [%]', [_LogInfoCaption[ELevel], EClass,
        GetInstanceDebugFile.FindLocationShort(EAddr),
        UnixTimeToString(ETimestamp, {expanded=}true, ' '),
        StringToUtf8(info.Message)], result);
      if EStack <> nil then
        for i := 0 to EStackCount - 1 do
          result := FormatUtf8('%, %',
            [result, ExeInstanceDebugFile.FindLocationShort(EStack[i])]);
    end
    else
      result := '';
end;

function GetLastExceptionText: RawUtf8;
var
  info: TSynLogExceptionInfo;
begin
  if GetLastException(info) then
    result := ToText(info)
  else
    result := '';
end;

{$endif NOEXCEPTIONINTERCEPT}


{ TSynLogCallbacks }

constructor TSynLogCallbacks.Create(aTrackedLog: TSynLogFamily);
begin
  inherited Create;
  Registrations.Init(TypeInfo(TSynLogCallbackDynArray), Registration, @fCount);
  TrackedLog := aTrackedLog;
  aTrackedLog.EchoRemoteStart(self, OnEcho, false);
end;

destructor TSynLogCallbacks.Destroy;
begin
  if TrackedLog <> nil then
    if TrackedLog.fEchoRemoteClient = self then
      TrackedLog.EchoRemoteStop; // unregister OnEcho() event
  inherited Destroy;
end;

function TSynLogCallbacks.OnEcho(Sender: TEchoWriter; Level: TSynLogLevel;
  const Text: RawUtf8): boolean;
var
  i: PtrInt;
  cb: ^TSynLogCallback;
begin
  result := false;
  if (Count = 0) or
     fCurrentlyEchoing then
    exit;
  fSafe.Lock; // not really concurrent, but faster
  try
    fCurrentlyEchoing := true; // avoid stack overflow if exception below
    cb := pointer(Registration);
    for i := Count - 1 downto 0 do
      if Level in cb^.Levels then
      try
        cb^.Callback.Log(Level, Text);
        result := true;
        inc(cb);
      except
        try
          Registrations.Delete(i); // safer to unsubscribe ASAP
        except
          result := false;
        end;
        cb := @Registration[i];  // may have moved in memory
      end;
  finally
    fCurrentlyEchoing := false;
    fSafe.UnLock;
  end;
end;

function TSynLogCallbacks.Subscribe(const Levels: TSynLogLevels;
  const Callback: ISynLogCallback; ReceiveExistingKB: cardinal): integer;
var
  reg: TSynLogCallback;
  previousContent: RawUtf8;
begin
  if Assigned(Callback) then
  try
    if ReceiveExistingKB > 0 then
    begin
      GlobalThreadLock.Lock;
      previousContent := TrackedLog.GetExistingLog(ReceiveExistingKB);
      if TrackedLog.HighResolutionTimestamp and
         (TrackedLog.fGlobalLog <> nil) then
        with TrackedLog.fGlobalLog do
          Callback.Log(sllNone, FormatUtf8('freq=%,%,%',
            [1000000, double(fStartTimestampDateTime), fFileName]));
      Callback.Log(sllNone, previousContent);
    end;
    reg.Levels := Levels;
    reg.Callback := Callback;
    fSafe.Lock;
    try
      Registrations.Add(reg);
    finally
      fSafe.UnLock;
    end;
  finally
    if ReceiveExistingKB > 0 then
      GlobalThreadLock.UnLock;
  end;
  result := length(previousContent);
end;

procedure TSynLogCallbacks.Unsubscribe(const Callback: ISynLogCallback);
var
  i: PtrInt;
begin
  fSafe.Lock;
  try
    for i := Count - 1 downto 0 do
      if Registration[i].Callback = Callback then
        Registrations.Delete(i);
  finally
    fSafe.UnLock;
  end;
end;


{ TSynLogSettings }

constructor TSynLogSettings.Create;
begin
  inherited Create; // may have been overriden
  fDestinationPath := GetSystemPath(spLog);
  fLevels := LOG_STACKTRACE + [sllNewRun];
  fRotateFileCount := 2;
end;

procedure TSynLogSettings.SetLog(aLogClass: TSynLogClass);
var
  f: TSynLogFamily;
begin
  if self = nil then
    exit;
  if aLogClass = nil then
    aLogClass := TSynLog;
  f := aLogClass.Family;
  f.DestinationPath := EnsureDirectoryExists(fDestinationPath);
  f.PerThreadLog := ptIdentifiedInOneFile; // ease multi-threaded server debug
  f.RotateFileCount := fRotateFileCount;
  if fRotateFileCount > 0 then
  begin
    f.RotateFileSizeKB := 20 * 1024; // rotate by 20 MB logs
    f.FileExistsAction := acAppend;  // as expected in rotation mode
  end
  else
    f.HighResolutionTimestamp := true;
  f.Level := fLevels;
  fLogClass := aLogClass;
end;


function _LogCompressAlgoArchive(aAlgo: TAlgoCompress; aMagic: cardinal;
  const aOldLogFileName, aDestinationPath: TFileName): boolean;
var
  folder, dest, ext: TFileName;
  fsize: Int64;
  ftime: TUnixMSTime;
  n: integer;
begin
  result := false;
  if (aOldLogFileName = '') or // last call is always with ''
     not FileInfoByName(aOldLogFileName, fsize, ftime) or
     (fsize < 0) then
    // old log file does not exist (or is a folder)
    exit
  else if fsize = 0 then
    // just delete a void .log file (not from TSynLog, but supported anyway)
    result := DeleteFile(aOldLogFileName)
  else
  try
    // dest = 'ArchivePath\log\YYYYMM\yyyymmddhhmmss.log.synlz/synliz'
    folder := EnsureDirectoryExists(aDestinationPath);
    if aAlgo <> nil then
      ext := aAlgo.AlgoFileExt;
    n := 100;
    repeat
      dest := FormatString('%%.log%', [folder, UnixMSTimeToFileShort(ftime), ext]);
      if not FileExists(dest) then
        break;
      inc(ftime, MilliSecsPerSec); // ensure unique
      dec(n);
      if n = 0 then // paranoid
        ESynLogException.RaiseU('LogCompressAlgoArchive infinite loop');
    until false;
    // compress or copy the old file, then delete it
    if (aAlgo = nil) or // no compression
       aAlgo.FileIsCompressed(aOldLogFileName, aMagic) then // already compressed
      result := RenameFile(aOldLogFileName, dest) or
                (CopyFile(aOldLogFileName, dest, false) and
                 DeleteFile(aOldLogFileName))
    else if aAlgo.FileCompress(aOldLogFileName, dest, aMagic, {hash32=}true) then
      result := DeleteFile(aOldLogFileName);
  except
    on Exception do
      DeleteFile(aOldLogFileName);
  end;
end;

function EventArchiveDelete(
  const aOldLogFileName, aDestinationPath: TFileName): boolean;
begin
  result := DeleteFile(aOldLogFileName);
end;

function EventArchiveSynLZ(
  const aOldLogFileName, aDestinationPath: TFileName): boolean;
begin
  // compress and delete the file
  result := LogCompressAlgoArchive(
    AlgoSynLZ, LOG_MAGIC, aOldLogFileName, aDestinationPath);
end;


{ ************** Efficient .log File Access via TSynLogFile }

{ TSynLogFile }

constructor TSynLogFile.Create;
var
  L: TSynLogLevel;
begin
  for L := low(TSynLogLevel) to high(TSynLogLevel) do
    // LOG_LEVEL_TEXT[L][3] -> case-sensitive lookup e.g. 'ust4' chars
    fLogLevelsTextMap[L] := PCardinal(@LOG_LEVEL_TEXT[L][3])^;
  // minimal good-enough size for thread info or per-thread profiling
  SetLength(fThreadInfo, 256);
  SetLength(fLogProcStack, 256);
  SetLength(fLogProcStackCount, 256);
end;

function TSynLogFile.GetLogLevelFromText(LineBeg: PUtf8Char): TSynLogLevel;
begin // very fast lookup, using SSE2 on Intel/AMD
  result := TSynLogLevel(IntegerScanIndex(@fLogLevelsTextMap[succ(sllNone)],
         ord(high(TSynLogLevel)), PCardinal(LineBeg + fLineLevelOffset)^) + 1);
end;

function TSynLogFile.EventCount(const aSet: TSynLogLevels): integer;
var
  i: PtrInt;
begin
  result := 0;
  if integer(aSet) <> 0 then
    for i := 0 to Count - 1 do
      if fLevels[i] in aSet then
        inc(result);
end;

function TSynLogFile.LineContains(const aUpperSearch: RawUtf8;
  aIndex: integer): boolean;
begin
  if (self = nil) or
     (cardinal(aIndex) >= cardinal(fCount)) or
     (aUpperSearch = '') then
    result := false
  else
    result := GetLineContains(PUtf8Char(fLines[aIndex]) + fLineTextOffset,
      fMapEnd, pointer(aUpperSearch));
end;

function TSynLogFile.EventDateTime(aIndex: integer): TDateTime;
var
  hires: Int64;
  P: PUtf8Char;
  Y, M, D, HH, MM, SS, MS4: cardinal;
  hex2bin: PByteArray;
begin
  result := 0;
  if cardinal(aIndex) >= cardinal(fCount) then
    exit;
  P := fLines[aIndex];
  if fFreq = 0 then
  begin
    hex2bin := @ConvertHexToBin;
    if Char4ToWord(P,      Y,   hex2bin) or
       Char2ToByte(P + 4,  M,   hex2bin) or
       Char2ToByte(P + 6,  D,   hex2bin) or
       Char2ToByte(P + 9,  HH,  hex2bin) or
       Char2ToByte(P + 11, MM,  hex2bin) or
       Char2ToByte(P + 13, SS,  hex2bin) or
       Char2ToByte(P + 15, MS4, hex2bin) then
      // not exact YYYYMMDD hhmmsszz layout -> try plain ISO-8601
      Iso8601ToDateTimePUtf8CharVar(P, 17, result)
    else
      // MS4 shl 4 = 16 ms resolution in TTextWriter.AddCurrentLogTime()
      result := EncodeDateTime(Y, M, D, HH, MM, SS, MS4 shl 4);
  end
  else if HexDisplayToBin(PAnsiChar(P), @hires, SizeOf(hires)) then
    result := fStartDateTime + (hires / fFreqPerDay);
end;

procedure TSynLogFile.CleanLevels;
var
  i, n, p, d, dChange, dMax: PtrInt;
  sll: TSynLogLevel;
begin
  n := 0;
  p := 0;
  d := 0;
  dMax := Length(fDayChangeIndex);
  if dMax > 0 then
    dChange := fDayChangeIndex[0]
  else
    dChange := -1;
  for i := 0 to fCount - 1 do
  begin
    sll := fLevels[i];
    if sll = sllNone then // just ignore any recognized line
      continue;
    fLevels[n] := sll;
    fLines[n]  := fLines[i];
    if fThreads <> nil then
      fThreads[n] := fThreads[i];
    if sll = sllEnter then
    begin
      fLogProcNatural[p].Index := n;
      inc(p);
    end;
    if dChange = i then
    begin
      fDayChangeIndex[d] := n;
      inc(d);
      if d < dMax then
        dChange := fDayChangeIndex[d];
    end;
    inc(n);
  end;
  fCount := n;
  assert(p = fLogProcNaturalCount);
  if dMax > 0 then
  begin
    SetLength(fDayCount, dMax);
    dec(dMax);
    for i := 0 to dMax - 1 do
      fDayCount[i] := fDayChangeIndex[i + 1] - fDayChangeIndex[i];
    fDayCount[dMax] := n - fDayChangeIndex[dMax];
  end;
end;

procedure TSynLogFile.RecomputeTime(p: PSynLogFileProc);
var
  ndx, lev: PtrInt;
  enter64, leave64: Int64;
  thd: cardinal;
begin // only called when out-of-range '99.xxx.xxx' was written in sllLeave
  lev := 0;
  ndx := p^.Index;
  if fThreads <> nil then
    thd := fThreads[ndx] // will only check sllEnter/sllLeave in this thread
  else
    thd := 0;
  repeat
    inc(ndx);
    if ndx = fCount then
      break;
    if (thd = 0) or
       (fThreads[ndx] = thd) then
      case fLevels[ndx] of
        sllEnter:
          inc(lev);
        sllLeave:
          if lev = 0 then // compute proper p^.Time from nested calls
          begin
            if fFreq = 0 then
              // adjust huge seconds timing from date/time column
              p^.Time := Round(
                (EventDateTime(ndx) -
                 EventDateTime(p^.Index)) * 86400000000.0) +
                p^.Time mod 1000000
            else
            begin
              // directly use high resolution timestamps as 64-bit integers
              HexDisplayToBin(fLines[p^.Index], @enter64, SizeOf(enter64));
              HexDisplayToBin(fLines[ndx],      @leave64, SizeOf(leave64));
              p^.Time := ((leave64 - enter64) * (1000 * 1000)) div fFreq;
            end;
            break;
          end
          else
            dec(lev);
      end;
  until false;
end;

function TSynLogFile.ComputeProperTime(start: PSynLogFileProc): PSynLogFileProc;
var
  ndx: PtrInt;
  thd: cardinal;
begin
  result := start;
  result^.ProperTime := result^.Time;
  ndx := result^.Index;
  if fThreads <> nil then
    thd := fThreads[ndx] // will only check sllEnter/sllLeave in this thread
  else
    thd := 0;
  repeat
    inc(ndx);
    if ndx = fCount then
      break;
    if (thd = 0) or
       (fThreads[ndx] = thd) then
      case fLevels[ndx] of
        sllEnter:
          begin
            inc(result);
            result := ComputeProperTime(result);
          end;
        sllLeave:
          begin
            while PtrUInt(result) > PtrUInt(start) do
            begin
              if (thd = 0) or
                 (fThreads[result^.Index] = thd) then
                dec(start^.ProperTime, result^.ProperTime);
              dec(result);
            end;
            break;
          end;
      end;
  until false;
end;

function StrPosILen(P, PEnd: PUtf8Char; SearchUp: PAnsiChar): PUtf8Char;
var
  tab: PNormTable;
begin
  result := P;
  tab := @NormToUpperAnsi7;
  while result < PEnd do
    if IdemPChar(result, SearchUp, tab) then
      exit
    else
      inc(result);
  result := nil;
end;

procedure TSynLogFile.LoadFromMap(AverageLineLength: integer);
var
  PBeg, P, PEnd: PUtf8Char;

  function GetOne(const UP: RawUtf8; var S: RawUtf8): boolean;
  var
    LUP: integer;
  begin
    LUP := length(UP);
    P := StrPosILen(PBeg, PEnd - LUP, pointer(UP));
    if P = nil then
      result := false
    else
    begin
      FastSetString(S, PBeg, P - PBeg);
      PBeg := P + LUP;
      result := pointer(S) <> nil;
    end;
  end;

var
  aWow64, feat: RawUtf8;
  f: PAnsiChar;
  i: PtrInt;
  fp, fpe: PSynLogFileProc;
  OK: boolean;
begin
  // 1. calculate fLines[] + fCount and fLevels[] + fLogProcNatural[] from .log content
  fLineHeaderCountToIgnore := 3;
  // call ProcessOneLine() in one pass
  inherited LoadFromMap(100);
  // cleanup transient working arrays memory
  fLogProcStack := nil;
  fLogProcStackCount := nil;
  // 2. fast retrieval of header
  OK := false;
  try
    {  C:\Dev\lib\SQLite3\exe\TestSQL3.exe 0.0.0.0 (2011-04-07 11:09:06)
    Host=BW013299 User=G018869 CPU=1*0-15-1027 OS=2.3=5.1.2600 Wow64=0 Freq=3579545
    TSynLog 1.13 2011-04-07 12:04:09 }
    if (fCount <= fLineHeaderCountToIgnore) or
       LineSizeSmallerThan(0, 24) or
       not IdemPChar(fLines[1], 'HOST=') or
       (fLevels = nil) or
       (fLineLevelOffset = 0) then
      exit;
    PBeg := fLines[0];
    PEnd := PBeg + LineSize(0) - 12;
    if PEnd < PBeg then
      exit;
    if PEnd^ = '(' then
    begin
      // '(2011-04-07)' format
      if (PEnd[-1] <> ' ') or
         (PEnd[0] <> '(') or
         (PEnd[11] <> ')') then
        exit;
      Iso8601ToDateTimePUtf8CharVar(PEnd + 1, 10, fExeDate);
    end
    else
    begin
      // '(2011-04-07 11:09:06)' format
      dec(PEnd, 9);
      if (PEnd < PBeg) or
         (PEnd[-1] <> ' ') or
         (PEnd[0] <> '(') or
         (PEnd[20] <> ')') then
        exit;
      Iso8601ToDateTimePUtf8CharVar(PEnd + 1, 19, fExeDate);
    end;
    dec(PEnd);
    P := PEnd;
    repeat
      if P <= PBeg then
        exit
      else
        dec(P)
    until P^ = ' ';
    FastSetString(fExeVersion, P + 1, PEnd - P - 1);
    repeat
      dec(P);
      if P <= PBeg then
        exit;
    until P^ <> ' ';
    FastSetString(fExeName, PBeg, P - PBeg + 1);
    PBeg := PUtf8Char(fLines[1]) + 5;
    PEnd := PUtf8Char(fLines[1]) + LineSize(1);
    if not GetOne(' USER=', fHost) or
       not GetOne(' CPU=', fUser) or
       not GetOne(' OS=', fCPU) or
       not GetOne(' WOW64=', fOsDetailed) or
       not GetOne(' FREQ=', aWow64) then
      exit;
    Split(fCPU, ':', fCpu, feat);
    f := pointer(feat);
    if f <> nil then
      // HexToBin() stops decoding at ' ' so AddBinToHexMinChars()-truncated or
      // old/smaller T*CpuFeatures members will be left filled with 0
      case f^ of
        '-': // ARM32 marker
          mormot.core.text.HexToBin(f + 1, @fArm32CPU, SizeOf(fArm32CPU));
        '+': // AARCH64 marker
          mormot.core.text.HexToBin(f + 1, @fArm64CPU, SizeOf(fArm64CPU));
      else
        mormot.core.text.HexToBin(f, @fIntelCPU, SizeOf(fIntelCPU));
      end;
    i := GetInteger(pointer(aWow64)); // 0, 1, 2 or 3
    fWow64 := (i and 1) <> 0;
    fWow64Emulated := (i and 2) <> 0; // + ord(IsWow64Emulation) shl 1
    SetInt64(PBeg, fFreq);
    while (PBeg < PEnd) and
          (PBeg^ > ' ') do
      inc(PBeg);
    if IdemPChar(PBeg, ' INSTANCE=') then // only available for a library log
      FastSetString(fInstanceName, PBeg + 10, PEnd - PBeg - 10);
    fHeaderLinesCount := 4;
    while fHeaderLinesCount < fCount do
    begin
      if PAnsiChar(fLines[fHeaderLinesCount - 1])^ < ' ' then
        break; // end of header = void line
      inc(fHeaderLinesCount);
    end;
    if (LineSize(fHeaderLinesCount - 1) <> 0) or
       LineSizeSmallerThan(fHeaderLinesCount, 16) then
      exit;
    if fHeaderLinesCount <> 4 then
      FastSetString(fHeaders, fLines[2],
        PtrUInt(fLines[fHeaderLinesCount - 2]) - PtrUInt(fLines[2]));
    if PWord(fLines[fHeaderLinesCount])^ <> $3030 then
      // YYYYMMDD -> 20101225 e.g. fFreq=0 if date time,
      fFreq := 0
    else
      // fFreq>0 if high-resolution time stamp
      fFreqPerDay := fFreq * SecsPerDay;
    P := pointer(fOSDetailed);
    fOS := TWindowsVersion(GetNextItemCardinal(P, '.'));
    if fOS > high(fOs) then
     fOS := wUnknown
    else if fOS <> wUnknown then
      fOSServicePack := GetNextItemCardinal(P);
    P := fLines[fHeaderLinesCount - 2]; // TSqlLog 1.18.2765 ERTL FTS3 2016-07-17T22:38:03
    i := LineSize(fHeaderLinesCount - 2) - 19; // length('2016-07-17T22:38:03')=19
    if i > 0 then
    begin
      FastSetString(fFramework, P, i - 1);
      Iso8601ToDateTimePUtf8CharVar(P + i, 19, fStartDateTime);
    end;
    if fStartDateTime = 0 then
      exit;
    // 3. compute fCount and fLines[] so that all fLevels[]<>sllNone
    CleanLevels;
    if Length(fLevels) - fCount > 16384 then
    begin
      // size down only if worth it
      SetLength(fLevels, fCount);
      if fThreads <> nil then
      begin
        SetLength(fThreads, fCount);
        SetLength(fThreadInfo, fThreadMax + 1);
      end;
    end;
    // 4. compute customer-side profiling
    SetLength(fLogProcNatural, fLogProcNaturalCount); // exact resize
    fp := pointer(fLogProcNatural);
    fpe := @fLogProcNatural[fLogProcNaturalCount];
    while PtrUInt(fp) < PtrUInt(fpe) do
    begin
      if fp^.Time >= 99000000 then
        // 99.xxx.xxx means over range -> compute fp^.Time from nested calls
        RecomputeTime(fp);
      inc(fp);
    end;
    fp := pointer(fLogProcNatural);
    while PtrUInt(fp) < PtrUInt(fpe) do
    begin
      fp := ComputeProperTime(fp);
      inc(fp);
    end;
    LogProcMerged := false; // set LogProp[]
    OK := true;
  finally
    if not OK then
    begin
      Finalize(fLevels); // mark not a valid .log
      Finalize(fThreads);
      fLineLevelOffset := 0;
    end;
  end;
end;

procedure TSynLogFile.AddInMemoryLine(const aNewLine: RawUtf8);
var
  P: PUtf8Char;
begin
  if aNewLine = '' then
    exit;
  P := pointer(aNewLine);
  if (PInteger(P)^ =
      ord('f') + ord('r') shl 8 + ord('e') shl 16 + ord('q') shl 24) and
     (P[4] = '=') then
  begin
    inc(P, 5);
    fFreq := GetNextItemInt64(P);
    fFreqPerDay := fFreq * SecsPerDay;
    fStartDateTime := GetNextItemDouble(P);
    Utf8DecodeToString(P, StrLen(P), string(fFileName));
  end
  else
    inherited AddInMemoryLine(aNewLine);
end;

procedure TSynLogFile.LogProcSort(Order: TLogProcSortOrder);
begin
  if (fLogProcNaturalCount <= 1) or
     (Order = fLogProcSortInternalOrder) then
    exit;
  fLogProcSortInternalOrder := Order;
  case Order of
    soByName:
      fLogProcSortInternalComp := LogProcSortCompByName;
    soByOccurrence:
      fLogProcSortInternalComp := LogProcSortCompByOccurrence;
    soByTime:
      fLogProcSortInternalComp := LogProcSortCompByTime;
    soByProperTime:
      fLogProcSortInternalComp := LogProcSortCompByProperTime;
  else
    fLogProcSortInternalComp := LogProcSortCompDefault;
  end;
  LogProcSortInternal(0, fLogProcCurrentCount - 1);
  fLogProcSortInternalComp := nil;
end;

function StrICompLeftTrim(Str1, Str2: PUtf8Char): PtrInt;
var
  C1, C2: integer;
  tab: PByteArray;
begin
  while Str1^ in [#9, ' '] do
    inc(Str1);
  while Str2^ in [#9, ' '] do
    inc(Str2);
  tab := @NormToUpperByte;
  repeat
    C1 := tab[ord(Str1^)];
    C2 := tab[ord(Str2^)];
    if (C1 <> C2) or
       (C1 < 32) then
      break;
    inc(Str1);
    inc(Str2);
  until false;
  result := C1 - C2;
end;

function TSynLogFile.LogProcSortCompByName(A, B: PtrInt): PtrInt;
begin
  result := StrICompLeftTrim(
    PUtf8Char(fLines[LogProc[A].Index]) + fLineTextOffset,
    PUtf8Char(fLines[LogProc[B].Index]) + fLineTextOffset);
end;

function TSynLogFile.LogProcSortCompByOccurrence(A, B: PtrInt): PtrInt;
begin
  result := LogProc[A].Index - LogProc[B].Index;
end;

function TSynLogFile.LogProcSortCompByTime(A, B: PtrInt): PtrInt;
begin
  result := LogProc[B].Time - LogProc[A].Time;
end;

function TSynLogFile.LogProcSortCompByProperTime(A, B: PtrInt): PtrInt;
begin
  result := LogProc[B].ProperTime - LogProc[A].ProperTime;
end;

function TSynLogFile.LogProcSortCompDefault(A, B: PtrInt): PtrInt;
begin
  result := A - B;
end;

procedure LogProcSortExchg(var P1, P2: TSynLogFileProc);
  {$ifdef HASINLINE}inline;{$endif}
var
  c: TSynLogFileProc;
begin
  c := P1;
  P1 := P2;
  P2 := c;
end;

procedure TSynLogFile.LogProcSortInternal(L, R: PtrInt);
var
  I, J, P: PtrInt;
begin
  if L < R then
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      repeat
        while fLogProcSortInternalComp(I, P) < 0 do
          inc(I);
        while fLogProcSortInternalComp(J, P) > 0 do
          dec(J);
        if I <= J then
        begin
          LogProcSortExchg(LogProc[I], LogProc[J]);
          if P = I then
            P := J
          else if P = J then
            P := I;
          inc(I);
          dec(J);
        end;
      until I > J;
      if J - L < R - I then // use recursion only for smaller range
      begin
        if L < J then
          LogProcSortInternal(L, J);
        L := I;
      end
      else
      begin
        if I < R then
          LogProcSortInternal(I, R);
        R := J;
      end;
    until L >= R;
end;

function DecodeMicroSec(P: PByte): integer;
var
  B: integer;
  tab: PByteArray;
begin
  // fast decode 00.020.006 at the end of the line
  tab := @ConvertHexToBin;
  B := tab[P^];   // 00
  if B > 9 then
    result := -1
  else
  begin
    result := B;
    inc(P);
    B := tab[P^];
    if B > 9 then
      result := -1
    else
    begin
      result := result * 10 + B;
      inc(P, 2);    // .
      B := tab[P^]; // 020
      if B > 9 then
        result := -1
      else
      begin
        result := result * 10 + B;
        inc(P);
        B := tab[P^];
        if B > 9 then
          result := -1
        else
        begin
          result := result * 10 + B;
          inc(P);
          B := tab[P^];
          if B > 9 then
            result := -1
          else
          begin
            result := result * 10 + B;
            inc(P, 2);    // .
            B := tab[P^]; // 006
            if B > 9 then
              result := -1
            else
            begin
              result := result * 10 + B;
              inc(P);
              B := tab[P^];
              if B > 9 then
                result := -1
              else
              begin
                result := result * 10 + B;
                inc(P);
                B := tab[P^];
                if B > 9 then
                  result := -1
                else
                  result := result * 10 + B;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TSynLogFile.ProcessOneLine(LineBeg, LineEnd: PUtf8Char);
var
  thread: PtrUInt;
  MS: integer;
  L: TSynLogLevel;
  p: PCardinalArray;
begin
  inherited ProcessOneLine(LineBeg, LineEnd);
  if length(fLevels) < fLinesMax then
    SetLength(fLevels, fLinesMax);
  if (fCount <= fLineHeaderCountToIgnore) or
     (LineEnd - LineBeg < 24) then
    exit;
  if fLineLevelOffset = 0 then // detect the line layout (once)
  begin
    if (fCount > 50) or
       not (LineBeg[0] in ['0'..'9']) then
      exit; // definitively does not sound like a .log content
    if LineBeg[8] = ' ' then
    begin
      // YYYYMMDD HHMMSSXX[Z] is one/two chars bigger than Timestamp
      fLineLevelOffset := 19;
      if LineBeg[fLineLevelOffset] = 'Z' then
        inc(fLineLevelOffset); // did have TSynLogFamily.ZonedTimestamp
      fDayCurrent := PInt64(LineBeg)^;
      AddInteger(fDayChangeIndex, fCount - 1);
    end
    else
      fLineLevelOffset := 18;
    if (LineBeg[fLineLevelOffset] = '!') or // ! = thread 1
       (GetLogLevelFromText(LineBeg) = sllNone) then // may be thread > 1
    begin
      inc(fLineLevelOffset, 3);
      fThreadsCount := fLinesMax;
      SetLength(fThreads, fLinesMax);
    end;
    fLineTextOffset := fLineLevelOffset + 4;
  end;
  L := GetLogLevelFromText(LineBeg);
  if L = sllNone then
    exit;
  if (fDayChangeIndex <> nil) and
     (fDayCurrent <> PInt64(LineBeg)^) then
  begin
    fDayCurrent := PInt64(LineBeg)^;
    AddInteger(fDayChangeIndex, fCount - 1);
  end;
  if fThreads <> nil then
  begin
    if fThreadsCount < fLinesMax then
    begin
      fThreadsCount := fLinesMax;
      SetLength(fThreads, fLinesMax);
    end;
    thread := Chars3ToInt18(LineBeg + fLineLevelOffset - 5);
    fThreads[fCount - 1] := thread;
    if thread > fThreadMax then
    begin
      fThreadMax := thread;
      if PtrInt(thread) >= length(fThreadInfo) then
        SetLength(fThreadInfo, NextGrow(thread));
      if PtrInt(thread) >= length(fLogProcStack) then
      begin
        SetLength(fLogProcStack, NextGrow(thread));
        SetLength(fLogProcStackCount, length(fLogProcStack));
      end;
    end;
    inc(fThreadInfo[thread].Rows);
    if L = sllInfo then
    begin
      // fast detect the exact TSynLog.AddLogThreadName pattern
      p := pointer(LineBeg + fLineLevelOffset + 5); // from LogHeaderNoRecursion
      if (p^[0] = ord('S') + ord('e') shl 8 + ord('t') shl 16 + ord('T') shl 24) and
         (p^[1] = ord('h') + ord('r') shl 8 + ord('e') shl 16 + ord('a') shl 24) and
         (p^[2] = ord('d') + ord('N') shl 8 + ord('a') shl 16 + ord('m') shl 24) and
         ((p^[3] and $ffff) = ord('e') + ord(' ') shl 8) then
        PtrArrayAdd(fThreadInfo[thread].SetThreadName, LineBeg); // from now on
    end;
  end
  else
    thread := 0;
  fLevels[fCount - 1] := L; // need exact match of level text
  include(fLevelUsed, L);
  case L of
    sllEnter:
      begin
        AddInteger(fLogProcStack[thread], fLogProcStackCount[thread], fLogProcNaturalCount);
        if fLogProcNaturalCount >= length(fLogProcNatural) then
          SetLength(fLogProcNatural, NextGrow(fLogProcNaturalCount));
        // fLogProcNatural[].### fields will be set later during parsing
        inc(fLogProcNaturalCount);
      end;
    sllLeave:
      if (LineEnd - LineBeg > 10) and
         (LineEnd[-4] = '.') and
         (LineEnd[-8] = '.') and
         (fLogProcStackCount[thread] > 0) then
      begin
        // 00.020.006
        MS := DecodeMicroSec(PByte(LineEnd - 10));
        if MS >= 0 then
        begin
          dec(fLogProcStackCount[thread]);
          fLogProcNatural[fLogProcStack[thread]
            [fLogProcStackCount[thread]]].Time := MS;
        end;
      end;
  end;
end;

function TSynLogFile.ThreadRows(ThreadID: integer): cardinal;
begin
  if (fThreadInfo <> nil) and
     (cardinal(ThreadID) <= fThreadMax) then
    result := fThreadInfo[ThreadID].Rows
  else
    result := 0;
end;

function TSynLogFile.ThreadName(ThreadID, CurrentLogIndex: integer): RawUtf8;
var
  i: PtrInt;
  lineptr: PtrUInt;
  names: TPUtf8CharDynArray;
  found: pointer;
begin
  if ThreadID = 1 then
    result := 'Main Thread'
  else
  begin
    result := '';
    if cardinal(ThreadID) <= fThreadMax then
    begin
      names := fThreadInfo[ThreadID].SetThreadName;
      if names <> nil then // search the thread name at this position
      begin
        found := names[0];
        if cardinal(CurrentLogIndex) < cardinal(fCount) then
        begin
          lineptr := PtrUInt(fLines[CurrentLogIndex]);
          for i := length(names) - 1 downto 1 do
            if lineptr >= PtrUInt(names[i]) then
            begin
              found := names[i];
              break;
            end;
        end;
        FastSetString(result, found, GetLineSize(found, fMapEnd));
        delete(result, 1, PosEx('=', result, 40)); // raw thread name
      end;
    end;
    if result = '' then
      result := 'unnamed';
  end;
  if cardinal(ThreadID) <= fThreadMax then
    result := FormatUtf8('% % (% rows)',
      [ThreadID, result, fThreadInfo[ThreadID].Rows]);
end;

function TSynLogFile.ThreadNames(CurrentLogIndex: integer): TRawUtf8DynArray;
var
  i: PtrInt;
begin
  result := nil;
  SetLength(result, fThreadMax);
  if fThreadInfo <> nil then
    for i := 1 to fThreadMax do
      result[i - 1] := ThreadName(i, CurrentLogIndex);
end;

procedure TSynLogFile.GetDays(out Days: TDateTimeDynArray);
var
  i, n: PtrInt;
begin
  n := length(fDayChangeIndex);
  SetLength(Days, n);
  for i := 0 to n - 1 do
    Days[i] := EventDateTime(fDayChangeIndex[i]);
end;

function TSynLogFile.GetEventText(index: integer): RawUtf8;
var
  L: cardinal;
begin
  if (self = nil) or
     (cardinal(index) >= cardinal(fCount)) then
    result := ''
  else
  begin
    L := GetLineSize(fLines[index], fMapEnd);
    if L <= fLineTextOffset then
      result := ''
    else
      FastSetString(result, PAnsiChar(fLines[index]) + fLineTextOffset,
        L - fLineTextOffset);
  end;
end;

function TSynLogFile.EventString(index: integer; const replaceTabs: RawUtf8;
  maxutf8len: integer; includeFirstColumns: boolean): string;
var
  tmp: RawUtf8;
  header: string;
begin
  tmp := GetEventText(index);
  if tmp = '' then
  begin
    result := '';
    exit;
  end;
  if maxutf8len > 0 then
    Utf8TruncateToLength(tmp, maxutf8len);
  if replaceTabs <> '' then
    tmp := StringReplaceAll(tmp, #9, replaceTabs);
  if IsValidUtf8(tmp) then
    Utf8ToStringVar(tmp, result)
  else
    {$ifdef UNICODE}
    CurrentAnsiConvert.AnsiToUnicodeStringVar(pointer(tmp), length(tmp), result);
    {$else}
    result := tmp;
    {$endif UNICODE}
  if includeFirstColumns then
  begin
    Utf8DecodeToString(fLines[index], fLineTextOffset, header);
    result := header + result;
  end;
end;

procedure TSynLogFile.SetLogProcMerged(const Value: boolean);
var
  i, n: PtrInt;
  P, M: PSynLogFileProc;
  O: TLogProcSortOrder;
begin
  fLogProcIsMerged := Value;
  O := fLogProcSortInternalOrder;
  if Value then // set TSynLogFile.LogProcMerged=true profiling merged info
  begin
    if fLogProcMerged = nil then
    begin
      fLogProcCurrent := pointer(fLogProcNatural);
      fLogProcCurrentCount := fLogProcNaturalCount;
      LogProcSort(soByName); // sort by name to identify unique
      SetLength(fLogProcMerged, fLogProcNaturalCount);
      n := 0;
      i := 0;
      P := pointer(fLogProcNatural);
      repeat
        M := @fLogProcMerged[n];
        repeat
          M^.Index := P^.Index;
          inc(M^.Time, P^.Time);
          inc(M^.ProperTime, P^.ProperTime);
          inc(i);
          inc(P);
        until (i >= fLogProcNaturalCount) or
              (StrICompLeftTrim(PUtf8Char(fLines[LogProc[i - 1].Index]) + 22,
                                PUtf8Char(fLines[P^.Index]) + 22) <> 0);
        inc(n);
      until i >= fLogProcNaturalCount;
      SetLength(fLogProcMerged, n);
    end;
    fLogProcCurrent := pointer(fLogProcMerged);
    fLogProcCurrentCount := length(fLogProcMerged);
  end
  else // set TSynLogFile.LogProcMerged=true profiling natural/unmerged info
  begin
    fLogProcCurrent := pointer(fLogProcNatural);
    fLogProcCurrentCount := fLogProcNaturalCount;
  end;
  fLogProcSortInternalOrder := soNone;
  LogProcSort(O); // restore previous sort order
end;


{ TSynLogFileView }

procedure TSynLogFileView.LoadFromMap(AverageLineLength: integer);
begin
  inherited LoadFromMap(AverageLineLength);
  if fLevels <> nil then
  begin
    SetLength(fSelected, fCount);
    fSelectedCount := fCount;
    FillIncreasing(pointer(fSelected), 0, fCount);
    SetLength(fThreadSelected, (fThreadMax shr 3) + 1);
    SetAllThreads(true);
  end;
end;

procedure TSynLogFileView.AddInMemoryLine(const aNewLine: RawUtf8);
var
  index: integer;
  tm: cardinal;
begin
  tm := fThreadMax;
  inherited AddInMemoryLine(aNewLine);
  index := Count - 1;
  if EventLevel[index] in fEvents then
    AddInteger(fSelected, fSelectedCount, index);
  if tm <> fThreadMax then
  begin
    tm := (fThreadMax shr 3) + 1;
    if integer(tm) <> length(fThreadSelected) then
      SetLength(fThreadSelected, tm);
    SetBitPtr(pointer(fThreadSelected), fThreadMax - 1)
  end;
end;

const
  TIME_FORMAT = 'hh:mm:ss.zzz';
  MAXLOGLINES = 300;

function TSynLogFileView.GetLineForMemo(aRow, aTop, aBottom: integer): string;
var
  tim: string;
  elapsed: TDateTime;
begin
  result := '';
  if cardinal(aRow) < cardinal(fSelectedCount) then
    aRow := fSelected[aRow];
  if cardinal(aRow) < cardinal(fCount) then
  begin
    result := EventString(aRow, '', 0, true);
    if aBottom > aTop then
    begin
      elapsed := EventDateTime(aBottom) - EventDateTime(aTop);
      if Freq = 0 then
      begin
        DateTimeToString(tim, TIME_FORMAT, elapsed);
        result := tim + #13#10 + result;
      end
      else
      begin
        tim := IntToStr(trunc(elapsed * MilliSecsPerDay * 1000) mod 1000);
        result := StringOfChar('0', 3 - length(tim)) + tim + #13#10 + result;
        DateTimeToString(tim, TIME_FORMAT, elapsed);
        result := tim + '.' + result;
      end;
      result := FormatString('% lines - time elapsed: %',
        [aBottom - aTop + 1, result]);
    end;
  end;
end;

function TSynLogFileView.GetLineForClipboard(aRow: integer): string;
var
  dt: TDateTime;
begin
  result := '';
  if cardinal(aRow) < cardinal(fSelectedCount) then
    aRow := fSelected[aRow];
  if cardinal(aRow) < cardinal(fCount) then
  begin
    dt := EventDateTime(aRow);
    FormatString('% %'#9'%'#9, [DateToStr(dt), FormatDateTime(TIME_FORMAT, dt),
      _LogInfoCaption[EventLevel[aRow]]], result);
    if fThreads <> nil then
      result := result + IntToString(cardinal(fThreads[aRow])) + #9;
    result := result + EventString(aRow, '   ');
  end;
end;

function TSynLogFileView.GetCell(aCol, aRow: integer;
  out aLevel: TSynLogLevel): string;
begin
  aLevel := sllNone;
  result := '';
  if self <> nil then
    if cardinal(aRow) < cardinal(fSelectedCount) then
    begin
      aRow := fSelected[aRow];
      case aCol of
        0:
          DateTimeToString(result, TIME_FORMAT, EventDateTime(aRow));
        1:
          result := _LogInfoCaption[EventLevel[aRow]];
        2:
          if fThreads <> nil then
            result := IntToString(cardinal(fThreads[aRow]));
        3:
          result := EventString(aRow, '   ', MAXLOGLINES);
      end;
      aLevel := EventLevel[aRow];
    end
    else
      result := EventString(aRow, '   ', MAXLOGLINES);
end;

function TSynLogFileView.SearchNextEvent(aEvent: TSynLogLevel;
  aRow: integer): PtrInt;
begin
  if cardinal(aRow) < cardinal(fSelectedCount) then
  begin
    // search from next item
    for result := aRow + 1 to fSelectedCount - 1 do
      if fLevels[fSelected[result]] = aEvent then
        exit;
    // search from beginning
    for result := 0 to aRow - 1 do
      if fLevels[fSelected[result]] = aEvent then
        exit;
  end;
  result := -1;
end;

function TSynLogFileView.SearchNextText(const aPattern: RawUtf8;
  aRow, aDelta: integer): PtrInt;
begin
  result := -1;
  if (self = nil) or
     (aPattern = '') then
    exit;
  if fLevels = nil then // plain text search
  begin
    // search from next item
    for result := aRow + aDelta to fCount - 1 do
      if LineContains(aPattern, result) then
        exit;
    // search from beginning
    for result := 0 to aRow - 1 do
      if LineContains(aPattern, result) then
        exit;
  end
  else
  begin
    // search from next item
    for result := aRow + aDelta to fSelectedCount - 1 do
      if LineContains(aPattern, fSelected[result]) then
        exit;
    // search from beginning
    for result := 0 to aRow - 1 do
      if LineContains(aPattern, fSelected[result]) then
        exit;
  end;
  result := -1;
end;

function TSynLogFileView.SearchPreviousText(const aPattern: RawUtf8;
  aRow: integer): PtrInt;
begin
  result := -1;
  if (self = nil) or
     (aPattern = '') then
    exit;
  if fLevels = nil then
  // plain text search
  begin
    // search from previous item
    for result := aRow - 1 downto 0 do
      if LineContains(aPattern, result) then
        exit;
    // search from end
    for result := fCount - 1 downto aRow + 1 do
      if LineContains(aPattern, result) then
        exit;
  end
  else
  begin
    // search from previous item
    for result := aRow - 1 downto 0 do
      if LineContains(aPattern, fSelected[result]) then
        exit;
    // search from end
    for result := fSelectedCount - 1 downto aRow + 1 do
      if LineContains(aPattern, fSelected[result]) then
        exit;
  end;
  result := -1;
end;

function TSynLogFileView.SearchThread(aThreadID: word;
  aRow: integer): PtrInt;
begin
  if (self <> nil) and
     (cardinal(aRow) < cardinal(fSelectedCount)) and
     (fThreads <> nil) then
  begin
    for result := aRow + 1 to fSelectedCount - 1 do
      if fThreads[fSelected[result]] = aThreadID then
        exit;
    for result := 0 to aRow - 1 do
      if fThreads[fSelected[result]] = aThreadID then
        exit;
  end;
  result := -1;
end;

function TSynLogFileView.SearchNextThread(aRow: integer): PtrInt;
var
  currentThreadID: word;
begin
  if (self <> nil) and
     (cardinal(aRow) < cardinal(fSelectedCount)) and
     (fThreads <> nil) then
  begin
    result := aRow;
    currentThreadID := fThreads[fSelected[result]];
    repeat
      inc(result);
      if result = fSelectedCount then
        break;
      if fThreads[fSelected[result]] <> currentThreadID then
        exit; // found
    until false;
  end;
  result := -1;
end;

function TSynLogFileView.SearchNextSameThread(aRow: integer): PtrInt;
var
  currentThreadID: word;
begin
  if (self <> nil) and
     (cardinal(aRow) < cardinal(fSelectedCount)) and
     (fThreads <> nil) then
  begin
    result := aRow;
    currentThreadID := fThreads[fSelected[result]];
    repeat
      inc(result);
      if result = fSelectedCount then
        break;
      if fThreads[fSelected[result]] = currentThreadID then
        exit; // found
    until false;
  end;
  result := -1;
end;

function TSynLogFileView.SearchPreviousSameThread(aRow: integer): PtrInt;
var
  currentThreadID: word;
begin
  if (self <> nil) and
     (cardinal(aRow) < cardinal(fSelectedCount)) and
     (fThreads <> nil) then
  begin
    result := aRow;
    currentThreadID := fThreads[fSelected[result]];
    repeat
      dec(result);
      if result < 0 then
        break;
      if fThreads[fSelected[result]] = currentThreadID then
        exit; // found
    until false;
  end;
  result := -1;
end;

function TSynLogFileView.SearchEnterLeave(aRow: integer): PtrInt;
var
  Level, ndx: PtrInt;
  currentThreadID: word;
begin
  if (self = nil) or
     (cardinal(aRow) >= cardinal(fSelectedCount)) then
  begin
    result := -1;
    exit;
  end;
  Level := 0;
  result := aRow;
  ndx := fSelected[result];
  if EventThread <> nil then
    currentThreadID := EventThread[ndx]
  else
    currentThreadID := 0;
  case EventLevel[ndx] of
    sllEnter: // retrieve corresponding Leave event
      repeat
        inc(result);
        if result >= fSelectedCount then
          break;
        ndx := fSelected[result];
        case EventLevel[ndx] of
          sllEnter:
            if (currentThreadID = 0) or
               (EventThread[ndx] = currentThreadID) then
              inc(Level);
          sllLeave:
            if (currentThreadID = 0) or
               (EventThread[ndx] = currentThreadID) then
              if Level = 0 then
                exit
              else
                dec(Level);
        end;
      until false;
    sllLeave: // retrieve corresponding Enter event
      repeat
        dec(result);
        if result < 0 then
          break;
        ndx := fSelected[result];
        case EventLevel[ndx] of
          sllLeave:
            if (currentThreadID = 0) or
               (EventThread[ndx] = currentThreadID) then
              inc(Level);
          sllEnter:
            if (currentThreadID = 0) or
               (EventThread[ndx] = currentThreadID) then
              if Level = 0 then
                exit
              else
                dec(Level);
        end;
      until false;
  end;
  result := -1;
end;

function TSynLogFileView.SearchNextSelected(aIndex: integer): PtrInt;
begin
  // TODO: use faster binary search instead of this rough O(n) loop?
  for result := 0 to fSelectedCount - 1 do
    if fSelected[result] >= aIndex then
      exit;
  result := -1;
end;

procedure TSynLogFileView.SetAllThreads(enabled: boolean);
const
  B: array[boolean] of byte = (0, 255);
begin
  FillcharFast(fThreadSelected[0], length(fThreadSelected), B[enabled]);
end;

procedure TSynLogFileView.SetThreads(thread: integer; value: boolean);
begin
  dec(thread);
  if cardinal(thread) < fThreadMax then
    if value then
      SetBitPtr(pointer(fThreadSelected), thread)
    else
      UnSetBitPtr(pointer(fThreadSelected), thread);
end;

function TSynLogFileView.GetThreads(thread: integer): boolean;
begin
  dec(thread);
  result := (cardinal(thread) < fThreadMax) and
            GetBitPtr(pointer(fThreadSelected), thread);
end;

function TSynLogFileView.Select(aRow: integer): integer;
var
  i, search: PtrInt;
begin
  result := 0;
  if integer(fEvents) <> 0 then
  begin
    if cardinal(aRow) < cardinal(fSelectedCount) then
      search := fSelected[aRow]
    else
      search := maxInt;
    fSelectedCount := 0;
    for i := 0 to Count - 1 do
      if fLevels[i] in fEvents then
        if (fThreads = nil) or
           GetThreads(fThreads[i]) then
        begin
          if search <= i then
          begin
            // found the closed selected index
            result := fSelectedCount;
            search := maxInt;
          end;
          if fSelectedCount = length(fSelected) then
            SetLength(fSelected, NextGrow(fSelectedCount));
          fSelected[fSelectedCount] := i;
          inc(fSelectedCount);
        end;
  end;
end;


{ **************  SysLog Messages Support as defined by RFC 5424 }

function PrintUSAscii(P: PUtf8Char; const text: RawUtf8): PUtf8Char;
var
  i: PtrInt;
begin
  P^ := ' ';
  inc(P);
  for i := 1 to length(text) do
    if ord(text[i]) in [33..126] then
    begin
      // only non-space printable ASCII chars
      P^ := text[i];
      inc(P);
    end;
  if P[-1] = ' ' then
  begin
    // nothing appended -> NILVALUE
    P^ := '-';
    inc(P);
  end;
  result := P;
end;

function SyslogMessage(facility: TSyslogFacility; severity: TSyslogSeverity;
  const msg, procid, msgid: RawUtf8; destbuffer: PUtf8Char; destsize: PtrInt;
  trimmsgfromlog: boolean): PtrInt;
var
  P: PAnsiChar;
  start: PUtf8Char;
  len: PtrInt;
  st: TSynSystemTime;
begin
  result := 0;
  if destsize < 127 then
    exit;
  start := destbuffer;
  destbuffer^ := '<';
  destbuffer := AppendUInt32ToBuffer(destbuffer + 1,
    ord(severity) + ord(facility) shl 3);
  PInteger(destbuffer)^ :=
    ord('>') + ord('1') shl 8 + ord(' ') shl 16; // VERSION=1
  inc(destbuffer, 3);
  st.FromNowUtc;
  DateToIso8601PChar(destbuffer, true, st.Year, st.Month, st.Day);
  TimeToIso8601PChar(destbuffer + 10,
    true, st.Hour, st.Minute, st.Second, st.MilliSecond, 'T', {withms=}true);
  destbuffer[23] := 'Z';
  inc(destbuffer, 24);
  with Executable do
  begin
    if length(Host) + length(ProgramName) + length(procid) +
       length(msgid) + (destbuffer - start) + 15 > destsize then
      // avoid buffer overflow
      exit;
    destbuffer := PrintUSAscii(destbuffer, Host);         // HOST
    destbuffer := PrintUSAscii(destbuffer, ProgramName);  // APP-NAME
  end;
  destbuffer := PrintUSAscii(destbuffer, procid);         // PROCID
  destbuffer := PrintUSAscii(destbuffer, msgid);          // MSGID
  destbuffer := PrintUSAscii(destbuffer, '');             // no STRUCTURED-DATA
  destbuffer^ := ' ';
  inc(destbuffer);
  len := length(msg);
  P := pointer(msg);
  if trimmsgfromlog and
     (len > 27) then
    if (P[0] = '2') and
       (P[8] = ' ') then
    begin
      // trim e.g. '20160607 06442255  ! trace '
      inc(P, 27);
      dec(len, 27);
    end
    else if mormot.core.text.HexToBin(P, nil, 8) then
    begin
      // trim e.g. '00000000089E5A13  " info '
      inc(P, 25);
      dec(len, 25);
    end;
  while (len > 0) and
        (P^ <= ' ') do
  begin
    // trim left spaces
    inc(P);
    dec(len);
  end;
  len := Utf8TruncatedLength(P, len, destsize - (destbuffer - start) - 3);
  if not IsAnsiCompatible(P, len) then
  begin
    PInteger(destbuffer)^ := BOM_UTF8; // weird enough behavior on POSIX :(
    inc(destbuffer, 3);
  end;
  MoveFast(P^, destbuffer^, len);
  result := (destbuffer - start) + len;
end;


procedure InitializeUnit;
begin
  GlobalThreadLock.Init;
  GetEnumTrimmedNames(TypeInfo(TSynLogLevel), @_LogInfoText);
  GetEnumCaptions(TypeInfo(TSynLogLevel), @_LogInfoCaption);
  _LogInfoCaption[sllNone] := '';
  GetEnumTrimmedNames(TypeInfo(TAppLogLevel), @_LogAppText);
  SetThreadName := _SetThreadName;
  GetCurrentThreadName := _GetCurrentThreadName;
  SetCurrentThreadName('MainThread');
  GetExecutableLocation := _GetExecutableLocation; // use FindLocationShort()
  LogCompressAlgo := AlgoSynLZ; // very fast and efficient on logs
  LogCompressAlgoArchive := @_LogCompressAlgoArchive;
  //writeln(BacktraceStrFpc(Get_pc_addr));
  //writeln(GetExecutableLocation(get_caller_addr(get_frame)));
  //writeln(GetInstanceDebugFile.FindLocationShort(PtrUInt(@TDynArray.InitFrom)));
  //GetInstanceDebugFile.SaveToJson(NowToFileShort+'.json',jsonUnquotedPropName);
end;

procedure FinalizeUnit;
var
  files: TSynLogDynArray; // thread-safe local copy
begin
  {$ifndef NOEXCEPTIONINTERCEPT}
  HandleExceptionFamily := nil; // disable exception interception
  {$endif NOEXCEPTIONINTERCEPT}
  SynLogFileFreeing := true;    // to avoid GPF at shutdown
  GlobalThreadLock.Lock;
  files := SynLogFile;
  SynLogFile := nil;            // would break any background process
  SynLogFamily := nil;          // paranoid - freed as TRttiCustom.Private
  GlobalThreadLock.UnLock;
  if AutoFlushThread <> nil then
  begin
    AutoFlushThread.Terminate;
    AutoFlushThread.fEvent.SetEvent; // notify TAutoFlushThread.Execute
    AutoFlushThread.WaitFor;
    FreeAndNilSafe(AutoFlushThread);
  end;
  ObjArrayClear(files, {safe=}true); // TRttiCustom.Private frees TSynLogFamily
  {$ifdef FPC}
  if @BacktraceStrFunc = @BacktraceStrFpc then
    BacktraceStrFunc := SysBacktraceStr; // avoid instability
  {$endif FPC}
  FreeAndNilSafe(ExeInstanceDebugFile);
  GlobalThreadLock.Done;
end;


initialization
  InitializeUnit;

finalization
  FinalizeUnit;

end.

