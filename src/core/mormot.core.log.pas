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
  syncobjs,
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

const
  /// up to 16 TSynLogFamily, i.e. TSynLog children classes can be defined
  MAX_SYNLOGFAMILY = 15;

  /// can be set to TSynLogFamily.Level in order to log all available events
  LOG_VERBOSE: TSynLogInfos =
    [succ(sllNone)..high(TSynLogInfo)];

  /// contains the logging levels for which stack trace should be dumped
  // - which are mainly exceptions or application errors
  LOG_STACKTRACE: TSynLogInfos =
    [sllException,
     sllExceptionOS,
     sllLastError,
     sllError,
     sllDDDError];

  /// the text equivalency of each logging level, as written in the log file
  // - PCardinal(@LOG_LEVEL_TEXT[L][3])^ will be used for fast level matching
  // so text must be unique for characters [3..6] -> e.g. 'UST4'
  LOG_LEVEL_TEXT: array[TSynLogInfo] of string[7] = (
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

  /// RGB colors corresponding to each logging level
  // - matches the TColor values, as used by the VCL
  // - first array is for the background, second is for the text (black/white)
  LOG_LEVEL_COLORS: array[boolean, TSynLogInfo] of integer = (
   ($FFFFFF,  // sllNone
    $DCC0C0,  // sllInfo
    $DCDCDC,  // sllDebug
    $C0C0C0,  // sllTrace
    $8080C0,  // sllWarning
    $8080FF,  // sllError
    $C0DCC0,  // sllEnter
    $DCDCC0,  // sllLeave
    $C0C0F0,  // sllLastError
    $C080FF,  // sllException
    $C080F0,  // sllExceptionOS
    $C080C0,  // sllMemory
    $C080C0,  // sllStackTrace
    $4040FF,  // sllFail
    $B08080,  // sllSQL
    $B0B080,  // sllCache
    $8080DC,  // sllResult
    $80DC80,  // sllDB
    $DC8080,  // sllHTTP
    $DCFF00,  // sllClient
    $DCD000,  // sllServer
    $DCDC80,  // sllServiceCall
    $DC80DC,  // sllServiceReturn
    $DCDCDC,  // sllUserAuth
    $D0D0D0,  // sllCustom1
    $D0D0DC,  // sllCustom2
    $D0D0C0,  // sllCustom3
    $D0D0E0,  // sllCustom4
    $20E0D0,  // sllNewRun
    $8080FF,  // sllDDDError
    $DCCDCD,  // sllDDDInfo
    $C0C0C0), // sllMonitoring
    // black/white text corresponding to each colored background:
   ($000000,  // sllNone
    $000000,  // sllInfo
    $000000,  // sllDebug
    $000000,  // sllTrace
    $000000,  // sllWarning
    $FFFFFF,  // sllError
    $000000,  // sllEnter
    $000000,  // sllLeave
    $FFFFFF,  // sllLastError
    $FFFFFF,  // sllException
    $FFFFFF,  // sllExceptionOS
    $000000,  // sllMemory
    $000000,  // sllStackTrace
    $FFFFFF,  // sllFail
    $FFFFFF,  // sllSQL
    $000000,  // sllCache
    $FFFFFF,  // sllResult
    $000000,  // sllDB
    $000000,  // sllHTTP
    $000000,  // sllClient
    $000000,  // sllServer
    $000000,  // sllServiceCall
    $000000,  // sllServiceReturn
    $000000,  // sllUserAuth
    $000000,  // sllCustom1
    $000000,  // sllCustom2
    $000000,  // sllCustom3
    $000000,  // sllCustom4
    $000000,  // sllNewRun
    $FFFFFF,  // sllDDDError
    $000000,  // sllDDDInfo
    $000000));// sllMonitoring

  /// console colors corresponding to each logging level
  // - to be used with mormot.core.os TextColor()
  LOG_CONSOLE_COLORS: array[TSynLogInfo] of TConsoleColor = (
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

  /// how TLogFilter map TSynLogInfo events
  LOG_FILTER: array[TSynLogFilter] of TSynLogInfos = (
    [],                                                       // lfNone
    [succ(sllNone)..high(TSynLogInfo)],                       // lfAll
    [sllError, sllLastError, sllException, sllExceptionOS],   // lfErrors
    [sllException, sllExceptionOS],                           // lfExceptions
    [sllEnter, sllLeave],                                     // lfProfile
    [sllSQL, sllCache, sllDB],                                // lfDatabase
    [sllClient, sllServer, sllServiceCall, sllServiceReturn], // lfClientServer
    [sllDebug, sllTrace, sllEnter],                           // lfDebug
    [sllCustom1..sllCustom4],                                 // lfCustom
    [sllDDDError, sllDDDInfo]);                               // lfDDD

  /// may be used to log as Debug or Error event, depending on an Error: boolean
  LOG_DEBUGERROR: array[boolean] of TSynLogInfo = (
    sllDebug,
    sllError);

  /// may be used to log as Trace or Warning event, depending on an Error: boolean
  LOG_TRACEWARNING: array[boolean] of TSynLogInfo = (
    sllTrace,
    sllWarning);

  /// may be used to log as Trace or Error event, depending on an Error: boolean
  LOG_TRACEERROR: array[boolean] of TSynLogInfo = (
    sllTrace,
    sllError);

  /// may be used to log as Info or Warning event, depending on an Error: boolean
  LOG_INFOWARNING: array[boolean] of TSynLogInfo = (
    sllInfo,
    sllWarning);

/// returns the trimmed text value of a logging level
// - i.e. 'Warning' for sllWarning
function ToText(event: TSynLogInfo): RawUtf8; overload;

/// returns the trimmed text value of a logging levels set
function ToText(events: TSynLogInfos): ShortString; overload;

/// returns the ready-to-be displayed text of a TSynLogInfo value
function ToCaption(event: TSynLogInfo): string; overload;

/// returns the ready-to-be displayed text of a TSynLogFilter value
function ToCaption(filter: TSynLogFilter): string; overload;

/// returns a method event as text, using the .map/.dbg/.mab information if available
function ToText(const Event: TMethod): RawUtf8; overload;

/// retrieve a one-line of text including detailed heap information
// - will use the RTL status entrypoint, or detect mormot.core.fpcx64mm
// and retrieve all its available information
// - as used by TSynLog.AddMemoryStats
function RetrieveMemoryManagerInfo: RawUtf8;

var
  /// low-level variable used internally by this unit
  // - we use a process-wide giant lock to avoid proper multi-threading of logs
  // - do not access this variable in your code: defined here to allow inlining
  GlobalThreadLock: TRTLCriticalSection;

  /// is set to TRUE before ObjArrayClear(SynLogFile) in unit finalization
  // - defined here to avoid unexpected GPF at shutdown
  SynLogFileFreeing: boolean;

type
  /// class of Exceptions raised by this unit
  ESynLogException = class(ESynException);

  /// an exception which wouldn't be logged and intercepted by this unit
  // - only this exact class will be recognized by TSynLog: inheriting it
  // will trigger the interception, as any other regular exception
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
    procedure Log(Level: TSynLogInfo; const TextFmt: RawUtf8;
      const TextArgs: array of const; Instance: TObject = nil); overload;
    /// call this method to add some information to the log at a specified level
    // - if Instance is set and Text is not '', it will log the corresponding
    // class name and address (to be used e.g. if you didn't call TSynLog.Enter()
    // method first)
    // - if Instance is set and Text is '', will behave the same as
    // Log(Level,Instance), i.e. write the Instance as JSON content
    procedure Log(Level: TSynLogInfo; const Text: RawUtf8;
      Instance: TObject = nil; TextTruncateAtLength: integer = maxInt); overload;
    {$ifdef UNICODE}
    /// call this method to add some VCL string to the log at a specified level
    // - this overloaded version will avoid a call to StringToUtf8()
    procedure Log(Level: TSynLogInfo; const Text: string;
      Instance: TObject = nil); overload;
    {$endif UNICODE}
    /// call this method to add the content of an object to the log at a
    // specified level
    // - TSynLog will write the class and hexa address - TSqlLog will write the
    // object JSON content
    procedure Log(Level: TSynLogInfo; Instance: TObject); overload;
    /// call this method to add the content of most low-level types to the log
    // at a specified level
    // - TSynLog will handle enumerations and dynamic array; TSqlLog will be
    // able to write TObject/TOrm and sets content as JSON
    procedure Log(Level: TSynLogInfo; const aName: RawUtf8; aTypeInfo: PRttiInfo;
      const aValue; Instance: TObject); overload;
    /// call this method to add the caller address to the log at the specified level
    // - if the debugging info is available from TDebugFile, will log the
    // unit name, associated symbol and source code line
    procedure Log(Level: TSynLogInfo = sllTrace); overload;
    /// call this method to add some multi-line information to the log at a
    // specified level
    // - LinesToLog content will be added, one line per one line, delimited
    // by #13#10 (CRLF)
    // - if a line starts with IgnoreWhenStartWith (already uppercase), it won't
    // be added to the log content (to be used e.g. with '--' for SQL statements)
    procedure LogLines(Level: TSynLogInfo; LinesToLog: PUtf8Char;
      aInstance: TObject = nil; const IgnoreWhenStartWith: PAnsiChar = nil);
    /// retrieve the associated logging instance
    function Instance: TSynLog;
  end;

  /// this event can be set for a TSynLogFamily to archive any deprecated log
  // into a custom compressed format
  // - will be called by TSynLogFamily when TSynLogFamily.Destroy identify
  // some outdated files
  // - the aOldLogFileName will contain the .log file with full path
  // - the aDestinationPath parameter will contain 'ArchivePath\log\YYYYMM\'
  // - should return true on success, false on error
  // - example of matching event handler are EventArchiveDelete,
  // EventArchiveSynLZ, EventArchiveLizard or EventArchiveZip in SynZip.pas
  // - this event handler will be called one time per .log file to archive,
  // then one last time with aOldLogFileName='' in order to close any pending
  // archive (used e.g. by EventArchiveZip to open the .zip only once)
  TSynLogArchiveEvent = function(const aOldLogFileName,
    aDestinationPath: TFileName): boolean;

  /// this event can be set for a TSynLogFamily to customize the file rotation
  // - will be called by TSynLog.PerformRotation
  // - should return TRUE if the function did process the file name
  // - should return FALSE if the function did not do anything, so that the
  // caller should perform the rotation as usual
  TSynLogRotateEvent = function(aLog: TSynLog; const aOldLogFileName: TFileName): boolean;

  /// how threading is handled by the TSynLogFamily
  // - proper threading expects the TSynLog.NotifyThreadEnded method to be called
  // when a thread is about to terminate, e.g. from TRest.EndCurrentThread
  // - by default, ptMergedInOneFile will indicate that all threads are logged
  // in the same file, in occurence order
  // - if set to ptOneFilePerThread, it will create one .log file per thread
  // - if set to ptIdentifiedInOneFile, a new column will be added for each
  // log row, with the corresponding ThreadID - LogView tool will be able to
  // display per-thread logging, if needed - note that your application shall
  // use a thread pool (just like all mORMot servers classes do), otherwise
  // some random hash collision may occur if Thread IDs are not recycled enough
  // - if set to ptNoThreadProcess, no thread information is gathered, and all
  // Enter/Leave would be merged into a single call - but it may be mandatory
  // to use this option if TSynLog.NotifyThreadEnded is not called (e.g. from
  // legacy code), and that your process experiment instability issues
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
    const ThreadName: RawUtf8): boolean of object;

{$endif NOEXCEPTIONINTERCEPT}

  /// regroup several logs under an unique family name
  // - you should usualy use one family per application or per architectural
  // module: e.g. a server application may want to log in separate files the
  // low-level Communication, the DB access, and the high-level process
  // - initialize the family settings before using them, like in this code:
  // ! with TSynLogDB.Family do
  // ! begin
  // !   Level := LOG_VERBOSE;
  // !   PerThreadLog := ptOneFilePerThread;
  // !   DestinationPath := 'C:\Logs';
  // ! end;
  //- then use the logging system inside a method:
  // ! procedure TMyDB.MyMethod;
  // ! var ILog: ISynLog;
  // ! begin
  // !   ILog := TSynLogDB.Enter(self,'MyMethod');
  // !   // do some stuff
  // !   ILog.Log(sllInfo,'method called');
  // ! end; // when ILog is out-of-scope, will log the method leaving
  TSynLogFamily = class
  protected
    fLevel, fLevelStackTrace: TSynLogInfos;
    fArchiveAfterDays: integer;
    fArchivePath: TFileName;
    fOnArchive: TSynLogArchiveEvent;
    fOnRotate: TSynLogRotateEvent;
    fCustomFileName: TFileName;
    fGlobalLog: TSynLog;
    fSynLogClass: TSynLogClass;
    fDestinationPath: TFileName;
    fDefaultExtension: TFileName;
    fIdent: integer;
    fBufferSize: integer;
    fPerThreadLog: TSynLogPerThreadMode;
    fIncludeComputerNameInFileName: boolean;
    fHighResolutionTimestamp: boolean;
    fLocalTimestamp: boolean;
    fWithUnitName: boolean;
    fWithInstancePointer: boolean;
    fNoFile: boolean;
    fAutoFlushTimeOut: cardinal;
    {$ifndef NOEXCEPTIONINTERCEPT}
    fOnBeforeException: TOnBeforeException;
    fHandleExceptions: boolean;
    {$endif NOEXCEPTIONINTERCEPT}
    fStackTraceLevel: byte;
    fStackTraceUse: TSynLogStackTraceUse;
    fFileExistsAction: TSynLogExistsAction;
    {$ifdef OSWINDOWS}
    fNoEnvironmentVariable: boolean;
    {$endif OSWINDOWS}
    fExceptionIgnore: TList;
    fEchoToConsole: TSynLogInfos;
    fEchoCustom: TOnTextWriterEcho;
    fEchoRemoteClient: TObject;
    fEchoRemoteEvent: TOnTextWriterEcho;
    fEchoRemoteClientOwned: boolean;
    fEchoToConsoleUseJournal: boolean;
    fEchoToConsoleBackground: boolean;
    fEndOfLineCRLF: boolean;
    fDestroying: boolean;
    fRotateFileCurrent: cardinal;
    fRotateFileCount: cardinal;
    fRotateFileSize: cardinal;
    fRotateFileAtHour: integer;
    function CreateSynLog: TSynLog;
    procedure StartAutoFlush;
    procedure SetDestinationPath(const value: TFileName);
    procedure SetLevel(aLevel: TSynLogInfos);
    procedure SynLogFileListEcho(const aEvent: TOnTextWriterEcho; aEventAdd: boolean);
    procedure SetEchoToConsole(aEnabled: TSynLogInfos);
    procedure SetEchoToConsoleUseJournal(aValue: boolean);
    procedure SetEchoCustom(const aEvent: TOnTextWriterEcho);
    function GetSynLogClassName: string;
    {$ifndef NOEXCEPTIONINTERCEPT}
    function GetExceptionIgnoreCurrentThread: boolean;
    procedure SetExceptionIgnoreCurrentThread(aExceptionIgnoreCurrentThread: boolean);
    {$endif NOEXCEPTIONINTERCEPT}
  public
    /// intialize for a TSynLog class family
    // - add it in the global SynLogFileFamily[] list
    constructor Create(aSynLog: TSynLogClass);
    /// release associated memory
    // - will archive older DestinationPath\*.log files, according to
    // ArchiveAfterDays value and ArchivePath
    destructor Destroy; override;

    /// retrieve the corresponding log file of this thread and family
    // - creates the TSynLog if not already existing for this current thread
    // - not worth inlining: TSynLog.Add will directly check fGlobalLog
    function SynLog: TSynLog;
    /// register one object and one echo callback for remote logging
    // - aClient is typically a mORMot's TRestHttpClient or a TSynLogCallbacks
    // instance as defined in this unit
    // - if aClientOwnedByFamily is TRUE, its life time will be manage by this
    // TSynLogFamily: it will stay alive until this TSynLogFamily is destroyed,
    // or the EchoRemoteStop() method called
    // - aClientEvent should be able to send the log row to the remote server
    procedure EchoRemoteStart(aClient: TObject; const aClientEvent: TOnTextWriterEcho;
      aClientOwnedByFamily: boolean);
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

    /// you can add some exceptions to be ignored to this list
    // - for instance, EConvertError may be added to the list, as such:
    // ! TSqlLog.Family.ExceptionIgnore.Add(EConvertError);
    // - you may also trigger ESynLogSilent exceptions for silent process
    // - see also ExceptionIgnoreCurrentThread property, if you want a per-thread
    // filtering of all exceptions
    property ExceptionIgnore: TList
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
      read GetExceptionIgnoreCurrentThread write SetExceptionIgnoreCurrentThread;
    /// you can let exceptions be ignored from a callback
    // - if set and returns true, the given exception won't be logged
    // - execution of this event handler is protected via the logs global lock
    // - may be handy e.g. when working with code triggerring a lot of
    // exceptions (e.g. Indy), where ExceptionIgnore could be refined
    property OnBeforeException: TOnBeforeException
      read fOnBeforeException write fOnBeforeException;
    {$endif NOEXCEPTIONINTERCEPT}
    /// event called to archive the .log content after a defined delay
    // - Destroy will parse DestinationPath folder for *.log files matching
    // ArchiveAfterDays property value
    // - you can set this property to EventArchiveDelete in order to delete deprecated
    // files, or EventArchiveSynLZ to compress the .log file into our propertary
    // SynLZ format: resulting file name will be ArchivePath\log\YYYYMM\*.log.synlz
    // (use FileUnSynLZ function to uncompress it)
    // - if you use SynZip.EventArchiveZip, the log files will be archived in
    // ArchivePath\log\YYYYMM.zip
    // - the aDestinationPath parameter will contain 'ArchivePath\log\YYYYMM\'
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
    property EchoToConsole: TSynLogInfos
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
    /// index in global SynLogFileFamily[] and SynLogLookupThreadVar[] lists
    property Ident: integer
      read fIdent;
    /// the current level of logging information for this family
    // - can be set e.g. to LOG_VERBOSE in order to log every kind of events
    property Level: TSynLogInfos
      read fLevel write SetLevel;
    /// the levels which will include a stack trace of the caller
    // - by default, contains sllStackTrace,sllException,sllExceptionOS plus
    // sllError,sllFail,sllLastError,sllDDDError
    // - exceptions will always trace the stack
    property LevelStackTrace: TSynLogInfos
      read fLevelStackTrace write fLevelStackTrace;
    /// the folder where the log must be stored
    // - by default, is in the executable folder
    property DestinationPath: TFileName
      read fDestinationPath write SetDestinationPath;
    /// the file extension to be used
    // - is '.log' by default
    property DefaultExtension: TFileName
      read fDefaultExtension write fDefaultExtension;
    /// if TRUE, the log file name will contain the Computer name - as '(MyComputer)'
    property IncludeComputerNameInFileName: boolean
      read fIncludeComputerNameInFileName write fIncludeComputerNameInFileName;
    /// can be used to customized the default file name
    // - by default, the log file name is computed from the executable name
    // (and the computer name if IncludeComputerNameInFileName is true)
    // - you can specify your own file name here, to be used instead
    // - this file name should not contain any folder, nor file extension (which
    // are set by DestinationPath and DefaultExtension properties)
    property CustomFileName: TFileName
      read fCustomFileName write fCustomFileName;
    /// the folder where old log files must be compressed
    // - by default, is in the executable folder, i.e. the same as DestinationPath
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
    // in order to force the writting to disk
    // - is set to 4096 by default (4 KB is the standard hard drive cluster size)
    property BufferSize: integer
      read fBufferSize write fBufferSize;
    /// define how thread will be identified during logging process
    // - by default, ptMergedInOneFile will indicate that all threads are logged
    // in the same file, in occurence order (so multi-thread process on server
    // side may be difficult to interpret)
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
    // - by default, the log file will be written for every 4 KB of log (see
    // BufferSize property) - this will ensure that the main application won't
    // be slow down by logging
    // - in order not to loose any log, a background thread can be created
    // and will be responsible of flushing all pending log content every
    // period of time (e.g. every 10 seconds)
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
    // SynLZ algorithm, and will be named e.g. as MainLogFileName.0.synlz ..
    // MainLogFileName.7.synlz for RotateFileCount=9 (total count = 9, including
    // 1 main log file and 8 .synlz files)
    property RotateFileCount: cardinal
      read fRotateFileCount write fRotateFileCount;
    /// maximum size of auto-rotated logging files, in kilo-bytes (per 1024 bytes)
    // - specify the maximum file size upon which .synlz rotation takes place
    // - is not used if RotateFileCount is left to its default 0
    property RotateFileSizeKB: cardinal
      read fRotateFileSize write fRotateFileSize;
    /// fixed hour of the day where logging files rotation should be performed
    // - by default, equals -1, meaning no rotation
    // - you can set a time value between 0 and 23 to force the rotation at this
    // specified hour
    // - is not used if RotateFileCount is left to its default 0
    property RotateFileDailyAtHour: integer
      read fRotateFileAtHour write fRotateFileAtHour;
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

  /// TSynLogThreadContext will define a dynamic array of such information
  // - used by TSynLog.Enter methods to handle recursivity calls tracing
  TSynLogThreadRecursion = record
    /// associated class instance to be displayed
    Instance: TObject;
    /// method name (or message) to be displayed
    // - may be a RawUtf8 if MethodNameLocal=mnEnterOwnMethodName
    MethodName: PUtf8Char;
    /// internal reference count used at this recursion level by TSynLog._AddRef
    RefCount: integer;
    /// if the method name is local, i.e. shall not be displayed at Leave()
    MethodNameLocal: (mnAlways, mnEnter, mnLeave, mnEnterOwnMethodName);
    {$ifndef FPC}
    /// the caller address, ready to display stack trace dump if needed
    Caller: PtrUInt;
    {$endif FPC}
    /// the high-resolution QueryPerformanceMicroSeconds timestamp at enter time
    EnterTimestamp: Int64;
  end;
  PSynLogThreadRecursion = ^TSynLogThreadRecursion;

  /// thread-specific internal context used during logging
  // - this structure is a hashed-per-thread variable
  TSynLogThreadContext = record
    /// the corresponding Thread ID
    ID: TThreadID;
    /// number of items stored in Recursion[]
    RecursionCount: integer;
    /// number of items available in Recursion[]
    // - faster than length(Recursion)
    RecursionCapacity: integer;
    /// used by TSynLog.Enter methods to handle recursive calls tracing
    Recursion: array of TSynLogThreadRecursion;
    /// the associated thread name
    // - is probably more complete than CurrentThreadName: TShort31 threadvar
    ThreadName: RawUtf8;
  end;

  // pointer to thread-specific context information
  PSynLogThreadContext = ^TSynLogThreadContext;


  /// a per-family and/or per-thread log file content
  // - you should create a sub class per kind of log file
  // ! TSynLogDB = class(TSynLog);
  // - the TSynLog instance won't be allocated in heap, but will share a
  // per-thread (if Family.PerThreadLog=ptOneFilePerThread) or global private
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
    fWriterEcho: TEchoWriter;
    fThreadContext: PSynLogThreadContext;
    fThreadID: TThreadID;
    fThreadLastHash: integer;
    fThreadIndex: integer;
    fCurrentLevel: TSynLogInfo;
    fInternalFlags: set of (logHeaderWritten, logInitDone);
    fDisableRemoteLog: boolean;
    {$ifndef NOEXCEPTIONINTERCEPT}
    fExceptionIgnoredBackup: boolean;
    fExceptionIgnoreThreadVar: PBoolean;
    {$endif NOEXCEPTIONINTERCEPT}
    fThreadContexts: array of TSynLogThreadContext;
    fThreadHash: TWordDynArray; // 8 KB buffer
    fStartTimestamp: Int64;
    fCurrentTimestamp: Int64;
    fStartTimestampDateTime: TDateTime;
    fWriterClass: TBaseWriterClass;
    fWriterStream: TStream;
    fFileName: TFileName;
    fStreamPositionAfterHeader: cardinal;
    fFileRotationSize: cardinal;
    fFileRotationNextHour: Int64;
    fThreadIndexReleased: TIntegerDynArray;
    fThreadIndexReleasedCount: integer;
    fThreadContextCount: integer;
    fNextFlushTix10: cardinal;
    function QueryInterface({$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif}
      iid: TGuid; out obj): TIntQry;
      {$ifdef OSWINDOWS} stdcall {$else} cdecl {$endif};
    function _AddRef: TIntCnt;
      {$ifdef OSWINDOWS} stdcall {$else} cdecl {$endif};
    function _Release: TIntCnt;
      {$ifdef OSWINDOWS} stdcall {$else} cdecl {$endif};
    class function FamilyCreate: TSynLogFamily;
    procedure CreateLogWriter; virtual;
    procedure LogInternalFmt(Level: TSynLogInfo; const TextFmt: RawUtf8;
      const TextArgs: array of const; Instance: TObject);
    procedure LogInternalText(Level: TSynLogInfo; const Text: RawUtf8;
      Instance: TObject; TextTruncateAtLength: integer);
    procedure LogInternalRtti(Level: TSynLogInfo; const aName: RawUtf8;
      aTypeInfo: PRttiInfo; const aValue; Instance: TObject);
    // any call to this method MUST call LeaveCriticalSection(GlobalThreadLock)
    procedure LogHeader(Level: TSynLogInfo);
    procedure LogTrailer(Level: TSynLogInfo);
      {$ifdef HASINLINE}inline;{$endif}
    procedure LogCurrentTime; virtual;
    procedure LogFileInit; virtual;
    procedure LogFileHeader; virtual;
    procedure AddMemoryStats; virtual;
    procedure AddErrorMessage(Error: cardinal);
    procedure AddStackTrace(Level: TSynLogInfo; Stack: PPtrUInt);
    procedure ComputeFileName; virtual;
    function GetFileSize: Int64; virtual;
    procedure PerformRotation; virtual;
    procedure AddRecursion(aIndex: integer; aLevel: TSynLogInfo);
    function GetThreadContext: PSynLogThreadContext;
      {$ifdef HASINLINE}inline;{$endif}
    procedure GetThreadContextAndDisableExceptions;
      {$ifdef HASINLINE}inline;{$endif}
    procedure GetThreadContextInternal(id: PtrUInt);
    procedure ThreadContextRehash;
    function NewRecursion: PSynLogThreadRecursion;
    function Instance: TSynLog;
    function ConsoleEcho(Sender: TEchoWriter; Level: TSynLogInfo;
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
    procedure Flush(ForceDiskWrite: boolean);
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
    // Family.PerThreadLog=ptOneFilePerThread:
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
    procedure NotifyThreadEnded;
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
    // - on Delphi earlier than 10.4 (and not FPC), you could just call Enter()
    // inside the method block, without any ISynLog interface variable - but
    // it is not very future-proof to write the following code:
    // ! procedure TMyDB.SQLFlush;
    // ! begin
    // !   TSynLogDB.Enter(self,'SQLFlush');
    // !   // do some stuff
    // ! end;
    // - if no Method name is supplied, it will use the caller address, and
    // will write it as hexa and with full unit and symbol name, if the debugging
    // information is available (i.e. if TDebugFile retrieved the .map/.dbg content;
    // note that this is not available yet on FPC):
    // ! procedure TMyDB.SQLFlush;
    // ! var log: ISynLog;
    // ! begin
    // !   log := TSynLogDB.Enter(self);
    // !   // do some stuff
    // ! end;
    // - note that supplying a method name is faster than using the .map/.dbg content:
    // if you want accurate profiling, or expect to support FPC, it's better to
    // use a method name or not to relying on the .map/.dbg file - note that this
    // method name shall be a constant, and not a locally computed variable,
    // since it may trigger some random GPF at runtime - if it is a local
    // variable, you can set aMethodNameLocal=true
    // - if TSynLogFamily.HighResolutionTimestamp is TRUE, high-resolution
    // time stamp will be written instead of ISO 8601 date and time: this will
    // allow performance profiling of the application on the customer side
    // - Enter() will write the class name (and the unit name for classes with
    // published properties, if TSynLogFamily.WithUnitName is true) for both
    // enter (+) and leave (-) events:
    //  $ 20110325 19325801  +    MyDBUnit.TMyDB(004E11F4).SQLExecute
    //  $ 20110325 19325801 info   SQL=SELECT * FROM Table;
    //  $ 20110325 19325801  -    01.512.320
    class function Enter(aInstance: TObject = nil; aMethodName: PUtf8Char = nil;
      aMethodNameLocal: boolean = false): ISynLog; overload;
    /// handle method enter / auto-leave tracing, with some custom text arguments
    // - this overloaded method would not write the method name, but the supplied
    // text content, after expanding the parameters like FormatUtf8()
    // - it will append the corresponding sllLeave log entry when the method ends
    class function Enter(const TextFmt: RawUtf8; const TextArgs: array of const;
      aInstance: TObject = nil): ISynLog; overload;
    /// retrieve the current instance of this TSynLog class
    // - to be used for direct logging, without any Enter/Leave:
    // ! TSynLogDB.Add.Log(llError,'The % statement didn''t work',[SQL]);
    // - to be used for direct logging, without any Enter/Leave (one parameter
    // version - just the same as previous):
    // ! TSynLogDB.Add.Log(llError,'The % statement didn''t work',SQL);
    // - is just a wrapper around Family.SynLog - the same code will work:
    // ! TSynLogDB.Family.SynLog.Log(llError,'The % statement didn''t work',[SQL]);
    class function Add: TSynLog;
      {$ifdef HASINLINE}inline;{$endif}
    /// retrieve the family of this TSynLog class type
    class function Family: TSynLogFamily; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// returns a logging class which will never log anything
    // - i.e. a TSynLog sub-class with Family.Level := []
    class function Void: TSynLogClass;
    /// low-level method helper which can be called to make debugging easier
    // - log some warning message to the TSynLog family
    // - will force a manual breakpoint if tests are run from the IDE
    class procedure DebuggerNotify(Level: TSynLogInfo; const Format: RawUtf8;
      const Args: array of const);
    /// call this method to add some information to the log at the specified level
    // - will use TTextWriter.Add(...,twOnSameLine) to append its content
    // - % = #37 indicates a string, integer, floating-point, class parameter
    // to be appended as text (e.g. class name), any variant as JSON...
    // - note that cardinal values should be type-casted to Int64() (otherwise
    // the integer mapped value will be transmitted, therefore wrongly)
    procedure Log(Level: TSynLogInfo; const TextFmt: RawUtf8;
      const TextArgs: array of const; aInstance: TObject = nil); overload;
    /// same as Log(Level,TextFmt,[]) but with one RawUtf8 parameter
    procedure Log(Level: TSynLogInfo; const TextFmt: RawUtf8;
      const TextArg: RawUtf8; aInstance: TObject = nil); overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// same as Log(Level,TextFmt,[]) but with one Int64 parameter
    procedure Log(Level: TSynLogInfo; const TextFmt: RawUtf8;
      const TextArg: Int64; aInstance: TObject = nil); overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// call this method to add some information to the log at the specified level
    // - if Instance is set and Text is not '', it will log the corresponding
    // class name and address (to be used e.g. if you didn't call TSynLog.Enter()
    // method first) - for instance
    // ! TSqlLog.Add.Log(sllDebug,'GarbageCollector',GarbageCollector);
    // will append this line to the log:
    // $ 0000000000002DB9 debug TObjectList(00425E68) GarbageCollector
    // - if Instance is set and Text is '', will behave the same as
    // Log(Level,Instance), i.e. write the Instance as JSON content
    procedure Log(Level: TSynLogInfo; const Text: RawUtf8; aInstance: TObject = nil;
      TextTruncateAtLength: integer = maxInt); overload;
      {$ifdef HASINLINE} inline; {$endif}
    {$ifdef UNICODE}
    /// call this method to add some VCL string to the log at a specified level
    // - this overloaded version will avoid a call to StringToUtf8()
    procedure Log(Level: TSynLogInfo; const Text: string;
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
    procedure Log(Level: TSynLogInfo; aInstance: TObject); overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// call this method to add the content of most low-level types to the log
    // at a specified level
    // - this overridden implementation will write the value content,
    // written as human readable JSON: handle dynamic arrays and enumerations
    procedure Log(Level: TSynLogInfo; const aName: RawUtf8; aTypeInfo: PRttiInfo;
      const aValue; Instance: TObject); overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// call this method to add the caller address to the log at the specified level
    // - if the debugging info is available from TDebugFile, will log the
    // unit name, associated symbol and source code line
    procedure Log(Level: TSynLogInfo); overload;
    /// allows to identify the current thread with a textual representation
    // - would append an sllInfo entry with "SetThreadName ThreadID=Name" text
    // - entry would also be replicated at the begining of any rotated log file
    // - is called automatically by SetThreadName() global function
    // - if Name='', will use CurrentThreadName threadvar
    procedure LogThreadName(const Name: RawUtf8);
    /// call this method to add some multi-line information to the log at a
    // specified level
    // - LinesToLog content will be added, one line per one line, delimited by
    // #13#10 (CRLF)
    // - if a line starts with IgnoreWhenStartWith (already uppercase), it won't
    // be added to the log content (to be used e.g. with '--' for SQL statements)
    procedure LogLines(Level: TSynLogInfo; LinesToLog: PUtf8Char; aInstance: TObject = nil;
      const IgnoreWhenStartWith: PAnsiChar = nil);
    /// manual low-level TSynLog.Enter execution without the ISynLog
    // - may be used to log Enter/Leave stack from non-pascal code
    // - each call to ManualEnter should be followed by a matching ManualLeave
    // - aMethodName should be a not nil constant text
    procedure ManualEnter(aMethodName: PUtf8Char; aInstance: TObject = nil);
    /// manual low-level ISynLog release after TSynLog.Enter execution
    // - each call to ManualEnter should be followed by a matching ManualLeave
    procedure ManualLeave;
    /// low-level latest value returned by QueryPerformanceMicroSeconds()
    // - is only accurate after Enter() or if HighResolutionTimestamp is set
    function LastQueryPerformanceMicroSeconds: Int64;
      {$ifdef HASINLINE}inline;{$endif}
    /// allow to temporary disable remote logging
    // - to be used within a try ... finally section:
    // ! log.DisableRemoteLog(true);
    // ! try
    // !   log.Log(....); // won't be propagated to the remote log
    // ! finally
    // !   log.DisableRemoteLog(false);
    // ! end;
    procedure DisableRemoteLog(value: boolean);
    /// the associated TSynLog class
    function LogClass: TSynLogClass;
      {$ifdef HASINLINE}inline;{$endif}
    /// low-level class method which can be assigned to TSynLogProc callback
    // signature, or used instead of Add.Log
    // - will flush the content to disk and avoid any memory reallocation
    // if Level is sllExceptionOS, e.g. on SIGABRT/SIGQUIT/SIGINT
    class procedure DoLog(Level: TSynLogInfo; const Fmt: RawUtf8;
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
    /// the current number of thread contexts associated with this instance
    // - doesn't match necessary the number of threads of the process, but the
    // threads which are still marked as active for this TSynLog
    // - a huge number may therefore not indicate a potential "out of memory"
    // error, but a broken logic with missing NotifyThreadEnded calls
    property ThreadContextCount: integer
      read fThreadContextCount;
    /// the associated logging family
    property GenericFamily: TSynLogFamily
      read fFamily;
  end;

  TSynLogDynArray = array of TSynLog;

{$ifndef PUREMORMOT2}
const
  ptIdentifiedInOnFile = ptIdentifiedInOneFile;
{$endif PUREMORMOT2}


{ ************** High-Level Logs and Exception Related Features }

{$ifndef NOEXCEPTIONINTERCEPT}

var
  /// low-level variable used internally by this unit
  // - do not access this variable in your code: defined here to allow inlining
  GlobalCurrentHandleExceptionSynLog: TSynLog;

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

{$endif NOEXCEPTIONINTERCEPT}


type
  /// a mORMot-compatible calback definition
  // - used to notify a remote mORMot server via interface-based serivces
  // for any incoming event, using e.g. TSynLogCallbacks.Subscribe
  ISynLogCallback = interface(IInvokable)
    ['{9BC218CD-A7CD-47EC-9893-97B7392C37CF}']
    /// each line of the TTextWriter internal instance will trigger this method
    // - similar to TOnTextWriterEcho, as defined in mormot.core.text
    // - an initial call with Level=sllNone and the whole previous Text may be
    // transmitted, if ReceiveExistingKB is set for TSynLogCallbacks.Subscribe()
    procedure Log(Level: TSynLogInfo; const Text: RawUtf8);
  end;

  /// store a subscribe to ISynLogCallback
  TSynLogCallback = record
    Levels: TSynLogInfos;
    Callback: ISynLogCallback;
  end;

  /// store several subscribed ISynLogCallback
  TSynLogCallbackDynArray = array of TSynLogCallback;

  /// can manage a list of ISynLogCallback registrations
  TSynLogCallbacks = class(TSynLocked)
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
    function Subscribe(const Levels: TSynLogInfos; const Callback: ISynLogCallback;
      ReceiveExistingKB: cardinal = 0): integer; virtual;
    /// unregister a callback previously registered by Subscribe()
    procedure Unsubscribe(const Callback: ISynLogCallback); virtual;
    /// notify a given log event
    // - matches the TOnTextWriterEcho signature
    function OnEcho(Sender: TEchoWriter; Level: TSynLogInfo;
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
    fLevels: TSynLogInfos;
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
    property Levels: TSynLogInfos
      read fLevels write fLevels;
    /// allow to customize where the logs should be written
    // - default is the system log folder (e.g. /var/log on Linux)
    property DestinationPath: TFileName
      read fDestinationPath write fDestinationPath;
    /// how many files will be rotated (default is 2)
    property RotateFileCount: integer
      read fRotateFileCount write fRotateFileCount;
  end;


/// a TSynLogArchiveEvent handler which will delete older .log files
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

var
  /// define how .synlz files are compressed by TSynLog.PerformRotation
  // - assigned to AlgoSynLZ by default which is the fastest for logs
  // - you may set AlgoLizardFast or AlgoLizardHuffman as alternatives
  // (default AlgoLizard is much slower and less efficient on logs)
  // - consider AlgoDeflate which gives the best compression ratio and is also
  // very fast if libdeflate is available (e.g. on FPC x86_64)
  // - note that compression itself is run in the logging background thread
  LogCompressAlgo: TAlgoCompress;


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
    fLevels: TSynLogInfoDynArray;
    fThreads: TWordDynArray;
    fThreadInfo: array of record
      Rows: cardinal;
      SetThreadName: TPUtf8CharDynArray;
    end;
    fThreadInfoMax: cardinal;
    fThreadsCount: integer;
    fThreadMax: cardinal;
    fLineLevelOffset: cardinal;
    fLineTextOffset: cardinal;
    fLineHeaderCountToIgnore: integer;
    /// as extracted from the .log header
    fExeName, fExeVersion, fInstanceName: RawUtf8;
    fHost, fUser, fCPU, fOSDetailed, fFramework: RawUtf8;
    fExeDate: TDateTime;
    fOS: TWindowsVersion;
    fOSServicePack: integer;
    fWow64: boolean;
    fStartDateTime: TDateTime;
    fDayCurrent: Int64; // as PInt64('20160607')^
    fDayChangeIndex: TIntegerDynArray;
    fDayCount: TIntegerDynArray;
    /// retrieve all used event levels
    fLevelUsed: TSynLogInfos;
    /// =0 if date time resolution, >0 if high-resolution time stamp
    fFreq: Int64;
    /// used by EventDateTime() to compute date from time stamp
    fFreqPerDay: double;
    /// custom headers, to be searched as .ini content
    fHeaderLinesCount: integer;
    fHeaders: RawUtf8;
    /// method profiling data
    fLogProcCurrent: PSynLogFileProcArray;
    fLogProcCurrentCount: integer;
    fLogProcNatural: TSynLogFileProcDynArray;
    fLogProcNaturalCount: integer;
    fLogProcMerged: TSynLogFileProcDynArray;
    fLogProcMergedCount: integer;
    fLogProcIsMerged: boolean;
    fLogProcStack: array of array of cardinal;
    fLogProcStackCount: array of integer;
    fLogProcSortInternalOrder: TLogProcSortOrder;
    /// used by ProcessOneLine//GetLogLevelTextMap
    fLogLevelsTextMap: array[TSynLogInfo] of cardinal;
    fIntelCPU: TIntelCpuFeatures;
    fArm32CPU: TArm32HwCaps;
    fArm64CPU: TArm64HwCaps;
    procedure SetLogProcMerged(const Value: boolean);
    function GetEventText(index: integer): RawUtf8;
    function GetLogLevelFromText(LineBeg: PUtf8Char): TSynLogInfo;
    /// retrieve headers + fLevels[] + fLogProcNatural[], and delete invalid fLines[]
    procedure LoadFromMap(AverageLineLength: integer = 32); override;
    procedure CleanLevels;
    function ComputeProperTime(var procndx: PtrInt): cardinal; // returns leave
    /// compute fLevels[] + fLogProcNatural[] for each .log line during initial reading
    procedure ProcessOneLine(LineBeg, LineEnd: PUtf8Char); override;
    /// called by LogProcSort method
    function LogProcSortComp(A, B: PtrInt): PtrInt;
    procedure LogProcSortInternal(L, R: PtrInt);
  public
    /// initialize internal structure
    constructor Create; override;
    /// returns TRUE if the supplied text is contained in the corresponding line
    function LineContains(const aUpperSearch: RawUtf8; aIndex: integer): boolean; override;
    /// retrieve the date and time of an event
    // - returns 0 in case of an invalid supplied index
    function EventDateTime(aIndex: integer): TDateTime;
    /// retrieve the description text of an event, as native VCL string
    // - returns '' if supplied index is out of range
    // - if the text is not truly UTF-8 encoded, would use the current system
    // codepage to create a valid string
    // - you may specify a text to replace all #9 characters occurences
    // - is used e.g. in TMainLogView.ListDrawCell
    function EventString(index: integer; const replaceTabs: RawUtf8 = '';
      maxutf8len: integer = 0; includeFirstColumns: boolean = false): string;
    /// sort the LogProc[] array according to the supplied order
    procedure LogProcSort(Order: TLogProcSortOrder);
    /// return the number of matching events in the log
    function EventCount(const aSet: TSynLogInfos): integer;
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
    /// returns the number of occurences of a given thread
    function ThreadRows(ThreadID: integer): cardinal;
    /// retrieve the level of an event
    // - is calculated by Create() constructor
    // - EventLevel[] array index is from 0 to Count-1
    property EventLevel: TSynLogInfoDynArray
      read fLevels;
    /// retrieve all used event levels
    // - is calculated by Create() constructor
    property EventLevelUsed: TSynLogInfos
      read fLevelUsed;
    /// retrieve the description text of an event
    // - returns '' if supplied index is out of range
    // - see also EventString() function, for direct VCL use
    property EventText[index: integer]: RawUtf8
      read GetEventText;
    /// retrieve all event thread IDs
    // - contains something if TSynLogFamily.PerThreadLog was ptIdentifiedInOneFile
    // - for ptMergedInOneFile (default) or ptOneFilePerThread logging process,
    // the array will be void (EventThread=nil)
    property EventThread: TWordDynArray
      read fThreads;
    /// the number of threads
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
    property LevelUsed: TSynLogInfos
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
    fEvents: TSynLogInfos;
    fThreadSelected: TByteDynArray;
    fThreadSelectedMax: integer;
    procedure LoadFromMap(AverageLineLength: integer = 32); override;
    function GetThreads(thread: integer): boolean;
    procedure SetThreads(thread: integer; value: boolean);
  public
    /// add a new line to the already parsed content
    // - overriden method would add the inserted index to Selected[]
    procedure AddInMemoryLine(const aNewLine: RawUtf8); override;
    /// search for the next matching TSynLogInfo, from the current row index
    // - returns -1 if no match was found
    function SearchNextEvent(aEvent: TSynLogInfo; aRow: integer): PtrInt;
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
    function GetCell(aCol, aRow: integer; out aLevel: TSynLogInfo): string;
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
    property Events: TSynLogInfos
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
  LOG_TO_SYSLOG: array[TSynLogInfo] of TSyslogSeverity = (
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


implementation

{$ifdef FPC}
uses
  exeinfo; // cross-platform executable raw access for GDB DWARF support
{$endif FPC}


{ ************** Debug Symbols Processing from Delphi .map or FPC/GDB DWARF }

var
  ExeInstanceDebugFile: TDebugFile;

function GetInstanceDebugFile: TDebugFile;
var
  new: TDebugFile;
begin
  result := ExeInstanceDebugFile;
  if result <> nil then
    exit;
  new := TDebugFile.Create;
  EnterCriticalSection(GlobalThreadLock);
  if ExeInstanceDebugFile = nil then
    ExeInstanceDebugFile := new
  else
    new.Free;
  result := ExeInstanceDebugFile;
  LeaveCriticalSection(GlobalThreadLock);
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
    read: TFastReader;
    DebugLineSectionOffset, DebugLineSection_Size, // debug_line section
    DebugInfoSectionOffset, DebugInfoSection_Size, // debug_info section
    DebugAbbrevSectionOffset, DebugAbbrevSectionSize: integer; // debug_abbrev
    Abbrev: array of TDwarfDebugAbbrev; // debug_abbrev content
    Lines: TInt64DynArray;              // store TDebugUnit.Addr[]/Line[]
    dirs, files: TRawUtf8DynArray;
    isdwarf64, debugtoconsole: boolean;
    debug: TDebugFile;
    map: TMemoryMap;
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

{$I-}

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
  begin
    // optimize the most common case of -64..+63 range
    result := (not ((data and (Int64(1) shl 6)) - 1)) or data;
    exit;
  end;
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
  temp: AnsiChar;
  i: PtrUInt;
begin
  i := 0;
  while read.NextByteSafe(@temp) and
        ({%H-}temp <> #0) do
  begin
    inc(i);
    if i <= 255 then
      s[i] := temp;
  end;
  s[0] := AnsiChar(i);
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
  dirsn, filesn, linesn: integer;
  state: TDwarfMachineState;
  c: ansichar;
  unsorted: boolean;
  header64: TDwarfLineInfoHeader64;
  header32: TDwarfLineInfoHeader32;
  u: PDebugUnit;
  s: ShortString;
  filesdir: array[0..15] of byte;
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
      AppendShortChar(c, s);
    AddRawUtf8(dirs, dirsn, ShortStringToUtf8(s));
  until false;
  filesn := 0;
  repeat
    ReadString(s);
    if s[0] = #0 then
      break;
    if filesn <= high(filesdir) then
      filesdir[filesn] := read.VarUInt32;
    read.VarNextInt(2); // we ignore the attributes
    AddRawUtf8(files, filesn, ShortStringToUtf8(s));
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
          if (prevfile <= high(filesdir)) and
             ({%H-}filesdir[prevfile] > 0) then
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
  typname := '';
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
          if (typname <> '') and
             (typname[ord(typname[0])] <> '.') then
            AppendShortChar('.', typname);
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
        typname := '';
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
        typname := '';
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
      if not HexDisplayToBin(PAnsiChar(P) + 5, @Ptr, SizeOf(Ptr)) then
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
         HexDisplayToBin(PAnsiChar(P), @U.Symbol.Stop, 4) then
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
          inc(n); // occured with Delphi 2010 :(
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
  i, j: PtrInt;
  mapcontent: RawUtf8;
begin
  fSymbols.Capacity := 8000;
  mapcontent := StringFromFile(fDebugFile);
  // parse .map/.dbg sections into fSymbol[] and fUnit[]
  P := pointer(mapcontent);
  PEnd := P + length(mapcontent);
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
  MabFile := ChangeFileExt(fDebugFile, '.mab');
  if not FileExists(MabFile) then
    if not IsDirectoryWritable(ExtractFilePath(MabFile)) then
      // read/only exe folder -> store .mab in local non roaming user folder
      MabFile := IncludeTrailingPathDelimiter(GetSystemPath(spUserData)) +
                   ExtractFileName(Mabfile);
  EnterCriticalSection(GlobalThreadLock);
  try
    MapAge := FileAgeToUnixTimeUtc(fDebugFile);
    MabAge := FileAgeToUnixTimeUtc(MabFile);
    if (MapAge > 0) and
       (MabAge < MapAge) then
    begin
      // recompute from .map/.dbg if no faster-to-load .mab available
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
      {$ifndef FPC}
      for i := 1 to fSymbolsCount - 1 do
        if fSymbol[i].Start <= fSymbol[i - 1].Stop then
        begin
          // on Delphi, there should be no overlap
          fUnits.Clear;
          fSymbols.Clear;
          exit;
        end;
      {$endif FPC}
      if MabCreate then // just created from .map/.dbg -> create .mab file
        SaveToFile(MabFile);
      fHasDebugInfo := true;
    end
    else
      fDebugFile := '';
  finally
    LeaveCriticalSection(GlobalThreadLock);
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
  F: TFileStream;
begin
  if aFileName = '' then
    result := ChangeFileExt(GetModuleName(hInstance), '.mab')
  else
    result := aFileName;
  DeleteFile(result);
  F := TFileStream.Create(result, fmCreate);
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
  if result >= 0 then
  begin
    u := @fUnit[result];
    // unit found -> search line number
    if u^.Addr = nil then
      exit;
    max := DynArrayNotNilHigh(u^.Addr);
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
              (offset <= fUnit[DynArrayNotNilHigh(fUnit)].Symbol.Stop)) or
             ((fSymbol <> nil) and
              (offset >= fSymbol[0].Start) and
              (offset <= fSymbol[DynArrayNotNilHigh(fSymbol)].Stop)));
end;

class function TDebugFile.Log(W: TTextWriter; aAddressAbsolute: PtrUInt;
  AllowNotCodeAddr, SymbolNameNotFilename: boolean): boolean;
var
  u, s, Line, offset: integer;
  debug: TDebugFile;

  procedure AddHex;
  begin
    if AllowNotCodeAddr then
    begin
      W.AddPointer(aAddressAbsolute);
      W.Add(' ');
    end;
  end;

begin
  result := false;
  if (W = nil) or
     (aAddressAbsolute = 0) then
    exit;
  debug := ExeInstanceDebugFile;
  if debug = nil then
    debug := GetInstanceDebugFile;
  if (debug <> nil) and
     debug.HasDebugInfo then
  begin
    offset := debug.AbsoluteToOffset(aAddressAbsolute);
    s := debug.FindSymbol(offset);
    u := debug.FindUnit(offset, Line);
    if (s < 0) and
       (u < 0) then
    begin
      AddHex;
      exit;
    end;
    {$ifndef FPC}
    if (s >= 0) and
       not AllowNotCodeAddr and
       (FindPropName(['SynRtlUnwind', '@HandleAnyException',  'LogExcept',
         '@HandleOnException', 'ThreadWrapper', 'ThreadProc'],
         debug.Symbols[s].Name) >= 0) then
      // no stack trace within the Delphi exception interception functions
      exit;
    {$endif FPC}
    AddHex;
    if u >= 0 then
    begin
      if SymbolNameNotFilename then
        W.AddString(debug.Units[u].Symbol.Name)
      else
        W.AddString(debug.Units[u].FileName);
      W.Add(' ');
    end;
    if s >= 0 then
      W.AddString(debug.Symbols[s].Name);
    W.Add(' ');
    if Line > 0 then
    begin
      W.Add('(');
      W.Add(Line);
      W.Add(')', ' ');
    end;
    result := true;
  end
  else
    AddHex;
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
  AppendShortChar(' ', result);
  if u >= 0 then
  begin
    AppendShortAnsi7String(Units[u].FileName, result);
    AppendShortChar(' ', result);
  end
  else
    result[0] := #0;
  if s >= 0 then
    AppendShortAnsi7String(Symbols[s].Name, result);
  if line > 0 then
  begin
    AppendShort(' (', result);
    AppendShortInteger(line, result);
    AppendShortChar(')', result);
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
      if IdemPropNameU(fUnit[result].Symbol.Name, aUnitName) then
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
  _LogInfoText: array[TSynLogInfo] of RawUtf8;
  _LogInfoCaption: array[TSynLogInfo] of string;

function ToText(event: TSynLogInfo): RawUtf8;
begin
  result := _LogInfoText[event];
end;

function ToText(events: TSynLogInfos): ShortString;
begin
  GetSetNameShort(TypeInfo(TSynLogInfos), events, result, {trimleft=}true);
end;

function ToCaption(event: TSynLogInfo): string;
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

{$ifdef FPC}
type
  THeapInfo = function: string;

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
       KBNoSpace(MaxHeapSize), KBNoSpace(MaxHeapUsed)], result);
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
         KBNoSpace(TotalFree), KBNoSpace(FreeSmall), KBNoSpace(FreeBig),
         KBNoSpace(Unused), KBNoSpace(Overhead)], result)
    else
      result := '';
end;
{$endif FPC}


type
  /// an array to all available per-thread TSynLog instances
  TSynLogFileLookup = array[0..MAX_SYNLOGFAMILY] of TSynLog;

var
  /// internal list of registered TSynLogFamily instances
  // - up to MAX_SYNLOGFAMILY+1 families may be defined
  // - protected by GlobalThreadLock
  SynLogFamily: array of TSynLogFamily;

  /// internal list of created TSynLog instances, one per each log file on disk
  // - also used by AutoFlushProc() to get a global list of TSynLog instances
  // - protected by GlobalThreadLock
  SynLogFile: TSynLogDynArray;

threadvar
  /// each thread can access to its own TSynLog instance
  // - implements TSynLogFamily.PerThreadLog=ptOneFilePerThread option
  SynLogLookupThreadVar: TSynLogFileLookup;


type
  // RRD of last 128 lines to be sent to console (no need of older data)
  TAutoFlushThreadToConsole = record
    Safe: TLightLock;
    Next, Count: integer;
    Text: array[0..127] of RawUtf8; // must be a power-of-two length
    Color: array[0..127] of TConsoleColor;
  end;

  // cross-platform / cross-compiler TThread-based flush
  TAutoFlushThread = class(TThread)
  protected
    fEvent: TEvent;
    fToCompress: TFileName;
    fStartTix: Int64;
    fSecondElapsed: cardinal;
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
  fEvent := TEvent.Create(nil, false, false, '');
  fStartTix := mormot.core.os.GetTickCount64;
  inherited Create(false);
end;

destructor TAutoFlushThread.Destroy;
begin
  inherited Destroy;
  fEvent.Free;
end;

procedure TAutoFlushThread.AddToConsole(const s: RawUtf8; c: TConsoleColor);
begin
  with fToConsole do
  begin
    Safe.Lock;
    try
      Text[Next] := s;
      Color[Next] := c;
      Next := (Next + 1) and high(Text); // simple round-robin data buffer
      inc(Count);
    finally
      Safe.UnLock;
    end;
  end;
end;

procedure TAutoFlushThread.FlushConsole;
var
  i: PtrInt;
  c: TAutoFlushThreadToConsole;
begin
  fToConsole.Safe.Lock;
  try
    c := fToConsole; // thread-safe local copy
    Finalize(fToConsole.Text);
    fToConsole.Count := 0;
    fToConsole.Next := 0;
  finally
    fToConsole.Safe.UnLock;
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
  waitms, tix10: cardinal;
  files: TSynLogDynArray;
begin
  waitms := 1000;
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
      // 3. eventually flush log content to disk after AutoFlushTimeOut
      EnterCriticalSection(GlobalThreadLock);
      try
        if Terminated or
           SynLogFileFreeing then
          break;
        files := copy(SynLogFile); // don't slow down main logging process
      finally
        LeaveCriticalSection(GlobalThreadLock);
      end;
      if files <> nil then
      begin
        tix10 := mormot.core.os.GetTickCount64 shr 10;
        for i := 0 to high(files) do
          with files[i] do
            if Terminated or
               SynLogFileFreeing then
              // avoid GPF
              break
            else if (fNextFlushTix10 <> 0) and
                    (tix10 >= fNextFlushTix10) and
                    (fWriter <> nil) and
                    (fWriter.PendingBytes > 1) then
                Flush({forcediskwrite=}false); // write pending data
      end;
    except
      // on stability issue, try to identify this thread
      if not Terminated then
        try
          SetCurrentThreadName('log autoflush');
        except
          break;
        end;
    end;
  until Terminated;
  try
    if fToConsole.Count <> 0 then
      FlushConsole; // always write last pending console rows
  except
    ; // ignore any exception at shutdown
  end;
end;


{$ifndef NOEXCEPTIONINTERCEPT}

// this is the main entry point for all intercepted exceptions
procedure SynLogException(const Ctxt: TSynLogExceptionContext); forward;

threadvar
  /// each thread can have exceptions interception disabled
  // - as set by TSynLogFamily.ExceptionIgnoreCurrentThread property
  ExceptionIgnorePerThread: boolean;

{$endif NOEXCEPTIONINTERCEPT}


{ TSynLogFamily }

procedure TSynLogFamily.SetDestinationPath(const value: TFileName);
begin
  if value = '' then
    fDestinationPath := Executable.ProgramFilePath
  else
    fDestinationPath := IncludeTrailingPathDelimiter(value);
end;

procedure TSynLogFamily.SetLevel(aLevel: TSynLogInfos);
begin
  // ensure BOTH Enter+Leave are always selected at once, if any is set
  if sllEnter in aLevel then
    include(aLevel, sllLeave)
  else if sllLeave in aLevel then
    include(aLevel, sllEnter);
  fLevel := aLevel;
  {$ifndef NOEXCEPTIONINTERCEPT}
  // intercept exceptions, if necessary
  fHandleExceptions := (sllExceptionOS in aLevel) or
                       (sllException in aLevel);
  if fHandleExceptions and
     (GlobalCurrentHandleExceptionSynLog = nil) then
  begin
    SynLog; // force GlobalCurrentHandleExceptionSynLog assignment
    RawExceptionIntercept(SynLogException);
  end;
  {$endif NOEXCEPTIONINTERCEPT}
end;

procedure TSynLogFamily.SetEchoToConsole(aEnabled: TSynLogInfos);
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
  if self = nil then
    result := ''
  else
    result := ClassName;
end;

constructor TSynLogFamily.Create(aSynLog: TSynLogClass);
begin
  fSynLogClass := aSynLog;
  fIdent := ObjArrayAdd(SynLogFamily, self);
  fDestinationPath := Executable.ProgramFilePath; // use .exe path
  if not IsDirectoryWritable(fDestinationPath) then
    fDestinationPath := GetSystemPath(spLog); // fallback to a writable folder
  fDefaultExtension := '.log';
  fArchivePath := fDestinationPath;
  fArchiveAfterDays := 7;
  fRotateFileAtHour := -1;
  fBufferSize := 4096;
  fStackTraceLevel := 30;
  fWithUnitName := true;
  fWithInstancePointer := true;
  {$ifdef OSWINDOWS}
  fEchoToConsoleBackground := true; // big speed-up on Windows
  {$endif OSWINDOWS}
  fExceptionIgnore := TList.Create;
  fLevelStackTrace := [sllStackTrace, sllException, sllExceptionOS,
                       sllError, sllFail, sllLastError, sllDDDError];
end;

{$ifndef NOEXCEPTIONINTERCEPT}

function TSynLogFamily.GetExceptionIgnoreCurrentThread: boolean;
begin
  result := ExceptionIgnorePerThread;
end;

procedure TSynLogFamily.SetExceptionIgnoreCurrentThread(aExceptionIgnoreCurrentThread: boolean);
begin
  ExceptionIgnorePerThread := aExceptionIgnoreCurrentThread;
end;

{$endif NOEXCEPTIONINTERCEPT}

function TSynLogFamily.CreateSynLog: TSynLog;
begin
  if SynLogFileFreeing then
  begin
    result := nil;
    exit;
  end;
  EnterCriticalSection(GlobalThreadLock);
  try
    result := fSynLogClass.Create(self);
    ObjArrayAdd(SynLogFile, result);
    if fPerThreadLog = ptOneFilePerThread then
      if (fRotateFileCount = 0) and
         (fRotateFileSize = 0) and
         (fRotateFileAtHour < 0) and
         (fIdent <= MAX_SYNLOGFAMILY) then
        SynLogLookupThreadVar[fIdent] := result
      else
      begin
        fPerThreadLog := ptIdentifiedInOneFile; // rotation requires one file
        fGlobalLog := result;
      end
    else
      fGlobalLog := result;
  finally
    LeaveCriticalSection(GlobalThreadLock);
  end;
end;

procedure TSynLogFamily.StartAutoFlush;
begin
  if (AutoFlushThread = nil) and
     not SynLogFileFreeing and
     (fAutoFlushTimeOut <> 0)
     {$ifndef FPC} and (DebugHook = 0) {$endif} then
    AutoFlushThread := TAutoFlushThread.Create;
end;

destructor TSynLogFamily.Destroy;
var
  SR: TSearchRec;
  oldTime, aTime: TDateTime;
  Y, M, D: word;
  aOldLogFileName, aPath: TFileName;
  tmp: array[0..7] of AnsiChar;
begin
  fDestroying := true;
  EchoRemoteStop;
  ExceptionIgnore.Free;
  try
    if Assigned(OnArchive) then
      if FindFirst(fDestinationPath + '*' + fDefaultExtension, faAnyFile, SR) = 0 then
      try
        if ArchiveAfterDays < 0 then
          ArchiveAfterDays := 0;
        oldTime := Now - ArchiveAfterDays;
        repeat
          if (SR.Name[1] = '.') or
             (faDirectory and SR.Attr <> 0) then
            continue;
          aTime := SearchRecToDateTime(SR);
          if (aTime = 0) or
             (aTime > oldTime) then
            continue;
          aOldLogFileName := fDestinationPath + SR.Name;
          if {%H-}aPath = '' then
          begin
            aPath := EnsureDirectoryExists(ArchivePath + 'log');
            if aPath = '' then
              break; // impossible to create the archive folder
            DecodeDate(aTime, Y, M, D);
            YearToPChar(Y, @tmp[0]);
            PWord(@tmp[4])^ := TwoDigitLookupW[M];
            PWord(@tmp[6])^ := ord(PathDelim);
            aPath := aPath + Ansi7ToString(tmp, 7);
          end;
          OnArchive(aOldLogFileName, aPath);
        until FindNext(SR) <> 0;
      finally
        try
          OnArchive('', aPath); // mark no more .log file to archive -> close .zip
        finally
          FindClose(SR);
        end;
      end;
  finally
    inherited Destroy;
  end;
end;

function TSynLogFamily.SynLog: TSynLog;
begin
  if self <> nil then
  begin
    result := fGlobalLog;
    if result <> nil then
      // ptMergedInOneFile and ptIdentifiedInOneFile (most common case)
      exit;
    if (fPerThreadLog = ptOneFilePerThread) and
       (fRotateFileCount = 0) and
       (fRotateFileSize = 0) and
       (fRotateFileAtHour < 0) and
       (fIdent <= MAX_SYNLOGFAMILY) then
    begin
      // unrotated ptOneFilePerThread
      result := SynLogLookupThreadVar[fIdent];
      if result = nil then
        result := CreateSynLog;
    end
    else
      // new ptMergedInOneFile or ptIdentifiedInOneFile
      result := CreateSynLog;
    {$ifndef NOEXCEPTIONINTERCEPT}
    // we should check this now, so that any exception is handled in this log
    if fHandleExceptions and
       (GlobalCurrentHandleExceptionSynLog <> result) then
      GlobalCurrentHandleExceptionSynLog := result;
    {$endif NOEXCEPTIONINTERCEPT}
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
     (SynLogFile = nil) or
     (not Assigned(aEvent)) then
    exit;
  EnterCriticalSection(GlobalThreadLock);
  try
    for i := 0 to high(SynLogFile) do
      with SynLogFile[i] do
        if fFamily = self then
          if aEventAdd then
            fWriterEcho.EchoAdd(aEvent)
          else
            fWriterEcho.EchoRemove(aEvent);
  finally
    LeaveCriticalSection(GlobalThreadLock);
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
  // a 128 MB RawUtf8 is fair enough
  MAXPREVIOUSCONTENTSIZE = 128 shl 20;
var
  log: TSynLog;
  stream: TFileStream;
  endpos, start: Int64;
  c: AnsiChar;
  i, len, read, total: integer;
  P: PAnsiChar;
begin
  result := '';
  if SynLogFile = nil then
    exit;
  EnterCriticalSection(GlobalThreadLock);
  try
    for i := 0 to high(SynLogFile) do
    begin
      log := SynLogFile[i];
      if log.fFamily <> self then
        continue;
      log.Writer.FlushToStream;
      if log.Writer.Stream.InheritsFrom(TFileStream) then
      begin
        stream := TFileStream(log.Writer.Stream);
        endpos := stream.Position;
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
            stream.Position := start;
            repeat
              inc(start)
            until (stream.Read(c, 1) = 0) or
                  (ord(c) in [10, 13]);
          end
          else
            stream.Position := start;
          len := endpos - start;
          SetLength(result, len);
          P := pointer(result);
          total := 0;
          repeat
            read := stream.Read(P^, len);
            if read <= 0 then
            begin
              if total <> len then
                SetLength(result, total); // truncate on read error
              break;
            end;
            inc(P, read);
            dec(len, read);
            inc(total, read);
          until len = 0;
        finally
          stream.Position := endpos;
        end;
      end;
      break;
    end;
  finally
    LeaveCriticalSection(GlobalThreadLock);
  end;
end;

procedure TSynLogFamily.OnThreadEnded(Sender: TThread);
begin
  SynLog.NotifyThreadEnded;
end;


{ TFileStreamWithoutWriteError }

type
  /// file stream which ignores I/O write errors
  // - in case disk space is exhausted, TFileStreamWithoutWriteError.WriteBuffer
  // won't throw any exception, so application will continue to work
  // - used by TSynLog to let the application continue with no exception,
  // even in case of a disk/partition full of logs
  TFileStreamWithoutWriteError = class(TFileStream)
  public
    /// this overriden function returns Count, as if it was always sucessfull
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

function TFileStreamWithoutWriteError.Write(const Buffer; Count: Longint): Longint;
begin
  inherited Write(Buffer, Count);
  result := Count; // ignore I/O errors
end;


{ TSynLog }

class function TSynLog.Family: TSynLogFamily;
begin
  result := pointer(Self);
  if result <> nil then
  begin
    // inlined Rtti.Find(ClassType)
    result := PPointer(PAnsiChar(result) + vmtAutoTable)^;
    if result <> nil then
      // we know TRttiCustom is in the slot, and PrivateSlot as TSynLogFamily
      result := TRttiCustom(pointer(result)).PrivateSlot;
    if result = nil then
      // register the TSynLogFamily to the TRttiCustom.Private field
      result := FamilyCreate;
  end;
end;

class function TSynLog.Add: TSynLog;
var
  P: pointer;
label
  sl;
begin
  // inlined TSynLog.Family with direct fGlobalLog check
  result := pointer(Self);
  if result <> nil then
  begin
    P := PPointer(PAnsiChar(result) + vmtAutoTable)^;
    if P <> nil then
    begin
      // we know TRttiCustom is in the slot, and Private is TSynLogFamily
      P := TRttiCustom(P).PrivateSlot;
      result := TSynLogFamily(P).fGlobalLog;
      // <>nil for ptMergedInOneFile and ptIdentifiedInOneFile (most common case)
      if result = nil then
        goto sl;
    end
    else
    begin
      P := FamilyCreate;
sl:   result := TSynLogFamily(P).SynLog;
    end;
  end;
end;

class function TSynLog.FamilyCreate: TSynLogFamily;
var
  rtticustom: TRttiCustom;
  vmt: TObject;
begin
  // private sub function called from inlined TSynLog.Family / TSynLog.Add
  if (self <> nil) and
     InheritsFrom(TSynLog) then // paranoid
  begin
    rtticustom := Rtti.RegisterClass(self);
    vmt := PPointer(PAnsiChar(self) + vmtAutoTable)^;
    if (rtticustom = nil) or
       (vmt <> rtticustom) then
      // TSynLog.Family / TSynLog.Add expect TRttiCustom in the first slot
      raise ESynLogException.CreateUtf8(
        '%.FamilyCreate: vmtAutoTable=% not %', [self, vmt, rtticustom]);
    EnterCriticalSection(Rtti.RegisterLock);
    try
      result := rtticustom.PrivateSlot;
      if Assigned(result) then
        if result.InheritsFrom(TSynLogFamily) then
          // registered by a background thread
          exit
        else
          raise ESynLogException.CreateUtf8( // paranoid
            '%.FamilyCreate: vmtAutoTable=%', [self, result]);
      // create the TSynLogFamily instance associated with this TSynLog class
      result := TSynLogFamily.Create(self); // stored in SynLogFamily[]
      rtticustom.PrivateSlot := result; // will be owned by this TRttiCustom
    finally
      LeaveCriticalSection(Rtti.RegisterLock);
    end;
  end
  else
    result := nil;
end;

function TSynLog.GetThreadContext: PSynLogThreadContext;
var
  id: TThreadID;
begin
  id := GetCurrentThreadId;
  // most of the time, the thread didn't change so this method is inlined
  if id <> fThreadID then
    // quickly switch fThreadContext/fThreadIndex to the new thread
    GetThreadContextInternal(PtrUInt(id));
  result := fThreadContext;
end;

procedure TSynLog.GetThreadContextAndDisableExceptions;
var
  id: TThreadID;
begin
  id := GetCurrentThreadId;
  // most of the time, the thread didn't change so this method is inlined
  if id <> fThreadID then
    // quickly switch fThreadContext/fThreadIndex to the new thread
    GetThreadContextInternal(PtrUInt(id))
  {$ifndef NOEXCEPTIONINTERCEPT}
  else
    fExceptionIgnoredBackup := fExceptionIgnoreThreadVar^;
  // caller should always perform in its finally ... end block an eventual:
  // fExceptionIgnoreThreadVar^ := fExceptionIgnoredBackup;
  fExceptionIgnoreThreadVar^ := true;
  // any exception within logging process will be ignored from now on
  {$endif NOEXCEPTIONINTERCEPT}
end;

function TSynLog.NewRecursion: PSynLogThreadRecursion;
var
  c: PSynLogThreadContext;
begin
  c := GetThreadContext;
  if c^.RecursionCount = c^.RecursionCapacity then
  begin
    c^.RecursionCapacity := NextGrow(c^.RecursionCapacity);
    SetLength(c^.Recursion, c^.RecursionCapacity);
  end;
  result := @c^.Recursion[c^.RecursionCount];
  {$ifndef FPC}
  result^.Caller := 0; // no stack trace by default
  {$endif FPC}
  result^.RefCount := 0;
  inc(c^.RecursionCount);
end;

procedure TSynLog.LogTrailer(Level: TSynLogInfo);
begin
  if Level in fFamily.fLevelStackTrace then
    AddStackTrace(Level, nil);
  fWriterEcho.AddEndOfLine(fCurrentLevel);
  if (fFileRotationNextHour <> 0) and
     (GetTickCount64 >= fFileRotationNextHour) then
  begin
    inc(fFileRotationNextHour, MSecsPerDay);
    PerformRotation;
  end
  else if (fFileRotationSize > 0) and
          (fWriter.WrittenBytes > fFileRotationSize) then
    PerformRotation;
end;

const
  // would handle up to 4096 threads, using 8 KB of RAM for the hash table
  MAXLOGTHREADBITS = 12;

  // maximum of thread IDs which can exist for a process
  // - shall be a power of 2 (used for internal TSynLog.fThreadHash)
  // - with the default 1MB stack size, max is around 2000 threads for Win32
  // - thread IDs are recycled when released via TSynLog.NotifyThreadEnded
  MAXLOGTHREAD = 1 shl MAXLOGTHREADBITS;

procedure TSynLog.GetThreadContextInternal(id: PtrUInt);
var
  secondpass: boolean;
  hash: PtrUInt;
begin
  // method called when the thread context was changed
  fThreadID := TThreadID(id);
  {$ifndef NOEXCEPTIONINTERCEPT}
  fExceptionIgnoreThreadVar := @ExceptionIgnorePerThread;
  fExceptionIgnoredBackup := fExceptionIgnoreThreadVar^;
  {$endif NOEXCEPTIONINTERCEPT}
  // hashing algorithm should match TSynLog.ThreadContextRehash
  if fFamily.fPerThreadLog <> ptNoThreadProcess then
  begin
    secondpass := false;
    // efficient TThreadID hash on all architectures
    hash := 0;
    repeat
      hash := hash xor id;
      id := id shr (MAXLOGTHREADBITS - 1); // -1 for less collisions under Linux
    until id = 0; // on Windows, a single loop iteration is enough
    hash := hash and (MAXLOGTHREAD - 1);
    fThreadLastHash := hash;
    fThreadIndex := fThreadHash[hash];
    // fast O(1) loookup of the associated thread context
    if fThreadIndex <> 0 then
      repeat
        fThreadContext := @fThreadContexts[fThreadIndex - 1];
        if fThreadContext^.ID = fThreadID then
          // ThreadID found (very likely)
          exit;
        // hash collision -> try next item in fThreadHash[] if possible
        if fThreadLastHash = MAXLOGTHREAD - 1 then
          if secondpass then // avoid endless loop -> reuse last fThreadHash[]
            exit
          else
          begin
            fThreadLastHash := 0;
            secondpass := true;
          end
        else
          inc(fThreadLastHash);
        fThreadIndex := fThreadHash[fThreadLastHash];
      until fThreadIndex = 0;
    // here we know that fThreadIndex=fThreadHash[hash]=0 -> register the thread
    if fThreadIndexReleasedCount > 0 then
    begin
      // reuse an available NotifyThreadEnded() index
      dec(fThreadIndexReleasedCount);
      fThreadIndex := fThreadIndexReleased[fThreadIndexReleasedCount];
    end
    else
    begin
      // we need a new entry in the internal list
      if fThreadContextCount >= length(fThreadContexts) then
        SetLength(fThreadContexts, fThreadContextCount + 128);
      inc(fThreadContextCount);
      fThreadIndex := fThreadContextCount;
    end;
    fThreadHash[fThreadLastHash] := fThreadIndex;
  end
  else
    // ptNoThreadProcess will reuse the first context (assume a few threads)
    fThreadIndex := 1;
  // if we reach here, this is either the first time for this thread,
  // or we have a single context (ptNoThreadProcess) which needs to be updated
  fThreadContext := @fThreadContexts[fThreadIndex - 1];
  fThreadContext^.ID := fThreadID;
  if (fFamily.fPerThreadLog = ptIdentifiedInOneFile) and
     (fThreadContext^.ThreadName = '') and
     (sllInfo in fFamily.fLevel) and
     (CurrentThreadName[0] <> #0) then
    // set fThreadContext^.ThreadName, and log it (if not already)
    LogThreadName('');
end;

procedure TSynLog.ThreadContextRehash;
var
  i: integer;
  id, hash: PtrUInt;
  secondpass: boolean;
  ctxt: PSynLogThreadContext;
begin
  // hashing algorithm should match TSynLog.GetThreadContextInternal
  if fFamily.fPerThreadLog = ptNoThreadProcess then
    exit;
  FillcharFast(fThreadHash[0], MAXLOGTHREAD * SizeOf(fThreadHash[0]), 0);
  ctxt := pointer(fThreadContexts);
  for i := 1 to fThreadContextCount do // i > 0 to be stored in fThreadHash[]
  begin
    id := PtrUInt(ctxt^.id); // TThreadID  = ^TThreadRec under BSD
    if id <> 0 then
    begin
      // not empty slot
      hash := 0; // efficient TThreadID hash on all architectures
      repeat
        hash := hash xor id;
        id := id shr (MAXLOGTHREADBITS - 1); // -1 for less collisions on Linux
      until id = 0;
      hash := hash and (MAXLOGTHREAD - 1);
      secondpass := false;
      repeat
        if fThreadHash[hash] = 0 then
          // found a void entry to be used for this thread ID
          break;
        // hash collision (no need to check the ID here)
        if hash = MAXLOGTHREAD - 1 then
          if secondpass then // avoid endless loop
            break
          else
          begin
            hash := 0;
            secondpass := true;
          end
        else
          inc(hash);
      until false;
      fThreadHash[hash] := i;
    end;
    inc(ctxt);
  end;
end;

procedure TSynLog.NotifyThreadEnded;
begin
  CurrentThreadName[0] := #0; // reset threadvar
  if (self = nil) or
     (fThreadContextCount = 0) then
    exit; // nothing to release
  EnterCriticalSection(GlobalThreadLock);
  try
    Finalize(GetThreadContext^);
    FillcharFast(fThreadContext^, SizeOf(fThreadContext^), 0);
    ThreadContextRehash; // fThreadHash[fThreadLastHash] := 0 is not enough
    if fThreadIndexReleasedCount >= length(fThreadIndexReleased) then
      SetLength(fThreadIndexReleased, fThreadIndexReleasedCount + 128);
    fThreadIndexReleased[fThreadIndexReleasedCount] := fThreadIndex;
    inc(fThreadIndexReleasedCount); // allow naive but very efficient reuse
  finally
    LeaveCriticalSection(GlobalThreadLock);
  end;
end;

function TSynLog._AddRef: TIntCnt;
var
  c: PSynLogThreadContext;
  r: PSynLogThreadRecursion;
begin
  result := 1; // should never be 0 (would release TSynLog instance)
  if fFamily.Level * [sllEnter, sllLeave] <> [] then
  begin
    EnterCriticalSection(GlobalThreadLock);
    try
      c := GetThreadContext;
      if c^.RecursionCount > 0 then
      begin
        r := @c^.Recursion[c^.RecursionCount - 1];
        if (r^.RefCount = 0) and
           (sllEnter in fFamily.Level) then
        begin
          LogHeader(sllEnter);
          AddRecursion(c^.RecursionCount - 1, sllEnter);
        end;
        inc(r^.RefCount);
        result := r^.RefCount;
      end;
    finally
      LeaveCriticalSection(GlobalThreadLock);
    end
  end
end;

function TSynLog._Release: TIntCnt;
var
  c: PSynLogThreadContext;
  r: PSynLogThreadRecursion;
begin
  result := 1;
  if fFamily.Level * [sllEnter, sllLeave] <> [] then
  begin
    EnterCriticalSection(GlobalThreadLock);
    try
      c := GetThreadContext;
      if c^.RecursionCount > 0 then
      begin
        r := @c^.Recursion[c^.RecursionCount - 1];
        dec(r^.RefCount);
        if r^.RefCount = 0 then
        begin
          if sllLeave in fFamily.Level then
          begin
            LogHeader(sllLeave);
            AddRecursion(c^.RecursionCount - 1, sllLeave);
          end;
          dec(c^.RecursionCount);
        end;
        result := r^.RefCount;
      end;
    finally
      LeaveCriticalSection(GlobalThreadLock);
    end;
  end;
end;

constructor TSynLog.Create(aFamily: TSynLogFamily);
begin
  if aFamily = nil then
    aFamily := Family;
  fFamily := aFamily;
  SetLength(fThreadHash, MAXLOGTHREAD); // 8 KB buffer
  SetLength(fThreadContexts, 128);
end;

destructor TSynLog.Destroy;
begin
  {$ifndef NOEXCEPTIONINTERCEPT}
  if fFamily.fHandleExceptions and
     (GlobalCurrentHandleExceptionSynLog = self) then
    GlobalCurrentHandleExceptionSynLog := nil;
  {$endif NOEXCEPTIONINTERCEPT}
  Flush({forcediskwrite=}true);
  fWriterEcho.Free;
  fWriter.Free;
  fWriterStream.Free;
  inherited;
end;

procedure TSynLog.CloseLogFile;
begin
  if fWriter = nil then
    exit;
  EnterCriticalSection(GlobalThreadLock);
  try
    fWriter.FlushFinal;
    FreeAndNilSafe(fWriterEcho);
    FreeAndNilSafe(fWriter);
    FreeAndNilSafe(fWriterStream);
  finally
    LeaveCriticalSection(GlobalThreadLock);
  end;
end;

procedure TSynLog.Release;
begin
  EnterCriticalSection(GlobalThreadLock);
  try
    CloseLogFile;
    ObjArrayDelete(SynLogFile, self);
    if (fFamily.fPerThreadLog = ptOneFilePerThread) and
       (fFamily.fIdent <= MAX_SYNLOGFAMILY) then
      SynLogLookupThreadVar[fFamily.fIdent] := nil;
  finally
    LeaveCriticalSection(GlobalThreadLock);
  end;
  Free;
end;

procedure TSynLog.Flush(ForceDiskWrite: boolean);
var
  diskflush: THandle;
begin
  if fWriter = nil then
    exit;
  diskflush := 0;
  EnterCriticalSection(GlobalThreadLock);
  try
    fWriter.FlushToStream;
    if ForceDiskWrite and
       fWriterStream.InheritsFrom(TFileStream) then
      diskflush := TFileStream(fWriterStream).Handle;
    if AutoFlushThread = nil then
      fFamily.StartAutoFlush;
    fNextFlushTix10 := fFamily.AutoFlushTimeOut;
    if fNextFlushTix10 <> 0 then
      inc(fNextFlushTix10, mormot.core.os.GetTickCount64 shr 10);
  finally
    LeaveCriticalSection(GlobalThreadLock);
  end;
  if diskflush <> 0 then
    FlushFileBuffers(diskflush); // slow OS operation outside of the main lock
end;

function TSynLog.QueryInterface(
  {$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif} iid: TGuid;
  out obj): TIntQry;
begin
  result := E_NOINTERFACE;
end;


{$ifndef FPC}
  {$STACKFRAMES ON} // we need a stack frame for ebp/RtlCaptureStackBackTrace
  {$ifdef CPU64}
    {$define USERTLCAPTURESTACKBACKTRACE}
  {$else}
    {$define USEASMX86STACKBACKTRACE}
  {$endif CPU64}
{$endif FPC}

class function TSynLog.Enter(aInstance: TObject; aMethodName: PUtf8Char;
  aMethodNameLocal: boolean): ISynLog;
var
  log: TSynLog;
  r: PSynLogThreadRecursion;
  {$ifndef FPC}
  addr: PtrUInt;
  {$endif FPC}
begin
  log := Add;
  if (log <> nil) and
     (sllEnter in log.fFamily.fLevel) then
  begin
    {$ifndef FPC}
    addr := 0;
    if aMethodName = nil then
    begin
      {$ifdef USERTLCAPTURESTACKBACKTRACE}
      if RtlCaptureStackBackTrace(1, 1, @addr, nil) = 0 then
        addr := 0;
      {$endif USERTLCAPTURESTACKBACKTRACE}
      {$ifdef USEASMX86STACKBACKTRACE}
      asm
        mov  eax, [ebp + 4] // retrieve caller EIP from push ebp; mov ebp,esp
        mov  addr, eax
      end;
      {$endif USEASMX86STACKBACKTRACE}
      if addr <> 0 then
        dec(addr, 5);
    end;
    {$endif FPC}
    EnterCriticalSection(GlobalThreadLock);
    {$ifdef HASFASTTRYFINALLY}
    try
    {$else}
    begin
    {$endif HASFASTTRYFINALLY}
      r := log.NewRecursion;
      r^.Instance := aInstance;
      r^.MethodName := aMethodName;
      if aMethodNameLocal then
        r^.MethodNameLocal := mnEnter
      else
        r^.MethodNameLocal := mnAlways;
      {$ifndef FPC}
      r^.Caller := addr;
      {$endif FPC}
    {$ifdef HASFASTTRYFINALLY}
    finally
    {$endif HASFASTTRYFINALLY}
      LeaveCriticalSection(GlobalThreadLock);
    end;
  end;
  // copy to ISynLog interface -> will call TSynLog._AddRef
  result := log;
end;

{$STACKFRAMES OFF}

class function TSynLog.Enter(const TextFmt: RawUtf8; const TextArgs: array of const;
  aInstance: TObject): ISynLog;
var
  log: TSynLog;
  tmp: pointer;
  r: PSynLogThreadRecursion;
begin
  log := Add;
  if (log <> nil) and
     (sllEnter in log.fFamily.fLevel) then
  begin
    tmp := nil; // avoid GPF on next line
    FormatUtf8(TextFmt, TextArgs, RawUtf8(tmp)); // compute outside lock
    EnterCriticalSection(GlobalThreadLock);
    {$ifdef HASFASTTRYFINALLY}
    try
    {$else}
    begin
    {$endif HASFASTTRYFINALLY}
      r := log.NewRecursion;
      r^.Instance := aInstance;
      r^.MethodNameLocal := mnEnterOwnMethodName;
      r^.MethodName := tmp;
    {$ifdef HASFASTTRYFINALLY}
    finally
    {$endif HASFASTTRYFINALLY}
      LeaveCriticalSection(GlobalThreadLock);
    end;
  end;
  // copy to ISynLog interface -> will call TSynLog._AddRef
  result := log;
end;

procedure TSynLog.ManualEnter(aMethodName: PUtf8Char; aInstance: TObject);
var
  r: PSynLogThreadRecursion;
begin
  if (self = nil) or
     (fFamily.fLevel * [sllEnter, sllLeave] = []) then
    exit;
  if aMethodName = nil then
    aMethodName := ' '; // something non void (call stack is irrelevant)
  EnterCriticalSection(GlobalThreadLock);
  try
    r := NewRecursion;
    // inlined TSynLog.Enter
    r^.Instance := aInstance;
    r^.MethodName := aMethodName;
    r^.MethodNameLocal := mnEnter;
    // inlined TSynLog._AddRef
    if sllEnter in fFamily.Level then
    begin
      LogHeader(sllEnter);
      AddRecursion(fThreadContext^.RecursionCount - 1, sllEnter);
    end;
    inc(r^.RefCount);
  finally
    LeaveCriticalSection(GlobalThreadLock);
  end;
end;

procedure TSynLog.ManualLeave;
begin
  if self <> nil then
    _Release;
end;

function TSynLog.LastQueryPerformanceMicroSeconds: Int64;
begin
  if (self = nil) or
     (fCurrentTimestamp = 0) then
    result := 0
  else
    result := fCurrentTimestamp + fStartTimestamp;
end;

type
  TSynLogVoid = class(TSynLog);

class function TSynLog.Void: TSynLogClass;
begin
  TSynLogVoid.Family.Level := [];
  result := TSynLogVoid;
end;

function TSynLog.Instance: TSynLog;
begin
  result := self;
end;

{$ifdef OSLINUX}
procedure SystemdEcho(Level: TSynLogInfo; const Text: RawUtf8);
var
  tmp: TShort16;
  mtmp: RawUtf8;
  jvec: array[0..1] of TIoVec;
const
  _MESSAGE: array[0..7] of AnsiChar = 'MESSAGE=';
begin
  if (length(Text) < 18) or
     not sd.IsAvailable then
    // should be at last "20200615 08003008  "
    exit;
  FormatShort16('PRIORITY=%', [LOG_TO_SYSLOG[Level]], tmp);
  jvec[0].iov_base := @tmp[1];
  jvec[0].iov_len := length(tmp);
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

function TSynLog.ConsoleEcho(Sender: TEchoWriter; Level: TSynLogInfo;
  const Text: RawUtf8): boolean;
begin
  result := true;
  if not (Level in fFamily.fEchoToConsole) then
    exit;
  {$ifdef OSLINUX}
  if Family.EchoToConsoleUseJournal then
  begin
    SystemdEcho(Level, Text);
    exit;
  end;
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

procedure TSynLog.Log(Level: TSynLogInfo; const TextFmt: RawUtf8;
  const TextArgs: array of const; aInstance: TObject);
begin
  if (self <> nil) and
     (Level in fFamily.fLevel) then
    LogInternalFmt(Level, TextFmt, TextArgs, aInstance);
end;

procedure TSynLog.Log(Level: TSynLogInfo; const TextFmt: RawUtf8;
  const TextArg: RawUtf8; aInstance: TObject);
begin
  if (self <> nil) and
     (Level in fFamily.fLevel) then
    LogInternalFmt(Level, TextFmt, [TextArg], aInstance);
end;

procedure TSynLog.Log(Level: TSynLogInfo; const TextFmt: RawUtf8;
  const TextArg: Int64; aInstance: TObject);
begin
  if (self <> nil) and
     (Level in fFamily.fLevel) then
    LogInternalFmt(Level, TextFmt, [TextArg], aInstance);
end;

procedure TSynLog.Log(Level: TSynLogInfo; const Text: RawUtf8;
  aInstance: TObject; TextTruncateAtLength: integer);
begin
  if (self <> nil) and
     (Level in fFamily.fLevel) then
    LogInternalText(Level, Text, aInstance, TextTruncateAtLength);
end;

{$ifdef UNICODE}
procedure TSynLog.Log(Level: TSynLogInfo; const Text: string; aInstance: TObject);
begin
  if (self <> nil) and
     (Level in fFamily.fLevel) then
    LogInternalFmt(Level, '%', [Text], aInstance);
end;
{$endif UNICODE}

procedure TSynLog.LogLines(Level: TSynLogInfo; LinesToLog: PUtf8Char;
  aInstance: TObject; const IgnoreWhenStartWith: PAnsiChar);

  procedure DoLog(LinesToLog: PUtf8Char);
  var
    s: RawUtf8;
  begin
    repeat
      GetNextItemTrimedCRLF(LinesToLog, s);
      if s <> '' then
        if (IgnoreWhenStartWith = nil) or
           not IdemPChar(pointer(s), IgnoreWhenStartWith) then
          LogInternalText(Level, s, aInstance, maxInt);
    until LinesToLog = nil;
  end;

begin
  if (self <> nil) and
     (Level in fFamily.fLevel) and
     (LinesToLog <> nil) then
    DoLog(LinesToLog);
end;

procedure TSynLog.LogThreadName(const Name: RawUtf8);
var
  n: RawUtf8;
begin
  if (self <> nil) and
     (sllInfo in fFamily.fLevel) and
     (fFamily.fPerThreadLog = ptIdentifiedInOneFile) then
  begin
    if Name = '' then
      n := GetCurrentThreadName
    else
      n := Name;
    EnterCriticalSection(GlobalThreadLock);
    try
      if GetThreadContext^.ThreadName = n then
        exit;
      fThreadContext^.ThreadName := n;
      LogHeader(sllInfo);
      fWriter.AddShort('SetThreadName ');
      fWriter.AddPointer(PtrUInt(fThreadID));
      fWriter.Add('=');
      fWriter.AddString(n);
      LogTrailer(sllInfo);
    finally
      LeaveCriticalSection(GlobalThreadLock);
    end;
  end;
end;

function TSynLog.LogClass: TSynLogClass;
begin
  if self = nil then
    result := nil
  else
    result := PPointer(self)^;
end;

class procedure TSynLog.DoLog(Level: TSynLogInfo; const Fmt: RawUtf8;
   const Args: array of const; Instance: TObject);
var
  log: TSynLog;
begin
  log := Add;
  if (log <> nil) and
     (Level in log.fFamily.fLevel) then
  begin
    log.LogInternalFmt(Level, Fmt, Args, Instance);
    if Level = sllExceptionOS then
      // ensure all log is safely written
      log.Flush({diskwrite=}true);
  end;
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
  EnterCriticalSection(GlobalThreadLock);
  try
    PerformRotation;
  finally
    LeaveCriticalSection(GlobalThreadLock);
  end;
end;

procedure TSynLog.DisableRemoteLog(value: boolean);
begin
  if (fDisableRemoteLog = value) or
     (not Assigned(fFamily.fEchoRemoteEvent)) then
    exit;
  if value then
  begin
    // fDisableRemoteLog=false -> remove from events, within the global mutex
    EnterCriticalSection(GlobalThreadLock);
    if fDisableRemoteLog = value then // unlikely set in-between
      LeaveCriticalSection(GlobalThreadLock)
    else
    begin
      fDisableRemoteLog := true;
      fWriterEcho.EchoRemove(fFamily.fEchoRemoteEvent);
    end;
  end
  else
  begin
    // fDisableRemoteLog=true -> add to events, already within the global mutex
    fDisableRemoteLog := false;
    fWriterEcho.EchoAdd(fFamily.fEchoRemoteEvent);
    LeaveCriticalSection(GlobalThreadLock);
  end;
end;

procedure TSynLog.Log(Level: TSynLogInfo; aInstance: TObject);
begin
  if (self <> nil) and
     (Level in fFamily.fLevel) and
     (aInstance <> nil) then
    LogInternalText(Level, '', aInstance, maxInt);
end;

procedure TSynLog.Log(Level: TSynLogInfo; const aName: RawUtf8;
  aTypeInfo: PRttiInfo; const aValue; Instance: TObject);
begin
  if (self <> nil) and
     (Level in fFamily.fLevel) then
    LogInternalRtti(Level, aName, aTypeInfo, aValue, Instance);
end;

{$ifndef FPC}
  {$STACKFRAMES ON} // we need a stack frame for ebp/RtlCaptureStackBackTrace
{$endif FPC}

procedure TSynLog.Log(Level: TSynLogInfo);
var
  lasterror: integer;
  {$ifndef FPC}
  addr: PtrUInt;
  {$endif FPC}
begin
  if (self <> nil) and
     (Level in fFamily.fLevel) then
  begin
    if Level = sllLastError then
      lasterror := GetLastError
    else
      lasterror := 0;
    EnterCriticalSection(GlobalThreadLock);
    try
      GetThreadContextAndDisableExceptions;
      LogHeader(Level);
      if lasterror <> 0 then
        AddErrorMessage(lasterror);
      {$ifndef FPC}
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
      {$endif FPC}
      LogTrailer(Level);
    finally
      {$ifndef NOEXCEPTIONINTERCEPT}
      fExceptionIgnoreThreadVar^ := fExceptionIgnoredBackup;
      {$endif NOEXCEPTIONINTERCEPT}
      LeaveCriticalSection(GlobalThreadLock);
      if lasterror <> 0 then
        SetLastError(lasterror);
    end;
  end;
end;

{$STACKFRAMES OFF}

{$ifdef CPU64DELPHI} // Delphi Win64 has no 64-bit inline assembler
procedure DebugBreak;
asm
     .noframe
     int  3
end;
{$endif CPU64DELPHI}

class procedure TSynLog.DebuggerNotify(Level: TSynLogInfo;
  const Format: RawUtf8; const Args: array of const);
var
  Msg: RawUtf8;
begin
  if Format <> '' then
  begin
    FormatUtf8(Format, Args, Msg);
    Add.LogInternalText(Level, Msg, nil, maxInt);
    {$ifdef OSWINDOWS}
    if IsDebuggerPresent then
      {$ifdef CPU64DELPHI}
      DebugBreak;
      {$else}
      asm
        int  3
      end;
      {$endif CPU64DELPHI}
    {$else not OSWINDOWS}
    ConsoleWrite('%  ', [Msg], LOG_CONSOLE_COLORS[Level], {noLF=}true);
    {$endif OSWINDOWS}
  end;
end;

procedure TSynLog.LogFileInit;
begin
  QueryPerformanceMicroSeconds(fStartTimestamp);
  if (fFileRotationSize > 0) or
     (fFileRotationNextHour <> 0) then
    fFamily.HighResolutionTimestamp := false;
  fStreamPositionAfterHeader := fWriter.WrittenBytes;
  if fFamily.LocalTimestamp then
    fStartTimestampDateTime := Now
  else
    fStartTimestampDateTime := NowUtc;
  Include(fInternalFlags, logInitDone);
end;

procedure TSynLog.LogFileHeader;
var
  WithinEvents: boolean;
  i: PtrInt;
  {$ifdef OSWINDOWS}
  Env: PWideChar;
  P: PWideChar;
  L: integer;
  {$endif OSWINDOWS}

  procedure NewLine;
  begin
    if WithinEvents then
    begin
      fWriterEcho.AddEndOfLine(sllNewRun);
      LogCurrentTime;
      fWriter.AddShorter(LOG_LEVEL_TEXT[sllNewRun]);
    end
    else
      fWriter.Add(#10);
  end;

begin
  WithinEvents := fWriter.WrittenBytes > 0;
  // array of const is buggy under Delphi 5 :( -> use fWriter.Add*() below
  if WithinEvents then
  begin
    LogCurrentTime;
    fWriter.AddShorter(LOG_LEVEL_TEXT[sllNewRun]);
    fWriter.AddChars('=', 50);
    NewLine;
  end;
  with Executable, fWriter do
  begin
    AddString(ProgramFullSpec);
    NewLine;
    AddShorter('Host=');
    AddString(Host);
    AddShorter(' User=');
    AddString(User);
    AddShorter(' CPU=');
    if CpuInfoText = '' then
      Add(SystemInfo.dwNumberOfProcessors)
    else
      for i := 1 to length(CpuInfoText) do
        if not (ord(CpuInfoText[i]) in [1..32, ord(':')]) then
          Add(CpuInfoText[i]);
    {$ifdef OSWINDOWS}
    with SystemInfo, OSVersionInfo do
    begin
      Add('*');
      Add(wProcessorArchitecture);
      Add('-');
      Add(wProcessorLevel);
      Add('-');
      Add(wProcessorRevision);
    {$endif OSWINDOWS}
    {$ifdef CPUINTEL}
      Add(':');
      AddBinToHex(@CpuFeatures, SizeOf(CpuFeatures));
    {$endif CPUINTEL}
    {$ifdef CPUARM3264}
      Add(':', {$ifdef CPUARM} '-' {$else} '+' {$endif}); // ARM marker
      AddBinToHex(@CpuFeatures, SizeOf(CpuFeatures));
    {$endif CPUARM3264}
      AddShorter(' OS=');
    {$ifdef OSWINDOWS}
      Add(ord(OSVersion));
      Add('.');
      Add(wServicePackMajor);
      Add('=');
      Add(dwMajorVersion);
      Add('.');
      Add(dwMinorVersion);
      Add('.');
      Add(dwBuildNumber);
    end;
    {$else}
    AddString(OS_NAME[OS_KIND]);
    Add('=');
    AddTrimSpaces(pointer(SystemInfo.uts.sysname));
    Add('-');
    AddTrimSpaces(pointer(SystemInfo.uts.release));
    AddReplace(pointer(SystemInfo.uts.version), ' ', '-');
    {$endif OSWINDOWS}
    if OSVersionInfoEx <> '' then
    begin
      Add('/');
      AddTrimSpaces(OSVersionInfoEx);
    end;
    AddShorter(' Wow64=');
    Add({$ifdef OSWINDOWS} integer(IsWow64) {$else} 0 {$endif});
    AddShort(' Freq=1000000'); // we use QueryPerformanceMicroSeconds()
    if IsLibrary then
    begin
      AddShort(' Instance=');
      AddNoJsonEscapeString(InstanceFileName);
    end;
    {$ifdef OSWINDOWS}
    if not fFamily.fNoEnvironmentVariable then
    begin
      NewLine;
      AddShort('Environment variables=');
      Env := GetEnvironmentStringsW;
      P := pointer(Env);
      while P^ <> #0 do
      begin
        L := StrLenW(P);
        if (L > 0) and
           (P^ <> '=') then
        begin
          AddNoJsonEscapeW(PWord(P), 0);
          Add(#9);
        end;
        inc(P, L + 1);
      end;
      FreeEnvironmentStringsW(Env);
      CancelLastChar(#9);
    end;
    {$endif OSWINDOWS}
    NewLine;
    AddClassName(self.ClassType);
    AddShort(' ' + SYNOPSE_FRAMEWORK_FULLVERSION + ' ');
    if fFamily.LocalTimestamp then
      AddDateTime(Now)
    else
      AddDateTime(NowUtc);
    if WithinEvents then
      fWriterEcho.AddEndOfLine(sllNone)
    else
      Add(#10, #10);
    FlushToStream;
    fWriterEcho.EchoReset; // header is not to be sent to console
  end;
  Include(fInternalFlags, logHeaderWritten);
  if not (logInitDone in fInternalFlags) then
    LogFileInit;
end;

procedure TSynLog.AddMemoryStats;
var
  info: TMemoryInfo; // cross-compiler and cross-platform
begin
  if GetMemoryInfo(info, {withalloc=}true) then
    fWriter.Add(
      ' System: memtotal=% memfree=% filetotal=% filefree=% allocres=% allocused=% ',
      [KBNoSpace(info.memtotal), KBNoSpace(info.memfree),
       KBNoSpace(info.filetotal), KBNoSpace(info.filefree),
       KBNoSpace(info.allocreserved), KBNoSpace(info.allocused)]);
  // include mormot.core.fpcx64mm raw information if available
  fWriter.AddNoJsonEscapeUtf8(RetrieveMemoryManagerInfo);
  fWriter.AddShorter('   ');
end;

procedure TSynLog.AddErrorMessage(Error: cardinal);
begin
  fWriter.Add(' ', '"');
  fWriter.AddOnSameLine(pointer(GetErrorText(Error)));
  fWriter.AddShorter('" (');
  fWriter.Add(Error);
  fWriter.Add(')', ' ');
end;

procedure TSynLog.LogCurrentTime;
begin
  if fFamily.HighResolutionTimestamp then
  begin
    QueryPerformanceMicroSeconds(fCurrentTimestamp);
    dec(fCurrentTimestamp, fStartTimestamp);
    fWriter.AddBinToHexDisplay(@fCurrentTimestamp, SizeOf(fCurrentTimestamp));
  end
  else
    fWriter.AddCurrentLogTime(fFamily.LocalTimestamp);
end;

procedure TSynLog.LogHeader(Level: TSynLogInfo);
var
  i: integer;
begin
  if fWriter = nil then
    CreateLogWriter; // file creation should be thread-safe
  if not (logHeaderWritten in fInternalFlags) then
    LogFileHeader
  else if not (logInitDone in fInternalFlags) then
    LogFileInit;
  if not (sllEnter in fFamily.Level) and
     (Level in fFamily.fLevelStackTrace) then
    // needed for pure ManualEnter with no Enter/Leave
    for i := 0 to fThreadContext^.RecursionCount - 1 do
    begin
      fWriter.AddChars(' ', i + 24 - byte(fFamily.HighResolutionTimestamp));
      AddRecursion(i, sllNone);
    end;
  LogCurrentTime;
  if fFamily.fPerThreadLog = ptIdentifiedInOneFile then
    fWriter.AddInt18ToChars3(fThreadIndex);
  fCurrentLevel := Level;
  fWriter.AddShorter(LOG_LEVEL_TEXT[Level]);
  i := fThreadContext^.RecursionCount;
  if i > 0 then
    fWriter.AddChars(#9, i - byte(Level in [sllEnter, sllLeave]));
  case Level of // handle additional information for some special error levels
    sllMemory:
      AddMemoryStats;
  end;
end;

procedure TSynLog.PerformRotation;
var
  currentMaxSynLZ: cardinal;
  i: integer;
  c: PSynLogThreadContext;
  FN: array of TFileName;
begin
  CloseLogFile;
  currentMaxSynLZ := 0;
  if not (assigned(fFamily.fOnRotate) and
     fFamily.fOnRotate(self, fFileName)) then
  begin
    if fFamily.fRotateFileCount > 1 then
    begin
      SetLength(FN, fFamily.fRotateFileCount - 1);
      for i := fFamily.fRotateFileCount - 1 downto 1 do
      begin
        FN[i - 1] := ChangeFileExt(fFileName, '.' + IntToStr(i) + '.synlz');
        if (currentMaxSynLZ = 0) and
           FileExists(FN[i - 1]) then
          currentMaxSynLZ := i;
      end;
      if currentMaxSynLZ = fFamily.fRotateFileCount - 1 then
        DeleteFile(FN[currentMaxSynLZ - 1]); // delete e.g. '9.synlz'
      for i := fFamily.fRotateFileCount - 2 downto 1 do
        RenameFile(FN[i - 1], FN[i]); // e.g. '8.synlz' -> '9.synlz'
      if (AutoFlushThread <> nil) and
         (AutoFlushThread.fToCompress = '') and
         RenameFile(fFileName, FN[0]) then
      begin
        AutoFlushThread.fToCompress := FN[0]; // background compression
        AutoFlushThread.fEvent.SetEvent;
      end
      else
        // blocking compression in the processing thread
        LogCompressAlgo.FileCompress(fFileName, FN[0], LOG_MAGIC, true);
    end;
    DeleteFile(fFileName);
  end;
  CreateLogWriter;
  LogFileHeader;
  if fFamily.fPerThreadLog = ptIdentifiedInOneFile then
  begin
    c := pointer(fThreadContexts);
    for i := 1 to fThreadContextCount do
    begin
      if (PtrUInt(c^.ID) <> 0) and
         (c^.ThreadName <> '') then
      begin
        // generate same output than TSynLog.LogThreadName
        LogCurrentTime;
        fWriter.AddInt18ToChars3(i);
        fWriter.AddShorter(LOG_LEVEL_TEXT[sllInfo]);
        fWriter.AddShort('SetThreadName ');
        fWriter.AddPointer(PtrUInt(c^.ID));
        fWriter.Add('=');
        fWriter.AddString(c^.ThreadName);
        fWriterEcho.AddEndOfLine(sllInfo);
      end;
      inc(c);
    end;
  end;
end;

procedure TSynLog.LogInternalFmt(Level: TSynLogInfo; const TextFmt: RawUtf8;
  const TextArgs: array of const; Instance: TObject);
var
  lasterror: cardinal;
begin
  if Level = sllLastError then
    lasterror := GetLastError
  else
    lasterror := 0;
  EnterCriticalSection(GlobalThreadLock);
  try
    GetThreadContextAndDisableExceptions;
    LogHeader(Level);
    if Instance <> nil then
      fWriter.AddInstancePointer(Instance, ' ', fFamily.WithUnitName,
        fFamily.WithInstancePointer);
    fWriter.Add(TextFmt, TextArgs, twOnSameLine,
      [woDontStoreDefault, woDontStoreVoid, woFullExpand]);
    if lasterror <> 0 then
      AddErrorMessage(lasterror);
    LogTrailer(Level);
  finally
    {$ifndef NOEXCEPTIONINTERCEPT}
    fExceptionIgnoreThreadVar^ := fExceptionIgnoredBackup;
    {$endif NOEXCEPTIONINTERCEPT}
    LeaveCriticalSection(GlobalThreadLock);
    if lasterror <> 0 then
      SetLastError(lasterror);
  end;
end;

procedure TSynLog.LogInternalText(Level: TSynLogInfo; const Text: RawUtf8;
  Instance: TObject; TextTruncateAtLength: integer);
var
  lasterror: cardinal;
begin
  if Level = sllLastError then
    lasterror := GetLastError
  else
    lasterror := 0;
  EnterCriticalSection(GlobalThreadLock);
  try
    GetThreadContextAndDisableExceptions;
    LogHeader(Level);
    if Text = '' then
    begin
      if Instance <> nil then
        if PClass(fWriter)^ = TTextWriter then
          // WriteObject() requires TJsonWriter from mormot.core.json.pas
          fWriter.AddInstancePointer(Instance, #0, {unit=}true, {ptr=}true)
        else
          // by definition, a JSON object is serialized on the same line
          fWriter.WriteObject(Instance, [woFullExpand]);
    end
    else
    begin
      if Instance <> nil then
        fWriter.AddInstancePointer(Instance, ' ', fFamily.WithUnitName,
          fFamily.WithInstancePointer);
      if length(Text) > TextTruncateAtLength then
      begin
        fWriter.AddOnSameLine(pointer(Text), TextTruncateAtLength);
        fWriter.AddShort('... (truncated) length=');
        fWriter.AddU(length(Text));
      end
      else
        fWriter.AddOnSameLine(pointer(Text));
    end;
    if lasterror <> 0 then
      AddErrorMessage(lasterror);
    LogTrailer(Level);
  finally
    {$ifndef NOEXCEPTIONINTERCEPT}
    fExceptionIgnoreThreadVar^ := fExceptionIgnoredBackup;
    {$endif NOEXCEPTIONINTERCEPT}
    LeaveCriticalSection(GlobalThreadLock);
    if lasterror <> 0 then
      SetLastError(lasterror);
  end;
end;

procedure TSynLog.LogInternalRtti(Level: TSynLogInfo; const aName: RawUtf8;
  aTypeInfo: PRttiInfo; const aValue; Instance: TObject);
begin
  EnterCriticalSection(GlobalThreadLock);
  try
    GetThreadContextAndDisableExceptions;
    LogHeader(Level);
    if Instance <> nil then
      fWriter.AddInstancePointer(Instance, ' ', fFamily.WithUnitName,
        fFamily.WithInstancePointer);
    fWriter.AddOnSameLine(pointer(aName));
    fWriter.Add('=');
    fWriter.AddTypedJson(@aValue, aTypeInfo, [woDontStoreVoid]);
    LogTrailer(Level);
  finally
    {$ifndef NOEXCEPTIONINTERCEPT}
    fExceptionIgnoreThreadVar^ := fExceptionIgnoredBackup;
    {$endif NOEXCEPTIONINTERCEPT}
    LeaveCriticalSection(GlobalThreadLock);
  end;
end;

procedure TSynLog.ComputeFileName;
var
  timeNow, hourRotate, timeBeforeRotate: TDateTime;
  {$ifdef OSPOSIX}
  i: PtrInt;
  {$endif OSPOSIX}
begin
  fFileName := fFamily.fCustomFileName;
  if fFileName = '' then
  begin
    Utf8ToFileName(Executable.ProgramName, fFileName);
    if fFamily.IncludeComputerNameInFileName then
      fFileName := fFileName + ' (' + Utf8ToString(Executable.Host) + ')';
  end;
  fFileRotationSize := 0;
  if fFamily.fRotateFileCount > 0 then
  begin
    if fFamily.fRotateFileSize > 0 then
      fFileRotationSize := fFamily.fRotateFileSize shl 10; // size KB -> B
    if fFamily.fRotateFileAtHour in [0..23] then
    begin
      hourRotate := EncodeTime(fFamily.fRotateFileAtHour, 0, 0, 0);
      timeNow := Time;
      if hourRotate < timeNow then
        hourRotate := hourRotate + 1; // trigger will be tomorrow
      timeBeforeRotate := hourRotate - timeNow;
      fFileRotationNextHour := GetTickCount64 + trunc(timeBeforeRotate * MSecsPerDay);
    end;
  end;
  if (fFileRotationSize = 0) and
     (fFileRotationNextHour = 0) then
    fFileName := fFileName + ' ' + Ansi7ToString(NowToString(false));
  {$ifdef OSWINDOWS}
  if IsLibrary and
     (fFamily.fCustomFileName = '') then
    fFileName := fFileName + ' ' + ExtractFileName(GetModuleName(HInstance));
  {$endif OSWINDOWS}
  if fFamily.fPerThreadLog = ptOneFilePerThread then
    fFileName := fFileName + ' ' +
      sysutils.IntToHex(PtrInt(GetCurrentThreadId), 8);
  {$ifdef OSPOSIX}
  for i := 1 to length(fFileName) do
    if fFileName[i] = ' ' then
      fFileName[i] := '-'; // more readable and usable on POSIX command line
  {$endif OSPOSIX}
  fFileName := fFamily.fDestinationPath + fFileName + fFamily.fDefaultExtension;
end;

procedure TSynLog.CreateLogWriter;
var
  i, retry: integer;
  exists: boolean;
begin
  if fWriterStream = nil then
  begin
    ComputeFileName;
    if fFamily.NoFile then
      fWriterStream := TFakeWriterStream.Create
    else
    begin
      if FileExists(fFileName) then
        case fFamily.FileExistsAction of
          acOverwrite:
            DeleteFile(fFileName);
          acAppend:
            Include(fInternalFlags, logHeaderWritten);
        end;
      for retry := 0 to 2 do
      begin
        for i := 1 to 10 do
        try
          exists := FileExists(fFileName);
          if exists and
             (fFamily.FileExistsAction <> acOverwrite) then
          begin
            if fFamily.FileExistsAction = acAppend then
              Include(fInternalFlags, logHeaderWritten);
          end
          else if (fFileRotationSize = 0) or
                  not exists then
            TFileStream.Create(fFileName, fmCreate).Free;   // create a void file
          fWriterStream := TFileStreamWithoutWriteError.Create(fFileName,
            fmOpenReadWrite or fmShareDenyWrite); // open with read sharing
          break;
        except
          on Exception do
            SleepHiRes(100);
        end;
        if fWriterStream <> nil then
          break;
        fFileName := ChangeFileExt(fFileName, '-' + fFamily.fDefaultExtension);
      end;
    end;
    if fWriterStream = nil then
      // let's continue if file creation fails (e.g. R/O folder or disk full)
      fWriterStream := TFakeWriterStream.Create;
    if (fFileRotationSize > 0) or
       (fFamily.FileExistsAction <> acOverwrite) then
      fWriterStream.Seek(0, soEnd); // in rotation mode, append at the end
  end;
  if fWriterClass = nil then
    // use TJsonWriter since mormot.core.json.pas is linked
    fWriterClass := TJsonWriter;
  if fWriter = nil then
  begin
    fWriter := fWriterClass.Create(fWriterStream, fFamily.BufferSize) as TJsonWriter;
    fWriter.CustomOptions := fWriter.CustomOptions
      + [twoEnumSetsAsTextInRecord, twoFullSetsAsStar, twoForceJsonExtended]
      - [twoFlushToStreamNoAutoResize]; // follow BufferSize
    fWriterEcho := TEchoWriter.Create(fWriter);
  end;
  fWriterEcho.EndOfLineCRLF := fFamily.EndOfLineCRLF;
  if integer(fFamily.EchoToConsole) <> 0 then
    fWriterEcho.EchoAdd(ConsoleEcho);
  if Assigned(fFamily.EchoCustom) then
    fWriterEcho.EchoAdd(fFamily.EchoCustom);
  if Assigned(fFamily.fEchoRemoteClient) then
    fWriterEcho.EchoAdd(fFamily.fEchoRemoteEvent);
  fFamily.StartAutoFlush;
end;

function TSynLog.GetFileSize: Int64;
begin
  if fWriterStream <> nil then
  begin
    EnterCriticalSection(GlobalThreadLock);
    try
      result := fWriterStream.Size;
    finally
      LeaveCriticalSection(GlobalThreadLock);
    end;
  end
  else
    result := 0;
end;

procedure TSynLog.AddRecursion(aIndex: integer; aLevel: TSynLogInfo);
var
  r: PSynLogThreadRecursion;
begin
  // at entry, aLevel is sllEnter, sllLeave or sllNone (from LogHeaderBegin)
  if cardinal(aIndex) < cardinal(fThreadContext^.RecursionCount) then
  begin
    r := @fThreadContext^.Recursion[aIndex];
    if aLevel <> sllLeave then
    begin
      // sllEnter or sllNone
      if r^.Instance <> nil then
        fWriter.AddInstancePointer(r^.Instance, '.', fFamily.WithUnitName,
          fFamily.WithInstancePointer);
      if r^.MethodName <> nil then
      begin
        if r^.MethodNameLocal <> mnLeave then
        begin
          fWriter.AddOnSameLine(r^.MethodName);
          case r^.MethodNameLocal of
            mnEnter:
              r^.MethodNameLocal := mnLeave;
            mnEnterOwnMethodName:
              begin
                r^.MethodNameLocal := mnLeave;
                RawUtf8(pointer(r^.MethodName)) := ''; // release temp string
              end;
          end;
        end;
      end
      {$ifndef FPC}
      else if r^.Caller <> 0 then
        // no method name specified -> try from map/mab symbols
        TDebugFile.Log(fWriter, r^.Caller, {notcode=}false, {symbol=}true)
      {$endif FPC};
    end;
    if aLevel <> sllNone then
    begin
      // sllEnter or sllLeave
      if not fFamily.HighResolutionTimestamp then
      begin
        // no previous TSynLog.LogCurrentTime call
        QueryPerformanceMicroSeconds(fCurrentTimestamp);
        dec(fCurrentTimestamp, fStartTimestamp);
      end;
      case aLevel of
        sllEnter:
          r^.EnterTimestamp := fCurrentTimestamp;
        sllLeave:
          fWriter.AddMicroSec(fCurrentTimestamp - r^.EnterTimestamp);
      end;
    end;
  end;
  fWriterEcho.AddEndOfLine(aLevel);
end;

{$ifdef FPC}

procedure TSynLog.AddStackTrace(Level: TSynLogInfo; Stack: PPtrUInt);
var
  frames: array[0..61] of pointer; // on Win64, RtlCaptureStackBackTrace < 62
  i, depth: PtrInt;
begin
  depth := fFamily.StackTraceLevel;
  if depth <> 0 then
    try
      fWriter.Add(' ');
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

procedure TSynLog.AddStackTrace(Level: TSynLogInfo; Stack: PPtrUInt);

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
  BackTrace: array[byte] of PtrUInt;
  {$ifndef NOEXCEPTIONINTERCEPT}
  nointercept: PBoolean;
  nointerceptbackup: boolean; // paranoid precaution
  {$endif NOEXCEPTIONINTERCEPT}
begin
  if fFamily.StackTraceLevel <= 0 then
    exit;
  {$ifndef NOEXCEPTIONINTERCEPT}
  nointercept := @ExceptionIgnorePerThread;
  nointerceptbackup := nointercept^;
  nointercept^ := true;
  {$endif NOEXCEPTIONINTERCEPT}
  try
    {$ifdef OSWINDOWS}
    logged := 0;
    if fFamily.StackTraceUse <> stOnlyManual then
    begin
      n := RtlCaptureStackBackTrace(2, fFamily.StackTraceLevel, @BackTrace, nil);
      if n <> 0 then
      begin
        fWriter.Add(' ');
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
  nointercept^ := nointerceptbackup;
  {$endif NOEXCEPTIONINTERCEPT}
end;

{$endif FPC}


{ ************** High-Level Logs and Exception Related Features }

{$ifndef NOEXCEPTIONINTERCEPT}

const
  MAX_EXCEPTHISTORY = 15;

type
  TSynLogExceptionInfos = array[0..MAX_EXCEPTHISTORY] of TSynLogExceptionInfo;

var
  GlobalLastException: TSynLogExceptionInfos;
  GlobalLastExceptionIndex: integer = -1;

function GetHandleExceptionSynLog: TSynLog;
var
  lookup: ^TSynLog;
  i: PtrInt;
begin
  result := nil;
  if SynLogFile = nil then
  begin
    // no log content yet -> check from family
    for i := 0 to high(SynLogFamily) do
      if SynLogFamily[i].fHandleExceptions then
      begin
        result := SynLogFamily[i].SynLog;
        exit;
      end;
  end
  else
  begin
    // check from active per-thread TSynLog instances
    lookup := @SynLogLookupThreadVar; // access TLS slot once
    for i := 0 to MAX_SYNLOGFAMILY do
    begin
      result := lookup^;
      if (result <> nil) and
         result.fFamily.fHandleExceptions then
        exit;
      inc(lookup);
    end;
    // check from global list of TSynLog instances
    EnterCriticalSection(GlobalThreadLock);
    try
      for i := 0 to high(SynLogFile) do
      begin
        result := SynLogFile[i];
        if result.fFamily.fHandleExceptions then
          exit;
      end;
    finally
      LeaveCriticalSection(GlobalThreadLock);
    end;
    result := nil;
  end;
end;

// this is the main entry point for all intercepted exceptions
procedure SynLogException(const Ctxt: TSynLogExceptionContext);
var
  log: TSynLog;
  info: ^TSynLogExceptionInfo;
  {$ifdef FPC}
  i: PtrInt;
  {$endif FPC}
label
  adr, fin;
begin
  if ExceptionIgnorePerThread or
     (Ctxt.EClass = ESynLogSilent) or
     SynLogFileFreeing  then
    exit;
  {$ifdef CPU64DELPHI} // Delphi<XE6 in System.pas to retrieve x64 dll exit code
  {$ifndef ISDELPHIXE6}
  if (Ctxt.EInstance <> nil) and // Ctxt.EClass is EExternalException
     (PShortString(PPointer(PPtrInt(Ctxt.EInstance)^ + vmtClassName)^)^ =
      '_TExitDllException') then
    exit;
  {$endif ISDELPHIXE6}
  {$endif CPU64DELPHI}
  log := GlobalCurrentHandleExceptionSynLog;
  EnterCriticalSection(GlobalThreadLock);
  try
    try
      if (log = nil) or
         not log.fFamily.fHandleExceptions then
        log := GetHandleExceptionSynLog;
      if (log = nil) or
         not (Ctxt.ELevel in log.fFamily.Level) or
         (log.fFamily.ExceptionIgnore.IndexOf(Ctxt.EClass) >= 0) then
      begin
        log := nil; // no GetThreadContextAndDisableExceptions -> no restore
        exit;
      end;
      log.GetThreadContextAndDisableExceptions;
      if Assigned(log.fFamily.OnBeforeException) then
        if log.fFamily.OnBeforeException(Ctxt, log.fThreadContext^.ThreadName) then
          // intercepted by custom callback
          exit;
      log.LogHeader(Ctxt.ELevel);
      if GlobalLastExceptionIndex = MAX_EXCEPTHISTORY then
        GlobalLastExceptionIndex := 0
      else
        inc(GlobalLastExceptionIndex);
      info := @GlobalLastException[GlobalLastExceptionIndex];
      info^.Context := Ctxt;
      if (Ctxt.ELevel = sllException) and
         (Ctxt.EInstance <> nil) then
      begin
        info^.Message := Ctxt.EInstance.Message;
        if Ctxt.EInstance.InheritsFrom(ESynException) then
        begin
          ESynException(Ctxt.EInstance).RaisedAt := pointer(Ctxt.EAddr);
          if ESynException(Ctxt.EInstance).CustomLog(log.fWriter, Ctxt) then
            goto fin;
          goto adr; // CustomLog() included DefaultSynLogExceptionToStr()
        end;
      end
      else
        info^.Message := '';
      if DefaultSynLogExceptionToStr(log.fWriter, Ctxt) then
        goto fin;
adr:  log.fWriter.Add(' ', '[');
      log.fWriter.AddShort(CurrentThreadName); // fThreadContext^.ThreadName may be ''
      log.fWriter.AddShorter('] at ');
      try
        TDebugFile.Log(log.fWriter, Ctxt.EAddr, {notcode=}true, {symbol=}false);
        {$ifdef FPC}
        // we rely on the stack trace supplied by FPC RTL
        for i := 0 to Ctxt.EStackCount - 1 do
          if (i = 0) or
             (Ctxt.EStack[i] <> Ctxt.EStack[i - 1]) then
            TDebugFile.Log(log.fWriter, Ctxt.EStack[i], {notcode=}false, {symbol=}false);
        {$else}
        {$ifdef CPUX86} // stack frame OK only for RTLUnwindProc by now
        log.AddStackTrace(Ctxt.ELevel, Ctxt.EStack);
        {$endif CPUX86}
        {$endif FPC}
      except // paranoid
      end;
fin:  log.fWriterEcho.AddEndOfLine(log.fCurrentLevel);
      log.fWriter.FlushToStream; // exceptions available on disk ASAP
    except
      // any nested exception should never be propagated to the OS caller
    end;
  finally
    if log <> nil then
      log.fExceptionIgnoreThreadVar^ := log.fExceptionIgnoredBackup;
    LeaveCriticalSection(GlobalThreadLock);
  end;
end;

function GetLastException(out info: TSynLogExceptionInfo): boolean;
begin
  if GlobalLastExceptionIndex < 0 then
  begin
    result := false;
    exit; // no exception intercepted yet
  end;
  EnterCriticalSection(GlobalThreadLock);
  try
    info := GlobalLastException[GlobalLastExceptionIndex];
  finally
    LeaveCriticalSection(GlobalThreadLock);
  end;
  info.Context.EInstance := nil; // avoid any GPF
  info.Context.EStack := nil;
  result := info.Context.ELevel <> sllNone;
end;

procedure GetLastExceptions(out result: TSynLogExceptionInfoDynArray;
  Depth: integer);
var
  infos: TSynLogExceptionInfos; // use thread-safe local copy of static array
  index, last, n, i: PtrInt;
begin
  if GlobalLastExceptionIndex < 0 then
    exit; // no exception intercepted yet
  EnterCriticalSection(GlobalThreadLock);
  try
    infos := GlobalLastException;
    index := GlobalLastExceptionIndex;
  finally
    LeaveCriticalSection(GlobalThreadLock);
  end;
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
        EStack := nil;
      end;
  end;
end;

function ToText(var info: TSynLogExceptionInfo): RawUtf8;
begin
  with info.Context do
    if ELevel <> sllNone then
      FormatUtf8('% % at %: % [%]', [_LogInfoCaption[ELevel], EClass,
        GetInstanceDebugFile.FindLocationShort(EAddr),
        UnixTimeToString(ETimestamp, {expanded=}true, ' '),
        StringToUtf8(info.Message)], result)
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


procedure _SetThreadName(ThreadID: TThreadID; const Format: RawUtf8;
  const Args: array of const);
var
  name: RawUtf8;
  i: PtrInt;
  n: TShort31;
begin
  if SynLogFileFreeing then
    exit;
  FormatUtf8(Format, Args, name);
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
  n[0] := #0;
  for i := 1 to length(name) do
    if name[i] in ['a'..'z', 'A'..'Z', '0'..'9', '.'
      {$ifdef OSWINDOWS}, ' ', '-'{$endif}] then
    begin
      inc(n[0]);
      n[ord(n[0])] := name[i];
      if n[0] = #31 then
        break; // TShort31
    end;
  if CurrentThreadName = n then
    exit; // already set as such
  RawSetThreadName(ThreadID, {$ifdef OSWINDOWS} name {$else} n {$endif});
  EnterCriticalSection(GlobalThreadLock);
  try
    CurrentThreadName := ''; // for LogThreadName(name) to appear once
    for i := 0 to high(SynLogFamily) do
      with SynLogFamily[i] do
        if (sllInfo in Level) and
           (PerThreadLog = ptIdentifiedInOneFile) and
           (fGlobalLog <> nil) then
          fGlobalLog.LogThreadName(name); // try to put the full name in log
  finally
    LeaveCriticalSection(GlobalThreadLock);
    CurrentThreadName := n; // low-level short name will be used from now
  end;
end;



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

function TSynLogCallbacks.OnEcho(Sender: TEchoWriter; Level: TSynLogInfo;
  const Text: RawUtf8): boolean;
var
  i: PtrInt;
begin
  result := false;
  if (Count = 0) or
     fCurrentlyEchoing then
    exit;
  Safe.Lock; // not really concurrent, but faster
  try
    fCurrentlyEchoing := true; // avoid stack overflow if exception below
    for i := Count - 1 downto 0 do
      if Level in Registration[i].Levels then
      try
        Registration[i].Callback.Log(Level, Text);
        result := true;
      except
        Registrations.Delete(i); // safer to unsubscribe ASAP
      end;
  finally
    fCurrentlyEchoing := false;
    Safe.UnLock;
  end;
end;

function TSynLogCallbacks.Subscribe(const Levels: TSynLogInfos;
  const Callback: ISynLogCallback; ReceiveExistingKB: cardinal): integer;
var
  reg: TSynLogCallback;
  previousContent: RawUtf8;
begin
  if Assigned(Callback) then
  try
    if ReceiveExistingKB > 0 then
    begin
      EnterCriticalSection(GlobalThreadLock);
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
    Safe.Lock;
    try
      Registrations.Add(reg);
    finally
      Safe.UnLock;
    end;
  finally
    if ReceiveExistingKB > 0 then
      LeaveCriticalSection(GlobalThreadLock);
  end;
  result := length(previousContent);
end;

procedure TSynLogCallbacks.Unsubscribe(const Callback: ISynLogCallback);
var
  i: PtrInt;
begin
  Safe.Lock;
  try
    for i := Count - 1 downto 0 do
      if Registration[i].Callback = Callback then
        Registrations.Delete(i);
  finally
    Safe.UnLock;
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


function EventArchiveDelete(
  const aOldLogFileName, aDestinationPath: TFileName): boolean;
begin
  result := DeleteFile(aOldLogFileName);
end;

function EventArchiveSynLZ(
  const aOldLogFileName, aDestinationPath: TFileName): boolean;
begin
  result := AlgoSynLZ.EventArchive(
    LOG_MAGIC, aOldLogFileName, aDestinationPath, '.synlz');
end;




{ ************** Efficient .log File Access via TSynLogFile }

{ TSynLogFile }

constructor TSynLogFile.Create;
var
  L: TSynLogInfo;
begin
  for L := low(TSynLogInfo) to high(TSynLogInfo) do
    fLogLevelsTextMap[L] := PCardinal(@LOG_LEVEL_TEXT[L][3])^; // [3] -> e.g. 'UST4'
end;

function TSynLogFile.GetLogLevelFromText(LineBeg: PUtf8Char): TSynLogInfo;
var
  P: PtrInt;
begin
  P := PtrInt(IntegerScan(@fLogLevelsTextMap[succ(sllNone)],
    ord(high(TSynLogInfo)), PCardinal(LineBeg + fLineLevelOffset)^));
  if P <> 0 then
    result := TSynLogInfo(
      (P - PtrInt(PtrUInt(@fLogLevelsTextMap[succ(sllNone)]))) shr 2 + 1)
  else
    result := sllNone;
end;

function TSynLogFile.EventCount(const aSet: TSynLogInfos): integer;
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
  Timestamp: Int64;
  P: PUtf8Char;
  Y, M, D, HH, MM, SS, MS: cardinal;
  hex2bin: PByteArray;
begin
  if cardinal(aIndex) >= cardinal(fCount) then
    result := 0
  else if fFreq = 0 then
  begin
    P := fLines[aIndex];
    hex2bin := @ConvertHexToBin;
    if Char4ToWord(P, Y, hex2bin) or Char2ToByte(P + 4, M, hex2bin) or
       Char2ToByte(P + 6, D, hex2bin) or Char2ToByte(P + 9, HH, hex2bin) or
       Char2ToByte(P + 11, MM, hex2bin) or Char2ToByte(P + 13, SS, hex2bin) or
       Char2ToByte(P + 15, MS, hex2bin) then
      // not exact YYYYMMDD hhmmsszz layout -> try plain ISO-8601
      Iso8601ToDateTimePUtf8CharVar(P, 17, result)
    else if TryEncodeDate(Y, M, D, result) then
      // MS shl 4 = 16 ms resolution in TTextWriter.AddCurrentLogTime()
      result := result + EncodeTime(HH, MM, SS, MS shl 4)
    else
      result := 0;
  end
  else if HexDisplayToBin(fLines[aIndex], @Timestamp, SizeOf(Timestamp)) then
    result := fStartDateTime + (Timestamp / fFreqPerDay)
  else
    result := 0;
end;

procedure TSynLogFile.CleanLevels;
var
  i, aCount, pCount, dCount, dValue, dMax: PtrInt;
begin
  aCount := 0;
  pCount := 0;
  dCount := 0;
  dMax := Length(fDayChangeIndex);
  if dMax > 0 then
    dValue := fDayChangeIndex[0]
  else
    dValue := -1;
  for i := 0 to fCount - 1 do
    if fLevels[i] <> sllNone then
    begin
      fLevels[aCount] := fLevels[i];
      fLines[aCount] := fLines[i];
      if fThreads <> nil then
        fThreads[aCount] := fThreads[i];
      if fLevels[i] = sllEnter then
      begin
        fLogProcNatural[pCount].index := aCount;
        inc(pCount);
      end;
      if dValue = i then
      begin
        fDayChangeIndex[dCount] := aCount;
        inc(dCount);
        if dCount < dMax then
          dValue := fDayChangeIndex[dCount];
      end;
      inc(aCount);
    end;
  fCount := aCount;
  assert(pCount = fLogProcNaturalCount);
  if dMax > 0 then
  begin
    SetLength(fDayCount, dMax);
    dec(dMax);
    for i := 0 to dMax - 1 do
      fDayCount[i] := fDayChangeIndex[i + 1] - fDayChangeIndex[i];
    fDayCount[dMax] := aCount - fDayChangeIndex[dMax];
  end;
end;

function TSynLogFile.ComputeProperTime(var procndx: PtrInt): cardinal;
var
  start, i: PtrInt;
  tim: cardinal;
  p: PSynLogFileProc;
begin
  start := procndx;
  with fLogProcNatural[procndx] do
  begin
    ProperTime := Time;
    result := index;
  end;
  repeat
    inc(result);
    if result >= cardinal(Count) then
      break;
    case fLevels[result] of
      sllEnter:
        begin
          inc(procndx);
          assert(fLogProcNatural[procndx].index = result);
          result := ComputeProperTime(procndx);
        end;
      sllLeave:
        begin
          p := @fLogProcNatural[start];
          tim := p^.ProperTime;
          for i := start + 1 to procndx do
            dec(tim, fLogProcNatural[i].ProperTime);
          p^.ProperTime := tim;
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
  i: PtrInt;
  j, Level: integer;
  TSEnter, TSLeave: Int64;
  OK: boolean;
begin
  // 1. calculate fLines[] + fCount and fLevels[] + fLogProcNatural[] from .log content
  fLineHeaderCountToIgnore := 3;
  inherited LoadFromMap(100);
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
    if feat <> '' then
      case feat[1] of
        '-': // ARM32 marker
          mormot.core.text.HexToBin(pointer(feat), @fArm32CPU, SizeOf(fArm32CPU));
        '+': // AARCH64 marker
          mormot.core.text.HexToBin(pointer(feat), @fArm64CPU, SizeOf(fArm64CPU));
      else
        mormot.core.text.HexToBin(pointer(feat), @fIntelCPU, SizeOf(fIntelCPU));
      end;
    fWow64 := aWow64 = '1';
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
    if PWord(fLines[fHeaderLinesCount])^ <> ord('0') + ord('0') shl 8 then
      // YYYYMMDD -> 20101225 e.g. fFreq=0 if date time,
      fFreq := 0
    else
      // fFreq>0 if high-resolution time stamp
      fFreqPerDay := fFreq * SecsPerDay;
    P := pointer(fOSDetailed);
    fOS := TWindowsVersion(GetNextItemCardinal(P, '.'));
    if fOS <> wUnknown then
      fOSServicePack := GetNextItemCardinal(P);
    P := fLines[fHeaderLinesCount - 2]; // TSqlLog 1.18.2765 ERTL FTS3 2016-07-17T22:38:03
    i := LineSize(fHeaderLinesCount - 2) - 19; // length('2016-07-17T22:38:03')=19
    if i > 0 then
    begin
      FastSetString(fFramework, PAnsiChar(P), i - 1);
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
    SetLength(fLogProcNatural, fLogProcNaturalCount);
    for i := 0 to fLogProcNaturalCount - 1 do
      if fLogProcNatural[i].Time >= 99000000 then
      begin
        // 99.xxx.xxx means over range -> compute
        Level := 0;
        j := fLogProcNatural[i].index;
        repeat
          inc(j);
          if j = fCount then
            break;
          case fLevels[j] of
            sllEnter:
              inc(Level);
            sllLeave:
              if Level = 0 then
              begin
                if fFreq = 0 then
                  // adjust huge seconds timing from date/time column
                  fLogProcNatural[i].Time := Round(
                    (EventDateTime(j) - EventDateTime(fLogProcNatural[i].index))
                     * 86400000000.0) +
                    fLogProcNatural[i].Time mod 1000000
                else
                begin
                  HexDisplayToBin(fLines[fLogProcNatural[i].index],
                    @TSEnter, SizeOf(TSEnter));
                  HexDisplayToBin(fLines[j],
                    @TSLeave, SizeOf(TSLeave));
                  fLogProcNatural[i].Time :=
                    ((TSLeave - TSEnter) * (1000 * 1000)) div fFreq;
                end;
                break;
              end
              else
                dec(Level);
          end;
        until false;
      end;
    i := 0;
    while i < fLogProcNaturalCount do
    begin
      ComputeProperTime(i);
      inc(i);
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
    Exit;
  fLogProcSortInternalOrder := Order;
  LogProcSortInternal(0, LogProcCount - 1);
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
    Inc(Str1);
    Inc(Str2);
  until false;
  result := C1 - C2;
end;

function TSynLogFile.LogProcSortComp(A, B: PtrInt): PtrInt;
begin
  case fLogProcSortInternalOrder of
    soByName:
      result := StrICompLeftTrim(
        PUtf8Char(fLines[LogProc[A].index]) + fLineTextOffset,
        PUtf8Char(fLines[LogProc[B].index]) + fLineTextOffset);
    soByOccurrence:
      result := LogProc[A].index - LogProc[B].index;
    soByTime:
      result := LogProc[B].Time - LogProc[A].Time;
    soByProperTime:
      result := LogProc[B].ProperTime - LogProc[A].ProperTime;
  else
    result := A - B;
  end;
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
        while LogProcSortComp(I, P) < 0 do
          inc(I);
        while LogProcSortComp(J, P) > 0 do
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
      if J - L < R - I then
      begin
        // use recursion only for smaller range
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
  thread, n, i: PtrUInt;
  MS: integer;
  L: TSynLogInfo;
begin
  inherited ProcessOneLine(LineBeg, LineEnd);
  if length(fLevels) < fLinesMax then
    SetLength(fLevels, fLinesMax);
  if (fCount <= fLineHeaderCountToIgnore) or
     (LineEnd - LineBeg < 24) then
    exit;
  if fLineLevelOffset = 0 then
  begin
    if (fCount > 50) or
       not (LineBeg[0] in ['0'..'9']) then
      exit; // definitively does not sound like a .log content
    if LineBeg[8] = ' ' then
    begin
      // YYYYMMDD HHMMSS is one char bigger than Timestamp
      fLineLevelOffset := 19;
      fDayCurrent := PInt64(LineBeg)^;
      AddInteger(fDayChangeIndex, fCount - 1);
    end
    else
      fLineLevelOffset := 18;
    if (LineBeg[fLineLevelOffset] = '!') or // ! = thread 1
      (GetLogLevelFromText(LineBeg) = sllNone) then
    begin
      inc(fLineLevelOffset, 3);
      fThreadsCount := fLinesMax;
      SetLength(fThreads, fLinesMax);
    end;
    fLineTextOffset := fLineLevelOffset + 4;
    SetLength(fLogProcStack, fLinesMax);
    SetLength(fLogProcStackCount, fLinesMax);
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
      if thread >= fThreadInfoMax then
      begin
        fThreadInfoMax := thread + 256;
        SetLength(fThreadInfo, fThreadInfoMax);
      end;
    end;
    inc(fThreadInfo[thread].Rows);
    if (L = sllInfo) and
       IdemPChar(LineBeg + fLineLevelOffset + 5, 'SETTHREADNAME ') then
      with fThreadInfo[thread] do
      begin
        // see TSynLog.LogThreadName
        n := length(SetThreadName);
        SetLength(SetThreadName, n + 1);
        SetThreadName[n] := LineBeg;
      end;
  end
  else
    thread := 0;
  fLevels[fCount - 1] := L; // need exact match of level text
  include(fLevelUsed, L);
  case L of
    sllEnter:
      begin
        n := length(fLogProcStack[thread]);
        i := fLogProcStackCount[thread];
        if i >= n then
          SetLength(fLogProcStack[thread], i + 256);
        fLogProcStack[thread][i] := fLogProcNaturalCount;
        inc(fLogProcStackCount[thread]);
        n := length(fLogProcNatural);
        if PtrUInt(fLogProcNaturalCount) >= n then
          SetLength(fLogProcNatural, NextGrow(fLogProcNaturalCount));
        // fLogProcNatural[].Index will be set in TSynLogFile.LoadFromMap
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
  if fThreadInfo <> nil then
    result := fThreadInfo[ThreadID].Rows
  else
    result := 0;
end;

function TSynLogFile.ThreadName(ThreadID, CurrentLogIndex: integer): RawUtf8;
var
  i: PtrInt;
  lineptr: PtrUInt;
  found: pointer;
begin
  if ThreadID = 1 then
    result := 'Main Thread'
  else
  begin
    result := '';
    if cardinal(ThreadID) <= fThreadMax then
      with fThreadInfo[ThreadID] do
        if SetThreadName <> nil then
        begin
          found := SetThreadName[0];
          if cardinal(CurrentLogIndex) < cardinal(fCount) then
          begin
            lineptr := PtrUInt(fLines[CurrentLogIndex]);
            for i := length(SetThreadName) - 1 downto 1 do
              if lineptr >= PtrUInt(SetThreadName[i]) then
              begin
                found := SetThreadName[i];
                break;
              end;
          end;
          FastSetString(result, found, GetLineSize(found, fMapEnd));
          delete(result, 1, PosEx('=', result, 40));
        end;
    if result = '' then
      result := 'Thread';
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
  if fThreadInfo = nil then
    exit;
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
  if IsValidUtf8(pointer(tmp)) then
    Utf8ToStringVar(tmp, result)
  else
    {$ifdef UNICODE}
    result := CurrentAnsiConvert.AnsiToUnicodeString(pointer(tmp), length(tmp));
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
  i: integer;
  P: ^TSynLogFileProc;
  O: TLogProcSortOrder;
begin
  fLogProcIsMerged := Value;
  O := fLogProcSortInternalOrder;
  if Value then
  begin
    if fLogProcMerged = nil then
    begin
      fLogProcCurrent := pointer(fLogProcNatural);
      fLogProcCurrentCount := fLogProcNaturalCount;
      LogProcSort(soByName); // sort by name to identify unique
      SetLength(fLogProcMerged, fLogProcNaturalCount);
      fLogProcMergedCount := 0;
      i := 0;
      P := pointer(fLogProcNatural);
      repeat
        with fLogProcMerged[fLogProcMergedCount] do
        begin
          repeat
            index := P^.index;
            inc(Time, P^.Time);
            inc(ProperTime, P^.ProperTime);
            inc(i);
            inc(P);
          until (i >= fLogProcNaturalCount) or
            (StrICompLeftTrim(PUtf8Char(fLines[LogProc[i - 1].index]) + 22,
             PUtf8Char(fLines[P^.index]) + 22) <> 0);
        end;
        inc(fLogProcMergedCount);
      until i >= fLogProcNaturalCount;
      SetLength(fLogProcMerged, fLogProcMergedCount);
    end;
    fLogProcCurrent := pointer(fLogProcMerged);
    fLogProcCurrentCount := fLogProcMergedCount;
  end
  else
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
        tim := IntToStr(trunc(elapsed * MSecsPerDay * 1000) mod 1000);
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
  out aLevel: TSynLogInfo): string;
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

function TSynLogFileView.SearchNextEvent(aEvent: TSynLogInfo;
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
  if fLevels = nil then
  begin
    // plain text search
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
    for result := fCount - 1 downto aRow + 1 do
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
           GetBitPtr(pointer(fThreadSelected), fThreads[i] - 1) then
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
    destbuffer := PrintUSAscii(destbuffer, ProgramName); // APP-NAME
  end;
  destbuffer := PrintUSAscii(destbuffer, procid);      // PROCID
  destbuffer := PrintUSAscii(destbuffer, msgid);      // MSGID
  destbuffer := PrintUSAscii(destbuffer, '');        // no STRUCTURED-DATA
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
    PInteger(destbuffer)^ := $bfbbef; // UTF-8 BOM
    inc(destbuffer, 3);
  end;
  MoveFast(P^, destbuffer^, len);
  result := (destbuffer - start) + len;
end;


procedure InitializeUnit;
begin
  InitializeCriticalSection(GlobalThreadLock);
  GetEnumTrimmedNames(TypeInfo(TSynLogInfo), @_LogInfoText);
  GetEnumCaptions(TypeInfo(TSynLogInfo), @_LogInfoCaption);
  _LogInfoCaption[sllNone] := '';
  SetThreadName := _SetThreadName;
  SetCurrentThreadName('MainThread');
  GetExecutableLocation := _GetExecutableLocation; // use FindLocationShort()
  LogCompressAlgo := AlgoSynLZ; // very fast and efficient on logs
  //writeln(BacktraceStrFpc(Get_pc_addr));
  //writeln(GetExecutableLocation(get_caller_addr(get_frame)));
  //writeln(GetInstanceDebugFile.FindLocationShort(PtrUInt(@TDynArray.InitFrom)));
  //GetInstanceDebugFile.SaveToJson(NowToFileShort+'.json',jsonUnquotedPropName);
end;

procedure FinalizeUnit;
var
  files: TSynLogDynArray; // thread-safe local copy
begin
  SynLogFileFreeing := true; // to avoid GPF at shutdown
  EnterCriticalSection(GlobalThreadLock);
  files := SynLogFile;
  SynLogFile := nil; // would break any background process
  LeaveCriticalSection(GlobalThreadLock);
  if AutoFlushThread <> nil then
  begin
    AutoFlushThread.Terminate;
    AutoFlushThread.fEvent.SetEvent; // notify TAutoFlushThread.Execute
    AutoFlushThread.WaitFor;
    FreeAndNilSafe(AutoFlushThread);
  end;
  ObjArrayClear(files); // TSynLogFamily are freed as TRttiCustom.Private
  {$ifdef FPC}
  if @BacktraceStrFunc = @BacktraceStrFpc then
    BacktraceStrFunc := SysBacktraceStr; // avoid instability
  {$endif FPC}
  FreeAndNilSafe(ExeInstanceDebugFile);
  DeleteCriticalSection(GlobalThreadLock);
end;


initialization
  InitializeUnit;

finalization
  FinalizeUnit;
  
end.

