/// Framework Core Logging
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.log;

{
  *****************************************************************************

   Logging functions shared by all framework units
    - Executable Symbols Processing
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
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime;


{ ************** Executable Symbols Processing }

type
  /// a debugger symbol, as decoded by TSynMapFile from a .map file
  TSynMapSymbol = packed record
    /// symbol internal name
    Name: RawUTF8;
    /// starting offset of this symbol in the executable
    // - addresses are integer, since map be <0 in Kylix .map files
    Start: integer;
    /// end offset of this symbol in the executable
    // - addresses are integer, since map be <0 in Kylix .map files
    Stop: integer;
  end;

  PSynMapSymbol = ^TSynMapSymbol;

  /// a dynamic array of symbols, as decoded by TSynMapFile from a .map file
  TSynMapSymbolDynArray = array of TSynMapSymbol;

  /// a debugger unit, as decoded by TSynMapFile from a .map file
  TSynMapUnit = packed record
    /// Name, Start and Stop of this Unit
    Symbol: TSynMapSymbol;
    /// associated source file name
    FileName: RawUTF8;
    /// list of all mapped source code lines of this unit
    Line: TIntegerDynArray;
    /// start code address of each source code line
    Addr: TIntegerDynArray;
  end;

  /// a dynamic array of units, as decoded by TSynMapFile from a .map file
  TSynMapUnitDynArray = array of TSynMapUnit;

  /// retrieve a .map file content, to be used e.g. with TSynLog to provide
  // additional debugging information
  // - original .map content can be saved as .mab file in a more optimized format
  TSynMapFile = class(TSynPersistent)
  protected
    fMapFile: TFileName;
    fSymbol: TSynMapSymbolDynArray;
    fUnit: TSynMapUnitDynArray;
    fSymbols: TDynArray;
    fUnits: TDynArrayHashed;
    fUnitSynLogIndex, fUnitSystemIndex: integer;
    fCodeOffset: PtrUInt;
    fHasDebugInfo: boolean;
  public
    /// get the available debugging information
    // - if aExeName is specified, will use it in its search for .map/.mab
    // - if aExeName is not specified, will use the currently running .exe/.dll
    // - it will first search for a .map matching the file name: if found,
    // will be read to retrieve all necessary debugging information - a .mab
    // file will be also created in the same directory (if MabCreate is TRUE)
    // - if .map is not not available, will search for the .mab file
    // - if no .mab is available, will search for a .mab appended to the .exe/.dll
    // - if nothing is available, will log as hexadecimal pointers, without
    // debugging information
    constructor Create(const aExeName: TFileName = ''; MabCreate: boolean = true);
      reintroduce;
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
    procedure SaveToJson(W: TBaseWriter); overload;
    /// save all debugging information as a JSON file
    // - may be useful from debugging purposes
    procedure SaveToJson(const aJsonFile: TFileName;
      aHumanReadable: boolean = false); overload;
    /// add some debugging information about the supplied absolute memory address
    // - will create a global TSynMapFile instance for the current process, if
    // necessary
    // - if no debugging information is available (.map or .mab), will write
    // the raw address pointer as hexadecimal
    // - under FPC, currently calls BacktraceStrFunc() which may be very slow
    class procedure Log(W: TBaseWriter; aAddressAbsolute: PtrUInt;
      AllowNotCodeAddr: boolean);
    /// compute the relative memory address from its absolute (pointer) value
    function AbsoluteToOffset(aAddressAbsolute: PtrUInt): integer;
    /// retrieve a symbol according to a relative code address
    // - use fast O(log n) binary search
    function FindSymbol(aAddressOffset: integer): PtrInt;
    /// retrieve an unit and source line, according to a relative code address
    // - use fast O(log n) binary search
    function FindUnit(aAddressOffset: integer; out LineNumber: integer): integer; overload;
    /// retrieve an unit information, according to the unit name
    // - will search within Units array
    function FindUnit(const aUnitName: RawUTF8): integer; overload;
    /// return the symbol location according to the supplied absolute address
    // - i.e. unit name, symbol name and line number (if any), as plain text
    // - returns '' if no match found
    function FindLocation(aAddressAbsolute: PtrUInt): RawUTF8; overload;
    /// return the symbol location according to the supplied ESynException
    // - i.e. unit name, symbol name and line number (if any), as plain text
    class function FindLocation(exc: ESynException): RawUTF8; overload;
    /// returns the file name of
    // - if unitname = '', returns the main file name of the current executable
    class function FindFileName(const unitname: RawUTF8): TFileName;
    /// all symbols associated to the executable
    property Symbols: TSynMapSymbolDynArray
      read fSymbol;
    /// all units, including line numbers, associated to the executable
    property Units: TSynMapUnitDynArray
      read fUnit;
  published
    /// the associated file name
    property FileName: TFileName
      read fMapFile;
    /// equals true if a .map or .mab debugging information has been loaded
    property HasDebugInfo: boolean
      read fHasDebugInfo;
  end;



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
    [sllException, sllExceptionOS, sllLastError, sllError, sllDDDError];

  /// the text equivalency of each logging level, as written in the log file
  // - PCardinal(@LOG_LEVEL_TEXT[L][3])^ will be used for fast level matching
  // so text must be unique for characters [3..6] -> e.g. 'UST4'
  LOG_LEVEL_TEXT: array[TSynLogInfo] of string[7] = (
    '       ', ' info  ', ' debug ', ' trace ', ' warn  ', ' ERROR ',
    '  +    ', '  -    ', ' OSERR ', ' EXC   ', ' EXCOS ', ' mem   ',
    ' stack ', ' fail  ', ' SQL   ', ' cache ', ' res   ', ' DB    ',
    ' http  ', ' clnt  ', ' srvr  ', ' call  ', ' ret   ', ' auth  ',
    ' cust1 ', ' cust2 ', ' cust3 ', ' cust4 ', ' rotat ', ' dddER ',
    ' dddIN ', ' mon   ');

  /// RGB colors corresponding to each logging level
  // - matches the TColor values, as used by the VCL
  LOG_LEVEL_COLORS: array[boolean, TSynLogInfo] of integer = (
  ($FFFFFF, $DCC0C0, $DCDCDC, $C0C0C0, $8080C0, $8080FF, $C0DCC0, $DCDCC0,
 // sllNone, sllInfo, sllDebug, sllTrace, sllWarning, sllError, sllEnter, sllLeave,
    $C0C0F0, $C080FF, $C080F0, $C080C0, $C080C0,
 // sllLastError, sllException, sllExceptionOS, sllMemory, sllStackTrace,
    $4040FF, $B08080, $B0B080, $8080DC, $80DC80, $DC8080, $DCFF00, $DCD000,
 // sllFail, sllSQL, sllCache, sllResult, sllDB, sllHTTP, sllClient, sllServer,
    $DCDC80, $DC80DC, $DCDCDC,
 //  sllServiceCall, sllServiceReturn, sllUserAuth,
    $D0D0D0, $D0D0DC, $D0D0C0, $D0D0E0, $20E0D0, $8080FF, $DCCDCD, $C0C0C0),
//  sllCustom1, sllCustom2, sllCustom3, sllCustom4, sllNewRun, sllDDDError,sllDDDInfo
    ($000000, $000000, $000000, $000000, $000000, $FFFFFF, $000000, $000000,
     $FFFFFF, $FFFFFF, $FFFFFF, $000000, $000000,
     $FFFFFF, $FFFFFF, $000000, $FFFFFF, $000000, $000000, $000000, $000000,
     $000000, $000000, $000000,
     $000000, $000000, $000000, $000000, $000000, $FFFFFF, $000000, $000000));

  /// console colors corresponding to each logging level
  // - to be used with mormot.core.os TextColor()
  LOG_CONSOLE_COLORS: array[TSynLogInfo] of TConsoleColor = (
  //  sllNone, sllInfo, sllDebug, sllTrace, sllWarning, sllError, sllEnter, sllLeave
    ccLightGray, ccWhite, ccLightGray, ccLightBlue, ccBrown, ccLightRed, ccGreen, ccGreen,
  //  sllLastError, sllException, sllExceptionOS, sllMemory, sllStackTrace,
    ccLightRed, ccLightRed, ccLightRed, ccLightGray, ccCyan,
  //  sllFail, sllSQL, sllCache, sllResult, sllDB, sllHTTP, sllClient, sllServer,
    ccLightRed, ccBrown, ccBlue, ccLightCyan, ccMagenta, ccCyan, ccLightCyan, ccLightCyan,
  //  sllServiceCall, sllServiceReturn, sllUserAuth,
    ccLightMagenta, ccLightMagenta, ccMagenta,
  //  sllCustom1, sllCustom2, sllCustom3, sllCustom4,
    ccLightGray, ccLightGray, ccLightGray, ccLightGray,
  //  sllNewRun, sllDDDError, sllDDDInfo, sllMonitoring
    ccLightMagenta, ccLightRed, ccWhite, ccLightBlue);

  /// how TLogFilter map TSynLogInfo events
  LOG_FILTER: array[TSynLogFilter] of TSynLogInfos = (
    [],
    [succ(sllNone)..high(TSynLogInfo)],
    [sllError, sllLastError, sllException, sllExceptionOS],
    [sllException, sllExceptionOS],
    [sllEnter, sllLeave],
    [sllSQL, sllCache, sllDB],
    [sllClient, sllServer, sllServiceCall, sllServiceReturn],
    [sllDebug, sllTrace, sllEnter],
    [sllCustom1..sllCustom4],
    [sllDDDError, sllDDDInfo]);

  /// the "magic" number used to identify .log.synlz compressed files, as
  // created by TSynLogFamily.EventArchiveSynLZ
  LOG_MAGIC = $ABA51051;

  /// may be used to log as Debug or Error event, depending on an Error: boolean
  LOG_DEBUGERROR: array[boolean] of TSynLogInfo = (
    sllDebug, sllError);

  /// may be used to log as Trace or Warning event, depending on an Error: boolean
  LOG_TRACEWARNING: array[boolean] of TSynLogInfo = (
    sllTrace, sllWarning);

  /// may be used to log as Info or Warning event, depending on an Error: boolean
  LOG_INFOWARNING: array[boolean] of TSynLogInfo = (
    sllInfo, sllWarning);

/// returns the trimmed text value of a logging level
// - i.e. 'Warning' for sllWarning
function ToText(event: TSynLogInfo): RawUTF8; overload;

/// returns the trimmed text value of a logging levels set
function ToText(events: TSynLogInfos): ShortString; overload;

/// returns the ready-to-be displayed text of a TSynLogInfo value
function ToCaption(event: TSynLogInfo): string; overload;

/// returns the ready-to-be displayed text of a TSynLogFilter value
function ToCaption(filter: TSynLogFilter): string; overload;

/// returns a method event as text, using the .map/.mab information if available
function ToText(const Event: TMethod): RawUTF8; overload;

var
  /// low-level variable used internaly by this unit
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
    // - will use TBaseWriter.Add(...,twOnSameLine) to append its content
    // - % = #37 indicates a string, integer, floating-point, class parameter
    // to be appended as text (e.g. class name), any variant as JSON...
    // - note that cardinal values should be type-casted to Int64() (otherwise
    // the integer mapped value will be transmitted, therefore wrongly)
    // - if Instance is set, it will log the corresponding class name and address
    // (to be used if you didn't call TSynLog.Enter() method first)
    procedure Log(Level: TSynLogInfo; const TextFmt: RawUTF8;
      const TextArgs: array of const; Instance: TObject = nil); overload;
    /// call this method to add some information to the log at a specified level
    // - if Instance is set and Text is not '', it will log the corresponding
    // class name and address (to be used e.g. if you didn't call TSynLog.Enter()
    // method first)
    // - if Instance is set and Text is '', will behave the same as
    // Log(Level,Instance), i.e. write the Instance as JSON content
    procedure Log(Level: TSynLogInfo; const Text: RawUTF8;
      Instance: TObject = nil; TextTruncateAtLength: integer = maxInt); overload;
    {$ifdef UNICODE}
    /// call this method to add some VCL string to the log at a specified level
    // - this overloaded version will avoid a call to StringToUTF8()
    procedure Log(Level: TSynLogInfo; const Text: string;
      Instance: TObject = nil); overload;
    {$endif UNICODE}
    /// call this method to add the content of an object to the log at a
    // specified level
    // - TSynLog will write the class and hexa address - TSQLLog will write the
    // object JSON content
    procedure Log(Level: TSynLogInfo; Instance: TObject); overload;
    /// call this method to add the content of most low-level types to the log
    // at a specified level
    // - TSynLog will handle enumerations and dynamic array; TSQLLog will be
    // able to write TObject/TOrm and sets content as JSON
    procedure Log(Level: TSynLogInfo; const aName: RawUTF8; aTypeInfo: PRttiInfo;
      const aValue; Instance: TObject); overload;
    /// call this method to add the caller address to the log at the specified level
    // - if the debugging info is available from TSynMapFile, will log the
    // unit name, associated symbol and source code line
    procedure Log(Level: TSynLogInfo = sllTrace); overload;
    /// call this method to add some multi-line information to the log at a
    // specified level
    // - LinesToLog content will be added, one line per one line, delimited
    // by #13#10 (CRLF)
    // - if a line starts with IgnoreWhenStartWith (already uppercase), it won't
    // be added to the log content (to be used e.g. with '--' for SQL statements)
    procedure LogLines(Level: TSynLogInfo; LinesToLog: PUTF8Char;
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
  // - example of matching event handler are EventArchiveDelete/EventArchiveSynLZ
  // or EventArchiveZip in SynZip.pas
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
  // - if set to ptIdentifiedInOnFile, a new column will be added for each
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
    ptIdentifiedInOnFile,
    ptNoThreadProcess);

  /// how stack trace shall be computed during logging
  TSynLogStackTraceUse = (
    stManualAndAPI,
    stOnlyAPI,
    stOnlyManual);

  /// how file existing shall be handled during logging
  TSynLogExistsAction = (
    acOverwrite,
    acAppend);

{$ifndef NOEXCEPTIONINTERCEPT}

  /// callback signature used by TSynLogFamilly.OnBeforeException
  // - should return false to log the exception, or true to ignore it
  TOnBeforeException = function(const Context: TSynLogExceptionContext;
    const ThreadName: RawUTF8): boolean of object;

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
  // ! end;
  TSynLogFamily = class
  protected
    fLevel, fLevelStackTrace: TSynLogInfos;
    fArchiveAfterDays: integer;
    fArchivePath: TFileName;
    fOnArchive: TSynLogArchiveEvent;
    fOnRotate: TSynLogRotateEvent;
    fPerThreadLog: TSynLogPerThreadMode;
    fIncludeComputerNameInFileName: boolean;
    fCustomFileName: TFileName;
    fGlobalLog: TSynLog;
    fSynLogClass: TSynLogClass;
    fIdent: integer;
    fDestinationPath: TFileName;
    fDefaultExtension: TFileName;
    fBufferSize: integer;
    fHRTimestamp: boolean;
    fLocalTimestamp: boolean;
    fWithUnitName: boolean;
    fWithInstancePointer: boolean;
    fNoFile: boolean;
    fAutoFlushTimeOut: cardinal;
    {$ifdef MSWINDOWS}
    fNoEnvironmentVariable: boolean;
    {$endif MSWINDOWS}
    {$ifndef NOEXCEPTIONINTERCEPT}
    fHandleExceptions: boolean;
    fOnBeforeException: TOnBeforeException;
    {$endif NOEXCEPTIONINTERCEPT}
    fStackTraceLevel: byte;
    fStackTraceUse: TSynLogStackTraceUse;
    fFileExistsAction: TSynLogExistsAction;
    fExceptionIgnore: TList;
    fEchoToConsole: TSynLogInfos;
    fEchoToConsoleUseJournal: boolean;
    fEchoCustom: TOnTextWriterEcho;
    fEchoRemoteClient: TObject;
    fEchoRemoteClientOwned: boolean;
    fEchoRemoteEvent: TOnTextWriterEcho;
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
    function GetExistingLog(MaximumKB: cardinal): RawUTF8;
    /// callback to notify the current logger that its thread is finished
    // - method follows TOnNotifyThread signature, which can be assigned to
    // TSynBackgroundThreadAbstract.OnAfterExecute
    // - is called e.g. by TRest.EndCurrentThread
    // - just a wrapper around TSynLog.NotifyThreadEnded
    procedure OnThreadEnded(Sender: TThread);

    /// you can add some exceptions to be ignored to this list
    // - for instance, EConvertError may be added to the list, as such:
    // ! TSQLLog.Family.ExceptionIgnore.Add(EConvertError);
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
    // debugging of services, for instance
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
    // sllError,sllFail,sllLastError,sllDDDError for Delphi only  - since FPC
    // BacktraceStrFunc() function is very slow
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
    // drive; you can call TSynLog.Flush method or set AutoFlushTimeOut to true
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
      read fHRTimestamp write fHRTimestamp;
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
    {$ifdef MSWINDOWS}
    /// force no environment variables to be written to the log file
    // - may be usefull if they contain some sensitive information
    property NoEnvironmentVariable: boolean
      read fNoEnvironmentVariable write fNoEnvironmentVariable;
    {$endif MSWINDOWS}
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
    // - default value is 30, maximum is 255
    // - if stOnlyAPI is defined as StackTraceUse under Windows XP, maximum
    // value may be around 60, due to RtlCaptureStackBackTrace() API limitations
    property StackTraceLevel: byte
      read fStackTraceLevel write fStackTraceLevel;
    /// how the stack trace shall use only the Windows API
    // - the class will use low-level RtlCaptureStackBackTrace() API to retrieve
    // the call stack: in some cases, it is not able to retrieve it, therefore
    // a manual walk of the stack can be processed - since this manual call can
    // trigger some unexpected access violations or return wrong positions,
    // you can disable this optional manual walk by setting it to stOnlyAPI
    // - default is stManualAndAPI, i.e. use RtlCaptureStackBackTrace() API and
    // perform a manual stack walk if the API returned no address (or <3); but
    // within the IDE, it will use stOnlyAPI, to ensure no annoyning AV occurs
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
    // - may be a RawUTF8 if MethodNameLocal=mnEnterOwnMethodName
    MethodName: PUTF8Char;
    /// internal reference count used at this recursion level by TSynLog._AddRef
    RefCount: integer;
    /// the caller address, ready to display stack trace dump if needed
    Caller: PtrUInt;
    /// the time stamp at enter time
    EnterTimestamp: Int64;
    /// if the method name is local, i.e. shall not be displayed at Leave()
    MethodNameLocal: (mnAlways, mnEnter, mnLeave, mnEnterOwnMethodName);
  end;

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
    ThreadName: RawUTF8;
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
  // log write (using an internal TBaseWriter)
  // - can use available debugging information via the TSynMapFile class, for
  // stack trace logging for exceptions, sllStackTrace, and Enter/Leave labelling
  TSynLog = class(TObject, ISynLog)
  // note: don't inherit from TSynInterfacedObject to avoid a method call
  protected
    fFamily: TSynLogFamily;
    fWriter: TBaseWriter;
    fWriterEcho: TEchoWriter;
    fWriterClass: TBaseWriterClass;
    fWriterStream: TStream;
    fThreadContext: PSynLogThreadContext;
    fThreadID: TThreadID;
    fThreadLastHash: integer;
    fThreadIndex: integer;
    fStartTimestamp: Int64;
    fCurrentTimestamp: Int64;
    fStartTimestampDateTime: TDateTime;
    fStreamPositionAfterHeader: cardinal;
    fFileName: TFileName;
    fFileRotationSize: cardinal;
    fFileRotationNextHour: Int64;
    fThreadHash: TWordDynArray; // 8 KB buffer
    fThreadIndexReleased: TWordDynArray;
    fThreadIndexReleasedCount: integer;
    fThreadContexts: array of TSynLogThreadContext;
    fThreadContextCount: integer;
    fCurrentLevel: TSynLogInfo;
    fInternalFlags: set of (logHeaderWritten, logInitDone);
    fDisableRemoteLog: boolean;
    function QueryInterface({$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif}
      iid: TGUID; out obj): TIntQry; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
    function _AddRef: TIntCnt;       {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
    function _Release: TIntCnt;      {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
    class function FamilyCreate: TSynLogFamily;
    procedure CreateLogWriter; virtual;
    procedure LogInternalFmt(Level: TSynLogInfo; const TextFmt: RawUTF8;
      const TextArgs: array of const; Instance: TObject);
    procedure LogInternalText(Level: TSynLogInfo; const Text: RawUTF8;
      Instance: TObject; TextTruncateAtLength: integer);
    procedure LogInternalRtti(Level: TSynLogInfo; const aName: RawUTF8;
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
    procedure AddStackTrace(Stack: PPtrUInt);
    procedure ComputeFileName; virtual;
    function GetFileSize: Int64; virtual;
    procedure PerformRotation; virtual;
    procedure AddRecursion(aIndex: integer; aLevel: TSynLogInfo);
    function GetThreadContext: PSynLogThreadContext;
      {$ifdef HASINLINE}inline;{$endif}
    procedure GetThreadContextInternal(id: PtrUInt);
    procedure ThreadContextRehash;
    function Instance: TSynLog;
    function ConsoleEcho(Sender: TBaseWriter; Level: TSynLogInfo;
      const Text: RawUTF8): boolean; virtual;
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
    // - this is the main method to be called within a procedure/function to trace:
    // ! procedure TMyDB.SQLExecute(const SQL: RawUTF8);
    // ! var ILog: ISynLog;
    // ! begin
    // !   ILog := TSynLogDB.Enter(self,'SQLExecute');
    // !   // do some stuff
    // !   ILog.Log(sllInfo,'SQL=%',[SQL]);
    // ! end;
    // - returning a ISynLog interface will allow you to have an automated
    // sllLeave log created when the method is left (thanks to the hidden
    // try..finally block generated by the compiler to protect the ISynLog var)
    // - it is convenient to define a local variable to store the returned ISynLog
    // and use it for any specific logging within the method execution
    // - if you just need to access the log inside the method block, you may
    // not need any ISynLog interface variable:
    // ! procedure TMyDB.SQLFlush;
    // ! begin
    // !   TSynLogDB.Enter(self,'SQLFlush');
    // !   // do some stuff
    // ! end;
    // - if no Method name is supplied, it will use the caller address, and
    // will write it as hexa and with full unit and symbol name, if the debugging
    // information is available (i.e. if TSynMapFile retrieved the .map content):
    // ! procedure TMyDB.SQLFlush;
    // ! begin
    // !   TSynLogDB.Enter(self);
    // !   // do some stuff
    // ! end;
    // - note that supplying a method name is faster than using the .map content:
    // if you want accurate profiling, it's better to use a method name or not to
    // use a .map file - note that this method name shall be a constant, and not
    // a locally computed variable, since it may trigger some random GPF at
    // runtime - if it is a local variable, you can set aMethodNameLocal=true
    // - if TSynLogFamily.HighResolutionTimestamp is TRUE, high-resolution
    // time stamp will be written instead of ISO 8601 date and time: this will
    // allow performance profiling of the application on the customer side
    // - Enter() will write the class name (and the unit name for classes with
    // published properties, if TSynLogFamily.WithUnitName is true) for both
    // enter (+) and leave (-) events:
    //  $ 20110325 19325801  +    MyDBUnit.TMyDB(004E11F4).SQLExecute
    //  $ 20110325 19325801 info   SQL=SELECT * FROM Table;
    //  $ 20110325 19325801  -    01.512.320
    // - note that due to a limitation (feature?) of the FPC compiler, you need
    // to hold the returned value into a local ISynLog variable, as such:
    // ! procedure TMyDB.SQLFlush;
    // ! var Log: ISynLog;
    // ! begin
    // !   Log := TSynLogDB.Enter(self);
    // !   // do some stuff
    // ! end; // here Log will be released
    // otherwise, the ISynLog instance would be released just after the Enter()
    // call, so the timing won't match the method execution
    class function Enter(aInstance: TObject = nil; aMethodName: PUTF8Char = nil;
      aMethodNameLocal: boolean = false): ISynLog; overload;
    /// handle method enter / auto-leave tracing, with some custom text
    // - this overloaded method would not write the method name, but the supplied
    // text content, after expanding the parameters like FormatUTF8()
    // - it will append the corresponding sllLeave log entry when the method ends
    class function Enter(const TextFmt: RawUTF8; const TextArgs: array of const;
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
    class procedure DebuggerNotify(Level: TSynLogInfo; const Format: RawUTF8;
      const Args: array of const);
    /// call this method to add some information to the log at the specified level
    // - will use TBaseWriter.Add(...,twOnSameLine) to append its content
    // - % = #37 indicates a string, integer, floating-point, class parameter
    // to be appended as text (e.g. class name), any variant as JSON...
    // - note that cardinal values should be type-casted to Int64() (otherwise
    // the integer mapped value will be transmitted, therefore wrongly)
    procedure Log(Level: TSynLogInfo; const TextFmt: RawUTF8;
      const TextArgs: array of const; aInstance: TObject = nil); overload;
    /// same as Log(Level,TextFmt,[]) but with one RawUTF8 parameter
    procedure Log(Level: TSynLogInfo; const TextFmt: RawUTF8;
      const TextArg: RawUTF8; aInstance: TObject = nil); overload;
    /// same as Log(Level,TextFmt,[]) but with one Int64 parameter
    procedure Log(Level: TSynLogInfo; const TextFmt: RawUTF8;
      const TextArg: Int64; aInstance: TObject = nil); overload;
    /// call this method to add some information to the log at the specified level
    // - if Instance is set and Text is not '', it will log the corresponding
    // class name and address (to be used e.g. if you didn't call TSynLog.Enter()
    // method first) - for instance
    // ! TSQLLog.Add.Log(sllDebug,'GarbageCollector',GarbageCollector);
    // will append this line to the log:
    // $ 0000000000002DB9 debug TObjectList(00425E68) GarbageCollector
    // - if Instance is set and Text is '', will behave the same as
    // Log(Level,Instance), i.e. write the Instance as JSON content
    procedure Log(Level: TSynLogInfo; const Text: RawUTF8; aInstance: TObject = nil;
      TextTruncateAtLength: integer = maxInt); overload;
    {$ifdef UNICODE}
    /// call this method to add some VCL string to the log at a specified level
    // - this overloaded version will avoid a call to StringToUTF8()
    procedure Log(Level: TSynLogInfo; const Text: string;
      aInstance: TObject = nil); overload;
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
    // - use TSQLLog from mORMot.pas unit to add the record content, written
    // as human readable JSON
    procedure Log(Level: TSynLogInfo; aInstance: TObject); overload;
    /// call this method to add the content of most low-level types to the log
    // at a specified level
    // - this overridden implementation will write the value content,
    // written as human readable JSON: handle dynamic arrays and enumerations
    // - TSQLLog from mORMot.pas unit will be able to write
    // TObject/TOrm and sets content as JSON
    procedure Log(Level: TSynLogInfo; const aName: RawUTF8; aTypeInfo: PRttiInfo;
      const aValue; Instance: TObject); overload;
    /// call this method to add the caller address to the log at the specified level
    // - if the debugging info is available from TSynMapFile, will log the
    // unit name, associated symbol and source code line
    procedure Log(Level: TSynLogInfo); overload;
    /// allows to identify the current thread with a textual representation
    // - would append an sllInfo entry with "SetThreadName ThreadID=Name" text
    // - entry would also be replicated at the begining of any rotated log file
    // - is called automatically by SetThreadName() global function
    // - if Name='', will use CurrentThreadName threadvar
    procedure LogThreadName(const Name: RawUTF8);
    /// call this method to add some multi-line information to the log at a
    // specified level
    // - LinesToLog content will be added, one line per one line, delimited by
    // #13#10 (CRLF)
    // - if a line starts with IgnoreWhenStartWith (already uppercase), it won't
    // be added to the log content (to be used e.g. with '--' for SQL statements)
    procedure LogLines(Level: TSynLogInfo; LinesToLog: PUTF8Char; aInstance: TObject = nil;
      const IgnoreWhenStartWith: PAnsiChar = nil);
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
    /// class method which can be assigned to mormot.core.os' TOnDaemonLog
    // event handler, or used instead of Add.Log
    // - will flush the content to disk and avoid any memory reallocation
    // if Level is sllExceptionOS, e.g. on SIGABRT/SIGQUIT/SIGINT
    class procedure DoLog(Level: TSynLogInfo; const Fmt: RawUTF8;
      const Args: array of const; Instance: TObject = nil);
    /// Force log rotation; Can be used for example inside SUGHUP signal handler
    procedure ForceRotation;
    /// direct access to the low-level writing content
    // - should usually not be used directly, unless you ensure it is safe
    property Writer: TBaseWriter
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



{ ************** High-Level Logs and Exception Related Features }

{$ifndef NOEXCEPTIONINTERCEPT}

var
  /// low-level variable used internaly by this unit
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
    /// ready-to-be-displayed text of the exception address
    Addr: RawUTF8;
  end;

  /// storage of information associated with one or several exceptions
  // - as returned by GetLastExceptions() function
  TSynLogExceptionInfoDynArray = array of TSynLogExceptionInfo;

/// makes a thread-safe copy of the latest intercepted exception
function GetLastException(out info: TSynLogExceptionInfo): boolean;

/// convert low-level exception information into some human-friendly text
function ToText(var info: TSynLogExceptionInfo): RawUTF8; overload;

/// returns some text about the latest intercepted exception
function GetLastExceptionText: RawUTF8;

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
    /// each line of the TBaseWriter internal instance will trigger this method
    // - similar to TOnTextWriterEcho, as defined in mormot.core.text
    // - an initial call with Level=sllNone and the whole previous Text may be
    // transmitted, if ReceiveExistingKB is set for TSynLogCallbacks.Subscribe()
    procedure Log(Level: TSynLogInfo; const Text: RawUTF8);
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
    function OnEcho(Sender: TBaseWriter; Level: TSynLogInfo;
      const Text: RawUTF8): boolean;
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

  /// used by TSynLogFile to refer to global method profiling in a .log file
  // - i.e. map all sllEnter/sllLeave event in the .log file
  TSynLogFileProcDynArray = array of TSynLogFileProc;

  TSynLogFileProcArray = array[0..(MaxInt div sizeof(TSynLogFileProc)) - 1] of TSynLogFileProc;
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
  // it will not be a concern, since the .log header is parsed explicitely
  TSynLogFile = class(TMemoryMapText)
  protected
    /// map the events occurring in the .log file content
    fLevels: TSynLogInfoDynArray;
    fThreads: TWordDynArray;
    fThreadInfo: array of record
      Rows: cardinal;
      SetThreadName: TPUTF8CharDynArray;
    end;
    fThreadInfoMax: cardinal;
    fThreadsCount: integer;
    fThreadMax: cardinal;
    fLineLevelOffset: cardinal;
    fLineTextOffset: cardinal;
    fLineHeaderCountToIgnore: integer;
    /// as extracted from the .log header
    fExeName, fExeVersion, fInstanceName: RawUTF8;
    fHost, fUser, fCPU, fOSDetailed, fFramework: RawUTF8;
    fExeDate: TDateTime;
    fIntelCPU: TIntelCpuFeatures;
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
    fHeaders: RawUTF8;
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
    procedure SetLogProcMerged(const Value: boolean);
    function GetEventText(index: integer): RawUTF8;
    function GetLogLevelFromText(LineBeg: PUTF8Char): TSynLogInfo;
    /// retrieve headers + fLevels[] + fLogProcNatural[], and delete invalid fLines[]
    procedure LoadFromMap(AverageLineLength: integer = 32); override;
    procedure CleanLevels;
    function ComputeProperTime(var procndx: PtrInt): cardinal; // returns leave
    /// compute fLevels[] + fLogProcNatural[] for each .log line during initial reading
    procedure ProcessOneLine(LineBeg, LineEnd: PUTF8Char); override;
    /// called by LogProcSort method
    function LogProcSortComp(A, B: PtrInt): PtrInt;
    procedure LogProcSortInternal(L, R: PtrInt);
  public
    /// initialize internal structure
    constructor Create; override;
    /// returns TRUE if the supplied text is contained in the corresponding line
    function LineContains(const aUpperSearch: RawUTF8; aIndex: integer): boolean; override;
    /// retrieve the date and time of an event
    // - returns 0 in case of an invalid supplied index
    function EventDateTime(aIndex: integer): TDateTime;
    /// retrieve the description text of an event, as native VCL string
    // - returns '' if supplied index is out of range
    // - if the text is not truly UTF-8 encoded, would use the current system
    // codepage to create a valid string
    // - you may specify a text to replace all #9 characters occurences
    // - is used e.g. in TMainLogView.ListDrawCell
    function EventString(index: integer; const replaceTabs: RawUTF8 = '';
      maxutf8len: integer = 0; includeFirstColumns: boolean = false): string;
    /// sort the LogProc[] array according to the supplied order
    procedure LogProcSort(Order: TLogProcSortOrder);
    /// return the number of matching events in the log
    function EventCount(const aSet: TSynLogInfos): integer;
    /// add a new line to the already parsed content
    // - overriden method which would identify the freq=%,%,% pseudo-header
    procedure AddInMemoryLine(const aNewLine: RawUTF8); override;
    /// returns the name of a given thread, according to the position in the log
    function ThreadName(ThreadID, CurrentLogIndex: integer): RawUTF8;
    /// returns the name of all threads, according to the position in the log
    // - result[0] stores the name of ThreadID = 1
    function ThreadNames(CurrentLogIndex: integer): TRawUTF8DynArray;
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
    property EventText[index: integer]: RawUTF8
      read GetEventText;
    /// retrieve all event thread IDs
    // - contains something if TSynLogFamily.PerThreadLog was ptIdentifiedInOnFile
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
    property Headers: RawUTF8
      read fHeaders;
    /// the available CPU features, as recognized at program startup
    // - is extracted from the last part of the CPU property text
    // - you could use the overloaded ToText() function to show it in an
    // human-friendly way
    property IntelCPU: TIntelCpuFeatures
      read fIntelCPU;
  published
    /// the associated executable name (with path)
    // - returns e.g. 'C:\Dev\lib\SQLite3\exe\TestSQL3.exe'
    property ExecutableName: RawUTF8
      read fExeName;
    /// the associated executable version
    // - returns e.g. '0.0.0.0'
    property ExecutableVersion: RawUTF8
      read fExeVersion;
    /// the associated executable build date and time
    property ExecutableDate: TDateTime
      read fExeDate;
    /// for a library, the associated instance name (with path)
    // - returns e.g. 'C:\Dev\lib\SQLite3\exe\TestLibrary.dll'
    // - for an executable, will be left void
    property InstanceName: RawUTF8
      read fInstanceName;
    /// the computer host name in which the process was running on
    property ComputerHost: RawUTF8
      read fHost;
    /// the computer user name who launched the process
    property RunningUser: RawUTF8
      read fUser;
    /// the computer CPU in which the process was running on
    // - returns e.g. '1*0-15-1027'
    property CPU: RawUTF8
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
    property DetailedOS: RawUTF8
      read fOSDetailed;
    /// the associated framework information
    // - returns e.g. 'TSQLLog 1.18.2765 ERTL FTS3'
    property Framework: RawUTF8
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
    procedure AddInMemoryLine(const aNewLine: RawUTF8); override;
    /// search for the next matching TSynLogInfo, from the current row index
    // - returns -1 if no match was found
    function SearchNextEvent(aEvent: TSynLogInfo; aRow: integer): PtrInt;
    /// search for the next matching text, from the current row index
    // - returns -1 if no match was found
    function SearchNextText(const aPattern: RawUTF8; aRow, aDelta: integer): PtrInt;
    /// search for the previous matching text, from the current row index
    // - returns -1 if no match was found
    function SearchPreviousText(const aPattern: RawUTF8; aRow: integer): PtrInt;
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
     ssDebug, ssInfo, ssDebug, ssDebug, ssNotice, ssWarn,
    // sllNone, sllInfo, sllDebug, sllTrace, sllWarning, sllError,
     ssDebug, ssDebug,
    // sllEnter, sllLeave,
    ssWarn, ssErr, ssErr, ssDebug, ssDebug,
    // sllLastError, sllException, sllExceptionOS, sllMemory, sllStackTrace,
    ssNotice, ssDebug, ssDebug, ssDebug, ssDebug, ssDebug, ssDebug, ssDebug,
    // sllFail, sllSQL, sllCache, sllResult, sllDB, sllHTTP, sllClient, sllServer,
    ssDebug, ssDebug, ssDebug,
    // sllServiceCall, sllServiceReturn, sllUserAuth,
    ssDebug, ssDebug, ssDebug, ssDebug, ssNotice,
    // sllCustom1, sllCustom2, sllCustom3, sllCustom4, sllNewRun,
    ssWarn, ssInfo, ssDebug);
    // sllDDDError, sllDDDInfo, sllMonitoring);

/// append some information to a syslog message memory buffer
// - following https://tools.ietf.org/html/rfc5424 specifications
// - ready to be sent via UDP to a syslog remote server
// - returns the number of bytes written to destbuffer (which should have
// destsize > 127)
function SyslogMessage(facility: TSyslogFacility; severity: TSyslogSeverity;
  const msg, procid, msgid: RawUTF8; destbuffer: PUTF8Char; destsize: PtrInt;
  trimmsgfromlog: boolean): PtrInt;


implementation

{$ifdef FPC_X64MM}
uses
  mormot.core.fpcx64mm; // for sllMemory detailed stats
{$endif FPC_X64MM}


{ ************** Executable Symbols Processing }

{ TSynMapFile }

const
  MAGIC_MAB = $A5A5A5A5;

function MatchPattern(P, PEnd, Up: PUTF8Char; var Dest: PUTF8Char): boolean;
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
      begin // ignore multiple spaces in P^
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

procedure ReadSymbol(var R: TFastReader; var A: TDynArray);
var
  i, n, L: integer;
  S: PSynMapSymbol;
  Addr: cardinal;
  P: PByte;
begin
  n := R.VarUInt32;
  A.Count := n;
  P := pointer(R.P);
  if (n = 0) or
     (P = nil) then
    exit;
  S := A.Value^;
  for i := 0 to n - 1 do
  begin
    L := FromVarUInt32(P); // inlined R.Read(S^.Name)
    FastSetString(S^.Name, P, L);
    inc(P, L);
    inc(PByte(S), A.Info.Cache.ItemSize); // may be TSynMapSymbol or TSynMapUnit
  end;
  S := A.Value^;
  Addr := FromVarUInt32(P);
  S^.Start := Addr;
  for i := 1 to n - 1 do
  begin
    inc(Addr, FromVarUInt32(P));
    S^.Stop := Addr - 1;
    inc(PByte(S), A.Info.Cache.ItemSize);
    S^.Start := Addr;
  end;
  S^.Stop := Addr + FromVarUInt32(P);
  R.P := pointer(P);
end;

const
  // Delphi linker starts the code section at this fixed offset
  CODE_SECTION = $1000;

constructor TSynMapFile.Create(const aExeName: TFileName = ''; MabCreate: boolean = true);

  procedure LoadMap;
  var
    P, PEnd: PUTF8Char;

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
         (PInteger(P)^ = ord('0') + ord('0') shl 8 +
           ord('0') shl 16 + ord('1') shl 24) and
         (P[4] = ':') then
      begin
        if not HexDisplayToBin(PAnsiChar(P) + 5, @Ptr, sizeof(Ptr)) then
          exit;
        while (P < PEnd) and
              (P^ > ' ') do
          inc(P);
        while (P < PEnd) and
              (P^ = ' ') do
          inc(P);
        if P < PEnd then
          result := true;
      end;
    end;

    procedure ReadSegments;
    var
      Beg: PAnsiChar;
      U: TSynMapUnit;
    begin
      NextLine;
      NextLine;
      while (P < PEnd) and
            (P^ < ' ') do
        inc(P);
      while (P + 10 < PEnd) and
            (P^ >= ' ') do
      begin
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
            fUnits.FindHashedAndUpdate(U, {addifnotexisting=}true);
        end;
        NextLine;
      end;
    end;

    procedure ReadSymbols;
    var
      Beg: PAnsiChar;
      Sym: TSynMapSymbol;
    begin
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
          {$ifdef ISDELPHI2005ANDUP}
          // trim left 'UnitName.' for each symbol (since Delphi 2005)
          case IdemPCharArray(P, 'SYWIFMVC') of // ignore RTL namespaces
            0:
              if IdemPChar(P + 2, 'STEM.') then
                if IdemPCharArray(P + 7, ['WIN.', 'RTTI.', 'TYPES.', 'ZLIB.',
                  'CLASSES.', 'SYSUTILS.', 'VARUTILS.', 'STRUTILS.', 'SYNCOBJS.',
                  'GENERICS.', 'CHARACTER.', 'TYPINFO.', 'VARIANTS.']) >= 0 then
                    inc(P, 9);
            1:
              if IdemPChar(P + 2, 'NAPI.') then
                inc(P, 7);
            2:
              if IdemPChar(P + 2, 'X.') then
                inc(P, 7);
            3:
              if IdemPChar(P + 2, 'L.') then
                inc(P, 7);
          end;
          while (P < PEnd) and
                (P^ <> '.') do
            if P^ <= ' ' then
              break
            else
              inc(P);
          if P^ = '.' then
          begin
            while (P < PEnd) and
                  (P^ = '.') do
              inc(P);
            Beg := pointer(P);
          end
          else
            P := pointer(Beg); // no '.' found
          {$endif ISDELPHI2005ANDUP}
          while (P < PEnd) and
                (P^ > ' ') do
            inc(P);
          FastSetString(Sym.Name, Beg, P - Beg);
          if (Sym.Name <> '') and
             not (Sym.Name[1] in ['$', '?']) then
            fSymbols.Add(Sym);
        end;
        NextLine;
      end;
    end;

    procedure ReadLines;
    var
      Beg: PAnsiChar;
      i, Count, n: integer;
      aName: RawUTF8;
      added: boolean;
      U: ^TSynMapUnit;
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
      i := fUnits.FindHashedForAdding(aName, added);
      U := @fUnit[i];
      if added then
        U^.Symbol.Name := aName; // should not occur, but who knows...
      if U^.FileName = '' then
      begin
        inc(P);
        Beg := pointer(P);
        while P^ <> ')' do
          if P = PEnd then
            exit
          else
            inc(P);
        FastSetString(U^.FileName, Beg, P - Beg);
      end;
      NextLine;
      NextLine;
      n := length(U^.Line);
      Count := n; // same unit may appear multiple times in .map content
      while (P + 10 < PEnd) and
            (P^ >= ' ') do
      begin
        while (P < PEnd) and
              (P^ = ' ') do
          inc(P);
        repeat
          if Count = n then
          begin
            n := NextGrow(n);
            SetLength(U^.Line, n);
            SetLength(U^.Addr, n);
          end;
          U^.Line[Count] := GetNextItemCardinal(P, ' ');
          if not GetCode(U^.Addr[Count]) then
            break;
          if U^.Addr[Count] <> 0 then
            inc(Count); // occured with Delphi 2010 :(
        until (P >= PEnd) or
              (P^ < ' ');
        NextLine;
      end;
      SetLength(U^.Line, Count);
      SetLength(U^.Addr, Count);
    end;

  var
    i, s, u: integer;
    RehashNeeded: boolean;
    mapcontent: RawUTF8;
  begin // LoadMap
    fSymbols.Capacity := 8000;
    mapcontent := StringFromFile(fMapFile);
    // parse .map sections into fSymbol[] and fUnit[]
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
    // now we should have read all .map content
    s := fSymbols.Count - 1;
    RehashNeeded := false;
    for i := fUnits.Count - 1 downto 0 do
      with fUnit[i] do
        if (Symbol.Start = 0) and
           (Symbol.Stop = 0) then
        begin
          fUnits.Delete(i); // occurs with Delphi 2010 :(
          RehashNeeded := true;
        end;
    u := fUnits.Count - 1;
    if RehashNeeded then
      fUnits.ReHash; // as expected by TDynArrayHashed
    {$ifopt C+}
    for i := 1 to u do
      assert(fUnit[i].Symbol.Start > fUnit[i - 1].Symbol.Stop);
    {$endif}
    for i := 0 to s - 1 do
      fSymbol[i].Stop := fSymbol[i + 1].Start - 1;
    if (u >= 0) and
       (s >= 0) then
      fSymbol[s].Stop := fUnit[u].Symbol.Stop;
  end;

  procedure LoadMab(const aMabFile: TFileName);
  var
    R: TFastReader;
    i: integer;
    S: TCustomMemoryStream;
    MS: TMemoryStream;
  begin
    fMapFile := aMabFile;
    if FileExists(aMabFile) then
    try
      S := TSynMemoryStreamMapped.Create(aMabFile);
      try
        MS := AlgoSynLZ.StreamUnCompress(S, MAGIC_MAB, {hash32=}true);
        if MS <> nil then
        try
          R.Init(MS.Memory, MS.Size);
          ReadSymbol(R, fSymbols);
          ReadSymbol(R, fUnits{$ifdef UNDIRECTDYNARRAY}.InternalDynArray{$endif});
          fUnits.ReHash;
          for i := 0 to fUnits.Count - 1 do
            with fUnit[i] do
            begin
              R.VarUTF8(FileName);
              R.ReadVarUInt32Array(Line);
              R.ReadVarUInt32Array(Addr);
            end;
          MabCreate := false;
        finally
          MS.Free;
        end;
      finally
        S.Free;
      end;
    except
      on Exception do
        ; // invalid file -> ignore any problem
    end;
  end;

var
  SymCount, UnitCount, i: integer;
  MabFile: TFileName;
  MapAge, MabAge: TDateTime;
  U: RawUTF8;
begin
  fSymbols.Init(TypeInfo(TSynMapSymbolDynArray), fSymbol, @SymCount);
  fUnits.Init(TypeInfo(TSynMapUnitDynArray), fUnit, nil, nil, nil, @UnitCount);
  fUnitSynLogIndex := -1;
  fUnitSystemIndex := -1;
  // 1. search for an external .map file matching the running .exe/.dll name
  if aExeName = '' then
  begin
    fMapFile := GetModuleName(hInstance);
    {$ifdef MSWINDOWS}
    fCodeOffset := GetModuleHandle(pointer(ExtractFileName(fMapFile))) + CODE_SECTION;
    {$else}
    {$ifdef KYLIX3}
    fCodeOffset := GetTextStart; // from SysInit.pas
    {$endif}
    {$endif}
  end
  else
    fMapFile := aExeName;
  fMapFile := ChangeFileExt(fMapFile, '.map');
  MabFile := ChangeFileExt(fMapFile, '.mab');
  EnterCriticalSection(GlobalThreadLock);
  try
    MapAge := FileAgeToDateTime(fMapFile);
    MabAge := FileAgeToDateTime(MabFile);
    if (MapAge > 0) and
       (MabAge < MapAge) then
      LoadMap; // if no faster-to-load .mab available and accurate
    // 2. search for a .mab file matching the running .exe/.dll name
    if (SymCount = 0) and
       (MabAge <> 0) then
      LoadMab(MabFile);
    // 3. search for an embedded compressed .mab file appended to the .exe/.dll
    if SymCount = 0 then
      if aExeName = '' then
        LoadMab(GetModuleName(hInstance))
      else
        LoadMab(aExeName);
    // finalize symbols
    if SymCount > 0 then
    begin
      for i := 1 to SymCount - 1 do
        assert(fSymbol[i].Start > fSymbol[i - 1].Stop);
      SetLength(fSymbol, SymCount);
      SetLength(fUnit, UnitCount);
      fSymbols.Init(TypeInfo(TSynMapSymbolDynArray), fSymbol);
      fUnits.Init(TypeInfo(TSynMapUnitDynArray), fUnit);
      if MabCreate then
        SaveToFile(MabFile); // if just created from .map -> create .mab file
      U := 'mormot.core.log';
      fUnitSynLogIndex := fUnits.FindHashed(U);
      U := 'System';
      fUnitSystemIndex := fUnits.FindHashed(U);
      fHasDebugInfo := true;
    end
    else
      fMapFile := '';
  finally
    LeaveCriticalSection(GlobalThreadLock);
  end;
end;

procedure WriteSymbol(var W: TBufferWriter; const A: TDynArray);
var
  i, n: integer;
  Diff: integer;
  S: PSynMapSymbol;
  P, Beg: PByte;
  tmp: RawByteString;
begin
  n := A.Count;
  W.WriteVarUInt32(n);
  if n = 0 then
    exit;
  S := A.Value^;
  for i := 0 to n - 1 do
  begin
    W.Write(S^.Name);
    inc(PByte(S), A.Info.Cache.ItemSize); // may be TSynMapSymbol or TSynMapUnit
  end;
  S := A.Value^;
  Diff := S^.Start;
  W.WriteVarUInt32(Diff);
  P := pointer(W.DirectWritePrepare(n * 5, tmp));
  Beg := P;
  for i := 1 to n - 1 do
  begin
    inc(PByte(S), A.Info.Cache.ItemSize);
    P := ToVarUInt32(S^.Start - Diff, P);
    Diff := S^.Start;
  end;
  P := ToVarUInt32(S^.Stop - Diff, P);
  W.DirectWriteFlush(PtrUInt(P) - PtrUInt(Beg), tmp);
end;

procedure TSynMapFile.SaveToStream(aStream: TStream);
var
  W: TBufferWriter;
  i: PtrInt;
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  W := TBufferWriter.Create(MS, 1 shl 20); // 1 MB should be enough at first
  try
    WriteSymbol(W, fSymbols);
    WriteSymbol(W, fUnits{$ifdef UNDIRECTDYNARRAY}.InternalDynArray{$endif});
    for i := 0 to high(fUnit) do
      with fUnit[i] do
      begin
        W.Write(FileName);
        W.WriteVarUInt32Array(Line, length(Line), wkOffsetI); // not always increasing
        W.WriteVarUInt32Array(Addr, length(Addr), wkOffsetU); // always increasing
      end;
    W.Flush;
    AlgoSynLZ.StreamCompress(MS, aStream, MAGIC_MAB, {hash32=}true);
  finally
    MS.Free;
    W.Free;
  end;
end;

procedure TSynMapFile.SaveToJson(W: TBaseWriter);
begin
  W.AddShort('{"Symbols":');
  fSymbols.Capacity := fSymbols.Count;
  fSymbols.SaveToJSON(W);
  W.AddShort(',"Units":');
  fUnits.SaveToJSON(W);
  W.Add('}');
end;

procedure TSynMapFile.SaveToJson(const aJsonFile: TFileName;
  aHumanReadable: boolean);
var
  W: TBaseWriter;
  json: RawUTF8;
begin
  W := DefaultTextWriterSerializer.CreateOwnedStream(65536);
  try
    SaveToJson(W);
    if aHumanReadable then
      W.SetText(json, jsonHumanReadable)
    else
      W.SetText(json);
    FileFromString(json, aJsonFile);
  finally
    W.Free;
  end;
end;

function TSynMapFile.SaveToFile(const aFileName: TFileName): TFileName;
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

procedure TSynMapFile.SaveToExe(const aExeName: TFileName);
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

function TSynMapFile.FindSymbol(aAddressOffset: integer): PtrInt;
var
  L, R: PtrInt;
begin
  R := high(fSymbol);
  L := 0;
  if (R >= 0) and
     (aAddressOffset >= fSymbol[0].Start) and
     (aAddressOffset <= fSymbol[R].Stop) then
    repeat
      result := (L + R) shr 1;
      with fSymbol[result] do
        if aAddressOffset < Start then
          R := result - 1
        else if aAddressOffset > Stop then
          L := result + 1
        else
          exit;
    until L > R;
  result := -1;
end;

function TSynMapFile.FindUnit(aAddressOffset: integer;
  out LineNumber: integer): integer;
var
  L, R, n, max: integer;
begin
  LineNumber := 0;
  R := high(fUnit);
  L := 0;
  if (R >= 0) and
     (aAddressOffset >= fUnit[0].Symbol.Start) and
     (aAddressOffset <= fUnit[R].Symbol.Stop) then
    repeat
      result := (L + R) shr 1;
      with fUnit[result] do
        if aAddressOffset < Symbol.Start then
          R := result - 1
        else if aAddressOffset > Symbol.Stop then
          L := result + 1
        else
        begin
        // unit found -> search line number
          L := 0;
          max := high(Addr);
          R := max;
          if R >= 0 then
            repeat
              n := (L + R) shr 1;
              if aAddressOffset < Addr[n] then
                R := n - 1
              else if (n < max) and
                      (aAddressOffset >= Addr[n + 1]) then
                L := n + 1
              else
              begin
                LineNumber := Line[n];
                exit;
              end;
            until L > R;
          exit;
        end;
    until L > R;
  result := -1;
end;

function TSynMapFile.AbsoluteToOffset(aAddressAbsolute: PtrUInt): integer;
begin
  if self = nil then
    result := 0
  else
    result := PtrInt(aAddressAbsolute) - PtrInt(fCodeOffset);
end;

var
  ExeInstanceMapFile: TSynMapFile;

function GetInstanceMapFile: TSynMapFile;
begin
  if ExeInstanceMapFile = nil then
    ExeInstanceMapFile := TSynMapFile.Create;
  result := ExeInstanceMapFile;
end;

class procedure TSynMapFile.Log(W: TBaseWriter; aAddressAbsolute: PtrUInt;
  AllowNotCodeAddr: boolean);
var
  {$ifdef FPC}
  s: ShortString;
  {$else}
  u, s, Line, offset: integer;
  {$endif}
begin
  if (W = nil) or
     (aAddressAbsolute = 0) then
    exit;
  {$ifdef FPC}
  // it won't actually use TSynMapFile, but just a FPC RTL raw function
  s := BacktraceStrFunc(pointer(aAddressAbsolute));
  if Pos('core.log', s) = 0 then // don't log internal calls
    W.AddShort(s);
  {$else}
  with GetInstanceMapFile do
    if HasDebugInfo then
    begin
      offset := AbsoluteToOffset(aAddressAbsolute);
      s := FindSymbol(offset);
      u := FindUnit(offset, Line);
      if s < 0 then
      begin
        if u < 0 then
        begin
          if AllowNotCodeAddr then
          begin
            W.AddBinToHexDisplayMinChars(@aAddressAbsolute, SizeOf(aAddressAbsolute));
            W.Add(' ');
          end;
          exit;
        end;
      end
      else if (u >= 0) and
              (s >= 0) and
              not AllowNotCodeAddr then
        if u = fUnitSynLogIndex then
          exit
        else
        // don't log stack trace internal to SynLog.pas :)
        if (u = fUnitSystemIndex) and
           (PosEx('Except', Symbols[s].Name) > 0) then
          exit; // do not log stack trace of System.SysRaiseException
      W.AddBinToHexDisplayMinChars(@aAddressAbsolute, SizeOf(aAddressAbsolute));
      W.Add(' ');
      if u >= 0 then
      begin
        W.AddString(Units[u].Symbol.Name);
        if s >= 0 then
          if Symbols[s].Name = Units[u].Symbol.Name then
            s := -1
          else
            W.Add('.');
      end;
      if s >= 0 then
        W.AddString(Symbols[s].Name);
      W.Add(' ');
      if Line > 0 then
      begin
        W.Add('(');
        W.Add(Line);
        W.Add(')', ' ');
      end;
    end
    else
    begin // no .map info available -> display address
      W.AddBinToHexDisplayMinChars(@aAddressAbsolute, SizeOf(aAddressAbsolute));
      W.Add(' ');
    end;
  {$endif FPC}
end;

function TSynMapFile.FindLocation(aAddressAbsolute: PtrUInt): RawUTF8;
var
  u, s, Line, offset: integer;
begin
  if (self = nil) or
     (aAddressAbsolute = 0) or
     not HasDebugInfo then
  begin
    PointerToHex(pointer(aAddressAbsolute), result);
    exit;
  end;
  result := '';
  offset := AbsoluteToOffset(aAddressAbsolute);
  s := FindSymbol(offset);
  u := FindUnit(offset, Line);
  if (s < 0) and
     (u < 0) then
    exit;
  if u >= 0 then
  begin
    result := Units[u].Symbol.Name;
    if s >= 0 then
      if Symbols[s].Name = result then
        s := -1
      else
        result := result + '.';
  end;
  if s >= 0 then
    result := result + Symbols[s].Name;
  if Line > 0 then
    result := result + ' (' + UInt32ToUtf8(Line) + ')';
end;

class function TSynMapFile.FindLocation(exc: ESynException): RawUTF8;
begin
  if (exc = nil) or
     (exc.RaisedAt = nil) then
    result := ''
  else
    result := GetInstanceMapFile.FindLocation(PtrUInt(exc.RaisedAt));
end;

function TSynMapFile.FindUnit(const aUnitName: RawUTF8): integer;
begin
  if (self <> nil) and
     (aUnitName <> '') then
    for result := 0 to high(fUnit) do
      if IdemPropNameU(fUnit[result].Symbol.Name, aUnitName) then
        exit;
  result := -1;
end;

class function TSynMapFile.FindFileName(const unitname: RawUTF8): TFileName;
var
  map: TSynMapFile;
  name: RawUTF8;
  u: integer;
begin
  result := '';
  map := GetInstanceMapFile;
  if map = nil then
    exit;
  if unitname = '' then
    name := ExeVersion.ProgramName
  else
    name := unitname;
  u := map.FindUnit(name);
  if u >= 0 then
    result := UTF8ToString(map.fUnit[u].FileName);
end;


{ ************** Logging via TSynLogFamily, TSynLog, ISynLog }

var
  _LogInfoText: array[TSynLogInfo] of RawUTF8;
  _LogInfoCaption: array[TSynLogInfo] of string;

function ToText(event: TSynLogInfo): RawUTF8;
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

function ToText(const Event: TMethod): RawUTF8;
begin
  FormatUTF8('% using %(%)', [GetInstanceMapFile.FindLocation(PtrUInt(Event.Code)),
    TObject(Event.Data), Event.Data], result);
end;


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
  // cross-platform / cross-compiler TThread-based flush
  TAutoFlushThread = class(TThread)
  protected
    fEvent: TEvent;
    procedure Execute; override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

var
  AutoFlushSecondElapsed: cardinal;
  AutoFlushThread: TAutoFlushThread;

constructor TAutoFlushThread.Create;
begin
  fEvent := TEvent.Create(nil, false, false, '');
  inherited Create(false);
end;

destructor TAutoFlushThread.Destroy;
begin
  inherited Destroy;
  fEvent.Free;
end;

procedure TAutoFlushThread.Execute;
var
  i: PtrInt;
  files: TSynLogDynArray;
begin
  repeat
    fEvent.WaitFor(1000);
    if Terminated then
      break;
    EnterCriticalSection(GlobalThreadLock);
    try
      files := copy(SynLogFile); // thread-safe local copy
    finally
      LeaveCriticalSection(GlobalThreadLock);
    end;
    if files <> nil then
      try
        inc(AutoFlushSecondElapsed);
        for i := 0 to high(files) do
          with files[i] do
            if Terminated then
              break // avoid GPF
            else if (fFamily.fAutoFlushTimeOut <> 0) and
                    (fWriter <> nil) and
                    (fWriter.PendingBytes > 1) and
                    (AutoFlushSecondElapsed mod fFamily.fAutoFlushTimeOut = 0) then
                Flush({forcediskwrite=}false); // write pending data
      except
        // on stability issue, identify this thread
        SetCurrentThreadName('log autoflush');
      end;
  until Terminated;
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
    fDestinationPath := ExeVersion.ProgramFilePath
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
    {$ifdef LINUXNOTBSD}
    if aValue and
       sd.IsAvailable then
      fEchoToConsoleUseJournal := true
    else
    {$endif LINUXNOTBSD}
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
  fDestinationPath := ExeVersion.ProgramFilePath; // use .exe path
  fDefaultExtension := '.log';
  fArchivePath := fDestinationPath;
  fArchiveAfterDays := 7;
  fRotateFileAtHour := -1;
  fBufferSize := 4096;
  fStackTraceLevel := 30;
  fWithUnitName := true;
  fWithInstancePointer := true;
  {$ifndef FPC}
  if DebugHook <> 0 then // never let stManualAndAPI trigger AV within the IDE
    fStackTraceUse := stOnlyAPI;
  {$endif FPC}
  fExceptionIgnore := TList.Create;
  fLevelStackTrace := [sllStackTrace, sllException, sllExceptionOS    {$ifndef FPC}, sllError, sllFail, sllLastError, sllDDDError{$endif}];
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
        fPerThreadLog := ptIdentifiedInOnFile; // rotation requires one file
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
     (fAutoFlushTimeOut <> 0)
     {$ifndef FPC} and (DebugHook = 0) {$endif} then
  begin
    AutoFlushSecondElapsed := 0;
    AutoFlushThread := TAutoFlushThread.Create;
  end;
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
  if AutoFlushThread <> nil then
  begin
    AutoFlushThread.Terminate;
    AutoFlushThread.fEvent.SetEvent; // notify TAutoFlushThread.Execute
    FreeAndNil(AutoFlushThread); // wait till actually finished
  end;
  ExceptionIgnore.Free;
  try
    if Assigned(OnArchive) then
      if FindFirst(DestinationPath + '*' + DefaultExtension, faAnyFile, SR) = 0 then
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
          aOldLogFileName := DestinationPath + SR.Name;
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
      // ptMergedInOneFile and ptIdentifiedInOnFile (most common case)
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
      // new ptMergedInOneFile or ptIdentifiedInOnFile
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
     not Assigned(aEvent) then
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
        FormatUTF8('%00%    Remote Client % Disconnected',
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

function TSynLogFamily.GetExistingLog(MaximumKB: cardinal): RawUTF8;
const
  // a 128 MB RawUTF8 is fair enough
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
    // faster than ClassPropertiesGet: we know it is the first slot
    result := PPointer(PAnsiChar(result) + vmtAutoTable)^;
    if result <> nil then
      // we know TRttiCustom is the first slot, and Private is TSynLogFamily
      result := TSynLogFamily(PRttiCustom(result)^.Private)
    else
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
      // we know TRttiCustom is the first slot, and Private is TSynLogFamily
      P := PRttiCustom(P)^.Private;
      result := TSynLogFamily(P).fGlobalLog;
      // <>nil for ptMergedInOneFile and ptIdentifiedInOnFile (most common case)
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
    vmt := PPPointer(PAnsiChar(self) + vmtAutoTable)^^;
    if (rtticustom = nil) or
       (vmt <> rtticustom) then
      // TSynLog.Family / TSynLog.Add expect TRttiCustom in the first slot
      raise ESynLogException.CreateUTF8('%.FamilyCreate: vmtAutoTable=% not %',
        [self, vmt, rtticustom]);
    EnterCriticalSection(GlobalThreadLock);
    try
      result := TSynLogFamily(rtticustom.Private);
      if Assigned(result) then
        if result.InheritsFrom(TSynLogFamily) then
          // registered by a background thread
          exit
        else
          // paranoid
          raise ESynLogException.CreateUTF8('%.FamilyCreate: vmtAutoTable=%',
            [self, result]);
      // create the TSynLogFamily instance associated with this TSynLog class
      result := TSynLogFamily.Create(self); // stored in SynLogFamily[]
      rtticustom.Private := result; // will be owned by this TRttiCustom
    finally
      LeaveCriticalSection(GlobalThreadLock);
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

procedure TSynLog.LogTrailer(Level: TSynLogInfo);
begin
  if Level in fFamily.fLevelStackTrace then
    AddStackTrace(nil);
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
  // should match TSynLog.ThreadContextRehash
  fThreadID := TThreadID(id);
  if fFamily.fPerThreadLog <> ptNoThreadProcess then
  begin
    secondpass := false;
    // efficient TThreadID hash on all architectures
    hash := 0;
    repeat
      hash := hash xor (id and (MAXLOGTHREAD - 1));
      id := id shr (MAXLOGTHREADBITS - 1); // -1 for less collisions under Linux
    until id = 0;
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
    fThreadIndex := 1;
  // if we reach here, this is either the first time for this thread,
  // or we have a single context (ptNoThreadProcess) which needs to be updated
  fThreadContext := @fThreadContexts[fThreadIndex - 1];
  fThreadContext^.ID := fThreadID;
  if (fFamily.fPerThreadLog = ptIdentifiedInOnFile) and
     (fThreadContext^.ThreadName = '') and
     (sllInfo in fFamily.fLevel) and
     (CurrentThreadName <> '') then
    LogThreadName('');
end;

procedure TSynLog.ThreadContextRehash;
var
  i: integer;
  id, hash: PtrUInt;
  secondpass: boolean;
  ctxt: PSynLogThreadContext;
begin
  // should match TSynLog.GetThreadContextInternal
  if fFamily.fPerThreadLog = ptNoThreadProcess then
    exit;
  FillcharFast(fThreadHash[0], MAXLOGTHREAD * sizeof(fThreadHash[0]), 0);
  ctxt := pointer(fThreadContexts);
  for i := 1 to fThreadContextCount do // i > 0 to be stored in fThreadHash[]
  begin
    id := PtrUInt(ctxt^.id); // TThreadID  = ^TThreadRec under BSD
    if id <> 0 then
    begin
      // not empty slot
      hash := 0; // efficient TThreadID hash on all architectures
      repeat
        hash := hash xor (id and (MAXLOGTHREAD - 1));
        id := id shr (MAXLOGTHREADBITS - 1); // -1 for less collisions on Linux
      until id = 0;
      secondpass := false;
      repeat
        if fThreadHash[hash] = 0 then
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
begin
  if fFamily.Level * [sllEnter, sllLeave] <> [] then
  begin
    EnterCriticalSection(GlobalThreadLock);
    try
      with GetThreadContext^ do
        if RecursionCount > 0 then
          with Recursion[RecursionCount - 1] do
          begin
            if (RefCount = 0) and
               (sllEnter in fFamily.Level) then
            begin
              LogHeader(sllEnter);
              AddRecursion(RecursionCount - 1, sllEnter);
            end;
            inc(RefCount);
            result := RefCount;
          end
        else
          result := 1; // should never be 0 (mark release of TSynLog instance)
    finally
      LeaveCriticalSection(GlobalThreadLock);
    end
  end
  else
    result := 1;
end;

{$STACKFRAMES ON}

{$ifndef FPC}
   // FPC has very slow debug info retrieval -> not for Enter/Leave
  {$ifdef MSWINDOWS}
    {$ifdef CPU64}
      {$define USERTLCAPTURESTACKBACKTRACE}
    {$else}
      {$define USEASMX86STACKBACKTRACE}
    {$endif CPU64}
  {$endif MSWINDOWS}
{$endif FPC}

function TSynLog._Release: TIntCnt;
var
  addr: PtrUInt;
begin
  if fFamily.Level * [sllEnter, sllLeave] <> [] then
  begin
    EnterCriticalSection(GlobalThreadLock);
    try
      with GetThreadContext^ do
        if RecursionCount > 0 then
        begin
          with Recursion[RecursionCount - 1] do
          begin
            dec(RefCount);
            if RefCount = 0 then
            begin
              if sllLeave in fFamily.Level then
              begin
                if MethodName = nil then
                begin
                  addr := 0;
                  {$ifdef USERTLCAPTURESTACKBACKTRACE}
                  if RtlCaptureStackBackTrace(1, 1, @addr, nil) = 0 then
                    addr := 0;
                  {$endif USERTLCAPTURESTACKBACKTRACE}
                  {$ifdef USEASMX86STACKBACKTRACE}
                  asm
                    mov  eax, [ebp + 16] // +4->_IntfClear +16->initial caller
                    mov  addr, eax
                  end;
                  {$endif USEASMX86STACKBACKTRACE}
                  if addr <> 0 then
                    Caller := addr {%H-}- 5;
                end;
                LogHeader(sllLeave);
                AddRecursion(RecursionCount - 1, sllLeave);
              end;
              dec(RecursionCount);
            end;
            result := RefCount;
          end;
        end
        else
          result := 1; // should never be 0 (mark release of TSynLog)
    finally
      LeaveCriticalSection(GlobalThreadLock);
    end;
  end
  else
    result := 1;
end;

{$STACKFRAMES OFF}

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
    FreeAndNil(fWriterEcho);
    FreeAndNil(fWriter);
    FreeAndNil(fWriterStream);
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
begin
  if fWriter = nil then
    exit;
  EnterCriticalSection(GlobalThreadLock);
  try
    fWriter.FlushToStream;
    if ForceDiskWrite and fWriterStream.InheritsFrom(TFileStream) then
      FlushFileBuffers(TFileStream(fWriterStream).Handle);
    fFamily.StartAutoFlush;
  finally
    LeaveCriticalSection(GlobalThreadLock);
  end;
end;

function TSynLog.QueryInterface(
  {$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif} iid: TGUID;
  out obj): TIntQry;
begin
  result := E_NOINTERFACE;
end;

{$STACKFRAMES ON}

class function TSynLog.Enter(aInstance: TObject; aMethodName: PUTF8Char;
  aMethodNameLocal: boolean): ISynLog;
var
  log: TSynLog;
  addr: PtrUInt;
begin
  log := Add;
  if (log <> nil) and
     (sllEnter in log.fFamily.fLevel) then
  begin
    EnterCriticalSection(GlobalThreadLock);
    try
      with log.GetThreadContext^ do
      begin
        if RecursionCount = RecursionCapacity then
        begin
          RecursionCapacity := NextGrow(RecursionCapacity);
          SetLength(Recursion, RecursionCapacity);
        end;
        addr := 0;
        if aMethodName = nil then
        begin
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
            dec(addr, 5);
        end;
        with Recursion[RecursionCount] do
        begin
          Instance := aInstance;
          MethodName := aMethodName;
          if aMethodNameLocal then
            MethodNameLocal := mnEnter
          else
            MethodNameLocal := mnAlways;
          Caller := addr;
          RefCount := 0;
        end;
        inc(RecursionCount);
      end;
    finally
      LeaveCriticalSection(GlobalThreadLock);
    end;
  end;
  // copy to ISynLog interface -> will call TSynLog._AddRef
  result := log;
end;

{$STACKFRAMES OFF}

class function TSynLog.Enter(const TextFmt: RawUTF8; const TextArgs: array of const;
  aInstance: TObject): ISynLog;
var
  log: TSynLog;
begin
  log := Add;
  if (log <> nil) and
     (sllEnter in log.fFamily.fLevel) then
  begin
    EnterCriticalSection(GlobalThreadLock);
    try
      with log.GetThreadContext^ do
      begin
        if RecursionCount = RecursionCapacity then
        begin
          RecursionCapacity := NextGrow(RecursionCapacity);
          SetLength(Recursion, RecursionCapacity);
        end;
        with Recursion[RecursionCount] do
        begin
          Instance := aInstance;
          MethodName := nil; // avoid GPF on next line
          FormatUTF8(TextFmt, TextArgs, RawUTF8(pointer(MethodName)));
          MethodNameLocal := mnEnterOwnMethodName;
          Caller := 0; // No stack trace needed here
          RefCount := 0;
        end;
        inc(RecursionCount);
      end;
    finally
      LeaveCriticalSection(GlobalThreadLock);
    end;
  end;
  // copy to ISynLog interface -> will call TSynLog._AddRef
  result := log;
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

function TSynLog.ConsoleEcho(Sender: TBaseWriter; Level: TSynLogInfo;
  const Text: RawUTF8): boolean;
{$ifdef LINUXNOTBSD}
var
  tmp, mtmp: RawUTF8;
  jvec: Array[0..1] of TioVec;
{$endif LINUXNOTBSD}
begin
  result := true;
  if not (Level in fFamily.fEchoToConsole) then
    exit;
  {$ifdef LINUXNOTBSD}
  if Family.EchoToConsoleUseJournal then
  begin
    if length(Text) < 18 then
      // should be at last "20200615 08003008  "
      exit;
    FormatUTF8('PRIORITY=%', [LOG_TO_SYSLOG[Level]], tmp);
    jvec[0].iov_base := pointer(tmp);
    jvec[0].iov_len := length(tmp);
    // skip time "20200615 08003008  ."
    // (journal do it for us, and first space after it)
    FormatUTF8('MESSAGE=%', [PUTF8Char(pointer(Text))+18], mtmp);
    jvec[1].iov_base := pointer(mtmp);
    jvec[1].iov_len := length(mtmp);
    sd.journal_sendv(jvec[0], 2);
    exit;
  end;
  {$endif LINUXNOTBSD}
  ConsoleWrite(Text, LOG_CONSOLE_COLORS[Level]);
  TextColor(ccLightGray);
end;

procedure TSynLog.Log(Level: TSynLogInfo; const TextFmt: RawUTF8;
  const TextArgs: array of const; aInstance: TObject);
begin
  if (self <> nil) and
     (Level in fFamily.fLevel) then
    LogInternalFmt(Level, TextFmt, TextArgs, aInstance);
end;

procedure TSynLog.Log(Level: TSynLogInfo; const TextFmt: RawUTF8;
  const TextArg: RawUTF8; aInstance: TObject);
begin
  if (self <> nil) and
     (Level in fFamily.fLevel) then
    LogInternalFmt(Level, TextFmt, [TextArg], aInstance);
end;

procedure TSynLog.Log(Level: TSynLogInfo; const TextFmt: RawUTF8;
  const TextArg: Int64; aInstance: TObject);
begin
  if (self <> nil) and
     (Level in fFamily.fLevel) then
    LogInternalFmt(Level, TextFmt, [TextArg], aInstance);
end;

procedure TSynLog.Log(Level: TSynLogInfo; const Text: RawUTF8;
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

procedure TSynLog.LogLines(Level: TSynLogInfo; LinesToLog: PUTF8Char;
  aInstance: TObject; const IgnoreWhenStartWith: PAnsiChar);

  procedure DoLog(LinesToLog: PUTF8Char);
  var
    s: RawUTF8;
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

procedure TSynLog.LogThreadName(const Name: RawUTF8);
var
  n: RawUTF8;
begin
  if (self <> nil) and
     (sllInfo in fFamily.fLevel) and
     (fFamily.fPerThreadLog = ptIdentifiedInOnFile) then
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

class procedure TSynLog.DoLog(Level: TSynLogInfo; const Fmt: RawUTF8;
   const Args: array of const; Instance: TObject);
var
  log: TSynLog;
begin
  log := Add;
  if (log <> nil) and
     (Level in log.fFamily.fLevel) then
  begin
    if Level = sllExceptionOS then
      // don't make Out-Of-Memory any worse
      log.Writer.CustomOptions := log.Writer.CustomOptions +
        [twoFlushToStreamNoAutoResize];
    log.LogInternalFmt(Level, Fmt, Args, Instance);
    if Level = sllExceptionOS then
      // ensure all log is safely written
      log.Flush({diskwrite=}true);
  end;
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
     not Assigned(fFamily.fEchoRemoteEvent) then
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
     (Level in fFamily.fLevel) then
    if aInstance <> nil then
      LogInternalText(Level, '', aInstance, maxInt)
    else
      LogInternalText(Level, 'Instance=nil', nil, maxInt);
end;

procedure TSynLog.Log(Level: TSynLogInfo; const aName: RawUTF8;
  aTypeInfo: PRttiInfo; const aValue; Instance: TObject);
begin
  if (self <> nil) and
     (Level in fFamily.fLevel) then
    LogInternalRtti(Level, aName, aTypeInfo, aValue, Instance);
end;

{$STACKFRAMES ON}

procedure TSynLog.Log(Level: TSynLogInfo);
var
  lasterror: integer;
  addr: PtrUInt;
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
      GetThreadContext;
      LogHeader(Level);
      if lasterror <> 0 then
        AddErrorMessage(lasterror);
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
        TSynMapFile.Log(fWriter, addr - 5, false);
      LogTrailer(Level);
    finally
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
  const Format: RawUTF8; const Args: array of const);
var
  Msg: RawUTF8;
begin
  if Format <> '' then
  begin
    FormatUTF8(Format, Args, Msg);
    Add.LogInternalText(Level, Msg, nil, maxInt);
    {$ifdef MSWINDOWS}
    if IsDebuggerPresent then
      {$ifdef CPU64DELPHI}
      DebugBreak;
      {$else}
      asm
        int  3
      end;
      {$endif CPU64DELPHI}
    {$else not MSWINDOWS}
    ConsoleWrite('%  ', [Msg], LOG_CONSOLE_COLORS[Level], {noLF=}true);
    {$endif MSWINDOWS}
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
    fStartTimestampDateTime := NowUTC;
  Include(fInternalFlags, logInitDone);
end;

procedure TSynLog.LogFileHeader;
var
  WithinEvents: boolean;
  i: PtrInt;
  {$ifdef MSWINDOWS}
  Env: PWideChar;
  P: PWideChar;
  L: integer;
  {$endif MSWINDOWS}

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
  with ExeVersion, fWriter do
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
    {$ifdef MSWINDOWS}
    with SystemInfo, OSVersionInfo do
    begin
      Add('*');
      Add(wProcessorArchitecture);
      Add('-');
      Add(wProcessorLevel);
      Add('-');
      Add(wProcessorRevision);
    {$endif MSWINDOWS}
    {$ifdef CPUINTEL}
      Add(':');
      AddBinToHex(@CpuFeatures, SizeOf(CpuFeatures));
    {$endif}
      AddShorter(' OS=');
    {$ifdef MSWINDOWS}
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
    {$endif MSWINDOWS}
    if OSVersionInfoEx <> '' then
    begin
      Add('/');
      AddTrimSpaces(OSVersionInfoEx);
    end;
    AddShorter(' Wow64=');
    Add({$ifdef MSWINDOWS} integer(IsWow64) {$else} 0 {$endif});
    AddShort(' Freq=1000000'); // we use QueryPerformanceMicroSeconds()
    if IsLibrary then
    begin
      AddShort(' Instance=');
      AddNoJSONEscapeString(InstanceFileName);
    end;
    {$ifdef MSWINDOWS}
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
          AddNoJSONEscapeW(PWord(P), 0);
          Add(#9);
        end;
        inc(P, L + 1);
      end;
      FreeEnvironmentStringsW(Env);
      CancelLastChar(#9);
    end;
    {$endif MSWINDOWS}
    NewLine;
    AddClassName(self.ClassType);
    AddShort(' ' + SYNOPSE_FRAMEWORK_FULLVERSION + ' ');
    if fFamily.LocalTimestamp then
      AddDateTime(Now)
    else
      AddDateTime(NowUTC);
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

{$ifdef FPC_X64MM}
// include mormot.core.fpcx64mm information

procedure WriteArena(W: TBaseWriter; const name: shortstring;
  const a: TMMStatusArena);
begin
  {$ifdef FPCMM_DEBUG}
  W.Add('  %: %=%/%=% peak=% sleep=% ',
    [name, K(a.CumulativeAlloc - a.CumulativeFree), KBNoSpace(a.CurrentBytes),
     K(a.CumulativeAlloc), KBNoSpace(a.CumulativeBytes),
     KBNoSpace(a.PeakBytes), K(a.SleepCount)]);
  {$else}
  W.Add(' %: %/% sleep=% ', [name, KBNoSpace(a.CurrentBytes),
    KBNoSpace(a.CumulativeBytes), K(a.SleepCount)]);
  {$endif}
end;

procedure WriteX64MM(W: TBaseWriter);
var
  s: TMMStatus;
  cont: TSmallBlockContentionDynArray;
  small: TSmallBlockStatusDynArray;
  sc, sb: PtrUInt;
  i: PtrInt;
begin
  s := CurrentHeapStatus;
  small := GetSmallBlockStatus(10, obTotal, @sc, @sb);
  W.Add('  Small: %=%/%=%',
    [K(s.SmallBlocks), KBNoSpace(s.SmallBlocksSize), K(sc), KBNoSpace(sb)]);
  for i := 0 to high(small) do
    with small[i] do
    W.Add(' %:%=%/%=%', [BlockSize, K(Current),
      KBNoSpace(Current * BlockSize), K(Total), KBNoSpace(Total * BlockSize)]);
  WriteArena(W, 'Medium', s.Medium);
  WriteArena(W, 'Large', s.Large);
  W.Add('  Sleep: count=% ', [K(s.SleepCount)]);
  {$ifdef FPCMM_DEBUG}
  W.Add(' rdtsc=%', [K(s.SleepCycles)]);
  {$ifdef FPCMM_LOCKLESSFREE}
  W.Add(' locklessspin=%', [K(s.SmallFreememLockLessSpin)]);
  {$endif FPCMM_LOCKLESSFREE}
  {$endif FPCMM_DEBUG}
  W.Add(' getmem=% freemem=%',
    [K(s.SmallGetmemSleepCount), K(s.SmallFreememSleepCount)]);
  if s.SmallGetmemSleepCount + s.SmallFreememSleepCount > 1000 then
  begin
    cont := GetSmallBlockContention(8);
    for i := 0 to high(cont) do
      with cont[i] do
        if GetmemBlockSize > 0 then
          W.Add(' getmem(%)=%', [GetmemBlockSize, K(SleepCount)])
        else
          W.Add(' freemem(%)=%', [FreememBlockSize, K(SleepCount)])
  end;
end;

{$endif FPC_X64MM}

procedure TSynLog.AddMemoryStats;
var
  info: TMemoryInfo; // cross-compiler and cross-platform
begin
  if GetMemoryInfo(info, {withalloc=}true) then
    fWriter.Add(
      ' memtotal=% memfree=% filetotal=% filefree=% allocres=% allocused=% ',
      [KBNoSpace(info.memtotal), KBNoSpace(info.memfree),
       KBNoSpace(info.filetotal), KBNoSpace(info.filefree),
       KBNoSpace(info.allocreserved), KBNoSpace(info.allocused)]);
  {$ifdef FPC_X64MM}
  WriteX64MM(fWriter);
  {$endif FPC_X64MM}
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
    fWriter.AddBinToHexDisplay(@fCurrentTimestamp, sizeof(fCurrentTimestamp));
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
    for i := 0 to fThreadContext^.RecursionCount - 1 do
    begin
      fWriter.AddChars(' ', i + 24 - byte(fFamily.HighResolutionTimestamp));
      AddRecursion(i, sllNone);
    end;
  LogCurrentTime;
  if fFamily.fPerThreadLog = ptIdentifiedInOnFile then
    fWriter.AddInt18ToChars3(fThreadIndex);
  fCurrentLevel := Level;
  fWriter.AddShorter(LOG_LEVEL_TEXT[Level]);
  fWriter.AddChars(#9, fThreadContext^.RecursionCount - byte(Level in [sllEnter, sllLeave]));
  case Level of // handle additional text for some special error levels
    sllMemory:
      AddMemoryStats;
  end;
end;

procedure TSynLog.PerformRotation;
var
  currentMaxSynLZ: cardinal;
  i: integer;
  FN: array of TFileName;
begin
  CloseLogFile;
  currentMaxSynLZ := 0;
  if not (assigned(fFamily.fOnRotate) and fFamily.fOnRotate(self, fFileName)) then
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
      AlgoSynLZ.FileCompress(fFileName, FN[0], LOG_MAGIC, true); // -> '1.synlz'
    end;
    DeleteFile(fFileName);
  end;
  CreateLogWriter;
  LogFileHeader;
  if fFamily.fPerThreadLog = ptIdentifiedInOnFile then
    for i := 0 to fThreadContextCount - 1 do
      with fThreadContexts[i] do
        if (PtrUInt(ID) <> 0) and
           (ThreadName <> '') then
        begin // see TSynLog.LogThreadName
          LogCurrentTime;
          fWriter.AddInt18ToChars3(i + 1);
          fWriter.AddShorter(LOG_LEVEL_TEXT[sllInfo]);
          fWriter.AddShort('SetThreadName ');
          fWriter.AddPointer(PtrUInt(ID));
          fWriter.Add('=');
          fWriter.AddString(ThreadName);
          fWriterEcho.AddEndOfLine(sllInfo);
        end;
end;

procedure TSynLog.LogInternalFmt(Level: TSynLogInfo; const TextFmt: RawUTF8;
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
    GetThreadContext;
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
    LeaveCriticalSection(GlobalThreadLock);
    if lasterror <> 0 then
      SetLastError(lasterror);
  end;
end;

procedure TSynLog.LogInternalText(Level: TSynLogInfo; const Text: RawUTF8;
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
    GetThreadContext;
    LogHeader(Level);
    if Text = '' then
    begin
      if Instance <> nil then
        if PClass(fWriter)^ = TBaseWriter then
          // WriteObject() requires TTextWriter from mormot.core.json.pas
          fWriter.AddInstancePointer(Instance, #0, {unit=}true, {ptr=}true)
        else
          // by definition, JSON object is serialized on the same line
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
    LeaveCriticalSection(GlobalThreadLock);
    if lasterror <> 0 then
      SetLastError(lasterror);
  end;
end;

procedure TSynLog.LogInternalRtti(Level: TSynLogInfo; const aName: RawUTF8;
  aTypeInfo: PRttiInfo; const aValue; Instance: TObject);
begin
  EnterCriticalSection(GlobalThreadLock);
  try
    GetThreadContext;
    LogHeader(Level);
    if Instance <> nil then
      fWriter.AddInstancePointer(Instance, ' ', fFamily.WithUnitName,
        fFamily.WithInstancePointer);
    fWriter.AddOnSameLine(pointer(aName));
    fWriter.Add('=');
    fWriter.AddTypedJSON(@aValue, aTypeInfo);
    LogTrailer(Level);
  finally
    LeaveCriticalSection(GlobalThreadLock);
  end;
end;

procedure TSynLog.ComputeFileName;
var
  timeNow, hourRotate, timeBeforeRotate: TDateTime;
begin
  fFileName := fFamily.fCustomFileName;
  if fFileName = '' then
  begin
    fFileName := UTF8ToString(ExeVersion.ProgramName);
    if fFamily.IncludeComputerNameInFileName then
      fFileName := fFileName + ' (' + UTF8ToString(ExeVersion.Host) + ')';
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
  {$ifdef MSWINDOWS}
  if IsLibrary and
     (fFamily.fCustomFileName = '') then
    fFileName := fFileName + ' ' + ExtractFileName(GetModuleName(HInstance));
  {$endif}
  if fFamily.fPerThreadLog = ptOneFilePerThread then
    fFileName := fFileName + ' ' +
      sysutils.IntToHex(PtrInt(GetCurrentThreadId), 8);
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
      fWriterStream.Seek(0, soFromEnd); // in rotation mode, append at the end
  end;
  if fWriterClass = nil then
    // use TTextWriter or TJSONSerializer if mormot.core.json.pas is linked
    fWriterClass := DefaultTextWriterSerializer;
  if fWriter = nil then
  begin
    fWriter := fWriterClass.Create(fWriterStream, fFamily.BufferSize);
    fWriter.CustomOptions := fWriter.CustomOptions +
      [twoEnumSetsAsTextInRecord, twoFullSetsAsStar, twoForceJSONExtended];
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
begin
  // at entry, aLevel is sllEnter, sllLeave or sllNone
  with fThreadContext^ do
    if cardinal(aIndex) < cardinal(RecursionCount) then
      with Recursion[aIndex] do
      begin
        if aLevel <> sllLeave then
        begin
          if Instance <> nil then
            fWriter.AddInstancePointer(Instance, '.', fFamily.WithUnitName,
              fFamily.WithInstancePointer);
          if MethodName <> nil then
          begin
            if MethodNameLocal <> mnLeave then
            begin
              fWriter.AddOnSameLine(MethodName);
              case MethodNameLocal of
                mnEnter:
                  MethodNameLocal := mnLeave;
                mnEnterOwnMethodName:
                  begin
                    MethodNameLocal := mnLeave;
                    RawUTF8(pointer(MethodName)) := ''; // release temp string
                  end;
              end;
            end;
          end
          {$ifndef FPC} // FPC, BacktraceStrFunc() is very slow
          else
            TSynMapFile.Log(fWriter, Caller, false)
          {$endif FPC};
        end;
        if aLevel <> sllNone then
        begin
          if not fFamily.HighResolutionTimestamp then
          begin // no previous TSynLog.LogCurrentTime call
            QueryPerformanceMicroSeconds(fCurrentTimestamp);
            dec(fCurrentTimestamp, fStartTimestamp);
          end;
          case aLevel of
            sllEnter:
              EnterTimestamp := fCurrentTimestamp;
            sllLeave:
              fWriter.AddMicroSec(fCurrentTimestamp - EnterTimestamp);
          end; // may be sllNone when called from LogHeaderBegin()
        end;
      end;
  fWriterEcho.AddEndOfLine(aLevel);
end;

const
  MINIMUM_EXPECTED_STACKTRACE_DEPTH = 2;

{$ifdef FPC}

procedure TSynLog.AddStackTrace(Stack: PPtrUInt);
var
  frames: array[0..61] of pointer; // on Win64, RtlCaptureStackBackTrace < 62
  i, n, depth: PtrInt;
begin
  depth := fFamily.StackTraceLevel;
  if (depth = 0) or
     (@BackTraceStrFunc = @SysBackTraceStr) then
    exit;
  try
    n := CaptureBacktrace(2, length(frames), @frames[0]);
    if n > depth then
      n := depth;
    for i := 0 to n - 1 do
      if (i = 0) or
         (frames[i] <> frames[i - 1]) then
        // on FPC, calling BacktraceStrFunc() may be very slow
        TSynMapFile.Log(fWriter, PtrUInt(frames[i]), false);
  except // don't let any unexpected GPF break the logging process
  end;
end;

{$else not FPC}

procedure TSynLog.AddStackTrace(Stack: PPtrUInt);

{$ifdef CPU64}

  procedure AddStackManual(Stack: PPtrUInt);
  begin
  end;

{$else}

  procedure AddStackManual(Stack: PPtrUInt);

    function check2(xret: PtrUInt): boolean;
    var
      i: PtrUInt;
    begin
      result := true;
      for i := 2 to 7 do
        if PWord(xret - i)^ and $38FF = $10FF then
          exit;
      result := false;
    end;

    function IsBadReadPtr(addr: pointer; len: integer): boolean;
    begin
      try
        asm
            mov     eax, addr
            mov     ecx, len
    @s:     mov     dl, [eax]
            inc     eax
            dec     ecx
            jnz     @S
    @e: end;
        result := false; // if we reached here, everything is ok
      except
        result := true;
      end;
    end;

  var
    st, max_stack, min_stack, depth: PtrUInt;
  begin
    depth := fFamily.StackTraceLevel;
    if depth = 0 then
      exit;
    asm
        mov     min_stack, ebp
    end;
    if Stack = nil then // if no Stack pointer set, retrieve current one
      Stack := pointer(min_stack);
    asm
        mov     eax, fs:[4]
        mov     max_stack, eax
      // mov eax,fs:[18h]; mov ecx,dword ptr [eax+4]; mov max_stack,ecx
    end;
    fWriter.AddShort(' stack trace ');
    if PtrUInt(Stack) >= min_stack then
    try
      while PtrUInt(Stack) < max_stack do
      begin
        st := Stack^;
        if ((st > max_stack) or
           (st < min_stack)) and
           not IsBadReadPtr(pointer(st - 8), 12) and
           ((pByte(st - 5)^ = $E8) or check2(st)) then
        begin
          TSynMapFile.Log(fWriter, st, false); // ignore any TSynLog.* methods
          dec(depth);
          if depth = 0 then
            break;
        end;
        inc(Stack);
      end;
    except
      // just ignore any access violation here
    end;
  end;

{$endif CPU64}

var
  n, i: integer;
  BackTrace: array[byte] of PtrUInt;
begin
  if fFamily.StackTraceLevel <= 0 then
    exit;
  {$ifdef MSWINDOWS}
  if fFamily.StackTraceUse = stOnlyManual then
    AddStackManual(stack)
  else
  begin
    try
      n := RtlCaptureStackBackTrace(2, fFamily.StackTraceLevel, @BackTrace, nil);
      if (n < MINIMUM_EXPECTED_STACKTRACE_DEPTH) and
         (fFamily.StackTraceUse <> stOnlyAPI) then
        AddStackManual(stack)
      else
      begin
        fWriter.AddShort(' stack trace API ');
        for i := 0 to n - 1 do
          TSynMapFile.Log(fWriter, BackTrace[i], false); // ignore any TSynLog.*
      end;
    except
      // just ignore any access violation here
    end;
  end;
  {$endif MSWINDOWS}
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


// this is the main entry point for all intercepted exceptions
procedure SynLogException(const Ctxt: TSynLogExceptionContext);

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
     (Ctxt.EClass = ESynLogSilent) then
    exit;
  {$ifdef CPU64DELPHI} // Delphi<XE6 in System.pas to retrieve x64 dll exit code
  {$ifndef ISDELPHIXE6}
  if (Ctxt.EInstance <> nil) and // Ctxt.EClass is EExternalException
     (PShortString(PPointer(PPtrInt(Ctxt.EInstance)^ + vmtClassName)^)^ =
      '_TExitDllException') then
    exit;
  {$endif ISDELPHIXE6}
  {$endif CPU64DELPHI}
  EnterCriticalSection(GlobalThreadLock);
  try
    log := GlobalCurrentHandleExceptionSynLog;
    if (log = nil) or
       not log.fFamily.fHandleExceptions then
      log := GetHandleExceptionSynLog;
    if (log = nil) or
       not (Ctxt.ELevel in log.fFamily.Level) or
       (log.fFamily.ExceptionIgnore.IndexOf(Ctxt.EClass) >= 0) then
      exit;
    log.GetThreadContext;
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
    {$ifdef FPC}
    if @BackTraceStrFunc <> @SysBackTraceStr then
      ShortStringToAnsi7String(BackTraceStrFunc(pointer(Ctxt.EAddr)), info^.Addr)
    else
    {$endif FPC}
      info^.Addr := '';
    if (Ctxt.ELevel = sllException) and
       (Ctxt.EInstance <> nil) then
    begin
      info^.Message := Ctxt.EInstance.Message;
      if Ctxt.EInstance.InheritsFrom(ESynException) then
      begin
        ESynException(Ctxt.EInstance).RaisedAt := pointer(Ctxt.EAddr);
        if ESynException(Ctxt.EInstance).CustomLog(log.fWriter, Ctxt) then
          goto fin;
        goto adr;
      end;
    end
    else
      info^.Message := '';
    if DefaultSynLogExceptionToStr(log.fWriter, Ctxt) then
      goto fin;
adr:with log.fWriter do
    begin
      Add(' ', '[');
      AddShort(CurrentThreadName); // fThreadContext^.ThreadName may be ''
      AddShorter('] at ');
    end;
    {$ifdef FPC}
    // warning: BackTraceStrFunc is much slower than TSynMapFile.Log
    with log.fWriter do
      if @BackTraceStrFunc = @SysBackTraceStr then
      begin // no debug information
        AddPointer(Ctxt.EAddr); // write addresses as hexa
        for i := 0 to Ctxt.EStackCount - 1 do
          if (i = 0) or
             (Ctxt.EStack[i] <> Ctxt.EStack[i - 1]) then
          begin
            Add(' ');
            AddPointer(Ctxt.EStack[i]);
          end;
      end
      else
      begin
        AddString(info^.Addr);
        for i := 0 to Ctxt.EStackCount - 1 do
          if (i = 0) or
             (Ctxt.EStack[i] <> Ctxt.EStack[i - 1]) then
            AddShort(BackTraceStrFunc(pointer(Ctxt.EStack[i])));
      end;
    {$else}
    TSynMapFile.Log(log.fWriter, Ctxt.EAddr, true);
    {$ifdef CPUX86} // stack frame OK only for RTLUnwindProc by now
    log.AddStackTrace(Ctxt.EStack);
    {$endif CPUX86}
    {$endif FPC}
fin:log.fWriterEcho.AddEndOfLine(log.fCurrentLevel);
    log.fWriter.FlushToStream; // exceptions available on disk ASAP
  finally
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
  infos: TSynLogExceptionInfos; // use thread-safe local copy
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

function ToText(var info: TSynLogExceptionInfo): RawUTF8;
begin
  with info.Context do
    if ELevel <> sllNone then
    begin
      if info.Addr = '' then
        info.Addr := GetInstanceMapFile.FindLocation(EAddr);
      FormatUTF8('% % at %: % [%]', [_LogInfoCaption[ELevel], EClass, info.Addr,
        DateTimeToIsoString(ETimestamp / SecsPerDay + UnixDateDelta),
        StringToUTF8(info.Message)], result);
    end
    else
      result := '';
end;

function GetLastExceptionText: RawUTF8;
var
  info: TSynLogExceptionInfo;
begin
  if GetLastException(info) then
    result := ToText(info)
  else
    result := '';
end;

{$endif NOEXCEPTIONINTERCEPT}


procedure _SetThreadName(ThreadID: TThreadID; const Format: RawUTF8;
  const Args: array of const);
var
  name: RawUTF8;
  i, L: PtrInt;
  n: TShort31;
begin
  FormatUTF8(Format, Args, name);
  for i := 1 to length(name) do
    if name[i] < ' ' then
      name[i] := ' '; // ensure on same line
  name := StringReplaceAll(name, [
    'TSQLRest', '',
    'TRest', '',
    'TSQL', '',
    'TOrmRest', '',
    'TOrm', '',
    'TWebSocket', 'WS',
    'TServiceFactory', 'SF',
    'TSyn', '',
    'Thread', '',
    'Process', '',
    'Background', 'Bgd',
    'WebSocket', 'WS',
    'Asynch', 'A',
    'Parallel', 'Par',
    'Timer', 'Tmr',
    'Thread', 'Thd',
    {$ifdef MSWINDOWS}
    '  ', ' '
    {$else}
    ' ', '' // on POSIX, pthread 16 chars limitation -> the shorter, the better
    {$endif MSWINDOWS}
    ]);
  L := length(name);
  if L > 31 then
    L := 31;
  SetString(n, PAnsiChar(pointer(name)), L);
  if CurrentThreadName = n then
    exit; // already set as such
  CurrentThreadName := n;
  RawSetThreadName(ThreadID, name);
  EnterCriticalSection(GlobalThreadLock);
  try
    for i := 0 to high(SynLogFamily) do
      with SynLogFamily[i] do
        if (sllInfo in Level) and
           (PerThreadLog = ptIdentifiedInOnFile) and
           (fGlobalLog <> nil) then
          fGlobalLog.LogThreadName(name);
  finally
    LeaveCriticalSection(GlobalThreadLock);
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

function TSynLogCallbacks.OnEcho(Sender: TBaseWriter; Level: TSynLogInfo;
  const Text: RawUTF8): boolean;
var
  i: PtrInt;
begin
  result := false;
  if (Count = 0) or
     fCurrentlyEchoing then
    exit;
  Safe.Lock;
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
  Reg: TSynLogCallback;
  previousContent: RawUTF8;
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
          Callback.Log(sllNone, FormatUTF8('freq=%,%,%',
            [1000000, double(fStartTimestampDateTime), fFileName]));
      Callback.Log(sllNone, previousContent);
    end;
    Reg.Levels := Levels;
    Reg.Callback := Callback;
    Safe.Lock;
    try
      Registrations.Add(Reg);
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
  inherited Create;
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
  f.PerThreadLog := ptIdentifiedInOnFile; // ease multi-threaded server debug
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


{ ************** Efficient .log File Access via TSynLogFile }

{ TSynLogFile }

constructor TSynLogFile.Create;
var
  L: TSynLogInfo;
begin
  for L := low(TSynLogInfo) to high(TSynLogInfo) do
    fLogLevelsTextMap[L] := PCardinal(@LOG_LEVEL_TEXT[L][3])^; // [3] -> e.g. 'UST4'
end;

function TSynLogFile.GetLogLevelFromText(LineBeg: PUTF8Char): TSynLogInfo;
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

function TSynLogFile.LineContains(const aUpperSearch: RawUTF8;
  aIndex: integer): boolean;
begin
  if (self = nil) or
     (cardinal(aIndex) >= cardinal(fCount)) or
     (aUpperSearch = '') then
    result := false
  else
    result := GetLineContains(PUTF8Char(fLines[aIndex]) + fLineTextOffset,
      fMapEnd, pointer(aUpperSearch));
end;

function TSynLogFile.EventDateTime(aIndex: integer): TDateTime;
var
  Timestamp: Int64;
  P: PUTF8Char;
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
      Iso8601ToDateTimePUTF8CharVar(P, 17, result)
    else if TryEncodeDate(Y, M, D, result) then
      // MS shl 4 = 16 ms resolution in TBaseWriter.AddCurrentLogTime()
      result := result + EncodeTime(HH, MM, SS, MS shl 4)
    else
      result := 0;
  end
  else if HexDisplayToBin(fLines[aIndex], @Timestamp, sizeof(Timestamp)) then
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
          with fLogProcNatural[start] do
            for i := start + 1 to procndx do
              dec(ProperTime, fLogProcNatural[i].ProperTime);
          break;
        end;
    end;
  until false;
end;

function StrPosILen(P, PEnd: PUTF8Char; SearchUp: PAnsiChar): PUTF8Char;
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
  PBeg, P, PEnd: PUTF8Char;

  function GetOne(const UP: RawUTF8; var S: RawUTF8): boolean;
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
  aWow64, feat: RawUTF8;
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
      Iso8601ToDateTimePUTF8CharVar(PEnd + 1, 10, fExeDate);
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
      Iso8601ToDateTimePUTF8CharVar(PEnd + 1, 19, fExeDate);
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
    PBeg := PUTF8Char(fLines[1]) + 5;
    PEnd := PUTF8Char(fLines[1]) + LineSize(1);
    if not GetOne(' USER=', fHost) or
       not GetOne(' CPU=', fUser) or
       not GetOne(' OS=', fCPU) or
       not GetOne(' WOW64=', fOsDetailed) or
       not GetOne(' FREQ=', aWow64) then
      exit;
    Split(fCPU, ':', fCpu, feat);
    mormot.core.text.HexToBin(pointer(feat), @fIntelCPU, SizeOf(fIntelCPU));
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
    P := fLines[fHeaderLinesCount - 2]; // TSQLLog 1.18.2765 ERTL FTS3 2016-07-17T22:38:03
    i := LineSize(fHeaderLinesCount - 2) - 19; // length('2016-07-17T22:38:03')=19
    if i > 0 then
    begin
      FastSetString(fFramework, PAnsiChar(P), i - 1);
      Iso8601ToDateTimePUTF8CharVar(P + i, 19, fStartDateTime);
    end;
    if fStartDateTime = 0 then
      exit;
    // 3. compute fCount and fLines[] so that all fLevels[]<>sllNone
    CleanLevels;
    if Length(fLevels) - fCount > 16384 then
    begin // size down only if worth it
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
      begin // 99.xxx.xxx means over range -> compute
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
                    @TSEnter, sizeof(TSEnter));
                  HexDisplayToBin(fLines[j],
                    @TSLeave, sizeof(TSLeave));
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

procedure TSynLogFile.AddInMemoryLine(const aNewLine: RawUTF8);
var
  P: PUTF8Char;
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
    UTF8DecodeToString(P, StrLen(P), string(fFileName));
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

function StrICompLeftTrim(Str1, Str2: PUTF8Char): PtrInt;
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
        PUTF8Char(fLines[LogProc[A].index]) + fLineTextOffset,
        PUTF8Char(fLines[LogProc[B].index]) + fLineTextOffset);
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
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if J - L < R - I then
      begin // use recursion only for smaller range
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
begin // fast decode 00.020.006 at the end of the line
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

procedure TSynLogFile.ProcessOneLine(LineBeg, LineEnd: PUTF8Char);
var
  thread, n: cardinal;
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
      begin // see TSynLog.LogThreadName
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
        if cardinal(fLogProcStackCount[thread]) >=
            cardinal(length(fLogProcStack[thread])) then
          SetLength(fLogProcStack[thread], length(fLogProcStack[thread]) + 256);
        fLogProcStack[thread][fLogProcStackCount[thread]] := fLogProcNaturalCount;
        inc(fLogProcStackCount[thread]);
        if cardinal(fLogProcNaturalCount) >= cardinal(length(fLogProcNatural)) then
          SetLength(fLogProcNatural, length(fLogProcNatural) + 32768);
        // fLogProcNatural[].Index will be set in TSynLogFile.LoadFromMap
        inc(fLogProcNaturalCount);
      end;
    sllLeave:
      if (LineEnd - LineBeg > 10) and
         (LineEnd[-4] = '.') and
         (LineEnd[-8] = '.') and
         (fLogProcStackCount[thread] > 0) then
      begin // 00.020.006
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

function TSynLogFile.ThreadName(ThreadID, CurrentLogIndex: integer): RawUTF8;
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
    result := FormatUTF8('% % (% rows)',
      [ThreadID, result, fThreadInfo[ThreadID].Rows]);
end;

function TSynLogFile.ThreadNames(CurrentLogIndex: integer): TRawUTF8DynArray;
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

function TSynLogFile.GetEventText(index: integer): RawUTF8;
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

function TSynLogFile.EventString(index: integer; const replaceTabs: RawUTF8;
  maxutf8len: integer; includeFirstColumns: boolean): string;
var
  tmp: RawUTF8;
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
  if IsValidUTF8(pointer(tmp)) then
    result := UTF8ToString(tmp)
  else
    {$ifdef UNICODE}
    result := CurrentAnsiConvert.AnsiToUnicodeString(pointer(tmp), length(tmp));
    {$else}
    result := tmp;
    {$endif UNICODE}
  if includeFirstColumns then
  begin
    UTF8DecodeToString(fLines[index], fLineTextOffset, header);
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
            (StrICompLeftTrim(PUTF8Char(fLines[LogProc[i - 1].index]) + 22,
             PUTF8Char(fLines[P^.index]) + 22) <> 0);
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

procedure TSynLogFileView.AddInMemoryLine(const aNewLine: RawUTF8);
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

function TSynLogFileView.SearchNextText(const aPattern: RawUTF8;
  aRow, aDelta: integer): PtrInt;
begin
  result := -1;
  if (self = nil) or
     (aPattern = '') then
    exit;
  if fLevels = nil then
  begin // plain text search
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

function TSynLogFileView.SearchPreviousText(const aPattern: RawUTF8;
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
  currentThreadID: Word;
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
  currentThreadID: Word;
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
  currentThreadID: Word;
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
  currentThreadID: Word;
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

function PrintUSAscii(P: PUTF8Char; const text: RawUTF8): PUTF8Char;
var
  i: PtrInt;
begin
  P^ := ' ';
  inc(P);
  for i := 1 to length(text) do
    if ord(text[i]) in [33..126] then
    begin
      // only printable ASCII chars
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
  const msg, procid, msgid: RawUTF8; destbuffer: PUTF8Char; destsize: PtrInt;
  trimmsgfromlog: boolean): PtrInt;
var
  P: PAnsiChar;
  start: PUTF8Char;
  len: PtrInt;
  st: TSynSystemTime;
begin
  result := 0;
  if destsize < 127 then
    exit;
  start := destbuffer;
  destbuffer^ := '<';
  destbuffer := AppendUInt32ToBuffer(
    destbuffer + 1, ord(severity) + ord(facility) shl 3);
  PInteger(destbuffer)^ :=
    ord('>') + ord('1') shl 8 + ord(' ') shl 16; // VERSION=1
  inc(destbuffer, 3);
  st.FromNowUTC;
  DateToIso8601PChar(destbuffer,
    true, st.Year, st.Month, st.Day);
  TimeToIso8601PChar(destbuffer + 10,
    true, st.Hour, st.Minute, st.Second, st.MilliSecond, 'T', {withms=}true);
  destbuffer[23] := 'Z';
  inc(destbuffer, 24);
  with ExeVersion do
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
end;

procedure FinalizeUnit;
begin
  ExeInstanceMapFile.Free;
  SynLogFileFreeing := true; // to avoid GPF at shutdown
  ObjArrayClear(SynLogFile); // TSynLogFamily are freed as TRttiCustom.Private
  DeleteCriticalSection(GlobalThreadLock);
end;

initialization
  InitializeUnit;

finalization
  FinalizeUnit;
  
end.

