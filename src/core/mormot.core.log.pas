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
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.core.buffers,
  mormot.core.data,
  mormot.core.json;


{ ************** Debug Symbols Processing from Delphi .map or FPC/GDB DWARF }

{
   Our TDebugFile is able to export the function names and line numbers into
   an optimized .mab binary, e.g. for our regression tests with Delphi 13.1:

   07/28/2026  06:02 PM        12,912,640 mormot2tests.exe
   07/28/2026  06:02 PM        18,159,799 mormot2tests.map
   07/28/2026  06:02 PM           518,119 mormot2tests.mab

   For a 13MB executable, Delphi .map text was 18MB but our .mab is only 500KB.
   Then this .mab file can be distributed alongside the executable, or just
   appended to it after build. See also /src/tools/mab/mab.dpr

   The benefit seems even more obvious with FPC Win32 and GDB information:

   07/28/2026  05:56 PM         8,024,595 mormot2tests.exe
   07/28/2026  05:56 PM        33,427,255 mormot2tests.dbg
   07/28/2026  05:56 PM           452,689 mormot2tests.mab
}

type
  /// we store 32-bit relative virtual addresses (RVA) in memory and in .mab files
  // - they are computed and persisted as VirtualAddress - ImageBase
  // - 32-bit RVAs are sufficient in practice for any executable or shared
  // library supported by mORMot; larger executables are typically installers with
  // compressed payloads appended after an initial Setup executable of a few MB
  // - enable efficient TBufferWriter.WriteVarUInt32Array encoding in .mab files
  TDebugAddress = integer;
  TDebugAddressDynArray = TIntegerDynArray;

  /// a debugger symbol, as decoded by TDebugFile from a .map/.dbg/.mab file
  // - may refer to a global variable, function or method name and address
  TDebugSymbol = packed record
    /// symbol identifier
    Name: RawUtf8;
    /// relative virtual address where this symbol starts
    Start: TDebugAddress;
    /// last relative virtual address belonging to this symbol
    Stop: TDebugAddress;
  end;
  PDebugSymbol = ^TDebugSymbol;

  /// a dynamic array of symbols, as decoded by TDebugFile from .map/.dbg/.mab
  // - stored in Start increasing order in memory for fast O(log(n)) lookup
  TDebugSymbolDynArray = array of TDebugSymbol;

  /// line number information for one contiguous source code range
  // - as decoded by TDebugFile from a .map/.dbg/.mab file
  // - may refer to the main .pas file, a nested .inc file, or source locations
  // generated for compiler features such as inlined routines or generics
  TDebugBlock = packed record
    /// identifier and address range of this source block
    // - Name is the main Pascal unit identifier, e.g. 'mormot.core.base'
    Symbol: TDebugSymbol;
    /// associated source file name for this source block
    // - usually matches Symbol.Name + '.pas', but may instead refer to an
    // included source file such as 'mormot.core.base.asmx64.inc' or the
    // source file defining a generic specialization
    FileName: RawUtf8;
    /// list of all mapped source code lines of this block
    Line: TIntegerDynArray;
    /// relative virtual address of each mapped source line
    // - stored in increasing order in memory for fast O(log(n)) lookup
    Addr: TDebugAddressDynArray;
  end;
  PDebugBlock = ^TDebugBlock;
  TDebugBlocks = array[byte] of TDebugBlock;
  PDebugBlocks = ^TDebugBlocks;

  /// a dynamic array of blocks, as decoded by TDebugFile from .map/.dbg/.mab
  // - stored in Start increasing order in memory for fast O(log(n)) lookup
  TDebugBlockDynArray = array of TDebugBlock;

  /// the known TDebugFile.DebugInfo property values
  TDebugInfo = (
    diNone,
    diExternalMab,
    diInternalMab,
    {$ifdef FPC} diInternalDwarf, diExternalDwarf {$else} diExternalMap {$endif});

  /// how stack trace shall be computed during logging
  // - stOnlyAPI is the first (and default) value, since manual stack makes
  // unexpected detections, and was reported as very slow on Windows 11
  // - on FPC, these values are ignored, because RTL CaptureBacktrace() is used
  TSynLogStackTraceUse = (
    stOnlyAPI,
    stManualAndAPI,
    stOnlyManual);

  /// allow to customize TDebugFile.Create and TDebugFile.SaveToFile process
  TDebugFileScope = set of (
    dfsIncludePathInFileName,
    dfsNoMabSaveAtCreate,
    dfsNoMabExternalCheck,
    dfsNoMabInternalCheck,
    dfsNoSymbols,
    dfsNoLines,
    dfsNoProducer);

  /// process a .map/.dbg file content, to be used e.g. with TSynLog to provide
  // additional debugging information for a given executable
  // - debug info can be saved as .mab file in a much more optimized format
  // - on FPC, DWARF symbols embedded to the executable can also be retrieved - but
  // you would better use an external .dbg file then convert it into a .mab
  // - on FPC, you don't need to specifly the -gl compiler switch
  // - location of a source code information from its address is below 10us
  // - never instantiate this class directly, but call TDebugFile.FindLocation()
  TDebugFile = class(TSynPersistent)
  protected
    fSymbol: TDebugSymbolDynArray;
    fBlock: TDebugBlockDynArray;
    fCodeOffset: PtrUInt;
    fSymbolsCount, fBlocksCount: integer;
    fStart, fStop: PtrUInt; // efficient IsCode()
    fExeFile, fExePath, fMabFile: TFileName;
    fExeAge: TUnixTime;
    fProducer: RawUtf8;
    fDebugInfo: TDebugInfo;
    fLinesCount: integer;
    fSymbols, fBlocks: TDynArray;
    fSymbolsTemp, fBlocksTemp: RawByteString; // pre-allocate all names at once
    fLoadingMicroSec: Int64;
    procedure GenerateFromMapOrDwarf(aWithDir: boolean); // from Create()
    function LoadMab(const aMabFile: TFileName): boolean;
    function AbsoluteToRelative(aPointer: PtrUInt): TDebugAddress;
      {$ifdef HASINLINE}inline;{$endif}
    procedure AppendLocationShort(aPointer: PtrUInt; var aInfo: ShortString);
    function AppendLog(W: TTextWriter; aPointer: PtrUInt; NoHex: boolean): boolean;
    // use fast O(log n) binary search to locate a symbol or line number
    function FindSymbol(rva: TDebugAddress): PDebugSymbol;
    function FindBlock(rva: TDebugAddress; out line: integer): PDebugBlock; overload;
    function FindBlock(rva: TDebugAddress): PDebugBlock; overload;
      {$ifdef HASINLINE}inline;{$endif}
    function FindBlockByName(const aUnitName: RawUtf8): PDebugBlock;
    function GetExeDate: RawUtf8;
  public
    /// get the available debugging information
    // - you should NEVER call this constructor, but TDebugFile.CurrentDebugFile
    // or TDebugFile.Get() class functions or just TDebugFile.FindLocation()
    // - if aExeName is specified, will use it in its search for .map/.dbg/.mab
    // - if aExeName is not specified, will use the currently running .exe/.dll
    // - it will first search for a .map/.dbg matching the file name: if found,
    // will be read to retrieve all necessary debugging information - a .mab
    // file will be also created in the same directory (if MabCreate is TRUE)
    // - if .map/.dbg is not not available, will search for the .mab file
    // - if no .mab is available, will search for a .mab appended to the .exe/.dll
    // - if nothing is available, will eventually log as hexadecimal pointers,
    // without debugging information
    constructor Create(const aExeName: TFileName = '';
      Scope: TDebugFileScope = []); reintroduce;
    /// finalize this instance
    destructor Destroy; override;
    /// save all debugging information in the .mab custom binary format
    // - if no file name is specified, it will be saved as ExeName.mab or DllName.mab
    // - this file content can be appended to the executable via SaveToExe method
    // - this function returns the created file name
    function SaveToFile(const aFileName: TFileName = '';
      Scope: TDebugFileScope = []): TFileName;
    /// save all debugging informat in our custom binary format
    procedure SaveToStream(aStream: TStream; Scope: TDebugFileScope);
    /// append all debugging information to an executable (or library)
    // - the executable name must be specified, because it's impossible to
    // write to the executable of a running process
    // - this method will work for .exe and for .dll (or .ocx)
    procedure SaveToExe(const aExeName: TFileName;
      Scope: TDebugFileScope = []);
    /// save all debugging information as JSON content
    // - may be useful from debugging purposes
    procedure SaveToJson(W: TTextWriter); overload;
    /// save all debugging information as a JSON file
    // - may be useful from debugging purposes
    procedure SaveToJson(const aJsonFile: TFileName;
      aJsonFormat: TTextWriterJsonFormat = jsonCompact); overload;
    /// check if this memory address is part of the code segments of this instance
    function IsCode(aPointer: PtrUInt): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// return the symbol location according to the supplied absolute address
    // - filename, symbol name and line number (if any), as plain text, e.g.
    // $ 5880ea mormot.core.log.pas InitializeUnit (8475)
    // $ 57f480 mormot.core.log.pas TSynLog.LogEscape (5782)
    // $ 4a0a40 mormot.core.base.asmx64.inc (mormot.core.base) Rdtsc (3005)
    // - returns only the hexadecimal value if no match is found in .map/.dbg/.mab
    // - won't allocate any heap memory during the text creation
    // - mormot.core.os.pas' GetExecutableLocation() redirects to this method
    class procedure FindLocationShort(aPointer: pointer; var aInfo: ShortString);
      {$ifdef HASINLINE} static; {$endif}
    /// return the symbol location according to the supplied absolute address
    // - filename, symbol name and line number (if any), as plain text, e.g.
    // $ 57f480 mormot.core.log.pas TSynLog.LogEscape (5782)
    // $ 4a0a40 mormot.core.base.asmx64.inc (mormot.core.base) Rdtsc (3005)
    // - returns only the hexadecimal value if no match is found in .map/.dbg/.mab
    class function FindLocation(aPointer: pointer): RawUtf8; overload;
      {$ifdef HASINLINE} static; {$endif}
    /// load .map/.dbg/.mab info and return the symbol location according
    // to the supplied ESynException.RaisedAt value
    // - i.e. unit name, symbol name and line number (if any), as plain text
    class function FindLocationRaisedAt(exc: ESynException): RawUtf8;
      {$ifdef HASINLINE} static; {$endif}
    /// load .map/.dbg/.mab info and returns the file name of a given unit
    // - if unitname is '', returns the main file name of the current executable
    class function FindFileName(const unitname: RawUtf8): TFileName;
      {$ifdef HASINLINE} static; {$endif}
    {$ifdef FPC}
    /// load DWARF .dbg/.mab info and replace FPC RTL BacktraceStrFunc()
    // - uses much less disk space (e.g. 33MB .dbg into 500KB)
    // - is much faster: around 1us per lookup, whereas lnfodwrf is 20ms
    class function RegisterBacktraceStrFunc: boolean; static;
    {$endif FPC}
    /// add some debugging information about the supplied absolute memory address
    class function AddLog(W: TTextWriter; aPointer: PtrUInt;
      NoHex: boolean = false): boolean; {$ifdef HASINLINE} static; {$endif}
    /// return the current thread stack trace as convenient plain text
    // - filename, symbol name and line number (if any) of each frame, e.g.
    // $ 57f480 mormot.core.log.pas TSynLog.LogEscape (5782) 4a0a40 ...
    // - skip is the number of caller frames to ignore (0 = start at caller)
    // - depth is the maximum number of located frames (0 = 30 as TSynLog)
    // - could be used e.g. for diagnostic endpoints or error reporting, with
    // no exception involved - returns '' if no stack trace is available
    class function StackTrace(skip: integer = 0; depth: integer = 0;
      use: TSynLogStackTraceUse = stManualAndAPI): RawUtf8; overload;
      {$ifdef HASINLINE} static; {$endif}
    /// append the current thread stack trace to an existing TTextWriter
    // - the current thread stack is captured via the RTL CaptureBacktrace()
    // on FPC, or the RtlCaptureStackBackTrace() API on Delphi + Windows,
    // with a manual stack walk fallback on Delphi Win32 (where this API is
    // limited) - not implemented on Delphi POSIX yet (nothing is appended)
    // - use follows TSynLogFamily.StackTraceUse semantics: ignored on FPC,
    // and stOnlyManual is implemented on Delphi Win32 only, as TSynLog
    // - skip does not apply to the heuristic manual stack walk
    // - a trailing space is left after each located frame, as TDebugFile.AddLog
    class procedure StackTrace(W: TTextWriter; skip: integer = 0;
      depth: integer = 0;
      use: TSynLogStackTraceUse = stManualAndAPI); overload;
      {$ifdef HASINLINE} static; {$endif}
    /// low-level resolution of a TDebugFile instance from a code address
    // - this is the main internal thread-safe factory method for this process
    // - returns nil if this code address has no known debug information
    class function Get(aPointer: pointer): TDebugFile;
      {$ifdef HASINLINE} static; {$endif}
    /// low-level resolution of the main TDebugFile from the current exe/dll
    class function CurrentDebugFile: TDebugFile;
      {$ifdef HASINLINE} static; {$endif}
    /// all symbols decoded from the debug information
    // - may refer to a global variable, function or method name and address
    property Symbols: TDebugSymbolDynArray
      read fSymbol;
    /// all source line blocks decoded from the debug information
    // - each block maps source lines to one contiguous executable address range
    // - may refer to a Pascal unit, included file, or compiler-generated code
    property Blocks: TDebugBlockDynArray
      read fBlock;
  published
    /// the associated executable or library file 
    property ExeFile: TFileName
      read fExeFile;
    /// the local timestamp of the main ExeFile
    property ExeDate: RawUtf8
      read GetExeDate;
    /// the expected location of the associated .mab file (may be non existing)
    property MabFile: TFileName
      read fMabFile;
    /// details about the compiler version - only available for FPC yet
    property Producer: RawUtf8
      read fProducer;
    /// equals true if a .map/.dbg or .mab debugging information has been loaded
    property DebugInfo: TDebugInfo
      read fDebugInfo;
    /// how many identifiers are currently stored in Symbols[]
    property SymbolsCount: integer
      read fSymbolsCount;
    /// how many code blocks with line info are currently stored in Blocks[]
    property BlocksCount: integer
      read fBlocksCount;
    /// how many line info are currently stored in all Blocks[].Line[]
    property LinesCount: integer
      read fLinesCount;
    /// how many microseconds did it need to parse .map/.dbg or .mab input
    property LoadingMicroSec: Int64
      read fLoadingMicroSec;
  end;
  PDebugFile = ^TDebugFile;

{$ifndef PUREMORMOT2}
  // backward compatibility type redirection
  TSynMapFile = TDebugFile;
{$endif PUREMORMOT2}

const
  /// the external debug information file extension of the current compiler
  DEBUG_EXT = {$ifdef FPC} '.dbg' {$else} '.map' {$endif};


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
  LOG_LEVEL_TEXT: array[TSynLogLevel] of TShort7 = (
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
  SynLogGlobalLock: TOSLock;

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

  /// how file existing shall be handled during logging
  TSynLogExistsAction = (
    acOverwrite,
    acAppend);

  {$ifndef NOEXCEPTIONINTERCEPT}
  /// callback signature used by TSynLogFamilly.OnBeforeException
  // - should return false to log the exception, or true to ignore it
  TOnBeforeException = function(const Context: TSynLogExceptionContext;
    const ThreadName: ShortString): boolean of object;
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
    fHandleExceptions, fExceptionIgnoreExternal: boolean;
    {$ifndef NOEXCEPTIONINTERCEPT}
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
    procedure SetEchoCustom(const aEvent: TOnTextWriterEcho);
    function GetSynLogClassName: string;
    function ArchiveAndDeleteFile(const aFileName: TFileName): boolean;
    function GetArchiveDestPath(age: TDateTime): TFileName;
    function GetCurrentThreadFlag(ti: TSynLogThreadInfoFlag): boolean;
    procedure SetCurrentThreadFlag(ti: TSynLogThreadInfoFlag; value: boolean);
  public
    /// initialize for a TSynLog class family
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
    /// allow to (temporarly) ignore exceptions in the current thread
    // - this property will affect all TSynLogFamily instances, for the
    // current thread
    // - may be used in a try...finally block e.g. when notifying the exception
    // to a third-party service, or during a particular process
    // - see also ExceptionIgnore property - which is also checked in addition
    // to this flag
    // - do nothing if exceptions are not intercepted on this target platform
    property ExceptionIgnoreCurrentThread: boolean
      index tiExceptionIgnore read GetCurrentThreadFlag write SetCurrentThreadFlag;
    /// set true will log exceptions only from the current (exe/dll) module
    // - will follow IsCurrentExecutable() logic against HInstance
    // - do nothing if exceptions are not intercepted on this target platform
    property ExceptionIgnoreExternal: boolean
      read fExceptionIgnoreExternal write fExceptionIgnoreExternal;
    /// allow to temporarly avoid logging in the current thread
    // - won't affect exceptions logging, as one would expect for safety reasons
    // - after setting true to this property, should eventually be reset to false:
    // ! TSynLog.Family.DisableCurrentThread := true;
    // ! try
    // !   ...
    // ! finally
    // !   TSynLog.Family.DisableCurrentThread := false;
    // ! end;
    // - do nothing if exceptions are not intercepted on this target platform
    property DisableCurrentThread: boolean
      index tiTemporaryDisable read GetCurrentThreadFlag write SetCurrentThreadFlag;
    {$ifndef NOEXCEPTIONINTERCEPT}
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
    /// redirect all EchoToConsole logging into the system journal service
    // - redirect log output to our JournalSend() function
    // - on Linux, will first try systemd journal, and fallback to syslog()
    // - on BSD/MacOS, will call libc syslog()
    // - on Windows, will call OutputDebugStringW() - TODO: support EWT
    // - such logs can be exported into a format which can be viewed by our
    // LogView tool using the following command (replacing UNIT with
    // your unit name and PROCESS with the executable name):
    // $ "journalctl -u UNIT --no-hostname -o short-iso-precise --since today | grep "PROCESS\[.*\]:  . " > todaysLog.log"
    property EchoToConsoleUseJournal: boolean
      read fEchoToConsoleUseJournal write fEchoToConsoleUseJournal;
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
    /// if TRUE, will log high-resolution time stamp (as hexadecimal microseconds)
    // instead of the ISO 8601 date and time
    // - this is less human readable, but allows performance profiling of your
    // application on the customer side (in addition to TSynLog.Enter methods)
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
    /// pre-computed "1 shl ((ThreadNumber - 1) and 31)" value as 32-bit mask
    // - equals 0 if InitThreadNumber() needs to be called
    ThreadBitLo: cardinal;
    /// pre-computed "(ThreadNumber - 1) shr 5" value
    ThreadBitHi: word;
    /// ready-to-be-written text timestamp, filled outside SynLogGlobalLock
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

  /// low-level callback triggered within the raw logging context
  // - allow TSynLog.RawLog() to ouput directly some data to Sender.Writer
  // - is called between LogHeader/LogTrailer methods, in the global lock
  // - the implementation should be stable and don't break the same-line output
  TOnRawLog = procedure(Sender: TSynLog; Level: TSynLogLevel;
    Opaque: pointer; Value: PtrInt; Instance: TObject) of object;

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
    fThreadInfoBackup: TSynLogThreadInfoFlags; // 8-bit
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
      iid: TGuid; out obj): TIntQry; {$ifdef FPCPOSIX}cdecl{$else}stdcall{$endif};
    function _AddRef: TIntCnt;       {$ifdef FPCPOSIX}cdecl{$else}stdcall{$endif};
    function _Release: TIntCnt;      {$ifdef FPCPOSIX}cdecl{$else}stdcall{$endif};
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
    /// initialize for a TSynLog class instance
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
    // - will force a manual breakpoint if tests are run from the Delphi IDE,
    // and will output the message to the current console
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
    /// call this method to execute a callback within custom TJsonWriter
    // - can be used to output directly e.g. JSON content into Sender.Writer
    // - Opaque/Value will be passed to Event, together with Instance
    procedure RawLog(Level: TSynLogLevel; const Event: TOnRawLog;
      Opaque: pointer = nil; Value: PtrInt = 0; Instance: TObject = nil);
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
    // - will enter the SynLogGlobalLock - and is NOT reentrant
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

// mostly published for regression tests
procedure CleanThreadName(var name: RawUtf8);


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
    fCount: integer; // not PtrInt
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
    /// the associated time elapsed in this method (in microseconds)
    // - computed from the sllLeave time difference (high resolution timer)
    // - 32-bit microseconds value would overflow after 1 hour and 11 minutes
    Time: cardinal;
    /// the time elapsed in this method and not in nested methods
    // - computed from Time property, minus the nested calls
    // - 32-bit microseconds value would overflow after 1 hour and 11 minutes
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
    fWow64: boolean;
    fWindowsSpecs: TWindowsSpecs;
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
    property WindowsSpecs: TWindowsSpecs
      read fWindowsSpecs;
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

/// raw computation of a RFC 5424 syslog message content into a memory buffer
// - returns the number of bytes written to destbuffer (with destsize > 127)
function SyslogMessage(facility: TSyslogFacility; severity: TSyslogSeverity;
  P: PAnsiChar; Len: PtrInt; const procid, msgid: RawUtf8; destbuffer: PUtf8Char;
  destsize: PtrInt; trimmsgfromlog: boolean; const appname: RawUtf8 = ''): PtrInt;

/// high-level computation of a RFC 5424 syslog message content
// - ready to be sent via UDP or TLS to a syslog remote server
// - TlsTcpFormat will prepend <len><space><sysmessage> as per RFC
// - use Temp as temporary storage, and return Dest/result bytes from it
function SyslogPrepare(Level: TSynLogLevel; Text: PUtf8Char; Len: PtrInt;
  var Temp: TBuffer2K; out Dest: PUtf8Char; TlsTcpFormat: boolean = true;
  TrimSynLogDate: boolean = false; const AppName: RawUtf8 = '';
  const MsgId: RawUtf8 = ''): PtrInt;

/// high-level computation of a RFC 3164 original BSD syslog message content
// - as expected locally on /var/log unix socket dgram in most POSIX systems
// but not on systemd Linux which is incompatible and favors sd_journal_send()
// - returns the number of bytes written to Temp
function SyslogBsdPrepare(Level: TSynLogLevel; Text: PUtf8Char; Len: PtrInt;
  var Temp: TBuffer2K; TrimSynLogDate: boolean = false;
  const AppName: RawUtf8 = ''): PtrInt;

/// send an event to the Operating System journal
// - use systemd library on Linux with fallback to syslog() on POSIX
// - on Windows, calls OutputDebugStringW() - TODO: use bloated ETW API?
// - as used e.g. for TSynLogFamily.EchoToConsoleUseJournal process
// - input text would detect and trim "20200615 08003008 xxxx" TSynLog format,
// unless TrimSynLogDate is forced to false
function JournalSend(Level: TSynLogLevel; const Text: RawUtf8;
  TrimSynLogDate: boolean = true {$ifdef OSLINUX};
  NoSysLogFallback: boolean = false {$endif OSLINUX}): boolean; overload;

/// send an event to the Operating System journal from an UTF-8 buffer
function JournalSend(Level: TSynLogLevel; Text: PUtf8Char; Len: PtrInt;
  TrimSynLogDate: boolean = true {$ifdef OSLINUX};
  NoSysLogFallback: boolean = false {$endif OSLINUX}): boolean; overload;

{$ifdef OSLINUX}
/// send an event to the systemd library with no fallback to syslog()
// - compatibility function with older mORMot - use JournalSend() instead
function SystemdEcho(Level: TSynLogLevel; const Text: RawUtf8;
  TrimSynLogDate: boolean = true): boolean;
{$endif OSLINUX}

/// extract the meaningfull text from a raw TSynLog output line
// - as used by SyslogMessage() and SystemdEcho()
procedure TrimSynLogMessage(var P: PUtf8Char; var len: PtrInt;
  trimSynLogDate: boolean; maxLen: PtrInt);


implementation

{$ifdef FPCDARWIN}
uses
  exeinfo; // MachO executable raw access for GDB DWARF support
{$endif FPCDARWIN}


{ ************** Debug Symbols Processing from Delphi .map or FPC/GDB DWARF }

{ TDebugFile }

function TDebugFile.AbsoluteToRelative(aPointer: PtrUInt): TDebugAddress;
begin
  dec(aPointer, fCodeOffset);
  if (PtrInt(aPointer) < PtrInt(fStart)) or
     (aPointer > fStop) then
    aPointer := 0; // our RVA should be positive and in 32-bit range
  result := aPointer;
end;

function TDebugFile.IsCode(aPointer: PtrUInt): boolean;
begin
  dec(aPointer, fCodeOffset); // inlined AbsoluteToRelative()
  result := (PtrInt(aPointer) >= PtrInt(fStart)) and
            (aPointer <= fStop);
end;

var
  DebugFileLast, DebugFileCurrent: TDebugFile; // aligned pointer access is atomic
  DebugFilesSafe: TRWLightLock;
  DebugFiles: array of TDebugFile;
  DebugFileNamesUnknown: TStringDynArray; // search once

function DebugFileSearch(f: PDebugFile; a: PtrUInt): TDebugFile;
var
  n: integer;
begin
  if f <> nil then
  begin
    n := PDALen(PAnsiChar(f) - _DALEN)^ + _DAOFF;
    repeat
      result := f^;
      if result.IsCode(a) then
        exit;
      inc(f);
      dec(n);
    until n = 0;
  end;
  result := nil;
end;

function DebugFileRegister(a: PtrUInt; s: PRawUtf8): TDebugFile;
var
  base: PtrUInt;     // where this exe/lib has been loaded
  symbol: PUtf8Char; // hold dlinfo.dli_sname - nil on Windows
  i: PtrInt;
  fn: TFileName;
begin
  result := nil;
  fn := GetExecutableName(pointer(a), @base, @symbol); // e.g. fast dladdr()
  if fn = '' then
    exit;
  DebugFilesSafe.WriteLock; // safe blocking registration process
  try
    if SynLogFileFreeing or
       (FindString(DebugFileNamesUnknown, fn) >= 0) then // known to be unknown
      exit;
    result := DebugFileSearch(pointer(DebugFiles), a); // paranoid
    if result <> nil then
      exit; // was registered in another background thread
    for i := 0 to length(DebugFiles) - 1 do
      if DebugFiles[i].fExeFile = fn then
        exit; // a is part of this exe/dll but outside of the debug info range
    try
      result := TDebugFile.Create(fn);
    except
      FreeAndNil(result);
    end;
    if (result = nil) or
       (result.DebugInfo = diNone) then
    begin
      AddString(DebugFileNamesUnknown, fn);
      FreeAndNil(result);
      exit;
    end;
    {$ifdef ISDELPHI}
    if base <> 0 then
      inc(base, $1000); // Delphi include BaseOfCode as .map offset
    {$endif ISDELPHI}
    result.fCodeOffset := base; // may be random for .dll or ASLR
    ObjArrayAdd(DebugFiles, result);
    if result.IsCode(PtrUInt(@DebugFileRegister)) then
      DebugFileCurrent := result
    else
      DebugFileLast := result;
    if not result.IsCode(a) then
      result := nil; // we loaded this exe/lib debug info but a is outside
  finally
    DebugFilesSafe.WriteUnLock;
    if (s <> nil) and
       (result = nil) then
      Make([' ', GetFileNameWithoutExtOrPath(fn), ' ', symbol], s^);
  end;
end;

function DebugFileGet(a: PtrUInt; s: PRawUtf8): TDebugFile;
begin
  result := nil;
  if SynLogFileFreeing or
     (a = 0) then
    exit;
  // naive but very efficient cache of last used TDebugFile instances
  result := DebugFileCurrent;
  if (result <> nil) and
     result.IsCode(a) then
    exit; // most common case
  result := DebugFileLast;
  if (result <> nil) and
     result.IsCode(a) then
    exit;
  // non-blocking search of this address in existing TDebugFile instances
  DebugFilesSafe.ReadLock;
  result := DebugFileSearch(pointer(DebugFiles), a);
  DebugFilesSafe.ReadUnLock;
  // call GetExecutableName() and try to create a new TDebugFile instance
  if result = nil then
    result := DebugFileRegister(a, s)
  else
    DebugFileLast := result;
end;

class function TDebugFile.Get(aPointer: pointer): TDebugFile;
begin
  result := DebugFileGet(PtrUInt(aPointer), nil);
end;

class function TDebugFile.CurrentDebugFile: TDebugFile;
begin
  if DebugFileCurrent = nil then // resolve local procedure
  begin
    DebugFileCurrent := Get(@TDebugFile.CurrentDebugFile);
    if DebugFileCurrent = nil then
      DebugFileCurrent := pointer(1);
  end;
  result := DebugFileCurrent;
  if result = pointer(1) then
    result := nil;
end;

{$ifdef FPC}

{  FPC can export DWARF/GDB info on POSIX and Windows from the project options.
   Code below was inspired - but highly rewritten - from RTL's linfodwrf.pp }

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

  TDwarfDebugAttr = packed record
    attr, form: byte; // attr truncated to 8-bit - store 0 if > 255
  end;

  TDwarfDebugAbbrev = record
    Tag: HalfUInt;
    AttrsCount: byte;
    Child: byte;
    Attrs: array of TDwarfDebugAttr;
  end;

  TDwarfMachineState = record
  public
    flags: set of (isstmt, basicblock, endsequence,
      prologueend, epiloguebegin, appendrow, invalidaddress);
    line, address, fileid: cardinal; // PInt64(@state.line)^ stored in Blocks[]
    procedure Init(aIs_Stmt: ByteBool);
  end;

  TDwarfReader = record
  public
    Read: TFastReader;
    Abbrev: array of TDwarfDebugAbbrev; // debug_abbrev content
    AttrsMax: cardinal;
    Dwarf64, IncludesDir: boolean;
    LineOffset, LineSize,              // debug_line
    InfoOffset, InfoSize,              // debug_info
    AbbrevOffset, AbbrevSize: integer; // debug_abbrev
    ImageBase: QWord; // e.g. 0100000000 on Win64 or 00400000 on Win32
    Owner: TDebugFile;
    Lines: TInt64DynArray; // TDebugBlock.Addr[] in high 32-bit, Line[] in lower
    Dirs, Files: TRawUtf8DynArray;
    FilesDir: TIntegerDynArray;
    Map: TMemoryMap;
    temp: ShortString;
    numoptable: array[1..255] of byte; // start at index 1 -> no THash2048
    function LoadSections: boolean;
    procedure ReadInit(aBase, aLimit: Int64);
    function ReadLeb128: Int64;
    function ReadAddress(addr_size: PtrInt; ctx: PUtf8Char): cardinal;
    procedure SkipAttr(form: PtrUInt; const header64: TDwarfDebugInfoHeader64);
    procedure ReadAbbrevTable(file_offset, file_size: QWord);
    function ParseCompilationUnit(file_offset, file_size: QWord): QWord;
    function ParseCompilationFunctions(file_offset, file_size: QWord): QWord;
  end;

procedure TDwarfMachineState.Init(aIs_Stmt: ByteBool);
begin
  byte(flags) := 0;
  if aIs_Stmt then
    include(flags, isstmt);
  address := 0;
  line := 1;
  fileid := 1;
end;

{.$define DWARFDEBUG} // for internal raw debugging

{$ifdef FPCDARWIN}
// use FPC RTL's cross-OS exeinfo.pp unit for macho format
function TDwarfReader.LoadSections: boolean;
var
  e: TExeFile;
begin
  result := false;
  // open exe filename or follow '.gnu_debuglink' redirection
  temp := Owner.fExeFile;
  Owner.fDebugInfo := diInternalDwarf;
  if not OpenExeFile(e, temp) then
  begin
    {$ifdef DWARFDEBUG}
    ConsoleWrite(['OpenExeFile failed on ', temp]);
    {$endif DWARFDEBUG}
    exit;
  end;
  if ReadDebugLink(e, temp) then // is there an external .dbg file?
  begin
    CloseExeFile(e);
    if not OpenExeFile(e, temp) then
    begin
      {$ifdef DWARFDEBUG}
      ConsoleWrite(['OpenExeFile failed on ', temp]);
      {$endif DWARFDEBUG}
      exit;
    end;
    Owner.fDebugInfo := diExternalDwarf;
  end;
  // locate debug_* sections after successfull OpenExeFile()
  if FindExeSection(e, '.debug_line', LineOffset, LineSize) and
     FindExeSection(e, '.debug_info', InfoOffset, InfoSize) and
     FindExeSection(e, '.debug_abbrev', AbbrevOffset, AbbrevSize) then
    result := Map.Map(temp);
  CloseExeFile(e);
end;
{$else}
// use our faster mormot.core.os.FindExeSection(TMemoryMap) on Linux+BSD+Windows
function TDwarfReader.LoadSections: boolean;
var
  off, siz, dbglen: integer; // not PtrInt
  crc: cardinal;
  dbgname: PUtf8Char;
  fn: string;
begin
  result := false;
  if not Map.Map(Owner.fExeFile, {forcemap=}true) then // main exe
    exit;
  Owner.fDebugInfo := diInternalDwarf;
  if FindExeSection(Map, '.gnu_debuglink', off, siz) <> efUnknown then
  begin
    dbgname := pointer(Map.Buffer + off);
    dbglen := StrLen(dbgname);
    if (dbglen = 0) or
       (dbglen > siz) or
       not IsValidUtf8WithoutControlChars(pointer(dbgname), dbglen) then
      exit;
    crc := PCardinal(dbgname + ((dbglen + 4) and not 3))^; // read before UnMap
    Utf8DecodeToString(dbgname, dbglen, fn); // e.g. mormot2tests.dbg
    Map.UnMap; // close main exe
    if Map.Map(Owner.fExePath + fn) or  // search dbg in dll/exe folder
       ((Executable.ProgramFilePath <> Owner.fExePath) and
        Map.Map(Executable.ProgramFilePath + fn)) then // search dbg with exe
      if crc32(0, Map.Buffer, Map.Size) <> crc then    // zlib algorithm
      begin
        Map.UnMap; // the located debug file does not match the executable
        exit;
      end;
    Owner.fDebugInfo := diExternalDwarf;
  end;
  if (FindExeSection(Map, '.debug_line', LineOffset, LineSize, @ImageBase) <> efUnknown) and
     (FindExeSection(Map, '.debug_info', InfoOffset, InfoSize) <> efUnknown) and
     (FindExeSection(Map, '.debug_abbrev', AbbrevOffset, AbbrevSize) <> efUnknown) then
    result := true;
  if result then
    SetLength(Files, 64) // good enough for most executables
  else
    Map.UnMap;
end;
{$endif FPCDARWIN}

procedure TDwarfReader.ReadInit(aBase, aLimit: Int64);
begin
  if aBase + aLimit > Int64(Map.Size) then
    Read.ErrorOverflow;
  Read.Init(Map.Buffer + aBase, aLimit);
end;

function TDwarfReader.ReadLeb128: Int64;
var
  shift: byte;
  data: PtrInt;
  val: Int64;
begin // LEB-128 encoding does not match our FromVarInt64 sign extension
  data := Read.NextByte;
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
    data := Read.NextByte;
  until false;
  // extend sign from current shifted bits
  result := (not ((result and (Int64(1) shl (shift - 1))) - 1)) or result;
end;

function TDwarfReader.ReadAddress(addr_size: PtrInt; ctx: PUtf8Char): cardinal;
var
  tmp: QWord; // temporary 64-bit variable on stack
begin
  if addr_size > SizeOf(tmp) then // typically 4 or 8
    Read.ErrorData('DWARF: ReadAddress % len=%', [ctx, addr_size]);
  tmp := 0;
  Read.Copy(@tmp, addr_size);
  if tmp > ImageBase then
  begin
    dec(tmp, ImageBase);  // e.g. 0100000000 on Win64 or 00400000 on Win32
    if tmp > MaxInt then
      Read.ErrorData('DWARF: ReadAddress %=% overflow %',
        [ctx, Int64ToHexShort(tmp), addr_size]);
    result := tmp; // it is fine to truncate to 32-bit
  end
  else
    result := 0; // skip null/invalid values emitted by FPC
end;

procedure TDwarfReader.ReadAbbrevTable(file_offset, file_size: QWord);
var
  nr, t, a, f, n: PtrUInt;
  p: ^TDwarfDebugAbbrev;
  bakp, baklast: pointer;
begin
  bakp := Read.P;
  baklast := Read.Last;
  ReadInit(file_offset, file_size);
  AttrsMax := 0;
  repeat
    nr := Read.VarUInt32;
    if nr = 0 then
      break;
    AttrsMax := MaxPtrUInt(nr, AttrsMax);
    if nr >= PtrUInt(length(Abbrev)) then
      SetLength(Abbrev, nr + 256);
    p := @Abbrev[nr];
    if p^.Attrs = nil then
      SetLength(p^.Attrs, 250);
    t := Read.VarUInt32;
    if t > high(p^.Tag) then
      Read.ErrorData('DWARF: tag=% overflow', [t]);
    p^.Tag := t;
    p^.Child := Read.NextByte;
    n := 0;
    repeat
      a := Read.VarUInt32;
      f := Read.VarUInt32;
      if a = 0 then
        break;
      if (f > 255) or
         (n > 250) then
        Read.ErrorData('DWARF: a=% f=% n=% overflow', [a, f, n]);
      // vendor-specific attributes don't fit in our byte-sized Attrs[].attr,
      // e.g. DW_AT_GNU_all_call_sites = $2117 as generated by gcc -g: they are
      // pretty common in a FPC executable statically linking any C object.
      // We only need to skip their value, which is driven by their (small)
      // form, so store attr = 0 which matches no DW_AT_* constant below.
      if a > 255 then
        a := 0;
      with p^.Attrs[n] do
      begin
        attr := a;
        form := f;
      end;
      inc(n);
    until false;
    p^.AttrsCount := n;
  until false;
  Read.P := bakp;
  Read.Last := baklast;
end;

function CalculateAddressIncrement(opcode: PtrInt;
  const header: TDwarfLineInfoHeader64): PtrInt; inline;
begin
  result := PtrInt(opcode - header.opcode_base) div header.line_range *
            header.minimum_instruction_length;
end;

// DWARF 2/3 most common opcodes
const
  DW_LNS_LNE                = 0; // see DW_LNE_*
  DW_LNS_COPY               = 1;
  DW_LNS_ADVANCE_PC         = 2;
  DW_LNS_ADVANCE_LINE       = 3;
  DW_LNS_SET_FILE           = 4;
  DW_LNS_SET_COLUMN         = 5;
  DW_LNS_NEGATE_STMT        = 6;
  DW_LNS_SET_BASIC_BLOCK    = 7;
  DW_LNS_CONST_ADD_PC       = 8;
  DW_LNS_FIXED_ADVANCE_PC   = 9;
  DW_LNS_SET_PROLOGUE_END   = 10;
  DW_LNS_SET_EPILOGUE_BEGIN = 11;
  DW_LNS_SET_ISA            = 12;

  DW_LNE_END_SEQUENCE   = 1;
  DW_LNE_SET_ADDRESS    = 2;
  DW_LNE_DEFINE_FILE    = 3;

  DW_TAG_padding        = $00;
  DW_TAG_class_type     = $02; // map Object Pascal class or object
  DW_TAG_compile_unit   = $11; // map Object pascal unit
  DW_TAG_structure_type = $13; // map Object Pascal record
  DW_TAG_subprogram     = $2e; // map object function or method

  DW_AT_name           = $03;
  DW_AT_low_pc         = $11;
  DW_AT_high_pc        = $12;
  DW_AT_producer       = $25;

  DW_FORM_addr         = $01;
  DW_FORM_block2       = $03;
  DW_FORM_block4       = $04;
  DW_FORM_data2        = $05;
  DW_FORM_data4        = $06;
  DW_FORM_data8        = $07;
  DW_FORM_string       = $08;
  DW_FORM_block        = $09;
  DW_FORM_block1       = $0a;
  DW_FORM_data1        = $0b;
  DW_FORM_flag         = $0c;
  DW_FORM_sdata        = $0d;
  DW_FORM_strp         = $0e;
  DW_FORM_udata        = $0f;
  DW_FORM_ref_addr     = $10;
  DW_FORM_ref1         = $11;
  DW_FORM_ref2         = $12;
  DW_FORM_ref4         = $13;
  DW_FORM_ref8         = $14;
  DW_FORM_ref_udata    = $15;
  DW_FORM_indirect     = $16;
  DW_FORM_sec_offset   = $17;
  DW_FORM_exprloc      = $18;
  DW_FORM_flag_present = $19;

procedure TDwarfReader.SkipAttr(form: PtrUInt; const header64: TDwarfDebugInfoHeader64);
begin
  case form of
    DW_FORM_addr:
      Read.Next(header64.address_size);
    DW_FORM_block,
    DW_FORM_exprloc:
      Read.Next(Read.VarUInt32);
    DW_FORM_block1:
      Read.Next(Read.NextByte);
    DW_FORM_block2:
      Read.Next(Read.Next2);
    DW_FORM_block4:
      Read.Next(Read.Next4);
    DW_FORM_ref1,
    DW_FORM_data1,
    DW_FORM_flag:
      Read.NextByte;
    DW_FORM_ref2,
    DW_FORM_data2:
      Read.Next2;
    DW_FORM_ref4,
    DW_FORM_data4:
      Read.Next4;
    DW_FORM_ref8,
    DW_FORM_data8:
      Read.Next8;
    DW_FORM_string:
      Read.NextAsciiz;
    DW_FORM_ref_udata,
    DW_FORM_udata,
    DW_FORM_sdata:
      Read.VarNextInt;
    DW_FORM_ref_addr:
      if header64.version > 2 then
        if Dwarf64 then
          Read.Next8
        else
          Read.Next4
      else if header64.address_size < 4 then
        Read.Next4
      else
        Read.Next(header64.address_size);
    DW_FORM_strp,
    DW_FORM_sec_offset:
      if Dwarf64 then
        Read.Next8
      else
        Read.Next4;
    DW_FORM_indirect:
      SkipAttr(Read.VarUInt32, header64);
    DW_FORM_flag_present:
      ; // none
  else
    Read.ErrorData('DWARF: unknown form: %', [form]);
  end;
end;

procedure FinalizeLines(b: PDebugBlock; n: PtrInt; p64: PInt64; dosort: boolean);
var
  i: PtrInt;
begin
  if (b = nil) or
     (n = 0) then
    exit;
  if dosort then
  begin
    QuickSortInt64(pointer(p64), 0, n - 1); // sort by Addr (high 32-bit)
    b^.Symbol.Start := p64^ shr 32; // set to unit first function Addr
  end;
  SetLength(b^.Addr, n);
  SetLength(b^.Line, n);
  for i := 0 to n - 1 do
  begin
    b^.Line[i] := p64^;        // low 32-bit
    b^.Addr[i] := p64^ shr 32; // high 32-bit
    inc(p64);
  end;
end;

function TDwarfReader.ParseCompilationUnit(file_offset, file_size: QWord): QWord;
var
  opcode, opcodeadjust, divlinerange,
  prevaddr, prevfile, prevline: cardinal;
  unitlen: QWord;
  opcodeextlen, headerlen, ndx: PtrInt;
  dirsn, filesn, linesn: integer;
  state: TDwarfMachineState;
  c: ansichar;
  unsorted: boolean;
  header64: TDwarfLineInfoHeader64;
  header32: TDwarfLineInfoHeader32;
  b: PDebugBlock;
  name: PAnsiChar;
  namelen: integer;
begin
  // check if DWARF 32-bit or 64-bit debug_line section format
  ReadInit(file_offset, file_size);
  header32.unit_length := Read.Next4;
  if header32.unit_length = 1 then // Elf64_Chdr.ch_type = ELFCOMPRESS_ZLIB
    Read.ErrorData('DWARF: unsupported SHF_COMPRESSED format', []);
  Dwarf64 := header32.unit_length = $ffffffff;
  if Dwarf64 then
    unitlen := Read.Next8 + SizeOf(header64.magic) + SizeOf(header64.unit_length)
  else
    unitlen := header32.unit_length + SizeOf(header32.unit_length);
  result := file_offset + unitlen;
  // normalize debug_line header into header64 fields
  ReadInit(file_offset, unitlen);
  if Dwarf64 then
  begin
    Read.Copy(@header64, SizeOf(header64));
    headerlen := SizeOf(header64.magic) + SizeOf(header64.unit_length) +
      SizeOf(header64.version) + SizeOf(header64.length) + header64.length;
  end
  else
  begin
    Read.Copy(@header32, SizeOf(header32));
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
  end;
  // read opcode parameter count table
  FillcharFast(numoptable, SizeOf(numoptable), 0);
  Read.Copy(@numoptable, header64.opcode_base - 1);
  // read directory and file names
  dirsn := 0;
  repeat
    namelen := Read.NextAsciiz(@name);
    if namelen = 0 then
      break;
    if not IncludesDir then
      continue;
    c := PathDelim;
    if ByteScanIndex(pointer(name), namelen, ord(InvertedPathDelim)) >= 0 then
      c := InvertedPathDelim;
    SetString(temp, name, namelen);
    if name[namelen - 1] <> c then
      AppendShortCharSafe(c, temp);
    if dirsn = length(Dirs) then
      SetLength(Dirs, NextGrow(dirsn));
    ShortStringToAnsi7String(temp, Dirs[dirsn]);
    inc(dirsn);
  until false;
  filesn := 0;
  repeat
    namelen := Read.NextAsciiz(@name);
    if namelen = 0 then
      break;
    if filesn = length(Files) then
      SetLength(Files, NextGrow(filesn));
    FastSetString(Files[filesn], name, namelen);
    AddInteger(FilesDir, filesn, Read.VarUInt32);
    Read.VarNextInt(2); // we ignore the attributes
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
  b := nil;
  while Read.NextByteSafe(@opcode) do
  begin
    case opcode of
      DW_LNS_LNE:
        begin
          // extended opcode
          opcodeextlen := Read.VarUInt32;
          case Read.NextByte of
            DW_LNE_END_SEQUENCE:
              state.flags := state.flags + [endsequence, appendrow];
            DW_LNE_SET_ADDRESS:
              begin
                state.address := ReadAddress(opcodeextlen - 1, 'CU');
                if state.address = 0 then // FPC sometimes emits these :(
                  include(state.flags, invalidaddress) // just ignore
                else
                  exclude(state.flags, invalidaddress)
              end;
          else
            // Unknown extended opcode
            Read.Next(opcodeextlen - 1);
          end;
        end;
      DW_LNS_COPY:
        state.flags := state.flags - [basicblock, prologueend, epiloguebegin]
                                   + [appendrow];
      DW_LNS_ADVANCE_PC:
        inc(state.address, Read.VarUInt32 * header64.minimum_instruction_length);
      DW_LNS_ADVANCE_LINE:
        // use ReadLeb128 < 0 to decrease state.line when needed
        state.line := Int64(state.line) + ReadLeb128;
      DW_LNS_SET_FILE:
        state.fileid := Read.VarUInt32;
      DW_LNS_NEGATE_STMT:
        if isstmt in state.flags then
          exclude(state.flags, isstmt)
        else
          include(state.flags, isstmt);
      DW_LNS_SET_BASIC_BLOCK:
        include(state.flags, basicblock);
      DW_LNS_CONST_ADD_PC:
        inc(state.address, CalculateAddressIncrement(255, header64));
      DW_LNS_FIXED_ADVANCE_PC:
        inc(state.address, Read.Next2);
      DW_LNS_SET_PROLOGUE_END:
        include(state.flags, prologueend);
      DW_LNS_SET_EPILOGUE_BEGIN:
        include(state.flags, epiloguebegin);
      DW_LNS_SET_COLUMN,
      DW_LNS_SET_ISA:
        Read.VarNextInt;
    else
      if opcode < header64.opcode_base then
        // skip unsupported standard opcode
        Read.VarNextInt(numoptable[opcode])
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
        state.flags := state.flags - [basicblock, prologueend, epiloguebegin]
                                   + [appendrow];
      end;
    end;
    if appendrow in state.flags then
    begin
      exclude(state.flags, appendrow);
      if (state.flags * [isstmt, invalidaddress] = [isstmt]) and
         (state.line > 1) and
         (state.line <> prevline) then
      begin
        prevline := state.line;
        if prevfile <> state.fileid then
        begin
          // each nested .inc/.pas triggers a new Blocks[] record
          FinalizeLines(b, linesn, pointer(Lines), unsorted);
          linesn := 0; // reuse the same 64-bit Lines[] buffer for Addr[]+Line[]
          prevaddr := 0;
          prevfile := state.fileid;
          ndx := prevfile - 1;
          {$ifdef DWARFDEBUG}
          ConsoleWrite(['-------------- ', Files[ndx]]);
          {$endif DWARFDEBUG}
          b := Owner.fBlocks.NewPtr;
          b^.Symbol.Name := Files[ndx]; // will eventually be replaced with CU
          if IncludesDir and
             (FilesDir[ndx] > 0) then
            Join([Dirs[FilesDir[ndx] - 1], Files[ndx]], b^.FileName)
          else
            b^.FileName := Files[ndx];
          b^.Symbol.Start := state.address;
        end;
        if state.address < prevaddr then
          // not increasing: need to sort b^.Addr[]+Line[] and b^.Symbol.Start
          unsorted := true;
        prevaddr := state.address;
        AddInt64(Lines, linesn, PInt64(@state.line)^); // address=hi 32-bit
        {$ifdef DWARFDEBUG}
        ConsoleWrite([Files[state.fileid - 1], ' ', state.line, ' ',
          CardinalToHexShort(state.address)]);
        {$endif DWARFDEBUG}
      end;
      if endsequence in state.flags then
        state.Init(header64.default_is_stmt);
    end;
  end;
  FinalizeLines(b, linesn, pointer(Lines), unsorted);
end;

procedure FinalizeLinesSymbol(b: PDebugBlock; n, low_pc, high_pc: PtrInt;
  id: PUtf8Char; idlen: PtrInt);
var
  start, len, i: PtrInt;
  name: RawUtf8;
begin // set Symbol.Name as main Pascal unit identifier as with Delphi .map
  if b <> nil then
    repeat
      start := b^.Symbol.Start; // note: b^.Symbol.Stop = 0 at this point
      if start >= high_pc then
        break // GenerateFromMapOrDwarf made fBlocks.Sort(SymbolSortByStartAddr)
      else if start >= low_pc then
      begin
        if name = '' then
        begin
          start := 0;
          len := idlen;
          for i := len - 1 downto 0 do
            case id[i] of
              '/', '\':
                begin // ../src/mormot.core.os.pas -> mormot.core.os.pas
                  start := i + 1;
                  break;
                end;
              '.': // mormot.core.os.pas -> mormot.core.os
                if len = idlen then
                  len := i;
            end;
          LowerCaseCopy(id + start, len - start, name);
        end;
        b^.Symbol.Name := name;
      end;
      inc(b);
      dec(n);
    until n = 0;
end;

function TDwarfReader.ParseCompilationFunctions(file_offset, file_size: QWord): QWord;
var
  s: ^TDebugSymbol;
  ab: ^TDwarfDebugAbbrev;
  a: ^TDwarfDebugAttr;
  header64: TDwarfDebugInfoHeader64;
  header32: TDwarfDebugInfoHeader32;
  unit_length: QWord;
  low_pc, high_pc, namelen, typlen, txtlen: integer;
  abbr, level, n: cardinal;
  name, typ, txt: PAnsiChar;
begin
  // check if DWARF 32-bit or 64-bit debug_info section format
  ReadInit(file_offset, file_size);
  header32.unit_length := Read.Next4;
  Dwarf64 := header32.unit_length = $ffffffff;
  if Dwarf64 then
    unit_length := Read.Next8 + SizeOf(header64.magic) + SizeOf(header64.unit_length)
  else
    unit_length := header32.unit_length + SizeOf(header32.unit_length);
  result := file_offset + unit_length;
  ReadInit(file_offset, unit_length);
  // normalize debug_info header into header64 fields
  if not Dwarf64 then
  begin
    Read.Copy(@header32, SizeOf(header32));
    header64.magic := $ffffffff;
    header64.unit_length := header32.unit_length;
    header64.version := header32.version;
    header64.debug_abbrev_offset := header32.debug_abbrev_offset;
    header64.address_size := header32.address_size;
  end
  else
    Read.Copy(@header64, SizeOf(header64));
  // Read the debug_abbrev section corresponding to this debug_info section
  ReadAbbrevTable(AbbrevOffset + header64.debug_abbrev_offset, AbbrevSize);
  // main decoding loop
  level := 0;
  abbr := Read.VarUInt32;
  typlen := 0;
  while abbr <> 0 do
  begin
    if abbr > AttrsMax then
      Read.ErrorData('DWARF: unexpected abbr=%>%', [abbr, AttrsMax]);
    ab := @Abbrev[abbr];
    if ab^.Child <> 0 then
      inc(level);
    a := pointer(ab^.Attrs);
    n := ab^.AttrsCount;
    if n <> 0 then
      if (ab^.Tag = DW_TAG_subprogram) or
         (ab^.Tag = DW_TAG_compile_unit) then
      begin
        low_pc := 1;
        high_pc := 0;
        namelen := 0;
        repeat
          case cardinal(PWord(a)^) of
            DW_AT_low_pc + DW_FORM_addr shl 8:
              low_pc := ReadAddress(header64.address_size, 'low_pc');
            DW_AT_high_pc + DW_FORM_addr shl 8:
              high_pc := ReadAddress(header64.address_size, 'high_pc');
            DW_AT_name + DW_FORM_string shl 8:
              namelen := Read.NextAsciiz(@name);
            DW_AT_producer + DW_FORM_string shl 8:
              if Owner.fProducer = '' then
              begin
                txtlen := Read.NextAsciiz(@txt);
                FastSetString(Owner.fProducer, txt, txtlen);
              end
              else
                Read.NextAsciiz;
          else
            SkipAttr(a^.form, header64);
          end;
          inc(a);
          dec(n);
        until n = 0;
        if low_pc < high_pc then
          if ab^.Tag = DW_TAG_subprogram then // only functions in Symbol[]
          begin
            s := Owner.fSymbols.NewPtr;
            if typlen = 0 then
              FastSetString(s^.Name, name, namelen)
            else
            begin
              if temp[0] = #0 then
              begin
                MoveFast(typ^, temp[1], typlen);
                if (typ[typlen - 1] <> '.') and
                   (typlen < 255) then
                begin
                  inc(typlen);
                  temp[typlen] := '.';
                end;
              end;
              temp[0] := AnsiChar(typlen); // truncate back to 'type.'
              AppendShortBuffer(name, namelen, high(temp), @temp);
              ShortStringToAnsi7String(temp, s^.Name);
            end;
            s^.Start := low_pc;
            s^.Stop := high_pc - 1;
            {$ifdef DWARFDEBUG}
            ConsoleWrite([s^.Name, ' ', CardinalToHexShort(low_pc), '-',
              CardinalToHexShort(high_pc)]);
            {$endif DWARFDEBUG}
          end
          else // Tag = DW_TAG_compile_unit
            // e.g. 'mormot.core.base.asmx86.inc' -> 'mormot.core.base.pas'
            FinalizeLinesSymbol(pointer(Owner.fBlock), Owner.fBlocksCount,
              low_pc, high_pc, name, namelen);
      end
      else if (level = 2) and
              ((ab^.Tag = DW_TAG_class_type) or
               (ab^.Tag = DW_TAG_structure_type)) then
      begin
        typlen := 0;
        temp[0] := #0; // computed on demand
        repeat
          if (a^.attr = DW_AT_name) and
             (a^.form = DW_FORM_string) then
            typlen := Read.NextAsciiz(@typ)
          else
            SkipAttr(a^.form, header64);
          inc(a);
          dec(n);
        until n = 0;
      end
      else
        repeat
          SkipAttr(a^.form, header64);
          inc(a);
          dec(n);
        until n = 0;
    if Read.EOF then
      exit;
    abbr := Read.VarUInt32;
    while (level > 0) and
          (abbr = 0) do
    begin
      if level = 1 then
        typlen := 0; // reset type name
      // skip entries signaling that no more child entries are following
      dec(level);
      if Read.EOF then
        exit;
      abbr := Read.VarUInt32;
    end;
  end;
end;

function SymbolSortByStartAddr(const A, B): integer;
begin
  result := CompareInteger(TDebugSymbol(A).Start, TDebugSymbol(B).Start);
end;

procedure TDebugFile.GenerateFromMapOrDwarf(aWithDir: boolean); // DWARF code
var
  dwarf: TDwarfReader;
  curr, last: QWord;
begin
  FillCharFast(dwarf, SizeOf(dwarf), 0);
  dwarf.Owner := self;
  dwarf.IncludesDir := aWithDir;
  if dwarf.LoadSections then
  try
    // retrieve line numbers and addresses into Lines[]
    curr := dwarf.LineOffset;
    last := curr + dwarf.LineSize;
    while curr < last do
      curr := dwarf.ParseCompilationUnit(curr, last - curr);
    fBlocks.Sort(SymbolSortByStartAddr);
    // retrieve function names into Symbols[]
    curr := dwarf.InfoOffset;
    last := curr + dwarf.InfoSize;
    while curr < last do
      curr := dwarf.ParseCompilationFunctions(curr, last - curr);
    fSymbols.Sort(SymbolSortByStartAddr);
  finally
    dwarf.Map.UnMap;
  end;
  if fBlocksCount or fSymbolsCount = 0 then
    fDebugInfo := diNone;
end;

function BacktraceStrFpc(Addr: CodePointer): ShortString;
begin
  TDebugFile.FindLocationShort(Addr, result);
end;

class function TDebugFile.RegisterBacktraceStrFunc: boolean;
begin
  BacktraceStrFunc := BacktraceStrFpc; // use our fast version from now on
  result := true;
end;

{$else}

{ Delphi can export detailed .map info as text from the project options }

function MatchPattern(P, PEnd, Up: PUtf8Char; var Dest: PUtf8Char): boolean;
begin
  result := false;
  repeat
    while (P < PEnd) and
          (P^ in [#1 .. ' ']) do
      inc(P);
    while NormToUpperAnsi7[P^] = Up^ do
    begin
      inc(P);
      if P > PEnd then
        exit;
      inc(Up);
      if (Up^ = ' ') and
         (P^ in [#1 .. ' ']) then
      begin
        // ignore multiple spaces in P^
        while (P < PEnd) and
              (P^ in [#1 .. ' ']) do
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

procedure TDebugFile.GenerateFromMapOrDwarf(aWithDir: boolean); // .map code
var
  p, pend: PUtf8Char;
  sections: TDebugBlockDynArray;

  procedure NextLine;
  begin
    while (p < pend) and
          (p^ >= ' ') do
      inc(p);
    if (p < pend) and
       (p^ = #13) then
      inc(p);
    if (p < pend) and
       (p^ = #10) then
      inc(p);
  end;

  function GetCode(var Ptr: integer): boolean;
  begin
    while (p < pend) and
          (p^ = ' ') do
      inc(p);
    result := false;
    if (p + 10 < pend) and
       (PInteger(p)^ = // 0001:## = function, 0002:## = const, 0005:##=pdata..
         ord('0') + ord('0') shl 8 + ord('0') shl 16 + ord('1') shl 24) and
       (p[4] = ':') then
    begin
      if not HexDisplayToCardinal(PAnsiChar(p) + 5, PCardinal(@Ptr)^) then
        exit;
      while (p < pend) and
            (p^ > ' ') do
        inc(p);
      while (p < pend) and
            (p^ = ' ') do
        inc(p);
      if p < pend then
        result := true; // and p points to symbol name
    end;
  end;

  procedure ReadSegments;
  var
    beg: PAnsiChar;
    b: TDebugBlock;
  begin
    NextLine;
    NextLine;
    while (p < pend) and
          (p^ < ' ') do
      inc(p);
    while (p + 10 < pend) and
          (p^ >= ' ') do
    begin
      // we just need the unit names now for ReadSymbols to detect and trim them
      // final Blocks[] will be filled in ReadLines with potential nested files
      if GetCode(b.Symbol.Start) and
         HexDisplayToCardinal(PAnsiChar(p), PCardinal(@b.Symbol.Stop)^) then
      begin
        while PWord(p)^ <> ord('M') + ord('=') shl 8 do
          if p + 10 > pend then
            exit
          else
            inc(p);
        beg := pointer(p + 2);
        while (p < pend) and
              (p^ > ' ') do
          inc(p);
        FastSetString(b.Symbol.Name, beg, p);
        inc(b.Symbol.Stop, b.Symbol.Start - 1);
        if (b.Symbol.Name <> '') and
           ((b.Symbol.Start <> 0) or
            (b.Symbol.Stop <> 0)) then
          fBlocks.FindAndAddIfNotExisting(b);
      end;
      NextLine;
    end;
  end;

  procedure ReadSymbols;
  var
    beg: PUtf8Char;
    sym: TDebugSymbol;
    {$ifdef ISDELPHI2005ANDUP}
    l, u: PtrInt;
    lastunituppercase: RawUtf8; // e.g. 'MORMOT.CORE.DATA.'
    {$endif ISDELPHI2005ANDUP}
  begin
    sym.Stop := 0;
    NextLine;
    NextLine;
    while (p + 10 < pend) and
          (p^ >= ' ') do
    begin
      if GetCode(sym.Start) then
      begin
        while (p < pend) and
              (p^ = ' ') do
          inc(p);
        beg := pointer(p);
        while (p < pend) and
              (p^ > ' ') do
          inc(p);
        {$ifdef ISDELPHI2005ANDUP}
        // trim left 'UnitName.' for each symbol (since Delphi 2005)
        if (lastunituppercase <> '') and
           IdemPChar(beg, pointer(lastunituppercase)) then
          // most common case since symbols are grouped address, i.e. by unit
          inc(beg, length(lastunituppercase))
        else
        begin
          // manual unit name search in fBlock[]
          lastunituppercase := '';
          for u := 0 to fBlocksCount - 1 do
            with fBlock[u].Symbol do
            begin
              l := length(Name);
              if (beg[l] = '.') and
                 (l > length(lastunituppercase)) and
                 IdemPropNameU(Name, beg, l) then
                lastunituppercase := UpperCase(Name); // find longest match
            end;
          if lastunituppercase <> '' then
          begin
            l := length(lastunituppercase);
            SetLength(lastunituppercase, l + 1);
            lastunituppercase[l] := '.';
            inc(beg, l + 1);
          end;
        end;
        {$endif ISDELPHI2005ANDUP}
        FastSetString(sym.Name, beg, p);
        if (sym.Name <> '') and
           not (sym.Name[1] in ['$', '?']) then
          fSymbols.Add(sym);
      end;
      NextLine;
    end;
    sections := fBlock;
    SetLength(sections, fBlocksCount);
    fBlocks.Clear; // ReadLines will repopulate Blocks[] with code blocks :)
  end;

  procedure ReadLines;
  var
    beg, idbeg, idend: PAnsiChar;
    n, capa: PtrInt;
    b: PDebugBlock;
  begin
    idbeg := pointer(p);
    while p^ <> '(' do
      if p = pend then
        exit
      else
        inc(p);
    idend := pointer(p);
    if idend = idbeg then
      exit;
    inc(p);
    beg := pointer(p);
    while p^ <> ')' do
      if p = pend then
        exit
      else
        inc(p);
    if not IdemPChar(p, ') SEGMENT .TEXT') then
      exit;
    b := fBlocks.NewPtr; // each nested .inc/.pas triggers a new Blocks[] record
    FastSetString(b^.Symbol.Name, idbeg, idend); // unit name
    FastSetString(b^.FileName, beg, p); // may be nested .inc
    NextLine;
    NextLine;
    capa := 0;
    n := 0;
    while (p + 10 < pend) and
          (p^ >= ' ') do
    begin
      while (p < pend) and
            (p^ = ' ') do
        inc(p);
      repeat
        if n = capa then
        begin
          capa := NextGrow(capa);
          SetLength(b^.Line, capa);
          SetLength(b^.Addr, capa);
        end;
        b^.Line[n] := GetNextItemCardinal(p, ' ');
        if not GetCode(b^.Addr[n]) then
          break;
        if b^.Addr[n] <> 0 then
          inc(n); // occurred with Delphi 2010 :(
      until (p >= pend) or
            (p^ < ' ');
      NextLine;
    end;
    if n > 0 then
      b^.Symbol.Start := b^.Addr[0];
    SetLength(b^.Line, n);
    SetLength(b^.Addr, n);
  end;

var
  i, j, l: PtrInt;
  mapage: TUnixTime;
  mapfile: TFileName;
  mapcontent: RawUtf8;
begin
  mapfile := ChangeFileExt(fExeFile, '.map'); // information is in .map
  mapage := FileAgeToUnixTimeUtc(mapfile);
  if (mapage = 0) or
     (abs(mapage - fExeAge) > SecsPerMin) then // deprecated .map
    exit;
  mapcontent := StringFromFile(mapfile);
  p := pointer(mapcontent);
  l := length(mapcontent);
  if (p = nil) or
     (StrLen(p) <> l) then
    exit; // this is no .map file for sure
  pend := p + l;
  // parse .map sections into Symbols[] and Blocks[]
  fSymbols.Capacity := 8000;
  while p < pend do
    if MatchPattern(p, pend, 'DETAILED MAP OF SEGMENTS', p) then
      ReadSegments
    else if MatchPattern(p, pend, 'ADDRESS PUBLICS BY VALUE', p) then
      ReadSymbols
    else if MatchPattern(p, pend, 'LINE NUMBERS FOR', p) then
      ReadLines
    else
      NextLine;
  // now we should have read all .map/.dbg content
  if fBlocksCount or fSymbolsCount = 0 then
    exit;
  fDebugInfo := diExternalMap;
  for i := fBlocksCount - 1 downto 0 do
    with fBlock[i] do
      if (Symbol.Start = 0) and
         (Symbol.Stop = 0) then
        fBlocks.Delete(i); // occurs with Delphi 2010 :(
  for i := 0 to fBlocksCount - 1 do
    with fBlock[i] do
      if Symbol.Stop = 0 then
      begin
        if i < fBlocksCount - 1 then
          Symbol.Stop := fBlock[i + 1].Symbol.Start - 1;
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

procedure ReadSymbol(var P: PByte; var A: TDynArray; var tmp: RawByteString);
var
  i, n, L: PtrInt;
  s: PDebugSymbol;
  prev: cardinal;
  sr: PStrRec;
begin
  A.Clear;
  n := FromVarUInt32(P);
  if n = 0 then
    exit;
  A.Capacity := n; // allocate TDebugSymbolDynArray/TDebugBlockDynArray
  A.Count := n;
  s := A.Value^;
  prev := 0;
  for i := 1 to n do
  begin
    inc(prev, FromVarUInt32(P));
    s^.Start := prev;
    inc(prev, FromVarUInt32(P));
    s^.Stop := prev;
    inc(PByte(s), A.Info.Cache.ItemSize); // may be TDebugSymbol or TDebugBlock
  end;
  s := A.Value^;
  if PInteger(P)^ = -1 then // new encoding with namesize prefix
  begin
    inc(PInteger(P)); // skip marker
    sr := StrRecAlloc(tmp, n, FromVarUInt32(P)); // allocate names at once
    for i := 1 to n do
    begin
      FromVarStrRec(P, sr, s^.Name); // inlined R.Read(s^.Name) over tmp
      inc(PByte(s), A.Info.Cache.ItemSize);
    end;
  end
  else // backward compatibility for existing .mab content
    for i := 1 to n do
    begin
      L := FromVarUInt32(P);
      FastSetString(s^.Name, P, L);
      inc(P, L);
      inc(PByte(s), A.Info.Cache.ItemSize);
    end;
end;

function TDebugFile.LoadMab(const aMabFile: TFileName): boolean;
var
  R: TFastReader;
  i: PtrInt;
  MS: TMemoryStream;
  b: PDebugBlock;
begin
  result := false;
  try
    // StreamUnCompress() will try from the end if aMabFile is an executable
    MS := AlgoSynLZ.StreamUnCompress(aMabFile, MAGIC_MAB, {hash32=}true);
    if MS <> nil then
    try
      fLinesCount := 0;
      R.Init(MS.Memory, MS.Size);
      ReadSymbol(PByte(R.P), fSymbols, fSymbolsTemp);
      ReadSymbol(PByte(R.P), fBlocks, fBlocksTemp);
      b := pointer(fBlock);
      for i := 1 to fBlocksCount do
      begin
        R.VarUtf8(b^.FileName);
        inc(b);
      end;
      b := pointer(fBlock);
      for i := 1 to fBlocksCount do
      begin
        R.ReadVarUInt32Array(b^.Line);
        R.ReadVarUInt32Array(b^.Addr);
        inc(fLinesCount, length(b^.Line));
        inc(b);
      end;
      if not R.EOF then
        R.VarUtf8(fProducer);
      fDebugInfo := diExternalMab;
      result := true;
    finally
      MS.Free;
    end;
  except
    on Exception do
      ; // invalid file -> ignore any problem
  end;
end;

function FinalizeSymbolStop(b: PDebugBlocks; n: integer): PtrInt;
begin
  result := 0; // returns fLinesCount computed value
  if n = 0 then
    exit;
  while n > 1 do // finalize fBlock[].Symbol.Stop missing fields
  begin
    inc(result, length(b^[0].Line));
    if b^[0].Symbol.Stop = 0 then
      b^[0].Symbol.Stop := b^[1].Symbol.Start - 1;
    b := @b^[1];
    dec(n);
  end;
  // fBlock[fBlocksCount - 1] specific fix
  inc(result, length(b^[0].Line));
  if b^[0].Symbol.Stop = 0 then
    if b^[0].Addr <> nil then
      // Blocks[] may overlap with .inc -> use Addr[]
      b^[0].Symbol.Stop := b^[0].Addr[high(b^[0].Addr)]
    else
      b^[0].Symbol.Stop := b^[0].Symbol.Start;
end;

constructor TDebugFile.Create(const aExeName: TFileName; Scope: TDebugFileScope);
var
  savemab: boolean;
  mabage: TUnixTime;
  start: Int64;
begin
  QueryPerformanceMicroSeconds(start);
  inherited Create; // may have been overriden
  fSymbols.InitSpecific(TypeInfo(TDebugSymbolDynArray), fSymbol, ptRawUtf8,
    @fSymbolsCount, true);
  fBlocks.InitSpecific(TypeInfo(TDebugBlockDynArray), fBlock, ptRawUtf8,
    @fBlocksCount, true);
  if SynLogFileFreeing then // avoid GPF
    exit;
  // check the supplied aExeName
  fExeFile := ExpandFileName(aExeName);
  fExeAge := FileAgeToUnixTimeUtc(fExeFile);
  if fExeAge = 0 then
    exit;
  fExePath := ExtractFilePath(fExeFile);
  savemab := false;
  fMabFile := ChangeFileExt(fExeFile, '.mab');
  // search for a .mab file matching the running .exe/.dll name
  mabage := FileAgeToUnixTimeUtc(fMabFile);
  if mabage = 0 then
  begin
    if not IsDirectoryWritable(fExePath) then
    begin
      // read/only exe folder -> store .mab in local non roaming user folder
      // ([idwExcludeWinSys] not needed because admin could do it once for all)
      fMabFile := MakeString([
                    GetSystemPath(spUserData),
                    crc32cStringToHexShort(fExePath), '-', // unique per-path
                    ExtractFileName(fMabfile)]);
      mabage := FileAgeToUnixTimeUtc(fMabFile);
    end;
  end;
  if (mabage <> 0) and // SaveToFile() set FileSetDateFrom(fExeFile);
     (abs(fExeAge - mabage) < 2) and // same exact age (allow 1 second diff)
     not (dfsNoMabExternalCheck in Scope) then
  begin
    LoadMab(fMabFile);
    if fBlocksCount or fSymbolsCount = 0 then
      DeleteFile(fMabFile);
  end;
  // recompute from .map/.dbg if no faster-to-load .mab available
  if fBlocksCount or fSymbolsCount = 0 then
  try
    GenerateFromMapOrDwarf(dfsIncludePathInFileName in Scope);
    if fBlocksCount or fSymbolsCount <> 0 then
    begin
      fSymbols.Capacity := fSymbolsCount; // only consume the needed memory
      fBlocks.Capacity := fBlocksCount;
      fLinesCount := FinalizeSymbolStop(pointer(fBlock), fBlocksCount);
      savemab := true; // trigger SaveToFile(MabFile) below
    end;
  except
    fSymbols.Clear;
    fBlocks.Clear;
  end;
  // search for an embedded compressed .mab file appended to the .exe/.dll
  if (fBlocksCount or fSymbolsCount = 0) and
     not (dfsNoMabInternalCheck in Scope) then
    if LoadMab(fExeFile) then
      fDebugInfo := diInternalMab;
  // finalize this instance
  if fBlocksCount <> 0 then
  begin
    fStart := fBlock[0].Symbol.Start;
    fStop := fBlock[fBlocksCount - 1].Symbol.Stop;
  end;
  if fSymbolsCount <> 0 then
  begin
    fStart := MinPtrInt(fStart, fSymbol[0].Start);
    fStop  := MaxPtrInt(fStop, fSymbol[fSymbolsCount - 1].Stop);
    if (fProducer = '') and
       (fExeFile = Executable.InstanceFileName) then
      fProducer := COMPILER_VERSION; // we know it for this compiled instance
  end;
  QueryPerformanceMicroSeconds(fLoadingMicroSec);
  dec(fLoadingMicroSec, start);
  // optionally persist as .mab after GenerateFromMapOrDwarf()
  if savemab and
     not (dfsNoMabSaveAtCreate in Scope) then
    SaveToFile(fMabFile, Scope);
end;

destructor TDebugFile.Destroy;
begin
  fSymbols.Clear; // ensure are released BEFORE fSymbolsTemp and fBlocksTemp
  fBlocks.Clear;
  inherited Destroy;
end;

function TDebugFile.GetExeDate: RawUtf8;
begin
  DateTimeToIso8601TextVar(UnixTimeToLocal(fExeAge), ' ', result);
end;

procedure WriteSymbol(var W: TBufferWriter; const A: TDynArray);
var
  i, n, namesize: integer;
  prev: TDebugAddress;
  s: PDebugSymbol;
  p, beg: PByte;
  tmp: RawByteString;
begin
  n := A.Count;
  if n = 0 then
  begin
    W.Write1(0);
    exit;
  end;
  W.WriteVarUInt32(n);
  p := pointer(W.DirectWritePrepare(n * 10, tmp));
  beg := p;
  prev := 0;
  namesize := 0;
  s := A.Value^;
  for i := 1 to n do
  begin
    inc(namesize, length(s^.Name));
    p := ToVarUInt32(s^.Start - prev, p);
    p := ToVarUInt32(s^.Stop - s^.Start, p);
    prev := s^.Stop;
    inc(PByte(s), A.Info.Cache.ItemSize); // may be TDebugSymbol or TDebugBlock
  end;
  W.DirectWriteFlush(PtrUInt(p) - PtrUInt(beg), tmp);
  W.Write4(-1); // marker for new format with namesize prefix
  W.WriteVarUInt32(namesize);
  s := A.Value^;
  repeat
    W.Write(s^.Name); // group for better compression
    inc(PByte(s), A.Info.Cache.ItemSize);
    dec(n);
  until n = 0;
end;

procedure TDebugFile.SaveToStream(aStream: TStream; Scope: TDebugFileScope);
var
  W: TBufferWriter;
  i: integer;
  MS: TMemoryStream;
  b: PDebugBlock;
begin
  MS := TMemoryStream.Create;
  try
    W := TBufferWriter.Create(MS, 1 shl 20); // 1 MB should be enough at first
    try
      if dfsNoSymbols in Scope then
        W.Write1(0)
      else
        WriteSymbol(W, fSymbols);
      if dfsNoLines in Scope then
        W.Write1(0)
      else
      begin
        WriteSymbol(W, fBlocks);
        b := pointer(fBlock);
        for i := 1 to fBlocksCount do
        begin
          W.Write(b^.FileName); // group for better compression
          inc(b);
        end;
        b := pointer(fBlock);
        for i := 1 to fBlocksCount do
        begin
          // Line values are not always increasing -> wkOffsetI
          W.WriteVarUInt32Array(b^.Line, length(b^.Line), wkOffsetI);
          // Addr are sorted, so always increasing -> wkOffsetU
          W.WriteVarUInt32Array(b^.Addr, length(b^.Addr), wkOffsetU);
          inc(b);
        end;
      end;
      if (fProducer <> '') and
         not (dfsNoProducer in Scope) then
        W.Write(fProducer);
      W.Flush; // now MS contains the uncompressed binary data
    finally
      W.Free;
    end;
    AlgoSynLZ.StreamCompress(MS, aStream, MAGIC_MAB, {hash32=}true, {trailer=}true);
  finally
    MS.Free;
  end;
end;

const
  _TDebugSymbol: RawUtf8 = 'name:RawUtf8 start,stop:integer';
  _TDebugBlock: RawUtf8 = 'symbol:TDebugSymbol filename:RawUtf8 line,addr:TIntegerDynArray';

procedure TDebugFile.SaveToJson(W: TTextWriter);
begin
  if Rtti.RegisterType(TypeInfo(TDebugSymbol)).Props.Count = 0 then
    Rtti.RegisterFromText([TypeInfo(TDebugSymbol), _TDebugSymbol,
                           TypeInfo(TDebugBlock),  _TDebugBlock]);
  W.AddShort('{"symbols":');
  fSymbols.SaveToJson(W, []);
  W.AddShort(',"blocks":');
  fBlocks.SaveToJson(W, []);
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

function TDebugFile.SaveToFile(const aFileName: TFileName; Scope: TDebugFileScope): TFileName;
var
  F: TStream;
begin
  if aFileName = '' then
    result := ChangeFileExt(Executable.InstanceFileName, '.mab')
  else
    result := aFileName;
  DeleteFile(result);
  F := TFileStreamEx.Create(result, fmCreate);
  try
    SaveToStream(F, Scope);
  finally
    F.Free;
  end;
  if fExeFile <> '' then
    FileSetDateFrom(aFileName, fExeFile);
end;

procedure TDebugFile.SaveToExe(const aExeName: TFileName; Scope: TDebugFileScope);
var
  exe, mab: TMemoryStream;
  exesize, mabsize: PtrUInt;
begin
  if not FileExists(aExeName) then
    exit;
  mab := TMemoryStream.Create;
  try
    // generate the .mab content in memory
    SaveToStream(mab, Scope);
    mabsize := mab.Size;
    // open the executable file in memory, trim any existing mab, append new mab
    exe := TMemoryStream.Create;
    try
      exe.LoadFromFile(aExeName);
      exesize := exe.Size;
      if exesize < 16 then
        exit;
      exesize := AlgoSynLZ.StreamComputeLen(exe.Memory, exesize, MAGIC_MAB);
      exe.Size := exesize + mabsize; // trim and reserve space for .mab
      MoveFast(mab.Memory^, PAnsiChar(exe.Memory)[exesize], mabsize); // append
      exe.SaveToFile(aExeName); // save
    finally
      exe.Free;
    end;
  finally
    mab.Free;
  end;
end;

function TDebugFile.FindSymbol(rva: TDebugAddress): PDebugSymbol;
var
  i, L, R: PtrInt;
begin
  L := 0;
  R := fSymbolsCount - 1;
  if (R >= 0) and
     (rva > 0) then
    repeat // efficient O(log(n)) binary search
      i := (L + R) shr 1;
      result := @fSymbol[i];
      if rva < result^.Start then
        R := i - 1
      else if rva > result^.Stop then
        L := i + 1
      else
        exit; // found
    until L > R;
  result := nil; // not found
end;

function TDebugFile.FindBlock(rva: TDebugAddress): PDebugBlock;
var
  i, L, R: PtrInt;
begin
  L := 0;
  R := fBlocksCount - 1;
  if (R >= 0) and
     (rva > 0) then
    repeat // efficient O(log(n)) binary search
      i := (L + R) shr 1;
      result := @fBlock[i];
      if rva < result^.Symbol.Start then
        R := i - 1
      else if rva > result^.Symbol.Stop then
        L := i + 1
      else
        exit; // found
    until L > R;
  result := nil; // not found
end;

function TDebugFile.FindBlock(rva: TDebugAddress; out line: integer): PDebugBlock;
var
  i, L, R, max: PtrInt;
  a: PIntegerArray;
begin
  line := 0;
  result := FindBlock(rva);
  if result = nil then
    exit;
  // unit found -> search line number from within matching Addr[]
  if result^.Addr = nil then
    exit;
  max := PDALen(PAnsiChar(result^.Addr) - _DALEN)^ + (_DAOFF- 1);
  L := 0;
  R := max;
  if R >= 0 then
    repeat // efficient O(log(i)) binary search
      i := (L + R) shr 1;
      a := @result^.Addr[i];
      if rva < a^[0] then
        R := i - 1
      else if (i < max) and
              (rva >= a^[1]) then
        L := i + 1
      else
      begin
        line := result^.Line[i]; // found
        exit;
      end;
    until L > R;
end;

function TDebugFile.AppendLog(W: TTextWriter; aPointer: PtrUInt; NoHex: boolean): boolean;
var
  rva: TDebugAddress;
  line: integer; // not PtrInt
  s: PDebugSymbol;
  l: PDebugBlock;
begin
  result := false;
  rva := AbsoluteToRelative(aPointer);
  if rva = 0 then
    exit;
  s := FindSymbol(rva);
  {$ifdef ISDELPHI}
  if (s <> nil) and
     (FindPropName(['SynRtlUnwind', '@HandleAnyException',  'LogExcept',
       '@HandleOnException', 'ThreadWrapper', 'ThreadProc'],
       s^.Name) >= 0) then
    // no stack trace within the Delphi exception interception functions
    exit;
  {$endif ISDELPHI}
  result := true;
  if not NoHex then
  begin
    W.AddPointer(aPointer);
    W.AddDirect(' ');
  end;
  l := FindBlock(rva, line);
  if l <> nil then
  begin
    if line = 0 then
      W.AddString(l^.Symbol.Name) // main unit name for convenience
    else
      W.AddString(l^.FileName);   // line number is always against a file
    W.AddDirect(' ');
  end;
  if s <> nil then
    W.AddString(s^.Name);
  W.AddDirect(' ');
  if line = 0 then
    exit;
  W.AddDirect('(');
  W.AddU(line);
  W.AddDirect(')', ' '); // always end with a ' '
end;

class function TDebugFile.AddLog(W: TTextWriter; aPointer: PtrUInt; NoHex: boolean): boolean;
var
  debug: TDebugFile;
begin
  result := false;
  if (W = nil) or
     (aPointer = 0) then
    exit;
  debug := TDebugFile.Get(pointer(aPointer));
  if debug <> nil then
    result := debug.AppendLog(W, aPointer, NoHex);
end;

procedure TDebugFile.AppendLocationShort(aPointer: PtrUInt; var aInfo: ShortString);
var
  line: integer; // not PtrInt
  rva: TDebugAddress;
  s: PDebugSymbol;
  l: PDebugBlock;
  c: PUtf8Char;
begin
  if (self = nil) or
     (fDebugInfo = diNone) then
    exit;
  rva := AbsoluteToRelative(aPointer);
  if rva = 0 then
    exit;
  s := FindSymbol(rva);
  l := FindBlock(rva, line);
  if (s = nil) and
     (l = nil) then
     exit;
  AppendShortChar(' ', @aInfo);
  if l <> nil then
  begin
    AppendShortAnsi7String(l^.FileName, aInfo);
    c := PUtf8Char(pointer(l^.FileName)) + length(l^.Symbol.Name);
    if not StartWithLower(l^.FileName, l^.Symbol.Name) or
       (c^ <> '.') or
       (PosChar(c + 1, '.') <> nil) then
    begin
      // e.g. 'a0a40 mormot.core.base.asmx64.inc (mormot.core.base) Rdtsc (3005)'
      AppendShort(' (', aInfo);
      AppendShortAnsi7String(l^.Symbol.Name, aInfo);
      AppendShortCharSafe(')', aInfo);
    end;
    AppendShortCharSafe(' ', aInfo);
  end;
  if s <> nil then
    AppendShortAnsi7String(s^.Name, aInfo);
  if line > 0 then
  begin
    AppendShortTwoCharsSafe(ord(' ') + ord('(') shl 8, aInfo);
    AppendShortCardinal(line, aInfo);
    AppendShortCharSafe(')', aInfo);
  end;
end;

class function TDebugFile.FindLocation(aPointer: pointer): RawUtf8;
var
  tmp: ShortString;
begin
  FindLocationShort(aPointer, tmp);
  ShortStringToAnsi7String(tmp, result);
end;

class procedure TDebugFile.FindLocationShort(aPointer: pointer;
  var aInfo: ShortString);
var
  deb: TDebugFile;
  tmp: pointer; // RawUtf8
begin
  aInfo := PointerToHexShort(aPointer);
  tmp := nil;
  deb := DebugFileGet(PtrUInt(aPointer), @tmp);
  if deb <> nil then
    deb.AppendLocationShort(PtrUInt(aPointer), aInfo)
  else if tmp <> nil then
  begin
    AppendShortAnsi7String(RawUtf8(tmp), aInfo);
    FastAssignNew(tmp);
  end;
end;

class function TDebugFile.FindLocationRaisedAt(exc: ESynException): RawUtf8;
begin
  if (exc = nil) or
     (exc.RaisedAt = nil) then
    FastAssignNew(result)
  else
    result := FindLocation(exc.RaisedAt);
end;

function _GetExecutableLocation(aAddress: pointer): ShortString;
begin
  TDebugFile.FindLocationShort(aAddress, result);
end;

function TDebugFile.FindBlockByName(const aUnitName: RawUtf8): PDebugBlock;
var
  i: integer;
begin
  if (self <> nil) and
     (aUnitName <> '') then
  begin
    result := pointer(fBlock);
    for i := 1 to fBlocksCount do
      if IdemPropNameU(result^.Symbol.Name, aUnitName) then // inlined
        exit // return the first occurence skipping any next nested inclusion
      else
        inc(result);
  end;
  result := nil;
end;

class function TDebugFile.FindFileName(const unitname: RawUtf8): TFileName;
var
  name: RawUtf8;
  l: PDebugBlock;
begin
  result := '';
  if unitname = '' then
    name := Executable.ProgramName
  else
    name := unitname;
  l := TDebugFile.CurrentDebugFile.FindBlockByName(name);
  if l <> nil then
    Utf8ToFileName(l^.FileName, result);
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
  if _LogInfoCaption[high(_LogInfoCaption)] = '' then // delayed translation
    GetEnumCaptions(TypeInfo(TSynLogLevel), @_LogInfoCaption);
  result := _LogInfoCaption[event];
end;

function ToCaption(filter: TSynLogFilter): string;
begin
  result := GetCaptionFromEnum(TypeInfo(TSynLogFilter), Ord(filter))
end;

function ToText(const Event: TMethod): RawUtf8;
var
  tmp: ShortString;
begin
  TDebugFile.FindLocationShort(Event.Code, tmp);
  FormatUtf8('% using %(%)', [tmp, TObject(Event.Data), Event.Data], result);
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
  {$ifdef ASMX64}
  // detect and include mormot.core.fpcx64mm raw information
  with GetHeapStatus do
    if PShortString(@TotalAddrSpace)^ = 'fpcx64mm' then // magic marker
    try
      result := StringReplaceAll(THeapInfo(PPointer(@Unused)^)(), '  ', ' ');
      exit;
    except
    end;
  {$endif ASMX64}
  // standard FPC memory manager
  with GetFPCHeapStatus do
    FormatUtf8(' - Heap: Current: used=% size=% free=%   Max: size=% used=%',
      [KBNoSpace(CurrHeapUsed), KBNoSpace(CurrHeapSize), KBNoSpace(CurrHeapFree),
       KBNoSpace(MaxHeapSize),  KBNoSpace(MaxHeapUsed)], result);
end;
{$else}
function RetrieveMemoryManagerInfo: RawUtf8;
{$if defined(OSWINDOWS) and defined(ISDELPHI2007ANDUP)}
var
  i: PtrInt;
  small, alloc, reserved, blocks: QWord;
  state: TMemoryManagerState;
{$ifend}
begin
  result := '';
  {$ifdef OSWINDOWS}
  {$ifdef ISDELPHI2007ANDUP}
  // new FastMM4 Delphi2007+ function - GetHeapStatus() is deprecated
  GetMemoryManagerState(state);
  small := 0;
  blocks := QWord(state.AllocatedMediumBlockCount) +
            QWord(state.AllocatedLargeBlockCount);
  reserved := QWord(state.ReservedMediumBlockAddressSpace) +
              QWord(state.ReservedLargeBlockAddressSpace);
  for i := 0 to high(state.SmallBlockTypeStates) do
    with state.SmallBlockTypeStates[i] do
    begin
      inc(small, QWord(AllocatedBlockCount) * UseableBlockSize);
      inc(reserved, ReservedAddressSpace);
      inc(blocks, AllocatedBlockCount);
    end;
  alloc := small + QWord(state.TotalAllocatedMediumBlockSize) +
                   QWord(state.TotalAllocatedLargeBlockSize);
  if reserved <> 0 then
    FormatUtf8(' - Heap: Allocated=% Reserved=% ' +
       'Small=% Medium=% Large=% Blocks=% ',
      [KBNoSpace(alloc), KBNoSpace(reserved), KBNoSpace(small),
       KBNoSpace(state.TotalAllocatedMediumBlockSize),
       KBNoSpace(state.TotalAllocatedLargeBlockSize), blocks], result);
  {$else}
  // Delphi 7+: use old GetHeapStatus
  with GetHeapStatus do
    if TotalAddrSpace <> 0 then
      FormatUtf8(' - Heap: AddrSpace=% Uncommitted=% Committed=% Allocated=% '+
         'Free=% FreeSmall=% FreeBig=% Unused=% Overheap=% ',
        [KBNoSpace(TotalAddrSpace), KBNoSpace(TotalUncommitted),
         KBNoSpace(TotalCommitted), KBNoSpace(TotalAllocated),
         KBNoSpace(TotalFree),      KBNoSpace(FreeSmall),
         KBNoSpace(FreeBig),        KBNoSpace(Unused),
         KBNoSpace(Overhead)], result);
  {$endif ISDELPHI2007ANDUP}
  {$endif OSWINDOWS}
end;
{$endif FPC}


var
  /// internal list of registered TSynLogFamily instances
  // - up to MAX_SYNLOGFAMILY TSynLog sub-classes may be defined
  // - protected by SynLogGlobalLock
  SynLogFamily: array of TSynLogFamily;

  /// internal list of created TSynLog instances, one per each log file on disk
  // - also used by AutoFlushProc() to get a global list of TSynLog instances
  // - protected by SynLogGlobalLock
  SynLogFile: TSynLogDynArray;


type
  // RRD of last 128 lines to be sent to console (no need of older data)
  TAutoFlushThreadToConsole = record
    Next, Count: integer;
    Text:  array[0..127] of RawUtf8; // must be a power-of-two length
    Color: array[0..127] of TConsoleColor;
  end;

  // cross-platform / cross-compiler TThread-based flush disk or console
  TAutoFlushThread = class(TThread) { no TThreadAbstract dependency }
  protected
    fToConsoleSafe: TLightLock; // topmost to ensure aarch64 alignment
    fEvent: TSynEvent;
    fToCompress: TFileName;
    fToConsole: TAutoFlushThreadToConsole; // Family.EchoToConsoleBackground
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
  TextColor(ccDefault); // eventually reset console text color
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
      SynLogGlobalLock.Lock;
      try
        if Terminated or
           SynLogFileFreeing then
          break;
        files := copy(SynLogFile); // don't slow down main logging process
      finally
        SynLogGlobalLock.UnLock;
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
  // Terminated is set: eventually display delayed console output
  try
    FlushConsole;
  except
    ; // ignore any exception at shutdown
  end;
  TSynLog.NotifyThreadEnded; // as in mormot.core.thread TThreadAbstract
end;

threadvar // do not publish for compilation within Delphi packages
  PerThreadInfo: TSynLogThreadInfo;

type
  // on Win64, RtlCaptureStackBackTrace() API is limited to < 62 frames
  TRawStackFrames = array[0..61] of PtrUInt;

{$STACKFRAMES ON} // we need a stack frame for the backtrace API calls below

{$ifndef FPC}
{$ifdef OSWINDOWS}
{$ifndef CPU64}

function CheckAsmX86(xret: PtrUInt): boolean; // naive x86 caller detection
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

// heuristic ebp-chain walk into frames[], returning the frames count
// - on Delphi Win32, RtlCaptureStackBackTrace() requires stack frames and
// is likely to return nothing, so the manual scan of TSynLog stOnlyManual
// mode is needed - note: skip levels do not apply to such a heuristic scan
function ManualStackTrace(var frames: TRawStackFrames): PtrInt;
var
  st, max_stack, min_stack, buf0, buf1: PtrUInt;
  stack: PPtrUInt;
begin
  result := 0;
  asm
      mov     min_stack, ebp
      mov     eax, fs:[4]
      mov     max_stack, eax
  end;
  buf0 := PtrUInt(@frames); // frames[] is likely on stack in this range:
  buf1 := buf0 + SizeOf(frames); // never scan our own output buffer
  stack := pointer(min_stack);
  try
    while (PtrUInt(stack) < max_stack) and
          (result < length(frames)) do
    begin
      if (PtrUInt(stack) >= buf0) and
         (PtrUInt(stack) < buf1) then
      begin
        stack := pointer(buf1); // jump over frames[] we are filling
        continue;
      end;
      st := stack^;
      inc(stack);
      if (st >= min_stack) and
         (st <= max_stack) then
        continue; // on-stack pointer is no code
      if SeemsRealPointer(pointer(st - 8)) and
         CheckAsmX86(st) then
      begin
        frames[result] := st;
        inc(result);
      end;
    end;
  except
    // just ignore any access violation here
  end;
end;

{$endif CPU64}
{$endif OSWINDOWS}
{$endif FPC}

// capture the current thread stack into frames[], returning the frames count
// - first frame is the caller of this function, plus optional skip levels
// - use follows TSynLogFamily.StackTraceUse semantics (ignored on FPC)
function RawStackTrace(skip: PtrInt; use: TSynLogStackTraceUse;
  var frames: TRawStackFrames): PtrInt;
begin
  {$ifdef FPC}
  result := CaptureBacktrace(skip + 1, length(frames), pointer(@frames));
  {$else}
  result := 0;
  {$ifdef OSWINDOWS}
  if use <> stOnlyManual then
    result := RtlCaptureStackBackTrace(skip + 1, length(frames), @frames, nil);
  {$ifndef CPU64}
  if (result < 2) and
     (use <> stOnlyAPI) then
    // support stOnlyManual/stManualAndAPI on Delphi Win32, where the API
    // needs stack frames and is likely to return (almost) nothing
    result := ManualStackTrace(frames);
  {$endif CPU64}
  {$endif OSWINDOWS}
  {$endif FPC}
end;

class function TDebugFile.StackTrace(skip, depth: integer;
  use: TSynLogStackTraceUse): RawUtf8;
var
  temp: TTextWriterStackBuffer;
  w: TTextWriter;
begin
  FastAssignNew(result);
  w := TTextWriter.CreateOwnedStream(temp);
  try
    StackTrace(w, skip + 1, depth, use); // + 1 to ignore this very method
    w.CancelLastChar(' ');
    w.SetText(result);
  finally
    w.Free;
  end;
end;

class procedure TDebugFile.StackTrace(W: TTextWriter; skip, depth: integer;
  use: TSynLogStackTraceUse);
var
  frames: TRawStackFrames;
  i, n: PtrInt;
  {$ifndef FPC}
  {$ifndef NOEXCEPTIONINTERCEPT}
  bak: TSynLogThreadInfoFlags; // paranoid precaution, as TSynLog.AddStackTrace
  threadflags: ^TSynLogThreadInfoFlags;
  {$endif NOEXCEPTIONINTERCEPT}
  {$endif FPC}
begin
  if W = nil then
    exit;
  if depth <= 0 then
    depth := 30; // as default TSynLogFamily.StackTraceLevel
  if skip < 0 then
    skip := 0;
  {$ifndef FPC}
  {$ifndef NOEXCEPTIONINTERCEPT}
  // the manual stack walk makes speculative reads: intercepted exceptions
  // should not reach the logs during the process
  threadflags := @PerThreadInfo.Flags;
  bak := threadflags^;
  include(threadflags^, tiExceptionIgnore);
  {$endif NOEXCEPTIONINTERCEPT}
  {$endif FPC}
  try
    n := RawStackTrace(skip + 1, use, frames); // + 1 to ignore this very method
    for i := 0 to n - 1 do
      if (i = 0) or
         (frames[i] <> frames[i - 1]) then
        if AddLog(W, frames[i]) then
        begin
          dec(depth);
          if depth = 0 then
            break;
        end;
  except // don't let any unexpected GPF break the caller
  end;
  {$ifndef FPC}
  {$ifndef NOEXCEPTIONINTERCEPT}
  threadflags^ := bak;
  {$endif NOEXCEPTIONINTERCEPT}
  {$endif FPC}
end;

{$STACKFRAMES OFF} // back to {$W-} normal state, as in mormot.defines.inc

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
  // reset TSynLogThreadInfo flags
  nfo^.RecursionCount := 0;
  byte(nfo^.Flags) := 0;
  nfo^.ThreadNumber := num;
  // pre-compute GetBitPtr() constants for SetThreadInfoAndThreadName()
  dec(num);
  nfo^.ThreadBitLo := 1 shl (num and 31); // 32-bit fThreadNameLogged[] mask
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
  fIdent := PtrArrayAdd(SynLogFamily, self); // index of this TSynLogClass
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
  SynLogGlobalLock.Lock;
  try
    result := fSynLogClass.Create(self);
    PtrArrayAdd(SynLogFile, result);
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
    SynLogGlobalLock.UnLock;
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
  tmp: TShort7;
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
  SynLogGlobalLock.Lock;
  try
    for i := 0 to high(SynLogFile) do
      with SynLogFile[i] do
        if fFamily = self then
          if aEventAdd then
            fWriterEcho.EchoAdd(aEvent)
          else
            fWriterEcho.EchoRemove(aEvent);
  finally
    SynLogGlobalLock.UnLock;
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
  stream: TStream;
  log: TSynLog;
  endpos, start: Int64;
  c: AnsiChar;
  i, len, read, total: integer;
  P: PAnsiChar;
begin
  FastAssignNew(result);
  if (SynLogFile = nil) or
     SynLogFileFreeing then
    exit;
  SynLogGlobalLock.Lock;
  try
    for i := 0 to high(SynLogFile) do
    begin
      log := SynLogFile[i];
      if log.fFamily <> self then
        continue;
      log.Writer.FlushToStream;
      stream := log.Writer.Stream;
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
              FakeLength(result, total); // truncate on read error (paranoid)
            break;
          end;
          inc(P, read);
          dec(len, read);
          inc(total, read);
        until len = 0;
      finally
        stream.Position := endpos;
      end;
      break;
    end;
  finally
    SynLogGlobalLock.UnLock;
  end;
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
  if s^[0] <> #0 then    // avoid GPF if returned @NULCHAR
    s^[0] := #0;         // reset TShort31 threadvar for consistency
  nfo := @PerThreadInfo; // no automatic InitThreadNumber()
  num := nfo^.ThreadNumber;
  if num = 0 then        // not touched yet by TSynLog, or called twice
    exit;
  nfo^.ThreadNumber := 0; // mark once as recycled
  nfo^.ThreadBitLo := 0;  // force InitThreadNumber on next thread access
  // reset global thread information
  if SynLogFileFreeing then
    exit; // inconsistent call at shutdown
  SynLogGlobalLock.Lock;
  try
    thd := @SynLogThreads;
    thd^.Safe.Lock;
    try
      // reset this thread name for ptIdentifiedInOneFile
      if num <= length(thd^.Name) then
        FastAssignNew(thd^.Name[num - 1]);
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
  finally
    SynLogGlobalLock.UnLock;
  end;
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
begin // caller just made SynLogGlobalLock.Lock
  log.fThreadInfo := nfo;
  // quickly check if we need to rotate or write the "SetThreadName" line
  if pendingRotate in log.fPendingFlags then   // from OnFlushToStream
    log.PerformRotation(nfo);
  if not (logAddThreadName in log.fFlags) then 
    exit; // no sllInfo + ptIdentifiedInOneFile
  p := pointer(log.fThreadNameLogged); // threads bit-set of this TSynLog
  if p <> nil then
  begin
    ndx := nfo^.ThreadBitHi; // use pre-computed runtime constants (favor FPC)
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
    FillInfo(nfo, nil); // syscall outside of SynLogGlobalLock
    SynLogGlobalLock.Lock;
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
  SynLogGlobalLock.Lock;
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
    SynLogGlobalLock.UnLock;
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
  SynLogGlobalLock.Lock;
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
    SynLogGlobalLock.UnLock;
  end;
end;

procedure TSynLog.Release;
begin
  SynLogGlobalLock.Lock;
  try
    CloseLogFile;
    ObjArrayDelete(SynLogFile, self);
    if fFamily.fPerThreadLog = ptOneFilePerThread then
      PerThreadInfo.FileLookup[fFamily.fIdent] := nil;
  finally
    SynLogGlobalLock.UnLock;
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
  SynLogGlobalLock.Lock;
  try
    if fWriter = nil then
      exit;
    fWriter.FlushToStream;
    if ForceDiskWrite and
       fWriterStream.InheritsFrom(THandleStream) then
      diskflush := THandleStream(fWriterStream).Handle;
  finally
    SynLogGlobalLock.UnLock;
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
  SynLogGlobalLock.Lock;
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
      TDebugFile.AddLog(fWriter, addr, {nohex=}true)
    {$endif ISDELPHI};
    fWriterEcho.AddEndOfLine(sllEnter);
  {$ifdef HASFASTTRYFINALLY}
  finally
  {$endif HASFASTTRYFINALLY}
    SynLogGlobalLock.UnLock;
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
    SynLogGlobalLock.UnLock;
  end;
end;

{$ifdef WINTELDELPHI} // specific to Delphi: fast get the caller method name

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

{$endif WINTELDELPHI}

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
  SynLogGlobalLock.UnLock;
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

function TSynLog.ConsoleEcho(Sender: TEchoWriter; Level: TSynLogLevel;
  const Text: RawUtf8): boolean;
begin
  result := true;
  if Level in fFamily.fEchoToConsole then
    if Family.EchoToConsoleUseJournal then
      JournalSend(Level, Text, {trimsynlog=}true)
    else if fFamily.EchoToConsoleBackground and
       Assigned(AutoFlushThread) then
      AutoFlushThread.AddToConsole(Text, LOG_CONSOLE_COLORS[Level])
    else
    begin
      ConsoleWrite(Text, LOG_CONSOLE_COLORS[Level]);
      TextColor(ccDefault);
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
  name := TrimU(StringReplaceCsv(name,
    'TSqlRest=,TRest=,TSql=,TSQLRest=,TSQL=,TOrmRest=,TOrm=,TWebSocket=WS,' +
    'TServiceFactory=SF,TSyn=,Thread=,Process=,Background=Bgd,WebSocket=WS,' +
    'Asynch=A,Async=A,Parallel=Prl,Timer=Tmr,Thread=Thd,Database=DB,Backup=Bak,' +
    'Server=Srv,Client=Cli,synopse=syn,memory=mem,  = '));
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
  FastAssignNew(result);
  if SynLogFileFreeing then
    exit;
  ndx := PerThreadInfo.ThreadNumber - 1; // no InitThreadNumber() call
  if ndx >= 0 then
  begin
    thd := @SynLogThreads;
    thd^.Safe.Lock;
    if ndx < length(thd^.Name) then
      result := thd^.Name[ndx]; // full thread name
    thd^.Safe.UnLock;
  end;
  if result = '' then // fallback to mormot.core.os default TShort31 value
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
  SynLogGlobalLock.Lock;
  try
    PerformRotation(nil);
  finally
    SynLogGlobalLock.UnLock;
  end;
end;

procedure TSynLog.DisableRemoteLog(entervalue: boolean);
begin
  if not Assigned(fFamily.fEchoRemoteEvent) then
    exit;
  if entervalue then
  begin
    SynLogGlobalLock.Lock;
    if pendingDisableRemoteLogLeave in fPendingFlags then
    begin
      SynLogGlobalLock.UnLock;
      ESynLogException.RaiseUtf8('Nested %.DisableRemoteLog', [self]);
    end;
    include(fPendingFlags, pendingDisableRemoteLogLeave);
  end
  else
  begin
    if not (pendingDisableRemoteLogLeave in fPendingFlags) then
      ESynLogException.RaiseUtf8('Missing %.DisableRemoteLog(true)', [self]);
    // DisableRemoteLog(false) -> add to events, and quit the global mutex
    exclude(fPendingFlags, pendingDisableRemoteLogLeave);
    fWriterEcho.EchoAdd(fFamily.fEchoRemoteEvent);
    SynLogGlobalLock.UnLock;
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
      TDebugFile.AddLog(fWriter, addr - 5, {nohex=}true);
    {$endif ISDELPHI}
    LogTrailer(Level);
  finally
    fThreadInfo^.Flags := fThreadInfoBackup;
    SynLogGlobalLock.UnLock;
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
    SynLogGlobalLock.UnLock;
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

procedure TSynLog.RawLog(Level: TSynLogLevel; const Event: TOnRawLog;
  Opaque: pointer; Value: PtrInt; Instance: TObject);
begin
  if (self = nil) or
     not (Level in fFamily.fLevel) or
     not Assigned(Event) then
    exit;
  if LockAndDisableExceptions then
  try
    LogHeader(Level, Instance);
    Event(self, Level, Opaque, Value, Instance);
    fWriterEcho.AddEndOfLine(Level); // LogTrailer(Level) is not needed here
  finally
    fThreadInfo^.Flags := fThreadInfoBackup;
    SynLogGlobalLock.UnLock;
  end;
end;

{$STACKFRAMES OFF} // back to {$W-} normal state, as in mormot.defines.inc

class procedure TSynLog.DebuggerNotify(Level: TSynLogLevel; const Text: RawUtf8);
begin
  if Text = '' then
    exit;
  Add.LogInternalText(Level, pointer(Text), length(Text), nil, 16384);
  if HasConsole then
    ConsoleWrite('%  ', [Text], LOG_CONSOLE_COLORS[Level], {noLF=}true);
  {$ifdef WINTELDELPHI}
  if IsDebuggerPresent then
    DebuggerBreak;
  {$endif WINTELDELPHI}
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
  SynLogGlobalLock.Lock;
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
    SynLogGlobalLock.UnLock;
  end;
end;

procedure TSynLog.LogFileHeader;
var
  w: TJsonWriter;
  i: PtrInt;
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
      w.AddU(CpuThreads)
    else
      for i := 1 to length(CpuInfoText) do
        if (CpuInfoText[i] > ' ') and
           (CpuInfoText[i] <> ':') then
          w.AddDirect(CpuInfoText[i]);
    {$ifdef OSWINDOWS}
    w.AddDirect('*');
    w.Add(SystemInfo.wProcessorArchitecture);
    w.AddDirect('-');
    w.Add(SystemInfo.wProcessorLevel);
    w.AddDirect('-');
    w.Add(SystemInfo.wProcessorRevision);
    {$endif OSWINDOWS}
    {$ifdef HASCPUFEATURES}
    w.AddDirect(':' {$ifdef ABIA32}, '-' {$endif} {$ifdef ABIA64}, '+' {$endif});
    w.AddBinToHexMinChars(@CpuFeatures, SizeOf(CpuFeatures), {lower=}true);
    {$endif HASCPUFEATURES}
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
    w.AddB(byte(WindowsSpecs));
    {$else}
    w.AddShorter(' Wow64=0');
    {$endif OSWINDOWS}
    w.AddShort(' Freq=1000000'); // we use QueryPerformanceMicroSeconds()
    if IsLibrary then
    begin
      w.AddShort(' Instance=');
      w.AddNoJsonEscapeString(Executable.InstanceFileName);
    end;
    {$ifdef OSWINDOWS} // too verbose on POSIX - even including some scripts :(
    if not fFamily.fNoEnvironmentVariable then
    begin
      w.AddDirect(#10);
      w.AddShort('Environment variables=');
      for i := 0 to length(_SystemEnvNames) - 1 do
      begin
        w.AddOnSameLine(pointer(_SystemEnvNames[i]));
        w.AddDirect('=');
        w.AddOnSameLine(pointer(_SystemEnvValues[i]));
        w.AddDirect(#9);
      end;
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
  fWriter.AddOnSameLine(@msg[1], ord(msg[0]));
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
begin // set timestamp [+ threadnumber] - usually run outside SynLogGlobalLock
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
    st.ToLogTime(@p[1]); // '20110325 19241502' 17 chars - not worth caching
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
begin // caller made SynLogGlobalLock.Lock
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
    SynLogGlobalLock.UnLock;
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
    SynLogGlobalLock.UnLock;
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
    SynLogGlobalLock.UnLock;
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
    begin
      if fFamily.IncludeComputerNameInFileName then
        if fFamily.IncludeUserNameInFileName then
          fn := FormatString('%(%@%)', [ProgramName, User, Host])
        else
          fn := FormatString('%(%)', [ProgramName, Host])
      else if fFamily.IncludeUserNameInFileName then
        fn := FormatString('%(%)', [ProgramName, User])
      else
        Utf8ToFileName(ProgramName, fn);
      if IsLibrary then // include library name
        fn := fn + ' ' + ExtractFileName(Executable.InstanceFileName);
    end;
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
  // include thread ID in ptOneFilePerThread mode
  if fFamily.fPerThreadLog = ptOneFilePerThread then
    fn := FormatString('% %',
      [fn, PointerToHexShort({%H-}pointer(GetCurrentThreadId))]);
  {$ifdef OSPOSIX}
  // normalize file name to be more readable and usable on POSIX command line
  fn := StringReplace(fn, ' ', '-', [rfReplaceAll]);
  {$endif OSPOSIX}
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
      exit; // exename-secondary.log was not yet active so has been selected
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
  SynLogGlobalLock.Lock;
  try
    if fWriterStream <> nil then
      result := fWriterStream.Size;
  finally
    SynLogGlobalLock.UnLock;
  end;
end;

{$ifdef FPC}

procedure TSynLog.AddStackTrace(Stack: PPtrUInt);
begin
  if fFamily.StackTraceLevel = 0 then
    exit;
  try
    fWriter.AddDirect(' ');
    // skip=2 to start at the caller of our caller, as this method did before
    TDebugFile.StackTrace(fWriter, {skip=}2, fFamily.StackTraceLevel,
      fFamily.StackTraceUse); // use is actually ignored on FPC
    fWriter.CancelLastChar(' ');
  except // don't let any unexpected GPF break the logging process
  end;
end;

{$else not FPC}

procedure TSynLog.AddStackTrace(Stack: PPtrUInt);
{$ifdef OSWINDOWS}
{$ifdef CPU64}

  procedure AddStackManual(Stack: PPtrUInt);
  begin
    // not implemented yet
  end;

{$else}

  procedure AddStackManual(Stack: PPtrUInt);
  // note: reuses CheckAsmX86() shared with the ManualStackTrace() function
  var
    st, max_stack, min_stack, depth: PtrUInt;
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
    fWriter.Add(' ');
    depth := fFamily.StackTraceLevel;
    try
      while (PtrUInt(Stack) < max_stack) and
            (depth > 0) do
      begin
        st := Stack^;
        inc(Stack);
        if (st >= min_stack) and
           (st <= max_stack) then
          continue; // on-stack pointer is no code
        if not SeemsRealPointer(pointer(st - 8)) or
           not CheckAsmX86(st) then
          continue;
        if not TDebugFile.AddLog(fWriter, st) then
        begin
          fWriter.AddPointer(st);
          fWriter.AddDirect(' ');
        end;
        dec(depth);
        if depth = 0 then
          break;
      end;
    except
      // just ignore any access violation here
    end;
  end;

{$endif CPU64}

var
  {$ifndef NOEXCEPTIONINTERCEPT}
  bak: TSynLogThreadInfoFlags; // paranoid precaution
  threadflags: ^TSynLogThreadInfoFlags;
  {$endif NOEXCEPTIONINTERCEPT}
  {$ifdef OSWINDOWS}
  n, i, logged: integer;
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
          if TDebugFile.AddLog(fWriter, BackTrace[i]) then
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

{$else}
begin // not implemented yet on Delphi POSIX
end;
{$endif OSWINDOWS}
{$endif FPC}


{ ************** High-Level Logs and Exception Related Features }

{$ifndef NOEXCEPTIONINTERCEPT}

procedure DoLogException(Log: TSynLog; Info: PSynLogThreadInfo;
  const Ctxt: TSynLogExceptionContext);
begin // called by SynLogException() within its SynLogGlobalLock.Lock
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
  bak: TSynLogThreadInfoFlags;
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
  if log.fFamily.ExceptionIgnoreExternal and
     (Ctxt.EAddr <> 0) and
     not IsCurrentExecutable(pointer(Ctxt.EAddr)) then // fast guess
    exit;
  thrdnam := CurrentThreadNameShort;
  bak := nfo^.Flags;
  exclude(nfo^.Flags, tiTemporaryDisable); // always log exceptions
  if log.LockAndDisableExceptions then
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
        n := MinPtrInt(high(last^.Stack) + 1, Ctxt.EStackCount);
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
      log.fWriter.AddDirect(' ', '['); // [#1 Main]
      n := nfo^.ThreadNumber;
      if n <> 0 then
      begin
        log.fWriter.AddDirect('#');
        log.fWriter.AddU(n);
      end;
      if thrdnam^[0] <> #0 then
      begin
        log.fWriter.AddDirect(' ');
        log.fWriter.AddShort(thrdnam^); // fThreadContext^.ThreadName may be ''
      end;
      log.fWriter.AddShorter('] at ');
      try
        log.fWriter.AddPointer(Ctxt.EAddr);
        log.fWriter.AddDirect(' ');
        TDebugFile.AddLog(log.fWriter, Ctxt.EAddr, {nohex=}true);
        {$ifdef FPC}
        prev := Ctxt.EAddr;
        // we rely on the stack trace supplied by the FPC RTL
        for i := 0 to Ctxt.EStackCount - 1 do
        begin
          curr := Ctxt.EStack[i];
          if curr = prev then
            continue; // don't log twice
          TDebugFile.AddLog(log.fWriter, curr);
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
        try // only DefaultSynLogExceptionToStr() but with no stack trace
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
    nfo^.Flags := bak; // may reintroduce tiTemporaryDisable
    SynLogGlobalLock.UnLock;
  end;
end;

function GetLastException(out info: TSynLogExceptionInfo): boolean;
begin
  result := false;
  if SynLogFileFreeing or
     (GlobalLastException.Index < 0) then
    exit; // no exception intercepted yet (or any more)
  SynLogGlobalLock.Lock;
  try
    if GlobalLastException.Index < 0 then
      exit;
    info := GlobalLastException.Infos[GlobalLastException.Index]; // copy
  finally
    SynLogGlobalLock.UnLock;
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
  SynLogGlobalLock.Lock;
  try
    infos := GlobalLastException.Infos;
    index := GlobalLastException.Index;
  finally
    SynLogGlobalLock.UnLock;
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
  tmp: ShortString;
begin
  with info.Context do
    if ELevel <> sllNone then
    begin
      TDebugFile.FindLocationShort(pointer(EAddr), tmp);
      FormatUtf8('% % at %: % [%]', [_LogInfoText[ELevel], EClass, tmp,
        UnixTimeToString(ETimestamp, {expanded=}true, ' '),
        StringToUtf8(info.Message)], result);
      if EStack <> nil then
        for i := 0 to EStackCount - 1 do
        begin
          TDebugFile.FindLocationShort(pointer(EStack[i]), tmp);
          Append(result, [', ', tmp]);
        end;
    end
    else
      FastAssignNew(result);
end;

function GetLastExceptionText: RawUtf8;
var
  info: TSynLogExceptionInfo;
begin
  if GetLastException(info) then
    result := ToText(info)
  else
    FastAssignNew(result);
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
      SynLogGlobalLock.Lock;
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
      SynLogGlobalLock.UnLock;
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
begin // overriden to take fLineTextOffset into account
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
    if sll = sllNone then // just ignore any unrecognized line
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
  enter64, leave64, time64: Int64;
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
              time64 := round(
                (EventDateTime(ndx) - EventDateTime(p^.Index)) * MicroSecsPerDay) +
                Int64(p^.Time mod 1000000)
            else
            begin
              // directly use high resolution timestamps as 64-bit integers
              HexDisplayToBin(fLines[p^.Index], @enter64, SizeOf(enter64));
              HexDisplayToBin(fLines[ndx],      @leave64, SizeOf(leave64));
              time64 := leave64 - enter64;
              if fFreq <> MicroSecsPerSec then
                time64 := (time64 * MicroSecsPerSec) div fFreq;
            end;
            if time64 shr 32 <> 0 then
              time64 := high(cardinal); // overflow over 1 hour and 11 minutes
            p^.Time := time64;
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
      FastSetString(S, PBeg, P);
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
    fWindowsSpecs := TWindowsSpecs(byte(GetInteger(pointer(aWow64))));
    fWow64 := wsWow64 in fWindowsSpecs;
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
        MS := DecodeMicroSec(PByteArray(LineEnd - 10));
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
    FastAssignNew(result);
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
    FastAssignNew(result)
  else
  begin
    L := GetLineSize(fLines[index], fMapEnd);
    if L <= fLineTextOffset then
      FastAssignNew(result)
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
      ToCaption(EventLevel[aRow])], result);
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
          result := ToCaption(EventLevel[aRow]);
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
    if ord(text[i]) in [33 .. 126] then
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

procedure TrimSynLogMessage(var P: PUtf8Char; var len: PtrInt;
  trimSynLogDate: boolean; maxLen: PtrInt);
begin
  if trimSynLogDate and
     (len > 27) then
  begin
    if (P[0] = '2') and
       (P[8] = ' ') then
    begin
      // trim e.g. '20160607 06442255  ! trace '
      inc(P, 27);
      dec(len, 27);
    end
    else if mormot.core.text.HexToBin(pointer(P), nil, 8) then
    begin
      // trim e.g. '00000000089E5A13  " info '
      inc(P, 25);
      dec(len, 25);
    end;
  end;
  while (len > 0) and
        (P^ <= ' ') do // trim left spaces (may be TSynLog indentation)
  begin
    inc(P);
    dec(len);
  end;
  while (len > 0) and
        (P[len - 1] <= ' ') do // trim right spaces
    dec(len);
  len := Utf8TruncatedLength(pointer(P), len, maxLen);
end;

const
  MAX_SYSLOG = 1500; // mimics UDP/Ethernet frame truncation

function SyslogMessage(facility: TSyslogFacility; severity: TSyslogSeverity;
  P: PAnsiChar; Len: PtrInt; const procid, msgid: RawUtf8; destbuffer: PUtf8Char;
  destsize: PtrInt; trimmsgfromlog: boolean; const appname: RawUtf8): PtrInt;
var
  start: PUtf8Char;
  name: PRawUtf8;
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
  FromGlobalTime(st, {local=}false);
  DateToIso8601PChar(destbuffer, true, st.Year, st.Month, st.Day);
  TimeToIso8601PChar(destbuffer + 10,
    true, st.Hour, st.Minute, st.Second, st.MilliSecond, 'T', {withms=}true);
  destbuffer[23] := 'Z';
  inc(destbuffer, 24);
  if appname <> '' then
    name := @appname
  else
    name := @Executable.ProgramName;
  if length(Executable.Host) + length(name^) + length(procid) +
     length(msgid) + (destbuffer - start) + 15 > destsize then
    // avoid buffer overflow
    exit;
  destbuffer := PrintUSAscii(destbuffer, Executable.Host); // HOST
  destbuffer := PrintUSAscii(destbuffer, name^);           // APP-NAME
  destbuffer := PrintUSAscii(destbuffer, procid);          // PROCID
  destbuffer := PrintUSAscii(destbuffer, msgid);           // MSGID
  destbuffer := PrintUSAscii(destbuffer, '');              // no STRUCTURED-DATA
  destbuffer^ := ' ';
  inc(destbuffer);
  TrimSynLogMessage(PUtf8Char(P), len, trimmsgfromlog,
    destsize - (destbuffer - start) - 3);
  if len < 2 then
    exit; // nothing to send
  if not IsAnsiCompatible(P, len) then
  begin
    PInteger(destbuffer)^ := BOM_UTF8; // weird enough behavior on POSIX :(
    inc(destbuffer, 3);
  end;
  MoveFast(P^, destbuffer^, len);
  destbuffer[len] := #0; // for debugging - not included in result length
  result := (destbuffer - start) + len;
end;

function SyslogPrepare(Level: TSynLogLevel; Text: PUtf8Char; Len: PtrInt;
  var Temp: TBuffer2K; out Dest: PUtf8Char; TlsTcpFormat, TrimSynLogDate: boolean;
  const AppName, MsgId: RawUtf8): PtrInt;
var
  DestEnd: PAnsiChar;
begin
  Dest := @Temp[8];
  result := SyslogMessage(sfUser, LOG_TO_SYSLOG[Level], pointer(Text), Len,
    UInt32ToUtf8(GetCurrentProcessId), MsgId, Dest, MAX_SYSLOG, TrimSynLogDate, AppName);
  if (result <= 0) or
     not TlsTcpFormat then
    exit;
  DestEnd := @Temp[result + 8];
  Temp[7] := ' '; // return as <len>' '<sysmessage>
  Dest := pointer(StrUInt32(PAnsiChar(@Temp[7]), result));
  result := DestEnd - Dest;
end;

function SyslogBsdPrepare(Level: TSynLogLevel; Text: PUtf8Char; Len: PtrInt;
  var Temp: TBuffer2K; TrimSynLogDate: boolean; const AppName: RawUtf8): PtrInt;
var
  now: TSynSystemTime;
  day: TShort3;
  h, a: TShort32; // truncated to 32 chars for legacy compatibility reasons
begin // <PRI>TIMESTAMP SP HOSTNAME SP TAG[: ]MESSAGE
  now.FromNowLocal; // the RFC 4.1.2 states it is the local time :(
  day[0] := #2;
  PWord(@day[1])^ := TwoDigitLookupW[now.Day];
  if day[1] = '0' then
    day[1] := ' ';
  h := Executable.Host;
  if AppName <> '' then
    a := AppName
  else
    a := Executable.ProgramName;
  result := FormatBuffer('<%>% % %:%:% % %[%]: ',
    [ord(LOG_TO_SYSLOG[Level]) + ord(sfUser) shl 3, HTML_MONTH_NAMES[now.Month],
     day, UInt2DigitsToShortFast(now.Hour), UInt2DigitsToShortFast(now.Minute),
     UInt2DigitsToShortFast(now.Second), h, a, GetCurrentProcessId],
    @Temp, SizeOf(Temp));
  TrimSynLogMessage(Text, Len, TrimSynLogDate, high(Temp) - result);
  MoveFast(Text^, Temp[result], Len);
  inc(result, Len);
  Temp[result] := #0; // for debugging
end;

{$ifdef OSLINUX} // compatibility function for old mORMot code
function SystemdEcho(Level: TSynLogLevel; const Text: RawUtf8;
  TrimSynLogDate: boolean): boolean;
begin
  result := JournalSend(Level, Text, TrimSynLogDate, {nosyslog=}true);
end;
{$endif OSLINUX}

function JournalSend(Level: TSynLogLevel; const Text: RawUtf8;
  TrimSynLogDate {$ifdef OSLINUX}, NoSysLogFallback{$endif}: boolean): boolean;
begin
  result := JournalSend(Level, pointer(Text), length(Text), TrimSynLogDate
    {$ifdef OSLINUX}, NoSysLogFallback{$endif});
end;

function JournalSend(Level: TSynLogLevel; Text: PUtf8Char; Len: PtrInt;
  TrimSynLogDate: boolean = true {$ifdef OSLINUX};
  NoSysLogFallback: boolean = false {$endif OSLINUX}): boolean;
{$ifdef OSPOSIX}
var
  priority: integer;
{$endif OSPOSIX}
begin
  // skip time and level e.g. '20200615 08003008  . '
  result := false;
  TrimSynLogMessage(Text, Len, TrimSynLogDate, MAX_SYSLOG);
  if len < 2 then
    exit; // nothing to send
  // call the proper OS API - note that bloated Windows ETW is not yet supported
  {$ifdef OSWINDOWS}
  WinDebugOutput(Text, len); // call OutputDebugStringW() API
  result := true;
  {$else}
  priority := ord(LOG_TO_SYSLOG[Level]);
  {$ifdef OSLINUX}
  if sd.IsAvailable and
     sd.Send(priority, Text, len) then
    result := true
  else if not NoSysLogFallback then
  {$endif OSLINUX}
    result := SysLogSend(priority, Text, len);
  {$endif OSWINDOWS}
end;


procedure InitializeUnit;
begin
  SynLogGlobalLock.Init;
  if (PtrUInt(@SynLogThreads) and POINTERAND) <> 0 then
    ESynLogException.RaiseU('SynLogThreads alignment issue');
  GetEnumTrimmedNames(TypeInfo(TSynLogLevel), @_LogInfoText);
  GetEnumTrimmedNames(TypeInfo(TAppLogLevel), @_LogAppText);
  SetThreadName := _SetThreadName;
  GetCurrentThreadName := _GetCurrentThreadName;
  SetCurrentThreadName('MainThread');
  GetExecutableLocation := _GetExecutableLocation; // use FindLocationShort()
  LogCompressAlgo := AlgoSynLZ; // very fast and efficient on logs
  LogCompressAlgoArchive := @_LogCompressAlgoArchive;
  //writeln(BacktraceStrFpc(Get_pc_addr));
  //writeln(GetExecutableLocation(get_caller_addr(get_frame)));
  //writeln(TDebugFile.FindLocation(@TDynArray.InitFrom));
  //TDebugFile.CurrentDebugFile.SaveToJson('debug.json',jsonUnquotedPropName);
end;

procedure FinalizeUnit;
var
  files: TSynLogDynArray; // thread-safe local copy
begin
  {$ifndef NOEXCEPTIONINTERCEPT}
  HandleExceptionFamily := nil; // disable exception interception
  {$endif NOEXCEPTIONINTERCEPT}
  SynLogFileFreeing := true;    // to avoid GPF at shutdown
  SynLogGlobalLock.Lock;
  files := SynLogFile;
  SynLogFile := nil;            // would break any background process
  SynLogFamily := nil;          // paranoid - freed as TRttiCustom.Private
  SynLogGlobalLock.UnLock;
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
  ObjArrayClear(DebugFiles);
  SynLogGlobalLock.Done;
end;


initialization
  InitializeUnit;

finalization
  FinalizeUnit;

end.

