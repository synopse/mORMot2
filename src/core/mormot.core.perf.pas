/// Framework Core Performance and Monitoring Classes
// - this unit is a part of the freeware Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.perf;

{
  *****************************************************************************

   Performance Monitoring functions shared by all framework units
    - Resource and Time Functions
    - Performance Counters
    - Monitoring Classes

  *****************************************************************************
}


interface

{$I ..\mormot.defines.inc}

uses
  Classes, 
  Contnrs,
  Types, 
  SysUtils, 
  mormot.core.base,
  mormot.core.data,
  mormot.core.text;


{ ************ Resource and Time Functions }

/// convert a size to a human readable value power-of-two metric value
// - append EB, PB, TB, GB, MB, KB or B symbol with or without preceding space
// - for EB, PB, TB, GB, MB and KB, add one fractional digit
procedure KB(bytes: Int64; out result: TShort16; nospace: boolean); overload;

/// convert a size to a human readable value
// - append EB, PB, TB, GB, MB, KB or B symbol with preceding space
// - for EB, PB, TB, GB, MB and KB, add one fractional digit
function KB(bytes: Int64): TShort16; overload;
  {$ifdef FPC_OR_UNICODE}inline;{$endif} // Delphi 2007 is buggy as hell

/// convert a size to a human readable value
// - append EB, PB, TB, GB, MB, KB or B symbol without preceding space
// - for EB, PB, TB, GB, MB and KB, add one fractional digit
function KBNoSpace(bytes: Int64): TShort16;
  {$ifdef FPC_OR_UNICODE}inline;{$endif} // Delphi 2007 is buggy as hell

/// convert a size to a human readable value
// - append EB, PB, TB, GB, MB, KB or B symbol with or without preceding space
// - for EB, PB, TB, GB, MB and KB, add one fractional digit
function KB(bytes: Int64; nospace: boolean): TShort16; overload;
  {$ifdef FPC_OR_UNICODE}inline;{$endif} // Delphi 2007 is buggy as hell

/// convert a string size to a human readable value
// - append EB, PB, TB, GB, MB, KB or B symbol
// - for EB, PB, TB, GB, MB and KB, add one fractional digit
function KB(const buffer: RawByteString): TShort16; overload;
  {$ifdef FPC_OR_UNICODE}inline;{$endif}

/// convert a size to a human readable value
// - append EB, PB, TB, GB, MB, KB or B symbol
// - for EB, PB, TB, GB, MB and KB, add one fractional digit
procedure KBU(bytes: Int64; var result: RawUTF8);

/// convert a micro seconds elapsed time into a human readable value
// - append 'us', 'ms', 's', 'm', 'h' and 'd' symbol for the given value range,
// with two fractional digits
function MicroSecToString(Micro: QWord): TShort16; overload;
  {$ifdef FPC_OR_UNICODE}inline;{$endif} // Delphi 2007 is buggy as hell

/// convert a micro seconds elapsed time into a human readable value
// - append 'us', 'ms', 's', 'm', 'h' and 'd' symbol for the given value range,
// with two fractional digits
procedure MicroSecToString(Micro: QWord; out result: TShort16); overload;

/// convert an integer value into its textual representation with thousands marked
// - ThousandSep is the character used to separate thousands in numbers with
// more than three digits to the left of the decimal separator
function IntToThousandString(Value: integer; const ThousandSep: TShort4=','): shortstring;



{ ************ Performance Counters }

type
  /// the kind of value stored in a TSynMonitor / TSynMonitorUsage property
  // - i.e. match TSynMonitorTotalMicroSec, TSynMonitorOneMicroSec,
  // TSynMonitorOneCount, TSynMonitorOneBytes, TSynMonitorBytesPerSec,
  // TSynMonitorTotalBytes, TSynMonitorCount and TSynMonitorCount64 types as
  // used to store statistic information
  // - "cumulative" values would sum each process values, e.g. total elapsed
  // time for SOA execution, task count or total I/O bytes
  // - "immediate" (e.g. svOneBytes or smvBytesPerSec) values would be an evolving
  // single value, e.g. an average value or current disk free size
  // - use SYNMONITORVALUE_CUMULATIVE = [smvMicroSec,smvBytes,smvCount,smvCount64]
  // constant to identify the kind of value
  // - TSynMonitorUsage.Track() would use MonitorPropUsageValue() to guess
  // the tracked properties type from class RTTI
  TSynMonitorType = (
    smvUndefined, smvOneMicroSec, smvOneBytes, smvOneCount, smvBytesPerSec,
    smvMicroSec, smvBytes, smvCount, smvCount64);
  /// value types as stored in TSynMonitor / TSynMonitorUsage
  TSynMonitorTypes = set of TSynMonitorType;

  /// would identify a cumulative time process information in micro seconds, during monitoring
  // - "cumulative" time would add each process timing, e.g. for statistics about
  // SOA computation of a given service
  // - any property defined with this type would be identified by TSynMonitorUsage
  TSynMonitorTotalMicroSec = type QWord;

  /// would identify an immediate time count information, during monitoring
  // - "immediate" counts won't accumulate, e.g. may store the current number
  // of thread used by a process
  // - any property defined with this type would be identified by TSynMonitorUsage
  TSynMonitorOneCount = type cardinal;

  /// would identify an immediate time process information in micro seconds, during monitoring
  // - "immediate" time won't accumulate, i.e. may store the duration of the
  // latest execution of a SOA computation
  // - any property defined with this type would be identified by TSynMonitorUsage
  TSynMonitorOneMicroSec = type QWord;

  /// would identify a process information as cumulative bytes count, during monitoring
  // - "cumulative" size would add some byte for each process, e.g. input/output
  // - any property defined with this type would be identified by TSynMonitorUsage
  TSynMonitorTotalBytes = type QWord;

  /// would identify an immediate process information as bytes count, during monitoring
  // - "immediate" size won't accumulate, i.e. may be e.g. computer free memory
  // at a given time
  // - any property defined with this type would be identified by TSynMonitorUsage
  TSynMonitorOneBytes = type QWord;

  /// would identify the process throughput, during monitoring
  // - it indicates e.g. "immediate" bandwith usage
  // - any property defined with this type would be identified by TSynMonitorUsage
  TSynMonitorBytesPerSec = type QWord;

  /// would identify a cumulative number of processes, during monitoring
  // - any property defined with this type would be identified by TSynMonitorUsage
  TSynMonitorCount = type cardinal;

  /// would identify a cumulative number of processes, during monitoring
  // - any property defined with this type would be identified by TSynMonitorUsage
  TSynMonitorCount64 = type QWord;

  /// pointer to a high resolution timer object/record
  PPrecisionTimer = ^TPrecisionTimer;

  /// indirect reference to a pointer to a high resolution timer object/record
  PPPrecisionTimer = ^PPrecisionTimer;

  /// high resolution timer (for accurate speed statistics)
  TPrecisionTimer = object
  protected
    fStart, fStop: Int64;
    /// contains the time elapsed in micro seconds between Start and Stop
    fTime: TSynMonitorTotalMicroSec;
    /// contains the time elapsed in micro seconds between Resume and Pause
    fLastTime: TSynMonitorOneMicroSec;
    fPauseCount: TSynMonitorCount;
  public
    /// initialize the timer
    // - will fill all internal state with 0
    // - not necessary e.g. if TPrecisionTimer is defined as a TObject field
    procedure Init; {$ifdef HASINLINE}inline;{$endif}
    /// initialize and start the high resolution timer
    // - similar to Init + Resume
    procedure Start;
    /// stop the timer, returning the total time elapsed as text
    // - with appended time resolution (us,ms,s) - from MicroSecToString()
    // - is just a wrapper around Pause + Time
    // - you can call Resume to continue adding time to this timer
    function Stop: TShort16; {$ifdef HASINLINE}inline;{$endif}
    /// stop the timer, returning the total time elapsed as microseconds
    // - is just a wrapper around Pause + Time
    // - you can call Resume to continue adding time to this timer
    function StopInMicroSec: TSynMonitorTotalMicroSec; {$ifdef HASINLINE}inline;{$endif}
    /// stop the timer, ready to continue its time measurement via Resume
    // - will also compute the global Time value
    // - do nothing if no previous Start/Resume call is pending
    procedure Pause;
    /// resume a paused timer, or start an initialized timer
    // - do nothing if no timer has been initialized or paused just before
    // - if the previous method called was Init, will act like Start
    // - if the previous method called was Pause, it will continue counting
    procedure Resume; {$ifdef HASINLINE}inline;{$endif}
    /// resume a paused timer until the method ends
    // - will internaly create a TInterfaceObject class to let the compiler
    // generate a try..finally block as expected to call Pause at method ending
    // - is therefore very convenient to have consistent Resume/Pause calls
    // - for proper use, expect TPrecisionTimer to be initialized to 0 before
    // execution (e.g. define it as a protected member of a class)
    // - typical use is to declare a fTimeElapsed: TPrecisionTimer protected
    // member, then call fTimeElapsed.ProfileCurrentMethod at the beginning of
    // all process expecting some timing, then log/save fTimeElapsed.Stop content
    // - FPC TIP: result should be assigned to a local variable of IUnknown type
    function ProfileCurrentMethod: IUnknown;
    /// low-level method to force values settings to allow thread safe timing
    // - by default, this timer is not thread safe: you can use this method to
    // set the timing values from manually computed performance counters
    // - the caller should also use a mutex to prevent from race conditions:
    // see e.g. TSynMonitor.FromExternalMicroSeconds implementation
    // - warning: Start, Stop, Pause and Resume methods are then disallowed
    procedure FromExternalMicroSeconds(const MicroSeconds: QWord);
      {$ifdef FPC_OR_UNICODE}inline;{$endif} // Delphi 2007 is buggy as hell
    /// low-level method to force values settings to allow thread safe timing
    // - by default, this timer is not thread safe: you can use this method to
    // set the timing values from manually computed performance counters
    // - the caller should also use a mutex to prevent from race conditions:
    // see e.g. TSynMonitor.FromExternalQueryPerformanceCounters implementation
    // - returns the time elapsed, in micro seconds (i.e. LastTime value)
    // - warning: Start, Stop, Pause and Resume methods are then disallowed
    function FromExternalQueryPerformanceCounters(const CounterDiff: QWord): QWord;
      {$ifdef FPCLINUX}inline;{$endif}
    /// compute the per second count
    function PerSec(const Count: QWord): QWord;
    /// compute the time elapsed by count, with appened time resolution (us,ms,s)
    function ByCount(Count: QWord): TShort16;
    /// returns e.g. '16.9 MB in 102.20ms i.e. 165.5 MB/s'
    function SizePerSec(Size: QWord): shortstring;
    /// textual representation of total time elapsed
    // - with appened time resolution (us,ms,s) - from MicroSecToString()
    // - not to be used in normal code (which could rather call the Stop method),
    // but e.g. for custom performance analysis
    function Time: TShort16;
    /// textual representation of last process timing after counter stopped
    // - Time returns a total elapsed time, whereas this method only returns
    // the latest resumed time
    // - with appened time resolution (us,ms,s) - from MicroSecToString()
    // - not to be used in normal code, but e.g. for custom performance analysis
    function LastTime: TShort16;
    /// check if Start/Resume were called at least once
    function Started: boolean;
    /// time elapsed in micro seconds after counter stopped
    // - not to be used in normal code, but e.g. for custom performance analysis
    property TimeInMicroSec: TSynMonitorTotalMicroSec read fTime write fTime;
    /// timing in micro seconds of the last process
    // - not to be used in normal code, but e.g. for custom performance analysis
    property LastTimeInMicroSec: TSynMonitorOneMicroSec read fLastTime write fLastTime;
    /// how many times the Pause method was called, i.e. the number of tasks
    // processeed
    property PauseCount: TSynMonitorCount read fPauseCount;
  end;

  /// interface to a reference counted high resolution timer instance
  // - implemented by TLocalPrecisionTimer
  ILocalPrecisionTimer = interface
    /// start the high resolution timer
    procedure Start;
    /// stop the timer, returning the time elapsed, with appened time resolution (us,ms,s)
    function Stop: TShort16;
    /// stop the timer, ready to continue its time measure
    procedure Pause;
    /// resume a paused timer, or start it if it hasn't be started
    procedure Resume;
    /// compute the per second count
    function PerSec(Count: cardinal): cardinal;
    /// compute the time elapsed by count, with appened time resolution (us,ms,s)
    function ByCount(Count: cardinal): RawUTF8;
  end;

  /// reference counted high resolution timer (for accurate speed statistics)
  // - since TPrecisionTimer shall be 32-bit aligned, you can use this class
  // to initialize a local auto-freeing ILocalPrecisionTimer variable on stack
  // - to be used as such:
  // ! var Timer: ILocalPrecisionTimer;
  // !  (...)
  // !   Timer := TLocalPrecisionTimer.Create;
  // !   Timer.Start;
  // !  (...)
  TLocalPrecisionTimer = class(TInterfacedObject, ILocalPrecisionTimer)
  protected
    fTimer: TPrecisionTimer;
  public
    /// initialize the instance, and start the high resolution timer
    constructor CreateAndStart;
    /// start the high resolution timer
    procedure Start;
    /// stop the timer, returning the time elapsed, with appened time resolution (us,ms,s)
    function Stop: TShort16;
    /// stop the timer, ready to continue its time measure
    procedure Pause;
    /// resume a paused timer, or start the timer
    procedure Resume;
    /// compute the per second count
    function PerSec(Count: cardinal): cardinal;
    /// compute the time elapsed by count, with appened time resolution (us,ms,s)
    function ByCount(Count: cardinal): RawUTF8;
  end;



{ ************ Monitoring Classes }

type
  /// able to serialize any cumulative timing as raw micro-seconds number or text
  // - "cumulative" time would add each process value, e.g. SOA methods execution
  TSynMonitorTime = class(TSynPersistent)
  protected
    fMicroSeconds: TSynMonitorTotalMicroSec;
    function GetAsText: TShort16;
  public
    /// compute a number per second, of the current value
    function PerSecond(const Count: QWord): QWord;
      {$ifdef FPC_OR_UNICODE}inline;{$endif} // Delphi 2007 is buggy as hell
  published
    /// micro seconds time elapsed, as raw number
    property MicroSec: TSynMonitorTotalMicroSec read fMicroSeconds write fMicroSeconds;
    /// micro seconds time elapsed, as '... us-ns-ms-s' text
    property Text: TShort16 read GetAsText;
  end;

  /// able to serialize any immediate timing as raw micro-seconds number or text
  // - "immediate" size won't accumulate, i.e. may be e.g. last process time
  TSynMonitorOneTime = class(TSynPersistent)
  protected
    fMicroSeconds: TSynMonitorOneMicroSec;
    function GetAsText: TShort16;
  public
    /// compute a number per second, of the current value
    function PerSecond(const Count: QWord): QWord;
      {$ifdef FPC_OR_UNICODE}inline;{$endif} // Delphi 2007 is buggy as hell
  published
    /// micro seconds time elapsed, as raw number
    property MicroSec: TSynMonitorOneMicroSec read fMicroSeconds write fMicroSeconds;
    /// micro seconds time elapsed, as '... us-ns-ms-s' text
    property Text: TShort16 read GetAsText;
  end;

  TSynMonitorSizeParent = class(TSynPersistent)
  protected
    fTextNoSpace: boolean;
  public
    /// initialize the instance
    constructor Create(aTextNoSpace: boolean); reintroduce;
  end;

  /// able to serialize any cumulative size as bytes number
  // - "cumulative" time would add each process value, e.g. global IO consumption
  TSynMonitorSize = class(TSynMonitorSizeParent)
  protected
    fBytes: TSynMonitorTotalBytes;
    function GetAsText: TShort16;
  published
    /// number of bytes, as raw number
    property Bytes: TSynMonitorTotalBytes read fBytes write fBytes;
    /// number of bytes, as '... B-KB-MB-GB' text
    property Text: TShort16 read GetAsText;
  end;

  /// able to serialize any immediate size as bytes number
  // - "immediate" size won't accumulate, i.e. may be e.g. computer free memory
  // at a given time
  TSynMonitorOneSize = class(TSynMonitorSizeParent)
  protected
    fBytes: TSynMonitorOneBytes;
    function GetAsText: TShort16;
  published
    /// number of bytes, as raw number
    property Bytes: TSynMonitorOneBytes read fBytes write fBytes;
    /// number of bytes, as '... B-KB-MB-GB' text
    property Text: TShort16 read GetAsText;
  end;

  /// able to serialize any bandwith as bytes count per second
  // - is usually associated with TSynMonitorOneSize properties,
  // e.g. to monitor IO activity
  TSynMonitorThroughput = class(TSynMonitorSizeParent)
  protected
    fBytesPerSec: QWord;
    function GetAsText: TShort16;
  published
    /// number of bytes per second, as raw number
    property BytesPerSec: QWord read fBytesPerSec write fBytesPerSec;
    /// number of bytes per second, as '... B-KB-MB-GB/s' text
    property Text: TShort16 read GetAsText;
  end;

  /// a generic value object able to handle any task / process statistic
  // - base class shared e.g. for ORM, SOA or DDD, when a repeatable data
  // process is to be monitored
  // - this class is thread-safe for its methods, but you should call explicitly
  // Lock/UnLock to access its individual properties
  TSynMonitor = class(TSynPersistentLock)
  protected
    fName: RawUTF8;
    fTaskCount: TSynMonitorCount64;
    fTotalTime: TSynMonitorTime;
    fLastTime: TSynMonitorOneTime;
    fMinimalTime: TSynMonitorOneTime;
    fAverageTime: TSynMonitorOneTime;
    fMaximalTime: TSynMonitorOneTime;
    fPerSec: QWord;
    fInternalErrors: TSynMonitorCount;
    fProcessing: boolean;
    fTaskStatus: (taskNotStarted,taskStarted);
    fLastInternalError: variant;
    procedure LockedPerSecProperties; virtual;
    procedure LockedFromProcessTimer; virtual;
    procedure LockedSum(another: TSynMonitor); virtual;
    procedure WriteDetailsTo(W: TAbstractWriter); virtual;
    procedure Changed; virtual;
  public
    /// low-level high-precision timer instance
    InternalTimer: TPrecisionTimer;
    /// initialize the instance nested class properties
    // - you can specify identifier associated to this monitored resource
    // which would be used for TSynMonitorUsage persistence
    constructor Create(const aName: RawUTF8); reintroduce; overload; virtual;
    /// initialize the instance nested class properties
    constructor Create; overload; override;
    /// finalize the instance
    destructor Destroy; override;
    /// lock the instance for exclusive access
    // - needed only if you access directly the instance properties
    procedure Lock; {$ifdef HASINLINE}inline;{$endif}
    /// release the instance for exclusive access
    // - needed only if you access directly the instance properties
    procedure UnLock; {$ifdef HASINLINE}inline;{$endif}
    /// create Count instances of this actual class in the supplied ObjArr[]
    class procedure InitializeObjArray(var ObjArr; Count: integer); virtual;
    /// should be called when the process starts, to resume the internal timer
    // - thread-safe method
    procedure ProcessStart; virtual;
    /// should be called each time a pending task is processed
    // - will increase the TaskCount property
    // - thread-safe method
    procedure ProcessDoTask; virtual;
    /// should be called when the process starts, and a task is processed
    // - similar to ProcessStart + ProcessDoTask
    // - thread-safe method
    procedure ProcessStartTask; virtual;
    /// should be called when an error occurred
    // - typical use is with ObjectToVariantDebug(E,...) kind of information
    // - thread-safe method
    procedure ProcessError(const info: variant); virtual;
    /// should be called when an error occurred
    // - typical use is with a HTTP status, e.g. as ProcessError(Call.OutStatus)
    // - just a wraper around overloaded ProcessError(), so a thread-safe method
    procedure ProcessErrorNumber(info: integer);
    /// should be called when an error occurred
    // - just a wraper around overloaded ProcessError(), so a thread-safe method
    procedure ProcessErrorFmt(const Fmt: RawUTF8; const Args: array of const);
    /// should be called when an Exception occurred
    // - just a wraper around overloaded ProcessError(), so a thread-safe method
    procedure ProcessErrorRaised(E: Exception);
    /// should be called when the process stops, to pause the internal timer
    // - thread-safe method
    procedure ProcessEnd; virtual;
    /// could be used to manage information average or sums
    // - thread-safe method calling LockedSum protected virtual method
    procedure Sum(another: TSynMonitor);
    /// returns a JSON content with all published properties information
    // - thread-safe method
    function ComputeDetailsJSON: RawUTF8;
    /// appends a JSON content with all published properties information
    // - thread-safe method
    procedure ComputeDetailsTo(W: TAbstractWriter); virtual;
    /// used to allow thread safe timing
    // - by default, the internal TPrecisionTimer is not thread safe: you can
    // use this method to update the timing from many threads
    // - if you use this method, ProcessStart, ProcessDoTask and ProcessEnd
    // methods are disallowed, and the global fTimer won't be used any more
    // - will return the processing time, converted into micro seconds, ready
    // to be logged if needed
    // - thread-safe method
    function FromExternalQueryPerformanceCounters(const CounterDiff: QWord): QWord;
    /// used to allow thread safe timing
    // - by default, the internal TPrecisionTimer is not thread safe: you can
    // use this method to update the timing from many threads
    // - if you use this method, ProcessStart, ProcessDoTask and ProcessEnd
    // methods are disallowed, and the global fTimer won't be used any more
    // - thread-safe method
    procedure FromExternalMicroSeconds(const MicroSecondsElapsed: QWord);
    /// an identifier associated to this monitored resource
    // - is used e.g. for TSynMonitorUsage persistence/tracking
    property Name: RawUTF8 read fName write fName;
  published
    /// indicates if this thread is currently working on some process
    property Processing: boolean read fProcessing write fProcessing;
    /// how many times the task was performed
    property TaskCount: TSynMonitorCount64 read fTaskCount write fTaskCount;
    /// the whole time spend during all working process
    property TotalTime: TSynMonitorTime read fTotalTime;
    /// the time spend during the last task processing
    property LastTime: TSynMonitorOneTime read fLastTime;
    /// the lowest time spent during any working process
    property MinimalTime: TSynMonitorOneTime read fMinimalTime;
    /// the time spent in average during any working process
    property AverageTime: TSynMonitorOneTime read fAverageTime;
    /// the highest time spent during any working process
    property MaximalTime: TSynMonitorOneTime read fMaximalTime;
    /// average of how many tasks did occur per second
    property PerSec: QWord read fPerSec;
    /// how many errors did occur during the processing
    property Errors: TSynMonitorCount read fInternalErrors;
    /// information about the last error which occured during the processing
    property LastError: variant read fLastInternalError;
  end;
  /// references a TSynMonitor instance
  PSynMonitor = ^TSynMonitor;

  /// handle generic process statistic with a processing data size and bandwitdh
  TSynMonitorWithSize = class(TSynMonitor)
  protected
    fSize: TSynMonitorSize;
    fThroughput: TSynMonitorThroughput;
    procedure LockedPerSecProperties; override;
    procedure LockedSum(another: TSynMonitor); override;
  public
    /// initialize the instance nested class properties
    constructor Create; override;
    /// finalize the instance
    destructor Destroy; override;
    /// increase the internal size counter
    // - thread-safe method
    procedure AddSize(const Bytes: QWord);
  published
    /// how many total data has been hanlded during all working process
    property Size: TSynMonitorSize read fSize;
    /// data processing bandwith, returned as B/KB/MB per second
    property Throughput: TSynMonitorThroughput read fThroughput;
  end;

  /// handle generic process statistic with a incoming and outgoing processing
  // data size and bandwitdh
  TSynMonitorInputOutput = class(TSynMonitor)
  protected
    fInput: TSynMonitorSize;
    fOutput: TSynMonitorSize;
    fInputThroughput: TSynMonitorThroughput;
    fOutputThroughput: TSynMonitorThroughput;
    procedure LockedPerSecProperties; override;
    procedure LockedSum(another: TSynMonitor); override;
  public
    /// initialize the instance nested class properties
    constructor Create; override;
    /// finalize the instance
    destructor Destroy; override;
    /// increase the internal size counters
    // - thread-safe method
    procedure AddSize(const Incoming, Outgoing: QWord);
  published
    /// how many data has been received
    property Input: TSynMonitorSize read fInput;
    /// how many data has been sent back
    property Output: TSynMonitorSize read fOutput;
    /// incoming data processing bandwith, returned as B/KB/MB per second
    property InputThroughput: TSynMonitorThroughput read fInputThroughput;
    /// outgoing data processing bandwith, returned as B/KB/MB per second
    property OutputThroughput: TSynMonitorThroughput read fOutputThroughput;
  end;

  /// could monitor a standard Server
  // - including Input/Output statistics and connected Clients count
  TSynMonitorServer = class(TSynMonitorInputOutput)
  protected
    fCurrentRequestCount: integer;
    fClientsCurrent: TSynMonitorOneCount;
    fClientsMax: TSynMonitorOneCount;
  public
    /// update ClientsCurrent and ClientsMax
    // - thread-safe method
    procedure ClientConnect;
    /// update ClientsCurrent and ClientsMax
    // - thread-safe method
    procedure ClientDisconnect;
    /// update ClientsCurrent to 0
    // - thread-safe method
    procedure ClientDisconnectAll;
    /// retrieve the number of connected clients
    // - thread-safe method
    function GetClientsCurrent: TSynMonitorOneCount;
    /// how many concurrent requests are currently processed
    // - returns the updated number of requests
    // - thread-safe method
    function AddCurrentRequestCount(diff: integer): integer;
  published
    /// current count of connected clients
    property ClientsCurrent: TSynMonitorOneCount read fClientsCurrent;
    /// max count of connected clients
    property ClientsMax: TSynMonitorOneCount read fClientsMax;
    /// how many concurrent requests are currently processed
    // - modified via AddCurrentRequestCount() in TSQLRestServer.URI()
    property CurrentRequestCount: integer read fCurrentRequestCount;
  end;

  /// a list of simple process statistics
  TSynMonitorObjArray = array of TSynMonitor;

  /// a list of data process statistics
  TSynMonitorWithSizeObjArray = array of TSynMonitorWithSize;

  /// a list of incoming/outgoing data process statistics
  TSynMonitorInputOutputObjArray = array of TSynMonitorInputOutput;

  /// class-reference type (metaclass) of a process statistic information
  TSynMonitorClass = class of TSynMonitor;


implementation

uses
  mormot.core.os;


{ ************ Resource and Time Functions }

procedure KB(bytes: Int64; out result: TShort16; nospace: boolean);
type
  TUnits = (kb, mb, gb, tb, pb, eb, b);
const
  TXT: array[boolean, TUnits] of RawUTF8 = (
    (' KB', ' MB', ' GB', ' TB', ' PB', ' EB', '% B'),
    ('KB', 'MB', 'GB', 'TB', 'PB', 'EB', '%B'));
var
  hi, rem: cardinal;
  u: TUnits;
begin
  if bytes < 1 shl 10 - (1 shl 10) div 10 then
  begin
    FormatShort16(TXT[nospace, b], [integer(bytes)], result);
    exit;
  end;
  if bytes < 1 shl 20 - (1 shl 20) div 10 then
  begin
    u := kb;
    rem := bytes;
    hi := bytes shr 10;
  end
  else if bytes < 1 shl 30 - (1 shl 30) div 10 then
  begin
    u := mb;
    rem := bytes shr 10;
    hi := bytes shr 20;
  end
  else if bytes < Int64(1) shl 40 - (Int64(1) shl 40) div 10 then
  begin
    u := gb;
    rem := bytes shr 20;
    hi := bytes shr 30;
  end
  else if bytes < Int64(1) shl 50 - (Int64(1) shl 50) div 10 then
  begin
    u := tb;
    rem := bytes shr 30;
    hi := bytes shr 40;
  end
  else if bytes < Int64(1) shl 60 - (Int64(1) shl 60) div 10 then
  begin
    u := pb;
    rem := bytes shr 40;
    hi := bytes shr 50;
  end
  else
  begin
    u := eb;
    rem := bytes shr 50;
    hi := bytes shr 60;
  end;
  rem := rem and 1023;
  if rem <> 0 then
    rem := rem div 102;
  if rem = 10 then
  begin
    rem := 0;
    inc(hi); // round up as expected by (most) human beings
  end;
  if rem <> 0 then
    FormatShort16('%.%%', [hi, rem, TXT[nospace, u]], result)
  else
    FormatShort16('%%', [hi, TXT[nospace, u]], result);
end;

function KB(bytes: Int64): TShort16;
begin
  KB(bytes, result, {nospace=}false);
end;

function KBNoSpace(bytes: Int64): TShort16;
begin
  KB(bytes, result, {nospace=}true);
end;

function KB(bytes: Int64; nospace: boolean): TShort16;
begin
  KB(bytes, result, nospace);
end;

function KB(const buffer: RawByteString): TShort16;
begin
  KB(length(buffer), result, {nospace=}false);
end;

procedure KBU(bytes: Int64; var result: RawUTF8);
var
  tmp: TShort16;
begin
  KB(bytes, tmp, {nospace=}false);
  FastSetString(result, @tmp[1], ord(tmp[0]));
end;

function IntToThousandString(Value: integer; const ThousandSep: TShort4): shortstring;
var
  i, L, Len: cardinal;
begin
  str(Value, result);
  L := length(result);
  Len := L + 1;
  if Value < 0 then
    dec(L, 2)
  else // ignore '-' sign
    dec(L);
  for i := 1 to L div 3 do
    insert(ThousandSep, result, Len - i * 3);
end;

function MicroSecToString(Micro: QWord): TShort16;
begin
  MicroSecToString(Micro, result);
end;

procedure MicroSecToString(Micro: QWord; out result: TShort16);

  procedure TwoDigitToString(value: cardinal; const u: shortstring; var result: TShort16);
  var
    d100: TDiv100Rec;
  begin
    if value < 100 then
      FormatShort16('0.%%', [UInt2DigitsToShortFast(value), u], result)
    else
    begin
      Div100(value, d100);
      if d100.m = 0 then
        FormatShort16('%%', [d100.d, u], result)
      else
        FormatShort16('%.%%', [d100.d, UInt2DigitsToShortFast(d100.m), u], result);
    end;
  end;

  procedure TimeToString(value: cardinal; const u: shortstring; var result: TShort16);
  var
    d: cardinal;
  begin
    d := value div 60;
    FormatShort16('%%%', [d, u, UInt2DigitsToShortFast(value - (d * 60))], result);
  end;

begin
  if Int64(Micro) <= 0 then
    result := '0us'
  else if Micro < 1000 then
    FormatShort16('%us', [Micro], result)
  else if Micro < 1000000 then
    TwoDigitToString({$ifdef CPU32} PCardinal(@Micro)^ {$else} Micro {$endif} div 10, 'ms', result)
  else if Micro < 60000000 then
    TwoDigitToString({$ifdef CPU32} PCardinal(@Micro)^ {$else} Micro {$endif} div 10000, 's', result)
  else if Micro < QWord(3600000000) then
    TimeToString({$ifdef CPU32} PCardinal(@Micro)^ {$else} Micro {$endif} div 1000000, 'm', result)
  else if Micro < QWord(86400000000 * 2) then
    TimeToString(Micro div 60000000, 'h', result)
  else
    FormatShort16('%d', [Micro div QWord(86400000000)], result)
end;



{ ************ Performance Counters }

{ TPrecisionTimer }

procedure TPrecisionTimer.Init;
begin
  FillCharFast(self, SizeOf(self), 0);
end;

procedure TPrecisionTimer.Start;
begin
  FillCharFast(self, SizeOf(self), 0);
  QueryPerformanceMicroSeconds(fStart);
end;

function TPrecisionTimer.Started: boolean;
begin
  result := (fStart <> 0) or (fTime <> 0);
end;

procedure TPrecisionTimer.Resume;
begin
  if fStart = 0 then
    QueryPerformanceMicroSeconds(fStart);
end;

procedure TPrecisionTimer.Pause;
begin
  if fStart = 0 then
    exit;
  QueryPerformanceMicroSeconds(fStop);
  FromExternalQueryPerformanceCounters(fStop - fStart);
  inc(fPauseCount);
end;

procedure TPrecisionTimer.FromExternalMicroSeconds(const MicroSeconds: QWord);
begin
  fLastTime := MicroSeconds;
  inc(fTime, MicroSeconds);
  fStart := 0; // indicates time has been computed
end;

function TPrecisionTimer.FromExternalQueryPerformanceCounters(const CounterDiff: QWord): QWord;
begin // mimics Pause from already known elapsed time
  FromExternalMicroSeconds(CounterDiff);
  result := fLastTime;
end;

function TPrecisionTimer.Stop: TShort16;
begin
  if fStart <> 0 then
    Pause;
  MicroSecToString(fTime, result);
end;

function TPrecisionTimer.StopInMicroSec: TSynMonitorTotalMicroSec;
begin
  if fStart <> 0 then
    Pause;
  result := fTime;
end;

function TPrecisionTimer.Time: TShort16;
begin
  if fStart <> 0 then
    Pause;
  MicroSecToString(fTime, result);
end;

function TPrecisionTimer.LastTime: TShort16;
begin
  if fStart <> 0 then
    Pause;
  MicroSecToString(fLastTime, result);
end;

function TPrecisionTimer.ByCount(Count: QWord): TShort16;
begin
  if Count = 0 then // avoid div per 0 exception
    result := '0'
  else
  begin
    if fStart <> 0 then
      Pause;
    MicroSecToString(fTime div Count, result);
  end;
end;

function TPrecisionTimer.PerSec(const Count: QWord): QWord;
begin
  if fStart <> 0 then
    Pause;
  if fTime <= 0 then // avoid negative value in case of incorrect Start/Stop sequence
    result := 0
  else // avoid div per 0 exception
    result := (Count * 1000000) div fTime;
end;

function TPrecisionTimer.SizePerSec(Size: QWord): shortstring;
begin
  FormatShort('% in % i.e. %/s', [KB(Size), Stop, KB(PerSec(Size))], result);
end;

type
  /// a class used internaly by TPrecisionTimer.ProfileMethod
  TPrecisionTimerProfiler = class(TInterfacedObject)
  protected
    fTimer: PPrecisionTimer;
  public
    constructor Create(aTimer: PPrecisionTimer);
    destructor Destroy; override;
  end;

constructor TPrecisionTimerProfiler.Create(aTimer: PPrecisionTimer);
begin
  fTimer := aTimer;
end;

destructor TPrecisionTimerProfiler.Destroy;
begin
  if fTimer <> nil then
    fTimer^.Pause;
  inherited;
end;

function TPrecisionTimer.ProfileCurrentMethod: IUnknown;
begin
  Resume;
  result := TPrecisionTimerProfiler.Create(@self);
end;


{ TLocalPrecisionTimer }

function TLocalPrecisionTimer.ByCount(Count: cardinal): RawUTF8;
begin
  result := fTimer.ByCount(Count);
end;

procedure TLocalPrecisionTimer.Pause;
begin
  fTimer.Pause;
end;

function TLocalPrecisionTimer.PerSec(Count: cardinal): cardinal;
begin
  result := fTimer.PerSec(Count);
end;

procedure TLocalPrecisionTimer.Resume;
begin
  fTimer.Resume;
end;

procedure TLocalPrecisionTimer.Start;
begin
  fTimer.Start;
end;

function TLocalPrecisionTimer.Stop: TShort16;
begin
  result := fTimer.Stop;
end;

constructor TLocalPrecisionTimer.CreateAndStart;
begin
  inherited;
  fTimer.Start;
end;


{ ************ Monitoring Classes }

{ TSynMonitorTime }

function TSynMonitorTime.GetAsText: TShort16;
begin
  MicroSecToString(fMicroSeconds, result);
end;

function TSynMonitorTime.PerSecond(const Count: QWord): QWord;
begin
  {$ifdef FPC}
  if Int64(fMicroSeconds) <= 0 then
  {$else}
  if PInt64(@fMicroSeconds)^ <= 0 then
  {$endif FPC}
    result := 0
  else // avoid negative or div per 0
    result := (Count * 1000000) div fMicroSeconds;
end;


{ TSynMonitorOneTime }

function TSynMonitorOneTime.GetAsText: TShort16;
begin
  MicroSecToString(fMicroSeconds, result);
end;

function TSynMonitorOneTime.PerSecond(const Count: QWord): QWord;
begin
  {$ifdef FPC}
  if Int64(fMicroSeconds) <= 0 then
  {$else}
  if PInt64(@fMicroSeconds)^ <= 0 then
  {$endif FPC}
    result := 0
  else
    result := (Count * QWord(1000000)) div fMicroSeconds;
end;


{ TSynMonitorSizeParent }

constructor TSynMonitorSizeParent.Create(aTextNoSpace: boolean);
begin
  inherited Create;
  fTextNoSpace := aTextNoSpace;
end;

{ TSynMonitorSize }

function TSynMonitorSize.GetAsText: TShort16;
begin
  KB(fBytes, result, fTextNoSpace);
end;

{ TSynMonitorOneSize }

function TSynMonitorOneSize.GetAsText: TShort16;
begin
  KB(fBytes, result, fTextNoSpace);
end;

{ TSynMonitorThroughput }

function TSynMonitorThroughput.GetAsText: TShort16;
begin
  FormatShort16('%/s', [KB(fBytesPerSec, fTextNoSpace)], result);
end;


{ TSynMonitor }

constructor TSynMonitor.Create;
begin
  inherited Create;
  fTotalTime := TSynMonitorTime.Create;
  fLastTime := TSynMonitorOneTime.Create;
  fMinimalTime := TSynMonitorOneTime.Create;
  fAverageTime := TSynMonitorOneTime.Create;
  fMaximalTime := TSynMonitorOneTime.Create;
end;

constructor TSynMonitor.Create(const aName: RawUTF8);
begin
  Create;
  fName := aName;
end;

destructor TSynMonitor.Destroy;
begin
  fMaximalTime.Free;
  fAverageTime.Free;
  fMinimalTime.Free;
  fLastTime.Free;
  fTotalTime.Free;
  inherited Destroy;
end;

procedure TSynMonitor.Lock;
begin
  fSafe^.Lock;
end;

procedure TSynMonitor.UnLock;
begin
  fSafe^.UnLock;
end;

procedure TSynMonitor.Changed;
begin // do nothing by default - overriden classes may track modified changes
end;

procedure TSynMonitor.ProcessStart;
begin
  if fProcessing then
    raise ESynException.CreateUTF8('Reentrant %.ProcessStart', [self]);
  fSafe^.Lock;
  try
    InternalTimer.Resume;
    fTaskStatus := taskNotStarted;
    fProcessing := true;
  finally
    fSafe^.UnLock;
  end;
end;

procedure TSynMonitor.ProcessDoTask;
begin
  fSafe^.Lock;
  try
    inc(fTaskCount);
    fTaskStatus := taskStarted;
    Changed;
  finally
    fSafe^.UnLock;
  end;
end;

procedure TSynMonitor.ProcessStartTask;
begin
  if fProcessing then
    raise ESynException.CreateUTF8('Reentrant %.ProcessStart', [self]);
  fSafe^.Lock;
  try
    InternalTimer.Resume;
    fProcessing := true;
    inc(fTaskCount);
    fTaskStatus := taskStarted;
    Changed;
  finally
    fSafe^.UnLock;
  end;
end;

procedure TSynMonitor.ProcessEnd;
begin
  fSafe^.Lock;
  try
    InternalTimer.Pause;
    LockedFromProcessTimer;
  finally
    fSafe^.UnLock;
  end;
end;

procedure TSynMonitor.LockedFromProcessTimer;
begin
  fTotalTime.MicroSec := InternalTimer.TimeInMicroSec;
  if fTaskStatus = taskStarted then
  begin
    fLastTime.MicroSec := InternalTimer.LastTimeInMicroSec;
    if (fMinimalTime.MicroSec = 0) or
       (InternalTimer.LastTimeInMicroSec < fMinimalTime.MicroSec) then
      fMinimalTime.MicroSec := InternalTimer.LastTimeInMicroSec;
    if InternalTimer.LastTimeInMicroSec > fMaximalTime.MicroSec then
      fMaximalTime.MicroSec := InternalTimer.LastTimeInMicroSec;
    fTaskStatus := taskNotStarted;
  end;
  LockedPerSecProperties;
  fProcessing := false;
  Changed;
end;

function TSynMonitor.FromExternalQueryPerformanceCounters(const CounterDiff: QWord): QWord;
begin
  fSafe^.Lock;
  try // thread-safe ProcessStart+ProcessDoTask+ProcessEnd
    inc(fTaskCount);
    fTaskStatus := taskStarted;
    result := InternalTimer.FromExternalQueryPerformanceCounters(CounterDiff);
    LockedFromProcessTimer;
  finally
    fSafe^.UnLock;
  end;
end;

procedure TSynMonitor.FromExternalMicroSeconds(const MicroSecondsElapsed: QWord);
begin
  fSafe^.Lock;
  try // thread-safe ProcessStart+ProcessDoTask+ProcessEnd
    inc(fTaskCount);
    fTaskStatus := taskStarted;
    InternalTimer.FromExternalMicroSeconds(MicroSecondsElapsed);
    LockedFromProcessTimer;
  finally
    fSafe^.UnLock;
  end;
end;

class procedure TSynMonitor.InitializeObjArray(var ObjArr; Count: integer);
var
  i: integer;
begin
  ObjArrayClear(ObjArr);
  SetLength(TPointerDynArray(ObjArr), Count);
  for i := 0 to Count - 1 do
    TPointerDynArray(ObjArr)[i] := Create;
end;

procedure TSynMonitor.ProcessError(const info: variant);
begin
  fSafe^.Lock;
  try
    if not VarIsEmptyOrNull(info) then
      inc(fInternalErrors);
    fLastInternalError := info;
    Changed;
  finally
    fSafe^.UnLock;
  end;
end;

procedure TSynMonitor.ProcessErrorFmt(const Fmt: RawUTF8; const Args: array of const);
begin
  ProcessError(RawUTF8ToVariant(FormatUTF8(Fmt, Args)));
end;

procedure TSynMonitor.ProcessErrorRaised(E: Exception);
begin
  ProcessErrorFmt('%: %', [E, E.Message]);
end;

procedure TSynMonitor.ProcessErrorNumber(info: integer);
begin
  ProcessError(info);
end;

procedure TSynMonitor.LockedPerSecProperties;
begin
  if fTaskCount = 0 then
    exit; // avoid division per zero
  fPerSec := fTotalTime.PerSecond(fTaskCount);
  fAverageTime.MicroSec := fTotalTime.MicroSec div fTaskCount;
end;

procedure TSynMonitor.Sum(another: TSynMonitor);
begin
  if (self = nil) or (another = nil) then
    exit;
  fSafe^.Lock;
  another.fSafe^.Lock;
  try
    LockedSum(another);
  finally
    another.fSafe^.UnLock;
    fSafe^.UnLock;
  end;
end;

procedure TSynMonitor.LockedSum(another: TSynMonitor);
begin
  fTotalTime.MicroSec := fTotalTime.MicroSec + another.fTotalTime.MicroSec;
  if (fMinimalTime.MicroSec = 0) or (another.fMinimalTime.MicroSec < fMinimalTime.MicroSec) then
    fMinimalTime.MicroSec := another.fMinimalTime.MicroSec;
  if another.fMaximalTime.MicroSec > fMaximalTime.MicroSec then
    fMaximalTime.MicroSec := another.fMaximalTime.MicroSec;
  inc(fTaskCount, another.fTaskCount);
  if another.Processing then
    fProcessing := true; // if any thread is active, whole daemon is active
  inc(fInternalErrors, another.Errors);
end;

procedure TSynMonitor.WriteDetailsTo(W: TAbstractWriter);
begin
  fSafe^.Lock;
  try
    W.WriteObject(self);
  finally
    fSafe^.UnLock;
  end;
end;

procedure TSynMonitor.ComputeDetailsTo(W: TAbstractWriter);
begin
  fSafe^.Lock;
  try
    LockedPerSecProperties; // may not have been calculated after Sum()
    WriteDetailsTo(W);
  finally
    fSafe^.UnLock;
  end;
end;

function TSynMonitor.ComputeDetailsJSON: RawUTF8;
var
  W: TAbstractWriter;
  temp: TTextWriterStackBuffer;
begin
  W := DefaultTextWriterSerializer.CreateOwnedStream(temp);
  try
    ComputeDetailsTo(W);
    W.SetText(result);
  finally
    W.Free;
  end;
end;


{ TSynMonitorWithSize}

constructor TSynMonitorWithSize.Create;
begin
  inherited Create;
  fSize := TSynMonitorSize.Create({nospace=}false);
  fThroughput := TSynMonitorThroughput.Create({nospace=}false);
end;

destructor TSynMonitorWithSize.Destroy;
begin
  inherited Destroy;
  fThroughput.Free;
  fSize.Free;
end;

procedure TSynMonitorWithSize.LockedPerSecProperties;
begin
  inherited LockedPerSecProperties;
  fThroughput.BytesPerSec := fTotalTime.PerSecond(fSize.Bytes);
end;

procedure TSynMonitorWithSize.AddSize(const Bytes: QWord);
begin
  fSafe^.Lock;
  try
    fSize.Bytes := fSize.Bytes + Bytes;
  finally
    fSafe^.UnLock;
  end;
end;

procedure TSynMonitorWithSize.LockedSum(another: TSynMonitor);
begin
  inherited LockedSum(another);
  if another.InheritsFrom(TSynMonitorWithSize) then
    AddSize(TSynMonitorWithSize(another).Size.Bytes);
end;


{ TSynMonitorInputOutput }

constructor TSynMonitorInputOutput.Create;
begin
  inherited Create;
  fInput := TSynMonitorSize.Create({nospace=}false);
  fOutput := TSynMonitorSize.Create({nospace=}false);
  fInputThroughput := TSynMonitorThroughput.Create({nospace=}false);
  fOutputThroughput := TSynMonitorThroughput.Create({nospace=}false);
end;

destructor TSynMonitorInputOutput.Destroy;
begin
  fOutputThroughput.Free;
  fOutput.Free;
  fInputThroughput.Free;
  fInput.Free;
  inherited Destroy;
end;

procedure TSynMonitorInputOutput.LockedPerSecProperties;
begin
  inherited LockedPerSecProperties;
  fInputThroughput.BytesPerSec := fTotalTime.PerSecond(fInput.Bytes);
  fOutputThroughput.BytesPerSec := fTotalTime.PerSecond(fOutput.Bytes);
end;

procedure TSynMonitorInputOutput.AddSize(const Incoming, Outgoing: QWord);
begin
  fSafe^.Lock;
  try
    fInput.Bytes := fInput.Bytes + Incoming;
    fOutput.Bytes := fOutput.Bytes + Outgoing;
  finally
    fSafe^.UnLock;
  end;
end;

procedure TSynMonitorInputOutput.LockedSum(another: TSynMonitor);
begin
  inherited LockedSum(another);
  if another.InheritsFrom(TSynMonitorInputOutput) then
  begin
    fInput.Bytes := fInput.Bytes + TSynMonitorInputOutput(another).Input.Bytes;
    fOutput.Bytes := fOutput.Bytes + TSynMonitorInputOutput(another).Output.Bytes;
  end;
end;


{ TSynMonitorServer }

procedure TSynMonitorServer.ClientConnect;
begin
  if self = nil then
    exit;
  fSafe^.Lock;
  try
    inc(fClientsCurrent);
    if fClientsCurrent > fClientsMax then
      fClientsMax := fClientsCurrent;
    Changed;
  finally
    fSafe^.UnLock;
  end;
end;

procedure TSynMonitorServer.ClientDisconnect;
begin
  if self = nil then
    exit;
  fSafe^.Lock;
  try
    if fClientsCurrent > 0 then
      dec(fClientsCurrent);
    Changed;
  finally
    fSafe^.UnLock;
  end;
end;

procedure TSynMonitorServer.ClientDisconnectAll;
begin
  if self = nil then
    exit;
  fSafe^.Lock;
  try
    fClientsCurrent := 0;
    Changed;
  finally
    fSafe^.UnLock;
  end;
end;

function TSynMonitorServer.GetClientsCurrent: TSynMonitorOneCount;
begin
  if self = nil then
  begin
    result := 0;
    exit;
  end;
  fSafe^.Lock;
  try
    result := fClientsCurrent;
  finally
    fSafe^.UnLock;
  end;
end;

function TSynMonitorServer.AddCurrentRequestCount(diff: integer): integer;
begin
  if self = nil then
  begin
    result := 0;
    exit;
  end;
  fSafe^.Lock;
  try
    inc(fCurrentRequestCount, diff);
    result := fCurrentRequestCount;
  finally
    fSafe^.UnLock;
  end;
end;


end.


