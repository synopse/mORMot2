/// Framework Core Performance and Monitoring Classes
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.perf;

{
  *****************************************************************************

   Performance Monitoring functions shared by all framework units
    - Performance Counters
    - TSynMonitor Process Information Classes
    - TSynMonitorUsage Process Information Database Storage
    - Operating System Monitoring
    - DMI/SMBIOS Binary Decoder
    - TSynFpuException Wrapper for FPU Flags Preservation

  *****************************************************************************
}


interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  typinfo, // to please Delphi
  mormot.core.base,
  mormot.core.os,
  mormot.core.datetime,
  mormot.core.data,
  mormot.core.rtti,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.variants,
  mormot.core.json,
  mormot.core.log,
  mormot.lib.static; // for TFpuFlags



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
    smvUndefined,
    smvOneMicroSec,
    smvOneBytes,
    smvOneCount,
    smvBytesPerSec,
    smvMicroSec,
    smvBytes,
    smvCount,
    smvCount64);

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
    // - not necessary e.g. if TPrecisionTimer is defined as a class field
    procedure Init;
      {$ifdef HASINLINE}inline;{$endif}
    /// initialize and start the high resolution timer
    // - similar to Init + Resume
    procedure Start; overload;
    /// initialize and start the high resolution timer with a supplied timestamp
    // - if CurrentMicroSeconds is 0, will call QueryPerformanceMicroSeconds()
    procedure Start(CurrentMicroSeconds: Int64); overload;
    /// stop the timer, returning the total time elapsed as text
    // - with appended time resolution (us,ms,s) - from MicroSecToString()
    // - is just a wrapper around Pause + Time
    // - you can call Resume to continue adding time to this timer
    function Stop: TShort16;
      {$ifdef HASINLINE}inline;{$endif}
    /// stop the timer, returning the total time elapsed as microseconds
    // - is just a wrapper around Pause + Time
    // - you can call Resume to continue adding time to this timer
    function StopInMicroSec: TSynMonitorTotalMicroSec;
      {$ifdef HASINLINE}inline;{$endif}
    /// stop the timer, ready to continue its time measurement via Resume
    // - will also compute the global Time value
    // - do nothing if no previous Start/Resume call is pending
    procedure Pause;
    /// resume a paused timer, or start an initialized timer
    // - do nothing if no timer has been initialized or paused just before
    // - if the previous method called was Init, will act like Start
    // - if the previous method called was Pause, it will continue counting
    procedure Resume;
      {$ifdef HASINLINE}inline;{$endif}
    /// resume a paused timer until the method ends
    // - will internally create a TInterfaceObject class to let the compiler
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
      {$ifdef HASINLINE}inline;{$endif}
    /// compute the per second count
    function PerSec(const Count: QWord): QWord;
    /// compute the time elapsed by count, with appened time resolution (us,ms,s)
    function ByCount(Count: QWord): TShort16;
    /// returns e.g. '16.9 MB in 102.20ms i.e. 165.5 MB/s'
    function SizePerSec(Size: QWord): ShortString;
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
    property TimeInMicroSec: TSynMonitorTotalMicroSec
      read fTime write fTime;
    /// timing in micro seconds of the last process
    // - not to be used in normal code, but e.g. for custom performance analysis
    property LastTimeInMicroSec: TSynMonitorOneMicroSec
      read fLastTime write fLastTime;
    /// how many times the Pause method was called, i.e. the number of tasks
    // processeed
    property PauseCount: TSynMonitorCount
      read fPauseCount;
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
    function ByCount(Count: cardinal): RawUtf8;
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
    function ByCount(Count: cardinal): RawUtf8;
  end;



{ ************ TSynMonitor Process Information Classes }

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
    property MicroSec: TSynMonitorTotalMicroSec
      read fMicroSeconds write fMicroSeconds;
    /// micro seconds time elapsed, as '... us-ns-ms-s' text
    property Text: TShort16
      read GetAsText;
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
    property MicroSec: TSynMonitorOneMicroSec
      read fMicroSeconds write fMicroSeconds;
    /// micro seconds time elapsed, as '... us-ns-ms-s' text
    property Text: TShort16
      read GetAsText;
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
    property Bytes: TSynMonitorTotalBytes
      read fBytes write fBytes;
    /// number of bytes, as '... B-KB-MB-GB' text
    property Text: TShort16
      read GetAsText;
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
    property Bytes: TSynMonitorOneBytes
      read fBytes write fBytes;
    /// number of bytes, as '... B-KB-MB-GB' text
    property Text: TShort16
      read GetAsText;
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
    property BytesPerSec: QWord
      read fBytesPerSec write fBytesPerSec;
    /// number of bytes per second, as '... B-KB-MB-GB/s' text
    property Text: TShort16
      read GetAsText;
  end;

  /// a generic value object able to handle any task / process statistic
  // - base class shared e.g. for ORM, SOA or DDD, when a repeatable data
  // process is to be monitored
  // - this class is thread-safe for its methods, but you should call explicitly
  // non-rentrant Lock/UnLock to access its individual properties
  TSynMonitor = class(TSynPersistent)
  protected
    fSafe: TLightLock;
    fName: RawUtf8;
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
    // warning: lock-free Locked* virtual methods because LightLock is not reentrant
    procedure LockedProcessDoTask; virtual;
    procedure LockedPerSecProperties; virtual;
    procedure LockedFromProcessTimer; virtual;
    procedure LockedSum(another: TSynMonitor); virtual;
    procedure LockedFromExternalMicroSeconds(const MicroSecondsElapsed: QWord);
      {$ifdef HASINLINE} inline; {$endif}
    procedure LockedProcessError(const info: variant); virtual;
    procedure LockedProcessErrorInteger(info: integer);
    procedure LockedWriteDetailsTo(W: TTextWriter); virtual;
  public
    /// low-level high-precision timer instance
    InternalTimer: TPrecisionTimer;
    /// initialize the instance nested class properties
    // - you can specify identifier associated to this monitored resource
    // which would be used for TSynMonitorUsage persistence
    constructor Create(const aName: RawUtf8); reintroduce; overload; virtual;
    /// initialize the instance nested class properties
    constructor Create; overload; override;
    /// finalize the instance
    destructor Destroy; override;
    /// create Count instances of this actual class in the supplied ObjArr[]
    class procedure InitializeObjArray(var ObjArr; Count: integer); virtual;
    /// should be called when the process starts, to resume the internal timer
    // - this method is not thread-safe, due to the shared InternalTimer: use
    // an external TPrecisionTimer then FromExternalMicroSeconds()
    procedure ProcessStart; virtual;
    /// should be called each time a pending task is processed
    // - will increase the TaskCount property
    // - this method is not thread-safe, due to the shared InternalTimer: use
    // an external TPrecisionTimer then FromExternalMicroSeconds()
    procedure ProcessDoTask;
    /// should be called when the process starts, and a task is processed
    // - similar to ProcessStart + ProcessDoTask
    // - this method is not thread-safe, due to the shared InternalTimer: use
    // an external TPrecisionTimer then FromExternalMicroSeconds()
    procedure ProcessStartTask; virtual;
    /// should be called when an error occurred
    // - typical use is with ObjectToVariant(E,...) kind of information
    // - thread-safe method
    procedure ProcessError(const info: variant);
    /// should be called when an error occurred
    // - typical use is with a HTTP status, e.g. as ProcessError(Call.OutStatus)
    // - just a wraper around overloaded ProcessError(), so a thread-safe method
    procedure ProcessErrorNumber(info: integer);
    /// should be called when an error occurred
    // - just a wraper around overloaded ProcessError(), so a thread-safe method
    procedure ProcessErrorFmt(const Fmt: RawUtf8; const Args: array of const);
    /// should be called when an Exception occurred
    // - just a wraper around overloaded ProcessError(), so a thread-safe method
    procedure ProcessErrorRaised(E: Exception);
    /// should be called when the process stops, to pause the internal timer
    // - this method is not thread-safe, due to the shared InternalTimer: use
    // an external TPrecisionTimer then FromExternalMicroSeconds()
    procedure ProcessEnd; virtual;
    /// could be used to manage information average or sums
    // - thread-safe method calling LockedSum protected virtual method
    procedure Sum(another: TSynMonitor);
    /// returns a JSON content with all published properties information
    // - thread-safe method
    function ComputeDetailsJson: RawUtf8;
    /// appends a JSON content with all published properties information
    // - thread-safe method
    procedure ComputeDetailsTo(W: TTextWriter); virtual;
    /// returns a TDocVariant with all published properties information
    // - thread-safe method
    function ComputeDetails: variant;
    /// used to allow thread safe timing
    // - by default, the internal TPrecisionTimer is not thread safe: you can
    // use this method to update the timing from many threads
    // - if you use this method, ProcessStart, ProcessDoTask and ProcessEnd
    // methods are disallowed, and the global fTimer won't be used any more
    // - this method is to be used with an external timer for thread-safety
    procedure FromExternalMicroSeconds(const MicroSecondsElapsed: QWord);
    /// non-reentrant exclusive lock acquisition - wrap fSafe.Lock
    // - warning: this non-reentrant method would deadlock if called twice
    procedure Lock;
      {$ifdef HASINLINE} inline; {$endif}
    /// release the non-reentrant exclusive lock - wrap fSafe.UnLock
    procedure UnLock;
      {$ifdef HASINLINE} inline; {$endif}
    /// customize JSON Serialization to set woEnumSetsAsText for readibility
    function RttiBeforeWriteObject(W: TTextWriter;
      var Options: TTextWriterWriteObjectOptions): boolean; override;
    /// an identifier associated to this monitored resource
    // - is used e.g. for TSynMonitorUsage persistence/tracking
    property Name: RawUtf8
      read fName write fName;
  published
    /// indicates if this thread is currently working on some process
    property Processing: boolean
      read fProcessing write fProcessing;
    /// how many times the task was performed
    property TaskCount: TSynMonitorCount64
      read fTaskCount write fTaskCount;
    /// the whole time spend during all working process
    property TotalTime: TSynMonitorTime
      read fTotalTime;
    /// the time spend during the last task processing
    property LastTime: TSynMonitorOneTime
      read fLastTime;
    /// the lowest time spent during any working process
    property MinimalTime: TSynMonitorOneTime
      read fMinimalTime;
    /// the time spent in average during any working process
    property AverageTime: TSynMonitorOneTime
      read fAverageTime;
    /// the highest time spent during any working process
    property MaximalTime: TSynMonitorOneTime
      read fMaximalTime;
    /// average of how many tasks did occur per second
    property PerSec: QWord
      read fPerSec;
    /// how many errors did occur during the processing
    property Errors: TSynMonitorCount
      read fInternalErrors;
    /// information about the last error which occured during the processing
    property LastError: variant
      read fLastInternalError;
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
    procedure AddSize(const Bytes: QWord); overload;
    /// increase the internal size counter and the current timer
    // - thread-safe method
    procedure AddSize(const Bytes, MicroSecs: QWord); overload;
  published
    /// how many total data has been hanlded during all working process
    property Size: TSynMonitorSize
      read fSize;
    /// data processing bandwith, returned as B/KB/MB per second
    property Throughput: TSynMonitorThroughput
      read fThroughput;
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
    /// encapsulate AddSite + ProcessErrorNumber + FromExternalMicroSeconds
    procedure Notify(const Incoming, Outgoing, MicroSec: QWord; Status: integer);
  published
    /// how many data has been received
    property Input: TSynMonitorSize
      read fInput;
    /// how many data has been sent back
    property Output: TSynMonitorSize
      read fOutput;
    /// incoming data processing bandwith, returned as B/KB/MB per second
    property InputThroughput: TSynMonitorThroughput
      read fInputThroughput;
    /// outgoing data processing bandwith, returned as B/KB/MB per second
    property OutputThroughput: TSynMonitorThroughput
      read fOutputThroughput;
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
    // - diff is expected to be either 0, -1 or 1
    // - thread-safe method
    procedure AddCurrentRequestCount(diff: integer);
      {$ifdef HASINLINE} inline; {$endif}
  published
    /// current count of connected clients
    property ClientsCurrent: TSynMonitorOneCount
      read fClientsCurrent;
    /// max count of connected clients
    property ClientsMax: TSynMonitorOneCount
      read fClientsMax;
    /// how many concurrent requests are currently processed
    // - modified via AddCurrentRequestCount() in TRestServer.Uri()
    property CurrentRequestCount: integer
      read fCurrentRequestCount;
  end;

  /// a list of simple process statistics
  TSynMonitorObjArray = array of TSynMonitor;

  /// a list of data process statistics
  TSynMonitorWithSizeObjArray = array of TSynMonitorWithSize;

  /// a list of incoming/outgoing data process statistics
  TSynMonitorInputOutputObjArray = array of TSynMonitorInputOutput;

  /// class-reference type (metaclass) of a process statistic information
  TSynMonitorClass = class of TSynMonitor;


{ ************ TSynMonitorUsage Process Information Database Storage }

type
  /// the time periods covered by TSynMonitorUsage process
  // - defines the resolution of information computed and stored
  TSynMonitorUsageGranularity = (
    mugUndefined,
    mugMinute,
    mugHour,
    mugDay,
    mugMonth,
    mugYear);

  /// defines one or several time periods for TSynMonitorUsage process
  TSynMonitorUsageGranularities = set of TSynMonitorUsageGranularity;

  /// how the TSynMonitorUsage storage IDs are computed
  // - stored e.g. in TOrmMonitorUsage.ID primary key (after a shift)
  // - it follows a 23 bit pattern of hour (5 bit), day (5 bit), month (4 bit),
  // year (9 bit - starting at 2016) so that it is monotonic over time
  // - by default, will store the information using mugHour granularity (i.e.
  // values for the 60 minutes in a record), and pseudo-hours of 29, 30 and 31
  // (see USAGE_ID_HOURMARKER[]) will identify mugDay, mugMonth and mugYear
  // consolidated statistics
  // - it will therefore store up to 24*365+365+12+1 = 9138 records per year
  // in the associated storage engine (so there is no actual need to purge it)
  {$ifdef USERECORDWITHMETHODS}
  TSynMonitorUsageID = record
  {$else}
  TSynMonitorUsageID = object
  {$endif USERECORDWITHMETHODS}
  public
    /// the TID, as computed from time and granularity
    Value: integer;
    /// computes an ID corresponding to mugHour granularity of a given time
    // - minutes and seconds will be ignored
    // - mugHour granularity will store 0..59 information about each minute
    procedure From(Y, M, D, H: integer); overload;
    /// computes an ID corresponding to mugDay granularity of a given time
    // - hours, minutes and seconds will be merged
    // - mugDay granularity will store 0..23 information about each hour
    // - a pseudo hour of 29 (i.e. USAGE_ID_HOURMARKER[mugDay]) is used
    procedure From(Y, M, D: integer); overload;
    /// computes an ID corresponding to mugMonth granularity of a given time
    // - days, hours, minutes and seconds will be merged
    // - mugMonth granularity will store 0..31 information about each day
    // - a pseudo hour of 30 (i.e. USAGE_ID_HOURMARKER[mugMonth]) is used
    procedure From(Y, M: integer); overload;
    /// computes an ID corresponding to mugYear granularity of a given time
    // - months, days, hours, minutes and seconds will be merged
    // - mugYear granularity will store 0..11 information about each month
    // - a pseudo hour of 31 (i.e. USAGE_ID_HOURMARKER[mugYear]) is used
    procedure From(Y: integer); overload;
    /// computes an ID corresponding to a given time
    // - will set the ID with mugHour granularity, i.e. the information about
    // the given hour, stored as per minute 0..59 values
    // - minutes and seconds in supplied TimeLog value will therefore be ignored
    procedure FromTimeLog(const TimeLog: TTimeLog);
    /// computes an ID corresponding to the current UTC date/time
    // - minutes and seconds will be ignored
    procedure FromNowUtc;
    /// returns the date/time
    // - minutes and seconds will set to 0
    function ToTimeLog: TTimeLog;
    /// convert to Iso-8601 encoded text
    function Text(Expanded: boolean; FirstTimeChar: AnsiChar = 'T'): RawUtf8;
    /// retrieve the resolution of the stored information
    // - i.e. either mugHour, mugDay, mugMonth or mugYear, which will store
    // a true 0..23 hour value (for mugHour), or 29/30/31 pseudo-hour (i.e.
    // USAGE_ID_HOURMARKER[mugDay/mugMonth/mugYear])
    function Granularity: TSynMonitorUsageGranularity;
    /// change the resolution of the stored information
    procedure Truncate(gran: TSynMonitorUsageGranularity);
    /// low-level read of a time field stored in this ID, per granularity
    function GetTime(gran: TSynMonitorUsageGranularity;
      monthdaystartat0: boolean = false): integer;
      {$ifdef HASINLINE}inline;{$endif}
    /// low-level modification of a time field stored in this ID, per granularity
    procedure SetTime(gran: TSynMonitorUsageGranularity; aValue: integer);
  end;

  TSynMonitorUsageTrackProp = record
    Info: PRttiProp;
    /// property type, as recognized by MonitorPropUsageValue()
    Kind: TSynMonitorType;
    Name: RawUtf8;
    Values: array[mugHour..mugYear] of TInt64DynArray;
    ValueLast: Int64;
  end;

  TSynMonitorUsageTrackPropDynArray = array of TSynMonitorUsageTrackProp;

  TSynMonitorUsageTrack = record
    Instance: TObject;
    Name: RawUtf8;
    Props: TSynMonitorUsageTrackPropDynArray;
  end;

  PSynMonitorUsageTrackProp = ^TSynMonitorUsageTrackProp;
  PSynMonitorUsageTrack = ^TSynMonitorUsageTrack;

  /// define all known information about a given time
  // - may be the current time, or a former time
  TSynMonitorUsageLoad = array[mugHour..mugYear] of variant;

  /// abstract class to track, compute and store TSynMonitor detailed statistics
  // - you should inherit from this class to implement proper data persistence,
  // e.g. using TSynMonitorUsageRest for ORM-based storage
  // - SaveDB may take some time, so a TSynLocker OS lock is used, not TRWLock
  TSynMonitorUsage = class(TSynPersistentLock)
  protected
    fLog: TSynLogFamily;
    fTracked: array of TSynMonitorUsageTrack;
    fValues: TSynMonitorUsageLoad;
    fCustomWritePropGranularity: TSynMonitorUsageGranularity;
    fLastInstance: TObject;
    fLastTrack: PSynMonitorUsageTrack;
    fPrevious: TTimeLogBits;
    fComment: RawUtf8;
    function TrackPropLock(Instance: TObject;
      Info: PRttiProp): PSynMonitorUsageTrackProp;
    // those methods will be protected (e.g. in Modified) by fSafe.Lock:
    procedure SavePrevious(Scope: TSynMonitorUsageGranularity);
    procedure Save(ID: TSynMonitorUsageID; Gran, Scope: TSynMonitorUsageGranularity);
    function Load(const Time: TTimeLogBits): boolean;
    procedure LoadTrack(var Track: TSynMonitorUsageTrack);
    // should be overriden with proper persistence storage:
    function SaveDB(ID: integer; const Track: variant;
      Gran: TSynMonitorUsageGranularity): boolean; virtual; abstract;
    function LoadDB(ID: integer; Gran: TSynMonitorUsageGranularity;
      out Track: variant): boolean; virtual; abstract;
    // may be overriden for testing purposes
    procedure SetCurrentUtcTime(out minutes: TTimeLogBits); virtual;
  public
    /// finalize the statistics, saving any pending information
    destructor Destroy; override;
    /// track the values of one named object instance
    // - will recognize the TSynMonitor* properties as TSynMonitorType from
    // RTTI, using MonitorPropUsageValue(), within any (nested) object
    // - the instance will be stored in fTracked[].Instance: ensure it will
    // stay available during the whole TSynMonitorUsage process
    function Track(Instance: TObject;
      const Name: RawUtf8 = ''): integer; overload; virtual;
    /// track the values of the given object instances
    // - will recognize the TSynMonitor* properties as TSynMonitorType from
    // RTTI, using MonitorPropUsageValue(), within any (nested) object
    // - instances will be stored in fTracked[].Instance: ensure they will
    // stay available during the whole TSynMonitorUsage process
    procedure Track(const Instances: array of TSynMonitor); overload;
    /// to be called when tracked properties changed on a tracked class instance
    function Modified(Instance: TObject): integer; overload;
    /// to be called when tracked properties changed on a tracked class instance
    function Modified(Instance: TObject; const PropNames: array of RawUtf8;
      ModificationTime: TTimeLog = 0): integer; overload; virtual;
    /// some custom text, associated with the current stored state
    // - will be persistented by Save() methods
    property Comment: RawUtf8
      read fComment write fComment;
  end;

const
  USAGE_VALUE_LEN: array[mugHour..mugYear] of integer = (
    60, 24, 31, 12);
  USAGE_ID_SHIFT: array[mugHour..mugYear] of byte = (
    0, 5, 10, 14);
  USAGE_ID_BITS: array[mugHour..mugYear] of byte = (
    5, 5, 4, 9);
  USAGE_ID_MASK: array[mugHour..mugYear] of integer = (
    31, 31, 15, 511);
  USAGE_ID_MAX: array[mugHour..mugYear] of cardinal = (
    23, 30, 11, 127);
  USAGE_ID_HOURMARKER: array[mugDay..mugYear] of integer = (
    29, 30, 31);
  USAGE_ID_YEAROFFSET = 2016;

  /// kind of "cumulative" TSynMonitorType stored in TSynMonitor / TSynMonitorUsage
  // - those properties will have their values reset for each granularity level
  // - will recognize TSynMonitorTotalMicroSec, TSynMonitorTotalBytes,
  // TSynMonitorOneBytes, TSynMonitorBytesPerSec, TSynMonitorCount and
  // TSynMonitorCount64 types
  SYNMONITORVALUE_CUMULATIVE =
    [smvMicroSec, smvBytes, smvCount, smvCount64];


/// guess the kind of value stored in a TSynMonitor / TSynMonitorUsage property
// - will recognize TSynMonitorTotalMicroSec, TSynMonitorOneMicroSec,
// TSynMonitorTotalBytes, TSynMonitorOneBytes, TSynMonitorBytesPerSec,
// TSynMonitorCount and TSynMonitorCount64 types from supplied RTTI
function MonitorPropUsageValue(info: PRttiProp): TSynMonitorType;

function ToText(gran: TSynMonitorUsageGranularity): PShortString; overload;



{ ************ Operating System Monitoring }

type
  /// event handler which may be executed by TSystemUse.BackgroundExecute
  // - called just after the measurement of each process CPU and RAM consumption
  // - run from the background thread, so should not directly make VCL calls,
  // unless BackgroundExecute is run from a VCL timer
  TOnSystemUseMeasured = procedure(ProcessID: integer;
    const Data: TSystemUseData) of object;

  /// internal storage of CPU and RAM usage for one process
  TSystemUseProcess = record
    ID: integer;
    Data: TSystemUseDataDynArray;
    PrevKernel: Int64;
    PrevUser: Int64;
  end;

  /// internal storage of CPU and RAM usage for a set of processes
  TSystemUseProcessDynArray = array of TSystemUseProcess;

  /// monitor CPU and RAM usage of one or several processes
  // - you should execute BackgroundExecute on a regular pace (e.g. every second)
  // to gather low-level CPU and RAM information for the given set of processes
  // - is able to keep an history of latest sample values
  // - use Current class function to access a process-wide instance
  TSystemUse = class(TSynPersistentRWLightLock)
  protected
    fProcess: TSystemUseProcessDynArray;
    fProcesses: TDynArray;
    fDataIndex: integer;
    fProcessInfo: TProcessInfo;
    fHistoryDepth: integer;
    fOnMeasured: TOnSystemUseMeasured;
    fTimer: TObject;
    fUnsubscribeProcessOnAccessError: boolean;
    function LockedProcessIndex(aProcessID: integer): PtrInt;
  public
    /// a VCL's TTimer.OnTimer compatible event
    // - to be run every few seconds and retrieve the CPU and RAM use:
    // ! tmrSystemUse.Interval := 10000; // every 10 seconds
    // ! tmrSystemUse.OnTimer := TSystemUse.Current.OnTimerExecute;
    /// - could also be run from a TSynBackgroundTimer instance
    procedure OnTimerExecute(Sender: TObject);
    /// track the CPU and RAM usage of the supplied set of Process ID
    // - any aProcessID[]=0 will be replaced by the current process ID
    // - you can specify the number of sample values for the History() method
    // - you should then execute the BackgroundExecute method of this instance
    // in a VCL timer or from a TSynBackgroundTimer.Enable() registration
    constructor Create(const aProcessID: array of integer;
      aHistoryDepth: integer = 60); reintroduce; overload; virtual;
    /// track the CPU and RAM usage of the current process
    // - you can specify the number of sample values for the History() method
    // - you should then execute the BackgroundExecute method of this instance
    // in a VCL timer or from a TSynBackgroundTimer.Enable() registration
    constructor Create(aHistoryDepth: integer = 60); reintroduce; overload; virtual;
    /// add a Process ID to the internal tracking list
    procedure Subscribe(aProcessID: integer);
    /// remove a Process ID from the internal tracking list
    function Unsubscribe(aProcessID: integer): boolean;
    /// returns the total (Kernel+User) CPU usage percent of the supplied process
    // - aProcessID=0 will return information from the current process
    // - returns -1 if the Process ID was not registered via Create/Subscribe
    function Percent(aProcessID: integer = 0): single; overload;
    /// returns the Kernel-space CPU usage percent of the supplied process
    // - aProcessID=0 will return information from the current process
    // - returns -1 if the Process ID was not registered via Create/Subscribe
    function PercentKernel(aProcessID: integer = 0): single; overload;
    /// returns the User-space CPU usage percent of the supplied process
    // - aProcessID=0 will return information from the current process
    // - returns -1 if the Process ID was not registered via Create/Subscribe
    function PercentUser(aProcessID: integer = 0): single; overload;
    /// returns the total (Work+Paged) RAM use of the supplied process, in KB
    // - aProcessID=0 will return information from the current process
    // - returns 0 if the Process ID was not registered via Create/Subscribe
    function KB(aProcessID: integer = 0): cardinal; overload;
    /// percent of current Idle/Kernel/User CPU usage for all processes
    function PercentSystem(out Idle, Kernel, User: single): boolean;
    /// returns the detailed CPU and RAM usage percent of the supplied process
    // - aProcessID=0 will return information from the current process
    // - returns -1 if the Process ID was not registered via Create/Subscribe
    function Data(out aData: TSystemUseData; aProcessID: integer = 0): boolean; overload;
    /// returns the detailed CPU and RAM usage percent of the supplied process
    // - aProcessID=0 will return information from the current process
    // - returns Timestamp=0 if the Process ID was not registered via Create/Subscribe
    function Data(aProcessID: integer = 0): TSystemUseData; overload;
    /// returns total (Kernel+User) CPU usage percent history of the supplied process
    // - aProcessID=0 will return information from the current process
    // - returns nil if the Process ID was not registered via Create/Subscribe
    // - returns the sample values as an array, starting from the last to the oldest
    // - you can customize the maximum depth, with aDepth < HistoryDepth
    function History(aProcessID: integer = 0; aDepth: integer = 0): TSingleDynArray; overload;
    /// returns total (Kernel+User) CPU usage percent history of the supplied
    // process, as a string of two digits values
    // - aProcessID=0 will return information from the current process
    // - returns '' if the Process ID was not registered via Create/Subscribe
    // - you can customize the maximum depth, with aDepth < HistoryDepth
    // - the memory history (in MB) can be optionally returned in aDestMemoryMB
    // - on POSIX, will call RetrieveLoadAvg function for system-wide info
    function HistoryText(aProcessID: integer = 0; aDepth: integer = 0;
      aDestMemoryMB: PRawUtf8 = nil): RawUtf8;
    /// returns total (Kernel+User) CPU usage percent history of the supplied process
    // - aProcessID=0 will return information from the current process
    // - returns null if the Process ID was not registered via Create/Subscribe
    // - returns the sample values as a TDocVariant array, starting from the
    // last to the oldest, with two digits precision (as currency values)
    // - you can customize the maximum depth, with aDepth < HistoryDepth
    function HistoryVariant(aProcessID: integer = 0; aDepth: integer = 0): variant;
    /// access to a global instance, corresponding to the current process
    // - its HistoryDepth will be of 60 items
    class function Current(aCreateIfNone: boolean = true): TSystemUse;
    /// returns detailed CPU and RAM usage history of the supplied process
    // - aProcessID=0 will return information from the current process
    // - returns nil if the Process ID was not registered via Create/Subscribe
    // - returns the sample values as an array, starting from the last to the oldest
    // - you can customize the maximum depth, with aDepth < HistoryDepth
    function HistoryData(aProcessID: integer = 0;
      aDepth: integer = 0): TSystemUseDataDynArray; overload;
    /// if any unexisting (e.g. closed/killed) process should be unregistered
    // - e.g. if OpenProcess() API call fails
    property UnsubscribeProcessOnAccessError: boolean
      read fUnsubscribeProcessOnAccessError write fUnsubscribeProcessOnAccessError;
    /// how many items are stored internally, and returned by the History() method
    property HistoryDepth: integer
      read fHistoryDepth;
    /// executed when TSystemUse.BackgroundExecute finished its measurement
    property OnMeasured: TOnSystemUseMeasured
      read fOnMeasured write fOnMeasured;
    /// low-level access to the associated timer running BackgroundExecute
    // - equals nil if has been associated to no timer
    // - holds e.g. a TRestBackgroundTimer from TRest.SystemUseTrack
    property Timer: TObject
      read fTimer write fTimer;
  end;

  /// value object able to gather information about the current system memory
  TSynMonitorMemory = class(TSynPersistent)
  protected
    FAllocatedUsed: TSynMonitorOneSize;
    FAllocatedReserved: TSynMonitorOneSize;
    FMemoryLoadPercent: integer;
    FPhysicalMemoryFree: TSynMonitorOneSize;
    FVirtualMemoryFree: TSynMonitorOneSize;
    FPagingFileTotal: TSynMonitorOneSize;
    FPhysicalMemoryTotal: TSynMonitorOneSize;
    FVirtualMemoryTotal: TSynMonitorOneSize;
    FPagingFileFree: TSynMonitorOneSize;
    fLastMemoryInfoRetrievedTix: cardinal;
    procedure RetrieveMemoryInfo; virtual;
    function GetAllocatedUsed: TSynMonitorOneSize;
    function GetAllocatedReserved: TSynMonitorOneSize;
    function GetMemoryLoadPercent: integer;
    function GetPagingFileFree: TSynMonitorOneSize;
    function GetPagingFileTotal: TSynMonitorOneSize;
    function GetPhysicalMemoryFree: TSynMonitorOneSize;
    function GetPhysicalMemoryTotal: TSynMonitorOneSize;
    function GetVirtualMemoryFree: TSynMonitorOneSize;
    function GetVirtualMemoryTotal: TSynMonitorOneSize;
  public
    /// initialize the class, and its nested TSynMonitorOneSize instances
    constructor Create(aTextNoSpace: boolean); reintroduce;
    /// finalize the class, and its nested TSynMonitorOneSize instances
    destructor Destroy; override;
    /// some text corresponding to current 'free/total' memory information
    // - returns e.g. '10.3 GB / 15.6 GB'
    class function FreeAsText(nospace: boolean = false;
      processfree: PRawUtf8 = nil): ShortString;
    /// how many physical memory is currently installed, as text (e.g. '32 GB');
    class function PhysicalAsText(nospace: boolean = false): TShort16;
    /// returns a JSON object with the current system memory information
    // - numbers would be given in KB (Bytes shl 10)
    class function ToJson: RawUtf8;
    /// fill a TDocVariant with the current system memory information
    // - numbers would be given in KB (Bytes shl 10)
    class function ToVariant: variant;
  published
    /// Total of allocated memory used by the program
    property AllocatedUsed: TSynMonitorOneSize
      read GetAllocatedUsed;
    /// Total of allocated memory reserved by the program
    property AllocatedReserved: TSynMonitorOneSize
      read GetAllocatedReserved;
    /// Percent of memory in use for the system
    property MemoryLoadPercent: integer
      read GetMemoryLoadPercent;
    /// Total of physical memory for the system
    property PhysicalMemoryTotal: TSynMonitorOneSize
      read GetPhysicalMemoryTotal;
    /// Free of physical memory for the system
    property PhysicalMemoryFree: TSynMonitorOneSize
      read GetPhysicalMemoryFree;
    /// Total of paging file for the system
    property PagingFileTotal: TSynMonitorOneSize
      read GetPagingFileTotal;
    /// Free of paging file for the system
    property PagingFileFree: TSynMonitorOneSize
      read GetPagingFileFree;
    {$ifdef OSWINDOWS}
    /// Total of virtual memory for the system
    // - property not defined under Linux, since not applying to this OS
    property VirtualMemoryTotal: TSynMonitorOneSize
      read GetVirtualMemoryTotal;
    /// Free of virtual memory for the system
    // - property not defined under Linux, since not applying to this OS
    property VirtualMemoryFree: TSynMonitorOneSize
      read GetVirtualMemoryFree;
    {$endif OSWINDOWS}
  end;

  /// value object able to gather information about a system drive
  TSynMonitorDisk = class(TSynPersistent)
  protected
    fName: TFileName;
    {$ifdef OSWINDOWS}
    fVolumeName: SynUnicode;
    {$endif OSWINDOWS}
    fAvailableSize: TSynMonitorOneSize;
    fFreeSize: TSynMonitorOneSize;
    fTotalSize: TSynMonitorOneSize;
    fLastDiskInfoRetrievedTix: cardinal;
    procedure RetrieveDiskInfo; virtual;
    function GetName: TFileName;
    function GetAvailable: TSynMonitorOneSize;
    function GetFree: TSynMonitorOneSize;
    function GetTotal: TSynMonitorOneSize;
  public
    /// initialize the class, and its nested TSynMonitorOneSize instances
    constructor Create; override;
    /// finalize the class, and its nested TSynMonitorOneSize instances
    destructor Destroy; override;
    /// some text corresponding to current 'free/total' disk information
    // - could return e.g. 'D: 64.4 GB / 213.4 GB'
    class function FreeAsText: RawUtf8;
  published
    /// the disk name
    property Name: TFileName
      read GetName;
    {$ifdef OSWINDOWS}
    /// the volume name (only available on Windows)
    property VolumeName: SynUnicode
      read fVolumeName write fVolumeName;
    /// space currently available on this disk for the current user
    // - may be less then FreeSize, if user quotas are specified (only taken
    // into account under Windows: on POSIX, AvailableSize=FreeSize)
    property AvailableSize: TSynMonitorOneSize
      read GetAvailable;
    {$endif OSWINDOWS}
    /// free space currently available on this disk
    property FreeSize: TSynMonitorOneSize
      read GetFree;
    /// total space
    property TotalSize: TSynMonitorOneSize
      read GetTotal;
  end;


/// convert Intel CPU features as plain CSV text
function ToText(const aIntelCPUFeatures: TIntelCpuFeatures;
  const Sep: RawUtf8 = ','): RawUtf8; overload;

/// convert ARM 32-bit CPU features as plain CSV text
function ToText(const aArm32CPUFeatures: TArm32HwCaps;
  const Sep: RawUtf8 = ','): RawUtf8; overload;

/// convert ARM 64-bit CPU features as plain CSV text
function ToText(const aArm64CPUFeatures: TArm64HwCaps;
  const Sep: RawUtf8 = ','): RawUtf8; overload;

/// contains the current CPU Features as space-separated text
// - computed from CpuFeatures set for Intel/AMD or ARM 32-bit/64-bit
// - contains the Flags: or Features: value of Linux /proc/cpuinfo otherwise
// (less accurate than our CpuFeatures set on older kernel)
var
  CpuFeaturesText: RawUtf8;

/// retrieve low-level information about all mounted disk partitions as text
// - returns e.g. under Linux
// '/ /dev/sda3 (19 GB), /boot /dev/sda2 (486.8 MB), /home /dev/sda4 (0.9 TB)'
// or under Windows 'C:\ System (115 GB), D:\ Data (99.3 GB)'
// - uses internally a cache unless nocache is true
// - includes the free space if withfreespace is true - e.g. '(80 GB / 115 GB)'
function GetDiskPartitionsText(nocache: boolean = false;
  withfreespace: boolean = false; nospace: boolean = false;
  nomount: boolean = false): RawUtf8;

/// returns a JSON object containing basic information about the computer
// - including Host, User, CPU, OS, freemem, freedisk...
function SystemInfoJson: RawUtf8;

/// returns a TDocVariant array of the latest intercepted exception texts
// - runs ToText() over all information returned by overloaded GetLastExceptions
// - defined in this unit to have TDocVariant at hand
function GetLastExceptions(Depth: integer = 0): variant; overload;


{ ************ DMI/SMBIOS Binary Decoder }

type
  TSmbiosBiosFlags = set of (
    sbfReserved0,
    sbfReserved1,
    sbfUnknonwn,
    sbfUnsupported,
    sbfIsa,
    sbfMca,
    sbfEisa,
    sbfPci,
    sbfPcmcia,
    sbfPlugAndPlay,
    sbfApm,
    sbfUpgradable,
    sbfShadowable,
    sbfVesa,
    sbfEscd,
    sbfBootCd,
    sbfBootSelectable,
    sbfRomSocketed,
    sbfBootPcmcia,
    sbfEdd,
    sbfNec12,
    sbfToshiba12,
    sbf525i360,
    sbf525i12,
    sbf35i720,
    sbf35i288,
    sbfInt5,
    sbfInt9,
    sbfInt14,
    sbfInt17,
    sbfInt10,
    sbfNecPc98,
    sbfAcpi,
    sbfUsbLegacy,
    sbfAgp,
    sbfBootI2O,
    sbfBootLS120,
    sbfBootZip,
    sbfBoot1394,
    sbfSmartBattery,
    sbfBootBiosSpecification,
    sbfBootKeyInitiatedNetwork,
    sbfTargetDistrib,
    sbfUefi,
    sbfVirtualMachine,
    sbfManufacturingModeSupported,
    sbfManufacturingModeEnabled,
    sbfR47,
    sbfR48,
    sbfR49,
    sbfR50,
    sbfR51,
    sbfR52,
    sbfR53,
    sbfR54,
    sbfR55,
    sbfR56,
    sbfR57,
    sbfR58,
    sbfR59
  );

  TSmbiosSystemWakeup = (
    sswReserved,
    sswOther,
    sswUnknown,
    sswApmTimer,
    sswModemRing,
    sswLanRemote,
    sswPowerSwitch,
    sswPciPme,
    sswAcPowerRestored
  );

  TSmbiosBoardFeatures = set of (
    sbfHostingBoard,
    sbfRequiresDaugtherBoard,
    sbfRemovable,
    sbfReplaceable,
    sbfSwappable
  );

  TSmbiosBoardType = (
    sbtUndefined,
    sbtUnknown,
    sbtOther,
    sbtServerBlade,
    sbtConnectivitySwitch,
    sbtSystemManagementModule,
    sbtProcessorModule,
    sbtIOModule,
    sbtMemoryModule,
    sbtDaugtherBoard,
    sbtMotherBoard,
    sbtProcessorMemoryModule,
    sbtProcessorIOModule,
    sbtInterconnectBoard
  );

  /// BIOS Information (Type 0) structure
  TSmbiosBios = packed record
    /// 2.0+ BIOS Vendor's Name (n)
    VendorName: RawUtf8;
    /// 2.0+ BIOS Version text (v)
    Version: RawUtf8;
    /// 2.0+ BIOS Release Date (b)
    BuildDate: RawUtf8;
    /// 2.0/2.4+ ROM Size as text (s)
    RomSize: RawUtf8;
    /// 2.4+ release version of the BIOS as text (r)
    Release: RawUtf8;
    /// 2.4+ release version of the embedded controller firmware (f)
    Firmware: RawUtf8;
    /// general features identified by this BIOS (c)
    Characteristics: TSmbiosBiosFlags;
  end;

  /// System Information (Type 1) structure
  TSmbiosSystem = packed record
    /// 2.0+ Manufacturer (m)
    Manufacturer: RawUtf8;
    /// 2.0+ Product Name (p)
    ProductName: RawUtf8;
    /// 2.0+ Version (v)
    Version: RawUtf8;
    /// 2.0+ Serial Number (s)
    Serial: RawUtf8;
    /// 2.1+ Universal Unique Identifier (u)
    UUID: RawUtf8;
    /// 2.4+ Product ID or Purchase Order Number, i.e. Sale identifier (k)
    SKU: RawUtf8;
    /// 2.4+ Computer Family, with similar branding and cosmetic features (f)
    Family: RawUtf8;
    /// 2.1+ Identifies the event that caused the system to power up (w)
    WakupType: TSmbiosSystemWakeup;
  end;

  /// Baseboard (or Module) Information (Type 2) structure (m)
  TSmbiosBoard = packed record
    /// Manufacturer (m)
    Manufacturer: RawUtf8;
    /// Product (p)
    Product: RawUtf8;
    /// Version (v)
    Version: RawUtf8;
    /// Serial Number (s)
    Serial: RawUtf8;
    /// Asset Tag (a)
    AssetTag: RawUtf8;
    /// Location within the BaseBoard Chassis (l)
    Location: RawUtf8;
    /// Feature flags (f)
    Features: TSmbiosBoardFeatures;
    /// Type of Board (t)
    BoardType: TSmbiosBoardType;
    /// Other structures contained in this Baseboard (c)
    //Contains: array of TSmbiosHandle;
  end;

  TSmbiosChassisType = (
    sctUndefined,
    sctOther,
    sctUnknown,
    sctDesktop,
    sctLowProfileDesktop,
    sctPizzaBox,
    sctMiniTower,
    sctTower,
    sctPortable,
    sctLaptop,
    sctNotebook,
    sctHandHeld,
    sctDockingStation,
    sctAllInOne,
    sctSubNotebook,
    sctSpaceSaving,
    sctLunchBox,
    sctMainServer,
    sctExpansion,
    sctSubChassis,
    sctBusExpansion,
    sctPeripheral,
    sctRaid,
    sctRackMount,
    sctSealedCase,
    sctMultiSystem,
    sctCompactPci,
    sctAdvancedTca,
    sctBlade,
    sctBladeEnclosure,
    sctTablet,
    sctConvertible,
    sctDetachable,
    sctIoTGateway,
    sctEmbeddedPC,
    sctMiniPC,
    sctStickPC
  );

  TSmbiosChassisState = (
    scsUndefined,
    scsOther,
    scsUnknown,
    scsSafe,
    scsWarning,
    scsCritical,
    scsNonRecoverable
  );

  TSmbiosChassisSecurityState = (
    scssUndefined,
    scssOther,
    scssUnknown,
    scssNone,
    scssExternalInterfaceLockedOut,
    scssExternalInterfaceEnabled
  );

  /// System Enclosure or Chassis (Type 3) structure
  TSmbiosChassis = packed record
    /// 2.0+ Chassis Lock (l)
    Lock: boolean;
    /// 2.0+ Chassis Type (t)
    ChassisType: TSmbiosChassisType;
    /// 2.0+ Manufacturer (m)
    Manufacturer: RawUtf8;
    /// 2.0+ Version (v)
    Version: RawUtf8;
    /// 2.0+ Serial Number (s)
    Serial: RawUtf8;
    /// 2.0+ Asset Tag (a)
    AssetTag: RawUtf8;
    /// 2.1+ State of the Chassis when it was last booted (b)
    BootUpState: TSmbiosChassisState;
    /// 2.1+ State of the Power Supply when the Chassis was last booted (w)
    PowerState: TSmbiosChassisState;
    /// 2.1+ Thermal State of the Chassis when it was last booted (h)
    ThermalState: TSmbiosChassisState;
    /// 2.1+ Physical Security Status (p)
    Security: TSmbiosChassisSecurityState;
    /// 2.3+ OEM-defined vendor-specific 32-bit information (o)
    OEM: cardinal;
    /// 2.3+ Height of the enclosure, in "U" units, 0 means unspecified (u)
    Height: byte;
    /// 2.3+ Number of associated power cords, 0 means unspecified (c)
    PowerCords: byte;
  end;

  TSmbiosProcessorType = (
    sptUndefined,
    sptOther,
    sptUnknown,
    sptCentral,
    sptMath,
    sptDsp,
    sptVideo
  );

  TSmbiosProcessorStatus = (
    spsUnknown,
    spsEnabled,
    spsDisabledByUserInBios,
    spsDisabledByBiosOnError,
    spsIdle
  );

  TSmbiosProcessorFlags = set of (
    spfReserved,
    spfUnknown,
    spf64Bit,
    spfMultiCore,
    spfHardwareThread,
    spfNXSupport,
    spfVirtSupport,
    spfPowerControl,
    spf128Bit,
    spfArm64SocID
  );

  TSmbiosProcessorUpgrade = (
    spuReserved,
    spuOther,
    spuUnknown,
    spuDaughterBoard,
    spuZIF,
    spuReplaceablePiggyBack,
    spuNone,
    spuLIF,
    spuSlot1,
    spuSlot2,
    spu370pin,
    spuSlotA,
    spuSlotM,
    spu423,
    spuA462,
    spu478,
    spu754,
    spu940,
    spu939,
    spumPGA604,
    spuLGA771,
    spuLGA775,
    spuS1,
    spuAM2,
    spuF1207,
    spuLGA1366,
    spuG34,
    spuAM3,
    spuC32,
    spuLGA1156,
    spuLGA1567,
    spuPGA988A,
    spuBGA1288,
    spurPGA988B,
    spuBGA1023,
    spuBGA1224,
    spuBGA1155,
    spuLGA1356,
    spuLGA2011,
    spuFS1,
    spuFS2,
    spuFM1,
    spuFM2,
    spuLGA2011_3,
    spuLGA1356_3,
    spuLGA1150,
    spuBGA1168,
    spuBGA1234,
    spuBGA1364,
    spuAM4,
    spuLGA1151,
    spuBGA1356,
    spuBGA1440,
    spuBGA1515,
    spuLGA3647_1,
    spuSP3,
    spuSP3r2,
    spuLGA2066,
    spuBGA1392,
    spuBGA1510,
    spuBGA1528,
    spuLGA4189,
    spuLGA1200,
    spuLGA4677,
    spuLGA1700,
    spuBGA1744,
    spuBGA1781,
    spuBGA1211,
    spuBGA2422,
    spuLGA1211,
    spuLGA2422,
    spuLGA5773,
    spuBGA5773
  );

  TSmbiosCacheLocation = (
    sclInternal,
    sclExternal,
    sclReserved,
    sclUnknown
  );

  TSmbiosCacheMode = (
    scmWriteThrough,
    scmWriteBack,
    scmVariesWithAddress,
    scmUnknown
  );

  TSmbiosCacheSramType = set of (
    sstSramOther,
    sstSramUnknown,
    sstNonBurst,
    sstBurst,
    sstPipelineBurst,
    sstSynchronous,
    sstAsynchronous
  );

  TSmbiosCacheEcc = (
    sceUndefined,
    sceUnknown,
    sceOther,
    sceNone,
    sceParity,
    sceSingleBitEcc,
    sceMultiBitEcc
  );

  TSmbiosCacheType = (
    sctUndefinedCache,
    sctOtherCache,
    sctNotKnown,
    sctInstruction,
    sctData,
    sctUnified
  );

  TSmbiosCacheAssociativity = (
    scaUndefined,
    scaOther,
    scaUnknown,
    scaDirectMapped,
    sca2way,
    sca4way,
    scaFully,
    sca8way,
    sca16way,
    sca12way,
    sca24way,
    sca32way,
    sca48way,
    sca64way,
    sca20way
  );

  /// Cache Information (Type 7) structure
  TSmbiosCache = packed record
    /// low-level handle used for linking into TSmbiosProcessor
    Handle: word;
    /// 2.0+  Reference Designation of this Socket (d)
    SocketDesignation: RawUtf8;
    /// 2.0+ Cache Level 1-8 (v)
    Level: byte;
    /// 2.0+ if Cache is Enabled (b)
    Enabled: boolean;
    /// 2.0+ if Cache is Socketed (k)
    Socketed: boolean;
    /// 2.0+ Location, relative to the CPU module (l)
    Location: TSmbiosCacheLocation;
    /// 2.0+ Operational Mode (o)
    OperationalMode: TSmbiosCacheMode;
    /// 2.0+/3.1+ Installed Size in bytes (s)
    Size: RawUtf8;
    /// 2.0+/3.1+ Maxium Size in bytes (m)
    MaxSize: RawUtf8;
    /// 2.0+ Current SRAM type (c)
    Sram: TSmbiosCacheSramType;
    /// 2.0+ Supported SRAM type (r)
    SuportedSram: TSmbiosCacheSramType;
    /// 2.1+ Speed in nanoseconds (n)
    Speed: byte;
    /// 2.1+ Error Correction Type (e)
    Ecc: TSmbiosCacheEcc;
    /// 2.1+ System Cache Type (t)
    CacheType: TSmbiosCacheType;
    /// 2.1+ Associativity (a)
    Associativity: TSmbiosCacheAssociativity;
  end;

  /// Processor Information (Type 4) structure
  TSmbiosProcessor = packed record
    /// 2.0+  Reference Designation of this Socket (d)
    SocketDesignation: RawUtf8;
    /// 2.0+ Processor type (t)
    ProcessorType: TSmbiosProcessorType;
    /// 2.0+/2.6+ Processor family (f)
    // - we use an ordinal and not an enumerate because there are too much types
    Family: cardinal;
    /// 2.0+ Processor 64-bit ID (i)
    // - we don't parse the Intel CpuID flags, because only 32-bit values are
    // truncated here so are much less than all TIntelCpuFeatures information
    ID: RawUtf8;
    /// 2.0+ Manufacturer (m)
    Manufacturer: RawUtf8;
    /// 2.0+ Version (v)
    Version: RawUtf8;
    /// 2.0+ Voltage (g)
    Voltage: RawUtf8;
    /// 2.0+ CPU Status (u)
    Status: TSmbiosProcessorStatus;
    /// 2.0+ CPU socket populated (l)
    Populated: boolean;
    /// 2.0+ Socket Upgrade (p)
    Upgrade: TSmbiosProcessorUpgrade;
    /// 2.1+ L1 Cache (1)
    L1Cache: TSmbiosCache;
    /// 2.1+ L2 Cache (2)
    L2Cache: TSmbiosCache;
    /// 2.1+ L3 Cache (3)
    L3Cache: TSmbiosCache;
    /// 2.3+ Serial Number (s)
    Serial: RawUtf8;
    /// 2.3+ Asset Tag (a)
    AssetTag: RawUtf8;
    /// 2.3+ Part Number (n)
    PartNumber: RawUtf8;
    /// 2.5+/3.0+ Number of Core per Socket (c)
    CoreCount: cardinal;
    /// 2.5+/3.0+ Number of Enabled Cores per Socket (e)
    CoreEnabled: cardinal;
    /// 2.5+/3.0+ Number of Thread Count per Socket (r)
    ThreadCount: cardinal;
    /// 3.6+ Number of Enabled Threads per Socket (b)
    ThreadEnabled: cardinal;
    /// 2.5+ Processor Characteristics (h)
    Flags: TSmbiosProcessorFlags;
  end;

  TSmbiosConnectorType = (
    sctNone,
    sctCentronics,
    sctMiniCentronics,
    sctProprietary,
    sctDB25M,
    sctDB25F,
    sctDB15M,
    sctDB15F,
    sctDB9M,
    sctDB9F,
    sctRJ11,
    sctRJ45,
    sct50pinMiniScsi,
    sctMiniDin,
    sctMicroDin,
    sctPS2,
    sctInfrared,
    sctHPHIL,
    sctAccessBusUsb,
    sctSsaScsi,
    sctCircularDin8M,
    sctCircularDin8F,
    sctOnBoardIDE,
    sctOnBoardFloppy,
    sct9pinDualInline,
    sct25pinDualInline,
    sct50pinDualInline,
    sct68pinDualInline,
    sctCdromSoundInput,
    sctMiniCentronicsType14,
    sctMiniCentronicsType26,
    sctMinijackHeadphones,
    sctBnc,
    sct1394,
    sctSasSataPlug,
    sctUsbC,
    sctPC98,
    sctPC98Hireso,
    sctPCH98,
    sctPC98Note,
    sctPC98Full
  );

  TSmbiosConnectorPort = (
    scpNone,
    scpParallelXTAT,
    scpParallelPS2,
    scpParallelECP,
    scpParallelEPP,
    scpParallelECPEPP,
    scpSerialXTAT,
    scpSerial16450,
    scpSerial16550,
    scpSerial16550A,
    scpScsi,
    scpMidi,
    scpJoyStick,
    scpKeyboard,
    scpMouse,
    scpSsaScsi,
    scpUsb,
    scpFireWire,
    scpPcmcia1,
    scpPcmcia2,
    scpPcmcia3,
    scpCardBus,
    scpAccessBus,
    scpScsi2,
    scpScsiWide,
    scpPC98,
    scpPC98Hireso,
    scpPCH98,
    scpVideo,
    scpAudio,
    scpModem,
    scpNetwork,
    scpSata,
    scpSas,
    scpMultiFunctionDisplayPort,
    scpThunderbolt
  );

  /// Port Connector Information (Type 8) structure
  TSmbiosConnector = packed record
    /// 2.0+ Internal Reference Designator (i)
    InternalName: RawUtf8;
    /// 2.0+ Internal Connector Type (j)
    InternalType: TSmbiosConnectorType;
    /// 2.0+ External Reference Designator (e)
    ExternalName: RawUtf8;
    /// 2.0+ External Connector Type (f)
    ExternalType: TSmbiosConnectorType;
    /// 2.0+ Describes the function of the Port (p)
    PortType: TSmbiosConnectorPort;
  end;

  // sstPC98C20 = A0H
  TSmbiosSlotType = (
    sstUndefined,
    sstOther,
    sstUnknown,
    sstIsa,
    sstMca,
    sstEisa,
    sstPci,
    sstPcmcia,
    sstVlVesa,
    sstProprietary,
    sstProcessorCard,
    sstProprietaryMemoryCard,
    sstIORiserCard,
    sstNuBus,
    sstPci66Mhz,
    sstAgp,
    sstAgp2X,
    sstAgp4X,
    sstPciX,
    sstAgp8X,
    sstM2Socket1DP_A,
    sstM2Socket1SD_E,
    sstM2Socket2_B,
    sstM2Socket3_M,
    sstMxm1,
    sstMxm2,
    sstMxm3,
    sstMxm3He,
    sstMxm4,
    sstMxm3A,
    sstMxm3B,
    sstPcieGen2,
    sstPcieGen3,
    sstPcieMini52pin_A,
    sstPcieMini52pin_B,
    sstPcieMini76pin,
    sstPcieGen4,
    sstPcieGen5,
    sstOcpNic3SFF,
    sstOcpNic3LFF,
    sstOcpNic,
    sstPC98C20,
    sstPC98C24,
    sstPC98E,
    sstPC98LocalBus,
    sstPC98Card,
    sstPcie,
    sstPcieX1,
    sstPcieX2,
    sstPcieX4,
    sstPcieX8,
    sstPcieX16,
    sstPcieGen2x,
    sstPcieGen2x1,
    sstPcieGen2x2 ,
    sstPcieGen2x4,
    sstPcieGen2x8 ,
    sstPcieGen2x16,
    sstPcieGen3x,
    sstPcieGen3x1,
    sstPcieGen3x2,
    sstPcieGen3x4,
    sstPcieGen3x8,
    sstPcieGen3x16,
    sstPcieGen4x,
    sstPcieGen4x1,
    sstPcieGen4x2,
    sstPcieGen4x4,
    sstPcieGen4x8,
    sstPcieGen4x16,
    sstPcieGen5x,
    sstPcieGen5x1,
    sstPcieGen5x2,
    sstPcieGen5x4,
    sstPcieGen5x8,
    sstPcieGen5x16,
    sstPcieGen6,
    sst1UE1FormFactor,
    sst3UE3FormFactor
  );

  TSmbiosSlotWidth = (
    sswUndefinedSlotWidth,
    sswOtherSlotWidth,
    sswUnknownSlotWidth,
    ssw8bit,
    ssw16bit,
    ssw32bit,
    ssw64bit,
    ssw128bit,
    sswx1,
    sswx2,
    sswx4,
    sswx8,
    sswx12,
    sswx16,
    sswx32
  );

  /// System Slot (Type 9) structure
  TSmbiosSlot = packed record
    /// 2.0+ Slot Designation (d)
    Designation: RawUtf8;
    /// 2.0+ Slot Type (t)
    SlotType: TSmbiosSlotType;
    /// 2.0+ Data Bus Width (w)
    Width: TSmbiosSlotWidth;
  end;

  TSmbiosMemoryFormFactor = (
    smfUndefined,
    smfOther,
    smfUnknown,
    smfSimm,
    smfSip,
    smfChip,
    smfDip,
    smfZip,
    smfProprietaryCard,
    smfDimm,
    smfTsop,
    smfRowOfCips,
    smfRimm,
    smfSodimm,
    smfSrimm,
    smfFbDimm,
    smfDie
  );

  TSmbiosMemoryType = (
    smtUndefined,
    smtOther,
    smtUnknown,
    smtDRAM,
    smtEDRAM,
    smtVRAM,
    smtSRAM,
    smtRAM,
    smtROM,
    smtFLASH,
    smtEEPROM,
    smtFEPROM,
    smtEPROM,
    smtCDRAM,
    smt3DRAM,
    smtSDRAM,
    smtSGRAM ,
    smtRDRAM,
    smtDDR,
    smtDDR2,
    smtDDR2FBDIMM,
    smt15,
    smt16,
    smt17,
    smtDDR3,
    smtFBD2,
    smtDDR4,
    smtLPDDR,
    smtLPDDR2,
    smtLPDDR3,
    smtLPDDR4,
    smtLogicalNonVolatileDevice,
    smtHBM,
    smtHBM2,
    smtDDR5,
    smtLPDDR5,
    smtHBM3
  );

  TSmbiosMemoryDetails = set of (
    smdReserved,
    smdOther,
    smdUnknown,
    smdFastPaged,
    smdStaticColumn,
    smdPseudoStatic,
    smdRambus,
    smdSynchronous,
    smdCmos,
    smdEdo,
    smdWindowDram,
    smdCacheDram,
    smdNonVolatile,
    smdRegisteredBuffered,
    smdUnbufferedUnRegistered,
    smdLrdimm
  );

  /// Memory Device (Type 17) structure
  TSmbiosMemory = packed record
    /// 2.1+ Total width, in bits
    // - equal DataWidth if there is no memory correction (w)
    TotalWidth: word;
    /// 2.1+ Data width, in bits (d)
    DataWidth: word;
    /// 2.1+/2.7+ Size of the memory device, in Bytes (s)
    Size: RawUtf8;
    /// 2.1+ Implementation form factor (f)
    FormFactor: TSmbiosMemoryFormFactor;
    /// 2.6+ Rank Attribute (r)
    Rank: byte;
    /// 2.1+ Type of Memory used by this Device (t)
    MemoryType: TSmbiosMemoryType;
    /// 2.1+ Features of this Memory Type (e)
    Details: TSmbiosMemoryDetails;
    /// 2.1+ Device Locator (l)
    Locator: RawUtf8;
    /// 2.1+ Bank Locator (b)
    Bank: RawUtf8;
    /// 2.3+ Manufacturer (m)
    Manufacturer: RawUtf8;
    /// 2.3+ Serial Number (n)
    Serial: RawUtf8;
    /// 2.3+ Asset Tag (a)
    AssetTag: RawUtf8;
    /// 2.3+ Part Number (p)
    PartNumber: RawUtf8;
    /// 2.3+/3.3+ Maximum Capable Speed, in Megatransfers per Seconds (c)
    MtPerSec: cardinal;
  end;

  TSmbiosPointingType = (
    sptUndefinedDevice,
    sptOtherDevice,
    sptUnknownDevice,
    sptMouse,
    sptTrackBall,
    sptTrackPoint,
    sptGlidePoint,
    sptTouchPad,
    sptTouchScreen,
    sptOpticalSensor
  );

  TSmbiosPointingInterface = (
    spiUndefined,
    spiOther,
    spiUnknown,
    spiSerial,
    spiPS2,
    spiInfrared,
    spiHpHil,
    spiBusMouse,
    spiADB,
    spiBusMouseDB9,
    spiBusMouseMicroDin,
    spiUSB,
    spiI2C,
    spiSPI
  );

  /// Built-in Pointing Device (Type 21) structure
  TSmbiosPointingDevice = packed record
    /// 2.1+ Type of Pointing Device (t)
    DeviceType: TSmbiosPointingType;
    /// 2.1+ Interface (i)
    InterfaceType: TSmbiosPointingInterface;
    /// 2.1+ Number of Buttons (b)
    Buttons: byte;
  end;

  /// Portable Battery (Type 22) structure
  TSmbiosBattery = packed record
    /// 2.1+ Location (l)
    Location: RawUtf8;
    /// 2.1+ Manufacturer (m)
    Manufacturer: RawUtf8;
    /// 2.1+ Serial Number (s)
    Serial: RawUtf8;
    /// 2.1+ Name (n)
    Name: RawUtf8;
    /// 2.1+ Version (v)
    Version: RawUtf8;
    /// 2.1+ Design Capacity in mW/h (c)
    Capacity: RawUtf8;
    /// 2.1+ Design Capacity in mV (g)
    Voltage: RawUtf8;
    /// 2.2+ Identifies the battery chemistry (h)
    Chemistry: RawUtf8;
    /// 2.2+ Manufacture Date (d)
    ManufactureDate: RawUtf8;
  end;

  TSmbiosSecurityStatus = (
    sssDisabled,
    sssEnabled,
    sssNotImplemented,
    sssUnknown
  );

  /// SMBIOS High-Level Information
  // - low-level DMI structures are decoded into ready-to-be-used text and sets
  // - when serialized as JSON, fields have very short identifiers
  // (e.g. "b":{"n:"...})
  // - for serialization as binary or JSON in short or human readable form:
  // ! b := BinarySave(@info, TypeInfo(TSmbiosInfo), rkRecordTypes);
  // ! SaveJson(info, TypeInfo(TSmbiosInfo), [twoIgnoreDefaultInRecord], s);
  // ! SaveJson(info, TypeInfo(TSmbiosInfo), [twoTrimLeftEnumSets, twoEnumSetsAsTextInRecord], s);
  TSmbiosInfo = packed record
    /// decoded BIOS Information (Type 0) structure (b)
    Bios: TSmbiosBios;
    /// decoded System Information (Type 1) structure (s)
    System: TSmbiosSystem;
    /// decoded Hardware Security (Type 24) structure (h)
    Security: packed record
      /// 2.2+ Front Panel Reset status (f)
      FrontPanelReset: TSmbiosSecurityStatus;
      /// 2.2+ Administrator Password status (a)
      AdministratorPassword: TSmbiosSecurityStatus;
      /// 2.2+ Keyboard Password status (k)
      KeyboardPassword: TSmbiosSecurityStatus;
      /// 2.2+ Power-on Password status (p)
      PoweronPassword: TSmbiosSecurityStatus;
    end;
    /// decoded Baseboard (or Module) Information (Type 2) structure (m)
    Board: array of TSmbiosBoard;
    /// decoded System Enclosure or Chassis (Type 3) structure (e)
    Chassis: array of TSmbiosChassis;
    /// decoded Processors Information (Type 4) structure (p)
    Processor: array of TSmbiosProcessor;
    /// decoded Port Connectors Information (Type 8) structure (c)
    Connector: array of TSmbiosConnector;
    /// decoded System Slots (Type 9) structure (t)
    Slot: array of TSmbiosSlot;
    /// decoded Memory Device (Type 17) structure (r)
    Memory: array of TSmbiosMemory;
    /// decoded Built-in Pointing Device (Type 21) (d)
    PointingDevice: array of TSmbiosPointingDevice;
    /// decoded Portable Battery (Type 22) structure (w)
    Battery: array of TSmbiosBattery;
  end;

var
  /// global variable filled by GetSmbiosInfo from SMBIOS binary information
  Smbios: TSmbiosInfo;

/// retrieve and decode DMI/SMBIOS data into high-level Smbios global variable
// - on POSIX, requires root to access SMBIOS raw memory so may return false
// - see also mormot.core.os.pas GetSmbios() more limited function
function GetSmbiosInfo: boolean;

/// decode SMBIOS raw binary into high-level usable information
// - see also mormot.core.os.pas DecodeSmbios() more limited function
// - optionally intern the strings, e.g. if you maintain several SMBIOS instances
function DecodeSmbiosInfo(const raw: TRawSmbiosInfo; out info: TSmbiosInfo;
  intern: TRawUtf8Interning = nil): boolean;


{ ************ TSynFpuException Wrapper for FPU Flags Preservation }

type
  /// a simple class which will set FPU exception flags for a code block
  // - using an IUnknown interface to let the compiler auto-generate a
  // try..finally block statement to reset the FPU exception register
  // - to be used e.g. as such:
  // !begin
  // !  with TSynFpuException.ForLibrayCode do
  // !  begin
  // !    ... now FPU exceptions will be ignored
  // !    ... so here it is safe to call external libray code
  // !  end; // now FPU exception will be reset as with standard Delphi
  // - it will avoid any unexpected invalid floating point operation in your
  // code, whereas it was in fact triggerred in some external library code
  TSynFpuException = class(TSynInterfacedObject)
  protected
    fExpected: TFpuFlags;
    fSaved: cardinal;
    function VirtualAddRef: integer; override;
    function VirtualRelease: integer; override;
  public
    /// internal constructor
    // - do not call this constructor directly, but rather use
    // ForLibraryCode/ForDelphiCode class methods
    // - for cpu32 flags are $1372 for Delphi, or $137F for library (mask all exceptions)
    // - for cpu64 flags are $1920 for Delphi, or $1FA0 for library (mask all exceptions)
    constructor Create(ExpectedFlags: TFpuFlags); reintroduce;
    /// after this method call, all FPU exceptions will be ignored
    // - until the method finishes (a try..finally block is generated by
    // the compiler), then FPU exceptions will be reset into "Delphi" mode
    // - you have to put this e.g. before calling an external libray
    // - this method is thread-safe and re-entrant (by reference-counting)
    class function ForLibraryCode: IUnknown;
    /// after this method call, all FPU exceptions will be enabled
    // - this is the Delphi normal behavior
    // - until the method finishes (a try..finally block is generated by
    // the compiler), then FPU execptions will be disabled again
    // - you have to put this e.g. before running object pascal code from
    // a callback executed in an external libray
    // - this method is thread-safe and re-entrant (by reference-counting)
    class function ForDelphiCode: IUnknown;
  end;


implementation

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

procedure TPrecisionTimer.Start(CurrentMicroSeconds: Int64);
begin
  FillCharFast(self, SizeOf(self), 0);
  if CurrentMicroSeconds = 0 then
    QueryPerformanceMicroSeconds(fStart)
  else
    fStart := CurrentMicroSeconds;
end;

function TPrecisionTimer.Started: boolean;
begin
  result := (fStart <> 0) or
            (fTime <> 0);
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
begin
  // mimics Pause from already known elapsed time
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
    if Int64(fTime) <= 0 then
      result := '0'
    else
      NanoSecToString((fTime * 1000) div Count, result);
  end;
end;

function TPrecisionTimer.PerSec(const Count: QWord): QWord;
begin
  if fStart <> 0 then
    Pause;
  if (Count = 0) or
     (Int64(fTime) <= 0) then
    // avoid negative or div per 0 in case of incorrect Start/Stop sequence
    result := 0
  else
    result := (Count * 1000000) div fTime;
end;

function TPrecisionTimer.SizePerSec(Size: QWord): ShortString;
begin
  FormatShort('% in % i.e. %/s', [KB(Size), Stop, KB(PerSec(Size))], result);
end;

type
  /// a class used internally by TPrecisionTimer.ProfileMethod
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

function TLocalPrecisionTimer.ByCount(Count: cardinal): RawUtf8;
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


{ ************ TSynMonitor Process Information Classes }

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
  inherited Create; // may have been overriden
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
  inherited Create; // may have been overriden
  fTotalTime := TSynMonitorTime.Create;
  fLastTime := TSynMonitorOneTime.Create;
  fMinimalTime := TSynMonitorOneTime.Create;
  fAverageTime := TSynMonitorOneTime.Create;
  fMaximalTime := TSynMonitorOneTime.Create;
end;

constructor TSynMonitor.Create(const aName: RawUtf8);
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

function TSynMonitor.RttiBeforeWriteObject(W: TTextWriter;
  var Options: TTextWriterWriteObjectOptions): boolean;
begin
 if woFullExpand in Options then
 begin
   // nested values do not need Instance name, but textual enums
   exclude(Options, woFullExpand);
   include(Options, woEnumSetsAsText);
 end;
 // call fSafe.Lock + continue serialization as usual
 result := inherited RttiBeforeWriteObject(W, Options);
end;

procedure TSynMonitor.ProcessStart;
begin
  if fProcessing then
    raise ESynException.CreateUtf8('Unexpected %.ProcessStart', [self]);
  InternalTimer.Resume;
  fTaskStatus := taskNotStarted;
  fProcessing := true;
end;

procedure TSynMonitor.LockedProcessDoTask;
begin
  inc(fTaskCount);
  fTaskStatus := taskStarted;
end;

procedure TSynMonitor.ProcessDoTask;
begin
  LockedProcessDoTask;
end;

procedure TSynMonitor.ProcessStartTask;
begin
  if fProcessing then
    raise ESynException.CreateUtf8('Reentrant %.ProcessStart', [self]);
  InternalTimer.Resume;
  fProcessing := true;
  LockedProcessDoTask;
end;

procedure TSynMonitor.ProcessEnd;
begin
  InternalTimer.Pause;
  LockedFromProcessTimer;
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
end;

procedure TSynMonitor.LockedFromExternalMicroSeconds(const MicroSecondsElapsed: QWord);
begin
  LockedProcessDoTask;
  InternalTimer.FromExternalMicroSeconds(MicroSecondsElapsed);
  LockedFromProcessTimer;
end;

procedure TSynMonitor.FromExternalMicroSeconds(const MicroSecondsElapsed: QWord);
begin
  // thread-safe ProcessStart+ProcessDoTask+ProcessEnd
  fSafe.Lock;
  {$ifdef HASFASTTRYFINALLY}
  try
  {$else}
  begin
  {$endif HASFASTTRYFINALLY}
    LockedFromExternalMicroSeconds(MicroSecondsElapsed);
  {$ifdef HASFASTTRYFINALLY}
  finally
  {$endif HASFASTTRYFINALLY}
    fSafe.UnLock;
  end;
end;

procedure TSynMonitor.Lock;
begin
  fSafe.Lock;
end;

procedure TSynMonitor.UnLock;
begin
  fSafe.UnLock;
end;

class procedure TSynMonitor.InitializeObjArray(var ObjArr; Count: integer);
var
  i: PtrInt;
begin
  ObjArrayClear(ObjArr);
  SetLength(TPointerDynArray(ObjArr), Count);
  for i := 0 to Count - 1 do
    TPointerDynArray(ObjArr)[i] := Create;
end;

procedure TSynMonitor.LockedProcessError(const info: variant);
begin
  if not VarIsEmptyOrNull(info) then
    inc(fInternalErrors);
  fLastInternalError := info;
end;

procedure TSynMonitor.LockedProcessErrorInteger(info: integer);
begin
  try
    LockedProcessError(info);
  except
  end;
end;

procedure TSynMonitor.ProcessError(const info: variant);
begin
  fSafe.Lock;
  try
    LockedProcessError(info);
  finally
    fSafe.UnLock;
  end;
end;

procedure TSynMonitor.ProcessErrorFmt(const Fmt: RawUtf8; const Args: array of const);
begin
  ProcessError(FormatVariant(Fmt, Args));
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
  if (self = nil) or
     (another = nil) then
    exit;
  fSafe.Lock;
  another.fSafe.Lock;
  {$ifdef HASFASTTRYFINALLY}
  try
  {$else}
  begin
  {$endif HASFASTTRYFINALLY}
    LockedSum(another);
  {$ifdef HASFASTTRYFINALLY}
  finally
  {$endif HASFASTTRYFINALLY}
    another.fSafe.UnLock;
    fSafe.UnLock;
  end;
end;

procedure TSynMonitor.LockedSum(another: TSynMonitor);
begin
  fTotalTime.MicroSec := fTotalTime.MicroSec + another.fTotalTime.MicroSec;
  if (fMinimalTime.MicroSec = 0) or
     (another.fMinimalTime.MicroSec < fMinimalTime.MicroSec) then
    fMinimalTime.MicroSec := another.fMinimalTime.MicroSec;
  if another.fMaximalTime.MicroSec > fMaximalTime.MicroSec then
    fMaximalTime.MicroSec := another.fMaximalTime.MicroSec;
  inc(fTaskCount, another.fTaskCount);
  if another.Processing then
    fProcessing := true; // if any thread is active, whole daemon is active
  inc(fInternalErrors, another.Errors);
end;

procedure TSynMonitor.LockedWriteDetailsTo(W: TTextWriter);
begin
  W.WriteObject(self); // simply use RTTI of published fields
end;

procedure TSynMonitor.ComputeDetailsTo(W: TTextWriter);
begin
  fSafe.Lock;
  try
    LockedPerSecProperties; // may not have been calculated after Sum()
    LockedWriteDetailsTo(W);
  finally
    fSafe.UnLock;
  end;
end;

function TSynMonitor.ComputeDetailsJson: RawUtf8;
var
  W: TJsonWriter;
  temp: TTextWriterStackBuffer;
begin
  W := TJsonWriter.CreateOwnedStream(temp);
  try
    ComputeDetailsTo(W);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

function TSynMonitor.ComputeDetails: variant;
begin
  _Json(ComputeDetailsJson, result{%H-}, JSON_FAST);
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
  fSafe.Lock;
  fSize.Bytes := fSize.Bytes + Bytes;
  fSafe.UnLock;
end;

procedure TSynMonitorWithSize.AddSize(const Bytes, MicroSecs: QWord);
begin
  fSafe.Lock;
  fSize.Bytes := fSize.Bytes + Bytes;
  LockedFromExternalMicroSeconds(MicroSecs);
  fSafe.UnLock;
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
  fInput  := TSynMonitorSize.Create({nospace=}false);
  fOutput := TSynMonitorSize.Create({nospace=}false);
  fInputThroughput  := TSynMonitorThroughput.Create({nospace=}false);
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
  fInputThroughput.BytesPerSec  := fTotalTime.PerSecond(fInput.Bytes);
  fOutputThroughput.BytesPerSec := fTotalTime.PerSecond(fOutput.Bytes);
end;

procedure TSynMonitorInputOutput.AddSize(const Incoming, Outgoing: QWord);
begin
  fSafe.Lock;
  fInput.Bytes  := fInput.Bytes + Incoming;
  fOutput.Bytes := fOutput.Bytes + Outgoing;
  fSafe.UnLock;
end;

procedure TSynMonitorInputOutput.Notify(
  const Incoming, Outgoing, MicroSec: QWord; Status: integer);
var
  error: boolean;
begin
  error := not StatusCodeIsSuccess(Status);
  fSafe.Lock;
  // inlined AddSize
  fInput.Bytes  := fInput.Bytes + Incoming;
  fOutput.Bytes := fOutput.Bytes + Outgoing;
  // inlined FromExternalMicroSeconds
  LockedFromExternalMicroSeconds(MicroSec);
  // inlined ProcessErrorNumber(Status)
  if error then
    LockedProcessErrorInteger(Status);
  fSafe.UnLock;
end;

procedure TSynMonitorInputOutput.LockedSum(another: TSynMonitor);
begin
  inherited LockedSum(another);
  if another.InheritsFrom(TSynMonitorInputOutput) then
  begin
    fInput.Bytes  := fInput.Bytes  + TSynMonitorInputOutput(another).Input.Bytes;
    fOutput.Bytes := fOutput.Bytes + TSynMonitorInputOutput(another).Output.Bytes;
  end;
end;


{ TSynMonitorServer }

procedure TSynMonitorServer.ClientConnect;
begin
  if self = nil then
    exit;
  fSafe.Lock;
  try
    inc(fClientsCurrent);
    if fClientsCurrent > fClientsMax then
      fClientsMax := fClientsCurrent;
  finally
    fSafe.UnLock;
  end;
end;

procedure TSynMonitorServer.ClientDisconnect;
begin
  if self = nil then
    exit;
  fSafe.Lock;
  try
    if fClientsCurrent > 0 then
      dec(fClientsCurrent);
  finally
    fSafe.UnLock;
  end;
end;

procedure TSynMonitorServer.ClientDisconnectAll;
begin
  if self = nil then
    exit;
  fSafe.Lock;
  try
    fClientsCurrent := 0;
  finally
    fSafe.UnLock;
  end;
end;

function TSynMonitorServer.GetClientsCurrent: TSynMonitorOneCount;
begin
  if self = nil then
    result := 0
  else
    result := fClientsCurrent;
end;

procedure TSynMonitorServer.AddCurrentRequestCount(diff: integer);
begin
  if self <> nil then
    if diff > 0 then
      LockedInc32(@fCurrentRequestCount)
    else if diff < 0 then
      LockedDec32(@fCurrentRequestCount);
end;


{ ************ TSynMonitorUsage Process Information Database Storage }

function ToText(gran: TSynMonitorUsageGranularity): PShortString;
begin
  result := GetEnumName(TypeInfo(TSynMonitorUsageGranularity), ord(gran));
end;

function MonitorPropUsageValue(info: PRttiProp): TSynMonitorType;
var
  typ: PRttiInfo;
begin
  typ := info^.TypeInfo;
  if typ = TypeInfo(TSynMonitorTotalMicroSec) then
    result := smvMicroSec
  else if typ = TypeInfo(TSynMonitorOneMicroSec) then
    result := smvOneMicroSec
  else if typ = TypeInfo(TSynMonitorTotalBytes) then
    result := smvBytes
  else if typ = TypeInfo(TSynMonitorOneBytes) then
    result := smvOneBytes
  else if typ = TypeInfo(TSynMonitorBytesPerSec) then
    result := smvBytesPerSec
  else if typ = TypeInfo(TSynMonitorCount) then
    result := smvCount
  else if typ = TypeInfo(TSynMonitorCount64) then
    result := smvCount64
  else if typ = TypeInfo(TSynMonitorOneCount) then
    result := smvOneCount
  else
    result := smvUndefined;
end;


{ TSynMonitorUsage }

function TSynMonitorUsage.Track(Instance: TObject; const Name: RawUtf8): integer;

  procedure ClassTrackProps(c: TClass;
    var props: TSynMonitorUsageTrackPropDynArray);
  var
    i, n: PtrInt;
    nfo: PRttiProp;
    k: TSynMonitorType;
    g: TSynMonitorUsageGranularity;
    p: PSynMonitorUsageTrackProp;
    ctp: TClass;
  begin
    n := length(props);
    while c <> nil do
    begin
      ctp := GetClassParent(c);
      for i := 1 to GetRttiProp(c, nfo) do
      begin
        k := MonitorPropUsageValue(nfo);
        if k <> smvUndefined then
        begin
          SetLength(props, n + 1);
          p := @props[n];
          p^.info := nfo;
          p^.Kind := k;
          ShortStringToAnsi7String(nfo^.Name^, p^.Name);
          if (ctp <> nil) and
             (FindPropName(['Bytes', 'MicroSec'], p^.Name) >= 0) then
            // meaningful property name = parent name
            ClassToText(ctp, p^.Name);
          for g := low(p^.Values) to high(p^.Values) do
            SetLength(p^.Values[g], USAGE_VALUE_LEN[g]);
          p^.ValueLast := nfo^.GetInt64Value(Instance);
          inc(n);
        end;
        nfo := nfo^.Next;
      end;
      c := ctp;
    end;
  end;

var
  i, n: PtrInt;
  instanceName: RawUtf8;
begin
  result := -1;
  if Instance = nil then
    exit; // nothing to track
  if (Name = '') and
     Instance.InheritsFrom(TSynMonitor) then
    instanceName := TSynMonitor(Instance).Name
  else
    instanceName := Name;
  if instanceName = '' then
    ClassToText(Instance.ClassType, instanceName);
  fSafe.Lock;
  try
    n := length(fTracked);
    for i := 0 to n - 1 do
      if fTracked[i].Instance = Instance then
        exit
      else if IdemPropNameU(fTracked[i].Name, instanceName) then
        raise ESynException.CreateUtf8('%.Track("%") name already exists',
          [self, instanceName]);
    SetLength(fTracked, n + 1);
    fTracked[n].Instance := Instance;
    fTracked[n].Name := instanceName;
    ClassTrackProps(PPointer(Instance)^, fTracked[n].Props);
    if fTracked[n].Props = nil then
      // nothing to track
      SetLength(fTracked, n)
    else
    begin
      // returns the index of the added item
      result := n;
      if fPrevious.Value <> 0 then
        LoadTrack(fTracked[n]);
    end;
  finally
    fSafe.UnLock;
  end;
end;

procedure TSynMonitorUsage.Track(const Instances: array of TSynMonitor);
var
  i: PtrInt;
begin
  if self <> nil then
    for i := 0 to high(Instances) do
      Track(Instances[i], Instances[i].Name);
end;

function TSynMonitorUsage.TrackPropLock(Instance: TObject;
  Info: PRttiProp): PSynMonitorUsageTrackProp;
var
  i, j: PtrInt;
begin
  result := nil;
  fSafe.Lock;
  for i := 0 to length(fTracked) - 1 do
    if fTracked[i].Instance = Instance then
      with fTracked[i] do
      begin
        for j := 0 to length(Props) - 1 do
          if Props[j].Info = Info then
          begin
            // returns found entry locked
            result := @Props[j];
            exit;
            // warning: caller should eventually make fSafe.ReadOnlyUnLock
          end;
        break;
      end;
  fSafe.UnLock;
end;

const
  // maps TTimeLogbits mask
  TL_MASK_SECONDS = pred(1 shl 6);
  TL_MASK_MINUTES = pred(1 shl 12);
  TL_MASK_HOURS   = pred(1 shl 17);
  TL_MASK_DAYS    = pred(1 shl 22);
  TL_MASK_MONTHS  = pred(1 shl 26);

  // truncates a TTimeLogbits value to a granularity
  AS_MINUTES = not TL_MASK_SECONDS;
  AS_HOURS   = not TL_MASK_MINUTES;
  AS_DAYS    = not TL_MASK_HOURS;
  AS_MONTHS  = not TL_MASK_DAYS;
  AS_YEARS   = not TL_MASK_MONTHS;

function TSynMonitorUsage.Modified(Instance: TObject): integer;
begin
  if self <> nil then
    result := Modified(Instance, [])
  else
    result := 0;
end;

procedure TSynMonitorUsage.SetCurrentUtcTime(out minutes: TTimeLogBits);
begin
  minutes.FromUtcTime;
end;

function TSynMonitorUsage.Modified(Instance: TObject;
  const PropNames: array of RawUtf8; ModificationTime: TTimeLog): integer;

  procedure save(const track: TSynMonitorUsageTrack);

    function scope({$ifdef CPU32}var{$endif}
      prev, current: Int64): TSynMonitorUsageGranularity;
    begin
      if prev and AS_YEARS <> current and AS_YEARS then
        result := mugYear
      else if prev and AS_MONTHS <> current and AS_MONTHS then
        result := mugMonth
      else if prev and AS_DAYS <> current and AS_DAYS then
        result := mugDay
      else if prev and AS_HOURS <> current and AS_HOURS then
        result := mugHour
      else if prev <> current then
        result := mugMinute
      else
        result := mugUndefined;
    end;

  var
    j, k, min: PtrInt;
    time: TTimeLogBits;
    v, diff: Int64;
  begin
    if ModificationTime = 0 then
      SetCurrentUtcTime(time)
    else
      time.Value := ModificationTime;
    time.Value := time.Value and AS_MINUTES; // save every minute
    if fPrevious.Value <> time.Value then
    begin
      if fPrevious.Value = 0 then
        // retrieve from database at startup
        Load(time)
      else
        // persist previous value to the database
        SavePrevious(scope(fPrevious.Value, time.Value));
      fPrevious.Value := time.Value;
    end;
    min := time.Minute;
    for j := 0 to length(track.Props) - 1 do
      with track.Props[j] do
        if (high(PropNames) < 0) or
           (FindPropName(PropNames, Name) >= 0) then
        begin
          v := Info^.GetInt64Value(Instance);
          diff := v - ValueLast;
          if diff <> 0 then
          begin
            inc(result);
            ValueLast := v;
            if Kind in SYNMONITORVALUE_CUMULATIVE then
            begin
              // propagate
              inc(Values[mugHour][min], diff);
              inc(Values[mugDay][time.Hour], diff);
              inc(Values[mugMonth][time.Day - 1], diff);
              inc(Values[mugYear][time.Month - 1], diff);
            end
            else
              // make instant values continuous
              for k := min to 59 do
                Values[mugHour][k] := v;
          end;
        end;
  end;

var
  i: PtrInt;
begin
  result := 0;
  if Instance = nil then
    exit;
  fSafe.Lock; // this single lock could make this method inefficient
  try
    for i := 0 to length(fTracked) - 1 do
      if fTracked[i].Instance = Instance then
      begin
        save(fTracked[i]);
        exit;
      end;
    if Instance.InheritsFrom(TSynMonitor) and
       (TSynMonitor(Instance).Name <> '') then
    begin
      i := Track(Instance, TSynMonitor(Instance).Name);
      if i >= 0 then
        save(fTracked[i]);
      exit;
    end;
  finally
    fSafe.UnLock;
  end;
end;

destructor TSynMonitorUsage.Destroy;
begin
  SavePrevious(mugUndefined); // save pending values for all granularities
  inherited Destroy;
end;

procedure TSynMonitorUsage.SavePrevious(Scope: TSynMonitorUsageGranularity);
var
  id: TSynMonitorUsageID;
  g: TSynMonitorUsageGranularity;
begin
  id.FromTimeLog(fPrevious.Value);
  Save(id, mugHour, Scope); // always save current minutes values
  for g := mugDay to mugYear do
    if (Scope <> mugUndefined) and
       (g > Scope) then
      break
    else
      // mugUndefined from Destroy
      Save(id, g, Scope);
end;

procedure TSynMonitorUsage.Save(ID: TSynMonitorUsageID;
  Gran, Scope: TSynMonitorUsageGranularity);
var
  t, n, p: PtrInt;
  track: PSynMonitorUsageTrack;
  data, val: TDocVariantData;
  g: PDocVariantData;
begin
  if Gran < low(fValues) then
    raise ESynException.CreateUtf8('%.Save(%) unexpected', [self, ToText(Gran)^]);
  TDocVariant.IsOfTypeOrNewFast(fValues[Gran]);
  g := _Safe(fValues[Gran]);
  for t := 0 to length(fTracked) - 1 do
  begin
    track := @fTracked[t];
    n := length(track^.Props);
    data.InitFast(n, dvObject);
    for p := 0 to n - 1 do
      with track^.Props[p] do
        if not IsZero(Values[Gran]) then
        begin
          // save non void values
          val.InitArrayFrom(Values[Gran], JSON_FAST);
          data.AddValue(Name, Variant(val));
          val.Clear;
          // handle local cache
          if Kind in SYNMONITORVALUE_CUMULATIVE then
          begin
            if Gran <= Scope then // reset of cumulative values
              FillZero(Values[Gran]);
          end
          else
          begin
            if Gran < mugYear then // propagate instant values
              // e.g. Values[mugDay][hour] := Values[mugHour][minute] (=v)
              Values[succ(Gran)][ID.GetTime(Gran, true)] :=
                Values[Gran][ID.GetTime(pred(Gran), true)];
          end;
        end;
    g^.AddOrUpdateValue(track^.Name, variant(data));
    data.Clear;
  end;
  g^.SortByName;
  ID.Truncate(Gran);
  if not SaveDB(ID.Value, fValues[Gran], Gran) then
    fLog.SynLog.Log(sllWarning, 'Save(ID=%=%,%) failed',
      [ID.Value, ID.Text(true), ToText(Gran)^], self);
end;

procedure TSynMonitorUsage.LoadTrack(var Track: TSynMonitorUsageTrack);
var
  ndx, v: PtrInt;
  g: TSynMonitorUsageGranularity;
  val, int: PDocVariantData;
begin
  // fValues[] variants -> fTracked[].Props[].Values[]
  for g := low(fValues) to high(fValues) do
    with _Safe(fValues[g])^ do
    begin
      val := GetAsDocVariantSafe(Track.Name);
      if val <> nil then
        for ndx := 0 to length(Track.Props) - 1 do
          with Track.Props[ndx] do
            if val^.GetAsDocVariant(Name, int) and
               (int^.Count > 0) and
               int^.IsArray then
            begin
              for v := 0 to length(Values[g]) - 1 do
                if v < int^.Count then
                  Values[g][v] := VariantToInt64Def(int^.Values[v], 0);
            end;
    end;
end;

function TSynMonitorUsage.Load(const Time: TTimeLogBits): boolean;
var
  g: TSynMonitorUsageGranularity;
  id: TSynMonitorUsageID;
  t: integer;
begin
  // load fValues[] variants
  result := true;
  id.FromTimeLog(Time.Value);
  for g := low(fValues) to high(fValues) do
  begin
    id.Truncate(g);
    if not LoadDB(id.Value, g, fValues[g]) then
      result := false;
  end;
  // fill fTracked[].Props[].Values[]
  for t := 0 to length(fTracked) - 1 do
    LoadTrack(fTracked[t]);
end;


{ TSynMonitorUsageID }

procedure TSynMonitorUsageID.From(Y, M, D, H: integer);
begin
  Value := H +
           (D - 1) shl USAGE_ID_SHIFT[mugDay] +
           (M - 1) shl USAGE_ID_SHIFT[mugMonth] +
           (Y - USAGE_ID_YEAROFFSET) shl USAGE_ID_SHIFT[mugYear];
end;

procedure TSynMonitorUsageID.From(Y, M, D: integer);
begin
  Value := USAGE_ID_HOURMARKER[mugDay] +
           (D - 1) shl USAGE_ID_SHIFT[mugDay] +
           (M - 1) shl USAGE_ID_SHIFT[mugMonth] +
           (Y - USAGE_ID_YEAROFFSET) shl USAGE_ID_SHIFT[mugYear];
end;

procedure TSynMonitorUsageID.From(Y, M: integer);
begin
  Value := USAGE_ID_HOURMARKER[mugMonth] +
           (M - 1) shl USAGE_ID_SHIFT[mugMonth] +
           (Y - USAGE_ID_YEAROFFSET) shl USAGE_ID_SHIFT[mugYear];
end;

procedure TSynMonitorUsageID.From(Y: integer);
begin
  Value := USAGE_ID_HOURMARKER[mugYear] +
           (Y - USAGE_ID_YEAROFFSET) shl USAGE_ID_SHIFT[mugYear];
end;

procedure TSynMonitorUsageID.FromTimeLog(const TimeLog: TTimeLog);
var
  bits: TTimeLogBits absolute TimeLog;
begin
  Value := bits.Hour +
           (bits.Day - 1) shl USAGE_ID_SHIFT[mugDay] +
           (bits.Month - 1) shl USAGE_ID_SHIFT[mugMonth] +
           (bits.Year - USAGE_ID_YEAROFFSET) shl USAGE_ID_SHIFT[mugYear];
end;

procedure TSynMonitorUsageID.FromNowUtc;
var
  now: TTimeLogBits;
begin
  now.FromUtcTime;
  From(now.Value);
end;

function TSynMonitorUsageID.GetTime(gran: TSynMonitorUsageGranularity;
  monthdaystartat0: boolean): integer;
begin
  if not (gran in [low(USAGE_ID_SHIFT)..high(USAGE_ID_SHIFT)]) then
    result := 0
  else
  begin
    result := (Value shr USAGE_ID_SHIFT[gran]) and USAGE_ID_MASK[gran];
    case gran of
      mugYear:
        inc(result, USAGE_ID_YEAROFFSET);
      mugDay,
      mugMonth:
        if not monthdaystartat0 then
          inc(result);
      mugHour:
        if cardinal(result) > USAGE_ID_MAX[mugHour] then
          // stored fake USAGE_ID_HOURMARKER[mugDay..mugYear] value
          result := 0;
    end;
  end;
end;

function TSynMonitorUsageID.Granularity: TSynMonitorUsageGranularity;
var
  h: integer;
begin
  h := Value and USAGE_ID_MASK[mugHour];
  if cardinal(h) > USAGE_ID_MAX[mugHour] then
  begin
    for result := mugDay to mugYear do
      if USAGE_ID_HOURMARKER[result] = h then
        exit;
    result := mugUndefined; // should not happen
  end
  else
    result := mugHour;
end;

procedure TSynMonitorUsageID.Truncate(gran: TSynMonitorUsageGranularity);
begin
  if gran > mugHour then
    Value := (Value and not USAGE_ID_MASK[mugHour]) or USAGE_ID_HOURMARKER[gran];
end;

procedure TSynMonitorUsageID.SetTime(gran: TSynMonitorUsageGranularity;
  aValue: integer);
begin
  case gran of
    mugYear:
      dec(aValue, USAGE_ID_YEAROFFSET);
    mugDay,
    mugMonth:
      dec(aValue);
    mugHour:
      ;
  else
    raise ERangeError.CreateFmt('SetValue(%s)', [ToText(gran)^]);
  end;
  if cardinal(aValue) > USAGE_ID_MAX[gran] then
    raise ERangeError.CreateFmt('%s should be 0..%d',
      [ToText(gran)^, USAGE_ID_MAX[gran]]);
  Value := (Value and not (USAGE_ID_MASK[gran] shl USAGE_ID_SHIFT[gran])) or
           (aValue shl USAGE_ID_SHIFT[gran]);
end;

function TSynMonitorUsageID.Text(Expanded: boolean;
  FirstTimeChar: AnsiChar): RawUtf8;
var
  bits: TTimeLogBits;
begin
  bits.Value := ToTimeLog;
  result := bits.Text(Expanded, FirstTimeChar);
end;

function TSynMonitorUsageID.ToTimeLog: TTimeLog;
begin
  PTimeLogBits(@result)^.From(GetTime(mugYear), GetTime(mugMonth),
    GetTime(mugDay), GetTime(mugHour), 0, 0);
end;



{ ************ Operating System Monitoring }

function FeaturesToText(Info: PRttiInfo; const Features;
  const Sep: RawUtf8; UnderscorePos: integer): RawUtf8;
var
  f, max: integer;
  List: PShortString;
begin
  result := '';
  max := GetEnumType(Info, List);
  for f := 0 to max do
  begin
    if GetBitPtr(@Features, f) and
       (List^[UnderscorePos] <> '_') then
    begin
      if result <> '' then
        result := result + Sep;
      result := result + RawUtf8(copy(List^, UnderscorePos, 20));
    end;
    inc(PByte(List), PByte(List)^ + 1); // next
  end;
end;

function ToText(const aIntelCPUFeatures: TIntelCpuFeatures;
  const Sep: RawUtf8): RawUtf8;
begin
  result := FeaturesToText(
    TypeInfo(TIntelCpuFeature), aIntelCPUFeatures, Sep, 3);
end;

function ToText(const aArm32CPUFeatures: TArm32HwCaps;
  const Sep: RawUtf8): RawUtf8;
begin
  result := FeaturesToText(
    TypeInfo(TArm32HwCap), aArm32CPUFeatures, Sep, 6);
end;

function ToText(const aArm64CPUFeatures: TArm64HwCaps;
  const Sep: RawUtf8): RawUtf8;
begin
  result := FeaturesToText(
    TypeInfo(TArm64HwCap), aArm64CPUFeatures, Sep, 6);
end;


function SystemInfoJson: RawUtf8;
var
  cpu, mem, free: RawUtf8;
begin
  cpu := TSystemUse.Current(false).HistoryText(0, 15, @mem);
  if mem = '' then
    free := TSynMonitorMemory.FreeAsText(false, @mem)
  else
    free := TSynMonitorMemory.FreeAsText;
  with SystemInfo do
    result := JsonEncode([
      'host',        Executable.Host,
      'user',        Executable.User,
      'os',          OSVersionText,
      'cpu',         CpuInfoText,
      'bios',        BiosInfoText,
      {$ifdef OSWINDOWS}{$ifdef CPU32}'wow64', IsWow64, {$endif}{$endif OSWINDOWS}
      'cpufeatures', CpuFeaturesText,
      'processcpu',  cpu,
      'processmem',  mem,
      'freemem',     free,
      'disk',        GetDiskPartitionsText({nocache=}false, {withfree=}true)]);
end;

{$ifdef NOEXCEPTIONINTERCEPT}
function GetLastExceptions(Depth: integer): variant;
begin
  VarClear(result{%H-});
end;
{$else}
function GetLastExceptions(Depth: integer): variant;
var
  info: TSynLogExceptionInfoDynArray;
  i: PtrInt;
begin
  VarClear(result{%H-});
  GetLastExceptions(info, Depth);
  if info = nil then
    exit;
  TDocVariantData(result).InitFast(length(info), dvArray);
  for i := 0 to high(info) do
    TDocVariantData(result).AddItemText(ToText(info[i]));
end;
{$endif NOEXCEPTIONINTERCEPT}


{ TSystemUse }

procedure TSystemUse.OnTimerExecute(Sender: TObject);
var
  i: PtrInt;
  now: TDateTime;
begin
  if (self = nil) or
     (fProcess = nil) or
     (fHistoryDepth = 0) or
     not fProcessInfo.Start then
    exit;
  fTimer := Sender;
  now := NowUtc;
  fSafe.WriteLock;
  try
    inc(fDataIndex);
    if fDataIndex >= fHistoryDepth then
      fDataIndex := 0;
    for i := high(fProcess) downto 0 do // backwards for fProcesses.Delete(i)
      with fProcess[i] do
        if fProcessInfo.PerProcess(ID, @now, Data[fDataIndex], PrevKernel, PrevUser) then
        begin
          if Assigned(fOnMeasured) then
            fOnMeasured(ID, Data[fDataIndex]);
        end
        else if UnsubscribeProcessOnAccessError then
          // if GetLastError=ERROR_INVALID_PARAMETER then
          fProcesses.Delete(i);
  finally
    fSafe.WriteUnLock;
  end;
end;

constructor TSystemUse.Create(const aProcessID: array of integer;
  aHistoryDepth: integer);
var
  i: PtrInt;
  it, kt, ut: Int64;
begin
  inherited Create; // may have been overriden
  fProcesses.Init(TypeInfo(TSystemUseProcessDynArray), fProcess);
  if not RetrieveSystemTimes(it, kt, ut) then
    exit; // no system monitoring API on Linux or oldest Windows
  if aHistoryDepth <= 0 then
    aHistoryDepth := 1;
  fHistoryDepth := aHistoryDepth;
  SetLength(fProcess, length(aProcessID));
  for i := 0 to high(aProcessID) do
  begin
    {$ifdef OSWINDOWS}
    if aProcessID[i] = 0 then
      fProcess[i].ID := GetCurrentProcessID
    else
    {$endif OSWINDOWS}
      fProcess[i].ID := aProcessID[i];
    SetLength(fProcess[i].Data, fHistoryDepth);
  end;
end;

constructor TSystemUse.Create(aHistoryDepth: integer);
begin
  Create([0], aHistoryDepth);
end;

procedure TSystemUse.Subscribe(aProcessID: integer);
var
  i, n: PtrInt;
begin
  if self = nil then
    exit;
  {$ifdef OSWINDOWS}
  if aProcessID = 0 then
    aProcessID := GetCurrentProcessID;
  {$endif OSWINDOWS}
  fSafe.WriteLock;
  try
    n := length(fProcess);
    for i := 0 to n - 1 do
      if fProcess[i].ID = aProcessID then
        exit; // already subscribed
    SetLength(fProcess, n + 1);
    fProcess[n].ID := aProcessID;
    SetLength(fProcess[n].Data, fHistoryDepth);
  finally
    fSafe.WriteUnLock;
  end;
end;

function TSystemUse.Unsubscribe(aProcessID: integer): boolean;
var
  i: PtrInt;
begin
  result := false;
  if self = nil then
    exit;
  fSafe.WriteLock;
  try
    i := LockedProcessIndex(aProcessID);
    if i >= 0 then
    begin
      fProcesses.Delete(i);
      result := true;
    end;
  finally
    fSafe.WriteUnLock;
  end;
end;

function TSystemUse.LockedProcessIndex(aProcessID: integer): PtrInt;
begin
  // caller should have made any fSafe lock
  {$ifdef OSWINDOWS}
  if aProcessID = 0 then
    aProcessID := GetCurrentProcessID;
  {$endif OSWINDOWS}
  if self <> nil then
    for result := 0 to high(fProcess) do
      if fProcess[result].ID = aProcessID then
        exit;
  result := -1;
end;

function TSystemUse.Data(out aData: TSystemUseData; aProcessID: integer): boolean;
var
  i: PtrInt;
begin
  result := false;
  if self <> nil then
  begin
    fSafe.ReadLock;
    try
      i := LockedProcessIndex(aProcessID);
      if i >= 0 then
      begin
        with fProcess[i] do
          aData := Data[fDataIndex];
        result := aData.Timestamp <> 0;
        if result then
          exit;
      end;
    finally
      fSafe.ReadUnLock;
    end;
  end;
  FillCharFast(aData, SizeOf(aData), 0);
end;

function TSystemUse.Data(aProcessID: integer): TSystemUseData;
begin
  Data(result, aProcessID);
end;

function TSystemUse.KB(aProcessID: integer): cardinal;
begin
  with Data(aProcessID) do
    result := WorkKB + VirtualKB;
end;

function TSystemUse.Percent(aProcessID: integer): single;
begin
  with Data(aProcessID) do
    result := Kernel + User;
end;

function TSystemUse.PercentKernel(aProcessID: integer): single;
begin
  result := Data(aProcessID).Kernel;
end;

function TSystemUse.PercentUser(aProcessID: integer): single;
begin
  result := Data(aProcessID).User;
end;

function TSystemUse.PercentSystem(out Idle, Kernel, User: single): boolean;
begin
  result := fProcessInfo.PerSystem(Idle, Kernel, User);
end;

function TSystemUse.HistoryData(aProcessID, aDepth: integer): TSystemUseDataDynArray;
var
  i, n, last: PtrInt;
begin
  result := nil;
  if self = nil then
    exit;
  fSafe.ReadLock;
  try
    i := LockedProcessIndex(aProcessID);
    if i >= 0 then
      with fProcess[i] do
      begin
        n := length(Data);
        last := n - 1;
        if (aDepth > 0) and
           (n > aDepth) then
          n := aDepth;
        SetLength(result, n); // make ordered copy
        for i := 0 to n - 1 do
        begin
          if i <= fDataIndex then
            result[i] := Data[fDataIndex - i]
          else
          begin
            result[i] := Data[last];
            dec(last);
          end;
          if PInt64(@result[i].Timestamp)^ = 0 then
          begin
            SetLength(result, i); // truncate to latest available sample
            break;
          end;
        end;
      end;
  finally
    fSafe.ReadUnLock;
  end;
end;

function TSystemUse.History(aProcessID, aDepth: integer): TSingleDynArray;
var
  i, n: PtrInt;
  data: TSystemUseDataDynArray;
begin
  result := nil;
  data := HistoryData(aProcessID, aDepth);
  n := length(data);
  SetLength(result, n);
  for i := 0 to n - 1 do
    result[i] := data[i].Kernel + data[i].User;
end;

var
  ProcessSystemUse: TSystemUse;

class function TSystemUse.Current(aCreateIfNone: boolean): TSystemUse;
begin
  if (ProcessSystemUse = nil) and
     aCreateIfNone then
  begin
    GlobalLock; // RegisterGlobalShutdownRelease() will use it anyway
    try
      if ProcessSystemUse = nil then
        ProcessSystemUse := RegisterGlobalShutdownRelease(
          TSystemUse.Create(60));
    finally
      GlobalUnLock;
    end;
  end;
  result := ProcessSystemUse;
end;

function TSystemUse.HistoryText(aProcessID, aDepth: integer;
  aDestMemoryMB: PRawUtf8): RawUtf8;
var
  data: TSystemUseDataDynArray;
  mem: RawUtf8;
  i: PtrInt;
begin
  result := '';
  mem := '';
  data := HistoryData(aProcessID, aDepth);
  {$ifndef OSWINDOWS}
  if data = nil then
    result := RetrieveLoadAvg // from '/proc/loadavg' or libc getloadavg()
  else
  {$endif OSWINDOWS}
    for i := 0 to high(data) do
      with data[i] do
      begin
        result := FormatUtf8('%% ', [result, TwoDigits(Kernel + User)]);
        if aDestMemoryMB <> nil then
          mem := FormatUtf8('%% ', [mem, TwoDigits(WorkKB / 1024)]);
      end;
  TrimSelf(result);
  if aDestMemoryMB <> nil then
    aDestMemoryMB^ := TrimU(mem);
end;

function TSystemUse.HistoryVariant(aProcessID, aDepth: integer): variant;
var
  res: TDocVariantData absolute result;
  data: TSystemUseDataDynArray;
  i: PtrInt;
begin
  VarClear(result{%H-});
  data := HistoryData(aProcessID, aDepth);
  res.InitFast(length(data), dvArray);
  for i := 0 to high(data) do
    res.AddItem(TwoDigits(data[i].Kernel + data[i].User));
end;

function SortDynArrayDiskPartitions(const A, B): integer;
begin
  result := SortDynArrayString(TDiskPartition(A).mounted, TDiskPartition(B).mounted);
end;

var
  _DiskPartitions: TDiskPartitions;

function GetDiskPartitionsText(
  nocache, withfreespace, nospace, nomount: boolean): RawUtf8;
var
  i: PtrInt;
  parts: TDiskPartitions;

  function GetInfo(var p: TDiskPartition): ShortString;
  const
    F: array[boolean] of RawUtf8 = ('% % (% / %)', '% % (%/%)');
    N: array[boolean] of RawUtf8 = ('% % / %', '% %/%');
  var
    av, fr, tot: QWord;
  begin
    if not withfreespace or
       not GetDiskInfo(p.mounted, av, fr, tot) then
      FormatShort('% % (%)',
        [p.mounted, p.name, KB(p.size, nospace)], result)
    else if nomount then
      FormatShort(N[nospace],
        [p.mounted, KB(fr, nospace), KB(tot, nospace)], result)
    else
      FormatShort(F[nospace],
        [p.mounted, p.name, KB(fr, nospace), KB(tot, nospace)], result);
  end;

begin
  if (_DiskPartitions = nil) or
     nocache then
  begin
    _DiskPartitions := GetDiskPartitions;
    {$ifdef OSPOSIX}
    DynArray(TypeInfo(TDiskPartitions), _DiskPartitions).
      Sort(SortDynArrayDiskPartitions);
    {$endif OSPOSIX}
  end;
  parts := _DiskPartitions;
  if parts = nil then
    result := ''
  else
    ShortStringToAnsi7String(GetInfo(parts[0]), result);
  for i := 1 to high(parts) do
    result := FormatUtf8('%, %', [result, GetInfo(parts[i])]);
end;


{ TSynMonitorMemory }

constructor TSynMonitorMemory.Create(aTextNoSpace: boolean);
begin
  inherited Create; // may have been overriden
  FAllocatedUsed := TSynMonitorOneSize.Create(aTextNoSpace);
  FAllocatedReserved := TSynMonitorOneSize.Create(aTextNoSpace);
  FPhysicalMemoryFree := TSynMonitorOneSize.Create(aTextNoSpace);
  FVirtualMemoryFree := TSynMonitorOneSize.Create(aTextNoSpace);
  FPagingFileTotal := TSynMonitorOneSize.Create(aTextNoSpace);
  FPhysicalMemoryTotal := TSynMonitorOneSize.Create(aTextNoSpace);
  FVirtualMemoryTotal := TSynMonitorOneSize.Create(aTextNoSpace);
  FPagingFileFree := TSynMonitorOneSize.Create(aTextNoSpace);
end;

destructor TSynMonitorMemory.Destroy;
begin
  FAllocatedReserved.Free;
  FAllocatedUsed.Free;
  FPhysicalMemoryFree.Free;
  FVirtualMemoryFree.Free;
  FPagingFileTotal.Free;
  FPhysicalMemoryTotal.Free;
  FVirtualMemoryTotal.Free;
  FPagingFileFree.Free;
  inherited Destroy;
end;

class function TSynMonitorMemory.FreeAsText(nospace: boolean;
  processfree: PRawUtf8): ShortString;
const
  F: array[boolean] of RawUtf8 = ('% / %', '%/%');
begin
  with TSynMonitorMemory.Create(nospace) do
  try
    RetrieveMemoryInfo;
    FormatShort(F[nospace], [fPhysicalMemoryFree.Text, fPhysicalMemoryTotal.Text], result);
    if processfree <> nil then
      FormatUtf8(F[noSpace], [fAllocatedUsed.Text, FAllocatedReserved.Text], processfree^);
  finally
    Free;
  end;
end;

var
  PhysicalAsTextCache: TShort16; // this value doesn't change usually

class function TSynMonitorMemory.PhysicalAsText(nospace: boolean): TShort16;
begin
  if PhysicalAsTextCache = '' then
    with TSynMonitorMemory.Create(nospace) do
    try
      PhysicalAsTextCache := PhysicalMemoryTotal.Text;
    finally
      Free;
    end;
  result := PhysicalAsTextCache;
end;

class function TSynMonitorMemory.ToJson: RawUtf8;
begin
  with TSynMonitorMemory.Create(false) do
  try
    RetrieveMemoryInfo;
    FormatUtf8('{Allocated:{reserved:%,used:%},Physical:{total:%,free:%,percent:%},' +
      {$ifdef OSWINDOWS}'Virtual:{total:%,free:%},' + {$endif}'Paged:{total:%,free:%}}',
      [fAllocatedReserved.Bytes shr 10, fAllocatedUsed.Bytes shr 10,
      fPhysicalMemoryTotal.Bytes shr 10, fPhysicalMemoryFree.Bytes shr 10,
      fMemoryLoadPercent, {$ifdef OSWINDOWS}fVirtualMemoryTotal.Bytes shr 10,
      fVirtualMemoryFree.Bytes shr 10, {$endif} fPagingFileTotal.Bytes shr 10,
      fPagingFileFree.Bytes shr 10], result);
  finally
    Free;
  end;
end;

class function TSynMonitorMemory.ToVariant: variant;
begin
  result := _JsonFast(ToJson);
end;

function TSynMonitorMemory.GetAllocatedUsed: TSynMonitorOneSize;
begin
  RetrieveMemoryInfo;
  result := FAllocatedUsed;
end;

function TSynMonitorMemory.GetAllocatedReserved: TSynMonitorOneSize;
begin
  RetrieveMemoryInfo;
  result := FAllocatedReserved;
end;

function TSynMonitorMemory.GetMemoryLoadPercent: integer;
begin
  RetrieveMemoryInfo;
  result := FMemoryLoadPercent;
end;

function TSynMonitorMemory.GetPagingFileFree: TSynMonitorOneSize;
begin
  RetrieveMemoryInfo;
  result := FPagingFileFree;
end;

function TSynMonitorMemory.GetPagingFileTotal: TSynMonitorOneSize;
begin
  RetrieveMemoryInfo;
  result := FPagingFileTotal;
end;

function TSynMonitorMemory.GetPhysicalMemoryFree: TSynMonitorOneSize;
begin
  RetrieveMemoryInfo;
  result := FPhysicalMemoryFree;
end;

function TSynMonitorMemory.GetPhysicalMemoryTotal: TSynMonitorOneSize;
begin
  RetrieveMemoryInfo;
  result := FPhysicalMemoryTotal;
end;

function TSynMonitorMemory.GetVirtualMemoryFree: TSynMonitorOneSize;
begin
  RetrieveMemoryInfo;
  result := FVirtualMemoryFree;
end;

function TSynMonitorMemory.GetVirtualMemoryTotal: TSynMonitorOneSize;
begin
  RetrieveMemoryInfo;
  result := FVirtualMemoryTotal;
end;

procedure TSynMonitorMemory.RetrieveMemoryInfo;
var
  tix: cardinal;
  info: TMemoryInfo;
begin
  tix := GetTickCount64 shr 7; // allow 128 ms resolution for updates
  if fLastMemoryInfoRetrievedTix <> tix then
  begin
    fLastMemoryInfoRetrievedTix := tix;
    if not GetMemoryInfo(info, {withalloc=}true) then
      exit;
    FMemoryLoadPercent := info.percent;
    FPhysicalMemoryTotal.Bytes := info.memtotal;
    FPhysicalMemoryFree.Bytes := info.memfree;
    FPagingFileTotal.Bytes := info.filetotal;
    FPagingFileFree.Bytes := info.filefree;
    FVirtualMemoryTotal.Bytes := info.vmtotal;
    FVirtualMemoryFree.Bytes := info.vmfree;
    FAllocatedReserved.Bytes := info.allocreserved;
    FAllocatedUsed.Bytes := info.allocused;
  end;
end;


{ TSynMonitorDisk }

constructor TSynMonitorDisk.Create;
begin
  inherited Create; // may have been overriden
  fAvailableSize := TSynMonitorOneSize.Create({nospace=}false);
  fFreeSize := TSynMonitorOneSize.Create({nospace=}false);
  fTotalSize := TSynMonitorOneSize.Create({nospace=}false);
end;

destructor TSynMonitorDisk.Destroy;
begin
  fAvailableSize.Free;
  fFreeSize.Free;
  fTotalSize.Free;
  inherited;
end;

function TSynMonitorDisk.GetName: TFileName;
begin
  RetrieveDiskInfo;
  result := fName;
end;

function TSynMonitorDisk.GetAvailable: TSynMonitorOneSize;
begin
  RetrieveDiskInfo;
  result := fAvailableSize;
end;

function TSynMonitorDisk.GetFree: TSynMonitorOneSize;
begin
  RetrieveDiskInfo;
  result := fFreeSize;
end;

function TSynMonitorDisk.GetTotal: TSynMonitorOneSize;
begin
  RetrieveDiskInfo;
  result := fTotalSize;
end;

class function TSynMonitorDisk.FreeAsText: RawUtf8;
var
  name: TFileName;
  avail, free, total: QWord;
begin
  GetDiskInfo(name, avail, free, total);
  FormatUtf8('% % / %', [name, KB(free), KB(total)], result);
end;

procedure TSynMonitorDisk.RetrieveDiskInfo;
var
  tix: cardinal;
begin
  tix := GetTickCount64 shr 7; // allow 128 ms resolution for updates
  if fLastDiskInfoRetrievedTix <> tix then
  begin
    fLastDiskInfoRetrievedTix := tix;
    GetDiskInfo(fName, PQWord(@fAvailableSize.Bytes)^, PQWord(@fFreeSize.Bytes)^,
      PQWord(@fTotalSize.Bytes)^ {$ifdef OSWINDOWS}, @fVolumeName{$endif});
  end;
end;


{ ************ DMI/SMBIOS Binary Decoder }

var
  GetSmbiosInfoChecked: boolean;

function GetSmbiosInfo: boolean;
begin
  if not GetSmbiosInfoChecked then
  begin
    GlobalLock;
    try
      if not GetSmbiosInfoChecked then
        if GetRawSmbios then
          DecodeSmbiosInfo(RawSmbios, Smbios);
      GetSmbiosInfoChecked := true;
    finally
      GlobalUnLock;
    end;
  end;
  result := Smbios.Bios.VendorName <> '';
end;

const
  _ROMSIZ: array[0..3] of string[2] = ('MB', 'GB', '??', '??');
  _VOLT:   array[0..3] of string[3] = ('5', '3.3', '2.9', '?');

procedure CacheSize8(b: PtrUInt; var res: RawUtf8);
var
  r: PtrUInt;
begin
  r := b and $7fff;
  if (b and $8000) <> 0 then
    r := r shl 16   // 64K granularity
  else
    r := r shl 10;  // 1K granularity
  KBU(r, res);
end;

function DecodeSmbiosInfo(const raw: TRawSmbiosInfo; out info: TSmbiosInfo;
  intern: TRawUtf8Interning): boolean;
var
  s: PByteArray;
  cur: PPRawUtf8;
  n, cap: cardinal;
  q: QWord;
  len, trimright, i: PtrInt;
  lines: array[byte] of PRawUtf8; // efficient string decoding
  cache: array of TSmbiosCache; // linked in 2nd pass

  procedure FillCache(var dest: TSmbiosCache);
  var
    i: PtrInt;
  begin
    for i := 0 to high(cache) do
      if cache[i].Handle = dest.Handle then
      begin
        dest := cache[i];
        break;
      end;
  end;

begin
  Finalize(info);
  result := false;
  s := pointer(raw.Data);
  if s = nil then
    exit;
  // first pass will fill the main info structures
  FillCharFast(lines, SizeOf(lines), 0);
  repeat
    if (s[0] = 127) or // type (127=EOT)
       (s[1] < 4) then // length
      break;
    case s[0] of
      // note: the deprecated/oem/seldom-used types are not decoded
      0: // Bios Information (type 0)
        begin
          lines[s[4]] := @info.Bios.VendorName;
          lines[s[5]] := @info.Bios.Version;
          lines[s[8]] := @info.Bios.BuildDate;
          if (s[9] = $ff) and
             (s[1] > $18) then // 3.1+
          begin
            n := PWord(@s[$18])^;
            FormatUtf8('% %', [n and $3fff, _ROMSIZ[n shr 14]], info.Bios.RomSize);
          end
          else
            KBU((PtrUInt(s[9]) + 1) shl 16, info.Bios.RomSize);
          PCardinal(@info.Bios.Characteristics)^ := PCardinal(@s[$0a])^;
          if s[1] >= $17 then // 2.4+
          begin
            PCardinalArray(@info.Bios.Characteristics)^[1] := PWord(@s[$12])^;
            FormatUtf8('%.%', [s[$14], s[$15]], info.Bios.Release);
            FormatUtf8('%.%', [s[$16], s[$17]], info.Bios.Firmware);
          end;
        end;
      1: // System Information (type 1)
        begin
          lines[s[4]] := @info.System.Manufacturer;
          lines[s[5]] := @info.System.ProductName;
          lines[s[6]] := @info.System.Version;
          lines[s[7]] := @info.System.Serial;
          if s[1] >= $18 then // 2.1+
          begin
            DecodeSmbiosUuid(@s[8], info.System.UUID, raw);
            info.System.WakupType := TSmbiosSystemWakeup(s[$18]);
            if s[1] >= $1a then // 2.4+
            begin
              lines[s[$19]] := @info.System.Sku;
              lines[s[$1a]] := @info.System.Family;
            end;
          end;
        end;
      2: // Baseboard (or Module) Information (type 2)
        begin
          SetLength(info.Board, length(info.Board) + 1);
          with info.Board[high(info.Board)] do
          begin
            lines[s[4]] := @Manufacturer;
            lines[s[5]] := @Product;
            lines[s[6]] := @Version;
            lines[s[7]] := @Serial;
            lines[s[8]] := @AssetTag;
            lines[s[10]] := @Location;
            Features := TSmbiosBoardFeatures(s[9]);
            BoardType := TSmbiosBoardType(s[$0D]);
          end;
        end;
      3: // System Enclosure or Chassis (type 3)
        begin
          SetLength(info.Chassis, length(info.Chassis) + 1);
          with info.Chassis[high(info.Chassis)] do
          begin
            lines[s[4]] := @Manufacturer;
            lines[s[6]] := @Version;
            lines[s[7]] := @Serial;
            lines[s[8]] := @AssetTag;
            Lock := s[5] and 128 <> 0;
            ChassisType := TSmbiosChassisType(s[5] and 127);
            if s[1] >= $0c then // 2.1+
            begin
              BootUpState := TSmbiosChassisState(s[$09]);
              PowerState  := TSmbiosChassisState(s[$0a]);
              ThermalState := TSmbiosChassisState(s[$0b]);
              Security := TSmbiosChassisSecurityState(s[$0c]);
              if s[1] >= $12 then // 2.3+
              begin
                OEM := PCardinal(@s[$0d])^;
                Height := s[$11];
                PowerCords := s[$12];
              end;
            end;
          end;
        end;
      4: // Processor Information (type 4)
        begin
          SetLength(info.Processor, length(info.Processor) + 1);
          with info.Processor[high(info.Processor)] do
          begin
            lines[s[4]] := @SocketDesignation;
            lines[s[7]] := @Manufacturer;
            lines[s[$10]] := @Version;
            if s[1] >= $22 then // 2.3+
            begin
              lines[s[$20]] := @Serial;
              lines[s[$21]] := @AssetTag;
              lines[s[$22]] := @PartNumber;
            end;
            ProcessorType := TSmbiosProcessorType(s[5]);
            Family := s[6];
            BinToHexLower(@PQWord(@s[8])^, 8, ID);
            n := s[$11];
            if n and 128 = 0 then
            begin
              for i := 0 to 3 do
                if (1 shl i) and n <> 0 then
                  Voltage := FormatUtf8('%%V ', [Voltage, _VOLT[i]]);
            end
            else
            begin
              n := n and 127;
              FormatUtf8('%.%V', [n div 10, n mod 10], Voltage);
            end;
            Status := TSmbiosProcessorStatus(s[$18] and 7);
            Populated := s[$18] and 64 <> 0;
            Upgrade := TSmbiosProcessorUpgrade(s[$19]);
            if s[1] >= $1e then // 2.1+
            begin
              L1Cache.Handle := PWord(@s[$1a])^; // resolved in 2nd pass
              L2Cache.Handle := PWord(@s[$1c])^;
              L3Cache.Handle := PWord(@s[$1e])^;
              if s[1] >= $26 then // 2.5+
              begin
                CoreCount := s[$23];
                CoreEnabled := s[$24];
                Threadcount := s[$25];
                Flags := TSmbiosProcessorFlags(PWord(@s[$26])^);
                if s[1] >= $29 then // 2.6+
                begin
                  Family := PWord(@s[$28])^;
                  if s[1] >= $2f then // 3.0+
                  begin
                    CoreCount := PWord(@s[$2a])^;
                    CoreEnabled := PWord(@s[$2c])^;
                    Threadcount := PWord(@s[$2e])^;
                    if s[1] >= $31 then // 3.6+
                      ThreadEnabled := PWord(@s[$30])^;
                  end;
                end;
              end;
            end;
          end;
        end;
      // type 5 and 6 are obsolete
      7: // Cache Information (type 7)
        begin
          SetLength(cache, length(cache) + 1);
          with cache[high(cache)] do
          begin
            Handle := PWord(@s[2])^; // for late binding within info.Processor[]
            lines[s[4]] := @SocketDesignation;
            Level := (s[5] and 7) + 1;
            Socketed := (s[5] and 8) <> 0;
            Location := TSmbiosCacheLocation((s[5] shr 6) and 3);
            Enabled := (s[5] and 128) <> 0;
            OperationalMode := TSmbiosCacheMode(s[6] and 3);
            CacheSize8(PWord(@s[$09])^, Size);
            CacheSize8(PWord(@s[$07])^, MaxSize);
            PWord(@Sram)^ := PWord(@s[$0d])^;
            PWord(@SuportedSram)^ := PWord(@s[$0b])^;
            if s[1] >= $12 then // 2.1+
            begin
              Speed := s[$0f];
              Ecc := TSmbiosCacheEcc(s[$10]);
              CacheType := TSmbiosCacheType(s[$11]);
              Associativity := TSmbiosCacheAssociativity(s[$12]);
            end;
          end;
        end;
      8: // Port Connector Information (type 8)
        begin
          SetLength(info.Connector, length(info.Connector) + 1);
          with info.Connector[high(info.Connector)] do
          begin
            if s[5] <> byte(sctNone) then
            begin
              lines[s[4]] := @InternalName;
              InternalType := TSmbiosConnectorType(s[5]);
            end;
            if s[7] <> byte(sctNone) then
            begin
              lines[s[6]] := @ExternalName;
              ExternalType := TSmbiosConnectorType(s[7]);
            end;
            PortType := TSmbiosConnectorPort(s[8]);
          end;
        end;
      9: // System Slot (type 9)
        begin
          SetLength(info.Slot, length(info.Slot) + 1);
          with info.Slot[high(info.Slot)] do
          begin
            lines[s[4]] := @Designation;
            SlotType := TSmbiosSlotType(s[5]);
            Width := TSmbiosSlotWidth(s[6]);
          end;
        end;
      17: // Memory Device (type 17)
        begin
          SetLength(info.Memory, length(info.Memory) + 1);
          with info.Memory[high(info.Memory)] do
          begin
            TotalWidth := PWord(@s[8])^;
            DataWidth := PWord(@s[$0a])^;;
            n := PWord(@s[$0c])^;
            if n <> $ffff then
            begin
              q := n and $7fff;
              if (s[$0d] and $80) = 0 then
                q := q shl 20
              else
                q := q shl 10;
              KBU(q, Size);
            end;
            FormFactor := TSmbiosMemoryFormFactor(s[$0e]);
            lines[s[$10]] := @Locator;
            lines[s[$11]] := @Bank;
            MemoryType := TSmbiosMemoryType(s[$12]);
            Details := TSmbiosMemoryDetails(PWord(@s[$13])^);
            if s[1] >= $1A then // 2.3+
            begin
              MtPerSec := PWord(@s[$15])^;
              lines[s[$17]] := @Manufacturer;
              lines[s[$18]] := @Serial;
              lines[s[$19]] := @AssetTag;
              lines[s[$1a]] := @PartNumber;
              if s[1] >= $1f then // 2.7+
              begin
                Rank := s[$1b] and 7;
                n := PCardinal(@s[$1c])^;
                if n <> 0 then
                  KBU(QWord(n) shl 20, Size); // in MB
                if s[1] >= $5b then // 3.3+
                  MtPerSec := PCardinal(@s[$58])^;
              end;
            end;
          end;
        end;
      21: // Built-in Pointing Device (type 21)
        begin
          SetLength(info.PointingDevice, length(info.PointingDevice) + 1);
          with info.PointingDevice[high(info.PointingDevice)] do
          begin
            DeviceType := TSmbiosPointingType(s[4]);
            if s[5] >= $a0 then
              InterfaceType := TSmbiosPointingInterface(
                s[5] - $a0 + ord(spiBusMouseDB9))
            else
              InterfaceType := TSmbiosPointingInterface(s[5]);
            Buttons := s[6];
          end;
        end;
      22: // Portable Battery (type 22)
        begin
          SetLength(info.Battery, length(info.Battery) + 1);
          with info.Battery[high(info.Battery)] do
          begin
            if s[1] >= $0f then // 2.1+
            begin
              lines[s[4]] := @Location;
              lines[s[5]] := @Manufacturer;
              lines[s[7]] := @Serial;
              lines[s[8]] := @Name;
              n := PWord(@s[$0c])^; // in mV
              FormatUtf8('%.% V', [n div 1000, (n mod 1000) div 100], Voltage);
              lines[s[$0e]] := @Version;
              cap := PWord(@s[$0a])^; // in mW/H
              if s[1] >= $15 then // 2.2+
              begin
                n := PWord(@s[$12])^;
                FormatUtf8('%/%/%', [ // mm/dd/yyyy as in info.Bios.BuildDate
                  UInt2DigitsToShortFast((n shr 5) and 15),
                  UInt2DigitsToShortFast(n and 31),
                  1980 + n shr 9], ManufactureDate);
                lines[s[$14]] := @Chemistry;
                if s[$15] > 1 then
                  cap := cap * s[$15]; // Design Capacity Multiplier
              end;
              FormatUtf8('%.% W/H',
                [cap div 1000, (cap mod 1000) div 100], Capacity);
            end;
          end;
        end;
      24: // Hardware Security (Type 24)
        with info.Security do
        begin
          FrontPanelReset := TSmbiosSecurityStatus(s[4] and 3);
          AdministratorPassword := TSmbiosSecurityStatus((s[4] shr 2) and 3);
          KeyboardPassword := TSmbiosSecurityStatus((s[4] shr 4) and 3);
          PoweronPassword := TSmbiosSecurityStatus((s[4] shr 6) and 3);
        end;
    end;
    s := @s[s[1]]; // go to string table
    cur := @lines[1];
    if s[0] = 0 then
      inc(PByte(s)) // no string table
    else
      repeat
        len := StrLen(s);
        if cur^ <> nil then
        begin
          trimright := len;
          while (trimright <> 0) and
                (s[trimright - 1] <= ord(' ')) do
            dec(trimright);
          intern.Unique(cur^^, pointer(s), trimright);
          cur^ := nil; // reset slot in lines[]
        end;
        s := @s[len + 1]; // next string
        inc(cur);
      until s[0] = 0; // end of string table
    inc(PByte(s)); // go to next structure
  until false;
  // 2nd pass will link all cache[] to info.Processor[]
  for i := 0 to high(info.Processor) do
    with info.Processor[i] do
    begin
      FillCache(L1Cache);
      FillCache(L2Cache);
      FillCache(L3Cache);
    end;
  result := info.Bios.VendorName <> '';
end;


{ ************ TSynFpuException Wrapper for FPU Flags Preservation }

{ TSynFpuException }

function TSynFpuException.VirtualAddRef: integer;
begin
  if fRefCount = 0 then
    // set FPU exceptions mask
    fSaved := SetFpuFlags(fExpected);
  inc(fRefCount);
  result := 1; // should never be 0 (mark release of TSynFpuException instance)
end;

function TSynFpuException.VirtualRelease: integer;
begin
  dec(fRefCount);
  if fRefCount = 0 then
    ResetFpuFlags(fSaved);
  result := 1; // should never be 0 (mark release of TSynFpuException instance)
end;

threadvar
  GlobalSynFpuExceptionDelphi,
  GlobalSynFpuExceptionLibrary: TSynFpuException;

constructor TSynFpuException.Create(ExpectedFlags: TFpuFlags);
begin
  // $1920=Delphi $1FA0=library (mask all exceptions)
  inherited Create;
  fExpected := ExpectedFlags;
end;

class function TSynFpuException.ForLibraryCode: IUnknown;
var
  obj: TSynFpuException;
begin
  result := GlobalSynFpuExceptionLibrary; // threadvar instances
  if result <> nil then
    exit;
  obj := TSynFpuException.Create(ffLibrary);
  RegisterGlobalShutdownRelease(obj);
  GlobalSynFpuExceptionLibrary := obj;
  result := obj;
end;

class function TSynFpuException.ForDelphiCode: IUnknown;
var
  obj: TSynFpuException;
begin
  result := GlobalSynFpuExceptionDelphi;
  if result <> nil then
    exit;
  obj := TSynFpuException.Create(ffPascal);
  RegisterGlobalShutdownRelease(obj);
  GlobalSynFpuExceptionDelphi := obj;
  result := obj;
end;


const
  _TSmbiosBios = 'n,v,b,s,r,f:RawUtf8 c:TSmbiosBiosFlags';
  _TSmbiosSystem = 'm,p,v,s,u,k,f:RawUtf8 w:TSmbiosSystemWakeup';
  _TSmbiosBoard = 'm,p,v,s,a,l:RawUtf8 f:TSmbiosBoardFeatures t:TSmbiosBoardType';
  _TSmbiosChassis = 'l:boolean t:TSmbiosChassisType m,v,s,a:RawUtf8 ' +
    'b,w,h:TSmbiosChassisState p:TSmbiosChassisSecurityState o:cardinal u,c:byte';
  _TSmbiosCache = 'h:word d:RawUtf8 v:byte b,k:boolean l:TSmbiosCacheLocation ' +
    'o:TSmbiosCacheMode s,m:RawUtf8 c,r:TSmbiosCacheSramType n:byte ' +
    'e:TSmbiosCacheEcc t:TSmbiosCacheType a:TSmbiosCacheAssociativity';
  _TSmbiosProcessor = 'd:RawUtf8 t:TSmbiosProcessorType f:cardinal i,m,v,g:RawUtf8 ' +
    'u:TSmbiosProcessorStatus l:boolean p:TSmbiosProcessorUpgrade ' +
    '1,2,3:TSmbiosCache s,a,n:RawUtf8 c,e,r,b:cardinal h:TSmbiosProcessorFlags';
  _TSmbiosConnector = 'i:RawUtf8 j:TSmbiosConnectorType e:RawUtf8 ' +
    'f:TSmbiosConnectorType p:TSmbiosConnectorPort';
  _TSmbiosSlot = 'd:RawUtf8 t:TSmbiosSlotType w:TSmbiosSlotWidth';
  _TSmbiosMemory = 'w,d:word s:RawUtf8 f:TSmbiosMemoryFormFactor r:byte ' +
    't:TSmbiosMemoryType e:TSmbiosMemoryDetails l,b,m,n,a,p:RawUtf8 c:cardinal';
  _TSmbiosPointingDevice = // no RTTI -> embedded within _TSmbiosInfo
    't:TSmbiosPointingType i:TSmbiosPointingInterface b:byte';
  _TSmbiosBattery = 'l,m,s,n,v,c,g,h,d:RawUtf8';
  _TSmbiosSecurity = 'f,a,k,p:TSmbiosSecurityStatus';
  _TSmbiosInfo = 'b:TSmbiosBios s:TSmbiosSystem h:{' + _TSmbiosSecurity + '} ' +
    'm:array of TSmbiosBoard e:array of TSmbiosChassis ' +
    'p:array of TSmbiosProcessor c:array of TSmbiosConnector ' +
    't:array of TSmbiosSlot r:array of TSmbiosMemory ' +
    'd:[' + _TSmbiosPointingDevice + '] w:array of TSmbiosBattery' ;

procedure InitializeUnit;
begin
  {$ifdef CPUINTELARM}
  // CpuFeatures: TIntelCpuFeatures/TArm32HwCaps/TArm64HwCaps
  CpuFeaturesText := LowerCase(ToText(CpuFeatures, ' '));
  if CpuFeaturesText = '' then
  {$endif CPUINTELARM}
  begin
    {$ifdef OSLINUXANDROID}
    CpuFeaturesText := LowerCase(CpuInfoFeatures); // fallback to /proc/cpuinfo
    {$endif OSLINUXANDROID}
  end;
  Rtti.RegisterTypes([
    TypeInfo(TSmbiosBiosFlags),            TypeInfo(TSmbiosSystemWakeup),
    TypeInfo(TSmbiosBoardFeatures),        TypeInfo(TSmbiosBoardType),
    TypeInfo(TSmbiosChassisType),          TypeInfo(TSmbiosChassisState),
    TypeInfo(TSmbiosChassisSecurityState), TypeInfo(TSmbiosCacheLocation),
    TypeInfo(TSmbiosCacheMode),            TypeInfo(TSmbiosCacheSramType),
    TypeInfo(TSmbiosCacheEcc),             TypeInfo(TSmbiosCacheType),
    TypeInfo(TSmbiosCacheAssociativity),   TypeInfo(TSmbiosProcessorType),
    TypeInfo(TSmbiosProcessorStatus),      TypeInfo(TSmbiosProcessorUpgrade),
    TypeInfo(TSmbiosProcessorFlags),       TypeInfo(TSmbiosConnectorType),
    TypeInfo(TSmbiosConnectorPort),        TypeInfo(TSmbiosSlotType),
    TypeInfo(TSmbiosSlotWidth),            TypeInfo(TSmbiosMemoryFormFactor),
    TypeInfo(TSmbiosMemoryType),           TypeInfo(TSmbiosMemoryDetails),
    TypeInfo(TSmbiosSecurityStatus),       TypeInfo(TSmbiosPointingType),
    TypeInfo(TSmbiosPointingInterface)
  ]);
  Rtti.RegisterFromText([
    TypeInfo(TSmbiosBios),            _TSmbiosBios,
    TypeInfo(TSmbiosSystem),          _TSmbiosSystem,
    TypeInfo(TSmbiosBoard),           _TSmbiosBoard,
    TypeInfo(TSmbiosChassis),         _TSmbiosChassis,
    TypeInfo(TSmbiosCache),           _TSmbiosCache,
    TypeInfo(TSmbiosProcessor),       _TSmbiosProcessor,
    TypeInfo(TSmbiosConnector),       _TSmbiosConnector,
    TypeInfo(TSmbiosSlot),            _TSmbiosSlot,
    TypeInfo(TSmbiosMemory),          _TSmbiosMemory,
    TypeInfo(TSmbiosBattery),         _TSmbiosBattery,
    TypeInfo(TSmbiosInfo),            _TSmbiosInfo
  ]);
end;

initialization
  InitializeUnit;

end.


