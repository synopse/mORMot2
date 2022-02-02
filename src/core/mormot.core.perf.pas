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
    procedure Start;
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

  /// abstract class to track, compute and store TSynMonitor detailed statistics
  // - you should inherit from this class to implement proper data persistence,
  // e.g. using TSynMonitorUsageRest for ORM-based storage
  // - SaveDB may take some time, so a TSynLocker OS lock is used, not TRWLock
  TSynMonitorUsage = class(TSynPersistentLock)
  protected
    fLog: TSynLogFamily;
    fTracked: array of TSynMonitorUsageTrack;
    fValues: array[mugHour..mugYear] of variant;
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
  withfreespace: boolean = false; nospace: boolean = false): RawUtf8;

/// returns a JSON object containing basic information about the computer
// - including Host, User, CPU, OS, freemem, freedisk...
function SystemInfoJson: RawUtf8;

/// returns a TDocVariant array of the latest intercepted exception texts
// - runs ToText() over all information returned by overloaded GetLastExceptions
// - defined in this unit to have TDocVariant at hand
function GetLastExceptions(Depth: integer = 0): variant; overload;



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
  p, v: PtrInt;
  g: TSynMonitorUsageGranularity;
  val, int: PDocVariantData;
begin
  // fValues[] variants -> fTracked[].Props[].Values[]
  for g := low(fValues) to high(fValues) do
    with _Safe(fValues[g])^ do
    begin
      val := GetAsDocVariantSafe(Track.Name);
      if val <> nil then
        for p := 0 to length(Track.Props) - 1 do
          with Track.Props[p] do
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
      'host', Executable.Host,
      'user', Executable.User,
      'os', OSVersionText,
      'cpu', CpuInfoText,
      'bios', BiosInfoText,
      {$ifdef OSWINDOWS}{$ifdef CPU32}'wow64', IsWow64, {$endif}{$endif OSWINDOWS}
      'cpufeatures', CpuFeaturesText,
      'processcpu', cpu,
      'processmem', mem,
      'freemem', free,
      'disk', GetDiskPartitionsText({nocache=}false, {withfree=}true)]);
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

function GetDiskPartitionsText(nocache, withfreespace, nospace: boolean): RawUtf8;
var
  i: PtrInt;
  parts: TDiskPartitions;

  function GetInfo(var p: TDiskPartition): ShortString;
  const
    F: array[boolean] of RawUtf8 = ('% % (% / %)', '% % (%/%)');
  var
    av, fr, tot: QWord;
  begin
    if not withfreespace or
       not GetDiskInfo(p.mounted, av, fr, tot) then
      FormatShort('% % (%)',
        [p.mounted, p.name, KB(p.size, nospace)], result)
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


procedure InitializeUnit;
begin
  {$ifdef CPUINTELARM}
  // CpuFeatures: TIntelCpuFeatures/TArm32HwCaps/TArm64HwCaps
  CpuFeaturesText := LowerCase(ToText(CpuFeatures, ' '));
  if CpuFeaturesText = '' then
  {$endif CPUINTELARM}
    {$ifdef OSLINUXANDROID}
    CpuFeaturesText := LowerCase(CpuInfoFeatures); // fallback to /proc/cpuinfo
    {$endif OSLINUXANDROID}
end;

initialization
  InitializeUnit;

end.


