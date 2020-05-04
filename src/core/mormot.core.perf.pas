/// Framework Core Performance and Monitoring Classes
// - this unit is a part of the freeware Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.perf;

{
  *****************************************************************************

   Performance Monitoring functions shared by all framework units
    - Performance Counters
    - TSynMonitor Root Classes
    - Operating System Monitoring

  *****************************************************************************
}


interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.data,
  mormot.core.rtti,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.variants,
  mormot.core.json;



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
      {$ifdef HASINLINE}inline;{$endif}
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



{ ************ TSynMonitor Root Classes }

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
    procedure WriteDetailsTo(W: TBaseWriter); virtual;
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
    procedure ComputeDetailsTo(W: TBaseWriter); virtual;
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
    // customize JSON Serialization to set woEnumSetsAsText
    function RttiBeforeWriteObject(W: TBaseWriter;
      var Options: TTextWriterWriteObjectOptions): boolean; override;
    // set the rcfSynPersistentHook flag to call RttiBeforeWriteObject
    class procedure RttiCustomSet(Rtti: TRttiCustom); override;
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


{ ************ Operating System Monitoring }

type
  /// event handler which may be executed by TSystemUse.BackgroundExecute
  // - called just after the measurement of each process CPU and RAM consumption
  // - run from the background thread, so should not directly make VCL calls,
  // unless BackgroundExecute is run from a VCL timer
  TOnSystemUseMeasured = procedure(ProcessID: integer; const Data: TSystemUseData) of object;

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
  TSystemUse = class(TSynPersistentLock)
  protected
    fProcess: TSystemUseProcessDynArray;
    fProcesses: TDynArray;
    fDataIndex: integer;
    fProcessInfo: TProcessInfo;
    fHistoryDepth: integer;
    fOnMeasured: TOnSystemUseMeasured;
    fTimer: TObject;
    fUnsubscribeProcessOnAccessError: boolean;
    function ProcessIndex(aProcessID: integer): integer;
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
    // - on Linux, will return the /proc/loadavg pseudo-file content
    function HistoryText(aProcessID: integer = 0; aDepth: integer = 0;
      aDestMemoryMB: PRawUTF8 = nil): RawUTF8;
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
    property HistoryDepth: integer read fHistoryDepth;
    /// executed when TSystemUse.BackgroundExecute finished its measurement
    property OnMeasured: TOnSystemUseMeasured read fOnMeasured write fOnMeasured;
    /// low-level access to the associated timer running BackgroundExecute
    // - equals nil if has been associated to no timer
    property Timer: TObject read fTimer write fTimer;
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
      processfree: PRawUTF8 = nil): ShortString;
    /// how many physical memory is currently installed, as text (e.g. '32 GB');
    class function PhysicalAsText(nospace: boolean = false): TShort16;
    /// returns a JSON object with the current system memory information
    // - numbers would be given in KB (Bytes shl 10)
    class function ToJSON: RawUTF8;
    /// fill a TDocVariant with the current system memory information
    // - numbers would be given in KB (Bytes shl 10)
    class function ToVariant: variant;
  published
    /// Total of allocated memory used by the program
    property AllocatedUsed: TSynMonitorOneSize read GetAllocatedUsed;
    /// Total of allocated memory reserved by the program
    property AllocatedReserved: TSynMonitorOneSize read GetAllocatedReserved;
    /// Percent of memory in use for the system
    property MemoryLoadPercent: integer read GetMemoryLoadPercent;
    /// Total of physical memory for the system
    property PhysicalMemoryTotal: TSynMonitorOneSize read GetPhysicalMemoryTotal;
    /// Free of physical memory for the system
    property PhysicalMemoryFree: TSynMonitorOneSize read GetPhysicalMemoryFree;
    /// Total of paging file for the system
    property PagingFileTotal: TSynMonitorOneSize read GetPagingFileTotal;
    /// Free of paging file for the system
    property PagingFileFree: TSynMonitorOneSize read GetPagingFileFree;
    {$ifdef MSWINDOWS}
    /// Total of virtual memory for the system
    // - property not defined under Linux, since not applying to this OS
    property VirtualMemoryTotal: TSynMonitorOneSize read GetVirtualMemoryTotal;
    /// Free of virtual memory for the system
    // - property not defined under Linux, since not applying to this OS
    property VirtualMemoryFree: TSynMonitorOneSize read GetVirtualMemoryFree;
    {$endif MSWINDOWS}
  end;

  /// value object able to gather information about a system drive
  TSynMonitorDisk = class(TSynPersistent)
  protected
    fName: TFileName;
    {$ifdef MSWINDOWS}
    fVolumeName: SynUnicode;
    {$endif MSWINDOWS}
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
    class function FreeAsText: RawUTF8;
  published
    /// the disk name
    property Name: TFileName read GetName;
    {$ifdef MSWINDOWS}
    /// the volume name (only available on Windows)
    property VolumeName: SynUnicode read fVolumeName write fVolumeName;
    /// space currently available on this disk for the current user
    // - may be less then FreeSize, if user quotas are specified (only taken
    // into account under Windows: on POSIX, AvailableSize=FreeSize)
    property AvailableSize: TSynMonitorOneSize read GetAvailable;
    {$endif MSWINDOWS}
    /// free space currently available on this disk
    property FreeSize: TSynMonitorOneSize read GetFree;
    /// total space
    property TotalSize: TSynMonitorOneSize read GetTotal;
  end;


/// convert Intel CPU features as plain CSV text
function ToText(const aIntelCPUFeatures: TIntelCpuFeatures;
  const Sep: RawUTF8 = ','): RawUTF8; overload;


/// retrieve low-level information about all mounted disk partitions as text
// - returns e.g. under Linux
// '/ /dev/sda3 (19 GB), /boot /dev/sda2 (486.8 MB), /home /dev/sda4 (0.9 TB)'
// or under Windows 'C:\ System (115 GB), D:\ Data (99.3 GB)'
// - uses internally a cache unless nocache is true
// - includes the free space if withfreespace is true - e.g. '(80 GB / 115 GB)'
function GetDiskPartitionsText(nocache: boolean = false;
  withfreespace: boolean = false; nospace: boolean = false): RawUTF8;

{$ifdef CPUINTEL}
/// returns the global Intel/AMD CpuFeatures flags as ready-to-be-displayed text
function CpuFeaturesText: RawUTF8;
{$endif CPUINTEL}

/// returns a JSON object containing basic information about the computer
// - including Host, User, CPU, OS, freemem, freedisk...
function SystemInfoJson: RawUTF8;




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


{ ************ TSynMonitor Root Classes }

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

class procedure TSynMonitor.RttiCustomSet(Rtti: TRttiCustom);
begin
  // let's call RttiBeforeWriteObject
  Rtti.Flags := Rtti.Flags + [rcfSynPersistentHook];
end;

function TSynMonitor.RttiBeforeWriteObject(W: TBaseWriter;
  var Options: TTextWriterWriteObjectOptions): boolean;
begin
 if woFullExpand in Options then
 begin
   // nested values do not need Instance name, but textual enums
   exclude(Options, woFullExpand);
   include(Options, woEnumSetsAsText);
 end;
 result := false; // continue serialization as usual
end;

procedure TSynMonitor.ProcessStart;
begin
  if fProcessing then
    raise ESynException.CreateUTF8('Unexpected %.ProcessStart', [self]);
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

procedure TSynMonitor.WriteDetailsTo(W: TBaseWriter);
begin
  fSafe^.Lock;
  try
    W.WriteObject(self);
  finally
    fSafe^.UnLock;
  end;
end;

procedure TSynMonitor.ComputeDetailsTo(W: TBaseWriter);
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
  W: TBaseWriter;
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


{ ************ Operating System Monitoring }

function ToText(const aIntelCPUFeatures: TIntelCpuFeatures;
  const Sep: RawUTF8): RawUTF8;
var
  f: TIntelCpuFeature;
  List: PShortString;
begin
  result := '';
  GetEnumType(TypeInfo(TIntelCpuFeature), List);
  for f := low(f) to high(f) do
  begin
    if (f in aIntelCPUFeatures) and (List^[3] <> '_') then
    begin
      if result <> '' then
        result := result + Sep;
      result := result + TrimLeftLowerCaseShort(List);
    end;
    inc(PByte(List), PByte(List)^ + 1); // next
  end;
end;

{$ifdef CPUINTEL}

var
  _CpuFeatures: RawUTF8;

function CpuFeaturesText: RawUTF8;
begin
  if _CpuFeatures = '' then
    _CpuFeatures := LowerCase(ToText(CpuFeatures, ' '));
  result := _CpuFeatures;
end;

{$endif CPUINTEL}

function SystemInfoJson: RawUTF8;
var
  cpu, mem, free: RawUTF8;
begin
  cpu := TSystemUse.Current(false).HistoryText(0, 15, @mem);
  if mem = '' then
    free := TSynMonitorMemory.FreeAsText(false, @mem)
  else
    free := TSynMonitorMemory.FreeAsText;
  with SystemInfo do
    result := JSONEncode(['host', ExeVersion.Host, 'user', ExeVersion.User,
      'os', OSVersionText, 'cpu', CpuInfoText, 'bios', BiosInfoText,
      {$ifdef MSWINDOWS}{$ifndef CPU64}'wow64', IsWow64, {$endif}{$endif MSWINDOWS}
      {$ifdef CPUINTEL}'cpufeatures', CpuFeaturesText, {$endif}
      'processcpu', cpu, 'processmem', mem, 'freemem', free,
      'disk', GetDiskPartitionsText(false, true)]);
end;


{ TSystemUse }

procedure TSystemUse.OnTimerExecute(Sender: TObject);
var
  i: PtrInt;
  now: TDateTime;
begin
  if (fProcess = nil) or (fHistoryDepth = 0) or not fProcessInfo.Start then
    exit;
  fTimer := Sender;
  now := NowUTC;
  fSafe.Lock;
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
    fSafe.UnLock;
  end;
end;

constructor TSystemUse.Create(const aProcessID: array of integer;
  aHistoryDepth: integer);
var
  i: integer;
  it, kt, ut: Int64;
begin
  inherited Create;
  fProcesses.Init(TypeInfo(TSystemUseProcessDynArray), fProcess);
  if not RetrieveSystemTimes(it, kt, ut) then
    exit; // no system monitoring API on Linux or oldest Windows
  if aHistoryDepth <= 0 then
    aHistoryDepth := 1;
  fHistoryDepth := aHistoryDepth;
  SetLength(fProcess, length(aProcessID));
  for i := 0 to high(aProcessID) do
  begin
    {$ifdef MSWINDOWS}
    if aProcessID[i] = 0 then
      fProcess[i].ID := GetCurrentProcessID
    else
    {$endif MSWINDOWS}
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
  i, n: integer;
begin
  if self = nil then
    exit;
  {$ifdef MSWINDOWS}
  if aProcessID = 0 then
    aProcessID := GetCurrentProcessID;
  {$endif MSWINDOWS}
  fSafe.Lock;
  try
    n := length(fProcess);
    for i := 0 to n - 1 do
      if fProcess[i].ID = aProcessID then
        exit; // already subscribed
    SetLength(fProcess, n + 1);
    fProcess[n].ID := aProcessID;
    SetLength(fProcess[n].Data, fHistoryDepth);
  finally
    fSafe.UnLock;
  end;
end;

function TSystemUse.Unsubscribe(aProcessID: integer): boolean;
var
  i: integer;
begin
  result := false;
  if self = nil then
    exit;
  fSafe.Lock;
  try
    i := ProcessIndex(aProcessID);
    if i >= 0 then
    begin
      fProcesses.Delete(i);
      result := true;
    end;
  finally
    fSafe.UnLock;
  end;
end;

function TSystemUse.ProcessIndex(aProcessID: integer): integer;
begin // caller should have made fSafe.Enter
  {$ifdef MSWINDOWS}
  if aProcessID = 0 then
    aProcessID := GetCurrentProcessID;
  {$endif MSWINDOWS}
  if self <> nil then
    for result := 0 to high(fProcess) do
      if fProcess[result].ID = aProcessID then
        exit;
  result := -1;
end;

function TSystemUse.Data(out aData: TSystemUseData; aProcessID: integer): boolean;
var
  i: integer;
begin
  result := false;
  if self <> nil then
  begin
    fSafe.Lock;
    try
      i := ProcessIndex(aProcessID);
      if i >= 0 then
      begin
        with fProcess[i] do
          aData := Data[fDataIndex];
        result := aData.Timestamp <> 0;
        if result then
          exit;
      end;
    finally
      fSafe.UnLock;
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
  i, n, last: integer;
begin
  result := nil;
  if self = nil then
    exit;
  fSafe.Lock;
  try
    i := ProcessIndex(aProcessID);
    if i >= 0 then
      with fProcess[i] do
      begin
        n := length(Data);
        last := n - 1;
        if (aDepth > 0) and (n > aDepth) then
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
    fSafe.UnLock;
  end;
end;

function TSystemUse.History(aProcessID, aDepth: integer): TSingleDynArray;
var
  i, n: integer;
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
  if (ProcessSystemUse = nil) and aCreateIfNone then
  begin
    GlobalLock; // paranoid thread-safety
    try
      if ProcessSystemUse = nil then
        ProcessSystemUse := TSystemUse.Create(60);
    finally
      GlobalUnLock;
    end;
  end;
  result := ProcessSystemUse;
end;

function TSystemUse.HistoryText(aProcessID, aDepth: integer;
  aDestMemoryMB: PRawUTF8): RawUTF8;
var
  data: TSystemUseDataDynArray;
  mem: RawUTF8;
  i: integer;
begin
  result := '';
  mem := '';
  data := HistoryData(aProcessID, aDepth);
  {$ifdef LINUXNOTBSD}
  // bsd: see VM_LOADAVG
  // https://www.retro11.de/ouxr/211bsd/usr/src/lib/libc/gen/getloadavg.c.html
  if data = nil then
    result := StringFromFile('/proc/loadavg', {HasNoSize=}true)
  else
  {$endif LINUXNOTBSD}
    for i := 0 to high(data) do
      with data[i] do
      begin
        result := FormatUTF8('%% ', [result, TwoDigits(Kernel + User)]);
        if aDestMemoryMB <> nil then
          mem := FormatUTF8('%% ', [mem, TwoDigits(WorkKB / 1024)]);
      end;
  result := trim(result);
  if aDestMemoryMB <> nil then
    aDestMemoryMB^ := trim(mem);
end;

function TSystemUse.HistoryVariant(aProcessID, aDepth: integer): variant;
var
  res: TDocVariantData absolute result;
  data: TSystemUseDataDynArray;
  i: integer;
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

function GetDiskPartitionsText(nocache, withfreespace, nospace: boolean): RawUTF8;
var
  i: integer;
  parts: TDiskPartitions;

  function GetInfo(var p: TDiskPartition): shortstring;
  const
    F: array[boolean] of RawUTF8 = ('% % (% / %)', '% % (%/%)');
  var
    av, fr, tot: QWord;
  begin
    if not withfreespace or not GetDiskInfo(p.mounted, av, fr, tot) then
      FormatShort('% % (%)', [p.mounted, p.name, KB(p.size, nospace)], result)
    else
      FormatShort(F[nospace],
        [p.mounted, p.name, KB(fr, nospace), KB(tot, nospace)], result);
  end;

begin
  if (_DiskPartitions = nil) or nocache then
  begin
    _DiskPartitions := GetDiskPartitions;
    {$ifndef MSWINDOWS}
    DynArray(TypeInfo(TDiskPartitions), _DiskPartitions).Sort(SortDynArrayDiskPartitions);
    {$endif MSWINDOWS}
  end;
  parts := _DiskPartitions;
  if parts = nil then
    result := ''
  else
    ShortStringToAnsi7String(GetInfo(parts[0]), result);
  for i := 1 to high(parts) do
    result := FormatUTF8('%, %', [result, GetInfo(parts[i])]);
end;


{ TSynMonitorMemory }

constructor TSynMonitorMemory.Create(aTextNoSpace: boolean);
begin
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
  processfree: PRawUTF8): ShortString;
const
  F: array[boolean] of RawUTF8 = ('% / %', '%/%');
begin
  with TSynMonitorMemory.Create(nospace) do
  try
    RetrieveMemoryInfo;
    FormatShort(F[nospace], [fPhysicalMemoryFree.Text, fPhysicalMemoryTotal.Text], result);
    if processfree <> nil then
      FormatUTF8(F[noSpace], [fAllocatedUsed.Text, FAllocatedReserved.Text], processfree^);
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

class function TSynMonitorMemory.ToJSON: RawUTF8;
begin
  with TSynMonitorMemory.Create(false) do
  try
    RetrieveMemoryInfo;
    FormatUTF8('{Allocated:{reserved:%,used:%},Physical:{total:%,free:%,percent:%},' +
      {$ifdef MSWINDOWS}'Virtual:{total:%,free:%},' + {$endif}'Paged:{total:%,free:%}}',
      [fAllocatedReserved.Bytes shr 10, fAllocatedUsed.Bytes shr 10,
      fPhysicalMemoryTotal.Bytes shr 10, fPhysicalMemoryFree.Bytes shr 10,
      fMemoryLoadPercent, {$ifdef MSWINDOWS}fVirtualMemoryTotal.Bytes shr 10,
      fVirtualMemoryFree.Bytes shr 10, {$endif} fPagingFileTotal.Bytes shr 10,
      fPagingFileFree.Bytes shr 10], result);
  finally
    Free;
  end;
end;

class function TSynMonitorMemory.ToVariant: variant;
begin
  result := _JsonFast(ToJSON);
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

class function TSynMonitorDisk.FreeAsText: RawUTF8;
var
  name: TFileName;
  avail, free, total: QWord;
begin
  GetDiskInfo(name, avail, free, total);
  FormatUTF8('% % / %', [name, KB(free), KB(total)], result);
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
      PQWord(@fTotalSize.Bytes)^ {$ifdef MSWINDOWS}, @fVolumeName{$endif});
  end;
end;


initialization

finalization
  ProcessSystemUse.Free;
end.


