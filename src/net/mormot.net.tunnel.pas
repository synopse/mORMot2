/// Network Port Forwarding / Tunnelling
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.tunnel;

{
  *****************************************************************************

   TCP/UDP Port Forwarding and Tunnelling
   - Abstract Definitions for Port Forwarding
   - Local NAT Client/Server to Tunnel TCP Streams
   - Abstract SOA implementation of a Relay Server

  *****************************************************************************

}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.data,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.json,
  mormot.core.threads,
  mormot.core.rtti,
  mormot.core.log,
  mormot.core.datetime,
  mormot.core.variants,
  mormot.core.interfaces,
  mormot.crypt.core,
  mormot.crypt.secure,   // for ICryptCert
  mormot.crypt.ecc256r1, // for ECDHE encryption
  mormot.net.sock;


{ ******************** Abstract Definitions for Port Forwarding }

type
  ETunnel = class(ESynException);

  /// each option available for TTunnelLocal process
  // - toEcdhe computes an ephemeral secret to encrypt the link: combined with
  // toClientSigned/toServerSigned, it offers perfect End-to-end encryption
  // - toClientSigned/toServerSigned will be set by Open() according to the
  // actual certificates available, to ensure endpoint authenticated handshake
  // - if toEcdhe is not set, toEncrypt will ensure a symmetric encryption
  // - only localhost clients are accepted, unless toAcceptNonLocal is set
  TTunnelOption = (
    toEcdhe,
    toClientSigned,
    toServerSigned,
    toEncrypt,
    toAcceptNonLocal);

  /// options for TTunnelLocal process
  TTunnelOptions = set of TTunnelOption;
  PTunnelOptions = ^TTunnelOptions;

  /// a session identifier which should match on both sides of the tunnel
  // - typically a NetRandom32 or a TBinaryCookieGeneratorSessionID value
  TTunnelSession = cardinal;
  PTunnelSession = ^TTunnelSession;

  /// abstract transmission layer with the central relay server
  // - may be implemented as raw sockets or over a mORMot SOA WebSockets link
  // - if toEcdhe or toEncrypt option is set, the frames are already encrypted
  // - named as Tunnel*() methods to be joined as a single service interface,
  // to leverage a single WebSockets callback
  ITunnelTransmit = interface(IInvokable)
    ['{AA661151-81EA-4665-895E-1487EF459AFF}']
    /// main method to emit the supplied binary Frame to the relay server
    // - the raw binary frame always end with 4 bytes of 32-bit TTunnelSession
    // - no result so that the frames could be gathered e.g. over WebSockets
    // - single binary parameter so that could be transmitted as
    // BINARY_CONTENT_TYPE without any base-64 encoding (to be done at WS level)
    procedure TunnelSend(const Frame: RawByteString);
    /// return some information about this connection(s)
    // - as a TDocVariant object for a single connection, or null from the relay
    function TunnelInfo: variant;
  end;
  PITunnelTransmit = ^ITunnelTransmit;
  ITunnelTransmits = array of ITunnelTransmit;
  PITunnelTransmits = ^ITunnelTransmits;

  /// abstract tunneling service implementation
  ITunnelLocal = interface(ITunnelTransmit)
    ['{201150B4-6E28-47A3-AAE5-1335C82B060A}']
    /// match mormot.soa.core IServiceWithCallbackReleased definition
    procedure CallbackReleased(const callback: IInvokable;
      const interfaceName: RawUtf8);
    /// the local port used for the tunnel local process
    function LocalPort: RawUtf8;
    /// the remote port used for the tunnel local process
    function RemotePort: cardinal;
    /// check if the background processing thread is using End-to-end encryption
    function Encrypted: boolean;
  end;

  TTunnelLocal = class;

  /// background thread bound or connected to a local port process
  TTunnelLocalThread = class(TLoggedThread)
  protected
    fSafe: TOSLightLock; // protect especially fClientSock at startup/closure
    fState: (stCreated, stAccepting, stProcessing, stTerminated);
    fStarted: boolean;
    fOwner: TTunnelLocal;
    fTransmit: ITunnelTransmit;
    fSession: TTunnelSession;
    fAes: array[{sending:}boolean] of TAesAbstract;
    fServerSock, fClientSock: TNetSocket;
    fClientAddr: TNetAddr;
    fPort: TNetPort;
    fTimeoutAcceptSecs: cardinal;
    function TransmitSock: TNetSocket;
      {$ifdef HASINLINE} inline; {$endif}
    /// accept/connect the connection, then crypt/redirect to fTransmit
    procedure DoExecute; override;
  public
    /// initialize the thread - called from Open()
    constructor Create(owner: TTunnelLocal; const transmit: ITunnelTransmit;
      const key, iv: THash128; sock: TNetSocket; acceptSecs: cardinal); reintroduce;
    /// release all sockets and encryption state
    destructor Destroy; override;
    /// redirected from TTunnelLocal.Send
    procedure OnReceived(Frame: pointer; FrameLen: PtrInt);
    /// true if internal state is stProcessing, i.e. after accept() and within
    // the main redirection loop
    function Processing: boolean;
      {$ifdef HASINLINE} inline; {$endif}
  published
    /// how much time this background thread should wait for accept()
    property TimeoutAcceptSecs: cardinal
      read fTimeoutAcceptSecs;
  end;

  /// define the wire frame layout for TTunnelLocal optional ECDHE handshake
  TTunnelEcdhFrame = packed record
    /// the 128-bit client or server random nonce
    rnd: TAesBlock;
    /// the public key of this side (33 random bytes if toEcdhe is not set)
    pub: TEccPublicKey;
  end;

  /// identify the TTunnelLocal handshake protocol version
  TTunnelLocalMagic = (
    tlmNone,
    tlmVersion1);

  /// the header of the inital frame exchanged between both TTunnelLocal ends
  // - full header content (therefore the execution context) should match
  // - should be at the end of the handshake, and ending with crc+port
  TTunnelLocalInfo = packed record
    /// protocol version
    magic: TTunnelLocalMagic;
    /// define how this link should be setup
    options: TTunnelOptions;
    /// a genuine 32-bit integer ID
    session: TTunnelSession;
    /// the local port used for communication - may be an ephemeral bound port
    port: word;
    /// SHAKE128 checksum/padding of all previous TTunnelLocalHandshake fields
    // - should be the very last field
    crc: THash128;
  end;

  /// define the inital frame exchanged between both TTunnelLocal ends
  // - its content is signed if toClientSigned/toServerSigned are in options
  TTunnelLocalHandshake = packed record
    /// optional ECDHE information - used if toEcdhe is in Info.Options
    Ecdh: TTunnelEcdhFrame;
    /// a digitally signed header with shared information
    Info: TTunnelLocalInfo;
  end;
  PTunnelLocalHandshake = ^TTunnelLocalHandshake;

  /// abstract tunneling service implementation
  // - is properly implemented by TTunnelLocalServer/TTunnelLocalClient classes
  // - published ITunnelTransmit so that could be used as receival callback
  // - if you release this instance, the tunnel will end
  TTunnelLocal = class(TInterfacedPersistent,
    ITunnelLocal, ITunnelTransmit)
  protected
    fSession: TTunnelSession;
    fSendSafe: TMultiLightLock; // protect fHandshake+fThread
    fPort, fRemotePort: TNetPort;
    fOptions: TTunnelOptions;
    fFlags: set of (fSocketBound, fClosePortNotified);
    fClosed, fVerboseLog: boolean;
    fThread: TTunnelLocalThread;
    fHandshake: TSynQueue;
    fEcdhe: TEccKeyPair;
    fTransmit: ITunnelTransmit;
    fSignCert, fVerifyCert: ICryptCert;
    fBytesIn, fBytesOut, fFramesIn, fFramesOut: Int64;
    fLogClass: TSynLogClass;
    fStartTicks: cardinal;
    fInfo: TDocVariantData;
    // methods to be overriden according to the client/server side
    procedure IncludeOptionsFromCert; virtual; abstract;
    procedure EcdheHashRandom(var hmac: THmacSha256;
      const local, remote: TTunnelEcdhFrame); virtual; abstract;
    // can optionally add a signature to the main handshake frame
    procedure FrameSign(var frame: RawByteString); virtual;
    function FrameVerify(frame: PAnsiChar; framelen, payloadlen: PtrInt): boolean; virtual;
    function GetElapsed: cardinal;
    // can be overriden to customize this class process
    procedure AfterHandshake; virtual;
    procedure OnTunnelInfo(var Info: TDocVariantData); virtual;
  public
    /// initialize the instance for process
    // - if no Context value is supplied, will compute an ephemeral key pair
    // - call Open() to perform actual handshaking and start the background
    // tunnelling thread
    constructor Create(Logger: TSynLogClass = nil;
      SpecificKey: PEccKeyPair = nil); reintroduce;
    /// main method to initialize tunnelling process
    // - Sess genuine integer identifier should match on both sides
    // - Transmit.TunnelSend will be used for sending raw data to the other end
    // - TransmitOptions will be amended to follow SignCert/VerifyCert properties
    // - TimeOutMS is the delay to receive an handshake answer from the other
    // end - this value will be used * 2 as TTunnelLocalThread.fTimeoutAcceptSecs
    // - AppSecret is used during handshake (and toEncrypt with no toEcdhe), and
    // should match on both sides
    // - if Address has a port, will connect a socket to this address:port
    // - if Address has no port, will bound its address an an ephemeral port,
    // which is returned as result for proper client connection
    // - InfoNameValue are name/value pairs of some JSON fields which will be
    // included to ITunnelTransmit.TunnelInfo returned object (e.g. Host name)
    // - SignCert/VerifyCert should have [cuDigitalSignature] usage, and match
    // VerifyCert/SignCert corresponding certificate on other side
    // - raise ETunnel or return 0 on error; return the new local port on sucsess
    // - should be called only once per TTunnelLocal instance
    function Open(Sess: TTunnelSession; const Transmit: ITunnelTransmit;
      TransmitOptions: TTunnelOptions; TimeOutMS: integer; const AppSecret, Address: RawUtf8;
      const InfoNameValue: array of const; const SignCert: ICryptCert = nil;
      const VerifyCert: ICryptCert = nil): TNetPort;
    /// finalize this instance, and its local TCP server
    destructor Destroy; override;
    /// called e.g. by CallbackReleased() or by Destroy
    procedure ClosePort;
  public
    /// ITunnelTransmit method: when a Frame is received from the relay server
    procedure TunnelSend(const aFrame: RawByteString);
    /// ITunnelTransmit method: return a TDocVariant object about this connection
    function TunnelInfo: variant;
    /// ITunnelLocal method: return the local port
    function LocalPort: RawUtf8;
    /// ITunnelLocal method: return the remote port
    function RemotePort: cardinal;
    /// ITunnelLocal method: check if the background thread uses E2EE
    function Encrypted: boolean;
    /// ITunnelLocal method: when a ITunnelTransmit remote callback is finished
    procedure CallbackReleased(const callback: IInvokable;
      const interfaceName: RawUtf8);
    /// access the logging features of this class
    property LogClass: TSynLogClass
      read fLogClass write fLogClass;
    /// log each received frame length for raw debugging
    property VerboseLog: boolean
      read fVerboseLog write fVerboseLog;
    /// raw access to the transmission method - used during testing
    property RawTransmit: ITunnelTransmit
      read fTransmit write fTransmit;
  published
    /// the ephemeral port on the loopback as returned by Open()
    property Port: TNetPort
      read fPort;
    /// the ephemeral port on the remote loopback as returned by Open()
    // on the other side
    property Remote: TNetPort
      read fRemotePort;
    /// the connection specifications, as used by Open()
    property Options: TTunnelOptions
      read fOptions;
    /// the TTunnelSession as supplied to Open()
    property Session: TTunnelSession
      read fSession;
    /// access to the associated background thread processing the data
    property Thread: TTunnelLocalThread
      read fThread;
    /// how many bytes have been received
    property BytesIn: Int64
      read fBytesIn;
    /// how many bytes have been sent
    property BytesOut: Int64
      read fBytesOut;
    /// how many frames have been received
    property FramesIn: Int64
      read fFramesIn;
    /// how many frames have been sent
    property FramesOut: Int64
      read fFramesOut;
    /// number of seconds elapsed since Open()
    property Elapsed: cardinal
      read GetElapsed;
    /// equals true after ClosePort
    property Closed: boolean
      read fClosed;
  end;

function ToText(opt: TTunnelOptions): ShortString; overload;

/// extract the 32-bit session trailer from a ITunnelTransmit.TunnelSend() frame
function FrameSession(const Frame: RawByteString): TTunnelSession;
  {$ifdef HASINLINE} inline; {$endif}

const
  toEncrypted = [toEcdhe, toEncrypt];


{ ******************** Local NAT Client/Server to Tunnel TCP Streams }

type
  /// implements server-side tunneling service
  // - here 'server' or 'client' side does not have any specific meaning - one
  // should just be at either end of the tunnel - but they must appear in a
  // coherent order during the handshake phase by using those overriden methods
  // - class usually assigned to ITunnelConsole in the TTunnelRelay context
  TTunnelLocalServer = class(TTunnelLocal)
  protected
    procedure IncludeOptionsFromCert; override;
    procedure EcdheHashRandom(var hmac: THmacSha256;
      const local, remote: TTunnelEcdhFrame); override;
  end;

  /// implements client-side tunneling service
  // - here 'server' or 'client' side does not have any specific meaning - one
  // should just be at either end of the tunnel - but they must appear in a
  // coherent order during the handshake phase by using those overriden methods
  // - class usually assigned to ITunnelAgent in the TTunnelRelay context
  TTunnelLocalClient = class(TTunnelLocal)
  protected
    procedure IncludeOptionsFromCert; override;
    procedure EcdheHashRandom(var hmac: THmacSha256;
      const local, remote: TTunnelEcdhFrame); override;
  end;

  /// maintain a list of ITunnelTransmit instances
  // - thread-safe storage of sessions, callbacks and pending state together
  // - no ITunnelTransmit callback or interface release happens in fSessionSafe
  TTunnelList = class(TSynPersistent)
  protected
    fSafe: TRWLightLock; // protect fItem[] fSession[] and fPendingSession[]
    fInfoCacheSafe: TLightLock;
    fCount: integer;
    fPendingCount: integer;
    fItem: ITunnelTransmits;
    fSession: TIntegerDynArray; // store TTunnelSession (=cardinal) values
    fPendingSession: TIntegerDynArray; // subset awaiting commit/rollback
    fPendingTix: TIntegerDynArray;     // matching GetTickSec values
    fInfoCache: TVariantDynArray;
    fInfoCacheTix32: cardinal;         // change every second
    fDeprecatedTix4: cardinal;         // check for deprecated every 16 seconds
    function LockedExists(aSession: TTunnelSession): boolean;
    function LockedDeletePending(ndx: PtrInt): boolean;
    function LockedExtract(ndx: PtrInt; out aTunnel: ITunnelTransmit): boolean;
  public
    /// return how many sessions are currently registered
    function Count: integer;
    /// append one ITunnelTransmit callback to the list
    function Add(aSession: TTunnelSession;
      const aInstance: ITunnelTransmit): boolean;
    /// remove one ITunnelTransmit from its session ID
    function Extract(aSession: TTunnelSession; out aTunnel: ITunnelTransmit): boolean;
    /// remove and deleted one ITunnelTransmit from its session ID
    // - i.e. runs Extract() then set aTunnel := nil
    function Delete(aSession: TTunnelSession): boolean;
    /// remove all ITunnelTransmit from another list
    // - returns the number of deleted items
    function DeleteFrom(aList: TTunnelList): integer;
    /// append one callback as a transient/pending session
    function AddTransient(aSession: TTunnelSession;
      const aInstance: ITunnelTransmit): boolean;
    /// check if a session is still transient/pending
    function HasTransient(aSession: TTunnelSession): boolean;
    /// mark a transient session as committed
    function Commit(aSession: TTunnelSession): boolean;
    /// rollback and remove a transient session
    function Rollback(aSession: TTunnelSession): boolean;
    /// purge expired transient sessions and return how many were removed
    function PurgeTransient(aTimeOutSecs: cardinal): integer;
    /// return a stable copy of all current session identifiers
    function SnapshotSessions(out aSessions: TIntegerDynArray;
      aTunnels: PITunnelTransmits = nil): PtrInt;
    /// search if one ITunnelTransmit matches a session ID
    function Exists(aSession: TTunnelSession): boolean;
    /// retrieve one ITunnelTransmit instance from its session ID
    // - make a local thread-safe copy
    function Get(aSession: TTunnelSession; out aTunnel: ITunnelTransmit): boolean;
    /// ask the TunnelInfo of a given session ID as TDocVariant object
    procedure GetInfo(aSession: TTunnelSession; out aInfo: variant);
    /// ask all TunnelInfo of all opended sessions as TDocVariant array
    // - with a one second cache
    function GetAllInfo: TVariantDynArray;
    /// ITunnelTransmit-like method which will redirect the given frame to the
    // expected registered ITunnelTransmit instance
    // - if the Frame does not match any known session, return false
    // - handle end of process notification from the other side with a frame
    // with no payload but just the
    function TunnelSend(const Frame: RawByteString;
      aSession: TTunnelSession = 0): boolean; virtual;
  end;


{ ******************** Abstract SOA implementation of a Relay Server }

type
  /// abstract parent to ITunnelAgent/ITunnelConsole service endpoints
  // - with shared methods to validate or cancel a two-phase startup
  // - here "agent" is a simple TTunnelLocal application opening a localhost
  // port, for transmitting some information (e.g. a VNC server) to a remote
  // "console" with its own TTunnelLocal redirected port (e.g. a VNC viewer)
  // - the steps of a TTunnelRelay session are therefore:
  // 1) TTunnelLocalClient/TTunnelLocalServer.Create as ITunnelTransmit callbacks
  // 2) ITunnelConsole/ITunnelAgent.TunnelPrepare() to retrieve a session ID;
  // 3) ITunnelAgent/ITunnelConsole.TunnelAccept() with this session ID;
  // 4) TTunnelLocal.Open() on the console and agent sides to start tunnelling
  // on a localhost TCP port (as server or client);
  // 5a) ITunnelOpen.TunnelCommit or TunnelRollback against Open() result or
  // 5b) after a timeout, the relay would delete any TunnelPrepare missing
  // proper TunnelAccept or TunnelCommit/TunnelRollback from its internal list
  ITunnelOpen = interface(ITunnelTransmit)
    /// initiate a new relay process as a two-phase commit from this end
    // - caller should call this method, then TTunnelLocal.Open() on its side,
    // and once the handshake is OK or KO, call TunnelCommit or TunnelRollback
    function TunnelPrepare(const callback: ITunnelTransmit): TTunnelSession;
    /// accept a new relay process as a two-phase commit from this end
    // - the relay was initiated by TunnelPrepare on the other end, and the
    // returned  session should be specified to this method
    // - caller should call this method, then TTunnelLocal.Open() on its side,
    // and once the handshake is OK or KO, call TunnelCommit or TunnelRollback
    function TunnelAccept(aSession: TTunnelSession;
      const callback: ITunnelTransmit): boolean;
    /// finalize a relay process startup after Open() success
    // - now ITunnelTransmit.TunnelSend will redirect frames from both sides
    function TunnelCommit(aSession: TTunnelSession): boolean;
    /// abort a relay after Open() failed
    // - now this session will be flushed from the internal list
    function TunnelRollback(aSession: TTunnelSession): boolean;
  end;

  /// service endpoint called by the consoles on the Relay Server
  // - ITunnelAgent/ITunnelConsole.TunnelPrepare() initiates the relay process,
  // and the corresponding ITunnelConsole/ITunnelAgent.TunnelAccept() method
  // setups the connection on the other end
  // - this service also supplies ITunnelTransmit to send remote frames, and is
  // likely to be implemented as sicPerSession over our SOA WebSockets
  // - ITunnelTransmit.TunnelInfo will be implemented and return information
  // about all associated sessions
  // - when the interface is released, will cancel all corresponding sessions
  ITunnelConsole = interface(ITunnelOpen)
    ['{9453C229-9D4A-4F93-B5B3-E4A05E28267F}']
    /// could be used to define a TDocVariant state object about this console
    // - ITunnelConsole.TunnelInfo will include this value, and will be
    // completed with "agents":[] array with each associated agent
    procedure TunnelSetInfo(const info: variant);
  end;

  /// service endpoint called by the agents on the Relay Server
  // - ITunnelAgent/ITunnelConsole.TunnelPrepare() initiates the relay process,
  // and the corresponding ITunnelConsole/ITunnelAgent.TunnelAccept() method
  // setups the connection on the other end
  // - this service also supplies ITunnelTransmit to send remote frames, and is
  // likely to be implemented as sicShared over our SOA WebSockets
  ITunnelAgent = interface(ITunnelOpen)
    ['{B3B39C9F-43AA-4EA0-A88E-662401755AD0}']
  end;

  TTunnelRelay = class;

  /// internal ref-counted state of one multiplexed tunnel endpoint
  // - owns only session routing/transient state and knows nothing about TTunnelRelay
  // - all access is interface-based so no concrete state object ever leaks
  ITunnelOpenState = interface
    ['{CF8D4693-640A-4B9C-9DB4-9A3211D8028E}']
    function Count: integer;
    function HasTransient(aSession: TTunnelSession): boolean;
    function AddTransient(aSession: TTunnelSession;
      const callback: ITunnelTransmit): boolean;
    procedure PurgeTransient;
    function Commit(aSession: TTunnelSession): boolean;
    function Rollback(aSession: TTunnelSession): boolean;
    function Exists(aSession: TTunnelSession): boolean;
    function Delete(aSession: TTunnelSession): boolean;
    function TunnelSend(const Frame: RawByteString): boolean;
    function GetAllInfo: TVariantDynArray;
    procedure SnapshotSessions(out aSessions: TIntegerDynArray);
  end;


  /// abstract parent of TTunnelConsole/TTunnelAgent service endpoints
  // - fState owns session routing while fOwner is the original non-owning relay
  // - all routing state access is performed through ITunnelOpenState
  TTunnelOpen = class(TInterfacedPersistent)
  protected
    fOwner: TTunnelRelay;
    fLogClass: TSynLogClass;
    fTimeOutSecs: cardinal;
    fState: ITunnelOpenState;
    function TunnelCommit(aSession: TTunnelSession): boolean;
    function TunnelRollback(aSession: TTunnelSession): boolean;
  public
    /// initialize this instance for a given TTunnelRelay main instance
    constructor Create(aOwner: TTunnelRelay; aTimeOutSecs: cardinal); reintroduce;
    /// finalize this instance
    destructor Destroy; override;
    /// return fState.Count or 0 if its instance is nil
    function Count: integer;
    /// access to the associated main TTunnelRelay instance
    property Owner: TTunnelRelay
      read fOwner;
    /// how many seconds a TunnelPrepare() would be in the transient/pending queue
    // - auto-trim if no TunnelCommit/TunnelRollback occured within this time slot
    property TimeOutSecs: cardinal
      read fTimeOutSecs;
  end;

  /// implement ITunnelConsole on the Relay Server
  // - one sicPerSession multiplexed control/data endpoint owned by SOA
  TTunnelConsole = class(TTunnelOpen, ITunnelConsole)
  protected
    procedure TunnelSetInfo(const info: variant);
    function TunnelPrepare(const callback: ITunnelTransmit): TTunnelSession;
    function TunnelAccept(aSession: TTunnelSession;
      const callback: ITunnelTransmit): boolean;
    function TunnelInfo: variant;
    procedure TunnelSend(const Frame: RawByteString);
  public
    destructor Destroy; override;
  end;

  /// implement ITunnelAgent on the Relay Server
  // - one sicShared multiplexed control/data endpoint
  TTunnelAgent = class(TTunnelOpen, ITunnelAgent)
  protected
    function TunnelPrepare(const callback: ITunnelTransmit): TTunnelSession;
    function TunnelAccept(aSession: TTunnelSession;
      const callback: ITunnelTransmit): boolean;
    function TunnelInfo: variant;
    procedure TunnelSend(const Frame: RawByteString);
  end;

  /// one console link state registered by TTunnelRelay
  // - State is a stable strong reference independent from ITunnelConsole lifetime
  // - Info is relay-level metadata protected by fConsoleSafe
  TTunnelRelayConsole = record
    State: ITunnelOpenState;
    Info: TDocVariantData;
  end;
  TTunnelRelayConsoles = array of TTunnelRelayConsole;

  /// implement Relay server process
  // - maintain one TTunnelAgent and several TTunnelConsole
  TTunnelRelay = class(TInterfaceResolver)
  protected
    fAgent: TTunnelAgent;              // stable view, owned by fAgentInstance
    fAgentInstance: ITunnelAgent;       // strong ownership of the shared endpoint
    // fConsoleSafe protects only the console registry and metadata
    // lock order is fConsoleSafe -> ITunnelOpenState/TTunnelList, never inverse
    //  no remote/interface callback is invoked while fConsoleSafe is held
    fConsoleSafe: TRWLightLock;
    fConsole: TTunnelRelayConsoles;
    fLogClass: TSynLogClass;
    fConsoleCount: integer;
    fTransientTimeOutSecs: cardinal;
    function LockedFindConsole(aSession: TTunnelSession): ITunnelOpenState;
    function HasConsolePrepared(aSession: TTunnelSession): boolean;
    function PrepareNewSession(const aEndPoint: ITunnelOpenState;
      const callback: ITunnelTransmit): TTunnelSession;
    procedure ConsoleSetInfo(const aConsole: ITunnelOpenState;
      const info: variant);
    procedure ConsoleTunnelSend(const Frame: RawByteString);
    function RemoveConsole(const aConsole: ITunnelOpenState): boolean;
    function TryResolve(aInterface: PRttiInfo; out Obj): boolean; override;
  public
    /// initialize this instance
    constructor Create(aLogClass: TSynLogClass;
      aTransientTimeOutSecs: cardinal = 120); reintroduce;
    /// finalize this instance and its associated nested classes
    destructor Destroy; override;
    /// ask all TunnelInfo of all opended Agent sessions as TDocVariant array
    function AgentsInfo: TVariantDynArray;
    /// ask all TunnelInfo of all opended Console sessions as TDocVariant array
    function ConsolesInfo: TVariantDynArray;
    /// low-level access to the "agents" list
    // - usually published as SOA sicShared ITunnelAgent endpoint
    property Agent: TTunnelAgent
      read fAgent;
    /// low-level access to the "consoles" list - associated with ConsoleCount
    // - these instances are allocated (as SOA sicPerSession) using Resolve()
    property Console: TTunnelRelayConsoles
      read fConsole;
    /// how many items are actually stored in Console[]
    property ConsoleCount: integer
      read fConsoleCount;
    /// how many seconds a TunnelPrepare() would be in the transient/pending queue
    // - auto-trim if no TunnelCommit/TunnelRollback occured within this time slot
    property TransientTimeOutSecs: cardinal
      read fTransientTimeOutSecs;
  end;

implementation


{ ******************** Abstract Definitions for Port Forwarding }

const
  TRAIL_SIZE = SizeOf(TTunnelSession);

function ToText(opt: TTunnelOptions): ShortString;
begin
  GetSetNameShort(TypeInfo(TTunnelOptions), opt, result, {trim=}true);
  LowerCaseShort(result);
end;

function FrameSession(const Frame: RawByteString): TTunnelSession;
var
  l: PtrInt;
begin
  l := length(Frame) - SizeOf(TTunnelSession); // TRAIL_SIZE can't be inlined
  if l >= 0 then
    result := PTunnelSession(@PByteArray(Frame)[l])^
  else
    result := 0;
end;


{ TTunnelLocalThread }

constructor TTunnelLocalThread.Create(owner: TTunnelLocal;
  const transmit: ITunnelTransmit; const key, iv: THash128; sock: TNetSocket;
  acceptSecs: cardinal);
begin
  fSafe.Init; // mandatory for TOSLightLock
  fOwner := owner;
  fPort := owner.Port;
  fSession := owner.Session;
  fTransmit := transmit;
  fTimeoutAcceptSecs := acceptSecs;
  if not IsZero(key) then
  begin
    // ecc256r1 shared secret has 128-bit resolution -> 128-bit AES-CTR
    fAes[{sending:}false] := AesIvUpdatedCreate(mCtr, key, 128, @iv);
    fAes[{sending:}true]  := AesIvUpdatedCreate(mCtr, key, 128, @iv);
    // won't include an IV with each frame, but update it after each frame
  end;
  fServerSock := sock;
  FreeOnTerminate := true;
  inherited Create({susp=}false, nil, nil, fOwner.fLogClass, Make(['tun', fPort]));
end;

destructor TTunnelLocalThread.Destroy;
begin
  Terminate;
  fSafe.Lock;
  try
    if fOwner <> nil then
    try
      fOwner.fThread := nil;
    except
    end;
    fServerSock.ShutdownAndClose({rdwr=}true);
    fClientSock.ShutdownAndClose({rdwr=}true);
  finally
    fSafe.UnLock;
  end;
  inherited Destroy;
  FreeAndNil(fAes[true]);
  FreeAndNil(fAes[false]);
  fSafe.Done; // mandatory for TOSLightLock
end;

function TTunnelLocalThread.Processing: boolean;
begin
  result := (self <> nil) and
            (fState = stProcessing);
end;

function TTunnelLocalThread.TransmitSock: TNetSocket;
begin
  result := fClientSock;    // after Accept()
  if result = nil then
    result := fServerSock;  // after Connect()
end;

procedure TTunnelLocalThread.OnReceived(Frame: pointer; FrameLen: PtrInt);
var
  res: TNetResult;
  data: RawByteString;
begin
  // validate and optionally decrypt the input frame
  if Terminated or
     (Frame = nil) then
    exit;
  if not fSafe.TryLock then
  begin
    fLogClass.Add.Log(sllDebug, 'OnReceived: wait for accept', self);
    fSafe.Lock; // use futex on Linux and Win8+
    fLogClass.Add.Log(sllDebug, 'OnReceived: accepted', self);
  end;
  try
    if fAes[{sending:}false] <> nil then
    begin
      data := fAes[false].DecryptPkcs7Buffer(
        Frame, FrameLen, {ivatbeg=}false, {raise=}false);
      if data = '' then
      begin
        Terminate;
        ETunnel.RaiseUtf8('%.OnReceived(%): decrypt error', [self, fPort]);
      end;
      Frame := pointer(data);
      FrameLen := length(data);
    end;
    // relay the (decrypted) data to the local loopback
    if Terminated then
      exit;
    if fOwner <> nil then
      inc(fOwner.fBytesIn, FrameLen);
    res := TransmitSock.SendAll(Frame, FrameLen, @Terminated);
  finally
    fSafe.UnLock;
  end;
  if (res = nrOk) or
     Terminated then
    exit;
  ETunnel.RaiseUtf8('%.OnReceived(%): error % when retransmitting',
    [self, fPort, _NR[res]]);
  Terminate;
end;

procedure TTunnelLocalThread.DoExecute;
var
  tmp: RawByteString;
  res: TNetResult;
  start: cardinal;
begin
  fStarted := true;
  try
    if (fOwner <> nil) and
       (fSocketBound in fOwner.fFlags) then
    begin
      // newsocket() was done in the main thread: blocking accept() now
      fState := stAccepting;
      fLog.Log(sllTrace,
        'DoExecute: waiting for accept on port %', [fPort], self);
      fSafe.Lock; // protect early fClientSock access in OnReceived()
      try
        start := GetTickSec; // socket timeout is 500ms: use a loop
        repeat
          res := fServerSock.Accept(fClientSock, fClientAddr, {async=}false);
          if (res = nrOk) and
             not Terminated then
          begin
            fLog.Log(sllTrace,
              'DoExecute: accepted %', [fClientAddr.IPShort({port=}true)], self);
            if (toAcceptNonLocal in fOwner.Options) or
               (fClientAddr.IP4 = cLocalhost32) then
             fState := stProcessing // start background process
            else
              fLog.Log(sllWarning, 'DoExecute: rejected non local client', self);
          end;
        until Terminated or
              (fState = stProcessing) or
              (fOwner = nil) or
              (res <> nrRetry) or
              (GetTickSec - start > fTimeoutAcceptSecs);
      finally
        fSafe.UnLock;
      end;
    end
    else
    begin
      // newsocket() with connect() was done in the main thread
      res := nrOk;
      fState := stProcessing;
    end;
    if fState = stProcessing then
      while not Terminated do
      begin
        // wait for some data on the local loopback
        res := TransmitSock.RecvWait(100, tmp, @Terminated);
        case res of
          nrRetry:
            continue;
          nrClosed:
            begin
              fLog.Log(sllTrace, 'DoExecute: closed connection on port %',
                [fPort], self);
              break;
            end;
          nrOK:
            if (tmp <> '') and
               not Terminated then
            begin
              // emit the (encrypted) data with a 32-bit TTunnelSession trailer
              if fOwner <> nil then
                inc(fOwner.fBytesOut, length(tmp)); // size before encryption
              if fAes[{send:}true] <> nil then
                tmp := fAes[true].EncryptPkcs7(tmp, {ivatbeg=}false, TRAIL_SIZE)
              else
                SetLength(tmp, length(tmp) + TRAIL_SIZE);
              PTunnelSession(@PByteArray(tmp)[length(tmp) - TRAIL_SIZE])^ := fSession;
              if (fTransmit <> nil) and
                 not Terminated then
              begin
                if fOwner <> nil then
                  inc(fOwner.fFramesOut);
                fTransmit.TunnelSend(tmp);
              end;
            end;
        else
          ETunnel.RaiseUtf8('%.Execute(%): error % receiving',
            [self, fPort, _NR[res]]);
        end;
      end
    else if not Terminated and
            (fOwner <> nil) then
      if res = nrRetry then
        ETunnel.RaiseUtf8('%.Execute(%): accept timeout after % seconds',
          [self, fPort, fTimeoutAcceptSecs])
      else if res = nrOk then
        ETunnel.RaiseUtf8('%.Execute(%): rejected client %',
          [self, fPort, fClientAddr.IPShort(true)])
      else
        ETunnel.RaiseUtf8('%.Execute(%): accept failed with %',
          [self, fPort, _NR[res]]);
  except
    on E: Exception do
    try
      fLog.Log(sllWarning, 'DoExecute: % aborted due to % [%]',
        [fProcessName, PClass(E)^, E.Message], self);
      if fOwner <> nil then
        fOwner.ClosePort;
    except
      on E2: Exception do
        fLog.Log(sllWarning, 'DoExecute: nested %', [PClass(E2)^], self);
    end;
  end;
  fState := stTerminated;
end;


{ TTunnelLocal }

constructor TTunnelLocal.Create(Logger: TSynLogClass; SpecificKey: PEccKeyPair);
begin
  fLogClass := Logger;
  inherited Create;
  if SpecificKey <> nil then
    fEcdhe := SpecificKey^;
  fHandshake := TSynQueue.Create(TypeInfo(TRawByteStringDynArray));
end;

destructor TTunnelLocal.Destroy;
begin
  if fThread <> nil then
    ClosePort; // calls Terminate
  inherited Destroy;
  FillCharFast(fEcdhe, SizeOf(fEcdhe), 0);
  FreeAndNil(fHandshake); // if Open() was not called
end;

procedure TTunnelLocal.ClosePort;
var
  thread: TTunnelLocalThread;
  frame: RawByteString; // notification frame to unregister to the other side
  callback: TNetSocket; // touch-and-go to the server to release main Accept()
  log: ISynLog;
begin
  if self = nil then
    exit;
  fLogClass.EnterLocal(log, 'ClosePort %', [fPort], self);
  fSendSafe.Lock; // protect fHandshake+fThread
  try
    if not (fClosePortNotified in fFlags) then
      try
        // send frame with only session (and no payload) to notify as closed
        include(fFlags, fClosePortNotified);
        if Assigned(log) then
          log.Log(sllTrace, 'ClosePort: notify other end', self);
        PTunnelSession(FastNewRawByteString(frame, TRAIL_SIZE))^ := fSession;
        if Assigned(fTransmit) then
        begin
          inc(fFramesOut);
          fTransmit.TunnelSend(frame);
        end;
      except
      end;
    thread := fThread;
    if thread <> nil then
      try
        fThread := nil;
        thread.Terminate;
        if thread.fState = stAccepting then
        begin
          if Assigned(log) then
            log.Log(sllDebug, 'ClosePort: release accept', self);
          if NewTcpClientSocket(cLocalhost, UInt32ToUtf8(fPort), 10, callback) = nrOK then
            // Windows socket may not release Accept() until connected
            callback.ShutdownAndClose({rdwr=}false);
        end;
      except
      end;
  finally
    fSendSafe.UnLock;
  end;
  if Assigned(log) then
    log.Log(sllTrace, 'ClosePort: %', [self]); // final statistics
  fPort := 0;
  fClosed := true;
end;

procedure TTunnelLocal.TunnelSend(const aFrame: RawByteString);
var
  l: PtrInt;
  p: PAnsiChar;
begin
  // ITunnelTransmit method: when a Frame is received from the relay server
  l := length(aFrame);
  if fVerboseLog then
    fLogClass.Add.Log(sllTrace, 'TunnelSend=%', [l]);
  dec(l, TRAIL_SIZE);
  if l < 0 then
    ETunnel.RaiseUtf8('%.Send: unexpected size=%', [self, l]);
  fSendSafe.Lock; // protect fHandshake+fThread
  try
    inc(fFramesIn);
    if fHandshake <> nil then
    begin
      fLogClass.Add.Log(sllTrace, 'TunnelSend: into Handshake queue', self);
      fHandshake.Push(aFrame); // during the handshake phase - maybe before Open
      exit;
    end;
    p := pointer(aFrame);
    if PTunnelSession(p + l)^ <> fSession then
      ETunnel.RaiseUtf8('%.Send: session mismatch', [self]);
    if l = 0 then
    begin
      // received frame with only session (and no payload) to notify as closed
      include(fFlags, fClosePortNotified);
      ClosePort;
    end
    else if fThread <> nil then // = nil after ClosePort (too late)
      fThread.OnReceived(p, l) // regular tunelling process
    else
      fLogClass.Add.Log(sllWarning, 'TunnelSend: Thread=nil', self); // unlikely
  finally
    fSendSafe.UnLock;
  end;
end;

procedure TTunnelLocal.CallbackReleased(const callback: IInvokable;
  const interfaceName: RawUtf8);
begin
  if not IdemPChar(pointer(interfaceName), 'ITUNNEL') then
    exit; // should be ITunnelLocal or ITunnelTransmit
  include(fFlags, fClosePortNotified); // no need to notify the remote end
  ClosePort;
end;

procedure TTunnelLocal.FrameSign(var frame: RawByteString);
begin
  if fSignCert <> nil then
    Append(frame, fSignCert.Sign(frame));
end;

function TTunnelLocal.FrameVerify(frame: PAnsiChar; framelen, payloadlen: PtrInt): boolean;
begin
  dec(framelen, payloadlen);
  result := (framelen >= 0) and
            ((fVerifyCert = nil) or
             (fVerifyCert.Verify(frame + payloadlen, frame, framelen, payloadlen)
               in CV_VALIDSIGN));
end;

function TTunnelLocal.GetElapsed: cardinal;
begin
  if (self = nil) or
     (fStartTicks = 0) then
    result := 0
  else
    result := GetUptimeSec - fStartTicks; // in seconds
end;

procedure TunnelHandshakeCrc(const Handshake: TTunnelLocalHandshake;
  const appsecret: RawUtf8; out crc: THash128);
var
  sha3: TSha3;
begin
  sha3.Init(SHAKE_128); // 128-bit digital signature of Handshake.Ecdh+Info
  if appsecret = '' then
    sha3.Update('AB15C52F754F49CB9B23CF88735E39C8') // some default secret
  else
    sha3.Update(appsecret); // custom symmetric application-specific secret
  sha3.Update(@Handshake, SizeOf(Handshake) - SizeOf(Handshake.Info.crc));
  sha3.Final(@crc, 128);
end;

function TTunnelLocal.Open(Sess: TTunnelSession; const Transmit: ITunnelTransmit;
  TransmitOptions: TTunnelOptions; TimeOutMS: integer; const AppSecret, Address: RawUtf8;
  const InfoNameValue: array of const; const SignCert, VerifyCert: ICryptCert): TNetPort;
var
  uri: TUri;
  sock: TNetSocket;
  addr: TNetAddr;
  l, li: PtrInt;
  frame, remote, info: RawByteString;
  infoaes: TAesCtr;
  rem: PTunnelLocalHandshake absolute remote;
  loc: TTunnelLocalHandshake;
  key, iv: THash256Rec;
  hmac, hmac2: THmacSha256;
  hqueue: TSynQueue;
  thread: TTunnelLocalThread;
  log: ISynLog;
const // port is asymmetrical so not included to the KDF - nor the crc
  KDF_SIZE = SizeOf(loc.Info) - (SizeOf(loc.Info.port) + SizeOf(loc.Info.crc));
  SUFFIX_SIZE = TRAIL_SIZE + SizeOf(word);
begin
  // validate input parameters
  fRemotePort := 0;
  fInfo.Clear;
  fSession := Sess;
  fSignCert := SignCert;
  fVerifyCert := VerifyCert;
  fOptions := TransmitOptions - [toClientSigned, toServerSigned];
  IncludeOptionsFromCert; // adjust from fSignCert/fVerifyCert
  if fLogClass <> nil then
    fLogClass.EnterLocal(log, 'Open(%,[%])', [Int64(Sess), ToText(fOptions)], self);
  if (fPort <> 0) or
     (not Assigned(Transmit)) then
    ETunnel.RaiseUtf8('%.Open invalid call', [self]);
  if not uri.From(Address, '0') then
    ETunnel.RaiseUtf8('%.Open invalid %', [self, Address]);
  fTransmit := Transmit;
  // bind to a local (ephemeral) port
  if (fThread <> nil) or
     (fHandshake = nil) then
    ETunnel.RaiseUtf8('%.Open called twice', [self]);
  fPort := 0;
  fFlags := [];
  result := uri.PortInt;
  if result = 0 then
  begin
    // bind on port='0' = ephemeral port
    ENetSock.Check(NewSocket(uri.Server, uri.Port, nlTcp, {bind=}true,
      500, 500, 500, {retry=}0, sock, @addr), 'Open');
    include(fFlags, fSocketBound);
    result := addr.Port;
    if Assigned(log) then
      log.Log(sllTrace, 'Open: bound to %', [addr.IPShort(true)], self);
  end
  else
  begin
    // connect to a local socket on address:port
    ENetSock.Check(
      NewTcpClientSocket(uri.Server, uri.Port, TimeOutMS, sock, @addr), 'Open');
    if Assigned(log) then
      log.Log(sllTrace, 'Open: connected to %:%', [uri.Server, uri.Port], self);
  end;
  // initial single round trip handshake
  infoaes := nil;
  try
    // header with optional ECDHE
    loc.Info.magic   := tlmVersion1;
    loc.Info.options := fOptions;
    loc.Info.session := fSession; // is typically an increasing sequence number
    loc.Info.port    := result;
    Random128(@loc.Ecdh.rnd);     // unpredictable
    if toEcdhe in fOptions then
    begin
      if IsZero(fEcdhe.pub) then  // ephemeral key was not specified at Create
      begin
        if Assigned(log) then
          log.Log(sllTrace, 'Open: generate ECHDE key', self);
        if not Ecc256r1MakeKey(fEcdhe.pub, fEcdhe.priv) then
          ETunnel.RaiseUtf8('%.Open: no ECC engine available', [self]);
      end;
      loc.Ecdh.pub := fEcdhe.pub;
    end
    else
      SharedRandom.Fill(@loc.Ecdh.pub, SizeOf(loc.Ecdh.pub)); // some padding
    TunnelHandshakeCrc(loc, AppSecret, loc.Info.crc);
    FastSetRawByteString(frame, @loc, SizeOf(loc));
    FrameSign(frame); // optional digital signature
    // append (and potentially encrypt) the info JSON payload
    info := JsonEncode(InfoNameValue);
    if toEncrypted * fOptions <> [] then // toEncrypt or toEcdhe
    begin
      if AppSecret = '' then
        hmac.Init('705FC9676148405B91A66FFE7C3B54AA') // some minimal key
      else
        hmac.Init(AppSecret);
      hmac.Update(@loc.Info, KDF_SIZE); // no replay (sequential session)
      hmac2 := hmac; // use hmac2 to compute info KDF
      hmac2.Done(key.b);
      infoaes := TAesCtr.Create(key.Lo, 128, @key.Hi); // simple encryption
      info := infoaes.EncryptPkcs7(info, {ivatbeg=}false);
      infoaes.IV := key.Hi; // use the same IV for decoding "remote" info below
    end;
    li := length(info);
    if li > 65535 then
      ETunnel.RaiseUtf8('Open: too much info (len=%)', [li]);
    // actually send the frame ending with its session ID
    Append(frame, [info, 'lisess']); // li:W sess:U32 = SUFFIX_SIZE
    l := length(frame);
    PWord(@PByteArray(frame)^[l - SUFFIX_SIZE])^ := li;
    PTunnelSession(@PByteArray(frame)^[l - TRAIL_SIZE])^ := fSession;
    inc(fFramesOut);
    fTransmit.TunnelSend(frame);
    if Assigned(log) then
      log.Log(sllTrace, 'Open: sent % - wait for answer', [length(frame)], self);
    // this method will wait until both sides sent a valid signed header
    if not fHandshake.WaitPop(TimeOutMS, nil, remote) then
      ETunnel.RaiseUtf8('Open: handshake %ms timeout on port %', [TimeOutMS, result]);
    if Assigned(log) then
      log.Log(sllTrace, 'Open: received len=%', [length(remote)], self);
    // ensure the returned frame is for this session
    if FrameSession(remote) <> fSession then
      ETunnel.RaiseUtf8('Open: wrong handshake trailer on port %', [result]);
    // extract (and potentially decrypt) the associated JSON info payload
    l := length(remote) - SUFFIX_SIZE;
    li := PWord(@PByteArray(remote)^[l])^;
    FastSetRawByteString(info, @PByteArray(remote)^[l - li], li);
    if infoaes <> nil then
      info := infoaes.DecryptPkcs7(info, {ivatbeg=}false);
    if fInfo.InitJsonInPlace(pointer(info), JSON_FAST) = nil then
      ETunnel.RaiseUtf8('Open: invalid JSON info on port %', [result]);
    // check the digital signature, maybe using the certificate
    dec(l, li);
    if l < SizeOf(loc) then // may have a signature trailer
      ETunnel.RaiseUtf8('Open: wrong handshake size=% on port %', [l, result]);
    if not CompareMemSmall(@rem.Info, @loc.Info, KDF_SIZE) then
      ETunnel.RaiseUtf8('Open: unexpected handshake on port %', [result]);
    TunnelHandshakeCrc(rem^, AppSecret, key.Lo);
    if not IsEqual(rem^.Info.crc, key.Lo) then
      ETunnel.RaiseUtf8('Open: invalid handshake signature on port %', [result]);
    if not FrameVerify(pointer(remote), l, SizeOf(loc)) then
      ETunnel.RaiseUtf8('Open: handshake failed on port %', [result]);
    fRemotePort := rem^.Info.port;
    // optional encryption - maybe with ECDHE ephemeral keys
    FillZero(key.b);
    if toEncrypted * fOptions <> [] then // toEncrypt or toEcdhe
    begin
      // hmac has been pre-computed above with loc.Info and AppSecret
      EcdheHashRandom(hmac, loc.Ecdh, rem^.Ecdh); // rnd+pub in same order
      if toEcdhe in fOptions then
      begin
        if Assigned(log) then
          log.Log(sllTrace, 'Open: compute ECDHE shared secret', self);
        if not Ecc256r1SharedSecret(rem^.Ecdh.pub, fEcdhe.priv, key.b) then
          exit;
        hmac.Update(key.b); // prime256v1 shared secret
      end;
      hmac2 := hmac;     // two labeled hmacs - see NIST SP 800-108
      hmac.Update('AES key'#0);
      hmac.Done(key.b);   // AES-128-CTR key
      hmac2.Update('IV'#1);
      hmac2.Done(iv.b);     // AES-128-CTR iv
    end;
    // launch the background processing thread
    fPort := result;
    TimeOutMS := (TimeOutMS shr 10) + 5; // minimal coherent accept time
    thread := TTunnelLocalThread.Create(
      self, fTransmit, key.Lo, iv.Lo, sock, TimeOutMS);
    SleepHiRes(100, thread.fStarted);
    if Assigned(log) then
      log.Log(sllTrace, 'Open: started=% %',
        [BOOL_STR[thread.fStarted], thread], self);
    fStartTicks := GetUptimeSec; // wall clock
    hqueue := fHandshake;
    fSendSafe.Lock; // re-entrant for TunnelSend()
    try
      fThread := thread;   // starts the normal tunnelling phase
      fHandshake := nil;   // ends the handshaking phase
      while hqueue.Pop(frame) do
      begin
        if Assigned(log) then
          log.Log(sllDebug, 'Open: delayed frame len=%', [length(frame)], self);
        TunnelSend(frame); // paranoid: redirect to this instance
      end;
    finally
      fSendSafe.UnLock;
      hqueue.Free;
    end;
    // now everything is running and we can prepare the fixed info
    fInfo.AddNameValuesToObject([
      'remotePort', fRemotePort,
      'localPort',  fPort,
      'started',    NowUtcToString,
      'session',    Int64(fSession),
      'encrypted',  Encrypted,
      'options',    ToText(fOptions)]);
    AfterHandshake; // may be overriden e.g. to customize fInfo
    if Assigned(log) then
      log.Log(sllTrace, 'Open=% %', [result, variant(fInfo)], self);
  except
    sock.ShutdownAndClose(true); // any error would abort and return 0
    result := 0;
  end;
  infoaes.Free;
  FillZero(key.b);
  FillZero(iv.b);
end;

procedure TTunnelLocal.AfterHandshake;
begin
  // do nothing by default, but could perform some custom process e.g. on fInfo
end;

function TTunnelLocal.LocalPort: RawUtf8;
begin
  if (self = nil) or
     (fPort = 0) then
    FastAssignNew(result)
  else
    UInt32ToUtf8(fPort, result);
end;

function TTunnelLocal.RemotePort: cardinal;
begin
  result := fRemotePort;
end;

function TTunnelLocal.Encrypted: boolean;
begin
  result := (self <> nil) and
            (fThread <> nil) and
            (fThread.fAes[false] <> nil);
end;

procedure TTunnelLocal.OnTunnelInfo(var Info: TDocVariantData);
begin
  // do nothing by default, but could remove or add some fields
end;

function TTunnelLocal.TunnelInfo: variant;
var
  dv: TDocVariantData absolute result;
begin
  VarClear(result);
  if fPort = 0 then
    exit;
  dv.InitFast(fInfo.Count + 7, dvObject);
  dv.AddFrom(fInfo);         // fixed values
  dv.AddNameValuesToObject([ // evolving values
    'elapsed',   GetElapsed,
    'bytesIn',   fBytesIn,
    'bytesOut',  fBytesOut,
    'framesIn',  fFramesIn,
    'framesOut', fFramesOut]);
  OnTunnelInfo(dv);
end;


{ ******************** Local NAT Client/Server to Tunnel TCP Streams }

{ TTunnelLocalServer }

procedure TTunnelLocalServer.IncludeOptionsFromCert;
begin
  if Assigned(fVerifyCert) then
    include(fOptions, toClientSigned); // client signature
  if Assigned(fSignCert) then
    include(fOptions, toServerSigned); // server signature
end;

procedure TTunnelLocalServer.EcdheHashRandom(var hmac: THmacSha256;
  const local, remote: TTunnelEcdhFrame);
begin
  hmac.Update(@remote.rnd, SizeOf(remote.rnd)); // client random
  hmac.Update(@local. rnd, SizeOf(local.rnd));  // server random
end;


{ TTunnelLocalClient }

procedure TTunnelLocalClient.IncludeOptionsFromCert;
begin
  if Assigned(fSignCert) then
    include(fOptions, toClientSigned); // client signature
  if Assigned(fVerifyCert) then
    include(fOptions, toServerSigned); // server signature
end;

procedure TTunnelLocalClient.EcdheHashRandom(var hmac: THmacSha256;
  const local, remote: TTunnelEcdhFrame);
begin
  hmac.Update(@local. rnd, SizeOf(local.rnd));  // client random
  hmac.Update(@remote.rnd, SizeOf(remote.rnd)); // server random
end;


{ TTunnelList }

function TTunnelList.LockedExists(aSession: TTunnelSession): boolean;
begin
  result := IntegerScanExists(pointer(fSession), fCount, aSession);
end;

function TTunnelList.LockedDeletePending(ndx: PtrInt): boolean;
begin
  result := false;
  if PtrUInt(ndx) >= PtrUInt(fPendingCount) then
    exit;
  DeleteInteger(fPendingSession, fPendingCount, ndx);
  UnmanagedDynArrayDelete(fPendingTix, fPendingCount, ndx, SizeOf(cardinal));
  result := true;
end;

function TTunnelList.LockedExtract(ndx: PtrInt;
  out aTunnel: ITunnelTransmit): boolean;
var
  session: TTunnelSession;
  pending: PtrInt;
begin
  result := false;
  if PtrUInt(ndx) >= PtrUInt(fCount) then
    exit;
  session := fSession[ndx];
  if not InterfaceArrayExtract(fItem, ndx, aTunnel) then // weak transfer
    exit;
  DeleteInteger(fSession, fCount, ndx);
  pending := IntegerScanIndex(pointer(fPendingSession), fPendingCount, session);
  if pending >= 0 then
    LockedDeletePending(pending);
  result := true;
end;

function TTunnelList.Count: integer;
begin
  result := fCount; // only informative, no need to be atomic
end;

function TTunnelList.Exists(aSession: TTunnelSession): boolean;
begin
  result := false;
  if aSession = 0 then
    exit;
  fSafe.ReadLock;
  try
    result := LockedExists(aSession);
  finally
    fSafe.ReadUnLock;
  end;
end;

function TTunnelList.Add(aSession: TTunnelSession;
  const aInstance: ITunnelTransmit): boolean;
begin
  result := false;
  if (aInstance = nil) or
     (aSession = 0) then
    exit;
  fSafe.WriteLock;
  try
    if LockedExists(aSession) then
      exit;
    AddInteger(fSession, fCount, aSession);
    InterfaceArrayAdd(fItem, aInstance);
    result := true;
  finally
    fSafe.WriteUnLock;
  end;
  fInfoCacheTix32 := 0; // flush cache info
end;

function TTunnelList.AddTransient(aSession: TTunnelSession;
  const aInstance: ITunnelTransmit): boolean;
var
  n: PtrInt;
  tix32: cardinal;
begin
  result := false;
  if (aInstance = nil) or
     (aSession = 0) then
    exit;
  fSafe.WriteLock;
  try
    if LockedExists(aSession) then
      exit;
    tix32 := GetTickSec;
    AddInteger(fSession, fCount, aSession);
    InterfaceArrayAdd(fItem, aInstance);
    n := fPendingCount;
    AddInteger(fPendingSession, fPendingCount, aSession);
    if fPendingCount >= length(fPendingTix) then
      SetLength(fPendingTix, length(fPendingSession));
    fPendingTix[n] := tix32;
    result := true;
  finally
    fSafe.WriteUnLock;
  end;
end;

function TTunnelList.Extract(aSession: TTunnelSession;
  out aTunnel: ITunnelTransmit): boolean;
var
  ndx: PtrInt;
begin
  result := false;
  if aSession = 0 then
    exit;
  fSafe.WriteLock;
  try
    ndx := IntegerScanIndex(pointer(fSession), fCount, aSession);
    if ndx >= 0 then
      result := LockedExtract(ndx, aTunnel);
  finally
    fSafe.WriteUnLock;
  end;
  if result then
    fInfoCacheTix32 := 0; // flush cache info
end;

function TTunnelList.Delete(aSession: TTunnelSession): boolean;
var
  tunnel: ITunnelTransmit;
begin
  result := Extract(aSession, tunnel);
  if not result then
    exit;
  fInfoCacheTix32 := 0; // flush cache info
  InterfaceNilSafe(tunnel); // final release outside fSafe
end;

function TTunnelList.DeleteFrom(aList: TTunnelList): integer;
var
  i: PtrInt;
  sessions: TIntegerDynArray;
begin
  result := 0;
  if (aList = nil) or
     (aList = self) then
    exit;
  aList.SnapshotSessions(sessions);
  for i := 0 to high(sessions) do
    if Delete(sessions[i]) then
      inc(result);
  if result <> 0 then
    fInfoCacheTix32 := 0; // flush cache info
end;

function TTunnelList.HasTransient(aSession: TTunnelSession): boolean;
begin
  result := false;
  if aSession = 0 then
    exit;
  fSafe.ReadLock;
  try
    result := IntegerScanExists(pointer(fPendingSession), fPendingCount, aSession);
  finally
    fSafe.ReadUnLock;
  end;
end;

function TTunnelList.Commit(aSession: TTunnelSession): boolean;
var
  ndx: PtrInt;
begin
  result := false;
  if aSession = 0 then
    exit;
  fSafe.WriteLock;
  try
    ndx := IntegerScanIndex(pointer(fPendingSession), fPendingCount, aSession);
    if ndx >= 0 then
      result := LockedDeletePending(ndx);
  finally
    fSafe.WriteUnLock;
  end;
end;

function TTunnelList.Rollback(aSession: TTunnelSession): boolean;
var
  ndx: PtrInt;
  tunnel: ITunnelTransmit;
begin
  result := false;
  if aSession = 0 then
    exit;
  fSafe.WriteLock;
  try
    if IntegerScanExists(pointer(fPendingSession), fPendingCount, aSession) then
    begin
      ndx := IntegerScanIndex(pointer(fSession), fCount, aSession);
      if ndx >= 0 then
        result := LockedExtract(ndx, tunnel);
    end;
  finally
    fSafe.WriteUnLock;
  end;
  if tunnel = nil then
    exit;
  fInfoCacheTix32 := 0; // flush cache info
  InterfaceNilSafe(tunnel); // final release outside fSafe
end;

function TTunnelList.PurgeTransient(aTimeOutSecs: cardinal): integer;
var
  tix32, tix4: cardinal;
  i, n, ndx: PtrInt;
  session: TTunnelSession;
  tunnel: ITunnelTransmit;
  garbage: ITunnelTransmits;
begin
  result := 0;
  if aTimeOutSecs = 0 then
    exit;
  tix32 := GetTickSec;
  tix4 := tix32 shr 4; // searching only every 16 seconds is enough
  if fDeprecatedTix4 = tix4 then
    exit;
  fSafe.WriteLock;
  try
    if fDeprecatedTix4 = tix4 then
      exit;
    fDeprecatedTix4 := tix4;
    i := fPendingCount - 1;
    while i >= 0 do
    begin
      if cardinal(fPendingTix[i]) + aTimeOutSecs < tix32 then
      begin
        session := fPendingSession[i];
        ndx := IntegerScanIndex(pointer(fSession), fCount, session);
        if (ndx >= 0) and
           LockedExtract(ndx, tunnel) then
        begin
          // keep a strong local copy so final _Release happens only after unlock
          n := length(garbage);
          SetLength(garbage, n + 1);
          garbage[n] := tunnel;
          InterfaceNilSafe(tunnel);
          inc(result);
        end
        else
          LockedDeletePending(i); // keep subset consistent on any stale entry
      end;
      dec(i);
    end;
  finally
    fSafe.WriteUnLock;
  end;
  fInfoCacheTix32 := 0; // flush cache info
  garbage := nil; // final interface releases outside fSafe
end;

function TTunnelList.SnapshotSessions(out aSessions: TIntegerDynArray;
  aTunnels: PITunnelTransmits): PtrInt;
begin
  fSafe.ReadLock;
  try
    result := fCount;
    aSessions := copy(fSession, 0, result);
    if aTunnels <> nil then
      aTunnels^ := copy(fItem, 0, result);
  finally
    fSafe.ReadUnLock;
  end;
end;

function TTunnelList.Get(aSession: TTunnelSession;
  out aTunnel: ITunnelTransmit): boolean;
var
  ndx: PtrInt;
begin
  result := false;
  if aSession = 0 then
    exit;
  fSafe.ReadLock;
  try
    ndx := IntegerScanIndex(pointer(fSession), fCount, aSession);
    if ndx < 0 then
      exit;
    aTunnel := fItem[ndx]; // strong local copy
    result := true;
  finally
    fSafe.ReadUnLock;
  end;
end;

function TTunnelList.TunnelSend(const Frame: RawByteString;
  aSession: TTunnelSession): boolean;
var
  tunnel: ITunnelTransmit;
begin
  result := false;
  if aSession = 0 then
    aSession := FrameSession(Frame);
  if Get(aSession, tunnel) then
    try
      tunnel.TunnelSend(Frame); // callback outside fSafe
      result := true;
    finally
      if length(Frame) = TRAIL_SIZE then
        Delete(aSession); // alwayd perform end-of-session cleanup
    end;
end;

procedure TTunnelList.GetInfo(aSession: TTunnelSession; out aInfo: variant);
var
  tunnel: ITunnelTransmit;
begin
  if Get(aSession, tunnel) then
    aInfo := tunnel.TunnelInfo; // callback outside fSafe
end;

function TTunnelList.GetAllInfo: TVariantDynArray;
var
  n, i: PtrInt;
  tix32: cardinal;
  sessions: TIntegerDynArray;
  tunnels: ITunnelTransmits;
begin
  result := nil;
  if fCount = 0 then
    exit;
  tix32 := GetTickSec;
  if tix32 = fInfoCacheTix32 then // fast path to the cached last value
  begin
    fInfoCacheSafe.Lock;
    try
      if (fInfoCache <> nil) and
         (tix32 = fInfoCacheTix32) then
      begin
        result := fInfoCache;
        exit;
      end;
    finally
      fInfoCacheSafe.UnLock;
    end;
  end;
  n := SnapshotSessions(sessions, @tunnels);
  if n = 0 then
    exit;
  SetLength(result, n);
  for i := 0 to n - 1 do
    try
      result[i] := tunnels[i].TunnelInfo; // all callbacks outside fSafe
    except
      Delete(sessions[i]); // catastrophic cleanup of unstable links
    end;
  fInfoCacheSafe.Lock;
  try
    fInfoCache := result;
    fInfoCacheTix32 := tix32; // set last
  finally
    fInfoCacheSafe.UnLock;
  end;
end;


{ ******************** Abstract SOA implementation of a Relay Server }

type
/// implementation of ITunnelOpenState
  // - owns one TTunnelList whose single lock protects sessions, callbacks and
  // transient timestamps together
  // - concrete implementation never leaks outside this unit
  TTunnelOpenState = class(TInterfacedObject, ITunnelOpenState)
  protected
    fLogClass: TSynLogClass;
    fList: TTunnelList;
    fTimeOutSecs: cardinal;
    // ITunnelOpenState
    function Count: integer;
    function HasTransient(aSession: TTunnelSession): boolean;
    function AddTransient(aSession: TTunnelSession;
      const callback: ITunnelTransmit): boolean;
    procedure PurgeTransient;
    function Commit(aSession: TTunnelSession): boolean;
    function Rollback(aSession: TTunnelSession): boolean;
    function Exists(aSession: TTunnelSession): boolean;
    function Delete(aSession: TTunnelSession): boolean;
    function TunnelSend(const Frame: RawByteString): boolean;
    function GetAllInfo: TVariantDynArray;
    procedure SnapshotSessions(out aSessions: TIntegerDynArray);
  public
    constructor Create(aLogClass: TSynLogClass; aTimeOutSecs: cardinal); reintroduce;
    destructor Destroy; override;
  end;

{ TTunnelRelay }

constructor TTunnelRelay.Create(aLogClass: TSynLogClass;
  aTransientTimeOutSecs: cardinal);
begin
  fLogClass := aLogClass;
  fLogClass.Add.Log(sllDebug, 'Create timeout=%', [aTransientTimeOutSecs], self);
  fTransientTimeOutSecs := aTransientTimeOutSecs;
  fAgent := TTunnelAgent.Create(self, fTransientTimeOutSecs);
  fAgentInstance := fAgent; // strong ownership of this shared endpoint
end;

destructor TTunnelRelay.Destroy;
var
  consoles: TTunnelRelayConsoles;
begin
  if fAgent <> nil then
    fLogClass.Add.Log(sllDebug, 'Destroy: AgentCount=% ConsoleCount=%',
      [fAgent.Count, fConsoleCount], self)
  else
    fLogClass.Add.Log(sllDebug, 'Destroy: AgentCount=0 ConsoleCount=%',
      [fConsoleCount], self);
  // atomically detach the whole registry, then finalize interfaces/variants later
  fConsoleSafe.WriteLock;
  try
    consoles := fConsole;
    fConsole := nil;
    fConsoleCount := 0;
  finally
    fConsoleSafe.WriteUnLock;
  end;
  if fAgent <> nil then
    fAgent.fOwner := nil; // fAgentInstance keeps this raw pointer valid here
  fAgent := nil;
  fAgentInstance := nil;
  consoles := nil; // state/interface releases outside fConsoleSafe
  inherited Destroy;
end;

function TTunnelRelay.HasConsolePrepared(aSession: TTunnelSession): boolean;
var
  i: PtrInt;
begin
  result := false;
  if (self = nil) or
     (aSession = 0) then
    exit;
  fConsoleSafe.ReadLock;
  try
    for i := 0 to fConsoleCount - 1 do
      if fConsole[i].State.HasTransient(aSession) then
      begin
        result := true;
        exit;
      end;
  finally
    fConsoleSafe.ReadUnLock;
  end;
  fLogClass.Add.Log(sllTrace, 'HasConsolePrepared(%)=false',
    [Int64(aSession)], self);
end;

function TTunnelRelay.LockedFindConsole(
  aSession: TTunnelSession): ITunnelOpenState;
var
  i: PtrInt;
begin
  result := nil;
  for i := 0 to fConsoleCount - 1 do
    if fConsole[i].State.Exists(aSession) then
    begin
      result := fConsole[i].State;
      exit;
    end;
end;

function TTunnelRelay.PrepareNewSession(const aEndPoint: ITunnelOpenState;
  const callback: ITunnelTransmit): TTunnelSession;
var
  n: integer;
begin
  result := 0;
  if (self = nil) or
     (aEndPoint = nil) or
     (callback = nil) or
     (fAgent = nil) then
    exit;
  // one relay-level writer serializes all session-ID allocations
  fConsoleSafe.WriteLock;
  try
    for n := 1 to 50 do
    begin
      repeat
        result := NetRandom32 shr 4;
      until result <> 0;
      if not fAgent.fState.Exists(result) and
         (LockedFindConsole(result) = nil) then
        break;
      result := 0;
      fLogClass.Add.Log(sllDebug, 'TunnelPrepare: collision #%', [n], self);
    end;
    if (result <> 0) and
       not aEndPoint.AddTransient(result, callback) then
      result := 0;
  finally
    fConsoleSafe.WriteUnLock;
  end;
  if result <> 0 then
    aEndPoint.PurgeTransient; // may release callbacks, so outside fConsoleSafe
end;

procedure TTunnelRelay.ConsoleSetInfo(const aConsole: ITunnelOpenState;
  const info: variant);
var
  i: PtrInt;
begin
  if (self = nil) or
     (aConsole = nil) then
    exit;
  fConsoleSafe.WriteLock;
  try
    for i := 0 to fConsoleCount - 1 do
      if fConsole[i].State = aConsole then
      begin
        fConsole[i].Info.Clear;
        fConsole[i].Info := _Safe(info)^;
        exit;
      end;
  finally
    fConsoleSafe.WriteUnLock;
  end;
end;

procedure TTunnelRelay.ConsoleTunnelSend(const Frame: RawByteString);
var
  s: TTunnelSession;
  state: ITunnelOpenState;
begin
  s := FrameSession(Frame);
  if (s = 0) or
     (self = nil) then
    exit;
  // retain the matching state only; actual callback happens after the relay lock
  fConsoleSafe.ReadLock;
  try
    state := LockedFindConsole(s);
  finally
    fConsoleSafe.ReadUnLock;
  end;
  if (state = nil) or
     not state.TunnelSend(Frame) then
    fLogClass.Add.Log(sllDebug, 'ConsoleTunnelSend(%): unknown session',
      [Int64(s)], self);
end;


function TTunnelRelay.TryResolve(aInterface: PRttiInfo; out Obj): boolean;
var
  n: PtrInt;
  c: TTunnelConsole;
  console: ITunnelConsole;
begin
  result := false;
  if aInterface = TypeInfo(ITunnelConsole) then
  begin
    c := TTunnelConsole.Create(self, fTransientTimeOutSecs);
    console := c; // SOA endpoint lifetime remains independent from relay state
    fConsoleSafe.WriteLock;
    try
      n := fConsoleCount;
      if n = length(fConsole) then
        SetLength(fConsole, NextGrow(n));
      fConsole[n].Info.Clear;
      fConsole[n].State := c.fState;
      inc(fConsoleCount);
    finally
      fConsoleSafe.WriteUnLock;
    end;
    ITunnelConsole(Obj) := console;
    fLogClass.Add.Log(sllTrace, 'TryResolve: new %', [c], self);
    result := true;
  end
  else if (aInterface = TypeInfo(ITunnelAgent)) and
          (fAgentInstance <> nil) then
  begin
    ITunnelAgent(Obj) := fAgentInstance;
    result := true;
  end;
end;

function TTunnelRelay.RemoveConsole(const aConsole: ITunnelOpenState): boolean;
var
  i, last, asagent, asconsole: integer;
  sessions: TIntegerDynArray;
begin
  result := false;
  if (self = nil) or
     (aConsole = nil) then
    exit;
  fConsoleSafe.WriteLock;
  try
    for i := 0 to fConsoleCount - 1 do
      if fConsole[i].State = aConsole then
      begin
        dec(fConsoleCount);
        last := fConsoleCount;
        if i <> last then
          fConsole[i] := fConsole[last];
        fConsole[last].State := nil;
        fConsole[last].Info.Clear;
        result := true;
        break;
      end;
  finally
    fConsoleSafe.WriteUnLock;
  end;
  asagent := 0;
  asconsole := 0;
  if result then
  begin
    asconsole := aConsole.Count;
    if (asconsole <> 0) and
       (fAgent <> nil) then
    begin
      aConsole.SnapshotSessions(sessions);
      for i := 0 to high(sessions) do
        if fAgent.fState.Delete(sessions[i]) then
          inc(asagent);
    end;
  end;
  fLogClass.Add.Log(sllTrace, 'RemoveConsole=% asagent=% asconsole=%',
    [BOOL_STR[result], asagent, asconsole], self);
end;

function TTunnelRelay.AgentsInfo: TVariantDynArray;
begin
  result := nil;
  if (self <> nil) and
     (fAgent <> nil) then
    result := fAgent.fState.GetAllInfo;
end;

function TTunnelRelay.ConsolesInfo: TVariantDynArray;
var
  i, n, count: PtrInt;
  consoles: TTunnelRelayConsoles;
  list: variant;
  dv: PDocVariantData;
begin
  result := nil;
  if self = nil then
    exit;
  fConsoleSafe.ReadLock;
  try
    n := fConsoleCount;
    if n = 0 then
      exit;
    consoles := copy(fConsole, 0, n); // strong state + metadata snapshot
  finally
    fConsoleSafe.ReadUnLock;
  end;
  SetLength(result, n);
  dv := pointer(result);
  for i := 0 to n - 1 do
  begin
    dv^.InitFast(consoles[i].Info.Count + 2, dvObject);
    dv^.AddFrom(consoles[i].Info);
    count := consoles[i].State.Count;
    dv^.AddValue('count', count);
    VarClear(list);
    if count <> 0 then
      TDocVariantData(list).InitArrayFromVariants(
        consoles[i].State.GetAllInfo, JSON_FAST);
    dv^.AddValue('list', list);
    inc(dv);
  end;
end;


{ TTunnelOpenState }

constructor TTunnelOpenState.Create(aLogClass: TSynLogClass;
  aTimeOutSecs: cardinal);
begin
  inherited Create;
  fLogClass := aLogClass;
  fTimeOutSecs := aTimeOutSecs;
  fList := TTunnelList.Create;
end;

destructor TTunnelOpenState.Destroy;
begin
  FreeAndNil(fList);
  inherited Destroy;
end;

function TTunnelOpenState.Count: integer;
begin
  result := fList.Count;
end;

function TTunnelOpenState.HasTransient(aSession: TTunnelSession): boolean;
begin
  result := fList.HasTransient(aSession);
end;

function TTunnelOpenState.AddTransient(aSession: TTunnelSession;
  const callback: ITunnelTransmit): boolean;
begin
  result := fList.AddTransient(aSession, callback);
  if fLogClass <> nil then
    fLogClass.Add.Log(sllTrace, 'AddTransient(%)=% count=%',
      [Int64(aSession), BOOL_STR[result], fList.Count], self);
end;

procedure TTunnelOpenState.PurgeTransient;
var
  gc: integer;
begin
  gc := fList.PurgeTransient(fTimeOutSecs);
  if (gc <> 0) and
     (fLogClass <> nil) then
    fLogClass.Add.Log(sllTrace, 'PurgeTransient gc=% count=%',
      [gc, fList.Count], self);
end;

function TTunnelOpenState.Commit(aSession: TTunnelSession): boolean;
begin
  result := fList.Commit(aSession);
  if fLogClass <> nil then
    fLogClass.Add.Log(sllTrace, 'Commit(%)=% count=%',
      [Int64(aSession), BOOL_STR[result], fList.Count], self);
end;

function TTunnelOpenState.Rollback(aSession: TTunnelSession): boolean;
begin
  result := fList.Rollback(aSession);
  if fLogClass <> nil then
    fLogClass.Add.Log(sllTrace, 'Rollback(%)=% count=%',
      [Int64(aSession), BOOL_STR[result], fList.Count], self);
end;

function TTunnelOpenState.Exists(aSession: TTunnelSession): boolean;
begin
  result := fList.Exists(aSession);
end;

function TTunnelOpenState.Delete(aSession: TTunnelSession): boolean;
begin
  result := fList.Delete(aSession);
end;

function TTunnelOpenState.TunnelSend(const Frame: RawByteString): boolean;
begin
  result := fList.TunnelSend(Frame);
end;

function TTunnelOpenState.GetAllInfo: TVariantDynArray;
begin
  result := fList.GetAllInfo;
end;

procedure TTunnelOpenState.SnapshotSessions(out aSessions: TIntegerDynArray);
begin
  fList.SnapshotSessions(aSessions);
end;


{ TTunnelOpen }

constructor TTunnelOpen.Create(aOwner: TTunnelRelay; aTimeOutSecs: cardinal);
begin
  inherited Create;
  fOwner := aOwner;
  if aOwner <> nil then
    fLogClass := aOwner.fLogClass;
  fTimeOutSecs := aTimeOutSecs;
  fState := TTunnelOpenState.Create(fLogClass, aTimeOutSecs);
end;

destructor TTunnelOpen.Destroy;
begin
  if (fState <> nil) and
     (fLogClass <> nil) then
    fLogClass.Add.Log(sllTrace, 'Destroy count=%', [fState.Count], self);
  fState := nil;
  fOwner := nil;
  inherited Destroy;
end;

function TTunnelOpen.Count: integer;
begin
  if fState = nil then
    result := 0
  else
    result := fState.Count;
end;

function TTunnelOpen.TunnelCommit(aSession: TTunnelSession): boolean;
begin
  result := (fState <> nil) and
            fState.Commit(aSession);
end;

function TTunnelOpen.TunnelRollback(aSession: TTunnelSession): boolean;
begin
  result := (fState <> nil) and
            fState.Rollback(aSession);
end;


{ TTunnelConsole }

destructor TTunnelConsole.Destroy;
var
  owner: TTunnelRelay;
  state: ITunnelOpenState;
begin
  state := fState;
  owner := fOwner;
  if (state <> nil) and
     (owner <> nil) then
    owner.RemoveConsole(state);
  inherited Destroy;
end;

procedure TTunnelConsole.TunnelSetInfo(const info: variant);
var
  owner: TTunnelRelay;
begin
  owner := fOwner;
  if owner <> nil then
  begin
    owner.ConsoleSetInfo(fState, info);
    owner.fLogClass.Add.Log(sllTrace, 'TunnelSetInfo %', [info], self);
  end;
end;

function TTunnelConsole.TunnelPrepare(
  const callback: ITunnelTransmit): TTunnelSession;
var
  owner: TTunnelRelay;
begin
  owner := fOwner;
  if owner = nil then
    result := 0
  else
    result := owner.PrepareNewSession(fState, callback);
end;

function TTunnelConsole.TunnelAccept(aSession: TTunnelSession;
  const callback: ITunnelTransmit): boolean;
var
  owner: TTunnelRelay;
  agent: ITunnelOpenState;
begin
  result := false;
  owner := fOwner;
  if owner = nil then
    exit;
  if owner.fAgent = nil then
    exit;
  agent := owner.fAgent.fState;
  if (agent <> nil) and
     agent.HasTransient(aSession) then
  begin
    result := fState.AddTransient(aSession, callback);
    if result then
      fState.PurgeTransient;
  end;
end;

function TTunnelConsole.TunnelInfo: variant;
var
  info: TVariantDynArray;
begin
  VarClear(result);
  if fState = nil then
    exit;
  info := fState.GetAllInfo;
  if info <> nil then
    TDocVariantData(result).InitArrayFromVariants(info, JSON_FAST);
end;

procedure TTunnelConsole.TunnelSend(const Frame: RawByteString);
var
  s: TTunnelSession;
  ok: boolean;
  owner: TTunnelRelay;
  agent: ITunnelOpenState;
begin
  owner := fOwner;
  if owner = nil then
    exit;
  if owner.fAgent = nil then
    exit;
  agent := owner.fAgent.fState;
  if agent = nil then
    exit;
  try
    agent.TunnelSend(Frame);
  finally
    if length(Frame) = TRAIL_SIZE then
    begin
      s := PTunnelSession(Frame)^;
      ok := fState.Delete(s);
      owner.fLogClass.Add.Log(sllTrace,
        'TunnelSend: Delete(%)=% after ClosePort',
        [Int64(s), BOOL_STR[ok]], self);
    end;
  end;
end;


{ TTunnelAgent }

function TTunnelAgent.TunnelAccept(aSession: TTunnelSession;
  const callback: ITunnelTransmit): boolean;
var
  owner: TTunnelRelay;
begin
  result := false;
  owner := fOwner;
  if owner = nil then
    exit;
  if owner.HasConsolePrepared(aSession) then
  begin
    result := fState.AddTransient(aSession, callback);
    if result then
      fState.PurgeTransient;
  end;
end;

function TTunnelAgent.TunnelPrepare(
  const callback: ITunnelTransmit): TTunnelSession;
var
  owner: TTunnelRelay;
begin
  owner := fOwner;
  if owner = nil then
    result := 0
  else
    result := owner.PrepareNewSession(fState, callback);
end;

function TTunnelAgent.TunnelInfo: variant;
begin
  VarClear(result);
end;

procedure TTunnelAgent.TunnelSend(const Frame: RawByteString);
var
  s: TTunnelSession;
  ok: boolean;
  owner: TTunnelRelay;
begin
  owner := fOwner;
  if owner = nil then
    exit;
  try
    owner.ConsoleTunnelSend(Frame);
  finally
    if length(Frame) = TRAIL_SIZE then
    begin
      s := PTunnelSession(Frame)^;
      ok := fState.Delete(s);
      owner.fLogClass.Add.Log(sllTrace,
        'TunnelSend: Delete(%)=% after ClosePort',
        [Int64(s), BOOL_STR[ok]], self);
    end;
  end;
end;

initialization
  TInterfaceFactory.RegisterInterfaces([
    TypeInfo(ITunnelTransmit),
    TypeInfo(ITunnelAgent),
    TypeInfo(ITunnelConsole)]);

end.

