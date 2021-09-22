/// low-level access to the OperatingSystem Sockets API (e.g. WinSock2)
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.sock;

{
  *****************************************************************************

   Cross-Platform Raw Sockets API Definition
   - Socket Process High-Level Encapsulation
   - TLS / HTTPS Encryption Abstract Layer
   - Efficient Multiple Sockets Polling
   - TUri parsing/generating URL wrapper
   - TCrtSocket Buffered Socket Read/Write Class

   The Low-Level Sockets API, which is complex and inconsistent among OS, is
   not made public and shouldn't be used in end-user code. This unit
   encapsultates all Sockets features into a single set of functions, and
   around the TNetSocket abstract wrapper.

  *****************************************************************************

  Notes:
    Oldest Delphis didn't include WinSock2.pas, so we defined our own.
    Under POSIX, will redirect to the libc or regular FPC units.

}


interface

{$I ..\mormot.defines.inc}


uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os;


{ ******************** Socket Process High-Level Encapsulation }

const
  cLocalhost = '127.0.0.1';
  cAnyHost = '0.0.0.0';
  cBroadcast = '255.255.255.255';
  c6Localhost = '::1';
  c6AnyHost = '::0';
  c6Broadcast = 'ffff::1';
  cAnyPort = '0';
  cLocalhost32 = $0100007f;

  {$ifdef OSWINDOWS}
  SOCKADDR_SIZE = 28;
  {$else}
  SOCKADDR_SIZE = 110; // able to store UNIX domain socket name
  {$endif OSWINDOWS}

var
  /// global variable containing '127.0.0.1'
  // - defined as var not as const to use reference counting from TNetAddr.IP
  IP4local: RawUtf8;

type
  /// the error codes returned by TNetSocket wrapper
  // - convenient cross-platform error handling is not possible, mostly because
  // Windows doesn't behave exactly like other targets: this enumeration
  // flattens socket execution results, and allow easy ToText() text conversion
  TNetResult = (
    nrOK,
    nrRetry,
    nrNoSocket,
    nrNotFound,
    nrNotImplemented,
    nrClosed,
    nrFatalError,
    nrUnknownError,
    nrTooManyConnections,
    nrRefused,
    nrConnectTimeout);

  /// exception class raised by this unit
  ENetSock = class(ExceptionWithProps)
  protected
    fLastError: TNetResult;
  public
    /// reintroduced constructor with TNetResult information
    constructor Create(msg: string; const args: array of const;
      error: TNetResult = nrOK); reintroduce;
    /// raise ENetSock if res is not nrOK or nrRetry
    class procedure Check(res: TNetResult; const Context: shortstring);
    /// call NetLastError and raise ENetSock if not nrOK nor nrRetry
    class procedure CheckLastError(const Context: shortstring; ForceRaise: boolean = false;
      AnotherNonFatal: integer = 0);
  published
    property LastError: TNetResult
      read fLastError default nrOk;
  end;

  /// one data state to be tracked on a given socket
  TNetEvent = (
    neRead,
    neWrite,
    neError,
    neClosed);

  /// the current whole read/write state on a given socket
  TNetEvents = set of TNetEvent;

  /// the available socket protocol layers
  // - by definition, nlUnix will return nrNotImplemented on Windows
  TNetLayer = (
    nlTcp,
    nlUdp,
    nlUnix);

  /// the available socket families - mapping AF_INET/AF_INET6/AF_UNIX
  TNetFamily = (
    nfUnknown,
    nfIP4,
    nfIP6,
    nfUnix);

  /// the IP port to connect/bind to
  TNetPort = cardinal;


const
  /// the socket protocol layers over the IP protocol
  nlIP = [nlTcp, nlUdp];

type
  /// internal mapping of an address, in any supported socket layer
  TNetAddr = object
  private
    // opaque wrapper with len: sockaddr_un=110 (POSIX) or sockaddr_in6=28 (Win)
    Addr: array[0..SOCKADDR_SIZE - 1] of byte;
  public
    function SetFrom(const address, addrport: RawUtf8; layer: TNetLayer): TNetResult;
    function Family: TNetFamily;
    function IP(localasvoid: boolean = false): RawUtf8;
    function IPShort(withport: boolean = false): shortstring; overload;
      {$ifdef HASINLINE}inline;{$endif}
    procedure IPShort(out result: shortstring; withport: boolean = false); overload;
    function Port: TNetPort;
    function SetPort(p: TNetPort): TNetResult;
    function Size: integer;
  end;

  /// pointer to a socket address mapping
  PNetAddr = ^TNetAddr;

  TNetAddrDynArray = array of TNetAddr;

type
  /// end-user code should use this TNetSocket type to hold a socket reference
  // - then methods allow cross-platform access to the connection
  TNetSocket = ^TNetSocketWrap;

  TNetSocketDynArray = array of TNetSocket;

  PTerminated = ^boolean; // on FPC system.PBoolean doesn't exist :(

  /// convenient object-oriented wrapper around a socket connection
  // - TNetSocket is a pointer to this, so TSocket(@self) is used for the OS API
  TNetSocketWrap = object
  private
    procedure SetOpt(prot, name: integer; value: pointer; valuelen: integer);
    function GetOptInt(prot, name: integer): integer;
    function SetIoMode(async: cardinal): TNetResult;
    procedure SetSendBufferSize(bytes: integer);
    procedure SetRecvBufferSize(bytes: integer);
    function GetSendBufferSize: integer;
    function GetRecvBufferSize: integer;
  public
    procedure SetupConnection(layer: TNetLayer; sendtimeout, recvtimeout: integer);
    procedure SetSendTimeout(ms: integer);
    procedure SetReceiveTimeout(ms: integer);
    procedure SetKeepAlive(keepalive: boolean);
    procedure SetLinger(linger: integer);
    procedure SetNoDelay(nodelay: boolean);
    function Accept(out clientsocket: TNetSocket; out addr: TNetAddr): TNetResult;
    function GetPeer(out addr: TNetAddr): TNetResult;
    function MakeAsync: TNetResult;
    function MakeBlocking: TNetResult;
    function Send(Buf: pointer; var len: integer): TNetResult;
    function Recv(Buf: pointer; var len: integer): TNetResult;
    function SendTo(Buf: pointer; len: integer; out addr: TNetAddr): TNetResult;
    function RecvFrom(Buf: pointer; len: integer; out addr: TNetAddr): integer;
    function WaitFor(ms: integer; scope: TNetEvents): TNetEvents;
    function RecvPending(out pending: integer): TNetResult;
    function RecvWait(ms: integer; out data: RawByteString;
      terminated: PTerminated = nil): TNetResult;
    function SendAll(Buf: PByte; len: integer;
      terminated: PTerminated = nil): TNetResult;
    function ShutdownAndClose(rdwr: boolean): TNetResult;
    function Close: TNetResult;
    function Socket: PtrInt;
      {$ifdef HASINLINE}inline;{$endif}
    property SendBufferSize: integer
      read GetSendBufferSize write SetSendBufferSize;
    property RecvBufferSize: integer
      read GetRecvBufferSize write SetRecvBufferSize;
  end;


  /// used by NewSocket() to cache the host names via NewSocketAddressCache global
  // - defined in this unit, but implemented in mormot.net.client.pas
  // - the implementation should be thread-safe
  INewSocketAddressCache = interface
    /// method called by NewSocket() to resolve its address
    function Search(const Host: RawUtf8; out NetAddr: TNetAddr): boolean;
    /// once resolved, NewSocket() will call this method to cache the TNetAddr
    procedure Add(const Host: RawUtf8; const NetAddr: TNetAddr);
    /// called by NewSocket() if connection failed, and force DNS resolution
    procedure Flush(const Host: RawUtf8);
    /// you can call this method to change the default timeout of 10 minutes
    procedure SetTimeOut(aSeconds: integer);
  end;


/// create a new Socket connected or bound to a given ip:port
function NewSocket(const address, port: RawUtf8; layer: TNetLayer;
  dobind: boolean; connecttimeout, sendtimeout, recvtimeout, retry: integer;
  out netsocket: TNetSocket; netaddr: PNetAddr = nil): TNetResult;


var
  /// contains the raw Socket API version, as returned by the Operating System
  SocketApiVersion: RawUtf8;

  /// used by NewSocket() to cache the host names
  // - avoiding DNS resolution is a always a good idea
  // - implemented by mormot.net.client unit using a TSynDictionary
  // - you may call its SetTimeOut or Flush methods to tune the caching
  NewSocketAddressCache: INewSocketAddressCache;

  /// Queue length for completely established sockets waiting to be accepted,
  // a backlog parameter for listen() function. If queue overflows client count,
  // ECONNREFUSED error is returned from connect() call
  // - for Windows default $7fffffff should not be modified. Actual limit is 200
  // - for Unix default is taken from constant (128 as in linux kernel >2.2),
  // but actual value is min(DefaultListenBacklog, /proc/sys/net/core/somaxconn)
  DefaultListenBacklog: integer;

  /// defines if a connection from the loopback should be reported as ''
  // - loopback connection will have no Remote-IP - for the default true
  // - or loopback connection will be explicitly '127.0.0.1' - if equals false
  // - used by both TCrtSock.AcceptRequest and THttpApiServer.Execute servers
  RemoteIPLocalHostAsVoidInServers: boolean = true;


/// returns the trimmed text of a network result
// - e.g. ToText(nrNotFound)='NotFound'
function ToText(res: TNetResult): PShortString; overload;



{ ******************** TLS / HTTPS Encryption Abstract Layer }

type
  /// pointer to TLS Options and Information for a given TCrtSocket connection
  PNetTlsContext = ^TNetTlsContext;

  /// callback raised by INetTls.AfterConnection to return a private key
  // password - typically prompting the user for it
  // - TLS is an opaque structure, typically an OpenSSL PSSL_CTX pointer
  TOnNetTlsGetPassword = function(Socket: TNetSocket;
    Context: PNetTlsContext; TLS: pointer): RawUtf8 of object;

  /// callback raised by INetTls.AfterConnection to validate a peer
  // - at this point, Context.CipherName is set, but PeerInfo, PeerIssuer and
  // PeerSubject are not - it is up to the event to compute the PeerInfo value
  // - TLS is an opaque structure, typically an OpenSSL PSSL pointer
  TOnNetTlsPeerValidate = procedure(Socket: TNetSocket;
    Context: PNetTlsContext; TLS: pointer) of object;

  /// callback raised by INetTls.AfterConnection after validating a peer
  // - called after standard peer validation - ignored by TOnNetTlsPeerValidate
  // - Context.CipherName, LastError PeerIssuer and PeerSubject are set
  // - TLS and Peer are opaque structures, typically OpenSSL PSSL and PX509
  TOnNetTlsAfterPeerValidate = procedure(Socket: TNetSocket;
    Context: PNetTlsContext; TLS, Peer: pointer) of object;

  /// callback raised by INetTls.AfterConnection for each peer verification
  // - wasok=true if the SSL library did validate the incoming certificate
  // - should process the supplied peer information, and return true to continue
  // and accept the connection, or false to abort the connection
  // - Context.PeerIssuer and PeerSubject have been properly populated from Peer
  // - TLS and Peer are opaque structures, typically OpenSSL PSSL and PX509
  TOnNetTlsEachPeerVerify = function(Socket: TNetSocket; Context: PNetTlsContext;
    wasok: boolean; TLS, Peer: pointer): boolean of object;

  /// TLS Options and Information for a given TCrtSocket/INetTls connection
  // - currently only properly implemented by mormot.lib.openssl11 - SChannel
  // on Windows only recognizes IgnoreCertificateErrors and sets CipherName
  // - typical usage is the following:
  // $ with THttpClientSocket.Create do
  // $ try
  // $   TLS.WithPeerInfo := true;
  // $   TLS.IgnoreCertificateErrors := true;
  // $   TLS.CipherList := 'ECDHE-RSA-AES256-GCM-SHA384';
  // $   OpenBind('synopse.info', '443', {bind=}false, {tls=}true);
  // $   writeln(TLS.PeerInfo);
  // $   writeln(TLS.CipherName);
  // $   writeln(Get('/forum/', 1000), ' len=', ContentLength);
  // $   writeln(Get('/fossil/wiki/Synopse+OpenSource', 1000));
  // $ finally
  // $   Free;
  // $ end;
  TNetTlsContext = record
    /// output: set by TCrtSocket.OpenBind() method if TLS was established
    Enabled: boolean;
    /// input: let HTTPS be less paranoid about TLS certificates
    IgnoreCertificateErrors: boolean;
    /// input: if PeerInfo field should be retrieved once connected
    WithPeerInfo: boolean;
    /// input: if TLS 1.0 or TLS 1.1 are allowed (default is TLS 1.2+ only)
    AllowDeprecatedTls: boolean;
    /// input: PEM file name containing a certificate to be loaded
    // - (Delphi) warning: encoded as UTF-8 not UnicodeString/TFileName
    // - on OpenSSL, calls the SSL_CTX_use_certificate_file() API
    CertificateFile: RawUtf8;
    /// input: PEM file name containing a private key to be loaded
    // - (Delphi) warning: encoded as UTF-8 not UnicodeString/TFileName
    PrivateKeyFile: RawUtf8;
    /// input: optional password to load the PrivateKey file
    // - see also OnPrivatePassword callback
    PrivatePassword: RawUtf8;
    /// input: file containing a specific set of CA certificates chain
    // - e.g. entrust_2048_ca.cer from https://web.entrust.com
    // - (Delphi) warning: encoded as UTF-8 not UnicodeString/TFileName
    // - on OpenSSL, calls the SSL_CTX_load_verify_locations() API
    CACertificatesFile: RawUtf8;
    /// input: preferred Cipher List
    CipherList: RawUtf8;
    /// input: a CSV list of host names to be validated
    // - e.g. 'smtp.example.com,example.com'
    HostNamesCsv: RawUtf8;
    /// output: the cipher description, as used for the current connection
    // - e.g. 'ECDHE-RSA-AES128-GCM-SHA256 TLSv1.2 Kx=ECDH Au=RSA Enc=AESGCM(128) Mac=AEAD'
    CipherName: RawUtf8;
    /// output: the connected Peer issuer name
    // - e.g. '/C=US/O=Let''s Encrypt/CN=R3'
    PeerIssuer: RawUtf8;
    /// output: the connected Peer subject name
    // - e.g. '/CN=synopse.info'
    PeerSubject: RawUtf8;
    /// output: detailed information about the connected Peer
    // - stored in the native format of the TLS library, e.g. X509_print()
    // - only populated if WithPeerInfo was set to true, or an error occurred
    PeerInfo: RawUtf8;
    /// output: low-level details about the last error at TLS level
    // - typically one X509_V_ERR_* integer constant
    LastError: RawUtf8;
    /// called by INetTls.AfterConnection to fully customize peer validation
    OnPeerValidate: TOnNetTlsPeerValidate;
    /// called by INetTls.AfterConnection for each peer validation
    // - allow e.g. to verify CN or DNSName fields of each peer certificate
    OnEachPeerVerify: TOnNetTlsEachPeerVerify;
    /// called by INetTls.AfterConnection after standard peer validation
    // - allow e.g. to verify CN or DNSName fields of the peer certificate
    OnAfterPeerValidate: TOnNetTlsAfterPeerValidate;
    /// called by INetTls.AfterConnection to retrieve a private password
    OnPrivatePassword: TOnNetTlsGetPassword;
  end;

  /// abstract definition of the TLS encrypted layer
  // - is implemented e.g. by the SChannel API on Windows, or OpenSSL on POSIX
  // if you include mormot.lib.openssl11 to your project
  INetTls = interface
    /// this method is called once to attach the underlying socket
    // - should make the proper initial TLS handshake to create a session
    // - should raise an exception on error
    procedure AfterConnection(Socket: TNetSocket; var Context: TNetTlsContext;
      const ServerAddress: RawUtf8);
    /// receive some data from the TLS layer
    function Receive(Buffer: pointer; var Length: integer): TNetResult;
    /// send some data from the TLS layer
    function Send(Buffer: pointer; var Length: integer): TNetResult;
  end;

  /// signature of a factory for a new TLS encrypted layer
  TOnNewNetTls = function: INetTls;

var
  /// global factory for a new TLS encrypted layer for TCrtSocket
  // - is set to use the SChannel API on Windows; on other targets, may be nil
  // unless the mormot.lib.openssl11.pas unit is included with your project
  NewNetTls: TOnNewNetTls;


{ ******************** Efficient Multiple Sockets Polling }

type
  /// the events monitored by TPollSocketAbstract
  // - we don't make any difference between urgent or normal read/write events
  TPollSocketEvent = (
    pseRead,
    pseWrite,
    pseError,
    pseClosed);

  /// set of events monitored by TPollSocketAbstract
  TPollSocketEvents = set of TPollSocketEvent;

  /// some opaque value (which may be a pointer) associated with a polling event
  TPollSocketTag = type PtrInt;
  TPollSocketTagDynArray = TPtrUIntDynArray;

  /// modifications notified by TPollSocketAbstract.WaitForModified
  TPollSocketResult = record
    /// opaque value as defined by TPollSocketAbstract.Subscribe
    tag: TPollSocketTag;
    /// the events which are notified
    events: TPollSocketEvents;
  end;
  PPollSocketResult = ^TPollSocketResult;

  /// all modifications returned by TPollSocketAbstract.WaitForModified
  TPollSocketResults = record
    // hold [0..Count-1] notified events
    Events: array of TPollSocketResult;
    /// how many modifications are currently monitored in Results[]
    Count: PtrInt;
  end;

  {$M+}
  /// abstract parent for TPollSocket* and TPollSockets polling
  TPollAbstract = class
  protected
    fCount: integer;
  public
    /// track status modifications on one specified TSocket
    // - you can specify which events are monitored - pseError and pseClosed
    // will always be notified
    // - tag parameter will be returned as TPollSocketResult - you may set
    // here the socket file descriptor value, or a transtyped class instance
    // - similar to epoll's EPOLL_CTL_ADD control interface
    function Subscribe(socket: TNetSocket; events: TPollSocketEvents;
      tag: TPollSocketTag): boolean; virtual; abstract;
  published
    /// how many TSocket instances are currently tracked
    property Count: integer
      read fCount;
  end;
  {$M-}

  /// abstract parent class for efficient socket polling
  // - works like Linux epoll API in level-triggered (LT) mode
  // - implements libevent-like cross-platform features
  // - use PollSocketClass global function to retrieve the best class depending
  // on the running Operating System
  // - actual classes are hidden in the implementation section of this unit,
  // and will use the fastest available API on each Operating System
  TPollSocketAbstract = class(TPollAbstract)
  protected
    fMaxSockets: integer;
  public
    /// class function factory, returning a socket polling instance matching
    // at best the current operating system
    // - return a hidden TPollSocketSelect instance under Windows,
    // TPollSocketEpoll instance under Linux, or TPollSocketPoll on BSD
    // - just a wrapper around PollSocketClass.Create
    class function New: TPollSocketAbstract;
    /// initialize the polling
    constructor Create; virtual;
    /// stop status modifications tracking on one specified TSocket
    // - the socket should have been monitored by a previous call to Subscribe()
    // - on success, returns true and fill tag with the associated opaque value
    // - similar to epoll's EPOLL_CTL_DEL control interface
    function Unsubscribe(socket: TNetSocket): boolean; virtual; abstract;
    /// waits for status modifications of all tracked TSocket
    // - will wait up to timeoutMS milliseconds, 0 meaning immediate return
    // and -1 for infinite blocking
    // - returns false on error (e.g. no TSocket registered) or no event
    // - returns true and results.Events[0..results.Count-1] notifications
    function WaitForModified(var results: TPollSocketResults;
      timeoutMS: integer): boolean; virtual; abstract;
  published
    /// how many TSocket instances could be tracked, at most
    // - depends on the API used
    property MaxSockets: integer
      read fMaxSockets;
  end;

  /// meta-class of TPollSocketAbstract socket polling classes
  // - since TPollSocketAbstract.Create is declared as virtual, could be used
  // to specify the proper polling class to add
  // - see PollSocketClass function and TPollSocketAbstract.New method
  TPollSocketClass = class of TPollSocketAbstract;

  /// TPollSockets.OnGetOneIdle callback prototype
  TOnPollSocketsIdle = procedure(Sender: TObject; NowTix: Int64) of object;

  TPollSocketsSubscribe = record
    socket: TNetSocket;
    tag: TPollSocketTag;
    events: TPollSocketEvents;
  end;

  // used internally by TPollSockets.Subscribe/Unsubscribe for thread safety
  TPollSocketsSubscription = record
    Unsubscribed: TNetSocketDynArray;
    UnsubscribedCount: integer;
    Subscribe: array of TPollSocketsSubscribe;
    SubscribeCount: PtrInt;
  end;

  /// implements efficient polling of multiple sockets
  // - will maintain a pool of TPollSocketAbstract instances, to monitor
  // incoming data or outgoing availability for a set of active connections
  // - call Subscribe/Unsubscribe to setup the monitored sockets
  // - call GetOne from any consumming threads to process new events
  TPollSockets = class(TPollAbstract)
  protected
    fPoll: array of TPollSocketAbstract; // each track up to fPoll[].MaxSockets
    fPollIndex: integer;
    fPending: TPollSocketResults;
    fPendingIndex: PtrInt;
    fGettingOne: integer;
    fTerminated: boolean;
    fPollClass: TPollSocketClass;
    fPollLock: TRTLCriticalSection;
    fPendingLock: TRTLCriticalSection;
    fOnLog: TSynLogProc;
    fOnGetOneIdle: TOnPollSocketsIdle;
    fLastUnsubscribedTag: TPollSocketTagDynArray;
    fLastUnsubscribedTagCount: integer;
    fSubscription: TPollSocketsSubscription;
    fUnsubscribeShouldShutdownSocket: boolean;
    procedure NoMorePending; {$ifdef HASINLINE} inline; {$endif}
    function IsValidPending(tag: TPollSocketTag): boolean; virtual;
  public
    /// initialize the sockets polling
    // - under Linux/POSIX, will set the open files maximum number for the
    // current process to match the system hard limit: if your system has a
    // low "ulimit -H -n" value, you may add the following line in your
    // /etc/limits.conf or /etc/security/limits.conf file:
    // $ * hard nofile 65535
    constructor Create(aPollClass: TPollSocketClass = nil);
    /// finalize the sockets polling, and release all used memory
    destructor Destroy; override;
    /// track modifications on one specified TSocket and tag
    // - the supplied tag value - maybe a PtrInt(aObject) - will be part of
    // GetOne method results
    // - will create as many TPollSocketAbstract instances as needed, depending
    // on the MaxSockets capability of the actual implementation class
    // - this method is thread-safe, and the actual fPoll[].Subscribe
    // will take place during the next PollForPendingEvents() call
    function Subscribe(socket: TNetSocket; events: TPollSocketEvents;
      tag: TPollSocketTag): boolean; override;
    /// stop status modifications tracking on one specified TSocket and tag
    // - the socket should have been monitored by a previous call to Subscribe()
    // - this method is thread-safe, and the actual fPoll[].UnSubscribe
    // will take place during the next PollForPendingEvents() call
    procedure Unsubscribe(socket: TNetSocket; tag: TPollSocketTag); virtual;
    /// retrieve the next pending notification, or let the poll wait for new
    // - if GetOnePending returns no pending notification, will try
    // PollForPendingEvents and wait up to timeoutMS milliseconds for events
    // - returns true and set notif.events/tag with the corresponding notification
    // - returns false if no pending event was handled within the timeoutMS period
    // - this method is thread-safe, and could be called from several threads
    function GetOne(timeoutMS: integer; const call: RawUtf8;
      out notif: TPollSocketResult): boolean; virtual;
    /// retrieve the next pending notification
    // - returns true and set notif.events/tag with the corresponding notification
    // - returns false if no pending event is available
    // - this method is thread-safe, and could be called from several threads
    function GetOnePending(out notif: TPollSocketResult; const call: RawUtf8): boolean;
    /// let the poll check for pending events and apend them to fPending results
    // - could be called when PendingCount=0, i.e. GetOnePending()=false
    // - returns how many new events have been retrieved for the subscribed sockets
    function PollForPendingEvents(timeoutMS: integer): integer; virtual;
    /// manually append one event to the pending nodifications
    // - ready to be retrieved by GetOnePending
    procedure AddOnePending(aTag: TPollSocketTag; aEvents: TPollSocketEvents);
    /// notify any GetOne waiting method to stop its polling loop
    procedure Terminate; virtual;
    /// indicates that Unsubscribe() should also call ShutdownAndClose(socket)
    // - Destroy will also shutdown any remaining sockets if PollForPendingEvents
    // has not been called before shutdown
    property UnsubscribeShouldShutdownSocket: boolean
      read fUnsubscribeShouldShutdownSocket write fUnsubscribeShouldShutdownSocket;
    /// the actual polling class used to track socket state changes
    property PollClass: TPollSocketClass
      read fPollClass write fPollClass;
    /// allow raw debugging via logs of the low-level process
    property OnLog: TSynLogProc
      read fOnLog write fOnLog;
    /// callback called by GetOne when Idle
    // - warning: any implementation should be very quick and non blocking
    property OnGetOneIdle: TOnPollSocketsIdle
      read fOnGetOneIdle write fOnGetOneIdle;
  published
    /// is set to true by the Terminate method
    property Terminated: boolean
      read fTerminated;
    /// the index of the last notified event in the internal queue
    property PendingIndex: PtrInt
      read fPendingIndex;
    /// how many notified events are currently in the internal queue
    property PendingCount: PtrInt
      read fPending.Count;
  end;


/// the TPollSocketAbstract class best fitting with the current Operating System
// - as used by TPollSocketAbstract.New method
// - returns e.g. TPollSocketEpoll on Linux, or TPollSocketSelect on Windows
function PollSocketClass: TPollSocketClass;


{ *************************** TUri parsing/generating URL wrapper }

type
  /// structure used to parse an URI into its components
  // - ready to be supplied e.g. to a THttpRequest sub-class
  // - used e.g. by class function THttpRequest.Get()
  // - will decode standard HTTP/HTTPS urls or Unix sockets URI like
  // 'http://unix:/path/to/socket.sock:/url/path'
  {$ifdef USERECORDWITHMETHODS}
  TUri = record
  {$else}
  TUri = object
  {$endif USERECORDWITHMETHODS}
  public
    /// if the server is accessible via https:// and not plain http://
    Https: boolean;
    /// either nlTcp for HTTP/HTTPS or nlUnix for Unix socket URI
    Layer: TNetLayer;
    /// if the server is accessible via something else than http:// or https://
    // - e.g. 'ws' or 'wss' for ws:// or wss://
    Scheme: RawUtf8;
    /// the server name
    // - e.g. 'www.somewebsite.com' or 'path/to/socket.sock' Unix socket URI
    Server: RawUtf8;
    /// the server port
    // - e.g. '80'
    Port: RawUtf8;
    /// optional user for authentication, as retrieved before '@'
    // - e.g. from 'https://user:password@server:port/address'
    User: RawUtf8;
    /// optional password for authentication, as retrieved before '@'
    // - e.g. from 'https://user:password@server:port/address'
    Password: RawUtf8;
    /// the resource address, including optional parameters
    // - e.g. '/category/name/10?param=1'
    Address: RawUtf8;
    /// reset all stored information
    procedure Clear;
    /// fill the members from a supplied URI
    // - recognize e.g. 'http://Server:Port/Address', 'https://Server/Address',
    // 'Server/Address' (as http), or 'http://unix:/Server:/Address' (as nlUnix)
    // - recognize 'https://user:password@server:port/address' authentication
    // - returns TRUE is at least the Server has been extracted, FALSE on error
    function From(aUri: RawUtf8; const DefaultPort: RawUtf8 = ''): boolean;
    /// compute the whole normalized URI
    // - e.g. 'https://Server:Port/Address' or 'http://unix:/Server:/Address'
    function URI: RawUtf8;
    /// the server port, as integer value
    function PortInt: TNetPort;
    /// compute the root resource Address, without any URI-encoded parameter
    // - e.g. '/category/name/10'
    function Root: RawUtf8;
    /// returns BinToBase64(User + ':' + Password) encoded value
    function UserPasswordBase64: RawUtf8;
  end;
  PUri = ^TUri;


const
  /// the default TCP port as text, as DEFAULT_PORT[Https]
  DEFAULT_PORT: array[boolean] of RawUtf8 = (
    '80', '443');
  /// the default TCP port as integer, as DEFAULT_PORT_INT[Https]
  DEFAULT_PORT_INT: array[boolean] of TNetPort = (
    80, 443);


{ ********* TCrtSocket Buffered Socket Read/Write Class }

type
  /// meta-class of a TCrtSocket (sub-)type
  TCrtSocketClass = class of TCrtSocket;

  /// identify the incoming data availability in TCrtSocket.SockReceivePending
  TCrtSocketPending = (
    cspSocketError,
    cspNoData,
    cspDataAvailable);

  {$M+}
  /// Fast low-level Socket implementation
  // - direct access to the OS (Windows, Linux) network layer API
  // - use Open constructor to create a client to be connected to a server
  // - use Bind constructor to initialize a server
  // - use SockIn and SockOut (after CreateSock*) to read/readln or write/writeln
  //  as with standard Delphi text files (see SendEmail implementation)
  // - even if you do not use read(SockIn^), you may call CreateSockIn then
  // read the (binary) content via SockInRead/SockInPending methods, which would
  // benefit of the SockIn^ input buffer to maximize reading speed
  // - to write data, CreateSockOut and write(SockOut^) is not mandatory: you
  // rather may use SockSend() overloaded methods, followed by a SockFlush call
  // - in fact, you can decide whatever to use none, one or both SockIn/SockOut
  // - since this class rely on its internal optimized buffering system,
  // TCP_NODELAY is set to disable the Nagle algorithm
  // - our classes are (much) faster than the Indy or Synapse implementation
  TCrtSocket = class
  protected
    fSock: TNetSocket;
    fServer: RawUtf8;
    fPort: RawUtf8;
    fProxyUrl: RawUtf8;
    // set by AcceptRequest() from TVarSin
    fRemoteIP: RawUtf8;
    fSockIn: PTextFile;
    fSockOut: PTextFile;
    fTimeOut: PtrInt;
    fBytesIn: Int64;
    fBytesOut: Int64;
    fSockInEofError: integer;
    fSocketLayer: TNetLayer;
    fWasBind: boolean;
    // updated by every SockSend() call
    fSndBuf: RawByteString;
    fSndBufLen: integer;
    // updated during UDP connection, accessed via PeerAddress/PeerPort
    fPeerAddr: PNetAddr;
    fSecure: INetTls;
    procedure SetKeepAlive(aKeepAlive: boolean); virtual;
    procedure SetLinger(aLinger: integer); virtual;
    procedure SetReceiveTimeout(aReceiveTimeout: integer); virtual;
    procedure SetSendTimeout(aSendTimeout: integer); virtual;
    procedure SetTcpNoDelay(aTcpNoDelay: boolean); virtual;
    function GetRawSocket: PtrInt;
  public
    /// direct access to the optional low-level HTTP proxy tunnelling information
    // - could have been assigned by a Tunnel.From() call
    // - User/Password would be taken into consideration for authentication
    Tunnel: TUri;
    /// direct access to the optional low-level TLS Options and Information
    // - depending on the actual INetTls implementation, some fields may not
    // be used nor populated - currently only supported by mormot.lib.openssl11
    TLS: TNetTlsContext;
    /// can be assigned from TSynLog.DoLog class method for low-level logging
    OnLog: TSynLogProc;
    /// common initialization of all constructors
    // - do not call directly, but use Open / Bind constructors instead
    constructor Create(aTimeOut: PtrInt = 10000); reintroduce; virtual;
    /// constructor to connect to aServer:aPort
    // - optionaly via TLS (using the SChannel API on Windows, or by including
    // mormot.lib.openssl11 unit to your project) - with custom input options
    // - aTunnel could be populated from mormot.net.client GetSystemProxyUri()
    // - see also SocketOpen() for a wrapper catching any connection exception
    constructor Open(const aServer, aPort: RawUtf8; aLayer: TNetLayer = nlTcp;
      aTimeOut: cardinal = 10000; aTLS: boolean = false;
      aTLSContext: PNetTlsContext = nil; aTunnel: PUri = nil);
    /// high-level constructor to connect to a given URI
    constructor OpenUri(const aUri: RawUtf8; out aAddress: RawUtf8;
      const aTunnel: RawUtf8 = ''; aTimeOut: cardinal = 10000;
      aTLSContext: PNetTlsContext = nil); overload;
    /// constructor to bind to an address
    // - aAddr='1234' - bind to a port on all interfaces, the same as '0.0.0.0:1234'
    // - aAddr='IP:port' - bind to specified interface only, e.g.
    // '1.2.3.4:1234'
    // - aAddr='unix:/path/to/file' - bind to unix domain socket, e.g.
    // 'unix:/run/mormot.sock'
    // - aAddr='' - bind to systemd descriptor on linux - see
    // http://0pointer.de/blog/projects/socket-activation.html
    constructor Bind(const aAddress: RawUtf8; aLayer: TNetLayer = nlTcp;
      aTimeOut: integer = 10000);
    /// low-level internal method called by Open() and Bind() constructors
    // - raise an ENetSock exception on error
    // - optionaly via TLS (using the SChannel API on Windows, or by including
    // mormot.lib.openssl11 unit) - with custom input options in the TLS fields
    procedure OpenBind(const aServer, aPort: RawUtf8; doBind: boolean;
      aTLS: boolean = false; aLayer: TNetLayer = nlTcp;
      aSock: TNetSocket = TNetSocket(-1));
    /// initialize the instance with the supplied accepted socket
    // - is called from a bound TCP Server, just after Accept()
    procedure AcceptRequest(aClientSock: TNetSocket; aClientAddr: PNetAddr);
    /// initialize SockIn for receiving with read[ln](SockIn^,...)
    // - data is buffered, filled as the data is available
    // - read(char) or readln() is indeed very fast
    // - multithread applications would also use this SockIn pseudo-text file
    // - default 1KB is big enough for headers (content will be read directly)
    // - by default, expect CR+LF as line feed (i.e. the HTTP way)
    procedure CreateSockIn(LineBreak: TTextLineBreakStyle = tlbsCRLF;
      InputBufferSize: integer = 1024);
    /// initialize SockOut for sending with write[ln](SockOut^,....)
    // - data is sent (flushed) after each writeln() - it's a compiler feature
    // - use rather SockSend() + SockSendFlush to send headers at once e.g.
    // since writeln(SockOut^,..) flush buffer each time
    procedure CreateSockOut(OutputBufferSize: integer = 1024);
    /// finalize SockIn receiving buffer
    // - you may call this method when you are sure that you don't need the
    // input buffering feature on this connection any more (e.g. after having
    // parsed the HTTP header, then rely on direct socket comunication)
    procedure CloseSockIn;
    /// finalize SockOut receiving buffer
    // - you may call this method when you are sure that you don't need the
    // output buffering feature on this connection any more (e.g. after having
    // parsed the HTTP header, then rely on direct socket comunication)
    procedure CloseSockOut;
    /// close and shutdown the connection
    // - called from Destroy, but is reintrant so could be called earlier
    procedure Close; virtual;
    /// close the opened socket, and corresponding SockIn/SockOut
    destructor Destroy; override;
    /// read Length bytes from SockIn buffer + Sock if necessary
    // - if SockIn is available, it first gets data from SockIn^.Buffer,
    // then directly receive data from socket if UseOnlySockIn = false
    // - if UseOnlySockIn = true, it will return the data available in SockIn^,
    // and returns the number of bytes
    // - can be used also without SockIn: it will call directly SockRecv()
    // in such case (assuming UseOnlySockin=false)
    function SockInRead(Content: PAnsiChar; Length: integer;
      UseOnlySockIn: boolean = false): integer;
    /// returns the number of bytes in SockIn buffer or pending in Sock
    // - if SockIn is available, it first check from any data in SockIn^.Buffer,
    // then call InputSock to try to receive any pending data if the buffer is void
    // - if aPendingAlsoInSocket is TRUE, returns the bytes available in both the buffer
    // and the socket (sometimes needed, e.g. to process a whole block at once)
    // - will wait up to the specified aTimeOutMS value (in milliseconds) for
    // incoming data - may wait a little less time on Windows due to a select bug
    // - returns -1 in case of a socket error (e.g. broken/closed connection);
    // you can raise a ENetSock exception to propagate the error
    function SockInPending(aTimeOutMS: integer;
      aPendingAlsoInSocket: boolean = false): integer;
    /// checks if the low-level socket handle has been assigned
    // - just a wrapper around PtrInt(fSock)>0
    function SockIsDefined: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// check the connection status of the socket
    function SockConnected: boolean;
    /// simulate writeln() with direct use of Send(Sock, ..) - includes trailing #13#10
    // - useful on multi-treaded environnement (as in THttpServer.Process)
    // - no temp buffer is used
    // - handle RawByteString, ShortString, Char, integer parameters
    // - raise ENetSock exception on socket error
    procedure SockSend(const Values: array of const); overload;
    /// simulate writeln() with a single line - includes trailing #13#10
    procedure SockSend(const Line: RawByteString); overload;
    /// append P^ data into SndBuf (used by SockSend(), e.g.) - no trailing #13#10
    // - call SockSendFlush to send it through the network via SndLow()
    procedure SockSend(P: pointer; Len: integer); overload;
    /// append #13#10 characters
    procedure SockSendCRLF;
    /// flush all pending data to be sent, optionally with some body content
    // - raise ENetSock on error
    procedure SockSendFlush(const aBody: RawByteString = '');
    /// send all TStream content till the end using SndLow()
    // - don't forget to call SockSendFlush before using this method
    // - will call Stream.Read() over a temporary buffer of 1MB by default
    // - Stream may be a TFileStream, THttpMultiPartStream or TNestedStreamReader
    // - raise ENetSock on error
    procedure SockSendStream(Stream: TStream; ChunkSize: integer = 1 shl 20);
    /// how many bytes could be added by SockSend() in the internal buffer
    function SockSendRemainingSize: integer;
      {$ifdef HASINLINE}inline;{$endif}
    /// fill the Buffer with Length bytes
    // - use TimeOut milliseconds wait for incoming data
    // - bypass the SockIn^ buffers
    // - raise ENetSock exception on socket error
    procedure SockRecv(Buffer: pointer; Length: integer);
    /// check if there are some pending bytes in the input sockets API buffer
    // - returns cspSocketError if the connection is broken or closed
    // - warning: on Windows, may wait a little less than TimeOutMS (select bug)
    function SockReceivePending(TimeOutMS: integer): TCrtSocketPending;
    /// returns the socket input stream as a string
    function SockReceiveString: RawByteString;
    /// fill the Buffer with Length bytes
    // - use TimeOut milliseconds wait for incoming data
    // - bypass the SockIn^ buffers
    // - return false on any fatal socket error, true on success
    // - call Close if the socket is identified as shutdown from the other side
    // - you may optionally set StopBeforeLength = true, then the read bytes count
    // are set in Length, even if not all expected data has been received - in
    // this case, Close method won't be called
    function TrySockRecv(Buffer: pointer; var Length: integer;
      StopBeforeLength: boolean = false): boolean;
    /// call readln(SockIn^,Line) or simulate it with direct use of Recv(Sock, ..)
    // - char are read one by one if needed
    // - use TimeOut milliseconds wait for incoming data
    // - raise ENetSock exception on socket error
    // - by default, will handle #10 or #13#10 as line delimiter (as normal text
    // files), but you can delimit lines using #13 if CROnly is TRUE
    procedure SockRecvLn(out Line: RawUtf8; CROnly: boolean = false); overload;
    /// call readln(SockIn^) or simulate it with direct use of Recv(Sock, ..)
    // - char are read one by one
    // - use TimeOut milliseconds wait for incoming data
    // - raise ENetSock exception on socket error
    // - line content is ignored
    procedure SockRecvLn; overload;
    /// direct send data through network
    // - raise a ENetSock exception on any error
    // - bypass the SockSend() or SockOut^ buffers
    procedure SndLow(P: pointer; Len: integer); overload;
    /// direct send data through network
    // - raise a ENetSock exception on any error
    // - bypass the SndBuf or SockOut^ buffers
    // - raw Data is sent directly to OS: no LF/CRLF is appened to the block
    procedure SndLow(const Data: RawByteString); overload;
    /// direct send data through network
    // - return false on any error, true on success
    // - bypass the SndBuf or SockOut^ buffers
    function TrySndLow(P: pointer; Len: integer): boolean;
    /// returns the low-level error number
    // - i.e. returns WSAGetLastError
    function LastLowSocketError: integer;
    /// direct accept an new incoming connection on a bound socket
    // - instance should have been setup as a server via a previous Bind() call
    // - returns nil on error or a ResultClass instance on success
    // - if ResultClass is nil, will return a plain TCrtSocket, but you may
    // specify e.g. THttpServerSocket if you expect incoming HTTP requests
    function AcceptIncoming(ResultClass: TCrtSocketClass = nil): TCrtSocket;
    /// remote IP address after AcceptRequest() call over TCP
    // - is either the raw connection IP to the current server socket, or
    // a custom header value set by a local proxy as retrieved by inherited
    // THttpServerSocket.GetRequest, searching the header named in
    // THttpServerGeneric.RemoteIPHeader (e.g. 'X-Real-IP' for nginx)
    property RemoteIP: RawUtf8
      read fRemoteIP write fRemoteIP;
    /// remote IP address of the last packet received (SocketLayer=slUDP only)
    function PeerAddress(LocalAsVoid: boolean = false): RawByteString;
    /// remote IP port of the last packet received (SocketLayer=slUDP only)
    function PeerPort: TNetPort;
    /// set the TCP_NODELAY option for the connection
    // - default true will disable the Nagle buffering algorithm; it should
    // only be set for applications that send frequent small bursts of information
    // without getting an immediate response, where timely delivery of data
    // is required - so it expects buffering before calling Write() or SndLow()
    // - you can set false here to enable the Nagle algorithm, if needed
    // - see http://www.unixguide.net/network/socketfaq/2.16.shtml
    property TcpNoDelay: boolean
      write SetTcpNoDelay;
    /// set the SO_SNDTIMEO option for the connection
    // - i.e. the timeout, in milliseconds, for blocking send calls
    // - see http://msdn.microsoft.com/en-us/library/windows/desktop/ms740476
    property SendTimeout: integer
      write SetSendTimeout;
    /// set the SO_RCVTIMEO option for the connection
    // - i.e. the timeout, in milliseconds, for blocking receive calls
    // - see http://msdn.microsoft.com/en-us/library/windows/desktop/ms740476
    property ReceiveTimeout: integer
      write SetReceiveTimeout;
    /// set the SO_KEEPALIVE option for the connection
    // - 1 (true) will enable keep-alive packets for the connection
    // - see http://msdn.microsoft.com/en-us/library/windows/desktop/ee470551
    property KeepAlive: boolean
      write SetKeepAlive;
    /// set the SO_LINGER option for the connection, to control its shutdown
    // - by default (or Linger<0), Close will return immediately to the caller,
    // and any pending data will be delivered if possible
    // - Linger > 0  represents the time in seconds for the timeout period
    // to be applied at Close; under Linux, will also set SO_REUSEADDR; under
    // Darwin, set SO_NOSIGPIPE
    // - Linger = 0 causes the connection to be aborted and any pending data
    // is immediately discarded at Close
    property Linger: integer
      write SetLinger;
    /// low-level socket handle, initialized after Open() with socket
    property Sock: TNetSocket
      read fSock write fSock;
    /// after CreateSockIn, use Readln(SockIn^,s) to read a line from the opened socket
    property SockIn: PTextFile
      read fSockIn;
    /// after CreateSockOut, use Writeln(SockOut^,s) to send a line to the opened socket
    property SockOut: PTextFile
      read fSockOut;
  published
    /// low-level socket type, initialized after Open() with socket
    property SocketLayer: TNetLayer
      read fSocketLayer;
    /// IP address, initialized after Open() with Server name
    property Server: RawUtf8
      read fServer;
    /// IP port, initialized after Open() with port number
    property Port: RawUtf8
      read fPort;
    /// contains Sock, but transtyped as number for log display
    property RawSocket: PtrInt
      read GetRawSocket;
    /// HTTP Proxy URI used for tunnelling, from Tunnel.Server/Port values
    property ProxyUrl: RawUtf8
      read fProxyUrl;
    /// if higher than 0, read loop will wait for incoming data till
    // TimeOut milliseconds (default value is 10000) - used also in SockSend()
    property TimeOut: PtrInt
      read fTimeOut;
    /// total bytes received
    property BytesIn: Int64
      read fBytesIn write fBytesIn;
    /// total bytes sent
    property BytesOut: Int64
      read fBytesOut write fBytesOut;
  end;
  {$M-}


/// create a TCrtSocket instance, returning nil on error
// - useful to easily catch any exception, and provide a custom TNetTlsContext
// - aTunnel could be populated from mormot.net.client GetSystemProxyUri()
function SocketOpen(const aServer, aPort: RawUtf8;
  aTLS: boolean = false; aTLSContext: PNetTlsContext = nil;
  aTunnel: PUri = nil): TCrtSocket;



implementation

{ ******** System-Specific Raw Sockets API Layer }

{ includes are below inserted just after 'implementation' keyword to allow
  their own private 'uses' clause }

{$ifdef OSWINDOWS}
  {$I mormot.net.sock.windows.inc}
{$endif OSWINDOWS}

{$ifdef OSPOSIX}
  {$I mormot.net.sock.posix.inc}
{$endif OSPOSIX}

const
  // don't use RTTI to avoid mormot.core.rtti.pas and have better spelling
  _NR: array[TNetResult] of string[20] = (
    'Ok',
    'Retry',
    'No Socket',
    'Not Found',
    'Not Implemented',
    'Closed',
    'Fatal Error',
    'Unknown Error',
    'Too Many Connections',
    'Refused',
    'Connect Timeout');

function NetLastError(AnotherNonFatal: integer = NO_ERROR;
  Error: PInteger = nil): TNetResult;
var
  err: integer;
begin
  err := sockerrno;
  if Error <> nil then
    Error^ := err;
  if err = NO_ERROR then
    result := nrOK
  else if {$ifdef OSWINDOWS}
          (err <> WSAETIMEDOUT) and
          (err <> WSAEWOULDBLOCK) and
          {$endif OSWINDOWS}
          (err <> WSATRY_AGAIN) and
          (err <> AnotherNonFatal) then
    if err = WSAEMFILE then
      result := nrTooManyConnections
    else if err = WSAECONNREFUSED then
      result := nrRefused
    {$ifdef OSLINUX}
    else if err = ESysEPIPE then
      result := nrClosed
    {$endif OSLINUX}
    else
      result := nrFatalError
  else
    result := nrRetry;
end;

function NetLastErrorMsg(AnotherNonFatal: integer = NO_ERROR): shortstring;
var
  nr: TNetResult;
  err: integer;
begin
  nr := NetLastError(AnotherNonFatal, @err);
  str(err, result);
  result := _NR[nr] + ' ' + result;
end;

function NetCheck(res: integer): TNetResult;
  {$ifdef HASINLINE}inline;{$endif}
begin
  if res = NO_ERROR then
    result := nrOK
  else
    result := NetLastError;
end;

procedure IP4Short(ip4addr: PByteArray; var s: shortstring);
begin
  str(ip4addr[0], s);
  inc(s[0]);
  s[ord(s[0])] := '.';
  AppendShortInteger(ip4addr[1], s);
  inc(s[0]);
  s[ord(s[0])] := '.';
  AppendShortInteger(ip4addr[2], s);
  inc(s[0]);
  s[ord(s[0])] := '.';
  AppendShortInteger(ip4addr[3], s);
end;

procedure IP4Text(ip4addr: PByteArray; var result: RawUtf8);
var
  s: shortstring;
begin
  if PCardinal(ip4addr)^ = 0 then
    // '0.0.0.0' bound to any host -> ''
    result := ''
  else if PCardinal(ip4addr)^ = cLocalhost32 then
    // '127.0.0.1' loopback -> no memory allocation
    result := IP4local
  else
  begin
    IP4Short(ip4addr, s);
    FastSetString(result, @s[1], ord(s[0]));
  end;
end;

function ToText(res: TNetResult): PShortString;
begin
  result := @_NR[res]; // no mormot.core.rtti.pas need
end;


{ ENetSock }

constructor ENetSock.Create(msg: string; const args: array of const;
  error: TNetResult);
begin
  fLastError := error;
  if error <> nrOK then
    msg := format('%s [%s - #%d]', [msg, _NR[error], ord(error)]);
  inherited CreateFmt(msg, args);
end;

class procedure ENetSock.Check(res: TNetResult; const Context: shortstring);
begin
  if (res <> nrOK) and
     (res <> nrRetry) then
    raise Create('%s failed', [Context], res);
end;

class procedure ENetSock.CheckLastError(const Context: shortstring;
  ForceRaise: boolean; AnotherNonFatal: integer);
var
  res: TNetResult;
begin
  res := NetLastError(AnotherNonFatal);
  if ForceRaise and
     (res in [nrOK, nrRetry]) then
    res := nrUnknownError;
  Check(res, Context);
end;



{ ******** TNetAddr Cross-Platform Wrapper }

{ TNetAddr }

function TNetAddr.Family: TNetFamily;
begin
  case PSockAddr(@Addr)^.sa_family of
    AF_INET:
      result := nfIP4;
    AF_INET6:
      result := nfIP6;
    {$ifdef OSPOSIX}
    AF_UNIX:
      result := nfUnix;
    {$endif OSPOSIX}
    else
      result := nfUnknown;
  end;
end;

function TNetAddr.IP(localasvoid: boolean): RawUtf8;
var
  tmp: ShortString;
begin
  result := '';
  with PSockAddr(@Addr)^ do
    case sa_family of
      AF_INET:
        // check most common used values
        if cardinal(sin_addr) = 0 then
          // '0.0.0.0' bound to any host -> ''
          exit
        else if cardinal(sin_addr) = cLocalhost32 then
        begin
          // '127.0.0.1' loopback -> no memory allocation
          if not localasvoid then
            result := IP4local;
          exit;
        end;
      {$ifdef OSPOSIX}
      AF_UNIX:
        begin
          if not localasvoid then
            result := IP4local;
          exit;
        end;
      {$endif OSPOSIX}
      else
        exit;
    end;
  IPShort(tmp, {withport=}false);
  if not localasvoid or
     (tmp <> c6Localhost) then
    FastSetString(result, @tmp[1], ord(tmp[0]));
end;

function TNetAddr.IPShort(withport: boolean): shortstring;
begin
  IPShort(result, withport);
end;

procedure TNetAddr.IPShort(out result: shortstring; withport: boolean);
var
  host: array[0..NI_MAXHOST] of AnsiChar;
  serv: array[0..NI_MAXSERV] of AnsiChar;
  hostlen, servlen: integer;
begin
  result[0] := #0;
  case PSockAddr(@Addr)^.sa_family of
    AF_INET:
      begin
        IP4Short(@PSockAddr(@Addr)^.sin_addr, result);
        if withport then
        begin
          AppendShortChar(':', result);
          AppendShortInteger(port, result);
        end;
      end;
    AF_INET6:
      begin
        hostlen := NI_MAXHOST;
        servlen := NI_MAXSERV;
        if getnameinfo(@Addr, SizeOf(sockaddr_in6), host{%H-}, hostlen,
             serv{%H-}, servlen, NI_NUMERICHOST + NI_NUMERICSERV) = NO_ERROR then
        begin
          SetString(result, PAnsiChar(@host), mormot.core.base.StrLen(@host));
          if withport then
          begin
            AppendShortChar(':', result);
            AppendShortBuffer(PAnsiChar(@serv), -1, result);
          end;
        end;
      end;
    {$ifdef OSPOSIX}
    AF_UNIX:
      SetString(result, PAnsiChar(@psockaddr_un(@Addr)^.sun_path),
        mormot.core.base.StrLen(@psockaddr_un(@Addr)^.sun_path));
    {$endif OSPOSIX}
  end;
end;

function TNetAddr.Port: TNetPort;
begin
  with PSockAddr(@Addr)^ do
    if sa_family in [AF_INET, AF_INET6] then
      result := htons(sin_port)
    else
      result := 0;
end;

function TNetAddr.SetPort(p: TNetPort): TNetResult;
begin
  with PSockAddr(@Addr)^ do
    if (sa_family in [AF_INET, AF_INET6]) and
       (p <= 65535) then
    begin
      sin_port := htons(p);
      result := nrOk;
    end
    else
      result := nrNotFound;
end;

function TNetAddr.Size: integer;
begin
  case PSockAddr(@Addr)^.sa_family of
    AF_INET:
      result := SizeOf(sockaddr_in);
    AF_INET6:
      result := SizeOf(sockaddr_in6);
  else
    result := SizeOf(Addr);
  end;
end;


{ ******** TNetSocket Cross-Platform Wrapper }

function NewSocket(const address, port: RawUtf8; layer: TNetLayer;
  dobind: boolean; connecttimeout, sendtimeout, recvtimeout, retry: integer;
  out netsocket: TNetSocket; netaddr: PNetAddr): TNetResult;
var
  addr: TNetAddr;
  sock: TSocket;
  fromcache, tobecached: boolean;
  connectendtix: Int64;
  p: cardinal;
begin
  netsocket := nil;
  fromcache := false;
  tobecached := false;
  // resolve the TNetAddr of the address:port layer - maybe from cache
  if (layer in nlIP) and
     (not dobind) and
     Assigned(NewSocketAddressCache) and
     ToCardinal(port, p, 1) then
    if (address = '') or
       (address = cLocalhost) or
       (address = cAnyHost) then // for client: '0.0.0.0'->'127.0.0.1'
      result := addr.SetFrom(cLocalhost, port, layer)
    else if NewSocketAddressCache.Search(address, addr) then
    begin
      fromcache := true;
      result := addr.SetPort(p);
    end
    else
    begin
      tobecached := true;
      result := addr.SetFrom(address, port, layer);
    end
  else
    result := addr.SetFrom(address, port, layer);
  if result <> nrOK then
    exit;
  // create the raw Socket instance
  sock := socket(PSockAddr(@addr)^.sa_family, _ST[layer], _IP[layer]);
  if sock = -1 then
  begin
    result := NetLastError(WSAEADDRNOTAVAIL);
    if fromcache then
      // force call the DNS resolver again, perhaps load-balacing is needed
      NewSocketAddressCache.Flush(address);
    exit;
  end;
  // bind or connect to this Socket
  {$ifdef OSWINDOWS}
  if not dobind then
  begin // on Windows, default buffers are of 8KB :(
    TNetSocket(sock).SetRecvBufferSize(65536);
    TNetSocket(sock).SetSendBufferSize(65536);
  end; // to be done before the actual connect() for proper TCP negotiation
  {$endif OSWINDOWS}
  // open non-blocking Client connection if a timeout was specified
  if (connecttimeout > 0) and
     not dobind then
  begin
    // SetReceiveTimeout/SetSendTimeout don't apply to connect() -> async
    if connecttimeout < 100 then
      connectendtix := 0
    else
      connectendtix := mormot.core.os.GetTickCount64 + connecttimeout;
    TNetSocket(sock).MakeAsync;
    connect(sock, @addr, addr.Size); // non-blocking connect() once
    TNetSocket(sock).MakeBlocking;
    result := nrConnectTimeout;
    repeat
      if TNetSocket(sock).WaitFor(1, [neWrite]) = [neWrite] then
      begin
        result := nrOK;
        break;
      end;
      SleepHiRes(1); // wait for actual connection
    until (connectendtix = 0) or
          (mormot.core.os.GetTickCount64 > connectendtix);
  end
  else
  repeat
    if dobind then
    begin
      // bound Socket should remain open for 5 seconds after a closesocket()
      TNetSocket(sock).SetLinger(5);
      // Server-side binding/listening of the socket to the address:port
      if (bind(sock, @addr, addr.Size) <> NO_ERROR) or
         ((layer <> nlUdp) and
          (listen(sock, DefaultListenBacklog) <> NO_ERROR)) then
        result := NetLastError(WSAEADDRNOTAVAIL);
    end
    else
      // open blocking Client connection (use system-defined timeout)
      if connect(sock, @addr, addr.Size) <> NO_ERROR then
        result := NetLastError(WSAEADDRNOTAVAIL);
    if (result = nrOK) or
       (retry <= 0) then
      break;
    dec(retry);
    SleepHiRes(10);
  until false;
  if result <> nrOK then
  begin
    // this address:port seems invalid or already bound
    closesocket(sock);
    if fromcache then
      // ensure the cache won't contain this faulty address any more
      NewSocketAddressCache.Flush(address);
  end
  else
  begin
    // Socket is successfully connected -> setup the connection
    if tobecached then
      // update cache once we are sure the host actually exists
      NewSocketAddressCache.Add(address, addr);
    netsocket := TNetSocket(sock);
    netsocket.SetupConnection(layer, sendtimeout, recvtimeout);
    if netaddr <> nil then
      MoveFast(addr, netaddr^, addr.Size);
  end;
end;


{ TNetSocketWrap }

procedure TNetSocketWrap.SetOpt(prot, name: integer;
  value: pointer; valuelen: integer);
begin
  if @self = nil then
    raise ENetSock.Create('SetOptions(%d,%d) with no socket', [prot, name]);
  if setsockopt(TSocket(@self), prot, name, value, valuelen) <> NO_ERROR then
    raise ENetSock.Create('SetOptions(%d,%d)', [prot, name], NetLastError);
end;

function TNetSocketWrap.GetOptInt(prot, name: integer): integer;
var
  len: integer;
begin
  if @self = nil then
    raise ENetSock.Create('GetOptInt(%d,%d) with no socket', [prot, name]);
  result := 0;
  len := SizeOf(result);
  if getsockopt(TSocket(@self), prot, name, @result, @len) <> NO_ERROR then
    raise ENetSock.Create('GetOptInt(%d,%d)', [prot, name], NetLastError);
end;

procedure TNetSocketWrap.SetKeepAlive(keepalive: boolean);
var
  v: integer;
begin
  v := ord(keepalive);
  SetOpt(SOL_SOCKET, SO_KEEPALIVE, @v, SizeOf(v));
end;

procedure TNetSocketWrap.SetNoDelay(nodelay: boolean);
var
  v: integer;
begin
  v := ord(nodelay);
  SetOpt(IPPROTO_TCP, TCP_NODELAY, @v, SizeOf(v));
end;

procedure TNetSocketWrap.SetSendBufferSize(bytes: integer);
begin
  SetOpt(SOL_SOCKET, SO_SNDBUF, @bytes, SizeOf(bytes));
end;

procedure TNetSocketWrap.SetRecvBufferSize(bytes: integer);
begin
  SetOpt(SOL_SOCKET, SO_RCVBUF, @bytes, SizeOf(bytes));
end;

function TNetSocketWrap.GetSendBufferSize: integer;
begin
  result := GetOptInt(SOL_SOCKET, SO_SNDBUF);
  // typical value on Linux is 2626560 bytes for TCP (16384 for accept),
  // 212992 for Unix socket - on Windows, default is 8192
end;

function TNetSocketWrap.GetRecvBufferSize: integer;
begin
  result := GetOptInt(SOL_SOCKET, SO_RCVBUF);
  // typical value on Linux is 131072 bytes for TCP, 212992 for Unix socket
  // - on Windows, default is 8192
end;

procedure TNetSocketWrap.SetupConnection(layer: TNetLayer;
  sendtimeout, recvtimeout: integer);
begin
  if @self = nil then
    exit;
  if sendtimeout > 0 then
    SetSendTimeout(sendtimeout);
  if recvtimeout > 0 then
    SetReceiveTimeout(recvtimeout);
  if layer = nlTcp then
  begin
    SetNoDelay(true);   // disable Nagle algorithm (we use our own buffers)
    SetKeepAlive(true); // enabled TCP keepalive
  end;
end;

{$ifdef OSWINDOWS}
type
  tsocklen = integer;
{$endif OSWINDOWS}

function TNetSocketWrap.Accept(out clientsocket: TNetSocket;
  out addr: TNetAddr): TNetResult;
var
  len: tsocklen;
  sock: TSocket;
begin
  if @self = nil then
    result := nrNoSocket
  else
  begin
    len := SizeOf(addr);
    sock := mormot.net.sock.accept(TSocket(@self), @addr, len);
    if sock = -1 then
    begin
      result := NetLastError;
      if result = nrOk then
        result := nrNotImplemented;
    end
    else
    begin
      clientsocket := TNetSocket(sock);
      result := nrOK;
    end;
  end;
end;

function TNetSocketWrap.GetPeer(out addr: TNetAddr): TNetResult;
var
  len: tsocklen;
begin
  FillCharFast(addr, SizeOf(addr), 0);
  if @self = nil then
    result := nrNoSocket
  else
  begin
    len := SizeOf(addr);
    result := NetCheck(getpeername(TSocket(@self), @addr, len));
  end;
end;

function TNetSocketWrap.SetIoMode(async: cardinal): TNetResult;
begin
  if @self = nil then
    result := nrNoSocket
  else
    result := NetCheck(ioctlsocket(TSocket(@self), FIONBIO, @async));
end;

function TNetSocketWrap.MakeAsync: TNetResult;
begin
  result := SetIoMode(1);
end;

function TNetSocketWrap.MakeBlocking: TNetResult;
begin
  result := SetIoMode(0);
end;

function TNetSocketWrap.Send(Buf: pointer; var len: integer): TNetResult;
begin
  if @self = nil then
    result := nrNoSocket
  else
  begin
    len := mormot.net.sock.send(TSocket(@self), Buf, len, MSG_NOSIGNAL);
    // Upon successful completion, send() shall return the number of bytes sent.
    // Otherwise, -1 is returned and errno set to indicate the error. (man send)
    if len < 0 then
      result := NetLastError
    else
      result := nrOK;
  end;
end;

function TNetSocketWrap.Recv(Buf: pointer; var len: integer): TNetResult;
begin
  if @self = nil then
    result := nrNoSocket
  else
  begin
    len := mormot.net.sock.recv(TSocket(@self), Buf, len, 0);
    // man recv: Upon successful completion, recv() shall return the length of
    // the message in bytes. If no messages are available to be received and the
    // peer has performed an orderly shutdown, recv() shall return 0.
    // Otherwise, -1 shall be returned and errno set to indicate the error,
    // which may be nrRetry if no data is available.
    if len <= 0 then
      if len = 0 then
        result := nrClosed
      else
        result := NetLastError
    else
      result := nrOK;
  end;
end;

function TNetSocketWrap.SendTo(Buf: pointer; len: integer;
  out addr: TNetAddr): TNetResult;
begin
  if @self = nil then
    result := nrNoSocket
  else if mormot.net.sock.sendto(TSocket(@self), Buf, len, 0, @addr, SizeOf(addr)) < 0 then
    result := NetLastError
  else
    result := nrOk;
end;

function TNetSocketWrap.RecvFrom(Buf: pointer; len: integer; out addr: TNetAddr): integer;
var
  addrlen: integer;
begin
  if @self = nil then
    result := -1
  else
  begin
    addrlen := SizeOf(addr);
    result := mormot.net.sock.recvfrom(TSocket(@self), Buf, len, 0, @addr, @addrlen);
  end;
end;

function TNetSocketWrap.RecvPending(out pending: integer): TNetResult;
begin
  if @self = nil then
  begin
    pending := 0;
    result := nrNoSocket;
  end
  else
    result := NetCheck(ioctlsocket(TSocket(@self), FIONREAD, @pending));
end;

function TNetSocketWrap.RecvWait(ms: integer;
  out data: RawByteString; terminated: PTerminated): TNetResult;
var
  events: TNetEvents;
  pending: integer;
begin
  events := WaitFor(100, [neRead]);
  if (neError in events) or
     (Assigned(terminated) and
      terminated^) then
    result := nrClosed
  else if neRead in events then
  begin
    result := RecvPending(pending);
    if result = nrOK then
      if pending > 0 then
      begin
        SetLength(data, pending);
        result := Recv(pointer(data), pending);
        if Assigned(terminated) and
           terminated^ then
          result := nrClosed;
        if result <> nrOK then
          exit;
        if pending <= 0 then
        begin
          result := nrUnknownError;
          exit;
        end;
        if pending <> length(data) then
          SetLength(data, pending);
      end
      else
        result := nrRetry;
  end
  else
    result := nrRetry;
end;

function TNetSocketWrap.SendAll(Buf: PByte; len: integer;
  terminated: PTerminated): TNetResult;
var
  sent: integer;
begin
  repeat
    sent := len;
    result := Send(Buf, len);
    if Assigned(terminated) and
       terminated^ then
      break;
    if sent > 0 then
    begin
      inc(Buf, sent);
      dec(len, sent);
      if len = 0 then
        exit;
    end;
    if result <> nrRetry then
      exit;
    SleepHiRes(1);
  until Assigned(terminated) and
        terminated^;
  result := nrClosed;
end;

function TNetSocketWrap.ShutdownAndClose(rdwr: boolean): TNetResult;
const
  SHUT_: array[boolean] of integer = (
    SHUT_RD, SHUT_RDWR);
begin
  if @self = nil then
    result := nrNoSocket
  else
  begin
    {$ifdef OSLINUX}
    // on Linux close() is enough after accept (e.g. nginx don't call shutdown)
    if rdwr then
    {$endif OSLINUX}
      shutdown(TSocket(@self), SHUT_[rdwr]);
    result := Close;
  end;
end;

function TNetSocketWrap.Close: TNetResult;
begin
  if @self = nil then
    result := nrNoSocket
  else
  begin
    closesocket(TSocket(@self)); // SO_LINGER usually set to 5 or 10 seconds
    result := nrOk;
  end;
end;

function TNetSocketWrap.Socket: PtrInt;
begin
  result := TSocket(@self);
end;


{ ******************** Efficient Multiple Sockets Polling }

{ TPollSocketAbstract }

class function TPollSocketAbstract.New: TPollSocketAbstract;
begin
  result := PollSocketClass.Create;
end;

constructor TPollSocketAbstract.Create;
begin
end;


{ TPollSockets }

constructor TPollSockets.Create(aPollClass: TPollSocketClass);
begin
  inherited Create;
  InitializeCriticalSection(fPendingLock);
  InitializeCriticalSection(fPollLock);
  if aPollClass = nil then
    fPollClass := PollSocketClass
  else
    fPollClass := aPollClass;
  {$ifdef OSPOSIX}
  SetFileOpenLimit(GetFileOpenLimit(true)); // set soft limit to hard value
  {$endif OSPOSIX}
end;

destructor TPollSockets.Destroy;
var
  i: PtrInt;
  endtix: Int64; // never wait forever
begin
  Terminate;
  if fGettingOne > 0 then
  begin
    if Assigned(fOnLog) then
      fOnLog(sllTrace, 'Destroy: wait for fGettingOne=%', [fGettingOne], self);
    endtix := mormot.core.os.GetTickCount64 + 5000;
    while (fGettingOne > 0) and
          (mormot.core.os.GetTickCount64 < endtix) do
      SleepHiRes(1);
    if Assigned(fOnLog) then
      fOnLog(sllTrace, 'Destroy: ended as fGettingOne=%', [fGettingOne], self);
  end;
  for i := 0 to high(fPoll) do
    FreeAndNilSafe(fPoll[i]);
  if fUnsubscribeShouldShutdownSocket and
     (fSubscription.UnsubscribedCount > 0) then
  begin
    if Assigned(fOnLog) then
      fOnLog(sllTrace, 'Destroy: shutdown UnsubscribedCount=%',
        [fSubscription.UnsubscribedCount], self);
    for i := 0 to fSubscription.UnsubscribedCount - 1 do
       fSubscription.Unsubscribed[i].ShutdownAndClose({rdwr=}false);
  end;
  DeleteCriticalSection(fPendingLock);
  DeleteCriticalSection(fPollLock);
  inherited Destroy;
end;

function TPollSockets.Subscribe(socket: TNetSocket; events: TPollSocketEvents;
  tag: TPollSocketTag): boolean;
var
  n: PtrInt;
  one: TPollSocketsSubscribe;
begin
  result := false;
  if (self = nil) or
     (socket = nil) or
     (events = []) then
    exit;
  // trick is to append the information to fSubscription.Subscribe[]
  one.socket := socket;
  one.tag := tag;
  one.events := events;
  mormot.core.os.EnterCriticalSection(fPendingLock);
  try
    n := fSubscription.SubscribeCount;
    if n = length(fSubscription.Subscribe) then
      SetLength(fSubscription.Subscribe, n + 64);
    fSubscription.Subscribe[n] := one;
    fSubscription.SubscribeCount := n + 1;
  finally
    mormot.core.os.LeaveCriticalSection(fPendingLock);
  end;
  result := true;
end;

function FindPending(res: PPollSocketResult; n: PtrInt;
  tag: TPollSocketTag): PPollSocketResult;
  {$ifdef HASINLINE} inline; {$endif}
begin
  if n > 0 then
  begin
    result := res;
    repeat
      if result^.tag = tag then
        exit;
      inc(result);
      dec(n);
    until n = 0;
  end;
  result := nil;
end;

procedure TPollSockets.Unsubscribe(socket: TNetSocket; tag: TPollSocketTag);
var
  fnd: PPollSocketResult;
begin
  mormot.core.os.EnterCriticalSection(fPendingLock);
  try
    if fPending.Count <> 0 then
    begin
      if Assigned(fOnLog) then
        fOnLog(sllTrace, 'Unsubscribe(%) count=% pending #%/%',
          [pointer(tag), fCount, fPendingIndex, fPending.Count], self);
      fnd := FindPending(@fPending.Events[fPendingIndex],
               fPending.Count - fPendingIndex, tag);
      if fnd <> nil then
      begin
        byte(fnd^.events) := 0; // GetOnePending() will just ignore it
        if Assigned(fOnLog) then
          fOnLog(sllTrace, 'Unsubscribed(%) events:=0 count=%',
            [pointer(fnd^.tag), fPending.Count], self);
      end;
    end;
    AddPtrUInt(fLastUnsubscribedTag, fLastUnsubscribedTagCount, tag);
    AddPtrUInt(TPtrUIntDynArray(fSubscription.Unsubscribed),
      fSubscription.UnsubscribedCount, PtrUInt(socket));
  finally
    mormot.core.os.LeaveCriticalSection(fPendingLock);
  end;
end;

procedure TPollSockets.NoMorePending;
begin
  fPending.Count := 0; // reuse shared Events[] memory
  fPendingIndex := 0;
  fLastUnsubscribedTagCount := 0;
end;

function TPollSockets.IsValidPending(tag: TPollSocketTag): boolean;
begin
  result := true;
end;

function TPollSockets.GetOnePending(out notif: TPollSocketResult;
  const call: RawUtf8): boolean;
var
  ndx: PtrInt;
label
  ok;
begin
  result := false;
  if fTerminated or
     (fPending.Count <= 0) then
    exit;
  mormot.core.os.EnterCriticalSection(fPendingLock);
  {$ifdef HASFASTTRYFINALLY} // make a huge performance difference
  try
  {$endif HASFASTTRYFINALLY}
    ndx := fPendingIndex;
    if ndx < fPending.Count then
    begin
      repeat
        // retrieve next notified event
        notif := fPending.Events[ndx];
        // move forward
        inc(ndx);
        if (byte(notif.events) <> 0) and // Unsubscribe() may have reset to 0
           IsValidPending(notif.tag) and
           ((fLastUnsubscribedTagCount = 0) or
            not PtrUIntScanExists(pointer(fLastUnsubscribedTag),
              fLastUnsubscribedTagCount, notif.tag)) then
        begin
          // there is a not-void event to return
          if Assigned(fOnLog) then
            fOnLog(sllTrace, 'GetOnePending(%)=% % #%/%', [call,
              pointer(notif.tag), byte(notif.events), ndx, fPending.Count], self);
          result := true;
          fPendingIndex := ndx; // continue with next event
          // quick exit with one notified event
          if ndx = fPending.Count then
            break;
          goto ok;
        end;
      until ndx >= fPending.Count;
      {if Assigned(fOnLog) then
        fOnLog(sllTrace, 'GetOnePending(%): reset after reached #%/% lastuns=%',
          [call, ndx, fPending.Count, fLastUnsubscribedTagCount], self);}
      fPending.Count := 0; // reuse shared Events[] memory
      fPendingIndex := 0;
      fLastUnsubscribedTagCount := 0;
ok: end;
  {$ifdef HASFASTTRYFINALLY}
  finally
    mormot.core.os.LeaveCriticalSection(fPendingLock);
  end;
  {$else}
  mormot.core.os.LeaveCriticalSection(fPendingLock);
  {$endif HASFASTTRYFINALLY}
end;

function MergePendingEvents(var res: TPollSocketResults; resindex: PtrInt;
  new: PPollSocketResult; newcount: integer): integer;
var
  n: PtrInt;
  exist: PPollSocketResult;
begin
  result := 0; // returns number of new events to process
  n := res.Count - resindex;
  // here we know that new.Count > 0 and n > 0
  if resindex <> 0 then
    MoveFast(res.Events[resindex], res.Events[0], n * SizeOf(res.Events[0]));
  if n + newCount > length(res.Events) then
    SetLength(res.Events, n + newCount + 16); // seldom needed in practice
  // remove any duplicate: PollForPendingEvents() called before GetOnePending()
  res.Count := n;
  repeat
    // O(n*m) is faster than UnSubscribe/Subscribe because n and m are small
    exist := FindPending(pointer(res.Events), n, new^.tag);
    if exist = nil then
    begin
      // new event to process
      res.Events[res.Count] := new^;
      inc(res.Count);
      inc(result);
    end
    else
      // merge with existing notification flags
      exist^.events := exist^.events + new^.events;
    inc(new);
    dec(newCount);
  until newCount = 0;
end;

function TPollSockets.PollForPendingEvents(timeoutMS: integer): integer;
var
  u, s, p, last, lastcount, n: PtrInt;
  poll: TPollSocketAbstract;
  sock: TNetSocket;
  // some local variables to avoid nested locks
  sub: TPollSocketsSubscription;
  new: TPollSocketResults;
begin
  // by design, this method is called from a single thread
  result := 0;
  if fTerminated or
     (fCount + fSubscription.SubscribeCount = 0) then
    exit;
  LockedInc32(@fGettingOne);
  try
    // thread-safe get the pending (un)subscriptions
    last := -1;
    new.Count := 0;
    mormot.core.os.EnterCriticalSection(fPendingLock);
    try
      if fPending.Count = 0 then
        new.Events := fPending.Events; // reuse the main dynamic array
      sub.SubscribeCount := fSubscription.SubscribeCount;
      sub.UnsubscribedCount := fSubscription.UnsubscribedCount;
      if (sub.SubscribeCount <> 0) or
         (sub.UnsubscribedCount <> 0) then
      begin
        MoveAndZero(@fSubscription, @sub, SizeOf(fSubscription));
        if Assigned(fOnLog) then
          fOnLog(sllTrace, 'PollForPendingEvents sub=% unsub=%',
            [sub.SubscribeCount, sub.UnsubscribedCount], self);
      end;
    finally
      mormot.core.os.LeaveCriticalSection(fPendingLock);
    end;
    // use fPoll[] to retrieve any pending notifications
    mormot.core.os.EnterCriticalSection(fPollLock);
    try
      // first unsubscribe closed connections
      for u := 0 to sub.UnsubscribedCount - 1 do
      begin
        sock := sub.Unsubscribed[u];
        if not fUnsubscribeShouldShutdownSocket then
          for s := 0 to sub.SubscribeCount - 1 do
            if sub.Subscribe[s].socket = sock then
            begin
              if Assigned(fOnLog) then
                fOnLog(sllTrace, 'PollForPendingEvents sub+unsub sock=%',
                  [pointer(sock)], self);
              sock := nil; // Unsubscribe after/before Subscribe -> no op
              sub.Subscribe[s].socket := nil;
              break;
            end;
        if sock <> nil then
          for p := 0 to length(fPoll) - 1 do
            if fPoll[p].Unsubscribe(sock) then
            begin
              dec(fCount);
              if fUnsubscribeShouldShutdownSocket then
                sock.ShutdownAndClose({rdwr=}false);
              {if Assigned(fOnLog) then
                fOnLog(sllTrace, 'PollForPendingEvents Unsubscribe(%) count=%',
                  [pointer(sock), fCount], self);}
              sock := nil;
              break;
            end;
        if sock <> nil then
          if Assigned(fOnLog) then
            fOnLog(sllTrace, 'PollForPendingEvents Unsubscribe(%) failed count=%',
              [pointer(sock), fCount], self);
      end;
      // then subscribe to the new connections
      for s := 0 to sub.SubscribeCount - 1 do
        if sub.Subscribe[s].socket <> nil then
        begin
          poll := nil;
          n := length(fPoll);
          for p := 0 to n - 1 do
            if fPoll[p].Count < fPoll[p].MaxSockets then
            begin
              poll := fPoll[p]; // stil some place in this poll instance
              break;
            end;
          if poll = nil then
          begin
            poll := fPollClass.Create; // need a new poll instance
            SetLength(fPoll, n + 1);
            fPoll[n] := poll;
          end;
          if Assigned(fOnLog) then
            fOnLog(sllTrace, 'PollForPendingEvents Subscribe(%) count=%',
              [pointer(sub.Subscribe[s].socket), fCount], self);
          with sub.Subscribe[s] do
            if poll.Subscribe(socket, events, tag) then
              inc(fCount)
            else if Assigned(fOnLog) then
              fOnLog(sllTrace, 'PollForPendingEvents Subscribe(%) failed count=%',
                [pointer(socket), fCount], self);
        end;
      // eventually do the actual polling
      if fTerminated or
         (fCount = 0) then
        exit; // nothing to track any more (all unsubscribed)
      n := length(fPoll);
      if n = 0 then
        exit;
      if timeoutMS > 0 then
      begin
        timeoutMS := timeoutMS div n;
        if timeoutMS = 0 then
          timeoutMS := 1;
      end;
      // calls fPoll[].WaitForModified() to refresh pending state
      for p := fPollIndex + 1 to n - 1 do
        // search from fPollIndex = last found
        if fTerminated then
          exit
        else if fPoll[p].WaitForModified(new, timeoutMS) then
        begin
          last := p;
          break;
        end;
      if last < 0 then
        for p := 0 to fPollIndex do
          // search from beginning up to fPollIndex
          if fTerminated then
            exit
          else if fPoll[p].WaitForModified(new, timeoutMS) then
          begin
            last := p;
            break;
          end;
      if last < 0 then
        exit;
      // WaitForModified() did return some events in new local list
      result := new.Count;
      fPollIndex := last; // next call will continue from fPoll[fPollIndex+1]
      if (result = 0) or
         fTerminated then
        exit;
      lastcount := fPoll[last].Count;
    finally
      LeaveCriticalSection(fPollLock);
    end;
    // append the new events to the main fPending list
    EnterCriticalSection(fPendingLock);
    try
      if (fPending.Count = 0) or
         (fPendingIndex = fPending.Count) then
        fPending := new // atomic list assignment (most common case)
      else
        result := MergePendingEvents(fPending, fPendingIndex,
          pointer(new.Events), new.Count); // avoid duplicates
      fPendingIndex := 0;
      new.Events := nil;
      if (result > 0) and
         Assigned(fOnLog) then
        fOnLog(sllTrace,
          'PollForPendingEvents=% in fPoll[%] (subscribed=%) pending=%',
            [result, last, lastcount, fPending.Count], self);
    finally
      mormot.core.os.LeaveCriticalSection(fPendingLock);
    end;
  finally
    LockedDec32(@fGettingOne);
  end;
end;

procedure TPollSockets.AddOnePending(
  aTag: TPollSocketTag; aEvents: TPollSocketEvents);
var
  n: PtrInt;
begin
  mormot.core.os.EnterCriticalSection(fPendingLock);
  try
    n := fPending.Count;
    if (n = 0) or
       (FindPending(@fPending.Events[fPendingIndex],
          n - fPendingIndex, aTag) = nil) then
    begin
      if n >= length(fPending.Events) then
        SetLength(fPending.Events, n + 32);
      with fPending.Events[n] do
      begin
        tag := aTag;
        events := aEvents;
      end;
      fPending.Count := n + 1;
    end;
  finally
    mormot.core.os.LeaveCriticalSection(fPendingLock);
  end;
end;

function TPollSockets.GetOne(timeoutMS: integer; const call: RawUtf8;
  out notif: TPollSocketResult): boolean;
var
  start, tix, endtix: Int64;
begin
  // first check if some pending events are available
  result := GetOnePending(notif, call);
  if result or
     fTerminated or
     (timeoutMS < 0) then
    exit;
  // here we need to ask the socket layer
  byte(notif.events) := 0;
  start := 0;
  endtix := 0;
  LockedInc32(@fGettingOne);
  try
    repeat
      // non-blocking search of pending events within all subscribed fPoll[]
      if fTerminated then
        exit;
      if fPending.Count = 0 then
        PollForPendingEvents({timeoutMS=}0);
      if fTerminated then
        exit;
      if GetOnePending(notif, call) then
      begin
        result := true;
        exit;
      end;
      // if we reached here, we have no pending event
      if fTerminated or
         (timeoutMS = 0) then
        exit;
      // wait a little for something to happen
      tix := SleepStep(start, @fTerminated); // 0/1/5/50/120-250 ms steps
      if endtix = 0 then
        endtix := start + timeoutMS
      else if Assigned(fOnGetOneIdle) then
        fOnGetOneIdle(self, tix);
      if fTerminated then
        exit;
      result := GetOnePending(notif, call); // retrieved from another thread?
    until result or
          (tix > endtix);
  finally
    LockedDec32(@fGettingOne);
  end;
end;

procedure TPollSockets.Terminate;
begin
  if self <> nil then
    fTerminated := true;
end;


{ *************************** TUri parsing/generating URL wrapper }

function StartWith(p, up: PUtf8Char): boolean;
// to avoid linking mormot.core.text for IdemPChar()
var
  c, u: AnsiChar;
begin
  result := false;
  if (p = nil) or
     (up = nil) then
    exit;
  repeat
    u := up^;
    if u = #0 then
      break;
    inc(up);
    c := p^;
    inc(p);
    if c = u  then
      continue;
    if (c >= 'a') and
       (c <= 'z') then
    begin
      dec(c, 32);
      if c <> u then
        exit;
    end
    else
      exit;
  until false;
  result := true;
end;

function SockBase64Encode(const s: RawUtf8): RawUtf8;
// to avoid linking mormot.core.buffers for BinToBase64()

  procedure DoEncode(rp, sp: PAnsiChar; len: cardinal);
  const
    b64: array[0..63] of AnsiChar =
      'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  var
    i, c: cardinal;
  begin
    for i := 1 to len div 3 do
    begin
      c := ord(sp[0]) shl 16 + ord(sp[1]) shl 8 + ord(sp[2]);
      rp[0] := b64[(c shr 18) and $3f];
      rp[1] := b64[(c shr 12) and $3f];
      rp[2] := b64[(c shr 6) and $3f];
      rp[3] := b64[c and $3f];
      inc(rp, 4);
      inc(sp, 3);
    end;
    case len mod 3 of
      1:
        begin
          c := ord(sp[0]) shl 16;
          rp[0] := b64[(c shr 18) and $3f];
          rp[1] := b64[(c shr 12) and $3f];
          rp[2] := '=';
          rp[3] := '=';
        end;
      2:
        begin
          c := ord(sp[0]) shl 16 + ord(sp[1]) shl 8;
          rp[0] := b64[(c shr 18) and $3f];
          rp[1] := b64[(c shr 12) and $3f];
          rp[2] := b64[(c shr 6) and $3f];
          rp[3] := '=';
        end;
    end;
  end;

var
  len: integer;
begin
  result:='';
  len := length(s);
  if len = 0 then
    exit;
  SetLength(result, ((len + 2) div 3) * 4);
  DoEncode(pointer(result), pointer(s), len);
end;

function SplitFromRight(const Text: RawUtf8; Sep: AnsiChar;
  var Before, After: RawUtf8): boolean;
var
  i: PtrInt;
begin
  for i := length(Text) - 1 downto 2 do // search Sep from right side
    if Text[i] = Sep then
    begin
      TrimCopy(Text, 1, i - 1, Before);
      TrimCopy(Text, i + 1, maxInt, After);
      result := true;
      exit;
    end;
  result := false;
end;


{ TUri }

procedure TUri.Clear;
begin
  Https := false;
  layer := nlTcp;
  Finalize(self);
end;

function TUri.From(aUri: RawUtf8; const DefaultPort: RawUtf8): boolean;
var
  P, S, P1, P2: PAnsiChar;
  i: integer;
begin
  Clear;
  result := false;
  aUri := TrimU(aUri);
  if aUri = '' then
    exit;
  P := pointer(aUri);
  S := P;
  while S^ in ['a'..'z', 'A'..'Z', '+', '-', '.', '0'..'9'] do
    inc(S);
  if PInteger(S)^ and $ffffff = ord(':') + ord('/') shl 8 + ord('/') shl 16 then
  begin
    FastSetString(Scheme, P, S - P);
    if StartWith(pointer(P), 'HTTPS') then
      Https := true
    else if StartWith(pointer(P), 'UDP') then
      layer := nlUdp; // 'udp://server:port';
    P := S + 3;
  end;
  if StartWith(pointer(P), 'UNIX:') then
  begin
    inc(P, 5); // 'http://unix:/path/to/socket.sock:/url/path'
    layer := nlUnix;
    S := P;
    while not (S^ in [#0, ':']) do
      inc(S); // Server='path/to/socket.sock'
  end
  else
  begin
    P1 := pointer(PosChar(pointer(P), '@'));
    if P1 <> nil then
    begin
      // parse 'https://user:password@server:port/address'
      P2 := pointer(PosChar(pointer(P), '/'));
      if (P2 = nil) or
         (PtrUInt(P2) > PtrUInt(P1)) then
      begin
        FastSetString(User, P, P1 - P);
        i := PosExChar(':', User);
        if i <> 0 then
        begin
          Password := copy(User, i + 1, 1000);
          SetLength(User, i - 1);
        end;
        P := P1 + 1;
      end;
    end;
    S := P;
    while not (S^ in [#0, ':', '/']) do
      inc(S); // 'server:port/address' or 'server/address'
  end;
  FastSetString(Server, P, S - P);
  if S^ = ':' then
  begin
    inc(S);
    P := S;
    while not (S^ in [#0, '/']) do
      inc(S);
    FastSetString(Port, P, S - P); // Port='' for nlUnix
  end
  else if DefaultPort <> '' then
    port := DefaultPort
  else
    port := DEFAULT_PORT[Https];
  if S^ <> #0 then // ':' or '/'
  begin
    inc(S);
    FastSetString(Address, S, StrLen(S));
  end;
  if Server <> '' then
    result := true;
end;

function TUri.URI: RawUtf8;
const
  Prefix: array[boolean] of RawUtf8 = (
    'http://', 'https://');
begin
  if layer = nlUnix then
    result := 'http://unix:' + Server + ':/' + address
  else if (port = '') or
          (port = '0') or
          (port = DEFAULT_PORT[Https]) then
    result := Prefix[Https] + Server + '/' + address
  else
    result := Prefix[Https] + Server + ':' + port + '/' + address;
end;

function TUri.PortInt: TNetPort;
begin
  result := GetCardinal(pointer(port));
end;

function TUri.Root: RawUtf8;
var
  i: PtrInt;
begin
  i := PosExChar('?', address);
  if i = 0 then
    Root := address
  else
    Root := copy(address, 1, i - 1);
end;

function TUri.UserPasswordBase64: RawUtf8;
begin
  if User = '' then
    result := ''
  else
    result := SockBase64Encode(User + ':' + Password);
end;


{ ********* TCrtSocket Buffered Socket Read/Write Class }

{ TCrtSocket }

function TCrtSocket.GetRawSocket: PtrInt;
begin
  result := PtrInt(fSock);
end;

procedure TCrtSocket.SetKeepAlive(aKeepAlive: boolean);
begin
  fSock.SetKeepAlive(aKeepAlive);
end;

procedure TCrtSocket.SetLinger(aLinger: integer);
begin
  fSock.SetLinger(aLinger);
end;

procedure TCrtSocket.SetReceiveTimeout(aReceiveTimeout: integer);
begin
  fSock.SetReceiveTimeout(aReceiveTimeout);
end;

procedure TCrtSocket.SetSendTimeout(aSendTimeout: integer);
begin
  fSock.SetSendTimeout(aSendTimeout);
end;

procedure TCrtSocket.SetTcpNoDelay(aTcpNoDelay: boolean);
begin
  fSock.SetNoDelay(aTcpNoDelay);
end;

constructor TCrtSocket.Create(aTimeOut: PtrInt);
begin
  fTimeOut := aTimeOut;
end;

constructor TCrtSocket.Open(const aServer, aPort: RawUtf8;
  aLayer: TNetLayer; aTimeOut: cardinal; aTLS: boolean;
  aTLSContext: PNetTlsContext; aTunnel: PUri);
begin
  Create(aTimeOut); // default read timeout is 10 seconds
  // copy the input parameters before OpenBind()
  if aTLSContext <> nil then
    TLS := aTLSContext^;
  if (aTunnel <> nil) and
     (aTunnel^.Server <> '') then
    Tunnel := aTunnel^;
  // OpenBind() raise an exception on error
  {$ifdef OSPOSIX}
  if StartWith(pointer(aServer), 'UNIX:') then
  begin
    // aServer='unix:/path/to/myapp.socket'
    OpenBind(copy(aServer, 6, 200), '', {dobind=}false, aTLS, nlUnix);
    fServer := aServer; // keep the full server name if reused
  end
  else
  {$endif OSPOSIX}
    OpenBind(aServer, aPort, {dobind=}false, aTLS, aLayer);
end;

constructor TCrtSocket.OpenUri(const aUri: RawUtf8; out aAddress: RawUtf8;
  const aTunnel: RawUtf8; aTimeOut: cardinal; aTLSContext: PNetTlsContext);
var
  u, t: TUri;
begin
  if not u.From(aUri) then
    raise ENetSock.Create('%s.OpenUri: invalid %s', [ClassNameShort(self)^, aUri]);
  aAddress := u.Address;
  t.From(aTunnel);
  Open(u.Server, u.Port, nlTcp, aTimeOut, u.Https, aTLSContext, @t);
end;

const
  BINDTXT: array[boolean] of string[4] = (
    'open', 'bind');
  BINDMSG: array[boolean] of string = (
    'Is a server available on this address:port?',
    'Another process may be currently listening to this port!');

constructor TCrtSocket.Bind(const aAddress: RawUtf8; aLayer: TNetLayer;
  aTimeOut: integer);
var
  s, p: RawUtf8;
  aSock: integer;
begin
  Create(aTimeOut);
  if aAddress = '' then
  begin
    {$ifdef OSLINUX} // try systemd activation
    if not sd.IsAvailable then
      raise ENetSock.Create('%s.Bind('''') but Systemd is not available',
        [ClassNameShort(self)^]);
    if sd.listen_fds(0) > 1 then
      raise ENetSock.Create('%s.Bind(''''): Systemd activation failed - too ' +
        'many file descriptors received', [ClassNameShort(self)^]);
    aSock := SD_LISTEN_FDS_START + 0;
    {$else}
    raise ENetSock.Create('%s.Bind(''''), i.e. Systemd activation, ' +
      'is not allowed on this platform', [ClassNameShort(self)^]);
    {$endif OSLINUX}
  end
  else
  begin
    aSock := -1; // force OpenBind to create listening socket
    if not SplitFromRight(aAddress, ':', s, p) then
    begin
      s := '0.0.0.0';
      p := aAddress;
    end;
    {$ifdef OSPOSIX}
    if s = 'unix' then
    begin
      // aAddress='unix:/path/to/myapp.socket'
      FpUnlink(pointer(p)); // previous bind may have left the .socket file
      OpenBind(p, '', {dobind=}true, {tls=}false, nlUnix, {%H-}TNetSocket(aSock));
      exit;
    end;
    {$endif OSPOSIX}
  end;
  // next line will raise exception on error
  OpenBind(s{%H-}, p{%H-}, {dobind=}true, {tls=}false, aLayer, {%H-}TNetSocket(aSock));
end;

procedure TCrtSocket.OpenBind(const aServer, aPort: RawUtf8;
  doBind, aTLS: boolean; aLayer: TNetLayer; aSock: TNetSocket);

  procedure DoTlsHandshake;
  begin
    try
      if not Assigned(NewNetTls) then
        raise ENetSock.Create('%s.OpenBind: TLS is not available - try ' +
          'including mormot.lib.openssl11 and installing OpenSSL 1.1.1',
          [ClassNameShort(self)^]);
      fSecure := NewNetTls;
      if fSecure = nil then
        raise ENetSock.Create('%s.OpenBind; TLS is not available on this ' +
          'system - try installing OpenSSL 1.1.1', [ClassNameShort(self)^]);
      fSecure.AfterConnection(fSock, TLS, fServer);
      TLS.Enabled := true;
    except
      on E: Exception do
      begin
        fSecure := nil;
        raise ENetSock.CreateFmt('%s.OpenBind(%s:%s,%s): TLS failed [%s %s]',
          [ClassNameShort(self)^, fServer, fPort, BINDTXT[doBind],
           ClassNameShort(E)^, E.Message]);
      end;
    end;
  end;

var
  retry: integer;
  head: RawUtf8;
  res: TNetResult;
begin
  fSocketLayer := aLayer;
  fWasBind := doBind;
  if {%H-}PtrInt(aSock)<=0 then
  begin
    // OPEN or BIND mode -> create the socket
    fServer := aServer;
    if (aPort = '') and
       (aLayer <> nlUnix) then
      fPort := DEFAULT_PORT[aTLS] // default port is 80/443 (HTTP/S)
    else
      fPort := aPort;
    if doBind then
      // allow small number of retries (e.g. XP or BSD during aggressive tests)
      retry := 10
    else if (Tunnel.Server <> '') and
            (Tunnel.Server <> fServer) and
            (aLayer = nlTcp) then
    begin
      // handle client tunnelling via an HTTP(s) proxy
      fProxyUrl := Tunnel.URI;
      if Tunnel.Https and aTLS then
        raise ENetSock.Create('%s.Open(%s:%s): %s proxy - unsupported dual ' +
          'TLS layers', [ClassNameShort(self)^, fServer, fPort, fProxyUrl]);
      try
        res := NewSocket(Tunnel.Server, Tunnel.Port, nlTcp, {doBind=}false,
          fTimeout, fTimeout, fTimeout, {retry=}2, fSock);
        if res = nrOK then
        begin
          res := nrRefused;
          SockSend(['CONNECT ', fServer, ':', fPort, ' HTTP/1.0']);
          if Tunnel.User <> '' then
            SockSend(['Proxy-Authorization: Basic ', Tunnel.UserPasswordBase64]);
          SockSendFlush(#13#10);
          repeat
            SockRecvLn(head);
            if StartWith(pointer(head), 'HTTP/') and
               (length(head) > 11) and
               (head[10] = '2') then // 'HTTP/1.1 2xx xxxx' success
              res := nrOK;
          until head = '';
        end;
      except
        on E: Exception do
          raise ENetSock.Create('%s.Open(%s:%s): %s proxy error %s',
            [ClassNameShort(self)^, fServer, fPort, fProxyUrl, E.Message]);
      end;
      if res <> nrOk then
        raise ENetSock.Create('%s.Open(%s:%s): %s proxy error',
          [ClassNameShort(self)^, fServer, fPort, fProxyUrl], res);
      if Assigned(OnLog) then
        OnLog(sllTrace, 'Open(%:%) via proxy %', [fServer, fPort, fProxyUrl], self);
      if aTLS then
        DoTlsHandshake;
      exit;
    end
    else
      // direct client connection
      retry := {$ifdef OSBSD} 10 {$else} 2 {$endif};
    res := NewSocket(fServer, fPort, aLayer, doBind,
      fTimeout, fTimeout, fTimeout, retry, fSock);
    if res <> nrOK then
      raise ENetSock.Create('%s %s.OpenBind(%s:%s)',
        [BINDMSG[doBind], ClassNameShort(self)^, fServer, fPort], res);
  end
  else
  begin
    // ACCEPT mode -> socket is already created by caller
    fSock := aSock;
    if TimeOut > 0 then
    begin
      // set timout values for both directions
      ReceiveTimeout := TimeOut;
      SendTimeout := TimeOut;
    end;
  end;
  if (aLayer = nlTcp) and
     aTLS and
     not doBind and
     ({%H-}PtrInt(aSock) <= 0) then
    DoTlsHandshake;
  if Assigned(OnLog) then
    OnLog(sllTrace, '%(%:%) sock=% %', [BINDTXT[doBind], fServer, fPort,
      fSock.Socket, TLS.CipherName], self);
end;

procedure TCrtSocket.AcceptRequest(aClientSock: TNetSocket; aClientAddr: PNetAddr);
begin
  {$ifdef OSLINUX}
  // on Linux fd returned from accept() inherits all parent fd options
  // except O_NONBLOCK and O_ASYNC
  fSock := aClientSock;
  {$else}
  // on other OS inheritance is undefined, so call OpenBind to set all fd options
  OpenBind('', '', {bind=}false, {tls=}false, fSocketLayer, aClientSock);
  // assign the ACCEPTed aClientSock to this TCrtSocket instance
  Linger := 5; // should remain open for 5 seconds after a closesocket() call
  {$endif OSLINUX}
  if aClientAddr <> nil then
    fRemoteIP := aClientAddr^.IP(RemoteIPLocalHostAsVoidInServers);
  {$ifdef OSLINUX}
  if Assigned(OnLog) then
    OnLog(sllTrace, 'Accept(%:%) sock=% %',
      [fServer, fPort, fSock.Socket, fRemoteIP], self);
  {$endif OSLINUX}
end;

const
  SOCKMINBUFSIZE = 1024; // big enough for headers (content will be read directly)

type
  PTextRec = ^TTextRec;
  PCrtSocket = ^TCrtSocket;

function OutputSock(var F: TTextRec): integer;
begin
  if F.BufPos = 0 then
    result := NO_ERROR
  else if PCrtSocket(@F.UserData)^.TrySndLow(F.BufPtr, F.BufPos) then
  begin
    F.BufPos := 0;
    result := NO_ERROR;
  end
  else
    result := -1; // on socket error -> raise ioresult error
end;

function InputSock(var F: TTextRec): integer;
// SockIn pseudo text file fill its internal buffer only with available data
// -> no unwanted wait time is added
// -> very optimized use for readln() in HTTP stream
var
  size: integer;
  sock: TCrtSocket;
begin
  F.BufEnd := 0;
  F.BufPos := 0;
  sock := PCrtSocket(@F.UserData)^;
  if not sock.SockIsDefined then
  begin
    result := WSAECONNABORTED; // on socket error -> raise ioresult error
    exit; // file closed = no socket -> error
  end;
  result := sock.fSockInEofError;
  if result <> 0 then
    exit; // already reached error below
  size := F.BufSize;
  if sock.SocketLayer = nlUdp then
  begin
    if sock.fPeerAddr = nil then
      New(sock.fPeerAddr); // allocated on demand (may be up to 110 bytes)
    size := sock.Sock.RecvFrom(F.BufPtr, size, sock.fPeerAddr^);
  end
  else
    // nlTcp/nlUnix
    if not sock.TrySockRecv(F.BufPtr, size, {StopBeforeLength=}true) then
      size := -1; // fatal socket error
  // TrySockRecv() may return size=0 if no data is pending, but no TCP/IP error
  if size >= 0 then
  begin
    F.BufEnd := size;
    inc(sock.fBytesIn, size);
    result := NO_ERROR;
  end
  else
  begin
    if not sock.SockIsDefined then // socket broken or closed
      result := WSAECONNABORTED
    else
    begin
      result := -sockerrno; // ioresult = low-level socket error as negative
      if result = 0 then
        result := WSAETIMEDOUT;
    end;
    sock.fSockInEofError := result; // error -> mark end of SockIn
    // result <0 will update ioresult and raise an exception if {$I+}
  end;
end;

function CloseSock(var F: TTextRec): integer;
begin
  if PCrtSocket(@F.UserData)^ <> nil then
    PCrtSocket(@F.UserData)^.Close;
  PCrtSocket(@F.UserData)^ := nil;
  result := NO_ERROR;
end;

function OpenSock(var F: TTextRec): integer;
begin
  F.BufPos := 0;
  F.BufEnd := 0;
  if F.Mode = fmInput then
  begin
    // ReadLn
    F.InOutFunc := @InputSock;
    F.FlushFunc := nil;
  end
  else
  begin
    // WriteLn
    F.Mode := fmOutput;
    F.InOutFunc := @OutputSock;
    F.FlushFunc := @OutputSock;
  end;
  F.CloseFunc := @CloseSock;
  result := NO_ERROR;
end;

{$ifdef FPC}
procedure SetLineBreakStyle(var T: Text; Style: TTextLineBreakStyle);
begin
  case Style of
    tlbsCR:
      TextRec(T).LineEnd := #13;
    tlbsLF:
      TextRec(T).LineEnd := #10;
    tlbsCRLF:
      TextRec(T).LineEnd := #13#10;
  end;
end;
{$endif FPC}

procedure TCrtSocket.CreateSockIn(LineBreak: TTextLineBreakStyle;
  InputBufferSize: integer);
begin
  if (Self = nil) or
     (SockIn <> nil) then
    exit; // initialization already occured
  if InputBufferSize < SOCKMINBUFSIZE then
    InputBufferSize := SOCKMINBUFSIZE;
  GetMem(fSockIn, sizeof(TTextRec) + InputBufferSize);
  FillCharFast(SockIn^, sizeof(TTextRec), 0);
  with TTextRec(SockIn^) do
  begin
    PCrtSocket(@UserData)^ := self;
    Mode := fmClosed;
    // ignore internal Buffer[], which is not trailing on latest Delphi and FPC
    BufSize := InputBufferSize;
    BufPtr := pointer(PAnsiChar(SockIn) + sizeof(TTextRec));
    OpenFunc := @OpenSock;
    Handle := {$ifdef FPC}THandle{$endif}(-1);
  end;
  SetLineBreakStyle(SockIn^, LineBreak); // http does break lines with #13#10
  Reset(SockIn^);
end;

procedure TCrtSocket.CreateSockOut(OutputBufferSize: integer);
begin
  if SockOut <> nil then
    exit; // initialization already occured
  if OutputBufferSize < SOCKMINBUFSIZE then
    OutputBufferSize := SOCKMINBUFSIZE;
  GetMem(fSockOut, sizeof(TTextRec) + OutputBufferSize);
  FillCharFast(SockOut^, sizeof(TTextRec), 0);
  with TTextRec(SockOut^) do
  begin
    PCrtSocket(@UserData)^ := self;
    Mode := fmClosed;
    BufSize := OutputBufferSize;
    BufPtr := pointer(PAnsiChar(SockIn) + sizeof(TTextRec)); // ignore Buffer[] (Delphi 2009+)
    OpenFunc := @OpenSock;
    Handle := {$ifdef FPC}THandle{$endif}(-1);
  end;
  SetLineBreakStyle(SockOut^, tlbsCRLF); // force e.g. for Linux platforms
  Rewrite(SockOut^);
end;

procedure TCrtSocket.CloseSockIn;
begin
  if (self <> nil) and
     (fSockIn <> nil) then
  begin
    Freemem(fSockIn);
    fSockIn := nil;
  end;
end;

procedure TCrtSocket.CloseSockOut;
begin
  if (self <> nil) and
     (fSockOut <> nil) then
  begin
    Freemem(fSockOut);
    fSockOut := nil;
  end;
end;

procedure TCrtSocket.Close;
begin
  fSndBufLen := 0; // always reset (e.g. in case of further Open)
  fSockInEofError := 0;
  ioresult; // reset readln/writeln value
  if SockIn <> nil then
  begin
    PTextRec(SockIn)^.BufPos := 0;  // reset input buffer
    PTextRec(SockIn)^.BufEnd := 0;
  end;
  if SockOut <> nil then
  begin
    PTextRec(SockOut)^.BufPos := 0; // reset output buffer
    PTextRec(SockOut)^.BufEnd := 0;
  end;
  if not SockIsDefined then
    exit; // no opened connection, or Close already executed
  fSecure := nil; // perform the TLS shutdown round and release the TLS context
  {$ifdef OSLINUX}
  if not fWasBind or
     (fPort <> '') then // no explicit shutdown necessary on Linux server side
  {$endif OSLINUX}
    fSock.ShutdownAndClose({rdwr=}fWasBind);
  fSock := TNetSocket(-1);
  // don't reset fServer/fPort/fTls/fWasBind: caller may use them to reconnect
  // (see e.g. THttpClientSocket.Request)
  {$ifdef OSPOSIX}
  if fSocketLayer = nlUnix then
    FpUnlink(pointer(fServer)); // 'unix:/path/to/myapp.socket' -> delete file
  {$endif OSPOSIX}
end;

destructor TCrtSocket.Destroy;
begin
  Close;
  CloseSockIn;
  CloseSockOut;
  if fPeerAddr <> nil then
    Dispose(fPeerAddr);
  inherited Destroy;
end;

function TCrtSocket.SockInRead(Content: PAnsiChar; Length: integer;
  UseOnlySockIn: boolean): integer;
var
  len, res: integer;
// read Length bytes from SockIn^ buffer + Sock if necessary
begin
  // get data from SockIn buffer, if any (faster than ReadChar)
  result := 0;
  if Length <= 0 then
    exit;
  if SockIn <> nil then
    with PTextRec(SockIn)^ do
      repeat
        len := BufEnd - BufPos;
        if len > 0 then
        begin
          if len > Length then
            len := Length;
          MoveFast(BufPtr[BufPos], Content^, len);
          inc(BufPos, len);
          inc(Content, len);
          dec(Length, len);
          inc(result, len);
        end;
        if Length = 0 then
          exit; // we got everything we wanted
        if not UseOnlySockIn then
          break;
        res := InputSock(PTextRec(SockIn)^);
        if res < 0 then
          ENetSock.CheckLastError('SockInRead', {forceraise=}true);
        // loop until Timeout
      until Timeout = 0;
  // direct receiving of the remaining bytes from socket
  if Length > 0 then
  begin
    SockRecv(Content, Length); // raise ENetSock if failed to read Length
    inc(result, Length);
  end;
end;

function TCrtSocket.SockIsDefined: boolean;
begin
  result := (self <> nil) and
            ({%H-}PtrInt(fSock) > 0);
end;

function TCrtSocket.SockInPending(aTimeOutMS: integer;
  aPendingAlsoInSocket: boolean): integer;
var
  backup: PtrInt;
  {$ifdef OSWINDOWS}
  insocket: integer;
  {$endif OSWINDOWS}
begin
  if SockIn = nil then
    raise ENetSock.Create('%s.SockInPending(SockIn=nil)',
      [ClassNameShort(self)^]);
  if aTimeOutMS < 0 then
    raise ENetSock.Create('%s.SockInPending(aTimeOutMS<0)',
      [ClassNameShort(self)^]);
  with PTextRec(SockIn)^ do
    result := BufEnd - BufPos;
  if result = 0 then
    // no data in SockIn^.Buffer, so try if some pending at socket level
    case SockReceivePending(aTimeOutMS) of
      cspDataAvailable:
        begin
          backup := fTimeOut;
          fTimeOut := 0; // not blocking call to fill SockIn buffer
          try
            // call InputSock() to actually retrieve any pending data
            if InputSock(PTextRec(SockIn)^) = NO_ERROR then
              with PTextRec(SockIn)^ do
                result := BufEnd - BufPos
            else
              result := -1; // indicates broken socket
          finally
            fTimeOut := backup;
          end;
        end;
      cspSocketError:
        result := -1; // indicates broken/closed socket
    end; // cspNoData will leave result=0
  {$ifdef OSWINDOWS}
  // under Unix SockReceivePending use poll(fSocket) and if data available
  // ioctl syscall is redundant
  if aPendingAlsoInSocket then
    // also includes data in socket bigger than TTextRec's buffer
    if (sock.RecvPending(insocket) = nrOK) and
       (insocket > 0) then
      inc(result, insocket);
  {$endif OSWINDOWS}
end;

function TCrtSocket.SockConnected: boolean;
var
  addr: TNetAddr;
begin
  result := SockIsDefined and
            (fSock.GetPeer(addr) = nrOK);
end;

procedure TCrtSocket.SockSend(P: pointer; Len: integer);
var
  cap: integer;
begin
  if Len <= 0 then
    exit;
  cap := Length(fSndBuf);
  if Len + fSndBufLen > cap then
    SetLength(fSndBuf, Len + cap + cap shr 3 + 2048);
  MoveFast(P^, PByteArray(fSndBuf)[fSndBufLen], Len);
  inc(fSndBufLen, Len);
end;

procedure TCrtSocket.SockSendCRLF;
var
  cap: integer;
begin
  cap := Length(fSndBuf);
  if fSndBufLen + 2 > cap then
    SetLength(fSndBuf, cap + cap shr 3 + 2048);
  PWord(@PByteArray(fSndBuf)[fSndBufLen])^ := $0a0d;
  inc(fSndBufLen, 2);
end;

procedure TCrtSocket.SockSend(const Values: array of const);
var
  i: PtrInt;
  tmp: shortstring;
begin
  for i := 0 to high(Values) do
    with Values[i] do
      case VType of
        vtString:
          SockSend(@VString^[1], PByte(VString)^);
        vtAnsiString:
          SockSend(VAnsiString, Length(RawByteString(VAnsiString)));
        {$ifdef HASVARUSTRING}
        vtUnicodeString:
          begin
            Unicode_WideToShort(VUnicodeString, // assume WinAnsi encoding
              length(UnicodeString(VUnicodeString)), 1252, tmp);
            SockSend(@tmp[1], Length(tmp));
          end;
        {$endif HASVARUSTRING}
        vtPChar:
          SockSend(VPChar, StrLen(VPChar));
        vtChar:
          SockSend(@VChar, 1);
        vtWideChar:
          SockSend(@VWideChar, 1); // only ansi part of the character
        vtInteger:
          begin
            Str(VInteger, tmp);
            SockSend(@tmp[1], Length(tmp));
          end;
        vtInt64 {$ifdef FPC}, vtQWord{$endif} :
          begin
            Str(VInt64^, tmp);
            SockSend(@tmp[1], Length(tmp));
          end;
      end;
  SockSendCRLF;
end;

procedure TCrtSocket.SockSend(const Line: RawByteString);
begin
  if Line <> '' then
    SockSend(pointer(Line), Length(Line));
  SockSendCRLF;
end;

function TCrtSocket.SockSendRemainingSize: integer;
begin
  result := Length(fSndBuf) - fSndBufLen;
end;

procedure TCrtSocket.SockSendFlush(const aBody: RawByteString);
var
  body: integer;
begin
  body := Length(aBody);
  if (body > 0) and
     (SockSendRemainingSize >= body) then // around 1800 bytes
  begin
    MoveFast(pointer(aBody)^, PByteArray(fSndBuf)[fSndBufLen], body);
    inc(fSndBufLen, body); // append to buffer as single TCP packet
    body := 0;
  end;
  {$ifdef SYNCRTDEBUGLOW}
  if Assigned(OnLog) then
  begin
    OnLog(sllCustom2, 'SockSend sock=% flush len=% body=% %', [fSock.Socket, fSndBufLen,
      Length(aBody), LogEscapeFull(pointer(fSndBuf), fSndBufLen)], self);
    if body > 0 then
      OnLog(sllCustom2, 'SockSend sock=% body len=% %', [fSock.Socket, body,
        LogEscapeFull(pointer(aBody), body)], self);
  end;
  {$endif SYNCRTDEBUGLOW}
  if fSndBufLen > 0 then
    if TrySndLow(pointer(fSndBuf), fSndBufLen) then
      fSndBufLen := 0
    else
      raise ENetSock.Create('%s.SockSendFlush(%s) len=%d',
        [ClassNameShort(self)^, fServer, fSndBufLen], NetLastError);
  if body > 0 then
    SndLow(pointer(aBody), body); // direct sending of biggest packets
end;

procedure TCrtSocket.SockSendStream(Stream: TStream; ChunkSize: integer);
var
  chunk: RawByteString;
  rd: integer;
  pos: Int64;
begin
  SetLength(chunk, ChunkSize);
  pos := 0;
  repeat
    rd := Stream.Read(pointer(chunk)^, ChunkSize);
    if rd = 0 then
      break;
    if not TrySndLow(pointer(chunk), rd) then
      raise ENetSock.Create('%s.SockSendStream(%s,%d) rd=%d pos=%d to %s:%s',
        [ClassNameShort(self)^, ClassNameShort(Stream)^, ChunkSize,
         rd, pos, fServer, fPort], NetLastError);
    inc(pos, rd);
  until false;
end;

procedure TCrtSocket.SockRecv(Buffer: pointer; Length: integer);
var
  read: integer;
begin
  read := Length;
  if not TrySockRecv(Buffer, read, {StopBeforeLength=}false) or
     (Length <> read) then
    raise ENetSock.Create('%s.SockRecv(%d) read=%d',
      [ClassNameShort(self)^, Length, read], NetLastError);
end;

function TCrtSocket.SockReceivePending(TimeOutMS: integer): TCrtSocketPending;
var
  events: TNetEvents;
begin
  if SockIsDefined then
    events := fSock.WaitFor(TimeOutMS, [neRead])
  else
    events := [neError];
  if neError in events then
    result := cspSocketError
  else if neRead in events then
    result := cspDataAvailable
  else
    result := cspNoData;
end;

function TCrtSocket.SockReceiveString: RawByteString;
var
  available, resultlen, read: integer;
begin
  result := '';
  if not SockIsDefined then
    exit;
  resultlen := 0;
  repeat
    if fSock.RecvPending(available) <> nrOK then
      exit; // raw socket error
    if available = 0 then // no data in the allowed timeout
      if result = '' then
      begin
        // wait till something
        SleepHiRes(1); // some delay in infinite loop
        continue;
      end
      else
        break; // return what we have
    SetLength(result, resultlen + available); // append to result
    read := available;
    if not TrySockRecv(@PByteArray(result)[resultlen], read,
         {StopBeforeLength=}true) then
    begin
      Close;
      SetLength(result, resultlen);
      exit;
    end;
    inc(resultlen, read);
    if read < available then
      SetLength(result, resultlen); // e.g. Read=0 may happen
    SleepHiRes(0); // 10us on POSIX, SwitchToThread on Windows
  until false;
end;

function TCrtSocket.TrySockRecv(Buffer: pointer; var Length: integer;
  StopBeforeLength: boolean): boolean;
var
  expected, read: integer;
  now, last, diff: Int64;
  res: TNetResult;
begin
  result := false;
  if SockIsDefined and
     (Buffer <> nil) and
     (Length > 0) then
  begin
    expected := Length;
    Length := 0;
    last := {$ifdef OSWINDOWS}mormot.core.os.GetTickCount64{$else}0{$endif};
    repeat
      read := expected - Length;
      if fSecure <> nil then
        res := fSecure.Receive(Buffer, read)
      else
        res := fSock.Recv(Buffer, read);
      if res <> nrOK then
      begin
        // no more to read, or socket issue?
        {$ifdef SYNCRTDEBUGLOW}
        if Assigned(OnLog) then
          OnLog(sllCustom2, 'TrySockRecv: sock=% Recv=% %',
            [fSock.Socket, read, SocketErrorMessage], self);
        {$endif SYNCRTDEBUGLOW}
        if StopBeforeLength and
           (res = nrRetry) then
          break;
        Close; // connection broken or socket closed gracefully
        exit;
      end
      else
      begin
        inc(fBytesIn, read);
        inc(Length, read);
        if StopBeforeLength or
           (Length = expected) then
          break; // good enough for now
        inc(PByte(Buffer), read);
      end;
      now := mormot.core.os.GetTickCount64;
      if (last = 0) or
         (read > 0) then // check timeout from unfinished read
        last := now
      else
      begin
        diff := now - last;
        if diff >= TimeOut then
        begin
          if Assigned(OnLog) then
            OnLog(sllTrace, 'TrySockRecv: timeout (diff=%>%)',
              [diff, TimeOut], self);
          exit; // identify read timeout as error
        end;
        if diff < 100 then
          SleepHiRes(0)
        else
          SleepHiRes(1);
      end;
    until false;
    result := true;
  end;
end;

procedure TCrtSocket.SockRecvLn(out Line: RawUtf8; CROnly: boolean);

  procedure RecvLn(var Line: RawUtf8);
  var
    P: PAnsiChar;
    LP, L: PtrInt;
    tmp: array[0..1023] of AnsiChar; // avoid ReallocMem() every char
  begin
    P := @tmp;
    Line := '';
    repeat
      SockRecv(P, 1); // this is very slow under Windows -> use SockIn^ instead
      if P^ <> #13 then // at least NCSA 1.3 does send a #10 only -> ignore #13
        if P^ = #10 then
        begin
          if Line = '' then // get line
            FastSetString(Line, @tmp, P - tmp)
          else
          begin
            // append to already read chars
            LP := P - tmp;
            L := Length(Line);
            Setlength(Line, L + LP);
            MoveFast(tmp, PByteArray(Line)[L], LP);
          end;
          exit;
        end
        else if P = @tmp[1023] then
        begin
          // tmp[] buffer full? -> append to already read chars
          L := Length(Line);
          Setlength(Line, L + 1024);
          MoveFast(tmp, PByteArray(Line)[L], 1024);
          P := tmp;
        end
        else
          inc(P);
    until false;
  end;

var
  c: byte;
  L, Error: PtrInt;
begin
  if CROnly then
  begin
    // slower but accurate version expecting #13 as line end
    // SockIn^ expect either #10, either #13#10 -> a dedicated version is needed
    repeat
      SockRecv(@c, 1); // this is slow but works
      if c in [0, 13] then
        exit; // end of line
      L := Length({%H-}Line);
      SetLength(Line, L + 1);
      PByteArray(Line)[L] := c;
    until false;
  end
  else if SockIn <> nil then
  begin
    {$I-}
    readln(SockIn^, Line); // example: HTTP/1.0 200 OK
    Error := ioresult;
    if Error <> 0 then
      raise ENetSock.Create('%s.SockRecvLn error %d after %d chars',
        [ClassNameShort(self)^, Error, Length(Line)]);
    {$I+}
  end
  else
    RecvLn(Line); // slow under Windows -> use SockIn^ instead
end;

procedure TCrtSocket.SockRecvLn;
var c: AnsiChar;
  Error: integer;
begin
  if SockIn <> nil then
  begin
    {$I-}
    readln(SockIn^);
    Error := ioresult;
    if Error <> 0 then
      raise ENetSock.Create('%s.SockRecvLn error %d',
        [ClassNameShort(self)^, Error]);
    {$I+}
  end
  else
    repeat
      SockRecv(@c, 1);
    until c = #10;
end;

procedure TCrtSocket.SndLow(P: pointer; Len: integer);
begin
  if not TrySndLow(P, Len) then
    raise ENetSock.Create('%s.SndLow(%s) len=%d',
      [ClassNameShort(self)^, fServer, Len], NetLastError);
end;

procedure TCrtSocket.SndLow(const Data: RawByteString);
begin
  SndLow(pointer(Data), Length(Data));
end;

function TCrtSocket.TrySndLow(P: pointer; Len: integer): boolean;
var
  sent: integer;
  now, start: Int64;
  res: TNetResult;
begin
  result := Len = 0;
  if not SockIsDefined or
     (Len <= 0) or
     (P = nil) then
    exit;
  start := {$ifdef OSWINDOWS}mormot.core.os.GetTickCount64{$else}0{$endif};
  repeat
    sent := Len;
    if fSecure <> nil then
      res := fSecure.Send(P, sent)
    else
      res := fSock.Send(P, sent);
    if sent > 0 then
    begin
      inc(fBytesOut, sent);
      dec(Len, sent);
      if Len <= 0 then
        break; // data successfully sent
      inc(PByte(P), sent);
    end
    else if (res <> nrOK) and
            (res <> nrRetry) then
      exit; // fatal socket error
    now := mormot.core.os.GetTickCount64;
    if (start = 0) or
       (sent > 0) then
      start := now
    else // measure timeout since nothing written
      if now - start > TimeOut then
        exit; // identify timeout as error
    SleepHiRes(1);
  until false;
  result := true;
end;

function TCrtSocket.LastLowSocketError: integer;
begin
  result := sockerrno;
end;

function TCrtSocket.AcceptIncoming(ResultClass: TCrtSocketClass): TCrtSocket;
var
  client: TNetSocket;
  addr: TNetAddr;
begin
  result := nil;
  if not SockIsDefined then
    exit;
  if fSock.Accept(client, addr) <> nrOK then
    exit;
  if ResultClass = nil then
    ResultClass := TCrtSocket;
  result := ResultClass.Create(Timeout);
  result.AcceptRequest(client, @addr);
  result.CreateSockIn; // use SockIn with 1KB input buffer: 2x faster
end;

function TCrtSocket.PeerAddress(LocalAsVoid: boolean): RawByteString;
begin
  if fPeerAddr = nil then
    result := ''
  else
    result := fPeerAddr^.IP(LocalAsVoid);
end;

function TCrtSocket.PeerPort: TNetPort;
begin
  if fPeerAddr = nil then
    result := 0
  else
    result := fPeerAddr^.Port;
end;

function SocketOpen(const aServer, aPort: RawUtf8; aTLS: boolean;
  aTLSContext: PNetTlsContext; aTunnel: PUri): TCrtSocket;
begin
  try
    result := TCrtSocket.Open(
      aServer, aPort, nlTcp, 10000, aTLS, aTLSContext, aTunnel);
  except
    result := nil;
  end;
end;


initialization
  IP4local := cLocalhost; // use var string with refcount=1 to avoid allocation
  assert(SizeOf(in_addr) = 4);
  assert(SizeOf(in6_addr) = 16);
  assert(SizeOf(sockaddr_in) = 16);
  assert(SizeOf(TNetAddr) = SOCKADDR_SIZE);
  assert(SizeOf(TNetAddr) >=
    {$ifdef OSWINDOWS} SizeOf(sockaddr_in6) {$else} SizeOf(sockaddr_un) {$endif});
  DefaultListenBacklog := SOMAXCONN;
  InitializeUnit; // in mormot.net.sock.windows/posix.inc

finalization
  FinalizeUnit;
  
end.

