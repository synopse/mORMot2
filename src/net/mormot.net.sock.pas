/// low-level access to the OperatingSystem Sockets API (e.g. WinSock2)
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.sock;

{
  *****************************************************************************

   Cross-Platform Raw Sockets API Definition
   - Socket Process High-Level Encapsulation
   - MAC and IP Addresses Support
   - TLS / HTTPS Encryption Abstract Layer
   - TSocketStream Socket Wrapper
   - Efficient Multiple Sockets Polling
   - Windows IOCP sockets support
   - TUri parsing/generating URL wrapper
   - TCrtSocket Buffered Socket Read/Write Class
   - NTP / SNTP Protocol Client

   The Low-Level Sockets API, which is complex and inconsistent among OS, is
   not made public and shouldn't be used in end-user code. This unit
   encapsultates all Sockets features into a single set of functions, and
   around the TNetSocket abstract wrapper.

  *****************************************************************************

  Notes:
    OS-specific code is located in mormot.net.sock.windows/posix.inc files.
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
  cLocalhost  = '127.0.0.1';
  cAnyHost    = '0.0.0.0';
  cBroadcast  = '255.255.255.255';
  c6Localhost = '::1';
  c6AnyHost   = '::';
  c6Broadcast = 'ffff::1';
  cAnyPort    = '0';

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
    nrTimeout,
    nrInvalidParameter);
  /// a pointer to a TNetSocket error
  PNetResult = ^TNetResult;

  /// exception class raised by this unit
  ENetSock = class(ExceptionWithProps)
  protected
    fLastError: TNetResult;
  public
    /// reintroduced constructor with TNetResult information
    constructor Create(msg: string; obj: TObject; const args: array of const;
      error: TNetResult = nrOK; errnumber: system.PInteger = nil); reintroduce;
    /// reintroduced constructor with NetLastError call
    constructor CreateLastError(const msg: string; const args: array of const;
      error: TNetResult = nrOk);
    /// raise ENetSock if res is not nrOK or nrRetry
    class procedure Check(res: TNetResult; const context: ShortString;
      errnumber: system.PInteger = nil);
    /// call NetLastError and raise ENetSock if not nrOK nor nrRetry
    class procedure CheckLastError(const Context: ShortString;
      ForceRaise: boolean = false; AnotherNonFatal: integer = 0);
  published
    property LastError: TNetResult
      read fLastError default nrOk;
  end;
  /// meta-class of ENetSock type definition
  ENetSockClass = class of ENetSock;

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
  NO_ERROR = 0;

  /// the socket protocol layers over the IP protocol
  nlIP = [nlTcp, nlUdp];

type
  /// end-user code should use this TNetSocket type to hold a socket handle
  // - then its methods will allow cross-platform access to the connection
  TNetSocket = ^TNetSocketWrap;

  /// pointer reference to a cross-platformsocket handle
  PNetSocket = ^TNetSocket;

  /// dynamic array of socket handles
  TNetSocketDynArray = array of TNetSocket;

  /// pointer reference to a dynamic array of socket handles
  // - used e.g. as optional parameter to GetReachableNetAddr()
  PNetSocketDynArray = ^TNetSocketDynArray;

  /// internal mapping of an address, in any supported socket layer
  {$ifdef USERECORDWITHMETHODS}
  TNetAddr = record
  {$else}
  TNetAddr = object
  {$endif USERECORDWITHMETHODS}
  private
    // opaque wrapper with len: sockaddr_un=110 (POSIX) or sockaddr_in6=28 (Win)
    Addr: array[0..SOCKADDR_SIZE - 1] of byte;
  public
    /// fill the meaningful bytes of the internal data structure with zeros
    procedure Clear;
      {$ifdef HASINLINE}inline;{$endif}
    /// initialize this address from standard IPv4/IPv6 or nlUnix textual value
    // - calls NewSocketIP4Lookup if available from mormot.net.dns (with a 32
    // seconds cache) or the proper getaddrinfo/gethostbyname OS API
    // - see also NewSocket() overload or GetSocketAddressFromCache() if you
    // want to use the global NewSocketAddressCache
    function SetFrom(const address, addrport: RawUtf8; layer: TNetLayer): TNetResult;
    /// internal host resolution from IPv4, known hosts, NetAddrCache or
    // NewSocketIP4Lookup (mormot.net.dns)
    // - as called by SetFrom() high-level method
    function SetFromIP4(const address: RawUtf8; noNewSocketIP4Lookup: boolean): boolean;
    /// initialize this address from a standard IPv4
    // - set a given 32-bit IPv4 address and its network port (0..65535)
    function SetIP4Port(ipv4: cardinal; netport: TNetPort): TNetResult;
    /// returns the network family of this address
    function Family: TNetFamily;
    /// compare two IPv4/IPv6  network addresses
    // - only compare the IP part of the address, not the port, nor any nlUnix
    function IPEqual(const another: TNetAddr): boolean;
      {$ifdef FPC}inline;{$endif}
    /// convert this address into its IPv4/IPv6 textual representation
    procedure IP(var res: RawUtf8; localasvoid: boolean = false); overload;
    /// convert this address into its IPv4/IPv6 textual representation
    function IP(localasvoid: boolean = false): RawUtf8; overload;
      {$ifdef HASSAFEINLINE}inline;{$endif}
    /// convert this address into its 32-bit IPv4 value, 0 on IPv6/nlUnix
    // - may return cLocalhost32 for 127.0.0.1
    // - returns 0 (i.e. 0.0.0.0) for AF_INET6 or AF_UNIX
    function IP4: cardinal;
      {$ifdef FPC}inline;{$endif}
    /// convert an IPv4 value into text, or '' for AF_INET6 or AF_UNIX
    function IP4Short: TShort16;
      {$ifdef FPC} inline; {$endif}
    /// convert this address into its shortstring IPv4/IPv6 textual representation
    function IPShort(withport: boolean = false): ShortString; overload;
      {$ifdef HASINLINE}inline;{$endif}
      /// convert this address into its shortstring IPv4/IPv6 textual representation
    procedure IPShort(out result: ShortString; withport: boolean = false); overload;
    /// convert this address into its 'IPv4/IPv6:port' textual representation
    function IPWithPort: RawUtf8; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// convert this address into its 'IPv4/IPv6:port' textual representation
    procedure IPWithPort(var Text: RawUtf8); overload;
    /// returns the network port (0..65535) of this address
    function Port: TNetPort;
      {$ifdef FPC}inline;{$endif}
    /// set the network port (0..65535) of this address
    function SetPort(p: TNetPort): TNetResult;
    /// compute the number of bytes actually used in this address buffer
    function Size: integer;
      {$ifdef FPC}inline;{$endif}
    /// create a new TNetSocket instance on this network address
    // - returns nil on API error
    // - SetFrom() should have been called before running this method
    function NewSocket(layer: TNetLayer): TNetSocket;
    /// connect a TNetSocket instance to this network address
    // - by default, blocking connect() timeout is not customizable: this method
    // will call MakeAsync/MakeBlocking and wait for the actual connection
    // - as called by NewSocket() high-level wrapper function
    // - you can specify ms<0 to make an asynchronous connect() on this address
    // without waiting yet
    function SocketConnect(socket: TNetSocket; ms: integer): TNetResult;
    /// bind a TNetSocket instance to this network address
    function SocketBind(socket: TNetSocket): TNetResult;
  end;

  /// pointer reference to a socket address mapping
  PNetAddr = ^TNetAddr;

  /// dynamic array of socket addresses
  TNetAddrDynArray = array of TNetAddr;

  PTerminated = ^boolean; // on FPC system.PBoolean doesn't exist :(

  /// convenient object-oriented wrapper around a socket connection
  // - encapsulate a cross-platform low-level access to the socket API
  // - TNetSocket is a pointer to this, so TSocket(@self) is used for OS calls
  {$ifdef USERECORDWITHMETHODS}
  TNetSocketWrap = record
  {$else}
  TNetSocketWrap = object
  {$endif USERECORDWITHMETHODS}
  private
    procedure SetOpt(prot, name: integer; value: pointer; valuelen: integer);
    function GetOptInt(prot, name: integer): integer;
    function SetIoMode(async: cardinal): TNetResult;
    procedure SetSendBufferSize(bytes: integer);
    procedure SetRecvBufferSize(bytes: integer);
    function GetSendBufferSize: integer;
    function GetRecvBufferSize: integer;
  public
    /// called by NewSocket to finalize a socket attributes
    procedure SetupConnection(layer: TNetLayer; sendtimeout, recvtimeout: integer);
    /// change the sending timeout of this socket, in milliseconds
    procedure SetSendTimeout(ms: integer);
    /// change the receiving timeout of this socket, in milliseconds
    procedure SetReceiveTimeout(ms: integer);
    /// change if this socket should enable TCP level keep-alive packets
    procedure SetKeepAlive(keepalive: boolean);
    /// change the SO_LINGER option, i.e. let the socket remain open for a while
    // - on POSIX, will also set the SO_REUSEADDR/SO_REUSEPORT option
    procedure SetLinger(linger: integer);
    /// allow to disable the Nagle's algorithm and send packets without delay
    procedure SetNoDelay(nodelay: boolean);
    /// set the TCP_CORK (Linux) or TCP_NOPUSH (BSD) option
    procedure SetCork(cork: boolean);
    /// set the SO_BROADCAST option for UDP
    procedure SetBroadcast(broadcast: boolean);
    /// set the SO_REUSEADDR/SO_REUSEPORT option for UDP
    // - this method is already called by SetLinger(true) for TCP on POSIX
    // - do nothing on Windows, since SO_REUSEADDR does something else than
    // on Linux, and is set by SetReuseAddrPort
    procedure SetReuseAddrPort;
    /// set low SIO_SET_PRIORITY_HINT (Windows 10+) or SO_PRIORITY (Linux)
    // - on Windows, try to use LEDBAT algorithm - to be set before accept/connect
    procedure SetLowPriority;
    /// check if SetLowPriority was successful on the connection
    // - on Windows, to be called after accept/connect to see e.g. if LEDBAT is
    // used on the connection (false for old Windows, or e.g. if TCP timestamps
    // are disabled on the other side)
    // - on POSIX, always return false
    function HasLowPriority: boolean;
    /// set the SO_REUSEPORT option, to allow several servers to bind on a port
    // - calls SetReuseAddrPort on Windows
    procedure ReusePort;
    /// accept an incoming socket, optionally asynchronous
    // - async=true will force clientsocket to be defined as asynchronous;
    // supporting accept4() syscall on Linux
    function Accept(out clientsocket: TNetSocket; out addr: TNetAddr;
      async: boolean): TNetResult;
    /// retrieve the current address associated on this connected socket
    function GetName(out addr: TNetAddr): TNetResult;
    /// retrieve this connected socket address as 'ip[:port]' text
    function GetIP(out ip: RawUtf8; withport: boolean = true): TNetResult;
    /// retrieve the peer address associated on this connected socket
    function GetPeer(out addr: TNetAddr): TNetResult;
    /// change the socket state to non-blocking
    // - note that on Windows, there is no easy way to check the non-blocking
    // state of the socket (WSAIoctl has been deprecated for this)
    function MakeAsync: TNetResult;
    /// change the socket state to blocking
    function MakeBlocking: TNetResult;
    /// low-level sending of some data via this socket
    function Send(Buf: pointer; var len: integer;
      rawError: system.PInteger = nil): TNetResult;
    /// low-level receiving of some data from this socket
    function Recv(Buf: pointer; var len: integer;
      rawError: system.PInteger = nil): TNetResult;
    /// low-level UDP sending to an address of some data
    function SendTo(Buf: pointer; len: integer; const addr: TNetAddr): TNetResult;
    /// low-level UDP receiving from an address of some data
    function RecvFrom(Buf: pointer; len: integer; out addr: TNetAddr): integer;
    /// wait for the socket to a given set of receiving/sending state
    // - using poll() on POSIX (as required), and select() on Windows
    // - ms < 0 means an infinite timeout (blocking until events happen)
    function WaitFor(ms: integer; scope: TNetEvents;
      loerr: system.PInteger = nil): TNetEvents;
    /// retrieve how many bytes are actually pending in the receiving queue
    function RecvPending(out pending: integer): TNetResult;
    /// return how many pending bytes are in the receiving queue
    // - returns 0 if no data is available, or if the connection is broken: call
    // RecvPending() to check for the actual state of the connection
    function HasData: integer;
    /// wrapper around WaitFor / RecvPending / Recv methods for a given time
    // - will return up to 64KB of pending data in the socket receiving queue
    function RecvWait(ms: integer; out data: RawByteString;
      terminated: PTerminated = nil): TNetResult;
    /// low-level receiving of some data of known length from this socket
    function RecvAll(ms: integer; Buf: PByte; len: integer;
      terminated: PTerminated = nil): TNetResult;
    /// call send in loop until the whole data buffer is sent
    function SendAll(Buf: PByte; len: integer;
      terminated: PTerminated = nil): TNetResult;
    /// check if the socket is not closed nor broken
    // - i.e. check if it is likely to be accept Send() and Recv() calls
    // - calls WaitFor(neRead) then Recv() to check e.g. WSACONNRESET on Windows
    function Available(loerr: system.PInteger = nil): boolean;
    /// call shutdown() on this socket - may be used to simulate a disconnection
    procedure RawShutdown;
    /// finalize a socket, calling Close after shutdown() if needed
    function ShutdownAndClose(rdwr: boolean; waitms: integer = 0): TNetResult;
    /// close the socket - consider ShutdownAndClose() for clean closing
    function Close: TNetResult;
    /// access to the raw socket handle, i.e. @self
    function Socket: PtrInt;
      {$ifdef HASSAFEINLINE}inline;{$endif}
    /// change the OS sending buffer size of this socket, in bytes
    // - do not use on Windows, because those values are indicative only and
    // are not working as documented by the standard for SO_SNDBUF
    property SendBufferSize: integer
      read GetSendBufferSize write SetSendBufferSize;
    /// change the OS receiving buffer size of this socket, in bytes
    // - do not use on Windows, because those values are indicative only and
    // are not working as documented by the standard for SO_RCVBUF
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
    // - is likely to flush the cache
    procedure SetTimeOut(aSeconds: integer);
    /// you can force a customized IP resolution for this host name
    procedure Force(const Host, IP: RawUtf8);
  end;


/// internal very low-level function retrieving the latest socket OS error code
function RawSocketErrNo: integer; {$ifdef OSWINDOWS} stdcall; {$endif}

/// internal low-level function retrieving the latest socket error information
function NetLastError(AnotherNonFatal: integer = NO_ERROR;
  Error: system.PInteger = nil): TNetResult;

/// internal low-level function retrieving the latest socket error message
function NetLastErrorMsg(AnotherNonFatal: integer = NO_ERROR): ShortString;

/// internal low-level function using known operating system error
function NetErrorFromSystem(SystemError, AnotherNonFatal: integer): TNetResult;

/// create a new Socket connected or bound to a given ip:port
function NewSocket(const address, port: RawUtf8; layer: TNetLayer;
  dobind: boolean; connecttimeout, sendtimeout, recvtimeout, retry: integer;
  out netsocket: TNetSocket; netaddr: PNetAddr = nil;
  bindReusePort: boolean = false): TNetResult;

/// create a new raw TNetSocket instance
// - returns nil on error
function NewRawSocket(family: TNetFamily; layer: TNetLayer): TNetSocket;

/// create several new raw TNetSocket instances
// - raise ENetSock on error
function NewRawSockets(family: TNetFamily; layer: TNetLayer;
  count: integer): TNetSocketDynArray;

/// delete a hostname from TNetAddr.SetFrom internal short-living cache
procedure NetAddrFlush(const hostname: RawUtf8);

/// return the IP (v4 or v6) address of a given hostname
// - just a wrapper around TNetAddr.SetFrom and TNetAddr.IP
function NetAddrResolve(const hostname: RawUtf8): RawUtf8;

/// resolve the TNetAddr of the address:port layer - maybe from NewSocketAddressCache
function GetSocketAddressFromCache(const address, port: RawUtf8;
  layer: TNetLayer; out addr: TNetAddr; var fromcache, tobecached: boolean): TNetResult;

/// check if an address is known from the current NewSocketAddressCache
// - calls GetSocketAddressFromCache() so would use the internal cache, if any
function ExistSocketAddressFromCache(const host: RawUtf8): boolean;

/// try to connect to several address:port servers simultaneously
// - return up to neededcount connected TNetAddr, until timeoutms expires
// - sockets are closed unless sockets^[] should contain the result[] sockets
function GetReachableNetAddr(const address, port: array of RawUtf8;
  timeoutms: integer = 1000; neededcount: integer = 1;
  sockets: PNetSocketDynArray = nil): TNetAddrDynArray;

var
  /// contains the raw Socket API version, as returned by the Operating System
  // - equals e.g. 'Debian Linux 6.1.0 epoll'
  SocketApiVersion: RawUtf8;

  /// callback used by NewSocket() to resolve the host name as IPv4
  // - not assigned by default, to use the OS default API, i.e. getaddrinfo()
  // on Windows, and gethostbyname() on POSIX
  // - if you include mormot.net.dns, its own IPv4 DNS resolution function will
  // be registered here
  // - this level of DNS resolution has a simple in-memory cache of 32 seconds
  // - NewSocketAddressCache from mormot.net.client will implement a more
  // tunable cache, for both IPv4 and IPv6 resolutions
  NewSocketIP4Lookup: function(const HostName: RawUtf8; out IP4: cardinal): boolean;

  /// the DNS resolver address(es) to be used by NewSocketIP4Lookup() callback
  // - default '' would call GetDnsAddresses to ask the server known by the OS
  // - you can specify an alternate CSV list of DNS servers to be called in order
  NewSocketIP4LookupServer: RawUtf8;

  /// interface used by NewSocket() to cache the host names
  // - avoiding DNS resolution is a always a good idea
  // - if you include mormot.net.client, will register its own implementation
  // class using a TSynDictionary over a 10 minutes default timeout
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
  // - with default true, loopback connection will have no RemoteIP address ('')
  // - or it will be explicitly '127.0.0.1' - if equals false
  // - used by both TCrtSock.AcceptRequest and THttpApiServer.Execute servers
  RemoteIPLocalHostAsVoidInServers: boolean = true;


const
  // don't use RTTI to avoid mormot.core.rtti.pas and have better spelling
  _NR: array[TNetResult] of TShort31 = (
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
    'Connect Timeout',
    'Invalid Parameter');

/// returns the plain English text of a network result
// - e.g. ToText(nrNotFound)='Not Found' as defined in _NR[] global constant
function ToText(res: TNetResult): PShortString; overload;
  {$ifdef HASINLINE} inline; {$endif}

/// convert a WaitFor() result set into a regular TNetResult enumerate
function NetEventsToNetResult(ev: TNetEvents): TNetResult;


{ ******************** Mac and IP Addresses Support }

type
  /// the filter used by GetIPAddresses() and IP4Filter()
  // - the "Public"/"Private" suffix maps IsPublicIP() IANA ranges of IPv4
  // address space, i.e. 10.x.x.x, 172.16-31.x.x and 192.168.x.x addresses
  // - the "Dhcp" suffix excludes IsApipaIP() 169.254.0.1 - 169.254.254.255
  // range, i.e. ensure the address actually came from a real DHCP server
  // - tiaAny always return true, for any IPv4 or IPv6 address
  // - tiaIPv4 identify any IPv4 address
  // - tiaIPv6 identify any IPv6 address
  // - tiaIPv4Public identify any IPv4 public address
  // - tiaIPv4Private identify any IPv4 private address
  // - tiaIPv4Dhcp identify any IPv4 address excluding APIPA range
  // - tiaIPv4DhcpPublic identify any IPv4 public address excluding APIPA range
  // - tiaIPv4DhcpPrivate identify any IPv4 private address excluding APIPA range
  TIPAddress = (
    tiaAny,
    tiaIPv4,
    tiaIPv6,
    tiaIPv4Public,
    tiaIPv4Private,
    tiaIPv4Dhcp,
    tiaIPv4DhcpPublic,
    tiaIPv4DhcpPrivate);

/// detect IANA private IPv4 address space from its 32-bit raw value
// - i.e. 10.x.x.x, 172.16-31.x.x and 192.168.x.x addresses
function IsPublicIP(ip4: cardinal): boolean;

/// detect APIPA private IPv4 address space from its 32-bit raw value
// - Automatic Private IP Addressing (APIPA) is used by Windows clients to
// setup some IP in case of local DHCP failure
// - it covers the 169.254.0.1 - 169.254.254.255 range
// - see tiaIPv4Dhcp, tiaIPv4DhcpPublic and tiaIPv4DhcpPrivate filters
function IsApipaIP(ip4: cardinal): boolean;

/// detect IANA private IPv4 masks as 32-bit raw values
// - i.e. 10.x.x.x, 172.16-31.x.x and 192.168.x.x addresses into
// 255.0.0.0, 255.255.0.0, 255.255.255.0 or 255.255.255.255
function IP4Mask(ip4: cardinal): cardinal;

/// compute a broadcast address from a IPv4 current address and its known mask
// - e.g. ip4=172.16.144.160 and mask4=255.255.255.0 returns 172.16.144.255
function IP4Broadcast(ip4, mask4: cardinal): cardinal;

/// compute the prefix size of a IPv4 prefix from a 32-bit network mask
// - e.g. returns 8 for 255.0.0.0
function IP4Prefix(netmask4: cardinal): integer; overload;

/// compute the prefix size of a IPv4 prefix from a text network mask
// - e.g. IP4Prefix('255.255.255.0') returns 24
function IP4Prefix(const netmask4: RawUtf8): integer; overload;

/// reverse conversion of IP4Prefix() into a 32-bit network mask
// - e.g. IP4Netmask(24) returns 255.255.255.0
function IP4Netmask(prefix: integer): cardinal; overload;

/// reverse conversion of IP4Prefix() into a 32-bit network mask
function IP4Netmask(prefix: integer; out mask: cardinal): boolean; overload;
  {$ifdef HASINLINE} inline; {$endif}

/// compute a subnet/CIDR value from a 32-bit IPv4 and its associated NetMask
// - e.g. ip4=192.168.0.16 and mask4=255.255.255.0 returns '192.168.0.0/24'
function IP4Subnet(ip4, netmask4: cardinal): TShort23; overload;

/// compute a subnet/CIDR value from an IPv4 and its associated NetMask
// - e.g. ip4='192.168.0.16' and mask4='255.255.255.0' returns '192.168.0.0/24'
function IP4Subnet(const ip4, netmask4: RawUtf8): RawUtf8; overload;

/// check if an IPv4 text match a CIDR sub-network
// - e.g. IP4Match('192.168.1.1', '192.168.1.0/24') = true
function IP4Match(const ip4, subnet: RawUtf8): boolean;

/// filter an IPv4 address to a given TIPAddress kind
// - return true if the supplied address does match the filter
// - by design, both 0.0.0.0 and 127.0.0.1 always return false
function IP4Filter(ip4: cardinal; filter: TIPAddress): boolean;

/// convert an IPv4 raw value into a ShortString text
// - won't use the Operating System network layer API so works on XP too
// - zero is returned as '0.0.0.0' and loopback as '127.0.0.1'
procedure IP4Short(ip4addr: PByteArray; var s: TShort16);

/// convert an IPv4 raw value into a ShortString text
function IP4ToShort(ip4addr: PByteArray): TShort16;
  {$ifdef HASINLINE} inline; {$endif}

/// convert an IPv4 raw value into a RawUtf8 text
// - zero 0.0.0.0 address  (i.e. bound to any host) is returned as ''
procedure IP4Text(ip4addr: PByteArray; var result: RawUtf8);

/// convert an IPv4 raw value into a RawUtf8 text
function IP4ToText(ip4addr: PByteArray): RawUtf8;
  {$ifdef HASINLINE} inline; {$endif}

/// convert an IPv6 raw value into a ShortString text
// - will shorten the address using the regular 0 removal scheme, e.g.
// 2001:00b8:0a0b:12f0:0000:0000:0000:0001 returns '2001:b8:a0b:12f0::1'
// - zero is returned as '::' and loopback as '::1'
// - does not support mapped IPv4 so never returns '::1.2.3.4' but '::102:304'
// - won't use the Operating System network layer API so is fast and consistent
procedure IP6Short(ip6addr: PByteArray; var s: ShortString);

/// convert an IPv6 raw value into a RawUtf8 text
// - zero '::' address  (i.e. bound to any host) is returned as ''
// - loopback address is returned as its '127.0.0.1' IPv4 representation
// for consistency with our high-level HTTP/REST code
// - does not support mapped IPv4 so never returns '::1.2.3.4' but '::102:304'
procedure IP6Text(ip6addr: PByteArray; var result: RawUtf8);

/// convert a MAC address value into its standard RawUtf8 text representation
// - calls ToHumanHex(mac, 6), returning e.g. '12:50:b6:1e:c6:aa'
function MacToText(mac: PByteArray): RawUtf8;
  {$ifdef HASINLINE} inline; {$endif}

/// convert a MAC address value from its standard hexadecimal text representation
// - returns e.g. '12:50:b6:1e:c6:aa' from '1250b61ec6aa' or '1250B61EC6AA'
function MacTextFromHex(const Hex: RawUtf8): RawUtf8;

/// convert a MAC address value into a RawUtf8 hexadecimal text with no ':'
// - returns e.g. '1250b61ec6aa'
function MacToHex(mac: PByteArray; maclen: PtrInt = 6): RawUtf8;

/// enumerate all IP addresses of the current computer
// - may be used to enumerate all adapters
// - no cache is used for this function - consider GetIPAddressesText instead
// - by design, 127.0.0.1 is excluded from the list
function GetIPAddresses(Kind: TIPAddress = tiaIPv4): TRawUtf8DynArray;

/// returns all IP addresses of the current computer as a single CSV text
// - may be used to enumerate all adapters
// - an internal cache of the result is refreshed every 32 seconds
function GetIPAddressesText(const Sep: RawUtf8 = ' ';
  Kind: TIPAddress = tiaIPv4): RawUtf8;

type
  /// the network interface type, as stored in TMacAddress.Kind
  // - we don't define all ARP models, but try to detect most basic types
  TMacAddressKind = (
    makUndefined,
    makEthernet,
    makWifi,
    makTunnel,
    makPpp,
    makCellular,
    makSoftware);
  /// a set of network interface types
  TMacAddressKinds = set of TMacAddressKind;

  /// interface name/address pairs as returned by GetMacAddresses
  // - associated IPv4 information is available on most systems
  TMacAddress = record
    /// short text description of this interface
    // - contains e.g. 'eth0' on Linux
    Name: RawUtf8;
    /// user-friendly name for the adapter
    // - e.g. on Windows: 'Local Area Connection 1.'
    // - on Linux, returns /sys/class/net/eth0/ifalias content, i.e. the value
    // fixed by "ip link set eth0 alias somename"
    // - not available on Android or BSD
    FriendlyName: RawUtf8;
    ///  name of the adapter with which these addresses are associated
    // - unlike FriendlyName, it can't be renamed by the end user
    // - e.g. on Windows: '{1C7CAE9E-3256-4784-8CA4-B721D3B5A00F}'
    // - equals Name on POSIX
    AdapterName: RawUtf8;
    /// the hardware MAC address of this adapter
    // - contains e.g. '12:50:b6:1e:c6:aa' from /sys/class/net/eth0/adddress
    // - may equal '00:00:00:00:00:00' for a non-physical interface (makSoftware)
    Address: RawUtf8;
    /// the raw IPv4 address of this interface
    // - not available on Android
    IP: RawUtf8;
    /// the raw IPv4 network mask of this interface
    // - not available on Android
    NetMask: RawUtf8;
    /// the raw IPv4 broadcast address of this interface
    Broadcast: RawUtf8;
    /// the raw IPv4 gateway address of this interface
    // - not available on Windows XP or BSD
    Gateway: RawUtf8;
    {$ifdef OSWINDOWS}
    /// the raw IPv4 address(es) of the associated DNS server(s), as CSV
    // - not available on POSIX (DNS are part of the routing, not interfaces)
    Dns: RawUtf8;
    /// the optional DNS suffix of this connection, e.g. 'ad.mycorp.com'
    // - not available on POSIX (DNS are part of the routing, not interfaces)
    DnsSuffix: RawUtf8;
    /// the raw IPv4 binary address of the main associated DHCP server
    // - not available on Windows XP or POSIX
    Dhcp: RawUtf8;
    {$endif OSWINDOWS}
    /// the current adapter Maximum Transmission Unit size (MTU), in bytes
    // - typically 1500 over an Ethernet network
    // - not available on BSD
    Mtu: cardinal;
    /// the current link speed in Mbits per second (typically 100 or 1000)
    // - not available on Windows XP or BSD
    // - some interfaces (e.g. makWifi on Linux) may have a 0 value
    Speed: cardinal;
    /// the interface index, as internally used by the OS
    // - may equal -1 for a non-physical interface (makSoftware)
    IfIndex: integer;
    /// the hardware model of this network interface
    // - retrieved from ARP protocol hardware identifiers on Linux, and
    // IfType field on Windows (seems accurate since Vista)
    // - not available on BSD
    Kind: TMacAddressKind;
  end;
  PMacAddress = ^TMacAddress;
  TMacAddressDynArray = array of TMacAddress;

const
  /// identify each TMacAddressKind as one uppercase letter
  MAK_TXT: array[TMacAddressKind] of AnsiChar = '?EWTPCS';

/// enumerate all network MAC addresses and their associated IP information
// - an internal 65-seconds cache is used, with explicit MacIPAddressFlush
function GetMacAddresses(UpAndDown: boolean = false): TMacAddressDynArray;

/// enumerate all MAC addresses of the current computer as 'name1=addr1 name2=addr2'
// - an internal 65-seconds cache is used, with explicit MacIPAddressFlush
function GetMacAddressesText(WithoutName: boolean = true;
  UpAndDown: boolean = false): RawUtf8;

/// flush the GetIPAddressesText/GetMacAddresses internal caches
// - may be called to force detection after HW configuration change (e.g. when
// wifi has been turned on)
// - this method is thread-safe about its internal caches
procedure MacIPAddressFlush;

{$ifdef OSWINDOWS}
/// remotely get the MAC address of a computer, from its IP Address
// - only works under Windows, which features a SendARP() API in user space:
// on POSIX, implementing ARP sadly requires root rights
// - return the MAC address as a 12 hexa chars ('0050C204C80A' e.g.)
function GetRemoteMacAddress(const IP: RawUtf8): RawUtf8;
{$endif OSWINDOWS}

/// get the local MAC address used to reach a computer, from its IP or Host name
// - return the local interface as a TMacAddress, with all its available info
// - under Windows, will call the GetBestInterface() API to retrieve a IfIndex
// - on POSIX, will call GetLocalIpAddress() to retrieve a local IP
// - always eventually makes a lookup to the GetMacAddresses() list per IfIndex
// (Windows) or IP (POSIX)
function GetLocalMacAddress(const Remote: RawUtf8; var Mac: TMacAddress): boolean;

/// get the local IP address used to reach a computer, from its IP Address
// - will create a SOCK_DGRAM socket over the supplied IP, and check
// the local socket address created
function GetLocalIpAddress(const Remote: RawUtf8): RawUtf8;

/// retrieve all DNS (Domain Name Servers) addresses known by the Operating System
// - on POSIX, return "nameserver" from /etc/resolv.conf unless usePosixEnv is set
// - on Windows, calls GetNetworkParams API from iphlpapi
// - an internal cache of the result will be refreshed every 8 seconds
function GetDnsAddresses(usePosixEnv: boolean = false): TRawUtf8DynArray;

/// append a custom resolver address for GetDnsAddresses() in addition to the OS
procedure RegisterDnsAddress(const DnsResolver: RawUtf8);


var
  /// if manually set, GetDomainNames() will return this value
  // - e.g. 'ad.mycompany.com'
  ForcedDomainName: RawUtf8;

/// retrieve the AD Domain Name addresses known by the Operating System
// - on POSIX, return all "search" from /etc/resolv.conf unless usePosixEnv is set
// - on Windows, calls GetNetworkParams API from iphlpapi to retrieve a single item
// - no cache is used for this function
// - you can force for a given value using ForcedDomainName, e.g. if the
// machine is not actually registered for / part of the domain, but has access
// to the domain controller
function GetDomainNames(usePosixEnv: boolean = false): TRawUtf8DynArray;

/// resolve a host name from the OS hosts file content
// - i.e. use a cache of /etc/hosts or c:\windows\system32\drivers\etc\hosts
// - returns true and the IPv4 address of the stored host found
// - if the file is modified on disk, the internal cache will be flushed
function GetKnownHost(const HostName: RawUtf8; out ip4: cardinal): boolean;

/// append a custom host/ipv4 pair in addition to the OS hosts file
// - to be appended to GetKnownHost() internal cache
procedure RegisterKnownHost(const HostName, Ip4: RawUtf8);


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
  // - TLS is an opaque structure, typically an OpenSSL PSSL pointer, so you
  // could use e.g. PSSL(TLS).PeerCertificate or PSSL(TLS).PeerCertificates array
  TOnNetTlsPeerValidate = procedure(Socket: TNetSocket;
    Context: PNetTlsContext; TLS: pointer) of object;

  /// callback raised by INetTls.AfterConnection after validating a peer
  // - called after standard peer validation - ignored by TOnNetTlsPeerValidate
  // - Context.CipherName, LastError PeerIssuer and PeerSubject are set
  // - TLS and Peer are opaque structures, typically OpenSSL PSSL and PX509
  TOnNetTlsAfterPeerValidate = procedure(Socket: TNetSocket;
    Context: PNetTlsContext; TLS, Peer: pointer) of object;

  /// callback raised by INetTls.AfterConnection for each peer verification
  // - wasok=true if the TLS library did validate the incoming certificate
  // - should process the supplied peer information, and return true to continue
  // and accept the connection, or false to abort the connection
  // - Context.PeerIssuer, PeerSubject and PeerCert have been properly populated
  // - TLS and Peer are opaque structures, typically OpenSSL PSSL and PX509 pointers
  TOnNetTlsEachPeerVerify = function(Socket: TNetSocket; Context: PNetTlsContext;
    wasok: boolean; TLS, Peer: pointer): boolean of object;

  /// callback raised by INetTls.AfterAccept for SNI poer-host resolution
  // - should check the ServerName and return the proper certificate context,
  // typically one OpenSSL PSSL_CTX instance
  // - if the ServerName has no match, and the default certificate is good
  // enough, should return nil
  // - on any error, should raise an exception
  // - TLS is an opaque structure, typically OpenSSL PSSL
  TOnNetTlsAcceptServerName = function(Context: PNetTlsContext; TLS: pointer;
    ServerName: PUtf8Char): pointer of object;

  /// TLS Options and Information for a given TCrtSocket/INetTls connection
  // - currently only properly implemented by mormot.lib.openssl11 - SChannel
  // on Windows only recognizes IgnoreCertificateErrors and sets CipherName
  // - typical usage is the following:
  // ! with THttpClientSocket.Create do
  // ! try
  // !   TLS.WithPeerInfo := true;
  // !   TLS.IgnoreCertificateErrors := true;
  // !   TLS.CipherList := 'ECDHE-RSA-AES256-GCM-SHA384';
  // !   ConnectUri('https://synopse.info');
  // !   ConsoleWrite(TLS.PeerInfo);
  // !   ConsoleWrite(TLS.CipherName);
  // !   ConsoleWrite([Get('/forum/', 1000), ' len=', ContentLength]);
  // !   ConsoleWrite(Get('/fossil/wiki/Synopse+OpenSource', 1000));
  // ! finally
  // !   Free;
  // ! end;
  // - for passing a PNetTlsContext, use InitNetTlsContext for initialization
  TNetTlsContext = record
    /// output: set by ConnectUri/OpenBind method once TLS is established
    Enabled: boolean;
    /// input: let HTTPS be less paranoid about TLS certificates
    // - on client: will avoid checking the server certificate, so will
    // allow to connect and encrypt e.g. with secTLSSelfSigned servers
    // - on OpenSSL server, should be true if no mutual authentication is done,
    // i.e. if OnPeerValidate/OnEachPeerVerify callbacks are not set
    IgnoreCertificateErrors: boolean;
    /// input: if PeerInfo field should be retrieved once connected
    WithPeerInfo: boolean;
    /// input: if deprecated TLS 1.0 or TLS 1.1 are allowed
    // - default is TLS 1.2+ only, and deprecated SSL 2/3 are always disabled
    AllowDeprecatedTls: boolean;
    /// input: enable two-way TLS for the server
    // - to be used with OnEachPeerVerify callback
    // - on OpenSSL client or server, set SSL_VERIFY_FAIL_IF_NO_PEER_CERT mode
    // - not used on SChannel
    ClientCertificateAuthentication: boolean;
    /// input: if two-way TLS client should be verified only once on the server
    // - to be used with OnEachPeerVerify callback
    // - on OpenSSL client or server, set SSL_VERIFY_CLIENT_ONCE mode
    // - not used on SChannel
    ClientVerifyOnce: boolean;
    /// input: allow legacy insecure renegotiation for unpatched/unsafe servers
    // - on OpenSSL client, set the SSL_OP_LEGACY_SERVER_CONNECT option
    // - not used on SChannel
    // - clients that are willing to connect to servers that don't implement RFC
    // 5746 secure renegotiation are subject to attacks such as CVE-2009-3555
    ClientAllowUnsafeRenegotation: boolean;
    /// input: PEM/PFX file name containing a certificate to be loaded
    // - (Delphi) warning: encoded as UTF-8 not UnicodeString/TFileName
    // - on OpenSSL client or server, calls SSL_CTX_use_certificate_file() API
    // - not used on SChannel client
    // - on SChannel server, expects a .pfx / PKCS#12 file format including
    // the certificate and the private key, e.g. generated from
    // ICryptCert.SaveToFile(FileName, cccCertWithPrivateKey, ', ccfBinary) or
    // openssl pkcs12 -inkey privkey.pem -in cert.pem -export -out mycert.pfx
    CertificateFile: RawUtf8;
    /// input: PEM/PFX content of a certificate to be loaded
    // - on OpenSSL client or server, calls SSL_CTX_use_certificate() API
    // - not used on SChannel client
    // - on SChannel server, expects a .pfx / PKCS#12 binary content
    CertificateBin: RawByteString;
    /// input: opaque pointer containing a certificate to be used
    // - on OpenSSL client or server, calls SSL_CTX_use_certificate() API
    // expecting the pointer to be of PX509 type
    // - not used on SChannel
    CertificateRaw: pointer;
    /// input: PEM file name containing a private key to be loaded
    // - (Delphi) warning: encoded as UTF-8 not UnicodeString/TFileName
    // - on OpenSSL client or server, calls SSL_CTX_use_PrivateKey_file() API
    // - not used on SChannel
    PrivateKeyFile: RawUtf8;
    /// input: optional password to load the PrivateKey file
    // - see also OnPrivatePassword callback
    // - on OpenSSL client or server, calls
    // SSL_CTX_set_default_passwd_cb_userdata() API
    // - not used on SChannel
    PrivatePassword: RawUtf8;
    /// input: opaque pointer containing a private key to be used
    // - on OpenSSL client or server, calls SSL_CTX_use_PrivateKey() API
    // expecting the pointer to be of PEVP_PKEY type
    // - not used on SChannel
    PrivateKeyRaw: pointer;
    /// input: file containing a specific set of CA certificates chain
    // - e.g. entrust_2048_ca.cer from https://web.entrust.com
    // - (Delphi) warning: encoded as UTF-8 not UnicodeString/TFileName
    // - on OpenSSL, calls the SSL_CTX_load_verify_locations() API
    // - not used on SChannel
    CACertificatesFile: RawUtf8;
    /// input: opaque pointers containing a set of CA certificates
    // - on OpenSSL client or server, calls SSL_CTX_get_cert_store() API then
    // X509_STORE_add_cert() on all pointers of PX509 type - i.e. expecting
    // here a PX509DynArray e.g. from LoadCertificates() as such:
    // ! var certs: PX509DynArray;
    // ! ...
    // !   certs := LoadCertificates(CA_CHAIN);
    // !   aTlsContext.CACertificatesRaw := TPointerDynArray(certs);
    // !   // ... eventually ...
    // !   PX509DynArrayFree(certs);
    // - not used on SChannel client
    CACertificatesRaw: TPointerDynArray;
    /// input: defines a set of CA certificates to be retrieved from the OS
    // - on OpenSSL, calls and uses our cached LoadCertificatesFromSystemStore()
    // which is more versatile than default SSL_CTX_set_default_verify_paths(),
    // especially on Windows
    // - not used on SChannel client
    CASystemStores: TSystemCertificateStores;
    /// input: preferred Cipher List
    // - not used on SChannel
    CipherList: RawUtf8;
    /// input: a CSV list of host names to be validated
    // - e.g. 'smtp.example.com,example.com'
    // - not used on SChannel
    HostNamesCsv: RawUtf8;
    /// output: the cipher description, as used for the current connection
    // - text format depends on the used TLS library e.g. on OpenSSL may be e.g.
    // 'ECDHE-RSA-AES128-GCM-SHA256 TLSv1.2 Kx=ECDH Au=RSA Enc=AESGCM(128) Mac=AEAD'
    // or 'TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA256_P256 TLSv1.2' with SChannel
    // (or less complete 'ECDHE256-AES128-SHA256 TLSv1.2' information on XP)
    CipherName: RawUtf8;
    /// output: the connected Peer issuer name
    // - e.g. '/C=US/O=Let''s Encrypt/CN=R3'
    // - populated on both SChannel and OpenSSL
    PeerIssuer: RawUtf8;
    /// output: the connected Peer subject name
    // - e.g. 'CN=synopse.info'
    // - populated on both SChannel and OpenSSL
    PeerSubject: RawUtf8;
    /// output: detailed information about the connected Peer as text
    // - stored in the native format of the TLS library, e.g. X509_print()
    // or ToText(TWinCertInfo)
    // - only populated if WithPeerInfo was set to true, or an error occurred
    PeerInfo: RawUtf8;
    /// output: full detailed raw information about the connected Peer
    // - is a PX509 on OpenSSL, or a PWinCertInfo from mormot.lib.sspi on SChannel
    PeerCert: pointer;
    /// output: low-level details about the last error at TLS level
    // - typically one X509_V_ERR_* integer constant
    LastError: RawUtf8;
    /// called by INetTls.AfterConnection to fully customize peer validation
    // - not implemented on SChannel
    OnPeerValidate: TOnNetTlsPeerValidate;
    /// called by INetTls.AfterConnection for each peer validation
    // - allow e.g. to verify CN or DNSName fields of each peer certificate
    // - see also ClientCertificateAuthentication and ClientVerifyOnce options
    // - not implemented on SChannel
    OnEachPeerVerify: TOnNetTlsEachPeerVerify;
    /// called by INetTls.AfterConnection after standard peer validation
    // - allow e.g. to verify CN or DNSName fields of the peer certificate
    // - not implemented on SChannel
    OnAfterPeerValidate: TOnNetTlsAfterPeerValidate;
    /// called by INetTls.AfterConnection to retrieve a private password
    // - not implemented on SChannel
    OnPrivatePassword: TOnNetTlsGetPassword;
    /// called by INetTls.AfterAccept to set a server/host-specific certificate
    // - used e.g. by TAcmeLetsEncryptServer to allow SNI per-host certificate
    // - not implemented on SChannel
    OnAcceptServerName: TOnNetTlsAcceptServerName;
    /// opaque pointer used by INetTls.AfterBind/AfterAccept to propagate the
    // bound server certificate context into each accepted connection
    // - so that certificates are decoded only once in AfterBind
    // - is typically a PSSL_CTX on OpenSSL, or a PCCERT_CONTEXT on SChannel
    AcceptCert: pointer;
  end;

  /// abstract definition of the TLS encrypted layer
  // - is implemented e.g. by the SChannel API on Windows by this unit, or
  // OpenSSL on POSIX if you include mormot.lib.openssl11 to your project
  // - on Windows, you can define USE_OPENSSL and FORCE_OPENSSL conditionals
  // in YOUR project options to switch to OpenSSL instead of SChannel
  INetTls = interface
    /// method called once to attach the socket from the client side
    // - should make the proper client-side TLS handshake and create a session
    // - should raise an exception on error
    procedure AfterConnection(Socket: TNetSocket; var Context: TNetTlsContext;
      const ServerAddress: RawUtf8);
    /// method called once the socket has been bound on server side
    // - will set Context.AcceptCert with reusable server certificates info
    procedure AfterBind(Socket: TNetSocket; var Context: TNetTlsContext;
      const ServerAddress: RawUtf8);
    /// method called for each new connection accepted on server side
    // - should make the proper server-side TLS handshake and create a session
    // - should raise an exception on error
    // - BoundContext is the associated server instance with proper AcceptCert
    // as filled by AfterBind()
    procedure AfterAccept(Socket: TNetSocket; const BoundContext: TNetTlsContext;
      LastError, CipherName: PRawUtf8);
    /// retrieve the textual name of the cipher used following AfterAccept()
    function GetCipherName: RawUtf8;
    /// return the low-level TLS instance used, depending on the engine
    // - typically a PSSL on OpenSSL, so you can use e.g. PSSL().PeerCertificate,
    // or a PCtxtHandle on SChannel
    function GetRawTls: pointer;
    /// return the low-level certificate binary content
    // - and optionally the name of its signature algorithm hash (e.g. 'SHA256')
    function GetRawCert(SignHashName: PRawUtf8 = nil): RawByteString;
    /// receive some data from the TLS layer
    function Receive(Buffer: pointer; var Length: integer): TNetResult;
    /// check if there are some input data within the TLS buffers
    // - may be the case even with no data any more at TCP/socket level
    // - returns -1 if there is no TLS connection opened
    // - returns the number of bytes in the internal buffer
    // - returns 0 if the internal buffer is void - but there may be some
    // data ready to be unciphered at socket level
    function ReceivePending: integer;
    /// send some data from the TLS layer
    function Send(Buffer: pointer; var Length: integer): TNetResult;
  end;

/// initialize a stack-allocated TNetTlsContext instance
procedure InitNetTlsContext(var TLS: TNetTlsContext); overload;

/// initialize a stack-allocated TNetTlsContext instance with auth parameters
procedure InitNetTlsContext(var TLS: TNetTlsContext; Server: boolean;
  const CertificateFile: TFileName = '';
  const PrivateKeyFile: TFileName = ''; const PrivateKeyPassword: RawUtf8 = '';
  const CACertificatesFile: TFileName = ''); overload;

/// purge all output fields for a TNetTlsContext instance for proper reuse
procedure ResetNetTlsContext(var TLS: TNetTlsContext);

/// setup and return a PNetTlsContext parameter from the specified parameters
function GetTlsContext(TlsEnabled, IgnoreTlsCertError: boolean;
  var Context: TNetTlsContext; Forced: PNetTlsContext = nil): PNetTlsContext;

/// compare the main fields of twoTNetTlsContext instances
// - won't compare the callbacks
function SameNetTlsContext(const tls1, tls2: TNetTlsContext): boolean;

var
  /// global factory for a new TLS encrypted layer for TCrtSocket
  // - on Windows, this unit will set a factory using the system SChannel API
  // - could also be overriden e.g. by the mormot.lib.openssl11.pas unit
  NewNetTls: function: INetTls;

  /// set globally to setup TNetTlsContext.OnAcceptServerName SNI callbacks
  // - default false may be lighter, e.g. for a single-host HTTPS server
  // - is set e.g. by mormot.net.acme.pas initialization section
  // - not used on SChannel yet
  EnableOnNetTlsAcceptServerName: boolean;

{$ifdef OSWINDOWS}
var
  /// try to enable TLS 1.3 over SChannel on Windows 11 or Windows Server 2022+
  // - this flag does nothing on older versions of Windows
  // - by default, it is disabled because our wrapper was reported to be
  // unstable on some Windows builds :(
  SChannelEnableTls13: boolean = false;

/// SChannel TLS layer communication factory - as expected by this unit
// - can be used at runtime to override another implementation e.g.
// @NewOpenSslNetTls from mormot.lib.openssl11 by executing:
// ! @NewNetTls := @NewSChannelNetTls;
function NewSChannelNetTls: INetTls;
{$endif OSWINDOWS}


{ ******************** TSocketStream Socket Wrapper }

type
  /// abstract parent class of both TSocketStream and TCrtSocketStream
  TSocketStreamAbstract = class(TStreamWithNoSeek)
  protected
    fLastRawError: integer;
    fLastResult: TNetResult;
    fOwned: TObject;
  public
    /// optional class instance for which Destroy will call Owned.Free
    property Owned: TObject
      read fOwned write fOwned;
  published
    /// the low-level result code of the last Read() or Write() method call
    property LastResult: TNetResult
      read fLastResult;
    /// the raw socket error code of the last Read() or Write() method call
    property LastRawError: integer
      read fLastRawError;
  end;

  /// encapsulate a raw (TLS-encrypted) Socket to a TStream class
  // - directly redirect Read/Write to socket's recv/send methods
  // - this class will report fake increasing Size = Position after Read/Write
  TSocketStream = class(TSocketStreamAbstract)
  protected
    fSocket: TNetSocket;
    fSecure: INetTls;
  public
    /// initialize this TStream for a given Socket handle
    // - this class instance won't own nor release this Socket once done
    constructor Create(aSocket: TNetSocket); reintroduce; overload;
    /// initialize this TStream for a given TLS encryption instance
    constructor Create(const aSecure: INetTls); reintroduce; overload;
    /// finalize this TStream instance, eventually calling Owned.Free
    destructor Destroy; override;
    /// receive some bytes from the associated Socket
    // - returns the number of bytes filled into Buffer (<=Count)
    function Read(var Buffer; Count: Longint): Longint; override;
    /// send some data to the associated Socket
    function Write(const Buffer; Count: Longint): Longint; override;
    /// access to the underlying Socket instance
    property Socket: TNetSocket
      read fSocket;
    /// access to the underlying INetTls instance
    property Secure: INetTls
      read fSecure;
  end;


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
  PPollSocketEvents = ^TPollSocketEvents;

  /// some opaque value (typically a pointer) associated with a polling event
  TPollSocketTag = type PtrInt;
  PPollSocketTag = ^TPollSocketTag;
  TPollSocketTagDynArray = TPtrUIntDynArray;

  /// modifications notified by TPollSocketAbstract.WaitForModified
  // - this opaque 64-bit tag will contain all the data needed for a result
  // - use ResToTag/ResToEvents and SetRes wrapper functions
  {$ifdef CPU32}
  TPollSocketResult = TQWordRec;
  {$else}
  TPollSocketResult = QWord;
  {$endif CPU32}

  PPollSocketResult = ^TPollSocketResult;
  TPollSocketResultDynArray = array of TPollSocketResult;

  /// all modifications returned by TPollSocketAbstract.WaitForModified
  TPollSocketResults = record
    // hold [0..Count-1] notified events
    Events: TPollSocketResultDynArray;
    /// how many modifications are currently monitored in Results[]
    Count: PtrInt;
  end;

  {$M+}
  TPollSockets = class;

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
    /// should finalize this processing before shutdown
    procedure Terminate; virtual;
  published
    /// how many TSocket instances are currently tracked
    property Count: integer
      read fCount;
  end;
  {$M-}

  /// abstract parent class for efficient socket polling
  // - on Linux, FollowEpoll=true uses the epoll API in level-triggered (LT) mode
  // - on other systems (Windows or BSD), fallback to select or poll API, with
  // FollowEpoll=false - note that Subscribe/Unsubscribe should be delayed
  // outside the WaitForModified() call using an async separated list
  // - implements libevent-like cross-platform features
  // - use PollSocketClass global function to retrieve the best class depending
  // on the running Operating System
  // - actual classes are hidden in the implementation section of this unit,
  // and will use the fastest available API on each Operating System
  // - this class is NOT thread-safe, with the exception of TPollSocketEpoll
  TPollSocketAbstract = class(TPollAbstract)
  protected
    fMaxSockets: integer;
    fOwner: TPollSockets;
  public
    /// initialize the polling
    constructor Create(aOwner: TPollSockets = nil); reintroduce; virtual;
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
    /// if this poll has no size limit, and subscription/wait is thread safe
    // with edge detection
    // - false for select/poll, but true for epoll
    class function FollowEpoll: boolean; virtual;
  published
    /// how many TSocket instances could be tracked, at most, in a single instance
    // - depends on the API used
    // - equals Count for TPollSocketEpoll, which has no absolute maximum
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

  // as used by TPollSockets.Subscribe for select/poll thread safety
  TPollSocketsSubscribe = record
    socket: TNetSocket;
    tag: TPollSocketTag;
    events: TPollSocketEvents;
  end;
  PPollSocketsSubscribe = ^TPollSocketsSubscribe;
  TPollSocketsSubscribeDynArray = array of TPollSocketsSubscribe;

  // as used by TPollSockets.Subscribe/Unsubscribe for select/poll thread safety
  TPollSocketsSubscription = record
    Unsubscribe: TNetSocketDynArray;
    Subscribe: TPollSocketsSubscribeDynArray;
    UnsubscribeCount: integer;
    SubscribeCount: integer;
  end;

  /// implements efficient polling of multiple sockets
  // - will maintain a pool of TPollSocketAbstract instances, to monitor
  // incoming data or outgoing availability for a set of active connections
  // - call Subscribe/Unsubscribe to setup the monitored sockets
  // - call GetOne from a main thread, optionally GetOnePending from sub-threads
  TPollSockets = class(TPollAbstract)
  protected
    fMergeSubscribeEventsLock: TLightLock; // topmost to ensure aarch64 alignment
    fSubscriptionSafe: TLightLock; // dedicated not to block Accept()
    fPendingSafe: TOSLightLock; // TLightLock seems less stable on high-end HW
    fPoll: array of TPollSocketAbstract; // each track up to fPoll[].MaxSockets
    fPending: TPollSocketResults;
    fPendingIndex: PtrInt;
    fPollIndex: integer;
    fGettingOne: integer;
    fTerminated: boolean;
    fUnsubscribeShutdownSocket: boolean;
    fPollClass: TPollSocketClass;
    fOnLog: TSynLogProc;
    fOnGetOneIdle: TOnPollSocketsIdle;
    // used for select/poll (FollowEpoll=false) with multiple thread-unsafe fPoll[]
    fSubscription: TPollSocketsSubscription;
    fPollLock: TOSLightLock;
    // note: $ifdef POLLSOCKETEPOLL is not possible here
    function GetSubscribeCount: integer;
    function GetUnsubscribeCount: integer;
    function MergePendingEvents(const new: TPollSocketResults): integer;
    function MergeSubscribeEvents: boolean;
    // virtual methods below could be overridden for O(1) pending state check
    function EnsurePending(tag: TPollSocketTag): boolean; virtual;
    procedure SetPending(tag: TPollSocketTag); virtual;
    function UnsetPending(tag: TPollSocketTag): boolean; virtual;
  public
    /// initialize the sockets polling
    // - under Linux/POSIX, will set the open files maximum number for the
    // current process to match the system hard limit: if your system has a
    // low "ulimit -H -n" value, you may add the following line in your
    // /etc/limits.conf or /etc/security/limits.conf file:
    // $ * hard nofile 65535
    // - you can specify PollFewSocketClass as aPollClass if only a few
    // sockets are likely to be tracked (to use lighter poll instead of epoll)
    constructor Create(aPollClass: TPollSocketClass = nil);
    /// finalize the sockets polling, and release all used memory
    destructor Destroy; override;
    /// track modifications on one specified TSocket and tag
    // - the supplied tag value - maybe a PtrInt(aObject) - will be part of
    // GetOne/GetOnePending methods TPollSocketResult.Tag results
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
    // - warning: this method should be called from a single thread on Linux
    // (PollClass.FollowEpoll=true) since epoll_wait() is used - other select/poll
    // API would work on concurrent call, but with lost resources - typically, a
    // main thread calls GetOne() while other threads could call GetOnePending()
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
    procedure AddOnePending(aTag: TPollSocketTag; aEvents: TPollSocketEvents;
      aSearchExisting: boolean);
    /// disable any pending notification associated with a given connection tag
    // - can be called when a connection is removed from the main logic
    // to ensure function UnsetPending() never raise any GPF, if the
    // connection has been set via AddOnePending() but not via Subscribe()
    function DeleteOnePending(aTag: TPollSocketTag): boolean;
    /// disable any pending notification associated with several connection tags
    // - note that aTag array will be sorted during the process
    function DeleteSeveralPending(aTag: PPollSocketTag; aTagCount: integer): integer;
    /// notify any GetOne waiting method to stop its polling loop
    procedure Terminate; override;
    /// indicates that Unsubscribe() should also call ShutdownAndClose(socket)
    // - Destroy will also shutdown any remaining sockets if PollForPendingEvents
    // has not been called before shutdown
    property UnsubscribeShouldShutdownSocket: boolean
      read fUnsubscribeShutdownSocket write fUnsubscribeShutdownSocket;
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
    /// how many connections are pending to be subscribed (poll/select API)
    property SubscribeCount: integer
      read GetSubscribeCount default 0;
    /// how many connections are pending to be unsubscribed (poll/select API)
    property UnsubscribeCount: integer
      read GetUnsubscribeCount default 0;
  end;


/// extract the TPollSocketTag pointer from TPollSocketResult opaque 64-bit
function ResToTag(const res: TPollSocketResult): TPollSocketTag;
  {$ifdef HASINLINE}inline;{$endif}

/// extract the TPollSocketEvents set from TPollSocketResult opaque 64-bit
function ResToEvents(const res: TPollSocketResult): TPollSocketEvents;
  {$ifdef HASINLINE}inline;{$endif}

/// fill a TPollSocketResult opaque 64-bit from its corresponding information
procedure SetRes(var res: TPollSocketResult; tag: TPollSocketTag; ev: TPollSocketEvents);
  {$ifdef HASINLINE}inline;{$endif}

/// set the TPollSocketEvents set as [] from TPollSocketResult opaque 64-bit
procedure ResetResEvents(var res: TPollSocketResult);
  {$ifdef HASINLINE}inline;{$endif}

/// class function factory, returning a socket polling class matching
// at best the current operating system for a high number of sockets
// - return a hidden TPollSocketSelect class under Windows, TPollSocketEpoll
// under Linux, or TPollSocketPoll on BSD
// - not to be used directly, but within TPollSockets.Create
function PollSocketClass: TPollSocketClass;

/// return a class instance able to poll the state of a few sockets
// - allow to track some sockets via Subscribe/WaitForModified/Unsubscribe
// - return a TPollSocketSelect under Windows, or TPollSocketPoll on POSIX, so
// up to 512 sockets on Windows (via select), 20000 on POSIX (via poll)
function PollFewSockets: TPollSocketAbstract;

/// poll once the state of several sockets, directly as TPollSocketResults info
// - will only check the subscribed Sockets[] in pseRead state
// - use select() under Windows, or poll() on POSIX with temporary FD arrays
function WaitForSeveral(const Sockets: TPollSocketsSubscribeDynArray;
  var results: TPollSocketResults; timeoutMS: integer): boolean;

const
  POLL_SOCKET_EVENT: array[TPollSocketEvent] of AnsiChar = 'rwec';

function ToText(ev: TPollSocketEvents): TShort8; overload;


{ *************************** Windows IOCP sockets support }

{$ifdef OSWINDOWS}

type
  EWinIocp = class(ExceptionWithProps);

  /// define the events TWinIocp can monitor
  // - all wieCustom* events are user-triggered events via TWinIocp.Enqueue()
  TWinIocpEvent = (
    wieRecv,
    wieSend,
    wieAccept,
    wieConnect,
    wieCustom1,
    wieCustom2,
    wieCustom3,
    wieCustom4);

  /// opaque pointer to one TWinIocp.Subscribe state
  PWinIocpSubscription = ^TWinIocpSubscription;
  /// high-level access to one TWinIocp.Subscribe state details
  {$ifdef USERECORDWITHMETHODS}
  TWinIocpSubscription = record
  {$else}
  TWinIocpSubscription = object
  {$endif USERECORDWITHMETHODS}
  public
    /// return the TPollSocketTag associated with a Subscribe() call
    function Tag: TPollSocketTag;
    /// return the TNetSocket associated with a Subscribe() call
    function Socket: TNetSocket;
    /// check the overlapped status of a Subscribe() call
    function CurrentStatus(event: TWinIocpEvent): TPollSocketEvents;
  end;

  /// allow to customize TWinIocp process
  // - wioUnsubscribeShutdownSocket let Unsubscribe() also call
  // ShutdownAndClose(socket)
  // - paranoid wioLockEvent will track PrepareNext/GetNext pairs
  TWinIocpOption = (
    wioUnsubscribeShutdownSocket,
    wioLockEvent);
  TWinIocpOptions = set of TWinIocpOption;

  {$M+}
  /// efficient socket polling via Windows' IOCP API
  // - IOCP logic does not match select() or poll/epoll() APIs so it can't
  // inherit from TPollAbstract, and requires its own stand-alone class
  // - will handle wieRecv/wieSend events on a set of subscribed sockets
  // - wieAccept/wieConnect events would track asynchronous AcceptEx() or
  // ConnectEx() calls
  // - mormot.net.async will check USE_WINIOCP conditional to use this class
  TWinIocp = class
  protected
    fOne: TLockedList; // O(1) allocate/recycle PWinIocpSubscription instances
    fMaxWait, fWaiting, fPosted: integer;
    fOptions: TWinIocpOptions;
    fTerminated: boolean;
    fOnLog: TSynLogProc;
    fIocp: THandle;
    fAcceptExUsed: TLightLock; // can track only a single AcceptEx()
    fAcceptSocket: TNetSocket;
    fAcceptExBuf: TBytes;
  public
    /// initialize this IOCP queue for a number of processing thread
    constructor Create(processing: integer = 1; options: TWinIocpOptions = []);
    /// finalize this IOCP queue
    destructor Destroy; override;
    /// associate this IOCP queue to a given socket
    // - no event is actually tracked, until PrepareNext() is called
    // - the IOCP API limits a socket to be tracked by a single TWinIocp queue
    function Subscribe(socket: TNetSocket;
      tag: TPollSocketTag): PWinIocpSubscription;
    /// unsubscribe for events on a given socket
    // - will also set one := nil to avoid any dangling pointer
    function Unsubscribe(var one: PWinIocpSubscription): boolean;
    /// notify IOCP that it needs to track the next event on this subscription
    // - typically called after socket recv/send to re-subscribe for events
    // - for wieRecv events, you should better not supply any buf/buflen to
    // avoid potential WSAENOBUFS errors (the "zero read byte trick")
    // - for wieSend, you would rather specify a buffer to be sent asynchronously
    // and avoid GetNext() to return immediately even if send() would fail
    // - for wieAccept, you can specify a pre-allocated TNetSocket (default nil
    // will allocate one in the method)
    // - for wieConnect, you need to specify a TNetSocket (not already bound) in
    // netsock and a TNetAddr in buf/buflen
    function PrepareNext(const ctxt: ShortString;
      one: PWinIocpSubscription; event: TWinIocpEvent;
      buf: pointer = nil; buflen: integer = 0; netsock: TNetSocket = nil): boolean;
    /// add manually an event to the IOCP queue
    // - it won't make any actual access to a socket, just append an event to
    // the queue, as regular wieRecv .. wieConnect event or any wieCustom*
    function Enqueue(one: PWinIocpSubscription; event: TWinIocpEvent;
      bytes: cardinal = 0): boolean;
    /// pick a pending task from the internal queue within a specified timeout
    // - is typically called from processing threads
    // - for wieRecv/wieSend, once data is recv/send from result^.Socket,
    // call PrepareNext()
    // - for wieAccept, call then GetNextAccept() and PrepareNext()
    // - for wieConnect, start using the socket, e.g. with wieRecv/wieSend events
    // - for wieCustom*, it depends on your own custom logic
    function GetNext(timeoutms: cardinal;
      out event: TWinIocpEvent; out bytes: cardinal): PWinIocpSubscription;
    /// retrieve the new socket and remote address after a GetNext(wieAccept)
    function GetNextAccept(one: PWinIocpSubscription;
      out Socket: TNetSocket; out Remote: TNetAddr): boolean;
    /// shutdown this IOCP process and its queue - called e.g. by Destroy
    procedure Terminate;
    /// how many processing threads are likely to call GetNext
    property MaxWait: integer
      read fMaxWait;
    /// flag set when Terminate has been called
    property Terminated: boolean
      read fTerminated;
    /// allow raw debugging via logs of the low-level process
    property OnLog: TSynLogProc
      read fOnLog write fOnLog;
  published
    /// how many TSocket instances are currently tracked
    property Count: integer
      read fOne.Count;
    /// how many PrepareNext() are waiting for their asynchronous GetNext()
    property Posted: integer
      read fPosted;
    /// how many GetNext() are waiting for the next event
    // - Waiting=MaxWait means that there is currently no completed event
    property Waiting: integer
      read fWaiting;
  end;
  {$M-}

{$endif OSWINDOWS}


{ *************************** TUri parsing/generating URL wrapper }

type
  /// the main URI schemes recognized by TUri.UriScheme
  TUriScheme = (usUndefined, usCustom,
    usHttp, usWs, usHttps, usWss, usUdp, usFile, usFtp, usFtps);

  /// structure used to parse an URI into its components
  // - ready to be supplied e.g. to a THttpRequest sub-class
  // - used e.g. by class function THttpRequest.Get()
  // - will decode standard HTTP/HTTPS urls or our custom Unix sockets URI like
  // 'http://unix:/path/to/socket.sock:/url/path'
  // - could also be used to generate an URI e.g. from Server/Address info
  {$ifdef USERECORDWITHMETHODS}
  TUri = record
  {$else}
  TUri = object
  {$endif USERECORDWITHMETHODS}
  public
    /// if the server is accessible via https:// wss:// and not plain http://
    Https: boolean;
    /// either nlTcp for HTTP/HTTPS or nlUnix for Unix socket URI
    Layer: TNetLayer;
    /// used to identify most known schemes
    UriScheme: TUriScheme;
    /// the protocol as specified for this URI
    // - e.g. 'http'/'https' for http:// https:// or 'ws'/'wss' for ws:// wss://
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
    // - e.g. 'category/name/10?param=1'
    Address: RawUtf8;
    /// reset all stored information
    procedure Clear;
    /// fill the members from a supplied URI
    // - recognize e.g. 'http://server:port/address', 'https://server/address',
    // 'server/address' (as http), 'http://unix:/server:/address' (as nlUnix),
    // 'https://user:password@server:port/address' (authenticated),
    // 'wss://Server/Address' (as https) or 'file://server/folder/data.xml'
    // - returns TRUE if the Server has been extracted and is not ''
    function From(aUri: RawUtf8; const DefaultPort: RawUtf8 = ''): boolean;
    /// check if a connection need to be re-established to follow this URI
    function Same(const aServer, aPort: RawUtf8; aHttps: boolean): boolean;
    /// check if a connection need to be re-established to follow this URI
    function SameUri(const aUri: RawUtf8): boolean;
    /// compute the whole normalized URI
    // - e.g. 'https://Server:Port/Address' or 'http://unix:/Server:/Address'
    // - User/Password property values won't be included
    function URI: RawUtf8;
    /// compute the normalized URI of the server and port
    // - e.g. 'https://Server:Port/' or 'http://unix:/Server:/'
    // - i.e. URI result without the Address part
    function ServerPort: RawUtf8;
    /// the server port, as integer value
    function PortInt: TNetPort;
    /// compute the root resource Address, without any URI-encoded parameter
    // - e.g. '/category/name/10'
    function Root: RawUtf8;
    /// comute the root resource Address as a resource "file" name
    // - e.g. '10' for '/category/name/10?param=1'
    // - warning: no TFileName nor UrlDecode() conversion is performed - call
    // ExtractResourceName() from mormot.net.client.pas for proper sanitation
    function ResourceName: RawUtf8;
    /// returns BinToBase64(User + ':' + Password) encoded value
    // - as used for "Authorization: Basic" and "Proxy-Authorization: Basic"
    function UserPasswordBase64: RawUtf8;
  end;
  PUri = ^TUri;

  /// 32-bit binary storage of a IPv4 CIDR sub-network for fast comparison
  {$ifdef USERECORDWITHMETHODS}
  TIp4SubNet = record
  {$else}
  TIp4SubNet = object
  {$endif USERECORDWITHMETHODS}
    /// 32-bit masked IP, e.g. 1.2.3.0 for '1.2.3.4/24'
    ip: cardinal;
    /// 32-bit IP mask, e.g. 255.255.255.0 for '1.2.3.4/24'
    mask: cardinal;
    /// check and decode the supplied CIDR address text from its format '1.2.3.4/24'
    // - e.g. as 32-bit 1.2.3.0 into ip and 255.255.255.0 into mask
    // - plain IP address like '1.2.3.4' will be decoded with mask=255.255.255.255
    function From(const subnet: RawUtf8): boolean;
    /// check if an 32-bit IPv4 matches a decoded CIDR sub-network
    function Match(ip4: cardinal): boolean; overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// check if a textual IPv4 matches a decoded CIDR sub-network
    function Match(const ip4: RawUtf8): boolean; overload;
  end;

  /// store one TIp4SubNets CIDR mask definition
  TIp4SubNetMask = record
    /// 32-bit IP mask, e.g. 255.255.255.0 for '1.2.3.4/24'
    Mask: cardinal;
    /// how many 32-bit masked IP are actually stored in IP[]
    IPCount: integer;
    /// list of 32-bit masked IPs, e.g. 1.2.3.0 for '1.2.3.4/24'
    // - sorted to allow efficient O(log(n)) binary search in TIp4SubNets.Match
    IP: TIntegerDynArray;
  end;
  PIp4SubNetMask = ^TIp4SubNetMask;
  TIp4SubNetMasks = array of TIp4SubNetMask;

  /// store several CIDR sub-network mask definitions for efficient search
  // - to handle typically a blacklist of IP ranges e.g. from spamhaus.org
  TIp4SubNets = class(TSynPersistent)
  protected
    fSubNet: TIp4SubNetMasks;
  public
    /// decode and register the supplied CIDR address text e.g. as '1.2.3.4/24'
    function Add(const subnet: RawUtf8): boolean; overload;
    /// decode and register the supplied CIDR address as TIp4SubNet ip/mask
    // - by definition, private IP like 192.168.x.x are not added
    function Add(ip, mask: cardinal): boolean; overload;
    /// decode and add all IP and CIDR listed in a text content
    // - i.e. netsets as IP or CIDR with # or ; comments e.g. as in
    // https://www.spamhaus.org/drop/drop.txt or
    // https://github.com/firehol/blocklist-ipsets/blob/master/firehol_level1.netset
    // - by definition, private IP like 192.168.x.x are not included
    // - returns the number of added IP or CIDR, merging with existing content
    function AddFromText(const text: RawUtf8): integer;
    /// ensure all length(SubNet[].IP) = IPCount after Add/AddFromText usage
    // - returns the current total number of stored IP or CIDR
    // - not needed at runtime - just here e.g. for testing or specific usecases
    function AfterAdd: integer;
    /// check if a 32-bit IPv4 matches a registered CIDR sub-network
    // - reach 16M/s per core with spamhaus or firehol databases
    function Match(ip4: cardinal): boolean; overload;
    /// check if a textual IPv4 matches a registered CIDR sub-network
    function Match(const ip4: RawUtf8): boolean; overload;
    // remove all registered CIDR sub-networks
    procedure Clear;
    /// persist this list as optimized binary
    function SaveToBinary: RawByteString;
    /// clear, decode and add all IP and CIDR listed in a text or binary buffer
    // - is a wrapper around Clear + AddFromText/LoadFromBinary + AfterAdd
    // - returns the number of stored IP or CIDR, clearing any existing content
    // - if buffer is in fact a SaveToBinary content, will detect and load it
    function LoadFrom(const buffer: RawByteString): integer;
    /// clear and retrieve from a binary buffer persisted via SaveToBinary
    // - returns the number of stored IP or CIDR, clearing any existing content
    function LoadFromBinary(const bin: RawByteString): integer;
    /// low-level access to the internal storage
    // - warning: length(IP) may be > IPCount - do not use "for in SubNet[].IP"
    // pattern unless you called AfterAdd or LoadFromBinary
    property SubNet: TIp4SubNetMasks
      read fSubNet;
  end;

const
  /// 'MEL1' 32-bit magic marker used for TIp4SubNets.SaveToBinary format
  IP4SUBNET_MAGIC: cardinal = $314c454d;

/// check if a 32-bit IPv4 matches a registered CIDR sub-network binary buffer
// - directly parse TIp4SubNets.SaveToBinary output for conveniency
// - performance is in pair with TIp4SubNets.Match - so could be an option
// if you do not need to Add() items at runtime, but only check a fixed list
function IP4SubNetMatch(P: PCardinalArray; ip4: cardinal): boolean; overload;

/// check if a textual IPv4 matches a registered CIDR sub-network binary buffer
function IP4SubNetMatch(const bin: RawByteString; const ip4: RawUtf8): boolean; overload;


const
  /// the default TCP port as text, as DEFAULT_PORT[Https]
  DEFAULT_PORT: array[boolean] of RawUtf8 = (
    '80', '443');
  /// the default TCP port as integer, as DEFAULT_PORT_INT[Https]
  DEFAULT_PORT_INT: array[boolean] of TNetPort = (
    80, 443);
  /// can be used to generate e.g. http:// ws:// or https:// wss:// constants
  TLS_TEXT: array[boolean] of string[1] = (
    '', 's');
  /// quick access to http:// or https:// constants
  HTTPS_TEXT: array[boolean] of RawUtf8 = (
    'http://', 'https://');
  /// the HTTP-based URI schemes recognized by TUri.UriScheme
  HTTP_SCHEME = [usHttp, usWs, usHttps, usWss];

/// check is the supplied address text is on format '1.2.3.4'
// - will optionally fill a 32-bit binary buffer with the decoded IPv4 address
// - end text input parsing at final #0 '/' or any char <= ' '
function NetIsIP4(text: PUtf8Char; value: PByte = nil): boolean;

/// parse a text input buffer until the end space or EOL
function NetGetNextSpaced(var P: PUtf8Char): RawUtf8;

/// IdemPChar() like function, to avoid linking mormot.core.text
function NetStartWith(p, up: PUtf8Char): boolean;

/// BinToBase64() like function, needing mormot.core.buffers in the project
// - calls mormot.core.os RawToBase64() - only used for TUri.UserPasswordBase64
function NetBinToBase64(const s: RawByteString): RawUtf8;

/// IsPem() like function, to avoid linking mormot.crypt.secure
// - search for '-----BEGIN' text, so may hardly give some false positives
function NetIsPem(p: PUtf8Char): boolean;


{ ********* TCrtSocket Buffered Socket Read/Write Class }

type
  /// meta-class of a TCrtSocket (sub-)type
  TCrtSocketClass = class of TCrtSocket;

  /// identify the incoming data availability in TCrtSocket.SockReceivePending
  TCrtSocketPending = (
    cspSocketError,
    cspSocketClosed,
    cspNoData,
    cspDataAvailable,
    cspDataAvailableOnClosedSocket);

  TCrtSocketTlsAfter = (
    cstaConnect,
    cstaBind,
    cstaAccept);

  TCrtSocketFlags = set of (
    fAborted,
    fWasBind,
    fBodyRetrieved,
    fServerTlsEnabled,
    fProxyConnect,
    fProxyHttp);

  {$M+}
  /// fast low-level Socket implementation with optional input/output bufferring
  // - direct access to the OS (Windows, Linux) network layer API
  // - use Open/OpenUri constructor or Create + ConnectUri for a client socket,
  // or the Bind constructor to initialize a server
  // - call CreateSockIn to instantiate an input buffer for faster process via
  // SockInReadLn/SockInRead methods or even regular readln(SockIn^, ...)
  // - use SockSend() overloaded methods, followed by an eventual SockFlush
  // call, for buffered output (mORMot 1 CreateSockOut is now deprecated because
  // the RTL doesn't properly use its buffer to reduce syscalls)
  // - since this class relies on its internal optimized SockSend/SockFlush
  // buffering system, TCP_NODELAY option is set to disable the Nagle algorithm
  // - can use TLS (using the SChannel API on Windows, or by including
  // mormot.lib.openssl11 unit to your project) or HTTP Proxy/Tunnel
  TCrtSocket = class
  protected
    fSock: TNetSocket; // wrapper to a raw socket handle, stored as a pointer
    fSecure: INetTls;
    fServer: RawUtf8;
    fPort: RawUtf8;
    fFlags: TCrtSocketFlags;
    fSocketLayer: TNetLayer;
    fSocketFamily: TNetFamily;
    fTimeOut: integer;
    fSockIn: PTextFile;    // allocated by CreateSockIn with its own buffer
    fSndBufLen: PtrInt;    // updated by every SockSend() call
    fSndBuf: RawByteString;
    fRemoteIP: RawUtf8;    // set by OpenBind() or AcceptRequest() from TNetAddr
    fProxyUrl: RawUtf8;
    fOpenUriFull: RawUtf8; // set by OpenUri()
    fBytesIn: Int64;
    fBytesOut: Int64;
    procedure DoRaise(const msg: string; const args: array of const;
      error: TNetResult = nrOK; errnumber: system.PInteger = nil;
      exc: ENetSockClass = nil); overload;
    procedure DoRaise(const msg: string); overload;
    procedure SetKeepAlive(aKeepAlive: boolean); virtual;
    procedure SetLinger(aLinger: integer); virtual;
    procedure SetReceiveTimeout(aReceiveTimeout: integer); virtual;
    procedure SetSendTimeout(aSendTimeout: integer); virtual;
    procedure SetTcpNoDelay(aTcpNoDelay: boolean); virtual;
    function EnsureSockSend(Len: PtrInt): PUtf8Char;
      {$ifdef FPC}inline;{$endif}
    function GetRawSocket: PtrInt;
      {$ifdef HASINLINE}inline;{$endif}
    function GetAborted: boolean;
      {$ifdef HASINLINE}inline;{$endif}
  public
    /// direct access to the optional low-level HTTP proxy tunnelling information
    // - User/Password would be taken into consideration for authentication
    // - could be populated by mormot.net.client Tunnel.From(GetSystemProxy())
    Tunnel: TUri;
    /// direct access to the optional low-level TLS Options and Information
    // - depending on the actual INetTls implementation, some fields may not
    // be used nor populated - currently only supported by mormot.lib.openssl11
    // - reflect the raw socket layer, so TLS.Enabled may apply to the proxy
    // connection, to the actual destination: see ServerTls method instead
    TLS: TNetTlsContext;
    /// can be assigned to TSynLog.DoLog class method for low-level logging
    OnLog: TSynLogProc;
    /// common initialization of all constructors of this class
    // - if you call it directly, you can setup all the needed parameters (e.g.
    // TLS, Tunnel, THttpClientWebSockets.Settings) then call ConnectUri()
    // - see also Open/OpenUri/Bind other constructors
    constructor Create(aTimeOut: integer = 10000); reintroduce; virtual;
    /// constructor to create a client connection to aServer:aPort
    // - see also SocketOpen() for a wrapper catching any connection exception
    // - aTunnel could be populated by mormot.net.client GetSystemProxyUri()
    constructor Open(const aServer, aPort: RawUtf8; aLayer: TNetLayer = nlTcp;
      aTimeOut: cardinal = 10000; aTLS: boolean = false;
      aTLSContext: PNetTlsContext = nil; aTunnel: PUri = nil);
    /// constructor to create a client connection to a given URI
    // - returns TUri.Address as parsed from aUri
    constructor OpenUri(const aUri: RawUtf8; out aAddress: RawUtf8;
      const aTunnel: RawUtf8 = ''; aTimeOut: cardinal = 10000;
      aTLSContext: PNetTlsContext = nil); overload;
    /// constructor to create a client connection to a given URI
    // - returns TUri.Address as parsed from aUri
    constructor OpenUri(const aUri: TUri; const aUriFull, aTunnel: RawUtf8;
      aTimeOut: cardinal; aTLSContext: PNetTlsContext); overload; virtual;
    /// constructor to bind to an address
    // - just a wrapper around Create(aTimeOut) and BindPort()
    constructor Bind(const aAddress: RawUtf8; aLayer: TNetLayer = nlTcp;
      aTimeOut: integer = 10000; aReusePort: boolean = false);
    /// address binding processing method, as called by the Bind() constructor
    // - aAddr='1234' - bind to a port on all interfaces, the same as '0.0.0.0:1234'
    // - aAddr='IP:port' - bind to specified interface only, e.g.
    // '1.2.3.4:1234'
    // - aAddr='unix:/path/to/file' - bind to unix domain socket, e.g.
    // 'unix:/run/mymormotapp.sock'
    // - aAddr='' - bind to systemd descriptor on linux - see
    // http://0pointer.de/blog/projects/socket-activation.html
    procedure BindPort(const aAddress: RawUtf8; aLayer: TNetLayer = nlTcp;
      aReusePort: boolean = false);
    /// after Create(), create a client connection to a given server URI
    // - optionally returns TUri.Address as parsed from aUri
    // - raise an ENetSock exception on error
    // - this is just a convenient wrapper around OpenBind() for a client socket
    procedure ConnectUri(const aUri: RawUtf8; aAddress: PRawUtf8 = nil);
    /// after Create(), open or bind to a given server port
    // - consider Connect() if you just want to connect
    // - low-level internal method called by Open() and Bind() constructors
    // - raise an ENetSock exception on error
    // - optionaly via TLS (using the SChannel API on Windows, or by including
    // mormot.lib.openssl11 unit) - with custom input options in the TLS fields
    procedure OpenBind(const aServer, aPort: RawUtf8; doBind: boolean;
      aTLS: boolean = false; aLayer: TNetLayer = nlTcp;
      aSock: TNetSocket = TNetSocket(-1); aReusePort: boolean = false); virtual;
    /// a wrapper around Close + OpenBind() with the current settings
    // - could be used to reestablish a broken or closed connection
    // - return '' on success, or an error message on failure
    // - could be overriden to customize the process
    function ReOpen(aTimeout: cardinal = 10000): string; virtual;
    /// initialize the instance with the supplied accepted socket
    // - is called from a bound TCP Server, just after Accept()
    procedure AcceptRequest(aClientSock: TNetSocket; aClientAddr: PNetAddr);
    /// low-level TLS support method
    procedure DoTlsAfter(caller: TCrtSocketTlsAfter);
    /// check if the Server is accessed using TLS
    // - TLS.Enabled flag is about the raw socket, probably over a Tunnel/Proxy
    // - this function reflects the actual aTLS parameter supplied to OpenBind()
    function ServerTls: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// initialize SockIn text file instance, and its internal SockIn^.Buffer
    // - readln(SockIn^) and SockInReadLn/SockInRead methods benefit from this buffer
    // - default 1KB seems big enough for headers (content will be read directly)
    // - by default, expects CR+LF as line feed (i.e. the HTTP way)
    procedure CreateSockIn(LineBreak: TTextLineBreakStyle = tlbsCRLF;
      InputBufferSize: integer = 1024);
    /// finalize the SockIn text file instance and its SockIn^.Buffer
    // - you may call this method when you are sure that you don't need the
    // input buffering feature on this connection any more (e.g. after having
    // parsed the HTTP header, then rely on direct socket comunication)
    procedure CloseSockIn;
    /// close and shutdown the connection
    // - called from Destroy, but is reintrant so could be called earlier
    procedure Close; virtual;
    /// close the opened socket, and corresponding SockIn text file (if any)
    destructor Destroy; override;
    /// mark the internal Aborted flag to let any blocking loop abort ASAP
    // - will also close any associated socket at OS level
    procedure Abort; virtual;
    /// read Length bytes from SockIn^.Buffer and raw socket if necessary
    // - if SockIn is available, it first gets data from SockIn^.Buffer,
    // then directly receive data from socket if UseOnlySockIn = false
    // - if UseOnlySockIn = true, it will return only the data available in
    // SockIn^.Buffer and returns the corresponding number of bytes
    // - can be used also without SockIn: it will call directly SockRecv()
    // in such case (assuming UseOnlySockin=false)
    function SockInRead(Content: PAnsiChar; Length: PtrInt;
      UseOnlySockIn: boolean = false): integer; overload;
    /// read Length bytes from SockIn^.Buffer and raw socket if necessary
    // - just allocate a result string and call SockInRead() to fill it
    function SockInRead(Length: integer;
      UseOnlySockIn: boolean = false): RawByteString; overload;
    /// read the next line of text from SockIn^.Buffer or from the socket API
    // - returns the line size in bytes, stored with an ending #0 in Buffer
    // - returns -1 if Buffer's Size is too short, or raise an ENetSock on error
    function SockInReadLn(Buffer: PAnsiChar; Size: PtrInt): PtrInt;
    /// returns the number of bytes in SockIn^.Buffer or pending in the OS stack
    // - it first checks and quickly returns any length pending in SockIn^.Buffer
    // - if buffer is void, will call InputSock to fill it or check the socket API
    // - returns -1/-2 in case of a socket error (e.g. broken/closed connection)
    // - returns the number of bytes available in input buffers (SockIn or TLS):
    // there may be more waiting at the socket level
    function SockInPending(aTimeOutMS: integer): integer;
    /// checks if the low-level socket handle has been assigned
    // - just a wrapper around PtrInt(fSock)>0
    function SockIsDefined: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// check the connection status of the socket using getpeername()
    function SockConnected: boolean;
    /// simulate writeln() with direct use of Send(Sock, ..) - includes trailing #13#10
    // - useful on multi-treaded environnement (as in THttpServer.Process)
    // - handle RawByteString, ShortString, Char, integer/Int64 parameters
    procedure SockSend(const Values: array of const); overload;
    /// simulate writeln() with direct use of Send(Sock, ..) - includes trailing #13#10
    // - slightly faster than SockSend([]) if all appended items are RawUtf8
    procedure SockSendLine(const Values: array of RawUtf8);
    /// simulate writeln() with a single line - includes trailing #13#10
    procedure SockSend(const Line: RawByteString); overload;
    /// append P^ data into SndBuf (used by SockSend(), e.g.) - no trailing #13#10
    // - call SockSendFlush to send it through the network via SndLow()
    procedure SockSend(P: pointer; Len: PtrInt); overload;
    /// append PWideChar as UTF-8 (used by SockSend(), e.g.) - no trailing #13#10
    procedure SockSendW(w: PWideChar; l: PtrInt);
    /// append headers content, normalizing #13#10 in the content and at ending
    procedure SockSendHeaders(const headers: RawUtf8);
    /// append #13#10 characters on all platforms, never #10 even on POSIX
    procedure SockSendCRLF;
    /// flush all pending data to be sent, optionally with some body content
    // - raise ENetSock on error, unless aNoRaise is set and it returns the error
    function SockSendFlush(const aBody: RawByteString = '';
      aNoRaise: boolean = false): TNetResult;
    /// send all TStream content till the end using SndLow()
    // - don't forget to call SockSendFlush before using this method
    // - will call Stream.Read() over a temporary buffer of 1MB by default
    // - Stream may be a TFileStream, THttpMultiPartStream or TNestedStreamReader
    // - raise ENetSock on error, unless aNoRaise is set and it returns the error
    // - if aCheckRecv is true, will check SockReceivePending() between each
    // chunk, and return nrRetry if the server did respond (e.g. a 413 error)
    function SockSendStream(Stream: TStream; ChunkSize: integer = 1 shl 20;
      aNoRaise: boolean = false; aCheckRecv: boolean = false): TNetResult;
    /// how many bytes could be added by SockSend() in the internal buffer
    function SockSendRemainingSize: PtrInt;
      {$ifdef HASINLINE}inline;{$endif}
    /// fill the Buffer with Length bytes
    // - wait TimeOut milliseconds until Length bytes are actually received
    // - bypass the SockIn^.Buffer
    // - raise ENetSock exception on socket error, or if Length was not reached
    procedure SockRecv(Buffer: pointer; Length: integer); overload;
    /// fill a RawByteString Buffer with Length bytes
    // - wait TimeOut milliseconds until Length bytes are actually received
    // - bypass the SockIn^.Buffer
    // - raise ENetSock exception on socket error, or if Length was not reached
    function SockRecv(Length: integer): RawByteString; overload;
    /// check if there are some pending bytes in the input sockets API buffer
    // - returns cspSocketError/cspSocketClosed if the connection is broken/closed
    // - will first check for any INetTls.ReceivePending bytes in the TLS buffers
    // - warning: on Windows, may wait for the next system timer interrupt, so
    // actual wait may be a less than TimeOutMS if < 16 (select bug/feature)
    function SockReceivePending(TimeOutMS: integer;
      loerr: system.PInteger = nil): TCrtSocketPending;
    /// return how many pending bytes are in the receiving socket or INetTls queue
    // - returns 0 if no data is available, or if the connection is broken: call
    // SockReceivePending() to check for the actual state of the connection
    function SockReceiveHasData: integer;
    /// returns the socket input stream as a string
    // - returns up to 64KB from the OS or TLS buffers within TimeOut
    function SockReceiveString(NetResult: PNetResult = nil;
      RawError: system.PInteger = nil): RawByteString;
    /// fill the Buffer with Length bytes
    // - use TimeOut milliseconds wait for incoming data
    // - bypass the SockIn^.Buffer
    // - return true on success, or false on any fatal socket error - NetResult^
    // (if not nil) would contain the actual socket error
    // - call Close if the socket is identified as shutdown from the other side
    // - you may optionally set StopBeforeLength = true, then the read bytes count
    // are set in Length, even if not all expected data has been received - in
    // this case, Close method won't be called
    function TrySockRecv(Buffer: pointer; var Length: integer;
      StopBeforeLength: boolean = false; NetResult: PNetResult = nil;
      RawError: system.PInteger = nil): boolean;
    /// faster readln(SockIn^,Line) or simulate it with direct use of Recv(Sock, ..)
    // - just wrap SockInReadLn() with a 16KB buffer (which is enough e.g. with HTTP)
    // - use TimeOut milliseconds wait for incoming data
    // - raise ENetSock exception on socket error
    // - will handle #10 or #13#10 as line delimiter (as normal text content)
    procedure SockRecvLn(out Line: RawUtf8); overload;
    /// faster readln(SockIn^) or simulate it with direct use of Recv(Sock, ..)
    // - just wrap SockInReadLn(Buffer=nil)
    // - use TimeOut milliseconds wait for incoming data
    // - raise ENetSock exception on socket error
    // - any remaining line content is just ignored - as with RTL readln()
    procedure SockRecvLn; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// direct send data through network
    // - raise a ENetSock exception on any error
    // - bypass the SockSend() buffers
    procedure SndLow(P: pointer; Len: integer); overload;
    /// direct send data through network
    // - raise a ENetSock exception on any error
    // - bypass the SockSend() buffers
    // - raw Data is sent directly to OS: no LF/CRLF is appened to the block
    procedure SndLow(const Data: RawByteString); overload;
    /// direct send data through network
    // - return true on success, or false on any fatal socket error - NetResult^
    // (if not nil) would contain the actual socket error
    // - bypass the SockSend() buffers
    function TrySndLow(P: pointer; Len: integer; NetResult: PNetResult = nil;
      RawError: system.PInteger = nil): boolean;
    /// direct accept an new incoming connection on a bound socket
    // - instance should have been setup as a server via a previous Bind() call
    // - returns nil on error or a ResultClass instance on success
    // - if ResultClass is nil, will return a plain TCrtSocket, but you may
    // specify e.g. THttpServerSocket if you expect incoming HTTP requests
    function AcceptIncoming(ResultClass: TCrtSocketClass = nil;
      Async: boolean = false): TCrtSocket;
    /// the IP address of the other endpoint of this connection
    // - after OpenBind/ConnectUri, is the TNetAddr.IP as returned by
    // NewSocket(), i.e. the IP of the server
    // - after AcceptRequest(), is either the raw IP of the client socket, or
    // a custom header value set by a local proxy as retrieved by inherited
    // THttpServerSocket.GetRequest, searching the header named in
    // THttpServerGeneric.RemoteIPHeader (e.g. 'X-Real-IP' for nginx)
    // - with SocketLayer = nlUdp, InputSock will put here the 'ip:port' of the
    // received packet during SocketIn^ process
    // - equals '' for localhost/127.0.0.1
    property RemoteIP: RawUtf8
      read fRemoteIP write fRemoteIP;
    /// the full requested URI, as specified to OpenUri() constructor
    property OpenUriFull: RawUtf8
      read fOpenUriFull;
    /// compute a TStream compatible class instance from this (secured) socket
    // - return nil if SockIsDefined is false, or a new TSocketStream instance
    // which should be owned and released by the caller, while keeping this
    // TCrtSocket instance available
    // - see TCrtSocketStream if you just want to encapsulate TCrtSocket calls
    function AsSocketStream: TSocketStream;
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
    /// low-level socket handle as pointer, initialized after Open() with socket
    property Sock: TNetSocket
      read fSock write fSock;
    /// low-level access to the TLS layer implementation class
    // - may be using OpenSSL or the SChannel API
    property Secure: INetTls
      read fSecure;
    /// after CreateSockIn, use e.g. Readln(SockIn^,s) to read a line of text
    // - consider the faster SockRecvLn/SockInReadLn methods which will still
    // benefit from SockIn^.Buffer but with no ioresult overhead
    property SockIn: PTextFile
      read fSockIn;
    /// equals true when the Abort method has been called
    // - could be used to abort any blocking process ASAP
    property Aborted: boolean
      read GetAborted;
  published
    /// low-level socket type, initialized after Open() with socket
    property SocketLayer: TNetLayer
      read fSocketLayer;
    /// low-level socket family, initialized after Open() with socket
    property SocketFamily: TNetFamily
      read fSocketFamily;
    /// IP address, initialized after Open() with Server name
    property Server: RawUtf8
      read fServer;
    /// IP port, initialized after Open() with port number
    property Port: RawUtf8
      read fPort;
    /// contains Sock, but transtyped as number for log display or low-level API
    property RawSocket: PtrInt
      read GetRawSocket;
    /// HTTP Proxy URI used for tunnelling, from Tunnel.Server/Port values
    property ProxyUrl: RawUtf8
      read fProxyUrl;
    /// if higher than 0, read loop will wait for incoming data till
    // TimeOut milliseconds (default value is 10000) - used also in SockSend()
    property TimeOut: integer
      read fTimeOut;
    /// total bytes received
    property BytesIn: Int64
      read fBytesIn write fBytesIn;
    /// total bytes sent
    property BytesOut: Int64
      read fBytesOut write fBytesOut;
  end;
  {$M-}

  /// encapsulate TCrtSocket process as a TStream class
  // - directly redirect Read/Write to TCrtSocket.SockRecv/SockSend methods
    // - this class will report fake increasing Size = Position after Read/Write
  // - see TSocketStream if you prefer raw TNetSocket/INetTls support
  TCrtSocketStream = class(TSocketStreamAbstract)
  protected
    fSocket: TCrtSocket;
  public
    /// initialize this TStream for a given TCrtSocket instance
    // - this class instance won't own nor release this TCrtSocket once done
    constructor Create(aSocket: TCrtSocket); reintroduce;
    /// finalize this TStream instance, eventually calling Owned.Free
    destructor Destroy; override;
    /// receive some bytes calling the associated TCrtSocket.SockRecv()
    // - returns the number of bytes filled into Buffer (<=Count)
    function Read(var Buffer; Count: Longint): Longint; override;
    /// send some data calling the associated TCrtSocket.SockSend()
    function Write(const Buffer; Count: Longint): Longint; override;
  published
    /// access to the underlying TCrtSocket instance
    property Socket: TCrtSocket
      read fSocket;
  end;


/// create a TCrtSocket instance, returning nil on error
// - useful to easily catch any exception, and provide a custom TNetTlsContext
// - aTunnel could be populated from mormot.net.client GetSystemProxyUri()
function SocketOpen(const aServer, aPort: RawUtf8; aTLS: boolean = false;
  aTLSContext: PNetTlsContext = nil; aTunnel: PUri = nil;
  aTLSIgnoreCertError: boolean = false): TCrtSocket;

var
  /// maximum chunk size for each TCrtSocket.TrySndLow/TrySockRecv syscall
  // - 256KB seems fair enough, safer and not slower in practice
  CrtSocketSendRecvMaxBytes: PtrInt = 256 shl 10;


{ ********* NTP / SNTP Protocol Client }

const
  /// Google provides a global NTP/SNTP servers cloud
  NTP_DEFAULT_SERVER = 'time.google.com';
  /// the default port for NTP servers
  NTP_DEFAULT_PORT = '123';

type
  /// additional information retrieved from a NTP server via GetNtpTime()
  TNtpInfo = record
    /// UTC timestamp of the remote NTP server - i.e. the GetNtpTime() result
    Time: TDateTime;
    /// delay (in seconds) between your computer and the remote NTP server
    // - i.e. the transmission time, similar to the network ping to this server
    Delay: double;
    /// offset (in seconds) between your computer and the remote NTP server
    // - i.e. the error between both clocks
    Offset: double;
  end;
  /// used as optional parameter to GetNtpTime()
  PNtpInfo = ^TNtpInfo;

/// retrieve the UTC time from a given NTP server
// - returns 0 on failure, or the UTC server timestamp
// - this function takes into account the transmission delay
// - could optionally return additional information about the request
// - note: most servers don't like to be called several times in a raw
function GetNtpTime(const aServer: RawUtf8 = NTP_DEFAULT_SERVER;
  const aPort: RawUtf8 = NTP_DEFAULT_PORT; aTimeOutMS: integer = 400;
  aInfo: PNtpInfo = nil): TDateTime;

/// retrieve the UTC time from a given SNTP server
// - SNTP is a sub-set of the NTP protocol, with potentially less accuracy
// - returns 0 on failure, or the UTC server timestamp
// - note: most servers don't like to be called several times in a raw
function GetSntpTime(const aServer: RawUtf8 = NTP_DEFAULT_SERVER;
  const aPort: RawUtf8 = NTP_DEFAULT_PORT; aTimeOutMS: integer = 400): TDateTime;




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

function NetErrorFromSystem(SystemError, AnotherNonFatal: integer): TNetResult;
begin
  case SystemError of
    NO_ERROR:
      result := nrOK;
    {$ifdef OSWINDOWS}
    WSAETIMEDOUT,
    WSAEWOULDBLOCK,
    {$else}
    ESysEINTR,
    {$endif OSWINDOWS}
    WSAIOPENDING,
    WSAEINPROGRESS,
    WSATRY_AGAIN:
      result := nrRetry;
    WSAEINVAL:
      result := nrInvalidParameter;
    WSAEMFILE:
      result := nrTooManyConnections;
    WSAECONNREFUSED:
      result := nrRefused;
    {$ifdef OSPOSIX}
    ESysEPIPE,
    {$endif OSPOSIX}
    WSAECONNRESET,
    WSAECONNABORTED:
      result := nrClosed;
  else
    if SystemError = AnotherNonFatal then
      result := nrRetry
    else
      result := nrFatalError;
  end;
end;

function NetLastError(AnotherNonFatal: integer; Error: system.PInteger): TNetResult;
var
  err: integer;
begin
  err := RawSocketErrNo;
  if Error <> nil then
    Error^ := err;
  result := NetErrorFromSystem(err, AnotherNonFatal);
end;

function NetLastErrorMsg(AnotherNonFatal: integer): ShortString;
var
  nr: TNetResult;
  err: integer;
begin
  nr := NetLastError(AnotherNonFatal, @err);
  result := _NR[nr];
  if err <> 0 then
    OsErrorAppend(err, result, ' ');
end;

function NetCheck(res: integer): TNetResult;
  {$ifdef HASINLINE}inline;{$endif}
begin
  if res = NO_ERROR then
    result := nrOK
  else
    result := NetLastError;
end;

function ToText(res: TNetResult): PShortString;
begin
  result := @_NR[res]; // no mormot.core.rtti.pas involved
end;

function NetEventsToNetResult(ev: TNetEvents): TNetResult;
begin
  if ev * [neRead, neWrite] <> [] then
    result := nrOk // [neRead/neWrite + neClosed] returns nrOk
  else if ev = [] then
    result := nrRetry
  else if neClosed in ev then
    result := nrClosed
  else
    result := nrFatalError;
end;


{ ENetSock }

constructor ENetSock.Create(msg: string; obj: TObject;
  const args: array of const; error: TNetResult; errnumber: system.PInteger);
begin
  if obj <> nil then
    msg := format('%s.%s', [ClassNameShort(obj)^, msg]);
  if error <> nrOK then
  begin
    fLastError := error;
    msg := format('%s [#%d %s]', [msg, ord(error), _NR[error]]);
    if (errnumber <> nil) and
       (error <> nrTimeout) and
       (errnumber^ <> NO_ERROR) then
      msg := format('%s sys=%d (%s)', [msg, errnumber^, GetErrorShort(errnumber^)]);
  end
  else
    fLastError := nrUnknownError;
  inherited CreateFmt(msg, args);
end;

constructor ENetSock.CreateLastError(const msg: string; const args: array of const;
  error: TNetResult);
var
  err: integer;
begin
  if error in [nrOk, nrUnknownError] then
    error := NetLastError(NO_ERROR, @err)
  else
    err := NO_ERROR;
  Create(msg, nil, args, error, @err);
end;

class procedure ENetSock.Check(res: TNetResult; const context: ShortString;
  errnumber: system.PInteger);
begin
  if (res <> nrOK) and
     (res <> nrRetry) then
    raise Create('%s failed', nil, [context], res, errnumber);
end;

class procedure ENetSock.CheckLastError(const Context: ShortString;
  ForceRaise: boolean; AnotherNonFatal: integer);
var
  res: TNetResult;
  err: integer;
begin
  res := NetLastError(AnotherNonFatal, @err);
  if ForceRaise and
     (res in [nrOK, nrRetry]) then
    res := nrUnknownError;
  Check(res, Context, @err);
end;



{ ******** TNetAddr Cross-Platform Wrapper }

{ TNetHostCache }

type
  // implement a thread-safe cache of IPv4 for hostnames
  // - used e.g. by TNetAddr.SetFromIP4 and GetKnownHost
  // - avoid the overhead of TSynDictionary for a few short-living items
  {$ifdef USERECORDWITHMETHODS}
  TNetHostCache = record
  {$else}
  TNetHostCache = object
  {$endif USERECORDWITHMETHODS}
  public
    Safe: TLightLock;
    Host: TRawUtf8DynArray;
    Tix, TixShr: cardinal;
    Count, Capacity: integer;
    IP: TCardinalDynArray;
    function TixDeprecated: boolean;
    procedure Add(const hostname: RawUtf8; ip4: cardinal);
    procedure AddFrom(const other: TNetHostCache);
    function Find(const hostname: RawUtf8; out ip4: cardinal): boolean;
    procedure SafeAdd(const hostname: RawUtf8; ip4, deprec: cardinal);
    function SafeFind(const hostname: RawUtf8; out ip4: cardinal): boolean;
    procedure SafeFlush(const hostname: RawUtf8);
  end;

function TNetHostCache.TixDeprecated: boolean;
var
  tix32: cardinal;
begin
  if TixShr = 0 then
    TixShr := 13; // refresh every 8192 ms by default
  tix32 := mormot.core.os.GetTickCount64 shr TixShr;
  result := tix32 <> Tix;
  if result then
    Tix := tix32;
end;

procedure TNetHostCache.Add(const hostname: RawUtf8; ip4: cardinal);
begin
  if hostname = '' then
    exit;
  if Capacity = Count then
  begin
    Capacity := NextGrow(Capacity);
    SetLength(Host, Capacity);
    SetLength(IP, Capacity);
  end;
  Host[Count] := hostname;
  IP[Count] := ip4;
  inc(Count);
end;

procedure TNetHostCache.AddFrom(const other: TNetHostCache);
var
  i: PtrInt;
begin
  for i := 0 to other.Count - 1 do
    Add(other.Host[i], other.IP[i]);
end;

function TNetHostCache.Find(const hostname: RawUtf8; out ip4: cardinal): boolean;
var
  i: PtrInt;
begin
  result := false;
  if (Count = 0) or
     (hostname = '') then
    exit;
  i := FindPropName(pointer(Host), hostname, Count); // case insensitive lookup
  if i < 0 then
    exit;
  ip4 := IP[i];
  result := true;
end;

procedure TNetHostCache.SafeAdd(const hostname: RawUtf8; ip4, deprec: cardinal);
begin
  Safe.Lock;
  if deprec <> 0 then
  begin
    TixShr := deprec; // may override e.g. to 15, i.e. 32768 ms cache
    if TixDeprecated then // flush any previous entry if needed
      Count := 0;
  end;
  Add(hostname, ip4);
  Safe.UnLock;
end;

function TNetHostCache.SafeFind(const hostname: RawUtf8; out ip4: cardinal): boolean;
begin
  result := false;
  if Count = 0 then
    exit;
  Safe.Lock;
  if TixDeprecated then
    Count := 0
  else
    result := Find(hostname, ip4);
  Safe.UnLock;
end;

procedure TNetHostCache.SafeFlush(const hostname: RawUtf8);
var
  i, n: PtrInt;
begin
  if (Count = 0) or
     (hostname = '') then
    exit;
  Safe.Lock;
  try
    if TixDeprecated then
      Count := 0
    else
    begin
      i := FindPropName(pointer(Host), hostname, Count);
      if i < 0 then
        exit; // case insensitive Host not found
      n := Count - 1;
      Count := n;
      Host[i] := '';
      dec(n, i);
      if n <= 0 then
        exit;
      MoveFast(pointer(Host[i + 1]), pointer(Host[i]), n * SizeOf(pointer));
      MoveFast(IP[i + 1], IP[i], n * SizeOf(cardinal));
      PtrUInt(Host[Count]) := 0; // avoid GPF
    end;
  finally
    Safe.UnLock;
  end;
end;


{ TNetAddr }

var
  NetAddrCache: TNetHostCache; // small internal cache valid for 32 seconds only

procedure NetAddrFlush(const hostname: RawUtf8);
begin
  NetAddrCache.SafeFlush(hostname);
end;

function NetAddrResolve(const hostname: RawUtf8): RawUtf8;
var
  addr: TNetAddr;
begin
  result := '';
  if addr.SetFrom(hostname, '80', nlTcp) = nrOK then
    addr.IP(result);
end;

function TNetAddr.SetFromIP4(const address: RawUtf8;
  noNewSocketIP4Lookup: boolean): boolean;
var
  ad4: sockaddr absolute Addr;
begin
  // allow to bind to any IPv6 address
  if address = c6AnyHost then // ::
  begin
    ad4.sin_family := AF_INET6; 
    FillZero(PHash128(@PSockAddrIn6(@Addr)^.sin6_addr)^); // all sin6_addr[] = 0
    result := true;
    exit;
  end;
  result := false;
  ad4.sin_family := 0; // keep sin_port
  ad4.sin_addr.s_addr := 0; // reset
  PInt64(@ad4.sin_zero)^ := 0; // seems mandatory on Windows
  if (address = cLocalhost) or
     (address = c6Localhost) or // ::1
     PropNameEquals(address, 'localhost') then
    ad4.sin_addr.s_addr := cLocalhost32 // 127.0.0.1
  else if (address = cBroadcast) or
          (address = c6Broadcast) then
    ad4.sin_addr.s_addr := cardinal(-1) // 255.255.255.255
  else if address = cAnyHost then
    // keep 0.0.0.0 for bind - but connect would redirect to 127.0.0.1
  else if NetIsIP4(pointer(address), @ad4.sin_addr) or
          GetKnownHost(address, ad4.sin_addr.s_addr) or
          NetAddrCache.SafeFind(address, ad4.sin_addr.s_addr) then
    // numerical IPv4, /etc/hosts, or cached entry
  else if (Assigned(NewSocketIP4Lookup) and
          not noNewSocketIP4Lookup and
          NewSocketIP4Lookup(address, ad4.sin_addr.s_addr)) then
    // cache value found from mormot.net.dns lookup for 1 shl 15 = 32 seconds
    NetAddrCache.SafeAdd(address, ad4.sin_addr.s_addr, {tixshr=}15)
  else
    // return result=false if unknown
    exit;
  // we found the IPv4 matching this address
  ad4.sin_family := AF_INET;
  result := true;
end;

function TNetAddr.Family: TNetFamily;
var
  ad4: sockaddr absolute Addr;
begin
  case ad4.sa_family of
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

procedure TNetAddr.IP(var res: RawUtf8; localasvoid: boolean);
var
  ad4: sockaddr absolute Addr;
begin
  res := '';
  case ad4.sa_family of
    AF_INET:
      if (not localasvoid) or
         (ad4.sin_addr.s_addr <> cLocalhost32) then
        IP4Text(@ad4.sin_addr, res); // detect 0.0.0.0 and 127.0.0.1
    AF_INET6:
      begin
        IP6Text(@PSockAddrIn6(@Addr)^.sin6_addr, res); // detect :: and ::1
        if localasvoid and
           (pointer(res) = pointer(IP4local)) then
          res := '';
      end;
    {$ifdef OSPOSIX}
    AF_UNIX:
        if not localasvoid then
          res := IP4local; // by definition, unix sockets are local
    {$endif OSPOSIX}
  end;
end;

function TNetAddr.IP(localasvoid: boolean): RawUtf8;
begin
  IP(result, localasvoid);
end;

function TNetAddr.IP4: cardinal;
var
  ad4: sockaddr absolute Addr;
begin
  if ad4.sa_family = AF_INET then
    result := ad4.sin_addr.s_addr // may be cLocalhost32
  else
    result := 0; // AF_INET6 or AF_UNIX return 0
end;

function TNetAddr.IP4Short: TShort16;
var
  ad4: sockaddr absolute Addr;
begin
  if ad4.sa_family = AF_INET then
    mormot.net.sock.IP4Short(@ad4.sin_addr, result)
  else
    result[0] := #0; // AF_INET6 or AF_UNIX return ''
end;

function TNetAddr.IPShort(withport: boolean): ShortString;
begin
  IPShort(result, withport);
end;

procedure TNetAddr.IPShort(out result: ShortString; withport: boolean);
var
  ad4: sockaddr absolute Addr;
begin
  result[0] := #0;
  case ad4.sa_family of
    AF_INET:
      mormot.net.sock.IP4Short(@ad4.sin_addr, result);
    AF_INET6:
      IP6Short(@PSockAddrIn6(@Addr)^.sin6_addr, result);
    {$ifdef OSPOSIX}
    AF_UNIX:
      with psockaddr_un(@Addr)^ do
      begin
        SetString(result, PAnsiChar(@sun_path), mormot.core.base.StrLen(@sun_path));
        exit; // no port
      end;
    {$endif OSPOSIX}
  else
    exit;
  end;
  if not withport then
    exit;
  AppendShortChar(':', @result);
  AppendShortCardinal(Port, result);
end;

procedure TNetAddr.IPWithPort(var Text: RawUtf8);
var
  tmp: ShortString;
begin
  IPShort(tmp, {withport=}true);
  ShortStringToAnsi7String(tmp, Text);
end;

function TNetAddr.IPWithPort: RawUtf8;
begin
  IPWithPort(result);
end;

function TNetAddr.Port: TNetPort;
var
  ad4: sockaddr absolute Addr;
begin
  if ad4.sa_family in [AF_INET, AF_INET6] then
    result := bswap16(ad4.sin_port)
  else
    result := 0;
end;

function TNetAddr.SetPort(p: TNetPort): TNetResult;
var
  ad4: sockaddr absolute Addr;
begin
  if (ad4.sa_family in [AF_INET, AF_INET6]) and
     (p <= 65535) then // p may equal 0 to set ephemeral port
  begin
    ad4.sin_port := bswap16(p);
    result := nrOk;
  end
  else
    result := nrNotFound;
end;

function TNetAddr.SetIP4Port(ipv4: cardinal; netport: TNetPort): TNetResult;
var
  ad4: sockaddr absolute Addr;
begin
  ad4.sin_family := AF_INET;
  ad4.sin_addr.s_addr := ipv4;
  PInt64(@ad4.sin_zero)^ := 0; // seems needed on Windows
  ad4.sin_port := bswap16(netport);
  if netport > 65535 then
    result := nrNotFound
  else
    result := nrOk;
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

function TNetAddr.IPEqual(const another: TNetAddr): boolean;
begin
  case PSockAddr(@Addr)^.sa_family of
    AF_INET:
      result := PSockAddr(@Addr)^.sin_addr.s_addr =
                PSockAddr(@another)^.sin_addr.s_addr;
    AF_INET6:
      result := (PHash128Rec(@PSockAddrIn6(@Addr)^.sin6_addr).Lo =
                 PHash128Rec(@PSockAddrIn6(@another)^.sin6_addr).Lo) and
                (PHash128Rec(@PSockAddrIn6(@Addr)^.sin6_addr).Hi =
                 PHash128Rec(@PSockAddrIn6(@another)^.sin6_addr).Hi);
  else
    result := false; // nlUnix has no IP
  end;
end;

function TNetAddr.NewSocket(layer: TNetLayer): TNetSocket;
var
  s: TSocket;
begin
  s := socket(PSockAddr(@Addr)^.sa_family, _ST[layer], _IP[layer]);
  if s <= 0 then
    result := nil
  else
    result := TNetSocket(s);
end;

function TNetAddr.SocketConnect(socket: TNetSocket; ms: integer): TNetResult;
var
  tix: Int64;
begin
  result := socket.MakeAsync;
  if result <> nrOK then
    exit;
  if connect(socket.Socket, @Addr, Size) = 0 then // non-blocking connect() once
    exit; // immediate success (unlikely)
  if ms < 0 then
    exit; // don't wait now
  result := NetLastError;
  if result <> nrRetry then
    exit; // abort on fatal error (e.g. invalid address)
  result := socket.MakeBlocking;
  if result <> nrOK then
    exit;
  if ms < 50 then
    tix := 0
  else
  begin
    tix := mormot.core.os.GetTickCount64 + ms;
    ms := 50;
  end;
  repeat
    result := NetEventsToNetResult(socket.WaitFor(ms, [neWrite, neError]));
    if result <> nrRetry then
      exit;
    // typically, status = [] for TRY_AGAIN result
    SleepHiRes(1); // paranoid to avoid buring CPU if WaitFor() doesn't wait
  until (tix = 0) or
        (mormot.core.os.GetTickCount64 > tix);
  result := nrTimeout;
end;

function TNetAddr.SocketBind(socket: TNetSocket): TNetResult;
begin
  if socket = nil then
    result := nrNoSocket
  else
    result := NetCheck(bind(socket.Socket, @Addr, Size));
end;


{ ******** TNetSocket Cross-Platform Wrapper }

function GetSocketAddressFromCache(const address, port: RawUtf8; layer: TNetLayer;
  out addr: TNetAddr; var fromcache, tobecached: boolean): TNetResult;
var
  p, ip4: cardinal;
begin
  fromcache := false;
  tobecached := false;
  if layer = nlUnix then
    result := addr.SetFrom(address, '', nlUnix)
  else if not ToCardinal(port, p, {minimal=}1) or
          ({%H-}p > 65535) then
    result := nrNotFound // port should be valid
  else if (address = '') or
          (address = cLocalhost) or
          (address = c6Localhost) or
          PropNameEquals(address, 'localhost') or
          (address = cAnyHost) then // for client: '0.0.0.0' -> '127.0.0.1'
    result := addr.SetIP4Port(cLocalhost32, p)
  else if NetIsIP4(pointer(address), @ip4) then
    result := addr.SetIP4Port(ip4, p) // from IPv4 '1.2.3.4"
  else
  begin
    if Assigned(NewSocketAddressCache) then
      if NewSocketAddressCache.Search(address, addr) then
      begin
        fromcache := true;
        result := addr.SetPort(p); // from cache
        exit;
      end
      else
        tobecached := true;
    result := addr.SetFrom(address, port, layer); // actual DNS resolution
  end;
end;

function ExistSocketAddressFromCache(const host: RawUtf8): boolean;
var
  addr: TNetAddr;
  fromcache, tobecached: boolean;
begin
  result := GetSocketAddressFromCache(
    host, '7777', nlTcp, addr, fromcache, tobecached) = nrOK;
  if result and
     tobecached then
    NewSocketAddressCache.Add(host, addr);
end;

function GetReachableNetAddr(const address, port: array of RawUtf8;
  timeoutms, neededcount: integer; sockets: PNetSocketDynArray): TNetAddrDynArray;
var
  i, n: PtrInt;
  s: TNetSocket;
  sock: TNetSocketDynArray;
  addr: TNetAddrDynArray;
  res: TNetResult;
  tix: Int64;
begin
  result := nil;
  if sockets <> nil then
    sockets^ := nil;
  if neededcount <= 0 then
    exit;
  n := length(address);
  if (n = 0) or
     (length(port) <> n) then
    exit;
  SetLength(sock, n);
  SetLength(addr, n);
  n := 0;
  for i := 0 to length(sock) - 1 do
  begin
    res := addr[n].SetFrom(address[i], port[i], nlTcp); // bypass DNS cache here
    if res <> nrOK then
      continue;
    s := addr[n].NewSocket(nlTcp);
    if (s = nil) or
       (s.MakeAsync <> nrOk) then
      continue;
    connect(s.Socket, @addr[n], addr[n].Size); // non-blocking connect() once
    if s.MakeBlocking <> nrOk then
    begin
      closesocket(s.Socket); // release handle
      continue;
    end;
    sock[n] := s;
    inc(n);
  end;
  if n = 0 then
    exit;
  if neededcount > n then
    neededcount := n;
  SetLength(result, n);
  if sockets <> nil then
    SetLength(sockets^, n);
  n := 0;
  tix := mormot.core.os.GetTickCount64 + timeoutms;
  repeat
    for i := 0 to length(result) - 1 do
      if (sock[i] <> nil) and
         (neWrite in sock[i].WaitFor(1, [neWrite, neError])) then
      begin
        if sockets = nil then
          sock[i].ShutdownAndClose(false)
        else
          sockets^[n] := sock[i]; // let caller own this socket from now on
        sock[i] := nil; // mark this socket as closed
        result[n] := addr[i];
        inc(n);
        dec(neededcount);
        if neededcount = 0 then
          break;
      end;
  until (neededcount = 0) or
        (mormot.core.os.GetTickCount64 > tix);
  if n <> length(result) then
  begin
    for i := 0 to length(result) - 1 do
      if sock[i] <> nil then
        sock[i].ShutdownAndClose(false);
    SetLength(result, n);
    if sockets <> nil then
      SetLength(sockets^, n);
  end;
end;

function NewSocket(const address, port: RawUtf8; layer: TNetLayer;
  dobind: boolean; connecttimeout, sendtimeout, recvtimeout, retry: integer;
  out netsocket: TNetSocket; netaddr: PNetAddr; bindReusePort: boolean): TNetResult;
var
  addr: TNetAddr;
  sock: TNetSocket;
  fromcache, tobecached: boolean;
begin
  netsocket := nil;
  // resolve the TNetAddr of the address:port layer - maybe from cache
  fromcache := false;
  tobecached := false;
  PInteger(@addr)^ := 0; // rough init - enough for addr.IP() = ''
  if dobind then
    result := addr.SetFrom(address, port, layer)
  else
    result := GetSocketAddressFromCache(
      address, port, layer, addr, fromcache, tobecached);
  if result <> nrOK then
    exit;
  // create the raw Socket instance
  sock := addr.NewSocket(layer);
  if sock = nil then
  begin
    result := NetLastError(WSAEADDRNOTAVAIL);
    if fromcache then
    begin
      // force call the DNS resolver again, perhaps load-balacing is needed
      NewSocketAddressCache.Flush(address);
      NetAddrCache.SafeFlush(address);
    end;
    exit;
  end;
  // bind or connect to this Socket
  // - note: no sock.SetRecvBufferSize/SetSendBufferSize call on Windows,
  // because the OS is already guessing the best values at runtime
  if (connecttimeout > 0) and
     // non-blocking connect() if a timeout was specified
     not dobind then
    // SetReceiveTimeout/SetSendTimeout don't apply to connect() -> async
    result := addr.SocketConnect(sock, connecttimeout)
  else
  repeat
    // bind, or no timeout specified: just call the OS bind() or connect()
    if dobind then
    begin
      // bound Socket should remain open for 5 seconds after a closesocket()
      if layer <> nlUdp then
        sock.SetLinger(5);
      if (layer in [nlTcp, nlUdp]) and
         bindReusePort then
        sock.ReusePort;
      // Server-side binding/listening of the socket to the address:port
      if (bind(sock.Socket, @addr, addr.Size) <> NO_ERROR) or
         ((layer <> nlUdp) and
          (listen(sock.Socket, DefaultListenBacklog) <> NO_ERROR)) then
        result := NetLastError(WSAEADDRNOTAVAIL);
    end
    else
      // open blocking Client connection (use system-defined timeout)
      if connect(sock.Socket, @addr, addr.Size) <> NO_ERROR then
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
    closesocket(sock.Socket);
    if fromcache then
      // ensure the cache won't contain this faulty address any more
      NewSocketAddressCache.Flush(address);
    exit;
  end;
  // Socket is successfully connected -> setup the connection
  if tobecached then
    // update cache once we are sure the host actually exists
    NewSocketAddressCache.Add(address, addr);
  netsocket := sock;
  netsocket.SetupConnection(layer, sendtimeout, recvtimeout);
  if netaddr <> nil then
    if (addr.Port <> 0) or                   // 0 = assigned by the OS
       (sock.GetName(netaddr^) <> nrOk) then // retrieve ephemeral port
      MoveFast(addr, netaddr^, addr.Size);
end;

function NewRawSocket(family: TNetFamily; layer: TNetLayer): TNetSocket;
var
  s: TSocket;
begin
  s := socket(_SF[family], _ST[layer], _IP[layer]);
  if s <= 0 then
    result := nil
  else
    result := TNetSocket(s);
end;

function NewRawSockets(family: TNetFamily; layer: TNetLayer;
  count: integer): TNetSocketDynArray;
var
  i: PtrInt;
  s: TSocket;
begin
  SetLength(result, count);
  for i := 0 to count - 1 do
  begin
    s := socket(_SF[family], _ST[layer], _IP[layer]);
    if s <= 0 then
      ENetSock.CheckLastError('NewNetSockets', {forceraise=}true);
    result[i] := TNetSocket(s);
  end;
end;


{ TNetSocketWrap }

procedure TNetSocketWrap.SetOpt(prot, name: integer;
  value: pointer; valuelen: integer);
begin
  if @self = nil then
    raise ENetSock.Create('SetOptions(%d,%d) with no socket', nil, [prot, name]);
  if setsockopt(TSocket(@self), prot, name, value, valuelen) <> NO_ERROR then
    raise ENetSock.CreateLastError('SetOptions(%d,%d)', [prot, name]);
end;

function TNetSocketWrap.GetOptInt(prot, name: integer): integer;
var
  len: integer;
begin
  if @self = nil then
    raise ENetSock.Create('GetOptInt(%d,%d) with no socket', nil, [prot, name]);
  result := 0;
  len := SizeOf(result);
  if getsockopt(TSocket(@self), prot, name, @result, @len) <> NO_ERROR then
    raise ENetSock.CreateLastError('GetOptInt(%d,%d)', [prot, name]);
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
  // no value should be set on Windows, because it is better adjusted at runtime
end;

procedure TNetSocketWrap.SetRecvBufferSize(bytes: integer);
begin
  SetOpt(SOL_SOCKET, SO_RCVBUF, @bytes, SizeOf(bytes));
  // no value should be set on Windows, because it is better adjusted at runtime
end;

function TNetSocketWrap.GetSendBufferSize: integer;
begin
  result := GetOptInt(SOL_SOCKET, SO_SNDBUF);
  // on Linux , typical value is 2626560 bytes for TCP (16384 for accept),
  //   and 212992 for Unix socket
  // on Windows, default is 8192 but it is only an initial informative value,
  //   and do not reflect any future runtime adjustment
end;

function TNetSocketWrap.GetRecvBufferSize: integer;
begin
  result := GetOptInt(SOL_SOCKET, SO_RCVBUF);
  // on Linux, typical value is 131072 bytes for TCP, 212992 for Unix socket
  // on Windows, default is 8192 but it is only an initial informative value
end;

procedure TNetSocketWrap.SetBroadcast(broadcast: boolean);
var
  v: integer;
begin
  v := ord(broadcast);
  SetOpt(SOL_SOCKET, SO_BROADCAST, @v, SizeOf(v));
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

function TNetSocketWrap.Accept(out clientsocket: TNetSocket;
  out addr: TNetAddr; async: boolean): TNetResult;
var
  sock: TSocket;
begin
  if @self = nil then
    result := nrNoSocket
  else
  begin
    sock := doaccept(TSocket(@self), @addr, async);
    if sock = -1 then
    begin
      result := NetLastError;
      if result = nrOk then
        result := nrNotImplemented;
    end
    else
    begin
      clientsocket := TNetSocket(sock);
      // note: no sock.SetRecvBufferSize/SetSendBufferSize call on Windows,
      // because the OS is already guessing the best values at runtime
      if async then // doaccept() may have set async=false with accept4()
        result := clientsocket.MakeAsync
      else
        result := nrOK;
    end;
  end;
end;

function TNetSocketWrap.GetName(out addr: TNetAddr): TNetResult;
var
  len: TSockLen;
begin
  addr.Clear;
  if @self = nil then
    result := nrNoSocket
  else
  begin
    len := SizeOf(addr);
    result := NetCheck(getsockname(TSocket(@self), @addr, len));
  end;
end;

function TNetSocketWrap.GetIP(out ip: RawUtf8; withport: boolean): TNetResult;
var
  addr: TNetAddr;
begin
  result := GetName(addr);
  if result = nrOK then
    ShortStringToAnsi7String(addr.IPShort(withport), ip);
end;

function TNetSocketWrap.GetPeer(out addr: TNetAddr): TNetResult;
var
  len: TSockLen;
begin
  addr.Clear;
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

function TNetSocketWrap.Send(Buf: pointer; var len: integer;
  rawError: system.PInteger): TNetResult;
begin
  if @self = nil then
    result := nrNoSocket
  else
  begin
    len := mormot.net.sock.send(TSocket(@self), Buf, len, MSG_NOSIGNAL);
    // man send: Upon success, send() returns the number of bytes sent.
    // Otherwise, -1 is returned and errno set to indicate the error.
    if len < 0 then
      result := NetLastError(NO_ERROR, rawError)
    else
      result := nrOK;
  end;
end;

function TNetSocketWrap.Recv(Buf: pointer; var len: integer;
  rawError: system.PInteger): TNetResult;
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
        result := NetLastError(NO_ERROR, rawError)
    else
      result := nrOK;
  end;
end;

function TNetSocketWrap.SendTo(Buf: pointer; len: integer;
  const addr: TNetAddr): TNetResult;
begin
  if @self = nil then
    result := nrNoSocket
  else if mormot.net.sock.sendto(
            TSocket(@self), Buf, len, 0, @addr, addr.Size) < 0 then
    result := NetLastError
  else
    result := nrOk;
end;

function TNetSocketWrap.RecvFrom(Buf: pointer; len: integer;
  out addr: TNetAddr): integer;
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
  pending := 0;
  if @self = nil then
    result := nrNoSocket
  else
    result := NetCheck(ioctlsocket(TSocket(@self), FIONREAD, @pending));
end;

function TNetSocketWrap.HasData: integer;
begin
  if RecvPending(result) <> nrOk then
    result := 0;
end;

function TNetSocketWrap.RecvWait(ms: integer;
  out data: RawByteString; terminated: PTerminated): TNetResult;
var
  read: integer;
  tmp: TBuffer64K; // use stack buffer to avoid RecvPending() syscall
begin
  result := NetEventsToNetResult(WaitFor(ms, [neRead, neError]));
  if Assigned(terminated) and
     terminated^ then
    result := nrClosed
  else if result = nrOk then
  begin
    read := SizeOf(tmp);
    result := Recv(@tmp, read);
    if Assigned(terminated) and
       terminated^ then
      result := nrClosed;
    if result = nrOK then
      if read <= 0 then
        result := nrUnknownError
      else
        FastSetRawByteString(data, @tmp, read);
  end;
end;

function TNetSocketWrap.SendAll(Buf: PByte; len: integer;
  terminated: PTerminated): TNetResult;
var
  sent: integer;
begin
  repeat
    sent := len;
    result := Send(Buf, sent);
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

function TNetSocketWrap.RecvAll(ms: integer; Buf: PByte; len: integer;
  terminated: PTerminated): TNetResult;
var
  received: integer;
begin
  repeat
    result := NetEventsToNetResult(WaitFor(ms, [neRead, neError]));
    if Assigned(terminated) and
       terminated^ then
      break
    else if result <> nrOK then
      exit;
    received := len;
    result := Recv(Buf, received);
    if Assigned(terminated) and
       terminated^ then
      break;
    if received > 0 then
    begin
      inc(Buf, received);
      dec(len, received);
      if len = 0 then
        exit;
    end;
    if result > nrRetry then
      exit;
  until Assigned(terminated) and
        terminated^;
  result := nrClosed;
end;

function TNetSocketWrap.Available(loerr: system.PInteger): boolean;
var
  events: TNetEvents;
  dummy: integer;
begin
  result := true;
  events := WaitFor(0, [neRead, neError], loerr); // select() or poll()
  if events = [] then
    exit; // the socket seems stable with no pending input
  if neRead in events then
    // - on Windows, may be WSACONNRESET (nrClosed), with recv() returning 0
    // - on POSIX, may be ESysEINPROGRESS (nrRetry) just after connect
    // - no need to MakeAsync: recv() should not block after neRead
    // - may be [neRead, neClosed] on gracefully closed HTTP/1.0 response
    // - expected recv() result: -1=error, 0=closed, 1=success
    if (mormot.net.sock.recv(TSocket(@self), @dummy, 1, MSG_PEEK) = 1) or
       (NetLastError(NO_ERROR, loerr) = nrRetry) then
      exit;
  result := false; // e.g. neError or neClosed with no neRead
end;

procedure TNetSocketWrap.RawShutdown;
begin
  if @self <> nil then
    shutdown(TSocket(@self), SHUT_RDWR);
end;

function TNetSocketWrap.ShutdownAndClose(rdwr: boolean; waitms: integer): TNetResult;
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
    {$ifdef OSWINDOWS}
    if waitms <> 0 then
      // try to close the socket as documented by Microsoft (with rdwr=true)
      // - documented pattern is: shutdown(SD_SEND) + recv()=0 + closesocket
      WaitFor(waitms, [neRead, neError]); // typically neRead = WSACONNRESET
    {$endif OSWINDOWS}
    result := Close; // eventual closesocket()
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


{ ******************** Mac and IP Addresses Support }

const // should be local for better code generation
  HexCharsLower: array[0..15] of AnsiChar = '0123456789abcdef';

function IsPublicIP(ip4: cardinal): boolean;
begin
  result := false;
  case ToByte(ip4) of // ignore IANA private IPv4 address spaces
    10:
      exit;
    172:
      if ToByte(ip4 shr 8) in [16..31] then
        exit;
    192:
      if ToByte(ip4 shr 8) = 168 then
        exit;
  end;
  result := true;
end;

function IsApipaIP(ip4: cardinal): boolean;
begin
  result := (ip4 and $ffff = ord(169) + ord(254) shl 8) and
            (ToByte(ip4 shr 16) < 255);
end;

function IP4Mask(ip4: cardinal): cardinal;
begin
  result := $ffffffff;
  case ToByte(ip4) of // detect IANA private IPv4 address spaces
    10:
      result := $000000ff;
    172:
      if ToByte(ip4 shr 8) in [16..31] then
        result := $0000ffff;
    192:
      if ToByte(ip4 shr 8) = 168 then
        result := $00ffffff;
  end;
end;

function IP4Broadcast(ip4, mask4: cardinal): cardinal;
begin
  // e.g. ip4=172.16.144.160 mask4=255.255.255.0
  result := (ip4 and mask4) {=172.16.144.0} or (not mask4); {=172.16.144.255}
end;

function IP4Netmask(prefix: integer): cardinal;
begin
  if cardinal(prefix - 1) < 32 then // needed for 32-bit ARM
    result := bswap32($ffffffff shl (32 - prefix))
  else
    result := 0;
end;

function IP4Netmask(prefix: integer; out mask: cardinal): boolean;
begin
  mask := IP4Netmask(prefix);
  result := (mask <> 0);
end;

function TIp4SubNet.Match(ip4: cardinal): boolean; // defined here for inlining
begin
  // e.g. ip4=172.16.144.160 subip=172.16.144.0 submask=255.255.255.0
  result := (ip4 and mask) = ip;
end;

function IP4Match(const ip4, subnet: RawUtf8): boolean;
var
  sub: TIp4SubNet;
begin
  result := sub.From(subnet) and
            sub.Match(ip4);
end;

function IP4Prefix(netmask4: cardinal): integer;
begin
  result := GetBitsCountPtrInt(netmask4); // may use SSE4.2 or optimized asm
  if IP4Netmask(result) <> netmask4 then
    result := 0; // invalid netmask
end;

function IP4Prefix(const netmask4: RawUtf8): integer;
var
  mask: cardinal;
begin
  if NetIsIP4(pointer(netmask4), @mask) then
    result := IP4Prefix(mask)
  else
    result := 0;
end;

function IP4Subnet(ip4, netmask4: cardinal): TShort23;
var
  w: integer;
begin
  result[0] := #0;
  w := IP4Prefix(netmask4);
  if w = 0 then
    exit;
  ip4 := ip4 and netmask4;
  IP4Short(@ip4, result);
  AppendShortChar('/', @result);
  AppendShortCardinal(w, result);
end;

function IP4Subnet(const ip4, netmask4: RawUtf8): RawUtf8;
var
  ip, mask: cardinal;
begin
  if NetIsIP4(pointer(ip4), @ip) and
     NetIsIP4(pointer(netmask4), @mask) then
    ShortStringToAnsi7String(IP4Subnet(ip, mask), result)
  else
    result := '';
end;

function IP4Filter(ip4: cardinal; filter: TIPAddress): boolean;
begin
  result := false; // e.g. tiaIPv6 or 0.0.0.0 or 127.0.0.1
  if (ip4 <> $0100007f) and
     (ip4 <> 0) then
    case filter of
      tiaAny,
      tiaIPv4:
        result := true;
      tiaIPv4Public:
        result := IsPublicIP(ip4);
      tiaIPv4Private:
        result := not IsPublicIP(ip4);
      tiaIPv4Dhcp:
        result := not IsApipaIP(ip4);
      tiaIPv4DhcpPublic:
        result := IsPublicIP(ip4) and
                  not IsApipaIP(ip4);
      tiaIPv4DhcpPrivate:
        result := not IsPublicIP(ip4) and
                  not IsApipaIP(ip4);
    end;
end;

procedure IP4Short(ip4addr: PByteArray; var s: TShort16);
begin
  s[0] := #0;
  AppendShortCardinal(ip4addr[0], s);
  AppendShortChar('.', @s);
  AppendShortCardinal(ip4addr[1], s);
  AppendShortChar('.', @s);
  AppendShortCardinal(ip4addr[2], s);
  AppendShortChar('.', @s);
  AppendShortCardinal(ip4addr[3], s);
  PAnsiChar(@s)[ord(s[0]) + 1] := #0; // make #0 terminated (won't hurt)
end;

function IP4ToShort(ip4addr: PByteArray): TShort16;
begin
  IP4Short(ip4addr, result);
end;

procedure IP4Text(ip4addr: PByteArray; var result: RawUtf8);
var
  s: ShortString;
begin
  if PCardinal(ip4addr)^ = 0 then
    // '0.0.0.0' bound to any host -> ''
    result := ''
  else if PCardinal(ip4addr)^ = cLocalhost32 then
    // '127.0.0.1' loopback (no memory allocation)
    result := IP4local
  else
  begin
    IP4Short(ip4addr, s);
    FastSetString(result, @s[1], ord(s[0]));
  end;
end;

function IP4ToText(ip4addr: PByteArray): RawUtf8;
begin
  IP4Text(ip4addr, result);
end;

procedure IP6Short(ip6addr: PByteArray; var s: ShortString);
// this code is faster than any other inet_ntop6() I could find around
var
  i: PtrInt;
  trimlead: boolean;
  c, n: byte;
  p: PAnsiChar;
  zeros, current: record pos, len: ShortInt; end;
  tab: PAnsiChar;
begin
  // find longest run of 0000: for :: shortening
  zeros.pos := -1;
  zeros.len := 0;
  current.pos := -1;
  current.len := 0;
  for i := 0 to 7 do
    if PWordArray(ip6addr)[i] = 0 then
      if current.pos < 0 then
      begin
        current.pos := i;
        current.len := 1;
      end
      else
        inc(current.len)
    else if current.pos >= 0 then
    begin
      if (zeros.pos < 0) or
         (current.len > zeros.len) then
        zeros := current;
      current.pos := -1;
    end;
  if (current.pos >= 0) and
     ((zeros.pos < 0) or
      (current.len > zeros.len)) then
    zeros := current;
  if (zeros.pos >= 0) and
     (zeros.len < 2) then
    zeros.pos := -1;
  // convert to hexa
  p := @s[1];
  tab := @HexCharsLower;
  n := 0;
  repeat
    if n = byte(zeros.pos) then
    begin
      // shorten double zeros to ::
      if n = 0 then
      begin
        p^ := ':';
        inc(p);
      end;
      p^ := ':';
      inc(p);
      ip6addr := @PWordArray(ip6addr)[zeros.len];
      inc(n, zeros.len);
      if n = 8 then
        break;
    end
    else
    begin
      // write up to 4 hexa chars, triming leading 0
      trimlead := true;
      c := ip6addr^[0] shr 4;
      if c <> 0 then
      begin
        p^ := tab[c];
        inc(p);
        trimlead := false;
      end;
      c := ip6addr^[0]; // in two steps for FPC
      c := c and 15;
      if ((c <> 0) and trimlead) or
         not trimlead then
      begin
        p^ := tab[c];
        inc(p);
        trimlead := false;
      end;
      c := ip6addr^[1] shr 4;
      if ((c <> 0) and trimlead) or
         not trimlead then
      begin
        p^ := tab[c];
        inc(p);
      end;
      c := ip6addr^[1];
      c := c and 15; // last hexa char is always there
      p^ := tab[c];
      inc(p);
      inc(PWord(ip6addr));
      inc(n);
      if n = 8 then
        break;
      p^ := ':';
      inc(p);
    end;
  until false;
  p^ := #0; // make null-terminated (won't hurt)
  s[0] := AnsiChar(p - @s[1]);
end;

procedure IP6Text(ip6addr: PByteArray; var result: RawUtf8);
var
  s: ShortString;
begin
  if (PInt64(ip6addr)^ = 0) and
     (PInt64(@ip6addr[7])^ = 0) then // start with 15 zeros?
    case ip6addr[15] of
      0: // IPv6 :: bound to any host -> ''
        begin
          result := '';
          exit;
        end;
      1: // IPv6 ::1 -> '127.0.0.1' loopback (with no memory allocation)
        begin
          result := IP4local;
          exit;
        end;
    end;
  IP6Short(ip6addr, s);
  FastSetString(result, @s[1], ord(s[0]));
end;

function MacToText(mac: PByteArray): RawUtf8;
begin
  ToHumanHex(result, pointer(mac), 6);
end;

function MacTextFromHex(const Hex: RawUtf8): RawUtf8;
var
  L: PtrInt;
  h, m: PAnsiChar;
begin
  L := length(Hex);
  if (L = 0) or
     (L and 1 <> 0) then
  begin
    result := '';
    exit;
  end;
  L := L shr 1;
  m := FastSetString(result, (L * 3) - 1);
  h := pointer(Hex);
  repeat
    m[0] := h[0];
    if h[0] in ['A'..'Z'] then
      inc(m[0], 32);
    m[1] := h[1];
    if h[1] in ['A'..'Z'] then
      inc(m[1], 32);
    dec(L);
    if L = 0 then
      break;
    m[2] := ':';
    inc(h, 2);
    inc(m, 3);
  until false;
end;

function MacToHex(mac: PByteArray; maclen: PtrInt): RawUtf8;
var
  P: PAnsiChar;
  i, c: PtrInt;
  tab: PAnsichar;
begin
  if maclen < 0 then
    maclen := 0;
  P := FastSetString(result, maclen * 2);
  if maclen = 0 then
    exit;
  dec(maclen);
  tab := @HexCharsLower;
  i := 0;
  repeat
    c := mac[i];
    P[0] := tab[c shr 4];
    c := c and 15;
    P[1] := tab[c];
    if i = maclen then
      break;
    inc(P, 2);
    inc(i);
  until false;
end;

procedure NetAddRawUtf8(var Values: TRawUtf8DynArray; const Value: RawUtf8);
var
  n: PtrInt;
begin
  n := length(Values);
  SetLength(Values, n + 1);
  Values[n] := Value;
end;

var
  // GetIPAddressesText(Sep=' ') cache - refreshed every 32 seconds
  IPAddresses: array[TIPAddress] of record
    Safe: TLightLock;
    Text: RawUtf8;
    Tix: integer;
  end;

  // GetMacAddresses / GetMacAddressesText cache - refreshed every 65 seconds
  MacAddresses: array[{UpAndDown=}boolean] of record
    Safe: TLightLock;
    Tix: integer;
    Addresses: TMacAddressDynArray;
    Text: array[{WithoutName=}boolean] of RawUtf8;
  end;

procedure MacIPAddressFlush;
var
  ip: TIPAddress;
  ud: boolean;
begin
  for ip := low(ip) to high(ip) do
    with IPAddresses[ip] do
    begin
      Safe.Lock;
      try
        Text := '';
        Tix := 0;
      finally
        Safe.UnLock;
      end;
    end;
  for ud := low(ud) to high(ud) do
    with MacAddresses[ud] do
    begin
      Safe.Lock;
      try
        Addresses := nil;
        Tix := 0;
        Text[false] := '';
        Text[true] := '';
      finally
        Safe.UnLock;
      end;
    end;
end;

procedure GetIPCSV(const Sep: RawUtf8; Kind: TIPAddress; out Text: RawUtf8);
var
  ip: TRawUtf8DynArray;
  i: PtrInt;
begin
  ip := GetIPAddresses(Kind); // from OS
  if ip = nil then
    exit;
  Text := ip[0];
  for i := 1 to high(ip) do
    Text := Text + Sep + ip[i]; // as CSV
end;

function GetIPAddressesText(const Sep: RawUtf8; Kind: TIPAddress): RawUtf8;
var
  now: integer;
begin
  result := '';
  if Sep = ' ' then
    with IPAddresses[Kind] do
    begin
      now := mormot.core.os.GetTickCount64 shr 15 + 1; // refresh every 32768 ms
      Safe.Lock;
      try
        if now <> Tix then
          Tix := now
        else
        begin
          result := Text;
          if result <> '' then
            exit; // return the value from cache
        end;
        GetIPCSV(Sep, Kind, result); // ask the OS for the current IP addresses
        Text := result;
      finally
        Safe.UnLock;
      end;
    end
  else
    // Sep <> ' ' -> can't use the cache, so don't need to lock
    GetIPCSV(Sep, Kind, result);
end;

function GetMacAddresses(UpAndDown: boolean): TMacAddressDynArray;
var
  now: integer;
begin
  with MacAddresses[UpAndDown] do
  begin
    now := mormot.core.os.GetTickCount64 shr 16 + 1; // refresh every 65536 ms
    if Tix <> now then
    begin
      Safe.Lock;
      try
        if Tix <> now then
        begin
          Addresses := RetrieveMacAddresses(UpAndDown);
          Tix := now
        end;
      finally
        Safe.UnLock;
      end;
    end;
    result := Addresses;
  end;
end;

function GetMacAddressesText(WithoutName: boolean; UpAndDown: boolean): RawUtf8;
var
  i: PtrInt;
  addr: TMacAddressDynArray;
  w, wo: RawUtf8;
  now: integer;
  ok: boolean;
begin
  now := mormot.core.os.GetTickCount64 shr 16 + 1; // refresh every 65536 ms
  with MacAddresses[UpAndDown] do
  begin
    Safe.Lock; // to avoid memory leak
    result := Text[WithoutName];
    ok := (result <> '') or
          (Tix = now);
    Safe.UnLock; // TLightLock is not reentrant
    if ok then
      exit;
    addr := GetMacAddresses(UpAndDown); // will call Safe.Lock/UnLock
    if addr = nil then
      exit;
    for i := 0 to high(addr) do
      with addr[i] do
        if Address <> '' then
        begin
          w := Join([{%H-}w, Name, '=', Address, ' ']);
          if Kind <> makSoftware then
            wo := Join([{%H-}wo, Address, ' ']);
        end;
    FakeLength(w, length(w) - 1); // trim ending spaces
    FakeLength(wo, length(wo) - 1);
    Safe.Lock;
    Text[false] := w;
    Text[true] := wo;
    result := Text[WithoutName];
    Safe.UnLock;
  end;
end;

function _GetSystemMacAddress: TRawUtf8DynArray;
var
  i, n: PtrInt;
  addr: TMacAddressDynArray;
begin
  addr := GetMacAddresses({UpAndDown=}true);
  SetLength(result, length(addr));
  n := 0;
  for i := 0 to length(addr) - 1 do
    with addr[i] do
      if (Address <> '') and
         (Kind <> makSoftware) and
         not NetStartWith(pointer(Name), 'DOCKER') then
      begin
        result[n] := Address;
        inc(n);
      end;
  SetLength(result, n);
end;

function GetLocalIpAddress(const Remote: RawUtf8): RawUtf8;
var
  addr: TNetAddr;
  sock: TNetSocket;
begin
  result := '';
  if addr.SetFrom(Remote, '9', nlUdp) <> nrOk then // 9 is discard port
    exit;
  sock := addr.NewSocket(nlUdp);
  if sock <> nil then
    try
      if (connect(sock.Socket, @addr, addr.Size) = NO_ERROR) and
         (sock.GetName(addr) = nrOk) then
        addr.IP(result);
    finally
      sock.Close;
    end;
end;

var
  DnsCache: record
    Safe: TLightLock;
    Tix: cardinal;
    Value, Custom: TRawUtf8DynArray;
  end;

function GetDnsAddresses(usePosixEnv: boolean): TRawUtf8DynArray;
var
  tix32: cardinal;
  i: PtrInt;
begin
  tix32 := mormot.core.os.GetTickCount64 shr 13 + 1; // refresh every 8192 ms
  with DnsCache do
  begin
    Safe.Lock;
    try
      if tix32 <> Tix then
      begin
        Value := _GetDnsAddresses(usePosixEnv, false);
        for i := 0 to length(Custom) - 1 do
          NetAddRawUtf8(Value, Custom[i]);
        Tix := tix32;
      end;
      result := Value;
    finally
      Safe.UnLock;
    end;
  end;
end;

procedure RegisterDnsAddress(const DnsResolver: RawUtf8);
begin
  with DnsCache do
  begin
    Safe.Lock;
    try
      NetAddRawUtf8(Custom, DnsResolver);
      Tix := 0; // flush cache
    finally
      Safe.UnLock;
    end;
  end;
end;

function GetDomainNames(usePosixEnv: boolean): TRawUtf8DynArray;
begin
  if ForcedDomainName <> '' then
  begin
    SetLength(result, 1);
    result[0] := ForcedDomainName;
  end
  else
    result := _GetDnsAddresses(usePosixEnv, {getAD=}true); // no cache for the AD
end;

var
  KnownHostCache: TNetHostCache;
  KnownHostCacheFileTime: TUnixTime;
  RegKnownHostCache: TNetHostCache;

procedure KnownHostCacheReload;
var
  p: PUtf8Char;
  ip4: cardinal;
  h: RawUtf8;
begin
  KnownHostCache.Count := 0;
  KnownHostCache.AddFrom(RegKnownHostCache);
  p := pointer(StringFromFile(host_file));
  while p <> nil do
  begin
    while p^ in [#9, ' '] do
      inc(p);
    if (p^ in ['1'..'9']) and
       NetIsIP4(p, @ip4) and
       ({%H-}ip4 <> 0) then
    begin
      repeat
        inc(p);
      until p^ <= ' '; // go to end of IP text
      repeat
        h := NetGetNextSpaced(p);
        if h = '' then
          break;
        KnownHostCache.Add(h, ip4);
      until false;
    end;
    p := GotoNextLine(p);
  end;
end;

function GetKnownHost(const HostName: RawUtf8; out ip4: cardinal): boolean;
var
  tixfile: TUnixTime;
begin
  result := false;
  if HostName = '' then
    exit;
  KnownHostCache.Safe.Lock;
  try
    if KnownHostCache.TixDeprecated then
    begin
      // check at least every 8 seconds if the file actually changed on disk
      tixfile := FileAgeToUnixTimeUtc(host_file);
      if tixfile = 0 then
        exit; // no hosts file
      if tixfile <> KnownHostCacheFileTime then
      begin
        // hosts file content changed: reload it
        KnownHostCacheFileTime := tixfile;
        KnownHostCacheReload;
      end;
    end;
    result := KnownHostCache.Find(HostName, ip4);
  finally
    KnownHostCache.Safe.UnLock;
  end;
end;

procedure RegisterKnownHost(const HostName, Ip4: RawUtf8);
var
  ip32: cardinal;
begin
  if (HostName <> '') and
     NetIsIP4(pointer(ip4), @ip32) then
  begin
    RegKnownHostCache.SafeAdd(HostName, ip32, {tixshr=}0);
    KnownHostCache.SafeAdd(HostName, ip32, 0); // for immediate GetKnownHost()
  end;
end;


{ ******************** TLS / HTTPS Encryption Abstract Layer }

procedure InitNetTlsContext(var TLS: TNetTlsContext); overload;
begin
  Finalize(TLS);
  FillCharFast(TLS, SizeOf(TLS), 0);
end;

procedure InitNetTlsContext(var TLS: TNetTlsContext; Server: boolean;
  const CertificateFile, PrivateKeyFile: TFileName;
  const PrivateKeyPassword: RawUtf8; const CACertificatesFile: TFileName);
begin
  InitNetTlsContext(TLS);
  TLS.IgnoreCertificateErrors := Server; // needed if no mutual auth is done
  TLS.CertificateFile := RawUtf8(CertificateFile); // RTL TFileName to RawUtf8
  TLS.PrivateKeyFile  := RawUtf8(PrivateKeyFile);
  TLS.PrivatePassword := PrivateKeyPassword;
  TLS.CACertificatesFile := RawUtf8(CACertificatesFile);
end;

procedure ResetNetTlsContext(var TLS: TNetTlsContext);
begin
  TLS.Enabled := false;
  FastAssignNew(TLS.CipherName);
  FastAssignNew(TLS.PeerIssuer);
  FastAssignNew(TLS.PeerSubject);
  FastAssignNew(TLS.PeerInfo);
  TLS.PeerCert := nil;
  FastAssignNew(TLS.LastError);
end;

function GetTlsContext(TlsEnabled, IgnoreTlsCertError: boolean;
  var Context: TNetTlsContext; Forced: PNetTlsContext): PNetTlsContext;
begin
  result := nil;
  if not TlsEnabled then
    exit;
  result := Forced;
  if result <> nil then
    exit;
  InitNetTlsContext(Context);
  Context.IgnoreCertificateErrors := IgnoreTlsCertError;
  result := @Context;
end;

function SameNetTlsContext(const tls1, tls2: TNetTlsContext): boolean;
begin
  result := (tls1.Enabled = tls2.Enabled) and
            ((not tls1.Enabled) or
             ((tls1.IgnoreCertificateErrors = tls2.IgnoreCertificateErrors) and
              (tls1.CertificateFile         = tls2.CertificateFile) and
              (tls1.CertificateBin          = tls2.CertificateBin) and
              (tls1.CACertificatesFile      = tls2.CACertificatesFile) and
              (tls1.CACertificatesRaw       = tls2.CACertificatesRaw) and
              (tls1.CASystemStores          = tls2.CASystemStores) and
              (tls1.CertificateRaw          = tls2.CertificateRaw) and
              (tls1.PrivateKeyFile          = tls2.PrivateKeyFile) and
              (tls1.PrivatePassword         = tls2.PrivatePassword) and
              (tls1.PrivateKeyRaw           = tls2.PrivateKeyRaw) and
              (tls1.HostNamesCsv            = tls2.HostNamesCsv)));
end;


{ ******************** TSocketStream Socket Wrapper }

{ TSocketStream }

constructor TSocketStream.Create(aSocket: TNetSocket);
begin
  fSocket := aSocket;
end;

constructor TSocketStream.Create(const aSecure: INetTls);
begin
  fSecure := aSecure;
end;

destructor TSocketStream.Destroy;
begin
  inherited Destroy;
  fSocket := nil;
  fSecure := nil; // before fOwned.Free e.g. if fOwned is matching TCrtSocket
  fOwned.Free;
end;

function TSocketStream.Read(var Buffer; Count: Longint): Longint;
begin
  if Assigned(fSecure) then
    fLastResult := fSecure.Receive(@Buffer, Count)
  else
    fLastResult := fSocket.Recv(@Buffer, Count, @fLastRawError);
  case fLastResult of
    nrOk:
      begin
        result := Count;
        inc(fSize, Count);
        fPosition := fSize;
      end;
    nrRetry:
      result := 0; // no data available yet
  else
    result := -1;  // fatal error - e.g. nrClosed for recv()=0
  end;
end;

function TSocketStream.Write(const Buffer; Count: Longint): Longint;
begin
  if Assigned(fSecure) then
    fLastResult := fSecure.Send(@Buffer, Count)
  else
    fLastResult := fSocket.Send(@Buffer, Count, @fLastRawError);
  case fLastResult of
    nrOk:
      begin
        result := Count;
        inc(fSize, Count);
        fPosition := fSize;
      end;
    nrRetry:
      result := 0; // no data available yet
  else
    result := -1;  // fatal error
  end;
end;


{ ******************** Efficient Multiple Sockets Polling }

{$ifdef CPU32}

function ResToTag(const res: TPollSocketResult): TPollSocketTag;
begin
  result := res.Li; // 32-bit integer
end;

function ResToEvents(const res: TPollSocketResult): TPollSocketEvents;
begin
  result := TPollSocketEvents(res.B[4]);
end;

procedure SetRes(var res: TPollSocketResult; tag: TPollSocketTag; ev: TPollSocketEvents);
begin
  res.Li := tag;
  res.B[4] := byte(ev);
end;

procedure ResetResEvents(var res: TPollSocketResult);
begin
  res.B[4] := 0;
end;

{$else}

function ResToTag(const res: TPollSocketResult): TPollSocketTag;
begin
  result := res and $00ffffffffffffff; // pointer from lower 56-bit integer
end;

function ResToEvents(const res: TPollSocketResult): TPollSocketEvents;
begin
  result := TPollSocketEvents(byte(res shr 60));
end;

procedure SetRes(var res: TPollSocketResult; tag: TPollSocketTag; ev: TPollSocketEvents);
begin
  res := tag or (PtrUInt(byte(ev)) shl 60);
end;

procedure ResetResEvents(var res: TPollSocketResult);
begin
  res := res and $00ffffffffffffff;
end;

{$endif CPU32}

function ToText(ev: TPollSocketEvents): TShort8;
var
  e: TPollSocketEvent;
begin
  result[0] := #0;
  for e := low(e) to high(e) do
    if e in ev then
    begin
      inc(result[0]);
      result[ord(result[0])] := POLL_SOCKET_EVENT[e];
    end;
end;


{ TPollAbstract }

procedure TPollAbstract.Terminate;
begin
end;


{ TPollSocketAbstract }

class function TPollSocketAbstract.FollowEpoll: boolean;
begin
  result := false; // select/poll API are not thread safe
end;

constructor TPollSocketAbstract.Create(aOwner: TPollSockets);
begin
  fOwner := aOwner;
end;



{ TPollSockets }

constructor TPollSockets.Create(aPollClass: TPollSocketClass);
begin
  inherited Create;
  if aPollClass = nil then
    fPollClass := PollSocketClass
  else
    fPollClass := aPollClass;
  fPendingSafe.Init; // mandatory for TOSLightLock
  {$ifdef POLLSOCKETEPOLL}
  // epoll has no size limit (so a single fPoll[0] can be assumed), and
  // TPollSocketEpoll is thread-safe and let epoll_wait() work in the background
  SetLength(fPoll, 1);
  fPoll[0] := fPollClass.Create(self);
  {$else}
  fPollLock.Init;
  {$endif POLLSOCKETEPOLL}
  {$ifdef OSPOSIX}
  SetFileOpenLimit(GetFileOpenLimit(true)); // set soft limit to hard value
  {$endif OSPOSIX}
end;

destructor TPollSockets.Destroy;
var
  i: PtrInt;
  endtix: cardinal; // never wait forever
begin
  Terminate;
  if fGettingOne > 0 then
  begin
    if Assigned(fOnLog) then
      fOnLog(sllTrace, 'Destroy: wait for fGettingOne=%', [fGettingOne], self);
    endtix := GetTickSec + 5;
    while (fGettingOne > 0) and
          (GetTickSec < endtix) do
      SleepHiRes(1);
    if Assigned(fOnLog) then
      fOnLog(sllTrace, 'Destroy: ended as fGettingOne=%', [fGettingOne], self);
  end;
  for i := 0 to high(fPoll) do
    FreeAndNilSafe(fPoll[i]);
  {$ifndef POLLSOCKETEPOLL}
  if fUnsubscribeShutdownSocket and
     (fSubscription.UnsubscribeCount > 0) then
  begin
    if Assigned(fOnLog) then
      fOnLog(sllTrace, 'Destroy: shutdown UnsubscribeCount=%',
        [fSubscription.UnsubscribeCount], self);
    for i := 0 to fSubscription.UnsubscribeCount - 1 do
       fSubscription.Unsubscribe[i].ShutdownAndClose({rdwr=}false);
  end;
  fPollLock.Done;
  {$endif POLLSOCKETEPOLL}
  fPendingSafe.Done; // mandatory for TOSLightLock
  inherited Destroy;
end;

function TPollSockets.Subscribe(socket: TNetSocket; events: TPollSocketEvents;
  tag: TPollSocketTag): boolean;
{$ifndef POLLSOCKETEPOLL}
var
  n: PtrInt;
  one: PPollSocketsSubscribe;
{$endif POLLSOCKETEPOLL}
begin
  result := false;
  if (self = nil) or
     (socket = nil) or
     (events = []) then
    exit;
  {$ifdef POLLSOCKETEPOLL}
  // TPollSocketEpoll is thread-safe and let epoll_wait() work in the background
  result := fPoll[0].Subscribe(socket, events, tag);
  if result then
    LockedInc32(@fCount);
  {$else}
  // fPoll[0].Subscribe() is not allowed when WaitForModified() is running
  // -> trick is to asynch append the information to fSubscription.Subscribe[]
  fSubscriptionSafe.Lock;
  try
    n := fSubscription.SubscribeCount;
    if n = length(fSubscription.Subscribe) then
      SetLength(fSubscription.Subscribe, n + 64);
    one := @fSubscription.Subscribe[n];
    one^.socket := socket;
    one^.tag := tag;
    one^.events := events;
    fSubscription.SubscribeCount := n + 1;
  finally
    fSubscriptionSafe.UnLock;
  end;
  result := true;
  {$endif POLLSOCKETEPOLL}
end;

procedure TPollSockets.Unsubscribe(socket: TNetSocket; tag: TPollSocketTag);
begin
  // actually unsubscribe from the sockets monitoring API
  {$ifdef POLLSOCKETEPOLL}
  // TPollSocketEpoll is thread-safe and let epoll_wait() work in the background
  if fPoll[0].Unsubscribe(socket) then
  begin
    LockedDec32(@fCount);
    if Assigned(fOnLog) then
      fOnLog(sllTrace, 'Unsubscribe(%) count=%', [pointer(socket), fCount], self);
  end;
  {$else}
  // fPoll[0].UnSubscribe() is not allowed when WaitForModified() is running
  // -> append to the unsubscription asynch list
  fSubscriptionSafe.Lock;
  AddPtrUInt(TPtrUIntDynArray(fSubscription.Unsubscribe),
    fSubscription.UnsubscribeCount, PtrUInt(socket));
  fSubscriptionSafe.UnLock;
  {$endif POLLSOCKETEPOLL}
end;

function FindPendingFromTag(res: PPollSocketResult; n: PtrInt;
  tag: TPollSocketTag): PPollSocketResult;
  {$ifdef HASINLINE} inline; {$endif}
begin
  if n > 0 then
  begin
    result := res;
    repeat
      if ResToTag(result^) = tag then // fast O(n) search in L1 cache
        exit;
      inc(result);
      dec(n);
    until n = 0;
  end;
  result := nil;
end;

function TPollSockets.EnsurePending(tag: TPollSocketTag): boolean;
begin
  // manual O(n) brute force search
  result := FindPendingFromTag(
    @fPending.Events[fPendingIndex], fPending.Count - fPendingIndex, tag) <> nil;
end;

procedure TPollSockets.SetPending(tag: TPollSocketTag);
begin
  // overriden method may set a per-connection flag for O(1) lookup
end;

function TPollSockets.UnsetPending(tag: TPollSocketTag): boolean;
begin
  result := true; // overriden e.g. in TPollAsyncReadSockets
end;

function TPollSockets.GetSubscribeCount: integer;
begin
  {$ifdef POLLSOCKETEPOLL}
  result := 0; // epoll_ctl() is called directly, so there is nothing pending
  {$else}
  result := fSubscription.SubscribeCount;
  {$endif POLLSOCKETEPOLL}
end;

function TPollSockets.GetUnsubscribeCount: integer;
begin
  {$ifdef POLLSOCKETEPOLL}
  result := 0;
  {$else}
  result := fSubscription.UnsubscribeCount;
  {$endif POLLSOCKETEPOLL}
end;

function TPollSockets.GetOnePending(out notif: TPollSocketResult;
  const call: RawUtf8): boolean;
var
  n, ndx: PtrInt;
begin
  result := false;
  if fTerminated or
     (fPending.Count <= 0) then
    exit;
  // search for the next event in fPending
  fPendingSafe.Lock; // former versions used TryLock but unstable on Windows
  try  // HASFASTTRYFINALLY is unsafe here and has little performance impact
    n := fPending.Count;
    if fTerminated or
       (n <= 0) then
      exit;
    ndx := fPendingIndex;
    if ndx < n then
      repeat
        // retrieve next notified event
        notif := fPending.Events[ndx];
        // move forward
        inc(ndx);
        if (byte(ResToEvents(notif)) <> 0) and // DeleteOnePending() may set 0
           UnsetPending(ResToTag(notif)) then  // e.g. TPollAsyncReadSockets
        begin
          // there is a non-void event to return
          result := true;
          fPendingIndex := ndx; // continue with next event
          break;
        end;
      until ndx >= n;
    if ndx >= n then
    begin
      fPending.Count := 0; // reuse shared fPending.Events[] memory
      fPendingIndex := 0;
    end;
  finally
    fPendingSafe.UnLock;
  end;
  (*{$ifndef POLLSOCKETEPOLL} // never called in practice :(
  // poll/socket does not include the just subscribed sockets
  if not result and
     MergeSubscribeEvents then
    result := GetOnePending(notif, call);
  {$endif POLLSOCKETEPOLL}*)
  if result and
     Assigned(fOnLog) then // log outside fPendingSafe
    fOnLog(sllTrace, 'GetOnePending(%)=% % #%/%', [call,
      pointer(ResToTag({%H-}notif)), byte(ResToEvents({%H-}notif)), ndx, n], self);
end;

function TPollSockets.MergePendingEvents(const new: TPollSocketResults): integer;
var
  n, len, cap: PtrInt;
  p: PPollSocketResult;
begin
  len := fPending.Count;
  if len = 0 then
  begin
    // no previous results: just replace the list
    result := new.Count;
    fPending.Count := new.Count;
    fPending.Events := new.Events;
    fPendingIndex := 0;
    if PClass(self)^ <> TPollSockets then // if SetPending() is overriden
    begin
      p := pointer(new.Events);
      n := new.Count;
      if n <> 0 then
        repeat
          SetPending(ResToTag(p^)); // O(1) flag set in TPollConnectionSockets
          inc(p);
          dec(n);
        until n = 0;
    end;
    exit;
  end;
  // vacuum the results list (to let caller set fPendingIndex := 0)
  if fPendingIndex <> 0 then
  begin
    dec(len, fPendingIndex);
    with fPending do
      MoveFast(Events[fPendingIndex], Events[0], len * SizeOf(Events[0]));
    fPending.Count := len;
    fPendingIndex := 0;
  end;
  result := 0; // returns number of new events to process
  // remove any duplicate: PollForPendingEvents() called before GetOnePending()
  n := new.Count;
  p := pointer(new.Events);
  cap := length(fPending.Events);
  if n <> 0 then
    repeat
      if (byte(ResToEvents(p^)) <> 0) and // DeleteOnePending() may set 0
         not EnsurePending(ResToTag(p^)) then // O(1) in TPollConnectionSockets
      begin
        // new event to process
        if len >= cap then
        begin
          cap := NextGrow(len + new.Count);
          SetLength(fPending.Events, cap); // seldom needed
        end;
        fPending.Events[len] := p^;
        inc(len);
        inc(result);
      end;
      inc(p);
      dec(n);
    until n = 0;
  fPending.Count := len;
end;

{$ifdef POLLSOCKETEPOLL}
function TPollSockets.MergeSubscribeEvents: boolean;
begin
  result := false; // epool has asynchronously subscription
end;
{$else}
function TPollSockets.MergeSubscribeEvents: boolean;
var
  sub: TPollSocketsSubscribeDynArray;
  new: TPollSocketResults;
begin
  // never called in practice: pending subscription seems no bottleneck
  result := false;
  if (fSubscription.SubscribeCount <> 0) and
     fMergeSubscribeEventsLock.TryLock then
  try
    fSubscriptionSafe.Lock;
    sub := copy(fSubscription.Subscribe, 0, fSubscription.SubscribeCount);
    fSubscriptionSafe.UnLock;
    if not WaitForSeveral(sub, new, {timeoutMS=}10) then
      exit;
    fPendingSafe.Lock;
    try
      result := MergePendingEvents(new) <> 0;
    finally
      fPendingSafe.UnLock;
    end;
  finally
    fMergeSubscribeEventsLock.UnLock;
  end;
end;
{$endif POLLSOCKETEPOLL}

function TPollSockets.PollForPendingEvents(timeoutMS: integer): integer;
var
  last, lastcount: PtrInt;
  start, stop: Int64;
  {$ifndef POLLSOCKETEPOLL}
  n, u, s, p: PtrInt;
  poll: TPollSocketAbstract;
  sock: TNetSocket;
  sub: TPollSocketsSubscription;
  {$endif POLLSOCKETEPOLL}
  new: TPollSocketResults; // local list for WaitForModified()
begin
  // by design, this method is called from a single thread
  result := 0;
  if fTerminated then
    exit;
  if Assigned(fOnLog) then
    QueryPerformanceMicroSeconds(start);
  LockedInc32(@fGettingOne);
  try
    // thread-safe get the pending (un)subscriptions
    last := -1;
    new.Count := 0;
    {$ifdef OSPOSIX} // TOSLight.TryLock is not available on Windows
    if (fPending.Count = 0) and
       fPendingSafe.TryLock then
    begin
      if fPending.Count = 0 then
      begin
        // reuse the main dynamic array of results
        pointer(new.Events) := pointer(fPending.Events); // inlined MoveAndZero
        pointer(fPending.Events) := nil;
      end;
      fPendingSafe.UnLock;
    end;
    {$endif OSPOSIX}
    {$ifdef POLLSOCKETEPOLL}
    // TPollSocketEpoll is thread-safe and let epoll_wait() work in the background
    {if Assigned(OnLog) then
      OnLog(sllTrace, 'PollForPendingEvents: before WaitForModified(%) count=% pending=%',
        [timeoutMS, fCount, fPending.Count], self);}
    // if fCount=0 epoll_wait() still wait and allow background subscription
    fPoll[0].WaitForModified(new, timeoutMS);
    last := 0;
    lastcount := fPoll[0].Count;
    {$else}
    // manual check of all fPoll[] for subscriptions or modifications
    if fCount + fSubscription.SubscribeCount = 0 then
      exit; // caller would loop
    fSubscriptionSafe.Lock;
    MoveFast(fSubscription, sub, SizeOf(sub));  // quick copy with no refcnt
    FillCharFast(fSubscription, SizeOf(fSubscription), 0);
    fSubscriptionSafe.UnLock;
    if Assigned(fOnLog) and
       ((sub.SubscribeCount <> 0) or
        (sub.UnsubscribeCount <> 0))then
      fOnLog(sllTrace, 'PollForPendingEvents sub=% unsub=%',
        [sub.SubscribeCount, sub.UnsubscribeCount], self);
    // ensure subscribe + unsubscribe pairs are ignored
    if not fUnsubscribeShutdownSocket then
      for u := 0 to sub.UnsubscribeCount - 1 do
      begin
        sock := sub.Unsubscribe[u];
        for s := 0 to sub.SubscribeCount - 1 do
          if sub.Subscribe[s].socket = sock then
          begin
            if Assigned(fOnLog) then
              fOnLog(sllTrace, 'PollForPendingEvents sub+unsub sock=%',
                [pointer(sock)], self);
            sub.Unsubscribe[u] := nil; // mark both no op
            sub.Subscribe[s].socket := nil;
            break;
          end;
      end;
    // use fPoll[] to retrieve any pending notifications
    fPollLock.Lock;
    try
      // first unsubscribe closed connections
      for u := 0 to sub.UnsubscribeCount - 1 do
      begin
        sock := sub.Unsubscribe[u];
        if sock <> nil then
          for p := 0 to length(fPoll) - 1 do
            if fPoll[p].Unsubscribe(sock) then
            begin
              dec(fCount);
              if fUnsubscribeShutdownSocket then
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
            poll := fPollClass.Create(self); // need a new poll instance
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
        exit; // nothing to track any more (all Unsubscribe)
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
      fPollIndex := last; // next call will continue from fPoll[fPollIndex+1]
      lastcount := fPoll[last].Count;
    finally
      fPollLock.UnLock;
    end;
    {$endif POLLSOCKETEPOLL}
    // append the new events to the main fPending list
    result := new.Count;
    if (result <= 0) or
       fTerminated then
      exit;
    fPendingSafe.Lock;
    try
      result := MergePendingEvents(new);
    finally
      fPendingSafe.UnLock;
    end;
    new.Events := nil;
    if (result > 0) and
       Assigned(fOnLog) then
    begin
      QueryPerformanceMicroSeconds(stop);
      fOnLog(sllTrace,
        'PollForPendingEvents=% in fPoll[%] (subscribed=%) pending=% %us',
          [result, last, lastcount, fPending.Count, stop - start], self);
    end;
  finally
    LockedDec32(@fGettingOne);
  end;
end;

procedure TPollSockets.AddOnePending(
  aTag: TPollSocketTag; aEvents: TPollSocketEvents; aSearchExisting: boolean);
var
  n: PtrInt;
  notif: TPollSocketResult;
begin
  SetRes(notif, aTag, aEvents);
  fPendingSafe.Lock;
  try
    n := fPending.Count;
    if (n = 0) or
       (not aSearchExisting) or
       (not Int64ScanExists(@fPending.Events[fPendingIndex],
         fPending.Count - fPendingIndex, PInt64(@notif)^)) then
    begin
      if n >= length(fPending.Events) then
        SetLength(fPending.Events, NextGrow(n));
      fPending.Events[n] := notif;
      fPending.Count := n + 1;
    end;
  finally
    fPendingSafe.UnLock;
  end;
end;

function TPollSockets.DeleteOnePending(aTag: TPollSocketTag): boolean;
var
  fnd: PPollSocketResult;
begin
  result := false;
  if (fPending.Count = 0) or
     (aTag = 0) then
    exit;
  fPendingSafe.Lock;
  try
    if fPending.Count <> 0 then
    begin
      fnd := FindPendingFromTag( // fast O(n) search in L1 cache
        @fPending.Events[fPendingIndex], fPending.Count - fPendingIndex, aTag);
      if fnd <> nil then
      begin
        ResetResEvents(fnd^);  // GetOnePending() will just ignore it
        result := true;
      end;
    end;
  finally
    fPendingSafe.UnLock;
  end;
end;

function TPollSockets.DeleteSeveralPending(
  aTag: PPollSocketTag; aTagCount: integer): integer;
var
  p: PPollSocketResult;
  n: integer;
begin
  result := 0;
  if (fPending.Count = 0) or
     (aTagCount = 0) then
    exit;
  dec(aTagCount);
  if aTagCount = 0 then
  begin
    result := ord(DeleteOnePending(aTag^));
    exit;
  end;
  QuickSortPtrInt(pointer(aTag), 0, aTagCount);
  fPendingSafe.Lock;
  try
    n := fPending.Count;
    if n = 0 then
      exit;
    dec(n, fPendingIndex);
    p := @fPending.Events[fPendingIndex];
    if n > 0 then
      repeat
        if FastFindPtrIntSorted(pointer(aTag), aTagCount, ResToTag(p^)) >= 0 then
        begin
          ResetResEvents(p^); // GetOnePending() will just ignore it
          inc(result);
        end;
        inc(p);
        dec(n)
      until n = 0;
  finally
    fPendingSafe.UnLock;
  end;
end;

function TPollSockets.GetOne(timeoutMS: integer; const call: RawUtf8;
  out notif: TPollSocketResult): boolean;
{$ifndef POLLSOCKETEPOLL}
var
  start, tix, endtix: Int64;
{$endif POLLSOCKETEPOLL}
begin
  // first check if some pending events are available
  result := GetOnePending(notif, call);
  if result or
     fTerminated or
     (timeoutMS < 0) then
    exit;
  // here we need to ask the socket layer
  {$ifdef POLLSOCKETEPOLL}
  // TPollSocketEpoll is thread-safe and let epoll_wait() work in the background
  PollForPendingEvents(timeoutMS); // inc(fGettingOne) +  blocking epoll_wait
  if fTerminated then
    exit;
  result := GetOnePending(notif, call);
  if Assigned(fOnGetOneIdle) then
    fOnGetOneIdle(self, mormot.core.os.GetTickCount64);
  {$else}
  // non-blocking call of PollForPendingEvents()
  PQWord(@notif)^ := 0;
  start := 0;
  endtix := 0;
  LockedInc32(@fGettingOne);
  try
    repeat
      // non-blocking search of pending events within all subscribed fPoll[]
      if fTerminated then
        exit;
      if fPending.Count = 0 then
        PollForPendingEvents({timeoutMS=}10);
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
  {$endif POLLSOCKETEPOLL}
end;

procedure TPollSockets.Terminate;
var
  i: PtrInt;
begin
  if self = nil then
    exit;
  fTerminated := true;
  for i := 0 to high(fPoll) do
    fPoll[i].Terminate;
end;


{ *************************** TUri parsing/generating URL wrapper }

function NetStartWith(p, up: PUtf8Char): boolean;
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
      break; // match
    inc(up);
    c := p^;
    inc(p);
    if c = u  then
      continue
    else if (c < 'a') or
            (c > 'z') then
      exit;
    dec(c, 32); // convert char to uppercase
    if c <> u then
      exit;
  until false;
  result := true;
end;

function NetIsIP4(text: PUtf8Char; value: PByte): boolean;
var
  n, o, b: integer;
begin
  result := false;
  if text = nil then
    exit;
  b := -1;
  n := 0;
  while true do
    case text^ of
      #0 .. ' ',
      '/': // allow CIDR '1.2.3.4/20' decoding
        if (b < 0) or
           (n <> 3) then
          exit
        else
          break;
      '.':
        begin
          if (b < 0) or
             (n = 3) then
            exit;
          if value <> nil then
          begin
            value^ := b;
            inc(value);
          end;
          b := -1;
          inc(n);
          repeat
            inc(text);
          until text^ <> ' '; // allow space between numbers
        end;
      '0' .. '9':
        begin
          o := ord(text^) - 48;
          if b < 0 then
            b := o
          else
          begin
            b := b * 10 + o;
            if b > 255 then
              exit; // out-of-range number
          end;
          inc(text);
        end
    else
      exit;
    end;
  if value <> nil then
    value^ := b;
  result := true; // 1.2.3.4
end;

function NetGetNextSpaced(var P: PUtf8Char): RawUtf8;
var
  S: PUtf8Char;
begin
  result := '';
  while P^ in [#9, ' '] do
    inc(P);
  if P^ < ' ' then
    exit; // end of line or end of file (buffer)
  S := P;
  repeat
    inc(P);
  until P^ <= ' ';
  FastSetString(result, S, P - S);
end;

function NetBinToBase64(const s: RawByteString): RawUtf8;
begin // just redirect from mormot.core.os.pas to mormot.core.buffers.pas
  result := RawToBase64(pointer(s), length(s), {uri=}false);
end;

function NetIsPem(p: PUtf8Char): boolean;
begin
  result := true;
  repeat
    p := PosChar(p, '-'); // may use SSE2 asm
    if p = nil then
      break;
    repeat
      inc(p);
      if (PCardinal(p)^ = $2d2d2d2d) and  // -----BEGIN
         (PCardinal(p + 4)^ = ord('B') + ord('E') shl 8 + ord('G') shl 16 + ord('I') shl 24) then
        exit;
    until p^ <> '-'
  until p^ = #0;
  result := false;
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


{ TIp4SubNet }

function TIp4SubNet.From(const subnet: RawUtf8): boolean;
var
  ip4, sub4: RawUtf8;
  ip32, prefix: cardinal; // local temporary ip32 is needed on Delphi XE4 :(
begin
  if SplitFromRight(subnet, '/', ip4, sub4) then // regular '1.2.3.4/sub' mask
  begin
    ip32 := 0;
    mask := 0;
    result := NetIsIP4(pointer(ip4), @ip32) and
              ToCardinal(sub4, prefix, 1) and
              IP4Netmask(prefix{%H-}, mask);
  end
  else
  begin
    mask := cardinal(-1); // 255.255.255.255
    result := NetIsIP4(pointer(subnet), @ip32); // plain '1.2.3.4' IPv4 address
  end;
  ip := ip32 and mask; // normalize
end;

function TIp4SubNet.Match(const ip4: RawUtf8): boolean;
var
  ip32: cardinal;
begin
  result := NetIsIP4(pointer(ip4), @ip32) and
            Match(ip32{%H-});
end;


{ TIp4SubNets }

function FindIp4SubNetsMask(m: PIp4SubNetMask; mask4: cardinal): PIp4SubNetMask;
var
  n: integer;
begin
  result := m;
  if result = nil then
    exit;
  n := PDALen(PAnsiChar(m) - _DALEN)^ + _DAOFF;
  repeat
    if result^.Mask = mask4 then // less than 20 masks in practice: O(n) is fine
      exit;
    inc(result);
    dec(n);
  until n = 0;
  result := nil;
end;

function TIp4SubNets.Add(ip, mask: cardinal): boolean;
var
  p: PIp4SubNetMask;
  n: PtrInt;
begin
  result := false;
  if (ip = cardinal(-1)) or  // 255.255.255.255
     not IsPublicIP(ip) then // e.g. 192.168.1.1
    exit;
  p := FindIp4SubNetsMask(pointer(fSubNet), mask);
  if p = nil then
  begin
    n := length(fSubNet);
    SetLength(fSubNet, n + 1);
    p := @fSubNet[n];
    p^.Mask := mask;
  end;
  result := AddSortedInteger(p^.IP, p^.IPCount, ip) >= 0;
end;

function TIp4SubNets.Add(const subnet: RawUtf8): boolean;
var
  sub: TIp4SubNet;
begin
  result := sub.From(subnet) and
            Add(sub.ip, sub.mask);
end;

function TIp4SubNets.Match(ip4: cardinal): boolean;
var
  p: PIp4SubNetMask;
  n: integer;
begin
  p := pointer(fSubNet);
  if p <> nil then
  begin
    result := true;
    n := PDALen(PAnsiChar(p) - _DALEN)^ + _DAOFF; // try all masks
    repeat
      // O(log(n)) binary search (branchless asm on x86_64)
      if FastFindIntegerSorted(pointer(p^.IP), p^.IPCount - 1, ip4 and P^.Mask) >= 0 then
        exit; // not faster to use IntegerScanIndex() for small IPCount
      inc(p);
      dec(n);
    until n = 0;
  end;
  result := false;
end;

function TIp4SubNets.Match(const ip4: RawUtf8): boolean;
var
  ip32: cardinal;
begin
  result := NetIsIP4(pointer(ip4), @ip32) and
            Match(ip32{%H-});
end;

procedure TIp4SubNets.Clear;
begin
  fSubNet := nil;
end;

function TIp4SubNets.SaveToBinary: RawByteString;
var
  i, n, L: PtrInt;
  p: PCardinalArray;
begin
  n := length(fSubNet);
  L := n * 8 + 8;
  for i := 0 to n - 1 do
    inc(L, fSubNet[i].IPCount * 4);
  p := FastNewRawByteString(result, L);
  p^[0] := IP4SUBNET_MAGIC;
  p^[1] := n;
  p := @p^[2];
  for i := 0 to n - 1 do
    with fSubNet[i] do
    begin
      p^[0] := Mask;
      p^[1] := IPCount;
      MoveFast(pointer(IP)^, p^[2], p^[1] * 4);
      p := @p^[p^[1] + 2];
    end;
end;

function TIp4SubNets.LoadFromBinary(const bin: RawByteString): integer;
var
  i, n: PtrInt;
  p: PCardinalArray;
  d: PIp4SubNetMask;
begin
  result := 0;
  Clear;
  n := length(bin);
  if (n <= 8) or
     (PCardinal(bin)^ <> IP4SUBNET_MAGIC) or
     ((n and 3) <> 0) then // should be an exact array of 32-bit integers
    exit;
  p := @PCardinalArray(bin)[1];
  n := (n shr 2) - 1;
  for i := 0 to p^[0] - 1 do
  begin
    if n < 2 then
      exit; // avoid buffer overflow
    dec(n, p^[2] + 2);
    p := @p^[p^[2] + 2];
  end;
  if n <> 1 then
    exit; // decoded size should be an exact match with supplied bin
  p := @PCardinalArray(bin)[1];
  SetLength(fSubNet, p^[0]);
  d := pointer(fSubNet);
  for i := 0 to p^[0] - 1 do
  begin
    d^.Mask := p^[1];
    d^.IPCount := p^[2];
    SetLength(d^.IP, p^[2]);
    MoveFast(p^[3], pointer(d^.IP)^, p^[2] * 4);
    inc(result, p^[2]);
    p := @p^[p^[2] + 2];
    inc(d);
  end;
end;

function TIp4SubNets.AddFromText(const text: RawUtf8): integer;
var
  p: PUtf8Char;
  ip, mask: cardinal;
begin
  result := 0;
  p := pointer(text);
  while p <> nil do
  begin
    while p^ in [#1 .. ' ' ] do
      inc(p);
    if NetIsIP4(p, @ip) then // ignore any line starting e.g. with # or ;
    begin
      while p^ in ['0' .. '9', '.', ' '] do
        inc(p);
      if p^ <> '/' then
        mask := cardinal(-1) // single IP has 255.255.255.255 mask
      else
        mask := IP4Netmask(GetCardinal(p + 1)); // CIDR
      if (mask <> 0) and
         Add(ip, mask) then
        inc(result); // first time seen
    end;
    p := GotoNextLine(p);
  end;
end;

function TIp4SubNets.AfterAdd: integer;
var
  n: integer;
  p: PIp4SubNetMask;
begin
  result := 0;
  p := pointer(fSubNet);
  if p = nil then
    exit;
  n := PDALen(PAnsiChar(p) - _DALEN)^ + _DAOFF; // process all masks
  repeat // SetLength(SubNet[].IP, SubNet[].IPCount) without any realloc
    PDALen(PAnsiChar(pointer(p^.IP)) - _DALEN)^ := p^.IPCount - _DAOFF;
    inc(result, p^.IPCount);
    inc(p);
    dec(n);
  until n = 0;
end;

function TIp4SubNets.LoadFrom(const buffer: RawBytestring): integer;
begin
  Clear;
  if (buffer <> '') and
     (PCardinal(buffer)^ = IP4SUBNET_MAGIC) then
    result := LoadFromBinary(buffer)
  else
    result := AddFromText(buffer);
  if AfterAdd = result then
    exit;
  Clear; // paranoid
  result := -1;
end;

function IP4SubNetMatch(P: PCardinalArray; ip4: cardinal): boolean;
var
  n: integer;
begin
  if (P <> nil) and
     (P^[0] = IP4SUBNET_MAGIC) then
  begin
    result := true;
    n := P^[1]; // try all masks - warning: won't check for buffer overflow
    repeat
      if FastFindIntegerSorted(@P^[4], P^[3] - 1, ip4 and P^[2]) >= 0 then
        exit;
      P := @P^[P^[3] + 2]; // O(log(n)) search the binary buffer in-place
      dec(n);
    until n = 0;
  end;
  result := false;
end;

function IP4SubNetMatch(const bin: RawByteString; const ip4: RawUtf8): boolean;
var
  ip32: cardinal;
begin
  result := (bin <> '') and
            (PCardinal(bin)^ = IP4SUBNET_MAGIC) and
            NetIsIP4(pointer(ip4), @ip32) and
            IP4SubNetMatch(pointer(bin), ip32{%H-});
end;


{ TUri }

procedure TUri.Clear;
begin
  Https := false;
  Layer := nlTcp;
  UriScheme := usUndefined;
  Finalize(self); // reset all RawUtf8 fields
end;

const
  _US: array[usHttp .. high(TUriScheme)] of RawUtf8 = (
    'http', 'ws', 'https', 'wss', 'udp', 'file', 'ftp', 'ftps');
  _US_PORT: array[TUriScheme] of RawUtf8 = (
    '', '', '80', '80', '443', '443', '', '', '20', '989');

function TUri.From(aUri: RawUtf8; const DefaultPort: RawUtf8): boolean;
var
  p, s, p1, p2: PAnsiChar;
  i: integer;
begin
  Clear;
  result := false;
  TrimSelf(aUri);
  if aUri = '' then
    exit;
  // parse Scheme
  p := pointer(aUri);
  s := p;
  while s^ in ['a'..'z', 'A'..'Z', '+', '-', '.', '0'..'9'] do
    inc(s);
  UriScheme := usHttp; // fallback to http:// if no scheme specified
  if PInteger(s)^ and $ffffff = ord(':') + ord('/') shl 8 + ord('/') shl 16 then
  begin
    FastSetString(Scheme, p, s - p);
    UriScheme := TUriScheme(FindPropName(@_US, Scheme, length(_US)) + ord(low(_US)));
    case UriScheme of
      usHttps,
      usWss:  // wss:// is just an upgraded https:
        Https := true;
      usUdp:  // 'udp://server:port'
        Layer := nlUdp;
      usFile: // https://en.wikipedia.org/wiki/File_URI_scheme#Number_of_slash_characters
        if PWord(s + 3)^ = ord('/') + ord('/') shl 8 then
          inc(s, 2); // support 'file:////server/folder/data.xml' form
    end;
    p := s + 3;
  end;
  // parse Server
  if NetStartWith(pointer(p), 'UNIX:/') then
  begin
    inc(p, 5); // 'http://unix:/path/to/socket.sock:/url/path'
    Layer := nlUnix;
    s := p;
    while not (s^ in [#0, ':']) do
      inc(s); // Server='/path/to/socket.sock'
  end
  else
  begin
    p1 := pointer(PosChar(pointer(p), '@'));
    if p1 <> nil then
    begin
      // parse 'https://user:password@server:port/address'
      p2 := pointer(PosChar(pointer(p), '/'));
      if (p2 = nil) or
         (PtrUInt(p2) > PtrUInt(p1)) then
      begin
        FastSetString(User, p, p1 - p);
        i := PosExChar(':', User);
        if i <> 0 then
        begin
          Password := copy(User, i + 1, 1000);
          SetLength(User, i - 1);
        end;
        p := p1 + 1;
      end;
    end;
    s := p;
    while not (s^ in [#0, ':', '/', '?']) do
      inc(s); // 'server:port/address' or 'server/address'
  end;
  FastSetString(Server, p, s - p);
  // optional Port
  if Server <> '' then // we need a server to have a port
    if s^ = ':' then
    begin
      inc(s);
      p := s;
      while not (s^ in [#0, '/']) do
        inc(s);
      FastSetString(Port, p, s - p); // Port='' for nlUnix
    end
    else if DefaultPort <> '' then
      Port := DefaultPort
    else
      Port := _US_PORT[UriScheme];
  // all the remaining text is the Address
  if s^ <> #0 then // ':' or '/' or '?'
  begin
    if s^ <> '?' then
      inc(s);
    FastSetString(Address, s, StrLen(s));
  end;
  if Server <> '' then
    result := true;
end;

function TUri.Same(const aServer, aPort: RawUtf8; aHttps: boolean): boolean;
begin
  result := (aHttps = Https) and
            PropNameEquals(aServer, Server) and
            (GetCardinal(pointer(aPort)) = PortInt);
end;

function TUri.SameUri(const aUri: RawUtf8): boolean;
var
  u: TUri;
begin
  result := u.From(aUri) and
            PropNameEquals(u.Scheme, Scheme) and
            u.Same(Server, Port, Https);
end;

function TUri.URI: RawUtf8;
begin
  Join([ServerPort, Address], result);
end;

function TUri.ServerPort: RawUtf8;
begin
  result := '';
  if layer = nlUnix then
  begin
    Join(['http://unix:', Server, ':/'], result); // our own layout
    exit;
  end;
  if UriScheme = usUndefined then // fields directly set, without any From()
    if Layer = nlUdp then
      UriScheme := usUdp
    else if Server = '' then
      exit // void e.g. just after Clear - http/https requires a server anyway
    else if Https then
      UriScheme := usHttps
    else
      UriScheme := usHttp;
  if UriScheme = usCustom then
    result := Scheme // as specified
  else
    result := _US[UriScheme]; // normalized or default 'http://'
  if result <> '' then
    if (Port = '') or
       (Port = '0') or
       (Port = _US_PORT[UriScheme]) then
      result := Join([result, '://', Server, '/'])
    else
      result := Join([result, '://', Server, ':', Port, '/']);
end;

function TUri.PortInt: TNetPort;
begin
  result := GetCardinal(pointer(Port));
end;

function TUri.Root: RawUtf8;
begin
  result := Split(Address, '?');
end;

function TUri.ResourceName: RawUtf8;
var
  i: PtrInt;
begin
  result := Root;
  for i := length(result) - 1 downto 1 do // - 1 to process '/url/file/' as 'file'
    if result[i] = '/' then
    begin
      delete(result, 1, i);
      break;
    end;
end;

function TUri.UserPasswordBase64: RawUtf8;
begin
  if User = '' then
    result := ''
  else
    result := NetBinToBase64(Join([User, ':', Password]));
end;


{ ********* TCrtSocket Buffered Socket Read/Write Class }

{ TCrtSocket }

function TCrtSocket.GetAborted: boolean;
begin
  result := fAborted in fFlags;
end;

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

constructor TCrtSocket.Create(aTimeOut: integer);
begin
  fTimeOut := aTimeOut;
end;

constructor TCrtSocket.Open(const aServer, aPort: RawUtf8;
  aLayer: TNetLayer; aTimeOut: cardinal; aTLS: boolean;
  aTLSContext: PNetTlsContext; aTunnel: PUri);
begin
  // call main virtual constructor
  Create(aTimeOut); // default read timeout is 10 seconds
  // copy the input parameters before OpenBind()
  if aTLSContext <> nil then
    TLS := aTLSContext^;
  if (aTunnel <> nil) and
     (aTunnel^.Server <> '') then
    Tunnel := aTunnel^;
  // OpenBind() raise an exception on error
  OpenBind(aServer, aPort, {dobind=}false, aTLS, aLayer);
  if aTLSContext <> nil then
    aTLSContext^ := TLS; // copy back information to the caller TNetTlsContext
end;

procedure TCrtSocket.DoRaise(const msg: string; const args: array of const;
  error: TNetResult; errnumber: system.PInteger; exc: ENetSockClass);
begin
  if exc = nil then
    exc := ENetSock;
  raise exc.Create(msg, self, args, error, errnumber);
end;

procedure TCrtSocket.DoRaise(const msg: string);
begin
  raise ENetSock.Create(msg, self, [], nrOk, nil);
end;

constructor TCrtSocket.OpenUri(const aUri: TUri; const aUriFull, aTunnel: RawUtf8;
  aTimeOut: cardinal; aTLSContext: PNetTlsContext);
var
  t: TUri;
begin
  if aUri.Server = '' then
    DoRaise('OpenUri(%s): invalid URI', [aUriFull]);
  fOpenUriFull := aUriFull;
  t.From(aTunnel);
  Open(aUri.Server, aUri.Port, nlTcp, aTimeOut, aUri.Https, aTLSContext, @t);
end;

constructor TCrtSocket.OpenUri(const aUri: RawUtf8; out aAddress: RawUtf8;
  const aTunnel: RawUtf8; aTimeOut: cardinal; aTLSContext: PNetTlsContext);
var
  u: TUri;
begin
  u.From(aUri); // e.g. 'file:///path/to' = false (since Server='') but is valid
  OpenUri(u, aUri, aTunnel, aTimeOut, aTLSContext);
  aAddress := u.Address;
end;

constructor TCrtSocket.Bind(const aAddress: RawUtf8; aLayer: TNetLayer;
  aTimeOut: integer; aReusePort: boolean);
begin
  Create(aTimeOut);
  BindPort(aAddress, aLayer, aReusePort);
end;

const
  BINDTXT: array[boolean] of string[7] = (
    'open', 'bind');
  BINDMSG: array[boolean] of string = (
    'Is a server available on this address:port?',
    'Port may be invalid or already bound by another process!');

procedure TCrtSocket.BindPort(const aAddress: RawUtf8; aLayer: TNetLayer;
  aReusePort: boolean);
var
  s, p: RawUtf8;
  aSock: integer;
begin
  if aAddress = '' then
  begin
    {$ifdef OSLINUX} // try systemd activation
    if not sd.IsAvailable then
      DoRaise('Bind('''') but Systemd is not available');
    if sd.listen_fds(0) > 1 then
      DoRaise('Bind(''''): Systemd activation failed - too many file descriptors');
    aSock := SD_LISTEN_FDS_START + 0;
    {$else}
    DoRaise('Bind(''''), i.e. Systemd activation, is not allowed on this platform');
    aSock := 0; // make compiler happy
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
  OpenBind(s{%H-}, p{%H-}, {dobind=}true, {tls=}false, aLayer,
    {%H-}TNetSocket(aSock), aReusePort);
  {$ifdef OSLINUX}
  // in case started by systemd (port=''), listening socket is created by
  // another process and do not interrupt when it got a signal. So we need to
  // set a timeout to unlock accept() periodically and check for termination
  if aAddress = '' then     // external socket
    ReceiveTimeout := 1000; // unblock accept every second
  {$endif OSLINUX}
end;

function TCrtSocket.ServerTls: boolean;
begin
  result := (fServerTlsEnabled in fFlags); // properly set by OpenBind()
end;

const
  CSTA_TXT: array[TCrtSocketTlsAfter] of AnsiChar = 'CBA';

procedure TCrtSocket.DoTlsAfter(caller: TCrtSocketTlsAfter);
begin
  if fSecure = nil then // ignore duplicated calls
  try
    if not Assigned(NewNetTls) then
      DoRaise('DoTlsAfter: TLS support not compiled ' +
        '- try including mormot.lib.openssl11 in your project');
    fSecure := NewNetTls;
    if fSecure = nil then
      DoRaise('DoTlsAfter: TLS is not available - try installing OpenSSL');
    case caller of
      cstaConnect:
        fSecure.AfterConnection(fSock, TLS, fServer);
      cstaBind:
        fSecure.AfterBind(fSock, TLS, fServer);
      cstaAccept:
        fSecure.AfterAccept(fSock, TLS, @TLS.LastError, @TLS.CipherName)
    end;
    if Assigned(OnLog) and
       (caller <> cstaBind) then
      OnLog(sllTrace, 'DoTlsAfter(%%:%): %',
        [CSTA_TXT[caller], fServer, fPort, TLS.CipherName], self);
    TLS.Enabled := true; // set the flag AFTER fSecure has been initialized
  except
    on E: Exception do
    begin
      fSecure := nil; // reset TLS context
      DoRaise('DoTlsAfter: TLS failed [%s %s]', [ClassNameShort(E)^, E.Message]);
    end;
  end;
end;

procedure TCrtSocket.ConnectUri(const aUri: RawUtf8; aAddress: PRawUtf8);
var
  u: TUri;
begin
  if not u.From(aUri) then
    DoRaise('ConnectUri(%s): invalid URI', [aUri]);
  OpenBind(u.Server, u.Port, {doBind=}false, u.Https);
  if aAddress <> nil then
    aAddress^ := u.Address;
end;

procedure TCrtSocket.OpenBind(const aServer, aPort: RawUtf8; doBind,
  aTLS: boolean; aLayer: TNetLayer; aSock: TNetSocket; aReusePort: boolean);
var
  retry: integer;
  s: RawUtf8;
  res: TNetResult;
  addr: TNetAddr;
begin
  ResetNetTlsContext(TLS); // TLS.Enabled is set at output if aTLS=true
  fSocketLayer := aLayer;
  fSocketFamily := nfUnknown;
  fFlags := [];
  if doBind then
    include(fFlags, fWasBind);
  if aTLS then
    include(fFlags, fServerTlsEnabled); // for proper reconnection
  if {%H-}PtrInt(aSock) <= 0 then
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
      // HTTP(S) tunnelling via CONNECT - see also THttpClientSocket.OpenBind
      fProxyUrl := Tunnel.URI;
      if Tunnel.Https and aTLS then
        // single TLS parameter for either the Tunnel or the destination
        DoRaise('Open(%s:%s): %s proxy - unsupported dual TLS layers',
          [fServer, fPort, fProxyUrl]);
      res := nrOk;
      try
        res := NewSocket(Tunnel.Server, Tunnel.Port, nlTcp, {doBind=}false,
          fTimeout, fTimeout, fTimeout, {retry=}2, fSock, @addr);
        if res = nrOK then
        begin
          addr.IP(fRemoteIP, true);
          fSocketFamily := addr.Family;
          include(fFlags, fProxyConnect);
          res := nrRefused;
          if Tunnel.Https then
            DoTlsAfter(cstaConnect); // the proxy requires a TLS connection
          SockSendLine(['CONNECT ', fServer, ':', fPort, ' HTTP/1.0']);
          if Tunnel.User <> '' then
            SockSendLine(['Proxy-Authorization: Basic ', Tunnel.UserPasswordBase64]);
          SockSendFlush(#13#10);
          repeat
            SockRecvLn(s);
            if NetStartWith(pointer(s), 'HTTP/') and
               (length(s) > 11) and
               (s[10] = '2') then // 'HTTP/1.1 2xx xxxx' success
              res := nrOK;
          until s = ''; // end of response headers
        end;
      except
        on E: Exception do
          DoRaise('Open(%s:%s): %s proxy error %s',
            [fServer, fPort, fProxyUrl, E.Message]);
      end;
      if res <> nrOk then
        DoRaise('Open(%s:%s): %s proxy error',
          [fServer, fPort, fProxyUrl], res);
      if Assigned(OnLog) then
        OnLog(sllTrace, 'Open(%:%) via proxy CONNECT %',
          [fServer, fPort, fProxyUrl], self);
      if aTLS then
        DoTlsAfter(cstaConnect); // raw TLS negotation after CONNECT
      exit;
    end
    else
      // direct client connection
      retry := {$ifdef OSBSD} 10 {$else} 2 {$endif};
    s := fServer;
    {$ifdef OSPOSIX}
    // check if aServer is 'unix:/path/to/myapp.socket' with default nlTcp
    if (aLayer = nlTcp) and
       NetStartWith(pointer(s), 'UNIX:') then
    begin
      aLayer := nlUnix;
      delete(s, 1, 5);
    end;
    {$endif OSPOSIX}
    //if Assigned(OnLog) then
    //  OnLog(sllTrace, 'Before NewSocket', [], self);
    res := NewSocket(s, fPort, aLayer, doBind, fTimeout, fTimeout, fTimeout,
                     retry, fSock, @addr, aReusePort);
    //if Assigned(OnLog) then
    //  OnLog(sllTrace, 'After NewSocket=%', [_NR[res]], self);
    addr.IP(fRemoteIP, true);
    if res <> nrOK then
      DoRaise('OpenBind(%s:%s): %s [remoteip=%s]',
        [fServer, fPort, BINDMSG[doBind], fRemoteIP], res);
    fSocketFamily := addr.Family;
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
     aTLS then
    if doBind then
      DoTlsAfter(cstaBind) // never called by OpenBind(aTLS=false) in practice
    else if {%H-}PtrInt(aSock) <= 0 then
      DoTlsAfter(cstaConnect);
  if Assigned(OnLog) then
    OnLog(sllTrace, '%(%:%) sock=% %', [BINDTXT[doBind], fServer, fPort,
      pointer(fSock.Socket), TLS.CipherName], self);
end;

function TCrtSocket.ReOpen(aTimeout: cardinal): string;
begin
  try
    Close;
    OpenBind(fServer, fPort, fWasBind in fFlags, ServerTls);
    if SockConnected then
      result := '' // success
    else
      result := 'Not connected';
  except
    on E: Exception do
      result := E.Message;
  end;
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
    aClientAddr^.IP(fRemoteIP, RemoteIPLocalHostAsVoidInServers);
  {$ifdef OSLINUX}
  if Assigned(OnLog) then
    OnLog(sllTrace, 'Accept(%:%) sock=% %',
      [fServer, fPort, fSock.Socket, fRemoteIP], self);
  {$endif OSLINUX}
end;

function TCrtSocket.SockIsDefined: boolean;
begin
  result := (self <> nil) and
            ({%H-}PtrInt(fSock) > 0);
end;

type
  PTextRec = ^TTextRec;
  TTextRecUserData = record // consume some of TTextRec.UserData 32 bytes
    Owner: TCrtSocket;
    LastIoResult: integer;
    LastRawError: integer;
    LastNetResult: TNetResult;
  end;
  PTextRecUserData = ^TTextRecUserData;

function TextRecUserData(const F: TTextRec): PTextRecUserData;
  {$ifdef HASINLINE} inline; {$endif}
begin
  result := @F.UserData; // we have up to 32 bytes to store our context
end;

function InputSock(var F: TTextRec): integer;
// SockIn pseudo text file fill its internal buffer only with available data
// -> no unwanted wait time is added
// -> very optimized for readln/SockRecvLn/SockInReadLn in HTTP headers
var
  size: integer;
  addr: TNetAddr;
  usr: PTextRecUserData;
begin
  F.BufEnd := 0;
  F.BufPos := 0;
  usr := TextRecUserData(F);
  result := usr^.LastIoResult;
  if result <> 0 then
    exit; // already reached error (paranoid)
  if usr^.Owner.SockIsDefined then
  begin
    size := F.BufSize;
    if usr^.Owner.SocketLayer = nlUdp then
    begin
      size := usr^.Owner.Sock.RecvFrom(F.BufPtr, size, addr);
      if size < 0 then
        usr^.LastNetResult := NetLastError(NO_ERROR, @usr^.LastRawError)
      else
        addr.IPWithPort(usr^.Owner.fRemoteIP); // set 'remoteip:port'
    end
    else // nlTcp/nlUnix: TrySockRecv() return size=0 on nrRetry
      if not usr^.Owner.TrySockRecv(F.BufPtr, size, {StopBeforeLength=}true,
                          @usr^.LastNetResult, @usr^.LastRawError) then
        size := -1; // fatal socket error
    if size >= 0 then
    begin
      F.BufEnd := size;
      inc(usr^.Owner.fBytesIn, size);
      result := NO_ERROR;
      exit; // success
    end;
    if usr^.LastNetResult = nrOk then
      usr^.LastNetResult := nrUnknownError;
  end
  else
    usr^.LastNetResult := nrNoSocket;  // file closed
  result := -usr^.LastRawError;        // ioresult = socket error as negative
  if result = 0 then
    result := ord(usr^.LastNetResult); // ioresult = ord(TNetResult) positive
  usr^.LastIoResult := result;
  // result <> 0 will update ioresult and raise an exception if {$I+}
end;

function CloseSock(var F: TTextRec): integer;
begin
  with TextRecUserData(F)^ do
    if Owner <> nil then
    begin
      Owner.Close;
      Owner := nil;
    end;
  result := NO_ERROR;
end;

function OpenSock(var F: TTextRec): integer;
begin
  F.BufPos := 0;
  F.BufEnd := 0;
  F.CloseFunc := @CloseSock;
  if F.Mode = fmInput then
  begin
    // ReadLn - as used from SockIn^ text pseudo-file
    F.InOutFunc := @InputSock;
    F.FlushFunc := nil;
    result := NO_ERROR;
  end
  else
    // WriteLn - as was used from CreateSockOut/SockOut^ in mORMot 1
    result := WSAEINVAL; // deprecated
end;

{$ifdef FPC}
procedure SetLineBreakStyle(var T: Text; Style: TTextLineBreakStyle);
begin
  case Style of
    tlbsCR:
      TextRec(T).LineEnd := #13; // FPC specific
    tlbsLF:
      TextRec(T).LineEnd := #10;
    tlbsCRLF:
      TextRec(T).LineEnd := #13#10;
  end;
end;
{$endif FPC}

procedure DoInputSock(r: PTextRec; const ctx: ShortString; notvoid: boolean);
var
  res: integer;
begin
  res := InputSock(r^);
  if res <> NO_ERROR then
    with TextRecUserData(r^)^ do
      Owner.DoRaise('%s', [ctx], LastNetResult, @LastRawError);
  if notvoid and
     (r^.BufEnd = r^.BufPos) then
    TextRecUserData(r^)^.Owner.DoRaise('%s: void or timeout', [ctx]);
end;

const
  SOCKMINBUFSIZE = 1024; // big enough for headers (body is read directly)

procedure TCrtSocket.CreateSockIn(LineBreak: TTextLineBreakStyle;
  InputBufferSize: integer);
var
  rec: PTextRec;
begin
  if (self = nil) or
     (fSockIn <> nil) then
    exit;
  if InputBufferSize < SOCKMINBUFSIZE then
    InputBufferSize := SOCKMINBUFSIZE;
  GetMem(fSockIn, SizeOf(rec^) + InputBufferSize);
  rec := pointer(fSockIn);
  FillCharFast(rec^, SizeOf(rec^), 0);
  TextRecUserData(rec^)^.Owner := self;
  rec^.Mode := fmClosed;
  // ignore internal Buffer[], which is not trailing on latest Delphi and FPC
  rec^.BufSize := InputBufferSize;
  rec^.BufPtr := pointer(PAnsiChar(rec) + SizeOf(rec^));
  rec^.OpenFunc := @OpenSock;
  rec^.Handle := {$ifdef FPC}THandle{$endif}(0); // some invalid handle
  SetLineBreakStyle(fSockIn^, LineBreak); // e.g. HTTP or SMTP requires #13#10
  Reset(fSockIn^);
end;

procedure TCrtSocket.CloseSockIn;
begin
  if (self = nil) or
     (fSockIn = nil) then
    exit;
  FreeMem(fSockIn);
  fSockIn := nil;
end;

{ $define SYNCRTDEBUGLOW2}

procedure TCrtSocket.Close;
// notice: sequential Close + OpenBind sets should work with the same instance
var
  rec: PTextRec;
{$ifdef SYNCRTDEBUGLOW2}
var // closesocket() or shutdown() are slow e.g. on Windows with wrong Linger
  start, stop: int64;
{$endif SYNCRTDEBUGLOW2}
begin
  // reset internal state
  fSndBufLen := 0; // always reset (e.g. in case of further Open after error)
  ioresult;        // reset readln/writeln value
  rec := pointer(fSockIn);
  if rec <> nil then
  begin
    rec^.BufPos := 0;  // reset input buffer, but keep allocated
    rec^.BufEnd := 0;
    with TextRecUserData(rec^)^ do // reset error flags, keeping Owner
    begin
      LastIoResult := 0;
      LastRawError := 0;
      LastNetResult := nrOk;
    end;
  end;
  if not SockIsDefined then
    exit; // no opened connection, or Close already executed
  // perform the TLS shutdown on socket and release the TLS execution interface
  fSecure := nil; // will depend on the actual implementation class
  // note: ResetNetTlsContext(TLS) is done in OpenBind()
  // actually close the socket and mark it as not SockIsDefined (<0)
  {$ifdef SYNCRTDEBUGLOW2}
  QueryPerformanceMicroSeconds(start);
  {$endif SYNCRTDEBUGLOW2}
  {$ifdef OSLINUX}
  if not (fWasBind in fFlags) or
     (fPort <> '') then // no explicit shutdown necessary on Linux server side
  {$endif OSLINUX}
    fSock.ShutdownAndClose({rdwr=}(fWasBind in fFlags));
  {$ifdef SYNCRTDEBUGLOW2}
  QueryPerformanceMicroSeconds(stop);
  TSynLog.Add.Log(sllTrace, 'ShutdownAndClose(%): %', [fWasBind, stop-start], self);
  {$endif SYNCRTDEBUGLOW2}
  fSock := TNetSocket(-1);
  // don't reset fServer/fPort/fTls/fWasBind: caller may use them to reconnect
  // (see e.g. THttpClientSocket.Request)
  {$ifdef OSPOSIX}
  if fSocketLayer = nlUnix then
    FpUnlink(pointer(fServer)); // 'unix:/path/to/myapp.socket' -> delete file
  {$endif OSPOSIX}
end;

procedure TCrtSocket.Abort;
begin
  if (self = nil) or
     (fAborted in fFlags) then
    exit;
  include(fFlags, fAborted); // global flag checked within most recv/send loops
  if Assigned(OnLog) then
    OnLog(sllTrace, 'Abort socket=%', [fSock.Socket], self);
  if SockIsDefined then
    {$ifdef OSWINDOWS}
    // closing will abort any pending Windows recv/send call in another thread
    fSock.Close;
    {$else}
    // shutdown should do the trick on Linux, without messing the file descriptor
    shutdown(fSock.Socket, SHUT_RDWR);
    {$endif OSWINDOWS}
end;

destructor TCrtSocket.Destroy;
begin
  Close;
  CloseSockIn;
  inherited Destroy;
end;

function TCrtSocket.SockInReadLn(Buffer: PAnsiChar; Size: PtrInt): PtrInt;
var
  len, line: PtrInt;
  read, err: integer;
  res: TNetResult;
  p: PUtf8Char;
  c: AnsiChar;
  r: PTextRec;

  function GetSockInLineLength: PtrInt; {$ifdef FPC} inline; {$endif}
  var
    available: PtrInt;
  begin
    repeat
      available := r^.BufEnd - r^.BufPos;
      if available > 0 then
      begin
        len := available;
        p := @r^.BufPtr[r^.BufPos];
        result := BufferLineLength(p, p + available); // SSE2 asm on x86-64
        exit;
      end;
      DoInputSock(r, 'SockInReadLn', {notvoid=}true);
    until fAborted in fFlags;
    result := 0;
  end;

begin
  result := 0;
  r := pointer(fSockIn);
  if r = nil then
  begin
    // no SockIn^ buffer -> need multiple sockets API calls
    repeat
      read := 1; // one syscall per char: may be very slow on Windows
      if not TrySockRecv(@c, read, {StopBeforeLength=}false, @res, @err) then
        case res of
          nrRetry:
            begin
              SleepHiRes(1); // this is a slow loop anyway
              continue;
            end;
          nrClosed:
            break; // stop at end of input stream
        else
          DoRaise('SockInReadLn: TrySockRecv(c) failed', [], res, @err);
        end
      else if read <> 1 then
        DoRaise('SockInReadLn: TrySockRecv(c) read=%d', [read]);
      if c <> #13 then
        if c = #10 then
          break
        else if Buffer <> nil then // Buffer=nil to go to end of line
        begin
          Buffer[result] := c;
          inc(result);
          if result = Size then
          begin
            result := -1; // avoid buffer overflow: notify Buffer is too short
            exit;
          end;
        end;
    until fAborted in fFlags;
    if Buffer <> nil then
      Buffer[result] := #0;
    exit;
  end;
  if Buffer <> nil then
  begin
    // read the next line content from SockIn^ into Buffer^
    repeat
      line := GetSockInLineLength;
      if line >= Size then
      begin
        result := -1; // avoid buffer overflow
        exit;
      end;
      MoveFast(p^, Buffer^, line);
      inc(r^.bufpos, line);
      inc(Buffer, line);
      inc(result, line);
      if (line <= Size) and
         (line < len) then
        break; // we got a line
      dec(Size, line);
    until fAborted in fFlags;
    Buffer[0] := #0;
    inc(p, line);
    dec(len, line);
    line := 0;
  end
  else
  begin
    len := 0; // called with Buffer = nil to go to end of line
    line := 0;
  end;
  // read the next line feed
  repeat
    while line < len do
    begin
      inc(r^.bufpos);
      case p[line] of
        #10:
          exit;
        #13:
          inc(line); // loop to handle the following #10
      else // p[line] should be either #10 or #13
        DoRaise('SockInReadln: BufferLineLength^=#%d', [ord(p[line])]);
      end;
    end;
    line := GetSockInLineLength;
    inc(r^.bufpos, line); // just ignore any text up to the line feed
  until fAborted in fFlags;
end;

function TCrtSocket.SockInRead(Content: PAnsiChar; Length: PtrInt;
  UseOnlySockIn: boolean): integer;
var
  len: PtrInt;
  r: PTextRec;
// read Length bytes from SockIn^ buffer + Sock if necessary
begin
  // get data from SockIn buffer, if any (faster than ReadChar)
  result := 0;
  if Length <= 0 then
    exit;
  r := pointer(fSockIn);
  if r <> nil then
    repeat
      len := r^.BufEnd - r^.BufPos;
      if len > 0 then
      begin
        if len > Length then
          len := Length;
        MoveFast(r^.BufPtr[r^.BufPos], Content^, len);
        inc(r^.BufPos, len);
        inc(Content, len);
        inc(result, len);
        dec(Length, len);
      end;
      if (Length = 0) or
         (fAborted in fFlags) then
        exit; // we got everything we wanted
      if not UseOnlySockIn then
        break;
      if Timeout = 0 then
        SleepHiRes(0); // don't burn 100% of CPU
      DoInputSock(r, 'SockInRead', {notvoid=}false);
    until fAborted in fFlags;
  // direct receiving of the remaining bytes from socket
  if (Length <= 0) or
     (fAborted in fFlags) then
    exit;
  SockRecv(Content, Length); // raise ENetSock if failed to read Length
  inc(result, Length);
end;

function TCrtSocket.SockInRead(Length: integer; UseOnlySockIn: boolean): RawByteString;
begin
  if (self = nil) or
     (Length <= 0) or
     (SockInRead(FastSetString(RawUtf8(result), Length),
                 Length, UseOnlySockIn) <> Length) then
    result := '';
end;

function TCrtSocket.SockInPending(aTimeOutMS: integer): integer;
var
  backup: PtrInt;
begin
  if aTimeOutMS < 0 then
    DoRaise('SockInPending(-1)');
  // first try in SockIn^.Buffer
  result := 0;
  if SockIn <> nil then
    with PTextRec(SockIn)^ do
      result := BufEnd - BufPos;
  if result <> 0 then
    exit;
  // no data in SockIn^.Buffer, so try if some pending at socket/TLS level
  case SockReceivePending(aTimeOutMS) of // check both TLS and socket levels
    cspDataAvailable,
    cspDataAvailableOnClosedSocket:
      if SockIn <> nil then
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
      end
      else
        result := Sock.HasData; // using FIONREAD
    cspSocketError:
      result := -1; // indicates broken socket
    cspSocketClosed:
      result := -2; // indicates closed socket
  end; // cspNoData will leave result=0
end;

function TCrtSocket.SockConnected: boolean;
var
  addr: TNetAddr;
begin
  result := SockIsDefined and
            (fSock.GetPeer(addr) = nrOK); // OS may return ENOTCONN/WSAENOTCONN
end;

function EnsureSockSendResize(var buf: RawByteString; Len: PtrInt): pointer;
  {$ifdef HASINLINEDELPHI} inline; {$endif}
var
  cap: PtrInt;
begin
  cap := Length(buf);
  SetLength(buf, Len + cap + cap shr 3 + 2048); // generous 2KB provision
  result := pointer(buf);
end;

function TCrtSocket.EnsureSockSend(Len: PtrInt): PUtf8Char;
begin
  result := pointer(fSndBuf);
  if (result = nil) or
     (Len + fSndBufLen > PStrLen(result - _STRLEN)^) then
    result := EnsureSockSendResize(fSndBuf, Len);
  inc(result, fSndBufLen);
  inc(fSndBufLen, Len);
end;

procedure TCrtSocket.SockSend(P: pointer; Len: PtrInt);
begin
  if Len > 0 then
    MoveFast(P^, EnsureSockSend(Len)^, Len);
end;

procedure TCrtSocket.SockSendCRLF;
begin
  PWord(EnsureSockSend(2))^ := EOLW;
end;

procedure TCrtSocket.SockSendW(w: PWideChar; l: PtrInt);
var
  p: PUtf8Char;
  tmp: TBuffer8K;
begin
  if w <> nil then
    if IsAnsiCompatibleW(w, l) then
    begin
      p := EnsureSockSend(l);
      repeat
        dec(l);
        p[l] := AnsiChar(w[l]);
      until l = 0;
    end
    else
      SockSend(@tmp, Unicode_WideToAnsi(w, @tmp, l, SizeOf(tmp), CP_UTF8));
end;

procedure TCrtSocket.SockSend(const Values: array of const);
var
  v: PVarRec;
  i: PtrInt;
  t: PAnsiChar;
  tmp: TTemp24;
begin
  v := @Values[0];
  for i := 0 to high(Values) do
  begin
    case v^.VType of // only most common arguments are supported
      vtString:
        SockSend(@v^.VString^[1], PByte(v^.VString)^);
      vtAnsiString:
        if v^.VAnsiString <> nil then
          SockSend(v^.VAnsiString, PStrLen(v^.VPChar - _STRLEN)^);
      vtPWideChar,
      vtWideString:
        if v^.VWideString <> nil then
          SockSendW(v^.VWideString, StrLenW(v^.VWideString));
      {$ifdef HASVARUSTRING}
      vtUnicodeString:
        if v^.VUnicodeString <> nil then
          SockSendW(v^.VUnicodeString, PStrLen(v^.VPChar - _STRLEN)^);
      {$endif HASVARUSTRING}
      vtPChar:
        SockSend(v^.VPChar, StrLen(v^.VPChar));
      vtChar:
        SockSend(@v^.VChar, 1);
      vtWideChar:
        SockSend(@v^.VWideChar, 1); // expects a 7-bit ASCII character
      vtInteger:
        begin
          t := StrInt32(@tmp[23], v^.VInteger);
          SockSend(t, @tmp[23] - t);
        end;
      {$ifdef FPC} vtQWord, {$endif}
      vtInt64: // e.g. for "Content-Length:" or  "Range:" sizes
        begin
          t := StrInt64(@tmp[23], v^.VInt64^);
          SockSend(t, @tmp[23] - t);
        end;
    else
      DoRaise('SockSend: unsupported VType=%d', [v^.VType]); // paranoid
    end;
    inc(v);
  end;
  SockSendCRLF;
end;

procedure TCrtSocket.SockSendLine(const Values: array of RawUtf8);
var
  i, len: PtrInt;
  p: PUtf8Char;
begin
  len := 2; // for trailing CRLF
  for i := 0 to high(Values) do
    inc(len, length(Values[i]));
  p := EnsureSockSend(len); // reserve all needed memory at once
  for i := 0 to high(Values) do
  begin
    len := length(Values[i]);
    MoveFast(pointer(Values[i])^, p^, len);
    inc(p, len);
  end;
  PWord(p)^ := EOLW;
end;

procedure TCrtSocket.SockSend(const Line: RawByteString);
var
  len: PtrInt;
  p: PUtf8Char;
begin
  len := length(Line);
  p := EnsureSockSend(len + 2);
  MoveFast(pointer(Line)^, p^, len);
  PWord(p + len)^ := EOLW;
end;

procedure TCrtSocket.SockSendHeaders(const headers: RawUtf8);
var
  p, pend, d: PUtf8Char;
  len: PtrInt;
begin
  p := pointer(headers);
  if p = nil then
    exit;
  pend := p + PStrLen(p - _STRLEN)^;
  repeat
    while p^ <= ' ' do
      if p^ <> #0 then
        inc(p) // trim spaces, and ignore any kind of line feed or void line
      else
        exit;  // end of input
    len := BufferLineLength(p, pend); // use SSE2 on x86-64 - we know len <> 0
    d := EnsureSockSend(len + 2);     // reserve enough space at once
    MoveFast(p^, d^, len);            // append line content
    PWord(d + len)^ := EOLW;          // normalize line end
    inc(p, len);
  until false;
end;

function TCrtSocket.SockSendRemainingSize: PtrInt;
begin
  result := Length(fSndBuf) - fSndBufLen;
end;

function TCrtSocket.SockSendFlush(const aBody: RawByteString;
  aNoRaise: boolean): TNetResult;
var
  bodylen, buflen: PtrInt;
  rawError: integer;
begin
  buflen := fSndBufLen;
  fSndBufLen := 0; // always reset the output buffer position
  result := nrOK;
  // check if we can send smallest body with the headers in a single syscall
  bodylen := Length(aBody);
  if (bodylen > 0) and
     (buflen + bodylen <= length(fSndBuf)) then // around 1800 bytes
  begin
    MoveFast(pointer(aBody)^, PByteArray(fSndBuf)[buflen], bodylen);
    inc(buflen, bodylen); // append to buffer as single TCP packet
    bodylen := 0;
  end;
  {$ifdef SYNCRTDEBUGLOW}
  if Assigned(OnLog) then
  begin
    OnLog(sllCustom2, 'SockSend sock=% flush len=% bodylen=% %',
      [fSock.Socket, buflen, Length(aBody),
       LogEscapeFull(pointer(fSndBuf), buflen)], self);
    if bodylen > 0 then
      OnLog(sllCustom2, 'SockSend sock=% bodylen len=% %',
        [fSock.Socket, bodylen, LogEscapeFull(pointer(aBody), bodylen)], self);
  end;
  {$endif SYNCRTDEBUGLOW}
  // actually send the internal buffer (headers + maybe body)
  if buflen > 0 then
    if not TrySndLow(pointer(fSndBuf), buflen, @result, @rawError) then
      if aNoRaise then
        exit
      else
        DoRaise('SockSendFlush(%s) len=%d',
          [fServer, buflen], result, @rawError);
  // direct sending of the remaining bodylen bytes (if needed)
  if bodylen > 0 then
    if not TrySndLow(pointer(aBody), bodylen, @result, @rawError) then
      if not aNoRaise then
        DoRaise('SockSendFlush(%s) bodylen=%d',
          [fServer, bodylen], result, @rawError);
end;

function TCrtSocket.SockSendStream(Stream: TStream; ChunkSize: integer;
  aNoRaise, aCheckRecv: boolean): TNetResult;
var
  chunk: RawByteString;
  rd, rawError: integer;
  pos: Int64;
begin
  result := nrOK;
  SetLength(chunk, ChunkSize);
  pos := 0;
  repeat
    rd := Stream.Read(pointer(chunk)^, ChunkSize);
    if rd <= 0 then
      break; // reached the end of the stream
    TrySndLow(pointer(chunk), rd, @result, @rawError); // error if result <> nrOk
    if aCheckRecv and  // always check for any response, e.g. on closed connection
       (fSecure = nil) and  // TLS fSecure.ReceivePending is not reliable
       (fSock.HasData > 0) then
    begin
      result := nrRetry; // received e.g. 413 HTTP_PAYLOADTOOLARGE
      break;
    end;
    if result <> nrOk then
      if aNoRaise then
        break
      else
        DoRaise('SockSendStream(%s,%d) rd=%d pos=%d to %s:%s',
          [ClassNameShort(Stream)^, ChunkSize, rd, pos, fServer, fPort],
          result, @rawError);
    inc(pos, rd);
  until false;
end;

procedure TCrtSocket.SockRecv(Buffer: pointer; Length: integer);
var
  read, rawError: integer;
  res: TNetResult;
begin
  read := Length;
  if not TrySockRecv(Buffer, read, {StopBeforeLength=}false, @res, @rawError) or
     (Length <> read) then
    DoRaise('SockRecv(%d) read=%d at %s:%s',
      [Length, read, fServer, fPort], res, @rawError);
end;

function TCrtSocket.SockRecv(Length: integer): RawByteString;
begin
  SockRecv(FastSetString(RawUtf8(result), Length), Length);
end;

function TCrtSocket.SockReceivePending(TimeOutMS: integer;
  loerr: system.PInteger): TCrtSocketPending;
var
  events: TNetEvents;
begin
  if loerr <> nil then
    loerr^ := 0;
  if SockIsDefined then
  begin
    if Assigned(fSecure) and
       (fSecure.ReceivePending > 0) then
    begin
      result := cspDataAvailable; // some data is available in the TLS buffers
      exit;
    end;
    // select() or poll() to check for incoming data on socket (even for TLS)
    events := fSock.WaitFor(TimeOutMS, [neRead, neError], loerr);
  end
  else
    events := [neError];
  if neError in events then
    result := cspSocketError
  else if neRead in events then
    if neClosed in events then
      result := cspDataAvailableOnClosedSocket // read+closed may coexist
    else
      result := cspDataAvailable
  else if neClosed in events then
    result := cspSocketClosed
  else
    result := cspNoData;
end;

function TCrtSocket.SockReceiveHasData: integer;
begin
  if SockIsDefined then
    if Assigned(fSecure) then
      result := fSecure.ReceivePending // data available in the TLS buffers
    else
      result := fSock.HasData // data available on the socket itself
  else
    result := 0;
end;

function TCrtSocket.SockReceiveString(
  NetResult: PNetResult; RawError: system.PInteger): RawByteString;
var
  read: integer;
  tmp: TBuffer64K; // big enough for INetTls or the socket API
begin
  read := SizeOf(tmp);
  if TrySockRecv(@tmp, read, {StopBeforeLength=}true, NetResult, RawError) and
     (read <> 0) then
    FastSetRawByteString(result, @tmp, read)
  else
    result := '';
end;

function TCrtSocket.TrySockRecv(Buffer: pointer; var Length: integer;
  StopBeforeLength: boolean; NetResult: PNetResult; RawError: system.PInteger): boolean;
var
  expected, read, pending: integer;
  events: TNetEvents;
  res: TNetResult;
begin
  if RawError <> nil then
    RawError^ := NO_ERROR;
  res := nrInvalidParameter;
  if SockIsDefined and
     (Buffer <> nil) and
     (Length > 0) and
     not (fAborted in fFlags) then
  begin
    expected := Length;
    Length := 0;
    repeat
      // first check for any available data
      // - some may be available at fSecure/TLS level, but not from fSock/TCP
      read := MinPtrInt(CrtSocketSendRecvMaxBytes, expected - Length);
      if fSecure <> nil then
        res := fSecure.Receive(Buffer, read)
      else
        res := fSock.Recv(Buffer, read, RawError);
      {$ifdef SYNCRTDEBUGLOW}
      if (res <> nrOk) and
         Assigned(OnLog) then
        OnLog(sllCustom2, 'TrySockRecv: sock=% Recv=% %',
          [fSock.Socket, read, SocketErrorMessage], self);
      {$endif SYNCRTDEBUGLOW}
      case res of
        nrOk:
          begin // Buffer^ was filled with read bytes
            inc(fBytesIn, read);
            inc(Length, read);
            inc(PByte(Buffer), read);
          end;
        nrRetry:
          begin
            res := nrOk; // make RecvPending + WaitFor below and retry Recv
            read := 0;
          end;
      else
        begin
          // no more to read, or socket closed/broken
          Close; // connection broken or socket closed gracefully
          break;
        end;
      end;
      if (fAborted in fFlags) or
         (Length = expected) or
         (StopBeforeLength and
          (read <> 0) and
          (read < CrtSocketSendRecvMaxBytes)) then
        break; // good enough for now
      if (res = nrOk) or
         ((fSock.RecvPending(pending) = nrOk) and
          (pending > 0)) then
        continue; // no need to call WaitFor()
      if fAborted in fFlags then
        break;
      events := fSock.WaitFor(TimeOut, [neRead, neError], RawError); // select/poll
      if neError in events then
      begin
        res := nrUnknownError;
        Close; // connection broken or socket closed gracefully
        break;
      end
      else if neRead in events then
        continue; // retry Recv()
      if Assigned(OnLog) then
        OnLog(sllTrace, 'TrySockRecv: timeout after %s', [TimeOut div 1000], self);
      res := nrTimeout;  // identify read timeout as error
      break;
    until fAborted in fFlags;
  end;
  if fAborted in fFlags then
    res := nrClosed;
  if NetResult <> nil then
    NetResult^ := res;
  result := (res = nrOK);
end;

procedure TCrtSocket.SockRecvLn(out Line: RawUtf8);
var
  tmp: TBuffer16K; // should be enough in our context (parsing HTTP headers)
  len: PtrInt;
begin
  len := SockInReadLn(@tmp, SizeOf(tmp)); // with or without SockIn^
  if len = 0 then
    exit;
  if len < 0 then
    DoRaise('SockRecvLn: line too long (>16KB)');
  FastSetString(Line, @tmp, len);
end;

procedure TCrtSocket.SockRecvLn;
begin
  SockInReadLn({Buffer=}nil, 0);
end;

procedure TCrtSocket.SndLow(P: pointer; Len: integer);
var
  res: TNetResult;
  rawError: integer;
begin
  if (Len <> 0) and
     not TrySndLow(P, Len, @res, @rawError) then
    DoRaise('SndLow(%s) len=%d', [fServer, Len], res, @rawError);
end;

procedure TCrtSocket.SndLow(const Data: RawByteString);
begin
  if self <> nil then
    SndLow(pointer(Data), Length(Data));
end;

function TCrtSocket.TrySndLow(P: pointer; Len: integer; NetResult: PNetResult;
  RawError: system.PInteger): boolean;
var
  sent: integer;
  events: TNetEvents;
  res: TNetResult;
begin
  if RawError <> nil then
    RawError^ := NO_ERROR;
  if fAborted in fFlags then
    res := nrClosed
  else if Len = 0 then
    res := nrOk
  else if not SockIsDefined then
    res := nrNoSocket
  else if (Len < 0) or
          (P = nil) then
   res := nrInvalidParameter
  else
  begin
    repeat
      sent := MinPtrInt(CrtSocketSendRecvMaxBytes, Len);
      if fSecure <> nil then
        res := fSecure.Send(P, sent)
      else
        res := fSock.Send(P, sent, RawError);
      if sent > 0 then
      begin
        inc(fBytesOut, sent);
        dec(Len, sent);
        if Len <= 0 then
          break; // all data successfully sent
        inc(PByte(P), sent);
        if res = nrOk then
          continue;
      end;
      if (fAborted in fFlags) or
         not (res in [nrOk, nrRetry]) then
        break;
      events := fSock.WaitFor(TimeOut, [neWrite, neError]); // select() or poll()
      res := nrUnknownError;
      if neError in events then
        break
      else if neWrite in events then
        continue; // retry Send()
      if Assigned(OnLog) then
        OnLog(sllTrace, 'TrySndLow: timeout after %ms)', [TimeOut], self);
      res := nrTimeout;  // identify write timeout as error
      break;
    until fAborted in fFlags;
    if fAborted in fFlags then
      res := nrClosed;
  end;
  if NetResult <> nil then
    NetResult^ := res; // always return a TNetResult
  result := (res = nrOK);
end;

function TCrtSocket.AcceptIncoming(
  ResultClass: TCrtSocketClass; Async: boolean): TCrtSocket;
var
  client: TNetSocket;
  addr: TNetAddr;
begin
  result := nil;
  if not SockIsDefined then
    exit;
  if fSock.Accept(client, addr, Async) <> nrOK then
    exit;
  if ResultClass = nil then
    ResultClass := TCrtSocket;
  result := ResultClass.Create(Timeout);
  result.AcceptRequest(client, @addr);
  result.CreateSockIn; // use SockIn with 1KB input buffer: 2x faster
end;

function TCrtSocket.AsSocketStream: TSocketStream;
begin
  if SockIsDefined then
    if Assigned(fSecure) then
      result := TSocketStream.Create(fSecure)
    else
      result := TSocketStream.Create(fSock)
  else
    result := nil;
end;


{ TCrtSocketStream }

constructor TCrtSocketStream.Create(aSocket: TCrtSocket);
begin
  fSocket := aSocket;
end;

destructor TCrtSocketStream.Destroy;
begin
  inherited Destroy;
  fSocket := nil; // before fOwned.Free e.g. if fOwned=fSocket
  fOwned.Free;
end;

function TCrtSocketStream.Read(var Buffer; Count: Longint): Longint;
begin
  if Count > 0 then
    if fSocket.TrySockRecv(@Buffer, Count, {stopbeforeCount=}true,
                 @fLastResult, @fLastRawError) then
    begin
      result := Count;
      inc(fSize, Count);
      fPosition := fSize;
    end
    else if fLastResult = nrRetry then
      result := 0
    else
      result := -1 // fatal error
  else
    result := 0; // nothing to receive
end;

function TCrtSocketStream.Write(const Buffer; Count: Longint): Longint;
begin
  if Count > 0 then
    if fSocket.TrySndLow(@Buffer, Count, @fLastResult, @fLastRawError) then
    begin
      result := Count;
      inc(fSize, Count);
      fPosition := fSize;
    end
    else if fLastResult = nrRetry then
      result := 0
    else
      result := -1 // fatal error, e.g. timeout
  else
    result := 0; // nothing to send
end;


function SocketOpen(const aServer, aPort: RawUtf8; aTLS: boolean;
  aTLSContext: PNetTlsContext; aTunnel: PUri;
  aTLSIgnoreCertError: boolean): TCrtSocket;
var
  tmp: TNetTlsContext;
begin
  try
    result := TCrtSocket.Open(aServer, aPort, nlTcp, 10000, aTLS,
      GetTlsContext(aTLS, aTLSIgnoreCertError, tmp, aTLSContext) , aTunnel);
  except
    result := nil;
  end;
end;



{ ********* NTP / SNTP Protocol Client }

type
  {$A-}
  /// a 64-bit SNTP timestamp, as described in RFC 2030
  {$ifdef USERECORDWITHMETHODS}
  TNtpTimestamp = record
  {$else}
  TNtpTimestamp = object
  {$endif USERECORDWITHMETHODS}
    Seconds: integer;
    Fraction: integer;
    procedure SwapEndian;
    function ToDateTime: TDateTime;
    procedure FromDateTime(Value: TDateTime);
  end;

  /// map a SNTP header, as described in RFC 2030
  {$ifdef USERECORDWITHMETHODS}
  TNtpPacket = record
  {$else}
  TNtpPacket = object
  {$endif USERECORDWITHMETHODS}
    LiVnMode, Stratum, Poll, Precision: byte;
    RootDelay, RootDispersion, ReferenceIdentifier: cardinal;
    Reference, Originate, Receive, Transmit: TNtpTimestamp;
    // optional: KeyID: cardinal; Digest: THash128;
    procedure Init;
    procedure SwapEndian;
  end;
  {$A+}

const
  maxFloat = 4294967295.0;
  maxInt32 = 2147483647;

procedure TNtpTimestamp.SwapEndian;
begin
  Seconds := bswap32(Seconds);
  Fraction := bswap32(Fraction);
end;

function TNtpTimestamp.ToDateTime: TDateTime;
var
  d, d1: Double;
begin
  d := Seconds;
  if d < 0 then
    d := maxFloat + d + 1;
  d1 := Fraction;
  if d1 < 0 then
    d1 := maxFloat + d1 + 1;
  d1 := d1 / maxFloat;
  d1 := Trunc(d1 * 10000) / 10000;
  result := (d + d1) / SecsPerDay;
  result := result + 2;
end;

procedure TNtpTimestamp.FromDateTime(Value: TDateTime);
var
  d, d1: Double;
begin
  d  := (Value - 2) * SecsPerDay;
  d1 := Frac(d);
  if d > maxInt32 then
     d := d - maxFloat - 1;
  d  := Trunc(d);
  d1 := Trunc(d1 * 10000) / 10000;
  d1 := d1 * maxFloat;
  if d1 > maxInt32 then
     d1 := d1 - maxFloat - 1;
  Seconds := Trunc(d);
  Fraction := Trunc(d1);
end;

procedure TNtpPacket.Init;
begin
  FillCharFast(self, SizeOf(self), 0);
  LiVnMode := $1b;
end;

procedure TNtpPacket.SwapEndian;
begin
  RootDelay := bswap32(RootDelay);
  RootDispersion := bswap32(RootDispersion);
  ReferenceIdentifier := bswap32(ReferenceIdentifier);
  Reference.SwapEndian;
  Originate.SwapEndian;
  Receive.SwapEndian;
  Transmit.SwapEndian;
end;

function NtpCall(const aServer, aPort: RawUtf8; TimeOutMS: integer;
  var pack: TNtpPacket): boolean;
var
  addr, resp: TNetAddr;
  sock: TNetSocket;
  len: PtrInt;
  res: TNetResult;
  tmp: TBuffer4K;
begin
  result := false;
  if addr.SetFrom(aServer, aPort, nlUdp) <> nrOK then
    exit;
  sock := addr.NewSocket(nlUdp);
  if sock <> nil then
    try
      sock.SetReceiveTimeout(TimeOutMS);
      res := sock.SendTo(@pack, SizeOf(pack), addr);
      if res <> nrOk then
        exit;
      len := sock.RecvFrom(@tmp, SizeOf(tmp), resp);
      if (len >= SizeOf(pack)) and
         addr.IPEqual(resp) then
      begin
        MoveFast(tmp, pack, SizeOf(pack));
        pack.SwapEndian;
        result := pack.Transmit.Seconds <> 0;
      end;
    finally
      sock.Close;
    end;
end;

function GetNtpTime(const aServer, aPort: RawUtf8; aTimeOutMS: integer;
  aInfo: PNtpInfo): TDateTime;
var
  pack: TNtpPacket;
  t1, t2, t3, t4 : TDateTime;
  nfo: TNtpInfo;
  addr: TNetAddr;
begin
  result := 0;
  addr.SetFrom(aServer, aPort, nlUdp); // DNS resolution and cache
  pack.Init;
  t1 := NowUtc;
  pack.Originate.FromDateTime(t1);
  pack.SwapEndian;
  if NtpCall(aServer, aPort, aTimeOutMS, pack) and
     (((pack.LiVnMode and $c0) shr 6) < 3) and  // LI
     ((pack.LiVnMode and $07) = 4) and          // Mode
     (pack.Stratum in [1..15]) and
     (pack.Receive.Seconds <> 0) then
  begin
    t2 := pack.Receive.ToDateTime;
    t3 := pack.Transmit.ToDateTime;
    t4 := NowUtc;
    nfo.Delay := (t4 - t1) - (t3 - t2);  // transmission delay
    nfo.Time := t3 + nfo.Delay / 2;      // halfway adjustment
    nfo.Delay := nfo.Delay * SecsPerDay; // as seconds
    nfo.Offset := (((t2 - t1) + (t3 - t4)) / 2) * SecsPerDay;
    if aInfo <> nil then
      aInfo^ := nfo;
    result := nfo.Time;
  end;
end;

function GetSntpTime(const aServer, aPort: RawUtf8; aTimeOutMS: integer): TDateTime;
var
  pack: TNtpPacket;
  start: TDateTime;
  addr: TNetAddr;
begin
  addr.SetFrom(aServer, aPort, nlUdp); // DNS resolution and cache
  pack.Init; // SNTP has no additional client information
  start := NowUtc;
  if NtpCall(aServer, aPort, aTimeOutMS, pack) then
    result := pack.Transmit.ToDateTime + (NowUtc - start) / 2 // simple adjust
  else
    result := 0;
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
  GetSystemMacAddress := @_GetSystemMacAddress;
  InitializeUnit; // in mormot.net.sock.windows/posix.inc

finalization
  FinalizeUnit;  // in mormot.net.sock.windows/posix.inc

end.

