/// low-level access to the OperatingSystem Sockets API (e.g. WinSock2)
// - this unit is a part of the freeware Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.lib.sock;

{
  *****************************************************************************

   Cross-Platform Raw Sockets API Definition
   - Socket Process High-Level Encapsulation
   - Efficient Multiple Sockets Polling

   The Low-Level Sockets API is encapsultated into a single set of functions,
   and wrapped around a TNetSocket abstract helper, and never made public.

  *****************************************************************************

  Notes:
    Oldest Delphis didn't include WinSock2.pas.
    Under POSIX, will redirect to regular FPC units.

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

  {$ifdef MSWINDOWS}
  SOCKADDR_SIZE = 28;
  {$else}
  SOCKADDR_SIZE = 110;
  {$endif MSWINDOWS}

var
  /// global variable containing '127.0.0.1'
  // - defined as var not as const to use reference counting from TNetAddr.IP
  IP4local: RawUTF8;

type
  /// the error codes returned by TNetSocket wrapper
  TNetResult = (
    nrOK, nrRetry, nrNoSocket, nrNotFound, nrNotImplemented, nrFatalError);

  /// exception class raise by this unit
  ENetSock = class(Exception)
  public
    /// raise ENetSock if res is not nrOK or nrRetry
    class procedure Check(res: TNetResult; const Context: shortstring);
  end;

  /// one data state on a given socket
  TNetEvent = (
    neRead, neWrite, neError, neClosed);

  /// the current whole read/write state on a given socket
  TNetEvents = set of TNetEvent;

  /// the available socket protocol layers
  // - by definition, nlUNIX will return nrNotImplemented on Windows
  TNetLayer = (
    nlTCP, nlUDP, nlUNIX);

  /// the available socket families - mapping AF_INET/AF_INET6/AF_UNIX
  TNetFamily = (
    nfUnknown, nfIP4, nfIP6, nfUNIX);

  /// internal mapping of an address, in any supported socket layer
  TNetAddr = object
  private
    // opaque wrapper with len: sockaddr_un=110 (POSIX) or sockaddr_in6=28 (Win)
    Addr: array[0..SOCKADDR_SIZE - 1] of byte;
  public
    function SetFrom(const address, addrport: RawUTF8; layer: TNetLayer): TNetResult;
    function Family: TNetFamily;
    function IP: RawUTF8;
    function IPShort(withport: boolean = false): shortstring; overload;
      {$ifdef HASINLINE} inline; {$endif}
    procedure IPShort(out result: shortstring; withport: boolean = false); overload;
    function Port: cardinal;
    function Size: integer;
  end;
  PNetAddr = ^TNetAddr;

type
  /// end-user code should use this TNetSocket type to hold a socket reference
  // - then methods allow cross-platform access to the connection
  TNetSocket = ^TNetSocketWrap;

  /// convenient object-oriented wrapper around a socket connection
  // - TNetSocket is a pointer to this, so TSocket(@self) is used for the API
  TNetSocketWrap = object
  private
    procedure SetOpt(prot, name: integer; value: pointer; valuelen: integer);
  public
    procedure SetupConnection(layer: TNetLayer; sendtimeout, recvtimeout: integer);
    procedure SetSendTimeout(ms: integer);
    procedure SetReceiveTimeout(ms: integer);
    procedure SetKeepAlive(keepalive: boolean);
    procedure SetLinger(linger: integer);
    procedure SetNoDelay(nodelay: boolean);
    function Accept(out clientsocket: TNetSocket; out addr: TNetAddr): TNetResult;
    function GetPeer(out addr: TNetAddr): TNetResult;
    function MakeAsynch: TNetResult;
    function Send(Buf: pointer; len: integer): TNetResult;
    function Recv(Buf: pointer; len: integer): TNetResult;
    function SendTo(Buf: pointer; len: integer; out addr: TNetAddr): TNetResult;
    function RecvFrom(Buf: pointer; len: integer; out addr: TNetAddr): TNetResult;
    function WaitFor(ms: integer; scope: TNetEvents): TNetEvents;
    function RecvPending(ms: integer; out pending: integer): TNetResult;
    function ShutdownAndClose(rdwr: boolean): TNetResult;
    function Close: TNetResult;
    function Socket: PtrInt;
  end;


/// create a new Socket connected or bound to a given ip:port
function NewSocket(const address, port: RawUTF8; layer: TNetLayer;
  dobind: boolean; connecttimeout, sendtimeout, recvtimeout, retry: integer;
  out netsocket: TNetSocket; netaddr: PNetAddr = nil): TNetResult;


var
  /// Queue length for completely established sockets waiting to be accepted,
  // a backlog parameter for listen() function. If queue overflows client count,
  // ECONNREFUSED error is returned from connect() call
  // - for Windows default $7fffffff should not be modified. Actual limit is 200
  // - for Unix default is taken from constant (128 as in linux kernel >2.2),
  // but actual value is min(DefaultListenBacklog, /proc/sys/net/core/somaxconn)
  DefaultListenBacklog: integer;

/// returns the trimmed text of a network result
// - e.g. ToText(nrNotFound)='NotFound'
function ToText(res: TNetResult): PShortString;

/// retrieve the HTTP reason text from a code
// - e.g. StatusCodeToReason(200)='OK'
// - see http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html
// - mORMot.StatusCodeToErrorMsg() will call this function
function StatusCodeToReason(Code: cardinal): RawUTF8;


{ ******************** Efficient Multiple Sockets Polling }

type
  /// the events monitored by TPollSocketAbstract
  // - we don't make any difference between urgent or normal read/write events
  TPollSocketEvent = (pseRead, pseWrite, pseError, pseClosed);

  /// set of events monitored by TPollSocketAbstract
  TPollSocketEvents = set of TPollSocketEvent;

  /// some opaque value (which may be a pointer) associated with a polling event
  TPollSocketTag = type PtrInt;

  /// modifications notified by TPollSocketAbstract.WaitForModified
  TPollSocketResult = record
    /// opaque value as defined by TPollSocketAbstract.Subscribe
    tag: TPollSocketTag;
    /// the events which are notified
    events: TPollSocketEvents;
  end;

  /// all modifications returned by IPollSocket.WaitForModified
  TPollSocketResults = array of TPollSocketResult;

  {$M+}
  /// abstract parent for TPollSocket* and TPollSockets polling
  TPollAbstract = class
  protected
    fCount: integer;
  public
    /// initialize the instance
    constructor Create; virtual;
    /// track status modifications on one specified TSocket
    // - you can specify which events are monitored - pseError and pseClosed
    // will always be notified
    // - tag parameter will be returned as TPollSocketResult - you may set
    // here the socket file descriptor value, or a transtyped class instance
    // - similar to epoll's EPOLL_CTL_ADD control interface
    function Subscribe(socket: TNetSocket; events: TPollSocketEvents;
      tag: TPollSocketTag): boolean; virtual; abstract;
    /// how many TSocket instances are currently tracked
    property Count: integer read fCount;
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
    /// stop status modifications tracking on one specified TSocket
    // - the socket should have been monitored by a previous call to Subscribe()
    // - on success, returns true and fill tag with the associated opaque value
    // - similar to epoll's EPOLL_CTL_DEL control interface
    function Unsubscribe(socket: TNetSocket): boolean; virtual; abstract;
    /// waits for status modifications of all tracked TSocket
    // - will wait up to timeoutMS milliseconds, 0 meaning immediate return
    // and -1 for infinite blocking
    // - returns -1 on error (e.g. no TSocket currently registered), or
    // the number of modifications stored in results[] (may be 0 if none)
    function WaitForModified(out results: TPollSocketResults;
      timeoutMS: integer): integer; virtual; abstract;
  published
    /// how many TSocket instances could be tracked, at most
    // - depends on the API used
    property MaxSockets: integer read fMaxSockets;
  end;

  /// meta-class of TPollSocketAbstract socket polling classes
  // - since TPollSocketAbstract.Create is declared as virtual, could be used
  // to specify the proper polling class to add
  // - see PollSocketClass function and TPollSocketAbstract.New method
  TPollSocketClass = class of TPollSocketAbstract;

  /// implements efficient polling of multiple sockets
  // - will maintain a pool of TPollSocketAbstract instances, to monitor
  // incoming data or outgoing availability for a set of active connections
  // - call Subscribe/Unsubscribe to setup the monitored sockets
  // - call GetOne from any consumming threads to process new events
  TPollSockets = class(TPollAbstract)
  protected
    fPoll: array of TPollSocketAbstract;
    fPollIndex: integer;
    fPending: TPollSocketResults;
    fPendingIndex: PtrInt;
    fGettingOne: integer;
    fTerminated: boolean;
    fPollClass: TPollSocketClass;
    fPollLock: TRTLCriticalSection;
    fPendingLock: TRTLCriticalSection;
  public
    /// initialize the sockets polling
    // - under Linux/POSIX, will set the open files maximum number for the
    // current process to match the system hard limit: if your system has a
    // low "ulimit -H -n" value, you may add the following line in your
    // /etc/limits.conf or /etc/security/limits.conf file:
    // $ * hard nofile 65535
    constructor Create; override;
    /// finalize the sockets polling, and release all used memory
    destructor Destroy; override;
    /// track modifications on one specified TSocket and tag
    // - the supplied tag value - maybe a PtrInt(aObject) - will be part of
    // GetOne method results
    // - will create as many TPollSocketAbstract instances as needed, depending
    // on the MaxSockets capability of the actual implementation class
    // - this method is thread-safe
    function Subscribe(socket: TNetSocket; events: TPollSocketEvents;
      tag: TPollSocketTag): boolean; override;
    /// stop status modifications tracking on one specified TSocket and tag
    // - the socket should have been monitored by a previous call to Subscribe()
    // - this method is thread-safe
    function Unsubscribe(socket: TNetSocket; tag: TPollSocketTag): boolean; virtual;
    /// retrieve the next pending notification, or let the poll wait for new
    // - if there is no pending notification, will poll and wait up to
    // timeoutMS milliseconds for pending data
    // - returns true and set notif.events/tag with the corresponding notification
    // - returns false if no pending event was handled within the timeoutMS period
    // - this method is thread-safe, and could be called from several threads
    function GetOne(timeoutMS: integer; out notif: TPollSocketResult): boolean; virtual;
    /// retrieve the next pending notification
    // - returns true and set notif.events/tag with the corresponding notification
    // - returns false if no pending event is available
    // - this method is thread-safe, and could be called from several threads
    function GetOneWithinPending(out notif: TPollSocketResult): boolean;
    /// notify any GetOne waiting method to stop its polling loop
    procedure Terminate; virtual;
    /// the actual polling class used to track socket state changes
    property PollClass: TPollSocketClass read fPollClass write fPollClass;
    /// set to true by the Terminate method
    property Terminated: boolean read fTerminated;
  end;


/// the TPollSocketAbstract class best fitting with the current Operating System
// - as used by TPollSocketAbstract.New method
function PollSocketClass: TPollSocketClass;


implementation


{ ******** System-Specific Raw Sockets API Layer }

{ includes are below inserted just after 'implementation' keyword to allow
  their own private 'uses' clause }

{$ifdef MSWINDOWS}
  {$I mormot.lib.sock.windows.inc}
{$endif MSWINDOWS}

{$ifdef LINUX}
  {$I mormot.lib.sock.posix.inc}
{$endif LINUX}


function NetLastError(anothernonfatal: integer = NO_ERROR): TNetResult;
var
  err: integer;
begin
  err := sockerrno;
  if err = NO_ERROR then
    result := nrOK
  else if {$ifdef MSWINDOWS} (err <> WSAETIMEDOUT) and (err <> WSAEWOULDBLOCK) and {$endif}
          (err <> WSATRY_AGAIN) and (err <> WSAEINTR) and (err <> anothernonfatal) then
    result := nrFatalError
  else
    result := nrRetry;
end;

function NetCheck(res: integer): TNetResult;
  {$ifdef HASINLINE} inline; {$endif}
begin
  if res = NO_ERROR then
    result := nrOK
  else
    result := NetLastError;
end;

procedure IP4Short(ip4addr: PByteArray; var s: shortstring);
var
  i: PtrInt;
begin
  s := '';
  i := 0;
  repeat
    AppendShortInteger(ip4addr[i], s);
    if i = 3 then
      break;
    AppendShortChar('.', s);
    inc(i);
  until false;
end;

procedure IP4Text(ip4addr: PByteArray; var result: RawUTF8);
var
  s: shortstring;
begin
  if PCardinal(ip4addr)^ = 0 then
    result := ''
  else if PCardinal(ip4addr)^ = cLocalhost32 then
    result := IP4local
  else
  begin
    IP4Short(ip4addr, s);
    SetString(result, PAnsiChar(@s[1]), ord(s[0]));
  end;
end;

const
  _NR: array[TNetResult] of string[15] = (
    'OK', 'Retry', 'NoSocket', 'NotFound', 'NotImplemented', 'FatalError');

function ToText(res: TNetResult): PShortString;
begin
  result := @_NR[res];
end;

var
  ReasonCache: array[1..5, 0..13] of RawUTF8; // avoid memory allocation

function StatusCodeToReasonInternal(Code: cardinal): RawUTF8;
begin
  case Code of
    100:
      result := 'Continue';
    101:
      result := 'Switching Protocols';
    200:
      result := 'OK';
    201:
      result := 'Created';
    202:
      result := 'Accepted';
    203:
      result := 'Non-Authoritative Information';
    204:
      result := 'No Content';
    205:
      result := 'Reset Content';
    206:
      result := 'Partial Content';
    207:
      result := 'Multi-Status';
    300:
      result := 'Multiple Choices';
    301:
      result := 'Moved Permanently';
    302:
      result := 'Found';
    303:
      result := 'See Other';
    304:
      result := 'Not Modified';
    305:
      result := 'Use Proxy';
    307:
      result := 'Temporary Redirect';
    308:
      result := 'Permanent Redirect';
    400:
      result := 'Bad Request';
    401:
      result := 'Unauthorized';
    403:
      result := 'Forbidden';
    404:
      result := 'Not Found';
    405:
      result := 'Method Not Allowed';
    406:
      result := 'Not Acceptable';
    407:
      result := 'Proxy Authentication Required';
    408:
      result := 'Request Timeout';
    409:
      result := 'Conflict';
    410:
      result := 'Gone';
    411:
      result := 'Length Required';
    412:
      result := 'Precondition Failed';
    413:
      result := 'Payload Too Large';
    414:
      result := 'URI Too Long';
    415:
      result := 'Unsupported Media Type';
    416:
      result := 'Requested Range Not Satisfiable';
    426:
      result := 'Upgrade Required';
    500:
      result := 'Internal Server Error';
    501:
      result := 'Not Implemented';
    502:
      result := 'Bad Gateway';
    503:
      result := 'Service Unavailable';
    504:
      result := 'Gateway Timeout';
    505:
      result := 'HTTP Version Not Supported';
    511:
      result := 'Network Authentication Required';
  else
    result := 'Invalid Request';
  end;
end;

function StatusCodeToReason(Code: cardinal): RawUTF8;
var
  Hi, Lo: cardinal;
begin
  if Code = 200 then
  begin
    // optimistic approach :)
    Hi := 2;
    Lo := 0;
  end
  else
  begin
    Hi := Code div 100;
    Lo := Code - Hi * 100;
    if not ((Hi in [1..5]) and (Lo in [0..13])) then
    begin
      result := StatusCodeToReasonInternal(Code);
      exit;
    end;
  end;
  result := ReasonCache[Hi, Lo];
  if result <> '' then
    exit;
  result := StatusCodeToReasonInternal(Code);
  ReasonCache[Hi, Lo] := result;
end;



{ ENetSock }

class procedure ENetSock.Check(res: TNetResult; const Context: shortstring);
begin
  if (res <> nrOK) and (res <> nrRetry) then
    raise CreateFmt('%s: ''%s'' error', [Context, _NR[res]]);
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
    {$ifndef MSWINDOWS}
    AF_UNIX:
      result := nfUNIX;
    {$endif}
    else
      result := nfUnknown;
  end;
end;

function TNetAddr.ip: RawUTF8;
var
  tmp: ShortString;
begin
  with PSockAddr(@Addr)^ do
    if sa_family = AF_INET then
      // check most common used values
      if cardinal(sin_addr) = 0 then
      begin
        result := '';
        exit;
      end
      else if cardinal(sin_addr) = cLocalhost32 then
      begin
        result := IP4local;
        exit;
      end;
  IPShort(tmp);
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
    {$ifndef MSWINDOWS}
    AF_UNIX:
      SetString(result, PAnsiChar(@psockaddr_un(@Addr)^.sun_path),
        mormot.core.base.StrLen(@psockaddr_un(@Addr)^.sun_path));
    {$endif MSWINDOWS}
  end;
end;

function TNetAddr.port: cardinal;
begin
  with PSockAddr(@Addr)^ do
    if sa_family in [AF_INET, AF_INET6] then
      result := swap(sin_port)
    else
      result := 0;
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

function NewSocket(const address, port: RawUTF8; layer: TNetLayer; dobind: boolean;
  connecttimeout, sendtimeout, recvtimeout, retry: integer;
  out netsocket: TNetSocket; netaddr: PNetAddr): TNetResult;
var
  addr: TNetAddr;
  sock: TSocket;
begin
  netsocket := nil;
  result := addr.SetFrom(address, port, layer);
  if result <> nrOK then
    exit;
  sock := socket(PSockAddr(@addr)^.sa_family, _ST[layer], _IP[layer]);
  if sock = -1 then
  begin
    result := NetLastError(WSAEADDRNOTAVAIL);
    exit;
  end;
  repeat
    if dobind then
    begin
      // Socket should remain open for 5 seconds after a closesocket() call
      TNetSocket(sock).SetLinger(5);
      if (bind(sock, @addr, addr.Size)  <> NO_ERROR) or
         ((layer <> nlUDP) and (listen(sock, DefaultListenBacklog)  <> NO_ERROR)) then
        result := NetLastError(WSAEADDRNOTAVAIL);
    end
    else
    begin
      // open Client connection
      if connecttimeout > 0 then
      begin
        TNetSocket(sock).SetReceiveTimeout(connecttimeout);
        TNetSocket(sock).SetSendTimeout(connecttimeout);
      end;
      if connect(sock, @addr, addr.Size)  <> NO_ERROR then
        result := NetLastError(WSAEADDRNOTAVAIL);
    end;
    if (result = nrOK) or (retry <= 0) then
      break;
    dec(retry);
    SleepHiRes(10);
  until false;
  if result <> nrOK then
    closesocket(sock)
  else
  begin
    netsocket := TNetSocket(sock);
    netsocket.SetupConnection(layer, sendtimeout, recvtimeout);
    if netaddr <> nil then
      MoveFast(addr, netaddr^, addr.Size);
  end;
end;


{ TNetSocketWrap }

procedure TNetSocketWrap.SetOpt(prot, name: integer; value: pointer; valuelen: integer);
begin
  if @self = nil then
    raise ENetSock.CreateFmt('SetOptions(%d,%d) with no socket', [prot, name]);
  if setsockopt(TSocket(@self), prot, name, value, valuelen)  <> NO_ERROR then
    raise ENetSock.CreateFmt('SetOptions(%d,%d) failed as %',
      [prot, name, _NR[NetLastError]]);
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

procedure TNetSocketWrap.SetupConnection(layer: TNetLayer;
  sendtimeout, recvtimeout: integer);
begin
  if @self = nil then
    exit;
  if sendtimeout > 0 then
    SetSendTimeout(sendtimeout);
  if recvtimeout > 0 then
    SetReceiveTimeout(recvtimeout);
  if layer = nlTCP then
  begin
    SetNoDelay(true);   // disable Nagle algorithm (we use our own buffers)
    SetKeepAlive(true); // enabled TCP keepalive
  end;
end;

function TNetSocketWrap.Accept(out clientsocket: TNetSocket;
  out addr: TNetAddr): TNetResult;
var
  len: integer;
  sock: TSocket;
begin
  if @self = nil then
    result := nrNoSocket
  else
  begin
    len := SizeOf(addr);
    sock := mormot.lib.sock.accept(TSocket(@self), @addr, len);
    if sock = -1 then
      result := NetLastError
    else
    begin
      clientsocket := TNetSocket(sock);
      result := nrOK;
    end;
  end;
end;

function TNetSocketWrap.GetPeer(out addr: TNetAddr): TNetResult;
var
  len: integer;
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

function TNetSocketWrap.MakeAsynch: TNetResult;
var
  nonblock: cardinal;
begin
  if @self = nil then
    result := nrNoSocket
  else
  begin
    nonblock := 1;
    result := NetCheck(ioctlsocket(TSocket(@self), FIONBIO, @nonblock));
  end;
end;

function TNetSocketWrap.Send(Buf: pointer; len: integer): TNetResult;
begin
  if @self = nil then
    result := nrNoSocket
  else
    result := NetCheck(mormot.lib.sock.send(TSocket(@self), Buf, len, 0));
end;

function TNetSocketWrap.Recv(Buf: pointer; len: integer): TNetResult;
begin
  if @self = nil then
    result := nrNoSocket
  else
    result := NetCheck(mormot.lib.sock.recv(TSocket(@self), Buf, len, 0));
end;

function TNetSocketWrap.SendTo(Buf: pointer; len: integer; out addr: TNetAddr): TNetResult;
begin
  if @self = nil then
    result := nrNoSocket
  else
    result := NetCheck(mormot.lib.sock.sendto(TSocket(@self),
      Buf, len, 0, @addr, SizeOf(addr)));
end;

function TNetSocketWrap.RecvFrom(Buf: pointer; len: integer; out addr: TNetAddr): TNetResult;
var
  addrlen: integer;
begin
  if @self = nil then
    result := nrNoSocket
  else
  begin
    addrlen := SizeOf(addr);
    result := NetCheck(mormot.lib.sock.recvfrom(TSocket(@self),
      Buf, len, 0, @addr, @addrlen));
  end;
end;

function TNetSocketWrap.RecvPending(ms: integer; out pending: integer): TNetResult;
begin
  if @self = nil then
    result := nrNoSocket
  else
    result := NetCheck(ioctlsocket(TSocket(@self), FIONREAD, @pending));
end;

function TNetSocketWrap.ShutdownAndClose(rdwr: boolean): TNetResult;
const
  SHUT_: array[boolean] of integer = (SHUT_RD, SHUT_RDWR);
begin
  if @self = nil then
    result := nrNoSocket
  else
  begin
    {$ifdef LINUXNOTBSD}
    // on Linux close() is enough (e.g. nginx doesn't call shutdown)
    if rdwr then
    {$endif LINUXNOTBSD}
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

{ TPollAbstract }

constructor TPollAbstract.Create;
begin
  // nothing to do
end;


{ TPollSocketAbstract }

class function TPollSocketAbstract.New: TPollSocketAbstract;
begin
  result := PollSocketClass.Create;
end;


{ TPollSockets }

constructor TPollSockets.Create;
begin
  inherited Create;
  InitializeCriticalSection(fPendingLock);
  InitializeCriticalSection(fPollLock);
  {$ifndef MSWINDOWS}
  SetFileOpenLimit(GetFileOpenLimit(true)); // set soft limit to hard value
  {$endif MSWINDOWS}
end;

destructor TPollSockets.Destroy;
var
  p: PtrInt;
  endtix: Int64; // never wait forever
begin
  Terminate;
  endtix := GetTickCount64 + 1000;
  while (fGettingOne > 0) and (GetTickCount64 < endtix) do
    SleepHiRes(1);
  for p := 0 to high(fPoll) do
    fPoll[p].Free;
  DeleteCriticalSection(fPendingLock);
  DeleteCriticalSection(fPollLock);
  inherited Destroy;
end;

function TPollSockets.Subscribe(socket: TNetSocket; events: TPollSocketEvents;
  tag: TPollSocketTag): boolean;
var
  p, n: PtrInt;
  poll: TPollSocketAbstract;
begin
  result := false;
  if (self = nil) or (socket = nil) or (events = []) then
    exit;
  EnterCriticalSection(fPollLock);
  try
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
      poll := fPollClass.Create;
      SetLength(fPoll, n + 1);
      fPoll[n] := poll;
    end;
    result := poll.Subscribe(socket, events, tag);
    if result then
      inc(fCount);
  finally
    LeaveCriticalSection(fPollLock);
  end;
end;

function TPollSockets.Unsubscribe(socket: TNetSocket; tag: TPollSocketTag): boolean;
var
  p: PtrInt;
begin
  result := false;
  EnterCriticalSection(fPendingLock);
  try
    for p := fPendingIndex to high(fPending) do
      if fPending[p].tag = tag then
        // event to be ignored in future GetOneWithinPending
        byte(fPending[p].events) := 0;
  finally
    LeaveCriticalSection(fPendingLock);
  end;
  EnterCriticalSection(fPollLock);
  try
    for p := 0 to high(fPoll) do
      if fPoll[p].Unsubscribe(socket) then
      begin
        dec(fCount);
        result := true;
        exit;
      end;
  finally
    LeaveCriticalSection(fPollLock);
  end;
end;

function TPollSockets.GetOneWithinPending(out notif: TPollSocketResult): boolean;
var
  last: PtrInt;
begin
  result := false;
  if fTerminated or (fPending = nil) then
    exit;
  EnterCriticalSection(fPendingLock);
  try
    last := high(fPending);
    while (fPendingIndex <= last) and (fPending <> nil) do
    begin
      // retrieve next notified event
      notif := fPending[fPendingIndex];
      // move forward
      if fPendingIndex < last then
        inc(fPendingIndex)
      else
      begin
        fPending := nil;
        fPendingIndex := 0;
      end;
      // return event (if not set to 0 by Unsubscribe)
      if byte(notif.events) <> 0 then
      begin
        result := true;
        exit;
      end;
    end;
  finally
    LeaveCriticalSection(fPendingLock);
  end;
end;

function TPollSockets.GetOne(timeoutMS: integer; out notif: TPollSocketResult): boolean;

  function PollAndSearchWithinPending(p: PtrInt): boolean;
  begin
    if not fTerminated and
       (fPoll[p].WaitForModified(fPending, {waitms=}0) > 0) then
    begin
      result := GetOneWithinPending(notif);
      if result then
        fPollIndex := p; // next call to continue from fPoll[fPollIndex+1]
    end
    else
      result := false;
  end;

var
  p, n: PtrInt;
  elapsed, start: Int64;
begin
  result := GetOneWithinPending(notif); // some events may be available
  if result or (timeoutMS < 0) then
    exit;
  InterlockedIncrement(fGettingOne);
  try
    byte(notif.events) := 0;
    if fTerminated then
      exit;
    if timeoutMS = 0 then
      start := 0
    else
      start := GetTickCount64;
    repeat
      // non-blocking search within all fPoll[] items
      if fCount > 0 then
      begin
        EnterCriticalSection(fPollLock);
        try
          // calls fPoll[].WaitForModified({waitms=}0) to refresh pending state
          n := length(fPoll);
          if n > 0 then
          begin
            for p := fPollIndex + 1 to n - 1 do
              // search from fPollIndex = last found
              if PollAndSearchWithinPending(p) then
                exit;
            for p := 0 to fPollIndex do
              // search from beginning up to fPollIndex
              if PollAndSearchWithinPending(p) then
                exit;
          end;
        finally
          LeaveCriticalSection(fPollLock);
          result := byte(notif.events) <> 0; // exit comes here -> set result
        end;
      end;
      // wait a little for something to happen
      if fTerminated or (timeoutMS = 0) then
        exit;
      elapsed := GetTickCount64 - start;
      if elapsed > timeoutMS then
        break;
      if elapsed > 300 then
        SleepHiRes(50)
      else if elapsed > 50 then
        SleepHiRes(10)
      else
        SleepHiRes(1);
      result := GetOneWithinPending(notif); // retrieved from another thread?
    until result or fTerminated;
  finally
    InterlockedDecrement(fGettingOne);
  end;
end;

procedure TPollSockets.Terminate;
begin
  if self <> nil then
    fTerminated := true;
end;


initialization
  IP4local := cLocalhost; // use var string with refcount=1 to avoid allocation
  assert(SizeOf(in_addr) = 4);
  assert(SizeOf(in6_addr) = 16);
  assert(SizeOf(sockaddr_in) = 16);
  assert(SizeOf(TNetAddr) = SOCKADDR_SIZE);
  assert(SizeOf(TNetAddr) >=
    {$ifdef MSWINDOWS} SizeOf(sockaddr_in6) {$else} SizeOf(sockaddr_un) {$endif});
  DefaultListenBacklog := SOMAXCONN;
  InitializeUnit; // in mormot.lib.sock.windows.inc

finalization
  FinalizeUnit;
end.

