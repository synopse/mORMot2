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
  mormot.core.base;


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
  /// exception class raise by this unit
  ENetSock = class(Exception);

  /// the error codes returned by TNetSocket wrapper
  TNetResult = (nrOK, nrRetry, nrNoSocket, nrNotFound, nrNotImplemented, nrFatalError);

  /// one data state on a given socket
  TNetEvent = (neRead, neWrite, neError, neClosed);
  /// the current whole read/write state on a given socket
  TNetEvents = set of TNetEvent;

  /// the available socket layers
  // - by definition, nlUNIX will return nrNotImplemented on Windows
  TNetLayer = (nlTCP, nlUDP, nlUNIX);

  /// internal mapping of an address, in any supported socket layer
  TNetAddr = object
  private
    // opaque TSockAddr wrapper with len: sockaddr_un=110 sockaddr_in6=28
    Addr: array[0..SOCKADDR_SIZE - 1] of byte;
  public
    function SetFrom(const address, addrport: RawUTF8; layer: TNetLayer): TNetResult;
    function Family: integer; {$ifdef FPC} inline; {$endif}
    function IP: RawUTF8;
    function IPShort(withport: boolean = false): shortstring; overload;
      {$ifdef HASINLINE} inline; {$endif}
    procedure IPShort(out result: shortstring; withport: boolean = false); overload;
    function Port: cardinal;
    function Size: integer;
  end;
  PNetAddr = TNetAddr;

type
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
    function Accept(out addr: TNetAddr): TNetResult;
    function GetPeer(out addr: TNetAddr): TNetResult;
    function MakeAsynch: TNetResult;
    function Send(Buf: pointer; len: integer): TNetResult;
    function Recv(Buf: pointer; len: integer): TNetResult;
    function SendTo(Buf: pointer; len: integer; out addr: TNetAddr): TNetResult;
    function RecvFrom(Buf: pointer; len: integer; out addr: TNetAddr): TNetResult;
    function RecvPending(ms: integer; out pending: integer): TNetResult;
    function WaitFor(ms: integer; scope: TNetEvents): TNetEvents;
    function ShutdownAndClose(rdwr: boolean): TNetResult;
    function Close: TNetResult;
    function Socket: PtrInt;
  end;

  /// end-user code should use this TNetSocket type to hold a socket reference
  TNetSocket = ^TNetSocketWrap;


/// create a new Socket connected or bound to a given ip:port
function NewSocket(const address, port: RawUTF8; layer: TNetLayer;
  dobind: boolean; connecttimeout, sendtimeout, recvtimeout, retry: integer;
  out netsocket: TNetSocket): TNetResult;


var
  /// Queue length for completely established sockets waiting to be accepted,
  // a backlog parameter for listen() function. If queue overflows client count,
  // ECONNREFUSED error is returned from connect() call
  // - for Windows default $7fffffff should not be modified. Actual limit is 200
  // - for Unix default is taken from constant (128 as in linux kernel >2.2),
  // but actual value is min(DefaultListenBacklog, /proc/sys/net/core/somaxconn)
  DefaultListenBacklog: integer;



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
    /// the events which are notified
    events: TPollSocketEvents;
    /// opaque value as defined by TPollSocketAbstract.Subscribe
    tag: TPollSocketTag;
  end;

  /// all modifications returned by IPollSocket.WaitForModified
  TPollSocketResults = array of TPollSocketResult;

  {$M+}
  /// abstract parent class for efficient socket polling
  // - works like Linux epoll API in level-triggered (LT) mode
  // - implements libevent-like cross-platform features
  // - use PollSocketClass global function to retrieve the best class depending
  // on the running Operating System
  // - actual classes are hidden in the implementation section of this unit,
  // and will use the fastest available API on each Operating System
  TPollSocketAbstract = class
  protected
    fCount: integer;
    fMaxSockets: integer;
  public
    /// class function factory, returning a socket polling instance matching
    // at best the current operating system
    // - return a hidden TPollSocketSelect instance under Windows,
    // TPollSocketEpoll instance under Linux, or TPollSocketPoll on BSD
    // - just a wrapper around PollSocketClass.Create
    class function New: TPollSocketAbstract;
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
    /// how many TSocket instances are currently tracked
    property Count: integer read fCount;
  end;
  {$M-}

  /// meta-class of TPollSocketAbstract socket polling classes
  // - since TPollSocketAbstract.Create is declared as virtual, could be used
  // to specify the proper polling class to add
  // - see PollSocketClass function and TPollSocketAbstract.New method
  TPollSocketClass = class of TPollSocketAbstract;

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


{ ******** TNetAddr Cross-Platform Wrapper }

{ TNetAddr }

function TNetAddr.Family: integer;
begin
  result := PSockAddr(@Addr)^.sa_family;
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
             serv{%H-}, servlen, NI_NUMERICHOST + NI_NUMERICSERV) = 0 then
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
  out netsocket: TNetSocket): TNetResult;
var
  addr: TNetAddr;
  sock: TSocket;
begin
  netsocket := nil;
  result := addr.SetFrom(address, port, layer);
  if result <> nrOK then
    exit;
  sock := socket(addr.Family, _ST[layer], _IP[layer]);
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
      if (bind(sock, @addr, addr.Size) <> 0) or
         ((layer <> nlUDP) and (listen(sock, DefaultListenBacklog) <> 0)) then
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
      if connect(sock, @addr, addr.Size) <> 0 then
        result := NetLastError(WSAEADDRNOTAVAIL);
    end;
    if (result = nrOK) or (retry <= 0) then
      break;
    dec(retry);
    Sleep(10);
  until false;
  if result <> nrOK then
    closesocket(sock)
  else
  begin
    netsocket := TNetSocket(sock);
    netsocket.SetupConnection(layer, sendtimeout, recvtimeout);
  end;
end;


{ TNetSocketWrap }

procedure TNetSocketWrap.SetOpt(prot, name: integer; value: pointer; valuelen: integer);
begin
  if @self = nil then
    raise ENetSock.CreateFmt('SetOptions(%d,%d) with no socket', [prot, name]);
  if setsockopt(TSocket(@self), prot, name, value, valuelen) <> 0 then
    raise ENetSock.CreateFmt('SetOptions(%d,%d) failed', [prot, name]);
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

function TNetSocketWrap.Accept(out addr: TNetAddr): TNetResult;
var
  len: integer;
begin
  len := SizeOf(addr);
  if @self = nil then
    result := nrNoSocket
  else
    result := NetCheck(mormot.lib.sock.accept(TSocket(@self), @addr, len));
end;

function TNetSocketWrap.GetPeer(out addr: TNetAddr): TNetResult;
var
  len: integer;
begin
  len := SizeOf(addr);
  FillCharFast(addr, SizeOf(addr), 0);
  if @self = nil then
    result := nrNoSocket
  else
    result := NetCheck(getpeername(TSocket(@self), @addr, len));
end;

function TNetSocketWrap.MakeAsynch: TNetResult;
var
  nonblock: cardinal;
begin
  nonblock := 1;
  if @self = nil then
    result := nrNoSocket
  else
    result := NetCheck(ioctlsocket(TSocket(@self), FIONBIO, @nonblock));
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
  addrlen := SizeOf(addr);
  if @self = nil then
    result := nrNoSocket
  else
    result := NetCheck(mormot.lib.sock.recvfrom(TSocket(@self),
      Buf, len, 0, @addr, @addrlen));
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
    // at last under Linux close() is enough (e.g. nginx doesn't call shutdown)
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

{ TPollSocketAbstract }

class function TPollSocketAbstract.New: TPollSocketAbstract;
begin
  result := PollSocketClass.Create;
end;

constructor TPollSocketAbstract.Create;
begin
  // nothing to do
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

