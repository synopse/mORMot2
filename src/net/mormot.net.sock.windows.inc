{
  This file is a part of the Open Source Synopse mORMot framework 2,
  licensed under a MPL/GPL/LGPL three license - see LICENSE.md

  Implements WinSock2 API calls, as used by mormot.net.sock.pas
}

uses
  Windows;
  
function NetLastError(AnotherNonFatal: integer = 0{NO_ERROR}): TNetResult; forward;

{ ******** WinSock2 Type Definitions }

const
  ws2 = 'ws2_32.dll';

const
  // default on Windows is 64, but we grow it to 512 for TPollSocketSelect
  // as stated by Raymond Chen: "you can make FD_SET­SIZE bigger if you need to"
  // https://devblogs.microsoft.com/oldnewthing/20161221-00/?p=94985
  // -> Xitami uses 1024 since 1996
  // -> safe since our TFDSet is INTERNAl to this unit, and never published :)
  FD_SETSIZE = 512;

type
  TSocket = PtrInt;

  PFDSet = ^TFDSet;
  TFDSet = record
    fd_count: integer;
    fd_array: array[0..FD_SETSIZE-1] of TSocket;
  end;

const
  FIONREAD     = $4004667f;
  FIONBIO      = $8004667e;
  FIOASYNC     = $8004667d;

type
  PTimeVal = ^TTimeVal;
  TTimeVal = record
    tv_sec: Longint;
    tv_usec: Longint;
  end;

const
  IPPROTO_IP     =   0;	   // Abstract
  IPPROTO_ICMP   =   1;	   // Internet Control Message Protocol
  IPPROTO_IGMP   =   2;	   // Internet Group Management Protocol
  IPPROTO_TCP    =   6;	   // TCP
  IPPROTO_UDP    =   17;   // User Datagram Protocol
  IPPROTO_IPV6   =   41;
  IPPROTO_ICMPV6 =   58;

type
  PInAddr = ^in_addr;
  in_addr = packed record
    case integer of
      0: (S_bytes: packed array [0..3] of byte);
      1: (S_addr: cardinal);
  end;

  PSockAddrIn = ^sockaddr_in;
  sockaddr_in = packed record
    case integer of
      0: (sin_family: word;
          sin_port: word;
          sin_addr: in_addr;
          sin_zero: array[0..7] of AnsiChar);
      1: (sa_family: word;
          sa_data: array[0..13] of AnsiChar)
  end;

  PInAddr6 = ^in6_addr;
  in6_addr = packed record
    case integer of
      0: (S6_addr:   packed array [0..15] of byte);
      1: (u6_addr8:  packed array [0..15] of byte);
      2: (u6_addr16: packed array [0..7]  of word);
      3: (u6_addr32: packed array [0..3]  of integer);
  end;

  PSockAddrIn6 = ^sockaddr_in6;
  sockaddr_in6 = packed record
    sin6_family:   word;     // AF_INET6
    sin6_port:     word;     // Transport level port number
    sin6_flowinfo: cardinal; // IPv6 flow information
    sin6_addr:     in6_addr; // IPv6 address
    sin6_scope_id: cardinal; // Scope Id: IF number for link-local
                             //           SITE id for site-local
  end;

const
  INADDR_ANY       = $00000000;
  INADDR_LOOPBACK  = $7F000001;
  INADDR_BROADCAST = $FFFFFFFF;
  INADDR_NONE      = $FFFFFFFF;
  ADDR_ANY		 = INADDR_ANY;
  INVALID_SOCKET		= TSocket(NOT(0));
  SOCKET_ERROR			= -1;

const
  IP_OPTIONS          = 1;
  IP_HDRINCL          = 2;
  IP_TOS              = 3;          { set/get IP Type Of Service       }
  IP_TTL              = 4;          { set/get IP Time To Live          }
  IP_MULTICAST_IF     = 9;          { set/get IP multicast interface   }
  IP_MULTICAST_TTL    = 10;         { set/get IP multicast timetolive  }
  IP_MULTICAST_LOOP   = 11;         { set/get IP multicast loopback    }
  IP_ADD_MEMBERSHIP   = 12;         { add  an IP group membership      }
  IP_DROP_MEMBERSHIP  = 13;         { drop an IP group membership      }
  IP_DONTFRAGMENT     = 14;         { set/get IP Don't Fragment flag   }

  IP_DEFAULT_MULTICAST_TTL   = 1;   { normally limit m'casts to 1 hop  }
  IP_DEFAULT_MULTICAST_LOOP  = 1;   { normally hear sends if a member  }
  IP_MAX_MEMBERSHIPS         = 20;  { per socket; must fit in one mbuf }

  SOL_SOCKET      = $ffff;          { options for socket level }

  { Option flags per-socket }
  SO_DEBUG        = $0001;          { turn on debugging info recording }
  SO_ACCEPTCONN   = $0002;          { socket has had listen() }
  SO_REUSEADDR    = $0004;          { allow local address reuse }
  SO_KEEPALIVE    = $0008;          { keep connections alive }
  SO_DONTROUTE    = $0010;          { just use interface addresses }
  SO_BROADCAST    = $0020;          { permit sending of broadcast msgs }
  SO_USELOOPBACK  = $0040;          { bypass hardware when possible }
  SO_LINGER       = $0080;          { linger on close if data present }
  SO_OOBINLINE    = $0100;          { leave received OOB data in line }
  SO_DONTLINGER  =   $ff7f;
  { Additional options }
  SO_SNDBUF       = $1001;          { send buffer size }
  SO_RCVBUF       = $1002;          { receive buffer size }
  SO_SNDLOWAT     = $1003;          { send low-water mark }
  SO_RCVLOWAT     = $1004;          { receive low-water mark }
  SO_SNDTIMEO     = $1005;          { send timeout }
  SO_RCVTIMEO     = $1006;          { receive timeout }
  SO_ERROR        = $1007;          { get error status and clear }
  SO_TYPE         = $1008;          { get socket type }
  { WinSock 2 extension -- new options }
  SO_GROUP_ID       = $2001;        { ID of a socket group}
  SO_GROUP_PRIORITY = $2002;        { the relative priority within a group}
  SO_MAX_MSG_SIZE   = $2003;        { maximum message size }
  SO_PROTOCOL_INFOA = $2004;        { WSAPROTOCOL_INFOA structure }
  SO_PROTOCOL_INFOW = $2005;        { WSAPROTOCOL_INFOW structure }
  SO_PROTOCOL_INFO  = SO_PROTOCOL_INFOA;
  PVD_CONFIG        = $3001;        { configuration info for service provider }
  { Option for opening sockets for synchronous access }
  SO_OPENTYPE     = $7008;
  SO_SYNCHRONOUS_ALERT    = $10;
  SO_SYNCHRONOUS_NONALERT = $20;
  { Other NT-specific options }
  SO_MAXDG        = $7009;
  SO_MAXPATHDG    = $700A;
  SO_UPDATE_ACCEPT_CONTEXT     = $700B;
  SO_CONNECT_TIME = $700C;

  SOMAXCONN       = $7fffffff;

  IPV6_UNICAST_HOPS      = 8;  // ???
  IPV6_MULTICAST_IF      = 9;  // set/get IP multicast i/f
  IPV6_MULTICAST_HOPS    = 10; // set/get IP multicast ttl
  IPV6_MULTICAST_LOOP    = 11; // set/get IP multicast loopback
  IPV6_JOIN_GROUP        = 12; // add an IP group membership
  IPV6_LEAVE_GROUP       = 13; // drop an IP group membership

  MSG_NOSIGNAL  = 0;

  // getnameinfo constants
  NI_MAXHOST	   = 1025;
  NI_MAXSERV	   = 32;
  NI_NOFQDN 	   = $1;
  NI_NUMERICHOST = $2;
  NI_NAMEREQD	   = $4;
  NI_NUMERICSERV = $8;
  NI_DGRAM       = $10;

const
  SOCK_STREAM     = 1;               { stream socket }
  SOCK_DGRAM      = 2;               { datagram socket }
  SOCK_RAW        = 3;               { raw-protocol interface }
  SOCK_RDM        = 4;               { reliably-delivered message }
  SOCK_SEQPACKET  = 5;               { sequenced packet stream }

  { TCP options }
  TCP_NODELAY     = $0001;

  { Address families }
  AF_UNSPEC       = 0;               { unspecified }
  AF_INET         = 2;               { internetwork: UDP, TCP, etc }
  AF_INET6        = 23;              { Internetwork Version 6 }
  AF_MAX          = 24;

  { Protocol families, same as address families for now }
  PF_UNSPEC       = AF_UNSPEC;
  PF_INET         = AF_INET;
  PF_INET6        = AF_INET6;
  PF_MAX          = AF_MAX;

type
  { Structure used by kernel to store most addresses }
  PSockAddr = ^TSockAddr;
  TSockAddr = sockaddr_in;

type
  PAddrInfo = ^TAddrInfo;
  TAddrInfo = record
    ai_flags: integer;       // AI_PASSIVE, AI_CANONNAME, AI_NUMERICHOST
    ai_family: integer;      // PF_xxx
    ai_socktype: integer;    // SOCK_xxx
    ai_protocol: integer;    // 0 or IPPROTO_xxx for IPv4 and IPv6
    ai_addrlen: integer;     // Length of ai_addr
    ai_canonname: PAnsiChar; // Canonical name for nodename
    ai_addr: PSockAddr;      // Binary address
    ai_next: PAddrInfo;      // Next structure in linked list
  end;

const
  // Flags used in "hints" argument to getaddrinfo().
  AI_PASSIVE     = $1;  // Socket address will be used in bind() call
  AI_CANONNAME   = $2;  // Return canonical name in first ai_canonname
  AI_NUMERICHOST = $4;  // Nodename must be a numeric address AnsiString

type
  { Structure used for manipulating the linger option }
  PLinger = ^TLinger;
  TLinger = packed record
    l_onoff: word;
    l_linger: word;
  end;

const
  MSG_OOB       = $01;  // Process out-of-band data
  MSG_PEEK      = $02;  // Peek at incoming messages

const
  NO_ERROR = 0;

  { All Windows Sockets error constants are biased by WSABASEERR from the "normal" }
  WSABASEERR              = 10000;

  { Windows Sockets definitions of regular Microsoft C error constants }
  WSAEINTR                = (WSABASEERR+4);
  WSAEBADF                = (WSABASEERR+9);
  WSAEACCES               = (WSABASEERR+13);
  WSAEFAULT               = (WSABASEERR+14);
  WSAEINVAL               = (WSABASEERR+22);
  WSAEMFILE               = (WSABASEERR+24);

  { Windows Sockets definitions of regular Berkeley error constants }
  WSAEWOULDBLOCK          = (WSABASEERR+35);
  WSAEINPROGRESS          = (WSABASEERR+36);
  WSAEALREADY             = (WSABASEERR+37);
  WSAENOTSOCK             = (WSABASEERR+38);
  WSAEDESTADDRREQ         = (WSABASEERR+39);
  WSAEMSGSIZE             = (WSABASEERR+40);
  WSAEPROTOTYPE           = (WSABASEERR+41);
  WSAENOPROTOOPT          = (WSABASEERR+42);
  WSAEPROTONOSUPPORT      = (WSABASEERR+43);
  WSAESOCKTNOSUPPORT      = (WSABASEERR+44);
  WSAEOPNOTSUPP           = (WSABASEERR+45);
  WSAEPFNOSUPPORT         = (WSABASEERR+46);
  WSAEAFNOSUPPORT         = (WSABASEERR+47);
  WSAEADDRINUSE           = (WSABASEERR+48);
  WSAEADDRNOTAVAIL        = (WSABASEERR+49);
  WSAENETDOWN             = (WSABASEERR+50);
  WSAENETUNREACH          = (WSABASEERR+51);
  WSAENETRESET            = (WSABASEERR+52);
  WSAECONNABORTED         = (WSABASEERR+53);
  WSAECONNRESET           = (WSABASEERR+54);
  WSAENOBUFS              = (WSABASEERR+55);
  WSAEISCONN              = (WSABASEERR+56);
  WSAENOTCONN             = (WSABASEERR+57);
  WSAESHUTDOWN            = (WSABASEERR+58);
  WSAETOOMANYREFS         = (WSABASEERR+59);
  WSAETIMEDOUT            = (WSABASEERR+60);
  WSAECONNREFUSED         = (WSABASEERR+61);
  WSAELOOP                = (WSABASEERR+62);
  WSAENAMETOOLONG         = (WSABASEERR+63);
  WSAEHOSTDOWN            = (WSABASEERR+64);
  WSAEHOSTUNREACH         = (WSABASEERR+65);
  WSAENOTEMPTY            = (WSABASEERR+66);
  WSAEPROCLIM             = (WSABASEERR+67);
  WSAEUSERS               = (WSABASEERR+68);
  WSAEDQUOT               = (WSABASEERR+69);
  WSAESTALE               = (WSABASEERR+70);
  WSAEREMOTE              = (WSABASEERR+71);

  { Extended Windows Sockets error constant definitions }
  WSASYSNOTREADY          = (WSABASEERR+91);
  WSAVERNOTSUPPORTED      = (WSABASEERR+92);
  WSANOTINITIALISED       = (WSABASEERR+93);
  WSAEDISCON              = (WSABASEERR+101);
  WSAENOMORE              = (WSABASEERR+102);
  WSAECANCELLED           = (WSABASEERR+103);
  WSAEEINVALIDPROCTABLE   = (WSABASEERR+104);
  WSAEINVALIDPROVIDER     = (WSABASEERR+105);
  WSAEPROVIDERFAILEDINIT  = (WSABASEERR+106);
  WSASYSCALLFAILURE       = (WSABASEERR+107);
  WSASERVICE_NOT_FOUND    = (WSABASEERR+108);
  WSATYPE_NOT_FOUND       = (WSABASEERR+109);
  WSA_E_NO_MORE           = (WSABASEERR+110);
  WSA_E_CANCELLED         = (WSABASEERR+111);
  WSAEREFUSED             = (WSABASEERR+112);

  { Error return codes from gethostbyname() and gethostbyaddr() }
  { Authoritative Answer: Host not found }
  WSAHOST_NOT_FOUND       = (WSABASEERR+1001);
  { Non-Authoritative: Host not found, or SERVERFAIL }
  WSATRY_AGAIN            = (WSABASEERR+1002);
  { Non recoverable errors, FORMERR, REFUSED, NOTIMP }
  WSANO_RECOVERY          = (WSABASEERR+1003);
  { Valid name, no data record of requested type }
  WSANO_DATA              = (WSABASEERR+1004);
  { no address, look for MX record }
  WSANO_ADDRESS           = WSANO_DATA;

const
  WSADESCRIPTION_LEN     =   256;
  WSASYS_STATUS_LEN      =   128;

  SHUT_RD = 0;
  SHUT_WR = 1;
  SHUT_RDWR = 2;

type
  PWSAData = ^TWSAData;
  TWSAData = record
     wVersion: word;
     wHighVersion: word;
     {$ifdef WIN64}
     iMaxSockets: word;
     iMaxUdpDg: word;
     lpVendorInfo: PAnsiChar;
     szDescription: array[0..WSADESCRIPTION_LEN] of AnsiChar;
     szSystemStatus: array[0..WSASYS_STATUS_LEN] of AnsiChar;
     {$else}
     szDescription: array[0..WSADESCRIPTION_LEN] of AnsiChar;
     szSystemStatus: array[0..WSASYS_STATUS_LEN] of AnsiChar;
     iMaxSockets: word;
     iMaxUdpDg: word;
     lpVendorInfo: PAnsiChar;
     {$endif WIN64}
  end;
  WSAData = TWSAData;

const
  _ST: array[TNetLayer] of integer = (SOCK_STREAM, SOCK_DGRAM, 0);
  _IP: array[TNetLayer] of integer = (IPPROTO_TCP, IPPROTO_UDP, 0);


{ ******** WinSock2 API calls }

function WSAStartup(wVersionRequired: word; var WSData: TWSAData): integer; stdcall;
  external ws2 name 'WSAStartup';
function WSACleanup: integer; stdcall;
  external ws2 name 'WSACleanup';
function sockerrno: integer; stdcall;
  external ws2 name 'WSAGetLastError';

function getaddrinfo(NodeName: PAnsiChar; ServName: PAnsiChar; Hints: PAddrInfo;
   var Addrinfo: PAddrInfo): integer; stdcall;
  external ws2 name 'getaddrinfo';
procedure freeaddrinfo(ai: PAddrInfo); stdcall;
  external ws2 name 'freeaddrinfo';
function socket(af, struct, protocol: integer): TSocket; stdcall;
  external ws2 name 'socket';
function setsockopt(s: TSocket; level, optname: integer;
   optval: pointer; optlen: integer): integer; stdcall;
  external ws2 name 'setsockopt';
function ioctlsocket(s: TSocket; cmd: cardinal; arg: PCardinal): integer; stdcall;
  external ws2 name 'ioctlsocket';
function shutdown(s: TSocket; how: integer): integer; stdcall;
  external ws2 name 'shutdown';
function closesocket(s: TSocket): integer; stdcall;
  external ws2 name 'closesocket';
function getnameinfo(addr: PSockAddr; namelen: integer; host: PAnsiChar;
   hostlen: cardinal; serv: PAnsiChar; servlen: cardinal; flags: integer): integer; stdcall;
  external ws2 name 'getnameinfo';
function bind(s: TSocket; addr: PSockAddr; namelen: integer): integer; stdcall;
  external ws2 name 'bind';
function listen(s: TSocket; backlog: integer): integer; stdcall;
  external ws2 name 'listen';
function accept(s: TSocket; addr: PSockAddr; var addrlen: integer): TSocket; stdcall;
  external ws2 name 'accept';
function connect(s: TSocket; name: PSockAddr; namelen: integer): integer; stdcall;
  external ws2 name 'connect';
function select(nfds: integer; readfds, writefds, exceptfds: PFDSet;
   timeout: PTimeVal): integer; stdcall;
  external ws2 name 'select';
function recv(s: TSocket; Buf: Pointer; len, flags: integer): integer; stdcall;
  external ws2 name 'recv';
function recvfrom(s: TSocket; Buf: Pointer; len, flags: integer;
   from: PSockAddr; fromlen: Pinteger): integer; stdcall;
  external ws2 name 'recvfrom';
function send(s: TSocket; Buf: Pointer; len, flags: integer): integer; stdcall;
  external ws2 name 'send';
function sendto(s: TSocket; Buf: Pointer; len, flags: integer;
   addrto: PSockAddr; tolen: integer): integer; stdcall;
  external ws2 name 'sendto';
function getpeername(s: TSocket; name: PSockAddr; var namelen: integer): integer; stdcall;
  external ws2 name 'getpeername';


{ ******** TNetSocket Cross-Platform Wrapper }

{ TNetAddr }

function SetAddr(const ip, port: RawUTF8; var h: TAddrInfo; var a: TSockAddr): boolean;
var
  res: PAddrInfo;
begin
  res := nil;
  FillCharFast(a, SizeOf(a), 0);
  if h.ai_socktype = SOCK_RAW then
  begin
    h.ai_socktype := 0;
    h.ai_protocol := 0;
    result := getaddrinfo(pointer(ip), nil, @h, res) = NO_ERROR;
  end
  else if (ip = cAnyHost) or
          (ip = c6AnyHost) then
  begin
    // 0.0.0.0
    h.ai_flags := AI_PASSIVE;
    result := getaddrinfo(nil, pointer(port), @h, res) = NO_ERROR;
  end
  else if (ip = '') or
          (ip = cLocalhost) or
          (ip = c6Localhost) or
          (PropNameEquals(ip, 'localhost')) then
    // 127.0.0.1
    result := getaddrinfo(nil, pointer(port), @h, res) = NO_ERROR
  else
    // ip or hostname
    result := getaddrinfo(pointer(ip), pointer(port), @h, res) = NO_ERROR;
  if result then
    MoveFast(res^.ai_addr^, a, res^.ai_addrlen);
  if res <> nil then
    freeaddrinfo(res);
end;

function TNetAddr.SetFrom(const address, addrport: RawUTF8;
  layer: TNetLayer): TNetResult;
var
  h4, h6: TAddrInfo;
begin
  if layer = nlUNIX then
    result := nrNotImplemented
  else
  begin
    // first try IP4, then IP6
    FillCharFast(h4, SizeOf(h4), 0);
    h4.ai_family := AF_INET;
    h4.ai_socktype := _ST[layer];
    h4.ai_protocol := _IP[layer];
    h6 := h4;
    h6.ai_family := AF_INET6;
    if SetAddr(address, addrport, h4, PSockAddr(@Addr)^) or
       SetAddr(address, addrport, h6, PSockAddr(@Addr)^) then
      result := nrOK
    else
      result := nrNotFound;
  end;
end;


{ TNetSocketWrap }

procedure TNetSocketWrap.SetSendTimeout(ms: integer);
begin
  // WinAPI expects the time out directly as ms integer
  SetOpt(SOL_SOCKET, SO_SNDTIMEO, @ms, SizeOf(ms));
end;

procedure TNetSocketWrap.SetReceiveTimeout(ms: integer);
begin
  SetOpt(SOL_SOCKET, SO_RCVTIMEO, @ms, SizeOf(ms));
end;

procedure TNetSocketWrap.SetLinger(linger: integer);
var
  v: TLinger;
begin
  v.l_linger := linger;
  v.l_onoff := ord(linger >= 0);
  SetOpt(SOL_SOCKET, SO_LINGER, @v, SizeOf(v));
end;

procedure SetTimeVal(ms: cardinal; out tv: TTimeVal);
begin
  if ms = 0 then
  begin
    tv.tv_sec := 0;
    tv.tv_usec := 0;
  end
  else
  begin
    tv.tv_sec := ms div 1000;
    tv.tv_usec := (ms - (cardinal(tv.tv_sec) * 1000)) * 1000;
  end;
end;

function TNetSocketWrap.WaitFor(ms: integer; scope: TNetEvents): TNetEvents;
var
  sock: TSocket;
  rd, wr: record
    fd_count: integer;
    fd_array: array[0..0] of TSocket; // no need of 512 entries here
  end;
  rdp, wrp: PFDSet;
  tv: TTimeVal;
  res: integer;
  pending: cardinal;
begin
  result := [neError];
  if @self = nil then
    exit;
  rdp := nil;
  wrp := nil;
  sock := TSocket(@self);
  if neRead in scope then
  begin
    rd.fd_array[0] := sock;
    rd.fd_count := 1;
    rdp := @rd;
  end;
  if (neWrite in scope) or
     not (neRead in scope) then // a least one is needed
  begin
    wr.fd_array[0] := sock;
    wr.fd_count := 1;
    wrp := @wr;
  end;
  SetTimeVal(ms, tv);
  res := select(sock + 1, rdp, wrp, nil, @tv);
  if res < 0 then
    exit;
  result := [];
  if res = 0 then
    // nothing new detected
    exit;
  if (rdp <> nil) and
     ({%H-}rd.fd_count = 1) and
     (rd.fd_array[0] = sock) then
  begin
    // Windows expects to check FIONREAD for graceful disconnection
    if ioctlsocket(sock, FIONREAD, @pending) <> NO_ERROR then
      include(result, neError)
    else if pending > 0 then
      include(result, neRead)
    else
      include(result, neClosed);
  end;
  if (wrp <> nil) and
     ({%H-}wr.fd_count = 1) and
     (wr.fd_array[0] = sock) then
    include(result, neWrite);
end;


{ ******************** Efficient Multiple Sockets Polling }

function FD_ISSET(Socket: TNetSocket; const FDSet: TFDSet): boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
  result := PtrUIntScanExists(@FDSet.fd_array, FDSet.fd_count, PtrUInt(Socket));
end;

procedure FD_CLR(Socket: TNetSocket; var FDSet: TFDSet);
var
  i: integer;
begin
  i := PtrUIntScanIndex(@FDSet.fd_array, FDSet.fd_count, PtrUInt(Socket));
  if i < 0 then
    exit;
  dec(FDSet.fd_count);
  if i < FDSet.fd_count then
    MoveFast(FDSet.fd_array[i + 1], FDSet.fd_array[i], (FDSet.fd_count - i) * sizeof(TSocket));
end;

procedure FD_SET(Socket: TNetSocket; var FDSet: TFDSet);
begin
  if (FDSet.fd_count >= FD_SETSIZE) or
     PtrUIntScanExists(@FDSet.fd_array, FDSet.fd_count, PtrUInt(Socket)) then
    exit;
  FDSet.fd_array[FDSet.fd_count] := TSocket(Socket);
  inc(FDSet.fd_count);
end;

type
  /// socket polling via Windows' Select() API
  // - under Windows, Select() handles up to 64 TSocket by default, but we
  // grow FD_SETSIZE up to 512 for the purpose of this class
  // - under Linux, select() is very limited, so poll/epoll APIs are to be used
  // - in practice, TPollSocketSelect is FASTER than TPollSocketPoll + WSAPoll()
  TPollSocketSelect = class(TPollSocketAbstract)
  protected
    fHighestSocket: TNetSocket;
    fTagSocket: array of TNetSocket;
    fTagTag: array of TPollSocketTag;
    fRead: TFDSet;
    fWrite: TFDSet;
  public
    constructor Create; override;
    function Subscribe(socket: TNetSocket; events: TPollSocketEvents;
      tag: TPollSocketTag): boolean; override;
    function Unsubscribe(socket: TNetSocket): boolean; override;
    function WaitForModified(out results: TPollSocketResults;
      timeoutMS: integer): integer; override;
  end;

constructor TPollSocketSelect.Create;
begin
  inherited Create;
  fMaxSockets := FD_SETSIZE; // 512 in our unit!
  SetLength(fTagSocket, FD_SETSIZE);
  SetLength(fTagTag, FD_SETSIZE);
end;

function TPollSocketSelect.Subscribe(socket: TNetSocket;
  events: TPollSocketEvents; tag: TPollSocketTag): boolean;
begin
  result := false;
  if (self = nil) or
     (socket = nil) or
     (byte(events) = 0) or
     (fCount = fMaxSockets) or
     PtrUIntScanExists(pointer(fTagSocket), fCount, PtrUInt(socket)) then
    exit;
  if pseRead in events then
    FD_SET(socket, fRead);
  if pseWrite in events then
    FD_SET(socket, fWrite);
  fTagSocket[fCount] := socket;
  fTagTag[fCount] := tag;
  inc(fCount);
  if PtrUInt(socket) > PtrUInt(fHighestSocket) then
    fHighestSocket := socket;
  result := true;
end;

function TPollSocketSelect.Unsubscribe(socket: TNetSocket): boolean;
var
  i: PtrInt;
begin
  result := false;
  if (self <> nil) and
     (socket <> nil) then
  begin
    i := PtrUIntScanIndex(pointer(fTagSocket), fCount, PtrUInt(socket));
    if i < 0 then
      exit;
    FD_CLR(socket, fRead);
    FD_CLR(socket, fWrite);
    dec(fCount);
    if i < fCount then
    begin
      MoveFast(fTagSocket[i + 1], fTagSocket[i], (fCount - i) * sizeof(fTagSocket[i]));
      MoveFast(fTagTag[i + 1], fTagTag[i], (fCount - i) * sizeof(fTagTag[i]));
    end;
    if fCount = 0 then
      fHighestSocket := nil;
    result := true;
  end;
end;

function TPollSocketSelect.WaitForModified(out results: TPollSocketResults;
  timeoutMS: integer): integer;
var
  tv: TTimeVal;
  rdp, wrp: PFDSet;
  ev: TPollSocketEvents;
  i: PtrInt;
  pending: cardinal;
  sock: TNetSocket;
  rd, wr: TFDSet;
  tmp: array[0..FD_SETSIZE - 1] of TPollSocketResult;
begin
  result := -1; // error
  if (self = nil) or
     (fCount = 0) then
    exit;
  if fRead.fd_count > 0 then
  begin
    rd := fRead;
    rdp := @rd;
  end
  else
    rdp := nil;
  if fWrite.fd_count > 0 then
  begin
    wr := fWrite;
    wrp := @wr;
  end
  else
    wrp := nil;
  SetTimeVal(timeoutMS, tv);
  result := Select(PtrUInt(fHighestSocket) + 1, rdp, wrp, nil, @tv);
  if result <= 0 then
    exit;
  result := 0;
  for i := 0 to fCount - 1 do
  begin
    byte(ev) := 0;
    sock := fTagSocket[i];
    if (rdp <> nil) and FD_ISSET(sock, rd{%H-}) then
    begin
      if (IoctlSocket(TSocket(sock), FIONREAD, @pending) = NO_ERROR) and
         ({%H-}pending = 0) then
        // socket closed gracefully - see TCrtSocket.SockReceivePending
        include(ev, pseClosed)
      else
        include(ev, pseRead);
    end;
    if (wrp <> nil) and FD_ISSET(sock, wr{%H-}) then
      include(ev, pseWrite);
    if byte(ev) <> 0 then
      with {%H-}tmp[result] do
      begin
        events := ev;
        tag := fTagTag[i];
        inc(result);
      end;
  end;
  SetLength(results, result);
  MoveFast(tmp, results[0], result * sizeof(tmp[0]));
end;


function PollSocketClass: TPollSocketClass;
begin
  result := TPollSocketSelect;
end;


{ ******************** TLS / HTTPS Encryption Abstract Layer }

{ SChannel low-level API  }

type
  TCredHandle = record
    dwLower: pointer;
    dwUpper: pointer;
  end;
  PCredHandle = ^TCredHandle;

  TCtxtHandle = type TCredHandle;
  PCtxtHandle = ^TCtxtHandle;

  {$ifdef DELPHI5OROLDER}
  PCardinal = ^Cardinal;
  {$endif}

  TSChannelCred = record
    dwVersion: cardinal;
    cCreds: cardinal;
    paCred: pointer;
    hRootStore: THandle;
    cMappers: cardinal;
    aphMappers: pointer;
    cSupportedAlgs: cardinal;
    palgSupportedAlgs: PCardinal;
    grbitEnabledProtocols: cardinal;
    dwMinimumCipherStrength: cardinal;
    dwMaximumCipherStrength: cardinal;
    dwSessionLifespan: cardinal;
    dwFlags: cardinal;
    dwCredFormat: cardinal;
  end;
  PSChannelCred = ^TSChannelCred;

  TSecBuffer = record
    cbBuffer: cardinal;
    BufferType: cardinal;
    pvBuffer: pointer;
  end;
  PSecBuffer = ^TSecBuffer;

  TSecBufferDesc = record
    ulVersion: cardinal;
    cBuffers: cardinal;
    pBuffers: PSecBuffer;
  end;
  PSecBufferDesc = ^TSecBufferDesc;

  TTimeStamp = record
    dwLowDateTime: cardinal;
    dwHighDateTime: cardinal;
  end;
  PTimeStamp = ^TTimeStamp;

  TSecPkgContextStreamSizes = record
    cbHeader: cardinal;
    cbTrailer: cardinal;
    cbMaximumMessage: cardinal;
    cBuffers: cardinal;
    cbBlockSize: cardinal;
  end;
  PSecPkgContextStreamSizes = ^TSecPkgContextStreamSizes;

  ESChannel = class(Exception);

  TSChannelClient = class(TInterfacedObject, INetTLS)
  private
    Cred: TCredHandle;
    Ctxt: TCtxtHandle;
    Sizes: TSecPkgContextStreamSizes;
    Data, Input: AnsiString;
    InputSize, DataPos, DataCount, InputCount: integer;
    procedure HandshakeLoop(aSocket: THandle);
    procedure AppendData(const aBuffer: TSecBuffer);
  public
    procedure AfterConnection(aSocket: TNetSocket; const aServerAddress: RawUTF8);
    procedure BeforeDisconnection(aSocket: TNetSocket);
    function Receive(aSocket: TNetSocket; aBuffer: pointer; var aLength: integer): TNetResult;
    function Send(aSocket: TNetSocket; aBuffer: pointer; var aLength: integer): TNetResult;
  end;

var
  AcquireCredentialsHandle: function(pszPrincipal: PAnsiChar;
    pszPackage: PAnsiChar; fCredentialUse: cardinal; pvLogonID: PInt64;
    pAuthData: PSChannelCred; pGetKeyFn: pointer; pvGetKeyArgument: pointer;
    phCredential: PCredHandle; ptsExpiry: PTimeStamp): cardinal; stdcall;
  FreeCredentialsHandle: function(phCredential: PCredHandle): cardinal; stdcall;
  InitializeSecurityContext: function(phCredential: PCredHandle;
    phContext: PCtxtHandle; pszTargetName: PAnsiChar; fContextReq: cardinal;
    Reserved1: cardinal; TargetDataRep: cardinal; pInput: PSecBufferDesc;
    Reserved2: cardinal; phNewContext: PCtxtHandle; pOutput: PSecBufferDesc;
    pfContextAttr: PCardinal; ptsExpiry: PTimeStamp): cardinal; stdcall;
  DeleteSecurityContext: function(phContext: PCtxtHandle): cardinal; stdcall;
  ApplyControlToken: function(phContext: PCtxtHandle;
    pInput: PSecBufferDesc): cardinal; stdcall;
  QueryContextAttributes: function(phContext: PCtxtHandle;
    ulAttribute: cardinal; pBuffer: pointer): cardinal; stdcall;
  FreeContextBuffer: function(pvContextBuffer: pointer): cardinal; stdcall;
  EncryptMessage: function(phContext: PCtxtHandle; fQOP: cardinal;
    pMessage: PSecBufferDesc; MessageSeqNo: cardinal): cardinal; stdcall;
  DecryptMessage: function(phContext: PCtxtHandle; pMessage: PSecBufferDesc;
    MessageSeqNo: cardinal; pfQOP: PCardinal): cardinal; stdcall;

const
  SP_PROT_TLS1 = $0C0;
  SP_PROT_TLS1_SERVER = $040;
  SP_PROT_TLS1_CLIENT = $080;
  SP_PROT_TLS1_1 = $300;
  SP_PROT_TLS1_1_SERVER = $100;
  SP_PROT_TLS1_1_CLIENT = $200;
  SP_PROT_TLS1_2 = $C00;
  SP_PROT_TLS1_2_SERVER = $400;
  SP_PROT_TLS1_2_CLIENT = $800;

  SECPKG_CRED_INBOUND = 1;
  SECPKG_CRED_OUTBOUND = 2;

  ISC_REQ_DELEGATE = $00000001;
  ISC_REQ_MUTUAL_AUTH = $00000002;
  ISC_REQ_REPLAY_DETECT = $00000004;
  ISC_REQ_SEQUENCE_DETECT = $00000008;
  ISC_REQ_CONFIDENTIALITY = $00000010;
  ISC_REQ_USE_SESSION_KEY = $00000020;
  ISC_REQ_PROMPT_FOR_CREDS = $00000040;
  ISC_REQ_USE_SUPPLIED_CREDS = $00000080;
  ISC_REQ_ALLOCATE_MEMORY = $00000100;
  ISC_REQ_USE_DCE_STYLE = $00000200;
  ISC_REQ_DATAGRAM = $00000400;
  ISC_REQ_CONNECTION = $00000800;
  ISC_REQ_CALL_LEVEL = $00001000;
  ISC_REQ_FRAGMENT_SUPPLIED = $00002000;
  ISC_REQ_EXTENDED_ERROR = $00004000;
  ISC_REQ_STREAM = $00008000;
  ISC_REQ_INTEGRITY = $00010000;
  ISC_REQ_IDENTIFY = $00020000;
  ISC_REQ_NULL_SESSION = $00040000;
  ISC_REQ_MANUAL_CRED_VALIDATION = $00080000;
  ISC_REQ_RESERVED1 = $00100000;
  ISC_REQ_FRAGMENT_TO_FIT = $00200000;
  ISC_REQ_FLAGS =
    ISC_REQ_SEQUENCE_DETECT or ISC_REQ_REPLAY_DETECT or
    ISC_REQ_CONFIDENTIALITY or ISC_REQ_EXTENDED_ERROR or
    ISC_REQ_ALLOCATE_MEMORY or ISC_REQ_STREAM;

  SECBUFFER_VERSION = 0;
  SECBUFFER_EMPTY = 0;
  SECBUFFER_DATA = 1;
  SECBUFFER_TOKEN = 2;
  SECBUFFER_EXTRA = 5;
  SECBUFFER_STREAM_TRAILER = 6;
  SECBUFFER_STREAM_HEADER = 7;

  SEC_E_OK = 0;
  SEC_I_CONTINUE_NEEDED = $00090312;
  SEC_I_RENEGOTIATE = $00090321;
  SEC_I_CONTEXT_EXPIRED	= $00090317;
  SEC_E_INCOMPLETE_MESSAGE = $80090318;
  SEC_E_INVALID_TOKEN = $80090308;

  DLLSecur32: PChar = 'secur32.dll';
  UNISP_NAME = 'Microsoft Unified Security Protocol Provider';
  SECPKG_ATTR_STREAM_SIZES = 4;
  SECURITY_NATIVE_DREP = $10;
  SCHANNEL_SHUTDOWN = 1;

{ TSChannel }

procedure ESChannelRaiseLastError; // not defined e.g. with Delphi 5
var
  LastError: Integer;
begin
  LastError := GetLastError;
  raise ESChannel.CreateFmt('System Error %d [%s]', [LastError, SysErrorMessage(LastError)]);
end;

function CheckSEC_E_OK(res: integer): cardinal;
begin
  if res <> SEC_E_OK then
    ESChannelRaiseLastError;
  result := res;
end;

function CheckSocket(res: integer): cardinal;
begin
  if res = SOCKET_ERROR then
    raise ESChannel.CreateFmt('Socket Error %d', [sockerrno]);
  if res = 0 then
    raise ESChannel.Create('Handshake aborted');
  result := res;
end;

const
  TLSRECMAXSIZE = 19000; // stack buffers for TSChannelClient.Receive/Send

type
  {$ifdef USERECORDWITHMETHODS}THandshakeBuf = record
    {$else}THandshakeBuf = object{$endif}
  public
    buf: array[0..2] of TSecBuffer;
    input, output: TSecBufferDesc;
    procedure Init;
  end;

procedure THandshakeBuf.Init;
begin
  input.ulVersion := SECBUFFER_VERSION;
  input.cBuffers := 2;
  input.pBuffers := @buf[0];
  buf[0].cbBuffer := 0;
  buf[0].BufferType := SECBUFFER_TOKEN;
  buf[0].pvBuffer := nil;
  buf[1].cbBuffer := 0;
  buf[1].BufferType := SECBUFFER_EMPTY;
  buf[1].pvBuffer := nil;
  output.ulVersion := SECBUFFER_VERSION;
  output.cBuffers := 1;
  output.pBuffers := @buf[2];
  buf[2].cbBuffer := 0;
  buf[2].BufferType := SECBUFFER_TOKEN;
  buf[2].pvBuffer := nil;
end;

procedure TSChannelClient.AppendData(const aBuffer: TSecBuffer);
var
  newlen: integer;
begin
  newlen := DataCount + integer(aBuffer.cbBuffer);
  if newlen > Length(Data) then
    SetLength(Data, newlen);
  MoveFast(aBuffer.pvBuffer^, PByteArray(Data)[DataCount], aBuffer.cbBuffer);
  inc(DataCount, aBuffer.cbBuffer);
end;

procedure TSChannelClient.AfterConnection(aSocket: TNetSocket;
  const aServerAddress: RawUTF8);
var
  buf: THandshakeBuf;
  res, f: cardinal;
begin
  if not Assigned(AcquireCredentialsHandle) then
    raise ESChannel.Create('SChannel API not available');
  CheckSEC_E_OK(AcquireCredentialsHandle(nil, UNISP_NAME, SECPKG_CRED_OUTBOUND,
    nil, nil, nil, nil, @Cred, nil));
  DataPos := 0;
  DataCount := 0;
  buf.Init;
  res := InitializeSecurityContext(@Cred, nil, pointer(aServerAddress), ISC_REQ_FLAGS, 0,
    SECURITY_NATIVE_DREP, nil, 0, @Ctxt, @buf.output, @f, nil);
  if res <> SEC_I_CONTINUE_NEEDED then
    ESChannelRaiseLastError;
  CheckSocket(mormot.net.sock.Send(aSocket.Socket, buf.buf[2].pvBuffer, buf.buf[2].cbBuffer, 0));
  CheckSEC_E_OK(FreeContextBuffer(buf.buf[2].pvBuffer));
  HandshakeLoop(aSocket.Socket);
  CheckSEC_E_OK(QueryContextAttributes(@Ctxt, SECPKG_ATTR_STREAM_SIZES, @Sizes));
  SetLength(Data, Sizes.cbMaximumMessage);
  InputSize := Sizes.cbHeader + Sizes.cbMaximumMessage + Sizes.cbTrailer;
  if InputSize > TLSRECMAXSIZE then
    raise ESChannel.CreateFmt('InputSize=%d>%d', [InputSize, TLSRECMAXSIZE]);
  SetLength(Input, InputSize);
  InputCount := 0;
end;

procedure TSChannelClient.HandshakeLoop(aSocket: THandle);
var
  buf: THandshakeBuf;
  len: integer;
  tmp: AnsiString;
  res, f: cardinal;
begin
  len := 0;
  SetLength(tmp, 65536);
  res := SEC_I_CONTINUE_NEEDED;
  while (res = SEC_I_CONTINUE_NEEDED) or
        (res = SEC_E_INCOMPLETE_MESSAGE) do
  begin
    if res <> SEC_E_INCOMPLETE_MESSAGE then
      len := 0;
    inc(len, CheckSocket(Recv(aSocket, @PByteArray(tmp)[len], length(tmp) - len, 0)));
    buf.Init;
    buf.buf[0].cbBuffer := len;
    buf.buf[0].BufferType := SECBUFFER_TOKEN;
    buf.buf[0].pvBuffer := pointer(tmp);
    res := InitializeSecurityContext(@Cred, @Ctxt, nil, ISC_REQ_FLAGS, 0,
      SECURITY_NATIVE_DREP, @buf.input, 0, @Ctxt, @buf.output, @f, nil);
    if (res = SEC_E_OK) or
       (res = SEC_I_CONTINUE_NEEDED) or
       ((f and ISC_REQ_EXTENDED_ERROR) <> 0) then
    begin
      if (buf.buf[2].cbBuffer <> 0) and
         (buf.buf[2].pvBuffer <> nil) then
      begin
        CheckSocket(mormot.net.sock.Send(
          aSocket, buf.buf[2].pvBuffer, buf.buf[2].cbBuffer, 0));
        CheckSEC_E_OK(FreeContextBuffer(buf.buf[2].pvBuffer));
      end;
    end;
  end;
  // TODO: handle SEC_I_INCOMPLETE_CREDENTIALS ?
  // see https://github.com/curl/curl/blob/master/lib/vtls/schannel.c
  CheckSEC_E_OK(res);
  if buf.buf[1].BufferType = SECBUFFER_EXTRA then
    AppendData(buf.buf[1]);
end;

procedure TSChannelClient.BeforeDisconnection(aSocket: TNetSocket);
var
  desc: TSecBufferDesc;
  buf: TSecBuffer;
  dt, f: cardinal;
begin
  try
    if {%H-}PtrInt(aSocket) > 0 then
    begin
      desc.ulVersion := SECBUFFER_VERSION;
      desc.cBuffers := 1;
      desc.pBuffers := @buf;
      buf.cbBuffer := 4;
      buf.BufferType := SECBUFFER_TOKEN;
      dt := SCHANNEL_SHUTDOWN;
      buf.pvBuffer := @dt;
      if ApplyControlToken(@Ctxt, @desc) = SEC_E_OK then
      begin
        buf.cbBuffer := 0;
        buf.BufferType := SECBUFFER_TOKEN;
        buf.pvBuffer := nil;
        if InitializeSecurityContext(@Cred, @Ctxt, nil, ISC_REQ_FLAGS, 0,
           SECURITY_NATIVE_DREP, nil, 0, @Ctxt, @desc, @f, nil) = SEC_E_OK then
        begin
          mormot.net.sock.Send(aSocket.Socket, buf.pvBuffer, buf.cbBuffer, 0);
          FreeContextBuffer(buf.pvBuffer);
        end;
      end;
    end;
    DeleteSecurityContext(@Ctxt);
    FreeCredentialsHandle(@Cred);
  finally
    Cred.dwLower := nil;
    Cred.dwUpper := nil;
  end;
end;

function TSChannelClient.Receive(aSocket: TNetSocket;
  aBuffer: pointer; var aLength: integer): TNetResult;
var
  desc: TSecBufferDesc;
  buf: array[0..3] of TSecBuffer;
  res: cardinal;
  read, i: integer;
  needsRenegotiate: boolean;

  function DecryptInput: cardinal;
  begin
    buf[0].cbBuffer := InputCount;
    buf[0].BufferType := SECBUFFER_DATA;
    buf[0].pvBuffer := pointer(Input);
    buf[1].cbBuffer := 0;
    buf[1].BufferType := SECBUFFER_EMPTY;
    buf[1].pvBuffer := nil;
    buf[2].cbBuffer := 0;
    buf[2].BufferType := SECBUFFER_EMPTY;
    buf[2].pvBuffer := nil;
    buf[3].cbBuffer := 0;
    buf[3].BufferType := SECBUFFER_EMPTY;
    buf[3].pvBuffer := nil;
    result := DecryptMessage(@Ctxt, @desc, 0, nil);
  end;

begin
  result := nrFatalError;
  while DataCount = 0 do
  try
    DataPos := 0;
    desc.ulVersion := SECBUFFER_VERSION;
    desc.cBuffers := 4;
    desc.pBuffers := @buf[0];
    repeat
      read := Recv(aSocket.Socket, @PByteArray(Input)[InputCount],
        InputSize - InputCount, MSG_NOSIGNAL);
      if read <= 0 then
      begin
        result := NetLastError; // may be nrRetry for WSATRY_AGAIN
        if result = nrOK then
          result := nrUnknownError;
        exit;
      end;
      inc(InputCount, read);
      res := DecryptInput;
    until res <> SEC_E_INCOMPLETE_MESSAGE;
    needsRenegotiate := false;
    repeat
      case res of
        SEC_I_RENEGOTIATE:
          needsRenegotiate := true;
        SEC_I_CONTEXT_EXPIRED:
          exit;
        SEC_E_INCOMPLETE_MESSAGE:
          break;
        else
          CheckSEC_E_OK(res);
      end;
      InputCount := 0;
      for i := 1 to 3 do
        case buf[i].BufferType of
          SECBUFFER_DATA:
            AppendData(buf[i]);
          SECBUFFER_EXTRA:
            begin
              MoveFast(buf[i].pvBuffer^, pointer(Input)^, buf[i].cbBuffer);
              InputCount := buf[i].cbBuffer;
            end;
        end;
      if InputCount = 0 then
        break;
      res := DecryptInput;
    until false;
    if needsRenegotiate then
      HandshakeLoop(aSocket.Socket);
  except
    exit; // shutdown the connection on ESChannel fatal error
  end;
  read := DataCount;
  if aLength < read then
    read := aLength;
  MoveFast(PByteArray(Data)[DataPos], aBuffer^, read);
  inc(DataPos, read);
  dec(DataCount, read);
  aLength := read;
  result := nrOK;
end;

function TSChannelClient.Send(aSocket: TNetSocket; aBuffer: pointer;
  var aLength: integer): TNetResult;
var
  desc: TSecBufferDesc;
  buf: array[0..3] of TSecBuffer;
  sent, s, len, trailer, pending, templen: cardinal;
  temp: array[0..TLSRECMAXSIZE] of byte;
begin
  result := nrFatalError;
  desc.ulVersion := SECBUFFER_VERSION;
  desc.cBuffers := 4;
  desc.pBuffers := @buf[0];
  pending := aLength;
  while pending > 0 do
  begin
    templen := pending;
    if templen > Sizes.cbMaximumMessage then
      templen := Sizes.cbMaximumMessage;
    MoveFast(aBuffer^, temp[Sizes.cbHeader], templen);
    inc(PByte(aBuffer), templen);
    dec(pending, templen);
    trailer := Sizes.cbHeader + templen;
    buf[0].cbBuffer := Sizes.cbHeader;
    buf[0].BufferType := SECBUFFER_STREAM_HEADER;
    buf[0].pvBuffer := @temp;
    buf[1].cbBuffer := templen;
    buf[1].BufferType := SECBUFFER_DATA;
    buf[1].pvBuffer := @temp[Sizes.cbHeader];
    buf[2].cbBuffer := Sizes.cbTrailer;
    buf[2].BufferType := SECBUFFER_STREAM_TRAILER;
    buf[2].pvBuffer := @temp[trailer];
    buf[3].cbBuffer := 0;
    buf[3].BufferType := SECBUFFER_EMPTY;
    buf[3].pvBuffer := nil;
    if EncryptMessage(@Ctxt, 0, @desc, 0) <> SEC_E_OK then
      exit; // shutdown the connection on SChannel error
    len := buf[0].cbBuffer + buf[1].cbBuffer + buf[2].cbBuffer;
    sent := 0;
    repeat
      s := mormot.net.sock.Send(aSocket.Socket, @temp[sent], len, MSG_NOSIGNAL);
      if s = len then
        break; // whole message sent
      if s = 0 then
        exit;  // report connection closed
      if integer(s) < 0 then
      begin
        result := NetLastError;
        if result <> nrRetry then
          exit; // report socket fatal error
      end
      else
      begin
        dec(len, s);
        inc(sent, s);
      end;
      Sleep(1); // try again
    until false;
  end;
  result := nrOK;
end;


function SChannelNewNetTLS: INetTLS;
begin
  result := TSChannelClient.Create;
end;

var
  WsaDataOnce: TWSADATA;
  LibSecurHandle: {$ifdef FPC}TLibHandle{$else}HMODULE{$endif};

procedure InitializeUnit;
begin
  WSAStartup($0202, WsaDataOnce);
  SocketAPIVersion := RawUTF8(Format('%s.%d', [WsaDataOnce.szDescription,WsaDataOnce.wVersion]));
  LibSecurHandle := LoadLibrary(DLLSecur32);
  if LibSecurHandle <> 0 then
  begin
    AcquireCredentialsHandle := GetProcAddress(LibSecurHandle, 'AcquireCredentialsHandleA');
    FreeCredentialsHandle := GetProcAddress(LibSecurHandle, 'FreeCredentialsHandle');
    InitializeSecurityContext := GetProcAddress(LibSecurHandle, 'InitializeSecurityContextA');
    DeleteSecurityContext := GetProcAddress(LibSecurHandle, 'DeleteSecurityContext');
    ApplyControlToken := GetProcAddress(LibSecurHandle, 'ApplyControlToken');
    QueryContextAttributes := GetProcAddress(LibSecurHandle, 'QueryContextAttributesA');
    FreeContextBuffer := GetProcAddress(LibSecurHandle, 'FreeContextBuffer');
    EncryptMessage := GetProcAddress(LibSecurHandle, 'EncryptMessage');
    DecryptMessage := GetProcAddress(LibSecurHandle, 'DecryptMessage');
    if Assigned(AcquireCredentialsHandle) and
       Assigned(InitializeSecurityContext) and
       Assigned(QueryContextAttributes) and
       Assigned(EncryptMessage) and
       Assigned(DecryptMessage) then
     NewNetTLS := SChannelNewNetTLS;
  end;
end;

procedure FinalizeUnit;
begin
  WSACleanup;
  if LibSecurHandle <> 0 then
    FreeLibrary(LibSecurHandle);
end;

