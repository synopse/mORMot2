{
  This file is a part of the Open Source Synopse mORMot framework 2,
  licensed under a MPL/GPL/LGPL three license - see LICENSE.md

  Implements WinSock2 API calls, as used by mormot.net.sock.pas on Windows.
  Using our own unit makes code easier to maintain between Delphi/FPC versions,
  and allow to increase FD_SETSIZE for better TPollSocketSelect performance.
}

uses
  Windows,
  mormot.lib.sspi; // for Low-Level SSPI/SChannel Functions


{ ******** WinSock2 Type Definitions }

const
  ws2 = 'ws2_32.dll';
  host_file = 'c:\windows\system32\drivers\etc\hosts';

const
  // default on Windows is 64, but we grow it to 512 for TPollSocketSelect
  // as stated by Raymond Chen: "you can make FD_SET­SIZE bigger if you need to"
  // https://devblogs.microsoft.com/oldnewthing/20161221-00/?p=94985
  // -> Xitami uses 1024 since 1996
  // -> safe since our TFDSet is INTERNAL to this unit, and never published :)
  // -> not too big, since TFDSet is allocated on stack
  FD_SETSIZE = 512;

type
  /// a socket is always pointer-sized on Windows
  // - and we also use PtrInt for our TNetSocketWrap cast
  TSocket = PtrInt;

  TSockLen = integer;

  PFDSet = ^TFDSet;
  TFDSet = record
    fd_count: integer;
    fd_array: array[0 .. FD_SETSIZE - 1] of TSocket;
  end;

const
  FIONREAD     = $4004667f;
  FIONBIO      = $8004667e;
  FIOASYNC     = $8004667d;

type
  PTimeVal = ^TTimeVal;
  TTimeVal = record
    tv_sec: integer;
    tv_usec: integer;
  end;

const
  IPPROTO_IP     =   0;	   // Abstract
  IPPROTO_ICMP   =   1;	   // Internet Control Message Protocol
  IPPROTO_IGMP   =   2;	   // Internet Group Management Protocol
  IPPROTO_TCP    =   6;	   // TCP
  IPPROTO_UDP    =   17;   // User Datagram Protocol
  IPPROTO_IPV6   =   41;
  IPPROTO_ICMPV6 =   58;

const
  SIO_GET_EXTENSION_FUNCTION_POINTER = $c8000006;
  WSAID_ACCEPTEX: TGuid =
    (D1: $b5367df1; D2: $cbac; D3: $11cf;
     D4: ($95, $ca, $00, $80, $5f, $48, $a1, $92));
  WSAID_GETACCEPTEXSOCKADDRS: TGuid =
    (D1: $b5367df2; D2: $cbac; D3: $11cf;
     D4: ($95, $ca, $00, $80, $5f, $48, $a1, $92));
  WSAID_CONNECTEX: TGuid = (
    D1: $25a207b9; D2:$ddf3; D3:$4660;
    D4: ($8e, $e9, $76, $e5, $8c, $74, $06, $3e));
  {
  WSAID_TRANSMITFILE: TGuid =
    (D1: $b5367df0; D2: $cbac; D3: $11cf;
     D4: ($95, $ca, $00, $80, $5f, $48, $a1, $92));
  }

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
  ADDR_ANY	   = INADDR_ANY;
  INVALID_SOCKET   = TSocket(NOT(0));
  SOCKET_ERROR	   = -1;

const
  IP_OPTIONS          = 1;
  IP_HDRINCL          = 2;
  IP_TOS              = 3;          // set/get IP Type Of Service
  IP_TTL              = 4;          // set/get IP Time To Live
  IP_MULTICAST_IF     = 9;          // set/get IP multicast interface
  IP_MULTICAST_TTL    = 10;         // set/get IP multicast timetolive
  IP_MULTICAST_LOOP   = 11;         // set/get IP multicast loopback
  IP_ADD_MEMBERSHIP   = 12;         // add  an IP group membership
  IP_DROP_MEMBERSHIP  = 13;         // drop an IP group membership
  IP_DONTFRAGMENT     = 14;         // set/get IP Don't Fragment flag

  IP_DEFAULT_MULTICAST_TTL   = 1;   // normally limit m'casts to 1 hop
  IP_DEFAULT_MULTICAST_LOOP  = 1;   // normally hear sends if a member
  IP_MAX_MEMBERSHIPS         = 20;  // per socket; must fit in one mbuf

  SOL_SOCKET      = $ffff;          // options for socket level

  // Option flags per-socket
  SO_DEBUG        = $0001;          // turn on debugging info recording
  SO_ACCEPTCONN   = $0002;          // socket has had listen()
  SO_REUSEADDR    = $0004;          // allow local address reuse
  SO_KEEPALIVE    = $0008;          // keep connections alive
  SO_DONTROUTE    = $0010;          // just use interface addresses
  SO_BROADCAST    = $0020;          // permit sending of broadcast msgs
  SO_USELOOPBACK  = $0040;          // bypass hardware when possible
  SO_LINGER       = $0080;          // linger on close if data present
  SO_OOBINLINE    = $0100;          // leave received OOB data in line
  SO_DONTLINGER   = $ff7f;
  // Additional options
  SO_SNDBUF       = $1001;          // send buffer size
  SO_RCVBUF       = $1002;          // receive buffer size
  SO_SNDLOWAT     = $1003;          // send low-water mark
  SO_RCVLOWAT     = $1004;          // receive low-water mark
  SO_SNDTIMEO     = $1005;          // send timeout
  SO_RCVTIMEO     = $1006;          // receive timeout
  SO_ERROR        = $1007;          // get error status and clear
  SO_TYPE         = $1008;          // get socket type
  // WinSock 2 extension -- new options
  SO_GROUP_ID       = $2001;        // ID of a socket group}
  SO_GROUP_PRIORITY = $2002;        // the relative priority within a group}
  SO_MAX_MSG_SIZE   = $2003;        // maximum message size
  SO_PROTOCOL_INFOA = $2004;        // WSAPROTOCOL_INFOA structure
  SO_PROTOCOL_INFOW = $2005;        // WSAPROTOCOL_INFOW structure
  SO_PROTOCOL_INFO  = SO_PROTOCOL_INFOA;
  PVD_CONFIG        = $3001;        // configuration info for service provider
  // Option for opening sockets for synchronous access
  SO_SYNCHRONOUS_ALERT    = $10;
  SO_SYNCHRONOUS_NONALERT = $20;
  // Other NT-specific options
  SO_OPENTYPE              = $7008;
  SO_MAXDG                 = $7009;
  SO_MAXPATHDG             = $700A;
  SO_UPDATE_ACCEPT_CONTEXT = $700B;
  SO_CONNECT_TIME          = $700C;

  SOMAXCONN       = $7fffffff;

  IPV6_UNICAST_HOPS      = 8;  // ???
  IPV6_MULTICAST_IF      = 9;  // set/get IP multicast i/f
  IPV6_MULTICAST_HOPS    = 10; // set/get IP multicast ttl
  IPV6_MULTICAST_LOOP    = 11; // set/get IP multicast loopback
  IPV6_JOIN_GROUP        = 12; // add an IP group membership
  IPV6_LEAVE_GROUP       = 13; // drop an IP group membership

  // getnameinfo constants
  NI_MAXHOST	   = 1025;
  NI_MAXSERV	   = 32;
  NI_NOFQDN 	   = $1;
  NI_NUMERICHOST   = $2;
  NI_NAMEREQD	   = $4;
  NI_NUMERICSERV   = $8;
  NI_DGRAM         = $10;

const
  SOCK_STREAM     = 1;               // stream socket
  SOCK_DGRAM      = 2;               // datagram socket
  SOCK_RAW        = 3;               // raw-protocol interface
  SOCK_RDM        = 4;               // reliably-delivered message
  SOCK_SEQPACKET  = 5;               // sequenced packet stream

  // TCP options
  TCP_NODELAY     = $0001;

  // Address families
  AF_UNSPEC       = 0;               // unspecified
  AF_INET         = 2;               // internetwork: UDP, TCP, etc
  AF_INET6        = 23;              // Internetwork Version 6
  AF_MAX          = 24;

  // Protocol families, same as address families for now
  PF_UNSPEC       = AF_UNSPEC;
  PF_INET         = AF_INET;
  PF_INET6        = AF_INET6;
  PF_MAX          = AF_MAX;

type
  // Structure used by kernel to store most addresses
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
  // Structure used for manipulating the linger option
  PLinger = ^TLinger;
  TLinger = packed record
    l_onoff: word;
    l_linger: word;
  end;

const
  MSG_NOSIGNAL  = $00;  // do not generate SIGPIPE (not defined on Windows)
  MSG_OOB       = $01;  // Process out-of-band data
  MSG_PEEK      = $02;  // Peek at incoming messages

const
  // All Windows Sockets error constants are biased by WSABASEERR offset
  WSABASEERR              = 10000;

  // Windows Sockets definitions of regular Microsoft C error constants
  WSAEINTR                = WSABASEERR + 4;  // legacy flag
  WSAEBADF                = WSABASEERR + 9;
  WSAEACCES               = WSABASEERR + 13;
  WSAEFAULT               = WSABASEERR + 14;
  WSAEINVAL               = WSABASEERR + 22;
  WSAEMFILE               = WSABASEERR + 24;

  // Windows Sockets definitions of regular Berkeley error constants
  WSAEWOULDBLOCK          = WSABASEERR + 35;
  WSAEINPROGRESS          = WSABASEERR + 36;
  WSAEALREADY             = WSABASEERR + 37;
  WSAENOTSOCK             = WSABASEERR + 38;
  WSAEDESTADDRREQ         = WSABASEERR + 39;
  WSAEMSGSIZE             = WSABASEERR + 40;
  WSAEPROTOTYPE           = WSABASEERR + 41;
  WSAENOPROTOOPT          = WSABASEERR + 42;
  WSAEPROTONOSUPPORT      = WSABASEERR + 43;
  WSAESOCKTNOSUPPORT      = WSABASEERR + 44;
  WSAEOPNOTSUPP           = WSABASEERR + 45;
  WSAEPFNOSUPPORT         = WSABASEERR + 46;
  WSAEAFNOSUPPORT         = WSABASEERR + 47;
  WSAEADDRINUSE           = WSABASEERR + 48;
  WSAEADDRNOTAVAIL        = WSABASEERR + 49;
  WSAENETDOWN             = WSABASEERR + 50;
  WSAENETUNREACH          = WSABASEERR + 51;
  WSAENETRESET            = WSABASEERR + 52;
  WSAECONNABORTED         = WSABASEERR + 53;
  WSAECONNRESET           = WSABASEERR + 54;
  WSAENOBUFS              = WSABASEERR + 55;
  WSAEISCONN              = WSABASEERR + 56;
  WSAENOTCONN             = WSABASEERR + 57;
  WSAESHUTDOWN            = WSABASEERR + 58;
  WSAETOOMANYREFS         = WSABASEERR + 59;
  WSAETIMEDOUT            = WSABASEERR + 60;
  WSAECONNREFUSED         = WSABASEERR + 61;
  WSAELOOP                = WSABASEERR + 62;
  WSAENAMETOOLONG         = WSABASEERR + 63;
  WSAEHOSTDOWN            = WSABASEERR + 64;
  WSAEHOSTUNREACH         = WSABASEERR + 65;
  WSAENOTEMPTY            = WSABASEERR + 66;
  WSAEPROCLIM             = WSABASEERR + 67;
  WSAEUSERS               = WSABASEERR + 68;
  WSAEDQUOT               = WSABASEERR + 69;
  WSAESTALE               = WSABASEERR + 70;
  WSAEREMOTE              = WSABASEERR + 71;

  // Extended Windows Sockets error constant definitions
  WSASYSNOTREADY          = WSABASEERR + 91;
  WSAVERNOTSUPPORTED      = WSABASEERR + 92;
  WSANOTINITIALISED       = WSABASEERR + 93;
  WSAEDISCON              = WSABASEERR + 101;
  WSAENOMORE              = WSABASEERR + 102;
  WSAECANCELLED           = WSABASEERR + 103;
  WSAEEINVALIDPROCTABLE   = WSABASEERR + 104;
  WSAEINVALIDPROVIDER     = WSABASEERR + 105;
  WSAEPROVIDERFAILEDINIT  = WSABASEERR + 106;
  WSASYSCALLFAILURE       = WSABASEERR + 107;
  WSASERVICE_NOT_FOUND    = WSABASEERR + 108;
  WSATYPE_NOT_FOUND       = WSABASEERR + 109;
  WSA_E_NO_MORE           = WSABASEERR + 110;
  WSA_E_CANCELLED         = WSABASEERR + 111;
  WSAEREFUSED             = WSABASEERR + 112;

  // Error return codes from gethostbyname and gethostbyaddr
  // Authoritative Answer: Host not found
  WSAHOST_NOT_FOUND       = WSABASEERR + 1001;
  // Non-Authoritative: Host not found, or SERVERFAIL
  WSATRY_AGAIN            = WSABASEERR + 1002;
  // Non recoverable errors, FORMERR, REFUSED, NOTIMP
  WSANO_RECOVERY          = WSABASEERR + 1003;
  // Valid name, no data record of requested type
  WSANO_DATA              = WSABASEERR + 1004;
  // no address, look for MX record
  WSANO_ADDRESS           = WSANO_DATA;

  // WinSock 2 extension
  WSAIOPENDING        = ERROR_IO_PENDING;
  WSAIOINCOMPLETE     = ERROR_IO_INCOMPLETE;
  WSAINVALIDHANDLE    = ERROR_INVALID_HANDLE;
  WSAINVALIDPARAMETER = ERROR_INVALID_PARAMETER;
  WSANOTENOUGHMEMORY  = ERROR_NOT_ENOUGH_MEMORY;
  WSAOPERATIONABORTED = ERROR_OPERATION_ABORTED;

const
  WSADESCRIPTION_LEN     =   256;
  WSASYS_STATUS_LEN      =   128;

  SHUT_RD   = 0;
  SHUT_WR   = 1;
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
    szDescription:  array[0..WSADESCRIPTION_LEN] of AnsiChar;
    szSystemStatus: array[0..WSASYS_STATUS_LEN] of AnsiChar;
    {$else}
    szDescription:  array[0..WSADESCRIPTION_LEN] of AnsiChar;
    szSystemStatus: array[0..WSASYS_STATUS_LEN] of AnsiChar;
    iMaxSockets: word;
    iMaxUdpDg: word;
    lpVendorInfo: PAnsiChar;
    {$endif WIN64}
  end;
  WSAData = TWSAData;

  PTransmitFileBuffers = ^TTransmitFileBuffers;
  TTransmitFileBuffers = record
    Head: pointer;
    HeadLength: cardinal;
    Tail: pointer;
    TailLength: cardinal;
 end;

const
  _ST: array[TNetLayer] of integer = (
    SOCK_STREAM,
    SOCK_DGRAM,
    0);
  _IP: array[TNetLayer] of integer = (
    IPPROTO_TCP,
    IPPROTO_UDP,
    0);
  _SF: array[TNetFamily] of integer = (
    0,
    AF_INET,
    AF_INET6,
    0);

function htons(port: cardinal): cardinal; {$ifdef HASINLINE} inline; {$endif}
begin
  result := swap(word(port)); // port should be 16-bit word
end;


{ ******** WinSock2 API calls }

function WSAStartup(wVersionRequired: word; var WSData: TWSAData): integer; stdcall;
  external ws2 name 'WSAStartup';

function WSACleanup: integer; stdcall;
  external ws2 name 'WSACleanup';

function RawSocketErrNo: integer; stdcall;
  external ws2 name 'WSAGetLastError';

function getaddrinfo(NodeName, ServName: PAnsiChar; Hints: PAddrInfo;
   var Addrinfo: PAddrInfo): integer; stdcall;
  external ws2 name 'getaddrinfo';

procedure freeaddrinfo(ai: PAddrInfo); stdcall;
  external ws2 name 'freeaddrinfo';

function socket(af, struct, protocol: integer): TSocket; stdcall;
  external ws2 name 'socket';

function setsockopt(s: TSocket; level, optname: integer;
   optval: pointer; optlen: integer): integer; stdcall;
  external ws2 name 'setsockopt';

function getsockopt(s: TSocket; level, optname: integer;
   optval: pointer; optlen: PInteger): integer; stdcall;
  external ws2 name 'getsockopt';

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

function recv(s: TSocket; Buf: pointer; len, flags: integer): integer; stdcall;
  external ws2 name 'recv';

type
  TWsaBuf = record
    cLength: cardinal;
    pBuffer: pointer;
  end;
  PWsaBuf = ^TWsaBuf;

function WSAConnect(s: TSocket; name: PSockAddr; namelen: integer;
   caller, callee, sqos, gqos: pointer): integer; stdcall;
  external ws2 name 'WSAConnect';

function WSARecv(s: TSocket; var lpBuffers: TWsaBuf; dwBufferCount: cardinal;
   var lpNumberOfBytesRecvd: cardinal; var lpFlags: cardinal;
   lpOverlapped: POverlapped; lpCompletionRoutine: pointer = nil): integer; stdcall;
  external ws2 name 'WSARecv';

function WSASend(s: TSocket; var lpBuffers: TWsaBuf; dwBufferCount: cardinal;
   var lpNumberOfBytesRecvd: cardinal; dwFlags: cardinal;
   lpOverlapped: POverlapped; lpCompletionRoutine: pointer = nil): integer; stdcall;
  external ws2 name 'WSASend';

function WSAIoctl(s: TSocket; dwIoControlCode: cardinal; lpvInBuffer: pointer;
   cbInBuffer: cardinal; lpvOutBuffer: pointer; cbOutBuffer: cardinal;
   lpcbBytesReturned: PCardinal; lpOverlapped: POverlapped;
   lpCompletionRoutine: pointer): integer; stdcall;
  external ws2 name 'WSAIoctl';

function recvfrom(s: TSocket; Buf: pointer; len, flags: integer;
   from: PSockAddr; fromlen: Pinteger): integer; stdcall;
  external ws2 name 'recvfrom';

function send(s: TSocket; Buf: pointer; len, flags: integer): integer; stdcall;
  external ws2 name 'send';

function sendto(s: TSocket; Buf: pointer; len, flags: integer;
   addrto: PSockAddr; tolen: integer): integer; stdcall;
  external ws2 name 'sendto';

function getsockname(s: TSocket; name: PSockAddr; var namelen: integer): integer; stdcall;
  external ws2 name 'getsockname';

function getpeername(s: TSocket; name: PSockAddr; var namelen: integer): integer; stdcall;
  external ws2 name 'getpeername';

function doaccept(s: TSocket; addr: PSockAddr; var async: boolean): TSocket;
var
  len: integer;
begin
  len := SOCKADDR_SIZE;
  result  := accept(s, addr, len);
  // no accept4() on Windows -> async will be done later
end;

var
  // some extended WinSock functions resolved in InitializeUnit below
  acceptex: function(ListenSocket, AcceptSocket: TSocket; pOutBuff: pointer;
    ReceiveDataLen, LocalAddrLen, RemoteAddrLen: DWORD; var BytesReceived: DWORD;
    pOverlapped: POverlapped): BOOL; stdcall;
  getacceptexsockaddrs: procedure(lpOutputBuffer: pointer;
    dwReceiveDataLength, dwLocalAddressLength, dwRemoteAddressLength: DWORD;
    var LocalSockaddr: PSockAddr; var LocalSockaddrLength: integer;
    var RemoteSockaddr: PSockAddr; var RemoteSockaddrLength: integer); stdcall;
  connectex: function(Sock: TSocket; Name: PSockAddr; NameLen: integer;
    SendBuf: pointer; SendLen: DWORD; var BytesSent: DWORD;
    Overlapped: POverlapped): BOOL; stdcall;
  {
  transmitfile: function(s: TSocket; hFile: THandle;
    NumberOfBytesToWrite, NumberOfBytesPerSend: DWORD; pOvp: POverlapped;
    pTransmitBuffers: PTransmitFileBuffers; dwFlags: DWORD): BOOL; stdcall;
  }


{ ******** TNetSocket Cross-Platform Wrapper }

{ TNetAddr }

function SetWinAddr(const host, port: RawUtf8; family: cardinal; layer: TNetLayer;
  out addr): boolean;
var
  h: TAddrInfo;
  res: PAddrInfo;
begin
  // use regular OS resolution for known addresses or host names
  FillCharFast(h, SizeOf(h), 0);
  h.ai_family := family;
  h.ai_socktype := _ST[layer];
  h.ai_protocol := _IP[layer];
  res := nil;
  result := getaddrinfo(pointer(host), pointer(port), @h, res) = NO_ERROR;
  if result then
    MoveFast(res^.ai_addr^, addr, res^.ai_addrlen); // return the first result
  if res <> nil then
    freeaddrinfo(res);
end;

function TNetAddr.SetFrom(const address, addrport: RawUtf8;
  layer: TNetLayer): TNetResult;
var
  p: cardinal;
begin
  FillCharFast(Addr, SizeOf(Addr), 0);
  result := nrNotImplemented;
  if layer = nlUnix then
    exit;
  result := nrNotFound;
  if address = '' then
    exit;
  // check supplied IP port
  p := GetCardinal(pointer(addrport));
  if (p = 0) and
     (addrport <> '0') then // explicit '0' to get ephemeral port
    exit;
  PSockAddr(@Addr)^.sin_port := htons(p);
  // first check most simple IPv4 resolution (maybe using mormot.net.dns)
  if SetFromIP4(address, false) or
     // fallback to OS getaddrinfo() resolution, first try as IP4, then IP6
     SetWinAddr(address, addrport, AF_INET, layer, Addr) or
     SetWinAddr(address, addrport, AF_INET6, layer, Addr) then
    result := nrOk;
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

procedure TNetSocketWrap.SetReuseAddrPort;
begin
  // do nothing on Windows
end;

procedure TNetSocketWrap.ReusePort;
var
  v: integer;
begin
  v := ord(true);
  SetOpt(SOL_SOCKET, SO_REUSEADDR, @v, SizeOf(v));
end;

procedure TNetSocketWrap.SetLinger(linger: integer);
var
  v: TLinger;
begin
  v.l_linger := linger;
  v.l_onoff := 0; // don't wait - ord(linger >= 0) would wait
  // https://docs.microsoft.com/en-us/windows/win32/api/winsock/nf-winsock-closesocket
  SetOpt(SOL_SOCKET, SO_LINGER, @v, SizeOf(v));
end;

procedure TNetSocketWrap.SetCork(cork: boolean);
begin
  // do nothing on Windows
end;

const
  // https://techcommunity.microsoft.com/t5/networking-blog/ledbat-background-data-transfer-for-windows/ba-p/3639278
  SIO_PRIORITY_HINT = integer($98000018); // _WSAIOW(IOC_VENDOR,$18)
  SocketPriorityHintVeryLow = 0;
  SocketPriorityHintLow = 1;
  SocketPriorityHintNormal = 2;
  SocketMaximumPriorityHintType = 3;

procedure TNetSocketWrap.SetLowPriority;
var
  v: integer;
begin
  v := SocketPriorityHintVeryLow;
  setsockopt(TSocket(@self), SOL_SOCKET, SIO_PRIORITY_HINT, @v, SizeOf(v));
  // don't call SetOpt() which raise ENetSock on oldest Windows
end;

function TNetSocketWrap.HasLowPriority: boolean;
var
  v, l: integer;
begin
  v := -1;
  l := SizeOf(v);
  result :=
    (getsockopt(TSocket(@self), SOL_SOCKET, SIO_PRIORITY_HINT, @v, @l) = NO_ERROR) and
    (v = SocketPriorityHintVeryLow);
end;

procedure SetTimeVal(ms: cardinal; out tv: TTimeVal);
  {$ifdef HASINLINE} inline; {$endif}
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

function TNetSocketWrap.WaitFor(ms: integer; scope: TNetEvents;
  loerr: system.PInteger): TNetEvents;
var
  sock: TSocket;
  rd, wr, er: record
    fd_count: integer;
    fd_array: array[0..0] of TSocket; // no need of the 512 entries of TFDSet
  end;
  rdp, wrp, erp: PFDSet;
  tv: TTimeVal;
  ptv: pointer;
  res: integer;
  pending: cardinal;
begin
  if loerr <> nil then
    loerr^ := 0;
  result := [neError];
  if @self = nil then
    exit;
  rdp := nil;
  wrp := nil;
  erp := nil;
  ptv := nil; // infinite timeout
  sock := TSocket(@self);
  if neWrite in scope then
  begin
    wr.fd_array[0] := sock;
    wr.fd_count := 1;
    wrp := @wr;
  end;
  if neError in scope then
  begin
    er.fd_array[0] := sock;
    er.fd_count := 1;
    erp := @er;
  end;
  if (neRead in scope) or
     ((wrp = nil) and
      (erp = nil)) then // select() needs at least one descriptor
  begin
    rd.fd_array[0] := sock;
    rd.fd_count := 1;
    rdp := @rd;
  end;
  if ms >= 0 then
  begin
    SetTimeVal(ms, tv);
    ptv := @tv;
  end;
  res := select({ignored=}0, rdp, wrp, erp, ptv);
  if res < 0 then
  begin
    if loerr <> nil then
      loerr^ := RawSocketErrNo;
    exit; // returning [neError]
  end;
  result := [];
  if res = 0 then
    // nothing new detected
    exit;
  if (rdp <> nil) and
     ({%H-}rd.fd_count = 1) and
     (rd.fd_array[0] = sock) then
    if ioctlsocket(sock, FIONREAD, @pending) <> NO_ERROR then
      include(result, neError)
    else 
      // FIONREAD is not enough on Windows for graceful disconnection
      // -> recv() should be called: https://stackoverflow.com/a/1586817/458259
      // -> return neRead and not neClosed if pending=0
      include(result, neRead);
  if (wrp <> nil) and
     ({%H-}wr.fd_count = 1) and
     (wr.fd_array[0] = sock) then
    include(result, neWrite);
  if (erp <> nil) and
     ({%H-}er.fd_count = 1) and
     (er.fd_array[0] = sock) then
    include(result, neError);
end;


{ ******************** IP Addresses Support }

type
  PMIB_IPADDRTABLE = ^MIB_IPADDRTABLE;
  MIB_IPADDRTABLE = record
    dwNumEntries: DWORD;
    ip: array[byte] of record
      dwAddr: DWORD;
      dwIndex: DWORD;
      dwMask: DWORD;
      dwBCastAddr: DWORD;
      dwReasmSize: DWORD;
      unused1: Word;
      wType: Word;
    end;
  end;

var
  iphlpapiDll: THandle;

  // some iphlpapi.dll late-binded API via DelayedProc()
  GetIpAddrTable: function (pIpAddrTable: PMIB_IPADDRTABLE;
    var pdwSize: DWORD; bOrder: BOOL): DWORD; stdcall;

function GetIPAddresses(Kind: TIPAddress): TRawUtf8DynArray;
var
  Table: MIB_IPADDRTABLE;
  Size: DWORD;
  i: PtrInt;
  n: PtrUInt;
begin
  result := nil;
  if Kind = tiaIPv6 then
    exit; // GetIpAddrTable() is an IPv4 only API
  Size := SizeOf(Table);
  if not DelayedProc(GetIpAddrTable, iphlpapiDll, 'iphlpapi.dll',
           'GetIpAddrTable') or
     (GetIpAddrTable(@Table, Size, false) <> NO_ERROR) then
    exit;
  SetLength(result, Table.dwNumEntries);
  n := 0;
  for i := 0 to Table.dwNumEntries-1 do
    with Table.ip[i] do
      if IP4Filter(dwAddr, Kind) then
      begin
        IP4Text(@dwAddr, result[n]);
        inc(n);
      end;
  if n <> Table.dwNumEntries then
    SetLength(result, n);
end;



{ ******************** MAC and DNS Addresses Support }

const
  GAA_FLAG_SKIP_UNICAST       = $1;
  GAA_FLAG_SKIP_ANYCAST       = $2;
  GAA_FLAG_SKIP_MULTICAST     = $4;
  GAA_FLAG_SKIP_DNS_SERVER    = $8;
  GAA_FLAG_INCLUDE_PREFIX     = $10;
  GAA_FLAG_SKIP_FRIENDLY_NAME = $20;
  // below flags are only available on Windows Vista and later
  GAA_FLAG_INCLUDE_WINS_INFO  = $40;
  GAA_FLAG_INCLUDE_GATEWAYS   = $80;
  GAA_FLAG_INCLUDE_ALL_INTERFACES = $100;

  IfOperStatusUp = 1;
  IfOperStatusDown = 2;

  IF_TYPE_ETHERNET_CSMACD   = 6;
  IF_TYPE_PPP               = 23;
  IF_TYPE_SOFTWARE_LOOPBACK = 24;
  IF_TYPE_IEEE80211         = 71; // Vista+
  IF_TYPE_TUNNEL            = 131;

  MAX_HOSTNAME_LEN        = 128;
  MAX_DOMAIN_NAME_LEN     = 128;
  MAX_SCOPE_ID_LEN        = 256;
  MAX_ADAPTER_ADDRESS_LEN = 8;

type
  SOCKET_ADDRESS = record
    lpSockaddr: PSOCKADDR;
    iSockaddrLength: integer;
  end;

  IP_PREFIX_ORIGIN = DWORD;
  IP_SUFFIX_ORIGIN = DWORD;
  IP_DAD_STATE = DWORD;

  IP_ADAPTER_UNION = record
    case integer of
      0: (
        Alignment: QWord);
      1: (
        Length: ULONG;
        FlagsOrIfIndex: DWORD);
  end;

  PIP_ADAPTER_UNICAST_ADDRESS = ^IP_ADAPTER_UNICAST_ADDRESS;
  IP_ADAPTER_UNICAST_ADDRESS = record
    Union: IP_ADAPTER_UNION;
    Next: PIP_ADAPTER_UNICAST_ADDRESS;
    Address: SOCKET_ADDRESS;
    PrefixOrigin: IP_PREFIX_ORIGIN;
    SuffixOrigin: IP_SUFFIX_ORIGIN;
    DadState: IP_DAD_STATE;
    ValidLifetime: ULONG;
    PreferredLifetime: ULONG;
    LeaseLifetime: ULONG;
    // below fields are only available on Windows Vista and later
    OnLinkPrefixLength: Byte;
  end;

  PIP_ADAPTER = ^IP_ADAPTER;
  // used for IP_ADAPTER_PREFIX, IP_ADAPTER_DNS_SERVER_ADDRESS,
  // IP_ADAPTER_GATEWAY_ADDRESS and IP_ADAPTER_WINS_SERVER_ADDRESS
  IP_ADAPTER = record
    Union: IP_ADAPTER_UNION;
    Next: PIP_ADAPTER;
    Address: SOCKET_ADDRESS;
    PrefixLength: ULONG; // used for IP_ADAPTER_PREFIX only
  end;

  PIP_ADAPTER_PREFIX = PIP_ADAPTER;
  PIP_ADAPTER_ANYCAST_ADDRESS = PIP_ADAPTER;
  PIP_ADAPTER_DNS_SERVER_ADDRESS = PIP_ADAPTER;
  PIP_ADAPTER_MULTICAST_ADDRESS = PIP_ADAPTER;
  PIP_ADAPTER_WINS_SERVER_ADDRESS = PIP_ADAPTER;
  PIP_ADAPTER_GATEWAY_ADDRESS = PIP_ADAPTER;

  PIP_ADAPTER_ADDRESSES = ^IP_ADAPTER_ADDRESSES;
  IP_ADAPTER_ADDRESSES = record
    Union: IP_ADAPTER_UNION;
    Next: PIP_ADAPTER_ADDRESSES;
    AdapterName: PAnsiChar;
    FirstUnicastAddress: PIP_ADAPTER_UNICAST_ADDRESS;
    FirstAnycastAddress: PIP_ADAPTER_ANYCAST_ADDRESS;
    FirstMulticastAddress: PIP_ADAPTER_MULTICAST_ADDRESS;
    FirstDnsServerAddress: PIP_ADAPTER_DNS_SERVER_ADDRESS;
    DnsSuffix: PWCHAR;
    Description: PWCHAR;
    FriendlyName: PWCHAR;
    PhysicalAddress: array [0 .. MAX_ADAPTER_ADDRESS_LEN - 1] of BYTE;
    PhysicalAddressLength: DWORD;
    Flags: DWORD;
    Mtu: DWORD;
    IfType: ULONG;
    OperStatus: DWORD;
    // below fields are only available on Windows XP with SP1 and later
    Ipv6IfIndex: DWORD;
    ZoneIndices: array [0..15] of DWORD;
    FirstPrefix: PIP_ADAPTER_PREFIX;
    // below fields are only available on Windows Vista and later
    TransmitLinkSpeed: Int64;
    ReceiveLinkSpeed: Int64;
    FirstWinsServerAddress: PIP_ADAPTER_WINS_SERVER_ADDRESS;
    FirstGatewayAddress: PIP_ADAPTER_GATEWAY_ADDRESS;
    Ipv4Metric: ULONG;
    Ipv6Metric: ULONG;
    Luid: Int64;
    Dhcpv4Server: SOCKET_ADDRESS;
    CompartmentId: DWORD;
    NetworkGuid: TGuid;
    ConnectionType: DWORD;
    TunnelType: DWORD;
    // DHCP v6 Info following (not used by our code)
  end;

  IP_ADDRESS_STRING = array [0..15] of AnsiChar;
  IP_ADDR_IP4 = cardinal;

  IP_ADDR_STRING = packed record
    IpAddress: IP_ADDRESS_STRING;
    IpMask: IP_ADDRESS_STRING;
    Context: IP_ADDR_IP4;
  end;
  PIP_ADDR_STRING = ^IP_ADDR_STRING;

  PIP_ADDR_STRING_LINKED_LIST = ^IP_ADDR_STRING_LINKED_LIST;
  IP_ADDR_STRING_LINKED_LIST = packed record
    Next: PIP_ADDR_STRING_LINKED_LIST;
    Current: IP_ADDR_STRING;
  end;

  FIXED_INFO = packed record
    HostName: array[0..MAX_HOSTNAME_LEN + 3] of AnsiChar;
    DomainName: array[0..MAX_DOMAIN_NAME_LEN + 3] of AnsiChar;
    CurrentDnsServer: PIP_ADDR_STRING;
    DnsServerList: IP_ADDR_STRING_LINKED_LIST;
    NodeType: DWORD;
    ScopeId: array[0 .. MAX_SCOPE_ID_LEN + 3] of AnsiChar;
    EnableRouting: DWORD;
    EnableProxy: DWORD;
    EnableDns: DWORD;
  end;
  PFIXED_INFO = ^FIXED_INFO;

var
  // some iphlpapi.dll late-binded API via DelayedProc()
  GetAdaptersAddresses: function(Family: ULONG; Flags: DWORD; Reserved: pointer;
      pAdapterAddresses: PIP_ADAPTER_ADDRESSES; pOutBufLen: PULONG): DWORD; stdcall;
  SendARP: function(DestIp: DWORD; srcIP: DWORD; pMacAddr: pointer;
    PhyAddrLen: pointer): DWORD; stdcall;
  GetNetworkParams: function(pFixedInfo: PFIXED_INFO;
    pOutBufLen: PULONG): DWORD; stdcall;
  GetBestInterface: function(dwDestAddrIPv4: cardinal;
    var pdwBestIfIndex: DWORD): DWORD; stdcall;

function _GetDnsAddresses(usePosixEnv, getAD: boolean): TRawUtf8DynArray;
var
  n, l: PtrInt;
  siz: ULONG;
  p: PIP_ADDR_STRING_LINKED_LIST;
  tmp: RawByteString;
begin
  result := nil;
  siz := 0;
  if not DelayedProc(GetNetworkParams, iphlpapiDll, 'iphlpapi.dll',
           'GetNetworkParams') or
     (GetNetworkParams(nil, @siz) <> ERROR_BUFFER_OVERFLOW) then
    exit;
  SetLength(tmp, siz);
  if GetNetworkParams(pointer(tmp), @siz) <> ERROR_SUCCESS then
    exit;
  if getAD then
  begin
    l := StrLen(@PFIXED_INFO(tmp)^.DomainName);
    if l <> 0 then
    begin
      SetLength(result, 1);
      FastSetString(result[0], @PFIXED_INFO(tmp)^.DomainName, l);
    end;
    exit;
  end;
  n := 0;
  p := @PFIXED_INFO(tmp)^.DnsServerList;
  repeat
    l := StrLen(@p^.Current.IpAddress);
    if l <> 0 then
    begin
      SetLength(result, n + 1);
      FastSetString(result[n], @p^.Current.IpAddress, l);
      inc(n);
    end;
    p := p^.Next;
  until p = nil;
end;

function RetrieveMacAddresses(UpAndDown: boolean): TMacAddressDynArray;
var
  n: PtrInt;
  siz, b, flags: ULONG;
  p: PIP_ADAPTER_ADDRESSES;
  a: PIP_ADAPTER_UNICAST_ADDRESS;
  r: PIP_ADAPTER;
  tmp: RawByteString;
  one: RawUtf8;
begin
  // see https://github.com/zeromq/czmq/blob/899f81985961513c/src/ziflist.c#L340
  result := nil;
  siz := 0;
  flags := GAA_FLAG_SKIP_ANYCAST or
           GAA_FLAG_SKIP_MULTICAST or
           GAA_FLAG_INCLUDE_PREFIX;
  if OsVersion >= wVista then
    flags := flags or GAA_FLAG_INCLUDE_GATEWAYS;
  if not DelayedProc(GetAdaptersAddresses, iphlpapiDll, 'iphlpapi.dll',
           'GetAdaptersAddresses') or
     (GetAdaptersAddresses(AF_INET, flags, nil, nil, @siz) <> ERROR_BUFFER_OVERFLOW) or
     (siz = 0) then
    exit;
  SetLength(tmp, siz); // around 600 bytes per interface
  p := pointer(tmp);
  if GetAdaptersAddresses(AF_INET, flags, nil, p, @siz) <> ERROR_SUCCESS then
    exit;
  n := 0;
  repeat
    if (p^.Flags <> 0) and
       (p^.IfType <> IF_TYPE_SOFTWARE_LOOPBACK) and
       (p^.OperStatus in [IfOperStatusUp, IfOperStatusDown]) and
       (UpAndDown or
        (p^.OperStatus = IfOperStatusUp)) and
       (p^.PhysicalAddressLength = 6) then
    begin
      SetLength(result, n + 1);
      with result[n] do
      begin
        Win32PWideCharToUtf8(p^.Description, Name);
        Win32PWideCharToUtf8(p^.FriendlyName, FriendlyName);
        FastSetString(AdapterName, p^.AdapterName, StrLen(p^.AdapterName));
        Win32PWideCharToUtf8(p^.DnsSuffix, DnsSuffix);
        Address := MacToText(@p^.PhysicalAddress);
        Mtu := p^.Mtu;
        IfIndex := p^.Union.FlagsOrIfIndex;
        case p^.IfType of
          IF_TYPE_ETHERNET_CSMACD:
            Kind := makEthernet;
          IF_TYPE_IEEE80211:
            Kind := makWifi;
          IF_TYPE_TUNNEL:
            Kind := makTunnel;
          IF_TYPE_PPP:
            Kind := makPpp;
        end;
        a := p^.FirstUnicastAddress;
        while a <> nil do
        begin
          case a^.Address.lpSockaddr^.sa_family of
            AF_INET: // retrieve first IPv4 address and masks
              begin
                IP4Text(@a^.Address.lpSockaddr^.sin_addr, IP);
                if OsVersion >= wVista then
                  b := a^.OnLinkPrefixLength
                else if (p^.FirstPrefix <> nil) and
                        (p^.FirstPrefix^.Address.lpSockaddr^.sin_family = AF_INET) and
                        (p^.FirstPrefix^.PrefixLength <= 32) then
                        b := p^.FirstPrefix^.PrefixLength
                      else
                        b := 255; // fallback to IP4Mask()
                if b <= 32 then
                  b := IP4Netmask(b)
                else
                  b := IP4Mask(a^.Address.lpSockaddr^.sin_addr.S_addr);
                IP4Text(@b, NetMask);
                b := a^.Address.lpSockaddr^.sin_addr.S_addr or (not b);
                IP4Text(@b, Broadcast);
                break; // we found the first IPv4 network information
              end;
            AF_INET6: // IPv6 address but no mask - happen
              IP6Text(@PSockAddrIn6(a^.Address.lpSockaddr)^.sin6_addr, IP);
          end;
          a := a^.Next; // IPv4 may be the second
        end;
        r := p^.FirstDnsServerAddress;
        while r <> nil do
        begin
          if r.Address.lpSockaddr^.sa_family = AF_INET then
          begin // retrieve all IPv4 DNS addresses as CSV
            IP4Text(@r^.Address.lpSockaddr^.sin_addr, one);
            if Dns = '' then
              Dns := one
            else
              Dns := NetConcat([Dns, ',', one]);
          end;
          r := r^.Next;
        end;
        if OsVersion >= wVista then
        begin
          // some fields are not available on XP
          r := p^.FirstGatewayAddress;
          while r <> nil do
            if r.Address.lpSockaddr^.sa_family = AF_INET then
            begin
              IP4Text(@r^.Address.lpSockaddr^.sin_addr, Gateway);
              break;
            end
            else
              r := r^.Next;
          Speed := p^.TransmitLinkSpeed div 1000; // stored as Mbits/s
          if p^.ReceiveLinkSpeed < Speed then
            Speed := p^.ReceiveLinkSpeed; // unlikely
          if p^.Dhcpv4Server.lpSockaddr <> nil then
            IP4Text(@p^.Dhcpv4Server.lpSockaddr^.sin_addr, Dhcp);
        end;
      end;
      inc(n);
    end;
    p := p^.Next;
  until p = nil;
end;

function GetLocalMacAddress(const Remote: RawUtf8;
  var Mac: TMacAddress): boolean;
var
  addr: TNetAddr;
  dwRemoteIP, ifIndex: DWORD;
  all: TMacAddressDynArray;
  i: PtrInt;
begin
  result := false;
  if not addr.SetFromIP4(Remote, {nolookup=}false) then
    exit;
  dwRemoteIP := addr.IP4;
  if (dwRemoteIP <> 0) and
     DelayedProc(GetBestInterface, iphlpapiDll,
       'iphlpapi.dll', 'GetBestInterface') and
     (GetBestInterface(dwRemoteIP, ifIndex) = NO_ERROR) then
  begin
    all := GetMacAddresses;
    for i := 0 to high(all) do
      if DWORD(all[i].IfIndex) = ifIndex then
      begin
        Mac := all[i];
        result := true;
        exit;
      end;
  end;
end;

function GetRemoteMacAddress(const IP: RawUtf8): RawUtf8;
// implements http://msdn.microsoft.com/en-us/library/aa366358
var
  dwRemoteIP: DWORD;
  PhyAddrLen: Longword;
  pMacAddr: array [0..7] of byte;
begin
  result := '';
  if NetIsIP4(pointer(IP), @dwRemoteIP) and
     DelayedProc(SendARP, iphlpapiDll, 'iphlpapi.dll', 'SendARP') then
  begin
    PhyAddrLen := 8;
    if SendARP(dwremoteIP, 0, @pMacAddr, @PhyAddrLen) = NO_ERROR then
      if PhyAddrLen = 6 then
        result := MacToText(@pMacAddr);
  end;
end;



{ ******************** Efficient Multiple Sockets Polling }

function FD_ISSET(Socket: TNetSocket; const FDSet: TFDSet): boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
  result := PtrUIntScanExists(@FDSet.fd_array, FDSet.fd_count, PtrUInt(Socket));
end;

procedure FD_CLR(Socket: TNetSocket; var FDSet: TFDSet);
var
  i, n: PtrInt;
begin
  i := PtrUIntScanIndex(@FDSet.fd_array, FDSet.fd_count, PtrUInt(Socket));
  if i < 0 then
    exit;
  dec(FDSet.fd_count);
  n := FDSet.fd_count - i;
  if n > 0 then
    MoveFast(FDSet.fd_array[i + 1], FDSet.fd_array[i], n * SizeOf(TSocket));
end;

procedure FD_SET(Socket: TNetSocket; var FDSet: TFDSet);
  {$ifdef HASINLINE}inline;{$endif}
begin
  // caller should ensure that the Socket is not already part of fd_array[]
  FDSet.fd_array[FDSet.fd_count] := TSocket(Socket);
  inc(FDSet.fd_count);
end;

type
  /// socket polling via Windows' Select() API
  // - under Windows, Select() handles up to 64 TSocket by default, but we
  // grow FD_SETSIZE up to 512 for the purpose of this class
  // - under POSIX, select() is very limited, so poll/epoll APIs are to be used
  // - in practice, TPollSocketSelect is FASTER than TPollSocketPoll + WSAPoll()
  // because the new WSAPoll API is just an emulator using select() internally
  TPollSocketSelect = class(TPollSocketAbstract)
  protected
    fTagSocket: array of TNetSocket;
    fTagTag: array of TPollSocketTag;
    // reference values prepared for Select() call
    fRead: TFDSet;
    fWrite: TFDSet;
  public
    constructor Create(aOwner: TPollSockets); override;
    function Subscribe(socket: TNetSocket; events: TPollSocketEvents;
      tag: TPollSocketTag): boolean; override;
    function Unsubscribe(socket: TNetSocket): boolean; override;
    function WaitForModified(var results: TPollSocketResults;
      timeoutMS: integer): boolean; override;
  end;

constructor TPollSocketSelect.Create(aOwner: TPollSockets);
begin
  inherited Create(aOwner);
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
    FD_SET(socket, fRead); // if not in fTagSocket[] -> not in fRead/fWrite
  if pseWrite in events then
    FD_SET(socket, fWrite);
  fTagSocket[fCount] := socket;
  fTagTag[fCount] := tag;
  inc(fCount);
  result := true;
end;

function TPollSocketSelect.Unsubscribe(socket: TNetSocket): boolean;
var
  i, n: PtrInt;
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
    n := fCount - i;
    if n > 0 then
    begin
      MoveFast(fTagSocket[i + 1], fTagSocket[i], n * SizeOf(fTagSocket[i]));
      MoveFast(fTagTag[i + 1], fTagTag[i], n * SizeOf(fTagTag[i]));
    end;
    result := true;
  end;
end;

function TPollSocketSelect.WaitForModified(var results: TPollSocketResults;
  timeoutMS: integer): boolean;
var
  tv: TTimeVal;
  rdp, wrp: PFDSet;
  ev: TPollSocketEvents;
  i, n: PtrInt;
  sock: TNetSocket;
  rd, wr: TFDSet; // modified by select() -> local copy on stack
begin
  result := false; // error
  results.Count := 0;
  if (self = nil) or
     (fCount = 0) then
    exit;
  n := fRead.fd_count; // fill local rd before select()
  if n > 0 then
  begin
    rd.fd_count := n;
    MoveFast(fRead.fd_array, rd.fd_array, n * SizeOf(rd.fd_array[0]));
    rdp := @rd;
  end
  else
    rdp := nil;
  n := fWrite.fd_count; // fill local wr
  if n > 0 then
  begin
    wr.fd_count := n;
    MoveFast(fWrite.fd_array, wr.fd_array, n * SizeOf(wr.fd_array[0]));
    wrp := @wr;
  end
  else
    wrp := nil;
  SetTimeVal(timeoutMS, tv);
  if select({ignored=}0, rdp, wrp, nil, @tv) <= 0 then
    exit;
  if length(results.Events) < fCount then
    SetLength(results.Events, FD_SETSIZE);
  for i := 0 to fCount - 1 do
  begin
    byte(ev) := 0;
    sock := fTagSocket[i];
    if (rdp <> nil) and
      FD_ISSET(sock, rd{%H-}) then
  (*
    // on heavily multi-threaded process, select() may be triggerred but
    // currently reading in another thread, so here IoctlSocket() would return
    // pending=0 whereas the socket is actually NOT closed (seen on Windows 7)
    // -> return pseRead now and let a single thread check for pending bytes and
    // detect gracefully closed sockets e.g. in TPollAsyncSockets.ProcessRead
    if (IoctlSocket(TSocket(sock), FIONREAD, @pending) = NO_ERROR) and
       ({%H-}pending = 0) then
      // socket closed gracefully - see TCrtSocket.SockReceivePending
      include(ev, pseClosed)
    else
  *)
      include(ev, pseRead);
    if (wrp <> nil) and
       FD_ISSET(sock, wr{%H-}) then
      include(ev, pseWrite);
    if byte(ev) <> 0 then
    begin
      SetRes(results.Events[results.Count], fTagTag[i], ev);
      result := true;
      inc(results.Count);
    end;
  end;
end;


function PollSocketClass: TPollSocketClass;
begin
  result := TPollSocketSelect;
end;

function PollFewSockets: TPollSocketAbstract;
begin
  result := TPollSocketSelect.Create(nil);
end;

function WaitForSeveral(const Sockets: TPollSocketsSubscribeDynArray;
  var results: TPollSocketResults; timeoutMS: integer): boolean;
var
  n: integer;
  s: PPollSocketsSubscribe;
  p: ^TSocket;
  r: PPollSocketResult;
  tv: TTimeVal;
  fd: TFDSet;
begin
  result := false;
  results.Count := 0;
  n := length(Sockets);
  if n = 0 then
    exit;
  fd.fd_count := 0;
  s := pointer(Sockets);
  p := @fd.fd_array;
  repeat
    if pseRead in s^.events then
    begin
      p^ := TSocket(s^.socket);
      inc(p);
      inc(fd.fd_count);
      if fd.fd_count = FD_SETSIZE then
        break;
    end;
    inc(s);
    dec(n);
  until n = 0;
  if fd.fd_count = 0 then
    exit; // no pseRead to check
  SetTimeVal(timeoutMS, tv);
  n := select({ignored=}0, @fd, nil, nil, @tv);
  if n <= 0 then
    exit;
  results.Count := n;
  SetLength(results.Events, n);
  r := pointer(results.Events);
  s := pointer(Sockets);
  repeat
    if FD_ISSET(s^.socket, fd) then
    begin
      SetRes(r^, s^.tag, [pseRead]);
      inc(r);
      dec(n);
      if n = 0 then
        break;
    end;
    inc(s);
  until false;
  result := true;
end;


{ *************************** Windows IOCP sockets support }

type
  // one overlapped structure for each TWinIocpEvent
  TOverlappedEvent = record // should NOT be packed
    prepared: TLightLock;   // used if wioLockEvent is set in fOptions
    overlapped: TOverlapped;
    event: TWinIocpEvent;   // should appear just after the TOverlapped
    buf: TWsaBuf;
  end;
  POverlappedEvent = ^TOverlappedEvent;

  // data structure returned by TWinIocp.Subscribe as PWinIocpSubscription
  PIocpSubscription = ^TIocpSubscription;
  TIocpSubscription = packed record
    head: TLockedListOne; // should be the first for TLockedList to work
    over: array[TWinIocpEvent] of TOverlappedEvent;
    tag: TPollSocketTag;
    socket: TSocket;
  end;


{ TWinIocpSubscription }

const
  STATUS_REMOTE_DISCONNECT = $C000013C;

function TWinIocpSubscription.Tag: TPollSocketTag;
begin
  if @self = nil then
    result := 0
  else
    result := PIocpSubscription(@self)^.tag;
end;

function TWinIocpSubscription.Socket: TNetSocket;
begin
  if @self = nil then
    result := nil
  else
    result := TNetSocket(PIocpSubscription(@self)^.socket);
end;

function TWinIocpSubscription.CurrentStatus: TPollSocketEvents;
var
  o: PIocpSubscription;
begin
  result := [];
  o := @self;
  if o = nil then
    exit;
  case cardinal(PtrUInt(o^.over[wieRecv].overlapped.Internal)) of
    STATUS_WAIT_0:
      ; // most common case
    STATUS_PENDING:
      include(result, pseRead);
    STATUS_REMOTE_DISCONNECT:
      include(result, pseClosed);
  end;
  case cardinal(PtrUInt(o^.over[wieSend].overlapped.Internal)) of
    STATUS_WAIT_0:
      ; // most common case
    STATUS_PENDING:
      include(result, pseWrite);
    STATUS_REMOTE_DISCONNECT:
      include(result, pseClosed);
  end;
  case cardinal(PtrUInt(o^.over[wieAccept].overlapped.Internal)) of
    STATUS_WAIT_0:
      ; // most common case
    STATUS_PENDING:
      include(result, pseRead); // as select/poll/epoll
    STATUS_REMOTE_DISCONNECT:
      include(result, pseClosed);
  end;
end;


{ TWinIocp }

constructor TWinIocp.Create(processing: integer; options: TWinIocpOptions);
begin
  if processing = 0 then
    processing := SystemInfo.dwNumberOfProcessors;
  fIocp := IocpCreate(INVALID_HANDLE_VALUE, 0, nil, processing);
  if fIocp <= 0 then
    RaiseLastError('TWinIocp: CreateIoCompletionPort', EWinIocp);
  fMaxWait := processing;
  fOptions := options;
  fOne.Init(SizeOf(TIocpSubscription));
  if Assigned(fOnLog) then
    fOnLog(sllDebug, 'Create maxwait=%', [fMaxWait], self);
end;

procedure TWinIocp.Terminate;
var
  i: integer;
begin
  if self = nil then
    exit;
  fTerminated := true; // set the flag
  for i := 1 to fMaxWait do
    IocpPostQueuedStatus(fIocp, 0, nil, nil); // notify threads/queue
end;

destructor TWinIocp.Destroy;
var
  tix, endtix: cardinal;
  sub: PIocpSubscription;
  tmp: cardinal;
  tmplen: integer;
begin
  if Assigned(fOnLog) then
    fOnLog(sllDebug, 'Destroy count=%', [fOne.Count], self);
  // notify the queue we are about to close
  if not fTerminated then
    Terminate;
  // wait for all pending GetNext() termination
  endtix := 0;
  if fWaiting <> 0 then
    repeat
      if Assigned(fOnLog) then
        fOnLog(sllDebug, 'Destroy waiting=%', [fWaiting], self);
      IocpPostQueuedStatus(fIocp, 0, nil, nil); // notify again (paranoid)
      SleepHiRes(10);
      tix := mormot.core.os.GetTickCount64 shr MilliSecsPerSecShl;
      if endtix = 0 then
        endtix := tix + 5
      else if tix > endtix then
        break; // never wait forever
    until fWaiting = 0;
  // delete the queue
  if fIocp <> 0 then
    CloseHandle(fIocp);
  // close and/or log remainining sockets
  if Assigned(fOnLog) or
     (wioUnsubscribeShutdownSocket in fOptions) then
  begin
    sub := fOne.Head;
    while sub <> nil do
    begin
      if Assigned(fOnLog) then
      begin
        tmplen := 4; // is usually recv=-1 + WSAEWOULDBLOCK so not recoverable
        tmplen := mormot.net.sock.recv(sub^.socket, @tmp, tmplen, 0);
        fOnLog(sllWarning, 'leak sock=% over=% recv=% %',
          [pointer(sub^.socket), PtrUInt(sub^.over[wieRecv].overlapped.Internal),
           tmplen, RawSocketErrNo], self);
      end;
      if wioUnsubscribeShutdownSocket in fOptions then
        TNetSocket(sub^.socket).ShutdownAndClose({rdwr=}false);
      sub := sub.head.next;
    end;
  end;
  if fAcceptSocket <> nil then
    fAcceptSocket^.Close;
  // release all memory slots
  fOne.Done;
  inherited Destroy;
end;

function TWinIocp.Subscribe(socket: TNetSocket;
  tag: TPollSocketTag): PWinIocpSubscription;
var
  o: PIocpSubscription;
  res: TNetResult;
  e: TWinIocpEvent;
  sll: TSynLogLevel;
begin
  result := nil;
  if (self = nil) or
     fTerminated then
    exit;
  if socket = nil then
    raise EWinIocp.Create('Unexpected TWinIocp.Subscribe(nil)');
  o := fOne.New;
  o^.tag := tag;
  o^.socket := TSocket(socket);
  for e := low(e) to high(e) do
    o^.over[e].event := e; // marker to convert POverlapped to PIocpSubscription
  sll := sllTrace;
  res := nrOk;
  if IocpCreate(o^.socket, fIocp, pointer(o^.head.sequence), 0) = 0 then
  begin
    sll := sllLastError;
    res := nrFatalError;
    RaiseLastError('TWinIocp: iocpcreate', EWinIocp);
  end;
  if Assigned(fOnLog) then
    fOnLog(sll, 'Subscribe(socket=% seq=%)=% cnt=%',
      [socket, pointer(o^.head.sequence), ToText(res)^, Count], self);
  if res = nrOk then
    result := pointer(o)
  else
    fOne.Free(o);
end;

const
  IOCP_EVENT: array[TWinIocpEvent] of AnsiChar = 'RWAC12345';
  ACCEPTEX_SIZE = SOCKADDR_SIZE + 16;

function TWinIocp.PrepareNext(one: PWinIocpSubscription;
  event: TWinIocpEvent; buf: pointer; buflen: integer;
  netsock: TNetSocket): boolean;
var
  bytes, flags, e: cardinal;
  o: PIocpSubscription absolute one;
  oe: POverlappedEvent;
  err: PUtf8Char;
begin
  result := false;
  if (self = nil) or
     (one = nil) or
     fTerminated then
    exit;
  oe := @o^.over[event]; // each event has its own TOverlappedEvent buffer
  if (wioLockEvent in fOptions) and
     not oe^.prepared.TryLock then
    raise EWinIocp.CreateFmt('TWinIocp.PrepareGetNet: concurrent %s on %x',
      [IOCP_EVENT[event], integer(o^.socket)]);
  err := nil;
  bytes := 0;
  flags := 0;
  if buflen <= 0 then
  begin
    buf := nil; // ensure consistent TWsaBuf
    buflen := 0;
  end;
  oe^.buf.pBuffer := buf;
  oe^.buf.cLength := buflen;
  case event of
    wieRecv:
      // from mormot.net.async: overlapped read with buflen=0 to avoid WSAENOBUFS
      // and work with regular sockets recv() calls like TPollSocketAbstract
      result := // WSARecv=NO_ERROR if there is something to recv()
        (WSARecv(o^.socket, oe^.buf, 1, bytes, flags, @oe^.overlapped) = NO_ERROR) or
        (RawSocketErrNo = WSAIOPENDING); // WSAIOPENDING = wait for incoming data
    wieSend:
      // from mormot.net.async: overlapped write with buf/buflen = connection.fWr
      // to avoid GetNext returning with no delay (and save a syscall)
      result :=
        (WSASend(o^.socket, oe^.buf, 1, bytes, 0, @oe^.overlapped) = NO_ERROR) or
        (RawSocketErrNo = WSAIOPENDING);
    wieAccept:
      // overlapped AcceptEx() to be followed by GetNextAcceptAndPrepare()
      // from mormot.net.async: not used, unless IOCP_ACCEPTEX is defined
      if Assigned(acceptex) and
         Assigned(getacceptexsockaddrs) then
        if (fAcceptSocket = nil) and
           fAcceptExUsed.TryLock then // a single AcceptEx() at a time
        begin
          if fAcceptExBuf = nil then
            SetLength(fAcceptExBuf, ACCEPTEX_SIZE * 2);
          if netsock = nil then
            netsock := TNetSocket(socket(AF_INET, SOCK_STREAM, IPPROTO_IP));
          fAcceptSocket := netsock;
          result := acceptex(
                     o^.Socket, TSocket(netsock), pointer(fAcceptExBuf), 0,
                     ACCEPTEX_SIZE, ACCEPTEX_SIZE, bytes, @oe^.overlapped) or
                    (RawSocketErrNo = WSAIOPENDING)
        end
        else
          err := 'concurrent wieAccept'
      else
         err := 'no acceptex() API';
    wieConnect:
      // overlapped ConnextEx()
      // from mormot.net.async: used by THttpAsyncClientConnections.StartRequest
      if Assigned(connectex) then
        if Assigned(buf) and
           (PNetAddr(buf)^.Size = buflen) then
          if Assigned(netsock) then
            // connectex() requires an explicit bind (which is done by connect)
            if bind(TSocket(netsock), PSockAddr(buf), buflen) = NO_ERROR then
              result := connectex(TSocket(netsock), PSockAddr(buf), buflen,
                          nil, 0, bytes, @oe^.overlapped) or
                        (RawSocketErrNo = WSAIOPENDING)
            else
              err := 'bind error'
          else
            err := 'no supplied netsock'
        else
          err := 'no supplied netaddr/buf'
      else
         err := 'no connectex() API';
    else
       err := 'unexpected event';
    end;
  if err <> nil then // fatal error: need to fix the code -> raise EWinIocp
  begin
    if wioLockEvent in fOptions then
      oe^.prepared.UnLock;
    raise EWinIocp.CreateFmt('TWinIocp.PrepareNext(%s): %s on %x',
      [IOCP_EVENT[event], err, integer(o^.socket)]);
  end;
  if result then
    LockedInc32(@fPosted);
  if Assigned(fOnLog) then
    if result then
      fOnLog(sllTrace, 'PrepareNext(%%,%)',
        [IOCP_EVENT[event], pointer(o^.socket), buflen], self)
    else
    begin
      e := GetLastError;
      fOnLog(sllDebug, 'PrepareNext(%%) error % %',
        [IOCP_EVENT[event], pointer(o^.socket), e, WinErrorConstant(e)], self);
    end;
end;

function TWinIocp.Enqueue(one: PWinIocpSubscription; event: TWinIocpEvent;
  bytes: cardinal): boolean;
var
  seq: pointer;
begin
  result := false;
  if (self = nil) or
     fTerminated then
    exit;
  // 65535 < seq < MaxInt from PrepareNext(), seq=nil from Terminate
  seq := pointer(ord(event) + 1);
  result := IocpPostQueuedStatus(fIocp, bytes, seq, {overlap=}pointer(one));
  if result then
    LockedInc32(@fPosted);
  if Assigned(fOnLog) then
    fOnLog(sllTrace, 'Enqueue(one=% % %)=%',
      [one, IOCP_EVENT[event], bytes, result], self);
end;

function TWinIocp.GetNext(timeoutms: cardinal;
  out event: TWinIocpEvent; out bytes: cardinal): PWinIocpSubscription;
var
  o, seq: pointer;
  e: PtrUInt;
  sll: TSynLogLevel;
  one: PIocpSubscription absolute o;
  oe: POverlappedEvent;
begin
  result := nil;
  if (self = nil) or
     fTerminated then
    exit;
  repeat
    bytes := 0;
    o := nil;
    seq := nil;
    sll := sllTrace;
    // retrieve the next pending event from the IOCP queue
    LockedInc32(@fWaiting);
    IocpGetQueuedStatus(fIocp, bytes, seq, o, timeoutms);
    LockedDec32(@fWaiting);
    if fTerminated or
       (seq = nil) then // seq=nil from Terminate
      exit;
    LockedDec32(@fPosted);
    e := PtrUInt(seq) - 1;
    if e <= PtrUInt(ord(high(event))) then
    begin
      // seq := pointer(ord(event) + 1) from Enqueue()
      PByte(@event)^ := e;
      if Assigned(fOnLog) then
        fOnLog(sllTrace, 'GetNextEnqueue=%% %',
          [IOCP_EVENT[event], pointer(one^.socket), bytes], self);
      break;
    end;
    // O(1) decode overlapped socket notification from PrepareNext()
    inc(POverlapped(o));
    PByte(@event)^ := PByte(o)^; // TOverlappedEvent.event just after overlapped
    if event > high(event) then
      raise EWinIocp.Create('TWinIocp.GetNext: corrupted overlapped structure');
    dec(PByte(o), PtrUInt(@PIocpSubscription(nil)^.over[event].event));
    // now o/one points to PWinIocpSubscription/PIocpSubscription
    if pointer(one^.head.sequence) <> seq then
      sll := sllWarning // detect ABA race condition
    else if wioLockEvent in fOptions then
    begin
      oe := @one^.over[event];
      if oe^.prepared.IsLocked then
        oe^.prepared.UnLock
      else
        sll := sllError; // this TOverlappedEvent should be locked/prepared
    end;
    if Assigned(fOnLog) then
      if sll = sllTrace then
        fOnLog(sll, 'GetNext=%% %',
          [IOCP_EVENT[event], pointer(one^.socket), bytes], self)
      else
        fOnLog(sll, 'GetNext=%% %=%', [IOCP_EVENT[event],
          pointer(one^.socket), pointer(one^.head.sequence), seq], self);
  until sll = sllTrace;
  result := o; // got valid event
end;

function TWinIocp.GetNextAccept(one: PWinIocpSubscription;
  out Socket: TNetSocket; out Remote: TNetAddr): boolean;
var
  l, r: PSockAddr;
  llen, rlen: integer;
begin
  result := false;
  if (self = nil) or
     (one = nil) or
     fTerminated then
    exit;
  FillCharFast(Remote, SizeOf(Remote), 0);
  if (fAcceptExBuf = nil) or
     (fAcceptSocket = nil) or
     not fAcceptExUsed.IsLocked or
     not Assigned(getacceptexsockaddrs) then
  begin
    if Assigned(fOnLog) then
      fOnLog(sllWarning, 'GetNextAccept(%)=false',
        [pointer(one^.socket)], self);
    exit;
  end;
  l := nil;
  r := nil;
  llen := 0;
  rlen := 0;
  getacceptexsockaddrs(pointer(fAcceptExBuf),
    0, ACCEPTEX_SIZE, ACCEPTEX_SIZE, l, llen, r, rlen);
  if (r = nil) or
     (rlen = 0) or
     (rlen > SizeOf(Remote)) then
    exit;
  MoveFast(r^, Remote, rlen);
  Socket := fAcceptSocket;
  fAcceptSocket := nil;
  fAcceptExUsed.UnLock;
  result := true;
  if Assigned(fOnLog) then
    fOnLog(sllTrace, 'GetNextAccept(%)=%',
      [pointer(one^.socket), Remote.IPShort], self);
end;

function TWinIocp.Unsubscribe(one: PWinIocpSubscription): boolean;
var
  res: TNetResult;
  o: PIocpSubscription absolute one;
begin
  result := false;
  if (self = nil) or
     (one = nil) or
     fTerminated then
    exit;
  res := nrOK;
  if wioUnsubscribeShutdownSocket in fOptions then
    res := TNetSocket(o^.socket).ShutdownAndClose({rdwr=}false);
  if Assigned(fOnLog) then
    fOnLog(sllTrace, 'UnSubscribe(socket=% seq=%) cnt=% %',
      [pointer(o^.socket), pointer(o^.head.sequence), Count - 1,
       ToText(res)^], self);
  result := fOne.Free(one); // one^.sequence will avoid ABA problem in GetNext()
end;


{ ******************** TLS / HTTPS Encryption Abstract Layer }

type
  ESChannel = class(Exception);

  TSChannelNetTls = class(TInterfacedObject, INetTls)
  private
    fSocket: TNetSocket;
    fCipherName: RawUtf8;
    fServerAddressW: SynUnicode;
    fCred: TCredHandle;
    fCtxt: TCtxtHandle;
    fFlags: cardinal;
    fLastCheckSecError: cardinal;
    fSizes: TSecPkgContext_StreamSizes;
    fData, fInput: AnsiString;
    fInputSize, fDataPos, fDataCount, fInputCount: integer;
    fSessionClosed: boolean;
    fAccept: boolean;
    fAcceptCert: mormot.core.os.PCCERT_CONTEXT;
    fAcceptCertStore: HCERTSTORE;
    procedure ESChannelRaiseLastError(const ctx: shortstring; res: cardinal);
    function CheckSEC_E_OK(const ctx: shortstring; res: integer): cardinal;
    function FreeAndCheckSocket(const ctx: shortstring; res: integer;
      tofree: pointer): cardinal;
    function HandshakeStep(buf: PByteArray; var len: integer): cardinal;
    procedure HandshakeLoop;
    procedure FinalizeCredCtxt;
  public
    destructor Destroy; override;
    // INetTls methods
    procedure AfterConnection(Socket: TNetSocket; var Context: TNetTlsContext;
      const ServerAddress: RawUtf8);
    procedure AfterBind(var Context: TNetTlsContext);
    procedure AfterAccept(Socket: TNetSocket; const BoundContext: TNetTlsContext;
      LastError, CipherName: PRawUtf8);
    function GetCipherName: RawUtf8;
    function GetRawTls: pointer;
    function Receive(Buffer: pointer; var Length: integer): TNetResult;
    function ReceivePending: integer;
    function Send(Buffer: pointer; var Length: integer): TNetResult;
  end;


{ TSChannelNetTls }

procedure TSChannelNetTls.ESChannelRaiseLastError(
  const ctx: shortstring; res: cardinal);
var
  sys: integer;
  msg: string;
begin
  sys := GetLastError;
  msg := format('<%s>: %s returned %x [%s], System Error %d [%s]',
    [fServerAddressW, ctx, res, SspiResToText(res), sys, GetErrorText(sys)]);
  //writeln(fSocket.Socket,' ',msg);
  raise ESChannel.Create(msg);
end;

function TSChannelNetTls.CheckSEC_E_OK(
  const ctx: shortstring; res: integer): cardinal;
begin
  fLastCheckSecError := res;
  if res <> SEC_E_OK then
    ESChannelRaiseLastError(ctx, res);
  result := res;
end;

function TSChannelNetTls.FreeAndCheckSocket(const ctx: shortstring;
  res: integer; tofree: pointer): cardinal;
begin
  //writeln(fSocket.Socket,' ',ctx,'=',res);
  if tofree <> nil then
    CheckSEC_E_OK(ctx, FreeContextBuffer(tofree)); // avoid leak id raised
  if res = SOCKET_ERROR then
    raise ESChannel.CreateFmt('%s: Socket Error %d', [ctx, RawSocketErrNo]);
  if res = 0 then
    raise ESChannel.CreateFmt('%s: Handshake aborted', [ctx]);
  result := res;
end;

procedure TSChannelNetTls.FinalizeCredCtxt;
begin
  DeleteSecurityContext(@fCtxt);
  FreeCredentialsHandle(@fCred);
  FillCharFast(fCtxt, SizeOf(fCtxt), 0);
  FillCharFast(fCred, SizeOf(fCred), 0);
end;

const
  TLSRECMAXSIZE = 19000; // buffers for TSChannelNetTls.Receive/Send

type
  THandshakeBuf = record
    buf: array[0..4] of TSecBuffer;
    input, output: TSecBufferDesc;
  end;

procedure HandshakeBufInit(var buf: THandshakeBuf);
begin
  FillCharFast(buf, SizeOf(buf), 0);
  buf.input.ulVersion := SECBUFFER_VERSION;
  buf.input.cBuffers := 2;
  buf.input.pBuffers := @buf.buf[0];
  buf.buf[0].BufferType := SECBUFFER_TOKEN;
  buf.buf[1].BufferType := SECBUFFER_EMPTY;
  buf.output.ulVersion := SECBUFFER_VERSION;
  buf.output.cBuffers := 3;
  buf.output.pBuffers := @buf.buf[2];
  buf.buf[2].BufferType := SECBUFFER_TOKEN;
  buf.buf[3].BufferType := SECBUFFER_ALERT;
  buf.buf[4].BufferType := SECBUFFER_EMPTY;
end;

procedure CredInit(var cred: TSChannelCred; AllowDeprecatedTLS: boolean;
  dwFlags: cardinal; paServCred: PPCCERT_CONTEXT);
begin
  FillCharFast(cred, SizeOf(cred), 0);
  // TLS 1.3 is officially supported starting in Windows 11 and Server 2022.
  // Enabling TLS 1.3 on earlier versions of Windows is possible through the
  // registry, but is explictly documented as unsafe by Microsoft.
  // https://learn.microsoft.com/en-us/windows/win32/secauthn/protocols-in-tls-ssl--schannel-ssp-
  if (OSVersion in [wEleven, wEleven_64]) or
     (OSVersion >= wServer2022_64) then
  begin
    // require new SCH_CREDENTIALS structure in order to enable TLS 1.3
    cred.New.dwVersion := SCH_CREDENTIALS_VERSION;
    if not AllowDeprecatedTLS then
    begin
      dwFlags := dwFlags or SCH_USE_STRONG_CRYPTO;
      cred.New.cTlsParameters := 1;
      cred.New.pTlsParameters := @cred.Tls;
      cred.Tls.grbitDisabledProtocols := cardinal(not SP_PROT_TLS_SAFE);
    end;
    cred.New.dwFlags := dwFlags;
    if paServCred <> nil then
    begin
      cred.New.cCreds := 1;
      cred.New.paCred := paServCred;
    end;
  end
  else
  begin
    // legacy SCHANNEL_CRED structure - deprecated since Windows 10, 1809
    cred.Old.dwVersion := SCHANNEL_CRED_VERSION;
    cred.Old.dwFlags := dwFlags;
    cred.Old.grbitEnabledProtocols := SP_PROT_TLS_SAFE;
    if (OSVersion < wSeven) or // XP and Vista only support TLS 1.0 anyway :(
       AllowDeprecatedTls then
      cred.Old.grbitEnabledProtocols := cred.Old.grbitEnabledProtocols or
        SP_PROT_TLS1_0 or SP_PROT_TLS1_1;
    if paServCred <> nil then
    begin
      cred.Old.cCreds := 1;
      cred.Old.paCred := paServCred;
    end;
  end;
end;

procedure TSChannelNetTls.AfterConnection(Socket: TNetSocket;
  var Context: TNetTlsContext; const ServerAddress: RawUtf8);
var
  cred: TSChannelCred;
  buf: THandshakeBuf;
  nfo: TWinCertInfo;
  res, f, trial: cardinal;
begin
  // prepare execution context
  fSocket := Socket;
  fServerAddressW := SynUnicode(ServerAddress);
  SetLength(fData, TLSRECMAXSIZE);
  fAccept := false;
  trial := 0;
  while true do
    try
      if Context.IgnoreCertificateErrors then
        f := SCH_CRED_MANUAL_CRED_VALIDATION or
             SCH_CRED_NO_DEFAULT_CREDS
      else
        f := SCH_CRED_REVOCATION_CHECK_CHAIN or
             SCH_CRED_IGNORE_REVOCATION_OFFLINE;
      CredInit(cred, Context.AllowDeprecatedTLS, f, nil);
      CheckSEC_E_OK('AcquireCredentialsHandleW', AcquireCredentialsHandleW(
        nil, UNISP_NAME, SECPKG_CRED_OUTBOUND, nil, @cred, nil, nil, @fCred, nil));
      fDataPos := 0;
      fDataCount := 0;
      fFlags := ISC_REQ_FLAGS;
      if Context.IgnoreCertificateErrors then
        // prevent SEC_E_UNTRUSTED_ROOT result in HandshakeLoop
        fFlags := fFlags or ISC_REQ_MANUAL_CRED_VALIDATION;
      // initiate a ClientHello TLS message and a new fCtxt
      HandshakeBufInit(buf);
      res := InitializeSecurityContextW(
        @fCred, nil, pointer(fServerAddressW), fFlags, 0, 0,
        nil, 0, @fCtxt, @buf.output, f, nil);
      if res <> SEC_I_CONTINUE_NEEDED then
        ESChannelRaiseLastError('InitializeSecurityContextW', res);
      if (buf.buf[2].cbBuffer = 0) or // SECBUFFER_TOKEN
         (buf.buf[2].pvBuffer = nil) then
        raise ESChannel.CreateFmt('Void Hello answer to %s', [ServerAddress]);
      FreeAndCheckSocket('send', mormot.net.sock.Send(
        fSocket.Socket, buf.buf[2].pvBuffer, buf.buf[2].cbBuffer, 0),
        buf.buf[2].pvBuffer);
      // make TLS handshake and prepare for process
      HandshakeLoop;
      break; // we are connected
    except
      // circumvent Windows 7/8 random bug identified on TLS 1.2 and
      // TLS_DHE_RSA_WITH_AES_128_GCM_SHA256 TLS_DHE_RSA_WITH_AES_256_GCM_SHA384
      // https://github.com/Waffle/waffle/pull/128#issuecomment-163342222
      on E: ESChannel do
        if (trial = 0) and
           ((fLastCheckSecError = SEC_E_BUFFER_TOO_SMALL) or
            (fLastCheckSecError = SEC_E_MESSAGE_ALTERED)) then
        begin
          // just retry once
          FinalizeCredCtxt;
          inc(trial);
        end
        else
          raise;
    end;
  Context.CipherName := GetCipherName;
  if TlsCertInfo(fCtxt, nfo) then
  begin
    Context.PeerIssuer := nfo.IssuerName;
    Context.PeerSubject := nfo.SubjectName;
    if Context.WithPeerInfo and
       Assigned(WinCertInfoToText) then
      Context.PeerInfo := WinCertInfoToText(nfo);
  end;
end;

procedure TSChannelNetTls.AfterBind(var Context: TNetTlsContext);
var
  certblob: RawByteString;
  blob: TCryptDataBlob;
  pass: SynUnicode;
  flags: integer;
  keyspec: cardinal;
  prov: HCRYPTPROV;
  freeprov: BOOL;
begin
  if Context.CertificateFile = '' then
    // Load certificate and private key from Windows certificate store
    fAcceptCertStore := CertOpenSystemStoreW(nil, 'MY')
  else
  begin
    certblob := StringFromFile(TFileName(Context.CertificateFile));
    blob.cbData := Length(certblob);
    blob.pbData := pointer(certblob);
    // Load certificate from file. You can use Let's Encrypt certificate,
    // converted to PFX:
    //   openssl pkcs12 -inkey privkey.pem -in cert.pem -export -out mycert.pfx
    // or using mormot.core.secure on OpenSSL:
    //   c := Cert('x509-rs256');
    //   c.Generate([cuTlsServer], '127.0.0.1', nil, 3650);
    //   FileFromString(c.Save('pass', ccfBinary), WorkDir + 'privkeycert.pfx');
    pass := SynUnicode(Context.PrivatePassword);
    flags := PKCS12_INCLUDE_EXTENDED_PROPERTIES;
    if OSVersion < wVista then
      flags := 0;
    fAcceptCertStore := PFXImportCertStore(@blob, pointer(pass), flags);
    if pass <> '' then
      FillCharFast(pointer(pass)^, length(pass) * 2, 0); // anti forensic
    if fAcceptCertStore = nil then
      ESChannelRaiseLastError('PFXImportCertStoreSEC_E_CERT_UNKNOWN',
        SEC_E_CERT_UNKNOWN);
  end;
  // find first certificate in store with private key
  fAcceptCert := nil;
  repeat
    fAcceptCert := mormot.lib.sspi.CertFindCertificateInStore(
      fAcceptCertStore, 0, 0, CERT_FIND_ANY, nil, fAcceptCert);
    if fAcceptCert = nil then
      raise ESChannel.Create('AfterBind: no Certificate available');
  until CryptAcquireCertificatePrivateKey(
         fAcceptCert, 0, nil, prov, keyspec, freeprov);
  if freeprov and
     CryptoApi.Available then
    CryptoApi.ReleaseContext(prov, 0);
  // this global certificate will be used by AfterAccept()
  Context.AcceptCert := fAcceptCert;
end;

procedure TSChannelNetTls.AfterAccept(Socket: TNetSocket;
  const BoundContext: TNetTlsContext; LastError, CipherName: PRawUtf8);
var
  cred: TSChannelCred;
begin
  // prepare execution context
  fSocket := Socket;
  fAccept := true;
  // prepare TLS connection properties from AfterBind() global certificate
  if BoundContext.AcceptCert = nil then
    raise ESChannel.Create('AfterAccept: missing AfterBind');
  CredInit(cred, BoundContext.AllowDeprecatedTls, 0, @BoundContext.AcceptCert);
  CheckSEC_E_OK('AcquireCredentialsHandleW',
    AcquireCredentialsHandleW(nil, UNISP_NAME, SECPKG_CRED_INBOUND,
      nil, @cred, nil, nil, @fCred, nil));
  fDataPos := 0;
  fDataCount := 0;
  fFlags := ASC_REQ_FLAGS;
  // make TLS handshake and prepare for process
  SetLength(fData, TLSRECMAXSIZE);
  HandshakeLoop;
  if CipherName <> nil then
    CipherName^ := GetCipherName;
end;

function TSChannelNetTls.GetCipherName: RawUtf8;
begin
  if fCipherName = '' then
    fCipherName := TlsConnectionInfo(fCtxt);
  result := fCipherName;
end;

function TSChannelNetTls.GetRawTls: pointer;
begin
  result := @fCtxt; // return as PCtxtHandle
end;

function TSChannelNetTls.HandshakeStep(buf: PByteArray; var len: integer): cardinal;
var
  f: cardinal;
  b: THandshakeBuf;
  LInCtxPtr: PSecHandle;
begin
  //writeln(fSocket.Socket,' Handshake STEP ', len);
  HandshakeBufInit(b);
  b.buf[0].pvBuffer := buf; // SECBUFFER_TOKEN
  b.buf[0].cbBuffer := len;
  f := 0;
  if fAccept then // server side
  begin
    if (fCtxt.dwLower = 0) and
       (fCtxt.dwUpper = 0) then
      LInCtxPtr := nil
    else
      LInCtxPtr := @fCtxt;
    result := AcceptSecurityContext(
      @fCred, LInCtxPtr, @b.input, fFlags, 0, @fCtxt, @b.output, f, nil);
  end
  else // client side
    result := InitializeSecurityContextW(
      @fCred, @fCtxt, pointer(fServerAddressW), fFlags, 0, 0,
      @b.input, 0, nil, @b.output, f, nil);
  //writeln(fSocket.Socket,' ',SspiResToText(result),' tosend=',b.buf[2].cbBuffer);
  if (result = SEC_E_OK) or
     (result = SEC_I_CONTINUE_NEEDED) or
     ((f and ISC_REQ_EXTENDED_ERROR) <> 0) then
    if (b.buf[2].cbBuffer <> 0) and
       (b.buf[2].pvBuffer <> nil) then
      // need to send back something to the server
      FreeAndCheckSocket('send', mormot.net.sock.Send(
        fSocket.Socket, b.buf[2].pvBuffer, b.buf[2].cbBuffer, 0),
        b.buf[2].pvBuffer);
  if (b.buf[1].BufferType = SECBUFFER_EXTRA) and
     (b.buf[1].cbBuffer <> 0) then
  begin
    // reuse pending bytes - avoid unexpected SEC_E_INVALID_TOKEN
    //writeln(fSocket.Socket,' SECBUFFER_EXTRA=',b.buf[1].cbBuffer,'/',len);
    MoveFast(buf[cardinal(len) - b.buf[1].cbBuffer], buf[0], b.buf[1].cbBuffer);
    len := b.buf[1].cbBuffer;
  end
  else if result <> SEC_E_INCOMPLETE_MESSAGE then
    len := 0;
end;

procedure TSChannelNetTls.HandshakeLoop;
var
  res: cardinal;
begin
  //writeln(fSocket.Socket,' Handshake IN');
  repeat
    inc(fDataCount, FreeAndCheckSocket('recv',
      recv(fSocket.Socket,
        @PByteArray(fData)[fDataCount], length(fData) - fDataCount, 0), nil));
    res := HandshakeStep(pointer(fData), fDataCount);
    if res = SEC_I_INCOMPLETE_CREDENTIALS then
      // check https://stackoverflow.com/a/47479968/458259
      // run twice to let the handshake pass
      res := HandshakeStep(pointer(fData), fDataCount);
  until (res <> SEC_I_CONTINUE_NEEDED) and
        (res <> SEC_E_INCOMPLETE_MESSAGE);
  CheckSEC_E_OK('HandshakeStep', res);
  // note: fDataCount trailing content may come from SECBUFFER_EXTRA bytes
  fCipherName := ''; // will be retrieved if needed
  if fInputSize = 0 then
  begin
    // retrieve the TLS field sizes during first handshake
    CheckSEC_E_OK('QueryContextAttributesW',
      QueryContextAttributesW(@fCtxt, SECPKG_ATTR_STREAM_SIZES, @fSizes));
    fInputSize := fSizes.cbHeader + fSizes.cbMaximumMessage + fSizes.cbTrailer;
    if (fInputSize = 0) or
       (fInputSize > TLSRECMAXSIZE) then
      raise ESChannel.CreateFmt('InputSize=%d>%d', [fInputSize, TLSRECMAXSIZE]);
    SetLength(fInput, fInputSize);
    fInputCount := 0;
  end;
  //writeln(fSocket.Socket,' Handshake OUT inputsize=',fInputSize);
end;

destructor TSChannelNetTls.Destroy;
var
  desc: TSecBufferDesc;
  buf: TSecBuffer;
  dt, f, res: cardinal;
begin
  try
    if {%H-}PtrInt(fSocket.Socket) > 0 then
    begin
      // notify the other end with proper TLS shutdown frames
      desc.ulVersion := SECBUFFER_VERSION;
      desc.cBuffers := 1;
      desc.pBuffers := @buf;
      buf.cbBuffer := 4;
      buf.BufferType := SECBUFFER_TOKEN;
      dt := SCHANNEL_SHUTDOWN;
      buf.pvBuffer := @dt;
      if ApplyControlToken(@fCtxt, @desc) = SEC_E_OK then
      begin
        buf.cbBuffer := 0;
        buf.BufferType := SECBUFFER_TOKEN;
        buf.pvBuffer := nil;
        if fAccept then
          res := AcceptSecurityContext(
            @fCred, @fCtxt, nil, fFlags, 0, nil, @desc, f, nil)
        else
          res := InitializeSecurityContextW(
            @fCred, @fCtxt, pointer(fServerAddressW), fFlags, 0, 0,
            nil, 0, @fCtxt, @desc, f, nil);
        if res = SEC_E_OK then
        begin
          mormot.net.sock.Send(fSocket.Socket, buf.pvBuffer, buf.cbBuffer, 0);
          FreeContextBuffer(buf.pvBuffer);
        end;
      end;
    end;
    // finalize TLS context for this connection
    FinalizeCredCtxt;
  finally
    // release AfterBind() certificates information
    if fAcceptCert <> nil then
      CertFreeCertificateContext(fAcceptCert);
    if fAcceptCertStore <> nil then
      CertCloseStore(fAcceptCertStore, CERT_CLOSE_STORE_DEFAULT);
    inherited Destroy;
  end;
end;

function TSChannelNetTls.ReceivePending: integer;
begin
  if fSessionClosed then
    result := -1
  else
    result := fDataCount;
end;

function TSChannelNetTls.Receive(Buffer: pointer; var Length: integer): TNetResult;
var
  desc: TSecBufferDesc;
  buf: array[0..3] of TSecBuffer;
  res: cardinal;
  read, i, newlen: integer;

  function DecryptInput: cardinal;
  var
    qop: cardinal;
  begin
    buf[0].cbBuffer := fInputCount;
    buf[0].BufferType := SECBUFFER_DATA;
    buf[0].pvBuffer := pointer(fInput);
    buf[1].cbBuffer := 0;
    buf[1].BufferType := SECBUFFER_EMPTY;
    buf[1].pvBuffer := nil;
    buf[2].cbBuffer := 0;
    buf[2].BufferType := SECBUFFER_EMPTY;
    buf[2].pvBuffer := nil;
    buf[3].cbBuffer := 0;
    buf[3].BufferType := SECBUFFER_EMPTY;
    buf[3].pvBuffer := nil;
    result := DecryptMessage(@fCtxt, @desc, 0, qop);
  end;

begin
  if fSessionClosed and
     (fDataCount = 0) then
  begin
    result := nrClosed;
    exit;
  end;
  while fDataCount = 0 do
  try
    fDataPos := 0;
    desc.ulVersion := SECBUFFER_VERSION;
    desc.cBuffers := 4;
    desc.pBuffers := @buf[0];
    repeat
      read := recv(fSocket.Socket,
                @PByteArray(fInput)[fInputCount], fInputSize - fInputCount, 0);
      if read <= 0 then
      begin
        if read = 0 then
          result := nrClosed
        else
        begin
          result := NetLastError; // may be nrRetry for WSATRY_AGAIN
          if result = nrOK then
            result := nrUnknownError;
        end;
        exit;
      end;
      inc(fInputCount, read);
      res := DecryptInput;
    until res <> SEC_E_INCOMPLETE_MESSAGE;
    repeat
//if res <> SEC_E_OK then writeln(fSocket.Socket,' res=',SspiResToText(res));
      case res of
        SEC_I_RENEGOTIATE:
          ; // appears with TLS 1.3
        SEC_I_CONTEXT_EXPIRED:
          fSessionClosed := true;
        SEC_E_INCOMPLETE_MESSAGE:
          break;
        else
          CheckSEC_E_OK('DecryptInput', res);
      end;
      fInputCount := 0;
      for i := 1 to 3 do // i=0 for SECBUFFER_DATA input
        case buf[i].BufferType of
          SECBUFFER_DATA:
            begin
              newlen := fDataCount + integer(buf[i].cbBuffer);
              if newlen > system.Length(fData) then
                SetLength(fData, newlen);
              MoveFast(buf[i].pvBuffer^, PByteArray(fData)[fDataCount], buf[i].cbBuffer);
              inc(fDataCount, buf[i].cbBuffer);
            end;
          SECBUFFER_EXTRA:
            begin
              MoveFast(buf[i].pvBuffer^, pointer(fInput)^, buf[i].cbBuffer);
              fInputCount := buf[i].cbBuffer;
            end;
        end;
      if res = SEC_I_RENEGOTIATE then
// https://learn.microsoft.com/en-us/windows/win32/secauthn/decryptmessage--schannel#remarks
        HandshakeStep(pointer(fInput), fInputCount);
      if fInputCount = 0 then
        break;
      res := DecryptInput;
    until false;
  except
    // catch any socket/SChannel exception for the caller to shutdown the socket
    result := nrFatalError;
    exit;
  end;
  read := fDataCount;
  if Length < read then
    read := Length;
  MoveFast(PByteArray(fData)[fDataPos], Buffer^, read);
  inc(fDataPos, read);
  dec(fDataCount, read);
  Length := read;
  result := nrOK;
end;

function TSChannelNetTls.Send(Buffer: pointer; var Length: integer): TNetResult;
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
  pending := Length;
  while pending > 0 do
  begin
    templen := pending;
    if templen > fSizes.cbMaximumMessage then
      templen := fSizes.cbMaximumMessage;
    MoveFast(Buffer^, temp[fSizes.cbHeader], templen);
    inc(PByte(Buffer), templen);
    dec(pending, templen);
    trailer := fSizes.cbHeader + templen;
    buf[0].cbBuffer := fSizes.cbHeader;
    buf[0].BufferType := SECBUFFER_STREAM_HEADER;
    buf[0].pvBuffer := @temp;
    buf[1].cbBuffer := templen;
    buf[1].BufferType := SECBUFFER_DATA;
    buf[1].pvBuffer := @temp[fSizes.cbHeader];
    buf[2].cbBuffer := fSizes.cbTrailer;
    buf[2].BufferType := SECBUFFER_STREAM_TRAILER;
    buf[2].pvBuffer := @temp[trailer];
    buf[3].cbBuffer := 0;
    buf[3].BufferType := SECBUFFER_EMPTY;
    buf[3].pvBuffer := nil;
    if EncryptMessage(@fCtxt, 0, @desc, 0) <> SEC_E_OK then
      exit; // caller would shutdown the connection on SChannel error
    len := buf[0].cbBuffer + buf[1].cbBuffer + buf[2].cbBuffer;
    sent := 0;
    repeat
      s := mormot.net.sock.Send(fSocket.Socket, @temp[sent], len, MSG_NOSIGNAL);
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
      SleepHiRes(0); // warning: Sleep(1) waits typically 1-15 ms on Windows
      // loop to try again
    until false;
  end;
  result := nrOK;
end;



function GetExtensionFunc(sock: TSocket; const id: TGuid): pointer;
var
  ret: cardinal;
begin
  result := nil;
  if WSAIoctl(sock, SIO_GET_EXTENSION_FUNCTION_POINTER, @id, sizeof(id),
      @result, SizeOf(result), @ret, nil, nil) <> 0 then
    result := nil;
end;

function NewSChannelNetTls: INetTls;
begin
  result := TSChannelNetTls.Create;
end;

var
  WsaDataOnce: TWSADATA;

procedure InitializeUnit;
var
  sock: TSocket;
begin
  // the Windows Sockets API (WSA) needs explicit initialization
  WSAStartup($0202, WsaDataOnce);
  SocketAPIVersion := RawUtf8(Format('%s.%d',
    [WsaDataOnce.szDescription, WsaDataOnce.wVersion]));
  // get some low-level extended functions for IOCP
  sock := socket(AF_INET, SOCK_STREAM, IPPROTO_IP);
  acceptex := GetExtensionFunc(sock, WSAID_ACCEPTEX);
  getacceptexsockaddrs := GetExtensionFunc(sock, WSAID_GETACCEPTEXSOCKADDRS);
  connectex := GetExtensionFunc(sock, WSAID_CONNECTEX);
  //transmitfile := GetExtensionFunc(sock, WSAID_TRANSMITFILE);
  closesocket(sock);
  // we can use SChannel for TLS support (including TLS 1.3 on Windows 11)
  NewNetTls := NewSChannelNetTls;
end;

procedure FinalizeUnit;
begin
  // the Windows Sockets API (WSA) needs explicit finalization
  WSACleanup;
end;

