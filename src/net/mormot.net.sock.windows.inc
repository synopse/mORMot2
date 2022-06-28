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

const
  // default on Windows is 64, but we grow it to 512 for TPollSocketSelect
  // as stated by Raymond Chen: "you can make FD_SET­SIZE bigger if you need to"
  // https://devblogs.microsoft.com/oldnewthing/20161221-00/?p=94985
  // -> Xitami uses 1024 since 1996
  // -> safe since our TFDSet is INTERNAL to this unit, and never published :)
  FD_SETSIZE = 512;

type
  /// a socket is always pointer-sized on Windows
  // - and we also use PtrInt for our TNetSocketWrap cast
  TSocket = PtrInt;

  tsocklen = integer;

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

  MSG_NOSIGNAL  = 0;

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
  MSG_OOB       = $01;  // Process out-of-band data
  MSG_PEEK      = $02;  // Peek at incoming messages

const
  // All Windows Sockets error constants are biased by WSABASEERR offset
  WSABASEERR              = 10000;

  // Windows Sockets definitions of regular Microsoft C error constants
  WSAEINTR                = (WSABASEERR + 4);  // legacy flag
  WSAEBADF                = (WSABASEERR + 9);
  WSAEACCES               = (WSABASEERR + 13);
  WSAEFAULT               = (WSABASEERR + 14);
  WSAEINVAL               = (WSABASEERR + 22);
  WSAEMFILE               = (WSABASEERR + 24);

  // Windows Sockets definitions of regular Berkeley error constants
  WSAEWOULDBLOCK          = (WSABASEERR + 35);
  WSAEINPROGRESS          = (WSABASEERR + 36);
  WSAEALREADY             = (WSABASEERR + 37);
  WSAENOTSOCK             = (WSABASEERR + 38);
  WSAEDESTADDRREQ         = (WSABASEERR + 39);
  WSAEMSGSIZE             = (WSABASEERR + 40);
  WSAEPROTOTYPE           = (WSABASEERR + 41);
  WSAENOPROTOOPT          = (WSABASEERR + 42);
  WSAEPROTONOSUPPORT      = (WSABASEERR + 43);
  WSAESOCKTNOSUPPORT      = (WSABASEERR + 44);
  WSAEOPNOTSUPP           = (WSABASEERR + 45);
  WSAEPFNOSUPPORT         = (WSABASEERR + 46);
  WSAEAFNOSUPPORT         = (WSABASEERR + 47);
  WSAEADDRINUSE           = (WSABASEERR + 48);
  WSAEADDRNOTAVAIL        = (WSABASEERR + 49);
  WSAENETDOWN             = (WSABASEERR + 50);
  WSAENETUNREACH          = (WSABASEERR + 51);
  WSAENETRESET            = (WSABASEERR + 52);
  WSAECONNABORTED         = (WSABASEERR + 53);
  WSAECONNRESET           = (WSABASEERR + 54);
  WSAENOBUFS              = (WSABASEERR + 55);
  WSAEISCONN              = (WSABASEERR + 56);
  WSAENOTCONN             = (WSABASEERR + 57);
  WSAESHUTDOWN            = (WSABASEERR + 58);
  WSAETOOMANYREFS         = (WSABASEERR + 59);
  WSAETIMEDOUT            = (WSABASEERR + 60);
  WSAECONNREFUSED         = (WSABASEERR + 61);
  WSAELOOP                = (WSABASEERR + 62);
  WSAENAMETOOLONG         = (WSABASEERR + 63);
  WSAEHOSTDOWN            = (WSABASEERR + 64);
  WSAEHOSTUNREACH         = (WSABASEERR + 65);
  WSAENOTEMPTY            = (WSABASEERR + 66);
  WSAEPROCLIM             = (WSABASEERR + 67);
  WSAEUSERS               = (WSABASEERR + 68);
  WSAEDQUOT               = (WSABASEERR + 69);
  WSAESTALE               = (WSABASEERR + 70);
  WSAEREMOTE              = (WSABASEERR + 71);

  // Extended Windows Sockets error constant definitions
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

  // Error return codes from gethostbyname() and gethostbyaddr()
  // Authoritative Answer: Host not found
  WSAHOST_NOT_FOUND       = (WSABASEERR+1001);
  // Non-Authoritative: Host not found, or SERVERFAIL
  WSATRY_AGAIN            = (WSABASEERR+1002);
  // Non recoverable errors, FORMERR, REFUSED, NOTIMP
  WSANO_RECOVERY          = (WSABASEERR+1003);
  // Valid name, no data record of requested type
  WSANO_DATA              = (WSABASEERR+1004);
  // no address, look for MX record
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

const
  _ST: array[TNetLayer] of integer = (
    SOCK_STREAM,
    SOCK_DGRAM,
    0);

  _IP: array[TNetLayer] of integer = (
    IPPROTO_TCP,
    IPPROTO_UDP,
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
function sockerrno: integer; stdcall;
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
function inet_addr(cp: PAnsiChar): cardinal; stdcall;
  external ws2 name 'inet_addr';

function doaccept(s: TSocket; addr: PSockAddr; var async: boolean): TSocket;
var
  len: integer;
begin
  len := SOCKADDR_SIZE;
  result  := accept(s, addr, len);
  // no accept4() on Windows -> async will be done later
end;


{ ******** TNetSocket Cross-Platform Wrapper }

{ TNetAddr }

function SetAddr(const ip, port: RawUtf8; var h: TAddrInfo; var a: TSockAddr): boolean;
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

function TNetAddr.SetFrom(const address, addrport: RawUtf8;
  layer: TNetLayer): TNetResult;
var
  h4, h6: TAddrInfo;
begin
  if layer = nlUnix then
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
  v.l_onoff := 0; // don't wait - ord(linger >= 0) would wait
  // https://docs.microsoft.com/en-us/windows/win32/api/winsock/nf-winsock-closesocket
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

function TNetSocketWrap.WaitFor(ms: integer; scope: TNetEvents;
  loerr: system.PInteger): TNetEvents;
var
  sock: TSocket;
  rd, wr: record
    fd_count: integer;
    fd_array: array[0..0] of TSocket; // no need of the 512 entries of TFDSet
  end;
  rdp, wrp: PFDSet;
  tv: TTimeVal;
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
  sock := TSocket(@self);
  if neWrite in scope then
  begin
    wr.fd_array[0] := sock;
    wr.fd_count := 1;
    wrp := @wr;
  end;
  if (neRead in scope) or
     not (neWrite in scope) then // a least one is needed
  begin
    rd.fd_array[0] := sock;
    rd.fd_count := 1;
    rdp := @rd;
  end;
  SetTimeVal(ms, tv);
  res := select(sock + 1, rdp, wrp, nil, @tv);
  if res < 0 then
  begin
    if loerr <> nil then
      loerr^ := sockerrno;
    exit;
  end;
  result := [];
  if res = 0 then
    // nothing new detected
    exit;
  if (rdp <> nil) and
     ({%H-}rd.fd_count = 1) and
     (rd.fd_array[0] = sock) then
  begin
    if ioctlsocket(sock, FIONREAD, @pending) <> NO_ERROR then
      include(result, neError)
    else 
      include(result, neRead);
    // FIONREAD is not enough on Windows for graceful disconnection
    // -> return neRead and not neClosed if pending=0
  end;
  if (wrp <> nil) and
     ({%H-}wr.fd_count = 1) and
     (wr.fd_array[0] = sock) then
    include(result, neWrite);
end;


{ ******************** IP Addresses Support }

const
  IPHLPAPI = 'iphlpapi.dll';

type
  PMIB_IPADDRTABLE = ^MIB_IPADDRTABLE;
  MIB_IPADDRTABLE = record
    dwNumEntries: DWORD;
    ip: array[0..200] of record
      dwAddr: DWORD;
      dwIndex: DWORD;
      dwMask: DWORD;
      dwBCastAddr: DWORD;
      dwReasmSize: DWORD;
      unused1: Word;
      wType: Word;
    end;
  end;

function GetIpAddrTable(pIpAddrTable: PMIB_IPADDRTABLE;
  var pdwSize: DWORD; bOrder: BOOL): DWORD; stdcall; external IPHLPAPI;


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
  if GetIpAddrTable(@Table, Size, false) <> NO_ERROR then
    exit;
  SetLength(result, Table.dwNumEntries);
  n := 0;
  for i := 0 to Table.dwNumEntries-1 do
    with Table.ip[i] do
    if (dwAddr <> $0100007f) and
       (dwAddr <> 0) then
    begin
      case Kind of
        tiaIPv4Public:
          if not IsPublicIP(dwAddr) then
            continue;
        tiaIPv4Private:
          if IsPublicIP(dwAddr) then
            continue;
      end;
      IP4Text(@dwAddr, result[n]);
      inc(n);
    end;
  if n <> Table.dwNumEntries then
    SetLength(result, n);
end;



{ ******************** Mac Addresses Support }

const
  MAX_ADAPTER_ADDRESS_LENGTH = 8;
  GAA_FLAG_SKIP_UNICAST = $1;
  GAA_FLAG_SKIP_ANYCAST = $2;
  GAA_FLAG_SKIP_MULTICAST = $4;
  GAA_FLAG_SKIP_DNS_SERVER = $8;
  GAA_FLAG_SKIP_FRIENDLY_NAME = $20;
  GAA_FLAG_INCLUDE_ALL_INTERFACES = $100; // Vista+
  GAA_FLAGS = GAA_FLAG_SKIP_UNICAST or
              GAA_FLAG_SKIP_ANYCAST or
              GAA_FLAG_SKIP_MULTICAST or
              GAA_FLAG_SKIP_DNS_SERVER or
              GAA_FLAG_SKIP_FRIENDLY_NAME;
              // or GAA_FLAG_INCLUDE_ALL_INTERFACES;
  IfOperStatusUp = 1;

type
  SOCKET_ADDRESS = record
    lpSockaddr: PSOCKADDR;
    iSockaddrLength: Integer;
  end;
  PIP_ADAPTER_UNICAST_ADDRESS = pointer;
  PIP_ADAPTER_ANYCAST_ADDRESS = pointer;
  PIP_ADAPTER_DNS_SERVER_ADDRESS = pointer;
  PIP_ADAPTER_MULTICAST_ADDRESS = pointer;
  PIP_ADAPTER_ADDRESSES = ^_IP_ADAPTER_ADDRESSES;
  _IP_ADAPTER_ADDRESSES = record
    Union: record
      case Integer of
        0: (
          Alignment: QWord);
        1: (
          Length: ULONG;
          IfIndex: DWORD);
    end;
    Next: PIP_ADAPTER_ADDRESSES;
    AdapterName: PAnsiChar;
    FirstUnicastAddress: PIP_ADAPTER_UNICAST_ADDRESS;
    FirstAnycastAddress: PIP_ADAPTER_ANYCAST_ADDRESS;
    FirstMulticastAddress: PIP_ADAPTER_MULTICAST_ADDRESS;
    FirstDnsServerAddress: PIP_ADAPTER_DNS_SERVER_ADDRESS;
    DnsSuffix: PWCHAR;
    Description: PWCHAR;
    FriendlyName: PWCHAR;
    PhysicalAddress: array [0..MAX_ADAPTER_ADDRESS_LENGTH - 1] of BYTE;
    PhysicalAddressLength: DWORD;
    Flags: DWORD;
    Mtu: DWORD;
    IfType: ULONG;
    OperStatus: DWORD;
    // below fields are only available on Windows XP with SP1 and later
    Ipv6IfIndex: ULONG;
    ZoneIndices: array [0..15] of DWORD;
    FirstPrefix: pointer;
    // below fields are only available on Windows Vista and later
    TransmitLinkSpeed: Int64;
    ReceiveLinkSpeed: Int64;
    FirstWinsServerAddress: pointer;
    FirstGatewayAddress: pointer;
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

function GetAdaptersAddresses(Family: ULONG; Flags: DWORD; Reserved: pointer;
  pAdapterAddresses: PIP_ADAPTER_ADDRESSES; pOutBufLen: PULONG): DWORD; stdcall;
  external IPHLPAPI;

function SendARP(DestIp: DWORD; srcIP: DWORD; pMacAddr: pointer;
  PhyAddrLen: Pointer): DWORD; stdcall; external IPHLPAPI;


function RetrieveMacAddresses(UpAndDown: boolean): TMacAddressDynArray;
var
  n: PtrInt;
  siz: ULONG;
  p: PIP_ADAPTER_ADDRESSES;
  tmp: RawByteString;
begin
  result := nil;
  siz := 0;
  if GetAdaptersAddresses(AF_UNSPEC, GAA_FLAGS, nil, nil, @siz) <> ERROR_BUFFER_OVERFLOW then
    exit;
  SetLength(tmp, siz); // around 600 bytes per interface
  p := pointer(tmp);
  if GetAdaptersAddresses(AF_UNSPEC, GAA_FLAGS, nil, p, @siz) = ERROR_SUCCESS then
  begin
    n := 0;
    repeat
      if (p^.Flags <> 0) and
         (UpAndDown or
          (p^.OperStatus = IfOperStatusUp)) and
         (p^.PhysicalAddressLength = 6) then
      begin
        SetLength(result, n + 1);
        Win32PWideCharToUtf8(
          p^.Description, StrLenW(p^.Description), result[n].Name);
        result[n].Address := MacToText(@p^.PhysicalAddress);
        inc(n);
      end;
      p := p^.Next;
    until p = nil;
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
  dwremoteIP := inet_addr(pointer(IP));
  if dwremoteIP <> 0 then
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
  i: PtrInt;
begin
  i := PtrUIntScanIndex(@FDSet.fd_array, FDSet.fd_count, PtrUInt(Socket));
  if i < 0 then
    exit;
  dec(FDSet.fd_count);
  if i < FDSet.fd_count then
    MoveFast(FDSet.fd_array[i + 1], FDSet.fd_array[i], (FDSet.fd_count - i) * SizeOf(TSocket));
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
  // because the new WSAPoll API is just an emulator usinig select() internally
  TPollSocketSelect = class(TPollSocketAbstract)
  protected
    fHighestSocket: TNetSocket;
    fTagSocket: array of TNetSocket;
    fTagTag: array of TPollSocketTag;
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
  if PtrUInt(socket) > PtrUInt(fHighestSocket) then
    fHighestSocket := socket;
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
    if fCount = 0 then
      fHighestSocket := nil;
    result := true;
  end;
end;

function TPollSocketSelect.WaitForModified(var results: TPollSocketResults;
  timeoutMS: integer): boolean;
var
  tv: TTimeVal;
  rdp, wrp: PFDSet;
  ev: TPollSocketEvents;
  i: PtrInt;
  sock: TNetSocket;
  rd, wr: TFDSet; // modified by select() -> local copy on stack
begin
  result := false; // error
  results.Count := 0;
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
  if select(PtrUInt(fHighestSocket) + 1, rdp, wrp, nil, @tv) <= 0 then
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
      with results.Events[results.Count] do
      begin
        result := true;
        events := ev;
        tag := fTagTag[i];
        inc(results.Count);
      end;
  end;
end;


function PollSocketClass: TPollSocketClass;
begin
  result := TPollSocketSelect;
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
    fSizes: TSecPkgContext_StreamSizes;
    fData, fInput: AnsiString;
    fInputSize, fDataPos, fDataCount, fInputCount: integer;
    fSessionClosed: boolean;
    fAccept: boolean;
    fAcceptCert: PCCERT_CONTEXT;
    fAcceptCertStore: HCERTSTORE;
    procedure ESChannelRaiseLastError(res: cardinal);
    function CheckSEC_E_OK(res: integer): cardinal;
    procedure HandshakeLoop;
  public
    destructor Destroy; override;
    // INetTls methods
    procedure AfterConnection(Socket: TNetSocket;  var Context: TNetTlsContext;
      const ServerAddress: RawUtf8);
    procedure AfterBind(var Context: TNetTlsContext);
    procedure AfterAccept(Socket: TNetSocket; const BoundContext: TNetTlsContext;
      LastError, CipherName: PRawUtf8);
    function GetCipherName: RawUtf8;
    function Receive(Buffer: pointer; var Length: integer): TNetResult;
    function Send(Buffer: pointer; var Length: integer): TNetResult;
  end;


{ TSChannelNetTls }

procedure TSChannelNetTls.ESChannelRaiseLastError(res: cardinal);
var
  LastError: integer;
begin
  LastError := GetLastError;
  raise ESChannel.CreateFmt('<%s>: Result %x [%s], System Error %d ''%s''',
    [fServerAddressW, res, SspiResToText(res), LastError,
     SysErrorMessage(LastError)]);
end;

function TSChannelNetTls.CheckSEC_E_OK(res: integer): cardinal;
begin
  if res <> SEC_E_OK then
    ESChannelRaiseLastError(res);
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

procedure CredInit(var cred: TSChannelCred; AllowDeprecatedTLS: boolean);
begin
  FillCharFast(cred, SizeOf(cred), 0);
  cred.dwVersion := SCHANNEL_CRED_VERSION;
  cred.grbitEnabledProtocols :=
     SP_PROT_TLS1_2_CLIENT or SP_PROT_TLS1_3_CLIENT or
     SP_PROT_TLS1_2_SERVER or SP_PROT_TLS1_3_SERVER;
  if (OSVersion < wSeven) or // XP and Vista only support TLS 1.0 anyway :(
     AllowDeprecatedTls then
    cred.grbitEnabledProtocols := cred.grbitEnabledProtocols or
     SP_PROT_TLS1_CLIENT or SP_PROT_TLS1_1_CLIENT or
     SP_PROT_TLS1_SERVER or SP_PROT_TLS1_1_SERVER;
end;

procedure TSChannelNetTls.AfterConnection(Socket: TNetSocket;
  var Context: TNetTlsContext; const ServerAddress: RawUtf8);
var
  cred: TSChannelCred;
  buf: THandshakeBuf;
  res, f: cardinal;
begin
  // prepare execution context
  fSocket := Socket;
  fServerAddressW := SynUnicode(ServerAddress);
  fAccept := false;
  CredInit(cred, Context.AllowDeprecatedTLS);
  CheckSEC_E_OK(AcquireCredentialsHandleW(
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
    ESChannelRaiseLastError(res);
  if (buf.buf[2].cbBuffer = 0) or
     (buf.buf[2].pvBuffer = nil) then
    raise ESChannel.CreateFmt('Void Hello answer to %s', [ServerAddress]);
  CheckSocket(mormot.net.sock.Send(
    fSocket.Socket, buf.buf[2].pvBuffer, buf.buf[2].cbBuffer, 0));
  CheckSEC_E_OK(FreeContextBuffer(buf.buf[2].pvBuffer));
  // make TLS handshake and prepare for process
  SetLength(fData, TLSRECMAXSIZE);
  HandshakeLoop;
  Context.CipherName := GetCipherName;
end;

procedure TSChannelNetTls.AfterBind(var Context: TNetTlsContext);
var
  certblob: RawByteString;
  blob: TCryptDataBlob;
  pass: SynUnicode;
  flags: integer;
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
      ESChannelRaiseLastError(SEC_E_CERT_UNKNOWN);
  end;
  // find first certificate in store
  fAcceptCert := CertFindCertificateInStore(fAcceptCertStore, 0, 0,
    CERT_FIND_ANY, nil, nil);
  if fAcceptCert = nil then
    raise ESChannel.Create('AfterBind: no Certificate available');
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
  CredInit(cred, BoundContext.AllowDeprecatedTls);
  cred.cCreds := 1;
  cred.paCred := @BoundContext.AcceptCert;
  CheckSEC_E_OK(AcquireCredentialsHandleW(nil, UNISP_NAME, SECPKG_CRED_INBOUND,
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

procedure TSChannelNetTls.HandshakeLoop;

  function ComputeAndSendAnswer: cardinal;
  var
    f: cardinal;
    buf: THandshakeBuf;
    LInCtxPtr: PSecHandle;
  begin
    HandshakeBufInit(buf);
    buf.buf[0].cbBuffer := fDataCount;
    buf.buf[0].pvBuffer := pointer(fData);
    f := 0;
    if fAccept then
    begin
      if (fCtxt.dwLower = 0) and (fCtxt.dwUpper = 0) then
        LInCtxPtr := nil
      else
        LInCtxPtr := @fCtxt;
      result := AcceptSecurityContext(@fCred, LInCtxPtr, @buf.input,
        fFlags, 0, @fCtxt, @buf.output, f, nil);
    end
    else
      result := InitializeSecurityContextW(
        @fCred, @fCtxt, pointer(fServerAddressW), fFlags, 0, 0,
        @buf.input, 0, nil, @buf.output, f, nil);
    //writeln(IntToHex(result, 8),' [',SspiResToText(result),'] tosend=',buf.buf[2].cbBuffer);
    if (result = SEC_E_OK) or
       (result = SEC_I_CONTINUE_NEEDED) or
       ((f and ISC_REQ_EXTENDED_ERROR) <> 0) then
      if (buf.buf[2].cbBuffer <> 0) and
         (buf.buf[2].pvBuffer <> nil) then
      begin
        // need to send back something to the server
        CheckSocket(mormot.net.sock.Send(
          fSocket.Socket, buf.buf[2].pvBuffer, buf.buf[2].cbBuffer, 0));
        CheckSEC_E_OK(FreeContextBuffer(buf.buf[2].pvBuffer));
      end;
    if (buf.buf[1].BufferType = SECBUFFER_EXTRA) and
       (buf.buf[1].cbBuffer <> 0) then
    begin
      // reuse pending bytes - avoid unexpected SEC_E_INVALID_TOKEN
      //writeln('SECBUFFER_EXTRA=',buf.buf[1].cbBuffer,' / ',fDataCount);
      MoveFast(PByteArray(fData)[cardinal(fDataCount) - buf.buf[1].cbBuffer],
               PByteArray(fData)[0], buf.buf[1].cbBuffer);
      fDataCount := buf.buf[1].cbBuffer;
    end
    else if result <> SEC_E_INCOMPLETE_MESSAGE then
      fDataCount := 0;
  end;

var
  res: cardinal;
begin
  repeat
    inc(fDataCount, CheckSocket(
      Recv(fSocket.Socket, @PByteArray(fData)[fDataCount],
        length(fData) - fDataCount, MSG_NOSIGNAL)));
    res := ComputeAndSendAnswer;
    if res = SEC_I_INCOMPLETE_CREDENTIALS then
      // check https://stackoverflow.com/a/47479968/458259
      res := ComputeAndSendAnswer; // run twice to let the handshake pass
  until (res <> SEC_I_CONTINUE_NEEDED) and
        (res <> SEC_E_INCOMPLETE_MESSAGE);
  CheckSEC_E_OK(res);
  // note: fDataCount trailing content may come from SECBUFFER_EXTRA bytes
  fCipherName := ''; // will be retrieved if needed 
  if fInputSize = 0 then
  begin
    // retrieve the TLS field sizes during first handshake
    CheckSEC_E_OK(QueryContextAttributesW(
      @fCtxt, SECPKG_ATTR_STREAM_SIZES, @fSizes));
    fInputSize := fSizes.cbHeader + fSizes.cbMaximumMessage + fSizes.cbTrailer;
    if (fInputSize = 0) or
       (fInputSize > TLSRECMAXSIZE) then
      raise ESChannel.CreateFmt('InputSize=%d>%d', [fInputSize, TLSRECMAXSIZE]);
    SetLength(fInput, fInputSize);
    fInputCount := 0;
  end;
end;

destructor TSChannelNetTls.Destroy;
var
  desc: TSecBufferDesc;
  buf: TSecBuffer;
  dt, f: cardinal;
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
        begin
          if AcceptSecurityContext(@fCred, @fCtxt, nil, fFlags, 0,
             nil, @desc, f, nil) = SEC_E_OK then
          begin
            mormot.net.sock.Send(fSocket.Socket, buf.pvBuffer, buf.cbBuffer, 0);
            FreeContextBuffer(buf.pvBuffer);
          end;
        end
        else
        begin
          if InitializeSecurityContextW(@fCred, @fCtxt, nil, fFlags, 0,
             0, nil, 0, @fCtxt, @desc, f, nil) = SEC_E_OK then
          begin
            mormot.net.sock.Send(fSocket.Socket, buf.pvBuffer, buf.cbBuffer, 0);
            FreeContextBuffer(buf.pvBuffer);
          end;
        end;
      end;
    end;
    // finalize TLS context for this connection
    DeleteSecurityContext(@fCtxt);
    FreeCredentialsHandle(@fCred);
  finally
    // release AfterBind() certificates information
    if fAcceptCert <> nil then
      CertFreeCertificateContext(fAcceptCert);
    if fAcceptCertStore <> nil then
      CertCloseStore(fAcceptCertStore, CERT_CLOSE_STORE_DEFAULT);
    inherited Destroy;
  end;
end;

function TSChannelNetTls.Receive(Buffer: pointer; var Length: integer): TNetResult;
var
  desc: TSecBufferDesc;
  buf: array[0..3] of TSecBuffer;
  res: cardinal;
  read, i, newlen: integer;
  needsRenegotiate: boolean;

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
      read := Recv(fSocket.Socket, @PByteArray(fInput)[fInputCount],
        fInputSize - fInputCount, MSG_NOSIGNAL);
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
    needsRenegotiate := false;
    repeat
      case res of
        SEC_I_RENEGOTIATE:
          needsRenegotiate := true;
        SEC_I_CONTEXT_EXPIRED:
          fSessionClosed := true;
        SEC_E_INCOMPLETE_MESSAGE:
          break;
        else
          CheckSEC_E_OK(res);
      end;
      fInputCount := 0;
      for i := 1 to 3 do
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
      if fInputCount = 0 then
        break;
      res := DecryptInput;
    until false;
    if needsRenegotiate then
      HandshakeLoop;
  except
    result := nrFatalError;
    exit; // shutdown the connection on ESChannel fatal error
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
      exit; // shutdown the connection on SChannel error
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



function NewSChannelNetTls: INetTls;
begin
  result := TSChannelNetTls.Create;
end;

var
  WsaDataOnce: TWSADATA;

procedure InitializeUnit;
begin
  WSAStartup($0202, WsaDataOnce);
  SocketAPIVersion := RawUtf8(Format(
    '%s.%d', [WsaDataOnce.szDescription, WsaDataOnce.wVersion]));
  NewNetTls := NewSChannelNetTls;
end;

procedure FinalizeUnit;
begin
  WSACleanup;
end;

