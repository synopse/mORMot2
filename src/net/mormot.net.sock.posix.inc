{
  This file is a part of the Open Source Synopse mORMot framework 2,
  licensed under a MPL/GPL/LGPL three license - see LICENSE.md

  POSIX Sockets API calls via the libc, as used by mormot.net.sock.pas
}

uses
  baseunix,
  termio,
  sockets,
  {$ifdef OSLINUXANDROID}
  linux,   // for epoll
  {$endif OSLINUXANDROID}
  initc; // link clib='c' for our socket purpose

{$ifdef OSLINUX}
  {$define HASACCEPT4} // use Linux-specific accept4() enhanced API call
  // define OLDLIBC conditional to disable this feature and fallback to accept()
{$endif OSLINUX}

{$ifdef OSBSDDARWIN}
  {$define USEPOLL} // epoll API is Linux-specific -> force use poll() on BSD
{$endif OSBSDDARWIN}

{.$define USEPOLL} // to validate poll API on Linux


{ ******** Sockets Type Definitions }

const
  host_file = '/etc/hosts';

  NI_MAXHOST = 1025;
  NI_MAXSERV = 32;

  WSATRY_AGAIN     = ESysEAGAIN;
  WSAEADDRNOTAVAIL = ESysEADDRNOTAVAIL;
  WSAECONNABORTED  = ESysECONNABORTED;
  WSAECONNRESET    = ESysECONNRESET;
  WSAETIMEDOUT     = ESysETIMEDOUT;
  WSAEINVAL        = ESysEINVAL;
  WSAEMFILE        = ESysEMFILE;
  WSAECONNREFUSED  = ESysECONNREFUSED;
  WSAEINPROGRESS   = ESysEINPROGRESS;

  {$ifdef OSLINUX}
  SOCK_NONBLOCK = O_NONBLOCK; // see net.h
  {$endif OSLINUX}

  {$ifdef OSLINUXANDROID}
  SO_REUSEPORT = 15; // available since Kernel 3.9 - not defined by FPC yet
  {$endif OSLINUXANDROID}

  {$ifdef OSFREEBSD}
  NI_NUMERICHOST = 2;
  NI_NUMERICSERV = 8;
  {$else}
  NI_NUMERICHOST = 1;
  NI_NUMERICSERV = 2;
  {$endif OSFREEBSD}

  FIONREAD = termio.FIONREAD;
  FIONBIO  = termio.FIONBIO;

  {$ifdef OSBSDDARWIN}

    {$ifndef OSOPENBSD}
    {$ifdef OSDARWIN}
    SO_NOSIGPIPE = $1022;
    {$else}
    SO_NOSIGPIPE = $800;
    {$endif OSDARWIN}
    {$endif OSOPENBSD}

    {$ifndef OSOPENBSD}
    // Works under MAC OS X and FreeBSD, but is undocumented, so FPC doesn't include it
    MSG_NOSIGNAL  = $20000;  // Do not generate SIGPIPE.
    {$else}
    MSG_NOSIGNAL  = $400;
    {$endif OSOPENBSD}
  {$else}

    {$ifdef SUNOS}
    MSG_NOSIGNAL = $20000;  // Do not generate SIGPIPE.
    {$else}
    MSG_NOSIGNAL = sockets.MSG_NOSIGNAL; // Do not generate SIGPIPE
    {$endif SUNOS}

  {$endif OSBSDDARWIN}

  _ST: array[TNetLayer] of integer = (
    SOCK_STREAM,
    SOCK_DGRAM,
    SOCK_STREAM);
  _IP: array[TNetLayer] of integer = (
    IPPROTO_TCP,
    IPPROTO_UDP,
    0);
  _SF: array[TNetFamily] of integer = (
    0,
    AF_INET,
    AF_INET6,
    AF_UNIX);

{$packrecords C}

type
  /// a socket is always cint = 32-bit integer sized on POSIX
  // - we extend it to PtrInt for our TNetSocketWrap cast
  TSocket = PtrInt;

  PSockAddrIn6 = ^sockaddr_in6;

  THostEnt = record
    h_name: PAnsiChar;
    h_aliases: PPAnsiChar;
    h_addrtype: integer;
    h_length: socklen_t;
    case integer of
       0: (
          h_addr_list: PPAnsiChar);
       1: (
          h_addr: ^pin_addr);
       2: (
          h_addr6: ^pin6_addr);
  end;
  PHostEnt = ^THostEnt;

const
  // poll() flag when there is data to read
  POLLIN       = $0001;
  // poll() flag when there is urgent data to read
  POLLPRI      = $0002;
  // poll() flag when writing now will not block
  POLLOUT      = $0004;
  // poll() flag error condition (always implicitly polled for)
  POLLERR      = $0008;
  // poll() flag hung up (always implicitly polled for)
  POLLHUP      = $0010;
  // poll() flag invalid polling request (always implicitly polled for)
  POLLNVAL     = $0020;
  // poll() flag when normal data may be read
  POLLRDNORM   = $0040;
  // poll() flag when priority data may be read
  POLLRDBAND   = $0080;
  // poll() flag when writing now will not block
  POLLWRNORM   = $0100;
  // poll() flag when priority data may be written
  POLLWRBAND   = $0200;
  // poll() flag extension for Linux
  POLLMSG      = $0400;

type
  /// polling request data structure for poll()
  TPollFD = record
    /// file descriptor to poll
    fd: integer;
    /// types of events poller cares about
    // - mainly POLLIN and/or POLLOUT
    events: Smallint;
    /// types of events that actually occurred
    // - caller could just reset revents := 0 to reuse the structure
    revents: Smallint;
  end;
  PPollFD = ^TPollFD;


{ ******** Sockets API calls via the libc }

// note: POSIX sockets are fd = C int = 32-bit integer, not our TSocket = PtrInt
//       so we use plain integer in the Sockets API calls below

{$ifdef OLDLIBC}
  {$undef HASACCEPT4}
{$endif OLDLIBC}

{.$define DIRECTSYSCALL} // no noticeable performance or stability change

{$ifdef SOCKDIRECTSYSCALL}

function gethostbyname(name: PAnsiChar): PHostEnt; cdecl;
  external clib name 'gethostbyname' + LIBC_SUFFIX;
  // no syscall available from FPC -> use libc wrapper

function socket(af, struct, protocol: integer): integer; inline;
begin
  result := fpsocket(af, struct, protocol);
end;

function setsockopt(s: integer; level, optname: integer;
   optval: pointer; optlen: integer): integer; inline;
begin
  result := fpsetsockopt(s, level, optname, optval, optlen);
end;

function getsockopt(s: integer; level, optname: integer;
   optval: pointer; optlen: pSocklen): integer; inline;
begin
  result := fpgetsockopt(s, level, optname, optval, optlen);
end;

function ioctlsocket(s: integer; cmd: cardinal; arg: PCardinal): integer; inline;
begin
  result := fpioctl(s, cmd, arg);
end;

function shutdown(s: integer; how: integer): integer; inline;
begin
  result := fpshutdown(s, how);
end;

function closesocket(s: integer): integer; inline;
begin
  result := fpclose(s);
end;

function getnameinfo(addr: PSockAddr; namelen: TSockLen; host: PAnsiChar;
   hostlen: TSockLen; serv: PAnsiChar; servlen: TSockLen; flags: integer): integer; cdecl;
  external clib name 'getnameinfo' + LIBC_SUFFIX;
  // no syscall available from FPC

function bind(s: integer; addr: PSockAddr; namelen: TSockLen): integer; inline;
begin
  result := fpbind(s, addr, namelen);
end;

function getsockname(s: integer; name: PSockAddr; var namelen: TSockLen): integer; inline;
begin
  result := fpgetsockname(s, name, @namelen);
end;

function listen(s: integer; backlog: integer): integer; inline;
begin
  result := fplisten(s, backlog);
end;

function accept(s: integer; addr: PSockAddr; var addrlen: TSockLen): integer; inline;
begin
  result := fpaccept(s, addr, @addrlen);
end;

{$ifdef HASACCEPT4}
function accept4(s: integer; addr: PSockAddr; var addrlen: TSockLen; flags: integer): integer; cdecl;
  external clib name 'accept4';
  // no syscall available from FPC
{$endif HASACCEPT4}

function connect(s: integer; name: PSockAddr; namelen: TSockLen): integer; inline;
  begin
    result := fpconnect(s, name, namelen);
  end;

function select(nfds: integer; readfds, writefds, exceptfds: PFDSet;
   timeout: PTimeVal): integer; inline;
begin
  result := fpselect(nfds, readfds, writefds, exceptfds, timeout);
end;

function recv(s: integer; Buf: pointer; len: size_t; flags: integer): ssize_t; inline;
begin
  result := fprecv(s, Buf, len, flags);
end;

function recvfrom(s: integer; Buf: pointer; len: size_t; flags: integer;
   from: PSockAddr; fromlen: pSocklen): ssize_t; inline;
begin
 result := fprecvfrom(s, Buf, len, flags, from, fromlen);
end;

function send(s: integer; Buf: pointer; len: size_t; flags: integer): ssize_t; inline;
begin
  result := fpsend(s, Buf, len, flags);
end;

function sendto(s: integer; Buf: pointer; len: size_t; flags: integer;
   addrto: PSockAddr; tolen: integer): ssize_t; inline;
begin
 result := fpsendto(s, Buf, len, flags, addrto, tolen);
end;

function getpeername(s: integer; name: PSockAddr; var namelen: TSockLen): integer; inline;
begin
  result := fpgetpeername(s, name, @namelen);
end;

{$else}

// we use the libc rather than fp*() low-level functions because the libc
// may be implemented via vDSO and avoid a syscall, and some high-level API
// like gethostbyname() or accept4() are not available as fp* syscalls

function gethostbyname(name: PAnsiChar): PHostEnt; cdecl;
  external clib name 'gethostbyname' + LIBC_SUFFIX;

function socket(af, struct, protocol: integer): integer; cdecl;
  external clib name 'socket' + LIBC_SUFFIX;

function setsockopt(s: integer; level, optname: integer;
   optval: pointer; optlen: integer): integer; cdecl;
  external clib name 'setsockopt' + LIBC_SUFFIX;

function getsockopt(s: integer; level, optname: integer;
   optval: pointer; optlen: PInteger): integer; cdecl;
  external clib name 'getsockopt' + LIBC_SUFFIX;

function ioctlsocket(s: integer; cmd: cardinal): integer; cdecl; varargs;
  external clib name 'ioctl' + LIBC_SUFFIX;

function shutdown(s: integer; how: integer): integer; cdecl;
  external clib name 'shutdown' + LIBC_SUFFIX;

function closesocket(s: integer): integer; cdecl;
  external clib name 'close' + LIBC_SUFFIX;

function getnameinfo(addr: PSockAddr; namelen: TSockLen; host: PAnsiChar;
   hostlen: TSockLen; serv: PAnsiChar; servlen: TSockLen; flags: integer): integer; cdecl;
  external clib name 'getnameinfo' + LIBC_SUFFIX;

function bind(s: integer; addr: PSockAddr; namelen: TSockLen): integer; cdecl;
  external clib name 'bind' + LIBC_SUFFIX;

function getsockname(s: integer; name: PSockAddr; var namelen: TSockLen): integer; cdecl;
  external clib name 'getsockname' + LIBC_SUFFIX;

function listen(s: integer; backlog: integer): integer; cdecl;
  external clib name 'listen' + LIBC_SUFFIX;

function accept(s: integer; addr: PSockAddr; var addrlen: TSockLen): integer; cdecl;
  external clib name 'accept' + LIBC_SUFFIX;

{$undef ACCEPT4_SYSCALL}

{$ifdef HASACCEPT4}

function accept4(s: integer; addr: PSockAddr; var addrlen: TSockLen; flags: integer): integer;
  cdecl; external clib name 'accept4'  // no LIBC_SUFFIX: libc 2.10 is required
    {$ifdef OSLINUXINTEL} + '@GLIBC_2.10' {$endif} ;

{$endif HASACCEPT4}

function connect(s: integer; name: PSockAddr; namelen: TSockLen): integer; cdecl;
  external clib name 'connect' + LIBC_SUFFIX;

function select(nfds: integer; readfds, writefds, exceptfds: PFDSet;
   timeout: PTimeVal): integer; cdecl;
  external clib name 'select' + LIBC_SUFFIX;

function recv(s: integer; Buf: pointer; len: size_t; flags: integer): ssize_t; cdecl;
  external clib name 'recv' + LIBC_SUFFIX;

function recvfrom(s: integer; Buf: pointer; len: size_t; flags: integer;
   from: PSockAddr; fromlen: Pinteger): ssize_t; cdecl;
  external clib name 'recvfrom' + LIBC_SUFFIX;

function send(s: integer; Buf: pointer; len: size_t; flags: integer): ssize_t; cdecl;
  external clib name 'send' + LIBC_SUFFIX;

function sendto(s: integer; Buf: pointer; len: size_t; flags: integer;
   addrto: PSockAddr; tolen: integer): ssize_t; cdecl;
  external clib name 'sendto' + LIBC_SUFFIX;

function getpeername(s: integer; name: PSockAddr; var namelen: TSockLen): integer; cdecl;
  external clib name 'getpeername' + LIBC_SUFFIX;

{$endif SOCKDIRECTSYSCALL}

{$ifdef OSDARWIN2}
// circumvent linking issue on Mac OSX
function poll(fds: PPollFD; nfds, timeout: integer): integer; inline;
begin
  result := fppoll(pointer(fds), nfds, timeout);
end;
{$else}
function poll(fds: PPollFD; nfds, timeout: integer): integer; cdecl;
  external clib name 'poll' + LIBC_SUFFIX;
{$endif OSDARWIN}

function RawSocketErrNo: integer;
begin
  result := cerrno; // thread-locale variable from libc
end;

{$ifdef HASACCEPT4}
var
  Accept4Unsupported: boolean;
{$endif HASACCEPT4}

function doaccept(s: TSocket; addr: PSockAddr; var async: boolean): integer;
var
  len: TSockLen;
begin
  len := SOCKADDR_SIZE;
  {$ifdef HASACCEPT4}
  if async and
     not Accept4Unsupported then
  begin
    // see https://github.com/eudev-project/eudev/issues/7#issuecomment-10502344
    result := accept4(s, addr, len, SOCK_NONBLOCK);
    if (result < 0) and
       ((cerrno = ESysENOSYS) or
        (cerrno = ESysEOPNOTSUPP)) then
    begin
      Accept4Unsupported := true; // will fallback from now on on old systems
      len := SOCKADDR_SIZE;
      result := accept(s, addr, len);
    end
    else
      async := false; // no need to call MakeAsync
  end
  else
  {$endif HASACCEPT4}
    result := accept(s, addr, len);
end;


{ ******** TNetSocket Cross-Platform Wrapper }

{ TNetAddr }

var
  _gethostbynamesafe: TOSLightLock;
  // gethostbyname() result is NOT thread-safe: we need to protect its calls;
  // since this may take some time, we use a pthread mutex, not a TLightLock

function TNetAddr.SetFrom(const address, addrport: RawUtf8;
  layer: TNetLayer): TNetResult;
var
  h: PHostEnt;
  addr4: sockaddr absolute Addr;
begin
  FillCharFast(Addr, SizeOf(Addr), 0);
  result := nrNotFound;
  if address = '' then
    exit;
  // handle domain socket name
  if layer = nlUnix then
  begin
    if length(address) >= SizeOf(psockaddr_un(@Addr)^.sun_path) then
      // avoid buffer overflow or truncated socket name
      result := nrFatalError
    else
      // socket file name is just copied to the address struct
      with psockaddr_un(@Addr)^ do
      begin
        sun_family := AF_UNIX;
        MoveFast(pointer(address)^, sun_path, length(address));
        result := nrOK;
      end;
    exit;
  end;
  // check supplied IP port
  addr4.sin_port := htons(GetCardinal(pointer(addrport)));
  if (addr4.sin_port = 0) and
     (addrport <> '0') then // explicit '0' to get ephemeral port
    exit;
  // check most simple IPv4 resolution (maybe using mormot.net.dns)
  result := nrOk;
  if SetFromIP4(address, false) then
    exit;
  // use OS API (getaddrinfo is more recent, but addrinfo is not cross-platform)
  _gethostbynamesafe.Lock;
  try
    h := gethostbyname(pointer(address));
    if h = nil then
      result := nrNotFound
    else
    begin
      addr4.sin_family := h^.h_addrtype;
      case h^.h_addrtype of
        AF_INET:
           addr4.sin_addr := h^.h_addr^^;
        AF_INET6:
           psockaddr6(@Addr)^.sin6_addr := h^.h_addr6^^;
      else
        result := nrNotImplemented;
      end;
    end;
  finally
    _gethostbynamesafe.UnLock;
  end;
end;


{ TNetSocketWrap }

procedure SetTimeVal(ms: PtrUInt; out tv: TTimeVal);
var
  d: PtrUInt;
begin
  if ms = 0 then
  begin
    tv.tv_sec := 0;
    tv.tv_usec := 0;
  end
  else
  begin
    d := ms div 1000;
    tv.tv_sec := d;
    tv.tv_usec := (ms - (d * 1000)) * 1000;
  end;
end;

procedure TNetSocketWrap.ReusePort;
var
  v: integer;
begin
  // see https://lwn.net/Articles/542629 for Linux (available since Kernel 3.9)
  v := ord(true);
  if @self <> nil then
    setsockopt(TSocket(@self), SOL_SOCKET, SO_REUSEPORT, @v, SizeOf(v));
  // don't call SetOpt() which raise ENetSock on old kernel without SO_REUSEPORT
end;

procedure TNetSocketWrap.SetSendTimeout(ms: integer);
var
  tv: TTimeVal; // POSIX uses a timeval (not Windows)
begin
  SetTimeVal(ms, tv);
  SetOpt(SOL_SOCKET, SO_SNDTIMEO, @tv, SizeOf(tv));
end;

procedure TNetSocketWrap.SetReceiveTimeout(ms: integer);
var
  tv: TTimeVal;
begin
  SetTimeVal(ms, tv);
  SetOpt(SOL_SOCKET, SO_RCVTIMEO, @tv, SizeOf(tv));
end;

procedure TNetSocketWrap.SetReuseAddrPort;
var
  v: integer;
begin
  v := ord(true);
  {$ifdef OSBSDDARWIN}
  SetOpt(SOL_SOCKET, SO_REUSEPORT, @v, SizeOf(v));
  {$ifndef OSOPENBSD}
  SetOpt(SOL_SOCKET, SO_NOSIGPIPE, @v, SizeOf(v));
  {$endif OSOPENBSD}
  {$else}
  SetOpt(SOL_SOCKET, SO_REUSEADDR, @v, SizeOf(v));
  {$endif OSBSDDARWIN}
end;

procedure TNetSocketWrap.SetLinger(linger: integer);
var
  v: TLinger;
begin
  v.l_linger := linger;
  v.l_onoff := ord(linger >= 0);
  SetOpt(SOL_SOCKET, SO_LINGER, @v, SizeOf(v));
  if linger > 0 then
    SetReuseAddrPort;
end;

procedure TNetSocketWrap.SetCork(cork: boolean);
var
  v: integer;
begin
  v := ord(cork);
  {$ifdef OSLINUXANDROID}
  SetOpt(IPPROTO_TCP, TCP_CORK, @v, SizeOf(v));
  {$else}
  {$ifdef OSBSDDARWIN}
  SetOpt(IPPROTO_TCP, TCP_NOPUSH, @v, SizeOf(v));
  {$endif OSBSDDARWIN}
  {$endif OSLINUXANDROID}
end;

{$ifdef OSLINUXANDROID}
procedure TNetSocketWrap.SetLowPriority;
var
  v: integer;
begin
  v := 0; // lowest priority
  SetOpt(SOL_SOCKET, SO_PRIORITY, @v, SizeOf(v));
end;
{$else}
procedure TNetSocketWrap.SetLowPriority;
begin
end;
{$endif OSLINUXANDROID}

function TNetSocketWrap.HasLowPriority: boolean;
begin
  result := false;
end;

function TNetSocketWrap.WaitFor(ms: integer; scope: TNetEvents;
  loerr: system.PInteger): TNetEvents;
var
  res: integer;
  p: TPollFD; // select() limits p.fd<FD_SETSIZE=1024 on POSIX -> use poll()
  // https://moythreads.com/wordpress/2009/12/22/select-system-call-limitation
begin
  if loerr <> nil then
    loerr^ := 0;
  result := [neError];
  if @self = nil then
    exit;
  p.fd := TSocket(@self);
  p.events := 0;
  if neRead in scope then
    p.events := POLLIN or POLLPRI;
  if neWrite in scope then
    p.events := p.events or POLLOUT;
  // POLLERR and POLLHUP are ignored in p.events: implicit [neError]
  p.revents := 0;
  res := poll(@p, 1, ms);
  if res < 0 then
  begin
    res := RawSocketErrNo;
    if loerr <> nil then
      loerr^ := res;
    if res = ESysEAGAIN then
      // timeout, not a true error
      result := [];
    exit;
  end;
  result := [];
  if p.revents and (POLLIN or POLLPRI) <> 0 then
    include(result, neRead);
  if p.revents and POLLOUT <> 0 then
    include(result, neWrite);
  if p.revents and POLLERR <> 0 then
    include(result, neError);
  if p.revents and POLLHUP <> 0 then
    include(result, neClosed); // may return neClosed + neRead e.g. on HTTP/1.0
end;


{ ******************** Mac and IP Addresses Support }

const
  IFF_UP        = $1;
  IFF_BROADCAST = $2;
  IFF_LOOPBACK  = $8;

  {$ifdef OSBSDDARWIN}

  // https://github.com/apple/darwin-xnu/blob/main/bsd/net/if_types.h
  IFT_ETHER    = $06;
  IFT_PPP      = $17;
  IFT_CELLULAR = $ff;

  {$else}

  // https://elixir.bootlin.com/linux/latest/source/include/uapi/linux/if_arp.h
  ARPHRD_ETHER     = 1;
  ARPHRD_PPP       = 512;
  ARPHRD_TUNNEL    = 768;
  ARPHRD_TUNNEL6   = 769;
  ARPHRD_IEEE80211 = 801;

  {$endif OSBSDDARWIN}

{$ifndef OSANDROID}
  // on POSIX, use getifaddrs/freeifaddrs from libc
  {$define USE_IFADDRS}
{$endif OSANDROID}

{$ifdef OSANDROIDARM64}
  {$define USE_IFADDRS}
  // Android BIONIC seems to implement it since Android N (API level 24)
  // https://android.googlesource.com/platform/bionic/+/master/docs/status.md
{$endif OSANDROID}

{$ifdef OSLINUX}
procedure RetrieveEnhancedInfo(var a: TMacAddress);
var
  sysfs: RawUtf8;
  p: PUtf8Char;
  l: PtrInt;
  ip: cardinal;
begin
  // see https://www.kernel.org/doc/Documentation/ABI/testing/sysfs-class-net
  sysfs := NetConcat(['/sys/class/net/', a.Name, '/']);
  a.FriendlyName := TrimU(StringFromFile(sysfs + 'ifalias', {nosize=}true));
  a.Speed := GetCardinal(pointer(StringFromFile(sysfs + 'speed', true)));
  a.Mtu := GetCardinal(pointer(StringFromFile(sysfs + 'mtu', true)));
  // we retrieve the gateway from /proc/net/route
  l := length(a.Name);
  p := pointer(StringFromFile('/proc/net/route', true));
  while p <> nil do
  begin
    if (MemCmp(pointer(p), pointer(a.Name), l) = 0) and
       (p[l] in [#9, ' ']) then
    begin
      inc(p, l + 1);
      if NetGetNextSpaced(p) = '00000000' then // destination=0.0.0.0
      begin
        ip := 0;
        ParseHex(pointer(NetGetNextSpaced(p)), @ip, 4);
        ip := bswap32(ip);
        IP4Text(@ip, a.Gateway);
        break;
      end;
    end;
    p := GotoNextLine(p);
  end;
end;
{$endif OSLINUX}

{$ifdef USE_IFADDRS}

type
  pifaddrs = ^ifaddrs;
  ifaddrs = record
    // Next item in list
    ifa_next: pifaddrs;
    // null-terminated interface name
    ifa_name: PAnsiChar;
    // mainly IFF_UP, IFF_BROADCAST and IFF_LOOPBACK
    ifa_flags: cardinal;
    // Address of interface
    ifa_addr: Psockaddr;
    // netmask associated with ifa_addr (may be nil)
    ifa_netmask: Psockaddr;
    // if IFF_BROADCAST is set, the broadcast address associated with ifa_addr
    ifa_broadaddr: Psockaddr;
    // address-family-specific data - e.g. statistics on AF_PACKET
    ifa_data: pointer;
  end;

  {$ifdef OSBSDDARWIN} // from bsd/net/if_dl.h
  psockaddr_dl = ^sockaddr_dl;
  sockaddr_dl = record
    sdl_len: byte;       // Total length of sockaddr
    sdl_family: byte;    // AF_LINK
    sdl_index: word;     // if != 0, system given index for interface
    sdl_type: byte;      // interface type
    sdl_nlen: byte;      // interface name length, no trailing 0 required
    sdl_alen: byte;      // link level address length
    sdl_slen: byte;      // link layer selector length
    sdl_data: array[byte] of byte; // interface name + link level address
  end;
  {$else}
  psockaddr_ll = ^sockaddr_ll;
  sockaddr_ll = record
    sll_family: word;
    sll_protocol: word;
    sll_ifindex: integer;
    sll_hatype: word;
    sll_pkttype: byte;
    sll_halen: byte;
    sll_addr: array[byte] of byte;
  end;
  {$endif OSBSDDARWIN}

function getifaddrs(var ifap: pifaddrs): integer; cdecl;
  external clib name 'getifaddrs';

procedure freeifaddrs(ifap: pifaddrs); cdecl;
  external clib name 'freeifaddrs';

function GetIPAddresses(Kind: TIPAddress): TRawUtf8DynArray;
var
  list, info: pifaddrs;
  n: PtrInt;
  s: RawUtf8;
begin
  result := nil;
  n := 0;
  if getifaddrs(list) = NO_ERROR then
  try
    info := list;
    repeat
      if (info^.ifa_addr <> nil) and
         (info^.ifa_flags and IFF_LOOPBACK = 0) and
         (info^.ifa_flags and IFF_UP <> 0) then
      begin
        s := '';
        case info^.ifa_addr^.sa_family of
          AF_INET:
            if IP4Filter(integer(info^.ifa_addr^.sin_addr), Kind) then
              IP4Text(@info^.ifa_addr^.sin_addr,s);
          AF_INET6:
            if kind in [tiaAny, tiaIPv6] then
              with PSockAddrIn6(info^.ifa_addr)^ do
                if sin6_scope_id = 0 then // don't include scoped IPv6
                  IP6Text(@sin6_addr, s);
        end;
        if s <> '' then
        begin
          if n = length(result) then
            SetLength(result, NextGrow(n));
          result[n] := s;
          inc(n);
        end;
      end;
      info := info^.ifa_next;
    until info = nil;
  finally
    freeifaddrs(list);
  end;
  if n <> length(result) then
    SetLength(result, n);
end;

function RetrieveMacAddresses(UpAndDown: boolean): TMacAddressDynArray;
var
  list, info: pifaddrs;
  i, n: PtrInt;
  mac: RawUtf8;
begin
  // https://github.com/zeromq/czmq/blob/899f81985961513c7f4e/src/ziflist.c#L210
  result := nil;
  if getifaddrs(list) = NO_ERROR then
  try
    n := 0;
    info := list;
    repeat
      mac := '';
      if (info^.ifa_name <> nil) and
         (info^.ifa_flags and IFF_LOOPBACK = 0) and
         (UpAndDown or
          (info^.ifa_flags and IFF_UP <> 0)) and
         ((info^.ifa_addr = nil) or
      // see https://stackoverflow.com/a/51218583/458259
      {$ifdef OSBSDDARWIN}
          (info^.ifa_addr^.sa_family = AF_LINK)) then
      begin
        if info^.ifa_addr <> nil then
          with psockaddr_dl(info^.ifa_addr)^ do
            if sdl_alen = 6 then
              // #define LLADDR(s) ((caddr_t)((s)->sdl_data + (s)->sdl_nlen))
              {$ifdef OSDARWIN}
              if PCardinal(@sdl_data[sdl_nlen])^ <> 2 then
                // iOS7+ return always 02:00:00:00:00:00 as MAC addresses :(
              {$endif OSDARWIN}
                mac := MacToText(@sdl_data[sdl_nlen]);
      {$else}
          (info^.ifa_addr^.sa_family = AF_PACKET)) then
      begin
        if info^.ifa_addr <> nil then
          mac := MacToText(@psockaddr_ll(info^.ifa_addr)^.sll_addr);
      {$endif OSBSDDARWIN}
        if mac = '' then
          mac := '00:00:00:00:00:00' // for makSoftware (e.g. tunnel)
        else
          for i := 0 to n - 1 do
            if result[i].Address = mac then
            begin
              mac := ''; // avoid any duplicate (paranoid)
              break;
            end;
        if mac <> '' then
        begin
          if n = length(result) then
            SetLength(result, NextGrow(n));
          with result[n] do
          begin
            Name := info^.ifa_name; // e.g. 'eth0' on Linux
            AdapterName := Name;
            Address := mac;
            if info^.ifa_addr <> nil then
            begin
              {$ifdef OSBSDDARWIN}
              // BSD/DARWIN do name the networks from their manufacturer
              // -> parse net.wlan.devices ?
              IfIndex := psockaddr_dl(info^.ifa_addr)^.sdl_index;
              case psockaddr_dl(info^.ifa_addr)^.sdl_type of
                IFT_ETHER:
                  Kind := makEthernet;
                IFT_PPP:
                  Kind := makPpp;
                IFT_CELLULAR:
                  Kind := makCellular;
              {$else}
              IfIndex := psockaddr_ll(info^.ifa_addr)^.sll_ifindex;
              case psockaddr_ll(info^.ifa_addr)^.sll_hatype of
                ARPHRD_ETHER:
                  begin
                    Kind := makEthernet;
                    if Name <> '' then
                      // set also for some wifi adapters
                      if PWord(Name)^ = ord('w') + ord('l') shl 8 then
                        Kind := makWifi; // detect by convention
                  end;
                ARPHRD_IEEE80211:
                  Kind := makWifi;
                ARPHRD_TUNNEL,
                ARPHRD_TUNNEL6:
                  Kind := makTunnel;
                ARPHRD_PPP:
                  Kind := makPpp;
            {$endif OSBSDDARWIN}
              else
                // https://systemd.io/PREDICTABLE_INTERFACE_NAMES
                if Name <> '' then
                  case PWord(Name)^ of
                    ord('e') + ord('t') shl 8,  // 'ethX'
                    ord('e') + ord('n') shl 8:  // systemd v197 'enpXsX'
                      Kind := makEthernet;
                    ord('w') + ord('l') shl 8:  // 'wlanX' and systemd v197 'wlX'
                      Kind := makWifi;
                  end;
              end;
            end
            else
            begin
              IfIndex := -1; // non physical interface
              Kind := makSoftware;
            end
          end;
          {$ifdef OSLINUX}
          if info^.ifa_flags and IFF_UP <> 0 then
            RetrieveEnhancedInfo(result[n]);
          {$endif OSLINUX}
          // on POSIX, there is no such things as per-interface DNS, and
          // no standard way of knowing the Gateway
          inc(n);
        end;
      end;
      info := info^.ifa_next;
    until info = nil;
    SetLength(result, n);
    // retrieve the IP configuration from a second loop
    info := list;
    repeat
      if (info^.ifa_addr <> nil) and
         (info^.ifa_flags and IFF_LOOPBACK = 0) and
         (info^.ifa_flags and IFF_UP <> 0) and
         (info^.ifa_addr^.sa_family = AF_INET) then
        for i := 0 to n - 1 do
          with result[i] do
            if StrComp(pointer(Name), info^.ifa_name) = 0 then
            begin
              if IP = '' then // only return the first IPv4 (paranoid)
              begin
                IP4Text(@info^.ifa_addr^.sin_addr, IP);
                if (info^.ifa_netmask <> nil) and
                   (info^.ifa_netmask^.sa_family = AF_INET) then
                  IP4Text(@info^.ifa_netmask^.sin_addr, NetMask);
                if (info^.ifa_flags and IFF_BROADCAST <> 0) and
                   (info^.ifa_broadaddr <> nil) and
                   (info^.ifa_broadaddr^.sa_family = AF_INET) then
                  IP4Text(@info^.ifa_broadaddr^.sin_addr, Broadcast);
                  // don't use IP4Broadcast() because if the OS says there is
                  // no broadcast address, then there is no broadcasting enabled
              end;
              break;
            end;
      info := info^.ifa_next;
    until info = nil;
  finally
    freeifaddrs(list);
  end;
end;

{$else}

function GetIPAddresses(Kind: TIPAddress): TRawUtf8DynArray;
begin
  result := nil;
end;

{$ifdef OSLINUX}
// on Android, /sys/class/net is not readable from the standard user :(

function RetrieveMacAddresses(UpAndDown: boolean): TMacAddressDynArray;
var
  n: integer;
  SR: TSearchRec;
  fn: TFileName;
  f: RawUtf8;
begin
  result := nil;
  if FindFirst('/sys/class/net/*', faDirectory, SR) = 0 then
  begin
    n := 0;
    repeat
      if (SR.Name <> 'lo') and
         SearchRecValidFolder(SR) then
      begin
        fn := '/sys/class/net/' + SR.Name;
        f := StringFromFile(fn + '/flags', {nosize=}true);
        if (length(f) > 2) and // e.g. '0x40' or '0x1043'
           (ParseHex0x(pointer(f)) and (IFF_UP or IFF_LOOPBACK) = IFF_UP) then
        begin
          f := TrimU(StringFromFile(fn + '/address', {nosize=}true));
          if f <> '' then
          begin
            SetLength(result, n + 1);
            result[n].Name := SR.Name;
            result[n].Address := f;
            RetrieveEnhancedInfo(result[n]);
            // sadly, retrieving IP and mask from /proc is very complex
            // https://stackoverflow.com/a/14725655/458259
            // a solution may be to use ioctl()
            // https://github.com/zeromq/czmq/blob/899f81985961513c7f4e/src/ziflist.c#L277
            inc(n);
          end;
        end;
      end;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
end;

{$else}

function RetrieveMacAddresses(UpAndDown: boolean): TMacAddressDynArray;
begin
  result := nil;
end;

{$endif OSLINUX}

{$endif USE_IFADDRS}

function GetLocalMacAddress(const Remote: RawUtf8; var Mac: TMacAddress): boolean;
var
  local: RawUtf8;
  all: TMacAddressDynArray;
  i: PtrInt;
begin
  result := false;
  local := GetLocalIpAddress(Remote);
  if local = '' then
    exit;
  all := GetMacAddresses;
  for i := 0 to high(all) do
    if all[i].IP = local then
    begin
      mac := all[i];
      result := true;
      break;
    end;
end;

procedure NetAddRawUtf8(var Values: TRawUtf8DynArray; const Value: RawUtf8); forward;

function _GetDnsAddresses(usePosixEnv, getAD: boolean): TRawUtf8DynArray;
var
  resolv, one: RawUtf8;
  p: PUtf8Char;
  ok: boolean;
begin
  result := nil;
  if usePosixEnv then
    resolv := StringFromFile(GetEnvironmentVariable('ETC'), true);
  if resolv = '' then
    // all systems are likely to have or generate this file
    // - even the libc seems to read it when implementing gethostname()
    resolv := StringFromFile('/etc/resolv.conf');
  p := pointer(resolv);
  while p <> nil do
  begin
    while p^ in [#9, ' '] do
      inc(p);
    if p^ in [#0, '#'] then
      ok := false
    else if getAD then
      ok := NetStartWith(p, 'SEARCH ') or
            NetStartWith(p, 'DOMAIN ')  // "domain" is used e.g. on MacOS
    else
      ok := NetStartWith(p, 'NAMESERVER ');
    if ok then
    begin
      repeat
        inc(p);
      until p^ <= ' '; // go after search/domain/nameserver keyword
      repeat
        one := NetGetNextSpaced(p);
        if one = '' then
          break;
        NetAddRawUtf8(result, one);
      until false;
    end;
    p := GotoNextLine(p);
  end;
end;


{ ******************** Efficient Multiple Sockets Polling }

function PseToPollEvent(events: TPollSocketEvents): integer; inline;
begin
  if pseRead in events then
    result := POLLIN or POLLPRI
  else
    result := 0;
  if pseWrite in events then
    result := result or POLLOUT;
end;

type
  /// socket polling via the POSIX poll API
  // - direct call of the Linux/POSIX poll() API
  // - note: Windows WSAPoll() API is actually slower than plain Select()
  TPollSocketPoll = class(TPollSocketAbstract)
  protected
    fFD: array of TPollFD; // fd=-1 for ignored fields
    fTags: array of TPollSocketTag;
    fFDCount: integer;
    procedure FDVacuum;
  public
    constructor Create(aOwner: TPollSockets); override;
    function Subscribe(socket: TNetSocket; events: TPollSocketEvents;
      tag: TPollSocketTag): boolean; override;
    function Unsubscribe(socket: TNetSocket): boolean; override;
    function WaitForModified(var results: TPollSocketResults;
      timeoutMS: integer): boolean; override;
  end;

constructor TPollSocketPoll.Create(aOwner: TPollSockets);
begin
  inherited Create(aOwner);
  fMaxSockets := 20000;
end;

function TPollSocketPoll.Subscribe(socket: TNetSocket; events: TPollSocketEvents;
  tag: TPollSocketTag): boolean;
var
  i, new: PtrInt;
  n, e, fd, s: integer;
  p: PPollFD;
begin
  result := false;
  if (self = nil) or
     (socket = nil) or
     (byte(events) = 0) or
     (fCount = fMaxSockets) then
    exit;
  e := PseToPollEvent(events);
  s := PtrUInt(socket);
  new := -1;
  p := pointer(fFD);
  if p <> nil then
    if fFDCount = fCount then
    begin
      // no void entry
      for i := 1 to fFDCount do
        if p^.fd = s then
          // already subscribed
          exit
        else
          inc(p);
    end
    else
      for i := 0 to fFDCount - 1 do
      begin
        fd := p^.fd;
        if fd = s then
          // already subscribed
          exit
        else if fd < 0 then
          // found a void entry
          new := i;
      end;
  if new < 0 then
  begin
    if fFDCount = length(fFD) then
    begin
      // add new entry to the array
      n := NextGrow(fFDCount);
      if n > fMaxSockets then
        n := fMaxSockets;
      SetLength(fFD, n);
      SetLength(fTags, n);
    end;
    new := fFDCount;
    inc(fFDCount);
  end;
  fTags[new] := tag;
  with fFD[new] do
  begin
    fd := TSocket(socket);
    events := e;
    revents := 0;
  end;
  inc(fCount);
  result := true;
end;

procedure TPollSocketPoll.FDVacuum;
var
  n, i: PtrInt;
begin
  n := 0;
  for i := 0 to fFDCount - 1 do
    if fFD[i].fd > 0 then
    begin
      if i <> n then
      begin
        fFD[n] := fFD[i];
        fTags[n] := fTags[i];
      end;
      inc(n);
    end;
  fFDCount := n;
end;

function TPollSocketPoll.Unsubscribe(socket: TNetSocket): boolean;
var
  i: PtrInt;
  s: integer;
  p: PPollFD;
begin
  s := PtrUInt(socket);
  p := pointer(fFD);
  for i := 1 to fFDCount do
    if p^.fd = s then
    begin
      p^.fd := -1;     // mark entry as void and reusable
      p^.revents := 0; // quickly ignored in WaitForModified()
      dec(fCount);
      if fCount <= fFDCount shr 1 then
        FDVacuum; // avoid too many void entries
      result := true;
      exit;
    end
    else
      inc(p);
  result := false;
end;

function PollEventToPse(r: integer): TPollSocketEvents; inline;
begin
  if r = POLLIN then
    byte(result) := byte([pseRead]) // optimized for the most common case
  else
  begin
    byte(result) := 0;
    if r and (POLLIN or POLLPRI) <> 0 then
      include(result, pseRead);
    if r and POLLOUT <> 0 then
      include(result, pseWrite);
    if r and POLLERR <> 0 then
      include(result, pseError);
    if r and POLLHUP <> 0 then
      include(result, pseClosed);
  end;
end;

procedure PollConvert(p: PPollFD; d: PPollSocketResult; n: integer; t: PPollSocketTag);
var
  r: integer;
begin
  repeat
    r := p^.revents;
    if (r <> 0) and
       (p^.fd > 0) then
    begin
      SetRes(d^, t^, PollEventToPse(r));
      inc(d);
      p^.revents := 0; // reset result flags for reuse
    end;
    inc(p);
    inc(t);
    dec(n);
  until n = 0;
end;

function TPollSocketPoll.WaitForModified(var results: TPollSocketResults;
  timeoutMS: integer): boolean;
var
  n: PtrInt;
begin
  result := false; // error
  if (self = nil) or
     (fCount = 0) then
    exit;
  // here fCount=0 would not permit next async subscription
  n := poll(pointer(fFD), fFDCount, timeoutMS);
  results.Count := n;
  if n <= 0 then
    exit;
  if results.Events = nil then
    SetLength(results.Events, n + 4) // optimized for a few items
  else if n > length(results.Events) then
  begin
    results.Events := nil;
    SetLength(results.Events, NextGrow(n + 64)); // more generous allocation
  end;
  PollConvert(pointer(fFD), pointer(results.Events), fFDCount, pointer(fTags));
  result := true;
end;

function PollFewSockets: TPollSocketAbstract;
begin
  result := TPollSocketPoll.Create(nil);
end;

function WaitForSeveral(const Sockets: TPollSocketsSubscribeDynArray;
  var results: TPollSocketResults; timeoutMS: integer): boolean;
var
  n, res: integer;
  fd: array of TPollFD;
  s: PPollSocketsSubscribe;
  p: PPollFD;
  r: PPollSocketResult;
begin
  result := false;
  results.Count := 0;
  n := length(Sockets);
  if n = 0 then
    exit;
  SetLength(fd, n);
  res := 0;
  s := pointer(Sockets);
  p := pointer(fd);
  repeat
    if pseRead in s^.events then
    begin
      p^.fd := PtrUInt(s^.socket);
      p^.events := POLLIN or POLLPRI;
      inc(p);
      inc(res);
    end;
    inc(s);
    dec(n);
  until n = 0;
  if res = 0 then
    exit; // no pseRead to check
  n := poll(pointer(fd), res, timeoutMS);
  if n <= 0 then
    exit;
  results.Count := n;
  SetLength(results.Events, n);
  r := pointer(results.Events);
  s := pointer(Sockets);
  p := pointer(fd);
  repeat
    res := p^.revents;
    if res <> 0 then
    begin
      SetRes(r^, s^.tag, PollEventToPse(res));
      inc(r);
      dec(n);
      if n = 0 then
        break;
    end;
    inc(s);
    inc(p);
  until false;
  result := true;
end;

{$ifdef USEPOLL}

function PollSocketClass: TPollSocketClass;
begin
  result := TPollSocketPoll;
end;

{$else}

type
  /// socket polling via Linux epoll optimized API
  // - not available under Windows or BSD/Darwin
  // - direct call of the epoll API in level-triggered (LT) mode
  // - this class IS thread-safe, as the epoll API itself
  TPollSocketEpoll = class(TPollSocketAbstract)
  protected
    fEPFD: integer;
  public
    constructor Create(aOwner: TPollSockets); override;
    destructor Destroy; override;
    // directly calls epoll's EPOLL_CTL_ADD control interface
    function Subscribe(socket: TNetSocket; events: TPollSocketEvents;
      tag: TPollSocketTag): boolean; override;
    // directly calls epoll's EPOLL_CTL_DEL control interface
    function Unsubscribe(socket: TNetSocket): boolean; override;
    // directly calls epool_wait() function
    function WaitForModified(var results: TPollSocketResults;
      timeoutMS: integer): boolean; override;
    // stop epolling
    procedure Terminate; override;
    // return true for epoll
    class function FollowEpoll: boolean; override;
    // read-only access to the low-level epoll_create file descriptor
    property EPFD: integer
      read fEPFD;
  end;

{$define POLLSOCKETEPOLL} // $ifdef in main mormot.net.sock.pas implementation
// is defined for TPollSocketEpoll.FollowEpoll = true

constructor TPollSocketEpoll.Create(aOwner: TPollSockets);
begin
  inherited Create(aOwner);
  fEPFD := epoll_create(10000); // since 2.6.8 size is ignored
end;

destructor TPollSocketEpoll.Destroy;
begin
  Terminate;
  inherited;
end;

procedure TPollSocketEpoll.Terminate;
begin
  if fEPFD <= 0 then
    exit;
  fpClose(fEPFD);
  fEPFD := -1;
end;

class function TPollSocketEpoll.FollowEpoll: boolean;
begin
  result := true; // TPollSocketEpoll is thread-safe and has no size limit
end;

const
  EPOLLRDHUP = $2000; // since 2.6.17 (not defined in FPC Linux.pp) = close
  // note: EPOLLRDHUP in LT mode needs recv() len=0 so is not worth it
  //                  in ET mode properly reflects connection close
  EPOLLMODE_LT = 0;                     // Level Triggered mode
  EPOLLMODE_ET = EPOLLET or EPOLLRDHUP; // Event Triggered mode + RDHUP
  // "EPOLLET" edge triggered detection is not really consistent with select
  // or poll and do not work as expected e.g. with TTestMultiThreadProcess
  // -> PollForPendingEvents() will use fast O(n*m) MergePendingEvents()
  EPOLLMODE = EPOLLMODE_LT;
  // EPOLLRDHUP is relevant only in ET mode
  EPOLLCLOSE = EPOLLHUP or (EPOLLMODE and EPOLLRDHUP);

function TPollSocketEpoll.Subscribe(socket: TNetSocket; events: TPollSocketEvents;
  tag: TPollSocketTag): boolean;
var
  e: TEPoll_Event;
  err: integer;
begin
  result := false;
  if (self = nil) or
     (socket = nil) or
     (fEPFD <= 0) or
     (PtrUInt(socket) = PtrUInt(fEPFD)) or
     (byte(events) = 0) then
    exit;
  e.data.ptr := pointer(tag); // don't use data.u64
  if pseRead in events then
    if pseWrite in events then
      e.events := EPOLLIN or EPOLLPRI or EPOLLOUT or EPOLLMODE
    else
      e.events := EPOLLIN or EPOLLPRI or EPOLLMODE
  else if pseWrite in events then
    e.events := EPOLLOUT or EPOLLMODE
  else
    e.events := 0;
  // EPOLLERR and EPOLLHUP are always implicitly tracked
  repeat
    err := epoll_ctl(fEPFD, EPOLL_CTL_ADD, TSocket(socket), @e);
  until err <> ESysEINTR;
  result := err >= NO_ERROR;
  if not result then
    exit;
  LockedInc32(@fCount);
  fMaxSockets := fCount;
end;

function TPollSocketEpoll.Unsubscribe(socket: TNetSocket): boolean;
var
  e: TEPoll_Event; // should be there even if not used (old kernel < 2.6.9)
  err: integer;
begin
  if (self = nil) or
     (socket = nil) or
     (fEPFD <= 0) or
     (PtrUInt(socket) = PtrUInt(fEPFD)) then
    result := false
  else
  begin
    repeat
      err := epoll_ctl(fEPFD, EPOLL_CTL_DEL, TSocket(socket), @e);
    until err <> ESysEINTR;
    result := err >= NO_ERROR;
    if result then
    begin
      LockedDec32(@fCount);
      fMaxSockets := fCount;
    end;
    if (fOwner <> nil) and
       fOwner.fUnsubscribeShutdownSocket then
      socket.ShutdownAndClose({rdwr=}false);
  end;
end;

function TPollSocketEpoll.WaitForModified(var results: TPollSocketResults;
  timeoutMS: integer): boolean;
var
  e: TPollSocketEvents;
  n: PtrInt;
  ev: integer;
  ee: PEPoll_Event;
  er: PPollSocketResult;
  tmp: array[byte] of TEPoll_Event; // up to 256 events at once seems fair
begin
  result := false; // error or nothing new
  if (self = nil) or
     (fEPFD <= 0) then
    exit;
  // we allow fCount=0 which enable background epoll_ctl subscription
  n := epoll_wait(fEPFD, @tmp, length(tmp), timeoutMS);
  // if pending > length(tmp): epoll_wait() would round-robin
  results.Count := n;
  if n <= 0 then
    exit; // nothing new
  // epoll notified some sockets
  if results.Events = nil then
    // temporary results.Events = new in PollForPendingEvents()
    SetLength(results.Events, n + 4)
  else if n > length(results.Events) then
  begin
    // results.Events reused between calls: allocate as much as possible
    results.Events := nil;
    SetLength(results.Events, length(tmp));
  end;
  ee := @tmp;
  er := pointer(results.Events);
  repeat
    ev := ee^.events;
    byte(e) := byte([pseRead]); // optimized for the most common case
    if ev <> EPOLLIN then
    begin
      if ev and (EPOLLIN or EPOLLPRI) = 0 then
        byte(e) := 0;
      if ev and EPOLLOUT <> 0 then
        include(e, pseWrite);
      if ev and EPOLLERR <> 0 then
        include(e, pseError);
      if ev and EPOLLCLOSE <> 0 then
        include(e, pseClosed);
    end;
    // inlined forward SetRes(er^, TPollSocketTag(ee^.data.ptr), e)
    {$ifdef CPU32}
    er^.B[4] := byte(e);
    er^.Li := TPollSocketTag(ee^.data.ptr);
    {$else}
    er^ := PtrUInt(ee^.data.ptr) or (PtrUInt(byte(e)) shl 60);
    {$endif CPU32}
    inc(ee);
    inc(er);
    dec(n)
  until n = 0;
  result := true;
end;

function PollSocketClass: TPollSocketClass;
begin
  result := TPollSocketEpoll;
end;

{$endif USEPOLL}



procedure InitializeUnit;
begin
  SocketAPIVersion := OSVersionShort +
    {$ifdef USEPOLL} ' poll' {$else} ' epoll' {$endif USEPOLL};
  _gethostbynamesafe.Init;
end;

procedure FinalizeUnit;
begin
  _gethostbynamesafe.Done;
end;

