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
  linux, // for epoll
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
  NI_MAXHOST = 1025;
  NI_MAXSERV = 32;

  WSATRY_AGAIN = ESysEAGAIN;
  WSAEADDRNOTAVAIL = ESysEADDRNOTAVAIL;
  WSAECONNABORTED = ESysECONNABORTED;
  WSAETIMEDOUT = ESysETIMEDOUT;
  WSAEMFILE = ESysEMFILE;
  WSAECONNREFUSED = ESysECONNREFUSED;

  {$ifdef OSLINUX}
  SOCK_NONBLOCK = O_NONBLOCK; // see net.h
  {$endif OSLINUX}

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
  POLLIN       = $001;
  // poll() flag when there is urgent data to read
  POLLPRI      = $002;
  // poll() flag when writing now will not block
  POLLOUT      = $004;
  // poll() flag error condition (always implicitly polled for)
  POLLERR      = $008;
  // poll() flag hung up (always implicitly polled for)
  POLLHUP      = $010;
  // poll() flag invalid polling request (always implicitly polled for)
  POLLNVAL     = $020;
  // poll() flag when normal data may be read
  POLLRDNORM   = $040;
  // poll() flag when priority data may be read
  POLLRDBAND   = $080;
  // poll() flag when writing now will not block
  POLLWRNORM   = $100;
  // poll() flag when priority data may be written
  POLLWRBAND   = $200;
  // poll() flag extension for Linux
  POLLMSG      = $400;

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
  external clib name 'gethostbyname'; // no syscall available from FPC

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

function getnameinfo(addr: PSockAddr; namelen: tsocklen; host: PAnsiChar;
   hostlen: tsocklen; serv: PAnsiChar; servlen: tsocklen; flags: integer): integer; cdecl;
  external clib name 'getnameinfo'; // no syscall available from FPC

function bind(s: integer; addr: PSockAddr; namelen: tsocklen): integer; inline;
begin
  result := fpbind(s, addr, namelen);
end;

function listen(s: integer; backlog: integer): integer; inline;
begin
  result := fplisten(s, backlog);
end;

function accept(s: integer; addr: PSockAddr; var addrlen: tsocklen): integer; inline;
begin
  result := fpaccept(s, addr, @addrlen);
end;

{$ifdef HASACCEPT4}
function accept4(s: integer; addr: PSockAddr; var addrlen: tsocklen; flags: integer): integer; cdecl;
  external clib name 'accept4'; // no syscall available from FPC
{$endif HASACCEPT4}

function connect(s: integer; name: PSockAddr; namelen: tsocklen): integer; inline;
  begin
    result := fpconnect(s, name, namelen);
  end;

function select(nfds: integer; readfds, writefds, exceptfds: PFDSet;
   timeout: PTimeVal): integer; inline;
begin
  result := fpselect(nfds, readfds, writefds, exceptfds, timeout);
end;

function recv(s: integer; Buf: Pointer; len: size_t; flags: integer): ssize_t; inline;
begin
  result := fprecv(s, Buf, len, flags);
end;

function recvfrom(s: integer; Buf: Pointer; len: size_t; flags: integer;
   from: PSockAddr; fromlen: pSocklen): ssize_t; inline;
begin
 result := fprecvfrom(s, Buf, len, flags, from, fromlen);
end;

function send(s: integer; Buf: Pointer; len: size_t; flags: integer): ssize_t; inline;
begin
  result := fpsend(s, Buf, len, flags);
end;

function sendto(s: integer; Buf: Pointer; len: size_t; flags: integer;
   addrto: PSockAddr; tolen: integer): ssize_t; inline;
begin
 result := fpsendto(s, Buf, len, flags, addrto, tolen);
end;

function getpeername(s: integer; name: PSockAddr; var namelen: tsocklen): integer; inline;
begin
  result := fpgetpeername(s, name, @namelen);
end;

{$else}

// we use the libc rather than fp*() low-level functions because the libc
// may be implemented via vDSO and avoid a syscall, and some high-level API
// like gethostbyname() or accept4() are not available as fp* syscalls

function gethostbyname(name: PAnsiChar): PHostEnt; cdecl;
  external clib name 'gethostbyname';

function socket(af, struct, protocol: integer): integer; cdecl;
  external clib name 'socket';

function setsockopt(s: integer; level, optname: integer;
   optval: pointer; optlen: integer): integer; cdecl;
  external clib name 'setsockopt';

function getsockopt(s: integer; level, optname: integer;
   optval: pointer; optlen: PInteger): integer; cdecl;
  external clib name 'getsockopt';

function ioctlsocket(s: integer; cmd: cardinal; arg: PCardinal): integer; cdecl;
  external clib name 'ioctl';

function shutdown(s: integer; how: integer): integer; cdecl;
  external clib name 'shutdown';

function closesocket(s: integer): integer; cdecl;
  external clib name 'close';

function getnameinfo(addr: PSockAddr; namelen: tsocklen; host: PAnsiChar;
   hostlen: tsocklen; serv: PAnsiChar; servlen: tsocklen; flags: integer): integer; cdecl;
  external clib name 'getnameinfo';

function bind(s: integer; addr: PSockAddr; namelen: tsocklen): integer; cdecl;
  external clib name 'bind';

function listen(s: integer; backlog: integer): integer; cdecl;
  external clib name 'listen';

function accept(s: integer; addr: PSockAddr; var addrlen: tsocklen): integer; cdecl;
  external clib name 'accept';

{$ifdef HASACCEPT4}
function accept4(s: integer; addr: PSockAddr; var addrlen: tsocklen; flags: integer): integer; cdecl;
  external clib name 'accept4';
{$endif HASACCEPT4}

function connect(s: integer; name: PSockAddr; namelen: tsocklen): integer; cdecl;
  external clib name 'connect';

function select(nfds: integer; readfds, writefds, exceptfds: PFDSet;
   timeout: PTimeVal): integer; cdecl;
  external clib name 'select';

function recv(s: integer; Buf: Pointer; len: size_t; flags: integer): ssize_t; cdecl;
  external clib name 'recv';

function recvfrom(s: integer; Buf: Pointer; len: size_t; flags: integer;
   from: PSockAddr; fromlen: Pinteger): ssize_t; cdecl;
  external clib name 'recvfrom';

function send(s: integer; Buf: Pointer; len: size_t; flags: integer): ssize_t; cdecl;
  external clib name 'send';

function sendto(s: integer; Buf: Pointer; len: size_t; flags: integer;
   addrto: PSockAddr; tolen: integer): ssize_t; cdecl;
  external clib name 'sendto';

function getpeername(s: integer; name: PSockAddr; var namelen: tsocklen): integer; cdecl;
  external clib name 'getpeername';

{$endif SOCKDIRECTSYSCALL}

{$ifdef OSDARWIN}
// circumvent linking issue on Mac OSX
function poll(fds: PPollFD; nfds, timeout: integer): integer; inline;
begin
  result := fppoll(pointer(fds), nfds, timeout);
end;
{$else}
function poll(fds: PPollFD; nfds, timeout: integer): integer; cdecl;
  external clib name 'poll';
{$endif OSDARWIN}

function sockerrno: integer; inline;
begin
  result := cerrno; // from libc
end;

{$ifdef HASACCEPT4}
var
  Accept4Unsupported: boolean;
{$endif HASACCEPT4}

function doaccept(s: TSocket; addr: PSockAddr; var async: boolean): integer;
var
  len: tsocklen;
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
  _gethostbynamesafe: TOSLightLock; // gethostbyname() result is NOT thread-safe

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
      // avoid buffer overflow
      result := nrFatalError
    else
      with psockaddr_un(@Addr)^ do
      begin
        sun_family := AF_UNIX;
        MoveFast(pointer(address)^, sun_path, length(address));
        result := nrOK;
      end;
    exit;
  end;
  // check supplied port
  addr4.sin_port := htons(GetCardinal(pointer(addrport)));
  if (addr4.sin_port = 0) and
     (addrport <> '0') then // explicit '0' to get ephemeral port
    exit;
  // handle simple cases
  result := nrOk;
  if (address = cLocalhost) or
     (address = c6Localhost) or
     PropNameEquals(address, 'localhost') then
  begin
    addr4.sin_family := AF_INET;
    PCardinal(@addr4.sin_addr)^ := cLocalhost32; // 127.0.0.1
    exit;
  end
  else if (address = cAnyHost) or
          (address = c6AnyHost) then
  begin
    addr4.sin_family := AF_INET; // 0.0.0.0
    exit;
  end;
  // resolve name or IP (getaddrinfo is more recent, but addrinfo is not fixed)
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

procedure TNetSocketWrap.SetLinger(linger: integer);
var
  v: TLinger;
begin
  v.l_linger := linger;
  v.l_onoff := ord(linger >= 0);
  SetOpt(SOL_SOCKET, SO_LINGER, @v, SizeOf(v));
  if linger > 0 then
  begin
    v.l_onoff := ord(true);
    {$ifdef OSBSDDARWIN}
    SetOpt(SOL_SOCKET, SO_REUSEPORT, @v.l_onoff, SizeOf(v.l_onoff));
    {$ifndef OSOPENBSD}
    SetOpt(SOL_SOCKET, SO_NOSIGPIPE, @v.l_onoff, SizeOf(v.l_onoff));
    {$endif OSOPENBSD}
    {$else}
    SetOpt(SOL_SOCKET, SO_REUSEADDR, @v.l_onoff, SizeOf(v.l_onoff));
    {$endif OSBSDDARWIN}
  end;
end;

function TNetSocketWrap.WaitFor(ms: integer; scope: TNetEvents;
  loerr: system.PInteger): TNetEvents;
var
  res: integer;
  p: TPollFD; // select() limits p.fd<FD_SETSIZE=1024 on POSIX -> use poll()
  // https://moythreads.com/wordpress/2009/12/22/select-system-call-limitation
begin
  result := [neError];
  if @self = nil then
    exit;
  p.fd := TSocket(@self);
  p.events := 0;
  if neRead in scope then
    p.events := POLLIN or POLLPRI;
  if neWrite in scope then
    p.events := p.events or POLLOUT;
  p.revents := 0;
  res := poll(@p, 1, ms);
  if res < 0 then
  begin
    res := sockerrno;
    if loerr <> nil then
      loerr^ := res;
    if res = WSATRY_AGAIN then
      // timeout, not a true error
      result := [];
    exit;
  end;
  result := [];
  if p.revents and (POLLIN or POLLPRI) <> 0 then
    include(result, neRead);
  if p.revents and POLLOUT <> 0 then
    include(result, neWrite);
  if p.revents and POLLHUP <> 0 then
    include(result, neClosed);
end;


{ ******************** Mac and IP Addresses Support }

{$ifndef OSANDROID}
  // on POSIX, use getifaddrs/freeifaddrs from libc
  {$define USE_IFADDRS}
{$endif OSANDROID}

{$ifdef OSANDROIDARM64}
  {$define USE_IFADDRS}
  // Android BIONIC seems to implement it since Android N (API level 24)
  // https://android.googlesource.com/platform/bionic/+/master/docs/status.md
{$endif OSANDROID}

{$ifdef USE_IFADDRS}

type
  pifaddrs = ^ifaddrs;
  ifaddrs = record
    ifa_next: pifaddrs;
    ifa_name: PAnsiChar;
    ifa_flags: cardinal;
    ifa_addr: Psockaddr;
    ifa_netmask: Psockaddr;
    ifa_dstaddr: Psockaddr;
    ifa_data: Pointer;
  end;

  {$ifdef OSBSDDARWIN} // from bsd/net/if_dl.h
  psockaddr_dl = ^sockaddr_dl;
  sockaddr_dl = record
    sdl_len: byte;	 // Total length of sockaddr
    sdl_family: byte;	 // AF_LINK
    sdl_index: word;	 // if != 0, system given index for interface
    sdl_type: byte;	 // interface type
    sdl_nlen: byte;	 // interface name length, no trailing 0 reqd.
    sdl_alen: byte;	 // link level address length
    sdl_slen: byte;	 // link layer selector length
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

const
  IFF_UP = $1;
  IFF_LOOPBACK = $8;

function getifaddrs(var ifap: pifaddrs): Integer; cdecl;
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
            if kind <> tiaIPv6 then
              with info^.ifa_addr^ do
                if (integer(sin_addr) <> $0100007f) and
                   (integer(sin_addr) <> 0) then
                  case Kind of
                    tiaIpv4,
                    tiaAny:
                      IP4Text(@sin_addr,s);
                    tiaIPv4Public:
                      if IsPublicIP(integer(sin_addr)) then
                        IP4Text(@sin_addr, s);
                    tiaIPv4Private:
                      if not IsPublicIP(integer(sin_addr)) then
                        IP4Text(@sin_addr, s);
                  end;
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
  result := nil;
  if getifaddrs(list) = NO_ERROR then
  try
    n := 0;
    info := list;
    repeat
      if (info^.ifa_addr <> nil) and
         (info^.ifa_flags and IFF_LOOPBACK = 0) and
         (UpAndDown or
          (info^.ifa_flags and IFF_UP <> 0)) and
      // see https://stackoverflow.com/a/51218583/458259
      {$ifdef OSBSDDARWIN}
         (info^.ifa_addr^.sa_family = AF_LINK) then
      begin
        mac := '';
        with psockaddr_dl(info^.ifa_addr)^ do
          if sdl_alen = 6 then
            // #define LLADDR(s) ((caddr_t)((s)->sdl_data + (s)->sdl_nlen))
            if PCardinal(@sdl_data[sdl_nlen])^ <> 2 then
              // iOS7+ return always 02:00:00:00:00:00 as MAC addresses :(
              mac := MacToText(@sdl_data[sdl_nlen]);
        if mac <> '' then
      {$else}
         (info^.ifa_addr^.sa_family = AF_PACKET) then
      begin
        mac := MacToText(@psockaddr_ll(info^.ifa_addr)^.sll_addr);
      {$endif OSBSDDARWIN}
        for i := 0 to n - 1 do
          if result[i].Address = mac then
          begin
            mac := ''; // may appear several times for each IP
            break;
          end;
        if mac <> '' then
        begin
          if n = length(result) then
            SetLength(result, NextGrow(n));
          result[n].Name := info^.ifa_name;
          result[n].Address := mac;
          inc(n);
        end;
      end;
      info := info^.ifa_next;
    until info = nil;
    SetLength(result, n);
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
           (PosixParseHex32(pointer(f)) and (IFF_UP or IFF_LOOPBACK) = IFF_UP) then
        begin
          f := TrimU(StringFromFile(fn + '/address', {nosize=}true));
          if f <> '' then
          begin
            SetLength(result, n + 1);
            result[n].Name := SR.Name;
            result[n].Address := f;
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


{ ******************** Efficient Multiple Sockets Polling }

{$ifdef USEPOLL}

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
  if pseRead in events then
    e := POLLIN or POLLPRI
  else
    e := 0;
  if pseWrite in events then
    e := e or POLLOUT;
  s := PtrUInt(socket);
  new := -1;
  p := pointer(fFD);
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

procedure PollConvert(p: PPollFD; d: PPollSocketResult; n: integer; t: PPollSocketTag);
var
  e: TPollSocketEvents;
  r: integer;
begin
  repeat
    r := p^.revents;
    if (r <> 0) and
       (p^.fd > 0) then
    begin
      if r = POLLIN then
        byte(d^.events) := byte([pseRead]) // optimized for the most common case
      else
      begin
        byte(e) := 0;
        if r and (POLLIN or POLLPRI) <> 0 then
          include(e, pseRead);
        if r and POLLOUT <> 0 then
          include(e, pseWrite);
        if r and POLLERR <> 0 then
          include(e, pseError);
        if r and POLLHUP <> 0 then
          include(e, pseClosed);
        d^.events := e;
      end;
      d^.tag := t^;
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
    SetLength(results.Events, n + 4)
  else if n > length(results.Events) then
  if length(results.Events) < n then
  begin
    results.Events := nil;
    SetLength(results.Events, fMaxSockets);
  end;
  PollConvert(pointer(fFD), pointer(results.Events), fFDCount, pointer(fTags));
  result := true;
end;

function PollSocketClass: TPollSocketClass;
begin
  PollSocketClass := TPollSocketPoll;
end;

{$else}

type
  /// socket polling via Linux epoll optimized API
  // - not available under Windows or BSD/Darwin
  // - direct call of the epoll API in level-triggered (LT) mode
  TPollSocketEpoll = class(TPollSocketAbstract)
  protected
    fEPFD: integer;
    fResults: array of EPoll_Event;
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
    // read-only access to the low-level epoll_create file descriptor
    property EPFD: integer
      read fEPFD;
  end;

{$define POLLSOCKETEPOLL}
// is defined for TPollSocketEpoll.FollowEpoll = true

constructor TPollSocketEpoll.Create(aOwner: TPollSockets);
begin
  inherited Create(aOwner);
  fMaxSockets := 100; // initial value: no limitation in practice and can grow
  fEPFD := epoll_create(10000); // since 2.6.8 size is ignored
  SetLength(fResults, fMaxSockets);
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
  e: EPoll_Event;
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
  inc(fCount);
  if fCount = fMaxSockets then
    fMaxSockets := NextGrow(fMaxSockets); // epoll has no theoritical limit
end;

function TPollSocketEpoll.Unsubscribe(socket: TNetSocket): boolean;
var
  e: EPoll_Event; // should be there even if not used (old kernel < 2.6.9)
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
      dec(fCount);
      if (fMaxSockets > 100) and
         (fCount < fMaxSockets shr 1) then
        fMaxSockets := fMaxSockets shr 1; // size down fMaxSockets
    end;
    if (fOwner <> nil) and
       fOwner.fUnsubscribeShouldShutdownSocket then
      socket.ShutdownAndClose({rdwr=}false);
  end;
end;

function TPollSocketEpoll.WaitForModified(var results: TPollSocketResults;
  timeoutMS: integer): boolean;
var
  e: TPollSocketEvents;
  n: PtrInt;
  ev: integer;
  ee: ^EPoll_Event;
  er: PPollSocketResult;
begin
  result := false; // error or nothing new
  if (self = nil) or
     (fEPFD <= 0) then
    exit;
  // we allow fCount=0 which enable background epoll_ctl subscription
  if fMaxSockets > length(fResults) then
  begin
    fResults := nil;
    SetLength(fResults, fMaxSockets);
  end;
  n := epoll_wait(fEPFD, pointer(fResults), fMaxSockets, timeoutMS);
  results.Count := n;
  if n <= 0 then
    exit;
  if results.Events = nil then
    // temporary results.Events = new in PollForPendingEvents()
    SetLength(results.Events, n + 4)
  else if n > length(results.Events) then
  begin
    // results.Events can be reused between PollForPendingEvents() calls
    results.Events := nil;
    SetLength(results.Events, fMaxSockets)
  end;
  ee := pointer(fResults);
  er := pointer(results.Events);
  repeat
    er^.tag := TPollSocketTag(ee^.data.ptr);
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
    er^.events := e;
    inc(ee);
    inc(er);
    dec(n)
  until n = 0;
  result := true;
end;

function PollSocketClass: TPollSocketClass;
begin
  PollSocketClass := TPollSocketEpoll;
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

