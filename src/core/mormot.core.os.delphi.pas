/// Framework Core POSIX API Wrappers for Delphi
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.os.delphi;

{
  *****************************************************************************

   Map FPC cross-platform API types and functions into Delphi POSIX units
   - Core POSIX Operating Systems API for Delphi
   - Network POSIX Operating Systems API for Delphi
  
   This unit is called by mormot.core.os.posix.inc and mormot.net.sock.posix.inc
   as a compatibility layer with the FPC POSIX units.

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

{$ifdef POSIXDELPHI} // do-nothing unit on FPC or Delphi for Windows
uses
  classes,
  types,
  sysutils,
  mormot.core.base,
  // the Delphi System and POSIX units exposed by this unit
  System.TimeSpan,
  System.DateUtils,
  System.IOUtils,
  System.SyncObjs,
  Posix.Base,
  Posix.Unistd,
  Posix.Stdio,
  Posix.Stdlib,
  Posix.Errno,
  Posix.Fcntl,
  Posix.Dlfcn,
  Posix.Pthread,
  Posix.Sched,
  {$IFDEF OSLINUX}
  Linuxapi.KernelIoctl,
  {$ENDIF OSLINUX}
  Posix.StrOpts,
  Posix.Time,
  Posix.SysTime,
  Posix.Utime,
  Posix.SysMman,
  Posix.SysStat,
  Posix.SysWait,
  Posix.SysSocket,
  Posix.SysUtsname,
  Posix.SysSysctl,
  Posix.DirEnt,
  Posix.NetinetIn,
  Posix.Termios;


{ ****************** Core POSIX Operating Systems API for Delphi }

// in the code below, PChar = PWideChar so those wrapper functions could make the
// proper temporary conversion from UTF-16 to UTF-8 before calling the POSIX API
// from regular pointer(aFileName) parameters

type
  cint8   = shortint;
  cuchar  = byte;
  cshort  = smallint;
  cushort = word;
  cint    = integer;
  cuint   = cardinal;
  cuint32 = cardinal;
  clong   = PtrInt;
  culong  = PtrUInt;
  cuint64 = QWord;
  pcint   = ^cint;
  ssize_t = PtrInt;
  size_t  = PtrUInt;
  psize_t = ^size_t;
  TUid    = cardinal;
  TGid    = cardinal;
  PGid    = ^TGid;
  TPid    = cint;
  dl_info = Posix.Dlfcn.dl_info;

function dlopen(Name: PWideChar; Flags: cint): pointer;
function dlsym(Lib: pointer; Name: PAnsiChar): pointer;
function dlclose(Lib: pointer): cint;
function dlerror: UnicodeString;
function dladdr(Lib: pointer; info: Pdl_info): cint;

function envp: PPAnsiChar; // system.envp is nil !

procedure InitCriticalSection(var cs);
procedure DoneCriticalSection(var cs);
procedure ThreadSwitch;

// wrap to System.SyncObjs.TEvent
function  RTLEventCreate: TEvent;
procedure RTLEventDestroy(state: TEvent);
procedure RTLEventSetEvent(state: TEvent);
procedure RTLEventResetEvent(state: TEvent);
procedure RTLEventWaitFor(state: TEvent); overload;
procedure RTLEventWaitFor(state: TEvent; timeout: cint); overload;

const
  clib = libc;

  ESysEACCES        = Posix.Errno.EACCES;
  ESysEINTR         = Posix.Errno.EINTR;
  ESysEFAULT        = Posix.Errno.EFAULT;
  ESysEPERM         = Posix.Errno.EPERM;
  ESysESRCH         = Posix.Errno.ESRCH;
  ESysE2BIG         = Posix.Errno.E2BIG;
  ESysEAGAIN        = Posix.ErrNo.EAGAIN;
  ESysEADDRNOTAVAIL = Posix.ErrNo.EADDRNOTAVAIL;
  ESysECONNABORTED  = Posix.ErrNo.ECONNABORTED;
  ESysECONNRESET    = Posix.ErrNo.ECONNRESET;
  ESysETIMEDOUT     = Posix.ErrNo.ETIMEDOUT;
  ESysEINVAL        = Posix.ErrNo.EINVAL;
  ESysEMFILE        = Posix.ErrNo.EMFILE;
  ESysECONNREFUSED  = Posix.ErrNo.ECONNREFUSED;
  ESysEINPROGRESS   = Posix.ErrNo.EINPROGRESS;
  ESysEALREADY      = Posix.ErrNo.EALREADY;
  ESysENOSYS        = Posix.ErrNo.ENOSYS;
  ESysEOPNOTSUPP    = Posix.ErrNo.EOPNOTSUPP;
  ESysEPROTOTYPE    = Posix.ErrNo.EPROTOTYPE;
  ESysEPIPE         = Posix.ErrNo.EPIPE;

  StdInputHandle    = 0;
  StdOutputHandle   = 1;
  StdErrorHandle    = 2;
  RTLD_LAZY         = Posix.Dlfcn.RTLD_LAZY;
  O_RDONLY          = O_RDONLY;
  O_NONBLOCK        = O_NONBLOCK;
  SEEK_CUR          = SEEK_CUR;
  FIONREAD          = FIONREAD;
  FIONBIO           = FIONBIO;
  F_OK              = F_OK;

  CLOCK_MONOTONIC_RAW = 4;

  SIGINT  = 2;
  SIGQUIT = 3;
  SIGKILL = 9;
  SIGTERM = 15;

  W_OK    = Posix.Unistd.W_OK;
  S_IRUSR = Posix.SysStat.S_IRUSR;
  S_IWUSR = Posix.SysStat.S_IWUSR;
  S_IRGRP = Posix.SysStat.S_IRGRP;
  S_IROTH = Posix.SysStat.S_IROTH;
  S_ISVTX = Posix.SysStat.S_ISVTX;
  S_IXUSR = Posix.SysStat.S_IXUSR;
  S_IXGRP = Posix.SysStat.S_IXGRP;
  S_IXOTH = Posix.SysStat.S_IXOTH;

type
  clockid_t = cint;
  time_t    = Posix.SysTime.time_t;
  ptimeval  = Posix.SysTime.ptimeval;
  TTimeVal  = Posix.SysTime.timeval;
  ptimezone = pointer;
  ptimespec = ^timespec;
  TTimeSpec = timespec;
  TStat     = _stat;
  TUtimBuf  = utimbuf;
  UtsName   = TUtsName;

function fpgeterrno: cint;
procedure fpseterrno(err: cint);
function cerrno: cint; inline; // internal to mormot.net.sock.posix.inc

function fpgettimeofday(tp: ptimeval; tzp: pointer): cint;
function fpsettimeofday(tp: ptimeval; tzp: pointer): cint;
function fpnanosleep(t, rem: ptimespec): cint;
function GetLocalTimeOffset: integer;
function TZSeconds: integer;
function fpuname(var uts: UtsName): cint;

function fpstat(path: PWideChar; var buf: _stat): cint;
function fplstat(path: PWideChar; var buf: _stat): cint;
function fpfstat(fd: cint; var buf: _stat): cint;
function fplseek(fd: cint; off: Int64; orig: cint): Int64;
function fpftruncate(fd: cint; off: Int64): Int64;
function fpfsync(fd: cint): cint;
function fpclose(fd: cint): cint;
function fputime(path: PWideChar; times: putimbuf): cint;
function fpaccess(path: PWideChar; mode: cint): cint;
function fpunlink(path: PWideChar): cint;
function fpchdir(path: PWideChar): cint;
function fprename(old, new: PWideChar): cint;
function fpsymlink(old, new: PWideChar): cint;
function fprmdir(path: PWideChar): cint;
function fpchmod(path: PWideChar; mode: cint): cint;
function fpopen(path: PWideChar; mode: cint): cint;
function fpopena(path: PAnsiChar; mode: cint): cint;
function fpwrite(fd: cint; buf: pointer; n: PtrInt): PtrInt;
function fpread(fd: cint; buf: pointer; n: PtrInt): PtrInt;
function fpioctl(fd, ndx: cint; data: pointer): cint;
function fpreadlink(fn, lnk: pointer; max: cint): cint;

function FpS_ISDIR(m: cint): boolean;
function FpS_ISSOCK(m: cint): boolean;
function FpS_ISBLK(m: cint): boolean;
function FpS_ISCHR(m: cint): boolean;
function FpS_ISFIFO(m: cint): boolean;
function FpS_ISLNK(m: cint): boolean;

function fpkill(pid, sig: cint): cint; cdecl;
  external clib name 'kill';
function fpfork: TPid; cdecl;
  external clib name 'fork';

{$ifdef OSDARWIN}

const
  CTL_HW          = 6;
  MAP_ANONYMOUS   = $1000; // not defined in Delphi RTL
  HW_USERMEM      = HW_USERMEM;
  HW_PAGESIZE     = HW_PAGESIZE;
  HW_MACHINE      = HW_MACHINE;
  HW_MODEL        = HW_MODEL;
  HW_NCPU         = HW_NCPU;
  HW_CACHELINE    = HW_CACHELINE;
  HW_L1DCACHESIZE = HW_L1DCACHESIZE;
  HW_L2CACHESIZE  = HW_L2CACHESIZE;
  HW_L3CACHESIZE  = HW_L3CACHESIZE;
  HW_MEMSIZE      = HW_MEMSIZE;

function fpsysctl(name: pcint; namelen: cuint; oldp: pointer;
    oldlenp: psize_t; newp: pointer; newlen: size_t): cint; cdecl;
  external clib name 'sysctl';
function fpsysctlbyname(name: PAnsiChar; oldp: pointer; oldlenp: psize_t;
    newp: pointer; newlen: size_t): cint; cdecl;
  external clib name 'sysctlbyname';

{$endif OSDARWIN}

function FpGetuid: TUid;
function FpGetgid: TGid;
function FpGetpid: TPid;
function FpGetppid: TPid;
function FpSetsid: TPid;
function fpumask(mode: cint): cint;
function fpexecve(path: PAnsiChar; argc, envp: pointer): cint;
procedure fpexit(code: cint);
function WaitProcess(pid: cint): cint;

function fpmmap(start: pointer; len: PtrUInt; prot, flags, fd: cint; offst: Int64): pointer;
function fpmunmap(start: pointer; len: PtrUint): cint;

type
  Dir     = Posix.DirEnt.Dir;
  pDir    = Posix.DirEnt.pDir;
  pDirent = Posix.DirEnt.pDirent;

function fpopendir(path: PWideChar): pDir;
function fpreaddir(var dirp: Dir): pDirent;
function fpclosedir(var dirp: Dir): cint;

{$ifdef OSLINUX}
type
  TStatfs = record
    fstype, bsize: clong;
    blocks, bfree, bavail, files, ffree, fsid: culong;
    namelen, frsize, flags: clong;
    spare: array[0..3] of clong; { For later use }
  end;

  TSysInfo = record
    uptime: clong;                     //* Seconds since boot */
    loads: array[0..2] of culong;      //* 1, 5, and 15 minute load averages */
    totalram: culong;                  //* Total usable main memory size */
    freeram: culong;                   //* Available memory size */
    sharedram: culong;                 //* Amount of shared memory */
    bufferram: culong;                 //* Memory used by buffers */
    totalswap: culong;                 //* Total swap space size */
    freeswap: culong;                  //* swap space still available */
    procs: cushort;                    //* Number of current processes */
    pad: cushort;                      //* explicit padding for m68k */
    totalhigh: culong;                 //* Total high memory size */
    freehigh: culong;                  //* Available high memory size */
    mem_unit: cuint;                   //* Memory unit size in bytes */
{$ifndef cpu64}
    { the upper bound of the array below is negative for 64 bit cpus }
    _f: array[0..19-2*sizeof(clong)-sizeof(cint)] of cChar;  //* Padding: libc5 uses this.. */
{$endif cpu64}
  end;
  PSysInfo = ^TSysInfo;

function SysInfo(Info: PSysinfo): cInt; cdecl;
  external clib name 'sysinfo';
{$endif OSLINUX}

function fpstatfs(path: PWideChar; nfo: pointer): cint;
function IsAtty(fd: cint): cint;


{ ****************** Network POSIX Operating Systems API for Delphi }

const
  IPPROTO_TCP  = IPPROTO_TCP;
  IPPROTO_UDP  = IPPROTO_UDP;
  TCP_NODELAY  = 1;
  TCP_CORK     = 3; // Linux specific
  TCP_NOPUSH   = 4; // BSD specific
  MSG_PEEK     = Posix.SysSocket.MSG_PEEK;
  SHUT_RD      = Posix.SysSocket.SHUT_RD;
  SHUT_WR      = Posix.SysSocket.SHUT_WR;
  SHUT_RDWR    = Posix.SysSocket.SHUT_RDWR;

  SOCK_RAW     = Posix.SysSocket.SOCK_RAW;
  SOCK_STREAM  = Posix.SysSocket.SOCK_STREAM;
  SOCK_DGRAM   = Posix.SysSocket.SOCK_DGRAM;
  AF_INET      = Posix.SysSocket.AF_INET;
  AF_INET6     = Posix.SysSocket.AF_INET6;
  AF_UNIX      = Posix.SysSocket.AF_UNIX;
  AF_PACKET    = 17; // Linux specific
  SOMAXCONN    = Posix.SysSocket.SOMAXCONN;
  SOL_SOCKET   = Posix.SysSocket.SOL_SOCKET;
  SO_SNDTIMEO  = Posix.SysSocket.SO_SNDTIMEO;
  SO_RCVTIMEO  = Posix.SysSocket.SO_RCVTIMEO;
  SO_REUSEADDR = Posix.SysSocket.SO_REUSEADDR;
  SO_LINGER    = Posix.SysSocket.SO_LINGER;
  SO_KEEPALIVE = Posix.SysSocket.SO_KEEPALIVE;
  SO_SNDBUF    = Posix.SysSocket.SO_SNDBUF;
  SO_RCVBUF    = Posix.SysSocket.SO_RCVBUF;
  SO_BROADCAST = Posix.SysSocket.SO_BROADCAST;
  {$ifdef OSLINUXANDROID}
  SO_PRIORITY  = Posix.SysSocket.SO_PRIORITY;
  {$endif OSLINUXANDROID}


type
  // POSIX definitions to share the same type fields between FPC and Delphi
  TSockLen  = Posix.SysSocket.socklen_t;
  TLinger   = linger;

const
  POLLIN      = $0001;
  POLLPRI     = $0002;

type
  TPollFD = record
    fd: cint;
    events: cshort;
    revents: cshort;
  end;
  PPollFD = ^TPollFD;

function fppoll(fds: PPollFD; nfds, timeout: cint): cint; cdecl;
  external clib name 'poll';

{$ifdef OSLINUX}

const
  EPOLLIN      = $01;
  EPOLLPRI     = $02;
  EPOLLOUT     = $04;
  EPOLLERR     = $08;
  EPOLLHUP     = $10;
  EPOLLONESHOT = $40000000;
  EPOLLET      = $80000000;

  EPOLL_CTL_ADD = 1;
  EPOLL_CTL_DEL = 2;
  EPOLL_CTL_MOD = 3;

type
  TEPoll_Data = record
    case integer of
      0: (ptr: pointer);
      1: (fd:  cint);
      2: (u32: cuint);
      3: (u64: cuint64);
  end;
  PEPoll_Data = ^TEpoll_Data;
  TEPoll_Event = {$ifdef CPUX64} packed {$endif} record
    events: cuint32;
    data: TEpoll_Data;
  end;
  PEPoll_Event = ^TEPoll_Event;

function epoll_create(size: cint): cint; cdecl;
  external clib name 'epoll_create';
function epoll_ctl(epfd, op, fd: cint; event: PEPoll_Event): cint; cdecl;
  external clib name 'epoll_ctl';
function epoll_wait(epfd: cint; events: PEPoll_Event;
    maxevents, timeout: cint): cint; cdecl;
  external clib name 'epoll_wait';

{$endif OSLINUX}



implementation

uses
  mormot.core.os;


{ ****************** Core POSIX Operating Systems API for Delphi }

function dlopen(Name: PWideChar; Flags: integer): pointer;
var
  tmp: TSynTempBuffer;
begin
  result := pointer(Posix.Dlfcn.dlopen(Unicode_ToUtf8(Name, tmp), Flags));
  tmp.Done;
end;

function dlsym(Lib: pointer; Name: PAnsiChar): pointer;
begin
  result := Posix.Dlfcn.dlsym(PtrUInt(Lib), Name);
end;

function dlclose(Lib: pointer) : integer;
begin
  result := Posix.Dlfcn.dlclose(PtrUInt(Lib));
end;

function dlerror: UnicodeString;
begin
  result := UnicodeString(Posix.Dlfcn.dlerror);
end;

function dladdr(Lib: pointer; info: Pdl_info): cint;
begin
  result := Posix.Dlfcn.dladdr(PtrUInt(Lib), info^);
end;

function envp: PPAnsiChar;
begin
  result := Posix.Unistd.environ;
end;

procedure InitCriticalSection(var cs);
begin
  TRTLCriticalSection(cs) := TRTLCriticalSection.Create;
end;

procedure DoneCriticalSection(var cs);
begin
  FreeAndNil(TRTLCriticalSection(cs));
end;

procedure ThreadSwitch;
begin
  sched_yield();
end;


function RTLEventCreate: TEvent;
begin
  result := TEvent.Create;
end;

procedure RTLEventDestroy(state: TEvent);
begin
  state.Free;
end;

procedure RTLEventSetEvent(state: TEvent);
begin
  state.SetEvent;
end;

procedure RTLEventResetEvent(state: TEvent);
begin
  state.ResetEvent;
end;

procedure RTLEventWaitFor(state: TEvent);
begin
  state.WaitFor(INFINITE);
end;

procedure RTLEventWaitFor(state: TEvent; timeout: cint);
begin
  state.WaitFor(timeout);
end;


function cerrno: cint;
begin
  result := Posix.Errno.errno;
end;

function fpgeterrno: cint;
begin
  result := Posix.Errno.errno;
end;

procedure fpseterrno(err: cint);
begin
  system.SetLastError(err);
end;

function fpsettimeofday(tp: ptimeval; tzp: pointer): cint;
begin
  result := settimeofday(tp, tzp);
end;

function fpgettimeofday(tp: ptimeval; tzp: pointer): cint;
begin
  result := gettimeofday(tp^, tzp);
end;

function fpnanosleep(t, rem: ptimespec): cint;
begin
  result := nanosleep(t^, pointer(rem));
end;

function GetLocalTimeOffset: integer;
begin
  result := Round(TTimeZone.Local.UtcOffset.TotalSeconds);
end;

function TZSeconds: integer;
begin
  result := -GetLocalTimeOffset * 60; // GetLocalTimeOffset = -TZseconds div 60
end;

function fpuname(var uts: UtsName): cint;
begin
  result := uname(uts);
end;
function fpstat(path: PWideChar; var buf: _stat): cint;
var
  tmp: TSynTempBuffer;
begin
  result := stat(Unicode_ToUtf8(path, tmp), buf);
  tmp.Done;
end;

function fplstat(path: PWideChar; var buf: _stat): cint;
var
  tmp: TSynTempBuffer;
begin
  result := lstat(Unicode_ToUtf8(path, tmp), buf);
  tmp.Done;
end;

function fpfstat(fd: cint; var buf: _stat): cint;
begin
  result := fstat(fd, buf);
end;

function fplseek(fd: cint; off: Int64; orig: cint): Int64;
begin
  result := {$ifdef ANDROID} lseek64 {$else} lseek {$endif}(fd, off, orig);
end;

function fpftruncate(fd: cint; off: Int64): Int64;
begin
  result := ftruncate(fd, off);
end;

function fpfsync(fd: cint): cint;
begin
  result := fsync(fd);
end;

function fpclose(fd: cint): cint;
begin
  result := __close(fd);
end;

function fputime(path: PWideChar; times: putimbuf): cint;
var
  tmp: TSynTempBuffer;
begin
  result := utime(Unicode_ToUtf8(path, tmp), times^);
  tmp.Done;
end;

function fpaccess(path: PWideChar; mode: cint): cint;
var
  tmp: TSynTempBuffer;
begin
  result := access(Unicode_ToUtf8(path, tmp), mode);
  tmp.Done;
end;

function fpunlink(path: PWideChar): cint;
var
  tmp: TSynTempBuffer;
begin
  result := unlink(Unicode_ToUtf8(path, tmp));
  tmp.Done;
end;

function fpchdir(path: PWideChar): cint;
var
  tmp: TSynTempBuffer;
begin
  result := __chdir(Unicode_ToUtf8(path, tmp));
  tmp.Done;
end;

function fprename(old, new: PWideChar): cint;
var
  o, n: TSynTempBuffer;
begin
  result := __rename(Unicode_ToUtf8(old, o), Unicode_ToUtf8(new, n));
  o.Done;
  n.Done;
end;

function fpsymlink(old, new: PWideChar): cint;
var
  o, n: TSynTempBuffer;
begin
  result := symlink(Unicode_ToUtf8(old, o), Unicode_ToUtf8(new, n));
  o.Done;
  n.Done;
end;

function fprmdir(path: PWideChar): cint;
var
  tmp: TSynTempBuffer;
begin
  result := __rmdir(Unicode_ToUtf8(path, tmp));
  tmp.Done;
end;

function fpchmod(path: PWideChar; mode: cint): cint;
var
  tmp: TSynTempBuffer;
begin
  result := chmod(Unicode_ToUtf8(path, tmp), mode);
  tmp.Done;
end;

function fpopen(path: PWideChar; mode: cint): cint;
var
  tmp: TSynTempBuffer;
begin
  result := __open(Unicode_ToUtf8(path, tmp), mode);
  tmp.Done;
end;

function fpopena(path: PAnsiChar; mode: cint): cint;
begin
  result := __open(path, mode);
end;

function fpwrite(fd: cint; buf: pointer; n: PtrInt): PtrInt;
begin
  result := __write(fd, buf, n);
end;

function fpread(fd: cint; buf: pointer; n: PtrInt): PtrInt;
begin
  result := __read(fd, buf, n);
end;

function fpioctl(fd, ndx: cint; data: pointer): cint;
begin
  result := ioctl(fd, ndx, data);
end;

function fpreadlink(fn, lnk: pointer; max: cint): cint;
begin
  result := readlink(fn, lnk, max);
end;

function FpS_ISDIR(m: cint): boolean;
begin
  result := S_ISDIR(m);
end;

function FpS_ISSOCK(m: cint): boolean;
begin
  result := S_ISSOCK(m);
end;

function FpS_ISBLK(m: cint): boolean;
begin
  result := S_ISBLK(m);
end;

function FpS_ISCHR(m: cint): boolean;
begin
  result := S_ISCHR(m);
end;

function FpS_ISFIFO(m: cint): boolean;
begin
  result := S_ISFIFO(m);
end;

function FpS_ISLNK(m: cint): boolean;
begin
  result := S_ISLNK(m);
end;


function FpGetuid: TUid;
begin
  result := getuid;
end;

function FpGetgid: TGid;
begin
  result := getgid;
end;

function FpGetpid: TPid;
begin
  result := getpid;
end;

function FpGetppid: TPid;
begin
  result := getppid;
end;

function FpSetsid: TPid;
begin
  result := setsid;
end;

function fpumask(mode: cint): cint;
begin
  result := umask(mode);
end;

function fpexecve(path: PAnsiChar; argc, envp: pointer): cint;
begin
  result := execve(path, argc, envp);
end;

procedure fpexit(code: cint);
begin
  __exit(code);
end;

function WaitProcess(pid: cint): cint;
var
  s: cint;
begin
  repeat
    s := $7f00;
    result := waitpid(pid, @s, 0);
    if (result < 0) and
       (cerrno = EINTR) then
      continue;
    if result = -1 then
      exit; // error
  until result <> 0;
  if WIFEXITED(s) then
    result := WEXITSTATUS(s)
  else if result > 0 then
    result := -result;
end;

function fpmmap(start: pointer; len: PtrUInt; prot, flags, fd: cint; offst: Int64): pointer;
begin
  result := mmap(start, len, prot, flags, fd, offst);
end;

function fpmunmap(start: pointer; len: PtrUint): cint;
begin
  result := munmap(start, len);
end;

function fpopendir(path: PWideChar): pDir;
var
  tmp: TSynTempBuffer;
begin
  result := opendir(Unicode_ToUtf8(path, tmp));
  tmp.Done;
end;

function fpreaddir(var dirp: Dir): pDirent;
begin
  result := readdir(@dirp);
end;

function fpclosedir(var dirp: Dir): cint;
begin
  result := closedir(@dirp);
end;

function statfs(path: PAnsiChar; nfo: pointer): cint; cdecl;
  external clib name 'statfs';

function fpstatfs(path: PWideChar; nfo: pointer): cint;
var
  tmp: TSynTempBuffer;
begin
  result := statfs(Unicode_ToUtf8(path, tmp), nfo);
  tmp.Done;
end;

function IsAtty(fd: cint): cint;
var
  t: Termios;
begin
  result := ord(tcgetattr(fd, t) = 0);
end;

{****************** Network POSIX Operating Systems API for Delphi }


initialization

{$else}
implementation
{$endif POSIXDELPHI}

end.

