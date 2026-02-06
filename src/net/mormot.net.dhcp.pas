/// Simple DHCP Protocol Support
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.dhcp;

{
  *****************************************************************************

   Simple Dynamic Host Configuration Protocol (DHCP) Protocol Support
    - Low-Level DHCP Protocol Definitions
    - Middle-Level DHCP Scope and Lease Logic
    - High-Level Multi-Scope DHCP Server Processing Logic

   Implement DISCOVER, OFFER, REQUEST, DECLINE, ACK, NAK, RELEASE, INFORM.
   Background lease persistence using dnsmasq text file.
   Scale up to dozen of thousands of leases with minimal RAM/CPU consumption.
   Support VLAN via SubNets / Scope and Relay Agent 82 Option.
   Prevent most client abuse with proper rate limiting.
   Cross-Platform on Windows, Linux and MacOS.
   Meaningful logging of the actual process.
   Easy configuration via JSON or INI files.
   Expandable in code via callbacks or virtual methods.
   e.g. as full local PXE environment with our mormot.net.tftp.server.pas unit

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.log,
  mormot.net.sock;


{ **************** Low-Level DHCP Protocol Definitions }

const
  /// regular UDP port of DHCP server
  DHCP_SERVER_PORT = 67;
  /// regular UDP port of DHCP client
  DHCP_CLIENT_PORT = 68;

  BOOT_REQUEST = 1;
  BOOT_REPLY   = 2;

type
  /// Exception class raised by this unit
  EDhcp = class(ESynException);

  /// a DHCP raw UDP packet message as defined in RFC 2131 from BOOTP layout
  TDhcpPacket = packed record
    /// BOOTP type i.e. BOOT_REQUEST/BOOT_REPLY, not TDhcpMessageType
    op:     byte;
    /// hardware type, mostly ARPHRD_ETHER = 1
    htype:  byte;
    /// hardware number of bytes, mostly 6 = SizeOf(TNetMac)
    hlen:   byte;
    hops:   byte;
    /// request transaction sequence
    xid:    cardinal;
    secs:   word;
    flags:  word;
    /// client IP address
    ciaddr: TNetIP4;
    /// your IP address
    yiaddr: TNetIP4;
    /// server IP address
    siaddr: TNetIP4;
    /// gateway IP address
    giaddr: TNetIP4;
    /// client MAC address - only chaddr[0..5] for htype=1 and hlen=6
    chaddr: array[0..15] of byte;
    sname:  array[0..63] of byte;
    file_:  array[0..127] of byte;
    /// magic cookie for DHCP encapsulated in BOOTP message
    cookie: cardinal;
    /// the raw DHCP options, as type/len/value triplets, ending with $ff
    options: array[0..307] of byte;
  end;
  /// points to a DHCP raw UDP packet message
  PDhcpPacket = ^TDhcpPacket;

  /// the DHCP message types as defined in RFC 2132, 3203, 4388, 6926, 7724
  // - ord() value does match the actual byte value within the DHCP frame
  TDhcpMessageType = (
    dmtUndefined,
    dmtDiscover,
    dmtOffer,
    dmtRequest,
    dmtDecline,
    dmtAck,
    dmtNak,
    dmtRelease,
    dmtInform,
    dmtForceRenew,
    dmtLeaseQuery,
    dmtLeaseUnassigned,
    dmtLeaseUnknown,
    dmtLeaseActive,
    dmtBulkLeaseQuery,
    dmtLeaseQueryDone,
    dmtActiveLeaseQuery,
    dmtLeaseQueryStatus,
    dmtTls);

 /// the main DHCP options - at least those parsed and recognized in this unit
 // - doPad is option 0 (not a true value)
 // - doSubnetMask is option 1 (255.255.255.0)
 // - doRouters is Default Gateway option 3 (192.168.1.1)
 // - doDomainNameServers is DNS Servers option 6 (8.8.8.8, 8.8.4.4)
 // - doHostName is option 12 (client01)
 // - doDomainName is option 15 (example.local)
 // - doBroadcastAddress is option 28 (192.168.1.255)
 // - doNtpServers is option 42
 // - doDhcpRequestedAddress is Requested IP Address option 50 (0.0.0.0)
 // - doDhcpLeaseTime is IP Lease Duration in seconds option 51 (86400 for 24h)
 // - doDhcpMessageType is option 53 (TDhcpMessageType) - first to appear
 // - doDhcpServerIdentifier is option 54 (192.168.1.1)
 // - doDhcpParameterRequestList is option 55 (1,3,6,15,51,54)
 // - doDhcpRenewalTime is T1 in seconds option 58 (43200 for 12h)
 // - doDhcpRebindingTime is T2 in seconds option 59 (75600 for 21h)
 // - doVendorClassIdentifier is PXE RFC 2132 option 60 (PXEClient)
 // - doDhcpClientIdentifier is option 61 ($01 + MAC)
 // - doTftpServerName is PXE option 66 (192.168.10.10 or host name)
 // - doBootFileName is PXE option 67 (pxelinux.0)
 // - doUserClass is PXE RFC 3004 option 77 (PXEClient:Arch:00000)
 // - doDhcpAgentOptions is Relay Agent (RFC 3046) option 82 - last to appear
 // - doSubnetSelection is option 118 to override the giaddr value
 // - doEnd is "End Of Options" marker option 255 (not a true value)
 TDhcpOption = (
   doPad,
   doSubnetMask,
   doRouters,
   doDomainNameServers,
   doHostName,
   doDomainName,
   doBroadcastAddress,
   doNtpServers,
   doDhcpRequestedAddress,
   doDhcpLeaseTime,
   doDhcpMessageType,
   doDhcpServerIdentifier,
   doDhcpParameterRequestList,
   doDhcpRenewalTime,
   doDhcpRebindingTime,
   doVendorClassIdentifier,
   doDhcpClientIdentifier,
   doTftpServerName,
   doBootFileName,
   doUserClass,
   doDhcpAgentOptions,
   doSubnetSelection,
   doEnd
 );
 /// set of supported DHCP options
 TDhcpOptions = set of TDhcpOption;
 /// pointer to a set of supported DHCP options
 PDhcpOptions = ^TDhcpOptions;

var
  /// uppercase identifier of each DHCP message type, e.g. 'DISCOVER' or 'ACK'
  DHCP_TXT: array[TDhcpMessageType] of RawUtf8;

  /// KEA identifier of each DHCP option, e.g. 'subnet-mask' or 'routers'
  // - https://kea.readthedocs.io/en/kea-3.1.4/arm/dhcp4-srv.html#standard-dhcpv4-options
  DHCP_OPTION: array[TDhcpOption] of RawUtf8;

function ToText(dmt: TDhcpMessageType): PShortString; overload;
function ToText(opt: TDhcpOption): PShortString; overload;

function FromText(const V: RawUtf8; out dmt: TDhcpMessageType): boolean; overload;
function FromText(const V: RawUtf8; out opt: TDhcpOption): boolean; overload;

const
  /// 1,3,6,15,28 options as used by default for DhcpClient()
  DHCP_REQUEST = [
    doSubnetMask, doRouters, doDomainNameServers, doDomainName, doBroadcastAddress];

/// initialize a DHCP client discover/request packet
// - returns pointer to @dhcp.options for additional DhcpAddOption() fluent calls
function DhcpClient(var dhcp: TDhcpPacket; dmt: TDhcpMessageType;
  const addr: TNetMac; req: TDhcpOptions = DHCP_REQUEST): PAnsiChar;

/// append a byte value to the TDhcpPacket.options packet
procedure DhcpAddOption(var p: PAnsiChar; op: TDhcpOption; b: byte); overload;
  {$ifdef FPC} inline; {$endif}

/// append a raw binary value to the TDhcpPacket.options packet
// - default length is 4 so that you could use just @SomeIP4
procedure DhcpAddOption(var p: PAnsiChar; op: TDhcpOption; b: pointer;
  len: PtrUInt = 4); overload;

/// append a non-TDhcpOption to the TDhcpPacket.options packet
// - could be used to append some unsupported option to the response
procedure DhcpAddOption(var p: PAnsiChar; op: byte; b: pointer; len: PtrUInt); overload;
  {$ifdef HASINLINE} inline; {$endif}

/// append a TNetIP4s dynamic array to the TDhcpPacket.options packet
// -  here ips is a <> nil pointer(dynamicarray) of TNetIP4 = cardinal
procedure DhcpAddOptions(var p: PAnsiChar; op: TDhcpOption; ips: PAnsiChar);

/// append a copy of an existing TDhcpPacket.options option for lens[opt]
// - sourcelen should point to recv[lens[opt]] with lens[opt] <> 0
procedure DhcpCopyOption(var p: PAnsiChar; sourcelen: PAnsiChar);


type
 /// efficient DhcpParse() results as O(1) lookup of recognized options
 // - 0 means that this TDhcpOption was not transmitted
 // - or store the position of an option length in TDhcpPacket.options[]
 TDhcpParsed = array[TDhcpOption] of byte;

/// parse a raw DHCP binary frame and return the length of all recognized options
// - returns dmtUndefined on invalid input DHCP frame
function DhcpParse(dhcp: PDhcpPacket; len: PtrInt; var lens: TDhcpParsed;
  found: PDhcpOptions = nil; mac: PNetMac = nil): TDhcpMessageType;

/// decode the pointer corresponding to lens[opt] within dhcp^.option[]
function DhcpData(dhcp: PDhcpPacket; len: PtrUInt): PShortString;
  {$ifdef HASINLINE} inline; {$endif}

/// decode the 32-bit IP address corresponding to lens[opt] within dhcp^.option[]
function DhcpIP4(dhcp: PDhcpPacket; len: PtrUInt): TNetIP4;
  {$ifdef HASINLINE} inline; {$endif}

/// decode the 32-bit big endian corresponding to lens[opt] within dhcp^.option[]
function DhcpInt(dhcp: PDhcpPacket; len: PtrUInt): cardinal;

/// decode the MAC address corresponding to lens[opt] within dhcp^.option[]
// - no DUID decoding is supported yet: only MAC or Eth=1 + MAC values
function DhcpMac(dhcp: PDhcpPacket; len: PtrUInt): PNetMac;

/// decode the lens[doDhcpParameterRequestList] within dhcp^.option[]
function DhcpRequestList(dhcp: PDhcpPacket; const lens: TDhcpParsed): TDhcpOptions;

type
  /// internal resultset used by ParseMacIP()
  TMacIP = record
    macp: pointer;
    ip: TNetIP4;
    mac: TNetMac;
  end;

/// parse a 'ip' or 'mac=ip' text into binary TNetIP4/TNetMac
function ParseMacIP(var nfo: TMacIP; const macip: RawUtf8): boolean;


{ **************** Middle-Level DHCP Scope and Lease Logic }

type
  /// the state of one DHCP TDhcpLease entry in memory
  // - lsFree identify an lsOutdated slot which IP4 has been reused
  // - lsReserved is used when an OFFER has been sent back to the client with IP4
  // - lsAck is used when REQUEST has been received and ACK has been sent back
  // - lsUnavailable is set when a DECLINE/INFORM message is received with an IP
  // - lsOutdated is set once Expired timestamp has been reached, so that the
  // IP4 address could be reused on the next DISCOVER for this MAC
  // - lsStatic when the lease comes for Scope.StaticMac[] but not Scope.Entry[]
  TLeaseState = (
    lsFree,
    lsReserved,
    lsAck,
    lsUnavailable,
    lsOutdated,
    lsStatic);

  /// store one DHCP lease in memory with all its logical properties
  // - efficiently padded to 16 bytes (128-bit), so 1,000 leases would use 16KB
  TDhcpLease = packed record
    /// the MAC address of this entry - should be the first of this record
    // - we only lookup clients by MAC: no DUID is supported (we found it error-
    // prone in practice, when some VMs are duplicated with the same DUID)
    // - may contain 0 for a lsFree or lsUnavailable sentinel entry
    Mac: TNetMac;
    /// how this entry should be handled
    State: TLeaseState;
    /// how many lsUnavailable IPs have been marked since the last OnIdle
    // - OnIdle() will reset it to 0, ComputeResponse() will increment it, and
    // silently ignore any abusive DECLINE/INFORM requests as rate limitation
    // - also align the whole structure to cpu-cache-friendly 16 bytes
    RateLimit: byte;
    /// the 32-bit reserved IP
    IP4: TNetIP4;
    /// GetTickSec monotonic value stored as 32-bit unsigned integer
    // - TUnixTime is used on disk but not during server process (not monotonic)
    // - for lsStatic as in Scope.StaticMac[], is the last seen timestamp
    Expired: cardinal;
  end;
  /// points to one DHCP lease entry in memory
  PDhcpLease = ^TDhcpLease;

  /// a dynamic array of TDhcpLease entries, as stored within TDhcpProcess
  // - 10,000 leases would consume 160KB which fits in L2 cache on modern CPU
  TLeaseDynArray = array of TDhcpLease;

  /// how to refine DHCP server process for one TDhcpScopeSettings
  // - dsoInformRateLimit will track INFORM per MAC and limit to 3 per second
  // (not included by default since seems overkill and KEA/Windows don't do it)
  TDhcpScopeOption = (
    dsoInformRateLimit
    );
  /// refine DHCP server process for one TDhcpScopeSettings
  TDhcpScopeOptions = set of TDhcpScopeOption;

  /// define one scope/subnet processing context from TDhcpServerSettings
  // - fields and methods are optimized to efficiently map TDhcpProcess logic
  // - most methods are not thread-safe: the caller should make Safe.Lock/UnLock
  // - each record consume a bit less than 128 bytes of memory
  {$ifdef USERECORDWITHMETHODS}
  TDhcpScope = record
  {$else}
  TDhcpScope = object
  {$endif USERECORDWITHMETHODS}
  public
    /// simple but not-rentrant lock to protect Entry[0..Count-1] values
    Safe: TLightLock;
    /// define the subnet of this scope
    // - Subnet.mask is the doSubnetMask option 1 of this scope
    Subnet: TIp4SubNet;
    /// store the current leases on this scope
    Entry: TLeaseDynArray;
    /// number of used slots in Entry[]
    Count: integer;
    /// the broadcast IP of this scope for doBroadcastAddress option 28
    Broadcast: TNetIP4;
    /// the gateway IP of this scope for doRouter option 3
    Gateway: TNetIP4;
    /// the server IP of this scope for doDhcpServerIdentifier option 54
    ServerIdentifier: TNetIP4;
    /// the DNS IPs of this scope for doDomainNameServers option 6
    DnsServer: TIntegerDynArray;
    /// list of static IPs of this scope
    // - sorted as 32-bit for efficient O(log(n)) branchless binary search
    // - also contain the StaticMac[].IP4 values for fast lookup
    StaticIP: TIntegerDynArray;
    /// list of MAC addresses with their static IP for this scope
    StaticMac: TLeaseDynArray;
    /// readjust all internal values according to to Subnet policy
    // - raise an EDhcp exception if the parameters are not correct
    procedure AfterFill(log: TSynLog);
    /// register another static IP address to the internal pool
    // - can be associated with a fixed MAC address for resolution
    function AddStatic(ip4: TNetIP4; mac: PNetMac): boolean; overload;
    /// register another static IP address to the internal pool
    // - value is expected to be supplied as 'ip' or 'mac=ip' text
    function AddStatic(const macip: RawUtf8): boolean; overload;
    /// remove one static IP address which was registered by AddStatic()
    function RemoveStatic(ip4: TNetIP4): boolean;
    /// efficient L1-cache friendly O(n) search of a MAC address in Entry[]
    // - will also search in StaticMac[] entries
    function FindMac(mac: Int64): PDhcpLease;
    /// efficient L1-cache friendly O(n) search of an IPv4 address in Entry[]
    // - won't search in StaticIP[] and StaticMac[]
    function FindIp4(ip4: TNetIP4): PDhcpLease;
    /// retrieve a new lease in Entry[] - maybe recycled from ReuseIp4()
    function NewLease: PDhcpLease;
    /// release a lease in Entry[] - add it to the recycled FreeList[]
    function ReuseIp4(p: PDhcpLease): TNetIP4;
    /// return the index in Entry[] of a given lease
    // - no range check is performed against the supplied p pointer
    function Index(p: PDhcpLease): cardinal;
      {$ifdef HASINLINE} inline; {$endif}
    /// quickly O(log(n)) check if an IPv4 is in StaticIP[]
    function IsStaticIP(ip4: TNetIP4): boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// compute the next IPv4 address available in this scope
    function NextIp4: TNetIP4;
    /// call DhcpAddOptions() with the settings of this scope
    procedure AddOptions(var f: PAnsiChar; withLeaseTimes: boolean);
    /// check all TDhcpLease.Expired against tix32
    // - return the number of leases marked as lsOutdated
    // - this method is thread safe and will call Safe.Lock/UnLock
    function CheckOutdated(tix32: cardinal): integer;
    /// persist all leases with "<expiry> <MAC> <IP>" dnsmasq file pattern
    // - this method is thread safe and will call Safe.Lock/UnLock
    // - returns the number of entries/lines added to the text file
    // - localcopy=true would make a transient copy of Entry[] to reduce the lock
    function TextWrite(W: TTextWriter; tix32: cardinal; boot: TUnixTime;
      localcopy: boolean): integer;
  private
    Options: TDhcpScopeOptions;
    // little-endian IP range values for NextIP
    LastIpLE, IpMinLE, IpMaxLE: TNetIP4;
    // match scope settings values in efficient actionable format
    LeaseTime, LeaseTimeLE, RenewalTime, Rebinding, OfferHolding: cardinal;
    MaxDeclinePerSec, DeclineTime, GraceFactor: cardinal;
    // some internal values for fast lookup and efficient Entry[] process
    LastDiscover, FreeListCount: integer;
    FreeList: TIntegerDynArray;
  end;

  /// access to one scope/subnet definition
  PDhcpScope = ^TDhcpScope;

  /// store all scope/subnet definitions in a dynamic array
  TDhcpScopes = array of TDhcpScope;

function ToText(st: TLeaseState): PShortString; overload;


{ **************** High-Level Multi-Scope DHCP Server Processing Logic }

type
  TDhcpProcess = class;

  /// main high-level options for defining one scope/subnet for our DHCP Server
  TDhcpScopeSettings = class(TSynPersistent)
  protected
    fSubnetMask: RawUtf8;
    fStatic: TRawUtf8DynArray;
    fRangeMin: RawUtf8;
    fRangeMax: RawUtf8;
    fDefaultGateway: RawUtf8;
    fDnsServers: RawUtf8;
    fDomainName: RawUtf8;
    fBroadCastAddress: RawUtf8;
    fLeaseTimeSeconds: cardinal;
    fMaxDeclinePerSecond: cardinal;
    fDeclineTimeSeconds: cardinal;
    fServerIdentifier: RawUtf8;
    fOfferHoldingSecs: cardinal;
    fGraceFactor: cardinal;
    fOptions: TDhcpScopeOptions;
  public
    /// setup this instance with default values
    // - default are just SubnetMask = '192.168.1.1/24', LeaseTimeSeconds = 120
    // and OfferHoldingSecs = 5, consistent with a simple local iPXE network
    constructor Create; override;
    /// compute the low-level TDhcpScope data structure for current settings
    // - raise an EDhcp exception if the parameters are not correct
    procedure PrepareScope(Sender: TDhcpProcess; var Data: TDhcpScope);
  published
    /// Subnet Mask e.g. '192.168.1.1/24'
    // - the CIDR 'ip/mask' pattern will compute RangeMin/RangeMax and
    // ServerIdentifier directly from this pattern
    // - accept also plain '255.255.255.0' IP if you want to specify by hand the
    // raw value sent in DHCP headers, and fill RangeMin/RangeMax and others
    property SubnetMask: RawUtf8
      read fSubnetMask write fSubnetMask;
    /// some static IP addresses potentially with their MAC, reserved on the network
    // - supplied as 'ip' or 'mac=ip' items
    // - e.g. ['192.168.1.2','2f:af:9e:0f:b8:2a=192.168.1.100']
    property Static: TRawUtf8DynArray
      read fStatic write fStatic;
    /// minimal IP range e.g. '192.168.1.10'
    // - default is '' and will be filled by SubnetMask value as 10..254
    property RangeMin: RawUtf8
      read fRangeMin write fRangeMin;
    /// maximal IP range e.g. '192.168.1.254'
    // - default is '' and will be filled by SubnetMask value as 10..254
    property RangeMax: RawUtf8
      read fRangeMax write fRangeMax;
    /// Default Gateway e.g. '192.168.1.1' - default is ''
    property DefaultGateway: RawUtf8
      read fDefaultGateway write fDefaultGateway;
    /// DNS Servers as CSV (option 6) e.g. '8.8.8.8,8.8.4.4' - default is ''
    property DnsServers: RawUtf8
      read fDnsServers write fDnsServers;
    /// Domain Name e.g. 'lan.local' - default is ''
    property DomainName: RawUtf8
      read fDomainName write fDomainName;
    /// Broadcast Address e.g. '192.168.1.255' - default is ''
    property BroadCastAddress: RawUtf8
      read fBroadCastAddress write fBroadCastAddress;
    /// IP Lease Duration in seconds - default is 120 for 2 minutes
    // - options 51/58/59 Lease/Renewal/Rebinding will use 100/50/87.5 percents
    // - default 120 secs seems fine for our minimal iPXE-oriented DHCP server
    property LeaseTimeSeconds: cardinal
      read fLeaseTimeSeconds write fLeaseTimeSeconds;
    /// how many DECLINE requests are allowed per second before ignoring them
    // - a malicious client can poison the pool by sending repeated DECLINEs
    // - default is 5 which seems reasonable and conservative
    // - note that INFORM rate limitation is hardcoded to 3 per second per MAC
    property MaxDeclinePerSecond: cardinal
      read fMaxDeclinePerSecond write fMaxDeclinePerSecond default 5;
    /// IP Decline Duration in seconds
    // - default to 0, to reuse the same value than LeaseTimeSeconds
    // - if LeaseTimeSeconds is small, you could set a bigger value here to be
    // more conservative about the static IP persistence in the network
    property DeclineTimeSeconds: cardinal
      read fDeclineTimeSeconds write fDeclineTimeSeconds;
    /// DHCP Server Identifier e.g. '192.168.1.1'
    // - default is '' and will be filled by SubnetMask value
    property ServerIdentifier: RawUtf8
      read fServerIdentifier write fServerIdentifier;
    /// how many seconds a DHCP dmtOffer would stay available - default is 5
    property OfferHoldingSecs: cardinal
      read fOfferHoldingSecs write fOfferHoldingSecs;
    /// if lease time is < 1 hour, allow relaxed lease expansion after expiry
    // - default is 2, meaning that for PXE with LeaseTimeSeconds = 120 < 1 hour,
    // a grace period of 240 seconds
    // - this grace delay is disabled if GraceFactor = 0 or for long leases > 1h
    property GraceFactor: cardinal
      read fGraceFactor write fGraceFactor default 2;
    /// refine DHCP server process for this scope
    // - default is [] but you may tune it for your actual network needs
    property Options: TDhcpScopeOptions
      read fOptions write fOptions;
  end;
  /// a dynamyc array of DHCP Server scope/subnet settings
  TDhcpScopeSettingsObjArray = array of TDhcpScopeSettings;

  /// main high-level options for defining our DHCP Server process
  TDhcpServerSettings = class(TSynAutoCreateFields)
  protected
    fFileName: TFileName;
    fFileFlushSeconds: cardinal;
    fScope: TDhcpScopeSettingsObjArray;
  public
    /// setup this instance with default values
    // - no scope is defined yet: you could just call AddScope to create
    // one default scope/subnet
    constructor Create; override;
    /// validate the consistency of settings, mainly about IPv4 subnets
    // - returns '' if no error occured with the current property values
    // - returns the TDhcpProcess.Setup() Exception message on failure
    function Verify: string;
    /// append at runtime a new scope/subnet settings
    // - call without any TDhcpScopeSettings parameter to create a default one
    // - supplied one will be owned by this instance from now on
    procedure AddScope(one: TDhcpScopeSettings = nil);
  published
    /// if set, OnIdle will persist the internal list into this file when needed
    // - file on disk would be regular dnsmasq-compatible format
    property FileName: TFileName
      read fFileName write fFileName;
    /// at which pace the FileName should be written on disk by OnIdle
    // - default is 30 seconds which seems light and safe enough
    // - set to 0 will disable background write, and persist only at shutdown
    property FileFlushSeconds: cardinal
      read fFileFlushSeconds write fFileFlushSeconds default 30;
    /// store the per subnet scope settings
    property Scope: TDhcpScopeSettingsObjArray
      read fScope;
  end;

  /// data context used by TDhcpProcess.ComputeResponse to process a request
  // - allow stack allocation of all memory needed during the frame processing
  // - also supplied to TOnComputeResponse callback as thread-safe context
  TDhcpProcessData = {$ifdef CPUINTEL} packed {$endif} record
    /// binary MAC address from the Recv frame, zero-extended to 64-bit/8-bytes
    Mac64: Int64;
    /// points to the last option of Send buffer
    // - callback could use this to call DhcpAddOption() overloads
    SendEnd: PAnsiChar;
    /// 32-bit binary IP address allocated for Send
    Ip4: TNetIP4;
    /// the GetTickSec current value
    Tix32: cardinal;
    /// the network subnet information related to this request
    // - Scope^.Safe.Lock has been done by TOnComputeResponse callback caller
    Scope: PDhcpScope;
    /// some pointer value set by the UDP server for its internal process
    Opaque: pointer;
    /// points to the raw value of doHostName option 12 in Recv.option[]
    // - points to @NULCHAR so HostName^='' if there is no such option
    HostName: PShortString;
    /// length of the Recv UDP frame received from the client
    RecvLen: integer;
    /// the server IP socket which received the UDP frame
    // - allow several UDP bound server sockets to share a single TDhcpProcess
    RecvIp4: TNetIP4;
    /// parsed options length position in Recv.option[]
    RecvLens: TDhcpParsed;
    /// the DHCP message type parsed from Recv
    RecvType: TDhcpMessageType;
    /// the DHCP message type prepared into Send
    SendType: TDhcpMessageType;
    /// UDP frame received from the client, parsed in RecvLens[]
    Recv: TDhcpPacket;
    /// UDP frame to be sent back to the client, after processing
    Send: TDhcpPacket;
    /// IP address allocated for Send (raw Ip4) as human readable text
    Ip: TShort16;
    /// contains the client MAC address (raw Mac64) as human readable text
    // - ready for logging, with no memory allocation during the process
    Mac: string[17];
  end;

  /// optional callback signature for TDhcpProcess.ComputeResponse
  // - input frame is parsed in Data.Recv/RecvLen/RecvLens
  // - could update SendEnd with DhcpAddOption() or set SendEnd=nil for no response
  TOnComputeResponse = procedure(Sender: TDhcpProcess; var Data: TDhcpProcessData);

  /// implements our DHCP server logic, abstracted from UDP/Socket reference
  // - main methods are Setup() and ComputeResponse() from TDhcpServer
  // - store and redirect all process to TDhcpScopes in-memory per-subnet lists
  // - several sockets could be bound in TDhcpServer, each with its own IP, and
  // on its own interface, all sharing this single TDhcpProcess subnets
  TDhcpProcess = class(TSynPersistent)
  protected
    fScopeSafe: TRWLightLock; // multi-read reentrant lock to protect Scope[]
    fScope: TDhcpScopes;
    fFileFlushSeconds: cardinal;
    fIdleTix, fFileFlushTix, fModifSequence, fModifSaved: cardinal;
    fLog: TSynLogClass;
    fFileName: TFileName;
    fOnComputeResponse: TOnComputeResponse;
    fState: (sNone, sSetup, sSetupFailed, sShutdown);
    function GetCount: integer;
    procedure SetFileName(const name: TFileName);
    function ParseFrame(var Data: TDhcpProcessData): boolean; virtual;
    function RetrieveFrameIP(var Data: TDhcpProcessData; Lease: PDhcpLease): boolean; virtual;
    function FinalizeFrame(var Data: TDhcpProcessData): PtrInt; virtual;
  public
    /// setup this DHCP process using the specified settings
    // - raise an EDhcp exception if the parameters are not correct - see
    // TDhcpServerSettings.Verify() if you want to intercept such errors
    // - if aSettings = nil, will use TDhcpServerSettings default parameters
    procedure Setup(aSettings: TDhcpServerSettings = nil);
    /// register another static 'ip' or 'mac=ip' address to the internal pool
    // - in addition to aScopeSettings.Static array
    // - could be used at runtime depending on the network logic
    // - those static IPs are not persisted in FileName/SaveToFile
    function AddStatic(const macip: RawUtf8): boolean;
    /// remove one static IP address which was registered by AddStatic()
    function RemoveStatic(const ip: RawUtf8): boolean;
    /// flush the internal lease lists and all Scope[] definitions
    procedure Clear;
    /// flush the internal lease lists but keep Scope[] definitions
    procedure ClearLeases;
    /// return the Scope[] entry matching its subnet mask
    // - can be used e.g. to check if IP address match the DHCP server subnets
    // - this method is not thread-safe by design
    function GetScope(ip: TNetIP4): PDhcpScope; overload;
    /// return the Scope[] entry matching its subnet mask
    // - can be used e.g. to check if IP address match the DHCP server subnets
    // - this method is not thread-safe by design
    function GetScope(const ip: RawUtf8): PDhcpScope; overload;
    /// should be called when the server is about to leave for a clean shutdown
    // - will mark the internal state as "do-nothing" and persist FileName
    // - won't clear the internal list
    // - if you want to re-start the server, call Setup() again
    procedure Shutdown;
    /// to be called on regular pace to make some background process
    // - every second, identify and return the number of outdated entries
    // - if FileName was set, would persist leases at FileFlushSeconds=30 pace
    function OnIdle(tix64: Int64): integer;
    /// persist the internal entry list using human-readable text format
    // - follow "<expiry> <MAC> <IP>" dnsmasq lease file pattern, using the
    // standard seconds since epoch (Unix time) for the <expiry> value, e.g.
    // $ 1770034159 c2:07:2c:9d:eb:71 192.168.1.207
    // - we skip <hostname> and <client id> values since we don't handle them
    // - by design, only lsAck and recently lsOutdated entries are included
    function SaveToText(SavedCount: PInteger = nil): RawUtf8;
    /// restore the internal entry list using SaveToText() format
    function LoadFromText(const Text: RawUtf8): boolean;
    /// persist the internal entry list using SaveToText() format
    // - returns the number of entries stored in the file, or -1 on write error
    function SaveToFile(const FileName: TFileName): integer;
    /// restore the internal entry list using SaveToText() format
    // - should be done before Setup() to validate the settings network mask
    function LoadFromFile(const FileName: TFileName): boolean;
    /// this is the main processing function of the DHCP server logic
    // - input should be stored in Data.Recv/RecvLen/RecvIp4 - and is untouched
    // - returns -1 if this input was invalid or unsupported
    // - returns 0 if input was valid, but no response is needed (e.g. decline)
    // - return > 0 the number of Data.Send bytes to broadcast back as UDP
    function ComputeResponse(var Data: TDhcpProcessData): PtrInt;
    /// raw access to the internal scope/subnet lists
    // - this property is not thread-safe by design
    property Scope: TDhcpScopes
      read fScope;
    /// raw multi-read reentrant lock to protect Scope[]
    property ScopeSafe: TRWLightLock
      read fScopeSafe;
    /// the associated TSynLog class used to debug the execution context
    property Log: TSynLogClass
      read fLog write fLog;
    /// how many leases are currently reserved in memory in all Scope[] subnets
    property Count: integer
      read GetCount;
    /// if set, OnIdle will persist the internal list into this file when needed
    // - trigger LoadFromFile() when you set a file name
    // - at startup, you should set the file name before calling Setup()
    // - file on disk would be in SaveToText() regular dnsmasq-compatible format
    // - it is easier and cleaner to use TDhcpServerSettings.FileName instead
    property FileName: TFileName
      read fFileName write SetFileName;
    /// the internal list modification sequence number
    // - increased at every update of the internal entry list
    // - used e.g. by OnIdle() to trigger SaveToFile() if FileName is defined
    property ModifSequence: cardinal
      read fModifSequence;
    /// optional callback for TDhcpProcess.ComputeResponse customization
    // - is called after parsing, when Data.Send is about to be returned
    property OnComputeResponse: TOnComputeResponse
      read fOnComputeResponse write fOnComputeResponse;
  end;


implementation


{ **************** Low-Level DHCP Protocol Definitions }

function ToText(dmt: TDhcpMessageType): PShortString;
begin
  result := GetEnumName(TypeInfo(TDhcpMessageType), ord(dmt));
end;

function ToText(opt: TDhcpOption): PShortString;
begin
  result := GetEnumName(TypeInfo(TDhcpOption), ord(opt));
end;

function FromText(const V: RawUtf8; out dmt: TDhcpMessageType): boolean;
begin
  result := GetEnumNameValue(TypeInfo(TDhcpMessageType), V, dmt, @DHCP_TXT);
end;

function FromText(const V: RawUtf8; out opt: TDhcpOption): boolean;
begin
  result := GetEnumNameValue(TypeInfo(TDhcpOption), V, opt, @DHCP_OPTION);
end;

const
  DHCP_OPTION_NUM: array[TDhcpOption] of byte = (
    0, 1, 3, 6, 12, 15, 28, 42, 50, 51, 53, 54, 55,
    58, 59, 60, 61, 66, 67, 77, 82, 118, 255);
var
  DHCP_OPTION_INV: array[0 .. 118] of TDhcpOption; // for fast O(1) lookup

procedure DhcpAddOption(var p: PAnsiChar; op: TDhcpOption; b: byte);
begin
  PCardinal(p)^ := cardinal(DHCP_OPTION_NUM[op]) + 1 shl 8 + cardinal(b) shl 16;
  p := @p[3];
end;

procedure DhcpAddOption(var p: PAnsiChar; op: byte; b: pointer; len: PtrUInt);
var
  d: PByteArray;
begin
  d := pointer(p);
  d[0] := op;
  d[1] := len;
  if len <> 0 then
    MoveFast(b^, d[2], len);
  p := @d[len + 2];
end;

procedure DhcpAddOption(var p: PAnsiChar; op: TDhcpOption; b: pointer; len: PtrUInt);
begin
  DhcpAddOption(p, DHCP_OPTION_NUM[op], b, len);
end;

procedure DhcpAddOptions(var p: PAnsiChar; op: TDhcpOption; ips: PAnsiChar);
begin
  // ips is a <> nil dynamic array of TNetIP4 = cardinal
  DhcpAddOption(p, op, pointer(ips), (PDALen(ips - _DALEN)^ + _DAOFF) * 4);
end;

procedure DhcpCopyOption(var p: PAnsiChar; sourcelen: PAnsiChar);
begin
  // copy whole "DHCP_OPTION_NUM + len + data" binary block
  MoveFast(sourcelen[-1], p^, ord(sourcelen^) + 2);
  inc(p, ord(sourcelen^) + 2);
end;

function DhcpAddOptionRequestList(p: PAnsiChar; op: TDhcpOptions): PAnsiChar;
var
  o: TDhcpOption;
begin
  PCardinal(p)^ := DHCP_OPTION_NUM[doDhcpParameterRequestList]; // op + len=0
  inc(p); // p[0]=len
  for o := succ(low(o)) to pred(high(o)) do
    if o in op then
    begin
      inc(p[0]);
      p[ord(p[0])] := AnsiChar(DHCP_OPTION_NUM[o]);
    end;
  result := @p[ord(p[0]) + 1];
end;

const
  DHCP_MAGIC_COOKIE = $63538263;  // little-endian 'cScC'

  DHCP_BOOT: array[TDhcpMessageType] of byte = (
    0,            // dmtUndefined
    BOOT_REQUEST, // dmtDiscover
    BOOT_REPLY,   // dmtOffer
    BOOT_REQUEST, // dmtRequest
    BOOT_REQUEST, // dmtDecline
    BOOT_REPLY,   // dmtAck
    BOOT_REPLY,   // dmtNak
    BOOT_REQUEST, // dmtRelease
    BOOT_REQUEST, // dmtInform
    BOOT_REQUEST, // dmtForceRenew
    BOOT_REQUEST, // dmtLeaseQuery
    BOOT_REPLY,   // dmtLeaseUnassigned
    BOOT_REPLY,   // dmtLeaseUnknown
    BOOT_REPLY,   // dmtLeaseActive
    BOOT_REQUEST, // dmtBulkLeaseQuery
    BOOT_REPLY,   // dmtLeaseQueryDone
    BOOT_REQUEST, // dmtActiveLeaseQuery
    BOOT_REPLY,   // dmtLeaseQueryStatus
    BOOT_REPLY);  // dmtTls

  ARPHRD_ETHER = 1; // from linux/if_arp.h

var
  DhcpClientId: integer; // thread-safe global random-initialized sequence

function DhcpNew(var dhcp: TDhcpPacket; dmt: TDhcpMessageType; xid: cardinal;
  const addr: TNetMac; serverid: TNetIP4 = 0): PAnsiChar;
begin
  FillCharFast(dhcp, SizeOf(dhcp) - SizeOf(dhcp.options), 0);
  dhcp.op := DHCP_BOOT[dmt];
  dhcp.htype := ARPHRD_ETHER;
  dhcp.hlen := SizeOf(addr);
  if xid = 0 then
  begin
    if DhcpClientId = 0 then
      DhcpClientId := Random32Not0;
    xid := InterlockedIncrement(DhcpClientId);
  end;
  dhcp.xid := xid;
  dhcp.flags := $8000; // not unicast in this userland UDP socket API unit
  PNetMac(@dhcp.chaddr)^ := addr;
  dhcp.cookie := DHCP_MAGIC_COOKIE;
  result := @dhcp.options;
  DhcpAddOption(result, doDhcpMessageType, ord(dmt));
  if serverid <> 0 then
  begin
    dhcp.siaddr := serverid;
    DhcpAddOption(result, doDhcpServerIdentifier, @serverid);
  end;
  result^ := #255;
end;

function DhcpClient(var dhcp: TDhcpPacket; dmt: TDhcpMessageType;
  const addr: TNetMac; req: TDhcpOptions): PAnsiChar;
begin
  result := DhcpNew(dhcp, dmt, {xid=}0, addr);
  result := DhcpAddOptionRequestList(result, req);
  result^ := #255; // only for internal/debug use
end;

function DhcpParse(dhcp: PDhcpPacket; len: PtrInt; var lens: TDhcpParsed;
  found: PDhcpOptions; mac: PNetMac): TDhcpMessageType;
var
  p: PAnsiChar;
  m: PNetMac;
  opt: TDhcpOption;
  dmt: byte;   // TDhcpMessageType
begin
  result := dmtUndefined;
  if found <> nil then
    found^ := [];
  FillCharFast(lens, SizeOf(lens), 0);
  // validate header
  dec(len, PtrInt(PtrUInt(@PDhcpPacket(nil)^.options)));
  if (len <= 0) or
     (dhcp = nil) or
     (dhcp^.cookie <> DHCP_MAGIC_COOKIE) or
     (dhcp^.htype <> ARPHRD_ETHER) or
     (dhcp^.hlen <> SizeOf(TNetMac)) or
     (dhcp^.xid = 0) or
     not (dhcp^.op in [BOOT_REQUEST, BOOT_REPLY]) then
    exit;
  // parse DHCP options
  p := @dhcp^.options;
  repeat
    dec(len, ord(p[1]) + 2);
    if len < 1 then
      exit; // avoid buffer overflow
    // fast O(1) option number lookup and lens[] initialization
    opt := doPad;
    if ord(p[0]) <= high(DHCP_OPTION_INV) then
      opt := DHCP_OPTION_INV[ord(p[0])];
    if opt <> doPad then
    begin
      if found <> nil then
        include(found^, opt);
      lens[opt] := PAnsiChar(@p[1]) - PAnsiChar(@dhcp^.options);
    end;
    // just ignore unsupported options
    p := @p[ord(p[1]) + 2];
  until p^ = #255;
  // validate message consistency
  dmt := lens[doDhcpMessageType];
  if (dmt = 0) or                   // not set
     (dhcp^.options[dmt] <> 1) then // length
    exit;
  dmt := dhcp^.options[dmt + 1];    // value
  if (dmt > byte(high(TDhcpMessageType))) or
     (DHCP_BOOT[TDhcpMessageType(dmt)] <> dhcp^.op) then // inconsistent type
    exit;
  // here we have a valid DHCP frame
  if mac <> nil then
  begin
    m := DhcpMac(dhcp, lens[doDhcpClientIdentifier]);
    if m = nil then
      m := @dhcp^.chaddr; // no option 61: fallback to BOOTP value
    mac^ := m^; // copy
  end;
  result := TDhcpMessageType(dmt);
end;

function DhcpData(dhcp: PDhcpPacket; len: PtrUInt): PShortString;
begin
  if len = 0 then
    result := @NULCHAR
  else
    result := @dhcp^.options[len]; // len+data matches PShortString binary
end;

function DhcpIP4(dhcp: PDhcpPacket; len: PtrUInt): TNetIP4;
begin
  result := 0;
  if len = 0 then
    exit;
  inc(len, PtrUInt(@dhcp.options));
  if PByte(len)^ = SizeOf(result) then
    result := PCardinal(len + 1)^;
end;

function DhcpInt(dhcp: PDhcpPacket; len: PtrUInt): cardinal;
begin
  result := bswap32(DhcpIP4(dhcp, len));
end;

function DhcpMac(dhcp: PDhcpPacket; len: PtrUInt): PNetMac;
begin
  result := nil;
  if len = 0 then
    exit;
  inc(len, PtrUInt(@dhcp.options));
  case PByte(len)^ of // e.g. client identifier
    SizeOf(TNetMac):
      // PXE clients often use MAC-only (6 bytes)
      result := pointer(len + 1);
    SizeOf(TNetMac) + 1:
      // Windows PE or iPXE may use 1-byte type + MAC (7 bytes)
      if PByte(len + 1)^ = ARPHRD_ETHER then
        result := pointer(len + 2);
  end;
end;

function DhcpRequestList(dhcp: PDhcpPacket; const lens: TDhcpParsed): TDhcpOptions;
var
  p, n: PtrUInt;
  opt: TDhcpOption;
begin
  result := [];
  p := lens[doDhcpParameterRequestList];
  if p = 0 then
    exit;
  inc(p, PtrUInt(@dhcp.options));
  n := PByte(p)^;
  if n <> 0 then
    repeat
      inc(p);
      opt := doPad;
      if PByte(p)^ <= high(DHCP_OPTION_INV) then
        opt := DHCP_OPTION_INV[PByte(p)^]; // fast O(1) option number lookup
      if opt <> doPad then
        include(result, opt);
      dec(n);
    until n = 0;
end;

function ParseMacIP(var nfo: TMacIP; const macip: RawUtf8): boolean;
var
  mac, ip: RawUtf8;
  p: PUtf8Char;
begin
  result := false;
  nfo.macp := nil;
  if TrimSplit(macip, mac, ip, '=') then // 'mac=ip' format
    if (length(mac) = 17) and // exact 'xx:xx:xx:xx:xx:xx' format
       (TextToMac(pointer(mac), @nfo.mac)) then
    begin
      nfo.macp := @nfo.mac;
      p := pointer(ip);
    end
    else
      exit // invalid MAC
  else
    p := pointer(macip); // only 'ip'
  result := NetIsIP4(p, @nfo.ip); // valid IP
end;


{ **************** Middle-Level DHCP Scope and Lease Logic }

function ToText(st: TLeaseState): PShortString;
begin
  result := GetEnumName(TypeInfo(TLeaseState), ord(st));
end;


{ TDhcpScope }

const
  // truncate 64-bit integer to 6-bytes TNetMac - efficient on Intel and aarch64
  MAC_MASK = $0000ffffffffffff;
  // hardcore limit of dmtInform to 3 IPs per second
  MAX_INFORM = 3;
  // pre-allocate 4KB of working entries e.g. in TDhcpScope.AfterFill
  PREALLOCATE_LEASES = 250;

// code below expects PDhcpLease^.Mac to be the first field

function DoFindMac(p: PDhcpLease; mac: Int64; n: cardinal): PDhcpLease;
begin // dedicated sub-function for better codegen
  result := p;
  if n <> 0 then
    repeat
      if PInt64(result)^ and MAC_MASK = mac then
        exit;
      inc(result);
      dec(n);
    until n = 0;
  result := nil;
end;

function TDhcpScope.FindMac(mac: Int64): PDhcpLease;
begin
  mac := mac and MAC_MASK;
  // naive but efficient lookup of the last added lease during DHCP negotiation
  if cardinal(LastDiscover) < cardinal(Count) then
  begin
    result := @Entry[LastDiscover];
    if PInt64(result)^ and MAC_MASK = mac then
      exit;
  end;
  // search if this MAC has a static IP allocated
  result := DoFindMac(pointer(StaticMac), mac, length(StaticMac));
  if result <> nil then
    result^.State := lsStatic // force for ComputeResponse() logic
  else
    // O(n) brute force search in L1/L2 cache is fine up to 100,000 items
    result := DoFindMac(pointer(Entry), mac, Count);
end;

function DoFindIp(p: PDhcpLease; ip4: TNetIP4; n: cardinal): PDhcpLease;
begin // dedicated sub-function for better codegen
  result := p;
  if n <> 0 then
    repeat
      if result^.IP4 = ip4 then
        exit;
      inc(result);
      dec(n);
    until n = 0;
  result := nil;
end;

function TDhcpScope.FindIp4(ip4: TNetIP4): PDhcpLease;
begin
  result := DoFindIp(pointer(Entry), ip4, Count);
end;

function TDhcpScope.NewLease: PDhcpLease;
var
  n: PtrInt;
begin
  // first try if we have any lsFree entries from ReuseIp4()
  if FreeListCount <> 0 then
  begin
    dec(FreeListCount);
    result := @Entry[FreeList[FreeListCount]]; // LIFO is the fastest
    if result^.State = lsFree then
      exit; // paranoid
  end;
  // we need to add a new entry - maybe with reallocation
  n := Count;
  if n = length(Entry) then
    SetLength(Entry, NextGrow(n));
  result := @Entry[n];
  inc(Count);
end;

function TDhcpScope.Index(p: PDhcpLease): cardinal;
begin
  result := cardinal(PtrUInt(p) - PtrUInt(Entry)) shr 4;
end;

function TDhcpScope.IsStaticIP(ip4: TNetIP4): boolean;
begin // use branchless asm on x86_64
  result := FastFindIntegerSorted(StaticIP, ip4) >= 0;
end;

function TDhcpScope.ReuseIp4(p: PDhcpLease): TNetIP4;
begin
  AddInteger(FreeList, FreeListCount, Index(p));
  result := p^.IP4;       // return this IP4
  FillZero(THash128(p^)); // set MAC=0 IP=0 State=lsFree
end;

function TDhcpScope.NextIp4: TNetIP4;
var
  le: TNetIP4;
  looped: boolean;
  existing, outdated: PDhcpLease;
begin
  result := LastIpLE; // all those f*LE variables are in little-endian order
  if (result < IpMinLE) or
     (result > IpMaxLE) then
    result := IpMinLE - 1; // e.g. at startup
  outdated := nil;
  looped := false;
  repeat
    inc(result);
    if result > IpMaxLE then
      // we reached the end of IP range
      if looped then
      begin
        // all IPs are already used in the internal list
        if outdated <> nil then
          // reuse the oldest outdated slot
          result := ReuseIP4(outdated) // set MAC=0 IP=0 State=lsFree
        else
          // pool exhausted: consider shorten the lease duration to 60-300 secs
          result := 0;
        exit;
      end
      else
      begin
        // wrap around the IP range once
        looped := true;
        result := IpMinLE;
      end;
    le := result;
    result := bswap32(result);     // back to network order
    if not IsStaticIP(result) then // O(log(n)) fast lookup
    begin
      existing := DoFindIp(pointer(Entry), result, Count);  // O(n)
      if existing = nil then
      begin
        // return the unused IP found
        LastIpLE := le;
        exit;
      end
      else if (existing^.State in [lsUnavailable, lsOutdated]) and
              ((outdated = nil) or
               (existing^.Expired < outdated^.Expired)) then
        outdated := existing; // detect oldest outdated entry
    end;
    result := le;
  until false;
end;

procedure TDhcpScope.AfterFill(log: TSynLog);
var
  i, n: PtrInt;
  p: PDhcpLease;

  procedure CheckSubNet(ident: PUtf8Char; ip: TNetIP4);
  begin
    if not Subnet.Match(ip) then
      EDhcp.RaiseUtf8(
        'PrepareScope: SubNetMask=% does not match %=%',
        [Subnet.Mask, ident, IP4ToShort(@ip)]);
  end;

begin
  // validate all settings values from the actual subnet
  if Gateway <> 0 then
    CheckSubnet('DefaultGateway', Gateway);
  CheckSubnet('ServerIdentifier', ServerIdentifier);
  if IpMinLE = 0 then
    IpMinLE := Subnet.ip + $0a000000; // e.g. 192.168.0.10
  CheckSubnet('RangeMin', IpMinLE);
  if IpMaxLE = 0 then
    // e.g. 192.168.0.0 + pred(not(255.255.255.0)) = 192.168.0.254
    IpMaxLE := bswap32(bswap32(Subnet.ip) +
                    pred(not(bswap32(Subnet.mask))));
  CheckSubnet('RangeMax', IpMaxLE);
  if bswap32(IpMaxLE) <= bswap32(IpMinLE) then
    EDhcp.RaiseUtf8('PrepareScope: unexpected %..% range',
      [IP4ToShort(@IpMinLE), IP4ToShort(@IpMaxLE)]);
  if Broadcast <> 0 then
    CheckSubNet('BroadCastAddress', Broadcast);
  for i := 0 to high(StaticIP) do
    CheckSubnet('Static', StaticIP[i]);
  for i := 0 to high(StaticMac) do
    CheckSubnet('Static', StaticMac[i].IP4);
  AddStatic(ServerIdentifier, nil);
  // prepare the leases in-memory database
  if Entry = nil then
    SetLength(Entry, PREALLOCATE_LEASES)
  else
  begin
    // filter/validate all current leases from the actual subnet and statics
    n := 0;
    p := pointer(Entry);
    for i := 0 to Count - 1 do
    begin
      if Subnet.Match(p^.IP4) and
         not IsStaticIP(p^.IP4) then
      begin
        if n <> i then
          Entry[n] := p^;
        inc(n);
      end;
      inc(p);
    end;
    if n <> Count then
    begin
      if Assigned(log) then
        log.Log(sllTrace, 'PrepareScope: subnet adjust count=% from %',
          [n, Count]);
      Count := n;
    end;
    FreeListCount := 0;
  end;
  // log the scope settings and the computed sub-network context
  if Assigned(log) then
    log.Log(sllInfo, 'PrepareScope: scope=% subnet=% min=% max=% server=% ' +
      'broadcast=% gateway=% static=% dns=% lease=%s renew=%s rebind=%s ' +
      'offer=%s maxdecline=% declinetime=%s grace=%',
      [Subnet.ToShort, IP4ToShort(@Subnet.mask), IP4ToShort(@IpMinLE),
       IP4ToShort(@IpMaxLE), IP4ToShort(@ServerIdentifier),
       IP4ToShort(@Broadcast), IP4ToShort(@Gateway),
       IP4sToText(TNetIP4s(StaticIP)),  IP4sToText(TNetIP4s(DnsServer)),
       LeaseTime, RenewalTime, Rebinding, OfferHolding,
       MaxDeclinePerSec, DeclineTime, GraceFactor]);
  // store internal values in the more efficient endianess for direct usage
  IpMinLE     := bswap32(IpMinLE);      // little-endian
  IpMaxLE     := bswap32(IpMaxLE);
  LeaseTimeLE := LeaseTime;
  LeaseTime   := bswap32(LeaseTime);    // big-endian
  RenewalTime := bswap32(RenewalTime);
  Rebinding   := bswap32(Rebinding);
end;

function TDhcpScope.AddStatic(ip4: TNetIP4; mac: PNetMac): boolean;
var
  n: PtrInt;
  p: PDhcpLease;
begin
  result := false;
  if (@self = nil) or
     not SubNet.Match(ip4) then
    exit;
  Safe.Lock;
  try
    if (mac <> nil) and
       (DoFindMac(pointer(StaticMac), PInt64(mac)^ and MAC_MASK, length(StaticMac)) <> nil) then
      exit; // duplicated static MAC (check first) - but allow leases override
    if AddSortedInteger(StaticIP, ip4) < 0 then
      exit; // duplicated IP
    if mac <> nil then
    begin
      n := length(StaticMac);
      SetLength(StaticMac, n + 1);
      p := @StaticMac[n];
      p^.Mac := mac^;
      p^.IP4 := ip4;
    end;
    result := true;
  finally
    Safe.UnLock;
  end;
end;

function TDhcpScope.AddStatic(const macip: RawUtf8): boolean;
var
  nfo: TMacIP;
begin
  result := ParseMacIP(nfo, macip) and
            AddStatic(nfo.ip, nfo.macp);
end;

function TDhcpScope.RemoveStatic(ip4: TNetIP4): boolean;
var
  i, n: PtrInt;
  p: PDhcpLease;
begin
  result := false;
  if (@self = nil) or
     not SubNet.Match(ip4) then
    exit;
  Safe.Lock;
  try
    i := FastFindIntegerSorted(StaticIP, ip4);
    if i < 0 then
      exit; // no previous AddStatic(ip4)
    result := true;
    DeleteInteger(StaticIP, i);
    n := length(StaticMac);
    p := DoFindIp(pointer(StaticMac), ip4, n);
    if p = nil then
      exit; // no associated MAC for this IP
    i := (PtrUInt(p) - PtrUInt(StaticMac)) shr 4; // index in StaticMac[]
    dec(n);
    UnmanagedDynArrayDelete(StaticMac, n, i, SizeOf(TNetMac));
    SetLength(StaticMac, n);
  finally
    Safe.UnLock;
  end;
end;

function DoWrite(W: TTextWriter; p: PDhcpLease; n, tix32, grace: cardinal;
  boot: TUnixTime; subnet: PIp4SubNet): integer;
begin // dedicated sub-function for better codegen
  result := 0;
  repeat
    // OFFERed leases are temporary; the client hasn't accepted this IP
    // DECLINE/INFORM IPs (with MAC = 0) are ephemeral internal-only markers
    if (PInt64(@p^.Mac)^ and MAC_MASK <> 0) and
       (p^.IP4 <> 0) and
       (((p^.State = lsAck) or
        ((p^.State = lsOutdated) and // detected as p^.Expired < tix32
         (cardinal(tix32 - p^.Expired) < grace)))) then // still realistic
    begin
      if subnet <> nil then
      begin
        // first add the subnet mask as '# 192.168.0.1/24' comment line
        W.AddDirect('#', ' ');
        W.AddShort(subnet^.ToShort);
        W.AddShorter(' subnet');
        W.AddDirectNewLine;
        subnet := nil; // append once, and only if necessary
      end;
      // format is "1770034159 c2:07:2c:9d:eb:71 192.168.1.207"
      W.AddQ(boot + Int64(p^.Expired));
      W.AddDirect(' ');
      W.AddShort(MacToShort(@p^.Mac));
      W.AddDirect(' ');
      W.AddShort(IP4ToShort(@p^.IP4));
      W.AddDirectNewLine;
      inc(result);
    end;
    inc(p);
    dec(n);
  until n = 0;
end;

function TDhcpScope.TextWrite(W: TTextWriter; tix32: cardinal; boot: TUnixTime;
  localcopy: boolean): integer;
var
  local: TLeaseDynArray; // 10,000 leases would use 160KB of temporary memory
  grace: cardinal;
begin
  result := 0;
  if Count = 0 then
    exit;
  // make a transient copy of all leases to keep the lock small for this subnet
  Safe.Lock;
  try
    if Count = 0 then
      exit;
    grace := LeaseTimeLE * MaxPtrUInt(GraceFactor, 2); // not too deprecated
    if (Count < 1000) or
       not localcopy then
      // small output could be done within the lock
      result := DoWrite(W, pointer(Entry), Count, tix32, grace, boot, @Subnet)
    else
      // local copy may eventually be done if OnIdle() made a background thread
      local := copy(Entry, 0, Count); // allocate Count * 16 bytes
  finally
    Safe.UnLock;
  end;
  // append all text lines from the local copy (if any) - not used yet
  if local <> nil then
    result := DoWrite(W, pointer(local), length(local), tix32, grace, boot, @Subnet)
end;

function DoOutdated(p: PDhcpLease; tix32, n: cardinal): integer;
begin // dedicated sub-function for better codegen (100ns for 200 entries)
  result := 0;
  if n <> 0 then
    repeat
      if (p^.Expired < tix32) and
         (p^.State in [lsReserved, lsAck, lsUnavailable]) then
      begin
        inc(result);
        p^.State := lsOutdated;
      end;
      {$ifndef CPUINTEL}        // seems actually slower on modern Intel/AMD
      if p^.RateLimit <> 0 then // don't invalidate L1 cache
      {$endif CPUINTEL}
        p^.RateLimit := 0;      // reset the rate limiter every second
      inc(p);
      dec(n);
    until n = 0;
end;

function TDhcpScope.CheckOutdated(tix32: cardinal): integer;
begin
  result := 0;
  if Count = 0 then
    exit;
  Safe.Lock;
  try
    result := DoOutdated(pointer(Entry), tix32, Count);
  finally
    Safe.UnLock;
  end;
end;

procedure TDhcpScope.AddOptions(var f: PAnsiChar; withLeaseTimes: boolean);
begin
  DhcpAddOption(f, doSubnetMask, @Subnet.mask);
  if Broadcast <> 0 then
    DhcpAddOption(f, doBroadcastAddress, @Broadcast);
  if Gateway <> 0 then
    DhcpAddOption(f, doRouters, @Gateway);
  if DnsServer <> nil then
    DhcpAddOptions(f, doDomainNameServers, pointer(DnsServer));
  if not withLeaseTimes then // stateless INFORM don't need timeouts
    exit;
  DhcpAddOption(f, doDhcpLeaseTime,     @LeaseTime); // big-endian
  DhcpAddOption(f, doDhcpRenewalTime,   @RenewalTime);
  DhcpAddOption(f, doDhcpRebindingTime, @Rebinding);
end;


{ **************** High-Level Multi-Scope DHCP Server Processing Logic }

{ TDhcpScopeSettings }

constructor TDhcpScopeSettings.Create;
begin
  inherited Create;
  fSubnetMask := '192.168.1.1/24';
  fLeaseTimeSeconds := 120; // avoid IP exhaustion during iPXE process
  fOfferHoldingSecs := 5;
  fMaxDeclinePerSecond := 5;
  fGraceFactor := 2;
end;

procedure TDhcpScopeSettings.PrepareScope(Sender: TDhcpProcess; var Data: TDhcpScope);
var
  mask: TNetIP4;
  i: PtrInt;
begin
  // convert the text settings into Data.* raw values
  Data.Gateway           := ToIP4(fDefaultGateway);
  Data.Broadcast         := ToIP4(fBroadCastAddress);
  Data.ServerIdentifier  := ToIP4(fServerIdentifier);
  Data.LastIpLE          := 0;
  Data.IpMinLE           := ToIP4(fRangeMin);
  Data.IpMaxLE           := ToIP4(fRangeMax);
  Data.DnsServer         := TIntegerDynArray(ToIP4s(fDnsServers));
  Data.StaticIP          := nil;
  Data.StaticMac         := nil;
  for i := 0 to high(fStatic) do
    if not Data.AddStatic(fStatic[i]) then // add sorted, from 'ip' or 'mac=ip'
      EDhcp.RaiseUtf8('PrepareScope: invalid Static=%', [fStatic[i]]);
  Data.LeaseTime         := fLeaseTimeSeconds; // 100%
  if Data.LeaseTime < 30 then
    Data.LeaseTime := 30;                      // 30 seconds minimum lease
  Data.RenewalTime       := Data.LeaseTime shr 1;       // 50%
  Data.Rebinding         := (Data.LeaseTime * 7) shr 3; // 87.5%
  Data.OfferHolding      := fOfferHoldingSecs; // 5 seconds
  if Data.OfferHolding < 1 then
    Data.OfferHolding := 1
  else if Data.OfferHolding > Data.RenewalTime then
    Data.OfferHolding := Data.RenewalTime;
  Data.MaxDeclinePerSec  := fMaxDeclinePerSecond; // max 5 DECLINE per sec
  Data.DeclineTime       := fDeclineTimeSeconds;
  if Data.DeclineTime = 0 then
    Data.DeclineTime := Data.LeaseTime;
  Data.GraceFactor       := fGraceFactor;         // * 2
  Data.Options           := fOptions;
  // retrieve and adjust the subnet mask from settings
  if not Data.Subnet.From(fSubnetMask) then
    EDhcp.RaiseUtf8(
      'PrepareScope: unexpected SubNetMask=% (should be mask ip or ip/sub)',
      [fSubnetMask]);
  if Data.Subnet.mask = cAnyHost32 then
  begin
    // SubNetMask was not '192.168.0.1/24': is expected to be '255.255.255.0'
    if IP4Prefix(Data.Subnet.ip) = 0 then
      EDhcp.RaiseUtf8('PrepareScope: SubNetMask=% is not a valid IPv4 mask',
        [fSubnetMask]);
    mask := Data.Subnet.ip;
    // we expect other parameters to be set specifically: compute final Subnet
    if Data.ServerIdentifier = 0 then
      EDhcp.RaiseUtf8('PrepareScope: SubNetMask=% but without ServerIdentifier',
        [SubnetMask]);
    Data.Subnet.mask := mask;
    Data.Subnet.ip := Data.ServerIdentifier and mask; // normalize as in From()
  end
  else
    // SubNetMask was e.g. '192.168.0.1/24'
    if Data.ServerIdentifier = 0 then
      // extract from exact SubNetMask text, since Subnet.ip = 192.168.0.0
      Data.ServerIdentifier := ToIP4(SubnetMask);
  // validate all settings values from the actual subnet
  Data.AfterFill(Sender.Log.Add); // may raise an EDhcp exception
  // trigger SaveToFile() in next OnIdle()
  inc(Sender.fModifSequence);
end;


{ TDhcpServerSettings }

constructor TDhcpServerSettings.Create;
begin
  inherited Create;
  fFileFlushSeconds := 30; // good tradeoff: low disk writes, still safe for PXE
end;

function TDhcpServerSettings.Verify: string;
var
  process: TDhcpProcess;
begin
  result := ''; // no error
  if self <> nil then
    try
      process := TDhcpProcess.Create;
      try
        process.Setup(self); // may trigger EDhcp exception
      finally
        process.Free;
      end;
    except
      on E: Exception do
        result := E.Message;
    end;
end;

procedure TDhcpServerSettings.AddScope(one: TDhcpScopeSettings);
begin
  if self = nil then
    exit;
  if one = nil then
    one := TDhcpScopeSettings.Create; // default '192.168.1.1/24' subnet
  PtrArrayAdd(fScope, one); // will be owned by this instance
end;


{ TDhcpProcess }

function TDhcpProcess.GetCount: integer;
var
  n: integer;
  s: PDhcpScope;
begin
  result := 0;
  s := pointer(fScope);
  if s = nil then
    exit;
  fScopeSafe.ReadLock;
  n := PDALen(PAnsiChar(s) - _DALEN)^ + _DAOFF;
  repeat
    inc(result, s^.Count - s^.FreeListCount);
    inc(s);
    dec(n);
  until n = 0;
  fScopeSafe.ReadUnLock;
end;

function TDhcpProcess.GetScope(ip: TNetIP4): PDhcpScope;
var
  n: integer;
begin
  if ip <> 0 then
  begin
    result := pointer(fScope);
    if result = nil then
      exit;
    n := PDALen(PAnsiChar(result) - _DALEN)^ + _DAOFF;
    repeat
      if result^.Subnet.Match(ip) then
        exit;
      inc(result);
      dec(n);
    until n = 0;
  end;
  result := nil;
end;

function TDhcpProcess.GetScope(const ip: RawUtf8): PDhcpScope;
var
  ip4: TNetIP4;
begin
   if NetIsIP4(pointer(ip), @ip4) then
     result := GetScope(ip4)
   else
     result := nil;
end;

procedure TDhcpProcess.Setup(aSettings: TDhcpServerSettings);
var
  owned: boolean;
  i: PtrInt;
  s: PDhcpScope;
  new: TDhcpScopes;
begin
  owned := aSettings = nil;
  if owned then
  begin
    aSettings := TDhcpServerSettings.Create; // use default values
    aSettings.AddScope; // with a single default scope
  end;
  fScopeSafe.WriteLock;
  try
    fState := sSetupFailed; // keep on EDhcp exception
    // assign aSettings.Scope[] subnet definitions
    SetLength(new, length(aSettings.Scope));
    for i := 0 to high(new) do
    begin
      aSettings.Scope[i].PrepareScope(self, new[i]); // compute Subnet
      s := GetScope(new[i].Subnet.ip);
      if s = nil then
        continue; // brand new subnet
      aSettings.Scope[i].PrepareScope(self, s^); // may adjust existing leases
      new[i] := s^;
    end;
    fScope := new; // replace
    // support FileName background persistence
    if aSettings.FileName <> '' then
      SetFileName(aSettings.FileName); // LoadFromFile() once fScope[] are set
    fFileFlushSeconds := aSettings.FileFlushSeconds;
    // success
    fState := sSetup;
  finally
    fScopeSafe.WriteUnLock;
    if owned then
      aSettings.Free; // avoid memory leak
  end;
end;

function TDhcpProcess.AddStatic(const macip: RawUtf8): boolean;
var
  nfo: TMacIP;
begin
  result := ParseMacIP(nfo, macip);
  if not result then
    exit;
  fScopeSafe.ReadLock;
  try
    result := GetScope(nfo.ip)^.AddStatic(nfo.ip, nfo.macp);
  finally
    fScopeSafe.ReadUnLock;
  end;
end;

function TDhcpProcess.RemoveStatic(const ip: RawUtf8): boolean;
var
  ip4: TNetIP4;
begin
  result := NetIsIP4(pointer(ip), @ip4);
  if not result then
    exit;
  fScopeSafe.ReadLock;
  try
    result := GetScope(ip4)^.RemoveStatic(ip4);
  finally
    fScopeSafe.ReadUnLock;
  end;
end;

procedure TDhcpProcess.Clear;
begin
  fScopeSafe.WriteLock;
  try
    fScope := nil;
    fModifSequence := 0;
    fModifSaved := 0;
  finally
    fScopeSafe.WriteUnLock;
  end;
end;

procedure TDhcpProcess.ClearLeases;
var
  s: PDhcpScope;
  i: PtrInt;
begin
  fScopeSafe.WriteLock;
  try
    s := pointer(fScope);
    for i := 1 to length(fScope) do
    begin
      s^.Safe.Lock;
      s^.Entry := nil;
      SetLength(s^.Entry, PREALLOCATE_LEASES); // as in TDhcpScope.AfterFill
      s^.Count := 0;
      s^.FreeListCount := 0;
      s^.Safe.UnLock;
      inc(s);
    end;
    fModifSequence := 0;
    fModifSaved := 0;
  finally
    fScopeSafe.WriteUnLock;
  end;
end;

procedure TDhcpProcess.Shutdown;
begin
  // disable ComputeResponse()
  if fState in [sNone, sShutdown] then
    exit; // no Setup(), or called twice
  fScopeSafe.WriteLock;
  try
    fState := sShutdown;
  finally
    fScopeSafe.WriteUnLock;
  end;
  // persist FileName for clean shutdown
  if (fFileName <> '') and
     (fModifSaved <> fModifSequence) then
    SaveToFile(fFileName); // make fScopeSafe.ReadLock/ReadUnLock
  fLog.Add.Log(sllDebug, 'Shutdown: count=% seq=% saved=%',
    [GetCount, fModifSequence, fModifSaved], self);
  // don't Clear the entries: we may call Setup() and restart again
end;

function TDhcpProcess.OnIdle(tix64: Int64): integer;
var
  tix32, n: cardinal;
  saved: integer;
  s: PDhcpScope;
begin
  // make periodical process at most every second
  result := 0;
  tix32 := tix64 div MilliSecsPerSec; // GetTickSec from GetTickCount64
  if (tix32 = fIdleTix) or
     (fScope = nil) or
     (fState <> sSetup) then // e.g. after Shutdown
    exit;
  fIdleTix := tix32;
  fScopeSafe.ReadLock;
  try
    // check deprecated entries in all scopes
    s := pointer(fScope);
    if s <> nil then
    begin
      n := PDALen(PAnsiChar(s) - _DALEN)^ + _DAOFF;
      repeat
        inc(result, s^.CheckOutdated(tix32)); // 5us for 100k leases ;)
        inc(s);
        dec(n);
      until n = 0;
    end;
    if result <> 0 then
      inc(fModifSequence); // trigger SaveToFile() below
    // background persist into FileName if needed
    saved := 0;
    if (fFileName <> '') and
       (fFileFlushSeconds <> 0) and         // = 0 if disabled
       (fModifSaved <> fModifSequence) then // if something new to be written
      if tix32 >= fFileFlushTix then     // reached the next persistence time
      begin
        saved := SaveToFile(fFileName);// make fScopeSafe.ReadLock/ReadUnLock
        // do not aggressively retry if saved<0 (write failed)
        fFileFlushTix := tix32 + fFileFlushSeconds;  // every 30 secs by default
        // note: no localcopy made yet - TUdpServerThread.OnIdle would block any
        // recv(UDP packet) so we would need to create a background thread
      end;
  finally
    fScopeSafe.ReadUnLock;
  end;
  // no ARP call here to clean the internal list: the user should rather tune
  // lease duration or use a bigger netmask; the RFC states that the client
  // would do its own ARP request and resend (dmtDecline +) dmtDiscover
  // - we mitigate aggressive clients via the RateLimit counter anyway
  if Assigned(fLog) and
     ((result or saved) <> 0) then
    fLog.Add.Log(sllTrace, 'OnIdle: outdated=% saved=%', [result, saved], self);
end;

function TDhcpProcess.SaveToText(SavedCount: PInteger): RawUtf8;
var
  tmp: TTextWriterStackBuffer; // 8KB static, then up to 1MB buffer
  W: TTextWriter;
  tix32, saved, n: cardinal;
  boot: TUnixTime;
  s: PDhcpScope;
begin
  result := CRLF; // returns something not void
  if SavedCount <> nil then
    SavedCount^ := 0;
  n := GetCount;
  if n = 0 then
    exit;
  tix32 := GetTickSec;
  boot := UnixTimeUtc - tix32;    // = UnixTimeUtc at computer boot
  if boot < UNIXTIME_MINIMAL then // we should have booted after 08 Dec 2016
    exit;
  // save all leases in all scopes in dnsmasq format
  saved := 0;
  n := MinPtrUInt(1 shl 20, 256 + (n + cardinal(length(fScope))) * 46);
  W := TTextWriter.CreateOwnedStream(tmp, n);
  try
    fScopeSafe.ReadLock;
    try
      s := pointer(fScope);
      if s <> nil then
      begin
        n := PDALen(PAnsiChar(s) - _DALEN)^ + _DAOFF;
        repeat
          inc(saved, s^.TextWrite(W, tix32, boot, {localcopy=}false));
          inc(s);
          dec(n);
        until n = 0;
      end;
    finally
      fScopeSafe.ReadUnLock;
    end;
    if W.TextLength <> 0 then
      W.SetText(result);
  finally
    W.Free;
  end;
  if SavedCount <> nil then
    SavedCount^ := saved;
end;

function TDhcpProcess.LoadFromText(const Text: RawUtf8): boolean;
var
  tix32, entries: cardinal;
  boot, expiry: TUnixTime;
  mac: TNetMac;
  ip4: TNetIP4;
  p, b: PUtf8Char;
  new: PDhcpLease;
  s, last: PDhcpScope;
begin
  result := false;
  tix32 := GetTickSec;
  boot := UnixTimeUtc - tix32;    // = UnixTimeUtc at computer boot
  if boot < UNIXTIME_MINIMAL then // we should have booted after 08 Dec 2016
    exit;
  ClearLeases;
  entries := 0;
  last := nil;
  fScopeSafe.WriteLock; // easier than complex s^.Lock/UnLock
  try
    p := pointer(Text);
    if (p <> nil) and
       (fScope <> nil) then
    repeat
      // line format is "<expiry> <mac> <ip4>" as for dnsmasq leases
      p := GotoNextNotSpace(p);
      if p^ in ['0' .. '9'] then // parse <expiry> but ignore e.g. '# 1.2.3.4/24'
      begin
        b := p;
        expiry := GetNextItemQWord(b, ' ');
        if (expiry >= boot - SecsPerMonth) and  // not too old
           (expiry < TUnixTime(UNIXTIME_MINIMAL) * 2) then // range in seconds
        begin
          p := GotoNextSpace(b);
          if (p - b = 17) and
             (p^ = ' ') and
             TextToMac(b, @mac) and
             (PInt64(@mac)^ and MAC_MASK <> 0) and
             NetIsIP4(p + 1, @ip4) and
             (ip4 <> 0) then
          begin
            // we parsed a valid lease into expiry/mac/ip4: locate its scope
            s := last;
            if (s = nil) or
               not s^.Subnet.Match(ip4) then
            begin
              s := GetScope(ip4); // called once per sub-net or incorrect IP
              if s <> nil then
                last := s; // is likely not to change for the next line
            end;
            if s <> nil then
            begin
              // add this lease to the internal list of this scope
              new := s^.NewLease;
              PInt64(@new^.Mac)^ := PInt64(@mac)^; // also reset State+RateLimit
              new^.IP4 := ip4;
              new^.Expired := expiry - boot;
              if new^.Expired < tix32 then // same logic than s^.CheckOutdated()
                new^.State := lsOutdated
              else
                new^.State := lsAck;
              inc(entries);
            end;
          end;
        end;
      end;
      // invalid lines are just skipped
      p := GotoNextLine(p);
    until (p = nil) or
          (p^ = #0);
  finally
    fScopeSafe.WriteUnLock;
  end;
  fLog.Add.Log(sllDebug, 'LoadFromText: leases=%', [entries], self);
  result := true;
end;

procedure TDhcpProcess.SetFileName(const name: TFileName);
begin
  if fFileName = name then
    exit;
  LoadFromFile(name); // make fScopeSafe.WriteLock/WriteUnlock
  fFileName := name;
end;

function TDhcpProcess.SaveToFile(const FileName: TFileName): integer;
var
  txt: RawUtf8;
begin
  txt := SaveToText(@result); // make fScopeSafe.ReadLock/ReadUnLock
  if FileFromString(txt, FileName) then
    fModifSaved := fModifSequence // success: won't retry on next OnIdle()
  else
    result := -1; // indicates error writing to disk
end;

function TDhcpProcess.LoadFromFile(const FileName: TFileName): boolean;
var
  txt: RawUtf8;
begin
  result := false;
  txt := StringFromFile(FileName);
  if txt <> '' then
    result := LoadFromText(txt); // make fScopeSafe.WriteLock/WriteUnlock
end;

function TDhcpProcess.ParseFrame(var Data: TDhcpProcessData): boolean;
begin
  Data.Ip4 := 0;
  Data.Mac64 := 0;
  Data.RecvType := DhcpParse(@Data.Recv, Data.RecvLen, Data.RecvLens, nil, @Data.Mac64);
  Data.Ip[0] := #0;
  if (Data.Mac64 <> 0) and
     (Assigned(fLog) or
      Assigned(fOnComputeResponse)) then
  begin
    Data.Mac[0] := #17;
    ToHumanHexP(@Data.Mac[1], @Data.Mac64, 6);
  end
  else
    Data.Mac[0] := #0;
  Data.HostName := DhcpData(@Data.Recv, Data.RecvLens[doHostName]);
  result := (Data.Mac64 <> 0) and
            (Data.RecvType in
              [dmtDiscover, dmtRequest, dmtDecline, dmtRelease, dmtInform]);
end;

function TDhcpProcess.RetrieveFrameIP(
  var Data: TDhcpProcessData; Lease: PDhcpLease): boolean;
var
  ip4: TNetIP4;
begin
  // called from DISCOVER and REQUEST
  result := false;
  ip4 := DhcpIP4(@Data.Recv, Data.RecvLens[doDhcpRequestedAddress]);
  if ip4 = 0 then
    exit;
  if (Lease = nil) or
     (Lease^.IP4 <> ip4) then
    if (not Data.Scope^.SubNet.Match(ip4)) or
       Data.Scope^.IsStaticIP(ip4) or
       (Data.Scope^.FindIp4(ip4) <> nil) then
    begin
      // this IP seems already used by another MAC
      if Assigned(fLog) then
      begin
        IP4Short(@ip4, Data.Ip);
        fLog.Add.Log(sllTrace, 'ComputeResponse: % % ignore requested=% %',
          [DHCP_TXT[Data.RecvType], Data.Mac, Data.Ip, Data.HostName^], self);
      end;
      exit;
    end;
  Data.Ip4 := ip4;
  result := true;
end;

function TDhcpProcess.FinalizeFrame(var Data: TDhcpProcessData): PtrInt;
begin
  // optional callback support
  if Assigned(fOnComputeResponse) then
  begin
    fOnComputeResponse(self, Data);
    if Data.SendEnd = nil then
    begin
      if Assigned(fLog) then
        fLog.Add.Log(sllTrace, 'ComputeResponse: % % ignored by callback',
          [DHCP_TXT[Data.RecvType], Data.Mac], self);
      result := 0;
      exit; // callback asked to silently ignore this frame
    end;
  end;
  // send back verbatim Option 61 if any
  if Data.RecvLens[doDhcpClientIdentifier] <> 0 then
    DhcpCopyOption(Data.SendEnd, @Data.Recv.options[Data.RecvLens[doDhcpClientIdentifier]]);
  // support Option 82 Relay Agent by sending it back - should be the last option
  if Data.RecvLens[doDhcpAgentOptions] <> 0 then
    DhcpCopyOption(Data.SendEnd, @Data.Recv.options[Data.RecvLens[doDhcpAgentOptions]]);
  // add trailer to the Data.Send frame and returns its final size in bytes
  Data.SendEnd^ := #255;
  result := Data.SendEnd - PAnsiChar(@Data.Send) + 1;
end;

function TDhcpProcess.ComputeResponse(var Data: TDhcpProcessData): PtrInt;
var
  p: PDhcpLease;
begin
  result := -1; // error
  // do nothing on missing Setup() or after Shutdown
  if (fState <> sSetup) or
     (fScope = nil) then
    exit;
  // parse and validate the request
  if not ParseFrame(Data) then
  begin
    if Assigned(fLog) then
      fLog.Add.Log(sllTrace, 'ComputeResponse: % % unexpected %',
        [DHCP_TXT[Data.RecvType], Data.Mac, Data.HostName^], self);
    exit; // invalid or unsupported frame
  end;
  fScopeSafe.ReadLock; // protect Scope[] but is reentrant and not-blocking
  try
    // detect the proper scope to use
    if Data.RecvLens[doSubnetSelection] <> 0 then
      // RFC 3011 defined option 118 which overrides giaddr
      Data.Scope := GetScope(DhcpIP4(@Data.Recv, Data.RecvLens[doSubnetSelection]))
    else if Data.Recv.giaddr <> 0 then
      // e.g. VLAN 10 relay set giaddr=192.168.10.1 Gateway IP field
      Data.Scope := GetScope(Data.Recv.giaddr)
    else if Data.RecvIp4 <> 0 then
      // no giaddr: check RecvIp4 bound server IP as set by the UDP server
      Data.Scope := GetScope(Data.RecvIp4)
    else
      // no option 118 no giaddr nor RecvIp4: default to Scope[0]
      Data.Scope := pointer(fScope);
    if Data.Scope = nil then
    begin
      if Assigned(fLog) then
      begin
        if Data.Recv.giaddr <> 0 then
          IP4Short(@Data.Recv.giaddr, Data.Ip)
        else if Data.RecvIp4 <> 0 then
          IP4Short(@Data.RecvIP4, Data.Ip);
        fLog.Add.Log(sllDebug, 'ComputeResponse: % % no subnet for % %',
          [DHCP_TXT[Data.RecvType], Data.Mac, Data.Ip, Data.HostName^], self);
      end;
      exit; // MUST NOT respond if no subnet matches giaddr
    end;
    // compute the corresponding IPv4 according to the internal lease list
    result := 0; // no response, but no error
    Data.Tix32 := GetTickSec;
    Data.Scope^.Safe.Lock; // blocking for this subnet
    try
      p := Data.Scope^.FindMac(Data.Mac64); // from Entry[] and StaticMac[]
      case Data.RecvType of
        dmtDiscover:
          begin
            if (p = nil) or    // first time this MAC is seen
               (p^.IP4 = 0) or // may happen after DECLINE
               (p^.State = lsReserved) then // reserved = client refused this IP
            begin
              // first time seen (most common case), or renewal
              if not RetrieveFrameIP(Data, p) then // IP from option 50
              begin
                Data.Ip4 := Data.Scope^.NextIp4; // guess a free IP from pool
                if Data.Ip4 = 0 then
                begin
                  // IPv4 exhausted: don't return NAK, but silently ignore
                  // - client will retry after a small temporisation
                  fLog.Add.Log(sllWarning,
                    'ComputeResponse: DISCOVER % exhausted IPv4 %',
                      [Data.Mac, Data.HostName^], self);
                  exit;
                end;
              end;
              if p = nil then
              begin
                p := Data.Scope^.NewLease;
                Data.Scope^.LastDiscover := Data.Scope^.Index(p);
                PInt64(@p^.Mac)^ := Data.Mac64; // also reset State+RateLimit
              end;
              p^.IP4 := Data.Ip4;
            end
            else
              // keep existing/previous/static IP4
              Data.Ip4 := p^.IP4;
            // update the lease information
            if p^.State = lsStatic then
              p^.Expired := Data.Tix32
            else
            begin
              inc(fModifSequence); // trigger SaveToFile() in next OnIdle()
              p^.State := lsReserved;
              p^.Expired := Data.Tix32 + Data.Scope^.OfferHolding;
            end;
            // respond with an OFFER
            Data.SendType := dmtOffer;
          end;
        dmtRequest:
          begin
            // RFC 2131: lease is Reserved after OFFER = SELECTING
            //           lease is Ack/Static/Outdated = RENEWING/REBINDING
            if (p = nil) or
               (p^.IP4 = 0) or
               not ((p^.State in [lsReserved, lsAck, lsStatic]) or
                    ((p^.State = lsOutdated) and
                     (Data.Scope^.GraceFactor > 1) and
                     (Data.Scope^.LeaseTimeLE < SecsPerHour) and // grace period
                     (Data.Tix32 - p^.Expired <
                        Data.Scope^.LeaseTimeLE * Data.Scope^.GraceFactor))) then
              // no good-enough lease
              if RetrieveFrameIP(Data, p) then
              begin
                // no lease, but Option 50 = INIT-REBOOT
                if p = nil then
                begin
                  p := Data.Scope^.NewLease;
                  PInt64(@p^.Mac)^ := Data.Mac64; // also reset State+RateLimit
                end;
                p^.IP4 := Data.Ip4;
              end
              else
              begin
                // no lease, and none or invalid Option 50 = send NAK response
                fLog.Add.Log(sllDebug,
                  'ComputeResponse: REQUEST % out-of-sync NAK %',
                    [Data.Mac, Data.HostName^], self);
                Data.SendType := dmtNak;
                Data.SendEnd := DhcpNew(Data.Send, dmtNak, Data.Recv.xid,
                  PNetMac(@Data.Mac64)^, Data.Scope^.ServerIdentifier);
                result := FinalizeFrame(Data);
                exit;
              end;
            // update the lease information
            if p^.State = lsStatic then
              p^.Expired := Data.Tix32
            else
            begin
              inc(fModifSequence); // trigger SaveToFile() in next OnIdle()
              p^.State := lsAck;
              p^.Expired := Data.Tix32 + Data.Scope^.LeaseTimeLE;
            end;
            // respond with an ACK on the known IP
            Data.Ip4 := p^.IP4;
            Data.SendType := dmtAck;
          end;
        dmtDecline:
          begin
            // client ARPed the OFFERed IP and detected conflict, so DECLINE it
            if (p = nil) or
               (p^.State <> lsReserved) then
            begin
              // ensure the server recently OFFERed one IP to that client
              // (match RFC intent and prevent blind poisoning of arbitrary IPs)
              fLog.Add.Log(sllDebug,
                'ComputeResponse: DECLINE % with no previous OFFER %',
                  [Data.Mac, Data.HostName^], self);
              exit;
            end;
            if (Data.Scope^.MaxDeclinePerSec <> 0) and // = 5 by default
               (fIdleTix <> 0) then  // OnIdle() would reset RateLimit := 0
              if p^.RateLimit < Data.Scope^.MaxDeclinePerSec then
                inc(p^.RateLimit)
              else
              begin
                // malicious client poisons the pool by sending repeated DECLINE
                fLog.Add.Log(sllDebug,
                  'ComputeResponse: DECLINE % overload', [Data.Mac], self);
                exit;
              end;
            // invalidate OFFERed IP
            Data.Ip4 := DhcpIP4(@Data.Recv, Data.RecvLens[doDhcpRequestedAddress]);
            if (Data.Ip4 <> 0) and // authoritative IP
               not Data.Scope^.SubNet.Match(Data.Ip4) then // need clean IP
              Data.Ip4 := 0;
            if Data.Ip4 = 0 then
              Data.Ip4 := p^.IP4; // option 50 was not set (or not in our subnet)
            p^.State := lsUnavailable;
            p^.IP4 := 0; // invalidate the previous offered IP
            if Data.Ip4 <> 0 then
            begin
              // store internally this IP as unavailable for NextIP4
              if Assigned(fLog) then
              begin
                IP4Short(@Data.Ip4, Data.Ip);
                fLog.Add.Log(sllTrace, 'ComputeResponse: DECLINE % as %',
                  [Data.Mac, Data.Ip], self);
              end;
              p := Data.Scope^.NewLease;
              PInt64(@p^.Mac)^ := 0; // used as sentinel to store this IP
              p^.State := lsUnavailable;
              p^.IP4 := Data.Ip4;
              p^.Expired := Data.Tix32 + Data.Scope^.DeclineTime;
            end;
            inc(fModifSequence); // trigger SaveToFile() in next OnIdle()
            exit; // server MUST NOT respond to a DECLINE message
          end;
        dmtRelease:
          begin
            // client informs the DHCP server that it no longer needs the lease
            if (p = nil) or
               (p^.IP4 <> Data.Recv.ciaddr) or
               not (p^.State in [lsAck, lsOutdated]) then
              // detect and ignore out-of-synch or malicious client
              fLog.Add.Log(sllDebug,
                'ComputeResponse: RELEASE % unexpected', [Data.Mac], self)
            else
            begin
              if Assigned(fLog) then
              begin
                IP4Short(@p^.IP4, Data.Ip);
                fLog.Add.Log(sllTrace, 'ComputeResponse: RELEASE % as %',
                  [Data.Mac, Data.Ip], self);
              end;
              Data.Scope^.ReuseIp4(p); // set MAC=0 IP=0 State=lsFree
              inc(fModifSequence); // trigger SaveToFile() in next OnIdle()
            end;
            exit; // server MUST NOT respond to a RELEASE message
          end;
        dmtInform:
          begin
            // client requests configuration options for its static IP
            Data.Ip4 := Data.Recv.ciaddr;
            if not Data.Scope^.Subnet.Match(Data.Ip4) or
               ((p <> nil) and
                not (p^.State in [lsUnavailable, lsOutdated, lsStatic])) then
            begin
              IP4Short(@Data.Ip4, Data.Ip);
              fLog.Add.Log(sllDebug, 'ComputeResponse: INFORM % unexpected %',
                [Data.Mac, Data.Ip], self);
              exit;
            end;
            if (p <> nil) and
               (p^.State = lsStatic) then
              p^.Expired := Data.Tix32
            else if (dsoInformRateLimit in Data.Scope^.Options) and
                    (fIdleTix <> 0) and // OnIdle() would reset RateLimit := 0
                    not Data.Scope^.IsStaticIP(Data.Ip4) then
            begin
              // temporarily marks client IP as unavailable to avoid conflicts
              // (not set by default: stronger than the RFC requires, and no
              // other DHCP server like KEA, dnsmasq or Windows implements it)
              if p = nil then
              begin
                p := Data.Scope^.NewLease;
                PInt64(@p^.Mac)^ := Data.Mac64; // also reset State+RateLimit
              end
              else if p^.RateLimit >= 2 then // up to 3 INFORM per MAC per sec
              begin
                fLog.Add.Log(sllDebug,
                  'ComputeResponse: INFORM % overload', [Data.Mac], self);
                exit;
              end
              else
              begin
                inc(p^.RateLimit);
                // mark this IP as temporary unavailable
                if p^.IP4 <> Data.Ip4 then
                begin
                  // create a new MAC-free lease for this other IP
                  p := Data.Scope^.NewLease;
                  PInt64(@p^.Mac)^ := 0; // used as sentinel to store this IP
                end;
              end;
              // update the lease information
              inc(fModifSequence); // trigger SaveToFile() in next OnIdle()
              p^.State := lsUnavailable;
              p^.Expired := Data.Tix32 + Data.Scope^.DeclineTime;
              p^.IP4 := Data.Ip4;
            end;
            // respond with an ACK and the standard options
            Data.SendType := dmtAck;
          end;
      else
        exit; // paranoid
      end;
      // compute the dmtOffer/dmtAck response frame over the very same xid
      if Assigned(fLog) or
         Assigned(fOnComputeResponse) then
        IP4Short(@Data.Ip4, Data.Ip);
      if Assigned(fLog) then
        with fLog.Family do
          if sllTrace in Level then
            Add.Log(sllTrace, 'ComputeResponse: % % into % % %',
              [DHCP_TXT[Data.RecvType], Data.Mac, DHCP_TXT[Data.SendType],
               Data.Ip, Data.HostName^], self);
      Data.SendEnd := DhcpNew(Data.Send, Data.SendType, Data.Recv.xid,
        PNetMac(@Data.Recv.chaddr)^, Data.Scope^.ServerIdentifier);
      Data.Send.ciaddr := Data.Ip4;
      Data.Scope^.AddOptions(Data.SendEnd, Data.RecvType <> dmtInform);
      // TODO: IPXE host/file options
      result := FinalizeFrame(Data); // callback + option 82 + length
    finally
      Data.Scope^.Safe.UnLock;
    end;
  finally
    fScopeSafe.ReadUnLock;
  end;
end;



initialization
  assert(ord(dmtTls) = 18);
  assert(PtrUInt(@PDhcpPacket(nil)^.options) = 240);
  assert(SizeOf(TDhcpPacket) = 548);
  assert(SizeOf(TDhcpLease) = 16);
  GetEnumTrimmedNames(TypeInfo(TDhcpMessageType), @DHCP_TXT, scUpperCase);
  GetEnumTrimmedNames(TypeInfo(TDhcpOption), @DHCP_OPTION, scKebabCase);
  FillLookupTable(@DHCP_OPTION_NUM, @DHCP_OPTION_INV, ord(high(DHCP_OPTION_NUM)));
  assert(DHCP_OPTION_INV[high(DHCP_OPTION_INV)] = pred(high(TDhcpOption)));

end.

