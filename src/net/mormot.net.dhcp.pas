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

   Purpose of this DCHP Server is not to be fully featured, but simple and
   compliant enough to implement e.g. a local PXE environment with our
   mormot.net.tftp.server.pas unit.

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
  // - doPadding is Padding option 0 (not a true value)
  // - doSubnetMask is Subnet Mask option 1 (255.255.255.0)
  // - doRouter is Default Gateway option 3 (192.168.1.1)
  // - doDns is DNS Servers option 6 (8.8.8.8, 8.8.4.4)
  // - doHostName is Hostname option 12 (client01)
  // - doDomainName is Domain Name option 15 (example.local)
  // - doBroadcastAddr is Broadcast Address option 28 (192.168.1.255)
  // - doNtpServer is NTP Server option 42
  // - doRequestedIp is Requested IP Address option 50 (0.0.0.0)
  // - doLeaseTimeValue is IP Lease Duration in seconds option 51 (86400 for 24h)
  // - doMessageType is DHCP Message Type option 53 (TDhcpMessageType)
  // - doServerIdentifier is DHCP Server Identifier option 54 (192.168.1.1)
  // - doParameterRequestList is Parameter Request List option 55 (1,3,6,15,51,54)
  // - doRenewalTimeValue is Renewal Time Value (T1) in seconds option 58 (43200 for 12h)
  // - doRebindingTimeValue is Rebinding Time Value (T2) in seconds option 59 (75600 for 21h)
  // - doVendorClassIdentifier is Vendor Class Identifier (PXE RFC 2132) option 60 (PXEClient)
  // - doClientIdentifier is Client Identifier option 61 ($01 + MAC)
  // - doTftpServerName is TFTP Server Name (PXE) option 66 (192.168.10.10 or host name)
  // - doBootfileName is Bootfile Name (PXE) option 67 (pxelinux.0)
  // - doUserClass is User Class (PXE RFC 3004) option 77 (PXEClient:Arch:00000)
  // - doRelayAgent is Relay Agent (RFC 3046) option 82
  // - doEnding is End Of Options marker option 255 (not a true value)
  TDhcpOption = (
   doPadding,
   doSubnetMask,
   doRouter,
   doDns,
   doHostName,
   doDomainName,
   doBroadcastAddr,
   doNtpServer,
   doRequestedIp,
   doLeaseTimeValue,
   doMessageType,
   doServerIdentifier,
   doParameterRequestList,
   doRenewalTimeValue,
   doRebindingTimeValue,
   doVendorClassIdentifier,
   doClientIdentifier,
   doTftpServerName,
   doBootfileName,
   doUserClass,
   doRelayAgent,
   doEnding
 );
 /// set of supported DHCP options
 TDhcpOptions = set of TDhcpOption;
 /// pointer to a set of supported DHCP options
 PDhcpOptions = ^TDhcpOptions;

var
  /// trimmed stUpperCase value of each DHCP message, e.g. 'DISCOVER' or 'ACK'
  DHCP_TXT: array[TDhcpMessageType] of RawUtf8;

function ToText(dmt: TDhcpMessageType): PShortString; overload;
function ToText(opt: TDhcpOption): PShortString; overload;

const
  /// 1,3,6,15,28 options as used by default for DhcpClient()
  DHCP_REQUEST = [doSubnetMask, doRouter, doDns, doDomainName, doBroadcastAddr];

/// initialize a DHCP client discover/request packet
// - returns pointer to @dhcp.options for additional DhcpAddOption() fluent calls
function DhcpClient(var dhcp: TDhcpPacket; dmt: TDhcpMessageType;
  const addr: TNetMac; req: TDhcpOptions = DHCP_REQUEST): PAnsiChar;

/// append a byte value to the TDhcpPacket.options packet
procedure DhcpAddOption(var p: PAnsiChar; op: TDhcpOption; b: byte); overload;
  {$ifdef FPC} inline; {$endif}

/// append a raw binary value to the TDhcpPacket.options packet
procedure DhcpAddOption(var p: PAnsiChar; op: TDhcpOption; b: pbyte;
  len: PtrUInt = 4); overload;

/// append a TNetIP4s dynamic array to the TDhcpPacket.options packet
procedure DhcpAddOptions(var p: PAnsiChar; op: TDhcpOption; ips: PAnsiChar);
  {$ifdef HASINLINE} inline; {$endif}

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

/// decode the lens[doParameterRequestList] within dhcp^.option[]
function DhcpRequestList(dhcp: PDhcpPacket; const lens: TDhcpParsed): TDhcpOptions;


{ **************** Middle-Level DHCP Scope and Lease Logic }

type
  /// the state of one DHCP TDhcpLease entry in memory
  // - lsFree identify an lsOutdated slot which IP4 has been reused
  // - lsReserved is used when an OFFER has been sent back to the client with IP4
  // - lsAck is used when REQUEST has been received and ACK has been sent back
  // - lsUnavailable is set when a DECLINE/INFORM message is received with an IP
  // - lsOutdated is set once Expired timestamp has been reached, so that the
  // IP4 address could be reused on the next DISCOVER for this MAC
  TLeaseState = (
    lsFree,
    lsReserved,
    lsAck,
    lsUnavailable,
    lsOutdated);

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
    Unavailable: byte;
    /// the 32-bit reserved IP
    IP4: TNetIP4;
    /// GetTickSec monotonic value stored as 32-bit unsigned integer
    // - TUnixTime is used on disk but not during server process (not monotonic)
    Expired: cardinal;
  end;
  /// points to one DHCP lease entry in memory
  PDhcpLease = ^TDhcpLease;

  /// a dynamic array of TDhcpLease entries, as stored within TDhcpProcess
  // - 10,000 leases would consume 160KB which fits in L2 cache on modern CPU
  TLeaseDynArray = array of TDhcpLease;

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
    /// the broadcast IP of this scope for doBroadcastAddr option 28
    Broadcast: TNetIP4;
    /// the gateway IP of this scope for doRouter option 3
    Gateway: TNetIP4;
    /// the server IP of this scope for doServerIdentifier option 54
    ServerIdentifier: TNetIP4;
    /// the DNS IPs of this scope for doDns option 6
    DnsServer: TIntegerDynArray;
    /// the 32-bit-integer-sorted list of static IPs of this scope
    // - sorted as 32-bit for efficient O(log(n)) branchless binary search
    SortedStatic: TIntegerDynArray;
    /// readjust all internal values according to to Subnet policy
    // - raise an EDhcp exception if the parameters are not correct
    procedure AfterFill(log: TSynLog);
    /// register another static IP address to the internal pool
    function AddStatic(ip4: TNetIP4): boolean;
    /// remove one static IP address which was registered by AddStatic()
    function RemoveStatic(ip4: TNetIP4): boolean;
    /// efficient L1-cache friendly O(n) search of a MAC address in Entry[]
    function FindMac(mac: Int64): PDhcpLease;
    /// efficient L1-cache friendly O(n) search of an IPv4 address in Entry[]
    function FindIp4(ip4: TNetIP4): PDhcpLease;
    /// retrieve a new lease in Entry[] - maybe recycled from ReuseIp4()
    function NewLease: PDhcpLease;
    /// release a lease in Entry[] - add it to the recycled FreeList[]
    function ReuseIp4(p: PDhcpLease): TNetIP4;
    /// return the index in Entry[] of a given lease
    // - no range check is performed against the supplied p pointer
    function Index(p: PDhcpLease): cardinal;
      {$ifdef HASINLINE} inline; {$endif}
    /// compute the next IPv4 address available in this scope
    function NextIp4: TNetIP4;
    /// call DhcpAddOptions() with the settings of this scope
    procedure AddOptions(var f: PAnsiChar; andLeaseTimes: boolean);
    /// check all TDhcpLease.Expired against tix32
    // - return the number of leases marked as lsOutdated
    // - this method is thread safe and will call Safe.Lock/UnLock
    function CheckOutdated(tix32: cardinal): integer;
    /// persist all leases with "<expiry> <MAC> <IP>" dnsmasq file pattern
    // - this method is thread safe and will call Safe.Lock/UnLock
    // - returns the number of entries/lines added to the text file
    function TextWrite(W: TTextWriter; tix32: cardinal; boot: TUnixTime): integer;
  private
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


{ **************** High-Level Multi-Scope DHCP Server Processing Logic }

type
  /// main high-level options for defining one scope/subnet for our DHCP Server
  TDhcpScopeSettings = class(TSynPersistent)
  protected
    fSubnetMask: RawUtf8;
    fStaticIPs: RawUtf8;
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
    fFileName: TFileName;
    fFileFlushSeconds: cardinal;
  public
    /// setup this instance with default values
    // - default are just SubnetMask = '192.168.1.1/24', LeaseTimeSeconds = 120
    // and OfferHoldingSecs = 5, consistent with a simple local iPXE network
    constructor Create; override;
    /// validate the consistency of settings, mainly about IPv4 subnet
    // - returns '' if no error occured with the current property values
    // - returns the TDhcpProcess.Setup() Exception message on failure
    function Verify: string;
  published
    /// Subnet Mask e.g. '192.168.1.1/24'
    // - the CIDR 'ip/mask' pattern will compute RangeMin/RangeMax and
    // ServerIdentifier directly from this pattern
    // - accept also plain '255.255.255.0' IP if you want to specify by hand the
    // raw value sent in DHCP headers, and fill RangeMin/RangeMax and others
    property SubnetMask: RawUtf8
      read fSubnetMask write fSubnetMask;
    /// some static IP addresses as CSV, reserved on the network
    // - e.g. '192.168.1.2,192.168.1.100'
    // - default is ''
    property StaticIPs: RawUtf8
      read fStaticIPs write fStaticIPs;
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
    /// if set, OnIdle will persist the internal list into this file when needed
    // - file on disk would be regular dnsmasq-compatible format
    property FileName: TFileName
      read fFileName write fFileName;
    /// at which pace the FileName should be written on disk by OnIdle
    // - default is 30 seconds which seems light and safe enough
    // - set to 0 will disable background write, and persist only at shutdown
    property FileFlushSeconds: cardinal
      read fFileFlushSeconds write fFileFlushSeconds default 30;
  end;

  /// data context used by TDhcpProcess.ProcessUdpFrame
  // - and as supplied to TOnDhcpProcess callback
  // - static allocation of all memory needed during the frame processing
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
    /// some pointer value set by the UDP server for its internal process
    Opaque: pointer;
    /// points to the raw value of doHostName option 12 in Recv.option[]
    // - points to @NULCHAR so HostName^='' if there is no such option
    HostName: PShortString;
    /// length of the Recv UDP frame received from the client
    RecvLen: integer;
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

  TDhcpProcess = class;

  /// optional callback signature for TDhcpProcess.ProcessUdpFrame
  // - input frame is parsed in Data.Recv/RecvLen/RecvLens
  // - could update SendEnd with DhcpAddOption() or set SendEnd=nil for no response
  TOnDhcpProcess = procedure(Sender: TDhcpProcess; var Data: TDhcpProcessData);

  /// maintain a list of DHCP leases
  // - contains the data logic of a simple DHCP server, good enough to implement
  // iPXE network boot together with mormot.net.tftp.server.pas
  TDhcpProcess = class(TSynPersistent)
  protected
    fSafe: TLightLock; // simple but not-rentrant lock
    fEntry: TLeaseDynArray;
    fLog: TSynLogClass;
    fCount, fLastFind, fFreeListCount: integer;
    fFreeList: TIntegerDynArray; // store indexes in fEntry[]
    fLastIpLE, fIpMinLE, fIpMaxLE: TNetIP4; // all little-endian for NextIp4
    fSubnetMask, fBroadcast, fGateway, fServerIdentifier: TNetIP4;
    fDnsServer, fSortedStatic: TIntegerDynArray;
    fLeaseTime, fLeaseTimeLE, fRenewalTime, fRebinding, fOfferHolding: cardinal;
    fSubnet: TIp4SubNet;
    fMaxDeclinePerSec, fDeclineTime, fGraceFactor, fFileFlushSeconds: cardinal;
    fIdleTix, fFileFlushTix, fModifSequence, fModifSaved: cardinal;
    fFileName: TFileName;
    fOnProcessUdpFrame: TOnDhcpProcess;
    procedure SetFileName(const name: TFileName);
    function FinalizeUdpFrame(var Data: TDhcpProcessData): PtrInt; virtual;
    function ParseUdpFrame(var Data: TDhcpProcessData): boolean; virtual;
    // low-level methods, thread-safe by the caller making fSafe.Lock/UnLock
    function FindMac(mac: Int64): PDhcpLease;
    function FindIp4(ip4: TNetIP4): PDhcpLease;
    function NewLease: PDhcpLease;
    function ReuseIp4(p: PDhcpLease): TNetIP4;
    function NextIp4: TNetIP4;
  public
    /// setup this DHCP process using the specified settings
    // - raise an EDhcp exception if the parameters are not correct - see
    // TDhcpServerSettings.Verify() if you want to intercept such errors
    // - if aSettings = nil, will use TDhcpServerSettings default parameters
    procedure Setup(aSettings: TDhcpServerSettings);
    /// register another static IP address to the internal pool
    // - in addition to aSettings.StaticIPs CSV list
    // - could be used at runtime depending on the network logic
    // - static IPs - as StaticIPs - are not persisted in FileName/SaveToFile
    function AddStatic(const ip: RawUtf8): boolean;
    /// remove one static IP address which was registered by AddStatic()
    function RemoveStatic(const ip: RawUtf8): boolean;
    /// flush the internal entry list
    procedure Clear;
    /// should be called when the server is about to leave for a clean shutdown
    // - will mark the internal state as "do-nothing" and persist FileName
    // - won't clear the internal list
    // - if you want to re-start the server, call Setup() again
    procedure Shutdown;
    /// to be called on regular pace to identify outdated entries every second
    // - return the number of outdated entries identified during this call
    // - if FileName was set, would persist any change in the internal list
    function OnIdle(tix64: Int64): integer;
    /// persist the internal entry list using human-readable text format
    // - follow "<expiry> <MAC> <IP>" dnsmasq lease file pattern, using the
    // standard seconds since epoch (Unix time) for the <expiry> value, e.g.
    // $ 1770034159 c2:07:2c:9d:eb:71 192.168.1.207
    // - we skip <hostname> and <client id> values since we don't handle them
    // - by design, only lsAck and recently lsOutdated entries are included
    function SaveToText: RawUtf8;
    /// restore the internal entry list using SaveToText() format
    function LoadFromText(const Text: RawUtf8): boolean;
    /// persist the internal entry list using SaveToText() format
    function SaveToFile(const FileName: TFileName): boolean;
    /// restore the internal entry list using SaveToText() format
    // - should be done before Setup() to validate the settings network mask
    function LoadFromFile(const FileName: TFileName): boolean;
    /// this is the main processing function of the DHCP server logic
    // - input should be stored in Data.Recv / Data.RecvLen - and is untouched
    // - returns -1 if this input was invalid or unsupported
    // - returns 0 if input was valid, but no response is needed (e.g. decline)
    // - return > 0 the number of Data.Send bytes to broadcast back as UDP
    function ProcessUdpFrame(var Data: TDhcpProcessData): PtrInt;
    /// can be used e.g. to check if IP address match the DHCP server network mask
    property Subnet: TIp4SubNet
      read fSubnet;
    /// the associated TSynLog class used to debug the execution context
    property Log: TSynLogClass
      read fLog write fLog;
    /// how many entries are currently reserved in memory
    property Count: integer
      read fCount;
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
    /// optional callback for TDhcpProcess.ProcessUdpFrame customization
    // - is called after parsing, when Data.Send is about to be returned
    property OnProcessUdpFrame: TOnDhcpProcess
      read fOnProcessUdpFrame write fOnProcessUdpFrame;
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

const
  DHCP_OPTION_NUM: array[TDhcpOption] of byte = (
    0, 1, 3, 6, 12, 15, 28, 42, 50, 51, 53, 54, 55,
    58, 59, 60, 61, 66, 67, 77, 82, 255);
var
  DHCP_OPTION_INV: array[0 .. 82] of TDhcpOption; // for fast O(1) lookup

procedure DhcpAddOption(var p: PAnsiChar; op: TDhcpOption; b: byte);
begin
  PCardinal(p)^ := cardinal(DHCP_OPTION_NUM[op]) + 1 shl 8 + cardinal(b) shl 16;
  p := @p[3];
end;

procedure DhcpAddOption(var p: PAnsiChar; op: TDhcpOption; b: pbyte; len: PtrUInt);
begin
  p[0] := AnsiChar(DHCP_OPTION_NUM[op]);
  p[1] := AnsiChar(len);
  if len <> 0 then
    MoveFast(b^, p[2], len);
  p := @p[len + 2];
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
  PCardinal(p)^ := DHCP_OPTION_NUM[doParameterRequestList];
  inc(p);
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
  DhcpAddOption(result, doMessageType, ord(dmt));
  if serverid <> 0 then
  begin
    dhcp.siaddr := serverid;
    DhcpAddOption(result, doServerIdentifier, @serverid);
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
    opt := doPadding;
    if ord(p[0]) <= high(DHCP_OPTION_INV) then
      opt := DHCP_OPTION_INV[ord(p[0])];
    if opt <> doPadding then
    begin
      if found <> nil then
        include(found^, opt);
      lens[opt] := PAnsiChar(@p[1]) - PAnsiChar(@dhcp^.options);
    end;
    // just ignore unsupported options
    p := @p[ord(p[1]) + 2];
  until p^ = #255;
  // validate message consistency
  dmt := lens[doMessageType];
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
    m := DhcpMac(dhcp, lens[doClientIdentifier]);
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
  p := lens[doParameterRequestList];
  if p = 0 then
    exit;
  inc(p, PtrUInt(@dhcp.options));
  n := PByte(p)^;
  if n <> 0 then
    repeat
      inc(p);
      opt := doPadding;
      if PByte(p)^ <= high(DHCP_OPTION_INV) then
        opt := DHCP_OPTION_INV[PByte(p)^]; // fast O(1) option number lookup
      if opt <> doPadding then
        include(result, opt);
      dec(n);
    until n = 0;
end;


{ **************** Middle-Level DHCP Scope and Lease Logic }

{ TDhcpScope }

const
  // truncate 64-bit integer to 6-bytes TNetMac - efficient on Intel and aarch64
  MAC_MASK = $0000ffffffffffff;
  // hardcore limit of dmtInform to 3 IPs per second
  MAX_INFORM = 3;
  // pre-allocate 4KB of working entries for TDhcpScope.AfterFill
  PREALLOCATE_LEASES = 250;

function DoFindMac(p: PDhcpLease; mac: Int64; n: cardinal): PDhcpLease;
begin // dedicated sub-function for better codegen
  result := p;
  repeat
    if PInt64(result)^ and MAC_MASK = mac then
      exit;
    inc(result);
    dec(n);
  until n = 0;
  result := nil;
end;

function TDhcpScope.FindMac(mac: Int64): PDhcpLease;
begin // code below expects result^.Mac to be the first field
  result := nil;
  if Count = 0 then
    exit;
  mac := mac and MAC_MASK;
  // naive but efficient lookup of the last added lease during DHCP negotiation
  if cardinal(LastDiscover) < cardinal(Count) then
  begin
    result := @Entry[LastDiscover];
    if PInt64(result)^ and MAC_MASK = mac then
      exit;
  end;
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
    result := bswap32(result); // network order
    if FastFindIntegerSorted(SortedStatic, result) < 0 then // fast O(log(n))
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
  for i := 0 to high(SortedStatic) do
    CheckSubnet('Static', SortedStatic[i]);
  AddInteger(SortedStatic, ServerIdentifier, {nodup=}true);
  QuickSortInteger(SortedStatic); // for fast branchless O(log(n)) search
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
         (FastFindIntegerSorted(SortedStatic, p^.IP4) < 0) then
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
       IP4sToText(TNetIP4s(SortedStatic)),  IP4sToText(TNetIP4s(DnsServer)),
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

function TDhcpScope.AddStatic(ip4: TNetIP4): boolean;
begin
  result := (@self <> nil) and
            SubNet.Match(ip4);
  if not result then
    exit;
  Safe.Lock;
  try
     result := AddSortedInteger(SortedStatic, ip4) >= 0;
  finally
    Safe.UnLock;
  end;
end;

function TDhcpScope.RemoveStatic(ip4: TNetIP4): boolean;
var
  ndx: PtrInt;
begin
  result := (@self <> nil) and
            SubNet.Match(ip4);
  if not result then
    exit;
  Safe.Lock;
  try
    ndx := FastFindIntegerSorted(SortedStatic, ip4);
    if ndx < 0 then
      result := false
    else
      DeleteInteger(SortedStatic, ndx);
  finally
    Safe.UnLock;
  end;
end;

function TDhcpScope.TextWrite(W: TTextWriter; tix32: cardinal; boot: TUnixTime): integer;
var
  p: PDhcpLease;
  n, grace: cardinal;
  header: boolean;
begin
  result := 0;
  if Count = 0 then
    exit;
  Safe.Lock;
  try
    n := Count;
    if n = 0 then
      exit;
    // append all lines
    header := false;
    grace := LeaseTimeLE * MaxPtrUInt(GraceFactor, 2); // not too deprecated
    p := pointer(Entry);
    repeat
      // OFFERed leases are temporary; the client hasn't accepted this IP
      // DECLINE/INFORM IPs (with MAC = 0) are ephemeral internal-only markers
      if (PInt64(@p^.Mac)^ and MAC_MASK <> 0) and
         (p^.IP4 <> 0) and
         (((p^.State = lsAck) or
          ((p^.State = lsOutdated) and // detected as p^.Expired < tix32
           (cardinal(tix32 - p^.Expired) < grace)))) then // still realistic
      begin
        if not header then
        begin
          // first add the subnet mask as '# 192.168.0.1/24' comment line
          header := true;
          W.AddDirect('#', ' ');
          W.AddShort(Subnet.ToShort);
          W.AddShorter(' subnet');
          W.AddDirectNewLine;
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
  finally
    Safe.UnLock;
  end;
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
      {$ifndef CPUINTEL}          // seems actually slower on modern Intel/AMD
      if p^.Unavailable <> 0 then // don't invalidate L1 cache
      {$endif CPUINTEL}
        p^.Unavailable := 0; // reset the rate limiter every second
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

procedure TDhcpScope.AddOptions(var f: PAnsiChar; andLeaseTimes: boolean);
begin
  DhcpAddOption(f, doSubnetMask, @Subnet.mask);
  if Broadcast <> 0 then
    DhcpAddOption(f, doBroadcastAddr, @Broadcast);
  if Gateway <> 0 then
    DhcpAddOption(f, doRouter, @Gateway);
  if DnsServer <> nil then
    DhcpAddOptions(f, doDns, pointer(DnsServer));
  if not andLeaseTimes then // static INFORM don't need timeouts
    exit;
  DhcpAddOption(f, doLeaseTimeValue,     @LeaseTime); // big-endian
  DhcpAddOption(f, doRenewalTimeValue,   @RenewalTime);
  DhcpAddOption(f, doRebindingTimeValue, @Rebinding);
end;


{ **************** High-Level Multi-Scope DHCP Server Processing Logic }

function TDhcpProcess.OnIdle(tix64: Int64): integer;
var
  tix32: cardinal;
  saved: boolean;
begin
  // make periodical process at most every second
  result := 0;
  tix32 := tix64 div MilliSecsPerSec; // GetTickSec from GetTickCount64
  if (tix32 = fIdleTix) or
     (fCount = 0) or
     (fSubnet.mask = 0) then // after Shutdown
    exit;
  fIdleTix := tix32;
  // check deprecated entries
  fSafe.Lock;
  try
    result := DoOutdated(pointer(fEntry), tix32, fCount); // efficient O(n) loop
    if result <> 0 then
      inc(fModifSequence); // trigger SaveToFile() below
  finally
    fSafe.UnLock;
  end;
  // background persist into FileName if needed
  saved := false;
  if (fFileName <> '') and
     (fFileFlushSeconds <> 0) and         // = 0 if disabled
     (fModifSaved <> fModifSequence) then // if something new to be written
    if (fFileFlushTix = 0) or
       (tix32 >= fFileFlushTix) then      // reached the next persistence time
    begin
      saved := SaveToFile(fFileName);     // do not aggressively retry if false
      fFileFlushTix := tix32 + fFileFlushSeconds; // every 30 seconds by default
    end;
  // no ARP call here to clean the internal list: the user should rather tune
  // lease duration or use a bigger netmask; the RFC states that the client
  // would do its own ARP request and resend (dmtDecline +) dmtDiscover
  // - we mitigate aggressive clients via the Unavailable counter anyway
  if Assigned(fLog) and
     ((result <> 0) or
      saved) then
    fLog.Add.Log(sllTrace, 'OnIdle: outdated=% saved=%', [result, saved], self);
end;

procedure DhcpTextWrite(p: PDhcpLease; n, grace: cardinal; W: TTextWriter);
var
  tix32: cardinal;
  boot: TUnixTime;
begin
  tix32 := GetTickSec;
  boot := UnixTimeUtc - tix32;    // = UnixTimeUtc at computer boot
  if boot > UNIXTIME_MINIMAL then // we should have booted after 08 Dec 2016
    repeat
      // OFFERed leases are temporary; the client hasn't accepted this IP
      // DECLINE/INFORM IPs (with MAC = 0) are ephemeral internal-only markers
      if (PInt64(@p^.Mac)^ and MAC_MASK <> 0) and
         (p^.IP4 <> 0) and
         (((p^.State = lsAck) or
          ((p^.State = lsOutdated) and // detected as p^.Expired < tix32
           (cardinal(tix32 - p^.Expired) < grace)))) then // still realistic
      begin
        // format is "1770034159 c2:07:2c:9d:eb:71 192.168.1.207"
        W.AddQ(boot + Int64(p^.Expired));
        W.AddDirect(' ');
        W.AddShort(MacToShort(@p^.Mac));
        W.AddDirect(' ');
        W.AddShort(IP4ToShort(@p^.IP4));
        W.AddDirectNewLine;
      end;
      inc(p);
      dec(n);
    until n = 0;
end;

function DhcpTextParse(p: PUtf8Char; var e: TLeaseDynArray): PtrInt;
var
  utc, boot: TUnixTime;
  tix32: cardinal;
  mac: TNetMac;
  ip4: TNetIP4;
  b: PUtf8Char;
  new: PDhcpLease;
begin
  result := 0; // number of entries in e[]
  if (p = nil) or
     (PWord(p)^ = 13) or
     (PCardinal(p)^ and $ffffff = $0a0d) then // '' or CRLF
    exit;
  tix32 := GetTickSec;
  boot := UnixTimeUtc - tix32;    // = UnixTimeUtc at computer boot
  if boot > UNIXTIME_MINIMAL then // we should have booted after 08 Dec 2016
    repeat
      // format is "<expiry> <MAC> <IP>"
      b := p;
      utc := GetNextItemQWord(b, ' ');
      if (utc >= boot - SecsPerMonth) and  // not too old
         (utc < TUnixTime(UNIXTIME_MINIMAL) * 2) then // range in seconds
      begin
        p := GotoNextSpace(b);
        if (p - b = 17) and
           (p^ = ' ') and
           TextToMac(b, @mac) and
           (PInt64(@mac)^ and MAC_MASK <> 0) and
           NetIsIP4(p + 1, @ip4) and
           (ip4 <> 0) then
        begin
          if length(e) = result then
            SetLength(e, NextGrow(result));
          new := @e[result];
          PInt64(@new^.Mac)^ := PInt64(@mac)^; // also reset State+Unavailable
          new^.IP4 := ip4;
          new^.Expired := utc - boot;
          if new^.Expired < tix32 then // same logic than DoOutdated()
            new^.State := lsOutdated
          else
            new^.State := lsAck;
          inc(result);
        end;
      end;
      // invalid lines are just skipped
      p := GotoNextLine(p);
    until (p = nil) or
          (p^ = #0);
end;

function TDhcpProcess.SaveToText: RawUtf8;
var
  temp: TTextWriterStackBuffer;
  local: TLeaseDynArray; // local unlocked copy
  W: TTextWriter;
  grace: cardinal;
begin
  result := CRLF; // return something not void
  if fCount = 0 then
    exit;
  fSafe.Lock;
  try
    grace := fLeaseTimeLE * MaxPtrUInt(fGraceFactor, 2); // not too deprecated
    local := copy(fEntry, 0, fCount); // keep lock as short as possible
  finally
    fSafe.UnLock;
  end;
  if local = nil then
    exit;
  W := TTextWriter.CreateOwnedStream(temp);
  try
    DhcpTextWrite(pointer(local), length(local), grace, W);
    if W.TextLength <> 0 then
      W.SetText(result);
  finally
    W.Free;
  end;
end;

function TDhcpProcess.LoadFromText(const Text: RawUtf8): boolean;
begin
  fSafe.Lock;
  if fEntry = nil then
    SetLength(fEntry, 250); // pre-allocate as in Setup()
  fCount := DhcpTextParse(pointer(Text), fEntry);
  fFreeListCount := 0;
  fModifSequence := 0;
  fModifSaved := 0;
  fSafe.UnLock;
  fLog.Add.Log(sllDebug, 'LoadFromText: entries=%', [fCount], self);
  result := true;
end;

procedure TDhcpProcess.SetFileName(const name: TFileName);
begin
  if fFileName = name then
    exit;
  LoadFromFile(name);
  fFileName := name;
end;

function TDhcpProcess.SaveToFile(const FileName: TFileName): boolean;
begin
  result := FileFromString(SaveToText, FileName);
  if not result then
    exit;
  fSafe.Lock;
  fModifSaved := fModifSequence; // success: won't retry on next OnIdle()
  fSafe.UnLock;
end;

function TDhcpProcess.LoadFromFile(const FileName: TFileName): boolean;
var
  txt: RawUtf8;
begin
  result := false;
  txt := StringFromFile(FileName);
  if txt <> '' then
    result := LoadFromText(txt);
end;

function TDhcpProcess.ParseUdpFrame(var Data: TDhcpProcessData): boolean;
begin
  Data.Ip4 := 0;
  Data.Mac64 := 0;
  Data.RecvType := DhcpParse(@Data.Recv, Data.RecvLen, Data.RecvLens, nil, @Data.Mac64);
  Data.Ip[0] := #0;
  if (Data.Mac64 <> 0) and
     (Assigned(fLog) or
      Assigned(fOnProcessUdpFrame)) then
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

function TDhcpProcess.FinalizeUdpFrame(var Data: TDhcpProcessData): PtrInt;
begin
  // optional callback support
  if Assigned(fOnProcessUdpFrame) then
  begin
    fOnProcessUdpFrame(self, Data);
    if Data.SendEnd = nil then
    begin
      if Assigned(fLog) then
        fLog.Add.Log(sllTrace, 'ProcessUdpFrame: % % ignored by callback',
          [DHCP_TXT[Data.RecvType], Data.Mac], self);
      result := 0;
      exit; // callback asked to silently ignore this frame
    end;
  end;
  // support Option 82 Relay Agent by sending it back - should be the last option
  if Data.RecvLens[doRelayAgent] <> 0 then
    DhcpCopyOption(Data.SendEnd, @Data.Recv.options[Data.RecvLens[doRelayAgent]]);
  // add trailer to the Data.Send frame and returns its final size in bytes
  Data.SendEnd^ := #255;
  result := Data.SendEnd - PAnsiChar(@Data.Send) + 1;
end;

function TDhcpProcess.ProcessUdpFrame(var Data: TDhcpProcessData): PtrInt;
var
  p: PDhcpLease;
  f: PAnsiChar;
begin
  result := -1; // error
  // do nothing on missing Setup() or after Shutdown
  if fSubnet.mask = 0 then
    exit;
  // parse and validate the request
  if not ParseUdpFrame(Data) then
  begin
    if Assigned(fLog) then
      fLog.Add.Log(sllTrace, 'ProcessUdpFrame: % % unexpected %',
        [DHCP_TXT[Data.RecvType], Data.Mac, Data.HostName^], self);
    exit; // invalid or unsupported frame
  end;
  // compute the corresponding IPv4 according to the internal lease list
  result := 0; // no response, but no error
  Data.Tix32 := GetTickSec;
  fSafe.Lock;
  try
    p := FindMac(Data.Mac64);
    case Data.RecvType of
      dmtDiscover:
        begin
          if (p = nil) or    // first time this MAC is seen
             (p^.IP4 = 0) or // may happen after DECLINE
             (p^.State = lsReserved) then // reserved = client refused this IP
          begin
            // first time seen (most common case), or renewal
            if Data.RecvLens[doRequestedIp] <> 0 then
            begin
              Data.Ip4 := DhcpIP4(@Data.Recv, Data.RecvLens[doRequestedIp]);
              if Data.Ip4 <> 0 then
                if (not fSubNet.Match(Data.Ip4)) or
                   (FastFindIntegerSorted(fSortedStatic, Data.Ip4) >= 0) or
                   (FindIp4(Data.Ip4) <> nil) then
                begin
                  // this IP seems already used by another MAC
                  if Assigned(fLog) then
                  begin
                    IP4Short(@Data.Ip4, Data.Ip);
                    fLog.Add.Log(sllTrace,
                      'ProcessUdpFrame: DISCOVER % ignored requestedip=% %',
                      [Data.Mac, Data.Ip, Data.HostName^], self);
                  end;
                  Data.Ip4 := 0; // NextIP4
                end;
            end;
            if Data.Ip4 = 0 then
              Data.Ip4 := NextIp4;
            if Data.Ip4 = 0 then
            begin
              // IPv4 exhausted: don't return NAK, but silently ignore
              // - client will retry automatically after a small temporisation
              fLog.Add.Log(sllWarning,
                'ProcessUdpFrame: DISCOVER % exhausted IPv4 %',
                  [Data.Mac, Data.HostName^], self);
              exit;
            end;
            if p = nil then
            begin
              p := NewLease;
              PInt64(@p^.Mac)^ := Data.Mac64; // also reset State+Unavailable
            end;
            p^.IP4 := Data.Ip4;
          end
          else
            // keep existing/previous IP4
            Data.Ip4 := p^.IP4;
          // respond with an OFFER
          p^.State := lsReserved;
          p^.Expired := Data.Tix32 + fOfferHolding;
          Data.SendType := dmtOffer;
        end;
      dmtRequest:
        begin
          // especially for PXE networks, it is safe and common to ignore
          // Option 50 and ciaddr in REQUEST and just ACK the already OFFERed IP
          if (p = nil) or
             (p^.IP4 = 0) or
             not ((p^.State in [lsReserved, lsAck]) or
                  ((p^.State = lsOutdated) and
                   (fGraceFactor > 1) and
                   (fLeaseTimeLE < SecsPerHour) and // grace period
                   (Data.Tix32 - p^.Expired < fLeaseTimeLE * fGraceFactor))) then
          begin
            fLog.Add.Log(sllDebug,
              'ProcessUdpFrame: REQUEST % out-of-sync NAK %',
                [Data.Mac, Data.HostName^], self);
            // send a NAK response anyway
            Data.SendType := dmtNak;
            Data.SendEnd := DhcpNew(Data.Send, dmtNak, Data.Recv.xid,
              PNetMac(@Data.Mac64)^, fServerIdentifier);
            result := FinalizeUdpFrame(Data);
            exit;
          end;
          // respond with an ACK on the known IP
          Data.Ip4 := p^.IP4;
          p^.State := lsAck;
          p^.Expired := Data.Tix32 + fLeaseTimeLE;
          Data.SendType := dmtAck;
        end;
      dmtDecline:
        begin
          // client ARPed the OFFERed IP and detect conflict, so DECLINE it
          if (p = nil) or
             (p^.State <> lsReserved) then
          begin
            // ensure the server recently OFFERed one IP to that client
            // (match RFC intent and prevent blind poisoning of arbitrary IPs)
            fLog.Add.Log(sllDebug,
              'ProcessUdpFrame: DECLINE % with no previous OFFER %',
                [Data.Mac, Data.HostName^], self);
            exit;
          end;
          if (fMaxDeclinePerSec <> 0) and // = 5 by default
             (fIdleTix <> 0) then         // OnIdle() is actually running
            if p^.Unavailable < fMaxDeclinePerSec then // rate limiting
              inc(p^.Unavailable)         // will be reset to 0 by OnIdle()
            else
            begin
              // malicious client poisons the pool by sending repeated DECLINE
              fLog.Add.Log(sllDebug,
                'ProcessUdpFrame: DECLINE % overload', [Data.Mac], self);
              exit;
            end;
          // invalidate OFFERed IP
          Data.Ip4 := DhcpIP4(@Data.Recv, Data.RecvLens[doRequestedIp]); // authoritative IP
          if (Data.Ip4 <> 0) and
             not fSubNet.Match(Data.Ip4) then // need clean IP
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
              fLog.Add.Log(sllTrace, 'ProcessUdpFrame: DECLINE % as %',
                [Data.Mac, Data.Ip], self);
            end;
            p := NewLease;
            PInt64(@p^.Mac)^ := 0; // used as sentinel to store this IP
            p^.State := lsUnavailable;
            p^.IP4 := Data.Ip4;
            p^.Expired := Data.Tix32 + fDeclineTime;
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
              'ProcessUdpFrame: RELEASE % unexpected', [Data.Mac], self)
          else
          begin
            if Assigned(fLog) then
            begin
              IP4Short(@p^.IP4, Data.Ip);
              fLog.Add.Log(sllTrace, 'ProcessUdpFrame: RELEASE % as %',
                [Data.Mac, Data.Ip], self);
            end;
            ReuseIp4(p); // set MAC=0 IP=0 State=lsFree
            inc(fModifSequence); // trigger SaveToFile() in next OnIdle()
          end;
          exit; // server MUST NOT respond to a RELEASE message
        end;
      dmtInform:
        begin
          // client requests configuration options for its static IP
          Data.Ip4 := Data.Recv.ciaddr;
          if ((p <> nil) and
              ((p^.Unavailable >= MAX_INFORM) or // rate limit to 3 per sec
               not (p^.State in [lsUnavailable, lsOutdated]))) or
             not fSubnet.Match(Data.Ip4) then
          begin
            IP4Short(@Data.Ip4, Data.Ip);
            fLog.Add.Log(sllDebug,
              'ProcessUdpFrame: INFORM % unexpected %', [Data.Mac, Data.Ip], self);
            exit;
          end;
          // respond with an ACK and its options (but no timeout values)
          if p = nil then
          begin
            // create a new lease for this MAC
            p := NewLease;
            PInt64(@p^.Mac)^ := Data.Mac64; // also reset State+Unavailable
            inc(p^.Unavailable);            // will be reset to 0 by OnIdle()
          end
          else
          begin
            inc(p^.Unavailable);            // will be reset to 0 by OnIdle()
            if p^.IP4 <> Data.Ip4 then
            begin
              // create a new MAC-free lease for this IP
              p := NewLease;
              PInt64(@p^.Mac)^ := 0; // used as sentinel to store this IP
            end;
          end;
          p^.State := lsUnavailable;
          p^.IP4 := Data.Ip4;
          p^.Expired := Data.Tix32 + fDeclineTime;
          Data.SendType := dmtAck;
        end;
    else
      exit; // paranoid
    end;
  finally
    fSafe.UnLock;
  end;
  // compute the dmtOffer/dmtAck response frame over the very same xid
  if Assigned(fLog) or
     Assigned(fOnProcessUdpFrame) then
    IP4Short(@Data.Ip4, Data.Ip);
  if Assigned(fLog) then
    with fLog.Family do
      if sllTrace in Level then
        Add.Log(sllTrace, 'ProcessUdpFrame: % % into % % %',
          [DHCP_TXT[Data.RecvType], Data.Mac, DHCP_TXT[Data.SendType], Data.Ip,
           Data.HostName^], self);
  f := DhcpNew(Data.Send, Data.SendType, Data.Recv.xid,
    PNetMac(@Data.Mac64)^, fServerIdentifier);
  Data.Send.ciaddr := Data.Ip4;
  DhcpAddOption(f, doSubnetMask, @fSubnetMask);
  if fBroadcast <> 0 then
    DhcpAddOption(f, doBroadcastAddr, @fBroadcast);
  if fGateway <> 0 then
    DhcpAddOption(f, doRouter, @fGateway);
  if fDnsServer <> nil then
    DhcpAddOptions(f, doDns, pointer(fDnsServer));
  if Data.RecvType <> dmtInform then // static INFORM don't need timeouts
  begin
    DhcpAddOption(f, doLeaseTimeValue,     @fLeaseTime); // big-endian
    DhcpAddOption(f, doRenewalTimeValue,   @fRenewalTime);
    DhcpAddOption(f, doRebindingTimeValue, @fRebinding);
  end;
  // TODO: IPXE host/file options
  Data.SendEnd := f;
  result := FinalizeUdpFrame(Data); // callback + option 82 + length
  inc(fModifSequence); // trigger SaveToFile() in next OnIdle()
end;


{ TDhcpServerSettings }

constructor TDhcpServerSettings.Create;
begin
  inherited Create;
  fSubnetMask := '192.168.1.1/24';
  fLeaseTimeSeconds := 120; // avoid IP exhaustion during iPXE process
  fOfferHoldingSecs := 5;
  fMaxDeclinePerSecond := 5;
  fGraceFactor := 2;
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


initialization
  assert(ord(dmtTls) = 18);
  assert(PtrUInt(@PDhcpPacket(nil)^.options) = 240);
  assert(SizeOf(TDhcpPacket) = 548);
  GetEnumTrimmedNames(TypeInfo(TDhcpMessageType), @DHCP_TXT, stUpperCase);
  FillLookupTable(@DHCP_OPTION_NUM, @DHCP_OPTION_INV, ord(high(DHCP_OPTION_NUM)));
  assert(DHCP_OPTION_INV[high(DHCP_OPTION_INV)] = doRelayAgent);

end.

