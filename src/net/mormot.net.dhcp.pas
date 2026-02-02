/// Simple DHCP Protocol Support
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.dhcp;

{
  *****************************************************************************

   Simple Dynamic Host Configuration Protocol (DHCP) Protocol Support
    - Low-Level DHCP Protocol Definitions
    - High-Level DHCP Server

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
  mormot.core.log,
  mormot.net.sock;


{ **************** Low-Level DHCP Protocol Definitions }

const
  DHCP_SERVER_PORT = 67;
  DHCP_CLIENT_PORT = 68;

type
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
    dmtInFrom,
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

  /// a DHCP UDP packet message as defined in RFC 2131 / BOOTP
  TDhcpPacket = packed record
    op:     byte;  // BOOTP type, not TDhcpMessageType
    htype:  byte;  // hardware type, mostly
    hlen:   byte;
    hops:   byte;
    xid:    cardinal;
    secs:   word;
    flags:  word;
    ciaddr: TNetIP4;
    yiaddr: TNetIP4;
    siaddr: TNetIP4;
    giaddr: TNetIP4;
    chaddr: array[0..15] of byte;
    sname:  array[0..63] of byte;
    file_:  array[0..127] of byte;
    cookie: cardinal;
    options: array[0..307] of byte;
  end;
  PDhcpPacket = ^TDhcpPacket;

  /// the main DHCP options
  TDhcpOption = (
   doPadding,               // Padding value 0
   doSubnetMask,            // Subnet Mask 1 (255.255.255.0)
   doRouter,                // Default Gateway 3 (192.168.1.1)
   doDns,                   // DNS Servers 6 (8.8.8.8, 8.8.4.4)
   doHostName,              // Hostname 12 (client01)
   doDomainName,            // Domain Name  15 (example.local)
   doBroadcastAddr,         // Broadcast Address 28 (192.168.1.255)
   doNtpServer,             // NTP Server 42
   doRequestedIp,           // Requested IP Address 50 (0.0.0.0)
   doLeaseTimeValue,        // IP Lease Duration in seconds 51 (86400 for 24h)
   doMessageType,           // DHCP Message Type 53 (TDhcpMessageType)
   doServerIdentifier,      // DHCP Server Identifier 54 (192.168.1.1)
   doParameterRequestList,  // Parameter Request List 55 (1,3,6,15,51,54)
   doRenewalTimeValue,      // Renewal Time Value (T1) in seconds 58 (43200 for 12h)
   doRebindingTimeValue,    // Rebinding Time Value (T2) in seconds 59 (75600 for 21h)
   doVendorClassIdentifier, // Vendor Class Identifier (PXE RFC 2132) 60 (PXEClient)
   doClientIdentifier,      // Client Identifier 61 ($01 + MAC)
   doTftpServerName,        // TFTP Server Name (PXE) 66 (192.168.10.10 or host name)
   doBootfileName,          // Bootfile Name (PXE) 67 (pxelinux.0)
   doUserClass,             // User Class (PXE RFC 3004) 77 (PXEClient:Arch:00000)
   doEnding                 // End Of Options marker 255
 );
 /// set of supported DHCP options
 TDhcpOptions = set of TDhcpOption;
 /// pointer to a set of supported DHCP options
 PDhcpOptions = ^TDhcpOptions;

function ToText(dmt: TDhcpMessageType): PShortString; overload;
function ToText(opt: TDhcpOption): PShortString; overload;

const
  /// 1,3,6,15,28 options as used for DhcpClient()
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

type
 /// efficient DhcpParse() resultset
 // - store the length position of an option in TDhcpPacket.options[]
 // - 0 means that this TDhcpOption was not transmitted
 // - very efficient O(1) lookup of recognized options with no memory allocation
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

const
  BOOT_REQUEST = 1;
  BOOT_REPLY   = 2;


{ **************** High-Level DHCP Server }

type
  /// main high-level options for a minimal DHCP Server
  TDhcpServerSettings = class(TSynPersistent)
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
    fServerIdentifier: RawUtf8;
    fOfferHoldingSecs: cardinal;
    fGraceFactor: cardinal;
  public
    /// setup this instance with default values
    // - default are just SubnetMask = '192.168.1.1/24', LeaseTimeSeconds = 120
    // and OfferHoldingSecs = 5, consistent with a simple local iPXE network
    constructor Create; override;
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
  end;

type
  /// the state of one DHCP TDhcpLease entry in memory
  // - lsFree identify an lsOutdated slot which IP4 has been reused
  // - lsReserved is used when an OFFER has been sent back to the client with IP4
  // - lsAck is used when REQUEST has been recevied and ASK has been sent
  // - lsDeclinedIP is set when a DECLINE message is received from a client
  // - lsOutdated is set once Timeout has been reached, so that the IP4 address
  // could be reused on the next DISCOVER for this MAC
  TLeaseState = (
    lsFree,
    lsReserved,
    lsAck,
    lsDeclinedIP,
    lsOutdated);

  /// store one DHCP lease in memory
  // - efficiently padded to 16 bytes
  TDhcpLease = packed record
    /// the MAC address of this entry - should be first
    // - we only lookup clients by MAC: no DUID is supported (we found it error
    // prone in practice, when some VMs are duplicated with the same DUID)
    // - may contain 0 for a lsFree or lsDeclinedIP entry
    Mac: TNetMac;
    /// how this entry should be handled
    State: TLeaseState;
    /// align the whole structure to cpu-cache-friendly 16 bytes
    Padding: byte;
    /// the 32-bit reserved IP
    IP4: TNetIP4;
    /// TUnixTime value stored as 32-bit unsigned integer (valid up to year 2152)
    Timeout: TUnixTimeMinimal;
  end;
  /// points to one DHCP lease entry in memory
  PDhcpLease = ^TDhcpLease;

  /// a dynamic array of TDhcpLease entries, as stored within TDhcpProcess
  TLeaseDynArray = array of TDhcpLease;

  /// Exception class raised by this unit
  EDhcp = class(ESynException);

  /// maintain a list of DHCP leases
  // - contains the data logic of a simple DHCP server, good enough to implement
  // iPXE network boot together with mormot.net.tftp.server.pas
  TDhcpProcess = class(TSynPersistent)
  protected
    fSafe: TLightLock;
    fEntry: TLeaseDynArray;
    fLog: TSynLogClass;
    fCount, fLastFind, fFreeListCount: integer;
    fFreeList: TIntegerDynArray;
    fLastIpLE, fIpMinLE, fIpMaxLE: TNetIP4; // all little-endian for NextIp4
    fSubnetMask, fBroadcast, fGateway, fServerIdentifier: TNetIP4;
    fDnsServer, fStatic: TNetIP4s;
    fLeaseTime, fLeaseTimeLE, fRenewalTime, fRebinding, fOfferHolding: cardinal;
    fSubnet: TIp4SubNet;
    fGraceFactor, fIdleTix, fModifSequence, fModifSaved: cardinal;
    fFileName: TFileName;
    procedure SetFileName(const name: TFileName);
    // low-level methods, thread-safe by the caller making fSafe.Lock/UnLock
    function FindMac(mac: Int64): PDhcpLease;
    function FindIp4(ip4: TNetIP4): PDhcpLease;
    function NewLease: PDhcpLease;
    function NextIp4: TNetIP4;
  public
    /// setup this DHCP process using the specified settings
    // - raise an EDhcp exception if the parameters are not correct
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
    /// to be called on regular pace to identify outdated entries every second
    // - return the number of outdated entries identified during this call
    // - if FileName was set, would persist any change in the internal list
    function OnIdle(tix32: cardinal): integer;
    /// persist the internal entry list using raw binary
    function SaveToFile(const FileName: TFileName): boolean;
    /// restore the internal entry list using raw binary
    // - should be done before Setup() to validate the settings network mask
    function LoadFromFile(const FileName: TFileName): boolean;
    /// this is the main processing function of the DHCP server logic
    // - returns false if the Frame input was invalid
    // - fills Frame with the proper answer, and return true and set Len to
    // the number of bytes of Frame as response to send back as UDP broadcast
    function ProcessUdpFrame(var Frame: TDhcpPacket; var Len: PtrInt): boolean;
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
    property FileName: TFileName
      read fFileName write SetFileName;
    /// the internal list modification sequence number
    // - increased at every update of the internal entry list
    // - used e.g. by OnIdle() to trigger SaveToFile() if FileName is defined
    property ModifSequence: cardinal
      read fModifSequence;
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
    58, 59, 60, 61, 66, 67, 77, 255);

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
  if ips <> nil then // ips is a dynamic array of TNetIP4 = cardinal
    DhcpAddOption(p, op, pointer(ips), (PDALen(ips - _DALEN)^ + _DAOFF) * 4);
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
  DHCP_MAGIC_COOKIE = $63538263;  // little-endian 'c' 'S' 'c' 'C'

  DHCP_BOOT: array[TDhcpMessageType] of byte = (
    0,            // dmtUndefined
    BOOT_REQUEST, // dmtDiscover
    BOOT_REPLY,   // dmtOffer
    BOOT_REQUEST, // dmtRequest
    BOOT_REQUEST, // dmtDecline
    BOOT_REPLY,   // dmtAck
    BOOT_REPLY,   // dmtNak
    BOOT_REQUEST, // dmtRelease
    BOOT_REQUEST, // dmtInFrom
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
  opt: PtrInt; // TDhcpOption
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
    opt := ByteScanIndex(@DHCP_OPTION_NUM, SizeOf(DHCP_OPTION_NUM), ord(p[0]));
    if opt >= 0 then // just ignore unsupported options
    begin
      if found <> nil then
        include(found^, TDhcpOption(opt));
      lens[TDhcpOption(opt)] := PAnsiChar(@p[1]) - PAnsiChar(@dhcp^.options);
    end;
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
    result := @dhcp^.options[len];
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
  p, n, o: PtrUInt;
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
      o := ByteScanIndex(@DHCP_OPTION_NUM, SizeOf(DHCP_OPTION_NUM), PByte(p)^);
      if o > 0 then
        include(result, TDhcpOption(o));
      dec(n);
    until n = 0;
end;


{ **************** High-Level DHCP Server }


{ TDhcpProcess }

{ low-level methods, thread-safe by the caller making fSafe.Lock/UnLock }

const
  MAC_MASK = $0000ffffffffffff; // truncate 64-bit to TNetMac

function TDhcpProcess.FindMac(mac: Int64): PDhcpLease;
var
  n: integer;
begin // code below expects result^.Mac to be first
  result := nil;
  if fCount = 0 then
    exit;
  mac := mac and MAC_MASK;
  if fLastFind < fCount then
  begin
    result := @fEntry[fLastFind];
    if PInt64(result)^ and MAC_MASK = mac then
      exit; // naive but efficient search during DHCP negotiation
  end;
  result := pointer(fEntry); // efficient O(n) brute force search
  n := fCount;
  repeat
    if PInt64(result)^ and MAC_MASK = mac then
      exit;
    inc(result);
    dec(n);
  until n = 0;
  result := nil;
end;

function TDhcpProcess.FindIp4(ip4: TNetIP4): PDhcpLease;
var
  n: integer;
begin
  result := pointer(fEntry);
  n := fCount;
  if n <> 0 then
    repeat
      if result^.IP4 = ip4 then
        exit;
      inc(result);
      dec(n);
    until n = 0;
  result := nil;
end;

function TDhcpProcess.NewLease: PDhcpLease;
begin
  // first try if we have any lsFree entries
  if fFreeListCount <> 0 then
  begin
    dec(fFreeListCount);
    fLastFind := fFreeList[fFreeListCount];
    result := @fEntry[fLastFind];
    if result^.State = lsFree then
      exit; // paranoid
  end;
  // we need to add a new entry - maybe with reallocation
  if fCount = length(fEntry) then
    SetLength(fEntry, NextGrow(fCount));
  result := @fEntry[fCount];
  fLastFind := fCount;
  inc(fCount);
end;

function TDhcpProcess.NextIp4: TNetIP4;
var
  le: TNetIP4;
  looped: boolean;
  existing, outdated: PDhcpLease;
begin
  result := fLastIpLE; // all those f*LE variables are in little-endian order
  if (result < fIpMinLE) or
     (result > fIpMaxLE) then
    result := fIpMinLE - 1; // e.g. at startup
  outdated := nil;
  looped := false;
  repeat
    inc(result);
    if result > fIpMaxLE then
      // we reached the end of IP range
      if looped then
      begin
        // all IPs are already used in the internal list
        if outdated <> nil then
        begin
          // reuse the oldest outdated slot
          AddInteger(fFreeList, fFreeListCount,
            {index=}(PtrUInt(outdated) - PtrUInt(fEntry)) div SizeOf(fEntry[0]));
          result := outdated^.IP4; // reuse this IP4
          FillZero(THash128(outdated^)); // and set outdated^.State := lsFree;
        end
        else
          // pool exhausted: consider shorten the lease duration to 60-300 secs
          result := 0;
        exit;
      end
      else
      begin
        // wrap around the IP range once
        looped := true;
        result := fIpMinLE;
      end;
    le := result;
    result := bswap32(result); // network order
    if not IntegerScanExists(pointer(fStatic), length(fStatic), result) then
    begin
      existing := FindIp4(result);
      if existing = nil then
      begin
        // return the unused IP found
        fLastIpLE := le;
        exit;
      end
      else if (existing^.State in [lsDeclinedIP, lsOutdated]) and
              ((outdated = nil) or
               (existing^.Timeout < outdated^.Timeout)) then
        outdated := existing; // detect oldest outdated entry
    end;
    result := le;
  until false;
end;


{ general-purpose methods }

procedure TDhcpProcess.Setup(aSettings: TDhcpServerSettings);
var
  owned: boolean;
  i, n: PtrInt;
  p: PDhcpLease;

  procedure CheckSubNet(ident: PUtf8Char; ip: TNetIP4);
  begin
    if not fSubnet.Match(ip) then
      EDhcp.RaiseUtf8('%.Setup: SubNetMask=% does not match %=%',
        [self, aSettings.SubnetMask, ident, IP4ToShort(@ip)]);
  end;

begin
  owned := aSettings = nil;
  if owned then
    aSettings := TDhcpServerSettings.Create; // use default values
  fSafe.Lock;
  try
    // convert the text settings into binary raw values
    fGateway          := ToIP4(aSettings.DefaultGateway);
    fBroadcast        := ToIP4(aSettings.BroadCastAddress);
    fServerIdentifier := ToIP4(aSettings.ServerIdentifier);
    fLastIpLE         := 0;
    fIpMinLE          := ToIP4(aSettings.RangeMin);
    fIpMaxLE          := ToIP4(aSettings.RangeMax);
    fDnsServer        := ToIP4s(aSettings.DnsServers);
    fStatic           := ToIP4s(aSettings.StaticIPs);
    fLeaseTime        := aSettings.LeaseTimeSeconds; // 100%
    if fLeaseTime < 30 then
      fLeaseTime := 30;                              // 30 seconds minimum lease
    fRenewalTime      := fLeaseTime shr 1;           // 50%
    fRebinding        := (fLeaseTime * 7) shr 3;     // 87.5%
    fOfferHolding     := aSettings.OfferHoldingSecs; // 5 seconds
    if fOfferHolding < 1 then
      fOfferHolding := 1
    else if fOfferHolding > fRenewalTime then
      fOfferHolding := fRenewalTime;
    fGraceFactor      := aSettings.GraceFactor;      // * 2
    // retrieve the subnet mask from settings
    if not fSubnet.From(aSettings.SubnetMask) then
      EDhcp.RaiseUtf8(
        '%.Setup: unexpected SubNetMask=% (should be mask ip or ip/sub)',
        [self, aSettings.SubnetMask]);
    if fSubnet.mask = cAnyHost32 then
    begin
      // SubNetMask was not '192.168.0.1/24': is expected to be '255.255.255.0'
      if IP4Prefix(fSubnet.ip) = 0 then
        EDhcp.RaiseUtf8('%.Setup: SubNetMask=% is not a mask IPv4',
          [self, aSettings.SubnetMask]);
      fSubnetMask := fSubnet.ip;
      // we expect other parameters to be set specifically: compute fSubnet
      if fServerIdentifier = 0 then
        EDhcp.RaiseUtf8('%.Setup: SubNetMask=% but without ServerIdentifier',
          [self, aSettings.SubnetMask]);
      fSubnet.mask := fSubnetMask;
      fSubnet.ip := fServerIdentifier and fSubnetMask; // normalize as in From()
    end
    else
      // SubNetMask was e.g. '192.168.0.1/24'
      if fServerIdentifier = 0 then
        // extract from exact SubNetMask text, since fSubnet.ip = 192.168.0.0
        fServerIdentifier := ToIP4(aSettings.SubnetMask);
    // validate all settings values from the actual subnet
    fSubnetMask := fSubnet.mask;
    if fGateway <> 0 then
      CheckSubnet('DefaultGateway', fGateway);
    CheckSubnet('ServerIdentifier', fServerIdentifier);
    if fIpMinLE = 0 then
      fIpMinLE := fSubnet.ip + $0a000000; // e.g. 192.168.0.10
    CheckSubnet('RangeMin', fIpMinLE);
    if fIpMaxLE = 0 then
      // e.g. 192.168.0.0 + pred(not(255.255.255.0)) = 192.168.0.254
      fIpMaxLE := bswap32(bswap32(fSubnet.ip) + pred(not(bswap32(fSubnet.mask))));
    CheckSubnet('RangeMax', fIpMaxLE);
    if bswap32(fIpMaxLE) <= bswap32(fIpMinLE) then
      EDhcp.RaiseUtf8('%.Setup: unexpected %..% range',
        [self, IP4ToShort(@fIpMinLE), IP4ToShort(@fIpMaxLE)]);
    if fBroadcast <> 0 then
      CheckSubNet('BroadCastAddress', fBroadcast);
    for i := 0 to high(fStatic) do
      CheckSubnet('Static', fStatic[i]);
    AddInteger(TIntegerDynArray(fStatic), fServerIdentifier, {nodup=}true);
    // prepare the leases in-memory database
    if fEntry = nil then
      SetLength(fEntry, 250) // pre-allocate 4KB of working entries
    else
    begin
      // validate all current leases from the actual subnet
      n := 0;
      p := pointer(fEntry);
      for i := 0 to fCount - 1 do
      begin
        if fSubnet.Match(p^.IP4) then
        begin
          if n <> i then
            fEntry[n] := p^;
          inc(n);
        end;
        inc(p);
      end;
      if n <> fCount then
      begin
        fLog.Add.Log(sllTrace, 'Setup: subnet adjust count=% from %',
          [n, fCount], self);
        fCount := n;
        inc(fModifSequence); // trigger SaveToFile() in next OnIdle()
      end;
      fFreeListCount := 0;
    end;
    // log the used network context
    if Assigned(fLog) then
      fLog.Add.Log(sllInfo, 'Setup%: subnet=% min=% max=% server=% broadcast=%' +
        ' gateway=% static=% dns=% lease=% renew=% rebind=% offer=%',
        [aSettings, IP4ToShort(@fSubnetMask), IP4ToShort(@fIpMinLE),
         IP4ToShort(@fIpMaxLE), IP4ToShort(@fServerIdentifier),
         IP4ToShort(@fBroadcast), IP4ToShort(@fGateway),
         IP4sToText(fStatic),  IP4sToText(fDnsServer),
         fLeaseTime, fRenewalTime, fRebinding, fOfferHolding],
        self);
    // store internal values in the more efficient endianess for direct usage
    fIpMinLE     := bswap32(fIpMinLE);      // little-endian
    fIpMaxLE     := bswap32(fIpMaxLE);
    fLeaseTimeLE := fLeaseTime;
    fLeaseTime   := bswap32(fLeaseTime);    // big-endian
    fRenewalTime := bswap32(fRenewalTime);
    fRebinding   := bswap32(fRebinding);
  finally
    fSafe.UnLock;
    if owned then
      aSettings.Free; // avoid memory leak
  end;
end;

function TDhcpProcess.AddStatic(const ip: RawUtf8): boolean;
var
  ip4: TNetIP4;
begin
  result := NetIsIP4(pointer(ip), @ip4);
  if not result then
    exit;
  fSafe.Lock;
  try
    result := AddInteger(TIntegerDynArray(fStatic), ip4, {nodup=}true);
  finally
    fSafe.UnLock;
  end;
end;

function TDhcpProcess.RemoveStatic(const ip: RawUtf8): boolean;
var
  ip4: TNetIP4;
  ndx: PtrInt;
begin
  result := NetIsIP4(pointer(ip), @ip4);
  if not result then
    exit;
  fSafe.Lock;
  try
    ndx := IntegerScanIndex(pointer(fStatic), length(fStatic), ip4);
    if ndx < 0 then
      result := false
    else
      DeleteInteger(TIntegerDynArray(fStatic), ndx);
  finally
    fSafe.UnLock;
  end;
end;

procedure TDhcpProcess.Clear;
begin
  fSafe.Lock;
  fCount := 0;
  fEntry := nil;
  fModifSequence := 0;
  fModifSaved := 0;
  fSafe.UnLock;
end;

function DoOutdated(p: PDhcpLease; utc: TUnixTimeMinimal; n: integer): integer;
begin
  result := 0;
  if n <> 0 then
    repeat
      if (p^.Timeout < utc) and
         (p^.State in [lsReserved, lsAck, lsDeclinedIP]) then
      begin
        inc(result);
        p^.State := lsOutdated;
      end;
      inc(p);
      dec(n);
    until n = 0;
end;

function TDhcpProcess.OnIdle(tix32: cardinal): integer;
var
  utc: TUnixTimeMinimal;
  saved: boolean;
begin
  result := 0;
  // make periodical process at most every second
  if (tix32 = fIdleTix) or
     (fCount = 0) then
    exit;
  fIdleTix := tix32;
  // check deprecated entries
  utc := UnixTimeMinimalUtc;
  fSafe.Lock;
  try
    result := DoOutdated(pointer(fEntry), utc, fCount); // efficient O(n) loop
    if result <> 0 then
      inc(fModifSequence); // trigger SaveToFile() below
  finally
    fSafe.UnLock;
  end;
  saved := false;
  // persist into FileName if needed
  if (fFileName <> '') and
     (fModifSaved <> fModifSequence) then
    saved := SaveToFile(fFileName);
  if Assigned(fLog) and
     ((result <> 0) or
      saved) then
    fLog.Add.Log(sllTrace, 'OnIdle: outdated=% saved=%', [result, saved], self);
  // no ARP call here to clean the internal list: the user should rather tune
  // lease duration or use a bigger netmask; the RFC states that the client
  // would do its own ARP request and resend (dmtDecline +) dmtDiscover
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
  fSafe.Lock;
  result := FileFromBuffer(pointer(fEntry), fCount * SizeOf(fEntry[0]), FileName);
  if result then
    fModifSaved := fModifSequence; // won't retry on next OnIdle()
  fSafe.UnLock;
end;

function TDhcpProcess.LoadFromFile(const FileName: TFileName): boolean;
var
  bin: RawByteString;
begin
  Clear;
  bin := StringFromFile(FileName);
  result := false;
  if (bin <> '') and
     (length(bin) mod SizeOf(fEntry[0]) <> 0) then
    exit;
  result := true;
  if bin = '' then
    exit;
  fSafe.Lock;
  fCount := length(bin) div SizeOf(fEntry[0]);
  SetLength(fEntry, NextGrow(fCount));
  MoveFast(pointer(bin)^, pointer(fEntry)^, length(bin));
  fModifSequence := 0;
  fModifSaved := 0;
  fSafe.UnLock;
end;

function TDhcpProcess.ProcessUdpFrame(
  var Frame: TDhcpPacket; var Len: PtrInt): boolean;
var
  lens: TDhcpParsed;
  dmt: TDhcpMessageType;
  fnd: TDhcpOptions;
  p: PDhcpLease;
  f: PAnsiChar;
  mac64: Int64;
  mac: TNetMac absolute mac64;
  ip4: TNetIP4;
  utc: TUnixTimeMinimal;
  macx: string[12]; // no memory allocation during the process
begin
  result := false;
  // parse and validate the request
  ip4 := 0;
  mac64 := 0;
  dmt := DhcpParse(@Frame, Len, lens, @fnd, @mac);
  Len := 0; // no response
  macx[0] := #12;
  if Assigned(fLog) then
    BinToHexLower(@mac, @macx[1], 6);
  if (mac64 = 0) or
     not (dmt in [dmtDiscover, dmtRequest, dmtDecline]) then
  begin
    if Assigned(fLog) then
      fLog.Add.Log(sllTrace, 'ProcessUdpFrame: invalid % frame from %',
        [ToText(dmt)^, macx], self);
    exit; // invalid or unsupported frame
  end;
  // compute the corresponding IPv4 according to the internal lease list
  utc := UnixTimeMinimalUtc;
  fSafe.Lock;
  try
    case dmt of
      dmtDiscover:
        begin
          p := FindMac(mac64);
          if (p = nil) or    // first time this MAC is seen
             (p^.IP4 = 0) or // may happen after dmtDecline
             (p^.State = lsReserved) then // reserved = client refused this IP
          begin
            // first time seen (most common case), or renewal
            if lens[doRequestedIp] <> 0 then
            begin
              ip4 := DhcpIP4(@Frame, lens[doRequestedIp]); // try to renew
              if ip4 <> 0 then
                if (not fSubNet.Match(ip4)) or
                   IntegerScanExists(pointer(fStatic), length(fStatic), ip4) or
                   (FindIp4(ip4) <> nil) then
                begin
                  // this IP seems already used by another MAC
                  if Assigned(fLog) then
                    fLog.Add.Log(sllDebug,
                      'ProcessUdpFrame: ignore invalid requestedip=%',
                      [IP4ToShort(@ip4)], self);
                  ip4 := 0; // NextIP4
                end;
            end;
            if ip4 = 0 then
              ip4 := NextIp4;
            if ip4 = 0 then
            begin
              // IPv4 exhausted: don't return NAK, but silently ignore
              // - client will retry automatically after a small temporisation
              fLog.Add.Log(sllWarning,
                'ProcessUdpFrame: IPv4 exhausted for Discover %', [macx], self);
              exit;
            end;
            if p = nil then
            begin
              p := NewLease;
              p^.Mac := mac;
            end;
            p^.IP4 := ip4;
          end
          else
            // keep existing/previous IP4
            ip4 := p^.IP4;
          // respond with an OFFER
          p^.State := lsReserved;
          p^.Timeout := utc + fOfferHolding;
          dmt := dmtOffer;
        end;
      dmtRequest:
        begin
          // especially for PXE networks, it is safe and common to ignore
          // Option 50 and ciaddr in REQUEST and just ACK the already OFFERed IP
          p := FindMac(mac64);
          if (p = nil) or
             (p^.IP4 = 0) or
             not ((p^.State in [lsReserved, lsAck]) or
                  ((p^.State = lsOutdated) and
                   (fGraceFactor > 1) and
                   (fLeaseTimeLE < SecsPerHour) and // grace period
                   (utc - p^.Timeout < fLeaseTimeLE * fGraceFactor))) then
          begin
            fLog.Add.Log(sllDebug,
              'ProcessUdpFrame: NAK after out-of-sync Request %', [macx], self);
            // send a NAK response anyway
            f := DhcpNew(Frame, dmtNak, Frame.xid, mac, fServerIdentifier);
            Len := f - PAnsiChar(@Frame) + 1;
            result := true;
            exit;
          end;
          // respond with an ACK on the known IP
          ip4 := p^.IP4;
          p^.State := lsAck;
          p^.Timeout := utc + fLeaseTimeLE;
          dmt := dmtAck;
        end;
      dmtDecline:
        begin
          // client ARPed the OFFERed IP and detect conflict: rejects it
          ip4 := DhcpIP4(@Frame, lens[doRequestedIp]); // authoritative IP
          if (ip4 <> 0) and
             not fSubNet.Match(ip4) then // need clean IP
            ip4 := 0;
          p := FindMac(mac64);
          if p <> nil then
          begin
            // invalidate OFFERed IP
            p^.State := lsDeclinedIP;
            if ip4 = 0 then
              ip4 := p^.IP4; // option 50 was not set
            p^.IP4 := 0;
          end;
          if ip4 <> 0 then
          begin
            // store internally this IP as declined for NextIP4
            p := NewLease;
            PInt64(@p^.Mac)^ := 0; // used as sentinel to store this IP
            p^.State := lsDeclinedIP;
            p^.IP4 := ip4;
            p^.Timeout := utc + fLeaseTimeLE; // use main lease time
          end;
          inc(fModifSequence); // trigger SaveToFile() in next OnIdle()
          result := true; // server MUST NOT respond to a DHCPDECLINE message
          exit;
        end;
    else
      exit; // paranoid
    end;
  finally
    fSafe.UnLock;
  end;
  // compute the response frame over the very same xid
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'ProcessUdpFrame: % IP=% for mac=%',
        [ToText(dmt)^, IP4ToShort(@ip4), macx], self);
  f := DhcpNew(Frame, dmt, Frame.xid, mac, fServerIdentifier);
  Frame.ciaddr := ip4;
  DhcpAddOption(f,   doSubnetMask,         @fSubnetMask);
  if fBroadcast <> 0 then
    DhcpAddOption(f, doBroadcastAddr,      @fBroadcast);
  if fGateway <> 0 then
    DhcpAddOption(f, doRouter,             @fGateway);
  DhcpAddOptions(f,  doDns,                pointer(fDnsServer));
  DhcpAddOption(f,   doLeaseTimeValue,     @fLeaseTime); // already big-endian
  DhcpAddOption(f,   doRenewalTimeValue,   @fRenewalTime);
  DhcpAddOption(f,   doRebindingTimeValue, @fRebinding);
  // TODO: IPXE host/file options
  // TODO: custom callback
  f^ := #255;
  Len := f - PAnsiChar(@Frame) + 1;
  inc(fModifSequence); // trigger SaveToFile() in next OnIdle()
  result := true;
end;


{ TDhcpServerSettings }

constructor TDhcpServerSettings.Create;
begin
  inherited Create;
  fSubnetMask := '192.168.1.1/24';
  fLeaseTimeSeconds := 120; // avoid IP exhaustion during iPXE process
  fOfferHoldingSecs := 5;
  fGraceFactor := 2;
end;




initialization
  assert(ord(dmtTls) = 18);
  assert(PtrUInt(@PDhcpPacket(nil)^.options) = 240);
  assert(SizeOf(TDhcpPacket) = 548);

end.

