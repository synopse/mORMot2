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
   doLeaseTime,             // IP Lease Duration in seconds 51 (86400 for 24h)
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

const

  /// 1,3,6,15,28 options as used for DhcpDiscover()
  DHCP_DISCOVER = [doSubnetMask, doRouter, doDns, doDomainName, doBroadcastAddr];

/// initialize a DHCP discover packet for a client
// - returns pointer to @dhcp.options for additional DhcpAddOption() fluent calls
function DhcpDiscover(var dhcp: TDhcpPacket; const addr: TNetMac;
  req: TDhcpOptions = DHCP_DISCOVER): PAnsiChar;

/// append a byte value to the TDhcpPacket.options packet
procedure DhcpAddOption(var p: PAnsiChar; op: TDhcpOption; b: byte); overload;
  {$ifdef FPC} inline; {$endif}

/// append a raw binary value to the TDhcpPacket.options packet
procedure DhcpAddOption(var p: PAnsiChar; op: TDhcpOption; b: pbyte; len: PtrUInt); overload;

type
 /// efficient DhcpParse() resultset
 // - store the length position of an option in TDhcpPacket.options[]
 // - 0 means that this TDhcpOption was not transmitted
 // - very efficient O(1) lookup of recognized options with no memory allocation
 TDhcpParsed = array[TDhcpOption] of byte;

/// parse a raw DHCP binary frame and return the length of all recognized options
// - returns dmtUndefined on invalid input DHCP frame
function DhcpParse(dhcp: PDhcpPacket; len: PtrInt;
  var lens: TDhcpParsed; found: PDhcpOptions = nil): TDhcpMessageType;

/// decode the pointer corresponding to lens[opt] within dhcp^.option[]
function DhcpData(dhcp: PDhcpPacket; len: PtrUInt): PShortString;
  {$ifdef HASINLINE} inline; {$endif}

/// decode the 32-bit IP address corresponding to lens[opt] within dhcp^.option[]
function DhcpIP4(dhcp: PDhcpPacket; len: PtrUInt): TNetIP4;
  {$ifdef HASINLINE} inline; {$endif}

/// decode the 32-bit big endian corresponding to lens[opt] within dhcp^.option[]
function DhcpInt(dhcp: PDhcpPacket; len: PtrUInt): cardinal;

/// decode the MAC address corresponding to lens[opt] within dhcp^.option[]
function DhcpMac(dhcp: PDhcpPacket; len: PtrUInt): PNetMac;

/// decode the lens[doParameterRequestList] within dhcp^.option[]
function DhcpRequestList(dhcp: PDhcpPacket; const lens: TDhcpParsed): TDhcpOptions;

const
  BOOT_REQUEST = 1;
  BOOT_REPLY   = 2;

type
  TLease = record
    Mac: TNetMac;
    IP4: TNetIP4;
  end;


{ **************** High-Level DHCP Server }

type
  /// main high-level options for a minimal DHCP Server
  TDhcpServerSettings = class(TSynPersistent)
  protected
    fSubnetMask: RawUtf8;
    fRangeMin: RawUtf8;
    fRangeMax: RawUtf8;
    fDefaultGateway: RawUtf8;
    fDnsServers: RawUtf8;
    fDomainName: RawUtf8;
    fBroadCastAddress: RawUtf8;
    fLeaseTimeSeconds: cardinal;
    fServerIdentifier: RawUtf8;
  public
    /// setup this instance with default values
    constructor Create; override;
  published
    /// Subnet Mask e.g. '255.255.255.0'
    property SubnetMask: RawUtf8
      read fSubnetMask write fSubnetMask;
    /// minimal IP range e.g. '192.168.1.10'
    property RangeMin: RawUtf8
      read fRangeMin write fRangeMin;
    /// maximal IP range e.g. '192.168.1.254'
    property RangeMax: RawUtf8
      read fRangeMax write fRangeMax;
    /// Default Gateway e.g. '192.168.1.1'
    property DefaultGateway: RawUtf8
      read fDefaultGateway write fDefaultGateway;
    /// DNS Servers 6 e.g. '8.8.8.8,8.8.4.4'
    property DnsServers: RawUtf8
      read fDnsServers write fDnsServers;
    /// Domain Name  e.g. 'lan.local'
    property DomainName: RawUtf8
      read fDomainName write fDomainName;
    /// Broadcast Address e.g. '192.168.1.255'
    property BroadCastAddress: RawUtf8
      read fBroadCastAddress write fBroadCastAddress;
    /// IP Lease Duration in seconds e.g. 86400 for 24h)
    property LeaseTimeSeconds: cardinal
      read fLeaseTimeSeconds write fLeaseTimeSeconds;
    /// DHCP Server Identifier e.g. '192.168.1.1'
    property ServerIdentifier: RawUtf8
      read fServerIdentifier write fServerIdentifier;
  end;


implementation


{ **************** Low-Level DHCP Protocol Definitions }

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

function DhcpNew(var dhcp: TDhcpPacket; dmt: TDhcpMessageType; xid: cardinal;
  const addr: TNetMac): PAnsiChar;
begin
  FillCharFast(dhcp, SizeOf(dhcp) - SizeOf(dhcp.options), 0);
  dhcp.op := DHCP_BOOT[dmt];
  dhcp.htype := ARPHRD_ETHER;
  dhcp.hlen := SizeOf(addr);
  dhcp.xid := xid;
  dhcp.flags := $8000; // not unicast in this userland UDP socket API unit
  PNetMac(@dhcp.chaddr)^ := addr;
  dhcp.cookie := DHCP_MAGIC_COOKIE;
  result := @dhcp.options;
  DhcpAddOption(result, doMessageType, ord(dmt));
end;

var
  DhcpDiscoverId: integer;

function DhcpDiscover(var dhcp: TDhcpPacket; const addr: TNetMac;
  req: TDhcpOptions): PAnsiChar;
begin
  if DhcpDiscoverId = 0 then
    DhcpDiscoverId := Random32Not0;
  result := DhcpNew(dhcp, dmtDiscover, InterlockedIncrement(DhcpDiscoverId), addr);
  result := DhcpAddOptionRequestList(result, req);
  result^ := #255; // only for internal/debug use
end;

function DhcpParse(dhcp: PDhcpPacket; len: PtrInt;
  var lens: TDhcpParsed; found: PDhcpOptions): TDhcpMessageType;
var
  p: PAnsiChar;
  opt: PtrInt; // TDhcpOption
  dmt: byte;   // TDhcpMessageType
begin
  result := dmtUndefined;
  if found <> nil then
    found^ := [];
  FillCharFast(lens, SizeOf(lens), 0);
  dec(len, PtrInt(PtrUInt(@PDhcpPacket(nil)^.options)));
  if (len <= 0) or
     (dhcp = nil) or
     (dhcp^.cookie <> DHCP_MAGIC_COOKIE) or
     (dhcp^.htype <> ARPHRD_ETHER) or
     (dhcp^.hlen <> SizeOf(TNetMac)) or
     (dhcp^.xid = 0) or
     not (dhcp^.op in [BOOT_REQUEST, BOOT_REPLY]) then
    exit;
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
  dmt := lens[doMessageType];
  if (dmt = 0) or                   // not set
     (dhcp^.options[dmt] <> 1) then // length
    exit;
  dmt := dhcp^.options[dmt + 1];    // value
  if (dmt > byte(high(TDhcpMessageType))) or
     (DHCP_BOOT[TDhcpMessageType(dmt)] <> dhcp^.op) then // inconsistent type
    exit;
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
  result := PtrUInt(len);
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
  result := pointer(len);
  if len = 0 then
    exit;
  inc(len, PtrUInt(@dhcp.options));
  case PByte(len)^ of
    SizeOf(TNetMac):
      result := pointer(len + 1);
    SizeOf(TNetMac) + 1:
      if PByte(len + 1)^ = ARPHRD_ETHER then // e.g. client identifier
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

{ TDhcpServerSettings }

constructor TDhcpServerSettings.Create;
begin
  inherited Create;
  fSubnetMask := '255.255.255.0';
  fRangeMin := '192.168.1.10';
  fRangeMax := '192.168.1.254';
  fDefaultGateway := '192.168.1.1';
  fDnsServers := '192.168.1.1';
  fDomainName := 'lan.local';
  fBroadCastAddress := '192.168.1.255';
  fLeaseTimeSeconds := 86400; // for 24h
  ServerIdentifier := '192.168.1.1';
end;




initialization
  assert(ord(dmtTls) = 18);
  assert(PtrUInt(@PDhcpPacket(nil)^.options) = 240);
  assert(SizeOf(TDhcpPacket) = 548);

end.

