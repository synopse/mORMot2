/// Simple DHCP Protocol Support
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.dhcp;

{
  *****************************************************************************

   Simple Dynamic Host Configuration Protocol (DHCP) Protocol Support
    - Low-Level DHCP Protocol Definitions
    - Low-Level per-scope DHCP metrics
    - Middle-Level DHCP Scope and Lease Logic
    - High-Level Multi-Scope DHCP Server Processing Logic

   Implement DISCOVER, OFFER, REQUEST, DECLINE, ACK, NAK, RELEASE, INFORM.
   Background lease persistence using dnsmasq-compatible text files.
   Static IP reservation using MAC address or UUID Identifiers Option 61.
   Support VLAN via SubNets / Scopes, giaddr and Relay Agent Option 82.
   Convenient JSON profiles for vendor-specific or user-specific options.
   Scale up to 100k+ leases per subnet with minimal RAM/CPU consumption.
   No memory allocation is performed during the response computation.
   Prevent most client abuse with configurable rate limiting.
   Cross-Platform on Windows, Linux and MacOS, running in a single thread/core.
   Meaningful and customizable logging of the actual process (e.g. into syslog).
   Generate detailed JSON and CSV metrics as local files, global and per scope.
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
    /// server/next-server IP address
    siaddr: TNetIP4;
    /// gateway IP address
    giaddr: TNetIP4;
    /// client MAC address - only chaddr[0..5] for htype=1 and hlen=6
    chaddr: array[0..15] of byte;
    /// server-hostname
    sname:  array[0..63] of byte;
    /// boot-file-name
    bootfile:  array[0..127] of byte;
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
 // - doVendorEncapsulatedOptions is option 43
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
 // - doRelayAgentInformation is Relay Agent (RFC 3046) option 82 - appear last
 // - doClientArchitecture is RFC 4578 PXE option 93 as uint16 (00:00)
 // - doUuidClientIdentifier is RFC 4578 PXE option 97 ($00 + SMBIOS_UUID)
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
   doVendorEncapsulatedOptions,
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
   doRelayAgentInformation,
   doClientArchitecture,
   doUuidClientIdentifier,
   doSubnetSelection,
   doEnd
 );
 /// set of supported DHCP options
 TDhcpOptions = set of TDhcpOption;
 /// pointer to a set of supported DHCP options
 PDhcpOptions = ^TDhcpOptions;

 /// decoded DHCP Option 82 relay-agent-information sub-options - see RFC 3046
 // - fixed as 8 items for TDhcpParsedRai to fit in 64-bit
 TDhcpOptionRai = (
   dorUndefined,
   dorCircuitId,
   dorRemoteId,
   dorLinkSelection,
   dorSubscriberId,
   dorServerIdOverride,
   dorRelayId,
   dorRelayPort);
 /// set of supported DHCP relay-agent-information sub-options
 TDhcpOptionRais = set of TDhcpOptionRai;

var
  /// uppercase identifier of each DHCP message type, e.g. 'DISCOVER' or 'ACK'
  DHCP_TXT: array[TDhcpMessageType] of RawUtf8;

  /// KEA-like identifier of each DHCP option
  // - https://kea.readthedocs.io/en/kea-3.1.4/arm/dhcp4-srv.html#standard-dhcpv4-options
  // - i.e. 'subnet-mask', 'routers', 'domain-name-servers', 'host-name',
  // 'domain-name', 'broadcast-address', 'ntp-servers', 'vendor-encapsulated-options',
  // 'dhcp-requested-address', 'dhcp-lease-time', 'dhcp-message-type',
  // 'dhcp-server-identifier', 'dhcp-parameter-request-list', 'dhcp-renewal-time',
  // 'dhcp-rebinding-time', 'vendor-class-identifier', 'dhcp-client-identifier',
  // 'tftp-server-name', 'boot-file-name', 'user-class', 'relay-agent-information',
  // 'client-architecture', 'uuid-client-identifier' and 'subnet-selection'
  DHCP_OPTION: array[TDhcpOption] of RawUtf8;
  /// KEA-like identifier of each DHCP RAI sub-option
  // - i.e. 'circuit-id', 'remote-id', 'link-selection', 'subscriber-id',
  // 'server-id-override', 'relay-id' and 'relay-port'
  RAI_OPTION: array[TDhcpOptionRai] of RawUtf8;

function ToText(dmt: TDhcpMessageType): PShortString; overload;
function ToText(opt: TDhcpOption): PShortString; overload;
function ToText(opt: TDhcpOptionRai): PShortString; overload;

function FromText(const V: RawUtf8; out dmt: TDhcpMessageType): boolean; overload;
function FromText(const V: RawUtf8; out opt: TDhcpOption): boolean; overload;
function FromText(const V: RawUtf8; out opt: TDhcpOptionRai): boolean; overload;

const
  /// 1,3,6,15,28 options as used by default for DhcpClient()
  DHCP_REQUEST = [
    doSubnetMask, doRouters, doDomainNameServers, doDomainName, doBroadcastAddress];

/// initialize a DHCP client discover/request packet
// - returns pointer to @dhcp.options for additional DhcpAddOption() fluent calls
function DhcpClient(var dhcp: TDhcpPacket; dmt: TDhcpMessageType;
  const addr: TNetMac; req: TDhcpOptions = DHCP_REQUEST): PAnsiChar;

/// raw append a value to TDhcpPacket.options[] - maybe any non-TDhcpOption
procedure DhcpAddOptionRaw(var p: PAnsiChar; const op: byte; b: pointer; len: PtrUInt);
  {$ifdef HASINLINE} inline; {$endif}

/// append a 8-bit byte value to TDhcpPacket.options[]
procedure DhcpAddOptionByte(var p: PAnsiChar; const op, b: cardinal);
  {$ifdef FPC} inline; {$endif}

/// append a 32-bit big endian value (e.g. an IPv4) to TDhcpPacket.options[]
procedure DhcpAddOption32(var p: PAnsiChar; const op: TDhcpOption; const be: cardinal);
  {$ifdef FPC} inline; {$endif}

/// append a raw binary value to TDhcpPacket.options[]
procedure DhcpAddOptionBuf(var p: PAnsiChar; const op: TDhcpOption;
  b: pointer; len: PtrUInt);

/// append a raw text or binary to TDhcpPacket.options[] - do nothing if v is ''
procedure DhcpAddOptionU(var p: PAnsiChar; const op: TDhcpOption; const v: RawUtf8);
  overload; {$ifdef HASINLINE} inline; {$endif}

/// append a raw text or binary to TDhcpPacket.options[] - do nothing if v is ''
procedure DhcpAddOptionU(var p: PAnsiChar; const op: byte; const v: RawUtf8);
  overload; {$ifdef HASINLINE} inline; {$endif}

/// append a short raw text to TDhcpPacket.options[] - do nothing if v is ''
procedure DhcpAddOptionShort(var p: PAnsiChar; const op: TDhcpOption; const v: ShortString);
  {$ifdef HASINLINE} inline; {$endif}

/// append a TNetIP4s dynamic array to TDhcpPacket.options[]
// -  here ips is a <> nil pointer(dynamicarray) of TNetIP4 = cardinal
procedure DhcpAddOptions(var p: PAnsiChar; const op: TDhcpOption; ips: PAnsiChar);

/// append a copy of an existing TDhcpPacket.options option for lens[opt]
// - sourcelen should point to recv[lens[opt]] with lens[opt] <> 0
procedure DhcpCopyOption(var p: PAnsiChar; sourcelen: PAnsiChar);


type
 /// efficient DhcpParse() results as O(1) lookup of recognized options
 // - 0 means that this TDhcpOption was not transmitted
 // - or store the position of an option length in TDhcpPacket.options[]
 TDhcpParsed = array[TDhcpOption] of byte;
 /// efficient O(1) lookup of DHCP Option 82 recognized RAI sub-options
 TDhcpParsedRai = array[TDhcpOptionRai] of byte;

/// parse a raw DHCP binary frame and return the length of all recognized options
// - returns dmtUndefined on invalid input DHCP frame
function DhcpParse(dhcp: PDhcpPacket; len: PtrInt; var lens: TDhcpParsed;
  found: PDhcpOptions = nil; mac: PNetMac = nil): TDhcpMessageType;

/// locate an option by number in a DHCP packet
// - assume dhcp^ contains a packet already validated by DhcpParse()
// - returns nil if not found, or the location of op value in dhcp^.option[]
// - brute force O(n) search - DhcpParse() would allow faster O(1) lens[] lookup
function DhcpFindOption(dhcp: PDhcpPacket; op: byte): PAnsiChar;
  {$ifdef HASINLINE} inline; {$endif}

/// result the text value stored at dhcp^.option[lens[opt]] or @NULCHAR = ''
function DhcpData(dhcp: PDhcpPacket; len: PtrUInt): PShortString;
  {$ifdef HASINLINE} inline; {$endif}

/// case-insensitive compare the text value at dhcp^.option[lens[opt]]
function DhcpIdem(dhcp: PDhcpPacket; len: PtrUInt; const P2: ShortString): boolean;

/// decode the 32-bit IP address stored at dhcp^.option[lens[opt]]
function DhcpIP4(dhcp: PDhcpPacket; len: PtrUInt): TNetIP4;
  {$ifdef HASINLINE} inline; {$endif}

/// decode the 32-bit big endian integer stored at dhcp^.option[lens[opt]]
function DhcpInt(dhcp: PDhcpPacket; len: PtrUInt): cardinal;

/// decode the MAC address stored at dhcp^.option[lens[opt]]
// - no DUID decoding is supported yet: only MAC or Eth=1 + MAC values
function DhcpMac(dhcp: PDhcpPacket; len: PtrUInt): PNetMac;

/// decode the lens[doDhcpParameterRequestList] within dhcp^.option[]
function DhcpRequestList(dhcp: PDhcpPacket; const lens: TDhcpParsed): TDhcpOptions;

type
  /// decoded DHCP options for PXE network booting process and its chain load
  // - as parsed by TDhcpProcess.SetBoot() and stored in TDhcpProcessData.Boot
  // - dcbDefault means regular OS DHCP request with no remote boot options
  // - dcbBios is first-stage TFTP loader on legacy BIOS PXE
  // - dcbX86,dcbX64,dcbA32,dcbA64 are first-stage TFTP loader with UEFI
  // - dcbX64Http,dcbA64Http are first-stage UEFI firmware allowing HTTP
  // - dcbIpxeBios is iPXE second-stage on legacy BIOS
  // - dcbIpxeX86,dcbIpxeX64,dcbIpxeA32,dcbIpxeA64 are iPXE second-stage
  TDhcpClientBoot = (
    dcbDefault,
    dcbBios,
    dcbX86,
    dcbX64,
    dcbA32,
    dcbA64,
    dcbX64Http,
    dcbA64Http,
    dcbIpxeBios,
    dcbIpxeX86,
    dcbIpxeX64,
    dcbIpxeA32,
    dcbIpxeA64);

  /// internal resultset used by ParseMacIP()
  TMacIP = record
    ip: TNetIP4;
    mac: TNetMac;
    uuid: RawByteString;
  end;

  /// store one DHCP option number associated to a binary value
  // - used to match "all" "any" "not-all" "not-any" against a value
  // - used to store "always" and "requested" data to be sent back to the client
  TProfileValue = record
    /// the raw option number to match, or to be sent back
    // - contains 0 for "always" options or "boot"/RAI match
    op: byte;
    /// the known option enum corresponding to the op integer
    // - opt=doPad for an unknown option: slower manual O(n) lookup would happen
    opt: TDhcpOption;
    /// the "boot" matching value (with op=0 and opt=doPad)
    boot: TDhcpClientBoot;
    /// the DHCP Option 82 relay-agent-information sub-option matching value
    rai: TDhcpOptionRai;
    /// the associated raw binary value
    value: RawByteString;
  end;
  PProfileValue = ^TProfileValue;

  /// store one DHCP option number associated to a binary value
  // - "always" options are just stored with op = 0
  TProfileValues = array of TProfileValue;

var
  /// contains PXE boot network identifiers used for JSON/INI settings fields
  // - i.e. 'default', 'bios', 'x86', 'x64', 'a32', 'a64', 'x64-http', 'a64-http',
  // 'ipxe-bios', 'ipxe-x86', 'ipxe-x64', 'ipxe-a32', 'ipxe-a64'
  BOOT_TXT: array[TDhcpClientBoot] of RawUtf8;

/// parse a 'ip', 'mac=ip' or 'uuid=ip' text into binary TNetIP4/TNetMac
// - recognize 'xx:xx:xx:xx:xx:xx' for MAC, and hexadecimal/GUID for UUID
function ParseMacIP(var nfo: TMacIP; const macip: RawUtf8): boolean;

/// raw generation of a Type-Length-Value (TLV) binary from JSON
function TlvFromJson(p: PUtf8Char; out v: RawByteString): boolean; overload;
function TlvFromJson(const json: RawUtf8): RawByteString; overload;

/// parse a CIDR route(s) text into a RFC 3442 compliant binary blob
// - expect '192.168.1.0/24,10.0.0.5,10.0.0.0/8,192.168.1.1' readable format
function CidrRoutes(p: PUtf8Char; var bin: RawByteString): boolean;


{ **************** Low-Level per-scope DHCP metrics }

type
  /// all available per-scope DHCP metrics
  // -  - Protocol messages
  // - dsmDiscover: DISCOVER packet is successfully received and processed
  // - dsmOffer: OFFER is sent in response to a DISCOVER
  // - dsmRequest: REQUEST is received and valid
  // - dsmAck: ACK is sent in response to REQUEST or INFORM
  // - dsmNak: NAK is sent in response to REQUEST with invalid or unavailable IP
  // - dsmInform: INFORM is received and processed
  // - dsmRelease: RELEASE is received and a lease is freed
  // - dsmDecline: DECLINE is received and IP is marked declined/unavailable
  // -  - Lease lifecycle
  // - dsmLeaseAllocated: new dynamic lease is allocated to a client
  // - dsmLeaseRenewed: existing lease is renewed via REQUEST
  // - dsmLeaseExpired: lease expires (OnIdle cleanup)
  // - dsmLeaseReleased: lease is freed due to RELEASE
  // -  - Static vs dynamic hits
  // - dsmStaticHits: static reservation (MAC or option 61) is used to assign an IP
  // - dsmDynamicHits: dynamic lease (from the pool) is used
  // -  Rate limiting / abuse
  // - dsmRateLimitHit: DECLINE packet is ignored due to rate limiting
  // - dsmInformRateLimitHit: INFORM packet is ignored due to rate limiting
  // - dsmInvalidRequest: received packet is malformed or cannot be processed
  // - dsmUnsupportedRequest: only DISCOVER/REQUEST/DECLINE/RELEASE/INFORM are handled
  // -  - Option usage counters
  // - dsmOption50Hits: option 50 dhcp-requested-address is present and used to setup IP
  // - dsmOption61Hits: option 61 dhcp-client-identifier is present and used to assign/lookup a lease
  // - dsmOption82Hits: option 82 relay-agent-information is present and processed
  // - dsmOption82SubnetHits: option 82 link-selection sub-option is present and processed
  // - dsmOption118Hits: option 118 subnet-selection is present and valid
  // - dsmPxeBoot: PXE/iPXE options have been returned for a given architecture
  // - - Drop / error reasons
  // - dsmDroppedPackets: incremented for any packet dropped silently (not matching a scope or giaddr)
  // - dsmDroppedNoSubnet: packet has no matching subnet for its giaddr or option 82/118
  // - dsmDroppedNoAvailableIP: the IPv4 range of a subnet is exhausted
  // - dsmDroppedInvalidIP: packet requests an IP that is already in use or invalid
  // - dsmDroppedCallback: the OnComputeResponse callback aborted this request
  // - dsmDroppedPxeBoot: there was no proper configuration for this PXE/iPXE
  TDhcpScopeMetric = (
    dsmDiscover,
    dsmOffer,
    dsmRequest,
    dsmAck,
    dsmNak,
    dsmInform,
    dsmRelease,
    dsmDecline,
    dsmLeaseAllocated,
    dsmLeaseRenewed,
    dsmLeaseExpired,
    dsmLeaseReleased,
    dsmStaticHits,
    dsmDynamicHits,
    dsmRateLimitHit,
    dsmInformRateLimitHit,
    dsmInvalidRequest,
    dsmUnsupportedRequest,
    dsmOption50Hits,
    dsmOption61Hits,
    dsmOption82Hits,
    dsmOption82SubnetHits,
    dsmOption118Hits,
    dsmPxeBoot,
    dsmDroppedPackets,
    dsmDroppedNoSubnet,
    dsmDroppedNoAvailableIP,
    dsmDroppedInvalidIP,
    dsmDroppedCallback,
    dsmDroppedPxeBoot);
  /// set of available per-scope DHCP metrics
  TDhcpScopeMetrics = set of TDhcpScopeMetric;

  /// store per-scope DHCP metrics as 64-bit unsigned monotonic counters
  TDhcpMetrics = array[TDhcpScopeMetric] of QWord;

  /// store three sets of per-scope DHCP metrics: current/last/total
  TDhcpAllMetrics = record
    /// the currently updated DHCP metrics
    // - reset by TDhcpProcess.OnIdle every TDhcpServerSettings.MetricsCsvMinutes
    Current: TDhcpMetrics;
    /// absolute sum of all DHCP metrics, since the server start
    // - excluses the Current[] pending counters
    Total: TDhcpMetrics;
  end;

var
  /// metrics fields identifier, e.g. "ack" or "static-hits"
  // - as used by MetricsToJson/MetricsFromJson() JSON object serialization
  METRIC_TXT: array[TDhcpScopeMetric] of RawUtf8;

/// reset all dst[] values to 0
procedure FillZero(var dst: TDhcpMetrics); overload;
  {$ifdef HASINLINE} inline; {$endif}

/// efficiently compute inc(dst[], src[0]]) of TDhcpMetrics values
procedure AddMetrics(var dst, src: TDhcpMetrics);
  {$ifdef HASINLINE} inline; {$endif}

/// check if all TDhcpMetrics values equals actually 0
function IsZero(const m: TDhcpMetrics): boolean; overload;
  {$ifdef HASINLINE} inline; {$endif}

/// compare two sets of TDhcpMetrics values
function IsEqual(const A, B: TDhcpMetrics): boolean; overload;
  {$ifdef HASINLINE} inline; {$endif}

/// persist all DHCP metrics values as a human-readable JSON object
// - set opt=[woDontStoreVoid] if you prefer the smallest possible JSON
function MetricsToJson(const m: TDhcpMetrics;
  opt: TTextWriterWriteObjectOptions = [woHumanReadable]): RawUtf8; overload;

/// unserialize all DHCP metrics from a MetricsToJson() JSON object
function MetricsFromJson(const json: RawUtf8; var m: TDhcpMetrics): boolean;

/// persist all DHCP metrics values as a CSV raw, potentially with header
function MetricsToCsv(const metrics: TDhcpMetrics; withheader: boolean;
  now: PShortString): RawUtf8;


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

  /// define the PXE network boot 43/66/67/174 options for a given scope/subnet
  // - used for TDhcpProcessData.Boot: TDhcpClientBoot <> dcbDefault
  // - Remote[] are consolidated for proper fallback between boot options,
  // unless dsoPxeNoInherit/"pxe-no-inherit" is set for the scope
  TDhcpScopeBoot = record
    /// IP address or hostname sent back as doTftpServerName option 66
    NextServer: RawUtf8;
    /// remote resource identifier sent back as doBootFileName option 67
    // - could be a TFTP file name, or a HTTP URI, depending on the context
    // - depending on the firmware level, e.g. 'undionly.kpxe' for dcbBios,
    // 'bootx64.efi' for dcbX64, 'http://server/bootx64.efi' for dcbX64Http,
    // or 'http://server/script' for dcbIpxeBios .. dcbIpxeA64
    Remote: array[dcbBios .. high(TDhcpClientBoot)] of RawUtf8;
  end;

  /// store one DHCP "profile" entry in a ready-to-be-processed way
  TDhcpScopeProfile = record
    /// optional identifier used in the logs (not in the internal logic itself)
    name: RawUtf8;
    /// store AND matching fields
    all: TProfileValues;
    /// store OR matching fields
    any: TProfileValues;
    /// store NOT AND matching fields
    notall: TProfileValues;
    /// store NOT OR matching fields
    notany: TProfileValues;
    /// the "always" and "requested" data to be sent back to the client
    // - "always" should be send[0] (if any, and with op=0)
    send: TProfileValues;
    /// the set of options part of "always"
    always: TDhcpOptions;
  end;
  PDhcpScopeProfile = ^TDhcpScopeProfile;
  /// store all DHCP "profile" ready-to-be-processed entries of a given scope
  TDhcpScopeProfiles = array of TDhcpScopeProfile;

  /// how to refine DHCP server process for one TDhcpScopeSettings
  // - dsoInformRateLimit will track INFORM per MAC and limit to 3 per second
  // (not included by default since seems overkill and KEA/Windows don't do it)
  // - dsoCsvUnixTime will use Unix timestamp instead of ISO-8601 text in CSVs
  // - dsoPxeNoInherit won't auto-fill the PXE boot-file-name from others
  // - dsoPxeDisable disables the whole PXE configuration from "Boot" settings
  TDhcpScopeOption = (
    dsoInformRateLimit,
    dsoCsvUnixTime,
    dsoPxeNoInherit,
    dsoPxeDisable
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
    /// the Domain Name e.g. 'lan.local' for doDomainName option 15
    DomainName: RawUtf8;
    /// the DNS IPs of this scope for doDomainNameServers option 6
    DnsServer: TIntegerDynArray;
    /// the IP of the associated NTP servers, for doNtpServers option 42
    NtpServers: TIntegerDynArray;
    /// list of static IPs of this scope
    // - sorted as 32-bit for efficient O(log(n)) branchless binary search
    // - also contain the StaticMac[].IP4 values for fast lookup
    StaticIP: TIntegerDynArray;
    /// list of MAC addresses with their static IP for this scope
    // - will be checked against plain chaddr or "01+MAC" option 61 values
    StaticMac: TLeaseDynArray;
    /// list of binary UUID with their static IP for this scope for option 61
    // - will be checked against not-"01+MAC" option 61 values - mainly UUID
    // - stored as RawByteString = doDhcpClientIdentifier-binary + 4-bytes-IP
    StaticUuid: TRawByteStringDynArray;
    /// store all DHCP "profiles" ready-to-be-processed entries for this scope
    Profiles: TDhcpScopeProfiles;
    /// readjust all internal values according to to Subnet policy
    // - raise an EDhcp exception if the parameters are not correct
    procedure AfterFill(log: TSynLog);
    /// register another static IP address to the internal pool
    // - can be associated with a fixed MAC address or option 61 UUID
    function AddStatic(var nfo: TMacIP): boolean; overload;
    /// register another static IP address to the internal pool
    // - value is expected to be supplied as 'ip', 'mac=ip' or 'uuid=ip' text
    function AddStatic(const macip: RawUtf8): boolean; overload;
    /// remove one static IP address which was registered by AddStatic()
    function RemoveStatic(ip4: TNetIP4): boolean;
    /// efficient L1-cache friendly O(n) search of a MAC address in Entry[]
    // - will also search in StaticMac[] entries
    function FindMac(mac: Int64): PDhcpLease;
    /// efficient L1-cache friendly O(n) search of an IPv4 address in Entry[]
    // - won't search in StaticIP[] and StaticMac[]
    function FindIp4(ip4: TNetIP4): PDhcpLease;
      {$ifdef FPC} inline; {$endif}
    /// retrieve a new lease in Entry[] - maybe recycled from ReuseIp4()
    function NewLease(mac64: Int64): PDhcpLease;
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
    /// check all TDhcpLease.Expired against tix32
    // - return the number of leases marked as lsOutdated
    // - this method is thread safe and will call Safe.Lock/UnLock
    function CheckOutdated(tix32: cardinal): integer;
    /// flush the internal Entry[] lease lists but keep subnet/scope definition
    procedure ClearLeases;
    /// reset back those per-scope Metrics[] to their initial 0 counter value
    procedure ResetMetrics;
    /// persist all leases with "<expiry> <MAC> <IP>" dnsmasq file pattern
    // - this method is thread safe and will call Safe.Lock/UnLock
    // - returns the number of entries/lines added to the text file
    // - localcopy=true would make a transient copy of Entry[] to reduce the lock
    function TextWrite(W: TTextWriter; tix32: cardinal; time: TUnixTime;
      localcopy: boolean): integer;
  private
    Options: TDhcpScopeOptions;
    MetricsCsvFileMonth: byte;
    // little-endian IP range values for NextIP
    LastIpLE, IpMinLE, IpMaxLE: TNetIP4;
    // match scope settings values in efficient actionable format
    LeaseTime, LeaseTimeLE, RenewalTime, Rebinding, OfferHolding: cardinal;
    MaxDeclinePerSec, DeclineTime, GraceFactor: cardinal;
    // some internal values for fast lookup and efficient Entry[] process
    LastDiscover, FreeListCount: integer;
    FreeList: TIntegerDynArray;
  public
    /// the PXE network boot settings for this scope/subnet
    Boot: TDhcpScopeBoot;
    /// where total counters are written e.g. '.../192-168-1-0_24.json'
    MetricsJsonFileName: TFileName;
    // where periodic values are appended e.g. '.../202602_192-168-1-0_24.csv'
    MetricsCsvFileName: TFileName;
    /// the 64-bit monotonic counters tracking this subnet/scope activity
    Metrics: TDhcpAllMetrics;
    /// the subnet/scope file-compatible name, e.g. '192-168-1-0_24'
    MetricsFileSubnet: TShort23;
  end;

  /// access to one scope/subnet definition
  PDhcpScope = ^TDhcpScope;

  /// store all scope/subnet definitions in a dynamic array
  TDhcpScopes = array of TDhcpScope;

function ToText(st: TLeaseState): PShortString; overload;


{ **************** High-Level Multi-Scope DHCP Server Processing Logic }

type
  TDhcpProcess = class;

  /// define the PXE network boot 66/67 options for a given scope/subnet
  // - used to fill TDhcpScopeBoot low-level data tructure
  TDhcpBootSettings = class(TSynPersistent)
  protected
    fNextServer: RawUtf8;
    fRemote: array[TDhcpClientBoot] of RawUtf8;
    { FPC is not able to generate RTTI for the following - Delphi has no issue
      TODO: bugreport?
        fRemote: array[dcbBios .. high(TDhcpClientBoot)] of RawUtf8; }
  public
    /// compute the low-level TDhcpScope.Boot data structure for current settings
    // - will also complete/consolidate configuration from sibling values
    procedure PrepareScope(var Data: TDhcpScopeBoot; Options: TDhcpScopeOptions);
  published
    /// option 66 IP address or hostname of the associated TFTP server
    property NextServer: RawUtf8
      read fNextServer write fNextServer;
    /// option 67 TFTP file name for legacy BIOS PXE
    property Bios: RawUtf8
      read fRemote[dcbBios] write fRemote[dcbBios];
    /// option 67 TFTP file name for UEFI i386
    property X86: RawUtf8
      read fRemote[dcbX86] write fRemote[dcbX86];
    /// option 67 TFTP file name for UEFI x64
    property X64: RawUtf8
      read fRemote[dcbX64] write fRemote[dcbX64];
    /// option 67 TFTP file name for UEFI Arm32
    property A32: RawUtf8
      read fRemote[dcbA32] write fRemote[dcbA32];
    /// option 67 TFTP file name for UEFI Arm64
    property A64: RawUtf8
      read fRemote[dcbA64] write fRemote[dcbA64];
    /// option 67 remote HTTP URI for UEFI x64
    property X64Http: RawUtf8
      read fRemote[dcbX64Http] write fRemote[dcbX64Http];
    /// option 67 remote HTTP URI for UEFI Arm64
    property A64Http: RawUtf8
      read fRemote[dcbA64Http] write fRemote[dcbA64Http];
    /// option 67 TFTP file name or HTTP URI for legacy BIOS iPXE
    property IpxeBios: RawUtf8
      read fRemote[dcbIpxeBios] write fRemote[dcbIpxeBios];
    /// option 67 TFTP file name or HTTP URI for iPXE i386
    property IpxeX86: RawUtf8
      read fRemote[dcbIpxeX86] write fRemote[dcbIpxeX86];
    /// option 67 TFTP file name or HTTP URI for iPXE x64
    property IpxeX64: RawUtf8
      read fRemote[dcbIpxeX64] write fRemote[dcbIpxeX64];
    /// option 67 TFTP file name or HTTP URI for iPXE Arm32
    property IpxeA32: RawUtf8
      read fRemote[dcbIpxeA32] write fRemote[dcbIpxeA32];
    /// option 67 TFTP file name or HTTP URI for iPXE Arm64
    property IpxeA64: RawUtf8
      read fRemote[dcbIpxeA64] write fRemote[dcbIpxeA64];
  end;

  /// define a profile to "match and send" options for a given scope/subnet
  // - i.e. settings for vendor-specific or client-specific DHCP options
  // - each RawJson property contain a JSON object as key/value pairs,
  // potentially in our enhanced format e.g. with unquote keys
  // - "all" "any" "not-all" "not-any" should all match to trigger the output,
  // defining a "first precise profile wins" deterministic behavior in the order
  // in the array of "profiles"
  // - "always" and "requested" define the options sent in the response
  // - keys are a DHCP option number ("77") or DHCP_OPTION[] ("user-class"),
  // - values are "text" "IP:x.x.x.x" "MAC:xx" "HEX:xx" "BASE64:xx" "UUID:xx"
  //  "UINT8:x" "UINT16:x", "UINT32:x", "UINT64:x", "ESC:x$yy", "CIDR:xx" with
  // CSV support for IP: MAC: or CIDR: to encode arrays of values
  // - values could also be integers, booleans, or nested TLV object
  // - array values would be internally converted into CSV for processing
  // - most values would have their default type guessed as UINT16, IP or UUID
  // - "all" "any" "not-all" "not-any" also support convenient "boot" key as
  // BOOT_TXT[] values, or RAI_OPTION[] keys like "circuit-id"
  TDhcpProfileSettings = class(TSynPersistent)
  protected
    fName: RawUtf8;
    fAll, fAny, fNotAll, fNotAny, fAlways, fRequested: RawJson;
  public
    /// compute the low-level TDhcpScope.Profiles[] entry from current settings
    // - raise an EDhcp exception if the parameters are not correct
    procedure PrepareScope(var Profile: TDhcpScopeProfile);
  published
    /// human-friendly identifier, only used in the logs as " profile=<name>"
    property Name: RawUtf8
      read fName write fName;
    /// a JSON object defining AND fields lookup logic
    // - all key/value pairs must match input client options
    // - those definitions are the same:
    // $ "all": { "77": "iPXE", "93": "HEX:0007" }
    // $ "all": { 77: "iPXE", 93: "UINT16:7" }
    // $ "all": { "user-class": "iPXE", "client-architecture": 7 }
    // $ "all": { user-class: "iPXE", client-architecture: 7 }
    // $ "all": { boot: "ipxe-x64" }
    property All: RawJson
      read fAll write fAll;
    /// a JSON object defining OR fields lookup logic
    // - a single key/value pairs match is enough to trigger this logic
    // - those definitions are the same:
    // $ "any": { "97": "UUID:815be81d-3da1-46e5-b679-5c682627ece5" }
    // $ "any": { 97: "815be81d-3da1-46e5-b679-5c682627ece5" }
    // $ "any": { "uuid-client-identifier": "{815be81d-3da1-46e5-b679-5c682627ece5}" }
    // $ "any": { uuid-client-identifier: "815be81d3da146e5b6795c682627ece5" }
    property Any: RawJson
      read fAny write fAny;
    /// a JSON object defining AND NOT fields lookup logic
    // - all key/value pairs must NOT match input client options
    property NotAll: RawJson
      read fNotAll write fNotAll;
    /// a JSON object defining OR NOT fields lookup logic
    // - a single key/value pairs match is enough to NOT trigger this logic
    property NotAny: RawJson
      read fNotAny write fNotAny;
    /// a JSON object defining the output options regardless of Option 55
    // - client Option 55 won't be checked: those options will always be sent
    // - so we could define e.g. with proper Type-Length-Value (TLV) encoding:
    // $ "always": {
    // $   boot-file-name: "bootx64-special.efi",
    // $   vendor-encapsulated-options:" {
    // $     6: "IP:10.0.0.5", 9: "BASE64:Zm9vYmFy" } }
    property Always: RawJson
      read fAlways write fAlways;
    /// a JSON object defining the output options following Option 55
    // - will send only the options requested within client Option 55 list
    // - those definitions are the same:
    // $ "requested": { "42": "IP:10.0.0.5,10.0.0.6" }
    // $ "requested": { 42: "10.0.0.5,10.0.0.6" }
    // $ "requested": { 42: ["10.0.0.5", "10.0.0.6"] }
    // $ "requested": { "ntp-servers": "hex:0a000005" }
    // $ "requested": { ntp-servers: "10.0.0.5,10.0.0.6" }
    // $ "requested": { ntp-servers: ["10.0.0.5", "10.0.0.6"] }
    property Requested: RawJson
      read fRequested write fRequested;
  end;
  /// a dynamic array of "match and send" options profiles
  // - store any number of vendor-specific or client-specific DHCP options
  TDhcpProfileSettingsObjArray = array of TDhcpProfileSettings;

  /// main high-level options for defining one scope/subnet for our DHCP Server
  TDhcpScopeSettings = class(TSynAutoCreateFields)
  protected
    fSubnetMask: RawUtf8;
    fStatic: TRawUtf8DynArray;
    fRangeMin: RawUtf8;
    fRangeMax: RawUtf8;
    fDefaultGateway: RawUtf8;
    fDnsServers: RawUtf8;
    fNtpServers: RawUtf8;
    fDomainName: RawUtf8;
    fBroadCastAddress: RawUtf8;
    fLeaseTimeSeconds: cardinal;
    fMaxDeclinePerSecond: cardinal;
    fDeclineTimeSeconds: cardinal;
    fServerIdentifier: RawUtf8;
    fOfferHoldingSecs: cardinal;
    fGraceFactor: cardinal;
    fOptions: TDhcpScopeOptions;
    fBoot: TDhcpBootSettings;
    fProfiles: TDhcpProfileSettingsObjArray;
  public
    /// setup this instance with default values
    // - default are just SubnetMask = '192.168.1.1/24', LeaseTimeSeconds = 120
    // and OfferHoldingSecs = 5, consistent with a simple local iPXE network
    constructor Create; override;
    /// append at runtime a new scope/subnet "profiles" settings
    // - call without any TDhcpProfileSettings parameter to create a default one
    // - supplied one will be owned by this instance from now on
    function AddProfile(one: TDhcpProfileSettings = nil): TDhcpProfileSettings;
    /// compute the low-level TDhcpScope data structure for current settings
    // - raise an EDhcp exception if the parameters are not correct
    procedure PrepareScope(Sender: TDhcpProcess; var Data: TDhcpScope);
  published
    /// Subnet Mask (option 1) e.g. "subnet-mask": "192.168.1.1/24"
    // - the CIDR 'ip/mask' pattern will compute RangeMin/RangeMax and
    // ServerIdentifier directly from this pattern
    // - accept also plain '255.255.255.0' IP if you want to specify by hand the
    // raw value sent in DHCP headers, and fill RangeMin/RangeMax and others
    property SubnetMask: RawUtf8
      read fSubnetMask write fSubnetMask;
    /// some static IP addresses potentially with their MAC or UUID, which
    // will be reserved for those on the network
    // - supplied as "ip", "mac=ip" or "uuid=ip" string items
    // - e.g. "static": ["192.168.1.2","2f:af:9e:0f:b8:2a=192.168.1.100"]
    property Static: TRawUtf8DynArray
      read fStatic write fStatic;
    /// minimal IP range e.g. "range-min": "192.168.1.10"
    // - default is '' and will be filled by "subnet-mask" value as 10
    property RangeMin: RawUtf8
      read fRangeMin write fRangeMin;
    /// maximal IP range e.g. "range-max": 192.168.1.254"
    // - default is '' and will be filled by "subnet-mask" value as 254
    property RangeMax: RawUtf8
      read fRangeMax write fRangeMax;
    /// Default Gateway (option 3) e.g. "default-gateway":"192.168.1.1"
    property DefaultGateway: RawUtf8
      read fDefaultGateway write fDefaultGateway;
    /// DNS Servers as CSV (option 6) e.g. "dns-servers":"8.8.8.8,8.8.4.4"
    property DnsServers: RawUtf8
      read fDnsServers write fDnsServers;
    /// NTP servers as CSV (option 42) e.g. 'ntp-servers:":"192.168.1.1"
    property NtpServers: RawUtf8
      read fNtpServers write fNtpServers;
    /// Domain Name (option 15) e.g. "domain-name": "lan.local"
    property DomainName: RawUtf8
      read fDomainName write fDomainName;
    /// Broadcast Address (option 28) e.g. "broadcast-address":"192.168.1.255"
    property BroadCastAddress: RawUtf8
      read fBroadCastAddress write fBroadCastAddress;
    /// IP Lease Duration in seconds (option 51) e.g. "lease-time-seconds":120
    // - default is 120 for 2 minutes
    // - options 51/58/59 Lease/Renewal/Rebinding will use 100/50/87.5 percents
    // - default 120 secs seems fine for our minimal iPXE-oriented DHCP server
    property LeaseTimeSeconds: cardinal
      read fLeaseTimeSeconds write fLeaseTimeSeconds;
    /// how many DECLINE requests are allowed per second before ignoring them
    // - e.g. "max-decline-per-second":5
    // - a malicious client can poison the pool by sending repeated DECLINEs
    // - default is 5 which seems reasonable and conservative
    // - note that INFORM rate limitation is hardcoded to 3 per second per MAC
    property MaxDeclinePerSecond: cardinal
      read fMaxDeclinePerSecond write fMaxDeclinePerSecond default 5;
    /// IP Decline Duration in seconds e.g. "decline-time-seconds":240
    // - default to 0, to reuse the same value than LeaseTimeSeconds
    // - if LeaseTimeSeconds is small, you could set a bigger value here to be
    // more conservative about the static IP persistence in the network
    property DeclineTimeSeconds: cardinal
      read fDeclineTimeSeconds write fDeclineTimeSeconds;
    /// DHCP Server Identifier e.g. "server-identifier":"192.168.1.1"
    // - default is '' and will be filled by "subnet-mask" IP value
    property ServerIdentifier: RawUtf8
      read fServerIdentifier write fServerIdentifier;
    /// how many seconds a DHCP dmtOffer would stay available
    // - default is "offer-holding-secs":5
    property OfferHoldingSecs: cardinal
      read fOfferHoldingSecs write fOfferHoldingSecs;
    /// if lease time is < 1 hour, allow relaxed lease expansion after expiry
    // - default is "grace-factor":2, meaning that for PXE with LeaseTimeSeconds
    // of 120 (< 1 hour), a grace period of 240 seconds
    // - this grace delay is disabled if GraceFactor = 0 or for long leases > 1h
    property GraceFactor: cardinal
      read fGraceFactor write fGraceFactor default 2;
    /// refine DHCP server process for this scope
    // - default is [] but you may tune it for your actual (sub-)network needs
    // using informRateLimit, csvUnixTime, pxeNoInherit, pxeDisable in "options"
    property Options: TDhcpScopeOptions
      read fOptions write fOptions;
    /// optional PXE network book settings as "boot" sub-object
    property Boot: TDhcpBootSettings
      read fBoot;
    /// vendor-specific or client-specific DHCP options for this scope as
    // "profiles" array of JSON objects
    // - order in this array defines "first profile wins" deterministic behavior
    property Profiles: TDhcpProfileSettingsObjArray
      read fProfiles;
  end;
  /// a dynamic array of DHCP Server scope/subnet settings
  TDhcpScopeSettingsObjArray = array of TDhcpScopeSettings;

  /// how to refine DHCP server process globally for all scopes
  // - dsoSystemLog would call JournalSend() in addition to default TSynLog -
  // but it will slowdown the process a lot, so should be used with caution
  TDhcpServerOption = (
    dsoSystemLog);
  /// refine DHCP server process for all scopes
  TDhcpServerOptions = set of TDhcpServerOption;

  /// main high-level options for defining our DHCP Server process
  TDhcpServerSettings = class(TSynAutoCreateFields)
  protected
    fFileName: TFileName;
    fMetricsFolder: TFileName;
    fFileFlushSeconds: cardinal;
    fMetricsCsvMinutes: cardinal;
    fOptions: TDhcpServerOptions;
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
    function AddScope(one: TDhcpScopeSettings = nil): TDhcpScopeSettings;
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
    /// if set, OnIdle will persist the internal metrics on disk in this folder
    // - will persist a MetricsFolder + 'metrics.json' with the current total
    // - and MetricsFolder + '192-168-1-0_24.json' with the per-scope total
    // - those .json files will be written/updated at FileFlushSeconds pace
    // - and MetricsFolder + '202602-192-168-1-0_24.csv' for a monthly CSV
    // aggregation file of the metrics, written at MetricsCsvMinutes pace
    property MetricsFolder: TFileName
      read fMetricsFolder write fMetricsFolder;
    /// at which pace the CSV metrics should be written in MetricsFolder by OnIdle
    // - default is every 5 minutes, which is convenient and not storage
    property MetricsCsvMinutes: cardinal
      read fMetricsCsvMinutes write fMetricsCsvMinutes default 5;
    /// customize server-level options, when Scope[].Options are not enough
    property Options: TDhcpServerOptions
      read fOptions write fOptions;
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
    /// the GetTickSec current 32-bit value
    Tix32: cardinal;
    /// the network subnet information related to this request
    // - Scope^.Safe.Lock has been done by TOnComputeResponse callback caller
    Scope: PDhcpScope;
    /// points to the raw value of doHostName option 12 in Recv.option[]
    // - points to @NULCHAR so HostName^='' if there is no such option
    RecvHostName: PShortString;
    /// the "profile" entry matched by this request
    RecvProfile: PDhcpScopeProfile;
    /// length of the Recv UDP frame received from the client
    RecvLen: integer;
    /// the server IP socket which received the UDP frame
    // - allow several UDP bound server sockets to share a single TDhcpProcess
    RecvIp4: TNetIP4;
    /// parsed options length position in Recv.option[]
    RecvLens: TDhcpParsed;
    /// option-82 RAI parsed sub-options length position in Recv.option[]
    RecvLensRai: TDhcpParsedRai;
    /// the DHCP message type parsed from Recv
    RecvType: TDhcpMessageType;
    /// the PXE remote boot type as parsed by TDhcpProcess.SetBoot
    RecvBoot: TDhcpClientBoot;
    /// the DHCP message type prepared into the Send buffer
    SendType: TDhcpMessageType;
    /// options set into the Send buffer by TDhcpProcess.FinalizeFrame
    SendOptions: TDhcpOptions;
    /// UDP frame received from the client, parsed in RecvLens[]
    Recv: TDhcpPacket;
    /// UDP frame to be sent back to the client, after processing
    Send: TDhcpPacket;
    /// some pointer value set by the UDP server for its internal process
    Opaque: pointer;
    /// IP address allocated for Send (raw Ip4) as human readable text
    Ip: TShort16;
    /// contains the client MAC address (raw Mac64) as human readable text
    // - ready for logging, with no memory allocation during the process
    // - may also contain hexadecimal UUID of static Option 61 client-identifier
    Mac: string[63];
    /// some temporary storage for a StaticUuid[] fake DHCP lease
    Temp: TDhcpLease;
  end;
  PDhcpProcessData = ^TDhcpProcessData;

  /// optional callback signature for TDhcpProcess.ComputeResponse
  // - input frame is parsed in Data.Recv/RecvLen/RecvLens/RecvLensRai
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
    fFileFlushSeconds, fMetricsCsvSeconds: cardinal;
    fIdleTix, fFileFlushTix, fMetricsCsvTix, fModifSequence, fModifSaved: cardinal;
    fLog: TSynLogClass;
    fFileName, fMetricsFolder, fMetricsJson: TFileName;
    fOnComputeResponse: TOnComputeResponse;
    fOptions: TDhcpServerOptions;
    fState: (sNone, sSetup, sSetupFailed, sShutdown);
    fMetricsDroppedPackets, fMetricsInvalidRequest: QWord; // no scope counters
    function GetCount: integer;
    procedure SetFileName(const name: TFileName);
    procedure SetMetricsFolder(const folder: TFileName);
    // some methods defined as virtual to all proper customization
    procedure DoLog(Level: TSynLogLevel; const Context: ShortString;
      const Data: TDhcpProcessData); virtual;
    function ParseFrame(var Data: TDhcpProcessData): boolean; virtual;
    function FindScope(var data: TDhcpProcessData): boolean; virtual;
    function FindLease(var data: TDhcpProcessData): PDhcpLease; virtual;
    function RetrieveFrameIP(var Data: TDhcpProcessData;
      Lease: PDhcpLease): boolean; virtual;
    procedure SetBoot(var Data: TDhcpProcessData); virtual;
    procedure AddBootOptions(var Data: TDhcpProcessData); virtual;
    procedure SetProfile(var Data: TDhcpProcessData); virtual;
    function CallbackAborted(var Data: TDhcpProcessData): boolean; virtual;
    function FinalizeFrame(var Data: TDhcpProcessData): PtrInt; virtual;
  public
    /// setup this DHCP process using the specified settings
    // - raise an EDhcp exception if the parameters are not correct - see
    // TDhcpServerSettings.Verify() if you want to intercept such errors
    // - if aSettings = nil, will use TDhcpServerSettings default parameters
    procedure Setup(aSettings: TDhcpServerSettings = nil);
    /// register another static 'ip', 'mac=ip' or 'uuid=ip' to the pool
    // - in addition to aScopeSettings.Static array
    // - could be used at runtime depending on the network logic
    // - those static IPs are not persisted in FileName/SaveToFile
    function AddStatic(const macip: RawUtf8): boolean;
    /// remove one static IP address which was registered by AddStatic()
    function RemoveStatic(const ip: RawUtf8): boolean;
    /// flush the internal lease lists and all Scope[] definitions
    procedure Clear;
    /// flush the internal lease lists but keep Scope[] definitions
    procedure ClearLeases(keepWriteLock: boolean = false);
    /// return the Scope[] entry matching its subnet mask
    // - can be used e.g. to check if IP address match the DHCP server subnets
    // - this method is not thread-safe by design
    function GetScope(ip: TNetIP4): PDhcpScope; overload;
      {$ifdef HASINLINE} inline; {$endif}
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
    /// aggregate all per-scope Total metrics into a single counter
    procedure ConsolidateMetrics(var global: TDhcpMetrics); overload;
    /// aggregate all per-scope metrics into a single set of Current/Total counters
    procedure ConsolidateMetrics(var global: TDhcpAllMetrics); overload;
    /// reset back all per-scope metrics to their initial 0 counter value
    procedure ResetMetrics;
    /// persist the main metrics as JSON object
    // - wrapper around ConsolidateMetrics() and MetricsToJson()
    function SaveMetricsToJson: RawUtf8;
    /// persist all scopes metrics as .json files and optionally all .csv files
    procedure SaveMetricsFolder(AndCsv: boolean = false);
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
    /// customize server-level options, when Scope[].Options are not enough
    property Options: TDhcpServerOptions
      read fOptions write fOptions;
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
    /// if set, OnIdle will persist the internal metrics on disk in this folder
    // - as 'metrics.json', '192-168-1-0_24.json' and '202602-192-168-1-0_24.csv'
    // - could be called after Setup() to refine the metrics process
    property MetricsFolder: TFileName
      read fMetricsFolder write SetMetricsFolder;
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

function ToText(opt: TDhcpOptionRai): PShortString;
begin
  result := GetEnumName(TypeInfo(TDhcpOptionRai), ord(opt));
end;

function FromText(const V: RawUtf8; out dmt: TDhcpMessageType): boolean;
begin
  result := GetEnumNameValue(TypeInfo(TDhcpMessageType), V, dmt, @DHCP_TXT);
end;

function FromText(const V: RawUtf8; out opt: TDhcpOption): boolean;
begin
  result := GetEnumNameValue(TypeInfo(TDhcpOption), V, opt, @DHCP_OPTION);
end;

function FromText(const V: RawUtf8; out opt: TDhcpOptionRai): boolean;
begin
  result := GetEnumNameValue(TypeInfo(TDhcpOptionRai), V, opt, @RAI_OPTION);
end;

const
  DHCP_OPTION_NUM: array[TDhcpOption] of byte = (
    0, 1, 3, 6, 12, 15, 28, 42, 43, 50, 51, 53, 54, 55,
    58, 59, 60, 61, 66, 67, 77, 82, 93, 97, 118, 255);
  RAI_OPTION_NUM: array[TDhcpOptionRai] of byte = (
    0, 1, 2, 5, 6, 11, 12, 19);
var // for fast O(1) lookup
  DHCP_OPTION_INV: array[0 .. 118] of TDhcpOption;
  RAI_OPTION_INV:  array[0 .. 19]  of TDhcpOptionRai;

procedure DhcpAddOptionByte(var p: PAnsiChar; const op, b: cardinal);
begin
  PCardinal(p)^ := op + 1 shl 8 + cardinal(b) shl 16;
  p := @p[3];
end;

procedure DhcpAddOption32(var p: PAnsiChar; const op: TDhcpOption; const be: cardinal);
var
  d: PAnsiChar;
begin
  d := p;
  d[0] := AnsiChar(DHCP_OPTION_NUM[op]);
  d[1] := #4;
  PCardinal(d + 2)^ := be;
  p := @d[6];
end;

procedure DhcpAddOptionRaw(var p: PAnsiChar; const op: byte; b: pointer; len: PtrUInt);
var
  d: PByteArray;
begin
  d := pointer(p);
  d[0] := op;
  d[1] := len;
  MoveFast(b^, d[2], len);
  p := @d[len + 2];
end;

procedure DhcpAddOptionBuf(var p: PAnsiChar; const op: TDhcpOption; b: pointer; len: PtrUInt);
begin
  DhcpAddOptionRaw(p, DHCP_OPTION_NUM[op], b, len);
end;

procedure DhcpAddOptionU(var p: PAnsiChar; const op: TDhcpOption; const v: RawUtf8);
begin
  if pointer(v) <> nil then
    DhcpAddOptionBuf(p, op, pointer(v), PStrLen(PAnsiChar(pointer(v)) - _STRLEN)^);
end;

procedure DhcpAddOptionU(var p: PAnsiChar; const op: byte; const v: RawUtf8);
begin
  if pointer(v) <> nil then
    DhcpAddOptionRaw(p, op, pointer(v), PStrLen(PAnsiChar(pointer(v)) - _STRLEN)^);
end;

procedure DhcpAddOptionShort(var p: PAnsiChar; const op: TDhcpOption; const v: ShortString);
begin
  if v[0] <> #0 then
    DhcpAddOptionBuf(p, op, @v[1], ord(v[0]));
end;

procedure DhcpAddOptions(var p: PAnsiChar; const op: TDhcpOption; ips: PAnsiChar);
begin
  // ips is a <> nil dynamic array of TNetIP4 = cardinal
  DhcpAddOptionRaw(p, DHCP_OPTION_NUM[op], pointer(ips),
    (PDALen(ips - _DALEN)^ + _DAOFF) * 4);
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
  DhcpAddOptionByte(result, {doDhcpMessageType=} 53, ord(dmt));
  if serverid <> 0 then
  begin
    dhcp.siaddr := serverid;
    DhcpAddOption32(result, doDhcpServerIdentifier, serverid);
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
  // parse DHCP options store as Type-Length-Value (TLV)
  p := @dhcp^.options;
  repeat
    // check len to avoid buffer overflow on malformatted input
    dec(len, ord(p[1]) + 2);
    if len < 1 then
      exit;
    // fast O(1) option number lookup and lens[]/found^ filling
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

function DhcpFindOption(dhcp: PDhcpPacket; op: byte): PAnsiChar;
begin
  result := @dhcp^.options;
  repeat
    if result[0] = AnsiChar(op) then // quickly parse Type-Length-Value encoding
      exit;
    result := @result[ord(result[1]) + 2];
  until result[0] = #255;
  result := nil;
end;

function DhcpData(dhcp: PDhcpPacket; len: PtrUInt): PShortString;
begin
  if len = 0 then
    result := @NULCHAR
  else
    result := @dhcp^.options[len]; // len+data matches PShortString binary
end;

function DhcpIdem(dhcp: PDhcpPacket; len: PtrUInt; const P2: ShortString): boolean;
begin
  result := (len <> 0) and
            PropNameEquals(PShortString(@dhcp^.options[len]), @P2);
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

procedure DhcpDataCopyOption(var Data: TDhcpProcessData; op: TDhcpOption);
  {$ifdef HASINLINE} inline; {$endif}
var
  b: PtrUInt;
begin
  b := Data.RecvLens[op];
  if b = 0 then
    exit;
  include(Data.SendOptions, op);
  DhcpCopyOption(Data.SendEnd, @Data.Recv.options[b]);
end;

procedure DhcpDataAddProfileOption(var Data: TDhcpProcessData; p: PProfileValue);
  {$ifdef HASINLINE} inline; {$endif}
var
  len: PtrUInt;
begin
  if pointer(p^.value) = nil then
    exit;
  // be paranoid with "profiles": avoid buffer overflow
  len := PStrLen(PAnsiChar(pointer(p^.value)) - _STRLEN)^;
  if Data.SendEnd + len >= @Data.Send.options[high(Data.Send.options)] then
    exit;
  // actually append the value and mark it in SendOptions
  DhcpAddOptionRaw(Data.SendEnd, p^.op, pointer(p^.value), len);
  include(Data.SendOptions, p^.opt);
end;

procedure DhcpDataAddOptionOnce32(var Data: TDhcpProcessData;
  const opt: TDhcpOption; const be: cardinal); overload;
  {$ifdef HASINLINE} inline; {$endif}
begin
  if (be = 0) or
     (opt in Data.SendOptions) or
     (Data.SendEnd + SizeOf(be) >= @Data.Send.options[high(Data.Send.options)]) then
    exit;
  DhcpAddOption32(Data.SendEnd, opt, be);
  include(Data.SendOptions, opt);
end;

procedure DhcpDataAddOptionOnceA32(var Data: TDhcpProcessData;
  const opt: TDhcpOption; const v: PAnsiChar);
  {$ifdef HASINLINE} inline; {$endif}
var
  len: PtrUInt;
begin
  if (v = nil) or
     (opt in Data.SendOptions) then
    exit;
  len := (PDALen(v - _DALEN)^ + _DAOFF) shl 2; // as bytes
  if Data.SendEnd + len >= @Data.Send.options[high(Data.Send.options)] then
    exit;
  DhcpAddOptionRaw(Data.SendEnd, DHCP_OPTION_NUM[opt], v, len);
  include(Data.SendOptions, opt);
end;

procedure DhcpDataAddOptionProfile(var Data: TDhcpProcessData);
var
  p: PProfileValue;
  len, n: PtrUInt;
  requested: PByteArray;
begin
  p := pointer(Data.RecvProfile.send);
  if p = nil then
    exit;
  n := PDALen(PAnsiChar(p) - _DALEN)^ + _DAOFF;
  // append "always" - should be as send[0].op=0
  if p^.op = 0 then
  begin
    Data.SendOptions := Data.SendOptions + Data.RecvProfile.always;
    len := PStrLen(PAnsiChar(pointer(p^.value)) - _STRLEN)^;
    MoveFast(pointer(p^.value)^, Data.SendEnd^, len); // as a pre-computed blob
    inc(Data.SendEnd, len);
    inc(p);
    dec(n);
    if n = 0 then
      exit;
  end;
  // access option 55 list
  len := Data.RecvLens[doDhcpParameterRequestList];
  if len = 0 then
    exit;
  requested := @Data.Recv.options[len + 1]; // len + [1,3,6,15,51,54]
  // process "requested" options, filtering each op
  repeat
    if ByteScanIndex(@requested[1], requested[0], p^.op) >= 0 then // SSE2 asm
      DhcpDataAddProfileOption(Data, p); // as individual TLV
    inc(p);
    dec(n);
  until n = 0;
end;

function DhcpDataMatchOne(var Data: TDhcpProcessData; one: PProfileValue): boolean;
var
  len: PtrUInt;
  option, value: PAnsiChar;
begin
  result := false;
  // locate the option value in Data.Recv[]
  if one^.opt <> doPad then              // most common case
  begin
    len := Data.RecvLens[one^.opt];      // fast O(1) lookup of known option
    if len = 0 then
      exit;
    option := @Data.Recv.options[len];
  end
  else if one^.op = 0 then               // check one^.boot and one^.rai
    if one^.boot <> dcbDefault then
    begin
      result := Data.RecvBoot = one^.boot;   // fastest path using SetBoot()
      exit;
    end
    else
    begin
      if one^.rai = dorUndefined then
        exit;
      len := Data.RecvLensRai[one^.rai]; // O(1) lookup of sub-option
      if len = 0 then
        exit;
      option := @Data.Recv.options[len];
    end
  else                                   // one^.op <> 0
  begin
    option := @Data.Recv.options;        // inlined DhcpFindOption()
    repeat
      if option[0] = AnsiChar(one^.op) then
        break;
      option := @option[ord(option[1]) + 2]; // O(n) lookup of raw op number
      if option[0] = #255 then
        exit;
    until false;
    inc(option); // option[0] = len
  end;
  // compare the one^.value with the option from Data.Recv
  value := pointer(one^.value);
  len := PStrLen(value - _STRLEN)^; // we know value<>nil and len>0
  if len <> ord(option[0]) then
    exit;
  repeat
    dec(len);                         // endings are more likely to change
    if option[len] <> value[len] then // faster than CompareMem() here
      exit;
  until len = 0;
  result := true; // exact case-sensitive match
end;

function DhcpDataMatchAll(var Data: TDhcpProcessData; all: PProfileValue): boolean;
var
  n: integer;
begin // caller ensured all <> nil
  result := false;
  n := PDALen(PAnsiChar(all) - _DALEN)^ + _DAOFF;
  repeat
    if not DhcpDataMatchOne(Data, all) then
      exit; // missing one
    inc(all);
    dec(n);
  until n = 0;
  result := true;
end;

function DhcpDataMatchAny(var Data: TDhcpProcessData; any: PProfileValue): boolean;
var
  n: integer;
begin // caller ensured any <> nil
  result := true;
  n := PDALen(PAnsiChar(any) - _DALEN)^ + _DAOFF;
  repeat
    if DhcpDataMatchOne(Data, any) then
      exit; // found one
    inc(any);
    dec(n);
  until n = 0;
  result := false;
end;

type
  TParseProfile = (ppMatch, ppValue, ppTlv);

function SetProfileValue(var p: TJsonParserContext; var v: RawByteString;
  op: byte): boolean;
var
  tmp: THash128Rec;
  len: PtrInt;
  d: PAnsiChar;
  arr: TRawUtf8DynArray;
  csv: RawUtf8;
label
  uuid97;
begin
  result := false;
  len := p.ValueLen;
  if len = 0 then
    exit;
  if not p.WasString and
     (p.Value^ = '[') then
  begin
    // e.g. "ntp-servers": ["ip:1.2.3.4", "1.2.3.5"] -> "ip:1.2.3.4,1.2.3.5"
    DynArrayLoadJsonInPlace(arr, p.Value, TypeInfo(TRawUtf8DynArray));
    if arr = nil then
      exit;
    RawUtf8ArrayToCsvVar(arr, csv);
    p.Get.WasString := true;
    p.Get.Value := pointer(csv);
    p.Get.ValueLen := length(csv);
  end;
  if p.WasString then
    // handle "..." string values
    case IdemPCharArray(p.Value,
           ['IP:', 'MAC:', 'HEX:', 'BASE64:', 'UUID:', 'GUID:',
            'UINT8:', 'UINT16:', 'UINT32:', 'UINT64:', 'ESC:', 'CIDR:']) of
      0:    // ip:
        begin
          v := IP4sToBinary(ToIP4s(p.Value + 3)); // allow CSV of IPs
          result := v <> '';
        end;
      1:    // mac:
        begin
          v := MacsToBinary(ToMacs(p.Value + 4)); // allow CSV of MACs
          result := v <> '';
        end;
      2:    // hex:
        result := (len > 4) and
                  (len < (255 * 2) + 4) and
                  HexToBin(PAnsiChar(p.Value + 4), len - 4, v);
      3:    // base64:
        result := (len > 7) and
                  (len < 340 + 7) and
                  Base64ToBin(PAnsiChar(p.Value + 7), len - 7, v);
      4, 5: // uuid: guid:
        begin
          result := RawUtf8ToGuid(p.Value + 5, len - 5, tmp.guid);
          if not result then
            exit;
          if op = 97 then
            goto uuid97; // special encoding as 17-bytes with initial #0
          FastSetRawByteString(v, @tmp.guid, SizeOf(TGuid));
        end;
      6:    // uint8:
        begin
          tmp.c0 := GetCardinal(p.Value + 6);
          FastSetRawByteString(v, @tmp.c0, 1);
          result := true;
        end;
      7:    // uint16:
        begin
          tmp.c0 := bswap16(GetCardinal(p.Value + 7));
          FastSetRawByteString(v, @tmp.c0, 2);
          result := true;
        end;
      8:    // uint32: is the default, but convenient to be specified
        begin
          tmp.c0 := bswap32(GetCardinal(p.Value + 7));
          FastSetRawByteString(v, @tmp.c0, 4);
          result := true;
        end;
      9:    // uint64:
        begin
          SetQWord(p.Value + 7, tmp.L);
          tmp.L := bswap64(tmp.L);
          FastSetRawByteString(v, @tmp.L, 8);
          result := true;
        end;
      10:   // esc:
        begin
          UnescapeHex(RawUtf8(v), p.Value + 4, p.ValueLen - 4, '$');
          result := true;
        end;
      11:  // cidr:
        result := CidrRoutes(p.Value + 5, v);
    else
      begin
        // recognize configurable options
        case op of
          3 .. 11, 16, 21, 28, 32, 33, 41, 42, 44, 45, 48, 49, 54, 65,
          68 .. 76, 85, 89, 112, 136, 138:
            begin
              v := IP4sToBinary(ToIP4s(p.Value)); // allow CSV of IP4
              result := v <> '';
              if result then
                exit;
            end;
          97:
            begin
              result := RawUtf8ToGuid(p.Value, len, tmp.guid);
              if result then
              begin
uuid97:         d := FastNewRawByteString(v, 17); // specific to RFC 4578 (PXE)
                d^ := #0;
                PGuid(d + 1)^ := tmp.guid;
                exit;
              end;
            end;
        end;
        // fallback to store as plain UTF-8 text
        if len > 255 then
          exit;
        FastSetString(RawUtf8(v), p.Value, len);
        result := true;
      end;
    end
  else
    case p.Value[0] of
      '0' .. '9':
        begin
          // recognize most JSON number size from configurable options
          tmp.c0 := GetCardinal(p.Value);
          case op of
            23, 37, 46, 52, 116:
              len := 1; // uint8
            13, 22, 25, 26, 57, 93, 117:
              begin
                tmp.c0 := bswap16(tmp.c0);
                len := 2; // uint16
              end;
          else
            begin
              tmp.c0 := bswap32(tmp.c0);
              len := 4; // default uint32
            end;
          end;
          FastSetRawByteString(v, @tmp.c0, len);
          result := true;
        end;
      't', 'f':
        begin
          // true/false boolean
          tmp.b[0] := ord(p.Value[0] = 't');
          FastSetRawByteString(v, @tmp.b, 1); // store 0/1 byte
          result := true;
        end;
      '{':
        // try to parse a JSON object into a TLV value
        result := TlvFromJson(p.Value, v);
    end;
end;

function ParseProfileValue(var parser: TJsonParserContext; var v: TProfileValue;
  pp: TParseProfile): boolean;
begin
  result := false;
  v.op := 0;
  v.opt := doPad;
  v.boot := dcbDefault;
  v.rai := dorUndefined;
  v.value := '';
  if not parser.GetJsonFieldName then
    exit;
  v.op := GetCardinal(parser.Value);
  if pp <> ppTlv then // TlvFromJson() only accept numbers, not options
    if v.op <> 0 then
    begin
      if v.op <= high(DHCP_OPTION_INV) then
        v.opt := DHCP_OPTION_INV[v.op];
    end
    else if parser.ValueLen = 0 then
      exit
    else if (pp = ppMatch) and
            PropNameEquals('boot', PAnsiChar(parser.Value), parser.ValueLen) then
    begin
      result := parser.ParseNext and
                parser.ValueEnumFromConst(@DHCP_BOOT, length(DHCP_BOOT), v.boot);
      exit;
    end
    else if parser.ValueEnumFromConst(@DHCP_OPTION, length(DHCP_OPTION), v.opt) then
      v.op := DHCP_OPTION_NUM[v.opt]
    else if (pp = ppMatch) and
            parser.ValueEnumFromConst(@RAI_OPTION, length(RAI_OPTION), v.rai) then
      // e.g. 'circuit-id' from within doRelayAgentInformation sub-options
    else
      exit;
  if not parser.ParseNextAny({NormalizeBoolean=}false) then
    exit;
  if pp = ppTlv then
    result := SetProfileValue(parser, v.value, 0) // op is no option
  else
    result := SetProfileValue(parser, v.value, v.op);
  if result and
     (length(v.value) > 255) then
    result := false; // avoid TLV 8-bit length overflow
end;

procedure AddProfileValue(var a: TProfileValues; var v: TProfileValue);
var
  n: PtrInt;
begin
  n := length(a);
  SetLength(a, n + 1);
  a[n] := v;
end;

// about TLV (Type-Length-Value) encoding, see e.g.
// https://www.ibm.com/docs/en/tpmfod/7.1.1.4?topic=configuration-dhcp-option-43

function TlvFromJson(p: PUtf8Char; out v: RawByteString): boolean;
var
  one: TProfileValue;
  parser: TJsonParserContext;
begin
  result := false;
  if p = nil then
    exit;
  parser.InitParser(p);
  if not parser.ParseObject then
    exit;
  repeat
    if not ParseProfileValue(parser, one, ppTlv) then
      exit;
    Append(v, [AnsiChar(one.op), AnsiChar(length(one.value)), one.value])
  until parser.EndOfObject = '}';
  result := true;
end;

function TlvFromJson(const json: RawUtf8): RawByteString; // for debug
var
  tmp: TSynTempBuffer; // make a private local copy
begin
  tmp.Init(json);
  try
    TlvFromJson(tmp.buf, result);
  finally
    tmp.Done;
  end;
end;

function CidrRoutes(p: PUtf8Char; var bin: RawByteString): boolean;
var
  dest, router: TNetIP4;
  w: PtrUInt;
  tmp: TShort15;
begin
  bin := '';
  result := false;
  if p <> nil then
    repeat
      if not NetIsIP4(p, @dest) then
        exit;
      while not (p^ in [#0, '/', ',']) do
        inc(p);
      w := 32; // default mask
      if p^ = '/' then
      begin
        w := MinPtrUInt(32, GetCardinal(p + 1)); // clamp mask width
        repeat
          inc(p);
        until p^ in [#0, ','];
      end;
      if p^ = #0 then
        exit; // premature
      inc(p); // jump ','
      if not NetIsIP4(p, @router) then
        exit;
      tmp[0] := #1;
      tmp[1] := AnsiChar(w);                         // mask width
      AppendShortBuffer(@dest, (w + 7) shr 3, @tmp); // append only prefix
      AppendShortBuffer(@router, 4, @tmp);           // router
      Append(bin, @tmp[1], ord(tmp[0]));
      while not (p^ in [#0, ';', ',']) do
        inc(p);
      if p^ = #0 then
        break;
      inc(p); // jump ',' or ';' between routes
    until p^ = #0;
  result := true;
end;

function ParseProfile(const json: RawUtf8; out v: TProfileValues;
  pp: TParseProfile): boolean;
var
  one: TProfileValue;
  parser: TJsonParserContext;
  tmp: TSynTempBuffer; // make a private local copy
begin
  result := json = '';
  if result then
    exit;
  tmp.Init(json);
  try
    parser.InitParser(tmp.buf);
    if not parser.ParseObject then
      exit;
    repeat
      if not ParseProfileValue(parser, one, pp) then
        exit;
      AddProfileValue(v, one)
    until parser.EndOfObject = '}';
    result := true;
  finally
    tmp.Done;
  end;
end;

function ParseMacIP(var nfo: TMacIP; const macip: RawUtf8): boolean;
var
  mac, ip: RawUtf8;
  p: PUtf8Char;
  guid: TGuid;
begin
  result := false;
  FillZero(nfo.mac);
  nfo.uuid := '';
  if TrimSplit(macip, mac, ip, '=') then // 'mac=ip' or 'uuid=ip' format
    if ((length(mac) = 17) and
        (TextToMac(pointer(mac), @nfo.mac))) then
      // exact 'xx:xx:xx:xx:xx:xx' format
      p := pointer(ip)
    else
    begin
      FillZero(nfo.mac); // ensure IsZero() after partial TextToMac()
      // try '0123456789abcdef' plain hexadecimal UUID - not GUID order
      if not HexToBin(pointer(mac), length(mac), nfo.uuid) then
        // '{3F2504E0-4F89-11D3-9A0C-0305E82C3301}' or '3F2504E0-..0305E82C3301'
        if RawUtf8ToGuid(mac, guid) then
          FastSetRawByteString(nfo.uuid, @guid, SizeOf(guid))
        else
          exit; // invalid MAC or UUID
      p := pointer(ip);
    end
  else
    p := pointer(macip); // only 'ip'
  result := NetIsIP4(p, @nfo.ip); // valid IP
end;


{ **************** Low-Level per-scope DHCP metrics }

procedure FillZero(var dst: TDhcpMetrics);
begin
  FillCharFast(dst, SizeOf(dst), 0);
end;

procedure AddMetrics(var dst, src: TDhcpMetrics);
begin
  AddInt64Array(@dst, @src, length(src));
end;

function IsZero(const m: TDhcpMetrics): boolean;
begin
  result := IsZero(@m, length(m));
end;

function IsEqual(const A, B: TDhcpMetrics): boolean;
begin
  result := CompareMem(@A, @B, SizeOf(A));
end;

function MetricsToJson(const m: TDhcpMetrics; opt: TTextWriterWriteObjectOptions): RawUtf8;
begin
  JsonObjectFromRttiArray(@m, @METRIC_TXT, length(m), TypeInfo(QWord), result, opt);
end;

function MetricsFromJson(const json: RawUtf8; var m: TDhcpMetrics): boolean;
var
  tmp: TSynTempBuffer;
begin
  FillZero(m);
  result := false;
  if json = '' then
    exit;
  tmp.Init(json); // make temporary copy since input json is parsed in-place
  result := JsonObjectToRttiArray(tmp.buf,
    @m, @METRIC_TXT, length(m), TypeInfo(QWord)) <> nil;
  tmp.Done;
end;

function MetricsToCsv(const metrics: TDhcpMetrics; withheader: boolean;
  now: PShortString): RawUtf8;
var
  tmp: TTextWriterStackBuffer; // 8KB static is enough
  W: TTextWriter;
  m: TDhcpScopeMetric;
begin
  W := TTextWriter.CreateOwnedStream(tmp);
  try
    if withheader then
    begin
      if now <> nil then
        W.AddShorter('time,');
      W.AddCsvStrings(@METRIC_TXT, ord(high(METRIC_TXT)));
      W.AddDirectNewLine;
    end;
    if now <> nil then
    begin
      W.AddShort(now^);
      W.AddComma;
    end;
    m := low(m);
    repeat
      W.AddQ(metrics[m]);
      if m = high(m) then
        break;
      inc(m);
      W.AddComma;
    until false;
    W.AddDirectNewLine;
    W.SetText(result);
  finally
    W.Free;
  end;
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

function TDhcpScope.NewLease(mac64: Int64): PDhcpLease;
var
  n: PtrInt;
begin
  inc(Metrics.Current[dsmLeaseAllocated]);
  // first try if we have any lsFree entries from ReuseIp4()
  if FreeListCount <> 0 then
  begin
    dec(FreeListCount);
    result := @Entry[FreeList[FreeListCount]]; // LIFO is the fastest
    PInt64(result)^ := mac64; // also reset State+RateLimit
    if result^.State = lsFree then // paranoid
      exit;
  end;
  // we need to add a new entry - maybe with reallocation
  n := Count;
  if n = length(Entry) then
    SetLength(Entry, NextGrow(n));
  result := @Entry[n];
  PInt64(result)^ := mac64; // also reset State+RateLimit
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
  dcb: TDhcpClientBoot;
  bootlog: RawUtf8;

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
  AddStatic(IP4ToText(@ServerIdentifier));
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
  begin
    log.Log(sllInfo, 'PrepareScope: scope=% subnet=% min=% max=% server=% ' +
      'broadcast=% gateway=% static=% dns=% lease=%s renew=%s rebind=%s ' +
      'offer=%s maxdecline=% declinetime=%s grace=%',
      [Subnet.ToShort, IP4ToShort(@Subnet.mask), IP4ToShort(@IpMinLE),
       IP4ToShort(@IpMaxLE), IP4ToShort(@ServerIdentifier),
       IP4ToShort(@Broadcast), IP4ToShort(@Gateway),
       IP4sToText(TNetIP4s(StaticIP)),  IP4sToText(TNetIP4s(DnsServer)),
       LeaseTime, RenewalTime, Rebinding, OfferHolding,
       MaxDeclinePerSec, DeclineTime, GraceFactor]);
    for dcb := low(Boot.Remote) to high(Boot.Remote) do
      if Boot.Remote[dcb] <> '' then
        Append(bootlog, [' ', BOOT_TXT[dcb], '=', Boot.Remote[dcb]]);
    if bootlog <> '' then
      log.Log(sllInfo, 'PrepareScope: PXE nextserver=%%',
        [Boot.NextServer, bootlog]);
  end;
  // store internal values in the more efficient endianess for direct usage
  IpMinLE     := bswap32(IpMinLE);      // little-endian
  IpMaxLE     := bswap32(IpMaxLE);
  LeaseTimeLE := LeaseTime;
  LeaseTime   := bswap32(LeaseTime);    // big-endian
  RenewalTime := bswap32(RenewalTime);
  Rebinding   := bswap32(Rebinding);
end;

procedure TDhcpScope.ClearLeases;
begin
  Safe.Lock;
  Entry := nil;
  SetLength(Entry, PREALLOCATE_LEASES); // as in TDhcpScope.AfterFill
  Count := 0;
  FreeListCount := 0;
  Safe.UnLock;
end;

const
  MIN_UUID_BYTES = 4; // < 4 bytes is too short to reliably identify a client

function DoFindUuid(u: PPAnsiChar; bin: PCardinalArray; binlen: TStrLen): TNetIP4;
var
  n: cardinal;
begin
  result := 0;
  if (u = nil) or
     (binlen < MIN_UUID_BYTES) then
    exit;
  n := PDALen(PAnsiChar(u) - _DALEN)^ + _DAOFF; // = length(StaticUuid)
  inc(binlen, 4); // StaticUuid[] stores raw BIN+IP
  repeat
    if (PStrLen(u^ - _STRLEN)^ = binlen) and
       (PCardinal(u^)^ = bin[0]) and // efficient 32-bit check < MIN_UUID_BYTES
       CompareMemSmall(@bin[1], u^ + 4, binlen - 8) then
    begin
      result := PCardinal(u^ + binlen - 4)^; // get IP from BIN+IP layout
      exit;
    end;
    inc(u);
    dec(n);
  until n = 0;
end;

function TDhcpScope.AddStatic(var nfo: TMacIP): boolean;
var
  n: PtrInt;
  p: PDhcpLease;
begin
  result := false;
  if (@self = nil) or
     not SubNet.Match(nfo.ip) then
    exit;
  Safe.Lock;
  try
    // duplicated static MAC or UUID (check first) - but allow leases override
    if not IsZero(nfo.mac) then
      if (nfo.uuid <> '') or
         (DoFindMac(pointer(StaticMac), PInt64(@nfo.mac)^ and MAC_MASK,
            length(StaticMac)) <> nil) then
      exit;
    n := length(nfo.uuid);
    if n <> 0 then
      if (n < MIN_UUID_BYTES) or
         (DoFindUuid(pointer(StaticUuid), pointer(nfo.uuid), n) <> 0) then
        exit;
    // register IP, properly sorted, and rejecting any duplicate
    if AddSortedInteger(StaticIP, nfo.ip) < 0 then
      exit;
    // register the MAC or UUID value
    if not IsZero(nfo.mac) then
    begin
      n := length(StaticMac);
      SetLength(StaticMac, n + 1);
      p := @StaticMac[n];
      p^.Mac := nfo.mac;
      p^.IP4 := nfo.ip;
    end
    else if nfo.uuid <> '' then
    begin
      Append(nfo.uuid, @nfo.ip, 4); // stored as raw BIN+IP
      AddRawUtf8(TRawUtf8DynArray(StaticUuid), nfo.uuid);
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
            AddStatic(nfo);
end;

function TDhcpScope.RemoveStatic(ip4: TNetIP4): boolean;
var
  i, n: PtrInt;
  p: PDhcpLease;
  u: PPAnsiChar;
begin
  result := false;
  if (@self = nil) or
     not SubNet.Match(ip4) then
    exit;
  Safe.Lock;
  try
    // remove this IP from the main sorted StaticIP[] list
    i := FastFindIntegerSorted(StaticIP, ip4);
    if i < 0 then
      exit; // no previous AddStatic(ip4)
    DeleteInteger(StaticIP, i);
    result := true;
    // remove this IP from StaticMac[]
    n := length(StaticMac) - 1;
    p := pointer(StaticMac);
    for i := 0 to n do
      if p^.IP4 = ip4 then
      begin
        UnmanagedDynArrayDelete(StaticMac, n, i, SizeOf(TNetMac));
        SetLength(StaticMac, n);
        exit; // one IP should not be in StaticMac[] and StaticUuid[]
      end
      else
        inc(p);
    // remove this IP from StaticUuid[] storing raw BIN+IP
    u := pointer(StaticUuid);
    for i := 0 to length(StaticUuid) - 1 do
      if PCardinal(u^ + PStrLen(u^ - _STRLEN)^ - 4)^ = ip4 then
      begin
        DeleteRawUtf8(TRawUtf8DynArray(StaticUuid), i);
        break;
      end
      else
        inc(u);
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

function TDhcpScope.TextWrite(W: TTextWriter; tix32: cardinal; time: TUnixTime;
  localcopy: boolean): integer;
var
  local: TLeaseDynArray; // 10,000 leases would use 160KB of temporary memory
  grace: cardinal;
begin
  result := 0;
  if Count = 0 then
    exit;
  Safe.Lock;
  try
    if Count = 0 then
      exit;
    grace := LeaseTimeLE * MaxPtrUInt(GraceFactor, 2); // not too deprecated
    if (Count < 1000) or
       not localcopy then
      // small output could be done within the lock
      result := DoWrite(W, pointer(Entry), Count, tix32, grace, time, @Subnet)
    else
      // make a transient copy of all leases to keep the lock small for this subnet
      // - could eventually be done if OnIdle() made a background thread (not yet)
      local := copy(Entry, 0, Count); // allocate Count * 16 bytes
  finally
    Safe.UnLock;
  end;
  // append all text lines from the local copy (if any) - not used yet
  if local <> nil then
    result := DoWrite(W, pointer(local), length(local), tix32, grace, time, @Subnet);
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
    if result <> 0 then
      inc(Metrics.Current[dsmLeaseExpired], result);
  finally
    Safe.UnLock;
  end;
end;

procedure TDhcpScope.ResetMetrics;
begin
  Safe.Lock;
  FillCharFast(Metrics, SizeOf(Metrics), 0); // set Current[] and Total[] := 0
  Safe.UnLock;
end;


{ **************** High-Level Multi-Scope DHCP Server Processing Logic }

{ TDhcpBootSettings }

procedure ConsolidateOption(var boot: TDhcpScopeBoot; ref, dst: TDhcpClientBoot);
begin
  if boot.Remote[dst] = '' then
    boot.Remote[dst] := boot.Remote[ref];
end;

procedure TDhcpBootSettings.PrepareScope(var Data: TDhcpScopeBoot;
  Options: TDhcpScopeOptions);
var
  dcb, ref: TDhcpClientBoot;
begin
  if dsoPxeDisable in Options then
  begin
    Finalize(Data); // reset all RawUtf8/RawByteString to ''
    exit;
  end;
  // copy the working parameters from the settings
  TrimU(fNextServer, Data.NextServer);
  for dcb := low(Data.Remote) to high(Data.Remote) do
    TrimU(fRemote[dcb], Data.Remote[dcb]);
  // complete configuration from sibling values
  if dsoPxeNoInherit in Options then
    exit;
  // 1. HTTP aware architecture fallback to their TFTP value
  ConsolidateOption(Data, dcbX64, dcbX64Http);
  ConsolidateOption(Data, dcbA64, dcbA64Http);
  // 2. assume we could share the main x64/x86 IPXE URI
  ref := dcbDefault;
  if Data.Remote[dcbIpxeX64] <> '' then
    ref := dcbIpxeX64
  else if Data.Remote[dcbIpxeX86] <> '' then
    ref := dcbIpxeX86;
  if ref <> dcbDefault then
    for dcb := dcbIpxeX86 to high(Data.Remote) do
      if dcb <> ref then
        ConsolidateOption(Data, ref, dcb);
end;


{ TDhcpProfileSettings }

procedure TDhcpProfileSettings.PrepareScope(var Profile: TDhcpScopeProfile);
var
  alw, req: TProfileValues;
  p: PProfileValue;
  v: TProfileValue;
  i: PtrInt;
begin
  // parse main "profiles" JSON object fields
  Profile.name := fName;
  if not ParseProfile(fAll, Profile.all, ppMatch) then
    EDhcp.RaiseUtf8('PrepareScope: invalid all:%', [fAll]);
  if not ParseProfile(fAny, Profile.any, ppMatch) then
    EDhcp.RaiseUtf8('PrepareScope: invalid any:%', [fAny]);
  if not ParseProfile(fNotAll, Profile.notall, ppMatch) then
    EDhcp.RaiseUtf8('PrepareScope: invalid not-all:%', [fNotAll]);
  if not ParseProfile(fNotAny, Profile.notany, ppMatch) then
    EDhcp.RaiseUtf8('PrepareScope: invalid not-any:%', [fNotAny]);
  if not ParseProfile(fAlways, alw, ppValue) then
    EDhcp.RaiseUtf8('PrepareScope: invalid always:%', [fAlways]);
  if not ParseProfile(fRequested, req, ppValue) then
    EDhcp.RaiseUtf8('PrepareScope: invalid requested:%', [fRequested]);
  // Profile.send[0] is "always" and should be stored as binary blob with op=0
  Profile.send := nil;
  Profile.always := [];
  if alw <> nil then
  begin
    v.op := 0;
    v.opt := doPad;
    v.boot := dcbDefault;
    v.rai := dorUndefined;
    p := pointer(alw);
    for i := 1 to length(alw) do
    // prepare raw TLV-concatenated binary buffer
    begin
      Append(v.value, [AnsiChar(p^.op), AnsiChar(length(p^.value)), p^.value]);
      if p^.opt <> doPad then
        if p^.opt in Profile.always then
          EDhcp.RaiseUtf8('PrepareScope: duplicated % in always:%',
            [DHCP_OPTION[p^.opt], fAlways])
        else
          include(Profile.always, p^.opt);
      inc(p);
    end;
    AddProfileValue(Profile.send, v);
  end;
  // append "requested" with their op, ready to be filtered against option 55
  p := pointer(req);
  for i := 1 to length(req) do
  begin
    if (p^.opt <> doPad) and
       (p^.opt in Profile.always) then
      EDhcp.RaiseUtf8('PrepareScope: duplicated % in requested:%',
        [DHCP_OPTION[p^.opt], fRequested]);
    AddProfileValue(Profile.send, p^);
    inc(p);
  end;
end;


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

function TDhcpScopeSettings.AddProfile(one: TDhcpProfileSettings): TDhcpProfileSettings;
begin
  if one = nil then
    one := TDhcpProfileSettings.Create;
  if self <> nil then
    PtrArrayAdd(fProfiles, one); // will be owned by this instance
  result := one;
end;

procedure TDhcpScopeSettings.PrepareScope(Sender: TDhcpProcess;
  var Data: TDhcpScope);
var
  mask: TNetIP4;
  i: PtrInt;
begin
  // convert the current settings into Data.* raw values - raise EDhcp on error
  Data.Gateway           := ToIP4(fDefaultGateway);
  Data.Broadcast         := ToIP4(fBroadCastAddress);
  Data.ServerIdentifier  := ToIP4(fServerIdentifier);
  Data.LastIpLE          := 0;
  Data.IpMinLE           := ToIP4(fRangeMin);
  Data.IpMaxLE           := ToIP4(fRangeMax);
  Data.DnsServer         := TIntegerDynArray(ToIP4s(fDnsServers));
  Data.NtpServers        := TIntegerDynArray(ToIP4s(fNtpServers));
  Data.DomainName        := fDomainName;
  Data.StaticIP          := nil;
  Data.StaticMac         := nil;
  for i := 0 to high(fStatic) do
    if not Data.AddStatic(fStatic[i]) then // add sorted, from 'ip' 'mac/hex=ip'
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
  fBoot.PrepareScope(Data.Boot, Data.Options);
  Data.Profiles := nil;
  SetLength(Data.Profiles, length(fProfiles));
  for i := 0 to high(fProfiles) do
    fProfiles[i].PrepareScope(Data.Profiles[i]);
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
  fMetricsCsvMinutes := 5;
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

function TDhcpServerSettings.AddScope(one: TDhcpScopeSettings): TDhcpScopeSettings;
begin
  if one = nil then
    one := TDhcpScopeSettings.Create; // default '192.168.1.1/24' subnet
  if self <> nil then
    PtrArrayAdd(fScope, one); // will be owned by this instance
  result := one;
end;


{ TDhcpProcess }

function TDhcpProcess.GetCount: integer;
var
  n: integer;
  s: PDhcpScope;
begin
  result := 0;
  if fScope = nil then
    exit;
  fScopeSafe.ReadLock; // protect fScope[]
  s := pointer(fScope);
  if s <> nil then
  begin
    n := PDALen(PAnsiChar(s) - _DALEN)^ + _DAOFF;
    repeat
      inc(result, s^.Count - s^.FreeListCount);
      inc(s);
      dec(n);
    until n = 0;
  end;
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
    // support FileName/MetricsFolder background persistence
    if aSettings.FileName <> '' then
      SetFileName(aSettings.FileName); // LoadFromFile() once fScope[] are set
    fFileFlushSeconds := aSettings.FileFlushSeconds;
    fMetricsCsvSeconds := aSettings.MetricsCsvMinutes * SecsPerMin;
    fMetricsFolder := '';
    if aSettings.MetricsFolder <> '' then
      SetMetricsFolder(aSettings.MetricsFolder);
    fOptions := aSettings.Options;
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
    result := GetScope(nfo.ip)^.AddStatic(nfo);
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
    fMetricsDroppedPackets := 0;
    fMetricsInvalidRequest := 0;
  finally
    fScopeSafe.WriteUnLock;
  end;
end;

procedure TDhcpProcess.ClearLeases(keepWriteLock: boolean);
var
  s: PDhcpScope;
  i: PtrInt;
begin
  fScopeSafe.WriteLock;
  try
    s := pointer(fScope);
    for i := 1 to length(fScope) do
    begin
      s^.ClearLeases;
      inc(s);
    end;
    fModifSequence := 0;
    fModifSaved := 0;
  finally
    if not keepWriteLock then // e.g. LoadFromText() would continue locked
      fScopeSafe.WriteUnLock;
  end;
end;

procedure TDhcpProcess.Shutdown;
begin
  // disable ComputeResponse()
  if fState in [sNone, sShutdown] then
    exit; // no Setup(), or called twice
  fState := sShutdown;  // abort any ComputeResponse() ASAP
  fScopeSafe.WriteLock; // wait for any pending process
  try
    fState := sShutdown; // paranoid
  finally
    fScopeSafe.WriteUnLock;
  end;
  // persist FileName and Metrics for clean shutdown
  if (fFileName <> '') and
     (fModifSaved <> fModifSequence) then
    SaveToFile(fFileName); // make fScopeSafe.ReadLock/ReadUnLock
  if fMetricsFolder <> '' then
    SaveMetricsFolder({andCsv=}true);
  fLog.Add.Log(sllDebug, 'Shutdown: count=% seq=% saved=%',
    [GetCount, fModifSequence, fModifSaved], self);
  // don't Clear the entries: we may call Setup() and restart again
end;

function TDhcpProcess.OnIdle(tix64: Int64): integer;
var
  tix32, n: cardinal;
  saved: integer;
  csv: boolean;
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
    // background persist into FileName and MetricsFolder if needed
    saved := 0;
    if (fFileName <> '') and
       (fFileFlushSeconds <> 0) and         // = 0 if disabled
       (fModifSaved <> fModifSequence) then // if something new to be written
      if tix32 >= fFileFlushTix then     // reached the next persistence time
      begin
        fFileFlushTix := tix32 + fFileFlushSeconds;  // every 30 secs by default
        saved := SaveToFile(fFileName);// make fScopeSafe.ReadLock/ReadUnLock
        // do not aggressively retry if saved<0 (write failed)
        // note: no localcopy made yet - TUdpServerThread.OnIdle would block any
        // recv(UDP packet) so we would need to create a background thread
        if fMetricsFolder <> '' then
        begin
          csv := tix32 >= fMetricsCsvTix;
          SaveMetricsFolder(csv);
          if csv then
            fMetricsCsvTix := tix32 + fMetricsCsvSeconds; // append every 5 mins
        end;
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
  entries := 0;
  last := nil;
  ClearLeases({keepWriteLock=}true); // easier than complex s^.Lock/UnLock
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
              if s^.Count = length(s^.Entry) then
                SetLength(s^.Entry, NextGrow(s^.Count));
              new := @s^.Entry[s^.Count];
              inc(s^.Count);
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

procedure TDhcpProcess.ConsolidateMetrics(var global: TDhcpAllMetrics);
var
  n: integer;
  s: PDhcpScope;
begin
  FillCharFast(global, SizeOf(global), 0);
  global.Total[dsmDroppedPackets] := fMetricsDroppedPackets; // no scope counter
  global.Total[dsmInvalidRequest] := fMetricsInvalidRequest;
  fScopeSafe.ReadLock; // protect fScope[]
  s := pointer(fScope);
  if s <> nil then
  begin
    n := PDALen(PAnsiChar(s) - _DALEN)^ + _DAOFF;
    repeat
      s^.Safe.Lock;
      AddMetrics(global.Current, s^.Metrics.Current);
      AddMetrics(global.Total,   s^.Metrics.Total);
      s^.Safe.UnLock;
      inc(s);
      dec(n);
    until n = 0;
  end;
  fScopeSafe.ReadUnLock;
end;

procedure TDhcpProcess.ConsolidateMetrics(var global: TDhcpMetrics);
var
  n: integer;
  s: PDhcpScope;
begin
  FillZero(global);
  global[dsmDroppedPackets] := fMetricsDroppedPackets; // no scope counters
  global[dsmInvalidRequest] := fMetricsInvalidRequest;
  fScopeSafe.ReadLock; // protect fScope[]
  s := pointer(fScope);
  if s <> nil then
  begin
    n := PDALen(PAnsiChar(s) - _DALEN)^ + _DAOFF;
    repeat
      s^.Safe.Lock;
      AddMetrics(global, s^.Metrics.Current);
      AddMetrics(global, s^.Metrics.Total);
      s^.Safe.UnLock;
      inc(s);
      dec(n);
    until n = 0;
  end;
  fScopeSafe.ReadUnLock;
end;

procedure TDhcpProcess.ResetMetrics;
var
  n: integer;
  s: PDhcpScope;
begin
  fMetricsDroppedPackets := 0;
  fMetricsInvalidRequest := 0;
  fScopeSafe.ReadLock; // protect fScope[]
  s := pointer(fScope);
  if s <> nil then
  begin
    n := PDALen(PAnsiChar(s) - _DALEN)^ + _DAOFF;
    repeat
      s^.ResetMetrics;
      inc(s);
      dec(n);
    until n = 0;
  end;
  fScopeSafe.ReadUnLock;
end;

function TDhcpProcess.SaveMetricsToJson: RawUtf8;
var
  global: TDhcpMetrics;
begin
  ConsolidateMetrics(global);
  result := MetricsToJson(global);
end;

procedure TDhcpProcess.SaveMetricsFolder(AndCsv: boolean);
var
  s: PDhcpScope;
  n: integer;
  u: RawUtf8;
  now: PShortString;
  needheader: boolean;
  T: TSynSystemTime;
  local: TDhcpMetrics;
  nowIso, nowUnix: TShort23;
begin
  // persist main 'metrics.json'
  if fMetricsJson <> '' then
    FileFromString(SaveMetricsToJson, fMetricsJson);
  // persist all scope information
  if AndCsv then
  begin
    T.FromNowUtc;     // needed for T.Month check anyway
    nowIso[0] := #0;  // date/time texts computed on need below
    nowUnix[0] := #0;
  end;
  fScopeSafe.ReadLock; // protect fScope[]
  try
    s := pointer(fScope);
    if s <> nil then
    begin
      n := PDALen(PAnsiChar(s) - _DALEN)^ + _DAOFF;
      repeat
        s^.Safe.Lock;
        try
          if s^.MetricsJsonFileName <> '' then
          begin
            // persist the main .json file of this scope in its current state
            local := s^.Metrics.Current;
            AddMetrics(local, s^.Metrics.Total);
            u := MetricsToJson(local);
            FileFromString(u, s^.MetricsJsonFileName);
          end;
          if AndCsv and
             not IsZero(s^.Metrics.Current) then
          begin
            // persist the CSV and reset Current[]
            needheader := false;
            if s^.MetricsCsvFileMonth <> T.Month then // first time or new month
            begin
              s^.MetricsCsvFileMonth := T.Month;
              s^.MetricsCsvFileName := MakeString([fMetricsFolder,
                UInt4DigitsToShort(T.Year), UInt2DigitsToShortFast(T.Month),
                '_', s^.MetricsFileSubnet, '.csv']);
              needheader := not FileExists(s^.MetricsCsvFileName);
            end;
            if dsoCsvUnixTime in s^.Options then
            begin
              if nowUnix[0] = #0 then
                AppendShortQWord(UnixTimeUtc, nowUnix); // e.g. '1770715986'
              now := @nowUnix;
            end
            else
            begin
              if nowIso[0] = #0 then
                T.ToIsoDateTimeShort(nowIso, ' '); // e.g. '2026-02-09 20:09:53'
              now := @nowIso;
            end;
            u := MetricsToCsv(s^.Metrics.Current, needheader, now);
            AppendToFile(u, s^.MetricsCsvFileName);
            AddMetrics(s^.Metrics.Total, s^.Metrics.Current);
            FillZero(s^.Metrics.Current);
          end;
        finally
          s^.Safe.UnLock;
        end;
        inc(s);
        dec(n);
      until n = 0;
    end;
  finally
    fScopeSafe.ReadUnLock;
  end;
end;

procedure TDhcpProcess.SetMetricsFolder(const folder: TFileName);
var
  s: PDhcpScope;
  json: RawUtf8;
  n: integer;
  i: PtrInt;
  p: PAnsiChar;
  local: TDhcpMetrics;
begin
  fMetricsFolder := '';
  if (folder = '') or
     (fMetricsCsvSeconds = 0) or
     not WritableFolder(folder, '', fMetricsFolder) then
    exit;
  // load non-scope metrics from main json file
  fMetricsJson := fMetricsFolder + 'metrics.json';
  json := StringFromFile(fMetricsJson);
  if (json <> '') and
     MetricsFromJson(json, local) then
  begin
    fMetricsDroppedPackets := local[dsmDroppedPackets]; // not in scopes
    fMetricsInvalidRequest := local[dsmInvalidRequest];
  end;
  // load scope metrics from their own json files
  fScopeSafe.ReadLock; // protect Scope[]
  try
    s := pointer(fScope);
    if s <> nil then
    begin
      n := PDALen(PAnsiChar(s) - _DALEN)^ + _DAOFF;
      repeat
        // compute the metrics file names for this scope
        s^.MetricsFileSubnet := s^.Subnet.ToShort;
        p := @s^.MetricsFileSubnet;
        for i := 1 to ord(p[0]) do // make it file-compatible
          case p[i] of
            '.':
              p[i] := '-';
            '/':
              p[i] := '_';
          end;
        s^.MetricsJsonFileName := MakeString([
          fMetricsFolder, s^.MetricsFileSubnet, '.json']);
        // retrieve the previous metrics totals for this scope
        json := StringFromFile(s^.MetricsJsonFileName);
        if json <> '' then
          MetricsFromJson(json, s^.Metrics.Total);
        inc(s);
        dec(n);
      until n = 0;
    end;
  finally
    fScopeSafe.ReadUnLock;
  end;
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

procedure TDhcpProcess.DoLog(Level: TSynLogLevel; const Context: ShortString;
  const Data: TDhcpProcessData);
var
  fam: TSynLogFamily;
  one: TSynLog;
  msg: ShortString;
begin
  // this method could be overriden to extend or replace the default logging
  fam := fLog.Family;
  if (fam <> nil) and
     (Level in fam.Level) then
    one := fam.Add
  else if dsoSystemLog in fOptions then
    one := nil // compute msg for JournalSend() but not for TSynLog.LogText()
  else
    exit;
  // generate 'ComputeResponse: REQUEST MAC context RESPONSE IP Host boot=..'
  msg := 'ComputeResponse: ';
  AppendShortAnsi7String(DHCP_TXT[Data.RecvType], msg);
  AppendShortChar(' ', @msg);
  if Data.Mac[0] <> #0 then
  begin
    AppendShort(Data.Mac, msg);
    AppendShortChar(' ', @msg);
  end;
  AppendShort(Context, msg);
  AppendShortChar(' ', @msg);
  if Data.SendType <> dmtUndefined then
  begin
    AppendShortAnsi7String(DHCP_TXT[Data.SendType], msg);
    AppendShortChar(' ', @msg);
  end;
  AppendShort(Data.Ip, msg);
  if Data.RecvHostName^[0] <> #0 then
  begin
    AppendShortChar(' ', @msg);
    AppendShort(Data.RecvHostName^, msg);
  end;
  if Data.RecvBoot <> dcbDefault then
  begin
    AppendShort(' boot=', msg); // e.g. 'boot=ipxe-x64'
    AppendShortAnsi7String(BOOT_TXT[Data.RecvBoot], msg);
  end;
  if (Data.RecvProfile <> nil) and
     (Data.RecvProfile.name <> '') then
  begin
    AppendShort(' profile=', msg);
    AppendShortAnsi7String(Data.RecvProfile.name, msg);
    if msg[0] = #255 then
      dec(msg[0]); // avoid buffer overflow writing ASCIIZ
  end;
  msg[ord(msg[0]) + 1] := #0; // ensure ASCIIZ
  // efficiently append to local TSynLog
  if one <> nil then
    one.LogText(Level, PUtf8Char(@msg[1]), nil); // this is the fastest API
  // optionnally send 'REQUEST MAC...' text to system logs (much slower)
  if dsoSystemLog in fOptions then
    JournalSend(Level, @msg[18], ord(msg[0]) - 17, {trimlogdate=}false);
end;

procedure ParseRecvLensRai(var Data: TDhcpProcessData);
var
  p: PAnsiChar;
  len: PtrInt;
begin
  p := @Data.Recv.options[Data.RecvLens[doRelayAgentInformation]]; // <> 0 by caller
  len := ord(p[0]);          // whole Option 82 length
  inc(p);
  repeat
    dec(len, ord(p[1]) + 2); // O(1) decode sub-option binary as TLV
    if len < 0 then          // avoid buffer overdloas
    begin
      Int64(Data.RecvLensRai) := 0; // ignore any previously decoded sub-options
      exit;
    end;
    if ord(p[0]) <= high(RAI_OPTION_INV) then
      Data.RecvLensRai[RAI_OPTION_INV[ord(p[0])]] :=
        PAnsiChar(@p[1]) - PAnsiChar(@Data.Recv.options);
    p := @p[ord(p[1]) + 2];
  until len = 0;
end;

function TDhcpProcess.ParseFrame(var Data: TDhcpProcessData): boolean;
begin
  Data.Mac64 := 0;
  Data.Ip4 := 0;
  Data.SendType := dmtUndefined;
  Data.RecvBoot := dcbDefault;
  Data.RecvProfile := nil;
  Data.RecvType := DhcpParse(@Data.Recv, Data.RecvLen, Data.RecvLens, nil, @Data.Mac64);
  Data.Ip[0] := #0;
  if Data.Mac64 <> 0 then
  begin
    // valid DHCP frame with RecvType <> dmtUndefined
    Data.Mac[0] := #17;
    ToHumanHexP(@Data.Mac[1], @Data.Mac64, 6);
    Data.RecvHostName := DhcpData(@Data.Recv, Data.RecvLens[doHostName]);
    Int64(Data.RecvLensRai) := 0;
    if Data.RecvLens[doRelayAgentInformation] <> 0 then
      ParseRecvLensRai(Data);
    result := true;
  end
  else
  begin
    Data.Mac[0] := #0;
    Data.RecvHostName := @Data.Mac;
    result := false;
  end;
end;

function TDhcpProcess.RetrieveFrameIP(
  var Data: TDhcpProcessData; Lease: PDhcpLease): boolean;
var
  ip4: TNetIP4;
begin
  // called from DISCOVER and REQUEST
  result := false;
  ip4 := DhcpIP4(@Data.Recv, Data.RecvLens[doDhcpRequestedAddress]); // opt 50
  if ip4 = 0 then
    exit;
  if (Lease = nil) or
     (Lease^.IP4 <> ip4) then
    if (not Data.Scope^.SubNet.Match(ip4)) or
       Data.Scope^.IsStaticIP(ip4) or
       (Data.Scope^.FindIp4(ip4) <> nil) then
    begin
      // this IP seems already used by another MAC
      IP4Short(@ip4, Data.Ip);
      DoLog(sllTrace, 'ignore requested', Data);
      inc(Data.Scope^.Metrics.Current[dsmDroppedInvalidIP]);
      exit;
    end;
  inc(Data.Scope^.Metrics.Current[dsmOption50Hits]);
  Data.Ip4 := ip4;
  result := true;
end;

procedure DhcpDataAddRegularOptions(var Data: TDhcpProcessData);
begin
  DhcpDataAddOptionOnce32(Data, doSubnetMask, Data.Scope^.Subnet.mask);
  DhcpDataAddOptionOnce32(Data, doBroadcastAddress, Data.Scope^.Broadcast);
  DhcpDataAddOptionOnce32(Data, doRouters, Data.Scope^.Gateway);
  DhcpDataAddOptionOnceA32(Data, doDomainNameServers, pointer(Data.Scope^.DnsServer));
  DhcpDataAddOptionOnceA32(Data, doNtpServers, pointer(Data.Scope^.NtpServers));
  if (Data.RecvType <> dmtInform) and
     not (doDhcpLeaseTime in Data.SendOptions) then // + big-endian 51,58,59
  begin
    DhcpDataAddOptionOnce32(Data, doDhcpLeaseTime, Data.Scope^.LeaseTime);
    DhcpDataAddOptionOnce32(Data, doDhcpRenewalTime, Data.Scope^.RenewalTime);
    DhcpDataAddOptionOnce32(Data, doDhcpRebindingTime, Data.Scope^.Rebinding);
  end;
end;

function DhcpDataEnd(var Data: TDhcpProcessData): PtrUInt;
var
  b: PtrUInt;
begin
  // send back verbatim Option 61 if any
  b := Data.RecvLens[doDhcpClientIdentifier];
  if b <> 0 then
  begin
    DhcpCopyOption(Data.SendEnd, @Data.Recv.options[b]);
    inc(Data.Scope^.Metrics.Current[dsmOption61Hits]);
  end;
  // send back Option 82 if any - should be the very last option by RFC 3046
  b := Data.RecvLens[doRelayAgentInformation];
  if b <> 0 then
  begin
    DhcpCopyOption(Data.SendEnd, @Data.Recv.options[b]);
    inc(Data.Scope^.Metrics.Current[dsmOption82Hits]);
  end;
  // add trailer to the Data.Send frame and returns its final size in bytes
  Data.SendEnd^ := #255;
  result := Data.SendEnd - PAnsiChar(@Data.Send) + 1;
end;

function TDhcpProcess.CallbackAborted(var Data: TDhcpProcessData): boolean;
begin
  // caller ensured fOnComputeResponse <> nil
  try
    fOnComputeResponse(self, Data);
  except
    Data.SendEnd := nil; // intercept any callback issue and abort
  end;
  result := Data.SendEnd = nil; // callback asked to silently ignore this frame
  if not result then
    exit; // continue
  DoLog(sllTrace, 'ignored by callback ', Data);
  inc(Data.Scope^.Metrics.Current[dsmDroppedCallback]);
end;

function TDhcpProcess.FinalizeFrame(var Data: TDhcpProcessData): PtrInt;
begin
  integer(Data.SendOptions) := 0;
  // check input and set Data.RecvBoot
  SetBoot(Data);
  // append "profiles" custom options - always first since have precedence
  if Data.RecvProfile <> nil then
    DhcpDataAddOptionProfile(Data);
  // append "boot" specific options 60,66,67,97
  if Data.RecvBoot <> dcbDefault then
    AddBootOptions(Data);
  // append regular DHCP 1,3,6,15,28,42 [+ 51,58,59] options
  DhcpDataAddRegularOptions(Data);
  // optional callback support
  if Assigned(fOnComputeResponse) and
     CallbackAborted(Data) then
    // aborted by callback: nothing to send
    result := 0
  else
    // final options 61/82 verbatim retransmission + end Send buffer stream
    result := DhcpDataEnd(Data);
end;

function TDhcpProcess.FindScope(var data: TDhcpProcessData): boolean;
begin
  result := true;
  Data.Scope := GetScope(DhcpIP4(@Data.Recv, Data.RecvLensRai[dorLinkSelection]));
  if Data.Scope <> nil then
  begin
    // RFC 3527 sub-option in relay-agent-information 82, on complex relay
    // environments where giaddr is already meaningful for routing and cannot
    // be overloaded for subnet selection
    inc(Data.Scope^.Metrics.Current[dsmOption82SubnetHits]);
    exit;
  end;
  // fallback to option 118 or giaddr if link-selection was set but invalid
  Data.Scope := GetScope(DhcpIP4(@Data.Recv, Data.RecvLens[doSubnetSelection]));
  if Data.Scope <> nil then
  begin
    // RFC 3011 defines option 118 which overrides giaddr, e.g. on centralized
    // DHCP server serving multiple logical subnets behind the same relay IP
    inc(Data.Scope^.Metrics.Current[dsmOption118Hits]);
    exit;
  end;
  // fallback to giaddr if subnet-selection was set but invalid
  if Data.Recv.giaddr <> 0 then
    // e.g. VLAN 10 relay set giaddr=192.168.10.1 Gateway IP field
    // - giaddr from RFC 2131 is authoritative so we should not fallback
    Data.Scope := GetScope(Data.Recv.giaddr)
  else if Data.RecvIp4 <> 0 then
    // no giaddr: check RecvIp4 bound local server IP as set by the UDP server
    Data.Scope := GetScope(Data.RecvIp4)
  else
    // no option 118 no giaddr nor RecvIp4: default to Scope[0]
    Data.Scope := pointer(fScope);
  result := Data.Scope <> nil;
end;

function ClientUuid(opt: TDhcpOption; var data: TDhcpProcessData; uuid: pointer): PDhcpLease;
var
  ip4: TNetIP4;
  v: PByte;
  m: PByteArray;
  len: PtrUInt;
begin // opt = doDhcpClientIdentifier or doUuidClientIdentifier
  result := nil;
  v := @data.Recv.options[data.RecvLens[opt]]; // v^[0] <> 0
  len := v^;
  inc(v); // v^ points to the data
  case len of // UUID/DUID/vendor-specific are usually >= 8-16 bytes
    0 .. 3,   // < MIN_UUID_BYTES
    6:
      exit;   // too short, or 6-bytes MAC
    7:
      if v^ = 1 then
        exit; // Eth=1 + MAC is searched by FindMac()
    17:
      if v^ = 0 then
      begin
        // e.g. doUuidClientIdentifier RFC 4578 send Type=0 + SMBIOS UUID
        inc(v);
        dec(len);
      end;
  end;
  ip4 := DoFindUuid(uuid, pointer(v), len); // O(n) fast search
  if ip4 = 0 then
    exit;
  result := @data.Temp; // fake transient PDhcpLease for this StaticUuid[]
  result^.IP4 := ip4;
  PInt64(@result^.Mac)^ := Data.Mac64; // also reset State+RateLimit
  result^.State := lsStatic;
  // append /GUID or /UUID in logs after the MAC address (up to 63 chars)
  m := @data.Mac;
  AppendShortChar('/', pointer(m));
  if len = SizeOf(TGuid) then
    AppendShortUuid(PGuid(v)^, PShortString(m)^)
  else
  begin
    len := MinPtrUInt((high(data.Mac) - m[0]) shr 1, len);
    mormot.core.text.BinToHex(pointer(v), @m[m[0]], len);
    inc(m[0], len * 2);
  end;
end;

function TDhcpProcess.FindLease(var data: TDhcpProcessData): PDhcpLease;
begin
  // from StaticUuid[]
  result := pointer(Data.Scope^.StaticUuid);
  if result <> nil then
    if Data.RecvLens[doDhcpClientIdentifier] <> 0 then  // computer MAC/UUID
      result := ClientUuid(doDhcpClientIdentifier, Data, pointer(result))
    else if Data.RecvLens[doUuidClientIdentifier] <> 0 then  // IPXE/VM
      result := ClientUuid(doUuidClientIdentifier, Data, pointer(result))
    else
      result := nil;
  // from Entry[] and StaticMac[]
  if result = nil then
    result := Data.Scope^.FindMac(Data.Mac64);
end;

const
  ARCH_DCB: array[0 .. 12] of TDhcpClientBoot = (
    dcbBios, dcbDefault, dcbX86, dcbDefault, dcbDefault, dcbDefault, // 0..5
    dcbX86, dcbX64, dcbX64, dcbX64, dcbA32, dcbA64, dcbA64);   // 6..12
  // 0 = BIOS x86 (Legacy), 1 = NEC/PC87, 2 = EFI x86, 3 = EFI bytecode,
  // 4 = EFI XScale, 5 = EFI early x64, 6 = EFI x86 Microsoft, 7 = EFI x64,
  // 8 = EFI x64 HTTP, 9 = EFI2 x64 HTTP, 10 = EFI arm32, 11 = EFI arm64,
  // 12 = EFI arm64 HTTP - 8,9,12 mean HTTP boot capability, not selection

procedure TDhcpProcess.SetBoot(var Data: TDhcpProcessData);
var
  b: TDhcpClientBoot;
  o: PByteArray;
  vendor: PShortString;
  a: PtrUInt;
begin
  b := dcbDefault;
  // parse RFC 4578 PXE option 93 as uint16 (00:00)
  o := @Data.RecvLens[doClientArchitecture];
  if o[0] <> 0 then
  begin
    o := @Data.Recv.options[o[0]];
    if (o[0] = 2) and
       (o[1] = 0) and // stored as BigEndian
       (o[2] <= high(ARCH_DCB)) then
      b := ARCH_DCB[o[2]];
  end;
  vendor := DhcpData(@Data.Recv, Data.RecvLens[doVendorClassIdentifier]);
  if b = dcbDefault then
    // fallback to Option 60 parsing - e.g. 'PXEClient:Arch:00007:UNDI:003016'
    if (vendor^[0] >= #17) and
       IdemPChar(@vendor^[1], 'PXECLIENT:ARCH:0') then
    begin
      a := GetCardinal(@vendor^[17]);
      if a <= high(ARCH_DCB) then
        b := ARCH_DCB[a];
    end;
  if b = dcbDefault then
    exit; // normal DHCP boot
  // detect iPXE from RFC 3004 Option 77
  if DhcpIdem(@Data.Recv, Data.RecvLens[doUserClass], 'iPXE') then
    // change dcbBios..dcbA64 into dcbIpxeBios..dcbIpxeA64
    inc(b, ord(dcbIpxeBios) - ord(dcbBios))
  else if (vendor^[0] >= #10) and
          IdemPChar(@vendor^[1], 'HTTPCLIENT') then
    // HTTPClient in Option 60 indicates native UEFI firmware HTTP boot
    // - will fallback to TFTP is no HTTP URI is supplied
    case b of
      dcbX64:
        b := dcbX64Http;
      dcbA64:
        b := dcbA64Http;
    end;
  Data.RecvBoot := b;
end;

procedure TDhcpProcess.AddBootOptions(var Data: TDhcpProcessData);
var
  boot: ^TDhcpScopeBoot;
begin
  // we know that Data.Boot <> dcbDefault: validate the request
  boot := @Data.Scope^.Boot;
  if boot^.Remote[Data.RecvBoot] = '' then
  begin
    DoLog(sllDebug, 'missing boot file', Data); // including 'boot=ipxe-x64'
    Data.RecvBoot := dcbDefault;                // no 'boot=...' any more
    inc(Data.Scope^.Metrics.Current[dsmDroppedPxeBoot]);
    // will still send back an OFFER/ACK but with no PXE options
    // - this is what ISC DHCP, KEA, and dnsmasq do in practice
    exit;
  end;
  inc(Data.Scope^.Metrics.Current[dsmPxeBoot]);
  // known configuration: append PXE/iPXE specific 66/67 options
  DhcpAddOptionU(Data.SendEnd, doTftpServerName, boot^.NextServer);
  DhcpAddOptionU(Data.SendEnd, doBootFileName,   boot^.Remote[Data.RecvBoot]);
  // copy back verbatim option 60 and 97 to PXE clients
  DhcpDataCopyOption(Data, doVendorClassIdentifier);
  DhcpDataCopyOption(Data, doUuidClientIdentifier);
end;

procedure TDhcpProcess.SetProfile(var Data: TDhcpProcessData);
var
  n: integer;
  p: PDhcpScopeProfile;
begin
  // implement "first precise profile wins" deterministic behavior
  p := pointer(Data.Scope^.Profiles); // caller ensured <> nil
  n := PDALen(PAnsiChar(p) - _DALEN)^ + _DAOFF;
  repeat
    if // AND conditions (all must match) = "all"
       ((p^.all = nil) or
        DhcpDataMatchAll(Data, pointer(p^.all))) and
       // OR conditions (at least one must match) = "any"
       ((p^.any = nil) or
        DhcpDataMatchAny(Data, pointer(p^.any))) and
       // NOT AND conditions (all must NOT match) = "not-all"
       ((p^.notall = nil) or
        not DhcpDataMatchAll(Data, pointer(p^.notall))) and
       // NOT OR conditions (at least one must NOT match) = "not-any"
       ((p^.notany = nil) or
        not DhcpDataMatchAny(Data, pointer(p^.notany))) then
    begin
      Data.RecvProfile := p;
      // all=any=nil as fallback if nothing more precise did apply
      // "not-all"/"not-any" without any "all"/"any" are just ignored
      // unless they are the eventual default
      if (p^.all <> nil) or
         (p^.any <> nil) then
        // return first exact matching profile (deterministic)
        exit;
    end;
    inc(p);
    dec(n);
  until n = 0;
end;

function TDhcpProcess.ComputeResponse(var Data: TDhcpProcessData): PtrInt;
var
  p: PDhcpLease;
begin
  result := -1; // error
  // do nothing on missing Setup() or after Shutdown
  if (self = nil) or
     (fScope = nil) or
     (fState <> sSetup) then
  begin
    if self <> nil then
      inc(fMetricsDroppedPackets); // no Scope yet
    exit;
  end;
  // parse and validate the request
  if not ParseFrame(Data) then
  begin
    if Data.RecvIp4 <> 0 then
      IP4Short(@Data.RecvIp4, Data.Ip); // bound server IP
    DoLog(sllTrace, 'frame', Data);
    inc(fMetricsInvalidRequest); // no Scope yet
    exit;
    // unsupported RecvType will continue and inc(Scope^.Metrics) below
  end;
  fScopeSafe.ReadLock; // protect Scope[] but is reentrant and not-blocking
  try
    // detect the proper scope to use
    if not FindScope(Data) then
    begin
      if Data.Recv.giaddr <> 0 then
        IP4Short(@Data.Recv.giaddr, Data.Ip)
      else if Data.RecvIp4 <> 0 then
        IP4Short(@Data.RecvIP4, Data.Ip);
      DoLog(sllDebug, 'not subnet for', Data);
      inc(Data.Scope^.Metrics.Current[dsmDroppedNoSubnet]);
      exit; // MUST NOT respond if no subnet matches giaddr
    end;
    // identify any matching input from this scope "profiles" definition
    if Data.Scope^.Profiles <> nil then
      SetProfile(Data);
    // compute the corresponding IPv4 according to the internal lease list
    result := 0; // no response, but no error
    Data.Tix32 := GetTickSec;
    Data.Scope^.Safe.Lock; // blocking for this subnet
    try
      // find any existing lease - or StaticMac[] StaticUuid[] fake lease
      p := FindLease(Data);
      // process the received DHCP message
      case Data.RecvType of
        dmtDiscover:
          begin
            inc(Data.Scope^.Metrics.Current[dsmDiscover]);
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
                  DoLog(sllWarning, 'exhausted IPv4', Data);
                  inc(Data.Scope^.Metrics.Current[dsmDroppedNoAvailableIP]);
                  exit;
                end;
              end;
              if p = nil then
              begin
                p := Data.Scope^.NewLease(Data.Mac64);
                Data.Scope^.LastDiscover := Data.Scope^.Index(p);
              end;
              p^.IP4 := Data.Ip4;
            end
            else
              // keep existing/previous/static IP4
              Data.Ip4 := p^.IP4;
            // update the lease information
            if p^.State = lsStatic then
            begin
              p^.Expired := Data.Tix32;
              inc(Data.Scope^.Metrics.Current[dsmStaticHits]);
            end
            else
            begin
              inc(fModifSequence); // trigger SaveToFile() in next OnIdle()
              p^.State := lsReserved;
              p^.Expired := Data.Tix32 + Data.Scope^.OfferHolding;
              inc(Data.Scope^.Metrics.Current[dsmDynamicHits]);
            end;
            // respond with an OFFER
            inc(Data.Scope^.Metrics.Current[dsmOffer]);
            Data.SendType := dmtOffer;
          end;
        dmtRequest:
          begin
            inc(Data.Scope^.Metrics.Current[dsmRequest]);
            if (p <> nil) and
               (p^.IP4 <> 0) and
               ((p^.State in [lsReserved, lsAck, lsStatic]) or
                ((p^.State = lsOutdated) and
                 (Data.Scope^.GraceFactor > 1) and
                 (Data.Scope^.LeaseTimeLE < SecsPerHour) and // grace period
                 (Data.Tix32 - p^.Expired <
                    Data.Scope^.LeaseTimeLE * Data.Scope^.GraceFactor))) then
              // RFC 2131: lease is Reserved after OFFER = SELECTING
              //           lease is Ack/Static/Outdated = RENEWING/REBINDING
              inc(Data.Scope^.Metrics.Current[dsmLeaseRenewed])
            else if RetrieveFrameIP(Data, p) then
              begin
                // no lease, but Option 50 = INIT-REBOOT
                if p = nil then
                  p := Data.Scope^.NewLease(Data.Mac64)
                else
                  inc(Data.Scope^.Metrics.Current[dsmLeaseRenewed]);
                p^.IP4 := Data.Ip4;
              end
              else
              begin
                // no lease, and none or invalid Option 50 = send NAK response
                DoLog(sllTrace, 'out-of-sync NAK', Data);
                Data.SendType := dmtNak;
                Data.SendEnd := DhcpNew(Data.Send, dmtNak, Data.Recv.xid,
                  PNetMac(@Data.Mac64)^, Data.Scope^.ServerIdentifier);
                result := FinalizeFrame(Data);
                inc(Data.Scope^.Metrics.Current[dsmNak]);
                exit;
              end;
            // update the lease information
            if p^.State = lsStatic then
            begin
              p^.Expired := Data.Tix32;
              inc(Data.Scope^.Metrics.Current[dsmStaticHits]);
            end
            else
            begin
              inc(fModifSequence); // trigger SaveToFile() in next OnIdle()
              p^.State := lsAck;
              p^.Expired := Data.Tix32 + Data.Scope^.LeaseTimeLE;
              inc(Data.Scope^.Metrics.Current[dsmDynamicHits]);
            end;
            // respond with an ACK on the known IP
            inc(Data.Scope^.Metrics.Current[dsmAck]);
            Data.Ip4 := p^.IP4;
            Data.SendType := dmtAck;
          end;
        dmtDecline:
          begin
            inc(Data.Scope^.Metrics.Current[dsmDecline]);
            // client ARPed the OFFERed IP and detected conflict, so DECLINE it
            if (p = nil) or
               (p^.State <> lsReserved) then
            begin
              // ensure the server recently OFFERed one IP to that client
              // (match RFC intent and prevent blind poisoning of arbitrary IPs)
              DoLog(sllDebug, 'with no previous OFFER', Data);
              inc(Data.Scope^.Metrics.Current[dsmDroppedPackets]);
              exit;
            end;
            if (Data.Scope^.MaxDeclinePerSec <> 0) and // = 5 by default
               (fIdleTix <> 0) then  // OnIdle() would reset RateLimit := 0
              if p^.RateLimit < Data.Scope^.MaxDeclinePerSec then
                inc(p^.RateLimit)
              else
              begin
                // malicious client poisons the pool by sending repeated DECLINE
                DoLog(sllDebug, 'overload', Data);
                inc(Data.Scope^.Metrics.Current[dsmRateLimitHit]);
                exit;
              end;
            // invalidate OFFERed IP
            Data.Ip4 := DhcpIP4(@Data.Recv, Data.RecvLens[doDhcpRequestedAddress]);
            if (Data.Ip4 <> 0) and // authoritative IP
               not Data.Scope^.SubNet.Match(Data.Ip4) then // need clean IP
            begin
              inc(Data.Scope^.Metrics.Current[dsmDroppedInvalidIP]);
              Data.Ip4 := 0;
            end;
            if Data.Ip4 = 0 then
              Data.Ip4 := p^.IP4 // option 50 was not set (or not in our subnet)
            else
              inc(Data.Scope^.Metrics.Current[dsmOption50Hits]);
            p^.State := lsUnavailable;
            p^.IP4 := 0; // invalidate the previous offered IP
            if Data.Ip4 <> 0 then
            begin
              // store internally this IP as unavailable for NextIP4
              IP4Short(@Data.Ip4, Data.Ip);
              DoLog(sllTrace, 'as', Data);
              p := Data.Scope^.NewLease(0); // mac=0: sentinel to store this IP
              p^.State := lsUnavailable;
              p^.IP4 := Data.Ip4;
              p^.Expired := Data.Tix32 + Data.Scope^.DeclineTime;
            end;
            inc(fModifSequence); // trigger SaveToFile() in next OnIdle()
            exit; // server MUST NOT respond to a DECLINE message
          end;
        dmtRelease:
          begin
            inc(Data.Scope^.Metrics.Current[dsmRelease]);
            // client informs the DHCP server that it no longer needs the lease
            if (p = nil) or
               (p^.IP4 <> Data.Recv.ciaddr) or
               not (p^.State in [lsAck, lsOutdated]) then
            begin
              // detect and ignore out-of-synch or malicious client
              DoLog(sllTrace, 'unexpected', Data);
              inc(Data.Scope^.Metrics.Current[dsmDroppedInvalidIP]);
            end
            else
            begin
              IP4Short(@p^.IP4, Data.Ip);
              DoLog(sllTrace, 'as', Data);
              Data.Scope^.ReuseIp4(p); // set MAC=0 IP=0 State=lsFree
              inc(fModifSequence); // trigger SaveToFile() in next OnIdle()
              inc(Data.Scope^.Metrics.Current[dsmLeaseReleased]);
            end;
            exit; // server MUST NOT respond to a RELEASE message
          end;
        dmtInform:
          begin
            inc(Data.Scope^.Metrics.Current[dsmInform]);
            // client requests configuration options for its static IP
            Data.Ip4 := Data.Recv.ciaddr;
            if not Data.Scope^.Subnet.Match(Data.Ip4) or
               ((p <> nil) and
                not (p^.State in [lsUnavailable, lsOutdated, lsStatic])) then
            begin
              IP4Short(@Data.Ip4, Data.Ip);
              DoLog(sllDebug, 'unexpected', Data);
              inc(Data.Scope^.Metrics.Current[dsmDroppedInvalidIP]);
              exit;
            end;
            if (p <> nil) and
               (p^.State = lsStatic) then
            begin
              p^.Expired := Data.Tix32;
              inc(Data.Scope^.Metrics.Current[dsmStaticHits]);
            end
            else
            begin
              if p <> nil then
                inc(Data.Scope^.Metrics.Current[dsmDynamicHits]);
              if (dsoInformRateLimit in Data.Scope^.Options) and
                 (fIdleTix <> 0) and // OnIdle() would reset RateLimit := 0
                 not Data.Scope^.IsStaticIP(Data.Ip4) then
              begin
                // temporarily marks client IP as unavailable to avoid conflicts
                // (not set by default: stronger than the RFC requires, and no
                // other DHCP server like KEA, dnsmasq or Windows implements it)
                if p = nil then
                  p := Data.Scope^.NewLease(Data.Mac64)
                else if p^.RateLimit >= 2 then // up to 3 INFORM per MAC per sec
                begin
                  DoLog(sllDebug, 'overload', Data);
                  inc(Data.Scope^.Metrics.Current[dsmInformRateLimitHit]);
                  exit;
                end
                else
                begin
                  inc(p^.RateLimit);
                  // mark this IP as temporary unavailable
                  if p^.IP4 <> Data.Ip4 then
                    // create a new MAC-free lease for this other IP
                    p := Data.Scope^.NewLease(0); // mac=0: sentinel for this IP
                end;
                // update the lease information
                inc(fModifSequence); // trigger SaveToFile() in next OnIdle()
                p^.State := lsUnavailable;
                p^.Expired := Data.Tix32 + Data.Scope^.DeclineTime;
                p^.IP4 := Data.Ip4;
              end;
              // respond with an ACK and the standard options
              inc(Data.Scope^.Metrics.Current[dsmAck]);
              Data.SendType := dmtAck;
            end;
          end
      else
        begin
          // ParseFrame() was correct but this message type is not supported yet
          inc(Data.Scope^.Metrics.Current[dsmUnsupportedRequest]);
          DoLog(sllTrace, 'unsupported', Data);
          result := -1; // error
          exit;
        end;
      end;
      // compute the dmtOffer/dmtAck response frame over the very same xid
      IP4Short(@Data.Ip4, Data.Ip);
      Data.SendEnd := DhcpNew(Data.Send, Data.SendType, Data.Recv.xid,
        PNetMac(@Data.Recv.chaddr)^, Data.Scope^.ServerIdentifier);
      Data.Send.ciaddr := Data.Ip4;
      result := FinalizeFrame(Data);   // callback + options
      if result <> 0 then
        DoLog(sllTrace, 'into', Data); // if not canceled by callback
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
  FillLookupTable(@DHCP_OPTION_NUM, @DHCP_OPTION_INV, ord(pred(high(DHCP_OPTION_NUM))));
  FillLookupTable(@RAI_OPTION_NUM,  @RAI_OPTION_INV,  ord(high(RAI_OPTION_NUM)));
  assert(DHCP_OPTION_INV[high(DHCP_OPTION_INV)] = pred(high(TDhcpOption)));
  GetEnumTrimmedNames(TypeInfo(TDhcpMessageType), @DHCP_TXT,    scUpperCase);
  DHCP_TXT[dmtUndefined] := 'invalid';
  GetEnumTrimmedNames(TypeInfo(TDhcpOption),      @DHCP_OPTION, scKebabCase);
  GetEnumTrimmedNames(TypeInfo(TDhcpOptionRai),   @RAI_OPTION,  scKebabCase);
  GetEnumTrimmedNames(TypeInfo(TDhcpScopeMetric), @METRIC_TXT,  scKebabCase);
  GetEnumTrimmedNames(TypeInfo(TDhcpClientBoot),  @BOOT_TXT,    scKebabCase);
  {$ifndef HASDYNARRAYTYPE}
  Rtti.RegisterObjArrays([
    TypeInfo(TDhcpScopeSettingsObjArray), TDhcpScopeSettings,
    TypeInfo(TDhcpProfileSettingsObjArray), TDhcpProfileSettings]);
  {$endif HASDYNARRAYTYPE}
  Rtti.ByClass[TDhcpServerSettings].Props.NameChangeCase(scKebabCase, {nest=}true);

end.

