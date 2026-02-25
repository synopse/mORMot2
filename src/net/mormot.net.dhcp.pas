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
    - Middle-Level DHCP State Machine
    - High-Level Multi-Scope DHCP Server Processing Logic

   Implement DISCOVER, OFFER, REQUEST, DECLINE, ACK, NAK, RELEASE, INFORM.
   Background lease persistence using dnsmasq-compatible text files.
   Static IP reservation using MAC address or any other set of options (e.g. 61).
   Try to adapt to Windows environments (e.g. options 81/249 support).
   Support VLAN via SubNets / Scopes, giaddr and Relay Agent Option 82.
   Versatile JSON rules for vendor-specific or user-specific options.
   Scale up to 100k leases per subnet with minimal RAM/CPU consumption.
   No memory allocation is performed during the response computation.
   Prevent most client abuse with configurable rate limiting.
   Cross-Platform on Windows, Linux and MacOS, running in a single thread/core.
   Meaningful and customizable logging of the actual process (e.g. into syslog).
   Optional verbose debug of all input/output frames as meaninful JSON objects.
   Generate detailed JSON and CSV metrics as local files, global and per scope.
   Easy configuration via JSON or INI files.
   Expandable in code via callbacks or virtual methods as plugins.
   e.g. as full local PXE environment with our mormot.net.tftp.server.pas unit

  *****************************************************************************
  TODO:  - "range" reservation in "rules"
         - DHCPv6 support (much more complex, and mostly not required)
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
  mormot.core.data,
  mormot.core.log,
  mormot.net.sock,
  mormot.net.http,
  mormot.net.server;


{ **************** Low-Level DHCP Protocol Definitions }

const
  /// regular UDP port of DHCP server
  DHCP_SERVER_PORT = 67;
  /// regular UDP port of DHCP client
  DHCP_CLIENT_PORT = 68;

  /// the number of bytes in BOOTP header before TDhcpPacket.options[]
  DHCP_PACKET_HEADER = 240;
  /// the maximum decoded/encoded size of TDhcpPacket.options[] with MTU=1500
  // - makes SizeOf(TDhcpPacket) = 1468 which seems a legitimate high limit
  DHCP_MAX_OPTIONS = 1228;

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
    options: array[0 .. DHCP_MAX_OPTIONS - 1] of byte;
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
 // - doRequestedAddress is Requested IP Address option 50 (0.0.0.0)
 // - doLeaseTime is IP Lease Duration in seconds option 51 (86400 for 24h)
 // - doMessageType is option 53 (TDhcpMessageType) - first to appear
 // - doServerIdentifier is option 54 (192.168.1.1)
 // - doParameterRequestList is option 55 (1,3,6,15,51,54)
 // - doRenewalTime is T1 in seconds option 58 (43200 for 12h)
 // - doRebindingTime is T2 in seconds option 59 (75600 for 21h)
 // - doVendorClassIdentifier is PXE RFC 2132 option 60 (PXEClient)
 // - doClientIdentifier is option 61 ($01 + MAC)
 // - doTftpServerName is PXE option 66 (192.168.10.10 or host name)
 // - doBootFileName is PXE option 67 (pxelinux.0)
 // - doUserClass is PXE RFC 3004 option 77 (PXEClient:Arch:00000)
 // - doFqdn is RFC 4702 option 81 (device.example.com)
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
   doRequestedAddress,
   doLeaseTime,
   doMessageType,
   doServerIdentifier,
   doParameterRequestList,
   doRenewalTime,
   doRebindingTime,
   doVendorClassIdentifier,
   doClientIdentifier,
   doTftpServerName,
   doBootFileName,
   doUserClass,
   doFqdn,
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
  // - DHCP_TXT[dmtUndefined] = 'invalid' as expected by DoLog()
  DHCP_TXT: array[TDhcpMessageType] of RawUtf8;

  /// the JSON identifier of each DHCP option, used e.g. in "rules" JSON
  // - i.e. 'subnet-mask', 'routers', 'domain-name-servers', 'host-name',
  // 'domain-name', 'broadcast-address', 'ntp-servers', 'vendor-encapsulated-options',
  // 'requested-address', 'lease-time', 'message-type', 'server-identifier',
  // 'parameter-request-list', 'renewal-time', 'rebinding-time',
  // 'vendor-class-identifier', 'client-identifier', 'tftp-server-name',
  // 'boot-file-name', 'user-class', 'fqdn', 'relay-agent-information',
  // 'client-architecture', 'uuid-client-identifier' and 'subnet-selection'
  // - those were designed for simple/obvious semantic and shorter JSON
  // - alternate KEA-like identifiers will also be recognized in "rules" as
  // defined in the DHCP_OPTION_ALTERNATE[] constant
  DHCP_OPTION: array[TDhcpOption] of RawUtf8;
  /// KEA-like identifier of each DHCP RAI sub-option
  // - i.e. 'circuit-id', 'remote-id', 'link-selection', 'subscriber-id',
  // 'server-id-override', 'relay-id' and 'relay-port'
  RAI_OPTION: array[TDhcpOptionRai] of RawUtf8;

const
  /// alternate DHCP option "rules" identifiers, especially for KEA compatibility
  // - will be recognized in "rules" for less-astonishment principle
  // - https://kea.readthedocs.io/en/kea-3.1.4/arm/dhcp4-srv.html#standard-dhcpv4-options
  // - KEA expects a verbose "dhcp-*" prefix for the main options
  // - also relaxed some other identifiers, like router/dns/hostname/domain...
  DHCP_OPTION_ALTERNATE: array[doRouters .. doUuidClientIdentifier] of RawUtf8 = (
    'router', 'dns', 'hostname', 'domain', 'broadcast-address', 'ntp-server',
    'vendor-options', 'dhcp-requested-address', 'dhcp-lease-time',
    'dhcp-message-type', 'dhcp-server-identifier', 'dhcp-parameter-request-list',
    'dhcp-renewal-time', 'dhcp-rebinding-time', 'vendor-class',
    'dhcp-client-identifier', 'tftp-server', 'boot-filename', 'user-class',
    'fqdn', 'relay-agent', 'client-arch', 'client-uuid');


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
// - use rather TDhcpState.ClientNew/ClientFlush wrapper methods
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
  {$ifdef HASINLINE} inline; {$endif}


type
 /// TDhcpPacket.options[] is 312 bytes in RFC 2131, and even more up to the MTU
 // - e.g. maximum-message-size option 57 could advertise up to 1260 bytes
 TDhcpParseLen = word;
 /// efficient DhcpParse() results as O(1) lookup of recognized options
 // - 0 means that this TDhcpOption was not transmitted
 // - or store the position of an option length in TDhcpPacket.options[]
 TDhcpParsed = array[TDhcpOption] of TDhcpParseLen;
 /// efficient O(1) lookup of DHCP Option 82 recognized RAI sub-options
 TDhcpParsedRai = array[TDhcpOptionRai] of TDhcpParseLen;

/// parse a raw DHCP binary frame and return the length of all recognized options
// - returns dmtUndefined on invalid input DHCP frame
function DhcpParse(dhcp: PDhcpPacket; len: PtrInt; var lens: TDhcpParsed;
  found: PDhcpOptions = nil; mac: PNetMac = nil): TDhcpMessageType;

/// quickly validate a DHCP binary frame header
function DhcpParseHeader(dhcp: PDhcpPacket; len: PtrInt): boolean;

/// parse a raw DHCP binary frame and encode it as a JSON object
function DhcpParseToJson(dhcp: PDhcpPacket; len: PtrInt;
  extended: boolean = false): RawJson;

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

/// decode MAC or Eth=1 + MAC values stored at dhcp^.option[lens[opt]]
function DhcpMac(dhcp: PDhcpPacket; len: PtrUInt): PNetMac;
  {$ifdef FPC} inline; {$endif}

/// decode the lens[doParameterRequestList] within dhcp^.option[]
function DhcpRequestList(dhcp: PDhcpPacket; const lens: TDhcpParsed): TDhcpOptions;

type
  /// decoded DHCP options for PXE network booting process and its chain load
  // - as parsed by TDhcpProcess.SetRecvBoot() and stored in TDhcpState.Boot
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

  /// internal resultset used by ParseMacIP() for storing "static" values
  TMacIP = record
    ip: TNetIP4;
    mac: TNetMac;
    uuid: RawByteString;
  end;

  /// define TRuleValue content as DHCP "rules" condition or action
  // - those items are ordered by the most used at runtime first
  // - pvkMac as "mac" value condition in the mac inlined field
  // - pvkOpt is a known option as DHCP_OPTION_NUM[TDhcpOption(mac[0])] = num
  // - pvkRai as Relay-Agent-Information sub-option condition like "circuit-id"
  // - pvkBoot as "boot" value condition
  // - pvkRaw is DHCP option outside of TDhcpOption - stored as plain num
  // - pvkMacs as array of "mac" values condition in the TNetMacs value field
  // - pvkTlv stores raw Type-Length-Value (TLV) with num = sub-option
  // - pvkAlways to store a binary blob from "always" JSON definition
  TRuleValueKind = (
    pvkMac,
    pvkOpt,
    pvkRai,
    pvkBoot,
    pvkRaw,
    pvkMacs,
    pvkTlv,
    pvkAlways,
    pvkUndefined);

  /// store one DHCP "rules" condition or action
  // - used for "all" "any" "not-all" "not-any" conditions against a value
  // - used as "always" and "requested" data to be sent back to the client
  // - kind/num/mac fields order matters to access PInt64(one)^ shr 16 = Mac64
  TRuleValue = packed record
    /// define which kind of value is stored in this entry
    // - is used by TDhcpState.MatchOne as byte-code-like efficient runtime
    kind: TRuleValueKind;
    /// the raw (sub-)option number/ordinal to match, or to be sent back
    // - is the DHCP option number for kind=pvkRaw/pvkOpt
    // - is the TLV sub-option number for kind=pckTlv
    // - is a TDhcpClientBoot ordinal for pvkBoot match
    // - is a TDhcpOptionRai ordinal for pvkRai match
    // - is the DHCP option number to be checked and included as "requested"
    // - not used (contains 0) for pckAlways and pvkMac/pvkMacs
    num: byte;
    /// the CPU-cache-friendly inlined pvkMac value
    // - for pvkOpt, TDhcpOption(mac[0]) store the known option
    mac: TNetMac;
    /// the associated raw binary value
    value: RawByteString;
  end;
  /// points to one DHCP "rules" entry and its associated value
  PRuleValue = ^TRuleValue;

  /// store one DHCP "rules" entry and its associated value
  TRuleValues = array of TRuleValue;

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
function IsValidTlv(op: PAnsiChar; len: integer): boolean;
function IsValidRfc3925(op: PAnsiChar; len: integer): boolean;
function TlvOptionToJson(opt: pointer; recognize: boolean): RawJson;

/// convert a DNS wire format aka binary "canonical form" into plain ASCII text
procedure DnsLabelToText(v: PByteArray; len: PtrInt; var dest: shortstring);

/// parse a CIDR route(s) text into a RFC 3442 compliant binary blob
// - expect '192.168.1.0/24,10.0.0.5,10.0.0.0/8,192.168.1.1' readable format
function CidrRoutes(p: PUtf8Char; var bin: RawByteString): boolean;


{ **************** Low-Level per-scope DHCP metrics }

type
  /// all available per-scope DHCP metrics
  // -  - Protocol Messages
  // - dsmDiscover: DISCOVER packet is successfully received and processed
  // - dsmOffer: OFFER is sent in response to a DISCOVER
  // - dsmRequest: REQUEST is received and valid
  // - dsmAck: ACK is sent in response to REQUEST or INFORM
  // - dsmNak: NAK is sent in response to REQUEST with invalid or unavailable IP
  // - dsmInform: INFORM is received and processed
  // - dsmRelease: RELEASE is received and a lease is freed
  // - dsmDecline: DECLINE is received and IP is marked declined/unavailable
  // -  - Lease Lifecycle
  // - dsmLeaseAllocated: new dynamic lease is allocated to a client
  // - dsmLeaseRenewed: existing lease is renewed via REQUEST
  // - dsmLeaseExpired: lease expires (OnIdle cleanup)
  // - dsmLeaseReleased: lease is freed due to RELEASE
  // -  - Static vs Dynamic Hits
  // - dsmStaticHits: static reservation (MAC or option 61) is used to assign an IP
  // - dsmDynamicHits: dynamic lease (from the pool) is used
  // -  Rate limiting / abuse
  // - dsmRateLimitHit: DECLINE packet is ignored due to rate limiting
  // - dsmInformRateLimitHit: INFORM packet is ignored due to rate limiting
  // - dsmInvalidRequest: received packet is malformed or cannot be processed
  // - dsmUnsupportedRequest: only DISCOVER/REQUEST/DECLINE/RELEASE/INFORM are handled
  // -  - Option Usage Counters
  // - dsmOption50Hits: option 50 requested-address is present and used to setup IP
  // - dsmOption61Hits: option 61 client-identifier is present and used to assign/lookup a lease
  // - dsmOption82Hits: option 82 relay-agent-information is present and processed
  // - dsmOption82SubnetHits: option 82 link-selection sub-option is present and processed
  // - dsmOption118Hits: option 118 subnet-selection is present and valid
  // - dsmPxeBoot: PXE/iPXE options have been returned for a given architecture
  // - - Drop / Error Reasons
  // - dsmDroppedPackets: incremented for any packet dropped silently (not matching a scope or giaddr)
  // - dsmDroppedNoSubnet: packet has no matching subnet for its giaddr or option 82/118
  // - dsmDroppedNoAvailableIP: the IPv4 range of a subnet is exhausted
  // - dsmDroppedInvalidIP: packet requests an IP that is already in use or invalid
  // - dsmDroppedCallback: the OnComputeResponse callback aborted this request
  // - dsmDroppedPxeBoot: there was no proper configuration for this PXE/iPXE
  // - dsmDroppedTooManyOptions: avoid a buffer overflow with too many options
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
    dsmDroppedPxeBoot,
    dsmDroppedTooManyOptions);
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
  // - State/RateLimit/Mac fields should be in this order to match expectations
  // - Benchmark of O(n) single core logic shows it depends only on CPU cache:
  // $    200 DHCP renewals in 31us i.e. 6.1M/s, aver. 155ns
  // $   1000 DHCP renewals in 275us i.e. 3.4M/s, aver. 275ns
  // $   2000 DHCP renewals in 854us i.e. 2.2M/s, aver. 427ns
  // $   5000 DHCP renewals in 4.55ms i.e. 1M/s, aver. 910ns
  // $  10000 DHCP renewals in 16.87ms i.e. 578.7K/s, aver. 1.68us
  // $  20000 DHCP renewals in 65.01ms i.e. 300.4K/s, aver. 3.25us
  // $  40000 DHCP renewals in 179.98ms i.e. 217K/s, aver. 4.49us
  // $  60000 DHCP renewals in 573.57ms i.e. 102.1K/s, aver. 9.55us
  // $ 100000 DHCP renewals in 1.61s i.e. 60.5K/s, aver. 16.12us
  // which exceeds actual network capatibility up to 100k leases - we could add
  // a TDynArrayHasher index but it seems not worth it yet
  TDhcpLease = packed record
    /// how this entry should be handled
    State: TLeaseState;
    /// how many lsUnavailable IPs have been marked since the last OnIdle
    // - OnIdle() will reset it to 0, ComputeResponse() will increment it, and
    // silently ignore any abusive DECLINE/INFORM requests as rate limitation
    // - also align the whole structure to cpu-cache-friendly 16 bytes
    RateLimit: byte;
    /// the MAC address of this entry - access as PInt64(lease)^ shr 16 = Mac64
    // - may contain 0 for a lsFree or lsUnavailable sentinel entry
    Mac: TNetMac;
    /// the 32-bit reserved IP
    IP4: TNetIP4;
    /// the GetTickSec monotonic value stored as 32-bit unsigned integer
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
  // - used for TDhcpState.Boot: TDhcpClientBoot <> dcbDefault
  // - Remote[] are inherited for proper fallback between boot options,
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

  /// store one DHCP "rules" object in a ready-to-be-processed way
  // - pre-compiled at runtime from TDhcpRuleSettings JSON fields
  TDhcpScopeRule = record
    /// store AND matching fields
    all: TRuleValues;
    /// store OR matching fields
    any: TRuleValues;
    /// store NOT AND matching fields
    notall: TRuleValues;
    /// store NOT OR matching fields
    notany: TRuleValues;
    /// if this rule should reserve a static "ip" for these conditions
    // - usually, there is a "mac": in "all" fields with an associated IPv4
    ip: TNetIP4;
    /// the set of options part of "always"
    always: TDhcpOptions;
    /// the "always" and "requested" data to be sent back to the client
    // - "always" is stored as send[0].kind=pvkAlways
    send: TRuleValues;
    /// optional identifier used in the logs (not in the internal logic itself)
    name: RawUtf8;
  end;
  PDhcpScopeRule = ^TDhcpScopeRule;
  /// ready-to-be-processed DHCP "rules" objects of a given scope
  TDhcpScopeRules = array of TDhcpScopeRule;

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
    dsoPxeDisable);
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
    /// the server IP of this scope for doServerIdentifier option 54
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
    // - stored as RawByteString = doClientIdentifier-binary + 4-bytes-IP
    StaticUuid: TRawByteStringDynArray;
    /// store all DHCP "rules" ready-to-be-processed objects for this scope
    Rules: TDhcpScopeRules;
    /// readjust all internal values according to to Subnet policy
    // - raise an EDhcp exception if the parameters are not correct
    procedure AfterFill(log: TSynLog);
    /// register another static IP address to the internal pool
    // - can be associated with a fixed MAC address or option 61 UUID
    function AddStatic(var nfo: TMacIP): boolean; overload;
    /// register another static IP address to the internal pool
    // - value is expected to be supplied as 'ip', 'mac=ip' or 'uuid=ip' text
    function AddStatic(const macip: RawUtf8): boolean; overload;
    /// add one entry in "rules" JSON object format
    // - supplied as a concatenation of strings, to create a JSON object
    // - will create a transient TDhcpRuleSettings instance and inject it
    // to Rules[] in a thread-safe way, or raise EDhcp on error
    // - return the index of the newly created entry in Rules[]
    function AddRule(const json: array of RawByteString): PtrInt; overload;
    /// add one entry in Rules[] array within Safe.Lock/UnLock
    // - return the index of the newly created entry in Rules[]
    function AddRule(one: TDhcpScopeRule): PtrInt; overload;
    /// remove a given entry in Rules[] by index - for testing (not thread-safe)
    function DeleteRule(index: PtrInt): boolean;
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
    function TextWrite(W: TTextWriter; tix32: cardinal; time: TUnixTime): integer;
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


{ **************** Middle-Level DHCP State Machine }

type
  {$ifdef CPUINTEL} {$A-} {$endif CPUINTEL}
  /// state machine used by TDhcpProcess.ComputeResponse to process a request
  // - stack allocation of around 3KB memory needed for the frame processing
  // - also supplied to TOnComputeResponse callback as thread-safe context
  // - caller should set Data.Recv/RecvLen/RecvIp4/Tix32 at input
  {$ifdef USERECORDWITHMETHODS}
  TDhcpState = record
  {$else}
  TDhcpState = object
  {$endif USERECORDWITHMETHODS}
  public
    /// the network subnet information related to this request
    // - Scope^.Safe.Lock has been done by TOnComputeResponse callback caller
    Scope: PDhcpScope;
    /// length of the Recv UDP frame received from the client
    RecvLen: {$ifdef CPUINTEL} word {$else} cardinal {$endif};
    // length of the Send UDP frame after Flush
    SendLen: {$ifdef CPUINTEL} word {$else} cardinal {$endif};
    /// 32-bit binary IP address allocated for Send
    Ip4: TNetIP4;
    /// binary MAC address from the Recv frame, zero-extended to 64-bit/8-bytes
    Mac64: Int64;
    /// points to the last option of Send buffer
    // - callback could use this to call DhcpAddOption() overloads
    SendEnd: PAnsiChar;
    /// points to the raw value of doHostName option 12 in Recv.option[]
    // - points to @NULCHAR so HostName^='' if there is no such option
    RecvHostName: PShortString;
    /// the "rules" entry matched by this request
    RecvRule: PDhcpScopeRule;
    /// the server IP socket which received the UDP frame
    // - allow several UDP bound server sockets to share a single TDhcpProcess
    RecvIp4: TNetIP4;
    /// the DHCP message type parsed from Recv
    RecvType: TDhcpMessageType;
    /// the PXE remote boot type as parsed by TDhcpProcess.SetRecvBoot
    RecvBoot: TDhcpClientBoot;
    /// the DHCP message type prepared into the Send buffer
    SendType: TDhcpMessageType;
    /// options set into the Send buffer by TDhcpProcess.Flush
    SendOptions: TDhcpOptions;
    /// parsed options length position in Recv.option[]
    RecvLens: TDhcpParsed;
    /// option-82 RAI parsed sub-options length position in Recv.option[]
    RecvLensRai: TDhcpParsedRai;
    /// UDP frame received from the client, parsed in RecvLens[]
    Recv: TDhcpPacket;
    /// UDP frame to be sent back to the client, after processing
    Send: TDhcpPacket;
    /// the GetTickSec current 32-bit value - as set by ComputeResponse() caller
    Tix32: cardinal;
    /// the QueryPerformanceMicroSeconds() start value
    // - only set if dsoVerboseLog is defined in server Options
    StartMicroSec: Int64;
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
    /// high-level 'match and append' of all "rules" entries into Send
    procedure AddRulesOptions;
    // append regular DHCP 1,3,6,15,28,42 [+ 51,58,59] options
    procedure AddRegularOptions;
    /// append verbatim options 61/82 copy + end Send buffer stream
    // - returns the number of bytes of response in Send[]
    function Flush: PtrUInt;
  public
    /// parse Recv/RecvLen input fields into Mac/Mac64/RecvLens/RecvLensRai
    function Parse: boolean;
    /// return a pointer to one Recv.option[] value length
    function Data(const opt: TDhcpOption): pointer;
      {$ifdef HASINLINE} inline; {$endif}
    /// raw append of a 32-bit big-endian/IPv4 option value into Send
    procedure AddOptionOnce32(const opt: TDhcpOption; be: cardinal);
      {$ifdef FPC} inline; {$endif}
    /// raw append of a RawByteString/RawUtf8 option into Send
    procedure AddOptionOnceU(const opt: TDhcpOption; const v: PAnsiChar);
    /// raw append of a dynamic array of 32-bit big-endian/IPv4 options into Send
    procedure AddOptionOnceA32(const opt: TDhcpOption; const v: PAnsiChar);
    /// raw append the options of a given "rules" entry into Send
    procedure AddOptionFromRule(p: PRuleValue);
      {$ifdef HASINLINE} inline; {$endif}
    /// raw append a verbatim copy of a Recv option into Send
    procedure AddOptionCopy(opt: TDhcpOption; dsm: TDhcpScopeMetric = dsmDiscover);
      {$ifdef HASINLINE} inline; {$endif}
    /// raw search of one "rules" value in State.Recv[]
    function MatchOne(one: PRuleValue): boolean;
    /// serialize the Recv/RecvType/RecvLens/RecvLensRai fields as a JSON object
    function RecvToJson(extended: boolean = false): RawJson;
    /// serialize the Send/SendLen fields as a JSON object
    function SendToJson(extended: boolean = false): RawJson;
    /// for testing: wrapper to DhcpClient() on the State.Recv[] buffer
    // - then call e.g. DhcpAddOption32/Raw/Buf/U/Short() functions
    // - warning: won't make Tix32 := GetTickSec
    function ClientNew(dmt: TDhcpMessageType; const addr: TNetMac;
      req: TDhcpOptions = DHCP_REQUEST): PAnsiChar;
    /// for testing: properly end ClientNew() buffer with #255 and compute RecvLen
    procedure ClientFlush(current: PAnsiChar);
  private
    // methods for internal use
    procedure AddOptionSafe(const op: byte; b: pointer; len: PtrUInt);
      {$ifdef HASINLINE} inline; {$endif}
    procedure AddOption81;
    procedure ParseRecvLensRai;
    function FakeLease(found: TNetIP4): PDhcpLease;
      {$ifdef HASINLINE} inline; {$endif}
    function ClientUuid(opt: TDhcpOption): PDhcpLease;
    procedure AppendToMac(ip4len: PtrUInt; const ident: ShortString);
  end;
  {$ifdef CPUINTEL} {$A+} {$endif CPUINTEL}
  /// pointer to one TDhcpProcess.ComputeResponse state machine
  PDhcpState = ^TDhcpState;


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
    // - will also complete/inherit configuration from sibling values
    procedure PrepareBoot(var Data: TDhcpScopeBoot; Options: TDhcpScopeOptions);
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

  /// define a rule to "match and send" options for a given scope/subnet
  // - i.e. settings for vendor-specific or client-specific DHCP options
  // - each RawJson property contain a JSON object as key/value pairs,
  // potentially in our enhanced format e.g. with unquote keys
  // - "all" "any" "not-all" "not-any" should all match to trigger the output,
  // defining a "first precise rule wins" deterministic behavior in the order
  // in the array of "rules"
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
  TDhcpRuleSettings = class(TSynPersistent)
  protected
    fName, fIP, fMac: RawUtf8;
    fAll, fAny, fNotAll, fNotAny, fAlways, fRequested: RawJson;
  public
    /// compute the low-level TDhcpScope.Rules[] entry from current settings
    // - raise an EDhcp exception if the parameters are not correct
    // - return true if the Rule can be added to the scope
    function PrepareRule(var Data: TDhcpScope; var Rule: TDhcpScopeRule): boolean;
  published
    /// human-friendly identifier, not used in the internal logic
    // - added in the logs as "rule=<name>", or as useful comment in settings
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
    /// root "mac" as convenient alternative to {"all":{"mac":"xxxxx"}} entry
    // - so that you could write meaningful registration like:
    // $ {
    // $   "name": "printer-2",
    // $   "mac": "00:11:22:33:44:56",
    // $   "ip": "192.168.1.51",
    // $   "always": {
    // $     "domain-name": "printers.local"
    // $   }
    // $ }
    // - would work also to customize options, without any "ip" reservation
    // - exclusive to "all" "any" "not-all" "not-any" member - would raise EDhcp
    property Mac: RawUtf8
      read fMac write fMac;
    /// reserve this static "ip" for a given MAC or client-specific options
    // - could be used as an alternative to the main "static" array of TMacIP,
    // especially if you expect "always"/"requested" custom options sent back
    property IP: RawUtf8
      read fIP write fIP;
  end;
  /// a dynamic array of "match and send" options Rules
  // - store any number of vendor-specific or client-specific DHCP options
  TDhcpRuleSettingsObjArray = array of TDhcpRuleSettings;

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
    fRules: TDhcpRuleSettingsObjArray;
  public
    /// setup this instance with default values
    // - default are subnet-mask = '192.168.1.1/24', lease-time-seconds = 120
    // and OfferHoldingSecs = 5, consistent with a simple local iPXE network
    constructor Create; override;
    /// append at runtime a new scope/subnet "rules" settings
    // - call without any TDhcpRuleSettings parameter to create a default one
    // - supplied one will be owned by this instance from now on
    function AddRule(one: TDhcpRuleSettings = nil): TDhcpRuleSettings;
    /// compute the low-level TDhcpScope data structure for current settings
    // - raise an EDhcp exception if the parameters are not correct
    procedure PrepareScope(Sender: TDhcpProcess; var Data: TDhcpScope);
  published
    /// Subnet Mask (option 1) e.g. "subnet-mask": "192.168.1.1/24"
    // - the CIDR 'ip/mask' pattern will compute range-min/range-max and
    // server-identifier directly from this pattern
    // - accept also plain '255.255.255.0' IP if you want to specify by hand the
    // raw value sent in DHCP headers, and fill range-min/range-max and others
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
    // - default to 0, to reuse the same value than lease-time-seconds
    // - if lease-time-seconds is small, you could set a bigger value here to be
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
    // - default is "grace-factor":2, meaning that for PXE with lease-time-seconds
    // of 120 (< 1 hour), a grace period of 240 seconds
    // - this grace delay is disabled if GraceFactor = 0 or for long leases > 1h
    property GraceFactor: cardinal
      read fGraceFactor write fGraceFactor default 2;
    /// refine DHCP server process for this scope
    // - default is [] but you may tune it for your actual (sub-)network needs
    // using "inform-rate-limit", "csv-unix-time", "pxe-no-inherit" and
    // "pxe-disable" in this "options" array
    property Options: TDhcpScopeOptions
      read fOptions write fOptions;
    /// optional PXE network book settings as "boot" sub-object
    property Boot: TDhcpBootSettings
      read fBoot;
    /// vendor-specific or client-specific DHCP options for this scope as
    // "rules" array of JSON objects
    // - order in this array defines "first rule wins" deterministic behavior
    property Rules: TDhcpRuleSettingsObjArray
      read fRules;
  end;
  /// a dynamic array of DHCP Server scope/subnet settings
  TDhcpScopeSettingsObjArray = array of TDhcpScopeSettings;

  /// how to refine DHCP server process globally for all scopes
  // - dsoSystemLog would call JournalSend() in addition to default TSynLog -
  // but it will slowdown the process a lot, so should be used with caution
  // - dsoNoFileBak would disable the '.bak' renaming during SaveToFile()
  // - dsoVerboseLog will include input/output frames to the log as JSON
  TDhcpServerOption = (
    dsoSystemLog,
    dsoNoFileBak,
    dsoVerboseLog);
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
    /// if set, OnIdle will persist the leases list into this file when needed
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

  /// optional callback signature for TDhcpProcess.ComputeResponse
  // - input frame is parsed in Data.Recv/RecvLen/RecvLens/RecvLensRai
  // - could update SendEnd with DhcpAddOption() or set SendEnd=nil for no response
  TOnComputeResponse = procedure(Sender: TDhcpProcess; var State: TDhcpState);

  /// implements our DHCP server logic, abstracted from UDP/Socket reference
  // - main methods are Setup() and ComputeResponse() from TDhcpServer
  // - store and redirect all process to TDhcpScopes in-memory per-subnet lists
  // - several sockets could be bound in TDhcpServer, each with its own IP, and
  // on its own interface, all sharing this single TDhcpProcess subnets
  // - easily extensible by overriding its core protected virtual methods
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
    fLogPrefix: RawUtf8;
    function GetCount: integer;
    procedure SetFileName(const name: TFileName);
    procedure SetMetricsFolder(const folder: TFileName);
    // ComputeResponse() virtual sub-methods for proper logic customization
    procedure DoLog(Level: TSynLogLevel; const Context: ShortString;
      const State: TDhcpState); virtual;
    function DoError(var State: TDhcpState; Context: TDhcpScopeMetric;
      Lease: PDhcpLease = nil): PtrInt; virtual;
    procedure OnLogFrame(Sender: TSynLog; Level: TSynLogLevel; Opaque: pointer;
      Value: PtrInt; Instance: TObject); virtual;
    function LockedResponse(var State: TDhcpState): PtrInt; virtual;
    function ParseFrame(var State: TDhcpState): boolean; virtual;
    function FindScope(var State: TDhcpState): boolean; virtual;
    function FindLease(var State: TDhcpState): PDhcpLease; virtual;
    function RetrieveFrameIP(var State: TDhcpState;
      Lease: PDhcpLease): boolean; virtual;
    procedure SetRecvBoot(var State: TDhcpState); virtual;
    procedure AddBootOptions(var State: TDhcpState); virtual;
    procedure SetRule(var State: TDhcpState); virtual;
    function RunCallbackAborted(var State: TDhcpState): boolean; virtual;
    function Flush(var State: TDhcpState): PtrInt; virtual;
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
    // - will maintain a .bak atomic copy unless dsoNoFileBak option is set
    function SaveToFile(const FileName: TFileName): integer;
    /// restore the internal entry list using SaveToText() format
    // - should be done before Setup() to validate the settings network mask
    function LoadFromFile(const FileName: TFileName): boolean;
    /// aggregate all per-scope Total metrics into a single counter
    procedure ComputeMetrics(var global: TDhcpMetrics); overload;
    /// aggregate all per-scope metrics into a single set of Current/Total counters
    procedure ComputeMetrics(var global: TDhcpAllMetrics); overload;
    /// reset back all per-scope metrics to their initial 0 counter value
    procedure ResetMetrics;
    /// persist the main metrics as JSON object
    // - wrapper around ComputeMetrics() and MetricsToJson()
    function SaveMetricsToJson: RawUtf8;
    /// persist all scopes metrics as .json files and optionally all .csv files
    procedure SaveMetricsFolder(AndCsv: boolean = false);
    /// this is the main processing function of the DHCP server logic
    // - caller should have set Data.Recv/RecvLen/RecvIp4/Tix32 - kept untouched
    // - returns -1 if this input was invalid or unsupported
    // - returns 0 if input was valid, but no response is needed (e.g. decline)
    // - return > 0 the number of Data.Send[] bytes to broadcast back as UDP
    function ComputeResponse(var State: TDhcpState): PtrInt;
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
    /// can replace 'ComputeResponse: ' default header for DoLog() events
    property LogPrefix: RawUtf8
      read fLogPrefix write fLogPrefix;
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
var
  i: PtrInt;
begin
  result := GetEnumNameValue(TypeInfo(TDhcpOption), V, opt, @DHCP_OPTION);
  if result and                           // e.g. 'leasetime' or 'lease-time'
     (V <> '') then
    exit;
  i := FindNonVoidRawUtf8I(@DHCP_OPTION_ALTERNATE, pointer(V), length(V),
         length(DHCP_OPTION_ALTERNATE)); // e.g. 'dhcp-lease-time'
  if i < 0 then
    exit;
  opt := TDhcpOption(i + ord(low(DHCP_OPTION_ALTERNATE))); // adjust
  result := true;
end;

function FromText(const V: RawUtf8; out opt: TDhcpOptionRai): boolean;
begin
  result := GetEnumNameValue(TypeInfo(TDhcpOptionRai), V, opt, @RAI_OPTION);
end;

const
  DHCP_OPTION_NUM: array[TDhcpOption] of byte = (
    0, 1, 3, 6, 12, 15, 28, 42, 43, 50, 51, 53, 54, 55,
    58, 59, 60, 61, 66, 67, 77, 81, 82, 93, 97, 118, 255);
  RAI_OPTION_NUM: array[TDhcpOptionRai] of byte = (
    0, 1, 2, 5, 6, 11, 12, 19);
var // fast O(1) inverse lookup from (sub-)option numbers to know enumerate item
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
  PCardinal(p)^ := DHCP_OPTION_NUM[doParameterRequestList]; // op + len=0
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
  FillCharFast(dhcp, DHCP_PACKET_HEADER, 0);
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
  DhcpAddOptionByte(result, {doMessageType=} 53, ord(dmt));
  if serverid <> 0 then
  begin
    dhcp.siaddr := serverid;
    DhcpAddOption32(result, doServerIdentifier, serverid);
  end;
  result^ := #255;
end;

function DhcpClient(var dhcp: TDhcpPacket; dmt: TDhcpMessageType;
  const addr: TNetMac; req: TDhcpOptions): PAnsiChar;
begin
  result := DhcpNew(dhcp, dmt, {xid=}0, addr);
  if req <> [] then
    result := DhcpAddOptionRequestList(result, req);
  result^ := #255; // only for internal/debug use
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
  result := false;
  if len <> 0 then
    result := IdemPropName(PShortString(@dhcp^.options[len])^, P2);
end;

function DhcpIP4(dhcp: PDhcpPacket; len: PtrUInt): TNetIP4;
begin
  result := len;
  if len = 0 then
    exit;
  len := PtrUInt(@dhcp.options[len]);
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
  len := PtrUInt(@dhcp.options[len]);
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

function DhcpParseHeader(dhcp: PDhcpPacket; len: PtrInt): boolean;
  {$ifdef FPC} inline; {$endif}
begin
  result := (dhcp <> nil) and
            (len >= DHCP_PACKET_HEADER) and
            (len <= SizeOf(dhcp^)) and
            (dhcp^.cookie = DHCP_MAGIC_COOKIE) and
            (dhcp^.htype = ARPHRD_ETHER) and
            (dhcp^.hlen = SizeOf(TNetMac)) and
            (dhcp^.xid <> 0) and
            (dhcp^.op in [BOOT_REQUEST, BOOT_REPLY]);
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
  if not DhcpParseHeader(dhcp, len) then
    exit;
  // parse DHCP options store as Type-Length-Value (TLV)
  dec(len, DHCP_PACKET_HEADER);
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
    inc(p);
    if opt <> doPad then
    begin
      if found <> nil then
        include(found^, opt);
      lens[opt] := p - PAnsiChar(@dhcp^.options); // p[0]=len p[1]=value
    end;
    // just ignore unsupported options
    p := @p[ord(p^) + 1];
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
      m := @dhcp^.chaddr; // option 61 is no MAC: fallback to BOOTP value
    mac^ := m^; // copy
  end;
  result := TDhcpMessageType(dmt);
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
      opt := doPad;
      if PByte(p)^ <= high(DHCP_OPTION_INV) then
        opt := DHCP_OPTION_INV[PByte(p)^]; // fast O(1) option number lookup
      if opt <> doPad then
        include(result, opt);
      dec(n);
    until n = 0;
end;

const
  CLIENT_SERVER: array[0..1] of RawUtf8 = ('client', 'server');
  RFC4702_FLAG_S = 1;
  RFC4702_FLAG_E = 4;

function Rfc4702FromJson(p: PUtf8Char; out v: RawByteString): boolean;
var
  d: PAnsiChar;
  parser: TJsonParserContext;
  s: byte;
begin
  // encode a RFC 4702 client/server:"fqdn" response value with E=0=ASCII
  result := false;
  if p = nil then
    exit;
  parser.InitParser(p);
  if not parser.ParseObject or
     not parser.GetJsonFieldName or
     not parser.ValueEnumFromConst(@CLIENT_SERVER, 2, s) or // s=0/1
     not parser.ParseNext or
     not parser.WasString or
     (parser.ValueLen = 0) then
    exit;
  d := FastNewRawByteString(v, parser.ValueLen + 3);
  PCardinal(d)^ := s * RFC4702_FLAG_S;    // flags=S=0/1 + rcode1=rcode2=0
  MoveFast(parser.Value^, d[3], parser.ValueLen); // append as ASCII (E=0)
  result := true;
end;

type
  TParseRule = (prMatch, prValue, prTlv);
  TParseType = (ptTextBin, ptIp4, ptMac, ptUuid, ptUuid97, ptBool,
                ptUInt8, ptUInt16, ptUInt32, ptUInt64,
                ptVendor43, ptMsg53, ptParam55, ptClient61, ptFqdn81, ptRelay82,
                ptCidr121, ptVendor12x);
const
  RULE_VALUE_PREFIX: array[0 .. 12] of PAnsiChar = (
    'IP:', 'MAC:', 'HEX:', 'BASE64:', 'UUID:', 'GUID:',
    'UINT8:', 'UINT16:', 'UINT32:', 'UINT64:', 'ESC:', 'CIDR:', nil);
  // O(1) lookup of raw DHCP option number to its value type
  PARSE_TYPE: array[0 .. 125] of TParseType = (
    ptTextBin, ptIp4, ptUInt32, ptIp4, ptIp4, ptIp4, ptIp4, ptIp4, ptIp4,
    ptIp4, ptIp4, ptIp4, ptTextBin, ptUInt16, ptTextBin, ptTextBin, ptIp4, // 16
    ptTextBin, ptTextBin, ptBool, ptBool, ptIp4, ptUInt16, ptUInt8, ptUInt32,
    ptUInt16, ptUInt16, ptBool, ptIp4, ptBool, ptBool, ptBool, ptIp4, ptIp4,
    ptBool, ptUInt32, ptBool, ptUInt8, ptUInt32, ptBool, ptTextBin, ptIp4, // 41
    ptIp4, ptVendor43, ptIp4, ptIp4, ptUInt8, ptTextBin, ptIp4, ptIp4,
    ptIp4, ptUInt32, ptUInt8, ptMsg53, ptIp4, ptParam55, ptTextBin,        // 56
    ptUInt16, ptUInt32, ptUInt32, ptTextBin, ptClient61, ptTextBin,
    ptTextBin, ptTextBin, ptIp4, ptTextBin, ptTextBin, ptIp4, ptIp4, ptIp4,
    ptIp4, ptIp4, ptIp4, ptIp4, ptIp4, ptIp4, ptTextBin, ptTextBin,        // 78
    ptTextBin, ptTextBin, ptFqdn81, ptRelay82, ptTextBin, ptTextBin, ptIp4,
    ptTextBin, ptTextBin, ptTextBin, ptIp4, ptTextBin, ptUInt32, ptIp4,    // 92
    ptUInt16, ptTextBin, ptTextBin, ptTextBin, ptUuid97, ptTextBin,
    ptTextBin, ptTextBin, ptTextBin, ptTextBin, ptTextBin, ptTextBin,     // 104
    ptTextBin, ptTextBin, ptTextBin, ptUInt32, ptTextBin, ptTextBin,
    ptTextBin, ptIp4, ptTextBin, ptTextBin, ptTextBin, ptUInt8, ptUInt16, // 117
    ptIp4, ptTextBin, ptTextBin, ptCidr121, ptTextBin, ptTextBin,
    ptVendor12x, ptVendor12x);                                            // 125
  FAKE_OP_MAC = 255; // to parse "mac": content

function ParseType(op: byte): TParseType;
  {$ifdef HASINLINE} inline; {$endif}
begin
  if op <= high(PARSE_TYPE) then
    result := PARSE_TYPE[op]
  else if op = 249 then
    result := ptCidr121 // Windows specific alias introduced for compatibility
  else if op = FAKE_OP_MAC then
    result := ptMac
  else
    result := ptTextBin;
end;

function SetRuleValue(var p: TJsonParserContext; var v: RawByteString;
  op: byte): boolean;
var
  tmp: THash128Rec;
  len: PtrInt;
  d: PAnsiChar;
label
  uuid97;
begin
  result := false;
  // optional in-place conversion of JSON array into CSV
  if not p.WasString and
     (p.ValueLen <> 0) and
     (p.Value^ = '[') then
    // e.g. "ntp-servers": ["ip:1.2.3.4", "1.2.3.5"] -> 'ip:1.2.3.4,1.2.3.5'
    p.Get.ValueLen := JsonArrayAsCsv(p.Value, ',', @p.Get.WasString);
  // actually decode the value
  len := p.ValueLen;
  if len = 0 then
  begin
    FastAssignNew(v);
    result := true;
    exit;
  end;
  if p.WasString then
    // handle "ip:..." .. "cidr:..." prefixes in string values
    case IdemPPChar(p.Value, @RULE_VALUE_PREFIX) of
      0:    // ip:
        result := ToIP4Binary(p.Value + 3, v) >= 0; // allow CSV of IPs
      1:    // mac:
        result := ToMacBinary(p.Value + 4, v) >= 0; // allow CSV of MACs
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
            goto uuid97; // special encoding as 17-bytes with type=#0 prefix
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
        // recognize configurable options in "string" value
        case ParseType(op) of
          ptIp4:
            begin
              result := ToIP4Binary(p.Value, v) >= 0; // allow CSV of IP4
              if result then
                exit;
            end;
          ptFqdn81:
            begin
              // set a RFC 4702 {client:"fqdn"} response value with flags=0
              d := FastNewRawByteString(v, p.ValueLen + 3);
              PCardinal(d)^ := 0; // flags=rcode1=rcode2=0
              MoveFast(p.Value^, d[3], p.ValueLen); // E=0=ASCII
              result := true;
              exit;
            end;
          ptUuid97:
            begin
              result := RawUtf8ToGuid(p.Value, len, tmp.guid);
              if result then
              begin
uuid97:         d := FastNewRawByteString(v, 17); // specific to RFC 4578 (PXE)
                d^ := #0;                         // type=#0 prefix
                PGuid(d + 1)^ := tmp.guid;
                exit;
              end;
            end;
          ptMac: // fake call with op=255 to parse "mac": content
            begin
              result := ToMacBinary(p.Value, v) >= 0; // allow CSV of MACs
              exit;
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
    // handle wasString = false: number, boolean or nested TLV object
    case p.Value[0] of
      '0' .. '9':
        begin
          // recognize most JSON number size from configurable options
          tmp.c0 := GetCardinal(p.Value);
          case ParseType(op) of
            ptUInt8:
              len := 1;
            ptUInt16:
              begin
                tmp.c0 := bswap16(tmp.c0);
                len := 2;
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
          FastSetRawByteString(v, @tmp.b, 1); // stored 0/1 byte
          result := true;
        end;
      '{':
        case op of
          81:
            // support {client:"fqdn"} or {server:"fqdn"} format
            result := Rfc4702FromJson(p.Value, v);
        else
         // try to parse a JSON object into a TLV value
          result := TlvFromJson(p.Value, v);
        end;
    end;
end;

function DhcpOptName(op: byte): PRawUtf8; // PRawUtf8 to avoid try..finally
  {$ifdef HASINLINE} inline; {$endif}
var
  opt: TDhcpOption;
begin
  opt := doPad;
  if op <= high(DHCP_OPTION_INV) then
    opt := DHCP_OPTION_INV[op];
  if opt = doPad then
    result := @SmallUInt32Utf8[op]
  else
    result := @DHCP_OPTION[opt];
end;

procedure AddTextBinJson(v: PAnsiChar; len: integer; W: TJsonWriter);
begin
  W.AddDirect('"');
  if IsValidUtf8WithoutControlChars(pointer(v), len) then
    W.AddJsonEscape(v, len)    // ASCII/UTF-8
  else if len <= 32 then
    W.AddBinToHumanHex(v, len) // e.g. for MAC or UUID
  else
  begin
    W.AddDirect('h', 'e', 'x', ':');
    W.AddBinToHex(v, len, {lower=}true);
  end;
  W.AddDirect('"');
end;

procedure AddTlvJson(v: PAnsiChar; len: integer; W: TJsonWriter; rai: boolean);
var
  l: PtrInt;
begin
  W.AddDirect('{');
  repeat
    if rai and
       (ord(v[0]) <= high(RAI_OPTION_INV)) then
      W.AddFieldName(RAI_OPTION[RAI_OPTION_INV[ord(v[0])]])
    else
      W.AddPropName(ord(v[0]));
    AddTextBinJson(@v[2], ord(v[1]), W);
    l := ord(v[1]) + 2;
    dec(len, l);
    if len = 0 then
      break;
    inc(v, l);
    W.AddComma;
  until false;
  W.AddDirect('}');
end;

function IsValidTlv(op: PAnsiChar; len: integer): boolean;
begin
  result := false;
  repeat
    dec(len, 2);          // type + len
    if len < 0 then
      exit;
    dec(len, ord(op[1])); // value
    if len = 0 then
      break;
    op := @op[ord(op[1]) + 2];
  until false;
  result := true;
end;

function IsValidRfc3925(op: PAnsiChar; len: integer): boolean;
begin
  result := false;
  repeat
    dec(len, 5);          // entreprise-number + data-len
    if len < 0 then
      exit;
    dec(len, ord(op[4])); // jump vendor-data
    if len = 0 then
      break;
    op := @op[ord(op[4]) + 5];
  until false;
  result := true;
end;


procedure DnsLabelToText(v: PByteArray; len: PtrInt; var dest: shortstring);
var
  vlen: PtrInt;
begin
  // decode DNS label format: [len + part] + ending len=0
  dest[0] := #0;
  while len > 1 do // end with a final #0 length
  begin
    if v[0] > len then
      break; // avoid buffer overflow
    vlen := v[0] + 1;
    dec(len, vlen);
    if len <= 1 then
      AppendShortCharSafe('.', dest); // last item is domain extension ('com')
    AppendShortBuffer(PAnsiChar(v) + 1, v[0], high(dest), @dest);
    inc(PByte(v), vlen);
  end;
end;

procedure AddJsonWriterRfc4702(W: TJsonWriter; v: PAnsiChar; len: PtrInt);
var
  tmp: ShortString;
begin
  // https://kea.readthedocs.io/en/kea-3.1.4/arm/dhcp4-srv.html#ddns-for-dhcpv4
  // we support S+E flags; O=0 from a client; N is ignored (no DDNS anyway here)
  W.AddDirect('{'); // {client/server:"fqdn"}
  W.AddFieldName(CLIENT_SERVER[ord(v[0]) and RFC4702_FLAG_S]);
  W.AddDirect('"');
  dec(len, 3);
  if ord(v[0]) and RFC4702_FLAG_E = 0 then
    // E=0: plain ASCII
    inc(v, 3)
  else
  begin
    // E=1: wire format aka "canonical form"
    DnsLabelToText(@v[3], len, tmp);
    v := @tmp[1];
    len := ord(tmp[0]);
  end;
  W.AddJsonEscape(v, len);
  W.AddDirect('"', '}');
end;

procedure ParseTypeJson(tlv: PByteArray; W: TJsonWriter; recognize: boolean);
var
  len: cardinal;
  v: PAnsiChar;
begin
  len := tlv[1];
  if len = 0 then
  begin
    W.AddNull;
    exit;
  end;
  v := @tlv[2];
  // recognize and handle most common option types
  if recognize then
    case ParseType(tlv[0]) of
      ptIp4:
        if len and 3 = 0 then
        begin
          len := len shr 2;
          if len = 1 then                          // as JSON "1.2.3.4"
          begin
            AddJsonWriterIP4(W, v);
            exit;
          end;
          W.AddDirect('[');                        // as JSON array
          repeat
            AddJsonWriterIP4(W, v);
            dec(len);
            if len = 0 then
              break;
            inc(PNetIP4(v));
            W.AddComma;
          until false;
          W.AddDirect(']');
          exit;
        end;
      ptMac:
        if len = SizeOf(TNetMac) then              // as JSON string "xx:xx:.."
        begin
          W.AddBinToHumanHex(v, SizeOf(TNetMac), '"');
          exit;
        end
        else if len mod SizeOf(TNetMac) = 0 then   // as JSON array
        begin
          W.AddDirect('[');
          repeat
            W.AddBinToHumanHex(v, SizeOf(TNetMac), '"');
            dec(len, SizeOf(TNetMac));
            if len = 0 then
              break;
            inc(PNetMac(v));
            W.AddComma;
          until false;
          W.AddDirect(']');
          exit;
        end;
      ptVendor43, // most vendor-encapsulated-options use nested TLV
      ptRelay82:  // relay-agent-information sub-options are always TLV encoded
        if IsValidTlv(v, len) then
        begin
          AddTlvJson(v, len, W, tlv[0] = 82);      // as JSON object
          exit;
        end;
      ptMsg53:
        if len = 1 then
        begin
          W.AddJsonString(DHCP_TXT[TDhcpMessageType(v^)]);
          exit;
        end;
      ptParam55:
        begin
          W.AddDirect('[');                        // as JSON array of "names"
          repeat
            if twoForceJsonStandard in W.CustomOptions then
              W.AddJsonString(DhcpOptName(PByte(v)^)^)
            else
              W.AddB(PByte(v)^); // shorter and simple enough as extended/log
            dec(len);
            if len = 0 then
              break;
            inc(PByte(v));
            W.AddComma;
          until false;
          W.AddDirect(']');
          exit;
        end;
      ptClient61:
        if (len = 6) or
           ((len = 7) and
            (ord(v[0]) = ARPHRD_ETHER)) then
        begin
          if len = 7 then
            inc(v);
          W.AddBinToHumanHex(v, 6, '"');           // as JSON string "xx:xx:.."
          exit;
        end;
      ptFqdn81:
        if (len > 3) and                           // RFC 4702
           (v[1] = #0) and                         // rcode1 = 0
           (v[2] = #0) then                        // rcode2 = 0
        begin
          AddJsonWriterRfc4702(W, v, len);         // {client/server:"fqdn"}
          exit;
        end;
      ptVendor12x:
        if IsValidRfc3925(v, len) then             // 124/125
        begin
          W.AddDirect('{');                        // as JSON object
          repeat
            W.AddPropName(bswap32(PCardinal(v)^)); // entreprise-number
            inc(PCardinal(v));                     // data-len + vendor-data
            if IsValidTlv(v + 1, ord(v[0])) then
              AddTlvJson(v + 1, ord(v[0]), W, {rai=}false)
            else
              AddTextBinJson(v + 1, ord(v[0]), W);
            dec(len, ord(v[0]) + 5);
            if len <= 0 then
              break;
            v := @v[ord(v[0]) + 1];
            W.AddComma;
          until false;
          W.AddDirect('}');
          exit;
        end;
      ptUInt8:
        begin
          if len = 1 then
          begin
            W.AddB(PByte(v)^);
            exit;
          end;
          W.AddDirect('[');                        // as JSON array
          repeat
            W.AddB(PByte(v)^);
            dec(len);
            if len = 0 then
              break;
            inc(PByte(v));
            W.AddComma;
          until false;
          W.AddDirect(']');
          exit;
        end;
      ptUInt16:
        if len and 1 = 0 then
        begin
          len := len shr 1;
          if len = 1 then
          begin
            W.AddU(bswap16(PWord(v)^));
            exit;
          end;
          W.AddDirect('[');                        // as JSON array
          repeat
            W.AddU(bswap16(PWord(v)^));
            dec(len);
            if len = 0 then
              break;
            inc(PWord(v));
            W.AddComma;
          until false;
          W.AddDirect(']');
          exit;
        end;
      ptUInt32:
        if len and 3 = 0 then
        begin
          len := len shr 2;
          if len = 1 then
          begin
            W.AddU(bswap32(PCardinal(v)^));
            exit;
          end;
          W.AddDirect('[');                        // as JSON array
          repeat
            W.AddU(bswap32(PCardinal(v)^));
            dec(len);
            if len = 0 then
              break;
            inc(PCardinal(v));
            W.AddComma;
          until false;
          W.AddDirect(']');
          exit;
        end;
      ptBool:
        if len = 1 then
        begin
          W.Add(PBoolean(v)^);                     // will normalize value byte
          exit;
        end;
      ptUuid97:
        if len in [16, 17] then
        begin
          if len = 17 then
            inc(v);
          W.AddJsonShort(UuidToShort(PGuid(v)^));  // as "uuid-xxx-xxx"
          exit;
        end;
  end;
  // if we reached here, we have either binary or plain text
  AddTextBinJson(v, len, w);
end;

procedure DoDhcpToJson(W: TJsonWriter; p: PDhcpPacket; len: PtrInt; s: PDhcpState);
var
  o: PAnsiChar;
begin
  W.AddDirect('{');
  // main DHCP packet fields
  if p^.op = BOOT_REQUEST then
    W.AddPropJsonShort('op', 'request')
  else
    W.AddPropJsonShort('op', 'reply');
  if p^.ciaddr <> 0 then
    W.AddPropJsonShort('ciaddr', IP4ToShort(@p^.ciaddr));
  if p^.yiaddr <> 0 then
    W.AddPropJsonShort('yiaddr', IP4ToShort(@p^.yiaddr));
  if p^.siaddr <> 0 then
    W.AddPropJsonShort('siaddr', IP4ToShort(@p^.siaddr));
  if p^.giaddr <> 0 then
    W.AddPropJsonShort('giaddr', IP4ToShort(@p^.giaddr));
  if not IsZero(PNetMac(@p^.chaddr)^) then
    W.AddPropJsonShort('chaddr', MacToShort(@p^.chaddr));
  // parse and serialize all option fields in packet order
  dec(len, DHCP_PACKET_HEADER);
  o := @p^.options;
  repeat
    dec(len, ord(o[1]) + 2);
    if len < 1 then
      break; // avoid buffer overflow
    W.AddFieldName(DhcpOptName(ord(o[0]))^);
    ParseTypeJson(pointer(o), W, true);
    W.AddComma;
    o := @o[ord(o[1]) + 2];
  until o^ = #255;
  // end with main sllServer decoded/parsed state fields in fields
  if s <> nil then
  begin
    if s^.RecvIp4 <> 0 then
      W.AddPropJsonShort('via', IP4ToShort(@s^.RecvIp4));
    if (s^.RecvHostName <> nil) and
       (s^.RecvHostName^[0] <> #0) then
      W.AddPropJsonShort('host', s^.RecvHostName^);
    if s^.RecvBoot <> dcbDefault then
      W.AddPropJsonString('boot', BOOT_TXT[s^.RecvBoot]);
    if s^.Ip[0] <> #0 then
      W.AddPropJsonShort('ip', s^.Ip);
  end;
  W.CancelLastComma('}');
end;

function DhcpParseToJson(dhcp: PDhcpPacket; len: PtrInt; extended: boolean): RawJson;
var
  tmp: TTextWriterStackBuffer; // 8KB static
  W: TJsonWriter;
begin
  result := '';
  if not DhcpParseHeader(dhcp, len) then
    exit;
  W := TJsonWriter.CreateOwnedStream(tmp);
  try
    if extended then
      W.CustomOptions := [twoForceJsonExtended];
    DoDhcpToJson(W, dhcp, len, {state=}nil); // as a single JSON object
    W.SetText(RawUtf8(result));
  finally
    W.Free;
  end;
end;

function TlvOptionToJson(opt: pointer; recognize: boolean): RawJson;
var
  tmp: TTextWriterStackBuffer; // 8KB static
  W: TJsonWriter;
begin
  W := TJsonWriter.CreateOwnedStream(tmp);
  try
    W.CustomOptions := [twoForceJsonExtended];
    ParseTypeJson(opt, W, recognize);
    W.SetText(RawUtf8(result));
  finally
    W.Free;
  end;
end;

const
  MAIN_RULE_VALUE: array[0..1] of RawUtf8 = ('boot', 'mac');

function ParseRuleValue(var parser: TJsonParserContext; var v: TRuleValue;
  pp: TParseRule): boolean;
begin
  result := false;
  PInt64(@v)^ := 0; // set kind+num+mac=0
  v.kind := pvkUndefined;
  v.value := '';
  // parse and recognize "77": "user-class": "boot": "circuit-id": keys
  if not parser.GetJsonFieldName or
     (parser.ValueLen = 0) then
    exit;
  v.num := GetCardinal(parser.Value); // for pvkTlv and pvkOpt/pvkRaw
  if pp = prTlv then
    v.kind := pvkTlv // TlvFromJson() accept only numbers, not DHCP options
  else if v.num <> 0 then
    // "77": "iPXE"  or  77: "iPXE"
    if v.num <= high(DHCP_OPTION_INV) then
    begin
      TDhcpOption(v.mac[0]) := DHCP_OPTION_INV[v.num]; // known option
      v.kind := pvkOpt;
    end
    else
      v.kind := pvkRaw
  else if parser.ValueEnumFromConst(@DHCP_OPTION, length(DHCP_OPTION), v.mac[0]) then
  begin
    // "lease-time": 7200
    v.num := DHCP_OPTION_NUM[TDhcpOption(v.mac[0])];
    v.kind := pvkOpt;
  end
  else if parser.ValueEnumFromConst(@DHCP_OPTION_ALTERNATE,
            length(DHCP_OPTION_ALTERNATE), v.mac[0]) then
  begin
    // "dhcp-lease-time": 7200
    inc(v.mac[0], ord(low(DHCP_OPTION_ALTERNATE))); // adjust
    v.num := DHCP_OPTION_NUM[TDhcpOption(v.mac[0])];
    v.kind := pvkOpt;
  end
  else if pp <> prMatch then
    exit // prValue requires a v.num to be sent back as "always" or "requested"
  else
    // prMatch-only identifiers e.g. "boot" "mac" "circuit-id" "remote-id"
    case FindNonVoidRawUtf8I(@MAIN_RULE_VALUE, parser.Value, parser.ValueLen,
           length(MAIN_RULE_VALUE)) of
      0:
        begin
          // "boot": "ipxe-x64"
          v.kind := pvkBoot;
          result := parser.ParseNext and
                    parser.ValueEnumFromConst(@BOOT_TXT, length(BOOT_TXT), v.num) and
                    (v.num <> 0);
          exit; // we won't check v.value
        end;
      1:
        begin
          // "mac": "00:11:22:33:44:55" - also accepts CSV or arrays
          v.kind := pvkMacs;          // v.value contains TNetMacs binary
          result := parser.ParseNextAny(false) and
                    SetRuleValue(parser, v.value, FAKE_OP_MAC) and
                    (v.value <> '') and
                    (length(v.value) mod SizeOf(TNetMac) = 0);
          if length(v.value) = SizeOf(TNetMac) then
          begin
            v.kind := pvkMac;
            v.mac := PNetMac(v.value)^; // inlined single mac
            v.value := '';              // not needed any more
          end;
          exit;
        end;
    else
      if parser.ValueEnumFromConst(@RAI_OPTION, length(RAI_OPTION), v.num) and
         (v.num <> 0) then
          // "circuit-id": "pon1/1/3"
          v.kind := pvkRai
        else
          exit;
    end;
  // parse and recognize the associated value
  if (v.kind = pvkUndefined) or
     not parser.ParseNextAny({NormalizeBoolean=}false) then
    exit;
  if pp = prTlv then
    result := SetRuleValue(parser, v.value, 0) // v.num is no option
  else
    result := SetRuleValue(parser, v.value, v.num);
  if result and
     (length(v.value) > 255) then
    result := false; // avoid TLV 8-bit length overflow
end;

procedure AddRuleValue(var a: TRuleValues; var v: TRuleValue);
var
  n: PtrInt;
begin
  if v.kind = pvkUndefined then
    EDhcp.RaiseU('Unexpected AddRuleValue()'); // paranoid
  n := length(a);
  SetLength(a, n + 1);
  a[n] := v;
end;

// about TLV (Type-Length-Value) encoding, see e.g.
// https://www.ibm.com/docs/en/tpmfod/7.1.1.4?topic=configuration-dhcp-option-43

function TlvFromJson(p: PUtf8Char; out v: RawByteString): boolean;
var
  one: TRuleValue;
  parser: TJsonParserContext;
  tmp: TSynTempAdder; // no transient memory allocation
begin
  result := false;
  if p = nil then
    exit;
  parser.InitParser(p);
  if not parser.ParseObject then
    exit;
  tmp.Init;
  repeat
    if not ParseRuleValue(parser, one, prTlv) then
      exit;
    tmp.AddDirect(AnsiChar(one.num), AnsiChar(length(one.value)));
    tmp.Add(one.value)
  until parser.EndOfObject = '}';
  tmp.Done(v, CP_RAWBYTESTRING);
  result := true;
end;

function TlvFromJson(const json: RawUtf8): RawByteString; // for debug/testing
var
  tmp: TSynTempBuffer; // make a private local copy for in-place JSON parsing
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
  tmp: TSynTempAdder;
begin
  bin := '';
  result := false;
  tmp.Init;
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
      tmp.AddDirect(AnsiChar(w));    // width
      tmp.Add(@dest, (w + 7) shr 3); // only prefix
      tmp.Add(@router, 4);           // router
      while not (p^ in [#0, ';', ',']) do
        inc(p);
      if p^ = #0 then
        break;
      inc(p); // jump ',' or ';' between routes
    until p^ = #0;
  tmp.Done(bin, CP_RAWBYTESTRING);
  result := true;
end;

function ParseRule(const json: RawUtf8; out v: TRuleValues;
  pr: TParseRule): boolean;
var
  one: TRuleValue;
  parser: TJsonParserContext;
  tmp: TSynTempBuffer; // make a private local copy for in-place JSON parsing
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
      if not ParseRuleValue(parser, one, pr) then
        exit;
      AddRuleValue(v, one)
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
  tmp: TSynTempBuffer; // for in-place JSON parsing
begin
  FillZero(m);
  result := false;
  if json = '' then
    exit;
  tmp.Init(json);
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

const
  // truncate 64-bit integer to 6-bytes TNetMac - efficient on Intel and aarch64
  MAC_MASK = $0000ffffffffffff;
  // hardcore limit of dmtInform to 3 IPs per second
  MAX_INFORM = 3;
  // pre-allocate 4KB of working entries e.g. in TDhcpScope.AfterFill
  PREALLOCATE_LEASES = 250;

// some sensitive O(n) functions which could favor specific alignment

{$ifdef FPC_CODEALIGN}
  {$PUSH}
  {$CODEALIGN PROC=32} // keep small function size aligned for opcode cache
  {$CODEALIGN LOOP=1}  // keep prolog as small as possible in next 2 functions
{$endif FPC_CODEALIGN}

// code below expects PDhcpLease^.Mac to be the first field

function DoFindMac(p: PDhcpLease; mac: Int64; n: cardinal): PDhcpLease;
begin // dedicated sub-function for better codegen
  result := p;
  if n <> 0 then
    repeat
      if PInt64(result)^ shr 16 = mac then // fast ignore State/RateLimit fields
        exit;
      inc(result);
      dec(n);
    until n = 0;
  result := nil;
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

{$ifdef FPC_CODEALIGN}
  {$CODEALIGN LOOP=16}  // may favor bigger loops in the next 2 functions
{$endif FPC_CODEALIGN}

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

{$ifdef FPC_CODEALIGN}
  {$POP}
{$endif FPC_CODEALIGN}


{ TDhcpScope }

function TDhcpScope.FindMac(mac: Int64): PDhcpLease;
begin
  mac := mac and MAC_MASK;
  // naive but efficient lookup of the last added lease during DHCP negotiation
  if cardinal(LastDiscover) < cardinal(Count) then
  begin
    result := @Entry[LastDiscover];
    if PInt64(result)^ shr 16 = mac then
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
    PInt64(result)^ := mac64 shl 16; // also reset State+RateLimit
    if result^.State = lsFree then   // paranoid
      exit;
  end;
  // we need to add a new entry - maybe with reallocation
  n := Count;
  if n = length(Entry) then
    SetLength(Entry, NextGrow(n));
  result := @Entry[n];
  PInt64(result)^ := mac64 shl 16; // also reset State+RateLimit
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
        'PrepareScope: subnet-mask=% does not match %=%',
        [Subnet.Mask, ident, IP4ToShort(@ip)]);
  end;

begin
  // validate all settings values from the actual subnet
  if Gateway <> 0 then
    CheckSubnet('default-gateway', Gateway);
  CheckSubnet('server-identifier', ServerIdentifier);
  if IpMinLE = 0 then
    IpMinLE := Subnet.ip + $0a000000; // e.g. 192.168.0.10
  CheckSubnet('range-min', IpMinLE);
  if IpMaxLE = 0 then
    // e.g. 192.168.0.0 + pred(not(255.255.255.0)) = 192.168.0.254
    IpMaxLE := bswap32(bswap32(Subnet.ip) +
                    pred(not(bswap32(Subnet.mask))));
  CheckSubnet('range-max', IpMaxLE);
  if bswap32(IpMaxLE) <= bswap32(IpMinLE) then
    EDhcp.RaiseUtf8('PrepareScope: unexpected %..% range',
      [IP4ToShort(@IpMinLE), IP4ToShort(@IpMaxLE)]);
  if Broadcast <> 0 then
    CheckSubNet('broadcast-address', Broadcast);
  for i := 0 to high(StaticIP) do
    CheckSubnet('static', StaticIP[i]);
  for i := 0 to high(StaticMac) do
    CheckSubnet('static', StaticMac[i].IP4);
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
        log.Log(sllTrace, 'PrepareScope: subnet-mask adjust count=% from %',
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
      log.Log(sllInfo, 'PrepareScope: PXE next-server=%%',
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

function TDhcpScope.AddRule(const json: array of RawByteString): PtrInt;
var
  s: TDhcpRuleSettings;
  rule: TDhcpScopeRule;
begin
  s := TDhcpRuleSettings.Create;
  try
    // JSON is parsed outside of the lock
    if not JsonSettingsToObject(Join(json), s) then
      EDhcp.RaiseU('AddRule: incorrect JSON');
    if s.PrepareRule(self, rule) then // not inserted via AddStatic()
      // append to Rules[] within Safe.Lock
      result := AddRule(rule)
    else
      result := -1;
  finally
    s.Free;
  end;
end;

function TDhcpScope.AddRule(one: TDhcpScopeRule): PtrInt;
begin
  Safe.Lock;
  try
    result := length(Rules);
    SetLength(Rules, result + 1);
    Rules[result] := one;
  finally
    Safe.UnLock;
  end;
end;

function TDhcpScope.DeleteRule(index: PtrInt): boolean;
begin
  Safe.Lock;
  try
    result := DynArrayDelete(TypeInfo(TDhcpScopeRules), Rules, index);
  finally
    Safe.UnLock;
  end;
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
  boot: TUnixTime; subnet: PShortString): integer;
var
  d, e: PAnsiChar;
begin // dedicated sub-function for better codegen
  result := 0;
  repeat
    // OFFERed leases are temporary; the client hasn't accepted this IP
    // DECLINE/INFORM IPs (with MAC = 0) are ephemeral internal-only markers
    if (PInt64(p)^ shr 16 <> 0) and  // Mac64 <> 0
       (p^.IP4 <> 0) and
       (((p^.State = lsAck) or
        ((p^.State = lsOutdated) and // detected as p^.Expired < tix32
         (cardinal(tix32 - p^.Expired) < grace)))) then // still reusable
    begin
      if subnet <> nil then
      begin
        // add once the subnet mask as '# 192.168.0.1/24 subnet' comment line
        W.AddShort(subnet^);
        subnet := nil; // append once, and only if necessary
      end;
      // format is "1770034159 c2:07:2c:9d:eb:71 192.168.1.207" + LF
      W.AddQ(boot + Int64(p^.Expired), {reserve=}48);
      d := PAnsiChar(W.B) + 1; // directly write to the output buffer
      d[0] := ' ';
      ToHumanHexP(d + 1, @p^.mac, SizeOf(p^.mac));
      d[18] := ' ';
      e := IP4TextAppend(@p^.IP4, d + 19);
      e[0] := #10;
      inc(W.B, e - d + 1);
      inc(result);
    end;
    inc(p);
    dec(n);
  until n = 0;
end;

function TDhcpScope.TextWrite(W: TTextWriter; tix32: cardinal; time: TUnixTime): integer;
var
  grace: cardinal;
  subtxt: TShort47;
begin
  result := 0;
  if Count = 0 then
    exit;
  subtxt := '# ';
  Safe.Lock;
  try
    // prepare the subnet mask as '# 192.168.0.1/24 subnet' comment line
    subtxt[0] := #2;
    AppendShort(SubNet.ToShort, subtxt);
    AppendShort(' subnet'#10, subtxt);
    // persist all leases of this subnet as text
    grace := LeaseTimeLE * MaxPtrUInt(GraceFactor, 2); // not too deprecated
    if Count <> 0 then
      result := DoWrite(W, pointer(Entry), Count, tix32, grace, time, @subtxt);
  finally
    Safe.UnLock;
  end;
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


{ **************** Middle-Level DHCP State Machine }

{ TDhcpState }

function TDhcpState.Data(const opt: TDhcpOption): pointer;
var
  len: PtrUInt;
begin
  result := @RecvLens[opt];
  len := PWord(result)^;
  if len <> 0 then
    result := @Recv.options[len];
end;

procedure TDhcpState.AddOptionOnce32(const opt: TDhcpOption; be: cardinal);
var
  d: PAnsiChar;
begin
  if (be = 0) or
     (opt in SendOptions) then
    exit;
  include(SendOptions, opt);
  d := pointer(SendEnd);
  SendEnd := d + 6;
  if d >= @Send.options[high(Send.options) - 6] then
    exit; // move output pointer, but avoid buffer overflow
  d[0] := AnsiChar(DHCP_OPTION_NUM[opt]);
  d[1] := #4;
  PCardinal(d + 2)^ := be;
end;

procedure TDhcpState.AddOptionSafe(const op: byte; b: pointer; len: PtrUInt);
var
  d: PByteArray;
begin
  d := pointer(SendEnd);
  SendEnd := @d[len + 2];
  if SendEnd >= @Send.options[high(Send.options)] then
    exit; // move output pointer, but avoid buffer overflow
  d[0] := op;
  d[1] := len;
  MoveFast(b^, d[2], len);
end;

procedure TDhcpState.AddOptionFromRule(p: PRuleValue);
var
  v: PAnsiChar;
begin
  v := pointer(p^.value);
  if v = nil then
    exit;
  // actually append the value and mark it in SendOptions
  include(SendOptions, TDhcpOption(p^.mac[0]));
  AddOptionSafe(p^.num, v, PStrLen(v - _STRLEN)^);
end;

procedure TDhcpState.AddOptionOnceU(const opt: TDhcpOption; const v: PAnsiChar);
begin
  if (v = nil) or
     (opt in SendOptions) then
    exit;
  include(SendOptions, opt);
  AddOptionSafe(DHCP_OPTION_NUM[opt], v, PStrLen(v - _STRLEN)^);
end;

procedure TDhcpState.AddOptionOnceA32(const opt: TDhcpOption; const v: PAnsiChar);
begin
  if (v = nil) or
     (opt in SendOptions) then
    exit;
  include(SendOptions, opt);
  AddOptionSafe(DHCP_OPTION_NUM[opt], v, (PDALen(v - _DALEN)^ + _DAOFF) shl 2);
end;

procedure TDhcpState.AddOptionCopy(opt: TDhcpOption; dsm: TDhcpScopeMetric);
var
  len: PtrUInt;
  src, d: PAnsiChar;
begin
  len := RecvLens[opt];
  if (len = 0) or
     (opt in SendOptions) then
    exit;
  include(SendOptions, opt);
  if dsm <> dsmDiscover then
    inc(Scope^.Metrics.Current[dsm]);
  // copy whole "DHCP_OPTION_NUM + len + data" binary block
  src := @Recv.options[len];
  d := SendEnd;
  len := ord(src^) + 2;
  SendEnd := d + len;
  if SendEnd < @Send.options[high(Send.options)] then // safe: no overflow
    MoveFast(src[-1], d^, len);
end;

procedure TDhcpState.AddRulesOptions;
var
  p: PRuleValue;
  len, n: PtrUInt;
  requested: PByteArray;
begin
  p := pointer(RecvRule.send);
  if p = nil then
    exit;
  n := PDALen(PAnsiChar(p) - _DALEN)^ + _DAOFF;
  // append "always" - should be as send[0].kind=pvkAlways
  if p^.kind = pvkAlways then
  begin
    SendOptions := SendOptions + RecvRule.always;
    len := PStrLen(PAnsiChar(pointer(p^.value)) - _STRLEN)^;
    MoveFast(pointer(p^.value)^, SendEnd^, len); // as a pre-computed blob
    inc(SendEnd, len);
    inc(p);
    dec(n);
    if n = 0 then
      exit;
  end;
  // access option 55 list
  len := RecvLens[doParameterRequestList];
  if len = 0 then
    exit;
  requested := @Recv.options[len]; // len + [1,3,6,15,51,54]
  // process "requested" options, filtering each op
  repeat
    if ByteScanIndex(@requested[1], requested[0], p^.num) >= 0 then // SSE2 asm
      AddOptionFromRule(p); // as individual TLV
    inc(p);
    dec(n);
  until n = 0;
end;

{$ifdef FPC_CODEALIGN} // manually tuned for FPC x86_64
  {$PUSH}
  {$CODEALIGN LOOP=2}  // worth it on this very sensitive function
{$endif FPC_CODEALIGN}

function TDhcpState.MatchOne(one: PRuleValue): boolean;
var
  len: PtrUInt;
  option, value: PAnsiChar;
label
  optlen, optval; // optimized for FPC 64-bit CPU like x86_64 and aarch64
begin
  result := false;
  case one^.kind of // locate the option value in Recv[]
    pvkMac:
      result := PInt64(one)^ shr 16 = Mac64;   // fast inlined TNetMac compare
    pvkOpt:
      begin                                    // O(1) lookup of known option
        len := RecvLens[TDhcpOption(one^.mac[0])];
        if len = 0 then
          exit;                                // no such option
optlen: option := @Recv.options[len];          // option[0] = len in Recv[]
        // compare the one^.value with the option located in Recv[]
optval: value := pointer(one^.value);
        len := PStrLen(value - _STRLEN)^;      // we know value<>nil and len>0
        if len <> ord(option[0]) then
          exit;                                // length mismatch
        inc(option);
        repeat
          dec(len);                            // endings mismatch more likely
          if option[len] <> value[len] then    // faster than CompareMem() here
            exit;
        until len = 0;
        result := true;                        // exact case-sensitive match
       end;
    pvkRai:
      begin                                    // O(1) lookup of sub-option
        len := RecvLensRai[TDhcpOptionRai(one^.num)];
        if len <> 0 then
          goto optlen;
      end;
    pvkBoot:
      result := RecvBoot = TDhcpClientBoot(one^.num);
    pvkRaw:
      begin
        option := @Recv.options;               // inlined DhcpFindOption()
        repeat
          if option[0] = AnsiChar(one^.num) then
            break;                             // found this TLV num
          option := @option[ord(option[1]) + 2];
          if option[0] = #255 then             // reached end of list
            exit;                              // no such option
        until false;
        inc(option);                           // option[0] = len in Recv[]
        goto optval;
      end;
    pvkMacs:
      begin                                    // specific TNetMacs lookup
        value := pointer(one^.value);          // we know value<>nil
        len := PtrUInt(@value[PStrLen(value - _STRLEN)^ - 2]);
        dec(PWord(value));                     // for "PInt64() shr 16" below
        repeat
          if PInt64(value)^ shr 16 = Mac64 then
            break;                             // found one
          inc(PNetMac(value));                 // search next
          if PtrUInt(value) >= len then
            exit;                              // no match
        until false;
        result := true;
      end;
  end;
end;

{$ifdef FPC_CODEALIGN}
  {$POP}
{$endif FPC_CODEALIGN}

procedure TDhcpState.AddOption81;
var
  p: PAnsiChar;
  len: PtrInt;
  bak: AnsiChar; // keep Recv[] untouched
begin
  p := @Recv.options[RecvLens[doFqdn]]; // caller ensure <> 0
  if (p[0] < #3) or
     (PWord(p + 2)^ <> 0) then // rcode1=rcode2=0
    exit;
  include(SendOptions, doFqdn);
  len := ord(p[0]);
  inc(p);
  // Windows client send typically 0x05 (E=1 for wire format + S=1 to ask
  // server for updates) -> reset all flags but E since we have no DDNS yet
  bak := p[0];
  p[0] := AnsiChar(byte(bak) and RFC4702_FLAG_E);
  AddOptionSafe(81, p, len);  // preserve E flag and return raw fqdn encoding
  p[0] := bak;
end;

procedure TDhcpState.AddRegularOptions;
begin
  // append 1,3,6,15,28,42 network options - if not already from "rules"
  AddOptionOnce32(doSubnetMask,         Scope^.Subnet.mask);
  AddOptionOnce32(doBroadcastAddress,   Scope^.Broadcast);
  AddOptionOnce32(doRouters,            Scope^.Gateway);
  AddOptionOnceA32(doDomainNameServers, pointer(Scope^.DnsServer));
  AddOptionOnceA32(doNtpServers,        pointer(Scope^.NtpServers));
  // optional 51,58,59 lease timing options
  if (RecvType <> dmtInform) and
     not (doLeaseTime in SendOptions) then
  begin
    AddOptionOnce32(doLeaseTime,     Scope^.LeaseTime); // big-endian
    AddOptionOnce32(doRenewalTime,   Scope^.RenewalTime);
    AddOptionOnce32(doRebindingTime, Scope^.Rebinding);
  end;
  // append back option 81 with no associated DDNS flags
  if (RecvLens[doFqdn] <> 0) and
     not (doFqdn in SendOptions) then
    AddOption81;
end;

function TDhcpState.Flush: PtrUInt;
begin
  // send back verbatim Option 61 if any - by RFC 6842
  AddOptionCopy(doClientIdentifier, dsmOption61Hits);
  // send back Option 82 if any - should be the very last option by RFC 3046
  AddOptionCopy(doRelayAgentInformation, dsmOption82Hits);
  // append #255 trailer in the Send frame and returns its final size in bytes
  result := SendEnd - PAnsiChar(@Send) + 1;
  if result >= high(Send.options) then
    result := 0 // too many options: do not send anything
  else
    SendEnd^ := #255; // end this valid frame
  SendLen := result;
end;

procedure TDhcpState.ParseRecvLensRai;
var
  p: PAnsiChar;
  len: PtrInt;
begin
  p := @Recv.options[RecvLens[doRelayAgentInformation]]; // <> 0 by caller
  len := ord(p[0]);            // whole Option 82 length
  inc(p);
  repeat
    dec(len, ord(p[1]) + 2)  ; // O(1) decode sub-option binary as TLV
    if len < 0 then            // avoid buffer overdloas
    begin
      FillZero(THash128(RecvLensRai)); // ignore any previously decoded sub-options
      exit;
    end;
    if ord(p[0]) <= high(RAI_OPTION_INV) then // O(1) fast inverse lookup
      RecvLensRai[RAI_OPTION_INV[ord(p[0])]] := (p + 1) - PAnsiChar(@Recv.options);
    p := @p[ord(p[1]) + 2];
  until len = 0;
end;

function TDhcpState.FakeLease(found: TNetIP4): PDhcpLease;
begin
  result := @Temp; // fake transient PDhcpLease for this StaticUuid[]
  result^.IP4 := found;
  PInt64(result)^ := Mac64 shl 16; // also reset State+RateLimit
  result^.State := lsStatic;
end;

function TDhcpState.ClientUuid(opt: TDhcpOption): PDhcpLease;
var
  fnd: TNetIP4;
  v: PByte;
  m: PByteArray;
  len: PtrUInt;
begin
  result := nil;
  // locate opt = doClientIdentifier or doUuidClientIdentifier value
  v := @Recv.options[RecvLens[opt]]; // v^[0] <> 0
  len := v^;
  inc(v);     // v^ points to the specified binary value
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
  // O(n) fast search
  fnd := DoFindUuid(pointer(Scope^.StaticUuid), pointer(v), len);
  if fnd = 0 then
    exit;
  // append /GUID or /UUID in logs after the MAC address (up to 63 chars)
  result := FakeLease(fnd);
  m := @Mac;
  AppendShortChar('/', pointer(m));
  if len = SizeOf(TGuid) then
    AppendShortUuid(PGuid(v)^, PShortString(m)^)
  else
  begin
    len := MinPtrUInt((high(Mac) - m[0]) shr 1, len);
    mormot.core.text.BinToHex(pointer(v), @m[m[0]], len);
    inc(m[0], len * 2);
  end;
end;

procedure TDhcpState.AppendToMac(ip4len: PtrUInt; const ident: ShortString);
var
  selection: TNetIP4;
begin
  if ip4len = 0 then
    exit;
  selection := DhcpIP4(@Recv, ip4len);
  if selection = 0 then
    exit;
  AppendShort(ident, Mac);
  IP4Short(@selection, Ip);
  AppendShort(Ip, Mac);
  Ip[0] := #0;
end;

function TDhcpState.Parse: boolean;
begin
  SendLen := 0;
  Ip4 := 0;
  Mac64 := 0;
  RecvRule := nil;
  RecvBoot := dcbDefault;
  SendType := dmtUndefined;
  RecvType := DhcpParse(@Recv, RecvLen, RecvLens, nil, @Mac64);
  Ip[0] := #0;
  if Mac64 <> 0 then
  begin
    // valid DHCP frame with RecvType <> dmtUndefined
    Mac[0] := #17;
    ToHumanHexP(@Mac[1], @Mac64, SizeOf(TNetMac));
    RecvHostName := Data(doHostName);
    FillZero(THash128(RecvLensRai));
    if RecvLens[doRelayAgentInformation] <> 0 then
      ParseRecvLensRai;
    result := true;
  end
  else
  begin
    Mac[0] := #0;
    RecvHostName := @NULCHAR;
    result := false;
  end;
end;

function TDhcpState.RecvToJson(extended: boolean): RawJson;
var
  tmp: TTextWriterStackBuffer; // 8KB static
  W: TJsonWriter;
begin
  result := '';
  if Mac64 = 0 then
    exit;
  W := TJsonWriter.CreateOwnedStream(tmp);
  try
    if extended then
      W.CustomOptions := [twoForceJsonExtended];
    DoDhcpToJson(W, @Recv, RecvLen, {state=}nil); // as a single JSON object
    W.SetText(RawUtf8(result));
  finally
    W.Free;
  end;
end;

function TDhcpState.SendToJson(extended: boolean): RawJson;
var
  tmp: TTextWriterStackBuffer; // 8KB static
  W: TJsonWriter;
begin
  result := '';
  if SendLen = 0 then
    exit;
  W := TJsonWriter.CreateOwnedStream(tmp);
  try
    if extended then
      W.CustomOptions := [twoForceJsonExtended];
    DoDhcpToJson(W, @Send, SendLen, {state=}nil); // as a single JSON object
    W.SetText(RawUtf8(result));
  finally
    W.Free;
  end;
end;

function TDhcpState.ClientNew(dmt: TDhcpMessageType; const addr: TNetMac;
  req: TDhcpOptions): PAnsiChar;
begin
  result := DhcpClient(Recv, dmt, addr, req);
end;

procedure TDhcpState.ClientFlush(current: PAnsiChar);
var
  size: PtrUInt;
begin
  size := current - PAnsiChar(@Recv) + 1;
  RecvLen := 0; // avoid GPF
  if size > SizeOf(Recv) then
    exit;
  RecvLen := size;
  current^ := #255;
end;


{ **************** High-Level Multi-Scope DHCP Server Processing Logic }

{ TDhcpBootSettings }

procedure InheritOption(var boot: TDhcpScopeBoot; ref, dst: TDhcpClientBoot);
  {$ifdef HASINLINE} inline; {$endif}
var
  p: PRawUtf8;
begin
  p := @boot.Remote[dst];
  if p^ = '' then
    p^ := boot.Remote[ref];
end;

procedure TDhcpBootSettings.PrepareBoot(var Data: TDhcpScopeBoot;
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
  InheritOption(Data, dcbX64, dcbX64Http);
  InheritOption(Data, dcbA64, dcbA64Http);
  // 2. assume we could share the main x64/x86 IPXE URI
  ref := dcbDefault;
  if Data.Remote[dcbIpxeX64] <> '' then
    ref := dcbIpxeX64
  else if Data.Remote[dcbIpxeX86] <> '' then
    ref := dcbIpxeX86;
  if ref <> dcbDefault then
    for dcb := dcbIpxeX86 to high(Data.Remote) do
      if dcb <> ref then
        InheritOption(Data, ref, dcb);
end;


{ TDhcpRuleSettings }

procedure DoParseRule(const json: RawUtf8; var v: TRuleValues; pr: TParseRule;
  ctx: PUtf8Char);
begin
  if not ParseRule(json, v, pr) then
    EDhcp.RaiseUtf8('PrepareRule: invalid %:%', [ctx, json]);
end;

function TDhcpRuleSettings.PrepareRule(var Data: TDhcpScope; var Rule: TDhcpScopeRule): boolean;
var
  alw, req: TRuleValues;
  p: PRuleValue;
  v: TRuleValue;
  i: PtrInt;
  nfo: TMacIP;
begin
  // parse main "rules" JSON object fields
  Rule.name := fName;
  DoParseRule(fAll, Rule.all, prMatch, 'all');
  DoParseRule(fAny, Rule.any, prMatch, 'any');
  DoParseRule(fNotAll, Rule.notall, prMatch, 'not-all');
  DoParseRule(fNotAny, Rule.notany, prMatch, 'not-any');
  DoParseRule(fAlways, alw, prValue, 'always');
  DoParseRule(fRequested, req, prValue, 'requested');
  // parse optional "mac" alias
  if fMac <> '' then
  begin
    if (Rule.all <> nil) or
       (Rule.any <> nil) or
       (Rule.notall <> nil) or
       (Rule.notany <> nil) then
      EDhcp.RaiseUtf8('PrepareRule: mac:% is exclusive', [fMac]);
    if (length(fMac) <> 17) or
       not TextToMac(pointer(fMac), @v.mac) then
      EDhcp.RaiseUtf8('PrepareRule: invalid mac:%', [fMac]);
    // generate the corresponding {"all":{"mac":"xxxxx"}} entry
    v.num := 0;
    v.kind := pvkMac;
    AddRuleValue(Rule.all, v);
  end;
  // parse specific static "ip" reservation
  Rule.ip := 0;
  if fIP <> '' then
  begin
    if not NetIsIP4(pointer(fIp), @Rule.ip) or
       not Data.Subnet.Match(Rule.Ip) then
      EDhcp.RaiseUtf8('PrepareRule: invalid ip:%', [fIp]);
    if (Rule.all = nil) and
       (Rule.any = nil) then
      EDhcp.RaiseUtf8('PrepareRule: missing any/all for ip=%', [fIp]);
    nfo.ip := Rule.ip;
    FillZero(nfo.mac);
    if (Rule.any = nil) and
       (length(Rule.all) = 1) then
      case Rule.all[0].kind of
        pvkMac:
          begin
            // plain {"all":{"mac":...}},"ip":...} entry = regular 'mac=ip'
            nfo.mac := Rule.all[0].mac;     // to register in StaticMac[]
            if (alw = nil) and
               (req = nil) then
            begin
              // the main statics mac/ip list is enough for this definition
              if not Data.AddStatic(nfo) then
                EDhcp.RaiseUtf8('PrepareRule: duplicated ip=% mac=%',
                  [fIp, MacToShort(@nfo.mac)]);
              result := false; // no rule needed
              exit;
            end;
          end;
        pvkMacs:
          EDhcp.RaiseUtf8('PrepareRule: ip=% requires a single mac', [fIp]);
      end;
    if not Data.AddStatic(nfo) then // add in main statics ip list
      EDhcp.RaiseUtf8('PrepareRule: duplicated ip=%', [fIp]);
  end;
  // Rule.send[0] is "always" and should be stored as pvkAlways binary blob
  Rule.send := nil;
  Rule.always := [];
  if alw <> nil then
  begin
    PInt64(@v)^ := 0; // set kind+num+mac=0
    v.kind := pvkAlways;
    p := pointer(alw);
    for i := 1 to length(alw) do
    // prepare raw TLV-concatenated binary buffer
    begin
      Append(v.value, [AnsiChar(p^.num), AnsiChar(length(p^.value)), p^.value]);
      if p^.kind = pvkOpt then
        if TDhcpOption(p^.mac[0]) in Rule.always then
          EDhcp.RaiseUtf8('PrepareRule: duplicated % in always:%',
            [DHCP_OPTION[TDhcpOption(p^.mac[0])], fAlways])
        else
          include(Rule.always, TDhcpOption(p^.mac[0]));
      inc(p);
    end;
    AddRuleValue(Rule.send, v);
  end;
  // append "requested" with their op, ready to be filtered against option 55
  p := pointer(req);
  for i := 1 to length(req) do
  begin
    if (p^.kind = pvkOpt) and
       (TDhcpOption(p^.mac[0]) in Rule.always) then
      EDhcp.RaiseUtf8('PrepareRule: duplicated % in requested:%',
        [DHCP_OPTION[TDhcpOption(p^.mac[0])], fRequested]);
    AddRuleValue(Rule.send, p^);
    inc(p);
  end;
  // return true if there is some condition to append: void entries are skipped
  result := Rule.send <> nil;
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

function TDhcpScopeSettings.AddRule(one: TDhcpRuleSettings): TDhcpRuleSettings;
begin
  if one = nil then
    one := TDhcpRuleSettings.Create;
  if self <> nil then
    PtrArrayAdd(fRules, one); // will be owned by this instance
  result := one;
end;

procedure TDhcpScopeSettings.PrepareScope(Sender: TDhcpProcess;
  var Data: TDhcpScope);
var
  mask: TNetIP4;
  i, n: PtrInt;
begin
  // convert the main settings into Data.* fields - raise EDhcp on error
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
      EDhcp.RaiseUtf8('PrepareScope: invalid static=%', [fStatic[i]]);
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
  fBoot.PrepareBoot(Data.Boot, Data.Options);
  // convert "rules" into ready-to-be-processed objects
  Data.Rules := nil;
  SetLength(Data.Rules, length(fRules));
  n := 0;
  for i := 0 to high(fRules) do
    if fRules[i].PrepareRule(Data, Data.Rules[n]) then
      inc(n);
  if n <> length(fRules) then
    SetLength(Data.Rules, n); // some "rules" were TMacIP static in disguise
  // retrieve and adjust the subnet mask from settings
  if not Data.Subnet.From(fSubnetMask) then
    EDhcp.RaiseUtf8(
      'PrepareScope: unexpected subnet-mask=% (should be mask ip or ip/sub)',
      [fSubnetMask]);
  if Data.Subnet.mask = cAnyHost32 then
  begin
    // subnet-mask was not '192.168.0.1/24': is expected to be '255.255.255.0'
    if IP4Prefix(Data.Subnet.ip) = 0 then
      EDhcp.RaiseUtf8('PrepareScope: subnet-mask=% is not a valid IPv4 mask',
        [fSubnetMask]);
    mask := Data.Subnet.ip;
    // we expect other parameters to be set specifically: compute final Subnet
    if Data.ServerIdentifier = 0 then
      EDhcp.RaiseUtf8('PrepareScope: subnet-mask=% but without server-identifier',
        [fSubnetMask]);
    Data.Subnet.mask := mask;
    Data.Subnet.ip := Data.ServerIdentifier and mask; // normalize as in From()
  end
  else
    // subnet-mask was e.g. '192.168.0.1/24'
    if Data.ServerIdentifier = 0 then
      // extract from exact subnet-mask text, since Subnet.ip = 192.168.0.0
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
    if fLogPrefix = '' then
      fLogPrefix := 'Compute Response: ';
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
    exit; // no valid Setup(), or called twice
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
  total, saved: integer;
  csv: boolean;
  s: PDhcpScope;
  start: Int64;
  tmp: TShort31;
begin
  // make periodical process at most every second
  result := 0;
  tix32 := tix64 div MilliSecsPerSec; // GetTickSec from GetTickCount64
  if (tix32 = fIdleTix) or
     (fScope = nil) or
     (fState <> sSetup) then // e.g. after Shutdown
    exit;
  saved := 0;
  total := 0;
  fIdleTix := tix32;
  fScopeSafe.ReadLock;
  try
    // check deprecated entries in all scopes
    s := pointer(fScope);
    if s <> nil then
    begin
      n := PDALen(PAnsiChar(s) - _DALEN)^ + _DAOFF;
      repeat
        inc(result, s^.CheckOutdated(tix32)); // 5us for 20k leases
        inc(total, s^.Count);
        inc(s);
        dec(n);
      until n = 0;
    end;
    if result <> 0 then
      inc(fModifSequence); // trigger SaveToFile() below
    // background persist into FileName and MetricsFolder if needed
    tmp[0] := #0;
    if (fFileName <> '') and
       (fFileFlushSeconds <> 0) and         // = 0 if disabled
       (fModifSaved <> fModifSequence) then // if something new to be written
      if tix32 >= fFileFlushTix then        // reached the next persistence time
      begin
        fFileFlushTix := tix32 + fFileFlushSeconds;  // every 30 secs by default
        QueryPerformanceMicroSeconds(start);         // saved=100000 in 5.65ms
        saved := SaveToFile(fFileName); // make fScopeSafe.ReadLock/ReadUnLock
        FormatShort(' saved=% in %', [saved, MicroSecFrom(start)], tmp);
        // notes: 1) do not aggressively retry if saved < 0 (write failed)
        //        2) no background thread needed - SaveToFile() takes only
        //           5.65ms with 100K leases for a 4.2MB text file
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
    fLog.Add.Log(sllTrace, 'OnIdle: outdated=%/%%', [result, total, tmp], self);
end;

function TDhcpProcess.SaveToText(SavedCount: PInteger): RawUtf8;
var
  tmp: TTextWriterStackBuffer; // 8KB static, then up to 1MB buffer (20K leases)
  W: TTextWriter;
  tix32, saved, n: cardinal;
  boot: TUnixTime;
  s: PDhcpScope;
begin
  result := CRLF; // returns something not void
  // prepare the export context
  if SavedCount <> nil then
    SavedCount^ := 0;
  n := GetCount;
  if n = 0 then
    exit;
  tix32 := GetTickSec;
  boot := UnixTimeUtc - tix32;    // = UnixTimeUtc at computer boot
  if boot < UNIXTIME_MINIMAL then // we should have booted after 08 Dec 2016 :)
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
          inc(saved, s^.TextWrite(W, tix32, boot));
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
  mac64: Int64;
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
  mac64 := 0;
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
             TextToMac(b, @mac64) and
             (mac64 <> 0) and
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
                last := s;        // is likely not to change for the next line
            end;
            if s <> nil then
            begin
              // add this lease to the internal list of this scope
              if s^.Count = length(s^.Entry) then
                SetLength(s^.Entry, NextGrow(s^.Count));
              new := @s^.Entry[s^.Count];
              inc(s^.Count);
              PInt64(new)^ := mac64 shl 16; // also set State+RateLimit
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

procedure TDhcpProcess.ComputeMetrics(var global: TDhcpAllMetrics);
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

procedure TDhcpProcess.ComputeMetrics(var global: TDhcpMetrics);
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
  ComputeMetrics(global);
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
  bak: TFileName;
  hasbak: boolean;
begin // for 100K leases: SaveToText=4.21ms FileFromString=1.44ms (4.2MB)
  txt := SaveToText(@result);     // make fScopeSafe.ReadLock/ReadUnLock
  hasbak := false;
  if not (dsoNoFileBak in fOptions) then
  begin
    bak := ChangeFileExt(FileName, '.bak');
    DeleteFile(bak);
    hasbak := RenameFile(FileName, bak);    // atomic backup
  end;
  if FileFromString(txt, FileName) then
    fModifSaved := fModifSequence // success: won't retry on next OnIdle()
  else
  begin
    if hasbak then
      RenameFile(bak, FileName);  // restore the previous file (if any)
    result := -1;                 // indicates error writing to disk
  end;
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
  const State: TDhcpState);
var
  fam: TSynLogFamily;
  one: TSynLog;
  msg: ShortString; // shared by TSynLog and JournalSend()
  prefixlen: PtrInt;
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
  prefixlen := length(fLogPrefix); // 'ComputeResponse: ' by default
  msg[0] := AnsiChar(prefixlen);
  MoveFast(pointer(fLogPrefix)^, msg[1], prefixlen);
  AppendShortAnsi7String(DHCP_TXT[State.RecvType], msg); // Undefined='invalid'
  AppendShortChar(' ', @msg);
  if State.Mac[0] <> #0 then
  begin
    AppendShort(State.Mac, msg);
    AppendShortChar(' ', @msg);
  end;
  AppendShort(Context, msg);
  AppendShortChar(' ', @msg);
  if State.SendType <> dmtUndefined then
  begin
    AppendShortAnsi7String(DHCP_TXT[State.SendType], msg);
    AppendShortChar(' ', @msg);
  end;
  AppendShort(State.Ip, msg);
  if State.RecvHostName^[0] <> #0 then
  begin
    AppendShortChar(' ', @msg);
    AppendShort(State.RecvHostName^, msg);
  end;
  if State.RecvBoot <> dcbDefault then
  begin
    AppendShort(' boot=', msg); // e.g. 'boot=ipxe-x64'
    AppendShortAnsi7String(BOOT_TXT[State.RecvBoot], msg);
  end;
  if (State.RecvRule <> nil) and
     (State.RecvRule.name <> '') then
  begin
    AppendShort(' rule=', msg);
    AppendShortAnsi7String(State.RecvRule.name, msg);
    if msg[0] = #255 then
      dec(msg[0]); // avoid buffer overflow on next line
  end;
  msg[ord(msg[0]) + 1] := #0; // ensure ASCIIZ
  // efficiently append to local TSynLog
  if one <> nil then
    one.LogText(Level, PUtf8Char(@msg[1]), nil); // this is the fastest API
  // optionnally send 'REQUEST MAC...' text to system logs (much slower)
  if dsoSystemLog in fOptions then
    JournalSend(Level, @msg[prefixlen + 1], ord(msg[0]) - prefixlen, false);
end;

procedure TDhcpProcess.OnLogFrame(Sender: TSynLog; Level: TSynLogLevel;
  Opaque: pointer; Value: PtrInt; Instance: TObject);
var
  s: PDhcpState absolute Opaque;
  us: Int64;
  W: TJsonWriter;
begin
  W := Sender.Writer; // called within TSynLog global lock
  case Level of
    sllClient:
      DoDhcpToJson(W, @s^.Recv, s^.RecvLen, {state=}nil);
    sllServer:
      begin
        QueryPerformanceMicroSeconds(us);
        DoDhcpToJson(W, @s^.Send, s^.SendLen, s);
        W.CancelLastChar;
        W.AddShorter(',us:'); // twoForceJsonExtended from within logs
        W.AddQ(us - s^.StartMicroSec);
        W.AddDirect('}');
      end;
  else
    W.AddShorter('LogFrame'); // paranoid
  end;
end;

function TDhcpProcess.DoError(var State: TDhcpState; Context: TDhcpScopeMetric;
  Lease: PDhcpLease): PtrInt;
begin
  result := 0; // no response, but no error
  case Context of
    dsmDroppedNoSubnet:
      begin
        // log context following FindScope() logic
        State.AppendToMac(State.RecvLensRai[dorLinkSelection], ' opt82=');
        State.AppendToMac(State.RecvLens[doSubnetSelection],   ' opt118=');
        if State.Recv.giaddr <> 0 then
          IP4Short(@State.Recv.giaddr, State.Ip)
        else if State.RecvIp4 <> 0 then
          IP4Short(@State.RecvIP4, State.Ip);
        DoLog(sllDebug, 'not subnet for', State);
        exit; // State.Scope = nil
      end;
    dsmDroppedNoAvailableIP:
      DoLog(sllWarning, 'exhausted IPv4', State);
    dsmNak:
      begin
        DoLog(sllTrace, 'out-of-sync NAK', State);
        State.SendType := dmtNak;
        State.SendEnd := DhcpNew(State.Send, dmtNak, State.Recv.xid,
          PNetMac(@State.Mac64)^, State.Scope^.ServerIdentifier);
        result := Flush(State);
      end;
    dsmDroppedPackets:
      DoLog(sllDebug, 'with no previous OFFER', State);
    dsmRateLimitHit,
    dsmInformRateLimitHit:
      DoLog(sllDebug, 'overload', State);
    dsmDroppedInvalidIP:
      DoLog(sllTrace, 'unexpected', State);
    dsmLeaseReleased:
      begin
        IP4Short(@Lease^.IP4, State.Ip);
        DoLog(sllTrace, 'as', State);
        State.Scope^.ReuseIp4(Lease); // set MAC=0 IP=0 State=lsFree
        inc(fModifSequence); // trigger SaveToFile() in next OnIdle()
      end;
    dsmUnsupportedRequest:
      begin
        DoLog(sllTrace, 'unsupported', State);
        result := -1; // error
      end;
    dsmDroppedCallback:
      DoLog(sllTrace, 'dropped by callback', State);
    dsmDroppedTooManyOptions:
      DoLog(sllWarning, 'dropped to avoid overflow', State);
  end;
  inc(State.Scope^.Metrics.Current[Context]);
end;

function TDhcpProcess.ParseFrame(var State: TDhcpState): boolean;
begin
  result := State.Parse;
  if result and
     (dsoVerboseLog in fOptions) and
     Assigned(fLog) then
    fLog.Add.RawLog(sllClient, OnLogFrame, @State);
end;

function TDhcpProcess.RetrieveFrameIP(
  var State: TDhcpState; Lease: PDhcpLease): boolean;
var
  ip4: TNetIP4;
begin
  // called from DISCOVER and REQUEST
  result := false;
  ip4 := DhcpIP4(@State.Recv, State.RecvLens[doRequestedAddress]); // opt 50
  if ip4 = 0 then
    exit;
  if (Lease = nil) or
     (Lease^.IP4 <> ip4) then
    if (not State.Scope^.SubNet.Match(ip4)) or
       State.Scope^.IsStaticIP(ip4) or
       (State.Scope^.FindIp4(ip4) <> nil) then
    begin
      // this IP seems already used by another MAC
      IP4Short(@ip4, State.Ip);
      DoLog(sllTrace, 'ignore requested', State); // transient info: no DoError
      inc(State.Scope^.Metrics.Current[dsmDroppedInvalidIP]);
      exit;
    end;
  inc(State.Scope^.Metrics.Current[dsmOption50Hits]);
  State.Ip4 := ip4;
  result := true;
end;

function TDhcpProcess.FindScope(var State: TDhcpState): boolean;
begin
  result := true;
  State.Scope := GetScope(DhcpIP4(@State.Recv, State.RecvLensRai[dorLinkSelection]));
  if State.Scope <> nil then
  begin
    // RFC 3527 sub-option in relay-agent-information 82, on complex relay
    // environments where giaddr is already meaningful for routing and cannot
    // be overloaded for subnet selection
    inc(State.Scope^.Metrics.Current[dsmOption82SubnetHits]);
    exit;
  end;
  // fallback to option 118 or giaddr if link-selection was set but invalid
  State.Scope := GetScope(DhcpIP4(@State.Recv, State.RecvLens[doSubnetSelection]));
  if State.Scope <> nil then
  begin
    // RFC 3011 defines option 118 which overrides giaddr, e.g. on centralized
    // DHCP server serving multiple logical subnets behind the same relay IP
    inc(State.Scope^.Metrics.Current[dsmOption118Hits]);
    exit;
  end;
  // fallback to giaddr if link-selection/subnet-selection was set but invalid
  if State.Recv.giaddr <> 0 then
    // e.g. VLAN 10 relay set giaddr=192.168.10.1 Gateway IP field
    // - giaddr from RFC 2131 is authoritative so we should not fallback
    State.Scope := GetScope(State.Recv.giaddr)
  else if State.RecvIp4 <> 0 then
    // no giaddr: check RecvIp4 bound local server IP as set by the UDP server
    State.Scope := GetScope(State.RecvIp4)
  else
    // no option 118 no giaddr nor RecvIp4: default to Scope[0]
    State.Scope := pointer(fScope);
  result := State.Scope <> nil;
end;

function TDhcpProcess.FindLease(var State: TDhcpState): PDhcpLease;
begin
  // from reserved static "ip" in "rules"
  if (State.RecvRule <> nil) and
     (State.RecvRule.ip <> 0) then
  begin
    result := State.FakeLease(State.RecvRule.ip);
    exit;
  end;
  // from StaticUuid[]
  result := pointer(State.Scope^.StaticUuid);
  if result <> nil then
    if State.RecvLens[doClientIdentifier] <> 0 then  // computer MAC/UUID
      result := State.ClientUuid(doClientIdentifier)
    else if State.RecvLens[doUuidClientIdentifier] <> 0 then  // IPXE/VM
      result := State.ClientUuid(doUuidClientIdentifier)
    else
      result := nil;
  // from Entry[] and StaticMac[]
  if result = nil then
    result := State.Scope^.FindMac(State.Mac64);
end;

const
  ARCH_DCB: array[0 .. 12] of TDhcpClientBoot = (
    dcbBios, dcbDefault, dcbX86, dcbDefault, dcbDefault, dcbDefault, // 0..5
    dcbX86, dcbX64, dcbX64, dcbX64, dcbA32, dcbA64, dcbA64);   // 6..12
  // 0 = BIOS x86 (Legacy), 1 = NEC/PC87, 2 = EFI x86, 3 = EFI bytecode,
  // 4 = EFI XScale, 5 = EFI early x64, 6 = EFI x86 Microsoft, 7 = EFI x64,
  // 8 = EFI x64 HTTP, 9 = EFI2 x64 HTTP, 10 = EFI arm32, 11 = EFI arm64,
  // 12 = EFI arm64 HTTP - 8,9,12 mean HTTP boot capability, not selection

procedure TDhcpProcess.SetRecvBoot(var State: TDhcpState);
var
  b: TDhcpClientBoot;
  o: PByteArray;
  vendor, user: PAnsiChar;
  a: PtrUInt;
begin
  b := dcbDefault;
  // parse RFC 4578 PXE option 93 as uint16 (00:00)
  o := @State.RecvLens[doClientArchitecture];
  if o[0] <> 0 then
  begin
    o := @State.Recv.options[o[0]];
    if (o[0] = 2) and
       (o[1] = 0) and // stored as BigEndian
       (o[2] <= high(ARCH_DCB)) then
      b := ARCH_DCB[o[2]];
  end;
  vendor := State.Data(doVendorClassIdentifier);
  if b = dcbDefault then
    // fallback to Option 60 parsing - e.g. 'PXEClient:Arch:00007:UNDI:003016'
    if (vendor[0] >= #17) and
       (PCardinal(vendor + 1)^ = // fast case-sensitive 'PXEC' exact check
         ord('P') + ord('X') shl 8 + ord('E') shl 16 + ord('C') shl 24) and
       CompareShort(vendor + 5, 'lient:Arch:0') then
    begin
      a := GetCardinal(PUtf8Char(vendor + 17));
      if a <= high(ARCH_DCB) then
        b := ARCH_DCB[a];
    end;
  if b = dcbDefault then
    exit; // normal DHCP boot
  // detect iPXE from RFC 3004 Option 77
  user := State.Data(doUserClass);
  if (user[0] = #4) and
     (PCardinal(user + 1)^ = // fast case-sensitive 'iPXE' exact check
       ord('i') + ord('P') shl 8 + ord('X') shl 16 + ord('E') shl 24) then
    // change dcbBios..dcbA64 into dcbIpxeBios..dcbIpxeA64
    inc(b, ord(dcbIpxeBios) - ord(dcbBios))
  else if (vendor[0] >= #10) and
          (PCardinal(vendor + 1)^ = HTTP_32) and
          CompareShort(vendor + 5, 'Client') then
    // HTTPClient in Option 60 indicates native UEFI firmware HTTP boot
    // - will fallback to TFTP is no HTTP URI is supplied
    case b of
      dcbX64:
        b := dcbX64Http;
      dcbA64:
        b := dcbA64Http;
    end;
  State.RecvBoot := b;
end;

procedure TDhcpProcess.AddBootOptions(var State: TDhcpState);
var
  boot: ^TDhcpScopeBoot;
begin
  // we know that State.Boot <> dcbDefault: validate the request
  boot := @State.Scope^.Boot;
  if boot^.Remote[State.RecvBoot] = '' then
  begin
    DoLog(sllDebug, 'missing boot file', State); // including 'boot=ipxe-x64'
    State.RecvBoot := dcbDefault;                // no 'boot=...' any more
    inc(State.Scope^.Metrics.Current[dsmDroppedPxeBoot]);
    // will still send back an OFFER/ACK but with no PXE options
    // - this is what ISC DHCP, KEA, and dnsmasq do in practice
    exit;
  end;
  inc(State.Scope^.Metrics.Current[dsmPxeBoot]);
  // known configuration: append PXE/iPXE specific 66/67 options
  State.AddOptionOnceU(doTftpServerName, pointer(boot^.NextServer));
  State.AddOptionOnceU(doBootFileName,   pointer(boot^.Remote[State.RecvBoot]));
  // copy back verbatim option 60 and 97 to PXE clients
  State.AddOptionCopy(doVendorClassIdentifier);
  State.AddOptionCopy(doUuidClientIdentifier);
end;

procedure TDhcpProcess.SetRule(var State: TDhcpState);
var
  rule: PDhcpScopeRule;  // runtime bytecode of each "rules"
  rules, hi: TDALen;     // use hi=high(TRuleValues) to favor FPC
  r: PRuleValue;
label
  ko; // unrolled logic for very efficient runtime execution
begin
  // implements a "first precise rule wins" deterministic Virtual Machine
  rule := pointer(State.Scope^.Rules); // caller ensured rule <> nil
  rules := PDALen(PAnsiChar(rule) - _DALEN)^ + _DAOFF;
  repeat
    // AND conditions (all must match) = "all"
    r := pointer(rule^.all);
    if r <> nil then
    begin
      hi := PDALen(PAnsiChar(r) - _DALEN)^ + (_DAOFF - 1);
      repeat
        if not State.MatchOne(r) then
          goto ko;  // missing one
        if hi = 0 then
          break;    // "all" condition passed
        dec(hi);
        inc(r);
      until false;
      // all did match
    end;
    // OR conditions (at least one must match) = "any"
    r := pointer(rule^.any);
    if r <> nil then
    begin
      hi := PDALen(PAnsiChar(r) - _DALEN)^ + (_DAOFF - 1);
      repeat
        if State.MatchOne(r) then
          break;    // at least one match - "any" condition passed
        if hi = 0 then
          goto ko;  // none did match
        dec(hi);
        inc(r);
      until false;
    end;
    // NOT AND conditions (all must NOT match) = "not-all"
    r := pointer(rule^.notall);
    if r <> nil then
    begin
      hi := PDALen(PAnsiChar(r) - _DALEN)^ + (_DAOFF - 1);
      repeat
        if State.MatchOne(r) then
          goto ko;  // one did match
        if hi = 0 then
          break;    // "not-all" condition passed
        dec(hi);
        inc(r);
      until false;
    end;
    // NOT OR conditions (at least one must NOT match) = "not-any"
    r := pointer(rule^.notany);
    if r <> nil then
    begin
      hi := PDALen(PAnsiChar(r) - _DALEN)^ + (_DAOFF - 1);
      repeat
        if not State.MatchOne(r) then
          break;    // one did not match - "not-any" condition passed
        if hi = 0 then
          goto ko;  // all did match
        dec(hi);
        inc(r);
      until false;
    end;
    // if we reached here, all conditions did apply
    State.RecvRule := rule;
    // all=any=nil as fallback if nothing more precise did apply
    // "not-all"/"not-any" without any "all"/"any" are just ignored
    // unless they are the eventual default - last default wins
    if (rule^.all <> nil) or
       (rule^.any <> nil) then
      // first exact matching rule wins (deterministic)
      exit;
ko: inc(rule);      // next "rules" object
    dec(rules);
  until rules = 0;
end;

function TDhcpProcess.RunCallbackAborted(var State: TDhcpState): boolean;
begin
  // caller ensured fOnComputeResponse <> nil
  try
    fOnComputeResponse(self, State);
  except
    State.SendEnd := nil; // intercept any callback issue and abort
  end;
  result := State.SendEnd = nil; // callback asked to silently ignore this frame
  if result then
    DoError(State, dsmDroppedCallback);
end;

function TDhcpProcess.Flush(var State: TDhcpState): PtrInt;
begin
  // recognize State.RecvBoot from options 60/77/93
  SetRecvBoot(State);
  // append "rules" custom options - always first since have precedence
  integer(State.SendOptions) := 0;
  if State.RecvRule <> nil then
    State.AddRulesOptions;
  // append "boot" specific options 60,66,67,97
  if State.RecvBoot <> dcbDefault then
    AddBootOptions(State);
  // append regular DHCP 1,3,6,15,28,42 [+ 51,58,59] options
  State.AddRegularOptions;
  // optional callback support
  if Assigned(fOnComputeResponse) and
     RunCallbackAborted(State) then
    // aborted by callback: nothing to send
    result := 0
  else
  begin
    // append verbatim options 61/82 copy + end Send buffer stream
    result := State.Flush;
    if result = 0 then
      result := DoError(State, dsmDroppedTooManyOptions)
    else if (dsoVerboseLog in fOptions) and
            Assigned(fLog) then
      fLog.Add.RawLog(sllServer, OnLogFrame, @State);
  end;
end;

function TDhcpProcess.LockedResponse(var State: TDhcpState): PtrInt;
var
  p: PDhcpLease;
begin
  // identify any matching input from this scope "rules" definition
  if State.Scope^.Rules <> nil then
    SetRule(State);
  // find any existing lease - or StaticMac[] StaticUuid[] fake lease
  p := FindLease(State);
  // process the received DHCP message
  case State.RecvType of
    dmtDiscover:
      begin
        inc(State.Scope^.Metrics.Current[dsmDiscover]);
        if (p = nil) or    // first time this MAC is seen
           (p^.IP4 = 0) or // may happen after DECLINE
           (p^.State = lsReserved) then // reserved = client refused this IP
        begin
          // first time seen (most common case), or renewal
          if not RetrieveFrameIP(State, p) then // IP from option 50
          begin
            State.Ip4 := State.Scope^.NextIp4; // guess a free IP from pool
            if State.Ip4 = 0 then
            begin
              // IPv4 exhausted: don't return NAK, but silently ignore
              // - client will retry after a small temporisation
              result := DoError(State, dsmDroppedNoAvailableIP);
              exit;
            end;
          end;
          if p = nil then
          begin
            p := State.Scope^.NewLease(State.Mac64);
            State.Scope^.LastDiscover := State.Scope^.Index(p);
          end;
          p^.IP4 := State.Ip4;
        end
        else
          // keep existing/previous/static IP4
          State.Ip4 := p^.IP4;
        // update the lease information
        if p^.State = lsStatic then
        begin
          p^.Expired := State.Tix32;
          inc(State.Scope^.Metrics.Current[dsmStaticHits]);
        end
        else
        begin
          inc(fModifSequence); // trigger SaveToFile() in next OnIdle()
          p^.State := lsReserved;
          p^.Expired := State.Tix32 + State.Scope^.OfferHolding;
          inc(State.Scope^.Metrics.Current[dsmDynamicHits]);
        end;
        // respond with an OFFER
        inc(State.Scope^.Metrics.Current[dsmOffer]);
        State.SendType := dmtOffer;
      end;
    dmtRequest:
      begin
        inc(State.Scope^.Metrics.Current[dsmRequest]);
        if (p <> nil) and
           (p^.IP4 <> 0) and
           ((p^.State in [lsReserved, lsAck, lsStatic]) or
            ((p^.State = lsOutdated) and
             (State.Scope^.GraceFactor > 1) and
             (State.Scope^.LeaseTimeLE < SecsPerHour) and // grace period
             (State.Tix32 - p^.Expired <
                State.Scope^.LeaseTimeLE * State.Scope^.GraceFactor))) then
          // RFC 2131: lease is Reserved after OFFER = SELECTING
          //           lease is Ack/Static/Outdated = RENEWING/REBINDING
          inc(State.Scope^.Metrics.Current[dsmLeaseRenewed])
        else if RetrieveFrameIP(State, p) then
          begin
            // no lease, but Option 50 = INIT-REBOOT
            if p = nil then
              p := State.Scope^.NewLease(State.Mac64)
            else
              inc(State.Scope^.Metrics.Current[dsmLeaseRenewed]);
            p^.IP4 := State.Ip4;
          end
          else
          begin
            // no lease, and none or invalid Option 50 = send NAK response
            result := DoError(State, dsmNak);
            exit;
          end;
        // update the lease information
        if p^.State = lsStatic then
        begin
          p^.Expired := State.Tix32;
          inc(State.Scope^.Metrics.Current[dsmStaticHits]);
        end
        else
        begin
          inc(fModifSequence); // trigger SaveToFile() in next OnIdle()
          p^.State := lsAck;
          p^.Expired := State.Tix32 + State.Scope^.LeaseTimeLE;
          inc(State.Scope^.Metrics.Current[dsmDynamicHits]);
        end;
        // respond with an ACK on the known IP
        inc(State.Scope^.Metrics.Current[dsmAck]);
        State.Ip4 := p^.IP4;
        State.SendType := dmtAck;
      end;
    dmtDecline:
      begin
        inc(State.Scope^.Metrics.Current[dsmDecline]);
        // client ARPed the OFFERed IP and detected conflict, so DECLINE it
        if (p = nil) or
           (p^.State <> lsReserved) then
        begin
          // ensure the server recently OFFERed one IP to that client
          // (match RFC intent and prevent blind poisoning of arbitrary IPs)
          result := DoError(State, dsmDroppedPackets);
          exit;
        end;
        if (State.Scope^.MaxDeclinePerSec <> 0) and // = 5 by default
           (fIdleTix <> 0) then  // OnIdle() would reset RateLimit := 0
          if p^.RateLimit < State.Scope^.MaxDeclinePerSec then
            inc(p^.RateLimit)
          else
          begin
            // malicious client poisons the pool by sending repeated DECLINE
            result := DoError(State, dsmRateLimitHit);
            exit;
          end;
        // retrieve and check requested IP
        State.Ip4 := DhcpIP4(@State.Recv, State.RecvLens[doRequestedAddress]);
        if State.Ip4 <> 0 then // authoritative IP is option 50
          inc(State.Scope^.Metrics.Current[dsmOption50Hits])
        else
          State.Ip4 := State.Recv.ciaddr; // fallback to ciaddr
        if State.Ip4 = 0 then
          State.Ip4 := p^.IP4 // last reserved IP would be invalidated below
        else if not State.Scope^.SubNet.Match(State.Ip4) then
        begin
          // option 50 or ciaddr is not for this subnet -> do nothing
          result := DoError(State, dsmDroppedInvalidIP);
          exit;
        end;
        // always invalidate any previous offered IP
        p^.State := lsUnavailable;
        p^.IP4 := 0;
        if State.Ip4 <> 0 then
        begin
          // store internally this IP as unavailable for NextIP4
          IP4Short(@State.Ip4, State.Ip);
          DoLog(sllTrace, 'as', State);  // success: no DoError()
          p := State.Scope^.NewLease(0); // mac=0: sentinel to store this IP
          p^.State := lsUnavailable;
          p^.IP4 := State.Ip4;
          p^.Expired := State.Tix32 + State.Scope^.DeclineTime;
        end;
        inc(fModifSequence); // trigger SaveToFile() in next OnIdle()
        result := 0;         // no response, but not an error
        exit; // server MUST NOT respond to a DECLINE message
      end;
    dmtRelease:
      begin
        inc(State.Scope^.Metrics.Current[dsmRelease]);
        // client informs the DHCP server that it no longer needs the lease
        if (p = nil) or
           (p^.IP4 <> State.Recv.ciaddr) or
           not (p^.State in [lsAck, lsOutdated]) then
          // detect and ignore out-of-synch or malicious client
          result := DoError(State, dsmDroppedInvalidIP)
        else
          result := DoError(State, dsmLeaseReleased, p);
        exit; // server MUST NOT respond to a RELEASE message
      end;
    dmtInform:
      begin
        inc(State.Scope^.Metrics.Current[dsmInform]);
        // client requests configuration options for its given IP
        State.Ip4 := State.Recv.ciaddr;
        if not State.Scope^.Subnet.Match(State.Ip4) then
        begin
          IP4Short(@State.Ip4, State.Ip); // no ciaddr or wrong subnet
          result := DoError(State, dsmDroppedInvalidIP);
          exit;
        end;
        if (p <> nil) and
           (p^.State = lsStatic) then
        begin
          p^.Expired := State.Tix32;
          inc(State.Scope^.Metrics.Current[dsmStaticHits]);
        end
        else
        begin
          if p <> nil then
            inc(State.Scope^.Metrics.Current[dsmDynamicHits]);
          if (dsoInformRateLimit in State.Scope^.Options) and
             (fIdleTix <> 0) and // OnIdle() would reset RateLimit := 0
             not State.Scope^.IsStaticIP(State.Ip4) then
          begin
            // temporarily marks client IP as unavailable to avoid conflicts
            // (not set by default: stronger than the RFC requires, and no
            // other DHCP server like KEA, dnsmasq or Windows implements it)
            if p = nil then
              p := State.Scope^.NewLease(State.Mac64)
            else if p^.RateLimit >= 2 then // up to 3 INFORM per MAC per sec
            begin
              result := DoError(State, dsmInformRateLimitHit);
              exit;
            end
            else
            begin
              inc(p^.RateLimit);
              // mark this IP as temporary unavailable
              if p^.IP4 <> State.Ip4 then
                // create a new MAC-free lease for this other IP
                p := State.Scope^.NewLease(0); // mac=0: sentinel for this IP
            end;
            // update the lease information
            inc(fModifSequence); // trigger SaveToFile() in next OnIdle()
            p^.State := lsUnavailable;
            p^.Expired := State.Tix32 + State.Scope^.DeclineTime;
            p^.IP4 := State.Ip4;
          end;
        end;
        // respond with an ACK and the standard options
        inc(State.Scope^.Metrics.Current[dsmAck]);
        State.SendType := dmtAck;
      end;
  else
    begin
      // ParseFrame() was correct but this message type is not supported yet
      result := DoError(State, dsmUnsupportedRequest);
      exit;
    end;
  end;
  // compute the dmtOffer/dmtAck response frame over the very same xid
  IP4Short(@State.Ip4, State.Ip);
  State.SendEnd := DhcpNew(State.Send, State.SendType, State.Recv.xid,
    PNetMac(@State.Recv.chaddr)^, State.Scope^.ServerIdentifier);
  State.Send.yiaddr := State.Ip4;
  result := Flush(State);   // callback + options
  if result <> 0 then
    DoLog(sllTrace, 'into', State); // if not canceled by callback
end;

function TDhcpProcess.ComputeResponse(var State: TDhcpState): PtrInt;
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
  if not ParseFrame(State) then
  begin
    if State.RecvIp4 <> 0 then
      IP4Short(@State.RecvIp4, State.Ip); // bound server IP
    DoLog(sllTrace, 'frame', State);
    inc(fMetricsInvalidRequest); // no Scope yet
    exit;
    // wrong RecvType calls DoError(dsmUnsupportedRequest) in LockedResponse
  end;
  fScopeSafe.ReadLock; // protect Scope[] but is reentrant and non-blocking
  try
    // detect the proper scope to use from option 118/82 or giaddr
    if not FindScope(State) then
    begin
      result := DoError(State, dsmDroppedNoSubnet);
      exit; // MUST NOT respond if no subnet matches
    end;
    // syscalls before locking this scope
    if State.Tix32 = 0 then
      State.Tix32 := GetTickSec; // caller should have set this field
    if (dsoVerboseLog in Options) and
       Assigned(fLog) then
      QueryPerformanceMicroSeconds(State.StartMicroSec);
    // evaluate the request in this blocked scope/subnet context
    State.Scope^.Safe.Lock;
    try
      result := LockedResponse(State);
    finally
      State.Scope^.Safe.UnLock;
    end;
  finally
    fScopeSafe.ReadUnLock;
  end;
end;


initialization
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
    TypeInfo(TDhcpRuleSettingsObjArray),  TDhcpRuleSettings]);
  {$endif HASDYNARRAYTYPE}
  Rtti.ByClass[TDhcpServerSettings].Props.NameChangeCase(scKebabCase, true, true);

end.

