/// Simple Network DNS Client
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.dns;

{
  *****************************************************************************

   Simple DNS Protocol Client
    - Low-Level DNS Protocol Definitions
    - High-Level DNS Query

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.buffers,
  mormot.net.sock;


{ **************** Low-Level DNS Protocol Definitions }


type
  /// Dns Resource Record (RR) Types
  // - from http://www.iana.org/assignments/dns-parameters
  // - main values are e.g. drrA for a host address, drrNS for an authoritative
  // name server, or drrCNAME for the alias canonical name
  // - this enumerate has no RTTI because it is mapped to the integer values
  TDnsResourceRecord = (
    drrA = 1,
    drrNS,
    drrMD,
    drrMF,
    drrCNAME,
    drrSOA,
    drrMB,
    drrMG,
    drrMR,
    drrNULL,
    drrWKS,
    drrPTR,
    drrHINFO,
    drrMINFO,
    drrMX,
    drrTXT,
    drrRP,
    drrAFSDB,
    drrX25,
    drrISDN,
    drrRT,
    drrNSAP,
    drrNSAP_P,
    drrSIG,
    drrKEY,
    drrPX,
    drrGPOS,
    drrAAAA,
    drrLOC,
    drrNXT,
    drrEID,
    drrNIMLOC,
    drrSRV,
    drrATMA,
    drrNAPTR,
    drrKX,
    drrCERT,
    drrA6,
    drrDNAME,
    drrSINK,
    drrOPT,
    drrAPL,
    drrDS,
    drrSSHFP,
    drrIPSECK,
    drrRRSIG,
    drrNSEC,
    drrDNSKEY,
    drrDHCID,
    drrNSEC3,
    drrNSEC3PARAM,
    drrTLSA,
    drrSMIMEA,
    drrHIP = 55,
    drrNINFO,
    drrRKEY,
    drrTALINK,
    drrCDS,
    drrCDNSKEY,
    drrOPENPGPKEY,
    drrCSYNC,
    drrZONEMD,
    drrSVCB,
    drrHTTPS,
    drrSPF = 99,
    drrUINFO,
    drrUID,
    drrGID,
    drrUNSPEC,
    drrNID,
    drrL32,
    drrL64,
    drrLP,
    drrEUI48,
    drrEUI64,
    drrTKEY = 249,
    drrTSIG,
    drrIXFR,
    drrAXFR,
    drrMAILB,
    drrMAILA,
    drrALL,
    drrURI,
    drrCAA,
    drrAVC,
    drrDOA,
    drrAMTRELAY,
    drrTA = 32768,
    drrDLV);


const
  DnsPort = '53';

  /// Internet DNS Question Class
  QC_INET = 1;


{$A-} // every record (or object) is packed from now on

type
  /// map a DNS header binary record
  // - with some easy getter/setter for the bit-oriented flags
  TDnsHeader = object
  private
    function GetAA: boolean;
      {$ifdef FPC} inline; {$endif}
    function GetOpCode: byte;
      {$ifdef FPC} inline; {$endif}
    function GetQR: boolean;
      {$ifdef FPC} inline; {$endif}
    function GetRA: boolean;
      {$ifdef FPC} inline; {$endif}
    function GetRCode: byte;
      {$ifdef FPC} inline; {$endif}
    function GetRD: boolean;
      {$ifdef FPC} inline; {$endif}
    function GetTC: boolean;
      {$ifdef FPC} inline; {$endif}
    function GetZ: byte;
    procedure SetOpCode(AValue: byte);
    procedure SetQR(AValue: boolean);
    procedure SetRD(AValue: boolean);
    procedure SetZ(AValue: byte);
  public
    Xid: word;
    Flags: byte;
    Flags2: byte;
    QuestionCount: word;
    AnswerCount: word;
    NameServerCount: word;
    AdditionalCount: word;
    property IsResponse: boolean
      read GetQR write SetQR;
    property OpCode: byte
      read GetOpCode write SetOpCode;
    property AuthorativeAnswer: boolean
      read GetAA;
    property Truncation: boolean
      read GetTC;
    property RecursionDesired: boolean
      read GetRD write SetRD;
    property RecursionAvailable: boolean
      read GetRA;
    property Z: byte
      read GetZ write SetZ;
    property ResponseCode: byte
      read GetRCode;
  end;
  PDnsHeader = ^TDnsHeader;

/// parse a DNS string entry
// - return 0 on error, or the next 0-based position in Answer
function DnsParseString(const Answer: RawByteString; Pos: PtrInt;
  var Text: RawUtf8): PtrInt;

/// parse a DNS 16-bit big endian value
function DnsParseWord(p: PByteArray; var pos: PtrInt): cardinal;
  {$ifdef HASINLINE} inline; {$endif}

/// parse a DNS 32-bit big endian value
function DnsParseCardinal(p: PByteArray; var pos: PtrInt): cardinal;
  {$ifdef HASINLINE} inline; {$endif}

/// raw parsing of best-known DNS record data into human readable text
// - as used by DnsParseRecord() to fill the TDnsAnswer.Text field
// - only recognize A AAAA CNAME TXT NS PTR MX SOA SRV Resource Records
procedure DnsParseData(RR: TDnsResourceRecord;
  const Answer: RawByteString; Pos, Len: PtrInt; var Text: RawUtf8);

/// raw computation of a DNS query message
function DnsBuildQuestion(const QName: RawUtf8; RR: TDnsResourceRecord;
  QClass: cardinal = QC_INET): RawByteString;

/// raw sending and receiving of DNS query message over UDP
function DnsSendQuestion(const Address, Port: RawUtf8;
  const Request: RawByteString; out Answer: RawByteString;
  out TimeElapsed: cardinal; TimeOutMS: integer = 2000): boolean;



{ **************** High-Level DNS Query }

{$A+}

type
  /// one DNS decoded record as stored by DnsQuery() in TDnsResult
  TDnsAnswer = record
    /// the Name of this record
    QName: RawUtf8;
    /// the type of this record
    QType: TDnsResourceRecord;
    /// after how many seconds this record information is deprecated
    TTL: cardinal;
    /// 0-based position of the raw binary of the record content
    // - pointing into TDnsResult.RawAnswer binary buffer
    Position: integer;
    /// main text information decoded from Data binary
    // - only best-known DNS resource record QType are recognized, i.e.
    // A AAAA CNAME TXT NS PTR MX SOA SRV as decoded by DnsParseData()
    Text: RawUtf8;
  end;
  PDnsAnswer = ^TDnsAnswer;
  TDnsAnswers = array of TDnsAnswer;

  /// the resultset of a DnsQuery() process
  TDnsResult = record
    /// the decoded DNS header of this query
    Header: TDnsHeader;
    /// the time needed for this DNS server lookup
    ElapsedMicroSec: cardinal;
    /// the Answers records
    Answer: TDnsAnswers;
    /// the Authorities records
    Authority: TDnsAnswers;
    /// the Additionals records
    Additional: TDnsAnswers;
    /// the raw binary UDP response frame, needed for DnsParseString() decoding
    RawAnswer: RawByteString;
  end;

/// parse a DNS answer record entry
function DnsParseRecord(const Answer: RawByteString; var Pos: PtrInt;
  var Dest: TDnsAnswer; QClass: cardinal): boolean;

/// send a DNS query and parse the answer
// - if no NameServer[] is supplied, will use GetDnsAddresses list from OS
// - the TDnsResult output gives access to all returned DNS records
// - use DnsLookup/DnsReverseLookup/DnsServices() for most simple requests
function DnsQuery(const QName: RawUtf8; out Res: TDnsResult;
  RR: TDnsResourceRecord = drrA; const NameServer: RawUtf8 = '';
  TimeOutMS: integer = 2000; QClass: cardinal = QC_INET): boolean;


/// retrieve the IPv4 address of a DNS host name - using DnsQuery(drrA)
// - e.g. DnsLookup('synopse.info') currently returns '62.210.254.173'
// - for aliases, the CNAME is ignored and only the first A is returned, e.g.
// DnsLookup('blog.synopse.info') would simply return '62.210.254.173'
// - will also recognize obvious values like 'localhost' or an IPv4 address
// - this unit will register this function to mormot.net.sock's NewSocketIP4Lookup
// - warning: executes a raw DNS query, so hosts system file is not used,
// and no cache is involved: use TNetAddr.SetFrom() instead if you can
function DnsLookup(const HostName: RawUtf8;
  const NameServer: RawUtf8 = ''): RawUtf8;

/// retrieve the IPv4 address(es) of a DNS host name - using DnsQuery(drrA)
// - e.g. DnsLookups('synopse.info') currently returns ['62.210.254.173'] but
// DnsLookups('yahoo.com') returns an array of several IPv4 addresses
// - will also recognize obvious values like 'localhost' or an IPv4 address
function DnsLookups(const HostName: RawUtf8;
  const NameServer: RawUtf8 = ''): TRawUtf8DynArray;

/// retrieve the DNS host name of an IPv4 address - using DnsQuery(drrPTR)
// - note that the reversed host name is the one from the hosting company, and
// unlikely the usual name: e.g. DnsReverseLookup(DnsLookup('synopse.info'))
// returns the horsey '62-210-254-173.rev.poneytelecom.eu' from online.net
function DnsReverseLookup(const IP4: RawUtf8;
  const NameServer: RawUtf8 = ''): RawUtf8;

/// retrieve the Services of a DNS host name - using DnsQuery(drrSRV)
// - services addresses are returned with their port, e.g.
// DnsServices('_ldap._tcp.ad.mycorp.com') returns
// ['dc-one.mycorp.com:389', 'dc-two.mycorp.com:389']
function DnsServices(const HostName: RawUtf8;
  const NameServer: RawUtf8 = ''): TRawUtf8DynArray;

/// retrieve the LDAP controlers from the current AD domain name
// - returns e.g. ['dc-one.mycorp.com:389', 'dc-two.mycorp.com:389']
// - optionally return the associated AD controler host name, e.g. 'ad.mycorp.com'
function DnsLdapControlers(const NameServer: RawUtf8 = '';
  UsePosixEnv: boolean = false; DomainName: PRawUtf8 = nil): TRawUtf8DynArray;


implementation

{ **************** Low-Level DNS Protocol Definitions }

const
  // Flags 1
  QF_QR     = $80;
  QF_OPCODE = $78;
  QF_AA     = $04;
  QF_TC     = $02;  // Truncated.
  QF_RD     = $01;

  // Flags 2
  QF_RA     = $80;
  QF_Z      = $70;
  QF_RCODE  = $0F;

  DNS_RELATIVE = $c0;
  DNS_RESP_SUCCESS = $00;


{ TDnsHeader }

function TDnsHeader.GetAA: boolean;
begin
  result := (Flags and QF_AA) <> 0;
end;

function TDnsHeader.GetOpCode: byte;
begin
  result := (Flags and QF_OPCODE) shr 3;
end;

function TDnsHeader.GetQR: boolean;
begin
  result := (Flags and QF_QR) <> 0;
end;

function TDnsHeader.GetRA: boolean;
begin
  result := (Flags2 and QF_RA) <> 0;
end;

function TDnsHeader.GetRCode: byte;
begin
  result := (Flags2 and QF_RCODE);
end;

function TDnsHeader.GetRD: boolean;
begin
  result := (Flags and QF_RD) <> 0;
end;

function TDnsHeader.GetTC: boolean;
begin
  result := (Flags and QF_TC) <> 0;
end;

function TDnsHeader.GetZ: byte;
begin
  result := (Flags and QF_Z) shr 4;
end;

procedure TDnsHeader.SetOpCode(AValue: byte);
begin
  Flags := (Flags and not(QF_OPCODE)) or ((AValue and $f) shl 3);
end;

procedure TDnsHeader.SetQR(AValue: boolean);
begin
  Flags := Flags and not(QF_QR);
  if AValue then
    Flags := Flags or QF_QR;
end;

procedure TDnsHeader.SetRD(AValue: boolean);
begin
  Flags := Flags and not(QF_RD);
  if AValue then
    Flags := Flags or QF_RD;
end;

procedure TDnsHeader.SetZ(AValue: byte);
begin
  Flags := (Flags2 and not(QF_Z)) or ((AValue and $7) shl 4);
end;


function DnsParseString(const Answer: RawByteString; Pos: PtrInt;
  var Text: RawUtf8): PtrInt;
var
  p: PByteArray;
  nextpos, max: PtrInt;
  len: byte;
  tmp: ShortString;
begin
  nextpos := 0;
  result := 0; // indicates error
  p := pointer(Answer);
  max := length(Answer);
  tmp[0] := #0;
  repeat
    if Pos >= max then
      exit; // avoid any buffer overflow on malformated/malinuous input
    len := p[Pos];
    inc(Pos);
    if len = 0 then
      break;
    while (len and DNS_RELATIVE) = DNS_RELATIVE do
    begin
      if nextpos = 0 then
        nextpos := Pos + 1; // if compressed, return end of 16-bit offset
      if Pos >= max then
        exit;
      Pos := PtrInt(len and (not DNS_RELATIVE)) shl 8 + p[Pos];
      if Pos >= max then
        exit;
      len := p[Pos];
      inc(Pos);
    end;
    if len = 0 then
      break;
    if Pos + len > max then
      exit;
    AppendShortBuffer(pointer(@p[Pos]), len, tmp);
    AppendShortChar('.', tmp);
    inc(Pos, len);
  until false;
  if tmp[0] = #0 then
    exit;
  if tmp[ord(tmp[0])] = '.' then
    dec(tmp[0]);
  FastSetString(Text, @tmp[1], ord(tmp[0]));
  if nextpos = 0 then
    result := Pos
  else
    result := nextpos;
end;

function DnsParseWord(p: PByteArray; var pos: PtrInt): cardinal;
begin
  result := swap(PWord(@p[pos])^);
  inc(pos, 2);
end;

function DnsParseCardinal(p: PByteArray; var pos: PtrInt): cardinal;
begin
  result := bswap32(PCardinal(@p[pos])^);
  inc(pos, 4);
end;

procedure DnsParseData(RR: TDnsResourceRecord;
  const Answer: RawByteString; Pos, Len: PtrInt; var Text: RawUtf8);
var
  p: PByteArray;
  s1, s2: RawUtf8;
begin
  p := @PByteArray(Answer)[Pos];
  case RR of
    drrA:
      // IPv4 binary address
      if Len = 4 then
        IP4Text(p, Text);
    drrAAAA:
      // IPv6 binary address
      if Len = 16 then
        IP6Text(p, Text);
    drrCNAME,
    drrTXT,
    drrNS,
    drrPTR:
      // single text Value
      DnsParseString(Answer, Pos, Text);
    drrMX:
      // Priority / Value
      if Len > 2 then
        DnsParseString(Answer, Pos + 2, Text);
    drrSOA:
      begin
        // MName / RName / Serial / Refresh / Retry / Expire / TTL
        Pos := DnsParseString(Answer, Pos, s1);
        if (Pos <> 0) and
           (DnsParseString(Answer, Pos, s2) <> 0) then
          Text := s1 + ' ' + s2;
      end;
    drrSRV:
      // Priority / Weight / Port / QName
      if Len > 6 then
        if DnsParseString(Answer, Pos + 6, Text) <> 0 then
          Text := Text + ':' + UInt32ToUtf8(swap(PWordArray(p)[2])); // :port
  end;
end;

function DnsBuildQuestion(const QName: RawUtf8; RR: TDnsResourceRecord;
  QClass: cardinal): RawByteString;
var
  tmp: TTextWriterStackBuffer;
  w: TBufferWriter;
  h: TDnsHeader;
  n: PUtf8Char;
  one: shortstring;
begin
  w := TBufferWriter.Create(tmp);
  try
    FillCharFast(h, SizeOf(h), 0);
    h.Xid := Random32; // truncated to 16-bit
    h.RecursionDesired := true;
    h.QuestionCount := 1 shl 8;
    w.Write(@h, SizeOf(h));
    n := pointer(QName);
    while n <> nil do
    begin
      GetNextItemShortString(n, @one, '.');
      if one[0] = #0 then
        break;
      w.Write1(ord(one[0]));
      w.Write(@one[1], ord(one[0]));
    end;
    w.Write1(0); // final #0
    w.Write2BigEndian(ord(RR));
    w.Write2BigEndian(QClass);
    result := w.FlushTo;
  finally
    w.Free;
  end;
end;

function DnsSendQuestion(const Address, Port: RawUtf8;
  const Request: RawByteString; out Answer: RawByteString;
  out TimeElapsed: cardinal; TimeOutMS: integer): boolean;
var
  addr, resp: TNetAddr;
  sock: TNetSocket;
  len: PtrInt;
  start, stop: Int64;
  res: TNetResult;
  tmp: TSynTempBuffer;
begin
  result := false;
  TimeElapsed := 0;
  QueryPerformanceMicroSeconds(start);
  // send the DNS query
  if addr.SetFrom(Address, Port, nlUdp) <> nrOk then
    exit;
  sock := addr.NewSocket(nlUdp);
  if sock <> nil then
    try
      sock.SetReceiveTimeout(TimeOutMS);
      res := sock.SendTo(pointer(Request), length(Request), addr);
      if res <> nrOk then
        exit;
      // get the response and ensure it is valid
      len := sock.RecvFrom(@tmp, SizeOf(tmp), resp);
      with PDnsHeader(@tmp)^ do
        if (len <= length(Request)) or
           not addr.IPEqual(resp) or
           (Xid <> PDnsHeader(Request)^.Xid) or
           not IsResponse or
           Truncation or
           not RecursionAvailable or
           (ResponseCode <> DNS_RESP_SUCCESS) or
           (AnswerCount + NameServerCount + AdditionalCount = 0) then
          exit;
      QueryPerformanceMicroSeconds(stop);
      TimeElapsed := stop - start;
      FastSetRawByteString(answer, @tmp, len);
      result := true;
    finally
      sock.Close;
    end;
end;


{ **************** High-Level DNS Query }

function DnsParseRecord(const Answer: RawByteString; var Pos: PtrInt;
  var Dest: TDnsAnswer; QClass: cardinal): boolean;
var
  len: PtrInt;
  p: PByteArray;
begin
  result := false;
  p := pointer(Answer);
  Pos := DnsParseString(Answer, Pos, Dest.QName);
  if (Pos = 0) or
     (Pos + 10 > length(Answer)) then
    exit;
  word(Dest.QType) := DnsParseWord(p, Pos);
  if DnsParseWord(p, Pos) <> QClass then
    exit;
  Dest.TTL := DnsParseCardinal(p, Pos);
  len := DnsParseWord(p, Pos);
  if Pos + len > length(Answer) then
    exit;
  Dest.Position := Pos;
  DnsParseData(Dest.QType, Answer, Pos, len, Dest.Text);
  inc(Pos, len);
  result := true;
end;

function DnsQuery(const QName: RawUtf8; out Res: TDnsResult;
  RR: TDnsResourceRecord; const NameServer: RawUtf8;
  TimeOutMS: integer; QClass: cardinal): boolean;
var
  i, pos: PtrInt;
  servers: TRawUtf8DynArray;
  request: RawByteString;
begin
  result := false;
  if (QName = '') or
     not IsAnsiCompatible(QName) then
    exit;
  Finalize(Res);
  FillCharFast(Res, SizeOf(Res), 0);
  request := DnsBuildQuestion(QName, RR, QClass);
  if NameServer = '' then
  begin
    // if no NameServer is specified, will ask all OS DNS in order
    servers := GetDnsAddresses;
    for i := 0 to high(servers) do
      if DnsSendQuestion(servers[i], DnsPort,
           request, Res.RawAnswer, Res.ElapsedMicroSec, TimeOutMS) then
        break;
    if Res.RawAnswer = '' then
      exit;
  end
  else if not DnsSendQuestion(NameServer, DnsPort,
                request, Res.RawAnswer, Res.ElapsedMicroSec, TimeOutMS) then
    exit;
  // we received a valid response from a DNS
  Res.Header := PDnsHeader(Res.RawAnswer)^;
  Res.Header.QuestionCount := swap(Res.Header.QuestionCount);
  Res.Header.AnswerCount := swap(Res.Header.AnswerCount);
  Res.Header.NameServerCount := swap(Res.Header.NameServerCount);
  Res.Header.AdditionalCount := swap(Res.Header.AdditionalCount);
  pos := length(request); // jump Header + Question = point to records
  if Res.Header.AnswerCount <> 0 then
  begin
    SetLength(Res.Answer, Res.Header.AnswerCount);
    for i := 0 to high(Res.Answer) do
      if not DnsParseRecord(Res.RawAnswer, pos, Res.Answer[i], QClass) then
        exit;
  end;
  if Res.Header.NameServerCount <> 0 then
  begin
    SetLength(Res.Authority, Res.Header.NameServerCount);
    for i := 0 to high(Res.Authority) do
      if not DnsParseRecord(Res.RawAnswer, pos, Res.Authority[i], QClass) then
        exit;
  end;
  if Res.Header.AdditionalCount <> 0 then
  begin
    SetLength(Res.Additional, Res.Header.AdditionalCount);
    for i := 0 to high(Res.Additional) do
      if not DnsParseRecord(Res.RawAnswer, pos, Res.Additional[i], QClass) then
        exit;
  end;
  result := true;
end;

function DnsLookupKnown(const HostName: RawUtf8; out Ip: RawUtf8): boolean;
begin
  result := true;
  if IdemPropNameU(HostName, 'localhost') or
     (HostName = c6Localhost) then
    Ip := IP4local
  else if NetIsIP4(pointer(HostName)) then
    Ip := HostName
  else
    result := false;
end;

function DnsLookup(const HostName, NameServer: RawUtf8): RawUtf8;
var
  res: TDnsResult;
  i: PtrInt;
begin
  if not DnsLookupKnown(HostName, result) then
    if DnsQuery(HostName, res, drrA, NameServer) then
      for i := 0 to high(res.Answer) do
        if res.Answer[i].QType = drrA then
        begin
          result := res.Answer[i].Text;
          break; // ignore CNAME but return first A record
        end;
end;

function DnsLookups(const HostName, NameServer: RawUtf8): TRawUtf8DynArray;
var
  res: TDnsResult;
  known: RawUtf8;
  i: PtrInt;
begin
  result := nil;
  if DnsLookupKnown(HostName, known) then
    AddRawUtf8(result, known)
  else if DnsQuery(HostName, res, drrA, NameServer) then
    for i := 0 to high(res.Answer) do
      if res.Answer[i].QType = drrA then
        AddRawUtf8(result, res.Answer[i].Text); // return all A records
end;

function DnsReverseLookup(const IP4, NameServer: RawUtf8): RawUtf8;
var
  b: array[0..3] of byte; // to be asked in inverse byte order
  res: TDnsResult;
  i: PtrInt;
begin
  result := '';
  cardinal(b) := 0;
  if NetIsIP4(pointer(IP4), @b) and
     DnsQuery(FormatUtf8('%.%.%.%.in-addr.arpa', [b[3], b[2], b[1], b[0]]),
       res, drrPTR, NameServer) then
    for i := 0 to high(res.Answer) do
      if res.Answer[i].QType = drrPTR then
      begin
        result := res.Answer[i].Text;
        exit;
      end;
end;

function DnsServices(const HostName, NameServer: RawUtf8): TRawUtf8DynArray;
var
  res: TDnsResult;
  i: PtrInt;
begin
  result := nil;
  if DnsQuery(HostName, res, drrSRV, NameServer) then
    for i := 0 to high(res.Answer) do
      if res.Answer[i].QType = drrSRV then
        AddRawUtf8(result, res.Answer[i].Text, {nodup=}true, {casesens=}false);
end;

function DnsLdapControlers(const NameServer: RawUtf8; UsePosixEnv: boolean;
  DomainName: PRawUtf8): TRawUtf8DynArray;
var
  ad: TRawUtf8DynArray;
  i: PtrInt;
begin
  result := nil;
  ad := GetDomainNames(UsePosixEnv);
  for i := 0 to high(ad) do
  begin
    result := DnsServices('_ldap._tcp.' + ad[i], NameServer);
    if result <> nil then
    begin
      if DomainName <> nil then
        DomainName^ := ad[i];
      exit;
    end;
  end;
end;

function _NewSocketIP4Lookup(const HostName: RawUtf8; out IP4: cardinal): boolean;
var
  ip: RawUtf8;
begin
  ip := DnsLookup(HostName, NewSocketIP4LookupServer);
  result := NetIsIP4(pointer(ip), @ip4);
end;


initialization
  assert(ord(drrHTTPS) = 65);
  assert(ord(drrSPF) = 99);
  assert(ord(drrEUI64) = 109);
  assert(ord(drrTKEY) = 249);
  assert(ord(drrAMTRELAY) = 260);
  NewSocketIP4Lookup := _NewSocketIP4Lookup;

end.

