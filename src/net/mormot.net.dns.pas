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
  mormot.net.sock,
  mormot.core.base,
  mormot.core.os,
  mormot.core.buffers,
  mormot.core.text,
  mormot.core.unicode;

{ **************** Low-Level DNS Protocol Definitions }


type
  /// Dns Resource Record (RR) Types
  // - from http://www.iana.org/assignments/dns-parameters
  // - main values are e.g. drrA for a host address, drrNS for an authoritative
  // name server, or drrCNAME for the alias canonical name
  // - this enumerate has no RTTI because it is mapped to the integer values
  TDnsResourceRecord = (
    drrUndefined,
    drrA,
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
  TDnsHeader = object
  private
    function GetAA: boolean;
    function GetOpCode: byte;
    function GetQR: boolean;
    function GetRA: boolean;
    function GetRCode: byte;
    function GetRD: boolean;
    function GetTC: boolean;
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

/// compute a DNS query message
function DnsBuildQuestion(const QName: RawUtf8; RR: TDnsResourceRecord;
  QClass: cardinal = QC_INET): RawByteString;



{ **************** High-Level DNS Query }

{$A+}

type
  TDnsAnswer = record
    QName: RawUtf8;
    QType: TDnsResourceRecord;
    TTL: cardinal;
    Data: RawByteString;
  end;
  PDnsAnswer = ^TDnsAnswer;
  TDnsAnswers = array of TDnsAnswer;

  /// the resultset of a DnsQuery() process
  TDnsResult = record
    Header: TDnsHeader;
    Answer: TDnsAnswers;
    Authority: TDnsAnswers;
    Additional: TDnsAnswers;
  end;

/// parse a DNS answer record entry
function DnsParseRecord(const answer: RawByteString; var pos: PtrInt;
  var dest: TDnsAnswer): boolean;

/// send a DNS query and parse the answer
// - if no NameServer[] is supplied, will use GetDnsAddresses list from OS
function DnsQuery(const QName: RawUtf8; out Res: TDnsResult;
  RR: TDnsResourceRecord = drrA; const NameServer: RawUtf8 = '';
  QClass: cardinal = QC_INET): boolean;



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
    Flags := Flags or (1 shl 7);
end;

procedure TDnsHeader.SetRD(AValue: boolean);
begin
  Flags := Flags and not(QF_RD);
  if AValue then
    Flags := Flags or 1;
end;

procedure TDnsHeader.SetZ(AValue: byte);
begin
  Flags := (Flags2 and not(QF_Z)) or ((AValue and $7) shl 4);
end;


function DnsParseString(const Answer: RawByteString; Pos: PtrInt;
  var Text: RawUtf8): PtrInt;
var
  p: PByteArray;
  len: byte;
  tmp: ShortString;
begin
  result := 0;
  p := pointer(Answer);
  tmp[0] := #0;
  repeat
    len := p[Pos];
    inc(Pos);
    if len = 0 then
      break;
    while (len and DNS_RELATIVE) =  DNS_RELATIVE do
    begin
      if result = 0 then
        result := Pos + 1; // if compressed, return end of 16-bit offset
      Pos := PtrInt(len and (not DNS_RELATIVE)) shl 8 + p[Pos];
      len := p[Pos];
      inc(Pos);
    end;
    if len = 0 then
      break;
    if Pos + len > length(Answer) then
    begin
      result := 0; // avoid buffer overflow, and return 0 as error parsing
      exit;
    end;
    AppendShortBuffer(pointer(@p[Pos]), len, tmp);
    AppendShortChar('.', tmp);
    inc(Pos, len);
  until false;
  if tmp[0] = #0 then
    exit;
  if tmp[ord(tmp[0])] = '.' then
    dec(tmp[0]);
  FastSetString(Text, @tmp[1], ord(tmp[0]));
  if result = 0 then
    result := Pos;
end;

function DnsParseWord(p: PByteArray; var pos: PtrInt): cardinal;
begin
  result := swap(PWord(@p[pos])^);
  inc(pos, 2);
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
  const Request: RawByteString; out Answer: RawByteString): boolean;
var
  addr, resp: TNetAddr;
  sock: TNetSocket;
  len: PtrInt;
  tmp: TSynTempBuffer;
  h: PDnsHeader;
begin
  result := false;
  if addr.SetFrom(Address, Port, nlUdp) <> nrOk then
    exit;
  sock := addr.NewSocket(nlUdp);
  if sock <> nil then
    try
      sock.SetReceiveTimeout(2000);
      if sock.SendTo(pointer(Request), length(Request), addr) <> nrOk then
        exit;
      len := sock.RecvFrom(@tmp, SizeOf(tmp), resp);
      h := @tmp;
      if (len <= length(Request)) or
         not addr.IsEqual(resp) or
         (h.Xid <> PDnsHeader(Request)^.Xid) or
         (h^.AnswerCount + h^.NameServerCount + h^.AdditionalCount = 0) then
        exit;
      FastSetRawByteString(answer, @tmp, len);
      result := true;
    finally
      sock.Close;
    end;
end;


{ **************** High-Level DNS Query }

function DnsParseRecord(const answer: RawByteString; var pos: PtrInt;
  var dest: TDnsAnswer): boolean;
var
  len: PtrInt;
  p: PByteArray;
begin
  result := false;
  p := pointer(answer);
  pos := DnsParseString(answer, pos, dest.QName);
  if pos = 0 then
    exit;
  word(dest.QType) := DnsParseWord(p, pos);
  if DnsParseWord(p, pos) <> QC_INET then
    exit;
  dest.TTL := bswap32(PCardinal(@p[pos])^);
  inc(pos, 4);
  len := DnsParseWord(p, pos);
  if pos + len > length(answer) then
    exit;
  FastSetRawByteString(dest.Data, @p[pos], len);
  inc(pos, len);
  result := true;
end;

function DnsQuery(const QName: RawUtf8; out Res: TDnsResult;
  RR: TDnsResourceRecord; const NameServer: RawUtf8; QClass: cardinal): boolean;
var
  i, pos: PtrInt;
  servers: TRawUtf8DynArray;
  request, answer: RawByteString;
begin
  result := false;
  Finalize(Res);
  request := DnsBuildQuestion(QName, RR, QClass);
  if NameServer = '' then
  begin
    servers := GetDnsAddresses;
    for i := 0 to high(servers) do
      if DnsSendQuestion(servers[i], DnsPort, request, answer) then
        break;
    if answer = '' then
      exit;
  end
  else if not DnsSendQuestion(NameServer, DnsPort, request, answer) then
    exit;
  Res.Header := PDnsHeader(answer)^;
  Res.Header.QuestionCount := swap(Res.Header.QuestionCount);
  Res.Header.AnswerCount := swap(Res.Header.AnswerCount);
  Res.Header.NameServerCount := swap(Res.Header.NameServerCount);
  Res.Header.AdditionalCount := swap(Res.Header.AdditionalCount);
  pos := length(request); // jump Header + Question
  if Res.Header.AnswerCount <> 0 then
  begin
    SetLength(Res.Answer, Res.Header.AnswerCount);
    for i := 0 to high(Res.Answer) do
      if not DnsParseRecord(answer, pos, Res.Answer[i]) then
        exit;
  end;
  if Res.Header.NameServerCount <> 0 then
  begin
    SetLength(Res.Authority, Res.Header.NameServerCount);
    for i := 0 to high(Res.Authority) do
      if not DnsParseRecord(answer, pos, Res.Authority[i]) then
        exit;
  end;
  if Res.Header.AdditionalCount <> 0 then
  begin
    SetLength(Res.Additional, Res.Header.AdditionalCount);
    for i := 0 to high(Res.Additional) do
      if not DnsParseRecord(answer, pos, Res.Additional[i]) then
        exit;
  end;
  result := true;
end;


initialization
  assert(ord(drrHTTPS) = 65);
  assert(ord(drrSPF) = 99);
  assert(ord(drrEUI64) = 109);
  assert(ord(drrTKEY) = 249);
  assert(ord(drrAMTRELAY) = 260);

end.

