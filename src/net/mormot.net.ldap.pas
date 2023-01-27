/// Simple Network LDAP Client
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.ldap;

{
  *****************************************************************************

   Simple LDAP Protocol Client
    - LDAP Response Storage
    - LDAP Client Class

  *****************************************************************************
  Code below was inspired by Synapse Library code:
   The Initial Developer of the Original Code is Lukas Gebauer (Czech Republic).
   Portions created by Lukas Gebauer are (c)2003-2014. All Rights Reserved.
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
  mormot.core.unicode,
  mormot.core.data,
  mormot.crypt.core;


{ **************** LDAP Response Storage }

type
  /// store a named LDAP attribute with the list of its values
  TLdapAttribute = class
  private
    fList: TRawUtf8DynArray;
    fAttributeName: RawUtf8;
    fCount: integer;
    fIsBinary: boolean;
  public
    /// initialize the attribute(s) storage
    constructor Create(const AttrName: RawUtf8);
    /// include a new value to this list
    // - IsBinary values will be stored base-64 encoded
    procedure Add(const aValue: RawByteString);
    /// retrieve a value as human-readable text
    function GetReadable(index: PtrInt): RawUtf8;
    /// retrieve a value as its inital value stored with Add()
    function GetRaw(index: PtrInt): RawByteString;
    /// how many values have been added to this attribute
    property Count: integer
      read fCount;
    /// name of this LDAP attribute
    property AttributeName: RawUtf8
      read fAttributeName;
    /// true if the attribute contains binary data
    property IsBinary: boolean
      read fIsBinary;
  end;
  /// dynamic array of LDAP attribute, as stored in TLdapAttributeList
  TLdapAttributeDynArray = array of TLdapAttribute;

  /// list one or several TLdapAttribute
  TLdapAttributeList = class
  private
    fItems: TLdapAttributeDynArray;
  public
    /// finalize the list
    destructor Destroy; override;
    /// clear the list
    procedure Clear;
    /// number of TLdapAttribute objects in this list
    function Count: integer;
      {$ifdef HASINLINE} inline; {$endif}
    /// allocate and a new TLdapAttribute object to the list
    function Add(const AttributeName: RawUtf8): TLdapAttribute;
    /// remove one TLdapAttribute object from the list
    procedure Delete(const AttributeName: RawUtf8);
    /// find and return attribute index with the requested name
    // - returns -1 if not found
    function FindIndex(const AttributeName: RawUtf8): PtrInt;
    /// find and return attribute with the requested name
    // - returns nil if not found
    function Find(const AttributeName: RawUtf8): TLdapAttribute;
    /// Find and return first attribute value with requested name
    // - calls GetReadable(0) on the found attribute
    // - returns empty string if not found
    function Get(const AttributeName: RawUtf8): RawUtf8;
    /// access to the internal list of TLdapAttribute objects
    property Items: TLdapAttributeDynArray
      read fItems;
  end;

  /// store one LDAP result, i.e. object name and attributes
  TLdapResult = class
  private
    fObjectName: RawUtf8;
    fAttributes: TLdapAttributeList;
  public
    /// initialize the instance
    constructor Create; reintroduce;
    /// finalize the instance
    destructor Destroy; override;
    /// Name of this LDAP object
    property ObjectName: RawUtf8
      read fObjectName write fObjectName;
    /// Here is list of object attributes
    property Attributes: TLdapAttributeList read fAttributes;
  end;
  TLdapResultObjArray = array of TLdapResult;

  /// maintain a list of LDAP result objects
  TLdapResultList = class(TObject)
  private
    fItems: TLdapResultObjArray;
    fCount: integer;
  public
    /// finalize the list
    destructor Destroy; override;
    /// create and add new TLdapResult object to the list
    function Add: TLdapResult;
    /// clear all TLdapResult objects in list
    procedure Clear;
    /// dump the result of a LDAP search into human readable form
    // - used for debugging
    function Dump: RawUtf8;
    /// number of TLdapResult objects in list
    property Count: integer
      read fCount;
    /// List of TLdapResult objects
    property Items: TLdapResultObjArray
      read fItems;
  end;


{ **************** LDAP Client Class }

type
  /// define possible operations for LDAP MODIFY operations
  TLdapModifyOp = (
    MO_Add,
    MO_Delete,
    MO_Replace
  );

  /// define possible values for LDAP search scope
  TLdapSearchScope = (
    SS_BaseObject,
    SS_SingleLevel,
    SS_WholeSubtree
  );

  /// define possible values about LDAP alias dereferencing
  TLdapSearchAliases = (
    SA_NeverDeref,
    SA_InSearching,
    SA_FindingBaseObj,
    SA_Always
  );

  /// parent class of application protocol implementations
  // By this class is defined common properties
  TAbstractClient = Class(TObject)
  protected
    fTargetHost: RawUtf8;
    fTargetPort: RawUtf8;
    fIPInterface: RawUtf8;
    fTimeout: integer;
    fUserName: RawUtf8;
    fPassword: RawUtf8;
  public
    /// initialize the instance
    constructor Create;
    /// target server IP (or symbolic name)
    // - default is 'localhost'
    property TargetHost: RawUtf8
      read fTargetHost Write fTargetHost;
    /// target server port (or symbolic name)
    property TargetPort: RawUtf8
      read fTargetPort Write fTargetPort;
    /// local socket address (outgoing IP address)
    // - default is '0.0.0.0' as wildcard for default IP
    property IPInterface: RawUtf8
      read fIPInterface Write fIPInterface;
    /// milliseconds timeout for socket operations
    // - default is 5000, ie. 5 seconds
    property Timeout: integer
      read fTimeout Write fTimeout;
    /// if protocol needs user authorization, then fill here user name
    property UserName: RawUtf8
      read fUserName Write fUserName;
    /// if protocol needs user authorization, then fill here its password
    property Password: RawUtf8
      read fPassword Write fPassword;
  end;

  /// we defined our own type to hold an ASN object binary
  TAsnObject = RawByteString;

  /// implementation of LDAP client version 2 and 3
  // - Authentication use parent TAbstractClient Username/Password properties
  // - Server/Port use parent TAbstractClient TargetHost/TargetPort properties
  TLdapClient = class(TAbstractClient)
  private
    fSock: TCrtSocket;
    fResultCode: integer;
    fResultString: RawUtf8;
    fFullResult: TAsnObject;
    fFullTls: boolean;
    fTlsContext: PNetTlsContext;
    fSeq: integer;
    fResponseCode: integer;
    fResponseDN: RawUtf8;
    fReferals: TRawUtf8List;
    fVersion: integer;
    fSearchScope: TLdapSearchScope;
    fSearchAliases: TLdapSearchAliases;
    fSearchSizeLimit: integer;
    fSearchTimeLimit: integer;
    fSearchPageSize: integer;
    fSearchCookie: RawUtf8;
    fSearchResult: TLdapResultList;
    fExtName: RawUtf8;
    fExtValue: RawUtf8;
    function Connect: boolean;
    function BuildPacket(const Asn1Data: TAsnObject): TAsnObject;
    procedure SendPacket(const Asn1Data: TAsnObject);
    function ReceiveResponse: TAsnObject;
    function DecodeResponse(const Asn1Response: TAsnObject): TAsnObject;
    function LdapSasl(const Value: RawUtf8): RawUtf8;
    function TranslateFilter(const Filter: RawUtf8): TAsnObject;
    class function GetErrorString(ErrorCode: integer): RawUtf8;
    function ReceiveString(Size: integer): RawByteString;
  public
    /// initialize this LDAP client instance
    constructor Create;
    /// finalize this LDAP client instance
    destructor Destroy; override;
    /// try to connect to LDAP server and start secure channel when it is required
    function Login: boolean;
    /// try to bind to the LDAP server with Username/Password
    // - if this is empty strings, then it does annonymous binding
    // - when you not call Bind on LDAPv3, then anonymous mode is used
    // - warning: uses plaintext transport of password: it is secure only on TLS
    function Bind: boolean;
    /// try to bind to the LDAP server with Username/Password
    // - when you not call Bind on LDAPv3, then anonymous mode is used
    // - warning: uses MD5 digest for password: it is really secure only on TLS
    function BindSasl: boolean;
    /// close connection to the LDAP server
    function Logout: boolean;

    /// modify content of a LDAP attribute on this object
    function Modify(const Obj: RawUtf8; Op: TLdapModifyOp;
      Value: TLdapAttribute): boolean;
    /// add list of attributes to specified object
    function Add(const Obj: RawUtf8; Value: TLdapAttributeList): boolean;
    /// delete this LDAP object from server
    function Delete(const Obj: RawUtf8): boolean;
    /// modify the object name of this LDAP object
    function ModifyDN(const obj, newRdn, newSuperior: RawUtf8;
      DeleteOldRdn: boolean): boolean;
    /// try to compare Attribute value with this LDAP object
    function Compare(const Obj, AttributeValue: RawUtf8): boolean;
    /// search LDAP base for LDAP objects using a given Filter
    function Search(const BaseDN: RawUtf8; TypesOnly: boolean;
      Filter: RawUtf8; const Attributes: array of RawByteString): boolean;
    /// call any LDAPv3 extended command
    function Extended(const Oid, Value: RawUtf8): boolean;
    /// the version of LDAP protocol used
    // - default value is 3
    property Version: integer
      read fVersion Write fVersion;
    /// contains the result code of the last LDAP operation
    property ResultCode: integer
      read fResultCode;
    /// human readable description of the last LDAP operation
    property ResultString: RawUtf8
      read fResultString;
    /// binary string of the last full response from LDAP server
    // - This string is encoded by ASN.1 BER encoding
    // - You need this only for debugging
    property FullResult: TAsnObject
      read fFullResult;
    /// if connection to the LDAP server is through TLS tunnel
    property FullTls: boolean
      read fFullTls Write fFullTls;
    /// optional advanced options for FullTls = true
    property TlsContext: PNetTlsContext
      read fTlsContext write fTlsContext;
    /// sequence number of the last LDAP command
    // - incremented with any LDAP command
    property Seq: integer
      read fSeq;
    /// the search scope used in search command
    property SearchScope: TLdapSearchScope
      read fSearchScope Write fSearchScope;
    /// how to handle aliases in search command
    property SearchAliases: TLdapSearchAliases
      read fSearchAliases Write fSearchAliases;
    /// result size limit in search command (bytes)
    // - 0 means without size limit
    property SearchSizeLimit: integer
      read fSearchSizeLimit Write fSearchSizeLimit;
    /// search time limit in search command (seconds)
    // - 0 means without time limit
    property SearchTimeLimit: integer
      read fSearchTimeLimit Write fSearchTimeLimit;
    /// number of results to return per search request
    // - 0 means no paging
    property SearchPageSize: integer
      read fSearchPageSize Write fSearchPageSize;
    /// cookie returned by paged search results
    // - use an empty string for the first search request
    property SearchCookie: RawUtf8
      read fSearchCookie Write fSearchCookie;
    /// result of the search command
    property SearchResult: TLdapResultList read fSearchResult;
    /// each LDAP operation on server can return some referals URLs
    property Referals: TRawUtf8List
      read fReferals;
    /// on Extended operation, here is the result Name asreturned by server
    property ExtName: RawUtf8
      read fExtName;
    /// on Extended operation, here is the result Value as returned by server
    property ExtValue: RawUtf8 read
      fExtValue;
    /// raw TCP socket used by all LDAP operations
    property Sock: TCrtSocket
      read fSock;
  end;



implementation


{ ****** Support procedures and functions ****** }

function UnquoteStr(const Value: RawUtf8): RawUtf8;
begin
  if (Value = '') or
     (Value[1] <> '"') then
    result := Value
  else
    UnQuoteSqlStringVar(pointer(Value), result);
end;

function IsBinaryString(const Value: RawByteString): boolean;
var
  n: PtrInt;
begin
  result := true;
  for n := 1 to length(Value) do
    if ord(Value[n]) in [0..8, 10..31] then
      // consider null-terminated strings as non-binary
      if (n <> length(value)) or
         (Value[n] = #0) then
        exit;
  result := false;
end;

function UnQuoteByBegin(const UpValue: RawUtf8; List: TRawUtf8List): RawUtf8;
var
  p: PPUtf8CharArray;
  i: PtrInt;
begin
  p := List.TextPtr;
  for i := 0 to List.Count - 1 do
    if IdemPChar(p[i], pointer(UpValue)) then
    begin
      result := p[i];
      delete(result, 1, length(UpValue));
      result := UnQuoteStr(TrimU(result));
      exit;
    end;
  result := '';
end;

function SeparateLeft(const Value: RawUtf8; Delimiter: AnsiChar): RawUtf8;
var
  x: PtrInt;
begin
  x := PosExChar(Delimiter, Value);
  if x = 0 then
    result := Value
  else
    result := copy(Value, 1, x - 1);
end;

function SeparateRight(const Value: RawUtf8; Delimiter: AnsiChar): RawUtf8;
var
  x: PtrInt;
begin
  x := PosExChar(Delimiter, Value);
  result := copy(Value, x + 1, length(Value) - x);
end;

function SeparateRightU(const Value, Delimiter: RawUtf8): RawUtf8;
var
  x: PtrInt;
begin
  x := mormot.core.base.PosEx(Delimiter, Value);
  TrimCopy(Value, x + 1, length(Value) - x, result);
end;

function GetBetween(PairBegin, PairEnd: AnsiChar; const Value: RawUtf8): RawUtf8;
var
  n, len, x: PtrInt;
  s: RawUtf8;
begin
  n := length(Value);
  if (n = 2) and
     (Value[1] = PairBegin) and
     (Value[2] = PairEnd) then
  begin
    result := '';//nothing between
    exit;
  end;
  if n < 2 then
  begin
    result := Value;
    exit;
  end;
  s := SeparateRight(Value, PairBegin);
  if s = Value then
  begin
    result := Value;
    exit;
  end;
  n := PosExChar(PairEnd, s);
  if n = 0 then
  begin
    result := Value;
    exit;
  end;
  len := length(s);
  x := 1;
  for n := 1 to len do
  begin
    if s[n] = PairBegin then
      inc(x)
    else if s[n] = PairEnd then
    begin
      dec(x);
      if x <= 0 then
      begin
        len := n - 1;
        break;
      end;
    end;
  end;
  result := copy(s, 1, len);
end;

function DecodeTriplet(const Value: RawUtf8; Delimiter: AnsiChar): RawUtf8;
var
  x, l, lv: integer;
  c: AnsiChar;
  b: byte;
  bad: boolean;
begin
  lv := length(Value);
  SetLength(result, lv);
  x := 1;
  l := 1;
  while x <= lv do
  begin
    c := Value[x];
    inc(x);
    if c <> Delimiter then
    begin
      result[l] := c;
      inc(l);
    end
    else
      if x < lv then
      begin
        case Value[x] of
          #13:
            if Value[x + 1] = #10 then
              inc(x, 2)
            else
              inc(x);
          #10:
            if Value[x + 1] = #13 then
              inc(x, 2)
            else
              inc(x);
        else
          begin
            bad := false;
            case Value[x] of
              '0'..'9':
                b := (byte(Value[x]) - 48) shl 4;
              'a'..'f', 'A'..'F':
                b := ((byte(Value[x]) and 7) + 9) shl 4;
            else
              begin
                b := 0;
                bad := true;
              end;
            end;
            case Value[x + 1] of
              '0'..'9':
                b := b or (byte(Value[x + 1]) - 48);
              'a'..'f', 'A'..'F':
                b := b or ((byte(Value[x + 1]) and 7) + 9);
            else
              bad := true;
            end;
            if bad then
            begin
              result[l] := c;
              inc(l);
            end
            else
            begin
              inc(x, 2);
              result[l] := AnsiChar(b);
              inc(l);
            end;
          end;
        end;
      end
      else
        break;
  end;
  dec(l);
  SetLength(result, l);
end;

function TrimSPLeft(const S: RawUtf8): RawUtf8;
var
  i, l: integer;
begin
  result := '';
  if S = '' then
    exit;
  l := length(S);
  i := 1;
  while (i <= l) and
        (S[i] = ' ') do
    inc(i);
  result := copy(S, i, Maxint);
end;

function TrimSPRight(const S: RawUtf8): RawUtf8;
var
  i: integer;
begin
  result := '';
  if S = '' then
    exit;
  i := length(S);
  while (i > 0) and
        (S[i] = ' ') do
    dec(i);
  result := copy(S, 1, i);
end;

function TrimSP(const S: RawUtf8): RawUtf8;
begin
  result := TrimSPRight(TrimSPLeft(s));
end;

function FetchBin(var Value: RawUtf8; Delimiter: AnsiChar): RawUtf8;
var
  s: RawUtf8;
begin
  result := SeparateLeft(Value, Delimiter);
  s := SeparateRight(Value, Delimiter);
  if s = Value then
    Value := ''
  else
    Value := s;
end;

function Fetch(var Value: RawUtf8; Delimiter: AnsiChar): RawUtf8;
begin
  result := TrimSP(FetchBin(Value, Delimiter));
  Value := TrimSP(Value);
end;



{ ****** ASN.1 BER encoding/decoding ****** }

const
  ASN1_BOOL = $01;
  ASN1_INT = $02;
  ASN1_OCTSTR = $04;
  ASN1_NULL = $05;
  ASN1_OBJID = $06;
  ASN1_ENUM = $0a;
  ASN1_SEQ = $30;
  ASN1_SETOF = $31;
  ASN1_IPADDR = $40;
  ASN1_COUNTER = $41;
  ASN1_GAUGE = $42;
  ASN1_TIMETICKS = $43;
  ASN1_OPAQUE = $44;
  ASN1_COUNTER64 = $46;

  LDAP_ASN1_BIND_REQUEST = $60;
  LDAP_ASN1_BIND_RESPONSE = $61;
  LDAP_ASN1_UNBIND_REQUEST = $42;
  LDAP_ASN1_SEARCH_REQUEST = $63;
  LDAP_ASN1_SEARCH_ENTRY = $64;
  LDAP_ASN1_SEARCH_DONE = $65;
  LDAP_ASN1_SEARCH_REFERENCE = $73;
  LDAP_ASN1_MODIFY_REQUEST = $66;
  LDAP_ASN1_MODIFY_RESPONSE = $67;
  LDAP_ASN1_ADD_REQUEST = $68;
  LDAP_ASN1_ADD_RESPONSE = $69;
  LDAP_ASN1_DEL_REQUEST = $4A;
  LDAP_ASN1_DEL_RESPONSE = $6B;
  LDAP_ASN1_MODIFYDN_REQUEST = $6C;
  LDAP_ASN1_MODIFYDN_RESPONSE = $6D;
  LDAP_ASN1_COMPARE_REQUEST = $6E;
  LDAP_ASN1_COMPARE_RESPONSE = $6F;
  LDAP_ASN1_ABANDON_REQUEST = $70;
  LDAP_ASN1_EXT_REQUEST = $77;
  LDAP_ASN1_EXT_RESPONSE = $78;
  LDAP_ASN1_CONTROLS = $A0;

  cLDAPProtocol = '389';


function AsnEncOidItem(Value: Int64): RawByteString;
var
  r: PByte;
begin
  FastSetRawByteString(result, nil, 16);
  r := pointer(result);
  r^ := byte(Value) and $7f;
  inc(r);
  Value := Value shr 7;
  while Value <> 0 do
  begin
    r^ := byte(Value) or $80;
    inc(r);
    Value := Value shr 7;
  end;
  FakeLength(result, PAnsiChar(r) - pointer(result));
end;

function AsnDecOidItem(var Pos: integer; const Buffer: RawByteString): integer;
var
  x: byte;
begin
  result := 0;
  repeat
    result := result shl 7;
    x := ord(Buffer[Pos]);
    inc(Pos);
    inc(result, x and $7F);
  until (x and $80) = 0;
end;

function AsnEncLen(Len: cardinal; dest: PByte): PtrInt;
var
  n: PtrInt;
  tmp: array[0..7] of byte;
begin
  if Len < $80 then
  begin
    dest^ := Len;
    result := 1;
    exit;
  end;
  n := 0;
  repeat
    tmp[n] := byte(Len);
    inc(n);
    Len := Len shr 8;
  until Len = 0;
  result := n + 1;
  dest^ := byte(n) xor $80; // first byte is number of following bytes + $80
  repeat
    inc(dest);
    dec(n);
    dest^ := tmp[n]; // stored as big endian
  until n = 0;
end;

function AsnDecLen(var Start: integer; const Buffer: RawByteString): cardinal;
var
  n: byte;
begin
  result := ord(Buffer[Start]);
  inc(Start);
  if result < $80 then
    exit;
  n := result and $7f;
  result := 0;
  repeat
    result := (result shl 8) + cardinal(Buffer[Start]);
    inc(Start);
    dec(n);
  until n = 0;
end;

function AsnEncInt(Value: Int64): RawByteString;
var
  y: byte;
  neg: boolean;
  n: PtrInt;
  p: PByte;
  tmp: array[0..15] of byte;
begin
  result := '';
  neg := Value < 0;
  Value := Abs(Value);
  if neg then
    dec(Value);
  n := 0;
  repeat
    y := byte(Value);
    if neg then
      y := not y;
    tmp[n] := y;
    inc(n);
    Value := Value shr 8;
  until Value = 0;
  if neg then
  begin
    if y <= $7f then
    begin
      tmp[n] := $ff; // negative numbers start with ff or 8x
      inc(n);
    end;
  end
  else if y > $7F then
  begin
    tmp[n] := 0; // positive numbers start with a 0 or 0x..7x
    inc(n);
  end;
  FastSetRawByteString(result, nil, n);
  p := pointer(result);
  repeat
    dec(n);
    p^ := tmp[n]; // stored as big endian
    inc(p);
  until n = 0;
end;

function Asn(AsnType: integer;
  const Content: array of RawByteString): TAsnObject; overload;
var
  tmp: array[0..7] of byte;
  i, len, al: PtrInt;
  p: PByte;
begin
  len := 0;
  for i := 0 to high(Content) do
    inc(len, length(Content[i]));
  al := AsnEncLen(len, @tmp);
  SetString(result, nil, 1 + al + len);
  p := pointer(result);
  p^ := AsnType;         // type
  inc(p);
  MoveFast(tmp, p^, al); // encoded length
  inc(p, al);
  for i := 0 to high(Content) do
  begin
    len := length(Content[i]);
    MoveFast(pointer(Content[i])^, p^, len); // content
    inc(p, len);
  end;
end;

function Asn(const Data: RawByteString; AsnType: integer = ASN1_OCTSTR): TAsnObject;
  overload; {$ifdef HASINLINE} inline; {$endif}
begin
  result := Asn(AsnType, [Data]);
end;

function Asn(Value: Int64; AsnType: integer = ASN1_INT): TAsnObject; overload;
begin
  result := Asn(AsnType, [AsnEncInt(Value)]);
end;

function Asn(Value: boolean): TAsnObject; overload;
var
  i: integer;
begin
  if Value then
    i := $ff
  else
    i := 0;
  result := Asn(ASN1_BOOL, [AsnEncInt(i)]);
end;

function AsnSeq(const Data: RawByteString): TAsnObject;
begin
  result := Asn(ASN1_SEQ, [Data]);
end;

procedure AsnAdd(var Data: TAsnObject; const Buffer: RawByteString);
  overload; {$ifdef HASINLINE} inline; {$endif}
begin
  AppendBufferToRawByteString(Data, Buffer);
end;

procedure AsnAdd(var Data: TAsnObject; const Buffer: RawByteString;
  AsnType: integer); overload;
begin
  AppendBufferToRawByteString(Data, Asn(AsnType, [Buffer]));
end;

function IdToMib(Pos, EndPos: integer; const Buffer: RawByteString): RawUtf8;
var
  x, y: integer;
begin
  result := '';
  while Pos < EndPos do
  begin
    x := AsnDecOidItem(Pos, Buffer);
    if Pos = 2 then
    begin
      y := x div 40; // first byte = two first numbers modulo 40
      x := x mod 40;
      UInt32ToUtf8(y, result);
    end;
    Append(result, ['.', x]);
  end;
end;

function AsnNext(var Pos: integer; const Buffer: TAsnObject;
  var ValueType: integer): RawByteString;
var
  asntype, asnsize, n, l: integer;
  y: int64;
  x: byte;
  neg: boolean;
begin
  result := '';
  ValueType := ASN1_NULL;
  l := length(Buffer);
  if Pos > l then
    exit;
  asntype := ord(Buffer[Pos]);
  ValueType := asntype;
  inc(Pos);
  asnsize := AsnDecLen(Pos, Buffer);
  if (Pos + asnsize - 1) > l then
    exit;
  if (asntype and $20) <> 0 then
    result := copy(Buffer, Pos, asnsize)
  else
    case asntype of
      ASN1_INT,
      ASN1_ENUM,
      ASN1_BOOL:
        begin
          y := 0;
          neg := false;
          for n := 1 to asnsize do
          begin
            x := ord(Buffer[Pos]);
            if (n = 1) and
               (x > $7F) then
              neg := true;
            if neg then
              x := not x;
            y := (y shl 8) + x;
            inc(Pos);
          end;
          if neg then
            y := -(y + 1);
          result := ToUtf8(y);
        end;
      ASN1_COUNTER,
      ASN1_GAUGE,
      ASN1_TIMETICKS,
      ASN1_COUNTER64:
        begin
          y := 0;
          for n := 1 to asnsize do
          begin
            y := (y shl 8) + ord(Buffer[Pos]);
            inc(Pos);
          end;
          result := ToUtf8(y);
        end;
      ASN1_OBJID:
        begin
          result := IdToMib(Pos, Pos + asnsize, Buffer);
          inc(Pos, asnsize);
        end;
      ASN1_IPADDR:
        begin
          case asnsize of
            4:
              IP4Text(pointer(@Buffer[Pos]), RawUtf8(result));
            16:
              IP6Text(pointer(@Buffer[Pos]), RawUtf8(result));
          else
            BinToHexLower(@Buffer[Pos], asnsize, RawUtf8(result));
          end;
          inc(Pos, asnsize);
        end;
      ASN1_NULL:
        inc(Pos, asnsize);
    else
      // ASN1_OCTSTR, ASN1_OPAQUE or unknown: return as raw binary
      begin
        result := copy(Buffer, Pos, asnsize);
        inc(Pos, asnsize);
      end;
    end;
end;

{$ifdef ASNDEBUG} // not used nor fully tested

function IntMibToStr(const Value: RawByteString): RawUtf8;
var
  i, y: integer;
begin
  y := 0;
  for i := 1 to length(Value) - 1 do
    y := (y shl 8) + ord(Value[i]);
  UInt32ToUtf8(y, result);
end;

function MibToId(Mib: RawUtf8): RawByteString;
var
  x: integer;

  function WalkInt(var s: RawUtf8): integer;
  var
    x: integer;
    t: RawByteString;
  begin
    x := PosExChar('.', s);
    if x < 1 then
    begin
      t := s;
      s := '';
    end
    else
    begin
      t := copy(s, 1, x - 1);
      s := copy(s, x + 1, length(s) - x);
    end;
    result := Utf8ToInteger(t, 0);
  end;

begin
  result := '';
  x := WalkInt(Mib);
  x := x * 40 + WalkInt(Mib);
  result := AsnEncOidItem(x);
  while Mib <> '' do
  begin
    x := WalkInt(Mib);
    Append(result, [AsnEncOidItem(x)]);
  end;
end;

function AsnEncUInt(Value: integer): RawByteString;
var
  x, y: integer;
  neg: boolean;
begin
  neg := Value < 0;
  x := Value;
  if neg then
    x := x and $7FFFFFFF;
  result := '';
  repeat
    y := x mod 256;
    x := x div 256;
    Prepend(result, [AnsiChar(y)]);
  until x = 0;
  if neg then
    result[1] := AnsiChar(ord(result[1]) or $80);
end;

function DumpExStr(const Buffer: RawByteString): RawUtf8;
var
  n: integer;
  x: byte;
begin
  result := '';
  for n := 1 to length(Buffer) do
  begin
    x := ord(Buffer[n]);
    if x in [65..90, 97..122] then
      Append(result, [' +''', AnsiChar(x), ''''])
    else
      Append(result, [' +#$', BinToHexDisplayLowerShort(@x, 1)]);
  end;
end;

function AsnDump(const Value: TAsnObject): RawUtf8;
var
  i, at, x, n: integer;
  s, indent: RawUtf8;
  il: TIntegerDynArray;
begin
  result := '';
  i := 1;
  indent := '';
  while i < length(Value) do
  begin
    for n := length(il) - 1 downto 0 do
    begin
      x := il[n];
      if x <= i then
      begin
        DeleteInteger(il, n);
        Delete(indent, 1, 2);
      end;
    end;
    s := AsnNext(i, Value, at);
    Append(result, [indent, '$', IntToHex(at, 2)]);
    if (at and $20) > 0 then
    begin
      x := length(s);
      Append(result, [' constructed: length ', x]);
      Append(indent, ['  ']);
      AddInteger(il, x + i - 1);
    end
    else
    begin
      case at of
        ASN1_BOOL:
          AppendToRawUtf8(result, ' BOOL: ');
        ASN1_INT:
          AppendToRawUtf8(result, ' INT: ');
        ASN1_ENUM:
          AppendToRawUtf8(result, ' ENUM: ');
        ASN1_COUNTER:
          AppendToRawUtf8(result, ' COUNTER: ');
        ASN1_GAUGE:
          AppendToRawUtf8(result, ' GAUGE: ');
        ASN1_TIMETICKS:
          AppendToRawUtf8(result, ' TIMETICKS: ');
        ASN1_OCTSTR:
          AppendToRawUtf8(result, ' OCTSTR: ');
        ASN1_OPAQUE:
          AppendToRawUtf8(result, ' OPAQUE: ');
        ASN1_OBJID:
          AppendToRawUtf8(result, ' OBJID: ');
        ASN1_IPADDR:
          AppendToRawUtf8(result, ' IPADDR: ');
        ASN1_NULL:
          AppendToRawUtf8(result, ' NULL: ');
        ASN1_COUNTER64:
          AppendToRawUtf8(result, ' COUNTER64: ');
      else // other
        AppendToRawUtf8(result, ' unknown: ');
      end;
      if IsBinaryString(s) then
        s := DumpExStr(s);
      AppendToRawUtf8(result, s);
    end;
    AppendCharToRawUtf8(result, #$0d);
    AppendCharToRawUtf8(result, #$0a);
  end;
end;

{$endif ASNDEBUG}


{ **************** LDAP Response Storage }

{ TLdapAttribute }

constructor TLdapAttribute.Create(const AttrName: RawUtf8);
begin
  inherited Create;
  fAttributeName := AttrName;
  fIsBinary := StrPosI(';BINARY', pointer(AttrName)) <> nil;
end;

procedure TLdapAttribute.Add(const aValue: RawByteString);
begin
  AddRawUtf8(fList, fCount, aValue);
end;

function TLdapAttribute.GetReadable(index: PtrInt): RawUtf8;
begin
  if (self = nil) or
     (index >= fCount) then
    result := ''
  else
  begin
    result := fList[index];
    if fIsBinary then
      result := BinToBase64(result)
    else if IsBinaryString(result) then
      result := LogEscapeFull(result);
  end;
end;

function TLdapAttribute.GetRaw(index: PtrInt): RawByteString;
begin
  if (self = nil) or
     (index >= fCount) then
    result := ''
  else
    result := fList[index];
end;


{ TLdapAttributeList }

destructor TLdapAttributeList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TLdapAttributeList.Clear;
begin
  ObjArrayClear(fItems);
end;

function TLdapAttributeList.Count: integer;
begin
  result := length(fItems);
end;

function TLdapAttributeList.FindIndex(const AttributeName: RawUtf8): PtrInt;
begin
  if self <> nil then
    for result := 0 to length(fItems) - 1 do
      if IdemPropNameU(fItems[result].AttributeName, AttributeName) then
        exit;
  result := -1;
end;

function TLdapAttributeList.Find(const AttributeName: RawUtf8): TLdapAttribute;
var
  i: PtrInt;
begin
  i := FindIndex(AttributeName);
  if i >= 0 then
    result := fItems[i]
  else
    result := nil;
end;

function TLdapAttributeList.Get(const AttributeName: RawUtf8): RawUtf8;
begin
  result := Find(AttributeName).GetReadable(0);
end;

function TLdapAttributeList.Add(const AttributeName: RawUtf8): TLdapAttribute;
begin
  result := TLdapAttribute.Create(AttributeName);
  ObjArrayAdd(fItems, result);
end;

procedure TLdapAttributeList.Delete(const AttributeName: RawUtf8);
begin
  ObjArrayDelete(fItems, FindIndex(AttributeName));
end;


{ TLdapResult }

constructor TLdapResult.Create;
begin
  inherited Create;
  fAttributes := TLdapAttributeList.Create;
end;

destructor TLdapResult.Destroy;
begin
  fAttributes.Free;
  inherited Destroy;
end;


{ TLdapResultList }

destructor TLdapResultList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TLdapResultList.Clear;
begin
  ObjArrayClear(fItems, fCount);
end;

function TLdapResultList.Add: TLdapResult;
begin
  result := TLdapResult.Create;
  ObjArrayAddCount(fItems, result, fCount);
end;

function TLdapResultList.Dump: RawUtf8;
var
  i, j, k: PtrInt;
  res: TLdapResult;
  attr: TLdapAttribute;
begin
  result := 'results: ' + ToUtf8(Count) + CRLF + CRLF;
  for i := 0 to Count - 1 do
  begin
    result := result + 'result: ' + ToUtf8(i) + CRLF;
    res := Items[i];
    result := result + '  Object: ' + res.ObjectName + CRLF;
    for j := 0 to res.Attributes.Count - 1 do
    begin
      attr := res.Attributes.Items[j];
      result := result + '  Attribute: ' + attr.AttributeName + CRLF;
      for k := 0 to attr.Count - 1 do
        result := result + '    ' + attr.GetReadable(k) + CRLF;
    end;
  end;
end;


{ **************** LDAP Client Class }

{ TAbstractClient }

constructor TAbstractClient.Create;
begin
  inherited Create;
  fIPInterface := cAnyHost;
  fTargetHost := cLocalhost;
  fTargetPort := cAnyPort;
  fTimeout := 5000;
end;


{ TLdapClient }

constructor TLdapClient.Create;
begin
  inherited Create;
  fReferals := TRawUtf8List.Create;
  fTimeout := 60000;
  fTargetPort := cLDAPProtocol;
  fVersion := 3;
  fSearchScope := SS_WholeSubtree;
  fSearchAliases := SA_Always;
  fSearchResult := TLdapResultList.Create;
end;

destructor TLdapClient.Destroy;
begin
  fSock.Free;
  fSearchResult.Free;
  fReferals.Free;
  inherited Destroy;
end;

class function TLdapClient.GetErrorString(ErrorCode: integer): RawUtf8;
begin
  case ErrorCode of
    0:
      result := 'Success';
    1:
      result := 'Operations error';
    2:
      result := 'Protocol error';
    3:
      result := 'Time limit Exceeded';
    4:
      result := 'Size limit Exceeded';
    5:
      result := 'Compare false';
    6:
      result := 'Compare true';
    7:
      result := 'Auth method not supported';
    8:
      result := 'Strong auth required';
    9:
      result := '-- reserved --';
    10:
      result := 'Referal';
    11:
      result := 'Admin limit exceeded';
    12:
      result := 'Unavailable critical extension';
    13:
      result := 'Confidentality required';
    14:
      result := 'Sasl bind in progress';
    16:
      result := 'No such attribute';
    17:
      result := 'Undefined attribute type';
    18:
      result := 'Inappropriate matching';
    19:
      result := 'Constraint violation';
    20:
      result := 'Attribute or value exists';
    21:
      result := 'Invalid attribute syntax';
    32:
      result := 'No such object';
    33:
      result := 'Alias problem';
    34:
      result := 'Invalid DN syntax';
    36:
      result := 'Alias dereferencing problem';
    48:
      result := 'Inappropriate authentication';
    49:
      result := 'Invalid credentials';
    50:
      result := 'Insufficient access rights';
    51:
      result := 'Busy';
    52:
      result := 'Unavailable';
    53:
      result := 'Unwilling to perform';
    54:
      result := 'Loop detect';
    64:
      result := 'Naming violation';
    65:
      result := 'Object class violation';
    66:
      result := 'Not allowed on non leaf';
    67:
      result := 'Not allowed on RDN';
    68:
      result := 'Entry already exists';
    69:
      result := 'Object class mods prohibited';
    71:
      result := 'Affects multiple DSAs';
    80:
      result := 'Other';
  else
    FormatUtf8('unknown #%', [ErrorCode], result);
  end;
end;

function TLdapClient.ReceiveString(Size: integer): RawByteString;
begin
  FastSetRawByteString(result, nil, Size);
  fSock.SockInRead(pointer(result), Size);
end;

function TLdapClient.Connect: boolean;
begin
  // do not call this function! It is calling by LOGIN method!
  FreeAndNil(fSock);
  result := false;
  fSeq := 0;
  try
    fSock := TCrtSocket.Open(
      fTargetHost, fTargetPort, nlTcp, 5000, fFullTls, fTlsContext);
    fSock.CreateSockIn;
    result := fSock.SockConnected;
  except
    on E: ENetSock do
      FreeAndNil(fSock);
  end;
end;

function TLdapClient.BuildPacket(const Asn1Data: TAsnObject): TAsnObject;
begin
  inc(fSeq);
  result := Asn(ASN1_SEQ, [
    Asn(fSeq),
    Asn1Data]);
end;

procedure TLdapClient.SendPacket(const Asn1Data: TAsnObject);
begin
  fSock.SockSendFlush(BuildPacket(Asn1Data));
end;

function TLdapClient.ReceiveResponse: TAsnObject;
var
  b: byte;
  len, pos: integer;
begin
  result := '';
  fFullResult := '';
  try
    // receive ASN type
    fSock.SockInRead(pointer(@b), 1);
    if b <> ASN1_SEQ then
      exit;
    result := AnsiChar(b);
    // receive length
    fSock.SockInRead(pointer(@b), 1);
    AppendBufferToRawByteString(result, b, 1);
    if b >= $80 then // $8x means x bytes of length
      AsnAdd(result, ReceiveString(b and $7f));
    // decode length of LDAP packet
    pos := 2;
    len := AsnDecLen(pos, result);
    // retrieve rest of LDAP packet
    if len > 0 then
      AsnAdd(result, ReceiveString(len));
  except
    on E: ENetSock do
    begin
      result := '';
      exit;
    end;
  end;
  fFullResult := result;
end;

function TLdapClient.DecodeResponse(const Asn1Response: TAsnObject): TAsnObject;
var
  i, x, numseq: integer;
  asntype: integer;
  s, t: TAsnObject;
begin
  result := '';
  fResultCode := -1;
  fResultString := '';
  fResponseCode := -1;
  fResponseDN := '';
  fReferals.Clear;
  i := 1;
  AsnNext(i, Asn1Response, asntype);
  numseq := Utf8ToInteger(AsnNext(i, Asn1Response, asntype), 0);
  if (asntype <> ASN1_INT) or
     (numseq <> fSeq) then
    exit;
  s := AsnNext(i, Asn1Response, asntype);
  fResponseCode := asntype;
  if fResponseCode in [LDAP_ASN1_BIND_RESPONSE, LDAP_ASN1_SEARCH_DONE,
    LDAP_ASN1_MODIFY_RESPONSE, LDAP_ASN1_ADD_RESPONSE, LDAP_ASN1_DEL_RESPONSE,
    LDAP_ASN1_MODIFYDN_RESPONSE, LDAP_ASN1_COMPARE_RESPONSE,
    LDAP_ASN1_EXT_RESPONSE] then
  begin
    fResultCode := Utf8ToInteger(AsnNext(i, Asn1Response, asntype), -1);
    fResponseDN := AsnNext(i, Asn1Response, asntype);
    fResultString := AsnNext(i, Asn1Response, asntype);
    if fResultString = '' then
      fResultString := GetErrorString(fResultCode);
    if fResultCode = 10 then
    begin
      s := AsnNext(i, Asn1Response, asntype);
      if asntype = $A3 then
      begin
        x := 1;
        while x < length(s) do
        begin
          t := AsnNext(x, s, asntype);
          fReferals.Add(t);
        end;
      end;
    end;
  end;
  result := copy(Asn1Response, i, length(Asn1Response) - i + 1); // body
end;

function TLdapClient.LdapSasl(const Value: RawUtf8): RawUtf8;
var
  s, a0, a1, a2, nonce, cnonce, nc, realm, qop, uri, response: RawUtf8;
  l: TRawUtf8List;
begin
  l := TRawUtf8List.Create;
  try
    l.SetText(Value, ',');
    nonce := UnQuoteByBegin('NONCE=', l);
    realm := UnQuoteByBegin('REALM=', l);
  finally
    l.Free;
  end;
  cnonce := Int64ToHex(GetTickCount64);
  nc := '00000001';
  qop := 'auth';
  uri := 'ldap/' + fSock.RemoteIP;
  FormatUtf8('%:%:%', [fUserName, realm, fPassword], a0);
  FormatUtf8('%:%:%', [md5(a0), nonce, cnonce], a1);
  FormatUtf8('AUTHENTICATE:%', [uri], a2);
  FormatUtf8('%:%:%:%:%:%',
    [BinToHex(md5(a1)), nonce, nc, cnonce, qop, BinToHex(md5(a2))], s);
  response := BinToHex(md5(s));
  FormatUtf8('username="%",realm="%",nonce="%",cnonce="%",nc=%,qop=%,' +
    'digest-uri="%",response=%',
    [fUserName, realm, nonce, cnonce, nc, qop, uri, response], result);
end;

function TLdapClient.TranslateFilter(const Filter: RawUtf8): TAsnObject;
var
  x, dn: integer;
  c: Ansichar;
  s, t, l, r, attr, rule: RawUtf8;
begin
  result := '';
  if Filter = '' then
    exit;
  s := Filter;
  if Filter[1] = '(' then
    for x := length(Filter) downto 2 do
      if Filter[x] = ')' then
      begin
        s := copy(Filter, 2, x - 2); // get value between (...)
        break;
      end;
  if s = '' then
    exit;
  case s[1] of
    '!':
      // NOT rule (recursive call)
      result := Asn(TranslateFilter(GetBetween('(', ')', s)), $A2);
    '&':
      // and rule (recursive call)
      begin
        repeat
          t := GetBetween('(', ')', s);
          s := SeparateRightU(s, t);
          if s <> '' then
            if s[1] = ')' then
              System.Delete(s, 1, 1);
          AsnAdd(result, TranslateFilter(t));
        until s = '';
        result := Asn(result, $A0);
      end;
    '|':
      // or rule (recursive call)
      begin
        repeat
          t := GetBetween('(', ')', s);
          s := SeparateRightU(s, t);
          if s <> '' then
            if s[1] = ')' then
              System.Delete(s, 1, 1);
          AsnAdd(result, TranslateFilter(t));
        until s = '';
        result := Asn(result, $A1);
      end;
    else
      begin
        l := TrimU(SeparateLeft(s, '='));
        r := TrimU(SeparateRight(s, '='));
        if l <> '' then
        begin
          c := l[length(l)];
          case c of
            ':':
              // Extensible match
              begin
                System.Delete(l, length(l), 1);
                dn := 0;
                attr := '';
                rule := '';
                if mormot.core.base.PosEx(':dn', l) > 0 then
                begin
                  dn := $ff; // true
                  l := StringReplaceAll(l, ':dn', '');
                end;
                attr := TrimU(SeparateLeft(l, ':'));
                rule := TrimU(SeparateRight(l, ':'));
                if rule = l then
                  rule := '';
                if rule <> '' then
                  result := Asn(rule, $81);
                if attr <> '' then
                  AsnAdd(result, attr, $82);
                AsnAdd(result, DecodeTriplet(r, '\'), $83);
                AsnAdd(result, AsnEncInt(dn), $84);
                result := Asn(result, $a9);
              end;
            '~':
              // Approx match
              begin
                System.Delete(l, length(l), 1);
                result := Asn($a8, [
                  Asn(l),
                  Asn(DecodeTriplet(r, '\'))]);
              end;
            '>':
              // Greater or equal match
              begin
                System.Delete(l, length(l), 1);
                result := Asn($a5, [
                   Asn(l),
                   Asn(DecodeTriplet(r, '\'))]);
              end;
            '<':
              // Less or equal match
              begin
                System.Delete(l, length(l), 1);
                result := Asn($a6, [
                   Asn(l),
                   Asn(DecodeTriplet(r, '\'))]);
              end;
          else
            // present
            if r = '*' then
              result := Asn(l, $87)
            else
              if PosExChar('*', r) > 0 then
              // substrings
              begin
                s := Fetch(r, '*');
                if s <> '' then
                  result := Asn(DecodeTriplet(s, '\'), $80);
                while r <> '' do
                begin
                  if PosExChar('*', r) <= 0 then
                    break;
                  s := Fetch(r, '*');
                  AsnAdd(result, DecodeTriplet(s, '\'), $81);
                end;
                if r <> '' then
                  AsnAdd(result, DecodeTriplet(r, '\'), $82);
                result := Asn($a4, [
                   Asn(l),
                   AsnSeq(result)]);
              end
              else
              begin
                // Equality match
                result := Asn($a3, [
                   Asn(l),
                   Asn(DecodeTriplet(r, '\'))]);
              end;
          end;
        end;
      end;
  end;
end;

function TLdapClient.Login: boolean;
begin
  result := false;
  if not Connect then
    exit;
  result := true;
end;

function TLdapClient.Bind: boolean;
var
  query: RawByteString;
begin
  query := Asn(LDAP_ASN1_BIND_REQUEST, [
             Asn(fVersion),
             Asn(fUserName),
             Asn(fPassword, $80)]);
  SendPacket(query);
  DecodeResponse(ReceiveResponse);
  result := fResultCode = 0;
end;

function TLdapClient.BindSasl: boolean;
var
  x, xt: integer;
  s, t, digreq: TAsnObject;
begin
  result := false;
  if fPassword = '' then
    result := Bind
  else
  begin
    digreq := Asn(LDAP_ASN1_BIND_REQUEST, [
                 Asn(fVersion),
                 Asn(''),
                 Asn($A3, [
                   Asn('DIGEST-MD5')])]);
    SendPacket(digreq);
    t := DecodeResponse(ReceiveResponse);
    if fResultCode = 14 then
    begin
      s := t;
      x := 1;
      t := AsnNext(x, s, xt);
      s := Asn(LDAP_ASN1_BIND_REQUEST, [
               Asn(fVersion),
               Asn(''),
               Asn($A3, [
                 Asn('DIGEST-MD5'),
                 Asn(LdapSasl(t))])]);
      SendPacket(s);
      DecodeResponse(ReceiveResponse);
      if fResultCode = 14 then
      begin
        SendPacket(digreq);
        s := ReceiveResponse;
        DecodeResponse(s);
      end;
      result := fResultCode = 0;
    end;
  end;
end;

function TLdapClient.Logout: boolean;
begin
  SendPacket(Asn('', LDAP_ASN1_UNBIND_REQUEST));
  FreeAndNil(fSock);
  result := true;
end;

function TLdapClient.Modify(const Obj: RawUtf8; Op: TLdapModifyOp;
  Value: TLdapAttribute): boolean;
var
  query, resp: TAsnObject;
  i: integer;
begin
  query := '';
  for i := 0 to Value.Count -1 do
    Append(query, [Asn(Value.GetRaw(i))]);
  query := Asn(LDAP_ASN1_MODIFY_REQUEST, [
    Asn(obj),
    Asn(ASN1_SEQ, [Asn(ASN1_SEQ, [
      Asn(ord(Op), ASN1_ENUM),
      Asn(ASN1_SEQ, [
        Asn(Value.AttributeName),
        Asn(query, ASN1_SETOF)])])])]);
  SendPacket(query);
  resp := ReceiveResponse;
  DecodeResponse(resp);
  result := fResultCode = 0;
end;

function TLdapClient.Add(const Obj: RawUtf8; Value: TLdapAttributeList): boolean;
var
  query, sub, resp: TAsnObject;
  attr: TLdapAttribute;
  i, j: PtrInt;
begin
  query := '';
  for i := 0 to Value.Count - 1 do
  begin
    attr := Value.Items[i];
    sub := '';
    for j := 0 to attr.Count - 1 do
      Append(sub, [Asn(attr.GetRaw(j))]);
    Append(query, [
      Asn(ASN1_SEQ, [
        Asn(attr.AttributeName),
        Asn(ASN1_SETOF, [sub])])]);
  end;
  query := Asn(LDAP_ASN1_ADD_REQUEST, [
             Asn(obj),
             AsnSeq(query)]);
  SendPacket(query);
  resp := ReceiveResponse;
  DecodeResponse(resp);
  result := fResultCode = 0;
end;

function TLdapClient.Delete(const Obj: RawUtf8): boolean;
var
  query, resp: TAsnObject;
begin
  query := Asn(obj, LDAP_ASN1_DEL_REQUEST);
  SendPacket(query);
  resp := ReceiveResponse;
  DecodeResponse(resp);
  result := fResultCode = 0;
end;

function TLdapClient.ModifyDN(const obj, newRdn, newSuperior: RawUtf8;
  DeleteOldRdn: boolean): boolean;
var
  query, resp: TAsnObject;
begin
  query := Asn(obj);
  Append(query, [Asn(newRdn), Asn(DeleteOldRdn)]);
  if newSuperior <> '' then
    Append(query, [Asn(newSuperior, $80)]);
  query := Asn(query, LDAP_ASN1_MODIFYDN_REQUEST);
  SendPacket(query);
  resp := ReceiveResponse;
  DecodeResponse(resp);
  result := fResultCode = 0;
end;

function TLdapClient.Compare(const Obj, AttributeValue: RawUtf8): boolean;
var
  query, resp: TAsnObject;
begin
  query := Asn(LDAP_ASN1_COMPARE_REQUEST, [
    Asn(obj),
    Asn(ASN1_SEQ, [
      Asn(TrimU(SeparateLeft(AttributeValue, '='))),
      Asn(TrimU(SeparateRight(AttributeValue, '=')))])]);
  SendPacket(query);
  resp := ReceiveResponse;
  DecodeResponse(resp);
  result := fResultCode = 0;
end;

function TLdapClient.Search(const BaseDN: RawUtf8; TypesOnly: boolean;
  Filter: RawUtf8; const Attributes: array of RawByteString): boolean;
var
  s, t, c: TAsnObject;
  u: RawUtf8;
  n, i, x: integer;
  r: TLdapResult;
  a: TLdapAttribute;
begin
  fSearchResult.Clear;
  fReferals.Clear;
  s := Asn(BaseDN);
  Append(s, [Asn(ord(fSearchScope), ASN1_ENUM),
             Asn(ord(fSearchAliases), ASN1_ENUM),
             Asn(fSearchSizeLimit),
             Asn(fSearchTimeLimit)]);
  Append(s, [Asn(TypesOnly)]);
  if Filter = '' then
    Filter := '(objectclass=*)';
  t := TranslateFilter(Filter);
  if t = '' then
    t := Asn('', ASN1_NULL);
  Append(s, [t]);
  t := '';
  for n := 0 to high(Attributes) - 1 do
    Append(t, [Asn(Attributes[n])]);
  Append(s, [AsnSeq(t)]);
  s := Asn(s, LDAP_ASN1_SEARCH_REQUEST);
  if fSearchPageSize > 0 then
  begin
    t := Asn(ASN1_SEQ, [
           Asn(fSearchPageSize),
           Asn(fSearchCookie)]);
    c := Asn(ASN1_SEQ, [
           Asn('1.2.840.113556.1.4.319'), // controlType: pagedresultsControl
           Asn(false), // criticality: false
           Asn(t)]);   // add searchControlValue as OCTET STRING
    Append(s, [Asn(c, LDAP_ASN1_CONTROLS)]); // append Controls to SearchRequest
  end;
  SendPacket(s);
  repeat
    t := DecodeResponse(ReceiveResponse);
    if fResponseCode = LDAP_ASN1_SEARCH_ENTRY then
    begin
      r := fSearchResult.Add;
      n := 1;
      r.ObjectName := AsnNext(n, t, x);
      AsnNext(n, t, x);
      if x = ASN1_SEQ then
      begin
        while n < length(t) do
        begin
          s := AsnNext(n, t, x);
          if x = ASN1_SEQ then
          begin
            i := n + length(s);
            u := AsnNext(n, t, x);
            a := r.Attributes.Add(u);
            AsnNext(n, t, x);
            if x = ASN1_SETOF then
              while n < i do
              begin
                u := AsnNext(n, t, x);
                a.Add(u);
              end;
          end;
        end;
      end;
    end;
    if fResponseCode = LDAP_ASN1_SEARCH_REFERENCE then
    begin
      n := 1;
      while n < length(t) do
        fReferals.Add(AsnNext(n, t, x));
    end;
  until fResponseCode = LDAP_ASN1_SEARCH_DONE;
  n := 1;
  AsnNext(n, t, x);
  if x = LDAP_ASN1_CONTROLS then
  begin
    AsnNext(n, t, x);
    if x = ASN1_SEQ then
    begin
      s := AsnNext(n, t, x);
      if s = '1.2.840.113556.1.4.319' then
      begin
        s := AsnNext(n, t, x); // searchControlValue
        n := 1;
        AsnNext(n, s, x);
        if x = ASN1_SEQ then
        begin
          AsnNext(n, s, x); // total number of result records, if known, otherwise 0
          fSearchCookie := AsnNext(n, s, x); // active search cookie, empty when done
        end;
      end;
    end;
  end;
  result := fResultCode = 0;
end;

function TLdapClient.Extended(const Oid, Value: RawUtf8): boolean;
var
  query, decoded: TAsnObject;
  pos, xt: integer;
begin
  query := Asn(Oid, $80);
  if Value <> '' then
    Append(query, [Asn(Value, $81)]);
  query := Asn(query, LDAP_ASN1_EXT_REQUEST);
  SendPacket(query);
  decoded := DecodeResponse(ReceiveResponse);
  result := fResultCode = 0;
  if result then
  begin
    pos := 1;
    fExtName  := AsnNext(pos, decoded, xt);
    fExtValue := AsnNext(pos, decoded, xt);
  end;
end;



end.

