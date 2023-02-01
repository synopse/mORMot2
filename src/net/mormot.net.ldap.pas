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

  /// we defined our own type to hold an ASN object binary
  TAsnObject = RawByteString;

  /// implementation of LDAP client version 2 and 3
  // - Authentication use Username/Password properties
  // - Server/Port use TargetHost/TargetPort properties
  TLdapClient = class(TSynPersistent)
  private
    fTargetHost: RawUtf8;
    fTargetPort: RawUtf8;
    fTimeout: integer;
    fUserName: RawUtf8;
    fPassword: RawUtf8;
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
    function SendAndReceive(const Asn1Data: TAsnObject): TAsnObject;
    function SaslDigestMd5(const Value: RawUtf8): RawUtf8;
    function TranslateFilter(const Filter: RawUtf8): TAsnObject;
    class function GetErrorString(ErrorCode: integer): RawUtf8;
    function ReceiveString(Size: integer): RawByteString;
  public
    /// initialize this LDAP client instance
    constructor Create; override;
    /// finalize this LDAP client instance
    destructor Destroy; override;
    /// try to connect to LDAP server and start secure channel when it is required
    function Login: boolean;
    /// authenticate a client to the directory server with Username/Password
    // - if this is empty strings, then it does annonymous binding
    // - when you not call Bind on LDAPv3, then anonymous mode is used
    // - warning: uses plaintext transport of password - consider using TLS
    function Bind: boolean;
    /// authenticate a client to the directory server with Username/Password
    // - when you not call Bind on LDAPv3, then anonymous mode is used
    // - uses DIGEST-MD5 as password obfuscation challenge - consider using TLS
    function BindSaslDigestMd5: boolean;
    /// close connection to the LDAP server
    function Logout: boolean;
    /// retrieve all entries that match a given set of criteria
    // - will generate as many requests/responses as needed to retrieve all
    // the information into the SearchResult property
    function Search(const BaseDN: RawUtf8; TypesOnly: boolean;
      Filter: RawUtf8; const Attributes: array of RawByteString): boolean;
    /// create a new entry in the directory
    function Add(const Obj: RawUtf8; Value: TLdapAttributeList): boolean;
    /// make one or more changes to the set of attribute values in an entry
    function Modify(const Obj: RawUtf8; Op: TLdapModifyOp;
      Value: TLdapAttribute): boolean;
    /// change an entry’s DN
    // - it can be used to rename the entry (by changing its RDN), move it to a
    // different location in the DIT (by specifying a new parent entry), or both
    function ModifyDN(const obj, newRdn, newSuperior: RawUtf8;
      DeleteOldRdn: boolean): boolean;
    ///  remove an entry from the directory server
    function Delete(const Obj: RawUtf8): boolean;
    /// determine whether a given entry has a specified attribute value
    function Compare(const Obj, AttributeValue: RawUtf8): boolean;
    /// call any LDAP v3 extended operations
    // - e.g. StartTLS, cancel, transactions
    function Extended(const Oid, Value: RawUtf8): boolean;
    /// the version of LDAP protocol used
    // - default value is 3
    property Version: integer
      read fVersion Write fVersion;
    /// target server IP (or symbolic name)
    // - default is 'localhost'
    property TargetHost: RawUtf8
      read fTargetHost Write fTargetHost;
    /// target server port (or symbolic name)
    // - is '389' by default but should be '636' (or '3269') on TLS
    property TargetPort: RawUtf8
      read fTargetPort Write fTargetPort;
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
    /// contains the result code of the last LDAP operation
    // - could be e.g. LDAP_RES_SUCCESS or an error code - see ResultString
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

const
  LDAP_RES_SUCCESS  = 0;
  LDAP_RES_REFERRAL = 10;
  LDAP_RES_BINDING  = 14;



implementation


{ ****** Support procedures and functions ****** }

procedure UnquoteStr(Value: PUtf8Char; var result: RawUtf8);
begin
  if (Value = nil) or
     (Value^ <> '"') then
    FastSetString(result, Value, StrLen(Value))
  else
    UnQuoteSqlStringVar(Value, result);
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
  // base types
  ASN1_BOOL        = $01;
  ASN1_INT         = $02;
  ASN1_BITSTR      = $03;
  ASN1_OCTSTR      = $04;
  ASN1_NULL        = $05;
  ASN1_OBJID       = $06;
  ASN1_ENUM        = $0a;
  ASN1_UTF8STRING  = $0c;
  ASN1_SEQ         = $30;
  ASN1_SETOF       = $31;
  ASN1_IPADDR      = $40;
  ASN1_COUNTER     = $41;
  ASN1_GAUGE       = $42;
  ASN1_TIMETICKS   = $43;
  ASN1_OPAQUE      = $44;
  ASN1_COUNTER64   = $46;

  // class type masks
  ASN1_CL_APP   = $40;
  ASN1_CL_CTX   = $80;
  ASN1_CL_PRI   = $c0;

  //  context-specific class, tag #n
  ASN1_CTX0  = $80;
  ASN1_CTX1  = $81;
  ASN1_CTX2  = $82;
  ASN1_CTX3  = $83;
  ASN1_CTX4  = $84;
  ASN1_CTX5  = $85;
  ASN1_CTX6  = $86;
  ASN1_CTX7  = $87;
  ASN1_CTX8  = $88;
  ASN1_CTX9  = $89;

  //  context-specific class, constructed, tag #n
  ASN1_CTC0  = $a0;
  ASN1_CTC1  = $a1;
  ASN1_CTC2  = $a2;
  ASN1_CTC3  = $a3;
  ASN1_CTC4  = $a4;
  ASN1_CTC5  = $a5;
  ASN1_CTC6  = $a6;
  ASN1_CTC7  = $a7;
  ASN1_CTC8  = $a8;
  ASN1_CTC9  = $a9;

  ASN1_BOOLEAN: array[boolean] of byte = (
    $00,
    $ff);

  // LDAP types
  LDAP_ASN1_BIND_REQUEST      = $60;
  LDAP_ASN1_BIND_RESPONSE     = $61;
  LDAP_ASN1_UNBIND_REQUEST    = $42;
  LDAP_ASN1_SEARCH_REQUEST    = $63;
  LDAP_ASN1_SEARCH_ENTRY      = $64;
  LDAP_ASN1_SEARCH_DONE       = $65;
  LDAP_ASN1_SEARCH_REFERENCE  = $73;
  LDAP_ASN1_MODIFY_REQUEST    = $66;
  LDAP_ASN1_MODIFY_RESPONSE   = $67;
  LDAP_ASN1_ADD_REQUEST       = $68;
  LDAP_ASN1_ADD_RESPONSE      = $69;
  LDAP_ASN1_DEL_REQUEST       = $4a;
  LDAP_ASN1_DEL_RESPONSE      = $6b;
  LDAP_ASN1_MODIFYDN_REQUEST  = $6c;
  LDAP_ASN1_MODIFYDN_RESPONSE = $6d;
  LDAP_ASN1_COMPARE_REQUEST   = $6e;
  LDAP_ASN1_COMPARE_RESPONSE  = $6f;
  LDAP_ASN1_ABANDON_REQUEST   = $70;
  LDAP_ASN1_EXT_REQUEST       = $77;
  LDAP_ASN1_EXT_RESPONSE      = $78;
  LDAP_ASN1_CONTROLS          = $a0;


function AsnEncOidItem(Value: Int64): TAsnObject;
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

function AsnDecOidItem(var Pos: integer; const Buffer: TAsnObject): integer;
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
  dest^ := byte(n) or $80; // first byte is number of following bytes + $80
  repeat
    inc(dest);
    dec(n);
    dest^ := tmp[n]; // stored as big endian
  until n = 0;
end;

function AsnDecLen(var Start: integer; const Buffer: TAsnObject): cardinal;
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

function AsnEncInt(Value: Int64): TAsnObject;
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
  const Content: array of TAsnObject): TAsnObject; overload;
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
begin
  result := Asn(ASN1_BOOL, [AsnEncInt(ASN1_BOOLEAN[Value])]);
end;

function AsnSeq(const Data: TAsnObject): TAsnObject;
begin
  result := Asn(ASN1_SEQ, [Data]);
end;

procedure AsnAdd(var Data: TAsnObject; const Buffer: TAsnObject);
  overload; {$ifdef HASINLINE} inline; {$endif}
begin
  AppendBufferToRawByteString(Data, Buffer);
end;

procedure AsnAdd(var Data: TAsnObject; const Buffer: TAsnObject;
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
  out ValueType: integer): RawByteString;
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
      // ASN1_UTF8STRING, ASN1_OCTSTR, ASN1_OPAQUE or unknown
      begin
        result := copy(Buffer, Pos, asnsize); // return as raw binary
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

{ TLdapClient }

constructor TLdapClient.Create;
begin
  inherited Create;
  fReferals := TRawUtf8List.Create;
  fTargetHost := cLocalhost;
  fTargetPort := '389';
  fTimeout := 60000;
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
    LDAP_RES_SUCCESS:
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
    LDAP_RES_REFERRAL:
      result := 'Referral';
    11:
      result := 'Admin limit exceeded';
    12:
      result := 'Unavailable critical extension';
    13:
      result := 'Confidentality required';
    LDAP_RES_BINDING:
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
  FreeAndNil(fSock);
  result := false;
  fSeq := 0;
  try
    fSock := TCrtSocket.Open(
      fTargetHost, fTargetPort, nlTcp, fTimeOut, fFullTls, fTlsContext);
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

// see https://ldap.com/ldapv3-wire-protocol-reference-ldap-result

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
  AsnNext(i, Asn1Response, asntype); // initial ANS1_SEQ
  numseq := Utf8ToInteger(AsnNext(i, Asn1Response, asntype), 0);
  if (asntype <> ASN1_INT) or
     (numseq <> fSeq) then
    exit;
  AsnNext(i, Asn1Response, fResponseCode);
  if fResponseCode in [LDAP_ASN1_BIND_RESPONSE, LDAP_ASN1_SEARCH_DONE,
    LDAP_ASN1_MODIFY_RESPONSE, LDAP_ASN1_ADD_RESPONSE, LDAP_ASN1_DEL_RESPONSE,
    LDAP_ASN1_MODIFYDN_RESPONSE, LDAP_ASN1_COMPARE_RESPONSE,
    LDAP_ASN1_EXT_RESPONSE] then
  begin
    fResultCode := Utf8ToInteger(AsnNext(i, Asn1Response, asntype), -1);
    fResponseDN := AsnNext(i, Asn1Response, asntype);   // matchedDN
    fResultString := AsnNext(i, Asn1Response, asntype); // diagnosticMessage
    if fResultString = '' then
      fResultString := GetErrorString(fResultCode);
    if fResultCode = LDAP_RES_REFERRAL then
    begin
      s := AsnNext(i, Asn1Response, asntype);
      if asntype = ASN1_CTC3 then
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

function TLdapClient.SendAndReceive(const Asn1Data: TAsnObject): TAsnObject;
begin
  SendPacket(Asn1Data);
  result := DecodeResponse(ReceiveResponse);
end;

function TLdapClient.SaslDigestMd5(const Value: RawUtf8): RawUtf8;
var
  v, ha0, ha1, ha2, nonce, cnonce, nc, realm, authzid, qop, uri, resp: RawUtf8;
  p, s: PUtf8Char;
  hasher: TMd5;
  dig: TMd5Digest;
begin
  // see https://en.wikipedia.org/wiki/Digest_access_authentication
  p := pointer(Value);
  while p <> nil do
  begin
    v := GetNextItem(p);
    s := pointer(v);
    if IdemPChar(s, 'NONCE=') then
      UnquoteStr(p + 6, nonce)
    else if IdemPChar(s, 'REALM=') then
      UnquoteStr(p + 6, realm)
    else if IdemPChar(s, 'AUTHZID=') then
      UnquoteStr(p + 8, authzid);
  end;
  cnonce := Int64ToHexLower(Random64);
  nc := '00000001';
  qop := 'auth';
  uri := 'ldap/' + LowerCaseU(fSock.Server);
  hasher.Init;
  hasher.Update(fUserName);
  hasher.Update(':');
  hasher.Update(realm);
  hasher.Update(':');
  hasher.Update(fPassword);
  hasher.Final(dig);
  FastSetString(ha0, @dig, SizeOf(dig)); // ha0 = md5 binary, not hexa
  ha1 := FormatUtf8('%:%:%', [ha0, nonce, cnonce]);
  if authzid <> '' then
    Append(ha1, [':', authzid]);
  ha1 := Md5(ha1); // Md5() = into lowercase hexadecimal
  ha2 := Md5(FormatUtf8('AUTHENTICATE:%', [uri]));
  resp := Md5(FormatUtf8('%:%:%:%:%:%', [ha1, nonce, nc, cnonce, qop, ha2]));
  FormatUtf8('username="%",realm="%",nonce="%",cnonce="%",nc=%,qop=%,' +
    'digest-uri="%",response=%',
    [fUserName, realm, nonce, cnonce, nc, qop, uri, resp], result);
end;

// https://ldap.com/ldapv3-wire-protocol-reference-search

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
      result := Asn(TranslateFilter(GetBetween('(', ')', s)), ASN1_CTC2);
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
        result := Asn(result, ASN1_CTC0);
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
        result := Asn(result, ASN1_CTC1);
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
                dn := ASN1_BOOLEAN[false];
                attr := '';
                rule := '';
                if mormot.core.base.PosEx(':dn', l) > 0 then
                begin
                  dn := ASN1_BOOLEAN[true];
                  l := StringReplaceAll(l, ':dn', '');
                end;
                attr := TrimU(SeparateLeft(l, ':'));
                rule := TrimU(SeparateRight(l, ':'));
                if rule = l then
                  rule := '';
                if rule <> '' then
                  result := Asn(rule, ASN1_CTX1);
                if attr <> '' then
                  AsnAdd(result, attr, ASN1_CTX2);
                AsnAdd(result, DecodeTriplet(r, '\'), ASN1_CTX3);
                AsnAdd(result, AsnEncInt(dn), ASN1_CTX4);
                result := Asn(result, ASN1_CTC9);
              end;
            '~':
              // Approx match
              begin
                System.Delete(l, length(l), 1);
                result := Asn(ASN1_CTC8, [
                  Asn(l),
                  Asn(DecodeTriplet(r, '\'))]);
              end;
            '>':
              // Greater or equal match
              begin
                System.Delete(l, length(l), 1);
                result := Asn(ASN1_CTC5, [
                   Asn(l),
                   Asn(DecodeTriplet(r, '\'))]);
              end;
            '<':
              // Less or equal match
              begin
                System.Delete(l, length(l), 1);
                result := Asn(ASN1_CTC6, [
                   Asn(l),
                   Asn(DecodeTriplet(r, '\'))]);
              end;
          else
            // present
            if r = '*' then
              result := Asn(l, ASN1_CTX7)
            else
              if PosExChar('*', r) > 0 then
              // substrings
              begin
                s := Fetch(r, '*');
                if s <> '' then
                  result := Asn(DecodeTriplet(s, '\'), ASN1_CTX0);
                while r <> '' do
                begin
                  if PosExChar('*', r) <= 0 then
                    break;
                  s := Fetch(r, '*');
                  AsnAdd(result, DecodeTriplet(s, '\'), ASN1_CTX1);
                end;
                if r <> '' then
                  AsnAdd(result, DecodeTriplet(r, '\'), ASN1_CTX2);
                result := Asn(ASN1_CTC4, [
                   Asn(l),
                   AsnSeq(result)]);
              end
              else
              begin
                // Equality match
                result := Asn(ASN1_CTC3, [
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

// see https://ldap.com/ldapv3-wire-protocol-reference-bind

function TLdapClient.Bind: boolean;
begin
  SendAndReceive(Asn(LDAP_ASN1_BIND_REQUEST, [
                   Asn(fVersion),
                   Asn(fUserName),
                   Asn(fPassword, ASN1_CTX0)]));
  result := fResultCode = LDAP_RES_SUCCESS;
end;

function TLdapClient.BindSaslDigestMd5: boolean;
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
                Asn(ASN1_CTC3, [
                  Asn('DIGEST-MD5')])]);
    t := SendAndReceive(digreq);
    if fResultCode = LDAP_RES_BINDING then
    begin
      s := t;
      x := 1;
      t := AsnNext(x, s, xt);
      SendAndReceive(Asn(LDAP_ASN1_BIND_REQUEST, [
                       Asn(fVersion),
                       Asn(''),
                       Asn(ASN1_CTC3, [
                         Asn('DIGEST-MD5'),
                         Asn(SaslDigestMd5(t))])]));
      if fResultCode = LDAP_RES_BINDING then
        SendAndReceive(digreq);
      result := fResultCode = LDAP_RES_SUCCESS;
    end;
  end;
end;

// TODO: GSSAPI SASL authentication using mormot.lib.gssapi/sspi units
// - see https://github.com/go-ldap/ldap/blob/master/bind.go#L561


// https://ldap.com/ldapv3-wire-protocol-reference-unbind

function TLdapClient.Logout: boolean;
begin
  SendPacket(Asn('', LDAP_ASN1_UNBIND_REQUEST));
  FreeAndNil(fSock);
  result := true;
end;

// https://ldap.com/ldapv3-wire-protocol-reference-modify

function TLdapClient.Modify(const Obj: RawUtf8; Op: TLdapModifyOp;
  Value: TLdapAttribute): boolean;
var
  query: TAsnObject;
  i: integer;
begin
  for i := 0 to Value.Count -1 do
    AsnAdd(query, Asn(Value.GetRaw(i)));
  SendAndReceive(Asn(LDAP_ASN1_MODIFY_REQUEST, [
                   Asn(obj),
                   Asn(ASN1_SEQ, [
                     Asn(ASN1_SEQ, [
                       Asn(ord(Op), ASN1_ENUM),
                       Asn(ASN1_SEQ, [
                         Asn(Value.AttributeName),
                         Asn(query, ASN1_SETOF)])])])]));
  result := fResultCode = LDAP_RES_SUCCESS;
end;

// https://ldap.com/ldapv3-wire-protocol-reference-add

function TLdapClient.Add(const Obj: RawUtf8; Value: TLdapAttributeList): boolean;
var
  query, sub: TAsnObject;
  attr: TLdapAttribute;
  i, j: PtrInt;
begin
  for i := 0 to Value.Count - 1 do
  begin
    attr := Value.Items[i];
    sub := '';
    for j := 0 to attr.Count - 1 do
      AsnAdd(sub, Asn(attr.GetRaw(j)));
    Append(query, [
      Asn(ASN1_SEQ, [
        Asn(attr.AttributeName),
        Asn(ASN1_SETOF, [sub])])]);
  end;
  SendAndReceive(Asn(LDAP_ASN1_ADD_REQUEST, [
                   Asn(obj),
                   AsnSeq(query)]));
  result := fResultCode = LDAP_RES_SUCCESS;
end;

// https://ldap.com/ldapv3-wire-protocol-reference-delete

function TLdapClient.Delete(const Obj: RawUtf8): boolean;
begin
  SendAndReceive(Asn(obj, LDAP_ASN1_DEL_REQUEST));
  result := fResultCode = LDAP_RES_SUCCESS;
end;

// https://ldap.com/ldapv3-wire-protocol-reference-modify-dn

function TLdapClient.ModifyDN(const obj, newRdn, newSuperior: RawUtf8;
  DeleteOldRdn: boolean): boolean;
var
  query: TAsnObject;
begin
  query := Asn(obj);
  Append(query, [Asn(newRdn), Asn(DeleteOldRdn)]);
  if newSuperior <> '' then
    AsnAdd(query, Asn(newSuperior, ASN1_CTX0));
  SendAndReceive(Asn(query, LDAP_ASN1_MODIFYDN_REQUEST));
  result := fResultCode = LDAP_RES_SUCCESS;
end;

// https://ldap.com/ldapv3-wire-protocol-reference-compare

function TLdapClient.Compare(const Obj, AttributeValue: RawUtf8): boolean;
begin
  SendAndReceive(Asn(LDAP_ASN1_COMPARE_REQUEST, [
                   Asn(obj),
                   Asn(ASN1_SEQ, [
                     Asn(TrimU(SeparateLeft(AttributeValue, '='))),
                     Asn(TrimU(SeparateRight(AttributeValue, '=')))])]));
  result := fResultCode = LDAP_RES_SUCCESS;
end;

// https://ldap.com/ldapv3-wire-protocol-reference-search

function TLdapClient.Search(const BaseDN: RawUtf8; TypesOnly: boolean;
  Filter: RawUtf8; const Attributes: array of RawByteString): boolean;
var
  s, filt, attr, resp: TAsnObject;
  u: RawUtf8;
  n, i, x: integer;
  r: TLdapResult;
  a: TLdapAttribute;
begin
  // see https://ldap.com/ldapv3-wire-protocol-reference-search
  fSearchResult.Clear;
  fReferals.Clear;
  if Filter = '' then
    Filter := '(objectclass=*)';
  filt := TranslateFilter(Filter);
  if filt = '' then
    filt := Asn('', ASN1_NULL);
  for n := 0 to high(Attributes) - 1 do
    AsnAdd(attr, Asn(Attributes[n]));
  s := Asn(LDAP_ASN1_SEARCH_REQUEST, [
           Asn(BaseDN),
           Asn(ord(fSearchScope),   ASN1_ENUM),
           Asn(ord(fSearchAliases), ASN1_ENUM),
           Asn(fSearchSizeLimit),
           Asn(fSearchTimeLimit),
           Asn(TypesOnly),
           filt,
           AsnSeq(attr)]);
  if fSearchPageSize > 0 then
    Append(s, [Asn(
        Asn(ASN1_SEQ, [
           Asn('1.2.840.113556.1.4.319'), // controlType: pagedresultsControl
           Asn(false), // criticality: false
           Asn(Asn(ASN1_SEQ, [
             Asn(fSearchPageSize),
             Asn(fSearchCookie)]))]), LDAP_ASN1_CONTROLS)]);
  SendPacket(s);
  repeat
    resp := DecodeResponse(ReceiveResponse);
    if fResponseCode = LDAP_ASN1_SEARCH_ENTRY then
    begin
      r := fSearchResult.Add;
      n := 1;
      r.ObjectName := AsnNext(n, resp, x);
      AsnNext(n, resp, x);
      if x = ASN1_SEQ then
      begin
        while n < length(resp) do
        begin
          s := AsnNext(n, resp, x);
          if x = ASN1_SEQ then
          begin
            i := n + length(s);
            u := AsnNext(n, resp, x);
            a := r.Attributes.Add(u);
            AsnNext(n, resp, x);
            if x = ASN1_SETOF then
              while n < i do
              begin
                u := AsnNext(n, resp, x);
                a.Add(u);
              end;
          end;
        end;
      end;
    end;
    if fResponseCode = LDAP_ASN1_SEARCH_REFERENCE then
    begin
      n := 1;
      while n < length(resp) do
        fReferals.Add(AsnNext(n, resp, x));
    end;
  until fResponseCode = LDAP_ASN1_SEARCH_DONE;
  n := 1;
  AsnNext(n, resp, x);
  if x = LDAP_ASN1_CONTROLS then
  begin
    AsnNext(n, resp, x);
    if x = ASN1_SEQ then
    begin
      s := AsnNext(n, resp, x);
      if s = '1.2.840.113556.1.4.319' then
      begin
        s := AsnNext(n, resp, x); // searchControlValue
        n := 1;
        AsnNext(n, s, x);
        if x = ASN1_SEQ then
        begin
          // total number of result records, if known, otherwise 0
          AsnNext(n, s, x);
          // active search cookie, empty when done
          fSearchCookie := AsnNext(n, s, x);
        end;
      end;
    end;
  end;
  result := fResultCode = LDAP_RES_SUCCESS;
end;

// https://ldap.com/ldapv3-wire-protocol-reference-extended

function TLdapClient.Extended(const Oid, Value: RawUtf8): boolean;
var
  query, decoded: TAsnObject;
  pos, xt: integer;
begin
  query := Asn(Oid, ASN1_CTX0);
  if Value <> '' then
    AsnAdd(query, Asn(Value, ASN1_CTX1));
  decoded := SendAndReceive(Asn(query, LDAP_ASN1_EXT_REQUEST));
  result := fResultCode = LDAP_RES_SUCCESS;
  if result then
  begin
    pos := 1;
    fExtName  := AsnNext(pos, decoded, xt);
    fExtValue := AsnNext(pos, decoded, xt);
  end;
end;



end.

