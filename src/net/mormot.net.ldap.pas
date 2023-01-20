{==============================================================================|
| Project : Ararat Synapse                                       | 001.007.001 |
|==============================================================================|
| Content: LDAP client                                                         |
|          support for ASN.1 BER coding and decoding                           |
|==============================================================================|
| Copyright (c)1999-2014, Lukas Gebauer                                        |
| All rights reserved.                                                         |
|                                                                              |
| Redistribution and use in source and binary forms, with or without           |
| modification, are permitted provided that the following conditions are met:  |
|                                                                              |
| Redistributions of source code must retain the above copyright notice, this  |
| list of conditions and the following disclaimer.                             |
|                                                                              |
| Redistributions in binary form must reproduce the above copyright notice,    |
| this list of conditions and the following disclaimer in the documentation    |
| and/or other materials provided with the distribution.                       |
|                                                                              |
| Neither the name of Lukas Gebauer nor the names of its contributors may      |
| be used to endorse or promote products derived from this software without    |
| specific prior written permission.                                           |
|                                                                              |
| THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  |
| AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    |
| IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   |
| ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR  |
| ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL       |
| DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR   |
| SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER   |
| CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT           |
| LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY    |
| OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  |
| DAMAGE.                                                                      |
|==============================================================================|
| The Initial Developer of the Original Code is Lukas Gebauer (Czech Republic).|
| Portions created by Lukas Gebauer are Copyright (c)2003-2014.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{:@abstract(LDAP client)

Used RFC: RFC-2251, RFC-2254, RFC-2696, RFC-2829, RFC-2830
}

// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md

unit mormot.net.ldap;

interface

{$I ..\mormot.defines.inc}

uses
  SysUtils,
  Classes,
  mormot.net.sock,
  mormot.core.base;

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


type

  {:@abstract(LDAP attribute with list of their values)
   This class holding name of LDAP attribute and list of their values. This is
   descendant of TStringList class enhanced by some new properties.}
  TLDAPAttribute = class(TStringList)
  private
    FAttributeName: RawUtf8;
    FIsBinary: Boolean;
  protected
    function Get(Index: integer): string; override;
    procedure Put(Index: integer; const Value: string); override;
    procedure SetAttributeName(Value: RawUtf8);
  public
    function Add(const S: string): Integer; override;
  published
    {:Name of LDAP attribute.}
    property AttributeName: RawUtf8 read FAttributeName Write SetAttributeName;
    {:Return @true when attribute contains binary data.}
    property IsBinary: Boolean read FIsBinary;
  end;

  {:@abstract(List of @link(TLDAPAttribute))
   This object can hold list of TLDAPAttribute objects.}
  TLDAPAttributeList = class(TObject)
  private
    FAttributeList: TList;
    function GetAttribute(Index: integer): TLDAPAttribute;
  public
    constructor Create;
    destructor Destroy; override;
    {:Clear list.}
    procedure Clear;
    {:Return count of TLDAPAttribute objects in list.}
    function Count: integer;
    {:Add new TLDAPAttribute object to list.}
    function Add: TLDAPAttribute;
    {:Delete one TLDAPAttribute object from list.}
    procedure Del(Index: integer);
    {:Find and return attribute with requested name. Returns nil if not found.}
    function Find(AttributeName: RawUtf8): TLDAPAttribute;
    {:Find and return attribute value with requested name. Returns empty string if not found.}
    function Get(AttributeName: RawUtf8): string;
    {:List of TLDAPAttribute objects.}
    property Items[Index: Integer]: TLDAPAttribute read GetAttribute; default;
  end;

  {:@abstract(LDAP result object)
   This object can hold LDAP object. (their name and all their attributes with
   values)}
  TLDAPresult = class(TObject)
  private
    FObjectName: RawUtf8;
    FAttributes: TLDAPAttributeList;
  public
    constructor Create;
    destructor Destroy; override;
  published
    {:Name of this LDAP object.}
    property ObjectName: RawUtf8 read FObjectName write FObjectName;
    {:Here is list of object attributes.}
    property Attributes: TLDAPAttributeList read FAttributes;
  end;

  {:@abstract(List of LDAP result objects)
   This object can hold list of LDAP objects. (for example result of LDAP SEARCH.)}
  TLDAPresultList = class(TObject)
  private
    FresultList: TList;
    function Getresult(Index: integer): TLDAPresult;
  public
    constructor Create;
    destructor Destroy; override;
    {:Clear all TLDAPresult objects in list.}
    procedure Clear;
    {:Return count of TLDAPresult objects in list.}
    function Count: integer;
    {:Create and add new TLDAPresult object to list.}
    function Add: TLDAPresult;
    {:List of TLDAPresult objects.}
    property Items[Index: Integer]: TLDAPresult read Getresult; default;
  end;

  {:Define possible operations for LDAP MODIFY operations.}
  TLDAPModifyOp = (
    MO_Add,
    MO_Delete,
    MO_Replace
  );

  {:Specify possible values for search scope.}
  TLDAPSearchScope = (
    SS_BaseObject,
    SS_SingleLevel,
    SS_WholeSubtree
  );

  {:Specify possible values about alias dereferencing.}
  TLDAPSearchAliases = (
    SA_NeverDeref,
    SA_InSearching,
    SA_FindingBaseObj,
    SA_Always
  );

  {:@abstract(Parent class of application protocol implementations.)
   By this class is defined common properties.}
  TSynaClient = Class(TObject)
  protected
    FTargetHost: RawUtf8;
    FTargetPort: RawUtf8;
    FIPInterface: RawUtf8;
    FTimeout: integer;
    FUserName: RawUtf8;
    FPassword: RawUtf8;
  public
    constructor Create;
  published
    {:Specify terget server IP (or symbolic name). Default is 'localhost'.}
    property TargetHost: RawUtf8 read FTargetHost Write FTargetHost;

    {:Specify terget server port (or symbolic name).}
    property TargetPort: RawUtf8 read FTargetPort Write FTargetPort;

    {:Defined local socket address. (outgoing IP address). By default is used
     '0.0.0.0' as wildcard for default IP.}
    property IPInterface: RawUtf8 read FIPInterface Write FIPInterface;

    {:Specify default timeout for socket operations.}
    property Timeout: integer read FTimeout Write FTimeout;

    {:If protocol need user authorization, then fill here username.}
    property UserName: RawUtf8 read FUserName Write FUserName;

    {:If protocol need user authorization, then fill here password.}
    property Password: RawUtf8 read FPassword Write FPassword;
  end;

  {:@abstract(Implementation of LDAP client)
   (version 2 and 3)

   Note: Are you missing properties for setting Username and Password? Look to
   parent @link(TSynaClient) object!

   Are you missing properties for specify server address and port? Look to
   parent @link(TSynaClient) too!}

  { TLDAPSend }

  TLDAPSend = class(TSynaClient)
  private
    FSock: TCrtSocket;
    FresultCode: Integer;
    FresultString: RawUtf8;
    FFullresult: RawByteString;
    FFullSSL: Boolean;
    FSeq: integer;
    FResponseCode: integer;
    FResponseDN: RawUtf8;
    FReferals: TStringList;
    FVersion: integer;
    FSearchScope: TLDAPSearchScope;
    FSearchAliases: TLDAPSearchAliases;
    FSearchSizeLimit: integer;
    FSearchTimeLimit: integer;
    FSearchPageSize: integer;
    FSearchCookie: RawUtf8;
    FSearchresult: TLDAPresultList;
    FExtName: RawUtf8;
    FExtValue: RawUtf8;
    function Connect: Boolean;
    function BuildPacket(const Asn1Data: RawByteString): RawByteString;
    function ReceiveResponse: RawByteString;
    function DecodeResponse(const Asn1Response: RawByteString): RawByteString;
    function LdapSasl(Value: RawUtf8): RawByteString;
    function TranslateFilter(Filter: RawUtf8): RawByteString;
    function GetErrorString(ErrorCode: integer): RawUtf8;
    function ReceiveString(Length: integer): RawByteString;
  public
    constructor Create;
    destructor Destroy; override;

    {:Try to connect to LDAP server and start secure channel, when it is required.}
    function Login: Boolean;

    {:Try to bind to LDAP server with @link(TSynaClient.Username) and
     @link(TSynaClient.Password). If this is empty strings, then it do annonymous
     Bind. When you not call Bind on LDAPv3, then is automaticly used anonymous
     mode.

     This method using plaintext transport of password! It is not secure!}
    function Bind: Boolean;

    {:Try to bind to LDAP server with @link(TSynaClient.Username) and
     @link(TSynaClient.Password). If this is empty strings, then it do annonymous
     Bind. When you not call Bind on LDAPv3, then is automaticly used anonymous
     mode.

     This method using SASL with DIGEST-MD5 method for secure transfer of your
     password.}
    function BindSasl: Boolean;

    {:Close connection to LDAP server.}
    function Logout: Boolean;

    {:Modify content of LDAP attribute on this object.}
    function Modify(obj: RawUtf8; Op: TLDAPModifyOp; const Value: TLDAPAttribute): Boolean;

    {:Add list of attributes to specified object.}
    function Add(obj: RawUtf8; const Value: TLDAPAttributeList): Boolean;

    {:Delete this LDAP object from server.}
    function Delete(obj: RawUtf8): Boolean;

    {:Modify object name of this LDAP object.}
    function ModifyDN(obj, newRDN, newSuperior: RawUtf8; DeleteoldRDN: Boolean): Boolean;

    {:Try to compare Attribute value with this LDAP object.}
    function Compare(obj, AttributeValue: RawUtf8): Boolean;

    {:Search LDAP base for LDAP objects by Filter.}
    function Search(BaseDN: RawUtf8; TypesOnly: Boolean; Filter: RawUtf8;
      const Attributes: TStrings): Boolean;

    {:Call any LDAPv3 extended command.}
    function Extended(const OID, Value: RawUtf8): Boolean;
  published
    {:Specify version of used LDAP protocol. Default value is 3.}
    property Version: integer read FVersion Write FVersion;

    {:result code of last LDAP operation.}
    property resultCode: Integer read FresultCode;

    {:Human readable description of result code of last LDAP operation.}
    property resultString: RawUtf8 read FresultString;

    {:Binary string with full last response of LDAP server. This string is
     encoded by ASN.1 BER encoding! You need this only for debugging.}
    property Fullresult: RawByteString read FFullresult;

    {:If @true, then use connection to LDAP server through SSL/TLS tunnel.}
    property FullSSL: Boolean read FFullSSL Write FFullSSL;

    {:Sequence number of last LDAp command. It is incremented by any LDAP command.}
    property Seq: integer read FSeq;

    {:Specify what search scope is used in search command.}
    property SearchScope: TLDAPSearchScope read FSearchScope Write FSearchScope;

    {:Specify how to handle aliases in search command.}
    property SearchAliases: TLDAPSearchAliases read FSearchAliases Write FSearchAliases;

    {:Specify result size limit in search command. Value 0 means without limit.}
    property SearchSizeLimit: integer read FSearchSizeLimit Write FSearchSizeLimit;

    {:Specify search time limit in search command (seconds). Value 0 means
     without limit.}
    property SearchTimeLimit: integer read FSearchTimeLimit Write FSearchTimeLimit;

    {:Specify number of results to return per search request. Value 0 means
     no paging.}
    property SearchPageSize: integer read FSearchPageSize Write FSearchPageSize;

    {:Cookie returned by paged search results. Use an empty string for the first
     search request.}
    property SearchCookie: RawUtf8 read FSearchCookie Write FSearchCookie;

    {:Here is result of search command.}
    property Searchresult: TLDAPresultList read FSearchresult;

    {:On each LDAP operation can LDAP server return some referals URLs. Here is
     their list.}
    property Referals: TStringList read FReferals;

    {:When you call @link(Extended) operation, then here is result Name returned
     by server.}
    property ExtName: RawUtf8 read FExtName;

    {:When you call @link(Extended) operation, then here is result Value returned
     by server.}
    property ExtValue: RawUtf8 read FExtValue;

    {:TCP socket used by all LDAP operations.}
    property Sock: TCrtSocket read FSock;
  end;

{:Dump result of LDAP SEARCH into human readable form. Good for debugging.}
function LDAPresultDump(const Value: TLDAPresultList): RawUtf8;

implementation

uses
  StrUtils,
  mormot.core.buffers,
  mormot.core.text,
  mormot.crypt.core,
  mormot.core.os;

const
  cLocalhost = '127.0.0.1';
  cAnyHost = '0.0.0.0';
  cAnyPort = '0';
  cLDAPProtocol = '389';

{ ****** From synautil ****** }

{:@abstract(Support procedures and functions)}

function UnquoteStr(const Value: RawUtf8; Quote: AnsiChar): RawUtf8;
var
  n: integer;
  inq, dq: Boolean;
  c, cn: AnsiChar;
begin
  result := '';
  if Value = '' then
    Exit;
  if Value = RawUtf8OfChar(Quote, 2) then
    Exit;
  inq := False;
  dq := False;
  for n := 1 to Length(Value) do
  begin
    c := Value[n];
    if n <> Length(Value) then
      cn := Value[n + 1]
    else
      cn := #0;
    if c = quote then
      if dq then
        dq := False
      else
        if not inq then
          inq := True
        else
          if cn = quote then
          begin
            AppendCharToRawUtf8(result, Quote);
            dq := True;
          end
          else
            inq := False
    else
      AppendCharToRawUtf8(result, c);
  end;
end;

function IsBinaryString(const Value: RawByteString): Boolean;
var
  n: integer;
begin
  result := False;
  for n := 1 to Length(Value) do
    if Ord(Value[n]) in [0..8, 10..31] then
      //ignore null-terminated strings
      if not ((n = Length(value)) and (Ord(Value[n]) = 0)) then
      begin
        result := True;
        Break;
      end;
end;

function DumpExStr(const Buffer: RawByteString): RawUtf8;
var
  n: Integer;
  x: Byte;
begin
  result := '';
  for n := 1 to Length(Buffer) do
  begin
    x := Ord(Buffer[n]);
    if x in [65..90, 97..122] then
      Append(result, [' +''', AnsiChar(x), ''''])
    else
      Append(result, [' +#$' + IntToHex(Ord(Buffer[n]), 2)]);
  end;
end;

function IndexByBegin(Value: RawUtf8; const List: TStrings): integer;
var
  n: integer;
  s: RawUtf8;
begin
  result := -1;
  Value := uppercase(Value);
  for n := 0 to List.Count -1 do
  begin
    s := UpperCase(List[n]);
    if Pos(Value, s) = 1 then
    begin
      result := n;
      Break;
    end;
  end;
end;

function SeparateLeft(const Value, Delimiter: RawUtf8): RawUtf8;
var
  x: Integer;
begin
  x := Pos(Delimiter, Value);
  if x < 1 then
    result := Value
  else
    result := Copy(Value, 1, x - 1);
end;

function SeparateRight(const Value, Delimiter: RawUtf8): RawUtf8;
var
  x: Integer;
begin
  x := Pos(Delimiter, Value);
  if x > 0 then
    x := x + Length(Delimiter) - 1;
  result := Copy(Value, x + 1, Length(Value) - x);
end;

function GetBetween(const PairBegin, PairEnd, Value: RawUtf8): RawUtf8;
var
  n: integer;
  x: integer;
  s: RawUtf8;
  lenBegin: integer;
  lenEnd: integer;
  str: RawUtf8;
  max: integer;
begin
  lenBegin := Length(PairBegin);
  lenEnd := Length(PairEnd);
  n := Length(Value);
  if (Value = PairBegin + PairEnd) then
  begin
    result := '';//nothing between
    exit;
  end;
  if (n < lenBegin + lenEnd) then
  begin
    result := Value;
    exit;
  end;
  s := SeparateRight(Value, PairBegin);
  if (s = Value) then
  begin
    result := Value;
    exit;
  end;
  n := Pos(PairEnd, s);
  if (n = 0) then
  begin
    result := Value;
    exit;
  end;
  result := '';
  x := 1;
  max := Length(s) - lenEnd + 1;
  for n := 1 to max do
  begin
    str := copy(s, n, lenEnd);
    if (str = PairEnd) then
    begin
      Dec(x);
      if (x <= 0) then
        Break;
    end;
    str := copy(s, n, lenBegin);
    if (str = PairBegin) then
      Inc(x);
    AppendCharToRawUtf8(result, s[n]);
  end;
end;

function DecodeTriplet(const Value: RawUtf8; Delimiter: AnsiChar): RawUtf8;
var
  x, l, lv: Integer;
  c: AnsiChar;
  b: Byte;
  bad: Boolean;
begin
  lv := Length(Value);
  SetLength(result, lv);
  x := 1;
  l := 1;
  while x <= lv do
  begin
    c := Value[x];
    Inc(x);
    if c <> Delimiter then
    begin
      result[l] := c;
      Inc(l);
    end
    else
      if x < lv then
      begin
        Case Value[x] Of
          #13:
            if (Value[x + 1] = #10) then
              Inc(x, 2)
            else
              Inc(x);
          #10:
            if (Value[x + 1] = #13) then
              Inc(x, 2)
            else
              Inc(x);
        else
          begin
            bad := False;
            Case Value[x] Of
              '0'..'9': b := (Byte(Value[x]) - 48) Shl 4;
              'a'..'f', 'A'..'F': b := ((Byte(Value[x]) And 7) + 9) shl 4;
            else
              begin
                b := 0;
                bad := True;
              end;
            end;
            Case Value[x + 1] Of
              '0'..'9': b := b Or (Byte(Value[x + 1]) - 48);
              'a'..'f', 'A'..'F': b := b Or ((Byte(Value[x + 1]) And 7) + 9);
            else
              bad := True;
            end;
            if bad then
            begin
              result[l] := c;
              Inc(l);
            end
            else
            begin
              Inc(x, 2);
              result[l] := AnsiChar(b);
              Inc(l);
            end;
          end;
        end;
      end
      else
        break;
  end;
  Dec(l);
  SetLength(result, l);
end;

function TrimSPLeft(const S: RawUtf8): RawUtf8;
var
  I, L: Integer;
begin
  result := '';
  if S = '' then
    Exit;
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] = ' ') do
    Inc(I);
  result := Copy(S, I, Maxint);
end;

function TrimSPRight(const S: RawUtf8): RawUtf8;
var
  I: Integer;
begin
  result := '';
  if S = '' then
    Exit;
  I := Length(S);
  while (I > 0) and (S[I] = ' ') do
    Dec(I);
  result := Copy(S, 1, I);
end;

function TrimSP(const S: RawUtf8): RawUtf8;
begin
  result := TrimSPLeft(s);
  result := TrimSPRight(result);
end;

function FetchBin(var Value: RawUtf8; const Delimiter: RawUtf8): RawUtf8;
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

function Fetch(var Value: RawUtf8; const Delimiter: RawUtf8): RawUtf8;
begin
  result := FetchBin(Value, Delimiter);
  result := TrimSP(result);
  Value := TrimSP(Value);
end;

{ ****** From asn1util ****** }

{: @abstract(Utilities for handling ASN.1 BER encoding)
By this unit you can parse ASN.1 BER encoded data to elements or build back any
 elements to ASN.1 BER encoded buffer. You can dump ASN.1 BER encoded data to
 human readable form for easy debugging, too.

Supported element types are: ASN1_BOOL, ASN1_INT, ASN1_OCTSTR, ASN1_NULL,
 ASN1_OBJID, ASN1_ENUM, ASN1_SEQ, ASN1_SETOF, ASN1_IPADDR, ASN1_COUNTER,
 ASN1_GAUGE, ASN1_TIMETICKS, ASN1_OPAQUE

For sample of using, look to @link(TSnmpSend) or @link(TLdapSend)class.
}

function ASNEncOIDItem(Value: Int64): RawByteString;
var
  x: Int64;
  xm: Byte;
  b: Boolean;
begin
  x := Value;
  b := False;
  result := '';
  repeat
    xm := x mod 128;
    x := x div 128;
    if b then
      xm := xm or $80;
    if x > 0 then
      b := True;
    Prepend(result, [AnsiChar(xm)]);
  until x = 0;
end;

function ASNDecOIDItem(var Start: Integer; const Buffer: RawByteString): Int64;
var
  x: Integer;
  b: Boolean;
begin
  result := 0;
  repeat
    result := result shl 7;
    x := Ord(Buffer[Start]);
    Inc(Start);
    b := x > $7F;
    x := x and $7F;
    result := result + x;
  until not b;
end;

function ASNEncLen(Len: Integer): RawByteString;
var
  x, y: Integer;
begin
  if Len < $80 then
    result := AnsiChar(Len)
  else
  begin
    x := Len;
    result := '';
    repeat
      y := x mod 256;
      x := x div 256;
      Prepend(result, [AnsiChar(y)]);
    until x = 0;
    y := Length(result);
    y := y or $80;
    Prepend(result, [AnsiChar(y)]);
  end;
end;

function ASNDecLen(var Start: Integer; const Buffer: RawByteString): Integer;
var
  x, n: Integer;
begin
  x := Ord(Buffer[Start]);
  Inc(Start);
  if x < $80 then
    result := x
  else
  begin
    result := 0;
    x := x and $7F;
    for n := 1 to x do
    begin
      result := result * 256;
      x := Ord(Buffer[Start]);
      Inc(Start);
      result := result + x;
    end;
  end;
end;

function ASNEncInt(Value: Int64): RawByteString;
const
  // Defining theses const to avoid implicit conversions from FPC
  Char0: RawByteString = #0;
  Char255: RawByteString = #255;
var
  x: Int64;
  y: byte;
  neg: Boolean;
begin
  neg := Value < 0;
  x := Abs(Value);
  if neg then
    x := x - 1;
  result := '';
  repeat
    y := x mod 256;
    x := x div 256;
    if neg then
      y := not y;
    Prepend(result, [AnsiChar(y)]);
  until x = 0;
  if (not neg) and (result[1] > #$7F) then
    Prepend(result, [Char0]);
  if (neg) and (result[1] < #$80) then
    Prepend(result, [Char255]);
end;

function ASNEncUInt(Value: Integer): RawByteString;
var
  x, y: Integer;
  neg: Boolean;
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
    result[1] := AnsiChar(Ord(result[1]) or $80);
end;

function ASNObject(const Data: RawByteString; ASNType: Integer): RawByteString;
begin
  result := AnsiChar(ASNType);
  Append(result, [ASNEncLen(Length(Data)), Data]);
end;

function MibToId(Mib: RawUtf8): RawByteString;
var
  x: Integer;

  function WalkInt(var s: RawUtf8): Integer;
  var
    x: Integer;
    t: RawByteString;
  begin
    x := Pos('.', s);
    if x < 1 then
    begin
      t := s;
      s := '';
    end
    else
    begin
      t := Copy(s, 1, x - 1);
      s := Copy(s, x + 1, Length(s) - x);
    end;
    result := StrToIntDef(t, 0);
  end;

begin
  result := '';
  x := WalkInt(Mib);
  x := x * 40 + WalkInt(Mib);
  result := ASNEncOIDItem(x);
  while Mib <> '' do
  begin
    x := WalkInt(Mib);
    Append(result, [ASNEncOIDItem(x)]);
  end;
end;

function IdToMib(const Id: RawByteString): RawUtf8;
var
  x, y, n: Integer;
begin
  result := '';
  n := 1;
  while Length(Id) + 1 > n do
  begin
    x := ASNDecOIDItem(n, Id);
    if (n - 1) = 1 then
    begin
      y := x div 40;
      x := x mod 40;
      result := IntToStr(y);
    end;
    Append(result, ['.', IntToStr(x)]);
  end;
end;

function IntMibToStr(const Value: RawByteString): RawByteString;
var
  n, y: Integer;
begin
  y := 0;
  for n := 1 to Length(Value) - 1 do
    y := y * 256 + Ord(Value[n]);
  result := IntToStr(y);
end;

function ASNItem(var Start: Integer; const Buffer: RawByteString;
  var ValueType: Integer): RawByteString;
var
  ASNType: Integer;
  ASNSize: Integer;
  y: int64;
  n: Integer;
  x: byte;
  s: RawByteString;
  c: AnsiChar;
  neg: Boolean;
  l: Integer;
begin
  result := '';
  s := '';
  ValueType := ASN1_NULL;
  l := Length(Buffer);
  if l < (Start + 1) then
    Exit;
  ASNType := Ord(Buffer[Start]);
  ValueType := ASNType;
  Inc(Start);
  ASNSize := ASNDecLen(Start, Buffer);
  if (Start + ASNSize - 1) > l then
    Exit;
  if (ASNType and $20) > 0 then
//    result := '$' + IntToHex(ASNType, 2)
    result := Copy(Buffer, Start, ASNSize)
  else
    case ASNType of
      ASN1_INT, ASN1_ENUM, ASN1_BOOL:
        begin
          y := 0;
          neg := False;
          for n := 1 to ASNSize do
          begin
            x := Ord(Buffer[Start]);
            if (n = 1) and (x > $7F) then
              neg := True;
            if neg then
              x := not x;
            y := y * 256 + x;
            Inc(Start);
          end;
          if neg then
            y := -(y + 1);
          result := IntToStr(y);
        end;
      ASN1_COUNTER, ASN1_GAUGE, ASN1_TIMETICKS, ASN1_COUNTER64:
        begin
          y := 0;
          for n := 1 to ASNSize do
          begin
            y := y * 256 + Ord(Buffer[Start]);
            Inc(Start);
          end;
          result := IntToStr(y);
        end;
      ASN1_OCTSTR, ASN1_OPAQUE:
        begin
          for n := 1 to ASNSize do
          begin
            c := AnsiChar(Buffer[Start]);
            Inc(Start);
            s := s + c;
          end;
          result := s;
        end;
      ASN1_OBJID:
        begin
          for n := 1 to ASNSize do
          begin
            c := AnsiChar(Buffer[Start]);
            Inc(Start);
            s := s + c;
          end;
          result := IdToMib(s);
        end;
      ASN1_IPADDR:
        begin
          s := '';
          for n := 1 to ASNSize do
          begin
            if (n <> 1) then
              s := s + '.';
            y := Ord(Buffer[Start]);
            Inc(Start);
            Append(s, [IntToStr(y)]);
          end;
          result := s;
        end;
      ASN1_NULL:
        begin
          result := '';
          Start := Start + ASNSize;
        end;
    else // unknown
      begin
        for n := 1 to ASNSize do
        begin
          c := AnsiChar(Buffer[Start]);
          Inc(Start);
          Append(s, [c]);
        end;
        result := s;
      end;
    end;
end;

function ASNdump(const Value: RawByteString): RawUtf8;
var
  i, at, x, n: integer;
  s, indent: RawUtf8;
  il: TStringList;
begin
  il := TStringList.Create;
  try
    result := '';
    i := 1;
    indent := '';
    while i < Length(Value) do
    begin
      for n := il.Count - 1 downto 0 do
      begin
        x := StrToIntDef(il[n], 0);
        if x <= i then
        begin
          il.Delete(n);
          Delete(indent, 1, 2);
        end;
      end;
      s := ASNItem(i, Value, at);
      Append(result, [indent, '$', IntToHex(at, 2)]);
      if (at and $20) > 0 then
      begin
        x := Length(s);
        Append(result, [' constructed: length ', IntToStr(x)]);
        Append(indent, ['  ']);
        il.Add(IntToStr(x + i - 1));
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
  finally
    il.Free;
  end;
end;

{==============================================================================}

constructor TSynaClient.Create;
begin
  inherited Create;
  FIPInterface := cAnyHost;
  FTargetHost := cLocalhost;
  FTargetPort := cAnyPort;
  FTimeout := 5000;
  FUsername := '';
  FPassword := '';
end;

function TLDAPAttribute.Add(const S: string): Integer;
begin
  result := inherited Add('');
  Put(result,S);
end;

function TLDAPAttribute.Get(Index: integer): string;
begin
  result := inherited Get(Index);
  if FIsbinary then
    result := Base64ToBin(result)
  else if IsBinaryString(result) then
    result := BinToHex(result);
end;

procedure TLDAPAttribute.Put(Index: integer; const Value: string);
var
  s: RawUtf8;
begin
  s := Value;
  if FIsbinary then
    s := BinToBase64(Value)
  else
    s := UnquoteStr(s, '"');
  inherited Put(Index, s);
end;

procedure TLDAPAttribute.SetAttributeName(Value: RawUtf8);
begin
  FAttributeName := Value;
  FIsBinary := Pos(';binary', Lowercase(value)) > 0;
end;

{==============================================================================}
constructor TLDAPAttributeList.Create;
begin
  inherited Create;
  FAttributeList := TList.Create;
end;

destructor TLDAPAttributeList.Destroy;
begin
  Clear;
  FAttributeList.Free;
  inherited Destroy;
end;

procedure TLDAPAttributeList.Clear;
var
  i: integer;
  attr: TLDAPAttribute;
begin
  for i := Count - 1 downto 0 do
  begin
    attr := GetAttribute(i);
    if Assigned(attr) then
      attr.Free;
  end;
  FAttributeList.Clear;
end;

function TLDAPAttributeList.Count: integer;
begin
  result := FAttributeList.Count;
end;

function TLDAPAttributeList.Get(AttributeName: RawUtf8): string;
var
  attr: TLDAPAttribute;
begin
  result := '';
  attr := self.Find(AttributeName);
  if attr <> nil then
    if attr.Count > 0 then
      result := attr[0];
end;

function TLDAPAttributeList.GetAttribute(Index: integer): TLDAPAttribute;
begin
  result := nil;
  if Index < Count then
    result := TLDAPAttribute(FAttributeList[Index]);
end;

function TLDAPAttributeList.Add: TLDAPAttribute;
begin
  result := TLDAPAttribute.Create;
  FAttributeList.Add(result);
end;

procedure TLDAPAttributeList.Del(Index: integer);
var
  attr: TLDAPAttribute;
begin
  attr := GetAttribute(Index);
  if Assigned(attr) then
    attr.free;
  FAttributeList.Delete(Index);
end;

function TLDAPAttributeList.Find(AttributeName: RawUtf8): TLDAPAttribute;
var
  i: integer;
  attr: TLDAPAttribute;
begin
  result := nil;
  AttributeName := lowercase(AttributeName);
  for i := 0 to Count - 1 do
  begin
    attr := GetAttribute(i);
    if Assigned(attr) then
      if lowercase(attr.AttributeName) = Attributename then
      begin
        result := attr;
        break;
      end;
  end;
end;

{==============================================================================}
constructor TLDAPresult.Create;
begin
  inherited Create;
  FAttributes := TLDAPAttributeList.Create;
end;

destructor TLDAPresult.Destroy;
begin
  FAttributes.Free;
  inherited Destroy;
end;

{==============================================================================}
constructor TLDAPresultList.Create;
begin
  inherited Create;
  FresultList := TList.Create;
end;

destructor TLDAPresultList.Destroy;
begin
  Clear;
  FresultList.Free;
  inherited Destroy;
end;

procedure TLDAPresultList.Clear;
var
  i: integer;
  attr: TLDAPresult;
begin
  for i := Count - 1 downto 0 do
  begin
    attr := Getresult(i);
    if Assigned(attr) then
      attr.Free;
  end;
  FresultList.Clear;
end;

function TLDAPresultList.Count: integer;
begin
  result := FresultList.Count;
end;

function TLDAPresultList.Getresult(Index: integer): TLDAPresult;
begin
  result := nil;
  if Index < Count then
    result := TLDAPresult(FresultList[Index]);
end;

function TLDAPresultList.Add: TLDAPresult;
begin
  result := TLDAPresult.Create;
  FresultList.Add(result);
end;

{==============================================================================}
constructor TLDAPSend.Create;
begin
  inherited Create;
  FReferals := TStringList.Create;
  FFullresult := '';
  FTimeout := 60000;
  FSock := nil;
  FTargetPort := cLDAPProtocol;
  FFullSSL := False;
  FSeq := 0;
  FVersion := 3;
  FSearchScope := SS_WholeSubtree;
  FSearchAliases := SA_Always;
  FSearchSizeLimit := 0;
  FSearchTimeLimit := 0;
  FSearchPageSize := 0;
  FSearchCookie := '';
  FSearchresult := TLDAPresultList.Create;
end;

destructor TLDAPSend.Destroy;
begin
  FSock.Free;
  FSearchresult.Free;
  FReferals.Free;
  inherited Destroy;
end;

function TLDAPSend.GetErrorString(ErrorCode: integer): RawUtf8;
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
      result := 'Compare FALSE';
    6:
      result := 'Compare TRUE';
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
    result := '--unknown--';
  end;
end;

function TLDAPSend.ReceiveString(Length: integer): RawByteString;
begin
  result := '';
  SetLength(result, Length);
  FSock.SockInRead(Pointer(result), Length);
end;

function TLDAPSend.Connect: Boolean;
begin
  // Do not call this function! It is calling by LOGIN method!
  if Assigned(FSock) then
    FSock.Free;
  FSeq := 0;
  try
    FSock := TCrtSocket.Open(FTargetHost, FTargetPort, nlTcp, 10000, FFullSSL);
    FSock.CreateSockIn;
  except on E:ENetSock do
  begin
    FSock := nil;
    result := False;
  end;
  end;
  result := FSock.SockConnected;
  if result then
    FSock.ReceiveTimeout := Timeout;
end;

function TLDAPSend.BuildPacket(const Asn1Data: RawByteString): RawByteString;
begin
  Inc(FSeq);
  result := ASNObject(ASNObject(ASNEncInt(FSeq), ASN1_INT) + Asn1Data,  ASN1_SEQ);
end;

function TLDAPSend.ReceiveResponse: RawByteString;
var
  asn1Type: Byte;
  i, j: integer;
begin
  result := '';
  FFullresult := '';
  try
    FSock.SockInRead(pointer(@asn1Type), 1);
    if asn1Type <> ASN1_SEQ then
      Exit;
    result := AnsiChar(asn1Type);
    FSock.SockInRead(pointer(@asn1Type), 1);
    AppendBufferToRawByteString(result, asn1Type, 1);
    if asn1Type < $80 then
      i := 0
    else
      i := asn1Type and $7F;
    if i > 0 then
      Append(result, [ReceiveString(i)]);
    //get length of LDAP packet
    j := 2;
    i := ASNDecLen(j, result);
    //retreive rest of LDAP packet
    if i > 0 then
      Append(result, [ReceiveString(i)]);
  except on E:ENetSock do
  begin
    result := '';
    Exit;
  end;
  end;
  FFullresult := result;
end;

function TLDAPSend.DecodeResponse(const Asn1Response: RawByteString
  ): RawByteString;
var
  i, x, SeqNumber: integer;
  Svt: Integer;
  s, t: RawByteString;
begin
  result := '';
  FresultCode := -1;
  Fresultstring := '';
  FResponseCode := -1;
  FResponseDN := '';
  FReferals.Clear;
  i := 1;
  ASNItem(i, Asn1Response, Svt);
  SeqNumber := StrToIntDef(ASNItem(i, Asn1Response, Svt), 0);
  if (svt <> ASN1_INT) or (SeqNumber <> FSeq) then
    Exit;
  s := ASNItem(i, Asn1Response, Svt);
  FResponseCode := svt;
  if FResponseCode in [LDAP_ASN1_BIND_RESPONSE, LDAP_ASN1_SEARCH_DONE,
    LDAP_ASN1_MODIFY_RESPONSE, LDAP_ASN1_ADD_RESPONSE, LDAP_ASN1_DEL_RESPONSE,
    LDAP_ASN1_MODIFYDN_RESPONSE, LDAP_ASN1_COMPARE_RESPONSE,
    LDAP_ASN1_EXT_RESPONSE] then
  begin
    FresultCode := StrToIntDef(ASNItem(i, Asn1Response, Svt), -1);
    FResponseDN := ASNItem(i, Asn1Response, Svt);
    FresultString := ASNItem(i, Asn1Response, Svt);
    if FresultString = '' then
      FresultString := GetErrorString(FresultCode);
    if FresultCode = 10 then
    begin
      s := ASNItem(i, Asn1Response, Svt);
      if svt = $A3 then
      begin
        x := 1;
        while x < Length(s) do
        begin
          t := ASNItem(x, s, Svt);
          FReferals.Add(t);
        end;
      end;
    end;
  end;
  result := Copy(Asn1Response, i, Length(Asn1Response) - i + 1);
end;

function TLDAPSend.LdapSasl(Value: RawUtf8): RawByteString;
var
  s, a1, a2, nonce, cnonce, nc, realm, qop, uri, response: RawUtf8;
  l: TStringList;
  n: integer;
begin
  l := TStringList.Create;
  try
    nonce := '';
    realm := '';
    l.CommaText := Value;
    n := IndexByBegin('nonce=', l);
    if n >= 0 then
      nonce := UnQuoteStr(Trim(SeparateRight(l[n], 'nonce=')), '"');
    n := IndexByBegin('realm=', l);
    if n >= 0 then
      realm := UnQuoteStr(Trim(SeparateRight(l[n], 'realm=')), '"');
    cnonce := IntToHex(GetTickCount64, 8);
    nc := '00000001';
    qop := 'auth';
    uri := 'ldap/' + FSock.RemoteIP;
    a1 := md5(FUsername + ':' + realm + ':' + FPassword)
      + ':' + nonce + ':' + cnonce;
    a2 := 'AUTHENTICATE:' + uri;
    s := BinToHex(md5(a1))+':' + nonce + ':' + nc + ':' + cnonce + ':'
      + qop +':'+BinToHex(md5(a2));
    response := BinToHex(md5(s));

    result := 'username="' + Fusername + '",realm="' + realm + '",nonce="';
    result := result + nonce + '",cnonce="' + cnonce + '",nc=' + nc + ',qop=';
    result := result + qop + ',digest-uri="' + uri + '",response=' + response;
  finally
    l.Free;
  end;
end;

function TLDAPSend.TranslateFilter(Filter: RawUtf8): RawByteString;
var
  x: integer;
  c: Ansichar;
  s, t, l, r, attr, rule: RawUtf8;
  dn: Boolean;
begin
  result := '';
  if Filter = '' then
    Exit;
  s := Filter;
  if Filter[1] = '(' then
  begin
    x := RPos(')', Filter);
    s := Copy(Filter, 2, x - 2);
  end;
  if s = '' then
    Exit;
  case s[1] of
    '!':
      // NOT rule (recursive call)
      begin
        result := ASNOBject(TranslateFilter(GetBetween('(', ')', s)), $A2);
      end;
    '&':
      // AND rule (recursive call)
      begin
        repeat
          t := GetBetween('(', ')', s);
          s := Trim(SeparateRight(s, t));
          if s <> '' then
            if s[1] = ')' then
              {$IFDEF CIL}Borland.Delphi.{$ENDIF}System.Delete(s, 1, 1);
          Append(result, [TranslateFilter(t)]);
        until s = '';
        result := ASNOBject(result, $A0);
      end;
    '|':
      // OR rule (recursive call)
      begin
        repeat
          t := GetBetween('(', ')', s);
          s := Trim(SeparateRight(s, t));
          if s <> '' then
            if s[1] = ')' then
              {$IFDEF CIL}Borland.Delphi.{$ENDIF}System.Delete(s, 1, 1);
          Append(result, [TranslateFilter(t)]);
        until s = '';
        result := ASNOBject(result, $A1);
      end;
    else
      begin
        l := Trim(SeparateLeft(s, '='));
        r := Trim(SeparateRight(s, '='));
        if l <> '' then
        begin
          c := l[Length(l)];
          case c of
            ':':
              // Extensible match
              begin
                {$IFDEF CIL}Borland.Delphi.{$ENDIF}System.Delete(l, Length(l), 1);
                dn := False;
                attr := '';
                rule := '';
                if Pos(':dn', l) > 0 then
                begin
                  dn := True;
                  l := StringReplaceAll(l, ':dn', '');
                end;
                attr := Trim(SeparateLeft(l, ':'));
                rule := Trim(SeparateRight(l, ':'));
                if rule = l then
                  rule := '';
                if rule <> '' then
                  result := ASNObject(rule, $81);
                if attr <> '' then
                  Append(result, [ASNObject(attr, $82)]);
                result := result + ASNObject(DecodeTriplet(r, '\'), $83);
                if dn then
                  Append(result, [ASNObject(AsnEncInt($ff), $84)])
                else
                  Append(result, [ASNObject(AsnEncInt(0), $84)]);
                result := ASNOBject(result, $a9);
              end;
            '~':
              // Approx match
              begin
                {$IFDEF CIL}Borland.Delphi.{$ENDIF}System.Delete(l, Length(l), 1);
                result := ASNOBject(l, ASN1_OCTSTR)
                  + ASNOBject(DecodeTriplet(r, '\'), ASN1_OCTSTR);
                result := ASNOBject(result, $a8);
              end;
            '>':
              // Greater or equal match
              begin
                {$IFDEF CIL}Borland.Delphi.{$ENDIF}System.Delete(l, Length(l), 1);
                result := ASNOBject(l, ASN1_OCTSTR)
                  + ASNOBject(DecodeTriplet(r, '\'), ASN1_OCTSTR);
                result := ASNOBject(result, $a5);
              end;
            '<':
              // Less or equal match
              begin
                {$IFDEF CIL}Borland.Delphi.{$ENDIF}System.Delete(l, Length(l), 1);
                result := ASNOBject(l, ASN1_OCTSTR)
                  + ASNOBject(DecodeTriplet(r, '\'), ASN1_OCTSTR);
                result := ASNOBject(result, $a6);
              end;
          else
            // present
            if r = '*' then
              result := ASNOBject(l, $87)
            else
              if Pos('*', r) > 0 then
              // substrings
              begin
                s := Fetch(r, '*');
                if s <> '' then
                  result := ASNOBject(DecodeTriplet(s, '\'), $80);
                while r <> '' do
                begin
                  if Pos('*', r) <= 0 then
                    break;
                  s := Fetch(r, '*');
                  Append(result, [ASNOBject(DecodeTriplet(s, '\'), $81)]);
                end;
                if r <> '' then
                  Append(result, [ASNOBject(DecodeTriplet(r, '\'), $82)]);
                result := ASNOBject(l, ASN1_OCTSTR)
                  + ASNOBject(result, ASN1_SEQ);
                result := ASNOBject(result, $a4);
              end
              else
              begin
                // Equality match
                result := ASNOBject(l, ASN1_OCTSTR)
                  + ASNOBject(DecodeTriplet(r, '\'), ASN1_OCTSTR);
                result := ASNOBject(result, $a3);
              end;
          end;
        end;
      end;
  end;
end;

function TLDAPSend.Login: Boolean;
begin
  result := False;
  if not Connect then
    Exit;
  result := True;
end;

function TLDAPSend.Bind: Boolean;
var
  Query, Response: RawByteString;
begin
  Query := ASNObject(
             ASNObject(ASNEncInt(FVersion), ASN1_INT) +
             ASNObject(FUsername, ASN1_OCTSTR) +
             ASNObject(FPassword, $80),
           LDAP_ASN1_BIND_REQUEST);
  FSock.SockSendFlush(BuildPacket(Query));
  Response := ReceiveResponse;
  DecodeResponse(Response);
  result := FresultCode = 0;
end;

function TLDAPSend.BindSasl: Boolean;
var
  x, xt: integer;
  s, t, digreq: RawByteString;
begin
  result := False;
  if FPassword = '' then
    result := Bind
  else
  begin
    digreq := ASNObject(ASNEncInt(FVersion), ASN1_INT)
      + ASNObject('', ASN1_OCTSTR)
      + ASNObject(ASNObject('DIGEST-MD5', ASN1_OCTSTR), $A3);
    digreq := ASNObject(digreq, LDAP_ASN1_BIND_REQUEST);
    FSock.SockSendFlush(BuildPacket(digreq));
    s := ReceiveResponse;
    t := DecodeResponse(s);
    if FresultCode = 14 then
    begin
      s := t;
      x := 1;
      t := ASNItem(x, s, xt);
      s := ASNObject(ASNEncInt(FVersion), ASN1_INT)
        + ASNObject('', ASN1_OCTSTR)
        + ASNObject(ASNObject('DIGEST-MD5', ASN1_OCTSTR)
          + ASNObject(LdapSasl(t), ASN1_OCTSTR), $A3);
      s := ASNObject(s, LDAP_ASN1_BIND_REQUEST);
      FSock.SockSendFlush(BuildPacket(s));
      s := ReceiveResponse;
      DecodeResponse(s);
      if FresultCode = 14 then
      begin
        FSock.SockSendFlush(BuildPacket(digreq));
        s := ReceiveResponse;
        DecodeResponse(s);
      end;
      result := FresultCode = 0;
    end;
  end;
end;

function TLDAPSend.Logout: Boolean;
begin
  FSock.SockSendFlush(BuildPacket(ASNObject('', LDAP_ASN1_UNBIND_REQUEST)));
  FSock.Free;
  FSock := nil;
  result := True;
end;

function TLDAPSend.Modify(obj: RawUtf8; Op: TLDAPModifyOp;
  const Value: TLDAPAttribute): Boolean;
var
  Query, Response: RawByteString;
  i: integer;
begin
  Query := '';
  for i := 0 to Value.Count -1 do
    Append(Query, [ASNObject(Value[i], ASN1_OCTSTR)]);
  Query := ASNObject(Value.AttributeName, ASN1_OCTSTR) + ASNObject(Query, ASN1_SETOF);
  Query := ASNObject(ASNEncInt(Ord(Op)), ASN1_ENUM) + ASNObject(Query, ASN1_SEQ);
  Query := ASNObject(Query, ASN1_SEQ);
  Query := ASNObject(obj, ASN1_OCTSTR) + ASNObject(Query, ASN1_SEQ);
  Query := ASNObject(Query, LDAP_ASN1_MODIFY_REQUEST);
  FSock.SockSendFlush(BuildPacket(Query));

  Response := ReceiveResponse;
  DecodeResponse(Response);
  result := FresultCode = 0;
end;

function TLDAPSend.Add(obj: RawUtf8; const Value: TLDAPAttributeList): Boolean;
var
  Query, SubQuery, Response: RawByteString;
  i, j: integer;
begin
  Query := '';
  for i := 0 to Value.Count - 1 do
  begin
    SubQuery := '';
    for j := 0 to Value[i].Count - 1 do
      Append(SubQuery, [ASNObject(Value[i][j], ASN1_OCTSTR)]);
    SubQuery := ASNObject(Value[i].AttributeName, ASN1_OCTSTR)
      + ASNObject(SubQuery, ASN1_SETOF);
    Append(Query, [ASNObject(SubQuery, ASN1_SEQ)]);
  end;
  Query := ASNObject(obj, ASN1_OCTSTR) + ASNObject(Query, ASN1_SEQ);
  Query := ASNObject(Query, LDAP_ASN1_ADD_REQUEST);
  FSock.SockSendFlush(BuildPacket(Query));

  Response := ReceiveResponse;
  DecodeResponse(Response);
  result := FresultCode = 0;
end;

function TLDAPSend.Delete(obj: RawUtf8): Boolean;
var
  Query, Response: RawByteString;
begin
  Query := ASNObject(obj, LDAP_ASN1_DEL_REQUEST);
  FSock.SockSendFlush(BuildPacket(Query));

  Response := ReceiveResponse;
  DecodeResponse(Response);
  result := FresultCode = 0;
end;

function TLDAPSend.ModifyDN(obj, newRDN, newSuperior: RawUtf8;
  DeleteoldRDN: Boolean): Boolean;
var
  Query, Response: RawByteString;
begin
  Query := ASNObject(obj, ASN1_OCTSTR) + ASNObject(newRDN, ASN1_OCTSTR);
  if DeleteOldRDN then
    Append(Query, [ASNObject(ASNEncInt($ff), ASN1_BOOL)])
  else
    Append(Query, [ASNObject(ASNEncInt(0), ASN1_BOOL)]);
  if newSuperior <> '' then
    Append(Query, [ASNObject(newSuperior, $80)]);
  Query := ASNObject(Query, LDAP_ASN1_MODIFYDN_REQUEST);
  FSock.SockSendFlush(BuildPacket(Query));

  Response := ReceiveResponse;
  DecodeResponse(Response);
  result := FresultCode = 0;
end;

function TLDAPSend.Compare(obj, AttributeValue: RawUtf8): Boolean;
var
  Query, Response: RawByteString;
begin
  Query := ASNObject(Trim(SeparateLeft(AttributeValue, '=')), ASN1_OCTSTR)
    + ASNObject(Trim(SeparateRight(AttributeValue, '=')), ASN1_OCTSTR);
  Query := ASNObject(obj, ASN1_OCTSTR) + ASNObject(Query, ASN1_SEQ);
  Query := ASNObject(Query, LDAP_ASN1_COMPARE_REQUEST);
  FSock.SockSendFlush(BuildPacket(Query));

  Response := ReceiveResponse;
  DecodeResponse(Response);
  result := FresultCode = 0;
end;

function TLDAPSend.Search(BaseDN: RawUtf8; TypesOnly: Boolean; Filter: RawUtf8;
  const Attributes: TStrings): Boolean;
var
  s, t, c: RawByteString;
  u: RawUtf8;
  n, i, x: integer;
  r: TLDAPresult;
  a: TLDAPAttribute;
begin
  FSearchresult.Clear;
  FReferals.Clear;
  s := ASNObject(BaseDN, ASN1_OCTSTR);
  Append(s, [ASNObject(ASNEncInt(Ord(FSearchScope)), ASN1_ENUM),
            ASNObject(ASNEncInt(Ord(FSearchAliases)), ASN1_ENUM),
            ASNObject(ASNEncInt(FSearchSizeLimit), ASN1_INT),
            ASNObject(ASNEncInt(FSearchTimeLimit), ASN1_INT)]);
  if TypesOnly then
    Append(s, [ASNObject(ASNEncInt($ff), ASN1_BOOL)])
  else
    Append(s, [ASNObject(ASNEncInt(0), ASN1_BOOL)]);
  if Filter = '' then
    Filter := '(objectclass=*)';
  t := TranslateFilter(Filter);
  if t = '' then
    Append(s, [ASNObject('', ASN1_NULL)])
  else
    Append(s, [t]);
  t := '';
  for n := 0 to Attributes.Count - 1 do
    Append(t, [ASNObject(Attributes[n], ASN1_OCTSTR)]);
  Append(s, [ASNObject(t, ASN1_SEQ)]);
  s := ASNObject(s, LDAP_ASN1_SEARCH_REQUEST);
  if FSearchPageSize > 0 then
  begin
    c := ASNObject('1.2.840.113556.1.4.319', ASN1_OCTSTR); // controlType: pagedresultsControl
    Append(c, [ASNObject(ASNEncInt(0), ASN1_BOOL)]); // criticality: FALSE
    t := ASNObject(ASNEncInt(FSearchPageSize), ASN1_INT); // page size
    Append(t, [ASNObject(FSearchCookie, ASN1_OCTSTR)]); // search cookie
    t := ASNObject(t, ASN1_SEQ); // wrap with SEQUENCE
    Append(c, [ASNObject(t, ASN1_OCTSTR)]); // add searchControlValue as OCTET STRING
    c := ASNObject(c, ASN1_SEQ); // wrap with SEQUENCE
    Append(s, [ASNObject(c, LDAP_ASN1_CONTROLS)]); // append Controls to SearchRequest
  end;
  FSock.SockSendFlush(BuildPacket(s));
  repeat
    s := ReceiveResponse;
    t := DecodeResponse(s);
    if FResponseCode = LDAP_ASN1_SEARCH_ENTRY then
    begin
      r := FSearchresult.Add;
      n := 1;
      r.ObjectName := ASNItem(n, t, x);
      ASNItem(n, t, x);
      if x = ASN1_SEQ then
      begin
        while n < Length(t) do
        begin
          s := ASNItem(n, t, x);
          if x = ASN1_SEQ then
          begin
            i := n + Length(s);
            a := r.Attributes.Add;
            u := ASNItem(n, t, x);
            a.AttributeName := u;
            ASNItem(n, t, x);
            if x = ASN1_SETOF then
              while n < i do
              begin
                u := ASNItem(n, t, x);
                a.Add(u);
              end;
          end;
        end;
      end;
    end;
    if FResponseCode = LDAP_ASN1_SEARCH_REFERENCE then
    begin
      n := 1;
      while n < Length(t) do
        FReferals.Add(ASNItem(n, t, x));
    end;
  until FResponseCode = LDAP_ASN1_SEARCH_DONE;
  n := 1;
  ASNItem(n, t, x);
  if x = LDAP_ASN1_CONTROLS then
  begin
    ASNItem(n, t, x);
    if x = ASN1_SEQ then
    begin
      s := ASNItem(n, t, x);
      if s = '1.2.840.113556.1.4.319' then
      begin
        s := ASNItem(n, t, x); // searchControlValue
        n := 1;
        ASNItem(n, s, x);
        if x = ASN1_SEQ then
        begin
          ASNItem(n, s, x); // total number of result records, if known, otherwise 0
          FSearchCookie := ASNItem(n, s, x); // active search cookie, empty when done
        end;
      end;
    end;
  end;
  result := FresultCode = 0;
end;

function TLDAPSend.Extended(const OID, Value: RawUtf8): Boolean;
var
  Query, Response, DecodedResponse: RawByteString;
  itemStart, xt: integer;
begin
  Query := ASNObject(OID, $80);
  if Value <> '' then
    Append(Query, [ASNObject(Value, $81)]);
  Query := ASNObject(Query, LDAP_ASN1_EXT_REQUEST);
  FSock.SockSendFlush(BuildPacket(Query));

  Response := ReceiveResponse;
  DecodedResponse := DecodeResponse(Response);
  result := FresultCode = 0;
  if result then
  begin
    itemStart := 1;
    FExtName := ASNItem(itemStart, DecodedResponse, xt);
    FExtValue := ASNItem(itemStart, DecodedResponse, xt);
  end;
end;

{==============================================================================}
function LDAPresultDump(const Value: TLDAPresultList): RawUtf8;
var
  i, j, k: integer;
  res: TLDAPresult;
  attr: TLDAPAttribute;
begin
  result := 'results: ' + IntToStr(Value.Count) + CRLF +CRLF;
  for i := 0 to Value.Count - 1 do
  begin
    result := result + 'result: ' + IntToStr(i) + CRLF;
    res := Value[i];
    result := result + '  Object: ' + res.ObjectName + CRLF;
    for j := 0 to res.Attributes.Count - 1 do
    begin
      attr := res.Attributes[j];
      result := result + '  Attribute: ' + attr.AttributeName + CRLF;
      for k := 0 to attr.Count - 1 do
        result := result + '    ' + attr[k] + CRLF;
    end;
  end;
end;

end.

