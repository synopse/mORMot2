/// low-level access to the Delphi RTL System.Net.HttpClient
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.lib.nethttp;

{
  *****************************************************************************

   Delphi RTL HTTP Client Wrapper
   - TNetHttpConnection over System.Net.HttpClient, i.e. the OS TLS stack

   The Delphi Android and iOS targets have neither OpenSSL nor libcurl, so
   mormot.net.client uses this wrapper (TDelphiNetHttp) for HTTPS. The RTL
   unit is referenced in the implementation section only, and no RTL type is
   published here, because its TURI/THTTPRequest identifiers would conflict
   with the mORMot ones.

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

{$ifdef USEDELPHINETHTTP}

uses
  sysutils,
  classes;

type
  /// a HTTP/1.1 client connection over System.Net.HttpClient.THTTPClient
  // - an instance is not thread-safe: use one per thread
  TNetHttpConnection = class
  protected
    fClient: TObject; // THTTPClient
    fValidator: TObject;
    fIgnoreCertificateErrors: boolean;
  public
    /// initialize the connection with the given timeouts (in ms)
    // - aProxy is either '' (use the OS settings) or a proxy URL/host:port
    constructor Create(aConnectionTimeout, aSendTimeout, aResponseTimeout: integer;
      const aProxy: string);
    /// finalize the connection
    destructor Destroy; override;
    /// perform a request
    // - aHeaders/OutHeaders are CRLF separated 'Name: Value' lines
    // - OutContentEncoding/OutAcceptEncoding return the matching response headers
    // - raise an exception on connection or TLS error
    function Request(const aMethod, aUrl, aHeaders: string;
      const aBody: RawByteString; aRedirectMax: integer; const aUserAgent: string;
      out OutHeaders, OutContentEncoding, OutAcceptEncoding: string;
      out OutBody: RawByteString): integer;
    /// if any TLS certificate should be accepted
    property IgnoreCertificateErrors: boolean
      read fIgnoreCertificateErrors write fIgnoreCertificateErrors;
  end;

{$endif USEDELPHINETHTTP}

implementation

{$ifdef USEDELPHINETHTTP}

uses
  System.Net.URLClient,
  System.Net.HttpClient;

type
  TNetHttpValidator = class
  public
    Owner: TNetHttpConnection;
    procedure Validate(const Sender: TObject; const ARequest: TURLRequest;
      const Certificate: TCertificate; var Accepted: Boolean);
  end;

procedure TNetHttpValidator.Validate(const Sender: TObject;
  const ARequest: TURLRequest; const Certificate: TCertificate; var Accepted: Boolean);
begin
  // Accepted is already true if the OS trusted the certificate
  if Owner.fIgnoreCertificateErrors then
    Accepted := true;
end;


{ TNetHttpConnection }

constructor TNetHttpConnection.Create(aConnectionTimeout, aSendTimeout,
  aResponseTimeout: integer; const aProxy: string);
var
  c: THTTPClient;
  v: TNetHttpValidator;
begin
  inherited Create;
  c := THTTPClient.Create;
  fClient := c;
  if aConnectionTimeout > 0 then
    c.ConnectionTimeout := aConnectionTimeout;
  if aSendTimeout > 0 then
    c.SendTimeout := aSendTimeout;
  if aResponseTimeout > 0 then
    c.ResponseTimeout := aResponseTimeout;
  c.AllowCookies := false; // as the other THttpRequest classes
  if (aProxy <> '') and
     not SameText(aProxy, 'none') then
    c.ProxySettings := TProxySettings.Create(aProxy);
  v := TNetHttpValidator.Create;
  v.Owner := self;
  fValidator := v;
  c.OnValidateServerCertificate := v.Validate;
end;

destructor TNetHttpConnection.Destroy;
begin
  fClient.Free;
  fValidator.Free;
  inherited Destroy;
end;

function StillPacked(const aEncoding: string; const aBody: RawByteString): boolean;
begin
  // under iOS, NSURLSession unpacks gzip/deflate itself but keeps
  // reporting Content-Encoding - unpacking it a second time then fails with
  // "gzip uncompress error". So we decide by the content, not by the header.
  result := false;
  if (aEncoding = '') or
     (length(aBody) < 2) then
    exit;
  if SameText(aEncoding, 'gzip') then
    result := (PByteArray(aBody)[0] = $1f) and  // gzip magic
              (PByteArray(aBody)[1] = $8b)
  else if SameText(aEncoding, 'deflate') then
    result := PByteArray(aBody)[0] = $78        // zlib header
  else
    result := true; // e.g. mORMot's own synlz, which no OS ever touches
end;

function TNetHttpConnection.Request(const aMethod, aUrl, aHeaders: string;
  const aBody: RawByteString; aRedirectMax: integer; const aUserAgent: string;
  out OutHeaders, OutContentEncoding, OutAcceptEncoding: string;
  out OutBody: RawByteString): integer;
var
  c: THTTPClient;
  req: IHTTPRequest;
  resp: IHTTPResponse;
  src, dst: TMemoryStream;
  lines: TStringList;
  h: TNetHeaders;
  i, p: integer;
  n: string;
begin
  c := THTTPClient(fClient);
  c.HandleRedirects := aRedirectMax > 0;
  if aRedirectMax > 0 then
    c.MaxRedirects := aRedirectMax;
  req := c.GetRequest(aMethod, aUrl);
  if aUserAgent <> '' then
    req.UserAgent := aUserAgent;
  lines := TStringList.Create;
  src := nil;
  dst := TMemoryStream.Create;
  try
    lines.Text := aHeaders;
    for i := 0 to lines.Count - 1 do
    begin
      p := Pos(':', lines[i]);
      if p > 1 then
        req.AddHeader(Trim(Copy(lines[i], 1, p - 1)),
          Trim(Copy(lines[i], p + 1, MaxInt)));
    end;
    if aBody <> '' then
    begin
      src := TMemoryStream.Create;
      src.WriteBuffer(pointer(aBody)^, length(aBody));
      src.Position := 0;
      req.SourceStream := src;
    end;
    resp := c.Execute(req, dst);
    result := resp.StatusCode;
    h := resp.Headers;
    OutHeaders := '';
    OutContentEncoding := '';
    OutAcceptEncoding := '';
    for i := 0 to high(h) do
    begin
      n := h[i].Name;
      if n = '' then
        continue; // e.g. the status line on Android
      OutHeaders := OutHeaders + n + ': ' + h[i].Value + #13#10;
      if SameText(n, 'Content-Encoding') then
        OutContentEncoding := h[i].Value
      else if SameText(n, 'Accept-Encoding') then
        OutAcceptEncoding := h[i].Value;
    end;
    SetString(OutBody, PAnsiChar(dst.Memory), dst.Size);
    if not StillPacked(OutContentEncoding, OutBody) then
      OutContentEncoding := ''; // the OS did unpack it for us
  finally
    dst.Free;
    src.Free;
    lines.Free;
  end;
end;


{$endif USEDELPHINETHTTP}

end.
