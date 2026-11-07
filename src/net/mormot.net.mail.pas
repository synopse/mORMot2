/// Native MIME E-Mail Message Builder and SMTP Client for mORMot 2
// - extension unit, licensed under the same MPL/GPL/LGPL
// tri-license terms as mORMot 2 itself
// - no dependency on Indy: only mormot.core.* and mormot.net.sock are used
unit mormot.net.mail;

{
  *****************************************************************************

   RFC 5322 / MIME (RFC 2045-2047, 2231) E-Mail Builder and SMTP Client
    - Mail Address and Header Encoding Functions
    - TMailMessage MIME Message Builder (text, html, attachments, inline images)
    - TSmtpClient SMTP/ESMTP Client with STARTTLS / Implicit TLS / XOAUTH2
    - Send Email using the SMTP Protocol (former mormot.net.client functions)

  Platform independent: Delphi 7 up to Delphi 13, FPC on Windows/Linux/BSD/Mac
  - TLS is provided by SChannel on Windows (no OpenSSL DLL needed), and by
    OpenSSL 1.1/3.x on POSIX (mormot.lib.openssl11 is included automatically)

  *****************************************************************************
}

interface

{$I mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.datetime,
  {$ifdef USE_OPENSSL}
  {$ifdef OSPOSIX}
  mormot.lib.openssl11, // register OpenSSL as TLS layer for TCrtSocket on POSIX
  {$endif OSPOSIX}
  {$endif USE_OPENSSL}
  mormot.net.sock,
  mormot.net.http; // GetHeader() DeleteHeader() MimeHeaderEncode()


{ ************ Mail Address and Header Encoding Functions }

type
  /// exception raised by TMailMessage
  EMailMessage = class(ESynException);

  /// one e-mail address, with an optional display name
  // - both fields are UTF-8 encoded
  TMailAddress = record
    /// the display name, e.g. 'Juergen Mueller' (optional)
    Name: RawUtf8;
    /// the address itself, e.g. 'jm@example.com'
    Address: RawUtf8;
  end;
  PMailAddress = ^TMailAddress;

  /// a dynamic array of e-mail addresses
  TMailAddressDynArray = array of TMailAddress;

/// fill a TMailAddress record from its UTF-8 values
function MailAddress(const Address: RawUtf8; const Name: RawUtf8 = ''): TMailAddress;

/// parse a single address text, e.g. 'a@b.de', '<a@b.de>',
// 'Name <a@b.de>' or '"Last, First" <a@b.de>'
// - returns false if no '@' was found in the address part
function MailAddressParse(const Text: RawUtf8; out Addr: TMailAddress): boolean;

/// parse and append a list of addresses, separated by ',' or ';'
// - separators within "quoted names" or <...> are properly ignored
// - returns the number of added addresses
function MailAddressListAdd(var List: TMailAddressDynArray;
  const Text: RawUtf8): integer;

/// encode an address for a To:/From:/Cc: header
// - quote or RFC 2047 encode the display name as needed
function MailAddressEncode(const Addr: TMailAddress): RawUtf8;

/// encode a UTF-8 header value following RFC 2047
// - pure 7-bit ASCII text is returned as such
// - otherwise, return '=?UTF-8?B?...?=' encoded-words of max 75 chars,
// folded with CRLF + space, never splitting a multi-byte UTF-8 character
// - CR/LF in Text are converted into spaces to avoid header injection
function MailHeaderEncode(const Text: RawUtf8): RawUtf8;

/// encode some binary/text content as Quoted-Printable (RFC 2045 6.7)
// - any line feed (CRLF, LF or CR) is normalized to a CRLF hard line break
// - lines are soft-wrapped at 76 chars; a leading '.' is always encoded
function QuotedPrintableEncode(const Data: RawByteString): RawUtf8;

/// return a RFC 5322 date header value, e.g. 'Thu, 08 Oct 2026 10:15:00 +0000'
// - UtcDateTime = 0 will use NowUtc
function MailDateTime(UtcDateTime: TDateTime = 0): RawUtf8;

/// return Bytes random bytes as lowercase hexadecimal text
function MailRandomHex(Bytes: integer): RawUtf8;

/// guess the MIME content type from a file name and/or its content
// - known Office/OpenDocument extensions are checked first, then mORMot
// GetMimeContentTypeFromExt/GetMimeContentTypeFromBuffer
// - default is 'application/octet-stream'
function MailContentType(const FileName: TFileName;
  const Content: RawByteString): RawUtf8;


{ ************ TMailMessage MIME Message Builder }

type
  /// the Content-Transfer-Encoding used for text/plain and text/html parts
  // - attachments are always Base64 encoded
  TMailEncoding = (
    meQuotedPrintable,
    meBase64);

  /// the message priority, as X-Priority and Importance headers
  TMailPriority = (
    mpNormal,
    mpHighest,
    mpHigh,
    mpLow,
    mpLowest);

  /// one attachment or inline (related) resource of a TMailMessage
  TMailAttachment = record
    /// the UTF-8 file name, as shown to the recipient
    FileName: RawUtf8;
    /// the MIME type, e.g. 'application/pdf'
    ContentType: RawUtf8;
    /// non-empty for an inline resource referenced as <img src="cid:...">
    ContentId: RawUtf8;
    /// the raw binary content
    Content: RawByteString;
  end;
  PMailAttachment = ^TMailAttachment;

  /// a dynamic array of attachments
  TMailAttachmentDynArray = array of TMailAttachment;

  /// native RFC 5322 / MIME e-mail message builder
  // - replaces TIdMessage + TIdMessageBuilderHtml/Plain with no Indy dependency
  // - generates the proper multipart/mixed, multipart/related and
  // multipart/alternative structure, depending on the supplied content
  // - all text is stored as UTF-8 and sent with charset="utf-8"
  // - string properties (Subject, PlainText, Html) convert from/to the
  // native string type (AnsiString on Delphi 7, UnicodeString since 2009)
  TMailMessage = class
  protected
    fFrom: TMailAddress;
    fSender: TMailAddress;
    fReplyTo, fTo, fCc, fBcc: TMailAddressDynArray;
    fSubject, fPlainText, fHtml: RawUtf8;
    fAttachments: TMailAttachmentDynArray;
    fHeaders: RawUtf8;
    fMessageId: RawUtf8;
    fDate: TDateTime;
    fPriority: TMailPriority;
    fTextEncoding: TMailEncoding;
    fReadReceipt: boolean;
    function GetSubject: string;
    procedure SetSubject(const Value: string);
    function GetPlainText: string;
    procedure SetPlainText(const Value: string);
    function GetHtml: string;
    procedure SetHtml(const Value: string);
    procedure AddAddress(var List: TMailAddressDynArray;
      const Address: RawUtf8; const Name: string);
    function DoAddAttachment(const Content: RawByteString;
      const FileName: RawUtf8; const ContentType, ContentId: RawUtf8): PtrInt;
    function HasInline: boolean;
    function HasAttached: boolean;
  public
    /// initialize a new empty message
    constructor Create; virtual;
    /// reset all content, recipients and attachments
    procedure Clear; virtual;
    /// set the From: address
    procedure SetFrom(const Address: RawUtf8; const Name: string = '');
    /// set an optional Sender: address (e.g. sending on behalf of From)
    // - will also be used as SMTP envelope sender (MAIL FROM) if set
    procedure SetSender(const Address: RawUtf8; const Name: string = '');
    /// add a To: recipient
    // - if Name is '', Address may be a list like 'a@b.de, Name <c@d.de>'
    procedure AddTo(const Address: RawUtf8; const Name: string = '');
    /// add a Cc: recipient - same parameters as AddTo()
    procedure AddCc(const Address: RawUtf8; const Name: string = '');
    /// add a Bcc: recipient - never written to the headers, only to envelope
    procedure AddBcc(const Address: RawUtf8; const Name: string = '');
    /// add a Reply-To: address
    procedure AddReplyTo(const Address: RawUtf8; const Name: string = '');
    /// add a file as attachment
    // - ContentType is guessed via MailContentType() if not specified
    // - DisplayName overrides the file name shown to the recipient
    // - returns the index in Attachments[]
    function AddAttachment(const FileName: TFileName;
      const ContentType: RawUtf8 = ''; const DisplayName: string = ''): PtrInt;
    /// add some in-memory content as attachment
    function AddAttachmentData(const Content: RawByteString;
      const FileName: string; const ContentType: RawUtf8 = ''): PtrInt;
    /// add an image file as inline resource for the HTML body
    // - returns the Content-ID to be used as <img src="cid:...">
    // - a random Content-ID is generated if none is supplied
    function AddInlineImage(const FileName: TFileName;
      const ContentId: RawUtf8 = ''): RawUtf8;
    /// add some in-memory content as inline resource for the HTML body
    function AddInlineData(const Content: RawByteString; const FileName: string;
      const ContentId: RawUtf8 = ''; const ContentType: RawUtf8 = ''): RawUtf8;
    /// add a custom header, e.g. AddHeader('X-Mailer', 'MyApp 1.0')
    // - Value is RFC 2047 encoded if it contains non-ASCII characters
    procedure AddHeader(const Name, Value: RawUtf8);
    /// generate the whole RFC 5322 message, with CRLF line ends
    // - this is the raw content as sent after the SMTP DATA command,
    // or as saved in a .eml file
    // - a Message-ID is generated (and stored in MessageId) if none was set
    function Build: RawByteString;
    /// save the generated message as a .eml file
    procedure SaveToFile(const FileName: TFileName);
    /// all SMTP envelope recipients, i.e. To + Cc + Bcc addresses
    function Envelope: TRawUtf8DynArray;
    /// the SMTP envelope sender, i.e. Sender address if set, or From address
    function EnvelopeFrom: RawUtf8;
    /// the From: address
    property From: TMailAddress
      read fFrom write fFrom;
    /// the optional Sender: address
    property Sender: TMailAddress
      read fSender write fSender;
    /// the To: recipients
    property ToList: TMailAddressDynArray
      read fTo write fTo;
    /// the Cc: recipients
    property CcList: TMailAddressDynArray
      read fCc write fCc;
    /// the Bcc: recipients
    property BccList: TMailAddressDynArray
      read fBcc write fBcc;
    /// the Reply-To: addresses
    property ReplyToList: TMailAddressDynArray
      read fReplyTo write fReplyTo;
    /// the subject, as native string
    property Subject: string
      read GetSubject write SetSubject;
    /// the subject, as UTF-8
    property SubjectUtf8: RawUtf8
      read fSubject write fSubject;
    /// the plain text body, as native string
    property PlainText: string
      read GetPlainText write SetPlainText;
    /// the plain text body, as UTF-8
    property PlainTextUtf8: RawUtf8
      read fPlainText write fPlainText;
    /// the HTML body, as native string
    property Html: string
      read GetHtml write SetHtml;
    /// the HTML body, as UTF-8
    property HtmlUtf8: RawUtf8
      read fHtml write fHtml;
    /// the attachments and inline resources
    property Attachments: TMailAttachmentDynArray
      read fAttachments;
    /// the Message-ID, without <>; generated by Build if empty
    property MessageId: RawUtf8
      read fMessageId write fMessageId;
    /// the Date: header, as UTC TDateTime - 0 means NowUtc at Build
    property Date: TDateTime
      read fDate write fDate;
    /// the message priority
    property Priority: TMailPriority
      read fPriority write fPriority;
    /// how text/plain and text/html parts are encoded
    property TextEncoding: TMailEncoding
      read fTextEncoding write fTextEncoding;
    /// add a Disposition-Notification-To header (read receipt request)
    property ReadReceipt: boolean
      read fReadReceipt write fReadReceipt;
  end;


{ ************ TSmtpClient SMTP/ESMTP Client }

type
  /// exception raised by TSmtpClient and SendEmail()
  ESmtpClient = class(ESynException);

  /// exception class raised by SendEmail() on raw SMTP process
  // - backward compatible alias of the former mormot.net.client class
  ESendEmail = ESmtpClient;

  /// how TLS is used by TSmtpClient
  // - smtpAuto: implicit TLS on port 465, otherwise STARTTLS if the server
  // advertises it (plain connection if not)
  // - smtpNone: never use TLS
  // - smtpImplicitTls: TLS from the first byte (port 465 "SMTPS")
  // - smtpStartTls: plain connection upgraded by STARTTLS - mandatory, i.e.
  // raise ESmtpClient if the server does not support it (port 587)
  TSmtpSecurity = (
    smtpAuto,
    smtpNone,
    smtpImplicitTls,
    smtpStartTls);

  /// the authentication method used by TSmtpClient
  // - sauAuto: XOAUTH2 if OAuth2Token is set, PLAIN or LOGIN if UserName is set
  // (as advertised by the server), none otherwise
  TSmtpAuth = (
    sauAuto,
    sauNone,
    sauPlain,
    sauLogin,
    sauXOAuth2);

  /// event to log the SMTP dialog - passwords/tokens are never logged
  TOnSmtpLog = procedure(Sender: TObject; const Line: RawUtf8;
    Outgoing: boolean) of object;

  /// native SMTP/ESMTP client, using mORMot TCrtSocket
  // - replaces TIdSMTP, with no Indy nor external OpenSSL DLL on Windows
  // - one connection can send several messages: call Connect once, then
  // Send() several times, then Disconnect - or just call Send() which
  // will connect on demand
  TSmtpClient = class
  protected
    fSock: TCrtSocket;
    fHost, fPort, fUserName, fPassword, fOAuth2Token, fHeloName: RawUtf8;
    fSecurity: TSmtpSecurity;
    fAuth: TSmtpAuth;
    fTimeOut: integer;
    fCapabilities: RawUtf8;
    fLastResponse: RawUtf8;
    fLastCode: integer;
    fTlsActive: boolean;
    fOnLog: TOnSmtpLog;
    procedure Log(const Line: RawUtf8; Outgoing: boolean);
    function ReadResponse: integer;
    function Exec(const Command: RawUtf8; const LogAs: RawUtf8 = ''): integer;
    function Cmd(const Command: RawUtf8; const Expected: array of integer;
      const LogAs: RawUtf8 = ''): integer;
    procedure CheckCode(const Expected: array of integer; const Context: RawUtf8);
    procedure DoEhlo;
    procedure DoAuth;
    function HasCapability(const Name: RawUtf8): boolean;
    function HasAuth(const Mechanism: RawUtf8): boolean;
  public
    /// TLS options, e.g. Tls.IgnoreCertificateErrors := true for a
    // self-signed intranet server certificate
    Tls: TNetTlsContext;
    /// initialize the client - Connect will be called on the first Send
    constructor Create(const aHost: RawUtf8 = ''; const aPort: RawUtf8 = '587';
      const aUserName: RawUtf8 = ''; const aPassword: RawUtf8 = ''); virtual;
    /// send QUIT and release the connection
    destructor Destroy; override;
    /// connect, EHLO, (STARTTLS, EHLO), and authenticate
    // - raise ESmtpClient or ENetSock on error
    procedure Connect;
    /// send QUIT (best effort) and close the connection
    procedure Disconnect;
    /// true if a connection is currently established
    function Connected: boolean;
    /// send a raw RFC 5322 message, as generated by TMailMessage.Build
    // - Data is normalized to CRLF and dot-stuffed before being sent
    // - raise ESmtpClient on any rejected command (sending RSET first)
    procedure SendRaw(const MailFrom: RawUtf8; const Recipients: array of RawUtf8;
      const Data: RawByteString);
    /// build and send a TMailMessage
    procedure Send(Msg: TMailMessage);
    /// the SMTP server host name or IP
    property Host: RawUtf8
      read fHost write fHost;
    /// the SMTP server port, e.g. '25', '465' or '587' (default)
    property Port: RawUtf8
      read fPort write fPort;
    /// the login name
    property UserName: RawUtf8
      read fUserName write fUserName;
    /// the login password
    property Password: RawUtf8
      read fPassword write fPassword;
    /// an OAuth2 access token, for XOAUTH2 (Microsoft 365, Gmail)
    property OAuth2Token: RawUtf8
      read fOAuth2Token write fOAuth2Token;
    /// the name sent with EHLO - default is the local computer name
    property HeloName: RawUtf8
      read fHeloName write fHeloName;
    /// TLS mode - default smtpAuto
    property Security: TSmtpSecurity
      read fSecurity write fSecurity;
    /// authentication method - default sauAuto
    property Auth: TSmtpAuth
      read fAuth write fAuth;
    /// connection/send/receive timeout in milliseconds - default 30000
    property TimeOut: integer
      read fTimeOut write fTimeOut;
    /// the EHLO response lines, i.e. the server extensions
    property Capabilities: RawUtf8
      read fCapabilities;
    /// the last (multi-line) server response text
    property LastResponse: RawUtf8
      read fLastResponse;
    /// the last server response code, e.g. 250
    property LastCode: integer
      read fLastCode;
    /// true if the current connection is encrypted
    property TlsActive: boolean
      read fTlsActive;
    /// optional event to log the SMTP dialog
    property OnLog: TOnSmtpLog
      read fOnLog write fOnLog;
  end;

/// normalize line feeds to CRLF, apply SMTP dot-stuffing (RFC 5321 4.5.2)
// and append the final CRLF.CRLF terminator
function SmtpDotStuff(const Data: RawByteString): RawByteString;

/// one-shot helper: connect, send the message, and disconnect
// - returns the final server response (e.g. '250 2.0.0 Ok: queued as ...')
// - raise ESmtpClient or ENetSock on error
function SendMailMessage(Msg: TMailMessage; const Host, Port, UserName,
  Password: RawUtf8; Security: TSmtpSecurity = smtpAuto;
  IgnoreCertificateErrors: boolean = false): RawUtf8;


{ ************ Send Email using the SMTP Protocol }

// - those were defined in mormot.net.client up to mORMot 2.4: just add
// mormot.net.mail to your uses clause - they now run a TSmtpClient instance

const
  /// the layout of TSmtpConnection.FromText method
  SMTP_DEFAULT = 'user:password@smtpserver:port';

type
  /// may be used to store a connection to a SMTP server
  // - see SendEmail() overloaded function
  {$ifdef USERECORDWITHMETHODS}
  TSmtpConnection = record
  {$else}
  TSmtpConnection = object
  {$endif USERECORDWITHMETHODS}
  public
    /// the SMTP server IP or host name
    Host: RawUtf8;
    /// the SMTP server port (25 by default)
    Port: RawUtf8;
    /// the SMTP user login (if any)
    User: RawUtf8;
    /// the SMTP user password (if any)
    Pass: RawUtf8;
    /// fill the STMP server information from a single text field
    // - expects 'user:password@smtpserver:port' format
    // - if aText equals SMTP_DEFAULT ('user:password@smtpserver:port'),
    // does nothing
    function FromText(const aText: RawUtf8): boolean;
  end;

  /// how TLS should be applied when sending an email via SendEmail()
  // - stlsNone: plain text connection (typically port 25)
  // - stlsImplicit: TLS from the first byte of the connection (typically port 465)
  // - stlsStartTls: plain connection upgraded to TLS via the STARTTLS command
  // after EHLO (typically the submission port 587, as expected by most providers)
  // - is mapped to smtpNone, smtpImplicitTls and smtpStartTls TSmtpSecurity
  TSmtpTls = (
    stlsNone,
    stlsImplicit,
    stlsStartTls);

/// send an email using the SMTP protocol
// - retry true on success, false if CsvDest is void or if the server could
// not be reached - raise ESendEmail if the server rejected a SMTP command
// - the Subject is expected to be in plain 7-bit ASCII, so you could use
// SendEmailSubject() to encode it as Unicode, if needed
// - by default, the text body is expected to be in ISO-8859-1, i.e. Utf8ToWinAnsi
// - you can optionally set another encoding charset or force TextCharSet='' to
// expect the 'Content-Type:' to be set in Headers and Text to be the raw body
// (e.g. a multi-part encoded message, as generated by TMailMessage.Build)
// - set Tls to stlsImplicit for TLS-from-start (port 465) or stlsStartTls to
// upgrade a plain connection via the STARTTLS command (port 587)
// - optional TlsCtx could supply a TNetTlsContext to customize the TLS handshake
// (e.g. TlsCtx^.IgnoreCertificateErrors), used for stlsImplicit and stlsStartTls
// - consider using TMailMessage and TSmtpClient for any new code
function SendEmail(const Server, From, CsvDest, Subject: RawUtf8;
  const Text: RawByteString; const Headers: RawUtf8 = ''; const User: RawUtf8 = '';
  const Pass: RawUtf8 = ''; const Port: RawUtf8 = '25';
  const TextCharSet: RawUtf8  =  'ISO-8859-1'; Tls: TSmtpTls = stlsNone;
  TlsCtx: PNetTlsContext = nil): boolean; overload;

/// send an email using the SMTP protocol via a TSmtpConnection definition
// - retry true on success
// - the Subject is expected to be in plain 7-bit ASCII, so you could use
// SendEmailSubject() to encode it as Unicode, if needed
// - by default, the text body is expected to be in ISO-8859-1, i.e. Utf8ToWinAnsi
// - you can optionally set another encoding charset or force TextCharSet='' to
// expect the 'Content-Type:' to be set in Headers and Text to be the raw body
// (e.g. a multi-part encoded message)
// - if Tls is left to stlsNone, it will be guessed from the port number:
// stlsImplicit for port 465, or stlsStartTls for port 587
function SendEmail(const Server: TSmtpConnection;
  const From, CsvDest, Subject: RawUtf8; const Text: RawByteString;
  const Headers: RawUtf8 = ''; const TextCharSet: RawUtf8  = 'ISO-8859-1';
  Tls: TSmtpTls = stlsNone; TlsCtx: PNetTlsContext = nil): boolean; overload;

/// convert a supplied subject text into an Unicode encoding
// - will convert the text into UTF-8 and append '=?UTF-8?B?'
// - for pre-Unicode versions of Delphi, Text is expected to be already UTF-8
// encoded - since Delphi 2010, it will be converted from UnicodeString
function SendEmailSubject(const Text: string): RawUtf8;


implementation


{ ************ Mail Address and Header Encoding Functions }

const
  /// RFC 5322 "specials" which require a display name to be quoted
  NAME_SPECIALS: TSynAnsicharSet = [
    '(', ')', '<', '>', '[', ']', ':', ';', '@', '\', ',', '.', '"'];
  /// chars which are never allowed within an address
  ADDR_INVALID: TSynAnsicharSet = [
    #0..' ', '<', '>', '"', ',', ';', '(', ')', '[', ']', '\'];

// replace any #0..#31 control char (e.g. CR/LF) by a space: no header injection
function HeaderSafe(const S: RawUtf8): RawUtf8;
begin
  result := S;
  if HasControlChars(pointer(S), length(S)) then
  begin
    UniqueRawUtf8(result);
    TrimControlCharsBuffer(pointer(result), length(result));
  end;
end;

function MailAddress(const Address, Name: RawUtf8): TMailAddress;
begin
  result.Address := TrimU(Address);
  result.Name := TrimU(Name);
end;

function MailAddressParse(const Text: RawUtf8; out Addr: TMailAddress): boolean;
var
  t, n: RawUtf8;
  i: PtrInt;
begin
  t := TrimU(Text);
  if PosExChar('<', t) <> 0 then
  begin
    // 'Name <a@b.de>' or '"Last, First" <a@b.de>' - the last '<' wins
    Split(SplitRight(t, '<', @n), '>', Addr.Address);
    Addr.Address := TrimU(Addr.Address);
    n := TrimU(n);
    if (length(n) >= 2) and (n[1] = '"') and (n[length(n)] = '"') then
    begin
      // unquote "display name" with \" and \\ escapes
      n := copy(n, 2, length(n) - 2);
      i := PosExChar('\', n);
      while i > 0 do
      begin
        delete(n, i, 1);
        i := PosEx('\', n, i + 1); // skip the escaped char
      end;
    end;
    Addr.Name := n;
  end
  else
  begin
    Addr.Address := t;
    Addr.Name := '';
  end;
  result := (PosExChar('@', Addr.Address) > 1) and
            not HasAnyChar(Addr.Address, ADDR_INVALID);
end;

function MailAddressListAdd(var List: TMailAddressDynArray;
  const Text: RawUtf8): integer;
var
  i, start, n: PtrInt;
  inquote, inangle: boolean;
  a: TMailAddress;

  procedure AddItem(const Item: RawUtf8);
  begin
    if TrimU(Item) = '' then
      exit;
    if not MailAddressParse(Item, a) then
      raise EMailMessage.CreateUtf8('Invalid e-mail address [%]', [Item]);
    n := length(List);
    SetLength(List, n + 1);
    List[n] := a;
    inc(result);
  end;

begin
  result := 0;
  inquote := false;
  inangle := false;
  start := 1;
  i := 1;
  while i <= length(Text) do
  begin
    case Text[i] of
      '\':
        if inquote then
          inc(i); // skip escaped char
      '"':
        if not inangle then
          inquote := not inquote;
      '<':
        if not inquote then
          inangle := true;
      '>':
        if not inquote then
          inangle := false;
      ',', ';':
        if not inquote and not inangle then
        begin
          AddItem(copy(Text, start, i - start));
          start := i + 1;
        end;
    end;
    inc(i);
  end;
  AddItem(copy(Text, start, length(Text) - start + 1));
end;

function MailHeaderEncode(const Text: RawUtf8): RawUtf8;
const
  MAXBYTES = 45; // 45 bytes = 60 base64 chars -> 72 chars per encoded-word
var
  s: RawUtf8;
  P: PAnsiChar;
  n, len: PtrInt;
  W: TRawByteStringBuffer;
begin
  s := HeaderSafe(Text);
  if IsAnsiCompatible(s) then
  begin
    result := s;
    exit;
  end;
  W.Reset;
  P := pointer(s);
  n := length(s);
  repeat
    // never cut within a UTF-8 multi-byte sequence
    len := Utf8TruncatedLength(P, n, MAXBYTES);
    W.Append(['=?UTF-8?B?', BinToBase64(P, len), '?=']);
    inc(P, len);
    dec(n, len);
    if n <= 0 then
      break;
    W.Append(#13#10' '); // folded header line
  until false;
  W.AsText(result);
end;

function QuoteName(const Name: RawUtf8): RawUtf8;
var
  i: PtrInt;
begin
  // RFC 5322 quoted-string: only \ and " need a backslash escape
  // (mORMot QuotedStr() doubles the quote, which is SQL - not RFC 5322)
  result := '"';
  for i := 1 to length(Name) do
  begin
    if Name[i] in ['"', '\'] then
      Append(result, '\');
    Append(result, Name[i]);
  end;
  Append(result, '"');
end;

function MailAddressEncode(const Addr: TMailAddress): RawUtf8;
var
  n: RawUtf8;
begin
  n := HeaderSafe(Addr.Name);
  if n = '' then
    result := HeaderSafe(Addr.Address)
  else
  begin
    if not IsAnsiCompatible(n) then
      n := MailHeaderEncode(n)
    else if HasAnyChar(n, NAME_SPECIALS) then
      n := QuoteName(n);
    result := Join([n, ' <', HeaderSafe(Addr.Address), '>']);
  end;
end;

function QuotedPrintableEncode(const Data: RawByteString): RawUtf8;
var
  P: PByteArray;
  D: PUtf8Char;
  i, n, col, len: PtrInt;
  c: byte;
  enc: boolean;
begin
  result := '';
  n := length(Data);
  if n = 0 then
    exit;
  P := pointer(Data);
  D := FastSetString(result, n * 4 + 16); // worst case: 3 bytes + soft breaks
  col := 0;
  i := 0;
  while i < n do
  begin
    c := P[i];
    if (c = 13) or (c = 10) then
    begin
      // hard line break: normalize CRLF, LF or CR into CRLF
      if (c = 13) and (i + 1 < n) and (P[i + 1] = 10) then
        inc(i);
      PWord(D)^ := EOLW;
      inc(D, 2);
      col := 0;
      inc(i);
      continue;
    end;
    if (c = 32) or (c = 9) then
      // trailing whitespace before a line break must be encoded
      enc := (i + 1 >= n) or (P[i + 1] = 13) or (P[i + 1] = 10)
    else
      enc := (c < 32) or (c > 126) or (c = ord('='));
    if enc then
      len := 3
    else
      len := 1;
    if col + len > 75 then
    begin
      // soft line break
      D^ := '=';
      PWord(D + 1)^ := EOLW;
      inc(D, 3);
      col := 0;
    end;
    if (col = 0) and (c = ord('.')) then
    begin
      enc := true; // avoid any SMTP dot confusion with naive relays
      len := 3;
    end;
    if enc then
    begin
      D^ := '=';
      ByteToHex(PAnsiChar(D) + 1, c); // uppercase hexa, as required by RFC 2045
    end
    else
      D^ := AnsiChar(c);
    inc(D, len);
    inc(col, len);
    inc(i);
  end;
  FakeLength(result, D);
end;

function MailDateTime(UtcDateTime: TDateTime): RawUtf8;
begin
  if UtcDateTime = 0 then
    UtcDateTime := NowUtc;
  result := DateTimeToHttpDate(UtcDateTime, '+0000');
end;

function MailRandomHex(Bytes: integer): RawUtf8;
var
  tmp: array[0..63] of byte;
begin
  if Bytes > SizeOf(tmp) then
    Bytes := SizeOf(tmp);
  RandomBytes(@tmp, Bytes);
  result := BinToHexLower(@tmp, Bytes);
end;

function MailContentType(const FileName: TFileName;
  const Content: RawByteString): RawUtf8;
const
  OFFICE: array[0..15, 0..1] of RawUtf8 = (
    ('docx', 'application/vnd.openxmlformats-officedocument.wordprocessingml.document'),
    ('xlsx', 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'),
    ('pptx', 'application/vnd.openxmlformats-officedocument.presentationml.presentation'),
    ('odt',  'application/vnd.oasis.opendocument.text'),
    ('ods',  'application/vnd.oasis.opendocument.spreadsheet'),
    ('odp',  'application/vnd.oasis.opendocument.presentation'),
    ('doc',  'application/msword'),
    ('xls',  'application/vnd.ms-excel'),
    ('ppt',  'application/vnd.ms-powerpoint'),
    ('rtf',  'application/rtf'),
    ('zip',  'application/zip'),
    ('eml',  'message/rfc822'),
    ('msg',  'application/vnd.ms-outlook'),
    ('vcf',  'text/vcard'),
    ('ics',  'text/calendar'),
    ('xml',  'application/xml'));
var
  ext: RawUtf8;
  i: PtrInt;
  m: TMimeType;
begin
  m := GetMimeContentTypeFromExt(FileName, @ext);
  if ext <> '' then
  begin
    // check first: mORMot maps e.g. .docx to 'DOC' = application/msword
    for i := low(OFFICE) to high(OFFICE) do
      if PropNameEquals(OFFICE[i, 0], ext) then
      begin
        result := OFFICE[i, 1];
        exit;
      end;
    if m <> mtUnknown then
    begin
      result := MIME_TYPE[m];
      exit;
    end;
  end;
  result := 'application/octet-stream';
  if Content <> '' then
    GetMimeContentTypeFromBuffer(Content, result);
end;

// RFC 5987 / RFC 2231 extended parameter value, e.g. UTF-8''%C3%A4.pdf
function Rfc2231Encode(const Value: RawUtf8): RawUtf8;
begin
  // UrlEncodeName() keeps only RFC 3986 unreserved chars, and space as %20
  result := 'UTF-8''''' + UrlEncodeName(Value);
end;

function SafeFileName(const Name: RawUtf8): RawUtf8;
begin
  result := StringReplaceChars(StringReplaceChars(
    HeaderSafe(Name), '"', '_'), '\', '_');
end;


{ ************ TMailMessage MIME Message Builder }

const
  PREAMBLE = 'This is a multi-part message in MIME format.'#13#10;

{ TMailMessage }

constructor TMailMessage.Create;
begin
  inherited Create;
  Clear;
end;

procedure TMailMessage.Clear;
begin
  Finalize(fFrom);
  Finalize(fSender);
  fReplyTo := nil;
  fTo := nil;
  fCc := nil;
  fBcc := nil;
  fSubject := '';
  fPlainText := '';
  fHtml := '';
  fAttachments := nil;
  fHeaders := '';
  fMessageId := '';
  fDate := 0;
  fPriority := mpNormal;
  fTextEncoding := meQuotedPrintable;
  fReadReceipt := false;
end;

function TMailMessage.GetSubject: string;
begin
  result := Utf8ToString(fSubject);
end;

procedure TMailMessage.SetSubject(const Value: string);
begin
  fSubject := StringToUtf8(Value);
end;

function TMailMessage.GetPlainText: string;
begin
  result := Utf8ToString(fPlainText);
end;

procedure TMailMessage.SetPlainText(const Value: string);
begin
  fPlainText := StringToUtf8(Value);
end;

function TMailMessage.GetHtml: string;
begin
  result := Utf8ToString(fHtml);
end;

procedure TMailMessage.SetHtml(const Value: string);
begin
  fHtml := StringToUtf8(Value);
end;

procedure TMailMessage.SetFrom(const Address: RawUtf8; const Name: string);
begin
  if Name = '' then
  begin
    if not MailAddressParse(Address, fFrom) then
      raise EMailMessage.CreateUtf8('Invalid From address [%]', [Address]);
  end
  else
    fFrom := MailAddress(Address, StringToUtf8(Name));
end;

procedure TMailMessage.SetSender(const Address: RawUtf8; const Name: string);
begin
  if Name = '' then
  begin
    if not MailAddressParse(Address, fSender) then
      raise EMailMessage.CreateUtf8('Invalid Sender address [%]', [Address]);
  end
  else
    fSender := MailAddress(Address, StringToUtf8(Name));
end;

procedure TMailMessage.AddAddress(var List: TMailAddressDynArray;
  const Address: RawUtf8; const Name: string);
var
  n: PtrInt;
begin
  if Name = '' then
    MailAddressListAdd(List, Address) // may be a 'a@b, c@d' list
  else
  begin
    n := length(List);
    SetLength(List, n + 1);
    List[n] := MailAddress(Address, StringToUtf8(Name));
  end;
end;

procedure TMailMessage.AddTo(const Address: RawUtf8; const Name: string);
begin
  AddAddress(fTo, Address, Name);
end;

procedure TMailMessage.AddCc(const Address: RawUtf8; const Name: string);
begin
  AddAddress(fCc, Address, Name);
end;

procedure TMailMessage.AddBcc(const Address: RawUtf8; const Name: string);
begin
  AddAddress(fBcc, Address, Name);
end;

procedure TMailMessage.AddReplyTo(const Address: RawUtf8; const Name: string);
begin
  AddAddress(fReplyTo, Address, Name);
end;

function TMailMessage.DoAddAttachment(const Content: RawByteString;
  const FileName: RawUtf8; const ContentType, ContentId: RawUtf8): PtrInt;
begin
  result := length(fAttachments);
  SetLength(fAttachments, result + 1);
  fAttachments[result].FileName := SafeFileName(FileName);
  if ContentType = '' then
    fAttachments[result].ContentType :=
      MailContentType(Utf8ToString(FileName), Content)
  else
    fAttachments[result].ContentType := HeaderSafe(ContentType);
  fAttachments[result].ContentId := ContentId;
  fAttachments[result].Content := Content;
end;

function TMailMessage.AddAttachment(const FileName: TFileName;
  const ContentType: RawUtf8; const DisplayName: string): PtrInt;
var
  content: RawByteString;
  name: string;
begin
  if not FileExists(FileName) then
    raise EMailMessage.CreateUtf8('AddAttachment: file not found [%]', [FileName]);
  content := StringFromFile(FileName);
  if DisplayName <> '' then
    name := DisplayName
  else
    name := ExtractFileName(FileName);
  result := DoAddAttachment(content, StringToUtf8(name), ContentType, '');
end;

function TMailMessage.AddAttachmentData(const Content: RawByteString;
  const FileName: string; const ContentType: RawUtf8): PtrInt;
begin
  result := DoAddAttachment(Content, StringToUtf8(FileName), ContentType, '');
end;

function TMailMessage.AddInlineData(const Content: RawByteString;
  const FileName: string; const ContentId, ContentType: RawUtf8): RawUtf8;
begin
  result := ContentId;
  if result = '' then
    result := Join([MailRandomHex(12), '@mormot']);
  DoAddAttachment(Content, StringToUtf8(FileName), ContentType, result);
end;

function TMailMessage.AddInlineImage(const FileName: TFileName;
  const ContentId: RawUtf8): RawUtf8;
begin
  if not FileExists(FileName) then
    raise EMailMessage.CreateUtf8('AddInlineImage: file not found [%]', [FileName]);
  result := AddInlineData(StringFromFile(FileName),
    ExtractFileName(FileName), ContentId, '');
end;

procedure TMailMessage.AddHeader(const Name, Value: RawUtf8);
begin
  Append(fHeaders, [HeaderSafe(Name), ': ', MailHeaderEncode(Value), #13#10]);
end;

function TMailMessage.HasInline: boolean;
var
  i: PtrInt;
begin
  result := true;
  for i := 0 to length(fAttachments) - 1 do
    if fAttachments[i].ContentId <> '' then
      exit;
  result := false;
end;

function TMailMessage.HasAttached: boolean;
var
  i: PtrInt;
begin
  result := true;
  for i := 0 to length(fAttachments) - 1 do
    if fAttachments[i].ContentId = '' then
      exit;
  result := false;
end;

function TMailMessage.Envelope: TRawUtf8DynArray;
var
  n: integer;

  procedure AddList(const List: TMailAddressDynArray);
  var
    i: PtrInt;
  begin
    for i := 0 to length(List) - 1 do
      if FindPropName(pointer(result), List[i].Address, n) < 0 then
        AddRawUtf8(result, n, List[i].Address); // no duplicated RCPT TO
  end;

begin
  result := nil;
  n := 0;
  AddList(fTo);
  AddList(fCc);
  AddList(fBcc);
  SetLength(result, n);
end;

function TMailMessage.EnvelopeFrom: RawUtf8;
begin
  if fSender.Address <> '' then
    result := fSender.Address
  else
    result := fFrom.Address;
end;

function TMailMessage.Build: RawByteString;
var
  W: TRawByteStringBuffer;
  domain: RawUtf8;

  procedure Add(const Text: array of RawUtf8);
  begin
    W.Append(Text);
  end;

  procedure AddLine(const Text: array of RawUtf8);
  begin
    W.Append(Text);
    W.AppendCRLF;
  end;

  procedure AddAddresses(const Header: RawUtf8; const List: TMailAddressDynArray);
  var
    j: PtrInt;
  begin
    if List = nil then
      exit;
    Add([Header, ': ']);
    for j := 0 to length(List) - 1 do
    begin
      if j > 0 then
        Add([','#13#10' ']); // folded header line
      Add([MailAddressEncode(List[j])]);
    end;
    W.AppendCRLF;
  end;

  function NewBoundary: RawUtf8;
  begin
    result := Join(['=_mORMot_', MailRandomHex(12)]);
  end;

  procedure WriteText(const MimeType, Text: RawUtf8);
  begin
    AddLine(['Content-Type: ', MimeType, '; charset="utf-8"']);
    if fTextEncoding = meBase64 then
    begin
      AddLine(['Content-Transfer-Encoding: base64']);
      W.AppendCRLF;
      if Text <> '' then
        W.Append(BinToBase64Line(pointer(Text), length(Text)));
    end
    else
    begin
      AddLine(['Content-Transfer-Encoding: quoted-printable']);
      W.AppendCRLF;
      W.Append(QuotedPrintableEncode(Text));
      W.AppendCRLF;
    end;
  end;

  procedure WriteAttachment(const Att: TMailAttachment);
  var
    ascii: boolean;
  begin
    ascii := IsAnsiCompatible(Att.FileName);
    Add(['Content-Type: ', Att.ContentType]);
    if Att.FileName <> '' then
      if ascii then
        Add([';'#13#10#9'name="', Att.FileName, '"'])
      else
        Add([';'#13#10#9'name="', MailHeaderEncode(Att.FileName), '"']);
    W.AppendCRLF;
    AddLine(['Content-Transfer-Encoding: base64']);
    if Att.ContentId <> '' then
    begin
      AddLine(['Content-ID: <', HeaderSafe(Att.ContentId), '>']);
      Add(['Content-Disposition: inline']);
    end
    else
      Add(['Content-Disposition: attachment']);
    if Att.FileName <> '' then
      if ascii then
        Add([';'#13#10#9'filename="', Att.FileName, '"'])
      else
        Add([';'#13#10#9'filename*=', Rfc2231Encode(Att.FileName)]);
    W.AppendCRLF;
    W.AppendCRLF;
    if Att.Content <> '' then
      W.Append(BinToBase64Line(pointer(Att.Content), length(Att.Content)));
  end;

  procedure WriteAlternative;
  var
    b: RawUtf8;
  begin
    if (fPlainText <> '') and (fHtml <> '') then
    begin
      b := NewBoundary;
      AddLine(['Content-Type: multipart/alternative;']);
      AddLine([#9'boundary="', b, '"']);
      W.AppendCRLF;
      AddLine(['--', b]);
      WriteText('text/plain', fPlainText);
      AddLine(['--', b]);
      WriteText('text/html', fHtml);
      AddLine(['--', b, '--']);
    end
    else if fHtml <> '' then
      WriteText('text/html', fHtml)
    else
      WriteText('text/plain', fPlainText);
  end;

  procedure WriteRelated;
  var
    b: RawUtf8;
    j: PtrInt;
  begin
    if not HasInline then
    begin
      WriteAlternative;
      exit;
    end;
    b := NewBoundary;
    AddLine(['Content-Type: multipart/related;']);
    if fPlainText <> '' then
      AddLine([#9'type="multipart/alternative";'])
    else
      AddLine([#9'type="text/html";']);
    AddLine([#9'boundary="', b, '"']);
    W.AppendCRLF;
    AddLine(['--', b]);
    WriteAlternative;
    for j := 0 to length(fAttachments) - 1 do
      if fAttachments[j].ContentId <> '' then
      begin
        AddLine(['--', b]);
        WriteAttachment(fAttachments[j]);
      end;
    AddLine(['--', b, '--']);
  end;

  procedure WriteMixed;
  var
    b: RawUtf8;
    j: PtrInt;
  begin
    if not HasAttached then
    begin
      WriteRelated;
      exit;
    end;
    b := NewBoundary;
    AddLine(['Content-Type: multipart/mixed;']);
    AddLine([#9'boundary="', b, '"']);
    W.AppendCRLF;
    W.Append(PREAMBLE);
    AddLine(['--', b]);
    WriteRelated;
    for j := 0 to length(fAttachments) - 1 do
      if fAttachments[j].ContentId = '' then
      begin
        AddLine(['--', b]);
        WriteAttachment(fAttachments[j]);
      end;
    AddLine(['--', b, '--']);
  end;

var
  res: RawUtf8;
begin
  if fFrom.Address = '' then
    raise EMailMessage.Create('TMailMessage.Build: missing From address');
  if (fHtml = '') and HasInline then
    raise EMailMessage.Create('TMailMessage.Build: inline images require Html');
  W.Reset;
  // main RFC 5322 headers
  AddLine(['Date: ', MailDateTime(fDate)]);
  AddLine(['From: ', MailAddressEncode(fFrom)]);
  if fSender.Address <> '' then
    AddLine(['Sender: ', MailAddressEncode(fSender)]);
  AddAddresses('Reply-To', fReplyTo);
  AddAddresses('To', fTo);
  AddAddresses('Cc', fCc);
  // Bcc is never written: recipients only appear in the SMTP envelope
  AddLine(['Subject: ', MailHeaderEncode(fSubject)]);
  if fMessageId = '' then
  begin
    domain := SplitRight(fFrom.Address, '@');
    if domain = fFrom.Address then
      domain := 'localhost';
    fMessageId := Join([MailRandomHex(16), '@', domain]);
  end;
  AddLine(['Message-ID: <', HeaderSafe(fMessageId), '>']);
  case fPriority of
    mpHighest:
      begin
        AddLine(['X-Priority: 1 (Highest)']);
        AddLine(['Importance: high']);
      end;
    mpHigh:
      begin
        AddLine(['X-Priority: 2 (High)']);
        AddLine(['Importance: high']);
      end;
    mpLow:
      begin
        AddLine(['X-Priority: 4 (Low)']);
        AddLine(['Importance: low']);
      end;
    mpLowest:
      begin
        AddLine(['X-Priority: 5 (Lowest)']);
        AddLine(['Importance: low']);
      end;
  end;
  if fReadReceipt then
    AddLine(['Disposition-Notification-To: ', MailAddressEncode(fFrom)]);
  W.Append(fHeaders);
  AddLine(['MIME-Version: 1.0']);
  // MIME body tree: mixed > related > alternative > text/plain + text/html
  WriteMixed;
  W.AsText(res);
  result := res;
end;

procedure TMailMessage.SaveToFile(const FileName: TFileName);
begin
  if not FileFromString(Build, FileName) then
    raise EMailMessage.CreateUtf8('SaveToFile: cannot write [%]', [FileName]);
end;


{ ************ TSmtpClient SMTP/ESMTP Client }

function SmtpDotStuff(const Data: RawByteString): RawByteString;
var
  P, D: PAnsiChar;
  i, n: PtrInt;
  bol: boolean;
begin
  n := length(Data);
  P := pointer(Data);
  D := FastNewRawByteString(result, n * 2 + 5); // worst case: all '.' or bare LF
  bol := true; // at beginning of line
  i := 0;
  while i < n do
  begin
    case P[i] of
      #13, #10:
        begin
          if (P[i] = #13) and (i + 1 < n) and (P[i + 1] = #10) then
            inc(i);
          PWord(D)^ := EOLW;
          inc(D, 2);
          bol := true;
        end;
      '.':
        begin
          if bol then
          begin
            D^ := '.'; // RFC 5321 4.5.2 transparency
            inc(D);
          end;
          D^ := '.';
          inc(D);
          bol := false;
        end;
    else
      begin
        D^ := P[i];
        inc(D);
        bol := false;
      end;
    end;
    inc(i);
  end;
  if not bol then
  begin
    PWord(D)^ := EOLW;
    inc(D, 2);
  end;
  D^ := '.';
  PWord(D + 1)^ := EOLW;
  inc(D, 3);
  FakeLength(result, D - pointer(result));
end;

{ TSmtpClient }

constructor TSmtpClient.Create(const aHost, aPort, aUserName, aPassword: RawUtf8);
begin
  inherited Create;
  fHost := aHost;
  fPort := aPort;
  fUserName := aUserName;
  fPassword := aPassword;
  fTimeOut := 30000;
  fHeloName := Executable.Host;
  if fHeloName = '' then
    fHeloName := 'localhost';
end;

destructor TSmtpClient.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TSmtpClient.Log(const Line: RawUtf8; Outgoing: boolean);
begin
  if Assigned(fOnLog) then
    fOnLog(self, Line, Outgoing);
end;

function TSmtpClient.Connected: boolean;
begin
  result := fSock <> nil;
end;

function TSmtpClient.ReadResponse: integer;
var
  line: RawUtf8;
begin
  fLastResponse := '';
  repeat
    fSock.SockRecvLn(line);
    Log(line, false);
    if fLastResponse = '' then
      fLastResponse := line
    else
      Append(fLastResponse, #13#10, line);
  until (length(line) < 4) or (line[4] <> '-'); // '250-' = more lines follow
  fLastCode := GetCardinal(pointer(line)); // parse the leading '250' digits
  if (fLastCode < 100) or (fLastCode > 599) then
    fLastCode := 0;
  result := fLastCode;
end;

procedure TSmtpClient.CheckCode(const Expected: array of integer;
  const Context: RawUtf8);
var
  i: PtrInt;
begin
  for i := 0 to high(Expected) do
    if fLastCode = Expected[i] then
      exit;
  raise ESmtpClient.CreateUtf8('SMTP % failed at %:% - [%]',
    [Context, fHost, fPort, fLastResponse]);
end;

function TSmtpClient.Exec(const Command, LogAs: RawUtf8): integer;
begin
  if fSock = nil then
    raise ESmtpClient.Create('SMTP: not connected');
  if LogAs <> '' then
    Log(LogAs, true)
  else
    Log(Command, true);
  fSock.SockSend(Command); // append CRLF
  fSock.SockSendFlush;
  result := ReadResponse;
end;

function TSmtpClient.Cmd(const Command: RawUtf8; const Expected: array of integer;
  const LogAs: RawUtf8): integer;
begin
  result := Exec(Command, LogAs);
  if LogAs <> '' then
    CheckCode(Expected, LogAs)
  else
    CheckCode(Expected, Command);
end;

// search the next EHLO response line matching Up (e.g. 'AUTH' or 'STARTTLS')
// and return its parameters (e.g. 'PLAIN LOGIN')
function NextCapability(var P: PUtf8Char; const Up: RawUtf8;
  out Params: RawUtf8): boolean;
var
  line: RawUtf8;
  L: PtrInt;
begin
  result := true;
  L := length(Up);
  while P <> nil do
  begin
    line := GetNextLine(P, P);
    // each line is like '250-STARTTLS' or '250 AUTH PLAIN LOGIN'
    if (length(line) >= L + 4) and
       IdemPChar(PUtf8Char(pointer(line)) + 4, pointer(Up)) and
       (PUtf8Char(pointer(line))[L + 4] in [#0, ' ', '=']) then
    begin
      Params := copy(line, L + 6, maxInt);
      exit;
    end;
  end;
  result := false;
end;

function TSmtpClient.HasCapability(const Name: RawUtf8): boolean;
var
  P: PUtf8Char;
  up, params: RawUtf8;
begin
  UpperCaseCopy(Name, up); // IdemPChar() expects an uppercase pattern
  P := pointer(fCapabilities);
  result := NextCapability(P, up, params);
end;

function TSmtpClient.HasAuth(const Mechanism: RawUtf8): boolean;
var
  P: PUtf8Char;
  params: RawUtf8;
begin
  result := true;
  P := pointer(fCapabilities);
  // old servers may send both 'AUTH=LOGIN' and 'AUTH PLAIN LOGIN' lines
  while NextCapability(P, 'AUTH', params) do
    if FindCsvIndex(pointer(params), Mechanism, ' ', {casesensitive=}false) >= 0 then
      exit;
  result := false;
end;

procedure TSmtpClient.DoEhlo;
begin
  if Exec('EHLO ' + fHeloName) = 250 then
    fCapabilities := fLastResponse
  else
  begin
    // very old server: fallback to plain SMTP
    Cmd('HELO ' + fHeloName, [250]);
    fCapabilities := '';
  end;
end;

procedure TSmtpClient.DoAuth;
var
  mech: TSmtpAuth;
begin
  mech := fAuth;
  if mech = sauAuto then
    if fOAuth2Token <> '' then
      mech := sauXOAuth2
    else if fUserName = '' then
      mech := sauNone
    else if HasAuth('PLAIN') then
      mech := sauPlain
    else if HasAuth('LOGIN') then
      mech := sauLogin
    else
      mech := sauPlain;
  case mech of
    sauPlain:
      Cmd('AUTH PLAIN ' + BinToBase64(Join([#0, fUserName, #0, fPassword])),
        [235], 'AUTH PLAIN ****');
    sauLogin:
      begin
        Cmd('AUTH LOGIN', [334]);
        Cmd(BinToBase64(fUserName), [334], 'AUTH LOGIN username');
        Cmd(BinToBase64(fPassword), [235], 'AUTH LOGIN ****');
      end;
    sauXOAuth2:
      if Exec('AUTH XOAUTH2 ' + BinToBase64(Join(['user=', fUserName,
           #1'auth=Bearer ', fOAuth2Token, #1#1])), 'AUTH XOAUTH2 ****') = 334 then
      begin
        // 334 + base64 JSON error: send an empty line to get the final 535
        Exec('');
        CheckCode([235], 'AUTH XOAUTH2');
      end
      else
        CheckCode([235], 'AUTH XOAUTH2');
  end;
end;

procedure TSmtpClient.Connect;
var
  sec: TSmtpSecurity;
begin
  if fSock <> nil then
    exit;
  if fHost = '' then
    raise ESmtpClient.Create('SMTP: no Host specified');
  sec := fSecurity;
  if (sec = smtpAuto) and (fPort = '465') then
    sec := smtpImplicitTls;
  fTlsActive := sec = smtpImplicitTls;
  fCapabilities := '';
  fSock := TCrtSocket.Open(fHost, fPort, nlTcp, fTimeOut, fTlsActive, @Tls);
  try
    fSock.CreateSockIn; // buffered SockRecvLn()
    ReadResponse;
    CheckCode([220], 'greeting');
    DoEhlo;
    if (sec = smtpStartTls) or
       ((sec = smtpAuto) and HasCapability('STARTTLS')) then
    begin
      if not HasCapability('STARTTLS') then
        raise ESmtpClient.CreateUtf8('SMTP server %:% does not support STARTTLS',
          [fHost, fPort]);
      Cmd('STARTTLS', [220]);
      fSock.TLS := Tls; // apply the caller TLS options before the handshake
      fSock.DoTlsAfter(cstaConnect);
      fTlsActive := true;
      DoEhlo; // capabilities may change once encrypted (e.g. AUTH)
    end;
    DoAuth;
  except
    FreeAndNil(fSock);
    fTlsActive := false;
    raise;
  end;
end;

procedure TSmtpClient.Disconnect;
begin
  if fSock = nil then
    exit;
  try
    Exec('QUIT'); // best effort
  except
    // some servers just close the connection
  end;
  FreeAndNil(fSock);
  fTlsActive := false;
end;

procedure TSmtpClient.SendRaw(const MailFrom: RawUtf8;
  const Recipients: array of RawUtf8; const Data: RawByteString);
var
  i: PtrInt;
  stuffed: RawByteString;
begin
  if length(Recipients) = 0 then
    raise ESmtpClient.Create('SMTP: no recipient');
  if fSock = nil then
    Connect;
  try
    Cmd(Join(['MAIL FROM:<', HeaderSafe(MailFrom), '>']), [250]);
    for i := 0 to high(Recipients) do
      Cmd(Join(['RCPT TO:<', HeaderSafe(Recipients[i]), '>']), [250, 251]);
    Cmd('DATA', [354]);
    stuffed := SmtpDotStuff(Data);
    Log(FormatUtf8('<% bytes of message data>', [length(stuffed)]), true);
    fSock.SockSendFlush(stuffed);
    ReadResponse;
    CheckCode([250], 'DATA');
  except
    on Exception do
    begin
      if fSock <> nil then
        try
          Exec('RSET'); // reset the transaction for the next message
        except
          FreeAndNil(fSock); // connection is broken
          fTlsActive := false;
        end;
      raise;
    end;
  end;
end;

procedure TSmtpClient.Send(Msg: TMailMessage);
var
  data: RawByteString;
begin
  if Msg = nil then
    raise ESmtpClient.Create('SMTP: Send(nil)');
  data := Msg.Build;
  SendRaw(Msg.EnvelopeFrom, Msg.Envelope, data);
end;

function SendMailMessage(Msg: TMailMessage; const Host, Port, UserName,
  Password: RawUtf8; Security: TSmtpSecurity;
  IgnoreCertificateErrors: boolean): RawUtf8;
var
  smtp: TSmtpClient;
begin
  smtp := TSmtpClient.Create(Host, Port, UserName, Password);
  try
    smtp.Security := Security;
    smtp.Tls.IgnoreCertificateErrors := IgnoreCertificateErrors;
    smtp.Send(Msg);
    result := smtp.LastResponse;
    smtp.Disconnect;
  finally
    smtp.Free;
  end;
end;


{ ************ Send Email using the SMTP Protocol }

function TSmtpConnection.FromText(const aText: RawUtf8): boolean;
var
  u, h: RawUtf8;
begin
  if aText = SMTP_DEFAULT then
  begin
    result := false;
    exit;
  end;
  h := SplitRight(aText, '@', @u);
  if (u <> '') and
     not Split(u, ':', User, Pass) then
    User := u;
  if not Split(h, ':', Host, Port) then
  begin
    Host := h;
    Port := '25';
  end;
  if (Host <> '') and
     (Host[1] = '?') then
    Host := '';
  result := Host <> '';
end;

function SendEmail(const Server: TSmtpConnection;
  const From, CsvDest, Subject: RawUtf8; const Text: RawByteString;
  const Headers, TextCharSet: RawUtf8; Tls: TSmtpTls; TlsCtx: PNetTlsContext): boolean;
begin
  if Tls = stlsNone then // guess the TLS mode from the well-known port numbers
    if Server.Port = '465' then
      Tls := stlsImplicit
    else if Server.Port = '587' then
      Tls := stlsStartTls;
  result := SendEmail(
    Server.Host, From, CsvDest, Subject, Text, Headers,
    Server.User, Server.Pass, Server.Port, TextCharSet, Tls, TlsCtx);
end;

const
  SMTP_SECURITY: array[TSmtpTls] of TSmtpSecurity = (
    smtpNone,         // stlsNone
    smtpImplicitTls,  // stlsImplicit
    smtpStartTls);    // stlsStartTls

// return the plain address of 'a@b.de', '<a@b.de>' or 'Name <a@b.de>'
function EnvelopeAddress(const Text: RawUtf8): RawUtf8;
var
  a: TMailAddress;
begin
  if MailAddressParse(Text, a) then
    result := a.Address
  else
    result := Text; // let the SMTP server reject it
end;

function SendEmail(const Server, From, CsvDest, Subject: RawUtf8;
  const Text: RawByteString; const Headers, User, Pass, Port, TextCharSet: RawUtf8;
  Tls: TSmtpTls; TlsCtx: PNetTlsContext): boolean;
var
  smtp: TSmtpClient;
  P: PUtf8Char;
  n: integer;
  rec, tolist, head, subj, frm: RawUtf8;
  rcpt: TRawUtf8DynArray;
  W: TRawByteStringBuffer;
  data: RawUtf8;
begin
  result := false;
  // parse the CSV recipients for both the envelope and the To: header
  n := 0;
  tolist := '';
  P := pointer(CsvDest);
  while P <> nil do
  begin
    GetNextItem(P, ',', rec);
    TrimSelf(rec);
    if rec = '' then
      continue;
    AddRawUtf8(rcpt, n, EnvelopeAddress(rec));
    if PosExChar('<', rec) = 0 then
      rec := Join(['<', rec, '>']);
    if tolist = '' then
      tolist := rec
    else
      Append(tolist, ', ', rec);
  end;
  if n = 0 then
    exit;
  SetLength(rcpt, n);
  // merge the From/Subject parameters with any matching lines in Headers:
  // a non-empty parameter wins (its header duplicate is removed), an empty
  // parameter is filled from the corresponding header value
  head := TrimU(Headers);
  subj := Subject;
  frm := From;
  if subj = '' then
    GetHeader(head, 'Subject', subj);
  if frm = '' then
    GetHeader(head, 'From', frm);
  head := DeleteHeader(head, 'Subject');
  head := DeleteHeader(head, 'From');
  // compute the raw message: SendRaw() will normalize CRLF and dot-stuff it
  W.Reset;
  W.Append(['Subject: ', subj, #13#10'From: ', frm, #13#10'To: ', tolist]);
  W.AppendCRLF;
  if (TextCharSet <> '') or
     (head = '') then
    W.Append(['Content-Type: text/plain;charset=', TextCharSet,
      #13#10'Content-Transfer-Encoding: 8bit'#13#10]);
  if head <> '' then
  begin
    W.Append(head);
    W.AppendCRLF;
  end;
  W.AppendCRLF; // end of headers
  W.Append(pointer(Text), length(Text));
  W.AsText(data);
  // send it using a TSmtpClient instance
  smtp := TSmtpClient.Create(Server, Port);
  try
    if (User <> '') and
       (Pass <> '') then
    begin
      smtp.UserName := User; // sauAuto will select PLAIN or LOGIN
      smtp.Password := Pass;
    end;
    smtp.Security := SMTP_SECURITY[Tls];
    smtp.TimeOut := 10000; // as the former SocketOpen() call
    if TlsCtx <> nil then
      smtp.Tls := TlsCtx^;
    try
      smtp.Connect;
    except
      on ESmtpClient do
        raise; // SMTP protocol error
      on Exception do
        exit;  // server not reachable: return false, as previously
    end;
    smtp.SendRaw(EnvelopeAddress(frm), rcpt, data);
    result := true; // the message is accepted once the final '.' returns 250
  finally
    smtp.Free; // send QUIT (best effort)
  end;
end;

function SendEmailSubject(const Text: string): RawUtf8;
begin
  StringToUtf8(Text, result{%H-});
  result := MimeHeaderEncode(result);
end;

end.
