/// small stand-alone tool to test SMTP email sending (plain / implicit TLS / STARTTLS)
// - by default, uses the SendEmail() function of mormot.net.mail
// - with --native, uses the mormot.net.mail TMailMessage MIME builder and
// TSmtpClient: HTML body, attachments, inline images, Cc/Bcc, priority...
// - part of the Open Source Synopse mORMot framework 2 examples
program mailtest;

{$I ..\..\..\src\mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$apptype console}
  {$R ..\..\..\src\mormot.win.default.manifest.res}
{$endif OSWINDOWS}

uses
  {$I ..\..\..\src\mormot.uses.inc}
  {$ifdef OSPOSIX}
  {$ifdef FPC}
  cwstring, // needed as fallback if ICU is not available (FPC only)
  {$endif FPC}
  mormot.lib.openssl11, // OpenSSL is required for TLS on POSIX
  {$endif OSPOSIX}
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.log,
  mormot.net.sock,
  mormot.net.http,   // for MimeHeaderEncode()
  mormot.net.mail;   // SendEmail() + TMailMessage + TSmtpClient

type
  /// command line options of this tool
  TMailTestOptions = record
    Conn, From, Dest, Subject, Body, Headers: RawUtf8;
    Html, Security, OAuth2, Cc, Bcc, ReplyTo, Priority, EmlFile: RawUtf8;
    HtmlFile: string;
    Attach, InlineImg: TStringDynArray;
    IgnoreCert, Native, Base64, Receipt: boolean;
  end;

function ParseCommandLine(out Opt: TMailTestOptions): boolean;
var
  cmd: TExecutableCommandLine;
  hdr: TRawUtf8DynArray;
  i: PtrInt;
begin
  FillCharFast(Opt, SizeOf(Opt), 0);
  // use the same -x / --name switches on all platforms (default is / on Windows)
  cmd := Executable.Command;
  cmd.Parse(CRLF, '-', '--');
  cmd.ExeDescription := 'Send a test e-mail via SMTP (plain, implicit TLS or ' +
    'STARTTLS).'#13#10'By default SendEmail() from mormot.net.mail is used; ' +
    'with --native, the MIME features of mormot.net.mail are available. ' +
    'In native mode, \n in the body is converted to a line feed.';
  // main arguments
  Opt.Conn := cmd.ArgU(0, '#user:password@server:port SMTP connection', false);
  Opt.From := cmd.ArgU(1, '#from@x sender address', false);
  Opt.Dest := cmd.ArgU(2, '#to@y recipient address', false);
  Opt.Subject := cmd.ArgU(3, 'the message #subject');
  Opt.Body := cmd.ArgU(4, 'the plain text #body');
  if Opt.Subject = '' then
    Opt.Subject := 'mORMot STARTTLS test';
  if Opt.Body = '' then
    Opt.Body := 'This is a test email sent by the mORMot 2 mailtest tool.';
  // common options
  cmd.Get(['header'], hdr, 'add an individual "Name: value" #header (repeatable)');
  for i := 0 to high(hdr) do
    Opt.Headers := Opt.Headers + hdr[i] + #13#10;
  Opt.IgnoreCert := cmd.Option('ignorecert',
    'disable the server certificate validation');
  // native options (mormot.net.mail)
  Opt.Native := cmd.Option('native',
    'use TMailMessage + TSmtpClient instead of SendEmail()');
  cmd.Get('html', Opt.Html, 'the HTML #body (the text body is then the alternative)');
  cmd.Get('htmlfile', Opt.HtmlFile, 'read the HTML body from a UTF-8 #file');
  cmd.Get('attach', Opt.Attach, 'attach a #file (repeatable)');
  cmd.Get('inline', Opt.InlineImg,
    'inline image #file appended as <img src="cid:.."> (repeatable)');
  cmd.Get('cc', Opt.Cc, '#addresses in Cc, separated by , or ;');
  cmd.Get('bcc', Opt.Bcc, '#addresses in Bcc (SMTP envelope only)');
  cmd.Get('replyto', Opt.ReplyTo, 'the Reply-To #address');
  cmd.Get('priority', Opt.Priority, 'highest, high, low or lowest', 'normal');
  Opt.Receipt := cmd.Option('receipt', 'request a read receipt');
  Opt.Base64 := cmd.Option('base64',
    'use Base64 instead of Quoted-Printable for the text parts');
  cmd.Get('security', Opt.Security,
    'auto, none, tls (implicit) or starttls', 'auto');
  cmd.Get('oauth2', Opt.OAuth2, 'XOAUTH2 access #token (Microsoft 365, Gmail)');
  cmd.Get('eml', Opt.EmlFile, 'only save the message in this .eml #file');
  // display the usage and exit on -h / --help, or on unexpected parameter
  if cmd.ConsoleHelpFailed then
  begin
    result := false;
    exit;
  end;
  result := (Opt.Conn <> '') and
            (Opt.From <> '') and
            (Opt.Dest <> '');
  if not result then
    ConsoleWrite(cmd.FullDescription);
end;

type
  /// a tiny holder to log the SMTP dialog on the console
  TSmtpConsoleLog = class
    procedure OnLog(Sender: TObject; const Line: RawUtf8; Outgoing: boolean);
  end;

procedure TSmtpConsoleLog.OnLog(Sender: TObject; const Line: RawUtf8;
  Outgoing: boolean);
begin
  if Outgoing then
    ConsoleWrite('C: %', [Line])
  else
    ConsoleWrite('S: %', [Line]);
end;

// send the message using the mormot.net.mail unit
function SendNative(const Opt: TMailTestOptions; const Smtp: TSmtpConnection;
  Tls: PNetTlsContext): boolean;
var
  msg: TMailMessage;
  client: TSmtpClient;
  logger: TSmtpConsoleLog;
  html, cid, line, name, value: RawUtf8;
  P: PUtf8Char;
  i: PtrInt;
begin
  msg := TMailMessage.Create;
  try
    msg.SetFrom(Opt.From);
    msg.AddTo(Opt.Dest);
    if Opt.Cc <> '' then
      msg.AddCc(Opt.Cc);
    if Opt.Bcc <> '' then
      msg.AddBcc(Opt.Bcc);
    if Opt.ReplyTo <> '' then
      msg.AddReplyTo(Opt.ReplyTo);
    msg.SubjectUtf8 := Opt.Subject; // any UTF-8 text, RFC 2047 encoded on the wire
    msg.PlainTextUtf8 := StringReplaceAll(Opt.Body, '\n', #13#10);
    if Opt.Base64 then
      msg.TextEncoding := meBase64;
    msg.ReadReceipt := Opt.Receipt;
    if IdemPropNameU(Opt.Priority, 'highest') then
      msg.Priority := mpHighest
    else if IdemPropNameU(Opt.Priority, 'high') then
      msg.Priority := mpHigh
    else if IdemPropNameU(Opt.Priority, 'low') then
      msg.Priority := mpLow
    else if IdemPropNameU(Opt.Priority, 'lowest') then
      msg.Priority := mpLowest;
    msg.AddHeader('X-Mailer', 'mORMot 2 mailtest');
    P := pointer(Opt.Headers); // the --header "Name: value" lines
    while P <> nil do
    begin
      line := GetNextLine(P, P);
      if Split(line, ':', name, value) then
        msg.AddHeader(TrimU(name), TrimU(value));
    end;
    html := Opt.Html;
    if Opt.HtmlFile <> '' then
      html := StringFromFile(Opt.HtmlFile);
    if (html = '') and
       (length(Opt.InlineImg) > 0) then
      html := '<p>' + msg.PlainTextUtf8 + '</p>'; // inline images need an HTML body
    for i := 0 to high(Opt.InlineImg) do
    begin
      cid := msg.AddInlineImage(Opt.InlineImg[i]);
      html := html + '<p><img src="cid:' + cid + '"></p>';
    end;
    msg.HtmlUtf8 := html;
    for i := 0 to high(Opt.Attach) do
      msg.AddAttachment(Opt.Attach[i]);
    if Opt.EmlFile <> '' then
    begin
      msg.SaveToFile(Utf8ToString(Opt.EmlFile));
      ConsoleWrite('Message saved to % (not sent)', [Opt.EmlFile]);
      result := true;
      exit;
    end;
    logger := TSmtpConsoleLog.Create;
    client := TSmtpClient.Create(Smtp.Host, Smtp.Port, Smtp.User, Smtp.Pass);
    try
      client.OnLog := logger.OnLog;
      client.OAuth2Token := Opt.OAuth2;
      if Tls <> nil then
        client.Tls := Tls^;
      if IdemPropNameU(Opt.Security, 'none') then
        client.Security := smtpNone
      else if IdemPropNameU(Opt.Security, 'tls') then
        client.Security := smtpImplicitTls
      else if IdemPropNameU(Opt.Security, 'starttls') then
        client.Security := smtpStartTls;
      // else smtpAuto: 465 = implicit TLS, else STARTTLS if advertised
      client.Send(msg);
      ConsoleWrite('Server response: %', [client.LastResponse]);
      client.Disconnect;
      result := true;
    finally
      client.Free;
      logger.Free;
    end;
  finally
    msg.Free;
  end;
end;

procedure Run;
var
  opt: TMailTestOptions;
  smtp: TSmtpConnection;
  tls: TNetTlsContext;
  tlsp: PNetTlsContext;
  ok: boolean;
begin
  if not ParseCommandLine(opt) then
  begin
    ExitCode := 1;
    exit;
  end;
  tlsp := nil;
  if opt.IgnoreCert then
  begin
    InitNetTlsContext(tls);
    tls.IgnoreCertificateErrors := true;
    tlsp := @tls;
  end;
  // ----- resolve the SMTP connection ('user:password@server:port') -----
  if not smtp.FromText(opt.Conn) then
  begin
    ConsoleWrite('Invalid connection string: %', [opt.Conn]);
    ExitCode := 1;
    exit;
  end;
  ConsoleWrite('Sending mail via %:% (user=%) ...',
    [smtp.Host, smtp.Port, smtp.User]);
  try
    if opt.Native then
      ok := SendNative(opt, smtp, tlsp)
    else
      // the TSmtpConnection overload guesses the TLS mode from the port:
      //   465 -> stlsImplicit, 587 -> stlsStartTls, else stlsNone (plain)
      // body is converted to ISO-8859-1 (the SendEmail default TextCharSet)
      ok := SendEmail(smtp, opt.From, opt.Dest, MimeHeaderEncode(opt.Subject),
        Utf8ToWinAnsi(opt.Body), opt.Headers, 'ISO-8859-1', stlsNone, tlsp);
    if ok then
      ConsoleWrite('Success: email accepted by the server.')
    else
    begin
      ConsoleWrite('Failure: server did not accept the email.');
      ExitCode := 2;
    end;
  except
    on E: Exception do
    begin
      ConsoleWrite('Error [%]: %', [ClassNameShort(E)^, StringToUtf8(E.Message)]);
      ExitCode := 3;
    end;
  end;
end;

begin
  // full verbose logging echoed to the console, to trace the SMTP/TLS dialog
  with TSynLog.Family do
  begin
    Level := LOG_VERBOSE;
    EchoToConsole := LOG_VERBOSE;
  end;
  {$ifdef OSPOSIX}
  OpenSslInitialize; // ensure the OpenSSL TLS layer is available on POSIX
  {$endif OSPOSIX}
  try
    Run;
  except
    on E: Exception do
    begin
      ConsoleShowFatalException(E);
      ExitCode := 4;
    end;
  end;
end.
