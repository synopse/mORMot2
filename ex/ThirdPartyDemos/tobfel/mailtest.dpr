/// small stand-alone tool to test SMTP email sending (plain / implicit TLS / STARTTLS)
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
  mormot.net.client;

procedure Run;
var
  smtp: TSmtpConnection;
  conn, from, dest, subject, body, headers: RawUtf8;
  tls: TNetTlsContext;
  tlsp: PNetTlsContext;
  ok: boolean;
  i: integer;
begin
  // ----- parse the command line -----
  // usage: mailtest user:password@server:port from@x to@y [subject] [body] [options]
  tlsp := nil;
  conn := '';
  from := '';
  dest := '';
  subject := 'mORMot STARTTLS test';
  body := 'This is a test email sent by the mORMot 2 mailtest tool.';
  headers := '';
  i := 1;
  while i <= ParamCount do
  begin
    if IdemPropNameU(StringToUtf8(ParamStr(i)), '--ignorecert') then
    begin
      InitNetTlsContext(tls);
      tls.IgnoreCertificateErrors := true;
      tlsp := @tls;
    end
    else if IdemPropNameU(StringToUtf8(ParamStr(i)), '-h') or
            IdemPropNameU(StringToUtf8(ParamStr(i)), '--header') then
    begin
      // -h "Name: value" adds an individual header line (like curl -H), which
      // is passed through to SendEmail's Headers parameter (repeatable)
      inc(i);
      if i <= ParamCount then
        headers := headers + StringToUtf8(ParamStr(i)) + #13#10;
    end
    else if conn = '' then
      conn := StringToUtf8(ParamStr(i))
    else if from = '' then
      from := StringToUtf8(ParamStr(i))
    else if dest = '' then
      dest := StringToUtf8(ParamStr(i))
    else if subject = 'mORMot STARTTLS test' then
      subject := StringToUtf8(ParamStr(i))
    else
      body := StringToUtf8(ParamStr(i));
    inc(i);
  end;
  if (conn = '') or
     (from = '') or
     (dest = '') then
  begin
    ConsoleWrite('Usage:');
    ConsoleWrite('  mailtest user:password@server:port from@x to@y ' +
      '[subject] [body] [options]');
    ConsoleWrite('');
    ConsoleWrite('  -h "Name: value" : add an individual header (like curl, repeatable)');
    ConsoleWrite('                     e.g. -h "Reply-To: reply@x.de" -h "Cc: cc@x.de"');
    ConsoleWrite('  --ignorecert     : disable the server certificate validation');
    ConsoleWrite('');
    ConsoleWrite('Examples:');
    ConsoleWrite('  Gmail STARTTLS (port 587, app password):');
    ConsoleWrite('    mailtest me:app-pwd@smtp.gmail.com:587 me@gmail.com you@x.de');
    ConsoleWrite('  Implicit TLS (port 465):');
    ConsoleWrite('    mailtest me:pwd@smtp.example.com:465 me@x.de you@x.de');
    ConsoleWrite('  Plain SMTP (port 25):');
    ConsoleWrite('    mailtest @localhost:25 me@x.de you@x.de');
    ExitCode := 1;
    exit;
  end;
  // ----- resolve the SMTP connection ('user:password@server:port') -----
  if not smtp.FromText(conn) then
  begin
    ConsoleWrite('Invalid connection string: %', [conn]);
    ExitCode := 1;
    exit;
  end;
  // the TSmtpConnection overload guesses the TLS mode from the port:
  //   465 -> stlsImplicit, 587 -> stlsStartTls, else stlsNone (plain)
  ConsoleWrite('Sending mail via %:% (user=%) ...',
    [smtp.Host, smtp.Port, smtp.User]);
  try
    // body is converted to ISO-8859-1 (the SendEmail default TextCharSet)
    ok := SendEmail(smtp, from, dest, MimeHeaderEncode(subject),
      Utf8ToWinAnsi(body), headers, 'ISO-8859-1', stlsNone, tlsp);
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
