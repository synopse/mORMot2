program SSLClientSample;

{
  SSL/HTTPS Client Sample - mORMot2 DMVC Port

  This example demonstrates:
  - Connecting to HTTPS endpoints using mORMot2's TSimpleHttpClient
  - Certificate validation with proper error handling
  - Option to ignore certificate errors (for development with self-signed certs)
  - GET and POST requests over secure connections
  - Proper TLS/SSL configuration

  Based on DelphiMVCFramework's ssl_client sample, ported to mORMot2.

  Key mORMot2 Features Used:
  - TSimpleHttpClient: High-level HTTP/HTTPS client
  - TNetTlsContext: TLS/SSL configuration
  - IgnoreCertificateErrors: Option for development/testing
  - Automatic protocol detection (HTTP/HTTPS)

  Security Notes:
  - In production, ALWAYS use proper certificates and validation
  - IgnoreCertificateErrors should ONLY be used in development
  - For client authentication, configure CertificateFile and PrivateKeyFile
  - For custom CA certificates, use CACertificatesFile

  Usage:
  1. Start an HTTPS server (e.g., example 11-ssl_server)
  2. Run this client application
  3. Enter the HTTPS URL
  4. Choose whether to validate certificates
  5. Click GET or POST to make a secure request
}

{$I mormot.defines.inc}

{$APPTYPE GUI}

uses
  {$ifdef MSWINDOWS}
  Windows,
  {$endif}
  Forms,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  MainFormU in 'src\MainFormU.pas' {MainForm};

{$R *.res}

begin
  {$ifdef DEBUG}
  // Enable detailed logging for debugging TLS issues
  with TSynLog.Family do
  begin
    Level := LOG_VERBOSE;
    PerThreadLog := ptIdentifiedInOneFile;
    HighResolutionTimestamp := true;
    AutoFlushTimeOut := 1;
  end;
  {$endif}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'SSL/HTTPS Client - mORMot2';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
