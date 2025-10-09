unit httpServerMain;

{$I mormot.defines.inc}

interface

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.log,
  mormot.core.text,
  mormot.core.data,
  mormot.net.sock,
  mormot.net.http,
  mormot.net.server,
  mormot.net.async,
  mormot.lib.openssl11,
  mormot.crypt.openssl;

var
  silent: boolean;

procedure Main;

implementation


procedure Main;
var
  F: TSearchRec;
  fn: TFileName;
  console, verbose: boolean;
  cmd: TExecutableCommandLine;
  logger: TSynLogFamily;
  settingsfolder, folder: TFileName;
  url: RawUtf8;
  settings: THttpProxyServerSettings;
  one: THttpProxyUrl;
  server: THttpProxyServer;
begin
  settings := THttpProxyServerSettings.Create;
  try
    // command line switches
    cmd := Executable.Command;
    console := cmd.Option('&console',    'debug output to the console');
    verbose := cmd.Option('log&verbose', 'enable verbose log');
    silent  := cmd.Option('silent', 'no output to the console');
    settingsfolder := cmd.ParamS('&settings', '#folder where *.json are located',
      Executable.ProgramFilePath + 'sites-enabled');
    folder := cmd.ParamS('&folder', 'a local #foldername to serve');
    url := cmd.Param('&url', 'a root #uri to serve this folder');
    SetObjectFromExecutableCommandLine(settings.Server, '', ' for HTTP/HTTPS');
    SetObjectFromExecutableCommandLine(settings.Server.Log, 'Log', ' for EnableLogging');
    {$ifdef USE_OPENSSL}
    OpenSslDefaultPath := cmd.ParamS('LibSsl', 'OpenSSL libraries #path');
    if OpenSslInitialize then
      RegisterOpenSsl;
    {$endif USE_OPENSSL}
    if cmd.ConsoleHelpFailed(
      'mORMot 2.' + SYNOPSE_FRAMEWORK_BRANCH + ' HTTP/HTTPS File Server') then
    begin
      ExitCode := 1;
      exit;
    end;
    // load local *.json files with URI
    if (settingsfolder <> '') and
       (FindFirst(MakePath([settingsfolder, '*.json']), faAnyFile - faDirectory, F) = 0) then
    begin
      repeat
        if SearchRecValidFile(F) then
        begin
          fn := Executable.ProgramFilePath + F.Name;
          one := THttpProxyUrl.Create;
          if JsonFileToObject(fn, one, nil, JSONPARSER_TOLERANTOPTIONS) then
            settings.AddUrl(one)
          else
            one.Free;
        end;
      until FindNext(F) <> 0;
      FindClose(F);
    end;
    // ensure we have something to serve (maybe from command line)
    if folder <> '' then
      settings.AddFolder(folder, url);
    if settings.Url = nil then
    begin
      ConsoleWrite('No folder to serve'#10, ccLightRed);
      ConsoleWrite(Executable.Command.FullDescription);
      ExitCode := 2;
      exit;
    end;
    // setup the TSynLog context
    if console or
       verbose then
    begin
      logger := TSynLog.Family;
      if verbose then
      begin
        logger.Level := LOG_VERBOSE;
        settings.Server.Options := settings.Server.Options + [psoLogVerbose];
      end
      else
        logger.Level := [sllWarning] + LOG_FILTER[lfErrors];
      if console and
         not silent then
        logger.EchoToConsole := logger.Level;
      logger.PerThreadLog := ptIdentifiedInOneFile;
      logger.HighResolutionTimestamp := true;
      logger.AutoFlushTimeOut := 1;
    end;
    // run the server
    server := THttpProxyServer.Create(settings);
    try
      if not silent then
        ConsoleWrite(['Bind server ', settings.Server.Port]);
      server.Start;
      if not silent then
        ConsoleWrite('Server running. Press [Enter] or Ctrl+C to quit.');
      ConsoleWaitForEnterKey;
      if not silent then
        ConsoleWrite('Server shutting down...');
    finally
      server.Free;
    end;
  finally
    settings.Free;
  end;
end;

end.

