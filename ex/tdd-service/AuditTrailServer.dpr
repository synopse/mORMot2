program AuditTrailServer;

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$APPTYPE CONSOLE}
{$endif OSWINDOWS}

uses
  {$I mormot.uses.inc}
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.log,
  mormot.app.daemon,
  mormot.db.raw.sqlite3,
  mormot.db.raw.sqlite3.static,
  dom.infra,       // IEventPersistence
  infra.orm,       // TEventPersistence (step 5)
  api.mobile,      // IApiMobile (registers the interface + DTO)
  app.server;      // TAuditTrailServer (the composition root)

type
  /// the JSON/INI settings persisted next to the executable
  TAuditTrailDaemonSettings = class(TSynDaemonSettings)
  public
    constructor Create; override;
  end;

  /// step 8: a stand-alone service/daemon hosting the mobile API over HTTP
  // - inheriting from TSynDaemon makes the very same executable run as a
  //   console application (--console), a POSIX daemon (--run / --fork) or a
  //   Windows Service (/install /start /stop /uninstall) with no code change
  TAuditTrailDaemon = class(TSynDaemon)
  protected
    fServer: TAuditTrailServer;
  public
    procedure Start; override;
    procedure Stop; override;
  end;


{ TAuditTrailDaemonSettings }

constructor TAuditTrailDaemonSettings.Create;
begin
  inherited Create;
  Log := LOG_VERBOSE;
  ServiceName := 'AuditTrailService';
  ServiceDisplayName := 'Audit Trail mobile API service';
end;


{ TAuditTrailDaemon }

procedure TAuditTrailDaemon.Start;
var
  persistence: IEventPersistence;
begin
  TSynLog.Enter(self, 'Start');
  // step 5 persistence: a SQLite3 database file kept next to the executable
  persistence := TEventPersistence.Create(
    Executable.ProgramFilePath + 'audittrail.db');
  // step 8 composition root: publish IApiMobile over HTTP (see app.server.pas)
  fServer := TAuditTrailServer.Create(persistence, AUDITTRAIL_HTTP_PORT);
  TSynLog.Add.Log(sllInfo, 'Audit Trail API ready on http://localhost:%/',
    [AUDITTRAIL_HTTP_PORT], self);
end;

procedure TAuditTrailDaemon.Stop;
begin
  TSynLog.Enter(self, 'Stop');
  FreeAndNil(fServer); // releases the HTTP edge, then the persistence
end;


begin
  with TAuditTrailDaemon.Create(
    TAuditTrailDaemonSettings, Executable.ProgramFilePath, '', '') do
  try
    CommandLine; // parse the switches and run accordingly
  finally
    Free;
  end;
end.
