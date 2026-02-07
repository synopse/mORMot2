{:
———————————————————————————————————————————————— © martindoyle 2017-2026 ——
 Project : Rechnung

 Using mORMot2
     Synopse mORMot2 framework. Copyright (C) 2025 Arnaud Bouchez
     Synopse Informatique - http://synopse.info

  Module : RechnungDaemon.dpr

  Last modified
    Date : 07.02.2026
    Author : Martin Doyle
    Email : martin-doyle@online.de

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to
    deal in the Software without restriction, including without limitation the
    rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
    sell copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in
    all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
    FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
    IN THE SOFTWARE.
————————————————————————————————————————————————————————————————————————————
}
program RechnungDaemon;

{$APPTYPE CONSOLE}

{$I mormot.defines.inc}

uses
  {$I mormot.uses.inc}
  SysUtils,
  mormot.app.daemon,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.db.raw.sqlite3,
  mormot.db.raw.sqlite3.static,
  mormot.orm.core,
  mormot.rest.http.server,
  rgData,
  rgConst,
  rgServer,
  rgServiceInterfaces,
  rgServiceImplementation;

type

  { TRgDaemonSettings }

  TRgDaemonSettings = class(TSynDaemonSettings)
  public
    constructor Create; override;
  end;

  { TRgDaemon }

  TRgDaemon = class(TSynDaemon)
  protected
    fModel: TOrmModel;
    fServer: TRgServer;
    fHttpServer: TRestHttpServer;
  public
    procedure Start; override;
    procedure Stop; override;
  end;

{ TRgDaemonSettings }

constructor TRgDaemonSettings.Create;
begin
  inherited Create;
  Log := LOG_VERBOSE;
  ServiceName := 'RechnungDaemon';
  ServiceDisplayName := 'Rechnung Daemon';
end;

{ TRgDaemon }

procedure TRgDaemon.Start;
begin
  SQLite3Log.Enter(self);
  fModel := CreateModel;
  fServer := TRgServer.Create(fModel, DataFile);
  fServer.DB.Synchronous := smOff;
  fServer.DB.LockingMode := lmExclusive;
  fHttpServer := TRestHttpServer.Create(
    HttpPort, [fServer], '+', HTTP_DEFAULT_MODE, 4);
  fHttpServer.AccessControlAllowOrigin := '*';
  SQLite3Log.Add.Log(sllInfo,
    'Rechnung HTTP server started on port %', [HttpPort]);
end;

procedure TRgDaemon.Stop;
begin
  if fHttpServer = nil then
    exit;
  SQLite3Log.Enter(self);
  try
    try
      FreeAndNil(fHttpServer);
      SQLite3Log.Add.Log(sllInfo, 'HTTP server stopped');
    except
      SQLite3Log.Add.Log(sllWarning, 'Error shutting down HTTP server');
    end;
  finally
    try
      FreeAndNil(fServer);
      SQLite3Log.Add.Log(sllInfo, 'Rechnung server stopped');
    except
      SQLite3Log.Add.Log(sllWarning, 'Error shutting down server');
    end;
    FreeAndNil(fModel);
  end;
end;

var
  Daemon: TRgDaemon;

begin
  with SQLite3Log.Family do
  begin
    Level := LOG_VERBOSE;
    PerThreadLog := ptIdentifiedInOnFile;
    EchoToConsole := LOG_VERBOSE;
  end;
  Daemon := TRgDaemon.Create(
    TRgDaemonSettings, Executable.ProgramFilePath, '', '');
  try
    Daemon.CommandLine;
  finally
    Daemon.Free;
  end;
end.
