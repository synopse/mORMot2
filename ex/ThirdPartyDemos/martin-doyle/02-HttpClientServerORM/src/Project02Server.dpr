{:
Synopse mORMot framework 2

Sample 02 - Http Client Server ORM
purpose of this sample is to show the basic ORM usage of the framework:
}
program Project02Server;

{$APPTYPE CONSOLE}

{$I mormot.defines.inc}
uses
  {$I mormot.uses.inc}
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.db.raw.sqlite3,
  mormot.orm.core,
  mormot.rest.http.server,
  data in 'data.pas',
  server in 'server.pas';

var
  Model: TOrmModel;
  SampleServer: TSampleServer;
  HttpServer: TRestHttpServer;
  LogFamily: TSynLogFamily;

begin
  LogFamily := SQLite3Log.Family;
  LogFamily.Level := LOG_VERBOSE;
  LogFamily.PerThreadLog := ptIdentifiedInOnFile;
  LogFamily.EchoToConsole := LOG_VERBOSE;
  Model := CreateSampleModel;
  try
    SampleServer := TSampleServer.Create(Model, ChangeFileExt(Executable.ProgramFileName,'.db'));
    try
      SampleServer.DB.Synchronous := smOff;
      SampleServer.DB.LockingMode := lmExclusive;
      SampleServer.Server.CreateMissingTables;
      HttpServer := TRestHttpServer.Create(HttpPort,[SampleServer],'+',HTTP_DEFAULT_MODE, 4);
      HttpServer.AccessControlAllowOrigin := '*';
      try
        Writeln('Server started on port ' + HttpPort);
        Readln;
      finally
        HttpServer.Free;
      end;
    finally
      SampleServer.Free;
    end;
  finally
    Model.Free;
  end;
end.
