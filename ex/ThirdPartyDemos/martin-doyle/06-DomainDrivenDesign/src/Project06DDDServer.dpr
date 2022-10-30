program Project06DDDServer;

{$APPTYPE CONSOLE}

{$I mormot.defines.inc}
uses
  {$I mormot.uses.inc}
  mormot.core.base,
  mormot.core.log,
  mormot.core.os,
  mormot.db.raw.sqlite3,
  mormot.orm.core,
  mormot.rest.http.server,
  DomRepositoryInterfaces,
  InfraRepositoryImplementation,
  data,
  server in 'server.pas';

var
  ServiceServer: TServiceServer;
  SampleRepository: ISampleRepository;
  HttpServer: TRestHttpServer;
  LogFamily: TSynLogFamily;
begin
  LogFamily := SQLite3Log.Family;
  LogFamily.Level := LOG_VERBOSE;
  LogFamily.PerThreadLog := ptIdentifiedInOnFile;
  LogFamily.EchoToConsole := LOG_VERBOSE;
  try
    SampleRepository := TSampleRepository.Create;
    try
      ServiceServer := TServiceServer.Create(SampleRepository);
      try
        HttpServer := TRestHttpServer.Create(HTTP_PORT, [ServiceServer], '+', HTTP_DEFAULT_MODE,4 );
        HttpServer.AccessControlAllowOrigin := '*';
        try
          Writeln('Server started on port ' + HTTP_PORT);
          Readln;
        finally
          HttpServer.Free;
        end;
      finally
        ServiceServer.Free;
      end;
    finally
      SampleRepository := nil;
    end;
  finally
  end;
end.
