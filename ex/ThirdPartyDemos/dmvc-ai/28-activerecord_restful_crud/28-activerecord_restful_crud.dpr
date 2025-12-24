program ActiveRecordRestfulCrud;

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$APPTYPE CONSOLE}
{$endif OSWINDOWS}

uses
  {$I mormot.uses.inc}
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.rtti,
  mormot.core.unicode,
  server in 'src\server.pas',
  entities in 'src\entities.pas',
  api.interfaces in 'src\api.interfaces.pas',
  api.impl in 'src\api.impl.pas';

var
  srv: TActiveRecordServer;

begin
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;

  WriteLn('mORMot2 ActiveRecord RESTful CRUD Sample');
  WriteLn('=========================================');
  WriteLn;
  WriteLn('Port of DMVCFramework activerecord_restful_crud sample');
  WriteLn;

  try
    srv := TActiveRecordServer.Create('8080');
    try
      srv.Start;
      WriteLn('Server started on http://localhost:8080/activerecord');
      WriteLn;
      WriteLn('Available endpoints:');
      WriteLn('  People:');
      WriteLn('    GET    /activerecord/ActiveRecordApi/GetAllPeople');
      WriteLn('    GET    /activerecord/ActiveRecordApi/GetPerson?id=1');
      WriteLn('    POST   /activerecord/ActiveRecordApi/CreatePerson');
      WriteLn('    POST   /activerecord/ActiveRecordApi/UpdatePerson');
      WriteLn('    DELETE /activerecord/ActiveRecordApi/DeletePerson?id=1');
      WriteLn;
      WriteLn('  Articles:');
      WriteLn('    GET    /activerecord/ActiveRecordApi/GetAllArticles');
      WriteLn('    GET    /activerecord/ActiveRecordApi/GetArticle?id=1');
      WriteLn('    POST   /activerecord/ActiveRecordApi/CreateArticle');
      WriteLn('    POST   /activerecord/ActiveRecordApi/UpdateArticle');
      WriteLn('    DELETE /activerecord/ActiveRecordApi/DeleteArticle?id=1');
      WriteLn;
      WriteLn('  Phones:');
      WriteLn('    GET    /activerecord/ActiveRecordApi/GetPhonesByPerson?personId=1');
      WriteLn('    POST   /activerecord/ActiveRecordApi/CreatePhone');
      WriteLn('    DELETE /activerecord/ActiveRecordApi/DeletePhone?id=1');
      WriteLn;
      WriteLn('Press [Enter] to quit');
      ReadLn;
    finally
      srv.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      TSynLog.Add.Log(sllError, 'Fatal error: %', [E.Message]);
    end;
  end;

end.
