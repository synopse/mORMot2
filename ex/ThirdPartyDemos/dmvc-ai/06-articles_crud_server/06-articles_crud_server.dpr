program ArticlesCrudServer;

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
  mormot.core.unicode,
  server in 'src\server.pas',
  entities in 'src\entities.pas',
  api.interfaces in 'src\api.interfaces.pas',
  api.impl in 'src\api.impl.pas';

var
  srv: TArticlesCrudServer;

begin
  // Port of DMVC logging setup
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;

  WriteLn('mORMot2 Articles CRUD Server');
  WriteLn('============================');
  WriteLn('Port of DMVC articles_crud_server to mORMot2');
  WriteLn;

  try
    srv := TArticlesCrudServer.Create('8080');
    try
      srv.Start;
      WriteLn('Server started on http://localhost:8080');
      WriteLn;
      WriteLn('Available endpoints (JSON-RPC style):');
      WriteLn('  POST http://localhost:8080/ArticlesApi.GetAll');
      WriteLn('  POST http://localhost:8080/ArticlesApi.Search');
      WriteLn('       Body: {"query":"Pizza"}');
      WriteLn('  POST http://localhost:8080/ArticlesApi.GetMeta');
      WriteLn('  POST http://localhost:8080/ArticlesApi.GetById');
      WriteLn('       Body: {"id":1}');
      WriteLn('  POST http://localhost:8080/ArticlesApi.Create');
      WriteLn('       Body: {"article":{"code":"C999","description":"Test","price":10.50}}');
      WriteLn('  POST http://localhost:8080/ArticlesApi.CreateBulk');
      WriteLn('       Body: {"articles":[...]}');
      WriteLn('  POST http://localhost:8080/ArticlesApi.Update');
      WriteLn('       Body: {"id":1,"article":{...}}');
      WriteLn('  POST http://localhost:8080/ArticlesApi.Delete');
      WriteLn('       Body: {"id":1}');
      WriteLn;
      WriteLn('Business rules:');
      WriteLn('  - Article code must be in format CXX, CXXX, or CXXXX (e.g., C01, C123, C1234)');
      WriteLn('  - Price must be > 2 for updates');
      WriteLn('  - Price must be > 5 for deletes');
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
