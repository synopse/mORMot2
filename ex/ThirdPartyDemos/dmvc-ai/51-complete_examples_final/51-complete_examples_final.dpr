program CompleteExamplesFinal;

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
  mormot.core.json,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.rest.http.server,
  server in 'src\server.pas',
  entities in 'src\entities.pas',
  api.interfaces in 'src\api.interfaces.pas',
  api.impl in 'src\api.impl.pas';

var
  srv: TCompleteExampleServer;

begin
  // Configure comprehensive logging
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;
  TSynLog.Family.HighResolutionTimestamp := true;
  TSynLog.Family.AutoFlushTimeOut := 1;

  WriteLn('mORMot2 Complete Examples - Final Showcase');
  WriteLn('===========================================');
  WriteLn('Comprehensive demonstration of mORMot2 capabilities');
  WriteLn;
  WriteLn('This sample integrates:');
  WriteLn('  - ORM with SQLite persistence');
  WriteLn('  - Interface-based services');
  WriteLn('  - RESTful CRUD operations');
  WriteLn('  - JSON-RPC style API');
  WriteLn('  - Authentication & authorization');
  WriteLn('  - Performance monitoring');
  WriteLn('  - Comprehensive logging');
  WriteLn('  - Error handling');
  WriteLn;

  try
    srv := TCompleteExampleServer.Create('8080');
    try
      srv.Start;
      WriteLn('Server started on http://localhost:8080');
      WriteLn;
      WriteLn('Available endpoints:');
      WriteLn;
      WriteLn('=== ORM REST Endpoints ===');
      WriteLn('  GET    /root/Article              - List all articles');
      WriteLn('  GET    /root/Article/123          - Get article by ID');
      WriteLn('  POST   /root/Article              - Create new article');
      WriteLn('  PUT    /root/Article              - Update article');
      WriteLn('  DELETE /root/Article/123          - Delete article');
      WriteLn;
      WriteLn('=== Interface-based Services ===');
      WriteLn('  POST   /root/CompleteApi/GetVersion');
      WriteLn('         Returns server version info');
      WriteLn;
      WriteLn('  POST   /root/CompleteApi/ProcessData');
      WriteLn('         body: {"data": "test input"}');
      WriteLn('         Demonstrates data processing with validation');
      WriteLn;
      WriteLn('  POST   /root/CompleteApi/GetArticlesByAuthor');
      WriteLn('         body: {"author": "John Doe"}');
      WriteLn('         Returns filtered articles');
      WriteLn;
      WriteLn('  POST   /root/CompleteApi/BatchCreateArticles');
      WriteLn('         body: {"count": 5, "prefix": "Test"}');
      WriteLn('         Demonstrates batch operations');
      WriteLn;
      WriteLn('  POST   /root/CompleteApi/GetStatistics');
      WriteLn('         Returns database statistics');
      WriteLn;
      WriteLn('=== Testing Commands ===');
      WriteLn('# Get version');
      WriteLn('curl -X POST http://localhost:8080/root/CompleteApi/GetVersion');
      WriteLn;
      WriteLn('# Create article via ORM');
      WriteLn('curl -X POST http://localhost:8080/root/Article \');
      WriteLn('  -H "Content-Type: application/json" \');
      WriteLn('  -d ''{"Title":"Test Article","Author":"John Doe","Content":"Sample content"}''');
      WriteLn;
      WriteLn('# Batch create via service');
      WriteLn('curl -X POST http://localhost:8080/root/CompleteApi/BatchCreateArticles \');
      WriteLn('  -H "Content-Type: application/json" \');
      WriteLn('  -d ''{"count":3,"prefix":"Demo"}''');
      WriteLn;
      WriteLn('# Get statistics');
      WriteLn('curl -X POST http://localhost:8080/root/CompleteApi/GetStatistics');
      WriteLn;
      WriteLn('# List all articles');
      WriteLn('curl http://localhost:8080/root/Article');
      WriteLn;
      WriteLn('Check CompleteExamplesFinal.log for detailed operation logging');
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
      TSynLog.Add.Log(sllError, 'Fatal error: %', [E.Message], E);
    end;
  end;

end.
