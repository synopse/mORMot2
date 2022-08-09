/// test access to a local MongoDB instance
program MongoDBTests;

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$APPTYPE CONSOLE}
{$endif OSWINDOWS}

{
  Hints for Docker:
  - sudo docker run --name mongodb -d -p 27017:27017 mongo:latest
  - sudo docker exec -it mongodb bash
  - sudo docker stop mongodb
  - sudo docker rm mongodb

}
uses
  {$I mormot.uses.inc}
  mormot.core.log,
  MongoDBTestCases;

begin
  with TTestMongoDB.Create do
  try
    Run;
    {$ifdef OSWINDOWS}
    readln;
    {$endif OSWINDOWS}
  finally
    Free;
  end;
  {$ifdef FPC_X64MM}
  WriteHeapStatus(#13#10'Memory Usage Report:', 16, 12, {flags=}true);
  {$endif FPC_X64MM}
end.
