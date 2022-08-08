/// test access to a local MongoDB instance
program MongoDBTests;

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$APPTYPE CONSOLE}
{$endif OSWINDOWS}

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
