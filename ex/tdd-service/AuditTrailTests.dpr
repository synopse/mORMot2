program AuditTrailTests;

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$APPTYPE CONSOLE}
{$endif OSWINDOWS}

uses
  {$I mormot.uses.inc}
  SysUtils,
  mormot.core.base,
  mormot.core.log,
  mormot.core.test,
  tests;
  
begin

  TAuditTrailTests.RunAsConsole('Ekon Workshop Regression Tests', LOG_VERBOSE);
    
end.
