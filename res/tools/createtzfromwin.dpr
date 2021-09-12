program createtzfromwin;

{$apptype console}

uses
  mormot.core.search;

begin
  {$ifdef MSWINDOWS}
  TSynTimeZone.Default.SaveToFile('..\tz.dat');
  writeln('tz.dat recreated');
  {$endif MSWINDOWS}
end.