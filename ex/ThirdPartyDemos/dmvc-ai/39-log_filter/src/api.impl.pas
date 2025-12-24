unit api.impl;

interface

uses
  mormot.core.base,
  mormot.core.log,
  mormot.core.text,
  api.interfaces;

type
  TLogFilterApi = class(TInterfacedObject, ILogFilterApi)
  public
    function Index: RawUtf8;
    function NotLogged: RawUtf8;
    function VerboseOnly: RawUtf8;
    function TestLevels: RawUtf8;
  end;

implementation

uses
  SysUtils,
  mormot.core.rtti;

{ TLogFilterApi }

function TLogFilterApi.Index: RawUtf8;
begin
  TSynLog.Add.Log(sllInfo, 'Index endpoint called - this appears in log');
  Result := '<html><body>' +
            '<h1>Hello mORMot2 Log Filter Sample</h1>' +
            '<p>This request is <strong>logged</strong> to file and console.</p>' +
            '<p>Try these endpoints:</p>' +
            '<ul>' +
            '<li><a href="/root/LogFilterApi/Index">/Index</a> - Logged request</li>' +
            '<li><a href="/root/LogFilterApi/NotLogged">/NotLogged</a> - Filtered request</li>' +
            '<li><a href="/root/LogFilterApi/VerboseOnly">/VerboseOnly</a> - Verbose logging</li>' +
            '<li><a href="/root/LogFilterApi/TestLevels">/TestLevels</a> - All log levels</li>' +
            '</ul>' +
            '<p>Check LogFilterSample.log to see which requests are filtered.</p>' +
            '</body></html>';
end;

function TLogFilterApi.NotLogged: RawUtf8;
begin
  // This endpoint's requests are filtered out by the custom log filter
  // The filter in server.pas excludes URIs containing '/NotLogged'
  Result := FormatUtf8('Current time: % (this request not logged to file)', 
                       [DateTimeToStr(Now)]);
end;

function TLogFilterApi.VerboseOnly: RawUtf8;
begin
  TSynLog.Add.Log(sllTrace, 'Trace level message - only if LOG_VERBOSE enabled');
  TSynLog.Add.Log(sllDebug, 'Debug level message');
  TSynLog.Add.Log(sllInfo, 'Info level message');
  
  Result := 'Verbose logging test complete - check log file for trace/debug/info messages';
end;

function TLogFilterApi.TestLevels: RawUtf8;
begin
  TSynLog.Add.Log(sllTrace, 'TRACE: Detailed diagnostic information');
  TSynLog.Add.Log(sllDebug, 'DEBUG: Debugging information');
  TSynLog.Add.Log(sllInfo, 'INFO: Informational message');
  TSynLog.Add.Log(sllWarning, 'WARNING: Warning message');
  TSynLog.Add.Log(sllError, 'ERROR: Error message (simulated)');
  
  Result := 'Logged messages at all levels: TRACE, DEBUG, INFO, WARNING, ERROR. ' +
            'Check LogFilterSample.log to see them.';
end;

end.
