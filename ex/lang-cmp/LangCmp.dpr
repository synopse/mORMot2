/// mORMot version of transit-lang-cmp languages comparison
program LangCmp;

{
  Implements https://github.com/losvedir/transit-lang-cmp with mORMot/FPC.
  Please download and unzip in ./MBTA_GTFS sub-folder the trips.txt and
   stop_times.txt reference CSV from https://cdn.mbta.com/MBTA_GTFS.zip
}

// define this to enable TRawUtf8Interning for CSV values
// - loading is slightly slower, but memory consumption is 68MB instead of 380MB
{$define LANGCMP_CSVTEXTINTERNING}

// define this to display BuildTripResponse and JSON timing
{ $define LANGCMP_TIMING}


{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$APPTYPE CONSOLE}
{$endif OSWINDOWS}

{$ifdef FPC_CPUX64}
  {$define FPC_X64MM} // use our x86_64 asm memory manager
{$endif FPC_CPUX64}

uses
  {$I mormot.uses.inc}
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.data,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.search,
  mormot.net.http,
  mormot.net.server,
  mormot.net.async;

type
  // high-level data structures with RawUtf8 (interned) values
  TStopTime = record
    TripID, StopID, Arrival, Departure: RawUtf8;
  end;
  TStopTimes = array of TStopTime;

  TTrip = record
    RouteID, TripID, ServiceID: RawUtf8;
  end;
  TTrips = array of TTrip;

  // we use PUtf8Char for responses as Rust is using &'data str
  TScheduleResponse = record
    StopID, Arrival, Departure: PUtf8Char;
  end;
  TScheduleResponses = array of TScheduleResponse;

  TTripResponse = record
    TripID, ServiceID, RouteID: PUtf8Char;
    Schedules: TScheduleResponses;
  end;
  TTripResponses = array of TTripResponse;


type
  // main HTTP server with its internal in-memory store
  TScheduler = class
  private
    stopTime: TStopTimes;
    trip: TTrips;
    stopTimes, trips: TDynArray;
    intern: TRawUtf8Interning;
    server: THttpServerSocketGeneric;
    procedure GetStopTimes;
    procedure GetTrips;
    procedure BuildTripResponse(const route: RawUtf8; out result: TTripResponses);
    function BuildTripResponseJson(const route: RawUtf8): RawUtf8;
    function DoOnRequest(Ctxt: THttpServerRequestAbstract): cardinal;
  public
    procedure LoadCsvData;
    procedure StartServer;
    destructor Destroy; override;
  end;

procedure TScheduler.GetStopTimes;
var
  start, stop: Int64;
  csv: RawUtf8;
begin
  QueryPerformanceMicroSeconds(start);
  stopTimes.InitSpecific(TypeInfo(TStopTimes), stopTime, ptRawUtf8);
  csv := StringFromFile(NormalizeFileName('../MBTA_GTFS/stop_times.txt'));
  TDynArrayLoadCsv(stopTimes, pointer(csv), intern);
  stopTimes.EnsureSorted; // sort by first ptRawUtf8 field = TripID
  QueryPerformanceMicroSeconds(stop);
  ConsoleWrite('parsed % stop times in %',
    [length(stopTime), MicroSecToString(stop - start)]);
end;

procedure TScheduler.GetTrips;
var
  start, stop: Int64;
  csv: RawUtf8;
begin
  QueryPerformanceMicroSeconds(start);
  trips.InitSpecific(TypeInfo(TTrips), trip, ptRawUtf8);
  csv := StringFromFile(NormalizeFileName('../MBTA_GTFS/trips.txt'));
  TDynArrayLoadCsv(trips, pointer(csv), intern);
  trips.EnsureSorted; // sort by first ptRawUtf8 field = RouteID
  QueryPerformanceMicroSeconds(stop);
  ConsoleWrite('parsed % trips in %',
    [length(trip), MicroSecToString(stop - start)]);
end;

procedure TScheduler.LoadCsvData;
begin
  {$ifdef LANGCMP_CSVTEXTINTERNING}
  intern := TRawUtf8Interning.Create;
  {$endif LANGCMP_CSVTEXTINTERNING};
  GetStopTimes;
  GetTrips;
end;

procedure TScheduler.BuildTripResponse(
  const route: RawUtf8; out result: TTripResponses);
var
  tr: ^TTrip;
  st: ^TStopTime;
  trn, stn: integer;
  res: ^TTripResponse;
  sch: ^TScheduleResponse;
begin
  tr := trips.FindAllSorted(route, trn);
  if tr = nil then
    exit;
  SetLength(result, trn);
  res := pointer(result);
  repeat
    res^.TripID := pointer(tr^.TripID);
    res^.ServiceID := pointer(tr^.ServiceID);
    res^.RouteID := pointer(tr^.RouteID);
    st := stopTimes.FindAllSorted(tr^.TripID, stn);
    if st <> nil then
    begin
      SetLength(res^.Schedules, stn);
      sch := pointer(res^.Schedules);
      repeat
        sch^.StopID := pointer(st^.StopID);
        sch^.Arrival := pointer(st^.Arrival);
        sch^.Departure := pointer(st^.Departure);
        inc(sch);
        inc(st);
        dec(stn);
      until stn = 0;
    end;
    inc(res);
    inc(tr);
    dec(trn);
  until trn = 0;
end;

{$ifdef LANGCMP_TIMING}

function TScheduler.BuildTripResponseJson(const route: RawUtf8): RawUtf8;
var
  resp: TTripResponses;
  start, stop: Int64;
begin
  QueryPerformanceMicroSeconds(start);
  BuildTripResponse(route, resp);
  QueryPerformanceMicroSeconds(stop);
  ConsoleWrite('BuildTripResponseJson: %', [MicroSecToString(stop - start)]);
  QueryPerformanceMicroSeconds(start);
  result := DynArraySaveJson(resp, TypeInfo(TTripResponses));
  QueryPerformanceMicroSeconds(stop);
  ConsoleWrite('JSONify: % (%)',
    [MicroSecToString(stop - start), KBNoSpace(length(result))]);
  QueryPerformanceMicroSeconds(start);
  Finalize(resp);
  QueryPerformanceMicroSeconds(stop);
  ConsoleWrite('cleanup: %', [MicroSecToString(stop - start)]);
end;

{$else}

function TScheduler.BuildTripResponseJson(const route: RawUtf8): RawUtf8;
var
  resp: TTripResponses;
begin
  BuildTripResponse(route, resp);
  result := DynArraySaveJson(resp, TypeInfo(TTripResponses));
end;

{$endif LANGCMP_TIMING}

function TScheduler.DoOnRequest(Ctxt: THttpServerRequestAbstract): cardinal;
begin
  if not IdemPChar(pointer(Ctxt.Url), '/SCHEDULES/') then
  begin
    result := HTTP_NOTFOUND;
    exit;
  end;
  Ctxt.OutContent := BuildTripResponseJson(copy(Ctxt.Url, 12, 100));
  Ctxt.OutContentType := JSON_CONTENT_TYPE;
  result := HTTP_SUCCESS;
end;

procedure TScheduler.StartServer;
begin
  server := THttpAsyncServer.Create('4000', nil, nil, '',
    SystemInfo.dwNumberOfProcessors + 1, 120000,
    [hsoNoXPoweredHeader, hsoNoStats]);
  server.HttpQueueLength := 100000;
  server.OnRequest := DoOnRequest;
  server.WaitStarted;
end;

destructor TScheduler.Destroy;
begin
  server.Free;
  intern.Free;
  inherited Destroy;
end;


begin
  // register the data structure fields
  Rtti.RegisterFromText([
    // those two are used for TDynArrayLoadCsv() parsing
    TypeInfo(TStopTime),
      'trip_id, stop_id, arrival_time, departure_time: RawUtf8',
    TypeInfo(TTrip),
      'route_id, trip_id, service_id: RawUtf8',
    // those two are used for JSON result generation
    TypeInfo(TScheduleResponses),
      'stop_id, arrival_time, departure_time: PUtf8Char',
    TypeInfo(TTripResponse),
      'trip_id, service_id, route_id: PUtf8Char; schedules: TScheduleResponses'
    ]);
  // run the HTTP server
  try
    with TScheduler.Create do
    try
      LoadCsvData;
      StartServer;
      ConsoleWrite('Server is running - try http://localhost:4000/schedules/121');
      {$ifdef OSPOSIX}
      SynDaemonIntercept; // to intercept ^C and SIGQUIT
      {$endif OSPOSIX}
      ConsoleWaitForEnterKey;
      ConsoleWrite('Server shutdown');
    finally
      Free;
    end;
  except
    on E: Exception do
      ConsoleShowFatalException(E, false)
  end;
  {$ifdef FPC_X64MM}
  WriteHeapStatus;
  {$endif FPC_X64MM}
end.
