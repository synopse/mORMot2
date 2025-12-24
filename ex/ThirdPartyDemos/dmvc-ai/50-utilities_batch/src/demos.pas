unit demos;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  Classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.rtti,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.data,
  mormot.core.threads,
  mormot.core.perf,
  mormot.core.json,
  mormot.core.variants;

procedure RunAllDemos;

implementation

type
  // Helper class for parallel demo
  TParallelDemo = class
  public
    Completed: Integer;
    procedure WorkerTask(IndexStart, IndexStop: integer);
  end;

  // Helper class for batch processing demo
  TBatchProcessingDemo = class
  public
    Results: array[0..99] of Integer;
    procedure ProcessBatch(IndexStart, IndexStop: integer);
  end;

  // Helper class for real-world scenario
  TRealWorldScenario = class
  public
    Processed: Integer;
    procedure ProcessRequest(IndexStart, IndexStop: integer);
  end;

{ TParallelDemo }

procedure TParallelDemo.WorkerTask(IndexStart, IndexStop: integer);
var
  i: integer;
begin
  for i := IndexStart to IndexStop do
  begin
    // Simulate work
    Sleep(10 + Random(20));
    InterlockedIncrement(Completed);
  end;
end;

{ TBatchProcessingDemo }

procedure TBatchProcessingDemo.ProcessBatch(IndexStart, IndexStop: integer);
var
  index, i, sum: Integer;
begin
  for index := IndexStart to IndexStop do
  begin
    // Simulate batch processing
    sum := 0;
    for i := 1 to 100 do
      sum := sum + (index * 100 + i);
    Results[index] := sum;
  end;
end;

{ TRealWorldScenario }

procedure TRealWorldScenario.ProcessRequest(IndexStart, IndexStop: integer);
var
  index: integer;
  json: RawUtf8;
  data: variant;
begin
  for index := IndexStart to IndexStop do
  begin
    // Simulate API request processing
    json := FormatUtf8('{"id":%,"name":"User%","email":"user%@test.com"}',
      [index, index, index]);
    _Json(json, data);
    Sleep(5 + Random(10)); // Simulate DB/processing time
    InterlockedIncrement(Processed);
  end;
end;

procedure PrintHeader(const aTitle: string);
begin
  WriteLn;
  WriteLn('=' + StringOfChar('=', Length(aTitle) + 2) + '=');
  WriteLn('| ' + aTitle + ' |');
  WriteLn('=' + StringOfChar('=', Length(aTitle) + 2) + '=');
  WriteLn;
end;

procedure DemoHighPerformanceStrings;
var
  str: RawUtf8;
  i: Integer;
  timer: TPrecisionTimer;
begin
  PrintHeader('DEMO 1: High-Performance String Operations');

  WriteLn('mORMot2 provides optimized string operations:');
  WriteLn;

  // String concatenation
  timer.Start;
  str := '';
  for i := 1 to 10000 do
    Append(str, 'item' + Int32ToUtf8(i) + ',');
  WriteLn(Format('Appended 10,000 items in %s', [timer.Stop]));
  WriteLn(Format('Result length: %d bytes', [Length(str)]));
  WriteLn;

  // Fast string operations
  WriteLn('Fast string utilities:');
  WriteLn('  - TrimU: "  hello  " -> "', TrimU('  hello  '), '"');
  WriteLn('  - UpperCaseU: "Hello" -> "', UpperCaseU('Hello'), '"');
  WriteLn('  - IdemPropNameU case-insensitive compare: ', BoolToStr(IdemPropNameU('Hello', 'HELLO'), true));
  WriteLn;

  // CSV parsing
  str := 'John,Doe,30,Engineer';
  WriteLn('CSV parsing: "', str, '"');
  WriteLn('  Split result:');
  var parts: TRawUtf8DynArray;
  parts := CsvToRawUtf8DynArray(str, ',');
  for i := 0 to High(parts) do
    WriteLn('    [', i, ']: "', parts[i], '"');
end;

procedure DemoThreadPool;
var
  parallel: TSynParallelProcess;
  demo: TParallelDemo;
  timer: TPrecisionTimer;
begin
  PrintHeader('DEMO 2: Parallel Processing for Concurrent Operations');

  WriteLn('Creating parallel processor with 4 workers...');
  parallel := TSynParallelProcess.Create(4, 'demo', nil, nil);
  demo := TParallelDemo.Create;
  try
    demo.Completed := 0;
    WriteLn('Processing 100 tasks in parallel...');
    WriteLn;

    timer.Start;
    parallel.ParallelRunAndWait(demo.WorkerTask, 100);

    WriteLn(Format('Completed 100 tasks in %s', [timer.Stop]));
    WriteLn(Format('Average time per task: %.2f ms',
      [timer.TimeInMicroSec / 100 / 1000]));
    WriteLn(Format('Parallel processor efficiency: %d workers handled 100 tasks',
      [parallel.ThreadPoolCount]));
  finally
    demo.Free;
    parallel.Free;
  end;
end;

procedure DemoBatchDataProcessing;
var
  data: TRawUtf8DynArray;
  i: Integer;
  timer: TPrecisionTimer;
  json: RawUtf8;
begin
  PrintHeader('DEMO 3: Batch Data Processing');

  WriteLn('Generating 1,000 data items...');
  SetLength(data, 1000);
  for i := 0 to High(data) do
    data[i] := FormatUtf8('Item #%, Value: %', [i, Random(10000)]);
  WriteLn(Format('Generated %d items', [Length(data)]));
  WriteLn;

  // Batch to JSON
  WriteLn('Converting batch to JSON...');
  timer.Start;
  json := DynArraySaveJson(data, TypeInfo(TRawUtf8DynArray));
  WriteLn(Format('Conversion completed in %s', [timer.Stop]));
  WriteLn(Format('JSON size: %d bytes', [Length(json)]));
  WriteLn('Sample (first 200 chars): ', Copy(json, 1, 200), '...');
  WriteLn;

  // Batch processing with filter
  WriteLn('Filtering items containing "50"...');
  var filtered := 0;
  timer.Start;
  for i := 0 to High(data) do
    if PosEx('50', data[i]) > 0 then
      Inc(filtered);
  WriteLn(Format('Filtered %d items in %s', [filtered, timer.Stop]));
end;

procedure DemoPerformanceMonitoring;
var
  stats: TSynMonitor;
  timer: TPrecisionTimer;
begin
  PrintHeader('DEMO 4: Performance Monitoring');

  WriteLn('mORMot2 provides built-in performance monitoring:');
  WriteLn;

  stats := TSynMonitor.Create('demo');
  try
    WriteLn('Simulating monitored operations...');

    // Simulate fast operations
    for var i := 1 to 10 do
    begin
      timer.Start;
      Sleep(Random(5));
      stats.FromExternalMicroSeconds(timer.TimeInMicroSec);
    end;

    // Simulate slow operations
    for var i := 1 to 3 do
    begin
      timer.Start;
      Sleep(20 + Random(10));
      stats.FromExternalMicroSeconds(timer.TimeInMicroSec);
    end;

    WriteLn;
    WriteLn('Performance Statistics:');
    WriteLn(Format('  Total operations: %d', [stats.TaskCount]));
    WriteLn('  Success rate: 100%');
    WriteLn(Format('  Average time: %.2f ms', [stats.AverageTime.MicroSec / 1000]));
    WriteLn(Format('  Total time: %.2f ms', [stats.TotalTime.MicroSec / 1000]));

  finally
    stats.Free;
  end;
end;

procedure DemoMemoryEfficientOperations;
var
  buf: TSynTempBuffer;
  stream: TRawByteStringStream;
  timer: TPrecisionTimer;
  size: Integer;
begin
  PrintHeader('DEMO 5: Memory-Efficient Operations');

  WriteLn('TSynTempBuffer - Stack/heap automatic buffer management:');
  WriteLn;

  buf.Init(100000); // 100KB buffer
  try
    WriteLn(Format('Allocated %d bytes buffer', [buf.len]));

    // Fill buffer
    timer.Start;
    FillCharFast(buf.buf^, buf.len, ord('A'));
    WriteLn(Format('Filled buffer in %s', [timer.Stop]));

    // Process buffer
    size := 0;
    for var i := 0 to buf.len - 1 do
      if PByteArray(buf.buf)[i] = ord('A') then
        Inc(size);
    WriteLn(Format('Verified %d bytes', [size]));

  finally
    buf.Done; // Automatic cleanup
  end;

  WriteLn;
  WriteLn('TRawByteStringStream - Zero-copy string streaming:');
  stream := TRawByteStringStream.Create;
  try
    timer.Start;
    for var i := 1 to 1000 do
    begin
      var line := 'Line ' + Int32ToUtf8(i) + #13#10;
      stream.Write(pointer(line)^, Length(line));
    end;
    WriteLn(Format('Wrote 1,000 lines in %s', [timer.Stop]));
    WriteLn(Format('Stream size: %d bytes', [stream.Size]));
  finally
    stream.Free;
  end;
end;

procedure DemoDynamicArrays;
var
  arr: TInt64DynArray;
  timer: TPrecisionTimer;
  sum: Int64;
begin
  PrintHeader('DEMO 6: High-Performance Dynamic Arrays');

  WriteLn('mORMot2 dynamic array operations:');
  WriteLn;

  // Fast array operations
  timer.Start;
  SetLength(arr, 10000);
  for var i := 0 to High(arr) do
    arr[i] := i * 2;
  WriteLn(Format('Initialized 10,000 elements in %s', [timer.Stop]));

  // Fast search
  timer.Start;
  var idx := Int64ScanIndex(Pointer(arr), Length(arr), 5000);
  WriteLn(Format('Linear search completed in %s', [timer.Stop]));
  WriteLn(Format('Found value 5000 at index: %d', [idx]));
  WriteLn;

  // Fast aggregation
  timer.Start;
  sum := 0;
  for var i := 0 to High(arr) do
    Inc(sum, arr[i]);
  WriteLn(Format('Calculated sum of 10,000 elements in %s', [timer.Stop]));
  WriteLn(Format('Sum: %d', [sum]));
  WriteLn;

  // Fast filtering
  timer.Start;
  var filtered: TInt64DynArray;
  for var i := 0 to High(arr) do
    if arr[i] mod 100 = 0 then
      AddInt64(filtered, arr[i]);
  WriteLn(Format('Filtered array (divisible by 100) in %s', [timer.Stop]));
  WriteLn(Format('Filtered count: %d', [Length(filtered)]));
end;

procedure DemoConcurrentBatchProcessing;
var
  parallel: TSynParallelProcess;
  demo: TBatchProcessingDemo;
  timer: TPrecisionTimer;
  total: Integer;
begin
  PrintHeader('DEMO 7: Concurrent Batch Processing');

  WriteLn('Processing 100 batches (100 items each) concurrently...');
  WriteLn;

  parallel := TSynParallelProcess.Create(SystemInfo.dwNumberOfProcessors, 'batch', nil, nil);
  demo := TBatchProcessingDemo.Create;
  try
    FillCharFast(demo.Results, SizeOf(demo.Results), 0);

    timer.Start;
    parallel.ParallelRunAndWait(demo.ProcessBatch, 100);

    WriteLn(Format('Processed 10,000 items (100 batches) in %s', [timer.Stop]));
    WriteLn(Format('Using %d CPU cores', [parallel.ThreadPoolCount]));
    WriteLn;

    // Calculate total
    total := 0;
    for var i := 0 to High(demo.Results) do
      Inc(total, demo.Results[i]);
    WriteLn(Format('Total sum: %d', [total]));
    WriteLn(Format('Average per batch: %d', [total div 100]));

  finally
    demo.Free;
    parallel.Free;
  end;
end;

procedure DemoRealWorldScenario;
var
  parallel: TSynParallelProcess;
  demo: TRealWorldScenario;
  timer: TPrecisionTimer;
begin
  PrintHeader('DEMO 8: Real-World API Server Scenario');

  WriteLn('SCENARIO: High-volume REST API with concurrent request processing');
  WriteLn('Simulating 1,000 concurrent API requests...');
  WriteLn;

  parallel := TSynParallelProcess.Create(8, 'api', nil, nil); // 8 worker threads
  demo := TRealWorldScenario.Create;
  try
    demo.Processed := 0;

    timer.Start;
    parallel.ParallelRunAndWait(demo.ProcessRequest, 1000);

    WriteLn(Format('Processed 1,000 requests in %s', [timer.Stop]));
    WriteLn(Format('Throughput: %.0f requests/second',
      [1000.0 / (timer.TimeInMicroSec / 1000000)]));
    WriteLn(Format('Average response time: %.2f ms',
      [timer.TimeInMicroSec / 1000 / 1000]));
    WriteLn;
    WriteLn('PRODUCTION BENEFITS:');
    WriteLn('  - Parallel processing reuses worker threads (lower overhead)');
    WriteLn('  - Concurrent processing maximizes CPU utilization');
    WriteLn('  - Non-blocking architecture handles high load');
    WriteLn('  - Built-in monitoring tracks performance');

  finally
    demo.Free;
    parallel.Free;
  end;
end;

procedure RunAllDemos;
begin
  try
    DemoHighPerformanceStrings;
    DemoThreadPool;
    DemoBatchDataProcessing;
    DemoPerformanceMonitoring;
    DemoMemoryEfficientOperations;
    DemoDynamicArrays;
    DemoConcurrentBatchProcessing;
    DemoRealWorldScenario;

    PrintHeader('All Demos Complete');
    WriteLn('KEY FEATURES DEMONSTRATED:');
    WriteLn;
    WriteLn('mORMot2 Utilities:');
    WriteLn('  • High-performance string operations');
    WriteLn('  • Parallel processing for concurrent execution');
    WriteLn('  • Memory-efficient buffer management');
    WriteLn('  • Fast dynamic array operations');
    WriteLn('  • Built-in performance monitoring');
    WriteLn('  • Zero-copy streaming');
    WriteLn('  • Batch processing utilities');
    WriteLn;
    WriteLn('Perfect for:');
    WriteLn('  • High-volume REST APIs');
    WriteLn('  • Concurrent request processing');
    WriteLn('  • Large-scale data processing');
    WriteLn('  • Performance-critical applications');
    WriteLn('  • Memory-constrained environments');

  except
    on E: Exception do
    begin
      WriteLn('Demo error: ', E.Message);
      TSynLog.Add.Log(sllError, 'Demo error: %', [E.Message], E);
    end;
  end;
end;

end.
