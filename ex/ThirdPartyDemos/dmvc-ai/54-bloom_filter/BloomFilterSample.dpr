program BloomFilterSample;

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
  mormot.core.text,
  mormot.core.rtti,
  mormot.core.search,
  mormot.core.unicode;

procedure DemonstratBasicBloomFilter;
var
  bloom: TSynBloomFilter;
  i: integer;
  testWords: array[0..9] of RawUtf8;
  existingWords: array[0..4] of RawUtf8;
  notExistingWords: array[0..4] of RawUtf8;
  falsePositives: integer;
begin
  WriteLn;
  WriteLn('=======================================');
  WriteLn('Basic Bloom Filter Demo');
  WriteLn('=======================================');
  WriteLn;

  // Initialize test data
  testWords[0] := 'apple';
  testWords[1] := 'banana';
  testWords[2] := 'cherry';
  testWords[3] := 'date';
  testWords[4] := 'elderberry';
  testWords[5] := 'fig';
  testWords[6] := 'grape';
  testWords[7] := 'honeydew';
  testWords[8] := 'kiwi';
  testWords[9] := 'lemon';

  existingWords[0] := 'apple';
  existingWords[1] := 'cherry';
  existingWords[2] := 'elderberry';
  existingWords[3] := 'grape';
  existingWords[4] := 'lemon';

  notExistingWords[0] := 'mango';
  notExistingWords[1] := 'orange';
  notExistingWords[2] := 'peach';
  notExistingWords[3] := 'pear';
  notExistingWords[4] := 'watermelon';

  // Create bloom filter for 10 items with 1% false positive rate
  bloom := TSynBloomFilter.Create(10, 1.0);
  try
    WriteLn('Created Bloom Filter:');
    WriteLn('  Expected items: ', bloom.Size);
    WriteLn('  False positive rate: ', bloom.FalsePositivePercent:0:2, '%');
    WriteLn('  Bits allocated: ', bloom.Bits);
    WriteLn('  Hash functions: ', bloom.HashFunctions);
    WriteLn;

    // Insert words
    WriteLn('Inserting words:');
    for i := 0 to High(testWords) do
    begin
      bloom.Insert(testWords[i]);
      WriteLn('  [', i + 1, '] ', testWords[i]);
    end;
    WriteLn('  Total inserted: ', bloom.Inserted);
    WriteLn;

    // Test existing words (should all return true - no false negatives)
    WriteLn('Testing words that WERE inserted (should all exist):');
    for i := 0 to High(existingWords) do
    begin
      if bloom.MayExist(existingWords[i]) then
        WriteLn('  [✓] "', existingWords[i], '" - EXISTS (correct)')
      else
        WriteLn('  [✗] "', existingWords[i], '" - NOT FOUND (ERROR: false negative!)');
    end;
    WriteLn;

    // Test non-existing words (may have false positives)
    WriteLn('Testing words that were NOT inserted (may have false positives):');
    falsePositives := 0;
    for i := 0 to High(notExistingWords) do
    begin
      if bloom.MayExist(notExistingWords[i]) then
      begin
        WriteLn('  [!] "', notExistingWords[i], '" - EXISTS (false positive)');
        Inc(falsePositives);
      end
      else
        WriteLn('  [✓] "', notExistingWords[i], '" - NOT FOUND (correct)');
    end;
    WriteLn;
    WriteLn('False positives: ', falsePositives, '/', Length(notExistingWords),
      ' (', (falsePositives * 100.0 / Length(notExistingWords)):0:1, '%)');
    WriteLn;

  finally
    bloom.Free;
  end;
end;

procedure DemonstrateBloomFilterSerialization;
var
  bloom1, bloom2: TSynBloomFilter;
  saved: RawByteString;
  i: integer;
begin
  WriteLn;
  WriteLn('=======================================');
  WriteLn('Bloom Filter Serialization Demo');
  WriteLn('=======================================');
  WriteLn;

  // Create and populate first bloom filter
  bloom1 := TSynBloomFilter.Create(1000, 1.0);
  try
    WriteLn('Creating first bloom filter and inserting 100 items...');
    for i := 1 to 100 do
      bloom1.Insert(FormatUtf8('item-%', [i]));

    WriteLn('  Inserted: ', bloom1.Inserted);
    WriteLn('  Memory used: ', bloom1.Bits div 8, ' bytes');
    WriteLn;

    // Save to binary
    WriteLn('Serializing bloom filter...');
    saved := bloom1.SaveTo;
    WriteLn('  Serialized size: ', Length(saved), ' bytes');
    WriteLn;

  finally
    bloom1.Free;
  end;

  // Load into new bloom filter
  WriteLn('Creating second bloom filter from serialized data...');
  bloom2 := TSynBloomFilter.Create(saved);
  try
    WriteLn('  Loaded successfully');
    WriteLn('  Size: ', bloom2.Size);
    WriteLn('  Inserted count: ', bloom2.Inserted);
    WriteLn;

    // Verify some items
    WriteLn('Testing deserialized filter:');
    if bloom2.MayExist('item-1') then
      WriteLn('  [✓] "item-1" - EXISTS (correct)')
    else
      WriteLn('  [✗] "item-1" - NOT FOUND (ERROR!)');

    if bloom2.MayExist('item-50') then
      WriteLn('  [✓] "item-50" - EXISTS (correct)')
    else
      WriteLn('  [✗] "item-50" - NOT FOUND (ERROR!)');

    if bloom2.MayExist('item-100') then
      WriteLn('  [✓] "item-100" - EXISTS (correct)')
    else
      WriteLn('  [✗] "item-100" - NOT FOUND (ERROR!)');

    if bloom2.MayExist('item-999') then
      WriteLn('  [!] "item-999" - EXISTS (false positive)')
    else
      WriteLn('  [✓] "item-999" - NOT FOUND (correct)');

    WriteLn;

  finally
    bloom2.Free;
  end;
end;

procedure DemonstratePerformanceComparison;
var
  bloom: TSynBloomFilter;
  list: TRawUtf8DynArray;
  i, j: integer;
  startTicks: Int64;
  bloomTime, linearTime: Int64;
  itemCount: integer;
begin
  WriteLn;
  WriteLn('=======================================');
  WriteLn('Bloom Filter Performance Demo');
  WriteLn('=======================================');
  WriteLn;

  itemCount := 10000;

  // Create and populate bloom filter
  WriteLn('Creating bloom filter with ', itemCount, ' items...');
  bloom := TSynBloomFilter.Create(itemCount, 1.0);
  try
    for i := 1 to itemCount do
      bloom.Insert(FormatUtf8('user-%', [i]));

    WriteLn('  Memory used: ', bloom.Bits div 8, ' bytes (',
      (bloom.Bits div 8) / 1024:0:2, ' KB)');
    WriteLn;

    // Create regular array for comparison
    WriteLn('Creating regular array with same items...');
    SetLength(list, itemCount);
    for i := 0 to itemCount - 1 do
      list[i] := FormatUtf8('user-%', [i + 1]);
    WriteLn('  Memory used (estimated): ', itemCount * 20, ' bytes (',
      (itemCount * 20) / 1024:0:2, ' KB)');
    WriteLn;

    // Test bloom filter performance
    WriteLn('Testing 1000 lookups with Bloom Filter...');
    startTicks := GetTickCount64;
    for i := 1 to 1000 do
      bloom.MayExist(FormatUtf8('user-%', [Random(itemCount * 2) + 1]));
    bloomTime := GetTickCount64 - startTicks;
    WriteLn('  Time: ', bloomTime, ' ms');
    WriteLn;

    // Test linear search performance
    WriteLn('Testing 1000 lookups with Linear Search...');
    startTicks := GetTickCount64;
    for i := 1 to 1000 do
    begin
      for j := 0 to High(list) do
        if list[j] = FormatUtf8('user-%', [Random(itemCount * 2) + 1]) then
          break;
    end;
    linearTime := GetTickCount64 - startTicks;
    WriteLn('  Time: ', linearTime, ' ms');
    WriteLn;

    WriteLn('Performance Summary:');
    WriteLn('  Bloom Filter: ', bloomTime, ' ms');
    WriteLn('  Linear Search: ', linearTime, ' ms');
    if bloomTime > 0 then
      WriteLn('  Speedup: ', (linearTime / bloomTime):0:1, 'x faster')
    else
      WriteLn('  Speedup: >1000x faster');
    WriteLn;

  finally
    bloom.Free;
  end;
end;

procedure DemonstrateFalsePositiveRates;
var
  bloom1, bloom5, bloom10: TSynBloomFilter;
  i, fp1, fp5, fp10: integer;
  testCount: integer;
begin
  WriteLn;
  WriteLn('=======================================');
  WriteLn('False Positive Rate Comparison');
  WriteLn('=======================================');
  WriteLn;

  testCount := 1000;

  // Create bloom filters with different false positive rates
  bloom1 := TSynBloomFilter.Create(testCount, 1.0);
  bloom5 := TSynBloomFilter.Create(testCount, 5.0);
  bloom10 := TSynBloomFilter.Create(testCount, 10.0);
  try
    WriteLn('Creating filters with different false positive rates:');
    WriteLn;
    WriteLn('  1% FP:  Bits=', bloom1.Bits, ', Hash Functions=', bloom1.HashFunctions,
      ', Memory=', bloom1.Bits div 8, ' bytes');
    WriteLn('  5% FP:  Bits=', bloom5.Bits, ', Hash Functions=', bloom5.HashFunctions,
      ', Memory=', bloom5.Bits div 8, ' bytes');
    WriteLn('  10% FP: Bits=', bloom10.Bits, ', Hash Functions=', bloom10.HashFunctions,
      ', Memory=', bloom10.Bits div 8, ' bytes');
    WriteLn;

    // Insert same items in all filters
    WriteLn('Inserting ', testCount, ' items in each filter...');
    for i := 1 to testCount do
    begin
      bloom1.Insert(FormatUtf8('item-%', [i]));
      bloom5.Insert(FormatUtf8('item-%', [i]));
      bloom10.Insert(FormatUtf8('item-%', [i]));
    end;
    WriteLn;

    // Test with items that were NOT inserted
    WriteLn('Testing with ', testCount, ' items that were NOT inserted:');
    fp1 := 0;
    fp5 := 0;
    fp10 := 0;
    for i := testCount + 1 to testCount * 2 do
    begin
      if bloom1.MayExist(FormatUtf8('item-%', [i])) then
        Inc(fp1);
      if bloom5.MayExist(FormatUtf8('item-%', [i])) then
        Inc(fp5);
      if bloom10.MayExist(FormatUtf8('item-%', [i])) then
        Inc(fp10);
    end;

    WriteLn;
    WriteLn('Results:');
    WriteLn('  1% FP filter:  ', fp1, ' false positives (',
      (fp1 * 100.0 / testCount):0:2, '% actual vs 1% expected)');
    WriteLn('  5% FP filter:  ', fp5, ' false positives (',
      (fp5 * 100.0 / testCount):0:2, '% actual vs 5% expected)');
    WriteLn('  10% FP filter: ', fp10, ' false positives (',
      (fp10 * 100.0 / testCount):0:2, '% actual vs 10% expected)');
    WriteLn;

  finally
    bloom1.Free;
    bloom5.Free;
    bloom10.Free;
  end;
end;

begin
  // Configure logging
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;

  WriteLn('=======================================');
  WriteLn('mORMot2 Bloom Filter Sample');
  WriteLn('=======================================');
  WriteLn;
  WriteLn('This sample demonstrates:');
  WriteLn('  - Basic Bloom Filter operations (Insert/MayExist)');
  WriteLn('  - Serialization/deserialization (SaveTo/LoadFrom)');
  WriteLn('  - Performance comparison with linear search');
  WriteLn('  - False positive rate configuration');
  WriteLn('  - Memory efficiency');
  WriteLn;
  WriteLn('Bloom Filters are space-efficient probabilistic data structures');
  WriteLn('used to test set membership. They allow false positives but');
  WriteLn('never false negatives, making them ideal for caching and');
  WriteLn('pre-filtering expensive operations.');
  WriteLn('=======================================');

  try
    DemonstratBasicBloomFilter;
    DemonstrateBloomFilterSerialization;
    DemonstratePerformanceComparison;
    DemonstrateFalsePositiveRates;

    WriteLn;
    WriteLn('=======================================');
    WriteLn('Sample completed successfully!');
    WriteLn('=======================================');
    WriteLn;

  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      TSynLog.Add.Log(sllError, 'Sample error: %', [E.Message]);
      ExitCode := 1;
    end;
  end;

  {$ifdef OSWINDOWS}
  WriteLn('Press Enter to exit...');
  ReadLn;
  {$endif OSWINDOWS}
end.
