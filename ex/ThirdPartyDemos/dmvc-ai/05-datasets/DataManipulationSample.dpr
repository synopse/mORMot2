program DataManipulationSample;

{$APPTYPE CONSOLE}

{$I mormot.defines.inc}

uses
  {$I mormot.uses.inc}
  System.SysUtils,
  System.Classes,
  Data.DB,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.json,
  mormot.core.data,
  mormot.core.rtti,
  mormot.core.variants,
  mormot.net.sock,
  mormot.net.client,
  mormot.net.http,
  mormot.db.rad.ui;

type
  // Record type for Todo items (from jsonplaceholder API)
  TTodo = packed record
    id: integer;
    userId: integer;
    title: RawUtf8;
    completed: boolean;
  end;
  TTodoArray = array of TTodo;

  // Record type for Customer data
  TCustomer = packed record
    CustNo: integer;
    Customer: RawUtf8;
    ContactFirst: RawUtf8;
    ContactLast: RawUtf8;
    AddressLine1: RawUtf8;
    AddressLine2: RawUtf8;
    City: RawUtf8;
    StateProvince: RawUtf8;
    Country: RawUtf8;
    PostalCode: RawUtf8;
    PhoneNo: RawUtf8;
    OnHold: RawUtf8;
  end;
  TCustomerArray = array of TCustomer;

procedure Demo1_LoadFromAPI;
var
  http: THttpClientSocket;
  json: RawUtf8;
  todos: TTodoArray;
  i, maxIdx, status: integer;
  doc: TDocVariantData;
begin
  WriteLn;
  WriteLn('=== DEMO 1: Load Data from REST API ===');
  WriteLn;

  http := THttpClientSocket.Open('jsonplaceholder.typicode.com', '443', nlTcp, 10000, true);
  try
    WriteLn('Fetching todos from https://jsonplaceholder.typicode.com/todos...');

    // Make HTTP GET request
    status := http.Get('/todos', 0);
    if status = HTTP_SUCCESS then
    begin
      json := http.Content;

      // Parse JSON array into dynamic array of records
      DynArrayLoadJson(todos, json, TypeInfo(TTodoArray));

      WriteLn(Format('Loaded %d todos', [Length(todos)]));
      WriteLn;
      WriteLn('First 5 todos:');
      WriteLn('================');

      if High(todos) < 4 then
        maxIdx := High(todos)
      else
        maxIdx := 4;
      for i := 0 to maxIdx do
      begin
        WriteLn(Format('ID: %d | UserID: %d | Completed: %s',
          [todos[i].id, todos[i].userId, BoolToStr(todos[i].completed, true)]));
        WriteLn(Format('  Title: %s', [Utf8ToString(todos[i].title)]));
        WriteLn;
      end;

      // Alternative: Load as TDocVariant for dynamic access
      doc.InitJson(json, JSON_FAST);
      WriteLn(Format('Total items via TDocVariant: %d', [doc.Count]));
      WriteLn(Format('First item title: %s', [doc.Values[0].U['title']]));
    end
    else
      WriteLn('HTTP request failed with status: ', status);
  finally
    http.Free;
  end;
end;

procedure Demo2_FilterAndTransform;
var
  customers: TCustomerArray;
  filtered: TCustomerArray;
  i, count: integer;
begin
  WriteLn;
  WriteLn('=== DEMO 2: Filter and Transform Data ===');
  WriteLn;

  // Create sample customer data
  SetLength(customers, 5);

  customers[0].CustNo := 1001;
  customers[0].Customer := 'Acme Corporation';
  customers[0].ContactFirst := 'John';
  customers[0].ContactLast := 'Smith';
  customers[0].City := 'New York';
  customers[0].Country := 'USA';
  customers[0].OnHold := 'No';

  customers[1].CustNo := 1002;
  customers[1].Customer := 'Global Industries';
  customers[1].ContactFirst := 'Jane';
  customers[1].ContactLast := 'Doe';
  customers[1].City := 'London';
  customers[1].Country := 'UK';
  customers[1].OnHold := 'Yes';

  customers[2].CustNo := 1003;
  customers[2].Customer := 'Tech Solutions';
  customers[2].ContactFirst := 'Bob';
  customers[2].ContactLast := 'Johnson';
  customers[2].City := 'Paris';
  customers[2].Country := 'France';
  customers[2].OnHold := 'No';

  customers[3].CustNo := 1004;
  customers[3].Customer := 'Innovation Labs';
  customers[3].ContactFirst := 'Alice';
  customers[3].ContactLast := 'Williams';
  customers[3].City := 'Berlin';
  customers[3].Country := 'Germany';
  customers[3].OnHold := 'No';

  customers[4].CustNo := 1005;
  customers[4].Customer := 'Digital Ventures';
  customers[4].ContactFirst := 'Charlie';
  customers[4].ContactLast := 'Brown';
  customers[4].City := 'Madrid';
  customers[4].Country := 'Spain';
  customers[4].OnHold := 'Yes';

  WriteLn(Format('Total customers: %d', [Length(customers)]));
  WriteLn;

  // Filter: Only customers not on hold
  count := 0;
  SetLength(filtered, Length(customers));
  for i := 0 to High(customers) do
    if customers[i].OnHold = 'No' then
    begin
      filtered[count] := customers[i];
      Inc(count);
    end;
  SetLength(filtered, count);

  WriteLn(Format('Customers not on hold: %d', [Length(filtered)]));
  WriteLn;
  for i := 0 to High(filtered) do
  begin
    WriteLn(Format('%d. %s - %s, %s',
      [filtered[i].CustNo,
       Utf8ToString(filtered[i].Customer),
       Utf8ToString(filtered[i].City),
       Utf8ToString(filtered[i].Country)]));
  end;
end;

procedure Demo3_JsonSerialization;
var
  customers: TCustomerArray;
  json: RawUtf8;
  restored: TCustomerArray;
  i: integer;
  fileStream: TFileStream;
begin
  WriteLn;
  WriteLn('=== DEMO 3: JSON Serialization ===');
  WriteLn;

  // Create sample data
  SetLength(customers, 3);

  customers[0].CustNo := 2001;
  customers[0].Customer := 'Sunrise Corp';
  customers[0].ContactFirst := 'Michael';
  customers[0].ContactLast := 'Anderson';
  customers[0].AddressLine1 := '123 Main St';
  customers[0].City := 'Boston';
  customers[0].Country := 'USA';
  customers[0].PostalCode := '02101';
  customers[0].PhoneNo := '+1-555-0101';

  customers[1].CustNo := 2002;
  customers[1].Customer := 'European Trading';
  customers[1].ContactFirst := 'Sophie';
  customers[1].ContactLast := 'Martinez';
  customers[1].AddressLine1 := '45 Rue de la Paix';
  customers[1].City := 'Paris';
  customers[1].Country := 'France';
  customers[1].PostalCode := '75002';
  customers[1].PhoneNo := '+33-1-42-86-82-00';

  customers[2].CustNo := 2003;
  customers[2].Customer := 'Asia Pacific Ltd';
  customers[2].ContactFirst := 'Yuki';
  customers[2].ContactLast := 'Tanaka';
  customers[2].AddressLine1 := '7-3-1 Ginza';
  customers[2].City := 'Tokyo';
  customers[2].Country := 'Japan';
  customers[2].PostalCode := '104-0061';
  customers[2].PhoneNo := '+81-3-5537-1111';

  // Serialize to JSON
  json := DynArraySaveJson(customers, TypeInfo(TCustomerArray));

  WriteLn('Serialized to JSON:');
  WriteLn(Utf8ToString(json));
  WriteLn;

  // Save to file
  fileStream := TFileStream.Create('customers.json', fmCreate);
  try
    fileStream.Write(pointer(json)^, Length(json));
    WriteLn('Saved to customers.json');
  finally
    fileStream.Free;
  end;
  WriteLn;

  // Deserialize from JSON
  DynArrayLoadJson(restored, json, TypeInfo(TCustomerArray));

  WriteLn(Format('Restored %d customers from JSON:', [Length(restored)]));
  for i := 0 to High(restored) do
  begin
    WriteLn(Format('  %d: %s (%s, %s)',
      [restored[i].CustNo,
       Utf8ToString(restored[i].Customer),
       Utf8ToString(restored[i].City),
       Utf8ToString(restored[i].Country)]));
  end;
end;

(*  // TODO: Fix Demo4 - has quote parsing error
procedure Demo4_DocVariantManipulation;
var
  doc: TDocVariantData;
  items: variant;
  i: integer;
begin
  WriteLn;
  WriteLn('=== DEMO 4: TDocVariant Manipulation ===');
  WriteLn;

  // Create document with array
  doc.InitFast(dvArray);

  // Add items
  doc.AddItem(_ObjFast([
    'id', 1,
    'name', 'Product A',
    'price', 99.99,
    'inStock', true
  ]));

  doc.AddItem(_ObjFast([
    'id', 2,
    'name', 'Product B',
    'price', 149.50,
    'inStock', false
  ]));

  doc.AddItem(_ObjFast([
    'id', 3,
    'name', 'Product C',
    'price', 79.99,
    'inStock', true
  ]));

  WriteLn('Created product catalog:');
  WriteLn(Utf8ToString(doc.ToJson));
  WriteLn;

  // Iterate and filter
  WriteLn('Products in stock:');
  items := doc.Value;
  for i := 0 to doc.Count - 1 do
  begin
    if _Safe(items[i]).B['inStock'] then
      WriteLn(Format('  - %s: $%.2f',
        [_Safe(items[i]).U['name'],
         _Safe(items[i]).D['price']]));
  end;

  WriteLn;
  WriteLn('Total items: ', doc.Count);
end;
*)

procedure Demo5_PagingAndSorting;
var
  allItems: TTodoArray;
  page: TTodoArray;
  pageSize, pageNum, startIdx, endIdx: integer;
  i, j: integer;
  temp: TTodo;
begin
  WriteLn;
  WriteLn('=== DEMO 5: Paging and Sorting ===');
  WriteLn;

  // Create sample todo items
  SetLength(allItems, 20);
  for i := 0 to High(allItems) do
  begin
    allItems[i].id := i + 1;
    allItems[i].userId := (i mod 5) + 1;
    allItems[i].title := FormatUtf8('Task #%', [i + 1]);
    allItems[i].completed := (i mod 3) = 0;
  end;

  WriteLn(Format('Created %d todo items', [Length(allItems)]));
  WriteLn;

  // Simple bubble sort by userId
  WriteLn('Sorting by userId...');
  for i := 0 to High(allItems) - 1 do
    for j := i + 1 to High(allItems) do
      if allItems[i].userId > allItems[j].userId then
      begin
        temp := allItems[i];
        allItems[i] := allItems[j];
        allItems[j] := temp;
      end;

  // Implement paging
  pageSize := 5;
  pageNum := 2; // Get second page (0-based)

  startIdx := pageNum * pageSize;
  if (startIdx + pageSize - 1) < High(allItems) then
    endIdx := startIdx + pageSize - 1
  else
    endIdx := High(allItems);

  SetLength(page, endIdx - startIdx + 1);
  for i := startIdx to endIdx do
    page[i - startIdx] := allItems[i];

  WriteLn(Format('Page %d (items %d-%d):', [pageNum + 1, startIdx + 1, endIdx + 1]));
  WriteLn('================================');
  for i := 0 to High(page) do
  begin
    WriteLn(Format('ID: %d | UserID: %d | Completed: %s',
      [page[i].id, page[i].userId, BoolToStr(page[i].completed, true)]));
    WriteLn(Format('  Title: %s', [Utf8ToString(page[i].title)]));
  end;

  WriteLn;
  WriteLn(Format('Total pages: %d', [(Length(allItems) + pageSize - 1) div pageSize]));
end;

procedure Demo6_VirtualDataSet;
var
  http: THttpClientSocket;
  json: RawUtf8;
  doc: TDocVariantData;
  dataSet: TDocVariantArrayDataSet;
  status: integer;
  i: integer;
begin
  WriteLn;
  WriteLn('=== DEMO 6: TVirtualDataSet Integration ===');
  WriteLn;

  // Fetch data from REST API
  http := THttpClientSocket.Open('jsonplaceholder.typicode.com', '443', nlTcp, 10000, true);
  try
    WriteLn('Fetching users from https://jsonplaceholder.typicode.com/users...');
    status := http.Get('/users', 0);

    if status = HTTP_SUCCESS then
    begin
      json := http.Content;

      // Parse JSON into TDocVariant
      doc.InitJson(json, JSON_FAST);
      WriteLn(Format('Loaded %d users', [doc.Count]));
      WriteLn;

      // Convert TDocVariant to TDataSet using DocVariantToDataSet
      dataSet := DocVariantToDataSet(nil, variant(doc));
      try
        WriteLn('Created TVirtualDataSet from TDocVariant');
        WriteLn(Format('Dataset RecordCount: %d', [dataSet.RecordCount]));
        WriteLn;

        // Demonstrate dataset navigation
        WriteLn('Iterating through dataset:');
        WriteLn('==========================');
        dataSet.First;
        i := 0;
        while (not dataSet.Eof) and (i < 5) do
        begin
          WriteLn(Format('Record %d:', [dataSet.RecNo]));
          WriteLn(Format('  ID: %s', [dataSet.FieldByName('id').AsString]));
          WriteLn(Format('  Name: %s', [dataSet.FieldByName('name').AsString]));
          WriteLn(Format('  Username: %s', [dataSet.FieldByName('username').AsString]));
          WriteLn(Format('  Email: %s', [dataSet.FieldByName('email').AsString]));
          WriteLn;
          dataSet.Next;
          Inc(i);
        end;

        // Demonstrate Locate functionality
        WriteLn('Testing Locate functionality:');
        if dataSet.Locate('id', 5, []) then
        begin
          WriteLn(Format('Found record with ID=5: %s',
            [dataSet.FieldByName('name').AsString]));
        end;
        WriteLn;

        // Show field information
        WriteLn(Format('Dataset has %d fields:', [dataSet.FieldCount]));
        for i := 0 to dataSet.FieldCount - 1 do
        begin
          WriteLn(Format('  Field %d: %s (size=%d)',
            [i, dataSet.Fields[i].FieldName, dataSet.Fields[i].Size]));
        end;
        WriteLn;

        WriteLn('Benefits of TVirtualDataSet:');
        WriteLn('- Compatible with VCL/FMX data-aware controls (TDBGrid, etc.)');
        WriteLn('- Read-only, lightweight, no buffer overhead');
        WriteLn('- Direct access to TDocVariant data (no copying)');
        WriteLn('- Supports Locate, navigation (First/Next/Prior/Last)');
        WriteLn('- Perfect bridge between mORMot2 JSON and legacy UI code');

      finally
        dataSet.Free;
      end;
    end
    else
      WriteLn('HTTP request failed with status: ', status);
  finally
    http.Free;
  end;
end;

begin
  try
    WriteLn('mORMot2 Data Manipulation Sample');
    WriteLn('=================================');
    WriteLn('Demonstrating: Arrays, JSON, REST API, Filtering, Paging');
    WriteLn;

    Demo1_LoadFromAPI;
    Demo2_FilterAndTransform;
    Demo3_JsonSerialization;
    // Demo4_DocVariantManipulation; // TODO: Fix quote parsing error
    Demo5_PagingAndSorting;
    Demo6_VirtualDataSet;

    WriteLn;
    WriteLn('All demos completed successfully!');
    WriteLn;
    WriteLn('Press Enter to exit...');
    ReadLn;
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.ClassName, ': ', E.Message);
      WriteLn;
      WriteLn('Press Enter to exit...');
      ReadLn;
      ExitCode := 1;
    end;
  end;
end.
