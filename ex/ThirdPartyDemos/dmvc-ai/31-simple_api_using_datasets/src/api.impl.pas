unit api.impl;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.text,
  mormot.core.interfaces,
  mormot.core.variants,
  mormot.core.json,
  mormot.core.datetime,
  mormot.core.os,
  mormot.db.core,
  mormot.orm.core,
  mormot.soa.server,
  api.interfaces;

type
  /// Dataset-based API implementation using direct SQL
  // Similar to DMVC's dataset-based controller
  // Uses mORMot2's ExecuteJson and ExecuteList methods
  TCustomerDataApiService = class(TInjectableObjectRest, ICustomerDataApi)
  public
    function GetCustomers: TVariantDynArray;
    function GetCustomerById(id: Integer): Variant;
    function CreateCustomer(const code, description, city, note: RawUtf8;
      rating: Integer): Integer;
    function UpdateCustomer(id: Integer; const code, description, city,
      note: RawUtf8; rating: Integer): Boolean;
    function DeleteCustomer(id: Integer): Boolean;
    function GetCustomerStatistics: Variant;
    function GetCustomersWithDetails: TVariantDynArray;
    function DemoDocVariantPatterns: Variant;
  end;

implementation

{ TCustomerDataApiService }

function TCustomerDataApiService.GetCustomers: TVariantDynArray;
var
  json: RawUtf8;
begin
  // Execute SQL query and get result as JSON array
  // Similar to DMVC: lCustDM.MyQuery.Open('select * from customers')
  json := Server.Orm.ExecuteJson([],
    'SELECT RowID as ID, Code, CompanyName, City, Rating, Note ' +
    'FROM CustomerOrm ORDER BY Code');

  // Convert JSON array to variant array
  Result := JsonToVariantDynArray(json);
end;

function TCustomerDataApiService.GetCustomerById(id: Integer): Variant;
var
  json: RawUtf8;
begin
  // Execute SQL query with parameter using FormatUtf8
  json := Server.Orm.ExecuteJson([],
    FormatUtf8('SELECT RowID as ID, Code, CompanyName, City, Rating, Note ' +
    'FROM CustomerOrm WHERE RowID=%', [id]));

  // If no results, return null
  if (json = '') or (json = '[]') then
    Result := Null
  else
  begin
    // Extract first element from JSON array
    var arr := JsonToVariantDynArray(json);
    if Length(arr) > 0 then
      Result := arr[0]
    else
      Result := Null;
  end;
end;

function TCustomerDataApiService.CreateCustomer(const code, description, city,
  note: RawUtf8; rating: Integer): Integer;
var
  json: RawUtf8;
begin
  // Direct SQL INSERT using ExecuteFmt
  // Similar to DMVC: lCustDM.MyQuery.ExecSQL('INSERT INTO ...')
  Result := 0;

  // Use ExecuteFmt for INSERT with parameters
  Server.Orm.ExecuteFmt(
    'INSERT INTO CustomerOrm (Code, CompanyName, City, Rating, Note) ' +
    'VALUES (%, %, %, %, %)', [code, description, city, rating, note]);

  // Get last insert ID using SQLite function
  json := Server.Orm.ExecuteJson([], 'SELECT last_insert_rowid() as ID');
  if (json <> '') and (json <> '[]') then
  begin
    var arr := JsonToVariantDynArray(json);
    if Length(arr) > 0 then
      Result := DocVariantData(arr[0])^.I['ID'];
  end;
end;

function TCustomerDataApiService.UpdateCustomer(id: Integer; const code,
  description, city, note: RawUtf8; rating: Integer): Boolean;
var
  json: RawUtf8;
begin
  // Direct SQL UPDATE using ExecuteFmt
  Server.Orm.ExecuteFmt(
    'UPDATE CustomerOrm SET Code=%, CompanyName=%, City=%, Rating=%, Note=% ' +
    'WHERE RowID=%', [code, description, city, rating, note, id]);

  // Check if row was updated
  json := Server.Orm.ExecuteJson([], 'SELECT changes() as Changed');
  if (json <> '') and (json <> '[]') then
  begin
    var arr := JsonToVariantDynArray(json);
    if Length(arr) > 0 then
      Result := DocVariantData(arr[0])^.I['Changed'] > 0
    else
      Result := false;
  end
  else
    Result := false;
end;

function TCustomerDataApiService.DeleteCustomer(id: Integer): Boolean;
var
  json: RawUtf8;
begin
  // Direct SQL DELETE using ExecuteFmt
  Server.Orm.ExecuteFmt('DELETE FROM CustomerOrm WHERE RowID=%', [id]);

  // Check if row was deleted
  json := Server.Orm.ExecuteJson([], 'SELECT changes() as Changed');
  if (json <> '') and (json <> '[]') then
  begin
    var arr := JsonToVariantDynArray(json);
    if Length(arr) > 0 then
      Result := DocVariantData(arr[0])^.I['Changed'] > 0
    else
      Result := false;
  end
  else
    Result := false;
end;

function TCustomerDataApiService.GetCustomerStatistics: Variant;
var
  json: RawUtf8;
  arr: TVariantDynArray;
  doc: TDocVariantData;
  i: Integer;
  ratingCounts: TDocVariantData;
begin
  // Demonstrates TDocVariantData construction instead of just parsing JSON
  // This shows how to build complex nested objects programmatically

  doc.InitFast;  // Initialize as object with JSON_FAST options

  // Get total customer count
  json := Server.Orm.ExecuteJson([],
    'SELECT COUNT(*) as Total FROM CustomerOrm');
  arr := JsonToVariantDynArray(json);
  if Length(arr) > 0 then
    doc.I['totalCustomers'] := DocVariantData(arr[0])^.I['Total'];

  // Get counts by rating (demonstrates nested object creation)
  ratingCounts.InitFast;
  json := Server.Orm.ExecuteJson([],
    'SELECT Rating, COUNT(*) as Count FROM CustomerOrm GROUP BY Rating ORDER BY Rating');
  arr := JsonToVariantDynArray(json);
  for i := 0 to High(arr) do
    with DocVariantData(arr[i])^ do
      ratingCounts.I[FormatUtf8('rating_%', [I['Rating']])] := I['Count'];

  // Add nested object using O_[] property (creates if doesn't exist)
  doc.Value['countsByRating'] := variant(ratingCounts);

  // Get top 5 cities (demonstrates nested array creation)
  json := Server.Orm.ExecuteJson([],
    'SELECT City, COUNT(*) as Count FROM CustomerOrm WHERE City IS NOT NULL ' +
    'GROUP BY City ORDER BY Count DESC LIMIT 5');
  arr := JsonToVariantDynArray(json);

  // Create array using A_[] property
  doc.A_['topCities']^.InitFast(dvArray);
  for i := 0 to High(arr) do
  begin
    var cityInfo: TDocVariantData;
    cityInfo.InitFast;
    with DocVariantData(arr[i])^ do
    begin
      cityInfo.U['city'] := U['City'];
      cityInfo.I['count'] := I['Count'];
    end;
    doc.A_['topCities']^.AddItem(variant(cityInfo));
  end;

  // Add metadata with different typed properties
  doc.U['generatedAt'] := DateTimeToIso8601Text(NowUtc);
  doc.B['success'] := true;

  Result := variant(doc);
end;

function TCustomerDataApiService.GetCustomersWithDetails: TVariantDynArray;
var
  json: RawUtf8;
  arr: TVariantDynArray;
  i: Integer;
begin
  // Demonstrates enhancing query results with computed fields
  // Shows nested object/array manipulation on existing data

  json := Server.Orm.ExecuteJson([],
    'SELECT RowID as ID, Code, CompanyName, City, Rating, Note ' +
    'FROM CustomerOrm ORDER BY Code');
  arr := JsonToVariantDynArray(json);

  // Enhance each customer with additional computed fields
  for i := 0 to High(arr) do
  begin
    // Access using typed properties for better performance
    with DocVariantData(arr[i])^ do
    begin
      // Add computed fields using typed property access
      U['displayName'] := FormatUtf8('% - %', [U['Code'], U['CompanyName']]);
      B['isPremium'] := I['Rating'] >= 4;

      // Add nested tags array based on rating
      A_['tags']^.InitFast(dvArray);
      if I['Rating'] >= 4 then
        A_['tags']^.AddItem('premium');
      if I['Rating'] = 5 then
        A_['tags']^.AddItem('vip');
      if (U['City'] <> '') and (U['City'] <> 'null') then
        A_['tags']^.AddItem('has-location');

      // Add nested metadata object
      O_['metadata']^.InitFast;
      O_['metadata']^.U['source'] := 'dataset-api';
      O_['metadata']^.I['version'] := 1;
    end;
  end;

  Result := arr;
end;

function TCustomerDataApiService.DemoDocVariantPatterns: Variant;
var
  doc: TDocVariantData;
  examples: TDocVariantData;
begin
  // This method demonstrates various TDocVariant manipulation patterns
  // It serves as a reference for different ways to work with TDocVariant

  doc.InitFast;
  doc.U['title'] := 'TDocVariant Manipulation Patterns Demo';

  // Pattern 1: Creating object with InitObject
  examples.InitObject(['pattern', 'InitObject',
                       'description', 'Fast object creation from name/value pairs',
                       'code', 'Doc.InitObject([''key'', ''value''])']);
  doc.A_['patterns']^.AddItem(variant(examples));

  // Pattern 2: Typed property access (faster than variant access)
  examples.InitFast;
  examples.U['pattern'] := 'Typed Properties';
  examples.U['description'] := 'Use I[], U[], B[], D[] for better performance';
  examples.U['code'] := 'Doc.I[''count''] := 42; name := Doc.U[''name''];';
  doc.A_['patterns']^.AddItem(variant(examples));

  // Pattern 3: Nested object creation with O_[]
  examples.InitFast;
  examples.U['pattern'] := 'Nested Objects';
  examples.U['description'] := 'O_[] creates nested object if doesn''t exist';
  examples.U['code'] := 'Doc.O_[''address'']^.U[''city''] := ''Paris'';';
  doc.A_['patterns']^.AddItem(variant(examples));

  // Pattern 4: Nested array creation with A_[]
  examples.InitFast;
  examples.U['pattern'] := 'Nested Arrays';
  examples.U['description'] := 'A_[] creates nested array if doesn''t exist';
  examples.U['code'] := 'Doc.A_[''tags'']^.AddItems([''a'', ''b'']);';
  doc.A_['patterns']^.AddItem(variant(examples));

  // Pattern 5: Safe iteration with _Safe()
  examples.InitFast;
  examples.U['pattern'] := 'Safe Iteration';
  examples.U['description'] := '_Safe() prevents exceptions on non-TDocVariant';
  examples.U['code'] := 'with _Safe(v)^ do for i := 0 to Count-1 do ...';
  doc.A_['patterns']^.AddItem(variant(examples));

  // Pattern 6: Direct array access for performance
  examples.InitFast;
  examples.U['pattern'] := 'Direct Arrays';
  examples.U['description'] := 'Access Names[]/Values[] directly for best performance';
  examples.U['code'] := 'for i := 0 to Doc.Count-1 do writeln(Doc.Names[i]);';
  doc.A_['patterns']^.AddItem(variant(examples));

  // Add usage recommendations
  doc.O_['recommendations']^.InitFast;
  doc.O_['recommendations']^.U['forReading'] :=
    'Use typed properties (I[], U[]) when reading from ExecuteJson results';
  doc.O_['recommendations']^.U['forBuilding'] :=
    'Use TDocVariantData stack-allocated for building responses';
  doc.O_['recommendations']^.U['forPerformance'] :=
    'Access Names[]/Values[] directly in loops, avoid Value[] property';
  doc.O_['recommendations']^.U['forSafety'] :=
    'Use _Safe() when variant might not be TDocVariant';

  Result := variant(doc);
end;

end.
