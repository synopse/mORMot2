unit api.impl;

interface

uses
  mormot.core.base,
  mormot.core.json,
  mormot.core.text,
  mormot.core.data,
  mormot.core.rtti,
  mormot.core.os,
  mormot.core.unicode,
  mormot.rest.core,
  mormot.rest.server,
  mormot.orm.core,
  api.interfaces,
  entities,
  System.SysUtils,
  System.Math,
  System.Classes;

type
  TRendersSample = class(TInterfacedObject, IRendersSample)
  private
    fRest: IRestOrm;
    function CreateSamplePerson(const aFirstName, aLastName: RawUtf8;
      const aDOB: TDateTime; aMarried: Boolean): TID;
    function CreateSampleCustomer(const aName, aContactFirst, aContactLast,
      aAddressLine1, aCity: RawUtf8): TID;
  public
    constructor Create(const aRest: IRestOrm);

    // IRendersSample implementation
    function GetPerson(id: TID): TPersonDTO;
    function GetPeople: TPersonDTODynArray;
    function GetCustomer(id: TID): TCustomerDTO;
    function GetCustomers: TCustomerDTODynArray;
    function GetPeopleWithMetadata: TPeopleWithMetadataDTO;
    function GetPersonAsText(id: TID): RawUtf8;
    function GetPeopleAsCSV: RawUtf8;
    procedure GetPeopleAsRawCSV;
    function GetSimpleArrays: TSimpleArraysDTO;
    function GetBinaryData(filename: RawUtf8): RawByteString;
    procedure GetRawBinaryData(filename: RawUtf8);
    function UploadBinaryData(const fieldname, filename, contenttype: RawUtf8;
      const data: RawByteString): RawUtf8;
  end;

implementation

const
  // Content-Type headers for raw responses
  CSV_CONTENT_TYPE_HEADER = 'Content-Type: text/csv';

{ TRendersSample }

constructor TRendersSample.Create(const aRest: IRestOrm);
begin
  inherited Create;
  fRest := aRest;

  // Create sample data if database is empty
  if fRest.TableRowCount(TOrmPerson) = 0 then
  begin
    CreateSamplePerson('Daniele', 'Teti', EncodeDate(1979, 11, 4), True);
    CreateSamplePerson('John', 'Doe', EncodeDate(1879, 10, 2), False);
    CreateSamplePerson('Jane', 'Doe', EncodeDate(1883, 1, 5), True);
  end;

  if fRest.TableRowCount(TOrmCustomer) = 0 then
  begin
    CreateSampleCustomer('bit Time Professionals', 'Daniele', 'Teti',
      'Via di Valle Morta 10', 'Rome, IT');
    CreateSampleCustomer('Stark Industries', 'Tony', 'Stark',
      'Superhero Street 555', 'Palo Alto, CA');
    CreateSampleCustomer('Google Inc', 'Larry', 'Page',
      '', 'Mountain View, CA');
  end;
end;

function TRendersSample.CreateSamplePerson(const aFirstName, aLastName: RawUtf8;
  const aDOB: TDateTime; aMarried: Boolean): TID;
var
  person: TOrmPerson;
begin
  person := TOrmPerson.Create;
  try
    person.FirstName := aFirstName;
    person.LastName := aLastName;
    person.DOB := aDOB;
    person.Married := aMarried;
    Result := fRest.Add(person, True);
  finally
    person.Free;
  end;
end;

function TRendersSample.CreateSampleCustomer(const aName, aContactFirst,
  aContactLast, aAddressLine1, aCity: RawUtf8): TID;
var
  customer: TOrmCustomer;
begin
  customer := TOrmCustomer.Create;
  try
    customer.Name := aName;
    customer.ContactFirst := aContactFirst;
    customer.ContactLast := aContactLast;
    customer.AddressLine1 := aAddressLine1;
    customer.City := aCity;
    Result := fRest.Add(customer, True);
  finally
    customer.Free;
  end;
end;

function TRendersSample.GetPerson(id: TID): TPersonDTO;
var
  person: TOrmPerson;
begin
  person := TOrmPerson.Create(fRest, id);
  try
    if person.ID = 0 then
      raise ERestException.CreateUtf8('Person with ID % not found', [id]);

    Result.id := person.ID;
    Result.firstname := person.FirstName;
    Result.lastname := person.LastName;
    Result.dob := person.DOB;
    Result.married := person.Married;
  finally
    person.Free;
  end;
end;

function TRendersSample.GetPeople: TPersonDTODynArray;
var
  people: TOrmPersonObjArray;
  i: PtrInt;
begin
  if fRest.RetrieveListObjArray(people, TOrmPerson, '', []) then
  try
    SetLength(Result, Length(people));
    for i := 0 to High(people) do
    begin
      Result[i].id := people[i].ID;
      Result[i].firstname := people[i].FirstName;
      Result[i].lastname := people[i].LastName;
      Result[i].dob := people[i].DOB;
      Result[i].married := people[i].Married;
    end;
  finally
    ObjArrayClear(people);
  end;
end;

function TRendersSample.GetCustomer(id: TID): TCustomerDTO;
var
  customer: TOrmCustomer;
begin
  customer := TOrmCustomer.Create(fRest, id);
  try
    if customer.ID = 0 then
      raise ERestException.CreateUtf8('Customer with ID % not found', [id]);

    Result.id := customer.ID;
    Result.name := customer.Name;
    Result.contactfirst := customer.ContactFirst;
    Result.contactlast := customer.ContactLast;
    Result.addressline1 := customer.AddressLine1;
    Result.city := customer.City;
  finally
    customer.Free;
  end;
end;

function TRendersSample.GetCustomers: TCustomerDTODynArray;
var
  customers: TOrmCustomerObjArray;
  i: PtrInt;
begin
  if fRest.RetrieveListObjArray(customers, TOrmCustomer, '', []) then
  try
    SetLength(Result, Length(customers));
    for i := 0 to High(customers) do
    begin
      Result[i].id := customers[i].ID;
      Result[i].name := customers[i].Name;
      Result[i].contactfirst := customers[i].ContactFirst;
      Result[i].contactlast := customers[i].ContactLast;
      Result[i].addressline1 := customers[i].AddressLine1;
      Result[i].city := customers[i].City;
    end;
  finally
    ObjArrayClear(customers);
  end;
end;

function TRendersSample.GetPeopleWithMetadata: TPeopleWithMetadataDTO;
var
  startTime: TDateTime;
begin
  startTime := Now;

  // Get people data
  Result.items := GetPeople;

  // Add metadata
  Result.metadata.startProcessing := startTime;
  Result.metadata.stopProcessing := Now;
  Result.metadata.customData := FormatUtf8('There are % people in the list',
    [Length(Result.items)]);
end;

function TRendersSample.GetPersonAsText(id: TID): RawUtf8;
var
  person: TOrmPerson;
begin
  person := TOrmPerson.Create(fRest, id);
  try
    if person.ID = 0 then
      raise ERestException.CreateUtf8('Person with ID % not found', [id]);

    Result := FormatUtf8('ID        : %'#13#10 +
                         'FirstName : %'#13#10 +
                         'LastName  : %'#13#10 +
                         'DOB       : %'#13#10 +
                         'Married   : %',
      [person.ID, person.FirstName, person.LastName,
       DateToStr(person.DOB), BoolToStr(person.Married, True)]);
  finally
    person.Free;
  end;
end;

function TRendersSample.GetPeopleAsCSV: RawUtf8;
var
  people: TOrmPersonObjArray;
  i: PtrInt;
  csv: RawUtf8;
begin
  csv := 'first_name;last_name;dob;married'#13#10;

  if fRest.RetrieveListObjArray(people, TOrmPerson, '', []) then
  try
    for i := 0 to High(people) do
    begin
      csv := csv + FormatUtf8('%;%;%;%'#13#10,
        [people[i].FirstName, people[i].LastName,
         DateToStr(people[i].DOB), BoolToStr(people[i].Married, True)]);
    end;
  finally
    ObjArrayClear(people);
  end;

  Result := csv;
end;

function TRendersSample.GetSimpleArrays: TSimpleArraysDTO;
var
  i: Integer;
begin
  // Create sample integer array
  SetLength(Result.integers, 5);
  for i := 0 to 4 do
    Result.integers[i] := i;

  // Create sample string array
  SetLength(Result.strings, 5);
  for i := 0 to 4 do
    Result.strings[i] := FormatUtf8('Value %', [i]);

  // Create sample double array
  SetLength(Result.doubles, 5);
  for i := 0 to 4 do
    Result.doubles[i] := Power(i, i) * 1.1;
end;

function TRendersSample.GetBinaryData(filename: RawUtf8): RawByteString;
var
  filePath: TFileName;
begin
  // For demonstration, create a simple binary file
  // In production, you would read from a real file
  filePath := Utf8ToString(filename);

  if not FileExists(filePath) then
    raise ERestException.CreateUtf8('File % not found', [filename]);

  Result := StringFromFile(filePath);
end;

function TRendersSample.UploadBinaryData(const fieldname, filename,
  contenttype: RawUtf8; const data: RawByteString): RawUtf8;
var
  outputFolder: TFileName;
  outputPath: TFileName;
  randomName: RawUtf8;
begin
  // Create upload directory if it doesn't exist
  outputFolder := ExpandFileName('uploadedfiles');
  if not DirectoryExists(outputFolder) then
    CreateDir(outputFolder);

  // Generate random filename
  randomName := CardinalToHexLower(Random32);
  outputPath := outputFolder + PathDelim + Utf8ToString(randomName) +
    ExtractFileExt(Utf8ToString(filename));

  // Save file
  FileFromString(data, outputPath);

  // Return file reference
  Result := FormatUtf8('{"filename":"%","ref":"/files/%"}',
    [ExtractFileName(outputPath), ExtractFileName(outputPath)]);
end;

procedure TRendersSample.GetPeopleAsRawCSV;
var
  people: TOrmPersonObjArray;
  i: PtrInt;
  csv: RawUtf8;
  ctxt: TServiceRunningContext;
begin
  // Build CSV content
  csv := 'first_name;last_name;dob;married'#13#10;

  if fRest.RetrieveListObjArray(people, TOrmPerson, '', []) then
  try
    for i := 0 to High(people) do
    begin
      csv := csv + FormatUtf8('%;%;%;%'#13#10,
        [people[i].FirstName, people[i].LastName,
         DateToStr(people[i].DOB), BoolToStr(people[i].Married, True)]);
    end;
  finally
    ObjArrayClear(people);
  end;

  // CORRECT: Use Ctxt.Returns for raw CSV with proper content-type
  // This is the idiomatic way to return non-JSON content from mORMot2 services
  ctxt := CurrentServiceContext;
  if ctxt.Request <> nil then
    ctxt.Request.Returns(csv, HTTP_SUCCESS, CSV_CONTENT_TYPE_HEADER);
end;

procedure TRendersSample.GetRawBinaryData(filename: RawUtf8);
var
  filePath: TFileName;
  data: RawByteString;
  ctxt: TServiceRunningContext;
  contentTypeHeader: RawUtf8;
begin
  // For demonstration, read a real binary file
  filePath := Utf8ToString(filename);

  if not FileExists(filePath) then
    raise ERestException.CreateUtf8('File % not found', [filename]);

  data := StringFromFile(filePath);

  // Determine content type based on file extension
  if EndWith(filename, '.PNG') then
    contentTypeHeader := 'Content-Type: image/png'
  else if EndWith(filename, '.JPG') or EndWith(filename, '.JPEG') then
    contentTypeHeader := 'Content-Type: image/jpeg'
  else if EndWith(filename, '.PDF') then
    contentTypeHeader := 'Content-Type: application/pdf'
  else
    contentTypeHeader := BINARY_CONTENT_TYPE_HEADER;

  // CORRECT: Use Ctxt.Returns for raw binary with proper content-type
  // This sends actual binary data, not base64-encoded JSON
  ctxt := CurrentServiceContext;
  if ctxt.Request <> nil then
    ctxt.Request.Returns(data, HTTP_SUCCESS, contentTypeHeader);
end;

end.
