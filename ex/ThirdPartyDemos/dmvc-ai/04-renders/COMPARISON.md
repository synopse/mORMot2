# DMVCframework vs mORMot2 - Renders Comparison

This document compares the renders sample implementation between DMVCframework and mORMot2.

## Architecture Differences

### DMVCframework

```
renders/
├── RenderSampleControllerU.pas    # Controller with Render() methods
├── BusinessObjectsU.pas            # Business objects (classes)
├── CustomTypesU.pas                # Custom types
├── InMemoryDataU.pas               # In-memory data generation
├── MyDataModuleU.pas/dfm           # DataModule with datasets
└── renders.dpr                     # Main program
```

### mORMot2

```
04-renders/
├── src/
│   ├── entities.pas                # ORM entities
│   ├── api.interfaces.pas          # Interface + DTOs (records)
│   ├── api.impl.pas                # Implementation
│   └── server.pas                  # Server setup
└── RendersSample.dpr               # Main program
```

## Code Comparison

### 1. Returning a Single Object

#### DMVCframework

```pascal
// Controller method
[MVCHTTPMethod([httpGET])]
[MVCPath('/people/($ID)')]
[MVCProduces('application/json')]
procedure GetPersonById(const ID: Integer);

// Implementation
procedure TRenderSampleController.GetPersonById(const ID: Integer);
var
  lPerson: TPerson;
begin
  lPerson := TPerson.Create;
  try
    lPerson.ID := ID;
    lPerson.FirstName := 'Daniele';
    lPerson.LastName := 'Teti';
    lPerson.DOB := EncodeDate(1979, 11, 4);
    lPerson.Married := True;
    Render(lPerson, False,  // False = don't free
      procedure(const AObject: TObject; const Links: IMVCLinks)
      begin
        Links.AddRefLink.Add(HATEOAS.HREF, '/people/' + TPerson(AObject).ID.ToString)
          .Add(HATEOAS.REL, 'self').Add(HATEOAS._TYPE, TMVCMediaType.APPLICATION_JSON);
      end);
  finally
    lPerson.Free;
  end;
end;
```

#### mORMot2

```pascal
// Interface definition
IRendersSample = interface(IInvokable)
  ['{A8F2C3D4-E5B6-47A8-89C0-1D2E3F4A5B6C}']
  function GetPerson(id: TID): TPersonDTO;
end;

// Implementation
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
```

**Key Differences:**
- DMVCframework: Uses `Render()` method with attributes
- mORMot2: Returns typed record, automatic JSON serialization
- DMVCframework: HATEOAS links added via callback
- mORMot2: Cleaner, no explicit rendering needed

### 2. Returning an Array

#### DMVCframework

```pascal
[MVCHTTPMethod([httpGET])]
[MVCPath('/people')]
[MVCProduces('application/json')]
procedure GetPeople_AsObjectList;

procedure TRenderSampleController.GetPeople_AsObjectList;
var
  People: TObjectList<TPerson>;
begin
  People := TObjectList<TPerson>.Create(True);
  People.Add(TPerson.GetNew('Daniele','Teti', EncodeDate(1979, 11, 4), True));
  People.Add(TPerson.GetNew('John','Doe', EncodeDate(1879, 10, 2), False));
  People.Add(TPerson.GetNew('Jane','Doe', EncodeDate(1883, 1, 5), True));

  // New approach with ObjectDict
  Render(HTTP_STATUS.OK, ObjectDict().Add('data', People));
end;
```

#### mORMot2

```pascal
function GetPeople: TPersonDTODynArray;

function TRendersSample.GetPeople: TPersonDTODynArray;
var
  people: TOrmPersonObjArray;
  i: PtrInt;
begin
  if fRest.RetrieveListObjArray(people, TOrmPerson, '') then
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
```

**Key Differences:**
- DMVCframework: Uses TObjectList<T>, requires Render() call
- mORMot2: Uses dynamic array of records, automatic serialization
- DMVCframework: ObjectDict wrapper for response structure
- mORMot2: Direct array return, mORMot wraps in {"result":[...]}

### 3. Plain Text Response

#### DMVCframework

```pascal
[MVCHTTPMethod([httpGET])]
[MVCPath('/customers/($ID)')]
[MVCProduces('text/plain')]
procedure GetPerson_AsText(const ID: Integer);

procedure TRenderSampleController.GetPerson_AsText(const ID: Integer);
begin
  ResponseStream
    .AppendLine('ID        :  ' + ID.ToString)
    .AppendLine('FirstName : Daniele')
    .AppendLine('LastName  : Teti')
    .AppendLine('DOB       : ' + DateToStr(EncodeDate(1979, 5, 2)))
    .AppendLine('Married   : yes');
  RenderResponseStream;
end;
```

#### mORMot2

```pascal
function GetPersonAsText(id: TID): RawUtf8;

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
```

**Key Differences:**
- DMVCframework: Uses ResponseStream builder pattern
- mORMot2: Returns RawUtf8 string directly
- DMVCframework: Explicit content-type via attribute
- mORMot2: Content-type inferred from return type

### 4. CSV Response

#### DMVCframework

```pascal
[MVCHTTPMethod([httpGET])]
[MVCPath('/customers.csv')]
procedure GetPeopleAsCSV;

procedure TRenderSampleController.GetPeopleAsCSV;
begin
  ResponseStream.AppendLine('first_name;last_name;age');
  ResponseStream.AppendLine('Daniele;Teti;38');
  ResponseStream.AppendLine('Peter;Parker;22');
  ResponseStream.AppendLine('Bruce;Banner;60');
  ContentType := TMVCMediaType.TEXT_CSV;
  RenderResponseStream;
end;
```

#### mORMot2

```pascal
function GetPeopleAsCSV: RawUtf8;

function TRendersSample.GetPeopleAsCSV: RawUtf8;
var
  people: TOrmPersonObjArray;
  i: PtrInt;
  csv: RawUtf8;
begin
  csv := 'first_name;last_name;dob;married'#13#10;

  if fRest.RetrieveListObjArray(people, TOrmPerson, '') then
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
```

**Key Differences:**
- DMVCframework: Manual ContentType setting
- mORMot2: ContentType inferred (text/plain for RawUtf8)
- Both: String building approach similar

### 5. Complex Response with Metadata

#### DMVCframework

```pascal
procedure GetPeopleWithTiming;

procedure TRenderSampleController.GetPeopleWithTiming;
var
  p: TPerson;
  People: TPeopleWithMetadata;
begin
  People := TPeopleWithMetadata.Create;
  try
    People.Metadata.StartProcessing := Now;

    Sleep(1000); // processing...

    p := TPerson.Create;
    p.FirstName := 'Daniele';
    p.LastName := 'Teti';
    p.DOB := EncodeDate(1979, 11, 4);
    p.Married := True;
    People.Items.Add(p);

    // ... more items ...

    People.Metadata.CustomData := Format('There are %d people in the list',
      [People.Items.Count]);
    People.Metadata.StopProcessing := Now;
    Render(People, False);
  finally
    People.Free;
  end;
end;
```

#### mORMot2

```pascal
function GetPeopleWithMetadata: TPeopleWithMetadataDTO;

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
```

**Key Differences:**
- DMVCframework: Uses class with TObjectList properties
- mORMot2: Uses record with dynamic array
- DMVCframework: Manual object lifecycle management
- mORMot2: Automatic memory management (records)

## Feature Comparison

| Feature | DMVCframework | mORMot2 |
|---------|---------------|---------|
| **Routing** | Attribute-based | Interface methods |
| **Response Type** | Explicit Render() | Return type |
| **Content-Type** | Attributes/Manual | Type inference |
| **DTOs** | Classes | Records |
| **Memory Management** | Manual Free() | Automatic |
| **Serialization** | TMVCJsonDataObjectsSerializer | RTTI-based automatic |
| **Arrays** | TObjectList<T> | TArray<T> |
| **HATEOAS** | Callback-based | Not built-in |
| **DataSets** | Native support | ORM-based |
| **Attributes** | Heavy use | Minimal |

## Performance Comparison

### Memory

- **DMVCframework**: Classes on heap, manual management
- **mORMot2**: Records on stack/heap, automatic cleanup

### Speed

- **DMVCframework**: Reflection + serializer
- **mORMot2**: RTTI + optimized binary serialization

### Scalability

- Both: Thread-safe with proper server configuration
- mORMot2: Lighter weight DTOs (records vs classes)

## Advantages and Disadvantages

### DMVCframework Advantages

1. **Explicit Control**: Full control over rendering process
2. **HATEOAS**: Built-in support for hypermedia links
3. **Flexibility**: Multiple render methods and formats
4. **DataSet Support**: Native FireDAC dataset rendering
5. **View Templates**: Built-in template engine

### DMVCframework Disadvantages

1. **Verbose**: Requires Render() calls and attributes
2. **Memory Management**: Manual object lifecycle
3. **Boilerplate**: More code for simple operations

### mORMot2 Advantages

1. **Type Safety**: Compile-time interface checking
2. **Clean Code**: No explicit render calls
3. **Performance**: Lighter DTOs (records)
4. **Automatic**: Serialization based on return type
5. **Less Boilerplate**: Minimal code for common tasks

### mORMot2 Disadvantages

1. **Less Flexible**: Return type determines response
2. **No HATEOAS**: No built-in hypermedia support
3. **Learning Curve**: Interface-based services pattern
4. **Binary Focus**: More suited to binary protocols

## Conclusion

Both frameworks excel at different things:

- **DMVCframework**: Better for traditional REST APIs with HATEOAS, datasets, and view templates
- **mORMot2**: Better for high-performance APIs with type safety and minimal boilerplate

Choose based on your needs:
- Need HATEOAS, datasets, templates? → DMVCframework
- Need performance, type safety, simplicity? → mORMot2
