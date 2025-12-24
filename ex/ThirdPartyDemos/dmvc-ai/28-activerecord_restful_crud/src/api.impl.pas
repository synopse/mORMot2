unit api.impl;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.interfaces,
  mormot.core.variants,
  mormot.core.json,
  mormot.orm.core,
  mormot.orm.base,
  mormot.soa.server,
  entities,
  api.interfaces;

type
  /// Implementation of IActiveRecordApi using mORMot2 ORM
  // This demonstrates the ActiveRecord pattern with full CRUD operations
  // Similar to DMVC's TMVCActiveRecord but using mORMot2's TOrm
  TActiveRecordApiService = class(TInjectableObjectRest, IActiveRecordApi)
  private
    function OrmToVariant(orm: TOrm): Variant;
  public
    // Person CRUD
    function GetPerson(id: TID): Variant;
    function GetAllPeople: TVariantDynArray;
    function CreatePerson(const lastName, firstName, note: RawUtf8;
      dob: TDateTime; isMale: Boolean): TID;
    function UpdatePerson(id: TID; const lastName, firstName, note: RawUtf8;
      dob: TDateTime; isMale: Boolean): Boolean;
    function DeletePerson(id: TID): Boolean;

    // Article CRUD
    function GetArticle(id: TID): Variant;
    function GetAllArticles: TVariantDynArray;
    function CreateArticle(const description: RawUtf8; price: Currency): TID;
    function UpdateArticle(id: TID; const description: RawUtf8;
      price: Currency): Boolean;
    function DeleteArticle(id: TID): Boolean;

    // Phone CRUD
    function GetPhone(id: TID): Variant;
    function GetPhonesByPerson(personId: TID): TVariantDynArray;
    function CreatePhone(personId: TID; const phoneNumber,
      numberType: RawUtf8): TID;
    function DeletePhone(id: TID): Boolean;
  end;

implementation

{ TActiveRecordApiService }

function TActiveRecordApiService.OrmToVariant(orm: TOrm): Variant;
var
  json: RawUtf8;
begin
  if orm = nil then
  begin
    Result := Null;
    Exit;
  end;
  json := orm.GetJsonValues(true, true, ooSelect);
  Result := _JsonFast(json);
end;

// Person CRUD implementation

function TActiveRecordApiService.GetPerson(id: TID): Variant;
var
  person: TPersonOrm;
begin
  person := TPersonOrm.Create(Server.Orm, id);
  try
    if person.ID = 0 then
      Result := Null
    else
      Result := OrmToVariant(person);
  finally
    person.Free;
  end;
end;

function TActiveRecordApiService.GetAllPeople: TVariantDynArray;
var
  people: TPersonOrmObjArray;
  i: Integer;
begin
  if Server.Orm.RetrieveListObjArray(people, TPersonOrm, '', []) then
  try
    SetLength(Result, length(people));
    for i := 0 to High(people) do
      Result[i] := OrmToVariant(people[i]);
  finally
    ObjArrayClear(people);
  end;
end;

function TActiveRecordApiService.CreatePerson(const lastName, firstName,
  note: RawUtf8; dob: TDateTime; isMale: Boolean): TID;
var
  person: TPersonOrm;
begin
  person := TPersonOrm.Create;
  try
    person.LastName := lastName;
    person.FirstName := firstName;
    person.DOB := dob;
    person.IsMale := isMale;
    person.Note := note;
    Result := Server.Orm.Add(person, true);
  finally
    person.Free;
  end;
end;

function TActiveRecordApiService.UpdatePerson(id: TID; const lastName,
  firstName, note: RawUtf8; dob: TDateTime; isMale: Boolean): Boolean;
var
  person: TPersonOrm;
begin
  person := TPersonOrm.Create(Server.Orm, id);
  try
    if person.ID = 0 then
      Exit(false);
    person.LastName := lastName;
    person.FirstName := firstName;
    person.DOB := dob;
    person.IsMale := isMale;
    person.Note := note;
    Result := Server.Orm.Update(person);
  finally
    person.Free;
  end;
end;

function TActiveRecordApiService.DeletePerson(id: TID): Boolean;
begin
  Result := Server.Orm.Delete(TPersonOrm, id);
end;

// Article CRUD implementation

function TActiveRecordApiService.GetArticle(id: TID): Variant;
var
  article: TArticleOrm;
begin
  article := TArticleOrm.Create(Server.Orm, id);
  try
    if article.ID = 0 then
      Result := Null
    else
      Result := OrmToVariant(article);
  finally
    article.Free;
  end;
end;

function TActiveRecordApiService.GetAllArticles: TVariantDynArray;
var
  articles: TArticleOrmObjArray;
  i: Integer;
begin
  if Server.Orm.RetrieveListObjArray(articles, TArticleOrm, '', []) then
  try
    SetLength(Result, length(articles));
    for i := 0 to High(articles) do
      Result[i] := OrmToVariant(articles[i]);
  finally
    ObjArrayClear(articles);
  end;
end;

function TActiveRecordApiService.CreateArticle(const description: RawUtf8;
  price: Currency): TID;
var
  article: TArticleOrm;
begin
  article := TArticleOrm.Create;
  try
    article.Description := description;
    article.Price := price;
    Result := Server.Orm.Add(article, true);
  finally
    article.Free;
  end;
end;

function TActiveRecordApiService.UpdateArticle(id: TID;
  const description: RawUtf8; price: Currency): Boolean;
var
  article: TArticleOrm;
begin
  article := TArticleOrm.Create(Server.Orm, id);
  try
    if article.ID = 0 then
      Exit(false);
    article.Description := description;
    article.Price := price;
    Result := Server.Orm.Update(article);
  finally
    article.Free;
  end;
end;

function TActiveRecordApiService.DeleteArticle(id: TID): Boolean;
begin
  Result := Server.Orm.Delete(TArticleOrm, id);
end;

// Phone CRUD implementation

function TActiveRecordApiService.GetPhone(id: TID): Variant;
var
  phone: TPhoneOrm;
begin
  phone := TPhoneOrm.Create(Server.Orm, id);
  try
    if phone.ID = 0 then
      Result := Null
    else
      Result := OrmToVariant(phone);
  finally
    phone.Free;
  end;
end;

function TActiveRecordApiService.GetPhonesByPerson(
  personId: TID): TVariantDynArray;
var
  phones: TPhoneOrmObjArray;
  i: Integer;
begin
  if Server.Orm.RetrieveListObjArray(phones, TPhoneOrm,
    'PersonID=?', [personId]) then
  try
    SetLength(Result, length(phones));
    for i := 0 to High(phones) do
      Result[i] := OrmToVariant(phones[i]);
  finally
    ObjArrayClear(phones);
  end;
end;

function TActiveRecordApiService.CreatePhone(personId: TID;
  const phoneNumber, numberType: RawUtf8): TID;
var
  phone: TPhoneOrm;
begin
  phone := TPhoneOrm.Create;
  try
    phone.PersonID := personId;
    phone.PhoneNumber := phoneNumber;
    phone.NumberType := numberType;
    Result := Server.Orm.Add(phone, true);
  finally
    phone.Free;
  end;
end;

function TActiveRecordApiService.DeletePhone(id: TID): Boolean;
begin
  Result := Server.Orm.Delete(TPhoneOrm, id);
end;

end.
