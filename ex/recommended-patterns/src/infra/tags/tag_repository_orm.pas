
unit tag_repository_orm;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  contnrs,
  mormot.core.base,
  mormot.core.collections,
  mormot.core.data,
  mormot.orm.core,
  tag,
  tag_repository;

type
  TTagRepositoryOrm = class(TInterfacedObject, ITagRepository)
  private
    fOrm: IRestOrm;
    function RetrieveToIList(const aWhere: RawUtf8;
      const aBoundArgs: array of const): IList<TTag>;
    procedure MigrateAll(const aList: IList<TTag>);
  public
    constructor Create(const aOrm: IRestOrm);
    function GetByID(aID: TID): TTag;
    function Add(aTag: TTag): TID;
    function Update(aTag: TTag): boolean;
    function Delete(aID: TID): boolean;
    function List: IList<TTag>;
    function FindByName(const aName: RawUtf8): IList<TTag>;
    function FindByNameExcludingID(const aName: RawUtf8; aExcludeID: TID): IList<TTag>;
    function SearchByLike(const aPattern: RawUtf8): IList<TTag>;
  end;

implementation

uses
  mormot.core.os,
  Classes;

{ Lazy migration: upgrades a tag record to the current schema version.
  Persistence concern (it writes through IRestOrm), so it lives here in infra/
  alongside the repository rather than in the dom/ aggregate. }
procedure MigrateTagIfNeeded(const aOrm: IRestOrm; aTag: TTag);
begin
  if aTag.SchemaVersion >= TAG_CURRENT_VERSION then
    exit;
  if aTag.SchemaVersion < 1 then
  begin
    if aTag.Color = '' then
      aTag.Color := GetDefaultColor;
  end;
  aTag.SchemaVersion := TAG_CURRENT_VERSION;
  aOrm.Update(aTag);
end;

constructor TTagRepositoryOrm.Create(const aOrm: IRestOrm);
begin
  inherited Create;
  fOrm := aOrm;
end;

function TTagRepositoryOrm.RetrieveToIList(const aWhere: RawUtf8;
  const aBoundArgs: array of const): IList<TTag>;
var
  Raw: TObjectList;
  i: integer;
begin
  Result := Collections.NewList<TTag>([], TypeInfo(TTagObjArray));
  Raw := fOrm.RetrieveList(TTag, aWhere, aBoundArgs, '');
  if Raw = nil then
    exit;
  try
    Raw.OwnsObjects := false;
    Result.Capacity := Raw.Count;
    for i := 0 to Raw.Count - 1 do
      Result.Add(TTag(Raw[i]));
  finally
    Raw.Free;
  end;
end;

procedure TTagRepositoryOrm.MigrateAll(const aList: IList<TTag>);
var
  i: integer;
begin
  for i := 0 to aList.Count - 1 do
    MigrateTagIfNeeded(fOrm, aList[i]);
end;

function TTagRepositoryOrm.GetByID(aID: TID): TTag;
begin
  Result := TTag.Create(fOrm, aID);
  if Result.ID = 0 then
  begin
    Result.Free;
    Result := nil;
    exit;
  end;
  // reads return aggregates already at the current schema version
  MigrateTagIfNeeded(fOrm, Result);
end;

function TTagRepositoryOrm.Add(aTag: TTag): TID;
begin
  Result := fOrm.Add(aTag, true);
end;

function TTagRepositoryOrm.Update(aTag: TTag): boolean;
begin
  Result := fOrm.Update(aTag);
end;

function TTagRepositoryOrm.Delete(aID: TID): boolean;
begin
  Result := fOrm.Delete(TTag, aID);
end;

function TTagRepositoryOrm.List: IList<TTag>;
begin
  Result := RetrieveToIList('', []);
  MigrateAll(Result);
end;

function TTagRepositoryOrm.FindByName(const aName: RawUtf8): IList<TTag>;
begin
  Result := RetrieveToIList('Name=?', [aName]);
end;

function TTagRepositoryOrm.FindByNameExcludingID(
  const aName: RawUtf8; aExcludeID: TID): IList<TTag>;
begin
  Result := RetrieveToIList('Name=? AND ID<>?', [aName, aExcludeID]);
end;

function TTagRepositoryOrm.SearchByLike(const aPattern: RawUtf8): IList<TTag>;
begin
  Result := RetrieveToIList('Name LIKE ?', [aPattern]);
  MigrateAll(Result);
end;

end.
