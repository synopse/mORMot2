
unit tag_query_impl;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.collections,
  mormot.core.log,
  mormot.core.unicode,
  mormot.core.interfaces,
  mormot.orm.core,
  mormot.rest.server,
  mormot.soa.server,
  tag,
  tag_dtos,
  tag_mappers,
  tag_repository,
  tag_query;

type
  TTagQueryService = class(TInjectableObjectRest, ITagQuery)
  private
    fRepo: ITagRepository;
  public
    function GetTagView(aTagID: TID): TTagViewDTO;
    function ListTags: TTagViewDTODynArray;
    function SearchTags(const aSearchTerm: RawUtf8): TTagViewDTODynArray;
  published
    property Repo: ITagRepository
      read fRepo write fRepo;
  end;

implementation

function TTagQueryService.GetTagView(aTagID: TID): TTagViewDTO;
var
  Tag: TTag;
begin
  Tag := fRepo.GetByID(aTagID);
  if Tag = nil then
  begin
    TSynLog.Add.Log(sllDebug, 'GetTagView: tag % not found', [aTagID]);
    FillChar(Result, SizeOf(Result), 0);
    exit;
  end;
  try
    Result := OrmToTagViewDTO(Tag);
  finally
    Tag.Free;
  end;
end;

function TTagQueryService.ListTags: TTagViewDTODynArray;
var
  Tags: IList<TTag>;
  i: integer;
begin
  // repository returns aggregates already migrated to the current schema
  Tags := fRepo.List;
  SetLength(Result, Tags.Count);
  for i := 0 to Tags.Count - 1 do
    Result[i] := OrmToTagViewDTO(Tags[i]);
end;

function TTagQueryService.SearchTags(const aSearchTerm: RawUtf8): TTagViewDTODynArray;
var
  Tags: IList<TTag>;
  i: integer;
begin
  SetLength(Result, 0);
  if aSearchTerm = '' then
    exit;
  Tags := fRepo.SearchByLike('%' + LowerCase(aSearchTerm) + '%');
  SetLength(Result, Tags.Count);
  for i := 0 to Tags.Count - 1 do
    Result[i] := OrmToTagViewDTO(Tags[i]);
end;

end.
