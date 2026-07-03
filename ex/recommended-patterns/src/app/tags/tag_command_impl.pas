
unit tag_command_impl;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.collections,
  mormot.core.log,
  mormot.core.perf,
  mormot.core.unicode,
  mormot.core.interfaces,
  mormot.orm.core,
  mormot.rest.server,
  mormot.soa.server,
  shared_types,
  tag,
  tag_dtos,
  tag_mappers,
  tag_repository,
  tag_command;

type
  /// Thread-safety: sicShared instance.
  TTagCommandService = class(TInjectableObjectRest, ITagCommand)
  private
    fRepo: ITagRepository;
    fMonitor: TSynMonitor;
  public
    constructor Create; override;
    destructor Destroy; override;
    function CreateTag(const aData: TTagCreateDTO): TCommandResult;
    function UpdateTag(const aData: TTagUpdateDTO): TCommandResult;
    function DeleteTag(aTagID: TID): TCommandResult;
  published
    property Repo: ITagRepository
      read fRepo write fRepo;
  end;

implementation

constructor TTagCommandService.Create;
begin
  inherited Create;
  fMonitor := TSynMonitor.Create('TagCommand');
end;

destructor TTagCommandService.Destroy;
begin
  fMonitor.Free;
  inherited Destroy;
end;

function TTagCommandService.CreateTag(const aData: TTagCreateDTO): TCommandResult;
var
  Tag: TTag;
  ErrMsg: RawUtf8;
  Existing: IList<TTag>;
  NewID: TID;
begin
  fMonitor.ProcessStart;
  try
    Existing := fRepo.FindByName(NormalizeTagName(aData.Name));
    if Existing.Count > 0 then
    begin
      Result := CommandError('Tag with this name already exists');
      exit;
    end;

    Tag := TTag.Create;
    try
      if not CreateTagDTOToOrm(aData, Tag, ErrMsg) then
      begin
        Result := CommandError(ErrMsg);
        exit;
      end;
      NewID := fRepo.Add(Tag);
      if NewID > 0 then
      begin
        TSynLog.Add.Log(sllInfo, 'CreateTag: created "%" ID=%', [Tag.Name, NewID]);
        Result := CommandSuccess(NewID);
      end
      else
        Result := CommandError('Failed to create tag', srDbError);
    finally
      Tag.Free;
    end;
  finally
    fMonitor.ProcessEnd;
  end;
end;

function TTagCommandService.UpdateTag(const aData: TTagUpdateDTO): TCommandResult;
var
  Tag: TTag;
  ErrMsg: RawUtf8;
  Existing: IList<TTag>;
begin
  fMonitor.ProcessStart;
  try
    Tag := fRepo.GetByID(aData.TagID);
    if Tag = nil then
    begin
      Result := CommandError('Tag not found', srNotFound);
      exit;
    end;
    try
      Existing := fRepo.FindByNameExcludingID(NormalizeTagName(aData.Name), aData.TagID);
      if Existing.Count > 0 then
      begin
        Result := CommandError('Tag with this name already exists');
        exit;
      end;

      if not ApplyUpdateTagDTO(aData, Tag, ErrMsg) then
      begin
        Result := CommandError(ErrMsg);
        exit;
      end;
      if fRepo.Update(Tag) then
        Result := CommandSuccess(aData.TagID)
      else
        Result := CommandError('Failed to update tag', srDbError);
    finally
      Tag.Free;
    end;
  finally
    fMonitor.ProcessEnd;
  end;
end;

function TTagCommandService.DeleteTag(aTagID: TID): TCommandResult;
var
  Tag: TTag;
begin
  fMonitor.ProcessStart;
  try
    Tag := fRepo.GetByID(aTagID);
    if Tag = nil then
    begin
      Result := CommandError('Tag not found', srNotFound);
      exit;
    end;
    Tag.Free;

    // No cascade — tag references in Task aggregates are cleaned up lazily.
    if fRepo.Delete(aTagID) then
    begin
      TSynLog.Add.Log(sllInfo, 'DeleteTag: deleted ID=%', [aTagID]);
      Result := CommandSuccess(aTagID);
    end
    else
      Result := CommandError('Failed to delete tag', srDbError);
  finally
    fMonitor.ProcessEnd;
  end;
end;

end.
