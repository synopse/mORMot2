
unit tag_tests;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.data,
  mormot.core.os,
  mormot.core.test,
  mormot.core.log,
  mormot.orm.core,
  mormot.rest.core,
  mormot.rest.server,
  shared_types,
  tag,
  tag_dtos,
  tag_mappers,
  tag_query,
  tag_command;

type
  /// TSynTestCase-based tests for the Tag feature module
  TTestTag = class(TSynTestCase)
  protected
    fOrm: IRestOrm;
    fServer: TRestServer;
    procedure Setup; override;
  published
    procedure CreateAndRetrieveTag;
    procedure UpdateTag;
    procedure DeleteTag;
    procedure DuplicateNameRejected;
    procedure SearchTags;
    procedure LazyMigration;
    procedure MapperProjections;
    procedure MapperWriteSide;
  end;

/// Wire the tests to the two-server daemon layout (call before Run).
procedure SetTagTestServers(aPersistence, aDispatcher: TRestServer);

implementation

var
  _Persistence: TRestServer;
  _Dispatcher: TRestServer;

procedure SetTagTestServers(aPersistence, aDispatcher: TRestServer);
begin
  _Persistence := aPersistence;
  _Dispatcher := aDispatcher;
end;

procedure TTestTag.Setup;
begin
  fServer := _Dispatcher;
  fOrm := _Persistence.Orm;
end;

procedure TTestTag.CreateAndRetrieveTag;
var
  CmdSvc: ITagCommand;
  QrySvc: ITagQuery;
  Res: TCommandResult;
  View: TTagViewDTO;
  CreateDTO: TTagCreateDTO;
begin
  Check(fServer.Services.Resolve(ITagCommand, CmdSvc), 'resolve ITagCommand');
  Check(fServer.Services.Resolve(ITagQuery, QrySvc), 'resolve ITagQuery');

  CreateDTO.Name := 'test-tag';
  CreateDTO.Color := '#FF0000';
  Res := CmdSvc.CreateTag(CreateDTO);
  Check(Res.Success, 'CreateTag success');
  Check(Res.ID > 0, 'returned ID');

  View := QrySvc.GetTagView(Res.ID);
  CheckEqual(View.Name, 'test-tag');
  CheckEqual(View.Color, '#FF0000');

  CmdSvc.DeleteTag(Res.ID);
end;

procedure TTestTag.UpdateTag;
var
  CmdSvc: ITagCommand;
  QrySvc: ITagQuery;
  Res: TCommandResult;
  View: TTagViewDTO;
  CreateDTO: TTagCreateDTO;
  UpdateDTO: TTagUpdateDTO;
begin
  fServer.Services.Resolve(ITagCommand, CmdSvc);
  fServer.Services.Resolve(ITagQuery, QrySvc);

  CreateDTO.Name := 'original-tag';
  CreateDTO.Color := '#000000';
  Res := CmdSvc.CreateTag(CreateDTO);

  UpdateDTO.TagID := Res.ID;
  UpdateDTO.Name := 'renamed-tag';
  UpdateDTO.Color := '#FFFFFF';
  Res := CmdSvc.UpdateTag(UpdateDTO);
  Check(Res.Success, 'UpdateTag success');

  View := QrySvc.GetTagView(Res.ID);
  CheckEqual(View.Name, 'renamed-tag');
  CheckEqual(View.Color, '#FFFFFF');

  CmdSvc.DeleteTag(Res.ID);
end;

procedure TTestTag.DeleteTag;
var
  CmdSvc: ITagCommand;
  QrySvc: ITagQuery;
  Res: TCommandResult;
  View: TTagViewDTO;
  CreateDTO: TTagCreateDTO;
begin
  fServer.Services.Resolve(ITagCommand, CmdSvc);
  fServer.Services.Resolve(ITagQuery, QrySvc);

  CreateDTO.Name := 'delete-me-tag';
  CreateDTO.Color := '';
  Res := CmdSvc.CreateTag(CreateDTO);

  Res := CmdSvc.DeleteTag(Res.ID);
  Check(Res.Success, 'DeleteTag success');

  View := QrySvc.GetTagView(Res.ID);
  CheckEqual(View.ID, 0, 'tag no longer exists');
end;

procedure TTestTag.DuplicateNameRejected;
var
  CmdSvc: ITagCommand;
  Res: TCommandResult;
  CreateDTO: TTagCreateDTO;
  FirstID: TID;
begin
  fServer.Services.Resolve(ITagCommand, CmdSvc);

  CreateDTO.Name := 'unique-name';
  CreateDTO.Color := '';
  Res := CmdSvc.CreateTag(CreateDTO);
  Check(Res.Success);
  FirstID := Res.ID;

  Res := CmdSvc.CreateTag(CreateDTO);
  Check(not Res.Success, 'duplicate rejected');
  Check(Res.Status = srInvalidRequest, 'duplicate status srInvalidRequest');

  // deleting a non-existent ID reports the explicit not-found status
  Res := CmdSvc.DeleteTag(999999);
  Check(Res.Status = srNotFound, 'delete missing tag status srNotFound');

  CmdSvc.DeleteTag(FirstID);
end;

procedure TTestTag.SearchTags;
var
  CmdSvc: ITagCommand;
  QrySvc: ITagQuery;
  Res: TCommandResult;
  Tags: TTagViewDTODynArray;
  CreateDTO: TTagCreateDTO;
begin
  fServer.Services.Resolve(ITagCommand, CmdSvc);
  fServer.Services.Resolve(ITagQuery, QrySvc);

  CreateDTO.Name := 'searchable-xyz';
  CreateDTO.Color := '';
  Res := CmdSvc.CreateTag(CreateDTO);

  Tags := QrySvc.SearchTags('searchable');
  Check(Length(Tags) > 0, 'search found tags');

  Tags := QrySvc.ListTags;
  Check(Length(Tags) > 0, 'list has tags');

  CmdSvc.DeleteTag(Res.ID);
end;

procedure TTestTag.LazyMigration;
var
  CmdSvc: ITagCommand;
  QrySvc: ITagQuery;
  Tag: TTag;
  View: TTagViewDTO;
  TagID: TID;
begin
  fServer.Services.Resolve(ITagCommand, CmdSvc);
  fServer.Services.Resolve(ITagQuery, QrySvc);

  Tag := TTag.Create;
  try
    Tag.Name := 'legacy-tag';
    Tag.Color := '';
    Tag.SchemaVersion := 0;
    TagID := fOrm.Add(Tag, true);
  finally
    Tag.Free;
  end;

  View := QrySvc.GetTagView(TagID);
  CheckEqual(View.Color, '#3498db', 'color migrated to default');

  CmdSvc.DeleteTag(TagID);
end;

procedure TTestTag.MapperProjections;
var
  Tag: TTag;
  View: TTagViewDTO;
  Created: TDateTime;
begin
  // OrmToTagViewDTO is a straight scalar copy (no computed fields):
  // assert every field carries through (§A.7/§15.4).
  Created := NowUtc - 4;
  Tag := TTag.Create;
  try
    Tag.Name := 'projected-tag';
    Tag.Color := '#abcdef';
    Tag.CreatedAt := Created;

    View := OrmToTagViewDTO(Tag);
    CheckEqual(View.Name, 'projected-tag', 'view name copied');
    CheckEqual(View.Color, '#abcdef', 'view color copied');
    CheckSame(View.CreatedAt, Created, 1E-6, 'view CreatedAt copied');
  finally
    Tag.Free;
  end;
end;

procedure TTestTag.MapperWriteSide;
var
  Tag: TTag;
  CreateDTO: TTagCreateDTO;
  UpdateDTO: TTagUpdateDTO;
  Err: RawUtf8;
begin
  // --- CreateTagDTOToOrm happy path: normalizes name, sets defaults ---
  CreateDTO.Name := '  Work-Tag  '; // mixed case + surrounding spaces
  CreateDTO.Color := '#FF8800';

  Tag := TTag.Create;
  try
    Err := 'x';
    Check(CreateTagDTOToOrm(CreateDTO, Tag, Err), 'create happy path');
    CheckEqual(Err, '', 'create no error');
    CheckEqual(Tag.Name, 'work-tag', 'create normalizes name (trim+lowercase)');
    CheckEqual(Tag.Color, '#FF8800', 'create keeps valid color');
    CheckEqual(Tag.SchemaVersion, TAG_CURRENT_VERSION, 'create SchemaVersion');
    Check(Tag.CreatedAt > 0, 'create CreatedAt set');

    // empty/blank color falls back to the default
    CreateDTO.Name := 'plain';
    CreateDTO.Color := '';
    Check(CreateTagDTOToOrm(CreateDTO, Tag, Err), 'create blank color happy path');
    CheckEqual(Tag.Color, GetDefaultColor, 'create blank color -> default');

    // failure: blank name (only whitespace) rejected
    CreateDTO.Name := '   ';
    CreateDTO.Color := '';
    Check(not CreateTagDTOToOrm(CreateDTO, Tag, Err), 'create blank name fails');
    CheckEqual(Err, 'Tag name cannot be empty', 'create blank name error');
  finally
    Tag.Free;
  end;

  // --- ApplyUpdateTagDTO: normalizes name, resolves color ---
  Tag := TTag.Create;
  try
    Tag.Name := 'before';
    Tag.Color := '#000000';

    UpdateDTO.TagID := 5;
    UpdateDTO.Name := 'After-Tag';
    UpdateDTO.Color := '#123456';
    Check(ApplyUpdateTagDTO(UpdateDTO, Tag, Err), 'update happy path');
    CheckEqual(Err, '', 'update no error');
    CheckEqual(Tag.Name, 'after-tag', 'update normalizes name');
    CheckEqual(Tag.Color, '#123456', 'update keeps valid color');

    // blank color falls back to default
    UpdateDTO.Color := '';
    Check(ApplyUpdateTagDTO(UpdateDTO, Tag, Err), 'update blank color happy path');
    CheckEqual(Tag.Color, GetDefaultColor, 'update blank color -> default');

    // failure: blank name rejected
    UpdateDTO.Name := '  ';
    Check(not ApplyUpdateTagDTO(UpdateDTO, Tag, Err), 'update blank name fails');
    CheckEqual(Err, 'Tag name cannot be empty', 'update blank name error');
  finally
    Tag.Free;
  end;

  // --- NormalizeTagName: trim + lowercase ---
  CheckEqual(NormalizeTagName('  MixedCase '), 'mixedcase', 'normalize trims+lowers');
end;

end.
