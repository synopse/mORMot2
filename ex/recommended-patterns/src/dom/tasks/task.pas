
unit task;

{ Task aggregate root (Recommended Patterns B.3 / B.6.1).

  The TOrm doubling as the domain object lives in dom/: it owns its own
  invariants (comment / tag mutators, completion state) and depends only on the
  ORM *base type* (mormot.orm.core), never the ORM *runtime* (no IRestOrm, no
  REST server, no SQL). The FTS5 index schema and the IRestOrm-based lazy
  migration are persistence concerns and live in infra/task_repository_orm. }

{$ifdef FPC}{$mode objfpc}{$H+}{$endif}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.data,
  mormot.core.json,
  mormot.core.rtti,
  mormot.orm.core,
  shared_types;

const
  TASK_CURRENT_VERSION = 1;

type
  /// Embedded comment within the Task aggregate
  TTaskComment = packed record
    Content: RawUtf8;
    Author: RawUtf8;
    CreatedAt: TDateTime;
    UpdatedAt: TDateTime;
    IsEdited: boolean;
  end;
  TTaskCommentDynArray = array of TTaskComment;

  TTask = class;
  /// Object-owning dynamic array of TTask; registered as a T*ObjArray so
  /// `IList<TTask>` storage frees its items automatically on release.
  TTaskObjArray = array of TTask;

  /// Task aggregate root — the central DDD aggregate
  /// Comments and tag references are embedded sub-structures,
  /// serialized as JSON in single table columns.
  TTask = class(TOrm)
  protected
    fTitle: RawUtf8;
    fDescription: RawUtf8;
    fPriority: integer;
    fDueDate: TDateTime;
    fStatus: RawUtf8;
    fIsCompleted: boolean;
    fCreatedAt: TDateTime;
    fUpdatedAt: TDateTime;
    fSchemaVersion: integer;
    fComments: TTaskCommentDynArray;
    fTagIDs: TIDDynArray;
  public
    // --- Domain behavior (B.3): the aggregate owns its own invariants, so
    // application services never reach into the embedded arrays directly. ---
    /// Stamp UpdatedAt = NowUtc. Called by every mutator below.
    procedure Touch;
    /// Set completion state and the matching Status string; touches UpdatedAt.
    procedure SetCompleted(aValue: boolean);
    /// Append an already-built comment; touches UpdatedAt.
    procedure AppendComment(const aComment: TTaskComment);
    /// Replace a comment's content (marks it edited). Returns false if the
    /// index is out of range; aChanged is true only when the content differed.
    function UpdateCommentContent(aIndex: integer; const aContent: RawUtf8;
      out aChanged: boolean): boolean;
    /// Remove the comment at aIndex. Returns false if the index is out of range.
    function DeleteComment(aIndex: integer): boolean;
    /// Number of embedded comments.
    function CommentCount: integer;
    /// True if aTagID is already referenced.
    function HasTag(aTagID: TID): boolean;
    /// Add a tag reference if not present. Returns true if it was added
    /// (false when already present); touches UpdatedAt only when added.
    function AddTag(aTagID: TID): boolean;
    /// Remove a tag reference. Returns true if it was present and removed;
    /// touches UpdatedAt only when removed.
    function RemoveTag(aTagID: TID): boolean;
  published
    property Title: RawUtf8 read fTitle write fTitle;
    property Description: RawUtf8 read fDescription write fDescription;
    property Priority: integer read fPriority write fPriority;
    property DueDate: TDateTime read fDueDate write fDueDate;
    property Status: RawUtf8 read fStatus write fStatus;
    property IsCompleted: boolean read fIsCompleted write fIsCompleted;
    property CreatedAt: TDateTime read fCreatedAt write fCreatedAt;
    property UpdatedAt: TDateTime read fUpdatedAt write fUpdatedAt;
    /// Domain-level schema version for lazy migration
    property SchemaVersion: integer read fSchemaVersion write fSchemaVersion;
    /// Embedded comments — stored as JSON array in a TEXT column
    property Comments: TTaskCommentDynArray read fComments write fComments;
    /// References to Tag aggregate by ID — stored as JSON array
    property TagIDs: TIDDynArray read fTagIDs write fTagIDs;
  end;

function GetPriorityName(Priority: integer): RawUtf8;
function IsValidStatus(const Status: RawUtf8): boolean;
function GetDefaultStatus: RawUtf8;

implementation

{ TTask — domain behavior }

procedure TTask.Touch;
begin
  fUpdatedAt := NowUtc;
end;

procedure TTask.SetCompleted(aValue: boolean);
begin
  fIsCompleted := aValue;
  if aValue then
    fStatus := 'completed'
  else
    fStatus := 'pending';
  Touch;
end;

procedure TTask.AppendComment(const aComment: TTaskComment);
var
  Len: integer;
begin
  Len := Length(fComments);
  SetLength(fComments, Len + 1);
  fComments[Len] := aComment;
  Touch;
end;

function TTask.UpdateCommentContent(aIndex: integer; const aContent: RawUtf8;
  out aChanged: boolean): boolean;
begin
  aChanged := false;
  if (aIndex < 0) or (aIndex >= Length(fComments)) then
  begin
    Result := false;
    exit;
  end;
  Result := true;
  if fComments[aIndex].Content = aContent then
    exit;
  fComments[aIndex].Content := aContent;
  fComments[aIndex].UpdatedAt := NowUtc;
  fComments[aIndex].IsEdited := true;
  aChanged := true;
  Touch;
end;

function TTask.DeleteComment(aIndex: integer): boolean;
var
  i: integer;
begin
  if (aIndex < 0) or (aIndex >= Length(fComments)) then
  begin
    Result := false;
    exit;
  end;
  for i := aIndex to Length(fComments) - 2 do
    fComments[i] := fComments[i + 1];
  SetLength(fComments, Length(fComments) - 1);
  Touch;
  Result := true;
end;

function TTask.CommentCount: integer;
begin
  Result := Length(fComments);
end;

function TTask.HasTag(aTagID: TID): boolean;
var
  i: integer;
begin
  for i := 0 to High(fTagIDs) do
    if fTagIDs[i] = aTagID then
    begin
      Result := true;
      exit;
    end;
  Result := false;
end;

function TTask.AddTag(aTagID: TID): boolean;
var
  Len: integer;
begin
  if HasTag(aTagID) then
  begin
    Result := false;
    exit;
  end;
  Len := Length(fTagIDs);
  SetLength(fTagIDs, Len + 1);
  fTagIDs[Len] := aTagID;
  Touch;
  Result := true;
end;

function TTask.RemoveTag(aTagID: TID): boolean;
var
  i, Len: integer;
begin
  Len := Length(fTagIDs);
  for i := 0 to Len - 1 do
    if fTagIDs[i] = aTagID then
    begin
      Move(fTagIDs[i + 1], fTagIDs[i], (Len - i - 1) * SizeOf(TID));
      SetLength(fTagIDs, Len - 1);
      Touch;
      Result := true;
      exit;
    end;
  Result := false;
end;

function GetPriorityName(Priority: integer): RawUtf8;
begin
  case Priority of
    1: Result := 'Low';
    2: Result := 'Medium';
    3: Result := 'High';
    4: Result := 'Urgent';
    5: Result := 'Critical';
  else
    Result := 'Unknown';
  end;
end;

function IsValidStatus(const Status: RawUtf8): boolean;
begin
  Result := (Status = 'pending') or
            (Status = 'in_progress') or
            (Status = 'completed');
end;

function GetDefaultStatus: RawUtf8;
begin
  Result := 'pending';
end;

initialization
  Rtti.RegisterFromText(TypeInfo(TTaskComment),
    'Content RawUtf8 Author RawUtf8 CreatedAt TDateTime ' +
    'UpdatedAt TDateTime IsEdited boolean');
  // TTaskComment is an embedded record DTO (serialized inside TTaskViewDTO),
  // so it follows the same camelCase wire contract as the other DTOs
  // (Recommended Patterns A.7, Gotcha 3). The TTask TOrm is left untouched.
  Rtti[TypeInfo(TTaskComment)].Props.NameChangeCase(DtoJsonCase);
  Rtti.RegisterObjArray(TypeInfo(TTaskObjArray), TTask);

end.
