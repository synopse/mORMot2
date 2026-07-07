
unit task_dtos;

{$ifdef FPC}{$mode objfpc}{$H+}{$endif}

interface

uses
  mormot.core.base,
  mormot.core.rtti,
  shared_types,
  task;

type
  /// DTO for displaying a single task (read side — masks and detail views)
  TTaskViewDTO = packed record
    ID: TID;
    Title: RawUtf8;
    Description: RawUtf8;
    Priority: integer;
    PriorityName: RawUtf8;
    DueDate: TDateTime;
    Status: RawUtf8;
    IsCompleted: boolean;
    CreatedAt: TDateTime;
    UpdatedAt: TDateTime;
    CommentCount: integer;
    TagIDs: TIDDynArray;
    Comments: TTaskCommentDynArray;
  end;

  /// DTO for task list items (read side — list views)
  TTaskListItemDTO = packed record
    ID: TID;
    Title: RawUtf8;
    Priority: integer;
    PriorityName: RawUtf8;
    Status: RawUtf8;
    IsCompleted: boolean;
    DueDate: TDateTime;
    CommentCount: integer;
    TagCount: integer;
  end;
  TTaskListItemDTODynArray = array of TTaskListItemDTO;

  /// DTO for task search criteria (read side)
  TTaskSearchDTO = packed record
    SearchTerm: RawUtf8;
    Status: RawUtf8;
    /// Unused padding — DO NOT remove.
    // Works around a mORMot SOA quirk: a service method whose sole input is a
    // record with *exactly two* fields deserializes to all-empty fields over
    // HTTP (the parser still reports success). A third field of any type
    // restores correct parsing; clients may omit it on the wire.
    Reserved: RawUtf8;
  end;

  /// DTO for creating a new task (write side)
  TTaskCreateDTO = packed record
    Title: RawUtf8;
    Description: RawUtf8;
    Priority: integer;
    DueDate: RawUtf8;
  end;

  /// DTO for updating a task (write side)
  TTaskUpdateDTO = packed record
    TaskID: TID;
    Title: RawUtf8;
    Description: RawUtf8;
    Priority: integer;
    DueDate: RawUtf8;
  end;

  /// DTO for adding a comment to a task (write side)
  TTaskAddCommentDTO = packed record
    TaskID: TID;
    Content: RawUtf8;
    Author: RawUtf8;
  end;

  /// DTO for updating a comment (write side)
  TTaskUpdateCommentDTO = packed record
    TaskID: TID;
    CommentIndex: integer;
    Content: RawUtf8;
  end;

implementation

initialization
  Rtti.RegisterFromText(TypeInfo(TTaskListItemDTO),
    'ID Int64 Title RawUtf8 Priority integer PriorityName RawUtf8 ' +
    'Status RawUtf8 IsCompleted boolean DueDate TDateTime ' +
    'CommentCount integer TagCount integer');
  Rtti.RegisterFromText(TypeInfo(TTaskSearchDTO),
    'SearchTerm RawUtf8 Status RawUtf8 Reserved RawUtf8');
  Rtti.RegisterFromText(TypeInfo(TTaskCreateDTO),
    'Title RawUtf8 Description RawUtf8 Priority integer DueDate RawUtf8');
  Rtti.RegisterFromText(TypeInfo(TTaskUpdateDTO),
    'TaskID Int64 Title RawUtf8 Description RawUtf8 Priority integer DueDate RawUtf8');
  Rtti.RegisterFromText(TypeInfo(TTaskAddCommentDTO),
    'TaskID Int64 Content RawUtf8 Author RawUtf8');
  Rtti.RegisterFromText(TypeInfo(TTaskUpdateCommentDTO),
    'TaskID Int64 CommentIndex integer Content RawUtf8');
  Rtti.RegisterFromText(TypeInfo(TTaskViewDTO),
    'ID Int64 Title RawUtf8 Description RawUtf8 Priority integer ' +
    'PriorityName RawUtf8 DueDate TDateTime Status RawUtf8 ' +
    'IsCompleted boolean CreatedAt TDateTime UpdatedAt TDateTime ' +
    'CommentCount integer TagIDs TIDDynArray Comments TTaskCommentDynArray');

  // Normalize the JSON wire contract to camelCase (Recommended Patterns A.7,
  // Gotcha 3). On the DTO types only — never the TOrm — and, on FPC, after the
  // RegisterFromText calls above built the fields these calls rename.
  Rtti[TypeInfo(TTaskListItemDTO)].Props.NameChangeCase(DtoJsonCase);
  Rtti[TypeInfo(TTaskSearchDTO)].Props.NameChangeCase(DtoJsonCase);
  Rtti[TypeInfo(TTaskCreateDTO)].Props.NameChangeCase(DtoJsonCase);
  Rtti[TypeInfo(TTaskUpdateDTO)].Props.NameChangeCase(DtoJsonCase);
  Rtti[TypeInfo(TTaskAddCommentDTO)].Props.NameChangeCase(DtoJsonCase);
  Rtti[TypeInfo(TTaskUpdateCommentDTO)].Props.NameChangeCase(DtoJsonCase);
  Rtti[TypeInfo(TTaskViewDTO)].Props.NameChangeCase(DtoJsonCase);

end.
