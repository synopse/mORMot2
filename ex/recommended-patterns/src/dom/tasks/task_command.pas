
unit task_command;

{$ifdef FPC}{$mode objfpc}{$H+}{$endif}

interface

uses
  mormot.core.base,
  mormot.core.interfaces,
  shared_types,
  task_dtos;

type
  /// CQRS Command interface — write operations for Task context
  ITaskCommand = interface(IInvokable)
    ['{A1B2C3D4-5555-6666-7777-888899990000}']
    function CreateTask(const aData: TTaskCreateDTO): TCommandResult;
    function UpdateTask(const aData: TTaskUpdateDTO): TCommandResult;
    function DeleteTask(aTaskID: TID): TCommandResult;
    function MarkComplete(aTaskID: TID; aIsComplete: boolean): TCommandResult;
    function AddComment(const aData: TTaskAddCommentDTO): TCommandResult;
    function UpdateComment(const aData: TTaskUpdateCommentDTO): TCommandResult;
    function DeleteComment(aTaskID: TID; aCommentIndex: integer): TCommandResult;
    function AddTag(aTaskID: TID; aTagID: TID): TCommandResult;
    function RemoveTag(aTaskID: TID; aTagID: TID): TCommandResult;
  end;

implementation

end.
