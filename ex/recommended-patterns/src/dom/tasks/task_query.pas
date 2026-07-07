
unit task_query;

{$ifdef FPC}{$mode objfpc}{$H+}{$endif}

interface

uses
  mormot.core.base,
  mormot.core.interfaces,
  task_dtos;

type
  /// CQRS Query interface — read-optimized operations for Task context
  ITaskQuery = interface(IInvokable)
    ['{A1B2C3D4-1111-2222-3333-444455556666}']
    function GetTaskView(aTaskID: TID): TTaskViewDTO;
    function ListTasks(const aStatus: RawUtf8): TTaskListItemDTODynArray;
    function SearchTasks(const aCriteria: TTaskSearchDTO): TTaskListItemDTODynArray;
  end;

implementation

end.
