
unit tag_command;

{$mode objfpc}{$H+}

interface

uses
  mormot.core.base,
  mormot.core.interfaces,
  shared_types,
  tag_dtos;

type
  /// CQRS Command interface — write operations for Tag context
  ITagCommand = interface(IInvokable)
    ['{B2C3D4E5-5555-6666-7777-888899990011}']
    function CreateTag(const aData: TTagCreateDTO): TCommandResult;
    function UpdateTag(const aData: TTagUpdateDTO): TCommandResult;
    function DeleteTag(aTagID: TID): TCommandResult;
  end;

implementation

end.
