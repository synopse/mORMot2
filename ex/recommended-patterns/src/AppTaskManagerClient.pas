
unit AppTaskManagerClient;

{ Consumer dependency unit (Recommended Patterns B.6.1: App<Name>Client.pas).

  This is the transport-agnostic surface every client consumes: it exposes the
  four CQRS ports (ITaskCommand/ITaskQuery/ITagCommand/ITagQuery) behind a single
  ITaskManagerClient handle and knows NOTHING about how they are reached. The two
  concrete backends provide a `CreateClient` factory returning this interface:
   - AppTaskManagerClientLocal  — in-process monolith with embedded SQLite (A.6.1)
   - AppTaskManagerClientRemote — HTTP client to the daemon's dispatcher (A.6.2)

  A consumer (the CLI, a GUI, a test) `uses` exactly one backend and is otherwise
  identical regardless of topology — that is the location transparency the
  patterns document is after. }

{$ifdef FPC}{$mode objfpc}{$H+}{$endif}

interface

uses
  mormot.core.base,
  mormot.core.interfaces,
  app_settings,
  task_command,
  task_query,
  tag_command,
  tag_query;

type
  /// Transport-agnostic handle to the Task Manager's CQRS ports.
  /// Lifetime owns the underlying transport (servers or HTTP client): the
  /// connection is torn down when the last reference is released.
  ITaskManagerClient = interface
    ['{D5E6F7A8-1111-2222-3333-444455556666}']
    function TaskCommand: ITaskCommand;
    function TaskQuery: ITaskQuery;
    function TagCommand: ITagCommand;
    function TagQuery: ITagQuery;
  end;

  /// Signature of the CreateClient factory both backends implement.
  /// The program file links exactly one backend and hands its CreateClient to
  /// the presentation layer, which stays backend-agnostic.
  TCreateClientFunc = function(aSettings: TTaskManagerSettings): ITaskManagerClient;

/// Register the RPC-exposed CQRS interfaces with the mORMot interface factory.
/// Idempotent; call once before creating a client (both backends need it).
procedure RegisterTaskManagerInterfaces;

implementation

procedure RegisterTaskManagerInterfaces;
begin
  TInterfaceFactory.RegisterInterfaces([
    TypeInfo(ITaskQuery),
    TypeInfo(ITaskCommand),
    TypeInfo(ITagQuery),
    TypeInfo(ITagCommand)
  ]);
end;

end.
