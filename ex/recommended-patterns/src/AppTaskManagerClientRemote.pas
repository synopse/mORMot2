
unit AppTaskManagerClientRemote;

{ HTTP backend (Recommended Patterns B.6.1: App<Name>ClientRemote.pas / A.6.2).

  Connects to the running daemon's public dispatcher over HTTP and resolves the
  four CQRS ports through the client's stub container. CreateClient hands them
  back behind ITaskManagerClient; the returned object owns the HTTP connection
  and closes it on release. No ORM, no SQLite — the wire contract is the DTOs. }

{$ifdef FPC}{$mode objfpc}{$H+}{$endif}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.interfaces,
  mormot.orm.core,
  mormot.rest.core,
  mormot.rest.http.client,
  mormot.soa.core,
  app_settings,
  task_query,
  task_command,
  tag_query,
  tag_command,
  AppTaskManagerClient;

/// Connect to the daemon at localhost:aSettings.HttpPort and resolve the CQRS
/// ports. RegisterTaskManagerInterfaces must have been called first.
/// Raises if the server is unreachable.
function CreateClient(aSettings: TTaskManagerSettings): ITaskManagerClient;

implementation

type
  TTaskManagerClientRemote = class(TInterfacedObject, ITaskManagerClient)
  private
    fModel: TOrmModel;
    fHttpClient: TRestHttpClient;
    fTaskCmd: ITaskCommand;
    fTaskQry: ITaskQuery;
    fTagCmd: ITagCommand;
    fTagQry: ITagQuery;
  public
    constructor Create(aSettings: TTaskManagerSettings);
    destructor Destroy; override;
    function TaskCommand: ITaskCommand;
    function TaskQuery: ITaskQuery;
    function TagCommand: ITagCommand;
    function TagQuery: ITagQuery;
  end;

constructor TTaskManagerClientRemote.Create(aSettings: TTaskManagerSettings);
begin
  inherited Create;
  fModel := TOrmModel.Create([], aSettings.Root);
  fHttpClient := TRestHttpClient.Create('localhost', aSettings.HttpPort, fModel);
  if not fHttpClient.ServerTimestampSynchronize then
    raise Exception.CreateFmt(
      'Cannot connect to server at localhost:%s. ' +
      'Make sure the task_manager server is running.', [aSettings.HttpPort]);
  fHttpClient.ServiceDefine([ITaskQuery, ITaskCommand, ITagQuery, ITagCommand],
    sicShared);
  fHttpClient.Services.Resolve(ITaskCommand, fTaskCmd);
  fHttpClient.Services.Resolve(ITaskQuery, fTaskQry);
  fHttpClient.Services.Resolve(ITagCommand, fTagCmd);
  fHttpClient.Services.Resolve(ITagQuery, fTagQry);
end;

destructor TTaskManagerClientRemote.Destroy;
begin
  fTaskCmd := nil;
  fTaskQry := nil;
  fTagCmd := nil;
  fTagQry := nil;
  fHttpClient.Free;
  fModel.Free;
  inherited Destroy;
end;

function TTaskManagerClientRemote.TaskCommand: ITaskCommand;
begin
  Result := fTaskCmd;
end;

function TTaskManagerClientRemote.TaskQuery: ITaskQuery;
begin
  Result := fTaskQry;
end;

function TTaskManagerClientRemote.TagCommand: ITagCommand;
begin
  Result := fTagCmd;
end;

function TTaskManagerClientRemote.TagQuery: ITagQuery;
begin
  Result := fTagQry;
end;

function CreateClient(aSettings: TTaskManagerSettings): ITaskManagerClient;
begin
  Result := TTaskManagerClientRemote.Create(aSettings);
end;

end.
