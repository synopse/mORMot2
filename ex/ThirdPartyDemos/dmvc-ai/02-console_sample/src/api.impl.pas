unit api.impl;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.os,
  mormot.core.rtti,
  mormot.core.variants,
  mormot.core.interfaces,
  mormot.core.log,
  mormot.orm.core,
  api.interfaces,
  entities;

type
  /// implement the Greeting Service API
  TGreetingService = class(TInterfacedObject, IGreetingService)
  protected
    fRest: IRestOrm;
    fGreetingMap: TRttiMap;
  public
    constructor Create(const aRest: IRestOrm); reintroduce;
    // IGreetingService methods
    function CreateGreeting(const name, message: RawUtf8): TGreetingID;
    function GetGreeting(id: TGreetingID): TGreetingDTO;
    function GetAllGreetings: TGreetingDTOs;
    function DeleteGreeting(id: TGreetingID): boolean;
  end;


implementation


{ TGreetingService }

constructor TGreetingService.Create(const aRest: IRestOrm);
begin
  fRest := aRest;
  // Map between ORM entity and DTO
  fGreetingMap.Init(TypeInfo(TOrmGreeting), TypeInfo(TGreetingDTO)).
    Map(['id', 'id',
         'name', 'name',
         'message', 'msg',
         'timestamp', 'created']);
  TSynLog.Add.Log(sllDebug, 'Greeting Service created');
end;

function TGreetingService.CreateGreeting(const name, message: RawUtf8): TGreetingID;
var
  greeting: TOrmGreeting;
begin
  result := 0;
  if not Assigned(fRest) then
  begin
    TSynLog.Add.Log(sllError, 'CreateGreeting: REST ORM not assigned');
    exit;
  end;
  greeting := TOrmGreeting.Create;
  try
    greeting.Name := name;
    greeting.Message := message;
    greeting.Timestamp := UnixMSTimeUtcFast;
    if not greeting.HasAllNeededFields then
    begin
      TSynLog.Add.Log(sllWarning, 'CreateGreeting: Missing required fields');
      exit;
    end;
    result := fRest.Add(greeting, {sendData=}true);
    TSynLog.Add.Log(sllInfo, 'CreateGreeting: Created greeting #% for %',
      [result, name]);
  finally
    greeting.Free;
  end;
end;

function TGreetingService.GetGreeting(id: TGreetingID): TGreetingDTO;
var
  greeting: TOrmGreeting;
begin
  FillCharFast(result, SizeOf(result), 0);
  if not Assigned(fRest) then
  begin
    TSynLog.Add.Log(sllError, 'GetGreeting: REST ORM not assigned');
    exit;
  end;
  greeting := TOrmGreeting.Create(fRest, id);
  try
    if greeting.ID <> 0 then
    begin
      fGreetingMap.ToB(greeting, @result);
      TSynLog.Add.Log(sllDebug, 'GetGreeting: Retrieved greeting #%', [id]);
    end
    else
      TSynLog.Add.Log(sllWarning, 'GetGreeting: Greeting #% not found', [id]);
  finally
    greeting.Free;
  end;
end;

function TGreetingService.GetAllGreetings: TGreetingDTOs;
var
  greetings: TOrmGreetingObjArray;
begin
  result := nil;
  if not Assigned(fRest) then
  begin
    TSynLog.Add.Log(sllError, 'GetAllGreetings: REST ORM not assigned');
    exit;
  end;
  try
    fRest.RetrieveListObjArray(greetings, TOrmGreeting, '', []);
    fGreetingMap.ToArrayB(greetings, result);
    TSynLog.Add.Log(sllDebug, 'GetAllGreetings: Retrieved % greetings',
      [length(result)]);
  finally
    ObjArrayClear(greetings);
  end;
end;

function TGreetingService.DeleteGreeting(id: TGreetingID): boolean;
begin
  result := false;
  if not Assigned(fRest) then
  begin
    TSynLog.Add.Log(sllError, 'DeleteGreeting: REST ORM not assigned');
    exit;
  end;
  result := fRest.Delete(TOrmGreeting, id);
  if result then
    TSynLog.Add.Log(sllInfo, 'DeleteGreeting: Deleted greeting #%', [id])
  else
    TSynLog.Add.Log(sllWarning, 'DeleteGreeting: Failed to delete greeting #%', [id]);
end;


end.
