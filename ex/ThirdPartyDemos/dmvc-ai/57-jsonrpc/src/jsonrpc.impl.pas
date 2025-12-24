unit jsonrpc.impl;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.rtti,
  mormot.core.text,
  mormot.core.interfaces,
  mormot.core.log,
  mormot.orm.core,
  mormot.soa.core,
  jsonrpc.interfaces,
  jsonrpc.entities;

type
  /// Implementation of Calculator Service
  TCalculatorService = class(TInterfacedObject, ICalculatorService)
  protected
    fRest: IRestOrm;
    fCalculationMap: TRttiMap;
  public
    constructor Create(const aRest: IRestOrm); reintroduce;
    // ICalculatorService methods
    function Add(a, b: double): double;
    function Subtract(a, b: double): double;
    function Multiply(a, b: double): double;
    function Divide(a, b: double): double;
    function GetHistory: TCalculatorResults;
    function ClearHistory: boolean;
  protected
    procedure SaveCalculation(const aOperation: RawUtf8; aResult: double);
  end;

  /// Implementation of User Service
  TUserService = class(TInterfacedObject, IUserService)
  protected
    fRest: IRestOrm;
    fUserMap: TRttiMap;
  public
    constructor Create(const aRest: IRestOrm); reintroduce;
    // IUserService methods
    function CreateUser(const username, email: RawUtf8): TID;
    function GetUser(id: TID): TUserInfo;
    function GetAllUsers: TUserInfos;
    function DeleteUser(id: TID): boolean;
  end;

implementation

{ TCalculatorService }

constructor TCalculatorService.Create(const aRest: IRestOrm);
begin
  fRest := aRest;
  // Map between ORM entity and DTO
  fCalculationMap.Init(TypeInfo(TOrmCalculation), TypeInfo(TCalculatorResult)).
    Map(['Operation', 'operation',
         'Result', 'result',
         'Timestamp', 'timestamp']);
  TSynLog.Add.Log(sllDebug, 'Calculator Service created');
end;

procedure TCalculatorService.SaveCalculation(const aOperation: RawUtf8; aResult: double);
var
  calc: TOrmCalculation;
begin
  if not Assigned(fRest) then
    exit;
  calc := TOrmCalculation.Create;
  try
    calc.Operation := aOperation;
    calc.Result := aResult;
    calc.Timestamp := UnixMSTimeUtcFast;
    fRest.Add(calc, true);
  finally
    calc.Free;
  end;
end;

function TCalculatorService.Add(a, b: double): double;
begin
  result := a + b;
  SaveCalculation(FormatUtf8('% + %', [a, b]), result);
  TSynLog.Add.Log(sllInfo, 'Add: % + % = %', [a, b, result]);
end;

function TCalculatorService.Subtract(a, b: double): double;
begin
  result := a - b;
  SaveCalculation(FormatUtf8('% - %', [a, b]), result);
  TSynLog.Add.Log(sllInfo, 'Subtract: % - % = %', [a, b, result]);
end;

function TCalculatorService.Multiply(a, b: double): double;
begin
  result := a * b;
  SaveCalculation(FormatUtf8('% * %', [a, b]), result);
  TSynLog.Add.Log(sllInfo, 'Multiply: % * % = %', [a, b, result]);
end;

function TCalculatorService.Divide(a, b: double): double;
begin
  if b = 0 then
    raise EServiceException.CreateUtf8('Division by zero is not allowed', []);
  result := a / b;
  SaveCalculation(FormatUtf8('% / %', [a, b]), result);
  TSynLog.Add.Log(sllInfo, 'Divide: % / % = %', [a, b, result]);
end;

function TCalculatorService.GetHistory: TCalculatorResults;
var
  calculations: TOrmCalculationObjArray;
begin
  result := nil;
  if not Assigned(fRest) then
  begin
    TSynLog.Add.Log(sllError, 'GetHistory: REST ORM not assigned');
    exit;
  end;
  try
    fRest.RetrieveListObjArray(calculations, TOrmCalculation, '', []);
    fCalculationMap.ToArrayB(calculations, result);
    TSynLog.Add.Log(sllDebug, 'GetHistory: Retrieved % calculations',
      [length(result)]);
  finally
    ObjArrayClear(calculations);
  end;
end;

function TCalculatorService.ClearHistory: boolean;
begin
  result := false;
  if not Assigned(fRest) then
  begin
    TSynLog.Add.Log(sllError, 'ClearHistory: REST ORM not assigned');
    exit;
  end;
  result := fRest.TableHasRows(TOrmCalculation);
  if result then
  begin
    fRest.Execute('DELETE FROM Calculation');
    TSynLog.Add.Log(sllInfo, 'ClearHistory: History cleared');
  end;
end;

{ TUserService }

constructor TUserService.Create(const aRest: IRestOrm);
begin
  fRest := aRest;
  // Map between ORM entity and DTO
  fUserMap.Init(TypeInfo(TOrmUser), TypeInfo(TUserInfo)).
    Map(['id', 'id',
         'Username', 'username',
         'Email', 'email',
         'Created', 'created']);
  TSynLog.Add.Log(sllDebug, 'User Service created');
end;

function TUserService.CreateUser(const username, email: RawUtf8): TID;
var
  user: TOrmUser;
begin
  result := 0;
  if not Assigned(fRest) then
  begin
    TSynLog.Add.Log(sllError, 'CreateUser: REST ORM not assigned');
    exit;
  end;
  user := TOrmUser.Create;
  try
    user.Username := username;
    user.Email := email;
    user.Created := UnixMSTimeUtcFast;
    result := fRest.Add(user, true);
    TSynLog.Add.Log(sllInfo, 'CreateUser: Created user #% (%)',
      [result, username]);
  finally
    user.Free;
  end;
end;

function TUserService.GetUser(id: TID): TUserInfo;
var
  user: TOrmUser;
begin
  FillCharFast(result, SizeOf(result), 0);
  if not Assigned(fRest) then
  begin
    TSynLog.Add.Log(sllError, 'GetUser: REST ORM not assigned');
    exit;
  end;
  user := TOrmUser.Create(fRest, id);
  try
    if user.ID <> 0 then
    begin
      fUserMap.ToB(user, @result);
      TSynLog.Add.Log(sllDebug, 'GetUser: Retrieved user #%', [id]);
    end
    else
      TSynLog.Add.Log(sllWarning, 'GetUser: User #% not found', [id]);
  finally
    user.Free;
  end;
end;

function TUserService.GetAllUsers: TUserInfos;
var
  users: TOrmUserObjArray;
begin
  result := nil;
  if not Assigned(fRest) then
  begin
    TSynLog.Add.Log(sllError, 'GetAllUsers: REST ORM not assigned');
    exit;
  end;
  try
    fRest.RetrieveListObjArray(users, TOrmUser, '', []);
    fUserMap.ToArrayB(users, result);
    TSynLog.Add.Log(sllDebug, 'GetAllUsers: Retrieved % users',
      [length(result)]);
  finally
    ObjArrayClear(users);
  end;
end;

function TUserService.DeleteUser(id: TID): boolean;
begin
  result := false;
  if not Assigned(fRest) then
  begin
    TSynLog.Add.Log(sllError, 'DeleteUser: REST ORM not assigned');
    exit;
  end;
  result := fRest.Delete(TOrmUser, id);
  if result then
    TSynLog.Add.Log(sllInfo, 'DeleteUser: Deleted user #%', [id])
  else
    TSynLog.Add.Log(sllWarning, 'DeleteUser: Failed to delete user #%', [id]);
end;

end.
