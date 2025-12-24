unit api.impl;

{$I mormot.defines.inc}

interface

uses
  sysutils,
  mormot.core.base,
  mormot.core.text,
  mormot.core.rtti,
  mormot.core.variants,
  mormot.core.interfaces,
  mormot.core.datetime,
  mormot.core.os,
  mormot.soa.core,
  api.interfaces;

type
  /// Implementation of the Routing API
  /// This demonstrates different routing patterns in mORMot2
  TRoutingApi = class(TInterfacedObject, IRoutingApi)
  protected
    fUsers: TUserDTOs; // In-memory user storage for demo
    procedure InitializeSampleData;
    function FindUserById(id: TUserID): integer;
  public
    constructor Create; reintroduce;
    // IRoutingApi methods
    function GetUser(id: TUserID): TUserDTO;
    function GetUserDetails(id: TUserID; includeStats: boolean): RawUtf8;
    function ListUsers(const filter: RawUtf8; limit: integer): TUserDTOs;
    function Search(const term: RawUtf8): TSearchResultDTO;
    function CreateUser(const name, email: RawUtf8): TUserID;
    function UpdateUser(id: TUserID; const name, email: RawUtf8): boolean;
    function DeleteUser(id: TUserID): boolean;
    function GetUsersByStatus(const status: RawUtf8; page, pageSize: integer): TUserDTOs;
    function BatchDeleteUsers(const ids: TInt64DynArray): integer;
  end;


implementation


{ TRoutingApi }

constructor TRoutingApi.Create;
begin
  inherited Create;
  InitializeSampleData;
end;

procedure TRoutingApi.InitializeSampleData;
var
  i: integer;
begin
  // Create 50 sample users
  SetLength(fUsers, 50);
  for i := 0 to High(fUsers) do
  begin
    fUsers[i].ID := i + 1;
    fUsers[i].Name := FormatUtf8('User %', [i + 1]);
    fUsers[i].Email := FormatUtf8('user%@example.com', [i + 1]);
    if (i mod 3) = 0 then
      fUsers[i].Status := 'active'
    else if (i mod 3) = 1 then
      fUsers[i].Status := 'inactive'
    else
      fUsers[i].Status := 'pending';
  end;
end;

function TRoutingApi.FindUserById(id: TUserID): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to High(fUsers) do
    if fUsers[i].ID = id then
    begin
      result := i;
      exit;
    end;
end;

function TRoutingApi.GetUser(id: TUserID): TUserDTO;
var
  idx: integer;
begin
  // Example: /api/users/123 in DMVC
  // mORMot2: POST /root/RoutingApi.GetUser with {"id":123}
  FillCharFast(result, SizeOf(result), 0);
  idx := FindUserById(id);
  if idx >= 0 then
    result := fUsers[idx]
  else
    raise EServiceException.CreateUtf8('User % not found', [id]);
end;

function TRoutingApi.GetUserDetails(id: TUserID; includeStats: boolean): RawUtf8;
var
  user: TUserDTO;
  doc: TDocVariantData;
begin
  // Example: /api/users/123/details?stats=true in DMVC
  // mORMot2: POST /root/RoutingApi.GetUserDetails with {"id":123,"includeStats":true}
  user := GetUser(id);

  doc.InitFast;
  doc.AddValue('id', user.ID);
  doc.AddValue('name', user.Name);
  doc.AddValue('email', user.Email);
  doc.AddValue('status', user.Status);

  if includeStats then
  begin
    doc.AddValue('loginCount', Random(1000));
    doc.AddValue('lastLogin', DateTimeToIso8601Text(NowUtc));
    doc.AddValue('accountAge', Random(365));
  end;

  result := doc.ToJson;
end;

function TRoutingApi.ListUsers(const filter: RawUtf8; limit: integer): TUserDTOs;
var
  i, count: integer;
  filterLower: RawUtf8;
begin
  // Example: /api/users?filter=active&limit=10 in DMVC
  // mORMot2: POST /root/RoutingApi.ListUsers with {"filter":"active","limit":10}

  result := nil;
  filterLower := LowerCase(filter);
  count := 0;

  // Apply filter if specified
  if filterLower <> '' then
  begin
    for i := 0 to High(fUsers) do
    begin
      if (fUsers[i].Status = filterLower) and ((limit = 0) or (count < limit)) then
      begin
        SetLength(result, count + 1);
        result[count] := fUsers[i];
        inc(count);
      end;
    end;
  end
  else
  begin
    // No filter - return all (up to limit)
    if (limit > 0) and (limit < Length(fUsers)) then
    begin
      SetLength(result, limit);
      MoveFast(fUsers[0], result[0], limit * SizeOf(TUserDTO));
    end
    else
      result := copy(fUsers);
  end;
end;

function TRoutingApi.Search(const term: RawUtf8): TSearchResultDTO;
var
  i: integer;
  termLower, nameLower, emailLower: RawUtf8;
begin
  // Example: /api/search/john in DMVC
  // mORMot2: POST /root/RoutingApi.Search with {"term":"john"}

  FillCharFast(result, SizeOf(result), 0);
  result.Term := term;
  result.Items := nil;

  if term = '' then
    exit;

  termLower := LowerCase(term);

  for i := 0 to High(fUsers) do
  begin
    nameLower := LowerCase(fUsers[i].Name);
    emailLower := LowerCase(fUsers[i].Email);

    if (Pos(termLower, nameLower) > 0) or (Pos(termLower, emailLower) > 0) then
    begin
      SetLength(result.Items, result.ResultCount + 1);
      result.Items[result.ResultCount] := FormatUtf8('% (%) - %',
        [fUsers[i].Name, fUsers[i].ID, fUsers[i].Email]);
      inc(result.ResultCount);
    end;
  end;
end;

function TRoutingApi.CreateUser(const name, email: RawUtf8): TUserID;
var
  newUser: TUserDTO;
begin
  // Example: POST /api/users in DMVC
  // mORMot2: POST /root/RoutingApi.CreateUser with {"name":"John","email":"john@example.com"}

  if (name = '') or (email = '') then
    raise EServiceException.Create('Name and email are required');

  // Generate new ID
  result := Length(fUsers) + 1;

  newUser.ID := result;
  newUser.Name := name;
  newUser.Email := email;
  newUser.Status := 'pending';

  // Add to array
  SetLength(fUsers, Length(fUsers) + 1);
  fUsers[High(fUsers)] := newUser;
end;

function TRoutingApi.UpdateUser(id: TUserID; const name, email: RawUtf8): boolean;
var
  idx: integer;
begin
  // Example: PUT /api/users/123 in DMVC
  // mORMot2: POST /root/RoutingApi.UpdateUser with {"id":123,"name":"Jane","email":"jane@example.com"}

  result := false;
  idx := FindUserById(id);
  if idx < 0 then
    raise EServiceException.CreateUtf8('User % not found', [id]);

  if name <> '' then
    fUsers[idx].Name := name;
  if email <> '' then
    fUsers[idx].Email := email;

  result := true;
end;

function TRoutingApi.DeleteUser(id: TUserID): boolean;
var
  idx, i: integer;
begin
  // Example: DELETE /api/users/123 in DMVC
  // mORMot2: POST /root/RoutingApi.DeleteUser with {"id":123}

  result := false;
  idx := FindUserById(id);
  if idx < 0 then
    exit;

  // Remove from array
  for i := idx to High(fUsers) - 1 do
    fUsers[i] := fUsers[i + 1];
  SetLength(fUsers, Length(fUsers) - 1);

  result := true;
end;

function TRoutingApi.GetUsersByStatus(const status: RawUtf8; page, pageSize: integer): TUserDTOs;
var
  i, count, skip, take: integer;
  statusLower: RawUtf8;
begin
  // Example: /api/users/status/active?page=1&pageSize=20 in DMVC
  // mORMot2: POST /root/RoutingApi.GetUsersByStatus with {"status":"active","page":1,"pageSize":20}

  result := nil;
  statusLower := LowerCase(status);

  if page < 1 then
    page := 1;
  if pageSize < 1 then
    pageSize := 10;

  skip := (page - 1) * pageSize;
  take := 0;
  count := 0;

  for i := 0 to High(fUsers) do
  begin
    if fUsers[i].Status = statusLower then
    begin
      if count >= skip then
      begin
        if take < pageSize then
        begin
          SetLength(result, take + 1);
          result[take] := fUsers[i];
          inc(take);
        end
        else
          break;
      end;
      inc(count);
    end;
  end;
end;

function TRoutingApi.BatchDeleteUsers(const ids: TInt64DynArray): integer;
var
  i: integer;
begin
  // Example: DELETE /api/users/batch with body [1,2,3] in DMVC
  // mORMot2: POST /root/RoutingApi.BatchDeleteUsers with {"ids":[1,2,3]}

  result := 0;
  for i := 0 to High(ids) do
    if DeleteUser(ids[i]) then
      inc(result);
end;


end.
