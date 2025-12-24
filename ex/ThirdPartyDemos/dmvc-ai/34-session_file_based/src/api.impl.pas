unit api.impl;

interface

uses
  mormot.core.base,
  mormot.core.interfaces,
  mormot.rest.server,
  mormot.soa.server,
  mormot.core.json,
  mormot.core.data,
  mormot.core.variants,
  api.interfaces;

type
  /// Session API implementation
  TSessionApi = class(TInjectableObjectRest, ISessionApi)
  public
    function SetValue(const key, value: RawUtf8): RawUtf8;
    function GetValue(const key: RawUtf8): RawUtf8;
    function ListKeys: TRawUtf8DynArray;
    function DeleteValue(const key: RawUtf8): boolean;
    function ClearSession: boolean;
    function SessionInfo: RawUtf8;
  end;

implementation

uses
  mormot.core.os,
  mormot.rest.core,
  SysUtils;

{ TSessionApi }

function TSessionApi.SetValue(const key, value: RawUtf8): RawUtf8;
var
  user: TAuthUser;
  doc: TDocVariantData;
begin
  user := Server.SessionGetUser(CurrentServiceContext.Request.Session);
  if user = nil then
  begin
    Result := JsonEncode(['error', 'No session found']);
    exit;
  end;

  // Parse existing session data as JSON object
  doc.InitJson(user.Data, JSON_FAST);

  // Set the key
  doc.Value[key] := value;

  // Save back to session
  user.Data := doc.ToJson;

  Result := JsonEncode(['success', true, 'key', key, 'value', value]);
end;

function TSessionApi.GetValue(const key: RawUtf8): RawUtf8;
var
  user: TAuthUser;
  doc: TDocVariantData;
  value: variant;
begin
  user := Server.SessionGetUser(CurrentServiceContext.Request.Session);
  if user = nil then
  begin
    Result := JsonEncode(['error', 'No session found']);
    exit;
  end;

  // Parse session data as JSON object
  doc.InitJson(user.Data, JSON_FAST);

  // Get the value
  if doc.GetValueOrDefault(key, value) then
    Result := JsonEncode(['key', key, 'value', value])
  else
    Result := JsonEncode(['error', 'Key not found', 'key', key]);
end;

function TSessionApi.ListKeys: TRawUtf8DynArray;
var
  user: TAuthUser;
  doc: TDocVariantData;
  i: integer;
begin
  user := Server.SessionGetUser(CurrentServiceContext.Request.Session);
  if user = nil then
    exit;

  // Parse session data as JSON object
  doc.InitJson(user.Data, JSON_FAST);

  SetLength(Result, doc.Count);
  for i := 0 to doc.Count - 1 do
    Result[i] := doc.Names[i];
end;

function TSessionApi.DeleteValue(const key: RawUtf8): boolean;
var
  user: TAuthUser;
  doc: TDocVariantData;
begin
  Result := false;
  user := Server.SessionGetUser(CurrentServiceContext.Request.Session);
  if user = nil then
    exit;

  // Parse session data as JSON object
  doc.InitJson(user.Data, JSON_FAST);

  // Delete the key
  Result := doc.Delete(key);

  if Result then
    user.Data := doc.ToJson;
end;

function TSessionApi.ClearSession: boolean;
var
  user: TAuthUser;
begin
  Result := false;
  user := Server.SessionGetUser(CurrentServiceContext.Request.Session);
  if user = nil then
    exit;

  // Clear all session data
  user.Data := '{}';
  Result := true;
end;

function TSessionApi.SessionInfo: RawUtf8;
var
  user: TAuthUser;
  info: TDocVariantData;
begin
  user := Server.SessionGetUser(CurrentServiceContext.Request.Session);
  if user = nil then
  begin
    Result := JsonEncode(['error', 'No session found']);
    exit;
  end;

  info.InitFast;
  info.AddValue('session_id', CurrentServiceContext.Request.Session);
  info.AddValue('user', user.LogonName);
  info.AddValue('user_id', user.ID);
  info.AddValue('data', user.Data);

  Result := info.ToJson;
end;

end.
