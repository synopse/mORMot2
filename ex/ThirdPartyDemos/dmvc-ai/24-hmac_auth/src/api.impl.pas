unit api.impl;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.text,
  mormot.core.json,
  mormot.core.unicode,
  mormot.core.datetime,
  mormot.core.interfaces,
  mormot.core.log,
  api.interfaces;

type
  /// Implementation of the secure API
  TSecureApi = class(TInjectableObject, ISecureApi)
  public
    function GetSecretData: RawUtf8;
    function GetUserInfo(const UserId: RawUtf8): RawUtf8;
    function SubmitData(const Data: RawUtf8): RawUtf8;
  end;

implementation

{ TSecureApi }

function TSecureApi.GetSecretData: RawUtf8;
begin
  TSynLog.Add.Log(sllInfo, 'GetSecretData called - HMAC verified');
  Result := JsonEncode([
    'secret', 'This is protected data that required HMAC authentication',
    'timestamp', DateTimeToIso8601(Now, True),
    'accessLevel', 'confidential'
  ]);
end;

function TSecureApi.GetUserInfo(const UserId: RawUtf8): RawUtf8;
begin
  TSynLog.Add.Log(sllInfo, 'GetUserInfo called for user: %', [UserId]);
  Result := JsonEncode([
    'userId', UserId,
    'name', 'Demo User',
    'email', 'user@example.com',
    'verified', True,
    'lastAccess', DateTimeToIso8601(Now, True)
  ]);
end;

function TSecureApi.SubmitData(const Data: RawUtf8): RawUtf8;
begin
  TSynLog.Add.Log(sllInfo, 'SubmitData called with: %', [Data]);
  Result := JsonEncode([
    'status', 'accepted',
    'message', 'Data received and verified via HMAC',
    'receivedLength', Length(Data),
    'timestamp', DateTimeToIso8601(Now, True)
  ]);
end;

end.
