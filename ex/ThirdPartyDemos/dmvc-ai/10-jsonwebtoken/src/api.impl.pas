unit api.impl;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.text,
  mormot.core.json,
  mormot.core.data,
  mormot.core.variants,
  mormot.core.interfaces,
  mormot.core.log,
  mormot.rest.core,
  mormot.rest.server,
  mormot.soa.server,
  api.interfaces,
  auth;

type
  /// Implementation of public service
  TPublicService = class(TInjectableObjectRest, IPublicService)
  public
    function GetPublicMessage: RawUtf8;
  end;

  /// Implementation of protected service with JWT context
  // Uses CurrentJwtClaims() to access actual JWT identity
  TProtectedService = class(TInjectableObjectRest, IProtectedService)
  public
    function GetUserInfo: RawUtf8;
    function GetAdminInfo: RawUtf8;
  end;

implementation

{ TPublicService }

function TPublicService.GetPublicMessage: RawUtf8;
begin
  TSynLog.Add.Log(sllInfo, 'GetPublicMessage called');
  Result := 'This is a public section - no authentication required';
end;

{ TProtectedService }

function TProtectedService.GetUserInfo: RawUtf8;
var
  doc: TDocVariantData;
  claims: TJwtClaims;
begin
  // Get actual JWT claims from the verified token
  claims := CurrentJwtClaims;

  if not claims.Valid then
  begin
    TSynLog.Add.Log(sllWarning, 'GetUserInfo called without valid JWT claims');
    Result := JsonEncode(['error', 'No valid JWT token']);
    exit;
  end;

  TSynLog.Add.Log(sllInfo, 'GetUserInfo called for user: %', [claims.UserName]);

  // Return actual JWT identity (matches DMVC example format)
  doc.InitObject([
    'user', claims.UserName,
    'roles', variant(claims.Roles),
    'customClaims', variant(claims.CustomClaims)
  ]);

  Result := doc.ToJson;
end;

function TProtectedService.GetAdminInfo: RawUtf8;
var
  doc: TDocVariantData;
  claims: TJwtClaims;
begin
  // Get actual JWT claims from the verified token
  claims := CurrentJwtClaims;

  if not claims.Valid then
  begin
    TSynLog.Add.Log(sllWarning, 'GetAdminInfo called without valid JWT claims');
    Result := JsonEncode(['error', 'No valid JWT token']);
    exit;
  end;

  TSynLog.Add.Log(sllInfo, 'GetAdminInfo called for user: %', [claims.UserName]);

  // Return actual JWT identity with admin context
  doc.InitObject([
    'message', 'This is protected admin content',
    'user', claims.UserName,
    'roles', variant(claims.Roles),
    'accessLevel', 'admin',
    'customClaims', variant(claims.CustomClaims)
  ]);

  Result := doc.ToJson;
end;

end.
