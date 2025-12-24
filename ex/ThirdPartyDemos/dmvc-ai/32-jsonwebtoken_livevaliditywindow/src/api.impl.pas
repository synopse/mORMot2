unit api.impl;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.text,
  mormot.core.json,
  mormot.core.variants,
  mormot.rest.server,
  mormot.soa.server,
  api.interfaces;

type
  TPublicApi = class(TInjectableObjectRest, IPublicApi)
  public
    function PublicInfo: RawUtf8;
  end;

  TAdminRole1Api = class(TInjectableObjectRest, IAdminRole1Api)
  public
    function ProtectedRole1: RawUtf8;
    function ProtectedRole1Json: RawUtf8;
  end;

  TAdminRole2Api = class(TInjectableObjectRest, IAdminRole2Api)
  public
    function ProtectedRole2: RawUtf8;
  end;

implementation

{ TPublicApi }

function TPublicApi.PublicInfo: RawUtf8;
begin
  Result := 'This is a public section - no authentication required';
end;

{ TAdminRole1Api }

function TAdminRole1Api.ProtectedRole1: RawUtf8;
var
  user: RawUtf8;
  ctx: PServiceRunningContext;
begin
  // Access authenticated user info from ServiceRunningContext
  ctx := ServiceRunningContext;
  if ctx <> nil then
  begin
    user := ctx^.Request.AuthenticationBearerToken;
    // Format response
    Result := FormatUtf8('Hey! Hello %, you are a logged user with role1 access!', [user]);
  end
  else
    Result := 'Authentication context not available';
end;

function TAdminRole1Api.ProtectedRole1Json: RawUtf8;
var
  doc: TDocVariantData;
begin
  doc.InitObject(['message', 'This is protected content accessible only by users with role1',
                  'timestamp', DateTimeToStr(Now)]);
  Result := doc.ToJson;
end;

{ TAdminRole2Api }

function TAdminRole2Api.ProtectedRole2: RawUtf8;
var
  user: RawUtf8;
  ctx: PServiceRunningContext;
begin
  // Access authenticated user info from ServiceRunningContext
  ctx := ServiceRunningContext;
  if ctx <> nil then
  begin
    user := ctx^.Request.AuthenticationBearerToken;
    Result := FormatUtf8('Hey! Hello %, you are a logged user with role2 access!', [user]);
  end
  else
    Result := 'Authentication context not available';
end;

end.
