unit api.impl;

{$I mormot.defines.inc}

interface

uses
  sysutils,
  mormot.core.base,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.json,
  mormot.core.data,
  mormot.core.variants,
  mormot.core.interfaces,
  mormot.rest.core,
  mormot.rest.server,
  mormot.soa.server,
  api.interfaces,
  authentication;

type
  /// Basic Auth API Implementation
  // Port of DMVC TApp1MainController and TAdminController
  TBasicAuthApi = class(TInjectableObjectRest, IBasicAuthApi)
  private
    fCurrentUser: RawUtf8;
    fCurrentRoles: TRawUtf8DynArray;
  public
    /// Public endpoint
    function PublicSection: RawUtf8;

    /// Index redirect
    function Index: RawUtf8;

    /// Protected endpoints
    function OnlyRole1: RawUtf8;
    function OnlyRole1Json(const par1: RawUtf8): RawUtf8;
    function OnlyRole2: RawUtf8;

    /// Set authentication context (called by server auth filter)
    procedure SetAuthContext(const aUserName: RawUtf8;
      const aRoles: TRawUtf8DynArray);
  end;

implementation

{ TBasicAuthApi }

procedure TBasicAuthApi.SetAuthContext(const aUserName: RawUtf8;
  const aRoles: TRawUtf8DynArray);
begin
  fCurrentUser := aUserName;
  fCurrentRoles := aRoles;
end;

function TBasicAuthApi.PublicSection: RawUtf8;
begin
  Result := 'This is a public section';
end;

function TBasicAuthApi.Index: RawUtf8;
begin
  // In DMVC this redirects to /index.html
  // In mORMot2, we return a simple message
  Result := 'Index page - Authentication demo. Try /BasicAuthApi/PublicSection or protected endpoints';
end;

function TBasicAuthApi.OnlyRole1: RawUtf8;
begin
  Result := FormatUtf8('Hey! Hello %, now you are a logged user and this is a protected content!' + #13#10 +
    'As logged user you have the following roles:' + #13#10 + '%',
    [fCurrentUser, RawUtf8ArrayToCsv(fCurrentRoles, #13#10)]);
end;

function TBasicAuthApi.OnlyRole1Json(const par1: RawUtf8): RawUtf8;
var
  doc: TDocVariantData;
begin
  doc.InitObject(['message', 'This is protected content accessible only by role1',
                  'parameter', par1,
                  'user', fCurrentUser]);
  Result := doc.ToJson;
end;

function TBasicAuthApi.OnlyRole2: RawUtf8;
begin
  Result := FormatUtf8('Hey! Hello %, now you are a logged user and this is a protected content!' + #13#10 +
    'As logged user you have the following roles:' + #13#10 + '%',
    [fCurrentUser, RawUtf8ArrayToCsv(fCurrentRoles, #13#10)]);
end;

end.
