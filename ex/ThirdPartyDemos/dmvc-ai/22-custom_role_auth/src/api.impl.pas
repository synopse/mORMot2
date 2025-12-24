unit api.impl;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.interfaces,
  mormot.soa.server,
  api.interfaces;

type
  /// Private API implementation
  TPrivateApi = class(TInjectableObjectRest, IPrivateApi)
  public
    function Index: RawUtf8;
    function PublicAction: RawUtf8;
    function OnlyRole1: RawUtf8;
    function OnlyRole2: RawUtf8;
    function OnlyRole1And2: RawUtf8;
    function OnlyRole1Or2: RawUtf8;
    function AccessByRole(const role: RawUtf8): RawUtf8;
  end;

implementation

uses
  mormot.core.text;

{ TPrivateApi }

function TPrivateApi.Index: RawUtf8;
begin
  Result := 'Hello World';
end;

function TPrivateApi.PublicAction: RawUtf8;
begin
  Result := 'OK from a public action (no login required)';
end;

function TPrivateApi.OnlyRole1: RawUtf8;
begin
  Result := 'OK from a "role1" action';
end;

function TPrivateApi.OnlyRole2: RawUtf8;
begin
  Result := 'OK from a "role2" action';
end;

function TPrivateApi.OnlyRole1And2: RawUtf8;
begin
  Result := 'OK from a "role1 and role2" action';
end;

function TPrivateApi.OnlyRole1Or2: RawUtf8;
begin
  Result := 'OK from a "role1 or role2" action';
end;

function TPrivateApi.AccessByRole(const role: RawUtf8): RawUtf8;
begin
  Result := FormatUtf8('OK This resource was accessed by role: %', [role]);
end;

end.
