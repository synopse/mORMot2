unit api.impl;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.interfaces,
  mormot.rest.server,
  mormot.soa.server,
  api.interfaces;

type
  /// Public API implementation
  TPublicApi = class(TInjectableObjectRest, IPublicApi)
  public
    function Index: RawUtf8;
  end;

  /// Private API implementation
  TPrivateApi = class(TInjectableObjectRest, IPrivateApi)
  public
    function Index: RawUtf8;
    function PublicAction: RawUtf8;
    function OnlyRole1: RawUtf8;
    function OnlyRole2: RawUtf8;
  end;

implementation

{ TPublicApi }

function TPublicApi.Index: RawUtf8;
begin
  Result := 'Hello World';
end;

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

end.
