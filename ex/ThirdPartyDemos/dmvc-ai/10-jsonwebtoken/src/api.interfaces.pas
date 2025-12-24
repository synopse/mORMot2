unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.interfaces;

type
  /// Public API - accessible without authentication
  IPublicService = interface(IInvokable)
    ['{2E5C8E1A-3F4B-4C9D-8E6F-9A1B2C3D4E5F}']
    /// Returns a public message
    function GetPublicMessage: RawUtf8;
  end;

  /// Protected API - requires JWT authentication
  IProtectedService = interface(IInvokable)
    ['{3F6D9E2B-4A5C-5D0E-9F7A-0B2C3D4E5F6A}']
    /// Returns user info from JWT token (requires role1)
    // Returns JSON: {"user":"username","roles":["role1"],"customClaims":{...}}
    function GetUserInfo: RawUtf8;

    /// Returns admin info (requires role2)
    function GetAdminInfo: RawUtf8;
  end;

implementation

initialization
  TInterfaceFactory.RegisterInterfaces([
    TypeInfo(IPublicService),
    TypeInfo(IProtectedService)
  ]);

end.
