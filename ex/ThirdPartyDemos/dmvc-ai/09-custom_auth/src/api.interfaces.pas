unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.interfaces;

type
  /// Public API - No authentication required
  IPublicApi = interface(IInvokable)
    ['{8E4A1F21-9B3C-4D2F-A1E5-2C3D4E5F6A7B}']

    /// Public endpoint accessible without authentication
    // Returns: "Hello World" message
    function Index: RawUtf8;
  end;

  /// Private API - Authentication required
  // Different methods require different roles:
  // - Index: Requires 'admin' role
  // - PublicAction: No authentication required (exception)
  // - OnlyRole1: Requires 'role1' role
  // - OnlyRole2: Requires 'role2' role
  IPrivateApi = interface(IInvokable)
    ['{9F5B2E32-AC4D-5E3F-B2F6-3D4E5F6A7B8C}']

    /// Admin-only endpoint
    // Requires: 'admin' role
    // Returns: "Hello World" message
    function Index: RawUtf8;

    /// Public action in private controller - No auth required
    // This demonstrates a public endpoint within a private controller
    // Returns: Success message
    function PublicAction: RawUtf8;

    /// Role1-only endpoint
    // Requires: 'role1' role
    // Returns: Success message for role1
    function OnlyRole1: RawUtf8;

    /// Role2-only endpoint
    // Requires: 'role2' role
    // Returns: Success message for role2
    function OnlyRole2: RawUtf8;
  end;

implementation

initialization
  TInterfaceFactory.RegisterInterfaces([
    TypeInfo(IPublicApi),
    TypeInfo(IPrivateApi)
  ]);

end.
