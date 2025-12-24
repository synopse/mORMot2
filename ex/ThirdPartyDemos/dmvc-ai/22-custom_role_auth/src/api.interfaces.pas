unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.interfaces;

type
  /// Private API - Authentication and role-based authorization
  // Different methods require different roles:
  // - Index: Requires authentication
  // - PublicAction: No authentication required (exception)
  // - OnlyRole1: Requires 'role1' role
  // - OnlyRole2: Requires 'role2' role
  // - OnlyRole1And2: Requires BOTH 'role1' AND 'role2' roles
  // - OnlyRole1Or2: Requires EITHER 'role1' OR 'role2' role
  // - AccessByRole: Requires the role specified in the parameter
  IPrivateApi = interface(IInvokable)
    ['{9F5B2E32-AC4D-5E3F-B2F6-3D4E5F6A7B8C}']

    /// Index endpoint - authentication required
    // Requires: Valid session (any authenticated user)
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

    /// Role1 AND Role2 endpoint
    // Requires: BOTH 'role1' AND 'role2' roles
    // Returns: Success message for role1 and role2
    function OnlyRole1And2: RawUtf8;

    /// Role1 OR Role2 endpoint
    // Requires: EITHER 'role1' OR 'role2' role
    // Returns: Success message for role1 or role2
    function OnlyRole1Or2: RawUtf8;

    /// Dynamic role endpoint
    // Requires: The role specified in the role parameter
    // Params: role - The required role name
    // Returns: Success message indicating the role
    function AccessByRole(const role: RawUtf8): RawUtf8;
  end;

implementation

initialization
  TInterfaceFactory.RegisterInterfaces([
    TypeInfo(IPrivateApi)
  ]);

end.
