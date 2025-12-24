unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.interfaces;

type
  /// Public API - no authentication required
  IPublicApi = interface(IInvokable)
    ['{8E7D1A2B-5C3F-4D6E-9A1B-2C3D4E5F6A7B}']
    /// Returns public information
    function PublicInfo: RawUtf8;
  end;

  /// Admin API - requires role1
  IAdminRole1Api = interface(IInvokable)
    ['{7F6E5D4C-3B2A-1E9F-8D7C-6B5A4E3D2C1B}']
    /// Returns role1 protected content
    function ProtectedRole1: RawUtf8;
    /// Returns role1 protected JSON content
    function ProtectedRole1Json: RawUtf8;
  end;

  /// Admin API - requires role2
  IAdminRole2Api = interface(IInvokable)
    ['{6E5D4C3B-2A1F-9E8D-7C6B-5A4E3D2C1B0A}']
    /// Returns role2 protected content
    function ProtectedRole2: RawUtf8;
  end;

implementation

end.
