unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.interfaces;

type
  /// Basic Authentication API Interface
  // Port of DMVC TApp1MainController and TAdminController
  IBasicAuthApi = interface(IInvokable)
    ['{D8E5F4A1-8B2C-4F6D-9E3A-1C7B4D5E6F7A}']

    /// Public endpoint - no authentication required
    // Port of TApp1MainController.PublicSection
    function PublicSection: RawUtf8;

    /// Index endpoint - redirects to static content
    // Port of TApp1MainController.Index
    function Index: RawUtf8;

    /// Protected endpoint for role1
    // Port of TAdminController.OnlyRole1
    function OnlyRole1: RawUtf8;

    /// Protected endpoint for role1 (JSON response)
    // Port of TAdminController.OnlyRole1EmittingJSON
    function OnlyRole1Json(const par1: RawUtf8): RawUtf8;

    /// Protected endpoint for role2
    // Port of TAdminController.OnlyRole2
    function OnlyRole2: RawUtf8;
  end;

implementation

end.
