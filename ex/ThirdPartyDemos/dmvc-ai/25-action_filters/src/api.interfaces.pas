unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.interfaces;

type
  /// Action Filters API interface
  // Port of DMVC TActionFiltersController
  IActionFiltersApi = interface(IInvokable)
    ['{8B5A1F3C-9D2E-4F6A-B7C1-3E8A5D9F2C4B}']

    /// Get person by ID
    // Port of TActionFiltersController.GetPerson
    // Demonstrates action filters on service method
    function GetPerson(const id: RawUtf8): RawUtf8;
  end;

implementation

end.
