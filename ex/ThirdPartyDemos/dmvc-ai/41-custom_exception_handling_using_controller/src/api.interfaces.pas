unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.interfaces;

type
  /// Exception handling API service interface
  // Port of TMyController from DMVC exception_handling_with_controller
  IExceptionHandlingApi = interface(IInvokable)
    ['{D4E5F6A7-8B9C-0D1E-2F3A-4B5C6D7E8F90}']

    /// Returns a welcome message
    // Port of Index method
    function Index: RawUtf8;

    /// Raises a custom mORMot2 exception (EServiceException)
    // Port of Index method that raises EMVCException
    function RaiseServiceError: RawUtf8;

    /// Raises a standard exception
    // Port of Error method
    function RaiseStandardError: RawUtf8;

    /// Returns HTTP 204 No Content
    // Port of GetCustomer method
    function GetCustomer(aID: Integer): RawUtf8;
  end;

implementation

end.
