unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.interfaces;

type
  /// unique identifier for greeting records
  TGreetingID = TID;

  /// Data Transfer Object for greeting information
  TGreetingDTO = packed record
    id: TGreetingID;
    name: RawUtf8;
    msg: RawUtf8;
    created: TUnixMSTime;
  end;

  /// array of greeting DTOs
  TGreetingDTOs = array of TGreetingDTO;

  /// Greeting Service API
  // - Simple REST API for greeting management
  IGreetingService = interface(IInvokable)
    ['{8F7D3A1E-9C2B-4F6D-A5E8-1B4C7D9F2E3A}']
    /// Create a new greeting
    function CreateGreeting(const name, message: RawUtf8): TGreetingID;
    /// Get a greeting by ID
    function GetGreeting(id: TGreetingID): TGreetingDTO;
    /// Get all greetings
    function GetAllGreetings: TGreetingDTOs;
    /// Delete a greeting by ID
    function DeleteGreeting(id: TGreetingID): boolean;
  end;

implementation

end.
