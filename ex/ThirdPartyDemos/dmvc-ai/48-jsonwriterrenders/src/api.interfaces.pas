unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.json,
  mormot.core.interfaces,
  mormot.rest.core;

type
  // User DTO
  TUserDTO = packed record
    UserName: RawUtf8;
  end;

  TUserDTODynArray = array of TUserDTO;

  // Users list with custom rendering
  TUsersListDTO = packed record
    Users: TUserDTODynArray;
  end;

  // Main API interface for JSON writer renders sample
  IJSONWriterSample = interface(IInvokable)
    ['{B9E3D4F5-A6C7-48B9-9AD1-2E3F4B5C6D7E}']

    // Get users list as custom JSON
    function GetUsers: TUsersListDTO;
  end;

implementation

initialization
  TInterfaceFactory.RegisterInterfaces([
    TypeInfo(IJSONWriterSample)
  ]);

end.
