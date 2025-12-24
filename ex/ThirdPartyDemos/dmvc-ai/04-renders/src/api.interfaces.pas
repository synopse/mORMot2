unit api.interfaces;

interface

uses
  mormot.core.base,
  mormot.core.json,
  mormot.core.interfaces,
  mormot.rest.core;

type
  // DTOs for data transfer
  TPersonDTO = packed record
    id: TID;
    firstname: RawUtf8;
    lastname: RawUtf8;
    dob: TDateTime;
    married: Boolean;
  end;

  TCustomerDTO = packed record
    id: TID;
    name: RawUtf8;
    contactfirst: RawUtf8;
    contactlast: RawUtf8;
    addressline1: RawUtf8;
    city: RawUtf8;
  end;

  // Dynamic arrays for lists
  TPersonDTODynArray = array of TPersonDTO;
  TCustomerDTODynArray = array of TCustomerDTO;

  // Simple types for demonstration
  TSimpleArraysDTO = packed record
    integers: TIntegerDynArray;
    strings: TRawUtf8DynArray;
    doubles: TDoubleDynArray;
  end;

  // Metadata container
  TMetadataDTO = packed record
    startProcessing: TDateTime;
    stopProcessing: TDateTime;
    customData: RawUtf8;
  end;

  // People with metadata
  TPeopleWithMetadataDTO = packed record
    items: TPersonDTODynArray;
    metadata: TMetadataDTO;
  end;

  // Main API interface for renders sample
  IRendersSample = interface(IInvokable)
    ['{A8F2C3D4-E5B6-47A8-89C0-1D2E3F4A5B6C}']

    // JSON rendering - objects
    function GetPerson(id: TID): TPersonDTO;
    function GetPeople: TPersonDTODynArray;
    function GetCustomer(id: TID): TCustomerDTO;
    function GetCustomers: TCustomerDTODynArray;

    // JSON rendering - with metadata
    function GetPeopleWithMetadata: TPeopleWithMetadataDTO;

    // Plain text rendering
    function GetPersonAsText(id: TID): RawUtf8;

    // CSV rendering - WRONG WAY (wraps in JSON envelope)
    function GetPeopleAsCSV: RawUtf8;

    // CSV rendering - CORRECT WAY (raw CSV with proper content-type)
    procedure GetPeopleAsRawCSV;

    // Simple arrays
    function GetSimpleArrays: TSimpleArraysDTO;

    // Binary data - WRONG WAY (base64 encoded in JSON transport)
    function GetBinaryData(filename: RawUtf8): RawByteString;

    // Binary data - CORRECT WAY (raw binary with proper content-type)
    procedure GetRawBinaryData(filename: RawUtf8);

    // File upload (receives base64 encoded data)
    function UploadBinaryData(const fieldname, filename, contenttype: RawUtf8;
      const data: RawByteString): RawUtf8;
  end;

implementation

initialization
  TInterfaceFactory.RegisterInterfaces([
    TypeInfo(IRendersSample)
  ]);

end.
