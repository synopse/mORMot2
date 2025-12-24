unit api.interfaces;

interface

uses
  mormot.core.base,
  mormot.core.json,
  mormot.core.interfaces,
  mormot.rest.core;

type
  // File download DTO
  TFileDownloadDTO = packed record
    FileName: RawUtf8;
    ContentType: RawUtf8;
    Data: RawByteString; // Base64 encoded for JSON transport
  end;

  // File upload response DTO
  TFileUploadResponseDTO = packed record
    Success: Boolean;
    Message: RawUtf8;
    SavedFileName: RawUtf8;
    Reference: RawUtf8;
  end;

  // Main API interface for binary content rendering
  IBinaryContentSample = interface(IInvokable)
    ['{D3E4F5A6-B7C8-49DA-AB2E-3F4C5D6E7F8A}']

    // Download file by name (returns binary content as base64 in JSON)
    function GetFileByName(const FileName: RawUtf8): TFileDownloadDTO;

    // Alternative download method (same functionality, different approach)
    function GetStreamByFileName(const FileName: RawUtf8): TFileDownloadDTO;

    // Upload binary data (receives base64 encoded data)
    function UploadBinaryData(const fieldname, filename, contenttype: RawUtf8;
      const data: RawByteString): TFileUploadResponseDTO;
  end;

implementation

initialization
  TInterfaceFactory.RegisterInterfaces([
    TypeInfo(IBinaryContentSample)
  ]);

end.
