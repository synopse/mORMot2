unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.interfaces;

type
  /// File information record
  TUploadedFileInfo = packed record
    FileName: RawUtf8;
    Size: Int64;
  end;
  TUploadedFileInfoDynArray = array of TUploadedFileInfo;

  /// File upload API interface
  IFileUploadApi = interface(IInvokable)
    ['{8F5E9A2B-1C3D-4E5F-9A7B-2C3D4E5F6A7B}']

    /// Get list of uploaded files
    function GetUploadedFiles(out Files: TUploadedFileInfoDynArray): Boolean;
  end;

implementation

end.
