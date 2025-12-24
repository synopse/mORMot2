unit api.interfaces;

interface

uses
  mormot.core.base,
  mormot.core.interfaces;

type
  /// Session API interface
  ISessionApi = interface(IInvokable)
    ['{F8E7D6C5-B4A3-9281-7065-544352413039}']

    /// Store a value in the session
    function SetValue(const key, value: RawUtf8): RawUtf8;

    /// Retrieve a value from the session
    function GetValue(const key: RawUtf8): RawUtf8;

    /// List all session keys
    function ListKeys: TRawUtf8DynArray;

    /// Delete a value from the session
    function DeleteValue(const key: RawUtf8): boolean;

    /// Clear all session data
    function ClearSession: boolean;

    /// Get session information
    function SessionInfo: RawUtf8;
  end;

implementation

end.
