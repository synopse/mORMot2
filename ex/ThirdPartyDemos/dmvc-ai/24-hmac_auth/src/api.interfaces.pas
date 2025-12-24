unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.interfaces;

type
  {$M+}
  /// Protected API that requires HMAC authentication
  ISecureApi = interface(IInvokable)
    ['{F2A3B4C5-D6E7-4890-AB12-CD34EF567890}']
    /// Get secret data (requires valid HMAC signature)
    function GetSecretData: RawUtf8;
    /// Get user info (requires valid HMAC signature)
    function GetUserInfo(const UserId: RawUtf8): RawUtf8;
    /// Submit data (requires valid HMAC signature)
    function SubmitData(const Data: RawUtf8): RawUtf8;
  end;
  {$M-}

implementation

end.
