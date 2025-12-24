unit api.interfaces;

interface

uses
  mormot.core.base,
  mormot.core.interfaces;

type
  /// Advanced session API interface
  ISessionApi = interface(IInvokable)
    ['{E9F8D7C6-B5A4-9382-8176-655463524140}']

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

    /// Get all active sessions (admin only)
    function ListActiveSessions: RawUtf8;

    /// Increment a counter in the session
    function IncrementCounter(const key: RawUtf8): integer;
  end;

  /// Shopping cart API for demonstrating per-session service instances
  ICartApi = interface(IInvokable)
    ['{D8E7C6B5-A4F3-8271-7065-544352413141}']

    /// Add item to cart
    function AddItem(const itemId: RawUtf8; quantity: integer): RawUtf8;

    /// Remove item from cart
    function RemoveItem(const itemId: RawUtf8): boolean;

    /// Get cart contents
    function GetCart: RawUtf8;

    /// Clear cart
    function ClearCart: boolean;

    /// Get cart total
    function GetTotal: currency;
  end;

implementation

end.
