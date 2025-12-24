unit jsonrpc.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.interfaces;

type
  /// Calculator operation result
  TCalculatorResult = packed record
    operation: RawUtf8;
    result: double;
    timestamp: TUnixMSTime;
  end;

  /// Array of calculator results
  TCalculatorResults = array of TCalculatorResult;

  /// Calculator Service API using JSON-RPC 2.0
  // - Demonstrates mORMot2's automatic JSON-RPC support
  // - All methods are exposed as POST endpoints with JSON bodies
  // - Example: POST /CalculatorService/Add {"a":5,"b":3}
  ICalculatorService = interface(IInvokable)
    ['{A5B8C3D4-E6F7-4A9B-8C1D-2E3F4A5B6C7D}']
    /// Add two numbers
    function Add(a, b: double): double;
    /// Subtract two numbers
    function Subtract(a, b: double): double;
    /// Multiply two numbers
    function Multiply(a, b: double): double;
    /// Divide two numbers (raises exception if b = 0)
    function Divide(a, b: double): double;
    /// Get calculation history
    function GetHistory: TCalculatorResults;
    /// Clear calculation history
    function ClearHistory: boolean;
  end;

  /// User information DTO
  TUserInfo = packed record
    id: TID;
    username: RawUtf8;
    email: RawUtf8;
    created: TUnixMSTime;
  end;

  /// Array of user information
  TUserInfos = array of TUserInfo;

  /// User Service API using JSON-RPC 2.0
  // - Demonstrates multiple services in one server
  IUserService = interface(IInvokable)
    ['{B6C9D4E5-F7A8-4B0C-9D2E-3F4A5B6C7D8E}']
    /// Create a new user
    function CreateUser(const username, email: RawUtf8): TID;
    /// Get user by ID
    function GetUser(id: TID): TUserInfo;
    /// Get all users
    function GetAllUsers: TUserInfos;
    /// Delete user by ID
    function DeleteUser(id: TID): boolean;
  end;

implementation

end.
