unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.rtti,
  mormot.core.interfaces;

type
  TUserID = Int64;

  /// User DTO (Data Transfer Object)
  TUserDTO = packed record
    ID: TUserID;
    Name: RawUtf8;
    Email: RawUtf8;
    Status: RawUtf8;
  end;
  TUserDTOs = array of TUserDTO;

  /// Search result DTO
  TSearchResultDTO = packed record
    Term: RawUtf8;
    ResultCount: Integer;
    Items: TRawUtf8DynArray;
  end;

  /// Routing API interface - demonstrates different routing patterns
  IRoutingApi = interface(IInvokable)
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']

    // === Path parameter examples ===
    /// Get user by ID (path parameter)
    /// Call as: POST /root/RoutingApi.GetUser with {"id":123}
    function GetUser(id: TUserID): TUserDTO;

    /// Get user details with multiple parameters
    /// Call as: POST /root/RoutingApi.GetUserDetails with {"id":123,"includeStats":true}
    function GetUserDetails(id: TUserID; includeStats: boolean): RawUtf8;

    // === Query parameter examples ===
    /// List users with optional filter
    /// Call as: POST /root/RoutingApi.ListUsers with {"filter":"active","limit":10}
    function ListUsers(const filter: RawUtf8; limit: integer): TUserDTOs;

    /// Search with term
    /// Call as: POST /root/RoutingApi.Search with {"term":"searchtext"}
    function Search(const term: RawUtf8): TSearchResultDTO;

    // === Different HTTP methods simulation ===
    /// Create a new user (simulates POST)
    /// Call as: POST /root/RoutingApi.CreateUser with {"name":"John","email":"john@example.com"}
    function CreateUser(const name, email: RawUtf8): TUserID;

    /// Update user (simulates PUT)
    /// Call as: POST /root/RoutingApi.UpdateUser with {"id":123,"name":"Jane","email":"jane@example.com"}
    function UpdateUser(id: TUserID; const name, email: RawUtf8): boolean;

    /// Delete user (simulates DELETE)
    /// Call as: POST /root/RoutingApi.DeleteUser with {"id":123}
    function DeleteUser(id: TUserID): boolean;

    // === Complex routing examples ===
    /// Get users filtered by status with pagination
    /// Call as: POST /root/RoutingApi.GetUsersByStatus with {"status":"active","page":1,"pageSize":20}
    function GetUsersByStatus(const status: RawUtf8; page, pageSize: integer): TUserDTOs;

    /// Batch operation example
    /// Call as: POST /root/RoutingApi.BatchDeleteUsers with {"ids":[1,2,3]}
    function BatchDeleteUsers(const ids: TInt64DynArray): integer;
  end;


implementation


initialization
  // Customize DTO field names for JSON (shorter names)
  Rtti.RegisterFromText(TypeInfo(TUserDTO),
    'id:Int64 name:RawUtf8 email:RawUtf8 status:RawUtf8');

  Rtti.RegisterFromText(TypeInfo(TSearchResultDTO),
    'term:RawUtf8 count:Integer items:TRawUtf8DynArray');

  // Register interface for factory
  TInterfaceFactory.RegisterInterfaces([TypeInfo(IRoutingApi)]);

end.
