unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.interfaces;

type
  /// Public API - accessible without authentication
  IPublicService = interface(IInvokable)
    ['{7A8B9C0D-1E2F-3A4B-5C6D-7E8F9A0B1C2D}']
    /// Returns a public message
    function GetPublicMessage: RawUtf8;
  end;

  /// User API - requires JWT authentication (any authenticated user)
  IUserService = interface(IInvokable)
    ['{8B9C0D1E-2F3A-4B5C-6D7E-8F9A0B1C2D3E}']
    /// Returns user profile (any authenticated user)
    function GetProfile: RawUtf8;

    /// Returns user's own data
    function GetMyData: RawUtf8;
  end;

  /// Manager API - requires 'manager' role
  IManagerService = interface(IInvokable)
    ['{9C0D1E2F-3A4B-5C6D-7E8F-9A0B1C2D3E4F}']
    /// Returns team information (requires 'manager' role)
    function GetTeamInfo: RawUtf8;

    /// Approves requests (requires 'manager' role)
    function ApproveRequest(const aRequestId: RawUtf8): RawUtf8;
  end;

  /// Admin API - requires 'admin' role
  IAdminService = interface(IInvokable)
    ['{0D1E2F3A-4B5C-6D7E-8F9A-0B1C2D3E4F5A}']
    /// Returns system info (requires 'admin' role)
    function GetSystemInfo: RawUtf8;

    /// Manages users (requires 'admin' role)
    function ManageUsers: RawUtf8;

    /// Returns all user sessions (requires 'admin' role)
    function GetAllSessions: RawUtf8;
  end;

  /// Multi-Role API - demonstrates endpoints requiring multiple roles
  IMultiRoleService = interface(IInvokable)
    ['{1E2F3A4B-5C6D-7E8F-9A0B-1C2D3E4F5A6B}']
    /// Requires both 'manager' and 'reports' roles
    function GetFinancialReport: RawUtf8;

    /// Requires either 'admin' or 'auditor' role
    function GetAuditLog: RawUtf8;
  end;

implementation

initialization
  TInterfaceFactory.RegisterInterfaces([
    TypeInfo(IPublicService),
    TypeInfo(IUserService),
    TypeInfo(IManagerService),
    TypeInfo(IAdminService),
    TypeInfo(IMultiRoleService)
  ]);

end.
