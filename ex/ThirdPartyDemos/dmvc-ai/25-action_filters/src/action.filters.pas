unit action.filters;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  DateUtils,
  TypInfo,
  mormot.core.base,
  mormot.core.text,
  mormot.core.log,
  mormot.core.os,
  mormot.core.rtti,
  mormot.core.interfaces,
  mormot.rest.core,
  mormot.rest.server,
  mormot.soa.server;

type
  /// Service-level action filter middleware
  // Port of DMVC OnBeforeAction/OnAfterAction functionality
  // Demonstrates: Pre/post processing of service method calls using per-service hooks
  // REFACTORED: Uses OnMethodExecute + AddInterceptor (service-level) instead of
  // OnBeforeUri/OnAfterUri (global server-level)
  TActionFilterMiddleware = class
  private
    fServer: TRestServer;
    fFactory: TServiceFactoryServer;
    fStartTime: Int64; // Track timing for interceptor

    /// OnMethodExecute - called before service method execution for authorization
    // Port of TMVCController.OnBeforeAction
    // Returns True if method can be executed, False to block
    function OnMethodExecute(Ctxt: TRestServerUriContext;
      const Method: TInterfaceMethod): Boolean;

    /// Interceptor - called before and after service method execution
    // Port of TMVCController.OnAfterAction
    // Logs execution details
    procedure OnInterceptMethod(Sender: TInterfaceMethodExecuteRaw;
      Step: TInterfaceMethodExecuteEventStep);
  public
    constructor Create(aServer: TRestServer; aFactory: TServiceFactoryServer);
    destructor Destroy; override;
  end;

implementation

{ TActionFilterMiddleware }

constructor TActionFilterMiddleware.Create(aServer: TRestServer;
  aFactory: TServiceFactoryServer);
begin
  inherited Create;
  fServer := aServer;
  fFactory := aFactory;

  // Hook lifecycle events
  TSynLog.Add.Log(sllInfo, 'ActionFilterMiddleware created (equivalent to MVCControllerAfterCreate)', self);

  // NEW: Use service-level hooks (more idiomatic, better isolation)
  // 1. OnMethodExecute: Authorization/validation filter (can block execution)
  fFactory.OnMethodExecute := OnMethodExecute;

  // 2. AddInterceptor: Before/after execution hook (logging, metrics)
  fFactory.AddInterceptor(OnInterceptMethod);

  TSynLog.Add.Log(sllInfo, 'Service-level filters registered: OnMethodExecute + Interceptor', self);
end;

destructor TActionFilterMiddleware.Destroy;
begin
  TSynLog.Add.Log(sllInfo, 'ActionFilterMiddleware being destroyed (equivalent to MVCControllerBeforeDestroy)', self);
  inherited;
end;

function TActionFilterMiddleware.OnMethodExecute(Ctxt: TRestServerUriContext;
  const Method: TInterfaceMethod): Boolean;
var
  methodName: RawUtf8;
  clientIP: RawUtf8;
begin
  // Port of TMVCController.OnBeforeAction
  // NOTE: This is a VALIDATION filter - can block execution
  Result := True; // Allow by default

  methodName := Method.InterfaceDotMethodName;
  clientIP := Ctxt.Call^.LowLevelRemoteIP;

  TSynLog.Add.Log(sllInfo, 'OnMethodExecute: Validating method % from %',
    [methodName, clientIP], self);

  // Validation: Block requests on weekends
  // Port of DMVC: if DayOfWeek(date) in [1, 7] then raise...
  if DayOfWeek(Now) in [1, 7] then
  begin
    TSynLog.Add.Log(sllWarning, 'OnMethodExecute: Request blocked - Weekend access denied', self);

    // Return error response
    Ctxt.Error('You cannot use this service in the WeekEnd', HTTP_FORBIDDEN);

    Result := False; // Block the request
  end;

  // If Result = True, the actual method will be called
  // If Result = False (or exception raised), method will NOT be called
end;

procedure TActionFilterMiddleware.OnInterceptMethod(Sender: TInterfaceMethodExecuteRaw;
  Step: TInterfaceMethodExecuteEventStep);
var
  methodName: RawUtf8;
  duration: Int64;
begin
  // Port of TMVCController.OnAfterAction
  // NOTE: This is a LOGGING filter - called before/after execution

  methodName := Sender.Method.InterfaceDotMethodName;

  case Step of
    smsBefore:
      begin
        fStartTime := GetTickCount64; // Start timing
        TSynLog.Add.Log(sllDebug, 'Interceptor BEFORE: % execution starting',
          [methodName], self);
      end;

    smsAfter:
      begin
        duration := GetTickCount64 - fStartTime; // Calculate duration in milliseconds
        TSynLog.Add.Log(sllInfo, 'Interceptor AFTER: % completed successfully (took % ms)',
          [methodName, duration], self);
      end;

    smsError:
      begin
        duration := GetTickCount64 - fStartTime;
        TSynLog.Add.Log(sllError, 'Interceptor ERROR: % failed after % ms with exception: %',
          [methodName, duration, Sender.LastException.Message], self);
      end;
  end;
end;

end.
