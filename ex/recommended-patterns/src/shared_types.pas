
unit shared_types;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.unicode,
  mormot.core.rtti;

var
  /// JSON field-name casing convention applied to every DTO wire contract
  /// (Recommended Patterns A.7, Gotcha 3). Single switch for the whole app:
  /// each DTO unit calls NameChangeCase(DtoJsonCase) on its types — on the DTOs
  /// only, never the TOrm. scLowerCaseFirst is the camelCase default for new
  /// web APIs; switch to scSnakeCase here to flip the whole contract at once.
  DtoJsonCase: TSetCase = scLowerCaseFirst;

type
  /// Shared base result status for all CQRS Command operations
  /// - srSuccess: the command completed
  /// - srInvalidRequest: validation/empty/range/duplicate rejection
  /// - srNotFound: the target aggregate does not exist
  /// - srDenied: reserved for future authorization checks
  /// - srDbError: transaction/persistence failure or unexpected exception
  TServiceResult = (
    srSuccess,
    srInvalidRequest,
    srNotFound,
    srDenied,
    srDbError);

  /// Standard result type for all CQRS Command operations
  /// - Status is the explicit discriminator; Success is a convenience that is
  ///   true exactly when Status = srSuccess (kept for existing callers/tests)
  TCommandResult = packed record
    Success: boolean;
    ID: TID;
    ErrorMessage: RawUtf8;
    Status: TServiceResult;
  end;

function CommandSuccess(aID: TID): TCommandResult; overload;
function CommandSuccess: TCommandResult; overload;
function CommandError(const aMessage: RawUtf8;
  aStatus: TServiceResult = srInvalidRequest): TCommandResult;

implementation

function CommandSuccess(aID: TID): TCommandResult;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Success := true;
  Result.ID := aID;
end;

function CommandSuccess: TCommandResult;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Success := true;
end;

function CommandError(const aMessage: RawUtf8;
  aStatus: TServiceResult): TCommandResult;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.ErrorMessage := aMessage;
  Result.Status := aStatus;
end;

initialization
  // register the enum by name so RegisterFromText can resolve the Status field
  Rtti.RegisterType(TypeInfo(TServiceResult));
  Rtti.RegisterFromText(TypeInfo(TCommandResult),
    'Success boolean ID Int64 ErrorMessage RawUtf8 Status TServiceResult');
  // camelCase the wire contract (after RegisterFromText built the fields)
  Rtti[TypeInfo(TCommandResult)].Props.NameChangeCase(DtoJsonCase);

end.
