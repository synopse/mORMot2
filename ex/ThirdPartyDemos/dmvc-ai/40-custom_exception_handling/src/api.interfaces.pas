unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.rtti,
  mormot.core.interfaces;

type
  /// Custom exception severity levels
  // Port of TMyExceptionSeverity from DMVC
  TMyExceptionSeverity = (
    Fatal,
    Error,
    Warning,
    Information
  );

  /// Custom exception class with rich error information
  // Port of EMyException from DMVC MyControllerU.pas
  EMyException = class(Exception)
  private
    fSeverity: TMyExceptionSeverity;
    fCode: Integer;
    fDetails: RawUtf8;
    fDiagnostics: RawUtf8;
    fExpression: RawUtf8;
  public
    constructor Create(const aMsg: RawUtf8; aSeverity: TMyExceptionSeverity;
      aCode: Integer; const aDetails, aDiagnostics, aExpression: RawUtf8);

    property Severity: TMyExceptionSeverity read fSeverity write fSeverity;
    property Code: Integer read fCode write fCode;
    property Details: RawUtf8 read fDetails write fDetails;
    property Diagnostics: RawUtf8 read fDiagnostics write fDiagnostics;
    property Expression: RawUtf8 read fExpression write fExpression;
  end;

  /// Exception handling API service interface
  // Port of TMyController from DMVC
  IExceptionHandlingApi = interface(IInvokable)
    ['{B8F3A6E1-9C4D-4F2A-8B5E-1D3C7E9F4A2B}']

    /// Returns a welcome message
    // Port of Index method
    function Index: RawUtf8;

    /// Raises a custom exception with rich error information
    // Port of Error method (raises EMyException)
    function RaiseCustomError: RawUtf8;

    /// Raises a standard exception
    // Port of Error method (raises Exception)
    function RaiseStandardError: RawUtf8;

    /// Returns HTTP 204 No Content
    // Port of GetCustomer method
    function GetCustomer(aID: Integer): RawUtf8;
  end;

implementation

{ EMyException }

constructor EMyException.Create(const aMsg: RawUtf8; aSeverity: TMyExceptionSeverity;
  aCode: Integer; const aDetails, aDiagnostics, aExpression: RawUtf8);
begin
  inherited CreateFmt('%s (Code: %d, Severity: %s)',
    [aMsg, aCode, GetEnumName(TypeInfo(TMyExceptionSeverity), Ord(aSeverity))^]);
  fSeverity := aSeverity;
  fCode := aCode;
  fDetails := aDetails;
  fDiagnostics := aDiagnostics;
  fExpression := aExpression;
end;

end.
