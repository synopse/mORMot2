unit jsonrpc.entities;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.orm.core;

type
  /// ORM entity for calculator operation history
  TOrmCalculation = class(TOrm)
  private
    fOperation: RawUtf8;
    fResult: double;
    fTimestamp: TUnixMSTime;
  published
    property Operation: RawUtf8 read fOperation write fOperation;
    property Result: double read fResult write fResult;
    property Timestamp: TUnixMSTime read fTimestamp write fTimestamp;
  end;

  /// Array of calculation ORM entities
  TOrmCalculationObjArray = array of TOrmCalculation;

  /// ORM entity for user information
  TOrmUser = class(TOrm)
  private
    fUsername: RawUtf8;
    fEmail: RawUtf8;
    fCreated: TUnixMSTime;
  published
    property Username: RawUtf8 read fUsername write fUsername;
    property Email: RawUtf8 read fEmail write fEmail;
    property Created: TUnixMSTime read fCreated write fCreated;
  end;

  /// Array of user ORM entities
  TOrmUserObjArray = array of TOrmUser;

implementation

end.
