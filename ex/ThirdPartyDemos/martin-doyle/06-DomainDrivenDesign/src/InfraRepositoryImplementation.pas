unit InfraRepositoryImplementation;

interface

{$I mormot.defines.inc}
uses
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.orm.base,
  mormot.orm.core,
  mormot.db.raw.sqlite3,

  mormot.rest.sqlite3,
  mormot.db.raw.sqlite3.static,
  DomTypes,
  DomRepositoryInterfaces;

type
  TOrmSample = class(TOrm)
  private
    FName: RawUTF8;
    FQuestion: RawUTF8;
    FTime: TModTime;
  published
    property Name: RawUTF8 read FName write FName;
    property Question: RawUTF8 read FQuestion write FQuestion;
    property Time: TModTime read FTime write FTime;
  end;

  TSampleRepository = class(TInterfacedObject, ISampleRepository)
  private
    FOrmServer: TRestServerDB;
    FRestOrm: IRestOrm;
  protected
    function GetSample(const AName: TName): TOrmSample;
  public
    constructor Create; overload;
    destructor Destroy; override;
    function RetrieveSample(var ASample: TSample): TSampleRepositoryError;
    function SaveNewSample(var ASample: TSample): TSampleRepositoryError;
  end;


function CreateSampleModel: TOrmModel;

implementation

function CreateSampleModel: TOrmModel;
begin
  result := TOrmModel.Create([TOrmSample]);
end;

{
****************************** TSampleRepository *******************************
}
constructor TSampleRepository.Create;
begin
  inherited Create;
  FOrmServer := TRestServerDB.Create(CreateSampleModel, ChangeFileExt(Executable.ProgramFileName, '.db'));
  FOrmServer.DB.Synchronous := smOff;
  FOrmServer.DB.LockingMode := lmExclusive;
  FOrmServer.Server.CreateMissingTables;
  FRestOrm := FOrmServer.Orm;
end;

destructor TSampleRepository.Destroy;
begin
  FRestOrm := nil;
  FOrmServer.Free;
  inherited Destroy;
end;

function TSampleRepository.GetSample(const AName: TName): TOrmSample;
begin
  Result := TOrmSample.Create(FRestOrm,'Name=?',[AName]);
end;

function TSampleRepository.RetrieveSample(var ASample: TSample):
    TSampleRepositoryError;
var
  OrmSample: TOrmSample;
begin
  Result := srNotFound;
  OrmSample := GetSample(ASample.Name);
  try
    if OrmSample.IDValue = 0 then
      exit;
    ASample.Question := OrmSample.Question;
    Result := srSuccess;
  finally
    OrmSample.Free;
  end;
end;

function TSampleRepository.SaveNewSample(var ASample: TSample):
    TSampleRepositoryError;
var
  OrmSample: TOrmSample;
begin
  Result := srWriteFailure;
  OrmSample := GetSample(ASample.Name);
  try
    if OrmSample.IDValue <> 0 then
    begin
      Result := srDuplicatedInfo;
      exit;
    end;
    OrmSample.Name := ASample.Name;
    OrmSample.Question := ASample.Question;
    if FRestOrm.Add(OrmSample, true) > 0 then
      Result := srSuccess
  finally
    OrmSample.Free;
  end;
end;


end.
