unit server;

interface

{$I mormot.defines.inc}
uses
  SysUtils,
  Contnrs,
  mormot.core.base,
  mormot.core.os,
  mormot.core.data,
  mormot.core.unicode,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.rest.server,
  mormot.soa.core,
  mormot.soa.server,
  mormot.rest.sqlite3,
  mormot.db.raw.sqlite3.static,
  data;

type
  TExampleService = class(TInjectableObjectRest, IExample)
  public
    function Add(var ASample: TSample): Integer;
    function Find(var ASample: TSample): Integer;
    function List(out ASamples: TSampleInfoDynArray): Integer;
    function Delete(AID: TID): Integer;
  end;

  TSampleServer = class(TRestServerDB)
  public
    constructor Create(aModel: TOrmModel; const aDBFileName: TFileName);
        reintroduce;  
  end;

implementation

{
******************************* TExampleService ********************************
}
function TExampleService.Add(var ASample: TSample): Integer;
var
  OrmSample: TOrmSample;
begin
  OrmSample := TOrmSample.Create;
  try
    OrmSample.Name := ASample.Name;
    OrmSample.Question := ASample.Question;
    if Self.Server.Orm.Add(OrmSample, true) > 0 then
    begin
      Writeln('Record created OK');
      Result := 0;
    end
    else
    begin
      Writeln('Error creating Record');
      Result := -1;
    end;
  finally
    OrmSample.Free;
  end;
end;

function TExampleService.Find(var ASample: TSample): Integer;
var
  OrmSample: TOrmSample;
begin
  OrmSample := TOrmSample.Create(Self.Server.Orm,'Name=?',[ASample.Name]);
  try
    if OrmSample.ID=0 then
    begin
      Writeln('Error reading Record');
      Result := -1;
    end
    else
    begin
      Writeln('Record read OK');
      ASample.Name := OrmSample.Name;
      ASample.Question := OrmSample.Question;
      Result := 0;
    end;
  finally
    OrmSample.Free;
  end;
end;

function TExampleService.List(out ASamples: TSampleInfoDynArray): Integer;
var
  OrmSamples: TObjectList;
  i: Integer;
begin
  OrmSamples := Self.Server.Orm.RetrieveList(TOrmSample, '', []);
  if OrmSamples = nil then
  begin
    SetLength(ASamples, 0);
    Result := 0;
    Exit;
  end;
  try
    SetLength(ASamples, OrmSamples.Count);
    for i := 0 to OrmSamples.Count - 1 do
    begin
      ASamples[i].ID := TOrmSample(OrmSamples[i]).ID;
      ASamples[i].Name := TOrmSample(OrmSamples[i]).Name;
    end;
    Result := OrmSamples.Count;
    Writeln('List returned ', Result, ' records');
  finally
    OrmSamples.Free;
  end;
end;

function TExampleService.Delete(AID: TID): Integer;
begin
  if Self.Server.Orm.Delete(TOrmSample, AID) then
  begin
    Writeln('Record deleted OK');
    Result := 0;
  end
  else
  begin
    Writeln('Error deleting Record');
    Result := -1;
  end;
end;

{
******************************** TSampleServer *********************************
}
constructor TSampleServer.Create(aModel: TOrmModel;
  const aDBFileName: TFileName);
begin
  inherited Create(AModel, ADBFileName);
  ServiceDefine(TExampleService, [IExample], sicShared);
end;


end.
