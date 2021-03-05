unit server;

interface

{$I mormot.defines.inc}
uses
  SysUtils,
  mormot.core.base,
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
  end;

  TSampleServer = class(TRestServerDB)
  public
    constructor Create(aModel: TOrmModel; const aDBFileName: TFileName);
        overload;
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

{
******************************** TSampleServer *********************************
}
constructor TSampleServer.Create(aModel: TOrmModel; const aDBFileName:
    TFileName);
begin
  inherited Create(AModel, ADBFileName);
  ServiceDefine(TExampleService, [IExample], sicShared);
end;


end.
