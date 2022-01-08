unit server;

interface

{$I mormot.defines.inc}
uses
  mormot.core.base,
  mormot.core.data,
  mormot.core.os,
  mormot.core.unicode,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.rest.server,
  mormot.rest.sqlite3,
  mormot.db.raw.sqlite3.static,
  data;

type
  TSampleServer = class(TRestServerDB)
  published
    procedure Example(Ctxt: TRestServerUriContext);
  end;

implementation

{
******************************** TSampleServer *********************************
}
procedure TSampleServer.Example(Ctxt: TRestServerUriContext);
var
  OrmSample: TOrmSample;
  Sample: TSample;
  InBody: RawUTF8;
begin
  case Ctxt.Method of
  mGET:
    begin
      OrmSample := TOrmSample.Create(Self.Orm,'Name=?',[Ctxt['Name']]);
      try
      if OrmSample.ID=0 then
      begin
        Writeln('Error reading Record');
        Ctxt.Error('Not found', HTTP_NOTFOUND);
      end
      else
      begin
        Writeln('Record read OK');
        Ctxt.Results([UTF8ToString(OrmSample.Question)]);
      end;
      finally
        OrmSample.Free;
      end;
    end;
  mPOST, mPUT:
    begin
      InBody := Ctxt.Call.InBody;
      OrmSample := TOrmSample.Create;
      try
        RecordLoad(Sample, Ctxt.Call.InBody, TypeInfo(TSample));
        OrmSample.Name := Sample.Name;
        OrmSample.Question := Sample.Question;
        if Self.Orm.Add(OrmSample, true) > 0 then
        begin
          Writeln('Record created OK');
          Ctxt.Success(HTTP_CREATED);
        end
        else
        begin
          Writeln('Error creating Record');
          Ctxt.Error('Bad Request', HTTP_BADREQUEST);
        end;
      finally
        OrmSample.Free;
      end;
    end;
  mDELETE:
    Ctxt.Error('Forbidden', HTTP_FORBIDDEN);
  end;
end;


end.
