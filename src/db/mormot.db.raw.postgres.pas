/// Database Framework Low-Level PostgreSQL Client
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.db.raw.postgres;

{
  *****************************************************************************

   Efficient Direct PostgreSQL (libpq) API Access
    - Native PostgreSQL Client Library Constants
    - PostgreSQL Client Library Loading

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.core.log,
  mormot.db.core,
  mormot.db.sql;


{ ************ Native PostgreSQL Client Library Constants }

const
  // see pg_type.h
  BOOLOID          = 16;
  BYTEAOID         = 17;
  CHAROID          = 18;
  NAMEOID          = 19;
  INT8OID          = 20;
  INT2OID          = 21;
  INT2VECTOROID	   = 22;
  INT4OID          = 23;
  REGPROCOID       = 24;
  TEXTOID          = 25;
  OIDOID           = 26;
  TIDOID           = 27;
  XIDOID           = 28;
  CIDOID           = 29;
  OIDVECTOROID	   = 30;
  PGDDLCOMMANDOID  = 32;
  JSONOID          = 114;
  XMLOID           = 142;
  PGNODETREEOID	   = 194;
  POINTOID         = 600;
  LSEGOID          = 601;
  PATHOID          = 602;
  BOXOID           = 603;
  POLYGONOID       = 604;
  LINEOID          = 628;
  CIDROID          = 650;
  FLOAT4OID        = 700;
  FLOAT8OID        = 701;
  ABSTIMEOID       = 702;
  RELTIMEOID       = 703;
  TINTERVALOID     = 704;
  UNKNOWNOID       = 705;
  CIRCLEOID        = 718;
  CASHOID          = 790;
  MACADDROID       = 829;
  INETOID          = 869;
  INT2ARRAYOID     = 1005;
  INT4ARRAYOID     = 1007;
  TEXTARRAYOID     = 1009;
  INT8ARRAYOID     = 1016;
  FLOAT4ARRAYOID   = 1021;
  FLOAT8ARRAYOID   = 1022;
  OIDARRAYOID      = 1028;
  ACLITEMOID       = 1033;
  BPCHAROID        = 1042;
  VARCHAROID       = 1043;
  DATEOID          = 1082;
  TIMEOID          = 1083;
  TIMESTAMPOID     = 1114;
  TIMESTAMPTZOID   = 1184;
  INTERVALOID      = 1186;
  CSTRINGARRAYOID  = 1263;
  TIMETZOID        = 1266;
  BITOID           = 1560;
  VARBITOID        = 1562;
  NUMERICOID       = 1700;
  REFCURSOROID     = 1790;
  REGPROCEDUREOID  = 2202;
  REGOPEROID       = 2203;
  REGOPERATOROID   = 2204;
  REGCLASSOID      = 2205;
  REGTYPEOID       = 2206;
  REGTYPEARRAYOID  = 2211;
  UUIDOID          = 2950;
  LSNOID           = 3220;
  TSVECTOROID      = 3614;
  GTSVECTOROID     = 3642;
  TSQUERYOID       = 3615;
  REGCONFIGOID     = 3734;
  REGDICTIONARYOID = 3769;
  JSONBOID         = 3802;
  INT4RANGEOID     = 3904;
  REGNAMESPACEOID  = 4089;
  REGROLEOID       = 4096;

const
  PGRES_EMPTY_QUERY      = 0;
  PGRES_COMMAND_OK       = 1;
  PGRES_TUPLES_OK        = 2;
  PGRES_COPY_OUT         = 3;
  PGRES_COPY_IN          = 4;
  PGRES_BAD_RESPONSE     = 5;
  PGRES_NONFATAL_ERROR   = 6;
  PGRES_FATAL_ERROR      = 7;
  PGRES_COPY_BOTH        = 8;  // Copy In/Out data transfer in progress
  PGRES_SINGLE_TUPLE     = 9;  // single tuple from larger resultset
  PGRES_PIPELINE_SYNC    = 10; // pipeline synchronization point
  PGRES_PIPELINE_ABORTED = 11; // Command didn't run because of an abort

  PQ_PIPELINE_OFF = 0;

  CONNECTION_OK                = 0;
  CONNECTION_BAD               = 1;
  CONNECTION_STARTED           = 2;
  CONNECTION_MADE              = 3;
  CONNECTION_AWAITING_RESPONSE = 4;
  CONNECTION_AUTH_OK           = 5;
  CONNECTION_SETENV            = 6;
  CONNECTION_SSL_STARTUP       = 7;
  CONNECTION_NEEDED            = 8;

  PGFMT_TEXT = 0;
  PGFMT_BIN  = 1;

/// compute the PostgreSQL raw binary to encode an array of (integer) parameters
function ToArrayOid(Values: PByte; ArrayOid, ValueCount, ValueSize: integer;
  var Bin: RawByteString): boolean;



{ ************ PostgreSQL Client Library Loading }

type
  /// exception type associated to the native libpg Interface
  ESqlDBPostgres = class(ESqlDBException);

  PPGconn = type pointer;
  PPGresult = type pointer;
  PPPGresult = ^PPGresult;

  PQnoticeProcessor = procedure(arg: pointer; message: PUtf8Char); cdecl;

  /// direct access to the libpq native Postgres protocol 3 library
  // - only the endpoints needed by this unit are imported
  TSqlDBPostgresLib = class(TSynLibrary)
  public
    LibVersion: function: integer; cdecl;
    IsThreadSafe: function: integer; cdecl;
    SetDBLogin: function(pghost, pgport, pgoptions, pgtty, dbName,
      login, pwd: PUtf8Char): PPGconn; cdecl;
    Status: function(conn: PPGconn): integer; cdecl;
    Finish: procedure(conn: PPGconn); cdecl;
    ResultStatus: function(res: PPGresult): integer; cdecl;
    ResultErrorField: function(res: PPGresult; fieldcode: integer): PUtf8Char; cdecl;
    ErrorMessage: function(conn: PPGconn): PUtf8Char; cdecl;
    SetNoticeProcessor: function(conn: PPGconn; proc: PQnoticeProcessor;
      arg: pointer): PQnoticeProcessor; cdecl;
    Clear: procedure(res: PPGresult); cdecl;
    Freemem: procedure(ptr: pointer); cdecl;
    Exec: function(conn: PPGconn; query: PUtf8Char): PPGresult; cdecl;
    Prepare: function(conn: PPGconn; stmtName, query: PUtf8Char; nParams: integer;
      paramTypes: PCardinal): PPGresult; cdecl;
    DescribePrepared: function(conn: PPGconn; stmtName: PUtf8Char): PPGresult; cdecl;
    NParams: function(res: PPGresult): integer; cdecl;
    ParamType: function(res: PPGresult; param_number: integer): word; cdecl;
    ExecPrepared: function(conn: PPGconn; stmtName: PUtf8Char; nParams: integer;
      paramValues: PPchar; paramLengths, paramFormats: PInteger;
      resultFormat: integer): PPGresult; cdecl;
    ExecParams: function(conn: PPGconn; command: PUtf8Char; nParams: integer;
      paramTypes: PCardinal; paramValues: PPchar; paramLengths, paramFormats: PInteger;
      resultFormat: integer): PPGresult; cdecl;
    nfields: function(res: PPGresult): integer; cdecl;
    ntuples: function(res: PPGresult): integer; cdecl;
    cmdTuples: function(res: PPGresult): PUtf8Char; cdecl;
    fname: function(res: PPGresult; field_num: integer): PUtf8Char; cdecl;
    ftype: function(res: PPGresult; field_num: integer): cardinal; cdecl;
    GetValue: function(res: PPGresult; tup_num, field_num: integer): PUtf8Char; cdecl;
    GetLength: function(res: PPGresult; tup_num, field_num: integer): integer; cdecl;
    GetIsNull: function(res: PPGresult; tup_num, field_num: integer): integer; cdecl;
    enterPipelineMode: function(conn: PPGconn): integer; cdecl;
    exitPipelineMode: function(conn: PPGconn): integer; cdecl;
    pipelineSync: function(conn: PPGconn): integer; cdecl;
    sendFlushRequest: function(conn: PPGconn): integer; cdecl;
    pipelineStatus: function(const conn: PPGconn): integer; cdecl;
    flush: function(conn: PPGconn): integer; cdecl;
    sendQueryParams: function(conn: PPGconn; command: PUtf8Char; nParams: integer;
      paramTypes: PCardinal; paramValues: PPchar; paramLengths, paramFormats: PInteger;
      resultFormat: integer): integer; cdecl;
    sendPrepare: function(conn: PPGconn; stmtName, query: PUtf8Char; nParams: integer;
      paramTypes: PCardinal): PPGresult; cdecl;
    sendQueryPrepared: function(conn: PPGconn; stmtName: PUtf8Char; nParams: integer;
      paramValues: PPchar; paramLengths, paramFormats: PInteger;
      resultFormat: integer): integer; cdecl;
    getResult: function(conn: PPGconn): PPGresult; cdecl;
    socket: function(conn: PPGconn): integer; cdecl;
  public
    /// try to dynamically load the libpq library
    // - raise an ESqlDBPostgres if the expected library is not found
    constructor Create;
    /// just a wrapper around FastSetString + GetValue/GetLength
    procedure GetRawUtf8(res: PPGresult; tup_num, field_num: integer;
      var result: RawUtf8);
    /// raise an ESqlDBPostgres on error and clean result
    // - will set pRes to nil if passed
    // - if andClear is true - will call always PQ.Clear(res)
    procedure Check(conn: PPGconn; const ctxt: ShortString; res: PPGresult;
      pRes: PPPGresult = nil; andClear: boolean = true); overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// raise an ESqlDBPostgres and clean result
    procedure RaiseError(conn: PPGconn; const ctxt: ShortString;
      res: PPGresult = nil);
  end;

var
  /// raw access to the low-level libpq API functions, once loaded
  // - is set by calling PostgresLibraryInitialize
  PQ: TSqlDBPostgresLib = nil;

  /// allow to specify a libpq library file name to use
  SynDBPostgresLibrary: TFileName;

/// try to load the libpq library
// - raise a ESqlDBPostgres exception if loading failed
// - you can setup a non-standard libpq location in SynDBPostgresLibrary
// global variable, before calling this procedure
procedure PostgresLibraryInitialize;


implementation


{ ************ Native PostgreSQL Client Library Constants }

// see https://stackoverflow.com/a/66499392/458259

function ToArrayOid(Values: PByte; ArrayOid, ValueCount, ValueSize: integer;
  var Bin: RawByteString): boolean;
var
  len: PtrInt;
  v: Int64;
  p: PCardinal;
begin
  result := false;
  if (ValueCount <= 0) or
     not (ValueSize in [4, 8]) then
    exit;
  case ArrayOid of
    INT4ARRAYOID:
      begin
        ArrayOid := INT4OID;
        len := (5 * 4) + ValueCount * (4 + 4);
      end;
    INT8ARRAYOID:
      begin
        ArrayOid := INT8OID;
        len := (5 * 4) + ValueCount * (4 + 8);
      end;
  else
    exit; // unsupported
  end;
  FastNewRawByteString(Bin, len);
  p := pointer(Bin);
  p^ := $01000000;           // dimensions
  inc(p);
  p^ := $00000000;           // has null
  inc(p);
  p^ := bswap32(ArrayOid);   // items type
  inc(p);
  p^ := bswap32(ValueCount); // items count
  inc(p);
  p^ := $01000000;           // offset
  inc(p);
  if ArrayOid = INT4OID then
    repeat
      p^ := $04000000;        // item length
      inc(p);
      p^ := bswap32(PCardinal(Values)^); // item value (maybe truncated)
      inc(p);
      inc(Values, ValueSize);
      dec(ValueCount)
    until ValueCount = 0
  else if ValueSize = 4 then
    repeat
      p^ := $08000000;          // item length
      inc(p);
      v := PInteger(Values)^;   // expand sign to 64-bit
      PInt64(p)^ := bswap64(v); // item value
      inc(PInt64(p));
      inc(PInteger(Values));
      dec(ValueCount)
    until ValueCount = 0
  else // ValueSize = 8
    repeat
      p^ := $08000000;
      inc(p);
      PInt64(p)^ := bswap64(PInt64(Values)^);
      inc(PInt64(p));
      inc(PInt64(Values));
      dec(ValueCount)
    until ValueCount = 0;
  //if PAnsiChar(p) - pointer(Bin) <> length(Bin) then
  //  raise ESqlDBPostgres.Create('ToIntArrayOid');
  result := true;
end;


{ ************ PostgreSQL Client Library Loading }

const
  PQ_ENTRIES: array[0..36] of RawUtf8 = (
    'libVersion',
    'isthreadsafe',
    'setdbLogin',
    'status',
    'finish',
    'resultStatus',
    'resultErrorField',
    'errorMessage',
    'setNoticeProcessor',
    'clear',
    'freemem',
    'exec',
    'prepare',
    'describePrepared',
    'nparams',
    'paramtype',
    'execPrepared',
    'execParams',
    'nfields',
    'ntuples',
    'cmdTuples',
    'fname',
    'ftype',
    'getvalue',
    'getlength',
    'getisnull',
    // optional API entries for pipelining mode
    'enterPipelineMode',
    'exitPipelineMode',
    'pipelineSync',
    'sendFlushRequest',
    'pipelineStatus',
    'flush',
    'sendQueryParams',
    'sendPrepare',
    'sendQueryPrepared',
    'getResult',
    'socket'
    );


{ TSqlDBPostgresLib }

const
  {$ifdef OSWINDOWS}
    LIBNAME = 'libpq.dll';
    LIBNAME2 = '';
  {$else}
    {$ifdef OSDARWIN}
      LIBNAME = 'libpq.dylib';
      LIBNAME2 = '';
    {$else}
      LIBNAME = 'libpq.so.5';
      LIBNAME2 = 'libpq.so.4';
    {$endif OSDARWIN}
  {$endif OSWINDOWS}

constructor TSqlDBPostgresLib.Create;
var
  P: PPointerArray;
  raiseonfailure: ExceptionClass;
  i: PtrInt;
begin
  TryFromExecutableFolder := true;
  TryLoadLibrary([
    SynDBPostgresLibrary, LIBNAME, LIBNAME2], ESqlDBPostgres);
  P := @@LibVersion;
  raiseonfailure := ESqlDBPostgres;
  for i := 0 to High(PQ_ENTRIES) do
  begin
    if PQ_ENTRIES[i] = 'enterPipelineMode' then
      raiseonfailure := nil; // old libpq with no pipelining API
    Resolve('PQ', PQ_ENTRIES[i], @P[I], raiseonfailure);
  end;
end;

procedure TSqlDBPostgresLib.GetRawUtf8(res: PPGresult;
  tup_num, field_num: integer; var result: RawUtf8);
begin
  FastSetString(result, GetValue(res, tup_num, field_num),
    GetLength(res, tup_num, field_num));
end;

procedure TSqlDBPostgresLib.RaiseError(conn: PPGconn; const ctxt: ShortString;
  res: PPGresult);
var
  errMsg, errCode: PUtf8Char;
begin
  errMsg := ErrorMessage(conn);
  if res <> nil then
  begin
    errCode := ResultErrorField(res, ord('C'){PG_DIAG_SQLSTATE});
    Clear(res);
  end
  else
    errCode := nil;
  ESqlDBPostgres.RaiseUtf8('% % failed: % [%]', [self, ctxt, errCode, errMsg]);
end;

procedure TSqlDBPostgresLib.Check(conn: PPGconn; const ctxt: ShortString;
  res: PPGresult; pRes: PPPGresult; andClear: boolean);
begin
  if (res = nil) or // nil in case of very fatal error, e.g. out of memory
     (ResultStatus(res) in
       [PGRES_BAD_RESPONSE, PGRES_NONFATAL_ERROR, PGRES_FATAL_ERROR]) then
  begin
    if pRes <> nil then
      pRes^ := nil;
    RaiseError(conn, ctxt, res);
  end
  else if andClear then
    Clear(res);
end;

procedure PostgresLibraryInitialize;
begin
  if PQ <> nil then
    exit;
  GlobalLock;
  try
    if PQ = nil then
      PQ := TSqlDBPostgresLib.Create;
  finally
    GlobalUnLock;
  end;
end;


initialization

finalization
  FreeAndNil(PQ);

end.

