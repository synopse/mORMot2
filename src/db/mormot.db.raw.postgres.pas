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
  BOOLOID = 16;
  BYTEAOID = 17;
  INT8OID = 20;
  INT2OID = 21;
  INT4OID = 23;
  REGPROCOID = 24;
  TEXTOID = 25;
  OIDOID = 26;
  FLOAT4OID = 700;
  FLOAT8OID = 701;
  ABSTIMEOID = 702;
  CASHOID = 790;
  DATEOID = 1082;
  TIMEOID = 1083;
  TIMESTAMPOID = 1114;
  TIMESTAMPTZOID = 1184;
  TIMETZOID = 1266;
  NUMERICOID = 1700;

  CHAROID = 18;
  NAMEOID = 19;
  INT2VECTOROID	= 22;
  TIDOID = 27;
  XIDOID = 28;
  CIDOID = 29;
  OIDVECTOROID	= 30;
  JSONOID = 114;
  XMLOID = 142;
  PGNODETREEOID	= 194;
  PGDDLCOMMANDOID = 32;
  POINTOID= 600;
  LSEGOID = 601;
  PATHOID = 602;
  BOXOID = 603;
  POLYGONOID = 604;
  LINEOID = 628;
  RELTIMEOID = 703;
  TINTERVALOID = 704;
  UNKNOWNOID = 705;
  CIRCLEOID = 718;
  MACADDROID = 829;
  INETOID = 869;
  CIDROID = 650;
  INT2ARRAYOID = 1005;
  INT4ARRAYOID = 1007;
  TEXTARRAYOID= 1009;
  OIDARRAYOID = 1028;
  FLOAT4ARRAYOID = 1021;
  ACLITEMOID = 1033;
  CSTRINGARRAYOID = 1263;
  BPCHAROID = 1042;
  VARCHAROID = 1043;
  INTERVALOID = 1186;
  BITOID = 1560;
  VARBITOID = 1562;
  REFCURSOROID= 1790;
  REGPROCEDUREOID = 2202;
  REGOPEROID = 2203;
  REGOPERATOROID = 2204;
  REGCLASSOID = 2205;
  REGTYPEOID = 2206;
  REGROLEOID = 4096;
  REGNAMESPACEOID = 4089;
  REGTYPEARRAYOID = 2211;
  UUIDOID = 2950;
  LSNOID = 3220;
  TSVECTOROID = 3614;
  GTSVECTOROID = 3642;
  TSQUERYOID = 3615;
  REGCONFIGOID = 3734;
  REGDICTIONARYOID = 3769;
  JSONBOID = 3802;
  INT4RANGEOID	  = 3904;

const
  PGRES_EMPTY_QUERY = 0;
  PGRES_COMMAND_OK = 1;
  PGRES_TUPLES_OK = 2;
  PGRES_COPY_OUT = 3;
  PGRES_COPY_IN = 4;
  PGRES_BAD_RESPONSE = 5;
  PGRES_NONFATAL_ERROR = 6;
  PGRES_FATAL_ERROR = 7;
  PGRES_COPY_BOTH = 8;        // Copy In/Out data transfer in progress
  PGRES_SINGLE_TUPLE = 9;     // single tuple from larger resultset
  PGRES_PIPELINE_SYNC = 10;   // pipeline synchronization point
  PGRES_PIPELINE_ABORTED= 11; // Command didn't run because of an abort

  PQ_PIPELINE_OFF = 0;

  CONNECTION_OK = 0;
  CONNECTION_BAD = 1;
  CONNECTION_STARTED = 2;
  CONNECTION_MADE = 3;
  CONNECTION_AWAITING_RESPONSE = 4;
  CONNECTION_AUTH_OK = 5;
  CONNECTION_SETENV = 6;
  CONNECTION_SSL_STARTUP = 7;
  CONNECTION_NEEDED = 8;

  PGFMT_TEXT = 0;
  PGFMT_BIN = 1;


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
  protected
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
  public
    /// try to dynamically load the libpq library
    // - raise ESqlDBPostgres if the expected library is not found
    constructor Create;
    /// just a wrapper around FastSetString + GetValue/GetLength
    procedure GetRawUtf8(res: PPGresult; tup_num, field_num: integer;
      var result: RawUtf8);
    /// raise an exception on error and clean result
    // - will set pRes to nil if passed
    // - if andClear is true - will call always PQ.Clear(res)
    procedure Check(conn: PPGconn; res: PPGresult;
      pRes: PPPGresult = nil; andClear: boolean = true);
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

{ ************ PostgreSQL Client Library Loading }

const
  PQ_ENTRIES: array[0..32] of RawUtf8 = (
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
    'getResult'
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

procedure TSqlDBPostgresLib.Check(conn: PPGconn; res: PPGresult;
  pRes: PPPGresult; andClear: boolean);
var
  errMsg, errCode: PUtf8Char;
begin
  if (res = nil) or // nil in case of very fatal error, out of emory for example
     (ResultStatus(res) in
       [PGRES_BAD_RESPONSE, PGRES_NONFATAL_ERROR, PGRES_FATAL_ERROR]) then
  begin
    errMsg := ErrorMessage(conn);
    if res <> nil then
      errCode := ResultErrorField(res, Ord('C'){PG_DIAG_SQLSTATE})
    else
      errCode := nil;
    Clear(res);
    if pRes <> nil then
      pRes^ := nil;
    raise ESqlDBPostgres.CreateUtf8(
            '% PGERRCODE: %, %', [self, errCode, errMsg]);
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

