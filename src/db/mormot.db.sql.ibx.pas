/// Database Framework IBX/FB Pascal API Connection (beta)
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.db.sql.ibx;

{
  *****************************************************************************

   Direct FirebirdSQL Client Access using the FB / IBX2 Pascal API layer
    -  TSqlDBIbxConnection* and TSqlDBIbxStatement Classes

  *****************************************************************************

  Written by https://github.com/TTomas - BETA stage until further validated
  more details on https://synopse.info/forum/viewtopic.php?pid=14086#p14086

  For low level connection, uses the MWA Software Firebird Pascal API package,
  (fbintf) part of IBX for Lazarus - https://www.mwasoftware.co.uk/fb-pascal-api
  This version work with trunk version of IBX
  svn co https://svn.mwasoftware.co.uk/public/ibx/trunk ibx
  more details on https://forum.lazarus.freepascal.org/index.php/topic,56267.0.html
  Only fbintf package need to be installed

  - With explicit StartTransaction (or Batch): all statements in connection
    are executed within this main transaction owned by the connection.
  - If no explicit StartTransaction is called, all statements create their
    own proper transaction on prepare, and COMMIT them after execution or
    Eof or ReleaseRows. This implements a software-simulated "auto commit".
    Each internal transaction is owned by the associated Statement.
  - if TSqlDBIbxConnectionProperties.CreateDescendingOnlyPK is forced to True,
    it will create only one descending PK index using this statement:
      PRIMARY KEY(ID) using desc index PK_TableName
    default dFirebird create two indexes on ID, one ascending, second descending
    nedded for select max(ID) - see http://www.firebirdfaq.org/faq205
  - Batch is implemented for insert, update, delete using "execute block"
  - TODO: Firebird4 API interface, with its new IBatch interface for
    insert/update also implemented in fbintf package.
}


interface


{$ifdef NOSYNDBIBX}
// NOSYNDBIBX from mormot2.lpk Lazarus package > Custom Options > Defines

implementation // compile a void unit if NOSYNDBIBX conditional is set

{$else}

{.$define ZEOSTRANS}
// simulate transaction management like ZeosLib (testing only)
// - Zeos don't commit read (select) statements and transaction remains open
// for a long period of time
// - only CommitRetaining transaction with write (insert, update, delete) statements
// - for testing purposes, to compare performance with ZeosLib

{$I mormot.defines.inc}

uses
  types,
  sysutils,
  classes,
  variants,
  // main IBX/FB Pascal API units
  IB,
  // mORMot 2 units
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.json,
  mormot.core.datetime,
  mormot.core.data,
  mormot.core.perf,
  mormot.core.rtti,
  mormot.core.log,
  mormot.core.buffers,
  mormot.db.core,
  mormot.db.sql;

type
  /// Exception type associated to the IBX/FB Pascal API database components
  ESqlDBIbx = class(ESqlDBException);

  /// implement properties shared by IBX/FB Pascal API connections
  TSqlDBIbxConnectionProperties = class(TSqlDBConnectionPropertiesThreadSafe)
  protected
    fCreateDescendingOnlyPK: boolean;
    fFirebirdLibraryPathName: string;
    fIbxDBParams: TStringList;
    fCreateIfNotExists: boolean;
    procedure SetCreateDescendingOnlyPK(AValue: boolean);
    /// initialize fForeignKeys content with all foreign keys of this DB
    // - do nothing by now
    procedure GetForeignKeys; override;
    // Override to enable descending PK
    function SqlFieldCreate(const aField: TSqlDBColumnCreate;
      var aAddPrimaryKey: RawUtf8): RawUtf8; override;
  public
    /// initialize the properties to connect to the IBX/FB Pascal API engine
    // - aServerName shall contain the Firebird server and port URI, e.g:
    // HOST[:PORT], empty for embbeded firebird will set ThreadingMode to tmMainConnection
    // - aDatabaseName, aUserID, aPassword
    // - note that when run from mORMot's ORM, this class will by default
    // create one connection per thread
    constructor Create(const aServerName, aDatabaseName,
      aUserID, aPassWord: RawUtf8); override;
    /// finalize this connection properties
    destructor Destroy; override;
    /// create a new connection
    // - caller is responsible of freeing this instance
    // - this overridden method will create an TSqlDBIbxConnection instance
    function NewConnection: TSqlDBConnection; override;
    /// method overriden to support our CreateDescendingOnlyPK property
    function SqlCreate(const aTableName: RawUtf8;
      const aFields: TSqlDBColumnCreateDynArray; aAddID: boolean): RawUtf8; override;
    /// method overriden to support our CreateDescendingOnlyPK property
    function IsPrimaryKeyIndexed(var AscendingOnly: boolean): boolean; override;
  published
    /// full file path name to the firebird client dll (fbclient.dll), default ''
    property FirebirdLibraryPathName: RawUtf8
      read fFirebirdLibraryPathName write fFirebirdLibraryPathName;
    /// optional low-levl IBX DB Params, see documentation in IBX/FB Pascal API
    property IbxDBParams: TStringList
      read fIbxDBParams;
    /// create the database file if not exists, default is True
    property CreateIfNotExists: boolean
      read fCreateIfNotExists;
    /// force to create only a DESC index on primary key, for best performance
    // - the default mORMot behavior is to create both ascending and descending
    // indexes on the ID primary key: it is needed because only an ascending
    // index is created on FireBird PK, and max(ID) is slow - see
    // http://www.firebirdfaq.org/faq205
    // - but having two indexes on same column slow down database writes
    // - forcing this property to True will create only one descending PK index
    property CreateDescendingOnlyPK: boolean
      read fCreateDescendingOnlyPK write SetCreateDescendingOnlyPK;
  end;

  /// implements a connection via the IBX/FB Pascal API access layer
  TSqlDBIbxConnection = class(TSqlDBConnectionThreadSafe)
  protected
    fFbLibraryPathName: string;
    fDBParams: TStringList;
    fCreateDBIfNotExists: boolean;
    fDBName: string;
    fFirebirdAPI: IFirebirdAPI;
    fAttachment: IAttachment;
    // Main Transaction used with Begin(Start)Transaction/Batch
    fTPB: ITPB;
    fTransaction: ITransaction;
    function GetFirebirdAPI: IFirebirdAPI;
    function GenerateTPB(aReadOnly: boolean = false): ITPB;
  public
    /// prepare a connection to a specified Firebird database server
    constructor Create(aProperties: TSqlDBConnectionProperties); override;
    /// finalize the connection
    destructor Destroy; override;
    /// connect to the specified Firebird server
    // - should raise an ESqlDBIbx on error
    procedure Connect; override;
    /// stop connection to the specified Firebird database server
    // - should raise an ESqlDBIbx on error
    procedure Disconnect; override;
    /// return TRUE if Connect has been already successfully called
    function IsConnected: boolean; override;
    /// create a new statement instance
    function NewStatement: TSqlDBStatement; override;
    /// begin a Transaction for this connection
    procedure StartTransaction; override;
    /// commit changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Commit; override;
    /// discard changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Rollback; override;
    /// access to the associated IBX/FB Pascal API connection instance
    property Attachment:  IAttachment
      read fAttachment;
    /// access to the associated IBX/FB Pascal raw API
    property FirebirdAPI: IFirebirdAPI
      read GetFirebirdAPI;
    // main Transaction used with Begin(Start)Transaction/Batch
    property Transaction: ITransaction
      read fTransaction;
  end;

  TIBXColumnsMeta = record
    SQLType: Cardinal;
    CodePage: TSystemCodePage;
    Scale: integer;
    Subtype: integer;
  end;

  /// implements a statement via a IBX/FB Pascal API database connection
  TSqlDBIbxStatement = class(TSqlDBStatementWithParamsAndColumns)
  protected
    fAutoStartCommitTrans: boolean;
    fStatement: IStatement;
    fResultSet: IResultSet;
    fResults: IResults;
    fMeta: IMetaData;
    fColumnsMeta: array of TIBXColumnsMeta;
    // Internal Transaction used for all statements if not explicit StartTransaction/Batch
    // This transaction is Started and COMMIT after execution (auto commit)
    fInternalTPB: ITPB;
    fInternalTransaction: ITransaction;
    fReadOnlyTransaction: boolean;
    procedure InternalStartTransaction;
    procedure InternalCommitTransaction;
    procedure ErrorColAndRowset(const Col: integer);
    procedure CheckColAndRowset(const Col: integer);
      {$ifdef HASINLINE} inline; {$endif}
    function IbxSQLTypeToTSqlDBFieldType(const aColMeta: TIBXColumnsMeta): TSqlDBFieldType;
  public
    destructor Destroy; override;
    /// Prepare an UTF-8 encoded SQL statement
    // - parameters marked as ? will be bound later, before ExecutePrepared call
    // - if ExpectResults is TRUE, then Step() and Column*() methods are available
    // to retrieve the data rows
    // - raise an ESqlDBIbx on any error
    procedure Prepare(const aSQL: RawUtf8;
      ExpectResults: boolean = false); overload; override;
    /// Execute a prepared SQL statement
    // - parameters marked as ? should have been already bound with Bind*() functions
    // - this implementation will also handle bound array of values (if any)
    // - this overridden method will log the SQL statement if sllSQL has been
    // enabled in SynDBLog.Family.Level
    // - raise an ESqlDBIbx on any error
    procedure ExecutePrepared; override;
    /// gets a number of updates made by latest executed statement
    function UpdateCount: integer; override;
    /// Reset the previous prepared statement
    // - this overridden implementation will reset all bindings and the cursor state
    // - raise an ESqlDBIbx on any error
    procedure Reset; override;

    /// Access the next or first row of data from the SQL Statement result
    // - return true on success, with data ready to be retrieved by Column*() methods
    // - return false if no more row is available (e.g. if the SQL statement
    // is not a SELECT but an UPDATE or INSERT command)
    // - if SeekFirst is TRUE, will put the cursor on the first row of results
    // - raise an ESqlDBIbx on any error
    function Step(SeekFirst: boolean = false): boolean; override;
    /// free IResultSet/IResultSetMetaData when ISqlDBStatement is back in cache
    procedure ReleaseRows; override;
    /// return a Column integer value of the current Row, first Col is 0
    function ColumnInt(Col: integer): Int64; override;
    /// returns TRUE if the column contains NULL
    function ColumnNull(Col: integer): boolean; override;
    /// return a Column floating point value of the current Row, first Col is 0
    function ColumnDouble(Col: integer): double; override;
    /// return a Column date and time value of the current Row, first Col is 0
    function ColumnDateTime(Col: integer): TDateTime; override;
    /// return a Column currency value of the current Row, first Col is 0
    function ColumnCurrency(Col: integer): currency; override;
    /// return a Column UTF-8 encoded text value of the current Row, first Col is 0
    function ColumnUtf8(Col: integer): RawUtf8; override;
    /// return a Column as a blob value of the current Row, first Col is 0
    function ColumnBlob(Col: integer): RawByteString; override;
    /// append all columns values of the current Row to a JSON stream
    // - will use WR.Expand to guess the expected output format
    // - this overriden implementation will call fReultSet methods to avoid
    // creating most temporary variable
    procedure ColumnsToJson(WR: TResultsWriter); override;
  end;



implementation

uses
  IBErrorCodes;


{ TSqlDBIbxStatement }

procedure TSqlDBIbxStatement.InternalStartTransaction;
begin
  {$ifndef ZEOSTRANS}
  if (fInternalTransaction <> nil) and
     fInternalTransaction.InTransaction then
    raise ESqlDBIbx.CreateUtf8('Invalid Internal %.StartTransaction: ' +
      'Transaction is Started/InTransactions', [self]);
  {$endif ZEOSTRANS}
  if fInternalTransaction <> nil then
  {$ifdef ZEOSTRANS}
  begin
    if not fInternalTransaction.InTransaction then
      fInternalTransaction.Start(TACommit);
  end
  {$else}
    fInternalTransaction.Start
  {$endif ZEOSTRANS}
  else
  begin
    if (fInternalTPB = nil) then
      fInternalTPB := TSqlDBIbxConnection(Connection).GenerateTPB(fReadOnlyTransaction);
    fInternalTransaction := TSqlDBIbxConnection(Connection).Attachment.
      StartTransaction(fInternalTPB);
  end;
end;

procedure TSqlDBIbxStatement.InternalCommitTransaction;
begin
  if (fInternalTransaction <> nil) and
     fInternalTransaction.InTransaction then
  {$ifdef ZEOSTRANS}
    if fStatement.GetSQLStatementType in
         [SQLInsert, SQLUpdate, SQLDelete, SQLDDL, SQLSelectForUpdate,
          SQLSetGenerator] then
      fInternalTransaction.CommitRetaining;
  {$else}
    fInternalTransaction.Commit;
  {$endif ZEOSTRANS}
end;

procedure TSqlDBIbxStatement.CheckColAndRowset(const Col: integer);
begin
  if (fResultSet = nil) or
     (cardinal(Col) >= cardinal(fColumnCount)) then
    ErrorColAndRowset(Col);
end;

procedure TSqlDBIbxStatement.ErrorColAndRowset(const Col: integer);
begin
  raise ESqlDBIbx.CreateUtf8('%.ColumnInt(%) ResultSet=%',
    [self, Col, fResultSet]);
end;

function TSqlDBIbxStatement.IbxSQLTypeToTSqlDBFieldType(
  const aColMeta: TIBXColumnsMeta): TSqlDBFieldType;
var
  scale: integer;
begin
  case aColMeta.SQLType of
    SQL_VARYING,
    SQL_TEXT:
      result := ftUtf8;
    SQL_DOUBLE,
    SQL_FLOAT:
      result := ftDouble;
    SQL_TIMESTAMP,
    SQL_TIMESTAMP_TZ_EX,
    SQL_TIME_TZ_EX,
    SQL_TIMESTAMP_TZ,
    SQL_TIME_TZ,
    SQL_TYPE_TIME,
    SQL_TYPE_DATE:
      result := ftDate;
    SQL_BOOLEAN,
    SQL_LONG,
    SQL_SHORT,
    SQL_D_FLOAT,
    SQL_INT64:
      begin
        scale := aColMeta.Scale;
        if scale = 0 then
          result := ftInt64
        else
        if scale >= -4 then
          result := ftCurrency
        else
          result := ftDouble;
      end;
    SQL_BLOB:
      begin
        if aColMeta.Subtype = isc_blob_text then
          result := ftUtf8
        else
          result := ftBlob;
      end;
  else
    // SQL_INT128, SQL_DEC_FIXED, SQL_DEC16, SQL_DEC34
    // SQL_NULL, SQL_ARRAY, SQL_QUAD
    raise ESqlDBIbx.CreateUtf8('%: unexpected TIbxType %',
      [self, aColMeta.SQLType]);
  end;
end;

destructor TSqlDBIbxStatement.Destroy;
begin
  InternalCommitTransaction;
  if fResults <> nil then
    fResults.SetRetainInterfaces(false);
  if fResultSet <> nil then
    fResultSet.SetRetainInterfaces(false);
  fResultSet := nil;
  fResults := nil;
  if fStatement <> nil then
    fStatement.SetRetainInterfaces(false);
  fStatement := nil;
  fInternalTransaction := nil;
  fInternalTPB := nil;
  inherited Destroy;
end;

procedure TSqlDBIbxStatement.Prepare(const aSQL: RawUtf8; ExpectResults: boolean);
var
  con: TSqlDBIbxConnection;
  tr: ITransaction;
  fColumnMetaData: IColumnMetaData;
  i, n: PtrInt;
  name: string;
begin
  SQLLogBegin(sllDB);
  if (fStatement <> nil) or
     (fResultSet <> nil) then
    raise ESqlDBIbx.CreateUtf8('%.Prepare() shall be called once', [self]);
  inherited Prepare(aSQL, ExpectResults); // connect if necessary
  fReadOnlyTransaction := IdemPChar(pointer(fSQL), 'SELECT');
  con := (fConnection as TSqlDBIbxConnection);
  if not con.IsConnected then
    con.Connect;
  if (con.Transaction = nil) or
     not con.Transaction.GetInTransaction then
  begin
    fAutoStartCommitTrans := True;
    InternalStartTransaction;
    tr := fInternalTransaction;
  end
  else
  begin
    fAutoStartCommitTrans := False;
    tr := con.Transaction;
  end;
  fStatement := con.Attachment.Prepare(
    tr, {$ifdef UNICODE} Utf8ToString(fSQL) {$else} fSQL {$endif});
  fStatement.SetStaleReferenceChecks(false);
  fStatement.SetRetainInterfaces(true);
  fColumnCount := 0;
  fColumn.ForceReHash;
  if ExpectResults then
  begin
    fMeta := fStatement.GetMetaData;
    n := fMeta.getCount;
    SetLength(fColumnsMeta, n);
    fColumn.Capacity := n;
    for i := 0 to n - 1 do
    begin
      fColumnMetaData := fMeta.getColumnMetaData(i);
      fColumnsMeta[i].SQLType := fColumnMetaData.GetSQLType;
      fColumnsMeta[i].CodePage := fColumnMetaData.getCodePage;
      fColumnsMeta[i].Scale := fColumnMetaData.getScale;
      fColumnsMeta[i].Subtype := fColumnMetaData.getSubtype;
      name := fColumnMetaData.getName;
      PSqlDBColumnProperty(fColumn.AddAndMakeUniqueName(
        // Delphi<2009: already UTF-8 encoded due to controls_cp=CP_UTF8
        {$ifdef UNICODE} StringToUtf8 {$endif}(name)))^.ColumnType :=
          IbxSQLTypeToTSqlDBFieldType(fColumnsMeta[i]);
    end;
  end;
  SQLLogEnd;
end;

function Param2Type(const aParam: ISQLParam): RawUtf8;
begin
  case aParam.GetSQLType of
    SQL_VARYING,
    SQL_TEXT:
       FormatUtf8('VARCHAR(%)', [aParam.GetSize], result);
    SQL_DOUBLE,
    SQL_D_FLOAT:
       result := 'DOUBLE PRECISION';
    SQL_FLOAT:
       result := 'FLOAT';
    SQL_LONG:
      if aParam.getScale = 0 then
        result := 'INTEGER'
      else
      begin
        if aParam.getSubtype = 1 then
          FormatUtf8('NUMERIC(9,%)', [-aParam.getScale], result)
        else
          FormatUtf8('DECIMAL(9,%)', [-aParam.getScale], result);
      end;
    SQL_SHORT:
      if aParam.getScale = 0 then
        result := 'SMALLINT'
      else
      begin
        if aParam.getSubtype = 1 then
          FormatUtf8('NUMERIC(4,%)', [-aParam.getScale], result)
        else
          FormatUtf8('DECIMAL(4,%)', [-aParam.getScale], result);
      end;
    SQL_TIMESTAMP:
       result := 'TIMESTAMP';
    SQL_BLOB:
      if aParam.getSubtype = isc_blob_text then
        result := 'BLOB SUB_TYPE TEXT'
      else
        result := 'BLOB';
    //SQL_ARRAY = 540;
    //SQL_QUAD  = 550;
    SQL_TYPE_TIME:
       result := 'TIME';
    SQL_TYPE_DATE:
       result := 'DATE';
    SQL_INT64: // IB7
      if aParam.getScale = 0 then
        result := 'BIGINT'
      else
      begin
        if aParam.getSubtype = 1 then
          FormatUtf8('NUMERIC(18,%)', [-aParam.getScale], result)
        else
          FormatUtf8('DECIMAL(18,%)', [-aParam.getScale], result);
      end;
    SQL_BOOLEAN:
       result := 'BOOLEAN';
    SQL_NULL{FB25}:
       result := 'CHAR(1)';
  end;
end;

function Min(a, b: PtrInt): PtrInt;
  {$ifdef HASINLINE}inline;{$endif}
begin
  if a < b then
    result := a
  else
    result := b;
end;

procedure TSqlDBIbxStatement.ExecutePrepared;
var
  con: TSqlDBIbxConnection;
  iParams: ISQLParams;
  iParam : ISQLParam;
  i: integer;

  procedure BlockArrayExecute;
  const
    cMaxStm = 50;  // max statements in execute block, FB max is 255
  var
    oldSQL: RawUTF8;
    aPar: TRawUtf8DynArray;
    aParTyp: TRawUtf8DynArray;
    iP, iA, iStart, iEnd, iCnt, iStmCount: integer;
    W: TJsonWriter;
    newStatement: IStatement;

    procedure PrepareBlockStatement;
    begin
      newStatement := con.Attachment.Prepare(
        fStatement.GetTransaction,
        {$ifdef UNICODE} Utf8ToString(W.Text) {$else} W.Text {$endif});
      newStatement.SetStaleReferenceChecks(false);
    end;

    procedure ExecuteBlockStatement;
    var
      iP, iA, ndx: PtrInt;
    begin
      // Bind Params
      iParams := newStatement.GetSQLParams;
      ndx := fParamCount * (iEnd - iStart + 1);
      if iParams.Count <> ndx then
        raise ESqlDBIbx.CreateUtf8(
          '%.ExecutePrepared expected % bound parameters, got %',
          [self, iParams.Count, fParamCount * ndx]);
      for iP := 0 to fParamCount - 1 do
      begin
        if fParams[iP].VInt64 <> fParamsArrayCount then
          raise ESqlDBIbx.CreateUtf8(
            '%.ExecutePrepared: #% parameter expected array count %, got %',
            [self, iP, fParamsArrayCount, fParams[iP].VInt64]);
        with fParams[iP] do
        begin
          case VType of
            ftUnknown:
              raise ESqlDBIbx.CreateUtf8(
                '%.ExecutePrepared: Unknown type array parameter #%',
                [self, iP]);
            ftNull:
              // handle null column
              for iA := 0 to iEnd-iStart do
                iParams.getSQLParam(iA * fParamCount + iP).SetIsNull(true);
          else
            for iA := 0 to iEnd - iStart do
            begin
              iParam := iParams.getSQLParam(iA * fParamCount + iP);
              ndx := iA + iStart;
              if VArray[ndx] = 'null' then
                iParam.SetIsNull(true)
              else
              begin
                case VType of
                  ftDate:
                    iParam.SetAsDateTime(Iso8601ToDateTimePUtf8Char(
                      PUtf8Char(pointer(VArray[ndx])) + 1, Length(VArray[ndx]) - 2));
                  ftInt64:
                    iParam.SetAsInt64(GetInt64(pointer(VArray[ndx])));
                  ftDouble:
                    iParam.SetAsDouble(GetExtended(pointer(VArray[ndx])));
                  ftCurrency:
                    iParam.SetAsCurrency(StrToCurrency(pointer(VArray[ndx])));
                  ftUtf8:
                    iParam.SetAsString(UnQuoteSqlString(VArray[ndx]));
                  ftBlob:
                    iParam.SetAsString(VArray[ndx]);
                  else
                    raise ESqlDBIbx.CreateUtf8(
                      '%.ExecutePrepared: Invalid type parameter #%', [self, ndx]);
                end;
              end;
            end;
          end;
        end;
      end;
      // 4. Execute
      newStatement.Execute;
    end;

  begin
    // 1. Create execute block SQL
    oldSQL := StringReplaceAll(fSql, '?', '%');
    SetLength(aParTyp, fParamCount);
    SetLength(aPar, fParamCount);
    for iP := 0 to fParamCount-1 do
      aParTyp[iP] := Param2Type(iParams.Params[iP]);
    iStart := 0;
    iStmCount := Round(fParamsArrayCount /
                 Round(fParamsArrayCount / cMaxStm + 0.5));
    W := TJsonWriter.CreateOwnedStream(49152);
    try
      while iStart < fParamsArrayCount do
      begin
        iEnd := Min(iStart + iStmCount - 1, fParamsArrayCount - 1);
        if (iStart = 0) or
           (iEnd - iStart + 1 <> iStmCount) then
        begin
          iStmCount := iEnd - iStart + 1;
          W.CancelAll;
          W.AddShort('execute block('#10);
          iCnt := 0;
          for iA := iStart to iEnd do
            for iP := 0 to fParamCount - 1 do
            begin
              W.Add('p');
              W.AddU(iCnt);
              W.Add(' ');
              W.AddString(aParTyp[iP]);
              W.Add('=','?');
              W.AddComma;
              inc(iCnt);
            end;
          W.CancelLastComma;
          W.AddShort(') as begin'#10);
          iCnt := 0;
          for iA := iStart to iEnd do
          begin
            for iP := 0 to fParamCount - 1 do
            begin
              FormatUtf8(':p%', [iCnt], aPar[iP]);
              Inc(iCnt);
            end;
            W.Add(oldSQL, RawUtf8DynArrayToArrayOfConst(aPar));
            W.Add(';', #10);
          end;
          W.AddShort('end');
          PrepareBlockStatement;
        end;
        ExecuteBlockStatement;
        inc(iStart, iStmCount);
      end;
    finally
      W.Free;
    end;
  end;

begin
  SQLLogBegin(sllSQL);
  inherited ExecutePrepared;
  if fStatement = nil then
    raise ESqlDBIbx.CreateUtf8('%.ExecutePrepared() invalid call', [self]);
  con := (fConnection as TSqlDBIbxConnection);
  fAutoStartCommitTrans := (con.fTransaction=nil) or
                           not con.fTransaction.GetInTransaction;
  if fAutoStartCommitTrans and
     ((fInternalTransaction=nil) or
      not fInternalTransaction.GetInTransaction) then
  begin
    InternalStartTransaction;
    if not fStatement.IsPrepared then
    begin
      fStatement.Prepare(fInternalTransaction);
      fStatement.SetStaleReferenceChecks(false);
      fStatement.SetRetainInterfaces(true);
    end;
  end;
  iParams := fStatement.GetSQLParams;
  if fParamsArrayCount > 0 then      // Array Bindings
  begin
    if iParams.Count <> fParamCount then
      raise ESqlDBIbx.CreateUtf8(
        '%.ExecutePrepared expected % bound parameters, got %',
        [self, iParams.Count, fParamCount]);
    if fExpectResults then
      raise ESqlDBIbx.CreateUtf8(
        '%.ExecutePrepared cant ExpectResults with ArrayParams', [self]);
    BlockArrayExecute;
  end
  else
  begin
    if iParams.Count <> fParamCount then
      raise ESqlDBIbx.CreateUtf8(
        '%.ExecutePrepared expected % bound parameters, got %',
        [self, iParams.Count, fParamCount]);
    for i := 0 to fParamCount - 1 do
    // set parameters as expected by FirebirdSQL
    begin
      iParam := iParams.getSQLParam(i);
      with fParams[i] do
      begin
        case VType of
          ftUnknown,
          ftNull:
            iParam.SetIsNull(True);
          ftDate:
            iParam.SetAsDateTime(PDateTime(@VInt64)^);
          ftInt64:
            iParam.SetAsInt64(PInt64(@VInt64)^);
          ftDouble:
            iParam.SetAsDouble(unaligned(PDouble(@VInt64)^));
          ftCurrency:
            iParam.SetAsCurrency(PCurrency(@VInt64)^);
          ftUtf8:
            iParam.SetAsString(VData);
          ftBlob:
            iParam.SetAsString(VData);
        else
          raise ESqlDBIbx.CreateUtf8(
            '%.ExecutePrepared: Invalid type parameter #%', [self, i]);
        end;
      end;
    end;
    if fExpectResults then
    begin
      fCurrentRow := -1;
      if fAutoStartCommitTrans then
        fResultSet := fStatement.OpenCursor(fInternalTransaction)
      else
        fResultSet := fStatement.OpenCursor(con.fTransaction);
      fResults := fResultSet;
      fResultSet.SetRetainInterfaces(true);
      if not fResultSet.IsEof then
        fCurrentRow:=0;
      if fResultSet = nil then
        SynDBLog.Add.Log(sllWarning,'Ibx.ExecutePrepared returned nil %',
          [fSQL], self);
    end
    else
    begin
      if fAutoStartCommitTrans then
      begin
        fResults := fStatement.Execute(fInternalTransaction);
        InternalCommitTransaction;
      end
      else
        fResults := fStatement.Execute(con.fTransaction);
    end;
  end;
  SQLLogEnd;
end;

function TSqlDBIbxStatement.UpdateCount: integer;
var
  s, i, u, d: integer;
begin
  s := 0;
  i := 0;
  u := 0;
  d := 0;
  result := 0;
  if fStatement <> nil then
    if fStatement.GetRowsAffected(s, i, u, d) then
      result := i + u + d;
end;

procedure TSqlDBIbxStatement.Reset;
begin
  InternalCommitTransaction;
  inherited Reset;
end;

function TSqlDBIbxStatement.Step(SeekFirst: boolean): boolean;
begin
  if fColumnCount = 0 then // no row returned
    result := false
  else if fResultSet = nil then
    raise ESqlDBIbx.CreateUtf8('%.Step() invalid self', [self])
  else if SeekFirst then
  begin
    result := fResultSet.FetchNext;
    if result then
      fCurrentRow := 1
    else
    begin
      fCurrentRow := 0;
      InternalCommitTransaction;
    end;
  end
  else
  begin
    result := fResultSet.FetchNext;
    if result then
      inc(fCurrentRow)
    else
      InternalCommitTransaction;
  end;
  if not result then
    fResultSet.Close;
end;

procedure TSqlDBIbxStatement.ReleaseRows;
begin
  InternalCommitTransaction;
  if fResultSet <> nil then
  begin
    fResultSet.SetRetainInterfaces(false);
    fResultSet.Close;
    fResultSet := nil;
  end;
  if fResults <> nil then
  begin
    fResults.SetRetainInterfaces(false);
    fResults := nil;
  end;
  inherited ReleaseRows;
end;

function TSqlDBIbxStatement.ColumnInt(Col: integer): Int64;
begin
  CheckColAndRowset(Col);
  result := fResults[Col].GetAsInt64;
end;

function TSqlDBIbxStatement.ColumnNull(Col: integer): boolean;
var
  len: SmallInt;
  data: PByte;
begin
  CheckColAndRowset(Col);
  fResultSet.GetData(Col, result, len, data);
end;

function TSqlDBIbxStatement.ColumnDouble(Col: integer): double;
begin
  CheckColAndRowset(Col);
  result := fResults[Col].GetAsDouble;
end;

function TSqlDBIbxStatement.ColumnDateTime(Col: integer): TDateTime;
begin
  CheckColAndRowset(Col);
  result := fResults[Col].GetAsDateTime;
end;

function TSqlDBIbxStatement.ColumnCurrency(Col: integer): currency;
var
  nul: boolean;
  len: smallint;
  data: PByte;
begin
  CheckColAndRowset(Col);
  if fColumnsMeta[Col].Scale = -4 then
  begin
    fResults.GetData(Col, nul, len, data);
    PInt64(@result)^ := PInt64(data)^;
  end
  else
    result := fResults[Col].GetAsCurrency;
end;

function TSqlDBIbxStatement.ColumnUtf8(Col: integer): RawUtf8;
var
  nul: boolean;
  len: smallint;
  data: PByte;
begin
  CheckColAndRowset(Col);
  if fColumnsMeta[Col].CodePage = CP_UTF8 then
  begin
    fResults.GetData(Col, nul, len, data);
    FastSetString(result, data, len);
  end
  else
    result := fResults[Col].GetAsString;
end;

function TSqlDBIbxStatement.ColumnBlob(Col: integer): RawByteString;
begin
  CheckColAndRowset(Col);
  result := fResults[Col].GetAsString;
end;

procedure TSqlDBIbxStatement.ColumnsToJson(WR: TResultsWriter);
var
  I, H, C: integer;
  s:   RawUtf8;
  isNull: boolean;
  len: smallint;
  data: PByte;
begin
  if WR.Expand then
    WR.Add('{');
  if Assigned(WR.Fields) then
    H := High(WR.Fields)
  else
    H := High(WR.ColNames);
  for I := 0 to H do
  begin
    if Pointer(WR.Fields) = nil then
      C := I
    else
      C := WR.Fields[I];
    if WR.Expand then
      WR.AddString(WR.ColNames[I]); // add '"ColumnName":'
    fResults.GetData(C, isNull, len, data);
    if isNull then
      WR.AddNull
    else
    begin
      with fColumnsMeta[C] do
      case SQLType of
        SQL_VARYING,
        SQL_TEXT:
          begin
            WR.Add('"');
            if CodePage = CP_UTF8 then
              WR.AddJsonEscape(data, len)
            else
            begin
              s := fResults[C].AsString;
              WR.AddJsonEscape(pointer(s), length(s));
            end;
            WR.Add('"');
          end;
        SQL_DOUBLE,
        SQL_D_FLOAT:
          WR.AddDouble(PDouble(data)^);
        SQL_FLOAT:
          WR.AddSingle(PSingle(data)^);
        SQL_TIMESTAMP,
        SQL_TIMESTAMP_TZ_EX,
        SQL_TIME_TZ_EX,
        SQL_TIMESTAMP_TZ,
        SQL_TIME_TZ,
        SQL_TYPE_TIME,
        SQL_TYPE_DATE:
          begin
            WR.Add('"');
            WR.AddDateTime(fResults[C].GetAsDateTime, fForceDateWithMS);
            WR.Add('"');
          end;
        SQL_BOOLEAN:
          WR.Add(PByte(data)^ = 1);
        SQL_LONG:
          begin
            if Scale = 0 then
              WR.Add(PInteger(data)^)
            else
              WR.AddDouble(fResults[C].GetAsDouble);
          end;
        SQL_SHORT:
          begin
            if Scale=0 then
              WR.Add(PSmallInt(data)^)
            else
              WR.AddDouble(fResults[C].GetAsDouble);
          end;
        SQL_INT64:
          begin
            if Scale = 0 then
              WR.Add(PInt64(data)^)
            else
            if Scale = (-4) then
              WR.AddCurr64(PInt64(data))
            else
              WR.AddDouble(fResults[C].AsDouble);
          end;
        SQL_BLOB:
          begin
            if fForceBlobAsNull then
              WR.AddNull
            else
            begin
              if Subtype = isc_blob_text then
              begin
                s := fResults[C].AsString;
                WR.Add('"');
                WR.AddJsonEscape(pointer(s), length(s));
                WR.Add('"');
              end
              else
              begin
                s := fResults[C].GetAsString;
                WR.WrBase64(pointer(s), length(s), true);
              end;
            end;
          end
      else
        // SQL_INT128, SQL_DEC_FIXED, SQL_DEC16, SQL_DEC34
        // SQL_NULL, SQL_ARRAY, SQL_QUAD
        raise ESqlDBException.CreateUtf8(
          '%.ColumnsToJson: unexpected ColumnType(#% "%")=%',
          [self, C, fColumns[C].ColumnName, ord(fColumns[C].ColumnType)]);
      end;
    end;
    WR.AddComma;
  end;
  WR.CancelLastComma; // cancel last ','
  if WR.Expand then
    WR.Add('}');
  //FileFromString(WR.Text, FormatUtf8('row%.json', [fResults[0].AsString]));
end;


{ TSqlDBIbxConnection }

function TSqlDBIbxConnection.GetFirebirdAPI: IFirebirdAPI;
var
  fblib: IFirebirdLibrary;
begin
  if fFirebirdAPI = nil then
  begin
    if Trim(fFbLibraryPathName) = '' then
      fFirebirdAPI := IB.FirebirdAPI
    else
    begin
      fblib := IB.LoadFBLibrary(fFbLibraryPathName);
      if assigned(fblib) then
        fFirebirdAPI := fblib.GetFirebirdAPI;
    end;
  end;
  result := fFirebirdAPI;
end;

function TSqlDBIbxConnection.GenerateTPB(aReadOnly: boolean = false): ITPB;
begin
  result := FirebirdAPI.AllocateTPB;
  result.Add(isc_tpb_read_committed);
  result.Add(isc_tpb_rec_version);
  result.Add(isc_tpb_nowait);
  if aReadOnly then
    result.Add(isc_tpb_read)
  else
    result.Add(isc_tpb_write);
end;

constructor TSqlDBIbxConnection.Create(aProperties: TSqlDBConnectionProperties);
begin
  inherited Create(aProperties);
  fDBParams := TStringList.Create;
  with aProperties as TSqlDBIbxConnectionProperties do
  begin
    if DatabaseName = '' then
       raise ESqlDBIbx.CreateUtf8('% DatabaseName=''''', [self]);
    fFbLibraryPathName := FirebirdLibraryPathName;
    fCreateDbIfNotExists := CreateIfNotExists;
    fDBParams.Assign(IbxDBParams);
    if UserID <> '' then
      fDBParams.Values['user_name'] := UserID;
    if PassWord <> '' then
      fDBParams.Values['password'] := PassWord;
    if ServerName = '' then
      fDBName := DatabaseName
    else
      fDBName := ServerName + ':' + DatabaseName;
  end;
  if fDBParams.Values['lc_ctype'] = '' then
    fDBParams.Add('lc_ctype=UTF8');
end;

destructor TSqlDBIbxConnection.Destroy;
begin
  fDBParams.Free;
  inherited Destroy;
end;

procedure TSqlDBIbxConnection.Connect;
var
  DPB: IDPB;
  Status: IStatus;
  log: ISynLog;

  function GenerateDPB: IDPB;
  var
    i: PtrInt;
    ParamValue: string;
    DPBItem: IDPBItem;
  begin
    result := FirebirdAPI.AllocateDPB;
    // Iterate through the textual database parameters, constructing
    // a DPB on-the-fly
    for i := 0 to fDBParams.Count - 1 do
    begin
      // Get the parameter's name and value from the list, and make sure
      // that the name is all lowercase with no leading 'isc_dpb_' prefix
      if Trim(fDBParams.Names[i]) = '' then
        continue;
      DPBItem := result.AddByTypeName(fDBParams.Names[i]);
      ParamValue := fDBParams.ValueFromIndex[i];
       // A database parameter either contains a string value (case 1)
       // or an Integer value (case 2) or no value at all (case 3)
       // or an error needs to be generated (case else)
      case DPBItem.getParamType of
        isc_dpb_user_name,
        isc_dpb_password,
        isc_dpb_password_enc,
        isc_dpb_sys_user_name,
        isc_dpb_license,
        isc_dpb_encrypt_key,
        isc_dpb_lc_messages,
        isc_dpb_lc_ctype,
        isc_dpb_page_size,
        isc_dpb_sql_role_name:
          DPBItem.SetAsString(ParamValue);
        isc_dpb_sql_dialect:
          if (ParamValue = '') or
             (ParamValue[1] = '3') then
            DPBItem.SetAsString(#03)
          else
            DPBItem.SetAsString(#01);
        isc_dpb_num_buffers,
        isc_dpb_dbkey_scope,
        isc_dpb_force_write,
        isc_dpb_no_reserve,
        isc_dpb_damaged,
        isc_dpb_verify:
          DPBItem.SetAsByte(byte(ParamValue[1]));
        isc_dpb_sweep:
          DPBItem.SetAsByte(isc_dpb_records);
        isc_dpb_sweep_interval:
          DPBItem.SetAsInteger(StrToInt(ParamValue));
        isc_dpb_activate_shadow,
        isc_dpb_delete_shadow,
        isc_dpb_begin_log,
        isc_dpb_map_attach,
        isc_dpb_quit_log:
          DPBItem.SetAsByte(0);
      else
        raise ESqlDBIbx.CreateUtf8(
          '%.Connect() on % failed - IDPBItem type unsupported: %',
          [self, fProperties.ServerName, DPBItem.getParamTypeName]);
      end;
    end;
  end;

begin
  log := SynDBLog.Enter(self, 'Connect');
  if fAttachment<>nil then
     raise ESqlDBIbx.CreateUtf8(
       '%.Connect() on % failed: Attachment<>nil',
       [self, fProperties.ServerName]);
  DPB := GenerateDPB;
  fAttachment := FirebirdAPI.OpenDatabase(fDBName, DPB, false);
  if fAttachment = nil then
  begin
    Status := FirebirdAPI.GetStatus;
    if (Status.GetSQLCode = -902) and
       ((Status.GetIBErrorCode = isc_io_error) or
        (Status.GetIBErrorCode = isc_network_error)) and // Database not found
       fCreateDbIfNotExists then
    begin
      DPB.Add(isc_dpb_set_db_SQL_dialect).AsByte := 3; // use SQL Dialect 3
      if DPB.Find(isc_dpb_lc_ctype)=nil then
        DPB.Add(isc_dpb_lc_ctype).AsString := 'UTF8';
      fAttachment := FirebirdAPI.CreateDatabase(fDBName,DPB, false);
      if fAttachment=nil then
      begin
        DPB := GenerateDPB;
        fAttachment := FirebirdAPI.OpenDatabase(fDBName,DPB,false);
      end;
    end;
    if fAttachment = nil then
      raise ESqlDBIbx.CreateUtf8(
        '%.Connect() on % failed - SQLErrorCode: %, FbErrorCode: % ',
        [self, fDBName, Status.Getsqlcode, Status.GetIBErrorCode]);
  end;
  inherited Connect;
end;

procedure TSqlDBIbxConnection.Disconnect;
begin
  try
    inherited Disconnect; // flush any cached statement
  finally
    if fTransaction <> nil then
    begin
      if fTransaction.InTransaction then
        fTransaction.Commit;
      fTransaction := nil;
    end;
    if fAttachment <> nil then
    begin
      fAttachment.Disconnect(true);
      fAttachment := nil;
    end;
  end;
end;

function TSqlDBIbxConnection.IsConnected: boolean;
begin
  result := (fAttachment <> nil) and
             fAttachment.IsConnected;
end;

function TSqlDBIbxConnection.NewStatement: TSqlDBStatement;
begin
  result := TSqlDBIbxStatement.Create(self);
end;

procedure TSqlDBIbxConnection.StartTransaction;
var
  log: ISynLog;
begin
  log := SynDBLog.Enter(self, 'StartTransaction');
  if TransactionCount > 0 then
    raise ESqlDBIbx.CreateUtf8('Invalid %.StartTransaction: nested ' +
      'transactions are not supported/implemented', [self]);
  try
    inherited StartTransaction;
    if (fAttachment = nil) or
       not IsConnected then
      raise ESqlDBIbx.CreateUtf8('Invalid %.StartTransaction: ' +
        'Database not connected', [self]);
    if (fTransaction <> nil) and
       fTransaction.InTransaction then
      raise ESqlDBIbx.CreateUtf8('Invalid %.StartTransaction: ' +
        'Transaction is Started/InTransactions', [self]);
    if fTransaction <> nil then
      fTransaction.Start
    else
    begin
      if fTPB = nil then
        fTPB := GenerateTPB;
      fTransaction := fAttachment.StartTransaction(fTPB);
    end;
  except
    on E: Exception do
    begin
      if fTransactionCount > 0 then
         dec(fTransactionCount);
      raise;
    end;
  end;
end;

procedure TSqlDBIbxConnection.Commit;
begin
  inherited Commit;
  if fTransaction = nil then
    raise ESqlDBIbx.CreateUtf8('Invalid %.Commit call', [self]);
  try
    if fTransaction.InTransaction then
       fTransaction.Commit;
  except
    inc(fTransactionCount); // the transaction is still active
    raise;
  end;
end;

procedure TSqlDBIbxConnection.Rollback;
begin
  inherited Rollback;
  if fTransaction = nil then
    raise ESqlDBIbx.CreateUtf8('Invalid %.Rollback call', [self]);
  try
    if InTransaction then
       fTransaction.Rollback;
  except
    raise;
  end;
end;


{ TSqlDBIbxConnectionProperties }

function TSqlDBIbxConnectionProperties.IsPrimaryKeyIndexed(
  var AscendingOnly: boolean): boolean;
begin
  if fCreateDescendingOnlyPK then
    // SqlCreate() did already generate the needed DESC index
    // -> TRestStorageExternal.CreateSqlMultiIndex() has nothing to do
    result := true
  else
  begin
    // Firebird only creates an ASC index by default on its primary key
    // so max(RowID) is slow - see http://www.firebirdfaq.org/faq205
    // -> need to create a separated DESC index
    AscendingOnly := true;
    result := false;
  end;
end;

procedure TSqlDBIbxConnectionProperties.SetCreateDescendingOnlyPK(AValue: boolean);
begin
  // Engine='IBX0' - asc PK + desc NDX (our ORM default on dFirebird)
  // Engine='IBX1' - desc PK only (included in SqlCreate)
  fCreateDescendingOnlyPK := AValue;
  fEngineName := FormatUtf8('IBX%', [AValue]);
end;

procedure TSqlDBIbxConnectionProperties.GetForeignKeys;
begin
  { TODO : get FOREIGN KEYS }
end;

function TSqlDBIbxConnectionProperties.SqlFieldCreate(
  const aField: TSqlDBColumnCreate; var aAddPrimaryKey: RawUtf8): RawUtf8;
begin
  if (aField.DBType = ftUtf8) and
     (cardinal(aField.Width - 1) < fSqlCreateFieldMax) then
    FormatUtf8(fSqlCreateField[ftNull], [aField.Width], result)
  else
    result := fSqlCreateField[aField.DBType];
  if aField.NonNullable or aField.Unique or aField.PrimaryKey then
    result := result + ' NOT NULL';
  if aField.Unique and
     not aField.PrimaryKey then
    result := result + ' UNIQUE'; // see http://www.w3schools.com/sql/sql_unique.asp
  if aField.PrimaryKey then
  begin
    if fCreateDescendingOnlyPK then
      aAddPrimaryKey := aField.Name
    else
      result := result + ' PRIMARY KEY';
  end;
  result := aField.Name + result;
end;

constructor TSqlDBIbxConnectionProperties.Create(const aServerName,
  aDatabaseName, aUserID, aPassWord: RawUtf8);
begin
  fIbxDBParams := TStringList.Create;
  fCreateIfNotExists := true;
  SetCreateDescendingOnlyPK(false);
  fDbms := dFirebird;
  fBatchSendingAbilities := [cCreate, cUpdate, cDelete];
  fBatchMaxSentAtOnce := 5000;  // iters <= 32767 for better performance
  if aServerName = '' then
    ThreadingMode := tmMainConnection;
  fUseCache := true;
  inherited Create(aServerName, aDatabaseName, aUserID, aPassWord);
end;

destructor TSqlDBIbxConnectionProperties.Destroy;
begin
  fIbxDBParams.Free;
  inherited Destroy;
end;

function TSqlDBIbxConnectionProperties.NewConnection: TSqlDBConnection;
begin
  result := TSqlDBIbxConnection.Create(self);
  TSqlDBIbxConnection(result).InternalProcess(speCreated);
end;

function TSqlDBIbxConnectionProperties.SqlCreate(const aTableName: RawUtf8;
  const aFields: TSqlDBColumnCreateDynArray; aAddID: boolean): RawUtf8;
var
  i: PtrInt;
  F: RawUtf8;
  FieldID: TSqlDBColumnCreate;
  AddPrimaryKey: RawUtf8;
begin
  // use 'ID' instead of 'RowID' here since some DB (e.g. Oracle) use it
  result := '';
  if high(aFields) < 0 then
    exit; // nothing to create
  if aAddID then
  begin
    FieldID.DBType := ftInt64;
    FieldID.Name := ID_TXT;
    FieldID.Unique := true;
    FieldID.NonNullable := true;
    FieldID.PrimaryKey := true;
    result := SqlFieldCreate(FieldID, AddPrimaryKey) + ',';
  end;
  for i := 0 to high(aFields) do
  begin
    F := SqlFieldCreate(aFields[i], AddPrimaryKey);
    if i <> high(aFields) then
      F := F + ',';
    result := result + F;
  end;
  if AddPrimaryKey <> '' then
  begin
    result := result + ', PRIMARY KEY(' + AddPrimaryKey + ')';
    if fCreateDescendingOnlyPK then
      result := result + ' USING DESC INDEX PK_' + aTableName;
  end;
  result := 'CREATE TABLE ' + aTableName + ' (' + result + ')';
end;

{$endif NOSYNDBIBX}
// defined in mormot2.lpk Lazarus package > Custom Options > Defines

end.
