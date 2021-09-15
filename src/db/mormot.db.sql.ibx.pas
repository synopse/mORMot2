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

  - With explicit StartTransaction (or Batch): all statements in connection
    are executed within this main transaction owned by the connection.
  - If no explicit StartTransaction is called, all statements create their
    own proper transaction on prepare, and COMMIT them after execution or
    Eof or ReleaseRows. This implements a software-simulated "auto commit".
    Each internal transaction is owned by the associated Statement.
  - if TSqlDBIbxConnectionProperties.CreateDescendingPK is set to True (Default
    is False), it will create only one descending PK index using statement:
      PRIMARY KEY(ID) using desc index PK_TableName
    default dFirebird create two indexes on ID, one ascending, second descending
    nedded for select max(ID) - TRestStorageExternal.CreateSqlMultiIndex
    will detect the properties classname - see http://www.firebirdfaq.org/faq205
  - Batch is implemented for insert, update, delete using "execute block"
  - TODO Firebird4 API interface, with its new IBatch interface for insert/update
    also implemented in fbintf package.
  - You must patch FB30Statement.pas and FB25Statement.pas of fbintf, waiting
    for response from MWA Software. fbintf raise exception if Execute is
    executed within a different transaction than the prepared transaction.
    Just comment these lines in InternalExecute and InternalOpenCursor and rebuild package.
    more details on https://forum.lazarus.freepascal.org/index.php/topic,56267.0.html
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
  private
    fCreateDescendingPK: boolean;
    fFirebirdLibraryPathName: string;
    fIbxDBParams: TStringList;
    fCreateIfNotExists: boolean;
    procedure SetCreateDescendingPK(AValue: boolean);
  protected
    function IbxSQLTypeToTSqlDBFieldType(
      const aColMeta: IColumnMetaData): TSqlDBFieldType;
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
    // Override to enable descending PK
    function SqlCreate(const aTableName: RawUtf8;
      const aFields: TSqlDBColumnCreateDynArray; aAddID: boolean): RawUtf8; override;
  published
    /// Full file path name to firebird client dll (fbclient.dll), default ''
    property FirebirdLibraryPathName: RawUtf8
      read fFirebirdLibraryPathName write fFirebirdLibraryPathName;
    // You can add additional DB Params, see documentation in IBX/FB Pascal API
    property IbxDBParams: TStringList
      read fIbxDBParams;
    // Create database file if not exists, default is True
    property CreateIfNotExists: boolean
      read fCreateIfNotExists;
    // All firebird mormot drivers create ascending PK index on ID and
    // another descending index on ID
    // - see http://www.firebirdfaq.org/faq205
    // - Having two indexes on same column slow down any insert, updata, delete
    // - Setting CreateDescendingPK := True driver will create only one
    // descending PK index using this statement:
    // $ PRIMARY KEY(ID) using desc index PK_TableName
    property CreateDescendingPK: boolean
      read fCreateDescendingPK write SetCreateDescendingPK;
  end;

  /// implements a connection via the IBX/FB Pascal API access layer
  TSqlDBIbxConnection = class(TSqlDBConnectionThreadSafe)
  private
    function GetFirebirdAPI: IFirebirdAPI;
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
    function GenerateTPB: ITPB;
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

  /// implements a statement via a IBX/FB Pascal API database connection
  TSqlDBIbxStatement = class(TSqlDBStatementWithParamsAndColumns)
  protected
    fAutoStartCommitTrans: boolean;
    fStatement: IStatement;
    fResultSet: IResultSet;
    fResults: IResults;
    // Internal Transaction used for all statements if not explicit StartTransaction/Batch
    // This transaction is Started and COMMIT after execution (auto commit)
    fInternalTPB: ITPB;
    fInternalTransaction: ITransaction;
    procedure InternalStartTransaction;
    procedure InternalCommitTransaction;
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
    procedure ColumnsToJson(WR: TJsonWriter); override;
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
    fInternalTransaction.Start(TACommit)
  {$endif ZEOSTRANS}
  else
  begin
    if (fInternalTPB = nil) then
       fInternalTPB := TSqlDBIbxConnection(Connection).GenerateTPB;
    fInternalTransaction := TSqlDBIbxConnection(Connection).Attachment.
      StartTransaction(fInternalTPB, TACommit);
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
  {$endif ZEOSTRANS}
      fInternalTransaction.Commit;
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
begin
  SQLLogBegin(sllDB);
  if (fStatement <> nil) or
     (fResultSet <> nil) then
    raise ESqlDBIbx.CreateUtf8('%.Prepare() shall be called once', [self]);
  inherited Prepare(aSQL, ExpectResults); // connect if necessary
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
  SQLLogEnd;
end;

function DynRawUtf8ArrayToConst(const aValue: TRawUtf8DynArray): TTVarRecDynArray;
var ndx: PtrInt;
begin
  SetLength(Result, Length(aValue));
  for ndx := 0 to Length(aValue) - 1 do
  begin
    result[ndx].VType := vtAnsiString;
    result[ndx].VAnsiString := pointer(aValue[ndx]);
  end;
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
      else begin
        if aParam.getSubtype = 1 then
          FormatUtf8('NUMERIC(9,%)', [aParam.getScale], result)
        else
          FormatUtf8('DECIMAL(9,%)', [aParam.getScale], result);
      end;
    SQL_SHORT:
      if aParam.getScale = 0 then
        result := 'SMALLINT'
      else begin
        if aParam.getSubtype = 1 then
          FormatUtf8('NUMERIC(4,%)', [aParam.getScale], result)
        else
          FormatUtf8('DECIMAL(4,%)', [aParam.getScale], result);
      end;
    SQL_TIMESTAMP:
       result := 'TIMESTAMP';
    SQL_BLOB:
      if aParam.getSubtype = isc_blob_text then
        result := 'BLOB SUB_TYPE TEXT'
      else
        result := 'BLOB';
    //SQL_ARRAY                      = 540;
    //SQL_QUAD                       = 550;
    SQL_TYPE_TIME:
       result := 'TIME';
    SQL_TYPE_DATE:
       result := 'DATE';
    SQL_INT64: // IB7
      if aParam.getScale = 0 then
        result := 'BIGINT'
      else begin
        if aParam.getSubtype = 1 then
          FormatUtf8('NUMERIC(18,%)', [aParam.getScale], result)
        else
          FormatUtf8('DECIMAL(18,%)', [aParam.getScale], result);
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
  iMeta: IMetaData;
  iColMeta: IColumnMetaData;
  i, n: integer;
  name: string;
  Props: TSqlDBIbxConnectionProperties;

  procedure BlockArrayExecute;
  const
    cMaxStm = 100;  // max statements in execute block, FB max is 255
  var
    oldSQL: RawUTF8;
    aPar: TRawUtf8DynArray;
    aParTyp: TRawUtf8DynArray;
    iP, iA, iStart, iEnd, iCnt, iStmCount: integer;
    W: TTextWriter;
    newStatement: IStatement;

    procedure PrepareBlockStatement;
    begin
      newStatement := con.Attachment.Prepare(
        fStatement.GetTransaction,
        {$ifdef UNICODE} Utf8ToString(W.Text) {$else} W.Text {$endif});
    end;

    procedure ExecuteBlockStatement;
    var
      iP, iA, ndx: integer;
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
                iParams.Params[iA * fParamCount + iP].SetIsNull(true);
          else
            for iA := 0 to iEnd-iStart do
            begin
              iParam := iParams.Params[iA * fParamCount + iP];
              ndx := iA + iStart;
              if VArray[ndx] = 'null' then
                iParam.SetIsNull(true)
              else
              begin
                case VType of
                  ftDate:
                    iParam.AsDateTime := Iso8601ToDateTimePUtf8Char(
                      PUtf8Char(pointer(VArray[ndx])) + 1, Length(VArray[ndx]) - 2);
                  ftInt64:
                    iParam.AsInt64 := GetInt64(pointer(VArray[ndx]));
                  ftDouble:
                    iParam.AsDouble := GetExtended(pointer(VArray[ndx]));
                  ftCurrency:
                    iParam.AsCurrency := StrToCurrency(pointer(VArray[ndx]));
                  ftUtf8:
                    iParam.AsString := UnQuoteSqlString(VArray[ndx]);
                  ftBlob:
                    iParam.AsString := VArray[ndx];
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
    for iP:=0 to fParamCount-1 do
      aParTyp[iP] := Param2Type(iParams.Params[iP]);
    iStart := 0;
    iStmCount := Round(fParamsArrayCount /
                       Round( fParamsArrayCount / cMaxStm + 0.5));
    W := TTextWriter.CreateOwnedStream(49152);
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
            W.Add(oldSQL, DynRawUtf8ArrayToConst(aPar));
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
      fStatement.Prepare(fInternalTransaction);
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
      iParam := iParams.Params[i];
      with fParams[i] do
      begin
        case VType of
          ftUnknown,ftNull:
            begin
              iParam.IsNull := True;
            end;
          ftDate:
            begin
              iParam.AsDateTime := PDateTime(@VInt64)^;
            end;
          ftInt64:
            begin
              iParam.AsInt64 := PInt64(@VInt64)^;
            end;
          ftDouble:
            begin
              iParam.AsDouble := unaligned(PDouble(@VInt64)^);
            end;
          ftCurrency:
            begin
              iParam.AsCurrency := PCurrency(@VInt64)^;
            end;
          ftUtf8:
            begin
              iParam.AsString := VData;
            end;
          ftBlob:
            begin
              iParam.AsString := VData;
            end;
          else
            raise ESqlDBIbx.CreateUtf8(
              '%.ExecutePrepared: Invalid type parameter #%', [self, i]);
        end;
      end;
    end;
    if fExpectResults then
    begin
      fColumnCount := 0;
      fColumn.ReHash;
      fCurrentRow := -1;
      if fAutoStartCommitTrans then
        fResultSet := fStatement.OpenCursor(fInternalTransaction)
      else
        fResultSet := fStatement.OpenCursor(con.fTransaction);
      fResults := fResultSet;
      fResultSet.SetRetainInterfaces(true);
      fResults.SetRetainInterfaces(true);
      if not fResultSet.IsEof then
        fCurrentRow:=0;
      if fResultSet = nil then
        SynDBLog.Add.Log(sllWarning,'Ibx.ExecutePrepared returned nil %',
          [fSQL], self)
      else
      begin
        Props := fConnection.Properties as TSqlDBIbxConnectionProperties;
        iMeta := fStatement.GetMetaData;
        n := iMeta.getCount;
        fColumn.Capacity := n;
        for i := 0 to n - 1 do
        begin
          iColMeta := iMeta.getColumnMetaData(i);
          name := iColMeta.getName;
          PSqlDBColumnProperty(fColumn.AddAndMakeUniqueName(
            // Delphi<2009: already UTF-8 encoded due to controls_cp=CP_UTF8
            {$ifdef UNICODE} StringToUtf8 {$endif}(name)))^.ColumnType :=
              Props.IbxSQLTypeToTSqlDBFieldType(iColMeta);
        end;
      end;
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
  if (fResultSet = nil) or
     (cardinal(Col) >= cardinal(fColumnCount)) then
    raise ESqlDBIbx.CreateUtf8('%.ColumnInt(%) ResultSet=%',
      [self, Col, fResultSet]);
  result := fResultSet.Data[Col].GetAsInt64;
end;

function TSqlDBIbxStatement.ColumnNull(Col: integer): boolean;
begin
  if (fResultSet = nil) or
     (cardinal(Col) >= cardinal(fColumnCount)) then
    raise ESqlDBIbx.CreateUtf8('%.ColumnNull(%) ResultSet=%',
      [self, Col, fResultSet]);
  result := fResultSet.Data[Col].GetIsNull;
end;

function TSqlDBIbxStatement.ColumnDouble(Col: integer): double;
begin
  if (fResultSet = nil) or
     (cardinal(Col) >= cardinal(fColumnCount)) then
    raise ESqlDBIbx.CreateUtf8('%.ColumnDouble(%) ResultSet=%',
      [self, Col, fResultSet]);
  result := fResultSet.Data[Col].GetAsDouble;
end;

function TSqlDBIbxStatement.ColumnDateTime(Col: integer): TDateTime;
begin
  if (fResultSet = nil) or
     (cardinal(Col) >= cardinal(fColumnCount)) then
    raise ESqlDBIbx.CreateUtf8('%.ColumnDateTime(%) ResultSet=%',
      [self, Col, fResultSet]);
  result := fResultSet.Data[Col].GetAsDateTime;
end;

function TSqlDBIbxStatement.ColumnCurrency(Col: integer): currency;
begin
  if (fResultSet = nil) or
     (cardinal(Col) >= cardinal(fColumnCount)) then
    raise ESqlDBIbx.CreateUtf8('%.ColumnCurrency(%) ResultSet=%',
      [self, Col, fResultSet]);
  result := fResultSet.Data[Col].GetAsCurrency;
end;

function TSqlDBIbxStatement.ColumnUtf8(Col: integer): RawUtf8;
begin
  if (fResultSet = nil) or
     (cardinal(Col) >= cardinal(fColumnCount)) then
    raise ESqlDBIbx.CreateUtf8('%.ColumnUtf8(%) ResultSet=%',
      [self, Col, fResultSet]);
  result := fResultSet.Data[Col].GetAsString;
end;

function TSqlDBIbxStatement.ColumnBlob(Col: integer): RawByteString;
begin
  if (fResultSet = nil) or
     (cardinal(Col) >= cardinal(fColumnCount)) then
    raise ESqlDBIbx.CreateUtf8('%.ColumnBlob(%) ResultSet=%',
      [self, Col, fResultSet]);
  result := fResultSet.Data[Col].GetAsString;
end;

procedure TSqlDBIbxStatement.ColumnsToJson(WR: TJsonWriter);
var
  col: integer;
  s:   RawUtf8;
begin
  if WR.Expand then
    WR.Add('{');
  for col := 0 to fColumnCount - 1 do
  begin
    if WR.Expand then
      WR.AddFieldName(fColumns[col].ColumnName); // add '"ColumnName":'
    if fResultSet.Data[Col].IsNull then
      WR.AddNull
    else
    begin
      case fColumns[col].ColumnType of
        ftNull:
          WR.AddNull;
        ftInt64:
          WR.Add(fResultSet.Data[col].AsInt64);
        ftDouble:
          WR.AddDouble(fResultSet.Data[col].AsDouble);
        ftCurrency:
          WR.AddCurr(fResultSet.Data[col].AsCurrency);
        ftDate:
          begin
            WR.Add('"');
            WR.AddDateTime(fResultSet.Data[col].GetAsDateTime,
              fForceDateWithMS);
            WR.Add('"');
          end;
        ftUtf8:
          begin
            WR.Add('"');
            s := fResultSet.Data[col].GetAsString;
            WR.AddJsonEscape(pointer(s), length(s));
            WR.Add('"');
          end;
        ftBlob:
          begin
            if fForceBlobAsNull then
              WR.AddNull
            else
            begin
              s := fResultSet.Data[col].GetAsString;
              WR.WrBase64(pointer(s), length(s), true);
            end;
          end
      else
        raise ESqlDBException.CreateUtf8(
          '%.ColumnsToJson: invalid ColumnType(#% "%")=%',
          [self, col, fColumns[col].ColumnName, ord(fColumns[col].ColumnType)]);
      end;
    end;
    WR.AddComma;
  end;
  WR.CancelLastComma; // cancel last ','
  if WR.Expand then
    WR.Add('}');
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

function TSqlDBIbxConnection.GenerateTPB: ITPB;
begin
  result := FirebirdAPI.AllocateTPB;
  result.Add(isc_tpb_read_committed);
  result.Add(isc_tpb_rec_version);
  result.Add(isc_tpb_nowait);
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

  procedure GenerateDPB;
  var
    i: PtrInt;
    ParamValue: string;
    DPBItem: IDPBItem;
  begin
    DPB := FirebirdAPI.AllocateDPB;
    // Iterate through the textual database parameters, constructing
    // a DPB on-the-fly
    for i := 0 to fDBParams.Count - 1 do
    begin
      // Get the parameter's name and value from the list, and make sure
      // that the name is all lowercase with no leading 'isc_dpb_' prefix
      if Trim(fDBParams.Names[i]) = '' then
        continue;
      DPBItem := DPB.AddByTypeName(fDBParams.Names[i]);
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
  if fAttachment<>nil then
     raise ESqlDBIbx.CreateUtf8(
       '%.Connect() on % failed: Attachment<>nil',
       [self, fProperties.ServerName]);
  GenerateDPB;
  fAttachment := FirebirdAPI.OpenDatabase(fDBName,DPB,false);
  if fAttachment = nil then
  begin
    Status := FirebirdAPI.GetStatus;
    if (Status.GetSQLCode = -902) and
       (Status.GetIBErrorCode = isc_io_error) and // Database not found
       fCreateDbIfNotExists then
    begin
      DPB.Add(isc_dpb_set_db_SQL_dialect).AsByte := 3; // use SQL Dialect 3
      fAttachment := FirebirdAPI.CreateDatabase(fDBName,DPB, false);
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
      fTransaction.Start(TACommit)
    else
    begin
      if fTPB = nil then
        fTPB := GenerateTPB;
      fTransaction := fAttachment.StartTransaction(fTPB, TACommit);
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

procedure TSqlDBIbxConnectionProperties.SetCreateDescendingPK(AValue: boolean);
begin
  fCreateDescendingPK:=AValue;
  fEngineName:=FormatUtf8('IBX%', [AValue]);
end;

function TSqlDBIbxConnectionProperties.IbxSQLTypeToTSqlDBFieldType(
  const aColMeta: IColumnMetaData): TSqlDBFieldType;
begin
  case aColMeta.GetSQLType of
    SQL_VARYING, SQL_TEXT:
      result := ftUtf8;
    SQL_DOUBLE, SQL_FLOAT:
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
    SQL_QUAD,
    SQL_INT64:
      begin
        if aColMeta.getScale >= (-4) then
          result := ftCurrency
        else
          result := ftInt64;
      end;
    SQL_BLOB:
      begin
        if aColMeta.getSubtype = isc_blob_text then
          result := ftUtf8
        else
          result := ftBlob;
      end
  else
    //    SQL_INT128, SQL_DEC_FIXED, SQL_DEC16, SQL_DEC34
    //    SQL_NULL, SQL_ARRAY
    raise ESqlDBIbx.CreateUtf8('%: unexpected TIbxType % "%"', [self,
      aColMeta.GetSQLType, aColMeta.GetSQLTypeName]);
  end;
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
    if fCreateDescendingPK then
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
  SetCreateDescendingPK(false);
  fDbms := dFirebird;
  fBatchSendingAbilities := [cCreate, cUpdate, cDelete];
  fBatchMaxSentAtOnce := 1000;  // iters <= 32767 for better performance
  if aServerName = '' then
    ThreadingMode := tmMainConnection;
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
    if fCreateDescendingPK then
      result := result + ' using desc index PK_' + aTableName;
  end;
  result := 'CREATE TABLE ' + aTableName + ' (' + result + ')';
end;

{$endif NOSYNDBIBX}
// defined in mormot2.lpk Lazarus package > Custom Options > Defines

end.
