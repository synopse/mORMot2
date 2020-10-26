/// Database Framework Low-Level OleDB Access
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.db.raw.oledb;

{
  *****************************************************************************

   Efficient Direct OleDB API Access
    - Native OleDB Constants
    - Native OleDB Memory Structures
    - Native OleDB Interfaces
    - Low-Level OleDB Custom RowSet Processing
    - OleDB High-Level Access

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}


{$ifdef MSWINDOWS} // compiles as void unit for non-Windows - allow Lazarus package

uses
  sysutils,
  classes,
  ActiveX,
  ComObj,
  Windows,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.core.log,
  mormot.db.core,
  mormot.db.sql;


{ ************ Native OleDB Constants }

const
  IID_IUnknown: TGUID = '{00000000-0000-0000-C000-000000000046}';
  IID_IAccessor: TGUID = '{0C733A8C-2A1C-11CE-ADE5-00AA0044773D}';
  IID_IRowset: TGUID = '{0C733A7C-2A1C-11CE-ADE5-00AA0044773D}';
  IID_IMultipleResults: TGUID = '{0C733A90-2A1C-11CE-ADE5-00AA0044773D}';
  IID_IOpenRowset: TGUID = '{0C733A69-2A1C-11CE-ADE5-00AA0044773D}';
  IID_IDataInitialize: TGUID = '{2206CCB1-19C1-11D1-89E0-00C04FD7A829}';
  IID_IDBInitialize: TGUID = '{0C733A8B-2A1C-11CE-ADE5-00AA0044773D}';
  IID_ICommandText: TGUID = '{0C733A27-2A1C-11CE-ADE5-00AA0044773D}';
  IID_ISSCommandWithParameters: TGUID = '{EEC30162-6087-467C-B995-7C523CE96561}';
  IID_ITransactionLocal: TGUID = '{0C733A5F-2A1C-11CE-ADE5-00AA0044773D}';
  IID_IDBPromptInitialize: TGUID = '{2206CCB0-19C1-11D1-89E0-00C04FD7A829}';
  IID_ISQLServerErrorInfo: TGUID = '{5CF4CA12-EF21-11d0-97E7-00C04FC2AD98}';
  IID__Catalog: TGUID = '{00000603-0000-0010-8000-00AA006D2EA4}';
  CLASS_Catalog: TGUID = '{00000602-0000-0010-8000-00AA006D2EA4}';
  CLSID_DATALINKS: TGUID = '{2206CDB2-19C1-11D1-89E0-00C04FD7A829}';
  CLSID_MSDAINITIALIZE: TGUID = '{2206CDB0-19C1-11D1-89E0-00C04FD7A829}';
  CLSID_ROWSET_TVP: TGUID = '{C7EF28D5-7BEE-443F-86DA-E3984FCD4DF9}';
  DB_NULLGUID: TGuid = '{00000000-0000-0000-0000-000000000000}';
  DBGUID_DEFAULT: TGUID = '{C8B521FB-5CF3-11CE-ADE5-00AA0044773D}';
  DBSCHEMA_TABLES: TGUID = '{C8B52229-5CF3-11CE-ADE5-00AA0044773D}';
  DBSCHEMA_COLUMNS: TGUID = '{C8B52214-5CF3-11CE-ADE5-00AA0044773D}';
  DBSCHEMA_INDEXES: TGUID = '{C8B5221E-5CF3-11CE-ADE5-00AA0044773D}';
  DBSCHEMA_FOREIGN_KEYS: TGUID = '{C8B522C4-5CF3-11CE-ADE5-00AA0044773D}';
  DBPROPSET_SQLSERVERPARAMETER: TGUID = '{FEE09128-A67D-47EA-8D40-24A1D4737E8D}';

  // PropIds for DBPROPSET_SQLSERVERPARAMETER
  SSPROP_PARAM_XML_SCHEMACOLLECTION_CATALOGNAME = 24;
  SSPROP_PARAM_XML_SCHEMACOLLECTION_SCHEMANAME = 25;
  SSPROP_PARAM_XML_SCHEMACOLLECTIONNAME = 26;
  SSPROP_PARAM_UDT_CATALOGNAME = 27;
  SSPROP_PARAM_UDT_SCHEMANAME = 28;
  SSPROP_PARAM_UDT_NAME = 29;
  SSPROP_PARAM_TYPE_CATALOGNAME = 38;
  SSPROP_PARAM_TYPE_SCHEMANAME = 39;
  SSPROP_PARAM_TYPE_TYPENAME = 40;
  SSPROP_PARAM_TABLE_DEFAULT_COLUMNS = 41;
  SSPROP_PARAM_TABLE_COLUMN_SORT_ORDER = 42;

  DBTYPE_EMPTY = $00000000;
  DBTYPE_NULL = $00000001;
  DBTYPE_I2 = $00000002;
  DBTYPE_I4 = $00000003;
  DBTYPE_R4 = $00000004;
  DBTYPE_R8 = $00000005;
  DBTYPE_CY = $00000006;
  DBTYPE_DATE = $00000007;
  DBTYPE_BSTR = $00000008;
  DBTYPE_IDISPATCH = $00000009;
  DBTYPE_ERROR = $0000000A;
  DBTYPE_BOOL = $0000000B;
  DBTYPE_VARIANT = $0000000C;
  DBTYPE_IUNKNOWN = $0000000D;
  DBTYPE_DECIMAL = $0000000E;
  DBTYPE_UI1 = $00000011;
  DBTYPE_ARRAY = $00002000;
  DBTYPE_BYREF = $00004000;
  DBTYPE_I1 = $00000010;
  DBTYPE_UI2 = $00000012;
  DBTYPE_UI4 = $00000013;
  DBTYPE_I8 = $00000014;
  DBTYPE_UI8 = $00000015;
  DBTYPE_GUID = $00000048;
  DBTYPE_VECTOR = $00001000;
  DBTYPE_RESERVED = $00008000;
  DBTYPE_BYTES = $00000080;
  DBTYPE_STR = $00000081;
  DBTYPE_WSTR = $00000082;
  DBTYPE_NUMERIC = $00000083;
  DBTYPE_UDT = $00000084;
  DBTYPE_DBDATE = $00000085;
  DBTYPE_DBTIME = $00000086;
  DBTYPE_DBTIMESTAMP = $00000087;
  DBTYPE_FILETIME = $00000040;
  DBTYPE_DBFILETIME = $00000089;
  DBTYPE_PROPVARIANT = $0000008A;
  DBTYPE_VARNUMERIC = $0000008B;
  DBTYPE_TABLE = $0000008F; // introduced in SQL 2008

  DBPARAMIO_NOTPARAM = $00000000;
  DBPARAMIO_INPUT = $00000001;
  DBPARAMIO_OUTPUT = $00000002;

  DBPARAMFLAGS_ISINPUT    = $00000001;
  DBPARAMFLAGS_ISOUTPUT   = $00000002;
  DBPARAMFLAGS_ISSIGNED   = $00000010;
  DBPARAMFLAGS_ISNULLABLE = $00000040;
  DBPARAMFLAGS_ISLONG     = $00000080;

  DBPART_VALUE = $00000001;
  DBPART_LENGTH = $00000002;
  DBPART_STATUS = $00000004;

  DBMEMOWNER_CLIENTOWNED = $00000000;
  DBMEMOWNER_PROVIDEROWNED = $00000001;

  DBACCESSOR_ROWDATA = $00000002;
  DBACCESSOR_PARAMETERDATA = $00000004;
  DBACCESSOR_OPTIMIZED = $00000008;

  DB_E_CANCELED = HRESULT($80040E4E);
  DB_E_NOTSUPPORTED = HRESULT($80040E53);
  DBCOLUMNFLAGS_MAYBENULL = $00000040;
  ISOLATIONLEVEL_READCOMMITTED = $00001000;
  DBPROMPTOPTIONS_PROPERTYSHEET = $2;
  DB_NULL_HCHAPTER = $00;
  DB_S_ENDOFROWSET = $00040EC6;
  XACTTC_SYNC = $00000002;

  MAXBOUND = 65535; { High bound for arrays }

  DBKIND_GUID_NAME     = 0;
  DBKIND_GUID_PROPID   = DBKIND_GUID_NAME + 1;
  DBKIND_NAME          = DBKIND_GUID_PROPID + 1;
  DBKIND_PGUID_NAME    = DBKIND_NAME + 1;
  DBKIND_PGUID_PROPID  = DBKIND_PGUID_NAME + 1;
  DBKIND_PROPID        = DBKIND_PGUID_PROPID + 1;
  DBKIND_GUID          = DBKIND_PROPID + 1;

  IDList_type: WideString = 'IDList';
  StrList_TYPE: WideString = 'StrList';

type
  /// indicates whether the data value or some other value, such as a NULL,
  // is to be used as the value of the column or parameter
  // - see http://msdn.microsoft.com/en-us/library/ms722617
  // and http://msdn.microsoft.com/en-us/library/windows/desktop/ms716934
  TSQLDBOleDBStatus = (
    stOK, stBadAccessor, stCanNotConvertValue, stIsNull, stTruncated,
    stSignMismatch, stDataoverFlow, stCanNotCreateValue, stUnavailable,
    stPermissionDenied, stIntegrityViolation, stSchemaViolation, stBadStatus,
    stDefault, stCellEmpty, stIgnoreColumn, stDoesNotExist, stInvalidURL,
    stResourceLocked, stResoruceExists, stCannotComplete, stVolumeNotFound,
    stOutOfSpace, stCannotDeleteSource, stAlreadyExists, stCanceled,
    stNotCollection, stRowSetColumn);

  /// binding status of a given column
  // - see http://msdn.microsoft.com/en-us/library/windows/desktop/ms720969
  // and http://msdn.microsoft.com/en-us/library/windows/desktop/ms716934
  TSQLDBOleDBBindStatus = (
    bsOK, bsBadOrdinal, bsUnsupportedConversion, bsBadBindInfo,
    bsBadStorageFlags, bsNoInterface, bsMultipleStorage);

  PIUnknown = ^IUnknown;
  HACCESSOR = PtrUInt;
  HACCESSORDynArray = array of HACCESSOR;
  HCHAPTER = PtrUInt;
  HROW = PtrUInt;
  PHROW = ^HROW;

  DBPART = UINT;
  DBMEMOWNER = UINT;
  DBPARAMIO = UINT;
  DBPROPSTATUS = UINT;
  DBPROPID = UINT;
  DBPROPOPTIONS = UINT;
  DBCOLUMNFLAGS = UINT;
  DBKIND = UINT;
  DBSTATUS = DWORD;
  DBPARAMFLAGS = DWORD;
  DBTYPE = Word;
  DBRESULTFLAG = UINT;

  DBLENGTH = PtrUInt;
  DB_UPARAMS = PtrUInt;
  DBORDINAL = PtrUInt;


{ ************ Native OleDB Memory Structures }

type
{$ifdef CPU64}
  {$A8} // un-packed records
{$else}
  {$A-} // packed records
{$endif}
  TBoid = record
    rgb_: array[0..15] of Byte;
  end;
  PBoid = ^TBoid;

  TXactOpt = record
    ulTimeout: UINT;
    szDescription: array[0..39] of Shortint;
  end;

  TXactTransInfo = record
    uow: PBoid;
    isoLevel: Integer;
    isoFlags: UINT;
    grfTCSupported: UINT;
    grfRMSupported: UINT;
    grfTCSupportedRetaining: UINT;
    grfRMSupportedRetaining: UINT;
  end;

  PErrorInfo = ^TErrorInfo;
  TErrorInfo = record
    hrError: HRESULT;
    dwMinor: UINT;
    clsid: TGUID;
    iid: TGUID;
    dispid: Integer;
  end;

  PDBParams = ^TDBParams;
  TDBParams = record
    pData: Pointer;
    cParamSets: PtrUInt;
    HACCESSOR: HACCESSOR;
  end;

  PDBObject = ^TDBObject;
  TDBObject = record
    dwFlags: UINT;
    iid: TGUID;
  end;

  PDBBindExt = ^TDBBindExt;
  TDBBindExt = record
    pExtension: PByte;
    ulExtension: PtrUInt;
  end;

  PDBBinding = ^TDBBinding;
  TDBBinding = record
    iOrdinal: DBORDINAL;
    obValue: PtrUInt;
    obLength: PtrUInt;
    obStatus: PtrUInt;
    pTypeInfo: ITypeInfo;
    pObject: PDBObject;
    pBindExt: PDBBindExt;
    dwPart: DBPART;
    dwMemOwner: DBMEMOWNER;
    eParamIO: DBPARAMIO;
    cbMaxLen: PtrUInt;
    dwFlags: UINT;
    wType: DBTYPE;
    bPrecision: Byte;
    bScale: Byte;
  end;
  PDBBindingArray = ^TDBBindingArray;
  TDBBindingArray = array[0..MAXBOUND] of TDBBinding;
  TDBBindingDynArray = array of TDBBinding;

  DBIDGUID = record
    case Integer of
      0: (guid: TGUID);
      1: (pguid: ^TGUID);
  end;

  DBIDNAME = record
    case Integer of
      0: (pwszName: PWideChar);
      1: (ulPropid: UINT);
  end;

  PDBID = ^DBID;
  DBID = record
    uGuid: DBIDGUID;
    eKind: DBKIND;
    uName: DBIDNAME;
  end;
  PDBIDArray = ^TDBIDArray;
  TDBIDArray = array[0..MAXBOUND] of DBID;

  PDBColumnInfo = ^TDBColumnInfo;
  TDBColumnInfo = record
    pwszName: PWideChar;
    pTypeInfo: ITypeInfo;
    iOrdinal: DBORDINAL;
    dwFlags: DBCOLUMNFLAGS;
    ulColumnSize: PtrUInt;
    wType: DBTYPE;
    bPrecision: Byte;
    bScale: Byte;
    columnid: DBID;
  end;

  DBSOURCETYPE = DWORD;
  PDBSOURCETYPE = ^DBSOURCETYPE;

  TDBProp = record
    dwPropertyID: DBPROPID;
    dwOptions: DBPROPOPTIONS;
    dwStatus: DBPROPSTATUS;
    colid: DBID;
    vValue: OleVariant;
  end;
  PDBPropArray = ^TDBPropArray;
  TDBPropArray = array[0..MAXBOUND] of TDBProp;

  TDBPropSet = record
    rgProperties: PDBPropArray;
    cProperties: UINT;
    guidPropertySet: TGUID;
  end;
  PDBPropSet = ^TDBPropSet;

  PDBPropSetArray = ^TDBPropSetArray;
  TDBPropSetArray = array[0..MAXBOUND] of TDBPropSet;
  TDBSchemaRec = record
    SchemaGuid: TGuid;
    SupportedRestrictions: Integer;
  end;

  TSSPARAMPROPS = record
    iOrdinal: DBORDINAL;
    cPropertySets: ULONG;
    rgPropertySets: PDBPropSet;
  end;
  PSSPARAMPROPS = ^TSSPARAMPROPS;
  PSSPARAMPROPSArray = ^TSSPARAMPROPSArray;
  TSSPARAMPROPSArray = array[0..MAXBOUND] of TSSPARAMPROPS;
  TSSPARAMPROPSDynArray = array of TSSPARAMPROPS;

  PDBParamInfo = ^TDBParamInfo;
  DBPARAMINFO = record
    dwFlags: UINT;
    iOrdinal: DBORDINAL;
    pwszName: PWideChar;
    pTypeInfo: ITypeInfo;
    ulParamSize: DBLENGTH;
    wType: DBTYPE;
    bPrecision: Byte;
    bScale: Byte;
  end;
  TDBParamInfo = DBPARAMINFO;

  PUintArray = ^TUintArray;
  TUintArray = array[0..MAXBOUND] of UINT;
  TUintDynArray = array of UINT;

  PDBParamBindInfo = ^TDBParamBindInfo;
  DBPARAMBINDINFO = record
    pwszDataSourceType: PWideChar;
    pwszName: PWideChar;
    ulParamSize: DBLENGTH;
    dwFlags: DBPARAMFLAGS;
    bPrecision: Byte;
    bScale: Byte;
  end;
  TDBParamBindInfo = DBPARAMBINDINFO;
  PDBParamBindInfoArray = ^TDBParamBindInfoArray;
  TDBParamBindInfoArray = array[0..MAXBOUND] of TDBParamBindInfo;
  TDBParamBindInfoDynArray = array of TDBParamBindInfo;

  PSSERRORINFO = ^SSERRORINFO;
  SSERRORINFO = record
    pwszMessage: PWideChar;
    pwszServer: PWideChar;
    pwszProcedure: PWideChar;
    lNative: cardinal;
    bState: byte;
    bClass: byte;
    wLineNumber: word;
  end;

{$ifndef CPU64}
  {$A-} // packed records
{$endif}


{ ************ Native OleDB Interfaces }

type
  /// initialize and uninitialize OleDB data source objects and enumerators
  IDBInitialize = interface(IUnknown)
    ['{0C733A8B-2A1C-11CE-ADE5-00AA0044773D}']
    function Initialize: HRESULT; stdcall;
    function Uninitialize: HRESULT; stdcall;
  end;

  /// create an OleDB data source object using a connection string
  IDataInitialize = interface(IUnknown)
    ['{2206CCB1-19C1-11D1-89E0-00C04FD7A829}']
    function GetDataSource(const pUnkOuter: IUnknown; dwClsCtx: DWORD;
      pwszInitializationString: POleStr; const riid: TIID;
      var DataSource: IUnknown): HRESULT; stdcall;
    function GetInitializationString(const DataSource: IUnknown;
      fIncludePassword: Boolean; out pwszInitString: POleStr): HRESULT; stdcall;
    function CreateDBInstance(const clsidProvider: TGUID;
      const pUnkOuter: IUnknown; dwClsCtx: DWORD; pwszReserved: POleStr;
      riid: TIID; var DataSource: IUnknown): HRESULT; stdcall;
    function CreateDBInstanceEx(const clsidProvider: TGUID;
      const pUnkOuter: IUnknown; dwClsCtx: DWORD; pwszReserved: POleStr;
      pServerInfo: PCoServerInfo; cmq: ULONG; rgmqResults: PMultiQI): HRESULT; stdcall;
    function LoadStringFromStorage(pwszFileName: POleStr;
      out pwszInitializationString: POleStr): HRESULT; stdcall;
    function WriteStringToStorage(pwszFileName, pwszInitializationString: POleStr;
      dwCreationDisposition: DWORD): HRESULT; stdcall;
  end;

  /// obtain a new session to a given OleDB data source
  IDBCreateSession = interface(IUnknown)
    ['{0C733A5D-2A1C-11CE-ADE5-00AA0044773D}']
    function CreateSession(const punkOuter: IUnknown; const riid: TGUID;
      out ppDBSession: IUnknown): HRESULT; stdcall;
  end;

  /// commit, abort, and obtain status information about OleDB transactions
  ITransaction = interface(IUnknown)
    ['{0FB15084-AF41-11CE-BD2B-204C4F4F5020}']
    function Commit(fRetaining: BOOL; grfTC: UINT; grfRM: UINT): HRESULT; stdcall;
    function Abort(pboidReason: PBOID; fRetaining: BOOL; fAsync: BOOL): HRESULT; stdcall;
    function GetTransactionInfo(out pinfo: TXactTransInfo): HRESULT; stdcall;
  end;

  /// gets and sets a suite of options associated with an OleDB transaction
  ITransactionOptions = interface(IUnknown)
    ['{3A6AD9E0-23B9-11CF-AD60-00AA00A74CCD}']
    function SetOptions(var pOptions: TXactOpt): HRESULT; stdcall;
    function GetOptions(var pOptions: TXactOpt): HRESULT; stdcall;
  end;
  /// optional interface on OleDB sessions, used to start, commit, and abort
  // transactions on the session
  ITransactionLocal = interface(ITransaction)
    ['{0C733A5F-2A1C-11CE-ADE5-00AA0044773D}']
    function GetOptionsObject(out ppOptions: ITransactionOptions): HRESULT; stdcall;
    function StartTransaction(isoLevel: Integer; isoFlags: UINT;
      const pOtherOptions: ITransactionOptions; pulTransactionLevel: PUINT): HRESULT; stdcall;
  end;

  /// provide methods to execute commands
  ICommand = interface(IUnknown)
    ['{0C733A63-2A1C-11CE-ADE5-00AA0044773D}']
    function Cancel: HRESULT; stdcall;
    function Execute(const punkOuter: IUnknown; const riid: TGUID; var pParams: TDBParams;
      pcRowsAffected: PInteger; ppRowset: PIUnknown): HRESULT; stdcall;
    function GetDBSession(const riid: TGUID; out ppSession: IUnknown): HRESULT; stdcall;
  end;
  /// methods to access the ICommand text to be executed
  ICommandText = interface(ICommand)
    ['{0C733A27-2A1C-11CE-ADE5-00AA0044773D}']
    function GetCommandText(var pguidDialect: TGUID;
      out ppwszCommand: PWideChar): HRESULT; stdcall;
    function SetCommandText(const guidDialect: TGUID;
      pwszCommand: PWideChar): HRESULT; stdcall;
  end;

  ICommandWithParameters = interface(IUnknown)
    ['{0C733A64-2A1C-11CE-ADE5-00AA0044773D}']
    function GetParameterInfo(var pcParams: UINT; out prgParamInfo: PDBPARAMINFO;
      ppNamesBuffer: PPOleStr): HRESULT; stdcall;
    function MapParameterNames(cParamNames: DB_UPARAMS; rgParamNames: POleStrList;
      rgParamOrdinals: PPtrUIntArray): HRESULT; stdcall;
    function SetParameterInfo(cParams: DB_UPARAMS; rgParamOrdinals: PPtrUIntArray;
      rgParamBindInfo: PDBParamBindInfoArray): HRESULT; stdcall;
  end;

  ISSCommandWithParameters = interface(ICommandWithParameters)
    ['{EEC30162-6087-467C-B995-7C523CE96561}']
    function GetParameterProperties(var pcParams: PtrUInt; var prgParamProperties: PSSPARAMPROPS): HRESULT; stdcall;
    function SetParameterProperties (cParams: PtrUInt; prgParamProperties: PSSPARAMPROPS): HRESULT; stdcall;
  end;

  /// provides methods for fetching rows sequentially, getting the data from
  // those rows, and managing rows
  IRowset = interface(IUnknown)
    ['{0C733A7C-2A1C-11CE-ADE5-00AA0044773D}']
    /// Adds a reference count to an existing row handle
    function AddRefRows(cRows: PtrUInt; rghRows: PPtrUIntArray;
      rgRefCounts, rgRowStatus: PCardinalArray): HRESULT; stdcall;
    /// Retrieves data from the rowset's copy of the row
    function GetData(HROW: HROW; HACCESSOR: HACCESSOR; pData: Pointer): HRESULT; stdcall;
    /// Fetches rows sequentially, remembering the previous position
    // - this method has been modified from original OleDB.pas to allow direct
    // typecast of prghRows parameter to pointer(fRowStepHandles)
    function GetNextRows(hReserved: HCHAPTER; lRowsOffset: PtrInt; cRows: PtrInt;
      out pcRowsObtained: PtrUInt; var prghRows: pointer): HRESULT; stdcall;
    /// Releases rows
    function ReleaseRows(cRows: UINT; rghRows: PPtrUIntArray; rgRowOptions,
      rgRefCounts, rgRowStatus: PCardinalArray): HRESULT; stdcall;
    /// Repositions the next fetch position to its initial position
    // - that is, its position when the rowset was first created
    function RestartPosition(hReserved: HCHAPTER): HRESULT; stdcall;
  end;

  IOpenRowset = interface(IUnknown)
    ['{0C733A69-2A1C-11CE-ADE5-00AA0044773D}']
    function OpenRowset(const punkOuter: IUnknown; pTableID: PDBID; pIndexID: PDBID;
      const riid: TGUID; cPropertySets: UINT; rgPropertySets: PDBPropSetArray;
      ppRowset: PIUnknown): HRESULT; stdcall;
  end;

  IMultipleResults = interface(IUnknown)
  ['{0c733a8c-2a1c-11ce-ade5-00aa0044773d}']
    function GetResult(const pUnkOuter: IUnknown; lResultFlag: DBRESULTFLAG;
    const riid: TIID; pcRowsAffected: PInteger;ppRowset: PIUnknown): HRESULT; stdcall;
  end;

  /// interface used to retrieve enhanced custom error information
  IErrorRecords = interface(IUnknown)
    ['{0c733a67-2a1c-11ce-ade5-00aa0044773d}']
    function AddErrorRecord(pErrorInfo: PErrorInfo; dwLookupID: UINT;
      pDispParams: pointer; const punkCustomError: IUnknown;
      dwDynamicErrorID: UINT): HRESULT; stdcall;
    function GetBasicErrorInfo(ulRecordNum: UINT;
      pErrorInfo: PErrorInfo): HRESULT; stdcall;
    function GetCustomErrorObject(ulRecordNum: UINT;
      const riid: TGUID; var ppObject: IUnknown): HRESULT; stdcall;
    function GetErrorInfo(ulRecordNum: UINT; lcid: LCID;
      var ppErrorInfo: IErrorInfo): HRESULT; stdcall;
    function GetErrorParameters(ulRecordNum: UINT;
      pDispParams: pointer): HRESULT; stdcall;
    function GetRecordCount(var pcRecords: UINT): HRESULT; stdcall;
  end;

  /// used on an OleDB session to obtain a new command
  IDBCreateCommand = interface(IUnknown)
    ['{0C733A1D-2A1C-11CE-ADE5-00AA0044773D}']
    function CreateCommand(const punkOuter: IUnknown; const riid: TGUID;
      out ppCommand: ICommand): HRESULT; stdcall;
  end;

  /// provides methods for accessor management, to access OleDB data
  // - An accessor is a data structure created by the consumer that describes
  // how row or parameter data from the data store is to be laid out in the
  // consumer's data buffer.
  // - For each column in a row (or parameter in a set of parameters), the
  // accessor contains a binding. A binding is a DBBinding data structure that
  // holds information about a column or parameter value, such as its ordinal
  // value, data type, and destination in the consumer's buffer.
  IAccessor = interface(IUnknown)
    ['{0C733A8C-2A1C-11CE-ADE5-00AA0044773D}']
    function AddRefAccessor(HACCESSOR: HACCESSOR; pcRefCount: PUINT): HRESULT; stdcall;
    function CreateAccessor(dwAccessorFlags: UINT; cBindings: PtrUInt; rgBindings: PDBBindingArray;
      cbRowSize: PtrUInt; var phAccessor: HACCESSOR; rgStatus: PCardinalArray): HRESULT; stdcall;
    function GetBindings(HACCESSOR: HACCESSOR; pdwAccessorFlags: PUINT; var pcBindings: PtrUInt;
      out prgBindings: PDBBinding): HRESULT; stdcall;
    function ReleaseAccessor(HACCESSOR: HACCESSOR; pcRefCount: PUINT): HRESULT; stdcall;
  end;

  /// expose information about columns of an OleDB rowset or prepared command
  IColumnsInfo = interface(IUnknown)
    ['{0C733A11-2A1C-11CE-ADE5-00AA0044773D}']
    function GetColumnInfo(var pcColumns: PtrUInt; out prgInfo: PDBColumnInfo;
      out ppStringsBuffer: PWideChar): HRESULT; stdcall;
    function MapColumnIDs(cColumnIDs: PtrUInt; rgColumnIDs: PDBIDArray;
      rgColumns: PPtrUIntArray): HRESULT; stdcall;
  end;

  /// allows the display of the data link dialog boxes programmatically
  IDBPromptInitialize = interface(IUnknown)
    ['{2206CCB0-19C1-11D1-89E0-00C04FD7A829}']
    function PromptDataSource(const pUnkOuter: IUnknown; hWndParent: HWND;
      dwPromptOptions: UINT; cSourceTypeFilter: ULONG;
      rgSourceTypeFilter: PDBSOURCETYPE; pszProviderFilter: POleStr;
      const riid: TIID; var DataSource: IUnknown): HRESULT; stdcall;
    function PromptFileName(hWndParent: HWND; dwPromptOptions: UINT;
      pwszInitialDirectory, pwszInitialFile: POleStr;
      var ppwszSelectedFile: POleStr): HRESULT; stdcall;
  end;

  /// used to retrieve the database metadata (e.g. tables and fields layout)
  IDBSchemaRowset = interface(IUnknown)
    ['{0c733a7b-2a1c-11ce-ade5-00aa0044773d}']
    function GetRowset(pUnkOuter: IUnknown; const rguidSchema: TGUID;
      cRestrictions: Integer; rgRestrictions: pointer;
      const riid: TIID; cPropertySets: Integer; rgPropertySets: PDBPROPSET;
      var ppRowset: IRowset): HRESULT; stdcall;
    function GetSchemas(var pcSchemas: Integer; var prgSchemas: PGUID;
      var prgRestrictionSupport: PInteger): HRESULT; stdcall;
  end;

  /// used to access the low-level database information
  _Catalog = interface(IDispatch)
    ['{00000603-0000-0010-8000-00AA006D2EA4}']
    function Get_Tables: OleVariant; safecall;
    function Get_ActiveConnection: OleVariant; safecall;
    procedure Set_ActiveConnection(pVal: OleVariant); safecall;
    procedure _Set_ActiveConnection(const pVal: IDispatch); safecall;
    function Get_Procedures: OleVariant; safecall;
    function Get_Views: OleVariant; safecall;
    function Get_Groups: OleVariant; safecall;
    function Get_Users: OleVariant; safecall;
    function Create(const ConnectString: WideString): OleVariant; safecall;
    // warning: the following method won't work if you use SynFastWideString.pas
    // but we don't call it in this unit, you we can stay cool for now :)
    function GetObjectOwner(const ObjectName: WideString; ObjectType: OleVariant;
                            ObjectTypeId: OleVariant): WideString; safecall;
    procedure SetObjectOwner(const ObjectName: WideString; ObjectType: OleVariant;
                             const UserName: WideString; ObjectTypeId: OleVariant); safecall;
  end;

  /// used to retrieve enhanced Microsoft SQL Server error information
  ISQLServerErrorInfo = interface(IUnknown)
    ['{5CF4CA12-EF21-11d0-97E7-00C04FC2AD98}']
    function GetErrorInfo(out ppErrorInfo: PSSERRORINFO;
                          out Error: PWideChar): HRESULT; stdcall;
  end;


{ ************ Low-Level OleDB Custom RowSet Processing }

type
  /// our custom IRowSet implementation
  TBaseAggregatingRowset = class(TObject, IUnknown, IRowset)
  private
   fcTotalRows: UINT;
   // Defining as an array because in general there can be as many accessors as necessary
   // the reading rules from the provider for such scenarios are describe in the Books online
   fhAccessor: HACCESSORDynArray;
  protected
   fidxRow: UINT;
   fUnkInnerSQLNCLIRowset: IUnknown;
   // Save the handle of the accessor that we create, the indexing is 0 based
   procedure SetAccessorHandle(idxAccessor: ULONG; hAccessor: HACCESSOR );
  public
    constructor Create(cTotalRows: UINT);
    function SetupAccessors(pIAccessorTVP: IAccessor): HRESULT; virtual; abstract;
    destructor Destroy; override;
    {$ifdef FPC}
    function QueryInterface({$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif}
      iid : tguid; out obj) : longint;{$ifndef WINDOWS}cdecl{$else}stdcall{$endif};
    function _AddRef : longint;{$ifndef WINDOWS}cdecl{$else}stdcall{$endif};
    function _Release : longint;{$ifndef WINDOWS}cdecl{$else}stdcall{$endif};
    {$else}
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    {$endif FPC}
    /// Adds a reference count to an existing row handle
    function AddRefRows(cRows: PtrUInt; rghRows: PPtrUIntArray;
      rgRefCounts, rgRowStatus: PCardinalArray): HRESULT; stdcall;
    /// Retrieves data from the rowset's copy of the row
    function GetData(HROW: HROW; HACCESSOR: HACCESSOR; pData: Pointer): HRESULT; virtual; stdcall;
    /// Fetches rows sequentially, remembering the previous position
    // - this method has been modified from original OleDB.pas to allow direct
    // typecast of prghRows parameter to pointer(fRowStepHandles)
    function GetNextRows(hReserved: HCHAPTER; lRowsOffset: PtrInt; cRows: PtrInt;
      out pcRowsObtained: PtrUInt; var prghRows: pointer): HRESULT; stdcall;
    /// Releases rows
    function ReleaseRows(cRows: UINT; rghRows: PPtrUIntArray; rgRowOptions,
      rgRefCounts, rgRowStatus: PCardinalArray): HRESULT; stdcall;
    /// Repositions the next fetch position to its initial position
    // - that is, its position when the rowset was first created
    function RestartPosition(hReserved: HCHAPTER): HRESULT; stdcall;
  end;

  TIDListRec = record
    IDLen: PtrUInt;
    IDST: DBSTATUS;
    IDVal: int64;
    StrVal: PWideChar;
  end;
  PIDListRec = ^TIDListRec;

  TIDListRowset = class(TBaseAggregatingRowset)
  private
    farr: TRawUTF8DynArray;
    fType: TSQLDBFieldType;
  public
    constructor Create(arr: TRawUTF8DynArray; aType: TSQLDBFieldType);

    function Initialize(pIOpenRowset: IOpenRowset): HRESULT;
    function GetData(HROW: HROW; HACCESSOR: HACCESSOR; pData: Pointer): HRESULT; override; stdcall;
    function SetupAccessors(pIAccessorIDList: IAccessor): HRESULT; override;

    procedure FillRowData(pCurrentRec:PIDListRec);
    procedure FillBindingsAndSetupRowBuffer(pBindingsList: PDBBindingArray);
  end;


{ ************ OleDB High-Level Access }

type
  /// generic Exception type, generated for OleDB connection
  EOleDBException = class(ESQLDBException);


/// check from the file beginning if sounds like a valid Jet / MSAccess file
function IsJetFile(const FileName: TFileName): boolean;

/// low-level guess of our SQL Field Type from the OleDB numerical type value
function OleDBColumnToFieldType(wType: DBTYPE; bScale: byte): TSQLDBFieldType;

{$endif MSWINDOWS}


implementation

{$ifdef MSWINDOWS} // compiles as void unit for non-Windows - allow Lazarus package


{ ************ Low-Level OleDB Custom RowSet Processing }

{ TBaseAggregatingRowset }

function TBaseAggregatingRowset.AddRefRows(cRows: PtrUInt;
  rghRows: PPtrUIntArray; rgRefCounts, rgRowStatus: PCardinalArray): HResult;
begin
  // Never gets called, so we can return E_NOTIMPL
  result := E_NOTIMPL;
end;

constructor TBaseAggregatingRowset.Create(cTotalRows: UINT);
begin
  fidxRow := 1;
  fcTotalRows := cTotalRows;
  fUnkInnerSQLNCLIRowset := nil;
  SetLength(fhAccessor, 1);
  fhAccessor[0] := 0;
  inherited Create;
end;

destructor TBaseAggregatingRowset.Destroy;
var
  pIAccessor: IAccessor;
begin
  if fhAccessor[0] <> 0 then
  begin
    pIAccessor := nil;
    OleCheck(fUnkInnerSQLNCLIRowset.QueryInterface(IID_IAccessor, pIAccessor));
    OleCheck(pIAccessor.ReleaseAccessor(fhAccessor[0], nil));
  end;
  inherited;
end;

function TBaseAggregatingRowset.GetData(HROW: HROW; HACCESSOR: HACCESSOR; pData:
  Pointer): HResult;
begin
  result := S_OK;
end;

function TBaseAggregatingRowset.GetNextRows(hReserved: HCHAPTER;
  lRowsOffset, cRows: PtrInt; out pcRowsObtained: PtrUInt; var prghRows: pointer): HResult;
begin
  assert(lRowsOffset = 0);
  assert(cRows = 1);
  assert(Assigned(prghRows));
  pcRowsObtained := 0;
  // If we still have rows to give back
  if fidxRow <= fcTotalRows then
  begin
    pcRowsObtained := 1;
     // For us, row handle is simply an index in our row list
    PHROW(prghRows)^ := fidxRow;
    Inc(fidxRow);
    result := S_OK;
  end
  else
    result := DB_S_ENDOFROWSET;
end;

{$ifdef FPC}
function TBaseAggregatingRowset.QueryInterface(
  {$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif} iid : tguid;out obj) : longint;
  {$ifndef WINDOWS}cdecl{$else}stdcall{$endif};
{$else}
function TBaseAggregatingRowset.QueryInterface(const IID: TGUID; out Obj): HResult;
{$endif FPC}
begin
  if IsEqualGUID(@IID, @IID_IUnknown) then
    IUnknown(Obj) := Self
  else
  begin
    if not Assigned(fUnkInnerSQLNCLIRowset) then
    begin
      Pointer(Obj) := nil;
      result := E_NOINTERFACE;
      Exit;
    end;
    if IsEqualGUID(@IID, @IID_IRowset) then
      IUnknown(Obj) := self
    else
    begin
      result := fUnkInnerSQLNCLIRowset.QueryInterface(IID, Obj);
      exit;
    end;
  end;
  IUnknown(Obj)._AddRef;
  result := S_OK;
end;

function TBaseAggregatingRowset.ReleaseRows(cRows: UINT; rghRows: PPtrUIntArray;
  rgRowOptions, rgRefCounts, rgRowStatus: PCardinalArray): HResult;
begin
  assert(cRows = 1);
  assert(rghRows[0] <= fcTotalRows);
  result := S_OK;
end;

function TBaseAggregatingRowset.RestartPosition(hReserved: HCHAPTER): HResult;
begin
  fidxRow := 1;
  result := S_OK;
end;

procedure TBaseAggregatingRowset.SetAccessorHandle(idxAccessor: ULONG; hAccessor:
  hAccessor);
begin
  fhAccessor[idxAccessor] := hAccessor;
end;

{$ifdef FPC}
function TBaseAggregatingRowset._AddRef: longint;
  {$ifndef WINDOWS} cdecl {$else} stdcall {$endif};
{$else}
function TBaseAggregatingRowset._AddRef: Integer;
{$endif FPC}
begin
  result := 1;
end;

{$ifdef FPC}
function TBaseAggregatingRowset._Release: longint;
  {$ifndef WINDOWS} cdecl {$else} stdcall {$endif};
{$else}
function TBaseAggregatingRowset._Release: Integer;
{$endif FPC}
begin
  result := 1;
end;


{ TIDListRowset }

constructor TIDListRowset.Create(arr: TRawUTF8DynArray; aType: TSQLDBFieldType);
begin
  farr := arr;
  fType := aType;
  inherited Create(Length(farr));
end;

procedure TIDListRowset.FillBindingsAndSetupRowBuffer(pBindingsList: PDBBindingArray);
var
  i: Integer;
  rec: TIDListRec; // pseudo record to compute offset within TIDListRec
begin
  FillcharFast(rec, sizeof(rec), 0); // makes Win64 compiler happy
  pBindingsList[0].pTypeInfo := nil;
  pBindingsList[0].pObject := nil;
  pBindingsList[0].pBindExt := nil;
  pBindingsList[0].eParamIO := DBPARAMIO_NOTPARAM;
  pBindingsList[0].iOrdinal := 1;
  pBindingsList[0].dwPart := DBPART_VALUE or DBPART_STATUS or DBPART_LENGTH;
  pBindingsList[0].dwMemOwner := DBMEMOWNER_CLIENTOWNED;
  pBindingsList[0].dwFlags := 0;
  case fType of
    ftInt64:
      begin
        pBindingsList[0].cbMaxLen := sizeof(int64);
        pBindingsList[0].obValue := PAnsiChar(@rec.IDVal) - pointer(@rec);
        pBindingsList[0].wType := DBTYPE_I8;
      end;
    ftUTF8:
      begin
        pBindingsList[0].cbMaxLen := sizeof(PWideChar); //Check bind ''
        for i := 0 to Length(farr) - 1 do
          if Length(farr[i]) * SizeOf(WideChar) > Integer(pBindingsList[0].cbMaxLen) then
            pBindingsList[0].cbMaxLen := Length(farr[i]) * SizeOf(WideChar);
        pBindingsList[0].obValue := PAnsiChar(@rec.StrVal) - pointer(@rec);
        pBindingsList[0].wType := DBTYPE_BSTR
      end;
  end;
  pBindingsList[0].obStatus := PAnsiChar(@rec.IDST) - pointer(@rec);
  pBindingsList[0].obLength := PAnsiChar(@rec.IDLen) - pointer(@rec);
end;

procedure TIDListRowset.FillRowData(pCurrentRec: PIDListRec);
var
  curInd: Integer;
  tmp: RawUTF8;
begin
  curInd := fidxRow - 2;
  if farr[curInd] = 'null' then
  begin
    pCurrentRec.IDST := ord(stIsNull);
  end
  else
  begin
    pCurrentRec.IDST := 0;
    case fType of
      ftInt64:
        begin
          SetInt64(pointer(farr[curInd]), pCurrentRec.IDVal);
          pCurrentRec.IDLen := SizeOf(Int64);
        end;
      ftUTF8:
        begin
          tmp := UnQuoteSQLString(farr[curInd]);
          pCurrentRec.IDLen := (Length(tmp) + 1) * SizeOf(WideChar);
          pCurrentRec.StrVal := Pointer(UTF8ToWideString(tmp));
        end
    else
      raise EOleDBException.Create('Unsupported array parameter type');
    end;
  end;
end;

function TIDListRowset.GetData(HROW: HROW; HACCESSOR: HACCESSOR;
  pData: Pointer): HResult;
var
  currentRec: PIDListRec;
begin
  inherited GetData(HROW, HACCESSOR, pData);
  currentRec := pData;
  FillRowData(currentRec);
  result := S_OK;
end;

function TIDListRowset.Initialize(pIOpenRowset: IOpenRowset): HRESULT;
var
  dbidID: DBID;
begin
  dbidID.eKind := DBKIND_GUID_NAME;
  dbidID.uGuid.guid := CLSID_ROWSET_TVP;
  case fType of
    ftInt64:
      dbidID.uName.pwszName := pointer(IDList_type);
    ftUTF8:
      dbidID.uName.pwszName := pointer(StrList_type);
  end;
  OleCheck(pIOpenRowset.OpenRowset(self, @dbidID, nil, IID_IUnknown,
    0, nil, @fUnkInnerSQLNCLIRowset));
  SetupAccessors(self as IAccessor);
  result := S_OK;
end;

function TIDListRowset.SetupAccessors(pIAccessorIDList: IAccessor): HRESULT;
var
  binding: array[0..0] of TDBBinding;
  bindStatus: array[0..0] of DWORD;
  hAccessorIDList: HACCESSOR;
begin
  FillBindingsAndSetupRowBuffer(@binding);
  bindStatus[0] := 0;
  OleCheck(pIAccessorIDList.CreateAccessor(DBACCESSOR_ROWDATA, 1, @binding,
    SizeOf(TIDListRec), hAccessorIDList, @bindStatus));
  SetAccessorHandle(0, hAccessorIDList);
  result := S_OK;
end;


{ ************ OleDB High-Level Access }

function IsJetFile(const FileName: TFileName): boolean;
var
  F: THandle;
  Header: array[0..31] of AnsiChar;
begin
  F := FileOpen(FileName, fmOpenRead or fmShareDenyNone);
  if F = INVALID_HANDLE_VALUE then
    result := false
  else
  begin
    result := (FileRead(F, Header, sizeof(Header)) = SizeOf(Header)) and
      IdemPChar(@Header[4], 'STANDARD JET');
    FileClose(F);
  end;
end;

function OleDBColumnToFieldType(wType: DBTYPE; bScale: byte): TSQLDBFieldType;
begin
  case wType of
    DBTYPE_EMPTY:
      result := ftUnknown;
    DBTYPE_NULL:
      result := ftNull;
    DBTYPE_I1, DBTYPE_I2, DBTYPE_I4, DBTYPE_I8,
    DBTYPE_UI1, DBTYPE_UI2, DBTYPE_UI4, DBTYPE_UI8, DBTYPE_BOOL:
      result := ftInt64;
    DBTYPE_CY:
      result := ftCurrency;
    DBTYPE_R4, DBTYPE_R8:
      result := ftDouble;
    DBTYPE_DECIMAL, DBTYPE_NUMERIC, DBTYPE_VARNUMERIC:
      case bScale of // number of digits to the right of the decimal point
        0:
          result := ftInt64;
        1..4:
          result := ftCurrency;
      else
        result := ftDouble;
      end;
    DBTYPE_DATE, DBTYPE_DBDATE, DBTYPE_DBTIME, DBTYPE_FILETIME, DBTYPE_DBTIMESTAMP:
      result := ftDate;
    DBTYPE_BYTES, DBTYPE_UDT:
      result := ftBlob;
  else // all other types will be converted to text
    result := ftUtf8;
  end;
end;


{$endif MSWINDOWS}

end.

