/// Object-Relational-Mapping (ORM) Main Types and Classes
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.orm.core;

{
  *****************************************************************************

   Main Shared Types and Definitions for our RESTful ORM
    - ORM Specific TOrmPropInfoRtti Classes
    - IRestOrm IRestOrmServer IRestOrmClient Definitions
    - TOrm Definition
    - RecordRef Wrapper Definition
    - TOrmTable TOrmTableJson Definitions
    - TOrmMany Definition
    - TOrmVirtual Definitions
    - TOrmProperties Definitions
    - TOrmModel TOrmModelProperties Definitions
    - TRestCache Definition
    - TRestBatch TRestBatchLocked Definitions
    - TSynValidateRest TSynValidateUniqueField Definitions
    - TOrmAccessRights Definition
    - TOrm High-Level Parents

   This unit is not depending from mormot.rest.core so can be used as a pure
   ORM layer for your projects. IRestOrm is the main SOLID entry point.

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  variants,
  contnrs,
  {$ifndef FPC}
  typinfo, // for proper Delphi inlining
  {$endif FPC}
  mormot.core.base,
  mormot.core.os,
  mormot.core.buffers,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.variants,
  mormot.core.data,
  mormot.core.rtti,
  mormot.core.log,
  mormot.core.json,
  mormot.core.threads,
  mormot.core.search,
  {$ifdef HASGENERICS} // not supported on oldest compilers (e.g. < Delphi XE8)
  mormot.core.collections,
  {$endif HASGENERICS}
  mormot.crypt.secure, // for TSynUniqueIdentifierGenerator
  mormot.db.core,
  mormot.orm.base;




{ ************ TOrm TOrmModel TOrmTable IRestOrm Core Definitions }

// most types are defined as a single "type" statement due to classes coupling

type
  {$M+}
  { we expect RTTI information for the published properties of these
    forward definitions - due to internal coupling, all those classes are
    to be defined in a single "type" statement }
  TOrmTable = class;
  TOrm = class;
  TOrmMany = class;
  TOrmFts3 = class;
  TOrmRTree = class;
  TOrmProperties = class;
  TOrmModel = class;
  TOrmModelProperties = class;
  TRestOrmParent = class;
  TRestCache = class;
  TRestBatch = class;
  {$M-}

  /// class-reference type (metaclass) of TOrm
  TOrmClass = class of TOrm;

  /// pointer-level redirection of a TOrm metaclass
  // - used for efficient POrmClass(aRecord)^ access to the class info
  POrmClass = ^TOrmClass;

  /// class-reference type (metaclass) of a FTS3/FTS4/FTS5 virtual table
  // - either a TOrmFts3 TOrmFts4 or TOrmFts5 class
  TOrmFts3Class = class of TOrmFts3;

  /// class-reference type (metaclass) of RTREE virtual table
  // - either a TOrmRTree or a TOrmRTreeInteger
  TOrmRTreeClass = class of TOrmRTree;

  /// a dynamic array storing TOrm instances
  // - not used direcly, but as specialized T*ObjArray types
  TOrmObjArray = array of TOrm;

  /// a dynamic array used to store the TOrm classes in a Database Model
  TOrmClassDynArray = array of TOrmClass;

  /// a dynamic array of TOrmMany instances
  TOrmManyObjArray = array of TOrmMany;

  /// exception raised in case of TRestBatch problem
  EOrmBatchException = class(EOrmException);

  /// exception raised in case of wrong Model definition
  EModelException = class(EOrmException);


  { -------------------- ORM Specific TOrmPropInfoRtti Classes }

  /// information about a TOrm class TOrm property
  // - kind oftID, which are pointer(RecordID), not any true class instance
  // - will store the content just as an integer value
  // - will recognize any instance pre-allocated via Create*Joined() constructor
  TOrmPropInfoRttiID = class(TOrmPropInfoRttiInstance)
  public
    /// raise an exception if was created by Create*Joined() constructor
    procedure SetValue(Instance: TObject; Value: PUtf8Char; ValueLen: PtrInt;
      wasString: boolean); override;
    /// this method will recognize if the TOrm was allocated by
    // a Create*Joined() constructor: in this case, it will write the ID
    // of the nested property, and not the PtrInt() transtyped value
    procedure GetJsonValues(Instance: TObject; W: TJsonWriter); override;
  end;

  TOrmPropInfoRttiIDObjArray = array of TOrmPropInfoRttiID;

  /// information about a TID published property
  // - identified as a oftTID kind of property, optionally tied to a TOrm
  // class, via its custom type name, e.g.
  // ! TOrmClientID = type TID;  ->  TOrmClient class
  TOrmPropInfoRttiTID = class(TOrmPropInfoRttiRecordReference)
  protected
    fRecordClass: TOrmClass;
  public
    /// will setup the corresponding RecordClass property from the TID type name
    // - the TOrm type should have previously been registered to the
    // Rtti.RegisterClass list, e.g. in TOrmModel.Create, so that e.g.
    // 'TOrmClientID' type name will match TOrmClient
    // - in addition, the '...ToBeDeletedID' name pattern will set CascadeDelete
    constructor Create(aPropInfo: PRttiProp; aPropIndex: integer;
      aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions); override;
    /// the TOrm class associated to this TID
    // - is computed from its type name - for instance, if you define:
    // ! type
    // !   TOrmClientID = type TID;
    // !   TOrmOrder = class(TOrm)
    // !   ...
    // !   published OrderedBy: TOrmClientID read fOrderedBy write fOrderedBy;
    // !   ...
    // then this OrderedBy property will be tied to the TOrmClient class
    // of the corresponding model, and the field value will be reset to 0 when
    // the targetting record is deleted (emulating a ON DELETE SET DEFAULT)
    // - equals TOrm for plain TID field
    // - equals nil if T*ID type name doesn't match any registered class
    property RecordClass: TOrmClass
      read fRecordClass;
    /// TRUE if this oftTID type name follows the '...ToBeDeletedID' pattern
    // - e.g. 'TOrmClientToBeDeletedID' type name will match
    // TOrmClient and set CascadeDelete
    // - is computed from its type name - for instance, if you define:
    // ! type
    // !   TOrmClientToBeDeletedID = type TID;
    // !   TOrmOrder = class(TOrm)
    // !   ...
    // !   published OrderedBy: TOrmClientToBeDeletedID read fOrderedBy write fOrderedBy;
    // !   ...
    // then this OrderedBy property will be tied to the TOrmClient class
    // of the corresponding model, and the whole record will be deleted when
    // the targetting record is deleted (emulating a ON DELETE CASCADE)
    property CascadeDelete: boolean
      read fCascadeDelete;
  end;


  { -------------------- IRestOrm IRestOrmServer IRestOrmClient Definitions }

  {$ifdef HASGENERICS}
  TRestOrmGenerics = class;
  {$endif HASGENERICS}

  /// Object-Relational-Mapping calls for CRUD access to a database
  // - as implemented in TRest.ORM
  // - this is the main abstract entry point to all ORM process, to be used as
  // reference to the current TRest instance, without the REST/communication
  // particularities
  IRestOrm = interface
    ['{E3C24375-0E44-4C9F-B72C-89DBA8A8A9BD}']
    /// get the row count of a specified table
    // - returns -1 on error
    // - returns the row count of the table on success
    // - calls internally the "SELECT Count(*) FROM TableName;" SQL statement
    function TableRowCount(Table: TOrmClass): Int64;
    /// check if there is some data rows in a specified table
    // - calls internally a "SELECT RowID FROM TableName LIMIT 1" SQL statement,
    // which is much faster than testing if "SELECT count(*)" equals 0 - see
    // @http://stackoverflow.com/questions/8988915
    function TableHasRows(Table: TOrmClass): boolean;
    /// search for the last inserted ID in a specified table
    // - returns -1 on error
    // - will execute by default "SELECT max(rowid) FROM TableName"
    function TableMaxID(Table: TOrmClass): TID;
    /// check if a given ID do exist for a given table
    function MemberExists(Table: TOrmClass; ID: TID): boolean;
    /// get the UTF-8 encoded value of an unique field with a Where Clause
    // - example of use - including inlined parameters via :(...):
    // ! aClient.OneFieldValue(TOrm, 'Name', 'ID=:(23):')
    // you should better call the corresponding overloaded method as such:
    // ! aClient.OneFieldValue(TOrm, 'Name', 'ID=?', [aID])
    // which is the same as calling:
    // ! aClient.OneFieldValue(TOrm, 'Name', FormatUtf8('ID=?', [], [23]))
    // - call internally ExecuteList() to get the value
    function OneFieldValue(Table: TOrmClass;
      const FieldName, WhereClause: RawUtf8): RawUtf8; overload;
    /// get the Int64 value of an unique field with a Where Clause
    // - call internally ExecuteList() to get the value
    function OneFieldValueInt64(Table: TOrmClass;
      const FieldName, WhereClause: RawUtf8; Default: Int64 = 0): Int64;
    /// get the UTF-8 encoded value of an unique field with a Where Clause
    // - this overloaded function will call FormatUtf8 to create the Where Clause
    // from supplied parameters, binding all '?' chars with Args[] values
    // - example of use:
    // ! aClient.OneFieldValue(TOrm, 'Name', 'ID=?', [aID])
    // - call internally ExecuteList() to get the value
    // - note that this method prototype changed with revision 1.17 of the
    // framework: array of const used to be Args and '%' in the FormatSqlWhere
    // statement, whereas it now expects bound parameters as '?'
    function OneFieldValue(Table: TOrmClass; const FieldName: RawUtf8;
      const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const): RawUtf8; overload;
    /// get the UTF-8 encoded value of an unique field with a Where Clause
    // - this overloaded function will call FormatUtf8 to create the Where Clause
    // from supplied parameters, replacing all '%' chars with Args[], and all '?'
    // chars with Bounds[] (inlining them with :(...): and auto-quoting strings)
    // - example of use:
    // ! OneFieldValue(TOrm,'Name', '%=?', ['ID'], [aID])
    // - call internally ExecuteList() to get the value
    function OneFieldValue(Table: TOrmClass; const FieldName: RawUtf8;
      const WhereClauseFmt: RawUtf8; const Args, Bounds: array of const): RawUtf8; overload;
    /// get one integer value of an unique field with a Where Clause
    // - this overloaded function will return the field value as integer
    function OneFieldValue(Table: TOrmClass; const FieldName: RawUtf8;
      const WhereClauseFmt: RawUtf8; const Args, Bounds: array of const;
      out Data: Int64): boolean; overload;
    /// get the UTF-8 encoded value of an unique field from its ID
    // - example of use: OneFieldValue(TOrm,'Name',23)
    // - call internally ExecuteList() to get the value
    function OneFieldValue(Table: TOrmClass; const FieldName: RawUtf8;
      WhereID: TID): RawUtf8; overload;
    /// get the UTF-8 encoded value of some fields with a Where Clause
    // - example of use: MultiFieldValue(TOrm,['Name'],Name,'ID=:(23):')
    // (using inlined parameters via :(...): is always a good idea)
    // - FieldValue[] will have the same length as FieldName[]
    // - return true on success, false on SQL error or no result
    // - call internally ExecuteList() to get the list
    function MultiFieldValue(Table: TOrmClass;
      const FieldName: array of RawUtf8; var FieldValue: array of RawUtf8;
      const WhereClause: RawUtf8): boolean; overload;
    /// get the UTF-8 encoded value of some fields from its ID
    // - example of use: MultiFieldValue(TOrm,['Name'],Name,23)
    // - FieldValue[] will have the same length as FieldName[]
    // - return true on success, false on SQL error or no result
    // - call internally ExecuteList() to get the list
    function MultiFieldValue(Table: TOrmClass;
      const FieldName: array of RawUtf8; var FieldValue: array of RawUtf8;
      WhereID: TID): boolean; overload;
    /// get the UTF-8 encoded values of an unique field with a Where Clause
    // - example of use: OneFieldValue(TOrm,'FirstName','Name=:("Smith"):',Data)
    // (using inlined parameters via :(...): is always a good idea)
    // - leave WhereClause void to get all records
    // - call internally ExecuteList() to get the list
    // - returns TRUE on success, FALSE if no data was retrieved
    function OneFieldValues(Table: TOrmClass; const FieldName: RawUtf8;
      const WhereClause: RawUtf8; out Data: TRawUtf8DynArray): boolean; overload;
    /// get the integer value of an unique field with a Where Clause
    // - example of use: OneFieldValue(TOrmPeople,'ID','Name=:("Smith"):',Data)
    // (using inlined parameters via :(...): is always a good idea)
    // - leave WhereClause void to get all records
    // - call internally ExecuteList() to get the list
    function OneFieldValues(Table: TOrmClass; const FieldName: RawUtf8;
      const WhereClause: RawUtf8; var Data: TInt64DynArray;
      SQL: PRawUtf8 = nil): boolean; overload;
    /// get the CSV-encoded UTF-8 encoded values of an unique field with a Where Clause
    // - example of use: OneFieldValue(TOrm,'FirstName','Name=:("Smith")',Data)
    // (using inlined parameters via :(...): is always a good idea)
    // - leave WhereClause void to get all records
    // - call internally ExecuteList() to get the list
    // - using inlined parameters via :(...): in WhereClause is always a good idea
    function OneFieldValues(Table: TOrmClass; const FieldName: RawUtf8;
      const WhereClause: RawUtf8 = ''; const Separator: RawUtf8 = ','): RawUtf8; overload;
    /// get the string-encoded values of an unique field into some TStrings
    // - Items[] will be filled with string-encoded values of the given field)
    // - Objects[] will be filled with pointer(ID)
    // - call internally ExecuteList() to get the list
    // - returns TRUE on success, FALSE if no data was retrieved
    // - if IDToIndex is set, its value will be replaced with the index in
    // Strings.Objects[] where ID=IDToIndex^
    // - using inlined parameters via :(...): in WhereClause is always a good idea
    function OneFieldValues(Table: TOrmClass; const FieldName, WhereClause:
      RawUtf8; Strings: TStrings; IDToIndex: PID = nil): boolean; overload;
    /// Execute directly a SQL statement, returning a TOrmTable list of resutls
    // - return a TOrmTableJson instance on success, nil on failure
    // - FieldNames can be the CSV list of field names to be retrieved
    // - if FieldNames is '', will get all simple fields, excluding BLOBs
    // - if FieldNames is '*', will get ALL fields, including ID and BLOBs
    // - call internally ExecuteList() to get the list
    // - using inlined parameters via :(...): in WhereClause is always a good idea
    function MultiFieldValues(Table: TOrmClass; const FieldNames: RawUtf8;
      const WhereClause: RawUtf8 = ''): TOrmTable; overload;
    /// Execute directly a SQL statement, returning a TOrmTable list of resutls
    // - return a TOrmTableJson instance on success, nil on failure
    // - FieldNames can be the CSV list of field names to be retrieved
    // - if FieldNames is '', will get all simple fields, excluding BLOBs
    // - if FieldNames is '*', will get ALL fields, including ID and BLOBs
    // - this overloaded function will call FormatUtf8 to create the Where Clause
    // from supplied parameters, binding all '?' chars with Args[] values
    // - example of use:
    // ! aList := aClient.MultiFieldValues(
    // !   TOrm, 'Name,FirstName', 'Salary>=?', [aMinSalary]);
    // - call overloaded MultiFieldValues() / ExecuteList() to get the list
    // - note that this method prototype changed with revision 1.17 of the
    // framework: array of const used to be Args and '%' in the WhereClauseFormat
    // statement, whereas it now expects bound parameters as '?'
    function MultiFieldValues(Table: TOrmClass; const FieldNames: RawUtf8;
      const WhereClauseFormat: RawUtf8; const BoundsSqlWhere: array of const): TOrmTable; overload;
    /// Execute directly a SQL statement, returning a TOrmTable list of resutls
    // - return a TOrmTableJson instance on success, nil on failure
    // - FieldNames can be the CSV list of field names to be retrieved
    // - if FieldNames is '', will get all simple fields, excluding BLOBs
    // - if FieldNames is '*', will get ALL fields, including ID and BLOBs
    // - in this version, the WHERE clause can be created with the same format
    // as FormatUtf8() function, replacing all '%' chars with Args[], and all '?'
    // chars with Bounds[] (inlining them with :(...): and auto-quoting strings)
    // - example of use:
    // ! Table := MultiFieldValues(TOrm, 'Name', '%=?', ['ID'], [aID]);
    // - call overloaded MultiFieldValues() / ExecuteList() to get the list
    function MultiFieldValues(Table: TOrmClass; const FieldNames: RawUtf8;
      const WhereClauseFormat: RawUtf8; const Args, Bounds: array of const): TOrmTable; overload;
    /// dedicated method used to retrieve free-text matching DocIDs
    // - this method works for TOrmFts3, TOrmFts4 and TOrmFts5
    // - this method expects the column/field names to be supplied in the MATCH
    // statement clause
    // - example of use:  FTSMatch(TOrmMessage,'Body MATCH :("linu*"):',IntResult)
    // (using inlined parameters via :(...): is always a good idea)
    function FTSMatch(Table: TOrmFts3Class; const WhereClause: RawUtf8;
      var DocID: TIDDynArray): boolean; overload;
    /// dedicated method used to retrieve free-text matching DocIDs with
    // enhanced ranking information
    // - this method works for TOrmFts3, TOrmFts4 and TOrmFts5
    // - this method will search in all FTS3 columns, and except some floating-point
    // constants for weigthing each column (there must be the same number of
    // PerFieldWeight parameters as there are columns in the TOrmFts3 table)
    // - example of use:  FTSMatch(TOrmDocuments,'"linu*"',IntResult,[1,0.5])
    // which will sort the results by the rank obtained with the 1st column/field
    // beeing given twice the weighting of those in the 2nd (and last) column
    // - FTSMatch(TOrmDocuments,'linu*',IntResult,[1,0.5]) will perform a
    // SQL query as such, which is the fastest way of ranking according to
    // http://www.sqlite.org/fts3.html#appendix_a
    // $ SELECT RowID FROM Documents WHERE Documents MATCH 'linu*'
    // $ ORDER BY rank(matchinfo(Documents),1.0,0.5) DESC
    function FTSMatch(Table: TOrmFts3Class; const MatchClause: RawUtf8;
      var DocID: TIDDynArray; const PerFieldWeight: array of double;
      limit: integer = 0; offset: integer = 0): boolean; overload;
    /// retrieve the main field (mostly 'Name') value of the specified record
    // - use GetMainFieldName() method to get the main field name
    // - use OneFieldValue() method to get the field value
    // - return '' if no such field or record exists
    // - if ReturnFirstIfNoUnique is TRUE and no unique property is found,
    // the first RawUtf8 property is returned anyway
    function MainFieldValue(Table: TOrmClass; ID: TID;
      ReturnFirstIfNoUnique: boolean = false): RawUtf8;
    /// return the ID of the record which main field match the specified value
    // - search field is mainly the "Name" property, i.e. the one with
    // "stored AS_UNIQUE" (i.e. "stored false") definition on most TOrm
    // - returns 0 if no matching record was found }
    function MainFieldID(Table: TOrmClass; const Value: RawUtf8): TID;
    /// return the IDs of the record which main field match the specified values
    // - search field is mainly the "Name" property, i.e. the one with
    // "stored AS_UNIQUE" (i.e. "stored false") definition on most TOrm
    // - if any of the Values[] is not existing, then no ID will appear in the
    // IDs[] array - e.g. it will return [] if no matching record was found
    // - returns TRUE if any matching ID was found (i.e. if length(IDs)>0) }
    function MainFieldIDs(Table: TOrmClass; const Values: array of RawUtf8;
      out IDs: TIDDynArray): boolean;

    /// get a member from a SQL statement
    // - implements REST GET collection
    // - return true on success
    // - Execute 'SELECT * FROM TableName WHERE SqlWhere LIMIT 1' SQL Statememt
    // (using inlined parameters via :(...): in SqlWhere is always a good idea)
    // - since no record is specified, locking is pointless here
    // - default implementation call ExecuteList(), and fill Value from a
    // temporary TOrmTable
    // - if aCustomFieldsCsv is '', will get all simple fields, excluding BLOBs
    // and TOrmMany fields (use RetrieveBlob method or set
    // TRestClientUri.ForceBlobTransfert)
    // - if aCustomFieldsCsv is '*', will get ALL fields, including ID and BLOBs
    // - if this default set of simple fields does not fit your need, you could
    // specify your own set
    function Retrieve(const SqlWhere: RawUtf8; Value: TOrm;
      const aCustomFieldsCsv: RawUtf8 = ''): boolean; overload;
    /// get a member from a SQL statement
    // - implements REST GET collection
    // - return true on success
    // - same as Retrieve(const SqlWhere: RawUtf8; Value: TOrm) method, but
    // this overloaded function will call FormatUtf8 to create the Where Clause
    // from supplied parameters, replacing all '%' chars with Args[], and all '?'
    // chars with Bounds[] (inlining them with :(...): and auto-quoting strings)
    // - if aCustomFieldsCsv is '', will get all simple fields, excluding BLOBs
    // - if aCustomFieldsCsv is '*', will get ALL fields, including ID and BLOBs
    function Retrieve(const WhereClauseFmt: RawUtf8;
      const Args, Bounds: array of const; Value: TOrm;
      const aCustomFieldsCsv: RawUtf8 = ''): boolean; overload;
    /// get a member from its ID
    // - return true on success
    // - Execute 'SELECT * FROM TableName WHERE ID=:(aID): LIMIT 1' SQL Statememt
    // - if ForUpdate is true, the REST method is LOCK and not GET: it tries to lock
    // the corresponding record, then retrieve its content; caller has to call
    // UnLock() method after Value usage, to release the record
    // - this method will call EngineRetrieve() abstract method
    // - the RawBlob (BLOB) fields are not retrieved by this method, to
    // preserve bandwidth: use the RetrieveBlob() methods for handling
    // BLOB fields, or set either the TRestClientUri.ForceBlobTransfert
    // or TRestClientUri.ForceBlobTransfertTable[] properties
    // - the TOrmMany fields are not retrieved either: they are separate
    // instances created by TOrmMany.Create, with dedicated methods to
    // access to the separated pivot table
    function Retrieve(aID: TID; Value: TOrm;
      ForUpdate: boolean = false): boolean; overload;
    /// get a member from its TRecordReference property content
    // - instead of the other Retrieve() methods, this implementation Create an
    // instance, with the appropriated class stored in Reference
    // - returns nil on any error (invalid Reference e.g.)
    // - if ForUpdate is true, the REST method is LOCK and not GET: it tries to lock
    // the corresponding record, then retrieve its content; caller has to call
    // UnLock() method after Value usage, to release the record
    // - the RawBlob (BLOB) fields are not retrieved by this method, to
    // preserve bandwidth: use the RetrieveBlob() methods for handling
    // BLOB fields, or set either the TRestClientUri.ForceBlobTransfert
    // or TRestClientUri.ForceBlobTransfertTable[] properties
    // - the TOrmMany fields are not retrieved either: they are separate
    // instances created by TOrmMany.Create, with dedicated methods to
    // access to the separated pivot table
    function Retrieve(Reference: TRecordReference;
      ForUpdate: boolean = false): TOrm; overload;
    /// get a member from a published property TOrm
    // - those properties are not class instances, but TObject(aRecordID)
    // - is just a wrapper around Retrieve(aPublishedRecord.ID,aValue)
    // - return true on success
    function Retrieve(aPublishedRecord, aValue: TOrm): boolean; overload;
    /// get a list of members from a SQL statement as TObjectList
    // - implements REST GET collection
    // - for better server speed, the WHERE clause should use bound parameters
    // identified as '?' in the FormatSqlWhere statement, which is expected to
    // follow the order of values supplied in BoundsSqlWhere open array - use
    // DateToSql()/DateTimeToSql() for TDateTime, or directly any integer,
    // double, currency, RawUtf8 values to be bound to the request as parameters
    // - aCustomFieldsCsv can be the CSV list of field names to be retrieved
    // - if aCustomFieldsCsv is '', will get all simple fields, excluding BLOBs
    // - if aCustomFieldsCsv is '*', will get ALL fields, including ID and BLOBs
    // - return a TObjectList on success (possibly with Count=0) - caller is
    // responsible of freeing the instance
    // - this TObjectList will contain a list of all matching records
    // - return nil on error
    function RetrieveList(Table: TOrmClass;
      const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const;
      const aCustomFieldsCsv: RawUtf8 = ''): TObjectList; overload;
    {$ifdef HASGENERICS}
    /// access to ORM parametrized/generics methods
    // - since interface types can not have parametrized methods, we return
    // a TRestOrmGenerics abstract class to provide generics signature
    // - our IList<> and IKeyValue<> interfaces are faster and generates smaller
    // executables than Generics.Collections, and need no try..finally Free
    function Generics: TRestOrmGenerics;
    {$endif HASGENERICS}
    /// get a list of members from a SQL statement as RawJson
    // - implements REST GET collection
    // - for better server speed, the WHERE clause should use bound parameters
    // identified as '?' in the FormatSqlWhere statement, which is expected to
    // follow the order of values supplied in BoundsSqlWhere open array - use
    // DateToSql()/DateTimeToSql() for TDateTime, or directly any integer,
    // double, currency, RawUtf8 values to be bound to the request as parameters
    // - aCustomFieldsCsv can be the CSV list of field names to be retrieved
    // - if aCustomFieldsCsv is '', will get all simple fields, excluding BLOBs
    // - if aCustomFieldsCsv is '*', will get ALL fields, including ID and BLOBs
    // - returns the raw JSON array content with all items on success, with
    // our expanded / not expanded JSON format - so can be used with SOA methods
    // and RawJson results, for direct process from the client side
    // - returns '' on error
    // - the data is directly retrieved from raw JSON as returned by the database
    // without any conversion, so this method will be the fastest, but complex
    // types like dynamic array will be returned as Base64-encoded blob value -
    // if you need proper JSON access to those, see RetrieveDocVariantArray()
    function RetrieveListJson(Table: TOrmClass;
      const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const;
      const aCustomFieldsCsv: RawUtf8 = ''; aForceAjax: boolean = false): RawJson; overload;
    /// get a list of members from a SQL statement as RawJson
    // - implements REST GET collection
    // - this overloaded version expect the SqlWhere clause to be already
    // prepared with inline parameters using a previous FormatUtf8() call
    // - aCustomFieldsCsv can be the CSV list of field names to be retrieved
    // - if aCustomFieldsCsv is '', will get all simple fields, excluding BLOBs
    // - if aCustomFieldsCsv is '*', will get ALL fields, including ID and BLOBs
    // - returns the raw JSON array content with all items on success, with
    // our expanded / not expanded JSON format - so can be used with SOA methods
    // and RawJson results, for direct process from the client side
    // - returns '' on error
    // - the data is directly retrieved from raw JSON as returned by the database
    // without any conversion, so this method will be the fastest, but complex
    // types like dynamic array will be returned as Base64-encoded blob value -
    // if you need proper JSON access to those, see RetrieveDocVariantArray()
    function RetrieveListJson(Table: TOrmClass;
      const SqlWhere: RawUtf8; const aCustomFieldsCsv: RawUtf8 = '';
      aForceAjax: boolean = false): RawJson; overload;
    /// get a list of all members from a SQL statement as a TDocVariant
    // - implements REST GET collection
    // - if ObjectName='', it will return a TDocVariant of dvArray kind
    // - if ObjectName is set, it will return a TDocVariant of dvObject kind,
    // with one property containing the array of values: this returned variant
    // can be pasted e.g. directly as parameter to TSynMustache.Render()
    // - aCustomFieldsCsv can be the CSV list of field names to be retrieved
    // - if aCustomFieldsCsv is '', will get all simple fields, excluding BLOBs
    // - if aCustomFieldsCsv is '*', will get ALL fields, including ID and BLOBs
    // - the data will be converted to variants and TDocVariant following the
    // TOrm layout, so complex types like dynamic array will be returned
    // as a true array of values (in contrast to the RetrieveListJson method)
    // - warning: under FPC, we observed that assigning the result of this
    // method to a local variable may circumvent a memory leak FPC bug
    function RetrieveDocVariantArray(Table: TOrmClass;
      const ObjectName, CustomFieldsCsv: RawUtf8;
      FirstRecordID: PID = nil; LastRecordID: PID = nil): variant; overload;
    /// get a list of members from a SQL statement as a TDocVariant
    // - implements REST GET collection over a specified WHERE clause
    // - if ObjectName='', it will return a TDocVariant of dvArray kind
    // - if ObjectName is set, it will return a TDocVariant of dvObject kind,
    // with one property containing the array of values: this returned variant
    // can be pasted e.g. directly as parameter to TSynMustache.Render()
    // - for better server speed, the WHERE clause should use bound parameters
    // identified as '?' in the FormatSqlWhere statement, which is expected to
    // follow the order of values supplied in BoundsSqlWhere open array - use
    // DateToSql()/DateTimeToSql() for TDateTime, or directly any integer,
    // double, currency, RawUtf8 values to be bound to the request as parameters
    // - aCustomFieldsCsv can be the CSV list of field names to be retrieved
    // - if aCustomFieldsCsv is '', will get all simple fields, excluding BLOBs
    // - if aCustomFieldsCsv is '*', will get ALL fields, including ID and BLOBs
    // - the data will be converted to variants and TDocVariant following the
    // TOrm layout, so complex types like dynamic array will be returned
    // as a true array of values (in contrast to the RetrieveListJson method)
    // - warning: under FPC, we observed that assigning the result of this
    // method to a local variable may circumvent a memory leak FPC bug
    function RetrieveDocVariantArray(Table: TOrmClass;
      const ObjectName: RawUtf8; const FormatSqlWhere: RawUtf8;
      const BoundsSqlWhere: array of const; const CustomFieldsCsv: RawUtf8;
      FirstRecordID: PID = nil; LastRecordID: PID = nil): variant; overload;
    /// get all values of a SQL statement on a single column as a TDocVariant array
    // - implements REST GET collection on a single field
    // - for better server speed, the WHERE clause should use bound parameters
    // identified as '?' in the FormatSqlWhere statement, which is expected to
    // follow the order of values supplied in BoundsSqlWhere open array - use
    // DateToSql()/DateTimeToSql() for TDateTime, or directly any integer,
    // double, currency, RawUtf8 values to be bound to the request as parameters
    // - the data will be converted to variants and TDocVariant following the
    // TOrm layout, so complex types like dynamic array will be returned
    // as a true array of values (in contrast to the RetrieveListJson method)
    function RetrieveOneFieldDocVariantArray(Table: TOrmClass;
      const FieldName, FormatSqlWhere: RawUtf8;
      const BoundsSqlWhere: array of const): variant;
    /// get one member from a SQL statement as a TDocVariant
    // - implements REST GET collection
    // - the data will be converted to a TDocVariant variant following the
    // TOrm layout, so complex types like dynamic array will be returned
    // as a true array of values
    function RetrieveDocVariant(Table: TOrmClass;
      const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const;
      const CustomFieldsCsv: RawUtf8): variant;
    /// get a list of members from a SQL statement as T*ObjArray
    // - implements REST GET collection
    // - for better server speed, the WHERE clause should use bound parameters
    // identified as '?' in the FormatSqlWhere statement, which is expected to
    // follow the order of values supplied in BoundsSqlWhere open array - use
    // DateToSql()/DateTimeToSql() for TDateTime, or directly any integer,
    // double, currency, RawUtf8 values to be bound to the request as parameters
    // - aCustomFieldsCsv can be the CSV list of field names to be retrieved
    // - if aCustomFieldsCsv is '', will get all simple fields, excluding BLOBs
    // - if aCustomFieldsCsv is '*', will get ALL fields, including ID and BLOBs
    // - set the T*ObjArray variable with all items on success - so that it can
    // be used with SOA methods
    // - it is up to the caller to ensure that ObjClear(ObjArray) is called
    // when the T*ObjArray list is not needed any more
    // - returns true on success, false on error
    function RetrieveListObjArray(var ObjArray; Table: TOrmClass;
      const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const;
      const aCustomFieldsCsv: RawUtf8 = ''): boolean;
    /// get and append a list of members as an expanded JSON array
    // - implements REST GET collection
    // - generates '[{rec1},{rec2},...]' using a loop similar to:
    // ! while FillOne do .. AppendJsonObject() ..
    // - for better server speed, the WHERE clause should use bound parameters
    // identified as '?' in the FormatSqlWhere statement, which is expected to
    // follow the order of values supplied in BoundsSqlWhere open array - use
    // DateToSql()/DateTimeToSql() for TDateTime, or directly any integer,
    // double, currency, RawUtf8 values to be bound to the request as parameters
    // - if OutputFieldName is set, the JSON array will be written as a JSON,
    // property i.e. surrounded as '"OutputFieldName":[....],' - note ending ','
    // - CustomFieldsCsv can be the CSV list of field names to be retrieved
    // - if CustomFieldsCsv is '', will get all simple fields, excluding BLOBs
    // - if CustomFieldsCsv is '*', will get ALL fields, including ID and BLOBs
    // - is just a wrapper around TOrm.AppendFillAsJsonArray()
    procedure AppendListAsJsonArray(Table: TOrmClass;
      const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const;
      const OutputFieldName: RawUtf8; W: TOrmWriter;
      const CustomFieldsCsv: RawUtf8 = '');
    /// dedicated method used to retrieve matching IDs using a fast R-Tree index
    // - a TOrmRTree is associated to a TOrm with a specified BLOB
    // field, and will call TOrmRTree BlobToCoord and ContainedIn virtual
    // class methods to execute an optimized SQL query
    // - as alternative, with SQLite3 >= 3.24.0, you may use Auxiliary Columns
    // - will return all matching DataTable IDs in DataID[]
    // - will generate e.g. the following statement
    // $ SELECT MapData.ID From MapData, MapBox WHERE MapData.ID=MapBox.ID
    // $  AND minX>=:(-81.0): AND maxX<=:(-79.6): AND minY>=:(35.0): AND :(maxY<=36.2):
    // $  AND MapBox_in(MapData.BlobField,:('\uFFF0base64encoded-81,-79.6,35,36.2'):);
    // when the following Delphi code is executed:
    // ! aClient.RTreeMatch(TOrmMapData, 'BlobField', TOrmMapBox,
    // !   aMapData.BlobField, ResultID);
    function RTreeMatch(DataTable: TOrmClass;
      const DataTableBlobFieldName: RawUtf8; RTreeTable: TOrmRTreeClass;
      const DataTableBlobField: RawByteString; var DataID: TIDDynArray): boolean;
    /// Execute directly a SQL statement, expecting a list of results
    // - return a result table on success, nil on failure
    // - will call EngineList() abstract method to retrieve its JSON content
    function ExecuteList(const Tables: array of TOrmClass;
      const SQL: RawUtf8): TOrmTable;
    /// Execute directly a SQL statement, expecting a list of results
    // - you should not have to use this method, but the ORM versions instead
    // - return a result set as JSON on success, '' on failure
    // - will call EngineList() abstract method to retrieve its JSON content
    function ExecuteJson(const Tables: array of TOrmClass;
      const SQL: RawUtf8; ForceAjax: boolean = false;
      ReturnedRowCount: PPtrInt = nil): RawJson;
    /// Execute directly a SQL statement, without any expected result
    // - implements POST SQL on ModelRoot URI
    // - return true on success
    // - will call EngineExecute() abstract method to run the SQL statement
    function Execute(const aSql: RawUtf8): boolean;
    /// Execute directly a SQL statement with supplied parameters, with no result
    // - expect the same format as FormatUtf8() function, replacing all '%' chars
    // with Args[] values
    // - return true on success
    function ExecuteFmt(const SqlFormat: RawUtf8;
      const Args: array of const): boolean; overload;
    /// Execute directly a SQL statement with supplied parameters, with no result
    // - expect the same format as FormatUtf8() function, replacing all '%' chars
    // with Args[] values, and all '?' chars with Bounds[] (inlining them
    // with :(...): and auto-quoting strings)
    // - return true on success
    function ExecuteFmt(const SqlFormat: RawUtf8;
      const Args, Bounds: array of const): boolean; overload;
    /// unlock the corresponding record
    // - record should have been locked previously e.g. with Retrieve() and
    // forupdate=true, i.e. retrieved not via GET with LOCK REST-like verb
    // - use our custom UNLOCK REST-like verb
    // - returns true on success
    function UnLock(Table: TOrmClass; aID: TID): boolean; overload;
    /// unlock the corresponding record
    // - record should have been locked previously e.g. with Retrieve() and
    // forupdate=true, i.e. retrieved not via GET with LOCK REST-like verb
    // - use our custom UNLOCK REST-like method
    // - calls internally UnLock() above
    // - returns true on success
    function UnLock(Rec: TOrm): boolean; overload;
    /// create a new member
    // - implements REST POST collection
    // - if SendData is true, client sends the current content of Value with the
    // request, otherwise record is created with default values
    // - if ForceID is true, client sends the Value.ID field to use this ID for
    // adding the record (instead of a database-generated ID)
    // - on success, returns the new RowID value; on error, returns 0
    // - on success, Value.ID is updated with the new RowID
    // - the RawBlob(BLOB) fields values are not set by this method, to
    // preserve bandwidth - see UpdateBlobFields() and AddWithBlobs() methods
    // - the TOrmMany fields are not set either: they are separate
    // instances created by TOrmMany.Create, with dedicated methods to
    // access to the separated pivot table
    // - this method will call EngineAdd() to perform the request
    function Add(Value: TOrm; SendData: boolean;
      ForceID: boolean = false; DoNotAutoComputeFields: boolean = false): TID; overload;
    /// create a new member, including selected fields
    // - implements REST POST collection
    // - if ForceID is true, client sends the Value.ID field to use this ID for
    // adding the record (instead of a database-generated ID)
    // - this method will call EngineAdd() to perform the request
    function Add(Value: TOrm; const CustomCsvFields: RawUtf8;
      ForceID: boolean = false; DoNotAutoComputeFields: boolean = false): TID; overload;
    /// create a new member, including selected fields
    // - implements REST POST collection
    // - if ForceID is true, client sends the Value.ID field to use this ID for
    // adding the record (instead of a database-generated ID)
    // - this method will call EngineAdd() to perform the request
    function Add(Value: TOrm; const CustomFields: TFieldBits;
      ForceID: boolean = false; DoNotAutoComputeFields: boolean = false): TID; overload;
    /// create a new member, including its BLOB fields
    // - implements REST POST collection
    // - this method will create a JSON representation of the document
    // including the BLOB fields as Base64 encoded text, so will be less
    // efficient than a dual Add() + UpdateBlobFields() methods if the
    // binary content has a non trivial size
    // - this method will call EngineAdd() to perform the request
    function AddWithBlobs(Value: TOrm;
      ForceID: boolean = false; DoNotAutoComputeFields: boolean = false): TID;
    /// create a new member, from a supplied list of field values
    // - implements REST POST collection
    // - the aSimpleFields parameters must follow explicitly the order of published
    // properties of the supplied aTable class, excepting the RawBlob and
    // TOrmMany kind (i.e. only so called "simple fields")
    // - the aSimpleFields must have exactly the same count of parameters as
    // there are "simple fields" in the published properties
    // - if ForcedID is set to non null, client sends this ID to be used
    // when adding the record (instead of a database-generated ID)
    // - on success, returns the new RowID value; on error, returns 0
    // - call internally the Add virtual method above
    function AddSimple(aTable: TOrmClass;
      const aSimpleFields: array of const; ForcedID: TID = 0): TID;
    /// update a member from Value simple fields content
    // - implements REST PUT collection
    // - return true on success
    // - the RawBlob(BLOB) fields values are not updated by this method, to
    // preserve bandwidth: use the UpdateBlob() methods for handling BLOB fields
    // - the TOrmMany fields are not set either: they are separate
    // instances created by TOrmMany.Create, with dedicated methods to
    // access to the separated pivot table
    // - if CustomFields is left void, the  simple fields will be used, or the
    // fields retrieved via a previous FillPrepare() call; otherwise, you can
    // specify your own set of fields to be transmitted (including BLOBs, even
    // if they will be Base64-encoded within the JSON content) - CustomFields
    // could be computed by TOrmProperties.FieldBitsFromCsv()
    // or TOrmProperties.FieldBitsFrom()
    // - this method will always compute and send any TModTime fields
    // - this method will call EngineUpdate() to perform the request
    function Update(Value: TOrm; const CustomFields: TFieldBits = [];
      DoNotAutoComputeFields: boolean = false): boolean; overload;
    /// update a member from Value simple fields content
    // - implements REST PUT collection
    // - return true on success
    // - is an overloaded method to Update(Value,FieldBitsFromCsv())
    function Update(Value: TOrm; const CustomCsvFields: RawUtf8;
      DoNotAutoComputeFields: boolean = false): boolean; overload;
    /// update a member from a supplied list of simple field values
    // - implements REST PUT collection
    // - the aSimpleFields parameters MUST follow explicitly both count and
    // order of published properties of the supplied aTable class, excepting the
    // RawBlob and TOrmMany kind (i.e. only so called "simple fields")
    // - return true on success
    // - call internally the Update() / EngineUpdate() virtual methods
    function Update(aTable: TOrmClass; aID: TID;
      const aSimpleFields: array of const): boolean; overload;
    /// create or update a member, depending if the Value has already an ID
    // - implements REST POST if Value.ID=0 or ForceID is set, or a REST PUT
    // collection to update the record pointed by a Value.ID<>0
    // - will return the created or updated ID
    function AddOrUpdate(Value: TOrm; ForceID: boolean = false): TID;
    /// update one field/column value a given member
    // - implements REST PUT collection with one field value
    // - only one single field shall be specified in FieldValue, but could
    // be of any kind of value - for BLOBs, you should better use UpdateBlob()
    // - return true on success
    // - call internally the EngineUpdateField() abstract method
    // - note that this method won't update the TModTime properties: you should
    // rather use a classic Retrieve()/FillPrepare() followed by Update(); but
    // it will notify the internal Cache
    function UpdateField(Table: TOrmClass; ID: TID;
      const FieldName: RawUtf8; const FieldValue: array of const): boolean; overload;
    /// update one field in one or several members, depending on a WHERE clause
    // - implements REST PUT collection with one field value on a one where value
    // - only one single field shall be specified in FieldValue, but could
    // be of any kind of value - for BLOBs, you should better use UpdateBlob()
    // - only one single field shall be specified in WhereFieldValue, but could
    // be of any kind of value - for security reasons, void WHERE clause will
    // be rejected
    // - return true on success
    // - call internally the EngineUpdateField() abstract method
    // - note that this method won't update the TModTime properties: you should
    // rather use a classic Retrieve()/FillPrepare() followed by Update(); but
    // it will notify the internal Cache
    function UpdateField(Table: TOrmClass; const WhereFieldName: RawUtf8;
      const WhereFieldValue: array of const; const FieldName: RawUtf8;
      const FieldValue: array of const): boolean; overload;
    /// update one field in a given member with a value specified as variant
    // - implements REST PUT collection with one field value
    // - any value can be set in FieldValue, but for BLOBs, you should better
    // use UpdateBlob()
    // - return true on success
    // - call internally the EngineUpdateField() abstract method
    // - note that this method won't update the TModTime properties: you should
    // rather use a classic Retrieve()/FillPrepare() followed by Update(); but
    // it will notify the internal Cache
    function UpdateField(Table: TOrmClass; ID: TID;
      const FieldName: RawUtf8; const FieldValue: variant): boolean; overload;
    /// update one field in one or several members, depending on a WHERE clause,
    // with both update and where values specified as variant
    // - implements REST PUT collection with one field value on a one where value
    // - any value can be set in FieldValue, but for BLOBs, you should better
    // use UpdateBlob()
    // - for security reasons, void WHERE clause will be rejected
    // - return true on success
    // - call internally the EngineUpdateField() abstract method
    // - note that this method won't update the TModTime properties, nor the
    // internal table Cache: you should rather use a classic Retrieve()/FillPrepare()
    // followed by an Update() of the whole record
    function UpdateField(Table: TOrmClass;
      const WhereFieldName: RawUtf8; const WhereFieldValue: variant;
      const FieldName: RawUtf8; const FieldValue: variant): boolean; overload;
    /// update one field in one or several members, depending on a set of IDs
    // - return true on success
    // - note that this method won't update the TModTime properties: you should
    // rather use a classic Retrieve()/FillPrepare() followed by Update(), but
    // it will be much slower, even over a BATCH; anyway, it will update the
    // internal Cache
    // - will be executed as a regular SQL statement:
    // $ UPDATE table SET fieldname=fieldvalue WHERE RowID IN (...)
    // - warning: this method will call directly EngineExecute(), and will
    // work just fine with SQLite3, but some other DB engines may not allow
    // a huge number of items within the IN(...) clause
    function UpdateField(Table: TOrmClass; const IDs: array of Int64;
      const FieldName: RawUtf8; const FieldValue: variant): boolean; overload;
    /// increments one integer field value
    // - if available, this method will use atomic value modification, e.g.
    // $ UPDATE table SET field=field+?
    function UpdateFieldIncrement(Table: TOrmClass; ID: TID;
      const FieldName: RawUtf8; Increment: Int64 = 1): boolean;
    /// override this method to guess if this record can be updated or deleted
    // - this default implementation returns always true
    // - e.g. you can add digital signature to a record to disallow record editing
    // - the ErrorMsg can be set to a variable, which will contain an explicit
    // error message
    function RecordCanBeUpdated(Table: TOrmClass; ID: TID;
      Action: TOrmEvent; ErrorMsg: PRawUtf8 = nil): boolean;
    /// delete a member
    // - implements REST DELETE collection
    // - return true on success
    // - call internally the EngineDelete() abstract method
    function Delete(Table: TOrmClass; ID: TID): boolean; overload;
    /// delete a member with a WHERE clause
    // - implements REST DELETE collection
    // - return true on success
    // - this default method call OneFieldValues() to retrieve all matching IDs,
    // then will delete each row using protected EngineDeleteWhere() virtual method
    function Delete(Table: TOrmClass; const SqlWhere: RawUtf8): boolean; overload;
    /// delete a member with a WHERE clause
    // - implements REST DELETE collection
    // - return true on success
    // - for better server speed, the WHERE clause should use bound parameters
    // identified as '?' in the FormatSqlWhere statement, which is expected to
    // follow the order of values supplied in BoundsSqlWhere open array - use
    // DateToSql/DateTimeToSql for TDateTime, or directly any integer / double /
    // currency / RawUtf8 values to be bound to the request as parameters
    // - is a simple wrapper around:
    // ! Delete(Table, FormatUtf8(FormatSqlWhere, [], BoundsSqlWhere))
    function Delete(Table: TOrmClass;
      const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const): boolean; overload;

    /// get a blob field content from its record ID and supplied blob field name
    // - implements REST GET collection with a supplied member ID and a blob field name
    // - return true on success
    // - this method is defined as abstract, i.e. there is no default implementation:
    // it must be implemented 100% RestFul with a
    // GET ModelRoot/TableName/TableID/BlobFieldName request for example
    // - this method retrieve the blob data as a RawBlob string using
    // EngineRetrieveBlob()
    function RetrieveBlob(Table: TOrmClass;
      aID: TID; const BlobFieldName: RawUtf8;
      out BlobData: RawBlob): boolean; overload;
    /// get a blob field content from its record ID and supplied blob field name
    // - implements REST GET collection with a supplied member ID and field name
    // - return true on success
    // - this method will create a TStream instance (which must be freed by the
    // caller after use) and fill it with the blob data
    function RetrieveBlob(Table: TOrmClass;
      aID: TID; const BlobFieldName: RawUtf8;
      out BlobStream: TCustomMemoryStream): boolean; overload;
    /// update a blob field from its record ID and supplied blob field name
    // - implements REST PUT collection with a supplied member ID and field name
    // - return true on success
    // - call internally the EngineUpdateBlob() abstract method
    // - this method expect the Blob data to be supplied as RawBlob, using
    // EngineUpdateBlob()
    function UpdateBlob(Table: TOrmClass;
      aID: TID; const BlobFieldName: RawUtf8; const BlobData: RawBlob): boolean; overload;
    /// update a blob field from its record ID and blob field name
    // - implements REST PUT collection with a supplied member ID and field name
    // - return true on success
    // - call internally the EngineUpdateBlob() abstract method
    // - this method expect the Blob data to be supplied as a TStream: it will
    // send the whole stream content (from its beginning position upto its
    // current size) to the database engine
    function UpdateBlob(Table: TOrmClass;
      aID: TID; const BlobFieldName: RawUtf8; BlobData: TStream): boolean; overload;
    /// update a blob field from its record ID and blob field name
    // - implements REST PUT collection with a supplied member ID and field name
    // - return true on success
    // - call internally the EngineUpdateBlob() abstract method
    // - this method expect the Blob data to be supplied as direct memory pointer
    // and size
    function UpdateBlob(Table: TOrmClass;
      aID: TID; const BlobFieldName: RawUtf8;
      BlobData: pointer; BlobSize: integer): boolean; overload;
    /// update all BLOB fields of the supplied Value
    // - call several REST PUT collection (one for each BLOB) for the member
    // - uses the UpdateBlob() method to send the BLOB properties content to the Server
    // - called internally by Add and Update methods when ForceBlobTransfert /
    // ForceBlobTransfertTable[] is set
    // - you can use this method by hand, to avoid several calls to UpdateBlob()
    // - returns TRUE on success (or if there is no BLOB field)
    // - returns FALSE on error (e.g. if Value is invalid or with db/transmission)
    function UpdateBlobFields(Value: TOrm): boolean;
    /// get all BLOB fields of the supplied value from the remote server
    // - call several REST GET collection (one for each BLOB) for the member
    // - call internally e.g. by TRestClient.Retrieve method when
    // ForceBlobTransfert / ForceBlobTransfertTable[] is set
    function RetrieveBlobFields(Value: TOrm): boolean;

    /// begin a transaction (to be used on Server side)
    // - implements REST BEGIN collection
    // - warning: from CLIENT side, you should better use a BATCH process,
    // specifying a AutomaticTransactionPerRow value to BatchStart()
    // - may be used to speed up CRUD statements like Add/Update/Delete
    // - in the current implementation, nested transactions are not allowed
    // - must be ended with Commit on success
    // - must be aborted with Rollback if any SQL statement failed
    // - default implementation just handle the protected fTransactionActiveSession flag
    // - return true if no transaction is active, false otherwise
    // - in a multi-threaded or Client-Server with multiple concurrent Client
    // connections, you may check the returned value, as such:
    //  !if Client.TransactionBegin(TOrmPeopleObject) then
    //  !try
    //  !  //.... modify the database content, raise exceptions on error
    //  !  Client.Commit;
    //  !except
    //  !  Client.RollBack; // in case of error
    //  !end;
    // or use the TransactionBeginRetry() method
    // - the supplied SessionID will allow multi-user transaction safety on the
    // Server-Side: all database modification from another session will wait
    // for the global transaction to be finished; on Client-side, the SessionID
    // is just ignored (TRestClient will override this method with a default
    // SessionID=CONST_AUTHENTICATION_NOT_USED=1 parameter)
    // - if you have an external database engine which expect transactions to
    // take place in the same thread, ensure TRestServer force execution of
    // this method when accessed from RESTful clients in the same thread, e.g.:
    // ! AcquireExecutionMode[execOrmWrite] := amBackgroundThread;
    // ! AcquireWriteMode := amBackgroundThread; // same as previous
    function TransactionBegin(aTable: TOrmClass;
      SessionID: cardinal = CONST_AUTHENTICATION_NOT_USED): boolean;
    /// check current transaction status (to be used on Server side)
    // - warning: from CLIENT side, you should better use a BATCH process
    // - returns the session ID if a transaction is active
    // - returns 0 if no transaction is active
    function TransactionActiveSession: cardinal;
    /// end a transaction (to be used on Server side)
    // - implements REST END collection
    // - warning: from CLIENT side, you should better use a BATCH process,
    // specifying a AutomaticTransactionPerRow value to BatchStart()
    // - write all pending SQL statements to the disk
    // - default implementation just reset the protected fTransactionActiveSession flag
    // - the supplied SessionID will allow multi-user transaction safety on the
    // Server-Side: all database modification from another session will wait
    // for the global transaction to be finished; on Client-side, the SessionID
    // is just ignored (TRestClient will override this method with a default
    // SessionID=CONST_AUTHENTICATION_NOT_USED=1 parameter)
    // - if you have an external database engine which expect transactions to
    // take place in the same thread, ensure TRestServer force execution of
    // this method when accessed from RESTful clients in the same thread, e.g.:
    // ! AcquireExecutionMode[execOrmWrite] := amBackgroundThread;
    // ! AcquireWriteMode := amBackgroundThread; // same as previous
    // - by default, any exception will be catch and ignored, unless RaiseException
    // is set to TRUE so that the caller will be able to handle it
    procedure Commit(SessionID: cardinal = CONST_AUTHENTICATION_NOT_USED;
      RaiseException: boolean = false);
    /// abort a transaction (to be used on Server side)
    // - implements REST ABORT collection
    // - warning: from CLIENT side, you should better use a BATCH process
    // - restore the previous state of the database, before the call to TransactionBegin
    // - default implementation just reset the protected fTransactionActiveSession flag
    // - the supplied SessionID will allow multi-user transaction safety on the
    // Server-Side: all database modification from another session will wait
    // for the global transaction to be finished; on Client-side, the SessionID
    // is just ignored (TRestClient will override this method with a default
    // SessionID=CONST_AUTHENTICATION_NOT_USED=1 parameter)
    // - if you have an external database engine which expect transactions to
    // take place in the same thread, ensure TRestServer force execution of
    // this method when accessed from RESTful clients in the same thread, e.g.:
    // ! AcquireExecutionMode[execOrmWrite] := amBackgroundThread;
    // ! AcquireWriteMode := amBackgroundThread; // same as previous
    procedure RollBack(SessionID: cardinal = CONST_AUTHENTICATION_NOT_USED);
    /// enter the Mutex associated with the write operations of this instance
    // - just a wrapper around TRest.AcquireExecution[execOrmWrite].Safe.Lock
    procedure WriteLock;
    /// leave the Mutex associated with the write operations of this instance
    // - just a wrapper around TRest.AcquireExecution[execOrmWrite].Safe.UnLock
    procedure WriteUnLock;

    /// execute a BATCH sequence prepared in a TRestBatch instance
    // - implements the "Unit Of Work" pattern, i.e. safe transactional process
    // even on multi-thread environments
    // - it is more efficient and safe than TransactionBegin/Commit, and
    // definitively the way to go from the client side
    // - send all pending Add/Update/Delete statements to the DB or remote server
    // - will return the URI Status value, i.e. 200/HTTP_SUCCESS OK on success
    // - a dynamic array of integers will be created in Results,
    // containing all ROWDID created for each BatchAdd call, 200 (=HTTP_SUCCESS)
    // for all successfull BatchUpdate/BatchDelete, or 0 on error
    // - any error during server-side process MUST be checked against Results[]
    // (the main URI Status is 200 if about communication success, and won't
    // imply that all statements in the BATCH sequence were successfull),
    // or boRollbackOnError should be set in TRestBatchOptions
    // - note that the caller shall still free the supplied Batch instance
    function BatchSend(Batch: TRestBatch; var Results: TIDDynArray): integer; overload;
    /// execute a BATCH sequence prepared in a TRestBatch instance
    // - just a wrapper around the overloaded BatchSend() method without the
    // Results: TIDDynArray parameter
    function BatchSend(Batch: TRestBatch): integer; overload;
    /// raw send/execute the supplied JSON BATCH content, and return the expected array
    // - this method will be implemented for TRestClient and TRestServer only
    // - default implementation will trigger an EOrmException
    // - warning: supplied JSON Data can be parsed in-place, so modified
    // - you should not use this low-level method in your code, but rather the
    // overloaded BatchSend() functions; is defined for raw asynchronous call
    function BatchSend(Table: TOrmClass; var Data: RawUtf8;
       var Results: TIDDynArray; ExpectedResultsCount: integer): integer; overload;
    /// prepare an asynchronous ORM BATCH process, executed in a background thread
    // - will initialize a TRestBatch and call TimerEnable to initialize the
    // background thread, following the given processing period (in seconds),
    // or the TRestBatch.Count threshold to call BatchSend
    // - actual REST/CRUD commands will take place via AsyncBatchAdd,
    // AsyncBatchUpdate and AsyncBatchDelete methods
    // - only a single AsyncBatch() call per Table is allowed at a time, unless
    // AsyncBatchStop method is used to flush the current asynchronous BATCH
    // - using a BATCH in a dedicated thread will allow very fast background
    // asynchronous process of ORM methods, sufficient for most use cases
    // - is a wrapper around BackgroundTimer.AsyncBatchStart()
    function AsyncBatchStart(Table: TOrmClass; SendSeconds: integer;
      PendingRowThreshold: integer = 500; AutomaticTransactionPerRow: integer = 1000;
      Options: TRestBatchOptions = [boExtendedJson]): boolean;
    /// finalize asynchronous ORM BATCH process, executed in a background thread
    // - should have been preceded by a call to AsyncBatch(), or returns false
    // - Table=nil will release all existing batch instances
    // - is a wrapper around BackgroundTimer.AsyncBatchStop()
    function AsyncBatchStop(Table: TOrmClass): boolean;
    /// create a new ORM member in a BATCH to be written in a background thread
    // - should have been preceded by a call to AsyncBatchStart(), or returns -1
    // - is a wrapper around BackgroundTimer.AsyncBatchAdd(),
    // so will return the index in the BATCH rows, not the created TID
    // - this method is thread-safe
    function AsyncBatchAdd(Value: TOrm; SendData: boolean;
      ForceID: boolean = false; const CustomFields: TFieldBits = [];
      DoNotAutoComputeFields: boolean = false): integer;
    /// append some JSON content in a BATCH to be written in a background thread
    // - could be used to emulate AsyncBatchAdd() with an already pre-computed
    // JSON object
    // - is a wrapper around BackgroundTimer.AsyncBatchRawAdd(),
    // so will return the index in the BATCH rows, not the created TID
    // - this method is thread-safe
    function AsyncBatchRawAdd(Table: TOrmClass; const SentData: RawUtf8): integer;
    /// append some JSON content in a BATCH to be writen in a background thread
    // - could be used to emulate AsyncBatchAdd() with an already pre-computed
    // JSON object, as stored in a TJsonWriter instance
    // - is a wrapper around BackgroundTimer.AsyncBatchRawAppend()
    // - this method is thread-safe
    procedure AsyncBatchRawAppend(Table: TOrmClass; SentData: TJsonWriter);
    /// update an ORM member in a BATCH to be written in a background thread
    // - should have been preceded by a call to AsyncBatchStart(), or returns -1
    // - is a wrapper around BackgroundTimer.AsyncBatchUpdate()
    // - this method is thread-safe
    function AsyncBatchUpdate(Value: TOrm; const CustomFields: TFieldBits = [];
      DoNotAutoComputeFields: boolean = false): integer;
    /// delete an ORM member in a BATCH to be written in a background thread
    // - should have been preceded by a call to AsyncBatchStart(), or returns -1
    // - is a wrapper around the TRestBatch.Delete() sent in the Timer thread
    // - this method is thread-safe
    function AsyncBatchDelete(Table: TOrmClass; ID: TID): integer;

    /// access the Database Model associated with REST Client or Server instance
    function Model: TOrmModel;
    /// access the internal caching parameters for a given TOrm
    // - will always return a TRestCache instance, creating one if needed
    // - purpose of this caching mechanism is to speed up retrieval of some
    // common values at either Client or Server level (like configuration settings)
    // - by default, this CRUD level per-ID cache is disabled
    // - use Cache.SetCache() and Cache.SetTimeOut() methods to set the appropriate
    // configuration for this particular TRest instance
    // - only caching synchronization is about the direct RESTful/CRUD commands:
    // RETRIEVE, ADD, UPDATE and DELETE (that is, a complex direct SQL UPDATE or
    // via TOrmMany pattern won't be taken into account - only exception is
    // TRestStorage tables accessed as SQLite3 virtual table)
    // - this caching will be located at the TRest level, that is no automated
    // synchronization is implemented between TRestClient and TRestServer -
    // you shall ensure that your business logic is safe, calling Cache.Flush()
    // overloaded methods on purpose: better no cache than unproper cache -
    // "premature optimization is the root of all evil"
    function Cache: TRestCache;
    /// access the internal caching parameters for a given TOrm
    // - will return nil if no TRestCache instance has been defined
    function CacheOrNil: TRestCache;
    /// returns TRUE if this table is worth caching (e.g. not in memory)
    function CacheWorthItForTable(aTableIndex: cardinal): boolean;
    /// log the corresponding text (if logging is enabled)
    procedure InternalLog(const Text: RawUtf8; Level: TSynLogInfo); overload;
    /// log the corresponding text (if logging is enabled)
    procedure InternalLog(const Format: RawUtf8; const Args: array of const;
      Level: TSynLogInfo = sllTrace); overload;
    /// access to the associate TSynLog class type
    function LogClass: TSynLogClass;
    /// access to the associate TSynLog class familly
    function LogFamily: TSynLogFamily;
    /// retrieve the current server time stamp as a TTimeLog
    // - used e.g. by TOrm.ComputeFieldsBeforeWrite for oftModTime/oftCreateTime
    // - is safe on both client and server sides
    function GetServerTimestamp: TTimeLog;
    /// retrieve the logged session User ID
    // - used e.g. by TOrm.ComputeFieldsBeforeWrite for oftSessionUserID
    // - returns 0 if no session/authentication was currently initiated
    function GetCurrentSessionUserID: TID;
  end;

  /// Client-Specific Object-Relational-Mapping calls for CRUD access to a database
  // - in addition to the default IRestOrm methods, offer to drive a TRestBatch
  // instance owned on the client side
  IRestOrmClient = interface(IRestOrm)
    ['{6553FE4C-B841-493C-82F8-495A34A4F966}']
    /// get a member from its ID
    // - implements REST GET collection
    // - URI is 'ModelRoot/TableName/TableID' with GET method
    // - returns true on server returned 200/HTTP_SUCCESS OK success, false on error
    // - set Refreshed to true if the content changed
    function Refresh(aID: TID; Value: TOrm; var Refreshed: boolean): boolean;
    /// ask the server for its current internal state revision counter
    // - this counter is incremented every time the database is modified
    // - the returned value is 0 if the database doesn't support this feature
    // - TOrmTable does compare this value with its internal one to check if
    // its content must be updated
    // - is defined here and not in IRestOrmClient since it is very specific
    function ServerInternalState: cardinal;
    /// check if the data may have changed of the server for this objects, and
    // update it if possible
    // - only working types are TOrmTableJson and TOrm descendants
    // - make use of the InternalState function to check the data content revision
    // - return true if Data is updated successfully, or false on any error
    // during data retrieval from server (e.g. if the TOrm has been deleted)
    // - if Data contains only one TOrmTableJson, PCurrentRow can point to the
    // current selected row of this table, in order to refresh its value
    // - use this method to refresh the client UI, e.g. via a timer
    // - is defined here and not in IRestOrmClient since it is very specific
    function UpdateFromServer(const Data: array of TObject; out Refreshed: boolean;
      PCurrentRow: PInteger = nil): boolean;
    /// send a flush command to the remote Server cache
    // - this method will remotely call the Cache.Flush() methods of the server
    // instance, to force cohesion of the data
    // - ServerCacheFlush() with no parameter will flush all stored JSON content
    // - ServerCacheFlush(aTable) will flush the cache for a given table
    // - ServerCacheFlush(aTable,aID) will flush the cache for a given record
    function ServerCacheFlush(aTable: TOrmClass = nil;
      aID: TID = 0): boolean;
    /// retrieve a list of members as a TOrmTable
    // - implements REST GET collection
    // - default SQL statement is 'SELECT ID FROM TableName;' (i.e. retrieve
    // the list of all ID of this collection members)
    // - optional SqlSelect parameter to change the returned fields
    // as in 'SELECT SqlSelect FROM TableName;'
    // - optional SqlWhere parameter to change the search range or ORDER
    // as in 'SELECT SqlSelect FROM TableName WHERE SqlWhere;'
    // - using inlined parameters via :(...): in SqlWhere is always a good idea
    // - for one TClass, you should better use TRest.MultiFieldValues()
    function List(const Tables: array of TOrmClass;
      const SqlSelect: RawUtf8 = 'RowID';
      const SqlWhere: RawUtf8 = ''): TOrmTable;
    /// retrieve a list of members as a TOrmTable
    // - implements REST GET collection
    // - in this version, the WHERE clause can be created with the same format
    // as FormatUtf8() function, replacing all '%' chars with Args[] values
    // - using inlined parameters via :(...): in SqlWhereFormat is always a good idea
    // - for one TClass, you should better use TRest.MultiFieldValues()
    // - will call the List virtual method internally
    function ListFmt(const Tables: array of TOrmClass;
      const SqlSelect, SqlWhereFormat: RawUtf8;
      const Args: array of const): TOrmTable; overload;
    /// retrieve a list of members as a TOrmTable
    // - implements REST GET collection
    // - in this version, the WHERE clause can be created with the same format
    // as FormatUtf8() function, replacing all '%' chars with Args[], and all '?'
    // chars with Bounds[] (inlining them with :(...): and auto-quoting strings)
    // - example of use:
    // ! Table := ListFmt([TOrm],'Name','ID=?',[],[aID]);
    // - for one TClass, you should better use TRest.MultiFieldValues()
    // - will call the List virtual method internally
    function ListFmt(const Tables: array of TOrmClass;
      const SqlSelect, SqlWhereFormat: RawUtf8;
      const Args, Bounds: array of const): TOrmTable; overload;
    /// begin a transaction
    // - implements REST BEGIN collection
    // - in aClient-Server environment with multiple Clients connected at the
    // same time, you should better use BATCH process, specifying a positive
    // AutomaticTransactionPerRow parameter to BatchStart()
    // - this version retries a TranslationBegin() to be successfull within
    // a supplied number of times
    // - will retry every 100 ms for "Retries" times (excluding the connection
    // time in this 100 ms time period
    // - default is to retry 10 times, i.e. within 2 second timeout
    // - in the current implementation, the aTable parameter is not used yet
    // - typical usage should be for instance:
    // !if Client.TransactionBeginRetry(TOrmPeopleObject,20) then
    // !try
    // !  // .... modify the database content, raise exceptions on error
    // !  Client.Commit;
    // !except
    // !  Client.RollBack; //  in case of error
    // !end;
    function TransactionBeginRetry(aTable: TOrmClass;
      Retries: integer = 10): boolean;
    /// begin a BATCH sequence to speed up huge database change for a given table
    // - is a wrapper around TRestBatch.Create() which will be stored in the
    // implementation class instance - be aware that this won't be thread-safe
    // - if you need a thread-safe "Unit Of Work" process, please use a private
    // TRestBatch instance and the overloaded IRestOrm.BatchSend() method
    // - call BatchStartAny() or set the aTable parameter to nil if you want to
    // use any kind of TOrm objects within the process, not a single one
    // - force AutomaticTransactionPerRow=0 if TransactionBegin() has been called
    // - WARNING: on mORMot 1, AutomaticTransactionPerRow was 0 which was slower
    function BatchStart(aTable: TOrmClass;
      AutomaticTransactionPerRow: cardinal = 10000;
      Options: TRestBatchOptions = [boExtendedJson]): boolean; 
    /// begin a BATCH sequence to speed up huge database change for any table
    // - will call the BatchStart() method with aTable = nil so that you may be
    // able to use any kind of TOrm class within the process
    // - is a wrapper around TRestBatch.Create() which will be stored in the
    // implementation class instance - be aware that this won't be thread-safe
    function BatchStartAny(AutomaticTransactionPerRow: cardinal;
      Options: TRestBatchOptions = [boExtendedJson]): boolean;
    /// create a new member in current BATCH sequence
    // - is a wrapper around TRestBatch.Add() which will be stored in the
    // implementation class instance - be aware that this won't be thread-safe
    function BatchAdd(Value: TOrm; SendData: boolean; ForceID: boolean = false;
      const CustomFields: TFieldBits = []): integer;
    /// update a member in current BATCH sequence
    // - is a wrapper around TRestBatch.Update() which will be stored in the
    // implementation class instance - be aware that this won't be thread-safe
    // - this method will call BeforeUpdateEvent before TRestBatch.Update
    function BatchUpdate(Value: TOrm; const CustomFields: TFieldBits = [];
      DoNotAutoComputeFields: boolean = false): integer; overload;
    /// update a member in current BATCH sequence
    // - is a wrapper around TRestBatch.Update() which will be stored in the
    // implementation class instance - be aware that this won't be thread-safe
    // - this method will call BeforeUpdateEvent before TRestBatch.Update
    function BatchUpdate(Value: TOrm; const CustomFieldsCsv: RawUtf8;
      DoNotAutoComputeFields: boolean = false): integer; overload;
    /// delete a member in current BATCH sequence
    // - is a wrapper around TRestBatch.Delete() which will be stored in the
    // implementation class instance - be aware that this won't be thread-safe
    function BatchDelete(ID: TID): integer; overload;
    /// delete a member in current BATCH sequence
    // - is a wrapper around TRestBatch.Delete() which will be stored in the
    // implementation class instance - be aware that this won't be thread-safe
    function BatchDelete(Table: TOrmClass; ID: TID): integer; overload;
    /// retrieve the current number of pending transactions in the BATCH sequence
    // - every call to BatchAdd/Update/Delete methods increases this counter
    function BatchCount: integer;
    /// execute a BATCH sequence started by BatchStart method
    // - send all pending BatchAdd/Update/Delete statements to the remote server
    // - URI is 'ModelRoot/TableName/0' with POST (or PUT) method
    // - will return the URI Status value, i.e. 200/HTTP_SUCCESS OK on success
    // - a dynamic array of integers will be created in Results,
    // containing all ROWDID created for each BatchAdd call, 200 (=HTTP_SUCCESS)
    // for all successfull BatchUpdate/BatchDelete, or 0 on error
    // - any error during server-side process MUST be checked against Results[]
    // (the main URI Status is 200 if about communication success, and won't
    // imply that all statements in the BATCH sequence were successfull
    function BatchSend(var Results: TIDDynArray): integer; overload;
    /// abort a BATCH sequence started by BatchStart method
    // - in short, nothing is sent to the remote server, and current BATCH
    // sequence is closed
    // - will Free the TRestBatch stored in this class instance
    procedure BatchAbort;
    function GetForceBlobTransfert: boolean;
    procedure SetForceBlobTransfert(Value: boolean);
    function GetForceBlobTransfertTable(aTable: TOrmClass): boolean;
    procedure SetForceBlobTransfertTable(aTable: TOrmClass; aValue: boolean);
    /// if set to TRUE, all BLOB fields of all tables will be transferred
    // between the Client and the remote Server
    // - i.e. Add() Update() will use Blob-related RESTful PUT/POST request
    // - i.e. Retrieve() will use Blob-related RESTful GET request
    // - note that the Refresh method won't handle BLOB fields, even if this
    // property setting is set to TRUE
    // - by default, this property is set to FALSE, which setting will spare
    // bandwidth and CPU
    // - this property is global to all tables of the model - you can also use
    // ForceBlobTransfertTable[] to force it for a particular table
    property ForceBlobTransfert: boolean
      read GetForceBlobTransfert write SetForceBlobTransfert;
    /// if set to TRUE for a specified table of the model, all BLOB fields of
    // this tables will be transferred between the Client and the remote Server
    // - i.e. Add() Update() will use BLOB-related RESTful PUT/POST request for
    // this table
    // - i.e. Retrieve() will use BLOB-related RESTful GET request for
    // this table
    // - note that the Refresh method won't handle BLOB fields, even if this
    // property setting is set to TRUE
    // - by default, all items of this property are set to FALSE, which
    // setting will spare bandwidth and CPU
    // - this property is particular to a given tables of the model - you can
    // also use ForceBlobTransfert to force it for a all tables of this model
    property ForceBlobTransfertTable[aTable: TOrmClass]: boolean
      read GetForceBlobTransfertTable write SetForceBlobTransfertTable;
  end;

  /// event signature triggered by TRestBatch.OnWrite
  // - also used by TRestServer.RecordVersionSynchronizeSlave*() methods
  TOnBatchWrite = procedure(Sender: TRestBatch; Event: TOrmOccasion;
    Table: TOrmClass; const ID: TID; Value: TOrm;
    const ValueFields: TFieldBits) of object;

  /// Server-Specific Object-Relational-Mapping calls for CRUD access to a database
  IRestOrmServer = interface(IRestOrm)
    ['{F8FB2109-5629-4DFB-A74C-7A0F86F91362}']
    /// missing tables are created if they don't exist yet for every TOrm
    // class of the Database Model
    // - you must call explicitly this before having called StaticDataCreate()
    // - all table description (even Unique feature) is retrieved from the Model
    // - this method should also create additional fields, if the TOrm definition
    // has been modified; only field adding is mandatory, field renaming or
    // field deleting are not allowed in the FrameWork (in such cases, you must
    // create a new TOrm type)
    // - this virtual method do nothing by default - overridden versions should
    // implement it as expected by the underlying storage engine (e.g. SQLite3
    // or TRestServerFullInMemory)
    // - you can tune some options transmitted to the TOrm.InitializeTable
    // virtual methods, e.g. to avoid the automatic create of indexes
    procedure CreateMissingTables(user_version: cardinal = 0;
      options: TOrmInitializeTableOptions = []);
    /// create an index for the specific FieldName
    // - will call CreateSqlMultiIndex() internally
    function CreateSqlIndex(Table: TOrmClass; const FieldName: RawUtf8;
      Unique: boolean; const IndexName: RawUtf8 = ''): boolean; overload;
    /// create one or multiple index(es) for the specific FieldName(s)
    function CreateSqlIndex(Table: TOrmClass;
      const FieldNames: array of RawUtf8; Unique: boolean): boolean; overload;
    /// create one index for all specific FieldNames at once
    // - will call any static engine for the index creation of such tables, or
    // execute a CREATE INDEX IF NOT EXISTS on the main engine
    // - note that with SQLite3, your database schema should never contain two
    // indices where one index is a prefix of the other, e.g. if you defined:
    // ! aServer.CreateSqlMultiIndex(TEmails, ['Email','GroupID'], True);
    // Then the following index is not mandatory for SQLite3:
    // ! aServer.CreateSqlIndex(TEmails, 'Email', False);
    // see "1.6 Multi-Column Indices" in @http://www.sqlite.org/queryplanner.html
    function CreateSqlMultiIndex(Table: TOrmClass;
      const FieldNames: array of RawUtf8; Unique: boolean; IndexName: RawUtf8 = ''): boolean;

    /// initialize change tracking for the given tables
    // - by default, it will use the aTableHistory table (which should be a
    // TOrmHistory) to store the changes
    // - if aTableHistory is not already part of the TOrmModel, it will be added
    // - note that this setting should be consistent in time: if you disable
    // tracking for a while, or did not enable tracking before adding a record,
    // then the content history won't be consistent (or disabled) for this record
    // - at every change, aTableHistory.SentDataJson records will be added, up
    // to aMaxHistoryRowBeforeBlob items - then aTableHistory.History will store
    // a compressed version of all previous changes
    // - aMaxHistoryRowBeforeBlob is the maximum number of JSON rows per Table
    // before compression into BLOB is triggerred
    // - aMaxHistoryRowPerRecord is the maximum number of JSON rows per record,
    // above which the versions will be compressed as BLOB
    // - aMaxUncompressedBlobSize is the maximum BLOB size per record
    // - you can specify aMaxHistoryRowBeforeBlob=0 to disable change tracking
    // - you should call this method after the CreateMissingTables call
    // - note that change tracking may slow down the writing process, and
    // may increase storage space a lot (even if BLOB maximum size can be set),
    // so should be defined only when necessary
    procedure TrackChanges(const aTable: array of TOrmClass;
      aTableHistory: TOrmClass = nil;
      aMaxHistoryRowBeforeBlob: integer = 1000;
      aMaxHistoryRowPerRecord: integer = 10;
      aMaxUncompressedBlobSize: integer = 64*1024);
    /// force compression of all aTableHistory.SentDataJson into History BLOB
    // - by default, this will take place in InternalUpdateEvent() when
    // aMaxHistoryRowBeforeBlob - as set by TrackChanges() method - is reached
    // - you can manually call this method to force History BLOB update, e.g.
    // when the server is in Idle state, and ready for process
    procedure TrackChangesFlush(aTableHistory: TOrmClass);
    /// will compute the next monotonic value for a TRecordVersion field
    function RecordVersionCompute: TRecordVersion;
    /// read only access to the current monotonic value for a TRecordVersion field
    function RecordVersionCurrent: TRecordVersion;
    /// synchronous master/slave replication from a slave TRest
    // - apply all the updates from another (distant) master TRestOrm for a given
    // TOrm table, using its TRecordVersion field, to the calling slave
    // - both remote Master and local slave TRestServer should have the supplied
    // Table class in their data model (maybe in diverse order)
    // - by default, all pending updates are retrieved, but you can define a value
    // to ChunkRowLimit, so that the updates will be retrieved by smaller chunks
    // - returns -1 on error, or the latest applied revision number (which may
    // be 0 if there is no data in the table)
    // - this method will use regular REST ORM commands, so will work with any
    // communication channels: for real-time push synchronization, consider using
    // RecordVersionSynchronizeMasterStart and RecordVersionSynchronizeSlaveStart
    // over a bidirectionnal communication channel like WebSockets
    // - you can use RecordVersionSynchronizeSlaveToBatch if your purpose is
    // to access the updates before applying to the current slave storage
    function RecordVersionSynchronizeSlave(Table: TOrmClass;
      const Master: IRestOrm; ChunkRowLimit: integer = 0;
      const OnWrite: TOnBatchWrite = nil): TRecordVersion;
    /// synchronous master/slave replication from a slave TRest into a Batch
    // - will retrieve all the updates from a (distant) master TRest for a
    // given TOrm table, using its TRecordVersion field, and a supplied
    // TRecordVersion monotonic value, into a TRestBatch instance
    // - both remote Source and local TRestServer should have the supplied
    // Table class in each of their data model
    // - by default, all pending updates are retrieved, but you can define a value
    // to MaxRowLimit, so that the updates will be retrieved by smaller chunks
    // - returns nil if nothing new was found, or a TRestBatch instance
    // containing all modifications since RecordVersion revision
    // - when executing the returned TRestBatch on the database, you should
    // set TRestServer.RecordVersionDeleteIgnore := true so that the
    // TRecordVersion fields will be forced from the supplied value
    // - usually, you should not need to use this method, but rather the more
    // straightforward RecordVersionSynchronizeSlave()
    function RecordVersionSynchronizeSlaveToBatch(Table: TOrmClass;
      const Master: IRestOrm; var RecordVersion: TRecordVersion; MaxRowLimit: integer = 0;
      const OnWrite: TOnBatchWrite = nil): TRestBatch;

    /// check if the supplied TOrm is not a virtual or static table
    function IsInternalSQLite3Table(aTableIndex: integer): boolean;
    /// returns the maximum BLOB size per record as specified to TrackChanges()
    function MaxUncompressedBlobSize(Table: TOrmClass): integer;
    /// returns true if the server will handle per-user authentication and
    // access right management
    // - i.e. if the associated TOrmModel contains TAuthUser and
    // TAuthGroup tables (set by constructor)
    function HandleAuthentication: boolean;
    /// used by tests to set as false and force using SQlite3 virtual tables for
    // TOrmVirtualTableJson static tables (module JSON or BINARY)
    procedure SetStaticVirtualTableDirect(direct: boolean);
    /// create an external static redirection for a specific class
    // - call it just after Create, before IRestOrmServer.CreateMissingTables;
    // warning: if you don't call this method before CreateMissingTable method
    // is called, the table will be created as a regular table by the main
    // database engine, and won't be static
    // - the specified TOrm class will have all its CRUD / ORM methods be
    // redirected to aRemoteRest, which may be a TRestClient or another
    // TRestServer instance (e.g. a fast SQLITE_MEMORY_DATABASE_NAME)
    // - if aRemoteRest is a TRestClient, it should have been authenticated
    // to the remote TRestServer, so that CRUD / ORM operations will pass
    // - this will enable easy creation of proxies, or local servers, with they
    // own cache and data model - e.g. a branch office server which may serve
    // its local clients over Ethernet, but communicating to a main mORMot
    // server via Internet, storing the corporate data in the main office server
    // - you may also share some tables (e.g. TAuthUser and TAuthGroup)
    // between TRestServer instances in a single service
    // - returns a newly created TRestStorageRemote instance
    function RemoteDataCreate(aClass: TOrmClass;
      aRemoteRest: TRestOrmParent): TRestOrmParent; 
  end;


  { -------------------- TOrm Definitions }

  /// the possible options for handling table names in TOrmFill
  // - ctnTrimmed is set e.g. by TOrmMany.DestGetJoined
  TOrmCheckTableName = (
    ctnNoCheck,
    ctnMandatory,
    ctnTrimmed);

  /// used internally by TOrmFill for its field mapping
  TOrmFillTableMap = record
    /// the class instance to be filled from the TOrmTable
    // - can be a TOrmMany instance after FillPrepareMany() method call
    Dest: TOrm;
    /// the published property RTTI to be filled from the TOrmTable
    // - is nil for the RowID/ID field
    DestField: TOrmPropInfo;
    /// the column index in this TOrmTable
    TableIndex: integer;
  end;
  POrmFillTableMap = ^TOrmFillTableMap;

  /// internal data used by TOrm.FillPrepare()/FillPrepareMany() methods
  // - using a dedicated class will reduce memory usage for each TOrm
  // instance (which won't need these properties most of the time)
  TOrmFill = class
  protected
    /// associated table
    fTable: TOrmTable;
    /// current retrieved row
    fFillCurrentRow: integer;
    /// number of used items in TableMap[] array
    // - calculated in FillPrepare() or FillPrepareMany() methods
    fTableMapCount: integer;
    /// set by TOrm.FillPrepareMany() to release M.fDestID^ instances
    fTableMapRecordManyInstances: TOrmManyObjArray;
    /// map the published fields index
    // - calculated in FillPrepare() or FillPrepareMany() methods
    fTableMap: array of TOrmFillTableMap;
    /// mark all mapped or TModTime fields
    fTableMapFields: TFieldBits;
    /// if Joined instances were initialized via TOrm.CreateJoined()
    fJoinedFields: boolean;
    /// return fJoinedFields or false if self=nil
    function GetJoinedFields: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// add a property to the fTableMap[] array
    // - aIndex is the column index in TOrmTable
    procedure AddMap(aRecord: TOrm; aField: TOrmPropInfo;
      aIndex: integer);
    /// add a property to the fTableMap[] array
    // - aIndex is the column index in TOrmTable
    procedure AddMapFromName(aRecord: TOrm; aName: PUtf8Char; aIndex: integer);
    /// add all simple property names, with  to the fTableMap[] array
    // - will map ID/RowID, then all simple fields of this TOrm
    // - aIndex is the column index in TOrmTable
    procedure AddMapSimpleFields(aRecord: TOrm;
      const aProps: array of TOrmPropInfo; var aIndex: integer);
  public
    /// finalize the mapping
    destructor Destroy; override;
    /// map all columns of a TOrmTable to a record mapping
    // - this is the main entry point of this class
    procedure Map(aRecord: TOrm; aTable: TOrmTable; aCheckTableName: TOrmCheckTableName);
    /// reset the mapping
    // - is called e.g. by TOrm.FillClose
    // - will free any previous Table if necessary
    // - will release TOrmMany.Dest instances as set by TOrm.FillPrepareMany()
    procedure UnMap;
    /// fill a TOrm published properties from a TOrmTable row
    // - won't work with cross-reference mapping (FillPrepareMany)
    // - use the mapping prepared with Map() method
    // - can specify a destination record to be filled instead of main Dest
    function Fill(aRow: integer; aDest: TOrm = nil): boolean;
    /// compute the updated ORM field bits during a fill
    // - will return Props.SimpleFieldsBits[ooUpdate] if no fill is in process
    procedure ComputeSetUpdatedFieldBits(Props: TOrmProperties;
      out Bits: TFieldBits);
    /// compute the updated ORM field indexes during a fill
    // - set -1 for the ID/RowID or an unknown field
    procedure ComputeSetUpdatedFieldIndexes(var Props: TIntegerDynArray);
    /// retrieved the mapped information from the table field/column index
    // - returns nil if not found
    function TableFieldIndexToMap(TableField: integer): POrmFillTableMap;
    /// the TOrmTable stated as FillPrepare() parameter
    // - the internal temporary table is stored here for TOrmMany
    // - this instance is freed by TOrm.Destroy if fTable.OwnerMustFree=true
    property Table: TOrmTable
      read fTable;
    /// how many fields are currently mapped
    property TableMapCount: integer
      read fTableMapCount;
    /// the current Row during a Loop
    property FillCurrentRow: integer
      read fFillCurrentRow;
    /// equals TRUE if the instance was initialized via TOrm.CreateJoined()
    // TOrm.CreateAndFillPrepareJoined()
    // - it means that all nested TOrm are pre-allocated instances,
    // not trans-typed pointer(IDs)
    property JoinedFields: boolean
      read GetJoinedFields;
    /// set by TOrm.FillPrepareMany() to release M.fDestID^ instances
    property TableMapRecordManyInstances: TOrmManyObjArray
      read fTableMapRecordManyInstances;
    /// return all mapped fields, or [] if nil
    function TableMapFields: TFieldBits;
  end;

  /// root class for defining and mapping database records
  // - inherits a class from TOrm, and add published properties to describe
  // the table columns (see TPropInfo for SQL and Delphi type mapping/conversion)
  // - this published properties can be auto-filled from TOrmTable answer with
  // FillPrepare() and FillRow(), or FillFrom() with TOrmTable or JSON data
  // - these published properties can be converted back into UTF-8 encoded SQL
  // source with GetSqlValues or GetSqlSet or into JSON format with GetJsonValues
  // - BLOB fields are decoded to auto-freeing RawBlob properties
  // - any published property defined as a T*ObjArray dynamic array storage
  // of persistents (via Rtti.RegisterObjArray on Delphi 7-2009) will be freed
  // - consider inherit from TOrmNoCase and TOrmNoCaseExtended if
  // you expect regular NOCASE collation and smaller (but not standard JSON)
  // variant fields persistence
  TOrm = class(TObjectWithID)
  { note that every TOrm has an Instance size of 20 bytes (on 32-bit)
    for private and protected fields (such as fID or fFill e.g.) }
  protected
    /// used by FillPrepare() and corresponding Fill*() methods
    fFill: TOrmFill;
    /// internal properties getters (using fProps data for speed)
    function GetHasBlob: boolean;
    function GetSimpleFieldCount: integer;
    function GetFillCurrentRow: integer;
    function GetFillReachedEnd: boolean;
    function GetTable: TOrmTable;
    /// register RttiJsonRead/RttiJsonWrite callbacks for custom serialization
    class procedure RttiCustomSetParser(Rtti: TRttiCustom); override;
    /// fake nested TOrm classes would be serialized as integer
    function IsPropClassInstance(Prop: PRttiCustomProp): boolean; virtual;
    function RttiWritePropertyValue(W: TTextWriter; Prop: PRttiCustomProp;
      Options: TTextWriterWriteObjectOptions): boolean; override;
    function RttiBeforeReadPropertyValue(Ctxt: pointer;
      Prop: PRttiCustomProp): boolean; override;
    class procedure RttiJsonRead(var Context: TJsonParserContext; Instance: TObject);
    class procedure RttiJsonWrite(W: TJsonWriter; Instance: TObject;
      Options: TTextWriterWriteObjectOptions);
  protected
    fInternalState: cardinal;
    /// defined as a protected class function for OrmProps method inlining
    class function PropsCreate: TOrmProperties;
    /// virtual class method to be overridden to register some custom properties
    // - do nothing by default, but allow inherited classes to define some
    // properties, by adding some TOrmPropInfo instances to Props.Fields list,
    // or calling Props.RegisterCustomFixedSizeRecordProperty() or
    // Props.RegisterCustomRttiRecordProperty() methods
    // - can also be used to specify a custom text collation, by calling
    // Props.SetCustomCollationForAll() or SetCustomCollation() methods
    // - do not call OrmProps from here (e.g. by calling AddFilter*): it
    // would trigger a stack overflow, since at this state Props is not stored -
    // but rather use InternalDefineModel class method
    class procedure InternalRegisterCustomProperties(Props: TOrmProperties); virtual;
    /// virtual class method to be overridden to define some record-level modeling
    // - do nothing by default, but allow inherited classes to define some
    // process which will take place after TOrmProperties initialization
    // - this may be the place e.g. to call AddFilter*() methods, if you do not
    // want those to be written "in stone", and not manually when creating the
    // TOrmModel instance, or to call Props.SetCustomCollationForAll
    class procedure InternalDefineModel(Props: TOrmProperties); virtual;
    /// trick to get the ID even in case of a oftID published property
    function GetID: TID;
      {$ifdef OSWINDOWS}{$ifdef HASINLINE}inline;{$endif}{$endif}
    /// trick to typecast the ID on 64-bit platform
    function GetIDAsPointer: pointer;
      {$ifdef OSWINDOWS}{$ifdef HASINLINE}inline;{$endif}{$endif}
  public
    /// direct access to the TOrm properties from RTTI
    // - TOrmProperties is faster than e.g. the class function FieldProp()
    // - use internal the unused vmtAutoTable VMT entry to fast retrieve of a
    // class variable which is unique for each class ("class var" is unique only
    // for the class within it is defined, and we need a var for each class:
    // so even Delphi XE syntax is not powerful enough for our purpose, and the
    // vmtAutoTable trick if very fast, and works with all versions of Delphi -
    // including 64-bit target)
    class function OrmProps: TOrmProperties;
      {$ifdef HASINLINE}inline;{$endif}
    /// direct access to the TOrmProperties info of an existing TOrm instance
    // - same as OrmProps, but when we know that PropsCreate is never needed
    function Orm: TOrmProperties;
      {$ifdef HASINLINE}inline;{$endif}
    /// the Table name in the database, associated with this TOrm class
    // - 'TSql' or 'TOrm' chars are trimmed at the beginning of the ClassName
    // - or the ClassName is returned as is, if no 'TSql' or 'TOrm' at first
    // - is just a wrapper around OrmProps.SqlTableName field value
    class function SqlTableName: RawUtf8;
    /// register a custom filter (transformation) or validate to the
    // TOrm class for a specified field
    // - this will be used by TOrm.Filter and TOrm.Validate
    // methods (in default implementation)
    // - will raise an EModelException on failure
    // - this function is just a wrapper around OrmProps.AddFilterOrValidate
    class procedure AddFilterOrValidate(const aFieldName: RawUtf8;
      aFilter: TSynFilterOrValidate);
    /// register a TSynFilterTrim and a TSynValidateText filters so that
    // the specified fields, after space trimming, won't be void
    class procedure AddFilterNotVoidText(const aFieldNames: array of RawUtf8);
    /// register a TSynFilterTrim and a TSynValidateText filters so that
    // all text fields, after space trimming, won't be void
    // - will only affect RAWTEXT_FIELDS
    class procedure AddFilterNotVoidAllTextFields;

    /// protect several TOrm local variable instances
    // - WARNING: both FPC and Delphi 10.4+ don't keep the IAutoFree instance
    // up to the end-of-method -> you should not use TAutoFree for new projects :(
    // - specified as localVariable/recordClass pairs
    // - is a wrapper around TAutoFree.Several(...) constructor
    // - be aware that it won't implement a full ARC memory model, but may be
    // just used to avoid writing some try ... finally blocks on local variables
    // - use with caution, only on well defined local scope
    // - you may write for instance:
    // ! var info: TOrmBlogInfo;
    // !     article: TOrmArticle;
    // !     comment: TOrmComment;
    // ! begin
    // !   TOrm.AutoFree([ // avoid several try..finally
    // !     @info, TOrmBlogInfo, @article, TOrmArticle, @comment, TOrmComment]);
    // !   .... now you can use info, article or comment
    // ! end; // will call info.Free article.Free and comment.Free
    // - warning: under FPC, you should assign the result of this method to
    // a local IAutoFree variable, or use a with TOrm.AutoFree() do
    // statement - see http://bugs.freepascal.org/view.php?id=26602
    // - Delphi 10.4 also did change it and release the IAutoFree before the
    // end of the current method, and an "array of pointer" cannot be inlined
    // by the Delphi compiler, so you should explicitly call ForMethod:
    // !   TOrm.AutoFree([...]).ForMethod;
    class function AutoFree(varClassPairs: array of pointer): IAutoFree; overload;
    /// protect one TOrm local variable instance
    // - WARNING: both FPC and Delphi 10.4+ don't keep the IAutoFree instance
    // up to the end-of-method -> you should NOT use TAutoFree for new projects :(
    // - be aware that it won't implement a full ARC memory model, but may be
    // just used to avoid writing some try ... finally blocks on local variables
    // - use with caution, only on well defined local scope
    // - you may write for instance:
    // ! var info: TOrmBlogInfo;
    // ! begin
    // !   TOrmBlogInfo.AutoFree(info);
    // !   .... now you can use info
    // ! end; // will call info.Free
    // - warning: under FPC, you should assign the result of this method to
    // a local IAutoFree variable, or use a with TOrm.AutoFree() do
    // statement - see http://bugs.freepascal.org/view.php?id=26602
    class function AutoFree(var localVariable): IAutoFree; overload;
      {$ifdef ISDELPHI104} inline; {$endif}
    /// read and protect one TOrm local variable instance
    // - WARNING: both FPC and Delphi 10.4+ don't keep the IAutoFree instance
    // up to the end-of-method -> you should NOT use TAutoFree for new projects :(
    // - is a wrapper around TAutoFree.Create(localVariable,Create(Rest,ID))
    // - be aware that it won't implement a full ARC memory model, but may be
    // just used to avoid writing some try ... finally blocks on local variables
    // - use with caution, only on well defined local scope
    // - warning: under FPC, you should assign the result of this method to
    // a local IAutoFree variable, or use a with TOrm.AutoFree() do
    // statement - see http://bugs.freepascal.org/view.php?id=26602
    class function AutoFree(var localVariable; const Rest: IRestOrm; ID: TID): IAutoFree; overload;
      {$ifdef ISDELPHI104} inline; {$endif}
    /// FillPrepare and protect one TOrm local variable instance
    // - WARNING: both FPC and Delphi 10.4+ don't keep the IAutoFree instance
    // up to the end-of-method -> you should NOT use TAutoFree for new projects :(
    // - is a wrapper around TAutoFree.Create(localVariable,CreateAndFillPrepare(Rest,...))
    // - be aware that it won't implement a full ARC memory model, but may be
    // just used to avoid writing some try ... finally blocks on local variables
    // - use with caution, only on well defined local scope
    // - warning: under FPC, you should assign the result of this method to
    // a local IAutoFree variable, or use a with TOrm.AutoFree() do
    // statement - see http://bugs.freepascal.org/view.php?id=26602
    class function AutoFree(var localVariable; const Rest: IRestOrm;
      const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const;
      const aCustomFieldsCsv: RawUtf8 = ''): IAutoFree; overload;
    /// FillPrepare and protect one TOrm local variable instance
    // - WARNING: both FPC and Delphi 10.4+ don't keep the IAutoFree instance
    // up to the end-of-method -> you should NOT use TAutoFree for new projects :(
    // - is a wrapper around TAutoFree.Create(localVariable,CreateAndFillPrepare(Rest,...))
    // - be aware that it won't implement a full ARC memory model, but may be
    // just used to avoid writing some try ... finally blocks on local variables
    // - use with caution, only on well defined local scope
    // - warning: under FPC, you should assign the result of this method to
    // a local IAutoFree variable, or use a with TOrm.AutoFree() do
    // statement - see http://bugs.freepascal.org/view.php?id=26602
    class function AutoFree(var localVariable; const Rest: IRestOrm;
      const FormatSqlWhere: RawUtf8; const ParamsSqlWhere, BoundsSqlWhere: array of const;
      const aCustomFieldsCsv: RawUtf8 = ''): IAutoFree; overload;

    /// called when the associated table is created in the database
    // - if FieldName is '', initialization regarding all fields must be made;
    // if FieldName is specified, initialization regarding this field must be processed
    // - override this method in order to initialize indexs or create default records
    // - by default, create indexes for all TRecordReference properties, and
    // for all TOrm inherited properties (i.e. of oftID type, that is
    // an INTEGER field containing the ID of the pointing record)
    // - the options specified at CreateMissingTables() are passed to this method,
    // within the context of an opened DB transaction, in which missing tables
    // and fields have already been added
    // - is not part of TOrmProperties because has been declared as virtual
    class procedure InitializeTable(const Server: IRestOrmServer;
      const FieldName: RawUtf8; Options: TOrmInitializeTableOptions); virtual;

    /// filter/transform the specified fields values of the TOrm instance
    // - by default, this will perform all TSynFilter as registered by
    // [OrmProps.]AddFilterOrValidate()
    // - inherited classes may add some custom filtering/transformation here, if
    // it's not needed nor mandatory to create a new TSynFilter class type: in
    // this case, the function has to return TRUE if the filtering took place,
    // and FALSE if any default registered TSynFilter must be processed
    // - the default aFields parameter will process all fields
    function Filter(const aFields: TFieldBits = [0..MAX_SQLFIELDS - 1]): boolean;
      overload; virtual;
    ///  filter/transform the specified fields values of the TOrm instance
    // - this version will call the overloaded Filter() method above
    // - return TRUE if all field names were correct and processed, FALSE otherwise
    function Filter(const aFields: array of PUtf8Char): boolean; overload;
    /// validate the specified fields values of the current TOrm instance
    // - by default, this will perform all TSynValidate as registered by
    //  [OrmProps.]AddFilterOrValidate()
    // - it will also check if any UNIQUE field value won't be duplicated
    // - inherited classes may add some custom validation here, if it's not needed
    //  nor mandatory to create a new TSynValidate class type: in this case, the
    //  function has to return an explicit error message (as a generic VCL string)
    //  if the custom validation failed, or '' if the validation was successful:
    //  in this later case, all default registered TSynValidate are processed
    // - the default aFields parameter will process all fields
    // - if aInvalidFieldIndex is set, it will contain the first invalid field
    //  index found
    // - caller SHOULD always call the Filter() method before calling Validate()
    function Validate(const aRest: IRestOrm;
      const aFields: TFieldBits = [0.. MAX_SQLFIELDS - 1];
      aInvalidFieldIndex: PInteger = nil; aValidator: PSynValidate = nil): string;
        overload; virtual;
    ///  validate the specified fields values of the current TOrm instance
    // - this version will call the overloaded Validate() method above
    // - returns '' if all field names were correct and processed, or an
    // explicit error message (translated in the current language) on error
    // - if aInvalidFieldIndex is set, it will contain the first invalid field index
    function Validate(const aRest: IRestOrm; const aFields: array of PUtf8Char;
      aInvalidFieldIndex: PInteger = nil; aValidator: PSynValidate = nil): string; overload;
    /// filter (transform) then validate the specified fields values of the TOrm
    // - this version will call the overloaded Filter() and Validate() methods
    // and display the faulty field name at the beginning of the error message
    // - returns true if all field names were correct and processed, or false
    // and an explicit error message (translated in the current language) on error
    function FilterAndValidate(const aRest: IRestOrm; out aErrorMessage: string;
      const aFields: TFieldBits = [0..MAX_SQLFIELDS - 1];
      aValidator: PSynValidate = nil): boolean; overload;
    /// filter (transform) then validate the specified fields values of the TOrm
    // - this version will call the overloaded Filter() and Validate() methods
    // and return '' on validation success, or an error message with the faulty
    // field names at the beginning
    function FilterAndValidate(const aRest: IRestOrm;
      const aFields: TFieldBits = [0..MAX_SQLFIELDS - 1];
      aValidator: PSynValidate = nil): RawUtf8; overload;
    /// should modify the record content before writing to the Server
    // - this default implementation will update any oftModTime / TModTime,
    // oftCreateTime / TCreateTime and oftSessionUserID / TSessionUserID
    // properties content with the exact server time stamp
    // - you may override this method e.g. for custom calculated fields
    // - note that this is computed only on the Client side, before sending
    // back the content to the remote Server: therefore, TModTime / TCreateTime
    // fields are a pure client ORM feature - it won't work directly at REST level
    procedure ComputeFieldsBeforeWrite(const aRest: IRestOrm;
      aOccasion: TOrmEvent; aServerTimeStamp: TTimeLog = 0); virtual;

    /// this constructor initializes the ORM record
    // - auto-instanciate any TOrmMany instance defined in published properties
    // - override this method if you want to use some internal objects (e.g.
    // TStringList or TCollection as published property)
    constructor Create; overload; override;
    /// this constructor initializes the ORM record and set the simple fields
    // with the supplied values
    // - the aSimpleFields parameters must follow explicitly the order of
    // published properties of the aTable class, excepting the RawBlob and
    // TOrmMany kind (i.e. only so called "simple fields") - in
    // particular, parent properties must appear first in the list
    // - the aSimpleFields must have exactly the same count of parameters as
    // there are "simple fields" in the published properties
    // - will raise an EOrmException in case of wrong supplied values
    constructor Create(const aSimpleFields: array of const; aID: TID); reintroduce; overload;
    /// this constructor initializes the object as above, and fills its content
    // from a client or server connection
    // - if ForUpdate is true, the REST method is LOCK and not GET: it tries to lock
    // the corresponding record, then retrieve its content; caller has to call
    // UnLock() method after Value usage, to release the record
    constructor Create(const aClient: IRestOrm; aID: TID;
      ForUpdate: boolean = false); reintroduce; overload;
    /// this constructor initializes the object and fills its content from a client
    // or server connection, from a TOrm published property content
    // - is just a wrapper around Create(aClient,PtrInt(aPublishedRecord))
    // or Create(aClient,aPublishedRecord.ID)
    // - a published TOrm property is not a class instance, but a typecast to
    // TObject(RecordID) - you can also use its ID property
    // - if ForUpdate is true, the REST method is LOCK and not GET: it tries to lock
    // the corresponding record, then retrieve its content; caller has to call
    // UnLock() method after Value usage, to release the record
    constructor Create(const aClient: IRestOrm; aPublishedRecord: TOrm;
      ForUpdate: boolean = false); reintroduce; overload;
    /// this constructor initializes the object as above, and fills its content
    //  from a client or server connection, using a specified WHERE clause
    //  - the WHERE clause should use inlined parameters (like 'Name=:('Arnaud'):')
    //  for better server speed - note that you can use FormatUtf8() as such:
    //  ! aRec := TOrmMyRec.Create(Client,FormatUtf8('Salary>? AND Salary<?',[],[1000,2000]));
    //  or call the overloaded contructor with BoundsSqlWhere array of parameters
    constructor Create(const aClient: IRestOrm; const aSqlWhere: RawUtf8);
      reintroduce; overload;
    /// this constructor initializes the object as above, and fills its content
    // from a client or server connection, using a specified WHERE clause
    // with parameters
    // - for better server speed, the WHERE clause should use bound parameters
    // identified as '?' in the FormatSqlWhere statement, which is expected to
    // follow the order of values supplied in BoundsSqlWhere open array - use
    // DateToSql/DateTimeToSql for TDateTime, or directly any integer / double /
    // currency / RawUtf8 values to be bound to the request as parameters
    // - note that this method prototype changed with revision 1.17 of the
    // framework: array of const used to be ParamsSqlWhere and '%' in the
    // FormatSqlWhere statement, whereas it now expects bound parameters as '?'
    constructor Create(const aClient: IRestOrm; const FormatSqlWhere: RawUtf8;
      const BoundsSqlWhere: array of const); reintroduce; overload;
    /// this constructor initializes the object as above, and fills its content
    // from a client or server connection, using a specified WHERE clause
    // with parameters
    // - the FormatSqlWhere clause will replace all '%' chars with the supplied
    // ParamsSqlWhere[] values, and all '?' chars with BoundsSqlWhere[] values,
    // as :(...): inlined parameters - you should either call:
    // ! Rec := TOrmMyRecord.Create(aClient, 'Count=:(%):', [aCount],[]);
    // or (letting the inlined parameters being computed by FormatUtf8)
    // !  Rec := TOrmMyRecord.Create(aClient,'Count=?',[],[aCount]);
    // or even better, using the other Create overloaded constructor:
    // !  Rec := TOrmMyRecord.Create(aClient,'Count=?',[aCount]);
    // - using '?' and BoundsSqlWhere[] is perhaps more readable in your code, and
    // will in all case create a request with :(..): inline parameters, with
    // automatic RawUtf8 quoting if necessary
    constructor Create(const aClient: IRestOrm; const FormatSqlWhere: RawUtf8;
      const ParamsSqlWhere, BoundsSqlWhere: array of const); reintroduce; overload;
    /// this constructor initializes the object as above, and fills its content
    // from a supplied JSON content
    // - is a wrapper around Create + FillFrom() methods
    // - use JSON data, as exported by GetJsonValues(), expanded or not
    // - make an internal copy of the JSONTable RawUtf8 before calling
    // FillFrom() below
    constructor CreateFrom(const JsonRecord: RawUtf8); overload;
    /// this constructor initializes the object as above, and fills its content
    // from a supplied JSON buffer
    // - is a wrapper around Create + FillFrom() methods
    // - use JSON data, as exported by GetJsonValues(), expanded or not
    // - the data inside P^ is modified (unescaped and transformed in-place):
    // don't call CreateFrom(pointer(JsonRecord)) but CreateFrom(JsonRecord) which
    // makes a temporary copy of the JsonRecord text variable before parsing
    constructor CreateFrom(P: PUtf8Char); overload;
    /// this constructor initializes the object as above, and fills its content
    // from a supplied TDocVariant object document
    // - is a wrapper around Create + FillFrom() methods
    constructor CreateFrom(const aDocVariant: variant); overload;

    /// this constructor initializes the object as above, and prepares itself to
    // loop through a statement using a specified WHERE clause
    // - this method creates a TOrmTableJson, retrieves all records corresponding
    // to the WHERE clause, then call FillPrepare - previous Create(aClient)
    // methods retrieve only one record, this one more multiple rows
    // - you should then loop for all rows using 'while Rec.FillOne do ...'
    // - the TOrmTableJson will be freed by TOrm.Destroy
    // - the WHERE clause should use inlined parameters (like 'Name=:('Arnaud'):')
    // for better server speed - note that you can use FormatUtf8() as such:
    // ! aRec := TOrmMyRec.CreateAndFillPrepare(Client,FormatUtf8('Salary>? AND Salary<?',[],[1000,2000]));
    // or call the overloaded CreateAndFillPrepare() contructor directly with
    // BoundsSqlWhere array of parameters
    // - aCustomFieldsCsv can be used to specify which fields must be retrieved
    // - default aCustomFieldsCsv='' will retrieve all simple table fields
    // - if aCustomFieldsCsv='*', it will retrieve all fields, including BLOBs
    // - aCustomFieldsCsv can also be set to a CSV field list to retrieve only
    // the needed fields, and save remote bandwidth - note that any later
    // Update() will update all simple fields, so potentially with wrong
    // values; but BatchUpdate() can be safely used since it will
    constructor CreateAndFillPrepare(const aClient: IRestOrm;
      const aSqlWhere: RawUtf8; const aCustomFieldsCsv: RawUtf8 = ''); overload;
    /// this constructor initializes the object as above, and prepares itself to
    // loop through a statement using a specified WHERE clause
    // - this method creates a TOrmTableJson, retrieves all records corresponding
    // to the WHERE clause, then call FillPrepare - previous Create(aClient)
    // methods retrieve only one record, this one more multiple rows
    // - you should then loop for all rows using 'while Rec.FillOne do ...'
    // - the TOrmTableJson will be freed by TOrm.Destroy
    // - for better server speed, the WHERE clause should use bound parameters
    // identified as '?' in the FormatSqlWhere statement, which is expected to
    // follow the order of values supplied in BoundsSqlWhere open array - use
    // DateToSql/DateTimeToSql for TDateTime, or directly any integer / double /
    // currency / RawUtf8 values to be bound to the request as parameters
    // - note that this method prototype changed with revision 1.17 of the
    // framework: array of const used to be ParamsSqlWhere and '%' in the
    // FormatSqlWhere statement, whereas it now expects bound parameters as '?'
    // - aCustomFieldsCsv can be used to specify which fields must be retrieved
    // - default aCustomFieldsCsv='' will retrieve all simple table fields, but
    // you may need  to access only one or several fields, and will save remote
    // bandwidth by specifying the needed fields
    // - if aCustomFieldsCsv='*', it will retrieve all fields, including BLOBs
    // - note that you should not use this aCustomFieldsCsv optional parameter if
    // you want to Update the retrieved record content later, since any
    // missing fields will be left with previous values - but BatchUpdate() can be
    // safely used after FillPrepare (will set only ID, TModTime and mapped fields)
    constructor CreateAndFillPrepare(const aClient: IRestOrm;
      const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const;
      const aCustomFieldsCsv: RawUtf8 = ''); overload;
    /// this constructor initializes the object as above, and prepares itself to
    // loop through a statement using a specified WHERE clause
    // - this method creates a TOrmTableJson, retrieves all records corresponding
    // to the WHERE clause, then call FillPrepare - previous Create(aClient)
    // methods retrieve only one record, this one more multiple rows
    // - you should then loop for all rows using 'while Rec.FillOne do ...'
    // - the TOrmTableJson will be freed by TOrm.Destroy
    // - the FormatSqlWhere clause will replace all '%' chars with the supplied
    // ParamsSqlWhere[] supplied values, and bind all '?' chars as parameters
    // with BoundsSqlWhere[] values
    // - aCustomFieldsCsv can be used to specify which fields must be retrieved
    // - default aCustomFieldsCsv='' will retrieve all simple table fields, but
    // you may need  to access only one or several fields, and will save remote
    // bandwidth by specifying the needed fields
    // - if aCustomFieldsCsv='*', it will retrieve all fields, including BLOBs
    // - note that you should not use this aCustomFieldsCsv optional parameter if
    // you want to Update the retrieved record content later, since any
    // missing fields will be left with previous values - but BatchUpdate() can be
    // safely used after FillPrepare (will set only ID, TModTime and mapped fields)
    constructor CreateAndFillPrepare(const aClient: IRestOrm;
      const FormatSqlWhere: RawUtf8; const ParamsSqlWhere, BoundsSqlWhere: array of const;
      const aCustomFieldsCsv: RawUtf8 = ''); overload;
    /// this constructor initializes the object as above, and prepares itself to
    // loop through a given list of IDs
    // - this method creates a TOrmTableJson, retrieves all records corresponding
    // to the specified IDs, then call FillPrepare - previous Create(aClient)
    // methods retrieve only one record, this one more multiple rows
    // - you should then loop for all rows using 'while Rec.FillOne do ...'
    // - the TOrmTableJson will be freed by TOrm.Destroy
    // - aCustomFieldsCsv can be used to specify which fields must be retrieved
    // - default aCustomFieldsCsv='' will retrieve all simple table fields, but
    // you may need  to access only one or several fields, and will save remote
    // bandwidth by specifying the needed fields
    // - if aCustomFieldsCsv='*', it will retrieve all fields, including BLOBs
    // - note that you should not use this aCustomFieldsCsv optional parameter if
    // you want to Update the retrieved record content later, since any
    // missing fields will be left with previous values - but BatchUpdate() can be
    // safely used after FillPrepare (will set only ID, TModTime and mapped fields)
    constructor CreateAndFillPrepare(const aClient: IRestOrm;
      const aIDs: array of Int64; const aCustomFieldsCsv: RawUtf8 = ''); overload;
    /// this constructor initializes the object, and prepares itself to loop
    // through a specified JSON table, which will use a private copy
    // - this method creates a TOrmTableJson, fill it with the supplied JSON buffer,
    // then call FillPrepare - previous Create(aClient) methods retrieve only
    // one record, this one more multiple rows
    // - you should then loop for all rows using 'while Rec.FillOne do ...'
    // - the TOrmTableJson will be freed by TOrm.Destroy
    constructor CreateAndFillPrepare(const aJson: RawUtf8); overload;
    /// this constructor initializes the object, and prepares itself to loop
    // through a specified JSON table buffer, which will be modified in-place
    // - this method creates a TOrmTableJson, fill it with the supplied JSON buffer,
    // then call FillPrepare - previous Create(aClient) methods retrieve only
    // one record, this one more multiple rows
    // - you should then loop for all rows using 'while Rec.FillOne do ...'
    // - the TOrmTableJson will be freed by TOrm.Destroy
    constructor CreateAndFillPrepare(aJson: PUtf8Char; aJsonLen: integer); overload;
    /// this constructor initializes the object from its ID, including all
    // nested TOrm properties, through a JOINed statement
    // - by default, Create(aClient,aID) will return only the one-to-one
    // nested TOrm published properties IDs trans-typed as pointer - this
    // constructor allow to retrieve the nested values in one statement
    // - use this constructor if you want all TOrm published properties to
    // be allocated, and loaded with the corresponding values
    // - Free/Destroy will release these instances
    // - warning: if you call Update() after it, only the main object will be
    // updated, not the nested TOrm properties
    constructor CreateJoined(const aClient: IRestOrm; aID: TID);
    /// this constructor initializes the object, and prepares itself to loop
    // nested TOrm properties, through a JOINed statement and a WHERE clause
    // - by default, CreateAndFillPrepare() will return only the one-to-one
    // nested TOrm published properties IDs trans-typed as pointer - this
    // constructor allow to retrieve the nested values in one statement
    //  - this method creates a TOrmTableJson, fill it with the supplied JSON buffer,
    // then call FillPrepare - previous CreateJoined() method retrieve only
    // one record, this one more multiple rows
    // - you should then loop for all rows using 'while Rec.FillOne do ...'
    // - use this constructor if you want all TOrm published properties to
    // be allocated, and loaded with the corresponding values
    // - Free/Destroy will release these instances
    // - warning: if you call Update() after it, only the main object will be
    // updated, not the nested TOrm properties
    constructor CreateAndFillPrepareJoined(const aClient: IRestOrm;
      const aFormatSQLJoin: RawUtf8; const aParamsSQLJoin, aBoundsSQLJoin: array of const);
    /// this constructor initializes the object including all TOrmMany properties,
    // and prepares itself to loop through a JOINed statement
    // - the created instance will have all its TOrmMany Dest property allocated
    // with proper instance (and not only pointer(DestID) e.g.), ready to be
    // consumed during a while FillOne do... loop (those instances will be
    // freed by TOrm.FillClose or Destroy) - and the Source property
    // won't contain pointer(SourceID) but the main TOrm instance
    // - the aFormatSQLJoin clause will define a WHERE clause for an automated
    // JOINed statement, including TOrmMany published properties (and
    // their nested properties)
    // - a typical use could be the following:
    // ! aProd := TOrmProduct.CreateAndFillPrepareMany(Database,
    // !   'Owner=? and Categories.Dest.Name=? and (Sizes.Dest.Name=? or Sizes.Dest.Name=?)',
    // !   [], ['mark', 'for boy', 'small', 'medium']);
    // ! if aProd <> nil then
    // ! try
    // !   while aProd.FillOne do
    // !     //  here e.g. aProd.Categories.Dest are instantied (and Categories.Source=aProd)
    // !     writeln(aProd.Name, ' ', aProd.Owner, ' ', aProd.Categories.Dest.Name,
    // !       ' ', aProd.Sizes.Dest.Name);
    // !   //  you may also use aProd.FillTable to fill a grid, e.g.
    // !   //  (do not forget to set aProd.FillTable.OwnerMustFree := false)
    // ! finally
    // !   aProd.Free; //  will also free aProd.Categories/Sizes instances
    // ! end;
    // this will execute a JOINed SELECT statement similar to the following:
    // $ select p.*, c.*, s.*
    // $ from Product p, Category c, Categories cc, Size s, Sizes ss
    // $ where c.id=cc.dest and cc.source=p.id and
    // $  s.id=ss.dest and ss.source=p.id and
    // $  p.Owner='mark' and c.Name='for boy' and (s.Name='small' or s.Name='medium')
    // - you SHALL call explicitly the FillClose method before using any
    // methods of nested TOrmMany instances which may override the Dest
    // instance content (e.g. ManySelect) to avoid any GPF
    // - the aFormatSQLJoin clause will replace all '%' chars with the supplied
    // aParamsSQLJoin[] supplied values, and bind all '?' chars as bound
    // parameters with aBoundsSQLJoin[] values
    constructor CreateAndFillPrepareMany(const aClient: IRestOrm;
      const aFormatSQLJoin: RawUtf8; const aParamsSQLJoin, aBoundsSQLJoin: array of const);
    /// this method create a clone of the current record, with same ID and properties
    // - copy all COPIABLE_FIELDS, i.e. all fields excluding tftMany (because
    // those fields don't contain any data, but a TOrmMany instance
    // which allow to access to the pivot table data)
    // - you can override this method to allow custom copy of the object,
    // including (or not) published properties copy
    function CreateCopy: TOrm; overload; virtual;
    /// this method create a clone of the current record, with same ID and properties
    // - overloaded method to copy the specified properties
    function CreateCopy(const CustomFields: TFieldBits): TOrm; overload;
    /// set the bits corresponding to non-void (0,'') copiable fields
    function GetNonVoidFields: TFieldBits;
    /// release the associated memory
    // - in particular, release all TOrmMany instance created by the
    // constructor of this TOrm
    destructor Destroy; override;

    /// return the UTF-8 encoded SQL source to create the table containing the
    // published fields of a TOrm child
    // - a 'ID INTEGER PRIMARY KEY' field is always created first (mapping
    // SQLite3 RowID)
    // - AnsiString are created as TEXT COLLATE NOCASE (fast SQLite3 7bits compare)
    // - RawUnicode and RawUtf8 are created as TEXT COLLATE SYSTEMNOCASE
    // (i.e. use our fast Utf8IComp() for comparaison)
    // - TDateTime are created as TEXT COLLATE ISO8601
    // (which calls our very fast ISO TEXT to Int64 conversion routine)
    // - an individual bit set in UniqueField forces the corresponding field to
    // be marked as UNIQUE (an unique index is automaticaly created on the specified
    // column); use TOrmModel fIsUnique[] array, which set the bits values
    // to 1 if a property field was published with "stored AS_UNIQUE"
    // (i.e. "stored false")
    // - this method will handle TOrmFts* classes like FTS* virtual tables,
    // TOrmRTree as RTREE virtual table, and TOrmVirtualTable*ID
    // classes as corresponding Delphi designed virtual tables
    // - is not part of TOrmProperties because has been declared as virtual
    // so that you could specify a custom SQL statement, per TOrm type
    // - anyway, don't call this method directly, but use TOrmModel.GetSqlCreate()
    // - the aModel parameter is used to retrieve the Virtual Table module name,
    // and can be ignored for regular (not virtual) tables
    class function GetSqlCreate(aModel: TOrmModel): RawUtf8; virtual;
    /// return the Class Type of the current TOrm
    function RecordClass: TOrmClass;
      {$ifdef HASINLINE}inline;{$endif}
    /// return the RTTI property information for this record
    function ClassProp: TRttiJson;
      {$ifdef HASINLINE}inline;{$endif}
    /// return the TRecordReference Int64 value pointing to this record
    function RecordReference(Model: TOrmModel): TRecordReference;

    /// return the UTF-8 encoded SQL source to INSERT the values contained
    // in the current published fields of a TOrm child
    // - only simple fields name (i.e. not RawBlob/TOrmMany) are updated:
    // BLOB fields are ignored (use direct update via dedicated methods instead)
    // - format is '(COL1, COL2) VALUES ('VAL1', 'VAL2')' if some column was ignored
    // (BLOB e.g.)
    // - format is 'VALUES ('VAL1', 'VAL2')' if all columns values are available
    // - is not used by the ORM (do not use prepared statements) - only here
    // for conveniency
    function GetSqlValues: RawUtf8;
    /// return the UTF-8 encoded SQL source to UPDATE the values contained
    // in the current published fields of a TOrm child
    // - only simple fields name (i.e. not RawBlob/TOrmMany) are retrieved:
    // BLOB fields are ignored (use direct access via dedicated methods instead)
    // - format is 'COL1='VAL1', COL2='VAL2''
    // - is not used by the ORM (do not use prepared statements) - only here
    // for conveniency
    function GetSqlSet: RawUtf8;
    /// return the UTF-8 encoded JSON objects for the values of this TOrm
    // - layout and fields should have been set at TOrmWriter construction:
    // to append some content to an existing TOrmWriter, call the
    // AppendAsJsonObject() method
    procedure GetJsonValues(W: TOrmWriter); overload;
    /// return the UTF-8 encoded JSON objects for the values of this TOrm
    // - the JSON buffer will be finalized if needed (e.g. non expanded mode),
  	// and the supplied TOrmWriter instance will be freed by this method
    // - layout and fields should have been set at TOrmWriter construction:
    // to append some content to an existing TOrmWriter, call the
    // AppendAsJsonObject() method
    procedure GetJsonValuesAndFree(Json: TOrmWriter); overload;
    /// return the UTF-8 encoded JSON objects for the values contained
    // in the current published fields of a TOrm child
    // - only simple fields (i.e. not RawBlob/TOrmMany) are retrieved:
    //   BLOB fields are ignored (use direct access via dedicated methods instead)
    // - if Expand is true, JSON data is an object, for direct use with any Ajax or .NET client:
    // $ {"col1":val11,"col2":"val12"}
    // - if Expand is false, JSON data is serialized (as used in TOrmTableJson)
    // $ { "fieldCount":1,"values":["col1","col2",val11,"val12",val21,..] }
    // - if withID is true, then the first ID field value is included
    // - you can customize OrmOptions, e.g. if oftObject/oftBlobDynArray
    // property instance will be serialized as a JSON object or array, not a
    // JSON string (which is the default, as expected by the database storage),
    // or if an "ID_str" string field should be added for JavaScript
    procedure GetJsonValues(Json: TStream; Expand, withID: boolean;
      Occasion: TOrmOccasion; OrmOptions: TOrmWriterOptions = []); overload;
    /// same as overloaded GetJsonValues(), but returning result into a RawUtf8
    // - if UsingStream is not set, it will use a temporary TRawByteStringStream
    function GetJsonValues(Expand, withID: boolean;
      Occasion: TOrmOccasion; UsingStream: TRawByteStringStream = nil;
      OrmOptions: TOrmWriterOptions = []): RawUtf8; overload;
    /// same as overloaded GetJsonValues(), but allowing to set the fields to
    // be retrieved, and returning result into a RawUtf8
    function GetJsonValues(Expand, withID: boolean; const Fields: TFieldBits;
      OrmOptions: TOrmWriterOptions = []): RawUtf8; overload;
    /// same as overloaded GetJsonValues(), but allowing to set the fields to
    // be retrieved, and returning result into a RawUtf8
    function GetJsonValues(Expand, withID: boolean; const FieldsCsv: RawUtf8;
      OrmOptions: TOrmWriterOptions = []): RawUtf8; overload;
    /// will append the record fields as an expanded JSON object
    // - GetJsonValues() will expect a dedicated TOrmWriter, whereas this
    // method will add the JSON object directly to any TOrmWriter
    // - by default, will append the simple fields, unless the Fields optional
    // parameter is customized to a non void value
    procedure AppendAsJsonObject(W: TOrmWriter; Fields: TFieldBits);
    /// will append all the FillPrepare() records as an expanded JSON array
    // - generates '[{rec1},{rec2},...]' using a loop similar to:
    // ! while FillOne do .. AppendJsonObject() ..
    // - if FieldName is set, the JSON array will be written as a JSON property,
    // i.e. surrounded as '"FieldName":[....],' - note the ',' at the end
    // - by default, will append the simple fields, unless the Fields optional
    // parameter is customized to a non void value
    // - see also IRestOrm.AppendListAsJsonArray for a high-level wrapper method
    procedure AppendFillAsJsonArray(const FieldName: RawUtf8; W: TOrmWriter;
      const Fields: TFieldBits = []);
    /// change TDocVariantData.Options for all variant published fields
    // - may be used to replace e.g. JSON_FAST_EXTENDED by JSON_FAST
    procedure ForceVariantFieldsOptions(aOptions: TDocVariantOptions = JSON_FAST);
    /// write the field values into the binary buffer
    // - won't write the ID field (should be stored before, with the Count e.g.)
    procedure GetBinaryValues(W: TBufferWriter); overload;
    /// write the field values into the binary buffer
    // - won't write the ID field (should be stored before, with the Count e.g.)
    procedure GetBinaryValues(W: TBufferWriter; const aFields: TFieldBits);
      overload;
    /// write the simple field values (excluding ID) into the binary buffer
    procedure GetBinaryValuesSimpleFields(W: TBufferWriter);
    /// set the field values from a binary buffer
    // - won't read the ID field (should be read before, with the Count e.g.):
    // use SetBinary() to read all fields including the ID value
    procedure SetBinaryValues(var Read: TFastReader);
    /// set the simple field values from a binary buffer
    // - won't read the ID field (should be read before, with the Count e.g.):
    // use SetBinary() to read all fields including the ID value
    procedure SetBinaryValuesSimpleFields(var Read: TFastReader);
    /// write the record fields into RawByteString a binary buffer
    // - same as GetBinaryValues(), but also writing the ID field first
    function GetBinary: RawByteString;
    /// set the record fields from a binary buffer saved by GetBinary()
    // - same as SetBinaryValues(), but also reading the ID field first
    procedure SetBinary(var Read: TFastReader); overload;
    /// set the record fields from a binary buffer saved by GetBinary()
    // - same as SetBinaryValues(), but also reading the ID field first
    procedure SetBinary(const binary: RawByteString); overload;
    /// set all field values from a supplied array of TSqlVar values
    // - Values[] array must match the OrmProps.Field[] order: will return
    // false if the Values[].VType does not match OrmProps.FieldType[]
    function SetFieldSqlVars(const Values: TSqlVarDynArray): boolean;
    /// retrieve a field value from a given property name, as encoded UTF-8 text
    // - you should use strong typing and direct property access, following
    // the ORM approach of the framework; but in some cases (a custom Grid
    // display, for instance), it could be useful to have this method available
    // - will return '' in case of wrong property name
    // - BLOB and dynamic array fields are returned as '\uFFF0base64encodedbinary'
    function GetFieldValue(const PropName: RawUtf8): RawUtf8;
    /// set a field value of a given property name, from some encoded UTF-8 text
    // - you should use strong typing and direct property access, following
    // the ORM approach of the framework; but in some cases (a custom Grid
    // display, for instance), it could be useful to have this method available
    // - won't do anything in case of wrong property name
    // - expect BLOB and dynamic array fields encoded as SQlite3 BLOB literals
    // ("x'01234'" e.g.) or '\uFFF0base64encodedbinary'
    procedure SetFieldValue(const PropName: RawUtf8; Value: PUtf8Char; ValueLen: PtrInt);
    /// retrieve the record content as a TDocVariant custom variant object
    function GetAsDocVariant(withID: boolean; const withFields: TFieldBits;
      options: PDocVariantOptions = nil; replaceRowIDWithID: boolean = false): variant; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// retrieve the record content as a TDocVariant custom variant object
    procedure GetAsDocVariant(withID: boolean; const withFields: TFieldBits;
      var result: variant; options: PDocVariantOptions = nil;
      ReplaceRowIDWithID: boolean = false); overload;
    /// retrieve the simple record content as a TDocVariant custom variant object
    function GetSimpleFieldsAsDocVariant(withID: boolean = true;
      options: PDocVariantOptions = nil): variant;
    /// retrieve the published property value into a Variant
    // - will set the Variant type to the best matching kind according to the
    // property type
    // - will return a null variant in case of wrong property name
    // - BLOB fields are returned as SQlite3 BLOB literals ("x'01234'" e.g.)
    // - dynamic array fields are returned as a Variant array
    function GetFieldVariant(const PropName: string): Variant;
    /// set the published property value from a Variant value
    // - will convert from the variant type into UTF-8 text before setting the
    // value (so will work with any kind of Variant)
    // - won't do anything in case of wrong property name
    // - expect BLOB fields encoded as SQlite3 BLOB literals ("x'01234'" e.g.)
    procedure SetFieldVariant(const PropName: string; const Source: Variant);

    /// prepare to get values from a TOrmTable result
    // - then call FillRow(1..Table.RowCount) to get any row value
    // - or you can also loop through all rows with
    // ! while Rec.FillOne do
    // !   dosomethingwith(Rec);
    // - the specified TOrmTable is stored in an internal fTable protected field
    // - set aCheckTableName if you want e.g. the Field Names from the Table
    // any pending 'TableName.' trimmed before matching to the current record
    procedure FillPrepare(Table: TOrmTable;
      aCheckTableName: TOrmCheckTableName = ctnNoCheck); overload;
    /// prepare to get values from a SQL where statement
    // - returns true in case of success, false in case of an error during SQL request
    // - then call FillRow(1..Table.RowCount) to get any row value
    // - or you can also loop through all rows with
    // ! while Rec.FillOne do
    // !   dosomethingwith(Rec);
    // - a temporary TOrmTable is created then stored in an internal fTable protected field
    // - if aSqlWhere is left to '', all rows are retrieved as fast as possible
    // (e.g. by-passing SQLite3 virtual table modules for external databases)
    // - the WHERE clause should use inlined parameters (like 'Name=:('Arnaud'):')
    // for better server speed - note that you can use FormatUtf8() as such:
    // ! aRec.FillPrepare(Client, FormatUtf8('Salary>? AND Salary<?', [], [1000, 2000]));
    // or call the overloaded FillPrepare() method directly with  BoundsSqlWhere
    // array of parameters
    // - aCustomFieldsCsv can be used to specify which fields must be retrieved
    // - default aCustomFieldsCsv='' will retrieve all simple table fields, but
    // you may need  to access only one or several fields, and will save remote
    // bandwidth by specifying the needed fields
    // - if aCustomFieldsCsv='*', it will retrieve all fields, including BLOBs
    // - note that you should not use this aCustomFieldsCsv optional parameter if
    // you want to Update the retrieved record content later, since any
    // missing fields will be left with previous values - but BatchUpdate() can be
    // safely used after FillPrepare (will set only ID, TModTime and mapped fields)
    function FillPrepare(const aClient: IRestOrm;
      const aSqlWhere: RawUtf8 = ''; const aCustomFieldsCsv: RawUtf8 = '';
      aCheckTableName: TOrmCheckTableName = ctnNoCheck): boolean; overload;
    /// prepare to get values using a specified WHERE clause with '%' parameters
    // - returns true in case of success, false in case of an error during SQL request
    // - then call FillRow(1..Table.RowCount) to get any row value
    // - or you can also loop through all rows with
    // ! while Rec.FillOne do
    // !   dosomethingwith(Rec);
    // - a temporary TOrmTable is created then stored in an internal fTable protected field
    // - for better server speed, the WHERE clause should use bound parameters
    // identified as '?' in the FormatSqlWhere statement, which is expected to
    // follow the order of values supplied in BoundsSqlWhere open array - use
    // DateToSql/DateTimeToSql for TDateTime, or directly any integer / double /
    // currency / RawUtf8 values to be bound to the request as parameters
    // - note that this method prototype changed with revision 1.17 of the
    // framework: array of const used to be ParamsSqlWhere and '%' in the
    // FormatSqlWhere statement, whereas it now expects bound parameters as '?'
    // - aCustomFieldsCsv can be used to specify which fields must be retrieved
    // - default aCustomFieldsCsv='' will retrieve all simple table fields, but
    // you may need  to access only one or several fields, and will save remote
    // bandwidth by specifying the needed fields
    // - if aCustomFieldsCsv='*', it will retrieve all fields, including BLOBs
    // - note that you should not use this aCustomFieldsCsv optional parameter if
    // you want to Update the retrieved record content later, since any
    // missing fields will be left with previous values - but BatchUpdate() can be
    // safely used after FillPrepare (will set only ID, TModTime and mapped fields)
    function FillPrepare(const aClient: IRestOrm;
      const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const;
      const aCustomFieldsCsv: RawUtf8 = ''): boolean; overload;
    /// prepare to get values using a specified WHERE clause with '%' and '?' parameters
    // - returns true in case of success, false in case of an error during SQL request
    // - then call FillRow(1..Table.RowCount) to get any row value
    // - or you can also loop through all rows with
    // ! while Rec.FillOne do
    // !   dosomethingwith(Rec);
    // - a temporary TOrmTable is created then stored in an internal fTable
    // protected field
    // - the FormatSqlWhere clause will replace all '%' chars with the supplied
    // ParamsSqlWhere[] supplied values, and bind all '?' chars as bound
    // parameters with BoundsSqlWhere[] values
    // - aCustomFieldsCsv can be used to specify which fields must be retrieved
    // - default aCustomFieldsCsv='' will retrieve all simple table fields, but
    // you may need  to access only one or several fields, and will save remote
    // bandwidth by specifying the needed fields
    // - if aCustomFieldsCsv='*', it will retrieve all fields, including BLOBs
    // - note that you should not use this aCustomFieldsCsv optional parameter if
    // you want to Update the retrieved record content later, since any
    // missing fields will be left with previous values - but BatchUpdate() can be
    // safely used after FillPrepare (will set only ID, TModTime and mapped fields)
    function FillPrepare(const aClient: IRestOrm;
      const FormatSqlWhere: RawUtf8; const ParamsSqlWhere, BoundsSqlWhere: array of const;
      const aCustomFieldsCsv: RawUtf8 = ''): boolean; overload;
    /// prepare to get values from a list of IDs
    // - returns true in case of success, false in case of an error during SQL request
    // - then call FillRow(1..Table.RowCount) to get any row value
    // - or you can also loop through all rows with
    // ! while Rec.FillOne do
    // !   dosomethingwith(Rec);
    // - a temporary TOrmTable is created then stored in an internal fTable protected field
    // - aCustomFieldsCsv can be used to specify which fields must be retrieved
    // - default aCustomFieldsCsv='' will retrieve all simple table fields, but
    // you may need  to access only one or several fields, and will save remote
    // bandwidth by specifying the needed fields
    // - if aCustomFieldsCsv='*', it will retrieve all fields, including BLOBs
    // - note that you should not use this aCustomFieldsCsv optional parameter if
    // you want to Update the retrieved record content later, since any
    // missing fields will be left with previous values - but BatchUpdate() can be
    // safely used after FillPrepare (will set only ID, TModTime and mapped fields)
    function FillPrepare(const aClient: IRestOrm;
      const aIDs: array of Int64;
      const aCustomFieldsCsv: RawUtf8 = ''): boolean; overload;
    // / prepare to loop through a JOINed statement including TOrmMany fields
    // - all TOrmMany.Dest published fields will now contain a true TOrm
    // instance, ready to be filled with the JOINed statement results (these
    // instances will be released at FillClose) - the same for Source which will
    // point to the self instance
    // - the aFormatSQLJoin clause will define a WHERE clause for an automated
    // JOINed statement, including TOrmMany published properties (and
    // their nested properties)
    // - returns true in case of success, false in case of an error during SQL request
    // - a typical use could be the following:
    // ! if aProd.FillPrepareMany(Database,
    // !    'Owner=? and Categories.Dest.Name=? and (Sizes.Dest.Name=? or Sizes.Dest.Name=?)',
    // !    [], ['mark', 'for boy', 'small', 'medium']) then
    // !   while aProd.FillOne do
    // !     //  here e.g. aProd.Categories.Dest are instantied (and Categories.Source=aProd)
    // !     writeln(aProd.Name,' ',aProd.Owner,' ',aProd.Categories.Dest.Name,' ',aProd.Sizes.Dest.Name);
    // !   //  you may also use aProd.FillTable to fill a grid, e.g.
    // !   //  (do not forget to set aProd.FillTable.OwnerMustFree := false)
    // this will execute a JOINed SELECT statement similar to the following:
    // $ select p.*, c.*, s.*
    // $ from Product p, Category c, Categories cc, Size s, Sizes ss
    // $ where c.id=cc.dest and cc.source=p.id and
    // $  s.id=ss.dest and ss.source=p.id and
    // $  p.Owner='mark' and c.Name='for boy' and (s.Name='small' or s.Name='medium')
    // - the FormatSqlWhere clause will replace all '%' chars with the supplied
    // ParamsSqlWhere[] supplied values, and bind all '?' chars as parameters
    // with BoundsSqlWhere[] values
    // - you SHALL call explicitly the FillClose method before using any
    // methods of nested TOrmMany instances which may override the Dest
    // instance content (e.g. ManySelect) to avoid any GPF
    // - is used by TOrm.CreateAndFillPrepareMany constructor
    function FillPrepareMany(const aClient: IRestOrm;
      const aFormatSQLJoin: RawUtf8;
      const aParamsSQLJoin, aBoundsSQLJoin: array of const): boolean;
    /// compute a JOINed statement including TOrmMany fields
    // - is called by FillPrepareMany() to retrieve the JSON of the corresponding
    // request: so you could use this method to retrieve directly the same
    // information, ready to be transmitted (e.g. as RawJson) to a client
    function EnginePrepareMany(const aClient: IRestOrm;
      const aFormatSQLJoin: RawUtf8;
      const aParamsSQLJoin, aBoundsSQLJoin: array of const;
      out ObjectsClass: TOrmClassDynArray; out SQL: RawUtf8): RawUtf8;
    /// fill all published properties of an object from a TOrmTable prepared row
    // - FillPrepare() must have been called before
    // - if Dest is nil, this object values are filled
    // - if Dest is not nil, this object values will be filled, but it won't
    // work with TOrmMany properties (i.e. after FillPrepareMany call)
    // - ID field is updated if first Field Name is 'ID'
    // - Row number is from 1 to Table.RowCount
    // - setter method (write Set*) is called if available
    // - handle UTF-8 SQL to Delphi values conversion (see TPropInfo mapping)
    // - this method has been made virtual e.g. so that a calculated value can be
    // used in a custom field
    function FillRow(aRow: integer; aDest: TOrm = nil): boolean; virtual;
    /// fill all published properties of this object from the next available
    // TOrmTable prepared row
    // - FillPrepare() must have been called before
    // - the Row number is taken from property FillCurrentRow
    // - return true on success, false if no more Row data is available
    // - internally call FillRow() to update published properties values
    function FillOne(aDest: TOrm = nil): boolean;
    /// go to the first prepared row, ready to loop through all rows with FillOne()
    // - the Row number (property FillCurrentRow) is reset to 1
    // - return true on success, false if no Row data is available
    // - you can use it e.g. as:
    // ! while Rec.FillOne do
    // !   dosomethingwith(Rec);
    // ! if Rec.FillRewind then
    // !   while Rec.FillOne do
    // !     dosomeotherthingwith(Rec);
    function FillRewind: boolean;
    /// close any previous FillPrepare..FillOne loop
    // - is called implicitely by FillPrepare() call to release any previous loop
    // - release the internal hidden TOrmTable instance if necessary
    // - is not mandatory if the TOrm is released just after, since
    // TOrm.Destroy will call it
    // - used e.g. by FillFrom methods below to avoid any GPF/memory confusion
    procedure FillClose;
    /// will iterate over all FillPrepare items, appending them as a JSON array
    // - creates a JSON array of all record rows, using
    // ! while FillOne do GetJsonValues(W)...
    procedure AppendFillAsJsonValues(W: TOrmWriter);

    /// fill all published properties of this object from a TOrmTable result row
    // - call FillPrepare() then FillRow(Row)
    procedure FillFrom(Table: TOrmTable; Row: integer); overload;
    /// fill all published properties of this object from a JSON result row
    // - create a TOrmTable from the JSON data
    // - call FillPrepare() then FillRow(Row)
    procedure FillFrom(const JSONTable: RawUtf8; Row: integer); overload;
    /// fill all published properties of this object from a JSON object result
    // - use JSON data, as exported by GetJsonValues()
    // - JSON data may be expanded or not
    // - make an internal copy of the JSONTable RawUtf8 before calling
    // FillFrom() below
    // - if FieldBits is defined, it will store the identified field index
    procedure FillFrom(const JsonRecord: RawUtf8; FieldBits: PFieldBits = nil); overload;
    /// fill all published properties of this object from a JSON result
    // - the data inside P^ is modified (unescaped and transformed): don't call
    // FillFrom(pointer(JsonRecordUTF8)) but FillFrom(JsonRecordUTF8) which makes
    // a temporary copy of the JsonRecordUTF8 text
    // - use JSON data, as exported by GetJsonValues()
    // - JSON data may be expanded or not
    // - if FieldBits is defined, it will store the identified field index
    procedure FillFrom(P: PUtf8Char; FieldBits: PFieldBits = nil); overload;
    /// fill all published properties of this object from another object
    // - source object must be a parent or of the same class as the current record
    // - copy all COPIABLE_FIELDS, i.e. all fields excluding tftMany (because
    // those fields don't contain any data, but a TOrmMany instance
    // which allow to access to the pivot table data)
    procedure FillFrom(aRecord: TOrm); overload;
    /// fill the specified properties of this object from another object
    // - source object must be a parent or of the same class as the current record
    // - copy the fields, as specified by their bit index in the source record;
    // you may use aRecord.GetNonVoidFields if you want to update some fields
    procedure FillFrom(aRecord: TOrm; const aRecordFieldBits: TFieldBits); overload;
    /// fill all published properties of this object from a supplied TDocVariant
    // object document
    // - is a wrapper around VariantSaveJson() + FillFrom() methods
    procedure FillFrom(const aDocVariant: variant); overload;
    /// fill a published property value of this object from a UTF-8 encoded value
    // - see TPropInfo about proper Delphi / UTF-8 type mapping/conversion
    // - use this method to fill a BLOB property, i.e. a property defined with
    // type RawBlob, since by default all BLOB properties are not
    // set by the standard Retrieve() method (to save bandwidth)
    // - if FieldBits is defined, it will store the identified field index
    procedure FillValue(PropName, Value: PUtf8Char; ValueLen: PtrInt;
      wasString: boolean; FieldBits: PFieldBits = nil);

    /// return true if all published properties values in Other are identical to
    // the published properties of this object
    // - instances must be of the same class type
    // - only simple fields (i.e. not RawBlob/TOrmMany) are compared
    // - comparison is much faster than SameValues() below
    function SameRecord(Reference: TOrm): boolean;
    /// return true if all published properties values in Other are identical to
    // the published properties of this object
    // - work with different classes: Reference properties name must just be
    // present in the calling object
    // - only simple fields (i.e. not RawBlob/TOrmMany) are compared
    // - compare the text representation of the values: fields may be of different
    // type, encoding or precision, but still have same values
    function SameValues(Reference: TOrm): boolean;
    /// clear the values of all published properties, and also the ID property
    procedure ClearProperties; overload;
    /// clear the values of specified published properties
    // - '' will leave the content untouched, '*' will clear all simple fields
    procedure ClearProperties(const aFieldsCsv: RawUtf8); overload;
    /// set the simple fields with the supplied values
    // - the aSimpleFields parameters must follow explicitly the order of published
    // properties of the supplied aTable class, excepting the RawBlob and
    // TOrmMany kind (i.e. only so called "simple fields") - in particular,
    // parent properties must appear first in the list
    // - the aSimpleFields must have exactly the same count of parameters as there are
    // "simple fields" in the published properties
    // - return true on success, but be aware that the field list must match
    // the field layout, otherwise if may return true but will corrupt data
    function SimplePropertiesFill(const aSimpleFields: array of const): boolean;
    /// set the simple fields from a JSON array of values - after the initial [
    function FillFromArray(const Fields: TFieldBits; Json: PUtf8Char): boolean;
    /// initialize a TDynArray wrapper to map dynamic array property values
    // - if the field name is not existing or not a dynamic array, result.IsVoid
    // will be TRUE
    function DynArray(const DynArrayFieldName: RawUtf8): TDynArray; overload;
    /// initialize a TDynArray wrapper to map dynamic array property values
    // - this overloaded version expect the dynamic array to have been defined
    // with a not null index attribute, e.g.
    // ! published
    // !   property Ints: TIntegerDynArray index 1 read fInts write fInts;
    // !   property Currency: TCurrencyDynArray index 2 read fCurrency write fCurrency;
    // - if the field index is not existing or not a dynamic array, result.IsVoid
    // will be TRUE
    function DynArray(DynArrayFieldIndex: integer): TDynArray; overload;
    {$ifndef PUREMORMOT2}
    class function RecordProps: TOrmProperties;
      {$ifdef HASINLINE}inline;{$endif}
    {$endif PUREMORMOT2}

    /// this property stores the record's integer ID
    // - if this TOrm is not a instance, but a field value in a published
    //  property of type oftID (i.e. TOrm(aID)), this method will try
    //  to retrieve it; but prefered method is to typecast it via PtrInt(aProperty),
    //  because GetID() relies on some low-level Windows memory mapping trick, and
    //  will recognize an ID value up to 1,048,576 (i.e. $100000)
    // - notice: the Setter should not be used usualy; you should not have to write
    //  aRecord.ID := someID in your code, since the ID is set during Retrieve or
    //  Add of the record
    // - rather use IDValue property for direct read/write access to the
    // ID field, if you know that this TOrm is a true allocated class instance
    property ID: TID
      read GetID;
    /// this read-only property can be used to retrieve the ID as a TOrm object
    // - published properties of type TOrm (one-to-many relationship) do not
    // store real class instances (only exception is if they inherit from
    // TOrmMany) - you can use this value to assign a TOrm instance
    // to a published property, as such:
    // ! Main := TOrmMain.Create;
    // ! Client.Add(Main);
    // ! Detail := TOrmDetail.Create;
    // ! Detail.Main := Main.AsTOrm; // will store Main.ID in MAIN column
    // ! Client.Add(Detail);
    // - is especially useful on 64-bit plaform, since on 32-bit:
    // ! Detail.Main := pointer(Main.ID)
    // compiles (whereas it won't on 64-bit) and is the same than platform-independent
    // ! Detail.Main := Main.AsTOrm;
    // - using Main.AsTOrm will ensure that the ID is retrieved, even
    // if Main itself is not a true instance
    // - if the stored ID is bigger than 32-bit, then it will raise an
    // EOrmException: in this case, you should use a TID / T*ID kind of
    // published property, and not a TOrm, which is limited to the
    // pointer size
    // - on FPC, if you get an Error: Incompatible types: got "Pointer" expected
    // "T...", then you are missing a {$mode Delphi} conditional in your unit:
    // the easiest is to include {$I mormot.define.inc} at the top of your unit
    property AsTOrm: pointer
      read GetIDAsPointer;
    /// this property is set to true, if any published property is a BLOB (RawBlob)
    property HasBlob: boolean
      read GetHasBlob;
    /// this property returns the published property count with any valid
    // database field except RawBlob/TOrmMany
    // - by default, the RawBlob (BLOB) fields are not included into this set:
    // they must be read specificaly (in order to spare bandwidth)
    // - TOrmMany fields are not accessible directly, but as instances
    // created by TOrm.Create
    property SimpleFieldCount: integer
      read GetSimpleFieldCount;
    /// this property contains the TOrmTable after a call to FillPrepare()
    property FillTable: TOrmTable
      read GetTable;
    /// this property contains the current row number (beginning with 1),
    // initialized to 1 by FillPrepare(), which will be read by FillOne
    property FillCurrentRow: integer
      read GetFillCurrentRow;
    /// this property is set to true, if all rows have been browsed after
    // FillPrepare / while FillOne do ...
    property FillReachedEnd: boolean
      read GetFillReachedEnd;
    /// used internally by FillPrepare() and corresponding Fill*() methods
    property FillContext: TOrmFill
      read fFill;
    /// this property contains the internal state counter of the server database
    // when the data was retrieved from it
    // - can be used to check if retrieved data may be out of date
    property InternalState: cardinal
      read fInternalState write fInternalState;
  published
    { published properties in inherited classes will be interpreted as SQL fields }
  end;

  POrm = ^TOrm;

  TOrmArray = array[0..MaxInt div SizeOf(TOrm) - 1] of TOrm;
  POrmArray = ^TOrmArray;

  {$ifdef HASGENERICS}

  /// since Delphi interfaces cannot have parametrized methods, we need
  // to use this abstract class to use generics signature
  // - our IList<> and IKeyValue<> interfaces are faster and generates smaller
  // executables than Generics.Collections, and need no try..finally Free
  TRestOrmGenerics = class(TInterfacedObject)
  protected
    // needed to implement RetrieveIList<T> actual data retrieval
    function MultiFieldValues(Table: TOrmClass; const FieldNames: RawUtf8;
      const WhereClauseFormat: RawUtf8; const BoundsSqlWhere: array of const): TOrmTable;
      overload; virtual; abstract;
  public
    /// access to ORM parametrized/generic methods
    // - since Delphi interface cannot have parametrized methods, we need
    // to return this abstract class to use generics signature
    function Generics: TRestOrmGenerics;
    /// get a list of members from a SQL statement
    // - implements REST GET collection
    // - aCustomFieldsCsv can be the CSV list of field names to be retrieved
    // - if aCustomFieldsCsv is '', will get all simple fields, excluding BLOBs
    // - if aCustomFieldsCsv is '*', will get ALL fields, including ID and BLOBs
    // - return a IList<T> on success (possibly with Count=0) - caller is
    // responsible of freeing the instance
    // - return nil on error
    // - since Delphi interface cannot have parametrized methods, we need to
    // call this overloaded TRestOrmGenerics method to use generics signature
    // - you can write for instance:
    // !var list: IList<TOrmTest>;
    // !    R: TOrmTest;
    // !    orm: IRestOrm
    // ! ...
    // !    list := orm.Generics.RetrieveIList<TOrmTest>('ID,Test');
    // !    if list <> nil then
    // !      for R in list do
    // !        writeln(R.ID, '=', R.Test);
    function RetrieveIList<T: TOrm>(
      const aCustomFieldsCsv: RawUtf8 = ''): IList<T>; overload;
       {$ifdef HASINLINE}inline;{$endif}
    /// get a list of members from a SQL statement
    // - implements REST GET collection with a WHERE clause
    // - for better server speed, the WHERE clause should use bound parameters
    // identified as '?' in the FormatSqlWhere statement, which is expected to
    // follow the order of values supplied in BoundsSqlWhere open array - use
    // DateToSql()/DateTimeToSql() for TDateTime, or directly any integer,
    // double, currency, RawUtf8 values to be bound to the request as parameters
    // - aCustomFieldsCsv can be the CSV list of field names to be retrieved
    // - if aCustomFieldsCsv is '', will get all simple fields, excluding BLOBs
    // - if aCustomFieldsCsv is '*', will get ALL fields, including ID and BLOBs
    // - return a IList<T> on success (possibly with Count=0)
    // - return nil on error
    // - since Delphi interface cannot have parametrized methods, we need to
    // call this overloaded TRestOrmGenerics method to use generics signature
    function RetrieveIList<T: TOrm>(const FormatSqlWhere: RawUtf8;
      const BoundsSqlWhere: array of const;
      const aCustomFieldsCsv: RawUtf8 = ''): IList<T>; overload;
  end;

  TRestOrmParent = class(TRestOrmGenerics)

  {$else}

  /// parent class of TRestOrm, to implement IRestOrm methods
  // - since Delphi interface cannot have parametrized methods, we need
  // to define a TRestOrmGenerics abstract class to use generics signature
  TRestOrmParent = class(TInterfacedObject)

  {$endif HASGENERICS}
  public
    /// ensure the current thread will be taken into account during process
    // - this abstract method won't do anything, but overriden versions may
    procedure BeginCurrentThread(Sender: TThread); virtual;
    /// called when thread is finished to ensure
    // - this abstract method won't do anything, but overriden versions may
    procedure EndCurrentThread(Sender: TThread); virtual;
  end;


  { -------------------- RecordRef Wrapper Definition }

  /// useful object to type cast TRecordReference type value into explicit
  // TOrmClass and ID
  // - use RecordRef(Reference).TableIndex/Table/ID/Text methods to retrieve
  // the details of a TRecordReference encoded value
  // - use TRest.Retrieve(Reference) to get a record content from DB
  // - instead of From(Reference).From(), you could use the more explicit
  // TOrm.RecordReference(Model) or TOrmModel.RecordReference()
  // methods or RecordReference() function to encode the value
  // - don't change associated TOrmModel tables order, since TRecordReference
  // depends on it to store the Table type
  // - since 6 bits are used for the table index, the corresponding table
  // MUST appear in the first 64 items of the associated TOrmModel.Tables[]
  RecordRef = object
  public
    /// the value itself
    // - (value and 63) is the TableIndex in the current database Model
    // - (value shr 6) is the ID of the record in this table
    // - value=0 means no reference stored
    // - we use this coding and not the opposite (Table in MSB) to minimize
    // integer values; but special Utf8CompareRecord() function has to be used
    // for sorting
    // - type definition matches TRecordReference (i.e. Int64/TID) to allow
    // typecast as such:
    // ! aClass := PRecordRef(@Reference)^.Table(Model);
    Value: TID;
    /// return the index of the content Table in the TOrmModel
    function TableIndex: integer;
      {$ifdef HASINLINE}inline;{$endif}
    /// return the class of the content in a specified TOrmModel
    function Table(Model: TOrmModel): TOrmClass;
    /// return the ID of the content
    function ID: TID;
      {$ifdef HASINLINE}inline;{$endif}
    /// fill Value with the corresponding parameters
    // - since 6 bits are used for the table index, aTable MUST appear in the
    // first 64 items of the associated TOrmModel.Tables[] array
    procedure From(Model: TOrmModel; aTable: TOrmClass; aID: TID);
    /// get a ready to be displayed text from the stored Table and ID
    // - display 'Record 2301' e.g.
    function Text(Model: TOrmModel): RawUtf8; overload;
    /// get a ready to be displayed text from the stored Table and ID
    // - display 'Record "RecordName"' e.g.
    function Text(const Rest: IRestOrm): RawUtf8; overload;
  end;

  PRecordRef = ^RecordRef;


  { -------------------- TOrmTable TOrmTableJson Definitions }

  /// wrapper to an ORM result table, staticaly stored as UTF-8 text
  // - this abstract parent holds no data, just pointer to the values
  // - first row contains the field names, following rows contains the data
  // - will be implemented as TOrmTableJson holding JSON content
  TOrmTable = class(TOrmTableAbstract)
  protected
    fQueryTables: TOrmClassDynArray; // which TOrm classes generated the data
    function InitOneFieldType(field: PtrInt; out size: integer;
      out info: PRttiInfo; out tableindex: integer): TOrmFieldType; override;
    procedure FillOrms(P: POrm; RecordType: TOrmClass);
    {$ifdef HASGENERICS}
    /// create and fill a new IList<T>
    procedure ToNewIList(var result; item: TOrmClass);
    {$endif HASGENERICS}
    /// guess the property type information from ORM
    function FieldPropFromTables(const PropName: RawUtf8;
      out PropInfo: TOrmPropInfo; out TableIndex: integer): TOrmFieldType;
  public
    /// initialize the result table
    // - you can associate the corresponding TOrmClass types,
    // by which the results were computed (it will use RTTI for column typing)
    constructor CreateFromTables(const Tables: array of TOrmClass;
      const aSql: RawUtf8);
    /// read-only access to a particular field value, as VCL text
    // - Client is used to display TRecordReference via the associated TOrmModel
    // - returns the Field Type
    // - return generic string Text, i.e. UnicodeString for Delphi 2009+, ready
    // to be displayed to the VCL, for oftEnumerate, oftTimeLog,
    // oftUnixTime/oftUnixMSTime and oftRecord/oftRecordVersion/oftID/oftTID
    // - returns '' as string Text, if text can by displayed directly
    // with Get*() methods above
    // - returns '' for other properties kind, if Utf8ToString is nil,
    // or the ready to be displayed value if Utf8ToString event is set
    // (to be used mostly with Language.Utf8ToString)
    // - CustomFormat can optionaly set a custom format string, e.g. '%f' or '%n'
    // or complex FormatFloat()/FormatCurr() syntax (as '#,##0.00') for oftFloat
    // and oftCurrency columns (instead of plain JSON float value), or
    // date/time format as expected by FormatDateTime() for all date time kind
    // of fields (as oftDateTime, oftDateTimeMS, oftTimeLog, oftModTime,
    // oftCreateTime, oftUnixTime, oftUnixMSTime)
    function ExpandAsString(Row, Field: PtrInt; const Client: IRestOrm;
      out Text: string; const CustomFormat: string = ''): TOrmFieldType;
    /// read-only access to a particular field value, as VCL text
    // - this method is just a wrapper around ExpandAsString method, returning
    // the content as a SynUnicode string type (i.e. UnicodeString since Delphi
    // 2009, and WideString for non Unicode versions of Delphi)
    function ExpandAsSynUnicode(Row, Field: PtrInt; const Client: IRestOrm;
      out Text: SynUnicode): TOrmFieldType;
    /// get the record class (i.e. the table) associated to a field
    // - is nil if this table has no QueryTables property
    // - very fast: calculated only once for all fields
    function FieldTable(Field: PtrInt): TOrmClass;

    /// search a text value inside the table data in a specified field
    // - the text value must already be uppercased 7-bits ANSI encoded
    // - return the Row on success, 0 on error
    // - search only in the content of FieldIndex data
    // - you can specify a Soundex pronunciation to use, or leave as sndxNone for
    // standard case insensitive character match; aUpperValue can optional
    // indicate a Soundex search, by predeceding the searched text with % for
    // English, %% for French or %%% for Spanish (only works with WinAnsi
    // char set - i.e. code page 1252)
    // - if UnicodeComparison is set to TRUE, search will use low-level Windows
    // API for Unicode-level conversion - it will be much slower, but accurate
    // for the whole range of UTF-8 encoding
    // - if UnicodeComparison is left to FALSE, UTF-8 decoding will be done only
    // if necessary: it will work only with standard western-occidental alphabet
    // (i.e. WinAnsi - code page 1252), but it will be very fast
    function SearchValue(const UpperValue: RawUtf8;
      StartRow, FieldIndex: PtrInt; const Client: IRestOrm;
      Lang: TSynSoundExPronunciation = sndxNone;
      UnicodeComparison: boolean = false): PtrInt; overload;
    /// search a text value inside the table data in all fields
    // - the text value must already be uppercased 7-bits ANSI encoded
    // - return the Row on success, 0 on error
    // - search on all fields, returning field found in FieldIndex (if not nil)
    // - you can specify a Soundex pronunciation to use, or leave as sndxNone for
    // standard case insensitive character match; aUpperValue can optional
    // indicate a Soundex search, by predeceding the searched text with % for
    // English, %% for French or %%% for Spanish (only works with WinAnsi
    // char set - i.e. code page 1252)
    // - if UnicodeComparison is set to TRUE, search will use low-level Windows
    // API for Unicode-level conversion - it will be much slower, but accurate
    // for the whole range of UTF-8 encoding
    // - if UnicodeComparison is left to FALSE, UTF-8 decoding will be done only
    // if necessary: it will work only with standard western-occidental alphabet
    // (i.e. WinAnsi - code page 1252), but it will be very fast
    function SearchValue(const UpperValue: RawUtf8;
      StartRow: PtrInt; FieldIndex: PInteger; const Client: IRestOrm;
      Lang: TSynSoundExPronunciation = sndxNone;
      UnicodeComparison: boolean = false): PtrInt; overload;
    /// retrieve QueryTables[0], if existing
    function QueryRecordType: TOrmClass;
    /// create and own a new TOrm instance for a specific Table
    // - a void TOrm instance is created, ready to be filled
    // - use the specified TOrm class or create one instance
    // of the first associated record class (from internal QueryTables[])
    // - the returned records will be managed by this TOrmTable: they will be
    // freed when the TOrmTable is destroyed: you don't need to make a
    // try..finally..Free..end block with them
    function NewRecord(RecordType: TOrmClass = nil): TOrm;
    /// create a TObjectList with TOrm instances corresponding to this
    // TOrmTable result set
    // - use the specified TOrm class or create instances
    // of the first associated record class (from internal QueryTables[])
    // - always returns an instance, even if the TOrmTable is nil or void
    function ToObjectList(RecordType: TOrmClass = nil): TObjectList; overload;
    /// fill an existing TObjectList with TOrm instances corresponding
    // to this TOrmTable result set
    // - use the specified TOrm class or create instances
    // of the first associated record class (from internal QueryTables[])
    procedure ToObjectList(DestList: TObjectList;
      RecordType: TOrmClass = nil); overload;
    {$ifdef HASGENERICS}
    /// create a IList<TOrm> with TOrm instances corresponding to this resultset
    // - always returns an IList<> instance, even if the TOrmTable is nil or void
    // - our IList<> and IKeyValue<> interfaces are faster and generates smaller
    // executables than Generics.Collections, and need no try..finally Free: a
    // single TSynListSpecialized<TOrm> class will be reused for all IList<>
    function ToIList<T: TOrm>: IList<T>; overload;
    {$endif HASGENERICS}
    /// fill an existing T*ObjArray variable with TOrm instances
    // corresponding to this TOrmTable result set
    // - use the specified TOrm class or create instances
    // of the first associated record class (from internal QueryTables[])
    // - returns TRUE on success (even if ObjArray=[]), FALSE on error
    function ToObjArray(var ObjArray; RecordType: TOrmClass = nil): boolean;

    /// contains the associated record class on Query
    property QueryTables: TOrmClassDynArray
      read fQueryTables;
  end;


  /// store a read-only ORM result table from a JSON message
  // - the JSON data is parsed and unescaped in-place, to enhanced performance
  // and reduce resource consumption (mainly memory/heap fragmentation)
  // - is used by the ORM for TOrm.FillPrepare/FillOne methods for
  // fast access to individual object values
  // - some numbers taken from TTestCoreProcess.JSONBenchmark on my laptop:
  // $   TOrmTableJson expanded in 38.82ms, 505 MB/s
  // $   TOrmTableJson not expanded in 21.54ms, 400.3 MB/s
  // $   TOrmTableJson GetJsonValues in 22.94ms, 375.9 MB/s
  TOrmTableJson = class(TOrmTable)
  protected
    /// used if a private copy of the JSON buffer is needed
    fPrivateCopy: RawUtf8;
    /// contains the pointers/offset of start of every field value
    fJsonData: TOrmTableJsonDataArray;
    /// contain the hash value of the last JSON data sent to ContentChanged()
    // - used to don't repeat parsing if data has not been changed
    fPrivateCopyHash: cardinal;
    /// fill the result table content from a JSON-formated Data message
    // - returns TRUE on parsing success
    // - returns FALSE if no valid JSON data was found
    // - update all content fields (Results[], RowCount, FieldCount, etc...)
    // - expect the UTF-8 Buffer in either TSqlRequest.EngineExecute(DB,SQL,JSON)
    // format (i.e. expanded) or either in a not expanded format (as an
    // AJAX-ready array of objects)
    // - the conversion into PPUtf8CharArray is made inplace and is very fast
    // (no additional memory buffer is allocated)
    function ParseAndConvert(Buffer: PUtf8Char; BufferLen: integer): boolean;
    /// will check then set (if needed) internal fPrivateCopy[Hash] values
    // - returns TRUE if fPrivateCopy content changed (then fPrivateCopyHash
    // will be updated using crc32c hash if aUpdateHash is set)
    function PrivateCopyChanged(aJson: PUtf8Char; aLen: integer;
      aUpdateHash: boolean): boolean;
  public
    /// create the result table from a JSON-formated Data message
    // - the JSON data is parsed and formatted in-place
    // - please note that the supplied JSON buffer content will be changed:
    // if you want to reuse this JSON content, you shall make a private copy
    // before calling this constructor and you shall NOT release the corresponding
    // variable (Results/JsonResults[] will point inside this memory buffer):
    // use instead the overloaded Create constructor expecting a const
    // aJson: RawUtf8 parameter to allocate and hold a private copy of the data
    constructor Create(const aSql: RawUtf8;
      JsonBuffer: PUtf8Char; JsonBufferLen: integer); reintroduce; overload;
    /// create the result table from a JSON-formated Data message
    // - the JSON data is parsed and formatted in-place, after having been
    // copied in the protected fPrivateCopy variable
    constructor Create(const aSql, aJson: RawUtf8); reintroduce; overload;
    /// create the result table from a JSON-formated Data message
    // - the JSON data is parsed and formatted in-place
    // - you can specify a set of TOrm classes which will be used to
    // retrieve the column exact type information
    // - please note that the supplied JSON buffer content will be changed
    constructor CreateFromTables(const Tables: array of TOrmClass;
      const aSql: RawUtf8;
      JsonBuffer: PUtf8Char; JsonBufferLen: integer); reintroduce; overload;
    /// create the result table from a JSON-formated Data message
    // - you can specify a set of TOrm classes which will be used to
    // retrieve the column exact type information
    // - the JSON data is parsed and formatted in-place, after copied
    // in the protected fPrivateCopy variable (by reference if aJsonOwned=true)
    constructor CreateFromTables(const Tables: array of TOrmClass; const
      aSql, aJson: RawUtf8; aJsonOwned: boolean = false); reintroduce; overload;
    /// initialize the result table from a JSON-formated Data message
    // - you can set the expected column types matching the results column layout
    // - the JSON data is parsed and formatted in-place
    constructor CreateWithColumnTypes(const ColumnTypes: array of TOrmFieldType;
      const aSql: RawUtf8; JsonBuffer: PUtf8Char; JsonBufferLen: integer);
      reintroduce; overload;
    /// initialize the result table from a JSON-formated Data message
    // - you can set the expected column types matching the results column layout
    // - the JSON data is parsed and formatted in-place, after having been
    // copied in the protected fPrivateCopy variable
    constructor CreateWithColumnTypes(const ColumnTypes: array of TOrmFieldType;
      const aSql, aJson: RawUtf8); reintroduce; overload;

    /// update the result table content from a JSON-formated Data message
    // - return true on parsing success, false if no valid JSON data was found
    // - set Refreshed to true if the content changed
    // - update all content fields (Results[], RowCount, FieldCount, etc...)
    // - call SortFields() if was already done for this TOrmTable
    // - the conversion into PPUtf8CharArray is made inplace and is very fast
    // (only one memory buffer is allocated for the whole data)
    function UpdateFrom(const aJson: RawUtf8; var Refreshed: boolean;
      PCurrentRow: PInteger): boolean;

    /// the private copy of the processed data buffer
    // - available e.g. for Create constructor using aJson parameter,
    // or after the UpdateFrom() process
    // - this buffer is not to be access directly: this won't be a valid JSON
    // content, but a processed buffer, on which Results[] elements point to -
    // it will contain unescaped text and numerical values, ending with #0
    property PrivateInternalCopy: RawUtf8
      read fPrivateCopy;
  end;


  { -------------------- TOrmMany Definition }

  /// handle "has many" and "has many through" relationships
  // - many-to-many relationship is tracked using a table specifically for that
  // relationship, turning the relationship into two one-to-many relationships
  // pointing in opposite directions
  // - by default, only two TOrm (i.e. INTEGER) fields must be created,
  // named "Source" and "Dest", the first pointing to the source record (the one
  // with a TOrmMany published property) and the second to the destination record
  // - you should first create a type inheriting from TOrmMany, which
  // will define the pivot table, providing optional "through" parameters if needed
  // ! TOrmProductDest = class(TOrm);
  // ! TOrmProductSource = class;
  // ! TOrmProductDestPivot = class(TOrmMany)
  // ! private
  // !  fSource: TOrmProductSource;
  // !  fDest: TOrmProductDest;
  // !  fTime: TDateTime;
  // ! published
  // !   property Source: TOrmProductSource read fSource; // map Source column
  // !   property Dest: TOrmProductDest read fDest; // map Dest column
  // !   property AssociationTime: TDateTime read fTime write fTime;
  // ! end;
  // ! TOrmProductSource = class(TOrm)
  // ! private
  // !   fDestList: TOrmProductDestPivot;
  // ! published
  // !   DestList: TOrmProductDestPivot read fDestList;
  // ! end;
  // - in all cases, at leat two 'Source' and 'Dest' published properties must
  // be declared as TOrm children in any TOrmMany descendant
  // because they will always be needed for the 'many to many' relationship
  // - when a TOrmMany published property exists in a TOrm, it is
  // initialized automaticaly by TOrm.Create
  // - to add some associations to the pivot table, use the ManyAdd() method
  // - to retrieve an association, use the ManySelect() method
  // - to delete an association, use the ManyDelete() method
  // - to read all Dest records IDs, use the DestGet() method
  // - to read the Dest records and the associated "through" fields content, use
  // FillMany then FillRow, FillOne and FillRewind methods to loop through records
  // - to read all Source records and the associaed "through" fields content,
  // FillManyFromDest then FillRow, FillOne and FillRewind methods
  // - to read all Dest IDs after a join to the pivot table, use DestGetJoined
  TOrmMany = class(TOrm)
  protected
    // internal fields initialized during TOrm.Create
    // - map to the Source and Dest properties field values in TOrm values
    fSourceID: PPtrInt;
    fDestID: PPtrInt;
    /// retrieve the TOrmMany ID from a given source+dest IDs pair
    function InternalIDFromSourceDest(const aClient: IRestOrm;
      aSourceID, aDestID: TID): TID;
    function InternalFillMany(const aClient: IRestOrm; aID: TID;
      const aAndWhereSql: RawUtf8; isDest: boolean): integer;
    function IsPropClassInstance(Prop: PRttiCustomProp): boolean; override;
  public
    /// initialize this instance, and needed internal fields
    // - will set protected fSourceID/fDestID fields
    constructor Create; override;
    /// retrieve all records associated to a particular source record, which
    // has a TOrmMany property
    // - returns the Count of records corresponding to this aSource record
    // - the records are stored in an internal TOrmTable, refered in the private
    // fTable field, and initialized via a FillPrepare call: all Dest items
    // are therefore accessible with standard FillRow, FillOne and FillRewind methods
    // - use a "for .." loop or a "while FillOne do ..." loop to iterate
    // through all Dest items, getting also any additional 'through' columns
    // - if source ID parameter is 0, the ID is taken from the fSourceID field
    // (set by TOrm.Create)
    // - note that if the Source record has just been added, fSourceID is not
    // set, so this method will fail: please specify aSourceID parameter with
    // the one just added/created
    // - the optional aAndWhereSql parameter can be used to add any additional
    // condition to the WHERE statement (e.g. 'Salary>:(1000): AND Salary<:(2000):')
    // according to TOrmMany properties - note that you should better use
    // inlined parameters for faster processing on server, so you may call e.g.
    // ! aRec.FillMany(Client, 0, FormatUtf8('Salary>? AND Salary<?', [], [1000, 2000]));
    function FillMany(const aClient: IRestOrm; aSourceID: TID = 0;
      const aAndWhereSql: RawUtf8 = ''): integer;
    /// retrieve all records associated to a particular Dest record, which
    // has a TOrmMany property
    // - returns the Count of records corresponding to this aSource record
    // - use a "for .." loop or a "while FillOne do ..." loop to iterate
    // through all Dest items, getting also any additional 'through' columns
    // - the optional aAndWhereSql parameter can be used to add any additional
    // condition to the WHERE statement (e.g. 'Salary>:(1000): AND Salary<:(2000):')
    // according to TOrmMany properties - note that you should better use
    // inlined parameters for faster processing on server, so you may call e.g.
    // ! aRec.FillManyFromDest(Client, DestID, FormatUtf8('Salary>? AND Salary<?', [], [1000, 2000]));
    function FillManyFromDest(const aClient: IRestOrm; aDestID: TID;
      const aAndWhereSql: RawUtf8 = ''): integer;
    /// retrieve all Dest items IDs associated to the specified Source
    function DestGet(const aClient: IRestOrm; aSourceID: TID;
      out DestIDs: TIDDynArray): boolean; overload;
    /// retrieve all Dest items IDs associated to the current Source ID
    // - source ID is taken from the fSourceID field (set by TOrm.Create)
    // - note that if the Source record has just been added, fSourceID is not
    // set, so this method will fail: please call the other overloaded method
    function DestGet(const aClient: IRestOrm;
      out DestIDs: TIDDynArray): boolean; overload;
    /// retrieve all Source items IDs associated to the specified Dest ID
    function SourceGet(const aClient: IRestOrm; aDestID: TID;
      out SourceIDs: TIDDynArray): boolean;
    /// retrieve all Dest items IDs associated to the current or
    // specified Source ID, adding a WHERE condition against the Dest rows
    // - if aSourceID is 0, the value is taken from current fSourceID field
    // (set by TOrm.Create)
    // - aDestWhereSql can specify the Dest table name in the statement, e.g.
    //  'Salary>:(1000): AND Salary<:(2000):' - note that you should better use
    // inlined parameters for faster processing on server, so you may use the
    // more convenient function
    // ! FormatUtf8('Salary>? AND Salary<?', [], [1000, 2000])
    // - this is faster than a manual FillMany() then loading each Dest,
    // because the condition is executed in the SQL statement by the server
    function DestGetJoined(const aClient: IRestOrm; const aDestWhereSql: RawUtf8;
      aSourceID: TID; out DestIDs: TIDDynArray): boolean; overload;
    /// create a Dest record, then FillPrepare() it to retrieve all Dest items
    // associated to the current or specified Source ID, adding a WHERE condition
    // against the Dest rows
    // - if aSourceID is 0, the value is taken from current fSourceID field
    // (set by TOrm.Create)
    // - aDestWhereSql can specify the Dest table name in the statement, e.g.
    // 'Salary>:(1000): AND Salary<:(2000):') according to TOrmMany
    // properties - note that you should better use such inlined parameters as
    // ! FormatUtf8('Salary>? AND Salary<?', [], [1000, 2000])
    function DestGetJoined(const aClient: IRestOrm; const aDestWhereSql: RawUtf8;
      aSourceID: TID): TOrm; overload;
    /// create a TOrmTable, containing all specified Fields, after a JOIN
    // associated to the current or specified Source ID
    // - the Table will have the fields specified by the JoinKind parameter
    // - aCustomFieldsCsv can be used to specify which fields must be retrieved
    // (for jkDestFields, jkPivotFields, jkPivotAndDestFields) - default is all
    // - if aSourceID is 0, the value is taken from current fSourceID field
    // (set by TOrm.Create)
    // - aDestWhereSql can specify the Dest table name in the statement, e.g.
    // 'Salary>:(1000): AND Salary<:(2000):') according to TOrmMany
    // properties - note that you should better use such inlined parameters as
    // ! FormatUtf8('Salary>? AND Salary<?', [], [1000, 2000])
    function DestGetJoinedTable(const aClient: IRestOrm;
      const aDestWhereSql: RawUtf8; aSourceID: TID; JoinKind: TOrmManyJoinKind;
      const aCustomFieldsCsv: RawUtf8 = ''): TOrmTable;
    /// add a Dest record to the Source record list
    // - returns TRUE on success, FALSE on error
    // - if NoDuplicates is TRUE, the existence of this Source/Dest ID pair
    // is first checked
    // - current Source and Dest properties are filled with the corresponding
    // TRecordReference values corresponding to the supplied IDs
    // - any current value of the additional fields are used to populate the
    // newly created content (i.e. all published properties of this record)
    // - if aUseBatch is set, it will use this TRestBach.Add() instead
    // of the slower aClient.Add() method
    function ManyAdd(const aClient: IRestOrm; aSourceID, aDestID: TID;
      NoDuplicates: boolean = false; aUseBatch: TRestBatch = nil): boolean; overload;
    /// add a Dest record to the current Source record list
    // - source ID is taken from the fSourceID field (set by TOrm.Create)
    // - note that if the Source record has just been added, fSourceID is not
    // set, so this method will fail: please call the other overloaded method
    function ManyAdd(const aClient: IRestOrm; aDestID: TID;
      NoDuplicates: boolean = false): boolean; overload;
    /// will delete the record associated with a particular Source/Dest pair
    // - will return TRUE if the pair was found and successfully deleted
    // - if aUseBatch is set, it will use this TRestBach.Delete() instead
    // of the slower aClient.Delete() method
    function ManyDelete(const aClient: IRestOrm; aSourceID, aDestID: TID;
      aUseBatch: TRestBatch = nil): boolean; overload;
    /// will delete the record associated with the current source and a specified Dest
    // - source ID is taken from the fSourceID field (set by TOrm.Create)
    // - note that if the Source record has just been added, fSourceID is not
    // set, so this method will fail: please call the other overloaded method
    function ManyDelete(const aClient: IRestOrm; aDestID: TID): boolean; overload;
    /// will retrieve the record associated with a particular Source/Dest pair
    // - will return TRUE if the pair was found
    // - in this case, all "through" columns are available in the TOrmMany
    // field instance
    function ManySelect(const aClient: IRestOrm; aSourceID, aDestID: TID): boolean; overload;
    /// will retrieve the record associated with the current source and a specified Dest
    // - source ID is taken from the fSourceID field (set by TOrm.Create)
    // - note that if the Source record has just been added, fSourceID is not
    // set, so this method will fail: please call the other overloaded method
    function ManySelect(const aClient: IRestOrm; aDestID: TID): boolean; overload;

    // get the SQL WHERE statement to be used to retrieve the associated
    // records according to a specified ID
    // - search for aID as Source ID if isDest is FALSE
    // - search for aID as Dest ID if isDest is TRUE
    // - the optional aAndWhereSql parameter can be used to add any additional
    // condition to the WHERE statement (e.g. 'Salary>:(1000): AND Salary<:(2000):')
    // according to TOrmMany properties - note that you should better use
    // such inlined parameters e.g. calling
    // ! FormatUtf8('Salary>? AND Salary<?', [], [1000, 2000])
    function IDWhereSql(const aClient: IRestOrm; aID: TID; isDest: boolean;
      const aAndWhereSql: RawUtf8 = ''): RawUtf8;
  end;


  { -------------------- TOrmVirtual Definitions }

  /// parent of all ORM virtual classes
  // - you can define a plain TOrm class as virtual if needed  - e.g.
  // inheriting from TOrmMany then calling VirtualTableExternalRegister() -
  // but using this class will seal its state to be virtual
  TOrmVirtual = class(TOrm);

  /// Record associated to a Virtual Table implemented in Delphi, with ID
  // forced at INSERT
  // - will use TOrmVirtualTableModule / TOrmVirtualTable / TOrmVirtualTableCursor
  // classes for a generic Virtual table mechanism on the Server side
  // - call Model.VirtualTableRegister() before TRestServer.Create on the
  // Server side (not needed for Client) to associate such a record with a
  // particular Virtual Table module, otherwise an exception will be raised:
  // ! Model.VirtualTableRegister(TOrmDali1, TOrmVirtualTableJson);
  TOrmVirtualTableForcedID = class(TOrmVirtual);

  /// an abstract base class, corresponding to an R-Tree table of values
  // - do not use this class, but either TOrmRTree or TOrmRTreeInteger
  // - an R-Tree is a special index that is designed for doing range queries.
  // R-Trees are most commonly used in geospatial systems where each entry is a
  // rectangle with minimum and maximum X and Y coordinates. Given a query
  // rectangle, an R-Tree is able to quickly find all entries that are contained
  // within the query rectangle or which overlap the query rectangle. This idea
  // is easily extended to three dimensions for use in CAD systems. R-Trees also
  // find use in time-domain range look-ups. For example, suppose a database
  // records the starting and ending times for a large number of events. A R-Tree
  // is able to quickly find all events, for example, that were active at any
  // time during a given time interval, or all events that started during a
  // particular time interval, or all events that both started and ended within
  // a given time interval. And so forth. See http:// www.sqlite.org/rtree.html
  // - any record which inherits from this class as TOrmRTree must have
  // only oftFloat (double) fields (or integer fields for TOrmRTreeInteger)
  // grouped by pairs, each as minimum- and maximum-value, up to 5 dimensions
  // (i.e. 11 columns, including the ID property)
  // - since SQLite version 3.24.0 (2018-06-04), R-Tree tables can have
  // auxiliary columns that store arbitrary data: such fields should appear after
  // the boundary columns, and have their property name starting with '_' in the
  // class definition; in both SQL and Where clause, the '_' will be trimmed - note
  // that you should better use the mormot.db.raw.sqlite3.static unit, since an
  // external SQLite3 .dll/.so library as supplied by the system may be outdated
  // - internally, the SQlite3 R-Tree extension will be implemented as a virtual
  // table, storing coordinates/values as 32-bit floating point (single - as
  // TOrmRTree kind of ORM classes) or 32-bit integers (as TOrmRTreeInteger),
  // but will make all R-Tree computation using 64-bit floating point (double)
  // - as with any virtual table, the ID: TID property must be set before adding
  // a TOrmRTree to the database, e.g. to link a R-Tree representation to
  // a regular TOrm table
  // - queries against the ID or the coordinate ranges are almost immediate: so
  // you can e.g. extract some coordinates box from the regular TOrm
  // table, then use a TOrmRTree joined query to make the process faster;
  // this is exactly what the TRestClient.RTreeMatch method offers - of
  // course Auxiliary Columns could avoid to make the JOIN and call RTreeMatch
  TOrmRTreeAbstract = class(TOrmVirtual)
  public
    /// override this class function to implement a custom SQL *_in() function
    // - in practice, an R-Tree index does not normally provide the exact answer
    // but merely reduces the set of potential answers from millions to dozens:
    // this method will be called from the *_in() SQL function to actually
    // return exact matches
    // - by default, the BLOB array will be decoded via the BlobToCoord class
    // procedure, and will create a SQL function from the class name
    //  - used e.g. by the TRestClient.RTreeMatch method
    class function ContainedIn(const BlobA, BlobB): boolean; virtual; abstract;
    /// will return 'MapBox_in' e.g. for TOrmMapBox
    class function RTreeSQLFunctionName: RawUtf8; virtual;
  end;

  /// this kind of record array can be used for direct floating-point
  // coordinates storage as in TOrmRTree.BlobToCoord
  TOrmTreeCoords = array[0..RTREE_MAX_DIMENSION - 1] of packed record
    min, max: double;
  end;

  /// a base record, corresponding to an R-Tree table of floating-point values
  // - for instance, the following class will define a 2 dimensional RTree
  // of floating point coordinates, and an associated MapBox_in() function:
  // ! TOrmMapBox = class(TOrmRTree)
  // ! protected
  // !   fMinX, fMaxX, fMinY, fMaxY: double;
  // ! published
  // !   property MinX: double read fMinX write fMinX;
  // !   property MaxX: double read fMaxX write fMaxX;
  // !   property MinY: double read fMinY write fMinY;
  // !   property MaxY: double read fMaxY write fMaxY;
  // ! end;
  // - since SQLite version 3.24.0, TOrmRTree can have auxiliary columns
  // that store arbitrary data, having their property name starting with '_'
  // (only in this class definition: SQL and Where clauses will trim it)
  TOrmRTree = class(TOrmRTreeAbstract)
  public
    /// override this class function to implement a custom SQL *_in() function
    // - by default, the BLOB array will be decoded via the BlobToCoord() class
    // procedure, and will create a SQL function from the class name
    //  - used e.g. by the TRestClient.RTreeMatch method
    class function ContainedIn(const BlobA, BlobB): boolean; override;
    /// override this class function to implement a custom box coordinates
    // from a given BLOB content
    // - by default, the BLOB array will contain a simple array of double
    // - but you can override this method to handle a custom BLOB field content,
    // intended to hold some kind of binary representation of the precise
    // boundaries of the object, and convert it into box coordinates as
    // understood by the ContainedIn() class function
    // - the number of pairs in OutCoord will be taken from the current number
    // of published double properties
    // - used e.g. by the TRest.RTreeMatch method
    class procedure BlobToCoord(const InBlob;
      var OutCoord: TOrmTreeCoords); virtual;
  end;

  /// this kind of record array can be used for direct 32-bit integer
  // coordinates storage as in TOrmRTreeInteger.BlobToCoord
  TOrmTreeCoordsInteger = array[0..RTREE_MAX_DIMENSION - 1] of packed record
    min, max: integer;
  end;

  /// a base record, corresponding to an R-Tree table of 32-bit integer values
  // - for instance, the following class will define a 2 dimensional RTree
  // of 32-bit integer coordinates, and an associated MapBox_in() function:
  // ! TOrmMapBox = class(TOrmRTree)
  // ! protected
  // !   fMinX, fMaxX, fMinY, fMaxY: integer;
  // ! published
  // !   property MinX: integer read fMinX write fMinX;
  // !   property MaxX: integer read fMaxX write fMaxX;
  // !   property MinY: integer read fMinY write fMinY;
  // !   property MaxY: integer read fMaxY write fMaxY;
  // ! end;
  // - since SQLite version 3.24.0, TOrmRTreeInteger can have auxiliary
  // columns that store arbitrary data, having their property name starting with '_'
  // (only in this class definition: SQL and Where clauses will trim it)
  TOrmRTreeInteger = class(TOrmRTreeAbstract)
  public
    /// override this class function to implement a custom SQL *_in() function
    // - by default, the BLOB array will be decoded via the BlobToCoord() class
    // procedure, and will create a SQL function from the class name
    //  - used e.g. by the TRest.RTreeMatch method
    class function ContainedIn(const BlobA, BlobB): boolean; override;
    /// override this class function to implement a custom box coordinates
    // from a given BLOB content
    // - by default, the BLOB array will contain a simple array of integer
    // - but you can override this method to handle a custom BLOB field content,
    // intended to hold some kind of binary representation of the precise
    // boundaries of the object, and convert it into box coordinates as
    // understood by the ContainedIn() class function
    // - the number of pairs in OutCoord will be taken from the current number
    // of published integer properties
    // - used e.g. by the TRest.RTreeMatch method
    class procedure BlobToCoord(const InBlob;
      var OutCoord: TOrmTreeCoordsInteger); virtual;
  end;

  /// a base record, corresponding to a FTS3 table, i.e. implementing full-text
  // - FTS3/FTS4/FTS5 tables are SQLite virtual tables allowing users to perform
  // full-text searches on a set of documents. The most common (and effective)
  // way to describe full-text searches is "what Google, Yahoo and Altavista do
  // with documents placed on the World Wide Web". Users input a term, or
  // series of terms, perhaps connected by a binary operator or grouped together
  // into a phrase, and the full-text query system finds the set of documents
  // that best matches those terms considering the operators and groupings the
  // user has specified. See http:// sqlite.org/fts3.html
  // - any record which inherits from this class must have only oftUtf8Text
  // (RawUtf8) fields - with Delphi 2009+, you can have string fields
  // - this record has its fID: TID property which may be published
  // as DocID, to be consistent with SQLite3 praxis, and reflect that it
  // points to an ID of another associated TOrm
  // - a good approach is to store your data in a regular TOrm table, then
  // store your text content in a separated FTS3 table, associated to this
  // TOrmFts3 table via its ID/DocID
  // - the ID/DocID property can be set when the record is added, to retrieve any
  // associated TOrm (note that for a TOrm record,
  // the ID property can't be set at adding, but is calculated by the engine)
  // - static tables don't handle TOrmFts3 classes
  // - by default, the FTS3 engine ignore all characters >= #80, but handle
  // low-level case insentivity (i.e. 'A'..'Z') so you must keep your
  // request with the same range for upper case
  // - by default, the "simple" tokenizer is used, but you can inherits from
  // TOrmFts3Porter class if you want a better English matching, using
  // the Porter Stemming algorithm, or TOrmFts3Unicode61 for Unicode
  // support - see http:// sqlite.org/fts3.html#tokenizer
  // - you can select either the FTS3 engine, or the more efficient (and new)
  // FTS4 engine (available since version 3.7.4), by using the TOrmFts4
  // type, or TOrmFts5 for the latest (and preferred) FTS5 engine
  // - in order to make FTS queries, use the dedicated TRest.FTSMatch
  // method, with the MATCH operator (you can use regular queries, but you must
  // specify 'RowID' instead of 'DocID' or 'ID' because of FTS3 Virtual
  // table specificity):
  // ! var IDs: TIDDynArray;
  // ! if FTSMatch(TOrmMyFTS3Table, 'text MATCH "linu*"', IDs) then
  // !  //  you have all matching IDs in IDs[]
  // - by convention, inherited class name could specify a custom stemming
  // algorithm by starting with "TOrmFts3", and adding the algorithm name as
  // suffix, e.g. TOrmFts3Porter will create a "tokenize=porter" virtual table
  TOrmFts3 = class(TOrmVirtual)
  public
     /// optimize the FTS3 virtual table
     // - this causes FTS3 to merge all existing index b-trees into a single large
     // b-tree containing the entire index. This can be an expensive operation,
     // but may speed up future queries. See http://sqlite.org/fts3.html#section_1_2
     // - this method must be called server-side
     // - returns TRUE on success
    class function OptimizeFTS3Index(const Server: IRestOrmServer): boolean;
     /// this DocID property map the internal Row_ID property
     // - but you can set a value to this property before calling the Add()
     // method, to associate this TOrmFts3 to another TOrm
     // - ID property is read-only, but this DocID property can be written/set
     // - internally, we use RowID in the SQL statements, which is compatible
     // with both TOrm and TOrmFts3 kind of table
    property DocID: TID
      read GetID write fID;
  end;

  /// this base class will create a FTS3 table using the Porter Stemming algorithm
  // - see http://sqlite.org/fts3.html#tokenizer
  // - will generate tokenize=porter by convention from the class name
  TOrmFts3Porter = class(TOrmFts3);

  /// this base class will create a FTS3 table using the Unicode61 Stemming algorithm
  // - see http://sqlite.org/fts3.html#tokenizer
  // - will generate tokenize=unicode61 by convention from the class name
  TOrmFts3Unicode61 = class(TOrmFts3);

  /// a base record, corresponding to a FTS4 table, which is an enhancement to FTS3
  // - FTS3 and FTS4 are nearly identical. They share most of their code in common,
  // and their interfaces are the same. The only difference is that FTS4 stores
  // some additional information about the document collection in two of new FTS
  // shadow tables. This additional information allows FTS4 to use certain
  // query performance optimizations that FTS3 cannot use. And the added information
  // permits some additional useful output options in the matchinfo() function.
  // - for newer applications, TOrmFts5 is recommended; though if minimal
  // disk usage or compatibility with older versions of SQLite are important,
  // then TOrmFts3 will usually serve just as well
  // - see http:// sqlite.org/fts3.html#section_1_1
  // - by convention, inherited class name could specify a custom stemming
  // algorithm by starting with "TOrmFts4", and adding the algorithm name as
  // suffix, e.g. TOrmFts'Porter will create a "tokenize=porter" virtual table
  TOrmFts4 = class(TOrmFts3)
  public
    /// this overriden method will create TRIGGERs for FTSWithoutContent()
    class procedure InitializeTable(const Server: IRestOrmServer;
      const FieldName: RawUtf8; Options: TOrmInitializeTableOptions); override;
  end;

  /// this base class will create a FTS4 table using the Porter Stemming algorithm
  // - see http://sqlite.org/fts3.html#tokenizer
  // - will generate tokenize=porter by convention from the class name
  TOrmFts4Porter = class(TOrmFts4);

  /// this base class will create a FTS4 table using the Unicode61 Stemming algorithm
  // - see http://sqlite.org/fts3.html#tokenizer
  // - will generate tokenize=unicode61 by convention from the class name
  TOrmFts4Unicode61 = class(TOrmFts4);

  /// a base record, corresponding to a FTS5 table, which is an enhancement to FTS4
  // - FTS5 is a new version of FTS4 that includes various fixes and solutions for
  // problems that could not be fixed in FTS4 without sacrificing backwards compatibility
  // - for newer applications, TOrmFts5 is recommended; though if minimal
  // disk usage or compatibility with older versions of SQLite are important,
  // then TOrmFts3/TOrmFts4 will usually serve just as well
  // - see https://sqlite.org/fts5.html#appendix_a
  // - by convention, inherited class name could specify a custom stemming
  // algorithm by starting with "TOrmFts5", and adding the algorithm name as
  // suffix, e.g. TOrmFts5Porter will create a "tokenize=porter" virtual table
  TOrmFts5 = class(TOrmFts4);

  /// this base class will create a FTS5 table using the Porter Stemming algorithm
  // - see https://sqlite.org/fts5.html#tokenizers
  // - will generate tokenize=porter by convention from the class name
  TOrmFts5Porter = class(TOrmFts5);

  /// this base class will create a FTS5 table using the Unicode61 Stemming algorithm
  // - see https://sqlite.org/fts5.html#tokenizers
  // - will generate tokenize=unicode61 by convention from the class name
  TOrmFts5Unicode61 = class(TOrmFts5);


  { -------------------- TOrmProperties Definitions }

  /// used by TOrmProperties to store internally its associated TModel instances
  // - allow almost O(1) search of a TOrmClass in a model
  TOrmPropertiesModelEntry = record
    /// one associated model
    Model: TOrmModel;
    /// the index in the Model.Tables[] array
    TableIndex: PtrInt;
    /// associated ORM parameters
    Properties: TOrmModelProperties;
  end;

  /// some information about a given TOrm class properties
  // - used internally by TOrm, via a global cache handled by this unit:
  // you can access to each record's properties via TOrm.OrmProps class
  // - such a global cache saves some memory for each TOrm instance,
  // and allows faster access to most wanted RTTI properties
  TOrmProperties = class(TOrmPropertiesAbstract)
  protected
    fTable: TOrmClass;
    fJoinedFields: TOrmPropInfoRttiIDObjArray;
    fJoinedFieldsTable: TOrmClassDynArray;
    fFilters: TSynFilterOrValidateObjArrayArray;
    fModel: array of TOrmPropertiesModelEntry; // associated TOrmModel instances
    fModelMax: integer;
    /// add an entry in fModel[] / fModelMax
    procedure InternalRegisterModel(aModel: TOrmModel; aTableIndex: integer;
      aProperties: TOrmModelProperties);
  public
    /// initialize the properties content
    constructor Create(aTable: TOrmClass);
    /// release associated used memory
    destructor Destroy; override;

    /// allow to validate length of all text published properties of this table
    // - the "index" attribute of the RawUtf8/string published properties could
    // be used to specify a maximum length for external VARCHAR() columns
    // - SQLite3 will just ignore this "index" information, but it could be
    // handy to be able to validate the value length before sending to the DB
    // - this method will create TSynValidateText corresponding to the maximum
    // field size specified by the "index" attribute, to validate before write
    // - will expect the "index" value to be in UTF-16 codepoints, unless
    // IndexIsUtf8Length is set to TRUE, indicating UTF-8 length in "index"
    procedure SetMaxLengthValidatorForTextFields(IndexIsUtf8Length: boolean = false);
    /// allow to filter the length of all text published properties of this table
    // - the "index" attribute of the RawUtf8/string published properties could
    // be used to specify a maximum length for external VARCHAR() columns
    // - SQLite3 will just ignore this "index" information, but it could be
    // handy to be able to filter the value length before sending to the DB
    // - this method will create TSynFilterTruncate corresponding to the maximum
    // field size specified by the "index" attribute, to filter before write
    // - will expect the "index" value to be in UTF-16 codepoints, unless
    // IndexIsUtf8Length is set to TRUE, indicating UTF-8 length in "index"
    procedure SetMaxLengthFilterForTextFields(IndexIsUtf8Length: boolean = false);

    /// register a custom filter (transformation) or validation rule to
    // the TSQMRecord class for a specified field
    // - this will be used by TOrm.Filter and TOrm.Validate
    // methods (in default implementation)
    // - will return FALSE in case of an invalid field index
    function AddFilterOrValidate(aFieldIndex: integer;
      aFilter: TSynFilterOrValidate): boolean; overload;
    /// register a custom filter (transformation) or validatation to the
    // TOrm class for a specified field
    // - this will be used by TOrm.Filter and TOrm.Validate
    // methods (in default implementation)
    // - will raise an EModelException if the field name does not exist
    procedure AddFilterOrValidate(const aFieldName: RawUtf8;
      aFilter: TSynFilterOrValidate); overload;

    /// list all TOrm fields of this TOrm
    // - ready to be used by TOrmTableJson.CreateFromTables()
    // - i.e. the class itself then, all fields of type oftID (excluding oftMany)
    property JoinedFields: TOrmPropInfoRttiIDObjArray
      read fJoinedFields;
    /// wrapper of all nested TOrm class of this TOrm
    // - ready to be used by TOrmTableJson.CreateFromTables()
    // - i.e. the class itself as JoinedFieldsTable[0], then, all nested
    // TOrm published properties (of type oftID, ergo excluding oftMany)
    // - equals nil if there is no nested TOrm property (i.e. JoinedFields=nil)
    property JoinedFieldsTable: TOrmClassDynArray
      read fJoinedFieldsTable;
    /// all TSynFilter or TSynValidate instances registered per each field
    // - since validation and filtering are used within some CPU-consuming
    // part of the framework (like UI edition), both filters and validation
    // rules are grouped in the same list
    property Filters: TSynFilterOrValidateObjArrayArray
      read fFilters;

  published
    /// the TOrm class
    property Table: TOrmClass
      read fTable;
    /// the Table name in the database, associated with this TOrm class
    // - 'TSql' or 'TOrm' chars are trimmed at the beginning of the ClassName
    // - or the ClassName is returned as is, if no 'TSql' or 'TOrm' at first
    property SqlTableName: RawUtf8
      read fSqlTableName;
    /// returns 'COL1,COL2' with all COL* set to all field names, including
    // RowID, TRecordVersion and BLOBs
    // - this won't change depending on the ORM settings: so it can be safely
    // computed here and not in TOrmModelProperties
    // - used e.g. by TRest.InternalListJson()
    property SqlTableRetrieveAllFields: RawUtf8
      read fSqlTableRetrieveAllFields;
  end;

  /// pointer to external database properties for ORM
  // - is used e.g. to allow a "fluent" interface for MapField() method
  POrmPropertiesMapping = ^TOrmPropertiesMapping;

  /// allow custom field mapping of a TOrm
  // - used e.g. for external database process, including SQL generation,
  // as implemented in the mormot.orm.sql.pas unit
  // - in end user code, mostly MapField/MapFields/Options methods
  // should be used, if needed as a fluent chained interface - other lower
  // level methods will be used by the framework internals
  {$ifdef USERECORDWITHMETHODS}
  TOrmPropertiesMapping = record
  {$else}
  TOrmPropertiesMapping = object
  {$endif USERECORDWITHMETHODS}
  private
    /// storage of main read-only properties
    fProps: TOrmProperties;
    fConnectionProperties: TObject;
    fTableName: RawUtf8;
    fRowIDFieldName: RawUtf8;
    fExtFieldNames: TRawUtf8DynArray;
    fExtFieldNamesUnQuotedSQL: TRawUtf8DynArray;
    fSql: TOrmModelPropertiesSql;
    fFieldNamesMatchInternal: TFieldBits;
    fOptions: TOrmPropertiesMappingOptions;
    fAutoComputeSql: boolean;
    fMappingVersion: cardinal;
    /// fill fRowIDFieldName/fSql with the current information
    procedure ComputeSql;
  public
    /// add a custom field mapping
    // - will re-compute all needed SQL statements as needed, and initialize
    // fSortedFieldsName[] and fSortedFieldsIndex[] internal sorted arrays
    // - can be used e.g. as
    // ! aModel.Props[TOrmMyExternal].ExternalDB.MapField('IntField', 'ExtField');
    // - since it returns a POrmPropertiesMapping instance, you can
    // chain MapField().MapField().MapField(); calls to map several fields
    function MapField(
      const InternalName, ExternalName: RawUtf8): POrmPropertiesMapping;
    /// call this method to ensure that all fields won't conflict with a SQL
    // keyword for the given database
    // - by default, no check is performed: you can use this method to ensure
    // that all field names won't conflict with a SQL reserved keyword: such
    // fields will be identified and automatically mapped as fieldname_
    // - can be used e.g. as
    // ! aModel.Props[TOrmMyExternal].ExternalDB.
    // !   MapField('IntField', 'ExtField').
    // !   MapAutoKeywordFields;
    // - will in fact include the rpmAutoMapKeywordFields flag in Options
    // - since it returns a POrmPropertiesMapping instance, you can
    // chain MapField().MapAutoKeywordFields.MapField(); calls to map several fields
    function MapAutoKeywordFields: POrmPropertiesMapping;
    /// specify some advanced options for the field mapping
    // - see TOrmPropertiesMappingOptions for all possibilities
    // - can be used e.g. as
    // ! aModel.Props[TOrmMyExternal].ExternalDB.
    // !   MapField('IntField', 'ExtField').
    // !   SetOptions([rpmNoCreateMissingTable, rpmNoCreateMissingField]);
    // - since it returns a POrmPropertiesMapping instance, you can
    // chain MapField().SetOptions().MapField(); calls to map several fields
    function SetOptions(
      aOptions: TOrmPropertiesMappingOptions): POrmPropertiesMapping;
    /// add several custom field mappings
    // - can be used e.g. as
    // ! aModel.Props[TOrmMyExternal].ExternalDB.
    // !   MapFields(['IntField1', 'ExtField1', 'IntField2', 'ExtField2']);
    // - will re-compute all needed SQL statements as needed, and initialize
    // fSortedFieldsName[] and fSortedFieldsIndex[] internal sorted arrays
    // - is slightly faster than several chained MapField() calls, since SQL
    // will be computed only once
    function MapFields(
      const InternalExternalPairs: array of RawUtf8): POrmPropertiesMapping;
  public
    /// initialize the field mapping for a given TOrm
    // - if AutoComputeSql is true, will pre-compute all needed SQL from the
    // supplied information
    // - will left void fSortedFieldsName[] and fSortedFieldsIndex[], to disable
    // custom field mapping
    procedure Init(Table: TOrmClass; const MappedTableName: RawUtf8;
      MappedConnection: TObject; AutoComputeSql: boolean;
      MappingOptions: TOrmPropertiesMappingOptions);
    /// map a field name from its internal name to its external name
    // - raise an EOrmException if the supplied field name is not defined in
    // the TOrm as ID or a published property
    function InternalToExternal(const FieldName: RawUtf8): RawUtf8; overload;
    /// map a field name from its internal name to its external name
    // - raise an EOrmException if the supplied field name is not defined in
    // the TOrm as ID or a published property
    function InternalToExternal(BlobField: PRttiProp): RawUtf8; overload;
    /// map a CSV list of field names from its internals to its externals values
    // - raise an EOrmException if any of the supplied field name is not defined
    // in the TOrm as ID or as property (RowIDFieldName or FieldNames[])
    // - to be used for a simple CSV (e.g. for INSERT/SELECT statements):
    // ! ExtCsv := InternalCsvToExternalCsv('ID,Name');
    // - or for a more complex CSV (e.g. for UPDATE statements);
    // ! ExtCsv := InternalCsvToExternalCsv('ID=?,Name=?', '=?, '=?');
    function InternalCsvToExternalCsv(const CsvFieldNames: RawUtf8;
      const Sep: RawUtf8 = ','; const SepEnd: RawUtf8 = ''): RawUtf8;
    /// create a list of external field names, from the internal field names
    // - raise an EOrmException if any of the supplied field name is not defined
    // in the TOrm as ID or a published property
    // - if IntFieldIndex is set, it will store an array of internal field
    // indexes, i.e. -1 for ID or index in in FieldNames[] for other fields
    procedure InternalToExternalDynArray(const IntFieldNames: array of RawUtf8;
      out result: TRawUtf8DynArray; IntFieldIndex: PIntegerDynArray = nil);
    /// map an external field name into its internal field name
    // - return '' if the external field name is not RowIDFieldName nor in
    // FieldNames[]
    function ExternalToInternalOrNull(const ExtFieldName: RawUtf8): RawUtf8;
    /// map an external field name into its internal field index
    // - returns the index >=0 in FieldNames[] for a matching external field
    // - returns -1 if the field name is RowIDFieldName
    // - returns -2 if the field name is not mapped
    function ExternalToInternalIndex(const ExtFieldName: RawUtf8): integer;
    /// append a field name to a RawUtf8 Text buffer
    // - if FieldIndex=VIRTUAL_TABLE_ROWID_COLUMN (-1), appends RowIDFieldName
    // - on error (i.e. if FieldIndex is out of range) will return TRUE
    // - otherwise, will return FALSE and append the external field name to Text
    function AppendFieldName(FieldIndex: integer; var Text: RawUtf8): boolean; overload;
    /// append a field name to a TJsonWriter instance
    // - if FieldIndex=VIRTUAL_TABLE_ROWID_COLUMN (-1), appends RowIDFieldName
    // - on error (i.e. if FieldIndex is out of range) will return TRUE
    // - otherwise, will return FALSE and append the external field name to Text
    function AppendFieldName(FieldIndex: integer; WR: TJsonWriter): boolean; overload;
    /// return the field name as RawUtf8 value
    // - if FieldIndex=VIRTUAL_TABLE_ROWID_COLUMN (-1), appends RowIDFieldName
    // - otherwise, will return the external field name
    function FieldNameByIndex(FieldIndex: integer): RawUtf8;

    /// opaque object used on the Server side to specify e.g. the DB connection
    // - will define such a generic TObject, to avoid any unecessary type
    // dependency to other units, e.g. mormot.db.* or mormot.rest.*
    // - in practice, will be assigned by VirtualTableExternalRegister() to
    // a TSqlDBConnectionProperties instance in mormot.orm.sql.pas, or by
    // StaticMongoDBRegister() to a TMongoCollection instance, or by
    // TDDDRepositoryRestObjectMapping.Create to its associated TRest
    // - in ORM context, equals nil if the table is internal to SQLite3:
    // ! if Server.Model.Props[TOrmArticle].ExternalDB.ConnectionProperties = nil then
    // !   // this is not an external table, since Init() was not called
    property ConnectionProperties: TObject
      read fConnectionProperties;
    /// the associated TOrmProperties
    property Properties: TOrmProperties
      read fProps;
    /// used on the Server side to specify the external DB table name
    // - e.g. for including a schema name or an existing table name, with an
    // OleDB/MSSQL/Oracle/MySQL/PostgreSQL/Jet/SQLite3 backend
    // - equals SqlTableName by default (may be overridden e.g. by mormot.orm.sql's
    // VirtualTableExternalRegister procedure)
    property TableName: RawUtf8
      read fTableName;
    /// pre-computed SQL statements for this external TOrm in this model
    // - you can use those SQL statements directly with the external engine
    // - filled if AutoComputeSql was set to true in Init() method
    property SQL: TOrmModelPropertiesSql
      read fSql;
    /// the ID/RowID customized external field name, if any
    // - is 'ID' by default, since 'RowID' is a reserved column name for some
    // database engines (e.g. Oracle)
    // - can be customized e.g. via
    // ! aModel.Props[TOrmMyExternal].ExternalDB.MapField('ID', 'ExternalID');
    property RowIDFieldName: RawUtf8
      read fRowIDFieldName;
    /// the external field names, following fProps.Props.Field[] order
    // - excluding ID/RowID field, which is stored in RowIDFieldName
    property ExtFieldNames: TRawUtf8DynArray
      read fExtFieldNames;
    /// the unquoted external field names, following fProps.Props.Field[] order
    // - excluding ID/RowID field, which is stored in RowIDFieldName
    // - in respect to ExtFieldNames[], this array will never quote the field name
    property ExtFieldNamesUnQuotedSQL: TRawUtf8DynArray
      read fExtFieldNamesUnQuotedSQL;
    /// each bit set, following fProps.Props.Field[]+1 order (i.e. 0=ID,
    // 1=Field[0], ...), indicates that this external field name
    // has not been mapped
    property FieldNamesMatchInternal: TFieldBits
      read fFieldNamesMatchInternal;
    /// how the mapping process will take place
    property Options: TOrmPropertiesMappingOptions
      read fOptions;
    /// each time MapField/MapFields is called, this number will increase
    // - can be used to track mapping changes in real time
    property MappingVersion: cardinal
      read fMappingVersion;
  end;



  { -------------------- TOrmModel TOrmModelProperties Definitions }

  /// dynamic array of TOrmModelProperties
  // - used by TOrmModel to store the non-shared information of all its tables
  TOrmModelPropertiesObjArray = array of TOrmModelProperties;

  /// ORM properties associated to a TOrm within a given model
  // - "stable" / common properties derivated from RTTI are shared in the
  // TOrmProperties instance
  // - since the same TOrm can be defined in several models, with diverse
  // implementation patterns (e.g. internal in one, external in another),
  // this class is used to regroup all model-specific settings, like SQL
  // pre-generated patterns or external DB properties
  TOrmModelProperties = class
  protected
    fProps: TOrmProperties;
    fKind: TOrmVirtualKind;
    fModel: TOrmModel;
    fTableIndex: integer;
    fFTSWithoutContentTableIndex: integer;
    fFTSWithoutContentFields: RawUtf8;
    procedure SetKind(Value: TOrmVirtualKind);
    function GetProp(const PropName: RawUtf8): TOrmPropInfo;
  public
    /// pre-computed SQL statements for this TOrm in this model
    // - those statements will work for internal tables, not for external
    // tables with mapped table or fields names
    SQL: TOrmModelPropertiesSql;
    /// allow SQL process for one external TOrm in this model
    ExternalDB: TOrmPropertiesMapping;
    /// will by-pass automated table and field creation for this TOrm
    // - may be used e.g. when the TOrm is in fact mapped into a View,
    // or is attached as external table and not a real local table
    NoCreateMissingTable: boolean;

    /// initialize the ORM properties from the TOrm RTTI and the supplied
    // TOrmModel
    constructor Create(aModel: TOrmModel; aTable: TOrmClass;
      aKind: TOrmVirtualKind);
    /// clone ORM properties from an existing TOrmModelProperties to
    // another model
    constructor CreateFrom(aModel: TOrmModel; aSource: TOrmModelProperties);

    /// compute the SQL statement to be executed for a specific SELECT
    // - non simple fields (e.g. BLOBs) will be excluded if SelectFields='*'
    // - by default, will return the SELECT statement to be used for internal
    // virtual SQLite3 table - but if ExternalTable is TRUE, then it will
    // compute a SELECT matching ExternalDB settings
    function SqlFromSelectWhere(const SelectFields, Where: RawUtf8): RawUtf8;
    /// define if a FTS4 virtual table will not store its content, but will
    // be defined as an "external content" FTS4/FTS5 table
    // - see https://www.sqlite.org/fts3.html#section_6_2_2
    // - the virtual table will be created with content="ContentTableName",
    // and all fields of the FTS4/FTS5 table
    // - by design, all fields of the FTS4/FTS5 table should exist in the source
    // ContentTable - otherwise an exception is raised
    // - the indexed text will be assigned to the FTS4/FTS5 table, using
    // triggers generated by TOrmFts4.InitializeTable at table creation
    // - note that FTS3 does not support this feature
    procedure FTS4WithoutContent(ContentTable: TOrmClass);

    /// the table index of this TOrm in the associated Model
    property TableIndex: integer
      read fTableIndex;
    /// direct access to a property RTTI information, by name
    property Prop[const PropName: RawUtf8]: TOrmPropInfo
      read GetProp; default;
  published
    /// the shared TOrmProperties information of this TOrm
    // - as retrieved from RTTI
    property Props: TOrmProperties
      read fProps;
    /// define if is a normal table ( ovkSQLite3), an FTS/R-Tree virtual
    // table or a custom TOrmVirtualTable*ID (rCustomForcedID/rCustomAutoID)
    // - when set, all internal SQL statements will be (re)created, depending of
    // the expected ID/RowID column name expected (i.e. Sql.TableSimpleFields[]
    // and SqlSelectAll[] - SQLUpdateSet and SQLInsertSet do not include ID)
    property Kind: TOrmVirtualKind
      read fKind write SetKind default ovkSQLite3;
  end;

  /// how a TOrmModel stores a foreign link to be cascaded
  TOrmModelReference = record
    /// refers to the source TOrmClass as model Tables[] index
    TableIndex: integer;
    /// the property
    FieldType: TOrmPropInfo;
    /// the target TOrmClass of the field
    FieldTable: TOrmClass;
    /// the target TOrmClass of the field, from its Tables[] index
    FieldTableIndex: integer;
    /// TRUE if this field is a TRecordReferenceToBeDeleted
    CascadeDelete: boolean;
  end;

  POrmModelReference = ^TOrmModelReference;

  TOrmModelReferenceDynArray = array of TOrmModelReference;

  /// a Database Model (in a MVC-driven way), for storing some tables types
  // as TOrm classes
  // - share this Model between TRest Client and Server
  // - use this class to access the table properties: do not rely on the
  // low-level database methods (e.g. TSqlDataBase.GetTableNames), since the
  // tables may not exist in the main SQLite3 database, but in-memory or external
  // - don't modify the order of Tables inside this Model, if you publish
  // some TRecordReference property in any of your tables
  TOrmModel = class
  private
    fTables: TOrmClassDynArray;
    fRoot: RawUtf8;
    fRootUpper: RawUtf8;
    fTablesMax: integer;
    fTableProps: TOrmModelPropertiesObjArray;
    fCustomCollationForAll: array[TOrmFieldType] of RawUtf8;
    fOnClientIdle: TOnIdleSynBackgroundThread;
    /// contains the TRest caller of CreateOwnedStream()
    fOwner: TObject;
    /// for every table, contains a locked record list
    // - very fast, thanks to the use of a dynamic array with one entry by table
    fLocks: TOrmLocksDynArray;
    /// for fastest SQL Table name lookup via O(log(n)) binary search
    fSortedTablesNameUpper: TRawUtf8DynArray;
    fSortedTablesNameIndex: TIntegerDynArray;
    /// will contain the registered TOrmVirtualTableClass modules
    fVirtualTableModule: array of TClass;
    /// all TRecordReference and TOrm properties of the model
    fRecordReferences: TOrmModelReferenceDynArray;
    fIDGenerator: TSynUniqueIdentifierGenerators;
    procedure SetRoot(const aRoot: RawUtf8);
    procedure SetTableProps(aIndex: integer);
    function GetTableProps(aClass: TOrmClass): TOrmModelProperties;
    /// get the enumerate type information about the possible actions to be
    function GetLocks(aTable: TOrmClass): POrmLocks;
    function GetTable(const SqlTableName: RawUtf8): TOrmClass;
    function GetTableExactIndex(const TableName: RawUtf8): PtrInt;
    function GetTableExactClass(const TableName: RawUtf8): TOrmClass;
  public
    /// initialize the Database Model
    // - set the Tables to be associated with this Model, as TOrm classes
    // - set the optional Root URI path of this Model
    // - initialize the fIsUnique[] array from "stored AS_UNIQUE" (i.e. "stored
    // false") published properties of every TOrmClass
    constructor Create(const Tables: array of TOrmClass;
      const aRoot: RawUtf8 = 'root'); reintroduce; overload;
    /// you should not use this constructor, but one of the overloaded versions,
    // specifying the associated TOrmClass
    constructor Create; reintroduce; overload;
    /// clone an existing Database Model
    // - all supplied classes won't be redefined as non-virtual:
    // VirtualTableExternalRegister explicit calls are not mandatory here
    constructor Create(CloneFrom: TOrmModel); reintroduce; overload;
    /// release associated memory
    destructor Destroy; override;
    /// add the class if it doesn't exist yet
    // - return index in Tables[] if not existing yet and successfully added (in this case,
    // aTableIndexCreated^ is set to the newly created index in Tables[])
    // - supplied class will be redefined as non-virtual: VirtualTableExternalRegister
    // explicit call is to be made if table should be managed as external
    // - return FALSE if already present, or TRUE if was added to the internal list
    function AddTable(aTable: TOrmClass;
      aTableIndexCreated: PInteger = nil): boolean;
    /// add the class if it doesn't exist yet as itself or as inherited class
    // - similar to AddTable(), but any class inheriting from the supplied type
    // will be considered as sufficient
    // - return the class which has been added, or was already there as
    // inherited, so that could be used for further instance creation:
    // ! fAuthUserClass := Model.AddTableInherited(TAuthUser);
    function AddTableInherited(aTable: TOrmClass): pointer;
    /// return any class inheriting from the given table in the model
    // - if the model does not contain such table, supplied aTable is returned
    function GetTableInherited(aTable: TOrmClass): TOrmClass;
    /// get the index of aTable in Tables[]
    // - returns -1 if the table is not in the model
    function GetTableIndex(aTable: TOrmClass): PtrInt; overload;
    /// get the index of any class inherithing from aTable in Tables[]
    // - returns -1 if no table is matching in the model
    function GetTableIndexInheritsFrom(aTable: TOrmClass): PtrInt;
    /// get the index of aTable in Tables[]
    // - raise an EModelException if the table is not in the model
    function GetTableIndexExisting(aTable: TOrmClass): PtrInt;
    /// get the index of a table in Tables[]
    // - expects SqlTableName to be SQL-like formatted (i.e. without TOrm[Record])
    function GetTableIndex(const SqlTableName: RawUtf8): PtrInt; overload;
    /// get the index of a table in Tables[], optionally raising EModelException
    function GetTableIndexSafe(aTable: TOrmClass;
      RaiseExceptionIfNotExisting: boolean): PtrInt;
    /// get the index of a table in Tables[]
    // - expects SqlTableName to be SQL-like formatted (i.e. without TOrm[Record])
    function GetTableIndexPtr(SqlTableName: PUtf8Char): PtrInt;
    /// return the UTF-8 encoded SQL source to create the table
    function GetSqlCreate(aTableIndex: integer): RawUtf8;
    /// return the UTF-8 encoded SQL source to add the corresponding field
    // via a "ALTER TABLE" statement
    function GetSqlAddField(aTableIndex, aFieldIndex: integer): RawUtf8;
    /// return the TRecordReference pointing to the specified record
    function RecordReference(Table: TOrmClass; ID: TID): TRecordReference;
    /// return the table class correspondig to a TRecordReference
    function RecordReferenceTable(const Ref: TRecordReference): TOrmClass;
    /// return TRUE if the specified field of this class was marked as unique
    // - an unique field is defined as "stored AS_UNIQUE" (i.e. "stored false")
    // in its property definition
    // - reflects the internal private fIsUnique propery
    function GetIsUnique(aTable: TOrmClass; aFieldIndex: integer): boolean;
    /// try to retrieve a table index from a SQL statement
    // - naive search of '... FROM TableName' pattern in the supplied SQL,
    // using GetTableNameFromSqlSelect() function
    // - if EnsureUniqueTableInFrom is TRUE, it will check that only one Table
    // is in the FROM clause, otherwise it will return the first Table specified
    function GetTableIndexFromSqlSelect(const SQL: RawUtf8;
      EnsureUniqueTableInFrom: boolean): integer;
    /// try to retrieve one or several TOrmClass from a SQL statement
    // - naive search of '... FROM Table1,Table2' pattern in the supplied SQL,
    // using GetTableNamesFromSqlSelect() function
    function GetTablesFromSqlSelect(const SQL: RawUtf8): TOrmClassDynArray;
    /// try to retrieve one or several table index from a SQL statement
    // - naive search of '... FROM Table1,Table2' pattern in the supplied SQL,
    // using GetTableNamesFromSqlSelect() function
    function GetTableIndexesFromSqlSelect(const SQL: RawUtf8): TIntegerDynArray;
    /// check if the supplied URI matches the model's Root property
    // - allows sub-domains, e.g. if Root='root/sub1', then '/root/sub1/toto' and
    // '/root/sub1?n=1' will match, whereas '/root/sub1nope/toto' won't
    // - the returned enumerates allow to check if the match was exact (e.g.
    // 'root/sub' matches exactly Root='root'), or with character case
    // approximation (e.g. 'Root/sub' approximates Root='root')
    function UriMatch(const Uri: RawUtf8): TRestModelMatch;
    /// returns the URI corresponding to a given table, i.e. 'root/table'
    function GetUri(aTable: TOrmClass): RawUtf8;
    /// return the 'root/table/ID' URI
    function GetUriID(aTable: TOrmClass; aID: TID): RawUtf8;
    /// return the 'root/table/ID/method' URI
    function GetUriCallBack(const aMethodName: RawUtf8;
      aTable: TOrmClass; aID: TID): RawUtf8;
    /// compute the SQL statement to be executed for a specific SELECT on Tables
    // - you can set multiple Table class in Tables: the statement will contain the
    // table name ('SELECT T1.F1,T1.F2,T1.F3,T2.F1,T2.F2 FROM T1,T2 WHERE ..' e.g.)
    function SqlFromSelectWhere(const Tables: array of TOrmClass;
      const SqlSelect, SqlWhere: RawUtf8): RawUtf8;
    /// set a custom SQlite3 text column collation for all fields of a given
    // type for all TOrm of this model
    // - can be used e.g. to override ALL default COLLATE SYSTEMNOCASE of RawUtf8,
    // or COLLATE ISO8601 for TDateTime, and let the generated SQLite3 file be
    // available outside the scope of mORMot's SQLite3 engine
    // - collations defined within our mormot.db.raw.sqlite3 unit are the SQLite3
    // standard BINARY, NOCASE, RTRIM and our custom SYSTEMNOCASE, UNICODENOCASE,
    // ISO8601, WIN32CASE, WIN32NOCASE: if you want to use the NOCASE, write:
    // ! SetCustomCollationForAll(oftUtf8Text, 'NOCASE');
    // - shall be set on both Client and Server sides for consistency
    procedure SetCustomCollationForAll(aFieldType: TOrmFieldType;
      const aCollationName: RawUtf8);
    /// allow to validate length of all text published properties of all tables
    // of this model
    // - the "index" attribute of the RawUtf8/string published properties could
    // be used to specify a maximum length for external VARCHAR() columns
    // - SQLite3 will just ignore this "index" information, but it could be
    // handy to be able to validate the value length before sending to the DB
    // - this method will create TSynValidateText corresponding to the maximum
    // field size specified by the "index" attribute, to validate before write
    // - will expect the "index" value to be in UTF-16 codepoints, unless
    // IndexIsUtf8Length is set to TRUE, indicating UTF-8 length
    procedure SetMaxLengthValidatorForAllTextFields(IndexIsUtf8Length: boolean = false);
    /// allow to filter the length of all text published properties of all tables
    // of this model
    // - the "index" attribute of the RawUtf8/string published properties could
    // be used to specify a maximum length for external VARCHAR() columns
    // - SQLite3 will just ignore this "index" information, but it could be
    // handy to be able to filter the value length before sending to the DB
    // - this method will create TSynFilterTruncate corresponding to the maximum
    // field size specified by the "index" attribute, to validate before write
    // - will expect the "index" value to be in UTF-16 codepoints, unless
    // IndexIsUtf8Length is set to TRUE, indicating UTF-8 length
    procedure SetMaxLengthFilterForAllTextFields(IndexIsUtf8Length: boolean = false);
    /// customize the TDocVariant options for all variant published properties
    // - will change the TOrmPropInfoRttiVariant.DocVariantOptions value
    // - use e.g. as SetVariantFieldDocVariantOptions(JSON_FAST_EXTENDED)
    // - see also TOrmNoCaseExtended root class
    procedure SetVariantFieldsDocVariantOptions(const Options: TDocVariantOptions);
    /// force a given table to use a TSynUniqueIdentifierGenerator for its IDs
    /// - will initialize a generator for the supplied table, using the
    // given 16-bit process identifier
    // - you can supply an obfuscation key, which should be shared for the
    // whole system, so that you may use FromObfuscated/ToObfuscated methods
    function SetIDGenerator(aTable: TOrmClass;
      aIdentifier: TSynUniqueIdentifierProcess;
      const aSharedObfuscationKey: RawUtf8 = '';
      aSharedObfuscationKeyNewKdf: integer = 0): TSynUniqueIdentifierGenerator;
    /// returns the TSynUniqueIdentifierGenerator associated to a table, if any
    function GetIDGenerator(aTable: TOrmClass): TSynUniqueIdentifierGenerator;
    /// low-level access to the TSynUniqueIdentifierGenerator instances, if any
    property IDGenerator: TSynUniqueIdentifierGenerators
      read fIDGenerator;

    /// register a Virtual Table module for a specified class
    // - to be called server-side only (Client don't need to know the virtual
    // table implementation details, and it will increase the code size)
    // - aClass parameter could be either a TOrmVirtual class, either
    // a TOrm class which has its kind set to rCustomForcedID or
    // rCustomAutoID (e.g. TOrmMany calling VirtualTableExternalRegister)
    // - aModule is expected to be a TOrmVirtualTableClass type definition
    // - optional aExternalTableName, aExternalDataBase and aMappingOptions can
    // be used to specify e.g. connection parameters as expected by mormot.orm.sql
    // - call it before TRestServer.Create()
    function VirtualTableRegister(aClass: TOrmClass; aModule: TClass;
      const aExternalTableName: RawUtf8 = ''; aExternalDataBase: TObject = nil;
      aMappingOptions: TOrmPropertiesMappingOptions = []): boolean;
    /// retrieve a Virtual Table module associated to a class
    // - returns a TOrmVirtualTableClass type definition
    function VirtualTableModule(aClass: TOrmClass): TClass;

    /// create a New TOrm instance for a specific Table
    // - expects SqlTableName to be SQL-like formated (i.e. without TOrm[Record])
    // - use this to create a working copy of a table's record, e.g.
    // - don't forget to Free it when not used any more (use a try...finally
    // block)
    // - it's prefered in practice to directly call TOrm*.Create()
    // in your code
    function NewRecord(const SqlTableName: RawUtf8): TOrm;

    /// lock a record
    // - returns true on success, false if was already locked
    function Lock(aTable: TOrmClass; aID: TID): boolean; overload;
    /// lock a record
    // - returns true on success, false if was already locked
    function Lock(aTableIndex: integer; aID: TID): boolean; overload;
    /// lock a record
    // - returns true on success, false if was already locked
    function Lock(aRec: TOrm): boolean; overload;
    /// unlock a specified record
    // - returns true on success, false if was not already locked
    function UnLock(aTable: TOrmClass; aID: TID): boolean; overload;
    /// unlock a specified record
    // - returns true on success, false if was not already locked
    function UnLock(aTableIndex: integer; aID: TID): boolean; overload;
    /// unlock a specified record
    // - returns true on success, false if was not already locked
    function UnLock(aRec: TOrm): boolean; overload;
    /// unlock all previously locked records
    procedure UnLockAll;
    /// return true if a specified record is locked
    function isLocked(aTable: TOrmClass; aID: TID): boolean; overload;
    /// return true if a specified record is locked
    function isLocked(aRec: TOrm): boolean; overload;
    /// delete all the locked IDs entries, after a specified time
    // - to be used to release locked records if the client crashed
    // - default value is 30 minutes, which seems correct for common usage
    procedure PurgeOlderThan(MinutesFromNow: cardinal = 30);

    /// the associated ORM information for a given TOrm class
    // - raise an EModelException if aClass is not declared within this model
    // - returns the corresponding TableProps[] item if the class is known
    property Props[aClass: TOrmClass]: TOrmModelProperties
      read GetTableProps;
    /// get the classes list (TOrm descendent) of all available tables
    property Tables: TOrmClassDynArray
      read fTables;
    /// get a class from a table name
    // - expects SqlTableName to be SQL-like formated (i.e. without TOrm[Record])
    property Table[const SqlTableName: RawUtf8]: TOrmClass
      read GetTable; default;
    /// get a class from a table TableName (don't truncate TOrm* if necessary)
    property TableExact[const TableName: RawUtf8]: TOrmClass
      read GetTableExactClass;
    /// the maximum index of TableProps[] class properties array
    property TablesMax: integer
      read fTablesMax;

    /// returns the Root property, or '' if the instance is nil
    function SafeRoot: RawUtf8;
    /// compute the URI for a class in this Model, as 'ModelRoot/SqlTableName'
    // - set also GetUri/GetUriID/GetUriCallback methods
    property Uri[aClass: TOrmClass]: RawUtf8
      read GetUri;

    /// this property value is used to auto free the database Model class
    // - set this property after Owner.Create() in order to have
    // Owner.Destroy autofreeing this instance
    // - Owner is typically a TRest or a TRestOrm class
    property Owner: TObject
      read fOwner write fOwner;
    /// for every table, contains a locked record list
    // - very fast, thanks to the use one TOrmLocks entry by table
    property Locks: TOrmLocksDynArray
      read fLocks;
    /// this array contain all TRecordReference and TOrm properties
    // existing in the database model
    // - used in TRestServer.Delete() to enforce relational database coherency
    // after deletion of a record: all other records pointing to it will be
    // reset to 0 or deleted (if CascadeDelete is true)
    property RecordReferences: TOrmModelReferenceDynArray
      read fRecordReferences;
    /// set a callback event to be executed in loop during client remote
    // blocking process, e.g. to refresh the UI during a somewhat long request
    // - will be passed to TRestClientUri.OnIdle property by
    // TRestClientUri.RegisteredClassCreateFrom() method, if applying
    property OnClientIdle: TOnIdleSynBackgroundThread
      read fOnClientIdle write fOnClientIdle;
  published
    /// the Root URI path of this Database Model
    // - this textual value will be used directly to compute the URI for REST
    // routing, so it should contain only URI-friendly characters,
    // i.e. only alphanumerical characters, excluding e.g. space or '+',
    // otherwise an EModelException is raised
    // - use SafeRoot function is you are not sure that the TOrmModel is not nil
    property Root: RawUtf8
      read fRoot write SetRoot;
    /// the associated ORM information about all handled TOrm class properties
    // - this TableProps[] array will map the Tables[] array, and will allow
    // fast direct access to the Tables[].OrmProps values
    property TableProps: TOrmModelPropertiesObjArray
      read fTableProps;
  end;


  { -------------------- TRestCache Definition }

  /// implement a fast TOrm cache, per ID, at the TRest level
  // - purpose of this caching mechanism is to speed up retrieval of some common
  // values at either Client or Server level (like configuration settings)
  // - only caching synchronization is about the following RESTful basic commands:
  // RETRIEVE, ADD, DELETION and UPDATE (that is, a complex direct SQL UPDATE
  // or via TOrmMany pattern won't be taken into account)
  // - only Simple fields are cached: e.g. the BLOB fields are not stored
  // - this cache is thread-safe (access is locked per table)
  // - this caching will be located at the TRest level, that is no automated
  // synchronization is implemented between TRestClient and TRestServer:
  // you shall ensure that your code won't fail due to this restriction
  TRestCache = class
  protected
    fRest: IRestOrm;
    fModel: TOrmModel;
    /// fCache[] follows fRest.Model.Tables[] array: one entry per TOrm
    fCache: TRestCacheEntryDynArray;
  public
    /// create a cache instance
    // - the associated TOrmModel will be used internally
    constructor Create(const aRest: IRestOrm); reintroduce;
    /// release the cache instance
    destructor Destroy; override;
    /// flush the cache
    // - this will flush all stored JSON content, but keep the settings
    // (SetCache/SetTimeOut) as before
    procedure Flush; overload;
    /// flush the cache for a given table
    // - this will flush all stored JSON content, but keep the settings
    // (SetCache/SetTimeOut) as before for this table
    procedure Flush(aTable: TOrmClass); overload;
    /// flush the cache for a given record
    // - this will flush the stored JSON content for this record (and table
    // settings will be kept)
    procedure Flush(aTable: TOrmClass; aID: TID); overload;
    /// flush the cache for a set of specified records
    // - this will flush the stored JSON content for these record (and table
    // settings will be kept)
    procedure Flush(aTable: TOrmClass; const aIDs: array of TID); overload;
    /// flush the cache, and destroy all settings
    // - this will flush all stored JSON content, AND destroy the settings
    // (SetCache/SetTimeOut) to default (i.e. no cache enabled)
    procedure Clear;
    // - will fill the internal JSON cache of a given Table with data coming
    // from a REST query
    // - returns the number of TOrm items actually cached
    // - may be handy to pre-load a set of values (e.g. a lookup table) from a
    // single REST query, without waiting for each record to be retrieved
    function FillFromQuery(aTable: TOrmClass;
      const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const): integer;
    /// activate the internal caching for a whole Table
    // - any cached item of this table will be flushed
    // - return true on success
    function SetCache(aTable: TOrmClass): boolean; overload;
    /// activate the internal caching for a given TOrm
    // - if this item is already cached, do nothing
    // - return true on success
    function SetCache(aTable: TOrmClass; aID: TID): boolean; overload;
    /// activate the internal caching for a set of specified TOrm
    // - if these items are already cached, do nothing
    // - return true on success
    function SetCache(aTable: TOrmClass; const aIDs: array of TID):
      boolean; overload;
    /// activate the internal caching for a given TOrm
    // - will cache the specified aRecord.ID item
    // - if this item is already cached, do nothing
    // - return true on success
    function SetCache(aRecord: TOrm): boolean; overload;
    /// set the internal caching time out delay (in ms) for a given table
    // - actual resolution is 512 ms
    // - time out setting is common to all items of the table
    // - if aTimeOut is left to its default 0 value, caching will never expire
    // - return true on success
    function SetTimeOut(aTable: TOrmClass; aTimeoutMS: cardinal): boolean;
    /// returns TRUE if the table is part of the current caching policy
    function IsCached(aTable: TOrmClass): boolean;
    /// returns the number of JSON serialization records within this cache
    function CachedEntries: cardinal;
    /// returns the memory used by JSON serialization records within this cache
    // - this method will also flush any outdated entries in the cache
    function CachedMemory(FlushedEntriesCount: PInteger = nil): cardinal;
    /// read-only access to the associated TRest.ORM instance
    property Rest: IRestOrm
      read fRest;
    /// read-only access to the associated TOrmModel instance
    property Model: TOrmModel
      read fModel;
  public { TRest low level methods which are not to be called usualy: }
    /// retrieve a record specified by its ID from cache into JSON content
    // - return '' if the item is not in cache
    function Retrieve(aTableIndex: integer; aID: TID): RawUtf8; overload;
    /// fill a record specified by its ID from cache into a new TOrm instance
    // - return false if the item is not in cache
    // - this method will call RetrieveJson method, unserializing the cached
    // JSON content into the supplied aValue instance
    function Retrieve(aID: TID; aValue: TOrm): boolean; overload;
    /// TRest instance shall call this method when a record is added or updated
    // - this overloaded method expects the content to be specified as JSON object
    procedure Notify(aTable: TOrmClass; aID: TID; const aJson: RawUtf8;
      aAction: TOrmOccasion); overload;
    /// TRest instance shall call this method when a record is retrieved,
    // added or updated
    // - this overloaded method expects the content to be specified as JSON object,
    // and TOrmClass to be specified as its index in Rest.Model.Tables[]
    procedure Notify(aTableIndex: integer; aID: TID; const aJson: RawUtf8;
      aAction: TOrmOccasion); overload;
    /// TRest instance shall call this method when a record is added or updated
    // - this overloaded method will call the other Trace method, serializing
    // the supplied aRecord content as JSON (not in the case of oeDelete)
    procedure Notify(aRecord: TOrm; aAction: TOrmOccasion); overload;
    /// TRest instance shall call this method when a record is deleted
    // - this method is dedicated for a record deletion
    procedure NotifyDeletion(aTable: TOrmClass; aID: TID); overload;
    /// TRest instance shall call this method when a record is deleted
    // - this method is dedicated for a record deletion
    // - TOrmClass to be specified as its index in Rest.Model.Tables[]
    procedure NotifyDeletion(aTableIndex: integer; aID: TID); overload;
    /// TRest instance shall call this method when records are deleted
    // - TOrmClass to be specified as its index in Rest.Model.Tables[]
    procedure NotifyDeletions(aTableIndex: integer; const aIDs: array of Int64); overload;
  end;


  { -------------------- TRestBatch TRestBatchLocked Definitions }

  /// used to store a BATCH sequence of writing operations
  // - is used by TRest to process BATCH requests using BatchSend() method,
  // or TRestClientUri for its Batch*() methods
  // - but you can create your own stand-alone BATCH process, so that it will
  // be able to make some transactional process - aka the "Unit Of Work" pattern
  TRestBatch = class
  protected
    fRest: IRestOrm;
    fModel: TOrmModel;
    fInternalBufferSize: integer;
    fCalledWithinRest: boolean;
    fPreviousTableMatch: boolean;
    fBatch: TOrmWriter;
    fTable: TOrmClass;
    fTableIndex: integer;
    fBatchCount: integer;
    fOptions: TRestBatchOptions;
    fDeletedRecordRef: TIDDynArray;
    fDeletedCount: integer;
    fAddCount: integer;
    fUpdateCount: integer;
    fDeleteCount: integer;
    fAutomaticTransactionPerRow: cardinal;
    fOnWrite: TOnBatchWrite;
    fBatchFields: TFieldBits;
    fPreviousTable: TOrmClass;
    fPreviousEncoding: TRestBatchEncoding;
    fPreviousFields: TFieldBits;
    function GetCount: integer; {$ifdef HASINLINE} inline; {$endif}
    function GetSizeBytes: cardinal;
    procedure SetExpandedJsonWriter(Props: TOrmProperties;
      ForceResetFields, withID: boolean; const WrittenFields: TFieldBits);
    procedure Encode(EncodedTable: TOrmClass; Encoding: TRestBatchEncoding;
      Fields: PFieldBits = nil; ID: TID = 0);
  public
    /// begin a BATCH sequence to speed up huge database changes
    // - each call to normal Add/Update/Delete methods will create a Server request,
    // therefore can be slow (e.g. if the remote server has bad ping timing)
    // - start a BATCH sequence using this method, then call BatchAdd() BatchUpdate()
    // or BatchDelete() methods to make some changes to the database
    // - when BatchSend will be called, all the sequence transactions will be sent
    // as one to the remote server, i.e. in one URI request
    // - if BatchAbort is called instead, all pending BatchAdd/Update/Delete
    // transactions will be aborted, i.e. ignored
    // - expect one TOrmClass as parameter, which will be used for the whole
    // sequence (in this case, you can't mix classes in the same BATCH sequence)
    // - if no TOrmClass is supplied, the BATCH sequence will allow any
    // kind of individual record in BatchAdd/BatchUpdate/BatchDelete
    // - return TRUE on success, FALSE if aTable is incorrect or a previous BATCH
    // sequence was already initiated
    // - you may set AutomaticTransactionPerRow=0 inside a Transaction block: no
    // automated TransactionBegin..Commit/RollBack will be generated
    // - BatchOptions could be set to tune the SQL execution, e.g. force INSERT
    // OR IGNORE on internal SQLite3 engine
    // - InternalBufferSize could be set to some high value (e.g. 10 shl 20),
    // if you expect a very high number of rows in this BATCH
    constructor Create(const aRest: IRestOrm; aTable: TOrmClass;
      AutomaticTransactionPerRow: cardinal = 10000;
      Options: TRestBatchOptions = [boExtendedJson];
      InternalBufferSize: cardinal = 65536; CalledWithinRest: boolean = false);
    /// begin a BATCH sequence to speed up huge database changes with no IRestOrm
    // - could be done e.g. on client side with no remote REST ORM access
    constructor CreateNoRest(aModel: TOrmModel; aTable: TOrmClass;
      AutomaticTransactionPerRow: cardinal = 10000;
      Options: TRestBatchOptions = [boExtendedJson];
      InternalBufferSize: cardinal = 65536); virtual;
    /// finalize the BATCH instance
    destructor Destroy; override;
    /// reset the BATCH sequence so that you can re-use the same TRestBatch
    procedure Reset(aTable: TOrmClass; AutomaticTransactionPerRow: cardinal = 0;
      Options: TRestBatchOptions = [boExtendedJson]); overload; virtual;
    /// reset the BATCH sequence to its previous state
    // - could be used to prepare a next chunk of values, after a call to
    // TRest.BatchSend
    procedure Reset; overload;
    /// create a new member in current BATCH sequence
    // - work in BATCH mode: nothing is sent to the server until BatchSend call
    // - returns the corresponding index in the current BATCH sequence, -1 on error
    // - if SendData is true, content of Value is sent to the server as JSON
    // - if ForceID is true, client sends the Value.ID field to use this ID for
    // adding the record (instead of a database-generated ID)
    // - if Value is TOrmFts3/4/5, Value.ID is stored to the virtual table
    // - Value class MUST match the TOrmClass used at BatchStart,
    // or may be of any kind if no class was specified
    // - BLOB fields are NEVER transmitted here, even if ForceBlobTransfert=TRUE
    // - if CustomFields is left void, the simple fields will be used; otherwise,
    // you can specify your own set of fields to be transmitted when SendData=TRUE
    // (including BLOBs, even if they will be Base64-encoded within JSON content) -
    // CustomFields could be computed by TOrmProperties.FieldBitsFromCsv()
    // or TOrmProperties.FieldBitsFrom(), or by setting ALL_FIELDS
    // - this method will always compute and send TCreateTime/TModTime fields
    function Add(Value: TOrm; SendData: boolean; ForceID: boolean = false;
      const CustomFields: TFieldBits = []; DoNotAutoComputeFields: boolean = false): integer;
    /// update a member in current BATCH sequence
    // - work in BATCH mode: nothing is sent to the server until BatchSend call
    // - returns the corresponding index in the current BATCH sequence, -1 on error
    // - Value class MUST match the TOrmClass used at BatchStart,
    // or may be of any kind if no class was specified
    // - BLOB fields are NEVER transmitted here, even if ForceBlobTransfert=TRUE
    // - if Value has an opened FillPrepare() mapping, only the mapped fields
    // will be updated (and also ID and TModTime fields) - FillPrepareMany() is
    // not handled yet (all simple fields will be updated)
    // - if CustomFields is left void, the  simple fields will be used, or the
    // fields retrieved via a previous FillPrepare() call; otherwise, you can
    // specify your own set of fields to be transmitted (including BLOBs, even
    // if they will be Base64-encoded within the JSON content) - CustomFields
    // could be computed by TOrmProperties.FieldBitsFromCsv()
    // or TOrmProperties.FieldBitsFrom()
    // - this method will always compute and send any TModTime fields, unless
    // DoNotAutoComputeFields is set to true
    // - if not all fields are specified, will reset the cache entry associated
    // with this value, unless ForceCacheUpdate is TRUE
    function Update(Value: TOrm; const CustomFields: TFieldBits = [];
      DoNotAutoComputeFields: boolean = false; ForceCacheUpdate: boolean = false): integer; overload; virtual;
    /// update a member in current BATCH sequence
    // - work in BATCH mode: nothing is sent to the server until BatchSend call
    // - is an overloaded method to Update(Value,FieldBitsFromCsv())
    function Update(Value: TOrm; const CustomCsvFields: RawUtf8;
      DoNotAutoComputeFields: boolean = false; ForceCacheUpdate: boolean = false): integer; overload;
    /// delete a member in current BATCH sequence
    // - work in BATCH mode: nothing is sent to the server until BatchSend call
    // - returns the corresponding index in the current BATCH sequence, -1 on error
    // - deleted record class is the TOrmClass used at BatchStart()
    // call: it will fail if no class was specified for this BATCH sequence
    function Delete(ID: TID): integer; overload;
    /// delete a member in current BATCH sequence
    // - work in BATCH mode: nothing is sent to the server until BatchSend call
    // - returns the corresponding index in the current BATCH sequence, -1 on error
    // - with this overloaded method, the deleted record class is specified:
    // no TOrmClass shall have been set at BatchStart() call
    function Delete(Table: TOrmClass; ID: TID): integer; overload;
    /// allow to append some JSON content to the internal raw buffer
    // - could be used to emulate Add/Update/Delete
    // - FullRow=TRUE will increment the global Count
    function RawAppend(FullRow: boolean = true): TJsonWriter;
    /// allow to append some JSON content to the internal raw buffer for a POST
    // - could be used to emulate Add() with an already pre-computed JSON object
    // - returns the corresponding index in the current BATCH sequence, -1 on error
    function RawAdd(const SentData: RawUtf8): integer;
    /// allow to append some JSON content to the internal raw buffer for a PUT
    // - could be used to emulate Update() with an already pre-computed JSON object
    // - returns the corresponding index in the current BATCH sequence, -1 on error
    function RawUpdate(const SentData: RawUtf8; ID: TID): integer;
    /// close a BATCH sequence started by Start method
    // - Data is the JSON content, ready to be supplied to TRest.BatchSend()
    // overloaded method - its layout is '{"Table":["cmd":values,...]}'
    // - will also notify the TRest.Cache for all deleted IDs
    // - you should not have to call it in normal use cases
    function PrepareForSending(out Data: RawUtf8): boolean; virtual;
    /// read only access to the associated TRest instance
    property Rest: IRestOrm
      read fRest;
    /// read only access to the associated TOrmModel instance
    property Model: TOrmModel
      read fModel;
    /// the processing options as supplied to the constructor
    property Options: TRestBatchOptions
      read fOptions;
    /// how many times Add() has been called for this BATCH process
    property AddCount: integer
      read fAddCount;
    /// how many times Update() has been called for this BATCH process
    property UpdateCount: integer
      read fUpdateCount;
    /// how many times Delete() has been called for this BATCH process
    property DeleteCount: integer
      read fDeleteCount;
    /// this event handler will be triggerred by each Add/Update/Delete method
    property OnWrite: TOnBatchWrite
      read fOnWrite write fOnWrite;
  published
    /// read only access to the main associated TOrm class (if any)
    property Table: TOrmClass
      read fTable;
    /// retrieve the current number of pending transactions in the BATCH sequence
    property Count: integer
      read GetCount;
    /// retrieve the current JSON size of pending transaction in the BATCH sequence
    property SizeBytes: cardinal
      read GetSizeBytes;
  end;

  /// thread-safe class to store a BATCH sequence of writing operations
  TRestBatchLocked = class(TRestBatch)
  protected
    fResetTix: Int64;
    fThreshold: integer;
    fSafe: TSynLocker;
  public
    /// initialize the BATCH instance
    constructor CreateNoRest(aModel: TOrmModel; aTable: TOrmClass;
      AutomaticTransactionPerRow: cardinal = 10000;
      Options: TRestBatchOptions = [boExtendedJson];
      InternalBufferSize: cardinal = 65536); override;
    /// finalize the BATCH instance
    destructor Destroy; override;
    /// reset the BATCH sequence so that you can re-use the same TRestBatch
    procedure Reset(aTable: TOrmClass; AutomaticTransactionPerRow: cardinal = 0;
      Options: TRestBatchOptions = [boExtendedJson]); override;
    /// access to the locking methods of this instance
    // - use Safe.Lock/TryLock with a try ... finally Safe.Unlock block
    property Safe: TSynLocker
      read fSafe;
    /// property set to the current GetTickCount64 value when Reset was called
    property ResetTix: Int64
      read fResetTix write fResetTix;
    /// may be used to store a number of rows to flush the content
    property Threshold: integer
      read fThreshold write fThreshold;
  end;

  TRestBatchLockedDynArray = array of TRestBatchLocked;


/// compute the SQL field names, used to create a SQLite3 virtual table
function GetVirtualTableSqlCreate(Props: TOrmProperties): RawUtf8;

/// encode as a SQL-ready (multi) INSERT statement with ? as values
// - follow the SQLite3 syntax: INSERT INTO .. VALUES (..),(..),(..),..
// - same logic as TJsonObjectDecoder.EncodeAsSqlPrepared
procedure EncodeMultiInsertSQLite3(Props: TOrmProperties;
  const FieldNames: TRawUtf8DynArray; FieldBits: PFieldBits;
  BatchOptions: TRestBatchOptions; FieldCount, RowCount: integer;
  var result: RawUtf8);

/// TDynArraySortCompare compatible function, sorting by TOrm.ID
function TOrmDynArrayCompare(const Item1, Item2): integer;

/// TDynArrayHashOne compatible function, hashing TOrm.ID
function TOrmDynArrayHashOne(const Elem; Hasher: THasher): cardinal;

/// create a TRecordReference with the corresponding parameters
function RecordReference(Model: TOrmModel; aTable: TOrmClass;
  aID: TID): TRecordReference; overload;

/// create a TRecordReference with the corresponding parameters
function RecordReference(aTableIndex: cardinal; aID: TID): TRecordReference; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a dynamic array of TRecordReference into its corresponding IDs
procedure RecordRefToID(var aArray: TInt64DynArray);


{ ************ TSynValidateRest TSynValidateUniqueField Definitions }

type
  /// will define a validation to be applied to a TOrm field, using
  // if necessary an associated TRest instance and a TOrm class
  // - a typical usage is to validate a value to be unique in the table
  // (implemented in the TSynValidateUniqueField class)
  // - the optional associated parameters are to be supplied JSON-encoded
  // - ProcessRest and ProcessRec properties will be filled before Validate
  // method call by TOrm.Validate()
  TSynValidateRest = class(TSynValidate)
  protected
    fProcessRest: IRestOrm;
    fProcessRec: TOrm;
    function DoValidate(aFieldIndex: integer; const Value: RawUtf8;
      var ErrorMsg: string; const aProcessRest: IRestOrm; aProcessRec: TOrm): boolean;
      virtual; abstract;
  public
    function Process(aFieldIndex: integer; const Value: RawUtf8;
      var ErrorMsg: string): boolean; override;
    function Validate(aFieldIndex: integer; const Value: RawUtf8;
      var ErrorMsg: string; const aProcessRest: IRestOrm;
      aProcessRec: TOrm): boolean;
    /// access to the ORM process of the associated TRest instance
    // - this value is updated by Validate with the current
    // TRest used for the validation
    // - it can be used in the overridden DoValidate method
    property ProcessRest: IRestOrm
      read fProcessRest;
    /// the associated TOrm instance
    // - this value is updated by Validate with the current
    // TOrm instance to be validated
    // - it can be used in the overridden DoValidate method
    property ProcessRec: TOrm
      read fProcessRec;
  end;

  /// will define a validation for a TOrm Unique text field
  // - this class will handle only textual fields, not numeric values
  // - it will check that the field value is not void
  // - it will check that the field value is not a duplicate
  TSynValidateUniqueField = class(TSynValidateRest)
  protected
    /// perform the unique field validation action to the specified value
    function DoValidate(aFieldIndex: integer; const Value: RawUtf8;
      var ErrorMsg: string; const aProcessRest: IRestOrm;
      aProcessRec: TOrm): boolean; override;
  end;

  /// will define an unicity validation for a set of TOrm text fields
  // - field names should be specified as CSV in the JSON "FieldNames" property
  // in the constructor, or the Parameters field, e.g. like
  // ! TOrmSampleRecord.AddFilterOrValidate('propA',
  // !   TSynValidateUniqueFields.Create('{"FieldNames":"propA,propB"}'));
  // - this class will handle only textual fields, not numeric values
  // - it will check that the field values are not a duplicate
  TSynValidateUniqueFields = class(TSynValidateRest)
  protected
    fFieldNames: TRawUtf8DynArray;
    procedure SetParameters(const Value: RawUtf8); override;
    /// perform the unique fields validation action to the specified value
    function DoValidate(aFieldIndex: integer; const Value: RawUtf8;
      var ErrorMsg: string; const aProcessRest: IRestOrm;
      aProcessRec: TOrm): boolean; override;
  public
    /// the validated field names
    property FieldNames: TRawUtf8DynArray
      read fFieldNames;
  end;



{ ************ TOrmAccessRights Definition }

type
  /// a set of potential actions to be executed from the server
  // - reSQL will indicate the right to execute any POST SQL statement (not only
  // SELECT statements)
  // - reService will indicate the right to execute the interface-based JSON-RPC
  // service implementation
  // - reUrlEncodedSQL will indicate the right to execute a SQL query encoded
  // at the URI level, for a GET (to be used e.g. with XMLHTTPRequest, which
  // forced SentData='' by definition), encoded as sql=.... inline parameter
  // - reUrlEncodedDelete will indicate the right to delete items using a
  // WHERE clause for DELETE verb at /root/TableName?WhereClause
  // - reOneSessionPerUser will force that only one session may be created
  // for one user, even if connection comes from the same IP: in this case,
  // you may have to set the SessionTimeOut to a small value, in case the
  // session is not closed gracefully
  // - reSqlSelectWithoutTable will allow executing a SELECT statement with
  // arbitrary content via GET/LOCK (simple SELECT .. FROM aTable will be
  // checked against TOrmAccessRights.GET[] per-table right)
  // - by default, read/write access to the TAuthUser table is disallowed,
  // for obvious security reasons: but you can define reUserCanChangeOwnPassword
  // so that the current logged user will be able to change its own password
  // - order of this set does matter, since it will be stored as a byte value
  // e.g. by TOrmAccessRights.ToString: ensure that new items will always be
  // appended to the list, not inserted within
  TOrmAllowRemoteExecute = set of (
    reSQL,
    reService,
    reUrlEncodedSQL,
    reUrlEncodedDelete,
    reOneSessionPerUser,
    reSqlSelectWithoutTable,
    reUserCanChangeOwnPassword);

  /// set the User Access Rights, for each Table
  // - one property for every and each URI method (GET/POST/PUT/DELETE)
  // - one bit for every and each Table in Model.Tables[]
  TOrmAccessRights = object
  public
    /// set of allowed actions on the server side
    AllowRemoteExecute: TOrmAllowRemoteExecute;
    /// GET method (retrieve record) table access bits
    // - note that a GET request with a SQL statement without a table (i.e.
    // on 'ModelRoot' URI with a SQL statement as SentData, as used in
    // TRestClientUri.UpdateFromServer) will be checked for simple cases
    // (i.e. the first table in the FROM clause), otherwise will follow , whatever the bits
    // here are: since TRestClientUri.UpdateFromServer() is called only
    // for refreshing a direct statement, it will be OK; you can improve this
    // by overriding the TRestServer.Uri() method
    // - if the REST request is LOCK, the PUT access bits will be read instead
    // of the GET bits value
    GET: TOrmTableBits;
    /// POST method (create record) table access bits
    POST: TOrmTableBits;
    /// PUT method (update record) table access bits
    // - if the REST request is LOCK, the PUT access bits will be read instead
    // of the GET bits value
    PUT: TOrmTableBits;
    /// DELETE method (delete record) table access bits
    DELETE: TOrmTableBits;
    /// wrapper method which can be used to set the CRUD abilities over a table
    // - C=Create, R=Read, U=Update, D=Delete rights
    procedure Edit(aTableIndex: integer;
      C, R, U, D: boolean); overload;
    /// wrapper method which can be used to set the CRUD abilities over a table
    // - use TOrmOccasion set as parameter
    procedure Edit(aTableIndex: integer;
      aRights: TOrmOccasions); overload;
    /// wrapper method which can be used to set the CRUD abilities over a table
    // - will raise an EModelException if the supplied table is incorrect
    // - C=Create, R=Read, U=Update, D=Delete rights
    procedure Edit(aModel: TOrmModel; aTable: TOrmClass;
      C, R, U, D: boolean); overload;
    /// wrapper method which can be used to set the CRUD abilities over a table
    // - will raise an EModelException if the supplied table is incorrect
    // - use TOrmOccasion set as parameter
    procedure Edit(aModel: TOrmModel; aTable: TOrmClass;
      aRights: TOrmOccasions); overload;
    /// serialize the content as TEXT
    // - use the TAuthGroup.AccessRights CSV format
    function ToString: RawUtf8;
    /// unserialize the content from TEXT
    // - use the TAuthGroup.AccessRights CSV format
    procedure FromString(P: PUtf8Char);
  end;

  POrmAccessRights = ^TOrmAccessRights;


const
  /// Supervisor Table access right, i.e. alllmighty over all fields
  ALL_ACCESS_RIGHTS = [0..MAX_TABLES - 1];

  /// Complete Database access right, i.e. allmighty over all Tables
  // - WITH the possibility to remotely execute any SQL statement (reSQL right)
  // - is used by default by TRestClientDB.Uri() method, i.e. for direct
  // local/in-process access
  // - is used as reference to create TAuthUser 'Admin' access policy
  FULL_ACCESS_RIGHTS: TOrmAccessRights = (
    AllowRemoteExecute:
     [reSQL, reSqlSelectWithoutTable, reService, reUrlEncodedSQL, reUrlEncodedDelete];
    GET: ALL_ACCESS_RIGHTS;
    POST: ALL_ACCESS_RIGHTS;
    PUT: ALL_ACCESS_RIGHTS;
    DELETE: ALL_ACCESS_RIGHTS
  );

  /// Supervisor Database access right, i.e. allmighty over all Tables
  // - but WITHOUT the possibility to remotely execute any SQL statement (reSQL)
  // - is used as reference to create TAuthUser 'Supervisor' access policy
  SUPERVISOR_ACCESS_RIGHTS: TOrmAccessRights = (
    AllowRemoteExecute:
      [reSqlSelectWithoutTable, reService, reUrlEncodedSQL, reUrlEncodedDelete];
    GET: ALL_ACCESS_RIGHTS;
    POST: ALL_ACCESS_RIGHTS;
    PUT: ALL_ACCESS_RIGHTS;
    DELETE: ALL_ACCESS_RIGHTS
  );


{ ************** TOrm High-Level Parents }

type
  /// root class for defining and mapping database records with case-insensitive
  // NOCASE collation
  // - abstract ancestor, from which you may inherit your own ORM classes
  // - by default, any oftUtf8Text field (RawUtf8, UnicodeString, WideString
  // properties) will use our Unicode SYSTEMNOCASE SQLite3 collation, which calls
  // Utf8ILComp() to handle most western languages, but is not standard
  // - you may inherit from this class to ensure any text field will use the
  // faster and SQLite3 built-in NOCASE collation, handling only 7-bit A-Z chars
  // - inherit from TOrmNoCase or TOrmCaseSensitive if you expect
  // your text fields to contain only basic (un)accentued ASCCI characters, and
  // to be opened by any standard/ SQlite3 library or tool (outside of
  // mormot.db.raw.sqlite3.pas/SynDBExplorer)
  TOrmNoCase = class(TOrm)
  protected
    /// will call Props.SetCustomCollationForAll(oftUtf8Text,'NOCASE')
    class procedure InternalDefineModel(Props: TOrmProperties); override;
  end;

  /// root class for defining and mapping database records with case-sensitive
  // BINARY collation
  // - abstract ancestor, from which you may inherit your own ORM classes
  // - by default, any oftUtf8Text field (RawUtf8, UnicodeString, WideString
  // properties) will use our Unicode SYSTEMNOCASE SQLite3 collation, which calls
  // Utf8ILComp() to handle most western languages, but is not standard
  // - you may inherit from this class to ensure any text field will use the
  // faster and SQLite3 built-in BINARY collation, which is case-sensitive
  // - inherit from TOrmNoCase or TOrmCaseSensitive if you expect
  // your text fields to contain only basic (un)accentued ASCCI characters, and
  // to be opened by any standard/ SQlite3 library or tool (outside of
  // mormot.db.raw.sqlite3.pas/SynDBExplorer)
  TOrmCaseSensitive = class(TOrm)
  protected
    /// will call Props.SetCustomCollationForAll(oftUtf8Text,'BINARY')
    class procedure InternalDefineModel(Props: TOrmProperties); override;
  end;

  /// root class for defining and mapping database records with case-insensitive
  // UNICODENOCASE collation
  // - abstract ancestor, from which you may inherit your own ORM classes
  // - by default, any oftUtf8Text field (RawUtf8, UnicodeString, WideString
  // properties) will use our Unicode SYSTEMNOCASE SQLite3 collation, which calls
  // Utf8ILComp() to handle most western languages
  // - you may inherit from this class to ensure any text field will use our
  // Utf8ILCompReference() function which handles Unicode 10.0
  // - inherit from TOrmNoCase or TOrmCaseSensitive if you expect
  // your text fields to contain only basic (un)accentued ASCCI characters, and
  // to be opened by any standard/ SQlite3 library or tool (outside of
  // mormot.db.raw.sqlite3.pas/SynDBExplorer)
  TOrmUnicodeNoCase = class(TOrm)
  protected
    /// will call Props.SetCustomCollationForAll(oftUtf8Text,'UNICODENOCASE')
    class procedure InternalDefineModel(Props: TOrmProperties); override;
  end;

  /// database records with NOCASE collation and JSON_FAST_EXTENDED variants
  // - abstract ancestor, from which you may inherit your own ORM classes
  TOrmNoCaseExtended = class(TOrmNoCase)
  protected
    /// will call Props.SetVariantFieldsDocVariantOptions(JSON_FAST_EXTENDED);
    class procedure InternalDefineModel(Props: TOrmProperties); override;
  end;

  /// database records with BINARY collation and JSON_FAST_EXTENDED variants
  // - abstract ancestor, from which you may inherit your own ORM classes
  TOrmCaseSensitiveExtended = class(TOrmCaseSensitive)
  protected
    /// will call Props.SetVariantFieldsDocVariantOptions(JSON_FAST_EXTENDED);
    class procedure InternalDefineModel(Props: TOrmProperties); override;
  end;

  /// database records with NOCASE collation and JSON_FAST_EXTENDED
  // variants, and itoNoIndex4TID option to avoid indexes on TID/T*ID properties
  // - abstract ancestor, from which you may inherit your own ORM classes
  TOrmNoCaseExtendedNoID = class(TOrmNoCaseExtended)
  public
    /// overriden method forcing no index creation on TID/T*ID properties
    class procedure InitializeTable(const Server: IRestOrmServer;
     const FieldName: RawUtf8; Options: TOrmInitializeTableOptions); override;
  end;


// backward compatibility types redirections
{$ifndef PUREMORMOT2}

type
  TSqlRecord = TOrm;
  PSqlRecord = POrm;
  TSqlRecordArray = TOrmArray;
  PSqlRecordArray = POrmArray;
  TSqlRecordObjArray = TOrmObjArray;
  TSqlRecordClass = TOrmClass;
  TSqlRecordClassDynArray = TOrmClassDynArray;
  PSqlClass = POrmClass;
  TSqlTable = TOrmTable;
  TSqlTableJson = TOrmTableJson;
  TSqlInitializeTableOption = TOrmInitializeTableOption;
  TSqlInitializeTableOptions = TOrmInitializeTableOptions;
  TSqlAccessRights = TOrmAccessRights;
  PSqlAccessRights = POrmAccessRights;
  TSqlFieldType = TOrmFieldType;
  TSqlFieldTables = TOrmTableBits;
  TSqlModel = TOrmModel;
  TSqlModelProperties = TOrmModelProperties;
  TSqlModelPropertiesObjArray = TOrmModelPropertiesObjArray;
  TSqlProperties = TOrmProperties;
  TSqlPropInfo = TOrmPropInfo;
  TSqlPropInfoObjArray = TOrmPropInfoObjArray;
  TSqlPropInfoClass = TOrmPropInfoClass;
  TSqlPropInfoListOptions = TOrmPropInfoListOptions;
  TSqlPropInfoAttribute = TOrmPropInfoAttribute;
  TSqlPropInfoAttributes = TOrmPropInfoAttributes;
  TSqlRestCache = TRestCache;
  TSqlRestBatch = TRestBatch;
  TSqlRestBatchLocked = TRestBatchLocked;

{$endif PUREMORMOT2}


implementation


{ -------------------- ORM Specific TOrmPropInfoRtti Classes }

{ TOrmPropInfoRttiTID }

constructor TOrmPropInfoRttiTID.Create(aPropInfo: PRttiProp; aPropIndex: integer;
  aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions);
var
  TypeName: PShortString;
  Found: TRttiCustom;
begin
  inherited Create(aPropInfo, aPropIndex, aOrmFieldType, aOptions);
  TypeName := fPropType^.Name;
  if IdemPropName(TypeName^, 'TID') or
     (ord(TypeName^[1]) and $df <> ord('T')) or // expect T...ID pattern
     (PWord(@TypeName^[ord(TypeName^[0]) - 1])^ and $dfdf <> ord('I') + ord('D') shl 8) or
     (Rtti.Counts[rkClass] = 0) then
    exit;
  if (ord(TypeName^[0]) > 13) and
     IdemPropName('ToBeDeletedID', @TypeName^[ord(TypeName^[0]) - 12], 13) then
  begin
    // 'TOrmClientToBeDeletedID' -> TOrmClient + CascadeDelete=true
    fCascadeDelete := true;
    Found := Rtti.Find(@TypeName^[1], ord(TypeName^[0]) - 13, rkClass);
  end
  else    // 'TOrmClientID' -> TOrmClient
    Found := Rtti.Find(@TypeName^[1], ord(TypeName^[0]) - 2, rkClass);
  if (Found <> nil) and Found.ValueClass.InheritsFrom(TOrm) then
    fRecordClass := pointer(Found.ValueClass);
end;


{ TOrmPropInfoRttiID }

procedure TOrmPropInfoRttiID.SetValue(Instance: TObject; Value: PUtf8Char;
  ValueLen: PtrInt; wasString: boolean);
begin
  if TOrm(Instance).fFill.JoinedFields then
    raise EModelException.CreateUtf8('%(%).SetValue after Create*Joined', [self, Name]);
  inherited SetValue(Instance, Value, ValueLen, wasString);
end;

procedure TOrmPropInfoRttiID.GetJsonValues(Instance: TObject; W: TJsonWriter);
var
  ID: PtrUInt;
begin
  ID := PtrUInt(GetInstance(Instance));
  if TOrm(Instance).fFill.JoinedFields then
    ID := TOrm(ID).fID;
  W.AddU(ID);
end;



{ ************ TOrmModel TOrmTable IRestOrm Core Definitions }

{$ifdef CPUX64}

// very efficient branchless asm - rcx/rdi=Item1 rdx/rsi=Item2
function TOrmDynArrayCompare(const Item1, Item2): integer;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        mov     rcx, qword ptr [Item1]
        mov     rdx, qword ptr [Item2]
        mov     rcx, qword ptr [rcx + TOrm.fID]
        mov     rdx, qword ptr [rdx + TOrm.fID]
        xor     eax, eax
        cmp     rcx, rdx
        seta    al
        sbb     eax, 0
end;

{$else}

function TOrmDynArrayCompare(const Item1,Item2): integer;
begin
  // we assume Item1<>nil and Item2<>nil
  result := CompareQWord(TOrm(Item1).fID, TOrm(Item2).fID);
  // inlined branchless comparison or correct x86 asm for older Delphi
end;

{$endif CPUX64}

function TOrmDynArrayHashOne(const Elem; Hasher: THasher): cardinal;
begin
  result := Hasher(0, pointer(@TOrm(Elem).fID), SizeOf(TID));
end;

function GetVirtualTableSqlCreate(Props: TOrmProperties): RawUtf8;
var
  i: PtrInt;
  SQL: RawUtf8;
begin
  result := ''; // RowID is added by sqlite3_declare_vtab() for a Virtual Table
  for i := 0 to Props.Fields.Count - 1 do
    with Props.Fields.List[i] do
    begin
      SQL := Props.OrmFieldTypeToSql(i);
      if SQL <> '' then
        // = '' for field with no matching DB column
        result := result + Name + SQL;
    end;
  if result = '' then
    result := ');'
  else
    PWord(@result[length(result) - 1])^ := ord(')') + ord(';') shl 8;
end;

procedure EncodeMultiInsertSQLite3(Props: TOrmProperties;
  const FieldNames: TRawUtf8DynArray; FieldBits: PFieldBits;
  BatchOptions: TRestBatchOptions; FieldCount, RowCount: integer;
  var result: RawUtf8);
var
  f: PtrInt;
  W: TJsonWriter;
  temp: TTextWriterStackBuffer;
begin
  W := TJsonWriter.CreateOwnedStream(temp);
  try
    if boInsertOrIgnore in BatchOptions then
      W.AddShort('insert or ignore into ')
    else if boInsertOrReplace in BatchOptions then
      W.AddShort('insert or replace into ')
    else
      W.AddShort('insert into ');
    W.AddString(Props.SqlTableName);
    if FieldCount = 0 then
      W.AddShort(' default values')
    else
    begin
      W.Add(' ', '(');
      if FieldBits <> nil then
      begin
        W.AddShorter('RowID,'); // first is always the ID
        for f := 0 to Props.Fields.Count - 1 do
          if GetBitPtr(FieldBits, f) then
          begin
            W.AddString(Props.Fields.List[f].Name);
            W.AddComma;
          end;
      end
      else
        for f := 0 to FieldCount - 1 do
        begin
          // append 'COL1,COL2'
          W.AddString(FieldNames[f]);
          W.AddComma;
        end;
      W.CancelLastComma;
      W.AddShort(') values (');
      W.AddStrings('?,', FieldCount);
      while RowCount > 1 do
      begin
        // INSERT INTO .. VALUES (..),(..),(..),..
        W.CancelLastComma;
        W.AddShorter('),(');
        W.AddStrings('?,', FieldCount);
        dec(RowCount);
      end;
      W.CancelLastComma;
      W.Add(')');
    end;
    W.SetText(result);
  finally
    W.Free;
  end;
end;



{ ------------ RecordRef Wrapper Definition }

function RecordReference(Model: TOrmModel; aTable: TOrmClass;
  aID: TID): TRecordReference;
begin
  if aID = 0 then
    result := 0
  else
  begin
    result := Model.GetTableIndexExisting(aTable);
    if result > 63 then // TRecordReference handle up to 64=1 shl 6 tables
      result := 0
    else
      inc(result, aID shl 6); // 64=1 shl 6
  end;
end;

function RecordReference(aTableIndex: cardinal; aID: TID): TRecordReference;
begin
  if (aID = 0) or
     (aTableIndex > 63) then
    result := 0
  else
    result := aTableIndex + aID shl 6;
end;

procedure RecordRefToID(var aArray: TInt64DynArray);
var
  i: PtrInt;
begin
  for i := 0 to high(aArray) do
    aArray[i] := aArray[i] shr 6;
end;


{ RecordRef }

procedure RecordRef.From(Model: TOrmModel; aTable: TOrmClass; aID: TID);
begin
  Value := Model.GetTableIndexExisting(aTable);
  if Value > 63 then // TRecordReference handle up to 64=1 shl 6 tables
    Value := 0
  else
    inc(Value, aID shl 6); // 64=1 shl 6
end;

function RecordRef.ID: TID;
begin
  result := Value shr 6;  // 64=1 shl 6
end;

function RecordRef.Table(Model: TOrmModel): TOrmClass;
var
  V: integer;
begin
  if (Model = nil) or
     (Value = 0) then
    result := nil
  else
  begin
    V := Value and 63;
    if V > Model.TablesMax then
      result := nil
    else
      result := Model.Tables[V];
  end;
end;

function RecordRef.TableIndex: integer;
begin
  result := Value and 63;
end;

function RecordRef.Text(Model: TOrmModel): RawUtf8;
var
  aTable: TOrmClass;
begin
  if ((Value shr 6) = 0) then
    // Value=0 or no valid ID
    result := ''
  else
  begin
    aTable := Table(Model);
    if aTable = nil then
      result := ''
    else
      result := Model.TableProps[Value and 63].Props.SqlTableName + ' ' +
        Int64ToUtf8(Value shr 6);
  end;
end;

function RecordRef.Text(const Rest: IRestOrm): RawUtf8;
var
  T: TOrmClass;
  aID: TID;
  m: TOrmModel;
begin
  result := '';
  if ((Value shr 6) = 0) or
     (Rest = nil) then
    exit;
  m := Rest.Model;
  T := Table(m);
  if T = nil then
    exit;
  aID := ID;
  with m.TableProps[Value and 63].Props do
    if aID <= 0 then
      result := SqlTableName
    else
    begin
      result := Rest.MainFieldValue(T, aID, true);
      if result = '' then
        FormatUtf8('% %', [SqlTableName, aID], result)
      else
        result := FormatUtf8('% "%"', [SqlTableName, result]);
    end;
end;


{ ------------ TOrmTable TOrmTableJson Definitions }

{ TOrmTable }

function TOrmTable.FieldPropFromTables(const PropName: RawUtf8;
  out PropInfo: TOrmPropInfo; out TableIndex: integer): TOrmFieldType;

  procedure SearchInQueryTables(aPropName: PUtf8Char; aTableIndex: integer);
  begin
    if IsRowID(aPropName) then
    begin
      result := oftInteger;
      PropInfo := nil;
      TableIndex := aTableIndex;
      exit;
    end
    else if fQueryTables[aTableIndex] <> nil then
    begin
      PropInfo := fQueryTables[aTableIndex].OrmProps.Fields.ByName(aPropName);
      if PropInfo <> nil then
      begin
        result := PropInfo.OrmFieldTypeStored;
        if result <> oftUnknown then
          TableIndex := aTableIndex;
        exit;
      end;
      result := oftUnknown;
    end;
  end;

var
  i, t: PtrInt;
  P: PUtf8Char;
begin
  TableIndex := -1;
  result := oftUnknown;
  P := pointer(PropName);
  if fQueryTableIndexFromSql = -2 then
  begin
    fQueryTableIndexFromSql := -1; // search once to set fQueryTableIndexFromSql
    if (fQueryTables <> nil) and
       (QueryTableNameFromSql <> '') then
      for i := 0 to length(fQueryTables) - 1 do
        if IdemPropNameU(
             fQueryTables[i].OrmProps.SqlTableName, fQueryTableNameFromSql) then
        begin
          fQueryTableIndexFromSql := i;
          break;
        end;
  end;
  if fQueryTableIndexFromSql >= 0 then
  begin
    SearchInQueryTables(P, fQueryTableIndexFromSql);
    if result <> oftUnknown then
      exit;
  end;
  if length(fQueryTables) = 1 then
    SearchInQueryTables(P, 0)
  else
  begin
    i := PosExChar('.', PropName) - 1;
    if i < 0 then
      // no 'ClassName.PropertyName' format: find first exact property name
      for t := 0 to high(fQueryTables) do
      begin
        SearchInQueryTables(P, t);
        if result <> oftUnknown then
          exit;
      end
    else
      // handle property names as 'ClassName.PropertyName'
      for t := 0 to high(fQueryTables) do
        if fQueryTables[t] <> nil then // avoid GPF
          if IdemPropNameU(fQueryTables[t].OrmProps.SqlTableName, P, i) then
          begin
            SearchInQueryTables(P + i + 1, t);
            exit;
          end;
  end;
end;

function TOrmTable.InitOneFieldType(field: PtrInt; out size: integer;
  out info: PRttiInfo; out tableindex: integer): TOrmFieldType;
var
  prop: TOrmPropInfo;
begin
  if Assigned(fQueryColumnTypes) then
    result := fQueryColumnTypes[field]
  else if Assigned(fQueryTables) then
  begin // retrieve column info from field name
    result := FieldPropFromTables(Results[field], prop, tableindex);
    if prop <> nil then
    begin
      if prop.InheritsFrom(TOrmPropInfoRtti) then
        info := TOrmPropInfoRtti(prop).PropType;
      size := prop.FieldWidth;
    end;
  end
  else
    result := oftUnknown;
end;

constructor TOrmTable.CreateFromTables(const Tables: array of TOrmClass;
  const aSql: RawUtf8);
var
  n: PtrInt;
begin
  Create(aSql);
  n := length(Tables);
  if n > 0 then
  begin
    SetLength(fQueryTables, n);
    MoveFast(Tables[0], fQueryTables[0], n * SizeOf(TClass));
  end;
end;

function TOrmTable.QueryRecordType: TOrmClass;
begin
  if (self <> nil) and
     (pointer(fQueryTables) <> nil) then
    result := fQueryTables[0]
  else
    result := nil;
end;

function TOrmTable.NewRecord(RecordType: TOrmClass): TOrm;
begin
  result := nil;
  if self = nil then
    exit;
  if RecordType = nil then
  begin
    RecordType := QueryRecordType;
    if RecordType = nil then
      exit;
  end;
  result := RecordType.Create;
  if fOwnedRecords = nil then
    fOwnedRecords := TSynObjectList.Create({ownobj=}true);
  fOwnedRecords.Add(result);
end;

{$ifdef HASGENERICS}

procedure TOrmTable.ToNewIList(var result; item: TOrmClass);
var
  list: TSynListSpecialized<TOrm>;
  cloned, one: TOrm;
  r: integer;
  rec: POrm;
begin
  list := TSynListSpecialized<TOrm>.Create(
    [], ptClass, TypeInfo(TOrmObjArray), item.ClassInfo);
  // all IList<T> share the same VMT -> assign same TSynListSpecialized<TOrm>
  IList<TOrm>(result) := list;
  // IList<T> will own and free each T instance
  if (self = nil) or
     (fRowCount = 0) then
    exit;
  cloned := item.Create;
  try
    cloned.FillPrepare(self);
    list.SetCount(fRowCount); // allocate once
    rec := list.First;        // fast direct iteration
    for r := 1 to fRowCount do
    begin
      one := item.Create;
      rec^ := one;
      inc(rec);
      cloned.fFill.Fill(r, one);
      one.fInternalState := fInternalState;
    end;
  finally
    cloned.Free;
  end;
end;

function TOrmTable.ToIList<T>: IList<T>;
begin
  ToNewIList(result, TOrmClass(T)); // smaller executable with sub ToNewIList()
end;

{$endif HASGENERICS}

procedure TOrmTable.FillOrms(P: POrm; RecordType: TOrmClass);
var
  cloned: TOrm;
  r: integer;
begin
  cloned := RecordType.Create;
  try
    cloned.FillPrepare(self);
    for r := 1 to fRowCount do
    begin
      P^ := RecordType.Create;
      cloned.fFill.Fill(r, P^);
      P^.fInternalState := fInternalState;
      inc(P);
    end;
  finally
    cloned.Free;
  end;
end;

procedure TOrmTable.ToObjectList(DestList: TObjectList; RecordType: TOrmClass);
begin
  if DestList = nil then
    exit;
  DestList.Clear;
  if (self = nil) or
     (fRowCount = 0) then
    exit;
  if RecordType = nil then
  begin
    RecordType := QueryRecordType;
    if RecordType = nil then
      exit;
  end;
  DestList.Count := fRowCount; // faster than manual Add()
  FillOrms(pointer(DestList.List), RecordType);
end;

function TOrmTable.ToObjectList(RecordType: TOrmClass): TObjectList;
begin
  result := TObjectList.Create;
  ToObjectList(result, RecordType);
end;

function TOrmTable.ToObjArray(var ObjArray; RecordType: TOrmClass): boolean;
var
  arr: TOrmObjArray absolute ObjArray;
begin
  result := false;
  ObjArrayClear(arr);
  if self = nil then
    exit;
  if RecordType = nil then
  begin
    RecordType := QueryRecordType;
    if RecordType = nil then
      exit;
  end;
  result := true;
  if fRowCount = 0 then
    exit;
  SetLength(arr, fRowCount); // faster than manual ObjArrayAdd()
  FillOrms(pointer(arr), RecordType);
end;

function TOrmTable.FieldTable(Field: PtrInt): TOrmClass;
begin
  if (self = nil) or
     (PtrUInt(Field) >= PtrUInt(fFieldCount)) or
     (fQueryTables = nil) then
    result := nil
  else
  begin
    if fFieldType = nil then
      InitFieldTypes;
    Field := fFieldType[Field].TableIndex;
    if Field < 0 then
      result := nil
    else
      result := fQueryTables[Field];
  end;
end;

function TOrmTable.SearchValue(const UpperValue: RawUtf8;
  StartRow, FieldIndex: PtrInt; const Client: IRestOrm;
  Lang: TSynSoundExPronunciation; UnicodeComparison: boolean): PtrInt;
var
  Kind: TOrmFieldType;
  Search: PAnsiChar;
  UpperUnicode: RawUnicode;
  UpperUnicodeLen: integer;
  info: POrmTableFieldType;
  Val64: Int64;
  ValTimeLog: TTimelogBits absolute Val64;
  o, i: PtrInt;
  err: integer;
  EnumValue: RawUtf8;
  s: string;
  P: PShortString;
  EnumValues: set of 0..63;
  Soundex: TSynSoundEx;
  M: TOrmModel;
  tmp: array[0..23] of AnsiChar;
begin
  result := 0;
  if (self = nil) or
     (StartRow <= 0) or
     (StartRow > fRowCount) or
     (UpperValue = '') or
     (PtrUInt(FieldIndex) >= PtrUInt(fFieldCount)) then
    exit;
  Search := pointer(UpperValue);
  if Search^ = '%' then
  begin
    inc(Search);
    if Search^ = '%' then
    begin
      inc(Search);
      if Search^ = '%' then
      begin
        inc(Search);
        Lang := sndxSpanish;
      end
      else
        Lang := sndxFrench;
    end
    else
      Lang := sndxEnglish;
  end;
  if (Lang <> sndxNone) and
     not Soundex.Prepare(Search, Lang) then
    exit;
  result := StartRow;
  Kind := FieldType(FieldIndex, info);
  o := fFieldCount * StartRow + FieldIndex;
  // search in one specified field value
  if (Kind = oftEnumerate) and
     (info.ContentTypeInfo <> nil) then
  begin
    // for enumerates: first search in all available values
    Int64(EnumValues) := 0;
    P := PRttiEnumType(info.ContentTypeInfo)^.NameList;
    for i := 0 to PRttiEnumType(info.ContentTypeInfo)^.MaxValue do
    begin
      EnumValue := TrimLeftLowerCaseShort(P);
      GetCaptionFromPCharLen(pointer(EnumValue), s);
      StringToUtf8(s, EnumValue);
      if ((Lang <> sndxNone) and Soundex.Utf8(pointer(EnumValue))) or
         ((Lang = sndxNone) and FindUtf8(pointer(EnumValue), Search)) then
        include(EnumValues, i);
      inc(PByte(P), ord(P^[0]) + 1);
    end;
    // then search directly from the INTEGER value
    if Int64(EnumValues) <> 0 then
      while PtrUInt(result) <= PtrUInt(fRowCount) do
      begin
        i := GetInteger(GetResults(o), err);
        if (err = 0) and
           (i < 255) and
           (byte(i) in EnumValues) then
          exit; // we found a matching field
        inc(o, fFieldCount); // ignore all other fields -> jump to next row data
        inc(result);
      end;
    result := 0; // not found
    exit;
  end;
  // special cases: conversion from INTEGER to text before search
  if Kind in [oftTimeLog, oftModTime, oftCreateTime, oftUnixTime, oftUnixMSTime] then
    while PtrUInt(result) <= PtrUInt(fRowCount) do
    begin
      SetInt64(GetResults(o), Val64{%H-});
      if Val64 <> 0 then
      begin
        case Kind of
          oftUnixTime:
            ValTimeLog.FromUnixTime(Val64);
          oftUnixMSTime: // seconds resolution is enough for value search
            ValTimeLog.FromUnixMSTime(Val64);
        end;
        ValTimeLog.Text(tmp{%H-}, true, ' ')^ := #0;
        if FindAnsi(tmp, Search) then
          exit;
      end;
      inc(o, fFieldCount);
      inc(result);
    end
  else if (Kind in [oftRecord, oftID, oftTID, oftSessionUserID]) and
          (Client <> nil) and
          (Client.Model <> nil) then
  begin
    M := Client.Model;
    while PtrUInt(result) <= PtrUInt(fRowCount) do
    begin
      SetInt64(GetResults(o), Val64);
      if Val64 <> 0 then
      begin
        if Kind = oftRecord then
          EnumValue := RecordRef(Val64).Text(M)
        else // oftID/oftTID -> display ID number -> no sounded
          EnumValue := GetResults(o);
        if Lang = sndxNone then
        begin
          if FindUtf8(pointer(EnumValue), Search) then
            exit;
        end
        else if Soundex.Utf8(pointer(EnumValue)) then
          exit;
      end;
      inc(o, fFieldCount);
      inc(result);
    end
  end
  else // by default, search as UTF-8 encoded text
  if Lang <> sndxNone then
  begin
    while PtrUInt(result) <= PtrUInt(fRowCount) do
      if Soundex.Utf8(GetResults(o)) then
        exit
      else
      begin
        inc(o, fFieldCount);
        inc(result);
      end;
  end
  else if UnicodeComparison then
  begin
    // slowest but always accurate Unicode comparison
    UpperUnicode := Utf8DecodeToRawUnicodeUI(RawUtf8(Search), @UpperUnicodeLen);
    while PtrUInt(result) <= PtrUInt(fRowCount) do
      if FindUnicode(pointer(Utf8DecodeToRawUnicode(GetResults(o), 0)),
          pointer(UpperUnicode), UpperUnicodeLen) then
        exit
      else
      begin
        inc(o, fFieldCount);
        inc(result);
      end
  end
  else // default fast Win1252 search
    while PtrUInt(result) <= PtrUInt(fRowCount) do
      if FindUtf8(GetResults(o), Search) then
        exit
      else
      begin
        inc(o, fFieldCount);
        inc(result);
      end;
  result := 0; // not found
end;

function TOrmTable.SearchValue(const UpperValue: RawUtf8; StartRow: PtrInt;
  FieldIndex: PInteger; const Client: IRestOrm; Lang: TSynSoundExPronunciation;
  UnicodeComparison: boolean): PtrInt;
var
  F, Row: PtrInt;
begin
  result := 0;
  if (self = nil) or
     (StartRow <= 0) or
     (StartRow > fRowCount) or
     (UpperValue = '') then
    exit;
  // search in all fields values
  for F := 0 to FieldCount - 1 do
  begin
    Row := SearchValue(UpperValue, StartRow, F, Client, Lang, UnicodeComparison);
    if (Row <> 0) and
       ((result = 0) or (Row < result)) then
    begin
      if FieldIndex <> nil then
        FieldIndex^ := F;
      result := Row;
    end;
  end;
end;

function TOrmTable.ExpandAsString(Row, Field: PtrInt; const Client: IRestOrm;
  out Text: string; const CustomFormat: string): TOrmFieldType;
var
  info: POrmTableFieldType;
  err: integer;
  Value: Int64;
  ValueTimeLog: TTimeLogBits absolute Value;
  ValueDateTime: TDateTime;
  Ref: RecordRef absolute Value;
label
  dt;
begin // Text was already forced to '' because was defined as "out" parameter
  if Row = 0 then
  begin // Field Name
    result := oftUnknown;
    Text := GetCaption(0, Field);
    exit;
  end;
  result := FieldType(Field, info);
  case result of
    oftDateTime,
    oftDateTimeMS:
      begin
        Value := Iso8601ToTimeLogPUtf8Char(Get(Row, Field), 0);
dt:     if Value <> 0 then
        begin
          if CustomFormat <> '' then
          begin
            ValueDateTime := ValueTimeLog.ToDateTime;
            Text := FormatDateTime(CustomFormat, ValueDateTime);
            if Text <> CustomFormat then
              exit; // valid conversion
          end;
          Text := ValueTimeLog.i18nText;
          exit;
        end;
      end;
    oftBlob:
      Text := '???';
    oftFloat:
      if CustomFormat <> '' then
      try
        if pos('%', CustomFormat)>0 then
          Text := Format(CustomFormat, [GetExtended(Get(Row, Field))])
        else
          Text := FormatFloat(CustomFormat, GetExtended(Get(Row, Field)));
        exit;
      except
        on Exception do
          Text := '';
      end;
    oftCurrency:
      if CustomFormat <> '' then
      try
        if pos('%', CustomFormat)>0 then
          Text := Format(CustomFormat, [GetAsCurrency(Row, Field)])
        else
          Text := FormatCurr(CustomFormat, GetAsCurrency(Row, Field));
        exit;
      except
        on Exception do
          Text := '';
      end;
    oftEnumerate,
    oftSet,
    oftRecord,
    oftID,
    oftTID,
    oftRecordVersion,
    oftSessionUserID,
    oftTimeLog,
    oftModTime,
    oftCreateTime,
    oftUnixTime,
    oftUnixMSTime:
      begin
        Value := GetInt64(Get(Row, Field), err);
        if err <> 0 then
          // not an integer -> to be displayed as oftUtf8Text
          result := oftUtf8Text
        else
          case result of
            oftEnumerate:
              if info.ContentTypeInfo <> nil then
              begin
                Text := PRttiEnumType(info.ContentTypeInfo)^.GetCaption(Value);
                exit;
              end;
            oftTimeLog,
            oftModTime,
            oftCreateTime:
              goto dt;
            oftUnixTime:
              begin
                ValueTimeLog.FromUnixTime(Value);
                goto dt;
              end;
            oftUnixMSTime:
              if Value <> 0 then
              begin
                ValueDateTime := UnixMSTimeToDateTime(Value);
                if CustomFormat <> '' then
                begin
                  Text := FormatDateTime(CustomFormat, ValueDateTime);
                  if Text <> CustomFormat then
                    exit; // valid conversion
                end;
                Text := DateTimeToStr(ValueDateTime); // was DateTimeToi18n()
                exit;
              end;
      {      oftID, oftTID, oftSet, oftRecordVersion:
              result := oftUtf8Text; // will display INTEGER field as number }
            oftRecord:
              if (Value <> 0) and
                 (Client <> nil) then // 'TableName ID'
                {$ifdef UNICODE}
                Text := Ansi7ToString(Ref.Text(Client.Model))
                {$else}
                Text := Ref.Text(Client.Model)
                {$endif UNICODE}
              else
                result := oftUtf8Text; // display ID number if no table model
          end;
      end;
  end;
  if Text = '' then
    // returns the value as text by default
    Text := GetString(Row, Field);
end;

function TOrmTable.ExpandAsSynUnicode(Row, Field: PtrInt;
  const Client: IRestOrm; out Text: SynUnicode): TOrmFieldType;
var
  s: string;
begin
  result := ExpandAsString(Row, Field, Client, s);
  StringToSynUnicode(s, Text);
end;



{ TOrmTableJson }

constructor TOrmTableJson.Create(const aSql: RawUtf8; JsonBuffer: PUtf8Char;
  JsonBufferLen: integer);
begin // don't raise exception on error parsing
  inherited Create(aSql);
  ParseAndConvert(JsonBuffer, JsonBufferLen);
end;

constructor TOrmTableJson.Create(const aSql, aJson: RawUtf8);
var
  len: integer;
begin
  len := length(aJson);
  PrivateCopyChanged(pointer(aJson), len, {updatehash=}false);
  Create(aSql, pointer(fPrivateCopy), len);
end;

constructor TOrmTableJson.CreateFromTables(
  const Tables: array of TOrmClass; const aSql: RawUtf8;
  JsonBuffer: PUtf8Char; JsonBufferLen: integer);
begin
  // don't raise exception on error parsing
  inherited CreateFromTables(Tables, aSql);
  ParseAndConvert(JsonBuffer, JsonBufferLen);
end;

constructor TOrmTableJson.CreateFromTables(const Tables: array of TOrmClass;
  const aSql, aJson: RawUtf8; aJsonOwned: boolean);
var
  len: integer;
begin
  len := length(aJson);
  if aJsonOwned then
    fPrivateCopy := aJson
  else
    PrivateCopyChanged(pointer(aJson), len, {updatehash=}false);
  CreateFromTables(Tables, aSql, pointer(fPrivateCopy), len);
end;

constructor TOrmTableJson.CreateWithColumnTypes(
  const ColumnTypes: array of TOrmFieldType; const aSql: RawUtf8;
  JsonBuffer: PUtf8Char; JsonBufferLen: integer);
begin
  // don't raise exception on error parsing
  inherited CreateWithColumnTypes(ColumnTypes, aSql);
  ParseAndConvert(JsonBuffer, JsonBufferLen);
end;

constructor TOrmTableJson.CreateWithColumnTypes(
  const ColumnTypes: array of TOrmFieldType; const aSql, aJson: RawUtf8);
var
  len: integer;
begin
  len := length(aJson);
  PrivateCopyChanged(pointer(aJson), len, {updatehash=}false);
  CreateWithColumnTypes(ColumnTypes, aSql, pointer(fPrivateCopy), len);
end;

function TOrmTableJson.PrivateCopyChanged(aJson: PUtf8Char; aLen: integer;
  aUpdateHash: boolean): boolean;
var
  Hash: cardinal;
begin
  if aUpdateHash then
  begin
    Hash := DefaultHasher(0, pointer(aJson), aLen);
    result := (fPrivateCopyHash = 0) or
              (Hash = 0) or
              (Hash <> fPrivateCopyHash);
    if not result then
      exit;
    fPrivateCopyHash := Hash;
  end
  else
    result := true; // from Create() for better performance on single use
  FastSetString(fPrivateCopy, pointer(aJson), aLen);
end;

function GetFieldCountExpanded(P: PUtf8Char): integer;
var
  EndOfObject: AnsiChar;
begin
  result := 0;
  repeat
    P := GotoNextJsonItem(P, 2, @EndOfObject); // ignore Name+Value items
    if P = nil then
    begin // unexpected end
      result := 0;
      exit;
    end;
    inc(result);
    if EndOfObject = '}' then
      break; // end of object
  until false;
end;

function TOrmTableJson.ParseAndConvert(Buffer: PUtf8Char; BufferLen: integer): boolean;
var
  i, max, resmax, f: PtrInt;
  EndOfObject: AnsiChar;
  P, V: PUtf8Char;
  VLen: integer;
  wasString: boolean;
begin
  result := false; // error on parsing
  fFieldIndexID := -1;
  if (self = nil) or
     (Buffer = nil) then
    exit;
  // go to start of object
  {$ifndef NOPOINTEROFFSET}
  fDataStart := Buffer; // before first value, to ensure offset=0 means nil
  {$endif NOPOINTEROFFSET}
  P := GotoNextNotSpace(Buffer);
  if IsNotExpandedBuffer(P, Buffer + BufferLen, fFieldCount, fRowCount) then
  begin
    // A. Not Expanded (more optimized) format as array of values
(* {"fieldCount":9,"values":["ID","Int","Test","Unicode","Ansi","ValFloat","ValWord",
    "ValDate","Next",0,0,"abcde+?ef+?+?","abcde+?ef+?+?","abcde+?ef+?+?",
    3.14159265300000E+0000,1203,"2009-03-10T21:19:36",0,..],"rowCount":20} *)
    // 1. check RowCount and DataLen
    if fRowCount < 0 then
    begin
      // IsNotExpanded() detected invalid input
      fRowCount := 0;
      exit;
    end;
    // 2. initialize and fill fResults[] PPUtf8CharArray memory
    max := (fRowCount + 1) * fFieldCount;
    SetLength(fJsonData, max);
    {$ifndef NOTORMTABLELEN}
    SetLength(fLen, max);
    {$endif NOTORMTABLELEN}
    fData := pointer(fJsonData);
    // unescape+zeroify JSONData + fill fResults[] to proper place
    dec(max);
    for i := 0 to fFieldCount - 1 do
    begin
      V := GetJsonField(P, P, @wasString, nil, @VLen);
      SetResults(i, V, VLen);
      if not wasString then
        exit; // should start with field names
      if (fFieldIndexID < 0) and
         IsRowID(V) then
        fFieldIndexID := i;
    end;
    f := 0;
    for i := fFieldCount to max do
    begin
      // get a field value
      V := GetJsonFieldOrObjectOrArray(P, @wasString, nil,
        {handleobjarra=}true, {normalizebool=}false, @VLen);
      SetResults(i, V, VLen);
      if (P = nil) and
         (i <> max) then
        // failure (GetRowCountNotExpanded should have detected it)
        exit;
      if wasString then
        Include(fFieldParsedAsString, f); // mark column was "string"
      inc(f);
      if f = fFieldCount then
        f := 0; // reached next row
    end;
  end
  else
  begin
    // B. Expanded format as array of objects (each with field names)
(* [{"ID":0,"Int":0,"Test":"abcde+?ef+?+?","Unicode":"abcde+?ef+?+?","Ansi":
    "abcde+?ef+?+?","ValFloat": 3.14159265300000E+0000,"ValWord":1203,
    "ValDate":"2009-03-10T21:19:36","Next":0},{..}] *)
    // 1. get fields count from first row
    while P^ <> '[' do
      if P^ = #0 then
        exit
      else
        inc(P); // need an array of objects
    repeat
      inc(P);
      if P^ = #0 then
        exit;
    until P^ in ['{', ']']; // go to object beginning
    if P^ = ']' then
    begin
      // [] -> valid, but void data
      result := true;
      exit;
    end;
    inc(P);
    fFieldCount := GetFieldCountExpanded(P);
    if fFieldCount = 0 then
      // invalid data for first row
      exit;
    // 2. get values (assume fields are always the same as in the first object)
    max := fFieldCount; // index to start storing values in fResults[]
    resmax := max * 2;  // field names + 1 data row by default = 1 object
    SetLength(fJsonData, resmax);
    {$ifndef NOTORMTABLELEN}
    SetLength(fLen, resmax);
    {$endif NOTORMTABLELEN}
    fData := pointer(fJsonData); // needed for SetResults() below
    fRowCount := 0;
    repeat
      // let fJsonResults[] point to unescaped+zeroified JSON values
      for f := 0 to fFieldCount - 1 do
      begin
        if fRowCount = 0 then
        begin
          // get field name from 1st Row
          V := GetJsonPropName(P, @VLen);
          if (fFieldIndexID < 0) and
             IsRowID(V) then
            fFieldIndexID := f;
          SetResults(f, V, VLen);
          if P = nil then
            break;
        end
        else
        begin
          // warning: next field names are not checked, and should be correct
          P := GotoEndJsonItemString(P);
          if P = nil then
            break;
          inc(P); // ignore jcEndOfJsonFieldOr0
        end;
        if max >= resmax then
        begin // check space inside loop for GPF security
          resmax := NextGrow(resmax);
          SetLength(fJsonData, resmax);
          {$ifndef NOTORMTABLELEN}
          SetLength(fLen, resmax);
          {$endif NOTORMTABLELEN}
          fData := pointer(fJsonData);
        end;
        V := GetJsonFieldOrObjectOrArray(P, @wasString, @EndOfObject,
          {handleobjectarray=}true, {normbool=}false, @VLen);
        SetResults(max, V, VLen);
        if P = nil then
        begin
          // unexpected end
          fFieldCount := 0;
          break;
        end;
        if wasString then // mark column was "string"
          Include(fFieldParsedAsString, f);
        if (EndOfObject = '}') and
           (f < fFieldCount - 1) then
        begin
          // allow some missing fields in the input object
          inc(max, fFieldCount - f);
          break;
        end;
        inc(max);
      end;
      if P = nil then
        break; // unexpected end
      if {%H-}EndOfObject <> '}' then
        // data field layout is not consistent: should never happen
        break;
      inc(fRowCount);
      while (P^ <> '{') and
            (P^ <> ']') do
        // go to next object beginning
        if P^ = #0 then
          exit
        else
          inc(P);
      if P^ = ']' then
        break;
      inc(P); // jmp ']'
    until false;
    if max <> (fRowCount + 1) * fFieldCount then
    begin
      // field count must be the same for all objects
      fJsonData := nil;
      {$ifndef NOTORMTABLELEN}
      fLen := nil;
      {$endif NOTORMTABLELEN}
      fFieldCount := 0;
      fRowCount := 0;
      exit; // data field layout is not consistent: should never happen
    end;
  end;
  result := true; // if we reached here, means successfull conversion from P^
end;

function TOrmTableJson.UpdateFrom(const aJson: RawUtf8; var Refreshed: boolean;
  PCurrentRow: PInteger): boolean;
var
  len: integer;
begin
  len := length(aJson);
  if PrivateCopyChanged(pointer(aJson), len, {updatehash=}true) then
    if ParseAndConvert(pointer(fPrivateCopy), len) then
    begin // parse success from new aJson data -> need some other update?
      with fSortParams do
        if FieldCount <> 0 then
          // TOrmTable.SortFields() was called -> do it again
          SortFields(FieldIndex, Asc, PCurrentRow, FieldType);
      Refreshed := true;
      result := true;
    end
    else // parse error
      result := false
  else // data didn't change (fPrivateCopyHash checked)
    result := true;
end;


{ ------------ TOrm Definition }

// some methods defined ahead of time for proper inlining

class function TOrm.OrmProps: TOrmProperties;
begin
  result := PPointer(PAnsiChar(self) + vmtAutoTable)^;
  if result <> nil then
    // we expect TRttiCustom is in the slot, and PrivateSlot as TOrmProperties
    result := TRttiCustom(pointer(result)).PrivateSlot;
  if result = nil then
    // first time we use this TOrm class: generate information from RTTI
    result := PropsCreate;
end;

function TOrm.Orm: TOrmProperties;
begin
  // we know TRttiCustom is in the slot, and PrivateSlot is TOrmProperties
  result := PRttiCustom(PPAnsiChar(self)^ + vmtAutoTable)^.PrivateSlot;
end;

class function TOrm.SqlTableName: RawUtf8;
begin
  if self = nil then
    result := ''
  else
    result := OrmProps.SqlTableName;
end;


{ TOrmFill }

function TOrmFill.GetJoinedFields: boolean;
begin
  if self = nil then
    result := false
  else
    result := fJoinedFields;
end;

function TOrmFill.TableMapFields: TFieldBits;
begin
  if self = nil then
    FillZero(result{%H-})
  else
    result := fTableMapFields;
end;

procedure TOrmFill.AddMap(aRecord: TOrm; aField: TOrmPropInfo;
  aIndex: integer);
begin
  if (self = nil) or
     (aRecord = nil) then
    exit;
  if fTableMapCount >= length(fTableMap) then
    SetLength(fTableMap, fTableMapCount + fTableMapCount shr 1 + 16);
  with fTableMap[fTableMapCount] do
  begin
    Dest := aRecord;
    DestField := aField;
    TableIndex := aIndex;
    inc(fTableMapCount);
  end;
end;

procedure TOrmFill.AddMapFromName(aRecord: TOrm; aName: PUtf8Char; aIndex: integer);
var
  i: PtrInt;
begin
  if (self <> nil) and
     (aRecord <> nil) then
    if IsRowID(aName) then
      AddMap(aRecord, nil, aIndex)
    else
      with aRecord.Orm.Fields do
      begin
        i := IndexByName(aName);
        if i >= 0 then
        begin // only map if column name is a valid field
          include(fTableMapFields, i);
          AddMap(aRecord, List[i], aIndex);
        end;
      end;
end;

procedure TOrmFill.AddMapSimpleFields(aRecord: TOrm;
  const aProps: array of TOrmPropInfo; var aIndex: integer);
var
  i: PtrInt;
begin
  AddMap(aRecord, nil, aIndex);
  inc(aIndex);
  for i := 0 to high(aProps) do
    if aProps[i].OrmFieldTypeStored <> oftID then
    begin
      AddMap(aRecord, aProps[i], aIndex);
      inc(aIndex);
    end;
end;

destructor TOrmFill.Destroy;
begin
  try
    UnMap; // release fTable instance if necessary
  finally
    inherited;
  end;
end;

function TOrmFill.Fill(aRow: integer; aDest: TOrm): boolean;
var
  D: TOrm;
  f: integer;
  offs: PtrInt;
  P: PUtf8Char;
  map: POrmFillTableMap;
begin
  if (self = nil) or
     (Table = nil) or
     (cardinal(aRow) > cardinal(Table.fRowCount)) then
    result := False
  else
  begin
    aRow := aRow * Table.fFieldCount;
    map := pointer(fTableMap);
    for f := 0 to fTableMapCount - 1 do
    begin
      D := aDest;
      if D = nil then
        D := map^.Dest;
      offs := aRow + map^.TableIndex;
      P := Table.GetResults(offs);
      if map^.DestField = nil then
        SetID(P, D.fID)
      else
        map^.DestField.SetValue(D, P,
          {$ifdef NOTORMTABLELEN} StrLen(P) {$else} fTable.fLen[offs] {$endif},
          {wasstring=}map^.TableIndex in fTable.fFieldParsedAsString);
      inc(map);
    end;
    result := True;
  end;
end;

procedure TOrmFill.ComputeSetUpdatedFieldBits(Props: TOrmProperties;
  out Bits: TFieldBits);
begin
  if (self <> nil) and
     (fTable <> nil) and
     (fTableMapRecordManyInstances = nil) then
    // within FillPrepare/FillOne loop: update ID, TModTime and mapped fields
    Bits := fTableMapFields + Props.ComputeBeforeUpdateFieldsBits
  else
    // update all simple/custom fields (also for FillPrepareMany)
    Bits := Props.SimpleFieldsBits[ooUpdate];
end;

function TOrmFill.TableFieldIndexToMap(TableField: integer): POrmFillTableMap;
var
  i: integer;
begin
  result := pointer(fTableMap);
  for i := 1 to fTableMapCount do
    if result^.TableIndex = TableField then
      exit
    else
      inc(result);
  result := nil;
end;

procedure TOrmFill.ComputeSetUpdatedFieldIndexes(var Props: TIntegerDynArray);
var
  i: integer;
  map: POrmFillTableMap;
begin
  SetLength(Props, fTable.FieldCount);
  FillCharFast(pointer(Props)^, fTable.FieldCount * SizeOf(integer), 255); // -1
  map := pointer(fTableMap);
  for i := 1 to fTableMapCount do // no need to call TableFieldIndexToMap()
  begin
    if map^.DestField <> nil then
      Props[map^.TableIndex] := map^.DestField.PropertyIndex;
    inc(map);
  end;
end;

procedure TOrmFill.Map(aRecord: TOrm; aTable: TOrmTable;
  aCheckTableName: TOrmCheckTableName);
var
  f: PtrInt;
  fieldname: PUtf8Char;
  props: TOrmProperties;
begin
  if aTable = nil then // avoid any GPF
    exit;
  fTable := aTable;
  if aTable.fData = nil then
    exit; // void content
  props := nil;
  if aCheckTableName <> ctnNoCheck then
    props := aRecord.Orm; // e.g. ctnTrimmed from TOrmMany.DestGetJoined
  for f := 0 to aTable.FieldCount - 1 do
  begin
    fieldname := aTable.Results[f];
    if props <> nil then
      if IdemPChar(fieldname, pointer(props.SqlTableNameUpperWithDot)) then
        inc(fieldname, length(props.SqlTableNameUpperWithDot))
      else if aCheckTableName = ctnMandatory then
        continue;
    AddMapFromName(aRecord, fieldname, f);
  end;
  fFillCurrentRow := 1; // point to first data row (0 is field names)
end;

procedure TOrmFill.UnMap;
var
  i: PtrInt;
begin
  if self = nil then
    exit;
  fTableMapCount := 0;
  fFillCurrentRow := 0;
  // release TOrmMany.fDestID^ instances set by TOrm.FillPrepareMany()
  for i := 0 to length(fTableMapRecordManyInstances) - 1 do
    with fTableMapRecordManyInstances[i] do
    begin
      TObject(fDestID^).Free;
      fDestID^ := 0;
      fSourceID^ := 0;
    end;
  fTableMapRecordManyInstances := nil;
  FillZero(fTableMapFields);
  // free any previous fTable if necessary
  if Table <> nil then
  try
    if Table.OwnerMustFree then
      Table.Free;
  finally
    fTable := nil;
  end;
end;


{ TOrm }

// since "var class" are not available in Delphi 6-7, and is inherited by
// the children classes under latest Delphi versions (i.e. the "var class" is
// shared by all inherited classes, whereas we want one var per class), we
// reused one of the magic VMT slots (i.e. the one for automated methods,
// AutoTable, a relic from Delphi 2 that is generally not used anymore) - see
// http://hallvards.blogspot.com/2007/05/hack17-virtual-class-variables-part-ii.html
// [a slower alternative may have been to use a global TSynDictionary]

class function TOrm.PropsCreate: TOrmProperties;
var
  rtticustom: TRttiCustom;
begin
  // private sub function for proper TOrm.OrmProps method inlining
  rtticustom := Rtti.RegisterClass(self);
  Rtti.DoLock;
  try
    result := rtticustom.PrivateSlot; // Private is TOrmProperties
    if Assigned(result) then
      if result.InheritsFrom(TOrmProperties) then
        // registered by a background thread
        exit
      else
        // paranoid
        raise EModelException.CreateUtf8('%.OrmProps: vmtAutoTable=%',
          [self, result]);
    // create the properties information from RTTI
    result := TOrmProperties.Create(self);
    rtticustom.PrivateSlot := result; // will be owned by this TRttiCustom
    rtticustom.Flags := rtticustom.Flags +
      [rcfDisableStored,  // for AS_UNIQUE
       rcfHookWriteProperty, rcfHookReadProperty, // custom RttiWrite/RttiRead
       rcfClassMayBeID];  // for IsPropClassInstance
    self.InternalDefineModel(result);
  finally
    Rtti.DoUnLock;
  end;
end;

function TOrm.RecordClass: TOrmClass;
begin
  if self = nil then
    result := nil
  else
    result := POrmClass(self)^;
end;

procedure ManyFieldsCreate(self: TOrm; many: POrmPropInfoRttiMany);
var
  n: TDALen;
begin
  n := PDALen(PAnsiChar(many) - _DALEN)^ + _DAOFF;
  repeat
    many^.SetInstance(self, TOrmClass(many^.ObjectClass).Create);
    inc(many);
    dec(n);
  until n = 0;
end;

constructor TOrm.Create;
begin
  with OrmProps do // don't call inherited Create but OrmProps custom setup
    if pointer(ManyFields) <> nil then
      // auto-instanciate any TOrmMany instance
      ManyFieldsCreate(self, pointer(ManyFields));
end;

destructor TOrm.Destroy;
var
  i: PtrInt;
  props: TOrmProperties;
begin
  props := Orm;
  if fFill <> nil then
  begin
    if fFill.fJoinedFields then
      // free all TOrm instances created by TOrm.CreateJoined
      for i := 0 to length(props.JoinedFields) - 1 do
        props.JoinedFields[i].GetInstance(self).Free;
    fFill.Free; // call UnMap -> release fTable instance if necessary
  end;
  // free all TOrmMany instances created by TOrm.Create
  if props.ManyFields <> nil then
    for i := 0 to PDALen(PAnsiChar(props.ManyFields) - _DALEN)^ + (_DAOFF - 1) do
      props.ManyFields[i].GetInstance(self).Free;
  // free any registered T*ObjArray
  if props.DynArrayFieldsHasObjArray then
    for i := 0 to length(props.DynArrayFields) - 1 do
      with props.DynArrayFields[i] do
        if ObjArray <> nil then
          ObjArrayClear(PropInfo^.GetFieldAddr(self)^);
  inherited Destroy;
end;

constructor TOrm.Create(const aSimpleFields: array of const; aID: TID);
begin
  Create;
  fID := aID;
  if not SimplePropertiesFill(aSimpleFields) then
    raise EOrmException.CreateUtf8('Incorrect %.Create(aSimpleFields) call', [self]);
end;

function TOrm.CreateCopy: TOrm;
var
  f: PtrInt;
begin
  // create new instance
  result := POrmClass(self)^.Create;
  // copy properties content
  result.fID := fID;
  with Orm do
    for f := 0 to length(CopiableFields) - 1 do
      CopiableFields[f].CopyValue(self, result);
end;

function TOrm.CreateCopy(const CustomFields: TFieldBits): TOrm;
var
  f: PtrInt;
begin
  result := POrmClass(self)^.Create;
  // copy properties content
  result.fID := fID;
  with Orm do
    for f := 0 to Fields.Count - 1 do
      if (byte(f) in CustomFields) and
         (byte(f) in CopiableFieldsBits) then
        Fields.List[f].CopyValue(self, result);
end;

function TOrm.GetNonVoidFields: TFieldBits;
var
  f: PtrInt;
begin
  FillZero(result{%H-});
  with Orm do
    for f := 0 to Fields.Count - 1 do
      if (byte(f) in CopiableFieldsBits) and
         not Fields.List[f].IsValueVoid(self) then
        include(result, f);
end;

constructor TOrm.Create(const aClient: IRestOrm; aID: TID; ForUpdate: boolean);
begin
  Create;
  if aClient <> nil then
    aClient.Retrieve(aID, self, ForUpdate);
end;

constructor TOrm.Create(const aClient: IRestOrm;
  aPublishedRecord: TOrm; ForUpdate: boolean);
begin
  Create;
  if aClient <> nil then
    aClient.Retrieve(aPublishedRecord.ID, self, ForUpdate);
end;

constructor TOrm.Create(const aClient: IRestOrm; const aSqlWhere: RawUtf8);
begin
  Create;
  if aClient <> nil then
    aClient.Retrieve(aSqlWhere, self);
end;

constructor TOrm.Create(const aClient: IRestOrm;
  const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const);
begin
  Create;
  if aClient <> nil then
    aClient.Retrieve(FormatUtf8(FormatSqlWhere, [], BoundsSqlWhere), self);
end;

constructor TOrm.Create(const aClient: IRestOrm;
  const FormatSqlWhere: RawUtf8; const ParamsSqlWhere, BoundsSqlWhere: array of const);
begin
  Create;
  if aClient <> nil then
    aClient.Retrieve(FormatUtf8(FormatSqlWhere, ParamsSqlWhere, BoundsSqlWhere), self);
end;

constructor TOrm.CreateFrom(const JsonRecord: RawUtf8);
begin
  Create;
  FillFrom(JsonRecord);
end;

constructor TOrm.CreateFrom(P: PUtf8Char);
begin
  Create;
  FillFrom(P);
end;

constructor TOrm.CreateFrom(const aDocVariant: variant);
begin
  Create;
  FillFrom(aDocVariant);
end;

class procedure TOrm.InitializeTable(const Server: IRestOrmServer;
  const FieldName: RawUtf8; Options: TOrmInitializeTableOptions);
var
  f: PtrInt;
begin
  // is not part of TOrmProperties because has been declared as virtual
  if (self <> nil) and
     (Server <> nil) and
     (Options * INITIALIZETABLE_NOINDEX <> INITIALIZETABLE_NOINDEX) then
  begin
    // ensure ID/RowID column is indexed
    if not (itoNoIndex4ID in Options) then
      if (FieldName = '') or IsRowID(pointer(FieldName)) then
        Server.CreateSqlIndex(self, 'ID', true); // for external tables
    // automatic column indexation of fields which are commonly searched by value
    with OrmProps.Fields do
      for f := 0 to Count - 1 do
        with List[f] do
          if (FieldName = '') or
             IdemPropNameU(FieldName, Name) then
            if ((aIsUnique in Attributes) and
                not (itoNoIndex4UniqueField in Options)) or
               ((OrmFieldType = oftRecord) and
                not (itoNoIndex4RecordReference in Options)) or
               ((OrmFieldType = oftRecordVersion) and
                not (itoNoIndex4RecordVersion in Options)) or
               ((OrmFieldType = oftID) and
                not (itoNoIndex4NestedRecord in Options)) or
               ((OrmFieldType = oftTID) and
                not (itoNoIndex4TID in Options)) then
              Server.CreateSqlIndex(self, Name, false);
  end;
  // failure in above Server.CreateSqlIndex() are ignored (may already exist)
end;

procedure TOrm.FillFrom(aRecord: TOrm);
begin
  if (self <> nil) and
     (aRecord <> nil) then
    FillFrom(aRecord, aRecord.OrmProps.CopiableFieldsBits);
end;

procedure TOrm.FillFrom(aRecord: TOrm; const aRecordFieldBits: TFieldBits);
var
  i, f: PtrInt;
  S, D: TOrmPropInfoList;
  SP: TOrmPropInfo;
  wasString: boolean;
  tmp: RawUtf8;
begin
  if (self = nil) or
     (aRecord = nil) or
     IsZero(aRecordFieldBits) then
    exit;
  D := Orm.Fields;
  if POrmClass(aRecord)^.InheritsFrom(POrmClass(self)^) then
  begin
    // fast atttribution for two sibbling classes
    if POrmClass(aRecord)^ = POrmClass(self)^ then
      fID := aRecord.fID; // same class -> ID values will match
    for f := 0 to D.Count - 1 do
      if byte(f) in aRecordFieldBits then
        D.List[f].CopyValue(aRecord, self);
    exit;
  end;
  // two diverse tables -> don't copy ID, and per-name field lookup
  S := aRecord.OrmProps.Fields;
  for i := 0 to S.Count - 1 do
    if byte(i) in aRecordFieldBits then
    begin
      SP := S.List[i];
      if D.List[i].Name = SP.Name then
        // optimistic match
        f := i
      else
        f := D.IndexByName(SP.Name);
      if f >= 0 then
      begin
        SP.GetValueVar(aRecord, False, tmp, @wasString);
        D.List[f].SetValueVar(self, tmp, wasString);
      end;
    end;
end;

procedure TOrm.FillFrom(Table: TOrmTable; Row: integer);
begin
  try
    FillPrepare(Table);
    if Table.InternalState <> fInternalState then
      fInternalState := Table.InternalState;
    FillRow(Row);
  finally
    FillClose; // avoid GPF in TOrm.Destroy
  end;
end;

procedure TOrm.FillFrom(const JSONTable: RawUtf8; Row: integer);
var
  Table: TOrmTableJson;
  tmp: TSynTempBuffer; // work on a private copy
begin
  tmp.Init(JSONTable);
  try
    Table := TOrmTableJson.Create('', tmp.buf, tmp.len);
    try
      FillFrom(Table, Row);
    finally
      Table.Free;
    end;
  finally
    tmp.Done;
  end;
end;

procedure TOrm.FillFrom(const JsonRecord: RawUtf8; FieldBits: PFieldBits);
var
  tmp: TSynTempBuffer; // work on a private copy
begin
  tmp.Init(JsonRecord);
  try
    FillFrom(tmp.buf, FieldBits); // now we can safely call FillFrom()
  finally
    tmp.Done;
  end;
end;

procedure TOrm.FillFrom(P: PUtf8Char; FieldBits: PFieldBits);
(*
 NOT EXPANDED - optimized format with a JSON array of JSON values, fields first
 {"fieldCount":9,"values":["ID","Int","Test","Unicode","Ansi","ValFloat","ValWord",
   "ValDate","Next",0,0,"abcde+?ef+?+?","abcde+?ef+?+?","abcde+?ef+?+?",
   3.14159265300000E+0000,1203,"2009-03-10T21:19:36",0]}

 EXPANDED FORMAT - standard format with a JSON array of JSON objects
 {"ID":0,"Int":0,"Test":"abcde+?ef+?+?","Unicode":"abcde+?ef+?+?","Ansi":
  "abcde+?ef+?+?","ValFloat": 3.14159265300000E+0000,"ValWord":1203,
  "ValDate":"2009-03-10T21:19:36","Next":0}
*)
var
  F: array[0..MAX_SQLFIELDS - 1] of PUtf8Char; // store field/property names
  wasString: boolean;
  i, n: PtrInt;
  ValueLen: integer;
  Prop, Value: PUtf8Char;
begin
  if FieldBits <> nil then
    FillZero(FieldBits^);
  if P = nil then
    exit;
  while P^ <> '{' do  // go to start of object
    if P^ = #0 then
      exit
    else
      inc(P);
  // set each property from values using efficient TOrmPropInfo.SetValue()
  if Expect(P, FIELDCOUNT_PATTERN, 14) then
  begin
    // not expanded format: read the values directly from the input array
    n := GetNextItemCardinal(P, #0) - 1;
    if cardinal(n) > high(F) then
      exit;
    if Expect(P, ROWCOUNT_PATTERN, 12) then
      // just ignore "rowCount":.. here
      GetNextItemCardinal(P, #0);
    if not Expect(P, VALUES_PATTERN, 11) then
      exit;
    for i := 0 to n do
      F[i] := GetJsonField(P, P);
    for i := 0 to n do
    begin
      Value := GetJsonFieldOrObjectOrArray(
        P, @wasString, nil, true, true, @ValueLen);
      FillValue({%H-}F[i], Value, ValueLen, wasString, FieldBits);
    end;
  end
  else if P^ = '{' then
  begin
    // expanded format: check each property name
    inc(P);
    repeat
      Prop := GetJsonPropName(P);
      if (Prop = nil) or
         (P = nil) then
        break;
      Value := GetJsonFieldOrObjectOrArray(
        P, @wasString, nil, true, true, @ValueLen);
      FillValue(Prop, Value, ValueLen, wasString, FieldBits);
    until P = nil;
  end;
end;

procedure TOrm.FillFrom(const aDocVariant: variant);
var
  json: RawUtf8;
begin
  if _Safe(aDocVariant)^.IsObject then
  begin
    VariantSaveJson(aDocVariant, twJsonEscape, json);
    FillFrom(pointer(json));
  end;
end;

procedure TOrm.FillPrepare(Table: TOrmTable; aCheckTableName: TOrmCheckTableName);
begin
  if self = nil then
    exit;
  if fFill = nil then
    fFill := TOrmFill.Create
  else
    fFill.UnMap;
  fFill.Map(self, Table, aCheckTableName);
end;

function TOrm.FillPrepare(const aClient: IRestOrm; const aSqlWhere: RawUtf8;
  const aCustomFieldsCsv: RawUtf8; aCheckTableName: TOrmCheckTableName): boolean;
var
  T: TOrmTable;
begin
  result := false;
  FillClose; // so that no further FillOne will work
  if (self = nil) or
     (aClient = nil) then
    exit;
  T := aClient.MultiFieldValues(RecordClass, aCustomFieldsCsv, aSqlWhere);
  if T = nil then
    exit;
  T.OwnerMustFree := true;
  FillPrepare(T, aCheckTableName);
  result := true;
end;

function TOrm.FillPrepare(const aClient: IRestOrm; const FormatSqlWhere: RawUtf8;
  const BoundsSqlWhere: array of const; const aCustomFieldsCsv: RawUtf8): boolean;
var
  sqlwhere: RawUtf8;
begin
  sqlwhere := FormatUtf8(FormatSqlWhere, [], BoundsSqlWhere);
  result := FillPrepare(aClient, sqlwhere, aCustomFieldsCsv);
end;

function TOrm.FillPrepare(const aClient: IRestOrm; const FormatSqlWhere: RawUtf8;
  const ParamsSqlWhere, BoundsSqlWhere: array of const;
  const aCustomFieldsCsv: RawUtf8): boolean;
var
  sqlwhere: RawUtf8;
begin
  sqlwhere := FormatUtf8(FormatSqlWhere, ParamsSqlWhere, BoundsSqlWhere);
  result := FillPrepare(aClient, sqlwhere, aCustomFieldsCsv);
end;

function TOrm.FillPrepare(const aClient: IRestOrm;
  const aIDs: array of Int64; const aCustomFieldsCsv: RawUtf8): boolean;
begin
  if high(aIDs) < 0 then
    result := false
  else
    result := FillPrepare(aClient, SelectInClause('id', aIDs, '', INLINED_MAX),
      aCustomFieldsCsv);
end;

function TOrm.FillRow(aRow: integer; aDest: TOrm): boolean;
begin
  if self <> nil then
    if aDest = nil then
      result := fFill.Fill(aRow)
    else if fFill.fTableMapRecordManyInstances = nil then
      result := fFill.Fill(aRow, aDest)
    else
      raise EOrmException.CreateUtf8(
        '%.FillRow() forbidden after FillPrepareMany', [self])
  else
    result := false;
end;

function TOrm.FillOne(aDest: TOrm): boolean;
begin
  if (self = nil) or
     (fFill = nil) or
     (fFill.Table = nil) or
     (fFill.Table.fRowCount = 0) or // also check if FillTable is emtpy
     (cardinal(fFill.FillCurrentRow) > cardinal(fFill.Table.fRowCount)) then
    result := false
  else
  begin
    FillRow(fFill.FillCurrentRow, aDest);
    inc(fFill.fFillCurrentRow);
    result := true;
  end;
end;

function TOrm.FillRewind: boolean;
begin
  if (self = nil) or
     (fFill = nil) or
     (fFill.Table = nil) or
     (fFill.Table.fRowCount = 0) then
    result := false
  else
  begin
    fFill.fFillCurrentRow := 1;
    result := true;
  end;
end;

procedure TOrm.FillClose;
begin
  if self <> nil then
    fFill.UnMap;
end;

procedure TOrm.AppendFillAsJsonValues(W: TOrmWriter);
begin
  W.Add('[');
  while FillOne do
  begin
    GetJsonValues(W);
    W.AddComma;
  end;
  W.CancelLastComma;
  W.Add(']');
end;

procedure TOrm.FillValue(PropName, Value: PUtf8Char; ValueLen: PtrInt;
  wasString: boolean; FieldBits: PFieldBits);
var
  field: TOrmPropInfo;
begin
  if self <> nil then
    if IsRowID(PropName) then
      SetID(Value, fID)
    else
    begin
      field := Orm.Fields.ByName(PropName);
      if field <> nil then
      begin
        if (ValueLen = 0) and
           (Value <> nil) then
          ValueLen := StrLen(Value);
        field.SetValue(self, Value, ValueLen, wasString);
        if FieldBits <> nil then
          Include(FieldBits^, field.PropertyIndex);
      end;
    end;
end;

function TOrm.SetFieldSqlVars(const Values: TSqlVarDynArray): boolean;
var
  max, field: PtrInt;
begin
  result := false;
  max := length(Values) - 1;
  with Orm.Fields do
  begin
    // expect exact Values[] type match with FieldType[]
    if max <> Count - 1 then // must match field count
      exit
    else
      for field := 0 to max do
        if List[field].SqlDBFieldType <> Values[field].VType then
          exit;
    // now we can safely update field values
    for field := 0 to max do
      List[field].SetFieldSqlVar(self, Values[field]);
  end;
  result := true;
end;

procedure TOrm.GetBinaryValues(W: TBufferWriter);
var
  f: PtrInt;
begin
  with Orm.Fields do
    for f := 0 to Count - 1 do
      List[f].GetBinary(self, W);
end;

procedure TOrm.GetBinaryValuesSimpleFields(W: TBufferWriter);
var
  f: PtrInt;
begin
  with Orm do
    for f := 0 to SimpleFieldCount - 1 do
      SimpleFields[f].GetBinary(self, W);
end;

procedure TOrm.GetBinaryValues(W: TBufferWriter;
  const aFields: TFieldBits);
var
  f: PtrInt;
begin
  with Orm.Fields do
    for f := 0 to Count - 1 do
      if byte(f) in aFields then
        List[f].GetBinary(self, W);
end;

function TOrm.GetBinary: RawByteString;
var
  W: TBufferWriter;
  temp: TTextWriterStackBuffer; // 8KB
begin
  W := TBufferWriter.Create(temp{%H-});
  try
    W.WriteVarUInt64(fID);
    GetBinaryValues(W);
    result := W.FlushTo;
  finally
    W.Free;
  end;
end;

procedure TOrm.SetBinary(var Read: TFastReader);
begin
  fID := Read.VarUInt64;
  SetBinaryValues(Read);
end;

procedure TOrm.SetBinary(const binary: RawByteString);
var
  read: TFastReader;
begin
  read.Init(binary);
  SetBinary(read);
end;

procedure TOrm.SetBinaryValues(var Read: TFastReader);
var
  f: PtrInt;
begin
  with Orm.Fields do
    for f := 0 to Count - 1 do
      List[f].SetBinary(self, Read);
end;

procedure TOrm.SetBinaryValuesSimpleFields(var Read: TFastReader);
var
  f: PtrInt;
begin
  with Orm do
    for f := 0 to SimpleFieldCount - 1 do
      SimpleFields[f].SetBinary(self, Read);
end;

procedure TOrm.GetJsonValues(W: TOrmWriter);
var
  f, c: PtrInt;
  Props: TOrmPropInfoList;
begin
  if self = nil then
    exit;
  // write the row data
  if W.Expand then
  begin
    W.Add('{');
    if W.WithID then
      W.AddString(W.ColNames[0]);
  end;
  c := 0;
  if W.WithID then
  begin
    W.Add(fID);
    W.AddComma;
    if (owoID_str in W.OrmOptions) and W.Expand then
    begin
      W.AddShort('"ID_str":"');
      W.Add(fID);
      W.Add('"', ',');
    end;
    inc(c);
  end;
  if W.Fields <> nil then
  begin
    Props := Orm.Fields;
    for f := 0 to length(W.Fields) - 1 do
    begin
      if W.Expand then
      begin
        W.AddString(W.ColNames[c]); // '"'+ColNames[]+'":'
        inc(c);
      end;
      Props.List[W.Fields[f]].GetJsonValues(self, W);
      W.AddComma;
    end;
  end;
  W.CancelLastComma; // cancel last ','
  if W.Expand then
    W.Add('}');
end;

procedure TOrm.AppendAsJsonObject(W: TOrmWriter; Fields: TFieldBits);
var // Fields are not "const" since are modified if zero
  i: PtrInt;
  P: TOrmProperties;
  Props: TOrmPropInfoList;
begin
  if self = nil then
  begin
    W.AddNull;
    exit;
  end;
  W.AddShorter('{"ID":');
  W.Add(fID);
  P := Orm;
  if IsZero(Fields) then
    Fields := P.SimpleFieldsBits[ooSelect];
  Props := P.Fields;
  for i := 0 to Props.Count - 1 do
    if byte(i) in Fields then
    begin
      W.Add(',', '"');
      W.AddNoJsonEscape(pointer(Props.List[i].Name), length(Props.List[i].Name));
      W.Add('"', ':');
      Props.List[i].GetJsonValues(self, W);
    end;
  W.Add('}');
end;

procedure TOrm.AppendFillAsJsonArray(const FieldName: RawUtf8;
  W: TOrmWriter; const Fields: TFieldBits);
begin
  if FieldName <> '' then
    W.AddFieldName(FieldName);
  W.Add('[');
  while FillOne do
  begin
    AppendAsJsonObject(W, Fields);
    W.AddComma;
  end;
  W.CancelLastComma;
  W.Add(']');
  if FieldName <> '' then
    W.AddComma;
end;

procedure TOrm.ForceVariantFieldsOptions(aOptions: TDocVariantOptions);
var
  i: PtrInt;
  p: TOrmPropInfo;
begin
  if self <> nil then
    with Orm do
      if oftVariant in HasTypeFields then
        for i := 0 to Fields.Count - 1 do
        begin
          p := Fields.List[i];
          if (p.OrmFieldType = oftVariant) and
             p.InheritsFrom(TOrmPropInfoRttiVariant) then
            with TOrmPropInfoRttiVariant(p) do
              if PropInfo.GetterIsField then
                with _Safe(PVariant(PropInfo.GetterAddr(self))^)^ do
                  if Count > 0 then
                    options := aOptions;
        end;
end;

procedure TOrm.GetJsonValuesAndFree(Json: TOrmWriter);
begin
  if Json <> nil then
  try
    // write the row data
    GetJsonValues(Json);
    // end the Json object
    if not Json.Expand then
      Json.Add(']', '}');
    Json.FlushFinal;
  finally
    Json.Free;
  end;
end;

procedure TOrm.GetJsonValues(Json: TStream; Expand, withID: boolean;
  Occasion: TOrmOccasion; OrmOptions: TOrmWriterOptions);
var
  serializer: TOrmWriter;
  tmp: TTextWriterStackBuffer;
begin
  if self = nil then
    exit;
  with Orm do
    serializer := CreateJsonWriter(Json, Expand, withID,
      SimpleFieldsBits[Occasion], {knownrows=}0, 0, @tmp);
  serializer.OrmOptions := OrmOptions;
  GetJsonValuesAndFree(serializer);
end;

function TOrm.GetJsonValues(Expand, withID: boolean;
  const Fields: TFieldBits; OrmOptions: TOrmWriterOptions): RawUtf8;
var
  J: TRawByteStringStream;
  serializer: TOrmWriter;
  tmp: TTextWriterStackBuffer;
begin
  J := TRawByteStringStream.Create;
  try
    serializer := Orm.CreateJsonWriter(J, Expand, withID, Fields,
      {knownrows=}0, 0, @tmp);
    serializer.OrmOptions := OrmOptions;
    GetJsonValuesAndFree(serializer);
    result := J.DataString;
  finally
    J.Free;
  end;
end;

function TOrm.GetJsonValues(Expand, withID: boolean;
  const FieldsCsv: RawUtf8; OrmOptions: TOrmWriterOptions): RawUtf8;
var
  bits: TFieldBits;
begin
  if Orm.FieldBitsFromCsv(FieldsCsv, bits) then
    result := GetJsonValues(Expand, withID, bits, OrmOptions)
  else
    result := '';
end;

function TOrm.GetJsonValues(Expand, withID: boolean;
  Occasion: TOrmOccasion; UsingStream: TRawByteStringStream;
  OrmOptions: TOrmWriterOptions): RawUtf8;
var
  J: TRawByteStringStream;
begin
  if not withID and
     IsZero(Orm.SimpleFieldsBits[Occasion]) then
    // no simple field to write -> quick return
    result := ''
  else
  begin
    if UsingStream <> nil then
      J := UsingStream
    else
      J := TRawByteStringStream.Create;
    try
      GetJsonValues(J, Expand, withID, Occasion, OrmOptions);
      result := J.DataString;
    finally
      if UsingStream = nil then
        J.Free;
    end;
  end;
end;

{$ifdef ISDELPHI20062007} // circumvent a Delphi 2007 compiler paranoid warning
  {$warnings off}
{$endif ISDELPHI20062007}

class function TOrm.GetSqlCreate(aModel: TOrmModel): RawUtf8;
// not implemented in TOrmProperties since has been made virtual
var
  i: PtrInt;
  c: TClass;
  SQL, mname, cname, tokenizer: RawUtf8;
  M: TClass; // is a TOrmVirtualTableClass
  Props: TOrmModelProperties;
  fields: TOrmPropInfoList;
begin
  if aModel = nil then
    raise EModelException.CreateUtf8('Invalid %.GetSqlCreate(nil) call', [self]);
  Props := aModel.Props[self];
  if Props.Kind <>  ovkSQLite3 then
  begin
    // create a FTS3/FTS4/RTREE virtual table
    result := 'CREATE VIRTUAL TABLE ' + SqlTableName + ' USING ';
    case Props.Kind of
      ovkFTS3:
        result := result + 'fts3(';
      ovkFTS4:
        result := result + 'fts4(';
      ovkFTS5:
        result := result + 'fts5(';
      ovkRTree:
        result := result + 'rtree(RowID,';
      ovkRTreeInteger:
        result := result + 'rtree_i32(RowID,';
      ovkCustomForcedID,
      ovkCustomAutoID:
        begin
          M := aModel.VirtualTableModule(self);
          if (M = nil) or
             not Assigned(GetVirtualTableModuleName) then
            raise EModelException.CreateUtf8('No registered module for %', [self]);
          mname := GetVirtualTableModuleName(M);
          if Props.Props.Fields.Count = 0 then
            raise EModelException.CreateUtf8(
              'Virtual % % should have published properties', [mname, self]);
          result := result + mname + '(';
        end;
    else
      raise EModelException.CreateUtf8('%.GetSqlCreate(%)?', [self, ToText(Props.Kind)^]);
    end;
    fields := Props.Props.Fields;
    case Props.Kind of
      ovkFTS3,
      ovkFTS4,
      ovkFTS5:
        begin
          if (Props.fFTSWithoutContentFields <> '') and
             (Props.fFTSWithoutContentTableIndex >= 0) then
          begin
            result := FormatUtf8('%content="%",', [result,
              aModel.Tables[Props.fFTSWithoutContentTableIndex].SqlTableName]);
            if Props.Kind = ovkFTS5 then
              result := FormatUtf8('%content_rowid="ID",', [result]);
          end;
          for i := 0 to fields.Count - 1 do
            result := result + fields.List[i].Name + ',';
          if Props.Kind = ovkFTS5 then
            tokenizer := 'ascii'   // FTS5 knows ascii/porter/unicode61
          else
            tokenizer := 'simple'; // FTS3-4 know simple/porter/unicode61
          c := self;
          repeat
            ClassToText(c, cname); // TOrmFtsTest = class(TOrmFts3Porter)
            if IdemPChar(pointer(cname), 'TORMFTS') and
               (cname[8] in ['3', '4', '5']) then
            begin // e.g. TOrmFts3Porter -> 'tokenize=porter'
              if length(cname) > 8 then
                tokenizer := LowerCase(copy(cname, 9, 100));
              break;
            end;
            {$ifndef PUREMORMOT2}
            if IdemPChar(pointer(cname), 'TSQLRECORDFTS') and
               (cname[14] in ['3', '4', '5']) then
            begin // e.g. TSqlRecordFTS3Porter -> 'tokenize=porter'
              if length(cname) > 14 then
                tokenizer := LowerCase(copy(cname, 15, 100));
              break;
            end;
            {$endif PUREMORMOT2}
            c := GetClassParent(c);
          until c = TOrm;
          result := FormatUtf8('% tokenize=%)', [result, tokenizer]);
        end;
      ovkRTree,
      ovkRTreeInteger:
        begin
          for i := 0 to fields.Count - 1 do
            with fields.List[i] do
              if aAuxiliaryRTreeField in Attributes then // for SQlite3 >= 3.24.0
                result := FormatUtf8('%+% %', [result, Name,
                  Props.Props.OrmFieldTypeToSql(i)])
              else
                result := result + Name + ',';
          result[length(result)] := ')';
        end;
      ovkCustomForcedID,
      ovkCustomAutoID:
        result := result + GetVirtualTableSqlCreate(Props.Props);
    end;
  end
  else
  begin
    // inherits from TOrm: create a "normal" SQLite3 table
    result := 'CREATE TABLE ' + SqlTableName + '(ID INTEGER PRIMARY KEY AUTOINCREMENT, ';
    // we always add an ID field which is an INTEGER PRIMARY KEY
    // column, as it is always created (as hidden RowID) by the SQLite3 engine
    with Props.Props do
      for i := 0 to fields.Count - 1 do
        with fields.List[i] do
        begin
          SQL := OrmFieldTypeToSql(i); // = '' for field with no matching DB column
          if SQL <> '' then
          begin
            result := result + Name + SQL;
            if byte(i) in IsUniqueFieldsBits then
              insert(' UNIQUE', result, length(result) - 1);
          end;
        end;
    PWord(@result[length(result) - 1])^ := ord(')') + ord(';') shl 8;
  end;
end;

{$ifdef ISDELPHI20062007} // circumvent a Delphi 2007 compiler paranoid warning
  {$warnings on}
{$endif ISDELPHI20062007}

function TOrm.GetSqlSet: RawUtf8;
var
  i: PtrInt;
  V: RawUtf8;
  wasString: boolean;
begin
  result := '';
  if self = nil then
    exit;
  with Orm do
    for i := 0 to length(SimpleFields) - 1 do
      with SimpleFields[i] do
      begin
      // format is 'COL1='VAL1', COL2='VAL2'' }
        GetValueVar(self, true, V, @wasString);
        if wasString then
          V := QuotedStr(V);
        result := result + Name + '=' + V + ', ';
      end;
  if result <> '' then
    SetLength(result, length(result) - 2);
end;

function TOrm.GetSqlValues: RawUtf8;
var
  i: PtrInt;
  V: RawUtf8;
  wasString: boolean;
begin
  result := '';
  if self <> nil then
    with Orm do
      if SimpleFields = nil then
        exit
      else
      begin
        if HasNotSimpleFields then
          // get 'COL1,COL2': no 'ID,' for INSERT (false below)
          result := SqlTableSimpleFieldsNoRowID; // always <> '*'
        result := result + ' VALUES (';
        for i := 0 to length(SimpleFields) - 1 do
          with SimpleFields[i] do
          begin
            GetValueVar(self, true, V, @wasString);
            if wasString then
              V := QuotedStr(V);
            result := result + V + ',';
          end;
        result[length(result)] := ')';
      end;
end;

function TOrm.SameRecord(Reference: TOrm): boolean;
var
  i: PtrInt;
begin
  result := false;
  if (self = nil) or
     (Reference = nil) or
     (POrmClass(Reference)^ <> POrmClass(self)^) or
     (Reference.fID <> fID) then
    exit;
  with Orm do
    for i := 0 to length(SimpleFields) - 1 do // not compare RawBlob/TOrmMany
      if SimpleFields[i].CompareValue(self, Reference, false) <> 0 then
        exit; // properties don't have the same value
  result := true;
end;

function TOrm.SameValues(Reference: TOrm): boolean;
var
  O: TOrmPropInfo;
  i: PtrInt;
  This, Ref: TOrmProperties;
begin
  result := false;
  if (self = nil) or
     (Reference = nil) or
     (Reference.fID <> fID) then // ID field must be tested by hand
    exit;
  if self <> Reference then
    if POrmClass(Reference)^ = POrmClass(self)^ then
    begin
      with Orm do  // fast comparison on same exact class - as TOrm.SameRecord
        for i := 0 to length(SimpleFields) - 1 do // not compare RawBlob/TOrmMany
          if SimpleFields[i].CompareValue(self, Reference, false) <> 0 then
            exit; // properties don't have the same value
    end
    else
    begin
      // comparison of all properties of Reference against self
      This := Orm;
      Ref := Reference.Orm;
      for i := 0 to length(Ref.SimpleFields) - 1 do
        with Ref.SimpleFields[i] do
        begin
          // compare not RawBlob/TOrmMany fields
          O := This.Fields.ByRawUtf8Name(Name);
          if O = nil then
            exit; // this Reference property doesn't exist in current object
          if GetValue(Reference, false, nil) <> O.GetValue(self, false, nil) then
            exit; // properties don't have the same value
        end;
    end;
  result := true;
end;

procedure TOrm.ClearProperties;
var
  i: PtrInt;
begin
  if self = nil then
    exit;
  fInternalState := 0;
  fID := 0;
  with Orm do
    if fFill.JoinedFields then
    begin
      for i := 0 to length(CopiableFields) - 1 do
        if CopiableFields[i].OrmFieldType <> oftID then
          CopiableFields[i].SetValue(self, nil, 0, false)
        else
          // clear nested allocated TOrm
          TOrm(TOrmPropInfoRttiInstance(CopiableFields[i]).GetInstance(self)).
            ClearProperties;
    end
    else
      for i := 0 to length(CopiableFields) - 1 do
        CopiableFields[i].SetValue(self, nil, 0, false);
end;

procedure TOrm.ClearProperties(const aFieldsCsv: RawUtf8);
var
  bits: TFieldBits;
  f: PtrInt;
begin
  if (self = nil) or
     (aFieldsCsv = '') then
    exit;
  with Orm do
  begin
    if aFieldsCsv = '*' then
      bits := SimpleFieldsBits[ooInsert]
    else if not FieldBitsFromCsv(aFieldsCsv, bits) then
      exit;
    for f := 0 to Fields.Count - 1 do
      if (byte(f) in bits) and
         (Fields.List[f].OrmFieldType in COPIABLE_FIELDS) then
        Fields.List[f].SetValue(self, nil, 0, false); // clear field value
  end;
end;

function TOrm.ClassProp: TRttiJson;
begin
  if self <> nil then
    result := PPointer(PPAnsiChar(self)^ + vmtAutoTable)^
  else
    result := nil; // avoid GPF
end;

function TOrm.RecordReference(Model: TOrmModel): TRecordReference;
begin
  if (self = nil) or
     (fID <= 0) then
    result := 0
  else
  begin
    result := Model.GetTableIndexExisting(POrmClass(self)^);
    if result > 63 then // TRecordReference handle up to 64=1 shl 6 tables
      result := 0
    else
      inc(result, fID shl 6);
  end;
end;

function TOrm.SimplePropertiesFill(const aSimpleFields: array of const): boolean;
var
  i: PtrInt;
  tmp: RawUtf8;
begin
  if self = nil then
    result := false
  else // means error
    with Orm do
      if length(SimpleFields) <> length(aSimpleFields) then
        result := false
      else
      begin
        for i := 0 to high(aSimpleFields) do
        begin
          VarRecToUtf8(aSimpleFields[i], tmp); // will work for every type
          SimpleFields[i].SetValueVar(self, tmp, false);
        end;
        result := True;
      end;
end;

function TOrm.FillFromArray(const Fields: TFieldBits; Json: PUtf8Char): boolean;
var
  i: PtrInt;
  val: PUtf8Char;
  vallen: integer;
  wasstring: boolean;
  props: TOrmPropInfoList;
begin
  result := false;
  Json := GotoNextNotSpace(Json);
  if Json^ <> '[' then
    exit;
  inc(Json);
  props := Orm.Fields;
  for i := 0 to props.Count - 1 do
    if GetBitPtr(@Fields, i) then
    begin
      val := GetJsonFieldOrObjectOrArray(Json, @wasstring, nil, true, true, @vallen);
      props.List[i].SetValue(self, val, vallen, wasstring);
    end;
  result := Json <> nil;
end;

constructor TOrm.CreateAndFillPrepare(const aClient: IRestOrm;
  const aSqlWhere: RawUtf8; const aCustomFieldsCsv: RawUtf8);
var
  aTable: TOrmTable;
begin
  Create;
  aTable := aClient.MultiFieldValues(RecordClass, aCustomFieldsCsv, aSqlWhere);
  if aTable = nil then
    exit;
  aTable.OwnerMustFree := true;
  FillPrepare(aTable);
end;

constructor TOrm.CreateAndFillPrepare(const aClient: IRestOrm;
  const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const;
  const aCustomFieldsCsv: RawUtf8);
var
  where: RawUtf8;
begin
  where := FormatUtf8(FormatSqlWhere, [], BoundsSqlWhere);
  CreateAndFillPrepare(aClient, where, aCustomFieldsCsv);
end;

constructor TOrm.CreateAndFillPrepare(const aClient: IRestOrm;
  const FormatSqlWhere: RawUtf8; const ParamsSqlWhere, BoundsSqlWhere: array of const;
  const aCustomFieldsCsv: RawUtf8);
var
  where: RawUtf8;
begin
  where := FormatUtf8(FormatSqlWhere, ParamsSqlWhere, BoundsSqlWhere);
  CreateAndFillPrepare(aClient, where, aCustomFieldsCsv);
end;

constructor TOrm.CreateAndFillPrepare(const aClient: IRestOrm;
  const aIDs: array of Int64; const aCustomFieldsCsv: RawUtf8);
begin
  Create;
  FillPrepare(aClient, aIDs, aCustomFieldsCsv);
end;

constructor TOrm.CreateAndFillPrepare(const aJson: RawUtf8);
var
  aTable: TOrmTable;
begin
  Create;
  aTable := TOrmTableJson.CreateFromTables([RecordClass], '', aJson);
  aTable.OwnerMustFree := true;
  FillPrepare(aTable);
end;

constructor TOrm.CreateAndFillPrepare(aJson: PUtf8Char; aJsonLen: integer);
var
  aTable: TOrmTable;
begin
  Create;
  aTable := TOrmTableJson.CreateFromTables([RecordClass], '', aJson, aJsonLen);
  aTable.OwnerMustFree := true;
  FillPrepare(aTable);
end;

constructor TOrm.CreateAndFillPrepareJoined(const aClient: IRestOrm;
  const aFormatSQLJoin: RawUtf8; const aParamsSQLJoin, aBoundsSQLJoin: array of const);
var
  i: PtrInt;
  n: integer;
  props: TOrmModelProperties;
  T: TOrmTable;
  instance: TOrm;
  SQL: RawUtf8;
begin
  Create;
  props := aClient.Model.Props[POrmClass(self)^];
  if props.props.JoinedFields = nil then
    raise EModelException.CreateUtf8('No nested TOrm to JOIN in %', [self]);
  SQL := props.SQL.SelectAllJoined;
  if aFormatSQLJoin <> '' then
    SQL := SQL + FormatUtf8(SqlFromWhere(aFormatSQLJoin), aParamsSQLJoin, aBoundsSQLJoin);
  T := aClient.ExecuteList(props.props.JoinedFieldsTable, SQL);
  if T = nil then
    exit;
  fFill := TOrmFill.Create;
  fFill.fJoinedFields := True;
  fFill.fTable := T;
  fFill.fTable.OwnerMustFree := true;
  n := 0;
  with props.props do
  begin // follow SQL.SelectAllJoined columns
    fFill.AddMapSimpleFields(self, SimpleFields, n);
    for i := 1 to length(JoinedFieldsTable) - 1 do
    begin
      instance := JoinedFieldsTable[i].Create;
      JoinedFields[i - 1].SetInstance(self, instance);
      fFill.AddMapSimpleFields(instance,
        JoinedFieldsTable[i].OrmProps.SimpleFields, n);
    end;
  end;
  fFill.fFillCurrentRow := 1; // point to first data row (0 is field names)
end;

constructor TOrm.CreateJoined(const aClient: IRestOrm; aID: TID);
begin
  CreateAndFillPrepareJoined(aClient, '%.RowID=?', [Orm.SqlTableName], [aID]);
  FillOne;
end;

constructor TOrm.CreateAndFillPrepareMany(const aClient: IRestOrm;
  const aFormatSQLJoin: RawUtf8; const aParamsSQLJoin, aBoundsSQLJoin: array of const);
begin
  Create;
  if Length(Orm.ManyFields) = 0 then
    raise EModelException.CreateUtf8(
      '%.CreateAndFillPrepareMany() with no many-to-many fields', [self]);
  if not FillPrepareMany(aClient, aFormatSQLJoin, aParamsSQLJoin, aBoundsSQLJoin) then
    raise EModelException.CreateUtf8(
      '%.CreateAndFillPrepareMany(): FillPrepareMany() failure', [self]);
end;

{$ifdef ISDELPHI20062007}
  {$warnings off} // avoid paranoid Delphi 2007 warning
{$endif ISDELPHI20062007}

function TOrm.EnginePrepareMany(const aClient: IRestOrm;
  const aFormatSQLJoin: RawUtf8; const aParamsSQLJoin, aBoundsSQLJoin: array of const;
  out ObjectsClass: TOrmClassDynArray; out SQL: RawUtf8): RawUtf8;
var
  aSqlFields, aSqlFrom, aSqlWhere, aSqlJoin: RawUtf8;
  aField: string[3];
  aMany: RawUtf8;
  f, n, i, SqlFieldsCount: integer;
  Props: TOrmProperties;
  SqlFields: array of record
    SQL: string[3];
    prop: TOrmPropInfo;
    Instance: TOrm;
  end;
  M: TOrmMany;
  D: TOrm;
  J, JBeg: PUtf8Char;
  Objects: array of TOrm;

  function AddField(aProp: TOrmPropInfo): boolean;
  begin
    if SqlFieldsCount >= MAX_SQLFIELDS then
      result := false
    else
      with SqlFields[SqlFieldsCount] do
      begin
        SQL := aField;
        prop := aProp;
        Instance := Objects[f];
        inc(SqlFieldsCount);
        result := true;
      end;
  end;

  function ProcessField(var P: PUtf8Char): RawUtf8;
  var
    B: PUtf8Char;
    field: TOrmPropInfo;
    i: PtrInt;
    M: TOrmMany;
    aManyField: string[63];

    function GetManyField(F: PUtf8Char): boolean;
    var
      B: PUtf8Char;
    begin
      result := true;
      B := F;
      while tcIdentifier in TEXT_CHARS[F^] do
        inc(F); // go to end of sub-field name
      if B = F then
      begin
        result := false;
        exit;
      end;
      dec(B, 2); // space for 'C.'
      SetString(aManyField, B, F - B);
      aManyField[2] := '.';
      P := F;
    end;

  begin
    B := P;
    while tcIdentifier in TEXT_CHARS[P^] do
      inc(P); // go to end of field name
    FastSetString(result, B, P - B);
    if (result = '') or
       IdemPropNameU(result, 'AND') or
       IdemPropNameU(result, 'OR')  or
       IdemPropNameU(result, 'LIKE') or
       IdemPropNameU(result, 'NOT') or
       IdemPropNameU(result, 'NULL') then
      exit;
    if not IsRowID(pointer(result)) then
    begin
      i := Props.Fields.IndexByName(result);
      if i < 0 then
        exit;
      field := Props.Fields.List[i];
      if field.OrmFieldType = oftMany then
      begin
        M := TOrmPropInfoRttiInstance(field).GetInstance(self) as TOrmMany;
        for i := 0 to n - 1 do
          if Objects[i * 2 + 1] = M then
          begin
            if IdemPChar(P, '.DEST.') then
            begin // special case of Many.Dest.*
              if GetManyField(P + 6) then
              begin
                aManyField[1] := AnsiChar(i * 2 + 67);
                result := RawUtf8(aManyField);
                exit; // Categories.Dest.Name=? -> C.Name=?
              end;
            end
            else if (P^ = '.') and GetManyField(P + 1) then
            begin
              aManyField[1] := AnsiChar(i * 2 + 66);
              result := RawUtf8(aManyField);
              exit;  // Categories.Kind=? -> CC.Kind=?
            end;
          end;
        exit;
      end;
    end;
    result := 'A.' + result; // Owner=? -> A.Owner=?
  end;

begin
  result := '';
  FillClose; // so that no further FillOne will work
  if (self = nil) or
     (aClient = nil) then
    exit;
  // reset TOrmFill object
  if fFill = nil then
    fFill := TOrmFill.Create
  else
    fFill.UnMap;
  // compute generic joined SQL statement and initialize Objects*[]+SqlFields[]
  SetLength(SqlFields, MAX_SQLFIELDS);
  Props := Orm;
  n := Length(Props.ManyFields);
  if n = 0 then
    exit;
  SetLength(Objects, n * 2 + 1);
  SetLength(ObjectsClass, n * 2 + 1);
  Objects[0] := self;
  ObjectsClass[0] := POrmClass(self)^;
  SetLength(fFill.fTableMapRecordManyInstances, n);  // fFill.UnMap will release memory
  for f := 0 to n - 1 do
  begin
    M := TOrmMany(Props.ManyFields[f].GetInstance(self));
    if M = nil then
      raise EOrmException.CreateUtf8('%.Create should have created %:% for EnginePrepareMany',
        [self, Props.ManyFields[f].Name, Props.ManyFields[f].ObjectClass]);
    fFill.fTableMapRecordManyInstances[f] := M;
    Objects[f * 2 + 1] := M;
    ObjectsClass[f * 2 + 1] := POrmClass(M)^;
    with M.Orm do
    begin
      if (fRecordManySourceProp.ObjectClass <> PClass(self)^) or
         (fRecordManyDestProp.ObjectClass = nil) then
        raise EOrmException.CreateUtf8('%.EnginePrepareMany %:% mismatch',
          [self, Props.ManyFields[f].Name, Props.ManyFields[f].ObjectClass]);
      ObjectsClass[f * 2 + 2] := TOrmClass(fRecordManyDestProp.ObjectClass);
      D := TOrmClass(fRecordManyDestProp.ObjectClass).Create;
      // let TOrmMany.Source and Dest point to real instances
      M.fSourceID^ := PtrInt(self);
      M.fDestID^ := PtrInt(D);
    end;
    Objects[f * 2 + 2] := TOrm(M.fDestID^);
    if Props.fSqlFillPrepareMany = '' then
    begin
      aMany := AnsiChar(f * 2 + 66); // Many=B,D,F...
      if {%H-}aSqlWhere <> '' then
        aSqlWhere := aSqlWhere + ' and ';
      aSqlWhere := FormatUtf8('%%.Source=A.RowID and %.Dest=%.RowID',
        [aSqlWhere, aMany, aMany, AnsiChar(f * 2 + 67){Dest=C,E,G..}]);
    end;
  end;
  SqlFieldsCount := 0;
  aField := 'A00';
  for f := 0 to length(ObjectsClass) - 1 do
    with ObjectsClass[f].OrmProps do
    begin
      PWord(@aField[2])^ := ord('I') + ord('D') shl 8;
      if not AddField(nil) then
        Exit; // try to add the ID field
      if Props.fSqlFillPrepareMany = '' then
      begin
        if {%H-}aSqlFields <> '' then
          aSqlFields := aSqlFields + ',';
        aSqlFields := FormatUtf8('%%.RowID %', [aSqlFields, aField[1], aField]);
      end;
      for i := 0 to length(SimpleFields) - 1 do
        with SimpleFields[i] do
        begin
          if (f and 1 = 0) {self/dest}  or
             not (IdemPropNameU(Name, 'SOURCE') or
             IdemPropNameU(Name, 'DEST')) {many} then
          begin
            PWord(@aField[2])^ := TwoDigitLookupW[i];
            if not AddField(SimpleFields[i]) then
              exit; // try to add this simple field
            if Props.fSqlFillPrepareMany = '' then
              aSqlFields := FormatUtf8('%,%.% %', [aSqlFields, aField[1], Name, aField]);
          end;
        end;
      if Props.fSqlFillPrepareMany = '' then
      begin
        if {%H-}aSqlFrom <> '' then
          aSqlFrom := aSqlFrom + ',';
        aSqlFrom := aSqlFrom + SqlTableName + ' ' + ToUtf8(aField[1]);
      end;
      inc(aField[1]);
    end;
  if Props.fSqlFillPrepareMany <> '' then
    SQL := Props.fSqlFillPrepareMany
  else
  begin
    FormatUtf8('select % from % where %', [aSqlFields, aSqlFrom, aSqlWhere], SQL);
    Props.fSqlFillPrepareMany := SQL;
  end;
  // process aFormatSQLJoin,aParamsSQLJoin and aBoundsSQLJoin parameters
  if aFormatSQLJoin <> '' then
  begin
    aSqlWhere := '';
    FormatUtf8(aFormatSQLJoin, aParamsSQLJoin, aSqlJoin);
    JBeg := pointer(aSqlJoin);
    repeat
      J := JBeg;
      while not (tcIdentifier in TEXT_CHARS[J^]) do
      begin
        case J^ of
          '"':
            repeat
              inc(J)
            until J^ in [#0, '"'];
          '''':
            repeat
              inc(J)
            until J^ in [#0, ''''];
        end;
        if J^ = #0 then
          break;
        inc(J);
      end;
      if J <> JBeg then
      begin // append ' ',')'..
        FastSetString(aSqlFrom, JBeg, J - JBeg);
        aSqlWhere := aSqlWhere + aSqlFrom;
        JBeg := J;
      end;
      if J^ = #0 then
        break;
      aSqlWhere := aSqlWhere + ProcessField(JBeg);
    until JBeg^ = #0;
    SQL := SQL + ' and (' + FormatUtf8(aSqlWhere, [], aBoundsSQLJoin) + ')';
  end;
  // execute SQL statement and retrieve the matching data
  result := aClient.ExecuteJson([], SQL);
  if result <> '' then // prepare Fill mapping on success - see FillPrepareMany()
    for i := 0 to SqlFieldsCount - 1 do
      with SqlFields[i] do
        fFill.AddMap(Instance, prop, i);
end;

{$ifdef ISDELPHI20062007}
  {$warnings on} // avoid paranoid Delphi 2007 warning
{$endif ISDELPHI20062007}

function TOrm.FillPrepareMany(const aClient: IRestOrm;
  const aFormatSQLJoin: RawUtf8;
  const aParamsSQLJoin, aBoundsSQLJoin: array of const): boolean;
var
  json, sql: RawUtf8;
  ObjectsClass: TOrmClassDynArray;
  T: TOrmTable;
begin
  result := false;
  json := EnginePrepareMany(aClient, aFormatSQLJoin, aParamsSQLJoin,
    aBoundsSQLJoin, ObjectsClass, sql);
  if json = '' then
    exit;
  T := TOrmTableJson.CreateFromTables(ObjectsClass, sql, json,
    {ownJSON=}PRefCnt(PAnsiChar(pointer(json)) - _STRREFCNT)^ = 1);
  if (T = nil) or
     (T.fData = nil) then
  begin
    T.Free;
    exit;
  end;
  { assert(T.FieldCount=SqlFieldsCount);
    for i := 0 to SqlFieldsCount-1 do
      assert(IdemPropNameU(SqlFields[i].sql,T.fResults[i],T.fLen[i])); }
  fFill.fTable := T;
  T.OwnerMustFree := true;
  fFill.fFillCurrentRow := 1; // point to first data row (0 is field names)
  result := true;
end;

class procedure TOrm.RttiCustomSetParser(Rtti: TRttiCustom);
var
  read: TOnClassJsonRead;
  write: TOnClassJsonWrite;
begin
  inherited RttiCustomSetParser(Rtti); // register fID as 'ID' field
  read := RttiJsonRead; // enhanced parsing using Fields.SetValue/GetJsonValues
  Rtti.JsonReader := TMethod(read);
  write := RttiJsonWrite;
  Rtti.JsonWriter := TMethod(write);
end;

function TOrm.IsPropClassInstance(Prop: PRttiCustomProp): boolean;
begin
  // returns TRUE for object serialization, FALSE for integer value
  result := fFill.JoinedFields;
end;

function TOrm.RttiWritePropertyValue(W: TTextWriter; Prop: PRttiCustomProp;
  Options: TTextWriterWriteObjectOptions): boolean;
begin
  if (not(rcfClassMayBeID in Prop^.Value.Flags)) or
     IsPropClassInstance(Prop) or
     (Prop^.OffsetGet < 0) then
    result := false // default JSON object serialization
  else
  begin
    W.Add(PPtrInt(PAnsiChar(self) + Prop^.OffsetGet)^); // serialized as integer
    result := true; // abort default serialization
  end;
end;

function TOrm.RttiBeforeReadPropertyValue(Ctxt: pointer;
  Prop: PRttiCustomProp): boolean;
begin
  if (not(rcfClassMayBeID in Prop^.Value.Flags)) or
     IsPropClassInstance(Prop) or
     (Prop^.OffsetSet < 0) then
    result := false // default JSON object serialization
  else
  begin
    PPtrInt(PAnsiChar(self) + Prop^.OffsetSet)^ :=
      PJsonParserContext(Ctxt)^.ParseInteger;
    result := true; // abort default serialization
  end;
end;

class procedure TOrm.RttiJsonRead(var Context: TJsonParserContext; Instance: TObject);
var
  cur: POrmPropInfo;
  f: TOrmPropInfo;
  props: TOrmPropInfoList;
  name: PUtf8Char;
  namelen: integer;
  orm: TOrm absolute Instance;
begin
  // manually parse incoming JSON object using Orm.Fields.SetValue()
  if not Context.ParseObject then
    exit; // invalid or {} or null
  props := OrmProps.Fields;
  cur := pointer(props.List);
  repeat
     name := GetJsonPropName(Context.Json, @namelen);
     if name = nil then
       Context.Valid := false;
     if not Context.ParseNextAny then // GetJsonFieldOrObjectOrArray()
       exit;
     if IsRowID(name, namelen) then // handle both ID and RowID names
       SetID(Context.Value, orm.fID)
     else
     begin
       if (cur <> nil) and
          IdemPropNameU(cur^.Name, name, namelen) then
       begin
         f := cur^; // optimistic O(1) property lookup
         inc(cur);
       end
       else
       begin
         f := props.ByName(name); // O(log(n)) binary search
         cur := nil;
       end;
       if f <> nil then // just ignore unknown property names
         f.SetValue(orm, Context.Value, Context.ValueLen, Context.WasString);
     end;
  until Context.EndOfObject = '}';
  Context.ParseEndOfObject;
end;

const
  ID_JSON: array[boolean] of string[7] = (
    'RowID', 'ID'); // see also TOrmWriter.SetOrmOptions: Ajax requires ID

class procedure TOrm.RttiJsonWrite(W: TJsonWriter; Instance: TObject;
  Options: TTextWriterWriteObjectOptions);
var
  cur: POrmPropInfo;
  props: TOrmPropInfoList;
  n: integer;
begin
  if Instance = nil then
  begin
    W.AddNull;
    exit;
  end;
  W.BlockBegin('{', Options);
  W.AddPropJsonInt64(ID_JSON[woIDAsIDstr in Options], TOrm(Instance).fID);
  props := TOrm(Instance).Orm.Fields;
  cur := pointer(props.List);
  n := props.Count;
  repeat
    W.WriteObjectPropName(pointer(cur^.Name), length(cur^.Name), Options);
    cur^.GetJsonValues(Instance, W);
    inc(cur);
    dec(n);
    if n = 0 then
      break;
    W.BlockAfterItem(Options);
  until false;
  W.BlockEnd('}', Options);
end;

function TOrm.GetID: TID;
begin
  {$ifdef OSWINDOWS}
  if PtrUInt(self) < PtrUInt(SystemInfo.lpMinimumApplicationAddress) then
    // was called from a TOrm property (oftID type)
    // (will return 0 if current instance is nil)
    result := PtrUInt(self)
  else
    result := fID;
    // was called from a real TOrm instance
  {$else}
  if PtrUInt(self) < $100000 then // rough estimation, may work in practice
    result := PtrUInt(self)
  else
  try
    result := fID;
  except
    result := PtrUInt(self);
  end;
  {$endif OSWINDOWS}
end;

function TOrm.GetIDAsPointer: pointer;
begin
  {$ifdef OSWINDOWS}
  if PtrUInt(self) < PtrUInt(SystemInfo.lpMinimumApplicationAddress) then
    // was called from a TOrm property (oftID type)
    // (will return 0 if current instance is nil)
    result := self
  else    // was called from a real TOrm instance
  {$ifdef CPU32}
  if fID > MaxInt then
    raise EOrmException.CreateUtf8('%.GetIDAsPointer is storing ID=%, which ' +
      'cannot be stored in a pointer/TOrm 32-bit instance: use ' +
      'a TID/T*ID published field for 64-bit IDs', [self, fID])
  else
  {$endif CPU32}
    result := pointer(PtrInt(fID));
  {$else}
  if PtrUInt(self) < $100000 then // rough estimation, but works in practice
    result := self
  else
  try
    result := pointer(PtrInt(fID));
  except
    result := self;
  end;
  {$endif OSWINDOWS}
end;

class procedure TOrm.InternalRegisterCustomProperties(Props: TOrmProperties);
begin // do nothing by default
end;

class procedure TOrm.InternalDefineModel(Props: TOrmProperties);
begin // do nothing by default
end;

function TOrm.GetHasBlob: boolean;
begin
  if self = nil then
    result := false
  else
    result := Orm.BlobFields <> nil;
end;

function TOrm.GetSimpleFieldCount: integer;
begin
  if self = nil then
    result := 0
  else
    result := length(Orm.SimpleFields);
end;

function TOrm.GetFillCurrentRow: integer;
begin
  if (self = nil) or
     (fFill = nil) then
    result := 0
  else
    result := fFill.FillCurrentRow;
end;

function TOrm.GetFillReachedEnd: boolean;
begin
  result := (self = nil) or
            (fFill = nil) or
            (fFill.Table.fRowCount = 0) or
            (cardinal(fFill.FillCurrentRow) > cardinal(fFill.Table.fRowCount));
end;

function TOrm.GetTable: TOrmTable;
begin
  if (self = nil) or
     (fFill = nil) then
    result := nil
  else
    result := fFill.Table;
end;

function TOrm.GetFieldValue(const PropName: RawUtf8): RawUtf8;
var
  P: TOrmPropInfo;
begin
  result := '';
  if self = nil then
    exit;
  P := Orm.Fields.ByName(pointer(PropName));
  if P <> nil then
    P.GetValueVar(self, False, result, nil);
end;

procedure TOrm.SetFieldValue(const PropName: RawUtf8; Value: PUtf8Char; ValueLen: PtrInt);
var
  P: TOrmPropInfo;
begin
  if self = nil then
    exit;
  P := Orm.Fields.ByName(pointer(PropName));
  if P <> nil then
    P.SetValue(self, Value, ValueLen, false);
end;

function TOrm.GetAsDocVariant(withID: boolean;
  const withFields: TFieldBits; options: PDocVariantOptions;
  replaceRowIDWithID: boolean): variant;
begin
  GetAsDocVariant(withID, withFields, result, options, replaceRowIDWithID);
end;

procedure TOrm.GetAsDocVariant(withID: boolean; const withFields: TFieldBits;
  var result: variant; options: PDocVariantOptions; ReplaceRowIDWithID: boolean);
const
  _ID: array[boolean] of RawUtf8 = ('RowID', 'ID');
var
  f, i: PtrInt;
  Fields: TOrmPropInfoList;
  intvalues: TRawUtf8Interning;
  doc: TDocVariantData absolute result;
begin
  VarClear(result);
  if self = nil then
    exit;
  Fields := Orm.Fields;
  doc.InitFast(Fields.Count + 1, dvObject);
  intvalues := nil;
  if options <> nil then
  begin // force options
    PDocVariantData(@result)^.options := options^;
    if dvoInternValues in options^ then
      intvalues := DocVariantType.InternValues;
  end;
  if withID then
  begin // use temp i to ensure FPC optimizer is not confused
    i := doc.InternalAdd(_ID[replaceRowIDWithID]);
    doc.Values[i] := fID;
  end;
  for f := 0 to Fields.Count - 1 do
    if byte(f) in withFields then
    begin
      i := doc.InternalAdd(Fields.List[f].Name);
      Fields.List[f].GetVariant(self, doc.Values[i]);
      if intvalues <> nil then // doc.Values[i] set manually -> manual interning
        intvalues.UniqueVariant(doc.Values[i]);
    end;
end;

function TOrm.GetSimpleFieldsAsDocVariant(withID: boolean;
  options: PDocVariantOptions): variant;
begin
  if self = nil then
    VarClear(result)
  else
    GetAsDocVariant(withID, Orm.SimpleFieldsBits[ooSelect], result, options);
end;

function TOrm.GetFieldVariant(const PropName: string): variant;
var
  P: TOrmPropInfo;
begin
  if self = nil then
    P := nil
  else
    P := Orm.Fields.ByRawUtf8Name(
      {$ifdef UNICODE}StringToUtf8{$endif}(PropName));
  if P = nil then
    VarClear(result)
  else
    P.GetVariant(self, result);
end;

procedure TOrm.SetFieldVariant(const PropName: string; const Source: Variant);
var
  P: TOrmPropInfo;
begin
  if self = nil then
    P := nil
  else
    P := Orm.Fields.ByRawUtf8Name(
      {$ifdef UNICODE}StringToUtf8{$endif}(PropName));
  if P <> nil then
    P.SetVariant(self, Source);
end;

function TOrm.Filter(const aFields: TFieldBits): boolean;
var
  f, i: PtrInt;
  Value, Old: RawUtf8;
begin
  result := IsZero(aFields);
  if (self = nil) or result then
    // avoid GPF and handle case if no field was selected
    exit;
  with Orm do
    if Filters = nil then
    // no filter set yet -> process OK
      result := true
    else
    begin
      for f := 0 to Fields.Count - 1 do
        if (Fields.List[f].OrmFieldType in COPIABLE_FIELDS) then
          for i := 0 to length(Filters[f]) - 1 do
            if Filters[f, i].InheritsFrom(TSynFilter) then
            begin
              Fields.List[f].GetValueVar(self, false, Value, nil);
              Old := Value;
              TSynFilter(Filters[f, i]).Process(f, Value);
              if Old <> Value then
                // value was changed -> store modified
                Fields.List[f].SetValueVar(self, Value, false);
            end;
    end;
end;

function TOrm.Filter(const aFields: array of PUtf8Char): boolean;
var
  f: TFieldBits;
begin
  if Orm.FieldBitsFrom(aFields, f) then
    // must always call the virtual Filter() method
    result := Filter(F)
  else
    result := false;
end;

class function TOrm.AutoFree(varClassPairs: array of pointer): IAutoFree;
var
  n, i: PtrInt;
begin
  n := length(varClassPairs);
  if (n = 0) or
     (n and 1 = 1) then
    exit;
  n := n shr 1;
  if n = 0 then
    exit;
  for i := 0 to n - 1 do // convert TOrmClass into TOrm instances
    varClassPairs[i * 2 + 1] := TOrmClass(varClassPairs[i * 2 + 1]).Create;
  result := TAutoFree.Create(varClassPairs);
end;

class function TOrm.AutoFree(var localVariable): IAutoFree;
begin
  result := TAutoFree.Create(localVariable, Create);
  {$ifdef ISDELPHI104}
  result.ForMethod;
  {$endif ISDELPHI104}
end;

class function TOrm.AutoFree(var localVariable; const Rest: IRestOrm;
  ID: TID): IAutoFree;
begin
  result := TAutoFree.Create(localVariable, Create(Rest, ID));
  {$ifdef ISDELPHI104}
  result.ForMethod;
  {$endif ISDELPHI104}
end;

class function TOrm.AutoFree(var localVariable; const Rest: IRestOrm;
  const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const;
  const aCustomFieldsCsv: RawUtf8): IAutoFree;
begin
  result := TAutoFree.Create(localVariable, CreateAndFillPrepare(
    Rest, FormatSqlWhere, BoundsSqlWhere, aCustomFieldsCsv));
end;

class function TOrm.AutoFree(var localVariable; const Rest: IRestOrm;
  const FormatSqlWhere: RawUtf8; const ParamsSqlWhere, BoundsSqlWhere: array of const;
  const aCustomFieldsCsv: RawUtf8): IAutoFree;
begin
  result := TAutoFree.Create(localVariable, CreateAndFillPrepare(
    Rest, FormatSqlWhere, ParamsSqlWhere, BoundsSqlWhere, aCustomFieldsCsv));
end;

class procedure TOrm.AddFilterOrValidate(const aFieldName: RawUtf8;
  aFilter: TSynFilterOrValidate);
begin
  OrmProps.AddFilterOrValidate(aFieldName, aFilter);
end;

class procedure TOrm.AddFilterNotVoidText(const aFieldNames: array of RawUtf8);
var
  i, f: PtrInt;
begin
  with OrmProps do
    for i := 0 to high(aFieldNames) do
    begin
      f := Fields.IndexByNameOrExcept(aFieldNames[i]);
      AddFilterOrValidate(f, TSynFilterTrim.Create);
      AddFilterOrValidate(f, TSynValidateNonVoidText.Create);
    end;
end;

class procedure TOrm.AddFilterNotVoidAllTextFields;
var
  f: PtrInt;
begin
  with OrmProps, Fields do
    for f := 0 to Count - 1 do
      if List[f].OrmFieldType in RAWTEXT_FIELDS then
      begin
        AddFilterOrValidate(f, TSynFilterTrim.Create);
        AddFilterOrValidate(f, TSynValidateNonVoidText.Create);
      end;
end;

function TOrm.Validate(const aRest: IRestOrm; const aFields: TFieldBits;
  aInvalidFieldIndex: PInteger; aValidator: PSynValidate): string;
var
  f, i: PtrInt;
  Value: RawUtf8;
  Validate: TSynValidate;
  valid: boolean;
begin
  result := '';
  if (self = nil) or IsZero(aFields) then
    // avoid GPF and handle case if no field was selected
    exit;
  with Orm do
    if Filters <> nil then
      for f := 0 to Fields.Count - 1 do
        if Fields.List[f].OrmFieldType in COPIABLE_FIELDS then
        begin
          for i := 0 to length(Filters[f]) - 1 do
          begin
            Validate := TSynValidate(Filters[f, i]);
            if Validate.InheritsFrom(TSynValidate) then
            begin
              if {%H-}Value = '' then
                Fields.List[f].GetValueVar(self, false, Value, nil);
              if Validate.InheritsFrom(TSynValidateRest) then
                valid := TSynValidateRest(Validate).Validate(
                  f, Value, result, aRest, self)
              else
                valid := Validate.Process(f, Value, result);
              if not valid then
              begin
                // TSynValidate process failed -> notify caller
                if aInvalidFieldIndex <> nil then
                  aInvalidFieldIndex^ := f;
                if aValidator <> nil then
                  aValidator^ := Validate;
                if result = '' then
                   // no custom message -> show a default message
                  result := format(sValidationFailed,
                    [GetCaptionFromClass(Validate.ClassType)]);
                exit;
              end;
            end;
          end;
          Value := '';
        end;
end;

function TOrm.Validate(const aRest: IRestOrm; const aFields: array of PUtf8Char;
  aInvalidFieldIndex: PInteger; aValidator: PSynValidate): string;
var
  f: TFieldBits;
begin
  if Orm.FieldBitsFrom(aFields, f) then
    // must always call the virtual Validate() method
    result := Validate(aRest, f, aInvalidFieldIndex, aValidator)
  else
    result := '';
end;

function TOrm.FilterAndValidate(const aRest: IRestOrm;
  out aErrorMessage: string; const aFields: TFieldBits;
  aValidator: PSynValidate): boolean;
var
  invalidField: integer;
begin
  Filter(aFields);
  aErrorMessage := Validate(aRest, aFields, @invalidField, aValidator);
  if aErrorMessage = '' then
    result := true
  else
  begin
    if invalidField >= 0 then
      aErrorMessage := FormatString('"%": %',
        [Orm.Fields.List[invalidField].NameDisplay, aErrorMessage]);
    result := false;
  end;
end;

function TOrm.FilterAndValidate(const aRest: IRestOrm;
  const aFields: TFieldBits; aValidator: PSynValidate): RawUtf8;
var
  msg: string;
begin
  if FilterAndValidate(aRest, msg, aFields, aValidator) then
    result := ''
  else
    StringToUtf8(msg, result);
end;

function TOrm.DynArray(const DynArrayFieldName: RawUtf8): TDynArray;
var
  f: PtrInt;
begin
  with Orm do
    for f := 0 to length(DynArrayFields) - 1 do
      with DynArrayFields[f] do
        if IdemPropNameU(Name, DynArrayFieldName) then
        begin
          GetDynArray(self, result);
          exit;
        end;
  result.Void;
end;

function TOrm.DynArray(DynArrayFieldIndex: integer): TDynArray;
var
  f: PtrInt;
begin
  if DynArrayFieldIndex > 0 then
    with Orm do
      for f := 0 to length(DynArrayFields) - 1 do
        with DynArrayFields[f] do
          if DynArrayIndex = DynArrayFieldIndex then
          begin
            GetDynArray(self, result);
            exit;
          end;
  result.Void;
end;

procedure TOrm.ComputeFieldsBeforeWrite(const aRest: IRestOrm;
  aOccasion: TOrmEvent; aServerTimeStamp: TTimeLog);
var
  f: PtrInt;
  types: TOrmFieldTypes;
  sess: TID;
  p: TOrmPropInfoRttiInt64;
begin
  if self <> nil then
    with Orm do
    begin
      integer(types) := 0;
      if oftModTime in HasTypeFields then
        include(types, oftModTime);
      if (oftCreateTime in HasTypeFields) and
         (aOccasion = oeAdd) then
        include(types, oftCreateTime);
      if integer(types) <> 0 then
      begin
        if aServerTimeStamp = 0 then
          if Assigned(aRest) then
            aServerTimeStamp := aRest.GetServerTimestamp
          else
            aServerTimeStamp := TimeLogNowUtc; // fallback to in-process time
        for f := 0 to Fields.Count - 1 do
        begin
          p := pointer(Fields.List[f]);
          if p.OrmFieldType in types then
            p.SetValueInt64(self, aServerTimeStamp);
        end;
      end;
      if (oftSessionUserID in HasTypeFields) and
         Assigned(aRest) then
      begin
        sess := aRest.GetCurrentSessionUserID;
        if sess <> 0 then
          for f := 0 to Fields.Count - 1 do
          begin
            p := pointer(Fields.List[f]);
            if p.OrmFieldType = oftSessionUserID then
              p.SetValueInt64(self, sess);
          end;
      end;
    end;
end;

{$ifndef PUREMORMOT2}
class function TOrm.RecordProps: TOrmProperties;
begin
  result := OrmProps;
end;
{$endif PUREMORMOT2}


{$ifdef HASGENERICS}

{ TRestOrmGenerics }

function TRestOrmGenerics.Generics: TRestOrmGenerics;
begin
  result := self; // circumvent limitation of non parametrized interface definition
end;

function TRestOrmGenerics.RetrieveIList<T>(const aCustomFieldsCsv: RawUtf8): IList<T>;
begin
  result := RetrieveIList<T>('', [], aCustomFieldsCsv);
end;

function TRestOrmGenerics.RetrieveIList<T>(const FormatSqlWhere: RawUtf8;
  const BoundsSqlWhere: array of const; const aCustomFieldsCsv: RawUtf8): IList<T>;
var table: TOrmTable;
begin
  result := nil;
  if self = nil then
    exit;
  table := MultiFieldValues(TOrmClass(T), aCustomFieldsCsv,
    FormatSqlWhere, BoundsSqlWhere);
  if table <> nil then
    try
      result := table.ToIList<T>;
    finally
      table.Free;
    end;
end;

{$endif HASGENERICS}

{ TRestOrmParent }

procedure TRestOrmParent.BeginCurrentThread(Sender: TThread);
begin
  // nothing do to at this level -> see e.g. TRestOrmServer.BeginCurrentThread
end;

procedure TRestOrmParent.EndCurrentThread(Sender: TThread);
begin
  // nothing do to at this level -> see e.g. TRestOrmServer.EndCurrentThread
end;


{ ------------ TOrmMany Definition }

{ TOrmMany }

constructor TOrmMany.Create;
begin
  inherited Create;
  with Orm do
    if (fRecordManySourceProp <> nil) and
       (fRecordManyDestProp <> nil) then
    begin
      fSourceID := fRecordManySourceProp.GetFieldAddr(self);
      fDestID := fRecordManyDestProp.GetFieldAddr(self);
    end;
end;

function TOrmMany.ManyAdd(const aClient: IRestOrm; aSourceID, aDestID: TID;
  NoDuplicates: boolean; aUseBatch: TRestBatch): boolean;
begin
  result := false;
  if (self = nil) or
     (aClient = nil) or
     (aSourceID = 0) or
     (aDestID = 0) or
     (fSourceID = nil) or
     (fDestID = nil) then
    exit; // invalid parameters
  if NoDuplicates and
     (InternalIDFromSourceDest(aClient, aSourceID, aDestID) <> 0) then
    exit; // this TRecordReference pair already exists
  fSourceID^ := aSourceID;
  fDestID^ := aDestID;
  if aUseBatch <> nil then
    result := aUseBatch.Add(self, true) >= 0
  else
    result := aClient.Add(self, true) <> 0;
end;

function TOrmMany.ManyAdd(const aClient: IRestOrm; aDestID: TID;
  NoDuplicates: boolean): boolean;
begin
  if (self = nil) or
     (fSourceID = nil) then
    result := false
  else // avoid GPF
    result := ManyAdd(aClient, fSourceID^, aDestID, NoDuplicates);
end;

function TOrmMany.DestGet(const aClient: IRestOrm; aSourceID: TID;
  out DestIDs: TIDDynArray): boolean;
var
  Where: RawUtf8;
begin
  Where := IDWhereSql(aClient, aSourceID, False);
  if Where = '' then
    result := False
  else
    result := aClient.OneFieldValues(RecordClass, 'Dest', Where,
      TInt64DynArray(DestIDs));
end;

function TOrmMany.DestGetJoined(const aClient: IRestOrm;
  const aDestWhereSql: RawUtf8; aSourceID: TID; out DestIDs: TIDDynArray): boolean;
var
  aTable: TOrmTable;
begin
  aTable := DestGetJoinedTable(aClient, aDestWhereSql, aSourceID, jkDestID);
  if aTable = nil then
    result := False
  else
  try
    aTable.GetRowValues(0, TInt64DynArray(DestIDs));
    result := true;
  finally
    aTable.Free;
  end;
end;

function TOrmMany.DestGetJoined(const aClient: IRestOrm;
  const aDestWhereSql: RawUtf8; aSourceID: TID): TOrm;
var
  aTable: TOrmTable;
begin
  aTable := DestGetJoinedTable(aClient, aDestWhereSql, aSourceID, jkDestFields);
  if aTable = nil then
    result := nil
  else
  begin
    result := TOrmClass(Orm.fRecordManyDestProp.ObjectClass).Create;
    aTable.OwnerMustFree := true;
    result.FillPrepare(aTable, ctnTrimmed);
  end;
end;

function TOrmMany.DestGetJoinedTable(const aClient: IRestOrm;
  const aDestWhereSql: RawUtf8; aSourceID: TID; JoinKind: TOrmManyJoinKind;
  const aCustomFieldsCsv: RawUtf8): TOrmTable;
var
  Select, SQL: RawUtf8;
  SelfProps, DestProps: TOrmModelProperties;

  procedure SelectFields(const Classes: array of TOrmModelProperties);
  var
    i: PtrInt;
  begin
    for i := 0 to high(Classes) do
    begin
      Select := Select + Classes[i].SQL.TableSimpleFields[True, True];
      if i < high(Classes) then
        Select := Select + ',';
    end;
  end;

begin
  result := nil;
  if (self = nil) or
     (fSourceID = nil) or
     (fDestID = nil) or
     (aClient = nil) then
    exit;
  if aSourceID = 0 then
    if fSourceID <> nil then
      aSourceID := fSourceID^;
  if aSourceID = 0 then
    exit;
  with aClient.Model do
  begin
    SelfProps := Props[POrmClass(self)^];
    DestProps := Props[
      TOrmClass(SelfProps.Props.fRecordManyDestProp.ObjectClass)];
  end;
  case JoinKind of
    jkDestID:
      Select := DestProps.Props.SqlTableName + '.RowID';
    jkPivotID:
      Select := SelfProps.Props.SqlTableName + '.RowID';
    jkDestFields:
      if aCustomFieldsCsv = '' then
        SelectFields([DestProps])
      else
        Select := AddPrefixToCsv(pointer(aCustomFieldsCsv),
          DestProps.Props.SqlTableName + '.');
    jkPivotFields:
      if aCustomFieldsCsv = '' then
        SelectFields([SelfProps])
      else
        Select := AddPrefixToCsv(pointer(aCustomFieldsCsv),
          SelfProps.Props.SqlTableName + '.');
    jkPivotAndDestFields:
      if aCustomFieldsCsv = '' then
        SelectFields([SelfProps, DestProps])
      else
        Select := aCustomFieldsCsv;
  end;
  if aDestWhereSql = '' then
    // fast inlined prepared statement
    SQL := 'SELECT % FROM %,% WHERE %.Source=:(%): AND %.Dest=%.RowID'
  else if PosEx(RawUtf8(':('), aDestWhereSql, 1) > 0 then
    // statement is globaly inlined -> cache prepared statement
    SQL := 'SELECT % FROM %,% WHERE %.Source=:(%): AND %.Dest=%.RowID AND %'
  else
    // statement is not globaly inlined -> no caching of prepared statement
    SQL := 'SELECT % FROM %,% WHERE %.Source=% AND %.Dest=%.RowID AND %';
  result := aClient.ExecuteList([POrmClass(self)^,
    TOrmClass(SelfProps.Props.fRecordManyDestProp.ObjectClass)],
    FormatUtf8(SQL, [{%H-}Select, DestProps.Props.SqlTableName,
      SelfProps.Props.SqlTableName, SelfProps.Props.SqlTableName,
      aSourceID, SelfProps.Props.SqlTableName,
      DestProps.Props.SqlTableName, aDestWhereSql]));
end;

function TOrmMany.DestGet(const aClient: IRestOrm;
  out DestIDs: TIDDynArray): boolean;
begin
  if fSourceID = nil then
    result := false
  else // avoid GPF
    result := DestGet(aClient, fSourceID^, DestIDs);
   // fSourceID has been set by TOrm.Create
end;

function TOrmMany.ManyDelete(const aClient: IRestOrm;
  aSourceID, aDestID: TID; aUseBatch: TRestBatch): boolean;
var
  aID: TID;
begin
  result := false;
  if (self = nil) or
     (aClient = nil) or
     (aSourceID = 0) or
     (aDestID = 0) then
    exit;
  aID := InternalIDFromSourceDest(aClient, aSourceID, aDestID);
  if aID <> 0 then
    if aUseBatch <> nil then
      result := aUseBatch.Delete(RecordClass, aID) >= 0
    else
      result := aClient.Delete(RecordClass, aID);
end;

function TOrmMany.ManyDelete(const aClient: IRestOrm; aDestID: TID): boolean;
begin
  if fSourceID = nil then
    result := false
  else // avoid GPF
    result := ManyDelete(aClient, fSourceID^, aDestID, nil);
end;

function TOrmMany.ManySelect(const aClient: IRestOrm;
  aSourceID, aDestID: TID): boolean;
begin
  if (self = nil) or
     (aClient = nil) or
     (aSourceID = 0) or
     (aDestID = 0) then
    result := false
  else // invalid parameters
    result := aClient.Retrieve(FormatUtf8('Source=:(%): AND Dest=:(%):',
      [aSourceID, aDestID]), self);
end;

function TOrmMany.ManySelect(const aClient: IRestOrm; aDestID: TID): boolean;
begin
  if (self = nil) or
     (fSourceID = nil) then
    result := false
  else // avoid GPF
    result := ManySelect(aClient, fSourceID^, aDestID);
end;

function TOrmMany.InternalFillMany(const aClient: IRestOrm; aID: TID;
  const aAndWhereSql: RawUtf8; isDest: boolean): integer;
var
  aTable: TOrmTable;
  Where: RawUtf8;
begin
  result := 0;
  if self = nil then
    exit;
  if not isDest and
     (aID = 0) then
    if fSourceID <> nil then
      aID := fSourceID^; // has been set by TOrm.Create
  Where := IDWhereSql(aClient, aID, isDest, aAndWhereSql);
  if Where = '' then
    exit;
  aTable := aClient.MultiFieldValues(RecordClass, '', Where);
  if aTable = nil then
    exit;
  aTable.OwnerMustFree := true;
  FillPrepare(aTable); // temporary storage for FillRow, FillOne and FillRewind
  result := aTable.fRowCount;
end;

function TOrmMany.IsPropClassInstance(Prop: PRttiCustomProp): boolean;
begin
  // returns TRUE for object serialization, FALSE for integer value
  if IdemPropNameU(Prop^.Name, 'source') or
     IdemPropNameU(Prop^.Name, 'dest') then
    result := false // source/dest fields are not class instances
  else
    result := fFill.JoinedFields;
end;

function TOrmMany.FillMany(const aClient: IRestOrm; aSourceID: TID;
  const aAndWhereSql: RawUtf8): integer;
begin
  result := InternalFillMany(aClient, aSourceID, aAndWhereSql, false);
end;

function TOrmMany.FillManyFromDest(const aClient: IRestOrm; aDestID: TID;
  const aAndWhereSql: RawUtf8): integer;
begin
  result := InternalFillMany(aClient, aDestID, aAndWhereSql, true);
end;

function TOrmMany.IDWhereSql(const aClient: IRestOrm; aID: TID;
  isDest: boolean; const aAndWhereSql: RawUtf8): RawUtf8;
const
  FieldName: array[boolean] of RawUtf8 = ('Source=', 'Dest=');
begin
  if (self = nil) or
     (aID = 0) or
     (fSourceID = nil) or
     (fDestID = nil) or
     (aClient = nil) then
    result := ''
  else
  begin
    if aAndWhereSql <> '' then
      if PosEx(RawUtf8(':('), aAndWhereSql, 1) > 0 then
        result := '%:(%): AND %'
      else // inlined parameters
        result := '%% AND %'
    else // no inlined parameters -> not cached
      result := '%:(%):'; // no additional where clause -> inline ID
    result := FormatUtf8(result, [FieldName[isDest], aID, aAndWhereSql]);
  end;
end;

function TOrmMany.SourceGet(const aClient: IRestOrm; aDestID: TID;
  out SourceIDs: TIDDynArray): boolean;
var
  Where: RawUtf8;
begin
  Where := IDWhereSql(aClient, aDestID, True);
  if Where = '' then
    result := false
  else
    result := aClient.OneFieldValues(RecordClass, 'Source', Where,
      TInt64DynArray(SourceIDs));
end;

function TOrmMany.InternalIDFromSourceDest(const aClient: IRestOrm;
  aSourceID, aDestID: TID): TID;
begin
  SetID(aClient.OneFieldValue(RecordClass, 'RowID', FormatUtf8(
    'Source=:(%): AND Dest=:(%):', [aSourceID, aDestID])), result{%H-});
end;


{ ------------ TOrmVirtual Definitions }

{ TOrmFts3 }

class function TOrmFts3.OptimizeFTS3Index(const Server: IRestOrmServer): boolean;
begin
  if (self = nil) or
     (Server = nil) then
    result := false
  else
    with OrmProps do
      result := Server.ExecuteFmt('INSERT INTO %(%) VALUES(''optimize'');',
        [SqlTableName, SqlTableName]);
end;


{ TOrmFts4 }

class procedure TOrmFts4.InitializeTable(const Server: IRestOrmServer;
  const FieldName: RawUtf8; Options: TOrmInitializeTableOptions);
var
  m: TOrmModel;
  p: TOrmModelProperties;
  main, fts, ftsfields, oldfields: RawUtf8;
begin
  inherited;
  if FieldName <> '' then
    exit;
  m := Server.Model;
  p := m.Props[self];
  if (p = nil) or
     (p.fFTSWithoutContentFields = '') then
    exit;
  main := m.Tables[p.fFTSWithoutContentTableIndex].SqlTableName;
  if not Server.IsInternalSQLite3Table(p.fFTSWithoutContentTableIndex) then
    raise EModelException.CreateUtf8(
      '% is an external content FTS4/5 table but source % is not ' +
      'a local SQLite3 table: FTS search will be unavailable', [self, main]);
  fts := p.Props.SqlTableName;
  ftsfields := p.Props.SqlTableSimpleFieldsNoRowID;
  // see http://www.sqlite.org/fts3.html#*fts4content
  if p.Kind = ovkFTS5 then
  begin
    // In fts 5 we can't use docid only rowid, also use insert() values('delete',) to delete record
    oldfields := StringReplaceAll(p.fFTSWithoutContentFields, 'new.', 'old.');
    Server.ExecuteFmt('CREATE TRIGGER %_bu BEFORE UPDATE ON % ' +
      'BEGIN INSERT INTO %(%,rowid,%) VALUES(''delete'',old.rowid%); END;',
      [main, main, fts, fts, ftsfields, oldfields]);
    Server.ExecuteFmt('CREATE TRIGGER %_bd BEFORE DELETE ON % ' +
      'BEGIN INSERT INTO %(%,rowid,%) VALUES(''delete'',old.rowid%); END;',
      [main, main, fts, fts, ftsfields, oldfields]);
    Server.ExecuteFmt('CREATE TRIGGER %_au AFTER UPDATE ON % ' +
      'BEGIN INSERT INTO %(rowid,%) VALUES(new.rowid%); END;',
      [main, main, fts, ftsfields, p.fFTSWithoutContentFields]);
    Server.ExecuteFmt('CREATE TRIGGER %_ai AFTER INSERT ON % ' +
      'BEGIN INSERT INTO %(rowid,%) VALUES(new.rowid%); END;',
      [main, main, fts, ftsfields, p.fFTSWithoutContentFields]);
  end
  else
  begin
    Server.ExecuteFmt('CREATE TRIGGER %_bu BEFORE UPDATE ON % ' +
      'BEGIN DELETE FROM % WHERE docid=old.rowid; END;', [main, main, fts]);
    Server.ExecuteFmt('CREATE TRIGGER %_bd BEFORE DELETE ON % ' +
      'BEGIN DELETE FROM % WHERE docid=old.rowid; END;', [main, main, fts]);
    Server.ExecuteFmt('CREATE TRIGGER %_au AFTER UPDATE ON % ' +
      'BEGIN INSERT INTO %(docid,%) VALUES(new.rowid%); END;',
      [main, main, fts, ftsfields, p.fFTSWithoutContentFields]);
    Server.ExecuteFmt('CREATE TRIGGER %_ai AFTER INSERT ON % ' +
      'BEGIN INSERT INTO %(docid,%) VALUES(new.rowid%); END;',
      [main, main, fts, ftsfields, p.fFTSWithoutContentFields]);
  end;
end;


{ TOrmRTreeAbstract }

class function TOrmRTreeAbstract.RTreeSQLFunctionName: RawUtf8;
begin
  result := OrmProps.SqlTableName + '_in';
end;

{ TOrmRTree }

class procedure TOrmRTree.BlobToCoord(const InBlob;
  var OutCoord: TOrmTreeCoords);
begin // direct memory copy with no memory check
  MoveFast(InBlob, OutCoord,
    (OrmProps.RTreeCoordBoundaryFields shr 1) * SizeOf(double));
end;

class function TOrmRTree.ContainedIn(const BlobA, BlobB): boolean;
var
  A, B: TOrmTreeCoords;
  i: PtrInt;
begin
  BlobToCoord(BlobA, A);
  BlobToCoord(BlobB, B);
  result := false;
  for i := 0 to (OrmProps.RTreeCoordBoundaryFields shr 1) - 1 do
    if (A[i].max < B[i].min) or
       (A[i].min > B[i].max) then
      exit; // no match
  result := true; // box match
end;

{ TOrmRTreeInteger }

class procedure TOrmRTreeInteger.BlobToCoord(const InBlob;
  var OutCoord: TOrmTreeCoordsInteger);
begin // direct memory copy with no memory check
  MoveFast(InBlob, OutCoord,
    (OrmProps.RTreeCoordBoundaryFields shr 1) * SizeOf(integer));
end;

class function TOrmRTreeInteger.ContainedIn(const BlobA, BlobB): boolean;
var
  A, B: TOrmTreeCoordsInteger;
  i: PtrInt;
begin
  BlobToCoord(BlobA, A);
  BlobToCoord(BlobB, B);
  result := false;
  for i := 0 to (OrmProps.RTreeCoordBoundaryFields shr 1) - 1 do
    if (A[i].max < B[i].min) or
       (A[i].min > B[i].max) then
      exit; // no match
  result := true; // box match
end;


{ ------------ TOrmProperties Definitions }

{ TOrmProperties }

procedure TOrmProperties.InternalRegisterModel(aModel: TOrmModel;
  aTableIndex: integer; aProperties: TOrmModelProperties);
var
  i: PtrInt;
begin
  //assert(aTableIndex>=0);
  EnterCriticalSection(fLock); // may be called from several threads at once
  try
    for i := 0 to fModelMax do
      if fModel[i].Model = aModel then
        exit; // already registered
    inc(fModelMax);
    if fModelMax >= length(fModel) then
      SetLength(fModel, fModelMax + 4);
    with fModel[fModelMax] do
    begin
      Model := aModel;
      Properties := aProperties;
      TableIndex := aTableIndex;
    end;
  finally
    LeaveCriticalSection(fLock);
  end;
end;

const // the most ambigous keywords - others may be used as column names
  SQLITE3_KEYWORDS = ' from where group in as ';

constructor TOrmProperties.Create(aTable: TOrmClass);
var
  i, j, nProps: PtrInt;
  nMany, nORM, nSimple, nDynArray, nBlob, nBlobCustom, nCopiableFields: integer;
  isTOrmMany: boolean;
  F: TOrmPropInfo;
  opt: TOrmPropInfoListOptions;
label
  Simple, Small, Copiabl;
begin
  inherited Create;
  if aTable = nil then
    raise EModelException.Create('TOrmProperties.Create(nil)');
  // register for JsonToObject() and for TOrmPropInfoRttiTID.Create()
  // (should have been done before in TOrmModel.Create/AddTable)
  fTableRtti := Rtti.RegisterClass(aTable) as TRttiJson;
  // initialize internal structures
  fModelMax := -1;
  fTable := aTable;
  fSqlTableName := GetDisplayNameFromClass(aTable);
  fSqlTableNameUpperWithDot := UpperCase(SqlTableName) + '.';
  isTOrmMany := aTable.InheritsFrom(TOrmMany);
  // add properties to internal Fields list
  nProps := ClassFieldCountWithParents(aTable);
  if nProps > MAX_SQLFIELDS_INCLUDINGID then
    raise EModelException.CreateUtf8('% has too many fields: %>=%',
      [Table, nProps, MAX_SQLFIELDS]);
  opt := [pilRaiseEOrmExceptionIfNotHandled];
  if aTable.InheritsFrom(TOrmRTreeAbstract) then
    include(opt, pilAuxiliaryFields);
  fFields := TOrmPropInfoList.Create(aTable, opt);
  aTable.InternalRegisterCustomProperties(self);
  if Fields.Count > MAX_SQLFIELDS_INCLUDINGID then
    raise EModelException.CreateUtf8(
      '% has too many fields after InternalRegisterCustomProperties(%): %>=%',
      [Table, self, Fields.Count, MAX_SQLFIELDS]);
  Fields.AfterAdd;
  // generate some internal lookup information
  SetLength(fJoinedFields, MAX_SQLFIELDS);
  nMany := 0;
  nSimple := 0;
  nORM := 0;
  nCopiableFields := 0;
  nDynArray := 0;
  nBlob := 0;
  nBlobCustom := 0;
  for i := 0 to Fields.Count - 1 do
  begin
    F := Fields.List[i];
    // check field name
    if IsRowID(pointer(F.Name)) then
      raise EModelException.CreateUtf8('ID is already defined in TOrm: ' +
        '%.% field name is not allowed as published property', [Table, F.Name]);
    if PosEx(' ' + LowerCase(F.Name) + ' ', SQLITE3_KEYWORDS) > 0 then
      raise EModelException.CreateUtf8(
        '%.% field name conflicts with a SQL keyword', [Table, F.Name]);
    //  handle unique fields, i.e. if marked as "stored false"
    if aIsUnique in F.Attributes then
    begin
      include(IsUniqueFieldsBits, i);
      // must trim() text value before storage, and validate for unicity
      if F.OrmFieldType in [oftUtf8Text, oftAnsiText] then
        AddFilterOrValidate(i, TSynFilterTrim.Create);
      AddFilterOrValidate(i, TSynValidateUniqueField.Create);
    end;
    // get corresponding properties content
    include(fHasTypeFields, F.OrmFieldType);
    include(FieldBits[F.OrmFieldType], i);
    case F.OrmFieldType of
      oftUnknown:
        ;
      oftUtf8Text:
        begin
          if aIsUnique in F.Attributes then
            if MainField[false] < 0 then
              MainField[false] := i;
          if MainField[true] < 0 then
            MainField[true] := i;
          goto Small;
        end;
      oftBlob:
        begin
          BlobFields[nBlob] := F as TOrmPropInfoRttiRawBlob;
          inc(nBlob);
          fSqlTableUpdateBlobFields := fSqlTableUpdateBlobFields + F.Name + '=?,';
          fSqlTableRetrieveBlobFields := fSqlTableRetrieveBlobFields + F.Name + ',';
          fSqlTableRetrieveAllFields := fSqlTableRetrieveAllFields + ',' + F.Name;
          goto Copiabl;
        end;
      oftID: // = TOrm(aID)
        if isTOrmMany and
           (IdemPropNameU(F.Name, 'Source') or
            IdemPropNameU(F.Name, 'Dest')) then
          goto Small
        else
        begin
          JoinedFields[nORM] := F as TOrmPropInfoRttiID;
          inc(nORM);
          goto Small;
        end;
      oftMany:
        begin
          ManyFields[nMany] := F as TOrmPropInfoRttiMany;
          inc(nMany);
        end;
      oftBlobDynArray:
        with F as TOrmPropInfoRttiDynArray do
        begin
          if DynArrayIndex > 0 then
            for j := 0 to nDynArray - 1 do
              if DynArrayFields[j].DynArrayIndex = DynArrayIndex then
                raise EModelException.CreateUtf8('dup index % for %.% and %.% properties',
                  [DynArrayIndex, Table, Name, Table, DynArrayFields[j].Name]);
          DynArrayFields[nDynArray] := TOrmPropInfoRttiDynArray(F);
          if TOrmPropInfoRttiDynArray(F).ObjArray <> nil then
            fDynArrayFieldsHasObjArray := true;
          inc(nDynArray);
          goto Simple; // dynarray are stored as blob, but as simple fields
        end;
      oftBlobCustom,
      oftUtf8Custom:
        begin
          BlobCustomFields[nBlobCustom] := F as TOrmPropInfoCustom;
          inc(nBlobCustom);
          goto Simple;
        end;
      oftCreateTime:
        begin
          include(ComputeBeforeAddFieldsBits, i);
          goto Small;
        end;
      oftModTime,
      oftSessionUserID:
        begin
          include(ComputeBeforeAddFieldsBits, i);
          include(ComputeBeforeUpdateFieldsBits, i);
          goto Small;
        end;
      oftRecordVersion:
        begin
          if fRecordVersionField <> nil then
            raise EModelException.CreateUtf8('%: only a single TRecordVersion ' +
              'field is allowed per class', [Table]);
          fRecordVersionField := F as TOrmPropInfoRttiRecordVersion;
          fSqlTableRetrieveAllFields := fSqlTableRetrieveAllFields + ',' + F.Name;
          goto Copiabl;
        end; // TRecordVersion is a copiable but not a simple field!
      oftVariant: // oftNullable are included in SmallfieldsBits
        goto Simple;
    else
      begin
Small:  include(SmallFieldsBits, i);
        // this code follows NOT_SIMPLE_FIELDS/COPIABLE_FIELDS constants
Simple: SimpleFields[nSimple] := F;
        inc(nSimple);
        SimpleFieldSelect[nSimple].Field := i + 1; // [0]=ID
        include(SimpleFieldsBits[ooSelect], i);
        fSqlTableSimpleFieldsNoRowID := fSqlTableSimpleFieldsNoRowID + F.Name + ',';
        fSqlTableRetrieveAllFields := fSqlTableRetrieveAllFields + ',' + F.Name;
Copiabl:include(CopiableFieldsBits, i);
        CopiableFields[nCopiableFields] := F;
        inc(nCopiableFields);
      end;
    end;
  end;
  if fSqlTableSimpleFieldsNoRowID <> '' then
    SetLength(fSqlTableSimpleFieldsNoRowID, length(fSqlTableSimpleFieldsNoRowID) - 1);
  if fSqlTableUpdateBlobFields <> '' then
    SetLength(fSqlTableUpdateBlobFields, length(fSqlTableUpdateBlobFields) - 1);
  if fSqlTableRetrieveBlobFields <> '' then
    SetLength(fSqlTableRetrieveBlobFields, length(fSqlTableRetrieveBlobFields) - 1);
  SetLength(fManyFields, nMany);
  SetLength(fSimpleFields, nSimple);
  SetLength(SimpleFieldSelect, nSimple + 1); // Select[0].Field := 0 -> ID
  SetLength(fJoinedFields, nORM);
  if nORM > 0 then
  begin
    SetLength(fJoinedFieldsTable, nORM + 1);
    fJoinedFieldsTable[0] := aTable;
    for i := 0 to nORM - 1 do
      fJoinedFieldsTable[i + 1] := TOrmClass(JoinedFields[i].ObjectClass);
  end;
  SetLength(fCopiableFields, nCopiableFields);
  SetLength(fDynArrayFields, nDynArray);
  SetLength(fBlobCustomFields, nBlobCustom);
  SetLength(fBlobFields, nBlob);
  SimpleFieldsBits[ooInsert] := SimpleFieldsBits[ooSelect];
  SimpleFieldsBits[ooUpdate] := SimpleFieldsBits[ooSelect];
  SimpleFieldsBits[ooDelete] := SimpleFieldsBits[ooSelect];
  SimpleFieldsCount[ooInsert] := nSimple;
  SimpleFieldsCount[ooUpdate] := nSimple;
  SimpleFieldsCount[ooDelete] := nSimple;
  fHasNotSimpleFields := nSimple <> Fields.Count;
  for i := 0 to Fields.Count - 1 do
    if Fields.List[i].OrmFieldType = oftCreateTime then
    begin
      exclude(SimpleFieldsBits[ooUpdate], i);
      dec(SimpleFieldsCount[ooUpdate]);
    end;
  if SmallFieldsBits <> SimpleFieldsBits[ooSelect] - FieldBits[oftVariant] -
    FieldBits[oftBlobDynArray] - FieldBits[oftBlobCustom] - FieldBits[oftUtf8Custom] then
    raise EModelException.CreateUtf8('TOrmProperties.Create(%) Bits?', [Table]);
  if isTOrmMany then
  begin
    fRecordManySourceProp := Fields.ByRawUtf8Name('Source') as TOrmPropInfoRttiInstance;
    if fRecordManySourceProp = nil then
      raise EModelException.CreateUtf8('% expects a SOURCE field', [Table])
    else
      fRecordManyDestProp := Fields.ByRawUtf8Name('Dest') as TOrmPropInfoRttiInstance;
    if fRecordManyDestProp = nil then
      raise EModelException.CreateUtf8('% expects a DEST field', [Table]);
  end;
end;

procedure TOrmProperties.SetMaxLengthValidatorForTextFields(IndexIsUtf8Length: boolean);
var
  i: PtrInt;
begin
  if self <> nil then
    for i := 0 to Fields.Count - 1 do
      with Fields.List[i] do
        if (SqlDBFieldType in TEXT_DBFIELDS) and
           (cardinal(FieldWidth - 1) < 262144) then
          AddFilterOrValidate(i, TSynValidateText.CreateUtf8('{maxLength:%,Utf8Length:%}',
            [FieldWidth, IndexIsUtf8Length], []));
end;

procedure TOrmProperties.SetMaxLengthFilterForTextFields(IndexIsUtf8Length: boolean);
var
  i: PtrInt;
begin
  if self <> nil then
    for i := 0 to Fields.Count - 1 do
      with Fields.List[i] do
        if (SqlDBFieldType in TEXT_DBFIELDS) and
           (cardinal(FieldWidth - 1) < 262144) then
          AddFilterOrValidate(i, TSynFilterTruncate.CreateUtf8('{maxLength:%,Utf8Length:%}',
            [FieldWidth, IndexIsUtf8Length], []));
end;

function TOrmProperties.AddFilterOrValidate(aFieldIndex: integer;
  aFilter: TSynFilterOrValidate): boolean;
begin
  if (self = nil) or
     (cardinal(aFieldIndex) >= cardinal(Fields.Count)) or
     (aFilter = nil) then
    result := false
  else
  begin
    if Filters = nil then
      SetLength(fFilters, Fields.Count);
    aFilter.AddOnce(Filters[aFieldIndex]);
    result := true;
  end;
end;

procedure TOrmProperties.AddFilterOrValidate(const aFieldName: RawUtf8;
  aFilter: TSynFilterOrValidate);
begin
  AddFilterOrValidate(Fields.IndexByNameOrExcept(aFieldName), aFilter);
end;

destructor TOrmProperties.Destroy;
var
  f: PtrInt;
begin
  for f := 0 to high(Filters) do
    ObjArrayClear(Filters[f]); // will free any created TSynFilter instances
  inherited Destroy;
  Fields.Free;
end;


{ ------------ TOrmModel TOrmModelProperties Definitions }

{ TOrmModel }

function TOrmModel.GetTableIndexSafe(aTable: TOrmClass;
  RaiseExceptionIfNotExisting: boolean): PtrInt;
begin
  for result := 0 to fTablesMax do // manual search: GetTableIndex() may fail
    if fTables[result] = aTable then
      exit;
  if RaiseExceptionIfNotExisting then
    raise EModelException.CreateUtf8('% must include %', [self, aTable]);
  result := -1;
end;

procedure TOrmModel.SetTableProps(aIndex: integer);
var
  j, f: PtrInt;
  t: TOrmFieldType;
  Kind: TOrmVirtualKind;
  Table, TableID: TOrmClass;
  aTableName, aFieldName: RawUtf8;
  Props: TOrmModelProperties;
  fields: TOrmPropInfoList;
  W: TJsonWriter;

  procedure RegisterTableForRecordReference(aFieldType: TOrmPropInfo;
    aFieldTable: TClass);
  var
    R: integer;
  begin
    if (aFieldTable = nil) or
       (aFieldTable = TOrm) or
       not aFieldTable.InheritsFrom(TOrm) then
      exit; // no associated table to track deletion
    R := length(fRecordReferences);
    SetLength(fRecordReferences, R + 1);
    with fRecordReferences[R] do
    begin
      TableIndex := aIndex;
      FieldType := aFieldType;
      FieldTable := pointer(aFieldTable);
      FieldTableIndex := GetTableIndexSafe(FieldTable, false);
      if FieldTableIndex < 0 then
        FieldTableIndex := -2; // allow lazy table index identification
      if aFieldType.InheritsFrom(TOrmPropInfoRttiRecordReference) then
        CascadeDelete := TOrmPropInfoRttiRecordReference(aFieldType).CascadeDelete;
    end;
  end;

begin
  if (cardinal(aIndex) > cardinal(fTablesMax)) or
     (fTableProps[aIndex] <> nil) then
    raise EModelException.Create('TOrmModel.SetTableProps');
  Table := fTables[aIndex];
  if Table.InheritsFrom(TOrmFts5) then
    Kind := ovkFTS5
  else if Table.InheritsFrom(TOrmFts4) then
    Kind := ovkFTS4
  else if Table.InheritsFrom(TOrmFts3) then
    Kind := ovkFTS3
  else if Table.InheritsFrom(TOrmVirtualTableForcedID) then
    Kind := ovkCustomForcedID
  else if Table.InheritsFrom(TOrmRTree) then
    Kind := ovkRTree
  else if Table.InheritsFrom(TOrmRTreeInteger) then
    Kind := ovkRTreeInteger
  else if Table.InheritsFrom(TOrmVirtual) then
    Kind := ovkCustomAutoID
  else
    Kind :=  ovkSQLite3;
  Props := TOrmModelProperties.Create(self, Table, Kind);
  Props.Props.InternalRegisterModel(self, aIndex, Props);
  for t := low(t) to high(t) do
    if fCustomCollationForAll[t] <> '' then
      Props.Props.SetCustomCollationForAll(t, fCustomCollationForAll[t]);
  fTableProps[aIndex] := Props;
  aTableName := Props.Props.SqlTableName;
  UpperCaseCopy(aTableName, fSortedTablesNameUpper[aIndex]);
  fSortedTablesNameIndex[aIndex] := aIndex;
  fields := Props.Props.Fields;
  for f := 0 to fields.Count - 1 do
    case fields.List[f].OrmFieldType of
      oftRecord:
        RegisterTableForRecordReference(fields.List[f], Table); // Table not used
      oftID:
        RegisterTableForRecordReference(fields.List[f],
          (fields.List[f] as TOrmPropInfoRttiInstance).ObjectClass);
      oftTID:
        begin
          TableID := (fields.List[f] as TOrmPropInfoRttiTID).RecordClass;
          if TableID = nil then // T*ID name didn't match any TOrm type
            TOrmPropInfoRttiID(fields.List[f]).fOrmFieldType := oftInteger
          else
            RegisterTableForRecordReference(fields.List[f], TableID);
        end;
      oftMany:
        GetTableIndexSafe(
          pointer((fields.List[f] as TOrmPropInfoRttiMany).ObjectClass), true);
    end;
  if Props.Props.JoinedFieldsTable <> nil then
  begin
    W := TJsonWriter.CreateOwnedStream(1024);
    try
      W.AddShorter('SELECT ');
      // JoinedFieldsTable[0] is the class itself
      with Props.Props do
      begin
        W.Add('%.RowID as `%.RowID`,', [SqlTableName, SqlTableName]);
        for f := 0 to length(SimpleFields) - 1 do
          if SimpleFields[f].OrmFieldType <> oftID then
            W.Add('%.% as `%.%`,', [SqlTableName, SimpleFields[f].Name,
              SqlTableName, SimpleFields[f].Name]);
      end;
      // add JoinedFieldsTable[1..] fields
      for j := 1 to high(Props.Props.JoinedFieldsTable) do
      begin
        aFieldName := Props.Props.JoinedFields[j - 1].Name;
        W.Add('%.RowID as `%.RowID`,', [aFieldName, aFieldName]);
        with Props.Props.JoinedFieldsTable[j].OrmProps do
          for f := 0 to High(SimpleFields) do
            if SimpleFields[f].OrmFieldType <> oftID then
              W.Add('%.% as `%.%`,', [aFieldName, SimpleFields[f].Name,
                aFieldName, SimpleFields[f].Name]);
      end;
      W.CancelLastComma;
      // add LEFT JOIN clause
      W.AddStrings([' FROM ', aTableName]);
      for j := 1 to high(Props.Props.JoinedFieldsTable) do
      begin
        aFieldName := Props.Props.JoinedFields[j - 1].Name;
        with Props.Props.JoinedFieldsTable[j].OrmProps do
          W.Add(' LEFT JOIN % AS % ON %.%=%.RowID',
            [SqlTableName, aFieldName, aTableName, aFieldName, aFieldName]);
      end;
      W.SetText(Props.SQL.SelectAllJoined);
    finally
      W.Free;
    end;
  end;
end;

function TOrmModel.GetTableProps(aClass: TOrmClass): TOrmModelProperties;
begin
  result := fTableProps[GetTableIndexExisting(aClass)];
end;

function TOrmModel.AddTable(aTable: TOrmClass; aTableIndexCreated: PInteger): boolean;
var
  n: PtrInt;
begin
  // first register for JsonToObject() and for TOrmPropInfoRttiTID.Create()
  Rtti.RegisterClass(aTable);
  // insert only once
  if GetTableIndex(aTable) >= 0 then
  begin
    result := false;
    exit;
  end;
  // add to the model list
  inc(fTablesMax);
  n := fTablesMax + 1;
  SetLength(fTables, n);
  SetLength(fSortedTablesNameUpper, n);
  SetLength(fSortedTablesNameIndex, n);
  SetLength(fTableProps, n);
  fTables[fTablesMax] := aTable;
  SetTableProps(fTablesMax);
  QuickSortRawUtf8(fSortedTablesNameUpper, fTablesMax + 1, @fSortedTablesNameIndex);
  if aTableIndexCreated <> nil then
    aTableIndexCreated^ := fTablesMax;
  result := true;
end;

function TOrmModel.AddTableInherited(aTable: TOrmClass): pointer;
var
  ndx: integer;
begin
  ndx := GetTableIndexInheritsFrom(aTable);
  if ndx < 0 then
    if not AddTable(aTable, @ndx) then
      raise EModelException.CreateUtf8('%.AddTableInherited(%)', [self, aTable]);
  result := Tables[ndx];
end;

function TOrmModel.GetTableInherited(aTable: TOrmClass): TOrmClass;
var
  ndx: integer;
begin
  ndx := GetTableIndexInheritsFrom(aTable);
  if ndx < 0 then
    result := aTable
  else
    result := Tables[ndx];
end;

constructor TOrmModel.Create(CloneFrom: TOrmModel);
var
  i: PtrInt;
begin
  if CloneFrom = nil then
    raise EModelException.CreateUtf8('%.Create(CloneFrom=nil)', [self]);
  fTables := CloneFrom.fTables;
  fTablesMax := CloneFrom.fTablesMax;
  if fTablesMax <> High(fTables) then
    raise EModelException.CreateUtf8('%.Create: incorrect CloneFrom.TableMax', [self]);
  SetRoot(CloneFrom.fRoot);
  fOwner := CloneFrom.fOwner;
  fSortedTablesNameUpper := CloneFrom.fSortedTablesNameUpper;
  fSortedTablesNameIndex := CloneFrom.fSortedTablesNameIndex;
  fRecordReferences := CloneFrom.fRecordReferences;
  fVirtualTableModule := CloneFrom.fVirtualTableModule;
  fCustomCollationForAll := CloneFrom.fCustomCollationForAll;
  SetLength(fTableProps, fTablesMax + 1);
  for i := 0 to fTablesMax do
    fTableProps[i] := TOrmModelProperties.CreateFrom(
      self, CloneFrom.fTableProps[i]);
end;

constructor TOrmModel.Create;
begin
  raise EModelException.CreateUtf8(
    'Plain %.Create is not allowed: use overloaded Create()', [self]);
end;

function TOrmModel.SafeRoot: RawUtf8;
begin
  if self = nil then
    result := ''
  else
    result := fRoot;
end;

procedure TOrmModel.SetRoot(const aRoot: RawUtf8);
var
  i: PtrInt;
begin
  for i := 1 to length(aRoot) do // allow RFC URI + '/' for URI-fragment
    if not (aRoot[i] in ['0'..'9', 'a'..'z', 'A'..'Z', '_', '-', '.', '~', ' ', '/']) then
      raise EModelException.CreateUtf8(
        '%.Root=[%] contains URI unfriendly char #% [%]',
        [self, aRoot, ord(aRoot[i]), aRoot[i]]);
  if (aRoot <> '') and
     (aRoot[length(aRoot)] = '/') then
    fRoot := copy(aRoot, 1, Length(aRoot) - 1)
  else
    fRoot := aRoot;
  UpperCaseCopy(fRoot, fRootUpper);
end;

constructor TOrmModel.Create(const Tables: array of TOrmClass;
  const aRoot: RawUtf8);
var
  N, i: PtrInt;
begin
  N := length(Tables);
  if N > SizeOf(SUPERVISOR_ACCESS_RIGHTS.Get) * 8 then // TOrmAccessRights bits
    raise EModelException.CreateUtf8('% % has too many Tables: %>%',
      [self, aRoot, N, SizeOf(SUPERVISOR_ACCESS_RIGHTS.Get) * 8]); // e.g. N>64
  // set the Tables to be associated with this Model, as TOrm classes
  fTablesMax := N - 1;
  SetLength(fTables, N);
  MoveFast(Tables[0], fTables[0], N * SizeOf(Tables[0]));
  for i := 0 to N - 1 do
    // first register for JsonToObject() and for TOrmPropInfoRttiTID.Create()
    Rtti.RegisterClass(Tables[i]);
  SetLength(fSortedTablesNameUpper, N);
  SetLength(fSortedTablesNameIndex, N);
  SetLength(fTableProps, N);
  // initialize internal properties
  for i := 0 to fTablesMax do
    SetTableProps(i);
  QuickSortRawUtf8(fSortedTablesNameUpper, fTablesMax + 1, @fSortedTablesNameIndex);
  // set the optional Root URI path of this Model
  if aRoot <> '' then
    SetRoot(aRoot);
end;

function TOrmModel.GetIsUnique(aTable: TOrmClass; aFieldIndex: integer): boolean;
var
  i: PtrInt;
begin
  i := GetTableIndex(aTable);
  if (i < 0) or
     (cardinal(aFieldIndex) >= MAX_SQLFIELDS) then
    result := false
  else
    result := aFieldIndex in TableProps[i].Props.IsUniqueFieldsBits;
end;

function TOrmModel.GetTableIndexFromSqlSelect(const SQL: RawUtf8;
  EnsureUniqueTableInFrom: boolean): integer;
var
  TableName: RawUtf8;
begin
  TableName := GetTableNameFromSqlSelect(SQL, EnsureUniqueTableInFrom);
  result := GetTableIndex(TableName);
end;

function TOrmModel.GetTablesFromSqlSelect(const SQL: RawUtf8): TOrmClassDynArray;
var
  t: TIntegerDynArray;
  n, i: PtrInt;
begin
  result := nil;
  t := GetTableIndexesFromSqlSelect(SQL);
  n := length(t);
  if n = 0 then
    exit;
  SetLength(result, n);
  for i := 0 to n - 1 do
    result[i] := Tables[t[i]];
end;

function TOrmModel.GetTableIndexesFromSqlSelect(const SQL: RawUtf8): TIntegerDynArray;
var
  TableNames: TRawUtf8DynArray;
  i, t, n, ndx: PtrInt;
begin
  result := nil;
  TableNames := GetTableNamesFromSqlSelect(SQL);
  t := length(TableNames);
  if t = 0 then
    exit;
  SetLength(result, t);
  n := 0;
  for i := 0 to t - 1 do
  begin
    ndx := GetTableIndex(TableNames[i]);
    if ndx < 0 then
      continue;
    result[n] := ndx;
    inc(n);
  end;
  if n <> t then
    SetLength(result, n);
end;

function TOrmModel.GetTable(const SqlTableName: RawUtf8): TOrmClass;
var
  i: PtrInt;
begin
  i := GetTableIndex(SqlTableName);
  if i >= 0 then
    result := Tables[i]
  else
    result := nil;
end;

function TOrmModel.GetTableExactClass(const TableName: RawUtf8): TOrmClass;
var
  i: PtrInt;
begin
  i := GetTableExactIndex(TableName);
  if i >= 0 then
    result := Tables[i]
  else
    result := nil;
end;

function TOrmModel.GetTableIndex(aTable: TOrmClass): PtrInt;
var
  i: PtrInt;
  Props: TOrmProperties;
  m: ^TOrmPropertiesModelEntry;
  c: POrmClass;
begin
  if (self <> nil) and
     (aTable <> nil) then
  begin
    Props := aTable.OrmProps;
    if (Props <> nil) and
       (Props.fModelMax < fTablesMax) then
    begin
      // fastest O(1) search in all registered models (if worth it)
      m := pointer(Props.fModel);
      for i := 0 to Props.fModelMax do
        if m^.Model = self then
        begin
          result := m^.TableIndex; // almost always loop-free
          exit;
        end
        else
          inc(m);
    end;
    // manual search e.g. if fModel[] is not yet set
    c := pointer(Tables);
    for result := 0 to fTablesMax do
      if c^ = aTable then
        exit
      else
        inc(c);
  end;
  result := -1;
end;

function TOrmModel.GetTableIndexInheritsFrom(aTable: TOrmClass): PtrInt;
begin
  if (self <> nil) and
     (aTable <> nil) and
     (aTable <> TOrm) then
    for result := 0 to fTablesMax do
      if Tables[result].InheritsFrom(aTable) then
        exit;
  result := -1;
end;

function TOrmModel.GetTableIndexExisting(aTable: TOrmClass): PtrInt;
begin
  if self = nil then
    raise EModelException.Create('nil.GetTableIndexExisting');
  result := GetTableIndex(aTable);
  if result < 0 then
    raise EModelException.CreateUtf8('% is not part of % root=%',
      [aTable, self, Root]);
end;

function TOrmModel.GetTableExactIndex(const TableName: RawUtf8): PtrInt;
var
  L: integer;
begin
  if self <> nil then
  begin
    L := length(TableName);
    for result := 0 to fTablesMax do
      if Tables[result] <> nil then // avoid GPF
        if IdemPropName(ClassNameShort(Tables[result])^, pointer(TableName), L) then
          exit;  // case insensitive search
  end;
  result := -1;
end;

function TOrmModel.GetTableIndex(const SqlTableName: RawUtf8): PtrInt;
begin // use length(SqlTableName)
  if (self <> nil) and
     (SqlTableName <> '') then
  begin
    result := FastFindUpperPUtf8CharSorted( // branchless O(log(n)) bin search
      pointer(fSortedTablesNameUpper), fTablesMax, pointer(SqlTableName), length(SqlTableName));
    if result >= 0 then
      result := fSortedTablesNameIndex[result];
  end
  else
    result := -1;
end;

function TOrmModel.GetTableIndexPtr(SqlTableName: PUtf8Char): PtrInt;
begin
  if (self <> nil) and
     (SqlTableName <> nil) then
  begin
    result := FastFindUpperPUtf8CharSorted( // branchless O(log(n)) bin search
      pointer(fSortedTablesNameUpper), fTablesMax, SqlTableName, StrLen(SqlTableName));
    if result >= 0 then
      result := fSortedTablesNameIndex[result];
  end
  else
    result := -1;
end;

function TOrmModel.GetUri(aTable: TOrmClass): RawUtf8;
begin
  result := '';
  if self = nil then
    exit;
  if aTable <> nil then
    result := aTable.OrmProps.SqlTableName
  else
  begin
    result := Root;
    exit;
  end;
  if Root <> '' then
    result := Root + '/' + result;
end;

function TOrmModel.GetUriID(aTable: TOrmClass; aID: TID): RawUtf8;
begin
  result := GetUri(aTable);
  if aID > 0 then
    result := FormatUtf8('%/%', [result, aID]);
end;

function TOrmModel.GetUriCallBack(const aMethodName: RawUtf8;
  aTable: TOrmClass; aID: TID): RawUtf8;
begin
  result := GetUriID(aTable, aID) + '/' + aMethodName;
end;

function TOrmModel.UriMatch(const Uri: RawUtf8): TRestModelMatch;
var
  UriLen: PtrInt;
begin
  result := rmNoMatch;
  if (self = nil) or
     (fRoot = '') or
     (Uri = '') then
    exit;
  if IdemPChar(pointer(Uri), pointer(fRootUpper)) then
  begin
    UriLen := length(fRoot);
    if Uri[UriLen + 1] in [#0, '/', '?'] then
      if CompareMemFixed(pointer(Uri), pointer(fRoot), UriLen) then
        result := rmMatchExact
      else
        result := rmMatchWithCaseChange;
  end;
end;

function TOrmModel.SqlFromSelectWhere(const Tables: array of TOrmClass;
  const SqlSelect, SqlWhere: RawUtf8): RawUtf8;
var
  i: PtrInt;
  aProps: array[0..31] of TOrmModelProperties;
begin
  if self = nil then
    raise EOrmException.Create('Model required');
  if high(Tables) = 0 then
  begin
    // fastest common call with one TOrmClass
    result := Props[Tables[0]].SqlFromSelectWhere(SqlSelect, SqlWhere);
    exit;
  end;
  // 'SELECT T1.F1,T1.F2,T1.F3,T2.F1,T2.F2 FROM T1,T2 WHERE ..' e.g.
  if cardinal(high(Tables)) > high(aProps) then
    raise EModelException.CreateUtf8('%.SqlFromSelectWhere() up to % Tables[]',
      [self, Length(aProps)]);
  for i := 0 to high(Tables) do
    aProps[i] := Props[Tables[i]]; // raise EModelException if not found
  if SqlSelect = '*' then
     // don't send BLOB values to query: retrieve all other fields
    if high(Tables) = 0 then
      result := 'SELECT ' + {%H-}aProps[0].SQL.TableSimpleFields[true, false]
    else
    begin
      result := 'SELECT ' + aProps[0].SQL.TableSimpleFields[true, true];
      for i := 1 to high(Tables) do
        result := result + ',' + aProps[i].SQL.TableSimpleFields[true, true];
    end
  else
    result := 'SELECT ' + SqlSelect;
  result := result + ' FROM ' + aProps[0].Props.SqlTableName;
  for i := 1 to high(Tables) do
    result := result + ',' + aProps[i].Props.SqlTableName;
  result := result + SqlFromWhere(SqlWhere);
end;

procedure TOrmModel.SetCustomCollationForAll(aFieldType: TOrmFieldType;
  const aCollationName: RawUtf8);
var
  i: PtrInt;
begin
  if self = nil then
    exit;
  if fCustomCollationForAll[aFieldType] <> '' then
    raise EModelException.CreateUtf8('%.SetCustomCollationForAll(%)' +
      ' shall be called only once', [self, aCollationName]);
  fCustomCollationForAll[aFieldType] := aCollationName;
  for i := 0 to high(fTableProps) do
    fTableProps[i].fProps.SetCustomCollationForAll(aFieldType, aCollationName);
end;

procedure TOrmModel.SetMaxLengthValidatorForAllTextFields(IndexIsUtf8Length: boolean);
var
  i: PtrInt;
begin
  if self <> nil then
    for i := 0 to high(fTableProps) do
      fTableProps[i].fProps.SetMaxLengthValidatorForTextFields(IndexIsUtf8Length);
end;

procedure TOrmModel.SetMaxLengthFilterForAllTextFields(IndexIsUtf8Length: boolean);
var
  i: PtrInt;
begin
  if self <> nil then
    for i := 0 to high(fTableProps) do
      fTableProps[i].fProps.SetMaxLengthFilterForTextFields(IndexIsUtf8Length);
end;

procedure TOrmModel.SetVariantFieldsDocVariantOptions(const Options: TDocVariantOptions);
var
  i: PtrInt;
begin
  if self <> nil then
    for i := 0 to high(fTableProps) do
      fTableProps[i].fProps.SetVariantFieldsDocVariantOptions(Options);
end;

function TOrmModel.SetIDGenerator(aTable: TOrmClass;
  aIdentifier: TSynUniqueIdentifierProcess; const aSharedObfuscationKey: RawUtf8;
  aSharedObfuscationKeyNewKdf: integer): TSynUniqueIdentifierGenerator;
var
  i: PtrInt;
begin
  i := GetTableIndexExisting(aTable);
  if i >= length(fIDGenerator) then
    SetLength(fIDGenerator, fTablesMax + 1);
  result := TSynUniqueIdentifierGenerator.Create(
    aIdentifier, aSharedObfuscationKey, aSharedObfuscationKeyNewKdf);
  fIDGenerator[i].Free;
  fIDGenerator[i] := result;
end;

function TOrmModel.GetIDGenerator(aTable: TOrmClass): TSynUniqueIdentifierGenerator;
var
  i: cardinal;
begin
  i := GetTableIndexExisting(aTable);
  if i < cardinal(length(fIDGenerator)) then
    result := fIDGenerator[i]
  else
    result := nil;
end;

function TOrmModel.NewRecord(const SqlTableName: RawUtf8): TOrm;
var
  aClass: TOrmClass;
begin
  aClass := Table[SqlTableName];
  if aClass = nil then
    result := nil
  else
    result := aClass.Create;
end;

function TOrmModel.GetSqlCreate(aTableIndex: integer): RawUtf8;
begin
  if (self = nil) or
     (cardinal(aTableIndex) > cardinal(fTablesMax)) then
    result := ''
  else
    result := Tables[aTableIndex].GetSqlCreate(self);
end;

function TOrmModel.GetSqlAddField(aTableIndex: integer; aFieldIndex: integer): RawUtf8;
begin
  if (self = nil) or
     (cardinal(aTableIndex) > cardinal(fTablesMax)) then
    result := ''
  else
    result := TableProps[aTableIndex].Props.SqlAddField(aFieldIndex);
end;

function TOrmModel.isLocked(aTable: TOrmClass; aID: TID): boolean;
begin
  result := GetLocks(aTable)^.isLocked(aID);
end;

function TOrmModel.isLocked(aRec: TOrm): boolean;
begin
  if aRec = nil then
    result := false
  else
    result := isLocked(POrmClass(aRec)^, aRec.fID);
end;

function TOrmModel.Lock(aTable: TOrmClass; aID: TID): boolean;
begin
  if self = nil then
    result := false
  else
  begin
    if fLocks = nil then
      SetLength(fLocks, fTablesMax + 1); // initialize fLocks[] if necessary
    result := GetLocks(aTable)^.Lock(aID);
  end;
end;

function TOrmModel.Lock(aTableIndex: integer; aID: TID): boolean;
begin
  if (self = nil) or
     (cardinal(aTableIndex) > cardinal(fTablesMax)) then
    result := false
  else
  begin
    if fLocks = nil then
      SetLength(fLocks, fTablesMax + 1); // initialize fLocks[] if necessary
    result := fLocks[aTableIndex].Lock(aID);
  end;
end;

function TOrmModel.Lock(aRec: TOrm): boolean;
begin
  if aRec = nil then
    result := false
  else
    result := Lock(POrmClass(aRec)^, aRec.fID);
end;

procedure TOrmModel.PurgeOlderThan(MinutesFromNow: cardinal);
var
  i: PtrInt;
begin
  if fLocks <> nil then
    for i := 0 to length(fLocks) - 1 do
      fLocks[i].PurgeOlderThan(MinutesFromNow);
end;

function TOrmModel.UnLock(aTable: TOrmClass; aID: TID): boolean;
begin
  if (self = nil) or
     (fLocks = nil) then
    result := false
  else
    result := GetLocks(aTable)^.UnLock(aID);
end;

function TOrmModel.UnLock(aTableIndex: integer; aID: TID): boolean;
begin
  if (self = nil) or
     (cardinal(aTableIndex) >= cardinal(length(fLocks))) then
    result := false
  else
    result := fLocks[aTableIndex].UnLock(aID);
end;

function TOrmModel.UnLock(aRec: TOrm): boolean;
begin
  if aRec = nil then
    result := false
  else
    result := UnLock(POrmClass(aRec)^, aRec.fID);
end;

function TOrmModel.GetLocks(aTable: TOrmClass): POrmLocks;
begin
  if (self = nil) or
     (fLocks = nil) then
    result := nil
  else
    result := @fLocks[GetTableIndexExisting(aTable)];
end;

procedure TOrmModel.UnLockAll;
var
  i: PtrInt;
begin
  for i := 0 to length(fLocks) - 1 do
    fLocks[i].Count := 0;
end;

function TOrmModel.RecordReference(Table: TOrmClass; ID: TID): TRecordReference;
begin
  if (self = nil) or
     (ID <= 0) then
    result := 0
  else
  begin
    result := GetTableIndexExisting(Table);
    if result > 63 then // TRecordReference handle up to 64=1 shl 6 tables
      result := 0
    else
      inc(result, ID shl 6);
  end;
end;

function TOrmModel.RecordReferenceTable(const Ref: TRecordReference): TOrmClass;
var
  i: PtrInt;
begin
  i := Ref and 63;
  if i <= fTablesMax then
    result := fTables[i]
  else
    result := nil;
end;

function TOrmModel.VirtualTableRegister(aClass: TOrmClass; aModule: TClass;
  const aExternalTableName: RawUtf8; aExternalDataBase: TObject;
  aMappingOptions: TOrmPropertiesMappingOptions): boolean;
var
  i: PtrInt;
begin
  result := false;
  if aClass = nil then
    exit;
  if (aModule = nil) or
     not Assigned(GetVirtualTableModuleName) or
     (GetVirtualTableModuleName(aModule) = '') then
    raise EModelException.CreateUtf8('Unexpected %.VirtualTableRegister(%,%)',
      [self, aClass, aModule]);
  i := GetTableIndexExisting(aClass);
  with TableProps[i] do
  begin
    if not (Kind in IS_CUSTOM_VIRTUAL) then
      if Kind =  ovkSQLite3 then
        SetKind(ovkCustomAutoID) // SetKind() recompute all SQL
      else
        raise EModelException.CreateUtf8('Invalid %.VirtualTableRegister(%) call: ' +
          'impossible to set class as virtual', [self, aClass]);
    ExternalDB.Init(aClass, aExternalTableName, aExternalDataBase, true, aMappingOptions);
  end;
  if high(fVirtualTableModule) <> fTablesMax then
    SetLength(fVirtualTableModule, fTablesMax + 1);
  fVirtualTableModule[i] := aModule;
  result := true;
end;

function TOrmModel.VirtualTableModule(aClass: TOrmClass): TClass;
var
  i: PtrInt;
begin
  result := nil;
  if (self = nil) or
     (fVirtualTableModule = nil) then
    exit;
  i := GetTableIndexExisting(aClass);
  if TableProps[i].Kind in IS_CUSTOM_VIRTUAL then
    result := fVirtualTableModule[i];
end;

destructor TOrmModel.Destroy;
var
  i, j: PtrInt;
begin
  for i := 0 to fTablesMax do
    with TableProps[i].Props do
    begin
      EnterCriticalSection(fLock); // may be called from several threads at once
      try
        for j := 0 to fModelMax do
          if fModel[j].Model = self then
          begin
            // un-associate this TOrm with this model
            MoveFast(fModel[j + 1], fModel[j], (fModelMax - j) * SizeOf(fModel[j]));
            dec(fModelMax);
            break;
          end;
        TableProps[i].Free;
      finally
        LeaveCriticalSection(fLock);
      end;
    end;
  ObjArrayClear(fIDGenerator);
  inherited;
end;


{ TOrmModelProperties }

constructor TOrmModelProperties.Create(aModel: TOrmModel; aTable:
  TOrmClass; aKind: TOrmVirtualKind);
var
  f: PtrInt;
begin // similar to TOrmPropertiesMapping.ComputeSql
  fModel := aModel;
  fTableIndex := fModel.GetTableIndexExisting(aTable);
  fProps := aTable.OrmProps;
  SetKind(aKind);
  with Props do
    for f := 0 to Fields.Count - 1 do
      with Fields.List[f] do
        if OrmFieldType in COPIABLE_FIELDS then
        begin // oftMany fields do not exist
          // pre-computation of SQL statements
          SQL.UpdateSetAll := SQL.UpdateSetAll + Name + '=?,';
          SQL.InsertSet := SQL.InsertSet + Name + ',';
          if byte(f) in SimpleFieldsBits[ooUpdate] then
            SQL.UpdateSetSimple := SQL.UpdateSetSimple + Name + '=?,';
          // filter + validation of unique fields, i.e. if marked as "stored false"
          if byte(f) in IsUniqueFieldsBits then
          begin
            // must trim() text value before storage, and validate for unicity
            if OrmFieldType in [oftUtf8Text, oftAnsiText] then
              AddFilterOrValidate(f, TSynFilterTrim.Create);
            // register unique field pre-validation
            AddFilterOrValidate(f, TSynValidateUniqueField.Create);
          end;
        end;
  SetLength(SQL.InsertSet, length(SQL.InsertSet) - 1);
  SetLength(SQL.UpdateSetAll, length(SQL.UpdateSetAll) - 1); // 'COL1=?,COL2=?'
  if SQL.UpdateSetSimple <> '' then
    SetLength(SQL.UpdateSetSimple, length(SQL.UpdateSetSimple) - 1); // 'COL1=?,COL2=?'
  Props.InternalRegisterModel(aModel, aModel.GetTableIndexExisting(aTable), self);
end;

constructor TOrmModelProperties.CreateFrom(aModel: TOrmModel;
  aSource: TOrmModelProperties);
begin
  inherited Create;
  fModel := aModel;
  fTableIndex := aSource.fTableIndex;
  fFTSWithoutContentTableIndex := aSource.fFTSWithoutContentTableIndex;
  fFTSWithoutContentFields := aSource.fFTSWithoutContentFields;
  fProps := aSource.fProps;
  fKind := aSource.Kind;
  SQL := aSource.SQL;
  ExternalDB := aSource.ExternalDB;
  Props.InternalRegisterModel(fModel, fModel.GetTableIndexExisting(fProps.Table), self);
end;

procedure TOrmModelProperties.SetKind(Value: TOrmVirtualKind);

  function ComputeSimpleFields(withID, withTableName: boolean): RawUtf8;
  const
    IDComma: array[TOrmVirtualKind] of rawUTF8 = ('ID,', 'RowID,',
      'RowID,', 'RowID,', 'RowID,', 'RowID,', 'RowID,', 'RowID,');
 //  SQLite3,FTS3,FTS4,FTS5,RTree,RTreeInteger,CustomForcedID,CustomAutoID
  var
    TableName: RawUtf8;
    i: PtrInt;
  begin
    if withTableName then
      TableName := Props.SqlTableName + '.'; // calc TableName once
    if withID then
      if withTableName then
        result := TableName{%H-} + IDComma[Kind]
      else
        result := IDComma[Kind]
    else
      result := '';
    for i := 0 to length(Props.SimpleFields) - 1 do
    begin
      if withTableName then
        result := result + TableName;
      result := result + Props.SimpleFields[i].Name + ','; // valid simple fields
    end;
    if result <> '' then
      SetLength(result, length(result) - 1); // trim last ','
  end;

var
  f: PtrInt;
  expected: TOrmFieldType;
begin
  case Value of // validates virtual table fields expectations
    ovkFTS3,
    ovkFTS4,
    ovkFTS5:
      begin
        if Props.Fields.Count = 0 then
          raise EModelException.CreateUtf8(
            'Virtual FTS class % should have published properties', [Props.Table]);
        for f := 0 to Props.Fields.Count - 1 do
          with Props.Fields.List[f] do
            if OrmFieldTypeStored <> oftUtf8Text then
              raise EModelException.CreateUtf8('%.%: FTS field must be RawUtf8',
                [Props.Table, Name])
      end;
    ovkRTree,
    ovkRTreeInteger:
      begin
        Props.RTreeCoordBoundaryFields := 0;
        if Value = ovkRTree then
          expected := oftFloat
        else
          expected := oftInteger;
        for f := 0 to Props.Fields.Count - 1 do
          with Props.Fields.List[f] do
            if aAuxiliaryRTreeField in Attributes then // https://sqlite.org/rtree.html#auxiliary_columns
              expected := oftUnknown // will expect further columns to be auxiliary
            else if OrmFieldTypeStored <> expected then
              raise EModelException.CreateUtf8('%.%: RTREE field must be %',
                [Props.Table, Name, ToText(expected)^])
            else
              inc(Props.RTreeCoordBoundaryFields);
        if (Props.RTreeCoordBoundaryFields < 2) or
           (Props.RTreeCoordBoundaryFields > RTREE_MAX_DIMENSION * 2) or
           (Props.RTreeCoordBoundaryFields and 1 <> 0) then
          raise EModelException.CreateUtf8(
            '% has % fields: RTREE expects 2,4,6..% boundary columns',
            [Props.Table, Props.RTreeCoordBoundaryFields, RTREE_MAX_DIMENSION * 2]);
      end;
  end;
  fKind := Value;
  // SQL.TableSimpleFields[withID: boolean; withTableName: boolean]
  SQL.TableSimpleFields[false, false] := ComputeSimpleFields(false, false);
  SQL.TableSimpleFields[false, true]  := ComputeSimpleFields(false, true);
  SQL.TableSimpleFields[true, false]  := ComputeSimpleFields(true, false);
  SQL.TableSimpleFields[true, true]   := ComputeSimpleFields(true, true);
  if Props.SqlTableSimpleFieldsNoRowID <> SQL.TableSimpleFields[false, false] then
    raise EModelException.CreateUtf8('SetKind(%)', [Props.Table]);
  SQL.SelectAllWithRowID := SqlFromSelectWhere('*', '');
  SQL.SelectAllWithID := SQL.SelectAllWithRowID;
  if IdemPChar(PUtf8Char(pointer(SQL.SelectAllWithID)) + 7, 'ROWID') then
    delete(SQL.SelectAllWithID, 8, 3); // 'SELECT RowID,..' -> 'SELECT ID,'
  SQL.SelectOneWithID := FormatUtf8('SELECT % FROM % WHERE RowID=?',
    [SQL.TableSimpleFields[true, false], Props.SqlTableName]);
end;

function TOrmModelProperties.SqlFromSelectWhere(
  const SelectFields, Where: RawUtf8): RawUtf8;
begin
  result := SqlFromSelect(Props.SqlTableName, SelectFields, Where,
    SQL.TableSimpleFields[true, false]);
end;

procedure TOrmModelProperties.FTS4WithoutContent(ContentTable: TOrmClass);
var
  i: PtrInt;
  field: RawUtf8;
begin
  if not (Kind in [ovkFTS4, ovkFTS5]) then
    raise EModelException.CreateUtf8(
      'FTS4WithoutContent: % is not a FTS4/FTS5 table', [Props.Table]);
  fFTSWithoutContentTableIndex := fModel.GetTableIndexExisting(ContentTable);
  for i := 0 to Props.Fields.Count - 1 do
  begin
    field := Props.Fields.List[i].Name;
    if ContentTable.OrmProps.Fields.IndexByName(field) < 0 then
      raise EModelException.CreateUtf8('FTS4WithoutContent: %.% is not a % field',
        [Props.Table, field, ContentTable]);
    fFTSWithoutContentFields := fFTSWithoutContentFields + ',new.' + field;
  end;
  if fFTSWithoutContentFields = '' then
    raise EModelException.CreateUtf8('FTS4WithoutContent: % has no field', [Props.Table]);
end;

function TOrmModelProperties.GetProp(const PropName: RawUtf8): TOrmPropInfo;
begin
  if self <> nil then
    result := Props.Fields.ByName(pointer(PropName))
  else
    result := nil;
end;



{ TOrmPropertiesMapping }

procedure TOrmPropertiesMapping.Init(Table: TOrmClass;
  const MappedTableName: RawUtf8; MappedConnection: TObject; AutoComputeSql: boolean;
  MappingOptions: TOrmPropertiesMappingOptions);
begin
  // set associated properties
  fProps := Table.OrmProps;
  if MappedTableName = '' then
    fTableName := fProps.SqlTableName
  else
    fTableName := MappedTableName;
  fConnectionProperties := MappedConnection;
  fOptions := MappingOptions;
  fAutoComputeSql := AutoComputeSql;
  // setup default values
  fRowIDFieldName := ID_TXT;
  fProps.Fields.NamesToRawUtf8DynArray(fExtFieldNames);
  fProps.Fields.NamesToRawUtf8DynArray(fExtFieldNamesUnQuotedSQL);
  FillcharFast(fFieldNamesMatchInternal, SizeOf(fFieldNamesMatchInternal), 255);
  fMappingVersion := 1;
  if fAutoComputeSql then
    ComputeSql;
end;

function TOrmPropertiesMapping.MapField(
  const InternalName, ExternalName: RawUtf8): POrmPropertiesMapping;
begin
  MapFields([InternalName, ExternalName]);
  result := @self;
end;

function TOrmPropertiesMapping.MapFields(
  const InternalExternalPairs: array of RawUtf8): POrmPropertiesMapping;
var
  i, f: PtrInt;
begin
  for i := 0 to (length(InternalExternalPairs) shr 1) - 1 do
  begin
    f := fProps.Fields.IndexByNameOrExcept(InternalExternalPairs[i * 2]);
    if f < 0 then
    begin
      fRowIDFieldName := InternalExternalPairs[i * 2 + 1];
      if IdemPropNameU(fRowIDFieldName, 'ID') then
        include(fFieldNamesMatchInternal, 0)
      else     // [0]=ID
        exclude(fFieldNamesMatchInternal, 0);
    end
    else
    begin
      fExtFieldNames[f] := InternalExternalPairs[i * 2 + 1];
      fExtFieldNamesUnQuotedSQL[f] := UnQuotedSQLSymbolName(fExtFieldNames[f]);
      if IdemPropNameU(fExtFieldNames[f], fProps.Fields.List[f].Name) then
        include(fFieldNamesMatchInternal, f + 1)
      else // [0]=ID  [1..n]=fields[i-1]
        exclude(fFieldNamesMatchInternal, f + 1);
    end;
  end;
  inc(fMappingVersion);
  if fAutoComputeSql then
    ComputeSql;
  result := @self;
end;

function TOrmPropertiesMapping.MapAutoKeywordFields: POrmPropertiesMapping;
begin
  if @self <> nil then
    include(fOptions, rpmAutoMapKeywordFields);
  result := @self;
end;

function TOrmPropertiesMapping.SetOptions(
  aOptions: TOrmPropertiesMappingOptions): POrmPropertiesMapping;
begin
  if @self <> nil then
    fOptions := aOptions;
  result := @self;
end;

procedure TOrmPropertiesMapping.ComputeSql;

type
  // similar to TOrmModelProperties.Create()/SetKind()
  TComputeSqlContent = (
    cTableSimpleFields, cUpdateSimple, cUpdateSetAll, cInsertAll);

  procedure SetSQL(W: TJsonWriter; withID, withTableName: boolean;
    var result: RawUtf8; content: TComputeSqlContent = cTableSimpleFields);
  var
    f: PtrInt;
  begin
    W.CancelAll;
    if withID and
       (content = cTableSimpleFields) then
    begin
      if withTableName then
        W.AddStrings([TableName, '.']);
      W.AddString(RowIDFieldName);
      if 0 in FieldNamesMatchInternal then
        W.AddComma
      else
        W.AddShorter(' as ID,');
    end;
    with fProps do
      for f := 0 to Fields.Count - 1 do
        with Fields.List[f] do
          if OrmFieldType in COPIABLE_FIELDS then // oftMany fields do not exist
            case content of
              cTableSimpleFields:
                if byte(f) in SimpleFieldsBits[ooSelect] then
                begin
                  if withTableName then
                    W.AddStrings([TableName, '.']);
                  W.AddString(ExtFieldNames[f]);
                  if not (byte(f + 1) in FieldNamesMatchInternal) then
                    // to get expected JSON column name
                    W.AddStrings([' as ', Name]);
                  W.AddComma;
                end;
              cUpdateSimple:
                if byte(f) in SimpleFieldsBits[ooSelect] then
                  W.AddStrings([ExtFieldNames[f], '=?,']);
              cUpdateSetAll:
                W.AddStrings([ExtFieldNames[f], '=?,']);
              cInsertAll:
                W.AddStrings([ExtFieldNames[f], ',']);
            end;
    W.CancelLastComma;
    W.SetText(result);
  end;

var
  W: TJsonWriter;
  temp: TTextWriterStackBuffer;
begin
  W := TJsonWriter.CreateOwnedStream(temp);
  try // SQL.TableSimpleFields[withID: boolean; withTableName: boolean]
    SetSQL(W, false, false, fSql.TableSimpleFields[false, false]);
    SetSQL(W, false, true, fSql.TableSimpleFields[false, true]);
    SetSQL(W, true, false, fSql.TableSimpleFields[true, false]);
    SetSQL(W, true, true, fSql.TableSimpleFields[true, true]);
    // SQL.SelectAll: array[withRowID: boolean]
    fSql.SelectAllWithRowID := SqlFromSelect(
      TableName, '*', '', fSql.TableSimpleFields[true, false]);
    fSql.SelectAllWithID := fSql.SelectAllWithRowID;
    SetSQL(W, false, false, fSql.UpdateSetSimple, cUpdateSimple);
    SetSQL(W, false, false, fSql.UpdateSetAll, cUpdateSetAll);
    SetSQL(W, false, false, fSql.InsertSet, cInsertAll);
  finally
    W.Free;
  end;
end;

function TOrmPropertiesMapping.InternalToExternal(
  const FieldName: RawUtf8): RawUtf8;
var
  f: PtrInt;
begin
  f := fProps.Fields.IndexByNameOrExcept(FieldName);
  if f < 0 then
    result := RowIDFieldName
  else
    result := fExtFieldNames[f];
end;

function TOrmPropertiesMapping.InternalToExternal(BlobField: PRttiProp): RawUtf8;
var
  f: PtrInt;
begin
  f := fProps.Fields.IndexByNameOrExceptShort(BlobField.Name^);
  if f < 0 then
    result := RowIDFieldName
  else
    result := fExtFieldNames[f];
end;

function TOrmPropertiesMapping.InternalCsvToExternalCsv(
  const CsvFieldNames, Sep, SepEnd: RawUtf8): RawUtf8;
var
  IntFields, ExtFields: TRawUtf8DynArray;
begin
  CsvToRawUtf8DynArray(CsvFieldNames, Sep, SepEnd, IntFields);
  InternalToExternalDynArray(IntFields, ExtFields);
  result := RawUtf8ArrayToCsv(ExtFields, Sep) + SepEnd;
end;

procedure TOrmPropertiesMapping.InternalToExternalDynArray(
  const IntFieldNames: array of RawUtf8; out result: TRawUtf8DynArray;
  IntFieldIndex: PIntegerDynArray);
var
  i, n, ndx: PtrInt;
begin
  n := length(IntFieldNames);
  SetLength(result, n);
  if IntFieldIndex <> nil then
    SetLength(IntFieldIndex^, n);
  for i := 0 to n - 1 do
  begin
    ndx := fProps.Fields.IndexByNameOrExcept(IntFieldNames[i]);
    if IntFieldIndex <> nil then
      IntFieldIndex^[i] := ndx;
    if ndx < 0 then
      result[i] := RowIDFieldName
    else
      result[i] := fExtFieldNames[ndx];
  end;
end;

function TOrmPropertiesMapping.ExternalToInternalIndex(
  const ExtFieldName: RawUtf8): integer;
begin
  if IdemPropNameU(ExtFieldName, RowIDFieldName) then
    result := -1
  else
  begin
    // search for customized field mapping
    for result := 0 to length(fExtFieldNamesUnQuotedSQL) - 1 do
      if IdemPropNameU(ExtFieldName, fExtFieldNamesUnQuotedSQL[result]) then
        exit;
    result := -2; // indicates not found
  end;
end;

function TOrmPropertiesMapping.ExternalToInternalOrNull(
  const ExtFieldName: RawUtf8): RawUtf8;
var
  i: PtrInt;
begin
  i := ExternalToInternalIndex(ExtFieldName);
  if i = -1 then
    result := ID_TXT
  else if i >= 0 then
    result := fProps.Fields.List[i].Name
  else
    result := ''; // indicates not found
end;

function TOrmPropertiesMapping.AppendFieldName(FieldIndex: integer;
  var Text: RawUtf8): boolean;
begin
  result := false; // success
  if FieldIndex = VIRTUAL_TABLE_ROWID_COLUMN then
    Text := Text + RowIDFieldName
  else if cardinal(FieldIndex) >= cardinal(Length(ExtFieldNames)) then
    // FieldIndex out of range
    result := true
  else
    Text := Text + ExtFieldNames[FieldIndex];
end;

function TOrmPropertiesMapping.AppendFieldName(FieldIndex: integer;
  WR: TJsonWriter): boolean;
begin
  result := false; // success
  if FieldIndex = VIRTUAL_TABLE_ROWID_COLUMN then
    WR.AddString(RowIDFieldName)
  else if cardinal(FieldIndex) >= cardinal(Length(ExtFieldNames)) then
    // FieldIndex out of range
    result := true
  else
    WR.AddString(ExtFieldNames[FieldIndex]);
end;

function TOrmPropertiesMapping.FieldNameByIndex(FieldIndex: integer): RawUtf8;
begin
  if FieldIndex = VIRTUAL_TABLE_ROWID_COLUMN then
    result := RowIDFieldName
  else if cardinal(FieldIndex) >= cardinal(Length(ExtFieldNames)) then
    // FieldIndex out of range
    result := ''
  else
    result := ExtFieldNames[FieldIndex];
end;


{ ------------ TRestCache Definition }

/// update/refresh the cached JSON serialization of a supplied Record
procedure CacheSetJson(var Entry: TRestCacheEntry; aRecord: TOrm);
  {$ifdef HASINLINE} inline; {$endif}
begin  // ooInsert = include all fields
  if Entry.CacheEnable then
    Entry.SetJson(aRecord.fID, aRecord.GetJsonValues(true, false, ooInsert));
end;

/// unserialize a JSON cached record of a given ID
function CacheRetrieveJson(var Entry: TRestCacheEntry; aID: TID; aValue: TOrm;
  aTag: PCardinal = nil): boolean;
  {$ifdef HASINLINE} inline; {$endif}
var
  json: RawUtf8;
begin
  if Entry.CacheEnable and
     Entry.RetrieveJson(aID, json, aTag) then
  begin
    aValue.FillFrom(json);
    aValue.fID := aID; // override RowID field (may be not present after Update)
    result := true;
  end
  else
    result := false;
end;


{ TRestCache }

constructor TRestCache.Create(const aRest: IRestOrm);
var
  i: PtrInt;
begin
  if aRest = nil then
    EOrmException.CreateUtf8('%.Create', [self]);
  pointer(fRest) := pointer(aRest); // don't change IRestOrm reference count
  fModel := aRest.Model;
  SetLength(fCache, length(fModel.Tables));
  for i := 0 to length(fCache) - 1 do
    fCache[i].Init;
end;

destructor TRestCache.Destroy;
var
  i: PtrInt;
begin
  for i := 0 to length(fCache) - 1 do
    fCache[i].Done;
  pointer(fRest) := nil; // don't change reference count
  inherited Destroy;
end;

function TRestCache.CachedEntries: cardinal;
var
  i, j: PtrInt;
begin
  result := 0;
  if self <> nil then
    for i := 0 to length(fCache) - 1 do
      with fCache[i] do
        if CacheEnable then
        begin
          EnterCriticalSection(Mutex);
          try
            for j := 0 to Count - 1 do
              if Values[j].Timestamp512 <> 0 then
                inc(result);
          finally
            LeaveCriticalSection(Mutex);
          end;
        end;
end;

function TRestCache.CachedMemory(FlushedEntriesCount: PInteger): cardinal;
var
  i: PtrInt;
begin
  result := 0;
  if FlushedEntriesCount <> nil then
    FlushedEntriesCount^ := 0;
  if self <> nil then
    for i := 0 to length(fCache) - 1 do
      inc(result, fCache[i].CachedMemory(FlushedEntriesCount));
end;

function TRestCache.SetTimeOut(aTable: TOrmClass; aTimeoutMS: cardinal): boolean;
var
  i: PtrInt;
begin
  result := false;
  if (self = nil) or
     (aTable = nil) then
    exit;
  i := fModel.GetTableIndexExisting(aTable);
  if Rest.CacheWorthItForTable(i) then
    if PtrUInt(i) < PtrUInt(Length(fCache)) then
      with fCache[i] do
      begin
        EnterCriticalSection(Mutex);
        try
          TimeOutMS := aTimeoutMS;
        finally
          LeaveCriticalSection(Mutex);
        end;
        result := true;
      end;
end;

function TRestCache.IsCached(aTable: TOrmClass): boolean;
var
  i: PtrUInt;
begin
  result := false;
  if (self = nil) or
     (aTable = nil) then
    exit;
  i := fModel.GetTableIndexExisting(aTable);
  if i < PtrUInt(Length(fCache)) then
    if fCache[i].CacheEnable then
      result := true;
end;

function TRestCache.SetCache(aTable: TOrmClass): boolean;
var
  i: PtrInt;
begin
  result := false;
  if (self = nil) or
     (aTable = nil) then
    exit;
  i := fModel.GetTableIndexExisting(aTable);
  if Rest.CacheWorthItForTable(i) then
    if PtrUInt(i) < PtrUInt(Length(fCache)) then
      with fCache[i] do
      begin
        // global cache of all records of this table
        EnterCriticalSection(Mutex);
        try
          CacheEnable := true;
          CacheAll := true;
          Value.Clear;
          result := true;
        finally
          LeaveCriticalSection(Mutex);
        end;
      end;
end;

function TRestCache.SetCache(aTable: TOrmClass; aID: TID): boolean;
var
  i: PtrInt;
begin
  result := false;
  if (self = nil) or
     (aTable = nil) or
     (aID <= 0) then
    exit;
  i := fModel.GetTableIndex(aTable);
  if PtrUInt(i) >= PtrUInt(Length(fCache)) then
    exit;
  if Rest.CacheWorthItForTable(i) then
    fCache[i].SetCache(aID);
  result := True;
end;

function TRestCache.SetCache(aTable: TOrmClass;
  const aIDs: array of TID): boolean;
var
  i: PtrUInt;
  j: PtrInt;
begin
  result := false;
  if (self = nil) or
     (aTable = nil) or
     (length(aIDs) = 0) then
    exit;
  i := fModel.GetTableIndex(aTable);
  if i >= PtrUInt(Length(fCache)) then
    exit;
  if Rest.CacheWorthItForTable(i) then
    for j := 0 to high(aIDs) do
      fCache[i].SetCache(aIDs[j]);
  result := True;
end;

function TRestCache.SetCache(aRecord: TOrm): boolean;
begin
  if (self = nil) or
     (aRecord = nil) or
     (aRecord.fID <= 0) then
    result := false
  else
    result := SetCache(POrmClass(aRecord)^, aRecord.fID);
end;

procedure TRestCache.Clear;
var
  i: PtrInt;
begin
  if self <> nil then
    for i := 0 to length(fCache) - 1 do
      fCache[i].Clear;
end;

function TRestCache.FillFromQuery(aTable: TOrmClass;
  const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const): integer;
var
  rec: TOrm;
  cache: ^TRestCacheEntry;
begin
  result := 0;
  if self = nil then
    exit;
  cache := @fCache[fModel.GetTableIndexExisting(aTable)];
  if not cache^.CacheEnable then
    exit;
  rec := aTable.CreateAndFillPrepare(fRest, FormatSqlWhere, BoundsSqlWhere);
  try
    while rec.FillOne do
    begin
      CacheSetJson(cache^, rec);
      inc(result);
    end;
  finally
    rec.Free;
  end;
end;

procedure TRestCache.Flush;
var
  i: PtrInt;
begin
  if self <> nil then
    for i := 0 to length(fCache) - 1 do
      fCache[i].FlushCacheAllEntries; // include *CriticalSection(Mutex)
end;

procedure TRestCache.Flush(aTable: TOrmClass);
begin
  if self <> nil then // includes *CriticalSection(Mutex):
    fCache[fModel.GetTableIndexExisting(aTable)].FlushCacheAllEntries;
end;

procedure TRestCache.Flush(aTable: TOrmClass; aID: TID);
begin
  if self <> nil then
    with fCache[fModel.GetTableIndexExisting(aTable)] do
      if CacheEnable then
      begin
        EnterCriticalSection(Mutex);
        try
          FlushCacheEntry(Value.Find(aID));
        finally
          LeaveCriticalSection(Mutex);
        end;
      end;
end;

procedure TRestCache.Flush(aTable: TOrmClass; const aIDs: array of TID);
var
  i: PtrInt;
begin
  if (self <> nil) and
     (length(aIDs) > 0) then
    with fCache[fModel.GetTableIndexExisting(aTable)] do
      if CacheEnable then
      begin
        EnterCriticalSection(Mutex);
        try
          for i := 0 to high(aIDs) do
            FlushCacheEntry(Value.Find(aIDs[i]));
        finally
          LeaveCriticalSection(Mutex);
        end;
      end;
end;

procedure TRestCache.Notify(aTable: TOrmClass; aID: TID;
  const aJson: RawUtf8; aAction: TOrmOccasion);
begin
  if (self <> nil) and
     (aTable <> nil) and
     (aID > 0) then
    Notify(fModel.GetTableIndex(aTable), aID, aJson, aAction);
end;

procedure TRestCache.Notify(aRecord: TOrm; aAction: TOrmOccasion);
var
  aTableIndex: cardinal;
begin
  if (self = nil) or
     (aRecord = nil) or
     (aRecord.fID <= 0) or
     not (aAction in [ooInsert, ooUpdate]) then
    exit;
  aTableIndex := fModel.GetTableIndex(POrmClass(aRecord)^);
  if aTableIndex < cardinal(Length(fCache)) then
    CacheSetJson(fCache[aTableIndex], aRecord);
end;

procedure TRestCache.Notify(aTableIndex: integer; aID: TID;
  const aJson: RawUtf8; aAction: TOrmOccasion);
begin
  if (self <> nil) and
     (aID > 0) and
     (aAction in [ooSelect, ooInsert, ooUpdate]) and
     (aJson <> '') and
     (cardinal(aTableIndex) < cardinal(Length(fCache))) then
    with fCache[aTableIndex] do
      if CacheEnable then
        SetJson(aID, aJson);
end;

procedure TRestCache.NotifyDeletion(aTableIndex: integer; aID: TID);
begin
  if (self <> nil) and
     (aID > 0) and
     (cardinal(aTableIndex) < cardinal(Length(fCache))) then
    with fCache[aTableIndex] do
      if CacheEnable then
      begin
        EnterCriticalSection(Mutex);
        try
          FlushCacheEntry(Value.Find(aID));
        finally
          LeaveCriticalSection(Mutex);
        end;
      end;
end;

procedure TRestCache.NotifyDeletions(aTableIndex: integer;
  const aIDs: array of Int64);
var
  i: PtrInt;
begin
  if (self <> nil) and
     (high(aIDs) >= 0) and
     (cardinal(aTableIndex) < cardinal(Length(fCache))) then
    with fCache[aTableIndex] do
      if CacheEnable then
      begin
        EnterCriticalSection(Mutex);
        try
          for i := 0 to high(aIDs) do
            FlushCacheEntry(Value.Find(aIDs[i]));
        finally
          LeaveCriticalSection(Mutex);
        end;
      end;
end;

procedure TRestCache.NotifyDeletion(aTable: TOrmClass; aID: TID);
begin
  if (self <> nil) and
     (aTable <> nil) and
     (aID > 0) then
    NotifyDeletion(fModel.GetTableIndex(aTable), aID);
end;

function TRestCache.Retrieve(aID: TID; aValue: TOrm): boolean;
var
  TableIndex: cardinal;
begin
  result := false;
  if (self = nil) or
     (aValue = nil) or
     (aID <= 0) then
    exit;
  TableIndex := fModel.GetTableIndexExisting(POrmClass(aValue)^);
  if TableIndex < cardinal(Length(fCache)) then
    if CacheRetrieveJson(fCache[TableIndex], aID, aValue) then
      result := true;
end;

function TRestCache.Retrieve(aTableIndex: integer; aID: TID): RawUtf8;
begin
  result := '';
  if (self <> nil) and
     (aID > 0) and
     (cardinal(aTableIndex) < cardinal(Length(fCache))) then
    with fCache[aTableIndex] do
      if CacheEnable then
        RetrieveJson(aID, result);
end;


{ ------------ TRestBatch TRestBatchLocked Definitions }

{ TRestBatch }

constructor TRestBatch.Create(const aRest: IRestOrm; aTable: TOrmClass;
  AutomaticTransactionPerRow: cardinal; Options: TRestBatchOptions;
  InternalBufferSize: cardinal; CalledWithinRest: boolean);
begin
  if aRest = nil then
    raise EOrmBatchException.CreateUtf8('%.Create(aRest=nil)', [self]);
  fRest := aRest;
  fCalledWithinRest := CalledWithinRest;
  CreateNoRest(fRest.Model, aTable, AutomaticTransactionPerRow, Options,
    InternalBufferSize);
end;

constructor TRestBatch.CreateNoRest(aModel: TOrmModel; aTable: TOrmClass;
  AutomaticTransactionPerRow: cardinal; Options: TRestBatchOptions;
  InternalBufferSize: cardinal);
begin
  fModel := aModel;
  if InternalBufferSize < 4096 then
    InternalBufferSize := 4096;
  fInternalBufferSize := InternalBufferSize;
  Reset(aTable, AutomaticTransactionPerRow, Options);
end;

destructor TRestBatch.Destroy;
begin
  FreeAndNilSafe(fBatch);
  inherited;
end;

procedure TRestBatch.Reset(aTable: TOrmClass;
  AutomaticTransactionPerRow: cardinal; Options: TRestBatchOptions);
begin
  fBatch.Free; // full reset for SetExpandedJsonWriter
  fBatch := TOrmWriter.CreateOwnedStream(fInternalBufferSize);
  fBatch.Expand := true;
  FillZero(fBatchFields);
  fBatchCount := 0;
  fAddCount := 0;
  fUpdateCount := 0;
  fDeleteCount := 0;
  fDeletedCount := 0;
  fPreviousTable := nil;
  fTable := aTable;
  if boExtendedJson in Options then
    fBatch.CustomOptions := fBatch.CustomOptions + [twoForceJsonExtended];
  if (aTable <> nil) and
     (fModel <> nil) and
     not (boOnlyObjects in fOptions) then
  begin
    fTableIndex := fModel.GetTableIndexExisting(aTable);
    fBatch.Add('{'); // sending data is '{"Table":["cmd":values,...]}'
    fBatch.AddFieldName(aTable.SqlTableName);
  end
  else
    fTableIndex := -1;
  fBatch.Add('[');
  fAutomaticTransactionPerRow := AutomaticTransactionPerRow;
  fOptions := Options;
  if boOnlyObjects in Options then
  begin
    include(fOptions, boNoModelEncoding); // we expect no JSON array
    exit;
  end;
  if AutomaticTransactionPerRow > 0 then
  begin // should be the first command
    fBatch.AddShort('"automaticTransactionPerRow",');
    fBatch.Add(AutomaticTransactionPerRow);
    fBatch.AddComma;
  end;
  Options := Options - BATCH_OPTIONS_CLIENTONLY;
  if byte(Options) <> 0 then
  begin
    fBatch.AddShort('"options",');
    fBatch.Add(byte(Options));
    fBatch.AddComma;
  end;
end;

procedure TRestBatch.Reset;
begin
  if self <> nil then
    Reset(fTable, fAutomaticTransactionPerRow, fOptions);
end;

function TRestBatch.GetCount: integer;
begin
  if self = nil then
    result := 0
  else
    result := fBatchCount;
end;

function TRestBatch.GetSizeBytes: cardinal;
begin
  if self = nil then
    result := 0
  else
    result := fBatch.TextLength;
end;

procedure TRestBatch.SetExpandedJsonWriter(Props: TOrmProperties;
  ForceResetFields, withID: boolean; const WrittenFields: TFieldBits);
begin
  if (self = nil) or
     (fBatch = nil) then
    exit;
  if not ForceResetFields then
    if fBatch.Expand and
       (fBatch.WithID = withID) and
       IsEqual(fBatchFields, WrittenFields) then
      exit; // already set -> do not compute it again
  fBatchFields := WrittenFields;
  fBatch.ChangeExpandedFields(withID,
    FieldBitsToIndex(WrittenFields, Props.Fields.Count));
  Props.SetJsonWriterColumnNames(fBatch, 0);
end;

function TRestBatch.RawAppend(FullRow: boolean): TJsonWriter;
begin
  if FullRow then
    inc(fBatchCount);
  result := fBatch;
end;

function TRestBatch.RawAdd(const SentData: RawUtf8): integer;
begin // '{"Table":[...,"POST",{object},...]}'
  if (fBatch = nil) or
     (fTable = nil) then
    raise EOrmBatchException.CreateUtf8('%.RawAdd %', [self, SentData]);
  Encode(fTable, encPost);
  fBatch.AddString(SentData);
  fBatch.AddComma;
  result := fBatchCount;
  inc(fBatchCount);
  inc(fAddCount);
end;

function TRestBatch.RawUpdate(const SentData: RawUtf8; ID: TID): integer;
var
  sentID: TID;
begin // '{"Table":[...,"PUT",{object},...]}'
  if (fBatch = nil) or
     (fTable = nil) then
    raise EOrmBatchException.CreateUtf8('%.RawUpdate % %', [self, ID, SentData]);
  Encode(fTable, encPut);
  if JsonGetID(pointer(SentData), sentID) then
    if sentID <> ID then
      raise EOrmBatchException.CreateUtf8('%.RawUpdate ID=% <> %', [self, ID, SentData])
    else
      fBatch.AddString(SentData)
  else
  begin
    fBatch.AddShorter('{ID:');
    fBatch.Add(ID);
    fBatch.AddComma;
    fBatch.AddStringCopy(SentData, 2, maxInt shr 2);
  end;
  fBatch.AddComma;
  result := fBatchCount;
  inc(fBatchCount);
  inc(fUpdateCount);
end;

const
  BATCH_VERB: array[TRestBatchEncoding] of TShort8 = (
    '"POST',    // encPost
    '"SIMPLE',  // encSimple
    '"',        // encPostHex
    '"i',       // encPostHexID
    '"PUT',     // encPut
    '"u',       // encPutHex
    '"DELETE'); // encDelete

procedure TRestBatch.Encode(EncodedTable: TOrmClass;
  Encoding: TRestBatchEncoding; Fields: PFieldBits; ID: TID);
begin
  if (fTable <> nil) and
     (EncodedTable <> fTable) then
    raise EOrmBatchException.CreateUtf8('% %" with % whereas expects %',
      [self, BATCH_VERB[Encoding], EncodedTable, fTable]);
  fPreviousTableMatch := fPreviousTable = EncodedTable;
  if (boPostNoSimpleFields in fOptions) or
     (fPreviousTable <> EncodedTable) or
     (fPreviousEncoding <> Encoding) or
     ((Fields <> nil) and
      not IsEqual(Fields^, fPreviousFields)) then
  begin // allow "POST",{obj1},{obj2} or "SIMPLE",[v1],[v2] or "DELETE",id1,id2
    fPreviousTable := EncodedTable;
    fPreviousEncoding := Encoding;
    if Fields <> nil then
      fPreviousFields := Fields^;
    if not (boOnlyObjects in fOptions) then
    begin
      fBatch.AddShorter(BATCH_VERB[Encoding]);
      if Encoding in [encPostHex, encPostHexID, encPutHexID] then
        fBatch.AddBinToHexDisplayMinChars(Fields, SizeOf(Fields^));
      if fTable <> nil then
        fBatch.AddShorter('",') // '{"Table":[...,"POST",{object},...]}'
      else
      begin
        fBatch.Add('@'); // '[...,"POST@Table",{object}',...]'
        fBatch.AddString(EncodedTable.OrmProps.SqlTableName);
        fBatch.Add('"', ',');
      end;
    end;
  end;
  if ID <> 0 then
  begin
    fBatch.Add(ID);
    fBatch.AddComma;
  end;
end;

function TRestBatch.Add(Value: TOrm; SendData, ForceID: boolean;
  const CustomFields: TFieldBits; DoNotAutoComputeFields: boolean): integer;
var
  props: TOrmProperties;
  fields: TFieldBits;
  encoding: TRestBatchEncoding;
  blob: ^TOrmPropInfoRttiRawBlob;
  f: PtrInt;
  nfo: POrmPropInfo;
begin
  result := -1;
  if (self = nil) or
     (Value = nil) or
     (fBatch = nil) then
    exit; // invalid parameters, or not opened BATCH sequence
  props := Value.Orm;
  // ensure ForceID is properly set
  if SendData and
     ((fModel = nil) or
      (fModel.Props[POrmClass(Value)^].Kind in INSERT_WITH_ID)) then
    ForceID := true; // same format as TRestClient.Add
  if ForceID and
     (Value.IDValue = 0) then
    ForceID := false;
  // compute actual fields bits
  if IsZero(CustomFields) then
    fields := props.SimpleFieldsBits[ooInsert]
  else
  begin
    fields := CustomFields * props.CopiableFieldsBits; // refine from ALL_FIELDS
    if not DoNotAutoComputeFields then
      fields := fields + props.ComputeBeforeAddFieldsBits;
  end;
  blob := pointer(props.BlobFields);
  if blob <> nil then // no need to send any null: default blob value
    for f := 1 to length(props.BlobFields) do
    begin
      if (blob^.PropertyIndex in fields) and
         blob^.IsValueVoid(Value) then
        exclude(fields, blob^.PropertyIndex);
      inc(blob);
    end;
  if not DoNotAutoComputeFields then // update TModTime/TCreateTime fields
    Value.ComputeFieldsBeforeWrite(fRest, oeAdd);
  // guess best encoding
  encoding := encPost; // versatile "POST"/"POST@table" format by default
  if SendData and
     not (boPostNoSimpleFields in fOptions) then
    if not ForceID and
       IsEqual(fields, props.SimpleFieldsBits[ooInsert]) then
      encoding := encSimple // SIMPLE",[values.. is mORMot 1 compatible
    else if not (boNoModelEncoding in fOptions) then
      if ForceID then
        encoding := encPostHexID
      else
        encoding := encPostHex;
  // append the data as JSON
  Encode(POrmClass(Value)^, encoding, @fields);
  if SendData then
  begin
    case encoding of
      encPost:
        begin
          SetExpandedJsonWriter(props, not fPreviousTableMatch, ForceID, fields);
          Value.GetJsonValues(fBatch);
        end;
      encSimple,
      encPostHex,
      encPostHexID:
        begin
          fBatch.Add('[');
          if encoding = encPostHexID then
          begin
            fBatch.Add(Value.IDValue);
            fBatch.AddComma;
          end;
          nfo := pointer(props.Fields.List);
          for f := 0 to props.Fields.Count - 1 do
          begin
            if GetBitPtr(@fields, f) then
            begin
              nfo^.GetJsonValues(Value, fBatch);
              fBatch.AddComma;
            end;
            inc(nfo);
          end;
          fBatch.CancelLastComma;
          fBatch.Add(']');
        end
    end;
    if fCalledWithinRest and ForceID then
      fRest.CacheOrNil.Notify(Value, ooInsert);
  end
  else
    fBatch.Add('{', '}'); // '{"Table":[...,"POST",{},...]}'
  fBatch.AddComma;
  result := fBatchCount;
  inc(fBatchCount);
  inc(fAddCount);
  if Assigned(fOnWrite) then
    fOnWrite(self, ooInsert, POrmClass(Value)^, Value.IDValue, Value, fields);
end;

function TRestBatch.Delete(Table: TOrmClass; ID: TID): integer;
begin
  if (self = nil) or
     (fBatch = nil) or
     (Table = nil) or
     (ID <= 0) or
     (Assigned(fRest) and
      not fRest.RecordCanBeUpdated(Table, ID, oeDelete)) then
  begin
    result := -1; // invalid parameters, or not opened BATCH sequence
    exit;
  end;
  if Assigned(fRest) then
    AddID(fDeletedRecordRef, fDeletedCount, fModel.RecordReference(Table, ID));
  Encode(Table, encDelete, nil, ID);
  result := fBatchCount;
  inc(fBatchCount);
  inc(fDeleteCount);
  if Assigned(fOnWrite) then
    fOnWrite(self, ooDelete, Table, ID, nil, []);
end;

function TRestBatch.Delete(ID: TID): integer;
begin
  if (self = nil) or
     (fTable = nil) or
     (ID <= 0) or
     (Assigned(fRest) and
      not fRest.RecordCanBeUpdated(Table, ID, oeDelete)) then
  begin
    result := -1; // invalid parameters, or not opened BATCH sequence
    exit;
  end;
  if Assigned(fRest) then
    AddID(fDeletedRecordRef, fDeletedCount, RecordReference(fTableIndex, ID));
  Encode(fTable, encDelete, nil, ID);
  result := fBatchCount;
  inc(fBatchCount);
  inc(fDeleteCount);
  if Assigned(fOnWrite) then
    fOnWrite(self, ooDelete, fTable, ID, nil, []);
end;

function TRestBatch.Update(Value: TOrm; const CustomFields: TFieldBits;
  DoNotAutoComputeFields, ForceCacheUpdate: boolean): integer;
var
  props: TOrmProperties;
  fields: TFieldBits;
  tableindex: integer;
  nfo: POrmPropInfo;
  f: PtrInt;
begin
  result := -1;
  if (Value = nil) or
     (fBatch = nil) or
     (Value.IDValue <= 0) or
     (Assigned(fRest) and
      not fRest.RecordCanBeUpdated(POrmClass(Value)^, Value.IDValue, oeUpdate)) then
    exit; // invalid parameters, or not opened BATCH sequence
  props := Value.Orm;
  // compute actual fields bits - same format as TRest.Update, BUT including ID
  if IsZero(CustomFields) then
    Value.FillContext.ComputeSetUpdatedFieldBits(props, fields)
  else
  begin
    fields := CustomFields * props.CopiableFieldsBits; // refine from ALL_FIELDS
    if not DoNotAutoComputeFields then
      fields := fields + props.FieldBits[oftModTime];
  end;
  // append the udpated fields as JSON
  if not DoNotAutoComputeFields then
    Value.ComputeFieldsBeforeWrite(fRest, oeUpdate); // compute TModTime fields
  if boNoModelEncoding in fOptions then
  begin
    // versatile "PUT"/"PUT@table":{...} format as compatibility fallback
    Encode(POrmClass(Value)^, encPut);
    SetExpandedJsonWriter(props, not fPreviousTableMatch, {withID=}true, fields);
    Value.GetJsonValues(fBatch);
  end
  else
  begin
    // new "uhex"/"uhex@table":[id,...] format
    Encode(POrmClass(Value)^, encPutHexID, @fields);
    fBatch.Add('[');
    fBatch.Add(Value.IDValue); // RowID should be the first
    fBatch.AddComma;
    nfo := pointer(props.Fields.List);
    for f := 0 to props.Fields.Count - 1 do
    begin
      if GetBitPtr(@fields, f) then
      begin
        nfo^.GetJsonValues(Value, fBatch);
        fBatch.AddComma;
      end;
      inc(nfo);
    end;
    fBatch.CancelLastComma;
    fBatch.Add(']');
  end;
  fBatch.AddComma;
  if fCalledWithinRest and
     (fields - props.SimpleFieldsBits[ooUpdate] = []) then
    ForceCacheUpdate := true; // safe to update the cache with supplied values
  if ForceCacheUpdate then
    fRest.CacheOrNil.Notify(Value, ooUpdate)
  else if Assigned(fRest) then
  begin
    // may not contain all cached fields -> delete from cache
    tableindex := fTableIndex;
    if POrmClass(Value)^ <> fTable then
      tableindex := fModel.GetTableIndexExisting(props.Table);
    AddID(fDeletedRecordRef, fDeletedCount, RecordReference(tableindex, Value.IDValue));
  end;
  result := fBatchCount;
  inc(fBatchCount);
  inc(fUpdateCount);
  if Assigned(fOnWrite) then
    fOnWrite(self, ooUpdate, POrmClass(Value)^, Value.IDValue, Value, fields);
end;

function TRestBatch.Update(Value: TOrm; const CustomCsvFields: RawUtf8;
  DoNotAutoComputeFields, ForceCacheUpdate: boolean): integer;
var
  bits: TFieldBits;
begin
  if (Value = nil) or
     (fBatch = nil) or
     not Value.Orm.FieldBitsFromCsv(CustomCsvFields, bits) then
    result := -1
  else
    result := Update(Value, bits, DoNotAutoComputeFields, ForceCacheUpdate);
end;

function TRestBatch.PrepareForSending(out Data: RawUtf8): boolean;
var
  i: PtrInt;
begin
  if (self = nil) or
     (fBatch = nil) then // no opened BATCH sequence
    result := false
  else
  begin
    if fBatchCount > 0 then
    begin // if something to send
      if Assigned(fRest) then
        for i := 0 to fDeletedCount - 1 do
          if fDeletedRecordRef[i] <> 0 then
            fRest.CacheOrNil.NotifyDeletion(
              fDeletedRecordRef[i] and 63, fDeletedRecordRef[i] shr 6);
      fBatch.CancelLastComma;
      fBatch.Add(']');
      if (fTable <> nil) and
         (fModel <> nil) and
         not (boOnlyObjects in fOptions) then
        fBatch.Add('}'); // end sequence array '{"Table":["cmd":values,...]}'
      fBatch.SetText(Data);
    end;
    result := true;
  end;
end;


{ TRestBatchLocked }

constructor TRestBatchLocked.CreateNoRest(aModel: TOrmModel; aTable: TOrmClass;
  AutomaticTransactionPerRow: cardinal; Options: TRestBatchOptions;
  InternalBufferSize: cardinal);
begin
  inherited CreateNoRest(
    aModel, aTable, AutomaticTransactionPerRow, Options, InternalBufferSize);
  fSafe.Init;
end;

destructor TRestBatchLocked.Destroy;
begin
  fSafe.Done;
  inherited Destroy;
end;

procedure TRestBatchLocked.Reset(aTable: TOrmClass;
  AutomaticTransactionPerRow: cardinal; Options: TRestBatchOptions);
begin
  inherited Reset(aTable, AutomaticTransactionPerRow, Options);
  fResetTix := GetTickCount64;
end;



{ ------------ TSynValidateRest TSynValidateUniqueField Definitions }

{ TSynValidateRest }

function TSynValidateRest.Process(aFieldIndex: integer; const Value: RawUtf8;
  var ErrorMsg: string): boolean;
begin
  result := DoValidate(aFieldIndex, Value, ErrorMsg, fProcessRest, fProcessRec);
end;

function TSynValidateRest.Validate(aFieldIndex: integer; const Value: RawUtf8;
  var ErrorMsg: string; const aProcessRest: IRestOrm; aProcessRec: TOrm): boolean;
begin
  try
    fProcessRest := aProcessRest;
    fProcessRec := aProcessRec;
    result := DoValidate(aFieldIndex, Value, ErrorMsg, aProcessRest, aProcessRec);
  finally
    fProcessRest := nil;
    fProcessRec := nil;
  end;
end;

{ TSynValidateUniqueField }

function TSynValidateUniqueField.DoValidate(aFieldIndex: integer;
  const Value: RawUtf8; var ErrorMsg: string; const aProcessRest: IRestOrm;
  aProcessRec: TOrm): boolean;
var
  aID: TID;
begin
  result := false;
  if Value = '' then
    ErrorMsg := sValidationFieldVoid
  else if (aProcessRest = nil) or
          (aProcessRec = nil) then
    result := true
  else
    with aProcessRec.Orm do
      if cardinal(aFieldIndex) >= cardinal(Fields.Count) then
        result := true
      else
      begin
        SetID(aProcessRest.OneFieldValue(Table, 'RowID',
          Fields.List[aFieldIndex].Name + '=:(' + QuotedStr(Value, '''') + '):'),
          aID{%H-});
        if (aID > 0) and
           (aID <> aProcessRec.fID) then
          ErrorMsg := sValidationFieldDuplicate
        else
          result := true;
      end;
end;


{ TSynValidateUniqueFields }

procedure TSynValidateUniqueFields.SetParameters(const Value: RawUtf8);
var
  V: array[0..0] of TValuePUtf8Char;
  tmp: TSynTempBuffer;
begin
  tmp.Init(Value);
  try
    JsonDecode(tmp.buf, ['FieldNames'], @V, True);
    CsvToRawUtf8DynArray(V[0].Text, fFieldNames);
  finally
    tmp.Done;
  end;
end;

function TSynValidateUniqueFields.DoValidate(aFieldIndex: integer;
  const Value: RawUtf8; var ErrorMsg: string; const aProcessRest: IRestOrm;
  aProcessRec: TOrm): boolean;
var
  where: RawUtf8;
  i: PtrInt;
  aID: TID;
begin
  if (aProcessRest = nil) or
     (aProcessRec = nil) or
     (fFieldNames = nil) then
    result := true
  else
  begin
    for i := 0 to high(fFieldNames) do
    begin
      if {%H-}where <> '' then
        where := where + ' AND ';
      where := where + fFieldNames[i] + '=:(' +
        QuotedStr(aProcessRec.GetFieldValue(fFieldNames[i]), '''') + '):';
    end;
    SetID(aProcessRest.OneFieldValue(POrmClass(aProcessRec)^, 'ID', where), aID{%H-});
    if (aID > 0) and
       (aID <> aProcessRec.fID) then
    begin
      ErrorMsg := sValidationFieldDuplicate;
      result := false;
    end
    else
      result := true;
  end;
end;


{ ------------ TOrmAccessRights Definition }

{ TOrmAccessRights }

procedure TOrmAccessRights.Edit(aTableIndex: integer; C, R, U, D: boolean);
begin
  if C then
    Include(POST, aTableIndex)
  else
    Exclude(POST, aTableIndex);
  if R then
    Include(GET, aTableIndex)
  else
    Exclude(GET, aTableIndex);
  if U then
    Include(PUT, aTableIndex)
  else
    Exclude(PUT, aTableIndex);
  if D then
    Include(DELETE, aTableIndex)
  else
    Exclude(DELETE, aTableIndex);
end;

procedure TOrmAccessRights.Edit(aTableIndex: integer; aRights: TOrmOccasions);
begin
  Edit(aTableIndex, ooInsert in aRights, ooSelect in aRights,
    ooUpdate in aRights, ooDelete in aRights);
end;

procedure TOrmAccessRights.Edit(aModel: TOrmModel; aTable: TOrmClass;
  C, R, U, D: boolean);
begin
  Edit(aModel.GetTableIndexExisting(aTable), C, R, U, D);
end;

procedure TOrmAccessRights.Edit(aModel: TOrmModel; aTable: TOrmClass;
  aRights: TOrmOccasions);
begin
  Edit(aModel.GetTableIndexExisting(aTable), aRights);
end;

procedure TOrmAccessRights.FromString(P: PUtf8Char);
begin
  FillCharFast(self, SizeOf(self), 0);
  if P = nil then
    exit;
  AllowRemoteExecute := TOrmAllowRemoteExecute(byte(GetNextItemCardinal(P)));
  SetBitCsv(GET, MAX_TABLES, P);
  SetBitCsv(POST, MAX_TABLES, P);
  SetBitCsv(PUT, MAX_TABLES, P);
  SetBitCsv(DELETE, MAX_TABLES, P);
end;

function TOrmAccessRights.ToString: RawUtf8;
begin
  FormatUtf8('%,%,%,%,%', [byte(AllowRemoteExecute),
    GetBitCsv(GET, MAX_TABLES), GetBitCsv(POST, MAX_TABLES),
    GetBitCsv(PUT, MAX_TABLES), GetBitCsv(DELETE, MAX_TABLES)], result);
end;


{ ************** TOrm High-Level Parents }

{ TOrmNoCase }

class procedure TOrmNoCase.InternalDefineModel(Props: TOrmProperties);
begin
  Props.SetCustomCollationForAll(oftUtf8Text, 'NOCASE');
end;

{ TOrmCaseSensitive }

class procedure TOrmCaseSensitive.InternalDefineModel(Props: TOrmProperties);
begin
  Props.SetCustomCollationForAll(oftUtf8Text, 'BINARY');
end;

{ TOrmUnicodeNoCase }

class procedure TOrmUnicodeNoCase.InternalDefineModel(Props: TOrmProperties);
begin
  Props.SetCustomCollationForAll(oftUtf8Text, 'UNICODENOCASE');
end;

{ TOrmNoCaseExtended }

class procedure TOrmNoCaseExtended.InternalDefineModel(Props: TOrmProperties);
begin
  inherited InternalDefineModel(Props); // set NOCASE collation
  Props.SetVariantFieldsDocVariantOptions(JSON_FAST_EXTENDED);
end;

{ TOrmCaseSensitiveExtended }

class procedure TOrmCaseSensitiveExtended.InternalDefineModel(
  Props: TOrmProperties);
begin
  inherited InternalDefineModel(Props); // set BINARY collation
  Props.SetVariantFieldsDocVariantOptions(JSON_FAST_EXTENDED);
end;

{ TOrmNoCaseExtendedNoID }

class procedure TOrmNoCaseExtendedNoID.InitializeTable(
  const Server: IRestOrmServer; const FieldName: RawUtf8;
  Options: TOrmInitializeTableOptions);
begin
  include(Options, itoNoIndex4TID);
  inherited InitializeTable(Server, FieldName, Options);
end;


initialization
  // some injection to mormot.orm.base
  TOrmPropInfoRttiIDClass := TOrmPropInfoRttiID;
  TOrmPropInfoRttiTIDClass := TOrmPropInfoRttiTID;
  CLASSORMFIELDTYPELIST[1] := TOrmMany;
  CLASSORMFIELDTYPELIST[2] := TOrm;
  // FPC and modern Delphi do have RTTI for array of class
  {$ifndef HASDYNARRAYTYPE}
  Rtti.RegisterObjArray(
    TypeInfo(TOrmModelPropertiesObjArray), TOrmModelProperties);
  // ensure TOrmObjArray is recognized as a T*ObjArray
  // - needed e.g. by TRestStorageInMemory.Create
  Rtti.RegisterObjArray(TypeInfo(TOrmObjArray), TOrm);
  {$endif HASDYNARRAYTYPE}


end.

