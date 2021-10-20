/// Object-Relational-Mapping (ORM) Low-Level Process
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.orm.base;

{
  *****************************************************************************

   Low-Level Basic Types and Definitions for our RESTful ORM
   - Shared ORM/JSON Fields and Values Definitions
   - ORM Ready UTF-8 Comparison Functions
   - TJsonSerializer Class for TOrm Serialization
   - TOrmPropInfo ORM / RTTI Classes
   - Abstract TOrmTableAbstract Parent Class
   - TOrmTableRowVariant Custom Variant Type
   - TOrmLocks and TRestCacheEntry Basic Structures
   - Abstract TOrmPropertiesAbstract Parent Class

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
  mormot.core.json,
  mormot.core.threads,
  mormot.core.perf,
  mormot.core.search,
  mormot.core.zip,     // for ODS export
  mormot.crypt.secure, // for TSynUniqueIdentifierBits
  mormot.db.core;
  

{ ************ Shared ORM/JSON Fields and Values Definitions }

const
  /// maximum number of Tables in a Database Model
  // - this constant is used internally to optimize memory usage in the
  // generated asm code
  // - you should not change it to a value lower than expected in an existing
  // database (e.g. as expected by TOrmAccessRights or such)
  MAX_TABLES = 256;

  /// after how many parameters inlining is not worth it
  INLINED_MAX = 10;

  /// the used TAuthSession.IDCardinal value if the session not started yet
  // - i.e. if the session handling is still in its handshaking phase
  CONST_AUTHENTICATION_SESSION_NOT_STARTED = 0;

  /// the used TAuthSession.IDCardinal value if authentication mode is not set
  // - i.e. if TRest.HandleAuthentication equals FALSE
  CONST_AUTHENTICATION_NOT_USED = 1;

  /// maximum handled dimension for TOrmRTree
  // - this value is the one used by SQLite3 R-Tree virtual table
  RTREE_MAX_DIMENSION = 5;


type
  /// generic parent class of all custom Exception types of this unit
  EOrmException = class(ESynException);

  /// used to store bit set for all available Tables in a Database Model
  TOrmTableBits = set of 0..MAX_TABLES - 1;

  /// a reference to another record in any table in the database Model
  // - stored as a 64-bit signed integer (just like the TID type)
  // - type cast any value of TRecordReference with the RecordRef object below
  // for easy access to its content
  // - use TRest.Retrieve(Reference) to get a record value
  // - don't change associated TOrmModel tables order, since TRecordReference
  // depends on it to store the Table type in its highest bits
  // - when the pointed record will be deleted, this property will be set to 0
  // by TRestOrmServer.AfterDeleteForceCoherency()
  // - could be defined as value in a TOrm property as such:
  // ! property AnotherRecord: TRecordReference read fAnotherRecord write fAnotherRecord;
  TRecordReference = type Int64;

  /// a reference to another record in any table in the database Model
  // - stored as a 64-bit signed integer (just like the TID type)
  // - type cast any value of TRecordReference with the RecordRef object below
  // for easy access to its content
  // - use TRest.Retrieve(Reference) to get a record value
  // - don't change associated TOrmModel tables order, since TRecordReference
  // depends on it to store the Table type in its highest bits
  // - when the pointed record will be deleted, any record containg a matching
  // property will be deleted by TRestOrmServer.AfterDeleteForceCoherency()
  // - could be defined as value in a TOrm property as such:
  // ! property AnotherRecord: TRecordReferenceToBeDeleted
  // !   read fAnotherRecord write fAnotherRecord;
  TRecordReferenceToBeDeleted = type TRecordReference;

  /// an Int64-encoded date and time of the latest update of a record
  // - can be used as published property field in TOrm for oftModTime:
  // if any such property is defined in the table, it will be auto-filled with
  // the server timestamp corresponding to the latest record update
  // - use internally for computation an abstract "year" of 16 months of 32 days
  // of 32 hours of 64 minutes of 64 seconds - faster than TDateTime
  // - use TimeLogFromDateTime/TimeLogToDateTime/TimeLogNow/Iso8601ToTimeLog
  // functions, or type-cast the value with a TTimeLogBits memory structure for
  // direct access to its bit-oriented content (or via PTimeLogBits pointer)
  // - could be defined as value in a TOrm property as such:
  // ! property LastModif: TModTime read fLastModif write fLastModif;
  TModTime = type TTimeLog;

  /// an Int64-encoded date and time of the record creation
  // - can be used as published property field in TOrm for oftCreateTime:
  // if any such property is defined in the table, it will be auto-filled with
  // the server timestamp corresponding to the record creation
  // - use internally for computation an abstract "year" of 16 months of 32 days
  // of 32 hours of 64 minutes of 64 seconds - faster than TDateTime
  // - use TimeLogFromDateTime/TimeLogToDateTime/TimeLogNow/Iso8601ToTimeLog
  // functions, or type-cast the value with a TTimeLogBits memory structure for
  // direct access to its bit-oriented content (or via PTimeLogBits pointer)
  // - could be defined as value in a TOrm property as such:
  // ! property CreatedAt: TModTime read fCreatedAt write fCreatedAt;
  TCreateTime = type TTimeLog;

  /// the Int64/TID of the TAuthUser currently logged
  // - can be used as published property field in TOrm for oftSessionUserID:
  // if any such property is defined in the table, it will be auto-filled with
  // the current TAuthUser.ID value at update, or 0 if no session is running
  // - could be defined as value in a TOrm property as such:
  // ! property User: TSessionUserID read fUser write fUser;
  TSessionUserID = type TID;

  /// a monotonic version number, used to track changes on a table
  // - add such a published field to any TOrm will allow tracking of
  // record modifications - note that only a single field of this type should
  // be defined for a given record
  // - note that this published field is NOT part of the record "simple fields":
  // by default, the version won't be retrieved from the DB, nor will be sent
  // from a client - the Engine*() CRUD method will take care of computing the
  // monotonic version number, just before storage to the persistence engine
  // - such a field will use a separated TOrmTableDeletion table to
  // track the deleted items
  // - could be defined as value in a TOrm property as such:
  // ! property TrackedVersion: TRecordVersion read fVersion write fVersion;
  TRecordVersion = type Int64;

  /// the available types for any SQL field property, as managed with the
  // database driver
  // - oftUnknown: unknown or not defined field type
  // - oftAnsiText: a WinAnsi encoded TEXT, forcing a NOCASE collation
  // (TOrm Delphi property was declared as AnsiString or string before
  // Delphi 2009)
  // - oftUtf8Text is UTF-8 encoded TEXT, forcing a SYSTEMNOCASE collation,
  // i.e. using Utf8IComp() (TOrm property was declared as RawUtf8,
  // RawUnicode or WideString - or string in Delphi 2009+) - you may inherit
  // from TOrmNoCase to use the NOCASE standard SQLite3 collation
  //- oftEnumerate is an INTEGER value corresponding to an index in any
  // enumerate Delphi type; storage is an INTEGER value (fast, easy and size
  // efficient); at display, this integer index will be converted into the
  // left-trimed lowercased chars of the enumerated type text conversion:
  // TOpenType(1) = otDone -> 'Done'
  /// - oftSet is an INTEGER value corresponding to a bitmapped set of
  // enumeration; storage is an INTEGER value (fast, easy and size efficient);
  // displayed as an integer by default, sets with an enumeration type with
  // up to 64 elements is allowed yet (stored as an Int64)
  // - oftInteger is an INTEGER (Int64 precision, as expected by SQLite3) field
  // - oftID is an INTEGER field pointing to the ID/RowID of another record of
  // a table, defined by the class type of the TOrm inherited property;
  // coherency is always ensured: after a delete, all values pointing to
  // it is reset to 0
  // - oftRecord is an INTEGER field pointing to the ID/RowID of another
  // record: TRecordReference=Int64 Delphi property which can be typecasted to
  // RecordRef; coherency is always ensured: after a delete, all values
  // pointing to it are reset to 0 by the ORM
  // - oftBoolean is an INTEGER field for a boolean value: 0 is FALSE,
  // anything else TRUE (encoded as JSON 'true' or 'false' constants)
  // - oftFloat is a FLOAT (floating point double precision, cf. SQLite3)
  // field, defined as double (or single) published properties definition
  // - oftDateTime is a ISO 8601 encoded (SQLite3 compatible) TEXT field,
  // corresponding to a TDateTime Delphi property: a ISO8601 collation is
  // forced for such column, for proper date/time sorting and searching
  // - oftDateTimeMS is a ISO 8601 encoded (SQLite3 compatible) TEXT field,
  // corresponding to a TDateTimeMS Delphi property, i.e. a TDateTime with
  // millisecond resolution, serialized with '.sss' suffix: a ISO8601 collation
  // is forced for such column, for proper date/time sorting and searching
  // - oftTimeLog is an INTEGER field for coding a date and time (not SQLite3
  // compatible), which should be defined as TTimeLog=Int64 Delphi property,
  // ready to be typecasted to the TTimeLogBits optimized type for efficient
  // timestamp storage, with a second resolution
  // - oftCurrency is a FLOAT containing a 4 decimals floating point value,
  // compatible with the Currency Delphi type, which minimizes rounding errors
  // in monetary calculations which may occur with oftFloat type
  // - oftObject is a TEXT containing an ObjectToJson serialization, able to
  // handle published properties of any not TPersistent as JSON object,
  // TStrings or TRawUtf8List as JSON arrays of strings, TCollection or
  // TObjectList as JSON arrays of JSON objects
  // - oftVariant is a TEXT containing a variant value encoded as JSON:
  // string values are stored between quotes, numerical values directly stored,
  // and JSON objects or arrays will be handled as TDocVariant custom types
  // - oftNullable is a INTEGER/DOUBLE/TEXT field containing a NULLable value,
  // stored as a local variant property, identifying TNullableInteger,
  // TNullableBoolean, TNullableFloat, TNullableCurrency,
  // TNullableDateTime, TNullableTimeLog and TNullableUtf8Text types
  // - oftBlob is a BLOB field (RawBlob Delphi property), and won't be
  // retrieved by default (not part of ORM "simple types"), to save bandwidth
  // - oftBlobDynArray is a dynamic array, stored as BLOB field: this kind of
  // property will be retrieved by default, i.e. is recognized as a "simple
  // field", and will use Base64 encoding during JSON transmission, or a true
  // JSON array, depending on the database back-end (e.g. MongoDB)
  // - oftBlobCustom is a custom property, stored as BLOB field: such
  // properties are defined by adding a TOrmPropInfoCustom instance, overriding
  // TOrm.InternalRegisterCustomProperties virtual method - they will
  // be retrieved by default, i.e. recognized as "simple fields"
  // - oftUtf8Custom is a custom property, stored as JSON in a TEXT field,
  // defined by overriding TOrm.InternalRegisterCustomProperties
  // virtual method, and adding a TOrmPropInfoCustom instance, e.g. via
  // RegisterCustomPropertyFromTypeName() or RegisterCustomPropertyFromRtti();
  // they will be retrieved by default, i.e. recognized as "simple fields"
  // - oftMany is a 'many to many' field (TOrmMany Delphi property);
  // nothing is stored in the table row, but in a separate pivot table: so
  // there is nothing to retrieve here; in contrast to other TOrm
  // published properties, which contains an INTEGER ID, the TOrm.Create
  // will instanciate a true TOrmMany instance to handle this pivot table
  // via its dedicated ManyAdd/FillMany/ManySelect methods - as a result, such
  // properties won't be retrieved by default, i.e. not recognized as "simple
  // fields" unless you used the dedicated methods
  // - oftModTime is an INTEGER field containing the TModTime value, aka time
  // of the record latest update; TModTime (just like TTimeLog or TCreateTime)
  // published property can be typecasted to the TTimeLogBits memory structure;
  // the value of this field is automatically updated with the current
  // date and time each time a record is updated (with external DB, it will
  // use the Server time, as retrieved by TSqlDBConnection.ServerTimestamp
  // from mormot.db.sql.pas) - see ComputeFieldsBeforeWrite
  // virtual method of TOrm; note also that only RESTful PUT/POST access
  // will change this field value: manual SQL statements (like
  // 'UPDATE Table SET Column=0') won't change its content; note also that
  // this is automated on Delphi client side, so only within TOrm ORM use
  // (a pure AJAX application should fill such fields explicitly before sending)
  // - oftCreateTime is an INTEGER field containing the TCreateTime time
  // of the record creation; TCreateTime (just like TTimeLog or TModTime)
  // published property can be typecasted to the TTimeLogBits memory structure;
  // the value of this field is automatically updated with the current
  // date and time when the record is created (with external DB, it will
  // use the Server time, as retrieved by TSqlDBConnection.ServerTimestamp
  // from mormot.db.sql.pas) - see ComputeFieldsBeforeWrite
  // virtual method of TOrm; note also that only RESTful PUT/POST access
  // will set this field value: manual SQL statements (like
  // 'INSERT INTO Table ...') won't set its content; note also that this is
  // automated on Delphi client side, so only within TOrm ORM use (a
  // pure AJAX application should fill such fields explicitly before sending)
  // - oftTID is an INTEGER field containing a TID pointing to another record;
  // since regular TOrm published properties (i.e. oftID kind of field)
  // can not be greater than 2,147,483,647 (i.e. a signed 32-bit value) under
  // Win32, defining TID published properties will allow to store the ID
  // as signed 64-bit, e.g. up to 9,223,372,036,854,775,808; despite to
  // oftID kind of record, coherency is NOT ensured: after a deletion, all
  // values pointing to are NOT reset to 0 - it is up to your business logic
  // to ensure data coherency as expected
  // - oftRecordVersion is an INTEGER field containing a TRecordVersion
  // monotonic number: adding such a published field to any TOrm will
  // allow tracking of record modifications, at storage level; by design,
  // such a field won't be part of "simple types", so won't be transmitted
  // between the clients and the server, but will be updated at any write
  // operation by the low-level Engine*() storage methods - such a field
  // will use a TOrmTableDeletion table to track the deleted items
  // - oftSessionUserID is an INTEGER field containing the TAuthUser.ID
  // of the record modification; the value of this field is automatically
  // updated with the current User ID of the active session; note also that
  // only RESTful PUT/POST access will change this field value: manual SQL
  // statements (like 'UPDATE Table SET Column=0') won't change its content;
  // this is automated on Delphi client side, so only within TOrm ORM use
  // (a pure AJAX application should fill such fields explicitly before sending)
  // - oftUnixTime is an INTEGER field for coding a date and time as second-based
  // Unix Time (SQLite3 compatible), which should be defined as TUnixTime=Int64
  // TOrm property
  // - oftUnixMSTime is an INTEGER field for coding a date and time as
  // millisecond-based Unix Time (JavaScript compatible), which should be
  // defined as TUnixMSTime=Int64 TOrm property
  // - WARNING: do not change the order of items below, otherwise some methods
  // (like TOrmProperties.CheckBinaryHeader) may be broken and fail
  TOrmFieldType = (
    oftUnknown,
    oftAnsiText,
    oftUtf8Text,
    oftEnumerate,
    oftSet,
    oftInteger,
    oftID,
    oftRecord,
    oftBoolean,
    oftFloat,
    oftDateTime,
    oftTimeLog,
    oftCurrency,
    oftObject,
    oftVariant,
    oftNullable,
    oftBlob,
    oftBlobDynArray,
    oftBlobCustom,
    oftUtf8Custom,
    oftMany,
    oftModTime,
    oftCreateTime,
    oftTID,
    oftRecordVersion,
    oftSessionUserID,
    oftDateTimeMS,
    oftUnixTime,
    oftUnixMSTime);

  /// set of available SQL field property types
  TOrmFieldTypes = set of TOrmFieldType;

  //// a fixed array of SQL field property types
  TOrmFieldTypeArray = array[0..MAX_SQLFIELDS] of TOrmFieldType;

  /// contains the parameters used for sorting
  // - FieldCount is 0 if was never sorted
  // - used to sort data again after a successfull data update with
  // TOrmTableJson.FillFrom()
  TOrmTableSortParams = record
    Comp: TUtf8Compare;
    FieldCount, FieldIndex: integer;
    FieldType: TOrmFieldType;
    Asc: boolean;
  end;

  /// used to define the triggered Event types for TOnOrmEvent
  // - some Events can be triggered via TRestServer.OnUpdateEvent when
  // a Table is modified, and actions can be authorized via overriding the
  // TRest.RecordCanBeUpdated method
  // - OnUpdateEvent is called BEFORE deletion, and AFTER insertion or update; it
  // should be used only server-side, not to synchronize some clients: the framework
  // is designed around a stateless RESTful architecture (like HTTP/1.1), in which
  // clients ask the server for refresh (see TRestClientUri.UpdateFromServer)
  // - is used also by TOrm.ComputeFieldsBeforeWrite virtual method
  TOrmEvent = (
    oeAdd,
    oeUpdate,
    oeDelete,
    oeUpdateBlob);

  /// used to define the triggered Event types for TOrmHistory
  // - TOrmHistory.History will be used for heArchiveBlob
  // - TOrmHistory.SentDataJson will be used for other kind of events
  TOrmHistoryEvent = (
    heAdd,
    heUpdate,
    heDelete,
    heArchiveBlob);

  /// used to defined the CRUD associated SQL statement of a command
  // - used e.g. by TOrm.GetJsonValues methods and SimpleFieldsBits[] array
  // (in this case, ooDelete is never used, since deletion is global for all fields)
  // - also used for cache content notification
  TOrmOccasion = (
    ooSelect,
    ooInsert,
    ooUpdate,
    ooDelete);

  /// used to defined a set of CRUD associated SQL statement of a command
  TOrmOccasions = set of TOrmOccasion;

  /// the possible options for IRestOrmServer.CreateMissingTables and
  // TOrm.InitializeTable methods
  // - itoNoAutoCreateGroups and itoNoAutoCreateUsers will avoid
  // TAuthGroup.InitializeTable to fill the TAuthGroup and TAuthUser
  // tables with default records
  // - itoNoCreateMissingField will avoid to create the missing fields on a table
  // - itoNoIndex4ID won't create the index for the main ID field (do nothing
  // on SQLite3, by design - but may be used for tables on external databases)
  // - itoNoIndex4UniqueField won't create indexes for "stored AS_UNIQUE" fields
  // - itoNoIndex4NestedRecord won't create indexes for TOrm fields
  // - itoNoIndex4RecordReference won't create indexes for TRecordReference fields
  // - itoNoIndex4TID won't create indexes for TID fields
  // - itoNoIndex4RecordVersion won't create indexes for TRecordVersion fields
  // - INITIALIZETABLE_NOINDEX constant contain all itoNoIndex* items
  TOrmInitializeTableOption = (
    itoNoAutoCreateGroups,
    itoNoAutoCreateUsers,
    itoNoCreateMissingField,
    itoNoIndex4ID,
    itoNoIndex4UniqueField,
    itoNoIndex4NestedRecord,
    itoNoIndex4RecordReference,
    itoNoIndex4TID,
    itoNoIndex4RecordVersion);

  /// the options to be specified for IRestOrmServer.CreateMissingTables and
  // TOrm.InitializeTable methods
  TOrmInitializeTableOptions = set of TOrmInitializeTableOption;

const
  /// used as "stored AS_UNIQUE" published property definition in TOrm
  AS_UNIQUE = false;

  /// options to specify no index createon for IRestOrmServer.CreateMissingTables
  // and TOrm.InitializeTable methods
  INITIALIZETABLE_NOINDEX: TOrmInitializeTableOptions =
    [itoNoIndex4ID.. itoNoIndex4RecordVersion];

  EVENT2OCCASION: array[TOrmEvent] of TOrmOccasion = (
    ooInsert,    // oeAdd
    ooUpdate,    // oeUpdate
    ooDelete,    // oeDelete
    ooUpdate);   // oeUpdateBlob


// backward compatibility types redirections
{$ifndef PUREMORMOT2}

type
  TSqlEvent        = TOrmEvent;
  TSqlHistoryEvent = TOrmHistoryEvent;
  TSqlOccasion     = TOrmOccasion;
  TSqlOccasions    = TOrmOccasions;

const
  // TOrmFieldType into TSqlFieldType
  sftUnknown       = oftUnknown;
  sftAnsiText      = oftAnsiText;
  sftUtf8Text      = oftUtf8Text;
  sftEnumerate     = oftEnumerate;
  sftSet           = oftSet;
  sftInteger       = oftInteger;
  sftID            = oftID;
  sftRecord        = oftRecord;
  sftBoolean       = oftBoolean;
  sftFloat         = oftFloat;
  sftDateTime      = oftDateTime;
  sftTimeLog       = oftTimeLog;
  sftCurrency      = oftCurrency;
  sftObject        = oftObject;
  sftVariant       = oftVariant;
  sftNullable      = oftNullable;
  sftBlob          = oftBlob;
  sftBlobDynArray  = oftBlobDynArray;
  sftBlobCustom    = oftBlobCustom;
  sftUtf8Custom    = oftUtf8Custom;
  sftMany          = oftMany;
  sftModTime       = oftModTime;
  sftCreateTime    = oftCreateTime;
  sftTID           = oftTID;
  sftRecordVersion = oftRecordVersion;
  sftSessionUserID = oftSessionUserID;
  sftDateTimeMS    = oftDateTimeMS;
  sftUnixTime      = oftUnixTime;
  sftUnixMSTime    = oftUnixMSTime;

  // TOrmEvent/TOrmOccasion into TSqlEvent/TSqlOccasion
  seAdd        =  oeAdd;
  seUpdate     =  oeUpdate;
  seDelete     =  oeDelete;
  seUpdateBlob =  oeUpdateBlob;
  soSelect     =  ooSelect;
  soInsert     =  ooInsert;
  soUpdate     =  ooUpdate;
  soDelete     =  ooDelete;

{$endif PUREMORMOT2}

const
  /// kind of fields not retrieved during normal query, update or adding
  // - by definition, BLOB are excluded to save transmission bandwidth
  // - by design, TOrmMany properties are stored in an external pivot table
  // - by convenience, the TRecordVersion number is for internal use only
  NOT_SIMPLE_FIELDS: TOrmFieldTypes =
    [oftUnknown, oftBlob, oftMany, oftRecordVersion];

  /// kind of fields which can be copied from one TOrm instance to another
  COPIABLE_FIELDS: TOrmFieldTypes =
    [low(TOrmFieldType)..high(TOrmFieldType)] - [oftUnknown, oftMany];

  /// kind of DB fields which will contain TEXT content when converted to JSON
  TEXT_DBFIELDS: TSqlDBFieldTypes =
    [ftUtf8, ftDate];

  /// kind of fields which will contain pure TEXT values
  // - independently from the actual storage level
  // - i.e. will match RawUtf8, string, UnicodeString, WideString properties
  RAWTEXT_FIELDS: TOrmFieldTypes =
    [oftAnsiText, oftUtf8Text];

  /// kind of fields which will be stored as TEXT values
  // - i.e. RAWTEXT_FIELDS and TDateTime/TDateTimeMS
  STRING_FIELDS: TOrmFieldTypes =
    [oftAnsiText, oftUtf8Text, oftUtf8Custom, oftDateTime, oftDateTimeMS];

  /// the SQL field property types with their TNullable* equivalency
  // - those types may be stored in a variant published property, e.g.
  // ! property Int: TNullableInteger read fInt write fInt;
  // ! property Txt: TNullableUtf8Text read fTxt write fTxt;
  // ! property Txt: TNullableUtf8Text index 32 read fTxt write fTxt;
  NULLABLE_TYPES =
    [oftInteger, oftBoolean, oftEnumerate, oftFloat, oftCurrency,
     oftDateTime, oftTimeLog, oftUtf8Text];

/// detect ORM equivalency from TypeInfo(TNullable*) RTTI
function NullableTypeToOrmFieldType(aType: PRttiInfo): TOrmFieldType;

/// convert a ORM field type and associated RTTI into a DB data type
function OrmFieldTypeToDBField(aOrmFieldType: TOrmFieldType;
  aTypeInfo: PRttiInfo): TSqlDBFieldType;

function ToText(ft: TOrmFieldType): PShortString; overload;
function ToText(e: TOrmEvent): PShortString; overload;
function ToText(he: TOrmHistoryEvent): PShortString; overload;
function ToText(o: TOrmOccasion): PShortString; overload;

/// guess the content type of an UTF-8 encoded field value, as used in TOrmTable.Get()
// - if P if nil or 'null', return oftUnknown
// - otherwise, guess its type from its value characters
// - oftBlob is returned if the field is encoded as SQLite3 BLOB literals
// (X'53514C697465' e.g.) or with '\uFFF0' magic code
// - since P is PUtf8Char, string type is oftUtf8Text only
// - oftFloat is returned for any floating point value, even if it was
// declared as oftCurrency type
// - oftInteger is returned for any INTEGER stored value, even if it was declared
// as oftEnumerate, oftSet, oftID, oftTID, oftRecord, oftRecordVersion,
// oftSessionUserID, oftBoolean, oftModTime/oftCreateTime/oftTimeLog or
// oftUnixTime/oftUnixMSTime type
function Utf8ContentType(P: PUtf8Char): TOrmFieldType;

/// guess the number type of an UTF-8 encoded field value, as used in TOrmTable.Get()
// - if P if nil or 'null', return oftUnknown
// - will return oftInteger or oftFloat if the supplied text is a number
// - will return oftUtf8Text for any non numerical content
function Utf8ContentNumberType(P: PUtf8Char): TOrmFieldType;
  {$ifdef HASINLINE}inline;{$endif}

type
  /// the available options for TRest.BatchStart() process
  // - boInsertOrIgnore will create 'INSERT OR IGNORE' statements instead of
  // plain 'INSERT' - by now, only the direct SQLite3 engine supports it
  // - boInsertOrUpdate will create 'INSERT OR REPLACE' statements instead of
  // plain 'INSERT' - by now, only the direct SQLite3 engine supports it
  // - boExtendedJson will force the JSON to unquote the column names,
  // e.g. writing col1:...,col2:... instead of "col1":...,"col2"...
  // - boPostNoSimpleFields (client-side only) will avoid to send a
  // TRestBach.Add() with simple fields as "SIMPLE":[val1,val2...] or
  // "SIMPLE@tablename":[val1,val2...], without the field names - for
  // backward compatibility with very old mORMot 1 servers
  // - boPutNoCacheFlush won't force the associated Cache entry to be flushed:
  // it is up to the caller to ensure cache coherency
  // - boRollbackOnError will raise an exception and Rollback any transaction
  // if any step failed - default if to continue batch processs, but setting
  // a value <> 200/HTTP_SUCCESS in Results[]
  // - boNoModelEncoding (client-side only) will force the mORMot 1 more verbose
  // "POST"/"SIMPLE"/"PUT" encodings - for backward compatibility, or if client
  // and server TOrmModel tables or fields do not match
  // - boOnlyObjects will force to generate only a JSON array of raw JSON
  // objects with no BATCH prefix nor verbs
  TRestBatchOption = (
    boInsertOrIgnore,
    boInsertOrReplace,
    boExtendedJson,
    boPostNoSimpleFields,
    boPutNoCacheFlush,
    boRollbackOnError,
    boNoModelEncoding,
    boOnlyObjects);

  /// a set of options for TRest.BatchStart() process
  // - TJsonObjectDecoder will use it to compute the corresponding SQL
  TRestBatchOptions = set of TRestBatchOption;

  /// internal kind of encoding for one TRestBatch.Add/Update/Delete action
  TRestBatchEncoding = (
    encPost,
    encSimple,
    encPostHex,
    encPostHexID,
    encPut,
    encPutHexID,
    encDelete);

  /// how TOrmModel.UriMatch() will compare an URI
  // - will allow to make a difference about case-sensitivity
  TRestModelMatch = (
    rmNoMatch,
    rmMatchExact,
    rmMatchWithCaseChange);

  /// the kind of SQlite3 (virtual) table
  // - TOrmFts3/4/5 will be associated with vFTS3/vFTS4/vFTS5 values,
  // TOrmRTree/TOrmRTreeInteger with rRTree/rRTreeInteger, any native
  // SQlite3 table as vSQLite3, and a TOrmVirtualTable*ID as
  // rCustomForcedID/rCustomAutoID
  // - a plain TOrm class can be defined as rCustomForcedID (e.g. for
  // TOrmMany) after registration for an external DB via a call to
  // VirtualTableExternalRegister() from mormot.orm.sql unit
  TOrmVirtualKind = (
    ovkSQLite3,
    ovkFTS3,
    ovkFTS4,
    ovkFTS5,
    ovkRTree,
    ovkRTreeInteger,
    ovkCustomForcedID,
    ovkCustomAutoID);

  /// the kind of fields to be available in a Table resulting of
  // a TOrmMany.DestGetJoinedTable() method call
  // - Source fields are not available, because they will be always the same for
  // a same SourceID, and they should be available from the TOrm which
  // hold the TOrmMany instance
  // - jkDestID and jkPivotID will retrieve only DestTable.ID and PivotTable.ID
  // - jkDestFields will retrieve DestTable.* simple fields, or the fields
  // specified by aCustomFieldsCsv (the Dest table name will be added: e.g.
  // for aCustomFieldsCsv='One,Two', will retrieve DestTable.One, DestTable.Two)
  // - jkPivotFields will retrieve PivotTable.* simple fields, or the fields
  // specified by aCustomFieldsCsv (the Pivot table name will be added: e.g.
  // for aCustomFieldsCsv='One,Two', will retrieve PivotTable.One, PivotTable.Two)
  // - jkPivotAndDestAllFields for PivotTable.* and DestTable.* simple fields,
  // or will retrieve the specified aCustomFieldsCsv fields (with
  // the table name associated: e.g. 'PivotTable.One, DestTable.Two')
  TOrmManyJoinKind = (
    jkDestID,
    jkPivotID,
    jkDestFields,
    jkPivotFields,
    jkPivotAndDestFields);

  /// pre-computed SQL statements for ORM operations for a given
  // TOrmModelProperties instance
  // - those statements will work for internal tables, not for external
  // tables with mapped table or fields names
  TOrmModelPropertiesSql = record
    /// the simple field names in a SQL SELECT compatible format: 'COL1,COL2' e.g.
    // - format is
    // ! SQL.TableSimpleFields[withID: boolean; withTableName: boolean]
    // - returns '*' if no field is of RawBlob/TOrmMany kind
    // - returns 'COL1,COL2' with all COL* set to simple field names if withID is false
    // - returns 'ID,COL1,COL2' with all COL* set to simple field names if withID is true
    // - returns 'Table.ID,Table.COL1,Table.COL2' if withTableName and withID are true
    TableSimpleFields: array[boolean, boolean] of RawUtf8;
    /// the SQL statement for reading all simple fields and RowID of all rows
    // - to be checked if we may safely call EngineList()
    SelectAllWithRowID: RawUtf8;
    /// the SQL statement for reading all simple fields with ID of all rows
    // - to be checked if we may safely call EngineList()
    SelectAllWithID: RawUtf8;
    /// the SQL statement for reading all simple fields of a given ID
    // - as used e.g. by TRestOrmServerDB.MainEngineRetrieve
    SelectOneWithID: RawUtf8;
    /// the JOINed SQL statement for reading all fields with ID, including
    // nested TOrm pre-allocated instances
    // - is '' if there is no nested TOrm
    SelectAllJoined: RawUtf8;
    /// the updated simple fields exposed as 'COL1=?,COL2=?'
    // - excluding ID (but including TCreateTime fields - as used in
    // TOrmVirtualTableExternal.Update method)
    // - to be used e.g. for UPDATE statements
    UpdateSetSimple: RawUtf8;
    /// all updated fields exposed as 'COL1=?,COL2=?'
    // - excluding ID (but including TCreateTime fields - as used in
    // TOrmVirtualTableExternal.Update method)
    // - to be used e.g. for UPDATE statements
    UpdateSetAll: RawUtf8;
    /// all fields, excluding the ID field, exposed as 'COL1,COL2'
    // - to be used e.g. in TOrmVirtualTableExternal.Insert()
    InsertSet: RawUtf8;
  end;

  /// used by TOrmPropertiesMapping.Options for custom field mapping
  // of a TOrm on an external database process
  // - rpmAutoMapKeywordFields is set if MapAutoKeywordFields has been defined,
  // i.e. if field names which may conflict with a keyword should be
  // automatically mapped to a harmless symbol name
  // - rpmNoCreateMissingTable will bypass the existing table check, e.g.
  // to circumvent some specific DB provider or case sensitivity issue on tables
  // - rpmNoCreateMissingField will bypass the existing field check, e.g.
  // to circumvent some specific DB provider or case sensitivity issue on fields
  // - by default, check of missing field name will be case insensitive, unless
  // the rpmMissingFieldNameCaseSensitive option is set
  // - rpmQuoteFieldName will quote the field names - to be used e.g. with
  // FireBird in its Dialect 3
  // - rpmClearPoolOnConnectionIssue will enable detecting connection loss
  TOrmPropertiesMappingOptions = set of (
    rpmAutoMapKeywordFields,
    rpmNoCreateMissingTable,
    rpmNoCreateMissingField,
    rpmMissingFieldNameCaseSensitive,
    rpmQuoteFieldName,
    rpmClearPoolOnConnectionIssue);

const
  /// if the TOrmVirtual table kind is a FTS virtual table
  IS_FTS =
    [ovkFTS3, ovkFTS4, ovkFTS5];

  /// if the TOrmVirtual table kind is not an embedded type
  // - can be set for a TOrm after a VirtualTableExternalRegister call
  IS_CUSTOM_VIRTUAL =
    [ovkCustomForcedID, ovkCustomAutoID];

  /// if the TOrmVirtual table kind expects the ID to be set on INSERT
  INSERT_WITH_ID =
    [ovkFTS3, ovkFTS4, ovkFTS5, ovkRTree, ovkRTreeInteger, ovkCustomForcedID];

  /// if a TOrmVirtualTablePreparedConstraint.Column is to be ignored
  VIRTUAL_TABLE_IGNORE_COLUMN = -2;

  /// if a TOrmVirtualTablePreparedConstraint.Column points to the RowID
  VIRTUAL_TABLE_ROWID_COLUMN = -1;

  /// kind of encodings generated by TRestBatch.Add
  BATCH_INSERT = [encPost, encSimple, encPostHex, encPostHexID];

  /// kind of encodings generated by TRestBatch which result in a JSON array
  BATCH_DIRECT = [encSimple, encPostHex, encPostHexID, encPutHexID];

  /// kind of encodings generated by TRestBatch.Add which result in a JSON array
  BATCH_DIRECT_ADD = [encSimple, encPostHex, encPostHexID];

  /// kind of encodings generated by TRestBatch which result in a JSON array
  // starting with the inserted/updated ID field
  BATCH_DIRECT_ID = [encPostHexID, encPutHexID];

  /// client-side only options for TRestBatch, not transmitted to server
  BATCH_OPTIONS_CLIENTONLY = [boExtendedJson, boPostNoSimpleFields,
                              boNoModelEncoding, boOnlyObjects];

var
  /// late-binding return of TOrmVirtualTableClass.ModuleName
  // - link mormot.orm.storage.pas unit for properly set this value
  GetVirtualTableModuleName: function(VirtualTableClass: TClass): RawUtf8;

function ToText(enc: TRestBatchEncoding): PShortString; overload;
function ToText(vk: TOrmVirtualKind): PShortString; overload;


/// encode as a SQL-ready INSERT or UPDATE statement with ? as values
// - after a successfull call to Decode()
// - FieldValues[] content will be ignored
// - Occasion can be only ooInsert or ooUpdate
// - for ooUpdate, will create UPDATE ... SET ... where UpdateIDFieldName=?
// - you can specify some options, e.g. boInsertOrIgnore for ooInsert
// - MultiInsertRowCount would generate INSERT .. VALUES (..),(..),(..),..
function EncodeAsSqlPrepared(const Decoder: TJsonObjectDecoder;
  const TableName: RawUtf8; Occasion: TOrmOccasion;
  const UpdateIDFieldName: RawUtf8; BatchOptions: TRestBatchOptions;
  MultiInsertRowCount: integer = 1): RawUtf8;

/// low-level function used to convert a JSON Value into a variant,
// according to the property type
// - for oftObject, oftVariant, oftBlobDynArray and oftUtf8Custom, the
// JSON buffer may be an array or an object, so createValueTempCopy can
// create a temporary copy before parsing it in-place, to preserve the buffer
// - oftUnknown and oftMany will set a varEmpty (Unassigned) value
// - typeInfo may be used for oftBlobDynArray conversion to a TDocVariant array
procedure ValueVarToVariant(Value: PUtf8Char; ValueLen: integer;
  fieldType: TOrmFieldType; var result: TVarData; createValueTempCopy: boolean;
  typeInfo: PRttiInfo; options: TDocVariantOptions = JSON_FAST);


{ ****************** ORM Ready UTF-8 Comparison Functions }

/// special comparison function for sorting ftRecord (TRecordReference/RecordRef)
// UTF-8 encoded values in the SQLite3 database or JSON content
function Utf8CompareRecord(P1, P2: PUtf8Char): PtrInt;

/// special comparison function for sorting oftBoolean
// UTF-8 encoded values in the SQLite3 database or JSON content
function Utf8CompareBoolean(P1, P2: PUtf8Char): PtrInt;

/// special comparison function for sorting oftEnumerate, oftSet or oftID
// UTF-8 encoded values in the SQLite3 database or JSON content
function Utf8CompareUInt32(P1, P2: PUtf8Char): PtrInt;

/// special comparison function for sorting oftInteger, oftTID, oftRecordVersion
// oftTimeLog/oftModTime/oftCreateTime or oftUnixTime/oftUnixMSTime UTF-8 encoded
// values in the SQLite3 database or JSON content
function Utf8CompareInt64(P1, P2: PUtf8Char): PtrInt;

/// special comparison function for sorting oftCurrency
// UTF-8 encoded values in the SQLite3 database or JSON content
function Utf8CompareCurr64(P1, P2: PUtf8Char): PtrInt;

/// special comparison function for sorting oftFloat
// UTF-8 encoded values in the SQLite3 database or JSON content
function Utf8CompareDouble(P1, P2: PUtf8Char): PtrInt;

/// special comparison function for sorting oftDateTime or oftDateTimeMS
// UTF-8 encoded values in the SQLite3 database or JSON content
function Utf8CompareIso8601(P1, P2: PUtf8Char): PtrInt;

var
  /// simple wrapper to UTF-8 compare function for the SQLite3 field datatypes
  // - used internally for field sorting (see TOrmTable.SortFields() method)
  // and for default User Interface Query (see TRest.QueryIsTrue() method)
  // - some functions do not match exactly the TUtf8Compare signature, so will
  // be set in the initialization section of this unit
  OrmFieldTypeComp: array[TOrmFieldType] of TUtf8Compare  =
   (nil,                 // unknown
    nil,                 // AnsiText = AnsiIComp (in initialization below)
    Utf8IComp,           // Utf8Text, 8-bit case insensitive compared
    Utf8CompareUInt32,   // Enumerate
    Utf8CompareUInt32,   // Set
    Utf8CompareInt64,    // integer
    Utf8CompareInt64,    // ID
    Utf8CompareRecord,   // Record
    Utf8CompareBoolean,  // boolean
    Utf8CompareDouble,   // Float
    Utf8CompareIso8601,  // TDateTime
    Utf8CompareInt64,    // TTimeLog
    Utf8CompareCurr64,   // Currency
    nil,                 // Object (TEXT serialization) = StrComp
    nil,                 // Variant (TEXT serialization) = StrComp
    nil,                 // TNullable* = StrComp
    nil,                 // Blob
    nil,                 // BlobDynArray
    nil,                 // BlobCustom
    nil,                 // Utf8Custom
    nil,                 // Many
    Utf8CompareInt64,    // TModTime
    Utf8CompareInt64,    // TCreateTime
    Utf8CompareInt64,    // TID
    Utf8CompareInt64,    // TRecordVersion
    Utf8CompareInt64,    // TSessionUserID
    Utf8CompareIso8601,  // TDateTimeMS
    Utf8CompareInt64,    // TUnixTime
    Utf8CompareInt64);   // TUnixMSTime


{ ************ TJsonSerializer Class for TOrm Serialization }

type
  /// several options to customize how TOrm will be serialized
  // - e.g. if properties storing JSON should be serialized as an object, and not
  // escaped as a string (which is the default, matching ORM column storage)
  // - if an additional "ID_str":"12345" field should be added to the standard
  // "ID":12345 field, which may exceed 53-bit integer precision of JavsCript
  TJsonSerializerOrmOption = (
    jwoAsJsonNotAsString,
    jwoID_str);

  /// options to customize how TOrm will be written by TJsonSerializer
  TJsonSerializerOrmOptions = set of TJsonSerializerOrmOption;

  /// simple writer to a Stream, specialized for writing TOrm as JSON
  // - in respect to the standard TJsonWriter as defined in mormot.db.core,
  // this class has some options dedicated to our TOrm serialization
  TJsonSerializer = class(TJsonWriter)
  protected
    fOrmOptions: TJsonSerializerOrmOptions;
    procedure SetOrmOptions(Value: TJsonSerializerOrmOptions);
  public
    // backward compatibility methods - use Rtti global instead
    {$ifndef PUREMORMOT2}
    class procedure RegisterClassForJson(aItemClass: TClass); overload;
    class procedure RegisterClassForJson(const aItemClass: array of TClass); overload;
    class procedure RegisterCollectionForJson(aCollection: TCollectionClass;
      aItem: TCollectionItemClass);
    class procedure RegisterObjArrayForJson(aDynArray: PRttiInfo; aItem: TClass); overload;
    class procedure RegisterObjArrayForJson(const aDynArrayClassPairs: array of const); overload;
    {$endif PUREMORMOT2}
    /// customize TOrm.GetJsonValues serialization process
    // - jwoAsJsonNotAsString will force TOrm.GetJsonValues to serialize
    // nested property instances as a JSON object/array, not a JSON string:
    // i.e. root/table/id REST will be ready-to-be-consumed from AJAX clients
    // (e.g. TOrmPropInfoRttiObject.GetJsonValues as a JSON object, and
    // TOrmPropInfoRttiDynArray.GetJsonValues as a JSON array)
    // - jwoID_str will add an "ID_str":"12345" property to the default
    // "ID":12345 field to circumvent JavaScript's limitation of 53-bit for
    // integer numbers, which is easily reached with our 64-bit TID values, e.g.
    // if TSynUniqueIdentifier are used to generate the IDs: AJAX clients should
    // better use this "ID_str" string value to identify each record, and ignore
    // the "id" fields
    property OrmOptions: TJsonSerializerOrmOptions
      read fOrmOptions write SetOrmOptions;
  end;


{ ************ TOrmPropInfo ORM / RTTI Classes }

type
  /// ORM attributes for a TOrmPropInfo definition
  TOrmPropInfoAttribute = (
    aIsUnique,
    aAuxiliaryRTreeField,
    aBinaryCollation,
    aUnicodeNoCaseCollation);

  /// set of ORM attributes for a TOrmPropInfo definition
  TOrmPropInfoAttributes = set of TOrmPropInfoAttribute;

  /// abstract parent class to store information about a published property
  // - property information could be retrieved from RTTI (TOrmPropInfoRtti*),
  // or be defined by code (TOrmPropInfoCustom derivated classes) when RTTI
  // is not available
  TOrmPropInfo = class
  protected
    fName: RawUtf8;
    fNameUnflattened: RawUtf8;
    fOrmFieldType: TOrmFieldType;
    fOrmFieldTypeStored: TOrmFieldType;
    fSqlDBFieldType: TSqlDBFieldType;
    fAttributes: TOrmPropInfoAttributes;
    fFieldWidth: integer;
    fPropertyIndex: integer;
    function GetNameDisplay: string; virtual;
    /// those two protected methods allow custom storage of binary content as text
    // - default implementation is to use hexa (ToSql=true) or Base64 encodings
    procedure BinaryToText(var Value: RawUtf8; ToSql: boolean;
      wasSqlString: PBoolean); virtual;
    procedure TextToBinary(Value: PUtf8Char; var result: RawByteString); virtual;
    function GetOrmFieldTypeName: PShortString;
    function GetSqlFieldRttiTypeName: RawUtf8; virtual;
    // overriden method shall use direct copy of the low-level binary content,
    // to be faster than a DestInfo.SetValue(Dest,GetValue(Source)) call
    procedure CopySameClassProp(Source: TObject; DestInfo: TOrmPropInfo;
      Dest: TObject); virtual;
  public
    /// initialize the internal fields
    // - not called directly, but from TOrmPropInfoRtti.CreateFrom() factory
    constructor Create(const aName: RawUtf8; aOrmFieldType: TOrmFieldType;
      aAttributes: TOrmPropInfoAttributes; aFieldWidth, aPropertyIndex: integer);
      reintroduce; virtual;
    /// the property definition Name
    property Name: RawUtf8
      read fName;
    /// the property definition Name, afer un-camelcase and translation
    property NameDisplay: string
      read GetNameDisplay;
    /// the property definition Name, with full path name if has been flattened
    // - if the property has been flattened (for a TOrmPropInfoRtti), the real
    // full nested class will be returned, e.g. 'Address.Country.Iso' for
    // the 'Address_Country' flattened property name
    property NameUnflattened: RawUtf8
      read fNameUnflattened;
    /// the property index in the RTTI
    property PropertyIndex: integer
      read fPropertyIndex;
    /// the corresponding column type, as managed by the ORM layer
    property OrmFieldType: TOrmFieldType
      read fOrmFieldType;
    /// the corresponding column type, as stored by the ORM layer
    // - match OrmFieldType, unless for OrmFieldType=oftNullable, in which this
    // field will contain the simple type eventually stored in the database
    property OrmFieldTypeStored: TOrmFieldType
      read fOrmFieldTypeStored;
    /// the corresponding column type name, as managed by the ORM layer and
    // retrieved by the RTTI
    // - returns e.g. 'oftTimeLog'
    property OrmFieldTypeName: PShortString
      read GetOrmFieldTypeName;
    /// the type name, as defined in the RTTI
    // - returns e.g. 'RawUtf8'
    // - will return the TOrmPropInfo class name if it is not a TOrmPropInfoRtti
    property SqlFieldRttiTypeName: RawUtf8
      read GetSqlFieldRttiTypeName;
    /// the corresponding column type, as managed for abstract database access
    // - TNullable* fields will report here the corresponding simple DB type,
    // e.g. ftInt64 for TNullableInteger (following OrmFieldTypeStored value)
    property SqlDBFieldType: TSqlDBFieldType
      read fSqlDBFieldType;
    /// the corresponding column type name, as managed for abstract database access
    function SqlDBFieldTypeName: PShortString;
    /// the ORM attributes of this property
    // - contains aIsUnique e.g for TOrm published properties marked as
    // ! property MyProperty: RawUtf8 stored AS_UNIQUE;
    // (i.e. "stored false")
    property Attributes: TOrmPropInfoAttributes
      read fAttributes write fAttributes;
    /// the optional width of this field, in external databases
    // - is set e.g. by index attribute of TOrm published properties as
    // ! property MyProperty: RawUtf8 index 10;
    property FieldWidth: integer
      read fFieldWidth;
  public
    /// convert UTF-8 encoded text into the property value
    // - setter method (write Set*) is called if available
    // - if no setter exists (no write declaration), the getted field address is used
    // - handle UTF-8 SQL to Delphi values conversion
    // - expect BLOB fields encoded as SQlite3 BLOB literals ("x'01234'" e.g.)
    // or base-64 encoded stream for JSON ("\uFFF0base64encodedbinary") - i.e.
    // both format supported by BlobToRawBlob() function
    // - handle TPersistent, TCollection, TRawUtf8List or TStrings with JsonToObject
    // - note that the supplied Value buffer won't be modified by this method:
    // overriden implementation should create their own temporary copy
    procedure SetValue(Instance: TObject; Value: PUtf8Char; ValueLen: PtrInt;
      wasString: boolean); virtual; abstract;
    /// convert UTF-8 encoded text into the property value
    // - just a wrapper around SetValue(...,pointer(Value),...) which may be
    // optimized for overriden methods
    procedure SetValueVar(Instance: TObject; const Value: RawUtf8;
      wasString: boolean); virtual;
    /// convert the property value into an UTF-8 encoded text
    // - if ToSql is true, result is on SQL form (false->'0' e.g.)
    // - if ToSql is false, result is on JSON form (false->'false' e.g.)
    // - BLOB field returns SQlite3 BLOB literals ("x'01234'" e.g.) if ToSql is
    // true, or base-64 encoded stream for JSON ("\uFFF0base64encodedbinary")
    // - getter method (read Get*) is called if available
    // - handle Delphi values into UTF-8 SQL conversion
    // - oftBlobDynArray, oftBlobCustom or oftBlobRecord are returned as BLOB
    // litterals ("X'53514C697465'") if ToSql is true, or base-64 encoded stream
    // for JSON ("\uFFF0base64encodedbinary")
    // - handle TPersistent, TCollection, TRawUtf8List or TStrings with ObjectToJson
    function GetValue(Instance: TObject; ToSql: boolean;
      wasSqlString: PBoolean = nil): RawUtf8;
      {$ifdef HASINLINE}inline;{$endif}
    /// convert the property value into an UTF-8 encoded text
    // - this method is the same as GetValue(), but avoid assigning the result
    // string variable (some speed up on multi-core CPUs, since avoid a CPU LOCK)
    // - this virtual method is the one to be overridden by the implementing classes
    procedure GetValueVar(Instance: TObject; ToSql: boolean; var result: RawUtf8;
      wasSqlString: PBoolean); virtual; abstract;
    /// normalize the content of Value, so that GetValue(Object,true) should return the
    // same content (true for ToSql format)
    procedure NormalizeValue(var Value: RawUtf8); virtual; abstract;
    /// retrieve a field value into a TSqlVar value
    // - the temp RawByteString is used as a temporary storage for TEXT or BLOB
    // and should be available during all access to the TSqlVar fields
    procedure GetFieldSqlVar(Instance: TObject; var aValue: TSqlVar;
      var temp: RawByteString); virtual;
    /// set a field value from a TSqlVar value
    function SetFieldSqlVar(Instance: TObject; const aValue: TSqlVar): boolean; virtual;
    /// returns TRUE if value is 0 or ''
    function IsValueVoid(Instance: TObject): boolean; virtual;
    /// append the property value into a binary buffer
    procedure GetBinary(Instance: TObject; W: TBufferWriter); virtual; abstract;
    /// read the property value from a binary buffer
    procedure SetBinary(Instance: TObject; var Read: TFastReader);
      virtual; abstract;
    /// copy a property value from one instance to another
    // - both objects should have the same exact property
    procedure CopyValue(Source, Dest: TObject); virtual;
    /// copy a value from one instance to another property instance
    // - if the property has been flattened (for a TOrmPropInfoRtti), the real
    // Source/Dest instance will be used for the copy
    procedure CopyProp(Source: TObject; DestInfo: TOrmPropInfo; Dest: TObject);
    /// retrieve the property value into a Variant
    // - will set the Variant type to the best matching kind according to the
    // OrmFieldType type
    // - BLOB field returns SQlite3 BLOB textual literals ("x'01234'" e.g.)
    // - dynamic array field is returned as a variant array
    procedure GetVariant(Instance: TObject; var Dest: Variant); virtual;
    /// set the property value from a Variant value
    // - dynamic array field must be set from a variant array
    // - will set the Variant type to the best matching kind according to the
    // OrmFieldType type
    // - expect BLOB fields encoded as SQlite3 BLOB literals ("x'01234'" e.g.)
    procedure SetVariant(Instance: TObject; const Source: Variant); virtual;
    /// compare the content of the property of two objects
    // - not all kind of properties are handled: only main types (like GetHash)
    // - if CaseInsensitive is TRUE, will apply NormToUpper[] 8-bit uppercase,
    // handling RawUtf8 properties just like the SYSTEMNOCASE collation
    // - this method should match the case-sensitivity of GetHash()
    // - this default implementation will call GetValueVar() for slow comparison
    function CompareValue(Item1, Item2: TObject; CaseInsensitive: boolean): integer; virtual;
    /// retrieve an unsigned 32-bit hash of the corresponding property
    // - not all kind of properties are handled: only main types
    // - if CaseInsensitive is TRUE, will apply NormToUpper[] 8-bit uppercase,
    // handling RawUtf8 properties just like the SYSTEMNOCASE collation
    // - note that this method can return a hash value of 0
    // - this method should match the case-sensitivity of CompareValue()
    // - this default implementation will call GetValueVar() for slow computation
    function GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal; virtual;
    /// add the JSON content corresponding to the given property
    // - this default implementation will call safe but slow GetValueVar() method
    procedure GetJsonValues(Instance: TObject; W: TTextWriter); virtual;
    /// returns an untyped pointer to the field property memory in a given instance
    function GetFieldAddr(Instance: TObject): pointer; virtual; abstract;
    /// for pilSubClassesFlattening properties, compute the actual instance
    // containing the property value
    // - this abstract parent implementation returns the instance
    function Flattened(Instance: TObject): TObject; virtual;
    /// redirect TOnDynArraySortCompare callback to case-sensitive CompareValue()
    function EventCompare(const A, B): integer;
    /// redirect TOnDynArrayHashOne callback to case-sensitive GetHash()
    function EventHash(const Elem): cardinal;
    /// redirect TOnDynArraySortCompare callback to case-insensitive CompareValue()
    function EventCompareI(const A, B): integer;
    /// redirect TOnDynArrayHashOne to callback case-insensitive GetHash()
    function EventHashI(const Elem): cardinal;
  end;
  POrmPropInfo = ^TOrmPropInfo;

  /// class-reference type (metaclass) of a TOrmPropInfo information
  TOrmPropInfoClass = class of TOrmPropInfo;

  /// define how the published properties RTTI is to be interpreted
  // - i.e. how TOrmPropInfoList.Create() and TOrmPropInfoRtti.CreateFrom()
  // will handle the incoming RTTI
  TOrmPropInfoListOptions = set of (
    pilRaiseEOrmExceptionIfNotHandled,
    pilAllowIDFields,
    pilSubClassesFlattening,
    pilIgnoreIfGetter,
    pilSingleHierarchyLevel,
    pilAuxiliaryFields);

  /// parent information about a published property retrieved from RTTI
  TOrmPropInfoRtti = class(TOrmPropInfo)
  protected
    fPropInfo: PRttiProp;
    fPropType: PRttiInfo;
    fPropRtti: TRttiJson;
    fFlattenedProps: PRttiPropDynArray;
    fGetterIsFieldPropOffset, fSetterIsFieldPropOffset: PtrUInt;
    fInPlaceCopySameClassPropOffset: PtrUInt;
    fRttiCustomProp: PRttiCustomProp;
    function GetSqlFieldRttiTypeName: RawUtf8; override;
    function GetRttiCustomProp(Instance: TObject): PRttiCustomProp;
      {$ifdef HASINLINE}inline;{$endif}
  public
    /// main factory of a TOrmPropInfo instance retrieved from RTTI
    class function CreateFrom(aPropInfo: PRttiProp; aPropIndex: integer;
      aOptions: TOrmPropInfoListOptions;
      const aFlattenedProps: PRttiPropDynArray): TOrmPropInfo;
    /// initialize the internal fields
    // - not usually called directly, but from CreateFrom() factory
    constructor Create(aPropInfo: PRttiProp; aPropIndex: integer;
      aOrmFieldType: TOrmFieldType;
      aOptions: TOrmPropInfoListOptions); reintroduce; virtual;
    /// register this class corresponding to the RTTI TypeInfo() pointer
    // - could be used e.g. to define custom serialization and process of
    // any custom type
    class procedure RegisterTypeInfo(aTypeInfo: PRttiInfo);
    /// retrieve the property value into a Variant
    // - will set the Variant type to the best matching kind according to the
    // OrmFieldType type
    // - BLOB field returns SQlite3 BLOB textual literals ("x'01234'" e.g.)
    // - dynamic array field is returned as a variant array
    procedure GetVariant(Instance: TObject; var Dest: Variant); override;
    /// retrieve the property field offset from RTTI
    function GetFieldAddr(Instance: TObject): pointer; override;
    /// get the absolute reading property field offset from RTTI
    property GetterIsFieldPropOffset: PtrUInt
      read fGetterIsFieldPropOffset;
    /// get the absolute writing property field offset from RTTI
    property SetterIsFieldPropOffset: PtrUInt
      read fSetterIsFieldPropOffset;
    /// for pilSubClassesFlattening properties, compute the actual instance
    // containing the property value
    // - if the property was not flattened, return the instance
    function Flattened(Instance: TObject): TObject; override;
    /// corresponding RTTI information
    property PropInfo: PRttiProp
      read fPropInfo;
    /// for pilSubClassesFlattening properties, the parents RTTI
    property FlattenedPropInfo: PRttiPropDynArray
      read fFlattenedProps;
    /// corresponding type information, as retrieved from PropInfo RTTI
    property PropType: PRttiInfo
      read fPropType;
    /// corresponding JSON-aware type information, as retrieved from PropInfo RTTI
    property PropRtti: TRttiJson
      read fPropRtti;
  end;

  /// class-reference type (metaclass) of a TOrmPropInfoRtti information
  TOrmPropInfoRttiClass = class of TOrmPropInfoRtti;

  TOrmPropInfoRttiObjArray = array of TOrmPropInfoRtti;

  /// information about an ordinal Int32 published property
  TOrmPropInfoRttiInt32 = class(TOrmPropInfoRtti)
  protected
    fUnsigned, fIntegerGetPropOffset, fIntegerSetPropOffset: boolean;
    procedure CopySameClassProp(Source: TObject; DestInfo: TOrmPropInfo;
      Dest: TObject); override;
  public
    constructor Create(aPropInfo: PRttiProp; aPropIndex: integer;
      aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions); override;
    procedure SetValue(Instance: TObject; Value: PUtf8Char; ValueLen: PtrInt;
      wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSql: boolean; var result: RawUtf8;
      wasSqlString: PBoolean); override;
    procedure GetFieldSqlVar(Instance: TObject; var aValue: TSqlVar;
      var temp: RawByteString); override;
    function SetFieldSqlVar(Instance: TObject; const aValue: TSqlVar): boolean; override;
    procedure GetBinary(Instance: TObject; W: TBufferWriter); override;
    procedure SetBinary(Instance: TObject; var Read: TFastReader); override;
    function CompareValue(Item1, Item2: TObject; CaseInsensitive: boolean): integer; override;
    function GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal; override;
    procedure NormalizeValue(var Value: RawUtf8); override;
    procedure GetJsonValues(Instance: TObject; W: TTextWriter); override;
  end;

  /// information about a set published property
  TOrmPropInfoRttiSet = class(TOrmPropInfoRttiInt32)
  protected
    fSetEnumType: PRttiEnumType;
  public
    constructor Create(aPropInfo: PRttiProp; aPropIndex: integer;
      aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions); override;
    property SetEnumType: PRttiEnumType
      read fSetEnumType;
  end;

  /// information about a enumeration published property
  // - can be either oftBoolean or oftEnumerate kind of property
  TOrmPropInfoRttiEnum = class(TOrmPropInfoRttiInt32)
  protected
    fEnumType: PRttiEnumType;
  public
    constructor Create(aPropInfo: PRttiProp; aPropIndex: integer;
      aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions); override;
    procedure SetValue(Instance: TObject; Value: PUtf8Char; ValueLen: PtrInt;
      wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSql: boolean; var result: RawUtf8;
      wasSqlString: PBoolean); override;
    procedure NormalizeValue(var Value: RawUtf8); override;
    procedure GetJsonValues(Instance: TObject; W: TTextWriter); override;
    function GetCaption(Value: RawUtf8; out IntValue: integer): string;
    property EnumType: PRttiEnumType
      read fEnumType;
  end;

  /// information about a character published property
  TOrmPropInfoRttiChar = class(TOrmPropInfoRttiInt32)
  public
    procedure SetValue(Instance: TObject; Value: PUtf8Char; ValueLen: PtrInt;
      wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSql: boolean; var result: RawUtf8;
      wasSqlString: PBoolean); override;
    procedure NormalizeValue(var Value: RawUtf8); override;
  end;

  /// information about an ordinal Int64 published property
  TOrmPropInfoRttiInt64 = class(TOrmPropInfoRtti)
  protected
    fIsQWord: boolean;
    procedure CopySameClassProp(Source: TObject; DestInfo: TOrmPropInfo;
      Dest: TObject); override;
  public
    constructor Create(aPropInfo: PRttiProp; aPropIndex: integer;
      aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions); override;
    procedure SetValue(Instance: TObject; Value: PUtf8Char; ValueLen: PtrInt;
      wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSql: boolean; var result: RawUtf8;
      wasSqlString: PBoolean); override;
    procedure GetFieldSqlVar(Instance: TObject; var aValue: TSqlVar;
      var temp: RawByteString); override;
    function SetFieldSqlVar(Instance: TObject; const aValue: TSqlVar): boolean; override;
    procedure GetBinary(Instance: TObject; W: TBufferWriter); override;
    procedure SetBinary(Instance: TObject; var Read: TFastReader); override;
    function CompareValue(Item1, Item2: TObject; CaseInsensitive: boolean): integer; override;
    function GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal; override;
    procedure NormalizeValue(var Value: RawUtf8); override;
    procedure GetJsonValues(Instance: TObject; W: TTextWriter); override;
  end;

  /// information about a PtrInt published property, according to the native CPU
  // - not a real stand-alone class, but a convenient wrapper type
  {$ifdef CPU64}
  TOrmPropInfoRttiPtrInt = TOrmPropInfoRttiInt64;
  {$else}
  TOrmPropInfoRttiPtrInt =TOrmPropInfoRttiInt32;
  {$endif CPU64}

  /// information about a TTimeLog published property
  // - stored as an Int64, but with a specific class
  TOrmPropInfoRttiTimeLog = class(TOrmPropInfoRttiInt64);

  /// information about a TUnixTime published property
  // - stored as an Int64, but with a specific class
  TOrmPropInfoRttiUnixTime = class(TOrmPropInfoRttiInt64);

  /// information about a TUnixMSTime published property
  // - stored as an Int64, but with a specific class
  TOrmPropInfoRttiUnixMSTime = class(TOrmPropInfoRttiInt64);

  /// information about a floating-point Double published property
  TOrmPropInfoRttiDouble = class(TOrmPropInfoRtti)
  protected
    procedure CopySameClassProp(Source: TObject; DestInfo: TOrmPropInfo;
      Dest: TObject); override;
  public
    procedure SetValue(Instance: TObject; Value: PUtf8Char; ValueLen: PtrInt;
      wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSql: boolean; var result: RawUtf8;
      wasSqlString: PBoolean); override;
    procedure GetFieldSqlVar(Instance: TObject; var aValue: TSqlVar;
      var temp: RawByteString); override;
    function SetFieldSqlVar(Instance: TObject; const aValue: TSqlVar): boolean; override;
    procedure GetBinary(Instance: TObject; W: TBufferWriter); override;
    procedure SetBinary(Instance: TObject; var Read: TFastReader); override;
    procedure NormalizeValue(var Value: RawUtf8); override;
    procedure GetJsonValues(Instance: TObject; W: TTextWriter); override;
    function CompareValue(Item1, Item2: TObject; CaseInsensitive: boolean): integer; override;
    function GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal; override;
  end;

  /// information about a fixed-decimal Currency published property
  TOrmPropInfoRttiCurrency = class(TOrmPropInfoRttiDouble)
  protected
    procedure CopySameClassProp(Source: TObject; DestInfo: TOrmPropInfo;
      Dest: TObject); override;
  public
    procedure SetValue(Instance: TObject; Value: PUtf8Char; ValueLen: PtrInt;
      wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSql: boolean; var result: RawUtf8;
      wasSqlString: PBoolean); override;
    procedure GetFieldSqlVar(Instance: TObject; var aValue: TSqlVar;
      var temp: RawByteString); override;
    function SetFieldSqlVar(Instance: TObject; const aValue: TSqlVar): boolean; override;
    procedure GetBinary(Instance: TObject; W: TBufferWriter); override;
    procedure SetBinary(Instance: TObject; var Read: TFastReader); override;
    procedure NormalizeValue(var Value: RawUtf8); override;
    procedure GetJsonValues(Instance: TObject; W: TTextWriter); override;
    function CompareValue(Item1, Item2: TObject; CaseInsensitive: boolean): integer; override;
    function GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal; override;
  end;

  /// information about a TDateTime published property
  TOrmPropInfoRttiDateTime = class(TOrmPropInfoRttiDouble)
  public
    procedure SetValue(Instance: TObject; Value: PUtf8Char; ValueLen: PtrInt;
      wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSql: boolean; var result: RawUtf8;
      wasSqlString: PBoolean); override;
    procedure GetFieldSqlVar(Instance: TObject; var aValue: TSqlVar; var temp:
      RawByteString); override;
    procedure NormalizeValue(var Value: RawUtf8); override;
    procedure GetJsonValues(Instance: TObject; W: TTextWriter); override;
    function CompareValue(Item1, Item2: TObject; CaseInsensitive: boolean): integer; override;
  end;

  /// information about a AnsiString published property
  TOrmPropInfoRttiAnsi = class(TOrmPropInfoRtti)
  protected
    fEngine: TSynAnsiConvert;
    procedure CopySameClassProp(Source: TObject; DestInfo: TOrmPropInfo;
      Dest: TObject); override;
  public
    constructor Create(aPropInfo: PRttiProp; aPropIndex: integer;
      aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions); override;
    procedure SetValue(Instance: TObject; Value: PUtf8Char; ValueLen: PtrInt;
      wasString: boolean); override;
    procedure SetValueVar(Instance: TObject; const Value: RawUtf8; wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSql: boolean; var result: RawUtf8;
      wasSqlString: PBoolean); override;
    procedure CopyValue(Source, Dest: TObject); override;
    procedure GetBinary(Instance: TObject; W: TBufferWriter); override;
    procedure SetBinary(Instance: TObject; var Read: TFastReader); override;
    procedure GetJsonValues(Instance: TObject; W: TTextWriter); override;
    procedure GetFieldSqlVar(Instance: TObject; var aValue: TSqlVar; var temp:
      RawByteString); override;
    function SetFieldSqlVar(Instance: TObject; const aValue: TSqlVar): boolean; override;
    function CompareValue(Item1, Item2: TObject; CaseInsensitive: boolean): integer; override;
    function GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal; override;
    procedure NormalizeValue(var Value: RawUtf8); override;
  end;

  /// information about a RawUtf8 published property
  // - will also serialize a RawJson property without JSON escape
  TOrmPropInfoRttiRawUtf8 = class(TOrmPropInfoRttiAnsi)
  protected
    procedure CopySameClassProp(Source: TObject; DestInfo: TOrmPropInfo;
      Dest: TObject); override;
  public
    procedure SetValue(Instance: TObject; Value: PUtf8Char; ValueLen: PtrInt;
      wasString: boolean); override;
    procedure SetValueVar(Instance: TObject; const Value: RawUtf8;
      wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSql: boolean; var result: RawUtf8;
      wasSqlString: PBoolean); override;
    procedure GetFieldSqlVar(Instance: TObject; var aValue: TSqlVar;
      var temp: RawByteString); override;
    function SetFieldSqlVar(Instance: TObject; const aValue: TSqlVar): boolean; override;
    procedure SetBinary(Instance: TObject; var Read: TFastReader); override;
    function CompareValue(Item1, Item2: TObject; CaseInsensitive: boolean): integer; override;
    function GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal; override;
    procedure GetJsonValues(Instance: TObject; W: TTextWriter); override;
  end;

  /// information about a RawUnicode published property
  TOrmPropInfoRttiRawUnicode = class(TOrmPropInfoRttiAnsi)
  protected
    procedure CopySameClassProp(Source: TObject; DestInfo: TOrmPropInfo;
      Dest: TObject); override;
  public
    procedure SetValue(Instance: TObject; Value: PUtf8Char; ValueLen: PtrInt;
      wasString: boolean); override;
    procedure SetValueVar(Instance: TObject; const Value: RawUtf8;
      wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSql: boolean; var result: RawUtf8;
      wasSqlString: PBoolean); override;
    function CompareValue(Item1, Item2: TObject; CaseInsensitive: boolean): integer; override;
    function GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal; override;
  end;

  /// information about a RawBlob published property
  TOrmPropInfoRttiRawBlob = class(TOrmPropInfoRttiAnsi)
  protected
    procedure CopySameClassProp(Source: TObject; DestInfo: TOrmPropInfo;
      Dest: TObject); override;
  public
    procedure SetValue(Instance: TObject; Value: PUtf8Char; ValueLen: PtrInt;
      wasString: boolean); override;
    procedure SetValueVar(Instance: TObject; const Value: RawUtf8;
      wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSql: boolean; var result: RawUtf8;
      wasSqlString: PBoolean); override;
    procedure GetFieldSqlVar(Instance: TObject; var aValue: TSqlVar; var temp:
      RawByteString); override;
    function SetFieldSqlVar(Instance: TObject; const aValue: TSqlVar): boolean; override;
    function CompareValue(Item1, Item2: TObject; CaseInsensitive: boolean): integer; override;
    function GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal; override;
    procedure GetJsonValues(Instance: TObject; W: TTextWriter); override;
    procedure GetBlob(Instance: TObject; var Blob: RawByteString);
    procedure SetBlob(Instance: TObject; const Blob: RawByteString);
    function IsNull(Instance: TObject): boolean;
  end;
  TOrmPropInfoRttiRawBlobDynArray = array of TOrmPropInfoRttiRawBlob;

  /// information about a WideString published property
  TOrmPropInfoRttiWide = class(TOrmPropInfoRtti)
  protected
    procedure CopySameClassProp(Source: TObject; DestInfo: TOrmPropInfo;
      Dest: TObject); override;
  public
    procedure SetValue(Instance: TObject; Value: PUtf8Char; ValueLen: PtrInt;
      wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSql: boolean; var result: RawUtf8;
      wasSqlString: PBoolean); override;
    procedure CopyValue(Source, Dest: TObject); override;
    procedure GetBinary(Instance: TObject; W: TBufferWriter); override;
    procedure SetBinary(Instance: TObject; var Read: TFastReader); override;
    procedure GetJsonValues(Instance: TObject; W: TTextWriter); override;
    function CompareValue(Item1, Item2: TObject; CaseInsensitive: boolean): integer; override;
    function GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal; override;
  end;

  {$ifdef HASVARUSTRING}
  /// information about a UnicodeString published property
  TOrmPropInfoRttiUnicode = class(TOrmPropInfoRtti)
  protected
    procedure CopySameClassProp(Source: TObject; DestInfo: TOrmPropInfo;
      Dest: TObject); override;
  public
    procedure SetValue(Instance: TObject; Value: PUtf8Char; ValueLen: PtrInt;
      wasString: boolean); override;
    procedure SetValueVar(Instance: TObject; const Value: RawUtf8;
      wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSql: boolean; var result: RawUtf8;
      wasSqlString: PBoolean); override;
    procedure CopyValue(Source, Dest: TObject); override;
    procedure GetBinary(Instance: TObject; W: TBufferWriter); override;
    procedure SetBinary(Instance: TObject; var Read: TFastReader); override;
    procedure GetJsonValues(Instance: TObject; W: TTextWriter); override;
    procedure GetFieldSqlVar(Instance: TObject; var aValue: TSqlVar;
      var temp: RawByteString); override;
    function SetFieldSqlVar(Instance: TObject; const aValue: TSqlVar): boolean; override;
    function CompareValue(Item1, Item2: TObject; CaseInsensitive: boolean): integer; override;
    function GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal; override;
  end;
  {$endif HASVARUSTRING}

  /// information about a dynamic array published property
  TOrmPropInfoRttiDynArray = class(TOrmPropInfoRtti)
  protected
    fObjArray: TRttiJson;
    function GetDynArrayElemType: TRttiCustom;
      {$ifdef HASINLINE}inline;{$endif}
    /// will create TDynArray.SaveTo by default, or JSON if is T*ObjArray
    procedure Serialize(Instance: TObject; var data: RawByteString;
      ExtendedJson: boolean); virtual;
    procedure CopySameClassProp(Source: TObject; DestInfo: TOrmPropInfo;
      Dest: TObject); override;
  public
    /// initialize the internal fields
    // - should not be called directly, but with dedicated class methods like
    // global function TOrmPropInfoCreateFrom()
    constructor Create(aPropInfo: PRttiProp; aPropIndex: integer;
      aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions); override;
    procedure GetDynArray(Instance: TObject; var result: TDynArray);
      {$ifdef HASINLINE}inline;{$endif}
    procedure SetValue(Instance: TObject; Value: PUtf8Char; ValueLen: PtrInt;
      wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSql: boolean; var result: RawUtf8;
      wasSqlString: PBoolean); override;
    procedure GetFieldSqlVar(Instance: TObject; var aValue: TSqlVar;
      var temp: RawByteString); override;
    function SetFieldSqlVar(Instance: TObject; const aValue: TSqlVar): boolean; override;
    procedure GetBinary(Instance: TObject; W: TBufferWriter); override;
    procedure SetBinary(Instance: TObject; var Read: TFastReader); override;
    function CompareValue(Item1, Item2: TObject; CaseInsensitive: boolean): integer; override;
    function GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal; override;
    procedure NormalizeValue(var Value: RawUtf8); override;
    procedure GetJsonValues(Instance: TObject; W: TTextWriter); override;
    procedure GetVariant(Instance: TObject; var Dest: Variant); override;
    procedure SetVariant(Instance: TObject; const Source: Variant); override;
    /// optional index of the dynamic array published property
    // - used e.g. for fast lookup by TOrm.DynArray(DynArrayFieldIndex)
    property DynArrayIndex: integer
      read fFieldWidth;
    /// read-only access to the low-level type information the array item type
    property DynArrayElemType: TRttiCustom
      read GetDynArrayElemType;
    /// dynamic array item information for a T*ObjArray
    // - equals nil if this dynamic array was not previously registered via
    // Rtti.RegisterObjArray() on Delphi 7-2009
    // - note that if the field is a T*ObjArray, you could create a new item
    // by calling ObjArray^.ClassNewInstance
    // - T*ObjArray database column will be stored as text
    property ObjArray: TRttiJson
      read fObjArray;
  end;

  TOrmPropInfoRttiDynArrayObjArray = array of TOrmPropInfoRttiDynArray;

  /// information about a variant published property
  // - is also used for TNullable* properties
  TOrmPropInfoRttiVariant = class(TOrmPropInfoRtti)
  protected
    fDocVariantOptions: TDocVariantOptions;
    procedure CopySameClassProp(Source: TObject; DestInfo: TOrmPropInfo;
      Dest: TObject); override;
  public
    /// initialize the internal fields
    constructor Create(aPropInfo: PRttiProp; aPropIndex: integer;
      aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions); override;
    procedure SetValue(Instance: TObject; Value: PUtf8Char; ValueLen: PtrInt;
      wasString: boolean); override;
    procedure SetValueVar(Instance: TObject; const Value: RawUtf8; wasString: boolean); override;
    procedure SetValuePtr(Instance: TObject; Value: PUtf8Char; ValueLen: integer;
      wasString: boolean);
    procedure GetValueVar(Instance: TObject; ToSql: boolean; var result: RawUtf8;
      wasSqlString: PBoolean); override;
    procedure GetBinary(Instance: TObject; W: TBufferWriter); override;
    procedure SetBinary(Instance: TObject; var Read: TFastReader); override;
    function CompareValue(Item1, Item2: TObject; CaseInsensitive: boolean): integer; override;
    function GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal; override;
    procedure NormalizeValue(var Value: RawUtf8); override;
    procedure GetJsonValues(Instance: TObject; W: TTextWriter); override;
    procedure GetVariant(Instance: TObject; var Dest: Variant); override;
    procedure SetVariant(Instance: TObject; const Source: Variant); override;
    /// how this property will deal with its instances (including TDocVariant)
    // - by default, contains JSON_FAST for best performance - i.e.
    // [dvoReturnNullForUnknownProperty,dvoValueCopiedByReference]
    // - set JSON_FAST_EXTENDED (or include dvoSerializeAsExtendedJson)
    // so that any TDocVariant nested field names will not be double-quoted,
    // saving some chars in the stored TEXT column and in the JSON escaped
    // transmitted data over REST, by writing '{name:"John",age:123}' instead of
    // '{"name":"John","age":123}': be aware that this syntax is supported by
    // the ORM, SOA, TDocVariant, TBsonVariant, and our SynCrossPlatformJSON
    // unit, but not AJAX/JavaScript or most JSON libraries
    // - see also TOrmModel/TOrmProperties.SetVariantFieldsDocVariantOptions
    property DocVariantOptions: TDocVariantOptions
      read fDocVariantOptions write fDocVariantOptions;
  end;

  /// optional event handler used by TOrmPropInfoRecord to handle textual storage
  // - by default, TOrmPropInfoRecord content will be stored as oftBlobCustom;
  // specify such a callback event to allow storage as UTF-8 textual field and
  // use a oftUtf8Custom kind of column
  // - event implementation shall convert data/datalen binary value into Text
  TOnSqlPropInfoRecord2Text = procedure(Data: pointer; DataLen: integer;
    var Text: RawUtf8);

  /// optional event handler used by TOrmPropInfoRecord to handle textual storage
  // - by default, TOrmPropInfoRecord content will be stored as oftBlobCustom;
  // specify such a callback event to allow storage as UTF-8 textual field and
  // use a oftUtf8Custom kind of column
  // - event implementaiton shall convert Text into Data binary value
  TOnSqlPropInfoRecord2Data = procedure(Text: PUtf8Char; var Data: RawByteString);

  /// abstract information about a record-like property defined directly in code
  // - do not use this class, but TOrmPropInfoRecordRtti and TOrmPropInfoRecordFixedSize
  // - will store the content as BLOB by default, and OrmFieldType as oftBlobCustom
  // - if aData2Text/aText2Data are defined, use TEXT storage and oftUtf8Custom type
  TOrmPropInfoCustom = class(TOrmPropInfo)
  protected
    fOffset: PtrUInt;
    fData2Text: TOnSqlPropInfoRecord2Text;
    fText2Data: TOnSqlPropInfoRecord2Data;
    procedure BinaryToText(var Value: RawUtf8; ToSql: boolean;
      wasSqlString: PBoolean); override;
    procedure TextToBinary(Value: PUtf8Char; var result: RawByteString); override;
  public
    /// define a custom property in code
    // - do not call this constructor directly, but one of its inherited classes,
    // via a call to TOrmProperties.RegisterCustom*()
    constructor Create(const aName: RawUtf8; aOrmFieldType: TOrmFieldType;
      aAttributes: TOrmPropInfoAttributes; aFieldWidth, aPropIndex: integer;
      aProperty: pointer; aData2Text: TOnSqlPropInfoRecord2Text;
      aText2Data: TOnSqlPropInfoRecord2Data); reintroduce;
  public
    function GetFieldAddr(Instance: TObject): pointer; override;
  end;
  TOrmPropInfoCustomDynArray = array of TOrmPropInfoCustom;

  /// information about a record property defined directly in code using RTTI
  TOrmPropInfoRecordTyped = class(TOrmPropInfoCustom)
  protected
    fTypeInfo: PRttiInfo;
  public
    property TypeInfo: PRttiInfo
      read fTypeInfo;
  end;

  /// information about a record property defined directly in code
  // - Delphi does not publish RTTI for published record properties
  // - you can use this class to register a record property from its RTTI
  // - will store the content as BLOB by default, and OrmFieldType as oftBlobCustom
  // - if aData2Text/aText2Data are defined, use TEXT storage and oftUtf8Custom type
  // - this class will use only binary RecordLoad/RecordSave methods
  TOrmPropInfoRecordRtti = class(TOrmPropInfoRecordTyped)
  protected
    function GetSqlFieldRttiTypeName: RawUtf8; override;
    procedure CopySameClassProp(Source: TObject; DestInfo: TOrmPropInfo;
      Dest: TObject); override;
  public
    /// define a record property from its RTTI definition
    // - handle any kind of record with available generated TypeInfo()
    // - aPropertyPointer shall be filled with the offset to the private
    // field within a nil object, e.g for
    // !  class TMainObject = class(TOrm)
    // !    (...)
    // !    fFieldName: TMyRecord;
    // !  public
    // !    (...)
    // !    property FieldName: TMyRecord read fFieldName write fFieldName;
    // !  end;
    // you will have to register it via a call to
    // TOrmProperties.RegisterCustomRttiRecordProperty()
    // - optional aIsNotUnique parametercanl be defined
    // - implementation will use internally RecordLoad/RecordSave functions
    // - you can specify optional aData2Text/aText2Data callbacks to store
    // the content as textual values, and not as BLOB
    constructor Create(aRecordInfo: PRttiInfo; const aName: RawUtf8;
      aPropertyIndex: integer; aPropertyPointer: pointer;
      aAttributes: TOrmPropInfoAttributes = []; aFieldWidth: integer = 0;
      aData2Text: TOnSqlPropInfoRecord2Text = nil;
      aText2Data: TOnSqlPropInfoRecord2Data = nil); reintroduce; overload;
  public
    procedure SetValue(Instance: TObject; Value: PUtf8Char; ValueLen: PtrInt;
      wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSql: boolean; var result: RawUtf8;
      wasSqlString: PBoolean); override;
    procedure GetFieldSqlVar(Instance: TObject; var aValue: TSqlVar;
      var temp: RawByteString); override;
    function SetFieldSqlVar(Instance: TObject; const aValue: TSqlVar): boolean; override;
    procedure GetBinary(Instance: TObject; W: TBufferWriter); override;
    procedure SetBinary(Instance: TObject; var Read: TFastReader); override;
    function CompareValue(Item1, Item2: TObject; CaseInsensitive: boolean): integer; override;
    function GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal; override;
    procedure NormalizeValue(var Value: RawUtf8); override;
    procedure GetVariant(Instance: TObject; var Dest: Variant); override;
    procedure SetVariant(Instance: TObject; const Source: Variant); override;
  end;

  /// information about a fixed-size record property defined directly in code
  // - Delphi does not publish RTTI for published record properties
  // - you can use this class to register a record property with no RTTI (i.e.
  // a record with no reference-counted types within)
  // - will store the content as BLOB by default, and OrmFieldType as oftBlobCustom
  // - if aData2Text/aText2Data are defined, use TEXT storage and oftUtf8Custom type
  TOrmPropInfoRecordFixedSize = class(TOrmPropInfoRecordTyped)
  protected
    fRecordSize: integer;
    function GetSqlFieldRttiTypeName: RawUtf8; override;
    procedure CopySameClassProp(Source: TObject; DestInfo: TOrmPropInfo;
      Dest: TObject); override;
  public
    /// define an unmanaged fixed-size record property
    // - simple kind of records (i.e. those not containing reference-counted
    // members) do not have RTTI generated, at least in older versions of Delphi:
    // use this constructor to define a direct property access
    // - main parameter is the record size, in bytes
    constructor Create(aRecordSize: cardinal; const aName: RawUtf8;
      aPropertyIndex: integer; aPropertyPointer: pointer;
      aAttributes: TOrmPropInfoAttributes = []; aFieldWidth: integer = 0;
      aData2Text: TOnSqlPropInfoRecord2Text = nil;
      aText2Data: TOnSqlPropInfoRecord2Data = nil); reintroduce; overload;
  public
    procedure SetValue(Instance: TObject; Value: PUtf8Char; ValueLen: PtrInt;
      wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSql: boolean; var result: RawUtf8;
      wasSqlString: PBoolean); override;
    procedure GetFieldSqlVar(Instance: TObject; var aValue: TSqlVar;
      var temp: RawByteString); override;
    function SetFieldSqlVar(Instance: TObject; const aValue: TSqlVar): boolean; override;
    procedure GetBinary(Instance: TObject; W: TBufferWriter); override;
    procedure SetBinary(Instance: TObject; var Read: TFastReader); override;
    function CompareValue(Item1, Item2: TObject; CaseInsensitive: boolean): integer; override;
    function GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal; override;
    procedure NormalizeValue(var Value: RawUtf8); override;
    procedure GetVariant(Instance: TObject; var Dest: Variant); override;
    procedure SetVariant(Instance: TObject; const Source: Variant); override;
  end;

  /// information about a custom property defined directly in code
  // - you can define any kind of property, either a record or any type
  // - this class will use JSON serialization, by type name or TypeInfo() pointer
  // - will store the content as TEXT by default, and OrmFieldType as oftUtf8Custom
  TOrmPropInfoCustomJson = class(TOrmPropInfoRecordTyped)
  protected
    fCustomParser: TRttiJson;
    function GetSqlFieldRttiTypeName: RawUtf8; override;
    procedure SetCustomParser(aCustomParser: TRttiJson);
  public
    /// initialize the internal fields
    // - should not be called directly
    constructor Create(aPropInfo: PRttiProp; aPropIndex: integer);
      reintroduce; overload; virtual;
    /// define a custom property from its RTTI definition
    // - handle any kind of property, e.g. from enhanced RTTI or a custom record
    // defined by Rtti.RegisterFromText/TRttiJson.RegisterCustomSerializer
    // - aPropertyPointer shall be filled with the offset to the private
    // field within a nil object, e.g for
    // !  class TMainObject = class(TOrm)
    // !    (...)
    // !    fFieldName: TMyRecord;
    // !  public
    // !    (...)
    // !    property FieldName: TMyRecord read fFieldName write fFieldName;
    // !  end;
    // you will have to register it via a call to
    // TOrmProperties.RegisterCustomPropertyFromRtti()
    // - optional aIsNotUnique parameter can be defined
    // - implementation will use internally RecordLoadJson/RecordSave functions
    // - you can specify optional aData2Text/aText2Data callbacks to store
    // the content as textual values, and not as BLOB
    constructor Create(aTypeInfo: PRttiInfo; const aName: RawUtf8;
      aPropertyIndex: integer; aPropertyPointer: pointer;
      aAttributes: TOrmPropInfoAttributes = []; aFieldWidth: integer = 0);
      reintroduce; overload;
    /// define a custom property from its RTTI definition
    // - handle any kind of property, e.g. from enhanced RTTI or a custom record
    // defined by Rtti.RegisterFromText/TRttiJson.RegisterCustomSerializer
    // - aPropertyPointer shall be filled with the offset to the private
    // field within a nil object, e.g for
    // !  class TMainObject = class(TOrm)
    // !    (...)
    // !    fGUID: TGUID;
    // !  public
    // !    (...)
    // !    property GUID: TGUID read fGUID write fGUID;
    // !  end;
    // you will have to register it via a call to
    // TOrmProperties.RegisterCustomPropertyFromTypeName()
    // - optional aIsNotUnique parameter can be defined
    // - implementation will use internally RecordLoadJson/RecordSave functions
    // - you can specify optional aData2Text/aText2Data callbacks to store
    // the content as textual values, and not as BLOB
    constructor Create(const aTypeName, aName: RawUtf8; aPropertyIndex: integer;
      aPropertyPointer: pointer; aAttributes: TOrmPropInfoAttributes = [];
      aFieldWidth: integer = 0); reintroduce; overload;
    /// the corresponding custom JSON parser
    property CustomParser: TRttiJson
      read fCustomParser;
  public
    procedure SetValue(Instance: TObject; Value: PUtf8Char; ValueLen: PtrInt;
      wasString: boolean); override;
    procedure GetJsonValues(Instance: TObject; W: TTextWriter); override;
    procedure GetValueVar(Instance: TObject; ToSql: boolean; var result: RawUtf8;
      wasSqlString: PBoolean); override;
    procedure GetBinary(Instance: TObject; W: TBufferWriter); override;
    procedure SetBinary(Instance: TObject; var Read: TFastReader); override;
    procedure NormalizeValue(var Value: RawUtf8); override;
  end;

  /// dynamic array of ORM fields information for published properties
  TOrmPropInfoObjArray = array of TOrmPropInfo;

  /// abstract information about a TOrm class property
  // - oftID for TOrm properties, which are pointer(RecordID), not
  // any true class instance
  // - oftMany for TOrmMany properties, for which no data is
  // stored in the table itself, but in a pivot table
  // - oftObject for e.g. TStrings TRawUtf8List TCollection instances
  TOrmPropInfoRttiInstance = class(TOrmPropInfoRttiPtrInt)
  protected
    fObjectClass: TClass;
  public
    /// will setup the corresponding ObjectClass property
    constructor Create(aPropInfo: PRttiProp; aPropIndex: integer;
      aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions); override;
    /// direct access to the property class instance
    function GetInstance(Instance: TObject): TObject;
      {$ifdef HASINLINE}inline;{$endif}
    /// direct access to the property class instance
    procedure SetInstance(Instance, Value: TObject);
      {$ifdef HASINLINE}inline;{$endif}
    /// direct access to the property class
    // - can be used e.g. for TOrmMany properties
    property ObjectClass: TClass
      read fObjectClass;
  end;

  /// information about a TRecordReference/TRecordReferenceToBeDeleted
  // published property
  // - identified as a oftRecord kind of property
  TOrmPropInfoRttiRecordReference = class(TOrmPropInfoRttiInt64)
  protected
    fCascadeDelete: boolean;
  public
    /// will identify TRecordReferenceToBeDeleted kind of field, and
    // setup the corresponding CascadeDelete property
    constructor Create(aPropInfo: PRttiProp; aPropIndex: integer;
      aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions); override;
    /// TRUE if this oftRecord is a TRecordReferenceToBeDeleted
    property CascadeDelete: boolean
      read fCascadeDelete;
  end;

  TOrmPropInfoRttiRecordReferenceClass = class of TOrmPropInfoRttiRecordReference;

  /// information about a TRecordVersion published property
  // - identified as a oftRecordVersion kind of property, to track changes
  TOrmPropInfoRttiRecordVersion = class(TOrmPropInfoRttiInt64);

  /// information about a TOrm class TStrings/TRawUtf8List/TCollection
  // property
  // - kind oftObject e.g. for TStrings TRawUtf8List TCollection TObjectList instances
  // - binary serialization will store textual JSON serialization of the
  // object, including custom serialization
  TOrmPropInfoRttiObject = class(TOrmPropInfoRttiInstance)
  protected
    procedure CopySameClassProp(Source: TObject; DestInfo: TOrmPropInfo;
      Dest: TObject); override;
  public
    procedure SetValue(Instance: TObject; Value: PUtf8Char; ValueLen: PtrInt;
      wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSql: boolean; var result: RawUtf8;
      wasSqlString: PBoolean); override;
    procedure GetBinary(Instance: TObject; W: TBufferWriter); override;
    procedure SetBinary(Instance: TObject; var Read: TFastReader); override;
    function GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal; override;
    procedure NormalizeValue(var Value: RawUtf8); override;
    procedure GetJsonValues(Instance: TObject; W: TTextWriter); override;
  end;

  /// information about a TOrm class TOrmMany property
  // - kind oftMany, for which no data is stored in the table itself, but in
  // a separated pivot table
  TOrmPropInfoRttiMany = class(TOrmPropInfoRttiInstance)
  public
    procedure SetValue(Instance: TObject; Value: PUtf8Char; ValueLen: PtrInt;
      wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSql: boolean; var result: RawUtf8;
      wasSqlString: PBoolean); override;
    procedure GetBinary(Instance: TObject; W: TBufferWriter); override;
    procedure SetBinary(Instance: TObject; var Read: TFastReader); override;
  end;

  TOrmPropInfoRttiManyObjArray = array of TOrmPropInfoRttiMany;
  POrmPropInfoRttiMany = ^TOrmPropInfoRttiMany;

  /// handle a read-only list of fields information for published properties
  // - is mainly used by our ORM for TOrm RTTI, but may be used for
  // any TPersistent
  TOrmPropInfoList = class
  protected
    fList: TOrmPropInfoObjArray;
    fCount: integer;
    fTable: TClass; // TOrmClass
    fOptions: TOrmPropInfoListOptions;
    fOrderedByName: TByteDynArray;
    function GetItem(aIndex: PtrInt): TOrmPropInfo;
    procedure QuickSortByName(L, R: PtrInt);
    procedure InternalAddParentsFirst(aClassType: TClass); overload;
    procedure InternalAddParentsFirst(aClassType: TClass;
      aFlattenedProps: PRttiPropDynArray); overload;
  public
    /// initialize the list from a given class RTTI
    constructor Create(aTable: TClass; aOptions: TOrmPropInfoListOptions);
    /// release internal list items
    destructor Destroy; override;
    /// add a TOrmPropInfo to the list
    function Add(aItem: TOrmPropInfo): integer;
    /// set the length of the internal List[] array and the sorted names
    procedure AfterAdd;
    /// find an item in the list using O(log(n)) binary search
    // - returns nil if not found
    function ByRawUtf8Name(const aName: RawUtf8): TOrmPropInfo; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// find an item in the list using O(log(n)) binary search
    // - returns nil if not found
    function ByName(aName: PUtf8Char): TOrmPropInfo; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// find an item in the list using O(log(n)) binary search
    // - returns -1 if not found
    function IndexByName(const aName: RawUtf8): integer; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// find an item in the list using O(log(n)) binary search
    // - returns -1 if not found
    function IndexByName(aName: PUtf8Char): PtrInt; overload;
    /// find an item by name in the list, including RowID/ID
    // - will identify 'ID' / 'RowID' field name as -1
    // - raise an EOrmException if not found in the internal list
    function IndexByNameOrExcept(const aName: RawUtf8): integer;
    /// find an item by name in the list, including RowID/ID
    // - will identify 'ID' / 'RowID' field name as -1
    // - raise an EOrmException if not found in the internal list
    // - aName is not defined as "const aName" since it is made an ASCIIZ
    function IndexByNameOrExceptShort(aName: ShortString): integer;
    /// find one or several items by name in the list, including RowID/ID
    // - will identify 'ID' / 'RowID' field name as -1
    // - raise an EOrmException if not found in the internal list
    procedure IndexesByNamesOrExcept(const aNames: array of RawUtf8;
      const aIndexes: array of PInteger);
    /// find an item in the list, searching by unflattened name
    // - for a flattened property, you may for instance call
    // IndexByNameUnflattenedOrExcept('Address.Country.Iso')
    // instead of IndexByNameOrExcept('Address_Country')
    // - won't identify 'ID' / 'RowID' field names, just List[].
    // - raise an EOrmException if not found in the internal list
    function IndexByNameUnflattenedOrExcept(const aName: RawUtf8): integer;
    /// fill a TRawUtf8DynArray instance from the field names
    // - excluding ID
    procedure NamesToRawUtf8DynArray(var Names: TRawUtf8DynArray);
    /// returns the number of TOrmPropInfo in the list
    property Count: integer
      read fCount;
    /// quick access to the TOrmPropInfo list
    // - note that length(List) may not equal Count, since is its capacity
    property List: TOrmPropInfoObjArray
      read fList;
    /// read-only retrieval of a TOrmPropInfo item
    // - will raise an exception if out of range
    property Items[aIndex: PtrInt]: TOrmPropInfo
      read GetItem;
  end;


var
  /// global thread-safe list as used by TOrmPropInfoRtti.RegisterTypeInfo
  OrmPropInfoRegistration: TSynDictionary = nil;

var
  // internal ClassOrmFieldType() table - mormot.orm.core will inject TOrm/TOrmMany
  CLASSORMFIELDTYPELIST: array[0..6] of TClass = (
    TObject, nil{TOrmMany}, nil{TOrm}, TRawUtf8List, TStrings, TObjectList, TCollection);

  // injected by mormot.orm.core
  TOrmPropInfoRttiIDClass, TOrmPropInfoRttiTIDClass: TOrmPropInfoRttiClass;

/// get the SQL type of this class type
// - returns either oftObject, oftID, oftMany or oftUnknown
function ClassOrmFieldType(info: PRttiInfo): TOrmFieldType;

/// get the SQL type of this type, as managed with the database driver
function GetOrmFieldType(Info: PRttiInfo): TOrmFieldType;


{ ************ Abstract TOrmTableAbstract Parent Class }

type
  /// exception raised in case of incorrect TOrmTable.Step / Field*() use
  EOrmTable = class(ESynException);

  TOrmTableAbstract = class;

  /// allow on-the-fly translation of a TOrmTable grid value
  // - should return valid JSON value of the given cell (i.e. quoted strings,
  // or valid JSON object/array) unless HumanFriendly is defined
  // - e.g. TOrmTable.OnExportValue property will customize TOrmTable's
  // GetJsonValues, GetHtmlTable, and GetCsvValues methods returned content
  TOnOrmTableGetValue = function(Sender: TOrmTableAbstract; Row, Field: integer;
    HumanFriendly: boolean): RawJson of object;

  /// store TOrmFieldType and RTTI for a given TOrmTable field
  TOrmTableFieldType = record
    /// the field kind, as in JSON (match TOrmPropInfo.OrmFieldTypeStored)
    ContentType: TOrmFieldType;
    /// how this field could be stored in a database
    // - equals ftUnknown if InitFields guessed the field type, or for oftVariant
    ContentDB: TSqlDBFieldType;
    /// the field size in bytes; -1 means not computed yet
    ContentSize: integer;
    /// the field low-level RTTI information
    // - is a PRttiInfo for oftBlobDynArray/oftNullable,
    // or is a PRttiEnumType for oftEnumerate/oftSet, or nil
    ContentTypeInfo: pointer;
    /// the corresponding index in fQueryTables[]
    TableIndex: integer;
  end;

  POrmTableFieldType = ^TOrmTableFieldType;

  {$ifdef NOPOINTEROFFSET}
  TOrmTableDataArray = PPUtf8CharArray;
  TOrmTableJsonDataArray = TPUtf8CharDynArray;
  {$else} // reduce memory consumption by half on 64-bit CPUs
  TOrmTableDataArray = PIntegerArray; // 0 = nil, or offset in fDataStart[]
  TOrmTableJsonDataArray = TIntegerDynArray;
  {$endif NOPOINTEROFFSET}

  /// abstract parent to TOrmTable and TOrmTableJson classes
  // - will be fully implemented as TOrmTableJson holding JSON content
  TOrmTableAbstract = class
  protected
    fRowCount: PtrInt;
    fFieldCount: PtrInt;
    fData: TOrmTableDataArray;
    {$ifndef NOPOINTEROFFSET} // reduce memory consumption by half on 64-bit CPU
    fDataStart: PUtf8Char;
    {$endif NOPOINTEROFFSET}
    {$ifndef NOTORMTABLELEN}
    fLen: TIntegerDynArray;
    {$endif NOTORMTABLELEN}
    fFieldType: array of TOrmTableFieldType;
    fFieldTypeAllRows: boolean;
    fOwnerMustFree: boolean; // if owner TOrm should free it
    fFieldIndexID: integer; // index of a 'ID' field, -1 if none
    fStepRow: integer; // current 1..RowCount position during Step() process
    fInternalState: cardinal; // DB state counter when the data was retrieved
    fFieldNames: TRawUtf8DynArray;
    fFieldNameOrder: TCardinalDynArray; // O(log(n)) bin search for FieldIndex()
    fSortParams: TOrmTableSortParams; // last sorting parameters (for re-sort)
    fQueryColumnTypes: array of TOrmFieldType;
    fQuerySql: RawUtf8; // any associated SQL statement, set by Create()
    fQueryTableNameFromSql: RawUtf8;
    fQueryTableIndexFromSql: integer; // -2=nosearch -1=notfound fQueryTables[0..n]
    fFieldLengthMeanSum: integer;
    fFieldLengthMean: TIntegerDynArray;
    fFieldParsedAsString: set of 0..255; // set at parsing at wasstring=true
    fOnExportValue: TOnOrmTableGetValue;
    fOwnedRecords: TSynObjectList; // holds NewRecord() returned instances
    function GetResults(Offset: PtrInt): PUtf8Char; // low-level data access
      {$ifdef HASINLINE}inline;{$endif}
    function GetResultsLen(Offset: PtrInt): integer; overload;
      {$ifdef HASINLINE}inline;{$endif}
    function GetResultsLen(Offset: PtrInt; P: PUtf8Char): integer; overload;
      {$ifdef HASINLINE}inline;{$endif}
    procedure SetResults(Offset: PtrInt; Value: PUtf8Char; ValueLen: integer);
      {$ifdef HASINLINE}inline;{$endif}
    procedure SetResultsSafe(Offset: PtrInt; Value: PUtf8Char);
      {$ifdef HASINLINE}{$ifdef NOPOINTEROFFSET}inline;{$endif}{$endif}
    function GetRowCount: PtrInt; // avoid GPF when TOrmTable is nil
      {$ifdef HASINLINE}inline;{$endif}
    function InitOneFieldType(field: PtrInt; out size: integer;
      out info: PRttiInfo; out tableindex: integer): TOrmFieldType; virtual;
    procedure InitFieldTypes; // fill fFieldType[] from QueryTables[]/Data[]
    procedure InitFieldNames; // fill fFieldNames[] from first row
    function GetQueryTableNameFromSql: RawUtf8;
  public
    /// initialize the result table
    // - you can optionaly associate the corresponding TOrmClass types,
    // by which the results were computed (it will use RTTI for column typing)
    constructor Create(const aSql: RawUtf8);
    /// initialize the result table
    // - you can set the expected column types matching the results column layout
    constructor CreateWithColumnTypes(const ColumnTypes: array of TOrmFieldType;
      const aSql: RawUtf8);
    /// free associated memory and owned records
    destructor Destroy; override;
    /// copy basic reading parameters of a TOrmTable into this instance
    // - only work for basic Get() or Results[] access
    // - the Results[] remain in the source TOrmTable: source TOrmTable has to
    // remain available (i.e. not destroyed) before this TOrmTable
    procedure AssignRead(source: TOrmTableAbstract); virtual;

    /// read-only access to the ID of a particular row
    // - returns 0 if no RowID field exists (i.e. FieldIndexID < 0)
    function GetID(Row: PtrInt): TID;
    /// read-only access to a particular field value, as UTF-8 encoded buffer
    // - if Row and Fields are correct, returns a pointer to the UTF-8 buffer,
    // or nil if the corresponding JSON was null or ""
    // - if Row and Fields are not correct, returns nil
    function Get(Row, Field: PtrInt): PUtf8Char; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read-only access to a particular field UTF-8 value and length
    function Get(Row, Field: PtrInt; out Len: integer): PUtf8Char; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read-only access to a particular field value, as RawUtf8 text
    function GetU(Row, Field: PtrInt): RawUtf8; overload;
    /// read-only access to a particular field value, as UTF-8 encoded buffer
    // - points to memory buffer allocated by Init()
    function Get(Row: PtrInt; const FieldName: RawUtf8): PUtf8Char; overload;
    /// read-only access to a particular field value, as RawUtf8 text
    function GetU(Row: PtrInt; const FieldName: RawUtf8): RawUtf8; overload;
    /// read-only access to a particular field value, as Win Ansi text
    function GetA(Row, Field: PtrInt): WinAnsiString;
    /// read-only access to a particular field value, as Win Ansi text shortstring
    function GetS(Row, Field: PtrInt): shortstring;
    /// read-only access to a particular field value, as a Variant
    // - text will be stored as RawUtf8 (as varString type)
    // - will try to use the most approriate Variant type for conversion (will
    // use e.g. TDateTime for oftDateTime, or a TDocVariant for JSON objects
    // in a oftVariant column) - so you should better set the exact field types
    // (e.g. from ORM) before calling this method
    function GetVariant(Row, Field: PtrInt): variant; overload;
    /// read-only access to a particular field value, as a Variant
    // - text will be stored as RawUtf8 (as varString type)
    // - will try to use the most approriate Variant type for conversion (will
    // use e.g. TDateTime for oftDateTime, or a TDocVariant for JSON objects
    // in a oftVariant column) - so you should better set the exact field types
    // (e.g. from ORM) before calling this method
    procedure GetVariant(Row, Field: PtrInt; var result: variant); overload;
    /// read-only access to a particular field, via a lookup field name
    // - will call GetVariant() on the corresponding field
    // - returns null if the lookup did not have any match
    function GetValue(const aLookupFieldName, aLookupValue, aValueFieldName: RawUtf8): variant;
    /// read-only access to a particular field value, as VCL string text
    // - the global Utf8ToString() function will be used for the conversion:
    // for proper i18n handling before Delphi 2009, you should use the
    // overloaded method with aUtf8ToString=Language.Utf8ToString
    function GetString(Row, Field: PtrInt): string;
    /// read-only access to a particular field value, as fast Unicode string text
    // - SynUnicode is either WideString, either UnicodeString, depending on the
    // Delphi compiler revision, to ensure fastest native Unicode process available
    function GetSynUnicode(Row, Field: PtrInt): SynUnicode;
    /// fill a unicode buffer with a particular field value
    // - return number of wide characters written in Dest^
    function GetWP(Row, Field: PtrInt; Dest: PWideChar; MaxDestChars: cardinal): integer;
    /// read-only access to a particular field value, as UTF-16 Unicode text
    // - Raw Unicode is WideChar(zero) terminated
    // - its content is allocated to contain all WideChars (not trimed to 255,
    // like GetWP() above
    function GetW(Row, Field: PtrInt): RawUnicode;
    /// read-only access to a particular field value, as integer value
    function GetAsInteger(Row, Field: PtrInt): integer; overload;
    /// read-only access to a particular field value, as integer value
    function GetAsInteger(Row: PtrInt; const FieldName: RawUtf8): integer; overload;
    /// read-only access to a particular field value, as Int64 value
    function GetAsInt64(Row, Field: PtrInt): Int64; overload;
    /// read-only access to a particular field value, as Int64 value
    function GetAsInt64(Row: PtrInt; const FieldName: RawUtf8): Int64; overload;
    /// read-only access to a particular field value, as extended value
    function GetAsFloat(Row, Field: PtrInt): TSynExtended; overload;
    /// read-only access to a particular field value, as extended value
    function GetAsFloat(Row: PtrInt; const FieldName: RawUtf8): TSynExtended; overload;
    /// read-only access to a particular field value, as TDateTime value
    // - oftDateTime/oftDateTimeMS will be converted from ISO-8601 text
    // - oftTimeLog, oftModTime, oftCreateTime will expect the content to be
    // encoded as a TTimeLog Int64 value - as oftInteger may have been
    // identified by TOrmTable.InitFieldTypes
    // - oftUnixTime/oftUnixMSTime field will call UnixTimeToDateTime/UnixMSTimeToDateTime
    // - for oftTimeLog, oftModTime, oftCreateTime or oftUnixTime fields, you
    // may have to force the column type, since it may be identified as oftInteger
    // or oftCurrency by default from its JSON number content, e.g. via:
    // ! aTable.SetFieldType('FieldName', oftModTime);
    // - oftCurrency,oftFloat will return the corresponding double value
    // - any other types will try to convert ISO-8601 text }
    function GetAsDateTime(Row, Field: PtrInt): TDateTime; overload;
    /// read-only access to a particular field value, as TDateTime value
    function GetAsDateTime(Row: PtrInt; const FieldName: RawUtf8): TDateTime; overload;
    /// read-only access to a particular field value, as currency value
    function GetAsCurrency(Row, Field: PtrInt): currency; overload;
    /// read-only access to a particular field value, as currency value
    function GetAsCurrency(Row: PtrInt; const FieldName: RawUtf8): currency; overload;
    /// read-only access to a particular field value, ready to be displayed
    // - mostly used with Row=0, i.e. to get a display value from a field name
    // - use "string" type, i.e. UnicodeString for Delphi 2009+
    // - value is first un-camel-cased: 'OnLine' value will return 'On line' e.g.
    // - then System.LoadResStringTranslate() is called if available
    function GetCaption(Row, Field: PtrInt): string;
    /// read-only access to a particular Blob value
    // - a new RawBlob is created
    // - Blob data is converted from SQLite3 BLOB literals (X'53514C697465' e.g.)
    // or Base-64 encoded content ('\uFFF0base64encodedbinary')
    // - prefered manner is to directly use REST protocol to retrieve a blob field
    function GetBlob(Row, Field: PtrInt): RawBlob;
    /// read-only access to a particular Blob value
    // - a new TBytes is created
    // - Blob data is converted from SQLite3 BLOB literals (X'53514C697465' e.g.)
    //   or Base-64 encoded content ('\uFFF0base64encodedbinary')
    // - prefered manner is to directly use REST protocol to retrieve a blob field
    function GetBytes(Row, Field: PtrInt): TBytes;
    /// read-only access to a particular Blob value
    // - a new TCustomMemoryStream is created - caller shall free its instance
    // - Blob data is converted from SQLite3 BLOB literals (X'53514C697465' e.g.)
    //   or Base-64 encoded content ('\uFFF0base64encodedbinary')
    // - prefered manner is to directly use REST protocol to retrieve a blob field
    function GetStream(Row, Field: PtrInt): TStream;
    /// read-only access to a particular DateTime field value
    // - expect SQLite3 TEXT field in ISO 8601 'YYYYMMDD hhmmss' or
    // 'YYYY-MM-DD hh:mm:ss' format
    function GetDateTime(Row, Field: PtrInt): TDateTime;
    /// read-only access to a particular TTimeLog field value
    // - return the result as TTimeLogBits.Text() Iso-8601 encoded text
    function GetTimeLog(Row, Field: PtrInt; Expanded: boolean; FirstTimeChar:
      AnsiChar = 'T'): RawUtf8;
    /// widechar length (UTF-8 decoded as UTF-16) of a particular field value
    // - could be used with VCL's UnicodeString, or for Windows API
    function LengthW(Row, Field: PtrInt): integer;
    /// get all values for a specified field into a dynamic RawUtf8 array
    // - don't perform any conversion, but just create an array of raw PUtf8Char data
    // - returns the number of rows in Values[]
    function GetRowValues(Field: PtrInt; out Values: TRawUtf8DynArray): integer; overload;
    /// get all values for a specified field into a dynamic integer array
    // - returns the number of rows in Values[]
    function GetRowValues(Field: PtrInt; out Values: TInt64DynArray): integer; overload;
    /// get all values for a specified field as CSV
    // - don't perform any conversion, but create a CSV from raw PUtf8Char data
    function GetRowValues(Field: PtrInt; const Sep: RawUtf8 = ',';
      const Head: RawUtf8 = ''; const Trail: RawUtf8 = ''): RawUtf8; overload;
    /// get all values lengths for a specified field into a PIntegerArray
    // - returns the total length as result, and fill LenStore with all rows
    // individual lengths using StrLen() - caller should eventually call
    // LenStore.Done to release any temp memory
    // - returns 0 if Field is invalid or no data is stored in this TOrmTable -
    // don't call LenStore.Done in this case
    function GetRowLengths(Field: PtrInt; LenStore: PSynTempBuffer): integer;
    /// retrieve a field value as a variant
    // - returns null if the row/field is incorrect
    // - expand* methods will allow to return human-friendly representations
    procedure GetAsVariant(row, field: PtrInt; out value: variant;
      expandTimeLogAsText, expandEnumsAsText, expandHugeIDAsUniqueIdentifier: boolean;
      options: TDocVariantOptions = JSON_FAST);

    /// retrieve a row value as a variant, ready to be accessed via late-binding
    // - Row parameter numbering starts from 1 to RowCount
    // - this method will return a TDocVariant containing a copy of all
    // field values of this row, uncoupled to the TOrmTable instance life time
    // - expand* methods will allow to return human-friendly representations
    procedure ToDocVariant(Row: PtrInt; out doc: variant;
      options: TDocVariantOptions = JSON_FAST;
      expandTimeLogAsText: boolean = false; expandEnumsAsText: boolean = false;
      expandHugeIDAsUniqueIdentifier: boolean = false); overload;
    /// retrieve all row values as a dynamic array of variants, ready to be
    // accessed via late-binding
    // - if readonly is TRUE, will contain an array of TOrmTableRowVariant, which
    // will point directly to the TOrmTable, which should remain allocated
    // - if readonly is FALSE, will contain an array of TDocVariant, containing
    // a copy of all field values of this row, uncoupled to the TOrmTable instance
    // - readonly=TRUE is faster to allocate (around 4 times for 10,000 rows), but
    // may be slightly slower to access than readonly=FALSE, if all values are
    // likely be accessed later in the process
    procedure ToDocVariant(out docs: TVariantDynArray; readonly: boolean); overload;
    /// retrieve all row values as a TDocVariant of kind dvArray, ready to be
    // accessed via late-binding
    // - if readonly is TRUE, will contain an array of TOrmTableRowVariant, which
    // will point directly to the TOrmTable, which should remain allocated
    // - if readonly is FALSE, will contain an array of TDocVariant, containing
    // a copy of all field values of this row, uncoupled to the TOrmTable instance
    // - readonly=TRUE is faster to allocate (around 4 times for 10,000 rows), but
    // may be slightly slower to access than readonly=FALSE, if all values are
    // likely be accessed later in the process
    procedure ToDocVariant(out docarray: variant; readonly: boolean); overload;
    // {$ifdef HASINLINE}inline;{$endif} won't reset docarray as required

    /// save the table values in JSON format
    // - JSON data is added to TJsonWriter, with UTF-8 encoding, and not flushed
    // - if Expand is true, JSON data is an array of objects, for direct use
    // with any Ajax or .NET client:
    // & [ {"col1":val11,"col2":"val12"},{"col1":val21,... ]
    // - if W.Expand is false, JSON data is serialized (used in TOrmTableJson)
    // & { "fieldCount":1,"values":["col1","col2",val11,"val12",val21,..] }
    // - RowFirst and RowLast can be used to ask for a specified row extent
    // of the returned data (by default, all rows are retrieved)
    // - IDBinarySize will force the ID field to be stored as hexadecimal text
    procedure GetJsonValues(W: TJsonWriter; RowFirst: PtrInt = 0;
      RowLast: PtrInt = 0; IDBinarySize: integer = 0); overload;
    /// same as the overloaded method, but appending an array to a TStream
    procedure GetJsonValues(Json: TStream; Expand: boolean; RowFirst: PtrInt = 0;
      RowLast: PtrInt = 0; IDBinarySize: integer = 0); overload;
    /// same as the overloaded method, but returning result into a RawUtf8
    function GetJsonValues(Expand: boolean; IDBinarySize: integer = 0;
      BufferSize: integer = 0): RawUtf8; overload;
    /// save the table as CSV format, into a stream
    // - if Tab=TRUE, will use TAB instead of ',' between columns
    // - you can customize the ',' separator - use e.g. the global ListSeparator
    // variable (from SysUtils) to reflect the current system definition (some
    // country use ',' as decimal separator, for instance our "douce France")
    // - AddBOM will add a UTF-8 Byte Order Mark at the beginning of the content
    procedure GetCsvValues(Dest: TStream; Tab: boolean; CommaSep: AnsiChar = ',';
      AddBOM: boolean = false; RowFirst: PtrInt = 0; RowLast: PtrInt = 0); overload;
    /// save the table as CSV format, into a string variable
    // - if Tab=TRUE, will use TAB instead of ',' between columns
    // - you can customize the ',' separator - use e.g. the global ListSeparator
    // variable (from SysUtils) to reflect the current system definition (some
    // country use ',' as decimal separator, for instance our "douce France")
    // - AddBOM will add a UTF-8 Byte Order Mark at the beginning of the content
    function GetCsvValues(Tab: boolean; CommaSep: AnsiChar = ',';
      AddBOM: boolean = false; RowFirst: PtrInt = 0; RowLast: PtrInt = 0): RawUtf8; overload;
    /// save the table in 'schemas-microsoft-com:rowset' XML format
    // - this format is used by ADODB.recordset, easily consumed by MS apps
    // - see @https://synopse.info/forum/viewtopic.php?pid=11691#p11691
    procedure GetMSRowSetValues(Dest: TStream; RowFirst, RowLast: PtrInt); overload;
    /// save the table in 'schemas-microsoft-com:rowset' XML format
    // - this format is used by ADODB.recordset, easily consumed by MS apps
    // - see @https://synopse.info/forum/viewtopic.php?pid=11691#p11691
    function GetMSRowSetValues: RawUtf8; overload;
    /// save the table in Open Document Spreadsheet compressed format
    // - this is a set of XML files compressed in a zip container
    // - this method will return the raw binary buffer of the file
    // - see @https://synopse.info/forum/viewtopic.php?id=2133
    function GetODSDocument(withColumnTypes: boolean = false): RawByteString;
    /// append the table content as a HTML <table> ... </table>
    procedure GetHtmlTable(Dest: TTextWriter); overload;
    /// save the table as a <html><body><table> </table></body></html> content
    function GetHtmlTable(const Header: RawUtf8 = '<head><style>table,th,td' +
      '{border: 1px solid black;border-collapse: collapse;}th,td{padding: 5px;' +
      'font-family: sans-serif;}</style></head>'#10): RawUtf8; overload;

    /// get the Field index of a FieldName
    // - return -1 if not found, index (0..FieldCount-1) if found
    function FieldIndex(FieldName: PUtf8Char): PtrInt; overload;
    /// get the Field index of a FieldName
    // - return -1 if not found, index (0..FieldCount-1) if found
    function FieldIndex(const FieldName: RawUtf8): PtrInt; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// get the Field index of a FieldName
    // - raise an EOrmTable if not found, index (0..FieldCount-1) if found
    function FieldIndexExisting(const FieldName: RawUtf8): PtrInt; overload;
    /// get the Field indexes of several Field names
    // - could be used to speed-up field access in a TOrmTable loop, avoiding
    // a FieldIndex(aFieldName) lookup for each value
    // - returns the number of matching Field names
    // - set -1 in FieldIndexes[]^ if not found, index (0..FieldCount-1) if found
    function FieldIndex(const FieldNames: array of RawUtf8;
      const FieldIndexes: array of PInteger): PtrInt; overload;
    /// get the Field indexes of several Field names
    // - raise an EOrmTable if not found
    // - set FieldIndexes[]^ to the index (0..FieldCount-1) if found
    // - could be used to speed-up field access in a TOrmTable loop, avoiding
    // a FieldIndex(aFieldName) lookup for each value, as such:
    //! list := TOrmTableJson.Create('',pointer(json),length(json));
    //! list.FieldIndexExisting(
    //!   ['FirstName','LastName','YearOfBirth','YearOfDeath','RowID','Data'],
    //!   [@FirstName,@LastName,@YearOfBirth,@YearOfDeath,@RowID,@Data]);
    //! for i := 1 to list.RowCount do
    //! begin
    //!   Check(list.Get(i,FirstName)<>nil);
    //!   Check(list.Get(i,LastName)<>nil);
    //!   Check(list.GetAsInteger(i,YearOfBirth)<10000);
    procedure FieldIndexExisting(const FieldNames: array of RawUtf8;
      const FieldIndexes: array of PInteger); overload;
    /// retrieve all field names as a RawUtf8 dynamic array
    function FieldNames: TRawUtf8DynArray;
      {$ifdef HASINLINE}inline;{$endif}
    /// get the Field content (encoded as UTF-8 text) from a property name
    // - return nil if not found
    function FieldValue(const FieldName: RawUtf8; Row: PtrInt): PUtf8Char;
      {$ifdef HASINLINE}inline;{$endif}
    /// sort result Rows, according to a specific field
    // - default is sorting by ascending order (Asc=true)
    // - you can specify a Row index to be updated during the sort in PCurrentRow
    // - sort is very fast, even for huge tables (more faster than any indexed
    // SQL query): 500,000 rows are sorted instantly
    // - this optimized sort implementation does the comparison first by the
    // designed field, and, if the field value is identical, the ID value is
    // used (it will therefore sort by time all identical values)
    procedure SortFields(Field: integer; Asc: boolean = true;
      PCurrentRow: PInteger = nil; FieldType: TOrmFieldType = oftUnknown;
      CustomCompare: TUtf8Compare = nil); overload;
    /// sort result Rows, according to a specific field
    // - overloaded method allowing to specify the field by its name
    procedure SortFields(const FieldName: RawUtf8; Asc: boolean = true;
      PCurrentRow: PInteger = nil; FieldType: TOrmFieldType = oftUnknown;
      CustomCompare: TUtf8Compare = nil); overload;
    /// sort result Rows, according to some specific fields
    // - is able to make multi-field sort
    // - both Fields[] and Asc[] arrays should have the same count, otherwise
    // default Asc[]=true value will be assumed
    // - set any Fields[]=-1 to identify the ID column (even if is hidden)
    // - if  CustomCompare=[], which use the default comparison function for the
    // field type, unless you set as many custom comparison function items
    // as in the Fields[] and Asc[] parameters
    procedure SortFields(const Fields: array of integer;
      const Asc: array of boolean;
      const CustomCompare: array of TUtf8Compare); overload;
    /// guess the field type from first non null data row
    // - if QueryTables[] are set, exact field type and enumerate TypeInfo() is
    // retrieved from the Delphi RTTI; otherwise, get from the cells content
    // - return oftUnknown is all data fields are null
    // - oftBlob is returned if the field is encoded as SQLite3 BLOB literals
    // (X'53514C697465' e.g.)
    // - since TOrmTable data is PUtf8Char, string type is oftUtf8Text only
    function FieldType(Field: PtrInt): TOrmFieldType; overload;
    /// guess the field type from first non null data row
    // - if QueryTables[] are set, exact field type and (enumerate) TypeInfo() is
    // retrieved from the Delphi RTTI; otherwise, get from the cells content
    // - return oftUnknown is all data fields are null
    // - oftBlob is returned if the field is encoded as SQLite3 BLOB literals
    // (X'53514C697465' e.g.)
    // - since TOrmTable data is PUtf8Char, string type is oftUtf8Text only
    function FieldType(Field: integer;
      out FieldTypeInfo: POrmTableFieldType): TOrmFieldType; overload;
    /// get the appropriate Sort comparison function for a field,
    // nil if not available (bad field index or field is blob)
    // - field type is guessed from first data row
    function SortCompare(Field: integer): TUtf8Compare;
    /// get the mean of characters length of all fields
    // - the character length is for the first line of text only (stop counting
    // at every newline character, i.e. #10 or #13 char)
    // - return the sum of all mean of character lengths
    function CalculateFieldLengthMean(var aResult: TIntegerDynArray;
      FromDisplay: boolean = false): integer;
    /// get the mean of characters length of this field
    // - the character length is for the first line of text only (stop counting
    // at every newline character, i.e. #10 or #13 char)
    // - very fast: calculated only once for all fields
    function FieldLengthMean(Field: PtrInt): cardinal;
    /// get the sum of all mean of characters length of all fields
    // - very fast: calculated only once for all fields
    function FieldLengthMeanSum: cardinal;
    /// get the maximum number of characters of this field
    function FieldLengthMax(Field: PtrInt; NeverReturnsZero: boolean = false): cardinal;
    /// force the mean of characters length for every field
    // - expect as many parameters as fields in this table
    // - override internal fFieldLengthMean[] and fFieldLengthMeanSum values
    procedure SetFieldLengthMean(const Lengths: array of cardinal);
    /// set the exact type of a given field
    // - by default, column types and sizes will be retrieved from JSON content
    // from first row, or all rows if FieldTypeIntegerDetectionOnAllRows is set
    // - you can define a specific type for a given column, and optionally
    // a maximum column size
    // - FieldTypeInfo can be specified for sets or enumerations, as such:
    // ! aTable.SetFieldType(0, oftEnumerate, TypeInfo(TEnumSample));
    // ! aTable.SetFieldType(1, oftSet, TypeInfo(TSetSamples));
    // or for dynamic arrays
    procedure SetFieldType(Field: PtrInt; FieldType: TOrmFieldType;
      FieldTypeInfo: PRttiInfo = nil; FieldSize: integer = -1;
      FieldTableIndex: integer = -1); overload;
    /// set the exact type of a given field
    // - by default, column types and sizes will be retrieved from JSON content
    // from first row, or all rows if FieldTypeIntegerDetectionOnAllRows is set
    // - you can define a specific type for a given column, and optionally
    // a maximum column size
    // - FieldTypeInfo can be specified for sets or enumerations, as such:
    // ! aTable.SetFieldType('Sample', oftEnumerate, TypeInfo(TEnumSample));
    // ! aTable.SetFieldType('Samples', oftSet, TypeInfo(TSetSamples));
    procedure SetFieldType(const FieldName: RawUtf8; FieldType: TOrmFieldType;
      FieldTypeInfo: PRttiInfo = nil; FieldSize: integer = -1); overload;
    /// set the exact type of all fields, from the DB-like information
    procedure SetFieldTypes(const DBTypes: TSqlDBFieldTypeDynArray);
    /// increase a particular Field Length Mean value
    // - to be used to customize the field appareance (e.g. for adding of left
    // checkbox for Marked[] fields)
    procedure FieldLengthMeanIncrease(aField, aIncrease: PtrInt);

    /// search for a value inside the raw table data, using Utf8IComp/StrComp()
    // - returns 0 if not found, or the matching Row number otherwise
    function SearchFieldEquals(const Value: RawUtf8; FieldIndex: PtrInt;
      StartRow: PtrInt = 1; CaseSensitive: boolean = false): PtrInt; overload;
    /// search for a value inside the raw table data, using Utf8IComp/StrComp()
    // - returns 0 if not found, or the matching Row number otherwise
    function SearchFieldEquals(Value: PUtf8Char; FieldIndex: PtrInt;
      StartRow: PtrInt = 1; CaseSensitive: boolean = false): PtrInt; overload;
    /// search for a value inside the raw table data, using IdemPChar()
    // - returns 0 if not found, or the matching Row number otherwise
    function SearchFieldIdemPChar(const Value: RawUtf8; FieldIndex: PtrInt;
      StartRow: PtrInt = 1): PtrInt;
    /// search for a value using O(log(n)) binary search of a sorted field
    // - here the content should have been previously sorted via Sort(),
    // or CustomCompare should be defined, otherwise the SearchFieldEquals()
    // slower O(n) method is called
    // - returns 0 if not found, or the matching Row number otherwise
    function SearchFieldSorted(const Value: RawUtf8; FieldIndex: PtrInt;
      CustomCompare: TUtf8Compare = nil): PtrInt; overload;
    /// search for a value using O(log(n)) binary search of a sorted field
    // - here the content should have been previously sorted via Sort(),
    // or CustomCompare should be defined, otherwise the SearchFieldEquals()
    // slower O(n) method is called
    // - returns 0 if not found, or the matching Row number otherwise
    function SearchFieldSorted(Value: PUtf8Char; FieldIndex: PtrInt;
      CustomCompare: TUtf8Compare = nil): PtrInt; overload;

    /// get all IDs where individual bit in Bits are set
    procedure IDArrayFromBits(const Bits; out IDs: TIDDynArray);
    /// get all individual bit in Bits corresponding to the supplied IDs
    // - warning: var IDs integer array will be sorted within this method call
    procedure IDArrayToBits(var Bits; var IDs: TIDDynArray);
    /// get the Row index corresponding to a specified ID
    // - return the Row number, from 1 to RowCount
    // - return RowCount (last row index) if this ID was not found or no
    // ID field is available, unless aNotFoundMinusOne is set, and then -1 is
    // returned
    function RowFromID(aID: TID; aNotFoundMinusOne: boolean = false): PtrInt;

    /// delete the specified data Row from the Table
    // - only overwrite the internal Results[] pointers, don't free any memory,
    // nor modify the internal DataSet
    function DeleteRow(Row: PtrInt): boolean;
    /// delete the specified Column text from the Table
    // - don't delete the Column: only delete UTF-8 text in all rows for this field
    function DeleteColumnValues(Field: PtrInt): boolean;

    /// after a TOrmTable has been initialized, this method can be called
    // one or more times to iterate through all data rows
    // - you shall call this method before calling FieldBuffer()/Field() methods
    // - return TRUE on success, with data ready to be retrieved by Field*()
    // - return FALSE if no more row is available (i.e. exceeded RowCount)
    // - if SeekFirst is TRUE, will put the cursor on the first row of results,
    // otherwise, it will fetch one row of data, to be called within a loop
    // - you can specify a variant instance (e.g. allocated on the stack) in
    // optional RowVariant parameter, to access field values using late binding
    // - typical use may be:
    // ! while TableCustomers.Step do
    // !   writeln(Field('name'));
    // - or, when using a variant and late-binding:
    // ! var customer: variant;
    // ! ...
    // !   while TableCustomers.Step(false, @customer) do
    // !     writeln(customer.Name);
    function Step(SeekFirst: boolean = false; RowVariant: PVariant = nil): boolean;
    /// read-only access to a particular field value, as UTF-8 encoded buffer
    // - raise an EOrmTable if called outside valid Step() sequence
    // - similar to Get() method, but for the current Step
    function FieldBuffer(FieldIndex: PtrInt; Len: PInteger = nil): PUtf8Char; overload;
    /// read-only access to a particular field value, as UTF-8 encoded buffer
    // - raise an EOrmTable if called outside valid Step() sequence
    // - similar to Get() method, but for the current Step
    function FieldBuffer(const FieldName: RawUtf8; Len: PInteger = nil): PUtf8Char; overload;
    /// read-only access to a particular field value, as integer
    // - raise an EOrmTable if called outside valid Step() sequence
    // - similar to GetAsInteger() method, but for the current Step
    function FieldAsInteger(FieldIndex: PtrInt): Int64; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read-only access to a particular field value, as integer
    // - raise an EOrmTable if called outside valid Step() sequence
    // - similar to GetAsInteger() method, but for the current Step
    function FieldAsInteger(const FieldName: RawUtf8): Int64; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read-only access to a particular field value, as floating-point value
    // - raise an EOrmTable if called outside valid Step() sequence
    // - similar to GetAsFloat() method, but for the current Step
    function FieldAsFloat(FieldIndex: PtrInt): TSynExtended; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read-only access to a particular field value, as floating-point value
    // - raise an EOrmTable if called outside valid Step() sequence
    // - similar to GetAsFloat() method, but for the current Step
    function FieldAsFloat(const FieldName: RawUtf8): TSynExtended; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read-only access to a particular field value, as RawUtf8
    // - raise an EOrmTable if called outside valid Step() sequence
    // - similar to GetU() method, but for the current Step
    function FieldAsRawUtf8(FieldIndex: PtrInt): RawUtf8; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read-only access to a particular field value, as RawUtf8
    // - raise an EOrmTable if called outside valid Step() sequence
    // - similar to GetU() method, but for the current Step
    function FieldAsRawUtf8(const FieldName: RawUtf8): RawUtf8; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read-only access to a particular field value, as VCL String
    // - raise an EOrmTable if called outside valid Step() sequence
    // - similar to GetString() method, but for the current Step
    function FieldAsString(FieldIndex: PtrInt): string; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read-only access to a particular field value, as VCL String
    // - raise an EOrmTable if called outside valid Step() sequence
    // - similar to GetString() method, but for the current Step
    function FieldAsString(const FieldName: RawUtf8): string; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read-only access to a particular field value, as a variant
    // - raise an EOrmTable if called outside valid Step() sequence
    // - will call GetVariant() method for appropriate data conversion
    function Field(FieldIndex: PtrInt): variant; overload;
    /// read-only access to a particular field value, as a variant
    // - raise an EOrmTable if called outside valid Step() sequence
    // - will call GetVariant() method for appropriate data conversion
    function Field(const FieldName: RawUtf8): variant; overload;
    /// contains the associated SQL statement on Query
    property QuerySql: RawUtf8
      read fQuerySql;
    /// returns the SQL Table name, guessed from the associated QuerySql statement
    property QueryTableNameFromSql: RawUtf8
      read GetQueryTableNameFromSql;
    /// read-only access to the number of data Rows in this table
    // - first row contains field name
    // - then 1..RowCount rows contain the data itself
    // - safely returns 0 if the TOrmTable instance is nil
    property RowCount: PtrInt
      read GetRowCount;
    /// read-only access to the number of fields for each Row in this table
    property FieldCount: PtrInt
      read fFieldCount;
    /// raw access to the data values memory pointers
    // - you should rather use the Get*() methods which can use the length
    property Results[Offset: PtrInt]: PUtf8Char
      read GetResults write SetResultsSafe;
    /// raw access to the data values UTF-8 length
    // - you should rather use the Get*() methods instead of Results+ResultsLen
    {$ifdef NOTORMTABLELEN}
    property ResultsLen[Offset: PtrInt]: integer
      read GetResultsLen;
    {$else}
    property ResultsLen: TIntegerDynArray
      read fLen;
    {$endif NOTORMTABLELEN}
    /// read-only access to the ID/RowID field index
    property FieldIndexID: integer
      read fFieldIndexID;
    /// read-only acccess to the current Row number, after a Step() call
    // - contains 0 if accessed outside valid Step() sequence call
    // - contains 1..RowCount after a valid Step() iteration
    property StepRow: integer
      read fStepRow;
    /// this property contains the internal state counter of the server database
    // when the data was retrieved from it
    // - can be used to check if retrieved data may be out of date
    property InternalState: cardinal
      read fInternalState write fInternalState;
    /// if the TOrm is the owner of this table, i.e. if it must free it
    property OwnerMustFree: boolean
      read fOwnerMustFree write fOwnerMustFree;
    /// by default, if field types are not set, only the content of the first
    // row will be checked, to make a difference between a oftInteger and oftFloat
    // - you can set this property to TRUE so that all non string rows will
    // be checked for the exact number precision
    // - note that the safest is to provide the column type, either by supplying
    // the TOrm class, or by calling SetFieldType() overloaded methods
    property FieldTypeIntegerDetectionOnAllRows: boolean
      read fFieldTypeAllRows write fFieldTypeAllRows;
    /// used by GetJsonValues, GetHtmlTable and GetCsvValues methods
    // to export custom JSON content
    property OnExportValue: TOnOrmTableGetValue
      read fOnExportValue write fOnExportValue;
  end;


{ ************ TOrmTableRowVariant Custom Variant Type }

type
  /// memory structure used for our TOrmTableRowVariant custom variant type
  // used to have direct access to TOrmTable content
  // - the associated TOrmTable must stay allocated as long as this variant
  // is used, otherwise random GPF issues may occur
  TOrmTableRowVariantData = packed record
    /// the custom variant type registered number
    VType: TVarType;
    VFiller: array[1..SizeOf(TVarData) - SizeOf(TVarType)
      - SizeOf(TOrmTableAbstract) - SizeOf(integer)] of byte;
    /// reference to the associated TOrmTable
    VTable: TOrmTableAbstract;
    /// the row number corresponding to this value
    // - equals -1 if should follow StepRow property value
    VRow: integer;
  end;

  /// pointer to the memory structure used for TOrmTableRowVariant storage
  POrmTableRowVariantData = ^TOrmTableRowVariantData;

  /// a custom variant type used to have direct access to TOrmTable content
  // - use TOrmTable.Step(..,@Data) method to initialize such a Variant
  // - the variant members/fields are read-only by design
  // - the associated TOrmTable must stay allocated as long as this variant
  // is used, otherwise random GPF issues may occur
  TOrmTableRowVariant = class(TSynInvokeableVariantType)
  protected
    function IntGet(var Dest: TVarData; const Instance: TVarData;
      Name: PAnsiChar; NameLen: PtrInt; NoException: boolean): boolean; override;
  public
    /// customization of variant into JSON serialization
    procedure ToJson(W: TTextWriter; const Value: variant); override;
    /// handle type conversion to string
    procedure Cast(var Dest: TVarData; const Source: TVarData); override;
    /// handle type conversion to string
    procedure CastTo(var Dest: TVarData; const Source: TVarData;
      const AVarType: TVarType); override;
  end;



{ ************ TOrmLocks and TRestCacheEntry Basic Structures }

type
  /// used to store the locked record list, in a specified table
  // - the maximum count of the locked list if fixed to 512 by default,
  // which seems correct for common usage
  {$ifdef USERECORDWITHMETHODS}
  TOrmLocks = record
  {$else}
  TOrmLocks = object
  {$endif USERECORDWITHMETHODS}
  public
    /// the number of locked records stored in this object
    Count: integer;
    /// contains the locked record ID
    // - an empty position is marked with 0 after UnLock()
    IDs: TIDDynArray;
    /// contains the time and date of the lock
    // - filled internally by the fast GetTickCount64() function (faster than
    // TDateTime or TSystemTime/GetLocalTime)
    // - used to purge to old entries - see PurgeOlderThan() method below
    Ticks64s: TInt64DynArray;
    /// lock a record, specified by its ID
    // - returns true on success, false if was already locked
    function Lock(aID: TID): boolean;
    /// unlock a record, specified by its ID
    // - returns true on success, false if was not already locked
    function UnLock(aID: TID): boolean;
    /// return true if a record, specified by its ID, is locked
    function isLocked(aID: TID): boolean;
    /// delete all the locked IDs entries, after a specified time
    // - to be used to release locked records if the client crashed
    // - default value is 30 minutes, which seems correct for common database usage
    procedure PurgeOlderThan(MinutesFromNow: cardinal = 30);
  end;

  POrmLocks = ^TOrmLocks;

  TOrmLocksDynArray = array of TOrmLocks;

  /// for TRestCache, stores a table values
  TRestCacheEntryValue = packed record
    /// corresponding ID
    ID: TID;
    /// GetTickCount64 shr 9 timestamp when this cached value was stored
    // - resulting time period has therefore a resolution of 512 ms, and
    // overflows after 70 years without computer reboot
    // - equals 0 when there is no JSON value cached
    Timestamp512: cardinal;
    /// some associated unsigned integer value
    // - not used by TRestCache, but available at TRestCacheEntry level
    Tag: cardinal;
    /// JSON encoded UTF-8 serialization of the record
    Json: RawUtf8;
  end;

  /// for TRestCache, stores all tables values
  TRestCacheEntryValueDynArray = array of TRestCacheEntryValue;

  /// for TRestCache, stores a table settings and values
  // - use a TRestCacheEntryValue array sorted by ID for O(log(n)) search
  {$ifdef USERECORDWITHMETHODS}
  TRestCacheEntry = record
  {$else}
  TRestCacheEntry = object
  {$endif USERECORDWITHMETHODS}
  public
    /// TRUE if this table should use caching
    // - i.e. if was not set, or worth it for this table (e.g. in-memory table)
    CacheEnable: boolean;
    /// the whole specified Table content will be cached
    CacheAll: boolean;
    /// time out value (in ms)
    // - if 0, caching will never expire
    TimeOutMS: cardinal;
    /// the number of entries stored in Values[]
    Count: integer;
    /// all cached IDs and JSON content
    Values: TRestCacheEntryValueDynArray;
    /// TDynArray wrapper around the Values[] array
    Value: TDynArray;
    /// used to lock the table cache for multi thread safety
    Mutex: TRTLCriticalSection;
    /// initialize this table cache
    // - will set Value wrapper and Mutex handle - other fields should have
    // been cleared by caller (is the case for a TRestCacheEntryDynArray)
    procedure Init;
    /// reset all settings corresponding to this table cache
    procedure Clear;
    /// finalize this table cache entry
    procedure Done;
    /// flush cache for a given Value[] index
    procedure FlushCacheEntry(Index: integer);
    /// flush cache for all Value[]
    procedure FlushCacheAllEntries;
    /// add the supplied ID to the Value[] array
    procedure SetCache(aID: TID);
    /// update/refresh the cached JSON serialization of a given ID
    procedure SetJson(aID: TID; const aJson: RawUtf8;
      aTag: cardinal = 0); overload;
    /// retrieve a JSON serialization of a given ID from cache
    function RetrieveJson(aID: TID; var aJson: RawUtf8;
      aTag: PCardinal = nil): boolean; overload;
    /// compute how much memory stored entries are using
    // - will also flush outdated entries
    function CachedMemory(FlushedEntriesCount: PInteger = nil): cardinal;
  end;

  /// for TRestCache, stores all table settings and values
  // - this dynamic array will follow TRest.Model.Tables[] layout, i.e. one
  // entry per TOrm class in the data model
  TRestCacheEntryDynArray = array of TRestCacheEntry;



{ *********************** Abstract TOrmPropertiesAbstract Parent Class }

  /// custom layout of TOrmProperties.SaveFieldsFromJsonArray output JSON object
  TSaveFieldsAsObject = set of (
    sfoExtendedJson,
    sfoStartWithID,
    sfoEndWithID,
    sfoPutIDFirst,
    sfoPutIDLast);

  /// parent used to store some information about a given TOrm class properties
  // - is fully implemented in mormot.orm.core by the final TOrmProperties class
  TOrmPropertiesAbstract = class
  protected
    fLock: TRTLCriticalSection;
    fTableRtti: TRttiJson;
    fHasNotSimpleFields: boolean;
    fDynArrayFieldsHasObjArray: boolean;
    fHasTypeFields: TOrmFieldTypes;
    fFields: TOrmPropInfoList;
    fSimpleFields: TOrmPropInfoObjArray;
    fSqlTableName: RawUtf8;
    fCopiableFields: TOrmPropInfoObjArray;
    fDynArrayFields: TOrmPropInfoRttiDynArrayObjArray;
    fBlobCustomFields: TOrmPropInfoCustomDynArray;
    fBlobFields: TOrmPropInfoRttiRawBlobDynArray;
    fManyFields: TOrmPropInfoRttiManyObjArray;
    fRecordManySourceProp: TOrmPropInfoRttiInstance;
    fRecordManyDestProp: TOrmPropInfoRttiInstance;
    fSqlTableNameUpperWithDot: RawUtf8;
    fSqlFillPrepareMany: RawUtf8;
    fSqlTableSimpleFieldsNoRowID: RawUtf8;
    fSqlTableUpdateBlobFields: RawUtf8;
    fSqlTableRetrieveBlobFields: RawUtf8;
    fSqlTableRetrieveAllFields: RawUtf8;
    fRecordVersionField: TOrmPropInfoRttiRecordVersion;
    fWeakZeroClass: TObject;
    fCustomCollation: TRawUtf8DynArray;
  public
    /// initialize the list - mainly its lock
    constructor Create;
    /// finalize the list - mainly its lock
    destructor Destroy; override;

    /// return TRUE if the given name is either ID/RowID, either a property name
    function IsFieldName(PropName: PUtf8Char): boolean;
    /// return TRUE if the given name is either ID/RowID, either a property name,
    // or an aggregate function (MAX/MIN/AVG/SUM) on a valid property name
    function IsFieldNameOrFunction(const PropName: RawUtf8): boolean;
    /// set all bits corresponding to the supplied field names
    // - returns TRUE on success, FALSE if any field name is not existing
    function FieldBitsFrom(const aFields: array of PUtf8Char;
      var Bits: TFieldBits): boolean; overload;
    /// set all bits corresponding to the supplied field names
    // - returns the matching fields set
    function FieldBitsFrom(const aFields: array of PUtf8Char): TFieldBits; overload;
    {$ifndef PUREMORMOT2}
    function FieldBitsFromRawUtf8(const aFields: array of RawUtf8): TFieldBits; overload;
    function FieldBitsFromRawUtf8(const aFields: array of RawUtf8;
      var Bits: TFieldBits): boolean; overload;
    {$endif PUREMORMOT2}
    /// set all bits corresponding to the supplied CSV field names
    // - returns TRUE on success, FALSE if any field name is not existing
    function FieldBitsFromCsv(const aFieldsCsv: RawUtf8;
      var Bits: TFieldBits): boolean; overload;
    /// set all bits corresponding to the supplied CSV field names, including ID
    // - returns TRUE on success, FALSE if any field name is not existing
    // - this overloaded method will identify ID/RowID field name, and set
    // withID output parameter according to its presence
    // - if aFieldsCsv='*', Bits will contain all simple fields, and withID=true
    function FieldBitsFromCsv(const aFieldsCsv: RawUtf8; var Bits: TFieldBits;
      out withID: boolean): boolean; overload;
    /// set all bits corresponding to the supplied CSV field names
    // - returns the matching fields set
    function FieldBitsFromCsv(const aFieldsCsv: RawUtf8): TFieldBits; overload;
    /// set all simple bits corresponding to the simple fields, excluding some
    // - could be a convenient alternative to FieldBitsFromCsv() if only some
    // fields are to be excluded
    // - returns the matching fields set
    function FieldBitsFromExcludingCsv(const aFieldsCsv: RawUtf8;
      aOccasion: TOrmOccasion = ooSelect): TFieldBits;
    /// set all bits corresponding to the supplied BLOB field type information
    // - returns TRUE on success, FALSE if blob field is not recognized
    function FieldBitsFromBlobField(aBlobField: PRttiProp;
      var Bits: TFieldBits): boolean;
    /// compute the CSV field names text from a set of bits
    procedure CsvFromFieldBits(const Prefix: array of const;
      const Bits: TFieldBits; const BitsSuffix: shortstring;
      const Suffix: array of const; out Result: RawUtf8);
    /// set all field indexes corresponding to the supplied field names
    // - returns TRUE on success, FALSE if any field name is not existing
    function FieldIndexDynArrayFromRawUtf8(const aFields: array of RawUtf8;
      var Indexes: TFieldIndexDynArray): boolean; overload;
    /// set all field indexes corresponding to the supplied field names
    // - returns the matching fields set
    function FieldIndexDynArrayFromRawUtf8(
      const aFields: array of RawUtf8): TFieldIndexDynArray; overload;
    /// set all field indexes corresponding to the supplied CSV field names
    // - returns TRUE on success, FALSE if any field name is not existing
    function FieldIndexDynArrayFromCsv(const aFieldsCsv: RawUtf8;
      var Indexes: TFieldIndexDynArray): boolean; overload;
    /// set all field indexes corresponding to the supplied CSV field names
    // - returns the matching fields set
    function FieldIndexDynArrayFromCsv(
      const aFieldsCsv: RawUtf8): TFieldIndexDynArray; overload;
    /// set all field indexes corresponding to the supplied BLOB field type information
    // - returns TRUE on success, FALSE if blob field is not recognized
    function FieldIndexDynArrayFromBlobField(aBlobField: PRttiProp;
      var Indexes: TFieldIndexDynArray): boolean;
    /// retrieve a Field property RTTI information from a Property Name
    // - this version returns nil if the property is not a BLOB field
    function BlobFieldPropFromRawUtf8(const PropName: RawUtf8): PRttiProp;
    /// retrieve a Field property RTTI information from a Property Name
    // - this version returns nil if the property is not a BLOB field
    function BlobFieldPropFromUtf8(PropName: PUtf8Char; PropNameLen: integer): PRttiProp;

    /// append a field name to a RawUtf8 Text buffer
    // - if FieldIndex=VIRTUAL_TABLE_ROWID_COLUMN (-1), appends 'RowID' or
    // 'ID' (if ForceNoRowID=TRUE) to Text
    // - on error (i.e. if FieldIndex is out of range) will return TRUE
    // - otherwise, will return FALSE and append the field name to Text
    function AppendFieldName(FieldIndex: integer; var Text: RawUtf8;
      ForceNoRowID: boolean): boolean;
    /// return the first unique property of kind RawUtf8
    // - this property is mainly the "Name" property, i.e. the one with
    // "stored AS_UNIQUE" (i.e. "stored false") definition on most TOrm
    // - if ReturnFirstIfNoUnique is TRUE and no unique property is found,
    // the first RawUtf8 property is returned anyway
    // - returns '' if no matching field was found
    function MainFieldName(ReturnFirstIfNoUnique: boolean = false): RawUtf8;
    /// return the SQLite3 field datatype for each specified field
    // - set to '' for fields with no column created in the database (e.g. oftMany)
    // - returns e.g. ' INTEGER, ' or ' TEXT COLLATE SYSTEMNOCASE, '
    function OrmFieldTypeToSql(FieldIndex: integer): RawUtf8;
    /// return the UTF-8 encoded SQL statement source to alter the table for
    //  adding the specified field
    // - returns something like 'ALTER TABLE tablename ADD COLUMN coldef UNIQUE'
    function SqlAddField(FieldIndex: integer): RawUtf8;
    /// set a custom SQlite3 text column collation for a specified field
    // - can be used e.g. to override the default COLLATE SYSTEMNOCASE of RawUtf8
    // - collations defined within our mormot.db.raw.sqlite3 unit are the SQLite3
    // standard BINARY, NOCASE, RTRIM and our custom SYSTEMNOCASE, UNICODENOCASE,
    // ISO8601, WIN32CASE, WIN32NOCASE alternatives
    // - do nothing if FieldIndex is not valid, and returns false
    // - could be set in overridden class procedure TOrm.InternalDefineModel
    // so that it will be common to all database models, for both client and server
    function SetCustomCollation(FieldIndex: integer;
      const aCollationName: RawUtf8): boolean; overload;
    /// set a custom SQlite3 text column collation for a specified field
    // - overloaded method which expects the field to be named
    function SetCustomCollation(const aFieldName, aCollationName: RawUtf8): boolean; overload;
    /// set a custom SQlite3 text column collation for a given field type
    // - can be used e.g. to override ALL default COLLATE SYSTEMNOCASE of RawUtf8,
    // or the default COLLATE ISO8601 of TDateTime, and let the generated SQLite3
    // file be available outside the scope of mORMot's SQLite3 engine
    // - collations defined within our mormot.db.raw.sqlite3 unit are the SQLite3
    // standard BINARY, NOCASE, RTRIM and our custom SYSTEMNOCASE, UNICODENOCASE,
    // ISO8601, WIN32CASE, WIN32NOCASE alternatives
    // - could be set in overridden class procedure TOrm.InternalDefineModel
    // so that it will be common to all database models, for both client and server
    // - note that you may inherit from TOrmNoCase to use the NOCASE
    // standard SQLite3 collation for all descendant ORM objects
    procedure SetCustomCollationForAll(aFieldType: TOrmFieldType;
      const aCollationName: RawUtf8);
    /// customize the TDocVariant options for all variant published properties
    // - will change the TOrmPropInfoRttiVariant.DocVariantOptions value
    // - use e.g. as SetVariantFieldDocVariantOptions(JSON_FAST_EXTENDED)
    // - see also TOrmNoCaseExtended root class
    procedure SetVariantFieldsDocVariantOptions(const Options: TDocVariantOptions);

    /// create a TJsonWriter, ready to be filled with TOrm.GetJsonValues
    // - you can use TOrmProperties.FieldBitsFromCsv() or
    // TOrmProperties.FieldBitsFrom() to compute aFields
    function CreateJsonWriter(Json: TStream; Expand, withID: boolean;
      const aFields: TFieldBits; KnownRowsCount: integer; aBufSize: integer = 8192;
      aStackBuffer: PTextWriterStackBuffer = nil): TJsonSerializer; overload;
    /// create a TJsonWriter, ready to be filled with TOrm.GetJsonValues(W)
    // - you can use TOrmProperties.FieldBitsFromCsv() or
    // TOrmProperties.FieldBitsFrom() to compute aFields
    function CreateJsonWriter(Json: TStream; Expand, withID: boolean;
      const aFields: TFieldIndexDynArray; KnownRowsCount: integer;
      aBufSize: integer = 8192;
      aStackBuffer: PTextWriterStackBuffer = nil): TJsonSerializer; overload;
    /// create a TJsonWriter, ready to be filled with TOrm.GetJsonValues(W)
    // - this overloaded method will call FieldBitsFromCsv(aFieldsCsv,bits,withID)
    // to retrieve the bits just like a SELECT (i.e. '*' for simple fields)
    function CreateJsonWriter(Json: TStream; Expand: boolean;
      const aFieldsCsv: RawUtf8; KnownRowsCount: integer; aBufSize: integer = 8192;
      aStackBuffer: PTextWriterStackBuffer = nil): TJsonSerializer; overload;
    /// set the W.ColNames[] array content + W.AddColumns
    procedure SetJsonWriterColumnNames(W: TJsonSerializer; KnownRowsCount: integer);
    /// save the TOrm RTTI into a binary header
    // - used e.g. by TRestStorageInMemory.SaveToBinary()
    procedure SaveBinaryHeader(W: TBufferWriter);
    /// ensure that the TOrm RTTI matches the supplied binary header
    // - used e.g. by TRestStorageInMemory.LoadFromBinary()
    function CheckBinaryHeader(var R: TFastReader): boolean;
    /// convert a JSON array of simple field values into a matching JSON object
    function SaveSimpleFieldsFromJsonArray(var P: PUtf8Char; ID: PID;
      EndOfObject: PAnsiChar; Format: TSaveFieldsAsObject): RawUtf8;
      {$ifdef HASINLINE} inline; {$endif}
    /// convert a JSON array of simple field values into a matching JSON object
    function SaveFieldsFromJsonArray(var P: PUtf8Char; const Bits: TFieldBits;
      ID: PID; EndOfObject: PAnsiChar; Format: TSaveFieldsAsObject): RawUtf8;

    /// add a custom unmanaged fixed-size record property
    // - simple kind of records (i.e. those not containing reference-counted
    // members) do not have RTTI generated, at least in older versions of Delphi
    // - use this method within TOrm.InternalRegisterCustomProperties
    // overridden method to define a custom record property with no
    // reference-counted types within (like strings) - typical use may be TGUID
    // - main parameters are the record size, in bytes, and the property pointer
    // - add an TOrmPropInfoRecordFixedSize instance to the internal list
    // - if aData2Text/aText2Data parameters are not defined, it will fallback
    // to TOrmPropInfo.BinaryToText() simple text Base64 encoding
    // - can be used to override the default TOrm corresponding method:
    // !class procedure TOrmMyRecord.InternalRegisterCustomProperties(
    // !  Props: TOrmProperties);
    // !begin
    // !  Props.RegisterCustomFixedSizeRecordProperty(self, SizeOf(TMyRec),
    // !    'RecField', @TOrmMyRecord(nil).fRecField, [], SizeOf(TMyRec));
    // !end;
    procedure RegisterCustomFixedSizeRecordProperty(aTable: TClass;
      aRecordSize: cardinal; const aName: RawUtf8; aPropertyPointer: pointer;
      aAttributes: TOrmPropInfoAttributes; aFieldWidth: integer;
      aData2Text: TOnSqlPropInfoRecord2Text = nil;
      aText2Data: TOnSqlPropInfoRecord2Data = nil);
    /// add a custom record property from its RTTI definition
    // - handle any kind of record with TypeInfo() generated
    // - use this method within InternalRegisterCustomProperties overridden method
    // to define a custom record property containing reference-counted types
    // - main parameters are the record RTTI information, and the property pointer
    // - add an TOrmPropInfoRecordRtti instance to the internal list
    // - can be used as such:
    // !class procedure TOrmMyRecord.InternalRegisterCustomProperties(
    // !  Props: TOrmProperties);
    // !begin
    // !  Props.RegisterCustomRttiRecordProperty(self, TypeInfo(TMyRec),
    // !    'RecField', @TOrmMyRecord(nil).fRecField);
    // !end;
    procedure RegisterCustomRttiRecordProperty(aTable: TClass;
      aRecordInfo: PRttiInfo; const aName: RawUtf8; aPropertyPointer: pointer;
      aAttributes: TOrmPropInfoAttributes = []; aFieldWidth: integer = 0;
      aData2Text: TOnSqlPropInfoRecord2Text = nil;
      aText2Data: TOnSqlPropInfoRecord2Data = nil);
    /// add a custom property from its RTTI definition stored as JSON
    // - handle any kind of record with TypeInfo() generated
    // - use this method within InternalRegisterCustomProperties overridden method
    // to define a custom record property containing reference-counted types
    // - main parameters are the record RTTI information, and the property pointer
    // - add an TOrmPropInfoCustomJson instance to the internal list
    // - can be used as such:
    // !class procedure TOrmMyRecord.InternalRegisterCustomProperties(
    // !  Props: TOrmProperties);
    // !begin
    // !  Props.RegisterCustomPropertyFromRtti(self, TypeInfo(TMyRec),
    // !    'RecField', @TOrmMyRecord(nil).fRecField);
    // !end;
    procedure RegisterCustomPropertyFromRtti(aTable: TClass;
      aTypeInfo: PRttiInfo; const aName: RawUtf8; aPropertyPointer: pointer;
      aAttributes: TOrmPropInfoAttributes = []; aFieldWidth: integer = 0);
    /// add a custom property from its type name, stored as JSON
    // - handle any kind of registered record, including TGUID
    // - use this method within InternalRegisterCustomProperties overridden method
    // to define a custom record property containing reference-counted types
    // - main parameters are the record RTTI information, and the property pointer
    // - add an TOrmPropInfoCustomJson instance to the internal list
    // - can be used as such:
    // !class procedure TOrmMyRecord.InternalRegisterCustomProperties(
    // !  Props: TOrmProperties);
    // !begin
    // !  Props.RegisterCustomPropertyFromTypeName(self, 'TGUID', 'GUID',
    // !    @TOrmMyRecord(nil).fGUID, [aIsUnique], 38);
    // !end;
    procedure RegisterCustomPropertyFromTypeName(aTable: TClass;
      const aTypeName, aName: RawUtf8; aPropertyPointer: pointer;
      aAttributes: TOrmPropInfoAttributes = []; aFieldWidth: integer = 0);

    /// fast access to the RTTI properties attribute
    property TableRtti: TRttiJson
      read fTableRtti;
    /// if this class has any BLOB or TOrmRecordMany fields
    // - i.e. some fields to be ignored
    property HasNotSimpleFields: boolean
      read fHasNotSimpleFields;
    /// set of field types appearing in this record
    property HasTypeFields: TOrmFieldTypes
      read fHasTypeFields;
    /// list all fields, as retrieved from RTTI
    property Fields: TOrmPropInfoList
      read fFields;
    /// list all "simple" fields of this TOrm
    // - by default, the RawBlob and TOrmMany fields are not included
    // into this set: they must be read specificaly (in order to spare
    // bandwidth for BLOBs)
    // - dynamic arrays belong to simple fields: they are sent with other
    // properties content
    // - match inverted NOT_SIMPLE_FIELDS mask
    property SimpleFields: TOrmPropInfoObjArray
      read fSimpleFields;
    /// list all fields which can be copied from one TOrm instance to another
    // - match COPIABLE_FIELDS mask, i.e. all fields except oftMany
    property CopiableFields: TOrmPropInfoObjArray
      read fCopiableFields;
    /// list all TOrmMany fields of this TOrm
    property ManyFields: TOrmPropInfoRttiManyObjArray
      read fManyFields;

    /// list of all oftBlobDynArray fields of this TOrm
    property DynArrayFields: TOrmPropInfoRttiDynArrayObjArray
      read fDynArrayFields;
    /// TRUE if any of the oftBlobDynArray fields of this TOrm is a T*ObjArray
    // - used e.g. by TOrm.Destroy to release all owned nested instances
    property DynArrayFieldsHasObjArray: boolean
      read fDynArrayFieldsHasObjArray;
    /// list of all oftBlobCustom fields of this TOrm
    // - have been defined e.g. as TOrmPropInfoCustom custom definition
    property BlobCustomFields: TOrmPropInfoCustomDynArray
      read fBlobCustomFields;
    /// list all BLOB fields of this TOrm
    // - i.e. generic oftBlob fields (not oftBlobDynArray, oftBlobCustom nor
    // oftBlobRecord)
    property BlobFields: TOrmPropInfoRttiRawBlobDynArray
      read fBlobFields;
    /// for a TOrmMany class, points to the Source property RTTI
    property RecordManySourceProp: TOrmPropInfoRttiInstance
      read fRecordManySourceProp;
    /// for a TOrmMany class, points to the Dest property RTTI
    property RecordManyDestProp: TOrmPropInfoRttiInstance
      read fRecordManyDestProp;
    /// points to any TRecordVersion field
    // - contains nil if no such oftRecordVersion field do exist
    // - will be used by low-level storage engine to compute and store the
    // monotonic version number during any write operation
    property RecordVersionField: TOrmPropInfoRttiRecordVersion
      read fRecordVersionField;
    /// the Table name in the database in uppercase with a final '.'
    // - e.g. 'TEST.' for TOrmTest class
    // - can be used with IdemPChar() for fast check of a table name
    property SqlTableNameUpperWithDot: RawUtf8
      read fSqlTableNameUpperWithDot;
    /// returns 'COL1,COL2' with all COL* set to simple field names
    // - same value as Sql.TableSimpleFields[false,false]
    // - this won't change depending on the ORM settings: so it can be safely
    // computed here and not in TOrmModelProperties
    // - used e.g. by TOrm.GetSqlValues
    property SqlTableSimpleFieldsNoRowID: RawUtf8
      read fSqlTableSimpleFieldsNoRowID;
    /// returns 'COL1=?,COL2=?' with all BLOB columns names
    // - used e.g. by TRestServerDB.UpdateBlobFields()
    property SqlTableUpdateBlobFields: RawUtf8
      read fSqlTableUpdateBlobFields;
    /// returns 'COL1,COL2' with all BLOB columns names
    // - used e.g. by TRestServerDB.RetrieveBlobFields()
    property SqlTableRetrieveBlobFields: RawUtf8
      read fSqlTableRetrieveBlobFields;
  public
    /// bit set to 1 for indicating each TOrmFieldType fields of this TOrm
    FieldBits: array[TOrmFieldType] of TFieldBits;
    /// bit set to 1 for indicating TModTime/TSessionUserID fields
    // of this TOrm (leaving TCreateTime untouched)
    // - as applied before an UPDATE
    // - i.e. oftModTime and oftSessionUserID fields
    ComputeBeforeUpdateFieldsBits: TFieldBits;
    /// bit set to 1 for indicating TModTime/TCreateTime/TSessionUserID fields
    // of this TOrm
    // - as applied before an INSERT
    // - i.e. oftModTime, oftCreateTime and oftSessionUserID fields
    ComputeBeforeAddFieldsBits: TFieldBits;
    /// bit set to 1 for indicating fields to export, i.e. "simple" fields
    // - this array will handle special cases, like the TCreateTime fields
    // which shall not be included in ooUpdate but ooInsert and ooSelect e.g.
    SimpleFieldsBits: array[TOrmOccasion] of TFieldBits;
    /// number of fields to export, i.e. "simple" fields
    // - this array will handle special cases, like the TCreateTime fields
    // which shall not be included in ooUpdate but ooInsert and ooSelect e.g.
    SimpleFieldsCount: array[TOrmOccasion] of integer;
    /// "simple" fields SELECT expressions for TSelectStatement.Create parsing
    SimpleFieldSelect: TSelectStatementSelectDynArray;
    /// bit set to 1 for an unique field
    // - an unique field is defined as "stored AS_UNIQUE" (i.e. "stored false")
    // in its property definition
    IsUniqueFieldsBits: TFieldBits;
    /// bit set to 1 for the smallest simple fields
    // - i.e. excluding non only oftBlob and oftMany, but also oftVariant,
    // oftBlobDynArray, oftBlobCustom and oftUtf8Custom fields
    // - may be used to minimize the transmitted content, e.g. when serializing
    // to JSON for the essential information
    SmallFieldsBits: TFieldBits;
    /// bit set to 1 for the all fields storing some data
    // - match COPIABLE_FIELDS mask, i.e. all fields except oftMany
    CopiableFieldsBits: TFieldBits;
    /// contains the main field index (e.g. mostly 'Name')
    // - the [boolean] is for [ReturnFirstIfNoUnique] version
    // - contains -1 if no field matches
    MainField: array[boolean] of integer;
    /// count of coordinate fields of a TOrmRTree, before auxiliary columns
    RTreeCoordBoundaryFields: integer;
  end;

implementation


{ ************ Shared ORM/JSON Fields and Values Definitions }

function ToText(ft: TOrmFieldType): PShortString;
begin
  result := GetEnumName(TypeInfo(TOrmFieldType), ord(ft));
end;

function ToText(e: TOrmEvent): PShortString;
begin
  result := GetEnumName(TypeInfo(TOrmEvent), ord(e));
end;

function ToText(he: TOrmHistoryEvent): PShortString;
begin
  result := GetEnumName(TypeInfo(TOrmHistoryEvent), ord(he));
end;

function ToText(o: TOrmOccasion): PShortString;
begin
  result := GetEnumName(TypeInfo(TOrmOccasion), ord(o));
end;

function Utf8ContentNumberType(P: PUtf8Char): TOrmFieldType;
begin
  if (P = nil) or
     ((PInteger(P)^ = ord('n') + ord('u') shl 8 + ord('l') shl 16 +
       ord('l') shl 24) and
      (P[4] = #0)) then
    result := oftUnknown
  else
    case TextToVariantNumberType(P) of
      varInt64:
        result := oftInteger;
      varDouble:
        result := oftFloat;
      varCurrency:
        result := oftCurrency;
    else
      result := oftUtf8Text;
    end;
end;

function Utf8ContentType(P: PUtf8Char): TOrmFieldType;
var
  c, len: integer;
begin
  if P <> nil then
  begin
    while (P^ <= ' ') and
          (P^ <> #0) do
      inc(P);
    if (PInteger(P)^ = NULL_LOW) and
       (P[4] = #0) then
      result := oftUnknown
    else
    // don't check for 'false' or 'true' here, since their UTF-8 value is 0/1
    if P^ in ['-', '0'..'9'] then
      case TextToVariantNumberType(P) of
        varInt64:
          result := oftInteger;
        varDouble:
          result := oftFloat;
        varCurrency:
          result := oftCurrency;
      else
        begin
          len := StrLen(P);
          if (len > 15) and
             (Iso8601ToTimeLogPUtf8Char(P, len) <> 0) then
            result := oftDateTime
          else
            result := oftUtf8Text;
        end;
      end
    else
    begin
      c := PInteger(P)^ and $00ffffff;
      if (c = JSON_BASE64_MAGIC_C) or
         ((P^ = '''') and
          isBlobHex(P)) then
        result := oftBlob
      else if c = JSON_SQLDATE_MAGIC_C then
        result := oftDateTime
      else
        result := oftUtf8Text;
    end;
  end
  else
    result := oftUnknown;
end;

function ToText(enc: TRestBatchEncoding): PShortString;
begin
  result := GetEnumName(TypeInfo(TRestBatchEncoding), ord(enc));
end;

function ToText(vk: TOrmVirtualKind): PShortString;
begin
  result := GetEnumName(TypeInfo(TOrmVirtualKind), ord(vk));
end;



{$ifdef ISDELPHI20062007}
  {$WARNINGS OFF} // circumvent Delphi 2007 false positive warning
{$endif ISDELPHI20062007}

const
  PG_FT: array[TSqlDBFieldType] of string[9] = (
    'int4', 'text', 'int8', 'float8', 'numeric', 'timestamp', 'text', 'bytea');

function EncodeAsSqlPrepared(const Decoder: TJsonObjectDecoder;
  const TableName: RawUtf8; Occasion: TOrmOccasion;
  const UpdateIDFieldName: RawUtf8; BatchOptions: TRestBatchOptions;
  MultiInsertRowCount: integer): RawUtf8;
var
  f: PtrInt;
  W: TTextWriter;
  temp: TTextWriterStackBuffer;
begin
  W := TTextWriter.CreateOwnedStream(temp);
  try
    case Occasion of
      ooUpdate:
        begin
          if Decoder.FieldCount = 0 then
            raise EJsonObjectDecoder.Create('Invalid EncodeAsSqlPrepared(0)');
          W.AddShorter('update ');
          W.AddString(TableName);
          if Decoder.DecodedFieldTypesToUnnest <> nil then
          begin
            // PostgreSQL bulk update via nested array binding
            W.AddShort(' as t set ');
            for f := 0 to Decoder.FieldCount - 1 do
            begin
              W.AddString(Decoder.DecodedFieldNames^[f]);
              W.AddShorter('=v.');
              W.AddString(Decoder.DecodedFieldNames^[f]);
              W.AddComma;
            end;
            W.CancelLastComma;
            W.AddShort(' from ( select');
            for f := 0 to Decoder.FieldCount - 1 do
            begin
              W.AddShort(' unnest(?::');
              W.AddShort(PG_FT[Decoder.DecodedFieldTypesToUnnest^[f]]);
              W.AddShorter('[]),');
            end;
            W.AddShort(' unnest(?::int8[]) ) as v('); // last param is ID
            for f := 0 to Decoder.FieldCount - 1 do
            begin
              W.AddString(Decoder.DecodedFieldNames^[f]);
              W.AddComma;
            end;
            W.AddString(UpdateIDFieldName);
            W.AddShort(') where t.');
            W.AddString(UpdateIDFieldName);
            W.AddShorter('=v.');
            W.AddString(UpdateIDFieldName);
          end
          else
          begin
            // regular UPDATE statement
            W.AddShorter(' set ');
            for f := 0 to Decoder.FieldCount - 1 do
            begin
              // append 'COL1=?,COL2=?'
              W.AddString(Decoder.DecodedFieldNames^[f]);
              W.AddShorter('=?,');
            end;
            W.CancelLastComma;
            W.AddShort(' where ');
            W.AddString(UpdateIDFieldName);
            W.Add('=', '?'); // last param is ID
          end;
        end;
      ooInsert:
        begin
          if boInsertOrIgnore in BatchOptions then
            W.AddShort('insert or ignore into ')
          else if boInsertOrReplace in BatchOptions then
            W.AddShort('insert or replace into ')
          else
            W.AddShort('insert into ');
          W.AddString(TableName);
          if Decoder.FieldCount = 0 then
            W.AddShort(' default values')
          else
          begin
            W.Add(' ', '(');
            for f := 0 to Decoder.FieldCount - 1 do
            begin
              // append 'COL1,COL2'
              W.AddString(Decoder.DecodedFieldNames^[f]);
              W.AddComma;
            end;
            W.CancelLastComma;
            W.AddShort(') values (');
            if Decoder.DecodedFieldTypesToUnnest <> nil then
              // PostgreSQL bulk insert via nested array binding
              for f := 0 to Decoder.FieldCount - 1 do
              begin
                W.AddShort('unnest(?::');
                W.AddShort(PG_FT[Decoder.DecodedFieldTypesToUnnest^[f]]);
                W.AddShorter('[]),');
              end
            else
            begin
              // regular (multi) INSERT statement - e.g. for SQLite3
              W.AddStrings('?,', Decoder.FieldCount);
              while MultiInsertRowCount > 1 do
              begin
                // INSERT INTO .. VALUES (..),(..),(..),..
                W.CancelLastComma;
                W.AddShorter('),(');
                W.AddStrings('?,', Decoder.FieldCount);
                dec(MultiInsertRowCount);
              end;
            end;
            W.CancelLastComma;
            W.Add(')');
          end;
        end;
    else
      raise EJsonObjectDecoder.CreateUtf8('Unexpected EncodeAsSqlPrepared(%)',
        [ord(Occasion)]);
    end;
    W.SetText(result);
  finally
    W.Free;
  end;
end;

{$ifdef ISDELPHI20062007}
  {$WARNINGS ON}
{$endif ISDELPHI20062007}

procedure ValueVarToVariant(Value: PUtf8Char; ValueLen: integer;
  fieldType: TOrmFieldType; var result: TVarData; createValueTempCopy: boolean;
  typeInfo: PRttiInfo; options: TDocVariantOptions);
const
  /// map our available types for any SQL field property into variant values
  // - varNull will be used to store a true variant instance from JSON
  SQL_ELEMENTTYPES: array[TOrmFieldType] of word = (
    varEmpty,     // oftUnknown
    varString,    // oftAnsiText
    varString,    // oftUtf8Text
    varInteger,   // oftEnumerate
    varInt64,     // oftSet
    varInt64,     // oftInteger
    varInt64,     // oftID
    varInt64,     // oftRecord
    varBoolean,   // oftBoolean
    varDouble,    // oftFloat
    varDate,      // oftDateTime
    varInt64,     // oftTimeLog
    varCurrency,  // oftCurrency
    varNull,      // oftObject
    varNull,      // oftVariant
    varNull,      // oftNullable
    varString,    // oftBlob
    varNull,      // oftBlobDynArray
    varString,    // oftBlobCustom
    varString,    // oftUtf8Custom
    varEmpty,     // oftMany
    varInt64,     // oftModTime
    varInt64,     // oftCreateTime
    varInt64,     // oftTID
    varInt64,     // oftRecordVersion
    varInt64,     // oftSessionUserID
    varDate,      // oftDateTimeMS
    varInt64,     // oftUnixTime
    varInt64);    // oftUnixMSTime

  procedure Complex;
  var
    tmp: TSynTempBuffer;
    da: RawJson;
  begin
    tmp.buf := nil;
    try
      if (fieldType = oftBlobDynArray) and
         (typeInfo <> nil) and
         (Value <> nil) and
         (Value^ <> '[') and
         Base64MagicCheckAndDecode(Value, tmp, ValueLen) then
      begin
        da := DynArrayBlobSaveJson(typeInfo, tmp.buf);
        Value := pointer(da);
        ValueLen := length(da);
      end
      else if createValueTempCopy then
      begin
        tmp.Init(Value, ValueLen);
        Value := tmp.buf;
      end;
      GetJsonToAnyVariant(variant(result), Value, nil, @options, false);
    finally
      tmp.Done;
    end;
  end;

var
  err: integer;
begin
  VarClearAndSetType(variant(result), SQL_ELEMENTTYPES[fieldType]);
  result.VAny := nil; // avoid GPF
  case fieldType of
    oftCurrency:
      result.VInt64 := StrToCurr64(Value);
    oftFloat:
      begin
        result.VDouble := GetExtended(Value, err);
        if err <> 0 then
        begin
          result.VType := varString;
          FastSetString(RawUtf8(result.VAny), Value, ValueLen);
        end;
      end;
    oftDateTime,
    oftDateTimeMS:
      Iso8601ToDateTimePUtf8CharVar(Value, 0, result.VDate);
    oftBoolean:
      result.VBoolean := not ((Value = nil) or
                              (PWord(Value)^ = ord('0')) or
                         (PInteger(Value)^ = FALSE_LOW));
    oftEnumerate:
      result.VInteger := GetInteger(Value);
    oftInteger,
    oftID,
    oftTID,
    oftRecord,
    oftSet,
    oftRecordVersion,
    oftSessionUserID,
    oftTimeLog,
    oftModTime,
    oftCreateTime,
    oftUnixTime,
    oftUnixMSTime:
      SetInt64(Value, result.VInt64);
    oftAnsiText,
    oftUtf8Text:
      FastSetString(RawUtf8(result.VAny), Value, ValueLen);
    oftBlobCustom,
    oftBlob:
      BlobToRawBlob(Value, RawBlob(result.VAny), ValueLen);
    oftVariant,
    oftNullable,
    oftBlobDynArray,
    oftObject,
    oftUtf8Custom:
      Complex;
  end;
end;


{ ****************** ORM Ready UTF-8 Comparison Functions }

function Utf8CompareCurr64(P1, P2: PUtf8Char): PtrInt;
var
  V1, V2: Int64;
begin
  // faster than Utf8CompareDouble() for pure decimal (no exponent) values
  V1 := StrToCurr64(P1);
  V2 := StrToCurr64(P2);
  if V1 < V2 then
    result := -1
  else if V1 = V2 then
    result := 0
  else
    result := +1;
end;

function Utf8CompareBoolean(P1, P2: PUtf8Char): PtrInt;
label
  Z, P, n;
begin
  // assume 0 is FALSE, anything else is true
  if P1 = P2 then
    goto Z
  else if P1 = nil then
    goto P
  else if P2 = nil then
    goto n
  else if (P1^ = #0) or
          (PWord(P1)^ = ord('0')) then
    if (P2^ = #0) or
       (PWord(P2)^ = ord('0')) then
    begin
Z:    result := 0;  // P1=false P2=false
      exit;
    end
    else
    begin
n:    result := -1; // P1=false P2=true
      exit;
    end
  else if (P2^ <> #0) and
          (PWord(P2)^ <> ord('0')) then
    goto Z        // P1=true P2=true
  else
  begin
P:  result := 1;  // P1=true P2=false
    exit;
  end;
end;

function Utf8CompareInt32(P1, P2: PUtf8Char): PtrInt;
var
  V1, V2: PtrInt;
begin
  if P1 = P2 then
  begin
    result := 0;
    exit;
  end;
  V1 := GetInteger(P1);
  V2 := GetInteger(P2);
  if V1 < V2 then
    result := -1
  else if V1 = V2 then
    result := 0
  else
    result := +1;
end;

function Utf8CompareUInt32(P1, P2: PUtf8Char): PtrInt;
var
  V1, V2: PtrUInt;
begin
  if P1 = P2 then
  begin
    result := 0;
    exit;
  end;
  V1 := GetCardinal(P1);
  V2 := GetCardinal(P2);
  if V1 < V2 then
    result := -1
  else if V1 = V2 then
    result := 0
  else
    result := +1;
end;

function Utf8CompareRecord(P1, P2: PUtf8Char): PtrInt;
var
  V1, V2: Int64;
  T1, T2: cardinal;
begin
  if P1 = P2 then
  begin
    result := 0;
    exit;
  end;
  SetInt64(P1, V1{%H-});
  SetInt64(P2, V2{%H-});
  if V1 = V2 then
    result := 0
  else
  begin
    // special RecordRef / TRecordReference INTEGER sort
    T1 := V1 and 63;  // first sort by Table order
    T2 := V2 and 63;
    if T1 < T2 then
      result := -1
    else if T1 > T2 then
      result := +1
    else
    // we have T1=T2 -> same Table -> sort by ID
    if V1 < V2 then
      result := -1
    else if V1 = V2 then
      result := 0
    else
      result := +1;
  end;
end;

function Utf8CompareInt64(P1, P2: PUtf8Char): PtrInt;
var
  V1, V2: Int64;
begin
  if P1 = P2 then
  begin
    result := 0;
    exit;
  end;
  SetInt64(P1, V1{%H-});
  SetInt64(P2, V2{%H-});
  if V1 < V2 then
    result := -1
  else if V1 = V2 then
    result := 0
  else
    result := +1;
end;

function Utf8CompareDouble(P1, P2: PUtf8Char): PtrInt;
var
  V1, V2: TSynExtended;
  Err: integer;
label
  er;
begin
  if P1 = P2 then
  begin
    result := 0;
    exit;
  end;
  V1 := GetExtended(P1, Err);
  if Err <> 0 then
  begin
er: result := Utf8IComp(P1, P2);
    exit;
  end;
  V2 := GetExtended(P2, Err);
  if Err <> 0 then
    goto er;
  if V1 < V2 then // we don't care about exact = for a sort: Epsilon check is slow
    result := -1
  else
    result := +1;
end;

function Utf8CompareIso8601(P1, P2: PUtf8Char): PtrInt;
var
  V1, V2: TDateTime;
begin
  if P1 = P2 then
  begin
    result := 0;
    exit;
  end;
  Iso8601ToDateTimePUtf8CharVar(P1, 0, V1);
  Iso8601ToDateTimePUtf8CharVar(P2, 0, V2);
  if (V1 = 0) or
     (V2 = 0) then // any invalid date -> compare as strings
    result := StrComp(P1, P2)
  else if SameValue(V1, V2, 1 / MSecsPerDay) then
    result := 0
  else if V1 < V2 then
    result := -1
  else
    result := +1;
end;


{ ************ TJsonSerializer Class for TOrm Serialization }

{ TJsonSerializer }

procedure TJsonSerializer.SetOrmOptions(Value: TJsonSerializerOrmOptions);
begin
  fOrmOptions := Value;
  if Value * [jwoAsJsonNotAsString, jwoID_str] <> [] then
    if (ColNames <> nil) and
       (ColNames[0] = '"RowID":') then
      ColNames[0] := '"ID":'; // as expected by AJAX
end;

// backward compatibility methods - use Rtti global instead
{$ifndef PUREMORMOT2}

class procedure TJsonSerializer.RegisterClassForJson(aItemClass: TClass);
begin
  Rtti.RegisterClass(aItemClass);
end;

class procedure TJsonSerializer.RegisterClassForJson(
  const aItemClass: array of TClass);
begin
  Rtti.RegisterClasses(aItemClass);
end;

class procedure TJsonSerializer.RegisterCollectionForJson(
  aCollection: TCollectionClass; aItem: TCollectionItemClass);
begin
  Rtti.RegisterCollection(aCollection, aItem);
end;

class procedure TJsonSerializer.RegisterObjArrayForJson(
  aDynArray: PRttiInfo; aItem: TClass);
begin
  Rtti.RegisterObjArray(aDynArray, aItem);
end;

class procedure TJsonSerializer.RegisterObjArrayForJson(
  const aDynArrayClassPairs: array of const);
begin
  Rtti.RegisterObjArrays(aDynArrayClassPairs);
end;

{$endif PUREMORMOT2}


{ ************ TOrmPropInfo ORM / RTTI Classes }

const
  SQLFIELDTYPETODBFIELDTYPE: array[TOrmFieldType] of TSqlDBFieldType = (
    ftUnknown,   // oftUnknown
    ftUtf8,      // oftAnsiText
    ftUtf8,      // oftUtf8Text
    ftInt64,     // oftEnumerate
    ftInt64,     // oftSet
    ftInt64,     // oftInteger
    ftInt64,     // oftID = TOrm(aID)
    ftInt64,     // oftRecord = TRecordReference = RecordRef
    ftInt64,     // oftBoolean
    ftDouble,    // oftFloat
    ftDate,      // oftDateTime
    ftInt64,     // oftTimeLog
    ftCurrency,  // oftCurrency
    ftUtf8,      // oftObject
    ftUtf8,      // oftVariant
    ftNull,      // oftNullable
    ftBlob,      // oftBlob
    ftBlob,      // oftBlobDynArray
    ftBlob,      // oftBlobCustom
    ftUtf8,      // oftUtf8Custom
    ftUnknown,   // oftMany
    ftInt64,     // oftModTime
    ftInt64,     // oftCreateTime
    ftInt64,     // oftTID
    ftInt64,     // oftRecordVersion = TRecordVersion
    ftInt64,     // oftSessionUserID
    ftDate,      // oftDateTimeMS
    ftInt64,     // oftUnixTime = TUnixTime
    ftInt64);    // oftUnixMSTime = TUnixMSTime

  /// simple wrapper from each SQL used type into SQLite3 field datatype
  DEFAULT_ORMFIELDTYPETOSQL: array[TOrmFieldType] of RawUtf8 = (
    '',                              // oftUnknown - '' = no column created
    ' TEXT COLLATE NOCASE, ',        // oftAnsiText
    ' TEXT COLLATE SYSTEMNOCASE, ',  // oftUtf8Text
    ' INTEGER, ',                    // oftEnumerate
    ' INTEGER, ',                    // oftSet
    ' INTEGER, ',                    // oftInteger
    ' INTEGER, ',                    // oftID = TOrm(aID)
    ' INTEGER, ',                    // oftRecord = TRecordReference
    ' INTEGER, ',                    // oftBoolean
    ' FLOAT, ',                      // oftFloat
    ' TEXT COLLATE ISO8601, ',       // oftDateTime
    ' INTEGER, ',                    // oftTimeLog
    ' FLOAT, ',                      // oftCurrency
    ' TEXT COLLATE BINARY, ',        // oftObject
    ' TEXT COLLATE BINARY, ',        // oftVariant
    ' TEXT COLLATE NOCASE, ',        // oftNullable (from OrmFieldTypeStored)
    ' BLOB, ',                       // oftBlob
    ' BLOB, ',                       // oftBlobDynArray
    ' BLOB, ',                       // oftBlobCustom
    ' TEXT COLLATE NOCASE, ',        // oftUtf8Custom
    '',                              // oftMany - '' = no column created
    ' INTEGER, ',                    // oftModTime
    ' INTEGER, ',                    // oftCreateTime
    ' INTEGER, ',                    // oftTID
    ' INTEGER, ',                    // oftRecordVersion
    ' INTEGER, ',                    // oftSessionUserID
    ' TEXT COLLATE ISO8601, ',       // oftDateTimeMS
    ' INTEGER, ',                    // oftUnixTime
    ' INTEGER, ');                   // oftUnixMSTime


{ TOrmPropInfo }

constructor TOrmPropInfo.Create(const aName: RawUtf8; aOrmFieldType:
  TOrmFieldType; aAttributes: TOrmPropInfoAttributes;
  aFieldWidth, aPropertyIndex: integer);
begin
  if aName = '' then
    raise EOrmException.CreateUtf8('Void name for %.Create', [self]);
  if aAuxiliaryRTreeField in aAttributes then
    // '_NormalField' -> 'NormalField'
    fName := copy(aName, 2, MaxInt)
  else
    fName := aName;
  fNameUnflattened := fName;
  fOrmFieldType := aOrmFieldType;
  fOrmFieldTypeStored := aOrmFieldType;
  fSqlDBFieldType := SQLFIELDTYPETODBFIELDTYPE[fOrmFieldTypeStored];
  fAttributes := aAttributes;
  fFieldWidth := aFieldWidth;
  fPropertyIndex := aPropertyIndex;
end;

const
  NULL_SHORTSTRING: string[1] = '';

function TOrmPropInfo.GetOrmFieldTypeName: PShortString;
begin
  if self = nil then
    result := @NULL_SHORTSTRING
  else
    result := ToText(fOrmFieldType);
end;

function TOrmPropInfo.GetSqlFieldRttiTypeName: RawUtf8;
begin
  result := GetDisplayNameFromClass(ClassType);
  if IdemPChar(pointer(result), 'PROPINFO') then
    delete(result, 1, 8);
end;

function TOrmPropInfo.GetNameDisplay: string;
begin
  GetCaptionFromPCharLen(pointer(fName), result);
end;

procedure TOrmPropInfo.TextToBinary(Value: PUtf8Char; var result: RawByteString);
begin
  result := BlobToRawBlob(Value);
end;

procedure TOrmPropInfo.BinaryToText(var Value: RawUtf8; ToSql: boolean;
  wasSqlString: PBoolean);
begin
  if Value = '' then
  begin
    if wasSqlString <> nil then
      wasSqlString^ := false;
    Value := NULL_STR_VAR;
  end
  else
  begin
    if wasSqlString <> nil then
      wasSqlString^ := true;
    if ToSql then
      // encode as BLOB literals (e.g. "X'53514C697465'")
      Value := RawBlobToBlob(RawBlob(Value))
    else
      // JSON content is e.g. '\uFFF0base64encodedbinary'
      Value := BinToBase64WithMagic(Value);
  end;
end;

function TOrmPropInfo.GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal;
var
  tmp: RawUtf8;
begin
  GetValueVar(Instance, false, tmp, nil);
  result := DefaultHasher(0, pointer(tmp), length(tmp));
end;

procedure TOrmPropInfo.GetJsonValues(Instance: TObject; W: TTextWriter);
var
  wasString: boolean;
  tmp: RawUtf8;
begin
  GetValueVar(Instance, false, tmp, @wasString);
  if wasString then
  begin
    W.Add('"');
    if tmp <> '' then
      W.AddJsonEscape(pointer(tmp));
    W.Add('"');
  end
  else
    W.AddRawJson(tmp);
end;

function TOrmPropInfo.GetValue(Instance: TObject; ToSql: boolean;
  wasSqlString: PBoolean): RawUtf8;
begin
  GetValueVar(Instance, ToSql, result, wasSqlString);
end;

procedure TOrmPropInfo.SetValueVar(Instance: TObject; const Value: RawUtf8;
  wasString: boolean);
begin
  SetValue(Instance, pointer(Value), length(Value), wasString);
end;

function TOrmPropInfo.SqlDBFieldTypeName: PShortString;
begin
  result := ToText(fSqlDBFieldType);
end;

procedure TOrmPropInfo.GetFieldSqlVar(Instance: TObject; var aValue: TSqlVar;
  var temp: RawByteString);
begin
  GetValueVar(Instance, true, RawUtf8(temp), nil);
  aValue.Options := [];
  aValue.VType := fSqlDBFieldType;
  case aValue.VType of
    ftInt64:
      SetInt64(pointer(temp), aValue.VInt64);
    ftCurrency:
      aValue.VInt64 := StrToCurr64(pointer(temp));
    ftDouble:
      aValue.VDouble := GetExtended(pointer(temp));
    ftDate:
      aValue.VDateTime := Iso8601ToDateTime(temp);
    ftBlob:
      if temp = '' then
        aValue.VType := ftNull
      else
      begin
        temp := BlobToRawBlob(temp);
        aValue.VBlob := pointer(temp);
        aValue.VBlobLen := length(temp);
      end;
    ftUtf8:
      aValue.VText := pointer(temp);
  else
    aValue.VInt64 := 0;
  end;
end;

function TOrmPropInfo.IsValueVoid(Instance: TObject): boolean;
var
  temp: RawUtf8;
  wasString: boolean;
begin
  GetValueVar(Instance, true, temp, @wasString);
  if wasString then
    result := temp = ''
  else
    result := GetInt64(pointer(temp)) = 0;
end;

function TOrmPropInfo.SetFieldSqlVar(Instance: TObject;
  const aValue: TSqlVar): boolean;
begin
  case aValue.VType of
    ftInt64:
      SetValueVar(Instance, Int64ToUtf8(aValue.VInt64), false);
    ftCurrency:
      SetValueVar(Instance, Curr64ToStr(aValue.VInt64), false);
    ftDouble:
      SetValueVar(Instance, DoubleToStr(aValue.VDouble), false);
    ftDate:
      SetValueVar(Instance, DateTimeToIso8601Text(aValue.VDateTime, 'T',
        svoDateWithMS in aValue.Options), true);
    ftBlob:
      SetValueVar(Instance, RawBlobToBlob(aValue.VBlob, aValue.VBlobLen), true);
    ftUtf8:
      SetValue(Instance, aValue.VText, StrLen(aValue.VText), true);
  else
    SetValue(Instance, nil, 0, false);
  end;
  result := true;
end;

const
  NULL_LOW  = ord('n') + ord('u') shl 8 + ord('l') shl 16 + ord('l') shl 24;
  FALSE_LOW = ord('f') + ord('a') shl 8 + ord('l') shl 16 + ord('s') shl 24;
  TRUE_LOW  = ord('t') + ord('r') shl 8 + ord('u') shl 16 + ord('e') shl 24;

procedure TOrmPropInfo.GetVariant(Instance: TObject; var Dest: Variant);
var
  temp: RawUtf8;
begin
  GetValueVar(Instance, true, temp, nil);
  ValueVarToVariant(pointer(temp), Length(temp), fOrmFieldTypeStored,
    TVarData(Dest), false, nil);
end;

procedure TOrmPropInfo.SetVariant(Instance: TObject; const Source: Variant);
begin
  SetValueVar(Instance, VariantToUtf8(Source),
    (TVarData(Source).VType = varOleStr) or
    (TVarData(Source).VType >= varString));
end;

function TOrmPropInfo.CompareValue(Item1, Item2: TObject;
  CaseInsensitive: boolean): integer;
var
  tmp1, tmp2: RawUtf8;
begin
  if Item1 = Item2 then
    result := 0
  else if Item1 = nil then
    result := -1
  else if Item2 = nil then
    result := 1
  else
  begin
    // slow, always working implementation
    GetValueVar(Item1, false, tmp1, nil);
    GetValueVar(Item2, false, tmp2, nil);
    result := StrCompByCase[CaseInsensitive](pointer(tmp1), pointer(tmp2));
  end;
end;

function TOrmPropInfo.EventCompare(const A, B): integer;
begin
  result := CompareValue(TObject(A), TObject(B), {CaseInsensitive=}false);
end;

function TOrmPropInfo.EventHash(const Elem): cardinal;
begin
  result := GetHash(TObject(Elem), {CaseInsensitive=}false);
end;

function TOrmPropInfo.EventCompareI(const A, B): integer;
begin
  result := CompareValue(TObject(A), TObject(B), {CaseInsensitive=}true);
end;

function TOrmPropInfo.EventHashI(const Elem): cardinal;
begin
  result := GetHash(TObject(Elem), {CaseInsensitive=}true);
end;

procedure GenericCopy(Source, Dest: TObject; SourceInfo, DestInfo: TOrmPropInfo);
var
  tmp: RawUtf8;
  wasString: boolean;
  val: variant;
begin
  if (DestInfo.OrmFieldType = oftVariant) or
     (SourceInfo.OrmFieldType = oftVariant) then
  begin
    // force (doc)variant JSON serialization, e.g. when mapping dynamic arrays
    SourceInfo.GetVariant(Source, val);
    DestInfo.SetVariant(Dest, val);
    exit;
  end;
  SourceInfo.GetValueVar(Source, false, tmp, @wasString);
  DestInfo.SetValueVar(Dest, tmp, wasString);
end;

procedure TOrmPropInfo.CopyProp(Source: TObject; DestInfo: TOrmPropInfo;
  Dest: TObject);
begin
  if (Source <> nil) and
     (DestInfo <> nil) and
     (Dest <> nil) then
  begin
    Source := Flattened(Source);
    Dest := DestInfo.Flattened(Dest);
    if DestInfo.ClassType = ClassType then
      CopySameClassProp(Source, DestInfo, Dest) // fast overriden method
    else
      GenericCopy(Source, Dest, self, DestInfo);
  end;
end;

function TOrmPropInfo.Flattened(Instance: TObject): TObject;
begin
  result := Instance;
end;

procedure TOrmPropInfo.CopyValue(Source, Dest: TObject);
begin
  CopySameClassProp(Source, self, Dest);
end;

procedure TOrmPropInfo.CopySameClassProp(Source: TObject;
  DestInfo: TOrmPropInfo; Dest: TObject);
var
  tmp: RawUtf8;
  wasString: boolean;
begin
  GetValueVar(Source, false, tmp, @wasString);
  DestInfo.SetValueVar(Dest, tmp, wasString);
end;


{ TOrmPropInfoRtti }

class procedure TOrmPropInfoRtti.RegisterTypeInfo(aTypeInfo: PRttiInfo);
begin
  if OrmPropInfoRegistration = nil then
  begin
    GlobalLock;
    try
      if OrmPropInfoRegistration = nil then
        OrmPropInfoRegistration := TSynDictionary.Create(
          TypeInfo(TPointerDynArray), TypeInfo(TPointerDynArray));
    finally
      GlobalUnLock;
    end;
  end;
  OrmPropInfoRegistration.AddOrUpdate(aTypeInfo, self);
end;

constructor TOrmPropInfoRtti.Create(aPropInfo: PRttiProp; aPropIndex: integer;
  aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions);
var
  attrib: TOrmPropInfoAttributes;
  call: TMethod;
begin
  byte(attrib) := 0;
  if aPropInfo^.IsStored(nil) = AS_UNIQUE then
    Include(attrib, aIsUnique); // property MyProperty: RawUtf8 stored AS_UNIQUE;
  if (pilAuxiliaryFields in aOptions) and
     (aPropInfo^.Name^[1] = '_') then
    Include(attrib, aAuxiliaryRTreeField);
  inherited Create(aPropInfo^.NameUtf8, aOrmFieldType, attrib, aPropInfo^.Index,
    aPropIndex); // property MyProperty: RawUtf8 index 10; -> FieldWidth=10
  fPropInfo := aPropInfo;
  fPropType := aPropInfo^.TypeInfo;
  fPropRtti := Rtti.RegisterType(fPropType) as TRttiJson;
  if fPropRtti = nil then
    raise EOrmException.CreateUtf8('%.Create(%): unknown type',
      [self, aPropInfo^.Name^]);
  if aPropInfo.Setter(nil, @call) = rpcField then
    fSetterIsFieldPropOffset := PtrInt(call.Data);
  if aPropInfo.GetterIsField then
  begin
    fGetterIsFieldPropOffset := PtrUInt(fPropInfo.GetterAddr(nil));
    if fGetterIsFieldPropOffset = fSetterIsFieldPropOffset then
      fInPlaceCopySameClassPropOffset := fGetterIsFieldPropOffset;
  end;
end;

class function TOrmPropInfoRtti.CreateFrom(aPropInfo: PRttiProp;
  aPropIndex: integer; aOptions: TOrmPropInfoListOptions;
  const aFlattenedProps: PRttiPropDynArray): TOrmPropInfo;
var
  aOrmFieldType: TOrmFieldType;
  aType: PRttiInfo;
  C: TOrmPropInfoRttiClass;

  procedure FlattenedPropNameSet;
  var
    i, max: PtrInt;
    res: TOrmPropInfoRttiInstance absolute result;
  begin // Address.Street1 -> Address_Street1
    res.fFlattenedProps := aFlattenedProps;
    res.fNameUnflattened := result.Name;
    max := high(aFlattenedProps);
    for i := max downto 0 do
      res.fNameUnflattened :=
        ToUtf8(aFlattenedProps[i]^.Name^) + '.' + result.NameUnflattened;
    if (max >= 0) and
       (aFlattenedProps[max]^.TypeInfo^.ClassFieldCount(
         {withoutgetter=}pilIgnoreIfGetter in aOptions) = 1) then
    begin
      // Birth.Date -> Birth or Address.Country.Iso -> Address_Country
      res.fName := ToUtf8(aFlattenedProps[max]^.Name^);
      dec(max);
    end;
    for i := max downto 0 do
      res.fName := ToUtf8(aFlattenedProps[i]^.Name^) + '_' + result.Name;
  end;

begin
  if aPropInfo = nil then
    raise EOrmException.CreateUtf8('Invalid %.CreateFrom(nil) call', [self]);
  aType := aPropInfo^.TypeInfo;
  aOrmFieldType := GetOrmFieldType(aType); // guess from RTTI
  C := nil;
  result := nil;
  if (OrmPropInfoRegistration = nil) or
     not OrmPropInfoRegistration.FindAndCopy(aType, C) then
    // TypeInfo() was not in TOrmPropInfoRtti.RegisterTypeInfo -> use RTTI
    case aOrmFieldType of
      oftUnknown,
      oftBlobCustom:
        ; // will raise an EOrmException
      oftBoolean,
      oftEnumerate:
        C := TOrmPropInfoRttiEnum;
      oftTimeLog,
      oftModTime,
      oftCreateTime: // specific class for further use
        C := TOrmPropInfoRttiTimeLog;
      oftUnixTime:   // specific class for further use
        C := TOrmPropInfoRttiUnixTime;
      oftUnixMSTime:
        C := TOrmPropInfoRttiUnixMSTime;
      oftCurrency:
        C := TOrmPropInfoRttiCurrency;
      oftDateTime,
      oftDateTimeMS:
        C := TOrmPropInfoRttiDateTime;
      oftID:    // = TOrm(aID)
        C := TOrmPropInfoRttiIDClass;  // = mormot.orm.core TOrmPropInfoRttiID
      oftTID:   // = TID or T*ID
        C := TOrmPropInfoRttiTIDClass; // = mormot.orm.core TOrmPropInfoRttiTID
      oftSessionUserID:
        C := TOrmPropInfoRttiInt64;
      oftRecord: // = TRecordReference/TRecordReferenceToBeDeleted
        C := TOrmPropInfoRttiRecordReference;
      oftRecordVersion:
        C := TOrmPropInfoRttiRecordVersion;
      oftMany:
        C := TOrmPropInfoRttiMany;
      oftObject:
        C := TOrmPropInfoRttiObject;
      oftVariant:
        begin
          aOrmFieldType := NullableTypeToOrmFieldType(aType);
          if aOrmFieldType = oftUnknown then // no oftNullable type
            aOrmFieldType := oftVariant;     // = regular variant stored as JSON
          C := TOrmPropInfoRttiVariant;
        end;
      oftBlob:
        C := TOrmPropInfoRttiRawBlob;
      oftBlobDynArray:
        C := TOrmPropInfoRttiDynArray;
      oftUtf8Custom: // happens only for DELPHI XE5 and up
        result := TOrmPropInfoCustomJson.Create(aPropInfo, aPropIndex);
    else
      case aType^.Kind of // guess most precise class from RTTI
        rkInteger:
          C := TOrmPropInfoRttiInt32;
        rkSet:
          C := TOrmPropInfoRttiSet;
        rkChar,
        rkWChar:
          C := TOrmPropInfoRttiChar;
        {$ifdef FPC}
        rkQWord,
        {$endif FPC}
        rkInt64:
          C := TOrmPropInfoRttiInt64;
        rkFloat:
          if aType^.RttiFloat = rfDouble then
            C := TOrmPropInfoRttiDouble;
        rkLString:
          case aType^.AnsiStringCodePage of
            // recognize optimized UTF-8/UTF-16
            CP_UTF8:
              C := TOrmPropInfoRttiRawUtf8;
            CP_UTF16:
              C := TOrmPropInfoRttiRawUnicode;
          else
            C := TOrmPropInfoRttiAnsi; // will use the right TSynAnsiConvert
          end;
      {$ifdef HASVARUSTRING}
        rkUString:
          C := TOrmPropInfoRttiUnicode;
      {$endif HASVARUSTRING}
        rkWString:
          C := TOrmPropInfoRttiWide;
      end;
    end;
  if C <> nil then
    result := C.Create(aPropInfo, aPropIndex, aOrmFieldType, aOptions);
  if result <> nil then
  begin
    if aFlattenedProps <> nil then
      FlattenedPropNameSet;
  end
  else if pilRaiseEOrmExceptionIfNotHandled in aOptions then
    raise EOrmException.CreateUtf8(
      '%.CreateFrom: Unhandled %/% type for property %',
      [self, ToText(aOrmFieldType)^, ToText(aType^.Kind)^, aPropInfo^.Name^]);
end;

function TOrmPropInfoRtti.GetSqlFieldRttiTypeName: RawUtf8;
begin
  result := ToUtf8(fPropType^.Name^);
end;

function TOrmPropInfoRtti.GetRttiCustomProp(Instance: TObject): PRttiCustomProp;
begin
  if (fRttiCustomProp = nil) and
     (Instance <> nil) then
    fRttiCustomProp := Rtti.RegisterClass(Instance).Props.Find(fName);
  result := fRttiCustomProp;
end;

function TOrmPropInfoRtti.GetFieldAddr(Instance: TObject): pointer;
begin
  if Instance = nil then
    result := nil
  else
    result := fPropInfo^.GetFieldAddr(Instance);
end;

function TOrmPropInfoRtti.Flattened(Instance: TObject): TObject;
var
  i: PtrInt;
begin
  result := Instance;
  for i := 0 to length(fFlattenedProps) - 1 do
    result := fFlattenedProps[i].GetObjProp(result);
end;

procedure TOrmPropInfoRtti.GetVariant(Instance: TObject; var Dest: Variant);
var
  temp: RawUtf8;
begin
  GetValueVar(Instance, true, temp, nil);
  ValueVarToVariant(pointer(temp), length(temp), fOrmFieldTypeStored,
    TVarData(Dest), false, fPropInfo^.TypeInfo);
end;


{ TOrmPropInfoRttiInt32 }

constructor TOrmPropInfoRttiInt32.Create(aPropInfo: PRttiProp;
  aPropIndex: integer; aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions);
begin
  inherited Create(aPropInfo, aPropIndex, aOrmFieldType, aOptions);
  fUnsigned := fPropType^.RttiOrd in [roUByte, roUWord, roULong];
  if fPropType^.RttiOrd = roSLong then
    fIntegerGetPropOffset := fGetterIsFieldPropOffset <> 0;
  if fPropType^.RttiOrd in [roSLong, roULong] then
    fIntegerSetPropOffset := fSetterIsFieldPropOffset <> 0;
end;

procedure TOrmPropInfoRttiInt32.CopySameClassProp(Source: TObject;
  DestInfo: TOrmPropInfo; Dest: TObject);
begin
  TOrmPropInfoRttiInt32(DestInfo).fPropInfo.SetOrdProp(
    Dest, fPropInfo.GetOrdProp(Source));
end;

procedure TOrmPropInfoRttiInt32.GetBinary(Instance: TObject; W: TBufferWriter);
begin
  W.WriteVarUInt32(cardinal(fPropInfo.GetOrdProp(Instance)));
end;

function TOrmPropInfoRttiInt32.GetHash(Instance: TObject;
  CaseInsensitive: boolean): cardinal;
var
  v: integer;
begin
  v := fPropInfo.GetOrdProp(Instance);
  result := DefaultHasher(0, @v, 4);
end;

procedure TOrmPropInfoRttiInt32.GetJsonValues(Instance: TObject; W: TTextWriter);
var
  v: integer;
begin
  if fIntegerGetPropOffset then // roSLong without any getter
    W.Add(PInteger(PtrUInt(Instance) + fGetterIsFieldPropOffset)^)
  else
  begin
    v := fPropInfo.GetOrdProp(Instance);
    if fUnsigned then
      W.AddU(cardinal(v))
    else
      W.Add(v);
  end;
end;

procedure TOrmPropInfoRttiInt32.GetValueVar(Instance: TObject; ToSql: boolean;
  var result: RawUtf8; wasSqlString: PBoolean);
var
  v: integer;
begin
  if wasSqlString <> nil then
    wasSqlString^ := false;
  v := fPropInfo.GetOrdProp(Instance);
  if fUnsigned then
    UInt32ToUtf8(cardinal(v), result)
  else
    Int32ToUtf8(v, result);
end;

procedure TOrmPropInfoRttiInt32.NormalizeValue(var Value: RawUtf8);
var
  err, v: integer;
begin
  v := GetInteger(pointer(Value), err);
  if err <> 0 then
    Value := ''
  else if fUnsigned then
    UInt32ToUtf8(cardinal(v), Value)
  else
    Int32ToUtf8(v, Value);
end;

function TOrmPropInfoRttiInt32.CompareValue(Item1, Item2: TObject;
  CaseInsensitive: boolean): integer;
begin
  if Item1 = Item2 then
    result := 0
  else if Item1 = nil then
    result := -1
  else if Item2 = nil then
    result := 1
  else if fIntegerGetPropOffset then
    result := CompareInteger( // roSLong inlined comparison
      PInteger(PtrUInt(Item1) + fGetterIsFieldPropOffset)^,
      PInteger(PtrUInt(Item2) + fGetterIsFieldPropOffset)^)
  else
    result := CompareInteger( // use RTTI for any other kind of integers
      fPropInfo.GetOrdProp(Item1), fPropInfo.GetOrdProp(Item2));
end;

procedure TOrmPropInfoRttiInt32.SetBinary(Instance: TObject; var Read: TFastReader);
begin
  fPropInfo.SetOrdProp(Instance, Read.VarUInt32);
end;

procedure TOrmPropInfoRttiInt32.SetValue(Instance: TObject; Value: PUtf8Char;
  ValueLen: PtrInt; wasString: boolean);
var
  i32: integer;
begin
  i32 := GetInteger(Value);
  if fIntegerSetPropOffset then
    PInteger(PtrUInt(Instance) + fSetterIsFieldPropOffset)^ := i32
  else
    fPropInfo.SetOrdProp(Instance, i32);
end;

function TOrmPropInfoRttiInt32.SetFieldSqlVar(Instance: TObject;
  const aValue: TSqlVar): boolean;
begin
  if aValue.VType = ftInt64 then
  begin
    fPropInfo.SetOrdProp(Instance, aValue.VInt64);
    result := true;
  end
  else
    result := inherited SetFieldSqlVar(Instance, aValue);
end;

procedure TOrmPropInfoRttiInt32.GetFieldSqlVar(Instance: TObject;
  var aValue: TSqlVar; var temp: RawByteString);
var
  v: integer;
begin
  aValue.Options := [];
  aValue.VType := ftInt64;
  v := fPropInfo.GetOrdProp(Instance);
  if fUnsigned then
    aValue.VInt64 := cardinal(v)
  else
    aValue.VInt64 := v;
end;


{ TOrmPropInfoRttiSet }

constructor TOrmPropInfoRttiSet.Create(aPropInfo: PRttiProp; aPropIndex: integer;
  aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions);
begin
  inherited Create(aPropInfo, aPropIndex, aOrmFieldType, aOptions);
  fSetEnumType := fPropType^.SetEnumType;
end;


{ TOrmPropInfoRttiEnum }

constructor TOrmPropInfoRttiEnum.Create(aPropInfo: PRttiProp;
  aPropIndex: integer; aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions);
begin
  inherited Create(aPropInfo, aPropIndex, aOrmFieldType, aOptions);
  fEnumType := fPropType^.EnumBaseType;
end;

procedure TOrmPropInfoRttiEnum.GetJsonValues(Instance: TObject; W: TTextWriter);
var
  i: PtrInt;
begin
  i := fPropInfo.GetOrdProp(Instance);
  if fOrmFieldType = oftBoolean then
    W.Add(i <> 0)
  else
    W.Add(i);
end;

function TOrmPropInfoRttiEnum.GetCaption(Value: RawUtf8; out IntValue: integer): string;
begin
  NormalizeValue(Value);
  IntValue := GetInteger(pointer(Value));
  if Value = '' then
    result := ''
  else
    result := EnumType^.GetCaption(IntValue);
end;

procedure TOrmPropInfoRttiEnum.GetValueVar(Instance: TObject; ToSql: boolean;
  var result: RawUtf8; wasSqlString: PBoolean);
var
  i: PtrInt;
begin
  if wasSqlString <> nil then
    wasSqlString^ := false;
  i := fPropInfo.GetOrdProp(Instance);
  if (fOrmFieldType = oftBoolean) and not ToSql then
    result := BOOL_UTF8[i <> 0]
  else
    UInt32ToUtf8(i, result);
end;

procedure TOrmPropInfoRttiEnum.NormalizeValue(var Value: RawUtf8);
var
  i, err: integer;
begin
  i := GetInteger(pointer(Value), err);
  if err <> 0 then
    // we allow a value stated as text
    if fOrmFieldType = oftBoolean then
      i := Ord(IdemPropNameU(Value, 'TRUE') or IdemPropNameU(Value, 'YES'))
    else
      i := fEnumType^.GetEnumNameValue(pointer(Value), length(Value))
  else if fOrmFieldType = oftBoolean then // normalize boolean values range to 0,1
    if i <> 0 then
      i := 1;
  if cardinal(i) > cardinal(fEnumType^.MaxValue) then
    Value := ''
  else
    // only set a valid value
    UInt32ToUtf8(i, Value);
end;

procedure TOrmPropInfoRttiEnum.SetValue(Instance: TObject; Value: PUtf8Char;
  ValueLen: PtrInt; wasString: boolean);
var
  i, err: integer;
begin
  if Value = nil then
    i := 0
  else
  begin
    i := GetInteger(Value, err);
    if err <> 0 then
    begin
      // we allow a value stated as text
      if fOrmFieldType = oftBoolean then
        i := ord(IdemPropName('TRUE', Value, ValueLen) or
                 IdemPropName('YES', Value, ValueLen))
      else
        i := fEnumType^.GetEnumNameValue(Value); // -> convert into integer
      if cardinal(i) > cardinal(fEnumType^.MaxValue) then
        i := 0;  // only set a valid text value
    end
    else if fOrmFieldType = oftBoolean then // normalize boolean values range to 0,1
      if i <> 0 then
        i := 1;
  end;
  fPropInfo.SetOrdProp(Instance, i);
end;


{ TOrmPropInfoRttiChar }

procedure TOrmPropInfoRttiChar.GetValueVar(Instance: TObject; ToSql: boolean;
  var result: RawUtf8; wasSqlString: PBoolean);
var
  w: WideChar;
begin
  w := WideChar(fPropInfo.GetOrdProp(Instance));
  if ToSql and
     (w = #0) then
  begin
    // 'null' and not #0 to avoid end of SQL text - JSON will escape #0
    result := NULL_STR_VAR;
    if wasSqlString <> nil then
      wasSqlString^ := false;
  end
  else
  begin
    RawUnicodeToUtf8(@w, 1, result);
    if wasSqlString <> nil then
      wasSqlString^ := true;
  end;
end;

procedure TOrmPropInfoRttiChar.NormalizeValue(var Value: RawUtf8);
begin
  // do nothing: should already be UTF-8 encoded
end;

procedure TOrmPropInfoRttiChar.SetValue(Instance: TObject; Value: PUtf8Char;
  ValueLen: PtrInt; wasString: boolean);
var
  i: integer;
begin
  if (Value = nil) or
     (PInteger(Value)^ = NULL_LOW) then
    i := 0
  else
    // decode one UTF-16 or return UNICODE_REPLACEMENT_CHARACTER
    i := GetUtf8WideChar(Value);
  fPropInfo.SetOrdProp(Instance, i);
end;


{ TOrmPropInfoRttiInt64 }

constructor TOrmPropInfoRttiInt64.Create(aPropInfo: PRttiProp;
  aPropIndex: integer; aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions);
begin
  inherited Create(aPropInfo, aPropIndex, aOrmFieldType, aOptions);
  fIsQWord := fPropType^.IsQword;
end;

procedure TOrmPropInfoRttiInt64.CopySameClassProp(Source: TObject;
  DestInfo: TOrmPropInfo; Dest: TObject);
begin
  TOrmPropInfoRttiInt64(DestInfo).fPropInfo.SetInt64Prop(
    Dest, fPropInfo.GetInt64Prop(Source));
end;

procedure TOrmPropInfoRttiInt64.GetBinary(Instance: TObject; W: TBufferWriter);
var
  V64: Int64;
begin
  V64 := fPropInfo.GetInt64Prop(Instance);
  W.Write(@V64, SizeOf(Int64));
end;

function TOrmPropInfoRttiInt64.GetHash(Instance: TObject;
  CaseInsensitive: boolean): cardinal;
var
  V64: Int64;
begin
  if fGetterIsFieldPropOffset <> 0 then
    V64 := PInt64(PtrUInt(Instance) + fGetterIsFieldPropOffset)^
  else
    V64 := fPropInfo.GetInt64Prop(Instance);
  result := DefaultHasher(0, @V64, SizeOf(V64));
end;

procedure TOrmPropInfoRttiInt64.GetJsonValues(Instance: TObject; W: TTextWriter);
var
  V64: Int64;
begin
  if fGetterIsFieldPropOffset <> 0 then
    V64 := PInt64(PtrUInt(Instance) + fGetterIsFieldPropOffset)^
  else
    V64 := fPropInfo.GetInt64Prop(Instance);
  if fIsQWord then
    W.AddQ(V64)
  else
    W.Add(V64);
end;

procedure TOrmPropInfoRttiInt64.GetValueVar(Instance: TObject; ToSql: boolean;
  var result: RawUtf8; wasSqlString: PBoolean);
var
  V64: Int64;
begin
  if wasSqlString <> nil then
    wasSqlString^ := false;
  if fGetterIsFieldPropOffset <> 0 then
    V64 := PInt64(PtrUInt(Instance) + fGetterIsFieldPropOffset)^
  else
    V64 := fPropInfo.GetInt64Prop(Instance);
  if fIsQWord then
    UInt64ToUtf8(V64, result)
  else
    Int64ToUtf8(V64, result);
end;

procedure TOrmPropInfoRttiInt64.NormalizeValue(var Value: RawUtf8);
var
  err: integer;
  V64: Int64;
begin
  if fIsQWord then
  begin
    V64 := GetQWord(pointer(Value), err);
    if err <> 0 then
      Value := ''
    else
      UInt64ToUtf8(V64, Value);
  end
  else
  begin
    V64 := GetInt64(pointer(Value), err);
    if err <> 0 then
      Value := ''
    else
      Int64ToUtf8(V64, Value);
  end;
end;

function TOrmPropInfoRttiInt64.CompareValue(Item1, Item2: TObject;
  CaseInsensitive: boolean): integer;
var
  V1, V2: Int64;
begin
  if Item1 = Item2 then
    result := 0
  else if Item1 = nil then
    result := -1
  else if Item2 = nil then
    result := 1
  else
  begin
    if fGetterIsFieldPropOffset <> 0 then
    begin
      V1 := PInt64(PtrUInt(Item1) + fGetterIsFieldPropOffset)^;
      V2 := PInt64(PtrUInt(Item2) + fGetterIsFieldPropOffset)^;
    end
    else
    begin
      V1 := fPropinfo.GetInt64Prop(Item1);
      V2 := fPropinfo.GetInt64Prop(Item2);
    end;
    if fIsQWord then
      result := CompareQWord(V1, V2)
    else
      result := CompareInt64(V1, V2);
  end;
end;

procedure TOrmPropInfoRttiInt64.SetBinary(Instance: TObject; var Read: TFastReader);
begin
  fPropInfo.SetInt64Prop(Instance, Int64(Read.Next8));
end;

procedure TOrmPropInfoRttiInt64.SetValue(Instance: TObject; Value: PUtf8Char;
  ValueLen: PtrInt; wasString: boolean);
var
  V64: Int64;
begin
  if fIsQWord then
    SetQWord(Value, PQword(@V64)^)
  else
    SetInt64(Value, V64{%H-});
  if fSetterIsFieldPropOffset <> 0 then
    PInt64(PtrUInt(Instance) + fSetterIsFieldPropOffset)^ := V64
  else
    fPropInfo.SetInt64Prop(Instance, V64);
end;

function TOrmPropInfoRttiInt64.SetFieldSqlVar(Instance: TObject;
  const aValue: TSqlVar): boolean;
begin
  if aValue.VType = ftInt64 then
  begin
    fPropInfo.SetInt64Prop(Instance, aValue.VInt64);
    result := true;
  end
  else
    result := inherited SetFieldSqlVar(Instance, aValue);
end;

procedure TOrmPropInfoRttiInt64.GetFieldSqlVar(Instance: TObject;
  var aValue: TSqlVar; var temp: RawByteString);
begin
  aValue.Options := [];
  aValue.VType := ftInt64;
  aValue.VInt64 := fPropInfo.GetInt64Prop(Instance);
end;


{ TOrmPropInfoRttiDouble }

procedure TOrmPropInfoRttiDouble.CopySameClassProp(Source: TObject;
  DestInfo: TOrmPropInfo; Dest: TObject);
begin
  TOrmPropInfoRttiDouble(DestInfo).fPropInfo.SetDoubleProp(
    Dest, fPropInfo.GetDoubleProp(Source));
end;

procedure TOrmPropInfoRttiDouble.GetJsonValues(Instance: TObject; W: TTextWriter);
var
  V: double;
begin
  if fGetterIsFieldPropOffset <> 0 then
    V := unaligned(PDouble(PtrUInt(Instance) + fGetterIsFieldPropOffset)^)
  else
    V := fPropInfo.GetDoubleProp(Instance);
  W.AddDouble(V);
end;

procedure TOrmPropInfoRttiDouble.GetValueVar(Instance: TObject; ToSql: boolean;
  var result: RawUtf8; wasSqlString: PBoolean);
begin
  DoubleToStr(fPropInfo.GetDoubleProp(Instance), result);
  if wasSqlString <> nil then
    wasSqlString^ := (result = '') or
                     not (result[1] in ['0'..'9']);
end;

procedure TOrmPropInfoRttiDouble.NormalizeValue(var Value: RawUtf8);
var
  VFloat: TSynExtended;
  err: integer;
begin
  VFloat := GetExtended(pointer(Value), err);
  if err <> 0 then
    Value := ''
  else
    DoubleToStr(VFloat, Value);
end;

procedure TOrmPropInfoRttiDouble.SetValue(Instance: TObject; Value: PUtf8Char;
  ValueLen: PtrInt; wasString: boolean);
var
  V: double;
  err: integer;
begin
  if Value = nil then
    V := 0
  else
  begin
    V := GetExtended(pointer(Value), err);
    if err <> 0 then
      V := 0;
  end;
  if fSetterIsFieldPropOffset <> 0 then
    unaligned(PDouble(PtrUInt(Instance) + fSetterIsFieldPropOffset)^) := V
  else
    fPropInfo.SetDoubleProp(Instance, V);
end;

function TOrmPropInfoRttiDouble.CompareValue(Item1, Item2: TObject;
  CaseInsensitive: boolean): integer;
begin
  if Item1 = Item2 then
    result := 0
  else if Item1 = nil then
    result := -1
  else if Item2 = nil then
    result := 1
  else
    result := CompareFloat(
      fPropInfo.GetDoubleProp(Item1), fPropInfo.GetDoubleProp(Item2));
end;

function TOrmPropInfoRttiDouble.GetHash(Instance: TObject;
  CaseInsensitive: boolean): cardinal;
var
  V: double;
begin
  V := fPropInfo.GetDoubleProp(Instance);
  result := DefaultHasher(0, @V, SizeOf(V));
end;

procedure TOrmPropInfoRttiDouble.GetBinary(Instance: TObject; W: TBufferWriter);
var
  V: double;
begin
  V := fPropInfo.GetDoubleProp(Instance);
  W.Write(@V, SizeOf(V));
end;

procedure TOrmPropInfoRttiDouble.SetBinary(Instance: TObject; var Read: TFastReader);
var
  V: double;
begin
  Read.Copy(@V, SizeOf(V));
  fPropInfo.SetDoubleProp(Instance, V);
end;

function TOrmPropInfoRttiDouble.SetFieldSqlVar(Instance: TObject;
  const aValue: TSqlVar): boolean;
var
  V: double;
begin
  case aValue.VType of
    ftCurrency:
      V := aValue.VCurrency;
    ftDouble, ftDate:
      V := aValue.VDouble;
    ftInt64:
      V := aValue.VInt64;
  else
    begin
      result := inherited SetFieldSqlVar(Instance, aValue);
      exit;
    end;
  end;
  fPropInfo.SetDoubleProp(Instance, V);
  result := true;
end;

procedure TOrmPropInfoRttiDouble.GetFieldSqlVar(Instance: TObject;
  var aValue: TSqlVar; var temp: RawByteString);
begin
  aValue.Options := [];
  aValue.VType := ftDouble;
  aValue.VDouble := fPropInfo.GetDoubleProp(Instance);
end;


{ TOrmPropInfoRttiCurrency }

procedure TOrmPropInfoRttiCurrency.CopySameClassProp(Source: TObject;
  DestInfo: TOrmPropInfo; Dest: TObject);
var
  curr: currency;
begin
  fPropInfo.GetCurrencyProp(Source, curr);
  TOrmPropInfoRttiCurrency(DestInfo).fPropInfo.SetCurrencyProp(Dest, curr);
end;

procedure TOrmPropInfoRttiCurrency.GetJsonValues(Instance: TObject; W: TTextWriter);
var
  curr: currency;
begin
  fPropInfo.GetCurrencyProp(Instance, curr);
  W.AddCurr(curr);
end;

procedure TOrmPropInfoRttiCurrency.GetValueVar(Instance: TObject; ToSql: boolean;
  var result: RawUtf8; wasSqlString: PBoolean);
var
  curr: currency;
begin
  if wasSqlString <> nil then
    wasSqlString^ := false;
  fPropInfo.GetCurrencyProp(Instance, curr);
  result := CurrencyToStr(curr);
end;

procedure TOrmPropInfoRttiCurrency.NormalizeValue(var Value: RawUtf8);
begin
  Value := Curr64ToStr(StrToCurr64(pointer(Value)));
end;

procedure TOrmPropInfoRttiCurrency.SetValue(Instance: TObject; Value: PUtf8Char;
  ValueLen: PtrInt; wasString: boolean);
var
  tmp: Int64;
begin
  tmp := StrToCurr64(Value, nil);
  if fSetterIsFieldPropOffset <> 0 then
    PInt64(PtrUInt(Instance) + fSetterIsFieldPropOffset)^ := tmp
  else
    fPropInfo.SetCurrencyProp(Instance, PCurrency(@tmp)^);
end;

function TOrmPropInfoRttiCurrency.CompareValue(Item1, Item2: TObject;
  CaseInsensitive: boolean): integer;
var
  V1, V2: currency;
begin
  if Item1 = Item2 then
    result := 0
  else if Item1 = nil then
    result := -1
  else if Item2 = nil then
    result := 1
  else
  begin
    fPropInfo.GetCurrencyProp(Item1, V1);
    fPropInfo.GetCurrencyProp(Item2, V2);
    result := CompareInt64(PInt64(@V1)^, PInt64(@V2)^);
  end;
end;

function TOrmPropInfoRttiCurrency.GetHash(Instance: TObject;
  CaseInsensitive: boolean): cardinal;
var
  V: currency;
begin
  fPropInfo.GetCurrencyProp(Instance, V);
  result := DefaultHasher(0, @V, SizeOf(V));
end;

procedure TOrmPropInfoRttiCurrency.GetFieldSqlVar(Instance: TObject;
  var aValue: TSqlVar; var temp: RawByteString);
begin
  aValue.Options := [];
  aValue.VType := ftCurrency;
  fPropInfo.GetCurrencyProp(Instance, aValue.VCurrency);
end;

function TOrmPropInfoRttiCurrency.SetFieldSqlVar(Instance: TObject;
  const aValue: TSqlVar): boolean;
var
  V: currency;
begin
  case aValue.VType of
    ftDouble, ftDate:
      V := aValue.VDouble;
    ftInt64:
      V := aValue.VInt64;
    ftCurrency:
      V := aValue.VCurrency;
  else
    begin
      result := inherited SetFieldSqlVar(Instance, aValue);
      exit;
    end;
  end;
  fPropInfo.SetCurrencyProp(Instance, PCurrency(@V)^);
  result := true;
end;

procedure TOrmPropInfoRttiCurrency.GetBinary(Instance: TObject; W: TBufferWriter);
var
  V: currency;
begin
  fPropInfo.GetCurrencyProp(Instance, V);
  W.Write(@V, SizeOf(V));
end;

procedure TOrmPropInfoRttiCurrency.SetBinary(Instance: TObject; var Read: TFastReader);
var
  V: currency;
begin
  Read.Copy(@V, SizeOf(V));
  fPropInfo.SetCurrencyProp(Instance, V);
end;


{ TOrmPropInfoRttiDateTime }

procedure TOrmPropInfoRttiDateTime.GetJsonValues(Instance: TObject; W: TTextWriter);
begin
  W.Add('"');
  W.AddDateTime(fPropInfo.GetDoubleProp(Instance), fOrmFieldType = oftDateTimeMS);
  W.Add('"');
end;

function TOrmPropInfoRttiDateTime.CompareValue(Item1, Item2: TObject;
  CaseInsensitive: boolean): integer;
const
  PRECISION: array[boolean] of double = (1 / SecsPerDay, 1 / MSecsPerDay);
var
  V1, V2: double;
begin
  if Item1 = Item2 then
    result := 0
  else if Item1 = nil then
    result := -1
  else if Item2 = nil then
    result := 1
  else
  begin
    V1 := fPropInfo.GetDoubleProp(Item1);
    V2 := fPropInfo.GetDoubleProp(Item2);
    if mormot.core.base.SameValue(V1, V2, PRECISION[fOrmFieldType = oftDateTimeMS]) then
      result := 0
    else if V1 > V2 then
      result := 1
    else
      result := -1;
  end;
end;

procedure TOrmPropInfoRttiDateTime.GetValueVar(Instance: TObject; ToSql: boolean;
  var result: RawUtf8; wasSqlString: PBoolean);
begin
  if wasSqlString <> nil then
    wasSqlString^ := true;
  DateTimeToIso8601TextVar(fPropInfo.GetDoubleProp(Instance), 'T', result,
    fOrmFieldType = oftDateTimeMS);
end;

procedure TOrmPropInfoRttiDateTime.NormalizeValue(var Value: RawUtf8);
begin
  DateTimeToIso8601TextVar(Iso8601ToDateTime(Value), 'T', Value,
    {withms=}fOrmFieldType = oftDateTimeMS);
end;

procedure TOrmPropInfoRttiDateTime.SetValue(Instance: TObject; Value: PUtf8Char;
  ValueLen: PtrInt; wasString: boolean);
var
  V: TDateTime;
begin
  Iso8601ToDateTimePUtf8CharVar(Value, ValueLen, V);
  if fSetterIsFieldPropOffset <> 0 then
    unaligned(PDouble(PtrUInt(Instance) + fSetterIsFieldPropOffset)^) := V
  else
    fPropInfo.SetDoubleProp(Instance, V);
end;

procedure TOrmPropInfoRttiDateTime.GetFieldSqlVar(Instance: TObject;
  var aValue: TSqlVar; var temp: RawByteString);
begin
  if fOrmFieldType = oftDateTimeMS then
    aValue.Options := [svoDateWithMS]
  else
    aValue.Options := [];
  aValue.VType := ftDate;
  aValue.VDouble := fPropInfo.GetDoubleProp(Instance);
end;


{ TOrmPropInfoRttiMany }

// TOrmMany stores nothing within the table

procedure TOrmPropInfoRttiMany.GetValueVar(Instance: TObject; ToSql: boolean;
  var result: RawUtf8; wasSqlString: PBoolean);
begin
  result := '';
end;

procedure TOrmPropInfoRttiMany.SetValue(Instance: TObject; Value: PUtf8Char;
  ValueLen: PtrInt; wasString: boolean);
begin
end;

procedure TOrmPropInfoRttiMany.GetBinary(Instance: TObject; W: TBufferWriter);
begin
end;

procedure TOrmPropInfoRttiMany.SetBinary(Instance: TObject; var Read: TFastReader);
begin
end;


{ TOrmPropInfoRttiInstance }

constructor TOrmPropInfoRttiInstance.Create(aPropInfo: PRttiProp;
  aPropIndex: integer; aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions);
begin
  inherited Create(aPropInfo, aPropIndex, aOrmFieldType, aOptions);
  fObjectClass := fPropType^.RttiClass^.RttiClass;
end;

function TOrmPropInfoRttiInstance.GetInstance(Instance: TObject): TObject;
begin
  if fGetterIsFieldPropOffset <> 0 then
    result := PObject(PtrUInt(Instance) + fGetterIsFieldPropOffset)^
  else
    result := fPropInfo.GetObjProp(Instance);
end;

procedure TOrmPropInfoRttiInstance.SetInstance(Instance, Value: TObject);
begin
  if fSetterIsFieldPropOffset <> 0 then
    PObject(PtrUInt(Instance) + fSetterIsFieldPropOffset)^ := Value
  else
    fPropInfo.SetOrdProp(Instance, PtrInt(Value));
end;


{ TOrmPropInfoRttiRecordReference }

constructor TOrmPropInfoRttiRecordReference.Create(aPropInfo: PRttiProp;
  aPropIndex: integer; aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions);
begin
  inherited Create(aPropInfo, aPropIndex, aOrmFieldType, aOptions);
  fCascadeDelete := IdemPropName(fPropType^.RawName, 'TRecordReferenceToBeDeleted')
end;


{ TOrmPropInfoRttiIObject }

procedure TOrmPropInfoRttiObject.CopySameClassProp(Source: TObject;
  DestInfo: TOrmPropInfo; Dest: TObject);
var
  S, D: TObject;
begin
  // generic case: copy also class content (create instances)
  S := GetInstance(Source);
  D := TOrmPropInfoRttiObject(DestInfo).GetInstance(Dest);
  case fPropRtti.ValueRtlClass of
    vcCollection:
      CopyCollection(TCollection(S), TCollection(D));
    vcStrings:
      CopyStrings(TStrings(S), TStrings(D));
    else
      begin
        D.Free; // release previous instance
        TOrmPropInfoRttiObject(DestInfo).SetInstance(Dest, CopyObject(S));
      end;
  end;
end;

procedure TOrmPropInfoRttiObject.SetValue(Instance: TObject; Value: PUtf8Char;
  ValueLen: PtrInt; wasString: boolean);
var
  valid: boolean;
  tmp: TSynTempBuffer;
begin
  tmp.Init(Value, ValueLen); // private copy since the buffer will be modified
  try
    PropertyFromJson(GetRttiCustomProp(Instance), Instance, tmp.buf, valid,
      JSONPARSER_TOLERANTOPTIONS);
  finally
    tmp.Done;
  end;
end;

procedure TOrmPropInfoRttiObject.GetValueVar(Instance: TObject; ToSql: boolean;
  var result: RawUtf8; wasSqlString: PBoolean);
begin
  if wasSqlString <> nil then
    wasSqlString^ := true;
  result := ObjectToJson(GetInstance(Instance));
end;

procedure TOrmPropInfoRttiObject.GetBinary(Instance: TObject; W: TBufferWriter);
begin
  // serialize object as JSON UTF-8 TEXT - not fast, but works
  W.Write(ObjectToJson(GetInstance(Instance)));
end;

procedure TOrmPropInfoRttiObject.SetBinary(Instance: TObject; var Read: TFastReader);
var
  valid: boolean;
  tmp: TSynTempBuffer;
begin
  // unserialize object from JSON UTF-8 TEXT - not fast, but works
  Read.VarBlob(tmp);
  try
    PropertyFromJson(GetRttiCustomProp(Instance), Instance, tmp.buf, valid,
      JSONPARSER_TOLERANTOPTIONS);
  finally
    tmp.Done;
  end;
  if not valid then
    Read.ErrorData('%.SetBinary: invalid JSON for %', [self, fName]);
end;

function TOrmPropInfoRttiObject.GetHash(Instance: TObject;
  CaseInsensitive: boolean): cardinal;
var
  tmp: RawUtf8;
begin
  // JSON is case-sensitive by design -> ignore CaseInsensitive parameter
  tmp := ObjectToJson(GetInstance(Instance));
  result := DefaultHasher(0, pointer(tmp), length(tmp));
end;

procedure TOrmPropInfoRttiObject.NormalizeValue(var Value: RawUtf8);
begin
  // do nothing: should already be normalized
end;

procedure TOrmPropInfoRttiObject.GetJsonValues(Instance: TObject; W: TTextWriter);
begin
  if W.InheritsFrom(TJsonSerializer) and
     (jwoAsJsonNotAsString in TJsonSerializer(W).OrmOptions) then
    W.WriteObject(GetInstance(Instance))
  else
    W.WriteObjectAsString(GetInstance(Instance));
end;


{ TOrmPropInfoRttiAnsi }

constructor TOrmPropInfoRttiAnsi.Create(aPropInfo: PRttiProp;
  aPropIndex: integer; aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions);
begin
  inherited;
  fEngine := fPropRtti.Cache.Engine;
end;

procedure TOrmPropInfoRttiAnsi.CopySameClassProp(Source: TObject;
  DestInfo: TOrmPropInfo; Dest: TObject);
var
  Value: RawByteString;
begin
  if TOrmPropInfoRttiAnsi(DestInfo).fEngine = fEngine then
  begin
    fPropInfo.GetLongStrProp(Source, Value);
    TOrmPropInfoRttiAnsi(DestInfo).fPropInfo.SetLongStrProp(Dest, Value);
  end
  else
  begin
    GetValueVar(Source, false, RawUtf8(Value), nil);
    DestInfo.SetValueVar(Dest, Value, true);
  end;
end;

procedure TOrmPropInfoRttiAnsi.GetBinary(Instance: TObject; W: TBufferWriter);
var
  Value: RawByteString;
begin
  fPropInfo.GetLongStrProp(Instance, Value);
  W.Write(Value);
end;

function TOrmPropInfoRttiAnsi.GetHash(Instance: TObject;
  CaseInsensitive: boolean): cardinal;
var
  Up: array[byte] of AnsiChar; // temp stack buffer (no heap allocation)
  Value: RawByteString;
begin
  fPropInfo.GetLongStrProp(Instance, Value);
  if CaseInsensitive then // 255 max chars is enough to avoid hashing collisions
    if fEngine.CodePage = CODEPAGE_US then
      result := DefaultHasher(0, Up{%H-}, UpperCopyWin255(Up{%H-}, Value) - {%H-}Up)
    else
      result := DefaultHasher(0, Up, UpperCopy255Buf(Up, pointer(Value), length(Value)) - Up)
  else
    result := DefaultHasher(0, pointer(Value), length(Value));
end;

procedure TOrmPropInfoRttiAnsi.GetValueVar(Instance: TObject; ToSql: boolean;
  var result: RawUtf8; wasSqlString: PBoolean);
var
  tmp: RawByteString;
begin
  if wasSqlString <> nil then
    wasSqlString^ := true;
  fPropInfo.GetLongStrProp(Instance, tmp);
  result := fEngine.AnsiBufferToRawUtf8(pointer(tmp), length(tmp));
end;

procedure TOrmPropInfoRttiAnsi.NormalizeValue(var Value: RawUtf8);
begin
  // do nothing: should already be UTF-8 encoded
end;

function TOrmPropInfoRttiAnsi.CompareValue(Item1, Item2: TObject;
  CaseInsensitive: boolean): integer;
var
  tmp1, tmp2: RawByteString;
begin
  if Item1 = Item2 then
    result := 0
  else if Item1 = nil then
    result := -1
  else if Item2 = nil then
    result := 1
  else
  begin
    fPropInfo.GetLongStrProp(Item1, tmp1);
    fPropInfo.GetLongStrProp(Item2, tmp2);
    if CaseInsensitive then
      if fEngine.CodePage = CODEPAGE_US then
        result := AnsiIComp(pointer(tmp1), pointer(tmp2))
      else
        result := StrIComp(pointer(tmp1), pointer(tmp2))
    else
      result := StrComp(pointer(tmp1), pointer(tmp2));
  end;
end;

procedure TOrmPropInfoRttiAnsi.SetBinary(Instance: TObject; var Read: TFastReader);
begin
  fPropInfo.SetLongStrProp(Instance, Read.VarString(fEngine.CodePage));
end;

procedure TOrmPropInfoRttiAnsi.SetValue(Instance: TObject; Value: PUtf8Char;
  ValueLen: PtrInt; wasString: boolean);
begin
  if Value = nil then
    fPropInfo.SetLongStrProp(Instance, '')
  else
    fPropInfo.SetLongStrProp(Instance, fEngine.Utf8BufferToAnsi(Value, ValueLen));
end;

procedure TOrmPropInfoRttiAnsi.SetValueVar(Instance: TObject; const Value:
  RawUtf8; wasString: boolean);
begin
  fPropInfo.SetLongStrProp(Instance, fEngine.Utf8ToAnsi(Value));
end;

procedure TOrmPropInfoRttiAnsi.GetJsonValues(Instance: TObject; W: TTextWriter);
var
  tmp: RawByteString;
begin
  W.Add('"');
  fPropInfo.GetLongStrProp(Instance, tmp);
  if tmp <> '' then
    W.AddAnyAnsiString(tmp, twJsonEscape, fEngine.CodePage);
  W.Add('"');
end;

function TOrmPropInfoRttiAnsi.SetFieldSqlVar(Instance: TObject;
  const aValue: TSqlVar): boolean;
var
  tmp: RawByteString;
begin
  case aValue.VType of
    ftNull:
      ; // leave tmp=''
    ftUtf8:
      fEngine.Utf8BufferToAnsi(aValue.VText, StrLen(aValue.VText), tmp);
  else
    begin
      result := inherited SetFieldSqlVar(Instance, aValue);
      exit;
    end;
  end;
  fPropInfo.SetLongStrProp(Instance, tmp{%H-});
  result := True;
end;

procedure TOrmPropInfoRttiAnsi.GetFieldSqlVar(Instance: TObject;
  var aValue: TSqlVar; var temp: RawByteString);
begin
  fPropInfo.GetLongStrProp(Instance, temp);
  temp := fEngine.AnsiToUtf8(temp);
  aValue.Options := [];
  aValue.VType := ftUtf8;
  aValue.VText := pointer(temp);
end;

procedure TOrmPropInfoRttiAnsi.CopyValue(Source, Dest: TObject);
begin
  // avoid temporary variable use, for simple fields with no getter/setter
  if fInPlaceCopySameClassPropOffset = 0 then
    fPropInfo.CopyLongStrProp(Source, Dest)
  else
    PRawByteString(PtrUInt(Dest) + fInPlaceCopySameClassPropOffset)^ :=
      PRawByteString(PtrUInt(Source) + fInPlaceCopySameClassPropOffset)^;
end;


{ TOrmPropInfoRttiRawUtf8 }

procedure TOrmPropInfoRttiRawUtf8.CopySameClassProp(Source: TObject;
  DestInfo: TOrmPropInfo; Dest: TObject);
var
  Value: RawByteString;
begin
  // don't know why, but fInPlaceCopySameClassPropOffset trick leaks memory :(
  fPropInfo.GetLongStrProp(Source, Value);
  TOrmPropInfoRttiRawUtf8(DestInfo).fPropInfo.SetLongStrProp(Dest, Value);
end;

function TOrmPropInfoRttiRawUtf8.GetHash(Instance: TObject;
  CaseInsensitive: boolean): cardinal;
var
  Up: array[byte] of AnsiChar; // avoid slow heap allocation
  Value: RawByteString;
begin
  fPropInfo.GetLongStrProp(Instance, Value);
  if CaseInsensitive then
    result := DefaultHasher(0, Up{%H-}, Utf8UpperCopy255(Up{%H-}, Value) - {%H-}Up)
  else
    result := DefaultHasher(0, pointer(Value), length(Value));
end;

procedure TOrmPropInfoRttiRawUtf8.GetJsonValues(Instance: TObject; W: TTextWriter);
var
  tmp: RawByteString;
begin
  fPropInfo.GetLongStrProp(Instance, tmp);
  if fPropType = TypeInfo(RawJson) then
    W.AddRawJson(tmp)
  else
  begin
    W.Add('"');
    if tmp <> '' then
      W.AddJsonEscape(pointer(tmp));
    W.Add('"');
  end;
end;

procedure TOrmPropInfoRttiRawUtf8.GetValueVar(Instance: TObject; ToSql: boolean;
  var result: RawUtf8; wasSqlString: PBoolean);
begin
  if wasSqlString <> nil then
    wasSqlString^ := fPropType <> TypeInfo(RawJson);
  fPropInfo.GetLongStrProp(Instance, RawByteString(result));
end;

procedure TOrmPropInfoRttiRawUtf8.SetBinary(Instance: TObject; var Read: TFastReader);
begin
  fPropInfo.SetLongStrProp(Instance, Read.VarUtf8);
end;

function TOrmPropInfoRttiRawUtf8.SetFieldSqlVar(Instance: TObject;
  const aValue: TSqlVar): boolean;
var
  tmp: RawByteString;
begin
  case aValue.VType of
    ftNull:
      ; // leave tmp=''
    ftUtf8:
      SetString(tmp, PAnsiChar(aValue.VText), StrLen(aValue.VText));
  else
    begin
      result := inherited SetFieldSqlVar(Instance, aValue);
      exit;
    end;
  end;
  fPropInfo.SetLongStrProp(Instance, tmp{%H-});
  result := True;
end;

procedure TOrmPropInfoRttiRawUtf8.GetFieldSqlVar(Instance: TObject;
  var aValue: TSqlVar; var temp: RawByteString);
begin
  fPropInfo.GetLongStrProp(Instance, temp);
  aValue.Options := [];
  aValue.VType := ftUtf8;
  aValue.VText := Pointer(temp);
end;

function TOrmPropInfoRttiRawUtf8.CompareValue(Item1, Item2: TObject;
  CaseInsensitive: boolean): integer;

  function CompareUTF8WithLocalTempCopy: PtrInt;
  var
    tmp1, tmp2: RawByteString;
  begin
    fPropInfo.GetLongStrProp(Item1, tmp1);
    fPropInfo.GetLongStrProp(Item2, tmp2);
    if aUnicodeNoCaseCollation in Attributes then
      result := Utf8ICompReference(pointer(tmp1), pointer(tmp2))
    else if CaseInsensitive then
      result := Utf8IComp(pointer(tmp1), pointer(tmp2))
    else
      result := StrComp(pointer(tmp1), pointer(tmp2));
  end;

var
  offs: PtrUInt;
  p1, p2: pointer;
begin
  if Item1 = Item2 then
    result := 0
  else if Item1 = nil then
    result := -1
  else if Item2 = nil then
    result := 1
  else
  begin
    offs := fGetterIsFieldPropOffset;
    if offs <> 0 then
    begin
      // avoid any temporary variable
      p1 := PPointer(PtrUInt(Item1) + offs)^;
      p2 := PPointer(PtrUInt(Item2) + offs)^;
      if CaseInsensitive then
        if aUnicodeNoCaseCollation in Attributes then
          result := Utf8ICompReference(p1, p2)
        else
          result := Utf8IComp(p1, p2)
      else
        result := StrComp(p1, p2);
    end
    else
      result := CompareUTF8WithLocalTempCopy;
  end;
end;

procedure TOrmPropInfoRttiRawUtf8.SetValue(Instance: TObject; Value: PUtf8Char;
  ValueLen: PtrInt; wasString: boolean);
var
  tmp: pointer;
begin
  if fSetterIsFieldPropOffset <> 0 then
    FastSetString(PRawUtf8(PtrUInt(Instance) + fSetterIsFieldPropOffset)^, Value, ValueLen)
  else
  begin
    tmp := nil; // manual initialization/finalization with no hidden try/finally
    if Value <> nil then
      FastSetString(RawUtf8(tmp), Value, ValueLen);
    fPropInfo.SetLongStrProp(Instance, RawUtf8(tmp));
    FastAssignNew(tmp);
  end;
end;

procedure TOrmPropInfoRttiRawUtf8.SetValueVar(Instance: TObject;
  const Value: RawUtf8; wasString: boolean);
begin
  fPropInfo.SetLongStrProp(Instance, Value);
end;


{ TOrmPropInfoRttiRawUnicode }

procedure TOrmPropInfoRttiRawUnicode.CopySameClassProp(Source: TObject;
  DestInfo: TOrmPropInfo; Dest: TObject);
var
  Value: RawByteString;
begin
  fPropInfo.GetLongStrProp(Source, Value);
  TOrmPropInfoRttiRawUnicode(DestInfo).fPropInfo.SetLongStrProp(Dest, Value);
end;

function TOrmPropInfoRttiRawUnicode.GetHash(Instance: TObject;
  CaseInsensitive: boolean): cardinal;
var
  Up: array[byte] of AnsiChar; // avoid slow heap allocation
  Value: RawByteString;
begin
  fPropInfo.GetLongStrProp(Instance, Value);
  if CaseInsensitive then
    result := DefaultHasher(0, Up{%H-},
      UpperCopy255W(Up{%H-}, pointer(Value), length(Value) shr 1) - {%H-}Up)
  else
    result := DefaultHasher(0, pointer(Value), length(Value));
end;

procedure TOrmPropInfoRttiRawUnicode.GetValueVar(Instance: TObject;
  ToSql: boolean; var result: RawUtf8; wasSqlString: PBoolean);
var
  tmp: RawByteString;
begin
  if wasSqlString <> nil then
    wasSqlString^ := true;
  fPropInfo.GetLongStrProp(Instance, tmp);
  RawUnicodeToUtf8(pointer(tmp), length(tmp) shr 1, result);
end;

function TOrmPropInfoRttiRawUnicode.CompareValue(Item1, Item2: TObject;
  CaseInsensitive: boolean): integer;
var
  tmp1, tmp2: RawByteString;
begin
  if Item1 = Item2 then
    result := 0
  else if Item1 = nil then
    result := -1
  else if Item2 = nil then
    result := 1
  else
  begin
    fPropInfo.GetLongStrProp(Item1, tmp1);
    fPropInfo.GetLongStrProp(Item2, tmp2);
    if CaseInsensitive then
      result := AnsiICompW(pointer(tmp1), pointer(tmp2))
    else
      result := StrCompW(pointer(tmp1), pointer(tmp2));
  end;
end;

procedure TOrmPropInfoRttiRawUnicode.SetValue(Instance: TObject;
  Value: PUtf8Char; ValueLen: PtrInt; wasString: boolean);
begin
  if Value = nil then
    fPropInfo.SetLongStrProp(Instance, '')
  else
    fPropInfo.SetLongStrProp(Instance, Utf8DecodeToRawUnicode(Value, ValueLen));
end;

procedure TOrmPropInfoRttiRawUnicode.SetValueVar(Instance: TObject;
  const Value: RawUtf8; wasString: boolean);
begin
  fPropInfo.SetLongStrProp(Instance, Utf8DecodeToRawUnicode(Value));
end;


{ TOrmPropInfoRttiRawBlob }

procedure TOrmPropInfoRttiRawBlob.CopySameClassProp(Source: TObject;
  DestInfo: TOrmPropInfo; Dest: TObject);
var
  Value: RawByteString;
begin
  fPropInfo.GetLongStrProp(Source, Value);
  TOrmPropInfoRttiRawBlob(DestInfo).fPropInfo.SetLongStrProp(Dest, Value);
end;

function TOrmPropInfoRttiRawBlob.GetHash(Instance: TObject;
  CaseInsensitive: boolean): cardinal;
var
  Value: RawByteString;
begin
  fPropInfo.GetLongStrProp(Instance, Value);
  // binary -> case sensitive hash
  result := DefaultHasher(0, pointer(Value), length(Value));
end;

procedure TOrmPropInfoRttiRawBlob.GetJsonValues(Instance: TObject; W: TTextWriter);
var
  tmp: RawByteString;
begin
  fPropInfo.GetLongStrProp(Instance, tmp);
  W.WrBase64(pointer(tmp), length(tmp), true);
end;

procedure TOrmPropInfoRttiRawBlob.GetBlob(Instance: TObject; var Blob: RawByteString);
begin
  fPropInfo.GetLongStrProp(Instance, Blob);
end;

procedure TOrmPropInfoRttiRawBlob.SetBlob(Instance: TObject; const Blob: RawByteString);
begin
  fPropInfo.SetLongStrProp(Instance, Blob);
end;

function TOrmPropInfoRttiRawBlob.IsNull(Instance: TObject): boolean;
var
  Blob: RawByteString;
begin
  fPropInfo.GetLongStrProp(Instance, Blob);
  result := (Blob = '');
end;

procedure TOrmPropInfoRttiRawBlob.GetValueVar(Instance: TObject; ToSql: boolean;
  var result: RawUtf8; wasSqlString: PBoolean);
begin
  fPropInfo.GetLongStrProp(Instance, RawByteString(result));
  BinaryToText(result, ToSql, wasSqlString);
end;

function TOrmPropInfoRttiRawBlob.CompareValue(Item1, Item2: TObject;
  CaseInsensitive: boolean): integer;
var
  tmp1, tmp2: RawByteString;
begin
  if Item1 = Item2 then
    result := 0
  else if Item1 = nil then
    result := -1
  else if Item2 = nil then
    result := 1
  else
  begin
    fPropInfo.GetLongStrProp(Item1, tmp1);
    fPropInfo.GetLongStrProp(Item2, tmp2);
    // BLOB is binary so always case sensitive
    result := SortDynArrayRawByteString(tmp1, tmp2); // end with length() not #0
  end;
end;

procedure TOrmPropInfoRttiRawBlob.SetValue(Instance: TObject; Value: PUtf8Char;
  ValueLen: PtrInt; wasString: boolean);
begin
  fPropInfo.SetLongStrProp(Instance, BlobToRawBlob(Value, ValueLen));
end;

procedure TOrmPropInfoRttiRawBlob.SetValueVar(Instance: TObject;
  const Value: RawUtf8; wasString: boolean);
begin
  fPropInfo.SetLongStrProp(Instance, BlobToRawBlob(Value));
end;

function TOrmPropInfoRttiRawBlob.SetFieldSqlVar(Instance: TObject;
  const aValue: TSqlVar): boolean;
var
  tmp: RawByteString;
begin
  case aValue.VType of
    ftBlob:
      begin
        SetString(tmp, PAnsiChar(aValue.VBlob), aValue.VBlobLen);
        fPropInfo.SetLongStrProp(Instance, tmp);
        result := true;
      end;
    ftNull:
      begin
        fPropInfo.SetLongStrProp(Instance, '');
        result := true;
      end;
  else
    result := inherited SetFieldSqlVar(Instance, aValue);
  end;
end;

procedure TOrmPropInfoRttiRawBlob.GetFieldSqlVar(Instance: TObject;
  var aValue: TSqlVar; var temp: RawByteString);
begin
  fPropInfo.GetLongStrProp(Instance, temp);
  aValue.Options := [];
  if temp = '' then
    aValue.VType := ftNull
  else
  begin
    aValue.VType := ftBlob;
    aValue.VBlob := pointer(temp);
    aValue.VBlobLen := length(temp);
  end;
end;


{ TOrmPropInfoRttiWide }

procedure TOrmPropInfoRttiWide.CopySameClassProp(Source: TObject;
  DestInfo: TOrmPropInfo; Dest: TObject);
var
  Value: WideString;
begin
  fPropInfo.GetWideStrProp(Source, Value);
  TOrmPropInfoRttiWide(DestInfo).fPropInfo.SetWideStrProp(Dest, Value);
end;

procedure TOrmPropInfoRttiWide.GetBinary(Instance: TObject; W: TBufferWriter);
var
  Value: WideString;
begin
  fPropInfo.GetWideStrProp(Instance, Value);
  W.Write(WideStringToUtf8(Value));
end;

function TOrmPropInfoRttiWide.GetHash(Instance: TObject;
  CaseInsensitive: boolean): cardinal;
var
  Up: array[byte] of AnsiChar; // avoid slow heap allocation
  Value: WideString;
begin
  fPropInfo.GetWideStrProp(Instance, Value);
  if CaseInsensitive then
    result := DefaultHasher(0, Up{%H-},
      UpperCopy255W(Up{%H-}, pointer(Value), length(Value)) - {%H-}Up)
  else
    result := DefaultHasher(0, pointer(Value), length(Value) * 2);
end;

procedure TOrmPropInfoRttiWide.GetJsonValues(Instance: TObject; W: TTextWriter);
var
  Value: WideString;
begin
  W.Add('"');
  fPropInfo.GetWideStrProp(Instance, Value);
  if pointer(Value) <> nil then
    W.AddJsonEscapeW(pointer(Value), 0);
  W.Add('"');
end;

procedure TOrmPropInfoRttiWide.GetValueVar(Instance: TObject; ToSql: boolean;
  var result: RawUtf8; wasSqlString: PBoolean);
var
  Value: WideString;
begin
  fPropInfo.GetWideStrProp(Instance, Value);
  result := WideStringToUtf8(Value);
  if wasSqlString <> nil then
    wasSqlString^ := true;
end;

procedure TOrmPropInfoRttiWide.CopyValue(Source, Dest: TObject);
begin
  // avoid temporary variable use, for simple fields with no getter/setter
  if fInPlaceCopySameClassPropOffset = 0 then
    CopySameClassProp(Source, self, Dest)
  else
    PWideString(PtrUInt(Dest) + fInPlaceCopySameClassPropOffset)^ :=
      PWideString(PtrUInt(Source) + fInPlaceCopySameClassPropOffset)^;
end;

function TOrmPropInfoRttiWide.CompareValue(Item1, Item2: TObject;
  CaseInsensitive: boolean): integer;
var
  tmp1, tmp2: WideString;
begin
  if Item1 = Item2 then
    result := 0
  else if Item1 = nil then
    result := -1
  else if Item2 = nil then
    result := 1
  else
  begin
    fPropInfo.GetWideStrProp(Item1, tmp1);
    fPropInfo.GetWideStrProp(Item2, tmp2);
    if CaseInsensitive then
      result := AnsiICompW(pointer(tmp1), pointer(tmp2))
    else
      result := StrCompW(pointer(tmp1), pointer(tmp2));
  end;
end;

procedure TOrmPropInfoRttiWide.SetBinary(Instance: TObject; var Read: TFastReader);
begin
  fPropInfo.SetWideStrProp(Instance, Utf8ToWideString(Read.VarUtf8));
end;

procedure TOrmPropInfoRttiWide.SetValue(Instance: TObject; Value: PUtf8Char;
  ValueLen: PtrInt; wasString: boolean);
var
  Wide: WideString;
begin
  if Value <> nil then
    Utf8ToWideString(Value, ValueLen, Wide);
  fPropInfo.SetWideStrProp(Instance, Wide);
end;


{$ifdef HASVARUSTRING}

{ TOrmPropInfoRttiUnicode }

procedure TOrmPropInfoRttiUnicode.CopySameClassProp(Source: TObject;
  DestInfo: TOrmPropInfo; Dest: TObject);
var
  tmp: UnicodeString;
begin
  fPropInfo.GetUnicodeStrProp(Source, tmp);
  TOrmPropInfoRttiUnicode(DestInfo).fPropInfo.SetUnicodeStrProp(Dest, tmp);
end;

procedure TOrmPropInfoRttiUnicode.GetBinary(Instance: TObject; W: TBufferWriter);
var
  tmp: UnicodeString;
begin
  fPropInfo.GetUnicodeStrProp(Instance, tmp);
  W.Write(UnicodeStringToUtf8(tmp));
end;

procedure TOrmPropInfoRttiUnicode.CopyValue(Source, Dest: TObject);
begin
  // avoid temporary variable use, for simple fields with no getter/setter
  if fInPlaceCopySameClassPropOffset = 0 then
    CopySameClassProp(Source, self, Dest)
  else
    PUnicodeString(PtrUInt(Dest) + fInPlaceCopySameClassPropOffset)^ :=
      PUnicodeString(PtrUInt(Source) + fInPlaceCopySameClassPropOffset)^;
end;

function TOrmPropInfoRttiUnicode.GetHash(Instance: TObject;
  CaseInsensitive: boolean): cardinal;
var
  Up: array[byte] of AnsiChar; // avoid heap allocation - 255 chars is enough
  Value: UnicodeString;
begin
  fPropInfo.GetUnicodeStrProp(Instance, Value);
  if CaseInsensitive then
    result := DefaultHasher(0, Up{%H-},
      UpperCopy255W(Up{%H-}, pointer(Value), length(Value)) - {%H-}Up)
  else
    result := DefaultHasher(0, pointer(Value), length(Value) * 2);
end;

procedure TOrmPropInfoRttiUnicode.GetValueVar(Instance: TObject; ToSql: boolean;
  var result: RawUtf8; wasSqlString: PBoolean);
var
  tmp: UnicodeString;
begin
  fPropInfo.GetUnicodeStrProp(Instance, tmp);
  RawUnicodeToUtf8(pointer(tmp), length(tmp), result);
  if wasSqlString <> nil then
    wasSqlString^ := true;
end;

procedure TOrmPropInfoRttiUnicode.GetJsonValues(Instance: TObject; W: TTextWriter);
var
  tmp: UnicodeString;
begin
  W.Add('"');
  fPropInfo.GetUnicodeStrProp(Instance, tmp);
  if tmp <> '' then
    W.AddJsonEscapeW(pointer(tmp), 0);
  W.Add('"');
end;

function TOrmPropInfoRttiUnicode.CompareValue(Item1, Item2: TObject;
  CaseInsensitive: boolean): integer;
var
  tmp1, tmp2: UnicodeString;
begin
  if Item1 = Item2 then
    result := 0
  else if Item1 = nil then
    result := -1
  else if Item2 = nil then
    result := 1
  else
  begin
    fPropInfo.GetUnicodeStrProp(Item1, tmp1);
    fPropInfo.GetUnicodeStrProp(Item2, tmp2);
    if CaseInsensitive then
      result := AnsiICompW(pointer(tmp1), pointer(tmp2))
    else
      result := StrCompW(pointer(tmp1), pointer(tmp2));
  end;
end;

procedure TOrmPropInfoRttiUnicode.SetBinary(Instance: TObject; var Read: TFastReader);
begin
  fPropInfo.SetUnicodeStrProp(Instance, Utf8DecodeToUnicodeString(Read.VarUtf8));
end;

procedure TOrmPropInfoRttiUnicode.SetValue(Instance: TObject; Value: PUtf8Char;
  ValueLen: PtrInt; wasString: boolean);
var
  tmp: UnicodeString;
begin
  if Value <> nil then
    Utf8DecodeToUnicodeString(Value, ValueLen, tmp);
  fPropInfo.SetUnicodeStrProp(Instance, tmp);
end;

procedure TOrmPropInfoRttiUnicode.SetValueVar(Instance: TObject;
  const Value: RawUtf8; wasString: boolean);
begin
  fPropInfo.SetUnicodeStrProp(Instance, Utf8DecodeToUnicodeString(Value));
end;

function TOrmPropInfoRttiUnicode.SetFieldSqlVar(Instance: TObject;
  const aValue: TSqlVar): boolean;
var
  tmp: UnicodeString;
begin
  case aValue.VType of
    ftNull:
      ; // leave tmp=''
    ftUtf8:
      Utf8DecodeToUnicodeString(aValue.VText, StrLen(aValue.VText), tmp);
  else
    begin
      result := inherited SetFieldSqlVar(Instance, aValue);
      exit;
    end;
  end;
  fPropInfo.SetUnicodeStrProp(Instance, tmp{%H-});
  result := True;
end;

procedure TOrmPropInfoRttiUnicode.GetFieldSqlVar(Instance: TObject;
  var aValue: TSqlVar; var temp: RawByteString);
var
  tmp: UnicodeString;
begin
  fPropInfo.GetUnicodeStrProp(Instance, tmp);
  RawUnicodeToUtf8(pointer(tmp), length(tmp), RawUtf8(temp));
  aValue.Options := [];
  aValue.VType := ftUtf8;
  aValue.VText := Pointer(temp);
end;

{$endif HASVARUSTRING}


{ TOrmPropInfoRttiDynArray }

constructor TOrmPropInfoRttiDynArray.Create(aPropInfo: PRttiProp;
  aPropIndex: integer; aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions);
begin
  inherited Create(aPropInfo, aPropIndex, aOrmFieldType, aOptions);
  if rcfObjArray in fPropRtti.Flags then
  begin
    fObjArray := fPropRtti.ArrayRtti as TRttiJson;
    fSqlDBFieldType := ftUtf8; // matches GetFieldSqlVar() below
  end;
  if fGetterIsFieldPropOffset = 0 then
    raise EOrmException.CreateUtf8('%.Create(%) should be a field, not a getter!',
      [self, fPropType^.Name]);
end;

procedure TOrmPropInfoRttiDynArray.GetDynArray(Instance: TObject; var result: TDynArray);
begin
  // very fast assignment of pre-initialized RTTI
  result.InitRtti(fPropRtti, pointer(PtrUInt(Instance) + fGetterIsFieldPropOffset)^);
end;

function TOrmPropInfoRttiDynArray.GetDynArrayElemType: TRttiCustom;
begin
  result := fPropRtti.ArrayRtti;
end;

procedure TOrmPropInfoRttiDynArray.Serialize(Instance: TObject;
  var data: RawByteString; ExtendedJson: boolean);
var
  da: TDynArray;
  temp: TTextWriterStackBuffer;
begin
  GetDynArray(Instance, da);
  if da.Count = 0 then
    data := ''
  else if fObjArray <> nil then
    // T*ObjArray use JSON serialization
    with TJsonSerializer.CreateOwnedStream(temp) do
    try
      if ExtendedJson then // smaller content
        CustomOptions := CustomOptions + [twoForceJsonExtended];
      AddDynArrayJson(da);
      SetText(RawUtf8(data));
    finally
      Free;
    end
  else
    // regular (e.g. record) dynamic arrays use our binary encoding
    data := da.SaveTo;
end;

procedure TOrmPropInfoRttiDynArray.CopySameClassProp(Source: TObject;
  DestInfo: TOrmPropInfo; Dest: TObject);
var
  sda, dda: TDynArray;
begin
  GetDynArray(Source, sda);
  TOrmPropInfoRttiDynArray(DestInfo).GetDynArray(Dest, dda);
  if (fObjArray <> nil) or
     (TOrmPropInfoRttiDynArray(DestInfo).fObjArray <> nil) or
     (sda.Info.ArrayRtti <> dda.Info.ArrayRtti) then
    dda.LoadFromJson(pointer(sda.SaveToJson))
  else
    dda.Copy(@sda);
end;

procedure TOrmPropInfoRttiDynArray.GetBinary(Instance: TObject; W: TBufferWriter);
var
  Value: RawByteString;
begin
  Serialize(Instance, Value, true); // JSON for T*ObjArray, or our binary format
  if fObjArray <> nil then
    W.Write(Value)
  else
    W.WriteBinary(Value);
end;

function TOrmPropInfoRttiDynArray.GetHash(Instance: TObject;
  CaseInsensitive: boolean): cardinal;
var
  tmp: RawByteString;
begin
  Serialize(Instance, tmp, true);
  result := DefaultHasher(0, pointer(tmp), length(tmp));
end;

procedure TOrmPropInfoRttiDynArray.GetValueVar(Instance: TObject; ToSql: boolean;
  var result: RawUtf8; wasSqlString: PBoolean);
begin
  Serialize(Instance, RawByteString(result), false);
  if fObjArray = nil then
    BinaryToText(result, ToSql, wasSqlString);
end;

procedure TOrmPropInfoRttiDynArray.GetVariant(Instance: TObject; var Dest: Variant);
var
  json: RawUtf8;
  da: TDynArray;
begin
  GetDynArray(Instance, da);
  json := da.SaveToJson;
  VarClear(Dest);
  TDocVariantData(Dest).InitJsonInPlace(pointer(json), JSON_FAST);
end;

procedure TOrmPropInfoRttiDynArray.SetVariant(Instance: TObject; const Source: Variant);
var
  json: RawUtf8;
  da: TDynArray;
begin
  GetDynArray(Instance, da);
  VariantSaveJson(Source, twJsonEscape, json);
  da.LoadFromJson(pointer(json));
end;

procedure TOrmPropInfoRttiDynArray.NormalizeValue(var Value: RawUtf8);
begin
  // do nothing: should already be normalized
end;

function TOrmPropInfoRttiDynArray.CompareValue(Item1, Item2: TObject;
  CaseInsensitive: boolean): integer;
var
  da1, da2: TDynArray;
  n: PtrInt;
begin
  if Item1 = Item2 then
    result := 0
  else if Item1 = nil then
    result := -1
  else if Item2 = nil then
    result := 1
  else
  begin
    GetDynArray(Item1, da1);
    GetDynArray(Item2, da2);
    n := da1.Count;
    result := n - da2.Count;
    if result = 0 then
      if fObjArray <> nil then
        result := ObjectCompare(da1.Value^, da2.Value^, n, CaseInsensitive)
      else
        // compare using our fast RTTI functions
        result := da1.Compares(@da2, {ignorecomp:}false, not CaseInsensitive);
  end;
end;

procedure TOrmPropInfoRttiDynArray.SetBinary(Instance: TObject; var Read: TFastReader);
var
  tmp: TSynTempBuffer; // LoadFromJson() may change the input buffer
  da: TDynArray;
begin
  GetDynArray(Instance, da);
  if fObjArray <> nil then
  begin
    // T*ObjArray use JSON serialization
    Read.VarBlob(tmp);
    try
      da.LoadFromJson(tmp.buf);
    finally
      tmp.Done;
    end;
  end
  else
    // regular (e.g. record) dynamic arrays use our binary encoding
    da.LoadFromReader(Read);
end;

procedure TOrmPropInfoRttiDynArray.SetValue(Instance: TObject; Value: PUtf8Char;
  ValueLen: PtrInt; wasString: boolean);
var
  tmp: TSynTempBuffer;
  da: TDynArray;
begin
  GetDynArray(Instance, da);
  if Value = nil then
    da.Clear
  else
  try
    if (fObjArray = nil) and
       Base64MagicCheckAndDecode(Value, tmp, ValueLen) then
      da.LoadFrom(tmp.buf, PAnsiChar(tmp.buf) + tmp.len)
    else
      da.LoadFromJson(tmp.Init(Value));
  finally
    tmp.Done;
  end;
end;

function TOrmPropInfoRttiDynArray.SetFieldSqlVar(Instance: TObject;
  const aValue: TSqlVar): boolean;
var
  da: TDynArray;
begin
  if aValue.VType = ftBlob then
  begin
    GetDynArray(Instance, da);
    result := da.LoadFrom(aValue.VBlob, PAnsiChar(aValue.VBlob) + aValue.VBlobLen) <> nil;
  end
  else
    result := inherited SetFieldSqlVar(Instance, aValue);
end;

procedure TOrmPropInfoRttiDynArray.GetJsonValues(Instance: TObject; W: TTextWriter);
var
  tmp: RawByteString;
begin
  if W.InheritsFrom(TJsonSerializer) and
     (jwoAsJsonNotAsString in TJsonSerializer(W).OrmOptions) then
    W.AddDynArrayJson(GetFieldAddr(Instance), fPropRtti)
  else if fObjArray <> nil then
    W.AddDynArrayJsonAsString(fPropType, GetFieldAddr(Instance)^)
  else
  begin
    Serialize(Instance, tmp, false);
    W.WrBase64(pointer(tmp), Length(tmp), true); // withMagic=true -> add ""
  end;
end;

procedure TOrmPropInfoRttiDynArray.GetFieldSqlVar(Instance: TObject;
  var aValue: TSqlVar; var temp: RawByteString);
begin
  Serialize(Instance, temp, false);
  aValue.Options := [];
  if temp = '' then
    aValue.VType := ftNull
  else if fObjArray <> nil then
  begin
    aValue.VType := ftUtf8; // JSON
    aValue.VText := pointer(temp);
  end
  else
  begin
    aValue.VType := ftBlob; // binary
    aValue.VBlob := pointer(temp);
    aValue.VBlobLen := length(temp);
  end;
end;


{ TOrmPropInfoRttiVariant }

constructor TOrmPropInfoRttiVariant.Create(aPropInfo: PRttiProp;
  aPropIndex: integer; aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions);
begin
  inherited;
  if aOrmFieldType = oftVariant then
    fDocVariantOptions := JSON_FAST
  else
    fOrmFieldType := oftNullable; // TNullable* will use fOrmFieldTypeStored
end;

procedure TOrmPropInfoRttiVariant.CopySameClassProp(Source: TObject;
  DestInfo: TOrmPropInfo; Dest: TObject);
var
  value: Variant;
begin
  fPropInfo.GetVariantProp(Source, value, {byref=}true);
  TOrmPropInfoRttiVariant(DestInfo).fPropInfo.SetVariantProp(Dest, value);
end;

procedure TOrmPropInfoRttiVariant.GetBinary(Instance: TObject; W: TBufferWriter);
var
  value: Variant;
begin
  fPropInfo.GetVariantProp(Instance, value, {byref=}true);
  BinarySave(@value, fPropType, W);
end;

function TOrmPropInfoRttiVariant.GetHash(Instance: TObject; CaseInsensitive:
  boolean): cardinal;
var
  value: Variant;
begin
  fPropInfo.GetVariantProp(Instance, value, {byref=}true);
  result := VariantHash(value, CaseInsensitive);
end;

procedure TOrmPropInfoRttiVariant.GetJsonValues(Instance: TObject; W: TTextWriter);
var
  value: Variant;
  backup: TTextWriterOptions;
begin
  fPropInfo.GetVariantProp(Instance, value, {byref=}true);
  backup := W.CustomOptions;
  if W.InheritsFrom(TJsonSerializer) and
     (jwoAsJsonNotAsString in TJsonSerializer(W).OrmOptions) then
    W.CustomOptions := backup + [twoForceJsonStandard] - [twoForceJsonExtended];
  W.AddVariant(value, twJsonEscape); // even oftNullable should escape strings
  W.CustomOptions := backup;
end;

procedure TOrmPropInfoRttiVariant.GetValueVar(Instance: TObject; ToSql: boolean;
  var result: RawUtf8; wasSqlString: PBoolean);
var
  wasString: boolean;
  value: Variant;
begin
  fPropInfo.GetVariantProp(Instance, value, {byref=}true);
  VariantToUtf8(value, result, wasString);
  if wasSqlString <> nil then
    if fOrmFieldType = oftNullable then
      // only TNullableUtf8Text and TNullableDateTime will be actual text
      wasSqlString^ := (fSqlDBFieldType in TEXT_DBFIELDS) and not VarIsEmptyOrNull(value)
    else
      // from SQL point of view, variant columns are TEXT or NULL
      wasSqlString^ := not VarIsEmptyOrNull(value);
end;

procedure TOrmPropInfoRttiVariant.GetVariant(Instance: TObject; var Dest: Variant);
begin
  fPropInfo.GetVariantProp(Instance, Dest, {byref=}true);
end;

procedure TOrmPropInfoRttiVariant.NormalizeValue(var Value: RawUtf8);
begin
  // content should be already normalized
end;

function TOrmPropInfoRttiVariant.CompareValue(Item1, Item2: TObject;
  CaseInsensitive: boolean): integer;

  function CompareWithLocalTempCopy: PtrInt;
  var
    V1, V2: variant;
  begin
    fPropInfo.GetVariantProp(Item1, V1, {byref=}true);
    fPropInfo.GetVariantProp(Item2, V2, {byref=}true);
    result := FastVarDataComp(@V1, @V2, CaseInsensitive);
  end;

begin
  if Item1 = Item2 then
    result := 0
  else if Item1 = nil then
    result := -1
  else if Item2 = nil then
    result := 1
  else if fGetterIsFieldPropOffset <> 0 then // avoid any temporary variable
    result := FastVarDataComp(PVarData(PtrUInt(Item1) + fGetterIsFieldPropOffset),
            PVarData(PtrUInt(Item2) + fGetterIsFieldPropOffset), CaseInsensitive)
  else
    result := CompareWithLocalTempCopy;
end;

procedure TOrmPropInfoRttiVariant.SetBinary(Instance: TObject; var Read: TFastReader);
var
  value: Variant;
  bak: PDocVariantOptions;
begin
  bak := Read.CustomVariants;
  if fOrmFieldType = oftNullable then
    Read.CustomVariants := nil
  else
    Read.CustomVariants := @DocVariantOptions;
  RTTI_BINARYLOAD[rkVariant](@value, Read, fPropType);
  Read.CustomVariants := bak;
  fPropInfo.SetVariantProp(Instance, value);
end;

procedure TOrmPropInfoRttiVariant.SetValue(Instance: TObject; Value: PUtf8Char;
  ValueLen: PtrInt; wasString: boolean);
begin
  SetValuePtr(Instance, Value, ValueLen, wasString);
end;

procedure TOrmPropInfoRttiVariant.SetValueVar(Instance: TObject;
  const Value: RawUtf8; wasString: boolean);
begin
  SetValuePtr(Instance, pointer(Value), length(Value), wasString);
end;

procedure TOrmPropInfoRttiVariant.SetValuePtr(Instance: TObject;
  Value: PUtf8Char; ValueLen: integer; wasString: boolean);
var
  tmp: TSynTempBuffer;
  V: Variant;
begin
  if ValueLen > 0 then
  begin
    tmp.Init(Value, ValueLen);
    try
      if fOrmFieldType = oftNullable then
        if fSqlDBFieldType = ftDate then
        begin
          // decode as date/time variant
          TVarData(V).VType := varDate;
          TVarData(V).VDate := Iso8601ToDateTimePUtf8Char(Value, ValueLen);
        end
        else
          GetVariantFromJsonField(tmp.buf, wasString, V, nil, false, ValueLen)
      else
      begin
        if wasString and
           (GotoNextNotSpace(Value)^ in ['{', '[']) then
          wasString := false; // allow to create a TDocVariant stored as DB text
        GetVariantFromJsonField(tmp.buf, wasString, V, @DocVariantOptions,false, ValueLen);
      end;
    finally
      tmp.Done;
    end;
  end
  else
    TVarData(V).VType := varNull; // TEXT or NULL: see GetValueVar()
  if fSetterIsFieldPropOffset <> 0 then
    PVariant(PtrUInt(Instance) + fSetterIsFieldPropOffset)^ := V
  else
    fPropInfo.SetVariantProp(Instance, V);
end;

procedure TOrmPropInfoRttiVariant.SetVariant(Instance: TObject; const Source: Variant);
begin
  fPropInfo.SetVariantProp(Instance, Source);
end;


{ TOrmPropInfoCustom }

function TOrmPropInfoCustom.GetFieldAddr(Instance: TObject): pointer;
begin
  if Instance = nil then
    result := nil
  else
    result := PAnsiChar(Instance) + fOffset;
end;

constructor TOrmPropInfoCustom.Create(const aName: RawUtf8;
  aOrmFieldType: TOrmFieldType; aAttributes: TOrmPropInfoAttributes;
  aFieldWidth, aPropIndex: integer; aProperty: pointer;
  aData2Text: TOnSqlPropInfoRecord2Text; aText2Data: TOnSqlPropInfoRecord2Data);
begin
  inherited Create(aName, aOrmFieldType, aAttributes, aFieldWidth, aPropIndex);
  fOffset := PtrUInt(aProperty);
  if (Assigned(aData2Text) and not Assigned(aText2Data)) or
     (Assigned(aText2Data) and not Assigned(aData2Text)) then
    raise EOrmException.CreateUtf8(
      'Invalid %.Create: expecting both Data2Text/Text2Data', [self]);
  fData2Text := aData2Text;
  fText2Data := aText2Data;
end;

procedure TOrmPropInfoCustom.TextToBinary(Value: PUtf8Char; var result: RawByteString);
begin
  if Assigned(fText2Data) then
    fText2Data(Value, result)
  else
    result := BlobToRawBlob(Value);
end;

procedure TOrmPropInfoCustom.BinaryToText(var Value: RawUtf8; ToSql: boolean;
  wasSqlString: PBoolean);
begin
  if Assigned(fData2Text) then
    fData2Text(UniqueRawUtf8(Value), length(Value), Value)
  else
    inherited BinaryToText(Value, ToSql, wasSqlString);
end;


{ TOrmPropInfoRecordRtti }

procedure TOrmPropInfoRecordRtti.CopySameClassProp(Source: TObject;
  DestInfo: TOrmPropInfo; Dest: TObject);
begin
  if TOrmPropInfoRecordRtti(DestInfo).fTypeInfo = fTypeInfo then
    RecordCopy(TOrmPropInfoRecordRtti(DestInfo).GetFieldAddr(Dest)^,
      GetFieldAddr(Source)^, fTypeInfo)
  else
    inherited CopySameClassProp(Source, DestInfo, Dest);
end;

function TOrmPropInfoRecordRtti.GetSqlFieldRttiTypeName: RawUtf8;
begin
  if fTypeInfo = nil then
    result := inherited GetSqlFieldRttiTypeName
  else
    result := ToUtf8(fTypeInfo^.RawName);
end;

constructor TOrmPropInfoRecordRtti.Create(aRecordInfo: PRttiInfo;
  const aName: RawUtf8; aPropertyIndex: integer; aPropertyPointer: pointer;
  aAttributes: TOrmPropInfoAttributes; aFieldWidth: integer; aData2Text:
  TOnSqlPropInfoRecord2Text; aText2Data: TOnSqlPropInfoRecord2Data);
begin
  if (aRecordInfo = nil) or
     not (aRecordInfo^.Kind in rkRecordTypes) then
    raise EOrmException.CreateUtf8(
      '%.Create: Invalid type information for %', [self, aName]);
  inherited Create(aName, oftBlobCustom, aAttributes, aFieldWidth,
    aPropertyIndex, aPropertyPointer, aData2Text, aText2Data);
  fTypeInfo := aRecordInfo;
end;

procedure TOrmPropInfoRecordRtti.GetBinary(Instance: TObject; W: TBufferWriter);
var
  Value: RawByteString;
begin
  Value := RecordSave(GetFieldAddr(Instance)^, fTypeInfo);
  W.Write(pointer(Value), length(Value));
end;

function TOrmPropInfoRecordRtti.GetHash(Instance: TObject;
  CaseInsensitive: boolean): cardinal;
var
  tmp: TSynTempBuffer;
begin
  RecordSave(GetFieldAddr(Instance)^, tmp, fTypeInfo);
  result := DefaultHasher(0, tmp.buf, tmp.len);
  tmp.Done;
end;

procedure TOrmPropInfoRecordRtti.GetVariant(Instance: TObject; var Dest: Variant);
begin
  RawByteStringToVariant(RecordSave(GetFieldAddr(Instance)^, fTypeInfo), Dest);
end;

procedure TOrmPropInfoRecordRtti.SetVariant(Instance: TObject; const Source: Variant);
var
  tmp: RawByteString;
begin
  VariantToRawByteString(Source, tmp);
  RecordLoad(GetFieldAddr(Instance)^, tmp, fTypeInfo);
end;

procedure TOrmPropInfoRecordRtti.NormalizeValue(var Value: RawUtf8);
begin
  // a BLOB should already be normalized
end;

function TOrmPropInfoRecordRtti.CompareValue(Item1, Item2: TObject;
  CaseInsensitive: boolean): integer;
begin
  if RecordEquals(GetFieldAddr(Item1)^, GetFieldAddr(Item2)^, fTypeInfo) then
    result := 0
  else
    result := PtrInt(Item1) - PtrInt(Item2); // pseudo comparison
end;

procedure TOrmPropInfoRecordRtti.SetBinary(Instance: TObject; var Read: TFastReader);
begin
  // use our RecordLoad() binary serialization
  RTTI_BINARYLOAD[rkRecord](GetFieldAddr(Instance), Read, fTypeInfo);
end;

procedure TOrmPropInfoRecordRtti.SetValue(Instance: TObject; Value: PUtf8Char;
  ValueLen: PtrInt; wasString: boolean);
var
  data: RawByteString;
begin
  TextToBinary(Value, data);
  RecordLoad(GetFieldAddr(Instance)^, data, fTypeInfo);
end;

procedure TOrmPropInfoRecordRtti.GetValueVar(Instance: TObject; ToSql: boolean;
  var result: RawUtf8; wasSqlString: PBoolean);
begin
  result := RecordSave(GetFieldAddr(Instance)^, fTypeInfo);
  BinaryToText(result, ToSql, wasSqlString);
end;

function TOrmPropInfoRecordRtti.SetFieldSqlVar(Instance: TObject;
  const aValue: TSqlVar): boolean;
begin
  if aValue.VType = ftBlob then
    result := RecordLoad(GetFieldAddr(Instance)^, aValue.VBlob, fTypeInfo) <> nil
  else
    result := inherited SetFieldSqlVar(Instance, aValue);
end;

procedure TOrmPropInfoRecordRtti.GetFieldSqlVar(Instance: TObject;
  var aValue: TSqlVar; var temp: RawByteString);
begin
  temp := RecordSave(GetFieldAddr(Instance)^, fTypeInfo);
  aValue.Options := [];
  aValue.VType := ftBlob;
  aValue.VBlob := pointer(temp);
  aValue.VBlobLen := length(temp);
end;


{ TOrmPropInfoRecordFixedSize }

procedure TOrmPropInfoRecordFixedSize.CopySameClassProp(Source: TObject;
  DestInfo: TOrmPropInfo; Dest: TObject);
begin
  if TOrmPropInfoRecordFixedSize(DestInfo).fTypeInfo = fTypeInfo then
    MoveFast(GetFieldAddr(Source)^, TOrmPropInfoRecordFixedSize(DestInfo).GetFieldAddr
      (Dest)^, fRecordSize)
  else
    inherited CopySameClassProp(Source, DestInfo, Dest);
end;

function TOrmPropInfoRecordFixedSize.GetSqlFieldRttiTypeName: RawUtf8;
begin
  if fTypeInfo = nil then
    result := inherited GetSqlFieldRttiTypeName
  else
    result := ToUtf8(fTypeInfo^.RawName);
end;

constructor TOrmPropInfoRecordFixedSize.Create(aRecordSize: cardinal;
  const aName: RawUtf8; aPropertyIndex: integer; aPropertyPointer: pointer;
  aAttributes: TOrmPropInfoAttributes; aFieldWidth: integer;
  aData2Text: TOnSqlPropInfoRecord2Text; aText2Data: TOnSqlPropInfoRecord2Data);
begin
  if integer(aRecordSize) <= 0 then
    raise EOrmException.CreateUtf8('%.Create: invalid % record size',
      [self, aRecordSize]);
  fRecordSize := aRecordSize;
  inherited Create(aName, oftBlobCustom, aAttributes, aFieldWidth,
    aPropertyIndex, aPropertyPointer, aData2Text, aText2Data);
end;

procedure TOrmPropInfoRecordFixedSize.GetBinary(Instance: TObject; W: TBufferWriter);
begin
  W.Write(GetFieldAddr(Instance), fRecordSize);
end;

function TOrmPropInfoRecordFixedSize.GetHash(Instance: TObject;
  CaseInsensitive: boolean): cardinal;
begin
  result := DefaultHasher(0, GetFieldAddr(Instance), fRecordSize);
end;

procedure TOrmPropInfoRecordFixedSize.GetValueVar(Instance: TObject;
  ToSql: boolean; var result: RawUtf8; wasSqlString: PBoolean);
begin
  FastSetString(result, GetFieldAddr(Instance), fRecordSize);
  BinaryToText(result, ToSql, wasSqlString);
end;

procedure TOrmPropInfoRecordFixedSize.GetVariant(Instance: TObject; var Dest: Variant);
var
  tmp: RawByteString;
begin
  SetString(tmp, PAnsiChar(GetFieldAddr(Instance)), fRecordSize);
  Dest := tmp;
end;

procedure TOrmPropInfoRecordFixedSize.SetVariant(Instance: TObject;
  const Source: Variant);
begin
  if TVarData(Source).VType = varString then
    MoveFast(TVarData(Source).VAny^, GetFieldAddr(Instance)^, fRecordSize)
  else
    FillCharFast(GetFieldAddr(Instance)^, fRecordSize, 0);
end;

procedure TOrmPropInfoRecordFixedSize.NormalizeValue(var Value: RawUtf8);
begin
  // a BLOB should already be normalized
end;

function TOrmPropInfoRecordFixedSize.CompareValue(Item1, Item2: TObject;
  CaseInsensitive: boolean): integer;
begin
  if (Item1 = Item2) or
     (fRecordSize = 0) then
    result := 0
  else if Item1 = nil then
    result := -1
  else if Item2 = nil then
    result := 1
  else
    result := MemCmp(GetFieldAddr(Item1), GetFieldAddr(Item2), fRecordSize);
end;

procedure TOrmPropInfoRecordFixedSize.SetBinary(Instance: TObject; var Read: TFastReader);
begin
  Read.Copy(GetFieldAddr(Instance), fRecordSize);
end;

procedure TOrmPropInfoRecordFixedSize.SetValue(Instance: TObject;
  Value: PUtf8Char; ValueLen: PtrInt; wasString: boolean);
var
  data: RawByteString;
begin
  TextToBinary(Value, data);
  Value := pointer(data);
  if Value = nil then
    FillCharFast(GetFieldAddr(Instance)^, fRecordSize, 0)
  else
    MoveFast(Value^, GetFieldAddr(Instance)^, fRecordSize);
end;

function TOrmPropInfoRecordFixedSize.SetFieldSqlVar(Instance: TObject;
  const aValue: TSqlVar): boolean;
begin
  if aValue.VType = ftBlob then
  begin
    result := aValue.VBlobLen = fRecordSize;
    if result then
      MoveFast(aValue.VBlob^, GetFieldAddr(Instance)^, fRecordSize)
  end
  else
    result := inherited SetFieldSqlVar(Instance, aValue);
end;

procedure TOrmPropInfoRecordFixedSize.GetFieldSqlVar(Instance: TObject;
  var aValue: TSqlVar; var temp: RawByteString);
begin
  SetString(temp, PAnsiChar(GetFieldAddr(Instance)), fRecordSize);
  aValue.Options := [];
  aValue.VType := ftBlob;
  aValue.VBlob := pointer(temp);
  aValue.VBlobLen := length(temp);
end;


{ TOrmPropInfoCustomJson }

constructor TOrmPropInfoCustomJson.Create(aPropInfo: PRttiProp; aPropIndex: integer);
var
  attrib: TOrmPropInfoAttributes;
begin
  byte(attrib) := 0;
  if aPropInfo^.IsStored(nil) = AS_UNIQUE then
    Include(attrib, aIsUnique); // property MyProperty: RawUtf8 stored AS_UNIQUE;ieldWidth=10
  Create(aPropInfo^.TypeInfo, aPropInfo^.NameUtf8, aPropIndex,
    aPropInfo^.GetFieldAddr(nil), attrib, aPropInfo^.Index);
end;

constructor TOrmPropInfoCustomJson.Create(aTypeInfo: PRttiInfo;
  const aName: RawUtf8; aPropertyIndex: integer; aPropertyPointer: pointer;
  aAttributes: TOrmPropInfoAttributes; aFieldWidth: integer);
begin
  inherited Create(aName, oftUtf8Custom, aAttributes, aFieldWidth,
    aPropertyIndex, aPropertyPointer, nil, nil);
  fTypeInfo := aTypeInfo;
  SetCustomParser(Rtti.RegisterType(aTypeInfo) as TRttiJson);
end;

constructor TOrmPropInfoCustomJson.Create(const aTypeName, aName: RawUtf8;
  aPropertyIndex: integer; aPropertyPointer: pointer;
  aAttributes: TOrmPropInfoAttributes; aFieldWidth: integer);
begin
  inherited Create(aName, oftUtf8Custom, aAttributes, aFieldWidth,
    aPropertyIndex, aPropertyPointer, nil, nil);
  SetCustomParser(Rtti.RegisterTypeFromName(aTypeName) as TRttiJson);
end;

function TOrmPropInfoCustomJson.GetSqlFieldRttiTypeName: RawUtf8;
begin
  if fTypeInfo = nil then
    result := inherited GetSqlFieldRttiTypeName
  else
    result := ToUtf8(fTypeInfo^.RawName);
end;

procedure TOrmPropInfoCustomJson.SetCustomParser(aCustomParser: TRttiJson);
begin
  if aCustomParser = nil then
    raise EOrmException.CreateUtf8(
      '%.SetCustomParser: Invalid type information for %', [self, Name]);
  fCustomParser := aCustomParser;
end;

procedure TOrmPropInfoCustomJson.GetBinary(Instance: TObject; W: TBufferWriter);
var
  json: RawUtf8;
begin
  GetValueVar(Instance, false, json, nil);
  W.Write(json);
end;

procedure TOrmPropInfoCustomJson.SetBinary(Instance: TObject; var Read: TFastReader);
var
  tmp: TSynTempBuffer; // temporary copy
begin
  // stored as JSON VarString in the binary stream
  Read.VarBlob(tmp);
  try
    SetValue(Instance, tmp.buf, tmp.Len, false);
  finally
    tmp.Done;
  end;
end;

procedure TOrmPropInfoCustomJson.NormalizeValue(var Value: RawUtf8);
begin
  // do nothing: should already be normalized
end;

procedure TOrmPropInfoCustomJson.GetJsonValues(Instance: TObject; W: TTextWriter);
var
  Data: PByte;
begin
  Data := GetFieldAddr(Instance);
  W.AddRttiCustomJson(Data, fCustomParser, []);
end;

procedure TOrmPropInfoCustomJson.GetValueVar(Instance: TObject; ToSql: boolean;
  var result: RawUtf8; wasSqlString: PBoolean);
var
  W: TJsonSerializer;
  temp: TTextWriterStackBuffer;
begin
  W := TJsonSerializer.CreateOwnedStream(temp);
  try
    GetJsonValues(Instance, W);
    W.SetText(result);
    if wasSqlString <> nil then
      wasSqlString^ := (result <> '') and
                       (result[1] = '"');
  finally
    W.Free;
  end;
end;

procedure TOrmPropInfoCustomJson.SetValue(Instance: TObject; Value: PUtf8Char;
  ValueLen: PtrInt; wasString: boolean);
var
  Data: PByte;
  B: PUtf8Char;
  tmp: RawUtf8;
begin
  Data := GetFieldAddr(Instance);
  if Value <> nil then
  begin
    // exact JSON string, array of objet ?
    case GotoNextNotSpace(Value)^ of
      '{',
      '[':
        B := GotoEndJsonItem(Value);
      '"':
        begin
          B := GotoEndOfJsonString(Value);
          if B^ <> '"' then
            B := nil;
        end;
    else
      B := nil;
    end;
    if (B = nil) or
       (B - Value <> ValueLen) then
    begin
      QuotedStrJson(Value, ValueLen, tmp); // need escaping as JSON string
      Value := pointer(tmp);
    end;
  end;
  fCustomParser.ValueLoadJson(Data, Value, nil, JSONPARSER_TOLERANTOPTIONS, nil);
end;



{ TOrmPropInfoList }

constructor TOrmPropInfoList.Create(aTable: TClass; aOptions: TOrmPropInfoListOptions);
begin
  fTable := aTable;
  fOptions := aOptions;
  if pilSubClassesFlattening in fOptions then
    InternalAddParentsFirst(aTable, nil)
  else
    InternalAddParentsFirst(aTable);
end;

destructor TOrmPropInfoList.Destroy;
var
  i: PtrInt;
begin
  for i := 0 to fCount - 1 do
    fList[i].Free;
  inherited;
end;

// we don't use TRttiCustom.Props but the raw RTTI which doesn't include fID

procedure TOrmPropInfoList.InternalAddParentsFirst(aClassType: TClass;
  aFlattenedProps: PRttiPropDynArray);
var
  p: PRttiProp;
  i, prev: integer;
begin
  if aClassType = nil then
    exit; // no RTTI information (e.g. reached TObject level)
  // recursive call to include all parent properties first
  if not (pilSingleHierarchyLevel in fOptions) then
    InternalAddParentsFirst(GetClassParent(aClassType), aFlattenedProps);
  // append this level of class hierarchy
  for i := 1 to GetRttiProp(aClassType, p) do
  begin
    if (p^.TypeInfo^.Kind = rkClass) and
       (ClassOrmFieldType(p^.TypeInfo) in [oftObject, oftUnknown]) then
    begin
      prev := PtrArrayAdd(aFlattenedProps, p);
      InternalAddParentsFirst(p^.TypeInfo^.RttiClass^.RttiClass, aFlattenedProps);
      SetLength(aFlattenedProps, prev);
    end
    else if not (pilIgnoreIfGetter in fOptions) or
            p^.GetterIsField then
      Add(TOrmPropInfoRtti.CreateFrom(p, Count, fOptions, aFlattenedProps));
    p := p^.Next;
  end;
end;

procedure TOrmPropInfoList.InternalAddParentsFirst(aClassType: TClass);
var
  p: PRttiProp;
  i: integer;
begin
  if aClassType = nil then
    exit; // no RTTI information (e.g. reached TObject level)
  if not (pilSingleHierarchyLevel in fOptions) then
    InternalAddParentsFirst(GetClassParent(aClassType));
  for i := 1 to GetRttiProp(aClassType, p) do
  begin
    Add(TOrmPropInfoRtti.CreateFrom(p, Count, fOptions, nil));
    p := p^.Next;
  end;
end;

procedure TOrmPropInfoList.AfterAdd;
var
  i: PtrInt;
begin
  SetLength(fList, fCount);
  // initialize once the ordered lookup indexes, for binary search
  SetLength(fOrderedByName, fCount);
  for i := 0 to fCount - 1 do
    fOrderedByName[i] := i;
  QuickSortByName(0, fCount - 1);
end;

function TOrmPropInfoList.Add(aItem: TOrmPropInfo): integer;
var
  f: PtrInt;
begin
  if aItem = nil then
  begin
    result := -1;
    exit;
  end;
  // check that this property is not an ID/RowID (handled separately)
  if IsRowID(pointer(aItem.Name)) and
     not (pilAllowIDFields in fOptions) then
    raise EOrmException.CreateUtf8(
      '%.Add: % should not include a [%] published property', [self, fTable, aItem.Name]);
  // check that this property name is not already defined
  for f := 0 to fCount - 1 do
    if IdemPropNameU(fList[f].Name, aItem.Name) then
      raise EOrmException.CreateUtf8(
        '%.Add: % has duplicated name [%]', [self, fTable, aItem.Name]);
  // add to the internal list
  result := fCount;
  if result >= length(fList) then
    SetLength(fList, NextGrow(result));
  inc(fCount);
  fList[result] := aItem;
  fOrderedByName := nil; // force recompute sorted name array
end;

function TOrmPropInfoList.GetItem(aIndex: PtrInt): TOrmPropInfo;
begin
  if PtrUInt(aIndex) >= PtrUInt(fCount) then
    raise EOrmException.Create('Invalid TOrmPropInfoList index');
  result := fList[aIndex];
end;

procedure TOrmPropInfoList.QuickSortByName(L, R: PtrInt);
var
  I, J, P: PtrInt;
  tmp: byte;
  pivot: PUtf8Char;
begin
  if L < R then
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      repeat
        pivot := pointer(fList[fOrderedByName[P]].Name);
        while StrIComp(pointer(fList[fOrderedByName[I]].Name), pivot) < 0 do
          inc(I);
        while StrIComp(pointer(fList[fOrderedByName[J]].Name), pivot) > 0 do
          dec(J);
        if I <= J then
        begin
          tmp := fOrderedByName[J];
          fOrderedByName[J] := fOrderedByName[I];
          fOrderedByName[I] := tmp;
          if P = I then
            P := J
          else if P = J then
            P := I;
          inc(I);
          dec(J);
        end;
      until I > J;
      if J - L < R - I then
      begin
        // use recursion only for smaller range
        if L < J then
          QuickSortByName(L, J);
        L := I;
      end
      else
      begin
        if I < R then
          QuickSortByName(I, R);
        R := J;
      end;
    until L >= R;
end;

function TOrmPropInfoList.ByRawUtf8Name(const aName: RawUtf8): TOrmPropInfo;
var
  i: PtrInt;
begin
  i := IndexByName(pointer(aName));
  if i < 0 then
    result := nil
  else
    result := fList[i];
end;

function TOrmPropInfoList.ByName(aName: PUtf8Char): TOrmPropInfo;
var
  i: PtrInt;
begin
  i := IndexByName(aName);
  if i < 0 then
    result := nil
  else
    result := fList[i];
end;

function TOrmPropInfoList.IndexByName(aName: PUtf8Char): PtrInt;
var
  cmp, L, R: PtrInt;
  s: PByteArray;
  C1, C2: byte; // integer/PtrInt are actually slower on FPC
  {$ifdef CPUX86NOTPIC}
  table: TNormTableByte absolute NormToUpperAnsi7Byte;
  {$else}
  table: PByteArray;
  {$endif CPUX86NOTPIC}
label
  end1;
begin
  if (self <> nil) and
     (aName <> nil) and
     (fCount > 0) then
  begin
    {$ifndef CPUX86NOTPIC}
    table := @NormToUpperAnsi7Byte;
    {$endif CPUX86NOTPIC}
    L := 0;
    R := fCount - 1;
    repeat
      // fast O(log(n)) binary search using inlined StrIComp()
      {$ifdef CPUX64}
      result := L + R;
      result := result shr 1;
      {$else}
      result := (L + R) shr 1;
      {$endif CPUX64}
      s := pointer(aName);
      cmp := PtrUInt(fList[fOrderedByName[result]].Name);
      dec(cmp, PtrUInt(s));
      repeat
        C1 := table[s[0]];
        C2 := table[s[cmp]];
        s := @s[1];
        if C1 = 0 then
          goto end1;
      until C1 <> C2;
      cmp := result + 1; // compile as 2 branchless cmovc/cmovnc on FPC
      dec(result);
      if C1 > C2 then
        L := cmp
      else
        R := result;
      if L > R then
        break;
      continue;
end1: if C2 <> 0 then
        break;
      result := fOrderedByName[result]; // reached ending #0 on both names
      exit;
    until false;
  end;
  result := -1;
end;

function TOrmPropInfoList.IndexByName(const aName: RawUtf8): integer;
begin
  result := IndexByName(pointer(aName));
end;

function TOrmPropInfoList.IndexByNameOrExceptShort(aName: ShortString): integer;
begin
  if IsRowIDShort(aName) then
    result := -1
  else
  begin
    aName[ord(aName[0]) + 1] := #0; // make ASCIIZ
    result := IndexByName(@aName[1]); // fast O(log(n)) binary search
    if result < 0 then
      raise EOrmException.CreateUtf8(
        '%.IndexByNameOrExceptShort(%): unkwnown in %', [self, aName, fTable]);
  end;
end;

function TOrmPropInfoList.IndexByNameOrExcept(const aName: RawUtf8): integer;
begin
  if IsRowID(pointer(aName)) then
    result := -1
  else
  begin
    result := IndexByName(pointer(aName)); // fast O(log(n)) binary search
    if result < 0 then
      raise EOrmException.CreateUtf8(
        '%.IndexByNameOrExcept(%): unkwnown field in %', [self, aName, fTable]);
  end;
end;

procedure TOrmPropInfoList.IndexesByNamesOrExcept(const aNames: array of RawUtf8;
  const aIndexes: array of PInteger);
var
  i: PtrInt;
begin
  if high(aNames) <> high(aIndexes) then
    raise EOrmException.CreateUtf8('%.IndexesByNamesOrExcept(?)', [self]);
  for i := 0 to high(aNames) do
    if aIndexes[i] = nil then
      raise EOrmException.CreateUtf8('%.IndexesByNamesOrExcept(aIndexes[%]=nil)',
        [self, aNames[i]])
    else
      aIndexes[i]^ := IndexByNameOrExcept(aNames[i]);
end;

procedure TOrmPropInfoList.NamesToRawUtf8DynArray(var Names: TRawUtf8DynArray);
var
  i: PtrInt;
begin
  SetLength(Names, Count);
  for i := 0 to Count - 1 do
    Names[i] := fList[i].Name;
end;

function TOrmPropInfoList.IndexByNameUnflattenedOrExcept(const aName: RawUtf8): integer;
begin
  if pilSubClassesFlattening in fOptions then
  begin
    // O(n) iteration over unflattened field names
    for result := 0 to Count - 1 do
      if IdemPropNameU(List[result].NameUnflattened, aName) then
        exit;
  end
  else
  begin
    // direct - and faster - O(log(n)) binary search
    result := IndexByName(pointer(aName));
    if result >= 0 then
      exit;
  end;
  raise EOrmException.CreateUtf8(
    '%.IndexByNameUnflattenedOrExcept(%): unkwnown field in %', [self, aName, fTable]);
end;


function ClassOrmFieldType(info: PRttiInfo): TOrmFieldType;
var
  CT: PRttiClass;
  C: TClass;
  T: PClassArray; // for better code generation (especially on x86_64)
begin
  CT := info.RttiClass;
  T := @CLASSORMFIELDTYPELIST;
  result := oftUnknown;
  repeat
    // unrolled several InheritsFrom() calls
    C := CT^.RttiClass;
    if C = T[0] then         // TObject
      break; // loop over all parent classes until root TObject is reached
    if C <> T[1] then        // TOrmMany
      if C <> T[2] then      // TOrm
        if (C <> T[3]) and   // TRawUtf8List
           (C <> T[4]) and   // TStrings
           (C <> T[5]) and   // TObjectList
           (C <> T[6]) then  // TCollection
        begin
          if CT^.PropCount > 0 then
            // identify any class with published properties as oftObject
            // but continue searching for any known class
            result := oftObject;
          CT := CT^.ParentInfo.RttiClass;
          continue;
        end
        else
        begin
          result := oftObject;
          break;
        end
      else
      begin
        result := oftID; // TOrm field is pointer(RecordID), not an Instance
        break;
      end
    else
    begin
      result := oftMany; // no data is stored here, but in a pivot table
      break;
    end;
  until false;
end;

function GetOrmFieldType(Info: PRttiInfo): TOrmFieldType;
begin // very fast, thanks to the TypeInfo() compiler-generated function
  case Info^.Kind of
    rkInteger:
      begin
        result := oftInteger; // works also for otSQWord,otUQWord
        exit; // direct exit is faster in generated asm code
      end;
    rkInt64:
      if (Info = TypeInfo(TRecordReference)) or
         (Info = TypeInfo(TRecordReferenceToBeDeleted)) then
      begin
        result := oftRecord;
        exit;
      end
      else if Info = TypeInfo(TCreateTime) then
      begin
        result := oftCreateTime;
        exit;
      end
      else if Info = TypeInfo(TModTime) then
      begin
        result := oftModTime;
        exit;
      end
      else if Info = TypeInfo(TTimeLog) then
      begin
        result := oftTimeLog;
        exit;
      end
      else if Info = TypeInfo(TUnixTime) then
      begin
        result := oftUnixTime;
        exit;
      end
      else if Info = TypeInfo(TUnixMSTime) then
      begin
        result := oftUnixMSTime;
        exit;
      end
      else if Info = TypeInfo(TID) then
      begin
        result := oftTID;
        exit;
      end
      else if Info = TypeInfo(TSessionUserID) then
      begin
        result := oftSessionUserID;
        exit;
      end
      else if Info = TypeInfo(TRecordVersion) then
      begin
        result := oftRecordVersion;
        exit;
      end
      else if (ord(Info^.RawName[1]) and $df = ord('T')) and
        // T...ID pattern in type name -> TID
        (PWord(@Info^.RawName[ord(Info^.RawName[0]) - 1])^ and $dfdf =
           ord('I') + ord('D') shl 8) then
      begin
        result := oftTID;
        exit;
      end
      else
      begin
        result := oftInteger;
        exit;
      end;
    {$ifdef FPC}
    rkBool:
      begin
        result := oftBoolean;
        exit;
      end;
    rkQWord:
      begin
        result := oftInteger;
        exit;
      end;
    {$endif FPC}
    rkSet:
      begin
        result := oftSet;
        exit;
      end;
    rkEnumeration:
      if Info.IsBoolean then
      begin // also circumvent a Delphi RTTI bug
        result := oftBoolean;
        exit;
      end
      else
      begin
        result := oftEnumerate;
        exit;
      end;
    rkFloat:
      if Info.IsCurrency then
      begin
        result := oftCurrency;
        exit;
      end
      else if Info = TypeInfo(TDateTime) then
      begin
        result := oftDateTime;
        exit;
      end
      else if Info = TypeInfo(TDateTimeMS) then
      begin
        result := oftDateTimeMS;
        exit;
      end
      else
      begin
        result := oftFloat;
        exit;
      end;
    rkLString:
      // do not use AnsiStringCodePage since AnsiString = GetAcp may change
      if (Info = TypeInfo(RawBlob)) or
         (Info = TypeInfo(RawByteString)) then
      begin
        result := oftBlob;
        exit;
      end
      else if Info = TypeInfo(WinAnsiString) then
      begin
        result := oftAnsiText;
        exit;
      end
      else
      begin
        result := oftUtf8Text; // CP_UTF8,CP_UTF16 and any other to UTF-8 text
        exit;
      end;
    {$ifdef HASVARUSTRING} rkUString, {$endif}
    rkChar,
    rkWChar,
    rkWString:
      begin
        result := oftUtf8Text;
        exit;
      end;
    rkDynArray:
      begin
        result := oftBlobDynArray;
        exit;
      end;
    {$ifdef PUBLISHRECORD}
    rkRecord {$ifdef FPC}, rkObject{$endif}:
      begin
        result := oftUtf8Custom;
        exit;
      end;
    {$endif PUBLISHRECORD}
    rkVariant:
      begin // this function does not need to handle oftNullable
        result := oftVariant;
        exit;
      end;
    rkClass:
      begin
        result := ClassOrmFieldType(Info);
        exit;
      end;
    // note: tkString (shortstring) and tkInterface not handled
  else
    begin
      result := oftUnknown;
      exit;
    end;
  end;
end;

function NullableTypeToOrmFieldType(aType: PRttiInfo): TOrmFieldType;
begin
  if aType <> nil then
    if aType <> TypeInfo(TNullableInteger) then
      if aType <> TypeInfo(TNullableUtf8Text) then
        if aType <> TypeInfo(TNullableBoolean) then
          if aType <> TypeInfo(TNullableFloat) then
            if aType <> TypeInfo(TNullableCurrency) then
              if aType <> TypeInfo(TNullableDateTime) then
                if aType <> TypeInfo(TNullableTimeLog) then
                begin
                  result := oftUnknown;
                  exit;
                end
                else
                  result := oftTimeLog
              else
                result := oftDateTime
            else
              result := oftCurrency
          else
            result := oftFloat
        else
          result := oftBoolean
      else
        result := oftUtf8Text
    else
      result := oftInteger
  else
    result := oftUnknown;
end;

function OrmFieldTypeToDBField(aOrmFieldType: TOrmFieldType;
  aTypeInfo: PRttiInfo): TSqlDBFieldType;
begin
  if aOrmFieldType = oftNullable then
    aOrmFieldType := NullableTypeToOrmFieldType(aTypeInfo);
  result := SQLFIELDTYPETODBFIELDTYPE[aOrmFieldType];
end;


{ ************ Abstract TOrmTableAbstract Parent Class }

{ TOrmTableAbstract }

constructor TOrmTableAbstract.Create(const aSql: RawUtf8);
begin
  fQuerySql := aSql;
  fFieldIndexID := -1;
  fQueryTableIndexFromSql := -2; // indicates not searched
end;

constructor TOrmTableAbstract.CreateWithColumnTypes(
  const ColumnTypes: array of TOrmFieldType; const aSql: RawUtf8);
begin
  Create(aSql);
  SetLength(fQueryColumnTypes, length(ColumnTypes));
  MoveFast(ColumnTypes[0], fQueryColumnTypes[0], length(ColumnTypes) * SizeOf(TOrmFieldType));
end;

destructor TOrmTableAbstract.Destroy;
begin
  fOwnedRecords.Free;
  inherited Destroy;
end;

procedure TOrmTableAbstract.AssignRead(source: TOrmTableAbstract);
begin
  fData := source.fData;
  {$ifndef NOPOINTEROFFSET}
  fDataStart := source.fDataStart;
  {$endif NOPOINTEROFFSET}
  {$ifndef NOTORMTABLELEN}
  fLen := source.fLen;
  {$endif NOTORMTABLELEN}
  fRowCount := source.fRowCount;
  fFieldCount := source.fFieldCount;
  fFieldType := source.fFieldType;
  fFieldIndexID := source.fFieldIndexID;
end;

function TOrmTableAbstract.GetResults(Offset: PtrInt): PUtf8Char;
begin
  {$ifdef NOPOINTEROFFSET}
  result := fData[Offset];
  {$else}
  result := PUtf8Char(PtrInt(fData[Offset]));
  Offset := PtrUInt(fDataStart); // in two steps for better code generation
  if result = nil then
    Offset := PtrInt(result); // compile as branchless cmove on FPC
  inc(result, Offset);
  {$endif NOPOINTEROFFSET}
end;

function TOrmTableAbstract.GetResultsLen(Offset: PtrInt; P: PUtf8Char): integer;
begin
  {$ifdef NOTORMTABLELEN}
  result := StrLen(P);
  {$else}
  result := fLen[Offset];
  {$endif NOTORMTABLELEN}
end;

function TOrmTableAbstract.GetResultsLen(Offset: PtrInt): integer;
begin
  {$ifdef NOTORMTABLELEN}
  result := StrLen(GetResults(Offset));
  {$else}
  result := fLen[Offset];
  {$endif NOTORMTABLELEN}
end;

procedure TOrmTableAbstract.SetResults(
  Offset: PtrInt; Value: PUtf8Char; ValueLen: integer);
begin
  {$ifndef NOTORMTABLELEN}
  fLen[Offset] := ValueLen;
  {$endif NOTORMTABLELEN}
  {$ifdef NOPOINTEROFFSET}
  fData[Offset] := Value;
  {$else}
  if Value <> nil then
    dec(Value, PtrUInt(fDataStart));
  fData[Offset] := PtrInt(Value);
  {$endif NOPOINTEROFFSET}
end;

procedure TOrmTableAbstract.SetResultsSafe(Offset: PtrInt; Value: PUtf8Char);
begin
  {$ifndef NOTORMTABLELEN}
  if fLen <> nil then
    fLen[Offset] := StrLen(Value);
  {$endif NOTORMTABLELEN}
  {$ifdef NOPOINTEROFFSET}
  fData[Offset] := Value;
  {$else}
  if Value <> nil then
  begin
    dec(Value, PtrUInt(fDataStart));
    if (PtrInt(PtrUInt(Value)) > MaxInt) or
       (PtrInt(PtrUInt(Value)) < -MaxInt) then
      raise EOrmTable.CreateUtf8('%.Results[%] overflow: all PUtf8Char ' +
        'should be in a [-2GB..+2GB] 32-bit range (%) - consider forcing ' +
        'NOPOINTEROFFSET conditional for your project', [self, Offset, Value]);
  end;
  fData[Offset] := PtrInt(Value);
  {$endif NOPOINTEROFFSET}
end;

function TOrmTableAbstract.FieldNames: TRawUtf8DynArray;
begin
  if length(fFieldNames) <> fFieldCount then
    InitFieldNames;
  result := fFieldNames;
end;

const
  ORMTABLE_FIELDNAMEORDERED = 10;

function TOrmTableAbstract.FieldIndex(FieldName: PUtf8Char): PtrInt;
var
  P: PPUtf8CharArray;
  up: PNormTableByte;
begin
  if (self <> nil) and
     (fData <> nil) and
     (FieldName <> nil) and
     (fFieldCount > 0) then
    if IsRowID(FieldName) then
      result := fFieldIndexID // will work for both 'ID' or 'RowID'
    else
    begin
      if length(fFieldNames) <> fFieldCount then
        InitFieldNames;
      P := pointer(fFieldNames);
      if fFieldCount < ORMTABLE_FIELDNAMEORDERED then
      begin
        up := @NormToUpperAnsi7Byte;
        for result := 0 to fFieldCount - 1 do
          if StrICompNotNil(P[result], FieldName, up) = 0 then // good inlining
            exit;
        result := -1;
      end
      else
        result := FastFindIndexedPUtf8Char(P, FieldCount - 1,
          fFieldNameOrder, FieldName, @StrIComp); // O(log(n)) binary search
    end
  else
    result := -1;
end;

function TOrmTableAbstract.FieldIndex(const FieldName: RawUtf8): PtrInt;
begin
  result := FieldIndex(Pointer(FieldName));
end;

function TOrmTableAbstract.FieldIndexExisting(const FieldName: RawUtf8): PtrInt;
begin
  result := FieldIndex(Pointer(FieldName));
  if result < 0 then
    raise EOrmTable.CreateUtf8('%.FieldIndexExisting("%")', [self, FieldName]);
end;

function TOrmTableAbstract.FieldIndex(const FieldNames: array of RawUtf8;
  const FieldIndexes: array of PInteger): PtrInt;
var
  i: PtrInt;
begin
  result := 0;
  if high(FieldNames) < 0 then
    exit;
  if high(FieldNames) <> high(FieldIndexes) then
    raise EOrmTable.CreateUtf8('%.FieldIndex() argument count', [self]);
  for i := 0 to high(FieldNames) do
    if FieldIndexes[i] = nil then
      raise EOrmTable.CreateUtf8('%.FieldIndex() FieldIndexes["%"]=nil',
        [self, FieldNames[i]])
    else
    begin
      FieldIndexes[i]^ := FieldIndex(pointer(FieldNames[i]));
      if FieldIndexes[i]^ >= 0 then
        inc(result);
    end;
end;

procedure TOrmTableAbstract.FieldIndexExisting(const FieldNames: array of RawUtf8;
  const FieldIndexes: array of PInteger);
var
  i: PtrInt;
begin
  if high(FieldNames) < 0 then
    exit;
  if high(FieldNames) <> high(FieldIndexes) then
    raise EOrmTable.CreateUtf8('%.FieldIndexExisting() argument count', [self]);
  for i := 0 to high(FieldNames) do
    if FieldIndexes[i] = nil then
      raise EOrmTable.CreateUtf8('%.FieldIndexExisting() FieldIndexes["%"]=nil',
        [self, FieldNames[i]])
    else
      FieldIndexes[i]^ := FieldIndexExisting(FieldNames[i]);
end;

function TOrmTableAbstract.FieldValue(const FieldName: RawUtf8; Row: PtrInt): PUtf8Char;
var
  Index: integer;
begin
  Index := FieldIndex(pointer(FieldName));
  if (Index < 0) or
     (PtrUInt(Row - 1) >= PtrUInt(fRowCount)) then
    result := nil
  else
    result := GetResults(Index + Row * FieldCount);
end;

procedure TOrmTableAbstract.IDArrayFromBits(const Bits; out IDs: TIDDynArray);
var
  n, r, f: integer;
  id: PInt64;
begin
  f := fFieldIndexID;
  if f < 0 then // no ID column available
    exit;
  n := GetBitsCount(Bits, fRowCount);
  if n = 0 then
    exit;
  SetLength(IDs, n);
  id := pointer(IDs);
  for r := 0 to fRowCount - 1 do
  begin
    inc(f, fFieldCount); // next row - ignore first row = field names
    if GetBitPtr(@Bits, r) then
    begin
      SetInt64(GetResults(f), id^);
      inc(id);
    end;
  end;
end;

procedure TOrmTableAbstract.IDArrayToBits(var Bits; var IDs: TIDDynArray);
var
  r, f, idmax: integer;
  id: pointer;
begin
  if length(IDs) = fRowCount then
  begin
    // all selected -> all bits set to 1
    FillCharFast(Bits, (fRowCount shr 3) + 1, 255);
    exit;
  end;
  FillCharFast(Bits, (fRowCount shr 3) + 1, 0); // all Bits to 0
  // we sort IDs to use FastFindInt64Sorted() and its O(log(n)) binary search
  f := fFieldIndexID; // get id column field index
  id := pointer(IDs);
  if (f < 0) or
     (id = nil) then
    exit; // no id column or no IDs[] selected -> all bits left to 0
  idmax := length(IDs) - 1;
  QuickSortInt64(id, 0, idmax);
  for r := 0 to fRowCount - 1 do
  begin
    inc(f, fFieldCount); // next row - ignore first row = field names
    if FastFindInt64Sorted(id, idmax, GetInt64(GetResults(f))) >= 0 then
      SetBitPtr(@Bits, r);
  end;
end;

function TOrmTableAbstract.RowFromID(aID: TID; aNotFoundMinusOne: boolean): PtrInt;
var
  id: RawUtf8;
  f: PtrInt;
begin
  if (self <> nil) and
     (fRowCount > 0) and
     (aID > 0) and
     (fFieldIndexID >= 0) then
  begin
    Int64ToUtf8(aID, id);
    f := fFieldIndexID + fFieldCount; // ignore first row = field names
    for result := 1 to fRowCount do
      if StrComp(GetResults(f), pointer(id)) = 0 then
        exit
      else
        inc(f, fFieldCount); // next row
  end;
  if aNotFoundMinusOne then
    result := -1
  else
    result := fRowCount; // not found -> return last row index
end;

function TOrmTableAbstract.DeleteRow(Row: PtrInt): boolean;
begin
  if (self = nil) or
     (Row < 1) or
     (Row > fRowCount) then
  begin
    result := false;
    exit; // out of range
  end;
  if Row < fRowCount then
  begin
    Row := Row * FieldCount; // convert row index into position in fData[Offset]
    MoveFast(fData[Row + FieldCount], fData[Row],
      (fRowCount * FieldCount - Row) * SizeOf(fData[Row]));
  end;
  dec(fRowCount);
  result := true;
end;

procedure TOrmTableAbstract.InitFieldNames;
var
  f: PtrInt;
  P: PUtf8Char;
begin
  SetLength(fFieldNames, fFieldCount); // share one TRawUtf8DynArray
  for f := 0 to fFieldCount - 1 do
  begin
    P := GetResults(f);
    if IsRowID(P) then // normalize RowID field name to 'ID'
      fFieldNames[f] := ID_TXT
    else
      FastSetString(fFieldNames[f], P, GetResultsLen(f, P));
  end;
  if fFieldCount >= ORMTABLE_FIELDNAMEORDERED then
    QuickSortIndexedPUtf8Char(pointer(fFieldNames), fFieldCount, fFieldNameOrder);
end;

procedure TOrmTableAbstract.GetAsVariant(row, field: PtrInt; out value: variant;
  expandTimeLogAsText, expandEnumsAsText, expandHugeIDAsUniqueIdentifier: boolean;
  options: TDocVariantOptions);
const
  JAN2015_UNIX = 1420070400;
var
  t: TTimeLogBits;
  id: TSynUniqueIdentifierBits;
  V: PUtf8Char;
  enum, err: integer;
  time: RawUtf8;
begin
  if (self = nil) or
     (row < 1) or
     (row > fRowCount) or
     (PtrUInt(field) >= PtrUInt(fFieldCount)) then
    exit; // out of range
  if fFieldType = nil then
    InitFieldTypes;
  row := row * fFieldCount + field;
  V := GetResults(row);
  with fFieldType[field] do
    if expandHugeIDAsUniqueIdentifier and
       (field = fFieldIndexID) then
    begin
      SetInt64(V, PInt64(@id)^);
      if id.CreateTimeUnix > JAN2015_UNIX then
        id.ToVariant(value)
      else
        value := id.Value;
    end
    else
    begin
      if expandEnumsAsText and
         (ContentType = oftEnumerate) then
      begin
        enum := GetInteger(V, err);
        if (err = 0) and
           (ContentTypeInfo <> nil) then
        begin
          value := PRttiEnumType(ContentTypeInfo)^.GetEnumNameOrd(enum)^;
          exit;
        end;
      end
      else if expandTimeLogAsText then
        case ContentType of
          oftTimeLog,
          oftModTime,
          oftCreateTime,
          oftUnixTime,
          oftUnixMSTime:
            begin
              SetInt64(V, {%H-}t.Value);
              if t.Value = 0 then
                value := 0
              else
              begin
                if ContentType = oftUnixTime then
                  t.FromUnixTime(t.Value);
                if ContentType <> oftUnixMSTime then
                  time := t.Text(true)
                else
                  // no TTimeLog use for milliseconds resolution
                  time := UnixMSTimeToString(t.Value);
                TDocVariantData(value).InitObject([
                  'Time', time,
                  'Value', t.Value], JSON_FAST);
              end;
              exit;
            end;
        end;
      ValueVarToVariant(V, GetResultsLen(row, V), ContentType, TVarData(value), true,
        ContentTypeInfo, options);
    end;
end;

procedure TOrmTableAbstract.ToDocVariant(Row: PtrInt; out doc: variant;
  options: TDocVariantOptions; expandTimeLogAsText, expandEnumsAsText,
  expandHugeIDAsUniqueIdentifier: boolean);
var
  f: PtrInt;
  v: TVariantDynArray;
begin
  if (self = nil) or
     (Row < 1) or
     (Row > fRowCount) then
    exit; // out of range
  SetLength(v, fFieldCount);
  for f := 0 to fFieldCount - 1 do
    GetAsVariant(Row, f, v[f], expandTimeLogAsText, expandEnumsAsText,
      expandHugeIDAsUniqueIdentifier, options);
  if length(fFieldNames) <> fFieldCount then
    InitFieldNames; // will reuse fFieldNames using COW between rows
  TDocVariantData(doc).InitObjectFromVariants(fFieldNames, v, JSON_FAST);
end;

var
  OrmTableRowVariantType: TCustomVariantType = nil;

procedure TOrmTableAbstract.ToDocVariant(out docs: TVariantDynArray; readonly: boolean);
var
  r: PtrInt;
begin
  if (self = nil) or
     (fRowCount = 0) then
    exit;
  SetLength(docs, fRowCount);
  if readonly then
  begin
    // read-only access with no memory allocation via our TOrmTableRowVariant
    if OrmTableRowVariantType = nil then
      OrmTableRowVariantType := SynRegisterCustomVariantType(TOrmTableRowVariant);
    for r := 0 to fRowCount - 1 do
      with TOrmTableRowVariantData(docs[r]) do
      begin
        VType := OrmTableRowVariantType.VarType;
        VTable := self;
        VRow := r + 1;
      end;
  end
  else
    // manual conversion to stand-alone variants
    for r := 0 to fRowCount - 1 do
      ToDocVariant(r + 1, docs[r]);
end;

procedure TOrmTableAbstract.ToDocVariant(out docarray: variant; readonly: boolean);
var
  Values: TVariantDynArray;
begin
  ToDocVariant(Values, readonly);
  TDocVariantData(docarray).InitArrayFromVariants(Values, JSON_FAST);
end;

function TOrmTableAbstract.DeleteColumnValues(Field: PtrInt): boolean;
var
  i: integer;
begin
  if PtrUInt(Field) >= PtrUInt(fFieldCount) then
    result := false
  else
  begin
    for i := 1 to fRowCount do
    begin
      inc(Field, fFieldCount); // next row - ignore first row = field names
      SetResults(Field, nil, 0);
    end;
    result := true;
  end;
end;

function TOrmTableAbstract.GetQueryTableNameFromSql: RawUtf8;
begin
  if (fQueryTableNameFromSql = '') and
     (fQuerySql <> '') then
    fQueryTableNameFromSql := GetTableNameFromSqlSelect(fQuerySql, true);
  result := fQueryTableNameFromSql;
end;

procedure TOrmTableAbstract.SetFieldType(Field: PtrInt; FieldType: TOrmFieldType;
  FieldTypeInfo: PRttiInfo; FieldSize, FieldTableIndex: integer);
begin
  if (self = nil) or
     (PtrUInt(Field) >= PtrUInt(fFieldCount)) then
    exit;
  if fFieldType = nil then
    InitFieldTypes
  else if Field >= length(fFieldType) then
    SetLength(fFieldType, Field + 1); // e.g. after TOrmTableWritable.AddField
  with fFieldType[Field] do
  begin
    ContentType := FieldType;
    ContentSize := FieldSize;
    ContentTypeInfo := nil;
    if FieldTypeInfo <> nil then
      case FieldType of
        oftEnumerate:
          if FieldTypeInfo^.Kind = rkEnumeration then
            ContentTypeInfo := FieldTypeInfo^.EnumBaseType;
        oftSet:
          if FieldTypeInfo^.Kind = rkSet then
            ContentTypeInfo := FieldTypeInfo^.SetEnumType;
        oftBlobDynArray:
          ContentTypeInfo := FieldTypeInfo;
        oftNullable:
          begin
            ContentTypeInfo := FieldTypeInfo;
            ContentType := NullableTypeToOrmFieldType(FieldTypeInfo);
            if ContentType = oftUnknown then
              ContentType := oftNullable;
          end;
      end;
    if ContentType in [oftVariant, oftNullable] then
      // ftUtf8/ftNull are not precise enough
      ContentDB := ftUnknown
    else
      ContentDB := SQLFIELDTYPETODBFIELDTYPE[ContentType];
    TableIndex := FieldTableIndex;
  end;
end;

procedure TOrmTableAbstract.SetFieldType(const FieldName: RawUtf8;
  FieldType: TOrmFieldType; FieldTypeInfo: PRttiInfo; FieldSize: integer);
begin
  SetFieldType(FieldIndex(FieldName), FieldType, FieldTypeInfo, FieldSize);
end;

const
  DBTOFIELDTYPE: array[TSqlDBFieldType] of TOrmFieldType = (
    oftUnknown, oftUnknown,
    oftInteger, oftFloat, oftCurrency, oftDateTime, oftUtf8Text, oftBlob);

procedure TOrmTableAbstract.SetFieldTypes(const DBTypes: TSqlDBFieldTypeDynArray);
var
  f: PtrInt;
begin
  if length(DBTypes) <> fFieldCount then
    raise EOrmTable.CreateUtf8('%.SetFieldTypes(DBTypes?)', [self]);
  for f := 0 to fFieldCount - 1 do
    SetFieldType(f, DBTOFIELDTYPE[DBTypes[f]]);
end;

function TOrmTableAbstract.GetRowCount: PtrInt;
begin
  if self = nil then
    result := 0
  else
    result := fRowCount;
end;

function TOrmTableAbstract.InitOneFieldType(field: PtrInt; out size: integer;
  out info: PRttiInfo; out tableindex: integer): TOrmFieldType;
begin
  if Assigned(fQueryColumnTypes) then
    result := fQueryColumnTypes[field]
  else
    result := oftUnknown;
  // overriden TOrmTable.InitOneFieldType will search within fQueryTables
end;

procedure TOrmTableAbstract.InitFieldTypes;
var
  field, f, r, len: PtrInt;
  oft: TOrmFieldType;
  info: PRttiInfo;
  size, tableindex: integer;
  guessed: boolean;
  U: PUtf8Char;
  tlog: TTimeLog;
begin
  if Assigned(fQueryColumnTypes) and
     (fFieldCount <> length(fQueryColumnTypes)) then
    raise EOrmTable.CreateUtf8('%.CreateWithColumnTypes() called ' +
      'with % column types, whereas the result has % columns',
      [self, length(fQueryColumnTypes), fFieldCount]);
  SetLength(fFieldType, fFieldCount);
  for field := 0 to fFieldCount - 1 do
  begin
    size := -1;
    tableindex := -1;
    info := nil;
    guessed := false;
    // init fFieldType[] from fQueryTables/fQueryColumnTypes[]
    oft := InitOneFieldType(field, size, info, tableindex);
    if oft = oftUnknown then
      // not found in fQueryTables/fQueryColumnTypes[]: guess from content
      if IsRowID(Results[field]) then
        oft := oftInteger
      else
      begin
        guessed := true;
        f := field + fFieldCount;
        if byte(field) in fFieldParsedAsString then
        begin
          // the parser identified string values -> check if was oftDateTime
          oft := oftUtf8Text;
          for r := 1 to fRowCount do
          begin
            U := GetResults(f);
            if U = nil then  // search for a non void column
              inc(f, fFieldCount)
            else
            begin
              len := GetResultsLen(f, U);
              tlog := Iso8601ToTimeLogPUtf8Char(U, len);
              if tlog <> 0 then
                if (byte(len) in [8, 10]) and
                   (cardinal(tlog shr 26) - 1800 < 300) then
                  // e.g. YYYYMMDD date (Y=1800..2100)
                  oft := oftDateTime
                else if len >= 15 then
                  // e.g. YYYYMMDDThhmmss date/time value
                  oft := oftDateTime;
              break;
            end;
          end;
        end
        else
          for r := 1 to fRowCount do
          begin
            U := Results[f];
            oft := Utf8ContentNumberType(U);
            inc(f, fFieldCount);
            if oft = oftUnknown then
              // null -> search for next non-void column
              continue
            else if oft = oftInteger then // may be a floating point with no decimal
              if FieldTypeIntegerDetectionOnAllRows then
                continue
              else
                // we only checked the first field -> best guess...
                oft := oftCurrency;
            break; // found a non-integer content (e.g. oftFloat/oftUtf8Text)
          end;
      end;
    SetFieldType(field, oft, info, size, tableindex);
    if guessed then
      fFieldType[field].ContentDB := ftUnknown; // may fail on some later row
  end;
end;

function TOrmTableAbstract.FieldType(Field: PtrInt): TOrmFieldType;
begin
  if (self <> nil) and
     (PtrUInt(Field) < PtrUInt(fFieldCount)) then
  begin
    if fFieldType = nil then
      InitFieldTypes;
    result := fFieldType[Field].ContentType;
  end
  else
    result := oftUnknown;
end;

function TOrmTableAbstract.FieldType(Field: integer;
  out FieldTypeInfo: POrmTableFieldType): TOrmFieldType;
begin
  if (self <> nil) and
     (PtrUInt(Field) < PtrUInt(fFieldCount)) then
  begin
    if fFieldType = nil then
      InitFieldTypes;
    FieldTypeInfo := @fFieldType[Field];
    result := FieldTypeInfo^.ContentType;
  end
  else
  begin
    FieldTypeInfo := nil;
    result := oftUnknown;
  end;
end;

function TOrmTableAbstract.GetID(Row: PtrInt): TID;
begin
  if (self = nil) or
     (fData = nil) or
     (fFieldIndexID < 0) or
     (PtrUInt(Row) > PtrUInt(fRowCount)) then
    result := 0
  else
    SetID(GetResults(Row * fFieldCount + fFieldIndexID), result{%H-});
end;

function TOrmTableAbstract.Get(Row, Field: PtrInt): PUtf8Char;
begin
  if (self = nil) or
     (fData = nil) or
     (PtrUInt(Row) > PtrUInt(fRowCount)) or
     (PtrUInt(Field) >= PtrUInt(fFieldCount)) then
    result := nil
  else
  begin
    inc(Field, Row * fFieldCount);
    {$ifdef NOPOINTEROFFSET} // inlined GetResults() for Delphi 7
    result := fData[Field];
    {$else}
    result := PUtf8Char(PtrInt(fData[Field]));
    if result <> nil then
      inc(result, PtrUInt(fDataStart));
    {$endif NOPOINTEROFFSET}
  end;
end;

function TOrmTableAbstract.Get(Row, Field: PtrInt; out Len: integer): PUtf8Char;
begin
  if (self = nil) or
     (fData = nil) or
     (PtrUInt(Row) > PtrUInt(fRowCount)) or
     (PtrUInt(Field) >= PtrUInt(fFieldCount)) then
    result := nil
  else
  begin
    inc(Field, Row * fFieldCount);
    {$ifdef NOPOINTEROFFSET} // inlined GetResults() for Delphi 7
    result := fData[Field];
    {$else}
    result := PUtf8Char(PtrInt(fData[Field]));
    if result <> nil then
      inc(result, PtrUInt(fDataStart));
    {$endif NOPOINTEROFFSET}
    {$ifdef NOTORMTABLELEN}
    Len := StrLen(result);
    {$else}
    Len := fLen[Field];
    {$endif NOTORMTABLELEN}
  end;
end;

function TOrmTableAbstract.GetU(Row, Field: PtrInt): RawUtf8;
var
  P: PUtf8Char;
  PLen: integer;
begin
  P := Get(Row, Field, PLen);
  if P = nil then
    FastAssignNew(result)
  else
    FastSetString(result, P, PLen);
end;

function TOrmTableAbstract.Get(Row: PtrInt; const FieldName: RawUtf8): PUtf8Char;
begin
  result := Get(Row, FieldIndex(FieldName));
end;

function TOrmTableAbstract.GetU(Row: PtrInt; const FieldName: RawUtf8): RawUtf8;
begin
  result := GetU(Row, FieldIndex(FieldName));
end;

function TOrmTableAbstract.GetA(Row, Field: PtrInt): WinAnsiString;
begin
  result := Utf8ToWinAnsi(Get(Row, Field));
end;

function TOrmTableAbstract.GetAsInteger(Row, Field: PtrInt): integer;
begin
  result := GetInteger(Get(Row, Field));
end;

function TOrmTableAbstract.GetAsInteger(Row: PtrInt; const FieldName: RawUtf8): integer;
begin
  result := GetInteger(Get(Row, FieldIndex(FieldName)));
end;

function TOrmTableAbstract.GetAsInt64(Row, Field: PtrInt): Int64;
begin
  SetInt64(Get(Row, Field), result{%H-});
end;

function TOrmTableAbstract.GetAsInt64(Row: PtrInt; const FieldName: RawUtf8): Int64;
begin
  SetInt64(Get(Row, FieldIndex(FieldName)), result{%H-});
end;

function TOrmTableAbstract.GetAsFloat(Row, Field: PtrInt): TSynExtended;
begin
  result := GetExtended(Get(Row, Field));
end;

function TOrmTableAbstract.GetAsFloat(Row: PtrInt; const FieldName: RawUtf8): TSynExtended;
begin
  result := GetExtended(Get(Row, FieldIndex(FieldName)));
end;

function TOrmTableAbstract.GetAsCurrency(Row, Field: PtrInt): currency;
begin
  PInt64(@result)^ := StrToCurr64(Get(Row, Field), nil);
end;

function TOrmTableAbstract.GetAsCurrency(Row: PtrInt; const FieldName: RawUtf8): currency;
begin
  result := GetAsCurrency(Row, FieldIndex(FieldName));
end;

function TOrmTableAbstract.GetAsDateTime(Row, Field: PtrInt): TDateTime;
var
  P: PUtf8Char;
begin
  result := 0;
  if Row = 0 then
    exit; // header
  P := Get(Row, Field);
  if P = nil then
    exit;
  case FieldType(Field) of
    oftCurrency,
    oftFloat:
      result := GetExtended(P);
    oftInteger, // TOrmTableAbstract.InitFieldTypes may have recognized an integer
    oftTimeLog,
    oftModTime,
    oftCreateTime:
      result := TimeLogToDateTime(GetInt64(P));
    oftUnixTime:
      result := UnixTimeToDateTime(GetInt64(P));
    oftUnixMSTime:
      result := UnixMSTimeToDateTime(GetInt64(P));
  else // oftDateTime and any other kind will try from ISO-8601 text
    result := Iso8601ToDateTimePUtf8Char(P);
  end;
end;

function TOrmTableAbstract.GetAsDateTime(Row: PtrInt; const FieldName: RawUtf8): TDateTime;
begin
  result := GetAsDateTime(Row, FieldIndex(FieldName));
end;

function TOrmTableAbstract.GetS(Row, Field: PtrInt): shortstring;
begin
  Utf8ToShortString(result, Get(Row, Field));
end;

function TOrmTableAbstract.GetString(Row, Field: PtrInt): string;
var
  U: PUtf8Char;
  ULen: integer;
begin
  U := Get(Row, Field, ULen);
  if U = nil then
    result := ''
  else
    {$ifdef UNICODE}
    Utf8DecodeToUnicodeString(U, ULen, result);
    {$else}
    CurrentAnsiConvert.Utf8BufferToAnsi(U, ULen, RawByteString(result));
    {$endif UNICODE}
end;

function TOrmTableAbstract.GetSynUnicode(Row, Field: PtrInt): SynUnicode;
var
  U: PUtf8Char;
  ULen: integer;
begin
  result := '';
  U := Get(Row, Field, ULen);
  if U <> nil then
    Utf8ToSynUnicode(U, ULen, result);
end;

function TOrmTableAbstract.GetCaption(Row, Field: PtrInt): string;
begin
  GetCaptionFromPCharLen(Get(Row, Field), result);
end;

function TOrmTableAbstract.GetBlob(Row, Field: PtrInt): RawBlob;
begin
  result := BlobToRawBlob(Get(Row, Field));
end;

function TOrmTableAbstract.GetBytes(Row, Field: PtrInt): TBytes;
begin
  result := BlobToBytes(Get(Row, Field));
end;

function TOrmTableAbstract.GetStream(Row, Field: PtrInt): TStream;
begin
  result := BlobToStream(Get(Row, Field));
end;

function TOrmTableAbstract.GetDateTime(Row, Field: PtrInt): TDateTime;
begin
  result := Iso8601ToDateTimePUtf8Char(Get(Row, Field), 0)
end;

function TOrmTableAbstract.GetRowValues(Field: PtrInt;
  out Values: TRawUtf8DynArray): integer;
var
  i: PtrInt;
  U: PUtf8Char;
begin
  result := 0;
  if (self = nil) or
     (PtrUInt(Field) > PtrUInt(fFieldCount)) or
     (fRowCount = 0) then
    exit;
  SetLength(Values, fRowCount);
  for i := 0 to fRowCount - 1 do
  begin
    inc(Field, fFieldCount); // next row - ignore first row = field names
    U := GetResults(Field);
    FastSetString(Values[i], U, GetResultsLen(Field, U));
  end;
  result := fRowCount;
end;

function TOrmTableAbstract.GetRowValues(Field: PtrInt; out Values: TInt64DynArray): integer;
var
  i: PtrInt;
begin
  result := 0;
  if (self = nil) or
     (PtrUInt(Field) > PtrUInt(fFieldCount)) or
     (fRowCount = 0) then
    exit;
  SetLength(Values, fRowCount);
  for i := 0 to fRowCount - 1 do
  begin
    inc(Field, fFieldCount); // next row - ignore first row = field names
    SetInt64(GetResults(Field), Values[i]);
  end;
  result := fRowCount;
end;

function TOrmTableAbstract.GetRowLengths(Field: PtrInt; LenStore: PSynTempBuffer): integer;
var
  len: PInteger;
  i, l: integer;
  n: PtrInt;
begin
  result := 0;
  if (self = nil) or
     (PtrUInt(Field) > PtrUInt(fFieldCount)) or
     (fRowCount = 0) then
  begin
    if LenStore <> nil then
      LenStore^.buf := nil; // avoid GPF in LenStore.Done
    exit;
  end;
  if LenStore = nil then
    len := nil
  else
    len := LenStore.Init(fRowCount * SizeOf(integer));
  n := fFieldCount;
  for i := 1 to fRowCount do
  begin
    inc(Field, n); // next row - ignore first row = field names
    {$ifdef NOTORMTABLELEN}
    l := StrLen(GetResults(Field));
    {$else}
    l := fLen[Field];
    {$endif NOTORMTABLELEN}
    if len <> nil then
    begin
      len^ := l;
      inc(len);
    end;
    inc(result, l);
  end;
end;

function TOrmTableAbstract.GetRowValues(Field: PtrInt; const Sep, Head, Trail: RawUtf8): RawUtf8;
var
  n, L, SepLen: integer;
  i: PtrInt;
  P, U: PUtf8Char;
begin
  L := 0;
  for i := 1 to fRowCount do
    inc(L, ResultsLen[i]);
  if L = 0 then
  begin
    result := Head + Trail;
    exit;
  end;
  SepLen := length(Sep);
  inc(L, length(Head) + SepLen * (fRowCount - 1) + length(Trail));
  FastSetString(result, nil, L);
  P := AppendRawUtf8ToBuffer(pointer(result), Head);
  n := fRowCount;
  repeat
    inc(Field, fFieldCount); // next row - ignore first row = field names
    U := GetResults(Field);
    {$ifdef NOTORMTABLELEN}
    i := StrLen(U);
    {$else}
    i := fLen[Field];
    {$endif NOTORMTABLELEN}
    MoveFast(U^, P^, i);
    inc(P, i);
    dec(n);
    if n = 0 then
      break;
    if SepLen = 0 then
      continue;
    MoveFast(pointer(Sep)^, P^, SepLen); // MoveSmall() won't be inlined anyway
    inc(P, SepLen);
  until false;
end;

procedure TOrmTableAbstract.GetJsonValues(
  W: TJsonWriter; RowFirst, RowLast: PtrInt; IDBinarySize: integer);
var
  U: PUtf8Char;
  f, r, o: PtrInt;
  i64: Int64;
label
  nostr, str;
begin
  if (self = nil) or
     (fFieldCount <= 0) or
     (fRowCount <= 0) then
  begin
    W.Add('[', ']');
    exit;
  end;
  // check range
  if RowLast = 0 then
    RowLast := fRowCount
  else if RowLast > fRowCount then
    RowLast := fRowCount;
  if RowFirst <= 0 then
    RowFirst := 1; // start reading after first Row (Row 0 = Field Names)
  // get col names and types
  if fFieldType = nil then
    InitFieldTypes;
  SetLength(W.ColNames, FieldCount);
  for f := 0 to FieldCount - 1 do
  begin
    W.ColNames[f] := GetU(0, f); // first Row is field Names
    if not Assigned(OnExportValue) then
      if (f = fFieldIndexID) and
         (IDBinarySize > 0) then
        W.ColNames[f] := 'id'; // ajax-friendly
  end;
  W.AddColumns(RowLast - RowFirst + 1); // write or init field names (see JSON Expand)
  if W.Expand then
    W.Add('[');
  // write rows data
  o := fFieldCount * RowFirst;
  for r := RowFirst to RowLast do
  begin
    if W.Expand then
      W.Add('{');
    for f := 0 to FieldCount - 1 do
    begin
      U := GetResults(o);
      if W.Expand then
        W.AddString(W.ColNames[f]); // '"'+ColNames[]+'":'
      if Assigned(OnExportValue) then
        W.AddString(OnExportValue(self, r, f, false))
      else if (IDBinarySize > 0) and
              (f = fFieldIndexID) then
      begin
        SetInt64(U, i64{%H-});
        W.AddBinToHexDisplayQuoted(@i64, IDBinarySize);
      end
      else if U = nil then
        W.AddNull
      else
        case fFieldType[f].ContentDB of
          ftInt64,
          ftDouble,
          ftCurrency:
nostr:      {$ifdef NOTORMTABLELEN}
            W.AddNoJsonEscape(U, StrLen(U));
            {$else}
            W.AddNoJsonEscape(U, fLen[o]);
            {$endif NOTORMTABLELEN}
          ftDate,
          ftUtf8,
          ftBlob:
            begin
str:          W.Add('"');
              W.AddJsonEscape(U, 0); // Len=0 is slightly faster
              W.Add('"');
            end;
        else
          if IsStringJson(U) then // fast and safe enough
            goto str
          else
            goto nostr;
        end;
      W.AddComma;
      inc(o); // points to next value
    end;
    W.CancelLastComma;
    if W.Expand then
    begin
      W.Add('}', ',');
      if r <> RowLast then
        W.AddCR; // make expanded json more human readable
    end
    else
      W.AddComma;
  end;
  W.EndJsonObject(1, 0, false); // "RowCount": set by W.AddColumns() above
end;

procedure TOrmTableAbstract.GetJsonValues(Json: TStream; Expand: boolean;
  RowFirst, RowLast: PtrInt; IDBinarySize: integer);
var
  W: TJsonWriter;
  tmp: TTextWriterStackBuffer;
begin
  W := TJsonWriter.Create(Json, Expand, false, nil, 0, @tmp);
  try
    GetJsonValues(W, RowFirst, RowLast, IDBinarySize);
    W.FlushFinal;
  finally
    W.Free;
  end;
end;

function TOrmTableAbstract.GetJsonValues(Expand: boolean;
  IDBinarySize, BufferSize: integer): RawUtf8;
var
  W: TJsonWriter;
  tmp: TTextWriterStackBuffer;
begin
  if BufferSize < SizeOf(tmp) then
    W := TJsonWriter.CreateOwnedStream(tmp)
  else
    W := TJsonWriter.CreateOwnedStream(BufferSize);
  try
    W.Expand := Expand;
    GetJsonValues(W, 0, 0, IDBinarySize); // create JSON data in MS
    W.SetText(result);
  finally
    W.Free;
  end;
end;

procedure TOrmTableAbstract.GetCsvValues(Dest: TStream; Tab: boolean;
  CommaSep: AnsiChar; AddBOM: boolean; RowFirst, RowLast: PtrInt);
var
  U: PUtf8Char;
  F, R, FMax, o, len: PtrInt;
  W: TTextWriter;
  temp: TTextWriterStackBuffer;
begin
  if (self = nil) or
     (FieldCount <= 0) or
     (fRowCount <= 0) then
    exit;
  if (RowLast = 0) or
     (RowLast > fRowCount) then
    RowLast := fRowCount;
  if RowFirst < 0 then
    RowFirst := 0;
  W := TTextWriter.Create(Dest, @temp, SizeOf(temp));
  try
    if AddBOM then
      W.AddShorter(#$ef#$bb#$bf); // add UTF-8 Byte Order Mark
    if Tab then
      CommaSep := #9;
    FMax := FieldCount - 1;
    o := RowFirst * FieldCount;
    if Assigned(OnExportValue) then
      for R := RowFirst to RowLast do
        for F := 0 to FMax do
          W.AddString(OnExportValue(self, R, F, false))
    else
      for R := RowFirst to RowLast do
        for F := 0 to FMax do
        begin
          U := GetResults(o);
          len := GetResultsLen(o, U);
          if Tab or not IsStringJson(U) then
            W.AddNoJsonEscape(U, len)
          else
            W.AddQuotedStr(U, len, '"');
          if F = FMax then
            W.AddCR
          else
            W.Add(CommaSep);
          inc(o); // points to next value
        end;
    W.FlushFinal;
  finally
    W.Free;
  end;
end;

function TOrmTableAbstract.GetCsvValues(Tab: boolean; CommaSep: AnsiChar;
  AddBOM: boolean; RowFirst, RowLast: PtrInt): RawUtf8;
var
  MS: TRawByteStringStream;
begin
  MS := TRawByteStringStream.Create;
  try
    GetCsvValues(MS, Tab, CommaSep, AddBOM, RowFirst, RowLast);
    result := MS.DataString;
  finally
    MS.Free;
  end;
end;

procedure TOrmTableAbstract.GetMSRowSetValues(Dest: TStream; RowFirst, RowLast: PtrInt);
const
  FIELDTYPE_TOXML: array[TSqlDBFieldType] of RawUtf8 = (
    '', '', ' dt:type="i8"', ' dt:type="float"',
    ' dt:type="number" rs:dbtype="currency"',
    ' dt:type="dateTime"', ' dt:type="string"', ' dt:type="bin.hex"');
var
  W: TTextWriter;
  f, r: PtrInt;
  o: PtrInt;
  U: PUtf8Char;
begin
  W := TTextWriter.Create(Dest, 65536);
  try
    W.AddShort('<xml xmlns:s="uuid:BDC6E3F0-6DA3-11d1-A2A3-00AA00C14882" ' +
      'xmlns:dt="uuid:C2F41010-65B3-11d1-A29F-00AA00C14882" ' +
      'xmlns:rs="urn:schemas-microsoft-com:rowset" xmlns:z="#RowsetSchema">');
    if (self <> nil) and
       ((FieldCount > 0) or
        (fRowCount > 0)) then
    begin
      // retrieve normalized field names and types
      if length(fFieldNames) <> fFieldCount then
        InitFieldNames;
      if fFieldType = nil then
        InitFieldTypes;
      // check range
      if RowLast = 0 then
        RowLast := fRowCount
      else if RowLast > fRowCount then
        RowLast := fRowCount;
      if RowFirst <= 0 then
        RowFirst := 1; // start reading after first Row (Row 0 = Field Names)
      // write schema from col names and types
      W.AddShort('<s:Schema id="RowsetSchema"><s:ElementType name="row" content="eltOnly">');
      for f := 0 to FieldCount - 1 do
      begin
        W.AddShort('<s:AttributeType name="f');
        W.Add(f);
        W.AddShort('" rs:name="');
        W.AddString(fFieldNames[f]);
        W.Add('"');
        W.AddString(FIELDTYPE_TOXML[fFieldType[f].ContentDB]);
        W.Add('/', '>');
      end;
      W.AddShort('</s:ElementType></s:Schema>');
      // write rows data
      o := FieldCount * RowFirst;
      W.AddShort('<rs:data>');
      for r := RowFirst to RowLast do
      begin
        W.AddShorter('<z:row ');
        for f := 0 to FieldCount - 1 do
        begin
          U := GetResults(o);
          if U <> nil then
          begin
            W.Add('f');
            W.Add(f);
            W.Add('=', '"');
            W.AddXmlEscape(U);
            W.Add('"', ' ');
          end;
          inc(o); // points to next value
        end;
        W.Add('/', '>');
      end;
      W.AddShort('</rs:data>');
    end;
    W.AddShorter('</xml>');
    W.FlushFinal;
  finally
    W.Free;
  end;
end;

function TOrmTableAbstract.GetMSRowSetValues: RawUtf8;
var
  MS: TRawByteStringStream;
begin
  MS := TRawByteStringStream.Create;
  try
    GetMSRowSetValues(MS, 1, RowCount);
    result := MS.DataString;
  finally
    MS.Free;
  end;
end;

function TOrmTableAbstract.GetODSDocument(withColumnTypes: boolean): RawByteString;
const
  ODSmimetype: RawUtf8 =
    'application/vnd.oasis.opendocument.spreadsheet';
  ODSContentHeader: RawUtf8 =
    '<office:document-content office:version="1.2"' +
    ' xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0"' +
    ' xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0"' +
    ' xmlns:table="urn:oasis:names:tc:opendocument:xmlns:table:1.0"' +
    ' xmlns:meta="urn:oasis:names:tc:opendocument:xmlns:meta:1.0" >' +
    '<office:body><office:spreadsheet><table:table table:name="Sheet1">' +
    '<table:table-column table:number-columns-repeated="';
  ODSContentFooter =
    '</table:table><table:named-expressions/></office:spreadsheet>' +
    '</office:body></office:document-content>';
  ODSstyles: RawUtf8 = XMLUTF8_HEADER +
    '<office:document-styles xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0"' +
    ' office:version="1.2"></office:document-styles>';
  ODSmeta: RawUtf8 = XMLUTF8_HEADER +
    '<office:document-meta xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0"' +
    ' office:version="1.2"></office:document-meta>';
  ODSsettings: RawUtf8 = XMLUTF8_HEADER +
    '<office:document-settings xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0"' +
    ' office:version="1.2"></office:document-settings>';
  ODSmanifest: RawUtf8 = XMLUTF8_HEADER +
    '<manifest:manifest xmlns:manifest="urn:oasis:names:tc:opendocument:xmlns:manifest:1.0"' +
    ' manifest:version="1.2"><manifest:file-entry manifest:full-path="/"' +
    ' manifest:version="1.2" manifest:media-type="application/vnd.oasis.opendocument.spreadsheet"/>' +
    '<manifest:file-entry manifest:full-path="meta.xml" manifest:media-type="text/xml"/>' +
    '<manifest:file-entry manifest:full-path="settings.xml" manifest:media-type="text/xml"/>' +
    '<manifest:file-entry manifest:full-path="content.xml" manifest:media-type="text/xml"/>' +
    '<manifest:file-entry manifest:full-path="styles.xml" manifest:media-type="text/xml"/>' +
    '</manifest:manifest>';
var
  Zip: TZipWrite;
  Dest: TRawByteStringStream;
  content: RawUtf8;
  W: TTextWriter;
  r, f, o: PtrInt;
begin
  Dest := TRawByteStringStream.Create;
  try
    Zip := TZipWrite.Create(Dest);
    try
      Zip.AddStored('mimetype', pointer(ODSmimetype), length(ODSmimetype));
      Zip.AddDeflated('styles.xml', pointer(ODSstyles), length(ODSstyles));
      Zip.AddDeflated('meta.xml', pointer(ODSmeta), length(ODSmeta));
      Zip.AddDeflated('settings.xml', pointer(ODSsettings), length(ODSsettings));
      Zip.AddDeflated('META-INF/manifest.xml', pointer(ODSmanifest), length(ODSmanifest));
      W := TTextWriter.CreateOwnedStream(65536);
      try
        W.AddShort(XMLUTF8_HEADER);
        W.AddString(ODSContentHeader);
        W.Add(FieldCount);
        W.AddShorter('" />');
        if (self <> nil) and
           ((FieldCount > 0) or
            (fRowCount > 0)) then
        begin
          if withColumnTypes and
             (fFieldType = nil) then
            InitFieldTypes;
          o := 0;
          for r := 0 to fRowCount do
          begin
            W.AddShort('<table:table-row>');
            if withColumnTypes and
               (r > 0) then
            begin
              // export data cells
              for f := 0 to FieldCount - 1 do
              begin
                W.AddShort('<table:table-cell office:value-type="');
                case fFieldType[f].ContentDB of
                  ftInt64,
                  ftDouble,
                  ftCurrency:
                    begin
                      W.AddShort('float" office:value="');
                      W.AddXmlEscape(GetResults(o));
                      W.AddShorter('" />');
                    end;
                  ftDate:
                    begin
                      W.AddShort('date" office:date-value="');
                      W.AddXmlEscape(GetResults(o));
                      W.AddShorter('" />');
                    end;
                else
                  begin
                    //ftUnknown,ftNull,ftUtf8,ftBlob:
                    W.AddShort('string"><text:p>');
                    W.AddXmlEscape(GetResults(o));
                    W.AddShort('</text:p></table:table-cell>');
                  end;
                end;
                inc(o); // points to next value
              end;
            end
            else // export header cells
              for f := 0 to FieldCount - 1 do
              begin
                W.AddShort('<table:table-cell office:value-type="string"><text:p>');
                W.AddXmlEscape(GetResults(o));
                W.AddShort('</text:p></table:table-cell>');
                inc(o);
              end;
            W.AddShort('</table:table-row>');
          end;
        end;
        W.AddShort(ODSContentFooter);
        W.SetText(content);
      finally
        W.Free;
      end;
      Zip.AddDeflated('content.xml', pointer(content), length(content));
    finally
      Zip.Free;
    end;
    result := Dest.DataString;
  finally
    Dest.Free;
  end;
end;

procedure TOrmTableAbstract.GetHtmlTable(Dest: TTextWriter);
var
  r, f, o: PtrInt;
begin
  Dest.AddShort('<table>'#10);
  o := 0;
  for r := 0 to fRowCount do
  begin
    Dest.AddShorter('<tr>');
    for f := 0 to fFieldCount - 1 do
    begin
      if r = 0 then
        Dest.AddShorter('<th>') // header
      else
        Dest.AddShorter('<td>');
      if Assigned(OnExportValue) and
         (r > 0) then
        Dest.AddHtmlEscapeUtf8(OnExportValue(self, r, f, true), hfOutsideAttributes)
      else
        Dest.AddHtmlEscape(GetResults(o), hfOutsideAttributes);
      if r = 0 then
        Dest.AddShorter('</th>')
      else
        Dest.AddShorter('</td>');
      inc(o); // points to next value
    end;
    Dest.AddShorter('</tr>'#10);
  end;
  Dest.AddShorter('</table>');
end;

function TOrmTableAbstract.GetHtmlTable(const Header: RawUtf8): RawUtf8;
var
  W: TTextWriter;
  temp: TTextWriterStackBuffer;
begin
  W := TTextWriter.CreateOwnedStream(temp);
  try
    W.AddShorter('<html>');
    W.AddString(Header);
    W.AddShorter('<body>'#10);
    GetHtmlTable(W);
    W.AddShort(#10'</body></html>');
    W.SetText(result);
  finally
    W.Free;
  end;
end;

function TOrmTableAbstract.GetW(Row, Field: PtrInt): RawUnicode;
begin
  result := Utf8DecodeToRawUnicode(Get(Row, Field), 0);
end;

function TOrmTableAbstract.GetWP(Row, Field: PtrInt; Dest: PWideChar;
  MaxDestChars: cardinal): integer;
var
  P: PUtf8Char;
begin
  P := Get(Row, Field);
  result := Utf8ToWideChar(Dest, P, MaxDestChars, 0) shr 1; // bytes div 2
end;

function TOrmTableAbstract.LengthW(Row, Field: PtrInt): integer;
begin
  // fast calculate unicode length, without any memory allocation
  result := Utf8ToUnicodeLength(Get(Row, Field));
end;

type
  /// a static object is used for smaller recursive stack size and faster code
  // - these special sort implementation do the comparison first by the
  // designed field, and, if the field value is identical, the ID value is
  // used (it will therefore sort by time all identical values)
  // - code generated is very optimized: stack and memory usage, CPU registers
  // prefered, multiplication avoided to calculate memory position from index,
  // hand tuned assembler...
  TUtf8QuickSort = object
  public
    Data: TOrmTableDataArray;
    {$ifndef NOPOINTEROFFSET}
    DataStart: PUtf8Char;
    {$endif NOPOINTEROFFSET}
    Params: TOrmTableSortParams;
    CurrentRow: PtrInt;
    // temp vars (avoid stack usage)
    PID: Int64;
    PP: PUtf8Char;
    OI, OJ, OField2ID, i, j: integer;
    function Get(Offset: PtrInt): PUtf8Char; {$ifdef HASINLINE}inline;{$endif}
    function CompI: integer;                 {$ifdef HASINLINE}inline;{$endif}
    function CompJ: integer;                 {$ifdef HASINLINE}inline;{$endif}
    procedure SetPivot(aP: PtrInt);
    procedure Sort(L, R: integer); // main method
  end;

function TUtf8QuickSort.Get(Offset: PtrInt): PUtf8Char;
begin
  {$ifdef NOPOINTEROFFSET}
  result := Data[Offset];
  {$else}
  result := PUtf8Char(PtrInt(Data[Offset]));
  if result <> nil then
    inc(result, PtrUInt(DataStart));
  {$endif NOPOINTEROFFSET}
end;

procedure TUtf8QuickSort.SetPivot(aP: PtrInt);
begin
  PP := Get(aP);
  SetInt64(Get(aP - OField2ID), PID); // as needed by CompI/CompJ
end;

function TUtf8QuickSort.CompI: integer;
var
  i64: Int64;
begin
  result := Params.Comp(Get(OI), PP);
  if result = 0 then
  begin
    // same value -> sort by ID
    SetInt64(Get(OI - OField2ID), i64{%H-});
    result := ord(i64 > PID) - ord(i64 < PID);
  end;
end;

function TUtf8QuickSort.CompJ: integer;
var
  i64: Int64;
begin
  result := Params.Comp(Get(OJ), PP);
  if result = 0 then
  begin
    // same value -> sort by ID
    SetInt64(Get(OJ - OField2ID), i64{%H-});
    result := ord(i64 > PID) - ord(i64 < PID);
  end;
end;

{$ifdef CPUX86}
procedure ExchgData(P1, P2: PPointer; FieldCount: PtrUInt);
{$ifdef FPC} nostackframe; assembler; {$endif}
asm     // eax=P1 edx=P2 ecx=FieldCount
        push    esi
        push    edi
@1:     mov     esi, [eax]
        mov     edi, [edx]
        mov     [edx], esi
        mov     [eax], edi
        add     eax, 4
        add     edx, 4
        dec     ecx
        jnz     @1
        pop     edi
        pop     esi
end;
{$else}
procedure ExchgData(P1, P2: PIntegerArray; FieldCount: PtrUInt); inline;
var
  p: integer; // Data[] = 32-bit pointer or 32-bit offset
begin
  repeat
    dec(FieldCount);
    p := P1[FieldCount];
    P1[FieldCount] := P2[FieldCount];
    P2[FieldCount] := p;
  until FieldCount = 0;
end;
{$endif CPUX86}

procedure TUtf8QuickSort.Sort(L, R: integer);
var
  P: integer;
begin
  if @Params.Comp <> nil then
    repeat
      i := L;
      OI := i * Params.FieldCount + Params.FieldIndex;
      j := R;
      OJ := j * Params.FieldCount + Params.FieldIndex;
      P := ((i + j) shr 1);
      SetPivot(P * Params.FieldCount + Params.FieldIndex);
      repeat
        if Params.Asc then
        begin
          // ascending order comparison
          while compI < 0 do
          begin
            inc(i);
            inc(OI, Params.FieldCount); // next row
          end;
          while compJ > 0 do
          begin
            dec(j);
            dec(OJ, Params.FieldCount); // previous row
          end;
        end
        else
        begin
          // descending order comparison
          while compI > 0 do
          begin
            inc(i);
            inc(OI, Params.FieldCount);
          end;
          while compJ < 0 do
          begin
            dec(j);
            dec(OJ, Params.FieldCount);
          end;
        end;
        if i <= j then
        begin
          if i <> j then
          begin
            // swap elements
            if CurrentRow = j then // update current row number
              CurrentRow := i
            else if CurrentRow = i then
              CurrentRow := j;
            ExchgData(pointer(@Data[OI - Params.FieldIndex]),
                      pointer(@Data[OJ - Params.FieldIndex]), Params.FieldCount);
          end;
          if P = I then
            SetPivot(OJ)
          else if P = j then
            SetPivot(OI);
          inc(i);
          dec(j);
          inc(OI, Params.FieldCount);
          dec(OJ, Params.FieldCount);
        end
        else
          break;
      until i > j;
      if j - L < R - i then
      begin
        // use recursion only for smaller range
        P := i; // i,j will be overriden during Sort() call -> protect
        if L < j then
          Sort(L, j);
        L := P;
      end
      else
      begin
        P := j;
        if i < R then
          Sort(i, R);
        R := P
      end;
    until L >= R;
end;

procedure TOrmTableAbstract.SortFields(const FieldName: RawUtf8; Asc: boolean;
  PCurrentRow: PInteger; FieldType: TOrmFieldType; CustomCompare: TUtf8Compare);
begin
  SortFields(FieldIndex(FieldName), Asc, PCurrentRow, FieldType, CustomCompare);
end;

procedure TOrmTableAbstract.SortFields(Field: integer; Asc: boolean;
  PCurrentRow: PInteger; FieldType: TOrmFieldType; CustomCompare: TUtf8Compare);
var
  quicksort: TUtf8QuickSort; // fast static object for sorting
begin
  if (FieldCount = 0) or
     (PtrUInt(Field) >= PtrUInt(fFieldCount)) then
    exit;
  if FieldType = oftUnknown then // guess the field type from first row
    FieldType := self.FieldType(Field);
  // store sorting parameters for re-sort in TOrmTableJson.FillFrom()
  if Assigned(CustomCompare) then
    fSortParams.Comp := CustomCompare
  else
  begin
    fSortParams.Comp := OrmFieldTypeComp[FieldType];
    if @fSortParams.Comp = nil then
      exit;
  end;
  fSortParams.FieldType := FieldType;
  fSortParams.FieldCount := FieldCount;
  fSortParams.FieldIndex := Field;
  fSortParams.Asc := Asc;
  // this sort routine is very fast, thanks to the dedicated static object
  quicksort.Params := fSortParams;
  quicksort.Data := fData;
  {$ifndef NOPOINTEROFFSET}
  quicksort.DataStart := fDataStart;
  {$endif NOPOINTEROFFSET}
  if fFieldIndexID < 0 then // consummed as OffsetID = OffsetField - OField2ID
    quicksort.OField2ID := Field // if no ID colum, use first on field collision
  else
    quicksort.OField2ID := Field - fFieldIndexID;
  if PCurrentRow = nil then
    quicksort.CurrentRow := -1
  else
    quicksort.CurrentRow := PCurrentRow^;
  if fRowCount > 1 then // ignore first row = field names -> (1,RowCount)
    quicksort.Sort(1, fRowCount);
  if PCurrentRow <> nil then
    PCurrentRow^ := quicksort.CurrentRow;
end;

function TOrmTableAbstract.SearchFieldSorted(const Value: RawUtf8; FieldIndex: PtrInt;
  CustomCompare: TUtf8Compare): PtrInt;
begin
  result := SearchFieldSorted(pointer(Value), FieldIndex, CustomCompare);
end;

function TOrmTableAbstract.SearchFieldSorted(Value: PUtf8Char; FieldIndex: PtrInt;
  CustomCompare: TUtf8Compare): PtrInt;
var
  L, R, cmp: PtrInt;
begin
  if (self <> nil) and
     (Value <> nil) and
     (fRowCount > 0) and
     (PtrUInt(FieldIndex) < PtrUInt(fFieldCount)) then
  begin
    if not Assigned(CustomCompare) then
      CustomCompare := fSortParams.Comp;
    if Assigned(CustomCompare) then
    begin
      // fast binary search
      L := 1;
      R := fRowCount;
      repeat
        result := (L + R) shr 1;
        cmp := CustomCompare(Results[result * fFieldCount + FieldIndex], Value);
        if cmp = 0 then
          exit;
        if cmp < 0 then
          L := result + 1
        else
          R := result - 1;
      until L > R;
      result := 0;
    end
    else
      result := SearchFieldEquals(Value, FieldIndex);
  end
  else
    result := 0;
end;

type
  TUtf8QuickSortMulti = object
  public
    Data: TOrmTableDataArray;
    {$ifndef NOPOINTEROFFSET}
    DataStart: PUtf8Char;
    {$endif NOPOINTEROFFSET}
    FieldCount: integer;
    IndexMax: integer;
    Index: array[byte] of record
      ndx: integer;
      Comp: TUtf8Compare;
      Desc: boolean;
    end;
    function Get(Offset: PtrInt): PUtf8Char; {$ifdef HASINLINE}inline;{$endif}
    function Compare(A, B: integer): integer;
    procedure Sort(L, R: integer); // main method
  end;

function TUtf8QuickSortMulti.Get(Offset: PtrInt): PUtf8Char;
begin
  {$ifdef NOPOINTEROFFSET}
  result := Data[Offset];
  {$else}
  result := PUtf8Char(PtrInt(Data[Offset]));
  if result <> nil then
    inc(result, PtrUInt(DataStart));
  {$endif NOPOINTEROFFSET}
end;

function TUtf8QuickSortMulti.Compare(A, B: integer): integer;
var
  i: PtrInt;
begin
  result := 0;
  A := A * FieldCount;
  B := B * FieldCount;
  for i := 0 to IndexMax do
    with Index[i] do
    begin
      result := Comp(Get(A + ndx), Get(B + ndx));
      if result <> 0 then
      begin
        if Desc then
          result := -result; // descending order -> inverse comparison
        exit;
      end;
    end;
end;

procedure TUtf8QuickSortMulti.Sort(L, R: integer);
var
  i, j, p: integer;
begin
  if L < R then
    repeat
      i := L;
      j := R;
      p := (L + R) shr 1;
      repeat
        while Compare(i, p) < 0 do
          inc(i);
        while Compare(j, p) > 0 do
          dec(j);
        if i <= j then
        begin
          if i <> j then // swap elements (PUtf8Char or offset)
            ExchgData(pointer(@Data[i * FieldCount]),
                      pointer(@Data[j * FieldCount]), FieldCount);
          if p = i then
            p := j
          else if p = j then
            p := i;
          inc(i);
          dec(j);
        end;
      until i > j;
      if j - L < R - i then
      begin
        // use recursion only for smaller range
        if L < j then
          Sort(L, j);
        L := i;
      end
      else
      begin
        if i < R then
          Sort(i, R);
        R := j;
      end;
    until L >= R;
end;

procedure TOrmTableAbstract.SortFields(const Fields: array of integer;
  const Asc: array of boolean; const CustomCompare: array of TUtf8Compare);
var
  quicksort: TUtf8QuickSortMulti;
  i: PtrInt;
begin
  if (self = nil) or
     (fRowCount <= 1) or
     (FieldCount <= 0) or
     (length(Fields) = 0) then
    exit;
  quicksort.Data := fData;
  {$ifndef NOPOINTEROFFSET}
  quicksort.DataStart := fDataStart;
  {$endif NOPOINTEROFFSET}
  quicksort.FieldCount := FieldCount;
  quicksort.IndexMax := high(Fields);
  if quicksort.IndexMax > high(quicksort.Index) then
    raise EOrmTable.CreateUtf8('%.SortField(): too many Fields[]', [self]);
  for i := 0 to quicksort.IndexMax do
    with quicksort.Index[i] do
    begin
      Comp := nil;
      Desc := false;
      if i <= high(CustomCompare) then
        Comp := CustomCompare[i];
      ndx := Fields[i];
      if ndx < 0 then
      begin
        // Fields[]=-1 for ID column
        ndx := fFieldIndexID;  // use the ID column
        if ndx < 0 then
          exit; // no ID column available
        if @Comp = nil then
          Comp := @Utf8CompareInt64;
        continue;
      end;
      if @Comp = nil then
        Comp := SortCompare(ndx);
      if @Comp = nil then
        exit; // impossible to sort this kind of field (or invalid field index)
    end;
  for i := 0 to high(Asc) do
    if (i <= quicksort.IndexMax) and not Asc[i] then
      quicksort.Index[i].Desc := true;
  quicksort.Sort(1, fRowCount); // ignore first row = field names -> (1,RowCount)
end;

function TOrmTableAbstract.SortCompare(Field: integer): TUtf8Compare;
begin
  result := OrmFieldTypeComp[FieldType(Field)];
end;

function TOrmTableAbstract.Step(SeekFirst: boolean; RowVariant: PVariant): boolean;
begin
  result := false;
  if (self = nil) or
     (fRowCount <= 0) then
    exit; // nothing to iterate over
  if SeekFirst then
    fStepRow := 1
  else if fStepRow >= fRowCount then
    exit
  else
    inc(fStepRow);
  result := true;
  if RowVariant = nil then
    exit;
  if OrmTableRowVariantType = nil then
    OrmTableRowVariantType := SynRegisterCustomVariantType(TOrmTableRowVariant);
  if (PVarData(RowVariant)^.VType = OrmTableRowVariantType.VarType) and
     (POrmTableRowVariantData(RowVariant)^.VTable = self) and
     (POrmTableRowVariantData(RowVariant)^.VRow < 0) then
    exit; // already initialized -> quick exit
  VarClearAndSetType(RowVariant^, OrmTableRowVariantType.VarType);
  POrmTableRowVariantData(RowVariant)^.VTable := self;
  POrmTableRowVariantData(RowVariant)^.VRow := -1; // follow fStepRow
end;

function TOrmTableAbstract.FieldBuffer(
  FieldIndex: PtrInt; Len: PInteger): PUtf8Char;
begin
  if (self = nil) or
     (PtrUInt(FieldIndex) >= PtrUInt(fFieldCount)) then
    raise EOrmTable.CreateUtf8('%.FieldBuffer(%): invalid index',
      [self, FieldIndex]);
  if (fStepRow = 0) or
     (fStepRow > fRowCount) then
    raise EOrmTable.CreateUtf8('%.FieldBuffer(%): no previous Step',
      [self, FieldIndex]);
  inc(FieldIndex, fStepRow * FieldCount);
  result := GetResults(FieldIndex);
  if Len <> nil then
    {$ifdef NOTORMTABLELEN}
    Len^ := StrLen(result);
    {$else}
    Len^ := fLen[FieldIndex];
    {$endif NOTORMTABLELEN}
end;

function TOrmTableAbstract.FieldBuffer(
  const FieldName: RawUtf8; Len: PInteger): PUtf8Char;
var
  i: integer;
begin
  i := FieldIndex(FieldName);
  if i < 0 then
    raise EOrmTable.CreateUtf8('%.FieldBuffer(%): unknown field',
      [self, FieldName]);
  if (fStepRow = 0) or
     (fStepRow > fRowCount) then
    raise EOrmTable.CreateUtf8('%.FieldBuffer(%): no previous Step',
      [self, FieldName]);
  inc(i, fStepRow * FieldCount);
  result := GetResults(i);
  if Len <> nil then
    {$ifdef NOTORMTABLELEN}
    Len^ := StrLen(result);
    {$else}
    Len^ := fLen[i];
    {$endif NOTORMTABLELEN}
end;

function TOrmTableAbstract.FieldAsInteger(FieldIndex: PtrInt): Int64;
begin
  SetInt64(FieldBuffer(FieldIndex), result{%H-});
end;

function TOrmTableAbstract.FieldAsInteger(const FieldName: RawUtf8): Int64;
begin
  SetInt64(FieldBuffer(FieldName), result{%H-});
end;

function TOrmTableAbstract.FieldAsFloat(FieldIndex: PtrInt): TSynExtended;
begin
  result := GetExtended(FieldBuffer(FieldIndex));
end;

function TOrmTableAbstract.FieldAsFloat(const FieldName: RawUtf8): TSynExtended;
begin
  result := GetExtended(FieldBuffer(FieldName));
end;

function TOrmTableAbstract.FieldAsRawUtf8(FieldIndex: PtrInt): RawUtf8;
var
  buf: PUtf8Char;
  buflen: integer;
begin
  buf := FieldBuffer(FieldIndex, @buflen);
  FastSetString(result, buf, buflen);
end;

function TOrmTableAbstract.FieldAsRawUtf8(const FieldName: RawUtf8): RawUtf8;
var
  buf: PUtf8Char;
  buflen: integer;
begin
  buf := FieldBuffer(FieldName, @buflen);
  FastSetString(result, buf, buflen);
end;

function TOrmTableAbstract.FieldAsString(FieldIndex: PtrInt): string;
var
  buf: PUtf8Char;
  buflen: integer;
begin
  buf := FieldBuffer(FieldIndex, @buflen);
  Utf8DecodeToString(buf, buflen, result);
end;

function TOrmTableAbstract.FieldAsString(const FieldName: RawUtf8): string;
var
  buf: PUtf8Char;
  buflen: integer;
begin
  buf := FieldBuffer(FieldName, @buflen);
  Utf8DecodeToString(buf, buflen, result);
end;

function TOrmTableAbstract.Field(FieldIndex: PtrInt): variant;
begin
  if (self = nil) or
     (PtrUInt(FieldIndex) >= PtrUInt(fFieldCount)) then
    raise EOrmTable.CreateUtf8('%.Field(%): invalid index', [self, FieldIndex]);
  if (fStepRow = 0) or
     (fStepRow > fRowCount) then
    raise EOrmTable.CreateUtf8('%.Field(%): no previous Step',
      [self, FieldIndex]);
  GetVariant(fStepRow, FieldIndex, result);
end;

function TOrmTableAbstract.Field(const FieldName: RawUtf8): variant;
var
  i: PtrInt;
begin
  i := FieldIndex(FieldName);
  if i < 0 then
    raise EOrmTable.CreateUtf8('%.Field(%): unknown field', [self, FieldName]);
  result := Field(i);
end;

function TOrmTableAbstract.CalculateFieldLengthMean(var aResult: TIntegerDynArray;
  FromDisplay: boolean): integer;

  procedure CalculateEnumerates(F: integer; P: PRttiEnumType);
  var
    R, i, n, o: integer;
    EnumCounts: array of integer; // slow GetCaption() will be called once
  begin
    if P = nil then
      exit; // no a true enumerate field
    // 1. count of every possible enumerated value into EnumCounts[]
    SetLength(EnumCounts, P^.MaxValue + 1);
    o := fFieldCount + F; // start reading after first Row (= Field Names)
    for R := 1 to fRowCount do
    begin
      n := GetInteger(GetResults(o));
      if n <= P^.MaxValue then // update count of every enumerated value
        inc(EnumCounts[n])
      else // GetCaption(invalid index) displays first one
        inc(EnumCounts[0]);
      inc(o, fFieldCount); // points to next row
    end;
    // 2. update aResult[F] with displayed caption text length
    n := 0;
    for i := 0 to P^.MaxValue do
      if EnumCounts[i] <> 0 then
        inc(n, length(P^.GetCaption(i)) * EnumCounts[i]);
    aResult[F] := n; // store displayed total length
  end;

var
  r, f: PtrInt;
  n, Tot: cardinal;
begin
  SetLength(aResult, FieldCount);
  if FromDisplay and
     (length(fFieldLengthMean) = FieldCount) then
  begin
    MoveFast(fFieldLengthMean[0], aResult[0], FieldCount * SizeOf(integer));
    result := fFieldLengthMeanSum;
    exit;
  end;
  if fRowCount = 0 then
  begin
    // no data: calculate field length from first row (i.e. Field Names)
    for f := 0 to FieldCount - 1 do
      inc(aResult[f], Utf8FirstLineToUtf16Length(GetResults(f)));
    Tot := 1;
  end
  else
  begin
    if fFieldType = nil then
      InitFieldTypes;
    // sum all lengths by field
    for r := 1 to fRowCount do
      for f := 0 to FieldCount - 1 do
        case fFieldType[f].ContentType of
          oftInteger,
          oftBlob,
          oftBlobCustom,
          oftUtf8Custom,
          oftRecord,
          oftRecordVersion,
          oftID,
          oftTID,
          oftSet,
          oftCurrency:
            inc(aResult[f], 8); // naive average length of integer values
        else
          inc(aResult[f],
            Utf8FirstLineToUtf16Length(GetResults(r * fFieldCount + f)));
        end;
    // aResult[] must be recalculated from actual captions, if exists
    for f := 0 to FieldCount - 1 do
      with fFieldType[f] do
        if ContentTypeInfo <> nil then
        case ContentType of
          oftEnumerate:
            CalculateEnumerates(f, ContentTypeInfo);
        end;
    Tot := fRowCount;
  end;
  result := 0;
  for f := 0 to FieldCount - 1 do
  begin
    n := cardinal(aResult[f]) div Tot; // Mean = total/count
    if n = 0 then
      n := 1;  // none should be 0
    aResult[f] := n;
    inc(result, n); // fast calculate mean sum
  end;
end;

function TOrmTableAbstract.FieldLengthMean(Field: PtrInt): cardinal;
begin
  if (self = nil) or
     (PtrUInt(Field) >= PtrUInt(fFieldCount)) or
     (fData = nil) then
    result := 0
  else
  begin
    if fFieldLengthMean = nil then
      // if not already calculated, do it now
      fFieldLengthMeanSum := CalculateFieldLengthMean(fFieldLengthMean);
    result := fFieldLengthMean[Field];
  end;
end;

function TOrmTableAbstract.FieldLengthMeanSum: cardinal;
begin
  if self = nil then
    result := 0
  else
  begin
    if fFieldLengthMean = nil then
      FieldLengthMean(0); // initialize fFieldLengthMean[] and fFieldLengthMeanSum
    result := fFieldLengthMeanSum;
  end;
end;

function TOrmTableAbstract.FieldLengthMax(Field: PtrInt; NeverReturnsZero: boolean): cardinal;
var
  i, o: PtrInt;
  len: cardinal;
begin
  result := 0;
  if (self <> nil) and
     (PtrUInt(Field) < PtrUInt(fFieldCount)) then
  begin
    if fFieldType = nil then
      InitFieldTypes;
    with fFieldType[Field] do
      if ContentSize >= 0 then
        // return already computed value
        result := ContentSize
      else
      begin
        if (ContentTypeInfo <> nil) and
           (ContentType = oftEnumerate) then
        begin
          // compute maximum size from available captions
          for i := 0 to PRttiEnumType(ContentTypeInfo)^.MaxValue do
          begin
            len := length(PRttiEnumType(ContentTypeInfo)^.GetCaption(i));
            if len > result then
              result := len;
          end;
        end
        else
        begin
          // compute by checking all data rows length
          o := fFieldCount + Field;
          for i := 1 to fRowCount do
          begin
            {$ifdef NOTORMTABLELEN}
            len := StrLen(GetResults(o));
            {$else}
            len := fLen[o];
            {$endif NOTORMTABLELEN}
            if len > result then
              result := len;
            inc(o, fFieldCount); // next row
          end;
        end;
        ContentSize := result;
      end;
  end;
  if (result = 0) and NeverReturnsZero then
    result := 1; // minimal not null length
end;

procedure TOrmTableAbstract.SetFieldLengthMean(const Lengths: array of cardinal);
var
  f: PtrInt;
  n: cardinal;
begin
  if (self = nil) or
     (length(Lengths) <> fFieldCount) then
    exit;
  if fFieldLengthMean = nil then // if not already calculated, allocate array
    SetLength(fFieldLengthMean, FieldCount);
  fFieldLengthMeanSum := 0;
  for f := 0 to fFieldCount - 1 do
  begin
    n := Lengths[f];
    if n = 0 then
      n := 1;  // none should be 0
    fFieldLengthMean[f] := n;
    inc(fFieldLengthMeanSum, n); // fast calculate mean sum
  end;
end;

procedure TOrmTableAbstract.FieldLengthMeanIncrease(aField, aIncrease: PtrInt);
begin
  if (self = nil) or
     (PtrUInt(aField) >= PtrUInt(fFieldCount)) then
    exit; // avoid GPF
  if fFieldLengthMean = nil then
    FieldLengthMean(0); // initialize fFieldLengthMean[] and fFieldLengthMeanSum
  inc(fFieldLengthMean[aField], aIncrease);
  inc(fFieldLengthMeanSum, aIncrease);
end;

function TOrmTableAbstract.SearchFieldEquals(const Value: RawUtf8;
  FieldIndex, StartRow: PtrInt; CaseSensitive: boolean): PtrInt;
begin
  result := SearchFieldEquals(pointer(Value), FieldIndex, StartRow, CaseSensitive);
end;

function TOrmTableAbstract.SearchFieldEquals(Value: PUtf8Char;
  FieldIndex, StartRow: PtrInt; CaseSensitive: boolean): PtrInt;
var
  o: PtrInt;
begin
  if (self <> nil) and
     (Value <> nil) and
     (PtrUInt(FieldIndex) < PtrUInt(fFieldCount)) then
  begin
    o := fFieldCount * StartRow + FieldIndex;
    if CaseSensitive then
      for result := StartRow to fRowCount do
        if StrComp(GetResults(o), Value) = 0 then
          exit
        else
          inc(o, fFieldCount)
    else
      for result := StartRow to fRowCount do
        if Utf8IComp(GetResults(o), Value) = 0 then
          exit
        else
          inc(o, fFieldCount);
  end;
  result := 0;
end;

function TOrmTableAbstract.SearchFieldIdemPChar(const Value: RawUtf8;
  FieldIndex, StartRow: PtrInt): PtrInt;
var
  o: PtrInt;
  up: RawUtf8;
begin
  if (self <> nil) and
     (Value <> '') and
     (PtrUInt(FieldIndex) < PtrUInt(fFieldCount)) then
  begin
    UpperCaseCopy(Value, up);
    o := fFieldCount * StartRow + FieldIndex;
    for result := StartRow to fRowCount do
      if IdemPChar(GetResults(o), pointer(up)) then
        exit
      else
        inc(o, fFieldCount);
  end;
  result := 0;
end;

function TOrmTableAbstract.GetVariant(Row, Field: PtrInt): Variant;
begin
  GetVariant(Row, Field, result);
end;

procedure TOrmTableAbstract.GetVariant(Row, Field: PtrInt; var result: variant);
var
  aType: TOrmFieldType;
  info: POrmTableFieldType;
  U: PUtf8Char;
  ULen: integer;
begin
  if Row = 0 then // Field Name
    RawUtf8ToVariant(GetU(0, Field), result)
  else
  begin
    aType := FieldType(Field, info);
    U := Get(Row, Field, ULen);
    ValueVarToVariant(U, ULen, aType, TVarData(result), true, info.ContentTypeInfo);
  end;
end;

function TOrmTableAbstract.GetValue(const aLookupFieldName, aLookupValue,
  aValueFieldName: RawUtf8): variant;
var
  f, r, v: PtrInt;
begin
  SetVariantNull(result);
  f := FieldIndex(aLookupFieldName);
  v := FieldIndex(aValueFieldName);
  if (f < 0) or
     (v < 0) then
    exit;
  r := SearchFieldEquals(aLookupValue, f);
  if r > 0 then
    GetVariant(r, v, result);
end;

function TOrmTableAbstract.GetTimeLog(Row, Field: PtrInt; Expanded: boolean;
  FirstTimeChar: AnsiChar): RawUtf8;
var
  Value: TTimeLogBits;
begin
  SetInt64(Get(Row, Field), {%H-}Value.Value);
  result := Value.Text(Expanded, FirstTimeChar);
end;



{ ************ TOrmTableRowVariant Custom Variant Type }

{ TOrmTableRowVariant }

function TOrmTableRowVariant.IntGet(var Dest: TVarData; const Instance: TVarData;
  Name: PAnsiChar; NameLen: PtrInt; NoException: boolean): boolean;
var
  r, f: integer;
  rv: TOrmTableRowVariantData absolute Instance;
begin
  if rv.VTable = nil then
    raise EOrmTable.CreateUtf8('Invalid %.% call', [self, Name]);
  r := rv.VRow;
  if r < 0 then
  begin
    r := rv.VTable.fStepRow;
    if (r = 0) or
       (r > rv.VTable.fRowCount) then
      raise EOrmTable.CreateUtf8('%.%: no previous Step', [self, Name]);
  end;
  f := rv.VTable.FieldIndex(PUtf8Char(Name));
  result := f >= 0;
  if f >= 0 then
    rv.VTable.GetVariant(r, f, PVariant(@Dest)^);
end;

procedure TOrmTableRowVariant.Cast(var Dest: TVarData; const Source: TVarData);
begin
  CastTo(Dest, Source, VarType);
end;

procedure TOrmTableRowVariant.CastTo(var Dest: TVarData; const Source: TVarData;
  const AVarType: TVarType);
var
  r: integer;
  tmp: variant; // use a temporary TDocVariant for the conversion
begin
  if AVarType = VarType then
  begin
    RaiseCastError;
  end
  else
  begin
    if Source.VType <> VarType then
      RaiseCastError;
    r := TOrmTableRowVariantData(Source).VRow;
    if r < 0 then
      r := TOrmTableRowVariantData(Source).VTable.fStepRow;
    TOrmTableRowVariantData(Source).VTable.ToDocVariant(r, tmp);
    if AVarType = DocVariantVType then
    begin
      VarClear(variant(Dest));
      ZeroFill(@Dest); // avoid GPF
      TDocVariantData(Dest) := TDocVariantData(tmp);
    end
    else
      RawUtf8ToVariant(VariantSaveJson(tmp), Dest, AVarType);
  end;
end;

procedure TOrmTableRowVariant.ToJson(W: TTextWriter; const Value: variant);
var
  r: PtrInt;
  tmp: variant; // write row via a TDocVariant
begin
  r := TOrmTableRowVariantData(Value).VRow;
  if r < 0 then
    r := TOrmTableRowVariantData(Value).VTable.fStepRow;
  TOrmTableRowVariantData(Value).VTable.ToDocVariant(r, tmp);
  W.AddVariant(tmp, twJsonEscape);
end;


{ ************ TOrmLocks and TRestCacheEntry Basic Structures }

{ TOrmLocks }

function TOrmLocks.isLocked(aID: TID): boolean;
begin
  result := (@self <> nil) and
            (Count <> 0) and
            (aID <> 0) and
            Int64ScanExists(pointer(IDs), Count, aID);
end;

function TOrmLocks.Lock(aID: TID): boolean;
var
  P: PInt64;
begin
  if (@self = nil) or
     (aID = 0) then
    // void or full
    result := false
  else
  begin
    P := Int64Scan(pointer(IDs), Count, aID);
    if P <> nil then
      // already locked
      result := false
    else
    begin
      // add to ID[] and Ticks[]
      P := Int64Scan(pointer(IDs), Count, 0);
      if P = nil then
      begin
        // no free entry -> add at the end
        if Count >= length(IDs) then
        begin
          SetLength(IDs, Count + 512);
          SetLength(Ticks64s, Count + 512);
        end;
        IDs[Count] := aID;
        Ticks64s[Count] := GetTickCount64;
        inc(Count);
      end
      else
      begin
        // store at free entry
        P^ := aID;
        Ticks64s[(PtrUInt(P) - PtrUInt(IDs)) shr 3] := GetTickCount64;
      end;
      result := true;
    end;
  end;
end;

procedure TOrmLocks.PurgeOlderThan(MinutesFromNow: cardinal);
var
  LastOK64: Int64;
  i, LastEntry: PtrInt;
begin
  if (@self = nil) or
     (Count = 0) then
    exit; // nothing to purge
  LastOK64 := GetTickCount64 - MinutesFromNow * (1000 * 60); // GetTickCount64() unit is ms
  LastEntry := -1;
  for i := 0 to Count - 1 do
    if IDs[i] <> 0 then
      if Ticks64s[i] < LastOK64 then // too old?
        IDs[i] := 0
      else // 0 frees entry
        LastEntry := i; // refresh last existing entry
  Count := LastEntry + 1; // update count (may decrease list length)
end;

function TOrmLocks.UnLock(aID: TID): boolean;
var
  P: PInt64;
begin
  if (@self = nil) or
     (Count = 0) or
     (aID = 0) then
    result := false
  else
  begin
    P := Int64Scan(pointer(IDs), Count, aID);
    if P = nil then
      result := false
    else
    begin
      P^ := 0; // 0 marks free entry
      if ((PtrUInt(P) - PtrUInt(IDs)) shr 3 >= PtrUInt(Count - 1)) then
        dec(Count); // freed last entry -> decrease list length
      result := true;
    end;
  end;
end;


{ TRestCacheEntry }

procedure TRestCacheEntry.Init;
begin
  Value.InitSpecific(TypeInfo(TRestCacheEntryValueDynArray),
    Values, ptInt64, @Count); // will search/sort by first ID: TID field
  InitializeCriticalSection(Mutex);
end;

procedure TRestCacheEntry.Done;
begin
  DeleteCriticalSection(Mutex);
end;

procedure TRestCacheEntry.Clear;
begin
  EnterCriticalSection(Mutex);
  try
    Value.Clear;
    CacheAll := false;
    CacheEnable := false;
    TimeOutMS := 0;
  finally
    LeaveCriticalSection(Mutex);
  end;
end;

procedure TRestCacheEntry.FlushCacheEntry(Index: integer);
begin
  if cardinal(Index) < cardinal(Count) then
    if CacheAll then
      Value.FastDeleteSorted(Index)
    else
      with Values[Index] do
      begin
        Timestamp512 := 0;
        Json := '';
        Tag := 0;
      end;
end;

procedure TRestCacheEntry.FlushCacheAllEntries;
var
  i: PtrInt;
begin
  if not CacheEnable then
    exit;
  EnterCriticalSection(Mutex);
  try
    if CacheAll then
      Value.Clear
    else
      for i := 0 to Count - 1 do
        with Values[i] do
        begin
          Timestamp512 := 0;
          Json := '';
          Tag := 0;
        end;
  finally
    LeaveCriticalSection(Mutex);
  end;
end;

procedure TRestCacheEntry.SetCache(aID: TID);
var
  Rec: TRestCacheEntryValue;
  i: integer;
begin
  EnterCriticalSection(Mutex);
  try
    CacheEnable := true;
    if not CacheAll and
       not Value.FastLocateSorted(aID, i) and
       (i >= 0) then
    begin
      Rec.ID := aID;
      Rec.Timestamp512 := 0; // indicates no value cache yet
      Rec.Tag := 0;
      Value.FastAddSorted(i, Rec);
    end; // do nothing if aID is already in Values[]
  finally
    LeaveCriticalSection(Mutex);
  end;
end;

procedure TRestCacheEntry.SetJson(aID: TID; const aJson: RawUtf8; aTag: cardinal);
var
  Rec: TRestCacheEntryValue;
  i: integer;
begin
  Rec.ID := aID;
  Rec.Json := aJson;
  Rec.Timestamp512 := GetTickCount64 shr 9;
  Rec.Tag := aTag;
  EnterCriticalSection(Mutex);
  try
    if Value.FastLocateSorted(Rec, i) then
      Values[i] := Rec
    else if CacheAll and
            (i >= 0) then
      Value.FastAddSorted(i, Rec);
  finally
    LeaveCriticalSection(Mutex);
  end;
end;

function TRestCacheEntry.RetrieveJson(aID: TID; var aJson: RawUtf8;
  aTag: PCardinal): boolean;
var
  i: PtrInt;
begin
  result := false;
  EnterCriticalSection(Mutex);
  try
    i := Value.Find(aID); // fast O(log(n)) binary search by first ID field
    if i >= 0 then
      with Values[i] do
        if Timestamp512 <> 0 then // 0 when there is no JSON value cached
          if (TimeOutMS <> 0) and
             ((GetTickCount64 - TimeOutMS) shr 9 > Timestamp512) then
            FlushCacheEntry(i)
          else
          begin
            if aTag <> nil then
              aTag^ := Tag;
            aJson := JSON;
            result := true; // found a non outdated serialized value in cache
          end;
  finally
    LeaveCriticalSection(Mutex);
  end;
end;

function TRestCacheEntry.CachedMemory(FlushedEntriesCount: PInteger): cardinal;
var
  i: PtrInt;
  tix512: cardinal;
begin
  result := 0;
  if CacheEnable and
     (Count > 0) then
  begin
    tix512 := (GetTickCount64 - TimeOutMS) shr 9;
    EnterCriticalSection(Mutex);
    try
      for i := Count - 1 downto 0 do
        with Values[i] do
          if Timestamp512 <> 0 then
            if (TimeOutMS <> 0) and
               (tix512 > Timestamp512) then
            begin
              FlushCacheEntry(i);
              if FlushedEntriesCount <> nil then
                inc(FlushedEntriesCount^);
            end
            else
              inc(result, length(JSON) + (SizeOf(TRestCacheEntryValue) + 16));
    finally
      LeaveCriticalSection(Mutex);
    end;
  end;
end;


{ *********************** Abstract TOrmPropertiesAbstract Parent Class }

constructor TOrmPropertiesAbstract.Create;
begin
  InitializeCriticalSection(fLock);
  fSqlTableRetrieveAllFields := ID_TXT;
  SetLength(fManyFields, MAX_SQLFIELDS);
  SetLength(fSimpleFields, MAX_SQLFIELDS);
  SetLength(fCopiableFields, MAX_SQLFIELDS);
  SetLength(fDynArrayFields, MAX_SQLFIELDS);
  SetLength(fBlobCustomFields, MAX_SQLFIELDS);
  SetLength(fBlobFields, MAX_SQLFIELDS);
  SetLength(SimpleFieldSelect, MAX_SQLFIELDS_INCLUDINGID); // [0] is ID
  MainField[false] := -1;
  MainField[true] := -1;
end;

destructor TOrmPropertiesAbstract.Destroy;
begin
  inherited Destroy;
  DeleteCriticalSection(fLock);
end;

function TOrmPropertiesAbstract.BlobFieldPropFromRawUtf8(const PropName: RawUtf8): PRttiProp;
var
  i: PtrInt;
begin
  if (self <> nil) and
     (PropName <> '') then
    for i := 0 to high(BlobFields) do
      if IdemPropNameU(BlobFields[i].Name, PropName) then
      begin
        result := BlobFields[i].PropInfo;
        exit;
      end;
  result := nil;
end;

function TOrmPropertiesAbstract.BlobFieldPropFromUtf8(PropName: PUtf8Char;
  PropNameLen: integer): PRttiProp;
var
  i: PtrInt;
begin
  if (self <> nil) and
     (PropName <> '') then
    for i := 0 to high(BlobFields) do
      if IdemPropName(BlobFields[i].PropInfo^.Name^, PropName, PropNameLen) then
      begin
        result := BlobFields[i].PropInfo;
        exit;
      end;
  result := nil;
end;

function TOrmPropertiesAbstract.OrmFieldTypeToSql(FieldIndex: integer): RawUtf8;
begin
  if (self = nil) or
    (cardinal(FieldIndex) >= cardinal(Fields.Count)) then
    result := ''
  else if (FieldIndex < length(fCustomCollation)) and
          (fCustomCollation[FieldIndex] <> '') then
    result := ' TEXT COLLATE ' + fCustomCollation[FieldIndex] + ', '
  else
    result := DEFAULT_ORMFIELDTYPETOSQL[Fields.List[FieldIndex].OrmFieldTypeStored];
end;

function TOrmPropertiesAbstract.SetCustomCollation(FieldIndex: integer;
  const aCollationName: RawUtf8): boolean;
begin
  result := (self <> nil) and
            (cardinal(FieldIndex) < cardinal(Fields.Count));
  if result then
  begin
    if Fields.Count > length(fCustomCollation) then
      SetLength(fCustomCollation, Fields.Count);
    fCustomCollation[FieldIndex] := aCollationName;
    with Fields.List[FieldIndex] do
    begin
      Attributes := Attributes - [aBinaryCollation, aUnicodeNoCaseCollation];
      if IdemPropNameU(aCollationName, 'BINARY') then
        Attributes := Attributes + [aBinaryCollation]
      else if IdemPropNameU(aCollationName, 'UNICODENOCASE') then
        Attributes := Attributes + [aUnicodeNoCaseCollation];
    end;
  end;
end;

function TOrmPropertiesAbstract.SetCustomCollation(
  const aFieldName, aCollationName: RawUtf8): boolean;
begin
  result := SetCustomCollation(Fields.IndexByNameOrExcept(aFieldName), aCollationName);
end;

procedure TOrmPropertiesAbstract.SetCustomCollationForAll(aFieldType: TOrmFieldType;
  const aCollationName: RawUtf8);
var
  i: PtrInt;
begin
  if (self <> nil) and
     not (aFieldType in [oftUnknown, oftMany]) then
    for i := 0 to Fields.Count - 1 do
      if Fields.List[i].OrmFieldTypeStored = aFieldType then
        SetCustomCollation(i, aCollationName);
end;

procedure TOrmPropertiesAbstract.SetVariantFieldsDocVariantOptions(
  const Options: TDocVariantOptions);
var
  i: PtrInt;
begin
  if self <> nil then
    for i := 0 to Fields.Count - 1 do
      if (Fields.List[i].OrmFieldType = oftVariant) and
         Fields.List[i].InheritsFrom(TOrmPropInfoRttiVariant) then
        TOrmPropInfoRttiVariant(Fields.List[i]).DocVariantOptions := Options;
end;

function TOrmPropertiesAbstract.SqlAddField(FieldIndex: integer): RawUtf8;
begin
  result := OrmFieldTypeToSql(FieldIndex);
  if result = '' then
    exit; // some fields won't have any column created in the database
  result := FormatUtf8('ALTER TABLE % ADD COLUMN %%',
    [fSqlTableName, Fields.List[FieldIndex].Name, result]);
  if FieldIndex in IsUniqueFieldsBits then
    insert(' UNIQUE', result, length(result) - 1);
  result[length(result) - 1] := ';' // OrmFieldTypeToSql[] ends with ','
end;

procedure TOrmPropertiesAbstract.SetJsonWriterColumnNames(W: TJsonSerializer;
  KnownRowsCount: integer);
var
  i, n, nf: PtrInt;
begin
  // get col count overhead
  if W.withID then
    n := 1
  else
    n := 0;
  // set col names
  nf := Length(W.Fields);
  SetLength(W.ColNames, nf + n);
  if W.withID then
    W.ColNames[0] := ROWID_TXT; // works for both normal and FTS3 records
  for i := 0 to nf - 1 do
  begin
    W.ColNames[n] := Fields.List[W.Fields[i]].Name;
    inc(n);
  end;
  // write or init field names for appropriate JSON Expand
  W.AddColumns(KnownRowsCount);
end;

function TOrmPropertiesAbstract.CreateJsonWriter(Json: TStream; Expand,
  withID: boolean; const aFields: TFieldBits; KnownRowsCount, aBufSize: integer;
  aStackBuffer: PTextWriterStackBuffer): TJsonSerializer;
var
  f: TFieldIndexDynArray;
begin
  FieldBitsToIndex(aFields, f, Fields.Count);
  result := CreateJsonWriter(
    Json, Expand, withID, f, KnownRowsCount, aBufSize, aStackBuffer);
end;

function TOrmPropertiesAbstract.CreateJsonWriter(Json: TStream; Expand,
  withID: boolean; const aFields: TFieldIndexDynArray; KnownRowsCount,
  aBufSize: integer; aStackBuffer: PTextWriterStackBuffer): TJsonSerializer;
begin
  if (self = nil) or
     ((Fields.Count = 0) and not withID) then  // no data
    result := nil
  else
  begin
    result := TJsonSerializer.Create(
      Json, Expand, withID, aFields, aBufSize, aStackBuffer);
    SetJsonWriterColumnNames(result, KnownRowsCount);
  end;
end;

function TOrmPropertiesAbstract.CreateJsonWriter(Json: TStream; Expand: boolean;
  const aFieldsCsv: RawUtf8; KnownRowsCount, aBufSize: integer;
  aStackBuffer: PTextWriterStackBuffer): TJsonSerializer;
var
  withID: boolean;
  bits: TFieldBits;
begin
  if FieldBitsFromCsv(aFieldsCsv, bits, withID) then
    result := CreateJsonWriter(
      Json, Expand, withID, bits, KnownRowsCount, aBufSize, aStackBuffer)
  else
    result := nil;
end;

function TOrmPropertiesAbstract.SaveSimpleFieldsFromJsonArray(var P: PUtf8Char;
  ID: PID; EndOfObject: PAnsiChar; Format: TSaveFieldsAsObject): RawUtf8;
begin
  SaveFieldsFromJsonArray(P, SimpleFieldsBits[ooInsert],
    ID, EndOfObject, Format);
end;

function TOrmPropertiesAbstract.SaveFieldsFromJsonArray(var P: PUtf8Char;
  const Bits: TFieldBits; ID: PID; EndOfObject: PAnsiChar;
  Format: TSaveFieldsAsObject): RawUtf8;
var
  i: PtrInt;
  decoded: TID;
  W: TJsonSerializer;
  Start: PUtf8Char;
  temp: TTextWriterStackBuffer;
begin
  result := '';
  if P = nil then
    exit;
  while (P^ <= ' ') and
        (P^ <> #0) do
    inc(P);
  if P^ <> '[' then
    exit;
  repeat
    inc(P)
  until (P^ > ' ') or
        (P^ = #0);
  if ID = nil then
    decoded := 0
  else
    decoded := ID^;
  if sfoStartWithID in Format then
    decoded := GetInt64(GetJsonField(P, P));
  W := TJsonSerializer.CreateOwnedStream(temp);
  try
    if sfoExtendedJson in Format then
      W.CustomOptions := W.CustomOptions + [twoForceJsonExtended];
    W.Add('{');
    if (decoded <> 0) and
       (sfoPutIDFirst in Format) then
      W.AddPropJsonInt64('ID', decoded);
    for i := 0 to Fields.Count - 1 do
      if GetBitPtr(@Bits, i) then
      begin
        W.AddFieldName(Fields.List[i].Name);
        Start := P;
        P := GotoEndJsonItem(P);
        if (P = nil) or
           not (P^ in [',', ']']) then
          exit;
        W.AddNoJsonEscape(Start, P - Start);
        W.AddComma;
        repeat
          inc(P)
        until (P^ > ' ') or
              (P^ = #0);
      end;
    if sfoEndWithID in Format then
      decoded := GetInt64(GetJsonField(P, P));
    if (decoded <> 0) and
       (sfoPutIDLast in Format) then
      W.AddPropJsonInt64('ID', decoded);
    W.CancelLastComma;
    W.Add('}');
    W.SetText(result);
  finally
    W.Free;
  end;
  if (ID <> nil) and
     (decoded <> 0) then
    ID^ := decoded;
  if EndOfObject <> nil then
    EndOfObject^ := P^;
  if P^ <> #0 then
    repeat
      inc(P)
    until (P^ > ' ') or
          (P^ = #0);
end;

procedure TOrmPropertiesAbstract.SaveBinaryHeader(W: TBufferWriter);
var
  i: PtrInt;
  FieldNames: TRawUtf8DynArray;
begin
  W.Write(fSqlTableName);
  SetLength(FieldNames, Fields.Count);
  for i := 0 to Fields.Count - 1 do
    FieldNames[i] := Fields.List[i].Name;
  W.WriteRawUtf8DynArray(FieldNames, Fields.Count);
  for i := 0 to Fields.Count - 1 do
    W.Write(@Fields.List[i].OrmFieldType, SizeOf(TOrmFieldType));
end;

function TOrmPropertiesAbstract.CheckBinaryHeader(var R: TFastReader): boolean;
var
  n, i: PtrInt;
  FieldNames: TRawUtf8DynArray;
  FieldTypes: array[0..MAX_SQLFIELDS - 1] of TOrmFieldType;
begin
  result := false;
  if (R.VarUtf8 <> fSqlTableName) or
     (R.ReadVarRawUtf8DynArray(FieldNames) <> Fields.Count) then
    exit;
  n := SizeOf(TOrmFieldType) * Fields.Count;
  if not R.CopySafe(@FieldTypes, n) then
    exit;
  for i := 0 to Fields.Count - 1 do
    with Fields.List[i] do
      if (Name <> FieldNames[i]) or
         (OrmFieldType <> FieldTypes[i]) then
        exit;
  result := true;
end;

function TOrmPropertiesAbstract.IsFieldName(PropName: PUtf8Char): boolean;
begin
  result := (PropName <> nil) and
            (IsRowID(PropName) or
             (Fields.IndexByName(PropName) >= 0));
end;

const
  FUNCS: array[0..6] of PAnsiChar = (
    'MAX(',         // 0
    'MIN(',         // 1
    'AVG(',         // 2
    'SUM(',         // 3
    'JSONGET(',     // 4
    'JSONHAS(',     // 5
    nil);

function TOrmPropertiesAbstract.IsFieldNameOrFunction(const PropName: RawUtf8): boolean;
var
  L: integer;
  P, P2: PUtf8Char;
  tmp: shortstring; // no heap allocation
begin
  result := false;
  L := length(PropName);
  if (L = 0) or
     (self = nil) then
    exit;
  P := pointer(PropName);
  if P[L - 1] = ')' then
  begin
    case IdemPPChar(P, @FUNCS) of
      0..3:
        begin
          inc(P, 4);
          P2 := PosChar(P, ')');
        end;
      4..5:
        begin
          inc(P, 8);
          P2 := PosChar(P, ',');
        end;
    else
      exit;
    end;
    if P2 <> nil then
    begin
      SetString(tmp, PAnsiChar(P), P2 - P);
      tmp[ord(tmp[0]) + 1] := #0;
      result := IsFieldName(@tmp[1]);
    end;
  end
  else
    result := IsFieldName(P);
end;

function TOrmPropertiesAbstract.FieldBitsFromBlobField(aBlobField: PRttiProp;
  var Bits: TFieldBits): boolean;
var
  f: PtrInt;
begin
  FillZero(Bits);
  if self <> nil then
    for f := 0 to high(BlobFields) do
      if BlobFields[f].PropInfo = aBlobField then
      begin
        Include(Bits, BlobFields[f].PropertyIndex);
        result := true;
        exit;
      end;
  result := false;
end;

function TOrmPropertiesAbstract.FieldBitsFromCsv(const aFieldsCsv: RawUtf8;
  var Bits: TFieldBits): boolean;
var
  ndx: integer;
  P: PUtf8Char;
  FieldName: ShortString;
begin
  FillZero(Bits);
  result := false;
  if self = nil then
    exit;
  P := pointer(aFieldsCsv);
  while P <> nil do
  begin
    GetNextItemShortString(P, FieldName); // FieldName ends with #0
    ndx := Fields.IndexByName(@FieldName[1]);
    if ndx < 0 then
      exit; // invalid field name
    include(Bits, ndx);
  end;
  result := true;
end;

function TOrmPropertiesAbstract.FieldBitsFromCsv(const aFieldsCsv: RawUtf8;
  var Bits: TFieldBits; out withID: boolean): boolean;
var
  ndx: integer;
  P: PUtf8Char;
  FieldName: ShortString;
begin
  if (aFieldsCsv = '*') and
     (self <> nil) then
  begin
    Bits := SimpleFieldsBits[ooSelect];
    withID := true;
    result := true;
    exit;
  end;
  FillZero(Bits);
  withID := false;
  result := false;
  if self = nil then
    exit;
  P := pointer(aFieldsCsv);
  while P <> nil do
  begin
    GetNextItemShortString(P, FieldName); // FieldName ends with #0
    if IsRowIDShort(FieldName) then
    begin
      withID := true;
      continue;
    end;
    ndx := Fields.IndexByName(@FieldName[1]);
    if ndx < 0 then
      exit; // invalid field name
    include(Bits, ndx);
  end;
  result := true;
end;

function TOrmPropertiesAbstract.FieldBitsFromCsv(const aFieldsCsv: RawUtf8): TFieldBits;
begin
  if not FieldBitsFromCsv(aFieldsCsv, result) then
    FillZero(result);
end;

function TOrmPropertiesAbstract.FieldBitsFromExcludingCsv(
  const aFieldsCsv: RawUtf8; aOccasion: TOrmOccasion): TFieldBits;
var
  excluded: TFieldBits;
begin
  result := SimpleFieldsBits[aOccasion];
  if FieldBitsFromCsv(aFieldsCsv, excluded) then
    result := result - excluded;
end;

function TOrmPropertiesAbstract.FieldBitsFrom(
  const aFields: array of PUtf8Char; var Bits: TFieldBits): boolean;
var
  f, ndx: PtrInt;
begin
  FillZero(Bits);
  result := false;
  if self = nil then
    exit;
  for f := 0 to high(aFields) do
  begin
    ndx := Fields.IndexByName(aFields[f]);
    if ndx < 0 then
      exit; // invalid field name
    include(Bits, ndx);
  end;
  result := true;
end;

function TOrmPropertiesAbstract.FieldBitsFrom(
  const aFields: array of PUtf8Char): TFieldBits;
begin
  if not FieldBitsFrom(aFields, result) then
    FillZero(result);
end;

{$ifndef PUREMORMOT2}
function TOrmPropertiesAbstract.FieldBitsFromRawUtf8(
  const aFields: array of RawUtf8): TFieldBits;
begin
  if not FieldBitsFromRawUtf8(aFields, result) then
    FillZero(result);
end;

function TOrmPropertiesAbstract.FieldBitsFromRawUtf8(
  const aFields: array of RawUtf8; var Bits: TFieldBits): boolean;
begin
  result := FieldBitsFromRawUtf8(aFields, Bits);
end;
{$endif PUREMORMOT2}

procedure TOrmPropertiesAbstract.CsvFromFieldBits(const Prefix: array of const;
  const Bits: TFieldBits; const BitsSuffix: shortstring;
  const Suffix: array of const; out Result: RawUtf8);
var
  f: PtrInt;
  W: TTextWriter;
  temp: TTextWriterStackBuffer;
begin
  W := TTextWriter.CreateOwnedStream(temp);
  try
    W.Add(Prefix, twNone);
    for f := 0 to Fields.Count - 1 do
      if byte(f) in Bits then
      begin
        W.AddString(Fields.List[f].Name);
        if BitsSuffix <> '' then
          W.AddShort(BitsSuffix);
        W.AddComma;
      end;
    W.CancelLastComma;
    W.Add(Suffix, twNone);
    W.SetText(Result);
  finally
    W.Free;
  end;
end;

function TOrmPropertiesAbstract.FieldIndexDynArrayFromRawUtf8(
  const aFields: array of RawUtf8; var Indexes: TFieldIndexDynArray): boolean;
var
  f, ndx: PtrInt;
begin
  result := false;
  if self = nil then
    exit;
  for f := 0 to high(aFields) do
  begin
    ndx := Fields.IndexByName(pointer(aFields[f]));
    if ndx < 0 then
      exit; // invalid field name
    AddFieldIndex(Indexes, ndx);
  end;
  result := true;
end;

function TOrmPropertiesAbstract.FieldIndexDynArrayFromRawUtf8(
  const aFields: array of RawUtf8): TFieldIndexDynArray;
begin
  if not FieldIndexDynArrayFromRawUtf8(aFields, result) then
    result := nil;
end;

function TOrmPropertiesAbstract.FieldIndexDynArrayFromCsv(
  const aFieldsCsv: RawUtf8; var Indexes: TFieldIndexDynArray): boolean;
var
  ndx: integer;
  P: PUtf8Char;
  FieldName: ShortString;
begin
  result := false;
  if self = nil then
    exit;
  P := pointer(aFieldsCsv);
  while P <> nil do
  begin
    GetNextItemShortString(P, FieldName); // FieldName ends with #0
    ndx := Fields.IndexByName(@FieldName[1]);
    if ndx < 0 then
      exit; // invalid field name
    AddFieldIndex(Indexes, ndx);
  end;
  result := true;
end;

function TOrmPropertiesAbstract.FieldIndexDynArrayFromCsv(
  const aFieldsCsv: RawUtf8): TFieldIndexDynArray;
begin
  if not FieldIndexDynArrayFromCsv(aFieldsCsv, result) then
    result := nil;
end;

function TOrmPropertiesAbstract.FieldIndexDynArrayFromBlobField(
  aBlobField: PRttiProp; var Indexes: TFieldIndexDynArray): boolean;
var
  f: PtrInt;
begin
  if self <> nil then
    for f := 0 to high(BlobFields) do
      if BlobFields[f].PropInfo = aBlobField then
      begin
        AddFieldIndex(Indexes, BlobFields[f].PropertyIndex);
        result := true;
        exit;
      end;
  result := false;
end;

function TOrmPropertiesAbstract.AppendFieldName(FieldIndex: integer;
  var Text: RawUtf8; ForceNoRowID: boolean): boolean;
begin
  result := false; // success
  if FieldIndex = VIRTUAL_TABLE_ROWID_COLUMN then
    if ForceNoRowID then
      Text := Text + 'ID'
    else
      Text := Text + 'RowID'
  else if (self = nil) or
          (cardinal(FieldIndex) >= cardinal(Fields.Count)) then
    result := true
  else
    Text := Text + Fields.List[FieldIndex].Name;
end;

function TOrmPropertiesAbstract.MainFieldName(ReturnFirstIfNoUnique: boolean): RawUtf8;
begin
  if (self = nil) or
     (MainField[ReturnFirstIfNoUnique] < 0) then
    result := ''
  else
    result := Fields.List[MainField[ReturnFirstIfNoUnique]].Name;
end;

procedure TOrmPropertiesAbstract.RegisterCustomFixedSizeRecordProperty(
  aTable: TClass; aRecordSize: cardinal; const aName: RawUtf8;
  aPropertyPointer: pointer; aAttributes: TOrmPropInfoAttributes; aFieldWidth: integer;
  aData2Text: TOnSqlPropInfoRecord2Text; aText2Data: TOnSqlPropInfoRecord2Data);
begin
  Fields.Add(TOrmPropInfoRecordFixedSize.Create(aRecordSize, aName, Fields.Count,
    aPropertyPointer, aAttributes, aFieldWidth, aData2Text, aText2Data));
end;

procedure TOrmPropertiesAbstract.RegisterCustomRttiRecordProperty(aTable: TClass;
  aRecordInfo: PRttiInfo; const aName: RawUtf8; aPropertyPointer: pointer;
  aAttributes: TOrmPropInfoAttributes; aFieldWidth: integer;
  aData2Text: TOnSqlPropInfoRecord2Text; aText2Data: TOnSqlPropInfoRecord2Data);
begin
  Fields.Add(TOrmPropInfoRecordRtti.Create(aRecordInfo, aName, Fields.Count,
    aPropertyPointer, aAttributes, aFieldWidth, aData2Text, aText2Data));
end;

procedure TOrmPropertiesAbstract.RegisterCustomPropertyFromRtti(aTable: TClass;
  aTypeInfo: PRttiInfo; const aName: RawUtf8; aPropertyPointer: pointer;
  aAttributes: TOrmPropInfoAttributes; aFieldWidth: integer);
begin
  Fields.Add(TOrmPropInfoCustomJson.Create(aTypeInfo, aName, Fields.Count,
    aPropertyPointer, aAttributes, aFieldWidth));
end;

procedure TOrmPropertiesAbstract.RegisterCustomPropertyFromTypeName(aTable: TClass;
  const aTypeName, aName: RawUtf8; aPropertyPointer: pointer;
  aAttributes: TOrmPropInfoAttributes; aFieldWidth: integer);
begin
  Fields.Add(TOrmPropInfoCustomJson.Create(aTypeName, aName, Fields.Count,
    aPropertyPointer, aAttributes, aFieldWidth));
end;



initialization
  // manual set of OrmFieldTypeComp[] which are not exact TUtf8Compare match
  pointer(@OrmFieldTypeComp[oftAnsiText])   := @AnsiIComp;
  pointer(@OrmFieldTypeComp[oftUtf8Custom]) := @AnsiIComp;
  pointer(@OrmFieldTypeComp[oftObject])     := @StrComp;
  pointer(@OrmFieldTypeComp[oftVariant])    := @StrComp;
  pointer(@OrmFieldTypeComp[oftNullable])   := @StrComp;

finalization
  FreeAndNil(OrmPropInfoRegistration);

end.

