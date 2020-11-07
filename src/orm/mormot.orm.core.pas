/// Object-Relational-Mapping (ORM) Core Types and Classes
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.orm.core;

{
  *****************************************************************************

   Shared Types and Definitions for our RESTful ORM
    - Shared ORM/JSON Fields and Values Definitions
    - JSON Object Decoder and SQL Generation
    - TJSONSerializer Class for TOrm Serialization
    - TOrmPropInfo Classes for Efficient ORM Processing
    - IRestOrm IRestOrmServer IRestOrmClient Definitions
    - TOrm Definition
    - RecordRef Wrapper Definition
    - TOrmTable TOrmTableJSON Definitions
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
   ORM layer for your projects. IRestOrm is the main abstract entry point.

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
  {$ifdef ISDELPHI2010} // Delphi 2009/2010 generics are buggy
  Generics.Collections,
  {$endif ISDELPHI2010}
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
  mormot.core.secure,
  mormot.core.zip, // for ODS export
  mormot.db.core;


{ ************ Shared ORM/JSON Fields and Values Definitions }

const
  /// maximum number of Tables in a Database Model
  // - this constant is used internaly to optimize memory usage in the
  // generated asm code
  // - you should not change it to a value lower than expected in an existing
  // database (e.g. as expected by TOrmAccessRights or such)
  MAX_TABLES = 256;

  /// after how many parameters inlining is not worth it
  INLINED_MAX = 10;

type
  /// generic parent class of all custom Exception types of this unit
  EOrmException = class(ESynException);

  /// used to store bit set for all available Tables in a Database Model
  TOrmFieldTables = set of 0..MAX_TABLES - 1;

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
  // - oftUTF8Text is UTF-8 encoded TEXT, forcing a SYSTEMNOCASE collation,
  // i.e. using UTF8IComp() (TOrm property was declared as RawUTF8,
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
  // - oftObject is a TEXT containing an ObjectToJSON serialization, able to
  // handle published properties of any not TPersistent as JSON object,
  // TStrings or TRawUTF8List as JSON arrays of strings, TCollection or
  // TObjectList as JSON arrays of JSON objects
  // - oftVariant is a TEXT containing a variant value encoded as JSON:
  // string values are stored between quotes, numerical values directly stored,
  // and JSON objects or arrays will be handled as TDocVariant custom types
  // - oftNullable is a INTEGER/DOUBLE/TEXT field containing a NULLable value,
  // stored as a local variant property, identifying TNullableInteger,
  // TNullableBoolean, TNullableFloat, TNullableCurrency,
  // TNullableDateTime, TNullableTimeLog and TNullableUTF8Text types
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
  // - oftUTF8Custom is a custom property, stored as JSON in a TEXT field,
  // defined by overriding TOrm.InternalRegisterCustomProperties
  // virtual method, and adding a TOrmPropInfoCustom instance, e.g. via
  // RegisterCustomPropertyFromTypeName() or RegisterCustomPropertyFromRTTI();
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
  // use the Server time, as retrieved by TSQLDBConnection.ServerTimestamp
  // from mormot.db.sql.pas) - see ComputeFieldsBeforeWrite
  // virtual method of TOrm; note also that only RESTful PUT/POST access
  // will change this field value: manual SQL statements (like
  // 'UPDATE Table SET Column=0') won't change its content; note also that
  // this is automated on Delphi client side, so only within TOrm ORM use
  // (a pure AJAX application should fill such fields explicitely before sending)
  // - oftCreateTime is an INTEGER field containing the TCreateTime time
  // of the record creation; TCreateTime (just like TTimeLog or TModTime)
  // published property can be typecasted to the TTimeLogBits memory structure;
  // the value of this field is automatically updated with the current
  // date and time when the record is created (with external DB, it will
  // use the Server time, as retrieved by TSQLDBConnection.ServerTimestamp
  // from mormot.db.sql.pas) - see ComputeFieldsBeforeWrite
  // virtual method of TOrm; note also that only RESTful PUT/POST access
  // will set this field value: manual SQL statements (like
  // 'INSERT INTO Table ...') won't set its content; note also that this is
  // automated on Delphi client side, so only within TOrm ORM use (a
  // pure AJAX application should fill such fields explicitely before sending)
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
  // (a pure AJAX application should fill such fields explicitely before sending)
  // - oftUnixTime is an INTEGER field for coding a date and time as second-based
  // Unix Time (SQLite3 compatible), which should be defined as TUnixTime=Int64
  // TOrm property
  // - oftUnixMSTime is an INTEGER field for coding a date and time as
  // millisecond-based Unix Time (JavaScript compatible), which should be
  // defined as TUnixMSTime=Int64 TOrm property
  // - WARNING: do not change the order of items below, otherwise some methods
  // (like TOrmProperties.CheckBinaryHeader) may be broken and fail
  TOrmFieldType = (
    oftUnknown, oftAnsiText, oftUTF8Text, oftEnumerate, oftSet, oftInteger,
    oftID, oftRecord, oftBoolean, oftFloat, oftDateTime, oftTimeLog, oftCurrency,
    oftObject, oftVariant, oftNullable, oftBlob, oftBlobDynArray, oftBlobCustom,
    oftUTF8Custom, oftMany, oftModTime, oftCreateTime, oftTID, oftRecordVersion,
    oftSessionUserID, oftDateTimeMS, oftUnixTime, oftUnixMSTime);

  /// set of available SQL field property types
  TOrmFieldTypes = set of TOrmFieldType;

  //// a fixed array of SQL field property types
  TOrmFieldTypeArray = array[0..MAX_SQLFIELDS] of TOrmFieldType;

  /// contains the parameters used for sorting
  // - FieldCount is 0 if was never sorted
  // - used to sort data again after a successfull data update with
  // TOrmTableJSON.FillFrom()
  TOrmTableSortParams = record
    Comp: TUTF8Compare;
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
  // clients ask the server for refresh (see TRestClientURI.UpdateFromServer)
  // - is used also by TOrm.ComputeFieldsBeforeWrite virtual method
  TOrmEvent = (
    oeAdd, oeUpdate, oeDelete, oeUpdateBlob);

  /// used to define the triggered Event types for TOrmHistory
  // - TOrmHistory.History will be used for heArchiveBlob
  // - TOrmHistory.SentDataJSON will be used for other kind of events
  TOrmHistoryEvent = (
    heAdd, heUpdate, heDelete, heArchiveBlob);

  /// used to defined the CRUD associated SQL statement of a command
  // - used e.g. by TOrm.GetJSONValues methods and SimpleFieldsBits[] array
  // (in this case, ooDelete is never used, since deletion is global for all fields)
  // - also used for cache content notification
  TOrmOccasion = (
    ooSelect, ooInsert, ooUpdate, ooDelete);

  /// used to defined a set of CRUD associated SQL statement of a command
  TOrmOccasions = set of TOrmOccasion;


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
  TEXT_DBFIELDS: TSQLDBFieldTypes =
    [ftUTF8, ftDate];

  /// kind of fields which will contain pure TEXT values
  // - independently from the actual storage level
  // - i.e. will match RawUTF8, string, UnicodeString, WideString properties
  RAWTEXT_FIELDS: TOrmFieldTypes =
    [oftAnsiText, oftUTF8Text];

  /// kind of fields which will be stored as TEXT values
  // - i.e. RAWTEXT_FIELDS and TDateTime/TDateTimeMS
  STRING_FIELDS: TOrmFieldTypes =
    [oftAnsiText, oftUTF8Text, oftUTF8Custom, oftDateTime, oftDateTimeMS];

  /// the SQL field property types with their TNullable* equivalency
  // - those types may be stored in a variant published property, e.g.
  // ! property Int: TNullableInteger read fInt write fInt;
  // ! property Txt: TNullableUTF8Text read fTxt write fTxt;
  // ! property Txt: TNullableUTF8Text index 32 read fTxt write fTxt;
  NULLABLE_TYPES =
    [oftInteger, oftBoolean, oftEnumerate, oftFloat, oftCurrency,
     oftDateTime, oftTimeLog, oftUTF8Text];


function ToText(ft: TOrmFieldType): PShortString; overload;
function ToText(e: TOrmEvent): PShortString; overload;
function ToText(he: TOrmHistoryEvent): PShortString; overload;
function ToText(o: TOrmOccasion): PShortString; overload;

/// get the SQL type of this class type
// - returns either oftObject, oftID, oftMany or oftUnknown
function ClassOrmFieldType(info: PRttiInfo): TOrmFieldType;

/// get the SQL type of this type, as managed with the database driver
function GetOrmFieldType(Info: PRttiInfo): TOrmFieldType;

/// similar to AddInt64() function, but for a TIDDynArray
// - some random GPF were identified with AddInt64(TInt64DynArray(Values),...)
// with the Delphi Win64 compiler
procedure AddID(var Values: TIDDynArray; var ValuesCount: integer; Value: TID); overload;

/// similar to AddInt64() function, but for a TIDDynArray
// - some random GPF were identified with AddInt64(TInt64DynArray(Values),...)
// with the Delphi Win64 compiler
procedure AddID(var Values: TIDDynArray; Value: TID); overload;

/// set the TID (=64-bit integer) value from the numerical text stored in P^
// - just a redirection to SetInt64()
procedure SetID(P: PUTF8Char; var result: TID); overload;
  {$ifdef HASINLINE}inline;{$endif}

/// set the TID (=64-bit integer) value from the numerical text stored in U
// - just a redirection to SetInt64()
procedure SetID(const U: RawByteString; var result: TID); overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fill a RawBlob from TEXT-encoded blob data
// - blob data can be encoded as SQLite3 BLOB literals (X'53514C697465' e.g.) or
// or Base-64 encoded content ('\uFFF0base64encodedbinary') or plain TEXT
function BlobToRawBlob(P: PUTF8Char): RawBlob; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fill a RawBlob from TEXT-encoded blob data
// - blob data can be encoded as SQLite3 BLOB literals (X'53514C697465' e.g.) or
// or Base-64 encoded content ('\uFFF0base64encodedbinary') or plain TEXT
procedure BlobToRawBlob(P: PUTF8Char; var result: RawBlob); overload;

/// fill a RawBlob from TEXT-encoded blob data
// - blob data can be encoded as SQLite3 BLOB literals (X'53514C697465' e.g.) or
// or Base-64 encoded content ('\uFFF0base64encodedbinary') or plain TEXT
function BlobToRawBlob(const Blob: RawByteString): RawBlob; overload;

/// create a TBytes from TEXT-encoded blob data
// - blob data can be encoded as SQLite3 BLOB literals (X'53514C697465' e.g.) or
// or Base-64 encoded content ('\uFFF0base64encodedbinary') or plain TEXT
function BlobToBytes(P: PUTF8Char): TBytes;

/// create a memory stream from TEXT-encoded blob data
// - blob data can be encoded as SQLite3 BLOB literals (X'53514C697465' e.g.) or
// or Base-64 encoded content ('\uFFF0base64encodedbinary') or plain TEXT
// - the caller must free the stream instance after use
function BlobToStream(P: PUTF8Char): TStream;

/// creates a TEXT-encoded version of blob data from a RawBlob
// - TEXT will be encoded as SQLite3 BLOB literals (X'53514C697465' e.g.)
function RawBlobToBlob(const RawBlob: RawBlob): RawUTF8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// creates a TEXT-encoded version of blob data from a memory data
// - same as RawBlob, but with direct memory access via a pointer/byte size pair
// - TEXT will be encoded as SQLite3 BLOB literals (X'53514C697465' e.g.)
function RawBlobToBlob(RawBlob: pointer; RawBlobLength: integer): RawUTF8; overload;

/// convert a Base64-encoded content into binary hexadecimal ready for SQL
// - returns e.g. X'53514C697465'
procedure Base64MagicToBlob(Base64: PUTF8Char; var result: RawUTF8);

/// return true if the TEXT is encoded as SQLite3 BLOB literals (X'53514C697465' e.g.)
function isBlobHex(P: PUTF8Char): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// guess the content type of an UTF-8 encoded field value, as used in TOrmTable.Get()
// - if P if nil or 'null', return oftUnknown
// - otherwise, guess its type from its value characters
// - oftBlob is returned if the field is encoded as SQLite3 BLOB literals
// (X'53514C697465' e.g.) or with '\uFFF0' magic code
// - since P is PUTF8Char, string type is oftUTF8Text only
// - oftFloat is returned for any floating point value, even if it was
// declared as oftCurrency type
// - oftInteger is returned for any INTEGER stored value, even if it was declared
// as oftEnumerate, oftSet, oftID, oftTID, oftRecord, oftRecordVersion,
// oftSessionUserID, oftBoolean, oftModTime/oftCreateTime/oftTimeLog or
// oftUnixTime/oftUnixMSTime type
function UTF8ContentType(P: PUTF8Char): TOrmFieldType;

/// guess the number type of an UTF-8 encoded field value, as used in TOrmTable.Get()
// - if P if nil or 'null', return oftUnknown
// - will return oftInteger or oftFloat if the supplied text is a number
// - will return oftUTF8Text for any non numerical content
function UTF8ContentNumberType(P: PUTF8Char): TOrmFieldType;
  {$ifdef HASINLINE}inline;{$endif}

/// special comparison function for sorting ftRecord (TRecordReference/RecordRef)
// UTF-8 encoded values in the SQLite3 database or JSON content
function UTF8CompareRecord(P1, P2: PUTF8Char): PtrInt;

/// special comparison function for sorting oftBoolean
// UTF-8 encoded values in the SQLite3 database or JSON content
function UTF8CompareBoolean(P1, P2: PUTF8Char): PtrInt;

/// special comparison function for sorting oftEnumerate, oftSet or oftID
// UTF-8 encoded values in the SQLite3 database or JSON content
function UTF8CompareUInt32(P1, P2: PUTF8Char): PtrInt;

/// special comparison function for sorting oftInteger, oftTID, oftRecordVersion
// oftTimeLog/oftModTime/oftCreateTime or oftUnixTime/oftUnixMSTime UTF-8 encoded
// values in the SQLite3 database or JSON content
function UTF8CompareInt64(P1, P2: PUTF8Char): PtrInt;

/// special comparison function for sorting oftCurrency
// UTF-8 encoded values in the SQLite3 database or JSON content
function UTF8CompareCurr64(P1, P2: PUTF8Char): PtrInt;

/// special comparison function for sorting oftFloat
// UTF-8 encoded values in the SQLite3 database or JSON content
function UTF8CompareDouble(P1, P2: PUTF8Char): PtrInt;

/// special comparison function for sorting oftDateTime or oftDateTimeMS
// UTF-8 encoded values in the SQLite3 database or JSON content
function UTF8CompareISO8601(P1, P2: PUTF8Char): PtrInt;

type
  /// the available options for TRest.BatchStart() process
  // - boInsertOrIgnore will create 'INSERT OR IGNORE' statements instead of
  // plain 'INSERT' - by now, only the direct mORMotSQLite3 engine supports it
  // - boInsertOrUpdate will create 'INSERT OR REPLACE' statements instead of
  // plain 'INSERT' - by now, only the direct mORMotSQLite3 engine supports it
  // - boExtendedJSON will force the JSON to unquote the column names,
  // e.g. writing col1:...,col2:... instead of "col1":...,"col2"...
  // - boPostNoSimpleFields will avoid to send a TRestBach.Add() with simple
  // fields as "SIMPLE":[val1,val2...] or "SIMPLE@tablename":[val1,val2...],
  // without the field names
  // - boPutNoCacheFlush won't force the associated Cache entry to be flushed:
  // it is up to the caller to ensure cache coherency
  // - boRollbackOnError will raise an exception and Rollback any transaction
  // if any step failed - default if to continue batch processs, but setting
  // a value <> 200/HTTP_SUCCESS in Results[]
  TRestBatchOption = (
    boInsertOrIgnore, boInsertOrReplace, boExtendedJSON,
    boPostNoSimpleFields, boPutNoCacheFlush, boRollbackOnError);

  /// a set of options for TRest.BatchStart() process
  // - TJSONObjectDecoder will use it to compute the corresponding SQL
  TRestBatchOptions = set of TRestBatchOption;



{ ************ JSON Object Decoder and SQL Generation }

type
  /// define how TJSONObjectDecoder.Decode() will handle JSON string values
  TJSONObjectDecoderParams = (
    pInlined, pQuoted, pNonQuoted);

  /// define how TJSONObjectDecoder.FieldTypeApproximation[] is identified
  TJSONObjectDecoderFieldType = (
    ftaNumber, ftaBoolean, ftaString, ftaDate, ftaNull, ftaBlob, ftaObject, ftaArray);

  /// exception class raised by TJSONObjectDecoder
  EJSONObjectDecoder = class(ESynException);

  /// JSON object decoding and SQL generation, in the context of ORM process
  // - this is the main process for marshalling JSON into SQL statements
  // - used e.g. by GetJSONObjectAsSQL() function or ExecuteFromJSON and
  // InternalBatchStop methods
  {$ifdef USERECORDWITHMETHODS}
  TJSONObjectDecoder = record
  {$else}
  TJSONObjectDecoder = object
  {$endif USERECORDWITHMETHODS}
  public
    /// contains the decoded field names
    FieldNames: array[0..MAX_SQLFIELDS - 1] of RawUTF8;
    /// contains the decoded field values
    FieldValues: array[0..MAX_SQLFIELDS - 1] of RawUTF8;
    /// Decode() will set each field type approximation
    // - will recognize also JSON_BASE64_MAGIC/JSON_SQLDATE_MAGIC prefix
    FieldTypeApproximation: array[0..MAX_SQLFIELDS - 1] of TJSONObjectDecoderFieldType;
    /// number of fields decoded in FieldNames[] and FieldValues[]
    FieldCount: integer;
    /// set to TRUE if parameters are to be :(...): inlined
    InlinedParams: TJSONObjectDecoderParams;
    /// internal pointer over field names to be used after Decode() call
    // - either FieldNames, either Fields[] array as defined in Decode(), or
    // external names as set by TRestStorageExternal.JSONDecodedPrepareToSQL
    DecodedFieldNames: PRawUTF8Array;
    /// the ID=.. value as sent within the JSON object supplied to Decode()
    DecodedRowID: TID;
    /// internal pointer over field types to be used after Decode() call
    // - to create 'INSERT INTO ... SELECT UNNEST(...)' or 'UPDATE ... FROM
    // SELECT UNNEST(...)' statements for very efficient bulk writes in a
    // PostgreSQL database
    // - as set by TRestStorageExternal.JSONDecodedPrepareToSQL when
    // cPostgreBulkArray flag is detected - for mormot.db.sql.postgres.pas
    DecodedFieldTypesToUnnest: PSQLDBFieldTypeArray;
    /// decode the JSON object fields into FieldNames[] and FieldValues[]
    // - if Fields=nil, P should be a true JSON object, i.e. defined
    // as "COL1"="VAL1" pairs, stopping at '}' or ']'; otherwise, Fields[]
    // contains column names and expects a JSON array as "VAL1","VAL2".. in P
    // - P should be after the initial '{' or '[' character, i.e. at first field
    // - P returns the next object start or nil on unexpected end of input
    // - P^ buffer will let the JSON be decoded in-place, so consider using
    // the overloaded Decode(JSON: RawUTF8; ...) method
    // - FieldValues[] strings will be quoted and/or inlined depending on Params
    // - if RowID is set, a RowID column will be added within the returned content
    procedure Decode(var P: PUTF8Char; const Fields: TRawUTF8DynArray;
      Params: TJSONObjectDecoderParams; const RowID: TID = 0;
      ReplaceRowIDWithID: boolean = false); overload;
    /// decode the JSON object fields into FieldNames[] and FieldValues[]
    // - overloaded method expecting a RawUTF8 buffer, making a private copy
    // of the JSON content to avoid unexpected in-place modification, then
    // calling Decode(P: PUTF8Char) to perform the process
    procedure Decode(const JSON: RawUTF8; const Fields: TRawUTF8DynArray;
      Params: TJSONObjectDecoderParams; const RowID: TID = 0;
      ReplaceRowIDWithID: boolean = false); overload;
    /// can be used after Decode() to add a new field in FieldNames/FieldValues
    // - so that EncodeAsSQL() will include this field in the generated SQL
    // - caller should ensure that the FieldName is not already defined in
    // FieldNames[] (e.g. when the TRecordVersion field is forced)
    // - the caller should ensure that the supplied FieldValue will match
    // the quoting/inlining expectations of Decode(TJSONObjectDecoderParams) -
    // e.g. that string values are quoted if needed
    procedure AddFieldValue(const FieldName, FieldValue: RawUTF8;
      FieldType: TJSONObjectDecoderFieldType);
    /// encode as a SQL-ready INSERT or UPDATE statement
    // - after a successfull call to Decode()
    // - escape SQL strings, according to the official SQLite3 documentation
    // (i.e. ' inside a string is stored as '')
    // - if InlinedParams was TRUE, it will create prepared parameters like
    // 'COL1=:("VAL1"):, COL2=:(VAL2):'
    // - called by GetJSONObjectAsSQL() function or TRestStorageExternal
    function EncodeAsSQL(Update: boolean): RawUTF8;
    /// encode as a SQL-ready INSERT or UPDATE statement with ? as values
    // - after a successfull call to Decode()
    // - FieldValues[] content will be ignored
    // - Occasion can be only ooInsert or ooUpdate
    // - for ooUpdate, will create UPDATE ... SET ... where UpdateIDFieldName=?
    // - you can specify some options, e.g. boInsertOrIgnore for ooInsert
    function EncodeAsSQLPrepared(const TableName: RawUTF8; Occasion: TOrmOccasion;
      const UpdateIDFieldName: RawUTF8; BatchOptions: TRestBatchOptions): RawUTF8;
    /// encode the FieldNames/FieldValues[] as a JSON object
    procedure EncodeAsJSON(out result: RawUTF8);
    /// set the specified array to the fields names
    // - after a successfull call to Decode()
    procedure AssignFieldNamesTo(var Fields: TRawUTF8DynArray);
    /// returns TRUE if the specified array match the decoded fields names
    // - after a successfull call to Decode()
    function SameFieldNames(const Fields: TRawUTF8DynArray): boolean;
    /// search for a field name in the current identified FieldNames[]
    function FindFieldName(const FieldName: RawUTF8): PtrInt;
  end;


/// decode JSON fields object into an UTF-8 encoded SQL-ready statement
// - this function decodes in the P^ buffer memory itself (no memory allocation
// or copy), for faster process - so take care that it is an unique string
// - P should be after the initial '{' or '[' character, i.e. at first field
// - P contains the next object start or nil on unexpected end of input
// - if Fields is void, expects expanded "COL1"="VAL1" pairs in P^, stopping at '}' or ']'
// - otherwise, Fields[] contains the column names and expects "VAL1","VAL2".. in P^
// - returns 'COL1="VAL1", COL2=VAL2' if UPDATE is true (UPDATE SET format)
// - returns '(COL1, COL2) VALUES ("VAL1", VAL2)' otherwise (INSERT format)
// - escape SQL strings, according to the official SQLite3 documentation
// (i.e. ' inside a string is stored as '')
// - if InlinedParams is set, will create prepared parameters like
// 'COL1=:("VAL1"):, COL2=:(VAL2):'
// - if RowID is set, a RowID column will be added within the returned content
function GetJSONObjectAsSQL(var P: PUTF8Char; const Fields: TRawUTF8DynArray;
  Update, InlinedParams: boolean; RowID: TID = 0;
  ReplaceRowIDWithID: boolean = false): RawUTF8; overload;

/// decode JSON fields object into an UTF-8 encoded SQL-ready statement
// - is used e.g. by TRestServerDB.EngineAdd/EngineUpdate methods
// - expect a regular JSON expanded object as "COL1"="VAL1",...} pairs
// - make its own temporary copy of JSON data before calling GetJSONObjectAsSQL() above
// - returns 'COL1="VAL1", COL2=VAL2' if UPDATE is true (UPDATE SET format)
// - returns '(COL1, COL2) VALUES ("VAL1", VAL2)' otherwise (INSERT format)
// - if InlinedParams is set, will create prepared parameters like 'COL2=:(VAL2):'
// - if RowID is set, a RowID column will be added within the returned content
function GetJSONObjectAsSQL(const JSON: RawUTF8; Update, InlinedParams: boolean;
  RowID: TID = 0; ReplaceRowIDWithID: boolean = false): RawUTF8; overload;

/// get the FIRST field value of the FIRST row, from a JSON content
// - e.g. useful to get an ID without converting a JSON content into a TOrmTableJSON
function UnJSONFirstField(var P: PUTF8Char): RawUTF8;

/// returns TRUE if the JSON content is in expanded format
// - i.e. as plain [{"ID":10,"FirstName":"John","LastName":"Smith"}...]
// - i.e. not as '{"fieldCount":3,"values":["ID","FirstName","LastName",...']}
function IsNotAjaxJSON(P: PUTF8Char): boolean;

/// efficient retrieval of the number of rows in non-expanded layout
// - search for "rowCount": at the end of the JSON buffer
function NotExpandedBufferRowCountPos(P, PEnd: PUTF8Char): PUTF8Char;

/// retrieve a JSON '{"Name":Value,....}' object
// - P is nil in return in case of an invalid object
// - returns the UTF-8 encoded JSON object, including first '{' and last '}'
// - if ExtractID is set, it will contain the "ID":203 field value, and this
// field won't be included in the resulting UTF-8 encoded JSON object unless
// KeepIDField is true
// - this function expects this "ID" property to be the FIRST in the
// "Name":Value pairs, as generated by TOrm.GetJSONValues(W)
function JSONGetObject(var P: PUTF8Char; ExtractID: PID;
  var EndOfObject: AnsiChar; KeepIDField: boolean): RawUTF8;

/// retrieve the ID/RowID field of a JSON object
// - this function expects this "ID" property to be the FIRST in the
// "Name":Value pairs, as generated by TOrm.GetJSONValues(W)
// - returns TRUE if a ID/RowID>0 has been found, and set ID with the value
function JSONGetID(P: PUTF8Char; out ID: TID): boolean;

/// low-level function used to convert a JSON Value into a variant,
// according to the property type
// - for oftObject, oftVariant, oftBlobDynArray and oftUTF8Custom, the
// JSON buffer may be an array or an object, so createValueTempCopy can
// create a temporary copy before parsing it in-place, to preserve the buffer
// - oftUnknown and oftMany will set a varEmpty (Unassigned) value
// - typeInfo may be used for oftBlobDynArray conversion to a TDocVariant array
procedure ValueVarToVariant(Value: PUTF8Char; ValueLen: integer;
  fieldType: TOrmFieldType; var result: TVarData; createValueTempCopy: boolean;
  typeInfo: PRttiInfo; options: TDocVariantOptions = JSON_OPTIONS_FAST);


{ ************ TJSONSerializer Class for TOrm Serialization }

type
  /// several options to customize how TOrm will be serialized
  // - e.g. if properties storing JSON should be serialized as an object, and not
  // escaped as a string (which is the default, matching ORM column storage)
  // - if an additional "ID_str":"12345" field should be added to the standard
  // "ID":12345 field, which may exceed 53-bit integer precision of JavsCript
  TJSONSerializerOrmOption = (
    jwoAsJsonNotAsString, jwoID_str);

  /// options to customize how TOrm will be written by TJSONSerializer
  TJSONSerializerOrmOptions = set of TJSONSerializerOrmOption;

  /// simple writer to a Stream, specialized for writing TOrm as JSON
  // - in respect to the standard TJSONWriter as defined in mormot.db.core,
  // this class has some options dedicated to our TOrm serialization
  TJSONSerializer = class(TJSONWriter)
  protected
    fOrmOptions: TJSONSerializerOrmOptions;
    procedure SetOrmOptions(Value: TJSONSerializerOrmOptions);
  public
    /// customize TOrm.GetJSONValues serialization process
    // - jwoAsJsonNotAsString will force TOrm.GetJSONValues to serialize
    // nested property instances as a JSON object/array, not a JSON string:
    // i.e. root/table/id REST will be ready-to-be-consumed from AJAX clients
    // (e.g. TOrmPropInfoRTTIObject.GetJSONValues as a JSON object, and
    // TOrmPropInfoRTTIDynArray.GetJSONValues as a JSON array)
    // - jwoID_str will add an "ID_str":"12345" property to the default
    // "ID":12345 field to circumvent JavaScript's limitation of 53-bit for
    // integer numbers, which is easily reached with our 64-bit TID values, e.g.
    // if TSynUniqueIdentifier are used to generate the IDs: AJAX clients should
    // better use this "ID_str" string value to identify each record, and ignore
    // the "id" fields
    property OrmOptions: TJSONSerializerOrmOptions
      read fOrmOptions write SetOrmOptions;
  end;


{ ************ TOrmPropInfo Classes for Efficient ORM Processing }

type
  /// ORM attributes for a TOrmPropInfo definition
  TOrmPropInfoAttribute = (
    aIsUnique, aAuxiliaryRTreeField, aBinaryCollation);

  /// set of ORM attributes for a TOrmPropInfo definition
  TOrmPropInfoAttributes = set of TOrmPropInfoAttribute;

  /// abstract parent class to store information about a published property
  // - property information could be retrieved from RTTI (TOrmPropInfoRTTI*),
  // or be defined by code (TOrmPropInfoCustom derivated classes) when RTTI
  // is not available
  TOrmPropInfo = class
  protected
    fName: RawUTF8;
    fNameUnflattened: RawUTF8;
    fOrmFieldType: TOrmFieldType;
    fOrmFieldTypeStored: TOrmFieldType;
    fSQLDBFieldType: TSQLDBFieldType;
    fAttributes: TOrmPropInfoAttributes;
    fFieldWidth: integer;
    fPropertyIndex: integer;
    fFromRTTI: boolean;
    function GetNameDisplay: string; virtual;
    /// those two protected methods allow custom storage of binary content as text
    // - default implementation is to use hexa (ToSQL=true) or Base64 encodings
    procedure BinaryToText(var Value: RawUTF8; ToSQL: boolean;
      wasSQLString: PBoolean); virtual;
    procedure TextToBinary(Value: PUTF8Char; var result: RawByteString); virtual;
    function GetOrmFieldTypeName: PShortString;
    function GetSQLFieldRTTITypeName: RawUTF8; virtual;
    // overriden method shall use direct copy of the low-level binary content,
    // to be faster than a DestInfo.SetValue(Dest,GetValue(Source)) call
    procedure CopySameClassProp(Source: TObject; DestInfo: TOrmPropInfo;
      Dest: TObject); virtual;
  public
    /// initialize the internal fields
    // - should not be called directly, but with dedicated class methods like
    // class function TOrmPropInfoRTTI.CreateFrom() or overridden constructors
    constructor Create(const aName: RawUTF8; aOrmFieldType: TOrmFieldType;
      aAttributes: TOrmPropInfoAttributes; aFieldWidth, aPropertyIndex: integer);
      reintroduce; virtual;
    /// the property definition Name
    property Name: RawUTF8 read fName;
    /// the property definition Name, afer un-camelcase and translation
    property NameDisplay: string read GetNameDisplay;
    /// the property definition Name, with full path name if has been flattened
    // - if the property has been flattened (for a TOrmPropInfoRTTI), the real
    // full nested class will be returned, e.g. 'Address.Country.Iso' for
    // the 'Address_Country' flattened property name
    property NameUnflattened: RawUTF8 read fNameUnflattened;
    /// the property index in the RTTI
    property PropertyIndex: integer read fPropertyIndex;
    /// the corresponding column type, as managed by the ORM layer
    property OrmFieldType: TOrmFieldType read fOrmFieldType;
    /// the corresponding column type, as stored by the ORM layer
    // - match OrmFieldType, unless for OrmFieldType=oftNullable, in which this
    // field will contain the simple type eventually stored in the database
    property OrmFieldTypeStored: TOrmFieldType read fOrmFieldTypeStored;
    /// the corresponding column type name, as managed by the ORM layer and
    // retrieved by the RTTI
    // - returns e.g. 'oftTimeLog'
    property OrmFieldTypeName: PShortString read GetOrmFieldTypeName;
    /// the type name, as defined in the RTTI
    // - returns e.g. 'RawUTF8'
    // - will return the TOrmPropInfo class name if it is not a TOrmPropInfoRTTI
    property SQLFieldRTTITypeName: RawUTF8 read GetSQLFieldRTTITypeName;
    /// the corresponding column type, as managed for abstract database access
    // - TNullable* fields will report here the corresponding simple DB type,
    // e.g. ftInt64 for TNullableInteger (following OrmFieldTypeStored value)
    property SQLDBFieldType: TSQLDBFieldType read fSQLDBFieldType;
    /// the corresponding column type name, as managed for abstract database access
    function SQLDBFieldTypeName: PShortString;
    /// the ORM attributes of this property
    // - contains aIsUnique e.g for TOrm published properties marked as
    // ! property MyProperty: RawUTF8 stored AS_UNIQUE;
    // (i.e. "stored false")
    property Attributes: TOrmPropInfoAttributes read fAttributes write fAttributes;
    /// the optional width of this field, in external databases
    // - is set e.g. by index attribute of TOrm published properties as
    // ! property MyProperty: RawUTF8 index 10;
    property FieldWidth: integer read fFieldWidth;
  public
    /// convert UTF-8 encoded text into the property value
    // - setter method (write Set*) is called if available
    // - if no setter exists (no write declaration), the getted field address is used
    // - handle UTF-8 SQL to Delphi values conversion
    // - expect BLOB fields encoded as SQlite3 BLOB literals ("x'01234'" e.g.)
    // or base-64 encoded stream for JSON ("\uFFF0base64encodedbinary") - i.e.
    // both format supported by BlobToRawBlob() function
    // - handle TPersistent, TCollection, TRawUTF8List or TStrings with JSONToObject
    // - note that the supplied Value buffer won't be modified by this method:
    // overriden implementation should create their own temporary copy
    procedure SetValue(Instance: TObject; Value: PUTF8Char; wasString: boolean);
      virtual; abstract;
    /// convert UTF-8 encoded text into the property value
    // - just a wrapper around SetValue(...,pointer(Value),...) which may be
    // optimized for overriden methods
    procedure SetValueVar(Instance: TObject; const Value: RawUTF8;
      wasString: boolean); virtual;
    /// convert the property value into an UTF-8 encoded text
    // - if ToSQL is true, result is on SQL form (false->'0' e.g.)
    // - if ToSQL is false, result is on JSON form (false->'false' e.g.)
    // - BLOB field returns SQlite3 BLOB literals ("x'01234'" e.g.) if ToSQL is
    // true, or base-64 encoded stream for JSON ("\uFFF0base64encodedbinary")
    // - getter method (read Get*) is called if available
    // - handle Delphi values into UTF-8 SQL conversion
    // - oftBlobDynArray, oftBlobCustom or oftBlobRecord are returned as BLOB
    // litterals ("X'53514C697465'") if ToSQL is true, or base-64 encoded stream
    // for JSON ("\uFFF0base64encodedbinary")
    // - handle TPersistent, TCollection, TRawUTF8List or TStrings with ObjectToJSON
    function GetValue(Instance: TObject; ToSQL: boolean; wasSQLString: PBoolean = nil): RawUTF8;
      {$ifdef HASINLINE}inline;{$endif}
    /// convert the property value into an UTF-8 encoded text
    // - this method is the same as GetValue(), but avoid assigning the result
    // string variable (some speed up on multi-core CPUs, since avoid a CPU LOCK)
    // - this virtual method is the one to be overridden by the implementing classes
    procedure GetValueVar(Instance: TObject; ToSQL: boolean; var result: RawUTF8;
      wasSQLString: PBoolean); virtual; abstract;
    /// normalize the content of Value, so that GetValue(Object,true) should return the
    // same content (true for ToSQL format)
    procedure NormalizeValue(var Value: RawUTF8); virtual; abstract;
    /// retrieve a field value into a TSQLVar value
    // - the temp RawByteString is used as a temporary storage for TEXT or BLOB
    // and should be available during all access to the TSQLVar fields
    procedure GetFieldSQLVar(Instance: TObject; var aValue: TSQLVar;
      var temp: RawByteString); virtual;
    /// set a field value from a TSQLVar value
    function SetFieldSQLVar(Instance: TObject; const aValue: TSQLVar): boolean; virtual;
    /// returns TRUE if value is 0 or ''
    function IsValueVoid(Instance: TObject): boolean;
    /// append the property value into a binary buffer
    procedure GetBinary(Instance: TObject; W: TBufferWriter); virtual; abstract;
    /// read the property value from a binary buffer
    // - PEnd should point to the end of the P input buffer, to avoid any overflow
    // - returns next char in input buffer on success, or nil in case of invalid
    // content supplied e.g.
    function SetBinary(Instance: TObject; P, PEnd: PAnsiChar): PAnsiChar;
      virtual; abstract;
    /// copy a property value from one instance to another
    // - both objects should have the same exact property
    procedure CopyValue(Source, Dest: TObject); virtual;
    /// copy a value from one instance to another property instance
    // - if the property has been flattened (for a TOrmPropInfoRTTI), the real
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
    // - if CaseInsensitive is TRUE, will apply NormToUpper[] 8 bits uppercase,
    // handling RawUTF8 properties just like the SYSTEMNOCASE collation
    // - this method should match the case-sensitivity of GetHash()
    // - this default implementation will call GetValueVar() for slow comparison
    function CompareValue(Item1, Item2: TObject; CaseInsensitive: boolean): PtrInt; virtual;
    /// retrieve an unsigned 32-bit hash of the corresponding property
    // - not all kind of properties are handled: only main types
    // - if CaseInsensitive is TRUE, will apply NormToUpper[] 8 bits uppercase,
    // handling RawUTF8 properties just like the SYSTEMNOCASE collation
    // - note that this method can return a hash value of 0
    // - this method should match the case-sensitivity of CompareValue()
    // - this default implementation will call GetValueVar() for slow computation
    function GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal; virtual;
    /// add the JSON content corresponding to the given property
    // - this default implementation will call safe but slow GetValueVar() method
    procedure GetJSONValues(Instance: TObject; W: TJSONSerializer); virtual;
    /// returns an untyped pointer to the field property memory in a given instance
    function GetFieldAddr(Instance: TObject): pointer; virtual; abstract;
  end;

  /// class-reference type (metaclass) of a TOrmPropInfo information
  TOrmPropInfoClass = class of TOrmPropInfo;

  /// define how the published properties RTTI is to be interpreted
  // - i.e. how TOrmPropInfoList.Create() and TOrmPropInfoRTTI.CreateFrom()
  // will handle the incoming RTTI
  TOrmPropInfoListOptions = set of (
    pilRaiseEOrmExceptionIfNotHandled,
    pilAllowIDFields,
    pilSubClassesFlattening,
    pilIgnoreIfGetter,
    pilSingleHierarchyLevel,
    pilAuxiliaryFields);

  /// parent information about a published property retrieved from RTTI
  TOrmPropInfoRTTI = class(TOrmPropInfo)
  protected
    fPropInfo: PRttiProp; // equals fPropRttiProp.Prop
    fPropType: PRttiInfo;
    fPropRtti: TRttiJson; // equals fPropRttiProp.Value
    fPropRttiProp: PRttiCustomProp; // may be nil
    fFlattenedProps: PRttiPropDynArray;
    fGetterIsFieldPropOffset: PtrUInt;
    fInPlaceCopySameClassPropOffset: PtrUInt;
    function GetSQLFieldRTTITypeName: RawUTF8; override;
  public
    /// this meta-constructor will create an instance of the exact descendant
    // of the specified property RTTI
    // - it will raise an EOrmException in case of an unhandled type
    class function CreateFrom(aPropInfo: PRttiProp; aPropIndex: integer;
      aOptions: TOrmPropInfoListOptions;
      const aFlattenedProps: PRttiPropDynArray): TOrmPropInfo;
    /// register this class corresponding to the RTTI TypeInfo() pointer
    // - could be used e.g. to define custom serialization and process of
    // any custom type
    class procedure RegisterTypeInfo(aTypeInfo: PRttiInfo);
    /// initialize the internal fields
    // - should not be called directly, but with dedicated class methods like
    // class function CreateFrom()
    constructor Create(aPropInfo: PRttiProp; aPropIndex: integer;
      aOrmFieldType: TOrmFieldType;
      aOptions: TOrmPropInfoListOptions); reintroduce; virtual;
    /// retrieve the property value into a Variant
    // - will set the Variant type to the best matching kind according to the
    // OrmFieldType type
    // - BLOB field returns SQlite3 BLOB textual literals ("x'01234'" e.g.)
    // - dynamic array field is returned as a variant array
    procedure GetVariant(Instance: TObject; var Dest: Variant); override;
    /// retrieve the property field offset from RTTI
    function GetFieldAddr(Instance: TObject): pointer; override;
    /// get the absolute property field offset from RTTI
    property GetterIsFieldPropOffset: PtrUInt
      read fGetterIsFieldPropOffset;
    /// for pilSubClassesFlattening properties, compute the actual instance
    // containing the property value
    // - if the property was not flattened, return the instance
    function Flattened(Instance: TObject): TObject;
    /// corresponding RTTI information
    property PropInfo: PRttiProp read fPropInfo;
    /// for pilSubClassesFlattening properties, the parents RTTI
    property FlattenedPropInfo: PRttiPropDynArray read fFlattenedProps;
    /// corresponding type information, as retrieved from PropInfo RTTI
    property PropType: PRttiInfo read fPropType;
    /// corresponding JSON-aware type information, as retrieved from PropInfo RTTI
    property PropRtti: TRttiJson read fPropRtti;
  end;

  /// class-reference type (metaclass) of a TOrmPropInfoRTTI information
  TOrmPropInfoRTTIClass = class of TOrmPropInfoRTTI;

  TOrmPropInfoRTTIObjArray = array of TOrmPropInfoRTTI;

  /// information about an ordinal Int32 published property
  TOrmPropInfoRTTIInt32 = class(TOrmPropInfoRTTI)
  protected
    fUnsigned: boolean;
    procedure CopySameClassProp(Source: TObject; DestInfo: TOrmPropInfo;
      Dest: TObject); override;
  public
    constructor Create(aPropInfo: PRttiProp; aPropIndex: integer;
      aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions); override;
    procedure SetValue(Instance: TObject; Value: PUTF8Char; wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSQL: boolean; var result: RawUTF8;
      wasSQLString: PBoolean); override;
    procedure GetFieldSQLVar(Instance: TObject; var aValue: TSQLVar;
      var temp: RawByteString); override;
    function SetFieldSQLVar(Instance: TObject; const aValue: TSQLVar): boolean; override;
    procedure GetBinary(Instance: TObject; W: TBufferWriter); override;
    function SetBinary(Instance: TObject; P, PEnd: PAnsiChar): PAnsiChar; override;
    function CompareValue(Item1, Item2: TObject; CaseInsensitive: boolean): PtrInt; override;
    function GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal; override;
    procedure NormalizeValue(var Value: RawUTF8); override;
    procedure GetJSONValues(Instance: TObject; W: TJSONSerializer); override;
  end;

  /// information about a set published property
  TOrmPropInfoRTTISet = class(TOrmPropInfoRTTIInt32)
  protected
    fSetEnumType: PRttiEnumType;
  public
    constructor Create(aPropInfo: PRttiProp; aPropIndex: integer;
      aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions); override;
    property SetEnumType: PRttiEnumType read fSetEnumType;
  end;

  /// information about a enumeration published property
  // - can be either oftBoolean or oftEnumerate kind of property
  TOrmPropInfoRTTIEnum = class(TOrmPropInfoRTTIInt32)
  protected
    fEnumType: PRttiEnumType;
  public
    constructor Create(aPropInfo: PRttiProp; aPropIndex: integer;
      aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions); override;
    procedure SetValue(Instance: TObject; Value: PUTF8Char; wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSQL: boolean; var result: RawUTF8;
      wasSQLString: PBoolean); override;
    procedure NormalizeValue(var Value: RawUTF8); override;
    procedure GetJSONValues(Instance: TObject; W: TJSONSerializer); override;
    function GetCaption(Value: RawUTF8; out IntValue: integer): string;
    property EnumType: PRttiEnumType read fEnumType;
  end;

  /// information about a character published property
  TOrmPropInfoRTTIChar = class(TOrmPropInfoRTTIInt32)
  public
    procedure SetValue(Instance: TObject; Value: PUTF8Char; wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSQL: boolean; var result: RawUTF8;
      wasSQLString: PBoolean); override;
    procedure NormalizeValue(var Value: RawUTF8); override;
  end;

  /// information about an ordinal Int64 published property
  TOrmPropInfoRTTIInt64 = class(TOrmPropInfoRTTI)
  protected
    fIsQWord: boolean;
    procedure CopySameClassProp(Source: TObject; DestInfo: TOrmPropInfo;
      Dest: TObject); override;
  public
    constructor Create(aPropInfo: PRttiProp; aPropIndex: integer;
      aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions); override;
    procedure SetValue(Instance: TObject; Value: PUTF8Char; wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSQL: boolean; var result: RawUTF8;
      wasSQLString: PBoolean); override;
    procedure GetFieldSQLVar(Instance: TObject; var aValue: TSQLVar;
      var temp: RawByteString); override;
    function SetFieldSQLVar(Instance: TObject; const aValue: TSQLVar): boolean; override;
    procedure GetBinary(Instance: TObject; W: TBufferWriter); override;
    function SetBinary(Instance: TObject; P, PEnd: PAnsiChar): PAnsiChar; override;
    function CompareValue(Item1, Item2: TObject; CaseInsensitive: boolean): PtrInt; override;
    function GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal; override;
    procedure NormalizeValue(var Value: RawUTF8); override;
    procedure GetJSONValues(Instance: TObject; W: TJSONSerializer); override;
  end;

  /// information about a PtrInt published property, according to the native CPU
  // - not a real stand-alone class, but a convenient wrapper type
  {$ifdef CPU64}
  TOrmPropInfoRTTIPtrInt = TOrmPropInfoRTTIInt64;
  {$else}
  TOrmPropInfoRTTIPtrInt =TOrmPropInfoRTTIInt32;
  {$endif CPU64}

  /// information about a TTimeLog published property
  // - stored as an Int64, but with a specific class
  TOrmPropInfoRTTITimeLog = class(TOrmPropInfoRTTIInt64);

  /// information about a TUnixTime published property
  // - stored as an Int64, but with a specific class
  TOrmPropInfoRTTIUnixTime = class(TOrmPropInfoRTTIInt64);

  /// information about a TUnixMSTime published property
  // - stored as an Int64, but with a specific class
  TOrmPropInfoRTTIUnixMSTime = class(TOrmPropInfoRTTIInt64);

  /// information about a floating-point Double published property
  TOrmPropInfoRTTIDouble = class(TOrmPropInfoRTTI)
  protected
    procedure CopySameClassProp(Source: TObject; DestInfo: TOrmPropInfo;
      Dest: TObject); override;
  public
    procedure SetValue(Instance: TObject; Value: PUTF8Char; wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSQL: boolean; var result: RawUTF8;
      wasSQLString: PBoolean); override;
    procedure GetFieldSQLVar(Instance: TObject; var aValue: TSQLVar;
      var temp: RawByteString); override;
    function SetFieldSQLVar(Instance: TObject; const aValue: TSQLVar): boolean; override;
    procedure GetBinary(Instance: TObject; W: TBufferWriter); override;
    function SetBinary(Instance: TObject; P, PEnd: PAnsiChar): PAnsiChar; override;
    procedure NormalizeValue(var Value: RawUTF8); override;
    procedure GetJSONValues(Instance: TObject; W: TJSONSerializer); override;
    function CompareValue(Item1, Item2: TObject; CaseInsensitive: boolean): PtrInt; override;
    function GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal; override;
  end;

  /// information about a fixed-decimal Currency published property
  TOrmPropInfoRTTICurrency = class(TOrmPropInfoRTTIDouble)
  protected
    procedure CopySameClassProp(Source: TObject; DestInfo: TOrmPropInfo;
      Dest: TObject); override;
  public
    procedure SetValue(Instance: TObject; Value: PUTF8Char; wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSQL: boolean; var result: RawUTF8;
      wasSQLString: PBoolean); override;
    procedure GetFieldSQLVar(Instance: TObject; var aValue: TSQLVar;
      var temp: RawByteString); override;
    function SetFieldSQLVar(Instance: TObject; const aValue: TSQLVar): boolean; override;
    procedure GetBinary(Instance: TObject; W: TBufferWriter); override;
    function SetBinary(Instance: TObject; P, PEnd: PAnsiChar): PAnsiChar; override;
    procedure NormalizeValue(var Value: RawUTF8); override;
    procedure GetJSONValues(Instance: TObject; W: TJSONSerializer); override;
    function CompareValue(Item1, Item2: TObject; CaseInsensitive: boolean): PtrInt; override;
    function GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal; override;
  end;

  /// information about a TDateTime published property
  TOrmPropInfoRTTIDateTime = class(TOrmPropInfoRTTIDouble)
  public
    procedure SetValue(Instance: TObject; Value: PUTF8Char; wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSQL: boolean; var result: RawUTF8;
      wasSQLString: PBoolean); override;
    procedure GetFieldSQLVar(Instance: TObject; var aValue: TSQLVar; var temp:
      RawByteString); override;
    procedure NormalizeValue(var Value: RawUTF8); override;
    procedure GetJSONValues(Instance: TObject; W: TJSONSerializer); override;
    function CompareValue(Item1, Item2: TObject; CaseInsensitive: boolean):
      PtrInt; override;
  end;

  /// information about a AnsiString published property
  TOrmPropInfoRTTIAnsi = class(TOrmPropInfoRTTI)
  protected
    fEngine: TSynAnsiConvert;
    procedure CopySameClassProp(Source: TObject; DestInfo: TOrmPropInfo;
      Dest: TObject); override;
  public
    constructor Create(aPropInfo: PRttiProp; aPropIndex: integer;
      aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions); override;
    procedure SetValue(Instance: TObject; Value: PUTF8Char; wasString: boolean); override;
    procedure SetValueVar(Instance: TObject; const Value: RawUTF8; wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSQL: boolean; var result: RawUTF8;
      wasSQLString: PBoolean); override;
    procedure CopyValue(Source, Dest: TObject); override;
    procedure GetBinary(Instance: TObject; W: TBufferWriter); override;
    function SetBinary(Instance: TObject; P, PEnd: PAnsiChar): PAnsiChar; override;
    procedure GetJSONValues(Instance: TObject; W: TJSONSerializer); override;
    procedure GetFieldSQLVar(Instance: TObject; var aValue: TSQLVar; var temp:
      RawByteString); override;
    function SetFieldSQLVar(Instance: TObject; const aValue: TSQLVar): boolean; override;
    function CompareValue(Item1, Item2: TObject; CaseInsensitive: boolean): PtrInt; override;
    function GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal; override;
    procedure NormalizeValue(var Value: RawUTF8); override;
  end;

  /// information about a RawUTF8 published property
  // - will also serialize a RawJSON property without JSON escape
  TOrmPropInfoRTTIRawUTF8 = class(TOrmPropInfoRTTIAnsi)
  protected
    procedure CopySameClassProp(Source: TObject; DestInfo: TOrmPropInfo;
      Dest: TObject); override;
  public
    procedure SetValue(Instance: TObject; Value: PUTF8Char; wasString: boolean); override;
    procedure SetValueVar(Instance: TObject; const Value: RawUTF8;
      wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSQL: boolean; var result: RawUTF8;
      wasSQLString: PBoolean); override;
    procedure GetFieldSQLVar(Instance: TObject; var aValue: TSQLVar;
      var temp: RawByteString); override;
    function SetFieldSQLVar(Instance: TObject; const aValue: TSQLVar): boolean; override;
    function SetBinary(Instance: TObject; P, PEnd: PAnsiChar): PAnsiChar; override;
    function CompareValue(Item1, Item2: TObject; CaseInsensitive: boolean): PtrInt; override;
    function GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal; override;
    procedure GetJSONValues(Instance: TObject; W: TJSONSerializer); override;
  end;

  /// information about a RawUnicode published property
  TOrmPropInfoRTTIRawUnicode = class(TOrmPropInfoRTTIAnsi)
  protected
    procedure CopySameClassProp(Source: TObject; DestInfo: TOrmPropInfo;
      Dest: TObject); override;
  public
    procedure SetValue(Instance: TObject; Value: PUTF8Char; wasString: boolean); override;
    procedure SetValueVar(Instance: TObject; const Value: RawUTF8;
      wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSQL: boolean; var result: RawUTF8;
      wasSQLString: PBoolean); override;
    function CompareValue(Item1, Item2: TObject; CaseInsensitive: boolean): PtrInt; override;
    function GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal; override;
  end;

  /// information about a RawBlob published property
  TOrmPropInfoRTTIRawBlob = class(TOrmPropInfoRTTIAnsi)
  protected
    procedure CopySameClassProp(Source: TObject; DestInfo: TOrmPropInfo;
      Dest: TObject); override;
  public
    procedure SetValue(Instance: TObject; Value: PUTF8Char; wasString: boolean); override;
    procedure SetValueVar(Instance: TObject; const Value: RawUTF8;
      wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSQL: boolean; var result: RawUTF8;
      wasSQLString: PBoolean); override;
    procedure GetFieldSQLVar(Instance: TObject; var aValue: TSQLVar; var temp:
      RawByteString); override;
    function SetFieldSQLVar(Instance: TObject; const aValue: TSQLVar): boolean; override;
    function CompareValue(Item1, Item2: TObject; CaseInsensitive: boolean): PtrInt; override;
    function GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal; override;
    procedure GetJSONValues(Instance: TObject; W: TJSONSerializer); override;
    procedure GetBlob(Instance: TObject; var Blob: RawByteString);
    procedure SetBlob(Instance: TObject; const Blob: RawByteString);
    function IsNull(Instance: TObject): boolean;
  end;

  /// information about a WideString published property
  TOrmPropInfoRTTIWide = class(TOrmPropInfoRTTI)
  protected
    procedure CopySameClassProp(Source: TObject; DestInfo: TOrmPropInfo; Dest:
      TObject); override;
  public
    procedure SetValue(Instance: TObject; Value: PUTF8Char; wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSQL: boolean; var result: RawUTF8;
      wasSQLString: PBoolean); override;
    procedure CopyValue(Source, Dest: TObject); override;
    procedure GetBinary(Instance: TObject; W: TBufferWriter); override;
    function SetBinary(Instance: TObject; P, PEnd: PAnsiChar): PAnsiChar; override;
    procedure GetJSONValues(Instance: TObject; W: TJSONSerializer); override;
    function CompareValue(Item1, Item2: TObject; CaseInsensitive: boolean): PtrInt; override;
    function GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal; override;
  end;

  {$ifdef HASVARUSTRING}
  /// information about a UnicodeString published property
  TOrmPropInfoRTTIUnicode = class(TOrmPropInfoRTTI)
  protected
    procedure CopySameClassProp(Source: TObject; DestInfo: TOrmPropInfo;
      Dest: TObject); override;
  public
    procedure SetValue(Instance: TObject; Value: PUTF8Char; wasString: boolean); override;
    procedure SetValueVar(Instance: TObject; const Value: RawUTF8;
      wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSQL: boolean; var result: RawUTF8;
      wasSQLString: PBoolean); override;
    procedure CopyValue(Source, Dest: TObject); override;
    procedure GetBinary(Instance: TObject; W: TBufferWriter); override;
    function SetBinary(Instance: TObject; P, PEnd: PAnsiChar): PAnsiChar; override;
    procedure GetJSONValues(Instance: TObject; W: TJSONSerializer); override;
    procedure GetFieldSQLVar(Instance: TObject; var aValue: TSQLVar;
      var temp: RawByteString); override;
    function SetFieldSQLVar(Instance: TObject; const aValue: TSQLVar): boolean; override;
    function CompareValue(Item1, Item2: TObject; CaseInsensitive: boolean): PtrInt; override;
    function GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal; override;
  end;
  {$endif HASVARUSTRING}

  /// information about a dynamic array published property
  TOrmPropInfoRTTIDynArray = class(TOrmPropInfoRTTI)
  protected
    fObjArray: TRttiJson;
    procedure GetDynArray(Instance: TObject; var result: TDynArray);
      {$ifdef HASINLINE}inline;{$endif}
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
    // class function CreateFrom()
    constructor Create(aPropInfo: PRttiProp; aPropIndex: integer;
      aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions); override;
    procedure SetValue(Instance: TObject; Value: PUTF8Char; wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSQL: boolean; var result: RawUTF8;
      wasSQLString: PBoolean); override;
    procedure GetFieldSQLVar(Instance: TObject; var aValue: TSQLVar;
      var temp: RawByteString); override;
    function SetFieldSQLVar(Instance: TObject; const aValue: TSQLVar): boolean; override;
    procedure GetBinary(Instance: TObject; W: TBufferWriter); override;
    function SetBinary(Instance: TObject; P, PEnd: PAnsiChar): PAnsiChar; override;
    function CompareValue(Item1, Item2: TObject; CaseInsensitive: boolean): PtrInt; override;
    function GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal; override;
    procedure NormalizeValue(var Value: RawUTF8); override;
    procedure GetJSONValues(Instance: TObject; W: TJSONSerializer); override;
    procedure GetVariant(Instance: TObject; var Dest: Variant); override;
    procedure SetVariant(Instance: TObject; const Source: Variant); override;
    /// optional index of the dynamic array published property
    // - used e.g. for fast lookup by TOrm.DynArray(DynArrayFieldIndex)
    property DynArrayIndex: integer read fFieldWidth;
    /// read-only access to the low-level type information the array item type
    property DynArrayElemType: TRttiCustom read GetDynArrayElemType;
    /// dynamic array item information for a T*ObjArray
    // - equals nil if this dynamic array was not previously registered via
    // Rtti.RegisterObjArray()
    // - note that if the field is a T*ObjArray, you could create a new item
    // by calling ObjArray^.ClassNewInstance
    // - T*ObjArray database column will be stored as text
    property ObjArray: TRttiJson read fObjArray;
  end;

  TOrmPropInfoRTTIDynArrayObjArray = array of TOrmPropInfoRTTIDynArray;

  /// information about a variant published property
  // - is also used for TNullable* properties
  TOrmPropInfoRTTIVariant = class(TOrmPropInfoRTTI)
  protected
    fDocVariantOptions: TDocVariantOptions;
    procedure CopySameClassProp(Source: TObject; DestInfo: TOrmPropInfo;
      Dest: TObject); override;
  public
    /// initialize the internal fields
    constructor Create(aPropInfo: PRttiProp; aPropIndex: integer;
      aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions); override;
    procedure SetValue(Instance: TObject; Value: PUTF8Char; wasString: boolean); override;
    procedure SetValueVar(Instance: TObject; const Value: RawUTF8; wasString: boolean); override;
    procedure SetValuePtr(Instance: TObject; Value: PUTF8Char; ValueLen: integer;
      wasString: boolean);
    procedure GetValueVar(Instance: TObject; ToSQL: boolean; var result: RawUTF8;
      wasSQLString: PBoolean); override;
    procedure GetBinary(Instance: TObject; W: TBufferWriter); override;
    function SetBinary(Instance: TObject; P, PEnd: PAnsiChar): PAnsiChar; override;
    function CompareValue(Item1, Item2: TObject; CaseInsensitive: boolean): PtrInt; override;
    function GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal; override;
    procedure NormalizeValue(var Value: RawUTF8); override;
    procedure GetJSONValues(Instance: TObject; W: TJSONSerializer); override;
    procedure GetVariant(Instance: TObject; var Dest: Variant); override;
    procedure SetVariant(Instance: TObject; const Source: Variant); override;
    /// how this property will deal with its instances (including TDocVariant)
    // - by default, contains JSON_OPTIONS_FAST for best performance - i.e.
    // [dvoReturnNullForUnknownProperty,dvoValueCopiedByReference]
    // - set JSON_OPTIONS_FAST_EXTENDED (or include dvoSerializeAsExtendedJson)
    // so that any TDocVariant nested field names will not be double-quoted,
    // saving some chars in the stored TEXT column and in the JSON escaped
    // transmitted data over REST, by writing '{name:"John",age:123}' instead of
    // '{"name":"John","age":123}': be aware that this syntax is supported by
    // the ORM, SOA, TDocVariant, TBSONVariant, and our SynCrossPlatformJSON
    // unit, but not AJAX/JavaScript or most JSON libraries
    // - see also TOrmModel/TOrmProperties.SetVariantFieldsDocVariantOptions
    property DocVariantOptions: TDocVariantOptions
      read fDocVariantOptions write fDocVariantOptions;
  end;

  /// optional event handler used by TOrmPropInfoRecord to handle textual storage
  // - by default, TOrmPropInfoRecord content will be stored as oftBlobCustom;
  // specify such a callback event to allow storage as UTF-8 textual field and
  // use a oftUTF8Custom kind of column
  // - event implementation shall convert data/datalen binary value into Text
  TOnSQLPropInfoRecord2Text = procedure(Data: pointer; DataLen: integer;
    var Text: RawUTF8);

  /// optional event handler used by TOrmPropInfoRecord to handle textual storage
  // - by default, TOrmPropInfoRecord content will be stored as oftBlobCustom;
  // specify such a callback event to allow storage as UTF-8 textual field and
  // use a oftUTF8Custom kind of column
  // - event implementaiton shall convert Text into Data binary value
  TOnSQLPropInfoRecord2Data = procedure(Text: PUTF8Char; var Data: RawByteString);

  /// abstract information about a record-like property defined directly in code
  // - do not use this class, but TOrmPropInfoRecordRTTI and TOrmPropInfoRecordFixedSize
  // - will store the content as BLOB by default, and OrmFieldType as oftBlobCustom
  // - if aData2Text/aText2Data are defined, use TEXT storage and oftUTF8Custom type
  TOrmPropInfoCustom = class(TOrmPropInfo)
  protected
    fOffset: PtrUInt;
    fData2Text: TOnSQLPropInfoRecord2Text;
    fText2Data: TOnSQLPropInfoRecord2Data;
    procedure BinaryToText(var Value: RawUTF8; ToSQL: boolean;
      wasSQLString: PBoolean); override;
    procedure TextToBinary(Value: PUTF8Char; var result: RawByteString); override;
  public
    /// define a custom property in code
    // - do not call this constructor directly, but one of its inherited classes,
    // via a call to TOrmProperties.RegisterCustom*()
    constructor Create(const aName: RawUTF8; aOrmFieldType: TOrmFieldType;
      aAttributes: TOrmPropInfoAttributes; aFieldWidth, aPropIndex: integer;
      aProperty: pointer; aData2Text: TOnSQLPropInfoRecord2Text;
      aText2Data: TOnSQLPropInfoRecord2Data); reintroduce;
  public
    function GetFieldAddr(Instance: TObject): pointer; override;
  end;

  /// information about a record property defined directly in code using RTTI
  TOrmPropInfoRecordTyped = class(TOrmPropInfoCustom)
  protected
    fTypeInfo: PRttiInfo;
  public
    property TypeInfo: PRttiInfo read fTypeInfo;
  end;

  /// information about a record property defined directly in code
  // - Delphi does not publish RTTI for published record properties
  // - you can use this class to register a record property from its RTTI
  // - will store the content as BLOB by default, and OrmFieldType as oftBlobCustom
  // - if aData2Text/aText2Data are defined, use TEXT storage and oftUTF8Custom type
  // - this class will use only binary RecordLoad/RecordSave methods
  TOrmPropInfoRecordRTTI = class(TOrmPropInfoRecordTyped)
  protected
    function GetSQLFieldRTTITypeName: RawUTF8; override;
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
    // TOrmProperties.RegisterCustomRTTIRecordProperty()
    // - optional aIsNotUnique parametercanl be defined
    // - implementation will use internally RecordLoad/RecordSave functions
    // - you can specify optional aData2Text/aText2Data callbacks to store
    // the content as textual values, and not as BLOB
    constructor Create(aRecordInfo: PRttiInfo; const aName: RawUTF8;
      aPropertyIndex: integer; aPropertyPointer: pointer;
      aAttributes: TOrmPropInfoAttributes = []; aFieldWidth: integer = 0;
      aData2Text: TOnSQLPropInfoRecord2Text = nil;
      aText2Data: TOnSQLPropInfoRecord2Data = nil); reintroduce; overload;
  public
    procedure SetValue(Instance: TObject; Value: PUTF8Char; wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSQL: boolean; var result: RawUTF8;
      wasSQLString: PBoolean); override;
    procedure GetFieldSQLVar(Instance: TObject; var aValue: TSQLVar;
      var temp: RawByteString); override;
    function SetFieldSQLVar(Instance: TObject; const aValue: TSQLVar): boolean; override;
    procedure GetBinary(Instance: TObject; W: TBufferWriter); override;
    function SetBinary(Instance: TObject; P, PEnd: PAnsiChar): PAnsiChar; override;
    function CompareValue(Item1, Item2: TObject; CaseInsensitive: boolean): PtrInt; override;
    function GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal; override;
    procedure NormalizeValue(var Value: RawUTF8); override;
    procedure GetVariant(Instance: TObject; var Dest: Variant); override;
    procedure SetVariant(Instance: TObject; const Source: Variant); override;
  end;

  /// information about a fixed-size record property defined directly in code
  // - Delphi does not publish RTTI for published record properties
  // - you can use this class to register a record property with no RTTI (i.e.
  // a record with no reference-counted types within)
  // - will store the content as BLOB by default, and OrmFieldType as oftBlobCustom
  // - if aData2Text/aText2Data are defined, use TEXT storage and oftUTF8Custom type
  TOrmPropInfoRecordFixedSize = class(TOrmPropInfoRecordTyped)
  protected
    fRecordSize: integer;
    function GetSQLFieldRTTITypeName: RawUTF8; override;
    procedure CopySameClassProp(Source: TObject; DestInfo: TOrmPropInfo;
      Dest: TObject); override;
  public
    /// define an unmanaged fixed-size record property
    // - simple kind of records (i.e. those not containing reference-counted
    // members) do not have RTTI generated, at least in older versions of Delphi:
    // use this constructor to define a direct property access
    // - main parameter is the record size, in bytes
    constructor Create(aRecordSize: cardinal; const aName: RawUTF8;
      aPropertyIndex: integer; aPropertyPointer: pointer;
      aAttributes: TOrmPropInfoAttributes = []; aFieldWidth: integer = 0;
      aData2Text: TOnSQLPropInfoRecord2Text = nil;
      aText2Data: TOnSQLPropInfoRecord2Data = nil); reintroduce; overload;
  public
    procedure SetValue(Instance: TObject; Value: PUTF8Char; wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSQL: boolean; var result: RawUTF8;
      wasSQLString: PBoolean); override;
    procedure GetFieldSQLVar(Instance: TObject; var aValue: TSQLVar;
      var temp: RawByteString); override;
    function SetFieldSQLVar(Instance: TObject; const aValue: TSQLVar): boolean; override;
    procedure GetBinary(Instance: TObject; W: TBufferWriter); override;
    function SetBinary(Instance: TObject; P, PEnd: PAnsiChar): PAnsiChar; override;
    function CompareValue(Item1, Item2: TObject; CaseInsensitive: boolean): PtrInt; override;
    function GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal; override;
    procedure NormalizeValue(var Value: RawUTF8); override;
    procedure GetVariant(Instance: TObject; var Dest: Variant); override;
    procedure SetVariant(Instance: TObject; const Source: Variant); override;
  end;

  /// information about a custom property defined directly in code
  // - you can define any kind of property, either a record or any type
  // - this class will use JSON serialization, by type name or TypeInfo() pointer
  // - will store the content as TEXT by default, and OrmFieldType as oftUTF8Custom
  TOrmPropInfoCustomJSON = class(TOrmPropInfoRecordTyped)
  protected
    fCustomParser: TRttiJson;
    function GetSQLFieldRTTITypeName: RawUTF8; override;
    procedure SetCustomParser(aCustomParser: TRttiJson);
  public
    /// initialize the internal fields
    // - should not be called directly
    constructor Create(aPropInfo: PRttiProp; aPropIndex: integer);
      reintroduce; overload; virtual;
    /// define a custom property from its RTTI definition
    // - handle any kind of property, e.g. from enhanced RTTI or a custom record
    // defined via TTextWriter.RegisterCustomJSONSerializer[FromText]()
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
    // TOrmProperties.RegisterCustomPropertyFromRTTI()
    // - optional aIsNotUnique parameter can be defined
    // - implementation will use internally RecordLoadJSON/RecordSave functions
    // - you can specify optional aData2Text/aText2Data callbacks to store
    // the content as textual values, and not as BLOB
    constructor Create(aTypeInfo: PRttiInfo; const aName: RawUTF8;
      aPropertyIndex: integer; aPropertyPointer: pointer;
      aAttributes: TOrmPropInfoAttributes = []; aFieldWidth: integer = 0);
      reintroduce; overload;
    /// define a custom property from its RTTI definition
    // - handle any kind of property, e.g. from enhanced RTTI or a custom record
    // defined via TTextWriter.RegisterCustomJSONSerializer[FromText]()
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
    // - implementation will use internally RecordLoadJSON/RecordSave functions
    // - you can specify optional aData2Text/aText2Data callbacks to store
    // the content as textual values, and not as BLOB
    constructor Create(const aTypeName, aName: RawUTF8; aPropertyIndex: integer;
      aPropertyPointer: pointer; aAttributes: TOrmPropInfoAttributes = [];
      aFieldWidth: integer = 0); reintroduce; overload;
    /// finalize the instance
    destructor Destroy; override;
    /// the corresponding custom JSON parser
    property CustomParser: TRttiJson read fCustomParser;
  public
    procedure SetValue(Instance: TObject; Value: PUTF8Char; wasString: boolean); override;
    procedure GetJSONValues(Instance: TObject; W: TJSONSerializer); override;
    procedure GetValueVar(Instance: TObject; ToSQL: boolean; var result: RawUTF8;
      wasSQLString: PBoolean); override;
    procedure GetBinary(Instance: TObject; W: TBufferWriter); override;
    function SetBinary(Instance: TObject; P, PEnd: PAnsiChar): PAnsiChar; override;
    procedure NormalizeValue(var Value: RawUTF8); override;
  end;

  /// dynamic array of ORM fields information for published properties
  TOrmPropInfoObjArray = array of TOrmPropInfo;

  /// handle a read-only list of fields information for published properties
  // - is mainly used by our ORM for TOrm RTTI, but may be used for
  // any TPersistent
  TOrmPropInfoList = class
  protected
    fList: TOrmPropInfoObjArray;
    fCount: integer;
    fTable: TClass; // TOrmClass
    fOptions: TOrmPropInfoListOptions;
    fOrderedByName: TIntegerDynArray;
    function GetItem(aIndex: integer): TOrmPropInfo;
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
    /// find an item in the list
    // - returns nil if not found
    function ByRawUTF8Name(const aName: RawUTF8): TOrmPropInfo; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// find an item in the list
    // - returns nil if not found
    function ByName(aName: PUTF8Char): TOrmPropInfo; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// find an item in the list
    // - returns -1 if not found
    function IndexByName(const aName: RawUTF8): integer; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// find an item in the list
    // - returns -1 if not found
    function IndexByName(aName: PUTF8Char): integer; overload;
    /// find an item by name in the list, including RowID/ID
    // - will identify 'ID' / 'RowID' field name as -1
    // - raise an EOrmException if not found in the internal list
    function IndexByNameOrExcept(const aName: RawUTF8): integer;
    /// find an item by name in the list, including RowID/ID
    // - will identify 'ID' / 'RowID' field name as -1
    // - raise an EOrmException if not found in the internal list
    // - aName is not defined as "const aName" since it is made an ASCIIZ
    function IndexByNameOrExceptShort(aName: ShortString): integer;
    /// find one or several items by name in the list, including RowID/ID
    // - will identify 'ID' / 'RowID' field name as -1
    // - raise an EOrmException if not found in the internal list
    procedure IndexesByNamesOrExcept(const aNames: array of RawUTF8;
      const aIndexes: array of PInteger);
    /// find an item in the list, searching by unflattened name
    // - for a flattened property, you may for instance call
    // IndexByNameUnflattenedOrExcept('Address.Country.Iso')
    // instead of IndexByNameOrExcept('Address_Country')
    // - won't identify 'ID' / 'RowID' field names, just List[].
    // - raise an EOrmException if not found in the internal list
    function IndexByNameUnflattenedOrExcept(const aName: RawUTF8): integer;
    /// fill a TRawUTF8DynArray instance from the field names
    // - excluding ID
    procedure NamesToRawUTF8DynArray(var Names: TRawUTF8DynArray);
    /// returns the number of TOrmPropInfo in the list
    property Count: integer read fCount;
    /// quick access to the TOrmPropInfo list
    // - note that length(List) may not equal Count, since is its capacity
    property List: TOrmPropInfoObjArray read fList;
    /// read-only retrieval of a TOrmPropInfo item
    // - will raise an exception if out of range
    property Items[aIndex: integer]: TOrmPropInfo read GetItem;
  end;

  /// information about a TOrm class property
  // - oftID for TOrm properties, which are pointer(RecordID), not
  // any true class instance
  // - oftMany for TOrmMany properties, for which no data is
  // stored in the table itself, but in a pivot table
  // - oftObject for e.g. TStrings TRawUTF8List TCollection instances
  TOrmPropInfoRTTIInstance = class(TOrmPropInfoRTTIPtrInt)
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
    property ObjectClass: TClass read fObjectClass;
  end;

  /// information about a TRecordReference/TRecordReferenceToBeDeleted
  // published property
  // - identified as a oftRecord kind of property
  TOrmPropInfoRTTIRecordReference = class(TOrmPropInfoRTTIInt64)
  protected
    fCascadeDelete: boolean;
  public
    /// will identify TRecordReferenceToBeDeleted kind of field, and
    // setup the corresponding CascadeDelete property
    constructor Create(aPropInfo: PRttiProp; aPropIndex: integer;
      aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions); override;
    /// TRUE if this oftRecord is a TRecordReferenceToBeDeleted
    property CascadeDelete: boolean read fCascadeDelete;
  end;

  /// information about a TRecordVersion published property
  // - identified as a oftRecordVersion kind of property, to track changes
  TOrmPropInfoRTTIRecordVersion = class(TOrmPropInfoRTTIInt64);

  /// information about a TOrm class TOrm property
  // - kind oftID, which are pointer(RecordID), not any true class instance
  // - will store the content just as an integer value
  // - will recognize any instance pre-allocated via Create*Joined() constructor
  TOrmPropInfoRTTIID = class(TOrmPropInfoRTTIInstance)
  public
    /// raise an exception if was created by Create*Joined() constructor
    procedure SetValue(Instance: TObject; Value: PUTF8Char; wasString: boolean); override;
    /// this method will recognize if the TOrm was allocated by
    // a Create*Joined() constructor: in this case, it will write the ID
    // of the nested property, and not the PtrInt() transtyped value
    procedure GetJSONValues(Instance: TObject; W: TJSONSerializer); override;
  end;

  TOrmPropInfoRTTIIObjArray = array of TOrmPropInfoRTTIID;

  /// information about a TOrm class TStrings/TRawUTF8List/TCollection
  // property
  // - kind oftObject e.g. for TStrings TRawUTF8List TCollection TObjectList instances
  // - binary serialization will store textual JSON serialization of the
  // object, including custom serialization
  TOrmPropInfoRTTIObject = class(TOrmPropInfoRTTIInstance)
  protected
    procedure CopySameClassProp(Source: TObject; DestInfo: TOrmPropInfo;
      Dest: TObject); override;
  public
    procedure SetValue(Instance: TObject; Value: PUTF8Char; wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSQL: boolean; var result: RawUTF8;
      wasSQLString: PBoolean); override;
    procedure GetBinary(Instance: TObject; W: TBufferWriter); override;
    function SetBinary(Instance: TObject; P, PEnd: PAnsiChar): PAnsiChar; override;
    function GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal; override;
    procedure NormalizeValue(var Value: RawUTF8); override;
    procedure GetJSONValues(Instance: TObject; W: TJSONSerializer); override;
  end;

  /// information about a TOrm class TOrmMany property
  // - kind oftMany, for which no data is stored in the table itself, but in
  // a separated pivot table
  TOrmPropInfoRTTIMany = class(TOrmPropInfoRTTIInstance)
  public
    procedure SetValue(Instance: TObject; Value: PUTF8Char; wasString: boolean); override;
    procedure GetValueVar(Instance: TObject; ToSQL: boolean; var result: RawUTF8;
      wasSQLString: PBoolean); override;
    procedure GetBinary(Instance: TObject; W: TBufferWriter); override;
    function SetBinary(Instance: TObject; P, PEnd: PAnsiChar): PAnsiChar; override;
  end;

  TOrmPropInfoRTTIManyObjArray = array of TOrmPropInfoRTTIMany;

  POrmPropInfoRTTIMany = ^TOrmPropInfoRTTIMany;



{ ************ TOrm TOrmModel TOrmTable IRestOrm Core Definitions }

const
  /// maximum handled dimension for TOrmRTree
  // - this value is the one used by SQLite3 R-Tree virtual table
  RTREE_MAX_DIMENSION = 5;

// most types are defined as a single "type" statement due to classes coupling

type
  {$M+}
  { we expect RTTI information for the published properties of these
    forward definitions - due to internal coupling, those classes are
    to be defined in a single "type" statement }
  TOrmTable = class;
  TOrm = class;
  TOrmMany = class;
  TOrmFts3 = class;
  TOrmRTree = class;
  TOrmProperties = class;
  TOrmModel = class;
  TOrmModelProperties = class;
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

  /// exception raised in case of incorrect TOrmTable.Step / Field*() use
  EOrmTable = class(ESynException);

  /// exception raised in case of TRestBatch problem
  EOrmBatchException = class(EOrmException);

  /// exception raised in case of wrong Model definition
  EModelException = class(EOrmException);


  { -------------------- IRestOrm IRestOrmServer IRestOrmClient Definitions }

  {$ifdef ISDELPHI2010} // Delphi 2009/2010 generics support is buggy :(
  TRestOrmGenerics = class;
  {$endif ISDELPHI2010}

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
    // - calls internaly the "SELECT Count(*) FROM TableName;" SQL statement
    function TableRowCount(Table: TOrmClass): Int64;
    /// check if there is some data rows in a specified table
    // - calls internaly a "SELECT RowID FROM TableName LIMIT 1" SQL statement,
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
    // ! aClient.OneFieldValue(TOrm, 'Name', FormatUTF8('ID=?', [], [23]))
    // - call internaly ExecuteList() to get the value
    function OneFieldValue(Table: TOrmClass;
      const FieldName, WhereClause: RawUTF8): RawUTF8; overload;
    /// get the Int64 value of an unique field with a Where Clause
    // - call internaly ExecuteList() to get the value
    function OneFieldValueInt64(Table: TOrmClass;
      const FieldName, WhereClause: RawUTF8; Default: Int64 = 0): Int64;
    /// get the UTF-8 encoded value of an unique field with a Where Clause
    // - this overloaded function will call FormatUTF8 to create the Where Clause
    // from supplied parameters, binding all '?' chars with Args[] values
    // - example of use:
    // ! aClient.OneFieldValue(TOrm, 'Name', 'ID=?', [aID])
    // - call internaly ExecuteList() to get the value
    // - note that this method prototype changed with revision 1.17 of the
    // framework: array of const used to be Args and '%' in the FormatSQLWhere
    // statement, whereas it now expects bound parameters as '?'
    function OneFieldValue(Table: TOrmClass; const FieldName: RawUTF8;
      const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const): RawUTF8; overload;
    /// get the UTF-8 encoded value of an unique field with a Where Clause
    // - this overloaded function will call FormatUTF8 to create the Where Clause
    // from supplied parameters, replacing all '%' chars with Args[], and all '?'
    // chars with Bounds[] (inlining them with :(...): and auto-quoting strings)
    // - example of use:
    // ! OneFieldValue(TOrm,'Name', '%=?', ['ID'], [aID])
    // - call internaly ExecuteList() to get the value
    function OneFieldValue(Table: TOrmClass; const FieldName: RawUTF8;
      const WhereClauseFmt: RawUTF8; const Args, Bounds: array of const): RawUTF8; overload;
    /// get one integer value of an unique field with a Where Clause
    // - this overloaded function will return the field value as integer
    function OneFieldValue(Table: TOrmClass; const FieldName: RawUTF8;
      const WhereClauseFmt: RawUTF8; const Args, Bounds: array of const;
      out Data: Int64): boolean; overload;
    /// get the UTF-8 encoded value of an unique field from its ID
    // - example of use: OneFieldValue(TOrm,'Name',23)
    // - call internaly ExecuteList() to get the value
    function OneFieldValue(Table: TOrmClass; const FieldName: RawUTF8;
      WhereID: TID): RawUTF8; overload;
    /// get the UTF-8 encoded value of some fields with a Where Clause
    // - example of use: MultiFieldValue(TOrm,['Name'],Name,'ID=:(23):')
    // (using inlined parameters via :(...): is always a good idea)
    // - FieldValue[] will have the same length as FieldName[]
    // - return true on success, false on SQL error or no result
    // - call internaly ExecuteList() to get the list
    function MultiFieldValue(Table: TOrmClass;
      const FieldName: array of RawUTF8; var FieldValue: array of RawUTF8;
      const WhereClause: RawUTF8): boolean; overload;
    /// get the UTF-8 encoded value of some fields from its ID
    // - example of use: MultiFieldValue(TOrm,['Name'],Name,23)
    // - FieldValue[] will have the same length as FieldName[]
    // - return true on success, false on SQL error or no result
    // - call internaly ExecuteList() to get the list
    function MultiFieldValue(Table: TOrmClass;
      const FieldName: array of RawUTF8; var FieldValue: array of RawUTF8;
      WhereID: TID): boolean; overload;
    /// get the UTF-8 encoded values of an unique field with a Where Clause
    // - example of use: OneFieldValue(TOrm,'FirstName','Name=:("Smith"):',Data)
    // (using inlined parameters via :(...): is always a good idea)
    // - leave WhereClause void to get all records
    // - call internaly ExecuteList() to get the list
    // - returns TRUE on success, FALSE if no data was retrieved
    function OneFieldValues(Table: TOrmClass; const FieldName: RawUTF8;
      const WhereClause: RawUTF8; out Data: TRawUTF8DynArray): boolean; overload;
    /// get the integer value of an unique field with a Where Clause
    // - example of use: OneFieldValue(TOrmPeople,'ID','Name=:("Smith"):',Data)
    // (using inlined parameters via :(...): is always a good idea)
    // - leave WhereClause void to get all records
    // - call internaly ExecuteList() to get the list
    function OneFieldValues(Table: TOrmClass; const FieldName: RawUTF8;
      const WhereClause: RawUTF8; var Data: TInt64DynArray;
      SQL: PRawUTF8 = nil): boolean; overload;
    /// get the CSV-encoded UTF-8 encoded values of an unique field with a Where Clause
    // - example of use: OneFieldValue(TOrm,'FirstName','Name=:("Smith")',Data)
    // (using inlined parameters via :(...): is always a good idea)
    // - leave WhereClause void to get all records
    // - call internaly ExecuteList() to get the list
    // - using inlined parameters via :(...): in WhereClause is always a good idea
    function OneFieldValues(Table: TOrmClass; const FieldName: RawUTF8;
      const WhereClause: RawUTF8 = ''; const Separator: RawUTF8 = ','): RawUTF8; overload;
    /// get the string-encoded values of an unique field into some TStrings
    // - Items[] will be filled with string-encoded values of the given field)
    // - Objects[] will be filled with pointer(ID)
    // - call internaly ExecuteList() to get the list
    // - returns TRUE on success, FALSE if no data was retrieved
    // - if IDToIndex is set, its value will be replaced with the index in
    // Strings.Objects[] where ID=IDToIndex^
    // - using inlined parameters via :(...): in WhereClause is always a good idea
    function OneFieldValues(Table: TOrmClass; const FieldName, WhereClause:
      RawUTF8; Strings: TStrings; IDToIndex: PID = nil): boolean; overload;
    /// Execute directly a SQL statement, returning a TOrmTable list of resutls
    // - return a TOrmTableJSON instance on success, nil on failure
    // - FieldNames can be the CSV list of field names to be retrieved
    // - if FieldNames is '', will get all simple fields, excluding BLOBs
    // - if FieldNames is '*', will get ALL fields, including ID and BLOBs
    // - call internaly ExecuteList() to get the list
    // - using inlined parameters via :(...): in WhereClause is always a good idea
    function MultiFieldValues(Table: TOrmClass; const FieldNames: RawUTF8;
      const WhereClause: RawUTF8 = ''): TOrmTable; overload;
    /// Execute directly a SQL statement, returning a TOrmTable list of resutls
    // - return a TOrmTableJSON instance on success, nil on failure
    // - FieldNames can be the CSV list of field names to be retrieved
    // - if FieldNames is '', will get all simple fields, excluding BLOBs
    // - if FieldNames is '*', will get ALL fields, including ID and BLOBs
    // - this overloaded function will call FormatUTF8 to create the Where Clause
    // from supplied parameters, binding all '?' chars with Args[] values
    // - example of use:
    // ! aList := aClient.MultiFieldValues(
    // !   TOrm, 'Name,FirstName', 'Salary>=?', [aMinSalary]);
    // - call overloaded MultiFieldValues() / ExecuteList() to get the list
    // - note that this method prototype changed with revision 1.17 of the
    // framework: array of const used to be Args and '%' in the WhereClauseFormat
    // statement, whereas it now expects bound parameters as '?'
    function MultiFieldValues(Table: TOrmClass; const FieldNames: RawUTF8;
      const WhereClauseFormat: RawUTF8; const BoundsSQLWhere: array of const): TOrmTable; overload;
    /// Execute directly a SQL statement, returning a TOrmTable list of resutls
    // - return a TOrmTableJSON instance on success, nil on failure
    // - FieldNames can be the CSV list of field names to be retrieved
    // - if FieldNames is '', will get all simple fields, excluding BLOBs
    // - if FieldNames is '*', will get ALL fields, including ID and BLOBs
    // - in this version, the WHERE clause can be created with the same format
    // as FormatUTF8() function, replacing all '%' chars with Args[], and all '?'
    // chars with Bounds[] (inlining them with :(...): and auto-quoting strings)
    // - example of use:
    // ! Table := MultiFieldValues(TOrm, 'Name', '%=?', ['ID'], [aID]);
    // - call overloaded MultiFieldValues() / ExecuteList() to get the list
    function MultiFieldValues(Table: TOrmClass; const FieldNames: RawUTF8;
      const WhereClauseFormat: RawUTF8; const Args, Bounds: array of const): TOrmTable; overload;
    /// dedicated method used to retrieve free-text matching DocIDs
    // - this method works for TOrmFts3, TOrmFts4 and TOrmFts5
    // - this method expects the column/field names to be supplied in the MATCH
    // statement clause
    // - example of use:  FTSMatch(TSQLMessage,'Body MATCH :("linu*"):',IntResult)
    // (using inlined parameters via :(...): is always a good idea)
    function FTSMatch(Table: TOrmFts3Class; const WhereClause: RawUTF8;
      var DocID: TIDDynArray): boolean; overload;
    /// dedicated method used to retrieve free-text matching DocIDs with
    // enhanced ranking information
    // - this method works for TOrmFts3, TOrmFts4 and TOrmFts5
    // - this method will search in all FTS3 columns, and except some floating-point
    // constants for weigthing each column (there must be the same number of
    // PerFieldWeight parameters as there are columns in the TOrmFts3 table)
    // - example of use:  FTSMatch(TSQLDocuments,'"linu*"',IntResult,[1,0.5])
    // which will sort the results by the rank obtained with the 1st column/field
    // beeing given twice the weighting of those in the 2nd (and last) column
    // - FTSMatch(TSQLDocuments,'linu*',IntResult,[1,0.5]) will perform a
    // SQL query as such, which is the fastest way of ranking according to
    // http://www.sqlite.org/fts3.html#appendix_a
    // $ SELECT RowID FROM Documents WHERE Documents MATCH 'linu*'
    // $ ORDER BY rank(matchinfo(Documents),1.0,0.5) DESC
    function FTSMatch(Table: TOrmFts3Class; const MatchClause: RawUTF8;
      var DocID: TIDDynArray; const PerFieldWeight: array of double;
      limit: integer = 0; offset: integer = 0): boolean; overload;
    /// retrieve the main field (mostly 'Name') value of the specified record
    // - use GetMainFieldName() method to get the main field name
    // - use OneFieldValue() method to get the field value
    // - return '' if no such field or record exists
    // - if ReturnFirstIfNoUnique is TRUE and no unique property is found,
    // the first RawUTF8 property is returned anyway
    function MainFieldValue(Table: TOrmClass; ID: TID;
      ReturnFirstIfNoUnique: boolean = false): RawUTF8;
    /// return the ID of the record which main field match the specified value
    // - search field is mainly the "Name" property, i.e. the one with
    // "stored AS_UNIQUE" (i.e. "stored false") definition on most TOrm
    // - returns 0 if no matching record was found }
    function MainFieldID(Table: TOrmClass; const Value: RawUTF8): TID;
    /// return the IDs of the record which main field match the specified values
    // - search field is mainly the "Name" property, i.e. the one with
    // "stored AS_UNIQUE" (i.e. "stored false") definition on most TOrm
    // - if any of the Values[] is not existing, then no ID will appear in the
    // IDs[] array - e.g. it will return [] if no matching record was found
    // - returns TRUE if any matching ID was found (i.e. if length(IDs)>0) }
    function MainFieldIDs(Table: TOrmClass; const Values: array of RawUTF8;
      out IDs: TIDDynArray): boolean;

    /// get a member from a SQL statement
    // - implements REST GET collection
    // - return true on success
    // - Execute 'SELECT * FROM TableName WHERE SQLWhere LIMIT 1' SQL Statememt
    // (using inlined parameters via :(...): in SQLWhere is always a good idea)
    // - since no record is specified, locking is pointless here
    // - default implementation call ExecuteList(), and fill Value from a
    // temporary TOrmTable
    // - if aCustomFieldsCSV is '', will get all simple fields, excluding BLOBs
    // and TOrmMany fields (use RetrieveBlob method or set
    // TRestClientURI.ForceBlobTransfert)
    // - if aCustomFieldsCSV is '*', will get ALL fields, including ID and BLOBs
    // - if this default set of simple fields does not fit your need, you could
    // specify your own set
    function Retrieve(const SQLWhere: RawUTF8; Value: TOrm;
      const aCustomFieldsCSV: RawUTF8 = ''): boolean; overload;
    /// get a member from a SQL statement
    // - implements REST GET collection
    // - return true on success
    // - same as Retrieve(const SQLWhere: RawUTF8; Value: TOrm) method, but
    // this overloaded function will call FormatUTF8 to create the Where Clause
    // from supplied parameters, replacing all '%' chars with Args[], and all '?'
    // chars with Bounds[] (inlining them with :(...): and auto-quoting strings)
    // - if aCustomFieldsCSV is '', will get all simple fields, excluding BLOBs
    // - if aCustomFieldsCSV is '*', will get ALL fields, including ID and BLOBs
    function Retrieve(const WhereClauseFmt: RawUTF8;
      const Args, Bounds: array of const; Value: TOrm;
      const aCustomFieldsCSV: RawUTF8 = ''): boolean; overload;
    /// get a member from its ID
    // - return true on success
    // - Execute 'SELECT * FROM TableName WHERE ID=:(aID): LIMIT 1' SQL Statememt
    // - if ForUpdate is true, the REST method is LOCK and not GET: it tries to lock
    // the corresponding record, then retrieve its content; caller has to call
    // UnLock() method after Value usage, to release the record
    // - this method will call EngineRetrieve() abstract method
    // - the RawBlob (BLOB) fields are not retrieved by this method, to
    // preserve bandwidth: use the RetrieveBlob() methods for handling
    // BLOB fields, or set either the TRestClientURI.ForceBlobTransfert
    // or TRestClientURI.ForceBlobTransfertTable[] properties
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
    // BLOB fields, or set either the TRestClientURI.ForceBlobTransfert
    // or TRestClientURI.ForceBlobTransfertTable[] properties
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
    // identified as '?' in the FormatSQLWhere statement, which is expected to
    // follow the order of values supplied in BoundsSQLWhere open array - use
    // DateToSQL()/DateTimeToSQL() for TDateTime, or directly any integer,
    // double, currency, RawUTF8 values to be bound to the request as parameters
    // - aCustomFieldsCSV can be the CSV list of field names to be retrieved
    // - if aCustomFieldsCSV is '', will get all simple fields, excluding BLOBs
    // - if aCustomFieldsCSV is '*', will get ALL fields, including ID and BLOBs
    // - return a TObjectList on success (possibly with Count=0) - caller is
    // responsible of freeing the instance
    // - this TObjectList will contain a list of all matching records
    // - return nil on error
    function RetrieveList(Table: TOrmClass;
      const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const;
      const aCustomFieldsCSV: RawUTF8 = ''): TObjectList; overload;
    {$ifdef ISDELPHI2010} // Delphi 2009/2010 generics support is buggy :(
    /// access to ORM parametrized/generics methods
    // - since Delphi interface cannot have parametrized methods, we need
    // to return a TRestOrmGenerics abstract class to use generics signature
    function Generics: TRestOrmGenerics;
    {$endif ISDELPHI2010}
    /// get a list of members from a SQL statement as RawJSON
    // - implements REST GET collection
    // - for better server speed, the WHERE clause should use bound parameters
    // identified as '?' in the FormatSQLWhere statement, which is expected to
    // follow the order of values supplied in BoundsSQLWhere open array - use
    // DateToSQL()/DateTimeToSQL() for TDateTime, or directly any integer,
    // double, currency, RawUTF8 values to be bound to the request as parameters
    // - aCustomFieldsCSV can be the CSV list of field names to be retrieved
    // - if aCustomFieldsCSV is '', will get all simple fields, excluding BLOBs
    // - if aCustomFieldsCSV is '*', will get ALL fields, including ID and BLOBs
    // - returns the raw JSON array content with all items on success, with
    // our expanded / not expanded JSON format - so can be used with SOA methods
    // and RawJSON results, for direct process from the client side
    // - returns '' on error
    // - the data is directly retrieved from raw JSON as returned by the database
    // without any conversion, so this method will be the fastest, but complex
    // types like dynamic array will be returned as Base64-encoded blob value -
    // if you need proper JSON access to those, see RetrieveDocVariantArray()
    function RetrieveListJSON(Table: TOrmClass;
      const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const;
      const aCustomFieldsCSV: RawUTF8 = ''; aForceAJAX: boolean = false): RawJSON; overload;
    /// get a list of members from a SQL statement as RawJSON
    // - implements REST GET collection
    // - this overloaded version expect the SQLWhere clause to be already
    // prepared with inline parameters using a previous FormatUTF8() call
    // - aCustomFieldsCSV can be the CSV list of field names to be retrieved
    // - if aCustomFieldsCSV is '', will get all simple fields, excluding BLOBs
    // - if aCustomFieldsCSV is '*', will get ALL fields, including ID and BLOBs
    // - returns the raw JSON array content with all items on success, with
    // our expanded / not expanded JSON format - so can be used with SOA methods
    // and RawJSON results, for direct process from the client side
    // - returns '' on error
    // - the data is directly retrieved from raw JSON as returned by the database
    // without any conversion, so this method will be the fastest, but complex
    // types like dynamic array will be returned as Base64-encoded blob value -
    // if you need proper JSON access to those, see RetrieveDocVariantArray()
    function RetrieveListJSON(Table: TOrmClass;
      const SQLWhere: RawUTF8; const aCustomFieldsCSV: RawUTF8 = '';
      aForceAJAX: boolean = false): RawJSON; overload;
    /// get a list of all members from a SQL statement as a TDocVariant
    // - implements REST GET collection
    // - if ObjectName='', it will return a TDocVariant of dvArray kind
    // - if ObjectName is set, it will return a TDocVariant of dvObject kind,
    // with one property containing the array of values: this returned variant
    // can be pasted e.g. directly as parameter to TSynMustache.Render()
    // - aCustomFieldsCSV can be the CSV list of field names to be retrieved
    // - if aCustomFieldsCSV is '', will get all simple fields, excluding BLOBs
    // - if aCustomFieldsCSV is '*', will get ALL fields, including ID and BLOBs
    // - the data will be converted to variants and TDocVariant following the
    // TOrm layout, so complex types like dynamic array will be returned
    // as a true array of values (in contrast to the RetrieveListJSON method)
    // - warning: under FPC, we observed that assigning the result of this
    // method to a local variable may circumvent a memory leak FPC bug
    function RetrieveDocVariantArray(Table: TOrmClass;
      const ObjectName, CustomFieldsCSV: RawUTF8;
      FirstRecordID: PID = nil; LastRecordID: PID = nil): variant; overload;
    /// get a list of members from a SQL statement as a TDocVariant
    // - implements REST GET collection over a specified WHERE clause
    // - if ObjectName='', it will return a TDocVariant of dvArray kind
    // - if ObjectName is set, it will return a TDocVariant of dvObject kind,
    // with one property containing the array of values: this returned variant
    // can be pasted e.g. directly as parameter to TSynMustache.Render()
    // - for better server speed, the WHERE clause should use bound parameters
    // identified as '?' in the FormatSQLWhere statement, which is expected to
    // follow the order of values supplied in BoundsSQLWhere open array - use
    // DateToSQL()/DateTimeToSQL() for TDateTime, or directly any integer,
    // double, currency, RawUTF8 values to be bound to the request as parameters
    // - aCustomFieldsCSV can be the CSV list of field names to be retrieved
    // - if aCustomFieldsCSV is '', will get all simple fields, excluding BLOBs
    // - if aCustomFieldsCSV is '*', will get ALL fields, including ID and BLOBs
    // - the data will be converted to variants and TDocVariant following the
    // TOrm layout, so complex types like dynamic array will be returned
    // as a true array of values (in contrast to the RetrieveListJSON method)
    // - warning: under FPC, we observed that assigning the result of this
    // method to a local variable may circumvent a memory leak FPC bug
    function RetrieveDocVariantArray(Table: TOrmClass;
      const ObjectName: RawUTF8; const FormatSQLWhere: RawUTF8;
      const BoundsSQLWhere: array of const; const CustomFieldsCSV: RawUTF8;
      FirstRecordID: PID = nil; LastRecordID: PID = nil): variant; overload;
    /// get all values of a SQL statement on a single column as a TDocVariant array
    // - implements REST GET collection on a single field
    // - for better server speed, the WHERE clause should use bound parameters
    // identified as '?' in the FormatSQLWhere statement, which is expected to
    // follow the order of values supplied in BoundsSQLWhere open array - use
    // DateToSQL()/DateTimeToSQL() for TDateTime, or directly any integer,
    // double, currency, RawUTF8 values to be bound to the request as parameters
    // - the data will be converted to variants and TDocVariant following the
    // TOrm layout, so complex types like dynamic array will be returned
    // as a true array of values (in contrast to the RetrieveListJSON method)
    function RetrieveOneFieldDocVariantArray(Table: TOrmClass;
      const FieldName, FormatSQLWhere: RawUTF8;
      const BoundsSQLWhere: array of const): variant;
    /// get one member from a SQL statement as a TDocVariant
    // - implements REST GET collection
    // - the data will be converted to a TDocVariant variant following the
    // TOrm layout, so complex types like dynamic array will be returned
    // as a true array of values
    function RetrieveDocVariant(Table: TOrmClass;
      const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const;
      const CustomFieldsCSV: RawUTF8): variant;
    /// get a list of members from a SQL statement as T*ObjArray
    // - implements REST GET collection
    // - for better server speed, the WHERE clause should use bound parameters
    // identified as '?' in the FormatSQLWhere statement, which is expected to
    // follow the order of values supplied in BoundsSQLWhere open array - use
    // DateToSQL()/DateTimeToSQL() for TDateTime, or directly any integer,
    // double, currency, RawUTF8 values to be bound to the request as parameters
    // - aCustomFieldsCSV can be the CSV list of field names to be retrieved
    // - if aCustomFieldsCSV is '', will get all simple fields, excluding BLOBs
    // - if aCustomFieldsCSV is '*', will get ALL fields, including ID and BLOBs
    // - set the T*ObjArray variable with all items on success - so that it can
    // be used with SOA methods
    // - it is up to the caller to ensure that ObjClear(ObjArray) is called
    // when the T*ObjArray list is not needed any more
    // - returns true on success, false on error
    function RetrieveListObjArray(var ObjArray; Table: TOrmClass;
      const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const;
      const aCustomFieldsCSV: RawUTF8 = ''): boolean;
    /// get and append a list of members as an expanded JSON array
    // - implements REST GET collection
    // - generates '[{rec1},{rec2},...]' using a loop similar to:
    // ! while FillOne do .. AppendJsonObject() ..
    // - for better server speed, the WHERE clause should use bound parameters
    // identified as '?' in the FormatSQLWhere statement, which is expected to
    // follow the order of values supplied in BoundsSQLWhere open array - use
    // DateToSQL()/DateTimeToSQL() for TDateTime, or directly any integer,
    // double, currency, RawUTF8 values to be bound to the request as parameters
    // - if OutputFieldName is set, the JSON array will be written as a JSON,
    // property i.e. surrounded as '"OutputFieldName":[....],' - note ending ','
    // - CustomFieldsCSV can be the CSV list of field names to be retrieved
    // - if CustomFieldsCSV is '', will get all simple fields, excluding BLOBs
    // - if CustomFieldsCSV is '*', will get ALL fields, including ID and BLOBs
    // - is just a wrapper around TOrm.AppendFillAsJsonArray()
    procedure AppendListAsJsonArray(Table: TOrmClass;
      const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const;
      const OutputFieldName: RawUTF8; W: TJSONSerializer;
      const CustomFieldsCSV: RawUTF8 = '');
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
      const DataTableBlobFieldName: RawUTF8; RTreeTable: TOrmRTreeClass;
      const DataTableBlobField: RawByteString; var DataID: TIDDynArray): boolean;
    /// Execute directly a SQL statement, expecting a list of results
    // - return a result table on success, nil on failure
    // - will call EngineList() abstract method to retrieve its JSON content
    function ExecuteList(const Tables: array of TOrmClass;
      const SQL: RawUTF8): TOrmTable;
    /// Execute directly a SQL statement, expecting a list of results
    // - you should not have to use this method, but the ORM versions instead
    // - return a result set as JSON on success, '' on failure
    // - will call EngineList() abstract method to retrieve its JSON content
    function ExecuteJson(const Tables: array of TOrmClass;
      const SQL: RawUTF8; ForceAJAX: boolean = false;
      ReturnedRowCount: PPtrInt = nil): RawJSON;
    /// Execute directly a SQL statement, without any expected result
    // - implements POST SQL on ModelRoot URI
    // - return true on success
    // - will call EngineExecute() abstract method to run the SQL statement
    function Execute(const aSQL: RawUTF8): boolean;
    /// Execute directly a SQL statement with supplied parameters, with no result
    // - expect the same format as FormatUTF8() function, replacing all '%' chars
    // with Args[] values
    // - return true on success
    function ExecuteFmt(const SQLFormat: RawUTF8;
      const Args: array of const): boolean; overload;
    /// Execute directly a SQL statement with supplied parameters, with no result
    // - expect the same format as FormatUTF8() function, replacing all '%' chars
    // with Args[] values, and all '?' chars with Bounds[] (inlining them
    // with :(...): and auto-quoting strings)
    // - return true on success
    function ExecuteFmt(const SQLFormat: RawUTF8;
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
    function Add(Value: TOrm; const CustomCSVFields: RawUTF8;
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
    // - the aSimpleFields parameters must follow explicitely the order of published
    // properties of the supplied aTable class, excepting the RawBlob and
    // TOrmMany kind (i.e. only so called "simple fields")
    // - the aSimpleFields must have exactly the same count of parameters as
    // there are "simple fields" in the published properties
    // - if ForcedID is set to non null, client sends this ID to be used
    // when adding the record (instead of a database-generated ID)
    // - on success, returns the new RowID value; on error, returns 0
    // - call internaly the Add virtual method above
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
    // could be computed by TOrmProperties.FieldBitsFromCSV()
    // or TOrmProperties.FieldBitsFromRawUTF8()
    // - this method will always compute and send any TModTime fields
    // - this method will call EngineUpdate() to perform the request
    function Update(Value: TOrm; const CustomFields: TFieldBits = [];
      DoNotAutoComputeFields: boolean = false): boolean; overload;
    /// update a member from Value simple fields content
    // - implements REST PUT collection
    // - return true on success
    // - is an overloaded method to Update(Value,FieldBitsFromCSV())
    function Update(Value: TOrm; const CustomCSVFields: RawUTF8;
      DoNotAutoComputeFields: boolean = false): boolean; overload;
    /// update a member from a supplied list of simple field values
    // - implements REST PUT collection
    // - the aSimpleFields parameters MUST follow explicitely both count and
    // order of published properties of the supplied aTable class, excepting the
    // RawBlob and TOrmMany kind (i.e. only so called "simple fields")
    // - return true on success
    // - call internaly the Update() / EngineUpdate() virtual methods
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
    // - call internaly the EngineUpdateField() abstract method
    // - note that this method won't update the TModTime properties: you should
    // rather use a classic Retrieve()/FillPrepare() followed by Update(); but
    // it will notify the internal Cache
    function UpdateField(Table: TOrmClass; ID: TID;
      const FieldName: RawUTF8; const FieldValue: array of const): boolean; overload;
    /// update one field in one or several members, depending on a WHERE clause
    // - implements REST PUT collection with one field value on a one where value
    // - only one single field shall be specified in FieldValue, but could
    // be of any kind of value - for BLOBs, you should better use UpdateBlob()
    // - only one single field shall be specified in WhereFieldValue, but could
    // be of any kind of value - for security reasons, void WHERE clause will
    // be rejected
    // - return true on success
    // - call internaly the EngineUpdateField() abstract method
    // - note that this method won't update the TModTime properties: you should
    // rather use a classic Retrieve()/FillPrepare() followed by Update(); but
    // it will notify the internal Cache
    function UpdateField(Table: TOrmClass; const WhereFieldName: RawUTF8;
      const WhereFieldValue: array of const; const FieldName: RawUTF8;
      const FieldValue: array of const): boolean; overload;
    /// update one field in a given member with a value specified as variant
    // - implements REST PUT collection with one field value
    // - any value can be set in FieldValue, but for BLOBs, you should better
    // use UpdateBlob()
    // - return true on success
    // - call internaly the EngineUpdateField() abstract method
    // - note that this method won't update the TModTime properties: you should
    // rather use a classic Retrieve()/FillPrepare() followed by Update(); but
    // it will notify the internal Cache
    function UpdateField(Table: TOrmClass; ID: TID;
      const FieldName: RawUTF8; const FieldValue: variant): boolean; overload;
    /// update one field in one or several members, depending on a WHERE clause,
    // with both update and where values specified as variant
    // - implements REST PUT collection with one field value on a one where value
    // - any value can be set in FieldValue, but for BLOBs, you should better
    // use UpdateBlob()
    // - for security reasons, void WHERE clause will be rejected
    // - return true on success
    // - call internaly the EngineUpdateField() abstract method
    // - note that this method won't update the TModTime properties, nor the
    // internal table Cache: you should rather use a classic Retrieve()/FillPrepare()
    // followed by an Update() of the whole record
    function UpdateField(Table: TOrmClass;
      const WhereFieldName: RawUTF8; const WhereFieldValue: variant;
      const FieldName: RawUTF8; const FieldValue: variant): boolean; overload;
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
      const FieldName: RawUTF8; const FieldValue: variant): boolean; overload;
    /// increments one integer field value
    // - if available, this method will use atomic value modification, e.g.
    // $ UPDATE table SET field=field+?
    function UpdateFieldIncrement(Table: TOrmClass; ID: TID;
      const FieldName: RawUTF8; Increment: Int64 = 1): boolean;
    /// override this method to guess if this record can be updated or deleted
    // - this default implementation returns always true
    // - e.g. you can add digital signature to a record to disallow record editing
    // - the ErrorMsg can be set to a variable, which will contain an explicit
    // error message
    function RecordCanBeUpdated(Table: TOrmClass; ID: TID;
      Action: TOrmEvent; ErrorMsg: PRawUTF8 = nil): boolean;
    /// delete a member
    // - implements REST DELETE collection
    // - return true on success
    // - call internaly the EngineDelete() abstract method
    function Delete(Table: TOrmClass; ID: TID): boolean; overload;
    /// delete a member with a WHERE clause
    // - implements REST DELETE collection
    // - return true on success
    // - this default method call OneFieldValues() to retrieve all matching IDs,
    // then will delete each row using protected EngineDeleteWhere() virtual method
    function Delete(Table: TOrmClass; const SQLWhere: RawUTF8): boolean; overload;
    /// delete a member with a WHERE clause
    // - implements REST DELETE collection
    // - return true on success
    // - for better server speed, the WHERE clause should use bound parameters
    // identified as '?' in the FormatSQLWhere statement, which is expected to
    // follow the order of values supplied in BoundsSQLWhere open array - use
    // DateToSQL/DateTimeToSQL for TDateTime, or directly any integer / double /
    // currency / RawUTF8 values to be bound to the request as parameters
    // - is a simple wrapper around:
    // ! Delete(Table, FormatUTF8(FormatSQLWhere, [], BoundsSQLWhere))
    function Delete(Table: TOrmClass;
      const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const): boolean; overload;

    /// get a blob field content from its record ID and supplied blob field name
    // - implements REST GET collection with a supplied member ID and a blob field name
    // - return true on success
    // - this method is defined as abstract, i.e. there is no default implementation:
    // it must be implemented 100% RestFul with a
    // GET ModelRoot/TableName/TableID/BlobFieldName request for example
    // - this method retrieve the blob data as a RawBlob string using
    // EngineRetrieveBlob()
    function RetrieveBlob(Table: TOrmClass;
      aID: TID; const BlobFieldName: RawUTF8;
      out BlobData: RawBlob): boolean; overload;
    /// get a blob field content from its record ID and supplied blob field name
    // - implements REST GET collection with a supplied member ID and field name
    // - return true on success
    // - this method will create a TStream instance (which must be freed by the
    // caller after use) and fill it with the blob data
    function RetrieveBlob(Table: TOrmClass;
      aID: TID; const BlobFieldName: RawUTF8;
      out BlobStream: TCustomMemoryStream): boolean; overload;
    /// update a blob field from its record ID and supplied blob field name
    // - implements REST PUT collection with a supplied member ID and field name
    // - return true on success
    // - call internaly the EngineUpdateBlob() abstract method
    // - this method expect the Blob data to be supplied as RawBlob, using
    // EngineUpdateBlob()
    function UpdateBlob(Table: TOrmClass;
      aID: TID; const BlobFieldName: RawUTF8; const BlobData: RawBlob): boolean; overload;
    /// update a blob field from its record ID and blob field name
    // - implements REST PUT collection with a supplied member ID and field name
    // - return true on success
    // - call internaly the EngineUpdateBlob() abstract method
    // - this method expect the Blob data to be supplied as a TStream: it will
    // send the whole stream content (from its beginning position upto its
    // current size) to the database engine
    function UpdateBlob(Table: TOrmClass;
      aID: TID; const BlobFieldName: RawUTF8; BlobData: TStream): boolean; overload;
    /// update a blob field from its record ID and blob field name
    // - implements REST PUT collection with a supplied member ID and field name
    // - return true on success
    // - call internaly the EngineUpdateBlob() abstract method
    // - this method expect the Blob data to be supplied as direct memory pointer
    // and size
    function UpdateBlob(Table: TOrmClass;
      aID: TID; const BlobFieldName: RawUTF8;
      BlobData: pointer; BlobSize: integer): boolean; overload;
    /// update all BLOB fields of the supplied Value
    // - call several REST PUT collection (one for each BLOB) for the member
    // - uses the UpdateBlob() method to send the BLOB properties content to the Server
    // - called internaly by Add and Update methods when ForceBlobTransfert /
    // ForceBlobTransfertTable[] is set
    // - you can use this method by hand, to avoid several calls to UpdateBlob()
    // - returns TRUE on success (or if there is no BLOB field)
    // - returns FALSE on error (e.g. if Value is invalid or with db/transmission)
    function UpdateBlobFields(Value: TOrm): boolean;
    /// get all BLOB fields of the supplied value from the remote server
    // - call several REST GET collection (one for each BLOB) for the member
    // - call internaly e.g. by TRestClient.Retrieve method when
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
    function TransactionBegin(aTable: TOrmClass; SessionID: cardinal): boolean;
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
    procedure Commit(SessionID: cardinal; RaiseException: boolean = false);
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
    procedure RollBack(SessionID: cardinal);
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
    function BatchSend(Table: TOrmClass; var Data: RawUTF8;
       var Results: TIDDynArray; ExpectedResultsCount: integer): integer; overload;
    /// prepare an asynchronous ORM BATCH process, executed in a background thread
    // - will initialize a TRestBatch and call TimerEnable to initialize the
    // background thread, following the given processing period (in seconds),
    // or the TRestBatch.Count threshold to call BatchSend
    // - actual REST/CRUD commands will take place via AsynchBatchAdd,
    // AsynchBatchUpdate and AsynchBatchDelete methods
    // - only a single AsynchBatch() call per Table is allowed at a time, unless
    // AsynchBatchStop method is used to flush the current asynchronous BATCH
    // - using a BATCH in a dedicated thread will allow very fast background
    // asynchronous process of ORM methods, sufficient for most use cases
    // - is a wrapper around BackgroundTimer.AsynchBatchStart()
    function AsynchBatchStart(Table: TOrmClass; SendSeconds: integer;
      PendingRowThreshold: integer = 500; AutomaticTransactionPerRow: integer = 1000;
      Options: TRestBatchOptions = [boExtendedJSON]): boolean;
    /// finalize asynchronous ORM BATCH process, executed in a background thread
    // - should have been preceded by a call to AsynchBatch(), or returns false
    // - Table=nil will release all existing batch instances
    // - is a wrapper around BackgroundTimer.AsynchBatchStop()
    function AsynchBatchStop(Table: TOrmClass): boolean;
    /// create a new ORM member in a BATCH to be written in a background thread
    // - should have been preceded by a call to AsynchBatchStart(), or returns -1
    // - is a wrapper around BackgroundTimer.AsynchBatchAdd(),
    // so will return the index in the BATCH rows, not the created TID
    // - this method is thread-safe
    function AsynchBatchAdd(Value: TOrm; SendData: boolean;
      ForceID: boolean = false; const CustomFields: TFieldBits = [];
      DoNotAutoComputeFields: boolean = false): integer;
    /// append some JSON content in a BATCH to be written in a background thread
    // - could be used to emulate AsynchBatchAdd() with an already pre-computed
    // JSON object
    // - is a wrapper around BackgroundTimer.AsynchBatchRawAdd(),
    // so will return the index in the BATCH rows, not the created TID
    // - this method is thread-safe
    function AsynchBatchRawAdd(Table: TOrmClass; const SentData: RawUTF8): integer;
    /// append some JSON content in a BATCH to be writen in a background thread
    // - could be used to emulate AsynchBatchAdd() with an already pre-computed
    // JSON object, as stored in a TTextWriter instance
    // - is a wrapper around BackgroundTimer.AsynchBatchRawAppend()
    // - this method is thread-safe
    procedure AsynchBatchRawAppend(Table: TOrmClass; SentData: TTextWriter);
    /// update an ORM member in a BATCH to be written in a background thread
    // - should have been preceded by a call to AsynchBatchStart(), or returns -1
    // - is a wrapper around BackgroundTimer.AsynchBatchUpdate()
    // - this method is thread-safe
    function AsynchBatchUpdate(Value: TOrm; const CustomFields: TFieldBits = [];
      DoNotAutoComputeFields: boolean = false): integer;
    /// delete an ORM member in a BATCH to be written in a background thread
    // - should have been preceded by a call to AsynchBatchStart(), or returns -1
    // - is a wrapper around the TRestBatch.Delete() sent in the Timer thread
    // - this method is thread-safe
    function AsynchBatchDelete(Table: TOrmClass; ID: TID): integer;

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
    procedure InternalLog(const Text: RawUTF8; Level: TSynLogInfo); overload;
    /// log the corresponding text (if logging is enabled)
    procedure InternalLog(const Format: RawUTF8; const Args: array of const;
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
    // - only working types are TOrmTableJSON and TOrm descendants
    // - make use of the InternalState function to check the data content revision
    // - return true if Data is updated successfully, or false on any error
    // during data retrieval from server (e.g. if the TOrm has been deleted)
    // - if Data contains only one TOrmTableJSON, PCurrentRow can point to the
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
    // - optional SQLSelect parameter to change the returned fields
    // as in 'SELECT SQLSelect FROM TableName;'
    // - optional SQLWhere parameter to change the search range or ORDER
    // as in 'SELECT SQLSelect FROM TableName WHERE SQLWhere;'
    // - using inlined parameters via :(...): in SQLWhere is always a good idea
    // - for one TClass, you should better use TRest.MultiFieldValues()
    function List(const Tables: array of TOrmClass;
      const SQLSelect: RawUTF8 = 'RowID';
      const SQLWhere: RawUTF8 = ''): TOrmTable;
    /// retrieve a list of members as a TOrmTable
    // - implements REST GET collection
    // - in this version, the WHERE clause can be created with the same format
    // as FormatUTF8() function, replacing all '%' chars with Args[] values
    // - using inlined parameters via :(...): in SQLWhereFormat is always a good idea
    // - for one TClass, you should better use TRest.MultiFieldValues()
    // - will call the List virtual method internaly
    function ListFmt(const Tables: array of TOrmClass;
      const SQLSelect, SQLWhereFormat: RawUTF8;
      const Args: array of const): TOrmTable; overload;
    /// retrieve a list of members as a TOrmTable
    // - implements REST GET collection
    // - in this version, the WHERE clause can be created with the same format
    // as FormatUTF8() function, replacing all '%' chars with Args[], and all '?'
    // chars with Bounds[] (inlining them with :(...): and auto-quoting strings)
    // - example of use:
    // ! Table := ListFmt([TOrm],'Name','ID=?',[],[aID]);
    // - for one TClass, you should better use TRest.MultiFieldValues()
    // - will call the List virtual method internaly
    function ListFmt(const Tables: array of TOrmClass;
      const SQLSelect, SQLWhereFormat: RawUTF8;
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
    function BatchStart(aTable: TOrmClass;
      AutomaticTransactionPerRow: cardinal = 0;
      Options: TRestBatchOptions = []): boolean; 
    /// begin a BATCH sequence to speed up huge database change for any table
    // - will call the BatchStart() method with aTable = nil so that you may be
    // able to use any kind of TOrm class within the process
    // - is a wrapper around TRestBatch.Create() which will be stored in the
    // implementation class instance - be aware that this won't be thread-safe
    function BatchStartAny(AutomaticTransactionPerRow: cardinal;
      Options: TRestBatchOptions = []): boolean;
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
      DoNotAutoComputeFields: boolean = false): integer;
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
  end;

  /// Server-Specific Object-Relational-Mapping calls for CRUD access to a database
  IRestOrmServer = interface(IRestOrm)
    ['{F8FB2109-5629-4DFB-A74C-7A0F86F91362}']
    /// create an index for the specific FieldName
    // - will call CreateSQLMultiIndex() internaly
    function CreateSQLIndex(Table: TOrmClass; const FieldName: RawUTF8;
      Unique: boolean; const IndexName: RawUTF8 = ''): boolean; overload;
    /// create one or multiple index(es) for the specific FieldName(s)
    function CreateSQLIndex(Table: TOrmClass;
      const FieldNames: array of RawUTF8; Unique: boolean): boolean; overload;
    /// create one index for all specific FieldNames at once
    // - will call any static engine for the index creation of such tables, or
    // execute a CREATE INDEX IF NOT EXISTS on the main engine
    // - note that with SQLite3, your database schema should never contain two
    // indices where one index is a prefix of the other, e.g. if you defined:
    // ! aServer.CreateSQLMultiIndex(TEmails, ['Email','GroupID'], True);
    // Then the following index is not mandatory for SQLite3:
    // ! aServer.CreateSQLIndex(TEmails, 'Email', False);
    // see "1.6 Multi-Column Indices" in @http://www.sqlite.org/queryplanner.html
    function CreateSQLMultiIndex(Table: TOrmClass;
      const FieldNames: array of RawUTF8; Unique: boolean; IndexName: RawUTF8 = ''): boolean;
    /// check if the supplied TOrm is not a virtual or static table
    function IsInternalSQLite3Table(aTableIndex: integer): boolean;
    /// returns the maximum BLOB size per record as specified to TrackChanges()
    function MaxUncompressedBlobSize(Table: TOrmClass): integer;
    /// returns true if the server will handle per-user authentication and
    // access right management
    // - i.e. if the associated TOrmModel contains TAuthUser and
    // TAuthGroup tables (set by constructor)
    function HandleAuthentication: boolean;
  end;


  { -------------------- TOrm Definitions }

  /// the possible options for handling table names
  TOrmCheckTableName = (
    ctnNoCheck, ctnMustExist, ctnTrimExisting);

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
    fTableMap: array of record
      /// the class instance to be filled from the TOrmTable
      // - can be a TOrmMany instance after FillPrepareMany() method call
      Dest: TOrm;
      /// the published property RTTI to be filled from the TOrmTable
      // - is nil for the RowID/ID field
      DestField: TOrmPropInfo;
      /// the column index in TOrmTable
      TableIndex: integer;
    end;
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
      aIndex: integer); overload;
    /// add a property to the fTableMap[] array
    // - aIndex is the column index in TOrmTable
    procedure AddMap(aRecord: TOrm; const aFieldName: RawUTF8;
      aIndex: integer); overload;
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
    procedure Map(aRecord: TOrm; aTable: TOrmTable;
      aCheckTableName: TOrmCheckTableName);
    /// reset the mapping
    // - is called e.g. by TOrm.FillClose
    // - will free any previous Table if necessary
    // - will release TOrmMany.Dest instances as set by TOrm.FillPrepareMany()
    procedure UnMap;
    /// fill a TOrm published properties from a TOrmTable row
    // - use the mapping prepared with Map() method
    function Fill(aRow: integer): boolean; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// fill a TOrm published properties from a TOrmTable row
    // - use the mapping prepared with Map() method
    // - aTableRow will point to the first column of the matching row
    procedure Fill(aTableRow: PPUtf8CharArray); overload;
    /// fill a TOrm published properties from a TOrmTable row
    // - overloaded method using a specified destination record to be filled
    // - won't work with cross-reference mapping (FillPrepareMany)
    // - use the mapping prepared with Map() method
    // - aTableRow will point to the first column of the matching row
    procedure Fill(aTableRow: PPUtf8CharArray; aDest: TOrm); overload;
    /// fill a TOrm published properties from a TOrmTable row
    // - overloaded method using a specified destination record to be filled
    // - won't work with cross-reference mapping (FillPrepareMany)
    // - use the mapping prepared with Map() method
    function Fill(aRow: integer; aDest: TOrm): boolean; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// used to compute the updated field bits during a fill
    // - will return Props.SimpleFieldsBits[ooUpdate] if no fill is in process
    procedure ComputeSetUpdatedFieldBits(Props: TOrmProperties;
      out Bits: TFieldBits);
    /// the TOrmTable stated as FillPrepare() parameter
    // - the internal temporary table is stored here for TOrmMany
    // - this instance is freed by TOrm.Destroy if fTable.OwnerMustFree=true
    property Table: TOrmTable read fTable;
    /// the current Row during a Loop
    property FillCurrentRow: integer read fFillCurrentRow;
    /// equals TRUE if the instance was initialized via TOrm.CreateJoined()
    // TOrm.CreateAndFillPrepareJoined()
    // - it means that all nested TOrm are pre-allocated instances,
    // not trans-typed pointer(IDs)
    property JoinedFields: boolean read GetJoinedFields;
    /// set by TOrm.FillPrepareMany() to release M.fDestID^ instances
    property TableMapRecordManyInstances: TOrmManyObjArray
      read fTableMapRecordManyInstances;
    /// return all mapped fields, or [] if nil
    function TableMapFields: TFieldBits;
  end;


  /// the possible options for TRestServer.CreateMissingTables and
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
    itoNoAutoCreateGroups, itoNoAutoCreateUsers, itoNoCreateMissingField,
    itoNoIndex4ID, itoNoIndex4UniqueField, itoNoIndex4NestedRecord,
    itoNoIndex4RecordReference, itoNoIndex4TID, itoNoIndex4RecordVersion);

  /// the options to be specified for TRestServer.CreateMissingTables and
  // TOrm.InitializeTable methods
  TOrmInitializeTableOptions = set of TOrmInitializeTableOption;


  /// root class for defining and mapping database records
  // - inherits a class from TOrm, and add published properties to describe
  // the table columns (see TPropInfo for SQL and Delphi type mapping/conversion)
  // - this published properties can be auto-filled from TOrmTable answer with
  // FillPrepare() and FillRow(), or FillFrom() with TOrmTable or JSON data
  // - these published properties can be converted back into UTF-8 encoded SQL
  // source with GetSQLValues or GetSQLSet or into JSON format with GetJSONValues
  // - BLOB fields are decoded to auto-freeing RawBlob properties
  // - any published property defined as a T*ObjArray dynamic array storage
  // of persistents (via Rtti.RegisterObjArray) will be freed
  // - consider inherit from TOrmNoCase and TOrmNoCaseExtended if
  // you expect regular NOCASE collation and smaller (but not standard JSON)
  // variant fields persistence
  TOrm = class(TSynPersistentWithID)
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
  protected
    fInternalState: cardinal;
    /// defined as a protected class function for RecordProps method inlining
    class function PropsCreate: TOrmProperties;
    /// virtual class method to be overridden to register some custom properties
    // - do nothing by default, but allow inherited classes to define some
    // properties, by adding some TOrmPropInfo instances to Props.Fields list,
    // or calling Props.RegisterCustomFixedSizeRecordProperty() or
    // Props.RegisterCustomRTTIRecordProperty() methods
    // - can also be used to specify a custom text collation, by calling
    // Props.SetCustomCollationForAll() or SetCustomCollation() methods
    // - do not call RecordProps from here (e.g. by calling AddFilter*): it
    // woult trigger a stack overflow, since at this state Props is not stored -
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
      {$ifdef MSWINDOWS}{$ifdef HASINLINE}inline;{$endif}{$endif}
    /// trick to typecast the ID on 64-bit platform
    function GetIDAsPointer: pointer;
      {$ifdef MSWINDOWS}{$ifdef HASINLINE}inline;{$endif}{$endif}
  public
    /// direct access to the TOrm properties from RTTI
    // - TOrmProperties is faster than e.g. the class function FieldProp()
    // - use internal the unused vmtAutoTable VMT entry to fast retrieve of a
    // class variable which is unique for each class ("class var" is unique only
    // for the class within it is defined, and we need a var for each class:
    // so even Delphi XE syntax is not powerful enough for our purpose, and the
    // vmtAutoTable trick if very fast, and works with all versions of Delphi -
    // including 64-bit target)
    class function RecordProps: TOrmProperties;
      {$ifdef HASINLINE}inline;{$endif}
    /// the Table name in the database, associated with this TOrm class
    // - 'TSQL' or 'TOrm' chars are trimmed at the beginning of the ClassName
    // - or the ClassName is returned as is, if no 'TSQL' or 'TOrm' at first
    // - is just a wrapper around RecordProps.SQLTableName field value
    class function SQLTableName: RawUTF8;
      {$ifdef HASINLINE}inline;{$endif}
    /// register a custom filter (transformation) or validate to the
    // TOrm class for a specified field
    // - this will be used by TOrm.Filter and TOrm.Validate
    // methods (in default implementation)
    // - will raise an EModelException on failure
    // - this function is just a wrapper around RecordProps.AddFilterOrValidate
    class procedure AddFilterOrValidate(const aFieldName: RawUTF8;
      aFilter: TSynFilterOrValidate);
    /// register a TSynFilterTrim and a TSynValidateText filters so that
    // the specified fields, after space trimming, won't be void
    class procedure AddFilterNotVoidText(const aFieldNames: array of RawUTF8);
    /// register a TSynFilterTrim and a TSynValidateText filters so that
    // all text fields, after space trimming, won't be void
    // - will only affect RAWTEXT_FIELDS
    class procedure AddFilterNotVoidAllTextFields;

    /// protect several TOrm local variable instances
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
    class function AutoFree(varClassPairs: array of pointer): IAutoFree; overload;
    /// protect one TOrm local variable instance
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
    /// read and protect one TOrm local variable instance
    // - is a wrapper around TAutoFree.Create(localVariable,Create(Rest,ID))
    // - be aware that it won't implement a full ARC memory model, but may be
    // just used to avoid writing some try ... finally blocks on local variables
    // - use with caution, only on well defined local scope
    // - warning: under FPC, you should assign the result of this method to
    // a local IAutoFree variable, or use a with TOrm.AutoFree() do
    // statement - see http://bugs.freepascal.org/view.php?id=26602
    class function AutoFree(var localVariable; const Rest: IRestOrm; ID: TID): IAutoFree; overload;
    /// FillPrepare and protect one TOrm local variable instance
    // - is a wrapper around TAutoFree.Create(localVariable,CreateAndFillPrepare(Rest,...))
    // - be aware that it won't implement a full ARC memory model, but may be
    // just used to avoid writing some try ... finally blocks on local variables
    // - use with caution, only on well defined local scope
    // - warning: under FPC, you should assign the result of this method to
    // a local IAutoFree variable, or use a with TOrm.AutoFree() do
    // statement - see http://bugs.freepascal.org/view.php?id=26602
    class function AutoFree(var localVariable; const Rest: IRestOrm;
      const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const;
      const aCustomFieldsCSV: RawUTF8 = ''): IAutoFree; overload;
    /// FillPrepare and protect one TOrm local variable instance
    // - is a wrapper around TAutoFree.Create(localVariable,CreateAndFillPrepare(Rest,...))
    // - be aware that it won't implement a full ARC memory model, but may be
    // just used to avoid writing some try ... finally blocks on local variables
    // - use with caution, only on well defined local scope
    // - warning: under FPC, you should assign the result of this method to
    // a local IAutoFree variable, or use a with TOrm.AutoFree() do
    // statement - see http://bugs.freepascal.org/view.php?id=26602
    class function AutoFree(var localVariable; const Rest: IRestOrm;
      const FormatSQLWhere: RawUTF8; const ParamsSQLWhere, BoundsSQLWhere: array of const;
      const aCustomFieldsCSV: RawUTF8 = ''): IAutoFree; overload;

    /// virtual method called when the associated table is created in the database
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
      const FieldName: RawUTF8; Options: TOrmInitializeTableOptions); virtual;

    /// filter/transform the specified fields values of the TOrm instance
    // - by default, this will perform all TSynFilter as registered by
    // [RecordProps.]AddFilterOrValidate()
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
    function Filter(const aFields: array of RawUTF8): boolean; overload;
    /// validate the specified fields values of the current TOrm instance
    // - by default, this will perform all TSynValidate as registered by
    //  [RecordProps.]AddFilterOrValidate()
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
    function Validate(const aRest: IRestOrm; const aFields: array of RawUTF8;
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
      aValidator: PSynValidate = nil): RawUTF8; overload;
    /// should modify the record content before writing to the Server
    // - this default implementation will update any oftModTime / TModTime,
    // oftCreateTime / TCreateTime and oftSessionUserID / TSessionUserID
    // properties content with the exact server time stamp
    // - you may override this method e.g. for custom calculated fields
    // - note that this is computed only on the Client side, before sending
    // back the content to the remote Server: therefore, TModTime / TCreateTime
    // fields are a pure client ORM feature - it won't work directly at REST level
    procedure ComputeFieldsBeforeWrite(const aRest: IRestOrm;
      aOccasion: TOrmEvent); virtual;

    /// this constructor initializes the record
    // - auto-instanciate any TOrmMany instance defined in published properties
    // - override this method if you want to use some internal objects (e.g.
    // TStringList or TCollection as published property)
    constructor Create; overload; override;
    /// this constructor initializes the record and set the simple fields
    // with the supplied values
    // - the aSimpleFields parameters must follow explicitely the order of
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
    //  for better server speed - note that you can use FormatUTF8() as such:
    //  ! aRec := TOrmMyRec.Create(Client,FormatUTF8('Salary>? AND Salary<?',[],[1000,2000]));
    //  or call the overloaded contructor with BoundsSQLWhere array of parameters
    constructor Create(const aClient: IRestOrm; const aSQLWhere: RawUTF8);
      reintroduce; overload;
    /// this constructor initializes the object as above, and fills its content
    // from a client or server connection, using a specified WHERE clause
    // with parameters
    // - for better server speed, the WHERE clause should use bound parameters
    // identified as '?' in the FormatSQLWhere statement, which is expected to
    // follow the order of values supplied in BoundsSQLWhere open array - use
    // DateToSQL/DateTimeToSQL for TDateTime, or directly any integer / double /
    // currency / RawUTF8 values to be bound to the request as parameters
    // - note that this method prototype changed with revision 1.17 of the
    // framework: array of const used to be ParamsSQLWhere and '%' in the
    // FormatSQLWhere statement, whereas it now expects bound parameters as '?'
    constructor Create(const aClient: IRestOrm; const FormatSQLWhere: RawUTF8;
      const BoundsSQLWhere: array of const); reintroduce; overload;
    /// this constructor initializes the object as above, and fills its content
    // from a client or server connection, using a specified WHERE clause
    // with parameters
    // - the FormatSQLWhere clause will replace all '%' chars with the supplied
    // ParamsSQLWhere[] values, and all '?' chars with BoundsSQLWhere[] values,
    // as :(...): inlined parameters - you should either call:
    // ! Rec := TOrmMyRecord.Create(aClient, 'Count=:(%):', [aCount],[]);
    // or (letting the inlined parameters being computed by FormatUTF8)
    // !  Rec := TOrmMyRecord.Create(aClient,'Count=?',[],[aCount]);
    // or even better, using the other Create overloaded constructor:
    // !  Rec := TOrmMyRecord.Create(aClient,'Count=?',[aCount]);
    // - using '?' and BoundsSQLWhere[] is perhaps more readable in your code, and
    // will in all case create a request with :(..): inline parameters, with
    // automatic RawUTF8 quoting if necessary
    constructor Create(const aClient: IRestOrm; const FormatSQLWhere: RawUTF8;
      const ParamsSQLWhere, BoundsSQLWhere: array of const); reintroduce; overload;
    /// this constructor initializes the object as above, and fills its content
    // from a supplied JSON content
    // - is a wrapper around Create + FillFrom() methods
    // - use JSON data, as exported by GetJSONValues(), expanded or not
    // - make an internal copy of the JSONTable RawUTF8 before calling
    // FillFrom() below
    constructor CreateFrom(const JSONRecord: RawUTF8); overload;
    /// this constructor initializes the object as above, and fills its content
    // from a supplied JSON buffer
    // - is a wrapper around Create + FillFrom() methods
    // - use JSON data, as exported by GetJSONValues(), expanded or not
    // - the data inside P^ is modified (unescaped and transformed in-place):
    // don't call CreateFrom(pointer(JSONRecord)) but CreateFrom(JSONRecord) which
    // makes a temporary copy of the JSONRecord text variable before parsing
    constructor CreateFrom(P: PUTF8Char); overload;
    /// this constructor initializes the object as above, and fills its content
    // from a supplied TDocVariant object document
    // - is a wrapper around Create + FillFrom() methods
    constructor CreateFrom(const aDocVariant: variant); overload;

    /// this constructor initializes the object as above, and prepares itself to
    // loop through a statement using a specified WHERE clause
    // - this method creates a TOrmTableJSON, retrieves all records corresponding
    // to the WHERE clause, then call FillPrepare - previous Create(aClient)
    // methods retrieve only one record, this one more multiple rows
    // - you should then loop for all rows using 'while Rec.FillOne do ...'
    // - the TOrmTableJSON will be freed by TOrm.Destroy
    // - the WHERE clause should use inlined parameters (like 'Name=:('Arnaud'):')
    // for better server speed - note that you can use FormatUTF8() as such:
    // ! aRec := TOrmMyRec.CreateAndFillPrepare(Client,FormatUTF8('Salary>? AND Salary<?',[],[1000,2000]));
    // or call the overloaded CreateAndFillPrepare() contructor directly with
    // BoundsSQLWhere array of parameters
    // - aCustomFieldsCSV can be used to specify which fields must be retrieved
    // - default aCustomFieldsCSV='' will retrieve all simple table fields
    // - if aCustomFieldsCSV='*', it will retrieve all fields, including BLOBs
    // - aCustomFieldsCSV can also be set to a CSV field list to retrieve only
    // the needed fields, and save remote bandwidth - note that any later
    // Update() will update all simple fields, so potentially with wrong
    // values; but BatchUpdate() can be safely used since it will
    constructor CreateAndFillPrepare(const aClient: IRestOrm;
      const aSQLWhere: RawUTF8; const aCustomFieldsCSV: RawUTF8 = ''); overload;
    /// this constructor initializes the object as above, and prepares itself to
    // loop through a statement using a specified WHERE clause
    // - this method creates a TOrmTableJSON, retrieves all records corresponding
    // to the WHERE clause, then call FillPrepare - previous Create(aClient)
    // methods retrieve only one record, this one more multiple rows
    // - you should then loop for all rows using 'while Rec.FillOne do ...'
    // - the TOrmTableJSON will be freed by TOrm.Destroy
    // - for better server speed, the WHERE clause should use bound parameters
    // identified as '?' in the FormatSQLWhere statement, which is expected to
    // follow the order of values supplied in BoundsSQLWhere open array - use
    // DateToSQL/DateTimeToSQL for TDateTime, or directly any integer / double /
    // currency / RawUTF8 values to be bound to the request as parameters
    // - note that this method prototype changed with revision 1.17 of the
    // framework: array of const used to be ParamsSQLWhere and '%' in the
    // FormatSQLWhere statement, whereas it now expects bound parameters as '?'
    // - aCustomFieldsCSV can be used to specify which fields must be retrieved
    // - default aCustomFieldsCSV='' will retrieve all simple table fields, but
    // you may need  to access only one or several fields, and will save remote
    // bandwidth by specifying the needed fields
    // - if aCustomFieldsCSV='*', it will retrieve all fields, including BLOBs
    // - note that you should not use this aCustomFieldsCSV optional parameter if
    // you want to Update the retrieved record content later, since any
    // missing fields will be left with previous values - but BatchUpdate() can be
    // safely used after FillPrepare (will set only ID, TModTime and mapped fields)
    constructor CreateAndFillPrepare(const aClient: IRestOrm;
      const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const;
      const aCustomFieldsCSV: RawUTF8 = ''); overload;
    /// this constructor initializes the object as above, and prepares itself to
    // loop through a statement using a specified WHERE clause
    // - this method creates a TOrmTableJSON, retrieves all records corresponding
    // to the WHERE clause, then call FillPrepare - previous Create(aClient)
    // methods retrieve only one record, this one more multiple rows
    // - you should then loop for all rows using 'while Rec.FillOne do ...'
    // - the TOrmTableJSON will be freed by TOrm.Destroy
    // - the FormatSQLWhere clause will replace all '%' chars with the supplied
    // ParamsSQLWhere[] supplied values, and bind all '?' chars as parameters
    // with BoundsSQLWhere[] values
    // - aCustomFieldsCSV can be used to specify which fields must be retrieved
    // - default aCustomFieldsCSV='' will retrieve all simple table fields, but
    // you may need  to access only one or several fields, and will save remote
    // bandwidth by specifying the needed fields
    // - if aCustomFieldsCSV='*', it will retrieve all fields, including BLOBs
    // - note that you should not use this aCustomFieldsCSV optional parameter if
    // you want to Update the retrieved record content later, since any
    // missing fields will be left with previous values - but BatchUpdate() can be
    // safely used after FillPrepare (will set only ID, TModTime and mapped fields)
    constructor CreateAndFillPrepare(const aClient: IRestOrm;
      const FormatSQLWhere: RawUTF8; const ParamsSQLWhere, BoundsSQLWhere: array of const;
      const aCustomFieldsCSV: RawUTF8 = ''); overload;
    /// this constructor initializes the object as above, and prepares itself to
    // loop through a given list of IDs
    // - this method creates a TOrmTableJSON, retrieves all records corresponding
    // to the specified IDs, then call FillPrepare - previous Create(aClient)
    // methods retrieve only one record, this one more multiple rows
    // - you should then loop for all rows using 'while Rec.FillOne do ...'
    // - the TOrmTableJSON will be freed by TOrm.Destroy
    // - aCustomFieldsCSV can be used to specify which fields must be retrieved
    // - default aCustomFieldsCSV='' will retrieve all simple table fields, but
    // you may need  to access only one or several fields, and will save remote
    // bandwidth by specifying the needed fields
    // - if aCustomFieldsCSV='*', it will retrieve all fields, including BLOBs
    // - note that you should not use this aCustomFieldsCSV optional parameter if
    // you want to Update the retrieved record content later, since any
    // missing fields will be left with previous values - but BatchUpdate() can be
    // safely used after FillPrepare (will set only ID, TModTime and mapped fields)
    constructor CreateAndFillPrepare(const aClient: IRestOrm;
      const aIDs: array of Int64; const aCustomFieldsCSV: RawUTF8 = ''); overload;
    /// this constructor initializes the object, and prepares itself to loop
    // through a specified JSON table, which will use a private copy
    // - this method creates a TOrmTableJSON, fill it with the supplied JSON buffer,
    // then call FillPrepare - previous Create(aClient) methods retrieve only
    // one record, this one more multiple rows
    // - you should then loop for all rows using 'while Rec.FillOne do ...'
    // - the TOrmTableJSON will be freed by TOrm.Destroy
    constructor CreateAndFillPrepare(const aJSON: RawUTF8); overload;
    /// this constructor initializes the object, and prepares itself to loop
    // through a specified JSON table buffer, which will be modified in-place
    // - this method creates a TOrmTableJSON, fill it with the supplied JSON buffer,
    // then call FillPrepare - previous Create(aClient) methods retrieve only
    // one record, this one more multiple rows
    // - you should then loop for all rows using 'while Rec.FillOne do ...'
    // - the TOrmTableJSON will be freed by TOrm.Destroy
    constructor CreateAndFillPrepare(aJSON: PUTF8Char; aJSONLen: integer); overload;
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
    //  - this method creates a TOrmTableJSON, fill it with the supplied JSON buffer,
    // then call FillPrepare - previous CreateJoined() method retrieve only
    // one record, this one more multiple rows
    // - you should then loop for all rows using 'while Rec.FillOne do ...'
    // - use this constructor if you want all TOrm published properties to
    // be allocated, and loaded with the corresponding values
    // - Free/Destroy will release these instances
    // - warning: if you call Update() after it, only the main object will be
    // updated, not the nested TOrm properties
    constructor CreateAndFillPrepareJoined(const aClient: IRestOrm;
      const aFormatSQLJoin: RawUTF8; const aParamsSQLJoin, aBoundsSQLJoin: array of const);
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
    // - you SHALL call explicitely the FillClose method before using any
    // methods of nested TOrmMany instances which may override the Dest
    // instance content (e.g. ManySelect) to avoid any GPF
    // - the aFormatSQLJoin clause will replace all '%' chars with the supplied
    // aParamsSQLJoin[] supplied values, and bind all '?' chars as bound
    // parameters with aBoundsSQLJoin[] values
    constructor CreateAndFillPrepareMany(const aClient: IRestOrm;
      const aFormatSQLJoin: RawUTF8; const aParamsSQLJoin, aBoundsSQLJoin: array of const);
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
    // - RawUnicode and RawUTF8 are created as TEXT COLLATE SYSTEMNOCASE
    // (i.e. use our fast UTF8IComp() for comparaison)
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
    // - anyway, don't call this method directly, but use TOrmModel.GetSQLCreate()
    // - the aModel parameter is used to retrieve the Virtual Table module name,
    // and can be ignored for regular (not virtual) tables
    class function GetSQLCreate(aModel: TOrmModel): RawUTF8; virtual;
    /// return the Class Type of the current TOrm
    function RecordClass: TOrmClass;
      {$ifdef HASINLINE}inline;{$endif}
    /// return the RTTI property information for this record
    function ClassProp: TRttiJson;
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
    function GetSQLValues: RawUTF8;
    /// return the UTF-8 encoded SQL source to UPDATE the values contained
    // in the current published fields of a TOrm child
    // - only simple fields name (i.e. not RawBlob/TOrmMany) are retrieved:
    // BLOB fields are ignored (use direct access via dedicated methods instead)
    // - format is 'COL1='VAL1', COL2='VAL2''
    // - is not used by the ORM (do not use prepared statements) - only here
    // for conveniency
    function GetSQLSet: RawUTF8;
    /// return the UTF-8 encoded JSON objects for the values of this TOrm
    // - layout and fields should have been set at TJSONSerializer construction:
    // to append some content to an existing TJsonSerializer, call the
    // AppendAsJsonObject() method
    procedure GetJSONValues(W: TJSONSerializer); overload;
    /// return the UTF-8 encoded JSON objects for the values of this TOrm
    // - the JSON buffer will be finalized if needed (e.g. non expanded mode),
  	// and the supplied TJSONSerializer instance will be freed by this method
    // - layout and fields should have been set at TJSONSerializer construction:
    // to append some content to an existing TJsonSerializer, call the
    // AppendAsJsonObject() method
    procedure GetJSONValuesAndFree(JSON: TJSONSerializer); overload;
    /// return the UTF-8 encoded JSON objects for the values contained
    // in the current published fields of a TOrm child
    // - only simple fields (i.e. not RawBlob/TOrmMany) are retrieved:
    //   BLOB fields are ignored (use direct access via dedicated methods instead)
    // - if Expand is true, JSON data is an object, for direct use with any Ajax or .NET client:
    // $ {"col1":val11,"col2":"val12"}
    // - if Expand is false, JSON data is serialized (as used in TOrmTableJSON)
    // $ { "fieldCount":1,"values":["col1","col2",val11,"val12",val21,..] }
    // - if withID is true, then the first ID field value is included
    // - you can customize OrmOptions, e.g. if oftObject/oftBlobDynArray
    // property instance will be serialized as a JSON object or array, not a
    // JSON string (which is the default, as expected by the database storage),
    // or if an "ID_str" string field should be added for JavaScript
    procedure GetJSONValues(JSON: TStream; Expand, withID: boolean;
      Occasion: TOrmOccasion; OrmOptions: TJSONSerializerOrmOptions = []); overload;
    /// same as overloaded GetJSONValues(), but returning result into a RawUTF8
    // - if UsingStream is not set, it will use a temporary THeapMemoryStream instance
    function GetJSONValues(Expand, withID: boolean;
      Occasion: TOrmOccasion; UsingStream: TCustomMemoryStream = nil;
      OrmOptions: TJSONSerializerOrmOptions = []): RawUTF8; overload;
    /// same as overloaded GetJSONValues(), but allowing to set the fields to
    // be retrieved, and returning result into a RawUTF8
    function GetJSONValues(Expand, withID: boolean; const Fields: TFieldBits;
      OrmOptions: TJSONSerializerOrmOptions = []): RawUTF8; overload;
    /// same as overloaded GetJSONValues(), but allowing to set the fields to
    // be retrieved, and returning result into a RawUTF8
    function GetJSONValues(Expand, withID: boolean; const FieldsCSV: RawUTF8;
      OrmOptions: TJSONSerializerOrmOptions = []): RawUTF8; overload;
    /// will append the record fields as an expanded JSON object
    // - GetJsonValues() will expect a dedicated TJSONSerializer, whereas this
    // method will add the JSON object directly to any TJSONSerializer
    // - by default, will append the simple fields, unless the Fields optional
    // parameter is customized to a non void value
    procedure AppendAsJsonObject(W: TJSONSerializer; Fields: TFieldBits);
    /// will append all the FillPrepare() records as an expanded JSON array
    // - generates '[{rec1},{rec2},...]' using a loop similar to:
    // ! while FillOne do .. AppendJsonObject() ..
    // - if FieldName is set, the JSON array will be written as a JSON property,
    // i.e. surrounded as '"FieldName":[....],' - note the ',' at the end
    // - by default, will append the simple fields, unless the Fields optional
    // parameter is customized to a non void value
    // - see also IRestOrm.AppendListAsJsonArray for a high-level wrapper method
    procedure AppendFillAsJsonArray(const FieldName: RawUTF8; W: TJSONSerializer;
      const Fields: TFieldBits = []);
    /// change TDocVariantData.Options for all variant published fields
    // - may be used to replace e.g. JSON_OPTIONS_FAST_EXTENDED by JSON_OPTIONS_FAST
    procedure ForceVariantFieldsOptions(aOptions: TDocVariantOptions = JSON_OPTIONS_FAST);
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
    // - won't read the ID field (should be read before, with the Count e.g.)
    // - PEnd should point just after the P input buffer, to avoid buffer overflow
    // - returns true on success, or false in case of invalid content in P^ e.g.
    // - P is updated to the next pending content after the read values
    function SetBinaryValues(var P: PAnsiChar; PEnd: PAnsiChar): boolean;
    /// set the simple field values from a binary buffer
    // - won't read the ID field (should be read before, with the Count e.g.)
    // - PEnd should point just after the P input buffer, to avoid buffer overflow
    // - returns true on success, or false in case of invalid content in P^ e.g.
    // - P is updated to the next pending content after the read values,
    function SetBinaryValuesSimpleFields(var P: PAnsiChar; PEnd: PAnsiChar): boolean;
    /// write the record fields into RawByteString a binary buffer
    // - same as GetBinaryValues(), but also writing the ID field first
    function GetBinary: RawByteString;
    /// set the record fields from a binary buffer saved by GetBinary()
    // - same as SetBinaryValues(), but also reading the ID field first
    // - PEnd should point to the end of the P input buffer, to avoid any overflow
    function SetBinary(P, PEnd: PAnsiChar): boolean; overload;
    /// set the record fields from a binary buffer saved by GetBinary()
    // - same as SetBinaryValues(), but also reading the ID field first
    function SetBinary(const binary: RawByteString): boolean; overload;
    /// set all field values from a supplied array of TSQLVar values
    // - Values[] array must match the RecordProps.Field[] order: will return
    // false if the Values[].VType does not match RecordProps.FieldType[]
    function SetFieldSQLVars(const Values: TSQLVarDynArray): boolean;
    /// retrieve a field value from a given property name, as encoded UTF-8 text
    // - you should use strong typing and direct property access, following
    // the ORM approach of the framework; but in some cases (a custom Grid
    // display, for instance), it could be useful to have this method available
    // - will return '' in case of wrong property name
    // - BLOB and dynamic array fields are returned as '\uFFF0base64encodedbinary'
    function GetFieldValue(const PropName: RawUTF8): RawUTF8;
    /// set a field value of a given property name, from some encoded UTF-8 text
    // - you should use strong typing and direct property access, following
    // the ORM approach of the framework; but in some cases (a custom Grid
    // display, for instance), it could be useful to have this method available
    // - won't do anything in case of wrong property name
    // - expect BLOB and dynamic array fields encoded as SQlite3 BLOB literals
    // ("x'01234'" e.g.) or '\uFFF0base64encodedbinary'
    procedure SetFieldValue(const PropName: RawUTF8; Value: PUTF8Char);
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
    // - if aSQLWhere is left to '', all rows are retrieved as fast as possible
    // (e.g. by-passing SQLite3 virtual table modules for external databases)
    // - the WHERE clause should use inlined parameters (like 'Name=:('Arnaud'):')
    // for better server speed - note that you can use FormatUTF8() as such:
    // ! aRec.FillPrepare(Client, FormatUTF8('Salary>? AND Salary<?', [], [1000, 2000]));
    // or call the overloaded FillPrepare() method directly with  BoundsSQLWhere
    // array of parameters
    // - aCustomFieldsCSV can be used to specify which fields must be retrieved
    // - default aCustomFieldsCSV='' will retrieve all simple table fields, but
    // you may need  to access only one or several fields, and will save remote
    // bandwidth by specifying the needed fields
    // - if aCustomFieldsCSV='*', it will retrieve all fields, including BLOBs
    // - note that you should not use this aCustomFieldsCSV optional parameter if
    // you want to Update the retrieved record content later, since any
    // missing fields will be left with previous values - but BatchUpdate() can be
    // safely used after FillPrepare (will set only ID, TModTime and mapped fields)
    function FillPrepare(const aClient: IRestOrm;
      const aSQLWhere: RawUTF8 = ''; const aCustomFieldsCSV: RawUTF8 = '';
      aCheckTableName: TOrmCheckTableName = ctnNoCheck): boolean; overload;
    /// prepare to get values using a specified WHERE clause with '%' parameters
    // - returns true in case of success, false in case of an error during SQL request
    // - then call FillRow(1..Table.RowCount) to get any row value
    // - or you can also loop through all rows with
    // ! while Rec.FillOne do
    // !   dosomethingwith(Rec);
    // - a temporary TOrmTable is created then stored in an internal fTable protected field
    // - for better server speed, the WHERE clause should use bound parameters
    // identified as '?' in the FormatSQLWhere statement, which is expected to
    // follow the order of values supplied in BoundsSQLWhere open array - use
    // DateToSQL/DateTimeToSQL for TDateTime, or directly any integer / double /
    // currency / RawUTF8 values to be bound to the request as parameters
    // - note that this method prototype changed with revision 1.17 of the
    // framework: array of const used to be ParamsSQLWhere and '%' in the
    // FormatSQLWhere statement, whereas it now expects bound parameters as '?'
    // - aCustomFieldsCSV can be used to specify which fields must be retrieved
    // - default aCustomFieldsCSV='' will retrieve all simple table fields, but
    // you may need  to access only one or several fields, and will save remote
    // bandwidth by specifying the needed fields
    // - if aCustomFieldsCSV='*', it will retrieve all fields, including BLOBs
    // - note that you should not use this aCustomFieldsCSV optional parameter if
    // you want to Update the retrieved record content later, since any
    // missing fields will be left with previous values - but BatchUpdate() can be
    // safely used after FillPrepare (will set only ID, TModTime and mapped fields)
    function FillPrepare(const aClient: IRestOrm;
      const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const;
      const aCustomFieldsCSV: RawUTF8 = ''): boolean; overload;
    /// prepare to get values using a specified WHERE clause with '%' and '?' parameters
    // - returns true in case of success, false in case of an error during SQL request
    // - then call FillRow(1..Table.RowCount) to get any row value
    // - or you can also loop through all rows with
    // ! while Rec.FillOne do
    // !   dosomethingwith(Rec);
    // - a temporary TOrmTable is created then stored in an internal fTable
    // protected field
    // - the FormatSQLWhere clause will replace all '%' chars with the supplied
    // ParamsSQLWhere[] supplied values, and bind all '?' chars as bound
    // parameters with BoundsSQLWhere[] values
    // - aCustomFieldsCSV can be used to specify which fields must be retrieved
    // - default aCustomFieldsCSV='' will retrieve all simple table fields, but
    // you may need  to access only one or several fields, and will save remote
    // bandwidth by specifying the needed fields
    // - if aCustomFieldsCSV='*', it will retrieve all fields, including BLOBs
    // - note that you should not use this aCustomFieldsCSV optional parameter if
    // you want to Update the retrieved record content later, since any
    // missing fields will be left with previous values - but BatchUpdate() can be
    // safely used after FillPrepare (will set only ID, TModTime and mapped fields)
    function FillPrepare(const aClient: IRestOrm;
      const FormatSQLWhere: RawUTF8; const ParamsSQLWhere, BoundsSQLWhere: array of const;
      const aCustomFieldsCSV: RawUTF8 = ''): boolean; overload;
    /// prepare to get values from a list of IDs
    // - returns true in case of success, false in case of an error during SQL request
    // - then call FillRow(1..Table.RowCount) to get any row value
    // - or you can also loop through all rows with
    // ! while Rec.FillOne do
    // !   dosomethingwith(Rec);
    // - a temporary TOrmTable is created then stored in an internal fTable protected field
    // - aCustomFieldsCSV can be used to specify which fields must be retrieved
    // - default aCustomFieldsCSV='' will retrieve all simple table fields, but
    // you may need  to access only one or several fields, and will save remote
    // bandwidth by specifying the needed fields
    // - if aCustomFieldsCSV='*', it will retrieve all fields, including BLOBs
    // - note that you should not use this aCustomFieldsCSV optional parameter if
    // you want to Update the retrieved record content later, since any
    // missing fields will be left with previous values - but BatchUpdate() can be
    // safely used after FillPrepare (will set only ID, TModTime and mapped fields)
    function FillPrepare(const aClient: IRestOrm;
      const aIDs: array of Int64;
      const aCustomFieldsCSV: RawUTF8 = ''): boolean; overload;
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
    // - the FormatSQLWhere clause will replace all '%' chars with the supplied
    // ParamsSQLWhere[] supplied values, and bind all '?' chars as parameters
    // with BoundsSQLWhere[] values
    // - you SHALL call explicitely the FillClose method before using any
    // methods of nested TOrmMany instances which may override the Dest
    // instance content (e.g. ManySelect) to avoid any GPF
    // - is used by TOrm.CreateAndFillPrepareMany constructor
    function FillPrepareMany(const aClient: IRestOrm;
      const aFormatSQLJoin: RawUTF8;
      const aParamsSQLJoin, aBoundsSQLJoin: array of const): boolean;
    /// compute a JOINed statement including TOrmMany fields
    // - is called by FillPrepareMany() to retrieve the JSON of the corresponding
    // request: so you could use this method to retrieve directly the same
    // information, ready to be transmitted (e.g. as RawJSON) to a client
    function EnginePrepareMany(const aClient: IRestOrm;
      const aFormatSQLJoin: RawUTF8;
      const aParamsSQLJoin, aBoundsSQLJoin: array of const;
      out ObjectsClass: TOrmClassDynArray; out SQL: RawUTF8): RawUTF8;
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
    // ! while FillOne do GetJSONValues(W)...
    procedure AppendFillAsJsonValues(W: TJSONSerializer);

    /// fill all published properties of this object from a TOrmTable result row
    // - call FillPrepare() then FillRow(Row)
    procedure FillFrom(Table: TOrmTable; Row: integer); overload;
    /// fill all published properties of this object from a JSON result row
    // - create a TOrmTable from the JSON data
    // - call FillPrepare() then FillRow(Row)
    procedure FillFrom(const JSONTable: RawUTF8; Row: integer); overload;
    /// fill all published properties of this object from a JSON object result
    // - use JSON data, as exported by GetJSONValues()
    // - JSON data may be expanded or not
    // - make an internal copy of the JSONTable RawUTF8 before calling
    // FillFrom() below
    // - if FieldBits is defined, it will store the identified field index
    procedure FillFrom(const JSONRecord: RawUTF8; FieldBits: PFieldBits = nil); overload;
    /// fill all published properties of this object from a JSON result
    // - the data inside P^ is modified (unescaped and transformed): don't call
    // FillFrom(pointer(JSONRecordUTF8)) but FillFrom(JSONRecordUTF8) which makes
    // a temporary copy of the JSONRecordUTF8 text
    // - use JSON data, as exported by GetJSONValues()
    // - JSON data may be expanded or not
    // - if FieldBits is defined, it will store the identified field index
    procedure FillFrom(P: PUTF8Char; FieldBits: PFieldBits = nil); overload;
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
    // - is a wrapper around VariantSaveJSON() + FillFrom() methods
    procedure FillFrom(const aDocVariant: variant); overload;
    /// fill a published property value of this object from a UTF-8 encoded value
    // - see TPropInfo about proper Delphi / UTF-8 type mapping/conversion
    // - use this method to fill a BLOB property, i.e. a property defined with
    // type RawBlob, since by default all BLOB properties are not
    // set by the standard Retrieve() method (to save bandwidth)
    // - if FieldBits is defined, it will store the identified field index
    procedure FillValue(PropName, Value: PUTF8Char; wasString: boolean;
      FieldBits: PFieldBits = nil);

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
    procedure ClearProperties(const aFieldsCSV: RawUTF8); overload;
    /// set the simple fields with the supplied values
    // - the aSimpleFields parameters must follow explicitely the order of published
    // properties of the supplied aTable class, excepting the RawBlob and
    // TOrmMany kind (i.e. only so called "simple fields") - in particular,
    // parent properties must appear first in the list
    // - the aSimpleFields must have exactly the same count of parameters as there are
    // "simple fields" in the published properties
    // - return true on success, but be aware that the field list must match
    // the field layout, otherwise if may return true but will corrupt data
    function SimplePropertiesFill(const aSimpleFields: array of const): boolean;
    /// initialize a TDynArray wrapper to map dynamic array property values
    // - if the field name is not existing or not a dynamic array, result.IsVoid
    // will be TRUE
    function DynArray(const DynArrayFieldName: RawUTF8): TDynArray; overload;
    /// initialize a TDynArray wrapper to map dynamic array property values
    // - this overloaded version expect the dynamic array to have been defined
    // with a not null index attribute, e.g.
    // ! published
    // !   property Ints: TIntegerDynArray index 1 read fInts write fInts;
    // !   property Currency: TCurrencyDynArray index 2 read fCurrency write fCurrency;
    // - if the field index is not existing or not a dynamic array, result.IsVoid
    // will be TRUE
    function DynArray(DynArrayFieldIndex: integer): TDynArray; overload;

    /// this property stores the record's integer ID
    // - if this TOrm is not a instance, but a field value in a published
    //  property of type oftID (i.e. TOrm(aID)), this method will try
    //  to retrieve it; but prefered method is to typecast it via PtrInt(aProperty),
    //  because GetID() relies on some low-level Windows memory mapping trick, and
    //  will recognize an ID value up to 1,048,576 (i.e. $100000)
    // - notice: the Setter should not be used usualy; you should not have to write
    //  aRecord.ID := someID in your code, since the ID is set during Retrieve or
    //  Add of the record
    // - use parent TSynPersistentID.IDValue property for direct read/write
    // access to the record's ID field, if you know that this TOrm is a
    // true allocated class instance
    property ID: TID read GetID;
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
    property AsTOrm: pointer read GetIDAsPointer;
    /// this property is set to true, if any published property is a BLOB (RawBlob)
    property HasBlob: boolean read GetHasBlob;
    /// this property returns the published property count with any valid
    // database field except RawBlob/TOrmMany
    // - by default, the RawBlob (BLOB) fields are not included into this set:
    // they must be read specificaly (in order to spare bandwidth)
    // - TOrmMany fields are not accessible directly, but as instances
    // created by TOrm.Create
    property SimpleFieldCount: integer read GetSimpleFieldCount;
    /// this property contains the TOrmTable after a call to FillPrepare()
    property FillTable: TOrmTable read GetTable;
    /// this property contains the current row number (beginning with 1),
    // initialized to 1 by FillPrepare(), which will be read by FillOne
    property FillCurrentRow: integer read GetFillCurrentRow;
    /// this property is set to true, if all rows have been browsed after
    // FillPrepare / while FillOne do ...
    property FillReachedEnd: boolean read GetFillReachedEnd;
    /// used internally by FillPrepare() and corresponding Fill*() methods
    property FillContext: TOrmFill read fFill;
    /// this property contains the internal state counter of the server database
    // when the data was retrieved from it
    // - can be used to check if retrieved data may be out of date
    property InternalState: cardinal read fInternalState write fInternalState;
  published
    { published properties in inherited classes will be interpreted as SQL fields }
  end;

  POrm = ^TOrm;

  TOrmArray = array[0..MaxInt div SizeOf(TOrm) - 1] of TOrm;
  POrmArray = ^TOrmArray;

  /// information about a TID published property
  // - identified as a oftTID kind of property, optionally tied to a TOrm
  // class, via its custom type name, e.g.
  // ! TOrmClientID = type TID;  ->  TOrmClient class
  TOrmPropInfoRTTITID = class(TOrmPropInfoRTTIRecordReference)
  protected
    fRecordClass: TOrmClass;
  public
    /// will setup the corresponding RecordClass property from the TID type name
    // - the TOrm type should have previously been registered to the
    // TJSONSerializer.RegisterClassForJSON list, e.g. in TOrmModel.Create, so
    // that e.g. 'TOrmClientID' type name will match TOrmClient
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
    property RecordClass: TOrmClass read fRecordClass;
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
    property CascadeDelete: boolean read fCascadeDelete;
  end;

  {$ifdef ISDELPHI2010} // Delphi 2009/2010 generics support is buggy :(

  /// since Delphi interface cannot have parametrized methods, we need
  // to use this abstract class to use generics signature
  TRestOrmGenerics = class(TInterfacedObject)
  protected
    // needed to implement RetrieveList<T> actual data retrieval
    function MultiFieldValues(Table: TOrmClass; const FieldNames: RawUTF8;
      const WhereClauseFormat: RawUTF8; const BoundsSQLWhere: array of const): TOrmTable;
      overload; virtual; abstract;
  public
    /// access to ORM parametrized/generic methods
    // - since Delphi interface cannot have parametrized methods, we need
    // to return this abstract class to use generics signature
    function Generics: TRestOrmGenerics;
    /// get a list of members from a SQL statement
    // - implements REST GET collection
    // - aCustomFieldsCSV can be the CSV list of field names to be retrieved
    // - if aCustomFieldsCSV is '', will get all simple fields, excluding BLOBs
    // - if aCustomFieldsCSV is '*', will get ALL fields, including ID and BLOBs
    // - return a TObjectList<T> on success (possibly with Count=0) - caller is
    // responsible of freeing the instance
    // - return nil on error
    // - since Delphi interface cannot have parametrized methods, we need to
    // call this overloaded TRestOrmGenerics method to use generics signature
    // - you can write for instance:
    // !var list: TObjectList<TOrmTest>;
    // !    R: TOrmTest;
    // !    orm: IRestOrm
    // ! ...
    // !    list := orm.Generics.RetrieveList<TOrmTest>('ID,Test');
    // !    if list <> nil then
    // !    try
    // !      for R in list do
    // !        writeln(R.ID, '=', R.Test);
    // !    finally
    // !      list.Free;
    // !    end;
    function RetrieveList<T: TOrm>(
      const aCustomFieldsCSV: RawUTF8 = ''): TObjectList<T>; overload;
       {$ifdef HASINLINE}inline;{$endif}
    /// get a list of members from a SQL statement
    // - implements REST GET collection with a WHERE clause
    // - for better server speed, the WHERE clause should use bound parameters
    // identified as '?' in the FormatSQLWhere statement, which is expected to
    // follow the order of values supplied in BoundsSQLWhere open array - use
    // DateToSQL()/DateTimeToSQL() for TDateTime, or directly any integer,
    // double, currency, RawUTF8 values to be bound to the request as parameters
    // - aCustomFieldsCSV can be the CSV list of field names to be retrieved
    // - if aCustomFieldsCSV is '', will get all simple fields, excluding BLOBs
    // - if aCustomFieldsCSV is '*', will get ALL fields, including ID and BLOBs
    // - return a TObjectList<T> on success (possibly with Count=0) - caller is
    // responsible of freeing the instance
    // - return nil on error
    // - since Delphi interface cannot have parametrized methods, we need to
    // call this overloaded TRestOrmGenerics method to use generics signature
    function RetrieveList<T: TOrm>(const FormatSQLWhere: RawUTF8;
      const BoundsSQLWhere: array of const;
      const aCustomFieldsCSV: RawUTF8 = ''): TObjectList<T>; overload;
  end;

  TRestOrmParent = TRestOrmGenerics;

  {$else}

  /// parent class of TRestOrm, to implement IRestOrm methods
  // - since Delphi interface cannot have parametrized methods, we need
  // to define a TRestOrmGenerics abstract class to use generics signature
  TRestOrmParent = TInterfacedObject;

  {$endif ISDELPHI2010}


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
    // integer values; but special UTF8CompareRecord() function has to be used
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
    function Text(Model: TOrmModel): RawUTF8; overload;
    /// get a ready to be displayed text from the stored Table and ID
    // - display 'Record "RecordName"' e.g.
    function Text(const Rest: IRestOrm): RawUTF8; overload;
  end;

  PRecordRef = ^RecordRef;


  { -------------------- TOrmTable TOrmTableJSON Definitions }

  /// allow on-the-fly translation of a TOrmTable grid value
  // - should return valid JSON value of the given cell (i.e. quoted strings,
  // or valid JSON object/array) unless HumanFriendly is defined
  // - e.g. TOrmTable.OnExportValue property will customize TOrmTable's
  // GetJSONValues, GetHtmlTable, and GetCSVValues methods returned content
  TOnOrmTableGetValue = function(Sender: TOrmTable; Row, Field: integer;
    HumanFriendly: boolean): RawJSON of object;

  /// store TOrmFieldType and RTTI for a given TOrmTable field
  TOrmTableFieldType = record
    /// the field kind, as in JSON (match TOrmPropInfo.OrmFieldTypeStored)
    ContentType: TOrmFieldType;
    /// how this field could be stored in a database
    // - equals ftUnknown if InitFields guessed the field type, or for oftVariant
    ContentDB: TSQLDBFieldType;
    /// the field size in bytes; -1 means not computed yet
    ContentSize: integer;
    /// the field low-level RTTI information
    // - is the PRttiInfo for oftBlobDynArray/oftNullable, PRttiEnumType for
    // oftEnumerate/oftSet, or nil
    ContentTypeInfo: pointer;
    /// the corresponding index in fQueryTables[]
    TableIndex: integer;
  end;

  POrmTableFieldType = ^TOrmTableFieldType;

  /// wrapper to an ORM result table, staticaly stored as UTF-8 text
  // - contain all result in memory, until destroyed
  // - first row contains the field names
  // - following rows contains the data itself
  // - GetString() can be used in a TDrawString
  // - will be implemented as TOrmTableJSON for remote access through optimized
  // JSON content
  TOrmTable = class
  protected
    fRowCount: integer;
    fFieldCount: integer;
    /// contains the data, e.g. as returned by sqlite3_get_table()
    fResults: PPUTF8CharArray;
    fFieldType: array of TOrmTableFieldType;
    fFieldTypeAllRows: boolean;
    /// the field names
    fFieldNames: TRawUTF8DynArray;
    /// used by FieldIndex() for fast O(log(n)) binary search
    fFieldNameOrder: TCardinalDynArray;
    /// contain the fResults[] pointers, after a IDColumnHide() call
    fIDColumn, fNotIDColumn: TPUTF8CharDynArray;
    /// index of a 'ID' field, -1 if none (e.g. after IDColumnHide method call)
    fFieldIndexID: integer;
    /// the internal state counter of the database when the data was retrieved
    fInternalState: cardinal;
    /// contains the parameters used for sorting
    fSortParams: TOrmTableSortParams;
    /// contains the TOrm instances created by NewRecord method
    fOwnedRecords: TSynObjectList;
    /// if the TOrm is the owner of this table, i.e. if it must free it
    fOwnerMustFree: boolean;
    /// current cursor row (1..RowCount), as set by the Step() method
    fStepRow: integer;
    /// information about the Query sourcing this result set
    fQueryTables: TOrmClassDynArray;
    fQueryColumnTypes: array of TOrmFieldType;
    fQuerySQL: RawUTF8;
    fQueryTableNameFromSQL: RawUTF8;
    fQueryTableIndexFromSQL: integer; // -2=nosearch -1=notfound fQueryTables[0..n]
    /// field length information
    fFieldLengthMean: TIntegerDynArray;
    fFieldLengthMeanSum: integer;
    /// column bit set at parsing to mark a string value (e.g. "..." in JSON)
    fFieldParsedAsString: set of 0..255;
    fOnExportValue: TOnOrmTableGetValue;
    /// avoid GPF when TOrmTable is nil
    function GetRowCount: integer;
      {$ifdef HASINLINE}inline;{$endif}
    /// fill the fFieldType[] array (from fQueryTables[] or fResults[] content)
    procedure InitFieldTypes;
    /// fill the internal fFieldNames[] array
    procedure InitFieldNames;
    /// guess the property type information from ORM
    function FieldPropFromTables(const PropName: RawUTF8;
      out PropInfo: TOrmPropInfo; out TableIndex: integer): TOrmFieldType;
    function GetQueryTableNameFromSQL: RawUTF8;
  public
    /// initialize the result table
    // - you can optionaly associate the corresponding TOrmClass types,
    // by which the results were computed (it will use RTTI for column typing)
    constructor Create(const aSQL: RawUTF8);
    /// initialize the result table
    // - you can associate the corresponding TOrmClass types,
    // by which the results were computed (it will use RTTI for column typing)
    constructor CreateFromTables(const Tables: array of TOrmClass;
      const aSQL: RawUTF8);
    /// initialize the result table
    // - you can set the expected column types matching the results column layout
    constructor CreateWithColumnTypes(const ColumnTypes: array of TOrmFieldType;
      const aSQL: RawUTF8);
    /// free associated memory and owned records
    destructor Destroy; override;
    /// read-only access to a particular field value, as UTF-8 encoded buffer
    // - if Row and Fields are correct, returns a pointer to the UTF-8 buffer,
    // or nil if the corresponding JSON was null or ""
    // - if Row and Fields are not correct, returns nil
    function Get(Row, Field: integer): PUTF8Char; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read-only access to a particular field value, as RawUTF8 text
    function GetU(Row, Field: integer): RawUTF8; overload;
    /// read-only access to a particular field value, as UTF-8 encoded buffer
    // - points to memory buffer allocated by Init()
    function Get(Row: integer; const FieldName: RawUTF8): PUTF8Char; overload;
    /// read-only access to a particular field value, as RawUTF8 text
    function GetU(Row: integer; const FieldName: RawUTF8): RawUTF8; overload;
    /// read-only access to a particular field value, as Win Ansi text
    function GetA(Row, Field: integer): WinAnsiString;
    /// read-only access to a particular field value, as Win Ansi text shortstring
    function GetS(Row, Field: integer): shortstring;
    /// read-only access to a particular field value, as a Variant
    // - text will be stored as RawUTF8 (as varString type)
    // - will try to use the most approriate Variant type for conversion (will
    // use e.g. TDateTime for oftDateTime, or a TDocVariant for JSON objects
    // in a oftVariant column) - so you should better set the exact field types
    // (e.g. from ORM) before calling this method
    function GetVariant(Row, Field: integer): variant; overload;
    /// read-only access to a particular field value, as a Variant
    // - text will be stored as RawUTF8 (as varString type)
    // - will try to use the most approriate Variant type for conversion (will
    // use e.g. TDateTime for oftDateTime, or a TDocVariant for JSON objects
    // in a oftVariant column) - so you should better set the exact field types
    // (e.g. from ORM) before calling this method
    procedure GetVariant(Row, Field: integer; var result: variant); overload;
    /// read-only access to a particular field, via a lookup field name
    // - will call GetVariant() on the corresponding field
    // - returns null if the lookup did not have any match
    function GetValue(const aLookupFieldName, aLookupValue, aValueFieldName: RawUTF8): variant;
    /// read-only access to a particular field value, as VCL string text
    // - the global UTF8ToString() function will be used for the conversion:
    // for proper i18n handling before Delphi 2009, you should use the
    // overloaded method with aUTF8ToString=Language.UTF8ToString
    function GetString(Row, Field: integer): string;
    /// read-only access to a particular field value, as fast Unicode string text
    // - SynUnicode is either WideString, either UnicodeString, depending on the
    // Delphi compiler revision, to ensure fastest native Unicode process available
    function GetSynUnicode(Row, Field: integer): SynUnicode;
    /// fill a unicode buffer with a particular field value
    // - return number of wide characters written in Dest^
    function GetWP(Row, Field: integer; Dest: PWideChar; MaxDestChars: cardinal): integer;
    /// read-only access to a particular field value, as UTF-16 Unicode text
    // - Raw Unicode is WideChar(zero) terminated
    // - its content is allocated to contain all WideChars (not trimed to 255,
    // like GetWP() above
    function GetW(Row, Field: integer): RawUnicode;
    /// read-only access to a particular field value, as integer value
    function GetAsInteger(Row, Field: integer): integer; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read-only access to a particular field value, as integer value
    function GetAsInteger(Row: integer; const FieldName: RawUTF8): integer; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read-only access to a particular field value, as Int64 value
    function GetAsInt64(Row, Field: integer): Int64; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read-only access to a particular field value, as Int64 value
    function GetAsInt64(Row: integer; const FieldName: RawUTF8): Int64; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read-only access to a particular field value, as extended value
    function GetAsFloat(Row, Field: integer): TSynExtended; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read-only access to a particular field value, as extended value
    function GetAsFloat(Row: integer; const FieldName: RawUTF8): TSynExtended; overload;
      {$ifdef HASINLINE}inline;{$endif}
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
    function GetAsDateTime(Row, Field: integer): TDateTime; overload;
    /// read-only access to a particular field value, as TDateTime value
    function GetAsDateTime(Row: integer; const FieldName: RawUTF8): TDateTime; overload;
    /// read-only access to a particular field value, as currency value
    function GetAsCurrency(Row, Field: integer): currency; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read-only access to a particular field value, as currency value
    function GetAsCurrency(Row: integer; const FieldName: RawUTF8): currency; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read-only access to a particular field value, ready to be displayed
    // - mostly used with Row=0, i.e. to get a display value from a field name
    // - use "string" type, i.e. UnicodeString for Delphi 2009+
    // - value is first un-camel-cased: 'OnLine' value will return 'On line' e.g.
    // - then System.LoadResStringTranslate() is called if available
    function GetCaption(Row, Field: integer): string;
    /// read-only access to a particular Blob value
    // - a new RawBlob is created
    // - Blob data is converted from SQLite3 BLOB literals (X'53514C697465' e.g.)
    // or Base-64 encoded content ('\uFFF0base64encodedbinary')
    // - prefered manner is to directly use REST protocol to retrieve a blob field
    function GetBlob(Row, Field: integer): RawBlob;
    /// read-only access to a particular Blob value
    // - a new TBytes is created
    // - Blob data is converted from SQLite3 BLOB literals (X'53514C697465' e.g.)
    //   or Base-64 encoded content ('\uFFF0base64encodedbinary')
    // - prefered manner is to directly use REST protocol to retrieve a blob field
    function GetBytes(Row, Field: integer): TBytes;
    /// read-only access to a particular Blob value
    // - a new TCustomMemoryStream is created - caller shall free its instance
    // - Blob data is converted from SQLite3 BLOB literals (X'53514C697465' e.g.)
    //   or Base-64 encoded content ('\uFFF0base64encodedbinary')
    // - prefered manner is to directly use REST protocol to retrieve a blob field
    function GetStream(Row, Field: integer): TStream;
    /// read-only access to a particular field value, as VCL text
    // - Client is used to display TRecordReference via the associated TOrmModel
    // - returns the Field Type
    // - return generic string Text, i.e. UnicodeString for Delphi 2009+, ready
    // to be displayed to the VCL, for oftEnumerate, oftTimeLog,
    // oftUnixTime/oftUnixMSTime and oftRecord/oftRecordVersion/oftID/oftTID
    // - returns '' as string Text, if text can by displayed directly
    // with Get*() methods above
    // - returns '' for other properties kind, if UTF8ToString is nil,
    // or the ready to be displayed value if UTF8ToString event is set
    // (to be used mostly with Language.UTF8ToString)
    // - CustomFormat can optionaly set a custom format string, e.g. '%f' or '%n'
    // or complex FormatFloat()/FormatCurr() syntax (as '#,##0.00') for oftFloat
    // and oftCurrency columns (instead of plain JSON float value), or
    // date/time format as expected by FormatDateTime() for all date time kind
    // of fields (as oftDateTime, oftDateTimeMS, oftTimeLog, oftModTime,
    // oftCreateTime, oftUnixTime, oftUnixMSTime)
    function ExpandAsString(Row, Field: integer; const Client: IRestOrm;
      out Text: string; const CustomFormat: string = ''): TOrmFieldType;
    /// read-only access to a particular field value, as VCL text
    // - this method is just a wrapper around ExpandAsString method, returning
    // the content as a SynUnicode string type (i.e. UnicodeString since Delphi
    // 2009, and WideString for non Unicode versions of Delphi)
    function ExpandAsSynUnicode(Row, Field: integer; const Client: IRestOrm;
      out Text: SynUnicode): TOrmFieldType;
    /// read-only access to a particular DateTime field value
    // - expect SQLite3 TEXT field in ISO 8601 'YYYYMMDD hhmmss' or
    // 'YYYY-MM-DD hh:mm:ss' format
    function GetDateTime(Row, Field: integer): TDateTime;
    /// read-only access to a particular TTimeLog field value
    // - return the result as TTimeLogBits.Text() Iso-8601 encoded text
    function GetTimeLog(Row, Field: integer; Expanded: boolean; FirstTimeChar:
      AnsiChar = 'T'): RawUTF8;
    /// widechar length (UTF-8 decoded as UTF-16) of a particular field value
    // - could be used with VCL's UnicodeString, or for Windows API
    function LengthW(Row, Field: integer): integer;
    /// get all values for a specified field into a dynamic RawUTF8 array
    // - don't perform any conversion, but just create an array of raw PUTF8Char data
    // - returns the number of rows in Values[]
    function GetRowValues(Field: integer; out Values: TRawUTF8DynArray): integer; overload;
    /// get all values for a specified field into a dynamic integer array
    // - returns the number of rows in Values[]
    function GetRowValues(Field: integer; out Values: TInt64DynArray): integer; overload;
    /// get all values for a specified field as CSV
    // - don't perform any conversion, but create a CSV from raw PUTF8Char data
    function GetRowValues(Field: integer; const Sep: RawUTF8 = ',';
      const Head: RawUTF8 = ''; const Trail: RawUTF8 = ''): RawUTF8; overload;
    /// get all values lengths for a specified field into a PIntegerArray
    // - returns the total length as result, and fill LenStore with all rows
    // individual lengths using StrLen() - caller should eventually call
    // LenStore.Done to release any temp memory
    // - returns 0 if Field is invalid or no data is stored in this TOrmTable -
    // don't call LenStore.Done in this case
    function GetRowLengths(Field: integer; var LenStore: TSynTempBuffer): integer;
    /// retrieve a field value as a variant
    // - returns null if the row/field is incorrect
    // - expand* methods will allow to return human-friendly representations
    procedure GetAsVariant(row, field: integer; out value: variant;
      expandTimeLogAsText, expandEnumsAsText, expandHugeIDAsUniqueIdentifier: boolean;
      options: TDocVariantOptions = JSON_OPTIONS_FAST);
    /// retrieve a row value as a variant, ready to be accessed via late-binding
    // - Row parameter numbering starts from 1 to RowCount
    // - this method will return a TDocVariant containing a copy of all
    // field values of this row, uncoupled to the TOrmTable instance life time
    // - expand* methods will allow to return human-friendly representations
    procedure ToDocVariant(Row: integer; out doc: variant;
      options: TDocVariantOptions = JSON_OPTIONS_FAST;
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
    // - JSON data is added to TJSONWriter, with UTF-8 encoding, and not flushed
    // - if Expand is true, JSON data is an array of objects, for direct use
    // with any Ajax or .NET client:
    // & [ {"col1":val11,"col2":"val12"},{"col1":val21,... ]
    // - if W.Expand is false, JSON data is serialized (used in TOrmTableJSON)
    // & { "fieldCount":1,"values":["col1","col2",val11,"val12",val21,..] }
    // - RowFirst and RowLast can be used to ask for a specified row extent
    // of the returned data (by default, all rows are retrieved)
    // - IDBinarySize will force the ID field to be stored as hexadecimal text
    procedure GetJSONValues(W: TJSONWriter;
      RowFirst: integer = 0; RowLast: integer = 0; IDBinarySize: integer = 0); overload;
    /// same as the overloaded method, but appending an array to a TStream
    procedure GetJSONValues(JSON: TStream; Expand: boolean;
      RowFirst: integer = 0; RowLast: integer = 0; IDBinarySize: integer = 0); overload;
    /// same as the overloaded method, but returning result into a RawUTF8
    function GetJSONValues(Expand: boolean; IDBinarySize: integer = 0;
      BufferSize: integer = 0): RawUTF8; overload;
    /// save the table as CSV format, into a stream
    // - if Tab=TRUE, will use TAB instead of ',' between columns
    // - you can customize the ',' separator - use e.g. the global ListSeparator
    // variable (from SysUtils) to reflect the current system definition (some
    // country use ',' as decimal separator, for instance our "douce France")
    // - AddBOM will add a UTF-8 Byte Order Mark at the beginning of the content
    procedure GetCSVValues(Dest: TStream; Tab: boolean; CommaSep: AnsiChar = ',';
      AddBOM: boolean = false; RowFirst: integer = 0; RowLast: integer = 0); overload;
    /// save the table as CSV format, into a string variable
    // - if Tab=TRUE, will use TAB instead of ',' between columns
    // - you can customize the ',' separator - use e.g. the global ListSeparator
    // variable (from SysUtils) to reflect the current system definition (some
    // country use ',' as decimal separator, for instance our "douce France")
    // - AddBOM will add a UTF-8 Byte Order Mark at the beginning of the content
    function GetCSVValues(Tab: boolean; CommaSep: AnsiChar = ',';
      AddBOM: boolean = false; RowFirst: integer = 0; RowLast: integer = 0): RawUTF8; overload;
    /// save the table in 'schemas-microsoft-com:rowset' XML format
    // - this format is used by ADODB.recordset, easily consumed by MS apps
    // - see @https://synopse.info/forum/viewtopic.php?pid=11691#p11691
    procedure GetMSRowSetValues(Dest: TStream; RowFirst, RowLast: integer); overload;
    /// save the table in 'schemas-microsoft-com:rowset' XML format
    // - this format is used by ADODB.recordset, easily consumed by MS apps
    // - see @https://synopse.info/forum/viewtopic.php?pid=11691#p11691
    function GetMSRowSetValues: RawUTF8; overload;
    /// save the table in Open Document Spreadsheet compressed format
    // - this is a set of XML files compressed in a zip container
    // - this method will return the raw binary buffer of the file
    // - see @https://synopse.info/forum/viewtopic.php?id=2133
    function GetODSDocument(withColumnTypes: boolean = false): RawByteString;
    /// append the table content as a HTML <table> ... </table>
    procedure GetHtmlTable(Dest: TTextWriter); overload;
    /// save the table as a <html><body><table> </table></body></html> content
    function GetHtmlTable(const Header: RawUTF8 = '<head><style>table,th,td' +
      '{border: 1px solid black;border-collapse: collapse;}th,td{padding: 5px;' +
      'font-family: sans-serif;}</style></head>'#10): RawUTF8; overload;
    /// get the Field index of a FieldName
    // - return -1 if not found, index (0..FieldCount-1) if found
    function FieldIndex(FieldName: PUTF8Char): integer; overload;
    /// get the Field index of a FieldName
    // - return -1 if not found, index (0..FieldCount-1) if found
    function FieldIndex(const FieldName: RawUTF8): integer; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// get the Field index of a FieldName
    // - raise an EOrmTable if not found, index (0..FieldCount-1) if found
    function FieldIndexExisting(const FieldName: RawUTF8): integer; overload;
    /// get the Field indexes of several Field names
    // - could be used to speed-up field access in a TOrmTable loop, avoiding
    // a FieldIndex(aFieldName) lookup for each value
    // - returns the number of matching Field names
    // - set -1 in FieldIndexes[]^ if not found, index (0..FieldCount-1) if found
    function FieldIndex(const FieldNames: array of RawUTF8;
      const FieldIndexes: array of PInteger): integer; overload;
    /// get the Field indexes of several Field names
    // - raise an EOrmTable if not found
    // - set FieldIndexes[]^ to the index (0..FieldCount-1) if found
    // - could be used to speed-up field access in a TOrmTable loop, avoiding
    // a FieldIndex(aFieldName) lookup for each value, as such:
    //! list := TOrmTableJSON.Create('',pointer(json),length(json));
    //! list.FieldIndexExisting(
    //!   ['FirstName','LastName','YearOfBirth','YearOfDeath','RowID','Data'],
    //!   [@FirstName,@LastName,@YearOfBirth,@YearOfDeath,@RowID,@Data]);
    //! for i := 1 to list.RowCount do
    //! begin
    //!   Check(list.Get(i,FirstName)<>nil);
    //!   Check(list.Get(i,LastName)<>nil);
    //!   Check(list.GetAsInteger(i,YearOfBirth)<10000);
    procedure FieldIndexExisting(const FieldNames: array of RawUTF8;
      const FieldIndexes: array of PInteger); overload;
    /// retrieve all field names as a RawUTF8 dynamic array
    function FieldNames: TRawUTF8DynArray;
    /// get the Field content (encoded as UTF-8 text) from a property name
    // - return nil if not found
    function FieldValue(const FieldName: RawUTF8; Row: integer): PUTF8Char;
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
      CustomCompare: TUTF8Compare = nil); overload;
    /// sort result Rows, according to a specific field
    // - overloaded method allowing to specify the field by its name
    procedure SortFields(const FieldName: RawUTF8; Asc: boolean = true;
      PCurrentRow: PInteger = nil; FieldType: TOrmFieldType = oftUnknown;
      CustomCompare: TUTF8Compare = nil); overload;
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
      const CustomCompare: array of TUTF8Compare); overload;
    /// sort result Rows, according to the Bits set to 1 first
    procedure SortBitsFirst(var Bits);
    /// guess the field type from first non null data row
    // - if QueryTables[] are set, exact field type and enumerate TypeInfo() is
    // retrieved from the Delphi RTTI; otherwise, get from the cells content
    // - return oftUnknown is all data fields are null
    // - oftBlob is returned if the field is encoded as SQLite3 BLOB literals
    // (X'53514C697465' e.g.)
    // - since TOrmTable data is PUTF8Char, string type is oftUTF8Text only
    function FieldType(Field: integer): TOrmFieldType; overload;
    /// guess the field type from first non null data row
    // - if QueryTables[] are set, exact field type and (enumerate) TypeInfo() is
    // retrieved from the Delphi RTTI; otherwise, get from the cells content
    // - return oftUnknown is all data fields are null
    // - oftBlob is returned if the field is encoded as SQLite3 BLOB literals
    // (X'53514C697465' e.g.)
    // - since TOrmTable data is PUTF8Char, string type is oftUTF8Text only
    function FieldType(Field: integer;
      out FieldTypeInfo: POrmTableFieldType): TOrmFieldType; overload;
    /// get the appropriate Sort comparison function for a field,
    // nil if not available (bad field index or field is blob)
    // - field type is guessed from first data row
    function SortCompare(Field: integer): TUTF8Compare;
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
    function FieldLengthMean(Field: integer): cardinal;
    /// get the sum of all mean of characters length of all fields
    // - very fast: calculated only once for all fields
    function FieldLengthMeanSum: cardinal;
    /// get the maximum number of characters of this field
    function FieldLengthMax(Field: integer; NeverReturnsZero: boolean = false): cardinal;
    /// get the record class (i.e. the table) associated to a field
    // - is nil if this table has no QueryTables property
    // - very fast: calculated only once for all fields
    function FieldTable(Field: integer): TOrmClass;
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
    procedure SetFieldType(Field: integer; FieldType: TOrmFieldType;
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
    procedure SetFieldType(const FieldName: RawUTF8; FieldType: TOrmFieldType;
      FieldTypeInfo: PRttiInfo = nil; FieldSize: integer = -1); overload;
    /// set the exact type of all fields, from the DB-like information
    procedure SetFieldTypes(const DBTypes: TSQLDBFieldTypeDynArray);
    /// increase a particular Field Length Mean value
    // - to be used to customize the field appareance (e.g. for adding of left
    // checkbox for Marked[] fields)
    procedure FieldLengthMeanIncrease(aField, aIncrease: integer);

    /// copy the parameters of a TOrmTable into this instance
    // - the Results[] remain in the source TOrmTable: source TOrmTable has not
    // to be destroyed before this TOrmTable
    procedure Assign(source: TOrmTable);

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
    function SearchValue(const UpperValue: RawUTF8; StartRow, FieldIndex:
      integer; const Client: IRestOrm; Lang: TSynSoundExPronunciation = sndxNone;
      UnicodeComparison: boolean = false): integer; overload;
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
    function SearchValue(const UpperValue: RawUTF8;
      StartRow: integer; FieldIndex: PInteger; const Client: IRestOrm;
      Lang: TSynSoundExPronunciation = sndxNone;
      UnicodeComparison: boolean = false): integer; overload;
    /// search for a value inside the raw table data, using UTF8IComp/StrComp()
    // - returns 0 if not found, or the matching Row number otherwise
    function SearchFieldEquals(const Value: RawUTF8; FieldIndex: integer;
      StartRow: integer = 1; CaseSensitive: boolean = false): integer; overload;
    /// search for a value inside the raw table data, using UTF8IComp/StrComp()
    // - returns 0 if not found, or the matching Row number otherwise
    function SearchFieldEquals(Value: PUTF8Char; FieldIndex: integer;
      StartRow: integer = 1; CaseSensitive: boolean = false): integer; overload;
    /// search for a value inside the raw table data, using IdemPChar()
    // - returns 0 if not found, or the matching Row number otherwise
    function SearchFieldIdemPChar(const Value: RawUTF8; FieldIndex: integer;
      StartRow: integer = 1): integer;
    /// search for a value using O(log(n)) binary search of a sorted field
    // - here the content should have been previously sorted via Sort(),
    // or CustomCompare should be defined, otherwise the SearchFieldEquals()
    // slower O(n) method is called
    // - returns 0 if not found, or the matching Row number otherwise
    function SearchFieldSorted(const Value: RawUTF8; FieldIndex: integer;
      CustomCompare: TUTF8Compare = nil): integer; overload;
    /// search for a value using O(log(n)) binary search of a sorted field
    // - here the content should have been previously sorted via Sort(),
    // or CustomCompare should be defined, otherwise the SearchFieldEquals()
    // slower O(n) method is called
    // - returns 0 if not found, or the matching Row number otherwise
    function SearchFieldSorted(Value: PUTF8Char; FieldIndex: integer;
      CustomCompare: TUTF8Compare = nil): integer; overload;

    /// if the ID column is available, hides it from Results[]
    // - useful for simplier UI, with a hidden ID field
    // - use IDColumnHiddenValue() to get the ID of a specific row
    // - return true is ID was succesfully hidden, false if not possible
    function IDColumnHide: boolean;
    /// return the (previously hidden) ID value, 0 on error
    function IDColumnHiddenValue(Row: integer): TID;
    /// return all (previously hidden) ID values
    procedure IDColumnHiddenValues(var IDs: TIDDynArray);
    /// get all IDs where individual bit in Bits are set
    procedure IDArrayFromBits(const Bits; var IDs: TIDDynArray);
    /// get all individual bit in Bits corresponding to the supplied IDs
    // - warning: IDs integer array will be sorted within this method call
    procedure IDArrayToBits(var Bits; var IDs: TIDDynArray);
    /// get the Row index corresponding to a specified ID
    // - return the Row number, from 1 to RowCount
    // - return RowCount (last row index) if this ID was not found or no
    // ID field is available, unless aNotFoundMinusOne is set, and then -1 is
    // returned
    function RowFromID(aID: TID; aNotFoundMinusOne: boolean = false): integer;

    /// delete the specified data Row from the Table
    // - only overwrite the internal Results[] pointers, don't free any memory,
    // nor modify the internal DataSet
    function DeleteRow(Row: integer): boolean;
    /// delete the specified Column text from the Table
    // - don't delete the Column: only delete UTF-8 text in all rows for this field
    function DeleteColumnValues(Field: integer): boolean;

    /// retrieve QueryTables[0], if existing
    function QueryRecordType: TOrmClass;

    /// create a new TOrm instance for a specific Table
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
    {$ifdef ISDELPHI2010} // Delphi 2009/2010 generics are buggy
    /// create a TObjectList<TOrm> with TOrm instances corresponding
    // to this TOrmTable result set
    // - use the specified TOrm class or create instances
    // of the first associated record class (from internal QueryTables[])
    // - always returns an instance, even if the TOrmTable is nil or void
    function ToObjectList<T: TOrm>: TObjectList<T>; overload;
    {$endif ISDELPHI2010}
    /// fill an existing T*ObjArray variable with TOrm instances
    // corresponding to this TOrmTable result set
    // - use the specified TOrm class or create instances
    // of the first associated record class (from internal QueryTables[])
    // - returns TRUE on success (even if ObjArray=[]), FALSE on error
    function ToObjArray(var ObjArray; RecordType: TOrmClass = nil): boolean;

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
    function FieldBuffer(FieldIndex: integer): PUTF8Char; overload;
    /// read-only access to a particular field value, as UTF-8 encoded buffer
    // - raise an EOrmTable if called outside valid Step() sequence
    // - similar to Get() method, but for the current Step
    function FieldBuffer(const FieldName: RawUTF8): PUTF8Char; overload;
    /// read-only access to a particular field value, as integer
    // - raise an EOrmTable if called outside valid Step() sequence
    // - similar to GetAsInteger() method, but for the current Step
    function FieldAsInteger(FieldIndex: integer): Int64; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read-only access to a particular field value, as integer
    // - raise an EOrmTable if called outside valid Step() sequence
    // - similar to GetAsInteger() method, but for the current Step
    function FieldAsInteger(const FieldName: RawUTF8): Int64; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read-only access to a particular field value, as floating-point value
    // - raise an EOrmTable if called outside valid Step() sequence
    // - similar to GetAsFloat() method, but for the current Step
    function FieldAsFloat(FieldIndex: integer): TSynExtended; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read-only access to a particular field value, as floating-point value
    // - raise an EOrmTable if called outside valid Step() sequence
    // - similar to GetAsFloat() method, but for the current Step
    function FieldAsFloat(const FieldName: RawUTF8): TSynExtended; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read-only access to a particular field value, as RawUTF8
    // - raise an EOrmTable if called outside valid Step() sequence
    // - similar to GetU() method, but for the current Step
    function FieldAsRawUTF8(FieldIndex: integer): RawUTF8; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read-only access to a particular field value, as RawUTF8
    // - raise an EOrmTable if called outside valid Step() sequence
    // - similar to GetU() method, but for the current Step
    function FieldAsRawUTF8(const FieldName: RawUTF8): RawUTF8; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read-only access to a particular field value, as VCL String
    // - raise an EOrmTable if called outside valid Step() sequence
    // - similar to GetString() method, but for the current Step
    function FieldAsString(FieldIndex: integer): string; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read-only access to a particular field value, as VCL String
    // - raise an EOrmTable if called outside valid Step() sequence
    // - similar to GetString() method, but for the current Step
    function FieldAsString(const FieldName: RawUTF8): string; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read-only access to a particular field value, as a variant
    // - raise an EOrmTable if called outside valid Step() sequence
    // - will call GetVariant() method for appropriate data conversion
    function Field(FieldIndex: integer): variant; overload;
    /// read-only access to a particular field value, as a variant
    // - raise an EOrmTable if called outside valid Step() sequence
    // - will call GetVariant() method for appropriate data conversion
    function Field(const FieldName: RawUTF8): variant; overload;

    /// contains the associated record class on Query
    property QueryTables: TOrmClassDynArray read fQueryTables;
    /// contains the associated SQL statement on Query
    property QuerySQL: RawUTF8 read fQuerySQL;
    /// returns the SQL Table name, guessed from the associated QuerySQL statement
    property QueryTableNameFromSQL: RawUTF8 read GetQueryTableNameFromSQL;
    /// read-only access to the number of data Rows in this table
    // - first row contains field name
    // - then 1..RowCount rows contain the data itself
    // - safely returns 0 if the TOrmTable instance is nil
    property RowCount: integer read GetRowCount;
    /// read-only access to the number of fields for each Row in this table
    property FieldCount: integer read fFieldCount;
    /// raw access to the data values memory pointers
    // - you should rather use the Get*() methods
    property Results: PPUTF8CharArray read fResults;
    /// read-only access to the ID/RowID field index
    // - do not use this property if the ID column has been hidden, but
    // use IDColumnHiddenValue() method instead
    property FieldIndexID: integer read fFieldIndexID;
    /// read-only acccess to the current Row number, after a Step() call
    // - contains 0 if accessed outside valid Step() sequence call
    // - contains 1..RowCount after a valid Step() iteration
    property StepRow: integer read fStepRow;
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
    /// used by GetJsonValues, GetHtmlTable and GetCSVValues methods
    // to export custom JSON content
    property OnExportValue: TOnOrmTableGetValue
      read fOnExportValue write fOnExportValue;
  end;

  /// memory structure used for our TOrmTableRowVariant custom variant type
  // used to have direct access to TOrmTable content
  // - the associated TOrmTable must stay allocated as long as this variant
  // is used, otherwise random GPF issues may occur
  TOrmTableRowVariantData = packed record
    /// the custom variant type registered number
    VType: TVarType;
    VFiller: array[1..SizeOf(TVarData) - SizeOf(TVarType)
      - SizeOf(TOrmTable) - SizeOf(integer)] of byte;
    /// reference to the associated TOrmTable
    VTable: TOrmTable;
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
      Name: PAnsiChar; NameLen: PtrInt): boolean; override;
  public
    /// customization of variant into JSON serialization
    procedure ToJSON(W: TTextWriter; const Value: variant;
      Escape: TTextWriterKind); override;
    /// handle type conversion to string
    procedure Cast(var Dest: TVarData; const Source: TVarData); override;
    /// handle type conversion to string
    procedure CastTo(var Dest: TVarData; const Source: TVarData;
      const AVarType: TVarType); override;
  end;


  /// store a read-only ORM result table from a JSON message
  // - the JSON data is parsed and unescaped in-place, to enhanced performance
  // and reduce resource consumption (mainly memory/heap fragmentation)
  // - is used by the ORM for TOrm.FillPrepare/FillOne methods for
  // fast access to individual object values
  TOrmTableJSON = class(TOrmTable)
  protected
    /// used if a private copy of the JSON buffer is needed
    fPrivateCopy: RawUTF8;
    /// contains the pointers of start of every field value in JSONData
    fJSONResults: TPUTF8CharDynArray;
    /// contain the hash value of the last JSON data sent to ContentChanged()
    // - used to don't repeat parsing if data has not been changed
    fPrivateCopyHash: cardinal;
    /// fill the result table content from a JSON-formated Data message
    // - returns TRUE on parsing success
    // - returns FALSE if no valid JSON data was found
    // - update all content fields (Results[], RowCount, FieldCount, etc...)
    // - expect the UTF-8 Buffer in either TSQLRequest.EngineExecute(DB,SQL,JSON)
    // format (i.e. expanded) or either in a not expanded format (as an
    // AJAX-ready array of objects)
    // - the conversion into PPUTF8CharArray is made inplace and is very fast
    // (no additional memory buffer is allocated)
    function ParseAndConvert(Buffer: PUTF8Char; BufferLen: integer): boolean;
    /// will check then set (if needed) internal fPrivateCopy[Hash] values
    // - returns TRUE if fPrivateCopy content changed (then fPrivateCopyHash
    // will be updated using crc32c hash if aUpdateHash is set)
    function PrivateCopyChanged(aJSON: PUTF8Char; aLen: integer;
      aUpdateHash: boolean): boolean;
  public
    /// create the result table from a JSON-formated Data message
    // - the JSON data is parsed and formatted in-place
    // - please note that the supplied JSON buffer content will be changed:
    // if you want to reuse this JSON content, you shall make a private copy
    // before calling this constructor and you shall NOT release the corresponding
    // variable (Results/JSONResults[] will point inside this memory buffer):
    // use instead the overloaded Create constructor expecting a const
    // aJSON: RawUTF8 parameter to allocate and hold a private copy of the data
    constructor Create(const aSQL: RawUTF8;
      JSONBuffer: PUTF8Char; JSONBufferLen: integer); reintroduce; overload;
    /// create the result table from a JSON-formated Data message
    // - the JSON data is parsed and formatted in-place, after having been
    // copied in the protected fPrivateCopy variable
    constructor Create(const aSQL, aJSON: RawUTF8); reintroduce; overload;
    /// create the result table from a JSON-formated Data message
    // - the JSON data is parsed and formatted in-place
    // - you can specify a set of TOrm classes which will be used to
    // retrieve the column exact type information
    // - please note that the supplied JSON buffer content will be changed
    constructor CreateFromTables(const Tables: array of TOrmClass;
      const aSQL: RawUTF8;
      JSONBuffer: PUTF8Char; JSONBufferLen: integer); reintroduce; overload;
    /// create the result table from a JSON-formated Data message
    // - you can specify a set of TOrm classes which will be used to
    // retrieve the column exact type information
    // - the JSON data is parsed and formatted in-place, after having been
    // copied in the protected fPrivateCopy variable
    constructor CreateFromTables(const Tables: array of TOrmClass; const
      aSQL, aJSON: RawUTF8); reintroduce; overload;
    /// initialize the result table from a JSON-formated Data message
    // - you can set the expected column types matching the results column layout
    // - the JSON data is parsed and formatted in-place
    constructor CreateWithColumnTypes(const ColumnTypes: array of TOrmFieldType;
      const aSQL: RawUTF8; JSONBuffer: PUTF8Char; JSONBufferLen: integer);
      reintroduce; overload;
    /// initialize the result table from a JSON-formated Data message
    // - you can set the expected column types matching the results column layout
    // - the JSON data is parsed and formatted in-place, after having been
    // copied in the protected fPrivateCopy variable
    constructor CreateWithColumnTypes(const ColumnTypes: array of TOrmFieldType;
      const aSQL, aJSON: RawUTF8); reintroduce; overload;

    /// update the result table content from a JSON-formated Data message
    // - return true on parsing success, false if no valid JSON data was found
    // - set Refreshed to true if the content changed
    // - update all content fields (Results[], RowCount, FieldCount, etc...)
    // - call SortFields() or IDColumnHide if was already done for this TOrmTable
    // - the conversion into PPUTF8CharArray is made inplace and is very fast
    // (only one memory buffer is allocated for the whole data)
    function UpdateFrom(const aJSON: RawUTF8; var Refreshed: boolean;
      PCurrentRow: PInteger): boolean;

    /// the private copy of the processed data buffer
    // - available e.g. for Create constructor using aJSON parameter,
    // or after the UpdateFrom() process
    // - 16 more bytes will be allocated, to allow e.g. proper SSE4.2 process
    // - this buffer is not to be access directly: this won't be a valid JSON
    // content, but a processed buffer, on which fResults[] elements point to -
    // it will contain unescaped text and numerical values, ending with #0
    property PrivateInternalCopy: RawUTF8 read fPrivateCopy;
  end;


  { -------------------- TOrmMany Definition }

  /// the kind of fields to be available in a Table resulting of
  // a TOrmMany.DestGetJoinedTable() method call
  // - Source fields are not available, because they will be always the same for
  // a same SourceID, and they should be available from the TOrm which
  // hold the TOrmMany instance
  // - jkDestID and jkPivotID will retrieve only DestTable.ID and PivotTable.ID
  // - jkDestFields will retrieve DestTable.* simple fields, or the fields
  // specified by aCustomFieldsCSV (the Dest table name will be added: e.g.
  // for aCustomFieldsCSV='One,Two', will retrieve DestTable.One, DestTable.Two)
  // - jkPivotFields will retrieve PivotTable.* simple fields, or the fields
  // specified by aCustomFieldsCSV (the Pivot table name will be added: e.g.
  // for aCustomFieldsCSV='One,Two', will retrieve PivotTable.One, PivotTable.Two)
  // - jkPivotAndDestAllFields for PivotTable.* and DestTable.* simple fields,
  // or will retrieve the specified aCustomFieldsCSV fields (with
  // the table name associated: e.g. 'PivotTable.One, DestTable.Two')
  TOrmManyJoinKind = (
    jkDestID, jkPivotID, jkDestFields, jkPivotFields, jkPivotAndDestFields);

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
      const aAndWhereSQL: RawUTF8; isDest: boolean): integer;
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
    // - the optional aAndWhereSQL parameter can be used to add any additional
    // condition to the WHERE statement (e.g. 'Salary>:(1000): AND Salary<:(2000):')
    // according to TOrmMany properties - note that you should better use
    // inlined parameters for faster processing on server, so you may call e.g.
    // ! aRec.FillMany(Client, 0, FormatUTF8('Salary>? AND Salary<?', [], [1000, 2000]));
    function FillMany(const aClient: IRestOrm; aSourceID: TID = 0;
      const aAndWhereSQL: RawUTF8 = ''): integer;
    /// retrieve all records associated to a particular Dest record, which
    // has a TOrmMany property
    // - returns the Count of records corresponding to this aSource record
    // - use a "for .." loop or a "while FillOne do ..." loop to iterate
    // through all Dest items, getting also any additional 'through' columns
    // - the optional aAndWhereSQL parameter can be used to add any additional
    // condition to the WHERE statement (e.g. 'Salary>:(1000): AND Salary<:(2000):')
    // according to TOrmMany properties - note that you should better use
    // inlined parameters for faster processing on server, so you may call e.g.
    // ! aRec.FillManyFromDest(Client, DestID, FormatUTF8('Salary>? AND Salary<?', [], [1000, 2000]));
    function FillManyFromDest(const aClient: IRestOrm; aDestID: TID;
      const aAndWhereSQL: RawUTF8 = ''): integer;
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
    // - aDestWhereSQL can specify the Dest table name in the statement, e.g.
    //  'Salary>:(1000): AND Salary<:(2000):' - note that you should better use
    // inlined parameters for faster processing on server, so you may use the
    // more convenient function
    // ! FormatUTF8('Salary>? AND Salary<?', [], [1000, 2000])
    // - this is faster than a manual FillMany() then loading each Dest,
    // because the condition is executed in the SQL statement by the server
    function DestGetJoined(const aClient: IRestOrm; const aDestWhereSQL: RawUTF8;
      aSourceID: TID; out DestIDs: TIDDynArray): boolean; overload;
    /// create a Dest record, then FillPrepare() it to retrieve all Dest items
    // associated to the current or specified Source ID, adding a WHERE condition
    // against the Dest rows
    // - if aSourceID is 0, the value is taken from current fSourceID field
    // (set by TOrm.Create)
    // - aDestWhereSQL can specify the Dest table name in the statement, e.g.
    // 'Salary>:(1000): AND Salary<:(2000):') according to TOrmMany
    // properties - note that you should better use such inlined parameters as
    // ! FormatUTF8('Salary>? AND Salary<?', [], [1000, 2000])
    function DestGetJoined(const aClient: IRestOrm; const aDestWhereSQL: RawUTF8;
      aSourceID: TID): TOrm; overload;
    /// create a TOrmTable, containing all specified Fields, after a JOIN
    // associated to the current or specified Source ID
    // - the Table will have the fields specified by the JoinKind parameter
    // - aCustomFieldsCSV can be used to specify which fields must be retrieved
    // (for jkDestFields, jkPivotFields, jkPivotAndDestFields) - default is all
    // - if aSourceID is 0, the value is taken from current fSourceID field
    // (set by TOrm.Create)
    // - aDestWhereSQL can specify the Dest table name in the statement, e.g.
    // 'Salary>:(1000): AND Salary<:(2000):') according to TOrmMany
    // properties - note that you should better use such inlined parameters as
    // ! FormatUTF8('Salary>? AND Salary<?', [], [1000, 2000])
    function DestGetJoinedTable(const aClient: IRestOrm;
      const aDestWhereSQL: RawUTF8; aSourceID: TID; JoinKind: TOrmManyJoinKind;
      const aCustomFieldsCSV: RawUTF8 = ''): TOrmTable;
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
    // - the optional aAndWhereSQL parameter can be used to add any additional
    // condition to the WHERE statement (e.g. 'Salary>:(1000): AND Salary<:(2000):')
    // according to TOrmMany properties - note that you should better use
    // such inlined parameters e.g. calling
    // ! FormatUTF8('Salary>? AND Salary<?', [], [1000, 2000])
    function IDWhereSQL(const aClient: IRestOrm; aID: TID; isDest: boolean;
      const aAndWhereSQL: RawUTF8 = ''): RawUTF8;
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
  // ! Model.VirtualTableRegister(TOrmDali1, TOrmVirtualTableJSON);
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
    class function RTreeSQLFunctionName: RawUTF8; virtual;
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
  // - any record which inherits from this class must have only oftUTF8Text
  // (RawUTF8) fields - with Delphi 2009+, you can have string fields
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
     // - internaly, we use RowID in the SQL statements, which is compatible
     // with both TOrm and TOrmFts3 kind of table
    property DocID: TID read GetID write fID;
  end;

  /// this base class will create a FTS3 table using the Porter Stemming algorithm
  // - see http://sqlite.org/fts3.html#tokenizer
  // - will generate tokenize=porter by convention from the class name
  TOrmFts3Porter = class(TOrmFts3);

  /// this base class will create a FTS3 table using the Unicode61 Stemming algorithm
  // - see http://sqlite.org/fts3.html#tokenizer
  // - will generate tokenize=unicode64 by convention from the class name
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
      const FieldName: RawUTF8; Options: TOrmInitializeTableOptions); override;
  end;

  /// this base class will create a FTS4 table using the Porter Stemming algorithm
  // - see http://sqlite.org/fts3.html#tokenizer
  // - will generate tokenize=porter by convention from the class name
  TOrmFts4Porter = class(TOrmFts4);

  /// this base class will create a FTS4 table using the Unicode61 Stemming algorithm
  // - see http://sqlite.org/fts3.html#tokenizer
  // - will generate tokenize=unicode64 by convention from the class name
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
  // - will generate tokenize=unicode64 by convention from the class name
  TOrmFts5Unicode61 = class(TOrmFts5);


  { -------------------- TOrmProperties Definitions }

  /// some information about a given TOrm class properties
  // - used internaly by TOrm, via a global cache handled by this unit:
  // you can access to each record's properties via TOrm.RecordProps class
  // - such a global cache saves some memory for each TOrm instance,
  // and allows faster access to most wanted RTTI properties
  TOrmProperties = class
  protected
    fLock: TRTLCriticalSection;
    fTable: TOrmClass;
    fTableRtti: TRttiJson;
    fHasNotSimpleFields: boolean;
    fHasTypeFields: TOrmFieldTypes;
    fFields: TOrmPropInfoList;
    fSimpleFields: TOrmPropInfoObjArray;
    fSQLTableName: RawUTF8;
    fCopiableFields: TOrmPropInfoObjArray;
    fManyFields: TOrmPropInfoRTTIManyObjArray;
    fJoinedFields: TOrmPropInfoRTTIIObjArray;
    fJoinedFieldsTable: TOrmClassDynArray;
    fDynArrayFields: TOrmPropInfoRTTIDynArrayObjArray;
    fDynArrayFieldsHasObjArray: boolean;
    fBlobCustomFields: TOrmPropInfoObjArray;
    fBlobFields: TOrmPropInfoRTTIObjArray;
    fFilters: TSynFilterOrValidateObjArrayArray;
    fRecordManySourceProp: TOrmPropInfoRTTIInstance;
    fRecordManyDestProp: TOrmPropInfoRTTIInstance;
    fSQLTableNameUpperWithDot: RawUTF8;
    fSQLFillPrepareMany: RawUTF8;
    fSQLTableSimpleFieldsNoRowID: RawUTF8;
    fSQLTableUpdateBlobFields: RawUTF8;
    fSQLTableRetrieveBlobFields: RawUTF8;
    fSQLTableRetrieveAllFields: RawUTF8;
    fRecordVersionField: TOrmPropInfoRTTIRecordVersion;
    fWeakZeroClass: TObject;
    /// the associated TOrmModel instances
    // - e.g. allow O(1) search of a TOrmClass in a model
    fModel: array of record
      /// one associated model
      Model: TOrmModel;
      /// the index in the Model.Tables[] array
      TableIndex: PtrInt;
      /// associated ORM parameters
      Properties: TOrmModelProperties;
    end;
    fModelMax: integer;
    fCustomCollation: TRawUTF8DynArray;
    /// add an entry in fModel[] / fModelMax
    procedure InternalRegisterModel(aModel: TOrmModel; aTableIndex: integer;
      aProperties: TOrmModelProperties);
  public
    /// initialize the properties content
    constructor Create(aTable: TOrmClass);
    /// release associated used memory
    destructor Destroy; override;

    /// return TRUE if the given name is either ID/RowID, either a property name
    function IsFieldName(const PropName: RawUTF8): boolean;
    /// return TRUE if the given name is either ID/RowID, either a property name,
    // or an aggregate function (MAX/MIN/AVG/SUM) on a valid property name
    function IsFieldNameOrFunction(const PropName: RawUTF8): boolean;
    /// set all bits corresponding to the supplied field names
    // - returns TRUE on success, FALSE if any field name is not existing
    function FieldBitsFromRawUTF8(const aFields: array of RawUTF8;
      var Bits: TFieldBits): boolean; overload;
    /// set all bits corresponding to the supplied field names
    // - returns the matching fields set
    function FieldBitsFromRawUTF8(const aFields: array of RawUTF8): TFieldBits; overload;
    /// set all bits corresponding to the supplied CSV field names
    // - returns TRUE on success, FALSE if any field name is not existing
    function FieldBitsFromCSV(const aFieldsCSV: RawUTF8;
      var Bits: TFieldBits): boolean; overload;
    /// set all bits corresponding to the supplied CSV field names, including ID
    // - returns TRUE on success, FALSE if any field name is not existing
    // - this overloaded method will identify ID/RowID field name, and set
    // withID output parameter according to its presence
    // - if aFieldsCSV='*', Bits will contain all simple fields, and withID=true
    function FieldBitsFromCSV(const aFieldsCSV: RawUTF8; var Bits: TFieldBits;
      out withID: boolean): boolean; overload;
    /// set all bits corresponding to the supplied CSV field names
    // - returns the matching fields set
    function FieldBitsFromCSV(const aFieldsCSV: RawUTF8): TFieldBits; overload;
    /// set all simple bits corresponding to the simple fields, excluding some
    // - could be a convenient alternative to FieldBitsFromCSV() if only some
    // fields are to be excluded
    // - returns the matching fields set
    function FieldBitsFromExcludingCSV(const aFieldsCSV: RawUTF8;
      aOccasion: TOrmOccasion = ooSelect): TFieldBits;
    /// set all bits corresponding to the supplied BLOB field type information
    // - returns TRUE on success, FALSE if blob field is not recognized
    function FieldBitsFromBlobField(aBlobField: PRttiProp;
      var Bits: TFieldBits): boolean;
    /// compute the CSV field names text from a set of bits
    function CSVFromFieldBits(const Bits: TFieldBits): RawUTF8;
    /// set all field indexes corresponding to the supplied field names
    // - returns TRUE on success, FALSE if any field name is not existing
    function FieldIndexDynArrayFromRawUTF8(const aFields: array of RawUTF8;
      var Indexes: TFieldIndexDynArray): boolean; overload;
    /// set all field indexes corresponding to the supplied field names
    // - returns the matching fields set
    function FieldIndexDynArrayFromRawUTF8(
      const aFields: array of RawUTF8): TFieldIndexDynArray; overload;
    /// set all field indexes corresponding to the supplied CSV field names
    // - returns TRUE on success, FALSE if any field name is not existing
    function FieldIndexDynArrayFromCSV(const aFieldsCSV: RawUTF8;
      var Indexes: TFieldIndexDynArray): boolean; overload;
    /// set all field indexes corresponding to the supplied CSV field names
    // - returns the matching fields set
    function FieldIndexDynArrayFromCSV(
      const aFieldsCSV: RawUTF8): TFieldIndexDynArray; overload;
    /// set all field indexes corresponding to the supplied BLOB field type information
    // - returns TRUE on success, FALSE if blob field is not recognized
    function FieldIndexDynArrayFromBlobField(aBlobField: PRttiProp;
      var Indexes: TFieldIndexDynArray): boolean;
    /// retrieve a Field property RTTI information from a Property Name
    // - this version returns nil if the property is not a BLOB field
    function BlobFieldPropFromRawUTF8(const PropName: RawUTF8): PRttiProp;
    /// retrieve a Field property RTTI information from a Property Name
    // - this version returns nil if the property is not a BLOB field
    function BlobFieldPropFromUTF8(PropName: PUTF8Char; PropNameLen: integer): PRttiProp;

    /// append a field name to a RawUTF8 Text buffer
    // - if FieldIndex=VIRTUAL_TABLE_ROWID_COLUMN (-1), appends 'RowID' or
    // 'ID' (if ForceNoRowID=TRUE) to Text
    // - on error (i.e. if FieldIndex is out of range) will return TRUE
    // - otherwise, will return FALSE and append the field name to Text
    function AppendFieldName(FieldIndex: integer; var Text: RawUTF8;
      ForceNoRowID: boolean): boolean;
    /// return the first unique property of kind RawUTF8
    // - this property is mainly the "Name" property, i.e. the one with
    // "stored AS_UNIQUE" (i.e. "stored false") definition on most TOrm
    // - if ReturnFirstIfNoUnique is TRUE and no unique property is found,
    // the first RawUTF8 property is returned anyway
    // - returns '' if no matching field was found
    function MainFieldName(ReturnFirstIfNoUnique: boolean = false): RawUTF8;
    /// return the SQLite3 field datatype for each specified field
    // - set to '' for fields with no column created in the database (e.g. oftMany)
    // - returns e.g. ' INTEGER, ' or ' TEXT COLLATE SYSTEMNOCASE, '
    function OrmFieldTypeToSQL(FieldIndex: integer): RawUTF8;
    /// set a custom SQlite3 text column collation for a specified field
    // - can be used e.g. to override the default COLLATE SYSTEMNOCASE of RawUTF8
    // - collations defined within our mormot.db.raw.sqlite3 unit are named BINARY, NOCASE,
    // RTRIM and our custom SYSTEMNOCASE, ISO8601, WIN32CASE, WIN32NOCASE
    // - do nothing if FieldIndex is not valid, and returns false
    // - could be set in overridden class procedure TOrm.InternalDefineModel
    // so that it will be common to all database models, for both client and server
    function SetCustomCollation(FieldIndex: integer;
      const aCollationName: RawUTF8): boolean; overload;
    /// set a custom SQlite3 text column collation for a specified field
    // - overloaded method which expects the field to be named
    function SetCustomCollation(const aFieldName, aCollationName: RawUTF8): boolean; overload;
    /// set a custom SQlite3 text column collation for a given field type
    // - can be used e.g. to override ALL default COLLATE SYSTEMNOCASE of RawUTF8,
    // or the default COLLATE ISO8601 of TDateTime, and let the generated SQLite3
    // file be available outside the scope of mORMot's SQLite3 engine
    // - collations defined within our mormot.db.raw.sqlite3 unit are named BINARY, NOCASE,
    // RTRIM and our custom SYSTEMNOCASE, ISO8601, WIN32CASE, WIN32NOCASE
    // - could be set in overridden class procedure TOrm.InternalDefineModel
    // so that it will be common to all database models, for both client and server
    // - note that you may inherit from TOrmNoCase to use the NOCASE
    // standard SQLite3 collation for all descendant ORM objects
    procedure SetCustomCollationForAll(aFieldType: TOrmFieldType;
      const aCollationName: RawUTF8);
    /// allow to validate length of all text published properties of this table
    // - the "index" attribute of the RawUTF8/string published properties could
    // be used to specify a maximum length for external VARCHAR() columns
    // - SQLite3 will just ignore this "index" information, but it could be
    // handy to be able to validate the value length before sending to the DB
    // - this method will create TSynValidateText corresponding to the maximum
    // field size specified by the "index" attribute, to validate before write
    // - will expect the "index" value to be in UTF-16 codepoints, unless
    // IndexIsUTF8Length is set to TRUE, indicating UTF-8 length in "index"
    procedure SetMaxLengthValidatorForTextFields(IndexIsUTF8Length: boolean = false);
    /// allow to filter the length of all text published properties of this table
    // - the "index" attribute of the RawUTF8/string published properties could
    // be used to specify a maximum length for external VARCHAR() columns
    // - SQLite3 will just ignore this "index" information, but it could be
    // handy to be able to filter the value length before sending to the DB
    // - this method will create TSynFilterTruncate corresponding to the maximum
    // field size specified by the "index" attribute, to filter before write
    // - will expect the "index" value to be in UTF-16 codepoints, unless
    // IndexIsUTF8Length is set to TRUE, indicating UTF-8 length in "index"
    procedure SetMaxLengthFilterForTextFields(IndexIsUTF8Length: boolean = false);
    /// customize the TDocVariant options for all variant published properties
    // - will change the TOrmPropInfoRTTIVariant.DocVariantOptions value
    // - use e.g. as SetVariantFieldDocVariantOptions(JSON_OPTIONS_FAST_EXTENDED)
    // - see also TOrmNoCaseExtended root class
    procedure SetVariantFieldsDocVariantOptions(const Options: TDocVariantOptions);
    /// return the UTF-8 encoded SQL statement source to alter the table for
    //  adding the specified field
    // - returns something like 'ALTER TABLE tablename ADD COLUMN coldef UNIQUE'
    function SQLAddField(FieldIndex: integer): RawUTF8;

    /// create a TJSONWriter, ready to be filled with TOrm.GetJSONValues
    // - you can use TOrmProperties.FieldBitsFromCSV() or
    // TOrmProperties.FieldBitsFromRawUTF8() to compute aFields
    function CreateJSONWriter(JSON: TStream; Expand, withID: boolean;
      const aFields: TFieldBits; KnownRowsCount: integer;
      aBufSize: integer = 8192): TJSONSerializer; overload;
    /// create a TJSONWriter, ready to be filled with TOrm.GetJSONValues(W)
    // - you can use TOrmProperties.FieldBitsFromCSV() or
    // TOrmProperties.FieldBitsFromRawUTF8() to compute aFields
    function CreateJSONWriter(JSON: TStream; Expand, withID: boolean;
      const aFields: TFieldIndexDynArray; KnownRowsCount: integer;
      aBufSize: integer = 8192): TJSONSerializer; overload;
    /// create a TJSONWriter, ready to be filled with TOrm.GetJSONValues(W)
    // - this overloaded method will call FieldBitsFromCSV(aFieldsCSV,bits,withID)
    // to retrieve the bits just like a SELECT (i.e. '*' for simple fields)
    function CreateJSONWriter(JSON: TStream; Expand: boolean;
      const aFieldsCSV: RawUTF8; KnownRowsCount: integer;
      aBufSize: integer = 8192): TJSONSerializer; overload;
    /// set the W.ColNames[] array content + W.AddColumns
    procedure SetJSONWriterColumnNames(W: TJSONSerializer; KnownRowsCount: integer);
    /// save the TOrm RTTI into a binary header
    // - used e.g. by TRestStorageInMemory.SaveToBinary()
    procedure SaveBinaryHeader(W: TBufferWriter);
    /// ensure that the TOrm RTTI matches the supplied binary header
    // - used e.g. by TRestStorageInMemory.LoadFromBinary()
    function CheckBinaryHeader(var R: TFastReader): boolean;
    /// convert a JSON array of simple field values into a matching JSON object
    function SaveSimpleFieldsFromJsonArray(var P: PUTF8Char;
      var EndOfObject: AnsiChar; ExtendedJSON: boolean): RawUTF8;

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
    procedure AddFilterOrValidate(const aFieldName: RawUTF8;
      aFilter: TSynFilterOrValidate); overload;

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
      aRecordSize: cardinal; const aName: RawUTF8; aPropertyPointer: pointer;
      aAttributes: TOrmPropInfoAttributes; aFieldWidth: integer;
      aData2Text: TOnSQLPropInfoRecord2Text = nil;
      aText2Data: TOnSQLPropInfoRecord2Data = nil);
    /// add a custom record property from its RTTI definition
    // - handle any kind of record with TypeInfo() generated
    // - use this method within InternalRegisterCustomProperties overridden method
    // to define a custom record property containing reference-counted types
    // - main parameters are the record RTTI information, and the property pointer
    // - add an TOrmPropInfoRecordRTTI instance to the internal list
    // - can be used as such:
    // !class procedure TOrmMyRecord.InternalRegisterCustomProperties(
    // !  Props: TOrmProperties);
    // !begin
    // !  Props.RegisterCustomRTTIRecordProperty(self, TypeInfo(TMyRec),
    // !    'RecField', @TOrmMyRecord(nil).fRecField);
    // !end;
    procedure RegisterCustomRTTIRecordProperty(aTable: TClass;
      aRecordInfo: PRttiInfo; const aName: RawUTF8; aPropertyPointer: pointer;
      aAttributes: TOrmPropInfoAttributes = []; aFieldWidth: integer = 0;
      aData2Text: TOnSQLPropInfoRecord2Text = nil;
      aText2Data: TOnSQLPropInfoRecord2Data = nil);
    /// add a custom property from its RTTI definition stored as JSON
    // - handle any kind of record with TypeInfo() generated
    // - use this method within InternalRegisterCustomProperties overridden method
    // to define a custom record property containing reference-counted types
    // - main parameters are the record RTTI information, and the property pointer
    // - add an TOrmPropInfoCustomJSON instance to the internal list
    // - can be used as such:
    // !class procedure TOrmMyRecord.InternalRegisterCustomProperties(
    // !  Props: TOrmProperties);
    // !begin
    // !  Props.RegisterCustomPropertyFromRTTI(self, TypeInfo(TMyRec),
    // !    'RecField', @TOrmMyRecord(nil).fRecField);
    // !end;
    procedure RegisterCustomPropertyFromRTTI(aTable: TClass;
      aTypeInfo: PRttiInfo; const aName: RawUTF8; aPropertyPointer: pointer;
      aAttributes: TOrmPropInfoAttributes = []; aFieldWidth: integer = 0);
    /// add a custom property from its type name, stored as JSON
    // - handle any kind of registered record, including TGUID
    // - use this method within InternalRegisterCustomProperties overridden method
    // to define a custom record property containing reference-counted types
    // - main parameters are the record RTTI information, and the property pointer
    // - add an TOrmPropInfoCustomJSON instance to the internal list
    // - can be used as such:
    // !class procedure TOrmMyRecord.InternalRegisterCustomProperties(
    // !  Props: TOrmProperties);
    // !begin
    // !  Props.RegisterCustomPropertyFromTypeName(self, 'TGUID', 'GUID',
    // !    @TOrmMyRecord(nil).fGUID, [aIsUnique], 38);
    // !end;
    procedure RegisterCustomPropertyFromTypeName(aTable: TClass;
      const aTypeName, aName: RawUTF8; aPropertyPointer: pointer;
      aAttributes: TOrmPropInfoAttributes = []; aFieldWidth: integer = 0);

    /// fast access to the RTTI properties attribute
    property TableRtti: TRttiJson read fTableRtti;
    /// if this class has any BLOB or TOrmRecordMany fields
    // - i.e. some fields to be ignored
    property HasNotSimpleFields: boolean read fHasNotSimpleFields;
    /// set of field types appearing in this record
    property HasTypeFields: TOrmFieldTypes read fHasTypeFields;
    /// list all fields, as retrieved from RTTI
    property Fields: TOrmPropInfoList read fFields;
    /// list all "simple" fields of this TOrm
    // - by default, the RawBlob and TOrmMany fields are not included
    // into this set: they must be read specificaly (in order to spare
    // bandwidth for BLOBs)
    // - dynamic arrays belong to simple fields: they are sent with other
    // properties content
    // - match inverted NOT_SIMPLE_FIELDS mask
    property SimpleFields: TOrmPropInfoObjArray read fSimpleFields;
    /// list all fields which can be copied from one TOrm instance to another
    // - match COPIABLE_FIELDS mask, i.e. all fields except oftMany
    property CopiableFields: TOrmPropInfoObjArray read fCopiableFields;
    /// list all TOrmMany fields of this TOrm
    property ManyFields: TOrmPropInfoRTTIManyObjArray read fManyFields;
    /// list all TOrm fields of this TOrm
    // - ready to be used by TOrmTableJSON.CreateFromTables()
    // - i.e. the class itself then, all fields of type oftID (excluding oftMany)
    property JoinedFields: TOrmPropInfoRTTIIObjArray read fJoinedFields;
    /// wrapper of all nested TOrm class of this TOrm
    // - ready to be used by TOrmTableJSON.CreateFromTables()
    // - i.e. the class itself as JoinedFieldsTable[0], then, all nested
    // TOrm published properties (of type oftID, ergo excluding oftMany)
    // - equals nil if there is no nested TOrm property (i.e. JoinedFields=nil)
    property JoinedFieldsTable: TOrmClassDynArray read fJoinedFieldsTable;
    /// list of all oftBlobDynArray fields of this TOrm
    property DynArrayFields: TOrmPropInfoRTTIDynArrayObjArray read fDynArrayFields;
    /// TRUE if any of the oftBlobDynArray fields of this TOrm is a T*ObjArray
    // - used e.g. by TOrm.Destroy to release all owned nested instances
    property DynArrayFieldsHasObjArray: boolean read fDynArrayFieldsHasObjArray;
    /// list of all oftBlobCustom fields of this TOrm
    // - have been defined e.g. as TOrmPropInfoCustom custom definition
    property BlobCustomFields: TOrmPropInfoObjArray read fBlobCustomFields;
    /// list all BLOB fields of this TOrm
    // - i.e. generic oftBlob fields (not oftBlobDynArray, oftBlobCustom nor
    // oftBlobRecord)
    property BlobFields: TOrmPropInfoRTTIObjArray read fBlobFields;
    /// all TSynFilter or TSynValidate instances registered per each field
    // - since validation and filtering are used within some CPU-consuming
    // part of the framework (like UI edition), both filters and validation
    // rules are grouped in the same list
    property Filters: TSynFilterOrValidateObjArrayArray read fFilters;
    /// for a TOrmMany class, points to the Source property RTTI
    property RecordManySourceProp: TOrmPropInfoRTTIInstance read fRecordManySourceProp;
    /// for a TOrmMany class, points to the Dest property RTTI
    property RecordManyDestProp: TOrmPropInfoRTTIInstance read fRecordManyDestProp;
    /// points to any TRecordVersion field
    // - contains nil if no such oftRecordVersion field do exist
    // - will be used by low-level storage engine to compute and store the
    // monotonic version number during any write operation
    property RecordVersionField: TOrmPropInfoRTTIRecordVersion read fRecordVersionField;
    /// the Table name in the database in uppercase with a final '.'
    // - e.g. 'TEST.' for TOrmTest class
    // - can be used with IdemPChar() for fast check of a table name
    property SQLTableNameUpperWithDot: RawUTF8 read fSQLTableNameUpperWithDot;
    /// returns 'COL1,COL2' with all COL* set to simple field names
    // - same value as SQLTableSimpleFields[false,false]
    // - this won't change depending on the ORM settings: so it can be safely
    // computed here and not in TOrmModelProperties
    // - used e.g. by TOrm.GetSQLValues
    property SQLTableSimpleFieldsNoRowID: RawUTF8 read fSQLTableSimpleFieldsNoRowID;
    /// returns 'COL1=?,COL2=?' with all BLOB columns names
    // - used e.g. by TRestServerDB.UpdateBlobFields()
    property SQLTableUpdateBlobFields: RawUTF8 read fSQLTableUpdateBlobFields;
    /// returns 'COL1,COL2' with all BLOB columns names
    // - used e.g. by TRestServerDB.RetrieveBlobFields()
    property SQLTableRetrieveBlobFields: RawUTF8 read fSQLTableRetrieveBlobFields;
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
    /// bit set to 1 for an unique field
    // - an unique field is defined as "stored AS_UNIQUE" (i.e. "stored false")
    // in its property definition
    IsUniqueFieldsBits: TFieldBits;
    /// bit set to 1 for the smallest simple fields
    // - i.e. excluding non only oftBlob and oftMany, but also oftVariant,
    // oftBlobDynArray, oftBlobCustom and oftUTF8Custom fields
    // - may be used to minimize the transmitted content, e.g. when serializing
    // to JSON for the most
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
  published
    /// the TOrm class
    property Table: TOrmClass read fTable;
    /// the Table name in the database, associated with this TOrm class
    // - 'TSQL' or 'TORM' chars are trimmed at the beginning of the ClassName
    // - or the ClassName is returned as is, if no 'TSQL' or 'TORM' at first
    property SQLTableName: RawUTF8 read fSQLTableName;
    /// returns 'COL1,COL2' with all COL* set to all field names, including
    // RowID, TRecordVersion and BLOBs
    // - this won't change depending on the ORM settings: so it can be safely
    // computed here and not in TOrmModelProperties
    // - used e.g. by TRest.InternalListJSON()
    property SQLTableRetrieveAllFields: RawUTF8 read fSQLTableRetrieveAllFields;
  end;

  /// how TOrmModel.URIMatch() will compare an URI
  // - will allow to make a difference about case-sensitivity
  TRestModelMatch = (
    rmNoMatch, rmMatchExact, rmMatchWithCaseChange);

  /// the kind of SQlite3 (virtual) table
  // - TOrmFts3/4/5 will be associated with vFTS3/vFTS4/vFTS5 values,
  // TOrmRTree/TOrmRTreeInteger with rRTree/rRTreeInteger, any native
  // SQlite3 table as vSQLite3, and a TOrmVirtualTable*ID as
  // rCustomForcedID/rCustomAutoID
  // - a plain TOrm class can be defined as rCustomForcedID (e.g. for
  // TOrmMany) after registration for an external DB via a call to
  // VirtualTableExternalRegister() from mormot.orm.sql unit
  TOrmVirtualKind = (
    rSQLite3, rFTS3, rFTS4, rFTS5, rRTree, rRTreeInteger,
    rCustomForcedID, rCustomAutoID);

  /// pre-computed SQL statements for ORM operations for a given
  // TOrmModelProperties instance
  TOrmModelPropertiesSQL = record
    /// the simple field names in a SQL SELECT compatible format: 'COL1,COL2' e.g.
    // - format is
    // ! SQL.TableSimpleFields[withID: boolean; withTableName: boolean]
    // - returns '*' if no field is of RawBlob/TOrmMany kind
    // - returns 'COL1,COL2' with all COL* set to simple field names if withID is false
    // - returns 'ID,COL1,COL2' with all COL* set to simple field names if withID is true
    // - returns 'Table.ID,Table.COL1,Table.COL2' if withTableName and withID are true
    TableSimpleFields: array[boolean, boolean] of RawUTF8;
    /// the SQL statement for reading all simple fields and RowID
    // - to be checked if we may safely call EngineList()
    SelectAllWithRowID: RawUTF8;
    /// the SQL statement for reading all simple fields with ID
    // - to be checked if we may safely call EngineList()
    SelectAllWithID: RawUTF8;
    /// the JOINed SQL statement for reading all fields with ID, including
    // nested TOrm pre-allocated instances
    // - is '' if there is no nested TOrm
    SelectAllJoined: RawUTF8;
    /// the updated simple fields exposed as 'COL1=?,COL2=?'
    // - excluding ID (but including TCreateTime fields - as used in
    // TOrmVirtualTableExternal.Update method)
    // - to be used e.g. for UPDATE statements
    UpdateSetSimple: RawUTF8;
    /// all updated fields exposed as 'COL1=?,COL2=?'
    // - excluding ID (but including TCreateTime fields - as used in
    // TOrmVirtualTableExternal.Update method)
    // - to be used e.g. for UPDATE statements
    UpdateSetAll: RawUTF8;
    /// all fields, excluding the ID field, exposed as 'COL1,COL2'
    // - to be used e.g. in TOrmVirtualTableExternal.Insert()
    InsertSet: RawUTF8;
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
    fTableName: RawUTF8;
    fRowIDFieldName: RawUTF8;
    fExtFieldNames: TRawUTF8DynArray;
    fExtFieldNamesUnQuotedSQL: TRawUTF8DynArray;
    fSQL: TOrmModelPropertiesSQL;
    fFieldNamesMatchInternal: TFieldBits;
    fOptions: TOrmPropertiesMappingOptions;
    fAutoComputeSQL: boolean;
    fMappingVersion: cardinal;
    /// fill fRowIDFieldName/fSQL with the current information
    procedure ComputeSQL;
  public
    /// add a custom field mapping
    // - will re-compute all needed SQL statements as needed, and initialize
    // fSortedFieldsName[] and fSortedFieldsIndex[] internal sorted arrays
    // - can be used e.g. as
    // ! aModel.Props[TOrmMyExternal].ExternalDB.MapField('IntField', 'ExtField');
    // - since it returns a POrmPropertiesMapping instance, you can
    // chain MapField().MapField().MapField(); calls to map several fields
    function MapField(
      const InternalName, ExternalName: RawUTF8): POrmPropertiesMapping;
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
      const InternalExternalPairs: array of RawUTF8): POrmPropertiesMapping;
  public
    /// initialize the field mapping for a given TOrm
    // - if AutoComputeSQL is true, will pre-compute all needed SQL from the
    // supplied information
    // - will left void fSortedFieldsName[] and fSortedFieldsIndex[], to disable
    // custom field mapping
    procedure Init(Table: TOrmClass; const MappedTableName: RawUTF8;
      MappedConnection: TObject; AutoComputeSQL: boolean;
      MappingOptions: TOrmPropertiesMappingOptions);
    /// map a field name from its internal name to its external name
    // - raise an EOrmException if the supplied field name is not defined in
    // the TOrm as ID or a published property
    function InternalToExternal(const FieldName: RawUTF8): RawUTF8; overload;
    /// map a field name from its internal name to its external name
    // - raise an EOrmException if the supplied field name is not defined in
    // the TOrm as ID or a published property
    function InternalToExternal(BlobField: PRttiProp): RawUTF8; overload;
    /// map a CSV list of field names from its internals to its externals values
    // - raise an EOrmException if any of the supplied field name is not defined
    // in the TOrm as ID or as property (RowIDFieldName or FieldNames[])
    // - to be used for a simple CSV (e.g. for INSERT/SELECT statements):
    // ! ExtCSV := InternalCSVToExternalCSV('ID,Name');
    // - or for a more complex CSV (e.g. for UPDATE statements);
    // ! ExtCSV := InternalCSVToExternalCSV('ID=?,Name=?', '=?, '=?');
    function InternalCSVToExternalCSV(const CSVFieldNames: RawUTF8;
      const Sep: RawUTF8 = ','; const SepEnd: RawUTF8 = ''): RawUTF8;
    /// create a list of external field names, from the internal field names
    // - raise an EOrmException if any of the supplied field name is not defined
    // in the TOrm as ID or a published property
    // - if IntFieldIndex is set, it will store an array of internal field
    // indexes, i.e. -1 for ID or index in in FieldNames[] for other fields
    procedure InternalToExternalDynArray(const IntFieldNames: array of RawUTF8;
      out result: TRawUTF8DynArray; IntFieldIndex: PIntegerDynArray = nil);
    /// map an external field name into its internal field name
    // - return '' if the external field name is not RowIDFieldName nor in
    // FieldNames[]
    function ExternalToInternalOrNull(const ExtFieldName: RawUTF8): RawUTF8;
    /// map an external field name into its internal field index
    // - returns the index >=0 in FieldNames[] for a matching external field
    // - returns -1 if the field name is RowIDFieldName
    // - returns -2 if the field name is not mapped
    function ExternalToInternalIndex(const ExtFieldName: RawUTF8): integer;
    /// append a field name to a RawUTF8 Text buffer
    // - if FieldIndex=VIRTUAL_TABLE_ROWID_COLUMN (-1), appends RowIDFieldName
    // - on error (i.e. if FieldIndex is out of range) will return TRUE
    // - otherwise, will return FALSE and append the external field name to Text
    function AppendFieldName(FieldIndex: integer; var Text: RawUTF8): boolean;
    /// return the field name as RawUTF8 value
    // - if FieldIndex=VIRTUAL_TABLE_ROWID_COLUMN (-1), appends RowIDFieldName
    // - otherwise, will return the external field name
    function FieldNameByIndex(FieldIndex: integer): RawUTF8;

    /// opaque object used on the Server side to specify e.g. the DB connection
    // - will define such a generic TObject, to avoid any unecessary type
    // dependency to other units, e.g. mormot.db.* or mormot.rest.*
    // - in practice, will be assigned by VirtualTableExternalRegister() to
    // a TSQLDBConnectionProperties instance in mormot.orm.sql.pas, or by
    // StaticMongoDBRegister() to a TMongoCollection instance, or by
    // TDDDRepositoryRestObjectMapping.Create to its associated TRest
    // - in ORM context, equals nil if the table is internal to SQLite3:
    // ! if Server.Model.Props[TOrmArticle].ExternalDB.ConnectionProperties = nil then
    // !   // this is not an external table, since Init() was not called
    property ConnectionProperties: TObject read fConnectionProperties;
    /// the associated TOrmProperties
    property Properties: TOrmProperties read fProps;
    /// used on the Server side to specify the external DB table name
    // - e.g. for including a schema name or an existing table name, with an
    // OleDB/MSSQL/Oracle/MySQL/PostgreSQL/Jet/SQLite3 backend
    // - equals SQLTableName by default (may be overridden e.g. by mormot.orm.sql's
    // VirtualTableExternalRegister procedure)
    property TableName: RawUTF8 read fTableName;
    /// pre-computed SQL statements for this external TOrm in this model
    // - you can use those SQL statements directly with the external engine
    // - filled if AutoComputeSQL was set to true in Init() method
    property SQL: TOrmModelPropertiesSQL read fSQL;
    /// the ID/RowID customized external field name, if any
    // - is 'ID' by default, since 'RowID' is a reserved column name for some
    // database engines (e.g. Oracle)
    // - can be customized e.g. via
    // ! aModel.Props[TOrmMyExternal].ExternalDB.MapField('ID', 'ExternalID');
    property RowIDFieldName: RawUTF8 read fRowIDFieldName;
    /// the external field names, following fProps.Props.Field[] order
    // - excluding ID/RowID field, which is stored in RowIDFieldName
    property ExtFieldNames: TRawUTF8DynArray read fExtFieldNames;
    /// the unquoted external field names, following fProps.Props.Field[] order
    // - excluding ID/RowID field, which is stored in RowIDFieldName
    // - in respect to ExtFieldNames[], this array will never quote the field name
    property ExtFieldNamesUnQuotedSQL: TRawUTF8DynArray read fExtFieldNamesUnQuotedSQL;
    /// each bit set, following fProps.Props.Field[]+1 order (i.e. 0=ID,
    // 1=Field[0], ...), indicates that this external field name
    // has not been mapped
    property FieldNamesMatchInternal: TFieldBits read fFieldNamesMatchInternal;
    /// how the mapping process will take place
    property Options: TOrmPropertiesMappingOptions read fOptions;
    /// each time MapField/MapFields is called, this number will increase
    // - can be used to track mapping changes in real time
    property MappingVersion: cardinal read fMappingVersion;
  end;



  { -------------------- TOrmModel TOrmModelProperties Definitions }

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
    fFTSWithoutContentFields: RawUTF8;
    procedure SetKind(Value: TOrmVirtualKind);
    function GetProp(const PropName: RawUTF8): TOrmPropInfo;
  public
    /// pre-computed SQL statements for this TOrm in this model
    // - those statements will work for internal tables, not for external
    // tables with mapped table or fields names
    SQL: TOrmModelPropertiesSQL;
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
    function SQLFromSelectWhere(const SelectFields, Where: RawUTF8): RawUTF8;
    /// define if a FTS4 virtual table will not store its content, but will
    // be defined as an "external content" FTS4 table
    // - see https://www.sqlite.org/fts3.html#section_6_2_2
    // - the virtual table will be created with content="ContentTableName",
    // and all fields of the FTS4 table
    // - by design, all fields of the FTS4 table should exist in the source
    // ContentTable - otherwise an exception is raised
    // - the indexed text will be assigned to the FTS4 table, using triggers
    // generated by TOrmFts4.InitializeTable at table creation
    // - note that FTS3 does not support this feature
    procedure FTS4WithoutContent(ContentTable: TOrmClass);

    /// the table index of this TOrm in the associated Model
    property TableIndex: integer read fTableIndex;
    /// direct access to a property RTTI information, by name
    property Prop[const PropName: RawUTF8]: TOrmPropInfo read GetProp; default;
  published
    /// the shared TOrmProperties information of this TOrm
    // - as retrieved from RTTI
    property Props: TOrmProperties read fProps;
    /// define if is a normal table (rSQLite3), an FTS/R-Tree virtual
    // table or a custom TOrmVirtualTable*ID (rCustomForcedID/rCustomAutoID)
    // - when set, all internal SQL statements will be (re)created, depending of
    // the expected ID/RowID column name expected (i.e. SQLTableSimpleFields[]
    // and SQLSelectAll[] - SQLUpdateSet and SQLInsertSet do not include ID)
    property Kind: TOrmVirtualKind
      read fKind write SetKind default rSQLite3;
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
  // low-level database methods (e.g. TSQLDataBase.GetTableNames), since the
  // tables may not exist in the main SQLite3 database, but in-memory or external
  // - don't modify the order of Tables inside this Model, if you publish
  // some TRecordReference property in any of your tables
  TOrmModel = class
  private
    fTables: TOrmClassDynArray;
    fRoot: RawUTF8;
    fRootUpper: RawUTF8;
    fTablesMax: integer;
    fTableProps: TOrmModelPropertiesObjArray;
    fCustomCollationForAll: array[TOrmFieldType] of RawUTF8;
    fOnClientIdle: TOnIdleSynBackgroundThread;
    /// contains the TRest caller of CreateOwnedStream()
    fOwner: TObject;
    /// for every table, contains a locked record list
    // - very fast, thanks to the use of a dynamic array with one entry by table
    fLocks: TOrmLocksDynArray;
    /// for fastest SQL Table name lookup via O(log(n)) binary search
    fSortedTablesNameUpper: TRawUTF8DynArray;
    fSortedTablesNameIndex: TIntegerDynArray;
    /// will contain the registered TOrmVirtualTableClass modules
    fVirtualTableModule: array of TClass;
    /// all TRecordReference and TOrm properties of the model
    fRecordReferences: TOrmModelReferenceDynArray;
    fIDGenerator: TSynUniqueIdentifierGenerators;
    procedure SetRoot(const aRoot: RawUTF8);
    procedure SetTableProps(aIndex: integer);
    function GetTableProps(aClass: TOrmClass): TOrmModelProperties;
    /// get the enumerate type information about the possible actions to be
    function GetLocks(aTable: TOrmClass): POrmLocks;
    function GetTable(const SQLTableName: RawUTF8): TOrmClass;
    function GetTableExactIndex(const TableName: RawUTF8): PtrInt;
    function GetTableExactClass(const TableName: RawUTF8): TOrmClass;
  public
    /// initialize the Database Model
    // - set the Tables to be associated with this Model, as TOrm classes
    // - set the optional Root URI path of this Model
    // - initialize the fIsUnique[] array from "stored AS_UNIQUE" (i.e. "stored
    // false") published properties of every TOrmClass
    constructor Create(const Tables: array of TOrmClass;
      const aRoot: RawUTF8 = 'root'); reintroduce; overload;
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
    // ! fSQLAuthUserClass := Model.AddTableInherited(TAuthUser);
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
    // - expects SQLTableName to be SQL-like formatted (i.e. without TOrm[Record])
    function GetTableIndex(const SQLTableName: RawUTF8): PtrInt; overload;
    /// get the index of a table in Tables[], optionally raising EModelException
    function GetTableIndexSafe(aTable: TOrmClass;
      RaiseExceptionIfNotExisting: boolean): PtrInt;
    /// get the index of a table in Tables[]
    // - expects SQLTableName to be SQL-like formatted (i.e. without TOrm[Record])
    function GetTableIndexPtr(SQLTableName: PUTF8Char): PtrInt;
    /// return the UTF-8 encoded SQL source to create the table
    function GetSQLCreate(aTableIndex: integer): RawUTF8;
    /// return the UTF-8 encoded SQL source to add the corresponding field
    // via a "ALTER TABLE" statement
    function GetSQLAddField(aTableIndex, aFieldIndex: integer): RawUTF8;
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
    // using GetTableNameFromSQLSelect() function
    // - if EnsureUniqueTableInFrom is TRUE, it will check that only one Table
    // is in the FROM clause, otherwise it will return the first Table specified
    function GetTableIndexFromSQLSelect(const SQL: RawUTF8;
      EnsureUniqueTableInFrom: boolean): integer;
    /// try to retrieve one or several TOrmClass from a SQL statement
    // - naive search of '... FROM Table1,Table2' pattern in the supplied SQL,
    // using GetTableNamesFromSQLSelect() function
    function GetTablesFromSQLSelect(const SQL: RawUTF8): TOrmClassDynArray;
    /// try to retrieve one or several table index from a SQL statement
    // - naive search of '... FROM Table1,Table2' pattern in the supplied SQL,
    // using GetTableNamesFromSQLSelect() function
    function GetTableIndexesFromSQLSelect(const SQL: RawUTF8): TIntegerDynArray;
    /// check if the supplied URI matches the model's Root property
    // - allows sub-domains, e.g. if Root='root/sub1', then '/root/sub1/toto' and
    // '/root/sub1?n=1' will match, whereas '/root/sub1nope/toto' won't
    // - the returned enumerates allow to check if the match was exact (e.g.
    // 'root/sub' matches exactly Root='root'), or with character case
    // approximation (e.g. 'Root/sub' approximates Root='root')
    function URIMatch(const URI: RawUTF8): TRestModelMatch;
    /// returns the URI corresponding to a given table, i.e. 'root/table'
    function GetURI(aTable: TOrmClass): RawUTF8;
    /// return the 'root/table/ID' URI
    function GetURIID(aTable: TOrmClass; aID: TID): RawUTF8;
    /// return the 'root/table/ID/method' URI
    function GetURICallBack(const aMethodName: RawUTF8;
      aTable: TOrmClass; aID: TID): RawUTF8;
    /// compute the SQL statement to be executed for a specific SELECT on Tables
    // - you can set multiple Table class in Tables: the statement will contain the
    // table name ('SELECT T1.F1,T1.F2,T1.F3,T2.F1,T2.F2 FROM T1,T2 WHERE ..' e.g.)
    function SQLFromSelectWhere(const Tables: array of TOrmClass;
      const SQLSelect, SQLWhere: RawUTF8): RawUTF8;
    /// set a custom SQlite3 text column collation for all fields of a given
    // type for all TOrm of this model
    // - can be used e.g. to override ALL default COLLATE SYSTEMNOCASE of RawUTF8,
    // or COLLATE ISO8601 for TDateTime, and let the generated SQLite3 file be
    // available outside the scope of mORMot's SQLite3 engine
    // - collations defined within our mormot.db.raw.sqlite3 unit are named BINARY, NOCASE,
    // RTRIM and our custom SYSTEMNOCASE, ISO8601, WIN32CASE, WIN32NOCASE: if
    // you want to use the slow but Unicode ready Windows API, set for each model:
    // ! SetCustomCollationForAll(oftUTF8Text, 'WIN32CASE');
    // - shall be set on both Client and Server sides, otherwise some issues
    // may occur
    procedure SetCustomCollationForAll(aFieldType: TOrmFieldType;
      const aCollationName: RawUTF8);
    /// allow to validate length of all text published properties of all tables
    // of this model
    // - the "index" attribute of the RawUTF8/string published properties could
    // be used to specify a maximum length for external VARCHAR() columns
    // - SQLite3 will just ignore this "index" information, but it could be
    // handy to be able to validate the value length before sending to the DB
    // - this method will create TSynValidateText corresponding to the maximum
    // field size specified by the "index" attribute, to validate before write
    // - will expect the "index" value to be in UTF-16 codepoints, unless
    // IndexIsUTF8Length is set to TRUE, indicating UTF-8 length
    procedure SetMaxLengthValidatorForAllTextFields(IndexIsUTF8Length: boolean = false);
    /// allow to filter the length of all text published properties of all tables
    // of this model
    // - the "index" attribute of the RawUTF8/string published properties could
    // be used to specify a maximum length for external VARCHAR() columns
    // - SQLite3 will just ignore this "index" information, but it could be
    // handy to be able to filter the value length before sending to the DB
    // - this method will create TSynFilterTruncate corresponding to the maximum
    // field size specified by the "index" attribute, to validate before write
    // - will expect the "index" value to be in UTF-16 codepoints, unless
    // IndexIsUTF8Length is set to TRUE, indicating UTF-8 length
    procedure SetMaxLengthFilterForAllTextFields(IndexIsUTF8Length: boolean = false);
    /// customize the TDocVariant options for all variant published properties
    // - will change the TOrmPropInfoRTTIVariant.DocVariantOptions value
    // - use e.g. as SetVariantFieldDocVariantOptions(JSON_OPTIONS_FAST_EXTENDED)
    // - see also TOrmNoCaseExtended root class
    procedure SetVariantFieldsDocVariantOptions(const Options: TDocVariantOptions);
    /// force a given table to use a TSynUniqueIdentifierGenerator for its IDs
    /// - will initialize a generator for the supplied table, using the
    // given 16-bit process identifier
    // - you can supply an obfuscation key, which should be shared for the
    // whole system, so that you may use FromObfuscated/ToObfuscated methods
    function SetIDGenerator(aTable: TOrmClass;
      aIdentifier: TSynUniqueIdentifierProcess;
      const aSharedObfuscationKey: RawUTF8 = ''): TSynUniqueIdentifierGenerator;
    /// returns the TSynUniqueIdentifierGenerator associated to a table, if any
    function GetIDGenerator(aTable: TOrmClass): TSynUniqueIdentifierGenerator;
    /// low-level access to the TSynUniqueIdentifierGenerator instances, if any
    property IDGenerator: TSynUniqueIdentifierGenerators read fIDGenerator;

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
      const aExternalTableName: RawUTF8 = ''; aExternalDataBase: TObject = nil;
      aMappingOptions: TOrmPropertiesMappingOptions = []): boolean;
    /// retrieve a Virtual Table module associated to a class
    // - returns a TOrmVirtualTableClass type definition
    function VirtualTableModule(aClass: TOrmClass): TClass;

    /// create a New TOrm instance for a specific Table
    // - expects SQLTableName to be SQL-like formated (i.e. without TOrm[Record])
    // - use this to create a working copy of a table's record, e.g.
    // - don't forget to Free it when not used any more (use a try...finally
    // block)
    // - it's prefered in practice to directly call TOrm*.Create()
    // in your code
    function NewRecord(const SQLTableName: RawUTF8): TOrm;

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
    property Tables: TOrmClassDynArray read fTables;
    /// get a class from a table name
    // - expects SQLTableName to be SQL-like formated (i.e. without TOrm[Record])
    property Table[const SQLTableName: RawUTF8]: TOrmClass
      read GetTable; default;
    /// get a class from a table TableName (don't truncate TOrm* if necessary)
    property TableExact[const TableName: RawUTF8]: TOrmClass
      read GetTableExactClass;
    /// the maximum index of TableProps[] class properties array
    property TablesMax: integer read fTablesMax;

    /// returns the Root property, or '' if the instance is nil
    function SafeRoot: RawUTF8;
    /// compute the URI for a class in this Model, as 'ModelRoot/SQLTableName'
    // - set also GetURI/GetURIID/GetURICallback methods
    property URI[aClass: TOrmClass]: RawUTF8 read GetURI;

    /// this property value is used to auto free the database Model class
    // - set this property after Owner.Create() in order to have
    // Owner.Destroy autofreeing this instance
    // - Owner is typically a TRest or a TRestORM class
    property Owner: TObject read fOwner write fOwner;
    /// for every table, contains a locked record list
    // - very fast, thanks to the use one TOrmLocks entry by table
    property Locks: TOrmLocksDynArray read fLocks;
    /// this array contain all TRecordReference and TOrm properties
    // existing in the database model
    // - used in TRestServer.Delete() to enforce relational database coherency
    // after deletion of a record: all other records pointing to it will be
    // reset to 0 or deleted (if CascadeDelete is true)
    property RecordReferences: TOrmModelReferenceDynArray read fRecordReferences;
    /// set a callback event to be executed in loop during client remote
    // blocking process, e.g. to refresh the UI during a somewhat long request
    // - will be passed to TRestClientURI.OnIdle property by
    // TRestClientURI.RegisteredClassCreateFrom() method, if applying
    property OnClientIdle: TOnIdleSynBackgroundThread
      read fOnClientIdle write fOnClientIdle;
  published
    /// the Root URI path of this Database Model
    // - this textual value will be used directly to compute the URI for REST
    // routing, so it should contain only URI-friendly characters,
    // i.e. only alphanumerical characters, excluding e.g. space or '+',
    // otherwise an EModelException is raised
    // - use SafeRoot function is you are not sure that the TOrmModel is not nil
    property Root: RawUTF8 read fRoot write SetRoot;
    /// the associated ORM information about all handled TOrm class properties
    // - this TableProps[] array will map the Tables[] array, and will allow
    // fast direct access to the Tables[].RecordProps values
    property TableProps: TOrmModelPropertiesObjArray read fTableProps;
  end;


  { -------------------- TRestCache Definition }

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
    JSON: RawUTF8;
  end;

  /// for TRestCache, stores all tables values
  TRestCacheEntryValueDynArray = array of TRestCacheEntryValue;

  /// for TRestCache, stores a table settings and values
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
    Mutex: TSynLocker;
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
    procedure SetJSON(aID: TID; const aJSON: RawUTF8;
      aTag: cardinal = 0); overload;
    /// update/refresh the cached JSON serialization of a supplied Record
    procedure SetJSON(aRecord: TOrm); overload;
    /// retrieve a JSON serialization of a given ID from cache
    function RetrieveJSON(aID: TID; var aJSON: RawUTF8;
      aTag: PCardinal = nil): boolean; overload;
    /// unserialize a JSON cached record of a given ID
    function RetrieveJSON(aID: TID; aValue: TOrm;
      aTag: PCardinal = nil): boolean; overload;
    /// compute how much memory stored entries are using
    // - will also flush outdated entries
    function CachedMemory(FlushedEntriesCount: PInteger = nil): cardinal;
  end;

  /// for TRestCache, stores all table settings and values
  // - this dynamic array will follow TRest.Model.Tables[] layout, i.e. one
  // entry per TOrm class in the data model
  TRestCacheEntryDynArray = array of TRestCacheEntry;

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
    // - the associated TOrmModel will be used internaly
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
      const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const): integer;
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
    property Rest: IRestOrm read fRest;
    /// read-only access to the associated TOrmModel instance
    property Model: TOrmModel read fModel;
  public { TRest low level methods which are not to be called usualy: }
    /// retrieve a record specified by its ID from cache into JSON content
    // - return '' if the item is not in cache
    function Retrieve(aTableIndex: integer; aID: TID): RawUTF8; overload;
    /// fill a record specified by its ID from cache into a new TOrm instance
    // - return false if the item is not in cache
    // - this method will call RetrieveJSON method, unserializing the cached
    // JSON content into the supplied aValue instance
    function Retrieve(aID: TID; aValue: TOrm): boolean; overload;
    /// TRest instance shall call this method when a record is added or updated
    // - this overloaded method expects the content to be specified as JSON object
    procedure Notify(aTable: TOrmClass; aID: TID; const aJSON: RawUTF8;
      aAction: TOrmOccasion); overload;
    /// TRest instance shall call this method when a record is retrieved,
    // added or updated
    // - this overloaded method expects the content to be specified as JSON object,
    // and TOrmClass to be specified as its index in Rest.Model.Tables[]
    procedure Notify(aTableIndex: integer; aID: TID; const aJSON: RawUTF8;
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

  /// event signature triggered by TRestBatch.OnWrite
  // - also used by TRestServer.RecordVersionSynchronizeSlave*() methods
  TOnBatchWrite = procedure(Sender: TRestBatch; Event: TOrmOccasion;
    Table: TOrmClass; const ID: TID; Value: TOrm;
    const ValueFields: TFieldBits) of object;

  /// used to store a BATCH sequence of writing operations
  // - is used by TRest to process BATCH requests using BatchSend() method,
  // or TRestClientURI for its Batch*() methods
  // - but you can create your own stand-alone BATCH process, so that it will
  // be able to make some transactional process - aka the "Unit Of Work" pattern
  TRestBatch = class
  protected
    fRest: IRestOrm;
    fModel: TOrmModel;
    fInternalBufferSize: integer;
    fCalledWithinRest: boolean;
    fBatch: TJSONSerializer;
    fBatchFields: TFieldBits;
    fTable: TOrmClass;
    fTablePreviousSendData: TOrmClass;
    fTableIndex: integer;
    fBatchCount: integer;
    fDeletedRecordRef: TIDDynArray;
    fDeletedCount: integer;
    fAddCount: integer;
    fUpdateCount: integer;
    fDeleteCount: integer;
    fAutomaticTransactionPerRow: cardinal;
    fOptions: TRestBatchOptions;
    fOnWrite: TOnBatchWrite;
    function GetCount: integer;
    function GetSizeBytes: cardinal;
    procedure SetExpandedJSONWriter(Props: TOrmProperties;
      ForceResetFields, withID: boolean; const WrittenFields: TFieldBits);
  public
    /// begin a BATCH sequence to speed up huge database changes
    // - each call to normal Add/Update/Delete methods will create a Server request,
    //   therefore can be slow (e.g. if the remote server has bad ping timing)
    // - start a BATCH sequence using this method, then call BatchAdd() BatchUpdate()
    //   or BatchDelete() methods to make some changes to the database
    // - when BatchSend will be called, all the sequence transactions will be sent
    //   as one to the remote server, i.e. in one URI request
    // - if BatchAbort is called instead, all pending BatchAdd/Update/Delete
    //   transactions will be aborted, i.e. ignored
    // - expect one TOrmClass as parameter, which will be used for the whole
    //   sequence (in this case, you can't mix classes in the same BATCH sequence)
    // - if no TOrmClass is supplied, the BATCH sequence will allow any
    //   kind of individual record in BatchAdd/BatchUpdate/BatchDelete
    // - return TRUE on success, FALSE if aTable is incorrect or a previous BATCH
    //   sequence was already initiated
    // - should normally be used inside a Transaction block: there is no automated
    //   TransactionBegin..Commit/RollBack generated in the BATCH sequence if
    //   you leave the default AutomaticTransactionPerRow=0 parameter - but
    //   this may be a concern with a lot of concurrent clients
    // - you should better set AutomaticTransactionPerRow > 0 to execute all
    //   BATCH processes within an unique transaction grouped by a given number
    //   of rows, on the server side - set AutomaticTransactionPerRow=maxInt if
    //   you want one huge transaction, or set a convenient value (e.g. 10000)
    //   depending on the back-end database engine abilities, if you want to
    //   retain the transaction log file small enough for the database engine
    // - BatchOptions could be set to tune the SQL execution, e.g. force INSERT
    //   OR IGNORE on internal SQLite3 engine
    // - InternalBufferSize could be set to some high value (e.g. 10 shl 20),
    //   if you expect a very high number of rows in this BATCH
    constructor Create(const aRest: IRestOrm; aTable: TOrmClass;
      AutomaticTransactionPerRow: cardinal = 0; Options: TRestBatchOptions = [];
      InternalBufferSize: cardinal = 65536; CalledWithinRest: boolean = false); virtual;
    /// finalize the BATCH instance
    destructor Destroy; override;
    /// reset the BATCH sequence so that you can re-use the same TRestBatch
    procedure Reset(aTable: TOrmClass; AutomaticTransactionPerRow: cardinal = 0;
      Options: TRestBatchOptions = []); overload; virtual;
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
    // CustomFields could be computed by TOrmProperties.FieldBitsFromCSV()
    // or TOrmProperties.FieldBitsFromRawUTF8(), or by setting ALL_FIELDS
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
    // could be computed by TOrmProperties.FieldBitsFromCSV()
    // or TOrmProperties.FieldBitsFromRawUTF8()
    // - this method will always compute and send any TModTime fields, unless
    // DoNotAutoComputeFields is set to true
    // - if not all fields are specified, will reset the cache entry associated
    // with this value, unless ForceCacheUpdate is TRUE
    function Update(Value: TOrm; const CustomFields: TFieldBits = [];
      DoNotAutoComputeFields: boolean = false; ForceCacheUpdate: boolean = false): integer; overload; virtual;
    /// update a member in current BATCH sequence
    // - work in BATCH mode: nothing is sent to the server until BatchSend call
    // - is an overloaded method to Update(Value,FieldBitsFromCSV())
    function Update(Value: TOrm; const CustomCSVFields: RawUTF8;
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
    function RawAppend(FullRow: boolean = true): TTextWriter;
    /// allow to append some JSON content to the internal raw buffer for a POST
    // - could be used to emulate Add() with an already pre-computed JSON object
    // - returns the corresponding index in the current BATCH sequence, -1 on error
    function RawAdd(const SentData: RawUTF8): integer;
    /// allow to append some JSON content to the internal raw buffer for a PUT
    // - could be used to emulate Update() with an already pre-computed JSON object
    // - returns the corresponding index in the current BATCH sequence, -1 on error
    function RawUpdate(const SentData: RawUTF8; ID: TID): integer;
    /// close a BATCH sequence started by Start method
    // - Data is the JSON content, ready to be supplied to TRest.BatchSend()
    // overloaded method - its layout is '{"Table":["cmd":values,...]}'
    // - will also notify the TRest.Cache for all deleted IDs
    // - you should not have to call it in normal use cases
    function PrepareForSending(out Data: RawUTF8): boolean; virtual;
    /// read only access to the associated TRest instance
    property Rest: IRestOrm read fRest;
    /// read only access to the associated TOrmModel instance
    property Model: TOrmModel read fModel;
    /// how many times Add() has been called for this BATCH process
    property AddCount: integer read fAddCount;
    /// how many times Update() has been called for this BATCH process
    property UpdateCount: integer read fUpdateCount;
    /// how many times Delete() has been called for this BATCH process
    property DeleteCount: integer read fDeleteCount;
    /// this event handler will be triggerred by each Add/Update/Delete method
    property OnWrite: TOnBatchWrite read fOnWrite write fOnWrite;
  published
    /// read only access to the main associated TOrm class (if any)
    property Table: TOrmClass read fTable;
    /// retrieve the current number of pending transactions in the BATCH sequence
    property Count: integer read GetCount;
    /// retrieve the current JSON size of pending transaction in the BATCH sequence
    property SizeBytes: cardinal read GetSizeBytes;
  end;

  /// thread-safe class to store a BATCH sequence of writing operations
  TRestBatchLocked = class(TRestBatch)
  protected
    fResetTix: Int64;
    fSafe: TSynLocker;
    fThreshold: integer;
  public
    /// initialize the BATCH instance
    constructor Create(const aRest: IRestOrm; aTable: TOrmClass;
      AutomaticTransactionPerRow: cardinal = 0; Options: TRestBatchOptions = [];
      InternalBufferSize: cardinal = 65536; CalledWithinRest: boolean = false); override;
    /// finalize the BATCH instance
    destructor Destroy; override;
    /// reset the BATCH sequence so that you can re-use the same TRestBatch
    procedure Reset(aTable: TOrmClass;
      AutomaticTransactionPerRow: cardinal = 0; Options: TRestBatchOptions = []); override;
    /// access to the locking methods of this instance
    // - use Safe.Lock/TryLock with a try ... finally Safe.Unlock block
    property Safe: TSynLocker read fSafe;
    /// property set to the current GetTickCount64 value when Reset is called
    property ResetTix: Int64 read fResetTix write fResetTix;
    /// may be used to store a number of rows to flush the content
    property Threshold: integer read fThreshold write fThreshold;
  end;

  TRestBatchLockedDynArray = array of TRestBatchLocked;


  { -------------------- TSynValidateRest TSynValidateUniqueField Definitions }

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
    function DoValidate(aFieldIndex: integer; const Value: RawUTF8;
      var ErrorMsg: string; const aProcessRest: IRestOrm; aProcessRec: TOrm): boolean;
      virtual; abstract;
  public
    function Process(aFieldIndex: integer; const Value: RawUTF8;
      var ErrorMsg: string): boolean; override;
    function Validate(aFieldIndex: integer; const Value: RawUTF8;
      var ErrorMsg: string; const aProcessRest: IRestOrm;
      aProcessRec: TOrm): boolean;
    /// access to the ORM process of the associated TRest instance
    // - this value is updated by Validate with the current
    // TRest used for the validation
    // - it can be used in the overridden DoValidate method
    property ProcessRest: IRestOrm read fProcessRest;
    /// the associated TOrm instance
    // - this value is updated by Validate with the current
    // TOrm instance to be validated
    // - it can be used in the overridden DoValidate method
    property ProcessRec: TOrm read fProcessRec;
  end;

  /// will define a validation for a TOrm Unique text field
  // - this class will handle only textual fields, not numeric values
  // - it will check that the field value is not void
  // - it will check that the field value is not a duplicate
  TSynValidateUniqueField = class(TSynValidateRest)
  protected
    /// perform the unique field validation action to the specified value
    function DoValidate(aFieldIndex: integer; const Value: RawUTF8;
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
    fFieldNames: TRawUTF8DynArray;
    procedure SetParameters(const Value: RawUTF8); override;
    /// perform the unique fields validation action to the specified value
    function DoValidate(aFieldIndex: integer; const Value: RawUTF8;
      var ErrorMsg: string; const aProcessRest: IRestOrm;
      aProcessRec: TOrm): boolean; override;
  public
    /// the validated field names
    property FieldNames: TRawUTF8DynArray read fFieldNames;
  end;


  { -------------------- TOrmAccessRights Definition }

  /// a set of potential actions to be executed from the server
  // - reSQL will indicate the right to execute any POST SQL statement (not only
  // SELECT statements)
  // - reSQLSelectWithoutTable will allow executing a SELECT statement with
  // arbitrary content via GET/LOCK (simple SELECT .. FROM aTable will be checked
  // against TOrmAccessRights.GET[] per-table right
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
  // - by default, read/write access to the TAuthUser table is disallowed,
  // for obvious security reasons: but you can define reUserCanChangeOwnPassword
  // so that the current logged user will be able to change its own password
  // - order of this set does matter, since it will be stored as a byte value
  // e.g. by TOrmAccessRights.ToString: ensure that new items will always be
  // appended to the list, not inserted within
  TOrmAllowRemoteExecute = set of (
    reSQL, reService,
    reUrlEncodedSQL,
    reUrlEncodedDelete,
    reOneSessionPerUser,
    reSQLSelectWithoutTable,
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
    // TRestClientURI.UpdateFromServer) will be checked for simple cases
    // (i.e. the first table in the FROM clause), otherwise will follow , whatever the bits
    // here are: since TRestClientURI.UpdateFromServer() is called only
    // for refreshing a direct statement, it will be OK; you can improve this
    // by overriding the TRestServer.URI() method
    // - if the REST request is LOCK, the PUT access bits will be read instead
    // of the GET bits value
    GET: TOrmFieldTables;
    /// POST method (create record) table access bits
    POST: TOrmFieldTables;
    /// PUT method (update record) table access bits
    // - if the REST request is LOCK, the PUT access bits will be read instead
    // of the GET bits value
    PUT: TOrmFieldTables;
    /// DELETE method (delete record) table access bits
    DELETE: TOrmFieldTables;
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
    function ToString: RawUTF8;
    /// unserialize the content from TEXT
    // - use the TAuthGroup.AccessRights CSV format
    procedure FromString(P: PUTF8Char);
  end;

  POrmAccessRights = ^TOrmAccessRights;


const
  /// used as "stored AS_UNIQUE" published property definition in TOrm
  AS_UNIQUE = false;

  /// options to specify no index createon for TRestServer.CreateMissingTables
  // and TOrm.InitializeTable methods
  INITIALIZETABLE_NOINDEX: TOrmInitializeTableOptions =
    [itoNoIndex4ID.. itoNoIndex4RecordVersion];

  /// Supervisor Table access right, i.e. alllmighty over all fields
  ALL_ACCESS_RIGHTS = [0..MAX_TABLES - 1];

  /// Complete Database access right, i.e. allmighty over all Tables
  // - WITH the possibility to remotely execute any SQL statement (reSQL right)
  // - is used by default by TRestClientDB.URI() method, i.e. for direct
  // local/in-process access
  // - is used as reference to create TAuthUser 'Admin' access policy
  FULL_ACCESS_RIGHTS: TOrmAccessRights = (
    AllowRemoteExecute:
     [reSQL, reSQLSelectWithoutTable, reService, reUrlEncodedSQL, reUrlEncodedDelete];
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
      [reSQLSelectWithoutTable, reService, reUrlEncodedSQL, reUrlEncodedDelete];
    GET: ALL_ACCESS_RIGHTS;
    POST: ALL_ACCESS_RIGHTS;
    PUT: ALL_ACCESS_RIGHTS;
    DELETE: ALL_ACCESS_RIGHTS
  );

  /// if the TOrmVirtual table kind is a FTS virtual table
  IS_FTS =
    [rFTS3, rFTS4, rFTS5];

  /// if the TOrmVirtual table kind is not an embedded type
  // - can be set for a TOrm after a VirtualTableExternalRegister call
  IS_CUSTOM_VIRTUAL =
    [rCustomForcedID, rCustomAutoID];

  /// if the TOrmVirtual table kind expects the ID to be set on INSERT
  INSERT_WITH_ID =
    [rFTS3, rFTS4, rFTS5, rRTree, rRTreeInteger, rCustomForcedID];

  /// if a TOrmVirtualTablePreparedConstraint.Column is to be ignored
  VIRTUAL_TABLE_IGNORE_COLUMN = -2;

  /// if a TOrmVirtualTablePreparedConstraint.Column points to the RowID
  VIRTUAL_TABLE_ROWID_COLUMN = -1;

var
  /// late-binding return of TOrmVirtualTableClass.ModuleName
  // - link mormot.orm.storage.pas unit for properly set this value
  GetVirtualTableModuleName: function(VirtualTableClass: TClass): RawUTF8;

function ToText(vk: TOrmVirtualKind): PShortString; overload;

/// compute the SQL field names, used to create a SQLite3 virtual table
function GetVirtualTableSQLCreate(Props: TOrmProperties): RawUTF8;

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


{$ifndef PUREMORMOT2}
// backward compatibility types redirections

type
  TSQLRecord = TOrm;
  PSQLRecord = POrm;
  TSQLRecordArray = TOrmArray;
  PSQLRecordArray = POrmArray;
  TSQLRecordObjArray = TOrmObjArray;
  TSQLRecordClass = TOrmClass;
  TSQLRecordClassDynArray = TOrmClassDynArray;
  PSQLClass = POrmClass;
  TSQLTable = TOrmTable;
  TSQLTableJSON = TOrmTableJSON;
  TSQLInitializeTableOption = TOrmInitializeTableOption;
  TSQLInitializeTableOptions = TOrmInitializeTableOptions;
  TSQLAccessRights = TOrmAccessRights;
  PSQLAccessRights = POrmAccessRights;
  TSQLFieldTables = TOrmFieldTables;
  TSQLModel = TOrmModel;
  TSQLModelProperties = TOrmModelProperties;
  TSQLModelPropertiesObjArray = TOrmModelPropertiesObjArray;
  TSQLProperties = TOrmProperties;
  TSQLPropInfo = TOrmPropInfo;
  TSQLPropInfoObjArray = TOrmPropInfoObjArray;
  TSQLPropInfoClass = TOrmPropInfoClass;
  TSQLPropInfoListOptions = TOrmPropInfoListOptions;
  TSQLPropInfoAttribute = TOrmPropInfoAttribute;
  TSQLPropInfoAttributes = TOrmPropInfoAttributes;
  TSQLRestCache = TRestCache;
  TSQLRestBatch = TRestBatch;
  TSQLRestBatchLocked = TRestBatchLocked;
  TSQLEvent = TOrmEvent;
  TSQLHistoryEvent = TOrmHistoryEvent;
  TSQLOccasion = TOrmOccasion;
  TSQLOccasions = TOrmOccasions;

const
  // TOrmFieldType into TSQLFieldType
  sftUnknown       = oftUnknown;
  sftAnsiText      = oftAnsiText;
  sftUTF8Text      = oftUTF8Text;
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
  sftUTF8Custom    = oftUTF8Custom;
  sftMany          = oftMany;
  sftModTime       = oftModTime;
  sftCreateTime    = oftCreateTime;
  sftTID           = oftTID;
  sftRecordVersion = oftRecordVersion;
  sftSessionUserID = oftSessionUserID;
  sftDateTimeMS    = oftDateTimeMS;
  sftUnixTime      = oftUnixTime;
  sftUnixMSTime    = oftUnixMSTime;
  // TOrmEvent/TOrmOccasion into TSQLEvent/TSQLOccasion
  seAdd        =  oeAdd;
  seUpdate     =  oeUpdate;
  seDelete     =  oeDelete;
  seUpdateBlob =  oeUpdateBlob;
  soSelect     =  ooSelect;
  soInsert     =  ooInsert;
  soUpdate     =  ooUpdate;
  soDelete     =  ooDelete;

{$endif PUREMORMOT2}



{ ************** TOrm High-Level Parents }

type
  /// root class for defining and mapping database records with case-insensitive
  // NOCASE collation
  // - abstract ancestor, from which you may inherit your own ORM classes
  // - by default, any oftUTF8Text field (RawUTF8, UnicodeString, WideString
  // properties) will use our Unicode SYSTEMNOCASE SQLite3 collation, which calls
  // UTF8ILComp() to handle most western languages, but is not standard
  // - you may inherit from this class to ensure any text field will use the
  // faster and SQLite3 built-in NOCASE collation, handling only 7-bit A-Z chars
  // - inherit from TOrmNoCase or TOrmCaseSensitive if you expect
  // your text fields to contain only basic (un)accentued ASCCI characters, and
  // to be opened by any standard/ SQlite3 library or tool (outside of
  // mormot.db.raw.sqlite3.pas/SynDBExplorer)
  TOrmNoCase = class(TOrm)
  protected
    /// will call Props.SetCustomCollationForAll(oftUTF8Text,'NOCASE')
    class procedure InternalDefineModel(Props: TOrmProperties); override;
  end;

  /// root class for defining and mapping database records with case-sensitive
  // BINARY collation
  // - abstract ancestor, from which you may inherit your own ORM classes
  // - by default, any oftUTF8Text field (RawUTF8, UnicodeString, WideString
  // properties) will use our Unicode SYSTEMNOCASE SQLite3 collation, which calls
  // UTF8ILComp() to handle most western languages, but is not standard
  // - you may inherit from this class to ensure any text field will use the
  // faster and SQLite3 built-in BINARY collation, which is case-sensitive
  // - inherit from TOrmNoCase or TOrmCaseSensitive if you expect
  // your text fields to contain only basic (un)accentued ASCCI characters, and
  // to be opened by any standard/ SQlite3 library or tool (outside of
  // mormot.db.raw.sqlite3.pas/SynDBExplorer)
  TOrmCaseSensitive = class(TOrm)
  protected
    /// will call Props.SetCustomCollationForAll(oftUTF8Text,'BINARY')
    class procedure InternalDefineModel(Props: TOrmProperties); override;
  end;

  /// database records with NOCASE collation and JSON_OPTIONS_FAST_EXTENDED variants
  // - abstract ancestor, from which you may inherit your own ORM classes
  TOrmNoCaseExtended = class(TOrmNoCase)
  protected
    /// will call Props.SetVariantFieldsDocVariantOptions(JSON_OPTIONS_FAST_EXTENDED);
    class procedure InternalDefineModel(Props: TOrmProperties); override;
  end;

  /// database records with BINARY collation and JSON_OPTIONS_FAST_EXTENDED variants
  // - abstract ancestor, from which you may inherit your own ORM classes
  TOrmCaseSensitiveExtended = class(TOrmCaseSensitive)
  protected
    /// will call Props.SetVariantFieldsDocVariantOptions(JSON_OPTIONS_FAST_EXTENDED);
    class procedure InternalDefineModel(Props: TOrmProperties); override;
  end;

  /// database records with NOCASE collation and JSON_OPTIONS_FAST_EXTENDED
  // variants, and itoNoIndex4TID option to avoid indexes on TID/T*ID properties
  // - abstract ancestor, from which you may inherit your own ORM classes
  TOrmNoCaseExtendedNoID = class(TOrmNoCaseExtended)
  public
    /// overriden method forcing no index creation on TID/T*ID properties
    class procedure InitializeTable(const Server: IRestOrmServer;
     const FieldName: RawUTF8; Options: TOrmInitializeTableOptions); override;
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
        result := oftUTF8Text; // CP_UTF8,CP_UTF16 and any other to UTF-8 text
        exit;
      end;
    {$ifdef HASVARUSTRING} rkUString, {$endif} rkChar, rkWChar, rkWString:
      begin
        result := oftUTF8Text;
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
        result := oftUTF8Custom;
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

procedure AddID(var Values: TIDDynArray; var ValuesCount: integer; Value: TID);
begin
  if ValuesCount = length(Values) then
    SetLength(Values, NextGrow(ValuesCount));
  Values[ValuesCount] := Value;
  inc(ValuesCount);
end;

procedure AddID(var Values: TIDDynArray; Value: TID);
var
  n: PtrInt;
begin
  n := length(Values);
  SetLength(Values, n + 1);
  Values[n] := Value;
end;

procedure SetID(P: PUTF8Char; var result: TID);
begin
{$ifdef CPU64} // PtrInt is already int64 -> call PtrInt version
  result := GetInteger(P);
{$else}
  {$ifdef VER3_0} // FPC issue workaround
  SetInt64(P, result);
  {$else}
  SetInt64(P, PInt64(@result)^);
  {$endif}
{$endif CPU64}
end;

procedure SetID(const U: RawByteString; var result: TID);
begin
{$ifdef CPU64} // PtrInt is already int64 -> call PtrInt version
  result := GetInteger(pointer(U));
{$else}
  SetID(pointer(U), result);
{$endif CPU64}
end;

function BlobToRawBlob(P: PUTF8Char): RawBlob;
begin
  BlobToRawBlob(P, result);
end;

procedure BlobToRawBlob(P: PUTF8Char; var result: RawBlob);
var
  Len, LenHex: integer;
begin
  result := '';
  Len := StrLen(P);
  if Len = 0 then
    exit;
  if Len >= 3 then
    if (P[0] in ['x', 'X']) and
       (P[1] = '''') and
       (P[Len - 1] = '''') then
    begin
      // BLOB literals are string literals containing hexadecimal data and
      // preceded by a single "x" or "X" character. For example: X'53514C697465'
      LenHex := (Len - 3) shr 1;
      SetLength(result, LenHex);
      if mormot.core.text.HexToBin(@P[2], pointer(result), LenHex) then
        exit; // valid hexa data
    end
    else if (PInteger(P)^ and $00ffffff = JSON_BASE64_MAGIC) and
       Base64ToBinSafe(@P[3], Len - 3, RawByteString(result)) then
      exit; // safe decode Base-64 content ('\uFFF0base64encodedbinary')
  // TEXT format
  SetString(result, PAnsiChar(P), Len);
end;

function BlobToRawBlob(const Blob: RawByteString): RawBlob;
var
  Len, LenHex: integer;
  P: PUTF8Char;
begin
  result := '';
  if Blob = '' then
    exit;
  Len := length(Blob);
  P := pointer(Blob);
  if Len >= 3 then
    if (P[0] in ['x', 'X']) and
       (P[1] = '''') and
       (P[Len - 1] = '''') then
    begin
      // BLOB literals are string literals containing hexadecimal data and
      // preceded by a single "x" or "X" character. For example: X'53514C697465'
      LenHex := (Len - 3) shr 1;
      SetLength(result, LenHex);
      if mormot.core.text.HexToBin(@P[2], pointer(result), LenHex) then
        exit; // valid hexa data
    end
    else if (PInteger(P)^ and $00ffffff = JSON_BASE64_MAGIC) and
        Base64ToBinSafe(@P[3], Len - 3, RawByteString(result)) then
      exit; // safe decode Base-64 content ('\uFFF0base64encodedbinary')
  // TEXT format
  result := Blob;
end;

function BlobToStream(P: PUTF8Char): TStream;
begin
  result := TRawByteStringStream.Create(BlobToRawBlob(P));
end;

function BlobToBytes(P: PUTF8Char): TBytes;
var
  Len, LenResult: integer;
begin
  result := nil;
  Len := StrLen(P);
  if Len = 0 then
    exit;
  if Len >= 3 then
    if (P[0] in ['x', 'X']) and
       (P[1] = '''') and
       (P[Len - 1] = '''') then
    begin
      // BLOB literals format
      LenResult := (Len - 3) shr 1;
      SetLength(result, LenResult);
      if mormot.core.text.HexToBin(@P[2], pointer(result), LenResult) then
        exit; // valid hexa data
    end
    else if (PInteger(P)^ and $00ffffff = JSON_BASE64_MAGIC) and
            IsBase64(@P[3], Len - 3) then
    begin
      // Base-64 encoded content ('\uFFF0base64encodedbinary')
      inc(P, 3);
      dec(Len, 3);
      LenResult := Base64ToBinLength(pointer(P), Len);
      SetLength(result, LenResult);
      if LenResult > 0 then
        Base64Decode(pointer(P), pointer(result), Len shr 2);
      exit;
    end;
  // TEXT format
  SetLength(result, Len);
  MoveFast(P^, pointer(result)^, Len);
end;

function RawBlobToBlob(const RawBlob: RawBlob): RawUTF8;
// BLOB literals are string literals containing hexadecimal data and
//  preceded by a single "x" or "X" character. For example: X'53514C697465'
begin
  result := RawBlobToBlob(pointer(RawBlob), length(RawBlob));
end;

function RawBlobToBlob(RawBlob: pointer; RawBlobLength: integer): RawUTF8;
// BLOB literals are string literals containing hexadecimal data and
//  preceded by a single "x" or "X" character. For example: X'53514C697465'
var
  P: PAnsiChar;
begin
  result := '';
  if RawBlobLength <> 0 then
  begin
    SetLength(result, RawBlobLength * 2 + 3);
    P := pointer(result);
    P[0] := 'X';
    P[1] := '''';
    BinToHex(RawBlob, P + 2, RawBlobLength);
    P[RawBlobLength * 2 + 2] := '''';
  end;
end;

function isBlobHex(P: PUTF8Char): boolean;
// BLOB literals are string literals containing hexadecimal data and
// preceded by a single "x" or "X" character. For example: X'53514C697465'
var
  Len: integer;
begin
  if P = nil then
  begin
    result := false;
    exit;
  end;
  while (P^ <= ' ') and
        (P^ <> #0) do
    inc(P);
  if (P[0] in ['x', 'X']) and
     (P[1] = '''') then
  begin
    Len := (StrLen(P) - 3) shr 1;
    result := (P[Len - 1] = '''') and
              mormot.core.text.HexToBin(@P[2], nil, Len);
    exit;
  end
  else
  begin
    result := false;
    exit;
  end;
end;

procedure Base64MagicToBlob(Base64: PUTF8Char; var result: RawUTF8);
begin
  // do not escape the result: returns e.g. X'53514C697465'
  result := RawBlobToBlob(Base64ToBin(PAnsiChar(Base64), StrLen(Base64)));
end;

function UTF8ContentNumberType(P: PUTF8Char): TOrmFieldType;
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
      result := oftUTF8Text;
    end;
end;

function UTF8ContentType(P: PUTF8Char): TOrmFieldType;
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
          if (len > 15) and (Iso8601ToTimeLogPUTF8Char(P, len) <> 0) then
            result := oftDateTime
          else
            result := oftUTF8Text;
        end;
      end
    else
    begin
      c := PInteger(P)^ and $00ffffff;
      if (c = JSON_BASE64_MAGIC) or
         ((P^ = '''') and
          isBlobHex(P)) then
        result := oftBlob
      else if c = JSON_SQLDATE_MAGIC then
        result := oftDateTime
      else
        result := oftUTF8Text;
    end;
  end
  else
    result := oftUnknown;
end;

function UTF8CompareCurr64(P1, P2: PUTF8Char): PtrInt;
var
  V1, V2: Int64;
begin // faster than UTF8CompareDouble() for pure decimal (no exponent) values
  V1 := StrToCurr64(P1);
  V2 := StrToCurr64(P2);
  if V1 < V2 then
    result := -1
  else if V1 = V2 then
    result := 0
  else
    result := +1;
end;

function UTF8CompareBoolean(P1, P2: PUTF8Char): PtrInt;
label
  Z, P, n;
begin // assume 0 is FALSE, anything else is true
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
  else if (P2^ <> #0) and (PWord(P2)^ <> ord('0')) then
    goto Z        // P1=true P2=true
  else
  begin
P:  result := 1;  // P1=true P2=false
    exit;
  end;
end;

function UTF8CompareInt32(P1, P2: PUTF8Char): PtrInt;
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

function UTF8CompareUInt32(P1, P2: PUTF8Char): PtrInt;
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

function UTF8CompareRecord(P1, P2: PUTF8Char): PtrInt;
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

function UTF8CompareInt64(P1, P2: PUTF8Char): PtrInt;
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

function UTF8CompareDouble(P1, P2: PUTF8Char): PtrInt;
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
er: result := UTF8IComp(P1, P2);
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

function UTF8CompareISO8601(P1, P2: PUTF8Char): PtrInt;
var
  V1, V2: TDateTime;
begin
  if P1 = P2 then
  begin
    result := 0;
    exit;
  end;
  Iso8601ToDateTimePUTF8CharVar(P1, 0, V1);
  Iso8601ToDateTimePUTF8CharVar(P2, 0, V2);
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


{ ************ JSON Object Decoder and SQL Generation }

{ TJSONObjectDecoder }

const
  ENDOFJSONFIELD = [',', ']', '}', ':'];

procedure GetJSONArrayOrObject(P: PUTF8Char; out PDest: PUTF8Char;
  EndOfObject: PUTF8Char; var result: RawUTF8);
var
  Beg: PUTF8Char;
begin
  PDest := nil;
  Beg := P;
  P := GotoNextJSONObjectOrArray(P); // quick go to end of array of object
  if P = nil then
  begin
    result := '';
    exit;
  end;
  if EndOfObject <> nil then
    EndOfObject^ := P^;
  PDest := P + 1;
  FastSetString(result, Beg, P - Beg);
end;

procedure GetJSONArrayOrObjectAsQuotedStr(P: PUTF8Char; out PDest: PUTF8Char;
  EndOfObject: PUTF8Char; var result: RawUTF8);
var
  Beg: PUTF8Char;
begin
  result := '';
  PDest := nil;
  Beg := P;
  P := GotoNextJSONObjectOrArray(P); // quick go to end of array of object
  if P = nil then
    exit;
  if EndOfObject <> nil then
    EndOfObject^ := P^;
  P^ := #0; // so Beg will be a valid ASCIIZ string
  PDest := P + 1;
  QuotedStr(Beg, '''', result);
end;

procedure TJSONObjectDecoder.Decode(var P: PUTF8Char;
  const Fields: TRawUTF8DynArray; Params: TJSONObjectDecoderParams;
  const RowID: TID; ReplaceRowIDWithID: boolean);
var
  EndOfObject: AnsiChar;

  procedure GetSQLValue(ndx: PtrInt);
  var
    wasString: boolean;
    res: PUTF8Char;
    resLen, c: integer;
  begin
    res := P;
    if res = nil then
    begin
      FieldTypeApproximation[ndx] := ftaNull;
      FieldValues[ndx] := NULL_STR_VAR;
      exit;
    end;
    while res^ in [#1..' '] do
      inc(res);
    if (PInteger(res)^ = NULL_LOW) and
       (res[4] in [#0, #9, #10, #13, ' ', ',', '}', ']']) then
    begin
      /// GetJSONField('null') returns '' -> check here to make a diff with '""'
      FieldTypeApproximation[ndx] := ftaNull;
      FieldValues[ndx] := NULL_STR_VAR;
      inc(res, 4);
      while res^ in [#1..' '] do
        inc(res);
      if res^ = #0 then
        P := nil
      else
      begin
        EndOfObject := res^;
        res^ := #0;
        P := res + 1;
      end;
    end
    else
    begin
      // first check if nested object or array
      case res^ of // handle JSON {object} or [array] in P
        '{':
          begin // will work e.g. for custom variant types
            FieldTypeApproximation[ndx] := ftaObject;
            if Params = pNonQuoted then
              GetJSONArrayOrObject(res, P, @EndOfObject, FieldValues[ndx])
            else
              GetJSONArrayOrObjectAsQuotedStr(res, P, @EndOfObject, FieldValues[ndx]);
          end;
        '[':
          begin // will work e.g. for custom variant types
            FieldTypeApproximation[ndx] := ftaArray;
            if Params = pNonQuoted then
              GetJSONArrayOrObject(res, P, @EndOfObject, FieldValues[ndx])
            else
              GetJSONArrayOrObjectAsQuotedStr(res, P, @EndOfObject, FieldValues[ndx]);
          end;
      else
        begin
          // handle JSON string, number or false/true in P
          res := GetJSONField(res, P, @wasString, @EndOfObject, @resLen);
          if wasString then
          begin
            c := PInteger(res)^ and $00ffffff;
            if c = JSON_BASE64_MAGIC then
            begin
              FieldTypeApproximation[ndx] := ftaBlob;
              case Params of
                pInlined: // untouched -> recognized as BLOB in SQLParamContent()
                  QuotedStr(res, '''', FieldValues[ndx]);
              { pQuoted: // \uFFF0base64encodedbinary -> 'X''hexaencodedbinary'''
                // if not inlined, it can be used directly in INSERT/UPDATE statements
                Base64MagicToBlob(res+3,FieldValues[ndx]);
                pNonQuoted: }
              else // returned directly as RawByteString
                Base64ToBin(PAnsiChar(res) + 3, resLen - 3,
                  RawByteString(FieldValues[ndx]));
              end;
            end
            else
            begin
              if c = JSON_SQLDATE_MAGIC then
              begin
                FieldTypeApproximation[ndx] := ftaDate;
                inc(res, 3); // ignore \uFFF1 magic marker
              end
              else
                FieldTypeApproximation[ndx] := ftaString;
              // regular string content
              if Params = pNonQuoted then
                // returned directly as RawUTF8
                FastSetString(FieldValues[ndx], res, resLen)
              else
                 { escape SQL strings, cf. the official SQLite3 documentation:
                "A string is formed by enclosing the string in single quotes (').
                 A single quote within the string can be encoded by putting two
                 single quotes in a row - as in Pascal." }
                QuotedStr(res, '''', FieldValues[ndx]);
            end;
          end
          else if res = nil then
          begin
            FieldTypeApproximation[ndx] := ftaNull;
            FieldValues[ndx] := NULL_STR_VAR;
          end
          else // avoid GPF, but will return invalid SQL
          // non string params (numeric or false/true) are passed untouched
          if PInteger(res)^ = FALSE_LOW then
          begin
            FieldValues[ndx] := SmallUInt32UTF8[0];
            FieldTypeApproximation[ndx] := ftaBoolean;
          end
          else if PInteger(res)^ = TRUE_LOW then
          begin
            FieldValues[ndx] := SmallUInt32UTF8[1];
            FieldTypeApproximation[ndx] := ftaBoolean;
          end
          else
          begin
            FastSetString(FieldValues[ndx], res, resLen);
            FieldTypeApproximation[ndx] := ftaNumber;
          end;
        end;
      end;
    end;
  end;

var
  FN: PUTF8Char;
  F, FNlen: integer;
  FieldIsRowID: boolean;
begin
  FieldCount := 0;
  DecodedRowID := 0;
  DecodedFieldTypesToUnnest := nil;
  FillCharFast(FieldTypeApproximation, SizeOf(FieldTypeApproximation),
    ord(ftaNumber{TID}));
  InlinedParams := Params;
  if pointer(Fields) = nil then
  begin
    // get "COL1"="VAL1" pairs, stopping at '}' or ']'
    DecodedFieldNames := @FieldNames;
    if RowID > 0 then
    begin // insert explicit RowID
      if ReplaceRowIDWithID then
        FieldNames[0] := 'ID'
      else
        FieldNames[0] := 'RowID';
      Int64ToUtf8(RowID, FieldValues[0]);
      FieldCount := 1;
      DecodedRowID := RowID;
    end;
    repeat
      if P = nil then
        break;
      FN := GetJSONPropName(P, @FNlen);
      if (FN = nil) or
         (P = nil) then
        break; // invalid JSON field name
      FieldIsRowID := IsRowId(FN);
      if FieldIsRowID then
        if RowID > 0 then
        begin
          GetJSONField(P, P, nil, @EndOfObject); // ignore this if explicit RowID
          if EndOfObject in [#0, '}', ']'] then
            break
          else
            continue;
        end
        else if ReplaceRowIDWithID then
        begin
          FN := 'ID';
          FNlen := 2;
        end;
      FastSetString(FieldNames[FieldCount], FN, FNlen);
      GetSQLValue(FieldCount); // update EndOfObject
      if FieldIsRowID then
        SetID(FieldValues[FieldCount], DecodedRowID);
      inc(FieldCount);
      if FieldCount = MAX_SQLFIELDS then
        raise EJSONObjectDecoder.Create('Too many inlines in TJSONObjectDecoder');
    until {%H-}EndOfObject in [#0, '}', ']'];
  end
  else
  begin
    // get "VAL1","VAL2"...
    if P = nil then
      exit;
    if RowID > 0 then
      raise EJSONObjectDecoder.Create('TJSONObjectDecoder(expanded) won''t handle RowID');
    if length(Fields) > MAX_SQLFIELDS then
      raise EJSONObjectDecoder.Create('Too many inlines in TJSONObjectDecoder');
    DecodedFieldNames := pointer(Fields);
    FieldCount := length(Fields);
    for F := 0 to FieldCount - 1 do
      GetSQLValue(F); // update EndOfObject
  end;
end;

procedure TJSONObjectDecoder.Decode(const JSON: RawUTF8;
  const Fields: TRawUTF8DynArray; Params: TJSONObjectDecoderParams;
  const RowID: TID; ReplaceRowIDWithID: boolean);
var
  tmp: TSynTempBuffer;
  P: PUTF8Char;
begin
  tmp.Init(JSON);
  try
    P := tmp.buf;
    if P <> nil then
      while P^ in [#1..' ', '{', '['] do
        inc(P);
    Decode(P, Fields, Params, RowID, ReplaceRowIDWithID);
  finally
    tmp.Done;
  end;
end;

function TJSONObjectDecoder.SameFieldNames(const Fields: TRawUTF8DynArray): boolean;
var
  i: integer;
begin
  result := false;
  if length(Fields) <> FieldCount then
    exit;
  for i := 0 to FieldCount - 1 do
    if not IdemPropNameU(Fields[i], FieldNames[i]) then
      exit;
  result := true;
end;

procedure TJSONObjectDecoder.AssignFieldNamesTo(var Fields: TRawUTF8DynArray);
var
  i: integer;
begin
  SetLength(Fields, FieldCount);
  for i := 0 to FieldCount - 1 do
    Fields[i] := FieldNames[i];
end;

{$ifdef ISDELPHI20062007}
  {$WARNINGS OFF} // circument Delphi 2007 false positive warning
{$endif}

const
  PG_FT: array[TSQLDBFieldType] of string[9] = (
    'int4', 'text', 'int8', 'float8', 'numeric', 'timestamp', 'text', 'bytea');

function TJSONObjectDecoder.EncodeAsSQLPrepared(const TableName: RawUTF8;
  Occasion: TOrmOccasion; const UpdateIDFieldName: RawUTF8;
  BatchOptions: TRestBatchOptions): RawUTF8;
var
  F: integer;
  W: TTextWriter;
  temp: TTextWriterStackBuffer;
begin
  W := TTextWriter.CreateOwnedStream(temp);
  try
    case Occasion of
      ooUpdate:
        begin
          if FieldCount = 0 then
            raise EJSONObjectDecoder.Create('Invalid EncodeAsSQLPrepared(0)');
          W.AddShorter('update ');
          W.AddString(TableName);
          if DecodedFieldTypesToUnnest <> nil then
          begin
            // PostgreSQL bulk update via nested array binding
            W.AddShort(' as t set ');
            for F := 0 to FieldCount - 1 do
            begin
              W.AddString(DecodedFieldNames^[F]);
              W.AddShorter('=v.');
              W.AddString(DecodedFieldNames^[F]);
              W.Add(',');
            end;
            W.CancelLastComma;
            W.AddShort(' from ( select');
            for F := 0 to FieldCount - 1 do
            begin
              W.AddShort(' unnest(?::');
              W.AddShort(PG_FT[DecodedFieldTypesToUnnest^[F]]);
              W.AddShorter('[]),');
            end;
            W.AddShort(' unnest(?::int8[]) ) as v('); // last param is ID
            for F := 0 to FieldCount - 1 do
            begin
              W.AddString(DecodedFieldNames^[F]);
              W.Add(',');
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
            for F := 0 to FieldCount - 1 do
            begin // append 'COL1=?,COL2=?'
              W.AddString(DecodedFieldNames^[F]);
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
          if FieldCount = 0 then
            W.AddShort(' default values')
          else
          begin
            W.Add(' ', '(');
            for F := 0 to FieldCount - 1 do
            begin // append 'COL1,COL2'
              W.AddString(DecodedFieldNames^[F]);
              W.Add(',');
            end;
            W.CancelLastComma;
            W.AddShort(') values (');
            if DecodedFieldTypesToUnnest <> nil then
              // PostgreSQL bulk insert via nested array binding
              for F := 0 to FieldCount - 1 do
              begin
                W.AddShort('unnest(?::');
                W.AddShort(PG_FT[DecodedFieldTypesToUnnest^[F]]);
                W.AddShorter('[]),');
              end
            else
              // regular INSERT statement
              W.AddStrings('?,', FieldCount);
            W.CancelLastComma;
            W.Add(')');
          end;
        end;
    else
      raise EJSONObjectDecoder.CreateUTF8('Unexpected EncodeAsSQLPrepared(%)',
        [ord(Occasion)]);
    end;
    W.SetText(result);
  finally
    W.Free;
  end;
end;

{$ifdef ISDELPHI20062007}
  {$WARNINGS ON}
{$endif}

function TJSONObjectDecoder.EncodeAsSQL(Update: boolean): RawUTF8;
var
  F: integer;
  W: TTextWriter;
  temp: TTextWriterStackBuffer;

  procedure AddValue;
  begin
    if InlinedParams = pInlined then
      W.AddShorter(':(');
    W.AddString(FieldValues[F]);
    if InlinedParams = pInlined then
      W.AddShorter('):,')
    else
      W.Add(',');
  end;

begin
  result := '';
  if FieldCount = 0 then
    exit;
  W := TTextWriter.CreateOwnedStream(temp);
  try
    if Update then
    begin
      for F := 0 to FieldCount - 1 do
        // append 'COL1=...,COL2=...'
        if not IsRowID(pointer(DecodedFieldNames^[F])) then
        begin
          W.AddString(DecodedFieldNames^[F]);
          W.Add('=');
          AddValue;
        end;
      W.CancelLastComma;
    end
    else
    begin // returns ' (COL1,COL2) VALUES ('VAL1',VAL2)'
      W.Add(' ', '(');
      for F := 0 to FieldCount - 1 do
      begin // append 'COL1,COL2'
        W.AddString(DecodedFieldNames^[F]);
        W.Add(',');
      end;
      W.CancelLastComma;
      W.AddShort(') VALUES (');
      for F := 0 to FieldCount - 1 do
        AddValue;
      W.CancelLastComma;
      W.Add(')');
    end;
    W.SetText(result);
  finally
    W.Free;
  end;
end;

procedure TJSONObjectDecoder.EncodeAsJSON(out result: RawUTF8);
var
  F: integer;
  W: TTextWriter;
  temp: TTextWriterStackBuffer;
begin
  if FieldCount = 0 then
    exit;
  W := TTextWriter.CreateOwnedStream(temp);
  try
    W.Add('{');
    for F := 0 to FieldCount - 1 do
    begin
      W.AddFieldName(DecodedFieldNames^[F]);
      if FieldTypeApproximation[F] in [ftaBlob, ftaDate, ftaString] then
        if InlinedParams = pNonQuoted then
          W.AddJSONString(FieldValues[F])
        else
          W.AddQuotedStringAsJSON(FieldValues[F])
      else
        W.AddString(FieldValues[F]);
      W.Add(',');
    end;
    W.CancelLastComma;
    W.Add('}');
    W.SetText(result);
  finally
    W.Free;
  end;
end;

function TJSONObjectDecoder.FindFieldName(const FieldName: RawUTF8): PtrInt;
begin
  for result := 0 to FieldCount - 1 do
    if IdemPropNameU(FieldNames[result], FieldName) then
      exit;
  result := -1;
end;

procedure TJSONObjectDecoder.AddFieldValue(const FieldName, FieldValue: RawUTF8;
  FieldType: TJSONObjectDecoderFieldType);
begin
  if FieldCount = MAX_SQLFIELDS then
    raise EJSONObjectDecoder.CreateUTF8(
      'Too many fields for TJSONObjectDecoder.AddField(%) max=%',
      [FieldName, MAX_SQLFIELDS]);
  FieldNames[FieldCount] := FieldName;
  FieldValues[FieldCount] := FieldValue;
  FieldTypeApproximation[FieldCount] := FieldType;
  inc(FieldCount);
end;

const
  FROMINLINED: array[boolean] of TJSONObjectDecoderParams = (pQuoted, pInlined);

function GetJSONObjectAsSQL(var P: PUTF8Char; const Fields: TRawUTF8DynArray;
  Update, InlinedParams: boolean; RowID: TID; ReplaceRowIDWithID: boolean): RawUTF8;
var
  Decoder: TJSONObjectDecoder;
begin
  Decoder.Decode(P, Fields, FROMINLINED[InlinedParams], RowID, ReplaceRowIDWithID);
  result := Decoder.EncodeAsSQL(Update);
end;

function GetJSONObjectAsSQL(const JSON: RawUTF8; Update, InlinedParams: boolean;
  RowID: TID; ReplaceRowIDWithID: boolean): RawUTF8;
var
  Decoder: TJSONObjectDecoder;
begin
  Decoder.Decode(JSON, nil, FROMINLINED[InlinedParams], RowID, ReplaceRowIDWithID);
  result := Decoder.EncodeAsSQL(Update);
end;

function Expect(var P: PUTF8Char; Value: PUTF8Char; ValueLen: PtrInt): boolean;
  {$ifdef HASINLINE}inline;{$endif}
var
  i: PtrInt;
begin // ValueLen is at least 8 bytes long
  result := false;
  if P = nil then
    exit;
  while (P^ <= ' ') and
        (P^ <> #0) do
    inc(P);
  if PPtrInt(P)^ = PPtrInt(Value)^ then
  begin
    for i := SizeOf(PtrInt) to ValueLen - 1 do
      if P[i] <> Value[i] then
        exit;
    inc(P, ValueLen);
    result := true;
  end;
end;

const
  FIELDCOUNT_PATTERN: PUTF8Char = '{"fieldCount":'; // 14 chars
  ROWCOUNT_PATTERN: PUTF8Char = ',"rowCount":';   // 12 chars
  VALUES_PATTERN: PUTF8Char = ',"values":[';    // 11 chars

function UnJSONFirstField(var P: PUTF8Char): RawUTF8;
// expand=true: [ {"col1":val11} ] -> val11
// expand=false: { "fieldCount":1,"values":["col1",val11] } -> vall11
begin
  result := '';
  if P = nil then
    exit;
  if Expect(P, FIELDCOUNT_PATTERN, 14) then
  begin
    // not expanded format
    if GetNextItemCardinal(P) <> 1 then
      exit; // wrong field count
    while P^ <> '[' do
      if P^ = #0 then
        exit
      else
        inc(P); // go to ["col1"
    inc(P); // go to "col1"
  end
  else
  begin
    // expanded format
    while P^ <> '[' do
      if P^ = #0 then
        exit
      else
        inc(P); // need an array of objects
    repeat
      inc(P);
      if P^ = #0 then
        exit;
    until P^ = '{'; // go to object begining
  end;
  if GetJSONPropName(P) <> nil then // ignore field name
    result := GetJSONField(P, P); // get field value
end;

function IsNotAjaxJSON(P: PUTF8Char): boolean;
begin
  result := Expect(P, FIELDCOUNT_PATTERN, 14);
end;

function NotExpandedBufferRowCountPos(P, PEnd: PUTF8Char): PUTF8Char;
var
  i: PtrInt;
begin
  result := nil;
  if (PEnd <> nil) and (PEnd - P > 24) then
    for i := 1 to 24 do // search for "rowCount": at the end of the JSON buffer
      case PEnd[-i] of
        ']', ',':
          exit;
        ':':
          begin
            if CompareMemFixed(PEnd - i - 11, pointer(ROWCOUNT_PATTERN), 11) then
              result := PEnd - i + 1;
            exit;
          end;
      end;
end;

function IsNotExpandedBuffer(var P: PUTF8Char; PEnd: PUTF8Char;
  var FieldCount, RowCount: integer): boolean;

  procedure GetRowCountNotExpanded(P: PUTF8Char);
  begin
    RowCount := 0;
    repeat
      // get a row
      P := GotoNextJSONItem(P, FieldCount);
      if P = nil then
        exit; // unexpected end
      inc(RowCount);
    until P[-1] = ']'; // end of array
    if P^ in ['}', ','] then
    begin // expected formated JSON stream
      if RowCount > 0 then
        dec(RowCount); // first Row = field names -> data in rows 1..RowCount
    end
    else
      RowCount := -1; // bad format -> no data
  end;

var
  RowCountPos: PUTF8Char;
begin
  if not Expect(P, FIELDCOUNT_PATTERN, 14) then
  begin
    result := false;
    exit;
  end;
  FieldCount := GetNextItemCardinal(P, #0);
  if Expect(P, ROWCOUNT_PATTERN, 12) then
    RowCount := GetNextItemCardinal(P, #0)
  else
  begin
    RowCountPos := NotExpandedBufferRowCountPos(P, PEnd);
    if RowCountPos = nil then
      RowCount := -1
    else // mark "rowCount":.. not available
      RowCount := GetCardinal(RowCountPos);
  end;
  result := (FieldCount <> 0) and Expect(P, VALUES_PATTERN, 11);
  if result and (RowCount < 0) then
    GetRowCountNotExpanded(P); // returns RowCount=-1 if P^ is invalid
end;

function StartWithQuotedID(P: PUTF8Char; out ID: TID): boolean;
begin
  if PCardinal(P)^ and $ffffdfdf =
       ord('I') + ord('D') shl 8 + ord('"') shl 16 + ord(':') shl 24 then
  begin
    SetID(P + 4, ID{%H-});
    result := ID > 0;
    exit;
  end
  else if (PCardinalArray(P)^[0] and $dfdfdfdf =
           ord('R') + ord('O') shl 8 + ord('W') shl 16 + ord('I') shl 24) and
         (PCardinalArray(P)^[1] and $ffffdf =
           ord('D') + ord('"') shl 8 + ord(':') shl 16) then
  begin
    SetID(P + 7, ID);
    result := ID > 0;
    exit;
  end;
  ID := 0;
  result := false;
end;

function StartWithID(P: PUTF8Char; out ID: TID): boolean;
begin
  if PCardinal(P)^ and $ffdfdf =
       ord('I') + ord('D') shl 8 + ord(':') shl 16 then
  begin
    SetID(P + 3, ID{%H-});
    result := ID > 0;
    exit;
  end
  else if (PCardinalArray(P)^[0] and $dfdfdfdf =
           ord('R') + ord('O') shl 8 + ord('W') shl 16 + ord('I') shl 24) and
          (PCardinalArray(P)^[1] and $ffdf =
           ord('D') + ord(':') shl 8) then
  begin
    SetID(P + 6, ID);
    result := ID > 0;
    exit;
  end;
  ID := 0;
  result := false;
end;

function JSONGetID(P: PUTF8Char; out ID: TID): boolean;
begin
  if (P <> nil) and NextNotSpaceCharIs(P, '{') then
    if NextNotSpaceCharIs(P, '"') then
      result := StartWithQuotedID(P, ID)
    else
      result := StartWithID(P, ID)
  else
  begin
    ID := 0;
    result := false;
  end;
end;

function JSONGetObject(var P: PUTF8Char; ExtractID: PID;
  var EndOfObject: AnsiChar; KeepIDField: boolean): RawUTF8;
var
  Beg, PC: PUTF8Char;
begin
  result := '';
  if P = nil then
    exit;
  while (P^ <= ' ') and
        (P^ <> #0) do
    inc(P);
  if P^ <> '{' then
    exit;
  Beg := P;
  P := GotoNextJSONObjectOrArray(Beg);
  if (P <> nil) and not (P^ in ENDOFJSONFIELD) then
    P := nil;
  if P <> nil then
  begin
    EndOfObject := P^;
    inc(P); // ignore end of object, i.e. ',' or ']'
    if ExtractID <> nil then
      if JSONGetID(Beg, ExtractID^) and not KeepIDField then
      begin
        PC := PosChar(Beg, ','); // ignore the '"ID":203,' pair
        if PC = nil then
          exit;
        PC^ := '{';
        FastSetString(result, PC, P - PC - 1);
        exit;
      end;
    FastSetString(result, Beg, P - Beg - 1);
  end;
end;


{ ************ TJSONSerializer Class for TOrm Serialization }

{ TJSONSerializer }

procedure TJSONSerializer.SetOrmOptions(Value: TJSONSerializerOrmOptions);
begin
  fOrmOptions := Value;
  if Value * [jwoAsJsonNotAsString, jwoID_str] <> [] then
    if (ColNames <> nil) and (ColNames[0] = '"RowID":') then
      ColNames[0] := '"ID":'; // as expected by AJAX
end;



{ ************ TOrmPropInfo Classes for Efficient ORM Processing }

{ TOrmPropInfo }

const
  NULL_SHORTSTRING: string[1] = '';

function TOrmPropInfo.GetOrmFieldTypeName: PShortString;
begin
  if self = nil then
    result := @NULL_SHORTSTRING
  else
    result := ToText(fOrmFieldType);
end;

function TOrmPropInfo.GetSQLFieldRTTITypeName: RawUTF8;
begin
  result := GetDisplayNameFromClass(ClassType);
  if IdemPChar(pointer(result), 'PROPINFO') then
    delete(result, 1, 8);
end;

function TOrmPropInfo.GetNameDisplay: string;
begin
  GetCaptionFromPCharLen(pointer(fName), result);
end;

procedure TOrmPropInfo.TextToBinary(Value: PUTF8Char; var result: RawByteString);
begin
  result := BlobToRawBlob(Value);
end;

procedure TOrmPropInfo.BinaryToText(var Value: RawUTF8; ToSQL: boolean;
  wasSQLString: PBoolean);
begin
  if Value = '' then
  begin
    if wasSQLString <> nil then
      wasSQLString^ := false;
    Value := NULL_STR_VAR;
  end
  else
  begin
    if wasSQLString <> nil then
      wasSQLString^ := true;
    if ToSQL then
      // encode as BLOB literals (e.g. "X'53514C697465'")
      Value := RawBlobToBlob(RawBlob(Value))
    else
      // JSON content is e.g. '\uFFF0base64encodedbinary'
      Value := BinToBase64WithMagic(Value);
  end;
end;

function NullableTypeToOrmFieldType(aType: PRttiInfo): TOrmFieldType;
begin
  if aType <> nil then
    if aType <> TypeInfo(TNullableInteger) then
      if aType <> TypeInfo(TNullableUTF8Text) then
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
        result := oftUTF8Text
    else
      result := oftInteger
  else
    result := oftUnknown;
end;

const
  SQLFIELDTYPETODBFIELDTYPE: array[TOrmFieldType] of TSQLDBFieldType = (
    ftUnknown,   // oftUnknown
    ftUTF8,      // oftAnsiText
    ftUTF8,      // oftUTF8Text
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
    ftUTF8,      // oftObject
    ftUTF8,      // oftVariant
    ftNull,      // oftNullable
    ftBlob,      // oftBlob
    ftBlob,      // oftBlobDynArray
    ftBlob,      // oftBlobCustom
    ftUTF8,      // oftUTF8Custom
    ftUnknown,   // oftMany
    ftInt64,     // oftModTime
    ftInt64,     // oftCreateTime
    ftInt64,     // oftTID
    ftInt64,     // oftRecordVersion = TRecordVersion
    ftInt64,     // oftSessionUserID
    ftDate,      // oftDateTimeMS
    ftInt64,     // oftUnixTime = TUnixTime
    ftInt64);    // oftUnixMSTime = TUnixMSTime

function OrmFieldTypeToDBField(aOrmFieldType: TOrmFieldType;
  aTypeInfo: PRttiInfo): TSQLDBFieldType;
  {$ifdef HASINLINE}inline;{$endif}
begin
  if aOrmFieldType = oftNullable then
    aOrmFieldType := NullableTypeToOrmFieldType(aTypeInfo);
  result := SQLFIELDTYPETODBFIELDTYPE[aOrmFieldType];
end;

constructor TOrmPropInfo.Create(const aName: RawUTF8; aOrmFieldType:
  TOrmFieldType; aAttributes: TOrmPropInfoAttributes;
  aFieldWidth, aPropertyIndex: integer);
begin
  if aName = '' then
    raise EModelException.CreateUTF8('Void name for %.Create', [self]);
  if aAuxiliaryRTreeField in aAttributes then
    // '_NormalField' -> 'NormalField'
    fName := copy(aName, 2, MaxInt)
  else
    fName := aName;
  fNameUnflattened := fName;
  fOrmFieldType := aOrmFieldType;
  fOrmFieldTypeStored := aOrmFieldType;
  fSQLDBFieldType := SQLFIELDTYPETODBFIELDTYPE[fOrmFieldTypeStored];
  fAttributes := aAttributes;
  fFieldWidth := aFieldWidth;
  fPropertyIndex := aPropertyIndex;
end;

function TOrmPropInfo.GetHash(Instance: TObject; CaseInsensitive: boolean): cardinal;
var
  tmp: RawUTF8;
begin
  GetValueVar(Instance, false, tmp, nil);
  result := crc32c(0, pointer(tmp), length(tmp));
end;

procedure TOrmPropInfo.GetJSONValues(Instance: TObject; W: TJSONSerializer);
var
  wasString: boolean;
  tmp: RawUTF8;
begin
  GetValueVar(Instance, false, tmp, @wasString);
  if wasString then
  begin
    W.Add('"');
    if tmp <> '' then
      W.AddJSONEscape(pointer(tmp));
    W.Add('"');
  end
  else
    W.AddRawJSON(tmp);
end;

function TOrmPropInfo.GetValue(Instance: TObject; ToSQL: boolean;
  wasSQLString: PBoolean): RawUTF8;
begin
  GetValueVar(Instance, ToSQL, result, wasSQLString);
end;

procedure TOrmPropInfo.SetValueVar(Instance: TObject; const Value: RawUTF8;
  wasString: boolean);
begin
  SetValue(Instance, pointer(Value), wasString);
end;

function TOrmPropInfo.SQLDBFieldTypeName: PShortString;
begin
  result := ToText(fSQLDBFieldType);
end;

procedure TOrmPropInfo.GetFieldSQLVar(Instance: TObject; var aValue: TSQLVar;
  var temp: RawByteString);
begin
  GetValueVar(Instance, true, RawUTF8(temp), nil);
  aValue.Options := [];
  aValue.VType := fSQLDBFieldType;
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
    ftUTF8:
      aValue.VText := pointer(temp);
  else
    aValue.VInt64 := 0;
  end;
end;

function TOrmPropInfo.IsValueVoid(Instance: TObject): boolean;
var
  temp: RawUTF8;
  wasString: boolean;
begin
  GetValueVar(Instance, true, temp, @wasString);
  if wasString then
    result := temp = ''
  else
    result := GetInt64(pointer(temp)) = 0;
end;

function TOrmPropInfo.SetFieldSQLVar(Instance: TObject;
  const aValue: TSQLVar): boolean;
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
    ftUTF8:
      SetValue(Instance, aValue.VText, true);
  else
    SetValue(Instance, nil, false);
  end;
  result := true;
end;

const
  NULL_LOW  = ord('n') + ord('u') shl 8 + ord('l') shl 16 + ord('l') shl 24;
  FALSE_LOW = ord('f') + ord('a') shl 8 + ord('l') shl 16 + ord('s') shl 24;
  TRUE_LOW  = ord('t') + ord('r') shl 8 + ord('u') shl 16 + ord('e') shl 24;

procedure ValueVarToVariant(Value: PUTF8Char; ValueLen: integer;
  fieldType: TOrmFieldType; var result: TVarData; createValueTempCopy: boolean;
  typeInfo: PRttiInfo; options: TDocVariantOptions);
const
  /// map our available types for any SQL field property into variant values
  // - varNull will be used to store a true variant instance from JSON
  SQL_ELEMENTTYPES: array[TOrmFieldType] of word = (
 // oftUnknown, oftAnsiText, oftUTF8Text, oftEnumerate, oftSet,   oftInteger,
    varEmpty, varString, varString, varInteger, varInt64, varInt64,
 // oftID, oftRecord, oftBoolean, oftFloat, oftDateTime,
    varInt64, varInt64, varBoolean, varDouble, varDate,
 //  oftTimeLog, oftCurrency,  oftObject,
    varInt64, varCurrency, varNull,
 // oftVariant, oftNullable, oftBlob, oftBlobDynArray,
    varNull, varNull, varString, varNull,
 // oftBlobCustom, oftUTF8Custom, oftMany, oftModTime, oftCreateTime, oftTID,
    varString, varString, varEmpty, varInt64, varInt64, varInt64,
 // oftRecordVersion, oftSessionUserID, oftDateTimeMS, oftUnixTime, oftUnixMSTime
    varInt64, varInt64, varDate, varInt64, varInt64);

  procedure Complex;
  var
    tmp: TSynTempBuffer;
  begin
    if (fieldType = oftBlobDynArray) and (typeInfo <> nil) and (Value <> nil) and
       (Value^ <> '[') and Base64MagicCheckAndDecode(Value, tmp) then
      Value := pointer(DynArrayBlobSaveJSON(typeInfo, tmp.buf))
    else if createValueTempCopy then
      Value := tmp.Init(Value)
    else
      tmp.buf := nil;
    GetVariantFromJSON(Value, false, variant(result), @options);
    tmp.Done;
  end;

var
  err: integer;
begin
  VarClear(variant(result));
  result.VType := SQL_ELEMENTTYPES[fieldType];
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
          FastSetString(RawUTF8(result.VAny), Value, ValueLen);
        end;
      end;
    oftDateTime, oftDateTimeMS:
      Iso8601ToDateTimePUTF8CharVar(Value, 0, result.VDate);
    oftBoolean:
      result.VBoolean := not ((Value = nil) or
                         (PWord(Value)^ = ord('0')) or
                         (PInteger(Value)^ = FALSE_LOW));
    oftEnumerate:
      result.VInteger := GetInteger(Value);
    oftInteger, oftID, oftTID, oftRecord, oftSet, oftRecordVersion,
    oftSessionUserID, oftTimeLog, oftModTime, oftCreateTime,
    oftUnixTime, oftUnixMSTime:
      SetInt64(Value, result.VInt64);
    oftAnsiText, oftUTF8Text:
      FastSetString(RawUTF8(result.VAny), Value, ValueLen);
    oftBlobCustom, oftBlob:
      BlobToRawBlob(Value, RawBlob(result.VAny));
    oftVariant, oftNullable, oftBlobDynArray, oftObject, oftUTF8Custom:
      Complex;
  end;
end;

procedure TOrmPropInfo.GetVariant(Instance: TObject; var Dest: Variant);
var
  temp: RawUTF8;
begin
  GetValueVar(Instance, true, temp, nil);
  ValueVarToVariant(pointer(temp), Length(temp), fOrmFieldTypeStored,
    TVarData(Dest), false, nil);
end;

procedure TOrmPropInfo.SetVariant(Instance: TObject; const Source: Variant);
begin
  SetValueVar(Instance, VariantToUTF8(Source),
    (TVarData(Source).VType = varOleStr) or (TVarData(Source).VType >= varString));
end;

function TOrmPropInfo.CompareValue(Item1, Item2: TObject;
  CaseInsensitive: boolean): PtrInt;
var
  tmp1, tmp2: RawUTF8;
begin
  if Item1 = Item2 then
    result := 0
  else if Item1 = nil then
    result := -1
  else if Item2 = nil then
    result := 1
  else
  begin
    GetValueVar(Item1, false, tmp1, nil);
    GetValueVar(Item2, false, tmp2, nil);
    if CaseInsensitive then // slow, always working implementation
      result := StrIComp(pointer(tmp1), pointer(tmp2))
    else
      result := StrComp(pointer(tmp1), pointer(tmp2));
  end;
end;

procedure TOrmPropInfo.CopyProp(Source: TObject; DestInfo: TOrmPropInfo;
  Dest: TObject);

  procedure GenericCopy;
  var
    tmp: RawUTF8;
    wasString: boolean;
    val: variant;
  begin
    // force JSON serialization, e.g. for dynamic arrays
    if (DestInfo.OrmFieldType = oftVariant) or
       (OrmFieldType = oftVariant) then
    begin
      GetVariant(Source, val);
      DestInfo.SetVariant(Dest, val);
      exit;
    end;
    GetValueVar(Source, false, tmp, @wasString);
    DestInfo.SetValueVar(Dest, tmp, wasString);
  end;

var
  i: PtrInt;
begin
  if (Source = nil) or (DestInfo = nil) or (Dest = nil) then
    exit; // avoid GPF
  with TOrmPropInfoRTTI(self) do
    if fFromRTTI and (fFlattenedProps <> nil) then
      for i := 0 to length(fFlattenedProps) - 1 do
        Source := fFlattenedProps[i].GetObjProp(Source);
  with TOrmPropInfoRTTI(DestInfo) do
    if fFromRTTI and (fFlattenedProps <> nil) then
      for i := 0 to length(fFlattenedProps) - 1 do
        Dest := fFlattenedProps[i].GetObjProp(Dest);
  if DestInfo.ClassType = ClassType then
    CopySameClassProp(Source, DestInfo, Dest)
  else
    GenericCopy;
end;

procedure TOrmPropInfo.CopyValue(Source, Dest: TObject);
begin
  CopySameClassProp(Source, self, Dest);
end;

procedure TOrmPropInfo.CopySameClassProp(Source: TObject;
  DestInfo: TOrmPropInfo; Dest: TObject);
var
  tmp: RawUTF8;
  wasString: boolean;
begin
  GetValueVar(Source, false, tmp, @wasString);
  DestInfo.SetValueVar(Dest, tmp, wasString);
end;


{ TOrmPropInfoRTTI }

var
  OrmPropInfoRegistration: TSynDictionary = nil;

class procedure TOrmPropInfoRTTI.RegisterTypeInfo(aTypeInfo: PRttiInfo);
begin
  if OrmPropInfoRegistration = nil then
    OrmPropInfoRegistration := TSynDictionary.Create(
      TypeInfo(TPointerDynArray), TypeInfo(TPointerDynArray));
  OrmPropInfoRegistration.AddOrUpdate(aTypeInfo, self);
end;

class function TOrmPropInfoRTTI.CreateFrom(aPropInfo: PRttiProp;
  aPropIndex: integer; aOptions: TOrmPropInfoListOptions;
  const aFlattenedProps: PRttiPropDynArray): TOrmPropInfo;
var
  aOrmFieldType: TOrmFieldType;
  aType: PRttiInfo;
  C: TOrmPropInfoRTTIClass;

  procedure FlattenedPropNameSet;
  var
    i, max: PtrInt;
  begin // Address.Street1 -> Address_Street1
    (result as TOrmPropInfoRTTI).fFlattenedProps := aFlattenedProps;
    result.fNameUnflattened := result.fName;
    max := high(aFlattenedProps);
    for i := max downto 0 do
      result.fNameUnflattened :=
        ToUTF8(aFlattenedProps[i]^.Name^) + '.' + result.fNameUnflattened;
    if (max >= 0) and
       (aFlattenedProps[max]^.TypeInfo^.ClassFieldCount(
         {withoutgetter=}pilIgnoreIfGetter in aOptions) = 1) then
    begin
      // Birth.Date -> Birth or Address.Country.Iso -> Address_Country
      result.fName := ToUTF8(aFlattenedProps[max]^.Name^);
      dec(max);
    end;
    for i := max downto 0 do
      result.fName := ToUTF8(aFlattenedProps[i]^.Name^) + '_' + result.fName;
  end;

begin
  if aPropInfo = nil then
    raise EModelException.CreateUTF8('Invalid %.CreateFrom(nil) call', [self]);
  result := nil;
  aOrmFieldType := oftUnknown;
  aType := aPropInfo^.typeInfo;
  if aType^.Kind = rkVariant then
  begin
    aOrmFieldType := NullableTypeToOrmFieldType(aType);
    if aOrmFieldType <> oftUnknown then // handle oftNullable type
      result := TOrmPropInfoRTTIVariant.Create(aPropInfo, aPropIndex,
        aOrmFieldType, aOptions);
  end;
  if result = nil then
  begin
    aOrmFieldType := GetOrmFieldType(aType);
    C := nil;
    if (OrmPropInfoRegistration = nil) or
       not OrmPropInfoRegistration.FindAndCopy(aType, C) then
      case aOrmFieldType of
        oftUnknown, oftBlobCustom:
          ; // will raise an EOrmException
        oftBoolean, oftEnumerate:
          C := TOrmPropInfoRTTIEnum;
        oftTimeLog, oftModTime, oftCreateTime:
          // specific class for further use
          C := TOrmPropInfoRTTITimeLog;
        oftUnixTime:
          // specific class for further use
          C := TOrmPropInfoRTTIUnixTime;
        oftUnixMSTime:
          C := TOrmPropInfoRTTIUnixMSTime;
        oftCurrency:
          C := TOrmPropInfoRTTICurrency;
        oftDateTime, oftDateTimeMS:
          C := TOrmPropInfoRTTIDateTime;
        oftID: // = TOrm(aID)
          C := TOrmPropInfoRTTIID;
        oftTID:
          // = TID or T*ID
          C := TOrmPropInfoRTTITID;
        oftSessionUserID:
          C := TOrmPropInfoRTTIInt64;
        oftRecord:
          // = TRecordReference/TRecordReferenceToBeDeleted
          C := TOrmPropInfoRTTIRecordReference;
        oftRecordVersion:
          C := TOrmPropInfoRTTIRecordVersion;
        oftMany:
          C := TOrmPropInfoRTTIMany;
        oftObject:
          C := TOrmPropInfoRTTIObject;
        oftVariant:
          C := TOrmPropInfoRTTIVariant;  // oftNullable already handle above
        oftBlob:
          C := TOrmPropInfoRTTIRawBlob;
        oftBlobDynArray:
          C := TOrmPropInfoRTTIDynArray;
        oftUTF8Custom:
          // will happen only for DELPHI XE5 and up
          result := TOrmPropInfoCustomJSON.Create(aPropInfo, aPropIndex);
      else
        case aType^.Kind of // retrieve matched type from RTTI binary level
          rkInteger:
            C := TOrmPropInfoRTTIInt32;
          rkSet:
            C := TOrmPropInfoRTTISet;
          rkChar, rkWChar:
            C := TOrmPropInfoRTTIChar;
          rkInt64 {$ifdef FPC}, rkQWord{$endif}:
            C := TOrmPropInfoRTTIInt64;
          rkFloat:
            if aType^.RttiFloat = rfDouble then
              C := TOrmPropInfoRTTIDouble;
          rkLString:
            case aType^.AnsiStringCodePage of
              // recognize optimized UTF-8/UTF-16
              CP_UTF8:
                C := TOrmPropInfoRTTIRawUTF8;
              CP_UTF16:
                C := TOrmPropInfoRTTIRawUnicode;
            else
              C := TOrmPropInfoRTTIAnsi; // will use the right TSynAnsiConvert
            end;
        {$ifdef HASVARUSTRING}
          rkUString:
            C := TOrmPropInfoRTTIUnicode;
        {$endif HASVARUSTRING}
          rkWString:
            C := TOrmPropInfoRTTIWide;
        end;
      end;
    if C <> nil then
      result := C.Create(aPropInfo, aPropIndex, aOrmFieldType, aOptions);
  end;
  if result <> nil then
  begin
    if aFlattenedProps <> nil then
      FlattenedPropNameSet;
  end
  else if pilRaiseEOrmExceptionIfNotHandled in aOptions then
    raise EModelException.CreateUTF8('%.CreateFrom: Unhandled %/% type for property %',
      [self, ToText(aOrmFieldType)^, ToText(aType^.Kind)^, aPropInfo^.Name]);
end;

function TOrmPropInfoRTTI.GetSQLFieldRTTITypeName: RawUTF8;
begin
  result := ToUTF8(fPropType^.Name^);
end;

function TOrmPropInfoRTTI.GetFieldAddr(Instance: TObject): pointer;
begin
  if Instance = nil then
    result := nil
  else
    result := fPropInfo^.GetFieldAddr(Instance);
end;

function TOrmPropInfoRTTI.Flattened(Instance: TObject): TObject;
var
  i: integer;
begin
  result := Instance;
  for i := 0 to length(fFlattenedProps) - 1 do
    result := fFlattenedProps[i].GetObjProp(result);
end;

procedure TOrmPropInfoRTTI.GetVariant(Instance: TObject; var Dest: Variant);
var
  temp: RawUTF8;
begin
  GetValueVar(Instance, true, temp, nil);
  ValueVarToVariant(pointer(temp), length(temp), fOrmFieldTypeStored,
    TVarData(Dest), false, fPropInfo^.TypeInfo);
end;

constructor TOrmPropInfoRTTI.Create(aPropInfo: PRttiProp; aPropIndex: integer;
  aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions);
var
  attrib: TOrmPropInfoAttributes;
begin
  byte(attrib) := 0;
  if aPropInfo^.IsStored(nil) = AS_UNIQUE then
    Include(attrib, aIsUnique); // property MyProperty: RawUTF8 stored AS_UNIQUE;
  if (pilAuxiliaryFields in aOptions) and (aPropInfo^.Name^[1] = '_') then
    Include(attrib, aAuxiliaryRTreeField);
  inherited Create(ToUTF8(aPropInfo^.Name^), aOrmFieldType, attrib, aPropInfo^.Index,
    aPropIndex); // property MyProperty: RawUTF8 index 10; -> FieldWidth=10
  fPropInfo := aPropInfo;
  fPropType := aPropInfo^.typeInfo;
  fPropRtti := Rtti.RegisterType(fPropType) as TRttiJson;
  if fPropRtti = nil then
    raise EModelException.CreateUTF8('%.Create(%): unknown type', [self, aPropInfo^.Name^]);
  fPropRttiProp := fPropRtti.Props.Find(aPropInfo^.Name^);
  if (fPropRttiProp <> nil) and (fPropRttiProp.Prop <> fPropInfo) then
    raise EModelException.CreateUTF8('%.Create(%): invalid prop', [self, aPropInfo^.Name^]);
  if aPropInfo.GetterIsField then
  begin
    fGetterIsFieldPropOffset := PtrUInt(fPropInfo.GetterAddr(nil));
    if aPropInfo.SetterCall = rpcField then
      fInPlaceCopySameClassPropOffset := fGetterIsFieldPropOffset;
  end;
  fFromRTTI := true;
end;


{ TOrmPropInfoRTTIInt32 }

constructor TOrmPropInfoRTTIInt32.Create(aPropInfo: PRttiProp;
  aPropIndex: integer; aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions);
begin
  inherited Create(aPropInfo, aPropIndex, aOrmFieldType, aOptions);
  fUnsigned := fPropType^.RttiOrd in [roUByte, roUWord, roULong];
end;

procedure TOrmPropInfoRTTIInt32.CopySameClassProp(Source: TObject;
  DestInfo: TOrmPropInfo; Dest: TObject);
begin
  TOrmPropInfoRTTIInt32(DestInfo).fPropInfo.SetOrdProp(Dest, fPropInfo.GetOrdProp
    (Source));
end;

procedure TOrmPropInfoRTTIInt32.GetBinary(Instance: TObject; W: TBufferWriter);
begin
  W.WriteVarUInt32(cardinal(fPropInfo.GetOrdProp(Instance)));
end;

function TOrmPropInfoRTTIInt32.GetHash(Instance: TObject;
  CaseInsensitive: boolean): cardinal;
var
  v: integer;
begin
  v := fPropInfo.GetOrdProp(Instance);
  result := crc32cBy4(0, v); // better hash distribution using crc32c
end;

procedure TOrmPropInfoRTTIInt32.GetJSONValues(Instance: TObject; W: TJSONSerializer);
var
  v: integer;
begin
  v := fPropInfo.GetOrdProp(Instance);
  if fUnsigned then
    W.AddU(cardinal(v))
  else
    W.Add(v);
end;

procedure TOrmPropInfoRTTIInt32.GetValueVar(Instance: TObject; ToSQL: boolean;
  var result: RawUTF8; wasSQLString: PBoolean);
var
  v: integer;
begin
  if wasSQLString <> nil then
    wasSQLString^ := false;
  v := fPropInfo.GetOrdProp(Instance);
  if fUnsigned then
    UInt32ToUtf8(cardinal(v), result)
  else
    Int32ToUtf8(v, result);
end;

procedure TOrmPropInfoRTTIInt32.NormalizeValue(var Value: RawUTF8);
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

function TOrmPropInfoRTTIInt32.CompareValue(Item1, Item2: TObject;
  CaseInsensitive: boolean): PtrInt;
var
  A, B: integer;
begin
  if Item1 = Item2 then
    result := 0
  else if Item1 = nil then
    result := -1
  else if Item2 = nil then
    result := 1
  else
  begin
    A := fPropInfo.GetOrdProp(Item1);
    B := fPropInfo.GetOrdProp(Item2);
    result := CompareInteger(A, B);
  end;
end;

function TOrmPropInfoRTTIInt32.SetBinary(Instance: TObject; P, PEnd: PAnsiChar): PAnsiChar;
var
  c: cardinal;
begin
  if P <> nil then
  begin
    P := pointer(FromVarUInt32Safe(pointer(P), pointer(PEnd), c));
    if P <> nil then
      fPropInfo.SetOrdProp(Instance, integer(c));
  end;
  result := P;
end;

procedure TOrmPropInfoRTTIInt32.SetValue(Instance: TObject; Value: PUTF8Char;
  wasString: boolean);
begin
  fPropInfo.SetOrdProp(Instance, GetInteger(Value));
end;

function TOrmPropInfoRTTIInt32.SetFieldSQLVar(Instance: TObject;
  const aValue: TSQLVar): boolean;
begin
  if aValue.VType = ftInt64 then
  begin
    fPropInfo.SetOrdProp(Instance, aValue.VInt64);
    result := true;
  end
  else
    result := inherited SetFieldSQLVar(Instance, aValue);
end;

procedure TOrmPropInfoRTTIInt32.GetFieldSQLVar(Instance: TObject;
  var aValue: TSQLVar; var temp: RawByteString);
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


{ TOrmPropInfoRTTISet }

constructor TOrmPropInfoRTTISet.Create(aPropInfo: PRttiProp; aPropIndex: integer;
  aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions);
begin
  inherited Create(aPropInfo, aPropIndex, aOrmFieldType, aOptions);
  fSetEnumType := fPropType^.SetEnumType;
end;


{ TOrmPropInfoRTTIEnum }

constructor TOrmPropInfoRTTIEnum.Create(aPropInfo: PRttiProp;
  aPropIndex: integer; aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions);
begin
  inherited Create(aPropInfo, aPropIndex, aOrmFieldType, aOptions);
  fEnumType := fPropType^.EnumBaseType;
end;

procedure TOrmPropInfoRTTIEnum.GetJSONValues(Instance: TObject; W: TJSONSerializer);
var
  i: PtrInt;
begin
  i := fPropInfo.GetOrdProp(Instance);
  if fOrmFieldType = oftBoolean then
    W.Add(i <> 0)
  else
    W.Add(i);
end;

function TOrmPropInfoRTTIEnum.GetCaption(Value: RawUTF8; out IntValue: integer): string;
begin
  NormalizeValue(Value);
  IntValue := GetInteger(pointer(Value));
  if Value = '' then
    result := ''
  else
    result := EnumType^.GetCaption(IntValue);
end;

procedure TOrmPropInfoRTTIEnum.GetValueVar(Instance: TObject; ToSQL: boolean;
  var result: RawUTF8; wasSQLString: PBoolean);
var
  i: PtrInt;
begin
  if wasSQLString <> nil then
    wasSQLString^ := false;
  i := fPropInfo.GetOrdProp(Instance);
  if (fOrmFieldType = oftBoolean) and not ToSQL then
    result := BOOL_UTF8[i <> 0]
  else
    UInt32ToUtf8(i, result);
end;

procedure TOrmPropInfoRTTIEnum.NormalizeValue(var Value: RawUTF8);
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

procedure TOrmPropInfoRTTIEnum.SetValue(Instance: TObject; Value: PUTF8Char;
  wasString: boolean);
var
  i, err, len: integer;
begin
  if Value = nil then
    i := 0
  else
  begin
    i := GetInteger(Value, err);
    if err <> 0 then
    begin // we allow a value stated as text
      if fOrmFieldType = oftBoolean then
      begin
        len := StrLen(Value);
        i := ord(IdemPropName('TRUE', Value, len) or IdemPropName('YES', Value, len));
      end
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


{ TOrmPropInfoRTTIChar }

procedure TOrmPropInfoRTTIChar.GetValueVar(Instance: TObject; ToSQL: boolean;
  var result: RawUTF8; wasSQLString: PBoolean);
var
  w: WideChar;
begin
  w := WideChar(fPropInfo.GetOrdProp(Instance));
  if ToSQL and (w = #0) then
  begin
    // 'null' and not #0 to avoid end of SQL text - JSON will escape #0
    result := NULL_STR_VAR;
    if wasSQLString <> nil then
      wasSQLString^ := false;
  end
  else
  begin
    RawUnicodeToUtf8(@w, 1, result);
    if wasSQLString <> nil then
      wasSQLString^ := true;
  end;
end;

procedure TOrmPropInfoRTTIChar.NormalizeValue(var Value: RawUTF8);
begin // do nothing: should already be UTF-8 encoded
end;

procedure TOrmPropInfoRTTIChar.SetValue(Instance: TObject; Value: PUTF8Char;
  wasString: boolean);
var
  i: integer;
begin
  if (Value = nil) or (PInteger(Value)^ = NULL_LOW) then
    i := 0
  else
    i := GetUTF8Char(Value);
  fPropInfo.SetOrdProp(Instance, i);
end;


{ TOrmPropInfoRTTIInt64 }

constructor TOrmPropInfoRTTIInt64.Create(aPropInfo: PRttiProp;
  aPropIndex: integer; aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions);
begin
  inherited Create(aPropInfo, aPropIndex, aOrmFieldType, aOptions);
  fIsQWord := fPropType^.IsQword;
end;

procedure TOrmPropInfoRTTIInt64.CopySameClassProp(Source: TObject;
  DestInfo: TOrmPropInfo; Dest: TObject);
begin
  TOrmPropInfoRTTIInt64(DestInfo).fPropInfo.SetInt64Prop(
    Dest, fPropInfo.GetInt64Prop(Source));
end;

procedure TOrmPropInfoRTTIInt64.GetBinary(Instance: TObject; W: TBufferWriter);
var
  V64: Int64;
begin
  V64 := fPropInfo.GetInt64Prop(Instance);
  W.Write(@V64, SizeOf(Int64));
end;

function TOrmPropInfoRTTIInt64.GetHash(Instance: TObject;
  CaseInsensitive: boolean): cardinal;
var
  V64: TQWordRec;
begin
  if fGetterIsFieldPropOffset <> 0 then
    V64.V := PInt64(PtrUInt(Instance) + fGetterIsFieldPropOffset)^
  else
    V64.V := fPropInfo.GetInt64Prop(Instance);
  result := crc32cBy4(V64.L, V64.H); // better hash distribution using crc32c
end;

procedure TOrmPropInfoRTTIInt64.GetJSONValues(Instance: TObject; W: TJSONSerializer);
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

procedure TOrmPropInfoRTTIInt64.GetValueVar(Instance: TObject; ToSQL: boolean;
  var result: RawUTF8; wasSQLString: PBoolean);
var
  V64: Int64;
begin
  if wasSQLString <> nil then
    wasSQLString^ := false;
  if fGetterIsFieldPropOffset <> 0 then
    V64 := PInt64(PtrUInt(Instance) + fGetterIsFieldPropOffset)^
  else
    V64 := fPropInfo.GetInt64Prop(Instance);
  if fIsQWord then
    UInt64ToUtf8(V64, result)
  else
    Int64ToUtf8(V64, result);
end;

procedure TOrmPropInfoRTTIInt64.NormalizeValue(var Value: RawUTF8);
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

function TOrmPropInfoRTTIInt64.CompareValue(Item1, Item2: TObject;
  CaseInsensitive: boolean): PtrInt;
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

function TOrmPropInfoRTTIInt64.SetBinary(Instance: TObject; P, PEnd: PAnsiChar): PAnsiChar;
begin
  if P = nil then
    result := nil
  else
  begin
    result := P + SizeOf(Int64);
    if result > PEnd then
      result := nil
    else
      fPropInfo.SetInt64Prop(Instance, PInt64(P)^);
  end;
end;

procedure TOrmPropInfoRTTIInt64.SetValue(Instance: TObject; Value: PUTF8Char;
  wasString: boolean);
var
  V64: Int64;
begin
  if fIsQWord then
    SetQWord(Value, PQword(@V64)^)
  else
    SetInt64(Value, V64{%H-});
  fPropInfo.SetInt64Prop(Instance, V64);
end;

function TOrmPropInfoRTTIInt64.SetFieldSQLVar(Instance: TObject;
  const aValue: TSQLVar): boolean;
begin
  if aValue.VType = ftInt64 then
  begin
    fPropInfo.SetInt64Prop(Instance, aValue.VInt64);
    result := true;
  end
  else
    result := inherited SetFieldSQLVar(Instance, aValue);
end;

procedure TOrmPropInfoRTTIInt64.GetFieldSQLVar(Instance: TObject;
  var aValue: TSQLVar; var temp: RawByteString);
begin
  aValue.Options := [];
  aValue.VType := ftInt64;
  aValue.VInt64 := fPropInfo.GetInt64Prop(Instance);
end;


{ TOrmPropInfoRTTIDouble }

procedure TOrmPropInfoRTTIDouble.CopySameClassProp(Source: TObject;
  DestInfo: TOrmPropInfo; Dest: TObject);
begin
  TOrmPropInfoRTTIDouble(DestInfo).fPropInfo.SetDoubleProp(
    Dest, fPropInfo.GetDoubleProp(Source));
end;

procedure TOrmPropInfoRTTIDouble.GetJSONValues(Instance: TObject; W: TJSONSerializer);
begin
  W.AddDouble(fPropInfo.GetDoubleProp(Instance));
end;

procedure TOrmPropInfoRTTIDouble.GetValueVar(Instance: TObject; ToSQL: boolean;
  var result: RawUTF8; wasSQLString: PBoolean);
begin
  DoubleToStr(fPropInfo.GetDoubleProp(Instance), result);
  if wasSQLString <> nil then
    wasSQLString^ := (result = '') or
                     not (result[1] in ['0'..'9']);
end;

procedure TOrmPropInfoRTTIDouble.NormalizeValue(var Value: RawUTF8);
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

procedure TOrmPropInfoRTTIDouble.SetValue(Instance: TObject; Value: PUTF8Char;
  wasString: boolean);
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
  fPropInfo.SetDoubleProp(Instance, V);
end;

function TOrmPropInfoRTTIDouble.CompareValue(Item1, Item2: TObject;
  CaseInsensitive: boolean): PtrInt;
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

function TOrmPropInfoRTTIDouble.GetHash(Instance: TObject;
  CaseInsensitive: boolean): cardinal;
var
  V: double;
begin
  V := fPropInfo.GetDoubleProp(Instance);
  with PQWordRec(@V)^ do
    result := crc32cBy4(L, H); // better hash distribution using crc32c
end;

procedure TOrmPropInfoRTTIDouble.GetBinary(Instance: TObject; W: TBufferWriter);
var
  V: double;
begin
  V := fPropInfo.GetDoubleProp(Instance);
  W.Write(@V, SizeOf(V));
end;

{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
type
  unaligned = Double;
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}

function TOrmPropInfoRTTIDouble.SetBinary(Instance: TObject; P, PEnd: PAnsiChar):
  PAnsiChar;
begin
  if P = nil then
    result := nil
  else
  begin
    result := P + SizeOf(double);
    if result > PEnd then
      result := nil
    else
      fPropInfo.SetDoubleProp(Instance, unaligned(PDouble(P)^));
  end;
end;

function TOrmPropInfoRTTIDouble.SetFieldSQLVar(Instance: TObject;
  const aValue: TSQLVar): boolean;
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
      result := inherited SetFieldSQLVar(Instance, aValue);
      exit;
    end;
  end;
  fPropInfo.SetDoubleProp(Instance, V);
  result := true;
end;

procedure TOrmPropInfoRTTIDouble.GetFieldSQLVar(Instance: TObject;
  var aValue: TSQLVar; var temp: RawByteString);
begin
  aValue.Options := [];
  aValue.VType := ftDouble;
  aValue.VDouble := fPropInfo.GetDoubleProp(Instance);
end;


{ TOrmPropInfoRTTICurrency }

procedure TOrmPropInfoRTTICurrency.CopySameClassProp(Source: TObject;
  DestInfo: TOrmPropInfo; Dest: TObject);
var
  curr: currency;
begin
  fPropInfo.GetCurrencyProp(Source, curr);
  TOrmPropInfoRTTICurrency(DestInfo).fPropInfo.SetCurrencyProp(Dest, curr);
end;

procedure TOrmPropInfoRTTICurrency.GetJSONValues(Instance: TObject; W: TJSONSerializer);
var
  curr: currency;
begin
  fPropInfo.GetCurrencyProp(Instance, curr);
  W.AddCurr64(curr);
end;

procedure TOrmPropInfoRTTICurrency.GetValueVar(Instance: TObject; ToSQL: boolean;
  var result: RawUTF8; wasSQLString: PBoolean);
var
  curr: currency;
begin
  if wasSQLString <> nil then
    wasSQLString^ := false;
  fPropInfo.GetCurrencyProp(Instance, curr);
  result := CurrencyToStr(curr);
end;

procedure TOrmPropInfoRTTICurrency.NormalizeValue(var Value: RawUTF8);
begin
  Value := Curr64ToStr(StrToCurr64(pointer(Value)));
end;

procedure TOrmPropInfoRTTICurrency.SetValue(Instance: TObject; Value: PUTF8Char;
  wasString: boolean);
var
  tmp: Int64;
begin
  tmp := StrToCurr64(Value, nil);
  fPropInfo.SetCurrencyProp(Instance, PCurrency(@tmp)^);
end;

function TOrmPropInfoRTTICurrency.CompareValue(Item1, Item2: TObject;
  CaseInsensitive: boolean): PtrInt;
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

function TOrmPropInfoRTTICurrency.GetHash(Instance: TObject;
  CaseInsensitive: boolean): cardinal;
var
  V: currency;
begin
  fPropInfo.GetCurrencyProp(Instance, V);
  with PQWordRec(@V)^ do
    result := crc32cBy4(L, H); // better hash distribution using crc32c
end;

procedure TOrmPropInfoRTTICurrency.GetFieldSQLVar(Instance: TObject;
  var aValue: TSQLVar; var temp: RawByteString);
begin
  aValue.Options := [];
  aValue.VType := ftCurrency;
  fPropInfo.GetCurrencyProp(Instance, aValue.VCurrency);
end;

function TOrmPropInfoRTTICurrency.SetFieldSQLVar(Instance: TObject;
  const aValue: TSQLVar): boolean;
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
      result := inherited SetFieldSQLVar(Instance, aValue);
      exit;
    end;
  end;
  fPropInfo.SetCurrencyProp(Instance, PCurrency(@V)^);
  result := true;
end;

procedure TOrmPropInfoRTTICurrency.GetBinary(Instance: TObject; W: TBufferWriter);
var
  V: currency;
begin
  fPropInfo.GetCurrencyProp(Instance, V);
  W.Write(@V, SizeOf(V));
end;

function TOrmPropInfoRTTICurrency.SetBinary(Instance: TObject;
  P, PEnd: PAnsiChar): PAnsiChar;
begin
  if P = nil then
    result := nil
  else
  begin
    result := P + SizeOf(Currency);
    if result > PEnd then
      result := nil
    else
      fPropInfo.SetCurrencyProp(Instance, PCurrency(P)^);
  end;
end;


{ TOrmPropInfoRTTIDateTime }

procedure TOrmPropInfoRTTIDateTime.GetJSONValues(Instance: TObject; W: TJSONSerializer);
begin
  W.Add('"');
  W.AddDateTime(fPropInfo.GetDoubleProp(Instance), fOrmFieldType = oftDateTimeMS);
  W.Add('"');
end;

function TOrmPropInfoRTTIDateTime.CompareValue(Item1, Item2: TObject;
  CaseInsensitive: boolean): PtrInt;
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

procedure TOrmPropInfoRTTIDateTime.GetValueVar(Instance: TObject; ToSQL: boolean;
  var result: RawUTF8; wasSQLString: PBoolean);
begin
  if wasSQLString <> nil then
    wasSQLString^ := true;
  DateTimeToIso8601TextVar(fPropInfo.GetDoubleProp(Instance), 'T', result,
    fOrmFieldType = oftDateTimeMS);
end;

procedure TOrmPropInfoRTTIDateTime.NormalizeValue(var Value: RawUTF8);
begin
  DateTimeToIso8601TextVar(Iso8601ToDateTime(Value), 'T', Value,
    {withms=}fOrmFieldType = oftDateTimeMS);
end;

procedure TOrmPropInfoRTTIDateTime.SetValue(Instance: TObject; Value: PUTF8Char;
  wasString: boolean);
var
  V: TDateTime;
begin
  Iso8601ToDateTimePUTF8CharVar(Value, 0, V);
  fPropInfo.SetDoubleProp(Instance, V);
end;

procedure TOrmPropInfoRTTIDateTime.GetFieldSQLVar(Instance: TObject;
  var aValue: TSQLVar; var temp: RawByteString);
begin
  if fOrmFieldType = oftDateTimeMS then
    aValue.Options := [svoDateWithMS]
  else
    aValue.Options := [];
  aValue.VType := ftDate;
  aValue.VDouble := fPropInfo.GetDoubleProp(Instance);
end;


{ TOrmPropInfoRTTIMany }

// TOrmMany stores nothing within the table

procedure TOrmPropInfoRTTIMany.GetValueVar(Instance: TObject; ToSQL: boolean;
  var result: RawUTF8; wasSQLString: PBoolean);
begin
  result := '';
end;

procedure TOrmPropInfoRTTIMany.SetValue(Instance: TObject; Value: PUTF8Char;
  wasString: boolean);
begin
end;

procedure TOrmPropInfoRTTIMany.GetBinary(Instance: TObject; W: TBufferWriter);
begin
end;

function TOrmPropInfoRTTIMany.SetBinary(Instance: TObject; P, PEnd: PAnsiChar): PAnsiChar;
begin
  result := P;
end;


{ TOrmPropInfoRTTIInstance }

constructor TOrmPropInfoRTTIInstance.Create(aPropInfo: PRttiProp;
  aPropIndex: integer; aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions);
begin
  inherited Create(aPropInfo, aPropIndex, aOrmFieldType, aOptions);
  fObjectClass := fPropType^.RttiClass^.RttiClass;
end;

function TOrmPropInfoRTTIInstance.GetInstance(Instance: TObject): TObject;
begin
  result := fPropInfo.GetObjProp(Instance);
end;

procedure TOrmPropInfoRTTIInstance.SetInstance(Instance, Value: TObject);
begin
  fPropInfo.SetOrdProp(Instance, PtrInt(Value));
end;


{ TOrmPropInfoRTTIRecordReference }

constructor TOrmPropInfoRTTIRecordReference.Create(aPropInfo: PRttiProp;
  aPropIndex: integer; aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions);
begin
  inherited Create(aPropInfo, aPropIndex, aOrmFieldType, aOptions);
  fCascadeDelete := IdemPropName(fPropType^.Name^, 'TRecordReferenceToBeDeleted')
end;


{ TOrmPropInfoRTTITID }

constructor TOrmPropInfoRTTITID.Create(aPropInfo: PRttiProp; aPropIndex: integer;
  aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions);
var
  TypeName: PShortString;
  Found: TRttiCustom;
begin
  inherited Create(aPropInfo, aPropIndex, aOrmFieldType, aOptions);
  TypeName := fPropType^.Name;
  if IdemPropName(TypeName^, 'TID') or
     (ord(TypeName^[1]) and $df <> ord('T')) or // expect T...ID pattern
     (PWord(@TypeName^[ord(TypeName^[0]) - 1])^ and $dfdf <>
      ord('I') + ord('D') shl 8) or
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


{ TOrmPropInfoRTTIID }

procedure TOrmPropInfoRTTIID.SetValue(Instance: TObject; Value: PUTF8Char;
  wasString: boolean);
begin
  if TOrm(Instance).fFill.JoinedFields then
    raise EModelException.CreateUTF8('%(%).SetValue after Create*Joined', [self, Name]);
  inherited SetValue(Instance, Value, wasString);
end;

procedure TOrmPropInfoRTTIID.GetJSONValues(Instance: TObject; W: TJSONSerializer);
var
  ID: PtrUInt;
begin
  ID := fPropInfo.GetOrdProp(Instance);
  if TOrm(Instance).fFill.JoinedFields then
    ID := TOrm(ID).fID;
  W.AddU(ID);
end;


{ TOrmPropInfoRTTIIObject }

procedure TOrmPropInfoRTTIObject.CopySameClassProp(Source: TObject;
  DestInfo: TOrmPropInfo; Dest: TObject);
var
  S, D: TObject;
begin
  // generic case: copy also class content (create instances)
  S := GetInstance(Source);
  D := TOrmPropInfoRTTIObject(DestInfo).GetInstance(Dest);
  if fPropRtti.ValueKnownClass = TCollection then
    CopyCollection(TCollection(S), TCollection(D))
  else if fPropRtti.ValueKnownClass = TStrings then
    CopyStrings(TStrings(S), TStrings(D))
  else
  begin
    D.Free; // release previous instance
    TOrmPropInfoRTTIObject(DestInfo).SetInstance(Dest, CopyObject(S));
  end;
end;

procedure TOrmPropInfoRTTIObject.SetValue(Instance: TObject; Value: PUTF8Char;
  wasString: boolean);
var
  valid: boolean;
  tmp: TSynTempBuffer;
begin
  tmp.Init(Value); // private copy since the buffer will be modified
  try
    PropertyFromJSON(fPropRttiProp, Instance, tmp.buf, valid,
      JSONPARSER_TOLERANTOPTIONS);
  finally
    tmp.Done;
  end;
end;

procedure TOrmPropInfoRTTIObject.GetValueVar(Instance: TObject; ToSQL: boolean;
  var result: RawUTF8; wasSQLString: PBoolean);
begin
  if wasSQLString <> nil then
    wasSQLString^ := true;
  result := ObjectToJSON(GetInstance(Instance));
end;

procedure TOrmPropInfoRTTIObject.GetBinary(Instance: TObject; W: TBufferWriter);
begin
  // serialize object as JSON UTF-8 TEXT - not fast, but works
  W.Write(ObjectToJSON(GetInstance(Instance)));
end;

function TOrmPropInfoRTTIObject.SetBinary(Instance: TObject;
  P, PEnd: PAnsiChar): PAnsiChar;
var
  valid: boolean;
  tmp: TSynTempBuffer;
begin
  // unserialize object from JSON UTF-8 TEXT - not fast, but works
  FromVarString(PByte(P), PByte(PEnd), tmp);
  try
    PropertyFromJSON(fPropRttiProp, Instance, tmp.buf, valid,
      JSONPARSER_TOLERANTOPTIONS);
  finally
    tmp.Done;
  end;
  if valid then
    result := P
  else
    result := nil;
end;

function TOrmPropInfoRTTIObject.GetHash(Instance: TObject;
  CaseInsensitive: boolean): cardinal;
var
  tmp: RawUTF8;
begin // JSON is case-sensitive by design -> ignore CaseInsensitive parameter
  tmp := ObjectToJSON(GetInstance(Instance));
  result := crc32c(0, pointer(tmp), length(tmp));
end;

procedure TOrmPropInfoRTTIObject.NormalizeValue(var Value: RawUTF8);
begin // do nothing: should already be normalized
end;

procedure TOrmPropInfoRTTIObject.GetJSONValues(Instance: TObject; W: TJSONSerializer);
begin
  if jwoAsJsonNotAsString in W.fOrmOptions then
    W.WriteObject(GetInstance(Instance))
  else
    W.WriteObjectAsString(GetInstance(Instance));
end;


{ TOrmPropInfoRTTIAnsi }

constructor TOrmPropInfoRTTIAnsi.Create(aPropInfo: PRttiProp;
  aPropIndex: integer; aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions);
begin
  inherited;
  fEngine := TSynAnsiConvert.Engine(aPropInfo^.typeInfo^.AnsiStringCodePage);
end;

procedure TOrmPropInfoRTTIAnsi.CopySameClassProp(Source: TObject;
  DestInfo: TOrmPropInfo; Dest: TObject);
var
  Value: RawByteString;
begin
  if TOrmPropInfoRTTIAnsi(DestInfo).fEngine = fEngine then
  begin
    fPropInfo.GetLongStrProp(Source, Value);
    TOrmPropInfoRTTIAnsi(DestInfo).fPropInfo.SetLongStrProp(Dest, Value);
  end
  else
  begin
    GetValueVar(Source, false, RawUTF8(Value), nil);
    DestInfo.SetValueVar(Dest, Value, true);
  end;
end;

procedure TOrmPropInfoRTTIAnsi.GetBinary(Instance: TObject; W: TBufferWriter);
var
  Value: RawByteString;
begin
  fPropInfo.GetLongStrProp(Instance, Value);
  W.Write(Value);
end;

function TOrmPropInfoRTTIAnsi.GetHash(Instance: TObject;
  CaseInsensitive: boolean): cardinal;
var
  Up: array[byte] of AnsiChar; // avoid slow heap allocation
  Value: RawByteString;
begin
  fPropInfo.GetLongStrProp(Instance, Value);
  if CaseInsensitive then
    if fEngine.CodePage = CODEPAGE_US then
      result := crc32c(0, Up{%H-}, UpperCopyWin255(Up{%H-}, Value) - {%H-}Up)
    else
      result := crc32c(0, Up, UpperCopy255Buf(Up, pointer(Value), length(Value)) - Up)
  else
    result := crc32c(0, pointer(Value), length(Value));
end;

procedure TOrmPropInfoRTTIAnsi.GetValueVar(Instance: TObject; ToSQL: boolean;
  var result: RawUTF8; wasSQLString: PBoolean);
var
  tmp: RawByteString;
begin
  if wasSQLString <> nil then
    wasSQLString^ := true;
  fPropInfo.GetLongStrProp(Instance, tmp);
  result := fEngine.AnsiBufferToRawUTF8(pointer(tmp), length(tmp));
end;

procedure TOrmPropInfoRTTIAnsi.NormalizeValue(var Value: RawUTF8);
begin // do nothing: should already be UTF-8 encoded
end;

function TOrmPropInfoRTTIAnsi.CompareValue(Item1, Item2: TObject;
  CaseInsensitive: boolean): PtrInt;
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

function TOrmPropInfoRTTIAnsi.SetBinary(Instance: TObject; P, PEnd: PAnsiChar): PAnsiChar;
var
  tmp: RawByteString;
begin
  FromVarString(PByte(P), PByte(PEnd), tmp, fEngine.CodePage);
  fPropInfo.SetLongStrProp(Instance, tmp);
  result := P;
end;

procedure TOrmPropInfoRTTIAnsi.SetValue(Instance: TObject; Value: PUTF8Char;
  wasString: boolean);
begin
  if Value = nil then
    fPropInfo.SetLongStrProp(Instance, '')
  else
    fPropInfo.SetLongStrProp(Instance, fEngine.UTF8BufferToAnsi(Value, StrLen(Value)));
end;

procedure TOrmPropInfoRTTIAnsi.SetValueVar(Instance: TObject; const Value:
  RawUTF8; wasString: boolean);
begin
  fPropInfo.SetLongStrProp(Instance, fEngine.UTF8ToAnsi(Value));
end;

procedure TOrmPropInfoRTTIAnsi.GetJSONValues(Instance: TObject; W: TJSONSerializer);
var
  tmp: RawByteString;
begin
  W.Add('"');
  fPropInfo.GetLongStrProp(Instance, tmp);
  if tmp <> '' then
    W.AddAnyAnsiString(tmp, twJSONEscape, fEngine.CodePage);
  W.Add('"');
end;

function TOrmPropInfoRTTIAnsi.SetFieldSQLVar(Instance: TObject;
  const aValue: TSQLVar): boolean;
var
  tmp: RawByteString;
begin
  case aValue.VType of
    ftNull:
      ; // leave tmp=''
    ftUTF8:
      fEngine.UTF8BufferToAnsi(aValue.VText, StrLen(aValue.VText), tmp);
  else
    begin
      result := inherited SetFieldSQLVar(Instance, aValue);
      exit;
    end;
  end;
  fPropInfo.SetLongStrProp(Instance, tmp{%H-});
  result := True;
end;

procedure TOrmPropInfoRTTIAnsi.GetFieldSQLVar(Instance: TObject;
  var aValue: TSQLVar; var temp: RawByteString);
begin
  fPropInfo.GetLongStrProp(Instance, temp);
  temp := fEngine.AnsiToUTF8(temp);
  aValue.Options := [];
  aValue.VType := ftUTF8;
  aValue.VText := pointer(temp);
end;

procedure TOrmPropInfoRTTIAnsi.CopyValue(Source, Dest: TObject);
begin // avoid temporary variable use, for simple fields with no getter/setter
  if fInPlaceCopySameClassPropOffset = 0 then
    fPropInfo.CopyLongStrProp(Source, Dest)
  else
    PRawByteString(PtrUInt(Dest) + fInPlaceCopySameClassPropOffset)^ :=
      PRawByteString(PtrUInt(Source) + fInPlaceCopySameClassPropOffset)^;
end;


{ TOrmPropInfoRTTIRawUTF8 }

procedure TOrmPropInfoRTTIRawUTF8.CopySameClassProp(Source: TObject;
  DestInfo: TOrmPropInfo; Dest: TObject);
var
  Value: RawByteString;
begin // don't know why, but fInPlaceCopySameClassPropOffset trick leaks memory :(
  fPropInfo.GetLongStrProp(Source, Value);
  TOrmPropInfoRTTIRawUTF8(DestInfo).fPropInfo.SetLongStrProp(Dest, Value);
end;

function TOrmPropInfoRTTIRawUTF8.GetHash(Instance: TObject;
  CaseInsensitive: boolean): cardinal;
var
  Up: array[byte] of AnsiChar; // avoid slow heap allocation
  Value: RawByteString;
begin
  fPropInfo.GetLongStrProp(Instance, Value);
  if CaseInsensitive then
    result := crc32c(0, Up{%H-}, UTF8UpperCopy255(Up{%H-}, Value) - {%H-}Up)
  else
    result := crc32c(0, pointer(Value), length(Value));
end;

procedure TOrmPropInfoRTTIRawUTF8.GetJSONValues(Instance: TObject; W: TJSONSerializer);
var
  tmp: RawByteString;
begin
  fPropInfo.GetLongStrProp(Instance, tmp);
  if fPropType = TypeInfo(RawJSON) then
    W.AddRawJSON(tmp)
  else
  begin
    W.Add('"');
    if tmp <> '' then
      W.AddJSONEscape(pointer(tmp));
    W.Add('"');
  end;
end;

procedure TOrmPropInfoRTTIRawUTF8.GetValueVar(Instance: TObject; ToSQL: boolean;
  var result: RawUTF8; wasSQLString: PBoolean);
begin
  if wasSQLString <> nil then
    wasSQLString^ := fPropType <> TypeInfo(RawJSON);
  fPropInfo.GetLongStrProp(Instance, RawByteString(result));
end;

function TOrmPropInfoRTTIRawUTF8.SetBinary(Instance: TObject; P, PEnd: PAnsiChar):
  PAnsiChar;
begin
  fPropInfo.SetLongStrProp(Instance, FromVarString(PByte(P), PByte(PEnd)));
  result := P;
end;

function TOrmPropInfoRTTIRawUTF8.SetFieldSQLVar(Instance: TObject;
  const aValue: TSQLVar): boolean;
var
  tmp: RawByteString;
begin
  case aValue.VType of
    ftNull:
      ; // leave tmp=''
    ftUTF8:
      SetString(tmp, PAnsiChar(aValue.VText), StrLen(aValue.VText));
  else
    begin
      result := inherited SetFieldSQLVar(Instance, aValue);
      exit;
    end;
  end;
  fPropInfo.SetLongStrProp(Instance, tmp{%H-});
  result := True;
end;

procedure TOrmPropInfoRTTIRawUTF8.GetFieldSQLVar(Instance: TObject;
  var aValue: TSQLVar; var temp: RawByteString);
begin
  fPropInfo.GetLongStrProp(Instance, temp);
  aValue.Options := [];
  aValue.VType := ftUTF8;
  aValue.VText := Pointer(temp);
end;

function TOrmPropInfoRTTIRawUTF8.CompareValue(Item1, Item2: TObject;
  CaseInsensitive: boolean): PtrInt;

  function CompareWithLocalTempCopy: PtrInt;
  var
    tmp1, tmp2: RawByteString;
  begin
    fPropInfo.GetLongStrProp(Item1, tmp1);
    fPropInfo.GetLongStrProp(Item2, tmp2);
    if CaseInsensitive then
      result := UTF8IComp(pointer(tmp1), pointer(tmp2))
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
    begin // avoid any temporary variable
      p1 := PPointer(PtrUInt(Item1) + offs)^;
      p2 := PPointer(PtrUInt(Item2) + offs)^;
      if CaseInsensitive then
        result := UTF8IComp(p1, p2)
      else
        result := StrComp(p1, p2);
    end
    else
      result := CompareWithLocalTempCopy;
  end;
end;

procedure TOrmPropInfoRTTIRawUTF8.SetValue(Instance: TObject; Value: PUTF8Char;
  wasString: boolean);
var
  tmp: RawUTF8;
begin
  if Value <> nil then
    FastSetString(tmp, Value, StrLen(Value));
  fPropInfo.SetLongStrProp(Instance, tmp);
end;

procedure TOrmPropInfoRTTIRawUTF8.SetValueVar(Instance: TObject;
  const Value: RawUTF8; wasString: boolean);
begin
  fPropInfo.SetLongStrProp(Instance, Value);
end;


{ TOrmPropInfoRTTIRawUnicode }

procedure TOrmPropInfoRTTIRawUnicode.CopySameClassProp(Source: TObject;
  DestInfo: TOrmPropInfo; Dest: TObject);
var
  Value: RawByteString;
begin
  fPropInfo.GetLongStrProp(Source, Value);
  TOrmPropInfoRTTIRawUnicode(DestInfo).fPropInfo.SetLongStrProp(Dest, Value);
end;

function TOrmPropInfoRTTIRawUnicode.GetHash(Instance: TObject;
  CaseInsensitive: boolean): cardinal;
var
  Up: array[byte] of AnsiChar; // avoid slow heap allocation
  Value: RawByteString;
begin
  fPropInfo.GetLongStrProp(Instance, Value);
  if CaseInsensitive then
    result := crc32c(0, Up{%H-},
      UpperCopy255W(Up{%H-}, pointer(Value), length(Value) shr 1) - {%H-}Up)
  else
    result := crc32c(0, pointer(Value), length(Value));
end;

procedure TOrmPropInfoRTTIRawUnicode.GetValueVar(Instance: TObject;
  ToSQL: boolean; var result: RawUTF8; wasSQLString: PBoolean);
var
  tmp: RawByteString;
begin
  if wasSQLString <> nil then
    wasSQLString^ := true;
  fPropInfo.GetLongStrProp(Instance, tmp);
  RawUnicodeToUTF8(pointer(tmp), length(tmp) shr 1, result);
end;

function TOrmPropInfoRTTIRawUnicode.CompareValue(Item1, Item2: TObject;
  CaseInsensitive: boolean): PtrInt;
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

procedure TOrmPropInfoRTTIRawUnicode.SetValue(Instance: TObject;
  Value: PUTF8Char; wasString: boolean);
begin
  if Value = nil then
    fPropInfo.SetLongStrProp(Instance, '')
  else
    fPropInfo.SetLongStrProp(Instance,
      Utf8DecodeToRawUnicode(Value, StrLen(Value)));
end;

procedure TOrmPropInfoRTTIRawUnicode.SetValueVar(Instance: TObject;
  const Value: RawUTF8; wasString: boolean);
begin
  fPropInfo.SetLongStrProp(Instance, Utf8DecodeToRawUnicode(Value));
end;


{ TOrmPropInfoRTTIRawBlob }

procedure TOrmPropInfoRTTIRawBlob.CopySameClassProp(Source: TObject;
  DestInfo: TOrmPropInfo; Dest: TObject);
var
  Value: RawByteString;
begin
  fPropInfo.GetLongStrProp(Source, Value);
  TOrmPropInfoRTTIRawBlob(DestInfo).fPropInfo.SetLongStrProp(Dest, Value);
end;

function TOrmPropInfoRTTIRawBlob.GetHash(Instance: TObject;
  CaseInsensitive: boolean): cardinal;
var
  Value: RawByteString;
begin
  fPropInfo.GetLongStrProp(Instance, Value);
  result := crc32c(0, pointer(Value), length(Value)); // binary -> case sensitive
end;

procedure TOrmPropInfoRTTIRawBlob.GetJSONValues(Instance: TObject; W: TJSONSerializer);
var
  tmp: RawByteString;
begin
  fPropInfo.GetLongStrProp(Instance, tmp);
  W.WrBase64(pointer(tmp), length(tmp), true);
end;

procedure TOrmPropInfoRTTIRawBlob.GetBlob(Instance: TObject; var Blob: RawByteString);
begin
  fPropInfo.GetLongStrProp(Instance, Blob);
end;

procedure TOrmPropInfoRTTIRawBlob.SetBlob(Instance: TObject; const Blob: RawByteString);
begin
  fPropInfo.SetLongStrProp(Instance, Blob);
end;

function TOrmPropInfoRTTIRawBlob.IsNull(Instance: TObject): boolean;
var
  Blob: RawByteString;
begin
  fPropInfo.GetLongStrProp(Instance, Blob);
  result := (Blob = '');
end;

procedure TOrmPropInfoRTTIRawBlob.GetValueVar(Instance: TObject; ToSQL: boolean;
  var result: RawUTF8; wasSQLString: PBoolean);
begin
  fPropInfo.GetLongStrProp(Instance, RawByteString(result));
  BinaryToText(result, ToSQL, wasSQLString);
end;

function TOrmPropInfoRTTIRawBlob.CompareValue(Item1, Item2: TObject;
  CaseInsensitive: boolean): PtrInt;
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
    result := StrComp(pointer(tmp1), pointer(tmp2));
  end;
end;

procedure TOrmPropInfoRTTIRawBlob.SetValue(Instance: TObject; Value: PUTF8Char;
  wasString: boolean);
begin
  fPropInfo.SetLongStrProp(Instance, BlobToRawBlob(Value));
end;

procedure TOrmPropInfoRTTIRawBlob.SetValueVar(Instance: TObject;
  const Value: RawUTF8; wasString: boolean);
begin
  fPropInfo.SetLongStrProp(Instance, BlobToRawBlob(Value));
end;

function TOrmPropInfoRTTIRawBlob.SetFieldSQLVar(Instance: TObject;
  const aValue: TSQLVar): boolean;
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
    result := inherited SetFieldSQLVar(Instance, aValue);
  end;
end;

procedure TOrmPropInfoRTTIRawBlob.GetFieldSQLVar(Instance: TObject;
  var aValue: TSQLVar; var temp: RawByteString);
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


{ TOrmPropInfoRTTIWide }

procedure TOrmPropInfoRTTIWide.CopySameClassProp(Source: TObject;
  DestInfo: TOrmPropInfo; Dest: TObject);
var
  Value: WideString;
begin
  fPropInfo.GetWideStrProp(Source, Value);
  TOrmPropInfoRTTIWide(DestInfo).fPropInfo.SetWideStrProp(Dest, Value);
end;

procedure TOrmPropInfoRTTIWide.GetBinary(Instance: TObject; W: TBufferWriter);
var
  Value: WideString;
begin
  fPropInfo.GetWideStrProp(Instance, Value);
  W.Write(WideStringToUTF8(Value));
end;

function TOrmPropInfoRTTIWide.GetHash(Instance: TObject;
  CaseInsensitive: boolean): cardinal;
var
  Up: array[byte] of AnsiChar; // avoid slow heap allocation
  Value: WideString;
begin
  fPropInfo.GetWideStrProp(Instance, Value);
  if CaseInsensitive then
    result := crc32c(0, Up{%H-},
      UpperCopy255W(Up{%H-}, pointer(Value), length(Value)) - {%H-}Up)
  else
    result := crc32c(0, pointer(Value), length(Value) * 2);
end;

procedure TOrmPropInfoRTTIWide.GetJSONValues(Instance: TObject; W: TJSONSerializer);
var
  Value: WideString;
begin
  W.Add('"');
  fPropInfo.GetWideStrProp(Instance, Value);
  if pointer(Value) <> nil then
    W.AddJSONEscapeW(pointer(Value), 0);
  W.Add('"');
end;

procedure TOrmPropInfoRTTIWide.GetValueVar(Instance: TObject; ToSQL: boolean;
  var result: RawUTF8; wasSQLString: PBoolean);
var
  Value: WideString;
begin
  fPropInfo.GetWideStrProp(Instance, Value);
  result := WideStringToUTF8(Value);
  if wasSQLString <> nil then
    wasSQLString^ := true;
end;

procedure TOrmPropInfoRTTIWide.CopyValue(Source, Dest: TObject);
begin // avoid temporary variable use, for simple fields with no getter/setter
  if fInPlaceCopySameClassPropOffset = 0 then
    CopySameClassProp(Source, self, Dest)
  else
    PWideString(PtrUInt(Dest) + fInPlaceCopySameClassPropOffset)^ :=
      PWideString(PtrUInt(Source) + fInPlaceCopySameClassPropOffset)^;
end;

function TOrmPropInfoRTTIWide.CompareValue(Item1, Item2: TObject;
  CaseInsensitive: boolean): PtrInt;
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

function TOrmPropInfoRTTIWide.SetBinary(Instance: TObject; P, PEnd: PAnsiChar): PAnsiChar;
begin
  fPropInfo.SetWideStrProp(Instance,
    UTF8ToWideString(FromVarString(PByte(P), pointer(PEnd))));
  result := P;
end;

procedure TOrmPropInfoRTTIWide.SetValue(Instance: TObject; Value: PUTF8Char;
  wasString: boolean);
var
  Wide: WideString;
begin
  if Value <> nil then
    UTF8ToWideString(Value, StrLen(Value), Wide);
  fPropInfo.SetWideStrProp(Instance, Wide);
end;


{$ifdef HASVARUSTRING}

{ TOrmPropInfoRTTIUnicode }

procedure TOrmPropInfoRTTIUnicode.CopySameClassProp(Source: TObject;
  DestInfo: TOrmPropInfo; Dest: TObject);
var
  tmp: UnicodeString;
begin
  fPropInfo.GetUnicodeStrProp(Source, tmp);
  TOrmPropInfoRTTIUnicode(DestInfo).fPropInfo.SetUnicodeStrProp(Dest, tmp);
end;

procedure TOrmPropInfoRTTIUnicode.GetBinary(Instance: TObject; W: TBufferWriter);
var
  tmp: UnicodeString;
begin
  fPropInfo.GetUnicodeStrProp(Instance, tmp);
  W.Write(UnicodeStringToUtf8(tmp));
end;

procedure TOrmPropInfoRTTIUnicode.CopyValue(Source, Dest: TObject);
begin // avoid temporary variable use, for simple fields with no getter/setter
  if fInPlaceCopySameClassPropOffset = 0 then
    CopySameClassProp(Source, self, Dest)
  else
    PUnicodeString(PtrUInt(Dest) + fInPlaceCopySameClassPropOffset)^ :=
      PUnicodeString(PtrUInt(Source) + fInPlaceCopySameClassPropOffset)^;
end;

function TOrmPropInfoRTTIUnicode.GetHash(Instance: TObject;
  CaseInsensitive: boolean): cardinal;
var
  Up: array[byte] of AnsiChar; // avoid slow heap allocation
  Value: UnicodeString;
begin
  fPropInfo.GetUnicodeStrProp(Instance, Value);
  if CaseInsensitive then
    result := crc32c(0, Up{%H-},
      UpperCopy255W(Up{%H-}, pointer(Value), length(Value)) - {%H-}Up)
  else
    result := crc32c(0, pointer(Value), length(Value) * 2);
end;

procedure TOrmPropInfoRTTIUnicode.GetValueVar(Instance: TObject; ToSQL: boolean;
  var result: RawUTF8; wasSQLString: PBoolean);
var
  tmp: UnicodeString;
begin
  fPropInfo.GetUnicodeStrProp(Instance, tmp);
  RawUnicodeToUtf8(pointer(tmp), length(tmp), result);
  if wasSQLString <> nil then
    wasSQLString^ := true;
end;

procedure TOrmPropInfoRTTIUnicode.GetJSONValues(Instance: TObject; W: TJSONSerializer);
var
  tmp: UnicodeString;
begin
  W.Add('"');
  fPropInfo.GetUnicodeStrProp(Instance, tmp);
  if tmp <> '' then
    W.AddJSONEscapeW(pointer(tmp), 0);
  W.Add('"');
end;

function TOrmPropInfoRTTIUnicode.CompareValue(Item1, Item2: TObject;
  CaseInsensitive: boolean): PtrInt;
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

function TOrmPropInfoRTTIUnicode.SetBinary(Instance: TObject; P, PEnd: PAnsiChar):
  PAnsiChar;
begin
  fPropInfo.SetUnicodeStrProp(Instance,
    UTF8DecodeToUnicodeString(FromVarString(PByte(P), pointer(PEnd))));
  result := P;
end;

procedure TOrmPropInfoRTTIUnicode.SetValue(Instance: TObject; Value: PUTF8Char;
  wasString: boolean);
var
  tmp: UnicodeString;
begin
  if Value <> nil then
    UTF8DecodeToUnicodeString(Value, StrLen(Value), tmp);
  fPropInfo.SetUnicodeStrProp(Instance, tmp);
end;

procedure TOrmPropInfoRTTIUnicode.SetValueVar(Instance: TObject;
  const Value: RawUTF8; wasString: boolean);
begin
  fPropInfo.SetUnicodeStrProp(Instance, UTF8DecodeToUnicodeString(Value));
end;

function TOrmPropInfoRTTIUnicode.SetFieldSQLVar(Instance: TObject;
  const aValue: TSQLVar): boolean;
var
  tmp: UnicodeString;
begin
  case aValue.VType of
    ftNull:
      ; // leave tmp=''
    ftUTF8:
      UTF8DecodeToUnicodeString(aValue.VText, StrLen(aValue.VText), tmp);
  else
    begin
      result := inherited SetFieldSQLVar(Instance, aValue);
      exit;
    end;
  end;
  fPropInfo.SetUnicodeStrProp(Instance, tmp{%H-});
  result := True;
end;

procedure TOrmPropInfoRTTIUnicode.GetFieldSQLVar(Instance: TObject;
  var aValue: TSQLVar; var temp: RawByteString);
var
  tmp: UnicodeString;
begin
  fPropInfo.GetUnicodeStrProp(Instance, tmp);
  RawUnicodeToUtf8(pointer(tmp), length(tmp), RawUTF8(temp));
  aValue.Options := [];
  aValue.VType := ftUTF8;
  aValue.VText := Pointer(temp);
end;

{$endif HASVARUSTRING}


{ TOrmPropInfoRTTIDynArray }

constructor TOrmPropInfoRTTIDynArray.Create(aPropInfo: PRttiProp;
  aPropIndex: integer; aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions);
begin
  inherited Create(aPropInfo, aPropIndex, aOrmFieldType, aOptions);
  if rcfObjArray in fPropRtti.Flags then
  begin
    fObjArray := fPropRtti.ArrayRtti as TRttiJson;
    fSQLDBFieldType := ftUTF8; // matches GetFieldSQLVar() below
  end;
  if fGetterIsFieldPropOffset = 0 then
    raise EModelException.CreateUTF8('%.Create(%) should be a field, not with getter!',
      [self, fPropType^.Name]);
end;

procedure TOrmPropInfoRTTIDynArray.GetDynArray(Instance: TObject; var result: TDynArray);
begin
  // very fast assignment of fWrapper pre-initialized RTTI
  result.InitRtti(fPropRtti, pointer(PtrUInt(Instance) + fGetterIsFieldPropOffset)^);
end;

function TOrmPropInfoRTTIDynArray.GetDynArrayElemType: TRttiCustom;
begin
  result := fPropRtti.ArrayRtti;
end;

procedure TOrmPropInfoRTTIDynArray.Serialize(Instance: TObject;
  var data: RawByteString; ExtendedJson: boolean);
var
  da: TDynArray;
  temp: TTextWriterStackBuffer;
begin
  GetDynArray(Instance, da);
  if da.Count = 0 then
    data := ''
  else if fObjArray <> nil then
    with TJSONSerializer.CreateOwnedStream(temp) do
    try
      if ExtendedJson then
        include(fCustomOptions, twoForceJSONExtended); // smaller content
      AddDynArrayJSON(da);
      SetText(RawUTF8(data));
    finally
      Free;
    end
  else
    data := da.SaveTo;
end;

procedure TOrmPropInfoRTTIDynArray.CopySameClassProp(Source: TObject;
  DestInfo: TOrmPropInfo; Dest: TObject);
var
  sda, dda: TDynArray;
begin
  GetDynArray(Source, sda);
  TOrmPropInfoRTTIDynArray(DestInfo).GetDynArray(Dest, dda);
  if (fObjArray <> nil) or (TOrmPropInfoRTTIDynArray(DestInfo).fObjArray <> nil)
    or (sda.Info.ArrayRtti <> dda.Info.ArrayRtti) then
    dda.LoadFromJSON(pointer(sda.SaveToJSON))
  else
    dda.Copy(@sda);
end;

procedure TOrmPropInfoRTTIDynArray.GetBinary(Instance: TObject; W: TBufferWriter);
var
  Value: RawByteString;
begin
  Serialize(Instance, Value, true);
  if fObjArray <> nil then
    W.Write(Value)
  else
    W.WriteBinary(Value);
end;

function TOrmPropInfoRTTIDynArray.GetHash(Instance: TObject;
  CaseInsensitive: boolean): cardinal;
var
  tmp: RawByteString;
begin
  Serialize(Instance, tmp, true);
  result := crc32c(0, pointer(tmp), length(tmp));
end;

procedure TOrmPropInfoRTTIDynArray.GetValueVar(Instance: TObject; ToSQL: boolean;
  var result: RawUTF8; wasSQLString: PBoolean);
begin
  Serialize(Instance, RawByteString(result), false);
  if fObjArray = nil then
    BinaryToText(result, ToSQL, wasSQLString);
end;

procedure TOrmPropInfoRTTIDynArray.GetVariant(Instance: TObject; var Dest: Variant);
var
  json: RawUTF8;
  da: TDynArray;
begin
  GetDynArray(Instance, da);
  json := da.SaveToJSON;
  VarClear(Dest);
  TDocVariantData(Dest).InitJSONInPlace(pointer(json), JSON_OPTIONS_FAST);
end;

procedure TOrmPropInfoRTTIDynArray.SetVariant(Instance: TObject; const Source: Variant);
var
  json: RawUTF8;
  da: TDynArray;
begin
  GetDynArray(Instance, da);
  VariantSaveJSON(Source, twJSONEscape, json);
  da.LoadFromJSON(pointer(json));
end;

procedure TOrmPropInfoRTTIDynArray.NormalizeValue(var Value: RawUTF8);
begin // do nothing: should already be normalized
end;

function TOrmPropInfoRTTIDynArray.CompareValue(Item1, Item2: TObject;
  CaseInsensitive: boolean): PtrInt;
var
  da1, da2: TDynArray;
  i: integer;
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
    result := da1.Count - da2.Count;
    if result <> 0 then
      exit;
    result := PtrInt(Item1) - PtrInt(Item2); // pseudo comparison
    if fObjArray <> nil then
    begin
      for i := 0 to da1.Count - 1 do
      begin
        result := ObjectCompare(PObjectArray(da1.Value^)[i],
          PObjectArray(da2.Value^)[i], CaseInsensitive);
        if result <> 0 then
          exit;
      end;
    end
    else if not da1.Equals(@da2) then
      exit;
    result := 0;
  end;
end;

function TOrmPropInfoRTTIDynArray.SetBinary(Instance: TObject; P, PEnd:
  PAnsiChar): PAnsiChar;
var
  tmp: TSynTempBuffer; // LoadFromJSON() may change the input buffer
  da: TDynArray;
begin
  GetDynArray(Instance, da);
  if fObjArray <> nil then
  begin
    FromVarString(PByte(P), PByte(PEnd), tmp);
    try // T*ObjArray use JSON serialization
      da.LoadFromJSON(tmp.buf);
    finally
      tmp.Done;
    end;
    result := P;
  end
  else    // regular dynamic arrays use our binary encoding
    result := da.LoadFrom(P, PEnd);
end;

procedure TOrmPropInfoRTTIDynArray.SetValue(Instance: TObject; Value: PUTF8Char;
  wasString: boolean);
var
  tmp: TSynTempBuffer;
  da: TDynArray;
begin
  GetDynArray(Instance, da);
  if Value = nil then
    da.Clear
  else
  try
    if (fObjArray = nil) and Base64MagicCheckAndDecode(Value, tmp) then
      da.LoadFrom(tmp.buf, PAnsiChar(tmp.buf) + tmp.len)
    else
      da.LoadFromJSON(tmp.Init(Value));
  finally
    tmp.Done;
  end;
end;

function TOrmPropInfoRTTIDynArray.SetFieldSQLVar(Instance: TObject;
  const aValue: TSQLVar): boolean;
var
  da: TDynArray;
begin
  if aValue.VType = ftBlob then
  begin
    GetDynArray(Instance, da);
    result := da.LoadFrom(aValue.VBlob, PAnsiChar(aValue.VBlob) + aValue.VBlobLen) <> nil;
  end
  else
    result := inherited SetFieldSQLVar(Instance, aValue);
end;

procedure TOrmPropInfoRTTIDynArray.GetJSONValues(Instance: TObject; W: TJSONSerializer);
var
  tmp: RawByteString;
begin
  if jwoAsJsonNotAsString in W.fOrmOptions then
    W.AddDynArrayJSON(GetFieldAddr(Instance), fPropRtti)
  else if fObjArray <> nil then
    W.AddDynArrayJSONAsString(fPropType, GetFieldAddr(Instance)^)
  else
  begin
    Serialize(Instance, tmp, false);
    W.WrBase64(pointer(tmp), Length(tmp), true); // withMagic=true -> add ""
  end;
end;

procedure TOrmPropInfoRTTIDynArray.GetFieldSQLVar(Instance: TObject;
  var aValue: TSQLVar; var temp: RawByteString);
begin
  Serialize(Instance, temp, false);
  aValue.Options := [];
  if temp = '' then
    aValue.VType := ftNull
  else if fObjArray <> nil then
  begin
    aValue.VType := ftUTF8; // JSON
    aValue.VText := pointer(temp);
  end
  else
  begin
    aValue.VType := ftBlob; // binary
    aValue.VBlob := pointer(temp);
    aValue.VBlobLen := length(temp);
  end;
end;


{ TOrmPropInfoRTTIVariant }

constructor TOrmPropInfoRTTIVariant.Create(aPropInfo: PRttiProp;
  aPropIndex: integer; aOrmFieldType: TOrmFieldType; aOptions: TOrmPropInfoListOptions);
begin
  inherited;
  if aOrmFieldType = oftVariant then
    fDocVariantOptions := JSON_OPTIONS_FAST
  else
    fOrmFieldType := oftNullable; // TNullable* will use fOrmFieldTypeStored
end;

procedure TOrmPropInfoRTTIVariant.CopySameClassProp(Source: TObject;
  DestInfo: TOrmPropInfo; Dest: TObject);
var
  value: Variant;
begin
  fPropInfo.GetVariantProp(Source, value, {byref=}true);
  TOrmPropInfoRTTIVariant(DestInfo).fPropInfo.SetVariantProp(Dest, value);
end;

procedure TOrmPropInfoRTTIVariant.GetBinary(Instance: TObject; W: TBufferWriter);
var
  value: Variant;
begin
  fPropInfo.GetVariantProp(Instance, value, {byref=}true);
  BinarySave(@value, fPropType, W);
end;

function TOrmPropInfoRTTIVariant.GetHash(Instance: TObject; CaseInsensitive:
  boolean): cardinal;
var
  value: Variant;
begin
  fPropInfo.GetVariantProp(Instance, value, {byref=}true);
  result := VariantHash(value, CaseInsensitive);
end;

procedure TOrmPropInfoRTTIVariant.GetJSONValues(Instance: TObject; W: TJSONSerializer);
var
  value: Variant;
  backup: TTextWriterOptions;
begin
  fPropInfo.GetVariantProp(Instance, value, {byref=}true);
  backup := W.CustomOptions;
  if jwoAsJsonNotAsString in W.fOrmOptions then
    W.CustomOptions := backup + [twoForceJSONStandard] - [twoForceJSONExtended];
  W.AddVariant(value, twJSONEscape); // even oftNullable should escape strings
  W.CustomOptions := backup;
end;

procedure TOrmPropInfoRTTIVariant.GetValueVar(Instance: TObject; ToSQL: boolean;
  var result: RawUTF8; wasSQLString: PBoolean);
var
  wasString: boolean;
  value: Variant;
begin
  fPropInfo.GetVariantProp(Instance, value, {byref=}true);
  VariantToUTF8(value, result, wasString);
  if wasSQLString <> nil then
    if fOrmFieldType = oftNullable then
      // only TNullableUTF8Text and TNullableDateTime will be actual text
      wasSQLString^ := (fSQLDBFieldType in TEXT_DBFIELDS) and not VarIsEmptyOrNull(value)
    else
      // from SQL point of view, variant columns are TEXT or NULL
      wasSQLString^ := not VarIsEmptyOrNull(value);
end;

procedure TOrmPropInfoRTTIVariant.GetVariant(Instance: TObject; var Dest: Variant);
begin
  fPropInfo.GetVariantProp(Instance, Dest, {byref=}true);
end;

procedure TOrmPropInfoRTTIVariant.NormalizeValue(var Value: RawUTF8);
begin // content should be already normalized
end;

function TOrmPropInfoRTTIVariant.CompareValue(Item1, Item2: TObject;
  CaseInsensitive: boolean): PtrInt;

  function CompareWithLocalTempCopy: PtrInt;
  var
    V1, V2: variant;
  begin
    fPropInfo.GetVariantProp(Item1, V1, {byref=}true);
    fPropInfo.GetVariantProp(Item2, V2, {byref=}true);
    result := SortDynArrayVariantComp(TVarData(V1), TVarData(V2), CaseInsensitive);
  end;

begin
  if Item1 = Item2 then
    result := 0
  else if Item1 = nil then
    result := -1
  else if Item2 = nil then
    result := 1
  else if fGetterIsFieldPropOffset <> 0 then // avoid any temporary variable
    result := SortDynArrayVariantComp(PVarData(PtrUInt(Item1) +
      fGetterIsFieldPropOffset)^, PVarData(PtrUInt(Item2) +
      fGetterIsFieldPropOffset)^, CaseInsensitive)
  else
    result := CompareWithLocalTempCopy;
end;

function TOrmPropInfoRTTIVariant.SetBinary(Instance: TObject;
  P, PEnd: PAnsiChar): PAnsiChar;
var
  value: Variant;
  opt: PDocVariantOptions;
begin
  if fOrmFieldType = oftNullable then
    opt := nil
  else
    opt := @DocVariantOptions;
  // use our binary serialization
  result := BinaryLoad(@value, P, fPropType, nil, PEnd, [rkVariant], opt);
  fPropInfo.SetVariantProp(Instance, value);
end;

procedure TOrmPropInfoRTTIVariant.SetValue(Instance: TObject; Value: PUTF8Char;
  wasString: boolean);
begin
  SetValuePtr(Instance, Value, StrLen(Value), wasString);
end;

procedure TOrmPropInfoRTTIVariant.SetValueVar(Instance: TObject;
  const Value: RawUTF8; wasString: boolean);
begin
  SetValuePtr(Instance, pointer(Value), length(Value), wasString);
end;

procedure TOrmPropInfoRTTIVariant.SetValuePtr(Instance: TObject;
  Value: PUTF8Char; ValueLen: integer; wasString: boolean);
var
  tmp: TSynTempBuffer;
  V: Variant;
begin
  if ValueLen > 0 then
  begin
    tmp.Init(Value, ValueLen);
    try
      if fOrmFieldType = oftNullable then
        if fSQLDBFieldType = ftDate then
        begin // decode as date/time variant
          TVarData(V).VType := varDate;
          TVarData(V).VDate := Iso8601ToDateTimePUTF8Char(Value, ValueLen);
        end
        else
          GetVariantFromJSON(tmp.buf, wasString, V, nil)
      else
      begin
        if wasString and (GotoNextNotSpace(Value)^ in ['{', '[']) then
          wasString := false; // allow to create a TDocVariant stored as DB text
        GetVariantFromJSON(tmp.buf, wasString, V, @DocVariantOptions);
      end;
      fPropInfo.SetVariantProp(Instance, V);
    finally
      tmp.Done;
    end;
  end
  else
  begin
    TVarData(V).VType := varNull; // TEXT or NULL: see GetValueVar()
    fPropInfo.SetVariantProp(Instance, V);
  end;
end;

procedure TOrmPropInfoRTTIVariant.SetVariant(Instance: TObject; const Source: Variant);
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

constructor TOrmPropInfoCustom.Create(const aName: RawUTF8;
  aOrmFieldType: TOrmFieldType; aAttributes: TOrmPropInfoAttributes;
  aFieldWidth, aPropIndex: integer; aProperty: pointer;
  aData2Text: TOnSQLPropInfoRecord2Text; aText2Data: TOnSQLPropInfoRecord2Data);
begin
  inherited Create(aName, aOrmFieldType, aAttributes, aFieldWidth, aPropIndex);
  fOffset := PtrUInt(aProperty);
  if (Assigned(aData2Text) and not Assigned(aText2Data)) or
     (Assigned(aText2Data) and not Assigned(aData2Text)) then
    raise EModelException.CreateUTF8(
      'Invalid %.Create: expecting both Data2Text/Text2Data', [self]);
  fData2Text := aData2Text;
  fText2Data := aText2Data;
end;

procedure TOrmPropInfoCustom.TextToBinary(Value: PUTF8Char; var result: RawByteString);
begin
  if Assigned(fText2Data) then
    fText2Data(Value, result)
  else
    result := BlobToRawBlob(Value);
end;

procedure TOrmPropInfoCustom.BinaryToText(var Value: RawUTF8; ToSQL: boolean;
  wasSQLString: PBoolean);
begin
  if Assigned(fData2Text) then
    fData2Text(UniqueRawUTF8(Value), length(Value), Value)
  else
    inherited BinaryToText(Value, ToSQL, wasSQLString);
end;


{ TOrmPropInfoRecordRTTI }

procedure TOrmPropInfoRecordRTTI.CopySameClassProp(Source: TObject;
  DestInfo: TOrmPropInfo; Dest: TObject);
begin
  if TOrmPropInfoRecordRTTI(DestInfo).fTypeInfo = fTypeInfo then
    RecordCopy(TOrmPropInfoRecordRTTI(DestInfo).GetFieldAddr(Dest)^,
      GetFieldAddr(Source)^, fTypeInfo)
  else
    inherited CopySameClassProp(Source, DestInfo, Dest);
end;

function TOrmPropInfoRecordRTTI.GetSQLFieldRTTITypeName: RawUTF8;
begin
  if fTypeInfo = nil then
    result := inherited GetSQLFieldRTTITypeName
  else
    result := ToUTF8(fTypeInfo^.Name^);
end;

constructor TOrmPropInfoRecordRTTI.Create(aRecordInfo: PRttiInfo;
  const aName: RawUTF8; aPropertyIndex: integer; aPropertyPointer: pointer;
  aAttributes: TOrmPropInfoAttributes; aFieldWidth: integer; aData2Text:
  TOnSQLPropInfoRecord2Text; aText2Data: TOnSQLPropInfoRecord2Data);
begin
  if (aRecordInfo = nil) or
     not (aRecordInfo^.Kind in rkRecordTypes) then
    raise EModelException.CreateUTF8(
      '%.Create: Invalid type information for %', [self, aName]);
  inherited Create(aName, oftBlobCustom, aAttributes, aFieldWidth,
    aPropertyIndex, aPropertyPointer, aData2Text, aText2Data);
  fTypeInfo := aRecordInfo;
end;

procedure TOrmPropInfoRecordRTTI.GetBinary(Instance: TObject; W: TBufferWriter);
var
  Value: RawByteString;
begin
  Value := RecordSave(GetFieldAddr(Instance)^, fTypeInfo);
  W.Write(pointer(Value), length(Value));
end;

function TOrmPropInfoRecordRTTI.GetHash(Instance: TObject;
  CaseInsensitive: boolean): cardinal;
var
  tmp: TSynTempBuffer;
begin
  RecordSave(GetFieldAddr(Instance)^, tmp, fTypeInfo);
  result := crc32c(0, tmp.buf, tmp.len);
  tmp.Done;
end;

procedure TOrmPropInfoRecordRTTI.GetVariant(Instance: TObject; var Dest: Variant);
begin
  RawByteStringToVariant(RecordSave(GetFieldAddr(Instance)^, fTypeInfo), Dest);
end;

procedure TOrmPropInfoRecordRTTI.SetVariant(Instance: TObject; const Source: Variant);
var
  tmp: RawByteString;
begin
  VariantToRawByteString(Source, tmp);
  RecordLoad(GetFieldAddr(Instance)^, tmp, fTypeInfo);
end;

procedure TOrmPropInfoRecordRTTI.NormalizeValue(var Value: RawUTF8);
begin // a BLOB should already be normalized
end;

function TOrmPropInfoRecordRTTI.CompareValue(Item1, Item2: TObject;
  CaseInsensitive: boolean): PtrInt;
begin
  if RecordEquals(GetFieldAddr(Item1)^, GetFieldAddr(Item2)^, fTypeInfo) then
    result := 0
  else
    result := PtrInt(Item1) - PtrInt(Item2); // pseudo comparison
end;

function TOrmPropInfoRecordRTTI.SetBinary(Instance: TObject; P, PEnd: PAnsiChar):
  PAnsiChar;
begin // use our RecordLoad() binary serialization
  result := RecordLoad(GetFieldAddr(Instance)^, P, fTypeInfo, nil, PEnd);
end;

procedure TOrmPropInfoRecordRTTI.SetValue(Instance: TObject; Value: PUTF8Char;
  wasString: boolean);
var
  data: RawByteString;
begin
  TextToBinary(Value, data);
  RecordLoad(GetFieldAddr(Instance)^, data, fTypeInfo);
end;

procedure TOrmPropInfoRecordRTTI.GetValueVar(Instance: TObject; ToSQL: boolean;
  var result: RawUTF8; wasSQLString: PBoolean);
begin
  result := RecordSave(GetFieldAddr(Instance)^, fTypeInfo);
  BinaryToText(result, ToSQL, wasSQLString);
end;

function TOrmPropInfoRecordRTTI.SetFieldSQLVar(Instance: TObject;
  const aValue: TSQLVar): boolean;
begin
  if aValue.VType = ftBlob then
    result := RecordLoad(GetFieldAddr(Instance)^, aValue.VBlob, fTypeInfo) <> nil
  else
    result := inherited SetFieldSQLVar(Instance, aValue);
end;

procedure TOrmPropInfoRecordRTTI.GetFieldSQLVar(Instance: TObject;
  var aValue: TSQLVar; var temp: RawByteString);
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

function TOrmPropInfoRecordFixedSize.GetSQLFieldRTTITypeName: RawUTF8;
begin
  if fTypeInfo = nil then
    result := inherited GetSQLFieldRTTITypeName
  else
    result := ToUTF8(fTypeInfo^.Name^);
end;

constructor TOrmPropInfoRecordFixedSize.Create(aRecordSize: cardinal;
  const aName: RawUTF8; aPropertyIndex: integer; aPropertyPointer: pointer;
  aAttributes: TOrmPropInfoAttributes; aFieldWidth: integer;
  aData2Text: TOnSQLPropInfoRecord2Text; aText2Data: TOnSQLPropInfoRecord2Data);
begin
  if integer(aRecordSize) <= 0 then
    raise EModelException.CreateUTF8('%.Create: invalid % record size',
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
  result := crc32c(0, GetFieldAddr(Instance), fRecordSize);
end;

procedure TOrmPropInfoRecordFixedSize.GetValueVar(Instance: TObject;
  ToSQL: boolean; var result: RawUTF8; wasSQLString: PBoolean);
begin
  FastSetString(result, GetFieldAddr(Instance), fRecordSize);
  BinaryToText(result, ToSQL, wasSQLString);
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

procedure TOrmPropInfoRecordFixedSize.NormalizeValue(var Value: RawUTF8);
begin // a BLOB should already be normalized
end;

function TOrmPropInfoRecordFixedSize.CompareValue(Item1, Item2: TObject;
  CaseInsensitive: boolean): PtrInt;
var
  i: integer;
  P1, P2: PByteArray;
begin
  if (Item1 = Item2) or (fRecordSize = 0) then
    result := 0
  else if Item1 = nil then
    result := -1
  else if Item2 = nil then
    result := 1
  else
  begin
    result := 0;
    P1 := GetFieldAddr(Item1);
    P2 := GetFieldAddr(Item2);
    for i := 0 to fRecordSize - 1 do
    begin
      result := P1^[i] - P2^[i];
      if result <> 0 then
        exit;
    end;
  end;
end;

function TOrmPropInfoRecordFixedSize.SetBinary(Instance: TObject;
  P, PEnd: PAnsiChar): PAnsiChar;
begin
  result := P + fRecordSize;
  if result > PEnd then
    result := nil
  else
    MoveFast(P^, GetFieldAddr(Instance)^, fRecordSize);
end;

procedure TOrmPropInfoRecordFixedSize.SetValue(Instance: TObject;
  Value: PUTF8Char; wasString: boolean);
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

function TOrmPropInfoRecordFixedSize.SetFieldSQLVar(Instance: TObject;
  const aValue: TSQLVar): boolean;
begin
  if aValue.VType = ftBlob then
  begin
    result := aValue.VBlobLen = fRecordSize;
    if result then
      MoveFast(aValue.VBlob^, GetFieldAddr(Instance)^, fRecordSize)
  end
  else
    result := inherited SetFieldSQLVar(Instance, aValue);
end;

procedure TOrmPropInfoRecordFixedSize.GetFieldSQLVar(Instance: TObject;
  var aValue: TSQLVar; var temp: RawByteString);
begin
  SetString(temp, PAnsiChar(GetFieldAddr(Instance)), fRecordSize);
  aValue.Options := [];
  aValue.VType := ftBlob;
  aValue.VBlob := pointer(temp);
  aValue.VBlobLen := length(temp);
end;


{ TOrmPropInfoCustomJSON }

constructor TOrmPropInfoCustomJSON.Create(aPropInfo: PRttiProp; aPropIndex: integer);
var
  attrib: TOrmPropInfoAttributes;
begin
  byte(attrib) := 0;
  if aPropInfo^.IsStored(nil) = AS_UNIQUE then
    Include(attrib, aIsUnique); // property MyProperty: RawUTF8 stored AS_UNIQUE;ieldWidth=10
  Create(aPropInfo^.typeInfo, ToUTF8(aPropInfo^.Name^), aPropIndex, aPropInfo^.GetFieldAddr
    (nil), attrib, aPropInfo^.Index);
end;

constructor TOrmPropInfoCustomJSON.Create(aTypeInfo: PRttiInfo;
  const aName: RawUTF8; aPropertyIndex: integer; aPropertyPointer: pointer;
  aAttributes: TOrmPropInfoAttributes; aFieldWidth: integer);
begin
  inherited Create(aName, oftUTF8Custom, aAttributes, aFieldWidth,
    aPropertyIndex, aPropertyPointer, nil, nil);
  fTypeInfo := aTypeInfo;
  SetCustomParser(rtti.RegisterType(aTypeInfo) as TRttiJson);
end;

constructor TOrmPropInfoCustomJSON.Create(const aTypeName, aName: RawUTF8;
  aPropertyIndex: integer; aPropertyPointer: pointer;
  aAttributes: TOrmPropInfoAttributes; aFieldWidth: integer);
begin
  inherited Create(aName, oftUTF8Custom, aAttributes, aFieldWidth,
    aPropertyIndex, aPropertyPointer, nil, nil);
  SetCustomParser(rtti.Find(pointer(aTypeName), length(aTypeName)) as TRttiJson);
end;

function TOrmPropInfoCustomJSON.GetSQLFieldRTTITypeName: RawUTF8;
begin
  if fTypeInfo = nil then
    result := inherited GetSQLFieldRTTITypeName
  else
    result := ToUTF8(fTypeInfo^.Name^);
end;

procedure TOrmPropInfoCustomJSON.SetCustomParser(aCustomParser: TRttiJson);
begin
  if aCustomParser = nil then
    raise EModelException.CreateUTF8('%.SetCustomParser: Invalid type information for %',
      [self, Name]);
  fCustomParser := aCustomParser;
end;

destructor TOrmPropInfoCustomJSON.Destroy;
begin
  inherited;
  fCustomParser.Free;
end;

procedure TOrmPropInfoCustomJSON.GetBinary(Instance: TObject; W: TBufferWriter);
var
  JSON: RawUTF8;
begin
  GetValueVar(Instance, false, JSON, nil);
  W.Write(JSON);
end;

function TOrmPropInfoCustomJSON.SetBinary(Instance: TObject; P, PEnd: PAnsiChar):
  PAnsiChar;
var
  tmp: TSynTempBuffer;
begin // stored as JSON VarString in the binary stream
  if FromVarString(PByte(P), PByte(PEnd), tmp) then
  try
    SetValue(Instance, tmp.buf, false);
  finally
    tmp.Done;
  end
  else
    P := nil;
  result := P;
end;

procedure TOrmPropInfoCustomJSON.NormalizeValue(var Value: RawUTF8);
begin // do nothing: should already be normalized
end;

procedure TOrmPropInfoCustomJSON.GetJSONValues(Instance: TObject; W: TJSONSerializer);
var
  Data: PByte;
begin
  Data := GetFieldAddr(Instance);
  W.AddRttiCustomJSON(Data, fCustomParser, []);
end;

procedure TOrmPropInfoCustomJSON.GetValueVar(Instance: TObject; ToSQL: boolean;
  var result: RawUTF8; wasSQLString: PBoolean);
var
  W: TJSONSerializer;
  temp: TTextWriterStackBuffer;
begin
  W := TJSONSerializer.CreateOwnedStream(temp);
  try
    GetJSONValues(Instance, W);
    W.SetText(result);
    if wasSQLString <> nil then
      wasSQLString^ := (result <> '') and (result[1] = '"');
  finally
    W.Free;
  end;
end;

procedure TOrmPropInfoCustomJSON.SetValue(Instance: TObject; Value: PUTF8Char;
  wasString: boolean);
var
  Data: PByte;
  B: PUTF8Char;
  len: PtrInt;
  tmp: RawUTF8;
begin
  Data := GetFieldAddr(Instance);
  if Value <> nil then
  begin // exact JSON string, array of objet ?
    B := GotoNextJSONObjectOrArray(Value);
    if (B = nil) and (Value^ = '"') then
    begin
      B := GotoEndOfJSONString(Value);
      if B^ <> '"' then
        B := nil;
    end;
    len := StrLen(Value);
    if (B = nil) or (B - Value <> len) then
    begin
      QuotedStrJSON(Value, len, tmp); // need escaping as JSON string
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
  if aTable.InheritsFrom(TOrmRTreeAbstract) then
    include(fOptions, pilAuxiliaryFields);
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
    if (p^.typeInfo^.Kind = rkClass) and
       (ClassOrmFieldType(p^.typeInfo) in [oftObject, oftUnknown]) then
    begin
      prev := PtrArrayAdd(aFlattenedProps, p);
      InternalAddParentsFirst(p^.typeInfo^.RttiClass^.RttiClass, aFlattenedProps);
      SetLength(aFlattenedProps, prev);
    end
    else if not (pilIgnoreIfGetter in fOptions) or p^.GetterIsField then
      Add(TOrmPropInfoRTTI.CreateFrom(p, Count, fOptions, aFlattenedProps));
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
    Add(TOrmPropInfoRTTI.CreateFrom(p, Count, fOptions, nil));
    p := p^.Next;
  end;
end;

function TOrmPropInfoList.Add(aItem: TOrmPropInfo): integer;
var
  f: integer;
begin
  if aItem = nil then
  begin
    result := -1;
    exit;
  end;
  // check that this property is not an ID/RowID (handled separately)
  if IsRowID(pointer(aItem.Name)) and not (pilAllowIDFields in fOptions) then
    raise EModelException.CreateUTF8(
      '%.Add: % should not include a [%] published property', [self, fTable, aItem.Name]);
  // check that this property name is not already defined
  for f := 0 to fCount - 1 do
    if IdemPropNameU(fList[f].Name, aItem.Name) then
      raise EModelException.CreateUTF8(
        '%.Add: % has duplicated name [%]', [self, fTable, aItem.Name]);
  // add to the internal list
  result := fCount;
  if result >= length(fList) then
    SetLength(fList, NextGrow(result));
  inc(fCount);
  fList[result] := aItem;
  fOrderedByName := nil; // force recompute sorted name array
end;

function TOrmPropInfoList.GetItem(aIndex: integer): TOrmPropInfo;
begin
  if cardinal(aIndex) >= cardinal(fCount) then
    raise EOrmException.Create('Invalid TOrmPropInfoList index');
  result := fList[aIndex];
end;

procedure TOrmPropInfoList.QuickSortByName(L, R: PtrInt);
var
  I, J, P, tmp: PtrInt;
  pivot: PUTF8Char;
begin
  if L < R then
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      repeat
        pivot := pointer(fList[fOrderedByName[P]].fName);
        while StrIComp(pointer(fList[fOrderedByName[I]].fName), pivot) < 0 do
          inc(I);
        while StrIComp(pointer(fList[fOrderedByName[J]].fName), pivot) > 0 do
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

function TOrmPropInfoList.ByRawUTF8Name(const aName: RawUTF8): TOrmPropInfo;
var
  i: integer;
begin
  i := IndexByName(pointer(aName));
  if i < 0 then
    result := nil
  else
    result := fList[i];
end;

function TOrmPropInfoList.ByName(aName: PUTF8Char): TOrmPropInfo;
var
  i: integer;
begin
  i := IndexByName(aName);
  if i < 0 then
    result := nil
  else
    result := fList[i];
end;

function TOrmPropInfoList.IndexByName(aName: PUTF8Char): integer;
var
  cmp, L, R: integer;
begin
  if (self <> nil) and (aName <> nil) and (fCount > 0) then
    if fCount < 5 then
    begin
      // no need to use binary search for a few fields
      for result := 0 to fCount - 1 do
        if StrIComp(pointer(fList[result].fName), aName) = 0 then
          exit;
    end
    else
    begin
      if fOrderedByName = nil then
      begin
        // initialize once the ordered lookup indexes, for binary search
        SetLength(fOrderedByName, fCount);
        FillIncreasing(pointer(fOrderedByName), 0, fCount);
        QuickSortByName(0, fCount - 1);
      end;
      L := 0;
      R := fCount - 1;
      repeat
        // fast O(log(n)) binary search
        result := (L + R) shr 1;
        cmp := StrIComp(pointer(fList[fOrderedByName[result]].fName), aName);
        if cmp = 0 then
        begin
          result := fOrderedByName[result];
          exit;
        end;
        if cmp < 0 then
          L := result + 1
        else
          R := result - 1;
      until L > R;
    end;
  result := -1;
end;

function TOrmPropInfoList.IndexByName(const aName: RawUTF8): integer;
begin
  result := IndexByName(pointer(aName));
end;

function TOrmPropInfoList.IndexByNameOrExceptShort(aName: ShortString): integer;
begin
  if IsRowIDShort(aName) then
    result := -1
  else
  begin
    inc(aName[0]);
    aName[ord(aName[0])] := #0; // make ASCIIZ
    result := IndexByName(@aName[0]); // fast O(log(n)) binary search
    if result < 0 then
      raise EOrmException.CreateUTF8(
        '%.IndexByNameOrExceptShort(%): unkwnown in %', [self, aName, fTable]);
  end;
end;

function TOrmPropInfoList.IndexByNameOrExcept(const aName: RawUTF8): integer;
begin
  if IsRowID(pointer(aName)) then
    result := -1
  else
  begin
    result := IndexByName(pointer(aName)); // fast O(log(n)) binary search
    if result < 0 then
      raise EOrmException.CreateUTF8(
        '%.IndexByNameOrExcept(%): unkwnown field in %', [self, aName, fTable]);
  end;
end;

procedure TOrmPropInfoList.IndexesByNamesOrExcept(const aNames: array of RawUTF8;
  const aIndexes: array of PInteger);
var
  i: PtrInt;
begin
  if high(aNames) <> high(aIndexes) then
    raise EOrmException.CreateUTF8('%.IndexesByNamesOrExcept(?)', [self]);
  for i := 0 to high(aNames) do
    if aIndexes[i] = nil then
      raise EOrmException.CreateUTF8('%.IndexesByNamesOrExcept(aIndexes[%]=nil)',
        [self, aNames[i]])
    else
      aIndexes[i]^ := IndexByNameOrExcept(aNames[i]);
end;

procedure TOrmPropInfoList.NamesToRawUTF8DynArray(var Names: TRawUTF8DynArray);
var
  i: PtrInt;
begin
  SetLength(Names, Count);
  for i := 0 to Count - 1 do
    Names[i] := fList[i].Name;
end;

function TOrmPropInfoList.IndexByNameUnflattenedOrExcept(const aName: RawUTF8): integer;
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
    // faster O(log(n)) binary search
    result := IndexByName(pointer(aName));
    if result >= 0 then
      exit;
  end;
  raise EOrmException.CreateUTF8(
    '%.IndexByNameUnflattenedOrExcept(%): unkwnown field in %', [self, aName, fTable]);
end;


{ ************ TOrm TOrmModel TOrmTable IRestOrm Core Definitions }

{$ifdef CPUX64}

// very efficient branchless asm - rcx/rdi=Item1 rdx/rsi=Item2
function TOrmDynArrayCompare(const Item1,Item2): integer;
{$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        mov     rcx, qword ptr[Item1]
        mov     rdx, qword ptr[Item2]
        mov     rcx, qword ptr[rcx + TOrm.fID]
        mov     rdx, qword ptr[rdx + TOrm.fID]
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
  with PQWordRec(@TOrm(Elem).fID)^ do
    result := crc32cBy4(L, H);
end;

function ClassOrmFieldType(info: PRttiInfo): TOrmFieldType;
const
  T_: array[0..6] of TClass = (
    // efficient access to the classes to be recognized
    TObject, TOrmMany, TOrm, TRawUTF8List, TStrings, TObjectList, TCollection);
var
  CT: PRttiClass;
  C: TClass;
  T: PClassArray; // for better code generation (especially on x86_64)
begin
  CT := info.RttiClass;
  T := @T_;
  result := oftUnknown;
  repeat
    // unrolled several InheritsFrom() calls
    C := CT^.RttiClass;
    if C = T[0] then
      // loop over all parent classes until root TObject is reached
      break;
    if C <> T[1] then
      if C <> T[2] then
        if (C <> T[3]) and (C <> T[4]) and (C <> T[5]) and (C <> T[6]) then
        begin
          if CT^.PropCount > 0 then
            // identify any class with published properties as oftObject
            result := oftObject;
            // but continue searching for any known class
          CT := CT^.ParentInfo.RttiClass;
          continue;
        end
        else
        begin
          // T[3..6]=TRawUTF8List,TStrings,TObjectList,TCollection
          result := oftObject;
          break;
        end
      else
      begin
        // T[2]=TOrm
        result := oftID; // TOrm field is pointer(RecordID), not an Instance
        break;
      end
    else
    begin
      // T[1]=TOrmMany
      result := oftMany; // no data is stored here, but in a pivot table
      break;
    end;
  until false;
end;

function ToText(vk: TOrmVirtualKind): PShortString;
begin
  result := GetEnumName(TypeInfo(TOrmVirtualKind), ord(vk));
end;

function GetVirtualTableSQLCreate(Props: TOrmProperties): RawUTF8;
var
  i: PtrInt;
  SQL: RawUTF8;
begin
  result := ''; // RowID is added by sqlite3_declare_vtab() for a Virtual Table
  for i := 0 to Props.Fields.Count - 1 do
    with Props.Fields.List[i] do
    begin
      SQL := Props.OrmFieldTypeToSQL(i);
      if SQL <> '' then
        // = '' for field with no matching DB column
        result := result + Name + SQL;
    end;
  if result = '' then
    result := ');'
  else
    PWord(@result[length(result) - 1])^ := ord(')') + ord(';') shl 8;
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
  if (aID = 0) or (aTableIndex > 63) then
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
  if (Model = nil) or (Value = 0) then
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

function RecordRef.Text(Model: TOrmModel): RawUTF8;
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
      result := Model.TableProps[Value and 63].Props.SQLTableName + ' ' +
        Int64ToUtf8(Value shr 6);
  end;
end;

function RecordRef.Text(const Rest: IRestOrm): RawUTF8;
var
  T: TOrmClass;
  aID: TID;
  m: TOrmModel;
begin
  result := '';
  if ((Value shr 6) = 0) or (Rest = nil) then
    exit;
  m := Rest.Model;
  T := Table(m);
  if T = nil then
    exit;
  aID := ID;
  with m.TableProps[Value and 63].Props do
    if aID <= 0 then
      result := SQLTableName
    else
    begin
      result := Rest.MainFieldValue(T, aID, true);
      if result = '' then
        FormatUTF8('% %', [SQLTableName, aID], result)
      else
        result := FormatUTF8('% "%"', [SQLTableName, result]);
    end;
end;


{ ------------ TOrmTable TOrmTableJSON Definitions }

{ TOrmTable }

function TOrmTable.FieldIndex(FieldName: PUTF8Char): integer;
begin
  if (self <> nil) and (fResults <> nil) and (FieldName <> nil) and
     (FieldCount > 0) then
    if IsRowID(FieldName) then
    begin // will work for both 'ID' or 'RowID'
      result := fFieldIndexID;
      exit;
    end
    else if FieldCount < 4 then
    begin
      for result := 0 to FieldCount - 1 do
        if StrIComp(fResults[result], FieldName) = 0 then
          exit;
    end
    else
    begin
      if fFieldNameOrder = nil then
        QuickSortIndexedPUTF8Char(fResults, FieldCount, fFieldNameOrder);
      result := FastFindIndexedPUTF8Char(fResults, FieldCount - 1,
        fFieldNameOrder, FieldName, @StrIComp);
      exit;
    end;
  result := -1;
end;

function TOrmTable.FieldIndex(const FieldName: RawUTF8): integer;
begin
  result := FieldIndex(Pointer(FieldName));
end;

function TOrmTable.FieldIndexExisting(const FieldName: RawUTF8): integer;
begin
  result := FieldIndex(Pointer(FieldName));
  if result < 0 then
    raise EOrmTable.CreateUTF8('%.FieldIndexExisting("%")', [self, FieldName]);
end;

function TOrmTable.FieldIndex(const FieldNames: array of RawUTF8;
  const FieldIndexes: array of PInteger): integer;
var
  i: PtrInt;
begin
  result := 0;
  if high(FieldNames) < 0 then
    exit;
  if high(FieldNames) <> high(FieldIndexes) then
    raise EOrmTable.CreateUTF8('%.FieldIndex() argument count', [self]);
  for i := 0 to high(FieldNames) do
    if FieldIndexes[i] = nil then
      raise EOrmTable.CreateUTF8('%.FieldIndex() FieldIndexes["%"]=nil',
        [self, FieldNames[i]])
    else
    begin
      FieldIndexes[i]^ := FieldIndex(pointer(FieldNames[i]));
      if FieldIndexes[i]^ >= 0 then
        inc(result);
    end;
end;

procedure TOrmTable.FieldIndexExisting(const FieldNames: array of RawUTF8;
  const FieldIndexes: array of PInteger);
var
  i: PtrInt;
begin
  if high(FieldNames) < 0 then
    exit;
  if high(FieldNames) <> high(FieldIndexes) then
    raise EOrmTable.CreateUTF8('%.FieldIndexExisting() argument count', [self]);
  for i := 0 to high(FieldNames) do
    if FieldIndexes[i] = nil then
      raise EOrmTable.CreateUTF8('%.FieldIndexExisting() FieldIndexes["%"]=nil',
        [self, FieldNames[i]])
    else
      FieldIndexes[i]^ := FieldIndexExisting(FieldNames[i]);
end;

function TOrmTable.FieldNames: TRawUTF8DynArray;
begin
  if length(fFieldNames) <> fFieldCount then
    InitFieldNames;
  result := fFieldNames;
end;

function TOrmTable.FieldValue(const FieldName: RawUTF8; Row: integer): PUTF8Char;
var
  Index: integer;
begin
  Index := FieldIndex(pointer(FieldName));
  if (Index < 0) or (cardinal(Row - 1) >= cardinal(fRowCount)) then
    result := nil
  else
    result := fResults[Index + Row * FieldCount];
end;

procedure TOrmTable.SortBitsFirst(var Bits);
var
  oldIDColumn, oldResults: TPUTF8CharDynArray;
  i, j, nSet, n: integer;
  R: PPUTF8Char;
begin
  if fIDColumn <> nil then
  begin
    n := length(fIDColumn);
    SetLength(oldIDColumn, n);
    MoveFast(fIDColumn[0], oldIDColumn[0], n * SizeOf(PUTF8Char));
  end;
  i := (fRowCount + 1) * FieldCount;
  SetLength(oldResults, i);
  MoveFast(fResults[0], oldResults[0], i * SizeOf(PUTF8Char));
  // put marked IDs first
  n := 1; // copy row data (first row=0 i.e. idents is left as it is)
  R := @fResults[FieldCount];
  j := FieldCount;
  for i := 1 to fRowCount do
  begin
    if GetBitPtr(@Bits, i - 1) then
    begin
      if fIDColumn <> nil then
        fIDColumn[n] := {%H-}oldIDColumn[i];
      MoveFast(oldResults[j], R^, FieldCount * SizeOf(PUTF8Char));
      inc(n);
      inc(R, FieldCount);
    end;
    inc(j, FieldCount);
  end;
  nSet := n - 1;
  // put unmarked IDs
  j := FieldCount;
  for i := 1 to fRowCount do
  begin
    if not GetBitPtr(@Bits, i - 1) then
    begin
      if fIDColumn <> nil then
        fIDColumn[n] := oldIDColumn[i];
      MoveFast(oldResults[j], R^, FieldCount * SizeOf(PUTF8Char));
      inc(n);
      inc(R, FieldCount);
    end;
    inc(j, FieldCount);
  end;
  assert(n - 1 = fRowCount);
  // recalcultate Bits[]
  FillCharFast(Bits, (fRowCount shr 3) + 1, 0);
  for i := 0 to nSet - 1 do
    SetBitPtr(@Bits, i); // slow but accurate
end;

function TOrmTable.IDColumnHide: boolean;
var
  fID, R, F: integer;
  S, D1, D2: PPUTF8Char;
begin
  // 1. check if possible
  result := false;
  if (self = nil) or Assigned(fIDColumn) or (FieldCount <= 1) then
    exit; // already hidden or not possible
  fID := fFieldIndexID;
  if fID < 0 then
    exit; // no 'ID' field
  // 2. alloc new arrays of PUTF8Char
  dec(fFieldCount);
  R := fRowCount + 1;
  SetLength(fIDColumn, R);               // will contain the ID column data
  SetLength(fNotIDColumn, R * FieldCount); // will be the new fResults[]
  // 3. copy fResults[] into new arrays
  S := @fResults[0];
  D1 := @fNotIDColumn[0];
  D2 := @fIDColumn[0];
  for R := 0 to fRowCount do
    for F := 0 to FieldCount do
    begin // we have FieldCount := FieldCount-1
      if F <> fID then
      begin
        D1^ := S^; // copy not ID column into fNotIDColumn[]
        inc(D1);
      end
      else
      begin
        D2^ := S^; // copy ID column into fIDColumn[]
        inc(D2);
      end;
      inc(S);
    end;
  // 4. TOrmTable data now points to new values without ID field
  result := true;
  fResults := @fNotIDColumn[0];
end;

function TOrmTable.IDColumnHiddenValue(Row: integer): TID;
begin
  if (self = nil) or (fResults = nil) or (Row <= 0) or (Row > fRowCount) then
    result := 0
  else if Assigned(fIDColumn) then // get hidden ID column UTF-8 content
    SetID(fIDColumn[Row], result{%H-})
  else if fFieldIndexID >= 0 then // get ID column field index
    SetID(fResults[Row * FieldCount + fFieldIndexID], result)
  else
    result := 0;
end;

procedure TOrmTable.IDArrayFromBits(const Bits; var IDs: TIDDynArray);
var
  n, i, fID: integer;
begin
  if not Assigned(fIDColumn) then
  begin
    fID := fFieldIndexID; // get ID column field index
    if fID < 0 then
      exit;
  end
  else
    fID := 0; // make compiler happy
  n := GetBitsCount(Bits, fRowCount);
  if n = fRowCount then
  begin
    IDColumnHiddenValues(IDs); // all selected -> direct get all IDs
    exit;
  end;
  SetLength(IDs, n);
  if n = 0 then
    exit;
  n := 0;
  if Assigned(fIDColumn) then
  begin
    for i := 1 to fRowCount do
      if GetBitPtr(@Bits, i - 1) then
      begin
        IDs[n] := GetInt64(fIDColumn[i]); // get hidden ID column UTF-8 content
        inc(n);
      end;
  end
  else
  begin
    inc(fID, FieldCount); // [i*FieldCount+fID] = [(i+1)*FieldCount+fID] below
    for i := 0 to fRowCount - 1 do
      if GetBitPtr(@Bits, i) then
      begin
        IDs[n] := GetInt64(fResults[i * FieldCount + fID]); // get ID column UTF-8 content
        inc(n);
      end;
  end;
end;

procedure TOrmTable.IDColumnHiddenValues(var IDs: TIDDynArray);
var
  n, i, fID: integer;
  U: PPUTF8Char;
begin
  n := fRowCount;
  if not Assigned(fIDColumn) then
  begin
    fID := fFieldIndexID; // get ID column field index
    if fID < 0 then
      n := 0;
  end
  else
    fID := 0;
  SetLength(IDs, n);
  if n = 0 then
    exit;
  if Assigned(fIDColumn) then
  begin
    for i := 1 to fRowCount do
      IDs[i - 1] := GetInt64(fIDColumn[i]); // get hidden ID column UTF-8 content
  end
  else
  begin
    U := @fResults[fID + FieldCount];  // U^ = ID column UTF-8 content
    for i := 0 to fRowCount - 1 do
    begin
      IDs[i] := GetInt64(U^);
      inc(U, FieldCount);
    end;
  end;
end;

procedure TOrmTable.IDArrayToBits(var Bits; var IDs: TIDDynArray);
var
  i, fID: integer;
  U: PPUTF8Char;
  ID: Pointer;
  IDmax: integer;
//    AllID: : TIDDynArray;
begin
  if length(IDs) = fRowCount then
  begin // all selected -> all bits set to 1
    FillCharFast(Bits, (fRowCount shr 3) + 1, 255);
    exit;
  end;
  FillCharFast(Bits, (fRowCount shr 3) + 1, 0);
  if IDs = nil then
    exit; // no selected -> all bits left to 0
  // we sort IDs to use FastFindInt64Sorted() and its O(log(n)) binary search
  ID := @IDs[0];
  IDmax := length(IDs) - 1;
  QuickSortInt64(ID, 0, IDmax);
  if not Assigned(fIDColumn) then
  begin
    fID := fFieldIndexID; // get ID column field index
    if fID < 0 then
      exit; // no ID column -> unable to get bit index
  end
  else
    fID := 0; // make compiler happy
  if Assigned(fIDColumn) then
  begin
    for i := 1 to fRowCount do
      if FastFindInt64Sorted(ID, IDmax, GetInt64(fIDColumn[i])) >= 0 then
        SetBitPtr(@Bits, i - 1);
  end
  else
  begin
    U := @fResults[fID + FieldCount];  // U^ = ID column UTF-8 content
    for i := 0 to fRowCount - 1 do
    begin
      if FastFindInt64Sorted(ID, IDmax, GetInt64(U^)) >= 0 then
        SetBitPtr(@Bits, i);
      inc(U, FieldCount);
    end;
  end;
{  // debugg:
  IDArrayFromBits(Bits,AllID);
  assert(length(AllID)=length(IDs));
  QuickSortInteger(@AllID[0],0,high(AllID));
  QuickSortInteger(@IDs[0],0,high(IDs));
  assert(comparemem(@AllID[0],@IDs[0],length(AllID)*SizeOf(TID))); }
end;

function TOrmTable.RowFromID(aID: TID; aNotFoundMinusOne: boolean): integer;
var
  ID: RawUTF8;
  fID: integer;
  U: PPUTF8Char;
begin
  if self = nil then
  begin
    result := -1;
    exit;
  end;
  if (fResults <> nil) and (aID > 0) then
  begin
    // search aID as UTF-8 in fIDColumn[] or fResults[]
    Int64ToUtf8(aID, ID);
    if Assigned(fIDColumn) then
    begin // get hidden ID column UTF-8 content
      for result := 1 to fRowCount do
        if StrComp(fIDColumn[result], pointer(ID)) = 0 then
          exit;
    end
    else
    begin
      fID := fFieldIndexID;  // get ID column field index
      if fID >= 0 then
      begin
        U := @fResults[fID + FieldCount];  // U^ = ID column UTF-8 content
        for result := 1 to fRowCount do
          if StrComp(U^, pointer(ID)) = 0 then
            exit
          else
            inc(U, FieldCount);
      end;
    end;
  end;
  if aNotFoundMinusOne then
    result := -1
  else
    result := fRowCount; // not found -> return last row index
end;

function TOrmTable.DeleteRow(Row: integer): boolean;
begin
  if (self = nil) or (Row < 1) or (Row > fRowCount) then
  begin
    result := false;
    exit; // out of range
  end;
  if Assigned(fIDColumn) then
    if Row < fRowCount then
      MoveFast(fIDColumn[Row + 1], fIDColumn[Row],
        (fRowCount - Row) * SizeOf(PUTF8Char));
  if Row < fRowCount then
  begin
    Row := Row * FieldCount; // convert row index into position in fResults[]
    MoveFast(fResults[Row + FieldCount], fResults[Row],
      (fRowCount * FieldCount - Row) * SizeOf(pointer));
  end;
  dec(fRowCount);
  result := true;
end;

procedure TOrmTable.InitFieldNames;
var
  f: integer;
  P: PUTF8Char;
begin
  SetLength(fFieldNames, fFieldCount); // share one TRawUTF8DynArray
  for f := 0 to fFieldCount - 1 do
  begin
    P := Get(0, f);
    if IsRowID(P) then // normalize RowID field name to 'ID'
      fFieldNames[f] := 'ID'
    else
      FastSetString(fFieldNames[f], P, StrLen(P));
  end;
end;

procedure TOrmTable.GetAsVariant(row, field: integer; out value: variant;
  expandTimeLogAsText, expandEnumsAsText, expandHugeIDAsUniqueIdentifier: boolean;
  options: TDocVariantOptions);
const
  JAN2015_UNIX = 1420070400;
var
  t: TTimeLogBits;
  id: TSynUniqueIdentifierBits;
  V: PUtf8Char;
  enum, err: integer;
  time: RawUTF8;
begin
  if (self = nil) or (row < 1) or (row > fRowCount) or
     (cardinal(field) >= cardinal(fFieldCount)) then
    exit; // out of range
  if fFieldType = nil then
    InitFieldTypes;
  V := fResults[row * fFieldCount + field];
  with fFieldType[field] do
    if expandHugeIDAsUniqueIdentifier and (field = fFieldIndexID) then
    begin
      SetInt64(V, PInt64(@id)^);
      if id.CreateTimeUnix > JAN2015_UNIX then
        id.ToVariant(value)
      else
        value := id.Value;
    end
    else
    begin
      if expandEnumsAsText and (ContentType = oftEnumerate) then
      begin
        enum := GetInteger(V, err);
        if (err = 0) and (ContentTypeInfo <> nil) then
        begin
          value := PRttiEnumType(ContentTypeInfo)^.GetEnumNameOrd(enum)^;
          exit;
        end;
      end
      else if expandTimeLogAsText then
        case ContentType of
          oftTimeLog, oftModTime, oftCreateTime, oftUnixTime, oftUnixMSTime:
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
                  'Value', t.Value], JSON_OPTIONS_FAST);
              end;
              exit;
            end;
        end;
      ValueVarToVariant(V, StrLen(V), ContentType, TVarData(value), true,
        ContentTypeInfo, options);
    end;
end;

procedure TOrmTable.ToDocVariant(Row: integer; out doc: variant;
  options: TDocVariantOptions; expandTimeLogAsText, expandEnumsAsText,
  expandHugeIDAsUniqueIdentifier: boolean);
var
  f: PtrInt;
  v: TVariantDynArray;
begin
  if (self = nil) or (Row < 1) or (Row > fRowCount) then
    exit; // out of range
  SetLength(v, fFieldCount);
  for f := 0 to fFieldCount - 1 do
    GetAsVariant(Row, f, v[f], expandTimeLogAsText, expandEnumsAsText,
      expandHugeIDAsUniqueIdentifier, options);
  if length(fFieldNames) <> fFieldCount then
    InitFieldNames;
  TDocVariantData(doc).InitObjectFromVariants(fFieldNames, v, JSON_OPTIONS_FAST);
end;

var
  OrmTableRowVariantType: TCustomVariantType = nil;

procedure TOrmTable.ToDocVariant(out docs: TVariantDynArray; readonly: boolean);
var
  r: PtrInt;
begin
  if (self = nil) or (fRowCount = 0) then
    exit;
  SetLength(docs, fRowCount);
  if readonly then
  begin
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
    for r := 0 to fRowCount - 1 do
      ToDocVariant(r + 1, docs[r]);
end;

procedure TOrmTable.ToDocVariant(out docarray: variant; readonly: boolean);
var
  Values: TVariantDynArray;
begin
  ToDocVariant(Values, readonly);
  TDocVariantData(docarray).InitArrayFromVariants(Values, JSON_OPTIONS_FAST);
end;

function TOrmTable.DeleteColumnValues(Field: integer): boolean;
var
  i: integer;
  U: PPUTF8Char;
begin
  if cardinal(Field) >= cardinal(FieldCount) then
  begin
    result := false;
    exit; // out of range
  end;
  U := @fResults[Field + FieldCount];  // U^ = column UTF-8 content for this field
  for i := 1 to fRowCount do
  begin
    U^ := nil; // just void UTF-8 content text
    inc(U, FieldCount);
  end;
  result := true;
end;

function TOrmTable.GetQueryTableNameFromSQL: RawUTF8;
begin
  if (fQueryTableNameFromSQL = '') and (fQuerySQL <> '') then
    fQueryTableNameFromSQL := GetTableNameFromSQLSelect(fQuerySQL, true);
  result := fQueryTableNameFromSQL;
end;

function TOrmTable.FieldPropFromTables(const PropName: RawUTF8;
  out PropInfo: TOrmPropInfo; out TableIndex: integer): TOrmFieldType;

  procedure SearchInQueryTables(aPropName: PUTF8Char; aTableIndex: integer);
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
      PropInfo := fQueryTables[aTableIndex].RecordProps.Fields.ByName(aPropName);
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
begin
  TableIndex := -1;
  result := oftUnknown;
  if fQueryTableIndexFromSQL = -2 then
  begin
    fQueryTableIndexFromSQL := -1;
    if (fQueryTables <> nil) and (QueryTableNameFromSQL <> '') then
      for i := 0 to length(fQueryTables) - 1 do
        if IdemPropNameU(fQueryTables[i].SQLTableName, fQueryTableNameFromSQL) then
        begin
          fQueryTableIndexFromSQL := i;
          break;
        end;
  end;
  if fQueryTableIndexFromSQL >= 0 then
  begin
    SearchInQueryTables(pointer(PropName), fQueryTableIndexFromSQL);
    if result <> oftUnknown then
      exit;
  end;
  if length(fQueryTables) = 1 then
    SearchInQueryTables(pointer(PropName), 0)
  else
  begin
    i := PosExChar('.', PropName) - 1;
    if i < 0 then
      // no 'ClassName.PropertyName' format: find first exact property name
      for t := 0 to high(fQueryTables) do
      begin
        SearchInQueryTables(pointer(PropName), t);
        if result <> oftUnknown then
          exit;
      end
    else
      // handle property names as 'ClassName.PropertyName'
      for t := 0 to high(fQueryTables) do
        if fQueryTables[t] <> nil then // avoid GPF
          if IdemPropNameU(fQueryTables[t].RecordProps.SQLTableName,
            pointer(PropName), i) then
          begin
            SearchInQueryTables(@PropName[i + 2], t);
            exit;
          end;
  end;
end;

procedure TOrmTable.SetFieldType(Field: integer; FieldType: TOrmFieldType;
  FieldTypeInfo: PRttiInfo; FieldSize, FieldTableIndex: integer);
begin
  if (self = nil) or (cardinal(Field) >= cardinal(FieldCount)) then
    exit;
  if fFieldType = nil then
    InitFieldTypes
  else if Field >= length(fFieldType) then
    SetLength(fFieldType, Field + 1); // e.g. from TOrmTableWritable.AddField
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
      // ftUTF8/ftNull are not precise enough
      ContentDB := ftUnknown
    else
      ContentDB := SQLFIELDTYPETODBFIELDTYPE[ContentType];
    TableIndex := FieldTableIndex;
  end;
end;

procedure TOrmTable.SetFieldType(const FieldName: RawUTF8;
  FieldType: TOrmFieldType; FieldTypeInfo: PRttiInfo; FieldSize: integer);
begin
  SetFieldType(FieldIndex(FieldName), FieldType, FieldTypeInfo, FieldSize);
end;

const
  DBTOFIELDTYPE: array[TSQLDBFieldType] of TOrmFieldType = (
    oftUnknown, oftUnknown,
    oftInteger, oftFloat, oftCurrency, oftDateTime, oftUTF8Text, oftBlob);

procedure TOrmTable.SetFieldTypes(const DBTypes: TSQLDBFieldTypeDynArray);
var
  f: PtrInt;
begin
  if length(DBTypes) <> FieldCount then
    raise EOrmTable.CreateUTF8('%.SetFieldTypes(DBTypes?)', [self]);
  for f := 0 to FieldCount - 1 do
    SetFieldType(f, DBTOFIELDTYPE[DBTypes[f]]);
end;

function TOrmTable.GetRowCount: integer;
begin
  if self = nil then
    result := 0
  else
    result := fRowCount;
end;

procedure TOrmTable.InitFieldTypes;
var
  f, i, len: integer;
  oft: TOrmFieldType;
  info: PRttiInfo;
  prop: TOrmPropInfo;
  size, tableindex: integer;
  U: PPUTF8Char;
  guessed: boolean;
  tlog: TTimeLog;
begin
  if Assigned(fQueryColumnTypes) and (FieldCount <> length(fQueryColumnTypes)) then
    raise EOrmTable.CreateUTF8('%.CreateWithColumnTypes() called ' +
      'with % column types, whereas the result has % columns',
      [self, length(fQueryColumnTypes), FieldCount]);
  SetLength(fFieldType, FieldCount);
  for f := 0 to FieldCount - 1 do
  begin
    prop := nil;
    info := nil;
    size := -1;
    tableindex := -1;
    guessed := false;
    // init fFieldType[] from fQueryTables/fQueryColumnTypes[]
    if Assigned(fQueryColumnTypes) then
      oft := fQueryColumnTypes[f]
    else if Assigned(QueryTables) then
    begin // retrieve column info from field name
      oft := FieldPropFromTables(fResults[f], prop, tableindex);
      if prop <> nil then
      begin
        if prop.InheritsFrom(TOrmPropInfoRTTI) then
          info := TOrmPropInfoRTTI(prop).PropType;
        size := prop.FieldWidth;
      end;
    end
    else
      oft := oftUnknown;
    if oft = oftUnknown then
      // not found in fQueryTables/fQueryColumnTypes[]: guess from content
      if IsRowID(fResults[f]) then
        oft := oftInteger
      else
      begin
        guessed := true;
        if f in fFieldParsedAsString then
        begin
          // the parser identified string values -> check if was oftDateTime
          oft := oftUTF8Text;
          U := @fResults[FieldCount + f];
          for i := 1 to fRowCount do
            if U^ = nil then  // search for a non void column
              inc(U, FieldCount)
            else
            begin
              len := StrLen(U^);
              tlog := Iso8601ToTimeLogPUTF8Char(U^, len);
              if tlog <> 0 then
                if (len in [8, 10]) and (cardinal(tlog shr 26) - 1800 < 300) then
                  // e.g. YYYYMMDD date (Y=1800..2100)
                  oft := oftDateTime
                else if len >= 15 then
                  // e.g. YYYYMMDDThhmmss date/time value
                  oft := oftDateTime;
              break;
            end;
        end
        else
        begin
          U := @fResults[FieldCount + f];
          for i := 1 to fRowCount do
          begin
            oft := UTF8ContentNumberType(U^);
            inc(U, FieldCount);
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
      end;
    SetFieldType(f, oft, info, size, tableindex);
    if guessed then
      fFieldType[f].ContentDB := ftUnknown; // may fail on some later row
  end;
end;

function TOrmTable.FieldType(Field: integer): TOrmFieldType;
begin
  if (self <> nil) and (cardinal(Field) < cardinal(FieldCount)) then
  begin
    if fFieldType = nil then
      InitFieldTypes;
    result := fFieldType[Field].ContentType;
  end
  else
    result := oftUnknown;
end;

function TOrmTable.FieldType(Field: integer;
  out FieldTypeInfo: POrmTableFieldType): TOrmFieldType;
begin
  if (self <> nil) and (cardinal(Field) < cardinal(FieldCount)) then
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

function TOrmTable.Get(Row, Field: integer): PUTF8Char;
begin
  if (self = nil) or (fResults = nil) or
     (cardinal(Row) > cardinal(fRowCount)) or
     (cardinal(Field) >= cardinal(FieldCount)) then // cardinal() -> test <0
    result := nil
  else
    result := fResults[Row * FieldCount + Field];
end;

function TOrmTable.GetU(Row, Field: integer): RawUTF8;
var
  P: PUTF8Char;
begin
  if (self = nil) or (fResults = nil) or
     (cardinal(Row) > cardinal(fRowCount)) or
     (cardinal(Field) >= cardinal(FieldCount)) then // cardinal() -> test <0
    result := ''
  else
  begin
    P := fResults[Row * FieldCount + Field];
    FastSetString(result, P, StrLen(P));
  end;
end;

function TOrmTable.Get(Row: integer; const FieldName: RawUTF8): PUTF8Char;
begin
  result := Get(Row, FieldIndex(FieldName));
end;

function TOrmTable.GetU(Row: integer; const FieldName: RawUTF8): RawUTF8;
begin
  result := GetU(Row, FieldIndex(FieldName));
end;

function TOrmTable.GetA(Row, Field: integer): WinAnsiString;
begin
  result := Utf8ToWinAnsi(Get(Row, Field));
end;

function TOrmTable.GetAsInteger(Row, Field: integer): integer;
begin
  result := GetInteger(Get(Row, Field));
end;

function TOrmTable.GetAsInteger(Row: integer; const FieldName: RawUTF8): integer;
begin
  result := GetInteger(Get(Row, FieldIndex(FieldName)));
end;

function TOrmTable.GetAsInt64(Row, Field: integer): Int64;
begin
  SetInt64(Get(Row, Field), result{%H-});
end;

function TOrmTable.GetAsInt64(Row: integer; const FieldName: RawUTF8): Int64;
begin
  SetInt64(Get(Row, FieldIndex(FieldName)), result{%H-});
end;

function TOrmTable.GetAsFloat(Row, Field: integer): TSynExtended;
begin
  result := GetExtended(Get(Row, Field));
end;

function TOrmTable.GetAsFloat(Row: integer; const FieldName: RawUTF8): TSynExtended;
begin
  result := GetExtended(Get(Row, FieldIndex(FieldName)));
end;

function TOrmTable.GetAsCurrency(Row, Field: integer): currency;
begin
  PInt64(@result)^ := StrToCurr64(Get(Row, Field), nil);
end;

function TOrmTable.GetAsCurrency(Row: integer; const FieldName: RawUTF8): currency;
begin
  result := GetAsCurrency(Row, FieldIndex(FieldName));
end;

function TOrmTable.GetAsDateTime(Row, Field: integer): TDateTime;
var
  P: PUTF8Char;
begin
  result := 0;
  if Row = 0 then
    exit; // header
  P := Get(Row, Field);
  if P = nil then
    exit;
  case FieldType(Field) of
    oftCurrency, oftFloat:
      result := GetExtended(P);
    oftInteger, // TOrmTable.InitFieldTypes may have recognized an integer
    oftTimeLog, oftModTime, oftCreateTime:
      result := TimeLogToDateTime(GetInt64(P));
    oftUnixTime:
      result := UnixTimeToDateTime(GetInt64(P));
    oftUnixMSTime:
      result := UnixMSTimeToDateTime(GetInt64(P));
  else // oftDateTime and any other kind will try from ISO-8601 text
    result := Iso8601ToDateTimePUTF8Char(P);
  end;
end;

function TOrmTable.GetAsDateTime(Row: integer; const FieldName: RawUTF8): TDateTime;
begin
  result := GetAsDateTime(Row, FieldIndex(FieldName));
end;

function TOrmTable.GetS(Row, Field: integer): shortstring;
begin
  UTF8ToShortString(result, Get(Row, Field));
end;

function TOrmTable.GetString(Row, Field: integer): string;
var
  U: PUTF8Char;
begin
  U := Get(Row, Field);
  if U = nil then
    result := ''
  else
    {$ifdef UNICODE}
    UTF8DecodeToUnicodeString(U, StrLen(U), result);
    {$else}
    CurrentAnsiConvert.UTF8BufferToAnsi(U, StrLen(U), RawByteString(result));
    {$endif UNICODE}
end;

function TOrmTable.GetSynUnicode(Row, Field: integer): SynUnicode;
var
  U: PUTF8Char;
begin
  result := '';
  U := Get(Row, Field);
  if U <> nil then
    UTF8ToSynUnicode(U, StrLen(U), result);
end;

function TOrmTable.GetCaption(Row, Field: integer): string;
begin
  GetCaptionFromPCharLen(Get(Row, Field), result);
end;

function TOrmTable.GetBlob(Row, Field: integer): RawBlob;
begin
  result := BlobToRawBlob(Get(Row, Field));
end;

function TOrmTable.GetBytes(Row, Field: integer): TBytes;
begin
  result := BlobToBytes(Get(Row, Field));
end;

function TOrmTable.GetStream(Row, Field: integer): TStream;
begin
  result := BlobToStream(Get(Row, Field));
end;

function TOrmTable.GetDateTime(Row, Field: integer): TDateTime;
begin
  result := Iso8601ToDateTimePUTF8Char(Get(Row, Field), 0)
end;

function TOrmTable.GetRowValues(Field: integer; out Values: TRawUTF8DynArray): integer;
var
  i: PtrInt;
  U: PPUTF8Char;
begin
  result := 0;
  if (self = nil) or (cardinal(Field) > cardinal(FieldCount)) or (fRowCount = 0) then
    exit;
  SetLength(Values, fRowCount);
  U := @fResults[FieldCount + Field]; // start reading after first Row (= Field Names)
  for i := 0 to fRowCount - 1 do
  begin
    FastSetString(Values[i], U^, StrLen(U^));
    inc(U, FieldCount); // go to next row
  end;
  result := fRowCount;
end;

function TOrmTable.GetRowValues(Field: integer; out Values: TInt64DynArray): integer;
var
  i: PtrInt;
  U: PPUTF8Char;
begin
  result := 0;
  if (self = nil) or (cardinal(Field) > cardinal(FieldCount)) or (fRowCount = 0) then
    exit;
  SetLength(Values, fRowCount);
  U := @fResults[FieldCount + Field]; // start reading after first Row (= Field Names)
  for i := 0 to fRowCount - 1 do
  begin
    SetInt64(U^, Values[i]);
    inc(U, FieldCount); // go to next row
  end;
  result := fRowCount;
end;

function TOrmTable.GetRowLengths(Field: integer; var LenStore: TSynTempBuffer): integer;
var
  len: PInteger;
  i: integer;
  U: PPUTF8Char;
begin
  result := 0;
  if (self = nil) or (cardinal(Field) > cardinal(FieldCount)) or (fRowCount = 0) then
  begin
    LenStore.buf := nil; // avoid GPF in LenStore.Done
    exit;
  end;
  U := @fResults[FieldCount + Field]; // start reading after first Row (= Field Names)
  len := LenStore.Init(fRowCount * SizeOf({%H-}len^));
  for i := 1 to fRowCount do
  begin
    len^ := StrLen(U^);
    inc(result, len^);
    inc(len);
    inc(U, FieldCount); // go to next row
  end;
end;

function TOrmTable.GetRowValues(Field: integer; const Sep, Head, Trail: RawUTF8): RawUTF8;
var
  i, L, SepLen: integer;
  U: PPUTF8Char;
  P: PUTF8Char;
  len: PInteger;
  tmp: TSynTempBuffer;
begin
  L := GetRowLengths(Field, tmp);
  if L = 0 then
  begin
    result := Head + Trail;
    exit;
  end;
  SepLen := length(Sep);
  inc(L, length(Head) + SepLen * (fRowCount - 1) + length(Trail));
  FastSetString(result, nil, L);
  P := AppendRawUTF8ToBuffer(pointer(result), Head);
  U := @fResults[FieldCount + Field]; // start reading after first Row (= Field Names)
  len := tmp.buf;
  for i := 2 to fRowCount do
  begin
    MoveFast(U^^, P^, len^);
    inc(P, len^);
    if SepLen > 0 then
    begin
      MoveSmall(pointer(Sep), P, SepLen);
      inc(P, SepLen);
    end;
    inc(len);
    inc(U, FieldCount); // go to next row
  end;
  MoveFast(U^^, P^, len^); // last row without Sep
  if Trail <> '' then
    MoveFast(pointer(Trail)^, P[len^], length(Trail));
  tmp.Done;
end;

procedure TOrmTable.GetJSONValues(W: TJSONWriter; RowFirst, RowLast,
  IDBinarySize: integer);
var
  U: PPUTF8Char;
  f, r: PtrInt;
  i64: Int64;
label
  nostr, str;
begin
  if (self = nil) or (FieldCount <= 0) or (fRowCount <= 0) then
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
    W.ColNames[f] := fResults[f]; // first Row is field Names
    if not Assigned(OnExportValue) then
      if (f = fFieldIndexID) and (IDBinarySize > 0) then
        W.ColNames[f] := 'id'; // ajax-friendly
  end;
  W.AddColumns(RowLast - RowFirst + 1); // write or init field names (see JSON Expand)
  if W.Expand then
    W.Add('[');
  // write rows data
  U := @fResults[FieldCount * RowFirst];
  for r := RowFirst to RowLast do
  begin
    if W.Expand then
      W.Add('{');
    for f := 0 to FieldCount - 1 do
    begin
      if W.Expand then
        W.AddString(W.ColNames[f]); // '"'+ColNames[]+'":'
      if Assigned(OnExportValue) then
        W.AddString(OnExportValue(self, r, f, false))
      else if (IDBinarySize > 0) and (f = fFieldIndexID) then
      begin
        SetInt64(U^, i64{%H-});
        W.AddBinToHexDisplayQuoted(@i64, IDBinarySize);
      end
      else if U^ = nil then
        W.AddNull
      else
        case fFieldType[f].ContentDB of
          ftInt64, ftDouble, ftCurrency:
nostr:      W.AddNoJSONEscape(U^, StrLen(U^));
          ftDate, ftUTF8, ftBlob:
            begin
str:          W.Add('"');
              W.AddJSONEscape(U^);
              W.Add('"');
            end;
        else
          if IsStringJSON(U^) then // fast and safe enough
            goto str
          else
            goto nostr;
        end;
      W.Add(',');
      inc(U); // points to next value
    end;
    W.CancelLastComma;
    if W.Expand then
    begin
      W.Add('}', ',');
      if r <> RowLast then
        W.AddCR; // make expanded json more human readable
    end
    else
      W.Add(',');
  end;
  W.EndJSONObject(1, 0, false); // "RowCount": set by W.AddColumns() above
end;

procedure TOrmTable.GetJSONValues(JSON: TStream; Expand: boolean;
  RowFirst, RowLast, IDBinarySize: integer);
var
  W: TJSONWriter;
  tmp: TTextWriterStackBuffer;
begin
  W := TJSONWriter.Create(JSON, Expand, false, nil, 0, @tmp);
  try
    GetJSONValues(W, RowFirst, RowLast, IDBinarySize);
    W.FlushFinal;
  finally
    W.Free;
  end;
end;

function TOrmTable.GetJSONValues(Expand: boolean;
  IDBinarySize, BufferSize: integer): RawUTF8;
var
  W: TJSONWriter;
  tmp: TTextWriterStackBuffer;
begin
  if BufferSize < SizeOf(tmp) then
    W := TJSONWriter.CreateOwnedStream(tmp)
  else
    W := TJSONWriter.CreateOwnedStream(BufferSize);
  try
    W.Expand := Expand;
    GetJSONValues(W, 0, 0, IDBinarySize); // create JSON data in MS
    W.SetText(result);
  finally
    W.Free;
  end;
end;

procedure TOrmTable.GetCSVValues(Dest: TStream; Tab: boolean; CommaSep: AnsiChar;
  AddBOM: boolean; RowFirst, RowLast: integer);
var
  U: PPUTF8Char;
  F, R, FMax: integer;
  W: TTextWriter;
  temp: TTextWriterStackBuffer;
begin
  if (self = nil) or (FieldCount <= 0) or (fRowCount <= 0) then
    exit;
  if (RowLast = 0) or (RowLast > fRowCount) then
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
    U := @fResults[RowFirst * FieldCount];
    for R := RowFirst to RowLast do
      for F := 0 to FMax do
      begin
        if Assigned(OnExportValue) then
          W.AddString(OnExportValue(self, R, F, false))
        else if Tab or not IsStringJSON(U^) then
          W.AddNoJSONEscape(U^, StrLen(U^))
        else
        begin
          W.Add('"');
          W.AddNoJSONEscape(U^, StrLen(U^));
          W.Add('"');
        end;
        if F = FMax then
          W.AddCR
        else
          W.Add(CommaSep);
        inc(U); // points to next value
      end;
    W.FlushFinal;
  finally
    W.Free;
  end;
end;

function TOrmTable.GetCSVValues(Tab: boolean; CommaSep: AnsiChar;
  AddBOM: boolean; RowFirst, RowLast: integer): RawUTF8;
var
  MS: TRawByteStringStream;
begin
  MS := TRawByteStringStream.Create;
  try
    GetCSVValues(MS, Tab, CommaSep, AddBOM, RowFirst, RowLast);
    result := MS.DataString;
  finally
    MS.Free;
  end;
end;

procedure TOrmTable.GetMSRowSetValues(Dest: TStream; RowFirst, RowLast: integer);
const
  FIELDTYPE_TOXML: array[TSQLDBFieldType] of RawUTF8 = (
  // ftUnknown, ftNull, ftInt64, ftDouble, ftCurrency,
    '', '', ' dt:type="i8"', ' dt:type="float"',
    ' dt:type="number" rs:dbtype="currency"',
  // ftDate, ftUTF8, ftBlob
    ' dt:type="dateTime"', ' dt:type="string"', ' dt:type="bin.hex"');
var
  W: TTextWriter;
  f, r: integer;
  U: PPUTF8Char;
begin
  W := TTextWriter.Create(Dest, 32768);
  try
    W.AddShort('<xml xmlns:s="uuid:BDC6E3F0-6DA3-11d1-A2A3-00AA00C14882" ' +
      'xmlns:dt="uuid:C2F41010-65B3-11d1-A29F-00AA00C14882" ' +
      'xmlns:rs="urn:schemas-microsoft-com:rowset" xmlns:z="#RowsetSchema">');
    if (self <> nil) and ((FieldCount > 0) or (fRowCount > 0)) then
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
      U := @fResults[FieldCount * RowFirst];
      W.AddShort('<rs:data>');
      for r := RowFirst to RowLast do
      begin
        W.AddShorter('<z:row ');
        for f := 0 to FieldCount - 1 do
        begin
          if U^ <> nil then
          begin
            W.Add('f');
            W.Add(f);
            W.Add('=', '"');
            W.AddXmlEscape(U^);
            W.Add('"', ' ');
          end;
          inc(U); // points to next value
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

function TOrmTable.GetMSRowSetValues: RawUTF8;
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

function TOrmTable.GetODSDocument(withColumnTypes: boolean): RawByteString;
const
  ODSmimetype: RawUTF8 =
    'application/vnd.oasis.opendocument.spreadsheet';
  ODSContentHeader: RawUTF8 =
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
  ODSstyles: RawUTF8 = XMLUTF8_HEADER +
    '<office:document-styles xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0"' +
    ' office:version="1.2"></office:document-styles>';
  ODSmeta: RawUTF8 = XMLUTF8_HEADER +
    '<office:document-meta xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0"' +
    ' office:version="1.2"></office:document-meta>';
  ODSsettings: RawUTF8 = XMLUTF8_HEADER +
    '<office:document-settings xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0"' +
    ' office:version="1.2"></office:document-settings>';
  ODSmanifest: RawUTF8 = XMLUTF8_HEADER +
    '<manifest:manifest xmlns:manifest="urn:oasis:names:tc:opendocument:xmlns:manifest:1.0"' +
    ' manifest:version="1.2"><manifest:file-entry manifest:full-path="/"' +
    ' manifest:version="1.2" manifest:media-type="application/vnd.oasis.opendocument.spreadsheet"/>' +
    '<manifest:file-entry manifest:full-path="meta.xml" manifest:media-type="text/xml"/>' +
    '<manifest:file-entry manifest:full-path="settings.xml" manifest:media-type="text/xml"/>' +
    '<manifest:file-entry manifest:full-path="content.xml" manifest:media-type="text/xml"/>' +
    '<manifest:file-entry manifest:full-path="styles.xml" manifest:media-type="text/xml"/>' +
    '</manifest:manifest>';
var
  Zip: TZipWriteToStream;
  Dest: TRawByteStringStream;
  content: RawUTF8;
  W: TTextWriter;
  U: PPUTF8Char;
  r, f: integer;
begin
  Dest := TRawByteStringStream.Create;
  try
    Zip := TZipWriteToStream.Create(Dest);
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
        if (self <> nil) and ((FieldCount > 0) or (fRowCount > 0)) then
        begin
          if withColumnTypes and (fFieldType = nil) then
            InitFieldTypes;
          U := pointer(fResults);
          for r := 0 to fRowCount do
          begin
            W.AddShort('<table:table-row>');
            if withColumnTypes and (r > 0) then
            begin
              for f := 0 to FieldCount - 1 do
              begin
                W.AddShort('<table:table-cell office:value-type="');
                case fFieldType[f].ContentDB of
                  ftInt64, ftDouble, ftCurrency:
                    begin
                      W.AddShort('float" office:value="');
                      W.AddXmlEscape(U^);
                      W.AddShorter('" />');
                    end;
                  ftDate:
                    begin
                      W.AddShort('date" office:date-value="');
                      W.AddXmlEscape(U^);
                      W.AddShorter('" />');
                    end;
                else
                  begin
                    //ftUnknown,ftNull,ftUTF8,ftBlob:
                    W.AddShort('string"><text:p>');
                    W.AddXmlEscape(U^);
                    W.AddShort('</text:p></table:table-cell>');
                  end;
                end;
                inc(U); // points to next value
              end;
            end
            else
              for f := 0 to FieldCount - 1 do
              begin
                W.AddShort('<table:table-cell office:value-type="string"><text:p>');
                W.AddXmlEscape(U^);
                W.AddShort('</text:p></table:table-cell>');
                inc(U);
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

procedure TOrmTable.GetHtmlTable(Dest: TTextWriter);
var
  R, F: integer;
  U: PPUTF8Char;
begin
  Dest.AddShort('<table>'#10);
  U := pointer(fResults);
  for R := 0 to fRowCount do
  begin
    Dest.AddShorter('<tr>');
    for F := 0 to FieldCount - 1 do
    begin
      if R = 0 then
        Dest.AddShorter('<th>')
      else
        Dest.AddShorter('<td>');
      if Assigned(OnExportValue) and (R > 0) then
        Dest.AddHtmlEscapeUTF8(OnExportValue(self, R, F, true), hfOutsideAttributes)
      else
        Dest.AddHtmlEscape(U^, hfOutsideAttributes);
      if R = 0 then
        Dest.AddShorter('</th>')
      else
        Dest.AddShorter('</td>');
      inc(U); // points to next value
    end;
    Dest.AddShorter('</tr>'#10);
  end;
  Dest.AddShorter('</table>');
end;

function TOrmTable.GetHtmlTable(const Header: RawUTF8): RawUTF8;
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

function TOrmTable.GetW(Row, Field: integer): RawUnicode;
begin
  result := UTF8DecodeToRawUnicode(Get(Row, Field), 0);
end;

function TOrmTable.GetWP(Row, Field: integer; Dest: PWideChar; MaxDestChars:
  cardinal): integer;
var
  P: PUTF8Char;
begin
  P := Get(Row, Field);
  result := UTF8ToWideChar(Dest, P, MaxDestChars, 0) shr 1; // bytes div 2
end;

function TOrmTable.LengthW(Row, Field: integer): integer;
begin // nil -> fast calculate unicode length, without any memory allocation
  result := Utf8ToUnicodeLength(Get(Row, Field));
end;

var
  /// simple wrapper to UTF-8 compare function for the SQLite3 field datatypes
  // - used internaly for field sorting (see TOrmTable.SortFields() method)
  // and for default User Interface Query (see TRest.QueryIsTrue() method)
  // - some functions do not match exactly the TUTF8Compare signature, so will
  // be set in the initialization section of this unit
  OrmFieldTypeComp: array[TOrmFieldType] of TUTF8Compare  =
   (nil,                  // unknown
    nil,                 // AnsiText = AnsiIComp (in initialization below)
    UTF8IComp,           // UTF8Text, 8 bits case insensitive compared
    UTF8CompareUInt32,   // Enumerate
    UTF8CompareUInt32,   // Set
    UTF8CompareInt64,    // integer
    UTF8CompareInt64,    // ID
    UTF8CompareRecord,   // Record
    UTF8CompareBoolean,  // boolean
    UTF8CompareDouble,   // Float
    UTF8CompareISO8601,  // TDateTime
    UTF8CompareInt64,    // TTimeLog
    UTF8CompareCurr64,   // Currency
    nil,                 // Object (TEXT serialization) = StrComp
    nil,                 // Variant (TEXT serialization) = StrComp
    nil,                 // TNullable* = StrComp
    nil,                 // Blob
    nil,                 // BlobDynArray
    nil,                 // BlobCustom
    nil,                 // UTF8Custom
    nil,                 // Many
    UTF8CompareInt64,    // TModTime
    UTF8CompareInt64,    // TCreateTime
    UTF8CompareInt64,    // TID
    UTF8CompareInt64,    // TRecordVersion
    UTF8CompareInt64,    // TSessionUserID
    UTF8CompareISO8601,  // TDateTimeMS
    UTF8CompareInt64,    // TUnixTime
    UTF8CompareInt64);   // TUnixMSTime

type
  /// a static object is used for smaller recursive stack size and faster code
  // - these special sort implementation do the comparison first by the
  // designed field, and, if the field value is identical, the ID value is
  // used (it will therefore sort by time all identical values)
  // - code generated is very optimized: stack and memory usage, CPU registers
  // prefered, multiplication avoided to calculate memory position from index,
  // hand tuned assembler...
  TUTF8QuickSort = object
  public
    // sort parameters
    Results: PPUtf8CharArray;
    IDColumn: PPUtf8CharArray;
    Params: TOrmTableSortParams;
    CurrentRow: PtrInt;
    // avoid multiplications to calculate data memory position from index
    FieldCountNextPtr, FieldFirstPtr, FieldIDPtr: PtrUInt;
    // temp vars (avoid stack usage):
    PID: Int64;
    PP, CI, CJ: PPUTF8Char;
    i, J: PtrInt;
    /// recursively perform the sort
    procedure Sort(L, R: integer);
    /// compare value at index I with pivot value
    // - sort by ID if values are identical
    function CompI: integer; {$ifdef HASINLINE}inline;{$endif}
    /// compare value at index J with pivot value
    // - sort by ID if values are identical
    function CompJ: integer; {$ifdef HASINLINE}inline;{$endif}
    /// set the pivot value
    procedure SetPP(aPP: PPUTF8Char; aP: PtrInt);
  end;

procedure TUTF8QuickSort.SetPP(aPP: PPUTF8Char; aP: PtrInt);
begin
  PP := aPP;
  // PID must be updated every time PP is modified
  if Assigned(IDColumn) then
    SetInt64(IDColumn[aP], PID)
  else
    SetInt64(PPUTF8Char(PAnsiChar(aPP) - FieldIDPtr)^, PID);
end;

function TUTF8QuickSort.CompI: integer;
var
  i64: Int64;
begin
  result := Params.Comp(CI^, PP^);
  if result = 0 then
  begin
    // same value -> sort by ID
    if Assigned(IDColumn) then
      SetInt64(IDColumn[i], i64{%H-})
    else
      SetInt64(PPUTF8Char(PAnsiChar(CI) - FieldIDPtr)^, i64);
    if i64 < PID then
      result := -1
    else if i64 <> PID then
      result := +1;
  end;
end;

function TUTF8QuickSort.CompJ: integer;
var
  i64: Int64;
begin
  result := Params.Comp(CJ^, PP^);
  if result = 0 then
  begin
    // same value -> sort by ID
    if Assigned(IDColumn) then
      SetInt64(IDColumn[J], i64{%H-})
    else
      SetInt64(PPUTF8Char(PAnsiChar(CJ) - FieldIDPtr)^, i64);
    if i64 < PID then
      result := -1
    else if i64 <> PID then
      result := +1;
  end;
end;

{$ifdef CPUX86}
procedure ExchgFields(P1,P2: PPointer; FieldCount: PtrUInt);
{$ifdef FPC} nostackframe; assembler; {$endif}
asm // eax=P1 edx=P2 ecx=FieldCount
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
procedure ExchgFields(P1,P2: PPointer; FieldCount: PtrUInt); inline;
var p: pointer;
begin
  repeat
    p := P1^;
    P1^ := P2^;
    P2^ := p;
    inc(P1);
    inc(P2);
    dec(FieldCount);
  until FieldCount = 0;
end;
{$endif CPUX86}

procedure ExchgPointer(p1, p2: PPointer); {$ifdef HASINLINE}inline;{$endif}
var
  p: pointer;
begin
  p := p2^;
  p2^ := p1^;
  p1^ := p;
end;

procedure TUTF8QuickSort.Sort(L, R: integer);
var
  P: PtrInt;
begin
  if @Params.Comp <> nil then
    repeat
      i := L;
      CI := @Results[i * Params.FieldCount + Params.FieldIndex];
      J := R;
      CJ := @Results[J * Params.FieldCount + Params.FieldIndex];
      P := ((i + J) shr 1);
      SetPP(@Results[P * Params.FieldCount + Params.FieldIndex], P);
      repeat
        // this loop has no multiplication -> most of the time is spent in compIJ
        if Params.Asc then
        begin // ascending order comparison
          while compI < 0 do
          begin
            inc(i);
            inc(PByte(CI), FieldCountNextPtr); // next row
          end;
          while compJ > 0 do
          begin
            dec(J);
            dec(PByte(CJ), FieldCountNextPtr); // previous row
          end;
        end
        else
        begin // descending order comparison
          while compI > 0 do
          begin
            inc(i);
            inc(PByte(CI), FieldCountNextPtr); // next row
          end;
          while compJ < 0 do
          begin
            dec(J);
            dec(PByte(CJ), FieldCountNextPtr); // previous row
          end;
        end;
        if i <= J then
        begin
          if i <> J then
          begin // swap elements
            if CurrentRow = J then // update current row number
              CurrentRow := i
            else if CurrentRow = i then
              CurrentRow := J;
            // full row exchange
            // exchange PUTF8Char for whole I,J rows
            ExchgFields(pointer(PAnsiChar(CI) - FieldFirstPtr),
              pointer(PAnsiChar(CJ) - FieldFirstPtr), Params.FieldCount);
            if Assigned(IDColumn) then // exchange hidden ID column also
              ExchgPointer(@IDColumn[i], @IDColumn[J]);
          end;
          if PP = CI then
            SetPP(CJ, J)
          else if PP = CJ then
            SetPP(CI, i);
          inc(i);
          dec(J);
          inc(PByte(CI), FieldCountNextPtr);
          dec(PByte(CJ), FieldCountNextPtr);
        end
        else
          break;
      until i > J;
      if J - L < R - i then
      begin // use recursion only for smaller range
        P := i; // I,J will be overriden in QuickSort() call
        if L < J then
          Sort(L, J);
        L := P;
      end
      else
      begin
        P := J;
        if i < R then
          Sort(i, R);
        R := P
      end;
    until L >= R;
end;

procedure TOrmTable.SortFields(const FieldName: RawUTF8; Asc: boolean;
  PCurrentRow: PInteger; FieldType: TOrmFieldType; CustomCompare: TUTF8Compare);
begin
  SortFields(FieldIndex(FieldName), Asc, PCurrentRow, FieldType, CustomCompare);
end;

procedure TOrmTable.SortFields(Field: integer; Asc: boolean;
  PCurrentRow: PInteger; FieldType: TOrmFieldType; CustomCompare: TUTF8Compare);
var
  quicksort: TUTF8QuickSort; // fast static object for sorting
begin
  if (FieldCount = 0) or (cardinal(Field) >= cardinal(FieldCount)) then
    exit;
  if FieldType = oftUnknown then // guess the field type from first row
    FieldType := self.FieldType(Field);
  // store sorting parameters for re-sort in TOrmTableJSON.FillFrom()
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
  quicksort.Results := fResults;
  quicksort.IDColumn := @fIDColumn[0];
  quicksort.FieldCountNextPtr := FieldCount * SizeOf(PtrInt);
  quicksort.FieldFirstPtr := Field * SizeOf(PtrInt);
  if fFieldIndexID < 0 then // if no ID colum, assume first
    quicksort.FieldIDPtr := quicksort.FieldFirstPtr
  else
    quicksort.FieldIDPtr := (Field - fFieldIndexID) * SizeOf(PtrInt);
  if PCurrentRow = nil then
    quicksort.CurrentRow := -1
  else
    quicksort.CurrentRow := PCurrentRow^;
  if fRowCount > 1 then // ignore first row = field names -> (1,RowCount)
    quicksort.Sort(1, fRowCount);
  if PCurrentRow <> nil then
    PCurrentRow^ := quicksort.CurrentRow;
end;

function TOrmTable.SearchFieldSorted(const Value: RawUTF8; FieldIndex: integer;
  CustomCompare: TUTF8Compare): integer;
begin
  result := SearchFieldSorted(pointer(Value), FieldIndex, CustomCompare);
end;

function TOrmTable.SearchFieldSorted(Value: PUTF8Char; FieldIndex: integer;
  CustomCompare: TUTF8Compare): integer;
var
  L, R, cmp: integer;
begin
  if (self <> nil) and (Value <> nil) and (fRowCount > 0) and
     (cardinal(FieldIndex) < cardinal(fFieldCount)) then
  begin
    if not Assigned(CustomCompare) then
      CustomCompare := fSortParams.Comp;
    if Assigned(CustomCompare) then
    begin // fast binary search
      L := 1;
      R := fRowCount;
      repeat
        result := (L + R) shr 1;
        cmp := CustomCompare(fResults[result * fFieldCount + FieldIndex], Value);
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
  {$ifdef USERECORDWITHMETHODS}
  TUTF8QuickSortMulti = record
  {$else}
  TUTF8QuickSortMulti = object
  {$endif USERECORDWITHMETHODS}
  public
    Results: PPUtf8CharArray;
    IDColumn: PPUtf8CharArray;
    FieldCount: integer;
    IndexMax: integer;
    Index: array of record
      ndx: integer;
      Comp: TUTF8Compare;
      Desc: boolean;
    end;
    // used for row content comparison
    function Compare(A, B: integer): integer;
    /// recursively perform the sort
    procedure Sort(L, R: integer);
  end;

function TUTF8QuickSortMulti.Compare(A, B: integer): integer;
var
  i: PtrInt;
begin
  result := 0;
  for i := 0 to IndexMax do
    with Index[i] do
    begin
      if ndx >= 0 then
        result := Comp(Results[A * FieldCount + ndx], Results[B * FieldCount + ndx])
      else  // Fields[].ndx=-1 for hidden ID column
        result := GetInt64(IDColumn[A]) - GetInt64(IDColumn[B]);
      if result <> 0 then
      begin
        if Desc then
          result := -result; // descending order -> inverse comparison
        exit;
      end;
    end;
end;

procedure TUTF8QuickSortMulti.Sort(L, R: integer);
var
  I, J, P: integer;
begin
  if L < R then
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      repeat
        while Compare(I, P) < 0 do
          inc(I);
        while Compare(J, P) > 0 do
          dec(J);
        if I <= J then
        begin
          if I <> J then
          begin // swap elements
            ExchgFields(pointer(PtrUInt(@Results[I * FieldCount])),
              pointer(PtrUInt(@Results[J * FieldCount])), FieldCount);
            if Assigned(IDColumn) then // update hidden ID column also
              ExchgPointer(@IDColumn[I], @IDColumn[J]);
          end;
          if P = I then
            P := J
          else if P = J then
            P := I;
          inc(I);
          dec(J);
        end;
      until I > J;
      if J - L < R - I then
      begin // use recursion only for smaller range
        if L < J then
          Sort(L, J);
        L := I;
      end
      else
      begin
        if I < R then
          Sort(I, R);
        R := J;
      end;
    until L >= R;
end;

procedure TOrmTable.SortFields(const Fields: array of integer;
  const Asc: array of boolean; const CustomCompare: array of TUTF8Compare);
var
  quicksort: TUTF8QuickSortMulti;
  i: integer;
begin
  if (self = nil) or (fRowCount <= 1) or (FieldCount <= 0) or (length(Fields) = 0) then
    exit;
  quicksort.FieldCount := FieldCount;
  quicksort.IndexMax := high(Fields);
  SetLength(quicksort.Index, quicksort.IndexMax + 1);
  for i := 0 to quicksort.IndexMax do
    with quicksort.Index[i] do
    begin
      if i <= high(CustomCompare) then
        Comp := CustomCompare[i];
      ndx := Fields[i];
      if ndx < 0 then
      begin // Fields[]=-1 for ID column
        if not Assigned(fIDColumn) then
        begin // leave ndx<0 for hidden ID
          ndx := fFieldIndexID;  // use the ID column
          if ndx < 0 then
            exit; // no ID column available
          if @Comp = nil then
            Comp := @UTF8CompareInt64;
        end;
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
  quicksort.Results := fResults;
  quicksort.IDColumn := @fIDColumn[0];
  quicksort.Sort(1, fRowCount); // ignore first row = field names -> (1,RowCount)
end;

function TOrmTable.SortCompare(Field: integer): TUTF8Compare;
begin
  result := OrmFieldTypeComp[FieldType(Field)];
end;

procedure TOrmTable.Assign(source: TOrmTable);
begin
  fResults := source.fResults;
  fRowCount := source.fRowCount;
  fFieldCount := source.fFieldCount;
end;

constructor TOrmTable.Create(const aSQL: RawUTF8);
begin
  fQuerySQL := aSQL;
  fFieldIndexID := -1;
  fQueryTableIndexFromSQL := -2; // indicates not searched
end;

constructor TOrmTable.CreateFromTables(const Tables: array of TOrmClass;
  const aSQL: RawUTF8);
var
  n: integer;
begin
  Create(aSQL);
  n := length(Tables);
  if n > 0 then
  begin
    SetLength(fQueryTables, n);
    MoveFast(Tables[0], fQueryTables[0], n * SizeOf(TClass));
  end;
end;

constructor TOrmTable.CreateWithColumnTypes(
  const ColumnTypes: array of TOrmFieldType; const aSQL: RawUTF8);
begin
  Create(aSQL);
  SetLength(fQueryColumnTypes, length(ColumnTypes));
  MoveFast(ColumnTypes[0], fQueryColumnTypes[0], length(ColumnTypes) * SizeOf(TOrmFieldType));
end;

destructor TOrmTable.Destroy;
begin
  fOwnedRecords.Free;
  inherited Destroy;
end;

function TOrmTable.QueryRecordType: TOrmClass;
begin
  if (self <> nil) and (pointer(fQueryTables) <> nil) then
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
    fOwnedRecords := TSynObjectList.Create;
  fOwnedRecords.Add(result);
end;

{$ifdef ISDELPHI2010} // Delphi 2009/2010 generics are buggy

function TOrmTable.ToObjectList<T>: TObjectList<T>;
var
  R, Item: TOrm;
  row: PPUtf8Char;
  i: integer;
  {$ifdef ISDELPHIXE3}
  rec: POrmArray;
  {$endif ISDELPHIXE3}
begin
  result := TObjectList<T>.Create; // TObjectList<T> will free each T instance
  if (self = nil) or (fRowCount = 0) then
    exit;
  R := TOrmClass(T).Create;
  try
    R.FillPrepare(self);
    row := @fResults[FieldCount]; // row^ points to first row of data
    {$ifdef ISDELPHIXE3}
    result.Count := fRowCount; // faster than manual Add()
    rec := pointer(result.List);
    for i := 0 to fRowCount - 1 do
    begin
      Item := TOrmClass(T).Create;
      rec[i] := Item;
    {$else}
    for i := 1 to fRowCount do
    begin
      Item := TOrmClass(T).Create;
      result.Add(Item);
    {$endif ISDELPHIXE3}
      R.fFill.Fill(pointer(row), Item);
      Item.fInternalState := fInternalState; // set InternalState property
      Inc(row, FieldCount); // next data row
    end;
  finally
    R.Free;
  end;
end;

{$endif ISDELPHI2010}

procedure TOrmTable.ToObjectList(DestList: TObjectList; RecordType: TOrmClass);
var
  R: TOrm;
  row: PPUtf8Char;
  rec: POrm;
  i: integer;
begin
  if DestList = nil then
    exit;
  DestList.Clear;
  if (self = nil) or (fRowCount = 0) then
    exit;
  if RecordType = nil then
  begin
    RecordType := QueryRecordType;
    if RecordType = nil then
      exit;
  end;
  R := RecordType.Create;
  try
    R.FillPrepare(self);
    DestList.Count := fRowCount; // faster than manual Add()
    rec := pointer(DestList.List);
    row := @fResults[FieldCount]; // row^ points to first row of data
    for i := 1 to fRowCount do
    begin
      rec^ := RecordType.Create; // TObjectList will own and free each instance
      R.fFill.Fill(pointer(row), rec^);
      rec^.fInternalState := fInternalState; // set InternalState property
      inc(rec);
      inc(row, FieldCount); // next data row
    end;
  finally
    R.Free;
  end;
end;

function TOrmTable.ToObjArray(var ObjArray; RecordType: TOrmClass): boolean;
var
  R: TOrm;
  Row: PPUtf8Char;
  i: integer;
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
  R := RecordType.Create;
  try
    R.FillPrepare(self);
    SetLength(arr, fRowCount); // faster than manual ObjArrayAdd()
    Row := @fResults[FieldCount]; // Row^ points to first row of data
    for i := 0 to fRowCount - 1 do
    begin
      arr[i] := RecordType.Create;
      R.fFill.Fill(pointer(Row), arr[i]);
      Inc(Row, FieldCount); // next data row
    end;
  finally
    R.Free;
  end;
end;

function TOrmTable.ToObjectList(RecordType: TOrmClass): TObjectList;
begin
  result := TObjectList.Create;
  ToObjectList(result, RecordType);
end;

function TOrmTable.Step(SeekFirst: boolean; RowVariant: PVariant): boolean;
begin
  result := false;
  if (self = nil) or (fRowCount <= 0) then
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
  VarClear(RowVariant^);
  POrmTableRowVariantData(RowVariant)^.VType := OrmTableRowVariantType.VarType;
  POrmTableRowVariantData(RowVariant)^.VTable := self;
  POrmTableRowVariantData(RowVariant)^.VRow := -1; // follow fStepRow
end;

function TOrmTable.FieldBuffer(FieldIndex: integer): PUTF8Char;
begin
  if (self = nil) or (cardinal(FieldIndex) >= cardinal(fFieldCount)) then
    raise EOrmTable.CreateUTF8('%.FieldBuffer(%): invalid index',
      [self, FieldIndex]);
  if (fStepRow = 0) or (fStepRow > fRowCount) then
    raise EOrmTable.CreateUTF8('%.FieldBuffer(%): no previous Step',
      [self, FieldIndex]);
  result := fResults[fStepRow * FieldCount + FieldIndex];
end;

function TOrmTable.FieldBuffer(const FieldName: RawUTF8): PUTF8Char;
var
  i: integer;
begin
  i := FieldIndex(FieldName);
  if i < 0 then
    raise EOrmTable.CreateUTF8('%.FieldBuffer(%): unknown field',
      [self, FieldName]);
  if (fStepRow = 0) or (fStepRow > fRowCount) then
    raise EOrmTable.CreateUTF8('%.FieldBuffer(%): no previous Step',
      [self, FieldName]);
  result := fResults[fStepRow * FieldCount + i];
end;

function TOrmTable.FieldAsInteger(FieldIndex: integer): Int64;
begin
  SetInt64(FieldBuffer(FieldIndex), result{%H-});
end;

function TOrmTable.FieldAsInteger(const FieldName: RawUTF8): Int64;
begin
  SetInt64(FieldBuffer(FieldName), result{%H-});
end;

function TOrmTable.FieldAsFloat(FieldIndex: integer): TSynExtended;
begin
  result := GetExtended(FieldBuffer(FieldIndex));
end;

function TOrmTable.FieldAsFloat(const FieldName: RawUTF8): TSynExtended;
begin
  result := GetExtended(FieldBuffer(FieldName));
end;

function TOrmTable.FieldAsRawUTF8(FieldIndex: integer): RawUTF8;
var
  buf: PUTF8Char;
begin
  buf := FieldBuffer(FieldIndex);
  FastSetString(result, buf, StrLen(buf));
end;

function TOrmTable.FieldAsRawUTF8(const FieldName: RawUTF8): RawUTF8;
var
  buf: PUTF8Char;
begin
  buf := FieldBuffer(FieldName);
  FastSetString(result, buf, StrLen(buf));
end;

function TOrmTable.FieldAsString(FieldIndex: integer): string;
var
  buf: PUTF8Char;
begin
  buf := FieldBuffer(FieldIndex);
  UTF8DecodeToString(buf, StrLen(buf), result);
end;

function TOrmTable.FieldAsString(const FieldName: RawUTF8): string;
var
  buf: PUTF8Char;
begin
  buf := FieldBuffer(FieldName);
  UTF8DecodeToString(buf, StrLen(buf), result);
end;

function TOrmTable.field(FieldIndex: integer): variant;
begin
  if (self = nil) or (cardinal(FieldIndex) >= cardinal(fFieldCount)) then
    raise EOrmTable.CreateUTF8('%.Field(%): invalid index', [self, FieldIndex]);
  if (fStepRow = 0) or (fStepRow > fRowCount) then
    raise EOrmTable.CreateUTF8('%.Field(%): no previous Step',
      [self, FieldIndex]);
  GetVariant(fStepRow, FieldIndex, result);
end;

function TOrmTable.field(const FieldName: RawUTF8): variant;
var
  i: integer;
begin
  i := FieldIndex(FieldName);
  if i < 0 then
    raise EOrmTable.CreateUTF8('%.Field(%): unknown field', [self, FieldName]);
  result := field(i);
end;

function TOrmTable.CalculateFieldLengthMean(var aResult: TIntegerDynArray;
  FromDisplay: boolean = false): integer;

  procedure CalculateEnumerates(F: integer; P: PRttiEnumType);
  var
    R, i, n: integer;
    EnumCounts: array of integer; // slow GetCaption() will be called once
    U: PPUTF8Char;
  begin
    if P = nil then
      exit; // no a true enumerate field
    // 1. count of every possible enumerated value into EnumCounts[]
    SetLength(EnumCounts, P^.MaxValue + 1);
    U := @fResults[FieldCount + F]; // start reading after first Row (= Field Names)
    for R := 1 to fRowCount do
    begin
      n := GetInteger(U^);
      if n <= P^.MaxValue then
        // update count of every enumerated value
        inc(EnumCounts[n])
      else        // GetCaption(invalid index) displays first one
        inc(EnumCounts[0]);
      inc(U, FieldCount); // points to next row
    end;
    // 2. update aResult[F] with displayed caption text length
    n := 0;
    for i := 0 to P^.MaxValue do
      if EnumCounts[i] <> 0 then
        inc(n, length(P^.GetCaption(i)) * EnumCounts[i]);
    aResult[F] := n; // store displayed total length
  end;

var
  R, F, n: integer;
  U: PPUTF8Char;
  Tot: cardinal;
begin
  SetLength(aResult, FieldCount);
  if FromDisplay and (length(fFieldLengthMean) = FieldCount) then
  begin
    MoveFast(fFieldLengthMean[0], aResult[0], FieldCount * SizeOf(integer));
    result := fFieldLengthMeanSum;
    exit;
  end;
  if fRowCount = 0 then
  begin
    // no data: calculate field length from first row (i.e. Field Names)
    U := @fResults[0];
    for F := 0 to FieldCount - 1 do
    begin
      inc(aResult[F], Utf8FirstLineToUnicodeLength(U^));  // count
      inc(U); // points to next value
    end;
    Tot := 1;
  end
  else
  begin
    if fFieldType = nil then
      InitFieldTypes;
    U := @fResults[FieldCount]; // start reading after first Row
    for R := 1 to fRowCount do // sum all lengths by field
      for F := 0 to FieldCount - 1 do
      begin
        case fFieldType[F].ContentType of
          oftInteger, oftBlob, oftBlobCustom, oftUTF8Custom, oftRecord,
          oftRecordVersion, oftID, oftTID, oftSet, oftCurrency:
            inc(aResult[F], 8);
        else
          inc(aResult[F], Utf8FirstLineToUnicodeLength(U^));
        end;
        inc(U); // points to next value
      end;
    if Assigned(fQueryTables) then
    begin
      // aResult[] must be recalculated from captions, if exists
      for F := 0 to FieldCount - 1 do
        with fFieldType[F] do
          case ContentType of
            oftEnumerate:
              CalculateEnumerates(F, ContentTypeInfo);
          end;
    end;
    Tot := fRowCount;
  end;
  result := 0;
  for F := 0 to FieldCount - 1 do
  begin
    n := cardinal(aResult[F]) div Tot; // Mean = total/count
    if n = 0 then
      n := 1;  // none should be 0
    aResult[F] := n;
    inc(result, n); // fast calculate mean sum
  end;
end;

function TOrmTable.FieldLengthMean(Field: integer): cardinal;
begin
  if (self = nil) or (cardinal(Field) >= cardinal(FieldCount)) or (fResults = nil) then
    result := 0
  else
  begin
    if fFieldLengthMean = nil then
      // if not already calculated, do it now
      fFieldLengthMeanSum := CalculateFieldLengthMean(fFieldLengthMean);
    result := fFieldLengthMean[Field];
  end;
end;

function TOrmTable.FieldLengthMeanSum: cardinal;
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

function TOrmTable.FieldLengthMax(Field: integer; NeverReturnsZero: boolean): cardinal;
var
  i: integer;
  len: cardinal;
  U: PPUTF8Char;
begin
  result := 0;
  if (self <> nil) and (cardinal(Field) < cardinal(FieldCount)) then
  begin
    if fFieldType = nil then
      InitFieldTypes;
    with fFieldType[Field] do
      if ContentSize >= 0 then
        // return already computed value
        result := ContentSize
      else
      begin
        if (ContentTypeInfo <> nil) and (ContentType = oftEnumerate) then
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
          // compute by reading all data rows
          U := @fResults[FieldCount + Field];
          for i := 1 to fRowCount do
          begin
            len := StrLen(U^);
            if len > result then
              result := len;
            inc(U, FieldCount);
          end;
        end;
        ContentSize := result;
      end;
  end;
  if (result = 0) and NeverReturnsZero then
    result := 1; // minimal not null length
end;

function TOrmTable.FieldTable(Field: integer): TOrmClass;
begin
  if (self = nil) or (cardinal(Field) >= cardinal(FieldCount)) or
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

procedure TOrmTable.SetFieldLengthMean(const Lengths: array of cardinal);
var
  F: integer;
  n: cardinal;
begin
  if (self = nil) or (length(Lengths) <> FieldCount) then
    exit;
  if fFieldLengthMean = nil then // if not already calculated, allocate array
    SetLength(fFieldLengthMean, FieldCount);
  fFieldLengthMeanSum := 0;
  for F := 0 to FieldCount - 1 do
  begin
    n := Lengths[F];
    if n = 0 then
      n := 1;  // none should be 0
    fFieldLengthMean[F] := n;
    inc(fFieldLengthMeanSum, n); // fast calculate mean sum
  end;
end;

procedure TOrmTable.FieldLengthMeanIncrease(aField, aIncrease: integer);
begin
  if (self = nil) or (cardinal(aField) >= cardinal(FieldCount)) then
    exit; // avoid GPF
  if fFieldLengthMean = nil then
    FieldLengthMean(0); // initialize fFieldLengthMean[] and fFieldLengthMeanSum
  inc(fFieldLengthMean[aField], aIncrease);
  inc(fFieldLengthMeanSum, aIncrease);
end;

function TOrmTable.SearchValue(const UpperValue: RawUTF8;
  StartRow, FieldIndex: integer; const Client: IRestOrm;
  Lang: TSynSoundExPronunciation; UnicodeComparison: boolean): integer;
var
  U: PPUTF8Char;
  Kind: TOrmFieldType;
  Search: PAnsiChar;
  UpperUnicode: RawUnicode;
  UpperUnicodeLen: integer;
  info: POrmTableFieldType;
  Val64: Int64;
  ValTimeLog: TTimelogBits absolute Val64;
  i, err: integer;
  EnumValue: RawUTF8;
  s: string;
  P: PShortString;
  EnumValues: set of 0..63;
  Soundex: TSynSoundEx;
  M: TOrmModel;
  tmp: array[0..23] of AnsiChar;
begin
  result := 0;
  if (self = nil) or (StartRow <= 0) or (StartRow > fRowCount) or
     (UpperValue = '') or (cardinal(FieldIndex) >= cardinal(FieldCount)) then
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
  if ((Lang <> sndxNone) and not Soundex.Prepare(Search, Lang)) then
    exit;
  result := StartRow;
  Kind := FieldType(FieldIndex, info);
  U := @fResults[FieldCount * StartRow + FieldIndex];
  // search in one specified field value
  if (Kind = oftEnumerate) and (info.ContentTypeInfo <> nil) then
  begin
    // for enumerates: first search in all available values
    Int64(EnumValues) := 0;
    P := PRttiEnumType(info.ContentTypeInfo)^.NameList;
    for i := 0 to PRttiEnumType(info.ContentTypeInfo)^.MaxValue do
    begin
      EnumValue := TrimLeftLowerCaseShort(P);
      GetCaptionFromPCharLen(pointer(EnumValue), s);
      StringToUTF8(s, EnumValue);
      if ((Lang <> sndxNone) and Soundex.UTF8(pointer(EnumValue))) or
         ((Lang = sndxNone) and FindUTF8(pointer(EnumValue), Search)) then
        include(EnumValues, i);
      inc(PByte(P), ord(P^[0]) + 1);
    end;
    // then search directly from the INTEGER value
    if Int64(EnumValues) <> 0 then
      while cardinal(result) <= cardinal(fRowCount) do
      begin
        i := GetInteger(U^, err);
        if (err = 0) and (i in EnumValues) then
          exit; // we found a matching field
        inc(U, FieldCount); // ignore all other fields -> jump to next row data
        inc(result);
      end;
    result := 0; // not found
    exit;
  end;
  // special cases: conversion from INTEGER to text before search
  if Kind in [oftTimeLog, oftModTime, oftCreateTime, oftUnixTime, oftUnixMSTime] then
    while cardinal(result) <= cardinal(fRowCount) do
    begin
      SetInt64(U^, Val64{%H-});
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
      inc(U, FieldCount); // ignore all other fields -> jump to next row data
      inc(result);
    end
  else if ((Kind in [oftRecord, oftID, oftTID, oftSessionUserID]) and
          (Client <> nil) and (Client.Model <> nil)) then
  begin
    M := Client.Model;
    while cardinal(result) <= cardinal(fRowCount) do
    begin
      SetInt64(U^, Val64);
      if Val64 <> 0 then
      begin
        if Kind = oftRecord then
          EnumValue := RecordRef(Val64).Text(M)
        else
          EnumValue := U^; // oftID/oftTID -> display ID number -> no sounded
        if Lang = sndxNone then
        begin
          if FindUTF8(pointer(EnumValue), Search) then
            exit;
        end
        else if Soundex.UTF8(pointer(EnumValue)) then
          exit;
      end;
      inc(U, FieldCount); // ignore all other fields -> jump to next row data
      inc(result);
    end
  end
  else  // by default, search as UTF-8 encoded text
  if Lang <> sndxNone then
  begin
    while cardinal(result) <= cardinal(fRowCount) do
      if Soundex.UTF8(U^) then
        exit
      else
      begin
        inc(U, FieldCount); // ignore all other fields -> jump to next row data
        inc(result);
      end;
  end
  else if UnicodeComparison then
  begin
    // slowest but always accurate Unicode comparison
    UpperUnicode := UTF8DecodeToRawUnicodeUI(RawUTF8(Search), @UpperUnicodeLen);
    while cardinal(result) <= cardinal(fRowCount) do
      if FindUnicode(pointer(Utf8DecodeToRawUnicode(U^, 0)),
          pointer(UpperUnicode), UpperUnicodeLen) then
        exit
      else
      begin
        inc(U, FieldCount); // ignore all other fields -> jump to next row data
        inc(result);
      end
  end
  else    // default fast Win1252 search
    while cardinal(result) <= cardinal(fRowCount) do
      if FindUTF8(U^, Search) then
        exit
      else
      begin
        inc(U, FieldCount); // ignore all other fields -> jump to next row data
        inc(result);
      end;
  result := 0; // not found
end;

function TOrmTable.SearchValue(const UpperValue: RawUTF8; StartRow: integer;
  FieldIndex: PInteger; const Client: IRestOrm; Lang: TSynSoundExPronunciation;
  UnicodeComparison: boolean): integer;
var
  F, Row: integer;
begin
  result := 0;
  if (self = nil) or (StartRow <= 0) or (StartRow > fRowCount) or (UpperValue = '') then
    exit;
  // search in all fields values
  for F := 0 to FieldCount - 1 do
  begin
    Row := SearchValue(UpperValue, StartRow, F, Client, Lang, UnicodeComparison);
    if (Row <> 0) and ((result = 0) or (Row < result)) then
    begin
      if FieldIndex <> nil then
        FieldIndex^ := F;
      result := Row;
    end;
  end;
end;

function TOrmTable.SearchFieldEquals(const Value: RawUTF8;
  FieldIndex, StartRow: integer; CaseSensitive: boolean): integer;
begin
  result := SearchFieldEquals(pointer(Value), FieldIndex, StartRow, CaseSensitive);
end;

function TOrmTable.SearchFieldEquals(Value: PUTF8Char;
  FieldIndex, StartRow: integer; CaseSensitive: boolean): integer;
var
  U: PPUTF8Char;
begin
  if (self <> nil) and (Value <> nil) and
     (cardinal(FieldIndex) < cardinal(fFieldCount)) then
  begin
    U := @fResults[FieldCount * StartRow + FieldIndex];
    if CaseSensitive then
      for result := StartRow to fRowCount do
        if StrComp(U^, Value) = 0 then
          exit
        else
          inc(U, FieldCount)
    else
      for result := StartRow to fRowCount do
        if UTF8IComp(U^, Value) = 0 then
          exit
        else
          inc(U, FieldCount);
  end;
  result := 0;
end;

function TOrmTable.SearchFieldIdemPChar(const Value: RawUTF8;
  FieldIndex, StartRow: integer): integer;
var
  U: PPUTF8Char;
  up: RawUTF8;
begin
  if (self <> nil) and (Value <> '') and
     (cardinal(FieldIndex) < cardinal(fFieldCount)) then
  begin
    UpperCaseCopy(Value, up);
    U := @fResults[FieldCount * StartRow + FieldIndex];
    for result := StartRow to fRowCount do
      if IdemPChar(U^, pointer(up)) then
        exit
      else
        inc(U, FieldCount);
  end;
  result := 0;
end;

function TOrmTable.GetVariant(Row, Field: integer): Variant;
begin
  GetVariant(Row, Field, result);
end;

procedure TOrmTable.GetVariant(Row, Field: integer; var result: variant);
var
  aType: TOrmFieldType;
  info: POrmTableFieldType;
  U: PUTF8Char;
begin
  if Row = 0 then // Field Name
    RawUTF8ToVariant(GetU(0, Field), result)
  else
  begin
    aType := FieldType(Field, info);
    U := Get(Row, Field);
    ValueVarToVariant(U, StrLen(U), aType, TVarData(result), true, info.ContentTypeInfo);
  end;
end;

function TOrmTable.GetValue(const aLookupFieldName, aLookupValue,
  aValueFieldName: RawUTF8): variant;
var
  f, r, v: integer;
begin
  SetVariantNull(result);
  f := FieldIndex(aLookupFieldName);
  v := FieldIndex(aValueFieldName);
  if (f < 0) or (v < 0) then
    exit;
  r := SearchFieldEquals(aLookupValue, f);
  if r > 0 then
    GetVariant(r, v, result);
end;

function TOrmTable.ExpandAsString(Row, Field: integer; const Client: IRestOrm;
  out Text: string; const CustomFormat: string): TOrmFieldType;
var
  info: POrmTableFieldType;
  err: integer;
  Value: Int64;
  ValueTimeLog: TTimeLogBits absolute Value;
  ValueDateTime: TDateTime;
  Ref: RecordRef absolute Value;
label
  IsDateTime;
begin // Text was already forced to '' because was defined as "out" parameter
  if Row = 0 then
  begin // Field Name
    result := oftUnknown;
    Text := GetCaption(0, Field);
    exit;
  end;
  result := FieldType(Field, info);
  case result of
    oftDateTime, oftDateTimeMS:
      begin
        Value := Iso8601ToTimeLogPUTF8Char(Get(Row, Field), 0);
IsDateTime:
        if Value <> 0 then
        begin
          ValueDateTime := ValueTimeLog.ToDateTime;
          if CustomFormat <> '' then
          begin
            Text := FormatDateTime(CustomFormat, ValueDateTime);
            if Text <> CustomFormat then
              exit; // valid conversion
          end;
          Text := DateTimeToStr(ValueDateTime); // was i18nText()
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
    oftEnumerate, oftSet, oftRecord, oftID, oftTID, oftRecordVersion, oftSessionUserID,
    oftTimeLog, oftModTime, oftCreateTime, oftUnixTime, oftUnixMSTime:
      begin
        Value := GetInt64(Get(Row, Field), err);
        if err <> 0 then
          // not an integer -> to be displayed as oftUTF8Text
          result := oftUTF8Text
        else
          case result of
            oftEnumerate:
              if info.ContentTypeInfo <> nil then
              begin
                Text := PRttiEnumType(info.ContentTypeInfo)^.GetCaption(Value);
                exit;
              end;
            oftTimeLog, oftModTime, oftCreateTime:
              goto IsDateTime;
            oftUnixTime:
              begin
                ValueTimeLog.FromUnixTime(Value);
                goto IsDateTime;
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
              result := oftUTF8Text; // will display INTEGER field as number }
            oftRecord:
              if (Value <> 0) and (Client <> nil) then // 'TableName ID'
                {$ifdef UNICODE}
                Text := Ansi7ToString(Ref.Text(Client.Model))
                {$else}
                Text := Ref.Text(Client.Model)
                {$endif UNICODE}
              else
                result := oftUTF8Text; // display ID number if no table model
          end;
      end;
  end;
  if Text = '' then
    // returns the value as text by default
    Text := GetString(Row, Field);
end;

function TOrmTable.ExpandAsSynUnicode(Row, Field: integer;
  const Client: IRestOrm; out Text: SynUnicode): TOrmFieldType;
var
  s: string;
begin
  result := ExpandAsString(Row, Field, Client, s);
  StringToSynUnicode(s, Text);
end;

function TOrmTable.GetTimeLog(Row, Field: integer; Expanded: boolean;
  FirstTimeChar: AnsiChar): RawUTF8;
var
  Value: TTimeLogBits;
begin
  SetInt64(Get(Row, Field), {%H-}Value.Value);
  result := Value.Text(Expanded, FirstTimeChar);
end;


{ TOrmTableRowVariant }

function TOrmTableRowVariant.IntGet(var Dest: TVarData; const Instance: TVarData;
  Name: PAnsiChar; NameLen: PtrInt): boolean;
var
  r, f: integer;
  rv: TOrmTableRowVariantData absolute Instance;
begin
  if rv.VTable = nil then
    raise EOrmTable.CreateUTF8('Invalid %.% call', [self, Name]);
  r := rv.VRow;
  if r < 0 then
  begin
    r := rv.VTable.fStepRow;
    if (r = 0) or (r > rv.VTable.fRowCount) then
      raise EOrmTable.CreateUTF8('%.%: no previous Step', [self, Name]);
  end;
  f := rv.VTable.FieldIndex(PUTF8Char(Name));
  result := f >= 0;
  if f >= 0 then
    rv.VTable.GetVariant(r, f, Variant(Dest));
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
      TDocVariantData(Dest) := TDocVariantData(tmp);
    end
    else
      RawUTF8ToVariant(VariantSaveJSON(tmp), Dest, AVarType);
  end;
end;

procedure TOrmTableRowVariant.ToJSON(W: TTextWriter; const Value: variant;
  Escape: TTextWriterKind);
var
  r: integer;
  tmp: variant; // write row via a TDocVariant
begin
  r := TOrmTableRowVariantData(Value).VRow;
  if r < 0 then
    r := TOrmTableRowVariantData(Value).VTable.fStepRow;
  TOrmTableRowVariantData(Value).VTable.ToDocVariant(r, tmp);
  W.AddVariant(tmp, Escape);
end;


{ TOrmTableJSON }

function TOrmTableJSON.PrivateCopyChanged(aJSON: PUTF8Char; aLen: integer;
  aUpdateHash: boolean): boolean;
var
  Hash: cardinal;
begin
  if aUpdateHash then
  begin
    Hash := crc32c(0, pointer(aJSON), aLen);
    result := (fPrivateCopyHash = 0) or (Hash = 0) or (Hash <> fPrivateCopyHash);
    if not result then
      exit;
    fPrivateCopyHash := Hash;
  end
  else
    result := true; // from Create() for better performance on single use
  FastSetString(fPrivateCopy, nil, aLen + 16); // +16 for SSE4.2 read-ahead
  MoveFast(pointer(aJSON)^, pointer(fPrivateCopy)^, aLen + 1); // +1 for trailing #0
end;

function GetFieldCountExpanded(P: PUTF8Char): integer;
var
  EndOfObject: AnsiChar;
begin
  result := 0;
  repeat
    P := GotoNextJSONItem(P, 2, @EndOfObject); // ignore Name+Value items
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

function TOrmTableJSON.ParseAndConvert(Buffer: PUTF8Char; BufferLen: integer): boolean;
var
  i, max, nfield, nrow, resmax, f: integer;
  EndOfObject: AnsiChar;
  P: PUTF8Char;
  wasString: boolean;
begin
  result := false; // error on parsing
  fFieldIndexID := -1;
  if (self = nil) or (Buffer = nil) then
    exit;
  // go to start of object
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
    // 2. initialize and fill fResults[] PPUTF8CharArray memory
    max := (fRowCount + 1) * FieldCount;
    SetLength(fJSONResults, max);
    fResults := @fJSONResults[0];
    // unescape+zeroify JSONData + fill fResults[] to proper place
    dec(max);
    f := 0;
    for i := 0 to max do
    begin
      // get a field
      fJSONResults[i] := GetJSONFieldOrObjectOrArray(P, @wasString, nil, true);
      if (P = nil) and (i <> max) then
        // failure (GetRowCountNotExpanded should have detected it)
        exit;
      if i >= FieldCount then
      begin
        if wasString then
          Include(fFieldParsedAsString, f); // mark column was "string"
        inc(f);
        if f = FieldCount then
          f := 0; // check all rows
      end;
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
    nfield := GetFieldCountExpanded(P);
    if nfield = 0 then
      // invalid data for first row
      exit;
    // 2. get values (assume fields are always the same as in the first object)
    max := nfield; // index to start storing values in fResults[]
    resmax := nfield * 2;
    SetLength(fJSONResults, resmax); // space for field names + 1 data row
    nrow := 0;
    repeat
      // let fJSONResults[] point to unescaped+zeroified JSON values
      for f := 0 to nfield - 1 do
      begin
        if nrow = 0 then
          // get field name from 1st Row
          fJSONResults[f] := GetJSONPropName(P)
        else
          // ignore field name for later rows
          P := GotoNextJSONItem(P);
        // warning: field order if not checked, and should be as expected
        if max >= resmax then
        begin // check space inside loop for GPF security
          resmax := NextGrow(resmax);
          SetLength(fJSONResults, resmax); // enough space for more rows
        end;
        if P = nil then
          // normal end: no more field name
          break;
        fJSONResults[max] := GetJSONFieldOrObjectOrArray(
          P, @wasString, @EndOfObject, true);
        if P = nil then
        begin
          // unexpected end
          nfield := 0;
          break;
        end;
        if wasString then // mark column was "string"
          Include(fFieldParsedAsString, f);
        if (EndOfObject = '}') and (f < nfield - 1) then
        begin
          // allow some missing fields in the input object
          inc(max, nfield - f);
          break;
        end;
        inc(max);
      end;
      if P = nil then
        break; // unexpected end
      if {%H-}EndOfObject <> '}' then
        // data field layout is not consistent: should never happen
        break;
      inc(nrow);
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
    if max <> (nrow + 1) * nfield then
    begin
      // field count must be the same for all objects
      fFieldCount := 0;
      fRowCount := 0;
      exit; // data field layout is not consistent: should never happen
    end;
    // 3. save field pointers to fResults[]
    SetLength(fJSONResults, max); // resize to exact size
    fResults := @fJSONResults[0];
    fFieldCount := nfield;
    fRowCount := nrow;
  end;
  for i := 0 to fFieldCount - 1 do
    if IsRowID(fResults[i]) then
    begin
      fFieldIndexID := i;
      break;
    end;
  result := true; // if we reached here, means successfull conversion from P^
end;

function TOrmTableJSON.UpdateFrom(const aJSON: RawUTF8; var Refreshed: boolean;
  PCurrentRow: PInteger): boolean;
var
  len: integer;
begin
  len := length(aJSON);
  if PrivateCopyChanged(pointer(aJSON), len, {updatehash=}true) then
    if ParseAndConvert(pointer(fPrivateCopy), len) then
    begin
      // parse success from new aJSON data -> need some other update?
      if Assigned(fIDColumn) then
      begin
        // ID column was hidden -> do it again
        Finalize(fIDColumn);
        IDColumnHide;
      end;
      with fSortParams do
        if FieldCount <> 0 then
          // TOrmTable.SortFields() was called -> do it again
          SortFields(FieldIndex, Asc, PCurrentRow, FieldType);
      Refreshed := true;
      result := true;
    end
    else
      // parse error
      result := false
  else
    // data didn't change (fPrivateCopyHash checked)
    result := true;
end;

constructor TOrmTableJSON.Create(const aSQL: RawUTF8; JSONBuffer: PUTF8Char;
  JSONBufferLen: integer);
begin // don't raise exception on error parsing
  inherited Create(aSQL);
  ParseAndConvert(JSONBuffer, JSONBufferLen);
end;

constructor TOrmTableJSON.Create(const aSQL, aJSON: RawUTF8);
var
  len: integer;
begin
  len := length(aJSON);
  PrivateCopyChanged(pointer(aJSON), len, {updatehash=}false);
  Create(aSQL, pointer(fPrivateCopy), len);
end;

constructor TOrmTableJSON.CreateFromTables(
  const Tables: array of TOrmClass; const aSQL: RawUTF8;
  JSONBuffer: PUTF8Char; JSONBufferLen: integer);
begin
  // don't raise exception on error parsing
  inherited CreateFromTables(Tables, aSQL);
  ParseAndConvert(JSONBuffer, JSONBufferLen);
end;

constructor TOrmTableJSON.CreateFromTables(const Tables: array of TOrmClass;
  const aSQL, aJSON: RawUTF8);
var
  len: integer;
begin
  len := length(aJSON);
  PrivateCopyChanged(pointer(aJSON), len, {updatehash=}false);
  CreateFromTables(Tables, aSQL, pointer(fPrivateCopy), len);
end;

constructor TOrmTableJSON.CreateWithColumnTypes(
  const ColumnTypes: array of TOrmFieldType; const aSQL: RawUTF8;
  JSONBuffer: PUTF8Char; JSONBufferLen: integer);
begin
  // don't raise exception on error parsing
  inherited CreateWithColumnTypes(ColumnTypes, aSQL);
  ParseAndConvert(JSONBuffer, JSONBufferLen);
end;

constructor TOrmTableJSON.CreateWithColumnTypes(
  const ColumnTypes: array of TOrmFieldType; const aSQL, aJSON: RawUTF8);
var
  len: integer;
begin
  len := length(aJSON);
  PrivateCopyChanged(pointer(aJSON), len, {updatehash=}false);
  CreateWithColumnTypes(ColumnTypes, aSQL, pointer(fPrivateCopy), len);
end;


{ ------------ TOrm Definition }

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
  if (self = nil) or (aRecord = nil) then
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

procedure TOrmFill.AddMap(aRecord: TOrm; const aFieldName: RawUTF8;
  aIndex: integer);
var
  aFieldIndex: integer;
begin
  if (self <> nil) and (aRecord <> nil) then
    if IsRowID(pointer(aFieldName)) then
      AddMap(aRecord, nil, aIndex)
    else
      with aRecord.RecordProps do
      begin
        aFieldIndex := Fields.IndexByName(aFieldName);
        if aFieldIndex >= 0 then
        begin // only map if column name is a valid field
          include(fTableMapFields, aFieldIndex);
          AddMap(aRecord, Fields.List[aFieldIndex], aIndex);
        end;
      end;
end;

procedure TOrmFill.AddMapSimpleFields(aRecord: TOrm;
  const aProps: array of TOrmPropInfo; var aIndex: integer);
var
  i: integer;
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

function TOrmFill.Fill(aRow: integer): boolean;
begin
  if (self = nil) or (Table = nil) or
     (cardinal(aRow) > cardinal(Table.fRowCount)) then
    result := False
  else
  begin
    Fill(@Table.fResults[aRow * Table.FieldCount]);
    result := True;
  end;
end;

function TOrmFill.Fill(aRow: integer; aDest: TOrm): boolean;
begin
  if (self = nil) or (aDest = nil) or (Table = nil) or
     (cardinal(aRow) > cardinal(Table.fRowCount)) then
    result := False
  else
  begin
    Fill(@Table.fResults[aRow * Table.FieldCount], aDest);
    result := True;
  end;
end;

procedure TOrmFill.Fill(aTableRow: PPUtf8CharArray);
var
  f: PtrInt;
begin
  if (self <> nil) and (aTableRow <> nil) then
    for f := 0 to fTableMapCount - 1 do
      with fTableMap[f] do
        if DestField = nil then
          SetID(aTableRow[TableIndex], Dest.fID)
        else
          DestField.SetValue(Dest, aTableRow[TableIndex],
           TableIndex in fTable.fFieldParsedAsString);
end;

procedure TOrmFill.Fill(aTableRow: PPUtf8CharArray; aDest: TOrm);
var
  f: PtrInt;
begin
  if (self <> nil) and (aTableRow <> nil) then
    for f := 0 to fTableMapCount - 1 do
      with fTableMap[f] do
        if DestField = nil then
          SetID(aTableRow[TableIndex], aDest.fID)
        else
          DestField.SetValue(aDest, aTableRow[TableIndex],
            TableIndex in fTable.fFieldParsedAsString);
end;

procedure TOrmFill.ComputeSetUpdatedFieldBits(Props: TOrmProperties;
  out Bits: TFieldBits);
begin
  if (self <> nil) and (fTable <> nil) and
     (fTableMapRecordManyInstances = nil) then
    // within FillPrepare/FillOne loop: update ID, TModTime and mapped fields
    Bits := fTableMapFields + Props.ComputeBeforeUpdateFieldsBits
  else
    // update all simple/custom fields (also for FillPrepareMany)
    Bits := Props.SimpleFieldsBits[ooUpdate];
end;

procedure TOrmFill.Map(aRecord: TOrm; aTable: TOrmTable;
  aCheckTableName: TOrmCheckTableName);
var
  f: PtrInt;
  ColumnName: PUTF8Char;
  FieldName: RawUTF8;
  Props: TOrmProperties;
begin
  if aTable = nil then // avoid any GPF
    exit;
  fTable := aTable;
  if aTable.fResults = nil then
    exit; // void content
  Props := aRecord.RecordProps;
  for f := 0 to aTable.FieldCount - 1 do
  begin
    ColumnName := aTable.fResults[f];
    if aCheckTableName = ctnNoCheck then
      Utf8ToRawUTF8(ColumnName, FieldName)
    else if IdemPChar(ColumnName, pointer(Props.SQLTableNameUpperWithDot)) then
      Utf8ToRawUTF8(ColumnName + length(Props.SQLTableNameUpperWithDot), FieldName)
    else if aCheckTableName = ctnMustExist then
      continue
    else
      Utf8ToRawUTF8(ColumnName, FieldName);
    AddMap(aRecord, FieldName, f);
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

var
  vmtAutoTableLock: TRTLCriticalSection; // atomic set of the VMT AutoTable entry

class function TOrm.PropsCreate: TOrmProperties;
var
  rtticustom: TRttiCustom;
  vmt: TObject;
begin
  // private sub function for proper TOrm.RecordProps method inlining
  rtticustom := Rtti.RegisterClass(self);
  vmt := PPPointer(PAnsiChar(self) + vmtAutoTable)^^;
  if (rtticustom = nil) or (vmt <> rtticustom) then
    // TOrm.RecordProps expects TRttiCustom in the first slot
    raise EModelException.CreateUTF8('%.RecordProps: vmtAutoTable=% not %',
      [self, vmt, rtticustom]);
  EnterCriticalSection(vmtAutoTableLock);
  try
    result := TOrmProperties(rtticustom.Private); // Private is TOrmProperties
    if Assigned(result) then
      if result.InheritsFrom(TOrmProperties) then
        // registered by a background thread
        exit
      else
        // paranoid
        raise EModelException.CreateUTF8('%.RecordProps: vmtAutoTable=%',
          [self, result]);
    // create the properties information from RTTI
    result := TOrmProperties.Create(self);
    rtticustom.Private := result; // will be owned by this TRttiCustom
    self.InternalDefineModel(result);
  finally
    LeaveCriticalSection(vmtAutoTableLock);
  end;
end;

class function TOrm.RecordProps: TOrmProperties;
begin
  result := PPointer(PAnsiChar(self) + vmtAutoTable)^;
  if result <> nil then
    // we know TRttiCustom is the first slot, and Private is TOrmProperties
    result := TOrmProperties(PRttiCustom(result)^.Private)
  else
    // first time we use this TOrm: generate information from RTTI
    result := PropsCreate;
end;

function TOrm.RecordClass: TOrmClass;
begin
  if self = nil then
    result := nil
  else
    result := POrmClass(self)^;
end;

procedure ManyFieldsCreate(self: TOrm; many: POrmPropInfoRTTIMany);
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
  // no inherited TSynPersistent.Create since vmtAutoTable is set by RecordProps
  with RecordProps do
    if pointer(ManyFields) <> nil then
      // auto-instanciate any TOrmMany instance
      ManyFieldsCreate(self, pointer(ManyFields));
end;

destructor TOrm.Destroy;
var
  i: PtrInt;
  props: TOrmProperties;
begin
  props := RecordProps;
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
          ObjArrayClear(fPropInfo^.GetFieldAddr(self)^);
  inherited Destroy;
end;

constructor TOrm.Create(const aSimpleFields: array of const; aID: TID);
begin
  Create;
  fID := aID;
  if not SimplePropertiesFill(aSimpleFields) then
    raise EOrmException.CreateUTF8('Incorrect %.Create(aSimpleFields) call', [self]);
end;

function TOrm.CreateCopy: TOrm;
var
  f: PtrInt;
begin
  // create new instance
  result := RecordClass.Create;
  // copy properties content
  result.fID := fID;
  with RecordProps do
    for f := 0 to length(CopiableFields) - 1 do
      CopiableFields[f].CopyValue(self, result);
end;

function TOrm.CreateCopy(const CustomFields: TFieldBits): TOrm;
var
  f: PtrInt;
begin
  result := RecordClass.Create;
  // copy properties content
  result.fID := fID;
  with RecordProps do
    for f := 0 to Fields.Count - 1 do
      if (f in CustomFields) and (f in CopiableFieldsBits) then
        Fields.List[f].CopyValue(self, result);
end;

function TOrm.GetNonVoidFields: TFieldBits;
var
  f: PtrInt;
begin
  FillZero(result{%H-});
  with RecordProps do
    for f := 0 to Fields.Count - 1 do
      if (f in CopiableFieldsBits) and not Fields.List[f].IsValueVoid(self) then
        include(result, f);
end;

constructor TOrm.Create(const aClient: IRestOrm; aID: TID;
  ForUpdate: boolean);
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

constructor TOrm.Create(const aClient: IRestOrm; const aSQLWhere: RawUTF8);
begin
  Create;
  if aClient <> nil then
    aClient.Retrieve(aSQLWhere, self);
end;

constructor TOrm.Create(const aClient: IRestOrm;
  const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const);
begin
  Create;
  if aClient <> nil then
    aClient.Retrieve(FormatUTF8(FormatSQLWhere, [], BoundsSQLWhere), self);
end;

constructor TOrm.Create(const aClient: IRestOrm;
  const FormatSQLWhere: RawUTF8; const ParamsSQLWhere, BoundsSQLWhere: array of const);
begin
  Create;
  if aClient <> nil then
    aClient.Retrieve(FormatUTF8(FormatSQLWhere, ParamsSQLWhere, BoundsSQLWhere), self);
end;

constructor TOrm.CreateFrom(const JSONRecord: RawUTF8);
begin
  Create;
  FillFrom(JSONRecord);
end;

constructor TOrm.CreateFrom(P: PUTF8Char);
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
  const FieldName: RawUTF8; Options: TOrmInitializeTableOptions);
var
  f: PtrInt;
begin
  // is not part of TOrmProperties because has been declared as virtual
  if (self <> nil) and (Server <> nil) and
     (Options * INITIALIZETABLE_NOINDEX <> INITIALIZETABLE_NOINDEX) then
  begin
    // ensure ID/RowID column is indexed
    if not (itoNoIndex4ID in Options) then
      if (FieldName = '') or IsRowID(pointer(FieldName)) then
        Server.CreateSQLIndex(self, 'ID', true); // for external tables
    // automatic column indexation of fields which are commonly searched by value
    with RecordProps do
      for f := 0 to Fields.Count - 1 do
        with Fields.List[f] do
          if (FieldName = '') or IdemPropNameU(FieldName, Name) then
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
              Server.CreateSQLIndex(self, Name, false);
  end;
  // failure in above Server.CreateSQLIndex() are ignored (may already exist)
end;

procedure TOrm.FillFrom(aRecord: TOrm);
begin
  if (self <> nil) and (aRecord <> nil) then
    FillFrom(aRecord, aRecord.RecordProps.CopiableFieldsBits);
end;

procedure TOrm.FillFrom(aRecord: TOrm; const aRecordFieldBits: TFieldBits);
var
  i, f: PtrInt;
  S, D: TOrmProperties;
  SP: TOrmPropInfo;
  wasString: boolean;
  tmp: RawUTF8;
begin
  if (self = nil) or (aRecord = nil) or IsZero(aRecordFieldBits) then
    exit;
  D := RecordProps;
  if POrmClass(aRecord)^.InheritsFrom(POrmClass(self)^) then
  begin
    // fast atttribution for two sibbling classes
    if POrmClass(aRecord)^ = POrmClass(self)^ then
      fID := aRecord.fID; // same class -> ID values will match
    for f := 0 to D.Fields.Count - 1 do
      if f in aRecordFieldBits then
        D.Fields.List[f].CopyValue(aRecord, self);
    exit;
  end;
  // two diverse tables -> don't copy ID, and per-field lookup
  S := aRecord.RecordProps;
  for i := 0 to S.Fields.Count - 1 do
    if i in aRecordFieldBits then
    begin
      SP := S.Fields.List[i];
      if D.Fields.List[i].Name = SP.Name then
        // optimistic match
        f := i
      else
        f := D.Fields.IndexByName(SP.Name);
      if f >= 0 then
      begin
        SP.GetValueVar(aRecord, False, tmp, @wasString);
        D.Fields.List[f].SetValueVar(self, tmp, wasString);
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

procedure TOrm.FillFrom(const JSONTable: RawUTF8; Row: integer);
var
  Table: TOrmTableJSON;
  tmp: TSynTempBuffer; // work on a private copy
begin
  tmp.Init(JSONTable);
  try
    Table := TOrmTableJSON.Create('', tmp.buf, tmp.len);
    try
      FillFrom(Table, Row);
    finally
      Table.Free;
    end;
  finally
    tmp.Done;
  end;
end;

procedure TOrm.FillFrom(const JSONRecord: RawUTF8; FieldBits: PFieldBits);
var
  tmp: TSynTempBuffer; // work on a private copy
begin
  tmp.Init(JSONRecord);
  try
    FillFrom(tmp.buf, FieldBits); // now we can safely call FillFrom()
  finally
    tmp.Done;
  end;
end;

procedure TOrm.FillFrom(P: PUTF8Char; FieldBits: PFieldBits);
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
  F: array[0..MAX_SQLFIELDS - 1] of PUTF8Char; // store field/property names
  wasString: boolean;
  i, n: PtrInt;
  Prop, Value: PUTF8Char;
begin
  if FieldBits <> nil then
    FillZero(FieldBits^);
  // go to start of object
  if P = nil then
    exit;
  while P^ <> '{' do
    if P^ = #0 then
      exit
    else
      inc(P);
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
      F[i] := GetJSONField(P, P);
    for i := 0 to n do
    begin
      // set properties from values using efficient TOrmPropInfo.SetValue()
      Value := GetJSONFieldOrObjectOrArray(P, @wasString, nil, true);
      FillValue({%H-}F[i], Value, wasString, FieldBits);
    end;
  end
  else if P^ = '{' then
  begin
    // expanded format: check each property name
    inc(P);
    repeat
      Prop := GetJSONPropName(P);
      if (Prop = nil) or (P = nil) then
        break;
      // set each property from values using efficient TOrmPropInfo.SetValue()
      Value := GetJSONFieldOrObjectOrArray(P, @wasString, nil, true);
      FillValue(Prop, Value, wasString, FieldBits);
    until P = nil;
  end;
end;

procedure TOrm.FillFrom(const aDocVariant: variant);
var
  json: RawUTF8;
begin
  if _Safe(aDocVariant)^.Kind = dvObject then
  begin
    VariantSaveJSON(aDocVariant, twJSONEscape, json);
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

function TOrm.FillPrepare(const aClient: IRestOrm;
  const aSQLWhere: RawUTF8; const aCustomFieldsCSV: RawUTF8;
  aCheckTableName: TOrmCheckTableName): boolean;
var
  T: TOrmTable;
begin
  result := false;
  FillClose; // so that no further FillOne will work
  if (self = nil) or (aClient = nil) then
    exit;
  T := aClient.MultiFieldValues(RecordClass, aCustomFieldsCSV, aSQLWhere);
  if T = nil then
    exit;
  T.OwnerMustFree := true;
  FillPrepare(T, aCheckTableName);
  result := true;
end;

function TOrm.FillPrepare(const aClient: IRestOrm;
  const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const;
  const aCustomFieldsCSV: RawUTF8): boolean;
var
  sqlwhere: RawUTF8;
begin
  sqlwhere := FormatUTF8(FormatSQLWhere, [], BoundsSQLWhere);
  result := FillPrepare(aClient, sqlwhere, aCustomFieldsCSV);
end;

function TOrm.FillPrepare(const aClient: IRestOrm;
  const FormatSQLWhere: RawUTF8; const ParamsSQLWhere, BoundsSQLWhere: array of const;
  const aCustomFieldsCSV: RawUTF8): boolean;
var
  sqlwhere: RawUTF8;
begin
  sqlwhere := FormatUTF8(FormatSQLWhere, ParamsSQLWhere, BoundsSQLWhere);
  result := FillPrepare(aClient, sqlwhere, aCustomFieldsCSV);
end;

function TOrm.FillPrepare(const aClient: IRestOrm;
  const aIDs: array of Int64; const aCustomFieldsCSV: RawUTF8): boolean;
begin
  if high(aIDs) < 0 then
    result := false
  else
    result := FillPrepare(aClient, SelectInClause('id', aIDs, '', INLINED_MAX),
      aCustomFieldsCSV);
end;

function TOrm.FillRow(aRow: integer; aDest: TOrm): boolean;
begin
  if self <> nil then
    if aDest = nil then
      result := fFill.Fill(aRow)
    else if fFill.fTableMapRecordManyInstances = nil then
      result := fFill.Fill(aRow, aDest)
    else
      raise EOrmException.CreateUTF8(
        '%.FillRow() forbidden after FillPrepareMany', [self])
  else
    result := false;
end;

function TOrm.FillOne(aDest: TOrm): boolean;
begin
  if (self = nil) or (fFill = nil) or (fFill.Table = nil) or
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
  if (self = nil) or (fFill = nil) or (fFill.Table = nil) or
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

procedure TOrm.AppendFillAsJsonValues(W: TJSONSerializer);
begin
  W.Add('[');
  while FillOne do
  begin
    GetJSONValues(W);
    W.Add(',');
  end;
  W.CancelLastComma;
  W.Add(']');
end;

procedure TOrm.FillValue(PropName: PUTF8Char; Value: PUTF8Char;
  wasString: boolean; FieldBits: PFieldBits);
var
  field: TOrmPropInfo;
begin
  if self <> nil then
    if IsRowID(PropName) then
      SetID(Value, fID)
    else
    begin
      field := RecordProps.Fields.ByName(PropName);
      if field <> nil then
      begin
        field.SetValue(self, Value, wasString);
        if FieldBits <> nil then
          Include(FieldBits^, field.PropertyIndex);
      end;
    end;
end;

function TOrm.SetFieldSQLVars(const Values: TSQLVarDynArray): boolean;
var
  max, field: PtrInt;
begin
  result := false;
  max := length(Values) - 1;
  with RecordProps do
  begin
    // expect exact Values[] type match with FieldType[]
    if max <> Fields.Count - 1 then // must match field count
      exit
    else
      for field := 0 to max do
        if Fields.List[field].SQLDBFieldType <> Values[field].VType then
          exit;
    // now we can safely update field values
    for field := 0 to max do
      Fields.List[field].SetFieldSQLVar(self, Values[field]);
  end;
  result := true;
end;

procedure TOrm.GetBinaryValues(W: TBufferWriter);
var
  f: PtrInt;
begin
  with RecordProps do
    for f := 0 to Fields.Count - 1 do
      Fields.List[f].GetBinary(self, W);
end;

procedure TOrm.GetBinaryValuesSimpleFields(W: TBufferWriter);
var
  f: PtrInt;
begin
  with RecordProps do
    for f := 0 to SimpleFieldCount - 1 do
      SimpleFields[f].GetBinary(self, W);
end;

procedure TOrm.GetBinaryValues(W: TBufferWriter;
  const aFields: TFieldBits);
var
  f: PtrInt;
begin
  with RecordProps do
    for f := 0 to Fields.Count - 1 do
      if f in aFields then
        Fields.List[f].GetBinary(self, W);
end;

function TOrm.GetBinary: RawByteString;
var
  W: TBufferWriter;
begin
  W := TBufferWriter.Create(TRawByteStringStream);
  try
    W.WriteVarUInt64(fID);
    GetBinaryValues(W);
    result := W.FlushTo;
  finally
    W.Free;
  end;
end;

function TOrm.SetBinary(P, PEnd: PAnsiChar): boolean;
begin
  P := pointer(FromVarUInt64Safe(pointer(P), pointer(PEnd), PQWord(@fID)^));
  result := SetBinaryValues(P, PEnd);
end;

function TOrm.SetBinary(const binary: RawByteString): boolean;
begin
  result := SetBinary(pointer(binary), PAnsiChar(pointer(binary)) + length(binary));
end;

function TOrm.SetBinaryValues(var P: PAnsiChar; PEnd: PAnsiChar): boolean;
var
  f: PtrInt;
begin
  result := false;
  if P = nil then
    exit; // on error
  with RecordProps do
    for f := 0 to Fields.Count - 1 do
    begin
      P := Fields.List[f].SetBinary(self, P, PEnd);
      if P = nil then
        exit;
    end;
  result := true;
end;

function TOrm.SetBinaryValuesSimpleFields(var P: PAnsiChar;
  PEnd: PAnsiChar): boolean;
var
  f: PtrInt;
begin
  result := false;
  with RecordProps do
    for f := 0 to SimpleFieldCount - 1 do
    begin
      P := SimpleFields[f].SetBinary(self, P, PEnd);
      if P = nil then
        exit; // on error
    end;
  result := true;
end;

procedure TOrm.GetJSONValues(W: TJSONSerializer);
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
    W.Add(',');
    if (jwoID_str in W.fOrmOptions) and W.Expand then
    begin
      W.AddShort('"ID_str":"');
      W.Add(fID);
      W.Add('"', ',');
    end;
    inc(c);
  end;
  if W.Fields <> nil then
  begin
    Props := RecordProps.Fields;
    for f := 0 to length(W.Fields) - 1 do
    begin
      if W.Expand then
      begin
        W.AddString(W.ColNames[c]); // '"'+ColNames[]+'":'
        inc(c);
      end;
      Props.List[W.Fields[f]].GetJSONValues(self, W);
      W.Add(',');
    end;
  end;
  W.CancelLastComma; // cancel last ','
  if W.Expand then
    W.Add('}');
end;

procedure TOrm.AppendAsJsonObject(W: TJSONSerializer; Fields: TFieldBits);
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
  P := RecordProps;
  if IsZero(Fields) then
    Fields := P.SimpleFieldsBits[ooSelect];
  Props := P.Fields;
  for i := 0 to Props.Count - 1 do
    if i in Fields then
    begin
      W.Add(',', '"');
      W.AddNoJSONEscape(pointer(Props.List[i].Name), length(Props.List[i].Name));
      W.Add('"', ':');
      Props.List[i].GetJSONValues(self, W);
    end;
  W.Add('}');
end;

procedure TOrm.AppendFillAsJsonArray(const FieldName: RawUTF8;
  W: TJSONSerializer; const Fields: TFieldBits);
begin
  if FieldName <> '' then
    W.AddFieldName(FieldName);
  W.Add('[');
  while FillOne do
  begin
    AppendAsJsonObject(W, Fields);
    W.Add(',');
  end;
  W.CancelLastComma;
  W.Add(']');
  if FieldName <> '' then
    W.Add(',');
end;

procedure TOrm.ForceVariantFieldsOptions(aOptions: TDocVariantOptions);
var
  i: PtrInt;
  p: TOrmPropInfo;
begin
  if self <> nil then
    with RecordProps do
      if oftVariant in HasTypeFields then
        for i := 0 to Fields.Count - 1 do
        begin
          p := Fields.List[i];
          if (p.OrmFieldType = oftVariant) and
             p.InheritsFrom(TOrmPropInfoRTTIVariant) then
            with TOrmPropInfoRTTIVariant(p) do
              if PropInfo.GetterIsField then
                with _Safe(PVariant(PropInfo.GetterAddr(self))^)^ do
                  if Count > 0 then
                    options := aOptions;
        end;
end;

procedure TOrm.GetJSONValuesAndFree(JSON: TJSONSerializer);
begin
  if JSON <> nil then
  try
    // write the row data
    GetJSONValues(JSON);
    // end the JSON object
    if not JSON.Expand then
      JSON.AddNoJSONEscape(PAnsiChar(']}'), 2);
    JSON.FlushFinal;
  finally
    JSON.Free;
  end;
end;

procedure TOrm.GetJSONValues(JSON: TStream; Expand, withID: boolean;
  Occasion: TOrmOccasion; OrmOptions: TJSONSerializerOrmOptions);
var
  serializer: TJSONSerializer;
begin
  if self = nil then
    exit;
  with RecordProps do
    serializer := CreateJSONWriter(JSON, Expand, withID, SimpleFieldsBits[Occasion],
      {knownrows=}0);
  serializer.OrmOptions := OrmOptions;
  GetJSONValuesAndFree(serializer);
end;

function TOrm.GetJSONValues(Expand, withID: boolean;
  const Fields: TFieldBits; OrmOptions: TJSONSerializerOrmOptions): RawUTF8;
var
  J: TRawByteStringStream;
  serializer: TJSONSerializer;
begin
  J := TRawByteStringStream.Create;
  try
    serializer := RecordProps.CreateJSONWriter(J, Expand, withID, Fields, {knownrows=}0);
    serializer.OrmOptions := OrmOptions;
    GetJSONValuesAndFree(serializer);
    result := J.DataString;
  finally
    J.Free;
  end;
end;

function TOrm.GetJSONValues(Expand, withID: boolean;
  const FieldsCSV: RawUTF8; OrmOptions: TJSONSerializerOrmOptions): RawUTF8;
var
  bits: TFieldBits;
begin
  if RecordProps.FieldBitsFromCSV(FieldsCSV, bits) then
    result := GetJSONValues(Expand, withID, bits, OrmOptions)
  else
    result := '';
end;

function TOrm.GetJSONValues(Expand, withID: boolean;
  Occasion: TOrmOccasion; UsingStream: TCustomMemoryStream;
  OrmOptions: TJSONSerializerOrmOptions): RawUTF8;
var
  J: TRawByteStringStream;
begin
  if not withID and IsZero(RecordProps.SimpleFieldsBits[Occasion]) then
    // no simple field to write -> quick return
    result := ''
  else if UsingStream <> nil then
  begin
    UsingStream.Seek(0, soFromBeginning);
    GetJSONValues(UsingStream, Expand, withID, Occasion, OrmOptions);
    FastSetString(result, UsingStream.Memory, UsingStream.Seek(0, soFromCurrent));
  end
  else
  begin
    J := TRawByteStringStream.Create;
    try
      GetJSONValues(J, Expand, withID, Occasion, OrmOptions);
      result := J.DataString;
    finally
      J.Free;
    end;
  end;
end;

class function TOrm.GetSQLCreate(aModel: TOrmModel): RawUTF8;
// not implemented in TOrmProperties since has been made virtual
var
  i: PtrInt;
  c: TClass;
  SQL, mname, cname, tokenizer: RawUTF8;
  M: TClass; // is a TOrmVirtualTableClass
  Props: TOrmModelProperties;
  fields: TOrmPropInfoList;
begin
  if aModel = nil then
    raise EModelException.CreateUTF8('Invalid %.GetSQLCreate(nil) call', [self]);
  Props := aModel.Props[self];
  if Props.Kind <> rSQLite3 then
  begin
    // create a FTS3/FTS4/RTREE virtual table
    result := 'CREATE VIRTUAL TABLE ' + SQLTableName + ' USING ';
    case Props.Kind of
      rFTS3:
        result := result + 'fts3(';
      rFTS4:
        result := result + 'fts4(';
      rFTS5:
        result := result + 'fts5(';
      rRTree:
        result := result + 'rtree(RowID,';
      rRTreeInteger:
        result := result + 'rtree_i32(RowID,';
      rCustomForcedID, rCustomAutoID:
        begin
          M := aModel.VirtualTableModule(self);
          if (M = nil) or not Assigned(GetVirtualTableModuleName) then
            raise EModelException.CreateUTF8('No registered module for %', [self]);
          mname := GetVirtualTableModuleName(M);
          if Props.Props.Fields.Count = 0 then
            raise EModelException.CreateUTF8(
              'Virtual % % should have published properties', [mname, self]);
          result := result + mname + '(';
        end;
    else
      raise EModelException.CreateUTF8('%.GetSQLCreate(%)?', [self, ToText(Props.Kind)^]);
    end;
    fields := Props.Props.Fields;
    case Props.Kind of
      rFTS3, rFTS4, rFTS5:
        begin
          if (Props.fFTSWithoutContentFields <> '') and
             (Props.fFTSWithoutContentTableIndex >= 0) then
          begin
            result := FormatUTF8('%content="%",',
              [result, aModel.Tables[Props.fFTSWithoutContentTableIndex].SQLTableName]);
            if Props.Kind = rFTS5 then
              result := FormatUTF8('%content_rowid="ID",', [result]);
          end;
          for i := 0 to fields.Count - 1 do
            result := result + fields.List[i].Name + ',';
          tokenizer := 'simple';
          c := self;
          repeat
            ClassToText(c, cname); // TOrmFtsTest = class(TOrmFts3Porter)
            if IdemPChar(pointer(cname), 'TOrmFts') and
               (cname[14] in ['3', '4', '5']) then
            begin
              if length(cname) > 14 then
                tokenizer := copy(cname, 15, 100); // e.g. TOrmFts3Porter -> 'Porter'
              break;
            end;
            c := GetClassParent(c);
          until c = TOrm;
          result := FormatUTF8('% tokenize=%)', [result, LowerCaseU(tokenizer)]);
        end;
      rRTree, rRTreeInteger:
        begin
          for i := 0 to fields.Count - 1 do
            with fields.List[i] do
              if aAuxiliaryRTreeField in Attributes then // for SQlite3 >= 3.24.0
                result := FormatUTF8('%+% %', [result, Name,
                  Props.Props.OrmFieldTypeToSQL(i)])
              else
                result := result + Name + ',';
          result[length(result)] := ')';
        end;
      rCustomForcedID, rCustomAutoID:
        result := result + GetVirtualTableSQLCreate(Props.Props);
    end;
  end
  else
  begin
    // inherits from TOrm: create a "normal" SQLite3 table
    result := 'CREATE TABLE ' + SQLTableName + '(ID INTEGER PRIMARY KEY AUTOINCREMENT, ';
    // we always add an ID field which is an INTEGER PRIMARY KEY
    // column, as it is always created (as hidden RowID) by the SQLite3 engine
    with Props.Props do
      for i := 0 to fields.Count - 1 do
        with fields.List[i] do
        begin
          SQL := OrmFieldTypeToSQL(i); // = '' for field with no matching DB column
          if SQL <> '' then
          begin
            result := result + Name + SQL;
            if i in IsUniqueFieldsBits then
              insert(' UNIQUE', result, length(result) - 1);
          end;
        end;
    PWord(@result[length(result) - 1])^ := ord(')') + ord(';') shl 8;
  end;
end;

function TOrm.GetSQLSet: RawUTF8;
var
  i: PtrInt;
  V: RawUTF8;
  wasString: boolean;
begin
  result := '';
  if self = nil then
    exit;
  with RecordProps do
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

function TOrm.GetSQLValues: RawUTF8;
var
  i: PtrInt;
  V: RawUTF8;
  wasString: boolean;
begin
  result := '';
  if self <> nil then
    with RecordProps do
      if SimpleFields = nil then
        exit
      else
      begin
        if HasNotSimpleFields then // get 'COL1,COL2': no 'ID,' for INSERT (false below)
          result := SQLTableSimpleFieldsNoRowID; // always <> '*'
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
  if (self = nil) or (Reference = nil) or
     (POrmClass(Reference)^ <> POrmClass(self)^) or
     (Reference.fID <> fID) then
    exit;
  with RecordProps do
    for i := 0 to length(SimpleFields) - 1 do      // compare not RawBlob/TOrmMany fields
      with SimpleFields[i] do
        if CompareValue(self, Reference, false) <> 0 then
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
  if (self = nil) or (Reference = nil) or
     (Reference.fID <> fID) then // ID field must be tested by hand
    exit;
  if self <> Reference then
    if POrmClass(Reference)^ = POrmClass(self)^ then
    begin
      // faster comparison on same exact class
      with RecordProps do
        for i := 0 to length(SimpleFields) - 1 do      // compare not RawBlob/TOrmMany fields
          with SimpleFields[i] do
            if CompareValue(self, Reference, false) <> 0 then
              exit; // properties don't have the same value
    end
    else
    begin
      // comparison of all properties of Reference against self
      This := RecordProps;
      Ref := Reference.RecordProps;
      for i := 0 to length(Ref.SimpleFields) - 1 do
        with Ref.SimpleFields[i] do
        begin
          // compare not RawBlob/TOrmMany fields
          O := This.Fields.ByRawUTF8Name(Name);
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
  with RecordProps do
    if fFill.JoinedFields then
    begin
      for i := 0 to length(CopiableFields) - 1 do
        if CopiableFields[i].OrmFieldType <> oftID then
          CopiableFields[i].SetValue(self, nil, false)
        else
          // clear nested allocated TOrm
          TOrm(TOrmPropInfoRTTIInstance(CopiableFields[i]).GetInstance(self)).
            ClearProperties;
    end
    else
      for i := 0 to length(CopiableFields) - 1 do
        CopiableFields[i].SetValue(self, nil, false);
end;

procedure TOrm.ClearProperties(const aFieldsCSV: RawUTF8);
var
  bits: TFieldBits;
  f: PtrInt;
begin
  if (self = nil) or (aFieldsCSV = '') then
    exit;
  with RecordProps do
  begin
    if aFieldsCSV = '*' then
      bits := SimpleFieldsBits[ooInsert]
    else if not FieldBitsFromCSV(aFieldsCSV, bits) then
      exit;
    for f := 0 to Fields.Count - 1 do
      if (f in bits) and (Fields.List[f].OrmFieldType in COPIABLE_FIELDS) then
        Fields.List[f].SetValue(self, nil, false); // clear field value
  end;
end;

function TOrm.ClassProp: TRttiJson;
begin
  if self <> nil then
    result := ClassPropertiesGet(PClass(self)^, TRttiJson)
  else
    result := nil; // avoid GPF
end;

function TOrm.RecordReference(Model: TOrmModel): TRecordReference;
begin
  if (self = nil) or (fID <= 0) then
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
  tmp: RawUTF8;
begin
  if self = nil then
    result := false
  else // means error
    with RecordProps do
      if length(SimpleFields) <> length(aSimpleFields) then
        result := false
      else
      begin
        for i := 0 to high(aSimpleFields) do
        begin
          VarRecToUTF8(aSimpleFields[i], tmp); // will work for every handled type
          SimpleFields[i].SetValueVar(self, tmp, false);
        end;
        result := True;
      end;
end;

constructor TOrm.CreateAndFillPrepare(const aClient: IRestOrm;
  const aSQLWhere: RawUTF8; const aCustomFieldsCSV: RawUTF8);
var
  aTable: TOrmTable;
begin
  Create;
  aTable := aClient.MultiFieldValues(RecordClass, aCustomFieldsCSV, aSQLWhere);
  if aTable = nil then
    exit;
  aTable.OwnerMustFree := true;
  FillPrepare(aTable);
end;

constructor TOrm.CreateAndFillPrepare(const aClient: IRestOrm;
  const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const;
  const aCustomFieldsCSV: RawUTF8);
var
  where: RawUTF8;
begin
  where := FormatUTF8(FormatSQLWhere, [], BoundsSQLWhere);
  CreateAndFillPrepare(aClient, where, aCustomFieldsCSV);
end;

constructor TOrm.CreateAndFillPrepare(const aClient: IRestOrm;
  const FormatSQLWhere: RawUTF8; const ParamsSQLWhere, BoundsSQLWhere: array of const;
  const aCustomFieldsCSV: RawUTF8);
var
  where: RawUTF8;
begin
  where := FormatUTF8(FormatSQLWhere, ParamsSQLWhere, BoundsSQLWhere);
  CreateAndFillPrepare(aClient, where, aCustomFieldsCSV);
end;

constructor TOrm.CreateAndFillPrepare(const aClient: IRestOrm;
  const aIDs: array of Int64; const aCustomFieldsCSV: RawUTF8);
begin
  Create;
  FillPrepare(aClient, aIDs, aCustomFieldsCSV);
end;

constructor TOrm.CreateAndFillPrepare(const aJSON: RawUTF8);
var
  aTable: TOrmTable;
begin
  Create;
  aTable := TOrmTableJSON.CreateFromTables([RecordClass], '', aJSON);
  aTable.OwnerMustFree := true;
  FillPrepare(aTable);
end;

constructor TOrm.CreateAndFillPrepare(aJSON: PUTF8Char; aJSONLen: integer);
var
  aTable: TOrmTable;
begin
  Create;
  aTable := TOrmTableJSON.CreateFromTables([RecordClass], '', aJSON, aJSONLen);
  aTable.OwnerMustFree := true;
  FillPrepare(aTable);
end;

constructor TOrm.CreateAndFillPrepareJoined(const aClient: IRestOrm;
  const aFormatSQLJoin: RawUTF8; const aParamsSQLJoin, aBoundsSQLJoin: array of const);
var
  i: PtrInt;
  n: integer;
  props: TOrmModelProperties;
  T: TOrmTable;
  instance: TOrm;
  SQL: RawUTF8;
begin
  Create;
  props := aClient.Model.Props[POrmClass(self)^];
  if props.props.JoinedFields = nil then
    raise EModelException.CreateUTF8('No nested TOrm to JOIN in %', [self]);
  SQL := props.SQL.SelectAllJoined;
  if aFormatSQLJoin <> '' then
    SQL := SQL + FormatUTF8(SQLFromWhere(aFormatSQLJoin), aParamsSQLJoin, aBoundsSQLJoin);
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
        JoinedFieldsTable[i].RecordProps.SimpleFields, n);
    end;
  end;
  fFill.fFillCurrentRow := 1; // point to first data row (0 is field names)
end;

constructor TOrm.CreateJoined(const aClient: IRestOrm; aID: TID);
begin
  CreateAndFillPrepareJoined(aClient, '%.RowID=?', [RecordProps.SQLTableName], [aID]);
  FillOne;
end;

constructor TOrm.CreateAndFillPrepareMany(const aClient: IRestOrm;
  const aFormatSQLJoin: RawUTF8; const aParamsSQLJoin, aBoundsSQLJoin: array of const);
begin
  Create;
  if Length(RecordProps.ManyFields) = 0 then
    raise EModelException.CreateUTF8(
      '%.CreateAndFillPrepareMany() with no many-to-many fields', [self]);
  if not FillPrepareMany(aClient, aFormatSQLJoin, aParamsSQLJoin, aBoundsSQLJoin) then
    raise EModelException.CreateUTF8(
      '%.CreateAndFillPrepareMany(): FillPrepareMany() failure', [self]);
end;

{$ifdef ISDELPHI20062007}
  {$warnings off} // avoid paranoid Delphi 2007 warning
{$endif ISDELPHI20062007}

function TOrm.EnginePrepareMany(const aClient: IRestOrm;
  const aFormatSQLJoin: RawUTF8; const aParamsSQLJoin, aBoundsSQLJoin: array of const;
  out ObjectsClass: TOrmClassDynArray; out SQL: RawUTF8): RawUTF8;
var
  aSQLFields, aSQLFrom, aSQLWhere, aSQLJoin: RawUTF8;
  aField: string[3];
  aMany: RawUTF8;
  f, n, i, SQLFieldsCount: integer;
  Props: TOrmProperties;
  SQLFields: array of record
    SQL: string[3];
    prop: TOrmPropInfo;
    Instance: TOrm;
  end;
  M: TOrmMany;
  D: TOrm;
  J, JBeg: PUTF8Char;
  Objects: array of TOrm;

  function AddField(aProp: TOrmPropInfo): boolean;
  begin
    if SQLFieldsCount >= MAX_SQLFIELDS then
      result := false
    else
      with SQLFields[SQLFieldsCount] do
      begin
        SQL := aField;
        prop := aProp;
        Instance := Objects[f];
        inc(SQLFieldsCount);
        result := true;
      end;
  end;

  function ProcessField(var P: PUTF8Char): RawUTF8;
  var
    B: PUTF8Char;
    field: TOrmPropInfo;
    i: PtrInt;
    M: TOrmMany;
    aManyField: string[63];

    function GetManyField(F: PUTF8Char): boolean;
    var
      B: PUTF8Char;
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
    if (result = '') or IdemPropNameU(result, 'AND') or
       IdemPropNameU(result, 'OR')  or IdemPropNameU(result, 'LIKE') or
       IdemPropNameU(result, 'NOT') or IdemPropNameU(result, 'NULL') then
      exit;
    if not IsRowID(pointer(result)) then
    begin
      i := Props.Fields.IndexByName(result);
      if i < 0 then
        exit;
      field := Props.Fields.List[i];
      if field.OrmFieldType = oftMany then
      begin
        M := TOrmPropInfoRTTIInstance(field).GetInstance(self) as TOrmMany;
        for i := 0 to n - 1 do
          if Objects[i * 2 + 1] = M then
          begin
            if IdemPChar(P, '.DEST.') then
            begin // special case of Many.Dest.*
              if GetManyField(P + 6) then
              begin
                aManyField[1] := AnsiChar(i * 2 + 67);
                result := RawUTF8(aManyField);
                exit; // Categories.Dest.Name=? -> C.Name=?
              end;
            end
            else if (P^ = '.') and GetManyField(P + 1) then
            begin
              aManyField[1] := AnsiChar(i * 2 + 66);
              result := RawUTF8(aManyField);
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
  if (self = nil) or (aClient = nil) then
    exit;
  // reset TOrmFill object
  if fFill = nil then
    fFill := TOrmFill.Create
  else
    fFill.UnMap;
  // compute generic joined SQL statement and initialize Objects*[]+SQLFields[]
  SetLength(SQLFields, MAX_SQLFIELDS);
  Props := RecordProps;
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
      raise EOrmException.CreateUTF8('%.Create should have created %:% for EnginePrepareMany',
        [self, Props.ManyFields[f].Name, Props.ManyFields[f].ObjectClass]);
    fFill.fTableMapRecordManyInstances[f] := M;
    Objects[f * 2 + 1] := M;
    ObjectsClass[f * 2 + 1] := POrmClass(M)^;
    with M.RecordProps do
    begin
      if (fRecordManySourceProp.ObjectClass <> PClass(self)^) or
         (fRecordManyDestProp.ObjectClass = nil) then
        raise EOrmException.CreateUTF8('%.EnginePrepareMany %:% mismatch',
          [self, Props.ManyFields[f].Name, Props.ManyFields[f].ObjectClass]);
      ObjectsClass[f * 2 + 2] := TOrmClass(fRecordManyDestProp.ObjectClass);
      D := TOrmClass(fRecordManyDestProp.ObjectClass).Create;
      // let TOrmMany.Source and Dest point to real instances
      M.fSourceID^ := PtrInt(self);
      M.fDestID^ := PtrInt(D);
    end;
    Objects[f * 2 + 2] := TOrm(M.fDestID^);
    if Props.fSQLFillPrepareMany = '' then
    begin
      aMany := AnsiChar(f * 2 + 66); // Many=B,D,F...
      if {%H-}aSQLWhere <> '' then
        aSQLWhere := aSQLWhere + ' and ';
      aSQLWhere := FormatUTF8('%%.Source=A.RowID and %.Dest=%.RowID',
        [aSQLWhere, aMany, aMany, AnsiChar(f * 2 + 67){Dest=C,E,G..}]);
    end;
  end;
  SQLFieldsCount := 0;
  aField := 'A00';
  for f := 0 to length(ObjectsClass) - 1 do
    with ObjectsClass[f].RecordProps do
    begin
      PWord(@aField[2])^ := ord('I') + ord('D') shl 8;
      if not AddField(nil) then
        Exit; // try to add the ID field
      if Props.fSQLFillPrepareMany = '' then
      begin
        if {%H-}aSQLFields <> '' then
          aSQLFields := aSQLFields + ',';
        aSQLFields := FormatUTF8('%%.RowID %', [aSQLFields, aField[1], aField]);
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
            if Props.fSQLFillPrepareMany = '' then
              aSQLFields := FormatUTF8('%,%.% %', [aSQLFields, aField[1], Name, aField]);
          end;
        end;
      if Props.fSQLFillPrepareMany = '' then
      begin
        if {%H-}aSQLFrom <> '' then
          aSQLFrom := aSQLFrom + ',';
        aSQLFrom := aSQLFrom + SQLTableName + ' ' + ToUTF8(aField[1]);
      end;
      inc(aField[1]);
    end;
  if Props.fSQLFillPrepareMany <> '' then
    SQL := Props.fSQLFillPrepareMany
  else
  begin
    FormatUTF8('select % from % where %', [aSQLFields, aSQLFrom, aSQLWhere], SQL);
    Props.fSQLFillPrepareMany := SQL;
  end;
  // process aFormatSQLJoin,aParamsSQLJoin and aBoundsSQLJoin parameters
  if aFormatSQLJoin <> '' then
  begin
    aSQLWhere := '';
    FormatUTF8(aFormatSQLJoin, aParamsSQLJoin, aSQLJoin);
    JBeg := pointer(aSQLJoin);
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
        FastSetString(aSQLFrom, JBeg, J - JBeg);
        aSQLWhere := aSQLWhere + aSQLFrom;
        JBeg := J;
      end;
      if J^ = #0 then
        break;
      aSQLWhere := aSQLWhere + ProcessField(JBeg);
    until JBeg^ = #0;
    SQL := SQL + ' and (' + FormatUTF8(aSQLWhere, [], aBoundsSQLJoin) + ')';
  end;
  // execute SQL statement and retrieve the matching data
  result := aClient.ExecuteJson([], SQL);
  if result <> '' then // prepare Fill mapping on success - see FillPrepareMany()
    for i := 0 to SQLFieldsCount - 1 do
      with SQLFields[i] do
        fFill.AddMap(Instance, prop, i);
end;

{$ifdef ISDELPHI20062007}
  {$warnings on} // avoid paranoid Delphi 2007 warning
{$endif ISDELPHI20062007}

function TOrm.FillPrepareMany(const aClient: IRestOrm;
  const aFormatSQLJoin: RawUTF8;
  const aParamsSQLJoin, aBoundsSQLJoin: array of const): boolean;
var
  JSON, SQL: RawUTF8;
  ObjectsClass: TOrmClassDynArray;
  T: TOrmTable;
begin
  result := false;
  JSON := EnginePrepareMany(aClient, aFormatSQLJoin, aParamsSQLJoin,
    aBoundsSQLJoin, ObjectsClass, SQL);
  if JSON = '' then
    exit;
  T := TOrmTableJSON.CreateFromTables(ObjectsClass, SQL, JSON);
  if (T = nil) or (T.fResults = nil) then
  begin
    T.Free;
    exit;
  end;
  { assert(T.FieldCount=SQLFieldsCount);
    for i := 0 to SQLFieldsCount-1 do
      assert(IdemPropName(SQLFields[i].SQL,T.fResults[i],StrLen(T.fResults[i]))); }
  fFill.fTable := T;
  T.OwnerMustFree := true;
  fFill.fFillCurrentRow := 1; // point to first data row (0 is field names)
  result := true;
end;

function TOrm.GetID: TID;
begin
  {$ifdef MSWINDOWS}
  if PtrUInt(self) < PtrUInt(SystemInfo.lpMinimumApplicationAddress) then
    // was called from a TOrm property (oftID type)
    // (will return 0 if current instance is nil)
    result := PtrUInt(self)
  else
    result := fID;
    // was called from a real TOrm instance
  {$else}
  if PtrUInt(self) < $100000 then // rough estimation, but works in practice
    result := PtrUInt(self)
  else
  try
    result := fID;
  except
    result := PtrUInt(self);
  end;
  {$endif MSWINDOWS}
end;

function TOrm.GetIDAsPointer: pointer;
begin
  {$ifdef MSWINDOWS}
  if PtrUInt(self) < PtrUInt(SystemInfo.lpMinimumApplicationAddress) then
    // was called from a TOrm property (oftID type)
    // (will return 0 if current instance is nil)
    result := self
  else    // was called from a real TOrm instance
  {$ifndef CPU64}
  if fID > MaxInt then
    raise EOrmException.CreateUTF8('%.GetIDAsPointer is storing ID=%, which ' +
      'cannot be stored in a pointer/TOrm 32-bit instance: use ' +
      'a TID/T*ID published field for 64-bit IDs', [self, fID])
  else
  {$endif CPU64}
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
  {$endif MSWINDOWS}
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
    result := RecordProps.BlobFields <> nil;
end;

function TOrm.GetSimpleFieldCount: integer;
begin
  if self = nil then
    result := 0
  else
    result := length(RecordProps.SimpleFields);
end;

function TOrm.GetFillCurrentRow: integer;
begin
  if (self = nil) or (fFill = nil) then
    result := 0
  else
    result := fFill.FillCurrentRow;
end;

function TOrm.GetFillReachedEnd: boolean;
begin
  result := (self = nil) or (fFill = nil) or (fFill.Table.fRowCount = 0) or
         (cardinal(fFill.FillCurrentRow) > cardinal(fFill.Table.fRowCount));
end;

function TOrm.GetTable: TOrmTable;
begin
  if (self = nil) or (fFill = nil) then
    result := nil
  else
    result := fFill.Table;
end;

function TOrm.GetFieldValue(const PropName: RawUTF8): RawUTF8;
var
  P: TOrmPropInfo;
begin
  result := '';
  if self = nil then
    exit;
  P := RecordProps.Fields.ByName(pointer(PropName));
  if P <> nil then
    P.GetValueVar(self, False, result, nil);
end;

procedure TOrm.SetFieldValue(const PropName: RawUTF8; Value: PUTF8Char);
var
  P: TOrmPropInfo;
begin
  if self = nil then
    exit;
  P := RecordProps.Fields.ByName(pointer(PropName));
  if P <> nil then
    P.SetValue(self, Value, false);
end;

function TOrm.GetAsDocVariant(withID: boolean;
  const withFields: TFieldBits; options: PDocVariantOptions;
  replaceRowIDWithID: boolean): variant;
begin
  GetAsDocVariant(withID, withFields, result, options, replaceRowIDWithID);
end;

procedure TOrm.GetAsDocVariant(withID: boolean;
  const withFields: TFieldBits; var result: variant; options: PDocVariantOptions;
  replaceRowIDWithID: boolean);
const
  _ID: array[boolean] of RawUTF8 = ('RowID', 'ID');
var
  f, i: PtrInt;
  Fields: TOrmPropInfoList;
  intvalues: TRawUTF8Interning;
  doc: TDocVariantData absolute result;
begin
  VarClear(result);
  if self = nil then
    exit;
  Fields := RecordProps.Fields;
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
    if f in withFields then
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
    GetAsDocVariant(withID, RecordProps.SimpleFieldsBits[ooSelect], result, options);
end;

function TOrm.GetFieldVariant(const PropName: string): variant;
var
  P: TOrmPropInfo;
begin
  if self = nil then
    P := nil
  else
    P := RecordProps.Fields.ByRawUTF8Name(
      {$ifdef UNICODE}StringToUTF8{$endif}(PropName));
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
    P := RecordProps.Fields.ByRawUTF8Name(
      {$ifdef UNICODE}StringToUTF8{$endif}(PropName));
  if P <> nil then
    P.SetVariant(self, Source);
end;

function TOrm.Filter(const aFields: TFieldBits): boolean;
var
  f, i: PtrInt;
  Value, Old: RawUTF8;
begin
  result := IsZero(aFields);
  if (self = nil) or result then
    // avoid GPF and handle case if no field was selected
    exit;
  with RecordProps do
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

function TOrm.Filter(const aFields: array of RawUTF8): boolean;
var
  F: TFieldBits;
begin
  if RecordProps.FieldBitsFromRawUTF8(aFields, F) then
    // must always call the virtual Filter() method
    result := Filter(F)
  else
    result := false;
end;

class function TOrm.SQLTableName: RawUTF8;
begin
  if self = nil then
    result := ''
  else
    result := RecordProps.SQLTableName;
end;

class function TOrm.AutoFree(varClassPairs: array of pointer): IAutoFree;
var
  n, i: PtrInt;
begin
  n := length(varClassPairs);
  if (n = 0) or (n and 1 = 1) then
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
end;

class function TOrm.AutoFree(var localVariable; const Rest: IRestOrm;
  ID: TID): IAutoFree;
begin
  result := TAutoFree.Create(localVariable, Create(Rest, ID));
end;

class function TOrm.AutoFree(var localVariable; const Rest: IRestOrm;
  const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const;
  const aCustomFieldsCSV: RawUTF8): IAutoFree;
begin
  result := TAutoFree.Create(localVariable, CreateAndFillPrepare(
    Rest, FormatSQLWhere, BoundsSQLWhere, aCustomFieldsCSV));
end;

class function TOrm.AutoFree(var localVariable; const Rest: IRestOrm;
  const FormatSQLWhere: RawUTF8; const ParamsSQLWhere, BoundsSQLWhere: array of const;
  const aCustomFieldsCSV: RawUTF8): IAutoFree;
begin
  result := TAutoFree.Create(localVariable, CreateAndFillPrepare(
    Rest, FormatSQLWhere, ParamsSQLWhere, BoundsSQLWhere, aCustomFieldsCSV));
end;

class procedure TOrm.AddFilterOrValidate(const aFieldName: RawUTF8;
  aFilter: TSynFilterOrValidate);
begin
  RecordProps.AddFilterOrValidate(aFieldName, aFilter);
end;

class procedure TOrm.AddFilterNotVoidText(const aFieldNames: array of RawUTF8);
var
  i, f: PtrInt;
begin
  with RecordProps do
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
  with RecordProps, Fields do
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
  Value: RawUTF8;
  Validate: TSynValidate;
  ValidateRest: TSynValidateRest absolute Validate;
  valid: boolean;
begin
  result := '';
  if (self = nil) or IsZero(aFields) then
    // avoid GPF and handle case if no field was selected
    exit;
  with RecordProps do
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
                valid := TSynValidateRest(Validate).Validate(f, Value, result,
                  aRest, self)
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

function TOrm.Validate(const aRest: IRestOrm; const aFields: array of RawUTF8;
  aInvalidFieldIndex: PInteger; aValidator: PSynValidate): string;
var
  F: TFieldBits;
begin
  if RecordProps.FieldBitsFromRawUTF8(aFields, F) then
    // must always call the virtual Validate() method
    result := Validate(aRest, F, aInvalidFieldIndex, aValidator)
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
        [RecordProps.Fields.List[invalidField].GetNameDisplay, aErrorMessage]);
    result := false;
  end;
end;

function TOrm.FilterAndValidate(const aRest: IRestOrm;
  const aFields: TFieldBits; aValidator: PSynValidate): RawUTF8;
var
  msg: string;
begin
  if FilterAndValidate(aRest, msg, aFields, aValidator) then
    result := ''
  else
    StringToUTF8(msg, result);
end;

function TOrm.DynArray(const DynArrayFieldName: RawUTF8): TDynArray;
var
  F: PtrInt;
begin
  with RecordProps do
    for F := 0 to length(DynArrayFields) - 1 do
      with DynArrayFields[F] do
        if IdemPropNameU(Name, DynArrayFieldName) then
        begin
          GetDynArray(self, result);
          exit;
        end;
  result.Void;
end;

function TOrm.DynArray(DynArrayFieldIndex: integer): TDynArray;
var
  F: PtrInt;
begin
  if DynArrayFieldIndex > 0 then
    with RecordProps do
      for F := 0 to length(DynArrayFields) - 1 do
        with DynArrayFields[F] do
          if DynArrayIndex = DynArrayFieldIndex then
          begin
            GetDynArray(self, result);
            exit;
          end;
  result.Void;
end;

procedure TOrm.ComputeFieldsBeforeWrite(const aRest: IRestOrm;
  aOccasion: TOrmEvent);
var
  F: PtrInt;
  types: TOrmFieldTypes;
  i64: Int64;
  p: TOrmPropInfo;
begin
  if (self <> nil) and (aRest <> nil) then
    with RecordProps do
    begin
      integer(types) := 0;
      if oftModTime in HasTypeFields then
        include(types, oftModTime);
      if (oftCreateTime in HasTypeFields) and (aOccasion = oeAdd) then
        include(types, oftCreateTime);
      if integer(types) <> 0 then
      begin
        i64 := aRest.GetServerTimestamp;
        for F := 0 to Fields.Count - 1 do
        begin
          p := Fields.List[F];
          if p.OrmFieldType in types then
            TOrmPropInfoRTTIInt64(p).fPropInfo.SetInt64Prop(self, i64);
        end;
      end;
      if oftSessionUserID in HasTypeFields then
      begin
        i64 := aRest.GetCurrentSessionUserID;
        if i64 <> 0 then
          for F := 0 to Fields.Count - 1 do
          begin
            p := Fields.List[F];
            if p.OrmFieldType = oftSessionUserID then
              TOrmPropInfoRTTIInt64(p).fPropInfo.SetInt64Prop(self, i64);
          end;
      end;
    end;
end;


{$ifdef ISDELPHI2010} // Delphi 2009/2010 generics support is buggy :(

{ TRestOrmGenerics }

function TRestOrmGenerics.Generics: TRestOrmGenerics;
begin
  result := self; // circumvent limitation of non parametrized interface definition
end;

function TRestOrmGenerics.RetrieveList<T>(const aCustomFieldsCSV: RawUTF8): TObjectList<T>;
begin
  result := RetrieveList<T>('', [], aCustomFieldsCSV);
end;

function TRestOrmGenerics.RetrieveList<T>(const FormatSQLWhere: RawUTF8;
  const BoundsSQLWhere: array of const; const aCustomFieldsCSV: RawUTF8): TObjectList<T>;
var table: TOrmTable;
begin
  result := nil;
  if self = nil then
    exit;
  table := MultiFieldValues(TOrmClass(T), aCustomFieldsCSV,
    FormatSQLWhere, BoundsSQLWhere);
  if table <> nil then
  try
    result := table.ToObjectList<T>;
  finally
    table.Free;
  end;
end;

{$endif ISDELPHI2010}


{ ------------ TOrmMany Definition }

{ TOrmMany }

constructor TOrmMany.Create;
begin
  inherited Create;
  with RecordProps do
    if (fRecordManySourceProp <> nil) and (fRecordManyDestProp <> nil) then
    begin
      fSourceID := fRecordManySourceProp.GetFieldAddr(self);
      fDestID := fRecordManyDestProp.GetFieldAddr(self);
    end;
end;

function TOrmMany.ManyAdd(const aClient: IRestOrm; aSourceID, aDestID: TID;
  NoDuplicates: boolean; aUseBatch: TRestBatch): boolean;
begin
  result := false;
  if (self = nil) or (aClient = nil) or (aSourceID = 0) or (aDestID = 0) or
     (fSourceID = nil) or (fDestID = nil) then
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
  if (self = nil) or (fSourceID = nil) then
    result := false
  else // avoid GPF
    result := ManyAdd(aClient, fSourceID^, aDestID, NoDuplicates);
end;

function TOrmMany.DestGet(const aClient: IRestOrm; aSourceID: TID;
  out DestIDs: TIDDynArray): boolean;
var
  Where: RawUTF8;
begin
  Where := IDWhereSQL(aClient, aSourceID, False);
  if Where = '' then
    result := False
  else
    result := aClient.OneFieldValues(RecordClass, 'Dest', Where,
      TInt64DynArray(DestIDs));
end;

function TOrmMany.DestGetJoined(const aClient: IRestOrm;
  const aDestWhereSQL: RawUTF8; aSourceID: TID; out DestIDs: TIDDynArray): boolean;
var
  aTable: TOrmTable;
begin
  aTable := DestGetJoinedTable(aClient, aDestWhereSQL, aSourceID, jkDestID);
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
  const aDestWhereSQL: RawUTF8; aSourceID: TID): TOrm;
var
  aTable: TOrmTable;
begin
  aTable := DestGetJoinedTable(aClient, aDestWhereSQL, aSourceID, jkDestFields);
  if aTable = nil then
    result := nil
  else
  begin
    result := TOrmClass(RecordProps.fRecordManyDestProp.ObjectClass).Create;
    aTable.OwnerMustFree := true;
    result.FillPrepare(aTable, ctnTrimExisting);
  end;
end;

function TOrmMany.DestGetJoinedTable(const aClient: IRestOrm;
  const aDestWhereSQL: RawUTF8; aSourceID: TID; JoinKind: TOrmManyJoinKind;
  const aCustomFieldsCSV: RawUTF8): TOrmTable;
var
  Select, SQL: RawUTF8;
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
  if (self = nil) or (fSourceID = nil) or (fDestID = nil) or (aClient = nil) then
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
      Select := DestProps.Props.SQLTableName + '.RowID';
    jkPivotID:
      Select := SelfProps.Props.SQLTableName + '.RowID';
    jkDestFields:
      if aCustomFieldsCSV = '' then
        SelectFields([DestProps])
      else
        Select := AddPrefixToCSV(pointer(aCustomFieldsCSV),
          DestProps.Props.SQLTableName + '.');
    jkPivotFields:
      if aCustomFieldsCSV = '' then
        SelectFields([SelfProps])
      else
        Select := AddPrefixToCSV(pointer(aCustomFieldsCSV),
          SelfProps.Props.SQLTableName + '.');
    jkPivotAndDestFields:
      if aCustomFieldsCSV = '' then
        SelectFields([SelfProps, DestProps])
      else
        Select := aCustomFieldsCSV;
  end;
  if aDestWhereSQL = '' then
    // fast inlined prepared statement
    SQL := 'SELECT % FROM %,% WHERE %.Source=:(%): AND %.Dest=%.RowID'
  else if PosEx(RawUTF8(':('), aDestWhereSQL, 1) > 0 then
    // statement is globaly inlined -> cache prepared statement
    SQL := 'SELECT % FROM %,% WHERE %.Source=:(%): AND %.Dest=%.RowID AND %'
  else
    // statement is not globaly inlined -> no caching of prepared statement
    SQL := 'SELECT % FROM %,% WHERE %.Source=% AND %.Dest=%.RowID AND %';
  result := aClient.ExecuteList([POrmClass(self)^,
    TOrmClass(SelfProps.Props.fRecordManyDestProp.ObjectClass)],
    FormatUTF8(SQL, [{%H-}Select, DestProps.Props.SQLTableName,
      SelfProps.Props.SQLTableName, SelfProps.Props.SQLTableName,
      aSourceID, SelfProps.Props.SQLTableName,
      DestProps.Props.SQLTableName, aDestWhereSQL]));
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
  if (self = nil) or (aClient = nil) or (aSourceID = 0) or (aDestID = 0) then
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
  if (self = nil) or (aClient = nil) or (aSourceID = 0) or (aDestID = 0) then
    result := false
  else // invalid parameters
    result := aClient.Retrieve(FormatUTF8('Source=:(%): AND Dest=:(%):',
      [aSourceID, aDestID]), self);
end;

function TOrmMany.ManySelect(const aClient: IRestOrm; aDestID: TID): boolean;
begin
  if (self = nil) or (fSourceID = nil) then
    result := false
  else // avoid GPF
    result := ManySelect(aClient, fSourceID^, aDestID);
end;

function TOrmMany.InternalFillMany(const aClient: IRestOrm; aID: TID;
  const aAndWhereSQL: RawUTF8; isDest: boolean): integer;
var
  aTable: TOrmTable;
  Where: RawUTF8;
begin
  result := 0;
  if self = nil then
    exit;
  if not isDest and (aID = 0) then
    if fSourceID <> nil then
      aID := fSourceID^; // has been set by TOrm.Create
  Where := IDWhereSQL(aClient, aID, isDest, aAndWhereSQL);
  if Where = '' then
    exit;
  aTable := aClient.MultiFieldValues(RecordClass, '', Where);
  if aTable = nil then
    exit;
  aTable.OwnerMustFree := true;
  FillPrepare(aTable); // temporary storage for FillRow, FillOne and FillRewind
  result := aTable.fRowCount;
end;

function TOrmMany.FillMany(const aClient: IRestOrm; aSourceID: TID;
  const aAndWhereSQL: RawUTF8): integer;
begin
  result := InternalFillMany(aClient, aSourceID, aAndWhereSQL, false);
end;

function TOrmMany.FillManyFromDest(const aClient: IRestOrm; aDestID: TID;
  const aAndWhereSQL: RawUTF8): integer;
begin
  result := InternalFillMany(aClient, aDestID, aAndWhereSQL, true);
end;

function TOrmMany.IDWhereSQL(const aClient: IRestOrm; aID: TID;
  isDest: boolean; const aAndWhereSQL: RawUTF8): RawUTF8;
const
  FieldName: array[boolean] of RawUTF8 = ('Source=', 'Dest=');
begin
  if (self = nil) or (aID = 0) or (fSourceID = nil) or (fDestID = nil) or
     (aClient = nil) then
    result := ''
  else
  begin
    if aAndWhereSQL <> '' then
      if PosEx(RawUTF8(':('), aAndWhereSQL, 1) > 0 then
        result := '%:(%): AND %'
      else // inlined parameters
        result := '%% AND %'
    else // no inlined parameters -> not cached
      result := '%:(%):'; // no additional where clause -> inline ID
    result := FormatUTF8(result, [FieldName[isDest], aID, aAndWhereSQL]);
  end;
end;

function TOrmMany.SourceGet(const aClient: IRestOrm; aDestID: TID;
  out SourceIDs: TIDDynArray): boolean;
var
  Where: RawUTF8;
begin
  Where := IDWhereSQL(aClient, aDestID, True);
  if Where = '' then
    result := false
  else
    result := aClient.OneFieldValues(RecordClass, 'Source', Where,
      TInt64DynArray(SourceIDs));
end;

function TOrmMany.InternalIDFromSourceDest(const aClient: IRestOrm;
  aSourceID, aDestID: TID): TID;
begin
  SetID(aClient.OneFieldValue(RecordClass, 'RowID', FormatUTF8(
    'Source=:(%): AND Dest=:(%):', [aSourceID, aDestID])), result{%H-});
end;


{ ------------ TOrmVirtual Definitions }

{ TOrmFts3 }

class function TOrmFts3.OptimizeFTS3Index(const Server: IRestOrmServer): boolean;
begin
  if (self = nil) or (Server = nil) then
    result := false
  else
    with RecordProps do
      result := Server.ExecuteFmt('INSERT INTO %(%) VALUES(''optimize'');',
        [SQLTableName, SQLTableName]);
end;


{ TOrmFts4 }

class procedure TOrmFts4.InitializeTable(const Server: IRestOrmServer;
  const FieldName: RawUTF8; Options: TOrmInitializeTableOptions);
var
  m: TOrmModel;
  p: TOrmModelProperties;
  main, fts, ftsfields: RawUTF8;
begin
  inherited;
  if FieldName <> '' then
    exit;
  m := Server.Model;
  p := m.Props[self];
  if (p = nil) or (p.fFTSWithoutContentFields = '') then
    exit;
  main := m.Tables[p.fFTSWithoutContentTableIndex].SQLTableName;
  if not Server.IsInternalSQLite3Table(p.fFTSWithoutContentTableIndex) then
    raise EModelException.CreateUTF8(
      '% is an external content FTS4/5 table but source % is not ' +
      'a local SQLite3 table: FTS search will be unavailable', [self, main]);
  fts := p.Props.SQLTableName;
  ftsfields := p.Props.SQLTableSimpleFieldsNoRowID;
  // see http://www.sqlite.org/fts3.html#*fts4content
  if p.Kind = rFTS5 then
  begin
    // In fts 5 we can't use docid only rowid, also use insert() values('delete',) to delete record
    Server.ExecuteFmt('CREATE TRIGGER %_bu BEFORE UPDATE ON % ' +
      'BEGIN INSERT INTO %(%,rowid,%) VALUES(''delete'',old.rowid%); END;',
      [main, main, fts, fts, ftsfields, StringReplaceAll(p.fFTSWithoutContentFields,
       'new.', 'old.')]);
    Server.ExecuteFmt('CREATE TRIGGER %_bd BEFORE DELETE ON % ' +
      'BEGIN INSERT INTO %(%,rowid,%) VALUES(''delete'',old.rowid%); END;',
      [main, main, fts, fts, ftsfields, StringReplaceAll(p.fFTSWithoutContentFields,
      'new.', 'old.')]);
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

class function TOrmRTreeAbstract.RTreeSQLFunctionName: RawUTF8;
begin
  result := RecordProps.SQLTableName + '_in';
end;

{ TOrmRTree }

class procedure TOrmRTree.BlobToCoord(const InBlob;
  var OutCoord: TOrmTreeCoords);
begin // direct memory copy with no memory check
  MoveFast(InBlob, OutCoord,
    (RecordProps.RTreeCoordBoundaryFields shr 1) * SizeOf(double));
end;

class function TOrmRTree.ContainedIn(const BlobA, BlobB): boolean;
var
  A, B: TOrmTreeCoords;
  i: PtrInt;
begin
  BlobToCoord(BlobA, A);
  BlobToCoord(BlobB, B);
  result := false;
  for i := 0 to (RecordProps.RTreeCoordBoundaryFields shr 1) - 1 do
    if (A[i].max < B[i].min) or (A[i].min > B[i].max) then
      exit; // no match
  result := true; // box match
end;

{ TOrmRTreeInteger }

class procedure TOrmRTreeInteger.BlobToCoord(const InBlob;
  var OutCoord: TOrmTreeCoordsInteger);
begin // direct memory copy with no memory check
  MoveFast(InBlob, OutCoord,
    (RecordProps.RTreeCoordBoundaryFields shr 1) * SizeOf(integer));
end;

class function TOrmRTreeInteger.ContainedIn(const BlobA, BlobB): boolean;
var
  A, B: TOrmTreeCoordsInteger;
  i: PtrInt;
begin
  BlobToCoord(BlobA, A);
  BlobToCoord(BlobB, B);
  result := false;
  for i := 0 to (RecordProps.RTreeCoordBoundaryFields shr 1) - 1 do
    if (A[i].max < B[i].min) or (A[i].min > B[i].max) then
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
label
  Simple, Small, Copiabl;
begin
  InitializeCriticalSection(fLock);
  if aTable = nil then
    raise EModelException.Create('TOrmProperties.Create(nil)');
  // register for JSONToObject() and for TOrmPropInfoRTTITID.Create()
  // (should have been done before in TOrmModel.Create/AddTable)
  fTableRtti := rtti.RegisterClass(aTable) as TRttiJson;
  // initialize internal structures
  fModelMax := -1;
  fTable := aTable;
  fSQLTableName := GetDisplayNameFromClass(aTable);
  fSQLTableNameUpperWithDot := UpperCase(SQLTableName) + '.';
  isTOrmMany := aTable.InheritsFrom(TOrmMany);
  // add properties to internal Fields list
  nProps := ClassFieldCountWithParents(aTable);
  if nProps > MAX_SQLFIELDS_INCLUDINGID then
    raise EModelException.CreateUTF8('% has too many fields: %>=%',
      [Table, nProps, MAX_SQLFIELDS]);
  fFields := TOrmPropInfoList.Create(aTable, [pilRaiseEOrmExceptionIfNotHandled]);
  aTable.InternalRegisterCustomProperties(self);
  if Fields.Count > MAX_SQLFIELDS_INCLUDINGID then
    raise EModelException.CreateUTF8(
      '% has too many fields after InternalRegisterCustomProperties(%): %>=%',
      [Table, self, Fields.Count, MAX_SQLFIELDS]);
  SetLength(Fields.fList, Fields.Count);
  // generate some internal lookup information
  fSQLTableRetrieveAllFields := 'ID';
  SetLength(fManyFields, MAX_SQLFIELDS);
  SetLength(fSimpleFields, MAX_SQLFIELDS);
  SetLength(fJoinedFields, MAX_SQLFIELDS);
  SetLength(fCopiableFields, MAX_SQLFIELDS);
  SetLength(fDynArrayFields, MAX_SQLFIELDS);
  SetLength(fBlobCustomFields, MAX_SQLFIELDS);
  SetLength(fBlobFields, MAX_SQLFIELDS);
  MainField[false] := -1;
  MainField[true] := -1;
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
      raise EModelException.CreateUTF8('ID is already defined in TOrm: ' +
        '%.% field name is not allowed as published property', [Table, F.Name]);
    if PosEx(' ' + LowerCase(F.Name) + ' ', SQLITE3_KEYWORDS) > 0 then
      raise EModelException.CreateUTF8(
        '%.% field name conflicts with a SQL keyword', [Table, F.Name]);
    //  handle unique fields, i.e. if marked as "stored false"
    if aIsUnique in F.Attributes then
    begin
      include(IsUniqueFieldsBits, i);
      // must trim() text value before storage, and validate for unicity
      if F.OrmFieldType in [oftUTF8Text, oftAnsiText] then
        AddFilterOrValidate(i, TSynFilterTrim.Create);
      AddFilterOrValidate(i, TSynValidateUniqueField.Create);
    end;
    // get corresponding properties content
    include(fHasTypeFields, F.OrmFieldType);
    include(FieldBits[F.OrmFieldType], i);
    case F.OrmFieldType of
      oftUnknown:
        ;
      oftUTF8Text:
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
          BlobFields[nBlob] := F as TOrmPropInfoRTTI;
          inc(nBlob);
          fSQLTableUpdateBlobFields := fSQLTableUpdateBlobFields + F.Name + '=?,';
          fSQLTableRetrieveBlobFields := fSQLTableRetrieveBlobFields + F.Name + ',';
          fSQLTableRetrieveAllFields := fSQLTableRetrieveAllFields + ',' + F.Name;
          goto Copiabl;
        end;
      oftID: // = TOrm(aID)
        if isTOrmMany and (IdemPropNameU(F.Name, 'Source') or
          IdemPropNameU(F.Name, 'Dest')) then
          goto Small
        else
        begin
          JoinedFields[nORM] := F as TOrmPropInfoRTTIID;
          inc(nORM);
          goto Small;
        end;
      oftMany:
        begin
          ManyFields[nMany] := F as TOrmPropInfoRTTIMany;
          inc(nMany);
        end;
      oftBlobDynArray:
        with F as TOrmPropInfoRTTIDynArray do
        begin
          if DynArrayIndex > 0 then
            for j := 0 to nDynArray - 1 do
              if DynArrayFields[j].DynArrayIndex = DynArrayIndex then
                raise EModelException.CreateUTF8('dup index % for %.% and %.% properties',
                  [DynArrayIndex, Table, Name, Table, DynArrayFields[j].Name]);
          DynArrayFields[nDynArray] := TOrmPropInfoRTTIDynArray(F);
          if TOrmPropInfoRTTIDynArray(F).ObjArray <> nil then
            fDynArrayFieldsHasObjArray := true;
          inc(nDynArray);
          goto Simple;
        end;
      oftBlobCustom, oftUTF8Custom:
        begin
          BlobCustomFields[nBlobCustom] := F;
          inc(nBlobCustom);
          goto Simple;
        end;
      oftCreateTime:
        begin
          include(ComputeBeforeAddFieldsBits, i);
          goto Small;
        end;
      oftModTime, oftSessionUserID:
        begin
          include(ComputeBeforeAddFieldsBits, i);
          include(ComputeBeforeUpdateFieldsBits, i);
          goto Small;
        end;
      oftRecordVersion:
        begin
          if fRecordVersionField <> nil then
            raise EModelException.CreateUTF8('%: only a single TRecordVersion ' +
              'field is allowed per class', [Table]);
          fRecordVersionField := F as TOrmPropInfoRTTIRecordVersion;
          fSQLTableRetrieveAllFields := fSQLTableRetrieveAllFields + ',' + F.Name;
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
        include(SimpleFieldsBits[ooSelect], i);
        fSQLTableSimpleFieldsNoRowID := fSQLTableSimpleFieldsNoRowID + F.Name + ',';
        fSQLTableRetrieveAllFields := fSQLTableRetrieveAllFields + ',' + F.Name;
Copiabl:include(CopiableFieldsBits, i);
        CopiableFields[nCopiableFields] := F;
        inc(nCopiableFields);
      end;
    end;
  end;
  if fSQLTableSimpleFieldsNoRowID <> '' then
    SetLength(fSQLTableSimpleFieldsNoRowID, length(fSQLTableSimpleFieldsNoRowID) - 1);
  if fSQLTableUpdateBlobFields <> '' then
    SetLength(fSQLTableUpdateBlobFields, length(fSQLTableUpdateBlobFields) - 1);
  if fSQLTableRetrieveBlobFields <> '' then
    SetLength(fSQLTableRetrieveBlobFields, length(fSQLTableRetrieveBlobFields) - 1);
  SetLength(fManyFields, nMany);
  SetLength(fSimpleFields, nSimple);
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
    FieldBits[oftBlobDynArray] - FieldBits[oftBlobCustom] - FieldBits[oftUTF8Custom] then
    raise EModelException.CreateUTF8('TOrmProperties.Create(%) Bits?', [Table]);
  if isTOrmMany then
  begin
    fRecordManySourceProp := Fields.ByRawUTF8Name('Source') as TOrmPropInfoRTTIInstance;
    if fRecordManySourceProp = nil then
      raise EModelException.CreateUTF8('% expects a SOURCE field', [Table])
    else
      fRecordManyDestProp := Fields.ByRawUTF8Name('Dest') as TOrmPropInfoRTTIInstance;
    if fRecordManyDestProp = nil then
      raise EModelException.CreateUTF8('% expects a DEST field', [Table]);
  end;
end;

function TOrmProperties.BlobFieldPropFromRawUTF8(const PropName: RawUTF8): PRttiProp;
var
  i: integer;
begin
  if (self <> nil) and (PropName <> '') then
    for i := 0 to high(BlobFields) do
      if IdemPropNameU(BlobFields[i].Name, PropName) then
      begin
        result := BlobFields[i].PropInfo;
        exit;
      end;
  result := nil;
end;

function TOrmProperties.BlobFieldPropFromUTF8(PropName: PUTF8Char;
  PropNameLen: integer): PRttiProp;
var
  i: integer;
begin
  if (self <> nil) and (PropName <> '') then
    for i := 0 to high(BlobFields) do
      if IdemPropName(BlobFields[i].PropInfo^.Name^, PropName, PropNameLen) then
      begin
        result := BlobFields[i].PropInfo;
        exit;
      end;
  result := nil;
end;

const
  /// simple wrapper from each SQL used type into SQLite3 field datatype
  // - set to '' for fields with no column created in the database
  DEFAULT_ORMFIELDTYPETOSQL: array[TOrmFieldType] of RawUTF8 = ('',                              // oftUnknown
    ' TEXT COLLATE NOCASE, ',        // oftAnsiText
    ' TEXT COLLATE SYSTEMNOCASE, ',  // oftUTF8Text
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
    ' TEXT COLLATE NOCASE, ',        // oftUTF8Custom
    '',                              // oftMany
    ' INTEGER, ',                    // oftModTime
    ' INTEGER, ',                    // oftCreateTime
    ' INTEGER, ',                    // oftTID
    ' INTEGER, ',                    // oftRecordVersion
    ' INTEGER, ',                    // oftSessionUserID
    ' TEXT COLLATE ISO8601, ',       // oftDateTimeMS
    ' INTEGER, ',                    // oftUnixTime
    ' INTEGER, ');                   // oftUnixMSTime

function TOrmProperties.OrmFieldTypeToSQL(FieldIndex: integer): RawUTF8;
begin
  if (self = nil) or (cardinal(FieldIndex) >= cardinal(Fields.Count)) then
    result := ''
  else if (FieldIndex < length(fCustomCollation)) and
          (fCustomCollation[FieldIndex] <> '') then
    result := ' TEXT COLLATE ' + fCustomCollation[FieldIndex] + ', '
  else
    result := DEFAULT_ORMFIELDTYPETOSQL[Fields.List[FieldIndex].OrmFieldTypeStored];
end;

function TOrmProperties.SetCustomCollation(FieldIndex: integer;
  const aCollationName: RawUTF8): boolean;
begin
  result := (self <> nil) and (cardinal(FieldIndex) < cardinal(Fields.Count));
  if result then
  begin
    if Fields.Count > length(fCustomCollation) then
      SetLength(fCustomCollation, Fields.Count);
    fCustomCollation[FieldIndex] := aCollationName;
    with Fields.List[FieldIndex] do
      if IdemPropNameU(aCollationName, 'BINARY') then
        include(fAttributes, aBinaryCollation)
      else
        exclude(fAttributes, aBinaryCollation);
  end;
end;

function TOrmProperties.SetCustomCollation(
  const aFieldName, aCollationName: RawUTF8): boolean;
begin
  result := SetCustomCollation(Fields.IndexByNameOrExcept(aFieldName), aCollationName);
end;

procedure TOrmProperties.SetCustomCollationForAll(aFieldType: TOrmFieldType;
  const aCollationName: RawUTF8);
var
  i: PtrInt;
begin
  if (self <> nil) and not (aFieldType in [oftUnknown, oftMany]) then
    for i := 0 to Fields.Count - 1 do
      if Fields.List[i].OrmFieldTypeStored = aFieldType then
        SetCustomCollation(i, aCollationName);
end;

procedure TOrmProperties.SetMaxLengthValidatorForTextFields(
  IndexIsUTF8Length: boolean);
var
  i: PtrInt;
begin
  if self <> nil then
    for i := 0 to Fields.Count - 1 do
      with Fields.List[i] do
        if (SQLDBFieldType in TEXT_DBFIELDS) and (cardinal(FieldWidth - 1) < 262144) then
          AddFilterOrValidate(i, TSynValidateText.CreateUTF8('{maxLength:%,UTF8Length:%}',
            [FieldWidth, IndexIsUTF8Length], []));
end;

procedure TOrmProperties.SetMaxLengthFilterForTextFields(
  IndexIsUTF8Length: boolean);
var
  i: PtrInt;
begin
  if self <> nil then
    for i := 0 to Fields.Count - 1 do
      with Fields.List[i] do
        if (SQLDBFieldType in TEXT_DBFIELDS) and (cardinal(FieldWidth - 1) < 262144) then
          AddFilterOrValidate(i, TSynFilterTruncate.CreateUTF8('{maxLength:%,UTF8Length:%}',
            [FieldWidth, IndexIsUTF8Length], []));
end;

procedure TOrmProperties.SetVariantFieldsDocVariantOptions(
  const Options: TDocVariantOptions);
var
  i: PtrInt;
begin
  if self <> nil then
    for i := 0 to Fields.Count - 1 do
      if (Fields.List[i].OrmFieldType = oftVariant) and
         Fields.List[i].InheritsFrom(TOrmPropInfoRTTIVariant) then
        TOrmPropInfoRTTIVariant(Fields.List[i]).DocVariantOptions := Options;
end;

function TOrmProperties.SQLAddField(FieldIndex: integer): RawUTF8;
begin
  result := OrmFieldTypeToSQL(FieldIndex);
  if result = '' then
    exit; // some fields won't have any column created in the database
  result := FormatUTF8('ALTER TABLE % ADD COLUMN %%',
    [SQLTableName, Fields.List[FieldIndex].Name, result]);
  if FieldIndex in IsUniqueFieldsBits then
    insert(' UNIQUE', result, length(result) - 1);
  result[length(result) - 1] := ';' // OrmFieldTypeToSQL[] ends with ','
end;

procedure TOrmProperties.SetJSONWriterColumnNames(W: TJSONSerializer;
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
    W.ColNames[0] := 'RowID'; // works for both normal and FTS3 records
  for i := 0 to nf - 1 do
  begin
    W.ColNames[n] := Fields.List[W.Fields[i]].Name;
    inc(n);
  end;
  // write or init field names for appropriate JSON Expand
  W.AddColumns(KnownRowsCount);
end;

function TOrmProperties.CreateJSONWriter(JSON: TStream;
  Expand, withID: boolean; const aFields: TFieldBits;
  KnownRowsCount, aBufSize: integer): TJSONSerializer;
begin
  result := CreateJSONWriter(JSON, Expand, withID,
    FieldBitsToIndex(aFields, Fields.Count), KnownRowsCount, aBufSize);
end;

function TOrmProperties.CreateJSONWriter(JSON: TStream;
  Expand, withID: boolean; const aFields: TFieldIndexDynArray;
  KnownRowsCount, aBufSize: integer): TJSONSerializer;
begin
  if (self = nil) or ((Fields.Count = 0) and not withID) then  // no data
    result := nil
  else
  begin
    result := TJSONSerializer.Create(JSON, Expand, withID, aFields, aBufSize);
    SetJSONWriterColumnNames(result, KnownRowsCount);
  end;
end;

function TOrmProperties.CreateJSONWriter(JSON: TStream; Expand: boolean;
  const aFieldsCSV: RawUTF8; KnownRowsCount, aBufSize: integer): TJSONSerializer;
var
  withID: boolean;
  bits: TFieldBits;
begin
  FieldBitsFromCSV(aFieldsCSV, bits, withID);
  result := CreateJSONWriter(JSON, Expand, withID, bits, KnownRowsCount, aBufSize);
end;

function TOrmProperties.SaveSimpleFieldsFromJsonArray(var P: PUTF8Char;
  var EndOfObject: AnsiChar; ExtendedJSON: boolean): RawUTF8;
var
  i: PtrInt;
  W: TJSONSerializer;
  Start: PUTF8Char;
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
  W := TJSONSerializer.CreateOwnedStream(temp);
  try
    W.Add('{');
    for i := 0 to length(SimpleFields) - 1 do
    begin
      if ExtendedJSON then
      begin
        W.AddString(SimpleFields[i].Name);
        W.Add(':');
      end
      else
        W.AddFieldName(SimpleFields[i].Name);
      Start := P;
      P := GotoEndJSONItem(P);
      if (P = nil) or
         not (P^ in [',', ']']) then
        exit;
      W.AddNoJSONEscape(Start, P - Start);
      W.Add(',');
      repeat
        inc(P)
      until (P^ > ' ') or
            (P^ = #0);
    end;
    W.CancelLastComma;
    W.Add('}');
    W.SetText(result);
  finally
    W.Free;
  end;
  EndOfObject := P^;
  if P^ <> #0 then
    repeat
      inc(P)
    until (P^ > ' ') or
          (P^ = #0);
end;

procedure TOrmProperties.SaveBinaryHeader(W: TBufferWriter);
var
  i: PtrInt;
  FieldNames: TRawUTF8DynArray;
begin
  W.Write(SQLTableName);
  SetLength(FieldNames, Fields.Count);
  for i := 0 to Fields.Count - 1 do
    FieldNames[i] := Fields.List[i].Name;
  W.WriteRawUTF8DynArray(FieldNames, Fields.Count);
  for i := 0 to Fields.Count - 1 do
    W.Write(@Fields.List[i].fOrmFieldType, SizeOf(TOrmFieldType));
end;

function TOrmProperties.CheckBinaryHeader(var R: TFastReader): boolean;
var
  n, i: integer;
  FieldNames: TRawUTF8DynArray;
  FieldTypes: array[0..MAX_SQLFIELDS - 1] of TOrmFieldType;
begin
  result := false;
  if (R.VarUTF8 <> SQLTableName) or
     (R.ReadVarRawUTF8DynArray(FieldNames) <> Fields.Count) then
    exit;
  n := SizeOf(TOrmFieldType) * Fields.Count;
  if not R.CopySafe(@FieldTypes, n) then
    exit;
  for i := 0 to Fields.Count - 1 do
    with Fields.List[i] do
      if (Name <> FieldNames[i]) or (OrmFieldType <> FieldTypes[i]) then
        exit;
  result := true;
end;

function TOrmProperties.IsFieldName(const PropName: RawUTF8): boolean;
begin
  result := (PropName <> '') and (isRowID(pointer(PropName)) or
    (Fields.IndexByName(PropName) >= 0));
end;

function TOrmProperties.IsFieldNameOrFunction(const PropName: RawUTF8): boolean;
var
  L: integer;
begin
  L := length(PropName);
  if (L = 0) or (self = nil) then
    result := false
  else if PropName[L] = ')' then
    case IdemPCharArray(pointer(PropName),
       ['MAX(', 'MIN(', 'AVG(', 'SUM(', 'JSONGET(', 'JSONHAS(']) of
      0..3:
        result := IsFieldName(copy(PropName, 5, L - 5));
      4..5:
        result := IsFieldName(copy(PropName, 9, PosExChar(',', PropName) - 9));
    else
      result := IsFieldName(PropName);
    end
  else
    result := IsFieldName(PropName);
end;

function TOrmProperties.AddFilterOrValidate(aFieldIndex: integer;
  aFilter: TSynFilterOrValidate): boolean;
begin
  if (self = nil) or (cardinal(aFieldIndex) >= cardinal(Fields.Count)) or
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

procedure TOrmProperties.AddFilterOrValidate(const aFieldName: RawUTF8;
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
  inherited;
  DeleteCriticalSection(fLock);
  Fields.Free;
end;

function TOrmProperties.FieldBitsFromBlobField(aBlobField: PRttiProp;
  var Bits: TFieldBits): boolean;
var
  f: PtrInt;
begin
  FillZero(Bits);
  if self <> nil then
    for f := 0 to high(BlobFields) do
      if BlobFields[f].fPropInfo = aBlobField then
      begin
        Include(Bits, BlobFields[f].PropertyIndex);
        result := true;
        exit;
      end;
  result := false;
end;

function TOrmProperties.FieldBitsFromCSV(const aFieldsCSV: RawUTF8;
  var Bits: TFieldBits): boolean;
var
  ndx: integer;
  P: PUTF8Char;
  FieldName: ShortString;
begin
  FillZero(Bits);
  result := false;
  if self = nil then
    exit;
  P := pointer(aFieldsCSV);
  while P <> nil do
  begin
    GetNextItemShortString(P, FieldName);
    FieldName[ord(FieldName[0]) + 1] := #0; // make PUTF8Char
    ndx := Fields.IndexByName(@FieldName[1]);
    if ndx < 0 then
      exit; // invalid field name
    include(Bits, ndx);
  end;
  result := true;
end;

function TOrmProperties.FieldBitsFromCSV(const aFieldsCSV: RawUTF8;
  var Bits: TFieldBits; out withID: boolean): boolean;
var
  ndx: integer;
  P: PUTF8Char;
  FieldName: ShortString;
begin
  if (aFieldsCSV = '*') and (self <> nil) then
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
  P := pointer(aFieldsCSV);
  while P <> nil do
  begin
    GetNextItemShortString(P, FieldName);
    if IsRowIDShort(FieldName) then
    begin
      withID := true;
      continue;
    end;
    FieldName[ord(FieldName[0]) + 1] := #0; // make PUTF8Char
    ndx := Fields.IndexByName(@FieldName[1]);
    if ndx < 0 then
      exit; // invalid field name
    include(Bits, ndx);
  end;
  result := true;
end;

function TOrmProperties.FieldBitsFromCSV(const aFieldsCSV: RawUTF8): TFieldBits;
begin
  if not FieldBitsFromCSV(aFieldsCSV, result) then
    FillZero(result);
end;

function TOrmProperties.FieldBitsFromExcludingCSV(
  const aFieldsCSV: RawUTF8; aOccasion: TOrmOccasion): TFieldBits;
var
  excluded: TFieldBits;
begin
  result := SimpleFieldsBits[aOccasion];
  if FieldBitsFromCSV(aFieldsCSV, excluded) then
    result := result - excluded;
end;

function TOrmProperties.FieldBitsFromRawUTF8(
  const aFields: array of RawUTF8; var Bits: TFieldBits): boolean;
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

function TOrmProperties.FieldBitsFromRawUTF8(
  const aFields: array of RawUTF8): TFieldBits;
begin
  if not FieldBitsFromRawUTF8(aFields, result) then
    FillZero(result);
end;

function TOrmProperties.CSVFromFieldBits(const Bits: TFieldBits): RawUTF8;
var
  f: PtrInt;
  W: TTextWriter;
  temp: TTextWriterStackBuffer;
begin
  W := TTextWriter.CreateOwnedStream(temp);
  try
    for f := 0 to Fields.Count - 1 do
      if f in Bits then
      begin
        W.AddString(Fields.List[f].Name);
        W.Add(',');
      end;
    W.CancelLastComma;
    W.SetText(result);
  finally
    W.Free;
  end;
end;

function TOrmProperties.FieldIndexDynArrayFromRawUTF8(
  const aFields: array of RawUTF8; var Indexes: TFieldIndexDynArray): boolean;
var
  f, ndx: PtrInt;
begin
  result := false;
  if self = nil then
    exit;
  for f := 0 to high(aFields) do
  begin
    ndx := Fields.IndexByName(aFields[f]);
    if ndx < 0 then
      exit; // invalid field name
    AddFieldIndex(Indexes, ndx);
  end;
  result := true;
end;

function TOrmProperties.FieldIndexDynArrayFromRawUTF8(
  const aFields: array of RawUTF8): TFieldIndexDynArray;
begin
  if not FieldIndexDynArrayFromRawUTF8(aFields, result) then
    result := nil;
end;

function TOrmProperties.FieldIndexDynArrayFromCSV(
  const aFieldsCSV: RawUTF8; var Indexes: TFieldIndexDynArray): boolean;
var
  ndx: integer;
  P: PUTF8Char;
  FieldName: ShortString;
begin
  result := false;
  if self = nil then
    exit;
  P := pointer(aFieldsCSV);
  while P <> nil do
  begin
    GetNextItemShortString(P, FieldName);
    FieldName[ord(FieldName[0]) + 1] := #0; // make PUTF8Char
    ndx := Fields.IndexByName(@FieldName[1]);
    if ndx < 0 then
      exit; // invalid field name
    AddFieldIndex(Indexes, ndx);
  end;
  result := true;
end;

function TOrmProperties.FieldIndexDynArrayFromCSV(
  const aFieldsCSV: RawUTF8): TFieldIndexDynArray;
begin
  if not FieldIndexDynArrayFromCSV(aFieldsCSV, result) then
    result := nil;
end;

function TOrmProperties.FieldIndexDynArrayFromBlobField(
  aBlobField: PRttiProp; var Indexes: TFieldIndexDynArray): boolean;
var
  f: PtrInt;
begin
  if self <> nil then
    for f := 0 to high(BlobFields) do
      if BlobFields[f].fPropInfo = aBlobField then
      begin
        AddFieldIndex(Indexes, BlobFields[f].PropertyIndex);
        result := true;
        exit;
      end;
  result := false;
end;

function TOrmProperties.AppendFieldName(FieldIndex: integer;
  var Text: RawUTF8; ForceNoRowID: boolean): boolean;
begin
  result := false; // success
  if FieldIndex = VIRTUAL_TABLE_ROWID_COLUMN then
    if ForceNoRowID then
      Text := Text + 'ID'
    else
      Text := Text + 'RowID'
  else if (self = nil) or (cardinal(FieldIndex) >= cardinal(Fields.Count)) then
    result := true
  else
    Text := Text + Fields.List[FieldIndex].Name;
end;

function TOrmProperties.MainFieldName(ReturnFirstIfNoUnique: boolean): RawUTF8;
begin
  if (self = nil) or (Table = nil) or (MainField[ReturnFirstIfNoUnique] < 0) then
    result := ''
  else
    result := Fields.List[MainField[ReturnFirstIfNoUnique]].Name;
end;

procedure TOrmProperties.RegisterCustomFixedSizeRecordProperty(
  aTable: TClass; aRecordSize: cardinal; const aName: RawUTF8;
  aPropertyPointer: pointer; aAttributes: TOrmPropInfoAttributes; aFieldWidth: integer;
  aData2Text: TOnSQLPropInfoRecord2Text; aText2Data: TOnSQLPropInfoRecord2Data);
begin
  Fields.Add(TOrmPropInfoRecordFixedSize.Create(aRecordSize, aName, Fields.Count,
    aPropertyPointer, aAttributes, aFieldWidth, aData2Text, aText2Data));
end;

procedure TOrmProperties.RegisterCustomRTTIRecordProperty(aTable: TClass;
  aRecordInfo: PRttiInfo; const aName: RawUTF8; aPropertyPointer: pointer;
  aAttributes: TOrmPropInfoAttributes; aFieldWidth: integer;
  aData2Text: TOnSQLPropInfoRecord2Text; aText2Data: TOnSQLPropInfoRecord2Data);
begin
  Fields.Add(TOrmPropInfoRecordRTTI.Create(aRecordInfo, aName, Fields.Count,
    aPropertyPointer, aAttributes, aFieldWidth, aData2Text, aText2Data));
end;

procedure TOrmProperties.RegisterCustomPropertyFromRTTI(aTable: TClass;
  aTypeInfo: PRttiInfo; const aName: RawUTF8; aPropertyPointer: pointer;
  aAttributes: TOrmPropInfoAttributes; aFieldWidth: integer);
begin
  Fields.Add(TOrmPropInfoCustomJSON.Create(aTypeInfo, aName, Fields.Count,
    aPropertyPointer, aAttributes, aFieldWidth));
end;

procedure TOrmProperties.RegisterCustomPropertyFromTypeName(aTable: TClass;
  const aTypeName, aName: RawUTF8; aPropertyPointer: pointer;
  aAttributes: TOrmPropInfoAttributes; aFieldWidth: integer);
begin
  Fields.Add(TOrmPropInfoCustomJSON.Create(aTypeName, aName, Fields.Count,
    aPropertyPointer, aAttributes, aFieldWidth));
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
    raise EModelException.CreateUTF8('% must include %', [self, aTable]);
  result := -1;
end;

procedure TOrmModel.SetTableProps(aIndex: integer);
var
  j, f: PtrInt;
  t: TOrmFieldType;
  Kind: TOrmVirtualKind;
  Table, TableID: TOrmClass;
  aTableName, aFieldName: RawUTF8;
  Props: TOrmModelProperties;
  fields: TOrmPropInfoList;
  W: TTextWriter;

  procedure RegisterTableForRecordReference(aFieldType: TOrmPropInfo;
    aFieldTable: TClass);
  var
    R: integer;
  begin
    if (aFieldTable = nil) or (aFieldTable = TOrm) or
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
      if aFieldType.InheritsFrom(TOrmPropInfoRTTIRecordReference) then
        CascadeDelete := TOrmPropInfoRTTIRecordReference(aFieldType).CascadeDelete;
    end;
  end;

begin
  if (cardinal(aIndex) > cardinal(fTablesMax)) or (fTableProps[aIndex] <> nil) then
    raise EModelException.Create('TOrmModel.SetTableProps');
  Table := fTables[aIndex];
  if Table.InheritsFrom(TOrmFts5) then
    Kind := rFTS5
  else if Table.InheritsFrom(TOrmFts4) then
    Kind := rFTS4
  else if Table.InheritsFrom(TOrmFts3) then
    Kind := rFTS3
  else if Table.InheritsFrom(TOrmVirtualTableForcedID) then
    Kind := rCustomForcedID
  else if Table.InheritsFrom(TOrmRTree) then
    Kind := rRTree
  else if Table.InheritsFrom(TOrmRTreeInteger) then
    Kind := rRTreeInteger
  else if Table.InheritsFrom(TOrmVirtual) then
    Kind := rCustomAutoID
  else
    Kind := rSQLite3;
  Props := TOrmModelProperties.Create(self, Table, Kind);
  Props.Props.InternalRegisterModel(self, aIndex, Props);
  for t := low(t) to high(t) do
    if fCustomCollationForAll[t] <> '' then
      Props.Props.SetCustomCollationForAll(t, fCustomCollationForAll[t]);
  fTableProps[aIndex] := Props;
  aTableName := Props.Props.SQLTableName;
  UpperCaseCopy(aTableName, fSortedTablesNameUpper[aIndex]);
  fSortedTablesNameIndex[aIndex] := aIndex;
  fields := Props.Props.Fields;
  for f := 0 to fields.Count - 1 do
    case fields.List[f].OrmFieldType of
      oftRecord:
        RegisterTableForRecordReference(fields.List[f], Table); // Table not used
      oftID:
        RegisterTableForRecordReference(fields.List[f],
          (fields.List[f] as TOrmPropInfoRTTIInstance).ObjectClass);
      oftTID:
        begin
          TableID := (fields.List[f] as TOrmPropInfoRTTITID).RecordClass;
          if TableID = nil then // T*ID name didn't match any TOrm type
            fields.List[f].fOrmFieldType := oftInteger
          else
            RegisterTableForRecordReference(fields.List[f], TableID);
        end;
      oftMany:
        GetTableIndexSafe(
          pointer((fields.List[f] as TOrmPropInfoRTTIMany).ObjectClass), true);
    end;
  if Props.Props.JoinedFieldsTable <> nil then
  begin
    W := TTextWriter.CreateOwnedStream;
    try
      W.AddShorter('SELECT ');
      // JoinedFieldsTable[0] is the class itself
      with Props.Props do
      begin
        W.Add('%.RowID as `%.RowID`,', [SQLTableName, SQLTableName]);
        for f := 0 to length(SimpleFields) - 1 do
          if SimpleFields[f].OrmFieldType <> oftID then
            W.Add('%.% as `%.%`,', [SQLTableName, SimpleFields[f].Name,
              SQLTableName, SimpleFields[f].Name]);
      end;
      // add JoinedFieldsTable[1..] fields
      for j := 1 to high(Props.Props.JoinedFieldsTable) do
      begin
        aFieldName := Props.Props.JoinedFields[j - 1].Name;
        W.Add('%.RowID as `%.RowID`,', [aFieldName, aFieldName]);
        with Props.Props.JoinedFieldsTable[j].RecordProps do
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
        with Props.Props.JoinedFieldsTable[j].RecordProps do
          W.Add(' LEFT JOIN % AS % ON %.%=%.RowID',
            [SQLTableName, aFieldName, aTableName, aFieldName, aFieldName]);
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
  // first register for JSONToObject() and for TOrmPropInfoRTTITID.Create()
  rtti.RegisterClass(aTable);
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
  QuickSortRawUTF8(fSortedTablesNameUpper, fTablesMax + 1, @fSortedTablesNameIndex);
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
      raise EModelException.CreateUTF8('%.AddTableInherited(%)', [self, aTable]);
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
    raise EModelException.CreateUTF8('%.Create(CloneFrom=nil)', [self]);
  fTables := CloneFrom.fTables;
  fTablesMax := CloneFrom.fTablesMax;
  if fTablesMax <> High(fTables) then
    raise EModelException.CreateUTF8('%.Create: incorrect CloneFrom.TableMax', [self]);
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
  raise EModelException.CreateUTF8(
    'Plain %.Create is not allowed: use overloaded Create()', [self]);
end;

function TOrmModel.SafeRoot: RawUTF8;
begin
  if self = nil then
    result := ''
  else
    result := fRoot;
end;

procedure TOrmModel.SetRoot(const aRoot: RawUTF8);
var
  i: integer;
begin
  for i := 1 to length(aRoot) do // allow RFC URI + '/' for URI-fragment
    if not (aRoot[i] in ['0'..'9', 'a'..'z', 'A'..'Z', '_', '-', '.', '~', ' ', '/']) then
      raise EModelException.CreateUTF8('%.Root="%" contains URI unfriendly chars',
        [self, aRoot]);
  if (aRoot <> '') and (aRoot[length(aRoot)] = '/') then
    fRoot := copy(aRoot, 1, Length(aRoot) - 1)
  else
    fRoot := aRoot;
  UpperCaseCopy(fRoot, fRootUpper);
end;

constructor TOrmModel.Create(const Tables: array of TOrmClass;
  const aRoot: RawUTF8);
var
  N, i: PtrInt;
begin
  N := length(Tables);
  if N > SizeOf(SUPERVISOR_ACCESS_RIGHTS.Get) * 8 then // TOrmAccessRights bits size
    raise EModelException.CreateUTF8('% % has too many Tables: %>%',
      [self, aRoot, N, SizeOf(SUPERVISOR_ACCESS_RIGHTS.Get) * 8]); // e.g. N>64
  // set the Tables to be associated with this Model, as TOrm classes
  fTablesMax := N - 1;
  SetLength(fTables, N);
  MoveFast(Tables[0], fTables[0], N * SizeOf(Tables[0]));
  for i := 0 to N - 1 do
    // first register for JSONToObject() and for TOrmPropInfoRTTITID.Create()
    rtti.RegisterClass(Tables[i]);
  SetLength(fSortedTablesNameUpper, N);
  SetLength(fSortedTablesNameIndex, N);
  SetLength(fTableProps, N);
  // initialize internal properties
  for i := 0 to fTablesMax do
    SetTableProps(i);
  QuickSortRawUTF8(fSortedTablesNameUpper, fTablesMax + 1, @fSortedTablesNameIndex);
  // set the optional Root URI path of this Model
  if aRoot <> '' then
    SetRoot(aRoot);
end;

function TOrmModel.GetIsUnique(aTable: TOrmClass; aFieldIndex: integer): boolean;
var
  i: PtrInt;
begin
  i := GetTableIndex(aTable);
  if (i < 0) or (cardinal(aFieldIndex) >= MAX_SQLFIELDS) then
    result := false
  else
    result := aFieldIndex in TableProps[i].Props.IsUniqueFieldsBits;
end;

function GetTableNameFromSQLSelect(const SQL: RawUTF8;
  EnsureUniqueTableInFrom: boolean): RawUTF8;
var
  i, j, k: integer;
begin
  i := PosI(' FROM ', SQL);
  if i > 0 then
  begin
    inc(i, 6);
    while SQL[i] in [#1..' '] do
      inc(i);
    j := 0;
    while tcIdentifier in TEXT_CHARS[SQL[i + j]] do
      inc(j);
    if cardinal(j - 1) < 64 then
    begin
      k := i + j;
      while SQL[k] in [#1..' '] do
        inc(k);
      if not EnsureUniqueTableInFrom or (SQL[k] <> ',') then
      begin
        FastSetString(result, PAnsiChar(PtrInt(SQL) + i - 1), j);
        exit;
      end;
    end;
  end;
  result := '';
end;

function GetTableNamesFromSQLSelect(const SQL: RawUTF8): TRawUTF8DynArray;
var
  i, j, k, n: integer;
begin
  result := nil;
  n := 0;
  i := PosI(' FROM ', SQL);
  if i > 0 then
  begin
    inc(i, 6);
    repeat
      while SQL[i] in [#1..' '] do
        inc(i);
      j := 0;
      while tcIdentifier in TEXT_CHARS[SQL[i + j]] do
        inc(j);
      if cardinal(j - 1) > 64 then
      begin
        result := nil;
        exit; // seems too big
      end;
      k := i + j;
      while SQL[k] in [#1..' '] do
        inc(k);
      SetLength(result, n + 1);
      FastSetString(result[n], PAnsiChar(PtrInt(SQL) + i - 1), j);
      inc(n);
      if SQL[k] <> ',' then
        break;
      i := k + 1;
    until false;
  end;
end;

function TOrmModel.GetTableIndexFromSQLSelect(const SQL: RawUTF8;
  EnsureUniqueTableInFrom: boolean): integer;
var
  TableName: RawUTF8;
begin
  TableName := GetTableNameFromSQLSelect(SQL, EnsureUniqueTableInFrom);
  result := GetTableIndex(TableName);
end;

function TOrmModel.GetTablesFromSQLSelect(const SQL: RawUTF8): TOrmClassDynArray;
var
  t: TIntegerDynArray;
  n, i: PtrInt;
begin
  result := nil;
  t := GetTableIndexesFromSQLSelect(SQL);
  n := length(t);
  if n = 0 then
    exit;
  SetLength(result, n);
  for i := 0 to n - 1 do
    result[i] := Tables[t[i]];
end;

function TOrmModel.GetTableIndexesFromSQLSelect(const SQL: RawUTF8): TIntegerDynArray;
var
  TableNames: TRawUTF8DynArray;
  i, t, n, ndx: PtrInt;
begin
  result := nil;
  TableNames := GetTableNamesFromSQLSelect(SQL);
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

function TOrmModel.GetTable(const SQLTableName: RawUTF8): TOrmClass;
var
  i: PtrInt;
begin
  i := GetTableIndex(SQLTableName);
  if i >= 0 then
    result := Tables[i]
  else
    result := nil;
end;

function TOrmModel.GetTableExactClass(const TableName: RawUTF8): TOrmClass;
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
  c: POrmClass;
begin
  if (self <> nil) and (aTable <> nil) then
  begin
    Props := aTable.RecordProps;
    if (Props <> nil) and (Props.fModelMax < fTablesMax) then
      // fastest O(1) search in all registered models (if worth it)
      for i := 0 to Props.fModelMax do
        with Props.fModel[i] do
          if Model = self then
          begin
            result := TableIndex; // almost always loop-free
            exit;
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
  if (self <> nil) and (aTable <> nil) and (aTable <> TOrm) then
    for result := 0 to fTablesMax do
      if Tables[result].InheritsFrom(aTable) then
        exit;
  result := -1;
end;

function TOrmModel.GetTableIndexExisting(aTable: TOrmClass): PtrInt;
begin
  if self = nil then
    raise EModelException.Create('nil.GetTableIndexExisting');
  if aTable = nil then
    raise EModelException.CreateUTF8('%.GetTableIndexExisting(nil) %', [self, Root]);
  result := GetTableIndex(aTable);
  if result < 0 then
    raise EModelException.CreateUTF8('% is not part of % %', [aTable, self, Root]);
end;

function TOrmModel.GetTableExactIndex(const TableName: RawUTF8): PtrInt;
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

function TOrmModel.GetTableIndex(const SQLTableName: RawUTF8): PtrInt;
begin
  if (self <> nil) and (SQLTableName <> '') then
  begin
    result := FastFindUpperPUTF8CharSorted( // O(log(n)) binary search
      pointer(fSortedTablesNameUpper), fTablesMax,
      pointer(SQLTableName), length(SQLTableName));
    if result >= 0 then
      result := fSortedTablesNameIndex[result];
  end
  else
    result := -1;
end;

function TOrmModel.GetTableIndexPtr(SQLTableName: PUTF8Char): PtrInt;
begin
  if (self <> nil) and (SQLTableName <> nil) then
  begin
    result := FastFindUpperPUTF8CharSorted( // O(log(n)) binary search
      pointer(fSortedTablesNameUpper), fTablesMax, SQLTableName, StrLen(SQLTableName));
    if result >= 0 then
      result := fSortedTablesNameIndex[result];
  end
  else
    result := -1;
end;

function TOrmModel.GetURI(aTable: TOrmClass): RawUTF8;
begin
  result := '';
  if self = nil then
    exit;
  if aTable <> nil then
    result := aTable.RecordProps.SQLTableName
  else
  begin
    result := Root;
    exit;
  end;
  if Root <> '' then
    result := Root + '/' + result;
end;

function TOrmModel.GetURIID(aTable: TOrmClass; aID: TID): RawUTF8;
begin
  result := GetURI(aTable);
  if aID > 0 then
    result := result + '/' + Int64ToUtf8(aID);
end;

function TOrmModel.GetURICallBack(const aMethodName: RawUTF8;
  aTable: TOrmClass; aID: TID): RawUTF8;
begin
  result := GetURIID(aTable, aID) + '/' + aMethodName;
end;

function TOrmModel.URIMatch(const URI: RawUTF8): TRestModelMatch;
var
  URILen: integer;
begin
  result := rmNoMatch;
  if (self = nil) or (fRoot = '') or (URI = '') then
    exit;
  if IdemPChar(pointer(URI), pointer(fRootUpper)) then
  begin
    URILen := length(fRoot);
    if URI[URILen + 1] in [#0, '/', '?'] then
      if CompareMemFixed(pointer(URI), pointer(fRoot), URILen) then
        result := rmMatchExact
      else
        result := rmMatchWithCaseChange;
  end;
end;

function TOrmModel.SQLFromSelectWhere(const Tables: array of TOrmClass;
  const SQLSelect, SQLWhere: RawUTF8): RawUTF8;
var
  i: PtrInt;
  aProps: array[0..31] of TOrmModelProperties;
begin
  if self = nil then
    raise EOrmException.Create('Model required');
  if high(Tables) = 0 then
  begin
    // fastest common call with one TOrmClass
    result := Props[Tables[0]].SQLFromSelectWhere(SQLSelect, SQLWhere);
    exit;
  end;
  // 'SELECT T1.F1,T1.F2,T1.F3,T2.F1,T2.F2 FROM T1,T2 WHERE ..' e.g.
  if cardinal(high(Tables)) > high(aProps) then
    raise EModelException.CreateUTF8('%.SQLFromSelectWhere() up to % Tables[]',
      [self, Length(aProps)]);
  for i := 0 to high(Tables) do
    aProps[i] := Props[Tables[i]]; // raise EModelException if not found
  if SQLSelect = '*' then
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
    result := 'SELECT ' + SQLSelect;
  result := result + ' FROM ' + aProps[0].Props.SQLTableName;
  for i := 1 to high(Tables) do
    result := result + ',' + aProps[i].Props.SQLTableName;
  result := result + SQLFromWhere(SQLWhere);
end;

procedure TOrmModel.SetCustomCollationForAll(aFieldType: TOrmFieldType;
  const aCollationName: RawUTF8);
var
  i: PtrInt;
begin
  if self = nil then
    exit;
  if fCustomCollationForAll[aFieldType] <> '' then
    raise EModelException.CreateUTF8('%.SetCustomCollationForAll(%)' +
      ' shall be called only once', [self, aCollationName]);
  fCustomCollationForAll[aFieldType] := aCollationName;
  for i := 0 to high(fTableProps) do
    fTableProps[i].fProps.SetCustomCollationForAll(aFieldType, aCollationName);
end;

procedure TOrmModel.SetMaxLengthValidatorForAllTextFields(IndexIsUTF8Length: boolean);
var
  i: PtrInt;
begin
  if self <> nil then
    for i := 0 to high(fTableProps) do
      fTableProps[i].fProps.SetMaxLengthValidatorForTextFields(IndexIsUTF8Length);
end;

procedure TOrmModel.SetMaxLengthFilterForAllTextFields(IndexIsUTF8Length: boolean);
var
  i: PtrInt;
begin
  if self <> nil then
    for i := 0 to high(fTableProps) do
      fTableProps[i].fProps.SetMaxLengthFilterForTextFields(IndexIsUTF8Length);
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
  aIdentifier: TSynUniqueIdentifierProcess;
  const aSharedObfuscationKey: RawUTF8): TSynUniqueIdentifierGenerator;
var
  i: PtrInt;
begin
  i := GetTableIndexExisting(aTable);
  if i >= length(fIDGenerator) then
    SetLength(fIDGenerator, fTablesMax + 1);
  result := TSynUniqueIdentifierGenerator.Create(aIdentifier, aSharedObfuscationKey);
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

function TOrmModel.NewRecord(const SQLTableName: RawUTF8): TOrm;
var
  aClass: TOrmClass;
begin
  aClass := Table[SQLTableName];
  if aClass = nil then
    result := nil
  else
    result := aClass.Create;
end;

function TOrmModel.GetSQLCreate(aTableIndex: integer): RawUTF8;
begin
  if (self = nil) or (cardinal(aTableIndex) > cardinal(fTablesMax)) then
    result := ''
  else
    result := Tables[aTableIndex].GetSQLCreate(self);
end;

function TOrmModel.GetSQLAddField(aTableIndex: integer; aFieldIndex: integer): RawUTF8;
begin
  if (self = nil) or (cardinal(aTableIndex) > cardinal(fTablesMax)) then
    result := ''
  else
    result := TableProps[aTableIndex].Props.SQLAddField(aFieldIndex);
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
  if (self = nil) or (cardinal(aTableIndex) > cardinal(fTablesMax)) then
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
  if (self = nil) or (fLocks = nil) then
    result := false
  else
    result := GetLocks(aTable)^.UnLock(aID);
end;

function TOrmModel.UnLock(aTableIndex: integer; aID: TID): boolean;
begin
  if (self = nil) or (cardinal(aTableIndex) >= cardinal(length(fLocks))) then
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
  if (self = nil) or (fLocks = nil) then
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
  if (self = nil) or (ID <= 0) then
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
  i: integer;
begin
  i := Ref and 63;
  if i <= fTablesMax then
    result := fTables[i]
  else
    result := nil;
end;

function TOrmModel.VirtualTableRegister(aClass: TOrmClass; aModule: TClass;
  const aExternalTableName: RawUTF8; aExternalDataBase: TObject;
  aMappingOptions: TOrmPropertiesMappingOptions): boolean;
var
  i: PtrInt;
begin
  result := false;
  if aClass = nil then
    exit;
  if (aModule = nil) or not Assigned(GetVirtualTableModuleName) or
     (GetVirtualTableModuleName(aModule) = '') then
    raise EModelException.CreateUTF8('Unexpected %.VirtualTableRegister(%,%)',
      [self, aClass, aModule]);
  i := GetTableIndexExisting(aClass);
  with TableProps[i] do
  begin
    if not (Kind in IS_CUSTOM_VIRTUAL) then
      if Kind = rSQLite3 then
        SetKind(rCustomAutoID) // SetKind() recompute all SQL
      else
        raise EModelException.CreateUTF8('Invalid %.VirtualTableRegister(%) call: ' +
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
  if (self = nil) or (fVirtualTableModule = nil) then
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
  begin
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
  end;
  ObjArrayClear(fIDGenerator);
  inherited;
end;


{ TOrmModelProperties }

constructor TOrmModelProperties.Create(aModel: TOrmModel; aTable:
  TOrmClass; aKind: TOrmVirtualKind);
var
  f: PtrInt;
begin // similar to TOrmPropertiesMapping.ComputeSQL
  fModel := aModel;
  fTableIndex := fModel.GetTableIndexExisting(aTable);
  fProps := aTable.RecordProps;
  SetKind(aKind);
  with Props do
    for f := 0 to Fields.Count - 1 do
      with Fields.List[f] do
        if OrmFieldType in COPIABLE_FIELDS then
        begin // oftMany fields do not exist
          // pre-computation of SQL statements
          SQL.UpdateSetAll := SQL.UpdateSetAll + Name + '=?,';
          SQL.InsertSet := SQL.InsertSet + Name + ',';
          if f in SimpleFieldsBits[ooUpdate] then
            SQL.UpdateSetSimple := SQL.UpdateSetSimple + Name + '=?,';
          // filter + validation of unique fields, i.e. if marked as "stored false"
          if f in IsUniqueFieldsBits then
          begin
            // must trim() text value before storage, and validate for unicity
            if OrmFieldType in [oftUTF8Text, oftAnsiText] then
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

  function InTOrmTableSimpleFields(withID, withTableName: boolean): RawUTF8;
  const
    IDComma: array[TOrmVirtualKind] of rawUTF8 = ('ID,', 'RowID,',
      'RowID,', 'RowID,', 'RowID,', 'RowID,', 'RowID,', 'RowID,');
 // rSQLite3,rFTS3,rFTS4,rFTS5,rRTree,rRTreeInteger,rCustomForcedID,rCustomAutoID
  var
    TableName: RawUTF8;
    i: PtrInt;
  begin
    if withTableName then
      TableName := Props.SQLTableName + '.'; // calc TableName once
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
    rFTS3, rFTS4, rFTS5:
      begin
        if Props.Fields.Count = 0 then
          raise EModelException.CreateUTF8(
            'Virtual FTS class % should have published properties', [Props.Table]);
        for f := 0 to Props.Fields.Count - 1 do
          with Props.Fields.List[f] do
            if OrmFieldTypeStored <> oftUTF8Text then
              raise EModelException.CreateUTF8('%.%: FTS field must be RawUTF8',
                [Props.Table, Name])
      end;
    rRTree, rRTreeInteger:
      begin
        Props.RTreeCoordBoundaryFields := 0;
        if Value = rRTree then
          expected := oftFloat
        else
          expected := oftInteger;
        for f := 0 to Props.Fields.Count - 1 do
          with Props.Fields.List[f] do
            if aAuxiliaryRTreeField in Attributes then // https://sqlite.org/rtree.html#auxiliary_columns
              expected := oftUnknown // will expect further columns to be auxiliary
            else if OrmFieldTypeStored <> expected then
              raise EModelException.CreateUTF8('%.%: RTREE field must be %',
                [Props.Table, Name, ToText(expected)^])
            else
              inc(Props.RTreeCoordBoundaryFields);
        if (Props.RTreeCoordBoundaryFields < 2) or
           (Props.RTreeCoordBoundaryFields > RTREE_MAX_DIMENSION * 2) or
           (Props.RTreeCoordBoundaryFields and 1 <> 0) then
          raise EModelException.CreateUTF8(
            '% has % fields: RTREE expects 2,4,6..% boundary columns',
            [Props.Table, Props.RTreeCoordBoundaryFields, RTREE_MAX_DIMENSION * 2]);
      end;
  end;
  fKind := Value;
  // SQL.TableSimpleFields[withID: boolean; withTableName: boolean]
  SQL.TableSimpleFields[false, false] := InTOrmTableSimpleFields(false, false);
  SQL.TableSimpleFields[false, true]  := InTOrmTableSimpleFields(false, true);
  SQL.TableSimpleFields[true, false]  := InTOrmTableSimpleFields(true, false);
  SQL.TableSimpleFields[true, true]   := InTOrmTableSimpleFields(true, true);
  if Props.SQLTableSimpleFieldsNoRowID <> SQL.TableSimpleFields[false, false] then
    raise EModelException.CreateUTF8('SetKind(%)', [Props.Table]);
  SQL.SelectAllWithRowID := SQLFromSelectWhere('*', '');
  SQL.SelectAllWithID := SQL.SelectAllWithRowID;
  if IdemPChar(PUTF8Char(pointer(SQL.SelectAllWithID)) + 7, 'ROWID') then
    delete(SQL.SelectAllWithID, 8, 3); // 'SELECT RowID,..' -> 'SELECT ID,'
end;

function TOrmModelProperties.SQLFromSelectWhere(
  const SelectFields, Where: RawUTF8): RawUTF8;
begin
  result := SQLFromSelect(Props.SQLTableName, SelectFields, Where,
    SQL.TableSimpleFields[true, false]);
end;

procedure TOrmModelProperties.FTS4WithoutContent(ContentTable: TOrmClass);
var
  i: PtrInt;
  field: RawUTF8;
begin
  if not (Kind in [rFTS4, rFTS5]) then
    raise EModelException.CreateUTF8('FTS4WithoutContent: % is not a FTS4/5 table',
      [Props.Table]);
  fFTSWithoutContentTableIndex := fModel.GetTableIndexExisting(ContentTable);
  for i := 0 to Props.Fields.Count - 1 do
  begin
    field := Props.Fields.List[i].Name;
    if ContentTable.RecordProps.Fields.IndexByName(field) < 0 then
      raise EModelException.CreateUTF8('FTS4WithoutContent: %.% is not a % field',
        [Props.Table, field, ContentTable]);
    fFTSWithoutContentFields := fFTSWithoutContentFields + ',new.' + field;
  end;
  if fFTSWithoutContentFields = '' then
    raise EModelException.CreateUTF8('FTS4WithoutContent: % has no field', [Props.Table]);
end;

function TOrmModelProperties.GetProp(const PropName: RawUTF8): TOrmPropInfo;
begin
  if self <> nil then
    result := Props.Fields.ByName(pointer(PropName))
  else
    result := nil;
end;



{ TOrmPropertiesMapping }

procedure TOrmPropertiesMapping.Init(Table: TOrmClass;
  const MappedTableName: RawUTF8; MappedConnection: TObject; AutoComputeSQL: boolean;
  MappingOptions: TOrmPropertiesMappingOptions);
begin
  // set associated properties
  fProps := Table.RecordProps;
  if MappedTableName = '' then
    fTableName := fProps.SQLTableName
  else
    fTableName := MappedTableName;
  fConnectionProperties := MappedConnection;
  fOptions := MappingOptions;
  fAutoComputeSQL := AutoComputeSQL;
  // setup default values
  fRowIDFieldName := 'ID';
  fProps.Fields.NamesToRawUTF8DynArray(fExtFieldNames);
  fProps.Fields.NamesToRawUTF8DynArray(fExtFieldNamesUnQuotedSQL);
  FillcharFast(fFieldNamesMatchInternal, SizeOf(fFieldNamesMatchInternal), 255);
  fMappingVersion := 1;
  if fAutoComputeSQL then
    ComputeSQL;
end;

function TOrmPropertiesMapping.MapField(
  const InternalName, ExternalName: RawUTF8): POrmPropertiesMapping;
begin
  MapFields([InternalName, ExternalName]);
  result := @self;
end;

function TOrmPropertiesMapping.MapFields(
  const InternalExternalPairs: array of RawUTF8): POrmPropertiesMapping;
var
  i, int: PtrInt;
begin
  for i := 0 to (length(InternalExternalPairs) shr 1) - 1 do
  begin
    int := fProps.Fields.IndexByNameOrExcept(InternalExternalPairs[i * 2]);
    if int < 0 then
    begin
      fRowIDFieldName := InternalExternalPairs[i * 2 + 1];
      if IdemPropNameU(fRowIDFieldName, 'ID') then
        include(fFieldNamesMatchInternal, 0)
      else     // [0]=ID
        exclude(fFieldNamesMatchInternal, 0);
    end
    else
    begin
      fExtFieldNames[int] := InternalExternalPairs[i * 2 + 1];
      fExtFieldNamesUnQuotedSQL[int] := UnQuotedSQLSymbolName(fExtFieldNames[int]);
      if IdemPropNameU(fExtFieldNames[int], fProps.Fields.List[int].Name) then
        include(fFieldNamesMatchInternal, int + 1)
      else // [0]=ID  [1..n]=fields[i-1]
        exclude(fFieldNamesMatchInternal, int + 1);
    end;
  end;
  inc(fMappingVersion);
  if fAutoComputeSQL then
    ComputeSQL;
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

procedure TOrmPropertiesMapping.ComputeSQL;

type
  // similar to TOrmModelProperties.Create()/SetKind()
  TComputeSQLContent = (
    cTableSimpleFields, cUpdateSimple, cUpdateSetAll, cInsertAll);

  procedure SetSQL(W: TTextWriter; withID, withTableName: boolean;
    var result: RawUTF8; content: TComputeSQLContent = cTableSimpleFields);
  var
    f: PtrInt;
  begin
    W.CancelAll;
    if withID and (content = cTableSimpleFields) then
    begin
      if withTableName then
        W.AddStrings([TableName, '.']);
      W.AddString(RowIDFieldName);
      if 0 in FieldNamesMatchInternal then
        W.Add(',')
      else
        W.AddShorter(' as ID,');
    end;
    with fProps do
      for f := 0 to Fields.Count - 1 do
        with Fields.List[f] do
          if OrmFieldType in COPIABLE_FIELDS then // oftMany fields do not exist
            case content of
              cTableSimpleFields:
                if f in SimpleFieldsBits[ooSelect] then
                begin
                  if withTableName then
                    W.AddStrings([TableName, '.']);
                  W.AddString(ExtFieldNames[f]);
                  if not (f + 1 in FieldNamesMatchInternal) then
                    // to get expected JSON column name
                    W.AddStrings([' as ', Name]);
                  W.Add(',');
                end;
              cUpdateSimple:
                if f in SimpleFieldsBits[ooSelect] then
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
  W: TTextWriter;
  temp: TTextWriterStackBuffer;
begin
  W := TTextWriter.CreateOwnedStream(temp);
  try // SQL.TableSimpleFields[withID: boolean; withTableName: boolean]
    SetSQL(W, false, false, fSQL.TableSimpleFields[false, false]);
    SetSQL(W, false, true, fSQL.TableSimpleFields[false, true]);
    SetSQL(W, true, false, fSQL.TableSimpleFields[true, false]);
    SetSQL(W, true, true, fSQL.TableSimpleFields[true, true]);
    // SQL.SelectAll: array[withRowID: boolean]
    fSQL.SelectAllWithRowID := SQLFromSelect(
      TableName, '*', '', fSQL.TableSimpleFields[true, false]);
    fSQL.SelectAllWithID := fSQL.SelectAllWithRowID;
    SetSQL(W, false, false, fSQL.UpdateSetSimple, cUpdateSimple);
    SetSQL(W, false, false, fSQL.UpdateSetAll, cUpdateSetAll);
    SetSQL(W, false, false, fSQL.InsertSet, cInsertAll);
  finally
    W.Free;
  end;
end;

function TOrmPropertiesMapping.InternalToExternal(
  const FieldName: RawUTF8): RawUTF8;
var
  int: PtrInt;
begin
  int := fProps.Fields.IndexByNameOrExcept(FieldName);
  if int < 0 then
    result := RowIDFieldName
  else
    result := fExtFieldNames[int];
end;

function TOrmPropertiesMapping.InternalToExternal(BlobField: PRttiProp): RawUTF8;
var
  int: PtrInt;
begin
  int := fProps.Fields.IndexByNameOrExceptShort(BlobField.Name^);
  if int < 0 then
    result := RowIDFieldName
  else
    result := fExtFieldNames[int];
end;

function TOrmPropertiesMapping.InternalCSVToExternalCSV(
  const CSVFieldNames, Sep, SepEnd: RawUTF8): RawUTF8;
var
  IntFields, ExtFields: TRawUTF8DynArray;
begin
  CSVToRawUTF8DynArray(CSVFieldNames, Sep, SepEnd, IntFields);
  InternalToExternalDynArray(IntFields, ExtFields);
  result := RawUTF8ArrayToCSV(ExtFields, Sep) + SepEnd;
end;

procedure TOrmPropertiesMapping.InternalToExternalDynArray(
  const IntFieldNames: array of RawUTF8; out result: TRawUTF8DynArray;
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
  const ExtFieldName: RawUTF8): integer;
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
  const ExtFieldName: RawUTF8): RawUTF8;
var
  i: PtrInt;
begin
  i := ExternalToInternalIndex(ExtFieldName);
  if i = -1 then
    result := 'ID'
  else if i >= 0 then
    result := fProps.Fields.List[i].Name
  else
    result := ''; // indicates not found
end;

function TOrmPropertiesMapping.AppendFieldName(FieldIndex: integer;
  var Text: RawUTF8): boolean;
begin
  result := false; // success
  if FieldIndex = VIRTUAL_TABLE_ROWID_COLUMN then
    Text := Text + RowIDFieldName
  else if cardinal(FieldIndex) >= cardinal(Length(ExtFieldNames)) then
    result := true
  else // FieldIndex out of range
    Text := Text + ExtFieldNames[FieldIndex];
end;

function TOrmPropertiesMapping.FieldNameByIndex(FieldIndex: integer): RawUTF8;
begin
  if FieldIndex = VIRTUAL_TABLE_ROWID_COLUMN then
    result := RowIDFieldName
  else if cardinal(FieldIndex) >= cardinal(Length(ExtFieldNames)) then
    result := ''
  else // FieldIndex out of range
    result := ExtFieldNames[FieldIndex];
end;


{ TOrmLocks }

function TOrmLocks.isLocked(aID: TID): boolean;
begin
  result := (@self <> nil) and (Count <> 0) and (aID <> 0) and
            Int64ScanExists(pointer(IDs), Count, aID);
end;

function TOrmLocks.Lock(aID: TID): boolean;
var
  P: PInt64;
begin
  if (@self = nil) or (aID = 0) then
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
  if (@self = nil) or (Count = 0) then
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
  if (@self = nil) or (Count = 0) or (aID = 0) then
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


{ ------------ TRestCache Definition }

{ TRestCacheEntry }

procedure TRestCacheEntry.Init;
begin
  Value.InitSpecific(TypeInfo(TRestCacheEntryValueDynArray),
    Values, ptInt64, @Count); // will search/sort by first ID: TID field
  Mutex.Init;
end;

procedure TRestCacheEntry.Done;
begin
  Mutex.Done;
end;

procedure TRestCacheEntry.Clear;
begin
  Mutex.Lock;
  try
    Value.Clear;
    CacheAll := false;
    CacheEnable := false;
    TimeOutMS := 0;
  finally
    Mutex.UnLock;
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
        JSON := '';
        Tag := 0;
      end;
end;

procedure TRestCacheEntry.FlushCacheAllEntries;
var
  i: PtrInt;
begin
  if not CacheEnable then
    exit;
  Mutex.Lock;
  try
    if CacheAll then
      Value.Clear
    else
      for i := 0 to Count - 1 do
        with Values[i] do
        begin
          Timestamp512 := 0;
          JSON := '';
          Tag := 0;
        end;
  finally
    Mutex.UnLock;
  end;
end;

procedure TRestCacheEntry.SetCache(aID: TID);
var
  Rec: TRestCacheEntryValue;
  i: integer;
begin
  Mutex.Lock;
  try
    CacheEnable := true;
    if not CacheAll and not Value.FastLocateSorted(aID, i) and (i >= 0) then
    begin
      Rec.ID := aID;
      Rec.Timestamp512 := 0; // indicates no value cache yet
      Rec.Tag := 0;
      Value.FastAddSorted(i, Rec);
    end; // do nothing if aID is already in Values[]
  finally
    Mutex.UnLock;
  end;
end;

procedure TRestCacheEntry.SetJSON(aID: TID; const aJSON: RawUTF8; aTag: cardinal);
var
  Rec: TRestCacheEntryValue;
  i: integer;
begin
  Rec.ID := aID;
  Rec.JSON := aJSON;
  Rec.Timestamp512 := GetTickCount64 shr 9;
  Rec.Tag := aTag;
  Mutex.Lock;
  try
    if Value.FastLocateSorted(Rec, i) then
      Values[i] := Rec
    else if CacheAll and (i >= 0) then
      Value.FastAddSorted(i, Rec);
  finally
    Mutex.UnLock;
  end;
end;

procedure TRestCacheEntry.SetJSON(aRecord: TOrm);
begin  // ooInsert = include all fields
  SetJSON(aRecord.fID, aRecord.GetJSONValues(true, false, ooInsert));
end;

function TRestCacheEntry.RetrieveJSON(aID: TID; var aJSON: RawUTF8;
  aTag: PCardinal): boolean;
var
  i: PtrInt;
begin
  result := false;
  Mutex.Lock;
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
            aJSON := JSON;
            result := true; // found a non outdated serialized value in cache
          end;
  finally
    Mutex.UnLock;
  end;
end;

function TRestCacheEntry.RetrieveJSON(aID: TID; aValue: TOrm;
  aTag: PCardinal): boolean;
var
  JSON: RawUTF8;
begin
  if RetrieveJSON(aID, JSON, aTag) then
  begin
    aValue.FillFrom(JSON);
    aValue.fID := aID; // override RowID field (may be not present after Update)
    result := true;
  end
  else
    result := false;
end;

function TRestCacheEntry.CachedMemory(FlushedEntriesCount: PInteger): cardinal;
var
  i: PtrInt;
  tix512: cardinal;
begin
  result := 0;
  if CacheEnable and (Count > 0) then
  begin
    tix512 := (GetTickCount64 - TimeOutMS) shr 9;
    Mutex.Lock;
    try
      for i := Count - 1 downto 0 do
        with Values[i] do
          if Timestamp512 <> 0 then
            if (TimeOutMS <> 0) and (tix512 > Timestamp512) then
            begin
              FlushCacheEntry(i);
              if FlushedEntriesCount <> nil then
                inc(FlushedEntriesCount^);
            end
            else
              inc(result, length(JSON) + (SizeOf(TRestCacheEntryValue) + 16));
    finally
      Mutex.UnLock;
    end;
  end;
end;


{ TRestCache }

constructor TRestCache.Create(const aRest: IRestOrm);
var
  i: PtrInt;
begin
  if aRest = nil then
    EOrmException.CreateUTF8('%.Create', [self]);
  fRest := aRest;
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
  inherited;
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
          Mutex.Lock;
          try
            for j := 0 to Count - 1 do
              if Values[j].Timestamp512 <> 0 then
                inc(result);
          finally
            Mutex.UnLock;
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
  if (self = nil) or (aTable = nil) then
    exit;
  i := fModel.GetTableIndexExisting(aTable);
  if Rest.CacheWorthItForTable(i) then
    if PtrUInt(i) < PtrUInt(Length(fCache)) then
      with fCache[i] do
      begin
        Mutex.Lock;
        try
          TimeOutMS := aTimeoutMS;
        finally
          Mutex.UnLock;
        end;
        result := true;
      end;
end;

function TRestCache.IsCached(aTable: TOrmClass): boolean;
var
  i: PtrUInt;
begin
  result := false;
  if (self = nil) or (aTable = nil) then
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
  if (self = nil) or (aTable = nil) then
    exit;
  i := fModel.GetTableIndexExisting(aTable);
  if Rest.CacheWorthItForTable(i) then
    if PtrUInt(i) < PtrUInt(Length(fCache)) then
      with fCache[i] do
      begin
        // global cache of all records of this table
        Mutex.Lock;
        try
          CacheEnable := true;
          CacheAll := true;
          Value.Clear;
          result := true;
        finally
          Mutex.UnLock;
        end;
      end;
end;

function TRestCache.SetCache(aTable: TOrmClass; aID: TID): boolean;
var
  i: PtrInt;
begin
  result := false;
  if (self = nil) or (aTable = nil) or (aID <= 0) then
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
  if (self = nil) or (aTable = nil) or (length(aIDs) = 0) then
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
  if (self = nil) or (aRecord = nil) or (aRecord.fID <= 0) then
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
  const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const): integer;
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
  rec := aTable.CreateAndFillPrepare(fRest, FormatSQLWhere, BoundsSQLWhere);
  try
    while rec.FillOne do
    begin
      cache^.SetJSON(rec);
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
        Mutex.Lock;
        try
          FlushCacheEntry(Value.Find(aID));
        finally
          Mutex.UnLock;
        end;
      end;
end;

procedure TRestCache.Flush(aTable: TOrmClass; const aIDs: array of TID);
var
  i: PtrInt;
begin
  if (self <> nil) and (length(aIDs) > 0) then
    with fCache[fModel.GetTableIndexExisting(aTable)] do
      if CacheEnable then
      begin
        Mutex.Lock;
        try
          for i := 0 to high(aIDs) do
            FlushCacheEntry(Value.Find(aIDs[i]));
        finally
          Mutex.UnLock;
        end;
      end;
end;

procedure TRestCache.Notify(aTable: TOrmClass; aID: TID;
  const aJSON: RawUTF8; aAction: TOrmOccasion);
begin
  if (self <> nil) and (aTable <> nil) and (aID > 0) then
    Notify(fModel.GetTableIndex(aTable), aID, aJSON, aAction);
end;

procedure TRestCache.Notify(aRecord: TOrm; aAction: TOrmOccasion);
var
  aTableIndex: cardinal;
begin
  if (self = nil) or (aRecord = nil) or (aRecord.fID <= 0) or
     not (aAction in [ooInsert, ooUpdate]) then
    exit;
  aTableIndex := fModel.GetTableIndex(POrmClass(aRecord)^);
  if aTableIndex < cardinal(Length(fCache)) then
    with fCache[aTableIndex] do
      if CacheEnable then
        SetJSON(aRecord);
end;

procedure TRestCache.Notify(aTableIndex: integer; aID: TID;
  const aJSON: RawUTF8; aAction: TOrmOccasion);
begin
  if (self <> nil) and (aID > 0) and
     (aAction in [ooSelect, ooInsert, ooUpdate]) and
     (aJSON <> '') and (cardinal(aTableIndex) < cardinal(Length(fCache))) then
    with fCache[aTableIndex] do
      if CacheEnable then
        SetJSON(aID, aJSON);
end;

procedure TRestCache.NotifyDeletion(aTableIndex: integer; aID: TID);
begin
  if (self <> nil) and (aID > 0) and
     (cardinal(aTableIndex) < cardinal(Length(fCache))) then
    with fCache[aTableIndex] do
      if CacheEnable then
      begin
        Mutex.Lock;
        try
          FlushCacheEntry(Value.Find(aID));
        finally
          Mutex.UnLock;
        end;
      end;
end;

procedure TRestCache.NotifyDeletions(aTableIndex: integer;
  const aIDs: array of Int64);
var
  i: PtrInt;
begin
  if (self <> nil) and (high(aIDs) >= 0) and
     (cardinal(aTableIndex) < cardinal(Length(fCache))) then
    with fCache[aTableIndex] do
      if CacheEnable then
      begin
        Mutex.Lock;
        try
          for i := 0 to high(aIDs) do
            FlushCacheEntry(Value.Find(aIDs[i]));
        finally
          Mutex.UnLock;
        end;
      end;
end;

procedure TRestCache.NotifyDeletion(aTable: TOrmClass; aID: TID);
begin
  if (self <> nil) and
     (aTable <> nil) and (aID > 0) then
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
    with fCache[TableIndex] do
      if CacheEnable and RetrieveJSON(aID, aValue) then
        result := true;
end;

function TRestCache.Retrieve(aTableIndex: integer; aID: TID): RawUTF8;
begin
  result := '';
  if (self <> nil) and
     (aID > 0) and
     (cardinal(aTableIndex) < cardinal(Length(fCache))) then
    with fCache[aTableIndex] do
      if CacheEnable then
        RetrieveJSON(aID, result);
end;


{ ------------ TRestBatch TRestBatchLocked Definitions }

{ TRestBatch }

constructor TRestBatch.Create(const aRest: IRestOrm; aTable: TOrmClass;
  AutomaticTransactionPerRow: cardinal; Options: TRestBatchOptions;
  InternalBufferSize: cardinal; CalledWithinRest: boolean);
begin
  if aRest = nil then
    raise EOrmException.CreateUTF8('%.Create(aRest=nil)', [self]);
  fRest := aRest;
  fModel := fRest.Model;
  if InternalBufferSize < 4096 then
    InternalBufferSize := 4096;
  fInternalBufferSize := InternalBufferSize;
  fCalledWithinRest := CalledWithinRest;
  Reset(aTable, AutomaticTransactionPerRow, Options);
end;

procedure TRestBatch.Reset(aTable: TOrmClass;
  AutomaticTransactionPerRow: cardinal; Options: TRestBatchOptions);
begin
  fBatch.Free; // full reset for SetExpandedJSONWriter
  fBatch := TJSONSerializer.CreateOwnedStream(fInternalBufferSize);
  fBatch.Expand := true;
  FillZero(fBatchFields);
  fBatchCount := 0;
  fAddCount := 0;
  fUpdateCount := 0;
  fDeleteCount := 0;
  fDeletedCount := 0;
  fTable := aTable;
  if aTable <> nil then
  begin
    fTableIndex := fModel.GetTableIndexExisting(aTable);
    fBatch.Add('{'); // sending data is '{"Table":["cmd":values,...]}'
    fBatch.AddFieldName(aTable.SQLTableName);
  end
  else
    fTableIndex := -1;
  fBatch.Add('[');
  fAutomaticTransactionPerRow := AutomaticTransactionPerRow;
  if AutomaticTransactionPerRow > 0 then
  begin // should be the first command
    fBatch.AddShort('"automaticTransactionPerRow",');
    fBatch.Add(AutomaticTransactionPerRow);
    fBatch.Add(',');
  end;
  fOptions := Options;
  if boExtendedJSON in Options then
    include(fBatch.fCustomOptions, twoForceJSONExtended);
  Options := Options - [boExtendedJSON, boPostNoSimpleFields]; // client-side only
  if byte(Options) <> 0 then
  begin
    fBatch.AddShort('"options",');
    fBatch.Add(byte(Options));
    fBatch.Add(',');
  end;
end;

procedure TRestBatch.Reset;
begin
  if self <> nil then
    Reset(fTable, fAutomaticTransactionPerRow, fOptions);
end;

destructor TRestBatch.Destroy;
begin
  FreeAndNil(fBatch);
  inherited;
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

procedure TRestBatch.SetExpandedJSONWriter(Props: TOrmProperties;
  ForceResetFields, withID: boolean; const WrittenFields: TFieldBits);
begin
  if (self = nil) or
     (fBatch = nil) then
    exit;
  if not ForceResetFields then
    if fBatch.Expand and (fBatch.WithID = withID) and
       IsEqual(fBatchFields, WrittenFields) then
      exit; // already set -> do not compute it again
  fBatchFields := WrittenFields;
  fBatch.ChangeExpandedFields(withID,
    FieldBitsToIndex(WrittenFields, Props.Fields.Count));
  Props.SetJSONWriterColumnNames(fBatch, 0);
end;

function TRestBatch.RawAppend(FullRow: boolean): TTextWriter;
begin
  if FullRow then
    inc(fBatchCount);
  result := fBatch;
end;

function TRestBatch.RawAdd(const SentData: RawUTF8): integer;
begin // '{"Table":[...,"POST",{object},...]}'
  if (fBatch = nil) or
     (fTable = nil) then
    raise EOrmException.CreateUTF8('%.RawAdd %', [self, SentData]);
  fBatch.AddShorter('"POST",');
  fBatch.AddString(SentData);
  fBatch.Add(',');
  result := fBatchCount;
  inc(fBatchCount);
  inc(fAddCount);
end;

function TRestBatch.RawUpdate(const SentData: RawUTF8; ID: TID): integer;
var
  sentID: TID;
begin // '{"Table":[...,"PUT",{object},...]}'
  if (fBatch = nil) or
     (fTable = nil) then
    raise EOrmException.CreateUTF8('%.RawUpdate % %', [self, ID, SentData]);
  if JSONGetID(pointer(SentData), sentID) and (sentID <> ID) then
    raise EOrmException.CreateUTF8('%.RawUpdate ID=% <> %', [self, ID, SentData]);
  fBatch.AddShort('"PUT",{ID:');
  fBatch.Add(ID);
  fBatch.Add(',');
  fBatch.AddStringCopy(SentData, 2, maxInt shr 2);
  fBatch.Add(',');
  result := fBatchCount;
  inc(fBatchCount);
  inc(fUpdateCount);
end;

function TRestBatch.Add(Value: TOrm; SendData, ForceID: boolean;
  const CustomFields: TFieldBits; DoNotAutoComputeFields: boolean): integer;
var
  Props: TOrmProperties;
  FieldBits: TFieldBits;
  PostSimpleFields: boolean;
  f: PtrInt;
begin
  result := -1;
  if (self = nil) or
     (Value = nil) or
     (fBatch = nil) then
    exit; // invalid parameters, or not opened BATCH sequence
  if (fTable <> nil) and
     (POrmClass(Value)^ <> fTable) then
    exit;
  Props := Value.RecordProps;
  if SendData and
     (fModel.Props[POrmClass(Value)^].Kind in INSERT_WITH_ID) then
    ForceID := true; // same format as TRestClient.Add
  if SendData and not ForceID and IsZero(CustomFields) and
     not (boPostNoSimpleFields in fOptions) then
  begin
    PostSimpleFields := true;
    fBatch.AddShorter('"SIMPLE');
  end
  else
  begin
    PostSimpleFields := false;
    fBatch.AddShorter('"POST');
  end;
  if fTable <> nil then  // '{"Table":[...,"POST",{object},...]}'
    fBatch.AddShorter('",')
  else
  begin
    fBatch.Add('@'); // '[...,"POST@Table",{object}',...]'
    fBatch.AddString(Props.SQLTableName);
    fBatch.Add('"', ',');
  end;
  if SendData then
  begin
    if IsZero(CustomFields) then
      FieldBits := Props.SimpleFieldsBits[ooInsert]
    else if DoNotAutoComputeFields then
      FieldBits := CustomFields
    else
      FieldBits := CustomFields + Props.ComputeBeforeAddFieldsBits;
    SetExpandedJSONWriter(Props, fTablePreviousSendData <> POrmClass(Value)^,
      (Value.IDValue <> 0) and ForceID, FieldBits);
    fTablePreviousSendData := POrmClass(Value)^;
    if not DoNotAutoComputeFields then // update TModTime/TCreateTime fields
      Value.ComputeFieldsBeforeWrite(fRest, oeAdd);
    if PostSimpleFields then
    begin
      fBatch.Add('[');
      for f := 0 to length(Props.SimpleFields) - 1 do
      begin
        Props.SimpleFields[f].GetJSONValues(Value, fBatch);
        fBatch.Add(',');
      end;
      fBatch.CancelLastComma;
      fBatch.Add(']');
    end
    else
      Value.GetJSONValues(fBatch);
    if fCalledWithinRest and ForceID then
      fRest.CacheOrNil.Notify(Value, ooInsert);
  end
  else
    fBatch.Add('{', '}'); // '{"Table":[...,"POST",{},...]}'
  fBatch.Add(',');
  result := fBatchCount;
  inc(fBatchCount);
  inc(fAddCount);
  if Assigned(fOnWrite) then
    fOnWrite(self, ooInsert, POrmClass(Value)^, Value.IDValue, Value,
      FieldBits{%H-});
end;

function TRestBatch.Delete(Table: TOrmClass; ID: TID): integer;
begin
  if (self = nil) or
     (fBatch = nil) or
     (Table = nil) or
     (ID <= 0) or
     not fRest.RecordCanBeUpdated(Table, ID, oeDelete) then
  begin
    result := -1; // invalid parameters, or not opened BATCH sequence
    exit;
  end;
  AddID(fDeletedRecordRef, fDeletedCount, fModel.RecordReference(Table, ID));
  fBatch.AddShort('"DELETE@'); // '[...,"DELETE@Table",ID,...]}'
  fBatch.AddString(Table.RecordProps.SQLTableName);
  fBatch.Add('"', ',');
  fBatch.Add(ID);
  fBatch.Add(',');
  result := fBatchCount;
  inc(fBatchCount);
  inc(fDeleteCount);
  if Assigned(fOnWrite) then
    fOnWrite(self, ooDelete, Table, ID, nil, []);
end;

function TRestBatch.Delete(ID: TID): integer;
begin
  if (self = nil) or
     (fTable = nil) or (ID <= 0) or
     not fRest.RecordCanBeUpdated(fTable, ID, oeDelete) then
  begin
    result := -1; // invalid parameters, or not opened BATCH sequence
    exit;
  end;
  AddID(fDeletedRecordRef, fDeletedCount, RecordReference(fTableIndex, ID));
  fBatch.AddShort('"DELETE",'); // '{"Table":[...,"DELETE",ID,...]}'
  fBatch.Add(ID);
  fBatch.Add(',');
  result := fBatchCount;
  inc(fBatchCount);
  inc(fDeleteCount);
  if Assigned(fOnWrite) then
    fOnWrite(self, ooDelete, fTable, ID, nil, []);
end;

function TRestBatch.PrepareForSending(out Data: RawUTF8): boolean;
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
      for i := 0 to fDeletedCount - 1 do
        if fDeletedRecordRef[i] <> 0 then
          fRest.CacheOrNil.NotifyDeletion(fDeletedRecordRef[i] and 63,
            fDeletedRecordRef[i] shr 6);
      fBatch.CancelLastComma;
      fBatch.Add(']');
      if fTable <> nil then
        fBatch.Add('}'); // end sequence array '{"Table":["cmd":values,...]}'
      fBatch.SetText(Data);
    end;
    result := true;
  end;
end;

function TRestBatch.Update(Value: TOrm; const CustomFields: TFieldBits;
  DoNotAutoComputeFields, ForceCacheUpdate: boolean): integer;
var
  Props: TOrmProperties;
  FieldBits: TFieldBits;
  ID: TID;
  tableIndex: integer;
begin
  result := -1;
  if (Value = nil) or
     (fBatch = nil) then
    exit;
  ID := Value.IDValue;
  if (ID <= 0) or
     not fRest.RecordCanBeUpdated(Value.RecordClass, ID, oeUpdate) then
    exit; // invalid parameters, or not opened BATCH sequence
  Props := Value.RecordProps;
  if fTable <> nil then
    if POrmClass(Value)^ <> fTable then
      exit
    else
    begin // '{"Table":[...,"PUT",{object},...]}'
      tableIndex := fTableIndex;
      fBatch.AddShorter('"PUT",');
    end
  else
  begin
    tableIndex := fModel.GetTableIndexExisting(Props.Table);
    fBatch.AddShorter('"PUT@'); // '[...,"PUT@Table",{object}',...]'
    fBatch.AddString(Props.SQLTableName);
    fBatch.Add('"', ',');
  end;
  // same format as TRest.Update, BUT including the ID
  if IsZero(CustomFields) then
    Value.FillContext.ComputeSetUpdatedFieldBits(Props, FieldBits)
  else if DoNotAutoComputeFields then
    FieldBits := CustomFields * Props.CopiableFieldsBits
  else
    FieldBits := CustomFields * Props.CopiableFieldsBits + Props.FieldBits[oftModTime];
  SetExpandedJSONWriter(Props, fTablePreviousSendData <> POrmClass(Value)^,
    {withID=}true, FieldBits);
  fTablePreviousSendData := POrmClass(Value)^;
  if not DoNotAutoComputeFields then
    Value.ComputeFieldsBeforeWrite(fRest, oeUpdate); // update oftModTime fields
  Value.GetJSONValues(fBatch);
  fBatch.Add(',');
  if fCalledWithinRest and (FieldBits - Props.SimpleFieldsBits[ooUpdate] = []) then
    ForceCacheUpdate := true; // safe to update the cache with supplied values
  if ForceCacheUpdate then
    fRest.CacheOrNil.Notify(Value, ooUpdate)
  else
    // may not contain all cached fields -> delete from cache
    AddID(fDeletedRecordRef, fDeletedCount, RecordReference(tableIndex, ID));
  result := fBatchCount;
  inc(fBatchCount);
  inc(fUpdateCount);
  if Assigned(fOnWrite) then
    fOnWrite(self, ooUpdate, POrmClass(Value)^, Value.IDValue, Value, FieldBits);
end;

function TRestBatch.Update(Value: TOrm; const CustomCSVFields: RawUTF8;
  DoNotAutoComputeFields, ForceCacheUpdate: boolean): integer;
begin
  if (Value = nil) or
     (fBatch = nil) then
    result := -1
  else
    result := Update(Value, Value.RecordProps.FieldBitsFromCSV(CustomCSVFields),
      DoNotAutoComputeFields, ForceCacheUpdate);
end;


{ TRestBatchLocked }

constructor TRestBatchLocked.Create(const aRest: IRestOrm;
  aTable: TOrmClass; AutomaticTransactionPerRow: cardinal;
  Options: TRestBatchOptions; InternalBufferSize: cardinal;
  CalledWithinRest: boolean);
begin
  inherited Create(aRest, aTable, AutomaticTransactionPerRow,
    Options, InternalBufferSize, CalledWithinRest);
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

function TSynValidateRest.Process(aFieldIndex: integer; const Value: RawUTF8;
  var ErrorMsg: string): boolean;
begin
  result := DoValidate(aFieldIndex, Value, ErrorMsg, fProcessRest, fProcessRec);
end;

function TSynValidateRest.Validate(aFieldIndex: integer; const Value: RawUTF8;
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
  const Value: RawUTF8; var ErrorMsg: string; const aProcessRest: IRestOrm;
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
    with aProcessRec.RecordProps do
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

procedure TSynValidateUniqueFields.SetParameters(const Value: RawUTF8);
var
  V: array[0..0] of TValuePUTF8Char;
  tmp: TSynTempBuffer;
begin
  tmp.Init(Value);
  try
    JSONDecode(tmp.buf, ['FieldNames'], @V, True);
    CSVToRawUTF8DynArray(V[0].Value, fFieldNames);
  finally
    tmp.Done;
  end;
end;

function TSynValidateUniqueFields.DoValidate(aFieldIndex: integer;
  const Value: RawUTF8; var ErrorMsg: string; const aProcessRest: IRestOrm;
  aProcessRec: TOrm): boolean;
var
  where: RawUTF8;
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
    SetID(aProcessRest.OneFieldValue(aProcessRec.RecordClass, 'ID', where), aID{%H-});
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

procedure TOrmAccessRights.FromString(P: PUTF8Char);
begin
  FillCharFast(self, SizeOf(self), 0);
  if P = nil then
    exit;
  AllowRemoteExecute := TOrmAllowRemoteExecute(byte(GetNextItemCardinal(P)));
  SetBitCSV(GET, MAX_TABLES, P);
  SetBitCSV(POST, MAX_TABLES, P);
  SetBitCSV(PUT, MAX_TABLES, P);
  SetBitCSV(DELETE, MAX_TABLES, P);
end;

function TOrmAccessRights.ToString: RawUTF8;
begin
  FormatUTF8('%,%,%,%,%', [Byte(AllowRemoteExecute),
    GetBitCSV(GET, MAX_TABLES), GetBitCSV(POST, MAX_TABLES),
    GetBitCSV(PUT, MAX_TABLES), GetBitCSV(DELETE, MAX_TABLES)], result);
end;


{ ************** TOrm High-Level Parents }

{ TOrmNoCase }

class procedure TOrmNoCase.InternalDefineModel(Props: TOrmProperties);
begin
  Props.SetCustomCollationForAll(oftUTF8Text, 'NOCASE');
end;

{ TOrmCaseSensitive }

class procedure TOrmCaseSensitive.InternalDefineModel(Props: TOrmProperties);
begin
  Props.SetCustomCollationForAll(oftUTF8Text, 'BINARY');
end;

{ TOrmNoCaseExtended }

class procedure TOrmNoCaseExtended.InternalDefineModel(Props: TOrmProperties);
begin
  inherited InternalDefineModel(Props); // set NOCASE collation
  Props.SetVariantFieldsDocVariantOptions(JSON_OPTIONS_FAST_EXTENDED);
end;

{ TOrmCaseSensitiveExtended }

class procedure TOrmCaseSensitiveExtended.InternalDefineModel(
  Props: TOrmProperties);
begin
  inherited InternalDefineModel(Props); // set BINARY collation
  Props.SetVariantFieldsDocVariantOptions(JSON_OPTIONS_FAST_EXTENDED);
end;

{ TOrmNoCaseExtendedNoID }

class procedure TOrmNoCaseExtendedNoID.InitializeTable(
  const Server: IRestOrmServer; const FieldName: RawUTF8;
  Options: TOrmInitializeTableOptions);
begin
  include(Options, itoNoIndex4TID);
  inherited InitializeTable(Server, FieldName, Options);
end;


procedure InitializeUnit;
begin
  InitializeCriticalSection(vmtAutoTableLock);
  Rtti.RegisterObjArray(
    TypeInfo(TOrmModelPropertiesObjArray), TOrmModelProperties);
  // ensure TOrmObjArray is recognized as a T*ObjArray
  // - needed e.g. by TRestStorageInMemory.Create
  Rtti.RegisterObjArray(TypeInfo(TOrmObjArray), TOrm);
  // manual setting of OrmFieldTypeComp[] values which are not TUTF8Compare
  pointer(@OrmFieldTypeComp[oftAnsiText]) := @AnsiIComp;
  pointer(@OrmFieldTypeComp[oftUTF8Custom]) := @AnsiIComp;
  pointer(@OrmFieldTypeComp[oftObject]) := @StrComp;
  pointer(@OrmFieldTypeComp[oftVariant]) := @StrComp;
  pointer(@OrmFieldTypeComp[oftNullable]) := @StrComp;
end;

procedure FinalizeUnit;
begin
  DeleteCriticalSection(vmtAutoTableLock);
  FreeAndNil(OrmPropInfoRegistration);
end;

// yes, I know, this is a huge unit, but there are a lot of comments and
// the core ORM types are coupled together by design :)

initialization
  InitializeUnit;

finalization
  FinalizeUnit;
end.

