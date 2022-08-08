/// Database Framework MongoDB Direct Client
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.db.nosql.mongodb;

{
  *****************************************************************************

   MongoDB Client for NoSQL Data Access
    - MongoDB Wire Protocol Definitions
    - MongoDB Protocol Classes
    - MongoDB Client Classes

   TODO:
   - handle BULK commands support for MongoDB >=2.6 for faster writes
     see http://blog.mongodb.org/post/84922794768
   - GridFS support ?

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  variants,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.json,
  mormot.core.variants,
  mormot.core.datetime,
  mormot.core.data,
  mormot.core.perf,
  mormot.core.log,
  mormot.core.rtti,
  mormot.crypt.core,
  mormot.db.core,
  mormot.db.nosql.bson,
  mormot.net.sock;


{ ************ MongoDB Wire Protocol Definitions }

type
  /// store the binary raw data of a database response to a client message
  TMongoReply = RawByteString;

  /// store the genuine identifier of a Mongo cursor
  // - used to retrieve results in batches
  TMongoCursorID = Int64;

  /// internal low-level binary structure mapping all message headers
  TMongoWireHeader = packed record
    /// total message length, including the header
    MessageLength: integer;
    /// identifier of this message
    RequestID: integer;
    /// retrieve the RequestID from the original request
    ResponseTo: integer;
    /// low-level code of the message
    // - always equal to OP_MSG = 2013, unless MONGO_OLDPROTOCOL is set and
    // SendAndGetReply() will map it to a high-level TMongoOperation
    OpCode: integer;
  end;

  PMongoWireHeader = ^TMongoWireHeader;


  {$ifdef MONGO_OLDPROTOCOL}

  /// internal low-level binary structure mapping the TMongoReply header
  // - used e.g. by TMongoReplyCursor and TMongoConnection.SendAndGetReply()
  TMongoReplyHeader = packed record
    /// standard message header
    Header: TMongoWireHeader;
    /// response flags
    ResponseFlags: integer;
    /// cursor identifier if the client may need to perform further opGetMore
    CursorID: TMongoCursorID;
    /// where in the cursor this reply is starting
    StartingFrom: integer;
    /// number of documents in the reply
    NumberReturned: integer;
  end;

  /// points to an low-level binary structure mapping the TMongoReply header
  // - so that you can write e.g.
  // ! PMongoReplyHeader(aMongoReply)^.RequestID
  PMongoReplyHeader = ^TMongoReplyHeader;

  /// the available MongoDB driver Request Opcodes
  // - opReply: database reply to a client request - ResponseTo shall be set
  // - opMsgOld: generic msg command followed by a string (deprecated)
  // - opUpdate: update document
  // - opInsert: insert new document
  // - opQuery: query a collection
  // - opGetMore: get more data from a previous query
  // - opDelete: delete documents
  // - opKillCursors: notify database client is done with a cursor
  // - opMsg: new OP_MSG layout introduced in MongoDB 3.6 - replaces all other
  // opcodes, which are deprecated since 5.0, and removed since 5.1/6.0
  TMongoOperation = (
    opReply,
    opMsgOld,
    opUpdate,
    opInsert,
    opQuery,
    opGetMore,
    opDelete,
    opKillCursors,
    opMsg);

  /// define how an opQuery operation will behave
  // - if mqfTailableCursor is set, cursor is not closed when the last data
  // is retrieved
  // - if mqfSlaveOk is set, it will allow query of replica slave; normally
  // this returns an error except for namespace "local"
  // - mqfOplogReplay is internal replication use only - driver should not set
  // - if mqfNoCursorTimeout is set, the server normally does not times out
  // idle cursors after an inactivity period (10 minutes) to prevent
  // excess memory use
  // - if mqfAwaitData is to use with TailableCursor. If we are at the end
  // of the data, block for a while rather than returning no data. After a
  // timeout period, we do return as normal
  // - if mqfExhaust is set, stream the data down full blast in multiple "more"
  // packages, on the assumption that the client will fully read all data queried
  // - if mqfPartial is set, it will get partial results from a mongos if
  // some shards are down (instead of throwing an error)
  TMongoQueryFlag = (
    mqfTailableCursor = 1,
    mqfSlaveOk,
    mqfOplogReplay,
    mqfNoCursorTimeout,
    mqfAwaitData,
    mqfExhaust,
    mqfPartial);

  /// define how a TMongoRequestQuery message will behave
  TMongoQueryFlags = set of TMongoQueryFlag;

  /// define an opReply message execution content
  // - mrfCursorNotFound will be set when getMore is called but the cursor id
  // is not valid at the server; returned with zero results
  // - mrfQueryFailure is set when the query failed - results consist of one
  // document containing an "$err" field describing the failure
  // - mrfShardConfigStale should not be used by client, just by Mongos
  // - mrfAwaitCapable is set when the server supports the AwaitData Query
  // option (always set since Mongod version 1.6)
  TMongoReplyCursorFlag = (
    mrfCursorNotFound,
    mrfQueryFailure,
    mrfShardConfigStale,
    mrfAwaitCapable);

  /// define a TMongoReplyCursor message execution content
  TMongoReplyCursorFlags = set of TMongoReplyCursorFlag;

/// ready-to-be displayed text of a TMongoOperation item
function ToText(op: TMongoOperation): PShortString; overload;

  {$else}

  /// flags that modify the format and behavior of opMsg execution content
  // - mmfChecksumPresent indicates that a crc32c checksum is supplied
  // - mmfMoreToCome is set when another message will follow this one without
  // further action from the receiver. The receiver MUST NOT send another
  // message until receiving one with mmfMoreToCome is not set as sends may
  // block, causing deadlock. Requests with the mmfMoreToCome flag set will not
  // receive a reply. Replies will only have this set in response to requests
  // with the mmfExhaustAllowed bit set.
  // - mmfExhaustAllowed indicates the client supports the mmfMoreToCome flag
  // (currently never set, because we don't support it yet)
  // - by definition, is used for TMongoQueryFlags and TMongoReplyCursorFlags
  TMongoMsgFlag = (
    mmfChecksumPresent,
    mmfMoreToCome,
    mmfExhaustAllowed = 16);
  TMongoMsgFlags = set of TMongoMsgFlag;

  /// the kind of sections for a opMsg content
  // - mmkBody indicates that the section is encoded as a single BSON object:
  // this is the standard command request and reply body
  // - mmkSequence is used when there are several sections, encoded as the
  // 32-bit size, then the ASCIIZ document identifier, then zero or more
  // BSON objects, ending one the declared size has been reached
  // - mmkInternal is used for internal purposes
  TMongoMsgKind = (
    mmkBody,
    mmkSequence,
    mmkInternal);

  /// internal low-level binary structure mapping the Msg header
  TMongoMsgHeader = packed record
    /// standard message header
    Header: TMongoWireHeader;
    /// 32-bit query/response flags
    Flags: TMongoMsgFlags;
    /// how the following sections are defined
    SectionKind: TMongoMsgKind;
  end;
  PMongoMsgHeader = ^TMongoMsgHeader;

  TMongoQueryFlags = TMongoMsgFlags;
  TMongoReplyCursorFlags = TMongoMsgFlags;

  {$endif MONGO_OLDPROTOCOL}



{ ************ MongoDB Protocol Classes }

const
  /// MongoDB server default IP port
  MONGODB_DEFAULTPORT = 27017;

type
  /// exception type used for MongoDB process
  EMongoException = class(ESynException);

  /// define how an opUpdate operation will behave
  // - if mufUpsert is set, the database will insert the supplied object into
  // the collection if no matching document is found
  // - if mufMultiUpdate is set, the database will update all matching objects
  // in the collection; otherwise (by default) only updates first matching doc
  // - if mufContinueOnError is set, the database will not stop processing
  // bulk updates if one fails (e.g. due to duplicate IDs)
  // - if mufBypassDocumentValidation is set, update would be allowed even
  // if the content does not meet the validation requirements
  TMongoUpdateFlag = (
    mufUpsert,
    mufMultiUpdate,
    mufContinueOnError,
    mufBypassDocumentValidation);

  /// define how a TMongoRequestUpdate message will behave
  TMongoUpdateFlags = set of TMongoUpdateFlag;

  /// define how an opInsert operation will behave
  // - if mifContinueOnError is set, the database will not stop processing
  // bulk inserts if one fails (e.g. due to duplicate IDs); this makes bulk
  // insert behave similarly to a series of single inserts, except lastError
  // will be set if any insert fails, not just the last one
  // - if mifBypassDocumentValidation is set, insertion would be allowed even
  // if the content does not meet the validation requirements
  TMongoInsertFlag = (
    mifContinueOnError,
    mifBypassDocumentValidation);

  /// define how a TMongoRequestInsert message will behave
  TMongoInsertFlags = set of TMongoInsertFlag;

  /// define how an opDelete operation will behave
  // - if mdfSingleRemove is set, the database will remove only the first
  // matching document in the collection. Otherwise (by default) all matching
  // documents will be removed
  // - if mdfContinueOnError is set, the database will not stop processing
  // bulk deletes if one fails
  TMongoDeleteFlag = (
    mdfSingleRemove,
    mdfContinueOnError);

  /// define how a TMongoRequestDelete message will behave
  TMongoDeleteFlags = set of TMongoDeleteFlag;

  /// abstract class used to create MongoDB Wire Protocol client messages
  // - see http://docs.mongodb.org/meta-driver/latest/legacy/mongodb-wire-protocol
  // - this class is not tight to the connection class itself (which is one
  // known limitation of TMongoWire for instance)
  TMongoRequest = class(TBsonWriter)
  protected
    fRequestID: integer;
    fResponseTo: integer;
    fNumberToReturn: integer;
    fDatabaseName, fCollectionName: RawUtf8;
    fBsonDocument: TBsonDocument;
    {$ifdef MONGO_OLDPROTOCOL}
    fFullCollectionName: RawUtf8;
    fRequestOpCode: TMongoOperation;
    /// append a query parameter as a BSON document
    // - param can be a TDocVariant, e.g. created with:
    // ! _JsonFast('{name:"John",age:{$gt:21}}');
    // ! _JsonFastFmt('{name:?,age:{$gt:?}}',[],['John',21]);
    // ! _JsonFastFmt('{name:?,field:/%/i}',['acme.*corp'],['John']);
    // - param can be a TBsonVariant containing a TBsonDocument raw binary block
    // created e.g. from:
    // ! BsonVariant(['BSON',_Arr(['awesome',5.05, 1986])])
    // ! BsonVariantType[Bson(['BSON',_Arr(['awesome',5.05, 1986])])]
    // - if param is null, it will append a void document
    // - if param is a string, it will be converted as expected by most
    // database commands, e.g.
    // ! TMongoRequestQuery.Create('admin.$cmd','buildinfo',[],1)
    // will query   { buildinfo: 1 }  to the  admin.$cmd  collection, i.e.
    // $ admin.$cmd.findOne( { buildinfo: 1 } )
    procedure BsonWriteParam(const paramDoc: variant);
    {$endif MONGO_OLDPROTOCOL}
  public
    /// write a standard Message Header for MongoDB client
    // - opCode is the type of the message
    // - requestID  is a client or database-generated identifier that uniquely
    // identifies this message: in case of opQuery or opGetMore messages, it will
    // be sent in the responseTo field from the database
    // - responseTo is the requestID taken from previous opQuery or opGetMore
    {$ifdef MONGO_OLDPROTOCOL}
    constructor Create(const FullCollectionName: RawUtf8;
      opCode: TMongoOperation; requestID, responseTo: integer); reintroduce;
    {$else}
    constructor Create(const Database, Collection: RawUtf8); reintroduce;
    {$endif MONGO_OLDPROTOCOL}
    /// flush the content and return the whole binary encoded stream
    // - expect the TBsonWriter instance to have been created with reintroduced
    // Create() specific constructors inheriting from this TMongoRequest class
    // - this overridden version will adjust the size in the message header
    procedure ToBsonDocument(var result: TBsonDocument); override;
    /// write the main parameters of the request as JSON
    procedure ToJson(W: TJsonWriter; Mode: TMongoJsonMode); overload; virtual;
    /// write the main parameters of the request as JSON
    function ToJson(Mode: TMongoJsonMode): RawUtf8; overload;
    /// identify the message, after call to any reintroduced Create() constructor
    property MongoRequestID: integer
      read fRequestID;
    /// retrieve the NumberToReturn parameter as set to the constructor
    property NumberToReturn: integer
      read fNumberToReturn;
    /// the associated collection name, e.g. 'test'
    // - for OP_MSG, only set for "find", "aggregate" or "getMore" commands
    property CollectionName: RawUtf8
      read fCollectionName;
    {$ifdef MONGO_OLDPROTOCOL}
    /// the associated full collection name, e.g. 'db.test'
    property FullCollectionName: RawUtf8
      read fFullCollectionName;
    {$endif MONGO_OLDPROTOCOL}
    /// the associated full collection name, e.g. 'db'
    property DatabaseName: RawUtf8
      read fDatabaseName;
  end;

  {$ifdef MONGO_OLDPROTOCOL}

  /// a MongoDB client abstract ancestor which is able to create a BULK
  // command message for MongoDB >= 2.6 instead of older dedicated Wire messages
  TMongoRequestWritable = class(TMongoRequest)
  protected
  public
  end;

  /// a MongoDB client message to update a document in a collection
  TMongoRequestUpdate = class(TMongoRequestWritable)
  protected
    fSelector, fUpdate: TVarData;
  public
    /// initialize a MongoDB client message to update a document in a collection
    // - FullCollectionName is e.g. 'dbname.collectionname'
    // - how the update will be processed can be customized via Flags
    // - Selector is the BSON document query to select the document, supplied as
    // TDocVariant - i.e. created via _JsonFast() or _JsonFastFmt() - or as
    // TBsonVariant - i.e. created via BsonVariant() - or null if all documents
    // are to be updated
    // - Update is the BSON document specification of the update to perform,
    // supplied as TDocVariant or TBsonVariant
    // - there is no response to an opUpdate message
    constructor Create(const FullCollectionName: RawUtf8;
      const Selector, Update: variant;
      Flags: TMongoUpdateFlags = []); reintroduce;
    /// write the main parameters of the request as JSON
    procedure ToJson(W: TJsonWriter; Mode: TMongoJsonMode); override;
  end;

  /// a MongoDB client message to insert one or more documents in a collection
  TMongoRequestInsert = class(TMongoRequestWritable)
  public
    /// initialize a MongoDB client message to insert one or more documents in
    // a collection, supplied as variants
    // - FullCollectionName is e.g. 'dbname.collectionname'
    // - Documents is an array of TDocVariant or TBsonVariant - i.e. created via
    // _JsonFast() _JsonFastFmt() or BsonVariant()
    // - there is no response to an opInsert message
    constructor Create(const FullCollectionName: RawUtf8;
      const Documents: array of variant;
      Flags: TMongoInsertFlags = []); reintroduce; overload;
    /// initialize a MongoDB client message to insert one or more documents in
    // a collection, supplied as raw BSON binary
    // - FullCollectionName is e.g. 'dbname.collectionname'
    // - Documents is the low-level concatenation of BSON documents, created
    // e.g. with a TBsonWriter stream
    // - there is no response to an opInsert message
    constructor Create(const FullCollectionName: RawUtf8;
      const Documents: TBsonDocument;
      Flags: TMongoInsertFlags = []); reintroduce; overload;
    /// initialize a MongoDB client message to insert one or more documents in
    // a collection, supplied as JSON objects
    // - FullCollectionName is e.g. 'dbname.collectionname'
    // - JSONDocuments is an array of JSON objects
    // - there is no response to an opInsert message
    // - warning: JSONDocuments[] buffer will be modified in-place during
    // parsing, so a private copy may have to be made by the caller
    constructor Create(const FullCollectionName: RawUtf8;
      const JSONDocuments: array of PUtf8Char;
      Flags: TMongoInsertFlags = []); reintroduce; overload;
  end;

  /// a MongoDB client message to delete one or more documents in a collection
  TMongoRequestDelete = class(TMongoRequestWritable)
  protected
    fQuery: TVarData;
  public
    /// initialize a MongoDB client message to delete one or more documents in
    // a collection
    // - FullCollectionName is e.g. 'dbname.collectionname'
    // - Selector is the BSON document query to select the document, supplied as
    // TDocVariant - i.e. created via _JsonFast() or _JsonFastFmt() - or as
    // TBsonVariant - i.e. created via BsonVariant() - or null if all documents
    // are to be deleted
    // - warning: CreateDelete('db.coll',null) can be expensive so you should
    // better drop the whole collection
    // - there is no response to an opDelete message
    constructor Create(const FullCollectionName: RawUtf8;
      const Selector: variant;
      Flags: TMongoDeleteFlags = []); reintroduce;
    /// write the main parameters of the request as JSON
    procedure ToJson(W: TJsonWriter; Mode: TMongoJsonMode); override;
  end;

  /// a MongoDB client message to continue the query of one or more documents
  // in a collection, after a TMongoRequestQuery message
  TMongoRequestGetMore = class(TMongoRequest)
  public
    /// initialize a MongoDB client message to continue the query of one or more
    // documents in a collection, after a opQuery / TMongoRequestQuery message
    // - FullCollectionName is e.g. 'dbname.collectionname'
    // - you can specify the number of documents to return (e.g. from previous
    // opQuery response)
    // - CursorID should have been retrieved within an opReply message from the
    // database
    constructor Create(const FullCollectionName: RawUtf8;
      NumberToReturn: integer; CursorID: TMongoCursorID); reintroduce;
  end;

  /// a MongoDB client message to close one or more active cursors
  TMongoRequestKillCursor = class(TMongoRequest)
  protected
    fCursors: TInt64DynArray;
  public
    /// initialize a MongoDB client message to close one or more active cursors
    // in the database
    // - it is mandatory to ensure that database resources are reclaimed by
    // the client at the end of the query
    // - if a cursor is read until exhausted (read until opQuery or opGetMore
    // returns zero for the CursorId), there is no need to kill the cursor
    // - there is no response to an opKillCursor message
    constructor Create(const FullCollectionName: RawUtf8;
      const CursorIDs: array of TMongoCursorID); reintroduce;
    /// write the main parameters of the request as JSON
    procedure ToJson(W: TJsonWriter; Mode: TMongoJsonMode); override;
  end;

  /// a MongoDB client message to query one or more documents in a collection
  // - implemented using opMsg - or opQuery with MONGO_OLDPROTOCOL conditional
  TMongoRequestQuery = class(TMongoRequest)
  protected
    fNumberToSkip: integer;
    fQuery, fReturnFieldsSelector: TVarData;
  public
    /// initialize a MongoDB client message to query one or more documents in
    // a collection from a specified Cursor identifier
    // - FullCollectionName is e.g. 'dbname.collectionname'
    // - Query is the BSON document query to select the document, supplied as
    // TDocVariant - i.e. created via _JsonFast() or _JsonFastFmt() - or null
    // if all documents are to be retrieved - for instance:
    // ! _JsonFast('{name:"John",age:{$gt:21}}');
    // ! _JsonFastFmt('{name:?,age:{$gt:?}}',[],['John',21]);
    // ! _JsonFastFmt('{name:?,field:/%/i}',['acme.*corp'],['John']);
    // - if Query is a string, it will be converted as expected by most
    // database commands, e.g.
    // $ TMongoRequestQuery.Create('admin.$cmd','buildinfo',[],1)
    // will query   { buildinfo: 1 }  to the  admin.$cmd  collection, i.e.
    // $ admin.$cmd.findOne( { buildinfo: 1 } )
    // - Query can also be a TBsonVariant, e.g. created with:
    // ! BsonVariant('{name:?,age:{$gt:?}}',[],['John',21])
    // - ReturnFieldsSelector is an optional selector (set to null if not
    // applicable) as a BSON document that limits the fields in the returned
    // documents, supplied as TDocVariant or TBsonVariant - e.g. created via:
    // ! BsonVariantFieldSelector('a,b,c');
    // ! BsonVariantFieldSelector(['a','b','c']);
    // ! BsonVariant('{a:1,b:1,c:1}');
    // ! _JsonFast('{a:1,b:1,c:1}');
    // - if ReturnFieldsSelector is a string, it will be converted into
    // $ { ReturnFieldsSelector: 1 }
    constructor Create(const FullCollectionName: RawUtf8;
      const Query, ReturnFieldsSelector: variant; NumberToReturn: integer;
      NumberToSkip: integer = 0; Flags: TMongoQueryFlags = []); reintroduce;
    /// write the main parameters of the request as JSON
    procedure ToJson(W: TJsonWriter; Mode: TMongoJsonMode); override;
    /// retrieve the NumberToSkip parameter as set to the constructor
    property NumberToSkip: integer
      read fNumberToSkip;
  end;

  {$else}

  /// a MongoDB client message to access a collection
  // - implements the OP_MSG opcode for all its query or write process to the DB
  TMongoMsg = class(TMongoRequest)
  protected
    fCommand: variant;
  public
    /// initialize a MongoDB client message to access a database instance
    // - Collection is set for "find" and "aggregate" commands, to unest any
    // firstBatch/nextBatch arrays, and specify the collection name for "getMore"
    constructor Create(const Database, Collection: RawUtf8;
      const Command: variant; Flags: TMongoMsgFlags; ToReturn: integer); reintroduce;
    /// write the main parameters of the request as JSON
    procedure ToJson(W: TJsonWriter; Mode: TMongoJsonMode); override;
end;

  {$endif MONGO_OLDPROTOCOL}

  /// map a MongoDB server reply message as sent back from the database
  // - in response to TMongoMsg or TMongoRequestQuery / TMongoRequestGetMore
  // for MONGO_OLDPROTOCOL
  // - note: old opReply is removed since MongoDB 5.1 in favor of opMsg
  // - you can use the record's methods to retrieve information about a given
  // response, and navigate within all nested documents
  // - several TMongoReplyCursor instances may map the same TMongoReply content
  // - you can safely copy one TMongoReplyCursor instance to another
  {$ifdef USERECORDWITHMETHODS}
  TMongoReplyCursor = record
  {$else}
  TMongoReplyCursor = object
  {$endif USERECORDWITHMETHODS}
  private
    fRequest: TMongoRequest;
    fReply: TMongoReply;
    fRequestID: integer;
    fResponseTo: integer;
    fResponseFlags: TMongoReplyCursorFlags;
    fDocumentCount: integer;
    fCursorID: TMongoCursorID;
    fDocumentsOffset: TIntegerDynArray;
    fFirstDocument, fCurrent: PByte;
    fPosition: integer;
    fLatestDocIndex: integer;
    fLatestDocValue: variant;
    {$ifdef MONGO_OLDPROTOCOL}
    fStartingFrom: integer;
    procedure ComputeDocumentsList;
    {$endif MONGO_OLDPROTOCOL}
    function GetOneDocument(index: integer): variant;
  public
    /// initialize the cursor with a supplied binary reply from the server
    // - will raise an EMongoException if the content is not valid
    // - will populate all record fields with the supplied data
    procedure Init(Request: TMongoRequest; const ReplyMessage: TMongoReply);
    {$ifndef MONGO_OLDPROTOCOL}
    /// extract the firstBatch/nextBatch: nested array after Init()
    procedure ExtractBatch;
    {$endif MONGO_OLDPROTOCOL}

    /// iterate over the next document in the list, as a TDocVariant instance
    // - return TRUE if the supplied document has been retrieved
    // - return FALSE if there is no more document to get - you can use the
    // Rewind method to restart from the first document
    // - could be used e.g. as:
    // ! var Reply: TMongoReply;
    // !     doc: variant;
    // ! ...
    // !   Reply.Init(ResponseMessage);
    // !   while Reply.Next(doc) do
    // !      writeln('Name: ',doc.Name,' FirstName: ',doc.FirstName);
    function Next(out doc: variant;
      option: TBsonDocArrayConversion = asDocVariantPerReference): boolean; overload;
    /// iterate over the next document in the list, as BSON content
    // - return TRUE if the supplied document has been retrieved - then doc
    // points to a "int32 e_list #0" BSON document
    // - return FALSE if there is no more document to get - you can use the
    // Rewind method to restart from the first document
    // - this method is almost immediate, since the BSON raw binary is returned
    // directly without any conversion
    // - could be used e.g. as:
    // ! var Reply: TMongoReply;
    // !     doc: PByte;
    // ! ...
    // !   Reply.Init(ResponseMessage);
    // !   while Reply.Next(doc) do
    // !     writeln(BsonToJson(doc,0,modMongoShell)); // fast display
    function Next(out doc: PByte): boolean; overload;
    /// iterate over the next document in the list, as a BSON binary document
    // - return TRUE if the supplied document has been retrieved - then doc
    // points to a "int32 e_list #0" BSON document
    // - return FALSE if there is no more document to get - you can use the
    // Rewind method to restart from the first document
    // - this method is slightly slower than the one returning a PByte, since
    // it will allocate a memory buffer to store the TBsonDocument binary
    // - could be used e.g. as:
    // ! var Reply: TMongoReply;
    // !     doc: TBsonDocument;
    // ! ...
    // !   Reply.Init(ResponseMessage);
    // !   while Reply.Next(doc) do
    // !     writeln(BsonToJson(doc,0,modMongoShell)); // fast display
    function Next(out Bson: TBsonDocument): boolean; overload;
    /// iterate over the next document in the list, as JSON content
    // - return TRUE if the supplied document has been retrieved
    // - return FALSE if there is no more document to get - you can use the
    // Rewind method to restart from the first document
    // - could be used e.g. as:
    // ! var Reply: TMongoReply;
    // !     json: RawUtf8;
    // ! ...
    // !   Reply.Init(ResponseMessage);
    // !   while Reply.Next(json,modMongoShell) do
    // !     writeln(json); // fast display
    function Next(out Json: RawUtf8;
      Mode: TMongoJsonMode = modMongoStrict): boolean; overload;
    /// let Next() overloaded methods point to the first document of this message
    procedure Rewind;

    /// retrieve a given document as a TDocVariant instance
    // - this method won't use any cache (like Document[..] property), since
    // it should be used with a local variant on stack as cache:
    // ! var Reply: TMongoReply;
    // !     doc: variant;
    // !     i: integer;
    // ! ...
    // !   Reply.Init(ResponseMessage);
    // !   for i := 0 to Reply.DocumentCount-1 do
    // !   begin
    // !      GetDocument(i,doc);
    // !      writeln('Name: ',doc.Name,' FirstName: ',doc.FirstName);
    // !   end;
    procedure GetDocument(index: integer; var result: variant);
    /// return all documents content as a JSON array, or one JSON object
    // if there is only one document in this reply
    // - this method is very optimized and will convert the BSON binary content
    // directly into JSON
    procedure FetchAllToJson(W: TJsonWriter;
      Mode: TMongoJsonMode = modMongoStrict; WithHeader: boolean = false;
      MaxSize: PtrUInt = 0);
    /// return all documents content as a JSON array, or one JSON object
    // if there is only one document in this reply
    // - this method is very optimized and will convert the BSON binary content
    // directly into JSON
    function ToJson(Mode: TMongoJsonMode = modMongoStrict;
      WithHeader: boolean = false; MaxSize: PtrUInt = 0): RawUtf8;
    /// append all documents content to a dynamic array of TDocVariant
    // - return the new size of the Dest[] array
    function AppendAllToDocVariantDynArray(var Dest: TVariantDynArray): integer;
    /// append all documents content to a TDocVariant array instance
    // - if the supplied instance if not already a TDocVariant of kind dvArray,
    // a new void instance will be created
    // - return the new size of the Dest array
    function AppendAllToDocVariant(var Dest: TDocVariantData): integer;
    /// append all documents content to a BSON binary stream
    // - Dest.Tag will be used to count the current item number in the resulting
    // BSON array
    procedure AppendAllToBson(Dest: TBsonWriter);
    /// return a result as TDocVariant object or array
    // - if there are no result, returns null
    // - if there are one or several results, returns a TDocVariant array
    procedure AppendAllAsDocVariant(var Dest: variant);

    /// retrieve the context execution of this message
    property ResponseFlags: TMongoReplyCursorFlags
      read fResponseFlags;
    /// identifier of this message
    property RequestID: integer
      read fRequestID;
    /// retrieve the RequestID from the original request
    property ResponseTo: integer
      read fResponseTo;
    /// access to the low-level binary reply message
    property Reply: TMongoReply
      read fReply;
    /// cursor identifier if the client may need to perform further
    // TMongoRequestGetMore messages
    // - in the event that the result set of the query fits into one OP_REPLY
    // message, CursorID will be 0
    // - deprecated since MongoDB 5.0, and removed in MongoDB 6.0
    property CursorID: TMongoCursorID
      read fCursorID;
    {$ifdef MONGO_OLDPROTOCOL}
    /// where in the cursor this reply is starting
    // - deprecated since MongoDB 5.0, and removed in MongoDB 6.0
    property StartingFrom: integer
      read fStartingFrom;
    {$endif MONGO_OLDPROTOCOL}
    /// number of documents in the reply
    property DocumentCount: integer
      read fDocumentCount;
    /// points to the first document binary
    // - i.e. just after the Reply header
    property FirstDocument: PByte
      read fFirstDocument;
    /// direct access to the low-level BSON binary content of each document
    property DocumentOffset: TIntegerDynArray
      read fDocumentsOffset;
    /// retrieve a given document as a TDocVariant instance
    // - could be used e.g. as:
    // ! var Reply: TMongoReply;
    // !     i: integer;
    // ! ...
    // !   Reply.Init(ResponseMessage);
    // !   for i := 0 to Reply.DocumentCount-1 do
    // !      writeln('Name: ',Reply.Document[i].Name,' FirstName: ',Reply.Document[i].FirstName);
    // - note that there is an internal cache for the latest retrieved document
    // by this property, so that you can call Reply.Document[i] several times
    // without any noticeable speed penalty
    property Document[index: integer]: variant
      read GetOneDocument;
    /// position of the Next() call, starting at 0, up to DocumentCount
    property Position: integer
      read fPosition;
  end;




{ ************ MongoDB Client Classes }

type
    /// event callback signature for iterative process of TMongoConnection
  TOnMongoConnectionReply = procedure(Request: TMongoRequest;
    const Reply: TMongoReplyCursor; var Opaque) of object;

{$M+}

  TMongoClient = class;
  TMongoDatabase = class;
  TMongoCollection = class;

  /// one TCP/IP connection to a MongoDB server
  // - all access will be protected by a mutex (critical section): it is thread
  // safe but you may use one TMongoClient per thread or a connection pool, for
  // better performance
  TMongoConnection = class
  protected
    fLock: TRTLCriticalSection;
    fLocked: cardinal;
    fClient: TMongoClient;
    fSocket: TCrtSocket;
    fServerAddress: RawUtf8;
    fServerPort: integer;
    procedure Lock;
    procedure UnLock;
    function Send(Request: TMongoRequest): boolean;
    function GetOpened: boolean;
    function GetLocked: boolean;
    // will call TMongoReplyCursor.FetchAllToJson(TJsonWriter(Opaque))
    procedure ReplyJsonStrict(Request: TMongoRequest;
      const Reply: TMongoReplyCursor; var Opaque);
    procedure ReplyJsonExtended(Request: TMongoRequest;
      const Reply: TMongoReplyCursor; var Opaque);
    procedure ReplyJsonNoMongo(Request: TMongoRequest;
      const Reply: TMongoReplyCursor; var Opaque);
    // will call TMongoReplyCursor.AppendAllToDocVariantDynArray(TVariantDynArray(Opaque))
    procedure ReplyDocVariants(Request: TMongoRequest;
      const Reply: TMongoReplyCursor; var Opaque);
    // will call TMongoReplyCursor.AppendSingleAsDocVariant(TDocVariantData(Opaque))
    procedure ReplyDocVariant(Request: TMongoRequest;
      const Reply: TMongoReplyCursor; var Opaque);
    // will call TMongoReplyCursor.AppendAllToBson(TBsonWrite(Opaque))
    procedure ReplyBson(Request: TMongoRequest;
      const Reply: TMongoReplyCursor; var Opaque);
  public
    /// initialize the connection to the corresponding MongoDB server
    // - the server address is either a host name, or an IP address
    // - if no server address is specified, will try to connect to localhost
    // - this won't create the connection, until Open method is executed
    constructor Create(const aClient: TMongoClient; const aServerAddress: RawUtf8;
      aServerPort: integer = MONGODB_DEFAULTPORT); reintroduce;
    /// release the connection, including the socket
    destructor Destroy; override;
    /// connect to the MongoDB server
    // - will raise an EMongoException on error
    procedure Open;
    /// disconnect from MongoDB server
    // - will raise an EMongoException on error
    procedure Close;

    /// low-level method to send a request to the server
    // - if Request is not either TMongoRequestQuery or TMongoRequestGetMore,
    // will raise an EMongoException
    // - then will return the reply message as sent back by the database,
    // ready to be accessed using a TMongoReplyCursor wrapper
    procedure SendAndGetReply(Request: TMongoRequest; out result: TMongoReply);
    /// low-level method to send a request to the server, and return a cursor
    // - if Request is not either TMongoRequestQuery or TMongoRequestGetMore,
    // will raise an EMongoException
    // - then will parse and return a cursor to the reply message as sent back
    // by the database, with logging if necessary
    // - raise an EMongoException if mrfQueryFailure flag is set in the reply
    procedure SendAndGetCursor(Request: TMongoRequest; var Result: TMongoReplyCursor);
    /// low-level method to send a query to the server, calling a callback event
    // on each reply
    // - is used by GetDocumentsAndFree, GetBsonAndFree and GetJsonAndFree
    // methods to receive the whole document (you should better call those)
    // - the supplied Query instance will be released when not needed any more
    procedure SendAndGetRepliesAndFree(Request: TMongoRequest;
      const OnEachReply: TOnMongoConnectionReply; var Opaque);

    /// send a query to the server, returning a TDocVariant instance containing
    // all the incoming data
    // - will send the Request message, and any needed TMongoRequestGetMore
    // messages to retrieve all the data from the server
    // - the supplied Query instance will be released when not needed any more
    // - if Query.NumberToReturn<>1, it will return either null or a dvArray
    // kind of TDocVariant containing all returned items
    // - if Query.NumberToReturn=1, then it will return either null or a
    // single TDocVariant instance
    function GetDocumentsAndFree(Query: TMongoRequest): variant; overload;
    /// send a query to the server, returning a TDocVariant instance containing
    // all the incoming data
    // - will send the Request message, and any needed TMongoRequestGetMore
    // messages to retrieve all the data from the server
    // - the supplied Query instance will be released when not needed any more
    // - if Query.NumberToReturn<>1, it will return either null or a dvArray
    // kind of TDocVariant containing all returned items
    // - if Query.NumberToReturn=1, then it will return either null or a
    // single TDocVariant instance
    procedure GetDocumentsAndFree(Query: TMongoRequest;
      var result: variant); overload;
    /// send a query to the server, returning a dynamic array of TDocVariant
    // instance containing all the incoming data
    // - will send the Request message, and any needed TMongoRequestGetMore
    // messages to retrieve all the data from the server
    // - the supplied Query instance will be released when not needed any more
    procedure GetDocumentsAndFree(Query: TMongoRequest;
      var result: TVariantDynArray); overload;
    /// send a query to the server, returning a TBsonDocument instance containing
    // all the incoming data, as raw binary BSON document containing an array
    // of the returned items
    // - will send the Request message, and any needed TMongoRequestGetMore
    // messages to retrieve all the data from the server
    // - the supplied Query instance will be released when not needed any more
    function GetBsonAndFree(Query: TMongoRequest): TBsonDocument;
    /// send a query to the server, returning all the incoming data as JSON
    // - will send the Request message, and any needed TMongoRequestGetMore
    // messages to retrieve all the data from the server
    // - this method is very optimized and will convert the BSON binary content
    // directly into JSON, in either modMongoStrict or modMongoShell layout
    // (modNoMongo will do the same as modMongoStrict)
    // - if Query.NumberToReturn<>1, it will return either 'null' or a '[..]'
    // JSON array with all the incoming documents retrieved from the server
    // - if Query.NumberToReturn=1, it will return either 'null' or a single
    // '{...}' JSON object
    // - the supplied Query instance will be released when not needed any more
    function GetJsonAndFree(Query: TMongoRequest; Mode: TMongoJsonMode): RawUtf8;

    {$ifdef MONGO_OLDPROTOCOL}
    /// send a message to the MongoDB server
    // - will apply Client.WriteConcern policy, and run an EMongoException
    // in case of any error
    // - the supplied Request instance will be released when not needed any more
    // - by default, it will follow Client.WriteConcern pattern - but you can
    // set NoAcknowledge = TRUE to avoid calling the getLastError command
    // - will return the getLastError reply (if retrieved from server)
    function SendAndFree(Request: TMongoRequest;
      NoAcknowledge: boolean = false): variant;
    {$endif MONGO_OLDPROTOCOL}
    /// run a database command, supplied as a TDocVariant, TBsonVariant or a
    // string, and return the a TDocVariant instance
    // - see http://docs.mongodb.org/manual/reference/command for a list
    // of all available commands
    // - for instance:
    // ! RunCommand('test',_ObjFast(['dbStats',1,'scale',1024],stats);
    // ! RunCommand('test',BsonVariant(['dbStats',1,'scale',1024],stats);
    // ! RunCommand('admin','buildinfo',fServerBuildInfo);
    // - the message will be returned by the server as a single TDocVariant
    // instance (since the associated TMongoRequestQuery.NumberToSkip=1)
    // - in case of any error, the error message is returned as text
    // - in case of success, this method will return ''
    // - if you expects the result to return a cursor, e.g. for a "find" or
    // "aggregate" command, you can specify aCollectionName so that the "cursor"
    // response will be parsed and its nested batch array, and call "getMore"
    // if needed
    function RunCommand(const aDatabaseName: RawUtf8;
      const command: variant; var returnedValue: variant;
      flags: TMongoQueryFlags = []; const aCollectionName: RawUtf8 = ''): RawUtf8; overload;
    /// run a database command, supplied as a TDocVariant, TBsonVariant or a
    // string, and return the raw BSON document array of received items
    // - this overloaded method can be used on huge content to avoid the slower
    // conversion to an array of TDocVariant instances
    // - in case of success, this method will return TRUE, or FALSE on error
    // - if you expects the result to return a cursor, e.g. for a "find" or
    // "aggregate" command, you can specify aCollectionName so that the "cursor"
    // response will be parsed and its nested batch array, and call "getMore"
    // if needed
    function RunCommand(const aDatabaseName: RawUtf8;
      const command: variant; var returnedValue: TBsonDocument;
      flags: TMongoQueryFlags = []; const aCollectionName: RawUtf8 = ''): boolean; overload;

    /// return TRUE if the Open method has successfully been called
    property Opened: boolean
      read GetOpened;
    /// access to the corresponding MongoDB server
    property Client: TMongoClient
      read fClient;
    /// direct access to the low-level TCP/IP communication socket
    property Socket: TCrtSocket
      read fSocket;
    /// is TRUE when the connection is busy
    property Locked: boolean
      read GetLocked;
  published
    /// read-only access to the supplied server address
    // - the server address is either a host name, or an IP address
    property ServerAddress: RawUtf8
      read fServerAddress;
    /// read-only access to the supplied server port
    // - the server Port is MONGODB_DEFAULTPORT (27017) by default
    property ServerPort: integer
      read fServerPort;
  end;

  /// array of TCP connection to a MongoDB Replica Set
  // - first item [0] is the Primary member
  // - other items [1..] are the Secondary members
  TMongoConnectionDynArray = array of TMongoConnection;

  /// the available options for a TMongoClient client
  TMongoClientOption = (
    mcoTls);

  /// set of available options for a TMongoClient client
  TMongoClientOptions = set of TMongoClientOption;

  /// define Read Preference Modes to a MongoDB replica set
  // - Important: All read preference modes except rpPrimary may return stale
  // data because secondaries replicate operations from the primary with some
  // delay - ensure that your application can tolerate stale data if you choose
  // to use a non-primary mode
  // - rpPrimary: Default mode - all operations read from the current replica
  // set primary
  // - rpPrimaryPreferred: in most situations, operations read from the primary
  // but if it is unavailable, operations read from secondary members.
  // - rpPsecondary: all operations read from the secondary members
  // of the replica set
  // - rpPsecondaryPreferred: in most situations, operations read from
  // secondary members but if no secondary members are available, operations
  // read from the primary
  // rpNearest: read from the member of the replica set with the least network
  // latency, irrespective of whether that member is a primary or secondary
  // (in practice, we won't use latency, just a random distribution)
  TMongoClientReplicaSetReadPreference = (
    rpPrimary,
    rpPrimaryPreferred,
    rpSecondary,
    rpSecondaryPreferred,
    rpNearest);

  /// define Write Concern property of a MongoDB connection
  // - Write concern describes the guarantee that MongoDB provides when
  // reporting on the success of a write operation. The strength of the write
  // concerns determine the level of guarantee. When inserts, updates and
  // deletes have a weak write concern, write operations return quickly. In
  // some failure cases, write operations issued with weak write concerns may
  // not persist. With stronger write concerns, clients wait after sending a
  // write operation for MongoDB to confirm the write operations. MongoDB
  // provides different levels of write concern to better address the specific
  // needs of applications. Clients may adjust write concern to ensure that
  // the most important operations persist successfully to an entire
  // MongoDB deployment. For other less critical operations, clients can
  // adjust the write concern to ensure faster performance rather than
  // ensure persistence to the entire deployment.
  // - wcAcknowledged is the default safe mode: the mongod confirms the
  // receipt of the write operation. Acknowledged write concern allows clients
  // to catch network, duplicate key, and other errors.
  // - with wcJournaled, the mongod acknowledges the write operation only
  // after committing the data to the journal. This write concern ensures that
  // MongoDB can recover the data following a shutdown or power interruption.
  // - wcReplicaAcknowledged will guarantee that the write operation propagates
  // to at least one member of a replica set
  // - with wcUnacknowledged, MongoDB does not acknowledge the receipt of
  // write operation. Unacknowledged is similar to errors ignored; however,
  // drivers attempt to receive and handle network errors when possible. The
  // driver's ability to detect network errors depends on the system's
  // networking configuration.
  // - with wcErrorsIgnored, MongoDB does not acknowledge write operations.
  // With this level of write concern, the client cannot detect failed write
  // operations. These errors include connection errors and mongod exceptions
  // such as duplicate key exceptions for unique indexes. Although the errors
  // ignored write concern provides fast performance, this performance gain
  // comes at the cost of significant risks for data persistence and durability.
  // WARNING: Do not use wcErrorsIgnored write concern in normal operation.
  TMongoClientWriteConcern = (
    wcAcknowledged,
    wcJournaled,
    wcReplicaAcknowledged,
    wcUnacknowledged,
    wcErrorsIgnored);

  /// the known WireVersion as defined since Mongo 2.4.4
  // - we named ambiguous WireVersion 0..9 values from 2.4.4 to 4.4
  // - any version >= 10 matches the FCV defined in mongo-master releases.yml
  // - e.g. version 5.0 maps 13, and 6.1 equals 18
  // - swvInternal = 100 is a theoritical value used internally for testing
  TMongoServerWireVersion = (
    swvUnsupported,
    swvAggReturnsCursors,
    swvInsertUpdateDelete,
    swvScramSha1ListIndexesCollectionsExplain277,
    swvFindGetMore,
    swvCmdWriteConcern,
    swvOpMsg36,
    swvReplicaSetTransaction40,
    swvShardedTransaction42,
    swvResumeInitialSync44,
    swvInternal = 100);

  /// remote access to a MongoDB server
  // - a single server can have several active connections, if some secondary
  // hosts were defined
  TMongoClient = class
  protected
    fConnectionString: RawUtf8;
    fDatabases: TRawUtf8List;
    fConnections: TMongoConnectionDynArray;
    fReadPreference: TMongoClientReplicaSetReadPreference;
    fWriteConcern: TMongoClientWriteConcern;
    fConnectionTimeOut: cardinal;
    fOptions: TMongoClientOptions;
    fServerReadOnly: boolean;
    fServerMaxWireVersion: TMongoServerWireVersion;
    fGracefulReconnect: record
      Enabled, ForcedDBCR: boolean;
      User, Database: RawUtf8;
      EncryptedDigest: RawByteString;
    end;
    fFindBatchSize, fGetMoreBatchSize: integer;
    fLog: TSynLog;
    fLogRequestEvent: TSynLogInfo;
    fLogReplyEvent: TSynLogInfo;
    fLogReplyEventMaxSize: cardinal;
    fServerBuildInfo: variant;
    fServerBuildInfoNumber: cardinal;
    fServerMaxBsonObjectSize: integer;
    fServerMaxMessageSizeBytes: integer;
    fServerMaxWriteBatchSize: integer;
    fServerInfo: variant;
    fLatestReadConnectionIndex: PtrInt;
    procedure AfterOpen(ConnectionIndex: PtrInt); virtual;
    function GetOneReadConnection: TMongoConnection;
    function GetBytesReceived: Int64;
    function GetBytesSent: Int64;
    function GetBytesTransmitted: Int64;
    procedure Auth(const DatabaseName, UserName, Digest: RawUtf8;
      ForceMongoDBCR: boolean; ConnectionIndex: PtrInt);
    function ReOpen: boolean;
  public
    /// the optional low-level TLS parameters
    // - used when mcoTls was included in client Options
    ConnectionTlsContext: TNetTlsContext;
    /// the optional low-level Proxy parameters
    ConnectionTunnel: TUri;
    /// prepare a connection to a MongoDB server or Replica Set
    // - this constructor won't create the connection until the Open or
    // OpenAuth method is called
    // - you can specify multiple hosts, as CSV values, if necessary
    // - you may request for a mcoTls secured connection (optionally setting
    // ConnectionTlsContext parameters)
    constructor Create(const Host: RawUtf8; Port: integer = MONGODB_DEFAULTPORT;
      aOptions: TMongoClientOptions = []; const SecondaryHostCsv: RawUtf8 = '';
      const SecondaryPortCsv: RawUtf8 = ''); overload;
    /// connect to a database on a remote MongoDB primary server
    // - this method won't use authentication, and will return the corresponding
    // MongoDB database instance
    // - this method is an alias to the Database[] property
    function Open(const DatabaseName: RawUtf8): TMongoDatabase;
    /// secure connection to a database on a remote MongoDB server
    // - this method will use authentication and will return the corresponding
    // MongoDB database instance, with a dedicated secured connection
    // - will use MONGODB-CR for MongoDB engines up to 2.6 (or if ForceMongoDBCR
    // is TRUE), and SCRAM-SHA-1 since MongoDB 3.x
    // - see http://docs.mongodb.org/manual/administration/security-access-control
    function OpenAuth(const DatabaseName, UserName, PassWord: RawUtf8;
      ForceMongoDBCR: boolean = false): TMongoDatabase;
    /// close the connection and release all associated TMongoDatabase,
    // TMongoCollection and TMongoConnection instances
    destructor Destroy; override;
    /// define an optional logging instance to be used
    // - you can also specify the event types to be used for requests or
    // replay: by default, a verbose log with sllSQL and sllDB will be set
    // - e.g. mormot.orm.mongodb.pas will call Client.SetLog(SQLite3Log) for you
    procedure SetLog(LogClass: TSynLogClass; RequestEvent: TSynLogInfo = sllSQL;
      ReplyEvent: TSynLogInfo = sllDB; ReplyEventMaxSize: cardinal = 1024);

    /// retrieve extended server version and build information, as text
    // - will create a string from ServerBuildInfo object, e.g. as
    // $ 'MongoDB 3.2.0 mozjs mmapv1,wiredTiger'
    function ServerBuildInfoText: RawUtf8;
    /// retrieve the server version and build information after Open
    // - return the content as a TDocVariant document, e.g.
    // ! ServerBuildInfo.version = '2.4.9'
    // ! ServerBuildInfo.versionArray = [2,4,9,0]
    // - this property is cached, so request is sent only once at connection
    // - you may rather use ServerBuildInfoNumber to check for available
    // features at runtime, for easy comparison of the server version
    property ServerBuildInfo: variant
      read fServerBuildInfo;
    /// retrieve some server information at connection after Open
    // - return the content as a TDocVariant document, e.g.
    // ! ServerInfo.isWritablePrimary = true
    // ! ServerInfo.maxBsonObjectSize = 16777216
    // ! ServerInfo.maxWireVersion = 13
    // - this property is cached, so request is sent only once at connection
    // - reflected to the ServerMaxBsonObjectSize, ServerMaxMessageSizeBytes,
    // ServerMaxWriteBatchSize, ServerMaxWireVersion and ServerReadOnly properties
    property ServerInfo: variant
      read fServerInfo;
    /// the maximum permitted size of a BSON object in bytes for this server
    // - default is 16777216, i.e. 16MB
    property ServerMaxBsonObjectSize: integer
      read fServerMaxBsonObjectSize write fServerMaxBsonObjectSize;
    /// the maximum permitted size of a BSON wire protocol message (OP_MSG)
    // - default is 48000000, i.e. 45.8MB
    property ServerMaxMessageSizeBytes: integer
      read fServerMaxMessageSizeBytes write fServerMaxMessageSizeBytes;
    /// the maximum number of write operations permitted in a write batch
    // - if a batch exceeds this limit, the client driver should divide the
    // batch into smaller groups each with counts less than or equal to the
    // value of this field
    // - default is 100000
    property ServerMaxWriteBatchSize: integer
      read fServerMaxWriteBatchSize write fServerMaxWriteBatchSize;
    /// the maximum supported WireVersion b this server
    // - minimum is usually - and abusively - always reported as 0
    property ServerMaxWireVersion: TMongoServerWireVersion
      read fServerMaxWireVersion;
    /// true if this server instance is running in Read/Only mode
    property ServerReadOnly: boolean
      read fServerReadOnly;
    /// access to a given MongoDB database
    // - try to open it via a non-authenticated connection it if not already:
    // will raise an exception on error, or will return an instance
    // - will return an existing instance if has already been opened
    property Database[const DatabaseName: RawUtf8]: TMongoDatabase
      read Open; default;
    /// low-level access to the TCP/IP connections of this MongoDB replica set
    // - first item [0] is the Primary member
    // - other items [1..] are the Secondary members
    property Connections: TMongoConnectionDynArray
      read fConnections;
    /// define the logging instance to be used for LogRequestEvent/LogReplyEvent
    // - you may also call the SetLog() method to set all options at once
    property Log: TSynLog
      read fLog write fLog;
  published
    /// the connection definition used to connect to this MongoDB server
    property ConnectionString: RawUtf8
      read fConnectionString;
    /// retrieve the server version and build information
    // - return the content as a TDocVariant document, e.g.
    // ! 02040900 for MongoDB 2.4.9, or 02060000 for MongoDB 2.6, or
    // ! 03000300 for MongoDB 3.0.3
    // - this property is cached, so can be used to check for available
    // features at runtime, without any performance penalty
    property ServerBuildInfoNumber: cardinal
      read fServerBuildInfoNumber;
    /// define Read Preference mode to a MongoDB replica set
    // - see http://docs.mongodb.org/manual/core/read-preference
    // - default is rpPrimary, i.e. reading from the main primary instance
    // - Important: All read preference modes except rpPrimary may return stale
    // data because secondaries replicate operations from the primary with some
    // delay - ensure that your application can tolerate stale data if you choose
    // to use a non-primary mode
    property ReadPreference: TMongoClientReplicaSetReadPreference
      read fReadPreference write fReadPreference;
    /// define Write Concern mode to a MongoDB replica set
    // - see http://docs.mongodb.org/manual/core/write-concern
    // - default is wcAcknowledged, i.e. to acknowledge all write operations
    property WriteConcern: TMongoClientWriteConcern
      read fWriteConcern write fWriteConcern;
    /// the connection time out, in milliseconds
    // - default value is 30000, i.e. 30 seconds
    property ConnectionTimeOut: cardinal
      read fConnectionTimeOut write fConnectionTimeOut;
    /// the options of this socket connection
    property Options: TMongoClientOptions
      read fOptions;
    /// allow automatic reconnection (with authentication, if applying), if the
    // socket is closed (e.g. was dropped from the server)
    property GracefulReconnect: boolean
      read fGracefulReconnect.Enabled write fGracefulReconnect.Enabled;
    /// how many documents are returned at once from the server per OP_MSG
    // - i.e. the "batchSize" argument as supplied to "find" and "aggregate"
    // - we override the default MongoDB of value of 101, which is fine for
    // a shell but less for a software client, by setting 65536 here
    property FindBatchSize: integer
      read fFindBatchSize write fFindBatchSize;
    /// how many documents are returned at once from the server per OP_MSG
    // - i.e. the "batchSize" argument as supplied to "getMore"
    // - we override the default MongoDB of value of 101, which is fine for
    // a shell but less for a software client, by setting 65536 here
    property GetMoreBatchSize: integer
      read fGetMoreBatchSize write fGetMoreBatchSize;
    /// how may bytes this client did received, among all its connections
    property BytesReceived: Int64
      read GetBytesReceived;
    /// how may bytes this client did received, among all its connections
    property BytesSent: Int64
      read GetBytesSent;
    /// how may bytes this client did transmit, adding both input and output
    property BytesTransmitted: Int64
      read GetBytesTransmitted;
    /// if set to something else than default sllNone, will log each request
    // with the corresponding logging event kind
    // - will use the Log property for the destination log
    // - you may also call the SetLog() method to set all options at once
    property LogRequestEvent: TSynLogInfo
      read fLogRequestEvent write fLogRequestEvent;
    /// if set to something else than default sllNone, will log each reply
    // with the corresponding logging event kind
    // - WARNING: logging all incoming data may be very verbose, e.g. when
    // retrieving a document list - use it with care, not on production, but
    // only for debugging purposes - or set LogReplyEventMaxSize to a low value
    // - will use the Log property for the destination log
    // - you may also call the SetLog() method to set all options at once
    property LogReplyEvent: TSynLogInfo
      read fLogReplyEvent write fLogReplyEvent;
    /// defines how many characters a LogReplyEvent entry may append in the log
    // - is set by default to 1024, which sounds somewhat good for debugging
    property LogReplyEventMaxSize: cardinal
      read fLogReplyEventMaxSize write fLogReplyEventMaxSize;
  end;

  /// remote access to a MongoDB database
  TMongoDatabase = class
  protected
    fClient: TMongoClient;
    fName: RawUtf8;
    fCollections: TRawUtf8List;
    function GetCollection(const Name: RawUtf8): TMongoCollection;
    function GetCollectionOrCreate(const Name: RawUtf8): TMongoCollection;
    function GetCollectionOrNil(const Name: RawUtf8): TMongoCollection;
  public
    /// initialize a reference to a given MongoDB Database
    // - you should not use this constructor directly, but rather use the
    // TMongoClient.Database[] property
    // - it will connect to the Client's primary host, then retrieve all
    // collection names of this database
    constructor Create(aClient: TMongoClient; const aDatabaseName: RawUtf8);
    /// release all associated TMongoCollection instances
    destructor Destroy; override;

    /// run a database command, supplied as a TDocVariant, TBsonVariant or a
    // string, and return a TDocVariant instance
    // - this is the preferred method to issue database commands, as it provides
    // a consistent interface between the MongoDB shell and this driver
    // - see http://docs.mongodb.org/manual/reference/command for a list
    // of all available commands
    // - for instance:
    // ! RunCommand(_ObjFast(['dbStats',1,'scale',1024],stats);
    // ! RunCommand(BsonVariant(['dbStats',1,'scale',1024],stats);
    // ! RunCommand('dbStats',stats);
    // ! RunCommand('hostInfo',host);
    // - the message will be returned by the server as a TDocVariant instance
    // (since the associated TMongoRequestQuery.NumberToSkip=1)
    // - in case of any error, the error message is returned as text
    // - in case of success, this method will return ''
    // - if you expects the result to return a cursor, e.g. for a "find" or
    // "aggregate" command, you can specify collName so that the "cursor"
    // response will be parsed and its nested batch array, and call "getMore"
    // if needed
    function RunCommand(const command: variant;
      var returnedValue: variant; const collName: RawUtf8 = ''): RawUtf8; overload;
    /// run a database command, supplied as a TDocVariant, TBsonVariant or a
    // string, and return the raw BSON document array of received items
    // - this overloaded method can be used on huge content to avoid the slower
    // conversion to an array of TDocVariant instances
    // - in case of success, this method will return TRUE, or FALSE on error
    // - if you expects the result to return a cursor, e.g. for a "find" or
    // "aggregate" command, you can specify collName so that the "cursor"
    // response will be parsed and its nested batch array, and call "getMore"
    // if needed
    function RunCommand(const command: variant;
      var returnedValue: TBsonDocument; const collName: RawUtf8 = ''): boolean; overload;

    /// create the user in the database to which the user will belong
    // - you could specify the roles to use, for this database or others:
    // ! reportingDB.CreateUser('reportsUser','12345678',BsonVariant(
    // !  '[{ role: "readWrite", db: "reporting" }, { role: "read", db: "products" }]'));
    // - returns '' on sucess, an error message otherwise
    function CreateUser(const UserName, Password: RawUtf8;
      const roles: variant): RawUtf8;
    /// create the user with a read or read/write role on the current database
    // - returns '' on sucess, an error message otherwise
    function CreateUserForThisDatabase(const UserName, Password: RawUtf8;
      allowWrite: boolean = true): RawUtf8;
    /// deletes the supplied user on the current database
    // - returns '' on sucess, an error message otherwise
    function DropUser(const UserName: RawUtf8): RawUtf8;

    /// access to a given MongoDB collection
    // - raise an EMongoDatabaseException if the collection name does not exist
    property Collection[const Name: RawUtf8]: TMongoCollection
      read GetCollection; default;
    /// access to a given MongoDB collection
    // - if the collection name does not exist, it will return nil
    property CollectionOrNil[const Name: RawUtf8]: TMongoCollection
      read GetCollectionOrNil;
    /// access to a given MongoDB collection
    // - if the collection name does not exist, it will add use the name to
    // create a TMongoCollection instance and register it to the internal list
    property CollectionOrCreate[const Name: RawUtf8]: TMongoCollection
      read GetCollectionOrCreate;
  published
    /// the database name
    property Name: RawUtf8
      read fName;
    /// the associated MongoDB client instance
    property Client: TMongoClient
      read fClient;
  end;

  /// remote access to a MongoDB collection
  TMongoCollection = class
  protected
    fDatabase: TMongoDatabase;
    fName: RawUtf8;
    fFullCollectionName: RawUtf8;
    function AggregateCallFromJson(const pipelineJson: RawUtf8;
      var reply, res: variant): boolean; overload;
    function AggregateCallFromVariant(const pipelineArray: variant;
      var reply, res: variant): boolean; overload;
    function DoFind(const Criteria, Projection: Variant; NumberToReturn,
      NumberToSkip: integer; Flags: TMongoQueryFlags): TMongoRequest;
    {$ifndef MONGO_OLDPROTOCOL}
    function DoOperationCommand(const Command, DocsName: RawUtf8;
      const Docs: TBsonDocument; ContOnFail, BypassValidation: boolean): integer;
    {$endif MONGO_OLDPROTOCOL}
  public
    /// initialize a reference to a given MongoDB Collection
    // - you should not use this constructor directly, but rather use
    // TMongoClient.Database[].Collection[] property
    constructor Create(aDatabase: TMongoDatabase; const aCollectionName: RawUtf8);

    /// select documents in a collection and returns a dvArray TDocVariant
    // instance containing the selected documents
    // - Criteria can be null (to retrieve all documents) or a TDocVariant /
    // TBsonVariant query selector:
    // ! FindDoc(BsonVariant('{name:"John",age:{$gt:21}}'),null);
    // ! FindDoc(BsonVariant('{name:?,age:{$gt:?}}',[],['John',21]),null);
    // see http://docs.mongodb.org/manual/reference/operator for reference
    // - Projection can be null (to retrieve all fields) or a CSV string to set
    // field names to retrieve, or a TDocVariant or TBsonVariant - e.g.:
    // ! FindDoc(BsonVariant(['name','John']),null);
    // ! FindDoc(BsonVariant(['name','John']),'_id,name');
    // ! FindDoc(BsonVariant(['name','John']),BsonVariantFieldSelector('name,_id'));
    // - NumberToReturn can be left to its default maxInt value to return all
    // matching documents, or specify a limit (e.g. 1 for one document - in this
    // case, the returned instance won't be a dvArray kind of TDocVariant, but
    // either null or the single returned document)
    // - if the query does not have any matching record, it will return null
    function FindDoc(const Criteria, Projection: Variant;
      NumberToReturn:  integer = 1; NumberToSkip: integer = 0;
      Flags: TMongoQueryFlags = []): variant; overload;
    /// select documents in a collection and returns a dvArray TDocVariant
    // instance containing the selected documents
    // - Criteria can specify the query selector as (extended) JSON and
    // parameters:
    // ! FindDoc('{name:"John",age:{$gt:21}}',[]);
    // ! FindDoc('{name:?,age:{$gt:?}}',['John',21]);
    // see http://docs.mongodb.org/manual/reference/operator for reference
    // - this overloaded method will use a null Projection, i.e. will retrieve
    // all fields
    // - NumberToReturn can be left to its default maxInt value to return all
    // matching documents, or specify a limit (e.g. 1 for one document - in this
    // case, the returned instance won't be a dvArray kind of TDocVariant, but
    // either null or the single returned document)
    // - if the query does not have any matching record, it will return null
    function FindDoc(const Criteria: RawUtf8; const Params: array of const;
      NumberToReturn: integer = maxInt; NumberToSkip: integer = 0;
      Flags: TMongoQueryFlags = []): variant; overload;
    /// check for an existing document in a collection, by its _id field
    // - _id will identify the unique document to be retrieved
    function ExistOne(const _id: TBsonObjectID): boolean; overload;
    /// check for an existing document in a collection, by its _id field
    // - _id will identify the unique document to be retrieved e.g. as a TID
    function ExistOne(const _id: variant): boolean; overload;
    /// find an existing document in a collection, by its _id field
    // - _id will identify the unique document to be retrieved
    // - returns null, or a TDocVariant instance
    function FindOne(const _id: TBsonObjectID): variant; overload;
    /// find an existing document in a collection, by its _id field
    // - _id will identify the unique document to be retrieved e.g. as a TID
    // - returns null, or a TDocVariant instance
    function FindOne(const _id: variant): variant; overload;
    /// find an existing document in a collection, by a custom Criteria value
    // - Criteria object, specified as name/value pairs, will identify the
    // unique document to be retrieved
    // - returns the found TDocVariant instance
    // - if the Criteria has no match, return either null or a new object with
    // default values as NameValuePairs if ReturnNewObjectIfNotFound is true
    function FindOne(const NameValuePairs: array of const;
      ReturnNewObjectIfNotFound: boolean = false): variant; overload;
    /// returns a dynamic array of TDocVariant instance containing
    // all documents of a collection
    // - Projection can be null (to retrieve all fields) or a CSV string to set
    // field names to retrieve, or a TDocVariant or TBsonVariant with
    // projection operators
    procedure FindDocs(var result: TVariantDynArray; const Projection: variant;
      NumberToReturn: integer = maxInt; NumberToSkip: integer = 0;
      Flags: TMongoQueryFlags = []); overload;
    /// select documents in a collection and returns a dynamic array of
    // TDocVariant instance containing the selected documents
    // - you can e.g. fill a res: TVariantDynArray with the following query:
    // ! FindDocs('{name:?,age:{$gt:?}}',['John',21],res,null);
    // - Projection can be null (to retrieve all fields) or a CSV string to set
    // field names to retrieve, or a TDocVariant or TBsonVariant with
    // projection operators
    procedure FindDocs(const Criteria: RawUtf8; const Params: array of const;
      var result: TVariantDynArray; const Projection: variant;
      NumberToReturn: integer = maxInt; NumberToSkip: integer = 0;
      Flags: TMongoQueryFlags = []); overload;
    /// select documents in a collection and returns a dynamic array of
    // TDocVariant instance containing the selected documents
    // - could be used to fill a VCL grid using a TDocVariantArrayDataSet
    // as defined in mormot.ui.rad.pas:
    // ! ds1.DataSet := VariantsToDataSet(self,
    // !   FindDocs('{name:?,age:{$gt:?}}', ['John',21], null));
    // - Projection can be null (to retrieve all fields) or a CSV string to set
    // field names to retrieve, or a TDocVariant or TBsonVariant with
    // projection operators
    function FindDocs(const Criteria: RawUtf8;
      const Params: array of const; const Projection: variant;
      NumberToReturn: integer = maxInt; NumberToSkip: integer = 0;
      Flags: TMongoQueryFlags = []): TVariantDynArray; overload;

    /// select documents in a collection and returns a JSON array of documents
    // containing the selected documents
    // - Criteria can be null (to retrieve all documents) or a TDocVariant /
    // TBsonVariant query selector:
    // ! FindJson(BsonVariant('{name:"John",age:{$gt:21}}'),null);
    // ! FindJson(BsonVariant('{name:?,age:{$gt:?}}',[],['John',21]),null);
    // see http://docs.mongodb.org/manual/reference/operator for reference
    // - Projection can be null (to retrieve all fields) or a CSV string to set
    // the field names to retrieve, or a TDocVariant or TBsonVariant - e.g.:
    // ! FindJson(BsonVariant(['name','John']),null);
    // ! FindJson(BsonVariant(['name','John']),'_id');
    // ! FindJson(BsonVariant(['name','John']),BsonVariantFieldSelector('name,_id'));
    // - NumberToReturn can be left to its default maxInt value to return all
    // matching documents as a '[..]' JSON array, or specify a limit (e.g. 1
    // for one document - in this case, the returned instance won't be a '[..]'
    // JSON array, but either 'null' or a single '{..}' JSON object)
    // - this method is very optimized and will convert the BSON binary content
    // directly into JSON, in either modMongoStrict or modMongoShell layout
    // (modNoMongo will do the same as modMongoStrict)
    function FindJson(const Criteria, Projection: Variant;
      NumberToReturn: integer = maxInt; NumberToSkip: integer = 0;
      Flags: TMongoQueryFlags = [];
      Mode: TMongoJsonMode = modMongoStrict): RawUtf8; overload;
    /// select documents in a collection and returns a JSON array of documents
    // containing the selected documents
    // - Criteria can specify the query selector as (extended) JSON and
    // parameters:
    // ! FindJson('{name:"John",age:{$gt:21}}',[]);
    // ! FindJson('{name:?,age:{$gt:?}}',['John',21]);
    // see http://docs.mongodb.org/manual/reference/operator for reference
    // - this overloaded method will use a null Projection, i.e. will retrieve
    // all fields
    // - NumberToReturn can be left to its default maxInt value to return all
    // matching documents as a '[..]' JSON array, or specify a limit (e.g. 1
    // for one document - in this case, the returned instance won't be a '[..]'
    // JSON array, but either 'null' or a single '{..}' JSON object)
    function FindJson(const Criteria: RawUtf8; const Params: array of const;
      NumberToReturn: integer = maxInt; NumberToSkip: integer = 0;
      Flags: TMongoQueryFlags = [];
      Mode: TMongoJsonMode = modMongoStrict): RawUtf8; overload;
    /// select documents in a collection and returns a JSON array of documents
    // containing the selected documents
    // - Criteria and Projection can specify the query selector as (extended)
    // JSON and parameters
    function FindJson(const Criteria: RawUtf8; const Params: array of const;
      const Projection: variant; NumberToReturn: integer = maxInt;
      NumberToSkip: integer = 0; Flags: TMongoQueryFlags = [];
      Mode: TMongoJsonMode = modMongoStrict): RawUtf8; overload;

    /// select documents in a collection and returns a TBsonDocument instance
    // containing the selected documents as a raw binary BSON array document
    // - Criteria can be null (to retrieve all documents) or a TDocVariant /
    // TBsonVariant query selector:
    // ! FindBson(BsonVariant('{name:"John",age:{$gt:21}}'),null);
    // ! FindBson(BsonVariant('{name:?,age:{$gt:?}}',[],['John',21]),null);
    // - Projection can be null (to retrieve all fields) or a CSV string to set
    // the field names to retrieve, or a TDocVariant or TBsonVariant - e.g.:
    // ! FindBson(BsonVariant(['name','John']),null);
    // ! FindBson(BsonVariant(['name','John']),'_id');
    // ! FindBson(BsonVariant(['name','John']),BsonVariantFieldSelector('name,_id'));
    // - NumberToReturn can be left to its default maxInt value to return all
    // matching documents, or specify a limit (e.g. 1 for one document)
    function FindBson(const Criteria, Projection: Variant;
      NumberToReturn: integer = maxInt; NumberToSkip: integer = 0;
      Flags: TMongoQueryFlags = []): TBsonDocument;

    /// insert one or more documents in the collection
    // - Documents is an array of TDocVariant (i.e. created via _JsonFast()
    // or _JsonFastFmt()) - or of TBsonVariant (created via BsonVariant())
    // - returns the number of inserted documents
    function Insert(const Documents: array of variant;
      Flags: TMongoInsertFlags = []
      {$ifdef MONGO_OLDPROTOCOL}; NoAcknowledge: boolean = false{$endif}
      ): integer; overload;
    /// insert one or more documents in the collection
    // - Documents is the low-level concatenation of BSON documents, created
    // e.g. with a TBsonWriter stream
    // - returns the number of inserted documents
    function Insert(const Documents: TBsonDocument;
      Flags: TMongoInsertFlags = []
      {$ifdef MONGO_OLDPROTOCOL}; NoAcknowledge: boolean = false{$endif}
      ): integer; overload;
    /// insert one document, supplied as (extended) JSON and parameters,
    // in the collection
    // - supplied JSON could be either strict or in MongoDB Shell syntax:
    // !   products.insert('{ _id: ?, item: ?, qty: ? }',[1,'card',15]);
    // !   // here _id is forced on the client side
    // !   products.insert('{ item: ?, qty: ? }',[1,'card',15]);
    // !   // here the _id will be created on the client side as an ObjectID
    // - you can retrieve the associated ObjectID, as such:
    // ! var oid: TBsonObjectID;
    // ! ...
    // !   products.insert('{ item: ?, qty: ? }',['card',15],@oid);
    // !   writeln(oid.ToText);
    procedure Insert(const Document: RawUtf8; const Params: array of const;
      DocumentObjectID: PBsonObjectID = nil); overload;

    /// updates an existing document or inserts a new document, depending on
    // its document parameter
    // - this document should be a TDocVariant (i.e. created via _JsonFast()
    // or _JsonFastFmt()) since we need to check for the _id field, other types
    // will be converted to a TDocVariant instance (via its JSON representation)
    // so it is pointless to use BsonVariant() here
    // - if the document does not contain an _id field, then the Save() method
    // performs an insert; during the operation, the client will add to the
    // Document variant the _id field and assign it a unique ObjectId - and the
    // method returns FALSE
    // - if the document contains an _id field, then the save() method performs
    // an upsert, querying the collection on the _id field: if a document does
    // not exist with the specified _id value, the save() method performs an
    // insert; if a document exists with the specified _id value, the save()
    // method performs an update that replaces ALL fields in the existing
    // document with the fields from the document - and the method returns TRUE
    // - you can optionally retrieve the _id value with the DocumentObjectID pointer
    function Save(var Document: variant;
      DocumentObjectID: PBsonObjectID = nil): boolean; overload;
    /// updates an existing document or inserts a new document, depending on
    // its document parameter, supplied as (extended) JSON and parameters
    // - supplied JSON could be either strict or in MongoDB Shell syntax:
    // - will perform either an insert or an update, depending of the
    // presence of the _id field, as overloaded Save(const Document: variant)
    procedure Save(const Document: RawUtf8; const Params: array of const;
      DocumentObjectID: PBsonObjectID = nil); overload;

    /// modifies an existing document or several documents in a collection
    // - the method can modify specific fields of existing document or documents
    // or replace an existing document entirely, depending on the update parameter
    // - Query and Update parameters should be TDocVariant (i.e. created via
    // _JsonFast() or _JsonFastFmt()) or TBsonVariant (created via BsonVariant())
    // - Query is the selection criteria for the update; use the same query
    // selectors as used in the Find() method
    // - if Update contains a plain document, it will replace any existing data
    // - if Update contains update operators (like $set), it will update the
    // corresponding fields in the document
    // - returns the number of updated documents
    function Update(const Query, Update: variant;
      Flags: TMongoUpdateFlags = []): integer; overload;
    {$ifndef MONGO_OLDPROTOCOL}
    /// modifies several documents in a collection
    // - this overloaded function accept Queries/Updates pairs so that all
    // update would be processed as a single bulk call
    function Update(const Queries, Updates: TVariantDynArray;
      Flags: TMongoUpdateFlags = []): integer; overload;
    {$endif MONGO_OLDPROTOCOL}
    /// modifies an existing document or several documents in a collection
    // - the method can modify specific fields of existing document or documents
    // or replace an existing document entirely, depending on the update parameter
    // - since all content will be transformed into JSON internally, use this
    // method only if the supplied parameters are simple types: any complex value
    // (e.g. a TDateTime or a BsonVariant binary) won't be handled as expected -
    // use the overloaded Update() with explicit BsonVariant() values instead
    // - Query and Update parameters can be specified as JSON objects with
    // parameters
    // - Query is the selection criteria for the update; use the same query
    // selectors as used in the Find() method
    // - if Update contains a plain document, it will replace any existing data:
    // ! people.update('{name:?}',['Andy'],'{name:?,age:? }',['Andy',25],[mufUpsert]);
    // Warning: to avoid inserting the same document more than once, only use
    // mufUpsert if the query field is uniquely indexed
    // - if Update contains update operators (like $set), it will update the
    // corresponding fields in the document:
    // ! book.insert('{_id:?,item:?,stock:?}',[11,'Divine Comedy',2]);
    // ! book.update('{item:?},['Divine Comedy'],'{$set:{price:?},$inc:{stock:?}},[18,5]);
    // ! // the updated document is now:
    // ! { "_id" : 11, "item" : "Divine Comedy", "price" : 18, "stock" : 7 }
    procedure Update(
      const Query: RawUtf8; const QueryParams: array of const;
      const Update: RawUtf8; const UpdateParams: array of const;
      Flags: TMongoUpdateFlags = []); overload;
    /// modifies some fields of an existing document in a collection
    // - by default, Update() or Save() will replace the whole document
    // - this method will expect the identifier to be supplied as a variant -
    // may be via the ObjectID() function
    // - and will replace the specified fields, i.e. it will execute a $set:
    // with the supplied UpdatedFields value
    procedure UpdateOne(const _id, UpdatedFields: variant);

    /// delete an existing document or several documents in a collection
    // - Query parameter should be TDocVariant (i.e. created via _JsonFast() or
    // _JsonFastFmt()) or TBsonVariant (created via BsonVariant())
    // - Query is the selection criteria for the deletion; use the same query
    // selectors as used in the Find() method
    // - to limit the deletion to just one document, set Flags to [mdfSingleRemove]
    // - to delete all documents matching the deletion criteria, leave it to []
    // - returns the number of updated documents
    function Remove(const Query: variant; Flags: TMongoDeleteFlags = []): integer; overload;
    /// delete an existing document in a collection, by its _id field
    // - _id will identify the unique document to be deleted
    procedure RemoveOne(const _id: TBsonObjectID); overload;
    /// delete an existing document in a collection, by its _id field
    // - _id will identify the unique document to be deleted e.g. as TID
    procedure RemoveOne(const _id: variant); overload;
    /// delete an existing document or several documents in a collection
    // - Query parameter can be specified as JSON objects with parameters
    // - Query is the selection criteria for the deletion; use the same query
    // selectors as used in the Find() method
    // - to limit the deletion to just one document, set Flags to [mdfSingleRemove]
    // - to delete all documents matching the deletion criteria, leave it to []
    procedure RemoveFmt(const Query: RawUtf8; const Params: array of const;
      Flags: TMongoDeleteFlags = []);

    /// creates an index on the specified field(s) if the index does
    // not already exist
    // - Keys and Options parameters should be TDocVariant (e.g. created via
    // _JsonFast() or _JsonFastFmt()) - and not TBsonVariant values
    // - for ascending/descending indexes, Keys is a document that contains pairs
    // with the name of the field or fields to index and order of the index:
    // value of 1 specifies ascending and of -1 specifies descending
    // - options is a non-mandatory document that controls the creation
    // of the index -
    // - you can write e.g.
    // ! book.EnsureIndex(_JsonFast('{ orderDate: 1 }'),null)
    // ! book.EnsureIndex(_ObjFast(['orderDate',1]),null)
    procedure EnsureIndex(const Keys, Options: variant); overload;
    /// creates an index on the specified field(s) if the index does
    // not already exist
    // - Keys are the correspondiong field names
    // - you can write e.g. to create an ascending index on a given field:
    // ! book.EnsureIndex(['orderDate']);
    procedure EnsureIndex(const Keys: array of RawUtf8;
      Ascending: boolean = true; Unique: boolean = false); overload;
    /// drops the entire collection from the database
    // - once dropped, this TMongoCollection instance will be freed: never
    // use this instance again after success (i.e. returned '')
    // - in case of error, a textual message will be returned as result
    // - once dropped, this collection will be removed from the parent
    // Database.Collection[] internal list
    // - Warning: this method obtains a write lock on the affected database
    // and will block other operations until it has completed
    function Drop: RawUtf8;

    /// calculate the number of documents in the collection
    // - be aware that this method may be somewhat slow for huge collections,
    // since a full scan of an index is to be performed: if your purpose is
    // to ensure that a collection contains items, use rather IsEmpty method
    function Count: Int64;
    /// calculate the number of documents in the collection that match
    // a specific query
    // - Criteria can specify the query selector as a BsonVariant/TDocVariant
    function FindCount(const Query: variant): Int64; overload;
    /// calculate the number of documents in the collection that match
    // a specific query
    // - Criteria can specify the query selector as (extended) JSON and
    // parameters:
    // ! FindCount('{name:?,age:{$gt:?}}',[],['John',21]);
    // ! FindCount('{ ord_dt: { $gt: new Date(?) } }',[],[trunc(Now)-7]);
    // - optional MaxNumberToReturn can specify a limit for the search (e.g. if
    // you do not want an exact count, but only check for a specific limit)
    // - optional NumberToSkip can specify the number of matching documents
    // to skip before counting
    function FindCount(const Criteria: RawUtf8; const Args, Params: array of const;
      MaxNumberToReturn: integer = 0; NumberToSkip: integer = 0): Int64; overload;
    /// returns TRUE if the collection has no document, FALSE otherwise
    // - is much faster than Count, especially for huge collections
    function IsEmpty: boolean;
    /// calculate aggregate values using the MongoDB aggregation framework
    // and return the result as a TDocVariant instance
    // - the Aggregation Framework was designed to be more efficient than the
    // alternative map-reduce pattern, and is available since MongoDB 2.2 -
    // see http://docs.mongodb.org/manual/reference/command/aggregate
    // - you should specify the aggregation pipeline as a list of JSON object
    // operators (without the [..]) - for reference of all available phases,
    // see http://docs.mongodb.org/manual/core/aggregation-pipeline
    // - if the server sent back no {result:...} member, will return null
    // - if the server sent back one item as {result:[{..}]}, will return
    // this single item as a TDocVariant
    // - if the server sent back several items as {result:[{..},{..}]}, will
    // return a dvArray kind of TDocVariant
    function AggregateDoc(const Operators: RawUtf8;
      const Params: array of const): variant; overload;
    /// calculate JSON aggregate values using the MongoDB aggregation framework
    // - the Aggregation Framework was designed to be more efficient than the
    // alternative map-reduce pattern, and is available since MongoDB 2.2 -
    // see http://docs.mongodb.org/manual/reference/command/aggregate
    // - you should specify the aggregation pipeline as a list of JSON object
    // operators (without the [..]) - for reference of all available phases,
    // see http://docs.mongodb.org/manual/core/aggregation-pipeline
    // - for instance, the following will return as JSON a collection sorted in
    // descending order according by the age field and then in ascending order
    // according to the value in the posts field
    // ! AggregateJson('{ $sort : { age : -1, posts: 1 } }',[])
    function AggregateJson(const Operators: RawUtf8;
      const Params: array of const;
      Mode: TMongoJsonMode = modMongoStrict): RawUtf8; overload;
    /// calculate aggregate values using the MongoDB aggregation framework
    // and return the result as a TDocVariant instance
    // - overloaded method to specify the pipeline as a BSON raw document
    // as detailed by http://docs.mongodb.org/manual/core/aggregation-pipeline
    function AggregateDocFromVariant(const pipelineArray: variant): variant;
    /// calculate JSON aggregate values using the MongoDB aggregation framework
    // - overloaded method to specify the pipeline as a BSON raw document
    // as detailed by http://docs.mongodb.org/manual/core/aggregation-pipeline
    function AggregateJsonFromVariant(const pipelineArray: variant;
      Mode: TMongoJsonMode = modMongoStrict): RawUtf8; overload;
    /// calculate aggregate values using the MongoDB aggregation framework
    // and return the result as a TDocVariant instance
    // - overloaded method to specify the pipeline as a JSON text object
    // as detailed by http://docs.mongodb.org/manual/core/aggregation-pipeline
    // - for instance, the following will return the maximum _id value of
    // the collection:
    // ! AggregateDoc('{$group:{_id:null,max:{$max:"$_id"}}}').max
    function AggregateDocFromJson(const PipelineJson: RawUtf8): variant;
    /// calculate JSON aggregate values using the MongoDB aggregation framework
    // - overloaded method to specify the pipeline as a JSON text object
    // as detailed by http://docs.mongodb.org/manual/core/aggregation-pipeline
    function AggregateJsonFromJson(const PipelineJson: RawUtf8;
      Mode: TMongoJsonMode = modMongoStrict): RawUtf8; overload;
  published
    /// the collection name
    property Name: RawUtf8
      read fName;
    /// the full collection name, e.g. 'dbname.collectionname'
    property FullCollectionName: RawUtf8
      read fFullCollectionName;
    /// the associated MongoDB database instance
    property Database: TMongoDatabase
      read fDatabase;
  end;

  /// exception type used for MongoDB process, once connected
  EMongoConnectionException = class(EMongoException)
  protected
    fConnection: TMongoConnection;
  public
    /// initialize the Exception for a given request
    constructor Create(const aMsg: string; aConnection: TMongoConnection);
      reintroduce; overload;
    /// initialize the Exception for a given request
    constructor CreateUtf8(const Format: RawUtf8; const Args: array of const;
      aConnection: TMongoConnection); reintroduce;
  published
    /// the associated connection
    property Connection: TMongoConnection
      read fConnection;
  end;

  EMongoDatabaseException = class(EMongoConnectionException)
  protected
    fDatabase: TMongoDatabase;
  public
    /// initialize the Exception for a given request
    constructor Create(const aMsg: string; aDatabase: TMongoDatabase);
      reintroduce; overload;
    /// initialize the Exception for a given request
    constructor CreateUtf8(const Format: RawUtf8; const Args: array of const;
      aDatabase: TMongoDatabase); reintroduce;
    {$ifndef NOEXCEPTIONINTERCEPT}
    /// used to customize the exception log to contain information about the Query
    // - it will log the database parameters
    function CustomLog(WR: TTextWriter;
      const Context: TSynLogExceptionContext): boolean; override;
    {$endif NOEXCEPTIONINTERCEPT}
  published
    /// the associated Database
    property Database: TMongoDatabase
      read fDatabase;
  end;

  /// exception type used for MongoDB query process
  EMongoRequestException = class(EMongoConnectionException)
  protected
    fRequest: TMongoRequest;
    fError: TMongoReplyCursor;
    fErrorDoc: variant;
    function GetErrorDoc: variant;
  public
    /// initialize the Exception for a given request
    constructor Create(const aMsg: string; aConnection: TMongoConnection;
      aRequest: TMongoRequest = nil); reintroduce; overload;
    /// initialize the Exception for a given request
    constructor CreateUtf8(const Format: RawUtf8; const Args: array of const;
      aConnection: TMongoConnection; aRequest: TMongoRequest); reintroduce;
    /// initialize the Exception for a given request
    constructor Create(const aMsg: string; aConnection: TMongoConnection;
      aRequest: TMongoRequest; const aError: TMongoReplyCursor); reintroduce; overload;
    /// initialize the Exception for a given request
    constructor Create(const aMsg: string; aConnection: TMongoConnection;
      aRequest: TMongoRequest; const aErrorDoc: TDocVariantData); reintroduce; overload;
    {$ifndef NOEXCEPTIONINTERCEPT}
    /// used to customize the exception log to contain information about the Query
    // - it will log both the failing request and the returned error message
    function CustomLog(WR: TTextWriter;
      const Context: TSynLogExceptionContext): boolean; override;
    {$endif NOEXCEPTIONINTERCEPT}
    /// the associated error reply document
    property ErrorReply: TMongoReplyCursor
      read fError;
  published
    /// the associated error reply document, as a TDocVariant instance
    // - will return the first document available in ErrorReply, or the supplied
    // aErrorDoc: TDocVariantData instance
    property ErrorDoc: Variant
      read GetErrorDoc;
  end;

  /// exception type used for MongoDB query process after an Operating System
  // error (e.g. in case of socket error)
  EMongoRequestOSException = class(EMongoRequestException)
  protected
    fSystemLastError: cardinal;
  public
    /// initialize the Exception for a given request, including the last
    // error message retrieved from the operating system
    // - if such an exception is raised, you can use SystemLastError property
    // to retrieve the corresponding Operating System error code
    constructor Create(const aMsg: string; aConnection: TMongoConnection;
      aRequest: TMongoRequest = nil); reintroduce;
    /// contain the associated Operating System last error code
    // - will specify e.g. the kind of communication/socket error
    property SystemLastError: cardinal
      read fSystemLastError;
  end;

{$M-}


/// ready-to-be displayed text of a TMongoClientWriteConcern item
function ToText(wc: TMongoClientWriteConcern): PShortString; overload;

/// ready-to-be displayed text of a TMongoClientReplicaSetReadPreference item
function ToText(pref: TMongoClientReplicaSetReadPreference): PShortString; overload;



implementation

{ ************ MongoDB Protocol Classes }


var
  GlobalRequestID: integer;

{ TMongoRequest }

{$ifdef MONGO_OLDPROTOCOL}

const
  WIRE_OPCODES: array[TMongoOperation] of integer = (
    1,     // opReply
    1000,  // opMsgOld
    2001,  // opUpdate
    2002,  // opInsert
    2004,  // opQuery
    2005,  // opGetMore
    2006,  // opDelete
    2007,  // opKillCursors
    2013); // opMsg

  CLIENT_OPCODES = [
    opUpdate, opInsert, opQuery, opGetMore, opDelete, opKillCursors,
    opMsg];

procedure TMongoRequest.BsonWriteParam(const paramDoc: variant);
begin
  if TVarData(paramDoc).VType = varVariantByRef then
    BsonWriteParam(PVariant(TVarData(paramDoc).VPointer)^)
  else if VarIsStr(paramDoc) then
    BsonWriteProjection(VariantToUtf8(paramDoc))
  else 
    BsonWriteDoc(paramDoc); // TBsonVariant or TDocVariant
end;

{$else}

const
  OP_MSG = 2013;

{$endif MONGO_OLDPROTOCOL}


{$ifdef MONGO_OLDPROTOCOL}
constructor TMongoRequest.Create(const FullCollectionName: RawUtf8;
  opCode: TMongoOperation; requestID, responseTo: integer);
begin
  inherited Create(TRawByteStringStream);
  fFullCollectionName := FullCollectionName;
  Split(fFullCollectionName, '.', fDatabaseName, fCollectionName);
  if requestID = 0 then
    fRequestID := InterlockedIncrement(GlobalRequestID)
  else
    fRequestID := requestID;
  fResponseTo := responseTo;
  // write TMongoWireHeader
  BsonDocumentBegin;
  Write4(fRequestID);
  Write4(fResponseTo);
  if not (opCode in CLIENT_OPCODES) then
    raise EMongoException.CreateUtf8('Unexpected %.Create(opCode=%)',
      [self, ToText(opCode)^]);
  fRequestOpCode := opCode;
  Write4(WIRE_OPCODES[opCode]);
end;
{$else}
constructor TMongoRequest.Create(const Database, Collection: RawUtf8);
begin
  inherited Create(TRawByteStringStream);
  fDatabaseName := Database;
  fCollectionName := Collection;
  fRequestID := InterlockedIncrement(GlobalRequestID);
  fResponseTo := 0;
  // write TMongoWireHeader
  BsonDocumentBegin;
  Write4(fRequestID);
  Write4(fResponseTo);
  Write4(OP_MSG); // always opMsg
end;
{$endif MONGO_OLDPROTOCOL}

procedure TMongoRequest.ToBsonDocument(var result: TBsonDocument);
begin
  if {$ifdef MONGO_OLDPROTOCOL}
     (fRequestOpCode = opReply) or
     {$endif MONGO_OLDPROTOCOL}
     (fRequestID = 0) then
    raise EMongoException.CreateUtf8('Missing proper %.Create() call', [self]);
  if fBsonDocument = '' then
  begin
    BsonDocumentEnd(1, false);
    inherited ToBsonDocument(fBsonDocument);
  end;
  result := fBsonDocument;
end;

procedure TMongoRequest.ToJson(W: TJsonWriter; Mode: TMongoJsonMode);
begin
  if self = nil then
  begin
    W.AddNull;
    exit;
  end;
  W.Add('{');
  {$ifdef MONGO_OLDPROTOCOL} // always OP_MSG
  W.AddShort('collection:"');
  W.AddJsonEscape(pointer(fFullCollectionName));
  W.AddShort('",opCode:');
  W.AddTypedJson(@fRequestOpCode, TypeInfo(TMongoOperation));
  W.Add(',');
  {$endif MONGO_OLDPROTOCOL}
  W.AddShort('requestID:');
  W.AddPointer(PtrUInt(fRequestID), '"');
  if fResponseTo <> 0 then
  begin
    W.AddShort(',responseTo:');
    W.AddPointer(PtrUInt(fResponseTo), '"');
  end;
  W.Add('}');
end;

function TMongoRequest.ToJson(Mode: TMongoJsonMode): RawUtf8;
var
  W: TJsonWriter;
  tmp: TTextWriterStackBuffer;
begin
  W := TJsonWriter.CreateOwnedStream(tmp);
  try
    ToJson(W, Mode);
    W.SetText(result);
  finally
    W.Free;
  end;
end;


{$ifdef MONGO_OLDPROTOCOL}

function ToText(op: TMongoOperation): PShortString;
begin
  result := GetEnumName(TypeInfo(TMongoOperation), ord(op));
end;


{ TMongoRequestUpdate }

constructor TMongoRequestUpdate.Create(const FullCollectionName: RawUtf8;
  const Selector, Update: variant; Flags: TMongoUpdateFlags);
begin
  inherited Create(FullCollectionName, opUpdate, 0, 0);
  fSelector := TVarData(Selector);
  fUpdate := TVarData(Update);
  WriteCollectionName(0, FullCollectionName);
  Write4(byte(Flags));
  BsonWriteParam(Selector);
  BsonWriteParam(Update);
end;

procedure TMongoRequestUpdate.ToJson(W: TJsonWriter; Mode: TMongoJsonMode);
begin
  inherited;
  W.CancelLastChar('}');
  W.AddShort(',selector:');
  AddMongoJson(variant(fSelector), W, modMongoShell);
  W.AddShort(',update:');
  AddMongoJson(variant(fUpdate), W, modMongoShell);
  W.Add('}');
end;


{ TMongoRequestInsert }

constructor TMongoRequestInsert.Create(const FullCollectionName: RawUtf8;
  const Documents: array of variant; Flags: TMongoInsertFlags);
var
  i: PtrInt;
begin
  inherited Create(FullCollectionName, opInsert, 0, 0);
  WriteCollectionName(byte(Flags), FullCollectionName);
  for i := 0 to high(Documents) do
    BsonWriteParam(Documents[i]);
end;

constructor TMongoRequestInsert.Create(const FullCollectionName: RawUtf8;
  const Documents: TBsonDocument; Flags: TMongoInsertFlags);
begin
  inherited Create(FullCollectionName, opInsert, 0, 0);
  WriteCollectionName(byte(Flags), FullCollectionName);
  Write(pointer(Documents), Length(Documents));
end;

constructor TMongoRequestInsert.Create(const FullCollectionName: RawUtf8;
  const JSONDocuments: array of PUtf8Char; Flags: TMongoInsertFlags);
var
  i: PtrInt;
  kind: TBsonElementType;
begin
  inherited Create(FullCollectionName, opInsert, 0, 0);
  WriteCollectionName(byte(Flags), FullCollectionName);
  for i := 0 to high(JSONDocuments) do
    BsonWriteDocFromJson(JSONDocuments[i], nil, kind);
end;


{ TMongoRequestDelete }

constructor TMongoRequestDelete.Create(const FullCollectionName: RawUtf8;
  const Selector: variant; Flags: TMongoDeleteFlags);
begin
  inherited Create(FullCollectionName, opDelete, 0, 0);
  fQuery := TVarData(Selector);
  WriteCollectionName(byte(Flags), FullCollectionName);
  Write4(byte(Flags));
  BsonWriteParam(Selector);
end;

procedure TMongoRequestDelete.ToJson(W: TJsonWriter; Mode: TMongoJsonMode);
begin
  inherited;
  W.CancelLastChar('}');
  W.AddShort(',query:');
  AddMongoJson(variant(fQuery), W, modMongoShell);
  W.Add('}');
end;


{ TMongoRequestQuery }

constructor TMongoRequestQuery.Create(const FullCollectionName: RawUtf8;
  const Query, ReturnFieldsSelector: variant;
  NumberToReturn, NumberToSkip: integer; Flags: TMongoQueryFlags);
begin
  {$ifdef MONGO_OLDPROTOCOL}
  // follow TMongoReplyHeader
  inherited Create(FullCollectionName, opQuery, 0, 0);
  WriteCollectionName(byte(Flags), FullCollectionName);
  Write4(NumberToSkip);
  Write4(NumberToReturn);
  BsonWriteParam(Query);
  {$else}
  // follow TMongoMsgHeader
  inherited Create(FullCollectionName, opMsg, 0, 0); // write TMongoWireHeader
  if Flags <> [] then
    raise EMongoException.CreateUtf8(
      '%.Create: unsupported flags=%',[self, integer(Flags)]);
  Write4(integer(Flags));
  Write1(ord(mmkBody)); // a single document follow
  BsonWriteParam(Query, fDatabaseName);
  {$endif MONGO_OLDPROTOCOL}
  fNumberToReturn := NumberToReturn;
  fNumberToSkip := NumberToSkip;
  fQuery := TVarData(Query);
  fReturnFieldsSelector := TVarData(ReturnFieldsSelector);
  if TVarData(ReturnFieldsSelector).VType > varNull then
    BsonWriteParam(ReturnFieldsSelector);
end;

procedure TMongoRequestQuery.ToJson(W: TJsonWriter; Mode: TMongoJsonMode);
begin
  inherited;
  W.CancelLastChar('}');
  W.AddShort(',query:');
  AddMongoJson(variant(fQuery), W, modMongoShell);
  if fReturnFieldsSelector.VType <> varNull then
  begin
    W.AddShort(',projection:');
    AddMongoJson(variant(fReturnFieldsSelector), W, modMongoShell);
  end;
  if fNumberToReturn < maxInt then
  begin
    W.AddShort(',numberToReturn:');
    W.Add(fNumberToReturn);
  end;
  if fNumberToSkip > 0 then
  begin
    W.AddShort(',numberToSkip:');
    W.AddU(fNumberToSkip);
  end;
  W.Add('}');
end;


{ TMongoRequestGetMore }

constructor TMongoRequestGetMore.Create(const FullCollectionName: RawUtf8;
  NumberToReturn: integer; CursorID: Int64);
begin
  inherited Create(FullCollectionName, opGetMore, 0, 0);
  WriteCollectionName(0, FullCollectionName);
  Write4(NumberToReturn);
  Write8(@CursorID);
end;


{ TMongoRequestKillCursor }

constructor TMongoRequestKillCursor.Create(const FullCollectionName: RawUtf8;
  const CursorIDs: array of Int64);
var
  n: integer;
begin
  if high(CursorIDs) < 0 then
    raise EMongoException.CreateUtf8('Invalid %.Create([]) call', [self]);
  inherited Create(FullCollectionName, opKillCursors, 0, 0);
  Write4(0); // reserved for future use
  n := length(CursorIDs);
  Write4(n);
  SetLength(fCursors, n);
  n := n * SizeOf(Int64);
  MoveFast(CursorIDs[0], fCursors[0], n);
  Write(pointer(fCursors), n);
end;

procedure TMongoRequestKillCursor.ToJson(W: TJsonWriter; Mode: TMongoJsonMode);
var
  i: PtrInt;
begin
  inherited;
  W.CancelLastChar('}');
  W.AddShort(',cursorID:[');
  for i := 0 to high(fCursors) do
  begin
    W.Add(fCursors[i]);
    W.AddComma;
  end;
  W.CancelLastComma;
  W.Add(']', '}');
end;

{$else}


{ TMongoMsg }

constructor TMongoMsg.Create(const Database, Collection: RawUtf8;
  const Command: variant; Flags: TMongoMsgFlags; ToReturn: integer);
begin
  fNumberToReturn := ToReturn;
  // follow TMongoMsgHeader
  inherited Create(Database, Collection); // write TMongoWireHeader
  Write4(integer(Flags));
  Write1(ord(mmkBody)); // a single document follow
  if VarIsStr(Command) then
    fCommand := BsonVariant([Command, 1]) // as expected by hello command e.g.
  else
    fCommand := Command;
  BsonVariantType.AddItem(fCommand, ['$db', fDatabaseName]); // for OP_MSG
  //writeln('> ',fCommand);
  BsonWriteDoc(fCommand); // TBsonVariant or TDocVariant
end;

procedure TMongoMsg.ToJson(W: TJsonWriter; Mode: TMongoJsonMode);
begin
  inherited ToJson(W, Mode);
  W.CancelLastChar('}');
  W.AddShort(',command:');
  if AddMongoJson(fCommand, W, modMongoShell, 1024) then
    W.AddShorter('...') // huge Command has been truncated after 1KB
  else
    W.Add('}')
end;


{$endif MONGO_OLDPROTOCOL}


{ TMongoReplyCursor }

{$ifdef MONGO_OLDPROTOCOL}

procedure TMongoReplyCursor.Init(Request: TMongoRequest;
  const ReplyMessage: TMongoReply);
var
  Len: integer;
begin
  Len := length(ReplyMessage);
  with PMongoReplyHeader(ReplyMessage)^ do
  begin
    if (Len < SizeOf(TMongoReplyHeader)) or
       (Header.MessageLength <> Len) then
      raise EMongoException.CreateUtf8('TMongoReplyCursor.Init(len=%)', [Len]);
    if Header.OpCode <> WIRE_OPCODES[opReply] then
      raise EMongoException.CreateUtf8('TMongoReplyCursor.Init(OpCode=%)', [Header.OpCode]);
    fRequestID := requestID;
    fResponseTo := responseTo;
    byte(fResponseFlags) := ResponseFlags;
    fCursorID := CursorID;
    fStartingFrom := StartingFrom;
    fDocumentCount := NumberReturned;
  end;
  fRequest := Request;
  fReply := ReplyMessage;
  fFirstDocument := @PByteArray(pointer(fReply))[SizeOf(TMongoReplyHeader)];
  Rewind;
  fLatestDocIndex := -1;
end;

procedure TMongoReplyCursor.ComputeDocumentsList;
var
  i, Len: integer;
  P: PByte;
begin
  if (fDocumentsOffset <> nil) then
    exit;
  Len := length(fReply);
  SetLength(fDocumentsOffset, fDocumentCount);
  P := fFirstDocument;
  for i := 0 to fDocumentCount - 1 do
  begin
    fDocumentsOffset[i] := PtrUInt(P) - PtrUInt(fFirstDocument);
    inc(P, PInteger(P)^); // fast "parsing" of all supplied documents
    if PAnsiChar(P) - pointer(fReply) > Len then
      raise EMongoException.CreateUtf8('ComputeDocumentsList(Document[%])', [i]);
  end;
  if PAnsiChar(P) - pointer(fReply) <> Len then
    raise EMongoException.Create('ComputeDocumentsList(Documents)');
end;

{$else}

procedure TMongoReplyCursor.Init(Request: TMongoRequest;
  const ReplyMessage: TMongoReply);
var
  len: PtrInt;
begin
  len := length(ReplyMessage);
  with PMongoMsgHeader(ReplyMessage)^ do
  begin
    if (len < SizeOf(TMongoMsgHeader)) or
       (Header.MessageLength <> len) then
      raise EMongoException.CreateUtf8(
        'TMongoReplyCursor.Init(len=%)', [len]);
    if Header.OpCode <> OP_MSG then
      raise EMongoException.CreateUtf8(
        'TMongoReplyCursor.Init(OpCode=%)', [Header.OpCode]);
    if SectionKind <> mmkBody then
      raise EMongoException.CreateUtf8(
        'TMongoReplyCursor.Init(Kind=%)', [ord(SectionKind)]);
    fRequestID := requestID;
    fResponseTo := responseTo;
    fResponseFlags := ResponseFlags;
  end;
  fRequest := Request;
  fDocumentCount := 1; // as for mmkBody
  fCursorID := 0; // no need to call getMore
  fReply := ReplyMessage;
  fFirstDocument := @PByteArray(pointer(fReply))[SizeOf(TMongoMsgHeader)];
  Rewind;
  fLatestDocIndex := -1;
end;

procedure TMongoReplyCursor.ExtractBatch;
var
  len, cap: PtrInt;
  bson, batch: PByte;
  item: TBsonElement;
begin
  bson := pointer(fFirstDocument);
  BsonParseLength(bson);
  if item.FromSearch(bson, 'cursor') and
     (item.Kind = betDoc) then
  begin
    // handle find/getMore result with nested cursor.firstBach/nextBatch
    batch := nil;
    bson := item.Data.DocList;
    while item.FromNext(bson) do
      case item.NameLen of
        2:
          if PWord(item.Name)^ = ord('i') + ord('d') shl 8 then
            // fCursorID<>0 if getMore is needed
            fCursorID := item.ToInteger;
        9:
          if (item.Kind = betArray) and
             CompareMemFixed(item.Name, PAnsiChar('nextBatch'), 9) then
            // getMore command result is in cursor.nextBatch
            batch := item.Element;
        10:
          if (item.Kind = betArray) and
             CompareMemFixed(item.Name, PAnsiChar('firstBatch'), 10) then
            // find command result is in cursor.firstBatch
            batch := item.Element;
      end;
    if batch <> nil then
    begin
      // extract documents from firstBatch/nextBatch BSON array
      // (opQuery/opGetMore have no BSON array, but aggregated documents)
      BsonParseLength(batch);
      len := 0; // mimics ComputeDocumentsList
      cap := length(fDocumentsOffset); // reply instance may be reused
      while item.FromNext(batch) do
      begin
        if item.Kind <> betDoc then
          raise EMongoException.CreateUtf8(
            'TMongoReplyCursor.ExtractBatch(%)', [ToText(item.Kind)^]);
        if len = cap then
        begin
          if cap = 4 then
            cap := 72
          else
            cap := NextGrow(cap);
          SetLength(fDocumentsOffset, cap);
        end;
        if len = 0 then
          fFirstDocument := item.Element
        else
          fDocumentsOffset[len] := PtrUInt(item.Element) - PtrUInt(fFirstDocument);
        inc(len);
      end;
      fDocumentCount := len;
    end;
    Rewind;
    fLatestDocIndex := -1;
  end;
end;

{$endif MONGO_OLDPROTOCOL}

function TMongoReplyCursor.GetOneDocument(index: integer): variant;
begin
  if fLatestDocIndex <> index then
  begin
    // naive but efficient cache
    GetDocument(index, fLatestDocValue);
    fLatestDocIndex := index;
  end;
  result := fLatestDocValue;
end;

procedure TMongoReplyCursor.GetDocument(index: integer; var result: variant);
begin
  if cardinal(index) >= cardinal(fDocumentCount) then
    raise EMongoException.CreateUtf8('TMongoReplyCursor.GetDocument(index %>=%)',
      [index, fDocumentCount]);
  {$ifdef MONGO_OLDPROTOCOL}
  if fDocumentsOffset = nil then
    ComputeDocumentsList;
  BsonToDoc(pointer(PAnsiChar(fFirstDocument) + fDocumentsOffset[index]),
    result, 0, asDocVariantPerReference);
  {$else}
  BsonToDoc(fFirstDocument, result, 0, asDocVariantPerReference);
  {$endif MONGO_OLDPROTOCOL}
end;

function TMongoReplyCursor.Next(out doc: variant;
  option: TBsonDocArrayConversion): boolean;
var
  b: PByte;
begin
  if Next(b) then
  begin
    BsonToDoc(b, doc, 0, option);
    result := true;
  end
  else
    result := false;
end;

function TMongoReplyCursor.Next(out doc: PByte): boolean;
begin
  if fPosition < fDocumentCount then
  begin
    if fDocumentsOffset <> nil then
      // OP_MSG nested batch: BSON array has been parsed in Init()
      doc := pointer(PAnsiChar(fFirstDocument) + fDocumentsOffset[fPosition])
    else
    begin
      // opReply opQuery/opGetMore concatenation of documents parsing
      doc := fCurrent;
      inc(fCurrent, PInteger(fCurrent)^);
    end;
    inc(fPosition);
    result := true;
  end
  else
  begin
    doc := nil;
    result := false;
  end;
end;

function TMongoReplyCursor.Next(out Bson: TBsonDocument): boolean;
var
  b: PByte;
begin
  if Next(b) then
  begin
    FastSetRawByteString(Bson, PAnsiChar(b) + 4, PInteger(b)^);
    result := true;
  end
  else
    result := false;
end;

function TMongoReplyCursor.Next(out Json: RawUtf8; Mode: TMongoJsonMode): boolean;
var
  b: PByte;
begin
  if Next(b) then
  begin
    Json := BsonToJson(b, betDoc, 0, Mode);
    result := true;
  end
  else
    result := false;
end;

procedure TMongoReplyCursor.Rewind;
begin
  fPosition := 0;
  fCurrent := fFirstDocument;
end;

function TMongoReplyCursor.AppendAllToDocVariantDynArray(
  var Dest: TVariantDynArray): integer;
var
  b: PByte;
begin
  result := length(Dest);
  if (fReply = '') or
     (fDocumentCount <= 0) then
    exit; // nothing to append
  SetLength(Dest, result + fDocumentCount);
  Rewind;
  while Next(b) do
  begin
    BsonToDoc(b, Dest[result], 0, asDocVariantPerReference);
    inc(result);
  end;
  if result <> length(Dest) then
    raise EMongoException.Create('Invalid opReply Documents');
end;

procedure TMongoReplyCursor.AppendAllToBson(Dest: TBsonWriter);
var
  name: RawUtf8;
  b: PByte;
begin
  Rewind;
  while Next(b) do
  begin
    UInt32ToUtf8(Dest.Tag, name); // Dest.Tag = item number in array
    Dest.Tag := Dest.Tag + 1;
    Dest.BsonWrite(name, betDoc);
    Dest.Write(b, PInteger(b)^);
  end;
end;

function TMongoReplyCursor.AppendAllToDocVariant(var Dest: TDocVariantData): integer;
var
  item: variant;
begin
  if Dest.VarType <> DocVariantType.VarType then
    // may be called from getMore
    TDocVariant.NewFast(Variant(Dest), dvArray);
  result := Dest.Count;
  if (fReply = '') or
     (fDocumentCount <= 0) then
    exit; // nothing to append
  inc(result, fDocumentCount);
  Dest.Capacity := result;
  Rewind;
  while Next(item) do
    Dest.AddItem(item{%H-});
  if Dest.Count <> result then
    raise EMongoException.Create('Invalid opReply Documents');
end;

procedure TMongoReplyCursor.AppendAllAsDocVariant(var Dest: variant);
begin
  if (fReply <> '') and
     (fDocumentCount <> 0) then
    if (fDocumentCount = 1) and
       (fRequest.fNumberToReturn = 1) then
      BsonToDoc(fFirstDocument, Dest)
    else
      AppendAllToDocVariant(TDocVariantData(Dest))
    else
      VarClear(Dest);
end;

procedure TMongoReplyCursor.FetchAllToJson(W: TJsonWriter; Mode: TMongoJsonMode;
  WithHeader: boolean; MaxSize: PtrUInt);
var
  b: PByte;
begin
  if (fReply = '') or
     (fDocumentCount <= 0) then
  begin
    W.AddNull;
    exit;
  end;
  if WithHeader and
     (Mode = modMongoShell) then
    {$ifdef MONGO_OLDPROTOCOL}
    W.Add('{ReplyHeader:{ResponseFlags:%,RequestID:"%",ResponseTo:"%",' +
      'CursorID:%,StartingFrom:%,NumberReturned:%,ReplyDocuments:[',
      [byte(ResponseFlags), pointer(requestID), pointer(responseTo), CursorID,
       StartingFrom, fDocumentCount]);
    {$else}
    W.Add(
      '{ReplyHeader:{Flags:"%",RequestID:"%",ResponseTo:"%",Reply:',
      [ToHexShort(@ResponseFlags, SizeOf(ResponseFlags)),
       {%H-}pointer(requestID), {%H-}pointer(responseTo)]);
    {$endif MONGO_OLDPROTOCOL}
  Rewind;
  while Next(b) do
  begin
    inc(b, SizeOf(integer)); // points to the "e_list" of "int32 e_list #0"
    if BsonListToJson(b, betDoc, W, Mode, MaxSize) then
    begin
      W.AddShorter('...'); // truncated
      exit;
    end;
    W.AddComma;
    if (MaxSize <> 0) and
       (W.TextLength > MaxSize) then
      break;
  end;
  W.CancelLastComma;
  if WithHeader then
    W.Add({$ifdef MONGO_OLDPROTOCOL} ']', {$endif} '}');
end;

function TMongoReplyCursor.ToJson(Mode: TMongoJsonMode; WithHeader: boolean;
  MaxSize: PtrUInt): RawUtf8;
var
  W: TJsonWriter;
  tmp: TTextWriterStackBuffer;
begin
  if (fReply = '') or
     (fDocumentCount <= 0) then
    result := 'null'
  else
  begin
    W := TJsonWriter.CreateOwnedStream(tmp);
    try
      FetchAllToJson(W, Mode, WithHeader, MaxSize);
      W.SetText(result);
    finally
      W.Free;
    end;
  end;
end;



{ ************ MongoDB Client Classes }


{ TMongoConnection }

constructor TMongoConnection.Create(const aClient: TMongoClient;
  const aServerAddress: RawUtf8; aServerPort: integer);
begin
  if aClient = nil then
    raise EMongoException.CreateUtf8('%.Create(nil)', [self]);
  fClient := aClient;
  fServerAddress := TrimU(aServerAddress);
  if fServerAddress = '' then
    fServerAddress := '127.0.0.1';
  fServerPort := aServerPort;
  InitializeCriticalSection(fLock);
end;

destructor TMongoConnection.Destroy;
begin
  try
    try
      Close;
    except
      ; // continue on socket error
    end;
  finally
    DeleteCriticalSection(fLock);
    inherited Destroy;
  end;
end;

procedure TMongoConnection.Open;
begin
  if self = nil then
    raise EMongoException.Create('TMongoConnection(nil).Open');
  if fSocket <> nil then
    raise EMongoConnectionException.Create('Duplicate Open', self);
  try
    fSocket := TCrtSocket.Open(fServerAddress, UInt32ToUtf8(fServerPort),
      nlTcp, Client.ConnectionTimeOut, mcoTls in Client.Options,
      @Client.ConnectionTlsContext, @Client.ConnectionTunnel);
  except
    on E: Exception do
      raise EMongoException.CreateUtf8('%.Open unable to connect to MongoDB server %: % [%]',
        [self, Client.ConnectionString, E, E.Message]);
  end;
  fSocket.TcpNoDelay := true; // we buffer all output data before sending
  fSocket.KeepAlive := true;  // do not close the connection without notice
end;

function TMongoConnection.GetOpened: boolean;
begin
  result := (self <> nil) and
            (fSocket <> nil);
end;

procedure TMongoConnection.Close;
begin
  FreeAndNilSafe(fSocket);
end;

procedure TMongoConnection.GetDocumentsAndFree(Query: TMongoRequest;
  var result: TVariantDynArray);
begin
  result := nil;
  SendAndGetRepliesAndFree(Query, ReplyDocVariants, result);
end;

{$ifdef MONGO_OLDPROTOCOL}

procedure TMongoConnection.GetDocumentsAndFree(Query: TMongoRequest;
  var result: variant);
var
  ForceOneInstance: boolean;
  docs: TVariantDynArray;
begin
  ForceOneInstance := (Query as TMongoRequestQuery).NumberToReturn = 1;
  SetVariantNull(result);
  SendAndGetRepliesAndFree(Query, ReplyDocVariants, docs);
  if docs <> nil then
    if ForceOneInstance then
      result := docs[0]
    else
      TDocVariantData(result).InitArrayFromVariants(docs, JSON_FAST);
end;

{$else}

procedure TMongoConnection.GetDocumentsAndFree(Query: TMongoRequest;
  var result: variant);
begin
  // TMongoMsg asked for mmkBody with a single document section
  VarClear(result);
  SendAndGetRepliesAndFree(Query, ReplyDocVariant, result);
  //writeln('< ',result);
end;

{$endif MONGO_OLDPROTOCOL}

function TMongoConnection.GetDocumentsAndFree(Query: TMongoRequest): variant;
begin
  GetDocumentsAndFree(Query, result);
end;

function TMongoConnection.GetBsonAndFree(Query: TMongoRequest): TBsonDocument;
var
  W: TBsonWriter;
  tmp: TTextWriterStackBuffer;
begin
  W := TBsonWriter.Create(tmp{%H-});
  try
    W.BsonDocumentBegin;
    SendAndGetRepliesAndFree(Query, ReplyBson, W); // W.Tag = item num in array
    W.BsonDocumentEnd;
    W.ToBsonDocument(result);
  finally
    W.Free;
  end;
end;

function TMongoConnection.GetJsonAndFree(Query: TMongoRequest;
  Mode: TMongoJsonMode): RawUtf8;
var
  W: TJsonWriter;
  ReturnAsJsonArray: boolean;
  tmp: TTextWriterStackBuffer;
begin
  ReturnAsJsonArray := Query.NumberToReturn > 1; // set 1 to return an object
  W := TJsonWriter.CreateOwnedStream(tmp);
  try
    if ReturnAsJsonArray then
      W.Add('[');
    case Mode of
      modNoMongo:
        SendAndGetRepliesAndFree(Query, ReplyJsonNoMongo, W);
      modMongoStrict:
        SendAndGetRepliesAndFree(Query, ReplyJsonStrict, W);
      modMongoShell:
        SendAndGetRepliesAndFree(Query, ReplyJsonExtended, W);
    end;
    W.CancelLastComma;
    if ReturnAsJsonArray then
      W.Add(']');
    W.SetText(result);
    if (result = '') or
       (result = '[]') or
       (result = '{}') then
      result := 'null';
  finally
    W.Free;
  end;
  //writeln('< ',result);
end;

{$ifdef MONGO_OLDPROTOCOL}

procedure TMongoConnection.SendAndGetRepliesAndFree(Request: TMongoRequest;
  const OnEachReply: TOnMongoConnectionReply; var Opaque);
var
  query: TMongoRequestQuery;
  main: TMongoReplyCursor;
  more: TMongoReplyCursor;
  count: integer;
  getMore: TMongoRequestGetMore;
  cursorID: Int64;
begin
  try
    query := Request as TMongoRequestQuery;
    if not Assigned(OnEachReply) then
      raise EMongoRequestException.Create('OnEachReply=nil', self, query);
    count := query.NumberToReturn; // 0 means default return size
    SendAndGetCursor(query, main);
    if main.DocumentCount > 0 then
    begin
      OnEachReply(query, main, Opaque);
      if count > 0 then
        dec(count, main.DocumentCount);
    end;
    cursorID := main.CursorID;
    if cursorID <> 0 then
      if (query.NumberToReturn = 0) or
         ((query.NumberToReturn > 0) and
          (count > 0)) then
        repeat
          getMore := TMongoRequestGetMore.Create(
            query.FullCollectionName, count, cursorID);
          try
            SendAndGetCursor(getMore, more);
            if mrfCursorNotFound in more.ResponseFlags then
              raise EMongoRequestException.Create('GetMore cursor not found',
                self, query, more);
            if more.DocumentCount > 0 then
            begin
              OnEachReply(query, more, Opaque);
              dec(count, more.DocumentCount);
            end;
            cursorID := more.CursorID;
          finally
            getMore.Free;
          end;
        until ((query.NumberToReturn > 0) and
               (count <= 0)) or
              (cursorID = 0);
    if cursorID <> 0 then // if cursor not exhausted: need to kill it
      SendAndFree(TMongoRequestKillCursor.Create(
        query.FullCollectionName, [cursorID]), true);
  finally
    Request.Free;
  end;
end;

{$else}

procedure TMongoConnection.SendAndGetRepliesAndFree(Request: TMongoRequest;
  const OnEachReply: TOnMongoConnectionReply; var Opaque);
var
  msg: TMongoMsg;
  reply: TMongoReplyCursor;
begin
  try
    msg := Request as TMongoMsg;
    if not Assigned(OnEachReply) then
      raise EMongoRequestException.Create('OnEachReply=nil', self, Request);
    SendAndGetCursor(msg, reply);
    //writeln('<< ', reply.ToJson);
    if reply.DocumentCount > 0 then
      OnEachReply(msg, reply, Opaque);
    while reply.CursorID <> 0 do
    begin
      // https://www.mongodb.com/docs/manual/reference/command/getMore
      msg := TMongoMsg.Create(Request.DatabaseName, Request.CollectionName,
        BsonVariant(['getMore',    reply.CursorID,
                     'collection',  Request.CollectionName,
                     'batchSize',  Client.GetMoreBatchSize]),
        [], Request.NumberToReturn);
      try
        SendAndGetCursor(msg, reply);
        //writeln('<< ', reply.ToJson);
        if reply.DocumentCount > 0 then
          OnEachReply(msg, reply, Opaque);
      finally
        msg.Free;
      end;
    end;
  finally
    Request.Free;
  end;
end;

{$endif MONGO_OLDPROTOCOL}

procedure TMongoConnection.ReplyDocVariants(Request: TMongoRequest;
  const Reply: TMongoReplyCursor; var Opaque);
begin
  Reply.AppendAllToDocVariantDynArray(TVariantDynArray(Opaque));
end;

procedure TMongoConnection.ReplyDocVariant(Request: TMongoRequest;
  const Reply: TMongoReplyCursor; var Opaque);
begin
  Reply.AppendAllAsDocVariant(variant(Opaque));
end;

procedure TMongoConnection.ReplyJsonStrict(Request: TMongoRequest;
  const Reply: TMongoReplyCursor; var Opaque);
var
  W: TJsonWriter absolute Opaque;
begin
  Reply.FetchAllToJson(W, modMongoStrict, false);
  W.AddComma;
end;

procedure TMongoConnection.ReplyJsonExtended(Request: TMongoRequest;
  const Reply: TMongoReplyCursor; var Opaque);
var
  W: TJsonWriter absolute Opaque;
begin
  Reply.FetchAllToJson(W, modMongoShell, false);
  W.AddComma;
end;

procedure TMongoConnection.ReplyJsonNoMongo(Request: TMongoRequest;
  const Reply: TMongoReplyCursor; var Opaque);
var
  W: TJsonWriter absolute Opaque;
begin
  Reply.FetchAllToJson(W, modNoMongo, false);
  W.AddComma;
end;

procedure TMongoConnection.ReplyBson(Request: TMongoRequest;
  const Reply: TMongoReplyCursor; var Opaque);
begin
  Reply.AppendAllToBson(TBsonWriter(Opaque));
end;

function TMongoConnection.Send(Request: TMongoRequest): boolean;
var
  doc: TBsonDocument;
begin
  if not Opened and
     not Client.ReOpen then
    raise EMongoRequestException.Create('Send: Missing Open', self, Request);
  if Request = nil then
    raise EMongoRequestException.Create('Send(nil)', self);
  Request.ToBsonDocument(doc);
  if (Client.LogRequestEvent <> sllNone) and
     (Client.Log <> nil) and
     (Client.LogRequestEvent in Client.Log.Family.Level) then
    Client.Log.Log(Client.fLogRequestEvent, Request.ToJson(modMongoShell), Request);
  result := fSocket.TrySndLow(pointer(doc), length(doc));
end;

{$ifdef MONGO_OLDPROTOCOL}
function TMongoConnection.SendAndFree(Request: TMongoRequest;
  NoAcknowledge: boolean): variant;
var
  cmd: variant;
begin
  SetVariantNull(result);
  try
    if self = nil then
      raise EMongoRequestException.Create('Connection=nil', self, Request);
    Lock;
    try
      if Send(Request) then
      begin
        if NoAcknowledge or
           (Client.WriteConcern in [wcErrorsIgnored, wcUnacknowledged]) then
          exit;
        case Client.WriteConcern of
          wcAcknowledged:
            cmd := 'getLastError';
          wcJournaled:
            cmd := BsonVariant([
              'getLastError', 1,
              'j', true]);
          wcReplicaAcknowledged:
            cmd := BsonVariant([
              'getLastError', 1,
              'w', 2]);
        else
          raise EMongoRequestException.CreateUtf8('%.SendAndFree WriteConcern=%',
            [self, ord(Client.WriteConcern)], self, Request);
        end;
        RunCommand(Request.DatabaseName, cmd, result);
        if not VarIsNull(result.err) then
          raise EMongoRequestException.Create('SendAndFree', self, Request,
            TDocVariantData(result));
      end
      else
        // socket error on sending
        if Client.WriteConcern = wcErrorsIgnored then
          exit
        else
          raise EMongoRequestOSException.Create('SendAndFree', self, Request);
    finally
      UnLock;
    end;
  finally
    Request.Free;
  end;
end;
{$endif MONGO_OLDPROTOCOL}

procedure TMongoConnection.SendAndGetCursor(Request: TMongoRequest;
  var Result: TMongoReplyCursor);
var
  reply: TMongoReply;
begin
  SendAndGetReply(Request, reply);
  Result.Init(Request, reply);
  if (Client.LogReplyEvent <> sllNone) and
     (Client.Log <> nil) and
     (Client.LogReplyEvent in Client.Log.Family.Level) then
    Client.Log.Log(Client.LogReplyEvent,
      Result.ToJson(modMongoShell, True, Client.LogReplyEventMaxSize), Request);
  {$ifdef MONGO_OLDPROTOCOL}
  if mrfQueryFailure in Result.ResponseFlags then
    raise EMongoRequestException.Create('Query failure', self, Request, Result);
  {$else}
  if Request.CollectionName <> '' then
    Result.ExtractBatch; // from "find", "aggregate" or "getMore" commands
  {$endif MONGO_OLDPROTOCOL}
end;

const
  RECV_ERROR =
    '%.SendAndGetReply(%): Server response timeout or connection broken, ' +
    'probably due to a bad formatted BSON request -> close socket';

procedure TMongoConnection.SendAndGetReply(
  Request: TMongoRequest; out result: TMongoReply);
var
  Header: TMongoWireHeader;
  HeaderLen, DataLen: integer;
begin
  if self = nil then
    raise EMongoRequestException.Create('Connection=nil', self, Request);
  FillCharFast(Header, SizeOf(Header), 0);
  Lock;
  try
    if Send(Request) then
    begin
      HeaderLen := SizeOf(Header);
      if not fSocket.TrySockRecv(@Header, HeaderLen) then
        try
          Close;
        finally
          raise EMongoRequestException.CreateUtf8(
            RECV_ERROR, [self, 'hdr'], self, Request);
        end;
      SetLength(result, Header.MessageLength);
      PMongoWireHeader(result)^ := Header;
      DataLen := Header.MessageLength - SizeOf(Header);
      if not fSocket.TrySockRecv(@PByteArray(result)[SizeOf(Header)], DataLen) then
        try
          Close;
        finally
          raise EMongoRequestException.CreateUtf8(
            RECV_ERROR, [self, 'msg'], self, Request);
        end;
      if Header.ResponseTo = Request.MongoRequestID then
        exit; // success
      {$ifdef MONGO_OLDPROTOCOL}
      if Header.OpCode = WIRE_OPCODES[opMsgOld] then
      begin
        if Client.Log <> nil then
          Client.Log.Log(sllWarning, 'Msg (deprecated) from MongoDB: %',
            [BsonToJson(@PByteArray(result)[SizeOf(Header)], betDoc, DataLen,
             modMongoShell)], Request);
      end
      else if Header.OpCode = WIRE_OPCODES[opMsg] then
        if Client.Log <> nil then
          Client.Log.Log(sllWarning, 'Msg from MongoDB: %',
            [EscapeToShort(@PByteArray(result)[SizeOf(Header)], DataLen)], Request);
      {$endif MONGO_OLDPROTOCOL}
    end;
    // if we reached here, this is due to a socket error or an unexpeted opcode
    raise EMongoRequestException.CreateUtf8(
      '%.SendAndGetReply: OpCode=% and ResponseTo=% (expected:%)',
      [self, Header.OpCode, Header.ResponseTo, Request.MongoRequestID], self, Request);
  finally
    UnLock;
  end;
end;

function NewCommand(const db: RawUtf8; const command: variant;
  flags: TMongoQueryFlags; const aCollectionName: RawUtf8): TMongoRequest;
begin
  {$ifdef MONGO_OLDPROTOCOL}
  result := TMongoRequestQuery.Create(db + '.$cmd', command, null, 1, 0, flags);
  {$else}
  result := TMongoMsg.Create(db, aCollectionName, command, flags, 1);
  {$endif MONGO_OLDPROTOCOL}
end;

function TMongoConnection.RunCommand(const aDatabaseName: RawUtf8;
  const command: variant; var returnedValue: variant;
  flags: TMongoQueryFlags; const aCollectionName: RawUtf8): RawUtf8;
begin
  GetDocumentsAndFree(
    NewCommand(aDatabaseName, command, flags, aCollectionName), returnedvalue);
  with _Safe(returnedValue)^ do
    if GetValueOrDefault('ok', 1) <> 0 then
      result := ''
    else if not GetAsRawUtf8('errmsg', result) then
      result := 'unspecified error';
end;

function TMongoConnection.RunCommand(const aDatabaseName: RawUtf8;
  const command: variant; var returnedValue: TBsonDocument;
  flags: TMongoQueryFlags; const aCollectionName: RawUtf8): boolean;
var
  item: TBsonElement;
begin
  returnedValue := GetBsonAndFree(
    NewCommand(aDatabaseName, command, flags, aCollectionName));
  result := true;
  item.FromDocument(returnedValue);
  if item.DocItemToInteger('ok', 1) = 0 then
    result := false;
end;

procedure TMongoConnection.Lock;
begin
  EnterCriticalSection(fLock);
  inc(fLocked);
end;

procedure TMongoConnection.UnLock;
begin
  dec(fLocked);
  LeaveCriticalSection(fLock);
end;

function TMongoConnection.GetLocked: boolean;
begin
  result := (self <> nil) and
            (fLocked > 0);
end;


{ EMongoConnectionException }

constructor EMongoConnectionException.Create(const aMsg: string;
  aConnection: TMongoConnection);
begin
  inherited Create(aMsg);
  fConnection := aConnection;
end;

constructor EMongoConnectionException.CreateUtf8(const Format: RawUtf8;
  const Args: array of const; aConnection: TMongoConnection);
begin
  inherited CreateUtf8(Format, Args);
  fConnection := aConnection;
end;


{ EMongoRequestException }

constructor EMongoRequestException.Create(const aMsg: string;
  aConnection: TMongoConnection; aRequest: TMongoRequest);
begin
  inherited Create(aMsg, aConnection);
  fRequest := aRequest;
end;

constructor EMongoRequestException.CreateUtf8(const Format: RawUtf8;
  const Args: array of const; aConnection: TMongoConnection; aRequest: TMongoRequest);
begin
  inherited CreateUtf8(Format, Args, aConnection);
  fRequest := aRequest;
end;

constructor EMongoRequestException.Create(const aMsg: string;
  aConnection: TMongoConnection; aRequest: TMongoRequest; const aError: TMongoReplyCursor);
begin
  Create(aMsg, aConnection, aRequest);
  fError := aError;
end;

constructor EMongoRequestException.Create(const aMsg: string;
  aConnection: TMongoConnection; aRequest: TMongoRequest; const aErrorDoc: TDocVariantData);
begin
  Create(aMsg, aConnection, aRequest);
  fErrorDoc := variant(aErrorDoc);
end;

{$ifndef NOEXCEPTIONINTERCEPT}
function EMongoRequestException.CustomLog(WR: TTextWriter;
  const Context: TSynLogExceptionContext): boolean;
begin
  inherited CustomLog(WR, Context);
  if fRequest <> nil then
  begin
    WR.AddInstanceName(fRequest, ':');
    if WR.InheritsFrom(TJsonWriter) then
      fRequest.ToJson(TJsonWriter(WR), modMongoShell)
    else
      WR.AddNull;
  end;
  if (fError.fReply <> '') and
     WR.InheritsFrom(TJsonWriter) then
    fError.FetchAllToJson(TJsonWriter(WR), modMongoShell, True);
  result := false; // log stack trace
end;
{$endif NOEXCEPTIONINTERCEPT}

function EMongoRequestException.GetErrorDoc: variant;
begin
  if TVarData(fErrorDoc).VType = varEmpty then
  begin
    if not fError.Next(fErrorDoc) then
      SetVariantNull(fErrorDoc);
  end;
  result := fErrorDoc;
end;


{ EMongoRequestOSException }

constructor EMongoRequestOSException.Create(const aMsg: string;
  aConnection: TMongoConnection; aRequest: TMongoRequest);
begin
  fSystemLastError := GetLastError;
  CreateUtf8('%: % (%)', [aMsg, ToUtf8(SysErrorMessage(fSystemLastError)),
    fSystemLastError], aConnection, aRequest);
end;


{ EMongoDatabaseException }

constructor EMongoDatabaseException.Create(const aMsg: string;
  aDatabase: TMongoDatabase);
begin
  inherited Create(aMsg, aDatabase.Client.Connections[0]);
  fDatabase := aDatabase;
end;

constructor EMongoDatabaseException.CreateUtf8(const Format: RawUtf8;
  const Args: array of const; aDatabase: TMongoDatabase);
begin
  inherited CreateUtf8(Format, Args, aDatabase.Client.Connections[0]);
  fDatabase := aDatabase;
end;

{$ifndef NOEXCEPTIONINTERCEPT}
function EMongoDatabaseException.CustomLog(WR: TTextWriter;
  const Context: TSynLogExceptionContext): boolean;
begin
  inherited CustomLog(WR, Context);
  if WR.InheritsFrom(TJsonWriter) then
    TJsonWriter(WR).AddJsonEscape(['Database', fDatabase.Name]);
  result := false; // log stack trace
end;
{$endif NOEXCEPTIONINTERCEPT}


{ TMongoClient }

constructor TMongoClient.Create(const Host: RawUtf8; Port: integer;
  aOptions: TMongoClientOptions; const SecondaryHostCsv, SecondaryPortCsv: RawUtf8);
const
  PROT: array[boolean] of string[1] = (
    '', 's');
var
  secHost: TRawUtf8DynArray;
  secPort: TIntegerDynArray;
  nHost, i: PtrInt;
begin
  fConnectionTimeOut := 30000;
  fOptions := aOptions;
  fFindBatchSize := 65536; // 65536 documents per find/getMore batch
  fGetMoreBatchSize := fFindBatchSize;
  // overriden by "hello" command just after connection
  fServerMaxBsonObjectSize := 16777216;
  fServerMaxMessageSizeBytes := 48000000;
  fServerMaxWriteBatchSize := 100000;
  fLogReplyEventMaxSize := 1024;
  fGracefulReconnect.Enabled := true;
  FormatUtf8('mongodb%://%:%', [PROT[mcoTls in Options], Host, Port],
    fConnectionString);
  CsvToRawUtf8DynArray(pointer(SecondaryHostCsv), secHost);
  nHost := length(secHost);
  SetLength(fConnections, nHost + 1);
  fConnections[0] := TMongoConnection.Create(self, Host, Port);
  if nHost > 0 then
  begin
    CsvToIntegerDynArray(pointer(SecondaryPortCsv), secPort);
    for i := 0 to nHost - 1 do
    begin
      if i > high(secPort) then
        Port := MONGODB_DEFAULTPORT
      else
        Port := secPort[i];
      fConnections[i + 1] := TMongoConnection.Create(self, secHost[i], Port);
      fConnectionString :=
        FormatUtf8('%,%:%', [fConnectionString, secHost[i], Port]);
    end;
  end;
  fDatabases := TRawUtf8List.CreateEx([fObjectsOwned, fNoDuplicate, fCaseSensitive]);
end;

destructor TMongoClient.Destroy;
var
  i: PtrInt;
begin
  for i := 0 to high(fConnections) do
    FreeAndNilSafe(fConnections[i]);
  FreeAndNilSafe(fDatabases);
  inherited;
end;

procedure TMongoClient.SetLog(LogClass: TSynLogClass; RequestEvent, ReplyEvent:
  TSynLogInfo; ReplyEventMaxSize: cardinal);
begin
  fLog := LogClass.Add;
  LogRequestEvent := RequestEvent;
  LogReplyEvent := ReplyEvent;
  LogReplyEventMaxSize := ReplyEventMaxSize;
end;

function TMongoClient.ServerBuildInfoText: RawUtf8;
begin
  with _Safe(ServerBuildInfo)^ do
    if count = 0 then
      result := ''
    else
    begin
      FormatUtf8('MongoDB % %', [U['version'], U['javascriptEngine']], result);
      with A['storageEngines']^ do
      begin
        // "storageEngines":["devnull","ephemeralForTest","mmapv1","wiredTiger"]
        DeleteByValue('devnull');
        DeleteByValue('ephemeralForTest');
        if count > 0 then
          result := result + ' ' + ToCsv;
      end;
      {$ifdef MONGO_OLDPROTOCOL}
      result := result + ' oldOP'; // it is time to upgrade for sure
      {$endif MONGO_OLDPROTOCOL}
    end;
end;

function TMongoClient.GetOneReadConnection: TMongoConnection;

  function GetUnlockedSecondaryIndex: PtrInt;
  var
    retry: integer;
  begin
    if Length(fConnections) = 1 then // no secondary? use primary
      result := 0
    else
    begin
      for retry := 1 to 100 do
      begin
        // search for an inactive connection
        result := fLatestReadConnectionIndex; // simple round-robin pattern
        if result = high(fConnections) then
          if ReadPreference = rpSecondary then
            result := 1
          else
            result := 0
        else
          inc(result); // thread-safety is not an issue here
        if (retry <= length(fConnections)) and
           not fConnections[result].Opened then
        try
          fConnections[result].Open;
        except
          on E: Exception do
          begin
            SleepHiRes(2);
            continue;
          end;
        end;
        if fConnections[result].Opened then
          if fConnections[result].Locked then
            if retry mod length(fConnections) = 0 then
              SleepHiRes(2)
            else
              continue
          else
            break;
      end;
      if not fConnections[result].Opened then
        result := 0; // safe fallback to primary member in worst case
      fLatestReadConnectionIndex := result;
    end;
  end;

var
  n, retry: integer;
begin
  case ReadPreference of
    rpPrimaryPreferred:
      if fConnections[0].Locked then
        result := fConnections[GetUnlockedSecondaryIndex]
      else
        result := fConnections[0];
    rpSecondary,
    rpSecondaryPreferred:
      result := fConnections[GetUnlockedSecondaryIndex];
    rpNearest:
      begin
        n := Length(fConnections);
        for retry := 1 to n * 2 do
        begin
          result := fConnections[Random32(n)];
          if not result.Locked then
            exit;
        end;
        // falback to the main instance
        result := fConnections[0];
      end;
  else
    // rpPrimary or not handled yet
    result := fConnections[0];
  end;
end;

function TMongoClient.Open(const DatabaseName: RawUtf8): TMongoDatabase;
begin
  if self = nil then
    result := nil
  else
  begin
    result := fDatabases.GetObjectFrom(DatabaseName);
    if result = nil then
    begin
      // not already opened -> try now from primary host
      if not fConnections[0].Opened then
      begin
        fConnections[0].Open;
        AfterOpen(0);
      end;
      result := TMongoDatabase.Create(Self, DatabaseName);
      fDatabases.AddObjectUnique(DatabaseName, @result);
    end;
  end;
end;

function PasswordDigest(const UserName, Password: RawUtf8): RawUtf8;
begin
  result := Md5(UserName + ':mongo:' + Password);
end;

function TMongoClient.OpenAuth(const DatabaseName, UserName, PassWord: RawUtf8;
  ForceMongoDBCR: boolean): TMongoDatabase;
var
  digest: RawByteString;
  i: PtrInt;
begin
  if (self = nil) or
     (DatabaseName = '') or
     (UserName = '') or
     (PassWord = '') then
    raise EMongoException.CreateUtf8('Invalid %.OpenAuth("%") call',
      [self, DatabaseName]);
  result := fDatabases.GetObjectFrom(DatabaseName);
  if result = nil then  // not already opened -> try now from primary host
  try
    // ensure we are opened and authenticated on all connections
    for i := 0 to High(fConnections) do
      if not fConnections[i].Opened then
        try
          fConnections[i].Open; // open socket connection
          AfterOpen(i); // need ServerBuildInfoNumber just below
          digest := PasswordDigest(UserName, Password);
          Auth(DatabaseName, UserName, digest, ForceMongoDBCR, i);
          with fGracefulReconnect do
            if Enabled and
               (EncryptedDigest='') then
            begin
              ForcedDBCR := ForceMongoDBCR;
              User := UserName;
              Database := DatabaseName;
              EncryptedDigest := CryptDataForCurrentUser(digest, Database, true);
            end;
        except
          fConnections[i].Close;
          raise;
        end;
    result := TMongoDatabase.Create(Self, DatabaseName);
    fDatabases.AddObjectUnique(DatabaseName, @result);
  finally
    FillZero(digest);
  end;
end;

procedure TMongoClient.Auth(const DatabaseName, UserName, Digest: RawUtf8;
  ForceMongoDBCR: boolean; ConnectionIndex: PtrInt);
var
  res, bson: variant;
  err, nonce, first, key, user, msg, rnonce: RawUtf8;
  payload: RawByteString;
  rnd: TAesBlock;
  sha: TSha1;
  salted, client, stored, server: TSha1Digest;
  resp: TDocVariantData;

  procedure CheckPayload;
  var
    bin: PVariant;
  begin
    if err <> '' then
      exit;
    if _Safe(res)^.GetAsPVariant('payload', bin) and
       BsonVariantType.ToBlob({%H-}bin^, payload) then
      resp.InitCsv(pointer(payload), JSON_FAST, '=', ',')
    else
      err := 'missing or invalid returned payload';
  end;

begin
  // note: caller should have made fConnections[ConnectionIndex].Open
  if (self = nil) or
     (DatabaseName = '') or
     (UserName = '') or
     (Digest = '') then
    raise EMongoException.CreateUtf8('Invalid %.Auth("%") call',
      [self, DatabaseName]);
  if ForceMongoDBCR or
     (ServerBuildInfoNumber < 03000000) then
  begin
    // MONGODB-CR
    // http://docs.mongodb.org/meta-driver/latest/legacy/implement-authentication-in-driver
    bson := BsonVariant([
      'getnonce', 1]);
    err := fConnections[ConnectionIndex].RunCommand(DatabaseName, bson, res);
    if (err = '') and
       not _Safe(res)^.GetAsRawUtf8('nonce', nonce) then
      err := 'missing returned nonce';
    if err <> '' then
      raise EMongoException.CreateUtf8('%.OpenAuthCR("%") step1: % - res=%',
        [self, DatabaseName, err, res]);
    key := Md5(nonce + UserName + Digest);
    bson := BsonVariant([
      'authenticate', 1,
      'user', UserName,
      'nonce', nonce,
      'key', key]);
    err := fConnections[ConnectionIndex].RunCommand(DatabaseName, bson, res);
    if err <> '' then
      raise EMongoException.CreateUtf8('%.OpenAuthCR("%") step2: % - res=%',
        [self, DatabaseName, err, res]);
  end
  else
  begin
    // SCRAM-SHA-1
    // https://tools.ietf.org/html/rfc5802#section-5
    user := StringReplaceAll(UserName, ['=', '=3D', ',', '=2C']);
    TAesPrng.Main.FillRandom(rnd);
    nonce := BinToBase64(@rnd, SizeOf(rnd));
    FormatUtf8('n=%,r=%', [user, nonce], first);
    BsonVariantType.FromBinary('n,,' + first, bbtGeneric, bson);
    err := fConnections[ConnectionIndex].RunCommand(DatabaseName,
      BsonVariant([
        'saslStart', 1,
        'mechanism', 'SCRAM-SHA-1',
        'payload', bson,
        'autoAuthorize', 1
        ]), res);
    CheckPayload;
    if err = '' then
    begin
      rnonce := resp.U['r'];
      if copy(rnonce, 1, length(nonce)) <> nonce then
        err := 'returned invalid nonce';
    end;
    if err <> '' then
      raise EMongoException.CreateUtf8('%.OpenAuthSCRAM("%") step1: % - res=%',
        [self, DatabaseName, err, res]);
    key := 'c=biws,r=' {%H-}+ rnonce;
    Pbkdf2HmacSha1(Digest, Base64ToBin(resp.U['s']),
      Utf8ToInteger(resp.U['i']), salted);
    HmacSha1(salted, 'Client Key', client);
    sha.Full(@client, SizeOf(client), stored);
    msg := first + ',' + RawUtf8({%H-}payload) + ',' + key;
    HmacSha1(stored, msg, stored);
    XorMemory(@client, @stored, SizeOf(client));
    HmacSha1(salted, 'Server Key', server);
    HmacSha1(server, msg, server);
    msg := key + ',p=' + BinToBase64(@client, SizeOf(client));
    BsonVariantType.FromBinary(msg, bbtGeneric, bson);
    err := fConnections[ConnectionIndex].RunCommand(
      DatabaseName, BsonVariant([
        'saslContinue', 1,
        'conversationId', res.conversationId,
        'payload', bson
        ]), res);
    resp.Clear;
    CheckPayload;
    if (err = '') and
       (resp.U['v'] <> BinToBase64(@server, SizeOf(server))) then
      err := 'Server returned an invalid signature';
    if err <> '' then
      raise EMongoException.CreateUtf8('%.OpenAuthSCRAM("%") step2: % - res=%',
        [self, DatabaseName, err, res]);
    if not res.done then
    begin
      // third empty challenge may be required
      err := fConnections[ConnectionIndex].RunCommand(
        DatabaseName, BsonVariant([
           'saslContinue', 1,
           'conversationId', res.conversationId,
           'payload', ''
           ]), res);
      if (err = '') and
         not res.done then
        err := 'SASL conversation failed to complete';
      if err <> '' then
        raise EMongoException.CreateUtf8('%.OpenAuthSCRAM("%") step3: % - res=%',
          [self, DatabaseName, err, res]);
    end;
  end;
end;

procedure TMongoClient.AfterOpen;
var
  w: integer;
begin
  if VarIsEmptyOrNull(fServerBuildInfo) then
  begin
    fConnections[0].RunCommand('admin', 'buildinfo', fServerBuildInfo);
    with _Safe(fServerBuildInfo)^.A['versionArray']^ do
      if count = 4 then
        fServerBuildInfoNumber := // e.g. 02040900 for MongoDB 2.4.9
          integer(Values[0]) * 1000000 + integer(Values[1]) * 10000 +
          integer(Values[2]) * 100 +     integer(Values[3]);
  end;
  if VarIsEmptyOrNull(fServerInfo) then
  begin
// https://github.com/mongodb/specifications/blob/master/source/mongodb-handshake
    fConnections[0].RunCommand('admin',
      BsonVariant(['hello', 1,
                   'client',
                     '{',
                         'application',
                         '{',
                             'name',    Executable.ProgramName,
                         '}',
                         'driver',
                         '{',
                             'name',    SYNOPSE_FRAMEWORK_NAME,
                             'version', SYNOPSE_FRAMEWORK_VERSION,
                         '}',
                         'os',
                         '{',
                             'type', OS_TEXT,
                             'name', OSVersionShort,
                             'architecture', CPU_ARCH_TEXT,
                         '}',
                     '}']),fServerInfo);
    with _Safe(fServerInfo, dvObject)^ do
      if Count <> 0 then
      begin
        GetAsInteger('maxBsonObjectSize', fServerMaxBsonObjectSize);
        GetAsInteger('maxMessageSizeBytes', fServerMaxMessageSizeBytes);
        GetAsInteger('maxWriteBatchSize', fServerMaxWriteBatchSize);
        GetAsInteger('maxWireVersion', w);
        byte(fServerMaxWireVersion) := w;
        GetAsBoolean('readOnly', fServerReadOnly);
      end;
  end;
end;

function TMongoClient.ReOpen: boolean;
var
  digest: RawByteString;
  {%H-}log: ISynLog;
begin
  result := false;
  with fGracefulReconnect do
    if Enabled then
    try
      if fLog <> nil then
        log := fLog.Enter(self, 'ReOpen: graceful reconnect');
      fConnections[0].Open;
      if EncryptedDigest <> '' then
      try
        digest := CryptDataForCurrentUser(EncryptedDigest, Database, false);
        Auth(Database, user, digest, ForcedDBCR, 0);
      finally
        FillZero(digest);
      end;
      result := true;
    except
      fConnections[0].Close;
      raise;
    end;
end;

function TMongoClient.GetBytesReceived: Int64;
var
  i: PtrInt;
begin
  result := 0;
  if self <> nil then
    for i := 0 to high(fConnections) do
      with fConnections[i] do
        if fSocket <> nil then
          inc(result, fSocket.BytesIn);
end;

function TMongoClient.GetBytesSent: Int64;
var
  i: PtrInt;
begin
  result := 0;
  if self <> nil then
    for i := 0 to high(fConnections) do
      with fConnections[i] do
        if fSocket <> nil then
          inc(result, fSocket.BytesOut);
end;

function TMongoClient.GetBytesTransmitted: Int64;
var
  i: PtrInt;
begin
  result := 0;
  if self <> nil then
    for i := 0 to high(fConnections) do
      with fConnections[i] do
        if fSocket <> nil then
          inc(result, fSocket.BytesIn + fSocket.BytesOut);
end;


{ TMongoDatabase }

constructor TMongoDatabase.Create(aClient: TMongoClient; const aDatabaseName: RawUtf8);
var
  {$ifdef MONGO_OLDPROTOCOL}
  colls: TBsonIterator;
  full, db: RawUtf8;
  {$endif MONGO_OLDPROTOCOL}
  cmd, resp, batch: variant;
  ndx: PtrInt;
  coll: RawUtf8;
  mc: TMongoCollection;
begin
  fClient := aClient;
  fName := aDatabaseName;
  fCollections := TRawUtf8List.CreateEx(
    [fObjectsOwned, fNoDuplicate, fCaseSensitive]);
  {$ifdef MONGO_OLDPROTOCOL}
  if fClient.ServerBuildInfoNumber < 03000000 then
  begin
    if colls.Init(client.Connections[0].GetBsonAndFree(TMongoRequestQuery.Create(
       aDatabaseName + '.system.namespaces', null, 'name', maxInt))) then
      // e.g. [ {name:"test.system.indexes"}, {name:"test.test"} ]
      while colls.Next do
      begin
        full := colls.item.DocItemToRawUtf8('name');
        if full <> '' then
        begin
          split(full, '.', db, coll);
          if db <> aDatabaseName then
            raise EMongoConnectionException.CreateUtf8(
              '%.Create: invalid [%] collection name for DB [%]',
              [self, full, aDatabaseName], client.Connections[0]);
          mc := TMongoCollection.Create(self, coll);
          fCollections.AddObjectUnique(coll, @mc);
        end;
      end;
  end
  else
  {$endif MONGO_OLDPROTOCOL}
  begin
    if fClient.ServerBuildInfoNumber >= 04000000 then
      cmd := BsonVariant(['listCollections', 1,
                          'nameOnly', VarTrue,
                          'authorizedCollections', VarTrue])
    else
      cmd := 'listCollections';
    RunCommand(cmd, resp);
    if _Safe(resp)^.GetValueByPath('cursor.firstBatch', batch) then
      with _Safe(batch)^ do
        for ndx := 0 to count - 1 do
          if _Safe(Values[ndx]).GetAsRawUtf8('name', coll) then
          begin
            mc := TMongoCollection.Create(self, coll);
            fCollections.AddObjectUnique(coll, @mc);
          end;
  end;
end;

destructor TMongoDatabase.Destroy;
begin
  FreeAndNilSafe(fCollections);
  inherited;
end;

function TMongoDatabase.CreateUser(const UserName, Password: RawUtf8;
  const roles: variant): RawUtf8;
var
  res: variant;
  usr: TDocVariantData;
begin
  usr.InitObject([
    'createUser',     UserName,
    'pwd',            PasswordDigest(UserName, Password),
    'digestPassword', false,
    'roles',          roles], JSON_FAST);
  // note: passwordDigestor:"client" fails
  if client.ServerBuildInfoNumber >= 04000000 then
    usr.AddValue('mechanisms', _ArrFast(['SCRAM-SHA-1']));
  result := RunCommand(variant(usr), res);
end;

function TMongoDatabase.CreateUserForThisDatabase(
  const UserName, Password: RawUtf8; allowWrite: boolean): RawUtf8;
const
  RW: array[boolean] of RawUtf8 = (
    'read', 'readWrite');
begin
  result := CreateUser(UserName, Password,
    BsonVariant('[{role:?,db:?}]', [], [RW[allowWrite], name]));
end;

function TMongoDatabase.DropUser(const UserName: RawUtf8): RawUtf8;
var
  res: variant;
begin
  result := RunCommand(BsonVariant(['dropUser', UserName]), res);
end;

function TMongoDatabase.GetCollection(const Name: RawUtf8): TMongoCollection;
begin
  result := GetCollectionOrNil(Name);
  if result = nil then
    raise EMongoDatabaseException.CreateUtf8('%.GetCollection("%")', [self, Name], self);
end;

function TMongoDatabase.GetCollectionOrCreate(const Name: RawUtf8): TMongoCollection;
begin
  result := GetCollectionOrNil(Name);
  if result = nil then
    if self <> nil then
    begin
      result := TMongoCollection.Create(self, Name);
      fCollections.AddObjectUnique(Name, @result);
    end;
end;

function TMongoDatabase.GetCollectionOrNil(const Name: RawUtf8): TMongoCollection;
begin
  if self = nil then
    result := nil
  else
    result := fCollections.GetObjectFrom(Name);
end;

function TMongoDatabase.RunCommand(const command: variant;
  var returnedValue: variant; const collName: RawUtf8): RawUtf8;
begin
  result := client.Connections[0].RunCommand(
    name, command, returnedValue, [], collName);
end;

function TMongoDatabase.RunCommand(const command: variant;
  var returnedValue: TBsonDocument; const collName: RawUtf8): boolean;
begin
  result := client.Connections[0].RunCommand(
    name, command, returnedValue, [], collName);
end;


{ TMongoCollection }

constructor TMongoCollection.Create(aDatabase: TMongoDatabase;
  const aCollectionName: RawUtf8);
begin
  fDatabase := aDatabase;
  fName := aCollectionName;
  fFullCollectionName := fDatabase.Name + '.' + fName;
end;

function TMongoCollection.AggregateCallFromJson(const pipelineJson: RawUtf8;
  var reply, res: variant): boolean;
begin
  result := AggregateCallFromVariant(BsonVariant(pipelineJson), reply, res);
end;

function TMongoCollection.AggregateDoc(const Operators: RawUtf8;
  const Params: array of const): variant;
begin
  result := AggregateDocFromJson(FormatUtf8(Operators, Params));
end;

function TMongoCollection.AggregateJson(const Operators: RawUtf8;
  const Params: array of const; Mode: TMongoJsonMode): RawUtf8;
begin
  result := AggregateJsonFromJson(FormatUtf8(Operators, Params), Mode);
end;

function TMongoCollection.AggregateCallFromVariant(const pipelineArray: variant;
  var reply, res: variant): boolean;
var
  cmd: variant;
begin
  // see http://docs.mongodb.org/manual/reference/command/aggregate
  if fDatabase.Client.ServerBuildInfoNumber < 2020000 then
    raise EMongoException.Create('Aggregation needs MongoDB 2.2 or later');
  if fDatabase.Client.ServerBuildInfoNumber >= 3060000 then
  begin
    // since 3.6, the cursor:{} parameter is mandatory, even if void
    // and the returned resultset has the new firstBatch/nextBatch layout
    cmd := BsonVariant([
      'aggregate', fName,
      'pipeline',  pipelineArray,
      'cursor',    '{', 'batchSize', fDatabase.Client.FindBatchSize, '}']);
    // {"cursor":{"firstBatch":[{"_id":null,"max":1510}],"id":0,"ns":"db.test"},"ok":1}
    {$ifdef MONGO_OLDPROTOCOL}
    Database.RunCommand(cmd, reply);
    res := reply.cursor;
    if not VarIsNull(res) then
      res := res.firstBatch;
    {$else}
    Database.Client.GetOneReadConnection.GetDocumentsAndFree(
      TMongoMsg.Create(fDatabase.Name, fName, variant(cmd), [], maxInt), res);
    {$endif MONGO_OLDPROTOCOL}
  end
  else
  begin
    // db.runCommand({aggregate:"test",pipeline:[{$group:{_id:null,max:{$max:"$_id"}}}]})
    Database.RunCommand(BsonVariant([
      'aggregate', fName,
      'pipeline', pipelineArray]), reply);
    // { "result" : [ { "_id" : null, "max" : 1250 } ], "ok" : 1 }
    res := reply.result;
  end;
  result := not VarIsNull(res);
end;

function TMongoCollection.AggregateDocFromVariant(const pipelineArray: variant): variant;
var
  reply: variant;
begin
  if AggregateCallFromVariant(pipelineArray, reply, result) then
    TDocVariant.GetSingleOrDefault(result, result, result)
  else
    SetVariantNull(result);
end;

function TMongoCollection.AggregateJsonFromVariant(const pipelineArray: variant;
  Mode: TMongoJsonMode = modMongoStrict): RawUtf8;
var
  reply, res: variant;
begin
  if AggregateCallFromVariant(pipelineArray, reply, res) then
    result := VariantSaveMongoJson(res, Mode)
  else
    result := '';
end;

function TMongoCollection.AggregateDocFromJson(const PipelineJson: RawUtf8): variant;
var
  reply: variant;
begin
  if AggregateCallFromJson(PipelineJson, reply, result) then
    TDocVariant.GetSingleOrDefault(result, result, result)
  else
    SetVariantNull(result);
end;

function TMongoCollection.AggregateJsonFromJson(const PipelineJson: RawUtf8;
  Mode: TMongoJsonMode): RawUtf8;
var
  reply, res: variant;
begin
  if AggregateCallFromJson(PipelineJson, reply, res) then
    result := VariantSaveMongoJson(res, Mode)
  else
    result := '';
end;

function TMongoCollection.Drop: RawUtf8;
var
  res: Variant;
  {%H-}log: ISynLog;
begin
  if self = nil then
  begin
    result := 'No collection';
    exit;
  end;
  if Database.Client.Log <> nil then
    log := Database.Client.Log.Enter('Drop %', [fName], self);
  result := fDatabase.RunCommand(BsonVariant('{drop:?}', [], [fName]), res);
  Database.Client.Log.Log(sllTrace, 'Drop("%")->%', [fName, res], self);
  if result = '' then
    Database.fCollections.Delete(fName);
end;

procedure TMongoCollection.EnsureIndex(const Keys, Options: variant);
var
  doc, res: variant;
  indexName: RawUtf8;
  ndx, order: integer;
  useCommand: boolean;
  {%H-}log: ISynLog;
begin
  if (self = nil) or
     (Database = nil) then
    exit;
  if Database.Client.Log <> nil then
    log := Database.Client.Log.Enter('EnsureIndex %', [fName], self);
  if DocVariantData(Keys)^.kind <> dvObject then
    raise EMongoException.CreateUtf8('%[%].EnsureIndex(Keys?)', [self,
      FullCollectionName]);
  useCommand := fDatabase.Client.ServerBuildInfoNumber >= 2060000;
  doc := _ObjFast(['key', Keys]);
  if not useCommand then
    doc.ns := FullCollectionName;
  with _Safe(Options)^ do
    for ndx := 0 to Count - 1 do
      if Names[ndx] = 'name' then
        indexName := VariantToUtf8(Values[ndx])
      else
        TDocVariantData(doc).AddValue(Names[ndx], Values[ndx]);
  if {%H-}indexName = '' then
  begin
    with _Safe(Keys)^ do
      for ndx := 0 to count - 1 do
      begin
        indexName := indexName + Names[ndx] + '_';
        order := VariantToIntegerDef(Values[ndx], 10);
        if order = -1 then
          indexName := indexName + '_'
        else if order <> 1 then
          raise EMongoException.CreateUtf8('%[%].EnsureIndex() on order {%:%}',
            [self, FullCollectionName, Names[ndx], Values[ndx]]);
      end;
  end;
  if length(FullCollectionName) + length(indexName) > 120 then
    raise EMongoException.CreateUtf8(
      '%[%].EnsureIndex() computed name > 128 chars: please set as option',
      [self, FullCollectionName]);
  doc.name := indexName;
  if useCommand then
    fDatabase.RunCommand(BsonVariant('{ createIndexes: ?, indexes: [?] }',
      [], [fName, doc]), res)
  else
    fDatabase.GetCollectionOrCreate('system.indexes').Insert([doc]);
  Database.Client.Log.Log(sllTrace, 'EnsureIndex("%",%)->%', [fName, doc, res], self);
end;

procedure TMongoCollection.EnsureIndex(const Keys: array of RawUtf8;
  Ascending, Unique: boolean);
const
  order: array[boolean] of integer = ( -1, 1);
var
  k, opt: variant;
  A: integer;
begin
  if high(Keys) < 0 then
    exit; // no column name
  TDocVariant.NewFast(k);
  for A := 0 to high(Keys) do
    TDocVariantData(k).AddValue(Keys[A], order[Ascending]);
  if Unique then
    opt := _ObjFast(['unique', VarTrue]);
  EnsureIndex(k, opt{%H-});
end;

function TMongoCollection.Count: Int64;
var
  res: variant;
begin
  fDatabase.RunCommand(BsonVariant([
    'count', fName]), res);
  result := _Safe(res)^.GetValueOrDefault('n', 0);
end;

function TMongoCollection.FindCount(const Query: variant): Int64;
var
  res: variant;
begin
  fDatabase.RunCommand(BsonVariant([
    'count', fName,
    'query', Query]), res);
  result := _Safe(res)^.GetValueOrDefault('n', 0);
end;

function TMongoCollection.FindCount(const Criteria: RawUtf8;
  const Args, Params: array of const;
  MaxNumberToReturn, NumberToSkip: integer): Int64;
var
  cmd, query: RawUtf8;
  res: variant;
begin
  query := FormatUtf8(Criteria, Args, Params, true);
  FormatUtf8('{count:"%",query:%', [fName, query], cmd);
  if MaxNumberToReturn > 0 then
    cmd := FormatUtf8('%,limit:%', [cmd, MaxNumberToReturn]);
  if NumberToSkip > 0 then
    cmd := FormatUtf8('%,skip:%', [cmd, NumberToSkip]);
  fDatabase.RunCommand(BsonVariant(cmd + '}'), res);
  result := _Safe(res)^.GetValueOrDefault('n', 0);
end;

function TMongoCollection.IsEmpty: boolean;
var
  res: variant;
begin
  // much faster than Count>0 for huge collections
  res := FindDoc(BsonVariant('{$query:{}}'), BsonVariant(['_id', 1]));
  result := VarIsEmptyOrNull(res);
end;

{$ifdef MONGO_OLDPROTOCOL}

function TMongoCollection.DoFind(const Criteria, Projection: Variant;
  NumberToReturn, NumberToSkip: integer; Flags: TMongoQueryFlags): TMongoRequest;
begin
  result := TMongoRequestQuery.Create(fFullCollectionName,
    Criteria, Projection, NumberToReturn, NumberToSkip, Flags);
end;

{$else}

function TMongoCollection.DoFind(const Criteria, Projection: Variant;
  NumberToReturn, NumberToSkip: integer; Flags: TMongoQueryFlags): TMongoRequest;
var
  cmd: TDocVariantData;
  v: variant;
begin
  cmd.InitFast(7, dvObject);
  cmd.AddValue('find', fName);
  if not VarIsEmptyOrNull(Criteria) then
    if BsonVariantType.GetItem(Criteria, '$query', v) then
    begin
      // handle OP_QUERY criteria as generated by TRestStorageMongoDB.EngineList
      if not VarIsVoid(v) then
        cmd.AddValue('filter', v);
      if BsonVariantType.GetItem(Criteria, '$orderby', v) then
        cmd.AddValue('sort', v);
    end
    else
      cmd.AddValue('filter', Criteria);
  if not VarIsEmptyOrNull(Projection) then
    if VarIsStr(Projection) then
      raise EMongoException.CreateUtf8('%.DoFind: unsupported string', [self])
    else
      cmd.AddValue('projection', Projection);
  if cardinal(NumberToReturn) < cardinal(maxInt) then
    cmd.AddValue('limit', NumberToReturn);
  if NumberToSkip <> 0 then
    cmd.AddValue('skip', NumberToSkip);
  if NumberToReturn > 100 then // default MongoDB batch size of 101
    cmd.AddValue('batchSize', fDatabase.Client.FindBatchSize);
  //writeln('> ', variant(cmd));
  result := TMongoMsg.Create(
    fDatabase.Name, fName, variant(cmd), Flags, NumberToReturn);
end;

{$endif MONGO_OLDPROTOCOL}

function TMongoCollection.FindBson(const Criteria, Projection: Variant;
  NumberToReturn, NumberToSkip: integer;
  Flags: TMongoQueryFlags): TBsonDocument;
begin
  result := Database.Client.GetOneReadConnection.GetBsonAndFree(
    DoFind(Criteria, Projection, NumberToReturn, NumberToSkip, Flags));
end;

function TMongoCollection.FindDoc(const Criteria, Projection: Variant;
  NumberToReturn, NumberToSkip: integer; Flags: TMongoQueryFlags): variant;
begin
  Database.Client.GetOneReadConnection.GetDocumentsAndFree(
    DoFind(Criteria, Projection, NumberToReturn, NumberToSkip, Flags), result);
end;

function TMongoCollection.FindDoc(const Criteria: RawUtf8;
  const Params: array of const; NumberToReturn, NumberToSkip: integer;
  Flags: TMongoQueryFlags): variant;
begin
  result := FindDoc(
    BsonVariant(Criteria, [], Params), null, NumberToReturn, NumberToSkip, Flags);
end;

procedure TMongoCollection.FindDocs(const Criteria: RawUtf8;
  const Params: array of const; var result: TVariantDynArray;
  const Projection: variant; NumberToReturn, NumberToSkip: integer;
  Flags: TMongoQueryFlags);
begin
  Database.Client.GetOneReadConnection.GetDocumentsAndFree(
    DoFind(BsonVariant(Criteria, [], Params), Projection,
           NumberToReturn, NumberToSkip, Flags), result);
end;

function TMongoCollection.FindDocs(const Criteria: RawUtf8;
  const Params: array of const; const Projection: variant;
  NumberToReturn, NumberToSkip: integer;
  Flags: TMongoQueryFlags): TVariantDynArray;
begin
  FindDocs(Criteria, Params, result, Projection,
   NumberToReturn, NumberToSkip, Flags);
end;

function TMongoCollection.ExistOne(const _id: TBsonObjectID): boolean;
begin
  result := ExistOne(_id.ToVariant);
end;

function TMongoCollection.ExistOne(const _id: variant): boolean;
var
  res: variant;
begin
  // faster to retrieve only the _id field, not whole document
  res := FindDoc(BsonVariant(['_id', _id]), BsonVariant(['_id', 1]), 1);
  result := not VarIsEmptyOrNull(res);
end;

function TMongoCollection.FindOne(const _id: TBsonObjectID): variant;
begin
  result := FindOne(['_id', _id.ToVariant]);
end;

function TMongoCollection.FindOne(const _id: variant): variant;
begin
  result := FindOne(['_id', _id]);
end;

function TMongoCollection.FindOne(const NameValuePairs: array of const;
  ReturnNewObjectIfNotFound: boolean): variant;
begin
  result := FindDoc(BsonVariant(NameValuePairs), null, 1);
  if ReturnNewObjectIfNotFound and
     VarIsEmptyOrNull(result) then
    TDocVariantData(result).InitObject(NameValuePairs, JSON_FAST);
end;

procedure TMongoCollection.FindDocs(var result: TVariantDynArray;
  const Projection: variant; NumberToReturn, NumberToSkip: integer;
  Flags: TMongoQueryFlags);
begin
  Database.Client.GetOneReadConnection.GetDocumentsAndFree(
    DoFind(null, Projection, NumberToReturn, NumberToSkip, Flags), result);
end;

function TMongoCollection.FindJson(const Criteria, Projection: Variant;
  NumberToReturn, NumberToSkip: integer; Flags: TMongoQueryFlags;
  Mode: TMongoJsonMode): RawUtf8;
begin
  result := Database.Client.GetOneReadConnection.GetJsonAndFree(
    DoFind(Criteria, Projection, NumberToReturn, NumberToSkip, Flags), Mode);
end;

function TMongoCollection.FindJson(const Criteria: RawUtf8;
  const Params: array of const; NumberToReturn, NumberToSkip: integer;
  Flags: TMongoQueryFlags; Mode: TMongoJsonMode): RawUtf8;
begin
  result := FindJson(
    BsonVariant(Criteria, [], Params), null, NumberToReturn,
    NumberToSkip, Flags, Mode);
end;

function TMongoCollection.FindJson(
  const Criteria: RawUtf8; const Params: array of const;
  const Projection: variant; NumberToReturn, NumberToSkip: integer;
  Flags: TMongoQueryFlags; Mode: TMongoJsonMode): RawUtf8;
begin
  result := FindJson(
    BsonVariant(Criteria, [], Params), Projection, NumberToReturn,
    NumberToSkip, Flags, Mode);
end;

{$ifdef MONGO_OLDPROTOCOL}

function TMongoCollection.Insert(const Documents: array of variant;
  Flags: TMongoInsertFlags; NoAcknowledge: boolean): integer;
begin
  result := 0;
  _Safe(Database.Client.Connections[0].SendAndFree(
    TMongoRequestInsert.Create(fFullCollectionName,
      Documents, Flags), NoAcknowledge))^.GetAsInteger('n', result);
end;

function TMongoCollection.Insert(const Documents: TBsonDocument; Flags:
  TMongoInsertFlags; NoAcknowledge: boolean): integer;
begin
  result := 0;
  _Safe(Database.Client.Connections[0].SendAndFree(
    TMongoRequestInsert.Create(fFullCollectionName,
      Documents, Flags), NoAcknowledge))^.GetAsInteger('n', result);
end;

function TMongoCollection.Update(const Query, Update: variant;
  Flags: TMongoUpdateFlags): integer;
begin
  result := 0;
  _Safe(Database.Client.Connections[0].SendAndFree(
    TMongoRequestUpdate.Create(fFullCollectionName,
      Query, Update, Flags), false))^.GetAsInteger('n', result);
end;

function TMongoCollection.Remove(const Query: variant;
  Flags: TMongoDeleteFlags): integer;
begin
  result := 0;
  _Safe(Database.Client.Connections[0].SendAndFree(
    TMongoRequestDelete.Create(fFullCollectionName,
      Query, Flags), False))^.GetAsInteger('n', result);
end;

{$else}

function TMongoCollection.DoOperationCommand(const Command, DocsName: RawUtf8;
  const Docs: TBsonDocument; ContOnFail, BypassValidation: boolean): integer;
var
  W: TBsonWriter;
  cmd, res: variant;
  errors: PDocVariantData;
  tmp: TTextWriterStackBuffer;
begin
  result := 0;
  W := TBsonWriter.Create(tmp{%H-});
  try
    W.BsonDocumentBegin;
    W.BsonWriteUtf8(Command, fName);
    W.BsonWrite(DocsName, betArray);
    W.WriteBinary(Docs);
    if ContOnFail then
      W.BsonWrite('ordered', false); // default = true = stop at failure
    if BypassValidation then
      W.BsonWrite('bypassDocumentValidation', true);
    // TODO: writeConcern optional parameter?
    W.BsonDocumentEnd;
    W.ToBsonVariant(cmd);
  finally
    W.Free;
  end;
  Database.RunCommand(cmd, res);
  with _Safe(res)^ do
    // we can have "ok":1 with some "writeErrors":[..]
    if GetAsArray('writeErrors', errors) then
      if ContOnFail then
        Database.Client.Log.Log(sllWarning, '% on % failed as %',
          [Command, fFullCollectionName, PVariant(errors)^], self)
      else
        raise EMongoException.CreateUtf8('%.% on % failed as %',
          [self, Command, fFullCollectionName, PVariant(errors)^])
    else
      GetAsInteger('n', result); // =written docs, excluding writeErrors[] items
end;

function TMongoCollection.Insert(const Documents: TBsonDocument;
  Flags: TMongoInsertFlags): integer;
begin
  result := DoOperationCommand('insert', 'documents', Documents,
    mifContinueOnError in Flags, mifBypassDocumentValidation in Flags);
end;

function TMongoCollection.Insert(const Documents: array of variant;
  Flags: TMongoInsertFlags): integer;
begin
  result := Insert(BsonFrom(Documents), Flags);
end;

function OneMongoUpdate(const Query, Update: variant;
  Flags: TMongoUpdateFlags): variant;
var
  data: TDocVariantData absolute result;
begin
  data.InitFast(4, dvObject);
  data.AddValue('q', Query);
  data.AddValue('u', Update);
  if mufUpsert in Flags then
    data.AddValue('upsert', VarTrue);
  if mufMultiUpdate in Flags then
    data.AddValue('multi', VarTrue);
end;

function TMongoCollection.Update(const Query, Update: variant;
  Flags: TMongoUpdateFlags): integer;
begin
  result := DoOperationCommand('update', 'updates',
    BsonFrom([OneMongoUpdate(Query, Update, Flags)]),
    mufContinueOnError in Flags, mufBypassDocumentValidation in Flags);
end;

function TMongoCollection.Update(const Queries, Updates: TVariantDynArray;
  Flags: TMongoUpdateFlags): integer;
var
  i, n: PtrInt;
  upd: TVariantDynArray;
begin
  n := length(Queries);
  if length(Updates) <> length(Queries) then
    raise EMongoException.CreateUtf8('%.Update(%<>%)', [self, n, length(Updates)]);
  SetLength(upd, n);
  for i := 0 to n - 1 do
    upd[i] := OneMongoUpdate(Queries[i], Updates[i], Flags);
  result := DoOperationCommand('update', 'updates', BsonFrom(upd),
    mufContinueOnError in Flags, mufBypassDocumentValidation in Flags);
end;

function OneMongoDelete(const Query: variant;
  Flags: TMongoDeleteFlags): variant;
begin
  result := _ObjFast(['q',     Query,
                      'limit', ord(mdfSingleRemove in Flags)]);
end;

function TMongoCollection.Remove(const Query: variant;
  Flags: TMongoDeleteFlags): integer;
begin
  result := DoOperationCommand('delete', 'deletes',
    BsonFrom([OneMongoDelete(Query, Flags)]),
    mdfContinueOnError in Flags, false);
end;

{$endif MONGO_OLDPROTOCOL}

function EnsureDocumentHasID(var doc: TDocVariantData; oid: PVariant;
  DocumentObjectID: PBsonObjectID; deleteIndex: PInteger): boolean;
var
  ndx: integer;
  id: TBsonObjectID;
  v: PVariant;
begin
  if deleteIndex <> nil then
    deleteIndex^ := -1;
  ndx := doc.GetValueIndex('_id', 3, true);
  if ndx < 0 then
  begin
    ndx := doc.InternalAdd('_id');
    v := @doc.Values[ndx];
    result := true; // if _id needed to be computed (i.e. save=insert)
  end
  else
  begin
    v := @doc.Values[ndx];
    // _id may be an Int64=TID, not a ObjectID
    result := PVarData(v)^.VType <= varNull;
  end;
  if result then
  begin
    // no valid _id is found -> compute a new genuine one
    id.ComputeNew;
    id.ToVariant(v^);
    if DocumentObjectID <> nil then
      DocumentObjectID^ := id;
    if oid <> nil then
      oid^ := v^;
  end
  else
  begin
    // an _id is supplied
    if (DocumentObjectID <> nil) and
       not DocumentObjectID^.FromVariant(v^) then
      DocumentObjectID^.Init;
    if oid <> nil then
    begin
      // update: no need to include the _id into the payload
      oid^ := v^;
      if deleteIndex <> nil then
      begin
        deleteIndex^ := ndx;
        doc.Delete(ndx);
      end;
    end;
  end;
end;

procedure TMongoCollection.Insert(const Document: RawUtf8;
  const Params: array of const; DocumentObjectID: PBsonObjectID);
var
  doc: variant;
begin
  _JsonFmt(Document, [], Params, JSON_FAST, doc);
  EnsureDocumentHasID(TDocVariantData(doc), nil, DocumentObjectID, nil);
  Insert([doc]);
end;

function TMongoCollection.Save(var Document: variant;
  DocumentObjectID: PBsonObjectID): boolean;
var
  oid: variant;
  deleteIndex: integer;
begin
  if not DocVariantType.IsOfType(Document) then
    Document := _JsonFast(VariantSaveMongoJson(Document, modMongoShell));
  result := EnsureDocumentHasID(
    _Safe(Document, dvObject)^, @oid, DocumentObjectID, @deleteIndex);
  if result then
    Insert([Document])
  else
  begin
    Update(BsonVariant(['_id', oid]), Document, [mufUpsert]);
    if deleteIndex >= 0 then // restore _id in place
      _Safe(Document)^.AddValue('_id', oid, false, deleteIndex);
  end;
end;

procedure TMongoCollection.Save(const Document: RawUtf8;
  const Params: array of const; DocumentObjectID: PBsonObjectID);
var
  doc: variant;
begin
  _JsonFmt(Document, [], Params, JSON_FAST, doc);
  Save(doc, DocumentObjectID);
end;

procedure TMongoCollection.Update(
  const Query: RawUtf8; const QueryParams: array of const;
  const Update: RawUtf8; const UpdateParams: array of const;
  Flags: TMongoUpdateFlags);
var
  quer, upd: variant;
begin
  quer := BsonVariant(Query, [], QueryParams);
  upd  := BsonVariant(Update, [], UpdateParams);
  self.Update(quer, upd, Flags);
end;

procedure TMongoCollection.UpdateOne(const _id, UpdatedFields: variant);
begin
  Update(BsonVariant(['_id', _id]), BsonVariant(['$set', UpdatedFields]));
end;

procedure TMongoCollection.RemoveOne(const _id: TBsonObjectID);
begin
  Remove(BsonVariant(['_id', _id.ToVariant]), [mdfSingleRemove]);
end;

procedure TMongoCollection.RemoveOne(const _id: variant);
begin
  Remove(BsonVariant(['_id', _id]), [mdfSingleRemove]);
end;

procedure TMongoCollection.RemoveFmt(const Query: RawUtf8;
  const Params: array of const; Flags: TMongoDeleteFlags);
begin
  Remove(BsonVariant(Query, [], Params), Flags);
end;

function ToText(wc: TMongoClientWriteConcern): PShortString;
begin
  result := GetEnumName(TypeInfo(TMongoClientWriteConcern), ord(wc));
end;

function ToText(pref: TMongoClientReplicaSetReadPreference): PShortString;
begin
  result := GetEnumName(TypeInfo(TMongoClientReplicaSetReadPreference), ord(pref));
end;


initialization
  {$ifdef MONGO_OLDPROTOCOL}
  Assert(SizeOf(TMongoReplyHeader) = 36);
  {$else}
  Assert(SizeOf(TMongoMsgHeader) = 21);
  {$endif MONGO_OLDPROTOCOL}

end.

