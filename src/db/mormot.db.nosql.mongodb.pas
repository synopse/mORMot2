/// Database Framework MongoDB Direct Client
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.db.nosql.mongodb;

{
  *****************************************************************************

   Efficient BSON Support for MongoDB Clients
    - MongoDB Protocol Items
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
  mormot.core.crypto,
  mormot.db.core,
  mormot.db.nosql.bson,
  mormot.net.sock;


{ ************ MongoDB Protocol Items }

const
  /// MongoDB server default IP port
  MONGODB_DEFAULTPORT = 27017;

type
  /// exception type used for MongoDB process
  EMongoException = class(ESynException);

  /// the available MongoDB driver Request Opcodes
  // - opReply: database reply to a client request - ResponseTo shall be set
  // - opMsgOld: generic msg command followed by a string (deprecated)
  // - opUpdate: update document
  // - opInsert: insert new document
  // - opQuery: query a collection
  // - opGetMore: get more data from a previous query
  // - opDelete: delete documents
  // - opKillCursors: notify database client is done with a cursor
  // - opMsg: new OP_MSG layout introduced in MongoDB 3.6
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

  /// define how an opUpdate operation will behave
  // - if mufUpsert is set, the database will insert the supplied object into
  // the collection if no matching document is found
  // - if mufMultiUpdate is set, the database will update all matching objects
  // in the collection; otherwise (by default) only updates first matching doc
  TMongoUpdateFlag = (
    mufUpsert,
    mufMultiUpdate);

  /// define how a TMongoRequestUpdate message will behave
  TMongoUpdateFlags = set of TMongoUpdateFlag;

  /// define how an opInsert operation will behave
  // - if mifContinueOnError is set, the database will not stop processing a
  // bulk insert if one fails (e.g. due to duplicate IDs); this makes bulk
  // insert behave similarly to a series of single inserts, except lastError
  // will be set if any insert fails, not just the last one - if multiple
  // errors occur, only the most recent will be reported by getLastError
  TMongoInsertFlag = (
    mifContinueOnError);

  /// define how a TMongoRequestInsert message will behave
  TMongoInsertFlags = set of TMongoInsertFlag;

  /// define how an opDelete operation will behave
  // - if mdfSingleRemove is set, the database will remove only the first
  // matching document in the collection. Otherwise (by default) all matching
  // documents will be removed
  TMongoDeleteFlag = (
    mdfSingleRemove);

  /// define how a TMongoRequestDelete message will behave
  TMongoDeleteFlags = set of TMongoDeleteFlag;

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

  /// abstract class used to create MongoDB Wire Protocol client messages
  // - see http://docs.mongodb.org/meta-driver/latest/legacy/mongodb-wire-protocol
  // - this class is not tight to the connection class itself (which is one
  // known limitation of TMongoWire for instance)
  TMongoRequest = class(TBSONWriter)
  protected
    fRequestID: integer;
    fResponseTo: integer;
    fRequestOpCode: TMongoOperation;
    fDatabaseName, fCollectionName, fFullCollectionName: RawUTF8;
    fBSONDocument: TBSONDocument;
  public
    /// write a standard Message Header for MongoDB client
    // - opCode is the type of the message
    // - requestID  is a client or database-generated identifier that uniquely
    // identifies this message: in case of opQuery or opGetMore messages, it will
    // be sent in the responseTo field from the database
    // - responseTo is the requestID taken from previous opQuery or opGetMore
    constructor Create(const FullCollectionName: RawUTF8;
      opCode: TMongoOperation; requestID, responseTo: Integer); reintroduce;
    /// append a query parameter as a BSON document
    // - param can be a TDocVariant, e.g. created with:
    // ! _JsonFast('{name:"John",age:{$gt:21}}');
    // ! _JsonFastFmt('{name:?,age:{$gt:?}}',[],['John',21]);
    // ! _JsonFastFmt('{name:?,field:/%/i}',['acme.*corp'],['John']);
    // - param can be a TBSONVariant containing a TBSONDocument raw binary block
    // created e.g. from:
    // ! BSONVariant(['BSON',_Arr(['awesome',5.05, 1986])])
    // ! BSONVariantType[BSON(['BSON',_Arr(['awesome',5.05, 1986])])]
    // - if param is null, it will append a void document
    // - if param is a string, it will be converted as expected by most
    // database commands, e.g.
    // ! TMongoRequestQuery.Create('admin.$cmd','buildinfo',[],1)
    // will query   { buildinfo: 1 }  to the  admin.$cmd  collection, i.e.
    // $ admin.$cmd.findOne( { buildinfo: 1 } )
    procedure BSONWriteParam(const paramDoc: variant);
    /// flush the content and return the whole binary encoded stream
    // - expect the TBSONWriter instance to have been created with reintroduced
    // Create() specific constructors inheriting from this TMongoRequest class
    // - this overridden version will adjust the size in the message header
    procedure ToBSONDocument(var result: TBSONDocument); override;
    /// write the main parameters of the request as JSON
    procedure ToJSON(W: TTextWriter; Mode: TMongoJSONMode); overload; virtual;
    /// write the main parameters of the request as JSON
    function ToJSON(Mode: TMongoJSONMode): RawUTF8; overload;
    /// identify the message, after call to any reintroduced Create() constructor
    property MongoRequestID: integer read fRequestID;
    /// the associated full collection name, e.g. 'db.test'
    property FullCollectionName: RawUTF8 read fFullCollectionName;
    /// the associated full collection name, e.g. 'db'
    property DatabaseName: RawUTF8 read fDatabaseName;
    /// the associated full collection name, e.g. 'test'
    property CollectionName: RawUTF8 read fCollectionName;
    /// the message operation code
    // - should be either opUpdate, opInsert, opQuery, opGetMore, opDelete
    // or opKillCursors, depending on the TMongoRequest* class instantiated
    property MongoRequestOpCode: TMongoOperation read fRequestOpCode;
  end;

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
    // TBSONVariant - i.e. created via BSONVariant() - or null if all documents
    // are to be updated
    // - Update is the BSON document specification of the update to perform,
    // supplied as TDocVariant or TBSONVariant
    // - there is no response to an opUpdate message
    constructor Create(const FullCollectionName: RawUTF8;
      const Selector, Update: variant;
      Flags: TMongoUpdateFlags = []); reintroduce;
    /// write the main parameters of the request as JSON
    procedure ToJSON(W: TTextWriter; Mode: TMongoJSONMode); override;
  end;

  /// a MongoDB client message to insert one or more documents in a collection
  TMongoRequestInsert = class(TMongoRequestWritable)
  public
    /// initialize a MongoDB client message to insert one or more documents in
    // a collection, supplied as variants
    // - FullCollectionName is e.g. 'dbname.collectionname'
    // - Documents is an array of TDocVariant or TBSONVariant - i.e. created via
    // _JsonFast() _JsonFastFmt() or BSONVariant()
    // - there is no response to an opInsert message
    constructor Create(const FullCollectionName: RawUTF8;
      const Documents: array of variant;
      Flags: TMongoInsertFlags = []); reintroduce; overload;
    /// initialize a MongoDB client message to insert one or more documents in
    // a collection, supplied as raw BSON binary
    // - FullCollectionName is e.g. 'dbname.collectionname'
    // - Documents is the low-level concatenation of BSON documents, created
    // e.g. with a TBSONWriter stream
    // - there is no response to an opInsert message
    constructor Create(const FullCollectionName: RawUTF8;
      const Documents: TBSONDocument;
      Flags: TMongoInsertFlags = []); reintroduce; overload;
    /// initialize a MongoDB client message to insert one or more documents in
    // a collection, supplied as JSON objects
    // - FullCollectionName is e.g. 'dbname.collectionname'
    // - JSONDocuments is an array of JSON objects
    // - there is no response to an opInsert message
    // - warning: JSONDocuments[] buffer will be modified in-place during
    // parsing, so a private copy may have to be made by the caller
    constructor Create(const FullCollectionName: RawUTF8; const JSONDocuments:
      array of PUTF8Char; Flags: TMongoInsertFlags = []); reintroduce; overload;
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
    // TBSONVariant - i.e. created via BSONVariant() - or null if all documents
    // are to be deleted
    // - warning: CreateDelete('db.coll',null) can be expensive so you should
    // better drop the whole collection
    // - there is no response to an opDelete message
    constructor Create(const FullCollectionName: RawUTF8;
      const Selector: variant;
      Flags: TMongoDeleteFlags = []); reintroduce;
    /// write the main parameters of the request as JSON
    procedure ToJSON(W: TTextWriter; Mode: TMongoJSONMode); override;
  end;

  /// a MongoDB client message to query one or more documents in a collection
  TMongoRequestQuery = class(TMongoRequest)
  protected
    fNumberToReturn, fNumberToSkip: integer;
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
    // - Query can also be a TBSONVariant, e.g. created with:
    // ! BSONVariant('{name:?,age:{$gt:?}}',[],['John',21])
    // - ReturnFieldsSelector is an optional selector (set to null if not
    // applicable) as a BSON document that limits the fields in the returned
    // documents, supplied as TDocVariant or TBSONVariant - e.g. created via:
    // ! BSONVariantFieldSelector('a,b,c');
    // ! BSONVariantFieldSelector(['a','b','c']);
    // ! BSONVariant('{a:1,b:1,c:1}');
    // ! _JsonFast('{a:1,b:1,c:1}');
    // - if ReturnFieldsSelector is a string, it will be converted into
    // $ { ReturnFieldsSelector: 1 }
    constructor Create(const FullCollectionName: RawUTF8;
      const Query, ReturnFieldsSelector: variant; NumberToReturn: integer;
      NumberToSkip: Integer = 0; Flags: TMongoQueryFlags = []); reintroduce;
    /// write the main parameters of the request as JSON
    procedure ToJSON(W: TTextWriter; Mode: TMongoJSONMode); override;
    /// retrieve the NumberToReturn parameter as set to the constructor
    property NumberToReturn: integer read fNumberToReturn;
    /// retrieve the NumberToSkip parameter as set to the constructor
    property NumberToSkip: integer read fNumberToSkip;
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
    constructor Create(const FullCollectionName: RawUTF8;
      NumberToReturn: integer; CursorID: Int64); reintroduce;
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
    constructor Create(const FullCollectionName: RawUTF8;
      const CursorIDs: array of Int64); reintroduce;
    /// write the main parameters of the request as JSON
    procedure ToJSON(W: TTextWriter; Mode: TMongoJSONMode); override;
  end;


  /// used to store the binary raw data a database response to a
  // TMongoRequestQuery / TMongoRequestGetMore client message
  TMongoReply = RawByteString;

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

  /// internal low-level binary structure mapping all message headers
  TMongoWireHeader = packed record
    /// total message length, including the header
    MessageLength: integer;
    /// identifier of this message
    RequestID: integer;
    /// retrieve the RequestID from the original request
    ResponseTo: integer;
    /// low-level code of the message
    // - GetReply() will map it to a high-level TMongoOperation
    OpCode: integer;
  end;

  PMongoWireHeader = ^TMongoWireHeader;

  /// internal low-level binary structure mapping the TMongoReply header
  // - used e.g. by TMongoReplyCursor and TMongoConnection.GetReply()
  TMongoReplyHeader = packed record
    /// standard message header
    Header: TMongoWireHeader;
    /// response flags
    ResponseFlags: integer;
    /// cursor identifier if the client may need to perform further opGetMore
    CursorID: Int64;
    /// where in the cursor this reply is starting
    StartingFrom: integer;
    /// number of documents in the reply
    NumberReturned: integer;
  end;

  /// points to an low-level binary structure mapping the TMongoReply header
  // - so that you can write e.g.
  // ! PMongoReplyHeader(aMongoReply)^.RequestID
  PMongoReplyHeader = ^TMongoReplyHeader;


  /// map a MongoDB server reply message as sent by the database
  // - in response to TMongoRequestQuery / TMongoRequestGetMore messages
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
    fReply: TMongoReply;
    fRequestID: integer;
    fResponseTo: integer;
    fResponseFlags: TMongoReplyCursorFlags;
    fCursorID: Int64;
    fStartingFrom: integer;
    fNumberReturned: integer;
    fDocuments: TPointerDynArray;
    fCurrentPosition: integer;
    fFirstDocument, fCurrentDocument: PAnsiChar;
    fLatestDocIndex: integer;
    fLatestDocValue: variant;
    procedure ComputeDocumentsList;
    function GetOneDocument(index: integer): variant;
  public
    /// initialize the cursor with a supplied binary reply from the server
    // - will raise an EMongoException if the content is not valid
    // - will populate all record fields with the supplied data
    procedure Init(const ReplyMessage: TMongoReply);

    /// retrieve the next document in the list, as a TDocVariant instance
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
      option: TBSONDocArrayConversion = asDocVariantPerReference): boolean; overload;
    /// retrieve the next document in the list, as BSON content
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
    // !     writeln(BSONToJSON(doc,0,modMongoShell)); // fast display
    function Next(out doc: PByte): boolean; overload;
    /// retrieve the next document in the list, as a BSON binary document
    // - return TRUE if the supplied document has been retrieved - then doc
    // points to a "int32 e_list #0" BSON document
    // - return FALSE if there is no more document to get - you can use the
    // Rewind method to restart from the first document
    // - this method is slightly slower than the one returning a PByte, since
    // it will allocate a memory buffer to store the TBSONDocument binary
    // - could be used e.g. as:
    // ! var Reply: TMongoReply;
    // !     doc: TBSONDocument;
    // ! ...
    // !   Reply.Init(ResponseMessage);
    // !   while Reply.Next(doc) do
    // !     writeln(BSONToJSON(doc,0,modMongoShell)); // fast display
    function Next(out BSON: TBSONDocument): boolean; overload;
    /// retrieve the next document in the list, as JSON content
    // - return TRUE if the supplied document has been retrieved
    // - return FALSE if there is no more document to get - you can use the
    // Rewind method to restart from the first document
    // - could be used e.g. as:
    // ! var Reply: TMongoReply;
    // !     json: RawUTF8;
    // ! ...
    // !   Reply.Init(ResponseMessage);
    // !   while Reply.Next(json,modMongoShell) do
    // !     writeln(json); // fast display
    function Next(out JSON: RawUTF8;
      Mode: TMongoJSONMode = modMongoStrict): boolean; overload;
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
    // !      GmrfQueryFailureetDocument(i,doc);
    // !      writeln('Name: ',doc.Name,' FirstName: ',doc.FirstName);
    // !   end;
    procedure GetDocument(index: integer; var result: variant);
    /// return all documents content as a JSON array, or one JSON object
    // if there is only one document in this reply
    // - this method is very optimized and will convert the BSON binary content
    // directly into JSON
    procedure FetchAllToJSON(W: TTextWriter;
      Mode: TMongoJSONMode = modMongoStrict; WithHeader: boolean = false;
      MaxSize: Cardinal = 0);
    /// return all documents content as a JSON array, or one JSON object
    // if there is only one document in this reply
    // - this method is very optimized and will convert the BSON binary content
    // directly into JSON
    function ToJSON(Mode: TMongoJSONMode = modMongoStrict;
      WithHeader: boolean = false; MaxSize: Cardinal = 0): RawUTF8;
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
    procedure AppendAllToBSON(Dest: TBSONWriter);

    /// retrieve the context execution of this message
    property ResponseFlags: TMongoReplyCursorFlags read fResponseFlags;
    /// identifier of this message
    property RequestID: integer read fRequestID;
    /// retrieve the RequestID from the original request
    property ResponseTo: integer read fResponseTo;
    /// access to the low-level binary reply message
    property Reply: TMongoReply read fReply;
    /// cursor identifier if the client may need to perform further
    // TMongoRequestGetMore messages
    // - in the event that the result set of the query fits into one OP_REPLY
    // message, CursorID will be 0
    property CursorID: Int64 read fCursorID;
    /// where in the cursor this reply is starting
    property StartingFrom: integer read fStartingFrom;
    /// number of documents in the reply
    property DocumentCount: Integer read fNumberReturned;
    /// points to the first document binary
    // - i.e. just after the Reply header
    property FirstDocument: PAnsiChar read fFirstDocument;
    /// direct access to the low-level BSON binary content of each document
    property DocumentBSON: TPointerDynArray read fDocuments;
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
    property Document[index: integer]: variant read GetOneDocument;
    /// the current position of the Next() call, starting at 0
    property Position: integer read fCurrentPosition;
  end;


/// ready-to-be displayed text of a TMongoOperation item
function ToText(op: TMongoOperation): PShortString; overload;



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
    fServerAddress: RawUTF8;
    fServerPort: integer;
    procedure Lock;
    procedure UnLock;
    function Send(Request: TMongoRequest): boolean;
    function GetOpened: boolean;
    function GetLocked: boolean;
    // will call TMongoReplyCursor.FetchAllToJSON(TTextWriter(Opaque))
    procedure ReplyJSONStrict(Request: TMongoRequest;
      const Reply: TMongoReplyCursor; var Opaque);
    procedure ReplyJSONExtended(Request: TMongoRequest;
      const Reply: TMongoReplyCursor; var Opaque);
    procedure ReplyJSONNoMongo(Request: TMongoRequest;
      const Reply: TMongoReplyCursor; var Opaque);
    // will call TMongoReplyCursor.AppendAllToDocVariantDynArray(TVariantDynArray(Opaque))
    procedure ReplyDocVariant(Request: TMongoRequest;
      const Reply: TMongoReplyCursor; var Opaque);
    // will call TMongoReplyCursor.AppendAllToBSON(TBSONWrite(Opaque))
    procedure ReplyBSON(Request: TMongoRequest;
      const Reply: TMongoReplyCursor; var Opaque);
  public
    /// initialize the connection to the corresponding MongoDB server
    // - the server address is either a host name, or an IP address
    // - if no server address is specified, will try to connect to localhost
    // - this won't create the connection, until Open method is executed
    constructor Create(const aClient: TMongoClient; const aServerAddress: RawUTF8;
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
    procedure GetReply(Request: TMongoRequest; out result: TMongoReply);
    /// low-level method to send a request to the server, and return a cursor
    // - if Request is not either TMongoRequestQuery or TMongoRequestGetMore,
    // will raise an EMongoException
    // - then will parse and return a cursor to the reply message as sent back
    // by the database, with logging if necessary
    // - raise an EMongoException if mrfQueryFailure flag is set in the reply
    procedure GetCursor(Request: TMongoRequest; var Result: TMongoReplyCursor);
    /// low-level method to send a query to the server, calling a callback event
    // on each reply
    // - is used by GetDocumentsAndFree, GetBSONAndFree and GetJSONAndFree
    // methods to receive the whole document (you should better call those)
    // - the supplied Query instance will be released when not needed any more
    procedure GetRepliesAndFree(Query: TMongoRequestQuery;
      OnEachReply: TOnMongoConnectionReply; var Opaque);

    /// send a query to the server, returning a TDocVariant instance containing
    // all the incoming data
    // - will send the Request message, and any needed TMongoRequestGetMore
    // messages to retrieve all the data from the server
    // - the supplied Query instance will be released when not needed any more
    // - if Query.NumberToReturn<>1, it will return either null or a dvArray
    // kind of TDocVariant containing all returned items
    // - if Query.NumberToReturn=1, then it will return either null or a
    // single TDocVariant instance
    function GetDocumentsAndFree(Query: TMongoRequestQuery): variant; overload;
    /// send a query to the server, returning a TDocVariant instance containing
    // all the incoming data
    // - will send the Request message, and any needed TMongoRequestGetMore
    // messages to retrieve all the data from the server
    // - the supplied Query instance will be released when not needed any more
    // - if Query.NumberToReturn<>1, it will return either null or a dvArray
    // kind of TDocVariant containing all returned items
    // - if Query.NumberToReturn=1, then it will return either null or a
    // single TDocVariant instance
    procedure GetDocumentsAndFree(Query: TMongoRequestQuery;
      var result: variant); overload;
    /// send a query to the server, returning a dynamic array of TDocVariant
    // instance containing all the incoming data
    // - will send the Request message, and any needed TMongoRequestGetMore
    // messages to retrieve all the data from the server
    // - the supplied Query instance will be released when not needed any more
    procedure GetDocumentsAndFree(Query: TMongoRequestQuery;
      var result: TVariantDynArray); overload;
    /// send a query to the server, returning a TBSONDocument instance containing
    // all the incoming data, as raw binary BSON document containing an array
    // of the returned items
    // - will send the Request message, and any needed TMongoRequestGetMore
    // messages to retrieve all the data from the server
    // - the supplied Query instance will be released when not needed any more
    function GetBSONAndFree(Query: TMongoRequestQuery): TBSONDocument;
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
    function GetJSONAndFree(Query: TMongoRequestQuery; Mode: TMongoJSONMode): RawUTF8;

    /// send a message to the MongoDB server
    // - will apply Client.WriteConcern policy, and run an EMongoException
    // in case of any error
    // - the supplied Request instance will be released when not needed any more
    // - by default, it will follow Client.WriteConcern pattern - but you can
    // set NoAcknowledge = TRUE to avoid calling the getLastError command
    // - will return the getLastError reply (if retrieved from server)
    function SendAndFree(Request: TMongoRequest;
      NoAcknowledge: boolean = false): variant;
    /// run a database command, supplied as a TDocVariant, TBSONVariant or a
    // string, and return the a TDocVariant instance
    // - see http://docs.mongodb.org/manual/reference/command for a list
    // of all available commands
    // - for instance:
    // ! RunCommand('test',_ObjFast(['dbStats',1,'scale',1024],stats);
    // ! RunCommand('test',BSONVariant(['dbStats',1,'scale',1024],stats);
    // ! RunCommand('admin','buildinfo',fServerBuildInfo);
    // - the message will be returned by the server as a single TDocVariant
    // instance (since the associated TMongoRequestQuery.NumberToSkip=1)
    // - in case of any error, the error message is returned as text
    // - in case of success, this method will return ''
    function RunCommand(const aDatabaseName: RawUTF8; const command: variant;
      var returnedValue: variant): RawUTF8; overload;
    /// run a database command, supplied as a TDocVariant, TBSONVariant or a
    // string, and return the raw BSON document array of received items
    // - this overloaded method can be used on huge content to avoid the slower
    // conversion to an array of TDocVariant instances
    // - in case of success, this method will return TRUE, or FALSE on error
    function RunCommand(const aDatabaseName: RawUTF8; const command: variant;
      var returnedValue: TBSONDocument): boolean; overload;

    /// return TRUE if the Open method has successfully been called
    property Opened: boolean read GetOpened;
    /// access to the corresponding MongoDB server
    property Client: TMongoClient read fClient;
    /// direct access to the low-level TCP/IP communication socket
    property Socket: TCrtSocket read fSocket;
    /// is TRUE when the connection is busy
    property Locked: boolean read GetLocked;
  published
    /// read-only access to the supplied server address
    // - the server address is either a host name, or an IP address
    property ServerAddress: RawUTF8 read fServerAddress;
    /// read-only access to the supplied server port
    // - the server Port is MONGODB_DEFAULTPORT (27017) by default
    property ServerPort: integer read fServerPort;
  end;

  /// array of TCP connection to a MongoDB Replica Set
  // - first item [0] is the Primary member
  // - other items [1..] are the Secondary members
  TMongoConnectionDynArray = array of TMongoConnection;

  /// define Read Preference Modes to a MongoDB replica set
  // - Important: All read preference modes except rpPrimary may return stale
  // data because secondaries replicate operations from the primary with some
  // delay - ensure that your application can tolerate stale data if you choose
  // to use a non-primary mode
  // - rpPrimary:	Default mode - all operations read from the current replica
  // set primary
  // - rpPrimaryPreferred: in most situations, operations read from the primary
  // but if it is unavailable, operations read from secondary members.
  // - rpPsecondary: all operations read from the secondary members
  // of the replica set
  // - rpPsecondaryPreferred:	in most situations, operations read from
  // secondary members but if no secondary members are available, operations
  // read from the primary
  TMongoClientReplicaSetReadPreference = (
    rpPrimary,
    rpPrimaryPreferred,
    rpSecondary,
    rpSecondaryPreferred);

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

  /// remote access to a MongoDB server
  // - a single server can have several active connections, if some secondary
  // hosts were defined
  TMongoClient = class
  protected
    fConnectionString: RawUTF8;
    fDatabases: TRawUTF8List;
    fConnections: TMongoConnectionDynArray;
    fReadPreference: TMongoClientReplicaSetReadPreference;
    fWriteConcern: TMongoClientWriteConcern;
    fConnectionTimeOut: Cardinal;
    fConnectionTLS: boolean;
    fGracefulReconnect: record
      Enabled, ForcedDBCR: boolean;
      User, Database: RawUTF8;
      EncryptedDigest: RawByteString;
    end;
    fLog: TSynLog;
    fLogRequestEvent: TSynLogInfo;
    fLogReplyEvent: TSynLogInfo;
    fLogReplyEventMaxSize: cardinal;
    fServerBuildInfo: variant;
    fServerBuildInfoNumber: cardinal;
    fLatestReadConnectionIndex: integer;
    procedure AfterOpen; virtual;
    function GetOneReadConnection: TMongoConnection;
    function GetBytesReceived: Int64;
    function GetBytesSent: Int64;
    function GetBytesTransmitted: Int64;
    procedure Auth(const DatabaseName, UserName, Digest: RawUTF8; ForceMongoDBCR: boolean);
    function ReOpen: boolean;
  public
    /// prepare a connection to a MongoDB server or Replica Set
    // - this constructor won't create the connection until the Open method
    // is called
    // - you can specify multiple hosts, as CSV values, if necessary
    // - depending on the platform, you may request for a TLS secured connection
    constructor Create(const Host: RawUTF8; Port: Integer = MONGODB_DEFAULTPORT;
      aTLS: boolean = false; const SecondaryHostCSV: RawUTF8 = ''; const
      SecondaryPortCSV: RawUTF8 = ''); overload;
    /// connect to a database on a remote MongoDB primary server
    // - this method won't use authentication, and will return the corresponding
    // MongoDB database instance
    // - this method is an alias to the Database[] property
    function Open(const DatabaseName: RawUTF8): TMongoDatabase;
    /// secure connection to a database on a remote MongoDB server
    // - this method will use authentication and will return the corresponding
    // MongoDB database instance, with a dedicated secured connection
    // - will use MONGODB-CR for MongoDB engines up to 2.6 (or if ForceMongoDBCR
    // is TRUE), and SCRAM-SHA-1 since MongoDB 3.x
    // - see http://docs.mongodb.org/manual/administration/security-access-control
    function OpenAuth(const DatabaseName, UserName, PassWord: RawUTF8;
      ForceMongoDBCR: boolean = false): TMongoDatabase;
    /// close the connection and release all associated TMongoDatabase,
    // TMongoCollection and TMongoConnection instances
    destructor Destroy; override;
    /// define an optional logging instance to be used
    // - you can also specify the event types to be used for requests or
    // replay: by default, a verbose log with sllSQL and sllDB will be set
    // - e.g. mORMotMongoDB.pas will call Client.SetLog(SQLite3Log) for you
    procedure SetLog(LogClass: TSynLogClass; RequestEvent: TSynLogInfo = sllSQL;
      ReplyEvent: TSynLogInfo = sllDB; ReplyEventMaxSize: cardinal = 1024);

    /// retrieve extended server version and build information, as text
    // - will create a string from ServerBuildInfo object, e.g. as
    // $ 'MongoDB 3.2.0 mozjs mmapv1,wiredTiger'
    function ServerBuildInfoText: RawUTF8;
    /// retrieve the server version and build information
    // - return the content as a TDocVariant document, e.g.
    // ! ServerBuildInfo.version = '2.4.9'
    // ! ServerBuildInfo.versionArray = [2,4,9,0]
    // - this property is cached, so request is sent only once
    // - you may rather use ServerBuildInfoNumber to check for available
    // features at runtime, for easy comparison of the server version
    property ServerBuildInfo: variant read fServerBuildInfo;
    /// access to a given MongoDB database
    // - try to open it via a non-authenticated connection it if not already:
    // will raise an exception on error, or will return an instance
    // - will return an existing instance if has already been opened
    property Database[const DatabaseName: RawUTF8]: TMongoDatabase read Open; default;
    /// low-level access to the TCP/IP connections of this MongoDB replica set
    // - first item [0] is the Primary member
    // - other items [1..] are the Secondary members
    property Connections: TMongoConnectionDynArray read fConnections;
    /// define the logging instance to be used for LogRequestEvent/LogReplyEvent
    // - you may also call the SetLog() method to set all options at once
    property Log: TSynLog read fLog write fLog;
  published
    /// the connection definition used to connect to this MongoDB server
    property ConnectionString: RawUTF8 read fConnectionString;
    /// retrieve the server version and build information
    // - return the content as a TDocVariant document, e.g.
    // ! 2040900 for MongoDB 2.4.9, or 2060000 for MongoDB 2.6, or
    // ! 3000300 for MongoDB 3.0.3
    // - this property is cached, so can be used to check for available
    // features at runtime, without any performance penalty
    property ServerBuildInfoNumber: cardinal read fServerBuildInfoNumber;
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
    property ConnectionTimeOut: Cardinal
      read fConnectionTimeOut write fConnectionTimeOut;
    /// if the socket connection is secured over TLS
    property ConnectionTLS: boolean read fConnectionTLS;
    /// allow automatic reconnection (with authentication, if applying), if the
    // socket is closed (e.g. was dropped from the server)
    property GracefulReconnect: boolean
      read fGracefulReconnect.Enabled write fGracefulReconnect.Enabled;
    /// how may bytes this client did received, among all its connections
    property BytesReceived: Int64 read GetBytesReceived;
    /// how may bytes this client did received, among all its connections
    property BytesSent: Int64 read GetBytesSent;
    /// how may bytes this client did transmit, adding both input and output
    property BytesTransmitted: Int64 read GetBytesTransmitted;
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
    fName: RawUTF8;
    fCollections: TRawUTF8List;
    function GetCollection(const Name: RawUTF8): TMongoCollection;
    function GetCollectionOrCreate(const Name: RawUTF8): TMongoCollection;
    function GetCollectionOrNil(const Name: RawUTF8): TMongoCollection;
  public
    /// initialize a reference to a given MongoDB Database
    // - you should not use this constructor directly, but rather use the
    // TMongoClient.Database[] property
    // - it will connect to the Client's primary host, then retrieve all
    // collection names of this database
    constructor Create(aClient: TMongoClient; const aDatabaseName: RawUTF8);
    /// release all associated TMongoCollection instances
    destructor Destroy; override;

    /// run a database command, supplied as a TDocVariant, TBSONVariant or a
    // string, and return a TDocVariant instance
    // - this is the preferred method to issue database commands, as it provides
    // a consistent interface between the MongoDB shell and this driver
    // - see http://docs.mongodb.org/manual/reference/command for a list
    // of all available commands
    // - for instance:
    // ! RunCommand(_ObjFast(['dbStats',1,'scale',1024],stats);
    // ! RunCommand(BSONVariant(['dbStats',1,'scale',1024],stats);
    // ! RunCommand('dbStats',stats);
    // ! RunCommand('hostInfo',host);
    // - the message will be returned by the server as a TDocVariant instance
    // (since the associated TMongoRequestQuery.NumberToSkip=1)
    // - in case of any error, the error message is returned as text
    // - in case of success, this method will return ''
    function RunCommand(const command: variant;
      var returnedValue: variant): RawUTF8; overload;
    /// run a database command, supplied as a TDocVariant, TBSONVariant or a
    // string, and return the raw BSON document array of received items
    // - this overloaded method can be used on huge content to avoid the slower
    // conversion to an array of TDocVariant instances
    // - in case of success, this method will return TRUE, or FALSE on error
    function RunCommand(const command: variant;
      var returnedValue: TBSONDocument): boolean; overload;

    /// create the user in the database to which the user will belong
    // - you could specify the roles to use, for this database or others:
    // ! reportingDB.CreateUser('reportsUser','12345678',BSONVariant(
    // !  '[{ role: "readWrite", db: "reporting" }, { role: "read", db: "products" }]'));
    // - returns '' on sucess, an error message otherwise
    function CreateUser(const UserName, Password: RawUTF8;
      const roles: variant): RawUTF8;
    /// create the user with a read or read/write role on the current database
    // - returns '' on sucess, an error message otherwise
    function CreateUserForThisDatabase(const UserName, Password: RawUTF8;
      allowWrite: Boolean = true): RawUTF8;
    /// deletes the supplied user on the current database
    // - returns '' on sucess, an error message otherwise
    function DropUser(const UserName: RawUTF8): RawUTF8;

    /// access to a given MongoDB collection
    // - raise an EMongoDatabaseException if the collection name does not exist
    property Collection[const Name: RawUTF8]: TMongoCollection
      read GetCollection; default;
    /// access to a given MongoDB collection
    // - if the collection name does not exist, it will return nil
    property CollectionOrNil[const Name: RawUTF8]: TMongoCollection
      read GetCollectionOrNil;
    /// access to a given MongoDB collection
    // - if the collection name does not exist, it will add use the name to
    // create a TMongoCollection instance and register it to the internal list
    property CollectionOrCreate[const Name: RawUTF8]: TMongoCollection
      read GetCollectionOrCreate;
  published
    /// the database name
    property Name: RawUTF8 read fName;
    /// the associated MongoDB client instance
    property Client: TMongoClient read fClient;
  end;

  /// remote access to a MongoDB collection
  TMongoCollection = class
  protected
    fDatabase: TMongoDatabase;
    fName: RawUTF8;
    fFullCollectionName: RawUTF8;
    function AggregateCallFromJSON(const pipelineJSON: RawUTF8;
      var reply, res: variant): boolean; overload;
    function AggregateCallFromVariant(const pipelineArray: variant;
      var reply, res: variant): boolean; overload;
  public
    /// initialize a reference to a given MongoDB Collection
    // - you should not use this constructor directly, but rather use
    // TMongoClient.Database[].Collection[] property
    constructor Create(aDatabase: TMongoDatabase; const aCollectionName: RawUTF8);

    /// select documents in a collection and returns a dvArray TDocVariant
    // instance containing the selected documents
    // - Criteria can be null (to retrieve all documents) or a TDocVariant /
    // TBSONVariant query selector:
    // ! FindDoc(BSONVariant('{name:"John",age:{$gt:21}}'),null);
    // ! FindDoc(BSONVariant('{name:?,age:{$gt:?}}',[],['John',21]),null);
    // see http://docs.mongodb.org/manual/reference/operator for reference
    // - Projection can be null (to retrieve all fields) or a CSV string to set
    // field names to retrieve, or a TDocVariant or TBSONVariant - e.g.:
    // ! FindDoc(BSONVariant(['name','John']),null);
    // ! FindDoc(BSONVariant(['name','John']),'_id,name');
    // ! FindDoc(BSONVariant(['name','John']),BSONVariantFieldSelector('name,_id'));
    // - NumberToReturn can be left to its default maxInt value to return all
    // matching documents, or specify a limit (e.g. 1 for one document - in this
    // case, the returned instance won't be a dvArray kind of TDocVariant, but
    // either null or the single returned document)
    // - if the query does not have any matching record, it will return null
    function FindDoc(const Criteria, Projection: Variant;
      NumberToReturn:  integer = 1; NumberToSkip: Integer = 0;
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
    function FindDoc(Criteria: PUTF8Char; const Params: array of const;
      NumberToReturn: integer = maxInt; NumberToSkip: Integer = 0;
      Flags: TMongoQueryFlags = []): variant; overload;
    /// find an existing document in a collection, by its _id field
    // - _id will identify the unique document to be retrieved
    // - returns null, or a TDocVariant instance
    function FindOne(const _id: TBSONObjectID): variant; overload;
    /// find an existing document in a collection, by its _id field
    // - _id will identify the unique document to be retrieved
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
    // field names to retrieve, or a TDocVariant or TBSONVariant with
    // projection operators
    procedure FindDocs(var result: TVariantDynArray; const Projection: variant;
      NumberToReturn: integer = maxInt; NumberToSkip: Integer = 0;
      Flags: TMongoQueryFlags = []); overload;
    /// select documents in a collection and returns a dynamic array of
    // TDocVariant instance containing the selected documents
    // - you can e.g. fill a res: TVariantDynArray with the following query:
    // ! FindDocs('{name:?,age:{$gt:?}}',['John',21],res,null);
    // - Projection can be null (to retrieve all fields) or a CSV string to set
    // field names to retrieve, or a TDocVariant or TBSONVariant with
    // projection operators
    procedure FindDocs(Criteria: PUTF8Char; const Params: array of const;
      var result: TVariantDynArray; const Projection: variant;
      NumberToReturn: integer = maxInt; NumberToSkip: Integer = 0;
      Flags: TMongoQueryFlags = []); overload;
    /// select documents in a collection and returns a dynamic array of
    // TDocVariant instance containing the selected documents
    // - could be used to fill a VCL grid using a TDocVariantArrayDataSet
    // as defined in SynVirtualDataSet.pas:
    // ! ds1.DataSet := ToDataSet(self,FindDocs('{name:?,age:{$gt:?}}',['John',21],null));
    // - Projection can be null (to retrieve all fields) or a CSV string to set
    // field names to retrieve, or a TDocVariant or TBSONVariant with
    // projection operators
    function FindDocs(Criteria: PUTF8Char; const Params: array of const;
      const Projection: variant; NumberToReturn: integer = maxInt;
      NumberToSkip: Integer = 0; Flags: TMongoQueryFlags = []): TVariantDynArray; overload;

    /// select documents in a collection and returns a JSON array of documents
    // containing the selected documents
    // - Criteria can be null (to retrieve all documents) or a TDocVariant /
    // TBSONVariant query selector:
    // ! FindJSON(BSONVariant('{name:"John",age:{$gt:21}}'),null);
    // ! FindJSON(BSONVariant('{name:?,age:{$gt:?}}',[],['John',21]),null);
    // see http://docs.mongodb.org/manual/reference/operator for reference
    // - Projection can be null (to retrieve all fields) or a CSV string to set
    // the field names to retrieve, or a TDocVariant or TBSONVariant - e.g.:
    // ! FindJSON(BSONVariant(['name','John']),null);
    // ! FindJSON(BSONVariant(['name','John']),'_id');
    // ! FindJSON(BSONVariant(['name','John']),BSONVariantFieldSelector('name,_id'));
    // - NumberToReturn can be left to its default maxInt value to return all
    // matching documents as a '[..]' JSON array, or specify a limit (e.g. 1
    // for one document - in this case, the returned instance won't be a '[..]'
    // JSON array, but either 'null' or a single '{..}' JSON object)
    // - this method is very optimized and will convert the BSON binary content
    // directly into JSON, in either modMongoStrict or modMongoShell layout
    // (modNoMongo will do the same as modMongoStrict)
    function FindJSON(const Criteria, Projection: Variant;
      NumberToReturn: integer = maxInt; NumberToSkip: Integer = 0;
      Flags: TMongoQueryFlags = [];
      Mode: TMongoJSONMode = modMongoStrict): RawUTF8; overload;
    /// select documents in a collection and returns a JSON array of documents
    // containing the selected documents
    // - Criteria can specify the query selector as (extended) JSON and
    // parameters:
    // ! FindJSON('{name:"John",age:{$gt:21}}',[]);
    // ! FindJSON('{name:?,age:{$gt:?}}',['John',21]);
    // see http://docs.mongodb.org/manual/reference/operator for reference
    // - this overloaded method will use a null Projection, i.e. will retrieve
    // all fields
    // - NumberToReturn can be left to its default maxInt value to return all
    // matching documents as a '[..]' JSON array, or specify a limit (e.g. 1
    // for one document - in this case, the returned instance won't be a '[..]'
    // JSON array, but either 'null' or a single '{..}' JSON object)
    function FindJSON(Criteria: PUTF8Char; const Params: array of const;
      NumberToReturn: integer = maxInt; NumberToSkip: Integer = 0;
      Flags: TMongoQueryFlags = [];
      Mode: TMongoJSONMode = modMongoStrict): RawUTF8; overload;
    /// select documents in a collection and returns a JSON array of documents
    // containing the selected documents
    // - Criteria and Projection can specify the query selector as (extended)
    // JSON and parameters
    function FindJSON(Criteria: PUTF8Char; const CriteriaParams: array of const;
      const Projection: variant; NumberToReturn: integer = maxInt;
      NumberToSkip: Integer = 0; Flags: TMongoQueryFlags = [];
      Mode: TMongoJSONMode = modMongoStrict): RawUTF8; overload;

    /// select documents in a collection and returns a TBSONDocument instance
    // containing the selected documents as a raw binary BSON array document
    // - Criteria can be null (to retrieve all documents) or a TDocVariant /
    // TBSONVariant query selector:
    // ! FindBSON(BSONVariant('{name:"John",age:{$gt:21}}'),null);
    // ! FindBSON(BSONVariant('{name:?,age:{$gt:?}}',[],['John',21]),null);
    // - Projection can be null (to retrieve all fields) or a CSV string to set
    // the field names to retrieve, or a TDocVariant or TBSONVariant - e.g.:
    // ! FindBSON(BSONVariant(['name','John']),null);
    // ! FindBSON(BSONVariant(['name','John']),'_id');
    // ! FindBSON(BSONVariant(['name','John']),BSONVariantFieldSelector('name,_id'));
    // - NumberToReturn can be left to its default maxInt value to return all
    // matching documents, or specify a limit (e.g. 1 for one document)
    function FindBSON(const Criteria, Projection: Variant;
      NumberToReturn: integer = maxInt; NumberToSkip: Integer = 0;
      Flags: TMongoQueryFlags = []): TBSONDocument;

    /// insert one document, supplied as (extended) JSON and parameters,
    // in the collection
    // - supplied JSON could be either strict or in MongoDB Shell syntax:
    // !   products.insert('{ _id: ?, item: ?, qty: ? }',[1,'card',15]);
    // !   // here _id is forced on the client side
    // !   products.insert('{ item: ?, qty: ? }',[1,'card',15]);
    // !   // here the _id will be created on the client side as an ObjectID
    // - you can retrieve the associated ObjectID, as such:
    // ! var oid: TBSONObjectID;
    // ! ...
    // !   products.insert('{ item: ?, qty: ? }',['card',15],@oid);
    // !   writeln(oid.ToText);
    procedure Insert(const Document: RawUTF8; const Params: array of const;
      DocumentObjectID: PBSONObjectID = nil); overload;
    /// insert one or more documents in the collection
    // - Documents is an array of TDocVariant (i.e. created via _JsonFast()
    // or _JsonFastFmt()) - or of TBSONVariant (created via BSONVariant())
    // - by default, it will follow Client.WriteConcern pattern - but you can
    // set NoAcknowledge = TRUE to avoid calling the getLastError command and
    // increase the execution speed, at the expense of a unsafe process
    procedure Insert(const Documents: array of variant;
      Flags: TMongoInsertFlags = []; NoAcknowledge: boolean = false); overload;
    /// insert one or more documents in the collection
    // - Documents is the low-level concatenation of BSON documents, created
    // e.g. with a TBSONWriter stream
    // - by default, it will follow Client.WriteConcern pattern - but you can
    // set NoAcknowledge = TRUE to avoid calling the getLastError command and
    // increase the execution speed, at the expense of a unsafe process
    procedure Insert(const Documents: TBSONDocument;
      Flags: TMongoInsertFlags = []; NoAcknowledge: boolean = false); overload;
    /// insert one or more documents in the collection
    // - JSONDocuments is an array of JSON objects
    // - by default, it will follow Client.WriteConcern pattern - but you can
    // set NoAcknowledge = TRUE to avoid calling the getLastError command and
    // increase the execution speed, at the expense of a unsafe process
    procedure InsertJSON(const JSONDocuments: array of PUTF8Char;
      Flags: TMongoInsertFlags = []; NoAcknowledge: boolean = false);

    /// updates an existing document or inserts a new document, depending on
    // its document parameter
    // - this document should be a TDocVariant (i.e. created via _JsonFast()
    // or _JsonFastFmt()) since we need to check for the _id field, other types
    // will be converted to a TDocVariant instance (via its JSON representation)
    // so it is pointless to use BSONVariant() here
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
      DocumentObjectID: PBSONObjectID = nil): boolean; overload;
    /// updates an existing document or inserts a new document, depending on
    // its document parameter, supplied as (extended) JSON and parameters
    // - supplied JSON could be either strict or in MongoDB Shell syntax:
    // - will perform either an insert or an update, depending of the
    // presence of the _id field, as overloaded Save(const Document: variant)
    procedure Save(const Document: RawUTF8; const Params: array of const;
      DocumentObjectID: PBSONObjectID = nil); overload;

    /// modifies an existing document or several documents in a collection
    // - the method can modify specific fields of existing document or documents
    // or replace an existing document entirely, depending on the update parameter
    // - Query and Update parameters should be TDocVariant (i.e. created via
    // _JsonFast() or _JsonFastFmt()) or TBSONVariant (created via BSONVariant())
    // - Query is the selection criteria for the update; use the same query
    // selectors as used in the Find() method
    // - if Update contains a plain document, it will replace any existing data
    // - if Update contains update operators (like $set), it will update the
    // corresponding fields in the document
    procedure Update(const Query, Update: variant;
      Flags: TMongoUpdateFlags = []); overload;
    /// modifies an existing document or several documents in a collection
    // - the method can modify specific fields of existing document or documents
    // or replace an existing document entirely, depending on the update parameter
    // - since all content will be transformed into JSON internally, use this
    // method only if the supplied parameters are simple types: any complex value
    // (e.g. a TDateTime or a BSONVariant binary) won't be handled as expected -
    // use the overloaded Update() with explicit BSONVariant() values instead
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
    procedure Update(Query: PUTF8Char; const QueryParams: array of const;
      const Update: RawUTF8; const UpdateParams: array of const;
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
    // _JsonFastFmt()) or TBSONVariant (created via BSONVariant())
    // - Query is the selection criteria for the deletion; use the same query
    // selectors as used in the Find() method
    // - to limit the deletion to just one document, set Flags to [mdfSingleRemove]
    // - to delete all documents matching the deletion criteria, leave it to []
    procedure Remove(const Query: variant; Flags: TMongoDeleteFlags = []); overload;
    /// delete an existing document in a collection, by its _id field
    // - _id will identify the unique document to be deleted
    procedure RemoveOne(const _id: TBSONObjectID); overload;
    /// delete an existing document in a collection, by its _id field
    // - _id will identify the unique document to be deleted
    procedure RemoveOne(const _id: variant); overload;
    /// delete an existing document or several documents in a collection
    // - Query parameter can be specified as JSON objects with parameters
    // - Query is the selection criteria for the deletion; use the same query
    // selectors as used in the Find() method
    // - to limit the deletion to just one document, set Flags to [mdfSingleRemove]
    // - to delete all documents matching the deletion criteria, leave it to []
    procedure RemoveFmt(Query: PUTF8Char; const QueryParams: array of const;
      Flags: TMongoDeleteFlags = []);

    /// creates an index on the specified field(s) if the index does
    // not already exist
    // - Keys and Options parameters should be TDocVariant (e.g. created via
    // _JsonFast() or _JsonFastFmt()) - and not TBSONVariant values
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
    procedure EnsureIndex(const Keys: array of RawUTF8;
      Ascending: boolean = true; Unique: boolean = false); overload;
    /// drops the entire collection from the database
    // - once dropped, this TMongoCollection instance will be freed: never
    // use this instance again after success (i.e. returned '')
    // - in case of error, a textual message will be returned as result
    // - once dropped, this collection will be removed from the parent
    // Database.Collection[] internal list
    // - Warning: this method obtains a write lock on the affected database
    // and will block other operations until it has completed
    function Drop: RawUTF8;

    /// calculate the number of documents in the collection
    // - be aware that this method may be somewhat slow for huge collections,
    // since a full scan of an index is to be performed: if your purpose is
    // to ensure that a collection contains items, use rather IsEmpty method
    function Count: Int64;
    /// calculate the number of documents in the collection that match
    // a specific query
    // - Criteria can specify the query selector as a BSONVariant/TDocVariant
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
    function FindCount(Criteria: PUTF8Char; const Args, Params: array of const;
      MaxNumberToReturn: integer = 0; NumberToSkip: Integer = 0): Int64; overload;
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
    function AggregateDoc(Operators: PUTF8Char;
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
    // ! AggregateJSON('{ $sort : { age : -1, posts: 1 } }',[])
    function AggregateJSON(Operators: PUTF8Char; const Params: array of const;
      Mode: TMongoJSONMode = modMongoStrict): RawUTF8; overload;
    /// calculate aggregate values using the MongoDB aggregation framework
    // and return the result as a TDocVariant instance
    // - overloaded method to specify the pipeline as a BSON raw document
    // as detailed by http://docs.mongodb.org/manual/core/aggregation-pipeline
    function AggregateDocFromVariant(const pipelineArray: variant): variant;
    /// calculate JSON aggregate values using the MongoDB aggregation framework
    // - overloaded method to specify the pipeline as a BSON raw document
    // as detailed by http://docs.mongodb.org/manual/core/aggregation-pipeline
    function AggregateJSONFromVariant(const pipelineArray: variant;
      Mode: TMongoJSONMode = modMongoStrict): RawUTF8; overload;
    /// calculate aggregate values using the MongoDB aggregation framework
    // and return the result as a TDocVariant instance
    // - overloaded method to specify the pipeline as a JSON text object
    // as detailed by http://docs.mongodb.org/manual/core/aggregation-pipeline
    // - for instance, the following will return the maximum _id value of
    // the collection:
    // ! AggregateDoc('{$group:{_id:null,max:{$max:"$_id"}}}').max
    function AggregateDocFromJson(const PipelineJSON: RawUTF8): variant;
    /// calculate JSON aggregate values using the MongoDB aggregation framework
    // - overloaded method to specify the pipeline as a JSON text object
    // as detailed by http://docs.mongodb.org/manual/core/aggregation-pipeline
    function AggregateJSONFromJson(const PipelineJSON: RawUTF8;
      Mode: TMongoJSONMode = modMongoStrict): RawUTF8; overload;
  published
    /// the collection name
    property Name: RawUTF8 read fName;
    /// the full collection name, e.g. 'dbname.collectionname'
    property FullCollectionName: RawUTF8 read fFullCollectionName;
    /// the associated MongoDB database instance
    property Database: TMongoDatabase read fDatabase;
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
    constructor CreateUTF8(const Format: RawUTF8; const Args: array of const;
      aConnection: TMongoConnection); reintroduce;
  published
    /// the associated connection
    property Connection: TMongoConnection read fConnection;
  end;

  EMongoDatabaseException = class(EMongoConnectionException)
  protected
    fDatabase: TMongoDatabase;
  public
    /// initialize the Exception for a given request
    constructor Create(const aMsg: string; aDatabase: TMongoDatabase);
      reintroduce; overload;
    /// initialize the Exception for a given request
    constructor CreateUTF8(const Format: RawUTF8; const Args: array of const;
      aDatabase: TMongoDatabase); reintroduce;
    {$ifndef NOEXCEPTIONINTERCEPT}
    /// used to customize the exception log to contain information about the Query
    // - it will log the database parameters
    function CustomLog(WR: TBaseWriter;
      const Context: TSynLogExceptionContext): boolean; override;
    {$endif NOEXCEPTIONINTERCEPT}
  published
    /// the associated Database
    property Database: TMongoDatabase read fDatabase;
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
    constructor CreateUTF8(const Format: RawUTF8; const Args: array of const;
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
    function CustomLog(WR: TBaseWriter;
      const Context: TSynLogExceptionContext): boolean; override;
    {$endif NOEXCEPTIONINTERCEPT}
    /// the associated error reply document
    property ErrorReply: TMongoReplyCursor read fError;
  published
    /// the associated error reply document, as a TDocVariant instance
    // - will return the first document available in ErrorReply, or the supplied
    // aErrorDoc: TDocVariantData instance
    property ErrorDoc: Variant read GetErrorDoc;
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
    property SystemLastError: cardinal read fSystemLastError;
  end;

{$M-}


/// ready-to-be displayed text of a TMongoClientWriteConcern item
function ToText(wc: TMongoClientWriteConcern): PShortString; overload;

/// ready-to-be displayed text of a TMongoClientReplicaSetReadPreference item
function ToText(pref: TMongoClientReplicaSetReadPreference): PShortString; overload;



implementation

{ ************ MongoDB Protocol Items }

{ TMongoRequest }

const
  WIRE_OPCODES: array[TMongoOperation] of integer = (
    1, 1000, 2001, 2002, 2004, 2005, 2006, 2007, 2013);

  CLIENT_OPCODES =
    [opUpdate, opInsert, opQuery, opGetMore, opDelete, opKillCursors];

var
  GlobalRequestID: Integer;

constructor TMongoRequest.Create(const FullCollectionName: RawUTF8;
  opCode: TMongoOperation; requestID, responseTo: Integer);
begin
  if not (opCode in CLIENT_OPCODES) then
    raise EMongoException.CreateUTF8('Unexpected %.Create(opCode=%)',
      [self, ToText(opCode)^]);
  inherited Create(TRawByteStringStream);
  fFullCollectionName := FullCollectionName;
  Split(fFullCollectionName, '.', fDatabaseName, fCollectionName);
  if requestID = 0 then
    fRequestID := InterlockedIncrement(GlobalRequestID)
  else
    fRequestID := requestID;
  fResponseTo := responseTo;
  BSONDocumentBegin;
  fRequestOpCode := opCode;
  Write4(fRequestID);
  Write4(fResponseTo);
  Write4(WIRE_OPCODES[opCode]);
end;

procedure TMongoRequest.BSONWriteParam(const paramDoc: variant);
begin
  if TVarData(paramDoc).VType = varByRef or varVariant then
    BSONWriteParam(PVariant(TVarData(paramDoc).VPointer)^)
  else if VarIsStr(paramDoc) then
    BSONWriteProjection(VariantToUTF8(paramDoc))
  else if (TVarData(paramDoc).VType = BSONVariantType.VarType) and
          (TBSONVariantData(paramDoc).VKind in [betDoc, betArray]) and
          (TBSONVariantData(paramDoc).VBlob <> nil) then
    WriteBinary(RawByteString(TBSONVariantData(paramDoc).VBlob))
  else
    BSONWriteDoc(TDocVariantData(paramDoc)); // for TDocVariant or null
end;

procedure TMongoRequest.ToBSONDocument(var result: TBSONDocument);
begin
  if (fRequestID = 0) or
     (fRequestOpCode = opReply) then
    raise EMongoException.CreateUTF8('No previous proper %.Create() call', [self]);
  if fBSONDocument = '' then
  begin
    BSONDocumentEnd(1, false);
    inherited ToBSONDocument(fBSONDocument);
  end;
  result := fBSONDocument;
end;

procedure TMongoRequest.ToJSON(W: TTextWriter; Mode: TMongoJSONMode);
begin
  if self = nil then
  begin
    W.AddShort('null');
    exit;
  end;
  W.Add('{');
  W.AddShort('collection:"');
  W.AddJSONEscape(pointer(fFullCollectionName));
  W.AddShort('",opCode:');
  W.AddTypedJSON(@fRequestOpCode, TypeInfo(TMongoOperation));
  W.AddShort(',requestID:');
  W.AddU(fRequestID);
  if fResponseTo <> 0 then
  begin
    W.AddShort(',responseTo:');
    W.AddU(fResponseTo);
  end;
  W.Add('}');
end;

function TMongoRequest.ToJSON(Mode: TMongoJSONMode): RawUTF8;
var
  W: TTextWriter;
  tmp: TTextWriterStackBuffer;
begin
  W := TTextWriter.CreateOwnedStream(tmp);
  try
    ToJSON(W, Mode);
    W.SetText(result);
  finally
    W.Free;
  end;
end;


{ TMongoRequestUpdate }

constructor TMongoRequestUpdate.Create(const FullCollectionName: RawUTF8;
  const Selector, Update: variant; Flags: TMongoUpdateFlags);
begin
  inherited Create(FullCollectionName, opUpdate, 0, 0);
  fSelector := TVarData(Selector);
  fUpdate := TVarData(Update);
  WriteCollectionName(0, FullCollectionName);
  Write4(byte(Flags));
  BSONWriteParam(Selector);
  BSONWriteParam(Update);
end;

procedure TMongoRequestUpdate.ToJSON(W: TTextWriter; Mode: TMongoJSONMode);
begin
  inherited;
  W.CancelLastChar('}');
  W.AddShort(',selector:');
  AddMongoJSON(variant(fSelector), W, modMongoShell);
  W.AddShort(',update:');
  AddMongoJSON(variant(fUpdate), W, modMongoShell);
  W.Add('}');
end;


{ TMongoRequestInsert }

constructor TMongoRequestInsert.Create(const FullCollectionName: RawUTF8;
  const Documents: array of variant; Flags: TMongoInsertFlags);
var
  i: PtrInt;
begin
  inherited Create(FullCollectionName, opInsert, 0, 0);
  WriteCollectionName(byte(Flags), FullCollectionName);
  for i := 0 to high(Documents) do
    BSONWriteParam(Documents[i]);
end;

constructor TMongoRequestInsert.Create(const FullCollectionName: RawUTF8;
  const Documents: TBSONDocument; Flags: TMongoInsertFlags);
begin
  inherited Create(FullCollectionName, opInsert, 0, 0);
  WriteCollectionName(byte(Flags), FullCollectionName);
  Write(pointer(Documents), Length(Documents));
end;

constructor TMongoRequestInsert.Create(const FullCollectionName: RawUTF8;
  const JSONDocuments: array of PUTF8Char; Flags: TMongoInsertFlags);
var
  i: PtrInt;
  kind: TBSONElementType;
begin
  inherited Create(FullCollectionName, opInsert, 0, 0);
  WriteCollectionName(byte(Flags), FullCollectionName);
  for i := 0 to high(JSONDocuments) do
    BSONWriteDocFromJSON(JSONDocuments[i], nil, kind);
end;


{ TMongoRequestDelete }

constructor TMongoRequestDelete.Create(const FullCollectionName: RawUTF8;
  const Selector: variant; Flags: TMongoDeleteFlags);
begin
  inherited Create(FullCollectionName, opDelete, 0, 0);
  fQuery := TVarData(Selector);
  WriteCollectionName(byte(Flags), FullCollectionName);
  Write4(byte(Flags));
  BSONWriteParam(Selector);
end;

procedure TMongoRequestDelete.ToJSON(W: TTextWriter; Mode: TMongoJSONMode);
begin
  inherited;
  W.CancelLastChar('}');
  W.AddShort(',query:');
  AddMongoJSON(variant(fQuery), W, modMongoShell);
  W.Add('}');
end;


{ TMongoRequestQuery }

constructor TMongoRequestQuery.Create(const FullCollectionName: RawUTF8;
  const Query, ReturnFieldsSelector: variant;
  NumberToReturn, NumberToSkip: Integer; Flags: TMongoQueryFlags);
begin
  inherited Create(FullCollectionName, opQuery, 0, 0);
  fNumberToReturn := NumberToReturn;
  fNumberToSkip := NumberToSkip;
  fQuery := TVarData(Query);
  fReturnFieldsSelector := TVarData(ReturnFieldsSelector);
  WriteCollectionName(byte(Flags), FullCollectionName);
  Write4(NumberToSkip);
  Write4(NumberToReturn);
  BSONWriteParam(Query);
  if TVarData(ReturnFieldsSelector).VType > varNull then
    BSONWriteParam(ReturnFieldsSelector);
end;

procedure TMongoRequestQuery.ToJSON(W: TTextWriter; Mode: TMongoJSONMode);
begin
  inherited;
  W.CancelLastChar('}');
  W.AddShort(',query:');
  AddMongoJSON(variant(fQuery), W, modMongoShell);
  if fReturnFieldsSelector.VType <> varNull then
  begin
    W.AddShort(',projection:');
    AddMongoJSON(variant(fReturnFieldsSelector), W, modMongoShell);
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

constructor TMongoRequestGetMore.Create(const FullCollectionName: RawUTF8;
  NumberToReturn: integer; CursorID: Int64);
begin
  inherited Create(FullCollectionName, opGetMore, 0, 0);
  WriteCollectionName(0, FullCollectionName);
  Write4(NumberToReturn);
  Write8(@CursorID);
end;


{ TMongoRequestKillCursor }

constructor TMongoRequestKillCursor.Create(const FullCollectionName: RawUTF8;
  const CursorIDs: array of Int64);
var
  n: integer;
begin
  if high(CursorIDs) < 0 then
    raise EMongoException.CreateUTF8('Invalid %.Create([]) call', [self]);
  inherited Create(FullCollectionName, opKillCursors, 0, 0);
  Write4(0); // reserved for future use
  n := length(CursorIDs);
  Write4(n);
  SetLength(fCursors, n);
  n := n * sizeof(Int64);
  MoveFast(CursorIDs[0], fCursors[0], n);
  Write(pointer(fCursors), n);
end;

procedure TMongoRequestKillCursor.ToJSON(W: TTextWriter; Mode: TMongoJSONMode);
var
  i: integer;
begin
  inherited;
  W.CancelLastChar('}');
  W.AddShort(',cursorID:[');
  for i := 0 to high(fCursors) do
  begin
    W.Add(fCursors[i]);
    W.Add(',');
  end;
  W.CancelLastComma;
  W.Add(']', '}');
end;


{ TMongoReplyCursor }

procedure TMongoReplyCursor.Init(const ReplyMessage: TMongoReply);
var
  Len: integer;
begin
  Len := length(ReplyMessage);
  with PMongoReplyHeader(ReplyMessage)^ do
  begin
    if (Len < sizeof(TMongoReplyHeader)) or
       (Header.MessageLength <> Len) or
       (sizeof(TMongoReplyHeader) + NumberReturned * 5 > Len) then
      raise EMongoException.CreateUTF8('TMongoReplyCursor.Init(len=%)', [Len]);
    if Header.OpCode <> WIRE_OPCODES[opReply] then
      raise EMongoException.CreateUTF8('TMongoReplyCursor.Init(OpCode=%)', [Header.OpCode]);
    fRequestID := requestID;
    fResponseTo := responseTo;
    byte(fResponseFlags) := ResponseFlags;
    fCursorID := CursorID;
    fStartingFrom := StartingFrom;
    fNumberReturned := NumberReturned;
  end;
  fReply := ReplyMessage;
  fFirstDocument := PAnsiChar(pointer(fReply)) + sizeof(TMongoReplyHeader);
  Rewind;
  fLatestDocIndex := -1;
end;

procedure TMongoReplyCursor.ComputeDocumentsList;
var
  i, Len: integer;
  P: PAnsiChar;
begin
  if fDocuments <> nil then
    exit;
  Len := length(fReply);
  SetLength(fDocuments, DocumentCount);
  P := fFirstDocument;
  for i := 0 to DocumentCount - 1 do
  begin
    fDocuments[i] := P;
    inc(P, PInteger(P)^); // fast "parsing" of all supplied documents
    if P - pointer(fReply) > Len then
      raise EMongoException.CreateUTF8('ComputeDocumentsList(Document[%])', [i]);
  end;
  if P - pointer(fReply) <> Len then
    raise EMongoException.Create('ComputeDocumentsList(Documents)');
end;

function TMongoReplyCursor.GetOneDocument(index: integer): variant;
begin
  if fLatestDocIndex <> index then
  begin // naive but efficient cache
    GetDocument(index, fLatestDocValue);
    fLatestDocIndex := index;
  end;
  result := fLatestDocValue;
end;

procedure TMongoReplyCursor.GetDocument(index: integer; var result: variant);
begin
  if cardinal(index) >= cardinal(length(fDocuments)) then
    raise EMongoException.CreateUTF8('TMongoReplyCursor.GetDocument(index %>=%)',
      [index, length(fDocuments)]);
  if fDocuments = nil then
    ComputeDocumentsList;
  BSONToDoc(fDocuments[index], result, 0, asDocVariantPerReference);
end;

function TMongoReplyCursor.Next(out doc: variant;
  option: TBSONDocArrayConversion): boolean;
var
  b: PByte;
begin
  if Next(b) then
  begin
    BSONToDoc(b, doc, 0, option);
    result := true;
  end
  else
    result := false;
end;

function TMongoReplyCursor.Next(out doc: PByte): boolean;
begin
  if fCurrentPosition < DocumentCount then
  begin
    doc := PByte(fCurrentDocument);
    inc(fCurrentDocument, PInteger(fCurrentDocument)^);
    inc(fCurrentPosition);
    result := true;
  end
  else
  begin
    doc := nil;
    result := false;
  end;
end;

function TMongoReplyCursor.Next(out BSON: TBSONDocument): boolean;
var
  b: PByte;
begin
  if Next(b) then
  begin
    SetString(BSON, PAnsiChar(b) + 4, PInteger(b)^);
    result := true;
  end
  else
    result := false;
end;

function TMongoReplyCursor.Next(out JSON: RawUTF8; Mode: TMongoJSONMode): boolean;
var
  b: PByte;
begin
  if Next(b) then
  begin
    JSON := BSONToJSON(b, betDoc, 0, Mode);
    result := true;
  end
  else
    result := false;
end;

procedure TMongoReplyCursor.Rewind;
begin
  fCurrentPosition := 0;
  fCurrentDocument := fFirstDocument;
end;

function TMongoReplyCursor.AppendAllToDocVariantDynArray(
  var Dest: TVariantDynArray): integer;
var
  b: PByte;
begin
  result := length(Dest);
  if (fReply = '') or
     (DocumentCount <= 0) then
    exit; // nothing to append
  SetLength(Dest, result + DocumentCount);
  Rewind;
  while Next(b) do
  begin
    BSONToDoc(b, Dest[result], 0, asDocVariantPerReference);
    inc(result);
  end;
  if result <> length(Dest) then
    raise EMongoException.Create('Invalid opReply Documents');
end;

procedure TMongoReplyCursor.AppendAllToBSON(Dest: TBSONWriter);
var
  name: RawUTF8;
  i: integer;
  P: PAnsiChar;
begin
  P := FirstDocument;
  for i := 1 to DocumentCount do
  begin
    UInt32ToUtf8(Dest.Tag, name); // Dest.Tag = item number in array
    Dest.Tag := Dest.Tag + 1;
    Dest.BSONWrite(name, betDoc);
    Dest.Write(P, PInteger(P)^);
    inc(P, PInteger(P)^);
  end;
end;

function TMongoReplyCursor.AppendAllToDocVariant(var Dest: TDocVariantData): integer;
var
  item: variant;
begin
  if Dest.VarType <> DocVariantType.VarType then
    TDocVariant.New(Variant(Dest), JSON_OPTIONS_FAST);
  result := Dest.Count;
  if (fReply = '') or
     (DocumentCount <= 0) then
    exit; // nothing to append
  inc(result, DocumentCount);
  Dest.Capacity := result;
  Rewind;
  while Next(item) do
    Dest.AddItem(item{%H-});
  if Dest.Count <> result then
    raise EMongoException.Create('Invalid opReply Documents');
end;

procedure TMongoReplyCursor.FetchAllToJSON(W: TTextWriter; Mode: TMongoJSONMode;
  WithHeader: boolean; MaxSize: Cardinal);
var
  b: PByte;
begin
  if (fReply = '') or
     (DocumentCount <= 0) then
  begin
    W.AddShort('null');
    exit;
  end;
  if WithHeader and
     (Mode = modMongoShell) then
    W.Add('{ReplyHeader:{ResponseFlags:%,RequestID:%,ResponseTo:%,CursorID:%,' +
      'StartingFrom:%,NumberReturned:%,ReplyDocuments:[', [byte(ResponseFlags),
      requestID, responseTo, CursorID, StartingFrom, DocumentCount]);
  Rewind;
  while Next(b) do
  begin
    inc(b, sizeof(integer)); // points to the "e_list" of "int32 e_list #0"
    BSONListToJSON(b, betDoc, W, Mode);
    W.Add(',');
    if (MaxSize > 0) and
       (W.TextLength > MaxSize) then
    begin
      W.AddShort('...');
      break;
    end;
  end;
  W.CancelLastComma;
  if WithHeader then
    W.Add(']', '}');
end;

function TMongoReplyCursor.ToJSON(Mode: TMongoJSONMode; WithHeader: boolean;
  MaxSize: Cardinal): RawUTF8;
var
  W: TTextWriter;
  tmp: TTextWriterStackBuffer;
begin
  if (fReply = '') or
     (DocumentCount <= 0) then
    result := 'null'
  else
  begin
    W := TTextWriter.CreateOwnedStream(tmp);
    try
      FetchAllToJSON(W, Mode, WithHeader, MaxSize);
      W.SetText(result);
    finally
      W.Free;
    end;
  end;
end;

function ToText(op: TMongoOperation): PShortString;
begin
  result := GetEnumName(TypeInfo(TMongoOperation), ord(op));
end;



{ ************ MongoDB Client Classes }


{ TMongoConnection }

const
  /// message big enough to retrieve the maximum MongoDB document size
  MONGODB_MAXMESSAGESIZE = BSON_MAXDOCUMENTSIZE + sizeof(TMongoReplyHeader);

constructor TMongoConnection.Create(const aClient: TMongoClient;
  const aServerAddress: RawUTF8; aServerPort: integer);
begin
  if aClient = nil then
    raise EMongoException.CreateUTF8('%.Create(nil)', [self]);
  fClient := aClient;
  fServerAddress := trim(aServerAddress);
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
    fSocket := TCrtSocket.Open(fServerAddress, UInt32ToUtf8(fServerPort), nlTCP,
      Client.ConnectionTimeOut, Client.ConnectionTLS);
  except
    on E: Exception do
      raise EMongoException.CreateUTF8('%.Open unable to connect to MongoDB server %: % [%]',
        [self, Client.ConnectionString, E, E.Message]);
  end;
  fSocket.TCPNoDelay := true; // we buffer all output data before sending
  fSocket.KeepAlive := true;  // do not close the connection without notice
end;

function TMongoConnection.GetOpened: boolean;
begin
  result := (self <> nil) and
            (fSocket <> nil);
end;

procedure TMongoConnection.Close;
begin
  FreeAndNil(fSocket);
end;

procedure TMongoConnection.GetDocumentsAndFree(Query: TMongoRequestQuery; var
  result: TVariantDynArray);
begin
  result := nil;
  GetRepliesAndFree(Query, ReplyDocVariant, result);
end;

procedure TMongoConnection.GetDocumentsAndFree(Query: TMongoRequestQuery; var
  result: variant);
var
  ForceOneInstance: boolean;
  docs: TVariantDynArray;
begin
  ForceOneInstance := Query.NumberToReturn = 1;
  SetVariantNull(result);
  GetRepliesAndFree(Query, ReplyDocVariant, docs);
  if docs <> nil then
    if ForceOneInstance then
      result := docs[0]
    else
      TDocVariantData(result).InitArrayFromVariants(docs, JSON_OPTIONS_FAST);
end;

function TMongoConnection.GetDocumentsAndFree(Query: TMongoRequestQuery): variant;
begin
  GetDocumentsAndFree(Query, result);
end;

function TMongoConnection.GetBSONAndFree(Query: TMongoRequestQuery): TBSONDocument;
var
  W: TBSONWriter;
begin
  W := TBSONWriter.Create(TRawByteStringStream);
  try
    W.BSONDocumentBegin;
    GetRepliesAndFree(Query, ReplyBSON, W); // W.Tag = item number in array
    W.BSONDocumentEnd;
    W.ToBSONDocument(result);
  finally
    W.Free;
  end;
end;

function TMongoConnection.GetJSONAndFree(Query: TMongoRequestQuery; Mode:
  TMongoJSONMode): RawUTF8;
var
  W: TTextWriter;
  ReturnAsJSONArray: boolean;
  tmp: TTextWriterStackBuffer;
begin
  ReturnAsJSONArray := Query.NumberToReturn > 1;
  W := TTextWriter.CreateOwnedStream(tmp);
  try
    if ReturnAsJSONArray then
      W.Add('[');
    case Mode of
      modNoMongo:
        GetRepliesAndFree(Query, ReplyJSONNoMongo, W);
      modMongoStrict:
        GetRepliesAndFree(Query, ReplyJSONStrict, W);
      modMongoShell:
        GetRepliesAndFree(Query, ReplyJSONExtended, W);
    end;
    W.CancelLastComma;
    if ReturnAsJSONArray then
      W.Add(']');
    W.SetText(result);
    if (result = '') or
       (result = '[]') or
       (result = '{}') then
      result := 'null';
  finally
    W.Free;
  end;
end;

procedure TMongoConnection.GetRepliesAndFree(Query: TMongoRequestQuery;
  OnEachReply: TOnMongoConnectionReply; var Opaque);
var
  main, more: TMongoReplyCursor;
  getMore: TMongoRequestGetMore;
  count: integer;
  cursorID: Int64;
begin
  try
    if not Assigned(Query) then
      raise EMongoRequestException.Create('Query=nil', self);
    if not Assigned(OnEachReply) then
      raise EMongoRequestException.Create('OnEachReply=nil', self, Query);
    count := Query.NumberToReturn; // 0 means default return size
    GetCursor(Query, main);
    if main.DocumentCount > 0 then
    begin
      OnEachReply(Query, main, Opaque);
      if count > 0 then
        dec(count, main.DocumentCount);
    end;
    cursorID := main.CursorID;
    if cursorID <> 0 then
      if (Query.NumberToReturn = 0) or
         ((Query.NumberToReturn > 0) and
          (count > 0)) then
        repeat
          getMore := TMongoRequestGetMore.Create(
            Query.FullCollectionName, count, cursorID);
          try
            GetCursor(getMore, more);
            if mrfCursorNotFound in more.ResponseFlags then
              raise EMongoRequestException.Create('GetMore cursor not found',
                self, Query, more);
            if more.DocumentCount > 0 then
            begin
              OnEachReply(Query, more, Opaque);
              dec(count, more.DocumentCount);
            end;
            cursorID := more.CursorID;
          finally
            getMore.Free;
          end;
        until ((Query.NumberToReturn > 0) and
               (count <= 0)) or
              (cursorID = 0);
    if cursorID <> 0 then // if cursor not exhausted: need to kill it
      SendAndFree(TMongoRequestKillCursor.Create(
        Query.FullCollectionName, [cursorID]), true);
  finally
    Query.Free;
  end;
end;

procedure TMongoConnection.ReplyDocVariant(Request: TMongoRequest;
  const Reply: TMongoReplyCursor; var Opaque);
begin
  Reply.AppendAllToDocVariantDynArray(TVariantDynArray(Opaque));
end;

procedure TMongoConnection.ReplyJSONStrict(Request: TMongoRequest;
  const Reply: TMongoReplyCursor; var Opaque);
var
  W: TTextWriter absolute Opaque;
begin
  Reply.FetchAllToJSON(W, modMongoStrict, false);
  W.Add(',');
end;

procedure TMongoConnection.ReplyJSONExtended(Request: TMongoRequest;
  const Reply: TMongoReplyCursor; var Opaque);
var
  W: TTextWriter absolute Opaque;
begin
  Reply.FetchAllToJSON(W, modMongoShell, false);
  W.Add(',');
end;

procedure TMongoConnection.ReplyJSONNoMongo(Request: TMongoRequest;
  const Reply: TMongoReplyCursor; var Opaque);
var
  W: TTextWriter absolute Opaque;
begin
  Reply.FetchAllToJSON(W, modNoMongo, false);
  W.Add(',');
end;

procedure TMongoConnection.ReplyBSON(Request: TMongoRequest;
  const Reply: TMongoReplyCursor; var Opaque);
begin
  Reply.AppendAllToBSON(TBSONWriter(Opaque));
end;

function TMongoConnection.Send(Request: TMongoRequest): boolean;
var
  doc: TBSONDocument;
begin
  if not Opened and
     not Client.ReOpen then
    raise EMongoRequestException.Create('Send: Missing Open', self, Request);
  if Request = nil then
    raise EMongoRequestException.Create('Send(nil)', self);
  Request.ToBSONDocument(doc);
  if (Client.LogRequestEvent <> sllNone) and
     (Client.Log <> nil) and
     (Client.LogRequestEvent in Client.Log.Family.Level) then
    Client.Log.Log(Client.fLogRequestEvent, Request.ToJSON(modMongoShell), Request);
  result := fSocket.TrySndLow(pointer(doc), length(doc));
end;

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
            cmd := BSONVariant(['getLastError', 1, 'j', true]);
          wcReplicaAcknowledged:
            cmd := BSONVariant(['getLastError', 1, 'w', 2]);
        else
          raise EMongoRequestException.CreateUTF8('%.SendAndFree WriteConcern=%',
            [self, ord(Client.WriteConcern)], self, Request);
        end;
        RunCommand(Request.DatabaseName, cmd, result);
        if not VarIsNull(result.err) then
          raise EMongoRequestException.Create('SendAndFree', self, Request,
            TDocVariantData(result));
      end
      else // socket error on sending
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

procedure TMongoConnection.GetCursor(Request: TMongoRequest;
  var Result: TMongoReplyCursor);
var
  reply: TMongoReply;
begin
  GetReply(Request, reply);
  Result.Init(reply);
  if (Client.LogReplyEvent <> sllNone) and
     (Client.Log <> nil) and
     (Client.LogReplyEvent in Client.Log.Family.Level) then
    Client.Log.Log(Client.LogReplyEvent, Result.ToJSON(modMongoShell, True,
      Client.LogReplyEventMaxSize), Request);
  if mrfQueryFailure in Result.ResponseFlags then
    raise EMongoRequestException.Create('Query failure', self, Request, Result);
end;

const
  RECV_ERROR = '%.GetReply(%): Server response timeout or connection broken, ' +
    'probably due to a bad formatted BSON request -> close socket';

procedure TMongoConnection.GetReply(Request: TMongoRequest; out result: TMongoReply);
var
  Header: TMongoWireHeader;
  HeaderLen, DataLen: integer;
begin
  if self = nil then
    raise EMongoRequestException.Create('Connection=nil', self, Request);
  FillCharFast(Header, sizeof(Header), 0);
  try
    Lock;
    if Send(Request) then
    begin
      HeaderLen := SizeOf(Header);
      if not fSocket.TrySockRecv(@Header, HeaderLen) then
      try
        Close;
      finally
        raise EMongoRequestException.CreateUTF8(RECV_ERROR, [self, 'hdr'], self, Request);
      end;
      if Header.MessageLength > MONGODB_MAXMESSAGESIZE then
        raise EMongoRequestException.CreateUTF8('%.GetReply: MessageLength=%',
          [self, Header.MessageLength], self, Request);
      SetLength(result, Header.MessageLength);
      PMongoWireHeader(result)^ := Header;
      DataLen := Header.MessageLength - sizeof(Header);
      if not fSocket.TrySockRecv(@PByteArray(result)[sizeof(Header)], DataLen) then
      try
        Close;
      finally
        raise EMongoRequestException.CreateUTF8(RECV_ERROR, [self, 'msg'], self, Request);
      end;
      if Header.ResponseTo = Request.MongoRequestID then
        exit; // success
      case Header.OpCode of
        ord(opMsgOld):
          if Client.Log <> nil then
            Client.Log.Log(sllWarning, 'Msg (deprecated) from MongoDB: %',
              [BSONToJSON(@PByteArray(result)[sizeof(Header)], betDoc, DataLen,
               modMongoShell)], Request);
        ord(opMsg):
          // TODO: parse https://docs.mongodb.com/manual/reference/mongodb-wire-protocol/#op-msg
          if Client.Log <> nil then
            Client.Log.Log(sllWarning, 'Msg from MongoDB: %',
              [EscapeToShort(@PByteArray(result)[sizeof(Header)], DataLen)], Request);
      end;
    end;
    // if we reached here, this is due to a socket error or an unexpeted opcode
    raise EMongoRequestException.CreateUTF8('%.GetReply: OpCode=% and ResponseTo=% (expected:%)',
      [self, Header.OpCode, Header.ResponseTo, Request.MongoRequestID], self, Request);
  finally
    UnLock;
  end;
end;

function TMongoConnection.RunCommand(const aDatabaseName: RawUTF8;
  const command: variant; var returnedValue: variant): RawUTF8;
begin
  GetDocumentsAndFree(TMongoRequestQuery.Create(
    aDatabaseName + '.$cmd', command, null, 1), returnedValue);
  with _Safe(returnedValue)^ do
    if GetValueOrDefault('ok', 1) <> 0 then
      result := ''
    else if not GetAsRawUTF8('errmsg', result) then
      result := 'unspecified error';
end;

function TMongoConnection.RunCommand(const aDatabaseName: RawUTF8;
  const command: variant; var returnedValue: TBSONDocument): boolean;
var
  item: TBSONElement;
begin
  returnedValue := GetBSONAndFree(TMongoRequestQuery.Create(
    aDatabaseName + '.$cmd', command, null, 1));
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

constructor EMongoConnectionException.CreateUTF8(const Format: RawUTF8;
  const Args: array of const; aConnection: TMongoConnection);
begin
  inherited CreateUTF8(Format, Args);
  fConnection := aConnection;
end;


{ EMongoRequestException }

constructor EMongoRequestException.Create(const aMsg: string;
  aConnection: TMongoConnection; aRequest: TMongoRequest);
begin
  inherited Create(aMsg, aConnection);
  fRequest := aRequest;
end;

constructor EMongoRequestException.CreateUTF8(const Format: RawUTF8;
  const Args: array of const; aConnection: TMongoConnection; aRequest: TMongoRequest);
begin
  inherited CreateUTF8(Format, Args, aConnection);
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
function EMongoRequestException.CustomLog(WR: TBaseWriter;
  const Context: TSynLogExceptionContext): boolean;
begin
  inherited CustomLog(WR, Context);
  if fRequest <> nil then
  begin
    WR.AddInstanceName(fRequest, ':');
    if WR.InheritsFrom(TTextWriter) then
      fRequest.ToJSON(TTextWriter(WR), modMongoShell)
    else
      WR.AddNull;
  end;
  if (fError.Reply <> '') and
     WR.InheritsFrom(TTextWriter) then
    fError.FetchAllToJSON(TTextWriter(WR), modMongoShell, True);
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
  CreateUTF8('%: % (%)', [aMsg, SysErrorMessage(fSystemLastError),
    fSystemLastError], aConnection, aRequest);
end;


{ EMongoDatabaseException }

constructor EMongoDatabaseException.Create(const aMsg: string;
  aDatabase: TMongoDatabase);
begin
  inherited Create(aMsg, aDatabase.Client.Connections[0]);
  fDatabase := aDatabase;
end;

constructor EMongoDatabaseException.CreateUTF8(const Format: RawUTF8;
  const Args: array of const; aDatabase: TMongoDatabase);
begin
  inherited CreateUTF8(Format, Args, aDatabase.Client.Connections[0]);
  fDatabase := aDatabase;
end;

{$ifndef NOEXCEPTIONINTERCEPT}
function EMongoDatabaseException.CustomLog(WR: TBaseWriter;
  const Context: TSynLogExceptionContext): boolean;
begin
  inherited CustomLog(WR, Context);
  if WR.InheritsFrom(TTextWriter) then
    TTextWriter(WR).AddJSONEscape(['Database', fDatabase.Name]);
  result := false; // log stack trace
end;
{$endif NOEXCEPTIONINTERCEPT}


{ TMongoClient }

constructor TMongoClient.Create(const Host: RawUTF8; Port: Integer;
  aTLS: boolean; const SecondaryHostCSV, SecondaryPortCSV: RawUTF8);
const
  PROT: array[boolean] of string[1] = (
    '', 's');
var
  secHost: TRawUTF8DynArray;
  secPort: TIntegerDynArray;
  nHost, i: integer;
begin
  fConnectionTimeOut := 30000;
  fConnectionTLS := aTLS;
  fLogReplyEventMaxSize := 1024;
  fGracefulReconnect.Enabled := true;
  FormatUTF8('mongodb%://%:%', [PROT[aTLS], Host, Port], fConnectionString);
  CSVToRawUTF8DynArray(pointer(SecondaryHostCSV), secHost);
  nHost := length(secHost);
  SetLength(fConnections, nHost + 1);
  fConnections[0] := TMongoConnection.Create(self, Host, Port);
  if nHost > 0 then
  begin
    CSVToIntegerDynArray(pointer(SecondaryPortCSV), secPort);
    for i := 0 to nHost - 1 do
    begin
      if i > high(secPort) then
        Port := MONGODB_DEFAULTPORT
      else
        Port := secPort[i];
      fConnections[i + 1] := TMongoConnection.Create(self, secHost[i], Port);
      fConnectionString := FormatUTF8('%,%:%', [fConnectionString, secHost[i], Port]);
    end;
  end;
  fDatabases := TRawUTF8List.Create([fObjectsOwned, fNoDuplicate, fCaseSensitive]);
end;

destructor TMongoClient.Destroy;
var
  i: PtrInt;
begin
  for i := 0 to high(fConnections) do
    FreeAndNil(fConnections[i]);
  FreeAndNil(fDatabases);
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

function TMongoClient.ServerBuildInfoText: RawUTF8;
begin
  with _Safe(ServerBuildInfo)^ do
    if count = 0 then
      result := ''
    else
    begin
      FormatUTF8('MongoDB % %', [U['version'], U['javascriptEngine']], result);
      with A['storageEngines']^ do
      begin
        // "storageEngines":["devnull","ephemeralForTest","mmapv1","wiredTiger"]
        DeleteByValue('devnull');
        DeleteByValue('ephemeralForTest');
        if count > 0 then
          result := result + ' ' + ToCSV;
      end;
    end;
end;

function TMongoClient.GetOneReadConnection: TMongoConnection;

  function GetUnlockedSecondaryIndex: integer;
  var
    retry: integer;
  begin
    if Length(fConnections) = 1 then // no secondary? use primary
      result := 0
    else
    begin
      for retry := 1 to 100 do
      begin // search for an inactive connection
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

begin
  case ReadPreference of
    rpPrimaryPreferred:
      if fConnections[0].Locked then
        result := fConnections[GetUnlockedSecondaryIndex]
      else
        result := fConnections[0];
    rpSecondary, rpSecondaryPreferred:
      result := fConnections[GetUnlockedSecondaryIndex];
  else // rpPrimary:
    result := fConnections[0];
  end;
end;

function TMongoClient.Open(const DatabaseName: RawUTF8): TMongoDatabase;
begin
  if self = nil then
    result := nil
  else
  begin
    result := fDatabases.GetObjectFrom(DatabaseName);
    if result = nil then
    begin // not already opened -> try now from primary host
      if not fConnections[0].Opened then
      begin
        fConnections[0].Open;
        AfterOpen;
      end;
      result := TMongoDatabase.Create(Self, DatabaseName);
      fDatabases.AddObjectUnique(DatabaseName, @result);
    end;
  end;
end;

function PasswordDigest(const UserName, Password: RawUTF8): RawUTF8;
begin
  result := MD5(UserName + ':mongo:' + Password);
end;

function TMongoClient.OpenAuth(const DatabaseName, UserName, PassWord: RawUTF8;
  ForceMongoDBCR: boolean): TMongoDatabase;
var
  digest: RawByteString;
begin
  if (self = nil) or
     (DatabaseName = '') or
     (UserName = '') or
     (PassWord = '') then
    raise EMongoException.CreateUTF8('Invalid %.OpenAuth("%") call',
      [self, DatabaseName]);
  result := fDatabases.GetObjectFrom(DatabaseName);
  if result = nil then  // not already opened -> try now from primary host
  try // note: authentication works on a single database per socket connection
    if not fConnections[0].Opened then
    try
      fConnections[0].Open; // socket connection
      AfterOpen; // need ServerBuildInfoNumber just below
      digest := PasswordDigest(UserName, PassWord);
      Auth(DatabaseName, UserName, digest, ForceMongoDBCR);
      with fGracefulReconnect do
        if Enabled and
           (EncryptedDigest = '') then
        begin
          ForcedDBCR := ForceMongoDBCR;
          User := UserName;
          Database := DatabaseName;
          EncryptedDigest := CryptDataForCurrentUser(digest, Database, true);
        end;
    except
      fConnections[0].Close;
      raise;
    end;
    result := TMongoDatabase.Create(Self, DatabaseName);
    fDatabases.AddObjectUnique(DatabaseName, @result);
  finally
    FillZero(digest);
  end;
end;

procedure TMongoClient.Auth(const DatabaseName, UserName, Digest: RawUTF8;
  ForceMongoDBCR: boolean);
var
  res, bson: variant;
  err, nonce, first, key, user, msg, rnonce: RawUTF8;
  payload: RawByteString;
  rnd: TAESBlock;
  sha: TSHA1;
  salted, client, stored, server: TSHA1Digest;
  resp: TDocVariantData;

  procedure CheckPayload;
  var
    bin: PVariant;
  begin
    if err <> '' then
      exit;
    if _Safe(res)^.GetAsPVariant('payload', bin) and
       BSONVariantType.ToBlob({%H-}bin^, payload) then
      resp.InitCSV(pointer(payload), JSON_OPTIONS_FAST, '=', ',')
    else
      err := 'missing or invalid returned payload';
  end;

begin // caller should have made fConnections[0].Open
  if (self = nil) or
     (DatabaseName = '') or
     (UserName = '') or
     (Digest = '') then
    raise EMongoException.CreateUTF8('Invalid %.Auth("%") call',
      [self, DatabaseName]);
  if ForceMongoDBCR or
     (ServerBuildInfoNumber < 3000000) then
  begin
    // MONGODB-CR
    // http://docs.mongodb.org/meta-driver/latest/legacy/implement-authentication-in-driver
    bson := BSONVariant(['getnonce', 1]);
    err := fConnections[0].RunCommand(DatabaseName, bson, res);
    if (err = '') and
       not _Safe(res)^.GetAsRawUTF8('nonce', nonce) then
      err := 'missing returned nonce';
    if err <> '' then
      raise EMongoException.CreateUTF8('%.OpenAuthCR("%") step1: % - res=%',
        [self, DatabaseName, err, res]);
    key := MD5(nonce + UserName + Digest);
    bson := BSONVariant(['authenticate', 1, 'user', UserName, 'nonce', nonce,
      'key', key]);
    err := fConnections[0].RunCommand(DatabaseName, bson, res);
    if err <> '' then
      raise EMongoException.CreateUTF8('%.OpenAuthCR("%") step2: % - res=%',
        [self, DatabaseName, err, res]);
  end
  else
  begin
    // SCRAM-SHA-1
    // https://tools.ietf.org/html/rfc5802#section-5
    user := StringReplaceAll(UserName, ['=', '=3D', ',', '=2C']);
    TAESPRNG.Main.FillRandom(rnd);
    nonce := BinToBase64(@rnd, sizeof(rnd));
    FormatUTF8('n=%,r=%', [user, nonce], first);
    BSONVariantType.FromBinary('n,,' + first, bbtGeneric, bson);
    err := fConnections[0].RunCommand(DatabaseName, BSONVariant(['saslStart', 1,
      'mechanism', 'SCRAM-SHA-1', 'payload', bson, 'autoAuthorize', 1]), res);
    CheckPayload;
    if err = '' then
    begin
      rnonce := resp.U['r'];
      if copy(rnonce, 1, length(nonce)) <> nonce then
        err := 'returned invalid nonce';
    end;
    if err <> '' then
      raise EMongoException.CreateUTF8('%.OpenAuthSCRAM("%") step1: % - res=%',
        [self, DatabaseName, err, res]);
    key := 'c=biws,r=' {%H-}+ rnonce;
    PBKDF2_HMAC_SHA1(Digest, Base64ToBin(resp.U['s']),
      UTF8ToInteger(resp.U['i']), salted);
    HMAC_SHA1(salted, 'Client Key', client);
    sha.Full(@client, SizeOf(client), stored);
    msg := first + ',' + RawUTF8({%H-}payload) + ',' + key;
    HMAC_SHA1(stored, msg, stored);
    XorMemory(@client, @stored, SizeOf(client));
    HMAC_SHA1(salted, 'Server Key', server);
    HMAC_SHA1(server, msg, server);
    msg := key + ',p=' + BinToBase64(@client, SizeOf(client));
    BSONVariantType.FromBinary(msg, bbtGeneric, bson);
    err := fConnections[0].RunCommand(DatabaseName, BSONVariant(['saslContinue',
      1, 'conversationId', res.conversationId, 'payload', bson]), res);
    resp.Clear;
    CheckPayload;
    if (err = '') and
       (resp.U['v'] <> BinToBase64(@server, SizeOf(server))) then
      err := 'Server returned an invalid signature';
    if err <> '' then
      raise EMongoException.CreateUTF8('%.OpenAuthSCRAM("%") step2: % - res=%',
        [self, DatabaseName, err, res]);
    if not res.done then
    begin
      // third empty challenge may be required
      err := fConnections[0].RunCommand(DatabaseName, BSONVariant(['saslContinue',
        1, 'conversationId', res.conversationId, 'payload', '']), res);
      if (err = '') and
         not res.done then
        err := 'SASL conversation failed to complete';
      if err <> '' then
        raise EMongoException.CreateUTF8('%.OpenAuthSCRAM("%") step3: % - res=%',
          [self, DatabaseName, err, res]);
    end;
  end;
end;

procedure TMongoClient.AfterOpen;
begin
  if VarIsEmptyOrNull(fServerBuildInfo) then
  begin
    fConnections[0].RunCommand('admin', 'buildinfo', fServerBuildInfo);
    with _Safe(fServerBuildInfo)^.A['versionArray']^ do
      if count = 4 then
        fServerBuildInfoNumber := // e.g. 2040900 for MongoDB 2.4.9
          integer(Values[0]) * 1000000 + integer(Values[1]) * 10000 +
          integer(Values[2]) * 100 +     integer(Values[3]);
  end;
end;

function TMongoClient.ReOpen: boolean;
var
  digest: RawByteString;
begin
  result := false;
  with fGracefulReconnect do
    if Enabled then
    try
      if fLog <> nil then
        fLog.Enter(self, 'ReOpen: graceful reconnect');
      fConnections[0].Open;
      if EncryptedDigest <> '' then
      try
        digest := CryptDataForCurrentUser(EncryptedDigest, Database, false);
        Auth(Database, user, digest, ForcedDBCR);
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

constructor TMongoDatabase.Create(aClient: TMongoClient; const aDatabaseName: RawUTF8);
var
  colls: TBSONIterator;
  full, db, coll: RawUTF8;
  resp, batch: variant;
  mc: TMongoCollection;
  ndx: Integer;
begin
  fClient := aClient;
  fName := aDatabaseName;
  fCollections := TRawUTF8List.Create([fObjectsOwned, fNoDuplicate, fCaseSensitive]);
  if fClient.ServerBuildInfoNumber < 3000000 then
  begin
    if colls.Init(client.Connections[0].GetBSONAndFree(TMongoRequestQuery.Create(
       aDatabaseName + '.system.namespaces', null, 'name', maxInt))) then
      // e.g. [ {name:"test.system.indexes"}, {name:"test.test"} ]
      while colls.Next do
      begin
        full := colls.item.DocItemToRawUTF8('name');
        if full <> '' then
        begin
          split(full, '.', db, coll);
          if db <> aDatabaseName then
            raise EMongoConnectionException.CreateUTF8(
              '%.Create: invalid [%] collection name for DB [%]',
              [self, full, aDatabaseName], client.Connections[0]);
          mc := TMongoCollection.Create(self, coll);
          fCollections.AddObjectUnique(coll, @mc);
        end;
      end;
  end
  else
  begin
    RunCommand('listCollections', resp);
    if _Safe(resp)^.GetValueByPath('cursor.firstBatch', batch) then
      with _Safe(batch)^ do
        for ndx := 0 to count - 1 do
          if _Safe(Values[ndx]).GetAsRawUTF8('name', coll) then
          begin
            mc := TMongoCollection.Create(self, coll);
            fCollections.AddObjectUnique(coll, @mc);
          end;
  end;
end;

destructor TMongoDatabase.Destroy;
begin
  FreeAndNil(fCollections);
  inherited;
end;

function TMongoDatabase.CreateUser(const UserName, Password: RawUTF8; const
  roles: variant): RawUTF8;
var
  res: variant;
  usr: TDocVariantData;
begin
  usr.InitObject(['createUser', UserName,
    'pwd', PasswordDigest(UserName, Password), 'digestPassword', false,
    'roles', roles], JSON_OPTIONS_FAST);
  // note: passwordDigestor:"client" fails
  if client.ServerBuildInfoNumber >= 4000000 then
    usr.AddValue('mechanisms', _ArrFast(['SCRAM-SHA-1']));
  result := RunCommand(variant(usr), res);
end;

function TMongoDatabase.CreateUserForThisDatabase(
  const UserName, Password: RawUTF8; allowWrite: Boolean): RawUTF8;
const
  RW: array[boolean] of RawUTF8 = (
    'read', 'readWrite');
begin
  result := CreateUser(UserName, Password,
    BSONVariant('[{role:?,db:?}]', [], [RW[allowWrite], name]));
end;

function TMongoDatabase.DropUser(const UserName: RawUTF8): RawUTF8;
var
  res: variant;
begin
  result := RunCommand(BSONVariant(['dropUser', UserName]), res);
end;

function TMongoDatabase.GetCollection(const Name: RawUTF8): TMongoCollection;
begin
  result := GetCollectionOrNil(Name);
  if result = nil then
    raise EMongoDatabaseException.CreateUTF8('%.GetCollection("%")', [self, Name], self);
end;

function TMongoDatabase.GetCollectionOrCreate(const Name: RawUTF8): TMongoCollection;
begin
  result := GetCollectionOrNil(Name);
  if result = nil then
    if self <> nil then
    begin
      result := TMongoCollection.Create(self, Name);
      fCollections.AddObjectUnique(Name, @result);
    end;
end;

function TMongoDatabase.GetCollectionOrNil(const Name: RawUTF8): TMongoCollection;
begin
  if self = nil then
    result := nil
  else
    result := fCollections.GetObjectFrom(Name);
end;

function TMongoDatabase.RunCommand(const command: variant;
  var returnedValue: variant): RawUTF8;
begin
  result := client.Connections[0].RunCommand(name, command, returnedValue);
end;

function TMongoDatabase.RunCommand(const command: variant;
  var returnedValue: TBSONDocument): boolean;
begin
  result := client.Connections[0].RunCommand(name, command, returnedValue);
end;


{ TMongoCollection }

constructor TMongoCollection.Create(aDatabase: TMongoDatabase;
  const aCollectionName: RawUTF8);
begin
  fDatabase := aDatabase;
  fName := aCollectionName;
  fFullCollectionName := fDatabase.Name + '.' + fName;
end;

function TMongoCollection.AggregateCallFromJson(const pipelineJSON: RawUTF8;
  var reply, res: variant): boolean;
begin
  // see http://docs.mongodb.org/manual/reference/command/aggregate
  if fDatabase.Client.ServerBuildInfoNumber < 2020000 then
    raise EMongoException.Create('Aggregation needs MongoDB 2.2 or later');
  if fDatabase.Client.ServerBuildInfoNumber >= 3060000 then
  begin
    // db.runCommand({aggregate:"test",pipeline:[{$group:{_id:null,max:{$max:"$_id"}}}],cursor:{}})
    Database.RunCommand(BSONVariant('{aggregate:"%",pipeline:[%],cursor:{}}',
      [name, pipelineJSON], []), reply);
    // {"cursor":{"firstBatch":[{"_id":null,"max":1510}],"id":0,"ns":"db.test"},"ok":1}
    res := reply.cursor;
    if not VarIsNull(res) then
      res := res.firstBatch;
  end
  else
  begin
    // db.runCommand({aggregate:"test",pipeline:[{$group:{_id:null,max:{$max:"$_id"}}}]})
    Database.RunCommand(BSONVariant('{aggregate:"%",pipeline:[%]}',
      [name, pipelineJSON], []), reply);
    // { "result" : [ { "_id" : null, "max" : 1250 } ], "ok" : 1 }
    res := reply.result;
  end;
  result := not VarIsNull(res);
end;

function TMongoCollection.AggregateDoc(Operators: PUTF8Char;
  const Params: array of const): variant;
begin
  result := AggregateDocFromJson(FormatUTF8(Operators, Params));
end;

function TMongoCollection.AggregateJSON(Operators: PUTF8Char;
  const Params: array of const; Mode: TMongoJSONMode): RawUTF8;
begin
  result := AggregateJSONFromJson(FormatUTF8(Operators, Params), Mode);
end;

function TMongoCollection.AggregateCallFromVariant(const pipelineArray: variant;
  var reply, res: variant): boolean;
begin
  // see http://docs.mongodb.org/manual/reference/command/aggregate
  if fDatabase.Client.ServerBuildInfoNumber < 2020000 then
    raise EMongoException.Create('Aggregation needs MongoDB 2.2 or later');
  if fDatabase.Client.ServerBuildInfoNumber >= 3060000 then
  begin
    // db.runCommand({aggregate:"test",pipeline:[{$group:{_id:null,max:{$max:"$_id"}}}],cursor:{}})
    Database.RunCommand(BSONVariant(['aggregate', name,
      'pipeline', pipelineArray, 'cursor', '{', '}']), reply);
    // {"cursor":{"firstBatch":[{"_id":null,"max":1510}],"id":0,"ns":"db.test"},"ok":1}
    res := reply.cursor;
    if not VarIsNull(res) then
      res := res.firstBatch;
  end
  else
  begin
    // db.runCommand({aggregate:"test",pipeline:[{$group:{_id:null,max:{$max:"$_id"}}}]})
    Database.RunCommand(BSONVariant(['aggregate', name,
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

function TMongoCollection.AggregateJSONFromVariant(const pipelineArray: variant;
  Mode: TMongoJSONMode = modMongoStrict): RawUTF8;
var
  reply, res: variant;
begin
  if AggregateCallFromVariant(pipelineArray, reply, res) then
    result := VariantSaveMongoJSON(res, Mode)
  else
    result := '';
end;

function TMongoCollection.AggregateDocFromJson(const PipelineJSON: RawUTF8): variant;
var
  reply: variant;
begin
  if AggregateCallFromJSON(PipelineJSON, reply, result) then
    TDocVariant.GetSingleOrDefault(result, result, result)
  else
    SetVariantNull(result);
end;

function TMongoCollection.AggregateJSONFromJson(const PipelineJSON: RawUTF8;
  Mode: TMongoJSONMode): RawUTF8;
var
  reply, res: variant;
begin
  if AggregateCallFromJson(PipelineJSON, reply, res) then
    result := VariantSaveMongoJSON(res, Mode)
  else
    result := '';
end;

function TMongoCollection.Drop: RawUTF8;
var
  res: Variant;
begin
  if self = nil then
  begin
    result := 'No collection';
    exit;
  end;
  if Database.Client.Log <> nil then
    Database.Client.Log.Enter('Drop %', [name], self);
  result := fDatabase.RunCommand(BSONVariant('{drop:?}', [], [name]), res);
  Database.Client.Log.Log(sllTrace, 'Drop("%")->%', [name, res], self);
  if result = '' then
    Database.fCollections.Delete(name);
end;

procedure TMongoCollection.EnsureIndex(const Keys, Options: variant);
var
  doc, res: variant;
  indexName: RawUTF8;
  ndx, order: integer;
  useCommand: Boolean;
begin
  if (self = nil) or
     (Database = nil) then
    exit;
  if Database.Client.Log <> nil then
    Database.Client.Log.Enter('EnsureIndex %', [name], self);
  if DocVariantData(Keys)^.kind <> dvObject then
    raise EMongoException.CreateUTF8('%[%].EnsureIndex(Keys?)', [self,
      FullCollectionName]);
  useCommand := fDatabase.Client.ServerBuildInfoNumber >= 2060000;
  doc := _ObjFast(['key', Keys]);
  if not useCommand then
    doc.ns := FullCollectionName;
  if DocVariantType.IsOfType(Options) then
    with _Safe(Options)^ do
      for ndx := 0 to count - 1 do
        if Names[ndx] = 'name' then
          indexName := VariantToUTF8(Values[ndx])
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
          raise EMongoException.CreateUTF8('%[%].EnsureIndex() on order {%:%}',
            [self, FullCollectionName, Names[ndx], Values[ndx]]);
      end;
  end;
  if length(FullCollectionName) + length(indexName) > 120 then
    raise EMongoException.CreateUTF8(
      '%[%].EnsureIndex() computed name > 128 chars: please set as option',
      [self, FullCollectionName]);
  doc.name := indexName;
  if useCommand then
    fDatabase.RunCommand(BSONVariant('{ createIndexes: ?, indexes: [?] }',
      [], [name, doc]), res)
  else
    fDatabase.GetCollectionOrCreate('system.indexes').Insert([doc]);
  Database.Client.Log.Log(sllTrace, 'EnsureIndex("%",%)->%', [name, doc, res], self);
end;

procedure TMongoCollection.EnsureIndex(const Keys: array of RawUTF8;
  Ascending, Unique: boolean);
const
  order: array[boolean] of Integer = (
    -1, 1);
var
  k, opt: variant;
  A: integer;
begin
  if high(Keys) < 0 then
    exit; // no column name
  TDocVariant.New(k);
  for A := 0 to high(Keys) do
    TDocVariantData(k).AddValue(Keys[A], order[Ascending]);
  if Unique then
    opt := _ObjFast(['unique', true]);
  EnsureIndex(k, opt{%H-});
end;

function TMongoCollection.Count: Int64;
var
  res: variant;
begin
  fDatabase.RunCommand(BSONVariant(['count', name]), res);
  result := _Safe(res)^.GetValueOrDefault('n', 0);
end;

function TMongoCollection.FindCount(const Query: variant): Int64;
var
  res: variant;
begin
  fDatabase.RunCommand(BSONVariant(['count', name, 'query', Query]), res);
  result := _Safe(res)^.GetValueOrDefault('n', 0);
end;

function TMongoCollection.FindCount(Criteria: PUTF8Char;
  const Args, Params: array of const; MaxNumberToReturn, NumberToSkip: Integer): Int64;
var
  cmd, query: RawUTF8;
  res: variant;
begin
  query := FormatUTF8(Criteria, Args, Params, true);
  FormatUTF8('{count:"%",query:%', [name, query], cmd);
  if MaxNumberToReturn > 0 then
    cmd := FormatUTF8('%,limit:%', [cmd, MaxNumberToReturn]);
  if NumberToSkip > 0 then
    cmd := FormatUTF8('%,skip:%', [cmd, NumberToSkip]);
  fDatabase.RunCommand(BSONVariant(cmd + '}'), res);
  result := _Safe(res)^.GetValueOrDefault('n', 0);
end;

function TMongoCollection.IsEmpty: boolean;
var
  res: variant;
begin
  // much faster than Count>0 for huge collections
  res := FindDoc(BSONVariant('{$query:{}}'), BSONVariant(['_id', 1]));
  result := VarIsEmptyOrNull(res);
end;

function TMongoCollection.FindBSON(const Criteria, Projection: Variant;
  NumberToReturn, NumberToSkip: Integer; Flags: TMongoQueryFlags): TBSONDocument;
begin
  result := Database.Client.GetOneReadConnection.GetBSONAndFree(TMongoRequestQuery.Create(
    fFullCollectionName, Criteria, Projection, NumberToReturn, NumberToSkip, Flags));
end;

function TMongoCollection.FindDoc(const Criteria, Projection: Variant;
  NumberToReturn, NumberToSkip: Integer; Flags: TMongoQueryFlags): variant;
begin
  Database.Client.GetOneReadConnection.GetDocumentsAndFree(TMongoRequestQuery.Create(
    fFullCollectionName, Criteria, Projection, NumberToReturn, NumberToSkip, Flags), result);
end;

function TMongoCollection.FindDoc(Criteria: PUTF8Char;
  const Params: array of const; NumberToReturn, NumberToSkip: Integer;
  Flags: TMongoQueryFlags): variant;
begin
  result := FindDoc(
    BSONVariant(Criteria, [], Params), null, NumberToReturn, NumberToSkip, Flags);
end;

procedure TMongoCollection.FindDocs(Criteria: PUTF8Char;
  const Params: array of const; var result: TVariantDynArray;
  const Projection: variant; NumberToReturn, NumberToSkip: Integer; Flags: TMongoQueryFlags);
begin
  Database.Client.GetOneReadConnection.GetDocumentsAndFree(TMongoRequestQuery.Create(
    fFullCollectionName, BSONVariant(Criteria, [], Params), Projection,
      NumberToReturn, NumberToSkip, Flags), result);
end;

function TMongoCollection.FindDocs(Criteria: PUTF8Char;
  const Params: array of const; const Projection: variant;
  NumberToReturn, NumberToSkip: Integer; Flags: TMongoQueryFlags): TVariantDynArray;
begin
  FindDocs(Criteria, Params, result, Projection, NumberToReturn, NumberToSkip, Flags);
end;

function TMongoCollection.FindOne(const _id: TBSONObjectID): variant;
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
  result := FindDoc(BSONVariant(NameValuePairs), null, 1);
  if ReturnNewObjectIfNotFound and VarIsEmptyOrNull(result) then
    TDocVariantData(result).InitObject(NameValuePairs, JSON_OPTIONS_FAST);
end;

procedure TMongoCollection.FindDocs(var result: TVariantDynArray;
  const Projection: variant; NumberToReturn, NumberToSkip: Integer; Flags: TMongoQueryFlags);
begin
  Database.Client.GetOneReadConnection.GetDocumentsAndFree(TMongoRequestQuery.Create(
    fFullCollectionName, null, Projection, NumberToReturn, NumberToSkip, Flags), result);
end;

function TMongoCollection.FindJSON(const Criteria, Projection: Variant;
  NumberToReturn, NumberToSkip: Integer; Flags: TMongoQueryFlags; Mode:
  TMongoJSONMode): RawUTF8;
begin
  result := Database.Client.GetOneReadConnection.GetJSONAndFree(TMongoRequestQuery.Create(
    fFullCollectionName, Criteria, Projection, NumberToReturn, NumberToSkip, Flags), Mode);
end;

function TMongoCollection.FindJSON(Criteria: PUTF8Char;
  const Params: array of const; NumberToReturn, NumberToSkip: Integer;
  Flags: TMongoQueryFlags; Mode: TMongoJSONMode): RawUTF8;
begin
  result := FindJSON(BSONVariant(Criteria, [], Params), null, NumberToReturn,
    NumberToSkip, Flags, Mode);
end;

function TMongoCollection.FindJSON(Criteria: PUTF8Char;
  const CriteriaParams: array of const; const Projection: variant;
  NumberToReturn, NumberToSkip: Integer; Flags: TMongoQueryFlags; Mode: TMongoJSONMode): RawUTF8;
begin
  result := FindJSON(BSONVariant(Criteria, [], CriteriaParams), Projection,
    NumberToReturn, NumberToSkip, Flags, Mode);
end;

procedure TMongoCollection.Insert(const Documents: array of variant;
  Flags: TMongoInsertFlags; NoAcknowledge: boolean);
begin
  Database.Client.Connections[0].SendAndFree(TMongoRequestInsert.Create(
    fFullCollectionName, Documents, Flags), NoAcknowledge);
end;

procedure TMongoCollection.Insert(const Documents: TBSONDocument; Flags:
  TMongoInsertFlags; NoAcknowledge: boolean);
begin
  Database.Client.Connections[0].SendAndFree(TMongoRequestInsert.Create(
    fFullCollectionName, Documents, Flags), NoAcknowledge);
end;

procedure TMongoCollection.InsertJSON(const JSONDocuments: array of PUTF8Char;
  Flags: TMongoInsertFlags; NoAcknowledge: boolean);
begin
  Database.Client.Connections[0].SendAndFree(TMongoRequestInsert.Create(
    fFullCollectionName, JSONDocuments, Flags), NoAcknowledge);
end;

function EnsureDocumentHasID(var doc: TDocVariantData; oid: PPVariant;
  DocumentObjectID: PBSONObjectID): boolean;
var
  ndx: integer;
  id: TBSONObjectID;
  v: PVariant;
begin
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
    id.ComputeNew;
    id.ToVariant(v^);
    if DocumentObjectID <> nil then
      DocumentObjectID^ := id;
  end
  else if DocumentObjectID <> nil then
    if not DocumentObjectID^.FromVariant(v^) then
      DocumentObjectID^.Init;
  if oid <> nil then
    oid^ := v;
end;

procedure TMongoCollection.Insert(const Document: RawUTF8;
  const Params: array of const; DocumentObjectID: PBSONObjectID);
var
  doc: variant;
begin
  _JsonFmt(Document, [], Params, JSON_OPTIONS_FAST, doc);
  EnsureDocumentHasID(TDocVariantData(doc), nil, DocumentObjectID);
  Insert([doc]);
end;

function TMongoCollection.Save(var Document: variant;
  DocumentObjectID: PBSONObjectID): boolean;
var
  oid: PVariant;
begin
  if not DocVariantType.IsOfType(Document) then
    Document := _JsonFast(VariantSaveMongoJSON(Document, modMongoShell));
  result := EnsureDocumentHasID(_Safe(Document, dvObject)^, @oid, DocumentObjectID);
  if result then
    Insert([Document])
  else
    Update(BSONVariant(['_id', oid^]), Document, [mufUpsert])
end;

procedure TMongoCollection.Save(const Document: RawUTF8;
  const Params: array of const; DocumentObjectID: PBSONObjectID);
var
  doc: variant;
begin
  _JsonFmt(Document, [], Params, JSON_OPTIONS_FAST, doc);
  Save(doc, DocumentObjectID);
end;

procedure TMongoCollection.Update(Query: PUTF8Char; const QueryParams: array of const;
  const Update: RawUTF8; const UpdateParams: array of const; Flags: TMongoUpdateFlags);
var
  quer, upd: variant;
begin
  quer := BSONVariant(Query, [], QueryParams);
  upd := BSONVariant(Update, [], UpdateParams);
  self.Update(quer, upd, Flags);
end;

procedure TMongoCollection.Update(const Query, Update: variant; Flags: TMongoUpdateFlags);
begin
  Database.Client.Connections[0].SendAndFree(TMongoRequestUpdate.Create(
    fFullCollectionName, Query, Update, Flags), false);
end;

procedure TMongoCollection.UpdateOne(const _id, UpdatedFields: variant);
begin
  Update(BSONVariant(['_id', _id]), BSONVariant(['$set', UpdatedFields]));
end;

procedure TMongoCollection.Remove(const Query: variant; Flags: TMongoDeleteFlags);
begin
  Database.Client.Connections[0].SendAndFree(TMongoRequestDelete.Create(
    fFullCollectionName, Query, Flags), False);
end;

procedure TMongoCollection.RemoveOne(const _id: TBSONObjectID);
begin
  Remove(BSONVariant(['_id', _id.ToVariant]), [mdfSingleRemove]);
end;

procedure TMongoCollection.RemoveOne(const _id: variant);
begin
  Remove(BSONVariant(['_id', _id]), [mdfSingleRemove]);
end;

procedure TMongoCollection.RemoveFmt(Query: PUTF8Char;
  const QueryParams: array of const; Flags: TMongoDeleteFlags);
begin
  Remove(BSONVariant(Query, [], QueryParams), Flags);
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
  Assert(sizeof(TMongoReplyHeader) = 36);

end.

