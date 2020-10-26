/// Database Framework BSON Encoding for MongoDB
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.db.nosql.bson;

{
  *****************************************************************************

   Efficient BSON Support for MongoDB Clients
    - BSON Decimal128 Value
    - BSON ObjectID Value
    - TBSONVariantData / TBSONVariant Custom Variant Storage
    - TBSONElement / TBSONIterator for BSON Decoding
    - TBSONWriter for BSON Encoding
    - High-Level BSON/JSON Function Helpers

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
  mormot.core.variants,
  mormot.core.json,
  mormot.core.rtti,
  mormot.core.datetime,
  mormot.core.data,
  mormot.core.perf,
  mormot.core.log,
  mormot.db.core;


{ ************ BSON Decimal128 Value }

type
  /// binary representation of a 128-bit decimal, stored as 16 bytes
  // - i.e. IEEE 754-2008 128-bit decimal floating point as used in the
  // BSON Decimal128 format, and processed by the TDecimal128 object
  TDecimal128Bits = record
    case integer of
      0:
        (lo, hi: QWord);
      1:
        (l, h: Int64);
      2:
        (b: array[0..15] of byte);
      3:
        (c: array[0..3] of cardinal);
  end;

  /// points to a 128-bit decimal binary
  PDecimal128Bits = ^TDecimal128Bits;

  /// enough characters to contain any TDecimal128 text representation
  TDecimal128Str = array[0..42] of AnsiChar;

  /// some special 128-bit decimal values
  // - see TDecimal128.SetSpecial to set the corresponding value
  // - dsvError is returned by TDecimal128.FromText() on parsing error
  // - dsvValue indicates that this is not a known "special" value, but some
  // valid decimal number
  TDecimal128SpecialValue = (
    dsvError, dsvValue, dsvNan, dsvZero, dsvPosInf, dsvNegInf, dsvMin, dsvMax);

  /// handles a 128-bit decimal value
  // - i.e. IEEE 754-2008 128-bit decimal floating point as used in the
  // BSON Decimal128 format, i.e. betDecimal128 TBSONElementType
  // - the betFloat BSON format stores a 64-bit floating point value, which
  // doesn't have exact decimals, so may suffer from rounding or approximation
  // - for instance, if you work with some currency values, you may store
  // betDecimal128 values in MongoDB - the easiest way is to include it as a
  // TBSONVariant instance, via the NumberDecimal() function
  // - there is no mathematical operator/methods for Decimal128 Value Objects,
  // as required by MongoDB specifications: any computation must be done
  // explicitly on native language value representation (e.g. currency, TBCD or
  // any BigNumber library) - use ToCurr/FromCurr or ToText/FromText to make
  // the appropriate safe conversions
  {$ifdef USERECORDWITHMETHODS}
  TDecimal128 = record
  {$else}
  TDecimal128 = object
  {$endif USERECORDWITHMETHODS}
  public
    /// the raw binary storage
    Bits: TDecimal128Bits;
    /// fills with the Zero value
    // - note: under IEEE 754, Zero can have sign and exponents, so is not Hi=Lo=0
    // - is the same as Fill(dsvZero)
    procedure SetZero;
    /// fills with a special value
    // - dsvError or dsvValue will set dsvNan binary content
    procedure SetSpecial(special: TDecimal128SpecialValue);
    /// checks if the value matches one of the known special values
    // - will search for dsvNan, dsvZero, dsvPosInf, dsvNegInf, dsvMin, dsvMax
    function IsSpecial: TDecimal128SpecialValue;
    /// fills with a 32-bit signed value
    procedure FromInt32(value: integer);
    /// fills with a 32-bit unsigned value
    procedure FromUInt32(value: cardinal);
      {$ifdef HASINLINE}inline;{$endif}
    /// fills with a 64-bit signed value
    procedure FromInt64(value: Int64);
    /// fills with a 64-bit unsigned value
    procedure FromQWord(value: QWord);
      {$ifdef HASINLINE}inline;{$endif}
    /// fills with a fixed decimal value, as stored in currency
    // - will store the content with explictly four decimals, as in currency
    // - by design, this method is very fast and accurate
    procedure FromCurr(const value: currency);
    /// fills from the text representation of a decimal value
    // - returns dsvValue or one of the dsvNan, dsvZero, dsvPosInf, dsvNegInf
    // special value indicator otherwise on succes
    // - returns dsvError on parsing failure
    function FromText(text: PUTF8Char; textlen: integer): TDecimal128SpecialValue; overload;
    /// fills from the text representation of a decimal value
    // - returns dsvValue or one of the dsvNan, dsvZero, dsvPosInf, dsvNegInf
    // special value indicator otherwise on succes
    // - returns dsvError on parsing failure
    function FromText(const text: RawUTF8): TDecimal128SpecialValue; overload;
    /// convert a variant into one Decimal128 value
    // - will first check for a TBSONVariant containing a betDecimal128 (e.g.
    // as retrieved via the ToVariant method)
    // - will recognize currency and VariantToInt64() stored values
    // - then will try to convert the variant from its string value, expecting
    // a floating-point text content
    // - returns TRUE if conversion was made, FALSE on any error
    function FromVariant(const value: variant): boolean;
    /// fills with a native floating-point value
    // - note that it doesn't make much sense to use this method: you should
    // rather use the native betFloat BSON format, with native double precision
    // - this method is just a wrapper around ExtendedToShort and ToText,
    // so you should provide the expected precision, from the actual storage
    // variable (you may specify e.g. SINGLE_PRECISION or EXTENDED_PRECISION if
    // you don't use a double kind of value)
    function FromFloat(const value: TSynExtended; precision: integer = 0): boolean;
    /// fast bit-per-bit value comparison
    function Equals(const other: TDecimal128): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// converts the value to its string representation
    // - returns the number of AnsiChar written to Buffer
    function ToText(out Buffer: TDecimal128Str): integer; overload;
    /// converts this Decimal128 value to its string representation
    function ToText: RawUTF8; overload;
    /// converts this Decimal128 value to its string representation
    procedure ToText(var result: RawUTF8); overload;
    /// convert this Decimal128 value to its TBSONVariant custom variant value
    function ToVariant: variant; overload;
    /// convert this Decimal128 value to its TBSONVariant custom variant value
    procedure ToVariant(out result: variant); overload;
    /// converts this Decimal128 value to a floating-point value
    // - by design, some information may be lost during conversion
    // - note that it doesn't make much sense to use this method: you should
    // rather use the native betFloat BSON format, with native double precision
    function ToFloat: TSynExtended;
    /// converts this Decimal128 value to a fixed decimal value
    // - by design, some information may be lost during conversion, unless the
    // value has been stored previously via the FromCurr() method - in this
    // case, conversion is immediate and accurate
    function ToCurr: currency; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// converts this Decimal128 value to a fixed decimal value
    // - by design, some information may be lost during conversion, unless the
    // value has been stored previously via the FromCurr() method - in this
    // case, conversion is immediate and accurate
    procedure ToCurr(out result: currency); overload;
    /// converts this Decimal128 value to its string representation
    procedure AddText(W: TTextWriter);
  end;

  /// points to a 128-bit decimal value
  PDecimal128 = ^TDecimal128;

const
  /// the textual representation of the TDecimal128 special values
  DECIMAL128_SPECIAL_TEXT: array[TDecimal128SpecialValue] of RawUTF8 = (
  // dsvError, dsvValue, dsvNan, dsvZero, dsvPosInf, dsvNegInf, dsvMin, dsvMax
    '', '', 'NaN', '0', 'Infinity', '-Infinity',
    '-9.999999999999999999999999999999999E+6144',
    '9.999999999999999999999999999999999E+6144');

  BSON_DECIMAL128_HI_NAN = $7c00000000000000;
  BSON_DECIMAL128_HI_INT64POS = $3040000000000000; // 0 fixed decimals
  BSON_DECIMAL128_HI_INT64NEG = $b040000000000000;
  BSON_DECIMAL128_HI_CURRPOS = $3038000000000000; // 4 fixed decimals
  BSON_DECIMAL128_HI_CURRNEG = $b038000000000000;
  BSON_DECIMAL128_EXPONENT_MAX = 6111;
  BSON_DECIMAL128_EXPONENT_MIN = -6176;
  BSON_DECIMAL128_EXPONENT_BIAS = 6176;
  BSON_DECIMAL128_MAX_DIGITS = 34;

/// ready-to-be displayed text of a TDecimal128SpecialValue
function ToText(spec: TDecimal128SpecialValue): PShortString; overload;


{ ************ BSON ObjectID Value }

type
    /// 24-bit storage, mapped as a 3 bytes buffer
  // - as used fo TBSONObjectID.MachineID and TBSONObjectID.Counter
  TBSON24 = record
    b1, b2, b3: byte;
  end;

  /// points to 24-bit storage, mapped as a 3 bytes buffer
  PBSON24 = ^TBSON24;

  {$A-}

  /// BSON ObjectID 12-byte internal binary representation
  // - in MongoDB, documents stored in a collection require a unique _id field
  // that acts as a primary key: by default, it uses such a 12-byte ObjectID
  // - by design, sorting by _id: ObjectID is roughly equivalent to sorting by
  // creation time, so ease sharding and BTREE storage
  // - in our ODM, we rather use 64-bit genuine integer identifiers (TID),
  // as computed by an internal sequence or TSynUniqueIdentifierGenerator
  // - match betObjectID TBSONElementType
  {$ifdef USERECORDWITHMETHODS}
  TBSONObjectID = record
  {$else}
  TBSONObjectID = object
  {$endif USERECORDWITHMETHODS}
    /// big-endian 4-byte value representing the seconds since the Unix epoch
    // - time is expressed in Coordinated Universal Time (UTC), not local time
    UnixCreateTime: cardinal;
    /// 3-byte machine identifier
    // - ComputeNew will use a hash of ExeVersion.Host and ExeVersion.User
    MachineID: TBSON24;
    /// 2-byte process id
    // - ComputeNew will derivate it from MainThreadID
    ProcessID: word;
    /// 3-byte counter, starting with a random value
    // - used to avoid collision
    Counter: TBSON24;
    /// set all internal fields to zero
    procedure Init; {$ifdef HASINLINE}inline;{$endif}
    /// ObjectID content be filled with some unique values
    // - this implementation is thread-safe
    procedure ComputeNew;
    /// convert an hexadecimal string value into one ObjectID
    // - returns TRUE if conversion was made, FALSE on any error
    function FromText(const Text: RawUTF8): boolean; overload;
    /// convert an hexadecimal string value into one ObjectID
    // - returns TRUE if conversion was made, FALSE on any error
    function FromText(Text: PUTF8Char): boolean; overload;
    /// convert a variant into one ObjectID
    // - will first check for a TBSONVariant containing a betObjectID
    // - then will try to convert the variant from its string value, expecting
    // an hexadecimal text content
    // - returns TRUE if conversion was made, FALSE on any error
    function FromVariant(const value: variant): boolean;
    /// convert this ObjectID to its hexadecimal string value
    function ToText: RawUTF8; overload;
    /// convert this ObjectID to its hexadecimal string value
    procedure ToText(var result: RawUTF8); overload;
    /// convert this ObjectID to its TBSONVariant custom variant value
    function ToVariant: variant; overload;
    /// convert this ObjectID to its TBSONVariant custom variant value
    procedure ToVariant(var result: variant); overload;
    /// returns the timestamp portion of the ObjectId() object as a TDateTime
    // - time is expressed in Coordinated Universal Time (UTC), not local time
    // so you can compare it to NowUTC returned time
    function CreateDateTime: TDateTime;
    /// compare two Object IDs
    function Equal(const Another: TBSONObjectID): boolean; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// compare two Object IDs, the second being stored in a TBSONVariant
    function Equal(const Another: variant): boolean; overload;
      {$ifdef HASINLINE}inline;{$endif}
  end;

  /// points to a BSON ObjectID internal binary representation
  PBSONObjectID = ^TBSONObjectID;

  {$A+}

  
{ ************ TBSONVariantData / TBSONVariant Custom Variant Storage }

type
  /// exception type used for BSON process
  EBSONException = class(ESynException);

  /// storage of a BSON binary document
  // - a specific type is defined for consistency with this unit classes
  // - binary content should follow the "int32 e_list #0" standard layout
  TBSONDocument = RawByteString;

  /// dynamic array of BSON binary document storage
  TBSONDocumentDynArray = array of TBSONDocument;

  /// element types for BSON internal representation
  TBSONElementType = (
    betEOF, betFloat, betString, betDoc, betArray, betBinary,
    betDeprecatedUndefined, betObjectID, betBoolean, betDateTime, betNull,
    betRegEx, betDeprecatedDbptr, betJS, betDeprecatedSymbol, betJSScope,
    betInt32, betTimestamp, betInt64, betDecimal128);

  /// points to an element type for BSON internal representation
  PBSONElementType = ^TBSONElementType;

  /// sub-types for betBinary element BSON internal representation
  TBSONElementBinaryType = (
    bbtGeneric, bbtFunction, bbtOldBinary, bbtOldUUID,
    bbtUUID, bbtMD5, bbtUser = $80);

  {$A-}

  /// memory structure used for some special BSON storage as variant
  // - betObjectID kind will store a TBSONObjectID
  // - betBinary kind will store a BLOB content as RawByteString
  // - betDoc and betArray kind will store a BSON document, in its original
  // binary format as RawByteString (TBSONDocument)
  // - betDeprecatedDbptr, betJSScope, betTimestamp and betRegEx will store the
  // raw original BSON content as RawByteString
  // - betJS and betDeprecatedSymbol will store the UTF-8 encoded string
  // as a RawUTF8
  // - betDeprecatedUndefined or betMinKey/betMaxKey do not contain any data
  // - betDecimal128 will store the TDecimal128 16 bytes binary buffer
  // - warning: VBlob/VText use should match BSON_ELEMENTVARIANTMANAGED constant
  TBSONVariantData = packed record
    /// the variant type
    VType: TVarType;
    /// the kind of element stored
    case VKind: TBSONElementType of
      betObjectID:
        (
      {$ifdef fpc} {$push} {$endif} {$hints off}
        // does not complain if Filler is declared but void
        VFiller: array[1..SizeOf(TVarData) - SizeOf(TVarType)
          - SizeOf(TBSONElementType) - SizeOf(TBSONObjectID)] of byte;
      {$ifdef fpc} {$pop} {$else} {$hints on} {$endif}
        VObjectID: TBSONObjectID);
      betBinary, betDoc, betArray, betRegEx, betDeprecatedDbptr, betTimestamp,
      betJSScope, betDecimal128:
        (
        /// store the raw binary content as a RawByteString (or TBSONDocument for
        // betDoc/betArray, i.e. the "int32 e_list #0" standard layout)
        // - you have to use RawByteString(VBlob) when accessing this field
        // - e.g. for betRegEx, it will contain raw [cstring cstring] content
        VBlob: pointer;);
      betJS, betDeprecatedSymbol:
        (
        /// store here a RawUTF8 with the associated text
        // - you have to use RawUF8(VText) when accessing this field
        VText: pointer;);
  end;
  
  {$A+}

  /// points to memory structure used for some special BSON storage as variant
  PBSONVariantData = ^TBSONVariantData;

  /// custom variant type used to store some special BSON elements
  // - internal layout will follow TBSONVariantData
  // - handled kind of item are complex BSON types, like betObjectID, betBinary
  // or betDoc/betArray
  // - it will allow conversion to/from string (and to date for ObjectID)
  TBSONVariant = class(TSynInvokeableVariantType)
  protected
    function GetNewDoc(const BSONDoc: TBSONDocument): variant;
  public
    /// customization of JSON conversion into TBSONVariant kind of variants
    function TryJSONToVariant(var JSON: PUTF8Char; var Value: variant;
      EndOfObject: PUTF8Char): boolean; override;
    /// variant serialization will use modMongoStrict JSON-compatible mode
    procedure ToJSON(W: TTextWriter; const Value: variant; Escape:
      TTextWriterKind); override;
    /// handle type conversion
    // - only types processed by now are string/OleStr/UnicodeString/date
    procedure Cast(var Dest: TVarData; const Source: TVarData); override;
    /// handle type conversion
    // - only types processed by now are string/OleStr/UnicodeString/date
    procedure CastTo(var Dest: TVarData; const Source: TVarData;
      const AVarType: TVarType); override;
    /// clear the instance
    procedure Clear(var V: TVarData); override;
    /// copy one instance
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean); override;
    /// compare two variant values
    // - handle comparison of any variant, including TBSONVariant, via a
    // temporary JSON conversion, and case-sensitive comparison
    // - it uses case-sensitive text (hexadecimal) comparison for betObjectID
    procedure Compare(const Left, Right: TVarData;
      var Relationship: TVarCompareResult); override;
    /// convert a TBSONDocument binary content into a TBSONVariant of kind
    // betDoc or betArray
    // - see also all BSONVariant() overloaded functions, which also create
    // a TBSONVariant betDoc instance
    procedure FromBSONDocument(const BSONDoc: TBSONDocument; var result: variant;
      Kind: TBSONElementType = betDoc);
    /// convert a BLOB binary content into a TBSONVariant of kind betBinary
    // - if Bin is '', will store a NULL variant
    procedure FromBinary(const Bin: RawByteString;
      BinType: TBSONElementBinaryType; var result: variant);
    /// convert a JSON content into a TBSONVariant of kind betDoc or betArray
    // - warning: the supplied JSON buffer will be modified in-place
    // - will create a plain variant value if the JSON doesn't start with [ or {
    procedure FromJSON(json: PUTF8Char; var result: variant);
    /// returns TRUE if the supplied variant stores the supplied BSON kind of value
    function IsOfKind(const V: variant; Kind: TBSONElementType): boolean;
    /// retrieve a betBinary content stored in a TBSONVariant instance
    // - returns TRUE if the supplied variant is a betBinary, and set the
    // binary value into the supplied Blob variable
    // - returns FALSE otherwise
    function ToBlob(const V: Variant; var Blob: RawByteString): boolean;
    /// convert a TBSONDocument binary content into a TBSONVariant of kind betDoc
    // - is the default property, so that you can write:
    // ! BSONVariantType[BSON(['BSON',_Arr(['awesome',5.05, 1986])])]
    // - see also all BSONVariant() overloaded functions, which also create
    // a TBSONVariant betDoc instance
    property NewDoc[const BSONDoc: TBSONDocument]: variant read GetNewDoc; default;
  end;


{ ************ TBSONElement / TBSONIterator for BSON Decoding }

type
  /// define how betDoc/betArray BSON elements will be converted as variants
  // - by default a TBSONVariant custom type will be returned, containing the
  // raw BSON binary content of the embedded document or array
  // - asDocVariantPerValue or asDocVariantPerReference could be used to
  // create a tree of TDocVariant custom kind of variant, able to access
  // to its nested properties via late-binding (asDocVariantPerReference being
  // also much faster in some cases - but less safe - than asDocVariantPerValue)
  // - asDocVariantPerValue will set JSON_OPTIONS[false] settings:
  // ! [dvoReturnNullForUnknownProperty]
  // - asDocVariantPerReference will set JSON_OPTIONS[true]/JSON_OPTIONS_FAST
  // settings:
  // ! [dvoValueCopiedByReference,dvoReturnNullForUnknownProperty]
  // - asDocVariantInternNamesPerValue and asDocVariantInternNamesPerReference
  // will include dvoInternalNames to the TDocVariant.Options
  TBSONDocArrayConversion = (
    asBSONVariant, asDocVariantPerValue,
    asDocVariantPerReference, asDocVariantInternNamesPerValue,
    asDocVariantInternNamesPerReference);

  /// how TBSONElement.AddMongoJSON() method and AddMongoJSON() and
  // VariantSaveMongoJSON() functions will render their JSON content
  // - modMongoStrict and modNoMongo will follow the JSON RFC specifications
  // - modMongoShell will use a syntax incompatible with JSON RFC, but more
  // common to MongoDB daily use - as 'ObjectId()' or '{ field: /acme.*corp/i }'
  // - modMongoStrict will use the MongoDB Extended JSON syntax
  // - modNoMongo will serialize dates as ISO-8601 strings, ObjectID as hexadecimal
  // string and other MongoDB special objects in WrBase64() format
  // - see http://docs.mongodb.org/manual/reference/mongodb-extended-json
  TMongoJSONMode = (
    modNoMongo, modMongoStrict, modMongoShell);

  {$A-}

  /// data structure used during BSON binary decoding of one BSON element
  // - will be retrieved by FromVariant() or FromNext()
  // - see http://bsonspec.org/#/specification
  // - this structure has been optimized to map the BSON binary content,
  // without any temporary memory allocation (the SAX way)
  {$ifdef USERECORDWITHMETHODS}
  TBSONElement = record
  {$else}
  TBSONElement = object
  {$endif USERECORDWITHMETHODS}
  private
    /// used internally to set the TBSONElement content, once Kind has been set
    procedure FromBSON(bson: PByte);
  public
    /// index of this element in the original sequence list
    // - is correct only when the element has been reset before the parsing
    // loop, e.g.:
    // ! item.Index := -1; // so item.Index will count starting at 0
    // ! while item.FromNext(elem.Document) do
    // !   writeln(item.Index,' ',Item.Name,' ',Item.ValueBytes);
    Index: integer;
    /// the UTF-8 encoded name of this element
    Name: PUTF8Char;
    /// the name length (in chars) of this element
    NameLen: integer;
    /// the element type
    Kind: TBSONElementType;
    /// number of bytes in the BSON element
    // - will include the trailing #0 for string element
    ElementBytes: integer;
    /// pointer to the BSON element value
    // - is the raw value, without any parsing, e.g. points to a double value or
    // a document: "int32 e_list #0" standard layout (same as TBSONDocument)
    // - you may cast it for simple types:
    // ! PDouble(Element)^   PBoolean(Element)^        PInteger(Element)^
    // ! PInt64(Element)^    PBSONObjectID(Element)^   PDecimal128(Element)^
    // - or use the nested Data variant record to access more complex content
    // - warning: equals nil for betString/betJS after FromVariant()
    Element: pointer;
    /// depending on the Kind, will point to parsed complex sub-data
    // - since variable records can't have properties, we nest this information
    // within this main Data variable record
    // - not all Kind are handled here, only any complex data
    Data: record
      case TBSONElementType of
        betFloat, betBoolean, betInt32, betDateTime, betInt64:
          (
          /// this variable is not to be used directly, but for some internal
          // temporary storage, e.g. with FromVariant()
          // - use P*(Element)^ typecast instead
          InternalStorage: Int64;);
        betString, betJS:
          (
          /// points to the #0 ending string
          Text: PUTF8Char;
          /// number of bytes in Text (excluding trailing #0)
          TextLen: integer;);
        betDoc, betArray:
          (
          /// points to a "e_list #0" standard layout
          DocList: PByte;);
        betBinary:
          (
          /// points to the binary content
          Blob: pointer;
          /// number of bytes in Blob
          BlobLen: integer;
          /// corresponding sub-type of this Blob
          BlobSubType: TBSONElementBinaryType;);
        betRegEx:
          (RegEx: PUTF8Char;
          RegExLen: integer;
          RegExOptions: PUTF8Char;
          RegExOptionsLen: integer;);
        betJSScope:
          (JavaScript: PUTF8Char;
          JavaScriptLen: integer;
          ScopeDocument: PByte;);
        betTimestamp:
          (
          { map InternalStorage: Int64 }
          time_t: cardinal;
          ordinal: cardinal;);
    end;
    /// fill a BSON Element structure from a variant content and associated name
    // - perform the reverse conversion as made with ToVariant()
    // - since the result won't store any data but points to the original binary
    // content, the supplied Name/Value instances should remain available as long as
    // you will access to the result content
    // - aName here is just for conveniency, and could be left void
    // - supplied aTemp variable will be used for temporary storage, private to
    // this initialized TBSONElement
    procedure FromVariant(const aName: RawUTF8; const aValue: Variant;
      var aTemp: RawByteString);
    /// fill a BSON Element structure from a BSON document
    // - will check the document length then set Kind := betDoc and Data.DocList
    // - will return TRUE if the supplied doc has a valid length, FALSE otherwise
    // - you can later on use DocItemToVariant, DocItemToRawUTF8 or
    // DocItemToInteger methods
    // - the supplied "doc" variable should remain available until you are done
    // with this TBSONElement wrapper
    function FromDocument(const doc: TBSONDocument): boolean;
    /// fill a BSON Element structure from a BSON encoded binary buffer list
    // - parse the next BSON element: BSON parameter should point to the
    // "e_list" of the "int32 e_list #0" BSON document
    // - will decode the supplied binary buffer into the BSON element structure,
    // then it will let BSON point to the next element, and return TRUE
    // - returns FALSE when you reached betEOF, so that you can use it in a loop,
    // and retrieve all the content as consecutive events, without any memory
    // allocation (the SAX way):
    // ! var bson: PByte;
    // !     item: TBSONElement;
    // ! ...
    // ! BSONParseLength(bson);
    // ! while item.FromNext(bson) do
    // !   writeln(item.Name);
    // - will raise an EBSONException if BSON content is not correct
    // - as an alternative, consider using TBSONIterator, which wrap both a
    // PByte and a TBSONElement into one convenient item
    function FromNext(var BSON: PByte): boolean;
    /// search for a given name in a BSON encoded binary buffer list
    // - BSON parameter should point to the first "e_list" item of the
    // "int32 e_list #0" BSON document
    // - returns false if the item was not found (with case-insensitive search)
    // - otherwise returns TRUE and the matching element content has been
    // decoded within this TBSONElement structure
    function FromSearch(BSON: PByte; const aName: RawUTF8): boolean;
    /// convert a BSON element, as retrieved by TBSONElement.FromNext(),
    // into a variant
    // - it will return either standard variant values, or TBSONVariant custom type
    // for most complex kind of elements (see TBSONVariantData type definition)
    // - note that betString types will be stored as RawUTF8 varString
    // - by default, it will return TBSONVariant custom variants for documents or
    // arrays - but if storeDocArrayAsDocVariant is set, it will return a
    // TDocVariant custom kind of variant, able to access to its nested
    // properties via late-binding
    function ToVariant(
      DocArrayConversion: TBSONDocArrayConversion = asBSONVariant): variant; overload;
    /// convert a BSON element, as retrieved by TBSONElement.FromNext(),
    // into a variant
    // - same as the other ToVariant() overloaded function, but avoiding a copy
    // of the resulting variant
    procedure ToVariant(var result: variant;
      DocArrayConversion: TBSONDocArrayConversion = asBSONVariant); overload;
    /// convert a BSON element into an UTF-8 string
    // - any complex types (e.g. nested documents) will be converted via a
    // variant
    function ToRawUTF8: RawUTF8;
    /// convert a BSON element into an integer value
    // - will work only for betBoolean/betInt32/betInt64 types
    // - any other kind of values will return the supplied default value
    function ToInteger(const default: Int64 = 0): Int64;
    /// search a variant property value within the BSON element as document
    // - returns true if aName has been found as property in the BSON element,
    // and fills aValue with the corresponding value
    // - returns false if aName was not found, or if Kind is not betDoc or betArray
    function DocItemToVariant(const aName: RawUTF8; var aValue: variant;
      DocArrayConversion: TBSONDocArrayConversion = asBSONVariant): boolean;
    /// search an UTF-8 property value within the BSON element as document
    // - returns the value if aName has been found as property in the BSON element
    // - returns '' if aName was not found, or if Kind is not betDoc or betArray
    function DocItemToRawUTF8(const aName: RawUTF8): RawUTF8;
    /// search an integer property value within the BSON element as document
    // - returns the value if aName has been found as property in the BSON element
    // - returns default if aName was not found, or if Kind is not betDoc or betArray
    function DocItemToInteger(const aName: RawUTF8; const default: Int64 = 0): Int64;
    /// convert a BSON element, as retrieved by TBSONElement.FromNext(), into
    // its JSON representation
    // - this method will use by default the MongoDB Extended JSON syntax for
    // specific MongoDB objects but you may use modMongoShell if needed
    // - will raise an EBSONException if element is not correct
    procedure AddMongoJSON(W: TTextWriter;
      Mode: TMongoJSONMode = modMongoStrict); overload;
  end;

  PBSONElement = ^TBSONElement;

  /// data structure used for iterating over a BSON binary buffer
  // - is just a wrapper around a PByte value, to be used with a TBSONDocument
  {$ifdef USERECORDWITHMETHODS}
  TBSONIterator = record
  {$else}
  TBSONIterator = object
  {$endif USERECORDWITHMETHODS}
  private
    fBson: PByte;
  public
    /// map the current item, after the Next method did return TRUE
    // - map the global document, after Init() but before the first Next call
    Item: TBSONElement;
    /// initialize the iteration on the supplied BSON document
    // - will check the document length and returns TRUE if it is correct
    // - only accepted kind are betDoc and betArray (but not used later)
    // - you can then use the Next method to iterate over the Item elements
    // - after this call, the Item property map to the global BSON document
    // (note that after any call to the Next method, Item will map the current
    // iterated value, and not the global BSON document any more)
    function Init(const doc: TBSONDocument; kind: TBSONElementType = betArray): boolean;
    /// will iterate on the BSON document
    // - returns TRUE if the item has been retrieved into the Item property
    // - returns FALSE if we reached the end of the supplied BSON buffer
    function Next: boolean;
      {$ifdef HASINLINE}inline;{$endif}
  end;

  {$A+}
  

{ ************ TBSONWriter for BSON Encoding }

type
    /// used to write the BSON context
  TBSONWriter = class(TBufferWriter)
  { note: inlining methods generates 70% SLOWER code due to inefficient compiler :( }
  protected
    fDocumentCount: integer;
    fDocument: array of record
      Offset: cardinal;
      Length: cardinal;
    end;
    fDocumentStack: integer;
    fDocumentStackOffset: TCardinalDynArray;
    fDocumentArray: integer;
    procedure WriteCollectionName(Flags: integer; const CollectionName: RawUTF8);
  public
    /// rewind the Stream to the position when Create() was called
    // - this will also reset the internal document offset table
    procedure CancelAll; override;

    /// write a boolean value
    procedure BSONWrite(const name: RawUTF8; const value: boolean); overload;
    /// write a floating point value
    procedure BSONWrite(const name: RawUTF8; const value: Double); overload;
    /// write a 32 bit integer value
    procedure BSONWrite(const name: RawUTF8; const value: integer); overload;
    /// write a 64 bit integer value
    procedure BSONWrite(const name: RawUTF8; const value: Int64); overload;
    /// write a string (UTF-8) value
    procedure BSONWrite(const name: RawUTF8; const value: RawUTF8;
      isJavaScript: boolean = false); overload;
    /// write a string (UTF-8) value from a memory buffer
    procedure BSONWrite(const name: RawUTF8; value: PUTF8Char); overload;
    /// write a string (UTF-8) value from a memory buffer
    procedure BSONWriteString(const name: RawUTF8; value: PUTF8Char; valueLen: integer);
    /// write a binary (BLOB) value
    procedure BSONWrite(const name: RawUTF8; Data: pointer; DataLen: integer); overload;
    /// write an ObjectID value
    procedure BSONWrite(const name: RawUTF8; const value: TBSONObjectID); overload;
    /// write a RegEx value
    procedure BSONWriteRegEx(const name: RawUTF8; const RegEx, Options: RawByteString);
    /// write a data/time value
    procedure BSONWriteDateTime(const name: RawUTF8; const value: TDateTime);
    /// write an element with no value
    // - elemType can be either betNull, betMinKey or betMaxKey
    procedure BSONWrite(const name: RawUTF8; elemtype: TBSONElementType); overload;
    /// write an element with no value
    procedure BSONWrite(const name: RawUTF8; const elem: TBSONElement); overload;
    /// write a BSONVariant instance value
    procedure BSONWrite(const name: RawUTF8; const bson: TBSONVariantData); overload;
    /// write a DocVariant instance value
    procedure BSONWrite(const name: RawUTF8; const doc: TDocVariantData); overload;
    /// write a TDecimal128 value
    procedure BSONWrite(const name: RawUTF8; const value: TDecimal128); overload;
    /// write a variant value
    // - handle simple types (numbers, strings...) and custom types (TDocVariant
    // and TBSONVariant, trying a translation to JSON for other custom types)
    procedure BSONWriteVariant(const name: RawUTF8; const value: variant); overload;
    /// write an open array (const Args: array of const) argument
    // - handle simple types (numbers, strings...) and custom types (TDocVariant)
    procedure BSONWrite(const name: RawUTF8; const value: TVarRec); overload;
    /// write a value from the supplied JSON content
    // - is able to handle any kind of values, including nested documents or
    // BSON extended syntax (if DoNotTryExtendedMongoSyntax=false)
    // - this method is used recursively by BSONWriteDocFromJSON(), and should
    // not be called directly
    // - will return JSON=nil in case of unexpected error in the supplied JSON
    procedure BSONWriteFromJSON(const name: RawUTF8; var JSON: PUTF8Char;
      EndOfObject: PUTF8Char; DoNotTryExtendedMongoSyntax: boolean = false);

    /// recursive writing of a BSON document or value from a TDocVariant
    // object or array, used e.g. by BSON(const doc: TDocVariantData) function
    // - caller should execute BSONAdjustDocumentsSize() on the resulting buffer
    // - this method will call BSONDocumentBegin/BSONDocumentEnd internally
    // - will raise an EBSONException if doc is not a valid TDocVariant or null
    // or if the resulting binary content is bigger than BSON_MAXDOCUMENTSIZE
    procedure BSONWriteDoc(const doc: TDocVariantData);
    /// write an object specified as name/value pairs as a BSON document
    // - data must be supplied two by two, as Name,Value pairs, e.g.
    // ! aBSONWriter.BSONWriteObject(['name','John','year',1972]);
    // - this method wil be faster than using a BSONWriteDoc(_ObjFast(...))
    procedure BSONWriteObject(const NameValuePairs: array of const);
    /// write a projection specified as fieldname:1 pairs as a BSON document
    procedure BSONWriteProjection(const FieldNamesCSV: RawUTF8);
    /// write an object as query parameter
    // - will handle all SQL operators, including IN (), IS NULL or LIKE
    // - see @http://docs.mongodb.org/manual/reference/operator/query
    // - inverted should be TRUE e.g. for a NOT ... expression
    // - returns TRUE on success, FALSE if the operator is not implemented yet
    function BSONWriteQueryOperator(name: RawUTF8; inverted: boolean;
      op: TSynTableStatementOperator; const Value: variant): boolean;
    /// write one array item, i.e. the ASCII index name as text
    // - only one level of array should be used per TBSONWriter class
    procedure BSONWriteArray(const kind: TBSONElementType); overload;
    /// write an array specified as a list of items as a BSON document
    // - data must be supplied as a list of values e.g.
    // ! aBSONWriter.BSONWriteArray(['John',1972]);
    // - this method wil be faster than using a BSONWriteDoc(_ArrFast(...))
    procedure BSONWriteArray(const Items: array of const); overload;
    /// write an array of integers as a BSON Document
    procedure BSONWriteArrayOfInteger(const Integers: array of integer);
    /// write an array of integers as a BSON Document
    procedure BSONWriteArrayOfInt64(const Integers: array of Int64);
    /// write some BSON document from a supplied (extended) JSON array or object
    // - warning: the incoming JSON buffer will be modified in-place: so you
    // should make a private copy before running this method (see e.g. TSynTempBuffer)
    // - will handle only '{ ... }', '[ ... ]' or 'null' input, with the standard
    // strict JSON format, or BSON-like extensions, e.g. unquoted field names:
    // $ {id:10,doc:{name:"John",birthyear:1972}}
    // - if DoNotTryExtendedMongoSyntax is default FALSE, then the MongoDB Shell
    // syntax will also be recognized to create BSON custom types, like
    // $ new Date()   ObjectId()   MinKey   MaxKey  /<jRegex>/<jOptions>
    // see @http://docs.mongodb.org/manual/reference/mongodb-extended-json
    // $ {id:new ObjectId(),doc:{name:"John",date:ISODate()}}
    // $ {name:"John",field:/acme.*corp/i}
    // - if DoNotTryExtendedMongoSyntax is TRUE, process may be slightly faster
    // - will create the BSON binary without any temporary TDocVariant storage
    function BSONWriteDocFromJSON(JSON: PUTF8Char; aEndOfObject: PUTF8Char;
      out Kind: TBSONElementType; DoNotTryExtendedMongoSyntax: boolean = false): PUTF8Char;

    /// to be called before a BSON document will be written
    // - each BSONDocumentBegin should be followed by its nested BSONDocumentEnd
    procedure BSONDocumentBegin; overload;
    /// to be called before a BSON document will be written
    // - each BSONDocumentBegin should be followed by its nested BSONDocumentEnd
    // - you could create a new BSON object by specifying a name and its
    // type, i.e. either betDoc or betArray
    procedure BSONDocumentBegin(const name: RawUTF8;
      kind: TBSONElementType = betDoc); overload;
    /// to be called before a BSON document will be written in an array
    // - only one level of array should be used per TBSONWriter class
    procedure BSONDocumentBeginInArray(const name: RawUTF8;
      kind: TBSONElementType = betDoc);
    /// to be called when a BSON document has been written
    // - it will store the current stream position into an internal array,
    // which will be written when you call AdjustDocumentsSize()
    // - you can optional specify how many nested documents should be closed,
    // and/or if it should not write an ending betEof item
    procedure BSONDocumentEnd(CloseNumber: integer = 1; WriteEndingZero: boolean = true);
    /// after all content has been written, call this method on the resulting
    // memory buffer to store all document size as expected by the standard
    procedure BSONAdjustDocumentsSize(BSON: PByteArray); virtual;
    /// flush the content and return the whole binary encoded stream
    // - call BSONAdjustDocumentsSize() to adjust all internal document sizes
    // - expect the TBSONWriter instance to have been created as such:
    // ! TBSONWriter.Create(TRawByteStringStream);
    procedure ToBSONDocument(var result: TBSONDocument); virtual;
    /// flush the content and return the whole document as a TBSONVariant
    // - call ToBSONDocument() to adjust all internal document sizes
    // - expect the TBSONWriter instance to have been created as such:
    // ! TBSONWriter.Create(TRawByteStringStream);
    procedure ToBSONVariant(var result: variant; Kind: TBSONElementType = betDoc);
  end;


{ ************ High-Level BSON/JSON Function Helpers }

const
  /// fake BSON element type which compares lower than all other possible values
  // - element type sounds to be stored as shortint, so here $ff=-1<0=betEOF
  // - defined as an integer to circumvent a compilation issue with FPC trunk
  betMinKey = $ff;
  /// fake BSON element type which compares higher than all other possible values
  // - element type sounds to be stored as shortint, so here betInt64=$12<$7f
  // - defined as an integer to circumvent a compilation issue with FPC trunk
  betMaxKey = $7f;

  /// kind of elements which will store a RawByteString/RawUTF8 content
  // within its TBSONVariant kind
  // - i.e. TBSONVariantData.VBlob/VText field is to be managed
  BSON_ELEMENTVARIANTMANAGED = [betBinary, betDoc, betArray, betRegEx,
    betDeprecatedDbptr, betTimestamp, betJSScope, betJS, betDeprecatedSymbol,
    betDecimal128];

  /// by definition, maximum MongoDB document size is 16 MB
  BSON_MAXDOCUMENTSIZE = 16 * 1024 * 1024;

  /// special JSON string content which will be used to store a betDeprecatedUndefined item
  // - *[false] is for strict JSON, *[true] for MongoDB Extended JSON
  BSON_JSON_UNDEFINED: array[boolean] of string[23] = (
    '{"$undefined":true}', 'undefined');
  /// special JSON string content which will be used to store a betMinKey item
  // - *[false] is for strict JSON, *[true] for MongoDB Extended JSON
  BSON_JSON_MINKEY: array[boolean] of string[15] = (
    '{"$minKey":1}', 'MinKey');
  /// special JSON string content which will be used to store a betMaxKey item
  // - *[false] is for strict JSON, *[true] for MongoDB Extended JSON
  BSON_JSON_MAXKEY: array[boolean] of string[15] = (
    '{"$maxKey":1}', 'MaxKey');
  /// special JSON patterns which will be used to format a betObjectID item
  // - *[false,*] is to be written before the hexadecimal ID, *[true,*] after
  BSON_JSON_OBJECTID: array[boolean, TMongoJSONMode] of string[15] = (
    ('"', '{"$oid":"', 'ObjectId("'),
    ('"', '"}', '")'));
  /// special JSON patterns which will be used to format a betBinary item
  // - *[false,*] is for strict JSON, *[true,*] for MongoDB Extended JSON
  BSON_JSON_BINARY: array[boolean, boolean] of string[15] = (
    ('{"$binary":"', '","$type":"'), ('BinData(', ',"'));
  /// special JSON string content which will be used to store a betDeprecatedDbptr
  // - *[false,*] is for strict JSON, *[true,*] for MongoDB Extended JSON
  // - (not used by now for this deprecated content)
  BSON_JSON_DBREF: array[boolean, 0..2] of string[15] = (
    ('{"$ref":"', '","$id":"', '"}'), ('DBRef("', '","', '")'));
  /// special JSON string content which will be used to store a betRegEx
  BSON_JSON_REGEX: array[0..2] of string[15] = (
    '{"$regex":"', '","$options":"', '"}');
  /// special JSON patterns which will be used to format a betDateTime item
  // - *[*,false] is to be written before the date value, *[*,true] after
  BSON_JSON_DATE: array[TMongoJSONMode, boolean] of string[15] = (
    ('"', '"'), ('{"$date":"', '"}'), ('ISODate("', '")'));
  /// special JSON patterns which will be used to format a betDecimal128 item
  // - *[false,*] is to be written before the decimal value, *[true,*] after
  BSON_JSON_DECIMAL: array[boolean, TMongoJSONMode] of string[23] = (
    ('"', '{"$numberDecimal":"', 'NumberDecimal("'), ('"', '"}', '")'));

var
  /// global TCustomVariantType used to register BSON variant types
  // - if you use this unit, both TDocVariant and TBSONVariant custom types
  // will be registered, since they are needed for any MongoDB / BSON process
  BSONVariantType: TBSONVariant;

/// ready-to-be displayed text of a TBSONElementType value
function ToText(kind: TBSONElementType): PShortString; overload;

/// create a TBSONVariant custom variant type containing a BSON Object ID
// - will be filled with some unique values, ready to create a new document key
// - will store a BSON element of betObjectID kind
function ObjectID: variant; overload;

/// create a TBSONVariant Object ID custom variant type from a supplied text
// - will raise an EBSONException if the supplied text is not valid hexadecimal
// - will set a BSON element of betObjectID kind
function ObjectID(const Hexa: RawUTF8): variant; overload;

/// convert a TBSONVariant Object ID custom variant into a TBSONObjectID
// - raise an exception if the supplied variant is not a TBSONVariant Object ID
function BSONObjectID(const aObjectID: variant): TBSONObjectID;

/// create a TBSONVariant JavaScript custom variant type from a supplied code
// - will set a BSON element of betJS kind
function JavaScript(const JS: RawUTF8): variant; overload;

/// create a TBSONVariant JavaScript and associated scope custom variant type
// from a supplied code and document
// - will set a BSON element of betJSScope kind
function JavaScript(const JS: RawUTF8; const Scope: TBSONDocument): variant; overload;

/// create a TBSONVariant Decimal128 from some text corresponding to
// a floating-point number
// - will store internally a TDecimal128 storage
function NumberDecimal(const Value: RawUTF8): variant; overload;

/// create a TBSONVariant Decimal128 from a currency fixed decimal
// - will store internally a TDecimal128 storage, with explictly 4 decimals
// - if you want to store some floating-point value, use plain BSON double format
function NumberDecimal(const Value: currency): variant; overload;

/// store some object content into BSON encoded binary
// - object will be initialized with data supplied two by two, as Name,Value
// pairs, e.g.:
// ! aBson := BSON(['name','John','year',1972]);
// - you can define nested arrays or objects as TDocVariant, e.g:
// ! aBSON := BSON(['bsonDat',_Arr(['awesome',5.05, 1986])]);
// - or you can specify nested arrays or objects with '['..']' or '{'..'}':
// ! aBSON := BSON(['BSON','[','awesome',5.05,1986,']'])
// ! u := BSONToJSON(BSON(['doc','{','name','John','year',1982,'}','id',123]));
// ! assert(u='{"doc":{"name":"John","year":1982},"id":123}');
// ! u := BSONToJSON(BSON(['doc','{','name','John','abc','[','a','b','c',']','}','id',123]));
// ! assert(u='{"doc":{"name":"John","abc":["a","b","c"]},"id":123}');
// - will create the BSON binary without any temporary TDocVariant storage
function BSON(const NameValuePairs: array of const): TBSONDocument; overload;

/// create a fields selector BSON document from a field names list
// - can be used via a TBSONVariant instance for the projection parameter of
// a TMongoRequestQuery, e.g.:
// ! BSONToJSON(BSONFieldSelector(['a','b','c']))='{"a":1,"b":1,"c":1}'
function BSONFieldSelector(const FieldNames: array of RawUTF8): TBSONDocument; overload;

/// create a fields selector BSON document from a CSV field names list
// - can be used via a TBSONVariant instance for the projection parameter of
// a TMongoRequestQuery, e.g.:
// ! BSONToJSON(BSONFieldSelector('a,b,c'))='{"a":1,"b":1,"c":1}'
function BSONFieldSelector(const FieldNamesCSV: RawUTF8): TBSONDocument; overload;

/// store some object content, supplied as (extended) JSON, into BSON encoded binary
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, e.g. unquoted field names:
// ! BSON('{id:10,doc:{name:"John",birthyear:1972}}');
// - MongoDB Shell syntax will also be recognized to create TBSONVariant, like
// ! new Date()   ObjectId()   MinKey   MaxKey  /<jRegex>/<jOptions>
// see @http://docs.mongodb.org/manual/reference/mongodb-extended-json
// ! BSON('{id:new ObjectId(),doc:{name:"John",date:ISODate()}}');
// ! BSON('{name:"John",field:/acme.*corp/i}');
// - will create the BSON binary without any temporary TDocVariant storage, by
// calling JSONBufferToBSONDocument() on a temporary copy of the supplied JSON
function BSON(const JSON: RawUTF8; kind: PBSONElementType = nil): TBSONDocument; overload;
  {$ifndef ISDELPHI20092010}{$ifdef HASINLINE}inline;{$endif}{$endif}

/// store some object content, supplied as (extended) JSON and parameters,
// into BSON encoded binary
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, e.g. unquoted field names
// - MongoDB Shell syntax will also be recognized to create TBSONVariant, like
// ! new Date()   ObjectId()   MinKey   MaxKey  /<jRegex>/<jOptions>
// see @http://docs.mongodb.org/manual/reference/mongodb-extended-json
// - typical use could be:
// ! BSON('{%:{$in:[?,?]}}',['type'],['food','snack']);
// ! BSON('{type:{$in:?}}',[],[_Arr(['food','snack'])]);
// ! BSON('{%:[?,?,?]}',['BSON'],['awesome',5.05,1986])
// ! BSON('{%:?}',['BSON'],[_Arr(['awesome',5.05,1986])])
// ! BSON('{name:?,field:/%/i}',['acme.*corp'],['John']);
// ! BSON('{id:new ObjectId(),doc:{name:?,date:ISODate(?)}}',[],['John',NowUTC]);
// - will create the BSON binary without any temporary TDocVariant storage,
// by calling JSONBufferToBSONDocument() on the generated JSON content
// - since all content will be transformed into JSON internally, use this
// method only if the supplied parameters are simple types, and identified
// explicitely via BSON-like extensions: any complex value (e.g. a TDateTime
// or a BSONVariant binary) won't be handled as expected - use the overloaded
// BSON() with explicit BSONVariant() name/value pairs instead

function BSON(const Format: RawUTF8; const Args, Params: array of const;
  kind: PBSONElementType = nil): TBSONDocument; overload;

/// store some TDocVariant custom variant content into BSON encoded binary
// - will write either a BSON object or array, depending of the internal
// layout of this TDocVariantData instance (i.e. Kind property value)
// - if supplied variant is not a TDocVariant, raise an EBSONException
function BSON(const doc: TDocVariantData): TBSONDocument; overload;

/// store an array of integer into BSON encoded binary
// - object will be initialized with data supplied e.g. as a TIntegerDynArray
function BSONFromIntegers(const Integers: array of integer): TBSONDocument;

/// store an array of 64 bit integer into BSON encoded binary
// - object will be initialized with data supplied e.g. as a TIntegerDynArray
function BSONFromInt64s(const Integers: array of Int64): TBSONDocument;

/// store some object content, supplied as (extended) JSON into BSON binary
// - warning: the supplied JSON buffer will be modified in-place, if necessary:
// so you should create a temporary copy before calling this function, or call
// BSON(const JSON: RawUTF8) function instead
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, e.g. unquoted field names
// - if DoNotTryExtendedMongoSyntax is FALSE, then MongoDB Shell syntax will
// also be recognized to create BSON custom values, like
// ! new Date()   ObjectId()   MinKey   MaxKey  /<jRegex>/<jOptions>
// see @http://docs.mongodb.org/manual/reference/mongodb-extended-json
// ! BSON('{id:new ObjectId(),doc:{name:"John",date:ISODate()}}');
// ! BSON('{name:"John",field:/acme.*corp/i}');
// - will create the BSON binary without any temporary TDocVariant storage
// - will return the kind of BSON document created, i.e. either betDoc or betArray
function JSONBufferToBSONDocument(JSON: PUTF8Char; var doc: TBSONDocument;
  DoNotTryExtendedMongoSyntax: boolean = false): TBSONElementType;

/// store one JSON array into an array of BSON binary
// - since BSON documents are limited to 16 MB by design, this function
// will allow to process huge data content, as soon as it is provided as array
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, e.g. unquoted field names
// - if DoNotTryExtendedMongoSyntax is FALSE, then MongoDB Shell syntax will
// be recognized to create BSON custom values - but it will be slightly slower
function JSONBufferToBSONArray(JSON: PUTF8Char; out docs: TBSONDocumentDynArray;
  DoNotTryExtendedMongoSyntax: boolean = false): boolean;

/// store some object content into a TBSONVariant betDoc type instance
// - object will be initialized with data supplied two by two, as Name,Value
// pairs, as expected by the corresponding overloaded BSON() function
function BSONVariant(const NameValuePairs: array of const): variant; overload;

/// create a fields selector BSON document from a field names list
// - can be used for the projection parameter of a TMongoRequestQuery, e.g.:
// ! VariantToJSON(BSONVariantFieldSelector(['a','b','c']))='{"a":1,"b":1,"c":1}'
function BSONVariantFieldSelector(const FieldNames: array of RawUTF8): variant; overload;

/// create a fields selector BSON document from a CSV field names list
// - can be used for the projection parameter of a TMongoRequestQuery, e.g.:
// ! VariantToJSON(BSONVariantFieldSelector('a,b,c'))='{"a":1,"b":1,"c":1}'
function BSONVariantFieldSelector(const FieldNamesCSV: RawUTF8): variant; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// store some object content, supplied as (extended) JSON, into a TBSONVariant
// betDoc type instance
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, as with the overloaded BSON() function

function BSONVariant(const JSON: RawUTF8): variant; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// store some object content, supplied as (extended) JSON, into a TBSONVariant
// betDoc type instance
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, as with the overloaded BSON() function
// - warning: this overloaded method will mofify the supplied JSON buffer
// in-place: you can use the overloaded BSONVariant(const JSON: RawUTF8) function
// instead if you do not want to modify the input buffer content

procedure BSONVariant(JSON: PUTF8Char; var result: variant); overload;

/// store some object content, supplied as (extended) JSON and parameters,
// into a TBSONVariant betDoc type instance
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, as with the overloaded BSON() function
function BSONVariant(const Format: RawUTF8;
  const Args, Params: array of const): variant; overload;

/// convert a TDocVariant variant into a TBSONVariant betDoc type instance
function BSONVariant(doc: TDocVariantData): variant; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// store an array of integer into a TBSONVariant betArray type instance
// - object will be initialized with data supplied e.g. as a TIntegerDynArray

function BSONVariantFromIntegers(const Integers: array of integer): variant;

/// store an array of 64 bit integer into a TBSONVariant betArray type instance
// - object will be initialized with data supplied e.g. as a TIntegerDynArray
function BSONVariantFromInt64s(const Integers: array of Int64): variant;

/// parse the header of a BSON encoded binary buffer, and return its length
// - BSON should point to a "int32 e_list #0" BSON document (like TBSONDocument)
// - if ExpectedBSONLen is set, this function will check that the supplied
// BSON content "int32" length matches the supplied value, and raise an
// EBSONException if this comparison fails
// - as an alternative, consider using TBSONIterator, which wrap both a PByte
// and a TBSONElement into one convenient item
function BSONParseLength(var BSON: PByte; ExpectedBSONLen: integer = 0): integer;

/// parse the next element in supplied BSON encoded binary buffer list
// - BSON should point to the "e_list" of the "int32 e_list #0" BSON document
// - will decode the supplied binary buffer as a variant, then it will let BSON
// point to the next element, and return TRUE
// - returns FALSE when you reached betEOF, so that you can use it in a loop:
// ! var bson: PByte;
// !     name: RawUTF8;
// !     value: variant;
// ! ...
// ! BSONParseLength(bson);
// ! while BSONParseNextElement(bson,name,value) do
// !   writeln(name,':',value);
// - by default, it will return TBSONVariant custom variants for documents or
// arrays - but if storeDocArrayAsDocVariant is set, it will return a
// TDocVariant custom kind of variant, able to access to its nested
// properties via late-binding
// - if you want to parse a BSON list as fast as possible, you should better use
// TBSONElement.FromNext() which avoid any memory allocation (the SAX way) - in
// fact, this function is just a wrapper around TBSONElement.FromNext + ToVariant
// - as an alternative, consider using TBSONIterator, which wrap both a PByte
// and a TBSONElement into one convenient item
function BSONParseNextElement(var BSON: PByte; var name: RawUTF8;
  var element: variant; DocArrayConversion: TBSONDocArrayConversion = asBSONVariant): boolean;

/// search for a property by number in a a supplied BSON encoded binary buffer
// - BSON should point to a "int32 e_list #0" BSON document (like TBSONDocument)
// - returns FALSE if the list has too few elements (starting at index 0)
// - otherwise, returns TRUE then let item point to the corresponding element
function BSONPerIndexElement(BSON: PByte; index: integer; var item: TBSONElement): boolean;

/// convert a BSON document into a TDocVariant variant instance
// - BSON should point to a "int32 e_list #0" BSON document
// - if ExpectedBSONLen is set, this function will check that the supplied
// BSON content "int32" length matches the supplied value
// - by definition, asBSONVariant is not allowed as Option value
procedure BSONToDoc(BSON: PByte; var Result: Variant; ExpectedBSONLen: integer = 0;
  Option: TBSONDocArrayConversion = asDocVariantPerReference);

/// convert a TBSONDocument into a TDocVariant variant instance
// - BSON should be valid BSON document (length will be checked against expected
// "int32 e_list #0" binary layout)
// - by definition, asBSONVariant is not allowed as Option value
function BSONDocumentToDoc(const BSON: TBSONDocument;
  Option: TBSONDocArrayConversion = asDocVariantPerReference): variant;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a BSON document into its JSON representation
// - BSON should point to a "int32 e_list #0" BSON document
// - Kind should be either betDoc or betArray
// - if ExpectedBSONLen is set, this function will check that the supplied
// BSON content "int32" length matches the supplied value
// - this function will use by default the MongoDB Extended JSON syntax for
// specific MongoDB objects but you may use modMongoShell if needed
function BSONToJSON(BSON: PByte; Kind: TBSONElementType;
  ExpectedBSONLen: integer = 0; Mode: TMongoJSONMode = modMongoStrict): RawUTF8;

/// convert a TBSONDocument into its JSON representation
// - BSON should be valid BSON document (length will be checked against expected
// "int32 e_list #0" binary layout)
// - this function will use by default the MongoDB Extended JSON syntax for
// specific MongoDB objects but you may use modMongoShell if needed
function BSONDocumentToJSON(const BSON: TBSONDocument;
  Mode: TMongoJSONMode = modMongoStrict): RawUTF8;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a BSON list of elements into its JSON representation
// - BSON should point to the "e_list" of the "int32 e_list #0" BSON document,
// i.e. the item data as expected by TBSONElement.FromNext()
// - this function will use by default the MongoDB Extended JSON syntax for
// specific MongoDB objects but you may use modMongoShell if needed
procedure BSONListToJSON(BSONList: PByte; Kind: TBSONElementType;
  W: TTextWriter; Mode: TMongoJSONMode = modMongoStrict);

/// convert any kind of BSON/JSON element, encoded as variant, into JSON
// - this function will use by default the MongoDB Extended JSON syntax for
// specific MongoDB objects but you may use modMongoShell if needed
procedure AddMongoJSON(const Value: variant; W: TTextWriter;
  Mode: TMongoJSONMode = modMongoStrict); overload;

/// convert any kind of BSON/JSON element, encoded as variant, into JSON
// - in addition to default modMongoStrict as rendered by VariantSaveJSON(),
// this function can render the supplied variant with the Mongo Shell syntax
// or even raw JSON content
function VariantSaveMongoJSON(const Value: variant; Mode: TMongoJSONMode): RawUTF8;


implementation


{ ************ BSON Decimal128 Value }

{ TDecimal128 }

// see https://github.com/mongodb/libbson/blob/master/src/bson/bson-decimal128.c

procedure TDecimal128.SetZero;
begin
  Bits.lo := 0;
  Bits.hi := BSON_DECIMAL128_HI_INT64POS;
end;

const
  D128: array[TDecimal128SpecialValue] of TDecimal128Bits = (
    // dsvError, dsvValue, dsvNan, dsvZero, dsvPosInf, dsvNegInf, dsvMin, dsvMax
    (
    lo: 0;
    hi: BSON_DECIMAL128_HI_NAN
  ), (
    lo: 0;
    hi: BSON_DECIMAL128_HI_NAN
  ), (
    lo: 0;
    hi: BSON_DECIMAL128_HI_NAN
  ), (
    lo: 0;
    hi: BSON_DECIMAL128_HI_INT64POS
  ), (
    lo: 0;
    hi: $7800000000000000
  ), (
    lo: 0;
    hi: QWord($f800000000000000)
  ), (
    lo: $378d8e63ffffffff;
    hi: QWord($dfffed09bead87c0)
  ), (
    lo: $378d8e63ffffffff;
    hi: $5fffed09bead87c0
  ));

procedure TDecimal128.SetSpecial(special: TDecimal128SpecialValue);
begin
  Bits := D128[special];
end;

function TDecimal128.IsSpecial: TDecimal128SpecialValue;
begin
  for result := dsvNan to high(D128) do
    if (D128[result].hi = Bits.hi) and
       (D128[result].lo = Bits.lo) then
      exit;
  result := dsvValue;
end;

procedure TDecimal128.FromInt32(value: integer);
begin
  if value >= 0 then
  begin
    Bits.lo := value;
    Bits.hi := BSON_DECIMAL128_HI_INT64POS;
  end
  else
  begin
    Bits.lo := -value;
    Bits.hi := QWord(BSON_DECIMAL128_HI_INT64NEG);
  end;
end;

procedure TDecimal128.FromUInt32(value: cardinal);
begin
  Bits.lo := value;
  Bits.hi := BSON_DECIMAL128_HI_INT64POS;
end;

procedure TDecimal128.FromInt64(value: Int64);
begin
  if value >= 0 then
  begin
    Bits.lo := value;
    Bits.hi := BSON_DECIMAL128_HI_INT64POS;
  end
  else
  begin
    Bits.lo := -value;
    Bits.hi := QWord(BSON_DECIMAL128_HI_INT64NEG);
  end;
end;

procedure TDecimal128.FromQWord(value: QWord);
begin
  Bits.lo := value;
  Bits.hi := BSON_DECIMAL128_HI_INT64POS;
end;

function TDecimal128.FromFloat(const value: TSynExtended; precision: integer): boolean;
var
  tmp: shortstring;
begin
  if (precision <= 0) or
     (precision = DOUBLE_PRECISION) then
    tmp[0] := AnsiChar(DoubleToShort(tmp, value))
  else
    tmp[0] := AnsiChar(ExtendedToShort(tmp, value, precision));
  result := true;
  case FloatToShortNan(tmp) of
    fnNan:
      SetSpecial(dsvNan);
    fnInf:
      SetSpecial(dsvPosInf);
    fnNegInf:
      SetSpecial(dsvNegInf);
  else
    result := FromText(@tmp[1], ord(tmp[0])) <> dsvError;
  end;
end;

procedure TDecimal128.FromCurr(const value: currency);
begin // force exactly 4 decimals
  if value < 0 then
  begin
    Bits.lo := -PInt64(@value)^;
    Bits.hi := QWord(BSON_DECIMAL128_HI_CURRNEG);
  end
  else
  begin
    Bits.lo := PInt64(@value)^;
    Bits.hi := BSON_DECIMAL128_HI_CURRPOS;
  end;
end;

function TDecimal128.Equals(const other: TDecimal128): boolean;
begin
  result := (Bits.lo = other.Bits.lo) and
            (Bits.hi = other.Bits.hi);
end;

function div128bits9digits(var value: THash128Rec): PtrUInt;
var
  r64: QWord;
  i: PtrInt;
begin
  r64 := 0;
  for i := 0 to high(value.c) do
  begin
    r64 := r64 shl 32;   // adjust remainder to match value of next dividend
    inc(r64, value.c[i]); // add the divided to _rem
    if r64 = 0 then
      continue;
    value.c[i] := r64 div 1000000000;
    dec(r64, QWord(value.c[i]) * 1000000000);
  end;
  result := r64;
end;

procedure append(var dest: PUTF8Char; var dig: PByte; digits: PtrInt);
  {$ifdef HASINLINE}inline;{$endif}
begin
  if digits > 0 then
    repeat
      dest^ := AnsiChar(dig^ + ord('0'));
      inc(dig);
      inc(dest);
      dec(digits);
      if digits = 0 then
        break;
    until false;
end;

function TDecimal128.ToText(out Buffer: TDecimal128Str): integer;
var
  dest: PUTF8Char;
  dig: PByte;
  exp, sciexp, signdig, radixpos, j, k: PtrInt;
  combi, biasedexp, signmsb: PtrUInt;
  leastdig, fastdiv: cardinal;
  digbuffer: array[0..35] of byte;
  _128: THash128Rec;
begin
  dest := @Buffer;
  if Bits.h < 0 then
  begin
    dest^ := '-';
    inc(dest);
  end;
  if (Bits.lo = 0) and
     (Bits.hi = 0) then
  begin
    dest^ := '0';
    result := 1;
    exit;
  end;
  combi := (Bits.c[3] shr 26) and $1f;
  if combi shr 3 = 3 then
    case combi of
      30:
        begin
          result := AppendRawUTF8ToBuffer(dest,
            DECIMAL128_SPECIAL_TEXT[dsvPosInf]) - PUTF8Char(@Buffer);
          exit;
        end;
      31:
        begin
          result := AppendRawUTF8ToBuffer(@Buffer,
            DECIMAL128_SPECIAL_TEXT[dsvNan]) - PUTF8Char(@Buffer);
          exit;
        end;
    else
      begin
        biasedexp := (Bits.c[3] shr 15) and $3fff;
        signmsb := ((Bits.c[3] shr 14) and 1) + 8;
      end;
    end
  else
  begin
    biasedexp := (Bits.c[3] shr 17) and $3fff;
    signmsb := (Bits.c[3] shr 14) and 7;
  end;
  exp := biasedexp - BSON_DECIMAL128_EXPONENT_BIAS;
  _128.c[0] := (Bits.c[3] and $3fff) + ((signmsb and $0f) shl 14);
  _128.c[1] := Bits.c[2];
  _128.c[2] := Bits.c[1];
  _128.c[3] := Bits.c[0];
  FillCharFast(digbuffer, sizeof(digbuffer), 0);
  dig := @digbuffer;
  if ((_128.lo = 0) and (_128.hi = 0)) or
     (_128.c[0] >= 1 shl 17) then
    signdig := 1 // non-canonical or zero -> 0
  else
  begin
    for k := 3 downto 0 do
    begin
      if (_128.lo = 0) and (_128.hi = 0) then
        break;
      leastdig := div128bits9digits(_128);
      if leastdig = 0 then
        continue;
      for j := 8 downto 0 do
      begin
        {$ifdef CPU32DELPHI}
        asm // Delphi compiler is not efficient about division
          mov     eax, leastdig
          mov     fastdiv, eax
          mov     edx, 3435973837
          mul     edx
          shr     edx, 3
          mov     leastdig, edx
        end;
        {$else}
        fastdiv := leastdig;
        leastdig := leastdig div 10; // FPC will use reciprocal division
        {$endif CPU32DELPHI}
        digbuffer[k * 9 + j] := fastdiv - leastdig * 10;
        if leastdig = 0 then
          break;
      end;
    end;
    signdig := 36; // 4*9 = k*j loops above
    while dig^ = 0 do
    begin
      dec(signdig);
      inc(dig);
    end;
  end;
  sciexp := signdig - 1 + exp;
  if (sciexp < -6) or
     (exp > 0) then
  begin
    // scientific format
    dest^ := AnsiChar(dig^ + ord('0'));
    inc(dig);
    inc(dest);
    dec(signdig);
    if signdig <> 0 then
    begin
      dest^ := '.';
      inc(dest);
      append(dest, dig, signdig);
    end;
    if sciexp > 0 then
      PWord(dest)^ := ord('E') + ord('+') shl 8
    else
    begin
      PWord(dest)^ := ord('E') + ord('-') shl 8;
      sciexp := -sciexp;
    end;
    dest := AppendUInt32ToBuffer(dest + 2, sciexp)
  end
  else
  begin
    if exp >= 0 then // regular format with no decimal place
      append(dest, dig, signdig)
    else
    begin
      radixpos := signdig + exp;
      if radixpos > 0 then // non-zero digits before radix
        append(dest, dig, radixpos)
      else
      begin
        dest^ := '0'; // leading zero before radix point
        inc(dest);
      end;
      dest^ := '.';   // radix char
      inc(dest);
      while radixpos < 0 do
      begin // leading zeros after radix
        dest^ := '0';
        inc(dest);
        inc(radixpos);
      end;
      append(dest, dig, signdig - radixpos);
    end;
  end;
  result := dest - PUTF8Char(@Buffer);
end;

function TDecimal128.ToText: RawUTF8;
var
  tmp: TDecimal128Str;
begin
  FastSetString(result, @tmp, ToText(tmp));
end;

procedure TDecimal128.ToText(var result: RawUTF8);
var
  tmp: TDecimal128Str;
begin
  FastSetString(result, @tmp, ToText(tmp));
end;

procedure TDecimal128.AddText(W: TTextWriter);
var
  tmp: TDecimal128Str;
begin
  W.AddNoJSONEscape(@tmp, ToText(tmp));
end;

function TDecimal128.ToVariant: variant;
begin
  ToVariant(result);
end;

procedure TDecimal128.ToVariant(out result: variant);
begin
  with TBSONVariantData(result) do
  begin
    VType := BSONVariantType.VarType;
    VKind := betDecimal128;
    VBlob := nil;
    SetString(RawByteString(VBlob), PAnsiChar(@Bits), sizeof(TDecimal128));
  end;
end;

function TDecimal128.ToFloat: TSynExtended;
var
  tmp: TDecimal128Str;
begin
  tmp[ToText(tmp)] := #0; // makes ASCIIZ temporary text conversion
  result := GetExtended(@tmp);
end;

function TDecimal128.ToCurr: currency;
begin
  ToCurr(result);
end;

procedure TDecimal128.ToCurr(out result: currency);
var
  tmp: TDecimal128Str;
  res64: Int64 absolute result;
begin
  if Bits.hi = QWord(BSON_DECIMAL128_HI_CURRNEG) then // was e.g. FromCurr
    res64 := -Bits.lo
  else if Bits.hi = BSON_DECIMAL128_HI_CURRPOS then
    res64 := Bits.lo
  else
  begin
    tmp[ToText(tmp)] := #0; // makes ASCIIZ temporary text conversion
    res64 := StrToCurr64(@tmp);
  end;
end;

function TDecimal128.FromText(text: PUTF8Char; textlen: integer): TDecimal128SpecialValue;
var
  P, PEnd: PUTF8Char;
  c: AnsiChar;
  flags: set of (negative, signed, radix, nonzero);
  digits: array[0..BSON_DECIMAL128_MAX_DIGITS - 1] of byte;
  firstnon0, digread, digstored, digcount, radixpos, digfirst, diglast, exp,
    signdig, i: PtrInt;
  signhi, signlo, biasedexp: QWord;
  sign: THash128Rec;
begin
  for result := dsvNan to dsvNegInf do
    if IdemPropNameU(DECIMAL128_SPECIAL_TEXT[result], text, textlen) then
    begin
      Bits := D128[result];
      exit; // fast recognition of special text values (including '0')
    end;
  Bits := D128[dsvError];
  result := dsvError;
  if (textlen = 0) or
     (text = nil) then
    exit;
  P := text;
  PEnd := text + textlen;
  flags := [];
  if P^ in ['+', '-'] then
  begin
    include(flags, signed);
    if P^ = '-' then
      include(flags, negative);
    inc(P);
  end;
  digcount := 0;
  digread := 0;
  digstored := 0;
  radixpos := 0;
  firstnon0 := 0;
  exp := 0;
  while P < PEnd do
  begin
    c := P^;
    case c of
      '.':
        if radix in flags then // duplicated '.'
          exit
        else
        begin
          include(flags, radix);
          inc(P);
          continue;
        end;
      '0'..'9':
        if digstored < BSON_DECIMAL128_MAX_DIGITS then
          if (c > '0') or
             (nonzero in flags) then
          begin
            if not (nonzero in flags) then
            begin
              firstnon0 := digread;
              include(flags, nonzero);
            end;
            digits[digstored] := ord(c) - ord('0');
            inc(digstored);
          end;
      'E', 'e':
        begin
          inc(P);
          if P >= PEnd then
            exit;
          exp := GetInteger(P, PEnd);
          break;
        end;
    else
      exit;
    end;
    if nonzero in flags then
      inc(digcount);
    if radix in flags then
      inc(radixpos);
    inc(digread);
    inc(P);
  end;
  if digread = 0 then
    exit;
  digfirst := 0;
  if digstored = 0 then
  begin // value is zero
    diglast := 0;
    digits[0] := 0;
    digcount := 1;
    digstored := 1;
    signdig := 0;
  end
  else
  begin
    diglast := digstored - 1;
    signdig := digcount;
    // handle trailing zeros as non-significant
    while text[firstnon0 + signdig - 1 + ord(radix in flags) + ord(signed in
      flags)] = '0' do
      dec(signdig);
  end;
  if (exp <= radixpos) and
     (radixpos - exp > 1 shl 14) then
    exp := BSON_DECIMAL128_EXPONENT_MIN
  else
    dec(exp, radixpos);
  while exp > BSON_DECIMAL128_EXPONENT_MAX do
  begin
    inc(diglast);
    digits[diglast] := 0;
    if diglast - digfirst > BSON_DECIMAL128_MAX_DIGITS then
      if signdig = 0 then
      begin // zero clamping is allowed
        exp := BSON_DECIMAL128_EXPONENT_MAX;
        break;
      end
      else
        exit; // overflow is not permitted
    dec(exp);
  end;
  while (exp < BSON_DECIMAL128_EXPONENT_MIN) or
        (digstored < digcount) do
  begin
    if diglast = 0 then
      if signdig = 0 then
      begin // zero clamping
        exp := BSON_DECIMAL128_EXPONENT_MIN;
        break;
      end
      else
        exit; // overflow
    if digstored < digcount then
      if (text[digcount - 1 + ord(signed in flags) +
          ord(radix in flags)] <> '0') and
         (signdig <> 0) then
        exit
      else // overflow
        dec(digcount)
    else // adjust to non stored digits
    if digits[diglast] <> 0 then
      exit
    else // inexact rounding
      dec(diglast); // adjust to round
    if exp < BSON_DECIMAL128_EXPONENT_MAX then
      inc(exp)
    else
      exit;
  end;
  if diglast - digfirst + 1 < signdig then
    if text[firstnon0 + diglast + ord(signed in flags) + ord(radix in flags)] <> '0' then
      exit; // inexact rouding
  signhi := 0;
  signlo := 0;
  if signdig <> 0 then // if not zero
    {$ifdef CPU32DELPHI} // use "shl" under x86 to avoid slower "call _llmul"
    if diglast - digfirst < 17 then
      for i := digfirst to diglast do
        inc(signlo, signlo + signlo shl 3 + digits[i])
    else
    begin
      for i := digfirst to diglast - 17 do
        inc(signhi, signhi + signhi shl 3 + digits[i]);
      for i := diglast - 16 to diglast do
        inc(signlo, signlo + signlo shl 3 + digits[i]);
    end;
    {$else}
    if diglast - digfirst < 17 then
      for i := digfirst to diglast do
        signlo := signlo * 10 + digits[i]
    else
    begin
      for i := digfirst to diglast - 17 do
        signhi := signhi * 10 + digits[i];
      for i := diglast - 16 to diglast do
        signlo := signlo * 10 + digits[i];
    end;
    {$endif CPU32DELPHI}
  if signhi = 0 then
  begin
    sign.L := signlo;
    sign.H := 0;
  end
  else
  begin
    mul64x64(signhi, 100000000000000000, sign);
    inc(sign.L, signlo);
    {$ifdef FPC}
    if sign.L < signlo then
    {$else} // manual QWord processs for less efficient Delphi compilers
    if (sign.c1 < TQWordRec(signlo).H) or
       ((sign.c1 = TQWordRec(signlo).H) and
        (sign.c0 < TQWordRec(signlo).L)) then
    {$endif FPC}
      inc(sign.H);
  end;
  biasedexp := exp + BSON_DECIMAL128_EXPONENT_BIAS;
  if (sign.H shr 49) and 1 <> 0 then
    Bits.hi := (QWord(3) shl 61) or ((biasedexp and $3fff) shl 47) or
               (sign.H and $7fffffffffff)
  else
    Bits.hi := ((biasedexp and $3fff) shl 49) or (sign.H and $1ffffffffffff);
  Bits.lo := sign.L;
  if negative in flags then
    Bits.c[3] := Bits.c[3] or $80000000;
  result := dsvValue;
end;

function TDecimal128.FromText(const text: RawUTF8): TDecimal128SpecialValue;
begin
  result := FromText(pointer(text), length(text));
end;

function TDecimal128.FromVariant(const value: variant): boolean;
var
  txt: RawUTF8;
  wasString: boolean;
  bson: TBSONVariantData absolute value;
  v64: Int64;
begin
  if bson.VType = varByRef or varVariant then
  begin
    result := FromVariant(PVariant(TVarData(value).VPointer)^);
    exit;
  end;
  if (bson.VType = BSONVariantType.VarType) and
     (bson.VKind = betDecimal128) then
    Bits := PDecimal128(bson.VBlob)^.Bits
  else if bson.VType = varWord64 then
    FromQWord(TVarData(value).VInt64)
  else if VariantToInt64(value, v64) then
    FromInt64(v64)
  else if bson.VType = varCurrency then
    FromCurr(TVarData(value).VCurrency)
  else
  begin
    VariantToUTF8(value, txt, wasString);
    result := FromText(txt) <> dsvError;
    exit;
  end;
  result := true;
end;


{ ************ BSON ObjectID Value }

{ TBSONObjectID }

const
  COUNTER_MASK = $ffffff;

var
  GlobalBSONObjectID: record
    Section: TRTLCriticalSection;
    Default: packed record
      Counter: cardinal;
      MachineID: TBSON24;
      ProcessID: word;
    end;
    LastCreateTime: cardinal;
    LastCounter: cardinal;
  end;

procedure InitBSONObjectIDComputeNew;
begin
  InitializeCriticalSection(GlobalBSONObjectID.Section);
  with GlobalBSONObjectID.Default do
  begin
    Counter := Random32 and COUNTER_MASK;
    with ExeVersion do
      PCardinal(@MachineID)^ := crc32c(crc32c(
        0, pointer(Host), length(Host)), pointer(User), length(User));
    ProcessID := crc32c(0, @MainThreadID, SizeOf(MainThreadID)); // lower 16-bit
  end;
end;

procedure TBSONObjectID.Init;
begin // 12 bytes fill zero
  with PHash128Rec(@self)^ do
  begin
    i0 := 0;
    i1 := 0;
    i2 := 0;
  end;
end;

procedure TBSONObjectID.ComputeNew;
var
  now, count: cardinal;
begin
  now := UnixTimeUTC; // fast API call (no need of cache)
  with GlobalBSONObjectID do
  begin
    EnterCriticalSection(Section);
    if now > LastCreateTime then
    begin
      LastCreateTime := now;
      count := Default.Counter; // reset
    end
    else
    begin
      count := LastCounter + 1;
      if count and COUNTER_MASK = Default.Counter then
      begin
        count := Default.Counter; // collision -> cheat on timestamp
        inc(LastCreateTime);
      end;
    end;
    Counter.b1 := count shr 16; // stored as bigendian
    Counter.b2 := count shr 8;
    Counter.b3 := count;
    LastCounter := count;
    UnixCreateTime := bswap32(LastCreateTime);
    MachineID := Default.MachineID;
    ProcessID := Default.ProcessID;
    LeaveCriticalSection(Section);
  end;
end;

function TBSONObjectID.Equal(const Another: TBSONObjectID): boolean;
begin // first check Counter last field, which is more likely to diverse
  {$ifdef CPU64}
  result := (PIntegerArray(@Self)[2] = PIntegerArray(@Another)[2]) and
            (PInt64(@Self)^ = PInt64(@Another)^);
  {$else}
  result := (PIntegerArray(@Self)[2] = PIntegerArray(@Another)[2]) and
            (PIntegerArray(@Self)[1] = PIntegerArray(@Another)[1]) and
            (PIntegerArray(@Self)[0] = PIntegerArray(@Another)[0]);
  {$endif CPU64}
end;

function TBSONObjectID.Equal(const Another: variant): boolean;
var
  oid2: TBSONObjectID;
begin
  result := oid2.FromVariant(Another) and Equal(oid2);
end;

function TBSONObjectID.CreateDateTime: TDateTime;
begin
  result := UnixTimeToDateTime(bswap32(UnixCreateTime));
end;

function TBSONObjectID.ToText: RawUTF8;
begin
  ToText(result);
end;

function TBSONObjectID.ToVariant: variant;
begin
  VarClear(result);
  with TBSONVariantData(result) do
  begin
    VType := BSONVariantType.VarType;
    VKind := betObjectID;
    VObjectID := self;
  end;
end;

procedure TBSONObjectID.ToVariant(var result: variant);
begin
  VarClear(result);
  with TBSONVariantData(result) do
  begin
    VType := BSONVariantType.VarType;
    VKind := betObjectID;
    VObjectID := self;
  end;
end;

function TBSONObjectID.FromText(const Text: RawUTF8): boolean;
begin
  if length(Text) = SizeOf(self) * 2 then
    result := mormot.core.text.HexToBin(Pointer(Text), @self, SizeOf(self))
  else
    result := false;
end;

function TBSONObjectID.FromText(Text: PUTF8Char): boolean;
begin
  result := mormot.core.text.HexToBin(Pointer(Text), @self, SizeOf(self));
end;

function TBSONObjectID.FromVariant(const value: variant): boolean;
var
  txt: RawUTF8;
  wasString: boolean;
  bson: PBSONVariantData;
begin
  bson := @value;
  if bson^.VType = varByRef or varVariant then
    bson := TVarData(value).VPointer;
  if (bson^.VType = BSONVariantType.VarType) and
     (bson^.VKind = betObjectID) then
  begin
    self := bson^.VObjectID;
    result := true;
  end
  else
  begin
    VariantToUTF8(value, txt, wasString);
    result := wasString and FromText(txt);
  end;
end;

procedure TBSONObjectID.ToText(var result: RawUTF8);
begin
  FastSetString(result, nil, sizeof(self) * 2);
  mormot.core.text.BinToHex(@self, pointer(result), sizeof(self));
end;



{ ************ TBSONVariantData / TBSONVariant Custom Variant Storage }

{ TBSONVariant }

procedure TBSONVariant.ToJSON(W: TTextWriter; const Value: variant;
  Escape: TTextWriterKind);
var
  item: TBSONElement;
  temp: RawByteString;
begin
  item.FromVariant('', Value, temp);
  item.AddMongoJSON(W, modMongoStrict);
end;

function TBSONVariant.GetNewDoc(const BSONDoc: TBSONDocument): variant;
begin
  FromBSONDocument(BSONDoc, result);
end;

function TBSONVariant.IsOfKind(const V: variant; Kind: TBSONElementType): boolean;
begin
  with TBSONVariantData(V) do
    if VType = varByRef or varVariant then
      result := IsOfKind(PVariant(TVarData(V).VPointer)^, Kind)
    else
      result := (self <> nil) and
                (VType = VarType) and
                (VKind = Kind);
end;

function TBSONVariant.ToBlob(const V: Variant; var Blob: RawByteString): boolean;
begin
  with TVarData(V) do
    if VType = varByRef or varVariant then
    begin
      result := ToBlob(PVariant(VPointer)^, Blob);
      exit;
    end;
  with TBSONVariantData(V) do
  begin
    result := (VType = VarType) and
              (VKind = betBinary);
    if result then
      if (VBlob = nil) or
         (PInteger(VBlob)^ <> Length(RawByteString(VBlob)) - (sizeof(integer) + 1)) then
        Blob := ''
      else
        SetString(Blob, PAnsiChar(VBlob) + (sizeof(integer) + 1), PInteger(VBlob)^);
  end;
end;

procedure TBSONVariant.FromBinary(const Bin: RawByteString;
  BinType: TBSONElementBinaryType; var result: variant);
var
  Len: integer;
begin // "\x05" e_name int32 subtype (byte*)
  VarClear(result);
  with TBSONVariantData(result) do
  begin
    if Bin = '' then
    begin
      VType := varNull; // stores a NULL
      exit;
    end;
    VType := VarType;
    VKind := betBinary;
    VBlob := nil; // avoid GPF here below
    Len := length(Bin);
    SetLength(RawByteString(VBlob), Len + (sizeof(integer) + 1));
    PInteger(VBlob)^ := Len;
    PByteArray(VBlob)^[sizeof(integer)] := ord(BinType);
    MoveFast(pointer(Bin)^, PByteArray(VBlob)^[sizeof(integer) + 1], Len);
  end;
end;

procedure TBSONVariant.FromBSONDocument(const BSONDoc: TBSONDocument;
  var result: variant; Kind: TBSONElementType);
begin
  VarClear(result);
  with TBSONVariantData(result) do
  begin
    VType := VarType;
    VKind := Kind;
    VBlob := nil; // avoid GPF here below
    RawByteString(VBlob) := BSONDoc;
  end;
end;

procedure TBSONVariant.FromJSON(json: PUTF8Char; var result: variant);
begin
  VarClear(result);
  if json = nil then
    exit;
  if json^ in [#1..' '] then
    repeat
      inc(json)
    until not (json^ in [#1..' ']);
  if json^ in ['{', '['] then
    with TBSONVariantData(result) do
    begin
      VType := VarType;
      VBlob := nil; // avoid GPF here below
      VKind := JSONBufferToBSONDocument(json, TBSONDocument(VBlob));
    end
  else
    VariantLoadJSON(result, json);
end;

const
  BSON_JSON_NEWDATE: string[8] = 'ew Date('; // circumvent Delphi XE4 Win64 bug

{$ifdef fpc} {$push} {$endif} {$hints off} // avoid hints with CompareMemFixed() inlining

function TBSONVariant.TryJSONToVariant(var JSON: PUTF8Char; var Value: variant;
  EndOfObject: PUTF8Char): boolean;
// warning: code should NOT modify JSON buffer in-place, unless it returns true
var
  bsonvalue: TBSONVariantData absolute Value;
  varvalue: TVarData absolute Value;

  procedure Return(kind: TBSONElementType; P: PUTF8Char; GotoEndOfObject: AnsiChar);
  begin
    if GotoEndOfObject <> #0 then
      while P^ <> GotoEndOfObject do
        if P^ = #0 then
        begin
          if kind in [betRegEx, betDecimal128] then
            RawByteString(bsonvalue.VBlob) := ''; // avoid memory leak
          exit;
        end
        else
          inc(P);
    P := GotoNextNotSpace(P + 1);
    if EndOfObject <> nil then
      EndOfObject^ := P^;
    if P^ <> #0 then
      JSON := P + 1
    else
      JSON := P;
    case kind of
      betObjectID, betRegEx, betDecimal128:
        begin // see TBSONWriter.BSONWrite()
          bsonvalue.VType := VarType;
          bsonvalue.VKind := kind;
        end;
      betDateTime:
        varvalue.VType := varDate;
    end;
    result := true;
  end;

  procedure ReturnInt(kindint: integer; P: PUTF8Char; GotoEndOfObject: AnsiChar);
  {$ifdef HASINLINE}inline;{$endif}
  // redirection function to circumvent FPC trunk limitation
  var
    kind: TBSONElementType absolute kindint;
  begin
    Return(kind, P, GotoEndOfObject);
  end;

  procedure TryDate(P: PUTF8Char; GotoEndOfObject: AnsiChar);
  var
    L: integer;
  begin
    P := GotoNextNotSpace(P);
    if GotoEndOfObject = ')' then
      if P^ = ')' then
      begin // new date() constructor
        varvalue.VDate := NowUTC;
        Return(betDateTime, P, #0);
        exit;
      end
      else if P^ in ['0'..'9'] then
      begin
        varvalue.VDate := GetNextItemDouble(P, ')');
        if (varvalue.VDate <> 0) and
           (P <> nil) then
        begin
          Return(betDateTime, P - 1, #0);
          exit;
        end;
      end;
    if P^ <> '"' then
      exit;
    if PCardinal(P)^ = JSON_SQLDATE_MAGIC_QUOTE then
      inc(P, 3); // ignore\uFFF1 code for DateTimeToSQL/TimeLogToSQL functions
    L := 1;
    while P[L] <> '"' do
      if P[L] <= ' ' then
        exit
      else
        inc(L);
    Iso8601ToDateTimePUTF8CharVar(P + 1, L, varvalue.VDate);
    if varvalue.VDate <> 0 then
      Return(betDateTime, P + L + 1, GotoEndOfObject);
  end;

  procedure TryObjectID(P: PUTF8Char; GotoEndOfObject: AnsiChar);
  begin
    P := GotoNextNotSpace(P);
    if (GotoEndOfObject = ')') and
       (P^ = ')') then
    begin // ObjectId() constructor
      bsonvalue.VObjectID.ComputeNew;
      Return(betObjectID, P, #0);
      exit;
    end;
    if P^ <> '"' then
      exit;
    if bsonvalue.VObjectID.FromText(P + 1) then
      Return(betObjectID, P + 25, GotoEndOfObject);
  end;

  procedure TryDecimal(P: PUTF8Char; GotoEndOfObject: AnsiChar);
  var
    dec: TDecimal128;
    L: integer;
  begin
    if P^ <> '"' then
      exit;
    inc(P);
    L := 0;
    while P[L] <> '"' do
      if not (P[L] in ['0'..'9', 'e', 'E', '+', '-', '.']) then
        exit
      else
        inc(L);
    if dec.FromText(P, L) = dsvError then
      exit;
    bsonvalue.VBlob := nil; // avoid GPF
    SetString(RawByteString(bsonvalue.VBlob), PAnsiChar(@dec), sizeof(TDecimal128));
    Return(betDecimal128, P + L + 1, GotoEndOfObject);
  end;

var
  Reg, Opt: PUTF8Char;
  RegLen, OptLen: Integer;

  procedure ReturnRegEx(P: PUTF8Char; GotoEndOfObject: AnsiChar);
  var
    buf: PAnsiChar;
  begin
    bsonvalue.VBlob := nil; // avoid GPF
    SetString(RawByteString(bsonvalue.VBlob), nil, RegLen + OptLen + 2);
    buf := bsonvalue.VBlob;
    MoveFast(Reg^, buf^, RegLen);
    inc(buf, RegLen);
    buf^ := #0;
    inc(buf);
    MoveFast(Opt^, buf^, OptLen);
    inc(buf, OptLen);
    buf^ := #0;
    Return(betRegEx, P, GotoEndOfObject);
  end;

  procedure TryRegExShell(P: PUTF8Char);
  begin
    RegLen := 0;
    while P[RegLen] <> '/' do
      if P[RegLen] <= ' ' then
        exit
      else
        inc(RegLen);
    Reg := P;
    inc(P, RegLen);
    if P^ <> '/' then
      exit
    else
      inc(P);
    OptLen := 0;
    while tcWord in TEXT_CHARS[P[OptLen]] do
      inc(OptLen);
    if P[OptLen] = #0 then
      exit;
    Opt := P;
    ReturnRegEx(Opt + OptLen - 1, #0);
  end;

  procedure TryRegExStrict(P: PUTF8Char);
  begin // warning: this won't escape double quotes...
    P := GotoNextNotSpace(P);
    if P^ <> '"' then
      exit
    else
      inc(P);
    RegLen := 0;
    while P[RegLen] <> '"' do
      if P[RegLen] <= ' ' then
        exit
      else
        inc(RegLen);
    Reg := P;
    P := GotoNextNotSpace(Reg + RegLen + 1);
    if P^ <> ',' then
      Exit; // $regex:"acme*.corp",$options:"i"}
    P := GotoNextNotSpace(P + 1);
    if P^ = '"' then
      inc(P);
    if PInt64(P)^ <> PInt64(@BSON_JSON_REGEX[1][4])^ then
      exit
    else
      inc(P, 8);
    if P^ = '"' then
      inc(P);
    P := GotoNextNotSpace(P);
    if P^ <> ':' then
      exit;
    P := GotoNextNotSpace(P + 1);
    if P^ <> '"' then
      exit
    else
      inc(P);
    OptLen := 0;
    while P[OptLen] <> '"' do
      if P[OptLen] <= ' ' then
        exit
      else
        inc(OptLen);
    Opt := P;
    ReturnRegEx(Opt + OptLen + 1, '}');
  end;

var
  P: PUTF8Char;
begin // here JSON does not start with " or 1..9 (obvious simple types)
  // see http://docs.mongodb.org/manual/reference/mongodb-extended-json
  result := false;
  case NormToUpperAnsi7[JSON^] of
    '{':
      begin // strict MongoDB objects e.g. {"$undefined":true} or {"$oid":".."}
        P := JSON;
        repeat
          inc(P)
        until not (P^ in [#1..' ']);
        if P^ <> '"' then
          exit;
        repeat
          inc(P)
        until not (P^ in [#1..' ']);
        if P[0] = '$' then
          case P[1] of
            'u':
              if CompareMemFixed(P + 2, @BSON_JSON_UNDEFINED[false][5], 10) then
                Return(betDeprecatedUndefined, P + 12, '}');
            'm':
              if CompareMemFixed(P + 1, @BSON_JSON_MINKEY[false][4], 8) then
                ReturnInt(betMinKey, P + 9, '}')
              else if CompareMemFixed(P + 1, @BSON_JSON_MAXKEY[false][4], 8) then
                ReturnInt(betMaxKey, P + 9, '}');
            'o':
              if PInteger(P + 2)^ = PInteger(@BSON_JSON_OBJECTID[false, modMongoStrict][5])^ then
                TryObjectID(P + 6, '}');
            'd':
              if CompareMemSmall(P + 2, @BSON_JSON_DATE[modMongoStrict, false][5], 5) then
                TryDate(P + 7, '}');
            'r':
              if CompareMemFixed(P, @BSON_JSON_REGEX[0][3], 8) then
                TryRegExStrict(P + 8);
            'n':
              if CompareMemFixed(P, @BSON_JSON_DECIMAL[false, modMongoStrict][3], 16) then
                TryDecimal(P + 16, '}');
          end;
      end;
    // MongoDB Shell Mode extended syntax
    'U':
      if StrCompIL(JSON + 1, @BSON_JSON_UNDEFINED[true][2], 8) = 0 then
        Return(betDeprecatedUndefined, JSON + 8, #0);
    'M':
      if StrCompIL(JSON + 1, @BSON_JSON_MINKEY[true][2], 5) = 0 then
        ReturnInt(betMinKey, JSON + 5, #0)
      else if StrCompIL(JSON + 1, @BSON_JSON_MAXKEY[true][2], 7) = 0 then
        ReturnInt(betMaxKey, JSON + 5, #0);
    'O':
      if StrCompIL(JSON + 1, @BSON_JSON_OBJECTID[false, modMongoShell][2], 8) = 0 then
        TryObjectID(JSON + 9, ')');
    'N':
      if StrCompIL(JSON + 1, @BSON_JSON_NEWDATE[1], 8) = 0 then
        TryDate(JSON + 9, ')')
      else if StrCompIL(JSON + 1, @BSON_JSON_DECIMAL[false, modMongoShell][2], 13) = 0 then
        TryDecimal(JSON + 14, ')');
    'I':
      if StrCompIL(JSON + 1, @BSON_JSON_DATE[modMongoShell, false][2], 7) = 0 then
        TryDate(JSON + 8, ')');
    '/':
      TryRegExShell(JSON + 1);
  end;
end;

{$ifdef fpc} {$pop} {$else} {$hints on} {$endif}

procedure TBSONVariant.Cast(var Dest: TVarData; const Source: TVarData);
begin
  CastTo(Dest, Source, VarType);
end;

procedure TBSONVariant.CastTo(var Dest: TVarData; const Source: TVarData;
  const AVarType: TVarType);
var
  tmp: RawUTF8;
  wasString: boolean;
begin
  if AVarType = VarType then
  begin
    VariantToUTF8(Variant(Source), tmp, wasString);
    if wasString then
    begin
      VarClear(variant(Dest));
      if TBSONVariantData(Dest).VObjectID.FromText(tmp) then
      begin
        Dest.VType := VarType;
        TBSONVariantData(Dest).VKind := betObjectID;
        exit;
      end;
      variant(Dest) := BSONVariant(tmp); // convert from JSON text
      exit;
    end;
    RaiseCastError;
  end
  else
  begin
    if Source.VType <> VarType then
      RaiseCastError;
    with TBSONVariantData(Source) do
      if (VKind = betObjectID) and
         (AVarType in [varDate, varDouble]) then
      begin
        Dest.VType := AVarType;
        Dest.VDate := VObjectID.CreateDateTime;
        exit;
      end
      else
      begin
        if VKind = betObjectID then
          VObjectID.ToText(tmp)
        else
          tmp := VariantSaveMongoJSON(variant(Source), modMongoShell);
        RawUTF8ToVariant(tmp, Dest, AVarType); // convert to JSON text
      end;
  end;
end;

procedure TBSONVariant.Clear(var V: TVarData);
begin
  if TBSONVariantData(V).VKind in BSON_ELEMENTVARIANTMANAGED then
    RawByteString(TBSONVariantData(V).VBlob) := '';
  ZeroFill(@V); // will set V.VType := varEmpty
end;

procedure TBSONVariant.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: Boolean);
begin
  if Indirect then
    SimplisticCopy(Dest, Source, true)
  else
  begin
    VarClear(variant(Dest)); // Dest may be a complex type
    Dest := Source;
    with TBSONVariantData(Dest) do
      if VKind in BSON_ELEMENTVARIANTMANAGED then
      begin
        VBlob := nil; // avoid GPF
        RawByteString(VBlob) := RawByteString(TBSONVariantData(Source).VBlob);
      end;
  end;
end;

procedure TBSONVariant.Compare(const Left, Right: TVarData;
  var Relationship: TVarCompareResult);
var
  res: integer;
  LeftU, RightU: RawUTF8;
begin
  LeftU := VariantSaveMongoJSON(variant(Left), modMongoStrict);
  RightU := VariantSaveMongoJSON(variant(Right), modMongoStrict);
  if LeftU = RightU then
    Relationship := crEqual
  else
  begin
    res := StrComp(pointer(LeftU), pointer(RightU));
    if res < 0 then
      Relationship := crLessThan
    else if res > 0 then
      Relationship := crGreaterThan
    else
      Relationship := crEqual;
  end;
end;



{ ************ TBSONElement/TBSONIterator for BSON Decoding }

// used by TBSONElement.ToVariant() method and BSONToDoc() procedure
procedure BSONItemsToDocVariant(Kind: TBSONElementType; BSON: PByte;
  var Doc: TDocVariantData; Option: TBSONDocArrayConversion);
const
  OPTIONS: array[TBSONDocArrayConversion] of TDocVariantOptions = (
    [],
    [dvoReturnNullForUnknownProperty],
    [dvoReturnNullForUnknownProperty, dvoValueCopiedByReference],
    [dvoReturnNullForUnknownProperty, dvoInternNames],
    [dvoReturnNullForUnknownProperty, dvoValueCopiedByReference, dvoInternNames]);
var
  k: TDocVariantKind;
  i, n, cap: integer;
  intnames: TRawUTF8Interning;
  items: array[0..63] of TBSONElement;
begin // very fast optimized code
  if BSON = nil then
    TVarData(Doc).VType := varNull
  else
  begin
    intnames := nil;
    case Kind of
      betDoc:
        begin
          k := dvObject;
          if dvoInternNames in Doc.Options then
            intnames := DocVariantType.InternNames;
        end;
      betArray:
        k := dvArray;
    else
      exit; // leave Doc=varEmpty
    end;
    Doc.Init(OPTIONS[Option], k);
    cap := 0;
    repeat // will handle up to 64 TBSONElement per loop (via items[])
      n := 0;
      while {%H-}items[n].FromNext(BSON) do
      begin
        inc(n);
        if n = length(items) then
          break; // avoid buffer overflow
      end;
      if n = 0 then
        break;
      inc(cap, n); // pre-allocate Doc.Names[]/Values[]
      if Doc.Capacity < cap then
        Doc.Capacity := NextGrow(cap); // faster for huge arrays
      for i := 0 to n - 1 do
      begin
        if Kind = betDoc then
          if intnames <> nil then
            intnames.Unique(Doc.Names[i + Doc.Count], items[i].Name, items[i].NameLen)
          else
            FastSetString(Doc.Names[i + Doc.Count], items[i].Name, items[i].NameLen);
        items[i].ToVariant(Doc.Values[i + Doc.Count], Option);
      end;
      Doc.SetCount(Doc.Count + n);
    until (BSON = nil) or
          (BSON^ = byte(betEOF));
  end;
end;


{ TBSONElement }

var
  /// size (in bytes) of a BSON element
  // - equals -1 for varying elements
  BSON_ELEMENTSIZE: array[TBSONElementType] of integer = (
    //betEOF, betFloat, betString, betDoc, betArray, betBinary,
    0, sizeof(Double), -1, -1, -1, -1,
    //betDeprecatedUndefined, betObjectID, betBoolean, betDateTime,
    0, sizeof(TBSONObjectID), 1, sizeof(Int64),
    //betNull, betRegEx, betDeprecatedDbptr, betJS, betDeprecatedSymbol,
    0, -1, -1, -1, -1,
    //betJSScope, betInt32, betTimestamp, betInt64, betDecimal128
    -1, sizeof(Integer), sizeof(Int64), SizeOf(Int64), Sizeof(TDecimal128));

  /// types which do not have an exact equivalency to a standard variant
  // type will be mapped as varUnknown - and will be changed into
  // BSONVariantType.VarType
  BSON_ELEMENTTYPES: array[TBSONElementType] of word = (
    //betEOF, betFloat, betString, betDoc, betArray, betBinary,
    varEmpty, varDouble, varString, varUnknown, varUnknown, varUnknown,
    //betDeprecatedUndefined, betObjectID, betBoolean, betDateTime,
    varEmpty, varUnknown, varBoolean, varDate,
    //betNull, betRegEx, betDeprecatedDbptr, betJS, betDeprecatedSymbol,
    varNull, varUnknown, varUnknown, varUnknown, varUnknown,
    //betJSScope, betInt32, betTimestamp, betInt64, betDecimal128
    varUnknown, varInteger, varUnknown, varInt64, varUnknown);

function TBSONElement.ToVariant(DocArrayConversion: TBSONDocArrayConversion): variant;
begin
  ToVariant(result, DocArrayConversion);
end;

procedure TBSONElement.ToVariant(var result: variant;
  DocArrayConversion: TBSONDocArrayConversion);
var
  res: TVarData absolute result;
  resBSON: TBSONVariantData absolute result;
begin
  VarClear(result);
  res.VAny := nil; // avoid GPF below
  case Kind of
    betFloat:
      res.VDouble := unaligned(PDouble(Element)^);
    betString:
      FastSetString(RawUTF8(res.VAny), Data.Text, Data.TextLen);
    betJS, betDeprecatedSymbol:
      FastSetString(RawUTF8(resBSON.VText), Data.Text, Data.TextLen);
    betDoc, betArray:
      if DocArrayConversion = asBSONVariant then
        SetString(TBSONDocument(resBSON.VBlob), PAnsiChar(Element), ElementBytes)
      else
      begin
        BSONItemsToDocVariant(Kind, Data.DocList, TDocVariantData(result), DocArrayConversion);
        exit;
      end;
    betBinary, betRegEx, betDeprecatedDbptr, betJSScope, betTimestamp, betDecimal128:
      SetString(RawByteString(resBSON.VBlob), PAnsiChar(Element), ElementBytes);
    betObjectID:
      resBSON.VObjectID := PBSONObjectID(Element)^;
    betBoolean:
      res.VBoolean := PBoolean(Element)^;
    betDateTime:
      res.VDate := UnixMSTimeToDateTime(PUnixMSTime(Element)^);
    betInt32:
      res.VInteger := PInteger(Element)^;
    betInt64:
      res.VInt64 := PInt64(Element)^;
  // betNull, betDeprecatedUndefined, betMinKey or betMaxKey has no data
  end;
  res.VType := BSON_ELEMENTTYPES[Kind];
  if res.VType = varUnknown then
  begin
    resBSON.VType := BSONVariantType.VarType;
    resBSON.VKind := Kind;
  end;
end;

function TBSONElement.ToInteger(const default: Int64): Int64;
begin
  case Kind of
    betBoolean:
      result := PByte(Element)^;
    betFloat:
      result := Trunc(unaligned(PDouble(Element)^));
    betInt32:
      result := PInteger(Element)^;
    betInt64:
      result := PInt64(Element)^;
  else
    result := default;
  end;
end;

function TBSONElement.ToRawUTF8: RawUTF8;

  procedure ComplexType;
  var
    V: variant;
    wasString: boolean;
  begin
    ToVariant(V);
    VariantToUTF8(V, result, wasString);
  end;

begin
  case Kind of
    betFloat:
      DoubleToStr(unaligned(PDouble(Element)^), result);
    betString:
      FastSetString(result, Data.Text, Data.TextLen);
    betInt32:
      Int32ToUtf8(PInteger(Element)^, result);
    betInt64:
      Int64ToUtf8(PInt64(Element)^, result);
    betDecimal128:
      PDecimal128(Element)^.ToText(result);
  else
    ComplexType;
  end;
end;

function TBSONElement.DocItemToVariant(const aName: RawUTF8; var aValue: variant;
  DocArrayConversion: TBSONDocArrayConversion): boolean;
var
  item: TBSONElement;
begin
  if (Kind in [betDoc, betArray]) and
     item.FromSearch(Data.DocList, aName) then
  begin
    item.ToVariant(aValue, DocArrayConversion);
    result := true;
  end
  else
    result := false;
end;

function TBSONElement.DocItemToRawUTF8(const aName: RawUTF8): RawUTF8;
var
  item: TBSONElement;
begin
  if (Kind in [betDoc, betArray]) and
     item.FromSearch(Data.DocList, aName) then
    result := item.ToRawUTF8
  else
    result := '';
end;

function TBSONElement.DocItemToInteger(const aName: RawUTF8; const default: Int64): Int64;
var
  item: TBSONElement;
begin
  if (Kind in [betDoc, betArray]) and
     item.FromSearch(Data.DocList, aName) then
    result := item.ToInteger(default)
  else
    result := default;
end;

procedure TBSONElement.AddMongoJSON(W: TTextWriter; Mode: TMongoJSONMode);
label
  Bin, regex;
begin
  case integer(Kind) of
    ord(betFloat):
      W.AddDouble(unaligned(PDouble(Element)^));
    ord(betString), ord(betJS), ord(betDeprecatedSymbol):
      begin
        W.Add('"');
        W.AddJSONEscape(Data.Text, Data.TextLen);
        W.Add('"');
      end;
    ord(betDoc), ord(betArray):
      BSONListToJSON(Data.DocList, Kind, W, Mode);
    ord(betObjectID):
      begin
        W.AddShort(BSON_JSON_OBJECTID[false, Mode]);
        W.AddBinToHex(Element, SizeOf(TBSONObjectID));
        W.AddShort(BSON_JSON_OBJECTID[true, Mode]);
      end;
    ord(betDeprecatedUndefined):
      W.AddShort(BSON_JSON_UNDEFINED[Mode = modMongoShell]);
    ord(betBinary):
      case Mode of
        modNoMongo:
          W.WrBase64(Data.Blob, Data.BlobLen, true);
        modMongoStrict:
          begin
            W.AddShort(BSON_JSON_BINARY[false, false]);
            W.WrBase64(Data.Blob, Data.BlobLen, false);
            W.AddShort(BSON_JSON_BINARY[false, true]);
            W.AddBinToHex(@Data.BlobSubType, 1);
            W.AddShort('"}');
          end;
        modMongoShell:
          begin
            W.AddShort(BSON_JSON_BINARY[true, false]);
            W.AddBinToHex(@Data.BlobSubType, 1);
            W.AddShort(BSON_JSON_BINARY[true, true]);
            W.WrBase64(Data.Blob, Data.BlobLen, false);
            W.AddShort('")');
          end;
      end;
    ord(betRegEx):
      case Mode of
        modNoMongo:
Bin:      W.WrBase64(Element, ElementBytes, true);
        modMongoStrict:
          goto regex;
        modMongoShell:
          if (PosChar(Data.RegEx, '/') = nil) and
             (PosChar(Data.RegExOptions, '/') = nil) then
          begin
            W.Add('/');
            W.AddNoJSONEscape(Data.RegEx, Data.RegExLen);
            W.Add('/');
            W.AddNoJSONEscape(Data.RegExOptions, Data.RegExOptionsLen);
          end
          else
          begin
regex:      W.AddShort(BSON_JSON_REGEX[0]);
            W.AddJSONEscape(Data.RegEx, Data.RegExLen);
            W.AddShort(BSON_JSON_REGEX[1]);
            W.AddJSONEscape(Data.RegExOptions, Data.RegExOptionsLen);
            W.AddShort(BSON_JSON_REGEX[2]);
          end;
      end;
    ord(betDeprecatedDbptr):
      goto Bin; // no specific JSON construct for this deprecated item
    ord(betJSScope):
      goto Bin; // no specific JSON construct for this item yet
    ord(betTimestamp):
      goto Bin; // internal content will always be written as raw binary
    ord(betBoolean):
      W.Add(PBoolean(Element)^);
    ord(betDateTime):
      begin
        W.AddShort(BSON_JSON_DATE[Mode, false]);
        W.AddUnixMSTime(Element, false);
        W.AddShort(BSON_JSON_DATE[Mode, true]);
      end;
    ord(betNull):
      W.AddShort('null');
    ord(betInt32):
      W.Add(PInteger(Element)^);
    ord(betInt64):
      W.Add(PInt64(Element)^);
    ord(betDecimal128):
      begin
        W.AddShort(BSON_JSON_DECIMAL[false, Mode]);
        PDecimal128(Element)^.AddText(W);
        W.AddShort(BSON_JSON_DECIMAL[true, Mode]);
      end;
    betMinKey:
      W.AddShort(BSON_JSON_MINKEY[Mode = modMongoShell]);
    betMaxKey:
      W.AddShort(BSON_JSON_MAXKEY[Mode = modMongoShell]);
  else
    raise EBSONException.CreateUTF8('TBSONElement.AddMongoJSON: unexpected type %',
      [integer(Kind)]);
  end;
end;

procedure TBSONElement.FromVariant(const aName: RawUTF8; const aValue: Variant;
  var aTemp: RawByteString);
const
  ELEMKIND: array[varEmpty..varWord64] of TBSONElementType = (
    betEOF, betNull,
    betInt32, betInt32, betFloat, betFloat, betFloat, betDateTime, betString,
    betEOF, betEOF, betBoolean, betEof, betEOF, betEOF, betEOF, betInt32,
    betInt32, betInt32, betInt64, betInt64, betInt64);
var
  v: PVarData;
  vbson: PBSONVariantData absolute v;
  vdoc: PDocVariantData absolute v;
  vt: cardinal;
label
  str, st2;
begin
  v := @aValue;
  while v.VType = varByRef or varVariant do
    v := v.VPointer;
  FillCharFast(self, sizeof(self), 0);
  Name := pointer(aName);
  NameLen := length(aName);
  vt := v.VType;
  case vt of
    0..varDate, varBoolean..high(ELEMKIND):
      begin // simple types
        Element := @Data.InternalStorage;
        Kind := ELEMKIND[vt];
        case Kind of
          betFloat:
            unaligned(PDouble(Element)^) := double(aValue);
          betDateTime:
            PUnixMSTime(Element)^ := DateTimeToUnixMSTime(v.VDate);
          betBoolean:
            PBoolean(Element)^ := v.VBoolean;
          betInt32:
            if not VariantToInteger(aValue, PInteger(Element)^) then
              raise EBSONException.Create('TBSONElement.FromVariant(betInt32)');
          betInt64:
            if not VariantToInt64(aValue, PInt64(Element)^) then
              raise EBSONException.Create('TBSONElement.FromVariant(betInt64)');
        end;
        ElementBytes := BSON_ELEMENTSIZE[Kind];
      end;
    varString:
      if (v.VAny <> nil) and
         (PInteger(v.VAny)^ and $ffffff = JSON_SQLDATE_MAGIC) and
         Iso8601CheckAndDecode(PUTF8Char(v.VAny) + 3, Length(RawUTF8(v.VAny)) - 3,
           PDateTime(@Data.InternalStorage)^) then
      begin
        // recognized TTextWriter.AddDateTime(woDateTimeWithMagic) ISO-8601 format
        Element := @Data.InternalStorage;
        Kind := betDateTime;
        ElementBytes := BSON_ELEMENTSIZE[betDateTime];
      end
      else
      begin
        Kind := betString;
        Data.Text := v.VAny;
        Data.TextLen := Length(RawUTF8(v.VAny));
st2:    ElementBytes := Data.TextLen + 1;
        if v.VAny = nil then
          Data.InternalStorage := 1
        else
          Element := nil; // special case handled by TBSONWriter.BSONWrite()
      end;
  {$ifdef HASVARUSTRING}
    varUString:
      begin
        RawUnicodeToUtf8(v.VAny, length(UnicodeString(v.VAny)), RawUTF8(aTemp));
        goto str;
      end;
  {$endif HASVARUSTRING}
    varOleStr:
      begin
        RawUnicodeToUtf8(v.VAny, length(WideString(v.VAny)), RawUTF8(aTemp));
str:    Kind := betString;
        Data.Text := pointer(aTemp);
        Data.TextLen := Length(aTemp);
        goto st2;
      end;
  else
    if vt = cardinal(BSONVariantType.VarType) then
    begin
      Kind := vbson.VKind;
      case Kind of
        betObjectID:
          FromBSON(@vbson.VObjectID); // stored inlined
      else
        FromBSON(vbson.VBlob); // complex type stored as a RawByteString
      end;
      if ElementBytes < 0 then
        raise EBSONException.CreateUTF8('TBSONElement.FromVariant(bson,%)',
          [ToText(Kind)^]);
    end
    else if vt = cardinal(DocVariantVType) then
    begin
      with TBSONWriter.Create(TRawByteStringStream) do // inlined BSON()
      try
        BSONWriteDoc(vdoc^);
        ToBSONDocument(aTemp);
      finally
        Free;
      end;
      if dvoIsObject in vdoc.Options then
        Kind := betDoc
      else if dvoIsArray in vdoc.Options then
        Kind := betArray
      else
        raise EBSONException.CreateUTF8('TBSONElement.FromVariant(doc,%)',
          [ToText(vdoc.Kind)^]);
      FromBSON(pointer(aTemp));
      if ElementBytes < 0 then
        raise EBSONException.CreateUTF8('TBSONElement.FromVariant(docbson,%)',
          [ToText(Kind)^]);
    end
    else
      raise EBSONException.CreateUTF8('TBSONElement.FromVariant(VType=%)', [v.VType]);
  end;
end;

function TBSONElement.FromDocument(const doc: TBSONDocument): boolean;
var
  n: Integer;
begin
  FillCharFast(self, sizeof(self), 0);
  n := length(doc);
  if (n >= 4) and
     (PInteger(doc)^ = n) then
  begin
    Kind := betDoc;
    FromBSON(pointer(doc));
    result := true;
  end
  else
    result := false;
end;

procedure TBSONElement.FromBSON(bson: PByte);
begin // see http://bsonspec.org/#/specification
  Element := bson;
  case Kind of // handle variable-size storage
    betString, betJS, betDeprecatedSymbol:
      begin
        // "\x02" e_name string
        ElementBytes := PInteger(bson)^ + sizeof(integer); // int32 (byte*) "\x00"
        Data.TextLen := PInteger(bson)^ - 1;
        inc(bson, sizeof(integer));
        Data.Text := pointer(bson);
      end;
    betDoc, betArray:
      begin
        // "\x03" e_name document
        ElementBytes := PInteger(bson)^;
        inc(bson, sizeof(integer)); // points to a "e_list #0"
        Data.DocList := bson;
      end;
    betBinary:
      begin
        // "\x05" e_name int32 subtype (byte*)
        ElementBytes := PInteger(bson)^ + (sizeof(integer) + 1);
        Data.BlobLen := PInteger(bson)^;
        inc(bson, sizeof(integer));
        Data.BlobSubType := TBSONElementBinaryType(bson^);
        inc(bson);
        Data.Blob := bson;
      end;
    betRegEx:
      begin
        // "\x0B" e_name cstring cstring
        Data.RegEx := Element;
        Data.RegExLen := StrLen(Data.RegEx);
        Data.RegExOptions := Data.RegEx + Data.RegExLen + 1;
        Data.RegExOptionsLen := StrLen(Data.RegExOptions);
        ElementBytes := Data.RegExLen + Data.RegExOptionsLen + 2;
      end;
    betJSScope:
      begin
        // "\x0F" e_name  int32 string document
        ElementBytes := PInteger(bson)^;
        inc(bson, sizeof(integer));
        Data.JavaScriptLen := PInteger(bson)^ - 1;
        inc(bson, sizeof(integer));
        Data.JavaScript := pointer(bson);
        inc(bson, Data.JavaScriptLen + 1);
        Data.ScopeDocument := bson;
      end;
  else
    if Kind > high(BSON_ELEMENTSIZE) then // e.g. betMinKey betMaxKey
      ElementBytes := 0
    else
      ElementBytes := BSON_ELEMENTSIZE[Kind]; // fixed size storage
  end;
end;

function TBSONElement.FromNext(var BSON: PByte): boolean;
begin
  if BSON = nil then
  begin
    result := false;
    exit;
  end;
  Kind := TBSONElementType(BSON^);
  case integer(Kind) of
    ord(betEOF):
      result := false;
    ord(betFloat)..ord(betDecimal128), betMinKey, betMaxKey:
      begin
        inc(BSON);
        Name := PUTF8Char(BSON);
        NameLen := StrLen(PUTF8Char(BSON));
        inc(BSON, NameLen + 1);
        FromBSON(BSON);
        if ElementBytes < 0 then
          raise EBSONException.CreateUTF8(
            'TBSONElement.FromNext: unexpected size % for type %',
            [ElementBytes, ord(Kind)]);
        inc(BSON, ElementBytes);
        inc(Index);
        result := true;
      end;
  else
    raise EBSONException.CreateUTF8(
      'TBSONElement.FromNext: unexpected type %',
      [ord(Kind)]);
  end;
end;

function TBSONElement.FromSearch(BSON: PByte; const aName: RawUTF8): boolean;
begin
  result := true;
  while FromNext(BSON) do
    if IdemPropNameU(aName, Name, NameLen) then
      exit;
  result := false;
end;


{ TBSONIterator }

function TBSONIterator.Init(const doc: TBSONDocument; kind: TBSONElementType): boolean;
var
  n: integer;
begin
  FillCharFast(self, sizeof(self), 0);
  n := length(doc);
  if (kind in [betDoc, betArray]) and
     (n >= 4) and
     (PInteger(doc)^ = n) then
  begin
    item.Kind := kind;
    item.FromBSON(pointer(doc));
    fBson := item.Data.DocList;
    result := true;
  end
  else
    result := false;
end;

function TBSONIterator.Next: boolean;
begin
  result := item.FromNext(fBson);
end;


{ ************ TBSONWriter for BSON Encoding }

{ TBSONWriter }

procedure TBSONWriter.CancelAll;
begin
  inherited;
  fDocumentCount := 0;
end;

procedure TBSONWriter.WriteCollectionName(Flags: integer; const CollectionName: RawUTF8);
begin
  Write4(Flags);
  if CollectionName = '' then
    raise EBSONException.Create('Missing collection name');
  Write(pointer(CollectionName), length(CollectionName) + 1); // +1 for #0
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; elemtype: TBSONElementType);
begin
  Write1(ord(elemtype));
  if name = '' then
    write1(0)
  else // write only #0
    {$ifdef HASINLINE}
    Write(pointer(name), length(name) + 1); // +1 for #0
    {$else}
    Write(pointer(name), PInteger(PtrInt(name) - sizeof(integer))^ + 1); // +1 for #0
    {$endif HASINLINE}
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; const value: integer);
begin
  BSONWrite(name, betInt32);
  Write4(value);
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; const value: Double);
begin
  BSONWrite(name, betFloat);
  Write8(@value);
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; const value: boolean);
begin
  BSONWrite(name, betBoolean);
  Write1(ord(value));
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; const value: Int64);
begin
  if (value >= low(integer)) and
     (value <= high(integer)) then
  begin
    BSONWrite(name, betInt32);
    Write4(value);
  end
  else
  begin
    BSONWrite(name, betInt64);
    Write8(@value);
  end;
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; const value: TBSONObjectID);
begin
  BSONWrite(name, betObjectID);
  Write(@value, sizeof(value));
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; const value: TDecimal128);
begin
  BSONWrite(name, betDecimal128);
  Write(@value, sizeof(value));
end;

procedure TBSONWriter.BSONWriteRegEx(const name: RawUTF8;
  const RegEx, Options: RawByteString);
begin
  BSONWrite(name, betRegEx); // cstring cstring
  Write(pointer(RegEx), length(RegEx));
  Write1(0);
  Write(pointer(Options), length(Options));
  Write1(0);
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; const value: RawUTF8;
  isJavaScript: boolean);
const
  TYP: array[boolean] of TBSONElementType = (betString, betJS);
var
  L: integer;
begin
  BSONWrite(name, TYP[isJavaScript]);
  L := length(value) + 1; // +1 for ending #0
  Write4(L);
  if L = 1 then
    Write1(0)
  else
    Write(pointer(value), L);
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; value: PUTF8Char);
var
  L: integer;
begin
  BSONWrite(name, betString);
  L := StrLen(value) + 1;
  Write4(L);
  if L = 1 then
    Write1(0)
  else
    Write(value, L);
end;

procedure TBSONWriter.BSONWriteString(const name: RawUTF8; value: PUTF8Char;
  valueLen: integer);
begin
  BSONWrite(name, betString);
  inc(valueLen);
  Write4(valueLen);
  if valueLen = 1 then
    Write1(0)
  else
    Write(value, valueLen);
end;

procedure TBSONWriter.BSONWriteDateTime(const name: RawUTF8; const value: TDateTime);
var
  ms: TUnixMSTime;
begin
  ms := DateTimeToUnixMSTime(value);
  BSONWrite(name, betDateTime);
  Write8(@ms);
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; Data: pointer; DataLen: integer);
begin
  BSONWrite(name, betBinary);
  Write4(DataLen);
  Write1(ord(bbtGeneric));
  Write(Data, DataLen);
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; const elem: TBSONElement);
begin
  BSONWrite(name, elem.Kind);
  if (elem.Element = nil) and // handle special case of TBSONElement.FromVariant()
     (elem.Kind in [betString, betJS, betDeprecatedSymbol]) then
  begin
    Write4(elem.Data.TextLen + 1); // int32 (byte*) "\x00"
    Write(elem.Data.Text, elem.Data.TextLen + 1);
  end
  else
    Write(elem.Element, elem.ElementBytes);
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; const bson: TBSONVariantData);
begin
  case bson.VKind of
    betObjectID:
      BSONWrite(name, bson.VObjectID);
  else
    begin
      BSONWrite(name, bson.VKind);
      WriteBinary(RawByteString(bson.VBlob));
    end;
  end;
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; const doc: TDocVariantData);
begin
  if dvoIsObject in doc.Options then
    BSONWrite(name, betDoc)
  else if dvoIsArray in doc.Options then
    BSONWrite(name, betArray)
  else
    raise EBSONException.Create('Undefined nested document');
  BSONWriteDoc(doc);
end;

procedure TBSONWriter.BSONWriteArray(const kind: TBSONElementType);
begin
  BSONWrite(UInt32ToUtf8(fDocumentArray), kind);
  inc(fDocumentArray);
  if kind in [betDoc, betArray] then
    BSONDocumentBegin;
end;

procedure TBSONWriter.BSONDocumentBegin;
begin
  if fDocumentStack >= Length(fDocumentStackOffset) then
    SetLength(fDocumentStackOffset, NextGrow(fDocumentStack));
  fDocumentStackOffset[fDocumentStack] := TotalWritten;
  inc(fDocumentStack);
  Write4(0);
end;

procedure TBSONWriter.BSONDocumentBegin(const name: RawUTF8; kind: TBSONElementType);
begin
  if not (kind in [betDoc, betArray]) then
    raise EBSONException.Create('BSONDocumentBegin(?)');
  BSONWrite(name, kind);
  BSONDocumentBegin;
end;

procedure TBSONWriter.BSONDocumentBeginInArray(const name: RawUTF8;
  kind: TBSONElementType);
begin
  if fDocumentArray > 0 then
    BSONDocumentEnd;
  BSONWriteArray(kind);
  BSONDocumentBegin(name);
end;

procedure TBSONWriter.BSONDocumentEnd(CloseNumber: integer; WriteEndingZero: boolean);
begin
  while CloseNumber > 0 do
  begin
    if (CloseNumber > 1) or
       WriteEndingZero then
      Write1(0);
    if fDocumentStack = 0 then
      raise EBSONException.CreateUTF8('Unexpected %.BSONDocumentEnd', [self]);
    dec(fDocumentStack);
    if fDocumentCount >= Length(fDocument) then
      SetLength(fDocument, NextGrow(fDocumentCount));
    with fDocument[fDocumentCount] do
    begin
      Offset := fDocumentStackOffset[fDocumentStack];
      Length := TotalWritten - Offset;
    end;
    inc(fDocumentCount);
    dec(CloseNumber);
  end;
end;

procedure TBSONWriter.BSONAdjustDocumentsSize(BSON: PByteArray);
var
  i: PtrInt;
begin
  for i := 0 to fDocumentCount - 1 do
    with fDocument[i] do
      PCardinal(@BSON[Offset])^ := Length;
end;

procedure TBSONWriter.ToBSONDocument(var result: TBSONDocument);
begin
  result := FlushTo;
  BSONAdjustDocumentsSize(pointer(result));
end;

procedure TBSONWriter.ToBSONVariant(var result: variant; Kind: TBSONElementType);
var
  doc: TBSONDocument;
begin
  ToBSONDocument(doc);
  BSONVariantType.FromBSONDocument(doc, result, Kind);
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; const value: TVarRec);
var
  tmp: RawUTF8;
begin
  case value.VType of
    vtBoolean:
      BSONWrite(name, value.VBoolean);
    vtInteger:
      BSONWrite(name, value.VInteger);
    vtCurrency:
      BSONWrite(name, value.VCurrency^);
    vtExtended:
      BSONWrite(name, value.VExtended^);
    vtVariant:
      BSONWriteVariant(name, value.VVariant^);
    vtInt64{$ifdef FPC}, vtQWord{$endif}:
      BSONWrite(name, value.VInt64^);
    vtString, vtAnsiString, {$ifdef HASVARUSTRING}vtUnicodeString, {$endif}
    vtPChar, vtChar, vtWideChar, vtWideString:
      begin
        VarRecToUTF8(value, tmp);
        BSONWrite(name, tmp);
      end;
  else
    raise EBSONException.CreateUtf8('%.BSONWrite(TVarRec.VType=%)', [self, value.VType]);
  end;
end;

procedure TBSONWriter.BSONWriteVariant(const name: RawUTF8; const value: variant);

  procedure WriteComplex;
  var
    temp: RawUTF8;
    JSON: PUTF8Char;
  begin
    with TVarData(value) do
      case VType of
      {$ifdef HASVARUSTRING}
        varUString:
          begin
            RawUnicodeToUtf8(VAny, length(UnicodeString(VAny)), temp);
            BSONWrite(name, temp);
          end;
      {$endif HASVARUSTRING}
        varOleStr:
          begin
            RawUnicodeToUtf8(VAny, length(WideString(VAny)), temp);
            BSONWrite(name, temp);
          end;
      else
        begin
          VariantSaveJSON(value, twJSONEscape, temp);
          JSON := pointer(temp);
          BSONWriteFromJSON(name, JSON, nil);
          if JSON = nil then
            raise EBSONException.CreateUTF8('%.BSONWriteVariant(VType=%)', [self, VType]);
        end;
      end;
  end;

var
  dt: TDateTime;
begin
  with TVarData(value) do
    case VType of
      varEmpty, varNull:
        BSONWrite(name, betNull);
      varSmallint:
        BSONWrite(name, VSmallInt);
      varShortInt:
        BSONWrite(name, VShortInt);
      varWord:
        BSONWrite(name, VWord);
      varLongWord:
        BSONWrite(name, VLongWord);
      varByte:
        BSONWrite(name, VByte);
      varBoolean:
        BSONWrite(name, VBoolean);
      varInteger:
        BSONWrite(name, VInteger);
      varWord64, varInt64:
        BSONWrite(name, VInt64);
      varSingle:
        BSONWrite(name, VSingle);
      varDouble:
        BSONWrite(name, VDouble);
      varDate:
        BSONWriteDateTime(name, VDate);
      varCurrency:
        BSONWrite(name, VCurrency);
      varString:
        if (VAny <> nil) and
           (PInteger(VAny)^ and $ffffff = JSON_SQLDATE_MAGIC) and
           Iso8601CheckAndDecode(PUTF8Char(VAny) + 3, Length(RawUTF8(VAny)) - 3, dt) then
          // recognized TTextWriter.AddDateTime(woDateTimeWithMagic) ISO-8601 format
          BSONWriteDateTime(name, dt)
        else
          BSONWrite(name, RawUTF8(VAny)); // expect UTF-8 content
    else
      if VType = varByRef or varVariant then
        BSONWriteVariant(name, PVariant(VPointer)^)
      else if VType = BSONVariantType.VarType then
        BSONWrite(name, TBSONVariantData(value))
      else if VType = DocVariantType.VarType then
        BSONWrite(name, TDocVariantData(value))
      else
        WriteComplex;
    end;
end;

procedure TBSONWriter.BSONWriteDoc(const doc: TDocVariantData);
var
  Name: RawUTF8;
  i: PtrInt;
begin
  BSONDocumentBegin;
  if doc.VarType > varNull then // null,empty will write {}
    if doc.VarType <> DocVariantType.VarType then
      raise EBSONException.CreateUTF8('%.BSONWriteDoc(VType=%)', [self, doc.VarType])
    else
      for i := 0 to doc.Count - 1 do
      begin
        if doc.Names <> nil then
          Name := doc.Names[i]
        else
          UInt32ToUtf8(i, Name);
        BSONWriteVariant(Name, doc.Values[i]);
        if TotalWritten > BSON_MAXDOCUMENTSIZE then
          raise EBSONException.CreateUTF8('%.BSONWriteDoc(size=%>max %)', [self,
            TotalWritten, BSON_MAXDOCUMENTSIZE]);
      end;
  BSONDocumentEnd;
end;

procedure TBSONWriter.BSONWriteProjection(const FieldNamesCSV: RawUTF8);
var
  FieldNames: TRawUTF8DynArray;
  i: PtrInt;
begin
  CSVToRawUTF8DynArray(pointer(FieldNamesCSV), FieldNames);
  BSONDocumentBegin;
  for i := 0 to high(FieldNames) do
    BSONWrite(FieldNames[i], 1);
  BSONDocumentEnd;
end;

function TBSONWriter.BSONWriteQueryOperator(name: RawUTF8; inverted: boolean; op:
  TSynTableStatementOperator; const Value: variant): boolean;
const
  QUERY_OPS: array[opNotEqualTo..opIn] of RawUTF8 = (
    '$ne', '$lt', '$lte', '$gt', '$gte', '$in');
  INVERT_OPS: array[opEqualTo..opGreaterThanOrEqualTo] of TSynTableStatementOperator = (
    opNotEqualTo, opEqualTo, opGreaterThanOrEqualTo, opGreaterThan,
    opLessThanOrEqualTo, opLessThan);
var
  wasString: boolean;
  like: RawUTF8;
  len: integer;
  doInvert: boolean;
begin
  result := false; // error on premature exit
  case op of
    // http://docs.mongodb.org/manual/faq/developers/#faq-developers-query-for-nulls
    // {$type:10} would return only existing fields, but our ODM do not insert
    // blobs by default -> do not use {$type:10} trick but plain {field:null}
    opIsNull:
      op := opEqualTo;     // here Value=null
    opIsNotNull:
      op := opNotEqualTo;  // here Value=null
  end;
  doInvert := false;
  if inverted then
    if op <= high(INVERT_OPS) then
      op := INVERT_OPS[op]
    else
    begin
      doInvert := true;
      BSONDocumentBegin(name);
      name := '$not';
    end;
  case op of
    opEqualTo:
      BSONWriteVariant(name, Value);
    opNotEqualTo..opIn:
      begin
        BSONDocumentBegin(name);
        BSONWriteVariant(QUERY_OPS[op], Value);
        BSONDocumentEnd;
      end;
    opLike:
      begin
        VariantToUTF8(Value, like, wasString);
        len := length(like);
        if (len = 0) or
           not wasString then
          exit;
        if like[1] = '%' then
          if len = 1 then
            // LIKE '%' is invalid
            exit
          else if like[len] = '%' then
            if len = 2 then
              // LIKE '%%' is invalid
              exit
            else
              // LIKE '%a%' -> /a/
              like := copy(like, 2, len - 2)
          else
            // LIKE '%a'  -> /a$/
            like := copy(like, 2, len - 1) + '$'
        else
        if like[len] = '%' then
          // LIKE 'a%'  -> /^a/
          like := '^' + copy(like, 1, len - 1)
        else
          // LIKE 'a'   -> /^a$/
          like := '^' + like + '$';
        BSONWriteRegEx(name, like, 'i'); // /like/i for case-insensitivity
      end;
    opContains:
      begin // http://docs.mongodb.org/manual/reference/operator/query/in
        BSONDocumentBegin(name);
        if _Safe(Value)^.Kind = dvArray then
          BSONWriteVariant(QUERY_OPS[opIn], Value)
        else
        begin
          BSONWrite(QUERY_OPS[opIn], betArray);
          BSONWriteArray([Value]);
        end;
        BSONDocumentEnd;
      end;
  else
    exit; // unhandled operator
  end;
  if doInvert then
    BSONDocumentEnd;
  result := true;
end;

procedure TBSONWriter.BSONWriteObject(const NameValuePairs: array of const);
var
  Name: RawUTF8;
  i: PtrInt;
begin
  BSONDocumentBegin;
  for i := 0 to (length(NameValuePairs) shr 1) - 1 do
  begin
    VarRecToUTF8(NameValuePairs[i * 2], Name);
    BSONWrite(Name, NameValuePairs[i * 2 + 1]);
  end;
  BSONDocumentEnd;
end;

procedure TBSONWriter.BSONWriteArray(const Items: array of const);
var
  i: PtrInt;
begin
  BSONDocumentBegin;
  for i := 0 to high(Items) do
    BSONWrite(UInt32ToUtf8(i), Items[i]);
  BSONDocumentEnd;
end;

procedure TBSONWriter.BSONWriteArrayOfInteger(const Integers: array of integer);
var
  i: PtrInt;
begin
  BSONDocumentBegin;
  for i := 0 to high(Integers) do
    BSONWrite(UInt32ToUtf8(i), Integers[i]);
  BSONDocumentEnd;
end;

procedure TBSONWriter.BSONWriteArrayOfInt64(const Integers: array of Int64);
var
  i: PtrInt;
begin
  BSONDocumentBegin;
  for i := 0 to high(Integers) do
    BSONWrite(UInt32ToUtf8(i), Integers[i]);
  BSONDocumentEnd;
end;

procedure TBSONWriter.BSONWriteFromJSON(const name: RawUTF8; var JSON: PUTF8Char;
  EndOfObject: PUTF8Char; DoNotTryExtendedMongoSyntax: boolean);
var
  tmp: variant;
  blob: RawByteString;
  wasString: boolean;
  Value: PUTF8Char;
  ValueLen: integer;
  VDouble: double;
  ValueDateTime: TDateTime absolute VDouble;
  VInt64: Int64 absolute VDouble;
  Kind: TBSONElementType;
begin
  if JSON^ in [#1..' '] then
    repeat
      inc(JSON)
    until not (JSON^ in [#1..' ']);
  if not DoNotTryExtendedMongoSyntax and
     BSONVariantType.TryJSONToVariant(JSON, tmp, EndOfObject) then
    // was betDateTime, betObjectID or betRegEx, from strict or extended JSON
    BSONWriteVariant(name, tmp)
  else    // try from simple types
    case JSON^ of
      #0:
        begin
          JSON := nil;
          exit;
        end;
      '[':
        begin // nested array
          BSONWrite(name, betArray);
          JSON := BSONWriteDocFromJSON(JSON, EndOfObject, Kind,
            DoNotTryExtendedMongoSyntax);
        end;
      '{':
        begin // nested document
          BSONWrite(name, betDoc);
          JSON := BSONWriteDocFromJSON(JSON, EndOfObject, Kind,
            DoNotTryExtendedMongoSyntax);
        end;
    else
      begin // simple types
        Value := GetJSONField(JSON, JSON, @wasString, EndOfObject, @ValueLen);
        if JSON = nil then
          JSON := @NULCHAR;
        if (Value = nil) or
           not wasString then
          if GetVariantFromNotStringJSON(Value, TVarData(tmp), true) then
          begin
            BSONWriteVariant(name, tmp); // null,boolean,Int64,double
            exit;
          end;
        // found no simple value -> check text value
        if Base64MagicCheckAndDecode(Value, ValueLen, blob) then
          // recognized '\uFFF0base64encodedbinary' pattern
          BSONWrite(name, pointer(blob), length(blob))
        else if Iso8601CheckAndDecode(Value, ValueLen, ValueDateTime) then
          // recognized TTextWriter.AddDateTime() pattern
          BSONWriteDateTime(name, ValueDateTime)
        else if (PInteger(Value)^ and $ffffff = JSON_SQLDATE_MAGIC) and
           Iso8601CheckAndDecode(Value + 3, ValueLen - 3, ValueDateTime) then
          // recognized TTextWriter.AddDateTime(woDateTimeWithMagic) pattern
          BSONWriteDateTime(name, ValueDateTime)
        else        // will point to the in-place escaped JSON text
          BSONWriteString(name, Value, ValueLen);
      end;
    end;
  if TotalWritten > BSON_MAXDOCUMENTSIZE then
    raise EBSONException.CreateUTF8('%.BSONWriteDoc(size=% > max=%)', [self,
      TotalWritten, BSON_MAXDOCUMENTSIZE]);
end;

function TBSONWriter.BSONWriteDocFromJSON(JSON: PUTF8Char; aEndOfObject:
  PUTF8Char; out Kind: TBSONElementType; DoNotTryExtendedMongoSyntax: boolean): PUTF8Char;
var
  ndx: cardinal;
  EndOfObject: AnsiChar;
  Name: RawUTF8;
begin
  result := nil;
  if JSON = nil then
    exit;
  if JSON^ in [#1..' '] then
    repeat
      inc(JSON)
    until not (JSON^ in [#1..' ']);
  case JSON^ of
    '[':
      begin
        Kind := betArray;
        BSONDocumentBegin;
        repeat
          inc(JSON)
        until not (JSON^ in [#1..' ']);
        ndx := 0;
        if JSON^ = ']' then
          inc(JSON)
        else
          repeat
            UInt32ToUtf8(ndx, Name);
            BSONWriteFromJSON(Name, JSON, @EndOfObject, DoNotTryExtendedMongoSyntax);
            if JSON = nil then
              exit; // invalid content
            inc(ndx);
          until EndOfObject = ']';
      end;
    '{':
      begin
        Kind := betDoc;
        BSONDocumentBegin;
        repeat
          inc(JSON)
        until not (JSON^ in [#1..' ']);
        if JSON^ = '}' then
          inc(JSON)
        else
          repeat
            // see http://docs.mongodb.org/manual/reference/mongodb-extended-json
            Name := GetJSONPropName(JSON); // BSON/JSON accepts "" as key name
            BSONWriteFromJSON(Name, JSON, @EndOfObject, DoNotTryExtendedMongoSyntax);
            if (JSON = nil) or
               (EndOfObject = #0) then
              exit; // invalid content
          until EndOfObject = '}';
      end;
    'n', 'N':
      if IdemPChar(JSON + 1, 'ULL') then
      begin // append null as {}
        Kind := betDoc;
        BSONDocumentBegin;
        inc(JSON, 4);
      end
      else
        exit;
  else
    exit;
  end;
  BSONDocumentEnd;
  if JSON^ in [#1..' '] then
    repeat
      inc(JSON)
    until not (JSON^ in [#1..' ']);
  if aEndOfObject <> nil then
    aEndOfObject^ := JSON^;
  if JSON^ <> #0 then
    repeat
      inc(JSON)
    until not (JSON^ in [#1..' ']);
  result := JSON; // indicates successfully parsed
end;


{ ************ High-Level BSON/JSON Function Helpers }

function BSONParseLength(var BSON: PByte; ExpectedBSONLen: integer): integer;
begin
  if (BSON = nil) or
     ((ExpectedBSONLen <> 0) and
      (PInteger(BSON)^ <> ExpectedBSONLen)) then
    raise EBSONException.Create('Incorrect supplied BSON document content');
  result := PInteger(BSON)^;
  inc(PInteger(BSON));
end;

function BSONParseNextElement(var BSON: PByte; var name: RawUTF8;
  var element: variant; DocArrayConversion: TBSONDocArrayConversion): boolean;
var
  item: TBSONElement;
begin
  result := item.FromNext(BSON);
  if result then
  begin
    FastSetString(name, item.Name, item.NameLen);
    item.ToVariant(element, DocArrayConversion);
  end;
end;

function BSONPerIndexElement(BSON: PByte; index: integer;
  var item: TBSONElement): boolean;
begin
  result := true;
  if (index >= 0) and
     (BSON <> nil) and
     (BSONParseLength(BSON) <> 0) then
    while item.FromNext(BSON) do
      if index = 0 then
        exit
      else
        dec(index);
  result := false;
end;

procedure BSONToDoc(BSON: PByte; var Result: Variant; ExpectedBSONLen: Integer;
  Option: TBSONDocArrayConversion);
begin
  if Option = asBSONVariant then
    raise EBSONException.Create('BSONToDoc(option=asBSONVariant) is not allowed');
  VarClear(Result);
  BSONParseLength(BSON, ExpectedBSONLen);
  BSONItemsToDocVariant(betDoc, BSON, TDocVariantData(Result), Option);
end;

function BSONDocumentToDoc(const BSON: TBSONDocument;
  Option: TBSONDocArrayConversion): variant;
begin
  BSONToDoc(pointer(BSON), result, length(BSON));
end;

procedure BSONListToJSON(BSONList: PByte; Kind: TBSONElementType; W: TTextWriter;
  Mode: TMongoJSONMode);
var
  item: TBSONElement;
begin
  case Kind of
    betDoc:
      if BSONList^ = byte(betEOF) then
        W.Add('{', '}')
      else
      begin
        W.Add('{');
        while item.FromNext(BSONList) do
        begin
          if Mode = modMongoShell then
          begin
            W.AddNoJSONEscape(item.Name, item.NameLen);
            W.Add(':');
          end
          else
            W.AddProp(item.Name, item.NameLen);
          item.AddMongoJSON(W, Mode);
          W.Add(',');
        end;
        W.CancelLastComma;
        W.Add('}');
      end;
    betArray:
      begin
        W.Add('[');
        while item.FromNext(BSONList) do
        begin
          item.AddMongoJSON(W, Mode);
          W.Add(',');
        end;
        W.CancelLastComma;
        W.Add(']');
      end;
  else
    raise EBSONException.CreateUTF8('BSONListToJSON(Kind=%)', [ord(Kind)]);
  end;
end;

function BSONDocumentToJSON(const BSON: TBSONDocument; Mode: TMongoJSONMode): RawUTF8;
begin
  result := BSONToJSON(pointer(BSON), betDoc, length(BSON), Mode);
end;

function BSONToJSON(BSON: PByte; Kind: TBSONElementType;
  ExpectedBSONLen: integer; Mode: TMongoJSONMode): RawUTF8;
var
  W: TTextWriter;
  tmp: TTextWriterStackBuffer;
begin
  BSONParseLength(BSON, ExpectedBSONLen);
  W := TTextWriter.CreateOwnedStream(tmp);
  try
    BSONListToJSON(BSON, Kind, W, Mode);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

procedure AddMongoJSON(const Value: variant; W: TTextWriter; Mode: TMongoJSONMode);

  procedure AddCustom;
  var
    item: TBSONElement;
    temp: RawByteString;
  begin
    item.FromVariant('', Value, temp);
    item.AddMongoJSON(W, Mode);
  end;

begin
  if TVarData(Value).VType < $10F then
    W.AddVariant(Value, twJSONEscape)
  else
    AddCustom; // sub-procedure to avoid implicit try..finally
end;

function VariantSaveMongoJSON(const Value: variant; Mode: TMongoJSONMode): RawUTF8;
var
  W: TTextWriter;
  tmp: TTextWriterStackBuffer;
begin
  W := TTextWriter.CreateOwnedStream(tmp);
  try
    AddMongoJSON(Value, W, Mode);
    W.SetText(result);
  finally
    W.Free;
  end;
end;


{ main BSON* functions }

function ToText(kind: TBSONElementType): PShortString;
begin
  result := GetEnumName(TypeInfo(TBSONElementType), ord(kind));
end;

function ToText(spec: TDecimal128SpecialValue): PShortString;
begin
  result := GetEnumName(TypeInfo(TDecimal128SpecialValue), ord(spec));
end;

function ObjectID: variant;
var
  ID: TBSONObjectID;
begin
  ID.ComputeNew;
  ID.ToVariant(result);
end;

function ObjectID(const Hexa: RawUTF8): variant;
var
  ID: TBSONObjectID;
begin
  if ID.FromText(Hexa) then
    ID.ToVariant(result)
  else
    raise EBSONException.CreateUTF8('Invalid ObjectID("%")', [Hexa]);
end;

function BSONObjectID(const aObjectID: variant): TBSONObjectID;
begin
  if not result.FromVariant(aObjectID) then
    raise EBSONException.Create('BSONObjectID() over not ObjectID variant');
end;

function JavaScript(const JS: RawUTF8): variant;
begin
  VarClear(result);
  with TBSONVariantData(result) do
  begin
    VType := BSONVariantType.VarType;
    VKind := betJS;
    VText := nil; // avoid GPF
    RawUTF8(VText) := JS;
  end;
end;

function JavaScript(const JS: RawUTF8; const Scope: TBSONDocument): variant;
var
  Len, JSLen: integer;
begin
  VarClear(result);
  with TBSONVariantData(result) do
  begin
    VType := BSONVariantType.VarType;
    VKind := betJSScope;
    JSLen := Length(JS) + 1;                            // string = int32 text#0
    Len := SizeOf(integer) * 2 + JSLen + length(Scope); // int32 string document
    VBlob := nil; // avoid GPF
    SetLength(RawByteString(VBlob), Len);
    PIntegerArray(VBlob)^[0] := Len;                    // length:int32
    PIntegerArray(VBlob)^[1] := JSLen;                  // string:int32
    MoveFast(pointer(JS)^, PAnsiChar(VBlob)[8], JSLen); // string:text#0
    MoveFast(pointer(Scope)^, PAnsiChar(VBlob)[8 + JSLen], Length(Scope)); // document
  end;
end;

function NumberDecimal(const Value: RawUTF8): variant;
var
  dec: TDecimal128;
begin
  if dec.FromText(Value) = dsvError then
    raise EBSONException.CreateUTF8('Invalid NumberDecimal("%")', [Value]);
  dec.ToVariant(result);
end;

function NumberDecimal(const Value: currency): variant;
var
  dec: TDecimal128;
begin
  dec.FromCurr(Value);
  dec.ToVariant(result);
end;

function bson(const doc: TDocVariantData): TBSONDocument;
begin
  if doc.VarType = varVariant or varByRef then
  begin
    result := bson(PDocVariantData(TVarData(doc).VPointer)^);
    exit;
  end;
  if doc.VarType <> DocVariantType.VarType then
    raise EBSONException.Create('doc is not a TDocVariant');
  with TBSONWriter.Create(TRawByteStringStream) do
  try
    BSONWriteDoc(doc);
    ToBSONDocument(result);
  finally
    Free;
  end;
end;

function BSONFromIntegers(const Integers: array of integer): TBSONDocument;
begin
  with TBSONWriter.Create(TRawByteStringStream) do
  try
    BSONWriteArrayOfInteger(Integers);
    ToBSONDocument(result);
  finally
    Free;
  end;
end;

function BSONFromInt64s(const Integers: array of Int64): TBSONDocument;
begin
  with TBSONWriter.Create(TRawByteStringStream) do
  try
    BSONWriteArrayOfInt64(Integers);
    ToBSONDocument(result);
  finally
    Free;
  end;
end;

function bson(const NameValuePairs: array of const): TBSONDocument;
var
  W: TBSONWriter;
  name: RawUTF8;
  a: Integer;

  procedure WriteValue;
  var
    ndx: cardinal;
  begin
    case VarRecAsChar(NameValuePairs[a]) of
      ord('['):
        begin
          W.BSONDocumentBegin(name, betArray);
          ndx := 0;
          repeat
            inc(a);
            if VarRecAsChar(NameValuePairs[a]) = ord(']') then
              break;
            UInt32ToUtf8(ndx, name);
            WriteValue;
            inc(ndx);
          until a = high(NameValuePairs);
          W.BSONDocumentEnd;
        end;
      ord('{'):
        begin
          W.BSONDocumentBegin(name, betDoc);
          repeat
            inc(a);
            VarRecToUTF8(NameValuePairs[a], name);
            if (a = high(NameValuePairs)) or
               (name = '}') then
              break;
            inc(a);
            WriteValue;
          until a = high(NameValuePairs);
          W.BSONDocumentEnd;
        end
    else
      W.BSONWrite(name, NameValuePairs[a]);
    end;
  end;

begin
  W := TBSONWriter.Create(TRawByteStringStream);
  try
    W.BSONDocumentBegin;
    a := 0;
    while a < high(NameValuePairs) do
    begin
      VarRecToUTF8(NameValuePairs[a], name);
      inc(a);
      WriteValue;
      inc(a);
    end;
    W.BSONDocumentEnd;
    W.ToBSONDocument(result);
  finally
    W.Free;
  end;
end;

function BSONFieldSelector(const FieldNames: array of RawUTF8): TBSONDocument;
var
  i: integer;
  W: TBSONWriter;
begin
  W := TBSONWriter.Create(TRawByteStringStream, 512);
  try
    W.BSONDocumentBegin;
    for i := 0 to high(FieldNames) do
      W.BSONWrite(FieldNames[i], 1);
    W.BSONDocumentEnd;
    W.ToBSONDocument(result);
  finally
    W.Free;
  end;
end;

function BSONFieldSelector(const FieldNamesCSV: RawUTF8): TBSONDocument;
var
  FieldNames: TRawUTF8DynArray;
begin
  CSVToRawUTF8DynArray(pointer(FieldNamesCSV), FieldNames);
  result := BSONFieldSelector(FieldNames);
end;

function JSONBufferToBSONDocument(JSON: PUTF8Char; var doc: TBSONDocument;
  DoNotTryExtendedMongoSyntax: boolean): TBSONElementType;
var
  W: TBSONWriter;
begin
  W := TBSONWriter.Create(TRawByteStringStream);
  try
    W.BSONWriteDocFromJSON(JSON, nil, result, DoNotTryExtendedMongoSyntax);
    W.ToBSONDocument(doc);
  finally
    W.Free;
  end;
end;

function JSONBufferToBSONArray(JSON: PUTF8Char; out docs: TBSONDocumentDynArray;
  DoNotTryExtendedMongoSyntax: boolean): boolean;
var
  W: TBSONWriter;
  doc: TBSONDocument;
  EndOfObject: AnsiChar;
  Kind: TBSONElementType;
  n: integer;
begin
  result := false;
  if JSON = nil then
    exit;
  JSON := GotoNextNotSpace(JSON);
  if JSON^ <> '[' then
    exit;
  JSON := GotoNextNotSpace(JSON + 1);
  n := 0;
  W := TBSONWriter.Create(TRawByteStringStream, 16384);
  try
    repeat
      JSON := W.BSONWriteDocFromJSON(JSON, @EndOfObject, Kind,
        DoNotTryExtendedMongoSyntax);
      if JSON = nil then
        exit;
      W.ToBSONDocument(doc);
      if n >= length({%H-}docs) then
        SetLength(docs, NextGrow(n));
      docs[n] := doc;
      inc(n);
      W.CancelAll;
    until EndOfObject = ']';
  finally
    W.Free;
  end;
  SetLength(docs, n);
  result := true;
end;

function BSON(const Format: RawUTF8; const Args, Params: array of const; kind:
  PBSONElementType): TBSONDocument;
var
  JSON: RawUTF8; // since we use FormatUTF8(), TSynTempBuffer is useless here
  v: variant;
  k: TBSONElementType;
begin
  if (Format = '?') and
     (high(Params) >= 0) then
  begin
    VarRecToVariant(Params[0], v);
    if DocVariantType.IsOfType(v) then
    begin
      result := bson(TDocVariantData(v));
      if kind <> nil then
        if TDocVariantData(v).kind = dvArray then
          kind^ := betArray
        else
          kind^ := betDoc;
      exit;
    end;
  end;
  JSON := FormatUTF8(Format, Args, Params, true);
  UniqueRawUTF8(JSON); // ensure Format is untouched if Args=[]
  k := JSONBufferToBSONDocument(pointer(JSON), result);
  if kind <> nil then
    kind^ := k;
end;

function BSON(const JSON: RawUTF8; kind: PBSONElementType): TBSONDocument;
var
  tmp: TSynTempBuffer;
  k: TBSONElementType;
begin
  tmp.Init(JSON);
  try
    k := JSONBufferToBSONDocument(tmp.buf, result);
    if kind <> nil then
      kind^ := k;
  finally
    tmp.Done;
  end;
end;

function BSONVariant(const NameValuePairs: array of const): variant;
begin
  BSONVariantType.FromBSONDocument(bson(NameValuePairs), result, betDoc);
end;

function BSONVariant(const JSON: RawUTF8): variant;
var
  k: TBSONElementType;
  b: RawByteString;
begin
  b := bson(JSON, @k);
  BSONVariantType.FromBSONDocument(b, result, k);
end;

procedure BSONVariant(JSON: PUTF8Char; var result: variant);
var
  tmp: TBSONDocument;
begin
  JSONBufferToBSONDocument(JSON, tmp);
  BSONVariantType.FromBSONDocument(tmp, result);
end;

function BSONVariant(const Format: RawUTF8; const Args, Params: array of const):
  variant; overload;
var
  k: TBSONElementType;
  b: RawByteString;
begin
  b := bson(Format, Args, Params, @k);
  BSONVariantType.FromBSONDocument(b, result, k);
end;

function BSONVariant(doc: TDocVariantData): variant;
var
  k: TBSONElementType;
begin
  if doc.Kind = dvArray then
    k := betArray
  else
    k := betDoc;
  BSONVariantType.FromBSONDocument(bson(doc), result, k);
end;

function BSONVariantFieldSelector(const FieldNames: array of RawUTF8): variant;
begin
  BSONVariantType.FromBSONDocument(BSONFieldSelector(FieldNames), result);
end;

function BSONVariantFieldSelector(const FieldNamesCSV: RawUTF8): variant;
begin
  BSONVariantType.FromBSONDocument(BSONFieldSelector(FieldNamesCSV), result);
end;

function BSONVariantFromIntegers(const Integers: array of integer): variant;
begin
  BSONVariantType.FromBSONDocument(BSONFromIntegers(Integers), result, betArray);
end;

function BSONVariantFromInt64s(const Integers: array of Int64): variant;
begin
  BSONVariantType.FromBSONDocument(BSONFromInt64s(Integers), result, betArray);
end;

initialization
  Assert(sizeof(TDecimal128)=16);
  Assert(ord(betEof)=$00);
  Assert(ord(betInt64)=$12);
  Assert(ord(betDecimal128)=$13);
  Assert(ord(bbtGeneric)=$00);
  Assert(ord(bbtMD5)=$05);
  Assert(ord(bbtUser)=$80);
  Assert(sizeof(TBSONObjectID)=12);
  Assert(sizeof(TBSONVariantData)=sizeof(variant));
  BSONVariantType := SynRegisterCustomVariantType(TBSONVariant) as TBSONVariant;
  InitBSONObjectIDComputeNew;


end.

