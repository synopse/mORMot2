/// Framework Core Low-Level Memory Buffer Process
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.buffers;

{
  *****************************************************************************

   Low-Level Memory Buffers Processing Functions shared by all framework units
   - Variable Length Integer Encoding / Decoding
   - TAlgoCompress Compression/Decompression Classes - with AlgoSynLZ
   - TFastReader / TBufferWriter Binary Streams
   - Base64, Base64Uri, Base58 and Baudot Encoding / Decoding
   - URI-Encoded Text Buffer Process
   - Basic MIME Content Types Support
   - Text Memory Buffers and Files
   - TStreamRedirect and other Hash process
   - Markup (e.g. HTML or Emoji) process
   - RawByteString Buffers Aggregation via TRawByteStringGroup

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  classes,
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.rtti;


{ ************ Variable Length Integer Encoding / Decoding }

/// convert a cardinal into a 32-bit variable-length integer buffer
function ToVarUInt32(Value: cardinal; Dest: PByte): PByte;

/// return the number of bytes necessary to store a 32-bit variable-length integer
// - i.e. the ToVarUInt32() buffer size
function ToVarUInt32Length(Value: PtrUInt): PtrUInt;
  {$ifdef HASINLINE}inline;{$endif}

/// return the number of bytes necessary to store some data with a its
// 32-bit variable-length integer legnth
function ToVarUInt32LengthWithData(Value: PtrUInt): PtrUInt;
  {$ifdef HASINLINE}inline;{$endif}

/// convert an integer into a 32-bit variable-length integer buffer
// - store negative values as cardinal two-complement, i.e.
// 0=0,1=1,2=-1,3=2,4=-2...
function ToVarInt32(Value: PtrInt; Dest: PByte): PByte;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a 32-bit variable-length integer buffer into a cardinal
// - fast inlined process for any number < 128
// - use overloaded FromVarUInt32() or FromVarUInt32Safe() with a SourceMax
// pointer to avoid any potential buffer overflow
function FromVarUInt32(var Source: PByte): cardinal; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// safely convert a 32-bit variable-length integer buffer into a cardinal
// - slower but safer process checking out of boundaries memory access in Source
// - SourceMax is expected to be not nil, and to point to the first byte
// just after the Source memory buffer
// - returns nil on error, or point to next input data on successful decoding
function FromVarUInt32Safe(Source, SourceMax: PByte; out Value: cardinal): PByte;

/// convert a 32-bit variable-length integer buffer into a cardinal
// - will call FromVarUInt32() if SourceMax=nil, or FromVarUInt32Safe() if set
// - returns false on error, true if Value has been set properly
function FromVarUInt32(var Source: PByte; SourceMax: PByte;
  out Value: cardinal): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a 32-bit variable-length integer buffer into a cardinal
// - this version could be called if number is likely to be > $7f, so if
// inlining the first byte won't make any benefit
function FromVarUInt32Big(var Source: PByte): cardinal;

/// convert a 32-bit variable-length integer buffer into a cardinal
// - used e.g. when inlining FromVarUInt32()
// - this version must be called if Source^ has already been checked to be > $7f
// ! result := Source^;
// ! inc(Source);
// ! if result>$7f then
// !   result := (result and $7F) or FromVarUInt32Up128(Source);
function FromVarUInt32Up128(var Source: PByte): cardinal;

/// convert a 32-bit variable-length integer buffer into a cardinal
// - this version must be called if Source^ has already been checked to be > $7f
function FromVarUInt32High(var Source: PByte): cardinal;

/// convert a 32-bit variable-length integer buffer into an integer
// - decode negative values from cardinal two-complement, i.e.
// 0=0,1=1,2=-1,3=2,4=-2...
function FromVarInt32(var Source: PByte): integer;

/// convert a UInt64 into a 64-bit variable-length integer buffer
function ToVarUInt64(Value: QWord; Dest: PByte): PByte;

/// convert a 64-bit variable-length integer buffer into a UInt64
function FromVarUInt64(var Source: PByte): QWord; overload;

/// safely convert a 64-bit variable-length integer buffer into a UInt64
// - slower but safer process checking out of boundaries memory access in Source
// - SourceMax is expected to be not nil, and to point to the first byte
// just after the Source memory buffer
// - returns nil on error, or point to next input data on successful decoding
function FromVarUInt64Safe(Source, SourceMax: PByte; out Value: QWord): PByte;

/// convert a 64-bit variable-length integer buffer into a UInt64
// - will call FromVarUInt64() if SourceMax=nil, or FromVarUInt64Safe() if set
// - returns false on error, true if Value has been set properly
function FromVarUInt64(var Source: PByte; SourceMax: PByte;
  out Value: Qword): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a Int64 into a 64-bit variable-length integer buffer
function ToVarInt64(Value: Int64; Dest: PByte): PByte;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a 64-bit variable-length integer buffer into a Int64
function FromVarInt64(var Source: PByte): Int64;

/// convert a 64-bit variable-length integer buffer into a Int64
// - this version won't update the Source pointer
function FromVarInt64Value(Source: PByte): Int64;

/// jump a value in the 32-bit or 64-bit variable-length integer buffer
function GotoNextVarInt(Source: PByte): pointer;
 {$ifdef HASINLINE}inline;{$endif}

/// convert a RawUtf8 into an UTF-8 encoded variable-length buffer
function ToVarString(const Value: RawUtf8; Dest: PByte): PByte;

/// jump a value in variable-length text buffer
function GotoNextVarString(Source: PByte): pointer;
  {$ifdef HASINLINE}inline;{$endif}

/// retrieve a variable-length UTF-8 encoded text buffer in a newly allocation RawUtf8
function FromVarString(var Source: PByte): RawUtf8; overload;

/// safe retrieve a variable-length UTF-8 encoded text buffer in a newly allocation RawUtf8
// - supplied SourceMax value will avoid any potential buffer overflow
function FromVarString(var Source: PByte; SourceMax: PByte): RawUtf8; overload;

/// retrieve a variable-length text buffer
// - this overloaded function will set the supplied code page to the AnsiString
procedure FromVarString(var Source: PByte; var Value: RawByteString;
  CodePage: integer); overload;

/// retrieve a variable-length text buffer
// - this overloaded function will set the supplied code page to the AnsiString
// and will also check for the SourceMax end of buffer
// - returns TRUE on success, or FALSE on any buffer overload detection
function FromVarString(var Source: PByte; SourceMax: PByte;
  var Value: RawByteString; CodePage: integer): boolean; overload;

/// retrieve a variable-length UTF-8 encoded text buffer in a temporary buffer
// - caller should call Value.Done after use of the Value.buf memory
// - this overloaded function would include a trailing #0, so Value.buf could
// be parsed as a valid PUtf8Char buffer (e.g. containing JSON)
procedure FromVarString(var Source: PByte; var Value: TSynTempBuffer); overload;

/// retrieve a variable-length UTF-8 encoded text buffer in a temporary buffer
// - caller should call Value.Done after use of the Value.buf memory
// - this overloaded function will also check for the SourceMax end of buffer,
// returning TRUE on success, or FALSE on any buffer overload detection
function FromVarString(var Source: PByte; SourceMax: PByte;
  var Value: TSynTempBuffer): boolean; overload;

type
  /// kind of result returned by FromVarBlob() function
  TValueResult = record
    /// start of data value
    Ptr: PAnsiChar;
    /// value length (in bytes)
    Len: PtrInt;
  end;

/// retrieve pointer and length to a variable-length text/blob buffer
function FromVarBlob(Data: PByte): TValueResult;
  {$ifdef HASINLINE}inline;{$endif}



{ ************ TAlgoCompress Compression/Decompression Classes }

type
  /// exception raised by TAlgoCompress classes
  EAlgoCompress = class(ESynException);

  /// define the implementation used by TAlgoCompress.Decompress()
  TAlgoCompressLoad = (
    aclNormal,
    aclSafeSlow,
    aclNoCrcFast);

  /// abstract low-level parent class for generic compression/decompression algorithms
  // - will encapsulate the compression algorithm with crc32c hashing
  // - all Algo* abstract methods should be overriden by inherited classes
  // - don't inherit from TSynPersistent since we don't need any of it
  TAlgoCompress = class
  protected
    fAlgoID: byte;
  public
    /// computes by default the crc32c() digital signature of the buffer
    function AlgoHash(Previous: cardinal;
      Data: pointer; DataLen: integer): cardinal; overload; virtual;
    /// computes the digital signature of the buffer, or Hash32() if defined
    function AlgoHash(ForceHash32: boolean;
      Data: pointer; DataLen: integer): cardinal; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// get maximum possible (worse) compressed size for the supplied length
    function AlgoCompressDestLen(PlainLen: integer): integer; virtual; abstract;
    /// this method will compress the supplied data
    function AlgoCompress(Plain: pointer; PlainLen: integer;
      Comp: pointer): integer; virtual; abstract;
    /// this method will return the size of the decompressed data
    function AlgoDecompressDestLen(Comp: pointer): integer; virtual; abstract;
    /// this method will decompress the supplied data
    function AlgoDecompress(Comp: pointer; CompLen: integer;
      Plain: pointer): integer; virtual; abstract;
    /// this method will partially and safely decompress the supplied data
    // - expects PartialLen <= result < PartialLenMax, depending on the algorithm
    function AlgoDecompressPartial(Comp: pointer; CompLen: integer;
      Partial: pointer; PartialLen, PartialLenMax: integer): integer; virtual; abstract;
    /// contains a genuine byte identifier for this algorithm
    // - 0 is reserved for stored, 1 for TAlgoSynLz, 2/3 for TAlgoDeflate/Fast
    // (in mormot.core.zip.pas), 4/5/6 for TAlgoLizard/Fast/Huffman
    // (in mormot.lib.lizard.pas)
    property AlgoID: byte
      read fAlgoID;
  public
    /// will register AlgoID in the global list, for Algo() class methods
    // - no need to free this instance, since it will be owned by the global list
    // - raise a EAlgoCompress if the class or its AlgoID are already registered
    // - you should never have to call this constructor, but define a global
    // variable holding a reference to a shared instance
    constructor Create; virtual;
    /// get maximum possible (worse) compressed size for the supplied length
    // - including the crc32c + algo 9 bytes header
    function CompressDestLen(PlainLen: integer): integer;
      {$ifdef HASINLINE}inline;{$endif}
    /// compress a memory buffer with crc32c hashing to a RawByteString
    function Compress(const Plain: RawByteString;
      CompressionSizeTrigger: integer = 100;
      CheckMagicForCompressed: boolean = false;
      BufferOffset: integer = 0): RawByteString; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// compress a memory buffer with crc32c hashing to a RawByteString
    function Compress(Plain: PAnsiChar; PlainLen: integer;
      CompressionSizeTrigger: integer = 100;
      CheckMagicForCompressed: boolean = false;
      BufferOffset: integer = 0): RawByteString; overload;
    /// compress a memory buffer with crc32c hashing
    // - supplied Comp buffer should contain at least CompressDestLen(PlainLen) bytes
    function Compress(Plain, Comp: PAnsiChar; PlainLen, CompLen: integer;
      CompressionSizeTrigger: integer = 100; CheckMagicForCompressed: boolean = false): integer; overload;
    /// compress a memory buffer with crc32c hashing to a TByteDynArray
    function CompressToBytes(const Plain: RawByteString;
      CompressionSizeTrigger: integer = 100;
      CheckMagicForCompressed: boolean = false): TByteDynArray; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// compress a memory buffer with crc32c hashing to a TByteDynArray
    function CompressToBytes(Plain: PAnsiChar; PlainLen: integer;
      CompressionSizeTrigger: integer = 100;
      CheckMagicForCompressed: boolean = false): TByteDynArray; overload;
    /// uncompress a RawByteString memory buffer with crc32c hashing
    function Decompress(const Comp: RawByteString; Load: TAlgoCompressLoad = aclNormal;
      BufferOffset: integer = 0): RawByteString; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// uncompress a RawByteString memory buffer with crc32c hashing
    // - returns TRUE on success
    function TryDecompress(const Comp: RawByteString; out Dest: RawByteString;
      Load: TAlgoCompressLoad = aclNormal): boolean;
    /// uncompress a memory buffer with crc32c hashing
    procedure Decompress(Comp: PAnsiChar; CompLen: integer; out result: RawByteString;
      Load: TAlgoCompressLoad = aclNormal; BufferOffset: integer = 0); overload;
    /// uncompress a RawByteString memory buffer with crc32c hashing
    function Decompress(const Comp: TByteDynArray): RawByteString; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// uncompress a RawByteString memory buffer with crc32c hashing
    // - returns nil if crc32 hash failed, i.e. if the supplied Comp is not correct
    // - returns a pointer to the uncompressed data and fill PlainLen variable,
    // after crc32c hash
    // - avoid any memory allocation in case of a stored content - otherwise, would
    // uncompress to the tmp variable, and return pointer(tmp) and length(tmp)
    function Decompress(const Comp: RawByteString; out PlainLen: integer;
      var tmp: RawByteString; Load: TAlgoCompressLoad = aclNormal): pointer; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// uncompress a RawByteString memory buffer with crc32c hashing
    // - returns nil if crc32 hash failed, i.e. if the supplied Data is not correct
    // - returns a pointer to an uncompressed data buffer of PlainLen bytes
    // - avoid any memory allocation in case of a stored content - otherwise, would
    // uncompress to the tmp variable, and return pointer(tmp) and length(tmp)
    function Decompress(Comp: PAnsiChar; CompLen: integer; out PlainLen: integer;
      var tmp: RawByteString; Load: TAlgoCompressLoad = aclNormal): pointer; overload;
    /// decode the header of a memory buffer compressed via the Compress() method
    // - validates the crc32c of the compressed data (unless Load=aclNoCrcFast),
    // then return the uncompressed size in bytes, or 0 if the crc32c does not match
    // - should call DecompressBody() later on to actually retrieve the content
    function DecompressHeader(Comp: PAnsiChar; CompLen: integer;
      Load: TAlgoCompressLoad = aclNormal): integer;
    /// decode the content of a memory buffer compressed via the Compress() method
    // - PlainLen has been returned by a previous call to DecompressHeader()
    function DecompressBody(Comp, Plain: PAnsiChar; CompLen, PlainLen: integer;
      Load: TAlgoCompressLoad = aclNormal): boolean;
    /// partial decoding of a memory buffer compressed via the Compress() method
    // - returns 0 on error, or how many bytes have been written to Partial
    // - will call virtual AlgoDecompressPartial() which is slower, but expected
    // to avoid any buffer overflow on the Partial destination buffer
    // - some algorithms (e.g. Lizard) may need some additional bytes in the
    // decode buffer, so PartialLenMax bytes should be allocated in Partial^,
    // with PartialLenMax > expected PartialLen, and returned bytes may be >
    // PartialLen, but always <= PartialLenMax
    function DecompressPartial(Comp, Partial: PAnsiChar; CompLen,
      PartialLen, PartialLenMax: integer): integer;
    /// compress a Stream content using this compression algorithm
    // - source Stream is not split into 128 MB chunks, as with FileCompress,
    // but directly compressed and including Source and Dest checksums
    // - you should specify a Magic number to be used to identify the compressed
    // Stream format
    // - follow the StreamSynLZ() deprecated function format, if ForceHash32=true
    // so that Hash32() is used instead of the AlgoHash() of this instance
    function StreamCompress(Source: TCustomMemoryStream; Dest: TStream;
      Magic: cardinal; ForceHash32: boolean = false): integer; overload;
    /// compress a Stream content using this compression algorithm into a file
    // - source Stream is not split into 128 MB chunks, as with FileCompress,
    // but directly compressed and including Source and Dest checksums
    // - you should specify a Magic number to be used to identify the compressed
    // Stream format
    // - follow the StreamSynLZ() deprecated function format, if ForceHash32=true
    // so that Hash32() is used instead of the AlgoHash() of this instance
    function StreamCompress(Source: TCustomMemoryStream; const DestFile: TFileName;
      Magic: cardinal; ForceHash32: boolean = false): integer; overload;
    /// uncompress a Stream previoulsy compressed via StreamCompress()
    // - you should specify a Magic number to be used to identify the compressed
    // Stream format
    // - follow the StreamUnSynLZ() deprecated function format, if ForceHash32=true
    // so that Hash32() is used instead of the AlgoHash() of this instance
    function StreamUnCompress(Source: TStream; Magic: cardinal;
      ForceHash32: boolean = false): TMemoryStream; overload;
    /// uncompress a File previoulsy compressed via StreamCompress() into a Stream
    // - you should specify a Magic number to be used to identify the compressed
    // Stream format
    // - follow the StreamUnSynLZ() deprecated function format, if ForceHash32=true
    // so that Hash32() is used instead of the AlgoHash() of this instance
    function StreamUnCompress(const Source: TFileName; Magic: cardinal;
      ForceHash32: boolean = false): TMemoryStream; overload;
    /// compute the real length of a given StreamCompress() buffer
    // - allows to replace an existing appended content, for instance
    function StreamComputeLen(P: PAnsiChar; Len: PtrUInt; Magic: cardinal): integer;
    /// returns TRUE if the supplied file name is a compressed file,
    // matching the Magic number as supplied to FileCompress() function
    // - follow the FileIsSynLZ() deprecated function format
    function FileIsCompressed(const Name: TFileName; Magic: cardinal): boolean;
    /// compress a file content using this compression algorithm
    // - source file is split into ChunkBytes blocks (128 MB by default) for
    // fast in-memory compression of any file size, then compressed and
    // including checksums of Source/Dest data
    // - it is not compatible with StreamCompress format, which has no chunking
    // - you should specify a Magic number to be used to identify the compressed
    // file format
    // - follow the FileSynLZ() deprecated function format, if ForceHash32=true
    // so that Hash32() is used instead of the AlgoHash() of this instance
    function FileCompress(const Source, Dest: TFileName; Magic: cardinal;
      ForceHash32: boolean = false; ChunkBytes: Int64 = 128 shl 20): boolean;
    /// uncompress a file previoulsy compressed via FileCompress()
    // - you should specify a Magic number to be used to identify the compressed
    // file format
    // - follow the FileUnSynLZ() deprecated function format, if ForceHash32=true
    // so that Hash32() is used instead of the AlgoHash() of this instance
    function FileUnCompress(const Source, Dest: TFileName; Magic: cardinal;
      ForceHash32: boolean = false): boolean;
    /// a TSynLogArchiveEvent handler which will compress older .log files
    // using our proprietary FileCompress format for this algorithm
    function EventArchive(aMagic: cardinal;
      const aOldLogFileName, aDestinationPath, aDestinationExt: TFileName): boolean;

    /// get the TAlgoCompress instance corresponding to the AlgoID stored
    // in the supplied compressed buffer
    // - returns nil if no algorithm was identified
    class function Algo(Comp: PAnsiChar; CompLen: integer): TAlgoCompress; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// get the TAlgoCompress instance corresponding to the AlgoID stored
    // in the supplied compressed buffer
    // - returns nil if no algorithm was identified
    // - also identifies "stored" content in IsStored variable
    class function Algo(Comp: PAnsiChar; CompLen: integer;
      out IsStored: boolean): TAlgoCompress; overload;
    /// get the TAlgoCompress instance corresponding to the AlgoID stored
    // in the supplied compressed buffer
    // - returns nil if no algorithm was identified
    class function Algo(const Comp: RawByteString): TAlgoCompress; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// get the TAlgoCompress instance corresponding to the AlgoID stored
    // in the supplied compressed buffer
    // - returns nil if no algorithm was identified
    class function Algo(const Comp: TByteDynArray): TAlgoCompress; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// get the TAlgoCompress instance corresponding to the supplied AlgoID
    // - returns nil if no algorithm was identified
    // - stored content is identified as TAlgoSynLZ
    class function Algo(aAlgoID: byte): TAlgoCompress; overload;
    /// quickly validate a compressed buffer content, without uncompression
    // - extract the TAlgoCompress, and call DecompressHeader() to check the
    // hash of the compressed data, and return then uncompressed size
    // - returns 0 on error (e.g. unknown algorithm or incorrect hash)
    class function UncompressedSize(const Comp: RawByteString): integer;
    /// returns the algorithm name, from its classname
    // - e.g. TAlgoSynLZ->'synlz' TAlgoLizard->'lizard' nil->'none'
    function AlgoName: TShort16;
  end;

  /// implement our fast SynLZ compression as a TAlgoCompress class
  // - please use the AlgoSynLZ global variable methods instead of the deprecated
  // SynLZCompress/SynLZDecompress wrapper functions
  TAlgoSynLZ = class(TAlgoCompress)
  public
    /// set AlgoID = 1 as genuine byte identifier for SynLZ
    constructor Create; override;
    /// get maximum possible (worse) SynLZ compressed size for the supplied length
    function AlgoCompressDestLen(PlainLen: integer): integer; override;
    /// compress the supplied data using SynLZ
    function AlgoCompress(Plain: pointer; PlainLen: integer; Comp: pointer): integer; override;
    /// return the size of the SynLZ decompressed data
    function AlgoDecompressDestLen(Comp: pointer): integer; override;
    /// decompress the supplied data using SynLZ
    function AlgoDecompress(Comp: pointer; CompLen: integer; Plain: pointer): integer; override;
    /// partial (and safe) decompression of the supplied data using SynLZ
    function AlgoDecompressPartial(Comp: pointer; CompLen: integer;
      Partial: pointer; PartialLen, PartialLenMax: integer): integer; override;
  end;

  TAlgoCompressWithNoDestLenProcess = (
    doCompress,
    doUnCompress,
    doUncompressPartial);

  /// abstract class storing the plain length before calling compression API
  // - some libraries (e.g. Deflate or Lizard) don't provide the uncompressed
  // length from its output buffer - inherit from this class to store this value
  // as ToVarUInt32, and override the RawProcess abstract protected method
  TAlgoCompressWithNoDestLen = class(TAlgoCompress)
  protected
    /// inherited classes should implement this single method for the actual process
    // - dstMax is oinly used for doUncompressPartial
    function RawProcess(src, dst: pointer; srcLen, dstLen, dstMax: integer;
      process: TAlgoCompressWithNoDestLenProcess): integer; virtual; abstract;
  public
    /// performs the compression, storing PlainLen and calling protected RawProcess
    function AlgoCompress(Plain: pointer; PlainLen: integer; Comp: pointer): integer; override;
    /// return the size of the decompressed data (using FromVarUInt32)
    function AlgoDecompressDestLen(Comp: pointer): integer; override;
    /// performs the decompression, retrieving PlainLen and calling protected RawProcess
    function AlgoDecompress(Comp: pointer; CompLen: integer; Plain: pointer): integer; override;
    /// performs the decompression, retrieving PlainLen and calling protected RawProcess
    function AlgoDecompressPartial(Comp: pointer; CompLen: integer;
      Partial: pointer; PartialLen, PartialLenMax: integer): integer; override;
  end;

var
  /// acccess to our fast SynLZ compression as a TAlgoCompress class
  // - please use this global variable methods instead of the deprecated
  // SynLZCompress/SynLZDecompress wrapper functions
  AlgoSynLZ: TAlgoCompress;

const
  /// CompressionSizeTrigger parameter SYNLZTRIG[true] will disable then
  // SynLZCompress() compression
  SYNLZTRIG: array[boolean] of integer = (
    100, maxInt);

  /// used e.g. as when ALGO_SAFE[SafeDecompression] for TAlgoCompress.Decompress
  ALGO_SAFE: array[boolean] of TAlgoCompressLoad = (
    aclNormal, aclSafeSlow);


/// fast concatenation of several AnsiStrings
function RawByteStringArrayConcat(const Values: array of RawByteString): RawByteString;

/// creates a TBytes from a RawByteString memory buffer
procedure RawByteStringToBytes(const buf: RawByteString; out bytes: TBytes);

/// creates a RawByteString memory buffer from a TBytes content
procedure BytesToRawByteString(const bytes: TBytes; out buf: RawByteString);
  {$ifdef HASINLINE}inline;{$endif}

/// creates a RawByteString memory buffer from an embedded resource
// - returns '' if the resource is not found
// - warning: resources size may be rounded up to alignment
// - you can specify a library (dll) resource instance handle, if needed
procedure ResourceToRawByteString(const ResName: string; ResType: PChar;
  out buf: RawByteString; Instance: THandle = 0);

/// creates a RawByteString memory buffer from an SynLZ-compressed embedded resource
// - returns '' if the resource is not found
// - this method would use SynLZDecompress() after ResourceToRawByteString(),
// with a ResType=PChar(10) (i.e. RC_DATA)
// - you can specify a library (dll) resource instance handle, if needed
procedure ResourceSynLZToRawByteString(const ResName: string;
  out buf: RawByteString; Instance: THandle = 0);

{$ifndef PUREMORMOT2}

/// deprecated function - use AlgoSynLZ.StreamComputeLen
function StreamSynLZComputeLen(P: PAnsiChar;
  Len, Magic: cardinal): integer; deprecated;

/// deprecated function - use AlgoSynLZ.StreamCompress
function StreamSynLZ(Source: TCustomMemoryStream; Dest: TStream;
  Magic: cardinal): integer; overload; deprecated;

/// deprecated function - use AlgoSynLZ.StreamCompress
function StreamSynLZ(Source: TCustomMemoryStream; const DestFile: TFileName;
  Magic: cardinal): integer; overload;deprecated;

/// deprecated function - use AlgoSynLZ.FileCompress
function FileSynLZ(const Source, Dest: TFileName;
  Magic: cardinal): boolean; deprecated;

/// deprecated function - use AlgoSynLZ.FileUnCompress
function FileUnSynLZ(const Source, Dest: TFileName; Magic: cardinal): boolean; deprecated;

/// deprecated function - use AlgoSynLZ.FileIsCompressed
function FileIsSynLZ(const Name: TFileName; Magic: cardinal): boolean; deprecated;

/// deprecated function - use AlgoSynLZ.StreamUnCompress
function StreamUnSynLZ(const Source: TFileName;
  Magic: cardinal): TMemoryStream; overload; deprecated;

/// deprecated function - use AlgoSynLZ.StreamUnCompress
function StreamUnSynLZ(Source: TStream;
  Magic: cardinal): TMemoryStream; overload; deprecated;

/// deprecated function - please call AlgoSynLZ.Compress() method
function SynLZCompress(const Data: RawByteString; CompressionSizeTrigger: integer=100;
  CheckMagicForCompressed: boolean=false): RawByteString; overload;

/// deprecated function - please call AlgoSynLZ.Compress() method
procedure SynLZCompress(P: PAnsiChar; PLen: integer; out Result: RawByteString;
  CompressionSizeTrigger: integer=100; CheckMagicForCompressed: boolean=false); overload;

/// deprecated function - please call AlgoSynLZ.Compress() method
function SynLZCompress(P, Dest: PAnsiChar; PLen, DestLen: integer;
  CompressionSizeTrigger: integer=100; CheckMagicForCompressed: boolean=false): integer; overload;

/// deprecated function - please call AlgoSynLZ.Decompress() method
function SynLZDecompress(const Data: RawByteString): RawByteString; overload;

/// deprecated function - please call AlgoSynLZ.Decompress() method
procedure SynLZDecompress(P: PAnsiChar; PLen: integer; out Result: RawByteString;
  SafeDecompression: boolean=false); overload;

/// deprecated function - please call AlgoSynLZ.DecompressToBytes() method
function SynLZCompressToBytes(const Data: RawByteString;
  CompressionSizeTrigger: integer=100): TByteDynArray; overload;

/// deprecated function - please call AlgoSynLZ.CompressToBytes() method
function SynLZCompressToBytes(P: PAnsiChar; PLen: integer;
  CompressionSizeTrigger: integer=100): TByteDynArray; overload;

/// deprecated function - please call AlgoSynLZ.Decompress() method
function SynLZDecompress(const Data: TByteDynArray): RawByteString; overload;

/// deprecated function - please call AlgoSynLZ.Decompress() method
function SynLZDecompress(const Data: RawByteString; out Len: integer;
  var tmp: RawByteString): pointer; overload;

/// deprecated function - please call AlgoSynLZ.Decompress() method
function SynLZDecompress(P: PAnsiChar; PLen: integer; out Len: integer;
  var tmp: RawByteString): pointer; overload;

/// deprecated function - please call AlgoSynLZ.DecompressHeader() method
function SynLZDecompressHeader(P: PAnsiChar; PLen: integer): integer;

/// deprecated function - please call AlgoSynLZ.DecompressBody() method
function SynLZDecompressBody(P,Body: PAnsiChar; PLen,BodyLen: integer;
  SafeDecompression: boolean=false): boolean;

/// deprecated function - please call AlgoSynLZ.DecompressPartial() method
function SynLZDecompressPartial(P,Partial: PAnsiChar; PLen,PartialLen: integer): integer;

{$endif PUREMORMOT2}


{ ****************** TFastReader / TBufferWriter Binary Streams }

type
  /// exception raised during TFastReader decoding
  EFastReader = class(ESynException);

  /// event signature to customize TFastReader.ErrorOverflow notification
  TOnFastReaderErrorOverflow = procedure of object;

  /// event signature to customize TFastReader.ErrorData notification
  TOnFastReaderErrorData = procedure(const fmt: RawUtf8;
    const args: array of const) of object;

  /// safe decoding of a TBufferWriter content from an in-memory buffer
  // - raise a EFastReader exception on decoding error (e.g. if a buffer
  // overflow may occur) or call OnErrorOverflow/OnErrorData event handlers
  {$ifdef USERECORDWITHMETHODS}
  TFastReader = record
  {$else}
  TFastReader = object
  {$endif USERECORDWITHMETHODS}
  public
    /// the current position in the memory
    P: PAnsiChar;
    /// the last position in the buffer
    Last: PAnsiChar;
    /// use this event to customize the ErrorOverflow process
    OnErrorOverflow: TOnFastReaderErrorOverflow;
    /// use this event to customize the ErrorData process
    OnErrorData: TOnFastReaderErrorData;
    /// when used to unserialize variants, stores options for TDocVariant creation
    // - contains a PDocVariantOptions reference pointer as defined in the
    // mormot.core.data unit
    CustomVariants: pointer;
    /// some opaque value, e.g. a version number to define the binary layout
    Tag: PtrInt;
    /// initialize the reader from a memory block
    procedure Init(Buffer: pointer; Len: PtrInt); overload;
    /// initialize the reader from a RawByteString content
    procedure Init(const Buffer: RawByteString); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// raise a EFastReader with "Reached End of Input" error message
    procedure ErrorOverflow;
    /// raise a EFastReader with "Incorrect Data: ...." error message
    procedure ErrorData(const fmt: RawUtf8; const args: array of const);
    /// read the next 32-bit signed value from the buffer
    function VarInt32: integer;
      {$ifdef HASINLINE}inline;{$endif}
    /// read the next 32-bit unsigned value from the buffer
    function VarUInt32: cardinal;
    /// try to read the next 32-bit signed value from the buffer
    // - don't change the current position
    function PeekVarInt32(out value: PtrInt): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// try to read the next 32-bit unsigned value from the buffer
    // - don't change the current position
    function PeekVarUInt32(out value: PtrUInt): boolean;
    /// read the next 32-bit unsigned value from the buffer
    // - this version won't call ErrorOverflow, but return false on error
    // - returns true on read success
    function VarUInt32Safe(out Value: cardinal): boolean;
    /// read the next 64-bit signed value from the buffer
    function VarInt64: Int64;
      {$ifdef HASINLINE}inline;{$endif}
    /// read the next 64-bit unsigned value from the buffer
    function VarUInt64: QWord;
    /// read the next RawUtf8 value from the buffer
    function VarUtf8: RawUtf8; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read the next RawUtf8 value from the buffer
    procedure VarUtf8(out result: RawUtf8); overload;
    /// read the next RawUtf8 value from the buffer
    // - this version won't call ErrorOverflow, but return false on error
    // - returns true on read success
    function VarUtf8Safe(out Value: RawUtf8): boolean;
    /// read the next RawByteString value from the buffer
    function VarString: RawByteString; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read the next RawByteString value from the buffer
    function VarString(CodePage: integer): RawByteString; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read the next pointer and length value from the buffer
    procedure VarBlob(out result: TValueResult); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read the next pointer and length value from the buffer
    function VarBlob: TValueResult; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// copy the next VarBlob value from the buffer into a TSynTempBuffer
    procedure VarBlob(out Value: TSynTempBuffer); overload;
    /// read the next ShortString value from the buffer
    function VarShortString: shortstring;
      {$ifdef HASINLINE}inline;{$endif}
    /// fast ignore the next VarUInt32/VarInt32/VarUInt64/VarInt64 value
    // - don't raise any exception, so caller could check explicitly for any EOF
    procedure VarNextInt; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// fast ignore the next count VarUInt32/VarInt32/VarUInt64/VarInt64 values
    // - don't raise any exception, so caller could check explicitly for any EOF
    procedure VarNextInt(count: integer); overload;
    /// read the next byte from the buffer
    function NextByte: byte;
      {$ifdef HASINLINE}inline;{$endif}
    /// read the next byte from the buffer, checking
    function NextByteSafe(dest: pointer): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// read the next 2 bytes from the buffer as a 16-bit unsigned value
    function Next2: cardinal;
      {$ifdef HASINLINE}inline;{$endif}
    /// read the next 4 bytes from the buffer as a 32-bit unsigned value
    function Next4: cardinal;
      {$ifdef HASINLINE}inline;{$endif}
    /// read the next 8 bytes from the buffer as a 64-bit unsigned value
    function Next8: Qword;
      {$ifdef HASINLINE}inline;{$endif}
    /// consumes the next byte from the buffer, if matches a given value
    function NextByteEquals(Value: byte): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// returns the current position, and move ahead the specified bytes
    function Next(DataLen: PtrInt): pointer;
      {$ifdef HASINLINE}inline;{$endif}
    /// returns the current position, and move ahead the specified bytes
    function NextSafe(out Data: Pointer; DataLen: PtrInt): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// copy data from the current position, and move ahead the specified bytes
    procedure Copy(Dest: Pointer; DataLen: PtrInt);
      {$ifdef HASINLINE}inline;{$endif}
    /// copy data from the current position, and move ahead the specified bytes
    // - this version won't call ErrorOverflow, but return false on error
    // - returns true on read success
    function CopySafe(Dest: Pointer; DataLen: PtrInt): boolean;
    /// retrieved cardinal values encoded with TBufferWriter.WriteVarUInt32Array
    // - Values[] will be resized only if it is not long enough, to spare heap
    // - returns decoded count in Values[], which may not be length(Values)
    // - wkFakeMarker will return -count and the caller should make the decoding
    function ReadVarUInt32Array(var Values: TIntegerDynArray): PtrInt;
    /// retrieved Int64 values encoded with TBufferWriter.WriteVarUInt64DynArray
    // - Values[] will be resized only if it is not long enough, to spare heap
    // - returns decoded count in Values[], which may not be length(Values)
    function ReadVarUInt64Array(var Values: TInt64DynArray): PtrInt;
    /// retrieved RawUtf8 values encoded with TFileBufferWriter.WriteRawUtf8DynArray
    // - returns the number of items read into Values[] (may differ from length(Values))
    function ReadVarRawUtf8DynArray(var Values: TRawUtf8DynArray): PtrInt;
    /// retrieve some TAlgoCompress buffer, appended via Write()
    // - BufferOffset could be set to reserve some bytes before the uncompressed buffer
    function ReadCompressed(Load: TAlgoCompressLoad = aclNormal;
      BufferOffset: integer = 0): RawByteString;
    /// returns TRUE if the current position is the end of the input stream
    function EOF: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// returns remaining length (difference between Last and P)
    function RemainingLength: PtrUInt;
      {$ifdef HASINLINE}inline;{$endif}
  end;

  /// available kind of integer array storage, corresponding to the data layout
  // of TBufferWriter
  // - wkUInt32 will write the content as "plain" 4 bytes binary (this is the
  // prefered way if the integers can be negative)
  // - wkVarUInt32 will write the content using our 32-bit variable-length integer
  // encoding
  // - wkVarInt32 will write the content using our 32-bit variable-length integer
  // encoding and the by-two complement (0=0,1=1,2=-1,3=2,4=-2...)
  // - wkSorted will write an increasing array of integers, handling the special
  // case of a difference of similar value (e.g. 1) between two values - note
  // that this encoding is efficient only if the difference is mainly < 253
  // - wkOffsetU and wkOffsetI will write the difference between two successive
  // values, with detection of any constant difference (unsigned or signed)
  // - wkFakeMarker won't be used by WriteVarUInt32Array, but to notify a
  // custom encoding
  TBufferWriterKind = (
    wkUInt32,
    wkVarUInt32,
    wkVarInt32,
    wkSorted,
    wkOffsetU,
    wkOffsetI,
    wkFakeMarker);

  /// this class can be used to speed up writing to a file or a stream
  // - big speed up if data is written in small blocks
  // - also handle optimized storage of any integer/Int64/RawUtf8 values
  // - use TFileBufferReader or TFastReader for decoding of the stored binary
  TBufferWriter = class
  private
    fPos: PtrInt;
    fBufLen, fBufLen16: PtrInt;
    fBuffer: PByteArray;
    fStream: TStream;
    fTotalFlushed: Int64;
    fBufferInternal: pointer;
    fInternalStream: boolean;
    fTag: PtrInt;
    procedure InternalFlush;
    function GetTotalWritten: Int64;
      {$ifdef HASINLINE}inline;{$endif}
    procedure InternalWrite(Data: pointer; DataLen: PtrInt);
    procedure FlushAndWrite(Data: pointer; DataLen: PtrInt);
  public
    /// initialize the buffer, and specify a file handle to use for writing
    // - define an internal buffer of the specified size
    constructor Create(aFile: THandle; BufLen: integer = 65536); overload;
    /// initialize the buffer, and specify a TStream to use for writing
    // - define an internal buffer of the specified size
    constructor Create(aStream: TStream; BufLen: integer = 65536); overload;
    /// initialize the buffer, and specify a file to use for writing
    // - define an internal buffer of the specified size
    // - would replace any existing file by default, unless Append is TRUE
    constructor Create(const aFileName: TFileName; BufLen: integer = 65536;
      Append: boolean = false); overload;
    /// initialize with a specified buffer and an existing TStream instance
    // - use a specified external buffer (which may be allocated on stack),
    // to avoid a memory allocation
    constructor Create(aStream: TStream; aTempBuf: pointer; aTempLen: integer); overload;
    /// initialize the buffer, using an owned TStream instance
    // - parameter could be e.g. TMemoryStream or TRawByteStringStream
    // - use Flush then TMemoryStream(Stream) to retrieve its content, or
    // FlushTo if TRawByteStringStream was used
    // - Write() fails over 800MB (_STRMAXSIZE) for a TRawByteStringStream
    constructor Create(aClass: TStreamClass; BufLen: integer = 4096); overload;
    /// initialize with a specified buffer and an owned TStream
    // - use a specified external buffer (which may be allocated on stack),
    // to avoid a memory allocation
    // - aClass could be e.g. TMemoryStream or TRawByteStringStream
    constructor Create(aClass: TStreamClass; aTempBuf: pointer; aTempLen: integer); overload;
    /// initialize with a stack-allocated 8KB of buffer
    // - destination stream is an owned TRawByteStringStream - so you can
    // call FlushTo to retrieve all written data
    // - Write() fails over 800MB (_STRMAXSIZE) for a TRawByteStringStream
    // - convenient to reduce heap presure, when writing a few KB of data
    constructor Create(const aStackBuffer: TTextWriterStackBuffer); overload;
    /// release internal TStream (after AssignToHandle call)
    // - warning: an explicit call to Flush is needed to write the data pending
    // in internal buffer
    destructor Destroy; override;
    /// append 1 byte of data at the current position
    procedure Write1(Data: byte);
      {$ifdef HASINLINE}inline;{$endif}
    /// append 2 bytes of data at the current position
    procedure Write2(Data: cardinal);
      {$ifdef HASINLINE}inline;{$endif}
    /// append 4 bytes of data at the current position
    procedure Write4(Data: integer);
      {$ifdef HASINLINE}inline;{$endif}
    /// append 4 bytes of data, encoded as BigEndian, at the current position
    procedure Write4BigEndian(Data: integer);
      {$ifdef HASINLINE}inline;{$endif}
    /// append 8 bytes of data at the current position
    procedure Write8(Data8Bytes: pointer);
      {$ifdef HASINLINE}inline;{$endif}
    /// append 8 bytes of 64-bit integer at the current position
    procedure WriteI64(Data: Int64);
      {$ifdef HASINLINE}inline;{$endif}
    /// append the same byte a given number of occurences at the current position
    procedure WriteN(Data: byte; Count: integer);
    /// append some content (may be text or binary) prefixed by its encoded length
    // - will write DataLen as VarUInt32, then the Data content, as expected
    // by FromVarString/FromVarBlob functions
    procedure WriteVar(Data: pointer; DataLen: PtrInt); overload;
    /// append some TTempUtf8 text content prefixed by its encoded length
    // - will also release any memory stored in Item.TempRawUtf8
    procedure WriteVar(var Item: TTempUtf8); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append some UTF-8 encoded text at the current position
    // - will write the string length (as VarUInt32), then the string content
    // - is just a wrapper around WriteVar()
    procedure WriteShort(const Text: ShortString);
      {$ifdef HASINLINE}inline;{$endif}
    /// append some length-prefixed UTF-8 text at the current position
    // - will write the string length (as VarUInt32), then the string content, as expected
    // by the FromVarString() function
    // - is just a wrapper around WriteVar()
    procedure Write(const Text: RawByteString); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append some data at the current position
    // - will be inlined as a MoveFast() most of the time
    procedure Write(Data: pointer; DataLen: PtrInt); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append some content at the current position
    // - will write the binary data, without any length prefix
    procedure WriteBinary(const Data: RawByteString);
    /// append "New[0..Len-1] xor Old[0..Len-1]" bytes
    // - as used e.g. by ZeroCompressXor/TSynBloomFilterDiff.SaveTo
    procedure WriteXor(New, Old: PAnsiChar; Len: PtrInt; crc: PCardinal = nil);
    /// append a cardinal value using 32-bit variable-length integer encoding
    procedure WriteVarUInt32(Value: PtrUInt);
    /// append an integer value using 32-bit variable-length integer encoding of
    // the by-two complement of the given value
    procedure WriteVarInt32(Value: PtrInt);
    /// append an integer value using 64-bit variable-length integer encoding of
    // the by-two complement of the given value
    procedure WriteVarInt64(Value: Int64);
    /// append an unsigned integer value using 64-bit variable-length encoding
    procedure WriteVarUInt64(Value: QWord);
    /// append cardinal values (NONE must be negative!) using 32-bit
    // variable-length integer encoding or other specialized algorithm,
    // depending on the data layout
    // - could be decoded later on via TFastReader.ReadVarUInt32Array
    procedure WriteVarUInt32Array(const Values: TIntegerDynArray;
      ValuesCount: integer; DataLayout: TBufferWriterKind);
    /// append cardinal values (NONE must be negative!) using 32-bit
    // variable-length integer encoding or other specialized algorithms,
    // depending on the data layout
    // - could be decoded later on via TFastReader.ReadVarUInt32Array
    procedure WriteVarUInt32Values(Values: PIntegerArray; ValuesCount: integer;
      DataLayout: TBufferWriterKind);
    /// append UInt64 values using 64-bit variable length integer encoding
    // - if Offset is TRUE, then it will store the difference between
    // two values using 64-bit variable-length integer encoding (in this case,
    // a fixed-sized record storage is also handled separately)
    // - could be decoded later on via TFastReader.ReadVarUInt64Array
    procedure WriteVarUInt64DynArray(const Values: TInt64DynArray;
      ValuesCount: integer; Offset: boolean);
    /// append the RawUtf8 dynamic array
    // - handled the fixed size strings array case in a very efficient way
    procedure WriteRawUtf8DynArray(const Values: TRawUtf8DynArray; ValuesCount: integer);
    /// append a RawUtf8 array of values, from its low-level memory pointer
    // - handled the fixed size strings array case in a very efficient way
    procedure WriteRawUtf8Array(Values: PPtrUIntArray; ValuesCount: integer);
    /// append a TStream content
    // - is StreamSize is left as -1, the Stream.Size is used
    // - the size of the content is stored in the resulting stream
    procedure WriteStream(aStream: TCustomMemoryStream; aStreamSize: integer = -1);
    /// allows to write directly to a memory buffer
    // - caller should specify the maximum possible number of bytes to be written
    // - then write the data to the returned pointer, and call DirectWriteFlush
    // - if len is bigger than the internal buffer, tmp will be used instead
    function DirectWritePrepare(len: PtrInt; var tmp: RawByteString): PAnsiChar;
    /// finalize a direct write to a memory buffer
    // - by specifying the number of bytes written to the buffer
    procedure DirectWriteFlush(len: PtrInt; const tmp: RawByteString);
    /// write any pending data in the internal buffer to the stream
    // - after a Flush, it's possible to call FileSeek64(aFile,....)
    // - returns the number of bytes written between two FLush method calls
    function Flush: Int64;
    /// write any pending data, then create a RawByteString from the content
    // - raise an exception if internal Stream is not a TRawByteStringStream
    function FlushTo: RawByteString;
    /// write any pending data, then create a TBytes array from the content
    // - raise an exception if the size exceeds 800MB (_DAMAXSIZE)
    function FlushToBytes: TBytes;
    /// write any pending data, then call algo.Compress() on the buffer
    // - if algo is left to its default nil, will use global AlgoSynLZ
    // - features direct compression from internal buffer, if stream was not used
    // - BufferOffset could be set to reserve some bytes before the compressed buffer
    // - raise an exception if internal Stream is not a TRawByteStringStream
    function FlushAndCompress(nocompression: boolean = false;
      algo: TAlgoCompress = nil; BufferOffset: integer = 0): RawByteString;
    /// rewind the Stream to the position when Create() was called
    // - note that this does not clear the Stream content itself, just
    // move back its writing position to its initial place
    procedure CancelAll; virtual;
    /// the associated writing stream
    property Stream: TStream
      read fStream;
    /// the current position in the internal buffer
    property BufferPosition: PtrInt
      read fPos;
    /// get the byte count written since last Flush
    property TotalWritten: Int64
      read GetTotalWritten;
    /// simple property used to store some integer content
    property Tag: PtrInt
      read fTag write fTag;
  end;

{$ifndef PUREMORMOT2}

  /// deprecated alias to TBufferWriter binary serializer
  TFileBufferWriter = TBufferWriter;
  TFileBufferWriterKind = TBufferWriterKind;

const
  woHideSynPersistentPassword = woHideSensitivePersonalInformation;

{$endif PUREMORMOT2}


{ ************ Base64, Base64Uri, Base58 and Baudot Encoding / Decoding }

const
  /// UTF-8 encoded \uFFF0 special code to mark Base64 binary content in JSON
  // - Unicode special char U+FFF0 is UTF-8 encoded as EF BF B0 bytes
  // - as generated by BinToBase64WithMagic() functions, and expected by
  // SqlParamContent() and ExtractInlineParameters() functions
  // - used e.g. when transmitting TDynArray.SaveTo() content
  JSON_BASE64_MAGIC_C = $b0bfef;

  /// UTF-8 encoded \uFFF0 special code to mark Base64 binary content in JSON
  // - Unicode special char U+FFF0 is UTF-8 encoded as EF BF B0 bytes
  // - as generated by BinToBase64WithMagic() functions, and expected by
  // SqlParamContent() and ExtractInlineParameters() functions
  // - used e.g. when transmitting TDynArray.SaveTo() content
  JSON_BASE64_MAGIC_S: string[3] = #$ef#$bf#$b0;

  /// '"' + UTF-8 encoded \uFFF0 special code to mark Base64 binary in JSON
  JSON_BASE64_MAGIC_QUOTE_C = ord('"') + cardinal(JSON_BASE64_MAGIC_C) shl 8;

  /// '"' + UTF-8 encoded \uFFF0 special code to mark Base64 binary in JSON
  // - defined as a shortstring constant to be used as:
  // ! AddShorter(JSON_BASE64_MAGIC_QUOTE_S);
  JSON_BASE64_MAGIC_QUOTE_S: string[4] = '"'#$ef#$bf#$b0;

/// just a wrapper around Base64ToBin() for in-place decode of JSON_BASE64_MAGIC_C
// '\uFFF0base64encodedbinary' content into binary
// - input ParamValue shall have been checked to match the expected pattern
procedure Base64MagicDecode(var ParamValue: RawUtf8);

/// check and decode '\uFFF0base64encodedbinary' content into binary
// - this method will check the supplied value to match the expected
// JSON_BASE64_MAGIC_C pattern, decode and set Blob and return TRUE
function Base64MagicCheckAndDecode(Value: PUtf8Char; var Blob: RawByteString): boolean; overload;

/// check and decode '\uFFF0base64encodedbinary' content into binary
// - this method will check the supplied value to match the expected
// JSON_BASE64_MAGIC_C pattern, decode and set Blob and return TRUE
function Base64MagicCheckAndDecode(Value: PUtf8Char; ValueLen: integer;
  var Blob: RawByteString): boolean; overload;

/// check and decode '\uFFF0base64encodedbinary' content into binary
// - this method will check the supplied value to match the expected
// JSON_BASE64_MAGIC_C pattern, decode and set Blob and return TRUE
function Base64MagicCheckAndDecode(Value: PUtf8Char;
  var Blob: TSynTempBuffer; ValueLen: integer = 0): boolean; overload;

/// fast conversion from binary data into Base64 encoded UTF-8 text
function BinToBase64(const s: RawByteString): RawUtf8; overload;

/// fast conversion from binary data into Base64 encoded UTF-8 text
function BinToBase64(Bin: PAnsiChar; BinBytes: integer): RawUtf8; overload;

/// fast conversion from a small binary data into Base64 encoded UTF-8 text
function BinToBase64Short(const s: RawByteString): shortstring; overload;

/// fast conversion from a small binary data into Base64 encoded UTF-8 text
function BinToBase64Short(Bin: PAnsiChar; BinBytes: integer): shortstring; overload;

/// fast conversion from binary data into prefixed/suffixed Base64 encoded UTF-8 text
// - with optional JSON_BASE64_MAGIC_C prefix (UTF-8 encoded \uFFF0 special code)
function BinToBase64(const data, Prefix, Suffix: RawByteString; WithMagic: boolean): RawUtf8; overload;

/// fast conversion from binary data into Base64 encoded UTF-8 text
// with JSON_BASE64_MAGIC_C prefix (UTF-8 encoded \uFFF0 special code)
function BinToBase64WithMagic(const data: RawByteString): RawUtf8; overload;

/// fast conversion from binary data into Base64 encoded UTF-8 text
// with JSON_BASE64_MAGIC_C prefix (UTF-8 encoded \uFFF0 special code)
function BinToBase64WithMagic(Data: pointer; DataLen: integer): RawUtf8; overload;

/// raw function for efficient binary to Base64 encoding of the main block
// - don't use this function, but rather the BinToBase64() overloaded functions
function Base64EncodeMain(rp, sp: PAnsiChar; len: cardinal): integer;

/// raw function for efficient binary to Base64 encoding of the last bytes
// - don't use this function, but rather the BinToBase64() overloaded functions
procedure Base64EncodeTrailing(rp, sp: PAnsiChar; len: cardinal);
  {$ifdef FPC}inline;{$endif}

/// fast conversion from Base64 encoded text into binary data
// - is now just an alias to Base64ToBinSafe() overloaded function
// - returns '' if s was not a valid Base64-encoded input
function Base64ToBin(const s: RawByteString): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from Base64 encoded text into binary data
// - is now just an alias to Base64ToBinSafe() overloaded function
// - returns '' if sp/len buffer was not a valid Base64-encoded input
function Base64ToBin(sp: PAnsiChar; len: PtrInt): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from Base64 encoded text into binary data
// - is now just an alias to Base64ToBinSafe() overloaded function
// - returns false and data='' if sp/len buffer was invalid
function Base64ToBin(sp: PAnsiChar; len: PtrInt; var data: RawByteString): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from Base64 encoded text into binary data
// - returns TRUE on success, FALSE if sp/len buffer was invvalid
function Base64ToBin(sp: PAnsiChar; len: PtrInt; var Blob: TSynTempBuffer): boolean; overload;

/// fast conversion from Base64 encoded text into binary data
// - returns TRUE on success, FALSE if base64 does not match binlen
// - nofullcheck is deprecated and not used any more, since nofullcheck=false
// is now processed with no performance cost
function Base64ToBin(base64, bin: PAnsiChar; base64len, binlen: PtrInt
  {$ifndef PUREMORMOT2} ; nofullcheck: boolean = true {$endif}): boolean; overload;

/// fast conversion from Base64 encoded text into binary data
// - returns TRUE on success, FALSE if base64 does not match binlen
// - nofullcheck is deprecated and not used any more, since nofullcheck=false
// is now processed with no performance cost
function Base64ToBin(const base64: RawByteString; bin: PAnsiChar; binlen: PtrInt
  {$ifndef PUREMORMOT2} ; nofullcheck: boolean = true {$endif}): boolean; overload;

/// fast conversion from Base64 encoded text into binary data
// - will check supplied text is a valid Base64 encoded stream
function Base64ToBinSafe(const s: RawByteString): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from Base64 encoded text into binary data
// - will check supplied text is a valid Base64 encoded stream
function Base64ToBinSafe(sp: PAnsiChar; len: PtrInt): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from Base64 encoded text into binary data
// - will check supplied text is a valid Base64 encoded stream
function Base64ToBinSafe(sp: PAnsiChar; len: PtrInt; var data: RawByteString): boolean; overload;

/// check if the supplied text is a valid Base64 encoded stream
function IsBase64(const s: RawByteString): boolean; overload;

/// check if the supplied text is a valid Base64 encoded stream
function IsBase64(sp: PAnsiChar; len: PtrInt): boolean; overload;

/// retrieve the expected encoded length after Base64 process
function BinToBase64Length(len: PtrUInt): PtrUInt;
  {$ifdef HASINLINE}inline;{$endif}

/// retrieve the expected undecoded length of a Base64 encoded buffer
// - here len is the number of bytes in sp
function Base64ToBinLength(sp: PAnsiChar; len: PtrInt): PtrInt;

/// retrieve the expected undecoded length of a Base64 encoded buffer
// - here len is the number of bytes in sp
// - will check supplied text is a valid Base64 encoded stream
function Base64ToBinLengthSafe(sp: PAnsiChar; len: PtrInt): PtrInt;

/// direct low-level decoding of a Base64 encoded buffer
// - here len is the number of 4 chars chunks in sp input
// - deprecated low-level function: use Base64ToBin/Base64ToBinSafe instead
function Base64Decode(sp,rp: PAnsiChar; len: PtrInt): boolean;

/// fast conversion from binary data into Base64-like URI-compatible encoded text
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
function BinToBase64uri(const s: RawByteString): RawUtf8; overload;

/// fast conversion from a binary buffer into Base64-like URI-compatible encoded text
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
function BinToBase64uri(Bin: PAnsiChar; BinBytes: integer): RawUtf8; overload;

/// fast conversion from a binary buffer into Base64-like URI-compatible encoded shortstring
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
// - returns '' if BinBytes void or too big for the resulting shortstring
function BinToBase64uriShort(Bin: PAnsiChar; BinBytes: integer): shortstring;

/// conversion from any Base64 encoded value into URI-compatible encoded text
// - warning: will modify the supplied base64 string in-place
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
procedure Base64ToUri(var base64: RawUtf8);

/// low-level conversion from a binary buffer into Base64-like URI-compatible encoded text
// - you should rather use the overloaded BinToBase64uri() functions
procedure Base64uriEncode(rp, sp: PAnsiChar; len: cardinal);

/// retrieve the expected encoded length after Base64-URI process
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
function BinToBase64uriLength(len: PtrUInt): PtrUInt;
  {$ifdef HASINLINE}inline;{$endif}

/// retrieve the expected undecoded length of a Base64-URI encoded buffer
// - here len is the number of bytes in sp
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
function Base64uriToBinLength(len: PtrInt): PtrInt;

/// fast conversion from Base64-URI encoded text into binary data
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
function Base64uriToBin(sp: PAnsiChar; len: PtrInt): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from Base64-URI encoded text into binary data
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
procedure Base64uriToBin(sp: PAnsiChar; len: PtrInt;
  var result: RawByteString); overload;

/// fast conversion from Base64-URI encoded text into binary data
// - caller should always execute temp.Done when finished with the data
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
function Base64uriToBin(sp: PAnsiChar; len: PtrInt;
  var temp: TSynTempBuffer): boolean; overload;

/// fast conversion from Base64-URI encoded text into binary data
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
function Base64uriToBin(const s: RawByteString): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from Base64-URI encoded text into binary data
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
// - will check supplied text is a valid Base64-URI encoded stream
function Base64uriToBin(base64, bin: PAnsiChar; base64len, binlen: PtrInt): boolean; overload;

/// fast conversion from Base64-URI encoded text into binary data
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
// - will check supplied text is a valid Base64-URI encoded stream
function Base64uriToBin(const base64: RawByteString;
  bin: PAnsiChar; binlen: PtrInt): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// direct low-level decoding of a Base64-URI encoded buffer
// - the buffer is expected to be at least Base64uriToBinLength() bytes long
// - returns true if the supplied sp[] buffer has been successfully decoded
// into rp[] - will break at any invalid character, so is always safe to use
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
// - you should better not use this, but Base64uriToBin() overloaded functions
function Base64uriDecode(sp, rp: PAnsiChar; len: PtrInt): boolean;

/// conversion from a binary buffer into Base58 encoded text as TSynTempBuffer
// - Bitcoin' Base58 was defined as alphanumeric chars without misleading 0O I1
// - Base58 is much slower than Base64, performing in O(n^2) instead of O(n),
// and should not be used on big buffers
// - returns the number of encoded chars encoded into Dest.buf
// - caller should call Dest.Done once it is finished with the output text
function BinToBase58(Bin: PAnsiChar; BinLen: integer;
  var Dest: TSynTempBuffer): integer; overload;

/// conversion from a binary buffer into Base58 encoded text as TSynTempBuffer
// - Bitcoin' Base58 was defined as alphanumeric chars without misleading 0O I1
// - Base58 is much slower than Base64, performing in O(n^2) instead of O(n),
// and should not be used on big buffers
function BinToBase58(Bin: PAnsiChar; BinLen: integer): RawUtf8; overload;

/// conversion from a binary buffer into Base58 encoded text as TSynTempBuffer
// - Bitcoin' Base58 was defined as alphanumeric chars without misleading 0O I1
// - Base58 is much slower than Base64, performing in O(n^2) instead of O(n),
// and should not be used on big buffers
function BinToBase58(const Bin: RawByteString): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// conversion from Base58 encoded text into a binary buffer
// - Bitcoin' Base58 was defined as alphanumeric chars without misleading 0O I1
// - Base58 is much slower than Base64, performing in O(n^2) instead of O(n),
// and should not be used on big buffers
// - returns the number of decoded chars encoded into Dest.buf
// - caller should call Dest.Done once it is finished with the output binary
function Base58ToBin(B58: PAnsiChar; B58Len: integer;
  var Dest: TSynTempBuffer): integer; overload;

/// conversion from Base58 encoded text into a binary string
// - Bitcoin' Base58 was defined as alphanumeric chars without misleading 0O I1
// - Base58 is much slower than Base64, and should not be used on big buffers
// - returns '' if input was not valid Base58 encoded
function Base58ToBin(B58: PAnsiChar; B58Len: integer): RawByteString; overload;

/// conversion from Base58 encoded text into a binary string
// - Bitcoin' Base58 was defined as alphanumeric chars without misleading 0O I1
// - Base58 is much slower than Base64, and should not be used on big buffers
// - returns '' if input was not valid Base58 encoded
function Base58ToBin(const base58: RawUtf8): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fill a RawBlob from TEXT-encoded blob data
// - blob data can be encoded as SQLite3 BLOB literals (X'53514C697465' e.g.) or
// or Base-64 encoded content ('\uFFF0base64encodedbinary') or plain TEXT
function BlobToRawBlob(P: PUtf8Char; Len: integer = 0): RawBlob; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fill a RawBlob from TEXT-encoded blob data
// - blob data can be encoded as SQLite3 BLOB literals (X'53514C697465' e.g.) or
// or Base-64 encoded content ('\uFFF0base64encodedbinary') or plain TEXT
procedure BlobToRawBlob(P: PUtf8Char; var result: RawBlob; Len: integer = 0); overload;

/// fill a RawBlob from TEXT-encoded blob data
// - blob data can be encoded as SQLite3 BLOB literals (X'53514C697465' e.g.) or
// or Base-64 encoded content ('\uFFF0base64encodedbinary') or plain TEXT
function BlobToRawBlob(const Blob: RawByteString): RawBlob; overload;

/// create a TBytes from TEXT-encoded blob data
// - blob data can be encoded as SQLite3 BLOB literals (X'53514C697465' e.g.) or
// or Base-64 encoded content ('\uFFF0base64encodedbinary') or plain TEXT
function BlobToBytes(P: PUtf8Char): TBytes;

/// create a memory stream from TEXT-encoded blob data
// - blob data can be encoded as SQLite3 BLOB literals (X'53514C697465' e.g.) or
// or Base-64 encoded content ('\uFFF0base64encodedbinary') or plain TEXT
// - the caller must free the stream instance after use
function BlobToStream(P: PUtf8Char): TStream;

/// creates a TEXT-encoded version of blob data from a RawBlob
// - TEXT will be encoded as SQLite3 BLOB literals (X'53514C697465' e.g.)
function RawBlobToBlob(const RawBlob: RawBlob): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// creates a TEXT-encoded version of blob data from a memory data
// - same as RawBlob, but with direct memory access via a pointer/byte size pair
// - TEXT will be encoded as SQLite3 BLOB literals (X'53514C697465' e.g.)
function RawBlobToBlob(RawBlob: pointer; RawBlobLength: integer): RawUtf8; overload;

/// convert a Base64-encoded content into binary hexadecimal ready for SQL
// - returns e.g. X'53514C697465'
procedure Base64MagicToBlob(Base64: PUtf8Char; var result: RawUtf8);

/// return true if the TEXT is encoded as SQLite3 BLOB literals (X'53514C697465' e.g.)
function isBlobHex(P: PUtf8Char): boolean;
  {$ifdef HASINLINE}inline;{$endif}


type
  /// used by MultiPartFormDataDecode() to return one item of its data
  TMultiPart = record
    Name: RawUtf8;
    FileName: RawUtf8;
    ContentType: RawUtf8;
    Encoding: RawUtf8;
    Content: RawByteString;
  end;
  /// used by MultiPartFormDataDecode() to return all its data items
  TMultiPartDynArray = array of TMultiPart;

/// decode multipart/form-data POST request content into memory
// - following RFC 1867
// - decoded sections are appended to MultiPart[] existing array
function MultiPartFormDataDecode(const MimeType, Body: RawUtf8;
  var MultiPart: TMultiPartDynArray): boolean;

/// used e.g. by MultiPartFormDataEncode and THttpMultiPartStream.Add
function MultiPartFormDataNewBound(var boundaries: TRawUtf8DynArray): RawUtf8;

/// encode multipart fields and files
// - only one of them can be used because MultiPartFormDataDecode must implement
// both decodings
// - MultiPart: parts to build the multipart content from, which may be created
// using MultiPartFormDataAddFile/MultiPartFormDataAddField
// - MultiPartContentType: variable returning
// $ Content-Type: multipart/form-data; boundary=xxx
// where xxx is the first generated boundary
// - MultiPartContent: generated multipart content
// - Rfc2388NestedFiles will force the deprecated nested "multipart/mixed" format
// - consider THttpMultiPartStream from mormot.net.client for huge file content
function MultiPartFormDataEncode(const MultiPart: TMultiPartDynArray;
  var MultiPartContentType, MultiPartContent: RawUtf8;
  Rfc2388NestedFiles: boolean = false): boolean;

/// encode a file in a multipart array
// - FileName: file to encode
// - Multipart: where the part is added
// - Name: name of the part, is empty the name 'File###' is generated
// - consider THttpMultiPartStream from mormot.net.client for huge file content
function MultiPartFormDataAddFile(const FileName: TFileName;
  var MultiPart: TMultiPartDynArray; const Name: RawUtf8 = ''): boolean;

/// encode a field in a multipart array
// - FieldName: field name of the part
// - FieldValue: value of the field
// - Multipart: where the part is added
// - consider THttpMultiPartStream from mormot.net.client for huge file content
function MultiPartFormDataAddField(const FieldName, FieldValue: RawUtf8;
  var MultiPart: TMultiPartDynArray): boolean;


/// convert some ASCII-7 text into binary, using Emile Baudot code
// - as used in telegraphs, covering #10 #13 #32 a-z 0-9 - ' , ! : ( + ) $ ? @ . / ;
// charset, following a custom static-huffman-like encoding with 5-bit masks
// - any upper case char will be converted into lowercase during encoding
// - other characters (e.g. UTF-8 accents, or controls chars) will be ignored
// - resulting binary will consume 5 (or 10) bits per character
// - reverse of the BaudotToAscii() function
// - the "baud" symbol rate measurement comes from Emile's name ;)
function AsciiToBaudot(P: PAnsiChar; len: PtrInt): RawByteString; overload;

/// convert some ASCII-7 text into binary, using Emile Baudot code
// - as used in telegraphs, covering #10 #13 #32 a-z 0-9 - ' , ! : ( + ) $ ? @ . / ;
// charset, following a custom static-huffman-like encoding with 5-bit masks
// - any upper case char will be converted into lowercase during encoding
// - other characters (e.g. UTF-8 accents, or controls chars) will be ignored
// - resulting binary will consume 5 (or 10) bits per character
// - reverse of the BaudotToAscii() function
// - the "baud" symbol rate measurement comes from Emile's name ;)
function AsciiToBaudot(const Text: RawUtf8): RawByteString; overload;

/// convert some Baudot code binary, into ASCII-7 text
// - reverse of the AsciiToBaudot() function
// - any uppercase character would be decoded as lowercase - and some characters
// may have disapeared
// - the "baud" symbol rate measurement comes from Emile's name ;)
function BaudotToAscii(Baudot: PByteArray; len: PtrInt): RawUtf8; overload;

/// convert some Baudot code binary, into ASCII-7 text
// - reverse of the AsciiToBaudot() function
// - any uppercase character would be decoded as lowercase - and some characters
// may have disapeared
// - the "baud" symbol rate measurement comes from Emile's name ;)
function BaudotToAscii(const Baudot: RawByteString): RawUtf8; overload;



{ ***************** URI-Encoded Text Buffer Process }

/// encode a string to be compatible with URI encoding
function UrlEncode(const svar: RawUtf8): RawUtf8; overload;

/// encode a string to be compatible with URI encoding
function UrlEncode(Text: PUtf8Char): RawUtf8; overload;

/// encode supplied parameters to be compatible with URI encoding
// - parameters must be supplied two by two, as Name,Value pairs, e.g.
// ! url := UrlEncode(['select','*','where','ID=12','offset',23,'object',aObject]);
// - parameters names should be plain ASCII-7 RFC compatible identifiers
// (0..9a..zA..Z_.~), otherwise their values are skipped
// - parameters values can be either textual, integer or extended, or any TObject
// - TObject serialization into UTF-8 will be processed by the ObjectToJson()
// function
function UrlEncode(const NameValuePairs: array of const): RawUtf8; overload;

/// decode a string compatible with URI encoding into its original value
// - you can specify the decoding range (as in copy(s,i,len) function)
function UrlDecode(const s: RawUtf8; i: PtrInt = 1; len: PtrInt = -1): RawUtf8; overload;

/// decode a string compatible with URI encoding into its original value
function UrlDecode(U: PUtf8Char): RawUtf8; overload;

/// decode a specified parameter compatible with URI encoding into its original
// textual value
// - UrlDecodeValue('select=%2A&where=LastName%3D%27M%C3%B4net%27','SELECT=',V,@Next)
// will return Next^='where=...' and V='*'
// - if Upper is not found, Value is not modified, and result is FALSE
// - if Upper is found, Value is modified with the supplied content, and result is TRUE
function UrlDecodeValue(U: PUtf8Char; const Upper: RawUtf8;
  var Value: RawUtf8; Next: PPUtf8Char = nil): boolean;

/// decode a specified parameter compatible with URI encoding into its original
// integer numerical value
// - UrlDecodeInteger('offset=20&where=LastName%3D%27M%C3%B4net%27','OFFSET=',O,@Next)
// will return Next^='where=...' and O=20
// - if Upper is not found, Value is not modified, and result is FALSE
// - if Upper is found, Value is modified with the supplied content, and result is TRUE
function UrlDecodeInteger(U: PUtf8Char; const Upper: RawUtf8;
  var Value: integer; Next: PPUtf8Char = nil): boolean;

/// decode a specified parameter compatible with URI encoding into its original
// cardinal numerical value
// - UrlDecodeCardinal('offset=20&where=LastName%3D%27M%C3%B4net%27','OFFSET=',O,@Next)
// will return Next^='where=...' and O=20
// - if Upper is not found, Value is not modified, and result is FALSE
// - if Upper is found, Value is modified with the supplied content, and result is TRUE
function UrlDecodeCardinal(U: PUtf8Char; const Upper: RawUtf8;
  var Value: cardinal; Next: PPUtf8Char = nil): boolean;

/// decode a specified parameter compatible with URI encoding into its original
// Int64 numerical value
// - UrlDecodeInt64('offset=20&where=LastName%3D%27M%C3%B4net%27','OFFSET=',O,@Next)
// will return Next^='where=...' and O=20
// - if Upper is not found, Value is not modified, and result is FALSE
// - if Upper is found, Value is modified with the supplied content, and result is TRUE
function UrlDecodeInt64(U: PUtf8Char; const Upper: RawUtf8;
  var Value: Int64; Next: PPUtf8Char = nil): boolean;

/// decode a specified parameter compatible with URI encoding into its original
// floating-point value
// - UrlDecodeExtended('price=20.45&where=LastName%3D%27M%C3%B4net%27','PRICE=',P,@Next)
// will return Next^='where=...' and P=20.45
// - if Upper is not found, Value is not modified, and result is FALSE
// - if Upper is found, Value is modified with the supplied content, and result is TRUE
function UrlDecodeExtended(U: PUtf8Char; const Upper: RawUtf8;
  var Value: TSynExtended; Next: PPUtf8Char = nil): boolean;

/// decode a specified parameter compatible with URI encoding into its original
// floating-point value
// - UrlDecodeDouble('price=20.45&where=LastName%3D%27M%C3%B4net%27','PRICE=',P,@Next)
// will return Next^='where=...' and P=20.45
// - if Upper is not found, Value is not modified, and result is FALSE
// - if Upper is found, Value is modified with the supplied content, and result is TRUE
function UrlDecodeDouble(U: PUtf8Char; const Upper: RawUtf8;
  var Value: double; Next: PPUtf8Char = nil): boolean;

/// returns TRUE if all supplied parameters do exist in the URI encoded text
// - CsvNames parameter shall provide as a CSV list of names
// - e.g. UrlDecodeNeedParameters('price=20.45&where=LastName%3D','price,where')
// will return TRUE
function UrlDecodeNeedParameters(U, CsvNames: PUtf8Char): boolean;

/// decode the next Name=Value&.... pair from input URI
// - Name is returned directly (should be plain ASCII 7-bit text)
// - Value is returned after URI decoding (from %.. patterns)
// - if a pair is decoded, return a PUtf8Char pointer to the next pair in
// the input buffer, or points to #0 if all content has been processed
// - if a pair is not decoded, return nil
function UrlDecodeNextNameValue(U: PUtf8Char;
  var Name, Value: RawUtf8): PUtf8Char;

/// decode a URI-encoded Value from an input buffer
// - decoded value is set in Value out variable
// - returns a pointer just after the decoded value (may points e.g. to
// #0 or '&') - it is up to the caller to continue the process or not
function UrlDecodeNextValue(U: PUtf8Char; out Value: RawUtf8): PUtf8Char;

/// decode a URI-encoded Name from an input buffer
// - decoded value is set in Name out variable
// - returns a pointer just after the decoded name, after the '='
// - returns nil if there was no name=... pattern in U
function UrlDecodeNextName(U: PUtf8Char; out Name: RawUtf8): PUtf8Char;

/// checks if the supplied UTF-8 text don't need URI encoding
// - returns TRUE if all its chars are non-void plain ASCII-7 RFC compatible
// identifiers (0..9a..zA..Z-_.~)
function IsUrlValid(P: PUtf8Char): boolean;

/// checks if the supplied UTF-8 text values don't need URI encoding
// - returns TRUE if all its chars of all strings are non-void plain ASCII-7 RFC
// compatible identifiers (0..9a..zA..Z-_.~)
function AreUrlValid(const Url: array of RawUtf8): boolean;

/// ensure the supplied URI contains a trailing '/' charater
function IncludeTrailingUriDelimiter(const URI: RawByteString): RawByteString;


{ *********** Basic MIME Content Types Support }

/// retrieve the MIME content type from a supplied binary buffer
// - inspect the first bytes, to guess from standard known headers
// - return the MIME type, ready to be appended to a 'Content-Type: ' HTTP header
// - returns DefaultContentType if the binary buffer has an unknown layout
function GetMimeContentTypeFromBuffer(Content: Pointer; Len: PtrInt;
  const DefaultContentType: RawUtf8): RawUtf8;

/// retrieve the MIME content type from its file name or a supplied binary buffer
// - will first check for known file extensions, then inspect the binary content
// - return the MIME type, ready to be appended to a 'Content-Type: ' HTTP header
// - default is DefaultContentType or 'application/octet-stream' (BINARY_CONTENT_TYPE)
// or 'application/fileextension' if FileName was specified
// - see @http://en.wikipedia.org/wiki/Internet_media_type for most common values
function GetMimeContentType(Content: Pointer; Len: PtrInt;
  const FileName: TFileName = '';
  const DefaultContentType: RawUtf8 = BINARY_CONTENT_TYPE): RawUtf8;

/// retrieve the HTTP header for MIME content type from a supplied binary buffer
// - just append HEADER_CONTENT_TYPE and GetMimeContentType() result
// - can be used as such:
// !  Call.OutHead := GetMimeContentTypeHeader(Call.OutBody,aFileName);
function GetMimeContentTypeHeader(const Content: RawByteString;
  const FileName: TFileName = ''): RawUtf8;

const
  /// the "magic" number used to identify .log.synlz compressed files, as
  // created by EventArchiveSynLZ / EventArchiveLizard callbacks
  LOG_MAGIC = $ABA51051;

/// retrieve if some content is compressed, from a supplied binary buffer
// - returns TRUE, if the header in binary buffer "may" be compressed (this method
// can trigger false positives), e.g. begin with most common already compressed
// zip/gz/gif/png/jpeg/avi/mp3/mp4 markers (aka "magic numbers")
function IsContentCompressed(Content: Pointer; Len: PtrInt): boolean;

/// fast guess of the size, in pixels, of a JPEG memory buffer
// - will only scan for basic JPEG structure, up to the StartOfFrame (SOF) chunk
// - returns TRUE if the buffer is likely to be a JPEG picture, and set the
// Height + Width variable with its dimensions - but there may be false positive
// recognition, and no waranty that the memory buffer holds a valid JPEG picture
// - returns FALSE if the buffer does not have any expected SOI/SOF markers
function GetJpegSize(jpeg: PAnsiChar; len: PtrInt;
  out Height, Width: integer): boolean; overload;


{ ************* Text Memory Buffers and Files }

type
  {$M+}
  /// able to read a UTF-8 text file using memory map
  // - much faster than TStringList.LoadFromFile()
  // - will ignore any trailing UTF-8 BOM in the file content, but will not
  // expect one either
  TMemoryMapText = class
  protected
    fLines: PPointerArray;
    fLinesMax: integer;
    fCount: integer;
    fMapEnd: PUtf8Char;
    fMap: TMemoryMap;
    fFileName: TFileName;
    fAppendedLines: TRawUtf8DynArray;
    fAppendedLinesCount: integer;
    function GetLine(aIndex: integer): RawUtf8;
      {$ifdef HASINLINE}inline;{$endif}
    function GetString(aIndex: integer): string;
      {$ifdef HASINLINE}inline;{$endif}
    /// call once by Create constructors when fMap has been initialized
    procedure LoadFromMap(AverageLineLength: integer = 32); virtual;
    /// call once per line, from LoadFromMap method
    // - default implementation will set  fLines[fCount] := LineBeg;
    // - override this method to add some per-line process at loading: it will
    // avoid reading the entire file more than once
    procedure ProcessOneLine(LineBeg, LineEnd: PUtf8Char); virtual;
  public
    /// initialize the memory mapped text file
    // - this default implementation just do nothing but is called by overloaded
    // constructors so may be overriden to initialize an inherited class
    constructor Create; overload; virtual;
    /// read an UTF-8 encoded text file
    // - every line beginning is stored into LinePointers[]
    constructor Create(const aFileName: TFileName); overload;
    /// read an UTF-8 encoded text file content
    // - every line beginning is stored into LinePointers[]
    // - this overloaded constructor accept an existing memory buffer (some
    // uncompressed data e.g.)
    constructor Create(aFileContent: PUtf8Char; aFileSize: integer); overload;
    /// release the memory map and internal LinePointers[]
    destructor Destroy; override;
    /// save the whole content into a specified stream
    // - including any runtime appended values via AddInMemoryLine()
    procedure SaveToStream(Dest: TStream; const Header: RawUtf8);
    /// save the whole content into a specified file
    // - including any runtime appended values via AddInMemoryLine()
    // - an optional header text can be added to the beginning of the file
    procedure SaveToFile(FileName: TFileName; const Header: RawUtf8 = '');
    /// add a new line to the already parsed content
    // - this line won't be stored in the memory mapped file, but stay in memory
    // and appended to the existing lines, until this instance is released
    procedure AddInMemoryLine(const aNewLine: RawUtf8); virtual;
    /// clear all in-memory appended rows
    procedure AddInMemoryLinesClear; virtual;
    /// retrieve the number of UTF-8 chars of the given line
    // - warning: no range check is performed about supplied index
    function LineSize(aIndex: integer): integer;
      {$ifdef HASINLINE}inline;{$endif}
    /// check if there is at least a given number of UTF-8 chars in the given line
    // - this is faster than LineSize(aIndex)<aMinimalCount for big lines
    function LineSizeSmallerThan(aIndex, aMinimalCount: integer): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// returns TRUE if the supplied text is contained in the corresponding line
    function LineContains(const aUpperSearch: RawUtf8; aIndex: integer): boolean; virtual;
    /// retrieve a line content as UTF-8
    // - a temporary UTF-8 string is created
    // - will return '' if aIndex is out of range
    property Lines[aIndex: integer]: RawUtf8
      read GetLine;
    /// retrieve a line content as generic VCL string type
    // - a temporary VCL string is created (after conversion for UNICODE Delphi)
    // - will return '' if aIndex is out of range
    property Strings[aIndex: integer]: string
      read GetString;
    /// direct access to each text line
    // - use LineSize() method to retrieve line length, since end of line will
    // NOT end with #0, but with #13 or #10
    // - warning: no range check is performed about supplied index
    property LinePointers: PPointerArray
      read fLines;
    /// the memory map used to access the raw file content
    property Map: TMemoryMap
      read fMap;
  published
    /// the file name which was opened by this instance
    property FileName: TFileName
      read fFileName write fFileName;
    /// the number of text lines
    property Count: integer
      read fCount;
  end;
  {$M-}


/// fast add some content to a RawByteString buffer
procedure AppendBufferToRawByteString(var Content: RawByteString;
  const Buffer; BufferLen: PtrInt);

/// fast add some characters to a RawUtf8 string
// - faster than SetString(tmp,Buffer,BufferLen); Text := Text+tmp;
procedure AppendBufferToRawUtf8(var Text: RawUtf8;
  Buffer: pointer; BufferLen: PtrInt);

/// fast add one character to a RawUtf8 string
// - avoid a temporary memory allocation of a string, so slightly faster than
// ! Text := Text + ch;
procedure AppendCharToRawUtf8(var Text: RawUtf8; Ch: AnsiChar);

/// fast add one character to a RawUtf8 string, if not already present
// - avoid a temporary memory allocation of a string, so faster alternative to
// ! if (Text<>'') and (Text[length(Text)]<>Ch) then Text := Text + ch;
procedure AppendCharOnceToRawUtf8(var Text: RawUtf8; Ch: AnsiChar);

/// fast add some characters to a RawUtf8 string
// - faster than Text := Text+RawUtf8(Buffers[0])+RawUtf8(Buffers[0])+...
// - will handle up to 64 Buffers[] - raise an ESynException on too many Buffers
procedure AppendBuffersToRawUtf8(var Text: RawUtf8; const Buffers: array of PUtf8Char);

/// fast add some characters from a RawUtf8 string into a given buffer
// - warning: the Buffer should contain enough space to store the Text, otherwise
// you may encounter buffer overflows and random memory errors
function AppendRawUtf8ToBuffer(Buffer: PUtf8Char; const Text: RawUtf8): PUtf8Char;

/// fast add some characters from ane buffer into another buffer
// - warning: the Buffer should contain enough space to store the Text, otherwise
// you may encounter buffer overflows and random memory errors
function AppendBufferToBuffer(Buffer: PUtf8Char; Text: pointer; Len: PtrInt): PUtf8Char;
  {$ifdef HASINLINE} inline; {$endif}

/// fast add text conversion of a 32-bit signed integer value into a given buffer
// - warning: the Buffer should contain enough space to store the text, otherwise
// you may encounter buffer overflows and random memory errors
function AppendUInt32ToBuffer(Buffer: PUtf8Char; Value: PtrUInt): PUtf8Char;

/// fast add text conversion of 0-999 integer value into a given buffer
// - warning: it won't check that Value is in 0-999 range
// - up to 4 bytes may be written to the buffer (including trailing #0)
function Append999ToBuffer(Buffer: PUtf8Char; Value: PtrUInt): PUtf8Char;
  {$ifdef HASINLINE}inline;{$endif}

const
  /// can be used to append to most English nouns to form a plural
  // - as used by the Plural() function
  PLURAL_FORM: array[boolean] of RawUtf8 = (
    '', 's');

/// write count number and append 's' (if needed) to form a plural English noun
// - for instance, Plural('row',100) returns '100 rows' with no heap allocation
function Plural(const itemname: shortstring; itemcount: cardinal): shortstring;

/// low-level fast conversion from binary data to escaped text
// - non printable characters will be written as $xx hexadecimal codes
// - will be #0 terminated, with '...' characters trailing on dmax overflow
// - ensure the destination buffer contains at least dmax bytes, which is
// always the case when using LogEscape() and its local TLogEscape variable
function EscapeBuffer(s: PAnsiChar; slen: integer;
  d: PAnsiChar; dmax: integer): PAnsiChar;

type
  /// 512 bytes buffer to be allocated on stack when using LogEscape()
  TLogEscape = array[0..511] of AnsiChar;

/// fill TLogEscape stack buffer with the (hexadecimal) chars of the input binary
// - up to 512 bytes will be escaped and appended to a local temp: TLogEscape
// variable, using the EscapeBuffer() low-level function
// - you can then log the resulting escaped text by passing the returned
// PAnsiChar as % parameter to a TSynLog.Log() method
// - the "enabled" parameter can be assigned from a process option, avoiding to
// process the escape if verbose logs are disabled
// - used e.g. to implement logBinaryFrameContent option for WebSockets
function LogEscape(source: PAnsiChar; sourcelen: integer; var temp: TLogEscape;
  enabled: boolean = true): PAnsiChar;
  {$ifdef HASINLINE}inline;{$endif}

/// returns a text buffer with the (hexadecimal) chars of the input binary
// - is much slower than LogEscape/EscapeToShort, but has no size limitation
function LogEscapeFull(source: PAnsiChar; sourcelen: integer): RawUtf8; overload;

/// returns a text buffer with the (hexadecimal) chars of the input binary
// - is much slower than LogEscape/EscapeToShort, but has no size limitation
function LogEscapeFull(const source: RawByteString): RawUtf8; overload;

/// fill a shortstring with the (hexadecimal) chars of the input text/binary
function EscapeToShort(source: PAnsiChar; sourcelen: integer): shortstring; overload;

/// fill a shortstring with the (hexadecimal) chars of the input text/binary
function EscapeToShort(const source: RawByteString): shortstring; overload;


/// get text File contents (even UTF-16 or UTF-8) and convert it into a
// Charset-compatible AnsiString (for Delphi 7) or an UnicodeString (for Delphi
// 2009 and up) according to any BOM marker at the beginning of the file
// - before Delphi 2009, the current string code page is used (i.e. CurrentAnsiConvert)
function AnyTextFileToString(const FileName: TFileName;
  ForceUtf8: boolean = false): string;

/// get text file contents (even UTF-16 or UTF-8) and convert it into an
// Unicode string according to any BOM marker at the beginning of the file
// - any file without any BOM marker will be interpreted as plain ASCII: in this
// case, the current string code page is used (i.e. CurrentAnsiConvert class,
// which is likely to be UTF-8 on a recent POSIX system)
function AnyTextFileToSynUnicode(const FileName: TFileName;
  ForceUtf8: boolean = false): SynUnicode;

/// get text file contents (even UTF-16 or UTF-8) and convert it into an
// UTF-8 string according to any BOM marker at the beginning of the file
// - if AssumeUtf8IfNoBom is FALSE, the current string code page is used (i.e.
// CurrentAnsiConvert class) for conversion from ANSI into UTF-8
// - if AssumeUtf8IfNoBom is TRUE, any file without any BOM marker will be
// interpreted as UTF-8
function AnyTextFileToRawUtf8(const FileName: TFileName;
  AssumeUtf8IfNoBom: boolean = false): RawUtf8;


/// generate some pascal source code holding some data binary as constant
// - can store sensitive information (e.g. certificates) within the executable
// - generates a source code snippet of the following format:
// ! const
// !   // Comment
// !   ConstName: array[0..2] of byte = (
// !     $01,$02,$03);
procedure BinToSource(Dest: TBaseWriter; const ConstName, Comment: RawUtf8;
  Data: pointer; Len: integer; PerLine: integer = 16); overload;

/// generate some pascal source code holding some data binary as constant
// - can store sensitive information (e.g. certificates) within the executable
// - generates a source code snippet of the following format:
// ! const
// !   // Comment
// !   ConstName: array[0..2] of byte = (
// !     $01,$02,$03);
function BinToSource(const ConstName, Comment: RawUtf8; Data: pointer;
  Len: integer; PerLine: integer = 16; const Suffix: RawUtf8 = ''): RawUtf8; overload;


{ *************************** TStreamRedirect and other Hash process }

/// compute the 32-bit default hash of a file content
// - you can specify your own hashing function if DefaultHasher is not what you expect
function HashFile(const FileName: TFileName; Hasher: THasher = nil): cardinal; overload;

type
  /// prototype of a file hashing function, returning its hexadecimal hash
  // - match HashFileCrc32c() below, HashFileCrc32() in mormot.core.zip,
  // and HashFileMd5/HashFileSha* in mormot.crypt.secure functions signature
  THashFile = function(const FileName: TFileName): RawUtf8;

  TStreamRedirect = class;

  /// TStreamHasher.Write optional progression callback
  // - see Sender properties like Context/Size/PerSecond and ExpectedSize
  // (which may be 0 if the download size is unknown)
  // - see e.g. TStreamRedirect.ProgressToConsole
  TOnStreamProgress = procedure(Sender: TStreamRedirect) of object;

  /// optional callback as used e.g. by THttpClientSocketWGet.OnStreamCreate
  TOnStreamCreate = function(const FileName: string; Mode: cardinal): TStream of object;

  /// an abstract pipeline stream able to redirect and hash read/written content
  // - can be used either Read() or Write() calls during its livetime
  // - hashing is performed on the fly during the Read/Write process
  // - it features also a callback to mark its progress
  // - can sleep during Read/Write to reach a LimitPerSecond average bandwidth
  TStreamRedirect = class(TStreamWithPosition)
  protected
    fRedirected: TStream;
    fExpectedSize, fCurrentSize, fExpectedWrittenSize, fProcessedSize: Int64;
    fStartTix, fReportTix, fLastTix, fTimeOut, fElapsed, fRemaining: Int64;
    fPerSecond, fLimitPerSecond: PtrInt;
    fPercent: integer;
    fOnProgress: TOnStreamProgress;
    fOnLog: TSynLogProc;
    fContext: RawUtf8;
    fTerminated: boolean;
    fConsoleLen: byte;
    fMode: (mUnknown, mRead, mWrite);
    function GetSize: Int64; override;
    procedure DoReport(ReComputeElapsed: boolean);
    procedure DoHash(data: pointer; len: integer); virtual; // do nothing
    procedure SetExpectedSize(Value: Int64);
    procedure ReadWriteHash(const Buffer; Count: Longint); virtual;
    procedure ReadWriteReport(const Caller: ShortString); virtual;
    function GetProgress: RawUtf8;
  public
    /// initialize the internal structure, and start the timing
    // - before calling Read/Write, you should set the Redirected property or
    // specify aRedirected here - which will be owned by this instance
    // - if aRead is true, ExpectedSize is set from aRedirected.Size
    constructor Create(aRedirected: TStream; aRead: boolean = false); reintroduce; virtual;
    /// release the associated Redirected stream
    destructor Destroy; override;
    /// can be used by for TOnStreamProgress callback writing into the console
    class procedure ProgressToConsole(Sender: TStreamRedirect);
    /// update the hash and redirect the data to the associated TStream
    // - also trigger OnProgress at least every second
    // - will raise an error if Write() (or Append) have been called before
    function Read(var Buffer; Count: Longint): Longint; override;
    /// update the hash and redirect the data to the associated TStream
    // - also trigger OnProgress at least every second
    // - will raise an error if Read() has been called before
    function Write(const Buffer; Count: Longint): Longint; override;
    /// update the hash of the existing Redirected stream content
    // - ready to Write() some new data after the existing
    procedure Append;
    /// notify end of process
    // - should be called explicitly when all Read()/Write() has been done
    procedure Ended;
    /// could be set from another thread to abort the streaming process
    // - will raise an exception at the next Read()/Write() call
    procedure Terminate;
    /// return the current state of the hash as lower hexadecimal
    // - by default, will return '' meaning that no hashing algorithm was set
    function GetHash: RawUtf8; virtual;
    /// current algorithm name as file/url extension, e.g. '.md5' or '.sha256'
    // - by default, will return '' meaning that no hashing algorithm was set
    class function GetHashFileExt: RawUtf8; virtual;
    /// apply the internal hash algorithm to the supplied file content
    // - could be used ahead of time to validate a cached file
    class function HashFile(const FileName: TFileName): RawUtf8;
    /// specify a TStream to which any Read()/Write() will be redirected
    // - this TStream instance will be owned by the TStreamRedirect
    property Redirected: TStream
      read fRedirected write fRedirected;
    /// you can specify a number of bytes for the final Redirected size
    // - will be used for the callback progress - could be left to 0 for Write()
    // if size is unknown
    property ExpectedSize: Int64
      read fExpectedSize write SetExpectedSize;
    /// how many bytes have passed through Read() or Write()
    // - may not equal Size or Position after an Append - e.g. on resumed
    // download from partial file
    property ProcessedSize: Int64
      read fProcessedSize;
    /// percentage of Size versus ExpectedSize
    // - equals 0 if ExpectedSize is 0
    property Percent: integer
      read fPercent;
    /// number of milliseconds elasped since beginning, as set by Read/Write
    property Elapsed: Int64
      read fElapsed;
    /// number of milliseconds remaining for full process, as set by Read/Write
    // - equals 0 if ExpectedSize is 0
    // - is just an estimation based on the average PerSecond speed
    property Remaining: Int64
      read fRemaining;
    /// number of bytes processed per second, since initialization of this instance
    property PerSecond: PtrInt
      read fPerSecond;
    /// can limit the Read/Write bandwidth used
    // - sleep so that PerSecond will keep close to this LimitPerSecond value
    property LimitPerSecond: PtrInt
      read fLimitPerSecond write fLimitPerSecond;
    /// Read/Write will raise an exception if not finished after TimeOut milliseconds
    property TimeOut: Int64
      read fTimeOut write fTimeOut;
    /// optional process context, e.g. a download URI, used for logging/progress
    property Context: RawUtf8
      read fContext write fContext;
    /// can be assigned from TSynLog.DoLog class method for low-level logging
    property OnLog: TSynLogProc
      read fOnLog write fOnLog;
    /// optional callback triggered during Read/Write
    // - at least at process startup and finish, and every second
    property OnProgress: TOnStreamProgress
      read fOnProgress write fOnProgress;
  published
    /// the current progression as text, as returned by ProgressToConsole
    property Progress: RawUtf8
      read GetProgress;
  end;

  /// meta-class of TStreamRedirect hierarchy
  TStreamRedirectClass = class of TStreamRedirect;

  /// TStreamRedirect with 32-bit THasher checksum
  TStreamRedirectHasher = class(TStreamRedirect)
  protected
    fHash: cardinal;
  public
    function GetHash: RawUtf8; override;
  end;

  /// TStreamRedirect with crc32c 32-bit checksum
  TStreamRedirectCrc32c = class(TStreamRedirectHasher)
  protected
    procedure DoHash(data: pointer; len: integer); override;
  public
    class function GetHashFileExt: RawUtf8; override;
  end;


/// compute the crc32c checksum of a given file
// - this function maps the THashFile signature
function HashFileCrc32c(const FileName: TFileName): RawUtf8;


{ ************* Markup (e.g. HTML or Emoji) process }

type
  /// tune AddHtmlEscapeWiki/AddHtmlEscapeMarkdown wrapper functions process
  // - heHtmlEscape will escape any HTML special chars, e.g. & into &amp;
  // - heEmojiToUtf8 will convert any Emoji text into UTF-8 Unicode character,
  // recognizing e.g. :joy: or :) in the text
  TTextWriterHtmlEscape = set of (
    heHtmlEscape,
    heEmojiToUtf8);

/// convert some wiki-like text into proper HTML
// - convert all #13#10 into <p>...</p>, *..* into <em>..</em>, +..+ into
// <strong>..</strong>, `..` into <code>..</code>, and http://... as
// <a href=http://...>
// - escape any HTML special chars, and Emoji tags as specified with esc
procedure AddHtmlEscapeWiki(W: TBaseWriter; P: PUtf8Char;
  esc: TTextWriterHtmlEscape = [heHtmlEscape, heEmojiToUtf8]);

/// convert minimal Markdown text into proper HTML
// - see https://enterprise.github.com/downloads/en/markdown-cheatsheet.pdf
// - convert all #13#10 into <p>...</p>, *..* into <em>..</em>, **..** into
// <strong>..</strong>, `...` into <code>...</code>, backslash espaces \\
// \* \_ and so on, [title](http://...) and detect plain http:// as
// <a href=...>
// - create unordered lists from trailing * + - chars, blockquotes from
// trailing > char, and code line from 4 initial spaces
// - as with default Markdown, won't escape HTML special chars (i.e. you can
// write plain HTML in the supplied text) unless esc is set otherwise
// - only inline-style links and images are supported yet (not reference-style);
// tables aren't supported either
procedure AddHtmlEscapeMarkdown(W: TBaseWriter; P: PUtf8Char;
  esc: TTextWriterHtmlEscape = [heEmojiToUtf8]);

/// escape some wiki-marked text into HTML
// - just a wrapper around AddHtmlEscapeWiki() process
function HtmlEscapeWiki(const wiki: RawUtf8;
  esc: TTextWriterHtmlEscape = [heHtmlEscape, heEmojiToUtf8]): RawUtf8;

/// escape some Markdown-marked text into HTML
// - just a wrapper around AddHtmlEscapeMarkdown() process
function HtmlEscapeMarkdown(const md: RawUtf8;
  esc: TTextWriterHtmlEscape = [heEmojiToUtf8]): RawUtf8;

type
  /// map the first Unicode page of Emojis, from U+1F600 to U+1F64F
  // - naming comes from github/Markdown :identifiers:
  TEmoji = (
    eNone,
    eGrinning,
    eGrin,
    eJoy,
    eSmiley,
    eSmile,
    eSweat_smile,
    eLaughing,
    eInnocent,
    eSmiling_imp,
    eWink,
    eBlush,
    eYum,
    eRelieved,
    eHeart_eyes,
    eSunglasses,
    eSmirk,
    eNeutral_face,
    eExpressionless,
    eUnamused,
    eSweat,
    ePensive,
    eConfused,
    eConfounded,
    eKissing,
    eKissing_heart,
    eKissing_smiling_eyes,
    eKissing_closed_eyes,
    eStuck_out_tongue,
    eStuck_out_tongue_winking_eye,
    eStuck_out_tongue_closed_eyes,
    eDisappointed,
    eWorried,
    eAngry,
    ePout,
    eCry,
    ePersevere,
    eTriumph,
    eDisappointed_relieved,
    eFrowning,
    eAnguished,
    eFearful,
    eWeary,
    eSleepy,
    eTired_face,
    eGrimacing,
    eSob,
    eOpen_mouth,
    eHushed,
    eCold_sweat,
    eScream,
    eAstonished,
    eFlushed,
    eSleeping,
    eDizzy_face,
    eNo_mouth,
    eMask,
    eSmile_cat,
    eJoy_cat,
    eSmiley_cat,
    eHeart_eyes_cat,
    eSmirk_cat,
    eKissing_cat,
    ePouting_cat,
    eCrying_cat_face,
    eScream_cat,
    eSlightly_frowning_face,
    eSlightly_smiling_face,
    eUpside_down_face,
    eRoll_eyes,
    eNo_good,
    oOk_woman,
    eBow,
    eSee_no_evil,
    eHear_no_evil,
    eSpeak_no_evil,
    eRaising_hand,
    eRaised_hands,
    eFrowning_woman,
    ePerson_with_pouting_face,
    ePray);

var
  /// github/Markdown compatible text of Emojis
  // - e.g. 'grinning' or 'person_with_pouting_face'
  EMOJI_TEXT: array[TEmoji] of RawUtf8;

  /// github/Markdown compatible tag of Emojis, including trailing and ending :
  // - e.g. ':grinning:' or ':person_with_pouting_face:'
  EMOJI_TAG: array[TEmoji] of RawUtf8;

  /// the Unicode character matching a given Emoji, after UTF-8 encoding
  EMOJI_UTF8: array[TEmoji] of RawUtf8;

  /// low-level access to TEmoji RTTI - used when inlining EmojiFromText()
  EMOJI_RTTI: PShortString;

  /// to recognize simple :) :( :| :/ :D :o :p :s characters as smilleys
  EMOJI_AFTERDOTS: array['('..'|'] of TEmoji;

/// recognize github/Markdown compatible text of Emojis
// - for instance 'sunglasses' text buffer will return eSunglasses
// - returns eNone if no case-insensitive match was found
function EmojiFromText(P: PUtf8Char; len: PtrInt): TEmoji;
  {$ifdef HASINLINE}inline;{$endif}

/// low-level parser of github/Markdown compatible text of Emojis
// - supplied P^ should point to ':'
// - will append the recognized UTF-8 Emoji if P contains e.g. :joy: or :)
// - will append ':' if no Emoji text is recognized, and return eNone
// - will try both EMOJI_AFTERDOTS[] and EMOJI_RTTI[] reference set
// - if W is nil, won't append anything, but just return the recognized TEmoji
function EmojiParseDots(var P: PUtf8Char; W: TBaseWriter = nil): TEmoji;

/// low-level conversion of UTF-8 Emoji sequences into github/Markdown :identifiers:
procedure EmojiToDots(P: PUtf8Char; W: TBaseWriter); overload;

/// conversion of UTF-8 Emoji sequences into github/Markdown :identifiers:
function EmojiToDots(const text: RawUtf8): RawUtf8; overload;

/// low-level conversion of github/Markdown :identifiers: into UTF-8 Emoji sequences
procedure EmojiFromDots(P: PUtf8Char; W: TBaseWriter); overload;

/// conversion of github/Markdown :identifiers: into UTF-8 Emoji sequences
function EmojiFromDots(const text: RawUtf8): RawUtf8; overload;


{ ************ RawByteString Buffers Aggregation via TRawByteStringGroup }

type
  /// item as stored in a TRawByteStringGroup instance
  TRawByteStringGroupValue = record
    Position: integer;
    Value: RawByteString;
  end;

  PRawByteStringGroupValue = ^TRawByteStringGroupValue;

  /// items as stored in a TRawByteStringGroup instance
  TRawByteStringGroupValueDynArray = array of TRawByteStringGroupValue;

  /// store several RawByteString content with optional concatenation
  {$ifdef USERECORDWITHMETHODS}
  TRawByteStringGroup = record
  {$else}
  TRawByteStringGroup = object
  {$endif USERECORDWITHMETHODS}
  public
    /// actual list storing the data
    Values: TRawByteStringGroupValueDynArray;
    /// how many items are currently stored in Values[]
    Count: integer;
    /// the current size of data stored in Values[]
    Position: integer;
    /// naive but efficient cache for Find()
    LastFind: integer;
    /// add a new item to Values[]
    procedure Add(const aItem: RawByteString); overload;
    /// add a new item to Values[]
    procedure Add(aItem: pointer; aItemLen: integer); overload;
    /// add another TRawByteStringGroup to Values[]
    procedure Add(const aAnother: TRawByteStringGroup); overload;
    /// low-level method to abort the latest Add() call
    // - warning: will work only once, if an Add() has actually been just called:
    // otherwise, the behavior is unexpected, and may wrongly truncate data
    procedure RemoveLastAdd;
    /// compare two TRawByteStringGroup instance stored text
    function Equals(const aAnother: TRawByteStringGroup): boolean;
    /// clear any stored information
    procedure Clear;
    /// append stored information into another RawByteString, and clear content
    procedure AppendTextAndClear(var aDest: RawByteString);
    // compact the Values[] array into a single item
    // - is also used by AsText to compute a single RawByteString
    procedure Compact;
    /// return all content as a single RawByteString
    // - will also compact the Values[] array into a single item (which is returned)
    function AsText: RawByteString;
    /// return all content as a single TByteDynArray
    function AsBytes: TByteDynArray;
    /// save all content into a TTextWriter instance
    procedure Write(W: TBaseWriter; Escape: TTextWriterKind = twJsonEscape); overload;
    /// save all content into a TBufferWriter instance
    procedure WriteBinary(W: TBufferWriter); overload;
    /// save all content as a string into a TBufferWriter instance
    // - storing the length as WriteVarUInt32() prefix
    procedure WriteString(W: TBufferWriter);
    /// add another TRawByteStringGroup previously serialized via WriteString()
    procedure AddFromReader(var aReader: TFastReader);
    /// returns a pointer to Values[] containing a given position
    // - returns nil if not found
    function Find(aPosition: integer): PRawByteStringGroupValue; overload;
    /// returns a pointer to Values[].Value containing a given position and length
    // - returns nil if not found
    function Find(aPosition, aLength: integer): pointer; overload;
    /// returns the text at a given position in Values[]
    // - text should be in a single Values[] entry
    procedure FindAsText(aPosition, aLength: integer; out aText: RawByteString); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// returns the text at a given position in Values[]
    // - text should be in a single Values[] entry
    function FindAsText(aPosition, aLength: integer): RawByteString; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// returns the text at a given position in Values[]
    // - text should be in a single Values[] entry
    // - explicitly returns null if the supplied text was not found
    procedure FindAsVariant(aPosition, aLength: integer; out aDest: variant);
      {$ifdef HASINLINE}inline;{$endif}
    /// append the text at a given position in Values[], JSON escaped by default
    // - text should be in a single Values[] entry
    procedure FindWrite(aPosition, aLength: integer; W: TBaseWriter;
      Escape: TTextWriterKind = twJsonEscape; TrailingCharsToIgnore: integer = 0);
      {$ifdef HASINLINE}inline;{$endif}
    /// append the blob at a given position in Values[], Base64 encoded
    // - text should be in a single Values[] entry
    procedure FindWriteBase64(aPosition, aLength: integer; W: TBaseWriter;
      withMagic: boolean);
      {$ifdef HASINLINE}inline;{$endif}
    /// copy the text at a given position in Values[]
    // - text should be in a single Values[] entry
    procedure FindMove(aPosition, aLength: integer; aDest: pointer);
  end;

  /// pointer reference to a TRawByteStringGroup
  PRawByteStringGroup = ^TRawByteStringGroup;

  /// store one RawByteString content with an associated length
  // - to be used e.g. as a convenient reusable memory buffer
  {$ifdef USERECORDWITHMETHODS}
  TRawByteStringBuffer = record
  {$else}
  TRawByteStringBuffer = object
  {$endif USERECORDWITHMETHODS}
  private
    /// the actual storage, with length(Buffer) as Capacity
    fBuffer: RawByteString;
    fLen: PtrInt;
    function GetCapacity: PtrInt;
      {$ifdef HASINLINE}inline;{$endif}
  public
    /// set Len to 0, but doesn't clear/free the Buffer itself
    procedure Reset;
      {$ifdef HASINLINE}inline;{$endif}
    /// release/free the internal Buffer storage
    procedure Clear;
    /// a convenient wrapper to pointer(fBuffer) for direct Buffer/Len use
    function Buffer: pointer;
      {$ifdef HASINLINE}inline;{$endif}
    /// how many bytes are currently used in the Buffer
    property Len: PtrInt
      read fLen write fLen;
    /// how many bytes are currently allocated in the Buffer
    property Capacity: PtrInt
      read GetCapacity;
    /// add some content to the Buffer, resizing it if needed
    // - could optionally include a #13#10 end of line
    procedure Append(P: pointer; PLen: PtrInt; CRLF: boolean = false); overload;
    /// add some content to the Buffer, resizing it if needed
    procedure Append(const Text: RawUtf8; CRLF: boolean = false); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// add some values as text to the Buffer, resizing it if needed
    procedure Append(const Args: array of const; CRLF: boolean = false); overload;
    /// check if Append(Bytes) would not need to resize the internal Buffer
    function CanAppend(Bytes: PtrInt): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// ensure the internal Buffer has at least MaxSize bytes and return it
    // - also reset the internal Len to 0
    function Reserve(MaxSize: PtrInt): pointer;
    /// similar to delete(fBuffer, 1, FirstBytes)
    procedure Remove(FirstBytes: PtrInt);
    /// move up to Count bytes from the internal Buffer into another place
    // - returns how many bytes were available to be copied into Dest^
    // - then remove the copied bytes from the internal Buffer/Len storage
    function Extract(Dest: pointer; Count: PtrInt): PtrInt;
    /// move up to Count bytes from the internal Buffer into another place
    // - returns how many bytes were available to be copied into Dest^
    // - don't move any byte, but just update the given Pos index
    function ExtractAt(var Dest: PAnsiChar; var Count: PtrInt;
      var Pos: PtrInt): PtrInt;
    /// similar to insert(P/PLen, fBuffer, Position + 1)
    // - could optionally include a #13#10 pattern between the two
    procedure Insert(P: pointer; PLen: PtrInt; Position: PtrInt = 0;
      CRLF: boolean = false);
    /// retrieve the current Buffer/Len content as RawUtf8 text
    // - with some optional overhead for faster reallocmem at concatenation
    // - won't force Len to 0: caller should call Reset if done with it
    procedure AsText(out Text: RawUtf8; Overhead: PtrInt = 0);
  end;


implementation


{ ************ Variable Length Integer Encoding / Decoding }

function ToVarInt32(Value: PtrInt; Dest: PByte): PByte;
begin
  // 0=0,1=1,2=-1,3=2,4=-2...
  if Value < 0 then
    // -1->2, -2->4..
    Value := (-Value) shl 1
  else if Value > 0 then
    // 1->1, 2->3..
    Value := (Value shl 1) - 1;
    // 0->0
  result := ToVarUInt32(Value, Dest);
end;

function ToVarUInt32(Value: cardinal; Dest: PByte): PByte;
label
  _1, _2, _3; // ugly but fast
begin
  if Value > $7f then
  begin
    if Value < $80 shl 7 then
      goto _1
    else if Value < $80 shl 14 then
      goto _2
    else if Value < $80 shl 21 then
      goto _3;
    Dest^ := (Value and $7F) or $80;
    Value := Value shr 7;
    inc(Dest);
_3: Dest^ := (Value and $7F) or $80;
    Value := Value shr 7;
    inc(Dest);
_2: Dest^ := (Value and $7F) or $80;
    Value := Value shr 7;
    inc(Dest);
_1: Dest^ := (Value and $7F) or $80;
    Value := Value shr 7;
    inc(Dest);
  end;
  Dest^ := Value;
  inc(Dest);
  result := Dest;
end;

function ToVarUInt32Length(Value: PtrUInt): PtrUInt;
begin
  if Value <= $7f then
    result := 1
  else if Value < $80 shl 7 then
    result := 2
  else if Value < $80 shl 14 then
    result := 3
  else if Value < $80 shl 21 then
    result := 4
  else
    result := 5;
end;

function ToVarUInt32LengthWithData(Value: PtrUInt): PtrUInt;
begin
  if Value <= $7f then
    result := Value + 1
  else if Value < $80 shl 7 then
    result := Value + 2
  else if Value < $80 shl 14 then
    result := Value + 3
  else if Value < $80 shl 21 then
    result := Value + 4
  else
    result := Value + 5;
end;

function FromVarUInt32(var Source: PByte): cardinal;
begin
  result := Source^;
  inc(Source);
  if result > $7f then
    result := (result and $7F) or FromVarUInt32Up128(Source);
end;

function FromVarUInt32Big(var Source: PByte): cardinal;
var
  c: cardinal;
  p: PByte;
begin
  p := Source;
  result := p^;
  inc(p);
  if result > $7f then
  begin
    // Values between 128 and 16256
    c := p^;
    c := c shl 7;
    result := result and $7F or c;
    inc(p);
    if c > $7f shl 7 then
    begin
      // Values between 16257 and 2080768
      c := p^;
      c := c shl 14;
      inc(p);
      result := result and $3FFF or c;
      if c > $7f shl 14 then
      begin
        // Values between 2080769 and 266338304
        c := p^;
        c := c shl 21;
        inc(p);
        result := result and $1FFFFF or c;
        if c > $7f shl 21 then
        begin
          // Values above 266338304
          c := p^;
          c := c shl 28;
          inc(p);
          result := result and $FFFFFFF or c;
        end;
      end;
    end;
  end;
  Source := p;
end;

function FromVarUInt32Up128(var Source: PByte): cardinal;
var
  c: cardinal;
  p: PByte;
begin
  // Values above 127
  p := Source;
  result := p^ shl 7;
  inc(p);
  if result > $7f shl 7 then
  begin
    // Values above 16256
    c := p^;
    c := c shl 14;
    inc(p);
    result := result and $3FFF or c;
    if c > $7f shl 14 then
    begin
      // Values above 2080768
      c := p^;
      c := c shl 21;
      inc(p);
      result := result and $1FFFFF or c;
      if c > $7f shl 21 then
      begin
        // Values above 266338304
        c := p^;
        c := c shl 28;
        inc(p);
        result := result and $FFFFFFF or c;
      end;
    end;
  end;
  Source := p;
end;

function FromVarUInt32(var Source: PByte; SourceMax: PByte;
  out Value: cardinal): boolean;
begin
  if SourceMax = nil then
  begin
    Value := FromVarUInt32(Source);
    result := true;
  end
  else
  begin
    Source := FromVarUInt32Safe(Source, SourceMax, Value);
    result := Source <> nil;
  end;
end;

function FromVarUInt32Safe(Source, SourceMax: PByte; out Value: cardinal): PByte;
var
  c: cardinal;
begin
  result := nil; // error
  if PAnsiChar(Source) >= PAnsiChar(SourceMax) then
    exit;
  c := Source^;
  inc(Source);
  Value := c;
  if c > $7f then
  begin
    // Values between 128 and 16256
    if PAnsiChar(Source) >= PAnsiChar(SourceMax) then
      exit;
    c := Source^;
    c := c shl 7;
    Value := Value and $7F or c;
    inc(Source);
    if c > $7f shl 7 then
    begin
      // Values between 16257 and 2080768
      if PAnsiChar(Source) >= PAnsiChar(SourceMax) then
        exit;
      c := Source^;
      c := c shl 14;
      inc(Source);
      Value := Value and $3FFF or c;
      if c > $7f shl 14 then
      begin
        // Values between 2080769 and 266338304
        if PAnsiChar(Source) >= PAnsiChar(SourceMax) then
          exit;
        c := Source^;
        c := c shl 21;
        inc(Source);
        Value := Value and $1FFFFF or c;
        if c > $7f shl 21 then
        begin
          // Values above 266338304
          if PAnsiChar(Source) >= PAnsiChar(SourceMax) then
            exit;
          c := Source^;
          c := c shl 28;
          inc(Source);
          Value := Value and $FFFFFFF or c;
        end;
      end;
    end;
  end;
  result := Source; // safely decoded
end;

function FromVarInt32(var Source: PByte): integer;
var
  c: cardinal;
  p: PByte;
begin
  // fast stand-alone function with no FromVarUInt32 call
  p := Source;
  result := p^;
  inc(p);
  if result > $7f then
  begin
    c := p^;
    c := c shl 7;
    result := result and $7F or integer(c);
    inc(p);
    if c > $7f shl 7 then
    begin
      c := p^;
      c := c shl 14;
      inc(p);
      result := result and $3FFF or integer(c);
      if c > $7f shl 14 then
      begin
        c := p^;
        c := c shl 21;
        inc(p);
        result := result and $1FFFFF or integer(c);
        if c > $7f shl 21 then
        begin
          c := p^;
          c := c shl 28;
          inc(p);
          result := result and $FFFFFFF or integer(c);
        end;
      end;
    end;
  end;
  Source := p;
  // 0=0,1=1,2=-1,3=2,4=-2...
  if result and 1 <> 0 then
    // 1->1, 3->2..
    result := result shr 1 + 1
  else
    // 0->0, 2->-1, 4->-2..
    result := -(result shr 1);
end;

function FromVarUInt32High(var Source: PByte): cardinal;
var
  c: cardinal;
begin
  result := Source^;
  inc(Source);
  c := Source^ shl 7;
  inc(Source);
  result := result and $7F or c;
  if c <= $7f shl 7 then
    exit;
  c := Source^ shl 14;
  inc(Source);
  result := result and $3FFF or c;
  if c <= $7f shl 14 then
    exit;
  c := Source^ shl 21;
  inc(Source);
  result := result and $1FFFFF or c;
  if c <= $7f shl 21 then
    exit;
  c := Source^ shl 28;
  inc(Source);
  result := result and $FFFFFFF or c;
end;

function ToVarInt64(Value: Int64; Dest: PByte): PByte;
begin
  // 0=0,1=1,2=-1,3=2,4=-2...
{$ifdef CPU32}
  if Value <= 0 then
    // 0->0, -1->2, -2->4..
    result := ToVarUInt64((-Value) shl 1, Dest)
  else
     // 1->1, 2->3..
    result := ToVarUInt64((Value shl 1) - 1, Dest);
{$else}
  if Value <= 0 then
    // 0->0, -1->2, -2->4..
    Value := (-Value) shl 1
  else
    // 1->1, 2->3..
    Value := (Value shl 1) - 1;
  result := ToVarUInt64(Value, Dest);
{$endif CPU32}
end;

function ToVarUInt64(Value: QWord; Dest: PByte): PByte;
var
  c: cardinal;
label
  _1, _2, _4; // ugly but fast
begin
  repeat
    c := Value;
    {$ifdef CPU32}
    if PCardinalArray(@Value)^[1] = 0 then
    {$else}
    if Value shr 32 = 0 then
    {$endif CPU32}
      begin
        if c > $7f then
        begin
          if c < $80 shl 7 then
            goto _1
          else if c < $80 shl 14 then
            goto _2
          else if c >= $80 shl 21 then
            goto _4;
          Dest^ := (c and $7F) or $80;
          c := c shr 7;
          inc(Dest);
_2:       Dest^ := (c and $7F) or $80;
          c := c shr 7;
          inc(Dest);
_1:       Dest^ := (c and $7F) or $80;
          c := c shr 7;
          inc(Dest);
        end;
        Dest^ := c;
        inc(Dest);
        result := Dest;
        exit;
      end;
_4: PCardinal(Dest)^ := (c and $7F) or (((c shr 7) and $7F) shl 8) or
      (((c shr 14) and $7F) shl 16) or (((c shr 21) and $7F) shl 24) or $80808080;
    inc(Dest, 4);
    Value := Value shr 28;
  until false;
end;

function FromVarUInt64(var Source: PByte): QWord;
var
  c, n: PtrUInt;
  p: PByte;
begin
  p := Source;
  {$ifdef CPU64}
  result := p^;
  if result > $7f then
  begin
    result := result and $7F;
  {$else}
  if p^ > $7f then
  begin
    result := PtrUInt(p^) and $7F;
  {$endif}
    n := 0;
    inc(p);
    repeat
      c := p^;
      inc(n, 7);
      if c <= $7f then
        break;
      result := result or (QWord(c and $7f) shl n);
      inc(p);
    until false;
    result := result or (QWord(c) shl n);
  end
  {$ifdef CPU32}
  else
    result := p^
  {$endif CPU32};
  inc(p);
  Source := p;
end;

function FromVarUInt64Safe(Source, SourceMax: PByte; out Value: QWord): PByte;
var
  c, n: PtrUInt;
begin
  result := nil; // error
  if PAnsiChar(Source) >= PAnsiChar(SourceMax) then
    exit;
  c := Source^;
  inc(Source);
  if c > $7f then
  begin
    Value := c and $7F;
    n := 7;
    repeat
      if PAnsiChar(Source) >= PAnsiChar(SourceMax) then
        exit;
      c := Source^;
      inc(Source);
      if c <= $7f then
        break;
      c := c and $7f;
      Value := Value or (QWord(c) shl n);
      inc(n, 7);
    until false;
    Value := Value or (QWord(c) shl n);
  end
  else
    Value := c;
  result := Source; // safely decoded
end;

function FromVarUInt64(var Source: PByte; SourceMax: PByte; out Value: QWord): boolean;
begin
  if SourceMax = nil then
  begin
    Value := FromVarUInt64(Source);
    result := true;
  end
  else
  begin
    Source := FromVarUInt64Safe(Source, SourceMax, Value);
    result := Source <> nil;
  end;
end;

function FromVarInt64(var Source: PByte): Int64;
var
  c, n: PtrUInt;
begin
  // 0=0,1=1,2=-1,3=2,4=-2...
{$ifdef CPU64}
  result := Source^;
  if result > $7f then
  begin
    result := result and $7F;
    n := 0;
    inc(Source);
    repeat
      c := Source^;
      inc(n, 7);
      if c <= $7f then
        break;
      result := result or (Int64(c and $7f) shl n);
      inc(Source);
    until false;
    result := result or (Int64(c) shl n);
  end;
  if result and 1 <> 0 then
    // 1->1, 3->2..
    result := result shr 1 + 1
  else
    // 0->0, 2->-1, 4->-2..
    result := -(result shr 1);
{$else}
  c := Source^;
  if c > $7f then
  begin
    result := c and $7F;
    n := 0;
    inc(Source);
    repeat
      c := Source^;
      inc(n, 7);
      if c <= $7f then
        break;
      result := result or (Int64(c and $7f) shl n);
      inc(Source);
    until false;
    result := result or (Int64(c) shl n);
    if PCardinal(@result)^ and 1 <> 0 then
      // 1->1, 3->2..
      result := result shr 1 + 1
    else
      // 0->0, 2->-1, 4->-2..
      result := -(result shr 1);
  end
  else
  begin
    if c = 0 then
      result := 0
    else if c and 1 = 0 then
      // 0->0, 2->-1, 4->-2..
      result := -Int64(c shr 1)
    else
      // 1->1, 3->2..
      result := (c shr 1) + 1;
  end;
{$endif CPU64}
  inc(Source);
end;

function FromVarInt64Value(Source: PByte): Int64;
var
  c, n: PtrUInt;
begin
// 0=0,1=1,2=-1,3=2,4=-2...
  c := Source^;
  if c > $7f then
  begin
    result := c and $7F;
    n := 0;
    inc(Source);
    repeat
      c := Source^;
      inc(n, 7);
      if c <= $7f then
        break;
      result := result or (Int64(c and $7f) shl n);
      inc(Source);
    until false;
    result := result or (Int64(c) shl n);
    {$ifdef CPU64}
    if result and 1 <> 0 then
    {$else}
    if PCardinal(@result)^ and 1 <> 0 then
    {$endif}
      // 1->1, 3->2..
      result := result shr 1 + 1
    else
      // 0->0, 2->-1, 4->-2..
      result := -Int64(result shr 1);
  end
  else if c = 0 then
    result := 0
  else if c and 1 = 0 then
    // 0->0, 2->-1, 4->-2..
    result := -Int64(c shr 1)
  else
    // 1->1, 3->2..
    result := (c shr 1) + 1;
end;

function GotoNextVarInt(Source: PByte): pointer;
begin
  if Source <> nil then
  begin
    if Source^ > $7f then
      repeat
        inc(Source)
      until Source^ <= $7f;
    inc(Source);
  end;
  result := Source;
end;


function ToVarString(const Value: RawUtf8; Dest: PByte): PByte;
var
  Len: integer;
begin
  Len := Length(Value);
  Dest := ToVarUInt32(Len, Dest);
  if Len > 0 then
  begin
    MoveFast(pointer(Value)^, Dest^, Len);
    result := pointer(PAnsiChar(Dest) + Len);
  end
  else
    result := Dest;
end;

function GotoNextVarString(Source: PByte): pointer;
begin
  result := Pointer(PtrUInt(Source) + FromVarUInt32(Source));
end;

function FromVarString(var Source: PByte): RawUtf8;
var
  len: PtrUInt;
begin
  len := FromVarUInt32(Source);
  FastSetStringCP(result, Source, len, CP_UTF8);
  inc(Source, len);
end;

function FromVarString(var Source: PByte; SourceMax: PByte): RawUtf8;
var
  len: cardinal;
begin
  Source := FromVarUInt32Safe(Source, SourceMax, len);
  if (Source = nil) or
      (PAnsiChar(Source) + len > PAnsiChar(SourceMax)) then
    len := 0;
  FastSetStringCP(result, Source, len, CP_UTF8);
  inc(Source, len);
end;

procedure FromVarString(var Source: PByte; var Value: TSynTempBuffer);
var
  len: integer;
begin
  len := FromVarUInt32(Source);
  Value.Init(Source, len);
  PByteArray(Value.buf)[len] := 0; // include trailing #0
  inc(Source, len);
end;

function FromVarString(var Source: PByte; SourceMax: PByte;
  var Value: TSynTempBuffer): boolean;
var
  len: cardinal;
begin
  if SourceMax = nil then
    len := FromVarUInt32(Source)
  else
  begin
    Source := FromVarUInt32Safe(Source, SourceMax, len);
    if (Source = nil) or
       (PAnsiChar(Source) + len > PAnsiChar(SourceMax)) then
    begin
      result := false;
      exit;
    end;
  end;
  Value.Init(Source, len);
  PByteArray(Value.buf)[len] := 0; // include trailing #0
  inc(Source, len);
  result := true;
end;

procedure FromVarString(var Source: PByte; var Value: RawByteString;
  CodePage: integer);
var
  Len: PtrUInt;
begin
  Len := FromVarUInt32(Source);
  FastSetStringCP(Value, Source, Len, CodePage);
  inc(Source, Len);
end;

function FromVarString(var Source: PByte; SourceMax: PByte;
  var Value: RawByteString; CodePage: integer): boolean;
var
  len: cardinal;
begin
  if SourceMax = nil then
    len := FromVarUInt32(Source)
  else
  begin
    Source := FromVarUInt32Safe(Source, SourceMax, len);
    if (Source = nil) or
       (PAnsiChar(Source) + len > PAnsiChar(SourceMax)) then
    begin
      result := false;
      exit;
    end;
  end;
  FastSetStringCP(Value, Source, len, CodePage);
  inc(Source, len);
  result := true;
end;

function FromVarBlob(Data: PByte): TValueResult;
begin
  result.Len := FromVarUInt32(Data);
  result.Ptr := pointer(Data);
end;



{ ****************** TFastReader / TBufferWriter Binary Streams }

{ TFastReader }

procedure TFastReader.Init(Buffer: pointer; Len: PtrInt);
begin
  P := Buffer;
  Last := PAnsiChar(Buffer) + Len;
  OnErrorOverflow := nil;
  OnErrorData := nil;
  CustomVariants := nil;
end;

procedure TFastReader.Init(const Buffer: RawByteString);
begin
  Init(pointer(Buffer), length(Buffer));
end;

procedure TFastReader.ErrorOverflow;
begin
  if Assigned(OnErrorOverflow) then
    OnErrorOverflow
  else
    raise EFastReader.Create('Reached End of Input');
end;

procedure TFastReader.ErrorData(const fmt: RawUtf8; const args: array of const);
begin
  if Assigned(OnErrorData) then
    OnErrorData(fmt, args)
  else
    raise EFastReader.CreateUtf8('Incorrect Data: ' + fmt, args);
end;

function TFastReader.EOF: boolean;
begin
  result := P >= Last;
end;

function TFastReader.RemainingLength: PtrUInt;
begin
  result := PtrUInt(Last) - PtrUInt(P);
end;

function TFastReader.NextByte: byte;
begin
  if P >= Last then
    ErrorOverflow;
  result := ord(P^);
  inc(P);
end;

function TFastReader.NextByteSafe(dest: pointer): boolean;
begin
  if P >= Last then
    result := false
  else
  begin
    PAnsiChar(dest)^ := P^;
    inc(P);
    result := true;
  end;
end;

function TFastReader.Next2: cardinal;
begin
  if P + 1 >= Last then
    ErrorOverflow;
  result := PWord(P)^;
  inc(P, 2);
end;

function TFastReader.Next4: cardinal;
begin
  if P + 3 >= Last then
    ErrorOverflow;
  result := PCardinal(P)^;
  inc(P, 4);
end;

function TFastReader.Next8: Qword;
begin
  if P + 7 >= Last then
    ErrorOverflow;
  result := PQWord(P)^;
  inc(P, 8);
end;

function TFastReader.NextByteEquals(Value: byte): boolean;
begin
  if P >= Last then
    ErrorOverflow;
  if ord(P^) = Value then
  begin
    inc(P);
    result := true;
  end
  else
    result := false;
end;

function TFastReader.Next(DataLen: PtrInt): pointer;
begin
  if P + DataLen > Last then
    ErrorOverflow;
  result := P;
  inc(P, DataLen);
end;

function TFastReader.NextSafe(out Data: Pointer; DataLen: PtrInt): boolean;
begin
  if P + DataLen > Last then
    result := false
  else
  begin
    Data := P;
    inc(P, DataLen);
    result := true;
  end;
end;

procedure TFastReader.Copy(Dest: Pointer; DataLen: PtrInt);
begin
  if P + DataLen > Last then
    ErrorOverflow;
  MoveFast(P^, Dest^, DataLen);
  inc(P, DataLen);
end;

function TFastReader.CopySafe(Dest: Pointer; DataLen: PtrInt): boolean;
begin
  if P + DataLen > Last then
    result := false
  else
  begin
    MoveFast(P^, Dest^, DataLen);
    inc(P, DataLen);
    result := true;
  end;
end;

function TFastReader.VarInt32: integer;
begin
  result := VarUInt32;
  if result and 1 <> 0 then
    // 1->1, 3->2..
    result := result shr 1 + 1
  else    // 0->0, 2->-1, 4->-2..
    result := -(result shr 1);
end;

function TFastReader.VarInt64: Int64;
begin
  result := VarUInt64;
  if result and 1 <> 0 then
    // 1->1, 3->2..
    result := result shr 1 + 1
  else    // 0->0, 2->-1, 4->-2..
    result := -(result shr 1);
end;

{$ifdef CPUX86} // not enough CPU registers

function TFastReader.VarUInt32: cardinal;
var
  c: cardinal;
label
  e;
begin
  if P >= Last then
    goto e;
  result := ord(P^);
  inc(P);
  if result <= $7f then
    exit;
  if P >= Last then
    goto e;
  c := ord(P^) shl 7;
  inc(P);
  result := result and $7F or c;
  if c <= $7f shl 7 then
    exit; // Values between 128 and 16256
  if P >= Last then
    goto e;
  c := ord(P^) shl 14;
  inc(P);
  result := result and $3FFF or c;
  if c <= $7f shl 14 then
    exit; // Values between 16257 and 2080768
  if P >= Last then
    goto e;
  c := ord(P^) shl 21;
  inc(P);
  result := result and $1FFFFF or c;
  if c <= $7f shl 21 then
    exit; // Values between 2080769 and 266338304
  if P >= Last then
e:begin
    {$ifdef ISDELPHI}
    result := 0; // avoid paranoid compiler hint
    {$endif}
    ErrorOverflow;
  end;
  c := ord(P^) shl 28;
  inc(P);
  result := result {%H-}and $FFFFFFF or c;
end;

procedure TFastReader.VarNextInt;
begin
  repeat
    if P >= Last then
      break;  // reached end of input
    if P^ <= #$7f then
      break; // reached end of VarUInt32/VarUInt64
    inc(P);
  until false;
  inc(P);
end;

procedure TFastReader.VarNextInt(count: integer);
begin
  if count = 0 then
    exit;
  repeat
    if P >= Last then
      break;  // reached end of input
    if P^ > #$7f then
    begin
      inc(P);
      continue; // didn't reach end of VarUInt32/VarUInt64
    end;
    inc(P);
    dec(count);
    if count = 0 then
      break;
  until false;
end;

{$else not CPUX86} // on x86_64 and ARM, use registers for P/Last values

function TFastReader.VarUInt32: cardinal;
var
  c: cardinal;
  s, l: PByte;
label
  e, f;
begin
  s := pointer(P);
  l := pointer(Last);
  if PAnsiChar(s) >= PAnsiChar(l) then
    goto e;
  result := s^;
  inc(s);
  if result <= $7f then
    goto f;
  if PAnsiChar(s) >= PAnsiChar(l) then
    goto e;
  c := s^ shl 7;
  inc(s);
  result := result and $7F or c;
  if c <= $7f shl 7 then
    goto f; // Values between 128 and 16256
  if PAnsiChar(s) >= PAnsiChar(l) then
    goto e;
  c := s^ shl 14;
  inc(s);
  result := result and $3FFF or c;
  if c <= $7f shl 14 then
    goto f; // Values between 16257 and 2080768
  if PAnsiChar(s) >= PAnsiChar(l) then
    goto e;
  c := s^ shl 21;
  inc(s);
  result := result and $1FFFFF or c;
  if c <= $7f shl 21 then
    goto f; // Values between 2080769 and 266338304
  if PAnsiChar(s) >= PAnsiChar(l) then
e:begin
    {$ifdef ISDELPHI}
    result := 0; // avoid hint
    {$endif}
    ErrorOverflow;
  end;
  c := s^ shl 28;
  inc(s);
  result := result {%H-}and $FFFFFFF or c;
f:P := pointer(s);
end;

procedure TFastReader.VarNextInt;
var
  s, l: PAnsiChar;
begin
  s := P;
  l := Last;
  repeat
    if s >= l then
      break;  // reached end of input
    if s^ <= #$7f then
      break; // reached end of VarUInt32/VarUInt64
    inc(s);
  until false;
  P := s + 1;
end;

procedure TFastReader.VarNextInt(count: integer);
var
  s, l: PAnsiChar;
begin
  if count = 0 then
    exit;
  s := P;
  l := Last;
  repeat
    if s >= l then
      break;  // reached end of input
    if s^ > #$7f then
    begin
      inc(s);
      continue; // didn't reach end of VarUInt32/VarUInt64
    end;
    inc(s);
    dec(count);
    if count = 0 then
      break;
  until false;
  P := s;
end;

{$endif CPUX86}

function TFastReader.PeekVarInt32(out value: PtrInt): boolean;
begin
  result := PeekVarUInt32(PtrUInt(value));
  if result then
    if value and 1 <> 0 then
      // 1->1, 3->2..
      value := value shr 1 + 1
    else      // 0->0, 2->-1, 4->-2..
      value := -(value shr 1);
end;

function TFastReader.PeekVarUInt32(out value: PtrUInt): boolean;
var
  s: PAnsiChar;
begin
  result := false;
  s := P;
  repeat
    if s >= Last then
      exit  // reached end of input -> returns false
    else if s^ <= #$7f then
      break; // reached end of VarUInt32
    inc(s);
  until false;
  s := P;
  value := VarUInt32; // fast value decode
  P := s; // rewind
  result := true;
end;

function TFastReader.VarUInt32Safe(out Value: cardinal): boolean;
var
  c, n, v: cardinal;
begin
  result := false;
  if P >= Last then
    exit;
  v := ord(P^);
  inc(P);
  if v > $7f then
  begin
    n := 0;
    v := v and $7F;
    repeat
      if P >= Last then
        exit;
      c := ord(P^);
      inc(P);
      inc(n, 7);
      if c <= $7f then
        break;
      v := v or ((c and $7f) shl n);
    until false;
    v := v or (c shl n);
  end;
  Value := v;
  result := true; // success
end;

function TFastReader.VarUInt64: QWord;
label
  e;
var
  c, n: PtrUInt;
begin
  if P >= Last then
e:  ErrorOverflow;
  c := ord(P^);
  inc(P);
  if c > $7f then
  begin
    result := c and $7F;
    n := 0;
    repeat
      if P >= Last then
        goto e;
      c := ord(P^);
      inc(P);
      inc(n, 7);
      if c <= $7f then
        break;
      result := result or (QWord(c and $7f) shl n);
    until false;
    result := result or (QWord(c) shl n);
  end
  else
    result := c;
end;

procedure TFastReader.VarBlob(out result: TValueResult);
var
  len: PtrUInt;
begin
  len := VarUInt32;
  if P + len > Last then
    ErrorOverflow;
  result.Ptr := P;
  result.Len := len;
  inc(P, len);
end;

procedure TFastReader.VarBlob(out Value: TSynTempBuffer);
var
  len: PtrUInt;
begin
  len := VarUInt32;
  if P + len > Last then
    ErrorOverflow;
  Value.Init(P, len);
  inc(P, len);
end;

function TFastReader.VarBlob: TValueResult;
var
  len: PtrUInt;
label
  e;
{%H-}begin
  if P >= Last then
    goto e;
  len := ord(P^);
  if len < $80 then
    inc(P)
  else
    len := VarUInt32;
  result.Ptr := P;
  result.Len := len;
  if P + len <= Last then
    inc(P, len)
  else
e:  ErrorOverflow;
end;

function TFastReader.VarString: RawByteString;
begin
  with VarBlob do
    SetString(result, Ptr, Len);
end;

function TFastReader.VarString(CodePage: integer): RawByteString;
begin
  with VarBlob do
    FastSetStringCP(result, Ptr, Len, CodePage)
end;

procedure TFastReader.VarUtf8(out result: RawUtf8);
var
  len: PtrUInt;
label
  e;
begin
  if P >= Last then
    goto e;
  len := ord(P^);
  if len < $80 then
    inc(P)
  else
    len := VarUInt32;
  if P + len <= Last then
  begin
    FastSetString(result, P, len);
    inc(P, len);
  end
  else
e:  ErrorOverflow;
end;

function TFastReader.VarUtf8: RawUtf8;
begin
  VarUtf8(result);
end;

function TFastReader.VarShortString: shortstring;
var
  len: cardinal;
  s: PAnsiChar;
label
  e, r;
{%H-}begin
  s := P;
  if s >= Last then
    goto e;
  len := ord(s^);
  if len <= $7f then
  begin
    inc(s);
r:  P := s;
    inc(s, len);
    if s >= Last then
      goto e;
    result[0] := AnsiChar(len);
    MoveFast(P^, result[1], len);
    P := s;
    exit;
  end;
  len := (len and $7F) or (ord(s^) shl 7); // 2nd byte of VarUInt32 decoding
  inc(s);
  if len <= 255 then
    goto r;
e:ErrorOverflow;
end;

function TFastReader.VarUtf8Safe(out Value: RawUtf8): boolean;
var
  len: cardinal;
begin
  if VarUInt32Safe(len) then
    if len = 0 then
      result := true
    else if P + len <= Last then
    begin
      FastSetString(Value, P, len);
      inc(P, len);
      result := true;
    end
    else
      result := false
  else
    result := false;
end;

function CleverReadInteger(p, e: PAnsiChar; V: PInteger): PtrUInt;
// Clever = decode Values[i+1]-Values[i] storage (with special diff=1 count)
var
  i, n: PtrUInt;
begin
  result := PtrUInt(V);
  i := PInteger(p)^;
  inc(p, 4); // integer: firstValue
  V^ := i;
  inc(V);
  if PtrUInt(p) < PtrUInt(e) then
    repeat
      case p^ of
        #0:
          begin
            // B:0 W:difference with previous
            inc(i, PWord(p + 1)^);
            inc(p, 3);
            V^ := i;
            inc(V);
            if PtrUInt(p) < PtrUInt(e) then
              continue
            else
              break;
          end;
        #254:
          begin
            // B:254 W:byOne
            for n := 1 to PWord(p + 1)^ do
            begin
              inc(i);
              V^ := i;
              inc(V);
            end;
            inc(p, 3);
            if PtrUInt(p) < PtrUInt(e) then
              continue
            else
              break;
          end;
        #255:
          begin
            // B:255 B:byOne
            for n := 1 to PByte(p + 1)^ do
            begin
              inc(i);
              V^ := i;
              inc(V);
            end;
            inc(p, 2);
            if PtrUInt(p) < PtrUInt(e) then
              continue
            else
              break;
          end
      else
        begin
          // B:1..253 = difference with previous
          inc(i, ord(p^));
          inc(p);
          V^ := i;
          inc(V);
          if PtrUInt(p) < PtrUInt(e) then
            continue
          else
            break;
        end;
      end; // case p^ of
    until false;
  result := (PtrUInt(V) - result) shr 2; // returns count of stored integers
end;

function TFastReader.ReadVarUInt32Array(var Values: TIntegerDynArray): PtrInt;
var
  i: PtrInt;
  k: TBufferWriterKind;
  pi: PInteger;
  n, diff: integer;
  chunk, chunkend: PtrUInt;
begin
  result := VarUInt32;
  if result = 0 then
    exit;
  if result > length(Values) then // only set length is not big enough
    SetLength(Values, result);
  k := TBufferWriterKind(NextByte);
  pi := pointer(Values);
  n := result;
  case k of
    wkUInt32:
      begin
        Copy(pointer(Values), n * 4);
        exit;
      end;
    wkOffsetU, wkOffsetI:
      begin
        pi^ := VarUInt32;
        dec(n);
        if n = 0 then
          exit;
        diff := VarUInt32;
        if diff <> 0 then
        begin
          // all items have a fixed offset
          for i := 0 to n - 1 do
            PIntegerArray(pi)[i + 1] := PIntegerArray(pi)[i] + diff;
          exit;
        end
      end;
    wkFakeMarker:
      begin
        // caller should make the decoding: notify by returning the count as <0
        result := -result;
        exit;
      end;
  end;
  repeat
    // chunked format: Isize+values
    chunkend := Next4;
    chunk := PtrUInt(Next(chunkend));
    inc(chunkend, chunk);
    case k of
      wkVarInt32:
        repeat
          pi^ := FromVarInt32(PByte(chunk));
          inc(pi);
          dec(n);
        until (n = 0) or
              (chunk >= chunkend);
      wkVarUInt32:
        repeat
          pi^ := FromVarUInt32Big(PByte(chunk));
          inc(pi);
          dec(n);
        until (n = 0) or
              (chunk >= chunkend);
      wkSorted:
        begin
          diff := CleverReadInteger(pointer(chunk), pointer(chunkend), pi);
          dec(n, diff);
          inc(pi, diff);
        end;
      wkOffsetU:
        repeat
          PIntegerArray(pi)[1] := pi^ + integer(FromVarUInt32(PByte(chunk)));
          inc(pi);
          dec(n);
        until (n = 0) or
              (chunk >= chunkend);
      wkOffsetI:
        repeat
          PIntegerArray(pi)[1] := pi^ + FromVarInt32(PByte(chunk));
          inc(pi);
          dec(n);
        until (n = 0) or
              (chunk >= chunkend);
    else
      ErrorData('ReadVarUInt32Array got kind=%', [ord(k)]);
    end;
  until n = 0;
end;

type
  TBufferWriterKind64 = (
    wkVarUInt64, wkOffset64);

function TFastReader.ReadVarUInt64Array(var Values: TInt64DynArray): PtrInt;
var
  i, n: PtrInt;
  k: TBufferWriterKind64;
  pi: PQWord;
  diff: QWord;
  chunk, chunkend: PtrUInt;
begin
  result := VarUInt32;
  if result = 0 then
    exit;
  if result > length(Values) then // only set length is not big enough
    SetLength(Values, result);
  k := TBufferWriterKind64(NextByte);
  pi := pointer(Values);
  n := result;
  if k = wkOffset64 then
  begin
    pi^ := VarUInt64;
    dec(n);
    diff := VarUInt32;
    if diff <> 0 then
    begin
      // all items have a fixed offset
      for i := 0 to n - 1 do
        PQwordArray(pi)[i + 1] := PQwordArray(pi)[i] + diff;
      exit;
    end
  end;
  repeat
    // chunked format: Isize+values
    chunkend := Next4;
    chunk := PtrUInt(Next(chunkend));
    inc(chunkend, chunk);
    case k of
      wkVarUInt64:
        repeat
          pi^ := FromVarUInt64(PByte(chunk));
          inc(pi);
          dec(n);
        until (n = 0) or
              (chunk >= chunkend);
      wkOffset64:
        repeat
          PQwordArray(pi)[1] := pi^ + FromVarUInt64(PByte(chunk));
          inc(pi);
          dec(n);
        until (n = 0) or
              (chunk >= chunkend);
      else
        ErrorData('ReadVarUInt64Array got kind=%', [ord(k)]);
    end;
  until n = 0;
end;

function TFastReader.ReadVarRawUtf8DynArray(var Values: TRawUtf8DynArray): PtrInt;
var
  count, len: integer;
  fixedsize, chunk, chunkend: PtrUInt;
  PI: PRawUtf8;
begin
  result := VarUInt32;
  if result = 0 then
    exit;
  count := result;
  if count > length(Values) then // change Values[] length only if not big enough
    SetLength(Values, count);
  PI := pointer(Values);
  fixedsize := VarUInt32;
  repeat
    // chunked format: Isize+values
    chunkend := Next4;
    chunk := PtrUInt(Next(chunkend));
    inc(chunkend, chunk);
    if fixedsize = 0 then
      // variable size strings
      while (count > 0) and
            (chunk < chunkend) do
      begin
        len := FromVarUInt32(PByte(chunk));
        if len > 0 then
        begin
          FastSetString(PI^, pointer(chunk), len);
          inc(chunk, len);
        end
        else if PI^<>'' then
          PI^ := '';
        dec(count);
        inc(PI);
      end
    else
      // fixed size strings
      while (count > 0) and
            (chunk < chunkend) do
      begin
        FastSetString(PI^, pointer(chunk), fixedsize);
        inc(chunk, fixedsize);
        dec(count);
        inc(PI);
      end;
  until count <= 0;
  if PI <> @Values[result] then
    ErrorOverflow; // paranoid check
end;

function TFastReader.ReadCompressed(Load: TAlgoCompressLoad;
  BufferOffset: integer): RawByteString;
var
  comp: PAnsiChar;
  complen: PtrUInt;
begin
  complen := VarUInt32;
  comp := Next(complen);
  TAlgoCompress.Algo(comp, complen).Decompress(
    comp, complen, result, Load, BufferOffset);
end;


{ TBufferWriter }

constructor TBufferWriter.Create(aFile: THandle; BufLen: integer);
begin
  Create(THandleStream.Create(aFile), BufLen);
  fInternalStream := true;
end;

constructor TBufferWriter.Create(const aFileName: TFileName;
  BufLen: integer; Append: boolean);
var
  s: TStream;
begin
  if Append and
     FileExists(aFileName) then
  begin
    s := TFileStream.Create(aFileName, fmOpenWrite);
    s.Seek(0, soEnd);
  end
  else
    s := TFileStream.Create(aFileName, fmCreate);
  Create(s, BufLen);
  fInternalStream := true;
end;

constructor TBufferWriter.Create(aStream: TStream; BufLen: integer);
begin
  if BufLen > 1 shl 22 then
    fBufLen := 1 shl 22 // 4 MB sounds right enough
  else if BufLen < 128 then
    raise ESynException.CreateUtf8('%.Create(BufLen=%)', [self, BufLen]);
  fBufLen := BufLen;
  fBufLen16 := fBufLen - 16;
  fStream := aStream;
  GetMem(fBufferInternal, fBufLen);
  fBuffer := fBufferInternal;
end;

constructor TBufferWriter.Create(aStream: TStream;
  aTempBuf: pointer; aTempLen: integer);
begin
  fBufLen := aTempLen;
  fBufLen16 := fBufLen - 16;
  fBuffer := aTempBuf;
  fStream := aStream;
end;

constructor TBufferWriter.Create(aClass: TStreamClass; BufLen: integer);
begin
  Create(aClass.Create, BufLen);
  fInternalStream := true;
end;

constructor TBufferWriter.Create(aClass: TStreamClass;
  aTempBuf: pointer; aTempLen: integer);
begin
  Create(aClass.Create, aTempBuf, aTempLen);
  fInternalStream := true;
end;

constructor TBufferWriter.Create(const aStackBuffer: TTextWriterStackBuffer);
begin
  Create(TRawByteStringStream, @aStackBuffer, SizeOf(aStackBuffer));
end;

destructor TBufferWriter.Destroy;
begin
  if fInternalStream then
    fStream.Free;
  if fBufferInternal <> nil then
    FreeMem(fBufferInternal);
  inherited;
end;

procedure TBufferWriter.InternalFlush;
begin
  if fPos > 0 then
  begin
    InternalWrite(fBuffer, fPos);
    fPos := 0;
  end;
end;

procedure TBufferWriter.InternalWrite(Data: pointer; DataLen: PtrInt);
begin
  inc(fTotalFlushed, DataLen);
  if fStream.InheritsFrom(TRawByteStringStream) and
     (fTotalFlushed > _STRMAXSIZE) then
    // Delphi strings have a 32-bit length so you should change your algorithm
    raise ESynException.CreateUtf8('%.Write: % overflow (%)',
      [self, fStream, KBNoSpace(fTotalFlushed)]);
  fStream.WriteBuffer(Data^, DataLen);
end;

function TBufferWriter.GetTotalWritten: Int64;
begin
  result := fTotalFlushed + fPos;
end;

function TBufferWriter.Flush: Int64;
begin
  if fPos > 0 then
    InternalFlush;
  result := GetTotalWritten;
  fTotalFlushed := 0;
end;

procedure TBufferWriter.CancelAll;
begin
  fTotalFlushed := 0;
  fPos := 0;
  if fStream.ClassType = TRawByteStringStream then
    TRawByteStringStream(fStream).Size := 0
  else
    fStream.Seek(0, soBeginning);
end;

procedure TBufferWriter.FlushAndWrite(Data: pointer; DataLen: PtrInt);
begin
  if DataLen < 0 then
    exit;
  if fPos > 0 then
    InternalFlush;
  if DataLen > fBufLen then
    InternalWrite(Data, DataLen)
  else
  begin
    MoveFast(Data^, fBuffer^[fPos], DataLen);
    inc(fPos, DataLen);
  end;
end;

procedure TBufferWriter.Write(Data: pointer; DataLen: PtrInt);
var
  p: PtrUInt;
begin
  p := fPos;
  if p + PtrUInt(DataLen) <= PtrUInt(fBufLen) then
  begin
    MoveFast(Data^, fBuffer^[p], DataLen);
    inc(fPos, DataLen);
  end
  else
    FlushAndWrite(Data, DataLen); // will also handle DataLen<0
end;

procedure TBufferWriter.WriteN(Data: byte; Count: integer);
var
  len: integer;
begin
  while Count > 0 do
  begin
    if Count > fBufLen then
      len := fBufLen
    else
      len := Count;
    if fPos + len > fBufLen then
      InternalFlush;
    FillCharFast(fBuffer^[fPos], len, Data);
    inc(fPos, len);
    dec(Count, len);
  end;
end;

procedure TBufferWriter.Write1(Data: byte);
begin
  if fPos > fBufLen16 then
    InternalFlush;
  fBuffer^[fPos] := Data;
  inc(fPos);
end;

procedure TBufferWriter.Write2(Data: cardinal);
begin
  if fPos > fBufLen16 then
    InternalFlush;
  PWord(@fBuffer^[fPos])^ := Data;
  inc(fPos, SizeOf(Word));
end;

procedure TBufferWriter.Write4(Data: integer);
begin
  if fPos > fBufLen16 then
    InternalFlush;
  PInteger(@fBuffer^[fPos])^ := Data;
  inc(fPos, SizeOf(integer));
end;

procedure TBufferWriter.Write4BigEndian(Data: integer);
begin
  Write4(bswap32(Data));
end;

procedure TBufferWriter.Write8(Data8Bytes: pointer);
begin
  if fPos > fBufLen16 then
    InternalFlush;
  PInt64(@fBuffer^[fPos])^ := PInt64(Data8Bytes)^;
  inc(fPos, SizeOf(Int64));
end;

procedure TBufferWriter.WriteI64(Data: Int64);
begin
  if fPos > fBufLen16 then
    InternalFlush;
  PInt64(@fBuffer^[fPos])^ := Data;
  inc(fPos, SizeOf(Data));
end;

procedure TBufferWriter.WriteVar(Data: pointer; DataLen: PtrInt);
label
  wr;
begin
  if fPos + DataLen <= fBufLen16 then // could fit in buffer (most common case)
  begin
    if DataLen < $80 then // e.g. small strings
    begin
      fBuffer^[fPos] := DataLen;
      inc(fPos);
      if DataLen = 0 then
        exit;
wr:   MoveFast(Data^, fBuffer^[fPos], DataLen);
      inc(fPos, DataLen);
      exit;
    end;
    fPos := PtrUInt(ToVarUInt32(DataLen, @fBuffer^[fPos])) - PtrUInt(fBuffer);
    goto wr;
  end;
  // Data wouldn't fit in memory buffer -> write as two explicit calls
  WriteVarUInt32(DataLen);
  Write(Data, DataLen);
end;

procedure TBufferWriter.WriteVar(var Item: TTempUtf8);
begin
  WriteVar(Item.Text, Item.Len);
  if Item.TempRawUtf8 <> nil then
    {$ifdef FPC}
    FastAssignNew(Item.TempRawUtf8);
    {$else}
    RawUtf8(Item.TempRawUtf8) := '';
    {$endif FPC}
end;

procedure TBufferWriter.Write(const Text: RawByteString);
begin
  WriteVar(pointer(Text), length(Text));
end;

procedure TBufferWriter.WriteShort(const Text: ShortString);
begin
  WriteVar(@Text[1], ord(Text[0]));
end;

procedure TBufferWriter.WriteBinary(const Data: RawByteString);
begin
  Write(pointer(Data), Length(Data));
end;

function TBufferWriter.DirectWritePrepare(len: PtrInt;
  var tmp: RawByteString): PAnsiChar;
begin
  if (len <= fBufLen) and
     (fPos + len > fBufLen) then
    InternalFlush;
  if fPos + len > fBufLen then
  begin
    if len > length(tmp) then
      SetString(tmp, nil, len); // don't reallocate buffer, but reuse big enough
    result := pointer(tmp);
  end
  else
    result := @fBuffer^[fPos]; // write directly into the buffer
end;

procedure TBufferWriter.DirectWriteFlush(len: PtrInt; const tmp: RawByteString);
begin
  if tmp = '' then
    inc(fPos, len)
  else
    Write(pointer(tmp), len);
end;

procedure TBufferWriter.WriteXor(New, Old: PAnsiChar; Len: PtrInt;
  crc: PCardinal);
var
  L: integer;
  Dest: PAnsiChar;
begin
  if (New = nil) or
     (Old = nil) then
    exit;
  while Len > 0 do
  begin
    Dest := pointer(fBuffer);
    if fPos + Len > fBufLen then
      InternalFlush
    else
      inc(Dest, fPos);
    if Len > fBufLen then
      L := fBufLen
    else
      L := Len;
    XorMemory(pointer(Dest), pointer(New), pointer(Old), L);
    if crc <> nil then
      crc^ := crc32c(crc^, Dest, L);
    inc(Old, L);
    inc(New, L);
    dec(Len, L);
    inc(fPos, L);
  end;
end;

procedure TBufferWriter.WriteRawUtf8DynArray(const Values: TRawUtf8DynArray;
  ValuesCount: integer);
begin
  WriteRawUtf8Array(pointer(Values), ValuesCount);
end;

procedure TBufferWriter.WriteRawUtf8Array(Values: PPtrUIntArray;
  ValuesCount: integer);
var
  n, i: integer;
  fixedsize, len: PtrUInt;
  P, PEnd: PByte;
  PBeg: PAnsiChar;
begin
  WriteVarUInt32(ValuesCount);
  if ValuesCount = 0 then
    exit;
  fixedsize := Values^[0];
  if fixedsize <> 0 then
  begin
    fixedsize := {%H-}PStrLen(fixedsize - _STRLEN)^;
    for i := 1 to ValuesCount - 1 do
      if (Values^[i] = 0) or
         ({%H-}PStrLen(Values^[i] - _STRLEN)^ <> TStrLen(fixedsize)) then
      begin
        fixedsize := 0;
        break;
      end;
  end;
  WriteVarUInt32(fixedsize);
  repeat
    P := @fBuffer^[fPos];
    PEnd := @fBuffer^[fBufLen16];
    if PtrUInt(P) < PtrUInt(PEnd) then
    begin
      n := ValuesCount;
      PBeg := PAnsiChar(P); // leave space for chunk size
      inc(P, 4);
      if fixedsize = 0 then
        for i := 0 to ValuesCount - 1 do
          if Values^[i] = 0 then
          begin
            P^ := 0; // store length=0
            inc(P);
            if PtrUInt(P) >= PtrUInt(PEnd) then
            begin
              n := i + 1;
              break; // avoid buffer overflow
            end;
          end
          else
          begin
            len := {%H-}PStrLen(Values^[i] - _STRLEN)^;
            if PtrUInt(PEnd) - PtrUInt(P) <= len then
            begin
              n := i;
              break; // avoid buffer overflow
            end;
            P := ToVarUInt32(len, P);
            MoveFast(pointer(Values^[i])^, P^, len); // here len>0
            inc(P, len);
          end
      else // fixedsize<>0:
        for i := 0 to ValuesCount - 1 do
        begin
          if PtrUInt(PEnd) - PtrUInt(P) <= fixedsize then
          begin
            n := i;
            break; // avoid buffer overflow
          end;
          MoveFast(pointer(Values^[i])^, P^, fixedsize);
          inc(P, fixedsize);
        end;
      len := PAnsiChar(P) - PBeg; // format: Isize+varUInt32s*strings
      PInteger(PBeg)^ := len - 4;
      inc(fPos, len);
      inc(PByte(Values), n * SizeOf(PtrInt));
      dec(ValuesCount, n);
      if ValuesCount = 0 then
        break;
    end;
    InternalFlush;
  until false;
end;

procedure TBufferWriter.WriteStream(aStream: TCustomMemoryStream;
  aStreamSize: integer);
begin
  if aStreamSize < 0 then
    if aStream = nil then
      aStreamSize := 0
    else
      aStreamSize := aStream.Size;
  WriteVarUInt32(aStreamSize);
  if aStreamSize > 0 then
    Write(aStream.Memory, aStreamSize);
end;

procedure TBufferWriter.WriteVarInt32(Value: PtrInt);
begin
  if Value <= 0 then
    // 0->0, -1->2, -2->4..
    Value := (-Value) shl 1
  else    // 1->1, 2->3..
    Value := (Value shl 1) - 1;
  WriteVarUInt32(Value);
end;

procedure TBufferWriter.WriteVarUInt32(Value: PtrUInt);
begin
  if fPos > fBufLen16 then
    InternalFlush;
  fPos := PtrUInt(ToVarUInt32(Value, @fBuffer^[fPos])) - PtrUInt(fBuffer);
end;

procedure TBufferWriter.WriteVarInt64(Value: Int64);
begin
  if fPos > fBufLen16 then
    InternalFlush;
  fPos := PtrUInt(ToVarInt64(Value, @fBuffer^[fPos])) - PtrUInt(fBuffer);
end;

procedure TBufferWriter.WriteVarUInt64(Value: QWord);
begin
  if fPos > fBufLen16 then
    InternalFlush;
  fPos := PtrUInt(ToVarUInt64(Value, @fBuffer^[fPos])) - PtrUInt(fBuffer);
end;

function CleverStoreInteger(p: PInteger; V, VEnd: PAnsiChar; pCount: integer;
  var StoredCount: integer): PAnsiChar;
// Clever = store Values[i+1]-Values[i] (with special diff=1 count)
// format:  integer: firstValue, then:
//          B:0 W:difference with previous
//          B:1..253 = difference with previous
//          B:254 W:byOne
//          B:255 B:byOne
var
  i, d, byOne: integer;
begin
  StoredCount := pCount;
  if pCount <= 0 then
  begin
    result := V;
    exit;
  end;
  i := p^;
  PInteger(V)^ := p^;
  inc(V, 4);
  dec(pCount);
  inc(p);
  byOne := 0;
  if pCount > 0 then
    repeat
      d := p^ - i;
      i := p^;
      inc(p);
      if d = 1 then
      begin
        dec(pCount);
        inc(byOne);
        if pCount > 0 then
          continue;
      end
      else if d < 0 then
      begin
        result := nil;
        exit;
      end;
      if byOne <> 0 then
      begin
        case byOne of
          1:
            begin
              V^ := #1;
              inc(V);
            end; // B:1..253 = difference with previous
          2:
            begin
              PWord(V)^ := $0101;
              inc(V, 2);
            end; // B:1..253 = difference
        else
          if byOne > 255 then
          begin
            while byOne > 65535 do
            begin
              PInteger(V)^ := $fffffe;
              inc(V, 3); // store as many len=$ffff as necessary
              dec(byOne, $ffff);
            end;
            PInteger(V)^ := byOne shl 8 + $fe;
            inc(V, 3); // B:254 W:byOne
          end
          else
          begin
            PWord(V)^ := byOne shl 8 + $ff;
            inc(V, 2); // B:255 B:byOne
          end;
        end; // case byOne of
        if pCount = 0 then
          break;
        byOne := 0;
      end;
      if (d = 0) or
         (d > 253) then
      begin
        while cardinal(d) > 65535 do
        begin
          PInteger(V)^ := $ffff00;
          inc(V, 3); // store as many len=$ffff as necessary
          dec(cardinal(d), $ffff);
        end;
        dec(pCount);
        PInteger(V)^ := d shl 8;
        inc(V, 3); // B:0 W:difference with previous
        if (V < VEnd) and
           (pCount > 0) then
          continue
        else
          break;
      end
      else
      begin
        dec(pCount);
        V^ := AnsiChar(d);
        inc(V); // B:1..253 = difference with previous
        if (V < VEnd) and
           (pCount > 0) then
          continue
        else
          break;
      end;
      if V >= VEnd then
        break; // avoid GPF
    until false;
  dec(StoredCount, pCount);
  result := V;
end;

procedure TBufferWriter.WriteVarUInt32Array(const Values: TIntegerDynArray;
  ValuesCount: integer; DataLayout: TBufferWriterKind);
begin
  WriteVarUInt32Values(pointer(Values), ValuesCount, DataLayout);
end;

procedure TBufferWriter.WriteVarUInt32Values(Values: PIntegerArray;
  ValuesCount: integer; DataLayout: TBufferWriterKind);
var
  diff, v, vp, n: integer;
  i: PtrInt;
  P: PByte;
  PBeg, PEnd: PAnsiChar;
begin
  WriteVarUInt32(ValuesCount);
  if ValuesCount = 0 then
    exit;
  fBuffer^[fPos] := ord(DataLayout);
  inc(fPos);
  vp := Values^[0];
  if DataLayout in [wkOffsetU, wkOffsetI] then
  begin
    fPos := PtrUInt(ToVarUInt32(vp, @fBuffer^[fPos])) - PtrUInt(fBuffer);
    diff := Values^[1] - vp;
    inc(PInteger(Values));
    dec(ValuesCount);
    if ValuesCount = 0 then
      exit;
    if diff > 0 then
    begin
      for i := 1 to ValuesCount - 1 do
        if Values^[i] - Values^[i - 1] <> diff then
        begin
          diff := 0; // not always the same offset
          break;
        end;
    end
    else
      diff := 0;
    fPos := PtrUInt(ToVarUInt32(diff, @fBuffer^[fPos])) - PtrUInt(fBuffer);
    if diff <> 0 then
      exit; // same offset for all items (fixed sized records) -> quit now
  end;
  repeat
    P := @fBuffer^[fPos];
    PEnd := @fBuffer^[fBufLen16];
    if PtrUInt(P) < PtrUInt(PEnd) then
    begin
      case DataLayout of
        wkUInt32:
          // format: uncompressed array of cardinals
          begin
            n := (fBufLen - fPos) shr 2;
            if ValuesCount < n then
              n := ValuesCount;
            MoveFast(Values^, P^, n * 4);
            inc(P, n * 4);
          end;
        wkVarInt32, wkVarUInt32, wkOffsetU, wkOffsetI:
          begin
            // format: Isize + varUInt32s
            PBeg := PAnsiChar(P); // leave space for chunk size
            inc(P, 4);
            n := ValuesCount;
            for i := 0 to ValuesCount - 1 do
            begin
              v := Values^[i];
              case DataLayout of
                wkVarInt32:
                  P := ToVarInt32(v, P);
                wkVarUInt32:
                  P := ToVarUInt32(v, P);
                wkOffsetU:
                  P := ToVarUInt32(v - vp, P);
                wkOffsetI:
                  P := ToVarInt32(v - vp, P);
              end;
              vp := v;
              if PtrUInt(P) >= PtrUInt(PEnd) then
              begin
                n := i + 1;
                break; // avoid buffer overflow
              end;
            end;
            PInteger(PBeg)^ := PAnsiChar(P) - PBeg - 4;
          end;
        wkSorted:
          begin
            // format: Isize + cleverStorage
            PBeg := PAnsiChar(P) + 4; // leave space for chunk size
            P := PByte(CleverStoreInteger(pointer(Values), PBeg, PEnd, ValuesCount, n));
            if P = nil then
              raise ESynException.CreateUtf8(
                '%.WriteVarUInt32Array: data not sorted', [self]);
            PInteger(PBeg - 4)^ := PAnsiChar(P) - PBeg;
          end;
      end;
      inc(PByte(Values), n * 4);
      fPos := PtrUInt(P) - PtrUInt(fBuffer);
      dec(ValuesCount, n);
      if ValuesCount = 0 then
        break;
    end;
    InternalFlush;
  until false;
end;

procedure TBufferWriter.WriteVarUInt64DynArray(const Values: TInt64DynArray;
  ValuesCount: integer; Offset: boolean);
var
  n: integer;
  i: PtrInt;
  diff: Int64;
  P, PEnd: PByte;
  PI: PInt64Array;
  PBeg: PAnsiChar;
begin
  WriteVarUInt32(ValuesCount);
  if ValuesCount = 0 then
    exit;
  PI := pointer(Values);
  if Offset then
  begin
    fBuffer^[fPos] := ord(wkOffset64);
    fPos := PtrUInt(ToVarUInt64(PI^[0], @fBuffer^[fPos + 1])) - PtrUInt(fBuffer);
    diff := PI^[1] - PI^[0];
    inc(PByte(PI), 8);
    dec(ValuesCount);
    if ValuesCount = 0 then
      exit;
    if (diff > 0) and
       (diff < MaxInt) then
    begin
      for i := 1 to ValuesCount - 1 do
        if PI^[i] - PI^[i - 1] <> diff then
        begin
          diff := 0; // not always the same offset
          break;
        end;
    end
    else
      diff := 0;
    fPos := PtrUInt(ToVarUInt32(diff, @fBuffer^[fPos])) - PtrUInt(fBuffer);
    if diff <> 0 then
      exit; // same offset for all items (fixed sized records) -> quit now
  end
  else
  begin
    fBuffer^[fPos] := ord(wkVarUInt64);
    inc(fPos);
  end;
  repeat
    P := @fBuffer^[fPos];
    PEnd := @fBuffer^[fBufLen16];
    if PtrUInt(P) < PtrUInt(PEnd) then
    begin
      PBeg := PAnsiChar(P); // leave space for chunk size
      inc(P, 4);
      n := ValuesCount;
      for i := 0 to ValuesCount - 1 do
      begin
        if Offset then
          P := ToVarUInt64(PI^[i] - PI^[i - 1], P) // store diffs
        else
          P := ToVarUInt64(PI^[i], P);
        if PtrUInt(P) >= PtrUInt(PEnd) then
        begin
          n := i + 1;
          break; // avoid buffer overflow
        end;
      end;
      PInteger(PBeg)^ := PAnsiChar(P) - PBeg - 4; // format: Isize+varUInt32/64s
      inc(PByte(PI), n * 8);
      fPos := PtrUInt(P) - PtrUInt(fBuffer);
      dec(ValuesCount, n);
      if ValuesCount = 0 then
        break;
    end;
    InternalFlush;
  until false;
end;

function TBufferWriter.FlushTo: RawByteString;
begin
  InternalFlush;
  result := (fStream as TRawByteStringStream).DataString;
end;

function TBufferWriter.FlushToBytes: TBytes;
var
  siz: Int64;
begin
  result := nil;
  siz := GetTotalWritten;
  if siz > _DAMAXSIZE then
    raise ESynException.CreateUtf8('%.FlushToBytes: overflow (%)', [KB(siz)]);
  SetLength(result, siz);
  if fStream.Position = 0 then
    // direct assignment from internal buffer
    MoveFast(fBuffer[0], pointer(result)^, fPos)
  else
  begin
    // from temporary allocation in TRawByteStringStream.DataString
    Flush;
    MoveFast(pointer((fStream as TRawByteStringStream).DataString)^,
      pointer(result)^, TotalWritten);
  end;
end;

function TBufferWriter.FlushAndCompress(nocompression: boolean;
  algo: TAlgoCompress; BufferOffset: integer): RawByteString;
var
  trig: integer;
begin
  if algo = nil then
    algo := AlgoSynLZ;
  trig := SYNLZTRIG[nocompression];
  if fStream.Position = 0 then
    // direct compression from internal buffer
    result := algo.Compress(PAnsiChar(fBuffer), fPos, trig, false, BufferOffset)
  else
    // from temporary allocation in TRawByteStringStream.DataString
    result := algo.Compress(FlushTo, trig, false, BufferOffset);
end;



{ ************ TAlgoCompress Compression/Decompression Classes }

{ TAlgoCompress }

const
  COMPRESS_STORED = #0;
  COMPRESS_SYNLZ = 1;

var
  // don't use TObjectList before mormot.core.json registered TRttiJson
  SynCompressAlgos: array of TAlgoCompress;

constructor TAlgoCompress.Create;
var
  existing: TAlgoCompress;
begin
  existing := Algo(fAlgoID);
  if existing <> nil then
    raise EAlgoCompress.CreateUtf8('%.Create: AlgoID=% already registered by %',
      [self, fAlgoID, existing]);
  ObjArrayAdd(SynCompressAlgos, self);
end;

class function TAlgoCompress.Algo(const Comp: RawByteString): TAlgoCompress;
begin
  result := Algo(Pointer(Comp), Length(Comp));
end;

class function TAlgoCompress.Algo(const Comp: TByteDynArray): TAlgoCompress;
begin
  result := Algo(Pointer(Comp), Length(Comp));
end;

class function TAlgoCompress.Algo(Comp: PAnsiChar; CompLen: integer): TAlgoCompress;
begin
  if (Comp <> nil) and
     (CompLen > 9) then
    if ord(Comp[4]) <= 1 then // inline-friendly Comp[4]<=COMPRESS_SYNLZ
      result := AlgoSynLZ
    else // COMPRESS_STORED is also handled as SynLZ
      result := Algo(ord(Comp[4]))
  else
    result := nil;
end;

class function TAlgoCompress.Algo(Comp: PAnsiChar; CompLen: integer;
  out IsStored: boolean): TAlgoCompress;
begin
  if (Comp <> nil) and
     (CompLen > 9) then
  begin
    IsStored := Comp[4] = COMPRESS_STORED;
    result := Algo(ord(Comp[4]));
  end
  else
  begin
    IsStored := false;
    result := nil;
  end;
end;

class function TAlgoCompress.Algo(aAlgoID: byte): TAlgoCompress;
var
  n: integer;
  ptr: ^TAlgoCompress;
begin
  if aAlgoID <= COMPRESS_SYNLZ then // COMPRESS_STORED is handled as SynLZ
    result := AlgoSynLZ
  else
  begin
    ptr := pointer(SynCompressAlgos);
    if ptr <> nil then
    begin
      n := PDALen(PAnsiChar(ptr) - _DALEN)^ + ( _DAOFF - 1 ); // - 1 for List[0]
      if n > 0 then
        repeat
          inc(ptr); // ignore List[0] = AlgoSynLZ
          result := ptr^;
          if result.fAlgoID = aAlgoID then
            exit;
          dec(n);
        until n = 0;
    end;
    result := nil;
  end;
end;

class function TAlgoCompress.UncompressedSize(const Comp: RawByteString): integer;
begin
  result := Algo(Comp).DecompressHeader(pointer(Comp), length(Comp));
end;

function TAlgoCompress.AlgoName: TShort16;
var
  s: PShortString;
  i: integer;
begin
  if self = nil then
    result := 'none'
  else
  begin
    s := ClassNameShort(self);
    if IdemPChar(@s^[1], 'TALGO') then
    begin
      result[0] := AnsiChar(ord(s^[0]) - 5);
      inc(PByte(s), 5);
    end
    else
      result[0] := s^[0];
    if result[0] > #16 then
      result[0] := #16;
    for i := 1 to ord(result[0]) do
      result[i] := NormToLower[s^[i]];
  end;
end;

function TAlgoCompress.AlgoHash(Previous: cardinal;
  Data: pointer; DataLen: integer): cardinal;
begin
  result := crc32c(Previous, Data, DataLen);
end;

function TAlgoCompress.AlgoHash(ForceHash32: boolean;
  Data: pointer; DataLen: integer): cardinal;
begin
  if ForceHash32 then
    result := Hash32(Data, DataLen)
  else
    result := AlgoHash(0, Data, DataLen);
end;

function TAlgoCompress.Compress(const Plain: RawByteString;
  CompressionSizeTrigger: integer; CheckMagicForCompressed: boolean;
  BufferOffset: integer): RawByteString;
begin
  result := Compress(pointer(Plain), Length(Plain), CompressionSizeTrigger,
    CheckMagicForCompressed, BufferOffset);
end;

function TAlgoCompress.Compress(Plain: PAnsiChar; PlainLen: integer;
  CompressionSizeTrigger: integer; CheckMagicForCompressed: boolean;
  BufferOffset: integer): RawByteString;
var
  len: integer;
  R: PAnsiChar;
  crc: cardinal;
  tmp: array[0..16383] of AnsiChar;  // big enough to resize result in-place
begin
  if (self = nil) or
     (PlainLen = 0) or
     (Plain = nil) then
  begin
    result := '';
    exit;
  end;
  crc := AlgoHash(0, Plain, PlainLen);
  if (PlainLen < CompressionSizeTrigger) or
     (CheckMagicForCompressed and
      IsContentCompressed(Plain, PlainLen)) then
  begin
    SetString(result, nil, PlainLen + BufferOffset + 9);
    R := pointer(result);
    inc(R, BufferOffset);
    PCardinal(R)^ := crc;
    R[4] := COMPRESS_STORED;
    PCardinal(R + 5)^ := crc;
    MoveFast(Plain^, R[9], PlainLen);
  end
  else
  begin
    len := CompressDestLen(PlainLen) + BufferOffset;
    if len > SizeOf(tmp) then
    begin
      SetString(result, nil, len);
      R := pointer(result);
    end
    else
      R := @tmp;
    inc(R, BufferOffset);
    PCardinal(R)^ := crc;
    len := AlgoCompress(Plain, PlainLen, R + 9);
    if len + 64 >= PlainLen then
    begin
      // store if compression was not worth it
      R[4] := COMPRESS_STORED;
      PCardinal(R + 5)^ := crc;
      MoveFast(Plain^, R[9], PlainLen);
      len := PlainLen;
    end
    else
    begin
      R[4] := AnsiChar(AlgoID);
      PCardinal(R + 5)^ := AlgoHash(0, R + 9, len);
    end;
    inc(len, BufferOffset + 9);
    if R = @tmp[BufferOffset] then
      SetString(result, PAnsiChar(@tmp), len)
    else
      if result {%H-}<> '' then
        // don't call the MM which may move the data: just adjust length()
        PStrLen(R - _STRLEN)^ := len;
  end;
end;

function TAlgoCompress.Compress(Plain, Comp: PAnsiChar; PlainLen, CompLen: integer;
  CompressionSizeTrigger: integer; CheckMagicForCompressed: boolean): integer;
var
  len: integer;
begin
  result := 0;
  if (self = nil) or
     (PlainLen = 0) or
     (CompLen < PlainLen + 9) then
    exit;
  PCardinal(Comp)^ := AlgoHash(0, Plain, PlainLen);
  if (PlainLen >= CompressionSizeTrigger) and
     not (CheckMagicForCompressed and
          IsContentCompressed(Plain, PlainLen)) then
  begin
    len := CompressDestLen(PlainLen);
    if CompLen < len then
      exit;
    len := AlgoCompress(Plain, PlainLen, Comp + 9);
    if len < PlainLen then
    begin
      Comp[4] := AnsiChar(AlgoID);
      PCardinal(Comp + 5)^ := AlgoHash(0, Comp + 9, len);
      result := len + 9;
      exit;
    end;
  end;
  Comp[4] := COMPRESS_STORED;
  PCardinal(Comp + 5)^ := PCardinal(Comp)^;
  MoveFast(Plain^, Comp[9], PlainLen);
  result := PlainLen + 9;
end;

function TAlgoCompress.CompressDestLen(PlainLen: integer): integer;
begin
  if self = nil then
    result := 0
  else
    result := AlgoCompressDestLen(PlainLen) + 9;
end;

function TAlgoCompress.CompressToBytes(Plain: PAnsiChar; PlainLen: integer;
  CompressionSizeTrigger: integer; CheckMagicForCompressed: boolean): TByteDynArray;
var
  len: integer;
  R: PAnsiChar;
  crc: cardinal;
begin
  Finalize(result);
  if (self = nil) or
     (PlainLen = 0) then
    exit;
  crc := AlgoHash(0, Plain, PlainLen);
  if PlainLen < CompressionSizeTrigger then
  begin
    SetLength(result, PlainLen + 9);
    R := pointer(result);
    PCardinal(R)^ := crc;
    R[4] := COMPRESS_STORED;
    PCardinal(R + 5)^ := crc;
    MoveFast(Plain^, R[9], PlainLen);
  end
  else
  begin
    SetLength(result, CompressDestLen(PlainLen));
    R := pointer(result);
    PCardinal(R)^ := crc;
    len := AlgoCompress(Plain, PlainLen, R + 9);
    if len >= PlainLen then
    begin
      // store if compression not worth it
      R[4] := COMPRESS_STORED;
      PCardinal(R + 5)^ := crc;
      MoveFast(Plain^, R[9], PlainLen);
      len := PlainLen;
    end
    else
    begin
      R[4] := AnsiChar(AlgoID);
      PCardinal(R + 5)^ := AlgoHash(0, R + 9, len);
    end;
    SetLength(result, len + 9);
  end;
end;

function TAlgoCompress.CompressToBytes(const Plain: RawByteString;
  CompressionSizeTrigger: integer; CheckMagicForCompressed: boolean): TByteDynArray;
begin
  result := CompressToBytes(pointer(Plain), Length(Plain),
    CompressionSizeTrigger, CheckMagicForCompressed);
end;

function TAlgoCompress.Decompress(const Comp: TByteDynArray): RawByteString;
begin
  Decompress(pointer(Comp), length(Comp), result);
end;

procedure TAlgoCompress.Decompress(Comp: PAnsiChar; CompLen: integer;
  out result: RawByteString; Load: TAlgoCompressLoad; BufferOffset: integer);
var
  len: integer;
  dec: PAnsiChar;
begin
  len := DecompressHeader(Comp, CompLen, Load);
  if len = 0 then
    exit;
  SetString(result, nil, len + BufferOffset);
  dec := pointer(result);
  if not DecompressBody(Comp, dec + BufferOffset, CompLen, len, Load) then
    result := '';
end;

function TAlgoCompress.Decompress(const Comp: RawByteString; Load: TAlgoCompressLoad;
  BufferOffset: integer): RawByteString;
begin
  Decompress(pointer(Comp), length(Comp), result, Load, BufferOffset);
end;

function TAlgoCompress.TryDecompress(const Comp: RawByteString;
  out Dest: RawByteString; Load: TAlgoCompressLoad): boolean;
var
  len: integer;
begin
  result := Comp = '';
  if result then
    exit;
  len := DecompressHeader(pointer(Comp), length(Comp), Load);
  if len = 0 then
    exit; // invalid crc32c
  SetString(Dest, nil, len);
  if DecompressBody(pointer(Comp), pointer(Dest), length(Comp), len, Load) then
    result := true
  else
    Dest := '';
end;

function TAlgoCompress.Decompress(const Comp: RawByteString;
  out PlainLen: integer; var tmp: RawByteString;
  Load: TAlgoCompressLoad): pointer;
begin
  result := Decompress(pointer(Comp), length(Comp), PlainLen, tmp, Load);
end;

function TAlgoCompress.Decompress(Comp: PAnsiChar; CompLen: integer;
  out PlainLen: integer; var tmp: RawByteString; Load: TAlgoCompressLoad): pointer;
begin
  result := nil;
  PlainLen := DecompressHeader(Comp, CompLen, Load);
  if PlainLen = 0 then
    exit;
  if Comp[4] = COMPRESS_STORED then
    result := Comp + 9
  else
  begin
    if PlainLen > length(tmp) then
      SetString(tmp, nil, PlainLen);
    if DecompressBody(Comp, pointer(tmp), CompLen, PlainLen, Load) then
      result := pointer(tmp);
  end;
end;

function TAlgoCompress.DecompressPartial(Comp, Partial: PAnsiChar;
  CompLen, PartialLen, PartialLenMax: integer): integer;
var
  BodyLen: integer;
begin
  result := 0;
  if (self = nil) or
     (CompLen <= 9) or
     (Comp = nil) or
     (PartialLenMax < PartialLen) then
    exit;
  if Comp[4] = COMPRESS_STORED then
    if PCardinal(Comp)^ = PCardinal(Comp + 5)^ then
      BodyLen := CompLen - 9
    else
      exit
  else if Comp[4] = AnsiChar(AlgoID) then
    BodyLen := AlgoDecompressDestLen(Comp + 9)
  else
    exit;
  if PartialLen > BodyLen then
    PartialLen := BodyLen;
  if Comp[4] = COMPRESS_STORED then
    MoveFast(Comp[9], Partial[0], PartialLen)
  else if AlgoDecompressPartial(Comp + 9, CompLen - 9,
           Partial, PartialLen, PartialLenMax) < PartialLen then
    exit;
  result := PartialLen;
end;

type
  // disk header of TAlgoCompress chunk
  TAlgoCompressHead = packed record
    Magic: cardinal;
    CompressedSize: integer;
    HashCompressed: cardinal;
    UnCompressedSize: integer;
    HashUncompressed: cardinal;
  end;
  PAlgoCompressHead = ^TAlgoCompressHead;

  TAlgoCompressTrailer = packed record
    HeaderRelativeOffset: cardinal;
    Magic: cardinal;
  end;
  PAlgoCompressTrailer = ^TAlgoCompressTrailer;

function TAlgoCompress.StreamCompress(Source: TCustomMemoryStream;
  Dest: TStream; Magic: cardinal; ForceHash32: boolean): integer;
var
  DataLen: integer;
  S, D: pointer;
  Head: TAlgoCompressHead;
  Trailer: TAlgoCompressTrailer;
  tmp: TSynTempBuffer;
begin
  if Dest = nil then
  begin
    result := 0;
    exit;
  end;
  if Source <> nil then
  begin
    S := Source.Memory;
    DataLen := Source.Size;
  end
  else
  begin
    S := nil;
    DataLen := 0;
  end;
  tmp.Init(AlgoCompressDestLen(DataLen));
  try
    Head.Magic := Magic;
    Head.UnCompressedSize := DataLen;
    Head.HashUncompressed := AlgoHash(ForceHash32, S, DataLen);
    result := AlgoCompress(S, DataLen, tmp.buf);
    if result > tmp.len then
      raise EAlgoCompress.Create('StreamCompress: overflow'); // paranoid
    if result > DataLen then
    begin
      // compression is not worth it -> store
      result := DataLen;
      D := S;
      Head.HashCompressed := Head.HashUncompressed;
    end
    else
    begin
      D := tmp.buf;
      Head.HashCompressed := AlgoHash(ForceHash32, D, result)
    end;
    Head.CompressedSize := result;
    Dest.WriteBuffer(Head, SizeOf(Head));
    Dest.WriteBuffer(D^, Head.CompressedSize);
    Trailer.HeaderRelativeOffset := result + (SizeOf(Head) + SizeOf(Trailer));
    Trailer.Magic := Magic;
    Dest.WriteBuffer(Trailer, SizeOf(Trailer));
    result := Head.CompressedSize + (SizeOf(Head) + SizeOf(Trailer));
  finally
    tmp.Done;
  end;
end;

function TAlgoCompress.StreamCompress(Source: TCustomMemoryStream;
  const DestFile: TFileName; Magic: cardinal; ForceHash32: boolean): integer;
var
  F: TFileStream;
begin
  F := TFileStream.Create(DestFile, fmCreate);
  try
    result := StreamCompress(Source, F, Magic, ForceHash32);
  finally
    F.Free;
  end;
end;

function TAlgoCompress.StreamUnCompress(Source: TStream; Magic: cardinal;
  ForceHash32: boolean): TMemoryStream;
var
  S, D: PAnsiChar;
  sourcePosition, resultSize, sourceSize, buflen: Int64;
  t: PAlgoCompressTrailer;
  Head: TAlgoCompressHead;
  Trailer: TAlgoCompressTrailer;
  buf: RawByteString;
  stored: boolean;
begin
  result := nil;
  if Source = nil then
    exit;
  sourceSize := Source.Size;
  {$ifdef CPU32}
  if sourceSize > maxInt then
    exit; // result TMemoryStream should stay in memory!
  {$endif CPU32}
  sourcePosition := Source.Position;
  if sourceSize - sourcePosition < SizeOf(Head) then
    exit;
  resultSize := 0;
  repeat
    if (Source.Read(Head, SizeOf(Head)) <> SizeOf(Head)) or
       (Head.Magic <> Magic) then
    begin
      // Source not positioned as expected -> try from the end
      Source.Position := sourceSize - SizeOf(Trailer);
      if (Source.Read(Trailer, SizeOf(Trailer)) <> SizeOf(Trailer)) or
         (Trailer.Magic <> Magic) then
      begin
        // may have been appended before a digital signature -> try last 64KB
        buflen := 65536;
        if sourcesize < buflen then
          buflen := sourcesize;
        SetLength(buf, buflen);
        Source.Position := sourceSize - buflen;
        if Source.Read(pointer(buf)^, buflen) <> buflen then
          exit;
        t := @PByteArray(buf)[buflen - SizeOf(t^)];
        repeat
          dec(PByte(t)); // search backward
          if PtrUInt(t) < PtrUInt(buf) then
            exit;
        until t^.Magic = Magic;
        sourcePosition := sourceSize - t^.HeaderRelativeOffset; // found
      end
      else
        sourcePosition := sourceSize - Trailer.HeaderRelativeOffset;
      Source.Position := sourcePosition;
      if (Source.Read(Head, SizeOf(Head)) <> SizeOf(Head)) or
         (Head.Magic <> Magic) then
        exit;
    end;
    inc(sourcePosition, SizeOf(Head));
    if sourcePosition + Head.CompressedSize > sourceSize then
      exit;
    if Source.InheritsFrom(TCustomMemoryStream) then
    begin
      S := PAnsiChar(TCustomMemoryStream(Source).Memory) + PtrUInt(sourcePosition);
      Source.Seek(Head.CompressedSize, soCurrent);
    end
    else
    begin
      if Head.CompressedSize > length({%H-}buf) then
        SetString(buf, nil, Head.CompressedSize);
      S := pointer(buf);
      Source.Read(S^, Head.CompressedSize);
    end;
    inc(sourcePosition, Head.CompressedSize);
    if (Source.Read(Trailer, SizeOf(Trailer)) <> SizeOf(Trailer)) or
       (Trailer.Magic <> Magic) then
      // trailer not available in old .synlz layout, or in multiblocks
      Source.Position := sourcePosition
    else
      sourceSize := 0; // should be monoblock
    // Source stream will now point after all data
    stored := (Head.CompressedSize = Head.UnCompressedSize) and
              (Head.HashCompressed = Head.HashUncompressed);
    if not stored then
      if AlgoDecompressDestLen(S) <> Head.UnCompressedSize then
        exit;
    if AlgoHash(ForceHash32, S, Head.CompressedSize) <> Head.HashCompressed then
      exit;
    if result = nil then
      result := TMemoryStream.Create
    else
    begin
      {$ifdef CPU32}
      if resultSize + Head.UnCompressedSize > maxInt then
      begin
        FreeAndNil(result); // result TMemoryStream should stay in memory!
        break;
      end;
      {$endif CPU32}
    end;
    result.Size := resultSize + Head.UnCompressedSize;
    D := PAnsiChar(result.Memory) + resultSize;
    inc(resultSize, Head.UnCompressedSize);
    if stored then
      MoveFast(S^, D^, Head.CompressedSize)
    else if (AlgoDecompress(S, Head.CompressedSize, D) <> Head.UnCompressedSize) or
       (AlgoHash(ForceHash32, D, Head.UnCompressedSize) <> Head.HashUncompressed) then
      FreeAndNil(result);
  until (result = nil) or
        (sourcePosition >= sourceSize);
end;

function TAlgoCompress.StreamUnCompress(const Source: TFileName;
  Magic: cardinal; ForceHash32: boolean): TMemoryStream;
var
  S: TStream;
begin
  try
    S := TSynMemoryStreamMapped.Create(Source);
    try
      result := StreamUnCompress(S, Magic, ForceHash32);
    finally
      S.Free;
    end;
  except
    on E: Exception do
      result := nil;
  end;
end;

function TAlgoCompress.StreamComputeLen(P: PAnsiChar; Len: PtrUInt;
  Magic: cardinal): integer;
var
  trailer: PAlgoCompressTrailer;
begin
  if (P = nil) or
     (Len <= SizeOf(TAlgoCompressTrailer)) then
    result := 0
  else
  begin
    trailer := PAlgoCompressTrailer(P + Len - SizeOf(TAlgoCompressTrailer));
    if (Magic = trailer^.Magic) and
       (trailer^.HeaderRelativeOffset < Len) and
       (PAlgoCompressHead(P + Len - trailer^.HeaderRelativeOffset)^.Magic = Magic) then
      // trim existing content
      result := Len - trailer^.HeaderRelativeOffset
    else
      result := Len;
  end;
end;

function TAlgoCompress.FileIsCompressed(const Name: TFileName;
  Magic: cardinal): boolean;
var
  S: TFileStream;
  Head: TAlgoCompressHead;
begin
  result := false;
  if FileExists(Name) then
  try
    S := TFileStream.Create(Name, fmOpenRead or fmShareDenyNone);
    try
      if S.Read(Head, SizeOf(Head)) = SizeOf(Head) then
        if Head.Magic = Magic then
          result := true; // only check magic, since there may be several chunks
    finally
      S.Free;
    end;
  except
    on Exception do
      result := false;
  end;
end;

function TAlgoCompress.FileCompress(const Source, Dest: TFileName;
  Magic: cardinal; ForceHash32: boolean; ChunkBytes: Int64): boolean;
var
  src, dst: RawByteString; // tmp buffers
  S, D: THandleStream;
  Head: TAlgoCompressHead;
  Count: Int64;
begin
  result := false;
  if (ChunkBytes > 0) and
     FileExists(Source) then
  try
    S := FileStreamSequentialRead(Source);
    try
      DeleteFile(Dest);
      D := TFileStream.Create(Dest, fmCreate);
      try
        Head.Magic := Magic;
        Count := S.Size;
        while Count > 0 do
        begin
          // compress Source into Dest with proper chunking
          if Count > ChunkBytes then
            Head.UnCompressedSize := ChunkBytes
          else
            Head.UnCompressedSize := Count;
          if {%H-}src = '' then
            SetString(src, nil, Head.UnCompressedSize);
          if {%H-}dst = '' then
            SetString(dst, nil, AlgoCompressDestLen(Head.UnCompressedSize));
          Head.UnCompressedSize := S.Read(pointer(src)^, Head.UnCompressedSize);
          if Head.UnCompressedSize <= 0 then
            exit; // read error
          Head.HashUncompressed :=
            AlgoHash(ForceHash32, pointer(src), Head.UnCompressedSize);
          Head.CompressedSize :=
            AlgoCompress(pointer(src), Head.UnCompressedSize, pointer(dst));
          Head.HashCompressed :=
            AlgoHash(ForceHash32, pointer(dst), Head.CompressedSize);
          if (D.Write(Head, SizeOf(Head)) <> SizeOf(Head)) or
             (D.Write(pointer(dst)^, Head.CompressedSize) <> Head.CompressedSize) then
            exit;
          dec(Count, Head.UnCompressedSize);
        end;
      finally
        D.Free;
      end;
      result := FileSetDateFrom(Dest, S.Handle);
    finally
      S.Free;
    end;
  except
    on Exception do
      result := false;
  end;
end;

function TAlgoCompress.FileUnCompress(const Source, Dest: TFileName;
  Magic: cardinal; ForceHash32: boolean): boolean;
var
  src, dst: RawByteString;
  S, D: THandleStream;
  Count: Int64;
  Head: TAlgoCompressHead;
begin
  result := false;
  if FileExists(Source) then
  try
    S := FileStreamSequentialRead(Source);
    try
      DeleteFile(Dest);
      D := TFileStream.Create(Dest, fmCreate);
      try
        Count := S.Size;
        while Count > 0 do
        begin
          // uncompress Source into Dest with proper chunking
          if S.Read(Head, SizeOf(Head)) <> SizeOf(Head) then
            exit;
          dec(Count, SizeOf(Head));
          if (Head.Magic <> Magic) or
             (Head.CompressedSize > Count) then
            exit;
          if Head.CompressedSize > length({%H-}src) then
            SetString(src, nil, Head.CompressedSize);
          if S.Read(pointer(src)^, Head.CompressedSize) <> Head.CompressedSize then
            exit;
          dec(Count, Head.CompressedSize);
          if (AlgoHash(ForceHash32, pointer(src), Head.CompressedSize) <> Head.HashCompressed) or
             (AlgoDecompressDestLen(pointer(src)) <> Head.UnCompressedSize) then
            exit;
          if Head.UnCompressedSize > length({%H-}dst) then
            SetString(dst, nil, Head.UnCompressedSize);
          if AlgoDecompress(pointer(src), Head.CompressedSize, pointer(dst)) <>
              Head.UnCompressedSize then
             exit;
          if (AlgoHash(ForceHash32, pointer(dst), Head.UnCompressedSize) <> Head.HashUncompressed) or
             (D.Write(pointer(dst)^, Head.UncompressedSize) <> Head.UncompressedSize) then
            exit;
        end;
      finally
        D.Free;
      end;
      result := FileSetDateFrom(Dest, S.Handle);
    finally
      S.Free;
    end;
  except
    on Exception do
      result := false;
  end;
end;

function TAlgoCompress.DecompressHeader(Comp: PAnsiChar; CompLen: integer;
  Load: TAlgoCompressLoad): integer;
begin
  result := 0;
  if (self = nil) or
     (CompLen <= 9) or
     (Comp = nil) or
     ((Load <> aclNoCrcFast) and
      (AlgoHash(0, Comp + 9, CompLen - 9) <> PCardinal(Comp + 5)^)) then
    exit;
  if Comp[4] = COMPRESS_STORED then
  begin
    if PCardinal(Comp)^ = PCardinal(Comp + 5)^ then
      result := CompLen - 9;
  end
  else if Comp[4] = AnsiChar(AlgoID) then
    result := AlgoDecompressDestLen(Comp + 9);
end;

function TAlgoCompress.DecompressBody(Comp, Plain: PAnsiChar;
  CompLen, PlainLen: integer; Load: TAlgoCompressLoad): boolean;
begin
  result := false;
  if (self = nil) or
     (PlainLen <= 0) then
    exit;
  if Comp[4] = COMPRESS_STORED then
    MoveFast(Comp[9], Plain[0], PlainLen)
  else if Comp[4] = AnsiChar(AlgoID) then
    case Load of
      aclNormal:
        if (AlgoDecompress(Comp + 9, CompLen - 9, Plain) <> PlainLen) or
           (AlgoHash(0, Plain, PlainLen) <> PCardinal(Comp)^) then
          exit;
      aclSafeSlow:
        if (AlgoDecompressPartial(Comp + 9, CompLen - 9,
            Plain, PlainLen, PlainLen) <> PlainLen) or
           (AlgoHash(0, Plain, PlainLen) <> PCardinal(Comp)^) then
          exit;
      aclNoCrcFast:
        if AlgoDecompress(Comp + 9, CompLen - 9, Plain) <> PlainLen then
          exit;
    end;
  result := true;
end;

function TAlgoCompress.EventArchive(aMagic: cardinal;
  const aOldLogFileName, aDestinationPath, aDestinationExt: TFileName): boolean;
begin
  // aDestinationPath = 'ArchivePath\log\YYYYMM\'
  result := false;
  if (aOldLogFileName <> '') and
     FileExists(aOldLogFileName) then
  try
    if DirectoryExists(aDestinationPath) or
       CreateDir(aDestinationPath) then
      if FileCompress(aOldLogFileName,
         aDestinationPath + ExtractFileName(aOldLogFileName) + aDestinationExt,
         aMagic, {hash32=}true) then
        result := DeleteFile(aOldLogFileName);
  except
    on Exception do
      result := false;
  end;
end;


{ TAlgoSynLZ }

constructor TAlgoSynLZ.Create;
begin
  fAlgoID := COMPRESS_SYNLZ; // =1
  inherited Create;
end;

function TAlgoSynLZ.AlgoCompress(Plain: pointer; PlainLen: integer; Comp: pointer): integer;
begin
  result := SynLZcompress1(Plain, PlainLen, Comp);
end;

function TAlgoSynLZ.AlgoCompressDestLen(PlainLen: integer): integer;
begin
  result := SynLZcompressdestlen(PlainLen);
end;

function TAlgoSynLZ.AlgoDecompress(Comp: pointer; CompLen: integer; Plain: pointer): integer;
begin
  result := SynLZdecompress1(Comp, CompLen, Plain);
end;

function TAlgoSynLZ.AlgoDecompressDestLen(Comp: pointer): integer;
begin
  result := SynLZdecompressdestlen(Comp);
end;

function TAlgoSynLZ.AlgoDecompressPartial(Comp: pointer; CompLen: integer;
  Partial: pointer; PartialLen, PartialLenMax: integer): integer;
begin
  result := SynLZdecompress1partial(Comp, CompLen, Partial, PartialLen);
end;


{ TAlgoCompressWithNoDestLen }

function TAlgoCompressWithNoDestLen.AlgoCompress(Plain: pointer; PlainLen: integer;
  Comp: pointer): integer;
begin
  Comp := ToVarUInt32(PlainLen, Comp); // e.g. deflate don't store PlainLen
  result := RawProcess(Plain, Comp, PlainLen, AlgoCompressDestLen(PlainLen), 0, doCompress);
  if result > 0 then
    inc(result, ToVarUInt32Length(PlainLen));
end;

function TAlgoCompressWithNoDestLen.AlgoDecompress(Comp: pointer;
  CompLen: integer; Plain: pointer): integer;
var
  start: PAnsiChar;
begin
  start := Comp;
  result := FromVarUInt32(PByte(Comp));
  if RawProcess(Comp, Plain, CompLen + (start - Comp), result, 0, doUnCompress) <> result then
    result := 0;
end;

function TAlgoCompressWithNoDestLen.AlgoDecompressDestLen(Comp: pointer): integer;
begin
  if Comp = nil then
    result := 0
  else
    result := FromVarUInt32(PByte(Comp));
end;

function TAlgoCompressWithNoDestLen.AlgoDecompressPartial(Comp: pointer;
  CompLen: integer; Partial: pointer; PartialLen, PartialLenMax: integer): integer;
var
  start: PAnsiChar;
begin
  start := Comp;
  result := FromVarUInt32(PByte(Comp));
  if PartialLenMax > result then
    PartialLenMax := result;
  result := RawProcess(Comp, Partial, CompLen + (start - Comp),
    PartialLen, PartialLenMax, doUncompressPartial);
end;


function RawByteStringArrayConcat(const Values: array of RawByteString): RawByteString;
var
  i, L: PtrInt;
  P: PAnsiChar;
begin
  L := 0;
  for i := 0 to high(Values) do
    inc(L, length(Values[i]));
  SetString(result, nil, L);
  P := pointer(result);
  for i := 0 to high(Values) do
  begin
    L := length(Values[i]);
    MoveFast(pointer(Values[i])^, P^, L);
    inc(P, L);
  end;
end;

procedure RawByteStringToBytes(const buf: RawByteString; out bytes: TBytes);
var
  L: integer;
begin
  L := Length(buf);
  if L <> 0 then
  begin
    SetLength(bytes, L);
    MoveFast(pointer(buf)^, pointer(bytes)^, L);
  end;
end;

procedure BytesToRawByteString(const bytes: TBytes; out buf: RawByteString);
begin
  SetString(buf, PAnsiChar(pointer(bytes)), Length(bytes));
end;

procedure ResourceToRawByteString(const ResName: string; ResType: PChar;
  out buf: RawByteString; Instance: THandle);
var
  res: TExecutableResource;
begin
  if res.Open(ResName, ResType, Instance) then
  begin
    SetString(buf, PAnsiChar(res.Buffer), res.Size);
    res.Close;
  end;
end;

procedure ResourceSynLZToRawByteString(const ResName: string;
  out buf: RawByteString; Instance: THandle);
var
  res: TExecutableResource;
begin
  if res.Open(ResName, PChar(10), Instance) then
  begin
    AlgoSynLZ.Decompress(res.Buffer, res.Size, buf);
    res.Close;
  end;
end;

{$ifndef PUREMORMOT2}

function StreamSynLZComputeLen(P: PAnsiChar; Len, Magic: cardinal): integer;
begin
  result := AlgoSynLZ.StreamComputeLen(P, Len, Magic);
end;

function StreamSynLZ(Source: TCustomMemoryStream; Dest: TStream; Magic: cardinal): integer;
begin
  result := AlgoSynLZ.StreamCompress(Source, Dest, Magic, {hash32=}true);
end;

function StreamSynLZ(Source: TCustomMemoryStream; const DestFile: TFileName;
  Magic: cardinal): integer;
begin
  result := AlgoSynLZ.StreamCompress(Source, DestFile, Magic, {hash32=}true);
end;

function FileSynLZ(const Source, Dest: TFileName; Magic: cardinal): boolean;
begin
  result := AlgoSynLZ.FileCompress(Source, Dest, Magic, {hash32=}true);
end;

function FileUnSynLZ(const Source, Dest: TFileName; Magic: cardinal): boolean;
begin
  result := AlgoSynLZ.FileUnCompress(Source, Dest, Magic, {hash32=}true);
end;

function FileIsSynLZ(const Name: TFileName; Magic: cardinal): boolean;
begin
  result := AlgoSynLZ.FileIsCompressed(Name, Magic);
end;

function StreamUnSynLZ(const Source: TFileName; Magic: cardinal): TMemoryStream;
begin
  result := AlgoSynLZ.StreamUnCompress(Source, Magic, {hash32=}true);
end;

function StreamUnSynLZ(Source: TStream; Magic: cardinal): TMemoryStream;
begin
  result := AlgoSynLZ.StreamUnCompress(Source, Magic, {hash32=}true);
end;

function SynLZCompress(const Data: RawByteString; CompressionSizeTrigger: integer;
  CheckMagicForCompressed: boolean): RawByteString;
begin
  result := AlgoSynLZ.Compress(pointer(Data), length(Data),
    CompressionSizeTrigger, CheckMagicForCompressed);
end;

procedure SynLZCompress(P: PAnsiChar; PLen: integer; out Result: RawByteString;
  CompressionSizeTrigger: integer; CheckMagicForCompressed: boolean);
begin
  Result := AlgoSynLZ.Compress(P, PLen, CompressionSizeTrigger, CheckMagicForCompressed);
end;

function SynLZCompress(P, Dest: PAnsiChar; PLen, DestLen: integer;
  CompressionSizeTrigger: integer; CheckMagicForCompressed: boolean): integer;
begin
  result := AlgoSynLZ.Compress(P, Dest, PLen, DestLen,
    CompressionSizeTrigger, CheckMagicForCompressed);
end;

function SynLZDecompress(const Data: RawByteString): RawByteString;
begin
  AlgoSynLZ.Decompress(pointer(Data), Length(Data), result);
end;

function SynLZDecompressHeader(P: PAnsiChar; PLen: integer): integer;
begin
  result := AlgoSynLZ.DecompressHeader(P, PLen);
end;

function SynLZDecompressBody(P, Body: PAnsiChar; PLen, BodyLen: integer;
  SafeDecompression: boolean): boolean;
begin
  result := AlgoSynLZ.DecompressBody(P, Body, PLen, BodyLen,
    ALGO_SAFE[SafeDecompression]);
end;

function SynLZDecompressPartial(P, Partial: PAnsiChar; PLen, PartialLen: integer): integer;
begin
  result := AlgoSynLZ.DecompressPartial(P, Partial, PLen, PartialLen, PartialLen);
end;

procedure SynLZDecompress(P: PAnsiChar; PLen: integer; out Result: RawByteString;
  SafeDecompression: boolean);
begin
  AlgoSynLZ.Decompress(P, PLen, Result);
end;

function SynLZDecompress(const Data: RawByteString; out Len: integer;
  var tmp: RawByteString): pointer;
begin
  result := AlgoSynLZ.Decompress(pointer(Data), length(Data), Len, tmp);
end;

function SynLZDecompress(P: PAnsiChar; PLen: integer; out Len: integer;
  var tmp: RawByteString): pointer;
begin
  result := AlgoSynLZ.Decompress(P, PLen, Len, tmp);
end;

function SynLZCompressToBytes(const Data: RawByteString;
  CompressionSizeTrigger: integer): TByteDynArray;
begin
  result := AlgoSynLZ.CompressToBytes(pointer(Data), length(Data),
    CompressionSizeTrigger);
end;

function SynLZCompressToBytes(P: PAnsiChar;
  PLen, CompressionSizeTrigger: integer): TByteDynArray;
begin
  result := AlgoSynLZ.CompressToBytes(P, PLen, CompressionSizeTrigger);
end;

function SynLZDecompress(const Data: TByteDynArray): RawByteString;
begin
  AlgoSynLZ.Decompress(pointer(Data), length(Data), result);
end;

{$endif PUREMORMOT2}


{ ************ Base64, Base64Uri, Base58 and Baudot Encoding / Decoding }

type
  TBase64Enc = array[0..63] of AnsiChar;
  PBase64Enc = ^TBase64Enc;
  TBase64Dec = array[AnsiChar] of shortint;
  PBase64Dec = ^TBase64Dec;

const
  b64enc: TBase64Enc =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  b64Urienc: TBase64Enc =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_';

var
  /// a conversion table from Base64 text into binary data
  // - used by Base64ToBin/IsBase64 functions
  // - contains -1 for invalid char, -2 for '=', 0..63 for b64enc[] chars
  ConvertBase64ToBin, ConvertBase64UriToBin: TBase64Dec;


{ --------- Base64 encoding/decoding }

function Base64AnyDecode(const decode: TBase64Dec; sp, rp: PAnsiChar; len: PtrInt): boolean;
var
  c, ch: PtrInt;
begin
  result := false;
  while len >= 4 do
  begin
    c := decode[sp[0]];
    if c < 0 then
      exit;
    c := c shl 6;
    ch := decode[sp[1]];
    if ch < 0 then
      exit;
    c := (c or ch) shl 6;
    ch := decode[sp[2]];
    if ch < 0 then
      exit;
    c := (c or ch) shl 6;
    ch := decode[sp[3]];
    if ch < 0 then
      exit;
    c := c or ch;
    rp[2] := AnsiChar(c);
    c := c shr 8;
    rp[1] := AnsiChar(c);
    c := c shr 8;
    rp[0] := AnsiChar(c);
    dec(len, 4);
    inc(rp, 3);
    inc(sp, 4);
  end;
  if len >= 2 then
  begin
    c := decode[sp[0]];
    if c < 0 then
      exit;
    c := c shl 6;
    ch := decode[sp[1]];
    if ch < 0 then
      exit;
    if len = 2 then
      rp[0] := AnsiChar((c or ch) shr 4)
    else
    begin
      c := (c or ch) shl 6;
      ch := decode[sp[2]];
      if ch < 0 then
        exit;
      c := (c or ch) shr 2;
      rp[1] := AnsiChar(c);
      rp[0] := AnsiChar(c shr 8);
    end;
  end;
  result := true;
end;

function Base64Decode(sp, rp: PAnsiChar; len: PtrInt): boolean;
{$ifdef FPC} inline;{$endif}
var
  tab: PBase64Dec; // use local register
begin
  tab := @ConvertBase64ToBin;
  len := len shl 2; // len was the number of 4 chars chunks in sp
  if (len > 0) and
     (tab[sp[len - 2]] >= 0) then
    if tab[sp[len - 1]] >= 0 then
      // no trim
    else
      dec(len)
  else
    dec(len, 2); // Base64AnyDecode() algorithm ignores the trailing '='
  result := Base64AnyDecode(tab^, sp, rp, len);
end;

{$ifdef ASMX86}

function Base64EncodeMain(rp, sp: PAnsiChar; len: cardinal): integer;
  {$ifdef FPC}nostackframe; assembler; {$endif}
asm // eax=rp edx=sp ecx=len - pipeline optimized version by AB
        push    ebx
        push    esi
        push    edi
        push    ebp
        mov     ebx, edx
        mov     esi, eax
        mov     eax, ecx
        mov     edx, 1431655766 // faster eax=len div 3 using reciprocal
        sar     ecx, 31
        imul    edx
        mov     eax, edx
        sub     eax, ecx
        mov     edi, offset b64enc
        mov     ebp, eax
        push    eax
        jz      @z
        // edi=b64enc[] ebx=sp esi=rp ebp=len div 3
        xor     eax, eax
        @1:     // read 3 bytes from sp
        movzx   edx, byte ptr [ebx]
        shl     edx, 16
        mov     al, [ebx + 2]
        mov     ah, [ebx + 1]
        add     ebx, 3
        or      eax, edx
        // encode as Base64
        mov     ecx, eax
        mov     edx, eax
        shr     ecx, 6
        and     edx, $3f
        and     ecx, $3f
        mov     dh, [edi + edx]
        mov     dl, [edi + ecx]
        mov     ecx, eax
        shr     eax, 12
        shr     ecx, 18
        shl     edx, 16
        and     ecx, $3f
        and     eax, $3f
        mov     cl, [edi + ecx]
        mov     ch, [edi + eax]
        or      ecx, edx
        // write the 4 encoded bytes into rp
        mov     [esi], ecx
        add     esi, 4
        dec     ebp
        jnz     @1
@z:     pop     eax // result := len div 3
        pop     ebp
        pop     edi
        pop     esi
        pop     ebx
end;

{$else}

function Base64EncodeMain(rp, sp: PAnsiChar; len: cardinal): integer;
var
  c: cardinal;
  enc: PBase64Enc; // use local register
begin
  enc := @b64enc;
  len := len div 3;
  result := len;
  if len <> 0 then
    repeat
      c := (ord(sp[0]) shl 16) or (ord(sp[1]) shl 8) or ord(sp[2]);
      rp[0] := enc[(c shr 18) and $3f];
      rp[1] := enc[(c shr 12) and $3f];
      rp[2] := enc[(c shr 6) and $3f];
      rp[3] := enc[c and $3f];
      inc(rp, 4);
      inc(sp, 3);
      dec(len)
    until len = 0;
end;

{$endif ASMX86}

procedure Base64EncodeTrailing(rp, sp: PAnsiChar; len: cardinal);
var
  c: cardinal;
  enc: PBase64Enc; // use local register
begin
  enc := @b64enc;
  case len of
    1:
      begin
        c := ord(sp[0]) shl 4;
        rp[0] := enc[(c shr 6) and $3f];
        rp[1] := enc[c and $3f];
        PWord(rp + 2)^ := ord('=') + ord('=') shl 8;
      end;
    2:
      begin
        c := (ord(sp[0]) shl 10) or (ord(sp[1]) shl 2);
        rp[0] := enc[(c shr 12) and $3f];
        rp[1] := enc[(c shr 6) and $3f];
        rp[2] := enc[c and $3f];
        rp[3] := '=';
      end;
  end;
end;

procedure Base64Encode(rp, sp: PAnsiChar; len: cardinal);
var
  main: cardinal;
begin
  main := Base64EncodeMain(rp, sp, len);
  Base64EncodeTrailing(rp + main * 4, sp + main * 3, len - main * 3);
end;

function BinToBase64Length(len: PtrUInt): PtrUInt;
begin
  result := ((len + 2) div 3) * 4;
end;

function BinToBase64(const s: RawByteString): RawUtf8;
var
  len: integer;
begin
  result := '';
  len := length(s);
  if len = 0 then
    exit;
  FastSetString(result, nil, BinToBase64Length(len));
  Base64Encode(pointer(result), pointer(s), len);
end;

function BinToBase64Short(Bin: PAnsiChar; BinBytes: integer): shortstring;
var
  destlen: integer;
begin
  result := '';
  if BinBytes = 0 then
    exit;
  destlen := BinToBase64Length(BinBytes);
  if destlen > 255 then
    exit; // avoid buffer overflow
  result[0] := AnsiChar(destlen);
  Base64Encode(@result[1], Bin, BinBytes);
end;

function BinToBase64Short(const s: RawByteString): shortstring;
begin
  result := BinToBase64Short(pointer(s), length(s));
end;

function BinToBase64(Bin: PAnsiChar; BinBytes: integer): RawUtf8;
begin
  result := '';
  if BinBytes = 0 then
    exit;
  FastSetString(result, nil, BinToBase64Length(BinBytes));
  Base64Encode(pointer(result), Bin, BinBytes);
end;

function BinToBase64(const data, Prefix, Suffix: RawByteString; WithMagic: boolean): RawUtf8;
var
  lendata, lenprefix, lensuffix, len: integer;
  res: PByteArray absolute result;
begin
  result := '';
  lendata := length(data);
  lenprefix := length(Prefix);
  lensuffix := length(Suffix);
  if lendata + lenprefix + lensuffix = 0 then
    exit;
  len := ((lendata + 2) div 3) * 4 + lenprefix + lensuffix;
  if WithMagic then
    inc(len, 3);
  FastSetString(result, nil, len);
  if lenprefix > 0 then
    MoveSmall(pointer(Prefix), res, lenprefix);
  if WithMagic then
  begin
    PInteger(@res[lenprefix])^ := JSON_BASE64_MAGIC_C;
    inc(lenprefix, 3);
  end;
  Base64Encode(@res[lenprefix], pointer(data), lendata);
  if lensuffix > 0 then
    MoveSmall(pointer(Suffix), @res[len - lensuffix], lensuffix);
end;

function BinToBase64WithMagic(const data: RawByteString): RawUtf8;
var
  len: integer;
begin
  result := '';
  len := length(data);
  if len = 0 then
    exit;
  FastSetString(result, nil, ((len + 2) div 3) * 4 + 3);
  PInteger(pointer(result))^ := JSON_BASE64_MAGIC_C;
  Base64Encode(PAnsiChar(pointer(result)) + 3, pointer(data), len);
end;

function BinToBase64WithMagic(Data: pointer; DataLen: integer): RawUtf8;
begin
  result := '';
  if DataLen <= 0 then
    exit;
  FastSetString(result, nil, ((DataLen + 2) div 3) * 4 + 3);
  PInteger(pointer(result))^ := JSON_BASE64_MAGIC_C;
  Base64Encode(PAnsiChar(pointer(result)) + 3, Data, DataLen);
end;

function IsBase64Internal(sp: PAnsiChar; len: PtrInt; dec: PBase64Dec): boolean;
var
  i: PtrInt;
begin
  result := false;
  if (len = 0) or
     (len and 3 <> 0) then
    exit;
  for i := 0 to len - 5 do
    if dec[sp[i]] < 0 then
      exit;
  inc(sp, len - 4);
  if (dec[sp[0]] = -1) or
     (dec[sp[1]] = -1) or
     (dec[sp[2]] = -1) or
     (dec[sp[3]] = -1) then
    exit;
  result := true; // layout seems correct
end;

function IsBase64(sp: PAnsiChar; len: PtrInt): boolean;
begin
  result := IsBase64Internal(sp, len, @ConvertBase64ToBin);
end;

function IsBase64(const s: RawByteString): boolean;
begin
  result := IsBase64Internal(pointer(s), length(s), @ConvertBase64ToBin);
end;

function Base64ToBinLengthSafe(sp: PAnsiChar; len: PtrInt): PtrInt;
var
  dec: PBase64Dec;
begin
  dec := @ConvertBase64ToBin;
  if IsBase64Internal(sp, len, dec) then
  begin
    if dec[sp[len - 2]] >= 0 then
      if dec[sp[len - 1]] >= 0 then
        result := 0
      else
        result := 1
    else
      result := 2;
    result := (len shr 2) * 3 - result;
  end
  else
    result := 0;
end;

function Base64ToBinLength(sp: PAnsiChar; len: PtrInt): PtrInt;
var
  dec: PBase64Dec;
begin
  result := 0;
  if (len = 0) or
     (len and 3 <> 0) then
    exit;
  dec := @ConvertBase64ToBin;
  if dec[sp[len - 2]] >= 0 then
    if dec[sp[len - 1]] >= 0 then
      result := 0
    else
      result := 1
  else
    result := 2;
  result := (len shr 2) * 3 - result;
end;

function Base64ToBin(const s: RawByteString): RawByteString;
begin
  Base64ToBinSafe(pointer(s), length(s), result);
end;

function Base64ToBin(sp: PAnsiChar; len: PtrInt): RawByteString;
begin
  Base64ToBinSafe(sp, len, result);
end;

function Base64ToBin(sp: PAnsiChar; len: PtrInt; var data: RawByteString): boolean;
begin
  result := Base64ToBinSafe(sp, len, data);
end;

function Base64ToBinSafe(const s: RawByteString): RawByteString;
begin
  Base64ToBinSafe(pointer(s), length(s), result);
end;

function Base64ToBinSafe(sp: PAnsiChar; len: PtrInt): RawByteString;
begin
  Base64ToBinSafe(sp, len, result);
end;

function Base64ToBinSafe(sp: PAnsiChar; len: PtrInt; var data: RawByteString): boolean;
var
  resultLen: PtrInt;
begin
  resultLen := Base64ToBinLength(sp, len);
  if resultLen <> 0 then
  begin
    SetString(data, nil, resultLen);
    if ConvertBase64ToBin[sp[len - 2]] >= 0 then
      if ConvertBase64ToBin[sp[len - 1]] >= 0 then
        // keep len as it is
      else
        dec(len)
    else
      dec(len, 2); // adjust for Base64AnyDecode() algorithm
    result := Base64AnyDecode(ConvertBase64ToBin, sp, pointer(data), len);
    if not result then
      data := '';
  end
  else
  begin
    result := false;
    data := '';
  end;
end;

function Base64ToBin(sp: PAnsiChar; len: PtrInt; var blob: TSynTempBuffer): boolean;
begin
  blob.Init(Base64ToBinLength(sp, len));
  result := (blob.len > 0) and
            Base64Decode(sp, blob.buf, len shr 2);
end;

function Base64ToBin(base64, bin: PAnsiChar; base64len, binlen: PtrInt
  {$ifndef PUREMORMOT2} ; nofullcheck: boolean {$endif}): boolean;
begin
  // nofullcheck is just ignored and deprecated
  result := (bin <> nil) and
            (Base64ToBinLength(base64, base64len) = binlen) and
            Base64Decode(base64, bin, base64len shr 2);
end;

function Base64ToBin(const base64: RawByteString; bin: PAnsiChar; binlen: PtrInt
  {$ifndef PUREMORMOT2} ; nofullcheck: boolean {$endif}): boolean;
begin
  result := Base64ToBin(pointer(base64), bin, length(base64), binlen);
end;

{ --------- Base64 URI encoding/decoding }

{$ifdef ASMX86}

function Base64uriEncodeMain(rp, sp: PAnsiChar; len: cardinal): integer;
  {$ifdef FPC}nostackframe; assembler; {$endif}
asm // eax=rp edx=sp ecx=len - pipeline optimized version by AB
        push    ebx
        push    esi
        push    edi
        push    ebp
        mov     ebx, edx
        mov     esi, eax
        mov     eax, ecx
        mov     edx, 1431655766 // faster eax=len div 3 using reciprocal
        sar     ecx, 31
        imul    edx
        mov     eax, edx
        sub     eax, ecx
        mov     edi, offset b64urienc
        mov     ebp, eax
        push    eax
        jz      @z
        // edi=b64urienc[] ebx=sp esi=rp ebp=len div 3
        xor     eax, eax
@1:    // read 3 bytes from sp
        movzx   edx, byte ptr [ebx]
        shl     edx, 16
        mov     al, [ebx + 2]
        mov     ah, [ebx + 1]
        add     ebx, 3
        or      eax, edx
        // encode as Base64uri
        mov     ecx, eax
        mov     edx, eax
        shr     ecx, 6
        and     edx, $3f
        and     ecx, $3f
        mov     dh, [edi + edx]
        mov     dl, [edi + ecx]
        mov     ecx, eax
        shr     eax, 12
        shr     ecx, 18
        shl     edx, 16
        and     ecx, $3f
        and     eax, $3f
        mov     cl, [edi + ecx]
        mov     ch, [edi + eax]
        or      ecx, edx
        // write the 4 encoded bytes into rp
        mov     [esi], ecx
        add     esi, 4
        dec     ebp
        jnz     @1
@z:     pop     eax // result := len div 3
        pop     ebp
        pop     edi
        pop     esi
        pop     ebx
end;

procedure Base64uriEncodeTrailing(rp, sp: PAnsiChar; len: cardinal);
  {$ifdef HASINLINE}inline;{$endif}
var
  c: cardinal;
begin
  case len of
    1:
      begin
        c := ord(sp[0]) shl 4;
        rp[0] := b64urienc[(c shr 6) and $3f];
        rp[1] := b64urienc[c and $3f];
      end;
    2:
      begin
        c := ord(sp[0]) shl 10 + ord(sp[1]) shl 2;
        rp[0] := b64urienc[(c shr 12) and $3f];
        rp[1] := b64urienc[(c shr 6) and $3f];
        rp[2] := b64urienc[c and $3f];
      end;
  end;
end;

procedure Base64uriEncode(rp, sp: PAnsiChar; len: cardinal);
var
  main: cardinal;
begin
  main := Base64uriEncodeMain(rp, sp, len);
  Base64uriEncodeTrailing(rp + main * 4, sp + main * 3, len - main * 3);
end;

{$else}

procedure Base64uriEncode(rp, sp: PAnsiChar; len: cardinal);
var
  main, c: cardinal;
  enc: PBase64Enc; // faster especially on x86_64 and PIC
begin
  enc := @b64Urienc;
  main := len div 3;
  if main <> 0 then
  begin
    dec(len, main * 3); // fast modulo
    repeat
      c := (ord(sp[0]) shl 16) or (ord(sp[1]) shl 8) or ord(sp[2]);
      rp[0] := enc[(c shr 18) and $3f];
      rp[1] := enc[(c shr 12) and $3f];
      rp[2] := enc[(c shr 6) and $3f];
      rp[3] := enc[c and $3f];
      inc(rp, 4);
      inc(sp, 3);
      dec(main)
    until main = 0;
  end;
  case len of
    1:
      begin
        c := ord(sp[0]) shl 4;
        rp[0] := enc[(c shr 6) and $3f];
        rp[1] := enc[c and $3f];
      end;
    2:
      begin
        c := (ord(sp[0]) shl 10) or (ord(sp[1]) shl 2);
        rp[0] := enc[(c shr 12) and $3f];
        rp[1] := enc[(c shr 6) and $3f];
        rp[2] := enc[c and $3f];
      end;
  end;
end;

{$endif ASMX86}

function BinToBase64uriLength(len: PtrUInt): PtrUInt;
begin
  result := (len div 3) * 4;
  case len - (result shr 2) * 3 of // fast len mod 3
    1:
      inc(result, 2);
    2:
      inc(result, 3);
  end;
end;

function BinToBase64uri(const s: RawByteString): RawUtf8;
var
  len: integer;
begin
  result := '';
  len := length(s);
  if len = 0 then
    exit;
  FastSetString(result, nil, BinToBase64uriLength(len));
  Base64uriEncode(pointer(result), pointer(s), len);
end;

function BinToBase64uri(Bin: PAnsiChar; BinBytes: integer): RawUtf8;
begin
  result := '';
  if BinBytes <= 0 then
    exit;
  FastSetString(result, nil, BinToBase64uriLength(BinBytes));
  Base64uriEncode(pointer(result), Bin, BinBytes);
end;

function BinToBase64uriShort(Bin: PAnsiChar; BinBytes: integer): shortstring;
var
  len: integer;
begin
  result := '';
  if BinBytes <= 0 then
    exit;
  len := BinToBase64uriLength(BinBytes);
  if len > 255 then
    exit;
  byte(result[0]) := len;
  Base64uriEncode(@result[1], Bin, BinBytes);
end;

function Base64uriToBinLength(len: PtrInt): PtrInt;
begin
  if len = 0 then
    result := 0
  else
  begin
    result := (len shr 2) * 3;
    case len and 3 of
      1:
        result := 0;
      2:
        inc(result, 1);
      3:
        inc(result, 2);
    end;
  end;
end;

function Base64uriDecode(sp, rp: PAnsiChar; len: PtrInt): boolean;
begin
  result := Base64AnyDecode(ConvertBase64UriToBin, sp, rp, len);
end;

function Base64uriToBin(sp: PAnsiChar; len: PtrInt): RawByteString;
begin
  Base64uriToBin(sp, len, result);
end;

function Base64uriToBin(const s: RawByteString): RawByteString;
begin
  Base64uriToBin(pointer(s), length(s), result);
end;

procedure Base64uriToBin(sp: PAnsiChar; len: PtrInt; var result: RawByteString);
var
  resultLen: PtrInt;
begin
  resultLen := Base64uriToBinLength(len);
  if resultLen <> 0 then
  begin
    SetString(result, nil, resultLen);
    if Base64AnyDecode(ConvertBase64UriToBin, sp, pointer(result), len) then
      exit;
  end;
  result := '';
end;

function Base64uriToBin(sp: PAnsiChar; len: PtrInt; var temp: TSynTempBuffer): boolean;
begin
  temp.Init(Base64uriToBinLength(len));
  result := (temp.len > 0) and
            Base64AnyDecode(ConvertBase64UriToBin, sp, temp.buf, len);
end;

function Base64uriToBin(const base64: RawByteString; bin: PAnsiChar; binlen: PtrInt): boolean;
begin
  result := Base64uriToBin(pointer(base64), bin, length(base64), binlen);
end;

function Base64uriToBin(base64, bin: PAnsiChar; base64len, binlen: PtrInt): boolean;
var
  resultLen: PtrInt;
begin
  resultLen := Base64uriToBinLength(base64len);
  result := (resultLen = binlen) and
            Base64AnyDecode(ConvertBase64UriToBin, base64, bin, base64len);
end;

procedure Base64ToUri(var base64: RawUtf8);
var
  P: PUtf8Char;
begin
  P := UniqueRawUtf8(base64);
  if P <> nil then
    repeat
      case P^ of
        #0:
          break;
        '+':
          P^ := '-';
        '/':
          P^ := '_';
        '=':
          begin
            // trim unsignificant trailing '=' characters
            SetLength(base64, P - pointer(base64));
            break;
          end;
      end;
      inc(P);
    until false;
end;

procedure Base64MagicDecode(var ParamValue: RawUtf8);
var
  tmp: RawUtf8;
begin
  tmp := ParamValue;
  if not Base64ToBinSafe(PAnsiChar(pointer(tmp)) + 3, length(tmp) - 3,
          RawByteString(ParamValue)) then
    ParamValue := '';
end;

function Base64MagicCheckAndDecode(Value: PUtf8Char; var Blob: RawByteString): boolean;
var
  ValueLen: integer;
begin
  if (Value = nil) or
     (Value[0] = #0) or
     (Value[1] = #0) or
     (Value[2] = #0) or
     (PCardinal(Value)^ and $ffffff <> JSON_BASE64_MAGIC_C) then
    result := false
  else
  begin
    ValueLen := StrLen(Value) - 3;
    if ValueLen > 0 then
      result := Base64ToBinSafe(PAnsiChar(Value) + 3, ValueLen, Blob)
    else
      result := false;
  end;
end;

function Base64MagicCheckAndDecode(Value: PUtf8Char; var Blob: TSynTempBuffer;
  ValueLen: integer): boolean;
begin
  if (Value = nil) or
     (Value[0] = #0) or
     (Value[1] = #0) or
     (Value[2] = #0) or
     (PCardinal(Value)^ and $ffffff <> JSON_BASE64_MAGIC_C) then
    result := false
  else
  begin
    if ValueLen = 0 then
      ValueLen := StrLen(Value);
    dec(ValueLen, 3);
    if ValueLen > 0 then
      result := Base64ToBin(PAnsiChar(Value) + 3, ValueLen, Blob)
    else
      result := false;
  end;
end;

function Base64MagicCheckAndDecode(Value: PUtf8Char; ValueLen: integer;
  var Blob: RawByteString): boolean;
begin
  if (ValueLen < 4) or
     (PCardinal(Value)^ and $ffffff <> JSON_BASE64_MAGIC_C) then
    result := false
  else
    result := Base64ToBinSafe(PAnsiChar(Value) + 3, ValueLen - 3, Blob);
end;


{ --------- Base58 encoding/decoding }

type
  TBase58Enc = array[0..57] of AnsiChar;
  PBase58Enc = ^TBase58Enc;
  TBase58Dec = array[AnsiChar] of shortint;
  PBase58Dec = ^TBase58Dec;

const
  b58enc: TBase58Enc =
    '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz';
var
  /// a conversion table from Base58 text into binary data
  ConvertBase58ToBin: TBase58Dec;

function BinToBase58(Bin: PAnsiChar; BinLen: integer; var Dest: TSynTempBuffer): integer;
var
  P, PEnd, P2: PByte;
  len, c, carry, i: cardinal;
begin
  result := 0;
  if (Bin = nil) or
     (BinLen <= 0) then
  begin
    Dest.buf := nil;
    exit;
  end;
  while Bin^ = #0 do
  begin
    inc(result); // any leading zero is stored as '1' -> result = num of zeros
    inc(Bin);
    dec(BinLen);
    if BinLen = 0 then
      break;
  end;
  P := Dest.InitZero(result + integer(cardinal(BinLen * 138) div 100));
  PEnd := @PByteArray(P)[Dest.len];
  if result <> 0 then
  begin
    FillcharFast(P^, result, ord('1'));
    inc(P, result);
  end;
  if BinLen = 0 then
    exit;
  len := 0;
  repeat
    // this loop is O(n2) by definition so BinLen should remain small
    i := 0;
    P2 := PEnd;
    carry := PByte(Bin)^;
    while (PtrUInt(P2) >= PtrUInt(P)) and
          ((carry <> 0) or
           (i < len)) do
    begin
      inc(carry, cardinal(P2^) shl 8);
      c := carry div 58;   // FPC will use fast reciprocal mul by 0x8d3dcb09
      dec(carry, c * 58);
      P2^ := carry;        // P2^ := carry mod 58
      carry := c;
      dec(P2);
      inc(i);
    end;
    len := i;
    inc(Bin);
    dec(BinLen);
  until BinLen = 0;
  inc(PEnd);
  P2 := P;
  while (P2 <> PEnd) and
        (P2^ = 0) do
    inc(P2);
  inc(result, PtrUInt(PEnd) - PtrUInt(P2));
  while P2 <> PEnd do
  begin
    P^ := ord(b58enc[P2^]);
    inc(P);
    inc(P2);
  end;
end;

function BinToBase58(Bin: PAnsiChar; BinLen: integer): RawUtf8;
var
  temp: TSynTempBuffer;
  len: integer;
begin
  len := BinToBase58(Bin, BinLen, temp);
  FastSetString(result, temp.buf, len);
  temp.Done;
end;

function BinToBase58(const Bin: RawByteString): RawUtf8;
begin
  result := BinToBase58(pointer(Bin), length(Bin));
end;

function Base58ToBin(B58: PAnsiChar; B58Len: integer;
  var Dest: TSynTempBuffer): integer;
var
  P: PByteArray;
  PEnd, P2: PByte;
  zeros, carry: integer;
begin
  result := 0; // means void or error
  if (B58 = nil) or
     (B58Len <= 0) then
  begin
    Dest.buf := nil;
    exit;
  end;
  zeros := 0;
  while B58^ = '1' do
  begin
    inc(zeros);
    inc(B58);
    dec(B58Len);
    if B58Len = 0 then
      break;
  end;
  P := Dest.InitZero(zeros + integer(cardinal(B58Len * 733) div 1000));
  PEnd := @P[Dest.len];
  if B58Len = 0 then
  begin
    result := zeros;
    exit;
  end;
  repeat
    // this loop is O(n2) by definition so B58Len should remain small
    carry := ConvertBase58ToBin[B58^];
    inc(B58);
    if carry < 0 then
      exit; // invalid input
    P2 := PEnd;
    while PtrUInt(P2) >= PtrUInt(P) do
    begin
      inc(carry, 58 * P2^);
      P2^ := carry;
      carry := carry shr 8;
      dec(P2);
    end;
    dec(B58Len);
  until B58Len = 0;
  P2 := pointer(P);
  while (P2 <> PEnd) and
        (P2^ = 0) do
    inc(P2);
  result := PtrUInt(PEnd) - PtrUInt(P2) + 1;
  if result + zeros <> Dest.len + 1 then
    MoveFast(P[PtrUInt(P2) - PtrUInt(P)], P[zeros], result);
  inc(result, zeros);
end;

function Base58ToBin(B58: PAnsiChar; B58Len: integer): RawByteString;
var
  temp: TSynTempBuffer;
  len: integer;
begin
  len := Base58ToBin(B58, B58Len, temp);
  SetString(result, PAnsiChar(temp.buf), len);
  temp.Done;
end;

function Base58ToBin(const base58: RawUtf8): RawByteString;
begin
  result := Base58ToBin(pointer(base58), length(base58));
end;

function BlobToRawBlob(P: PUtf8Char; Len: integer): RawBlob;
begin
  BlobToRawBlob(P, result, Len);
end;

procedure BlobToRawBlob(P: PUtf8Char; var result: RawBlob; Len: integer);
var
  LenHex: integer;
begin
  result := '';
  if Len = 0 then
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
    else if (PInteger(P)^ and $00ffffff = JSON_BASE64_MAGIC_C) and
       Base64ToBinSafe(@P[3], Len - 3, RawByteString(result)) then
      exit; // safe decode Base-64 content ('\uFFF0base64encodedbinary')
  // TEXT format
  SetString(result, PAnsiChar(P), Len);
end;

function BlobToRawBlob(const Blob: RawByteString): RawBlob;
var
  Len, LenHex: integer;
  P: PUtf8Char;
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
    else if (PInteger(P)^ and $00ffffff = JSON_BASE64_MAGIC_C) and
        Base64ToBinSafe(@P[3], Len - 3, RawByteString(result)) then
      exit; // safe decode Base-64 content ('\uFFF0base64encodedbinary')
  // TEXT format
  result := Blob;
end;

function BlobToStream(P: PUtf8Char): TStream;
begin
  result := TRawByteStringStream.Create(BlobToRawBlob(P));
end;

function BlobToBytes(P: PUtf8Char): TBytes;
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
    else if (PInteger(P)^ and $00ffffff = JSON_BASE64_MAGIC_C) and
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

function RawBlobToBlob(const RawBlob: RawBlob): RawUtf8;
// BLOB literals are string literals containing hexadecimal data and
//  preceded by a single "x" or "X" character. For example: X'53514C697465'
begin
  result := RawBlobToBlob(pointer(RawBlob), length(RawBlob));
end;

function RawBlobToBlob(RawBlob: pointer; RawBlobLength: integer): RawUtf8;
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

function isBlobHex(P: PUtf8Char): boolean;
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

procedure Base64MagicToBlob(Base64: PUtf8Char; var result: RawUtf8);
begin
  // do not escape the result: returns e.g. X'53514C697465'
  result := RawBlobToBlob(Base64ToBin(PAnsiChar(Base64), StrLen(Base64)));
end;



{ --------- MultiPart encoding/decoding }

function MultiPartFormDataDecode(const MimeType, Body: RawUtf8;
  var MultiPart: TMultiPartDynArray): boolean;
var
  boundary, endBoundary: RawUtf8;
  i, j, n: integer;
  P: PUtf8Char;
  part: TMultiPart;

  function GetBoundary(const line: RawUtf8): boolean;
  var
    i: integer;
  begin
    result := false;
    i := PosEx('boundary=', line);
    if i = 0 then
      exit;
    TrimCopy(line, i + 9, 200, boundary);
    if (boundary <> '') and
       (boundary[1] = '"') then
      // "boundary" -> boundary
      boundary := copy(boundary, 2, length(boundary) - 2);
    boundary := '--' + boundary;
    endBoundary := boundary + '--' + #13#10;
    boundary := boundary + #13#10;
    result := true;
  end;

begin
  result := false;
  if not GetBoundary(MimeType) then
    exit;
  i := PosEx(boundary{%H-}, Body);
  if i <> 0 then
    repeat
      inc(i, length(boundary));
      if i = length(Body) then
        exit; // reached the (premature) end
      P := PUtf8Char(Pointer(Body)) + i - 1;
      Finalize(part);
      // decode section header
      repeat
        if IdemPChar(P, 'CONTENT-DISPOSITION: ') then
        begin
          inc(P, 21);
          if IdemPCharAndGetNextItem(P, 'FORM-DATA; NAME="', part.Name, '"') then
            IdemPCharAndGetNextItem(P, '; FILENAME="', part.FileName, '"')
          else if IdemPChar(P, 'FILE; ') then
          begin
            inc(P, 6);
            IdemPCharAndGetNextItem(P, 'NAME="', part.Name, '"');
            if P^ = ';' then
              P := GotoNextNotSpace(P + 1);
            IdemPCharAndGetNextItem(P, 'FILENAME="', part.FileName, '"');
          end;
        end
        else if IdemPCharAndGetNextItem(P, 'CONTENT-TYPE: ', part.ContentType) then
        begin
          if IdemPChar(pointer(part.ContentType), 'MULTIPART/MIXED') then
            if GetBoundary(part.ContentType) then
              part.ContentType := 'files'
            else
              exit;
        end
        else
          IdemPCharAndGetNextItem(P, 'CONTENT-TRANSFER-ENCODING: ', part.Encoding);
        P := GotoNextLine(P);
        if P = nil then
          exit;
      until PWord(P)^ = 13 + 10 shl 8;
      // decode section content
      i := P - PUtf8Char(Pointer(Body)) + 3; // i = just after header
      j := PosEx(boundary, Body, i);
      if j = 0 then
      begin
        j := PosEx(endBoundary{%H-}, Body, i); // try last boundary
        if j = 0 then
          exit;
        result := true; // content seems well formatted enough
      end;
      if part.ContentType <> 'files' then
      begin
        part.Content := copy(Body, i, j - i - 2); // -2 to ignore trailing #13#10
        if (part.ContentType = '') or
           (PosEx('-8', part.ContentType) > 0) then
        begin
          if IdemPChar(pointer(part.ContentType), JSON_CONTENT_TYPE_UPPER) then
            part.ContentType := JSON_CONTENT_TYPE
          else
            part.ContentType := TEXT_CONTENT_TYPE;
          {$ifdef HASCODEPAGE}
          SetCodePage(part.Content, CP_UTF8, false); // ensure raw value is UTF-8
          {$endif HASCODEPAGE}
        end;
        if IdemPropNameU(part.Encoding, 'base64') then
          part.Content := Base64ToBin(part.Content);
        // note: "quoted-printable" not yet handled here
        n := length(MultiPart);
        SetLength(MultiPart, n + 1);
        MultiPart[n] := part;
      end;
      i := j;
    until result;
end;

function MultiPartFormDataNewBound(var boundaries: TRawUtf8DynArray): RawUtf8;
var
  random: array[0..2] of cardinal;
begin
  RandomBytes(@random, SizeOf(random));
  result := BinToBase64uri(@random, SizeOf(random));
  AddRawUtf8(boundaries, result);
end;

function MultiPartFormDataEncode(const MultiPart: TMultiPartDynArray;
  var MultiPartContentType, MultiPartContent: RawUtf8;
  Rfc2388NestedFiles: boolean): boolean;
var
  len, filescount, i: integer;
  boundaries: TRawUtf8DynArray;
  bound: RawUtf8;
  W: TBaseWriter;
  temp: TTextWriterStackBuffer;
begin
  result := false;
  len := length(MultiPart);
  if len = 0 then
    exit;
  filescount := 0;
  W := TBaseWriter.CreateOwnedStream(temp);
  try
    // header - see https://www.w3.org/Protocols/rfc1341/7_2_Multipart.html
    bound := MultiPartFormDataNewBound(boundaries);
    MultiPartContentType :=
      'Content-Type: multipart/form-data; boundary=' + bound;
    for i := 0 to len - 1 do
      with MultiPart[i] do
      begin
        if FileName = '' then
          // simple name/value form section
          W.Add('--%'#13#10'Content-Disposition: form-data; name="%"'#13#10 +
            'Content-Type: %'#13#10#13#10'%'#13#10,
            [bound, Name, ContentType, Content])
        else
        begin
          // if this is the first file, create the RFC 2388 nested "files"
          if Rfc2388NestedFiles and
             (filescount = 0) then
          begin
            W.Add('--%'#13#10, [bound]);
            bound := MultiPartFormDataNewBound(boundaries);
            W.Add('Content-Disposition: form-data; name="files"'#13#10 +
              'Content-Type: multipart/mixed; boundary=%'#13#10#13#10, [bound]);
            W.Add('--%'#13#10'Content-Disposition: file; filename="%"'#13#10 +
              'Content-Type: %'#13#10, [bound, FileName, ContentType]);
          end
          else
            // see https://tools.ietf.org/html/rfc7578#appendix-A
            W.Add('--%'#13#10 +
              'Content-Disposition: form-data; name="%"; filename="%"'#13#10 +
              'Content-Type: %'#13#10,
              [bound, Name, FileName, ContentType]);
          if Encoding <> '' then
            W.Add('Content-Transfer-Encoding: %'#13#10, [Encoding]);
          W.AddCR;
          W.AddString(MultiPart[i].Content);
          W.AddCR;
          inc(filescount);
        end;
      end;
    // footer multipart
    for i := length(boundaries) - 1 downto 0 do
      W.Add('--%--'#13#10, [boundaries[i]]);
    W.SetText(MultiPartContent);
    result := True;
  finally
    W.Free;
  end;
end;

function MultiPartFormDataAddFile(const FileName: TFileName;
  var MultiPart: TMultiPartDynArray; const Name: RawUtf8): boolean;
var
  part: TMultiPart;
  newlen: integer;
  content: RawByteString;
begin
  result := false;
  content := StringFromFile(FileName);
  if content = '' then
    exit;
  newlen := length(MultiPart) + 1;
  if Name = '' then
    FormatUtf8('File%', [newlen], part.Name)
  else
    part.Name := Name;
  part.FileName := StringToUtf8(ExtractFileName(FileName));
  part.ContentType := GetMimeContentType(pointer(content), length(content), FileName);
  part.Encoding := 'base64';
  part.Content := BinToBase64(content);
  SetLength(MultiPart, newlen);
  MultiPart[newlen - 1] := part;
  result := true;
end;

function MultiPartFormDataAddField(const FieldName, FieldValue: RawUtf8;
  var MultiPart: TMultiPartDynArray): boolean;
var
  part: TMultiPart;
  newlen: integer;
begin
  result := false;
  if FieldName = '' then
    exit;
  newlen := length(MultiPart) + 1;
  part.Name := FieldName;
  part.ContentType := GetMimeContentTypeFromBuffer(
    pointer(FieldValue), length(FieldValue), TEXT_CONTENT_TYPE);
  part.Content := FieldValue;
  SetLength(MultiPart, newlen);
  MultiPart[newlen - 1] := part;
  result := true;
end;


{ --------- Baudot encoding/decoding }

const
  // see https://en.wikipedia.org/wiki/Baudot_code
  Baudot2Char: array[0..63] of AnsiChar =
   #0'e'#10'a siu'#13'drjnfcktzlwhypqobg'#254'mxv'#255+
   #0'3'#10'- ''87'#13#0'4'#0',!:(5+)2$6019?@'#254'./;'#255;
var
  Char2Baudot: array[AnsiChar] of byte;

function AsciiToBaudot(const Text: RawUtf8): RawByteString;
begin
  result := AsciiToBaudot(pointer(Text), length(Text));
end;

function AsciiToBaudot(P: PAnsiChar; len: PtrInt): RawByteString;
var
  i: PtrInt;
  c, d, bits: integer;
  shift: boolean;
  dest: PByte;
  tmp: TSynTempBuffer;
begin
  result := '';
  if (P = nil) or
     (len = 0) then
    exit;
  shift := false;
  dest := tmp.Init((len * 10) shr 3);
  d := 0;
  bits := 0;
  for i := 0 to len - 1 do
  begin
    c := Char2Baudot[P[i]];
    if c > 32 then
    begin
      if not shift then
      begin
        d := (d shl 5) or 27;
        inc(bits, 5);
        shift := true;
      end;
      d := (d shl 5) or (c - 32);
      inc(bits, 5);
    end
    else if c > 0 then
    begin
      if shift and
         (P[i] >= ' ') then
      begin
        d := (d shl 5) or 31;
        inc(bits, 5);
        shift := false;
      end;
      d := (d shl 5) or c;
      inc(bits, 5);
    end;
    while bits >= 8 do
    begin
      dec(bits, 8);
      dest^ := d shr bits;
      inc(dest);
    end;
  end;
  if bits > 0 then
  begin
    dest^ := d shl (8 - bits);
    inc(dest);
  end;
  SetString(result, PAnsiChar(tmp.buf), PAnsiChar(dest) - PAnsiChar(tmp.buf));
  tmp.Done;
end;

function BaudotToAscii(const Baudot: RawByteString): RawUtf8;
begin
  result := BaudotToAscii(pointer(Baudot), length(Baudot));
end;

function BaudotToAscii(Baudot: PByteArray; len: PtrInt): RawUtf8;
var
  i: PtrInt;
  c, b, bits, shift: integer;
  tmp: TSynTempBuffer;
  dest: PAnsiChar;
begin
  result := '';
  if (Baudot = nil) or
     (len <= 0) then
    exit;
  dest := tmp.Init((len shl 3) div 5);
  try
    shift := 0;
    b := 0;
    bits := 0;
    for i := 0 to len - 1 do
    begin
      b := (b shl 8) or Baudot[i];
      inc(bits, 8);
      while bits >= 5 do
      begin
        dec(bits, 5);
        c := (b shr bits) and 31;
        case c of
          27:
            if shift <> 0 then
              exit
            else
              shift := 32;
          31:
            if shift <> 0 then
              shift := 0
            else
              exit;
        else
          begin
            c := ord(Baudot2Char[c + shift]);
            if c = 0 then
              if Baudot[i + 1] = 0 then // allow triming of last 5 bits
                break
              else
                exit;
            dest^ := AnsiChar(c);
            inc(dest);
          end;
        end;
      end;
    end;
  finally
    tmp.Done(dest, result);
  end;
end;



{ ***************** URI-Encoded Text Buffer Process }

function UrlEncode(const svar: RawUtf8): RawUtf8;
begin
  result := UrlEncode(pointer(svar));
end;

// two sub-functions for better code generation of UrlEncode()

procedure _UrlEncode_Write(s, p: PByte; tab: PTextByteSet);
var
  c: cardinal;
  hex: ^TByteToWord;
begin
  hex := @TwoDigitsHexWB;
  repeat
    c := s^;
    inc(s);
    if tcUriUnreserved in tab[c] then
    begin
      // was ['_', '-', '.', '~', '0'..'9', 'a'..'z', 'A'..'Z']
      p^ := c;
      inc(p);
    end
    else if c = 0 then
      exit
    else if c = 32 then
    begin
      p^ := ord('+');
      inc(p);
    end
    else
    begin
      p^ := ord('%');
      inc(p);
      PWord(p)^ := hex[c];
      inc(p, 2);
    end;
  until false;
end;

function _UrlEncode_ComputeLen(s: PByte; tab: PTextByteSet): PtrInt;
var
  c: cardinal;
begin
  result := 0;
  repeat
    c := s^;
    inc(s);
    if (tcUriUnreserved in tab[c]) or
       (c = 32) then
    begin
      inc(result);
      continue;
    end;
    if c = 0 then
      exit;
    inc(result, 3);
  until false;
end;

function UrlEncode(Text: PUtf8Char): RawUtf8;
begin
  result := '';
  if Text = nil then
    exit;
  FastSetString(result, nil, _UrlEncode_ComputeLen(pointer(Text), @TEXT_CHARS));
  _UrlEncode_Write(pointer(Text), pointer(result), @TEXT_BYTES);
end;

function UrlEncode(const NameValuePairs: array of const): RawUtf8;
// (['select','*','where','ID=12','offset',23,'object',aObject]);
var
  a, n: PtrInt;
  name, value: RawUtf8;
  p: PVarRec;
begin
  result := '';
  n := high(NameValuePairs);
  if (n > 0) and
     (n and 1 = 1) then
  begin
    for a := 0 to n shr 1 do
    begin
      VarRecToUtf8(NameValuePairs[a * 2], name);
      if not IsUrlValid(pointer(name)) then
        continue; // just skip invalid names
      p := @NameValuePairs[a * 2 + 1];
      if p^.VType = vtObject then
        value := ObjectToJson(p^.VObject, [])
      else
        VarRecToUtf8(p^, value);
      result := result + '&' + name + '=' + UrlEncode(value);
    end;
    result[1] := '?';
  end;
end;

function IsUrlValid(P: PUtf8Char): boolean;
var
  tab: PTextCharSet;
begin
  result := false;
  if P = nil then
    exit;
  tab := @TEXT_CHARS;
  repeat
    if tcUriUnreserved in tab[P^] then
      inc(P) // was  ['_', '-', '.', '~', '0'..'9', 'a'..'z', 'A'..'Z']
    else
      exit;
  until P^ = #0;
  result := true;
end;

function AreUrlValid(const Url: array of RawUtf8): boolean;
var
  i: integer;
begin
  result := false;
  for i := 0 to high(Url) do
    if not IsUrlValid(pointer(Url[i])) then
      exit;
  result := true;
end;

function IncludeTrailingUriDelimiter(const URI: RawByteString): RawByteString;
begin
  if (URI <> '') and
     (Uri[length(URI)] <> '/') then
    result := URI + '/'
  else
    result := URI;
end;

function UrlDecode(const s: RawUtf8; i, len: PtrInt): RawUtf8;
var
  L: PtrInt;
  P: PUtf8Char;
  tmp: TSynTempBuffer;
begin
  result := '';
  L := PtrInt(s);
  if L = 0 then
    exit;
  L := PStrLen(L - _STRLEN)^;
  if len < 0 then
    len := L;
  if i > L then
    exit;
  dec(i);
  if len = i then
    exit;
  P := tmp.Init(len - i);  // reserve enough space for result
  while i < len do
  begin
    case s[i + 1] of
      #0:
        break; // reached end of s
      '%':
        if not HexToChar(PAnsiChar(pointer(s)) + i + 1, P) then
          P^ := s[i + 1]
        else
          inc(i, 2); // browsers may not follow the RFC (e.g. encode % as % !)
      '+':
        P^ := ' ';
    else
      P^ := s[i + 1];
    end; // case s[i] of
    inc(i);
    inc(P);
  end;
  tmp.Done(P, result);
end;

function UrlDecode(U: PUtf8Char): RawUtf8;
var
  P: PUtf8Char;
  L: integer;
  tmp: TSynTempBuffer;
begin
  result := '';
  L := StrLen(U);
  if L = 0 then
    exit;
  P := tmp.Init(L);
  repeat
    case U^ of
      #0:
        break; // reached end of URI
      '%':
        if not HexToChar(PAnsiChar(U + 1), P) then
          P^ := U^
        else
          inc(U, 2); // browsers may not follow the RFC (e.g. encode % as % !)
      '+':
        P^ := ' ';
    else
      P^ := U^;
    end; // case s[i] of
    inc(U);
    inc(P);
  until false;
  tmp.Done(P, result);
end;

function UrlDecodeNextValue(U: PUtf8Char; out Value: RawUtf8): PUtf8Char;
var
  Beg, V: PUtf8Char;
  len: PtrInt;
begin
  if U <> nil then
  begin
    // compute resulting length of value
    Beg := U;
    len := 0;
    while (U^ <> #0) and
          (U^ <> '&') do
    begin
      if (U^ = '%') and
         HexToCharValid(PAnsiChar(U + 1)) then
        inc(U, 3)
      else
        inc(U);
      inc(len);
    end;
    // decode value content
    if len <> 0 then
    begin
      FastSetString(Value, nil, len);
      V := pointer(Value);
      U := Beg;
      repeat
        if (U^ = '%') and
           HexToChar(PAnsiChar(U + 1), V) then
        begin
          inc(V);
          inc(U, 3);
        end
        else
        begin
          if U^ = '+' then
            V^ := ' '
          else
            V^ := U^;
          inc(V);
          inc(U);
        end;
        dec(len);
      until len = 0;
    end;
  end;
  result := U;
end;

function UrlDecodeNextName(U: PUtf8Char; out Name: RawUtf8): PUtf8Char;
var
  Beg, V: PUtf8Char;
  len: PtrInt;
begin
  result := nil;
  if U = nil then
    exit;
  // compute resulting length of name
  Beg := U;
  len := 0;
  repeat
    case U^ of
      #0:
        exit;
      '=':
        begin
          result := U + 1;
          break;
        end;
      '%':
        if (U[1] = '3') and
           (U[2] in ['D', 'd']) then
        begin
          result := U + 3;
          break;  // %3d means = according to the RFC
        end
        else if HexToCharValid(PAnsiChar(U + 1)) then
          inc(U, 3)
        else
          inc(U);
    else
      inc(U);
    end;
    inc(len);
  until false;
  if len = 0 then
    exit;
  // decode name content
  FastSetString(Name, nil, len);
  V := pointer(Name);
  U := Beg;
  repeat
    if (U^ = '%') and
       HexToChar(PAnsiChar(U + 1), V) then
    begin
      inc(V);
      inc(U, 3);
    end
    else
    begin
      if U^ = '+' then
        V^ := ' '
      else
        V^ := U^;
      inc(V);
      inc(U);
    end;
    dec(len);
  until len = 0;
end;

function UrlDecodeNextNameValue(U: PUtf8Char; var Name, Value: RawUtf8): PUtf8Char;
begin
  result := nil;
  if U = nil then
    exit;
  U := UrlDecodeNextName(U, Name);
  if U = nil then
    exit;
  U := UrlDecodeNextValue(U, Value);
  if U^ = #0 then
    result := U
  else
    result := U + 1; // jump '&' to let decode the next name=value pair
end;

function UrlDecodeValue(U: PUtf8Char; const Upper: RawUtf8;
  var Value: RawUtf8; Next: PPUtf8Char): boolean;
begin
  // UrlDecodeValue('select=%2A&where=LastName%3D%27M%C3%B4net%27','SELECT=',V,@U)
  // -> U^='where=...' and V='*'
  result := false; // mark value not modified by default
  if U = nil then
  begin
    if Next <> nil then
      Next^ := U;
    exit;
  end;
  if IdemPChar(U, pointer(Upper)) then
  begin
    result := true;
    inc(U, length(Upper));
    U := UrlDecodeNextValue(U, Value);
  end;
  if Next = nil then
    exit;
  while not (U^ in [#0, '&']) do
    inc(U);
  if U^ = #0 then
    Next^ := nil
  else
    Next^ := U + 1; // jump '&'
end;

function UrlDecodeInteger(U: PUtf8Char; const Upper: RawUtf8;
  var Value: integer; Next: PPUtf8Char): boolean;
var
  V: PtrInt;
  SignNeg: boolean;
begin
  // UrlDecodeInteger('offset=20&where=LastName%3D%27M%C3%B4net%27','OFFSET=',O,@Next)
  // -> Next^='where=...' and O=20
  result := false; // mark value not modified by default
  if U = nil then
  begin
    if Next <> nil then
      Next^ := U;
    exit;
  end;
  if IdemPChar(U, pointer(Upper)) then
  begin
    inc(U, length(Upper));
    if U^ = '-' then
    begin
      SignNeg := True;
      Inc(U);
    end
    else
      SignNeg := false;
    if U^ in ['0'..'9'] then
    begin
      V := 0;
      repeat
        V := (V * 10) + ord(U^) - 48;
        inc(U);
      until not (U^ in ['0'..'9']);
      if SignNeg then
        Value := -V
      else
        Value := V;
      result := true;
    end;
  end;
  if Next = nil then
    exit;
  while not (U^ in [#0, '&']) do
    inc(U);
  if U^ = #0 then
    Next^ := nil
  else
    Next^ := U + 1; // jump '&'
end;

function UrlDecodeCardinal(U: PUtf8Char; const Upper: RawUtf8;
  var Value: cardinal; Next: PPUtf8Char): boolean;
var
  V: PtrInt;
begin
  // UrlDecodeInteger('offset=20&where=LastName%3D%27M%C3%B4net%27','OFFSET=',O,@Next)
  // -> Next^='where=...' and O=20
  result := false; // mark value not modified by default
  if U = nil then
  begin
    if Next <> nil then
      Next^ := U;
    exit;
  end;
  if IdemPChar(U, pointer(Upper)) then
  begin
    inc(U, length(Upper));
    if U^ in ['0'..'9'] then
    begin
      V := 0;
      repeat
        V := (V * 10) + ord(U^) - 48;
        inc(U);
      until not (U^ in ['0'..'9']);
      Value := V;
      result := true;
    end;
  end;
  if Next = nil then
    exit;
  while not (U^ in [#0, '&']) do
    inc(U);
  if U^ = #0 then
    Next^ := nil
  else
    Next^ := U + 1; // jump '&'
end;

function UrlDecodeInt64(U: PUtf8Char; const Upper: RawUtf8;
  var Value: Int64; Next: PPUtf8Char): boolean;
var
  tmp: RawUtf8;
begin
  result := UrlDecodeValue(U, Upper, tmp, Next);
  if result then
    SetInt64(pointer(tmp), Value);
end;

function UrlDecodeExtended(U: PUtf8Char; const Upper: RawUtf8;
  var Value: TSynExtended; Next: PPUtf8Char): boolean;
var
  tmp: RawUtf8;
  err: integer;
begin
  result := UrlDecodeValue(U, Upper, tmp, Next);
  if result then
  begin
    Value := GetExtended(pointer(tmp), err);
    if err <> 0 then
      result := false;
  end;
end;

function UrlDecodeDouble(U: PUtf8Char; const Upper: RawUtf8;
  var Value: double; Next: PPUtf8Char): boolean;
var
  tmp: RawUtf8;
  err: integer;
begin
  result := UrlDecodeValue(U, Upper, tmp, Next);
  if result then
  begin
    Value := GetExtended(pointer(tmp), err);
    if err <> 0 then
      result := false;
  end;
end;

function UrlDecodeNeedParameters(U, CsvNames: PUtf8Char): boolean;
var
  tmp: array[byte] of AnsiChar;
  L: integer;
  Beg: PUtf8Char;
// UrlDecodeNeedParameters('price=20.45&where=LastName%3D','price,where') will
// return TRUE
begin
  result := (CsvNames = nil);
  if result or
     (U = nil) then
    exit; // no parameter to check -> success; no input data -> error
  repeat
    L := 0;
    while (CsvNames^ <> #0) and
          (CsvNames^ <> ',') do
    begin
      tmp[L] := NormToUpper[CsvNames^];
      if L = high(tmp) then
        exit
      else // invalid CSV parameter
        inc(L);
      inc(CsvNames);
    end;
    if L = 0 then
      exit; // invalid CSV parameter
    PWord(@tmp[L])^ := ord('=');
    Beg := U;
    repeat
      if IdemPChar(U, tmp) then
        break;
      while not (U^ in [#0, '&']) do
        inc(U);
      if U^ = #0 then
        exit
      else // didn't find tmp in U
        inc(U); // Jump &
    until false;
    U := Beg;
    if CsvNames^ = #0 then
      Break
    else // no more parameter to check
      inc(CsvNames); // jump &
  until false;
  result := true; // all parameters found
end;


{ *********** Basic MIME Content Types Support }

function GetMimeContentTypeFromBuffer(Content: Pointer; Len: PtrInt;
  const DefaultContentType: RawUtf8): RawUtf8;
begin
  // see http://www.garykessler.net/library/file_sigs.html for magic numbers
  result := DefaultContentType;
  if (Content <> nil) and
     (Len > 4) then
    case PCardinal(Content)^ + 1 of // + 1 to avoid finding it in the exe
      $04034B50 + 1:
        result := 'application/zip'; // 50 4B 03 04
      $46445025 + 1:
        result := 'application/pdf'; //  25 50 44 46 2D 31 2E
      $21726152 + 1:
        result := 'application/x-rar-compressed'; // 52 61 72 21 1A 07 00
      $AFBC7A37 + 1:
        result := 'application/x-7z-compressed';  // 37 7A BC AF 27 1C
      $694C5153 + 1:
        result := 'application/x-sqlite3'; // SQlite format 3 = 53 51 4C 69
      $75B22630 + 1:
        result := 'audio/x-ms-wma'; // 30 26 B2 75 8E 66
      $9AC6CDD7 + 1:
        result := 'video/x-ms-wmv'; // D7 CD C6 9A 00 00
      $474E5089 + 1:
        result := 'image/png'; // 89 50 4E 47 0D 0A 1A 0A
      $38464947 + 1:
        result := 'image/gif'; // 47 49 46 38
      $46464F77 + 1:
        result := 'application/font-woff'; // wOFF in BigEndian
      $A3DF451A + 1:
        result := 'video/webm'; // 1A 45 DF A3 MKV Matroska stream file
      $002A4949 + 1,
      $2A004D4D + 1,
      $2B004D4D + 1:
        result := 'image/tiff'; // 49 49 2A 00 or 4D 4D 00 2A or 4D 4D 00 2B
      $46464952 + 1:
        if Len > 16 then // RIFF
          case PCardinalArray(Content)^[2] of
            $50424557:
              result := 'image/webp';
            $20495641:
              if PCardinalArray(Content)^[3] = $5453494C then
                result := 'video/x-msvideo'; // Windows Audio Video Interleave file
          end;
      $E011CFD0 + 1: // Microsoft Office applications D0 CF 11 E0=DOCFILE
        if Len > 600 then
          case PWordArray(Content)^[256] of // at offset 512
            $A5EC:
              result := 'application/msword'; // EC A5 C1 00
            $FFFD: // FD FF FF
              case PByteArray(Content)^[516] of
                $0E, $1C, $43:
                  result := 'application/vnd.ms-powerpoint';
                $10, $1F, $20, $22, $23, $28, $29:
                  result := 'application/vnd.ms-excel';
              end;
          end;
      $5367674F + 1:
        if Len > 14 then // OggS
          if (PCardinalArray(Content)^[1] = $00000200) and
             (PCardinalArray(Content)^[2] = $00000000) and
                 (PWordArray(Content)^[6] = $0000) then
            result := 'video/ogg';
      $1C000000 + 1:
        if Len > 12 then
          if PCardinalArray(Content)^[1] = $70797466 then  // ftyp
            case PCardinalArray(Content)^[2] of
              $6D6F7369, // isom: ISO Base Media file (MPEG-4) v1
              $3234706D: // mp42: MPEG-4 video/QuickTime file
                result := 'video/mp4';
              $35706733: // 3gp5: MPEG-4 video files
                result := 'video/3gpp';
            end;
    else
      case PCardinal(Content)^ and $00ffffff of
        $685A42:
          result := 'application/bzip2'; // 42 5A 68
        $088B1F:
          result := 'application/gzip'; // 1F 8B 08
        $492049:
          result := 'image/tiff'; // 49 20 49
        $FFD8FF:
          result := JPEG_CONTENT_TYPE; // FF D8 FF DB/E0/E1/E2/E3/E8
      else
        case PWord(Content)^ of
          $4D42:
            result := 'image/bmp'; // 42 4D
        end;
      end;
    end;
end;

function GetMimeContentType(Content: Pointer; Len: PtrInt;
  const FileName: TFileName; const DefaultContentType: RawUtf8): RawUtf8;
var
  ext: RawUtf8;
begin
  if FileName <> '' then
  begin
    // file extension is more precise -> check first
    result := '';
    ext := LowerCase(RawUtf8(ExtractFileExt(FileName)));
    delete(ext, 1, 1);
    if ext <> '' then
      case PosEx('-' + copy(ext, 1, 4) + '-',
          '-png-gif-tiff-jpg-jpeg-bmp-doc-htm-html-css-js-ico-wof-txt-svg-' +
          // 1   5   9    14  18   23  27  31  35   40  44 47  51  55  59
          'atom-rdf-rss-webp-appc-mani-docx-xml-json-woff-ogg-ogv-mp4-m2v-' +
          // 63  68  72  76   81   86   91   96  100  105  110 114 118 122
          'm2p-mp3-h264-text-log-gz-webm-mkv-rar-7z-tif-x-') of
          // 126 130 134 139 144 148 151 156 160 164 167 171
        1:
          result := 'image/png';
        5:
          result := 'image/gif';
        9, 167:
          result := 'image/tiff';
        14, 18:
          result := JPEG_CONTENT_TYPE;
        23:
          result := 'image/bmp';
        27, 91:
          result := 'application/msword';
        31, 35:
          result := HTML_CONTENT_TYPE;
        40:
          result := 'text/css';
        44: // text/javascript and application/x-javascript are obsolete (RFC 4329)
          result := 'application/javascript';
        47:
          result := 'image/x-icon';
        51, 105:
          result := 'application/font-woff';
        55, 139, 144:
          result := TEXT_CONTENT_TYPE;
        59:
          result := 'image/svg+xml';
        63, 68, 72, 96:
          result := XML_CONTENT_TYPE;
        76:
          result := 'image/webp';
        81, 86:
          result := 'text/cache-manifest';
        100:
          result := JSON_CONTENT_TYPE_VAR;
        110, 114:
          result := 'video/ogg';  // RFC 5334
        118:
          result := 'video/mp4';  // RFC 4337 6381
        122, 126:
          result := 'video/mp2';
        130:
          result := 'audio/mpeg'; // RFC 3003
        134:
          result := 'video/H264'; // RFC 6184
        148:
          result := 'application/gzip';
        151, 156:
          result := 'video/webm';
        160:
          result := 'application/x-rar-compressed';
        164:
          result := 'application/x-7z-compressed';
        171:
          result := 'application/x-compress';
      else
        if not (ext[1] in ['a'..'z']) then
          ext := '';
      end;
    if result <> '' then
      // we found the exact type from the file extension
      exit;
    if ext <> '' then
      // e.g. 'application/zip' or 'application/pdf'
      result := 'application/' + ext
    else
      result := DefaultContentType;
    result := GetMimeContentTypeFromBuffer(Content, Len, result);
  end
  else
    result := GetMimeContentTypeFromBuffer(Content, Len, DefaultContentType);
end;

function GetMimeContentTypeHeader(const Content: RawByteString;
  const FileName: TFileName): RawUtf8;
begin
  result := HEADER_CONTENT_TYPE +
    GetMimeContentType(Pointer(Content), length(Content), FileName);
end;

function IsContentCompressed(Content: Pointer; Len: PtrInt): boolean;
begin
  // see http://www.garykessler.net/library/file_sigs.html
  result := false;
  if (Content <> nil) and
     (Len > 8) then
    case PCardinal(Content)^ of // check magic DWORD
      $002a4949,
      $2a004d4d,
      $2b004d4d, // 'image/tiff'
      $04034b50, // 'application/zip' = 50 4B 03 04
      $184d2204, // LZ4 stream format = 04 22 4D 18
      $21726152, // 'application/x-rar-compressed' = 52 61 72 21 1A 07 00
      $28635349, // cab = 49 53 63 28
      $38464947, // 'image/gif' = 47 49 46 38
      $43614c66, // FLAC = 66 4C 61 43 00 00 00 22
      $4643534d, // cab = 4D 53 43 46 [MSCF]
      $46464952, // avi,webp,wav = 52 49 46 46 [RIFF]
      $46464f77, // 'application/font-woff' = wOFF in BigEndian
      $474e5089, // 'image/png' = 89 50 4E 47 0D 0A 1A 0A
      $4d5a4cff, // LZMA = FF 4C 5A 4D 41 00
      $72613c21, // .ar/.deb package file = '!<arch>' (assuming compressed)
      $75b22630, // 'audio/x-ms-wma' = 30 26 B2 75 8E 66
      $766f6f6d, // mov = 6D 6F 6F 76 [....moov]
      $89a8275f, // jar = 5F 27 A8 89
      $9ac6cdd7, // 'video/x-ms-wmv' = D7 CD C6 9A 00 00
      $a5a5a5a5, // .mab file = MAGIC_MAB in mormot.core.log.pas
      $a5aba5a5, // .data = TRESTSTORAGEINMEMORY_MAGIC in mormot.orm.server.pas
      LOG_MAGIC, // .log.synlz with SynLZ or Lizard compression
      $aba5a5ab, // .dbsynlz = SQLITE3_MAGIC in mormot.db.raw.sqlite3.pas
      $afbc7a37, // 'application/x-7z-compressed' = 37 7A BC AF 27 1C
      $b7010000,
      $ba010000, // mpeg = 00 00 01 Bx
      $cececece, // jceks = CE CE CE CE
      $dbeeabed, // .rpm package file
      $e011cfd0: // msi = D0 CF 11 E0 A1 B1 1A E1
        result := true;
    else
      case PCardinal(Content)^ and $00ffffff of // 24-bit magic
        $088b1f, // 'application/gzip' = 1F 8B 08
        $334449, // mp3 = 49 44 33 [ID3]
        $492049, // 'image/tiff' = 49 20 49
        $535746, // swf = 46 57 53 [FWS]
        $535743, // swf = 43 57 53 [zlib]
        $53575a, // zws/swf = 5A 57 53 [FWS]
        $564c46, // flv = 46 4C 56 [FLV]
        $685a42, // 'application/bzip2' = 42 5A 68
        $ffd8ff: // JPEG_CONTENT_TYPE = FF D8 FF DB/E0/E1/E2/E3/E8
          result := true;
      else
        case PCardinalArray(Content)^[1] of // ignore variable 4 byte offset
          $70797466, // mp4,mov = 66 74 79 70 [33 67 70 35/4D 53 4E 56..]
          $766f6f6d: // mov = 6D 6F 6F 76
            result := true;
        end;
      end;
    end;
end;

function GetJpegSize(jpeg: PAnsiChar; len: PtrInt; out Height, Width: integer): boolean;
var
  je: PAnsiChar;
begin
  // see https://en.wikipedia.org/wiki/JPEG#Syntax_and_structure
  result := false;
  if (jpeg = nil) or
     (len < 100) or
     (PWord(jpeg)^ <> $d8ff) then // SOI
    exit;
  je := jpeg + len - 8;
  inc(jpeg, 2);
  while jpeg < je do
  begin
    if jpeg^ <> #$ff then
      exit;
    inc(jpeg);
    case ord(jpeg^) of
      $c0..$c3, $c5..$c7, $c9..$cb, $cd..$cf: // SOF
        begin
          Height := swap(PWord(jpeg + 4)^);
          Width := swap(PWord(jpeg + 6)^);
          result := (Height > 0) and
                    (Height < 20000) and
                    (Width > 0) and
                    (Width < 20000);
          exit;
        end;
      $d0..$d8, $01: // RST, SOI
        inc(jpeg);
      $d9: // EOI
        break;
      $ff: // padding
        ;
    else
      inc(jpeg, swap(PWord(jpeg + 1)^) + 1);
    end;
  end;
end;


{ ************* Text Memory Buffers and Files }

{ TMemoryMapText }

constructor TMemoryMapText.Create;
begin
end;

constructor TMemoryMapText.Create(aFileContent: PUtf8Char; aFileSize: integer);
begin
  Create;
  fMap.Map(aFileContent, aFileSize);
  LoadFromMap;
end;

constructor TMemoryMapText.Create(const aFileName: TFileName);
begin
  Create;
  fFileName := aFileName;
  if fMap.Map(aFileName) then
    LoadFromMap;
end; // invalid file or unable to memory map its content -> Count := 0

destructor TMemoryMapText.Destroy;
begin
  Freemem(fLines);
  fMap.UnMap;
  inherited;
end;

procedure TMemoryMapText.SaveToStream(Dest: TStream; const Header: RawUtf8);
var
  i: PtrInt;
  W: TBaseWriter;
  temp: TTextWriterStackBuffer;
begin
  i := length(Header);
  if i > 0 then
    Dest.WriteBuffer(pointer(Header)^, i);
  if fMap.Size > 0 then
    Dest.WriteBuffer(fMap.Buffer^, fMap.Size);
  if fAppendedLinesCount = 0 then
    exit;
  W := TBaseWriter.Create(Dest, @temp, SizeOf(temp));
  try
    if (fMap.Size > 0) and
       (fMap.Buffer[fMap.Size - 1] >= ' ') then
      W.Add(#10);
    for i := 0 to fAppendedLinesCount - 1 do
    begin
      W.AddString(fAppendedLines[i]);
      W.Add(#10);
    end;
    W.FlushFinal;
  finally
    W.Free;
  end;
end;

procedure TMemoryMapText.SaveToFile(FileName: TFileName; const Header: RawUtf8);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(FS, Header);
  finally
    FS.Free;
  end;
end;

function TMemoryMapText.GetLine(aIndex: integer): RawUtf8;
begin
  if (self = nil) or
     (cardinal(aIndex) >= cardinal(fCount)) then
    result := ''
  else
    FastSetString(result, fLines[aIndex], GetLineSize(fLines[aIndex], fMapEnd));
end;

function TMemoryMapText.GetString(aIndex: integer): string;
begin
  if (self = nil) or
     (cardinal(aIndex) >= cardinal(fCount)) then
    result := ''
  else
    Utf8DecodeToString(fLines[aIndex], GetLineSize(fLines[aIndex], fMapEnd), result);
end;

function TMemoryMapText.LineContains(const aUpperSearch: RawUtf8;
  aIndex: integer): boolean;
begin
  if (self = nil) or
     (cardinal(aIndex) >= cardinal(fCount)) or
     (aUpperSearch = '') then
    result := false
  else
    result := GetLineContains(fLines[aIndex], fMapEnd, pointer(aUpperSearch));
end;

function TMemoryMapText.LineSize(aIndex: integer): integer;
begin
  result := GetLineSize(fLines[aIndex], fMapEnd);
end;

function TMemoryMapText.LineSizeSmallerThan(aIndex, aMinimalCount: integer): boolean;
begin
  result := GetLineSizeSmallerThan(fLines[aIndex], fMapEnd, aMinimalCount);
end;

procedure TMemoryMapText.ProcessOneLine(LineBeg, LineEnd: PUtf8Char);
begin
  if fCount = fLinesMax then
  begin
    fLinesMax := NextGrow(fLinesMax);
    ReallocMem(fLines, fLinesMax * SizeOf(pointer));
  end;
  fLines[fCount] := LineBeg;
  inc(fCount);
end;

procedure ParseLines(P, PEnd: PUtf8Char; Map: TMemoryMapText);
var
  PBeg: PUtf8Char;
begin
  // generated asm is much better with a local proc
  if P < PEnd then
  repeat
    PBeg := P;
    {$ifdef CPUX64}
    inc(P, BufferLineLength(P, PEnd)); // use branchless SSE2 on x86_64
    {$else}
    while (P < PEnd) and
          (P^ <> #13) and
          (P^ <> #10) do
      inc(P);
    {$endif CPUX64}
    Map.ProcessOneLine(PBeg, P);
    if P + 1 < PEnd then
      if PWord(P)^ = 13 + 10 shl 8 then
      begin
        inc(P, 2); // ignore #13#10
        if P < PEnd then
          continue;
      end
      else
      begin
        inc(P);    // ignore #13 or #10
        if P < PEnd then
          continue;
      end;
    break;
  until false;
end;

procedure TMemoryMapText.LoadFromMap(AverageLineLength: integer = 32);
var
  P: PUtf8Char;
begin
  fLinesMax := fMap.FileSize div AverageLineLength + 8;
  GetMem(fLines, fLinesMax * SizeOf(pointer));
  P := pointer(fMap.Buffer);
  fMapEnd := P + fMap.Size;
  if Map.TextFileKind = isUtf8 then
    inc(PByte(P), 3); // ignore UTF-8 BOM
  ParseLines(P, fMapEnd, self);
  if fLinesMax > fCount + 16384 then
    Reallocmem(fLines, fCount * SizeOf(pointer)); // size down only if worth it
end;

procedure TMemoryMapText.AddInMemoryLine(const aNewLine: RawUtf8);
var
  P: PUtf8Char;
begin
  if aNewLine = '' then
    exit;
  AddRawUtf8(fAppendedLines, fAppendedLinesCount, aNewLine);
  P := pointer(fAppendedLines[fAppendedLinesCount - 1]);
  ProcessOneLine(P, P + StrLen(P));
end;

procedure TMemoryMapText.AddInMemoryLinesClear;
begin
  dec(fCount, fAppendedLinesCount);
  fAppendedLinesCount := 0;
  fAppendedLines := nil;
end;


procedure AppendCharToRawUtf8(var Text: RawUtf8; Ch: AnsiChar);
var
  L: PtrInt;
begin
  L := length(Text);
  SetLength(Text, L + 1); // reallocate - most MM keep in-place with no move
  PByteArray(Text)[L] := ord(Ch);
end;

procedure AppendCharOnceToRawUtf8(var Text: RawUtf8; Ch: AnsiChar);
var
  L: PtrInt;
begin
  L := length(Text);
  if (L <> 0) and
     (Text[L] = Ch) then
    exit;
  SetLength(Text, L + 1);
  PByteArray(Text)[L] := ord(Ch);
end;

procedure AppendBufferToRawUtf8(var Text: RawUtf8; Buffer: pointer; BufferLen: PtrInt);
var
  L: PtrInt;
begin
  if BufferLen <= 0 then
    exit;
  L := length(Text);
  SetLength(Text, L + BufferLen);
  MoveFast(Buffer^, pointer(PtrInt(Text) + L)^, BufferLen);
end;

procedure AppendBuffersToRawUtf8(var Text: RawUtf8; const Buffers: array of PUtf8Char);
var
  i, len, TextLen: PtrInt;
  lens: array[0..63] of integer;
  P: PUtf8Char;
begin
  if high(Buffers) > high(lens) then
    raise ESynException.Create('Too many params in AppendBuffersToRawUtf8()');
  len := 0;
  for i := 0 to high(Buffers) do
  begin
    lens[i] := StrLen(Buffers[i]);
    inc(len, lens[i]);
  end;
  TextLen := Length(Text);
  SetLength(Text, TextLen + len);
  P := pointer(Text);
  inc(P, TextLen);
  for i := 0 to high(Buffers) do
    if Buffers[i] <> nil then
    begin
      MoveFast(Buffers[i]^, P^, {%H-}lens[i]);
      inc(P, lens[i]);
    end;
end;

function AppendRawUtf8ToBuffer(Buffer: PUtf8Char; const Text: RawUtf8): PUtf8Char;
var
  L: PtrInt;
begin
  L := length(Text);
  if L <> 0 then
  begin
    MoveFast(Pointer(Text)^, Buffer^, L);
    inc(Buffer, L);
  end;
  result := Buffer;
end;

function Append999ToBuffer(Buffer: PUtf8Char; Value: PtrUInt): PUtf8Char;
var
  L: PtrInt;
  P: PAnsiChar;
begin
  P := pointer(SmallUInt32Utf8[Value]);
  L := PStrLen(P - _STRLEN)^;
  MoveSmall(P, Buffer, L);
  result := Buffer + L;
end;

function AppendBufferToBuffer(Buffer: PUtf8Char; Text: pointer; Len: PtrInt): PUtf8Char;
begin
  MoveFast(Text^, Buffer^, Len);
  result := Buffer + Len;
end;

function AppendUInt32ToBuffer(Buffer: PUtf8Char; Value: PtrUInt): PUtf8Char;
var
  L: PtrInt;
  P: PAnsiChar;
  tmp: array[0..23] of AnsiChar;
begin
  if Value <= high(SmallUInt32Utf8) then
  begin
    P := pointer(SmallUInt32Utf8[Value]);
    L := PStrLen(P - _STRLEN)^;
  end
  else
  begin
    P := StrUInt32(@tmp[23], Value);
    L := @tmp[23] - P;
  end;
  MoveSmall(P, Buffer, L);
  result := Buffer + L;
end;

procedure AppendBufferToRawByteString(
  var Content: RawByteString; const Buffer; BufferLen: PtrInt);
var
  ContentLen: PtrInt;
begin
  if BufferLen <= 0 then
    exit;
  ContentLen := length(Content);
  SetLength(Content, ContentLen + BufferLen);
  MoveFast(Buffer, PByteArray(Content)^[ContentLen], BufferLen);
end;

function Plural(const itemname: shortstring; itemcount: cardinal): shortstring;
var
  len, L: PtrInt;
begin
  len := (AppendUInt32ToBuffer(@result[1], itemcount) - PUtf8Char(@result[1])) + 1;
  result[len] := ' ';
  L := ord(itemname[0]);
  if (L > 0) and
     (L <= 240) then
  begin
    // avoid buffer overflow
    MoveSmall(@itemname[1], @result[len + 1], L);
    inc(len, L);
    if itemcount > 1 then
    begin
      inc(len);
      result[len] := 's';
    end;
  end;
  result[0] := AnsiChar(len);
end;

function EscapeBuffer(s: PAnsiChar; slen: integer;
  d: PAnsiChar; dmax: integer): PAnsiChar;
var
  c: AnsiChar;
  tab: PWordArray;
begin
  if (slen > 0) and
     (dmax > 7) then
  begin
    tab := @TwoDigitsHexWBLower;
    repeat
      c := s^;
      inc(s);
      if (c >= ' ') and
         (c <= #126) then
      begin
        d^ := c;
        inc(d);
        dec(dmax);
      end
      else
      begin
        d^ := '$';
        inc(d);
        PWord(d)^ := tab[ord(c)];
        inc(d, 2);
        dec(dmax, 3);
      end;
      if dmax <= 7 then // mark truncated
      begin
        PCardinal(d)^ := ord('.') + ord('.') shl 8 + ord('.') shl 16;
        inc(d, 3);
        break;
      end;
      dec(slen);
    until slen = 0;
  end;
  d^ := #0;
  result := d;
end;

function LogEscape(source: PAnsiChar; sourcelen: integer;
  var temp: TLogEscape; enabled: boolean): PAnsiChar;
begin
  if enabled then
  begin
    temp[0] := ' ';
    EscapeBuffer(source, sourcelen, @temp[1], SizeOf(temp) - 1);
  end
  else
    temp[0] := #0;
  result := @temp;
end;

function LogEscapeFull(const source: RawByteString): RawUtf8;
begin
  result := LogEscapeFull(pointer(source), length(source));
end;

function LogEscapeFull(source: PAnsiChar; sourcelen: integer): RawUtf8;
begin
  FastSetString(result, nil, sourcelen * 3); // worse case
  if sourcelen = 0 then
    exit;
  sourcelen := EscapeBuffer(source, sourcelen,
    pointer(result), sourcelen * 3) - pointer(result);
  // don't call the MM which may move the data -> just adjust length()
  PStrLen(PAnsiChar(pointer(result)) - _STRLEN)^ := sourcelen;
end;

function EscapeToShort(source: PAnsiChar; sourcelen: integer): shortstring;
begin
  result[0] := AnsiChar(
    EscapeBuffer(source, sourcelen, @result[1], 255) - @result[1]);
end;

function EscapeToShort(const source: RawByteString): shortstring;
begin
  result[0] := AnsiChar(
    EscapeBuffer(pointer(source), length(source), @result[1], 255) - @result[1]);
end;

function AnyTextFileToSynUnicode(const FileName: TFileName; ForceUtf8: boolean): SynUnicode;
var
  Map: TMemoryMap;
begin
  result := '';
  if Map.Map(FileName) then
  try
    if ForceUtf8 then
      Utf8ToSynUnicode(PUtf8Char(Map.Buffer), Map.Size, result)
    else
      case Map.TextFileKind of
        isUnicode:
          SetString(result, PWideChar(PtrUInt(Map.Buffer) + 2),
            (Map.Size - 2) shr 1);
        isUtf8:
          Utf8ToSynUnicode(PUtf8Char(pointer(PtrUInt(Map.Buffer) + 3)),
            Map.Size - 3, result);
        isAnsi:
          result := CurrentAnsiConvert.AnsiToUnicodeString(Map.Buffer, Map.Size);
      end;
  finally
    Map.UnMap;
  end;
end;

function AnyTextFileToRawUtf8(const FileName: TFileName; AssumeUtf8IfNoBom: boolean): RawUtf8;
var
  Map: TMemoryMap;
begin
  result := '';
  if Map.Map(FileName) then
  try
    case Map.TextFileKind of
      isUnicode:
        RawUnicodeToUtf8(PWideChar(PtrUInt(Map.Buffer) + 2),
          (Map.Size - 2) shr 1, result);
      isUtf8:
        FastSetString(result, pointer(PtrUInt(Map.Buffer) + 3), Map.Size - 3);
      isAnsi:
        if AssumeUtf8IfNoBom then
          FastSetString(result, Map.Buffer, Map.Size)
        else
          result := CurrentAnsiConvert.AnsiBufferToRawUtf8(Map.Buffer, Map.Size);
    end;
  finally
    Map.UnMap;
  end;
end;

function AnyTextFileToString(const FileName: TFileName; ForceUtf8: boolean): string;
var
  Map: TMemoryMap;
begin
  result := '';
  if Map.Map(FileName) then
  try
    {$ifdef UNICODE}
    if ForceUtf8 then
      Utf8DecodeToString(PUtf8Char(Map.Buffer), Map.Size, result)
    else
      case Map.TextFileKind of
        isUnicode:
          SetString(result,
            PWideChar(PtrUInt(Map.Buffer) + 2), (Map.Size - 2) shr 1);
        isUtf8:
          Utf8DecodeToString(
            pointer(PtrUInt(Map.Buffer) + 3), Map.Size - 3, result);
        isAnsi:
          result := CurrentAnsiConvert.AnsiToUnicodeString(
            Map.Buffer, Map.Size);
      end;
    {$else}
    if ForceUtf8 then
      result := CurrentAnsiConvert.Utf8BufferToAnsi(
        PUtf8Char(Map.Buffer), Map.Size)
    else
      case Map.TextFileKind of
        isUnicode:
          result := CurrentAnsiConvert.UnicodeBufferToAnsi(
            PWideChar(PtrUInt(Map.Buffer) + 2), (Map.Size - 2) shr 1);
        isUtf8:
          result := CurrentAnsiConvert.Utf8BufferToAnsi(
            pointer(PtrUInt(Map.Buffer) + 3), Map.Size - 3);
        isAnsi:
          SetString(result, PAnsiChar(Map.Buffer), Map.Size);
      end;
    {$endif UNICODE}
  finally
    Map.UnMap;
  end;
end;


function BinToSource(const ConstName, Comment: RawUtf8;
  Data: pointer; Len, PerLine: integer; const Suffix: RawUtf8): RawUtf8;
var
  W: TBaseWriter;
  temp: TTextWriterStackBuffer;
begin
  if (Data = nil) or
     (Len <= 0) or
     (PerLine <= 0) then
    result := ''
  else
  begin
    W := TBaseWriter.CreateOwnedStream(temp,
      Len * 5 + 50 + length(Comment) + length(Suffix));
    try
      BinToSource(W, ConstName, Comment, Data, Len, PerLine);
      if Suffix <> '' then
      begin
        W.AddString(Suffix);
        W.AddCR;
      end;
      W.SetText(result);
    finally
      W.Free;
    end;
  end;
end;

procedure BinToSource(Dest: TBaseWriter; const ConstName, Comment: RawUtf8;
  Data: pointer; Len, PerLine: integer);
var
  line, i: integer;
  P: PByte;
begin
  if (Dest = nil) or
     (Data = nil) or
     (Len <= 0) or
     (PerLine <= 0) then
    exit;
  Dest.AddShorter('const');
  if Comment <> '' then
    Dest.Add(#13#10'  // %', [Comment]);
  Dest.Add(#13#10'  %: array[0..%] of byte = (', [ConstName, Len - 1]);
  P := pointer(Data);
  repeat
    if len > PerLine then
      line := PerLine
    else
      line := Len;
    Dest.AddShorter(#13#10'    ');
    for i := 1 to line do
    begin
      Dest.Add('$');
      Dest.AddByteToHex(P^);
      inc(P);
      Dest.AddComma;
    end;
    dec(Len,line);
  until Len = 0;
  Dest.CancelLastComma;
  Dest.Add(');'#13#10'  %_LEN = SizeOf(%);'#13#10, [ConstName, ConstName]);
end;


{ *************************** TStreamRedirect and other Hash process }

{ TStreamRedirect }

constructor TStreamRedirect.Create(aRedirected: TStream; aRead: boolean);
begin
  fRedirected := aRedirected;
  if aRead and
     Assigned(aRedirected) then
    SetExpectedSize(aRedirected.Size); // needed e.g. to upload a file
  fStartTix := GetTickCount64;
end;

destructor TStreamRedirect.Destroy;
begin
  fRedirected.Free;
  inherited Destroy;
end;

function TStreamRedirect.GetProgress: RawUtf8;
var
  ctx, remain: shortstring;
begin
  if (self = nil) or
     fTerminated then
  begin
    result := '';
    exit;
  end;
  Ansi7StringToShortString(Context, ctx);
  if ctx[0] > #30 then
  begin
    ctx[0] := #33;
    PCardinal(@ctx[30])^ := ord('.') + ord('.') shl 8 + ord('.') shl 16;
  end;
  if ExpectedSize = 0 then
    // size may not be known (e.g. server-side chunking)
    FormatUtf8('% % read %/s ...',
      [ctx, KBNoSpace(fCurrentSize), KBNoSpace(PerSecond)], result)
  else if fCurrentSize < ExpectedSize then
  begin
    // we can state the current progression ratio
    if Remaining <= 0 then
      remain := ''
    else
      FormatShort(' remaining:%', [MicroSecToString(Remaining * 1000)], remain);
    FormatUtf8('% %% %/% %/s%',
      [ctx, Percent, '%', KBNoSpace(fCurrentSize), KBNoSpace(ExpectedSize),
       KBNoSpace(PerSecond), remain], result)
  end
  else
    // process is finished
    FormatUtf8('% % done in % (%/s)' + CRLF,
      [Context, KBNoSpace(ExpectedSize), MicroSecToString(Elapsed * 1000),
       KBNoSpace(PerSecond)], result);
end;

function TStreamRedirect.GetSize: Int64;
begin
  if (fMode <> mWrite) and
     (fExpectedSize <> 0) then
    result := fExpectedSize
  else
    result := fCurrentSize;
end;

{$I-}
class procedure TStreamRedirect.ProgressToConsole(Sender: TStreamRedirect);
var
  eraseline: shortstring;
  msg: RawUtf8;
begin
  eraseline[0] := AnsiChar(Sender.fConsoleLen + 2);
  eraseline[1] := #13;
  FillCharFast(eraseline[2], ord(eraseline[0]) - 2, 32);
  eraseline[ord(eraseline[0])] := #13;
  system.write(eraseline);
  msg := Sender.GetProgress;
  Sender.fConsoleLen := length(msg);
  system.write(msg);
  ioresult;
end;
{$I+}

procedure TStreamRedirect.DoReport(ReComputeElapsed: boolean);
begin
  if ReComputeElapsed then
    fElapsed := GetTickCount64 - fStartTix; // may have changed in-between
  if (fCurrentSize <> fExpectedSize) and
     (fElapsed < fReportTix) then
    exit;
  fReportTix := fElapsed + 1000; // notify once per second or when finished
  if fExpectedSize = 0 then
    fPercent := 0
  else if fCurrentSize >= fExpectedSize then
  begin
    fPercent := 100;
    fRemaining := 0;
  end
  else
  begin
    if (fElapsed <> 0) and
       (fProcessedSize <> 0) then
      fRemaining :=
        (fElapsed * (fExpectedWrittenSize - fProcessedSize)) div fProcessedSize;
    fPercent := (fCurrentSize * 100) div fExpectedSize;
  end;
  if fElapsed = 0 then
    fPerSecond := 0
  else
    fPerSecond := (fProcessedSize * 1000) div fElapsed;
  if Assigned(fOnLog) then
    if fExpectedSize = 0 then
      fOnLog(sllTrace, '%: % - % %/s',
        [fContext, KB(fCurrentSize), KB(fProcessedSize), KB(fPerSecond)], self)
    else
      fOnLog(sllTrace, '%: %% - % / % - % %/s', [fContext, fPercent, '%',
        KB(fCurrentSize), KB(fExpectedSize), KB(fProcessedSize), KB(PerSecond)], self);
  if Assigned(fOnProgress) then
    fOnProgress(self);
end;

procedure TStreamRedirect.DoHash(data: pointer; len: integer);
begin // no associated hasher on this parent class
end;

procedure TStreamRedirect.SetExpectedSize(Value: Int64);
begin
  fExpectedSize := Value;
  fExpectedWrittenSize := Value - fPosition;
end;

function TStreamRedirect.GetHash: RawUtf8;
begin
  result := ''; // no associated hasher on this parent class
end;

class function TStreamRedirect.GetHashFileExt: RawUtf8;
begin
  result := ''; // no associated hasher on this parent class
end;

class function TStreamRedirect.HashFile(const FileName: TFileName): RawUtf8;
var
  hasher: TStreamRedirect;
  f: THandle;
begin
  result := '';
  if GetHashFileExt = '' then
    exit; // no hash function defined
  f := FileOpenSequentialRead(FileName);
  if not ValidHandle(f) then
    exit;
  hasher := Create(TFileStreamFromHandle.Create(f));
  try
    hasher.Append;
    result := hasher.GetHash;
  finally
    hasher.Free; // includes FileClose(f)
  end;
end;

procedure TStreamRedirect.Append;
var
  buf: array[word] of cardinal; // 256KB of buffer
  read: PtrInt;
begin
  if fRedirected = nil then
    raise ESynException.CreateUtf8('%.Append(%): Redirected=nil',
      [self, fContext]);
  if fMode = mRead then
    raise ESynException.CreateUtf8('%.Append(%) after Read()', [self, fContext]);
  fMode := mWrite;
  if GetHashFileExt = '' then // DoHash() does nothing
  begin
    fCurrentSize := fRedirected.Seek(0, soEnd);
    fPosition := fCurrentSize;
  end
  else
    repeat
      read := fRedirected.Read(buf, SizeOf(buf));
      if read <= 0 then
        break;
      DoHash(@buf, read);
      inc(fCurrentSize, read);
      inc(fPosition, read);
    until false;
end;

procedure TStreamRedirect.Ended;
begin
  if fCurrentSize = fExpectedSize then
    exit; // nothing to report
  fExpectedSize := fCurrentSize; // reached 100%
  if Assigned(fOnProgress) or
     Assigned(fOnLog) then
    DoReport(true); // notify finished
end;

procedure TStreamRedirect.Terminate;
begin
  fTerminated := true;
end;

procedure TStreamRedirect.ReadWriteHash(const Buffer; Count: Longint);
begin
  DoHash(@Buffer, Count);
  inc(fCurrentSize, Count);
  inc(fProcessedSize, Count);
  inc(fPosition, Count);
end;

procedure TStreamRedirect.ReadWriteReport(const Caller: ShortString);
var
  tix, tosleep: Int64;
begin
  tix := GetTickCount64;
  fElapsed := tix - fStartTix;
  if (fLimitPerSecond <> 0) or
     (fTimeOut <> 0) then
  begin
    if tix shr 7 <> fLastTix shr 7 then // checking every 128 ms is good enough
    begin
      fLastTix := tix;
      if fElapsed > 0 then
      begin
        if (fTimeOut <> 0) and
           (fElapsed > fTimeOut) then
          raise ESynException.CreateUtf8('%.%(%) timeout after %',
            [self, Caller, fContext, MicroSecToString(fElapsed * 1000)]);
        if fLimitPerSecond > 0 then
        begin
          // adjust bandwith limit every 128 ms by adding some sleep() steps
          tosleep := ((fProcessedSize * 1000) div fLimitPerSecond) - fElapsed;
          if tosleep > 10 then // on Windows, typical resolution is 16ms
          begin
            while tosleep > 300 do
            begin
              SleepHiRes(300); // show progress on very low bandwidth
              if Assigned(fOnProgress) or
                 Assigned(fOnLog) then
                DoReport(true);
              dec(tosleep, 300);
              if fTerminated then
                raise ESynException.CreateUtf8('%.%(%) Terminated',
                  [self, Caller, fContext]);
            end;
            SleepHiRes(tosleep);
          end;
        end;
      end;
    end;
  end;
  if Assigned(fOnProgress) or
     Assigned(fOnLog) then
    DoReport(false);
  if fTerminated then
    raise ESynException.CreateUtf8('%.%(%) Terminated',
      [self, Caller, fContext]);
end;

function TStreamRedirect.Read(var Buffer; Count: Longint): Longint;
begin
  if fMode = mWrite then
    raise ESynException.CreateUtf8('%.Read(%) in Write() mode',
      [self, fContext]);
  fMode := mRead;
  if fRedirected = nil then
    raise ESynException.CreateUtf8('%.Read(%) with Redirected=nil',
      [self, fContext]);
  result := fRedirected.Read(Buffer, Count);
  ReadWriteHash(Buffer, result);
  ReadWriteReport('Read');
end;

function TStreamRedirect.Write(const Buffer; Count: Longint): Longint;
begin
  if fMode = mRead then
    raise ESynException.CreateUtf8('%.Write(%) in Read() mode',
      [self, fContext]);
  fMode := mWrite;
  ReadWriteHash(Buffer, Count);
  result := Count;
  if fRedirected = nil then
    exit; // we may just want the hash
  fRedirected.WriteBuffer(Buffer, Count);
  ReadWriteReport('Write');
end;


{ TStreamRedirectHasher }

function TStreamRedirectHasher.GetHash: RawUtf8;
begin
  result := CardinalToHexLower(fHash);
end;


{ TStreamRedirectCrc32c }

procedure TStreamRedirectCrc32c.DoHash(data: pointer; len: integer);
begin
  fHash := crc32c(fHash, data, len);
end;

class function TStreamRedirectCrc32c.GetHashFileExt: RawUtf8;
begin
  result := '.crc32c';
end;



function HashFile(const FileName: TFileName; Hasher: THasher): cardinal;
var
  buf: array[word] of cardinal; // 256KB of buffer
  read: integer;
  f: THandle;
begin
  if not Assigned(Hasher) then
    Hasher := DefaultHasher;
  result := 0;
  f := FileOpenSequentialRead(FileName);
  if ValidHandle(f) then
  begin
    repeat
      read := FileRead(f, buf, SizeOf(buf));
      if read <= 0 then
        break;
      result := Hasher(result, @buf, read);
    until false;
    FileClose(f);
  end;
end;

function HashFileCrc32c(const FileName: TFileName): RawUtf8;
begin
  result := CardinalToHexLower(HashFile(FileName, crc32c));
end;


{ ************* Markup (e.g. HTML or Emoji) process }

{ internal TTextWriterEscape class }

type
  TTextWriterEscapeStyle = (
    tweBold, tweItalic, tweCode);

  TTextWriterEscapeLineStyle = (
    twlNone, twlParagraph, twlOrderedList, twlUnorderedList, twlBlockquote,
    twlCode4, twlCode3);

  TTextWriterEscape = object
    P, B, P2, B2: PUtf8Char;
    W: TBaseWriter;
    st: set of TTextWriterEscapeStyle;
    fmt: TTextWriterHtmlFormat;
    esc: TTextWriterHtmlEscape;
    lst: TTextWriterEscapeLineStyle;
    procedure Start(dest: TBaseWriter; src: PUtf8Char; escape: TTextWriterHtmlEscape);
    function ProcessText(const stopchars: TSynByteSet): AnsiChar;
    procedure ProcessHRef;
    function ProcessLink: boolean;
    procedure ProcessEmoji;
      {$ifdef HASINLINE}inline;{$endif}
    procedure Toggle(style: TTextWriterEscapeStyle);
    procedure SetLine(style: TTextWriterEscapeLineStyle);
    procedure EndOfParagraph;
    procedure NewMarkdownLine;
    procedure AddHtmlEscapeWiki(dest: TBaseWriter; src: PUtf8Char;
      escape: TTextWriterHtmlEscape);
    procedure AddHtmlEscapeMarkdown(dest: TBaseWriter; src: PUtf8Char;
      escape: TTextWriterHtmlEscape);
  end;

procedure TTextWriterEscape.Start(dest: TBaseWriter; src: PUtf8Char;
  escape: TTextWriterHtmlEscape);
begin
  P := src;
  W := dest;
  st := [];
  if heHtmlEscape in escape then
    fmt := hfOutsideAttributes
  else
    fmt := hfNone;
  esc := escape;
  lst := twlNone;
end;

function IsHttpOrHttps(P: PUtf8Char): boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
  result := (PCardinal(P)^ =
             ord('h') + ord('t') shl 8 + ord('t') shl 16 + ord('p') shl 24) and
            ((PCardinal(P + 4)^ and $ffffff =
             ord(':') + ord('/') shl 8 + ord('/') shl 16) or
             (PCardinal(P + 4)^ =
             ord('s') + ord(':') shl 8 + ord('/') shl 16 + ord('/') shl 24));
end;

function TTextWriterEscape.ProcessText(const stopchars: TSynByteSet): AnsiChar;
begin
  if P = nil then
  begin
    result := #0;
    exit;
  end;
  B := P;
  while not (ord(P^) in stopchars) and
        not IsHttpOrHttps(P) do
    inc(P);
  W.AddHtmlEscape(B, P - B, fmt);
  result := P^;
end;

procedure TTextWriterEscape.ProcessHRef;
begin
  B := P;
  while P^ > ' ' do
    inc(P);
  W.AddShort('<a href="');
  W.AddHtmlEscape(B, P - B, hfWithinAttributes);
  W.AddShort('" rel="nofollow">');
  W.AddHtmlEscape(B, P - B);
  W.AddShorter('</a>');
end;

function TTextWriterEscape.ProcessLink: boolean;
begin
  inc(P);
  B2 := P;
  while not (P^ in [#0, ']']) do
    inc(P);
  P2 := P;
  if PWord(P)^ = ord(']') + ord('(') shl 8 then
  begin
    inc(P, 2);
    B := P;
    while not (P^ in [#0, ')']) do
      inc(P);
    if P^ = ')' then
    begin
      // [GitHub](https://github.com)
      result := true;
      exit;
    end;
  end;
  P := B2; // rollback
  result := false;
end;

procedure TTextWriterEscape.ProcessEmoji;
begin
  if heEmojiToUtf8 in esc then
    EmojiParseDots(P, W)
  else
  begin
    W.Add(':');
    inc(P);
  end;
end;

procedure TTextWriterEscape.Toggle(style: TTextWriterEscapeStyle);
const
  HTML: array[tweBold..tweCode] of string[7] = (
    'strong>', 'em>', 'code>');
begin
  W.Add('<');
  if style in st then
  begin
    W.Add('/');
    exclude(st, style);
  end
  else
    include(st, style);
  W.AddShorter(HTML[style]);
end;

procedure TTextWriterEscape.EndOfParagraph;
begin
  if tweBold in st then
    Toggle(tweBold);
  if tweItalic in st then
    Toggle(tweItalic);
  if P <> nil then
    if PWord(P)^ = $0a0d then
      inc(P, 2)
    else
      inc(P);
end;

procedure TTextWriterEscape.SetLine(style: TTextWriterEscapeLineStyle);
const
  HTML: array[twlParagraph..twlCode3] of string[5] = (
    'p>', 'li>', 'li>', 'p>', 'code>', 'code>');
  HTML2: array[twlOrderedList..twlCode3] of string[11] = (
    'ol>', 'ul>', 'blockquote>', 'pre>', 'pre>');
begin
  if lst >= low(HTML) then
  begin
    if (lst < twlCode4) or
       (lst <> style) then
    begin
      W.Add('<', '/');
      W.AddShorter(HTML[lst]);
    end;
    if (lst >= low(HTML2)) and
       (lst <> style) then
    begin
      W.Add('<', '/');
      W.AddShort(HTML2[lst]);
    end;
  end;
  if style >= low(HTML) then
  begin
    if (style >= low(HTML2)) and
       (lst <> style) then
    begin
      W.Add('<');
      W.AddShort(HTML2[style]);
    end;
    if (style < twlCode4) or
       (lst <> style) then
    begin
      W.Add('<');
      W.AddShorter(HTML[style]);
    end;
  end;
  lst := style;
end;

procedure TTextWriterEscape.NewMarkdownLine;
label
  none;
var
  c: cardinal;
begin
  if P = nil then
    exit;
  c := PCardinal(P)^;
  if c and $ffffff = ord('`') + ord('`') shl 8 + ord('`') shl 16 then
  begin
    inc(P, 3);
    if lst = twlCode3 then
    begin
      lst := twlCode4; // to close </code></pre>
      NewMarkdownLine;
      exit;
    end;
    SetLine(twlCode3);
  end;
  if lst = twlCode3 then
    exit; // no prefix process within ``` code blocks
  if c = $20202020 then
  begin
    SetLine(twlCode4);
    inc(P, 4);
    exit;
  end;
  P := GotoNextNotSpaceSameLine(P); // don't implement nested levels yet
  case P^ of
    '*', '+', '-':
      if P[1] = ' ' then
        SetLine(twlUnorderedList)
      else
        goto none;
    '1'..'9':
      begin
        // first should be 1. then any ##. number to continue
        B := P;
        repeat
          inc(P)
        until not (P^ in ['0'..'9']);
        if (P^ = '.') and
           ((lst = twlOrderedList) or
            (PWord(B)^ = ord('1') + ord('.') shl 8)) then
          SetLine(twlOrderedList)
        else
        begin
          P := B;
none:     if lst = twlParagraph then
          begin
            c := PWord(P)^; // detect blank line to separate paragraphs
            if c = $0a0d then
              inc(P, 2)
            else if c and $ff = $0a then
              inc(P)
            else
            begin
              W.AddOnce(' ');
              exit;
            end;
          end;
          SetLine(twlParagraph);
          exit;
        end;
      end;
    '>':
      if P[1] = ' ' then
        SetLine(twlBlockquote)
      else
        goto none;
  else
    goto none;
  end;
  P := GotoNextNotSpaceSameLine(P + 1);
end;

procedure TTextWriterEscape.AddHtmlEscapeWiki(dest: TBaseWriter;
  src: PUtf8Char; escape: TTextWriterHtmlEscape);
begin
  Start(dest, src, escape);
  SetLine(twlParagraph);
  repeat
    case ProcessText([0, 10, 13,
          ord('*'), ord('+'), ord('`'), ord('\'), ord(':')]) of
      #0:
        break;
      #10, #13:
        begin
          EndOfParagraph;
          SetLine(twlParagraph);
          continue;
        end;
      '\':
        if P[1] in ['\', '`', '*', '+'] then
        begin
          inc(P);
          W.Add(P^);
        end
        else
          W.Add('\');
      '*':
        Toggle(tweItalic);
      '+':
        Toggle(tweBold);
      '`':
        Toggle(tweCode);
      'h':
        begin
          ProcessHRef;
          continue;
        end;
      ':':
        begin
          ProcessEmoji;
          continue;
        end;
    end;
    inc(P);
  until false;
  EndOfParagraph;
  SetLine(twlNone);
end;

procedure TTextWriterEscape.AddHtmlEscapeMarkdown(dest: TBaseWriter;
  src: PUtf8Char; escape: TTextWriterHtmlEscape);
begin
  Start(dest, src, escape);
  NewMarkDownLine;
  repeat
    if lst >= twlCode4 then // no Markdown tags within code blocks
      if ProcessText([0, 10, 13]) = #0 then
        break
      else
      begin
        if PWord(P)^ = $0a0d then
          inc(P, 2)
        else
          inc(P);
        W.AddCR; // keep LF within <pre>
        NewMarkdownLine;
        continue;
      end
    else
      case ProcessText([0, 10, 13, ord('*'), ord('_'), ord('`'),
             ord('\'), ord('['), ord('!'), ord(':')]) of
        #0:
          break;
        #10, #13:
          begin
            EndOfParagraph;
            NewMarkdownLine;
            continue;
          end;
        '\':
          if P[1] in ['\', '`', '*', '_', '[', ']', '{', '}',
                      '(', ')', '#', '+', '-', '.', '!'] then
          begin
            // backslash escape
            inc(P);
            W.Add(P^);
          end
          else
            W.Add('\');
        '*', '_':
          if P[1] = P[0] then
          begin
            // **This text will be bold** or __This text will be bold__
            inc(P);
            Toggle(tweBold);
          end
          else
            // *This text will be italic* or _This text will be italic_
            Toggle(tweItalic);
        '`':
          // `This text will be code`
          Toggle(tweCode);
        '[':
          if ProcessLink then
          begin
            // [GitHub](https://github.com)
            W.AddShort('<a href="');
            W.AddHtmlEscape(B, P - B, hfWithinAttributes);
            if IsHttpOrHttps(B) then
              W.AddShort('" rel="nofollow">')
            else
              W.Add('"', '>');
            W.AddHtmlEscape(B2, P2 - B2, fmt);
            W.AddShorter('</a>'); // no continune -> need inc(P) over ending )
          end
          else
            // not a true link -> just append
            W.Add('[');
        '!':
          begin
            if P[1] = '[' then
            begin
              inc(P);
              if ProcessLink then
              begin
                W.AddShort('<img alt="');
                W.AddHtmlEscape(B2, P2 - B2, hfWithinAttributes);
                W.AddShorter('" src="');
                W.AddNoJsonEscape(B, P - B);
                W.AddShorter('">');
                inc(P);
                continue;
              end;
              dec(P);
            end;
            W.Add('!'); // not a true image
          end;
        'h':
          begin
            ProcessHRef;
            continue;
          end;
        ':':
          begin
            ProcessEmoji;
            continue;
          end;
      end;
    inc(P);
  until false;
  EndOfParagraph;
  SetLine(twlNone);
end;

function HtmlEscapeWiki(const wiki: RawUtf8; esc: TTextWriterHtmlEscape): RawUtf8;
var
  temp: TTextWriterStackBuffer;
  W: TBaseWriter;
begin
  W := TBaseWriter.CreateOwnedStream(temp);
  try
    AddHtmlEscapeWiki(W, pointer(wiki), esc);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

function HtmlEscapeMarkdown(const md: RawUtf8; esc: TTextWriterHtmlEscape): RawUtf8;
var
  temp: TTextWriterStackBuffer;
  W: TBaseWriter;
begin
  W := TBaseWriter.CreateOwnedStream(temp);
  try
    AddHtmlEscapeMarkdown(W, pointer(md), esc);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

procedure AddHtmlEscapeWiki(W: TBaseWriter; P: PUtf8Char; esc: TTextWriterHtmlEscape);
var
  doesc: TTextWriterEscape;
begin
  doesc.AddHtmlEscapeWiki(W, P, esc);
end;

procedure AddHtmlEscapeMarkdown(W: TBaseWriter; P: PUtf8Char; esc: TTextWriterHtmlEscape);
var
  doesc: TTextWriterEscape;
begin
  doesc.AddHtmlEscapeMarkdown(W, P, esc);
end;

function EmojiFromText(P: PUtf8Char; len: PtrInt): TEmoji;
begin
  // RTTI has shortstrings in adjacent L1 cache lines -> faster than EMOJI_TEXT[]
  result := TEmoji(FindShortStringListTrimLowerCase(
    EMOJI_RTTI, ord(high(TEmoji)) - 1, P, len) + 1);
  // note: we may enhance performance by using FastFindPUtf8CharSorted()
end;

function EmojiParseDots(var P: PUtf8Char; W: TBaseWriter): TEmoji;
var
  c: PUtf8Char;
begin
  result := eNone;
  inc(P); // ignore trailing ':'
  c := P;
  if c[-2] <= ' ' then
  begin
    if (c[1] <= ' ') and
       (c^ in ['('..'|']) then
      result := EMOJI_AFTERDOTS[c^]; // e.g. :)
    if result = eNone then
    begin
      while c^ in ['a'..'z', 'A'..'Z', '_'] do
        inc(c);
      if (c^ = ':') and
         (c[1] <= ' ') then // try e.g. :joy_cat:
        result := EmojiFromText(P, c - P);
    end;
    if result <> eNone then
    begin
      P := c + 1; // continue parsing after the Emoji text
      if W <> nil then
        W.AddNoJsonEscape(pointer(EMOJI_UTF8[result]), 4);
      exit;
    end;
  end;
  if W <> nil then
    W.Add(':');
end;

procedure EmojiToDots(P: PUtf8Char; W: TBaseWriter);
var
  B: PUtf8Char;
  c: cardinal;
begin
  if (P <> nil) and
     (W <> nil) then
    repeat
      B := P;
      while (P^ <> #0) and
            (PWord(P)^ <> $9ff0) do
        inc(P);
      W.AddNoJsonEscape(B, P - B);
      if P^ = #0 then
        break;
      B := P;
      c := NextUtf8Ucs4(P) - $1f5ff;
      if c <= cardinal(high(TEmoji)) then
        W.AddNoJsonEscapeUtf8(EMOJI_TAG[TEmoji(c)])
      else
        W.AddNoJsonEscape(B, P - B);
    until P^ = #0;
end;

function EmojiToDots(const text: RawUtf8): RawUtf8;
var
  W: TBaseWriter;
  tmp: TTextWriterStackBuffer;
begin
  if PosExChar(#$f0, text) = 0 then
  begin
    result := text; // no UTF-8 smiley for sure
    exit;
  end;
  W := TBaseWriter.CreateOwnedStream(tmp);
  try
    EmojiToDots(pointer(text), W);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

procedure EmojiFromDots(P: PUtf8Char; W: TBaseWriter);
var
  B: PUtf8Char;
begin
  if (P <> nil) and
     (W <> nil) then
    repeat
      B := P;
      while not (P^ in [#0, ':']) do
        inc(P);
      W.AddNoJsonEscape(B, P - B);
      if P^ = #0 then
        break;
      EmojiParseDots(P, W);
    until P^ = #0;
end;

function EmojiFromDots(const text: RawUtf8): RawUtf8;
var
  W: TBaseWriter;
  tmp: TTextWriterStackBuffer;
begin
  W := TBaseWriter.CreateOwnedStream(tmp);
  try
    EmojiFromDots(pointer(text), W);
    W.SetText(result);
  finally
    W.Free;
  end;
end;


{ ************ RawByteString Buffers Aggregation via TRawByteStringGroup }

{ TRawByteStringGroup }

procedure TRawByteStringGroup.Add(const aItem: RawByteString);
begin
  if Values = nil then
    Clear; // ensure all fields are initialized, even if on stack
  if Count = Length(Values) then
    SetLength(Values, NextGrow(Count));
  with Values[Count] do
  begin
    Position := self.Position;
    Value := aItem;
  end;
  LastFind := Count;
  inc(Count);
  inc(Position, Length(aItem));
end;

procedure TRawByteStringGroup.Add(aItem: pointer; aItemLen: integer);
var
  tmp: RawByteString;
begin
  SetString(tmp, PAnsiChar(aItem), aItemLen);
  Add(tmp);
end;

procedure TRawByteStringGroup.Add(const aAnother: TRawByteStringGroup);
var
  i: integer;
  s, d: PRawByteStringGroupValue;
begin
  if aAnother.Values = nil then
    exit;
  if Values = nil then
    Clear; // ensure all fields are initialized, even if on stack
  if Count + aAnother.Count > Length(Values) then
    SetLength(Values, Count + aAnother.Count);
  s := pointer(aAnother.Values);
  d := @Values[Count];
  for i := 1 to aAnother.Count do
  begin
    d^.Position := Position;
    d^.Value := s^.Value;
    inc(Position, length(s^.Value));
    inc(s);
    inc(d);
  end;
  inc(Count, aAnother.Count);
  LastFind := Count - 1;
end;

procedure TRawByteStringGroup.RemoveLastAdd;
begin
  if Count > 0 then
  begin
    dec(Count);
    dec(Position, Length(Values[Count].Value));
    Values[Count].Value := ''; // release memory
    LastFind := Count - 1;
  end;
end;

function TRawByteStringGroup.Equals(const aAnother: TRawByteStringGroup): boolean;
begin
  if ((Values = nil) and
      (aAnother.Values <> nil)) or
     ((Values <> nil) and
      (aAnother.Values = nil)) or
     (Position <> aAnother.Position) then
    result := false
  else if (Count <> 1) or
          (aAnother.Count <> 1) or
          (Values[0].Value <> aAnother.Values[0].Value) then
    result := AsText = aAnother.AsText
  else
    result := true;
end;

procedure TRawByteStringGroup.Clear;
begin
  Values := nil;
  Position := 0;
  Count := 0;
  LastFind := 0;
end;

procedure TRawByteStringGroup.AppendTextAndClear(var aDest: RawByteString);
var
  d, i: integer;
  v: PRawByteStringGroupValue;
begin
  d := length(aDest);
  SetLength(aDest, d + Position);
  v := pointer(Values);
  for i := 1 to Count do
  begin
    MoveFast(pointer(v^.Value)^, PByteArray(aDest)[d + v^.Position], length(v^.Value));
    inc(v);
  end;
  Clear;
end;

function TRawByteStringGroup.AsText: RawByteString;
begin
  if Values = nil then
    result := ''
  else
  begin
    if Count > 1 then
      Compact;
    result := Values[0].Value;
  end;
end;

procedure TRawByteStringGroup.Compact;
var
  i: integer;
  v: PRawByteStringGroupValue;
  tmp: RawByteString;
begin
  if (Values <> nil) and
     (Count > 1) then
  begin
    SetString(tmp, nil, Position);
    v := pointer(Values);
    for i := 1 to Count do
    begin
      MoveFast(pointer(v^.Value)^, PByteArray(tmp)[v^.Position], length(v^.Value));
      {$ifdef FPC}
      FastAssignNew(v^.Value);
      {$else}
      v^.Value := '';
      {$endif FPC}
      inc(v);
    end;
    Values[0].Value := tmp; // use result for absolute compaction ;)
    if Count > 128 then
      SetLength(Values, 128);
    Count := 1;
    LastFind := 0;
  end;
end;

function TRawByteStringGroup.AsBytes: TByteDynArray;
var
  i: integer;
begin
  result := nil;
  if Values = nil then
    exit;
  SetLength(result, Position);
  for i := 0 to Count - 1 do
    with Values[i] do
      MoveFast(pointer(Value)^, PByteArray(result)[Position], length(Value));
end;

procedure TRawByteStringGroup.Write(W: TBaseWriter; Escape: TTextWriterKind);
var
  i: integer;
begin
  if Values <> nil then
    for i := 0 to Count - 1 do
      with Values[i] do
        W.Add(PUtf8Char(pointer(Value)), length(Value), Escape);
end;

procedure TRawByteStringGroup.WriteBinary(W: TBufferWriter);
var
  i: integer;
begin
  if Values <> nil then
    for i := 0 to Count - 1 do
      W.WriteBinary(Values[i].Value);
end;

procedure TRawByteStringGroup.WriteString(W: TBufferWriter);
begin
  if Values = nil then
  begin
    W.Write1(0);
    exit;
  end;
  W.WriteVarUInt32(Position);
  WriteBinary(W);
end;

procedure TRawByteStringGroup.AddFromReader(var aReader: TFastReader);
var
  complexsize: integer;
begin
  complexsize := aReader.VarUInt32;
  if complexsize > 0 then
    // directly create a RawByteString from aReader buffer
    Add(aReader.Next(complexsize), complexsize);
end;

function TRawByteStringGroup.Find(aPosition: integer): PRawByteStringGroupValue;
var
  i: integer;
begin
  if (pointer(Values) <> nil) and
     (cardinal(aPosition) < cardinal(Position)) then
  begin
    result := @Values[LastFind]; // this cache is very efficient in practice
    if (aPosition >= result^.Position) and
       (aPosition < result^.Position + length(result^.Value)) then
      exit;
    result := @Values[1]; // seldom O(n) brute force search (in CPU L1 cache)
    for i := 0 to Count - 2 do
      if result^.Position > aPosition then
      begin
        dec(result);
        LastFind := i;
        exit;
      end
      else
        inc(result);
    dec(result);
    LastFind := Count - 1;
  end
  else
    result := nil;
end;

function TRawByteStringGroup.Find(aPosition, aLength: integer): pointer;
var
  P: PRawByteStringGroupValue;
  i: integer;
label
  found;
begin
  if (pointer(Values) <> nil) and
     (cardinal(aPosition) < cardinal(Position)) then
  begin
    P := @Values[LastFind]; // this cache is very efficient in practice
    i := aPosition - P^.Position;
    if (i >= 0) and
       (i + aLength < length(P^.Value)) then
    begin
      result := @PByteArray(P^.Value)[i];
      exit;
    end;
    P := @Values[1]; // seldom O(n) brute force search (in CPU L1 cache)
    for i := 0 to Count - 2 do
      if P^.Position > aPosition then
      begin
        LastFind := i;
found:  dec(P);
        dec(aPosition, P^.Position);
        if aLength - aPosition <= length(P^.Value) then
          result := @PByteArray(P^.Value)[aPosition]
        else
          result := nil;
        exit;
      end
      else
        inc(P);
    LastFind := Count - 1;
    goto found;
  end
  else
    result := nil;
end;

procedure TRawByteStringGroup.FindAsText(aPosition, aLength: integer;
  out aText: RawByteString);
var
  P: PRawByteStringGroupValue;
begin
  P := Find(aPosition);
  if P = nil then
    exit;
  dec(aPosition, P^.Position);
  if (aPosition = 0) and
     (length(P^.Value) = aLength) then
    aText := P^.Value
  else
  // direct return if not yet compacted
  if aLength - aPosition <= length(P^.Value) then
    SetString(aText, PAnsiChar(@PByteArray(P^.Value)[aPosition]), aLength);
end;

function TRawByteStringGroup.FindAsText(aPosition, aLength: integer): RawByteString;
{%H-}begin
  {%H-}FindAsText(aPosition, aLength, result);
end;

procedure TRawByteStringGroup.FindAsVariant(aPosition, aLength: integer;
  out aDest: variant);
var
  tmp: RawByteString;
begin
  FindAsText(aPosition, aLength, tmp);
  if {%H-}tmp <> '' then
    RawUtf8ToVariant(tmp, aDest);
end;

procedure TRawByteStringGroup.FindWrite(aPosition, aLength: integer;
  W: TBaseWriter; Escape: TTextWriterKind; TrailingCharsToIgnore: integer);
var
  P: pointer;
begin
  P := Find(aPosition, aLength);
  if P <> nil then
    W.Add(PUtf8Char(P) + TrailingCharsToIgnore, aLength - TrailingCharsToIgnore, Escape);
end;

procedure TRawByteStringGroup.FindWriteBase64(aPosition, aLength: integer;
  W: TBaseWriter; withMagic: boolean);
var
  P: pointer;
begin
  P := Find(aPosition, aLength);
  if P <> nil then
    W.WrBase64(P, aLength, withMagic);
end;

procedure TRawByteStringGroup.FindMove(aPosition, aLength: integer;
  aDest: pointer);
var
  P: pointer;
begin
  P := Find(aPosition, aLength);
  if P <> nil then
    MoveFast(P^, aDest^, aLength);
end;


{ TRawByteStringBuffer }

function TRawByteStringBuffer.GetCapacity: PtrInt;
begin
  result := length(fBuffer);
end;

procedure TRawByteStringBuffer.Reset;
begin
  fLen := 0;
end;

procedure TRawByteStringBuffer.Clear;
begin
  fLen := 0;
  fBuffer := '';
end;

function TRawByteStringBuffer.Buffer: pointer;
begin
  result := pointer(fBuffer);
end;

procedure TRawByteStringBuffer.Append(P: pointer; PLen: PtrInt; CRLF: boolean);
var
  cap, needed: PtrInt;
begin
  if (PLen <= 0) and
     not CRLF then
    exit;
  cap := Length(fBuffer);
  if cap = 0 then
    SetLength(fBuffer, PLen + 128) // small overhead at first
  else
  begin
    needed := fLen + PLen + 2;
    if needed > cap then
      SetLength(fBuffer, needed + needed shr 3 + 2048); // generous overhead
  end;
  if PLen > 0 then
  begin
    MoveFast(P^, PByteArray(fBuffer)[fLen], PLen);
    inc(fLen, PLen);
  end;
  if CRLF then
  begin
    PWord(@PByteArray(fBuffer)[fLen])^ := $0a0d;
    inc(fLen, 2);
  end;
end;

procedure TRawByteStringBuffer.Append(const Text: RawUtf8; CRLF: boolean);
begin
  Append(pointer(Text), length(Text), CRLF);
end;

procedure TRawByteStringBuffer.Append(const Args: array of const; CRLF: boolean);
var
  tmp: TTempUtf8;
  a: PtrInt;
begin
  for a := 0 to high(Args) do
  begin
    VarRecToTempUtf8(Args[a], tmp);
    Append(tmp.Text, tmp.Len);
    if tmp.TempRawUtf8 <> nil then
      RawUtf8(tmp.TempRawUtf8) := ''; // release temp memory
  end;
  if CRLF then
    Append('', {crlf=}true);
end;

function TRawByteStringBuffer.CanAppend(Bytes: PtrInt): boolean;
begin
  result := (Bytes = 0) or
            (fLen + Bytes <= length(fBuffer));
end;

function TRawByteStringBuffer.Reserve(MaxSize: PtrInt): pointer;
begin
  fLen := 0;
  if MaxSize > length(fBuffer) then
    SetString(fBuffer, nil, MaxSize); // no realloc -> not SetLength()
  result := pointer(fBuffer);
end;

procedure TRawByteStringBuffer.Remove(FirstBytes: PtrInt);
begin
  if FirstBytes > 0 then
    if FirstBytes >= fLen then
      fLen := 0
    else
    begin
      dec(fLen, FirstBytes);
      MoveFast(PByteArray(fBuffer)[FirstBytes], pointer(fBuffer)^, fLen);
    end;
end;

function TRawByteStringBuffer.Extract(Dest: pointer; Count: PtrInt): PtrInt;
begin
  result := fLen;
  if Count < result then
    result := Count;
  if result <= 0 then
    exit;
  MoveFast(pointer(fBuffer)^, Dest^, result);
  dec(fLen, result);
  if fLen <> 0 then // keep trailing bytes for next call
    MoveFast(PByteArray(fBuffer)[result], pointer(fBuffer)^, fLen);
end;

function TRawByteStringBuffer.ExtractAt(
  var Dest: PAnsiChar; var Count: PtrInt; var Pos: PtrInt): PtrInt;
begin
  result := fLen - Pos;
  if (result = 0) or
     (Count = 0) then
    exit;
  if result > Count then
    result := Count;
  MoveFast(PByteArray(fBuffer)[Pos], Dest^, result);
  inc(Pos, result);
  if Pos = fLen then
  begin
    Reset; // all pending content has been read
    Pos := 0;
  end;
  inc(Dest, result);
  dec(Count, result);
end;

procedure TRawByteStringBuffer.Insert(P: pointer; PLen: PtrInt;
  Position: PtrInt; CRLF: boolean);
begin
  inc(PLen, 2 * ord(CRLF));
  if PLen + fLen > length(fBuffer) then
    SetLength(fBuffer, PLen + fLen + fLen shr 3);
  MoveFast(pointer(fBuffer)^, PByteArray(fBuffer)[PLen], fLen);
  dec(PLen, 2 * ord(CRLF));
  MoveFast(P^, pointer(fBuffer)^, PLen);
  if CRLF then
    PWord(@PByteArray(fBuffer)[PLen])^ := $0a0d;
end;

procedure TRawByteStringBuffer.AsText(out Text: RawUtf8; Overhead: PtrInt);
begin
  if (Len = 0) or
     (OverHead < 0) then
    exit;
  FastSetString(Text, nil, Len + Overhead);
  MoveFast(pointer(fBuffer)^, pointer(Text)^, Len);
  if OverHead <> 0 then
  begin
    PAnsiChar(pointer(Text))[Len] := #0;
    PStrLen(PAnsiChar(pointer(Text)) - _STRLEN)^ := Len; // fake length
  end;
end;


procedure InitializeUnit;
var
  i: PtrInt;
  e: TEmoji;
begin
  // initialize Base64/Base64Uri/Base58 encoding/decoding tables
  FillcharFast(ConvertBase64ToBin, SizeOf(ConvertBase64ToBin), 255); // -1 = invalid
  for i := 0 to high(b64enc) do
    ConvertBase64ToBin[b64enc[i]] := i;
  ConvertBase64ToBin['='] := -2; // special value for '='
  for i := 0 to high(b64urienc) do
    ConvertBase64uriToBin[b64urienc[i]] := i;
  FillcharFast(ConvertBase58ToBin, SizeOf(ConvertBase58ToBin), 255); // -1 = invalid
  for i := 0 to high(b58enc) do
    ConvertBase58ToBin[b58enc[i]] := i;
  for i := high(Baudot2Char) downto 0 do
    if Baudot2Char[i]<#128 then
      Char2Baudot[Baudot2Char[i]] := i;
  for i := ord('a') to ord('z') do
    Char2Baudot[AnsiChar(i - 32)] := Char2Baudot[AnsiChar(i)]; // A-Z -> a-z
  // HTML/Emoji Efficient Parsing
  Assert(ord(high(TEmoji)) = $4f + 1);
  EMOJI_RTTI := GetEnumName(TypeInfo(TEmoji), 1); // ignore eNone=0
  GetEnumTrimmedNames(TypeInfo(TEmoji), @EMOJI_TEXT);
  EMOJI_TEXT[eNone] := '';
  for e := succ(low(e)) to high(e) do
  begin
    LowerCaseSelf(EMOJI_TEXT[e]);
    EMOJI_TAG[e] := ':' + EMOJI_TEXT[e] + ':';
    SetLength(EMOJI_UTF8[e], 4);
    Ucs4ToUtf8(ord(e) + $1f5ff, pointer(EMOJI_UTF8[e]));
  end;
  EMOJI_AFTERDOTS[')'] := eSmiley;
  EMOJI_AFTERDOTS['('] := eFrowning;
  EMOJI_AFTERDOTS['|'] := eExpressionless;
  EMOJI_AFTERDOTS['/'] := eConfused;
  EMOJI_AFTERDOTS['D'] := eLaughing;
  EMOJI_AFTERDOTS['o'] := eOpen_mouth;
  EMOJI_AFTERDOTS['O'] := eOpen_mouth;
  EMOJI_AFTERDOTS['p'] := eYum;
  EMOJI_AFTERDOTS['P'] := eYum;
  EMOJI_AFTERDOTS['s'] := eScream;
  EMOJI_AFTERDOTS['S'] := eScream;
  // setup internal lists and function wrappers
  AlgoSynLZ := TAlgoSynLZ.Create;
end;

procedure FinalizeUnit;
begin
  ObjArrayClear(SynCompressAlgos);
end;


initialization
  InitializeUnit;

finalization
  FinalizeUnit;
  
end.
