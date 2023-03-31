/// access to the 7-Zip library on Windows
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.lib.win7zip;

{
  *****************************************************************************

   Access to the 7-Zip Compression/Decompression DLL on Windows 
   - Low-Level 7-Zip API Definitions
   - I7zReadArchive/I7zWriter High-Level Wrappers

  *****************************************************************************

  Can I use the EXE or DLL files from 7-Zip in a commercial application?
  Yes, but you are required to specify in documentation for your application:
    (1) that you used parts of the 7-Zip program,
    (2) that 7-Zip is licensed under the GNU LGPL license and
    (3) you must give a link to www.7-zip.org, where the source code can be found.

}

interface

{$I ..\mormot.defines.inc}

{$ifdef OSPOSIX}

// do-nothing-unit on non Windows system

implementation

{$else}

uses
  sysutils,
  classes,
  types,
  ActiveX,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime;


{ ****************** Low-Level 7-Zip API Definitions }

{$Z4} // 32-bit enums

type
  T7zFileTimeType = (
    fttNotDefined = -1,
    fttWindows = 0,
    fttUnix,
    fttDOS,
    ftt1ns);

  T7zArcInfoFlags = set of (
    aifKeepName,
    aifAltStreams,
    aifNtSecure,
    aifFindSignature,
    aifMultiSignature,
    aifUseGlobalOffset,
    aifStartOpen,
    aifPureStartOpen,
    aifBackwardOpen,
    aifPreArc,
    aifSymLinks,
    aifHardLinks,
    aifByExtOnlyOpen,
    aifHashHandler,
    aifCTime,
    aifCTime_Default,
    aifATime,
    aifATime_Default,
    aifMTime,
    aifMTime_Default);

const
  kTime_Prec_Mask_bit_index = 0;
  kTime_Prec_Mask_num_bits = 26;
  kTime_Prec_Default_bit_index = 27;
  kTime_Prec_Default_num_bits = 5;

function TIME_PREC_TO_ARC_FLAGS_MASK(x: cardinal): cardinal;
  {$ifdef HASINLINE} inline; {$endif}

function TIME_PREC_TO_ARC_FLAGS_TIME_DEFAULT(x: cardinal): cardinal;
  {$ifdef HASINLINE} inline; {$endif}

type
  T7zHandlerPropID = (
    hpiName,            // VT_BSTR
    hpiClassID,         // binary GUID in VT_BSTR
    hpiExtension,       // VT_BSTR
    hpiAddExtension,    // VT_BSTR
    hpiUpdate,          // VT_BOOL
    hpiKeepName,        // VT_BOOL
    hpiSignature,       // binary in VT_BSTR
    hpiMultiSignature,  // binary in VT_BSTR
    hpiSignatureOffset, // VT_UI4
    hpiAltStreams,      // VT_BOOL
    hpiNtSecure,        // VT_BOOL
    hpiFlags,           // VT_UI4
    hpiTimeFlags);      // VT_UI4

  T7zExtractAskMode = (
    eamExtract,
    eamTest,
    eamSkip,
    eamReadExternal);

  T7zExtractOperationResult =  (
    eorOK,
    eorUnsupportedMethod,
    eorDataError,
    eorCRCError,
    eorUnavailable,
    eorUnexpectedEnd,
    eorDataAfterEnd,
    eorIsNotArc,
    eorHeadersError,
    eorWrongPassword);

  T7zEventIndexType = (
    eitNoIndex,
    eitInArcIndex,
    eitBlockIndex,
    eitOutArcIndex);

  T7zUpdateOperationResult = (
    uorOK,
    uorError,
    uorError_FileChanged);

const
  kpidNoProperty       = 0;

  kpidHandlerItemIndex = 2;
  kpidPath             = 3;  // VT_BSTR
  kpidName             = 4;  // VT_BSTR
  kpidExtension        = 5;  // VT_BSTR
  kpidIsFolder         = 6;  // VT_BOOL
  kpidSize             = 7;  // VT_UI8
  kpidPackedSize       = 8;  // VT_UI8
  kpidAttributes       = 9;  // VT_UI4
  kpidCreationTime     = 10; // VT_FILETIME
  kpidLastAccessTime   = 11; // VT_FILETIME
  kpidLastWriteTime    = 12; // VT_FILETIME
  kpidSolid            = 13; // VT_BOOL
  kpidCommented        = 14; // VT_BOOL
  kpidEncrypted        = 15; // VT_BOOL
  kpidSplitBefore      = 16; // VT_BOOL
  kpidSplitAfter       = 17; // VT_BOOL
  kpidDictionarySize   = 18; // VT_UI4
  kpidCRC              = 19; // VT_UI4
  kpidType             = 20; // VT_BSTR
  kpidIsAnti           = 21; // VT_BOOL
  kpidMethod           = 22; // VT_BSTR
  kpidHostOS           = 23; // VT_BSTR
  kpidFileSystem       = 24; // VT_BSTR
  kpidUser             = 25; // VT_BSTR
  kpidGroup            = 26; // VT_BSTR
  kpidBlock            = 27; // VT_UI4
  kpidComment          = 28; // VT_BSTR
  kpidPosition         = 29; // VT_UI4
  kpidPrefix           = 30; // VT_BSTR
  kpidNumSubDirs       = 31; // VT_UI4
  kpidNumSubFiles      = 32; // VT_UI4
  kpidUnpackVer        = 33; // VT_UI1
  kpidVolume           = 34; // VT_UI4
  kpidIsVolume         = 35; // VT_BOOL
  kpidOffset           = 36; // VT_UI8
  kpidLinks            = 37; // VT_UI4
  kpidNumBlocks        = 38; // VT_UI4
  kpidNumVolumes       = 39; // VT_UI4
  kpidTimeType         = 40; // VT_UI4
  kpidBit64            = 41; // VT_BOOL
  kpidBigEndian        = 42; // VT_BOOL
  kpidCpu              = 43; // VT_BSTR
  kpidPhySize          = 44; // VT_UI8
  kpidHeadersSize      = 45; // VT_UI8
  kpidChecksum         = 46; // VT_UI4
  kpidCharacts         = 47; // VT_BSTR
  kpidVa               = 48; // VT_UI8

  kpidTotalSize        = $1100; // VT_UI8
  kpidFreeSpace        = $1101; // VT_UI8
  kpidClusterSize      = $1102; // VT_UI8
  kpidVolumeName       = $1103; // VT_BSTR

  kpidLocalName        = $1200; // VT_BSTR
  kpidProvider         = $1201; // VT_BSTR

  kpidUserDefined      = $10000;

{$Z1} // back to default enum size

type
  T7zVariant = variant;

  IProgress = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000000050000}']
    function SetTotal(total: Int64): HRESULT; stdcall;
    function SetCompleted(completeValue: PInt64): HRESULT; stdcall;
  end;

  ICryptoGetTextPassword = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000500100000}']
    function CryptoGetTextPassword(var password: TBStr): HRESULT; stdcall;
  end;

  ICryptoGetTextPassword2 = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000500110000}']
    function CryptoGetTextPassword2(passwordIsDefined: PInteger;
      var password: TBStr): HRESULT; stdcall;
  end;

  ISequentialInStream = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000300010000}']
    function Read(data: pointer; size: cardinal;
      processedSize: PCardinal): HRESULT; stdcall;
  end;

  ISequentialOutStream = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000300020000}']
    function Write(data: pointer; size: cardinal;
      processedSize: PCardinal): HRESULT; stdcall;
  end;

  IInStream = interface(ISequentialInStream)
    ['{23170F69-40C1-278A-0000-000300030000}']
    function Seek(offset: Int64; seekOrigin: cardinal;
      newPosition: PInt64): HRESULT; stdcall;
  end;

  IOutStream = interface(ISequentialOutStream)
    ['{23170F69-40C1-278A-0000-000300040000}']
    function Seek(offset: Int64; seekOrigin: cardinal;
      newPosition: PInt64): HRESULT; stdcall;
    function SetSize(newSize: Int64): HRESULT; stdcall;
  end;

  IStreamGetSize = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000300060000}']
    function GetSize(size: PInt64): HRESULT; stdcall;
  end;

  IOutStreamFlush = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000300070000}']
    function Flush: HRESULT; stdcall;
  end;

  IArchiveOpenCallback = interface
    ['{23170F69-40C1-278A-0000-000600100000}']
    function SetTotal(files, bytes: PInt64): HRESULT; stdcall;
    function SetCompleted(files, bytes: PInt64): HRESULT; stdcall;
  end;

  IArchiveExtractCallback = interface(IProgress)
    ['{23170F69-40C1-278A-0000-000600200000}']
    function GetStream(index: cardinal; var outStream: ISequentialOutStream;
      askExtractMode: T7zExtractAskMode): HRESULT; stdcall;
    function PrepareOperation(
      askExtractMode: T7zExtractAskMode): HRESULT; stdcall;
    function SetOperationResult(
      resultEOperationResult: T7zExtractOperationResult): HRESULT; stdcall;
  end;

  IArchiveOpenVolumeCallback = interface
    ['{23170F69-40C1-278A-0000-000600300000}']
    function GetProperty(propID: TPropID; var value: T7zVariant): HRESULT; stdcall;
    function GetStream(const name: PWideChar;
      var inStream: IInStream): HRESULT; stdcall;
  end;

  IInArchiveGetStream = interface
    ['{23170F69-40C1-278A-0000-000600400000}']
    function GetStream(index: cardinal;
      var stream: ISequentialInStream ): HRESULT; stdcall;
  end;

  IArchiveOpenSetSubArchiveName = interface
    ['{23170F69-40C1-278A-0000-000600500000}']
    function SetSubArchiveName(name: PWideChar): HRESULT; stdcall;
  end;

  IInArchive = interface
    ['{23170F69-40C1-278A-0000-000600600000}']
    function Open(stream: IInStream; const maxCheckStartPosition: PInt64;
      openArchiveCallback: IArchiveOpenCallback): HRESULT; stdcall;
    function Close: HRESULT; stdcall;
    function GetNumberOfItems(var numItems: cardinal): HRESULT; stdcall;
    function GetProperty(index: cardinal; propID: TPropID;
      var value: T7zVariant): HRESULT; stdcall;
    function Extract(indices: PCardinalArray; numItems: cardinal;
      testMode: integer; extractCallback: IArchiveExtractCallback): HRESULT; stdcall;
    function GetArchiveProperty(propID: TPropID; var value: T7zVariant): HRESULT; stdcall;
    function GetNumberOfProperties(numProperties: PCardinal): HRESULT; stdcall;
    function GetPropertyInfo(index: cardinal;
      name: PBSTR; propID: PPropID; varType: PVarType): HRESULT; stdcall;
    function GetNumberOfArchiveProperties(var numProperties: cardinal): HRESULT; stdcall;
    function GetArchivePropertyInfo(index: cardinal;
      name: PBSTR; propID: PPropID; varType: PVarType): HRESULT; stdcall;
  end;

  IArchiveUpdateCallback = interface(IProgress)
    ['{23170F69-40C1-278A-0000-000600800000}']
    function GetUpdateItemInfo(index: cardinal;
      newData, newProperties: PInteger; indexInArchive: PCardinal): HRESULT; stdcall;
    function GetProperty(index: cardinal; propID: TPropID;
      var value: T7zVariant): HRESULT; stdcall;
    function GetStream(index: cardinal;
      var inStream: ISequentialInStream): HRESULT; stdcall;
    function SetOperationResult(operationResult: integer): HRESULT; stdcall;
  end;

  IArchiveUpdateCallback2 = interface(IArchiveUpdateCallback)
    ['{23170F69-40C1-278A-0000-000600820000}']
    function GetVolumeSize(index: cardinal; size: PInt64): HRESULT; stdcall;
    function GetVolumeStream(index: cardinal;
      var volumeStream: ISequentialOutStream): HRESULT; stdcall;
  end;

  IOutArchive = interface
    ['{23170F69-40C1-278A-0000-000600A00000}']
    function UpdateItems(outStream: ISequentialOutStream; numItems: cardinal;
      updateCallback: IArchiveUpdateCallback): HRESULT; stdcall;
    function GetFileTimeType(type_: PCardinal): HRESULT; stdcall;
  end;

  ISetProperties = interface
    ['{23170F69-40C1-278A-0000-000600030000}']
    function SetProperties(names: PPWideChar; values: PVariant;
      numProperties: integer): HRESULT; stdcall;
  end;

  ICompressProgressInfo = interface
    ['{23170F69-40C1-278A-0000-000400040000}']
    function SetRatioInfo(inSize, outSize: PInt64): HRESULT; stdcall;
  end;

  ICompressCoder = interface
    ['{23170F69-40C1-278A-0000-000400050000}']
    function Code(inStream, outStream: ISequentialInStream;
      inSize, outSize: PInt64;
      progress: ICompressProgressInfo): HRESULT; stdcall;
  end;

  ICompressCoder2 = interface
    ['{23170F69-40C1-278A-0000-000400180000}']
    function Code(var inStreams: ISequentialInStream; var inSizes: PInt64;
      numInStreams: cardinal; var outStreams: ISequentialOutStream;
      var outSizes: PInt64; numOutStreams: cardinal;
      progress: ICompressProgressInfo): HRESULT; stdcall;
  end;

  ICompressSetCoderProperties = interface
    ['{23170F69-40C1-278A-0000-000400200000}']
    function SetCoderProperties(propIDs: PPropID;
      properties: PVariant; numProperties: cardinal): HRESULT; stdcall;
  end;

  ICompressSetDecoderProperties2 = interface
    ['{23170F69-40C1-278A-0000-000400220000}']
    function SetDecoderProperties2(
      data: PByte; size: cardinal): HRESULT; stdcall;
  end;

  ICompressWriteCoderProperties = interface
    ['{23170F69-40C1-278A-0000-000400230000}']
    function WriteCoderProperties(
      outStreams: ISequentialOutStream): HRESULT; stdcall;
  end;

  ICompressGetInStreamProcessedSize = interface
    ['{23170F69-40C1-278A-0000-000400240000}']
    function GetInStreamProcessedSize(value: PInt64): HRESULT; stdcall;
  end;

  ICompressSetCoderMt = interface
    ['{23170F69-40C1-278A-0000-000400250000}']
    function SetNumberOfThreads(numThreads: cardinal): HRESULT; stdcall;
  end;

  ICompressGetSubStreamSize = interface
    ['{23170F69-40C1-278A-0000-000400300000}']
    function GetSubStreamSize(subStream: Int64; value: PInt64): HRESULT; stdcall;
  end;

  ICompressSetInStream = interface
    ['{23170F69-40C1-278A-0000-000400310000}']
    function SetInStream(inStream: ISequentialInStream): HRESULT; stdcall;
    function ReleaseInStream: HRESULT; stdcall;
  end;

  ICompressSetOutStream = interface
    ['{23170F69-40C1-278A-0000-000400320000}']
    function SetOutStream(outStream: ISequentialOutStream): HRESULT; stdcall;
    function ReleaseOutStream: HRESULT; stdcall;
  end;

  ICompressSetInStreamSize = interface
    ['{23170F69-40C1-278A-0000-000400330000}']
    function SetInStreamSize(inSize: PInt64): HRESULT; stdcall;
  end;

  ICompressSetOutStreamSize = interface
    ['{23170F69-40C1-278A-0000-000400340000}']
    function SetOutStreamSize(outSize: PInt64): HRESULT; stdcall;
  end;

  ICompressFilter = interface
    ['{23170F69-40C1-278A-0000-000400400000}']
    function Init: HRESULT; stdcall;
    function Filter(data: PByte; size: cardinal): cardinal; stdcall;
  end;

  ICryptoProperties = interface
    ['{23170F69-40C1-278A-0000-000400800000}']
    function SetKey(Data: PByte; size: cardinal): HRESULT; stdcall;
    function SetInitVector(data: PByte; size: cardinal): HRESULT; stdcall;
  end;

  ICryptoSetPassword = interface
    ['{23170F69-40C1-278A-0000-000400900000}']
    function CryptoSetPassword(data: PByte; size: cardinal): HRESULT; stdcall;
  end;

  ICryptoSetCRC = interface
    ['{23170F69-40C1-278A-0000-000400A00000}']
    function CryptoSetCRC(crc: cardinal): HRESULT; stdcall;
  end;


{ ****************** I7zReadArchive/I7zWriter High-Level Wrappers }

type
  /// kind of exceptions raised by this unit
  E7zip = class(ExceptionWithProps)
  protected
    class procedure RaiseAfterCheck(Caller: TObject; const Context: shortstring;
      Res: HResult);
  public
    class procedure Check(Caller: TObject; const Context: shortstring;
      Res: HResult);
      {$ifdef HASINLINE} inline; {$endif}
    class procedure CheckOk(Caller: TObject; const Context: shortstring;
      Res: HResult);
      {$ifdef HASINLINE} inline; {$endif}
  end;

{
  Discaimer:
   These wrapper interfaces were inspired by MPL 2.0 Licenced code available at
     https://github.com/PascalVault/Lazarus_7zip
}

type
  /// the supported archive formats
  // - an enumerate is easier and safer to work with than TGuid constants
  // - use T7zLib.FormatGuid to retrieve the TGuid of a given archive format
  T7zFormatHandler = (
    fhUndefined,
    fhZip,
    fhBZip2,
    fhRar,
    fhArj,
    fhZ,
    fhLzh,
    fh7z,
    fhCab,
    fhNsis,
    fhlzma,
    fhlzma86,
    fhxz,
    fhppmd,
    fhAVB,
    fhLP,
    fhSparse,
    fhAPFS,
    fhVhdx,
    fhBase64,
    fhCOFF,
    fhExt,
    fhVMDK,
    fhVDI,
    fhQcow,
    fhGPT,
    fhRar5,
    fhIHex,
    fhHxs,
    fhTE,
    fhUEFIc,
    fhUEFIs,
    fhSquashFS,
    fhCramFS,
    fhAPM,
    fhMslz,
    fhFlv,
    fhSwf,
    fhSwfc,
    fhNtfs,
    fhFat,
    fhMbr,
    fhVhd,
    fhPe,
    fhElf,
    fhMachO,
    fhUdf,
    fhXar,
    fhMub,
    fhHfs,
    fhDmg,
    fhCompound,
    fhWim,
    fhIso,
    fhChm,
    fhSplit,
    fhRpm,
    fhDeb,
    fhCpio,
    fhTar,
    fhGZip);

  // note: the sender of following events is a I7zReadArchive or I7zWriter
  
  /// event as used by I7zReadArchive.SetPasswordCallback
  // - should return S_OK to continue the execution, or something else to abort
  T7zPasswordCallback = function(sender: TInterfacedObject;
    var password: SynUnicode): HRESULT of object;
  /// event as used by I7zReadArchive.Extract/ExtractAll methods
  // - should return S_OK to continue the execution, or something else to abort
  T7zGetStreamCallBack = function(sender: TInterfacedObject; index: cardinal;
    var outStream: ISequentialOutStream): HRESULT of object;
  /// event as used by I7zReadArchive/I7zWriter.SetProgressCallback method
  // - should return S_OK to continue the execution, or something else to abort,
  // e.g. ERROR_OPERATION_ABORTED
  T7zProgressCallback = function(sender: TInterfacedObject;
    current, total: Int64): HRESULT of object;

  I7zReader = interface
    ['{9C0C0C8C-883A-49ED-B608-A6D66D36D53A}']
    // -- some internal methods used as property getters / setters
    function GetItemCRC(index: integer): cardinal;
    function GetItemPackSize(index: integer): Int64;
    function GetItemComment(index: integer): RawUtf8;
    function GetItemModDate(index: integer): TDateTime;
    function GetItemDate(index: integer): TDateTime;
    function GetItemPath(index: integer): RawUtf8;
    function GetItemName(index: integer): RawUtf8;
    function GetItemSize(index: integer): Int64;
    function GetItemIsFolder(index: integer): boolean;
    // -- main high-level methods
    /// open a given archive file
    procedure OpenFile(const filename: TFileName);
    /// open an archive content from a stream
    procedure OpenStream(stream: IInStream);
    /// close the archive
    procedure Close;
    /// the number of files in this archive
    // - used as index in Extract method and Item*[0..Count-1] properties
    function Count: integer;
    /// search for a given file name in this archive
    // - case-insensitive search within ItemPath[] item
    // - returns -1 if not found, or the index in Item*[] properties
    function NameToIndex(const name: RawUtf8): integer;
    /// uncompress a file by index from this archive into a stream
    // - if no Stream is specified, will test for the output
    procedure Extract(item: cardinal; Stream: TStream); overload;
    /// uncompress a file by name from this archive into a stream
    // - if no Stream is specified, will test for the output
    function Extract(const name: RawUtf8; Stream: TStream): boolean; overload;
    /// uncompress a file by name from this archive into a RawByteString
    function Extract(const name: RawUtf8): RawByteString; overload;
    /// uncompress several files from this archive using a callback per file
    // - if no Callback is specified (as default), will test for the output
    procedure Extract(const items: array of integer;
      const callback: T7zGetStreamCallBack = nil); overload;
    /// uncompress all files from this archive using a callback per file
    // - if no Callback is specified (as default), will test for the output
    procedure ExtractAll(const callback: T7zGetStreamCallBack = nil); overload;
    /// uncompress all files from this archive into a given local folder
    procedure ExtractAll(const path: TFileName; nosubfolder: boolean = false); overload;
    /// let an event be called to let the user supply a password 
    procedure SetPasswordCallback(const callback: T7zPasswordCallback);
    /// supply a password  
    procedure SetPassword(const password: SynUnicode);
    /// let an event be called during file decompression
    procedure SetProgressCallback(const callback: T7zProgressCallback);
    /// access to the 7-zip internal API raw object
    function InArchive: IInArchive;
    /// the high-level identifier of this archive format
    function Format: T7zFormatHandler;
    /// the 7-zip internal CSLID of this archive format
    function ClassId: TGuid;
    /// the kpidPath property value of an archived file
    // - i.e. its usual file name, potentially with a relative path
    // - e.g. 'toto.txt' or 'rep\toto.bmp'
    property ItemPath[index: integer]: RawUtf8
      read GetItemPath;
    /// the kpidName property value of an archived file
    // - is likely to be '' for most packers
    property ItemName[index: integer]: RawUtf8
      read GetItemName;
    /// the kpidSize property value of an archived file
    property ItemSize[index: integer]: Int64
      read GetItemSize;
    /// the kpidIsFolder property value of an archived file
    property ItemIsFolder[index: integer]: boolean
      read GetItemIsFolder;
    /// the kpidComment property value of an archived file
    property ItemComment[index: integer]: RawUtf8
      read GetItemComment;
    /// the kpidCRC property value of an archived file
    property ItemCRC[index: integer]: cardinal
      read GetItemCRC;
    /// the kpidCreationTime property value
    // - may be 0 for some packers
    property ItemDate[index: integer]: TDateTime
      read GetItemDate;
    /// the kpidLastWriteTime property value - i.e. the "usual" date
    property ItemModDate[index: integer]: TDateTime
      read GetItemModDate;
    /// the kpidPackedSize property value
    property ItemPackSize[index: integer]: Int64
      read GetItemPackSize;
  end;

  TZipCompressionMethod = (
    mzCopy,
    mzDeflate,
    mzDeflate64,
    mzBZip2,
    mzLzma,
    mzPpmd);

  TZipEncryptionMethod = (
    emAes128,
    emAes192,
    emAes256,
    emZipCrypto);

  T7zCompressionMethod = (
    m7Copy,
    m7Lzma,
    m7BZip2,
    m7Ppmd,
    m7Deflate,
    m7Deflate64);

  I7zWriter = interface
    ['{9C0C0C8C-883A-49ED-B608-A6D66D36D53B}']
    // main high-level methods
    procedure AddStream(Stream: TStream; Ownership: TStreamOwnership;
      Attributes: cardinal; CreationTime, LastWriteTime: TFileTime;
      const Path: RawUtf8; IsFolder, IsAnti: boolean);
    procedure AddFile(const Filename: TFileName; const Path: RawUtf8);
    procedure AddFiles(const Dir, Path, Wildcard: TFileName; recurse: boolean);
    procedure SaveToFile(const FileName: TFileName);
    procedure SaveToStream(stream: TStream);
    procedure SetProgressCallback(const callback: T7zProgressCallback);
    procedure ClearBatch;
    procedure SetPassword(const password: SynUnicode);
    procedure SetProperty(const name: RawUtf8; const value: T7zVariant);
    procedure SetCompressionLevel(level: cardinal);
    procedure SetMultiThreading(threadCount: cardinal);
    procedure SetCompressionMethod(method: TZipCompressionMethod);
    procedure SetEncryptionMethod(method: TZipEncryptionMethod);
    procedure SetDictionnarySize(size: cardinal);
    procedure SetMemorySize(size: cardinal);
    procedure SetDeflateNumPasses(pass: cardinal);
    procedure SetNumFastBytes(fb: cardinal);
    procedure SetNumMatchFinderCycles(mc: cardinal);
    procedure SetCompressionMethod7z(method: T7zCompressionMethod);
    procedure SetBindInfo7z(const bind: WideString);
    procedure SetSolidSettings7z(solid: boolean);
    procedure RemoveSfxBlock7z(remove: boolean);
    procedure AutoFilter7z(auto: boolean);
    procedure CompressHeaders7z(compress: boolean);
    procedure CompressHeadersFull7z(compress: boolean);
    procedure EncryptHeaders7z(encrypt: boolean);
    procedure VolumeMode7z(mode: boolean);
    function OutArchive: IOutArchive;
    function Format: T7zFormatHandler;
    function ClassId: TGuid;
  end;

  I7zCodec = interface
    ['{9C0C0C8C-883A-49ED-B608-A6D66D36D53C}']
  end;


type
  /// this is the main loader and factory for 7z.dll
  I7zLib = interface
    /// the library .dll file name as loaded by Create()
    function FileName: TFileName;
    /// factory of the main I7zReader high-level archive decompressor
    function NewReader(fmt: T7zFormatHandler): I7zReader; overload;
    /// factory of the main I7zReader high-level archive file decompressor
    // - will guess the file format from its existing content
    function NewReader(const name: TFileName): I7zReader; overload;
    /// factory of the main I7zWriter high-level archive compressor
    function NewWriter(fmt: T7zFormatHandler): I7zWriter;
  end;

  /// implement the main loader and factory for 7z.dll
  // - you can instantiate this class and store it into a I7zLib instance,
  // then use NewReader/NewWriter factories when needed: just ensure
  // the I7zlib instance is released last
  // - as alternative, you can use global New7zReader/New7zWriter factories
  T7zLib = class(TInterfacedObject, I7zLib)
  protected
    fHandle: THandle;
    fFileName: TFileName;
    fFormat: T7zFormatHandler;
    // the raw function from 7z.dll
    fCreateObject: function(const clsid, iid: TGuid; var outObject): HRESULT; stdcall;
    function TryLoad(const libname: TFileName): boolean;
    /// main encapsulated API factory of the library
    procedure CreateObject(const clsid, iid: TGuid; var obj);
  public
    /// return the GUID of a given archive format
    // - in the form '{23170F69-40C1-278A-1000-000110xx0000}'
    class function FormatGuid(fmt: T7zFormatHandler): TGuid;
    /// try to detect the proper archive format of a file
    // - will read the file header first, unless OnlyFileName is set
    class function FormatDetect(const FileName: TFileName;
      OnlyFileName: boolean = false): T7zFormatHandler;
    /// return the known file extensions of a given archive format as 'ext1;ext2'
    class function FormatFileExtensions(fmt: T7zFormatHandler): TFileName;
    /// load the 7z.dll
    constructor Create(lib: TFileName = ''); reintroduce;
    /// unload the 7z.dll
    destructor Destroy; override;
    /// I7zLib methods
    function FileName: TFileName;
    function NewReader(fmt: T7zFormatHandler): I7zReader; overload;
    function NewReader(const name: TFileName): I7zReader; overload;
    function NewWriter(fmt: T7zFormatHandler): I7zWriter;
  end;


/// global factory of the main I7zReader high-level archive file decompressor
// - will guess the file format from its existing content
// - will own its own TZlib instance to access the 7z.dll library 
function New7zReader(const name: TFileName; const lib: TFileName = ''): I7zReader;

/// global factory of the main I7zWriter high-level archive compressor
// - will own its own TZlib instance to access the 7z.dll library 
function New7zWriter(fmt: T7zFormatHandler; const lib: TFileName = ''): I7zWriter;


implementation


{ ****************** Low-Level 7-Zip API Definitions }

function TIME_PREC_TO_ARC_FLAGS_MASK(x: cardinal): cardinal;
begin
  result := cardinal(1) shl (kTime_Prec_Mask_bit_index + x);
end;

function TIME_PREC_TO_ARC_FLAGS_TIME_DEFAULT(x: cardinal): cardinal;
begin
  result := x shl kTime_Prec_Default_bit_index;
end;


{ ****************** I7zReader/I7zWriter High-Level Wrappers }

{ E7zip }

class procedure E7zip.RaiseAfterCheck(Caller: TObject;
  const Context: shortstring; Res: HResult);
begin
  raise CreateFmt('%s.%s error %x (%s)',
    [ClassNameShort(Caller)^, Context, Res, string(WinErrorText(Res, nil))])
end;

class procedure E7zip.Check(Caller: TObject; const Context: shortstring;
  Res: HResult);
begin
  if Res and $80000000 <> 0 then
    RaiseAfterCheck(Caller, Context, Res);
end;

class procedure E7zip.CheckOk(Caller: TObject; const Context: shortstring;
  Res: HResult);
begin
  if Res <> S_OK then
    RaiseAfterCheck(Caller, Context, Res);
end;


type
  /// the properties identifiers of a 7-zip
  T7zMethodPropID = (
    kID,
    kName,
    kDecoder,
    kEncoder,
    kInStreams,
    kOutStreams,
    kDescription,
    kDecoderIsAssigned,
    kEncoderIsAssigned
  );

  T7zStream = class(TInterfacedObject,
    ISequentialInStream, IInStream, IStreamGetSize,
    ISequentialOutStream, IOutStream, IOutStreamFlush)
  private
    fStream: TStream;
    fOwnerShip: TStreamOwnership;
  protected
    // ISequentialInStream method
    function Read(data: pointer; size: cardinal;
      processedSize: PCardinal): HRESULT; stdcall;
    // IInStream and IOutStream method
    function Seek(offset: Int64; seekOrigin: cardinal;
      newPosition: PInt64): HRESULT; stdcall;
    // IStreamGetSize method
    function GetSize(size: PInt64): HRESULT; stdcall;
    // ISequentialOutStream method
    function Write(data: pointer; size: cardinal;
      processedSize: PCardinal): HRESULT; stdcall;
    // IOutStream method
    function SetSize(newSize: Int64): HRESULT; stdcall;
    // IOutStreamFlush method
    function Flush: HRESULT; stdcall;
  public
    constructor Create(Stream: TStream; Ownership: TStreamOwnership);
    constructor CreateFromFile(const Name: TFileName; Mode: cardinal);
    destructor Destroy; override;
  end;

  T7zParent = class(TInterfacedObject)
  private
    fOwner: T7zLib;
  public
    constructor Create(lib: T7zLib); reintroduce; virtual;
  end;

  T7zCodec = class(T7zParent,
    I7zCodec, ICompressProgressInfo)
  private
    // the raw functions from 7z.dll
    fGetMethodProperty: function(index: cardinal; propID: T7zMethodPropID;
      var value: T7zVariant): HRESULT; stdcall;
    fGetNumberOfMethods: function(numMethods: PCardinal): HRESULT; stdcall;
  protected
    function GetNumberOfMethods: cardinal;
    function GetMethodProperty(index: cardinal; propID: T7zMethodPropID): T7zVariant;
    function GetName(index: integer): string;
    // ICompressProgressInfo method
    function SetRatioInfo(inSize, outSize: PInt64): HRESULT; stdcall;
  public
    function GetDecoder(index: integer): ICompressCoder;
    function GetEncoder(index: integer): ICompressCoder;
    constructor Create(lib: T7zLib); override;
    property MethodProperty[index: cardinal; propID: T7zMethodPropID]: T7zVariant
      read GetMethodProperty;
    property NumberOfMethods: cardinal
      read GetNumberOfMethods;
    property Name[index: integer]: string
      read GetName;
  end;

  T7zArchive = class(T7zParent)
  private
    // the raw function from 7z.dll
    fGetHandlerProperty: function(propID: T7zHandlerPropID;
      var value: T7zVariant): HRESULT; stdcall;
  protected
    fClassId: TGuid;
    fFormat: T7zFormatHandler;
    fLibOwned: I7zLib;
    fProgressCallback: T7zProgressCallback;
    fProgressCurrent: Int64;
    fProgressTotal: Int64;
    function GetLibStringProperty(index: T7zHandlerPropID): string;
    function GetLibGUIDProperty(index: T7zHandlerPropID): TGuid;
    procedure SetProgressCallback(const callback: T7zProgressCallback);
    // IProgress methods
    function SetTotal(total: Int64): HRESULT; overload; stdcall;
    function SetCompleted(completeValue: PInt64): HRESULT; overload; stdcall;
  public
    constructor Create(lib: T7zLib; fmt: T7zFormatHandler;
      libowned: boolean); reintroduce;
    function HandlerProperty(propID: T7zHandlerPropID): T7zVariant;
    property Name: string
      index hpiName read GetLibStringProperty;
    function Format: T7zFormatHandler;
    function ClassId: TGuid;
    property Extension: string
      index hpiExtension read GetLibStringProperty;
  end;

  T7zReader = class(T7zArchive,
    I7zReader, IProgress, IArchiveOpenCallback,
    IArchiveExtractCallback, ICryptoGetTextPassword,
    IArchiveOpenVolumeCallback, IArchiveOpenSetSubArchiveName)
  private
    fInArchive: IInArchive;
    fPasswordCallback: T7zPasswordCallback;
    fStream: TStream;
    fPasswordIsDefined: boolean;
    fPassword: SynUnicode;
    fSubArchiveMode: boolean;
    fSubArchiveName: RawUtf8;
    fExtractCallback: T7zGetStreamCallBack;
    fExtractPath: TFileName;
    fExtractPathNoSubFolder: boolean;
    function GetItemProp(item: cardinal; prop: TPropID): T7zVariant;
    function GetItemPropDateTime(item: cardinal; prop: TPropID): TDateTime;
    procedure EnsureOpened;
  protected
    // I7zReader methods
    procedure OpenFile(const filename: TFileName);
    procedure OpenStream(stream: IInStream);
    procedure Close;
    function InArchive: IInArchive;
    function Count: integer;
    function NameToIndex(const name: RawUtf8): integer;
    function GetItemPath(index: integer): RawUtf8;
    function GetItemName(index: integer): RawUtf8;
    function GetItemSize(index: integer): Int64;
    function GetItemIsFolder(index: integer): boolean;
    procedure Extract(item: cardinal; Stream: TStream); overload;
    function Extract(const name: RawUtf8; Stream: TStream): boolean; overload;
    function Extract(const name: RawUtf8): RawByteString; overload;
    procedure Extract(const items: array of integer;
      const callback: T7zGetStreamCallBack); overload;
    procedure SetPasswordCallback(const callback: T7zPasswordCallback);
    procedure ExtractAll(const callback: T7zGetStreamCallBack); overload;
    procedure ExtractAll(const path: TFileName; nosubfolder: boolean); overload;
    procedure SetPassword(const password: SynUnicode);
    function GetItemPackSize(index: integer): Int64;
    function GetItemCRC(index: integer): cardinal;
    function GetItemComment(index: integer): RawUtf8;
    function GetItemModDate(index: integer): TDateTime;
    function GetItemDate(index: integer): TDateTime;
    // IArchiveOpenCallback
    function SetTotal(files, bytes: PInt64): HRESULT; overload; stdcall;
    function SetCompleted(files, bytes: PInt64): HRESULT; overload; stdcall;
    // IArchiveExtractCallback
    function GetStream(index: cardinal; var outStream: ISequentialOutStream;
      askExtractMode: T7zExtractAskMode): HRESULT; overload; stdcall;
    function PrepareOperation(askExtractMode: T7zExtractAskMode): HRESULT; stdcall;
    function SetOperationResult(
      resultEOperationResult: T7zExtractOperationResult): HRESULT; overload; stdcall;
    // ICryptoGetTextPassword
    function CryptoGetTextPassword(var password: TBStr): HRESULT; stdcall;
    // IArchiveOpenVolumeCallback
    function GetProperty(propID: TPropID;
      var value: T7zVariant): HRESULT; overload; stdcall;
    function GetStream(const name: PWideChar;
      var inStream: IInStream): HRESULT; overload; stdcall;
    // IArchiveOpenSetSubArchiveName
    function SetSubArchiveName(name: PWideChar): HRESULT; stdcall;
  end;

  T7zItemSourceMode = (
    smStream,
    smFile);

  T7zItem = class
  public
    Stream: TStream;
    Attributes: cardinal;
    SourceMode: T7zItemSourceMode;
    Ownership: TStreamOwnership;
    IsFolder, IsAnti: boolean;
    CreationTime, LastWriteTime: TFileTime;
    Path: RawUtf8;
    FileName: TFileName;
    Size: Int64;
    destructor Destroy; override;
  end;

  T7zWriter = class(T7zArchive,
    I7zWriter, IArchiveUpdateCallback, ICryptoGetTextPassword2)
  private
    fOutArchive: IOutArchive;
    fEntries: array of T7zItem;
    fPassword: SynUnicode;
  protected
    procedure SetCardinalProperty(const name: RawUtf8; card: cardinal);
    procedure SetTextProperty(const name, text: RawUtf8);
    // I7zWriter methods
    procedure AddStream(Stream: TStream; Ownership: TStreamOwnership;
      Attributes: cardinal; CreationTime, LastWriteTime: TFileTime;
      const Path: RawUtf8; IsFolder, IsAnti: boolean);
    procedure AddFile(const Filename: TFileName; const Path: RawUtf8);
    procedure AddFiles(const Dir, Path, Wildcard: TFileName; recurse: boolean);
    procedure SaveToFile(const FileName: TFileName);
    procedure SaveToStream(stream: TStream);
    procedure ClearBatch;
    procedure SetPassword(const password: SynUnicode);
    procedure SetProperty(const name: RawUtf8; const value: T7zVariant);
    procedure SetCompressionLevel(level: cardinal);
    procedure SetMultiThreading(threadCount: cardinal);
    procedure SetCompressionMethod(method: TZipCompressionMethod);
    procedure SetEncryptionMethod(method: TZipEncryptionMethod);
    procedure SetDictionnarySize(size: cardinal); 
    procedure SetMemorySize(size: cardinal);
    procedure SetDeflateNumPasses(pass: cardinal);
    procedure SetNumFastBytes(fb: cardinal);
    procedure SetNumMatchFinderCycles(mc: cardinal);
    procedure SetCompressionMethod7z(method: T7zCompressionMethod);
    procedure SetBindInfo7z(const bind: WideString);
    procedure SetSolidSettings7z(solid: boolean);
    procedure RemoveSfxBlock7z(remove: boolean);
    procedure AutoFilter7z(auto: boolean);
    procedure CompressHeaders7z(compress: boolean);
    procedure CompressHeadersFull7z(compress: boolean);
    procedure EncryptHeaders7z(encrypt: boolean);
    procedure VolumeMode7z(mode: boolean);
    function OutArchive: IOutArchive;
    // IArchiveUpdateCallback methods
    function GetUpdateItemInfo(index: cardinal; newData, newProperties: PInteger;
      indexInArchive: PCardinal): HRESULT;  stdcall;
    function GetProperty(index: cardinal; propID: TPropID;
      var value: T7zVariant): HRESULT; stdcall;
    function GetStream(index: cardinal;
      var inStream: ISequentialInStream): HRESULT;  stdcall;
    function SetOperationResult(operationResult: integer): HRESULT;  stdcall;
    // ICryptoGetTextPassword2 methods
    function CryptoGetTextPassword2(passwordIsDefined: PInteger;
      var password: TBStr): HRESULT;  stdcall;
  public
    destructor Destroy; override;
  end;


{ T7zLib }

const
  // see Guid.txt: {23170F69-40C1-278A-1000-000110xx0000}
  HANDLER_TO_GUID: array[T7zFormatHandler] of byte = (
    $00,  // undefined
    $01,  // Zip
    $02,  // BZip2
    $03,  // Rar
    $04,  // Arj
    $05,  // Z
    $06,  // Lzh
    $07,  // 7z
    $08,  // Cab
    $09,  // Nsis
    $0A,  // lzma
    $0B,  // lzma86
    $0C,  // xz
    $0D,  // ppmd
    $C0,  // AVB
    $C1,  // LP
    $C2,  // Sparse
    $C3,  // APFS
    $C4,  // Vhdx
    $C5,  // Base64
    $C6,  // COFF
    $C7,  // Ext
    $C8,  // VMDK
    $C9,  // VDI
    $CA,  // Qcow
    $CB,  // GPT
    $CC,  // Rar5
    $CD,  // IHex
    $CE,  // Hxs
    $CF,  // TE
    $D0,  // UEFIc
    $D1,  // UEFIs
    $D2,  // SquashFS
    $D3,  // CramFS
    $D4,  // APM
    $D5,  // Mslz
    $D6,  // Flv
    $D7,  // Swf
    $D8,  // Swfc
    $D9,  // Ntfs
    $DA,  // Fat
    $DB,  // Mbr
    $DC,  // Vhd
    $DD,  // Pe
    $DE,  // Elf
    $DF,  // Mach-O
    $E0,  // Udf
    $E1,  // Xar
    $E2,  // Mub
    $E3,  // Hfs
    $E4,  // Dmg
    $E5,  // Compound
    $E6,  // Wim
    $E7,  // Iso
    $E9,  // Chm
    $EA,  // Split
    $EB,  // Rpm
    $EC,  // Deb
    $ED,  // Cpio
    $EE,  // Tar
    $EF); // GZip
  HANDLER_GUID: TGuid = '{23170F69-40C1-278A-1000-000110ff0000}';

class function T7zLib.FormatGuid(fmt: T7zFormatHandler): TGuid;
begin
  result := HANDLER_GUID;
  result.D4[5] := HANDLER_TO_GUID[fmt];
end;


const
  FILE_EXT: array[0..71] of PUtf8Char = (
    '001',
    'APM',
    'B64',
    'BZIP2', 'TBZ2', 'BZIP', 'TBZ',
    'CAB',
    'CHM',
    'CRAMFS',
    'DEB',
    'DMG',
    'DOCX', 'PPTX', 'XLSX', 'ZIP', 'JAR', 'XPI', 'ODT', 'ODS',
    'DOC', 'PPT', 'XLS', 'MSI', 'MSP',
    'EXE', 'DLL',
    'EXT2', 'EXT3', 'EXT4', 'EXT',
    'FAT',
    'FLV',
    'HFSX', 'HFS',
    'HXS', 'HXI', 'HXR', 'HXQ', 'HXW', 'LIT',
    'IHEX',
    'ISO',
    'LZH', 'LHA',
    'LZMA',
    'MBR',
    'MSLZ',
    'MUB',
    'NSIS',
    'NTFS',
    'PPMD',
    'RAR',
    'RPM',
    'SCAP',
    'SQUASHFS',
    'SWFC',
    'SWF',
    'TAR',
    'TGZ', 'GZIP', 'GZ',
    'UDF',
    'UEFIF',
    'VDI',
    'VMDK',
    'XAR', 'PKG',
    'XZ', 'TXZ',
    'WIM',
    nil
  );
  FILE_EXT_TYPE: array[0 .. high(FILE_EXT) - 1] of T7zFormatHandler = (
    fhSplit,
    fhAPM,
    fhBase64,
    fhBZip2, fhBZip2, fhBZip2, fhBZip2,
    fhCab,
    fhChm,
    fhCramFS,
    fhDeb,
    fhDmg,
    fhZip, fhZip, fhZip, fhZip, fhZip, fhZip, fhZip, fhZip,
    fhCompound, fhCompound, fhCompound, fhCompound, fhCompound,
    fhPe, fhPe,
    fhExt, fhExt, fhExt, fhExt,
    fhFat,
    fhFlv,
    fhHfs, fhHfs,
    fhHxs, fhHxs, fhHxs, fhHxs, fhHxs, fhHxs,
    fhIHex,
    fhIso,
    fhLzh, fhLzh,
    fhlzma,
    fhMbr,
    fhMslz,
    fhMub,
    fhNsis,
    fhNtfs,
    fhppmd,
    fhRar,
    fhRpm,
    fhUEFIc,
    fhSquashFS,
    fhSwfc,
    fhSwf,
    fhTar,
    fhGZip, fhGZip, fhGZip,
    fhUdf,
    fhUEFIs,
    fhVDI,
    fhVMDK,
    fhXar, fhXar,
    fhxz, fhxz,
    fhWim
  );

class function T7zLib.FormatDetect(const FileName: TFileName;
  OnlyFileName: boolean): T7zFormatHandler;
var
  h: THash128Rec;
  f: THandle;
  i, l: PtrInt;
  ext: RawUtf8;
begin
  result := fhUndefined;
  if FileName = '' then
    exit;
  // first try to identify from binary header
  if not OnlyFileName then
  begin
    f := FileOpen(FileName, fmOpenReadDenyNone);
    if not ValidHandle(f) then
      exit;
    l := FileRead(f, h, SizeOf(h));
    FileClose(f);
    if l <> SizeOf(h) then
      exit;
    case h.c[0] of
      $04034b50:
        result := fhZip;
      $21726152:
        if h.c[1] = $0001071a then
          result := fhRar
        else
          result := fhRar5;
      $afbc7a37:
        result := fh7z;
      $28635349:
        result := fhCab;
      $005a587a:
        result := fhxz;
      $84acaf8f,
      $8fafac84:
        result := fhppmd;
      $46535449:
        result := fhChm;
      $4957534d:
        result := fhWim;
      $37303730:
        result := fhCpio;
      $72613c21:
        result := fhDeb;
      $464c457f:
        result := fhElf;
    else
      case h.c[0] and $00ffffff of
        $685a42:
          result := fhBZip2;
        $088b1f:
          result := fhGzip;
        $564c46:
          result := fhFlv;
        $535746:
          result := fhSwf;
        $535743:
          result := fhSwfc;
        $494651:
          result := fhQcow;
      else
        case h.w[0] of
          $9d1f:
            result := fhZ;
          $ea60:
            result := fhArj;
          $c771,
          $71c7:
            result := fhCpio;
          $5a4d:
            result := fhPe;
        end;
      end;
    end;
    if result <> fhUndefined then
      exit;
  end;
  // fallback to guess from file extension
  ext := RawUtf8(ExtractFileExt(FileName));
  delete(ext, 1, 1);
  l := length(ext);
  if l = 1 then
    case ext[1] of
      'Z', 'z':
        result := fhZ;
    end;
  if (l <= 1) or // IdemPPChar() from 2 chars to 4 chars
     (l > 4) then
    exit;
  i := IdemPPChar(pointer(ext), @FILE_EXT);
  if (i >= 0) and
     (StrLen(FILE_EXT[i]) = l) then
    result := FILE_EXT_TYPE[i]
end;

var
  // internal FormatFileExtensions() cache
  FILE_EXT_NAME: array[T7zFormatHandler] of TFileName;

class function T7zLib.FormatFileExtensions(fmt: T7zFormatHandler): TFileName;
var
  i: PtrInt;
begin
  result := FILE_EXT_NAME[fmt];
  if result = ';' then
  begin
    result := ''; // unsupported
    exit;
  end;
  if (fmt = fhUndefined) or
     (result <> '') then
    exit;
  for i := 0 to high(FILE_EXT_TYPE) do
    if FILE_EXT_TYPE[i] = fmt then
      result := FormatString('%.%;', [result, LowerCase(RawUtf8(FILE_EXT[i]))]);
  if result = '' then
    result := ';'
  else
    SetLength(result, length(result) - 1);
end;

function T7zLib.TryLoad(const libname: TFileName): boolean;
begin
  if libname = '' then
  begin
    result := false;
    exit;
  end;
  fHandle := LibraryOpen(libname);
  result := ValidHandle(fHandle);
  if result then
    fFileName := libname;
end;

var
  LastFoundDll: TFileName; // do the folders ressearch once if possible
  
constructor T7zLib.Create(lib: TFileName);
begin
  if lib <> '' then
    TryLoad(lib)
  else
    // search in exe and 7-Zip folder, trying any possible .dll file name
    if not TryLoad(LastFoundDll) or
       not TryLoad(Executable.ProgramFilePath + '7z.dll') or
       not TryLoad(Executable.ProgramFilePath + '7-zip.dll') or
       {$ifdef CPU32}
       not TryLoad(Executable.ProgramFilePath + '7-zip32.dll') or
       {$endif CPU32}
       not TryLoad(Executable.ProgramFilePath + '7za.dll') or
       not TryLoad(Executable.ProgramFilePath + '7zxa.dll') or
       not TryLoad('c:\Program Files\7-Zip\7z.dll') or
       {$ifdef CPU32}
       not TryLoad('c:\Program Files (x86)\7-Zip\7z.dll') or
       {$endif CPU32}
       not TryLoad('7z.dll') then
      lib := '7z.dll'
    else
      LastFoundDll := fFileName;
  if fFileName = '' then
    raise E7zip.CreateFmt('Error loading %s library', [lib]);
  fCreateObject := LibraryResolve(fHandle, 'CreateObject');
  if not Assigned(fCreateObject) then
  begin
    fFileName := '';
    LastFoundDll := '';
    raise E7zip.CreateFmt('%s is not a 7zip library', [lib]);
  end;
end;

destructor T7zLib.Destroy;
begin
  if fHandle <> 0 then
    LibraryClose(fHandle);
  inherited Destroy;
end;

function T7zLib.FileName: TFileName;
begin
  result := fFileName;
end;

procedure T7zLib.CreateObject(const clsid, iid: TGuid; var obj);
begin
  E7zip.Check(self, 'CreateObject', fCreateObject(clsid, iid, obj));
end;

function T7zLib.NewReader(fmt: T7zFormatHandler): I7zReader;
begin
  result := T7zReader.Create(self, fmt, {libowned=}false);
end;

function T7zLib.NewReader(const name: TFileName): I7zReader;
begin
  result := T7zReader.Create(self, FormatDetect(name), {libowned=}false);
  result.OpenFile(name);
end;

function T7zLib.NewWriter(fmt: T7zFormatHandler): I7zWriter;
begin
  result := T7zWriter.Create(self, fmt, {libowned=}false);
end;


function New7zReader(const name: TFileName; const lib: TFileName): I7zReader;
begin
  result := T7zReader.Create(
    T7zLib.Create(lib), T7zLib.FormatDetect(name), {libowned=}true);
  result.OpenFile(name);
end;

function New7zWriter(fmt: T7zFormatHandler; const lib: TFileName): I7zWriter;
begin
  result := T7zWriter.Create(T7zLib.Create(lib), fmt, {libowned=}true);
end;


// --- here below are the actual classes implementing I7zReader/I7zWriter

{ T7zParent }

constructor T7zParent.Create(lib: T7zLib);
begin
  fOwner := lib;
  inherited Create;
end;


{ T7zCodec }

function VariantToClsid(const v: T7zVariant): TGuid;
begin
  with TVarData(v) do
    if VType = VT_CLSID then // = varOleClsid
      result := PGuid(VAny)^
    else
      FillZero(result{%H-});
end;

constructor T7zCodec.Create(lib: T7zLib);
begin
  inherited Create(lib);
  fGetMethodProperty := LibraryResolve(lib.fHandle, 'GetMethodProperty');
  fGetNumberOfMethods := LibraryResolve(lib.fHandle, 'GetNumberOfMethods');
  if not (Assigned(fGetMethodProperty) and
          Assigned(fGetNumberOfMethods)) then
    raise E7zip.CreateFmt('%s is not a codec library', [lib]);
end;

function T7zCodec.GetDecoder(index: integer): ICompressCoder;
var
  v: T7zVariant;
begin
  v := MethodProperty[index, kDecoder];
  fOwner.CreateObject(VariantToClsid(v), ICompressCoder, result);
end;

function T7zCodec.GetEncoder(index: integer): ICompressCoder;
var
  v: T7zVariant;
begin
  v := MethodProperty[index, kEncoder];
  fOwner.CreateObject(VariantToClsid(v), ICompressCoder, result);
end;

function T7zCodec.GetMethodProperty(index: cardinal;
  propID: T7zMethodPropID): T7zVariant;
begin
  E7zip.Check(self, 'GetMethodProperty',
    fGetMethodProperty(index, propID, result));
end;

function T7zCodec.GetName(index: integer): string;
begin
  result := MethodProperty[index, kName];
end;

function T7zCodec.GetNumberOfMethods: cardinal;
begin
  E7zip.Check(self, 'GetNumberOfMethods', fGetNumberOfMethods(@result));
end;

function T7zCodec.SetRatioInfo(inSize, outSize: PInt64): HRESULT;
begin
  result := S_OK;
end;



{ T7zArchive }

constructor T7zArchive.Create(lib: T7zLib; fmt: T7zFormatHandler;
  libowned: boolean);
begin
  if libowned then
    fLibOwned := lib; // to be released eventually
  inherited Create(lib);
  if fmt = fhUndefined then
    raise E7zip.CreateFmt('%.Create(fhUndefined)', [ClassNameShort(self)^]);
  fClassId := lib.FormatGuid(fmt);
  fFormat := fmt;
  fGetHandlerProperty := LibraryResolve(lib.fHandle, 'GetHandlerProperty');
  if not Assigned(fGetHandlerProperty) then
    raise E7zip.CreateFmt('%s is not a Format library', [lib]);
end;

function T7zArchive.ClassId: TGuid;
begin
  result := fClassId;
end;

function T7zArchive.Format: T7zFormatHandler;
begin
  result := fFormat;
end;

function T7zArchive.HandlerProperty(propID: T7zHandlerPropID): T7zVariant;
begin
  E7zip.Check(self, 'HandlerProperty',
    fGetHandlerProperty(propID, result));
end;

function T7zArchive.GetLibGUIDProperty(index: T7zHandlerPropID): TGuid;
begin
  result := VariantToClsid(HandlerProperty(index));
end;

procedure T7zArchive.SetProgressCallback(const callback: T7zProgressCallback);
begin
  fProgressCallback := callback;
  fProgressCurrent := 0;
  fProgressTotal := 0;
end;

function T7zArchive.SetTotal(total: Int64): HRESULT;
begin
  fProgressTotal := total;
  if Assigned(fProgressCallback) then
    result := fProgressCallback(self, fProgressCurrent, total)
  else
    result := S_OK;
end;

function T7zArchive.SetCompleted(completeValue: PInt64): HRESULT;
begin
  if completeValue <> nil then
    fProgressCurrent := completeValue^;
  if Assigned(fProgressCallback) then
    result := fProgressCallback(self, fProgressCurrent, fProgressTotal)
  else
    result := S_OK;
end;

function T7zArchive.GetLibStringProperty(index: T7zHandlerPropID): string;
begin
  result := string(HandlerProperty(index));
end;


{ T7zReader }

procedure T7zReader.Close;
begin
  fPasswordIsDefined := false;
  fSubArchiveMode := false;
  fInArchive.Close;
  fInArchive := nil;
end;

function T7zReader.InArchive: IInArchive;
begin
  if fInArchive = nil then
    fOwner.CreateObject(fClassID, IInArchive, fInArchive);
  result := fInArchive;
end;

procedure T7zReader.EnsureOpened;
begin
  if fInArchive = nil then
    raise E7zip.CreateFmt('%s: missing OpenFile/OpenStream',
      [ClassNameShort(self)^]); 
end;

function T7zReader.GetItemPath(index: integer): RawUtf8;
begin
  VariantToUtf8(GetItemProp(index, kpidPath), result);
end;

function T7zReader.GetItemModDate(index: integer): TDateTime;
begin
  result := GetItemPropDateTime(index, kpidLastWriteTime);
end;

function T7zReader.GetItemDate(index: integer): TDateTime;
begin
  result := GetItemPropDateTime(index, kpidCreationTime);
end;

function T7zReader.GetItemPackSize(index: integer): Int64;
begin
  result := VariantToInt64Def(GetItemProp(index, kpidPackedSize), -1);
end;

function T7zReader.GetItemComment(index: integer): RawUtf8;
begin
  VariantToUtf8(GetItemProp(index, kpidComment), result);
end;

function T7zReader.GetItemCRC(index: integer): cardinal;
begin
  result := VariantToInt64Def(GetItemProp(index, kpidCRC), -1);
end;

function T7zReader.Count: integer;
begin
  if fInArchive = nil then
    result := 0
  else
    E7zip.CheckOk(self, 'GetNumberOfItems',
      fInArchive.GetNumberOfItems(PCardinal(@result)^));
end;

function T7zReader.NameToIndex(const name: RawUtf8): integer;
begin
  if fInArchive <> nil then
    for result := 0 to Count - 1 do
      if IdemPropNameU(name, GetItemPath(result)) then
        exit;
  result := -1;
end;

const
  MAXCHECK: Int64 = 1 shl 20;

procedure T7zReader.OpenFile(const filename: TFileName);
var
  strm: IInStream;
begin
  strm := T7zStream.CreateFromFile(filename, fmOpenReadDenyNone);
  try
    E7zip.CheckOk(self, 'OpenFile',
      InArchive.Open(strm, @MAXCHECK, self as IArchiveOpenCallBack));
  finally
    strm := nil;
  end;
end;

procedure T7zReader.OpenStream(stream: IInStream);
begin
  E7zip.CheckOk(self, 'OpenStream',
    InArchive.Open(stream, @MAXCHECK, self as IArchiveOpenCallBack));
end;

function T7zReader.GetItemIsFolder(index: integer): boolean;
begin
  result := boolean(GetItemProp(index, kpidIsFolder));
end;

function T7zReader.GetItemProp(Item: cardinal;
  prop: TPropID): T7zVariant;
begin
  EnsureOpened;
  VarClear(result);
  E7zip.CheckOK(self, 'GetItemProp',
    fInArchive.GetProperty(Item, prop, result));
end;

function T7zReader.GetItemPropDateTime(item: cardinal;
  prop: TPropID): TDateTime;
var
  v: TVarData; // VT_FILETIME/varOleFileTime is not handled by the RTL
begin
  v.VType := 0;
  E7zip.CheckOK(self, 'GetItemPropDateTime',
    fInArchive.GetProperty(Item, prop, variant(v)));
  if not (v.VType in [varEmpty, VT_FILETIME]) then
    raise E7zip.CreateFmt('T7zReader.GetItemPropDateTime=%d', [v.VType]);
  VariantToDateTime(variant(v), result);
end;

procedure T7zReader.Extract(item: cardinal; Stream: TStream);
begin
  EnsureOpened;
  fStream := Stream;
  try
    E7zip.CheckOk(self, 'Extract',
      fInArchive.Extract(
        @item, 1, ord(Stream = nil), self as IArchiveExtractCallback));
  finally
    fStream := nil;
  end;
end;

function T7zReader.Extract(const name: RawUtf8; Stream: TStream): boolean;
var
  i: integer;
begin
  result := false;
  i := NameToIndex(name);
  if i >= 0 then
    try
      Extract(i, Stream);
      result := true;
    except
      result := false;
    end;
end;

function T7zReader.Extract(const name: RawUtf8): RawByteString;
var
  s: TRawByteStringStream;
begin
  result := '';
  s := TRawByteStringStream.Create;
  try
    if Extract(name, s) then
      result := s.DataString
  finally
    s.Free;
  end;
end;

function T7zReader.GetStream(index: cardinal;
  var outStream: ISequentialOutStream; askExtractMode: T7zExtractAskMode): HRESULT;
var
  path: TFileName;
begin
  if askExtractMode = eamExtract then
    if fStream <> nil then
      outStream := T7zStream.Create(fStream, soReference) as ISequentialOutStream
    else if assigned(fExtractCallback) then
    begin
      result := fExtractCallback(self, index, outStream);
      exit;
    end
    else if fExtractPath <> '' then
      if not GetItemIsFolder(index) then
      begin
        path := Utf8ToString(GetItemPath(index));
        if fExtractPathNoSubFolder then
          path := ExtractFileName(path);
        path := fExtractPath + path;
        ForceDirectories(ExtractFilePath(path));
        outStream := T7zStream.CreateFromFile(path, fmCreate);
      end;
  result := S_OK;
end;

function T7zReader.PrepareOperation(askExtractMode: T7zExtractAskMode): HRESULT;
begin
  result := S_OK;
end;

function T7zReader.SetCompleted(files, bytes: PInt64): HRESULT;
begin
  result := S_OK;
end;

function T7zReader.SetOperationResult(
  resultEOperationResult: T7zExtractOperationResult): HRESULT;
begin
  result := S_OK;
end;

function T7zReader.SetTotal(files, bytes: PInt64): HRESULT;
begin
  result := S_OK;
end;

function T7zReader.CryptoGetTextPassword(var password: TBStr): HRESULT;
var
  wpass: SynUnicode;
begin
  if fPasswordIsDefined then
  begin
    password := SysAllocString(PWideChar(fPassword));
    result := S_OK;
  end
  else if Assigned(fPasswordCallback) then
  begin
    result := fPasswordCallback(self, wpass);
    if result = S_OK then
    begin
      password := SysAllocString(PWideChar(wpass));
      fPasswordIsDefined := true;
      fPassword := wpass;
    end;
  end
  else
    result := S_FALSE;
end;

function T7zReader.GetProperty(propID: TPropID;
  var value: T7zVariant): HRESULT;
begin
  result := S_OK;
end;

function T7zReader.GetStream(const name: PWideChar;
  var inStream: IInStream): HRESULT;
begin
  result := S_OK;
end;

procedure T7zReader.SetPasswordCallback(const callback: T7zPasswordCallback);
begin
  fPasswordCallback := callback;
end;

function T7zReader.SetSubArchiveName(name: PWideChar): HRESULT;
begin
  fSubArchiveMode := true;
  fSubArchiveName := UnicodeBufferToUtf8(name);
  result := S_OK;
end;

function T7zReader.GetItemName(index: integer): RawUtf8;
begin
  VariantToUtf8(GetItemProp(index, kpidName), result);
end;

function T7zReader.GetItemSize(index: integer): Int64;
begin
  result := VariantToInt64Def(GetItemProp(index, kpidSize), -1);
end;

procedure T7zReader.Extract(const items: array of integer;
  const callback: T7zGetStreamCallBack);
begin
  EnsureOpened;
  if length(items) = 0 then
    exit;
  fExtractCallback := callback;
  try
    E7zip.CheckOk(self, 'Extract', fInArchive.Extract(@items[0], length(items),
      ord(Assigned(callback)), self as IArchiveExtractCallback));
  finally
    fExtractCallback := nil;
  end;
end;

procedure T7zReader.ExtractAll(const callback: T7zGetStreamCallBack);
begin
  EnsureOpened;
  fExtractCallback := callback;
  try
    E7zip.CheckOk(self, 'ExtractAll', fInArchive.Extract(
      nil, $FFFFFFFF, ord(Assigned(callback)), self as IArchiveExtractCallback));
  finally
    fExtractCallback := nil;
  end;
end;

procedure T7zReader.ExtractAll(const path: TFileName; nosubfolder: boolean);
begin
  EnsureOpened;
  fExtractPath := IncludeTrailingPathDelimiter(path);
  fExtractPathNoSubFolder := nosubfolder; 
  try
    E7zip.CheckOk(self, 'ExtractAll', fInArchive.Extract(
      nil, $FFFFFFFF, 0, self as IArchiveExtractCallback));
  finally
    fExtractPath := '';
  end;
end;

procedure T7zReader.SetPassword(const password: SynUnicode);
begin
  fPassword := password;
  fPasswordIsDefined :=  fPassword <> '';
end;



{ T7zStream }

constructor T7zStream.Create(Stream: TStream; Ownership: TStreamOwnership);
begin
  inherited Create;
  fStream := Stream;
  fOwnerShip := Ownership;
end;

constructor T7zStream.CreateFromFile(const Name: TFileName; Mode: cardinal);
begin
  Create(TFileStreamEx.Create(Name, Mode), soOwned);
end;

destructor T7zStream.Destroy;
begin
  inherited;
  if fOwnerShip = soOwned then
    fStream.Free;
end;

function T7zStream.Flush: HRESULT;
begin
  result := S_OK;
end;

function T7zStream.GetSize(size: PInt64): HRESULT;
begin
  if size <> nil then
    size^ := fStream.Size;
  result := S_OK;
end;

function T7zStream.Read(data: pointer; size: cardinal;
  processedSize: PCardinal): HRESULT;
var
  len: integer;
begin
  len := fStream.Read(data^, size);
  if processedSize <> nil then
    processedSize^ := len;
  result := S_OK;
end;

function T7zStream.Seek(offset: Int64; seekOrigin: cardinal;
  newPosition: PInt64): HRESULT;
var
  pos: Int64;
begin
  pos := fStream.Seek(offset, TSeekOrigin(seekOrigin));
  if newPosition <> nil then
    newPosition^ := pos;
  result := S_OK;
end;

function T7zStream.SetSize(newSize: Int64): HRESULT;
begin
  fStream.Size := newSize;
  result := S_OK;
end;

function T7zStream.Write(data: pointer; size: cardinal;
  processedSize: PCardinal): HRESULT;
var
  len: integer;
begin
  len := fStream.Write(data^, size);
  if processedSize <> nil then
    processedSize^ := len;
  result := S_OK;
end;

destructor T7zItem.Destroy;
begin
  if Ownership = soOwned then
    Stream.Free;
  inherited;
end;


{ T7zWriter }

function GetFileTime(hFile: THandle;
  lpCreationTime, lpLastAccessTime, lpLastWriteTime: PFileTime): LongBool;
    external 'kernel32' name 'GetFileTime';
function GetFileAttributes(lpFileName: PChar): cardinal;
    external 'kernel32' name 'GetFileAttributes' + _AW;

procedure T7zWriter.AddFile(const Filename: TFileName; const Path: RawUtf8);
var
  item: T7zItem;
  Handle: THandle;
begin
  if not FileExists(Filename) then
    exit;
  item := T7zItem.Create;
  Item.SourceMode := smFile;
  item.Stream := nil;
  item.FileName := Filename;
  item.Path := Path;
  Handle := FileOpen(Filename, fmOpenReadDenyNone);
  GetFileTime(Handle, @item.CreationTime, nil, @item.LastWriteTime);
  item.Size := FileSize(Handle);
  CloseHandle(Handle);
  item.Attributes := GetFileAttributes(pointer(Filename));
  item.IsFolder := false;
  item.IsAnti := false;
  item.Ownership := soOwned;
  ObjArrayAdd(fEntries, item);
end;

procedure T7zWriter.AddFiles(const Dir, Path, Wildcard: TFileName; recurse: boolean);
var
  lencut: integer;
  files: TStringList;
  d: TFileName;

  procedure Traverse(const p: TFileName);
  var
    f: TSearchRec;
    i: integer;
    fn: TFileName;
    item: T7zItem;
  begin
    if recurse then
    begin
      if FindFirst(p + '*.*', faDirectory, f) = 0 then
      repeat
        if (f.Name[1] <> '.') then
          Traverse(IncludeTrailingPathDelimiter(p + f.Name));
      until FindNext(f) <> 0;
      SysUtils.FindClose(f);
    end;
    for i := 0 to files.Count - 1 do
    begin
      if FindFirst(p + files[i],
        faReadOnly or faHidden{%H-} or faSysFile{%H-} or faArchive, f) = 0 then
      repeat
        item := T7zItem.Create;
        Item.SourceMode := smFile;
        item.Stream := nil;
        item.FileName := p + f.Name;
        fn := copy(item.FileName, lencut, length(item.FileName) - lencut + 1);
        if path <> '' then
          fn := IncludeTrailingPathDelimiter(path) + p;
        StringToUtf8(fn, item.Path);
        item.CreationTime := f.FindData.ftCreationTime;
        item.LastWriteTime := f.FindData.ftLastWriteTime;
        item.Attributes := f.FindData.dwFileAttributes;
        item.Size := f.Size;
        item.IsFolder := false;
        item.IsAnti := false;
        item.Ownership := soOwned;
        ObjArrayAdd(fEntries, item);
      until FindNext(f) <> 0;
      SysUtils.FindClose(f);
    end;
  end;

begin
  files := TStringList.Create;
  try
    files.Delimiter := ';';
    files.DelimitedText := Wildcard;
    d := IncludeTrailingPathDelimiter(Dir);
    lencut := Length(d) + 1;
    Traverse(d);
  finally
    files.Free;
  end;
end;

procedure T7zWriter.AddStream(Stream: TStream; Ownership: TStreamOwnership;
  Attributes: cardinal; CreationTime, LastWriteTime: TFileTime;
  const Path: RawUtf8; IsFolder, IsAnti: boolean);
var
  item: T7zItem;
begin
  item := T7zItem.Create;
  Item.SourceMode := smStream;
  item.Attributes := Attributes;
  item.CreationTime := CreationTime;
  item.LastWriteTime := LastWriteTime;
  item.Path := Path;
  item.IsFolder := IsFolder;
  item.IsAnti := IsAnti;
  item.Stream := Stream;
  item.Size := Stream.Size;
  item.Ownership := Ownership;
  ObjArrayAdd(fEntries, item);
end;

procedure T7zWriter.ClearBatch;
begin
  ObjArrayClear(fEntries);
end;

function T7zWriter.CryptoGetTextPassword2(passwordIsDefined: PInteger;
  var password: TBStr): HRESULT;
begin
  if fPassword <> '' then
  begin
   passwordIsDefined^ := 1;
   password := SysAllocString(PWideChar(fPassword));
  end
  else
    passwordIsDefined^ := 0;
  result := S_OK;
end;

destructor T7zWriter.Destroy;
begin
  fOutArchive := nil;
  ClearBatch;
  inherited;
end;

function T7zWriter.OutArchive: IOutArchive;
begin
  if fOutArchive = nil then
    fOwner.CreateObject(fClassID, IOutArchive, fOutArchive);
  result := fOutArchive;
end;

function T7zWriter.GetProperty(index: cardinal; propID: TPropID;
  var value: T7zVariant): HRESULT;
var
  item: T7zItem;
begin
  VarClear(value);
  if index >= cardinal(length(fEntries)) then
  begin
    result := ERROR_INVALID_PARAMETER;
    exit;
  end;
  item := fEntries[index];
  with TVarData(Value) do
    case propID of
      kpidAttributes:
        begin
          VType := VT_UI4; // = varLongWord
          VLongWord := item.Attributes;
        end;
      kpidLastWriteTime:
        begin
          VType := VT_FILETIME; // = varOleFileTime
          VInt64 := Int64(item.LastWriteTime);
        end;
      kpidPath:
        if item.Path <> '' then
          value := Utf8ToWideString(item.Path);
      kpidIsFolder:
        Value := item.IsFolder;
      kpidSize:
        begin
          VType := VT_UI8; // = varWord64
          VInt64 := item.Size;
        end;
      kpidCreationTime:
        begin
          VType := VT_FILETIME;
          VInt64 := Int64(item.CreationTime);
        end;
      kpidIsAnti:
        value := item.IsAnti;
    end;
  result := S_OK;
end;

function T7zWriter.GetStream(index: cardinal;
  var inStream: ISequentialInStream): HRESULT;
var
  item: T7zItem;
begin
  if index >= cardinal(length(fEntries)) then
  begin
    result := ERROR_INVALID_PARAMETER;
    exit;
  end;
  item := fEntries[index];
  case item.SourceMode of
    smFile:
      inStream := T7zStream.CreateFromFile(
        item.FileName, fmOpenReadDenyNone);
    smStream:
      begin
        item.Stream.Seek(0, soFromBeginning);
        inStream := T7zStream.Create(item.Stream, soReference);
      end;
  end;
  result := S_OK;
end;

function T7zWriter.GetUpdateItemInfo(index: cardinal; newData,
  newProperties: PInteger; indexInArchive: PCardinal): HRESULT;
begin
  if index >= cardinal(length(fEntries)) then
  begin
    result := ERROR_INVALID_PARAMETER;
    exit;
  end;
  newData^ := 1;
  newProperties^ := 1;
  indexInArchive^ := cardinal(-1);
  result := S_OK;
end;

procedure T7zWriter.SaveToFile(const FileName: TFileName);
var
  f: TFileStreamEx;
begin
  f := TFileStreamEx.Create(FileName, fmCreate);
  try
    SaveToStream(f);
  finally
    f.free;
  end;
end;

procedure T7zWriter.SaveToStream(stream: TStream);
var
  strm: ISequentialOutStream;
begin
  strm := T7zStream.Create(stream, soReference);
  try
    E7zip.CheckOk(self, 'SaveToStream',
      OutArchive.UpdateItems(strm, length(fEntries), self as IArchiveUpdateCallback));
  finally
    strm := nil;
  end;
end;

function T7zWriter.SetOperationResult(operationResult: integer): HRESULT;
begin
  result := S_OK;
end;

procedure T7zWriter.SetPassword(const password: SynUnicode);
begin
  fPassword := password;
end;

procedure T7zWriter.SetProperty(const name: RawUtf8;
  const value: T7zVariant);
var
  intf: ISetProperties;
  p: PWideChar;
begin
  intf := OutArchive as ISetProperties;
  p := pointer(Utf8DecodeToUnicodeRawByteString(name));
  E7zip.CheckOk(self, 'SetProperty', intf.SetProperties(@p, @value, 1));
end;

procedure T7zWriter.SetCardinalProperty(const name: RawUtf8; card: cardinal);
var
  v: TVarData;
begin
  v.VType := VT_UI4;
  v.VLongWord := card;
  SetProperty(name, T7zVariant(v));
end;

procedure T7zWriter.SetTextProperty(const name, text: RawUtf8);
var
  v: T7zVariant;
begin
  v := Utf8ToWideString(text);
  SetProperty(name, v);
end;

const
  ZipCompressionMethod: array[TZipCompressionMethod] of WideString = (
    'COPY', 'DEFLATE', 'DEFLATE64', 'BZIP2', 'LZMA', 'PPMD');
  ZipEncryptionMethod: array[TZipEncryptionMethod] of WideString = (
    'AES128', 'AES192', 'AES256', 'ZIPCRYPTO');
  SevCompressionMethod: array[T7zCompressionMethod] of WideString = (
    'COPY', 'LZMA', 'BZIP2', 'PPMD', 'DEFLATE', 'DEFLATE64');
  BooleanMethod: array[boolean] of WideString = (
    'OFF', 'ON');

procedure T7zWriter.SetCompressionLevel(level: cardinal);
begin
  SetCardinalProperty('X', level);
end;

procedure T7zWriter.SetMultiThreading(threadCount: cardinal);
begin
  SetCardinalProperty('MT', threadCount);
end;

procedure T7zWriter.SetCompressionMethod(method: TZipCompressionMethod);
begin
  SetProperty('M', ZipCompressionMethod[method]);
end;

procedure T7zWriter.SetEncryptionMethod(method: TZipEncryptionMethod);
begin
  SetProperty('EM', ZipEncryptionMethod[method]);
end;

procedure T7zWriter.SetDictionnarySize(size: cardinal);
begin
  SetCardinalProperty('D', size);
end;

procedure T7zWriter.SetMemorySize(size: cardinal);
begin
  SetCardinalProperty('MEM', size);
end;

procedure T7zWriter.SetDeflateNumPasses(pass: cardinal);
begin
  SetCardinalProperty('PASS', pass);
end;

procedure T7zWriter.SetNumFastBytes(fb: cardinal);
begin
  SetCardinalProperty('FB', fb);
end;

procedure T7zWriter.SetNumMatchFinderCycles(mc: cardinal);
begin
  SetCardinalProperty('MC', mc);
end;

procedure T7zWriter.SetCompressionMethod7z(method: T7zCompressionMethod);
begin
  SetProperty('0', SevCompressionMethod[method]);
end;

procedure T7zWriter.SetBindInfo7z(const bind: WideString);
begin
  SetProperty('B', bind);
end;

procedure T7zWriter.SetSolidSettings7z(solid: boolean);
begin
  SetProperty('S', BooleanMethod[solid]);
end;

procedure T7zWriter.RemoveSfxBlock7z(remove: boolean);
begin
  SetProperty('RSFX', BooleanMethod[remove]);
end;

procedure T7zWriter.AutoFilter7z(auto: boolean);
begin
  SetProperty('A', BooleanMethod[auto]);
end;

procedure T7zWriter.CompressHeaders7z(compress: boolean);
begin
  SetProperty('HC', BooleanMethod[compress]);
end;

procedure T7zWriter.CompressHeadersFull7z(compress: boolean);
begin
  SetProperty('HCF', BooleanMethod[compress]);
end;

procedure T7zWriter.EncryptHeaders7z(encrypt: boolean);
begin
  SetProperty('HE', BooleanMethod[encrypt]);
end;

procedure T7zWriter.VolumeMode7z(mode: boolean);
begin
  SetProperty('V', BooleanMethod[mode]);
end;



initialization
  assert(HANDLER_GUID.D4[5] = $ff);

{$endif OSPOSIX}

end.

