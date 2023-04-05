/// access to the 7-Zip library on Windows
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.lib.win7zip;

{
  *****************************************************************************

   Access to the 7-Zip Compression/Decompression DLL on Windows 
   - Low-Level 7-Zip API Definitions
   - I7zReader/I7zWriter High-Level Wrappers

  *****************************************************************************

  Two meaningful questions:

  Can I use the EXE or DLL files from 7-Zip in a commercial application?
  Yes, but you are required to specify in documentation for your application:
    (1) that you used parts of the 7-Zip program,
    (2) that 7-Zip is licensed under the GNU LGPL license and
    (3) you must give a link to www.7-zip.org, where the source code can be found.

  Can I become insane because of directly calling the 7-Zip public dll API?
  Yes, Igor's API is just bloated: the LCL unit has 10,000 lines just for basic
  archive work. So don't become insane and use our unit. Better safe than sorry.

}

interface

{$I ..\mormot.defines.inc}

{$ifdef OSPOSIX}

// do-nothing-unit on non Windows system

implementation

{$else}

uses
  Windows,
  ActiveX,
  sysutils,
  classes,
  types,
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
      opResult: T7zExtractOperationResult): HRESULT; stdcall;
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


{ ****************** I7zReader/I7zWriter High-Level Wrappers }

type
  /// kind of exceptions raised by this unit
  E7Zip = class(ESynException)
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

  I7zArchive = interface;

  // note: the sender of following events is a I7zReader or I7zWriter

  /// event as used by I7zReader.SetPasswordCallback
  // - should return true to continue the execution, or false to abort
  T7zPasswordCallback = function(const sender: I7zArchive;
    var password: RawUtf8): boolean of object;

  /// event as used by I7zReader.Extract/ExtractAll methods
  // - should return S_OK to continue the execution, or something else to abort
  T7zGetStreamCallBack = function(const sender: I7zArchive; index: cardinal;
    var outStream: ISequentialOutStream): HRESULT of object;

  /// event as used by I7zReader/I7zWriter.SetProgressCallback method
  // - should return S_OK to continue the execution, or something else to abort,
  // e.g. ERROR_OPERATION_ABORTED
  T7zProgressCallback = function(const sender: I7zArchive;
    current, total: Int64): HRESULT of object;

  /// the parent interface of both I7zRead and I7zWriter
  I7zArchive = interface
    ['{9C0C0C8C-883A-49ED-B608-A6D66D36D530}']
    // -- some internal methods used as property getters
    function GetSize(index: integer): Int64;
    function GetPackSize(index: integer): Int64;
    function GetCrc(index: integer): cardinal;
    function GetComment(index: integer): RawUtf8;
    function GetModDate(index: integer): TDateTime;
    function GetCreateDate(index: integer): TDateTime;
    function GetZipName(index: integer): RawUtf8;
    function GetName(index: integer): RawUtf8;
    function GetMethod(index: integer): RawUtf8;
    function GetIsFolder(index: integer): boolean;
    function GetAttributes(index: integer): cardinal;
    function GetInstance: TObject;
    /// the file name specified to OpenFile() or SaveToFile()
    function FileName: TFileName;
    /// the high-level identifier of this archive format
    function Format: T7zFormatHandler;
    /// the usual main extension of this archive format (e.g. 'zip' for fhZip)
    function FormatExt: string;
    /// all known extensions of this archive format (as '*.ext1;*.ext2')
    function FormatExts: string;
    /// the 7-zip internal CSLID of this archive format
    function ClassId: TGuid;
    /// let an event be called during file process
    procedure SetProgressCallback(const callback: T7zProgressCallback);
    /// let an event be called to let the user supply a password
    procedure SetPasswordCallback(const callback: T7zPasswordCallback);
    /// supply a password
    procedure SetPassword(const password: RawUtf8);
    /// the number of files in this archive
    // - used as index in Item*[0..Count-1] properties and
    // I7zReader.Extract or I7zWriter.SaveToFile methods
    function Count: integer;
    /// search for a given file name in this archive
    // - case-insensitive search within ItemZipName[] items
    // - returns -1 if not found, or the index in Item*[] properties
    function NameToIndex(const zipname: RawUtf8): integer;
    /// the kpidPath property value of an archived file
    // - i.e. its usual file name, potentially with a relative path
    // - e.g. 'toto.txt' or 'rep\toto.bmp'
    property ZipName[index: integer]: RawUtf8
      read GetZipName;
    /// the kpidName property value of an archived file
    // - is likely to be '' for most packers
    property FullName[index: integer]: RawUtf8
      read GetName;
    /// the kpidSize property value of an archived file
    property Size[index: integer]: Int64
      read GetSize;
    /// the kpidPackedSize property value
    property PackSize[index: integer]: Int64
      read GetPackSize;
    /// the kpidIsFolder property value of an archived file
    property IsFolder[index: integer]: boolean
      read GetIsFolder;
    /// the kpidMethod property value of an archived file
    property Method[index: integer]: RawUtf8
      read GetMethod;
    /// the kpidComment property value of an archived file
    property Comment[index: integer]: RawUtf8
      read GetComment;
    /// the kpidCRC property value of an archived file
    property Crc[index: integer]: cardinal
      read GetCrc;
    /// the kpidLastWriteTime property value - i.e. the "usual" date
    property ModDate[index: integer]: TDateTime
      read GetModDate;
    /// the kpidCreationTime property value
    // - may be 0 for some packers (e.g. zip)
    property CreateDate[index: integer]: TDateTime
      read GetCreateDate;
    /// the kpidAttributes property value
    // - may be 0 for some packers (e.g. zip)
    property Attributes[index: integer]: cardinal
      read GetAttributes;
  end;

  /// reading acccess to an archive content using the 7z.dll
  I7zReader = interface(I7zArchive)
    ['{9C0C0C8C-883A-49ED-B608-A6D66D36D53A}']
    // -- main high-level methods
    /// open a given archive file
    procedure OpenFile(const name: TFileName);
    /// open an archive content from a stream
    procedure OpenStream(stream: IInStream);
    /// close the archive
    procedure Close;
    /// retrieve the raw value of one property
    function GetProp(item: cardinal; prop: TPropID): T7zVariant;
    /// uncompress a file by index from this archive into a stream
    // - if no Stream is specified, will test for the output
    procedure Extract(item: cardinal; Stream: TStream); overload;
    /// uncompress a file by index from this archive into a local folder
    procedure Extract(item: cardinal; const path: TFileName;
      nosubfolder: boolean); overload;
    /// uncompress a file by name from this archive into a stream
    // - if no Stream (i.e. nil) is specified, will test for the output
    function Extract(const zipname: RawUtf8; Stream: TStream): boolean; overload;
    /// uncompress a file by name from this archive into a RawByteString
    function Extract(const zipname: RawUtf8): RawByteString; overload;
    /// uncompress a file by name from this archive into a local folder
    function Extract(const zipname: RawUtf8; const path: TFileName;
      nosubfolder: boolean = false): boolean; overload;
    /// uncompress some files by name from this archive into a local folder
    // - returns nil on success, or the list of invalid zipnames
    function Extract(const zipnames: array of RawUtf8; const path: TFileName;
      nosubfolder: boolean = false): TRawUtf8DynArray; overload;
    /// uncompress several files from this archive using a callback per file
    // - if no Callback is specified (as default), will test for the output
    procedure Extract(const items: array of integer;
      const callback: T7zGetStreamCallBack = nil); overload;
    /// uncompress all files from this archive using a callback per file
    // - if no Callback is specified (as default), will test for the output
    procedure ExtractAll(const callback: T7zGetStreamCallBack = nil); overload;
    /// uncompress all files from this archive into a given local folder
    procedure ExtractAll(const path: TFileName; nosubfolder: boolean = false); overload;
    /// access to the 7-zip internal API reading raw object
    function InArchive: IInArchive;
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

  /// writing acccess to an archive content using the 7z.dll
  // - main pattern is to call AddFile/AddFiles/AddBuffer then SaveToFile() to
  // generate a new archive
  I7zWriter = interface(I7zArchive)
    ['{9C0C0C8C-883A-49ED-B608-A6D66D36D53B}']
    // -- main high-level methods
    /// add (or replace if ZipName already exists) a file within the archive
    function AddFile(const Filename: TFileName; const ZipName: RawUtf8): boolean;
    /// add (or replace) some files from a folder within the archive
    procedure AddFiles(const Dir, Path, Wildcard: TFileName; recurse: boolean);
    /// add (or replace) a file from a memory buffer within the archive
    procedure AddBuffer(const ZipName: RawUtf8; const Data: RawByteString);
    /// add (or replace) a file from a stream within the archive
    procedure AddStream(Stream: TStream; Ownership: TStreamOwnership;
      Attributes: cardinal; CreationTime, LastWriteTime: TUnixTime;
      const Path: RawUtf8; IsFolder, IsAnti: boolean);
    /// remove a file within the archive, from its index
    function Delete(index: integer): boolean; overload;
    /// remove a file within the archive, from its zipname
    function Delete(const ZipName: RawUtf8): boolean; overload;
    /// create a new archive file from the previously set AddFile/Delete
    procedure SaveToFile(const DestName: TFileName);
    /// create a new archive stream from the previously set AddFile/Delete
    procedure SaveToStream(stream: TStream);
    /// remove all previously set AddFile/Delete
    procedure Clear;
    /// modify a property of a file within the archive
    procedure SetProperty(const zipname: RawUtf8; const value: T7zVariant);
    /// modify the compression level to be used during process
    procedure SetCompressionLevel(level: cardinal);
    /// modify the threading level to be used during process
    procedure SetMultiThreading(threadCount: cardinal);
    /// modify the compression method to be used during process
    procedure SetCompressionMethod(method: TZipCompressionMethod);
    /// modify the encryption method to be used during process
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
    /// access to the 7-zip internal API writing raw object
    function OutArchive: IOutArchive;
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
    /// return the known file extensions of a given archive format as '*.ext1;*.ext2'
    class function FormatFileExtensions(fmt: T7zFormatHandler): string;
    /// return the main file extension of a given archive format as 'ext'
    class function FormatFileExtension(fmt: T7zFormatHandler): string;
    /// load the 7z.dll
    constructor Create(lib: TFileName = ''); reintroduce;
    /// unload the 7z.dll
    destructor Destroy; override;
    /// I7zLib methods
    function FileName: TFileName;
    function NewReader(fmt: T7zFormatHandler): I7zReader; overload;
    function NewReader(const name: TFileName): I7zReader; overload;
    function NewWriter(fmt: T7zFormatHandler): I7zWriter; overload;
    function NewWriter(const name: TFileName): I7zWriter; overload;
  end;


/// global factory of the main I7zReader high-level archive file decompressor
// - will guess the file format from its existing content
// - will own its own TZlib instance to access the 7z.dll library
function New7zReader(const name: TFileName; const lib: TFileName = ''): I7zReader;

/// global factory of the main I7zWriter high-level archive compressor
// - will own its own TZlib instance to access the 7z.dll library
function New7zWriter(fmt: T7zFormatHandler; const lib: TFileName = ''): I7zWriter; overload;

/// global factory of the main I7zWriter to update an existing archive file
// - will guess the file format from its existing content
// - will own its own TZlib instance to access the 7z.dll library
function New7zWriter(const name: TFileName; const lib: TFileName = ''): I7zWriter; overload;


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

{ E7Zip }

class procedure E7Zip.RaiseAfterCheck(Caller: TObject;
  const Context: shortstring; Res: HResult);
begin
  raise CreateFmt('%s.%s error %x (%s)',
    [ClassNameShort(Caller)^, Context, Res, string(WinErrorText(Res, nil))])
end;

class procedure E7Zip.Check(Caller: TObject; const Context: shortstring;
  Res: HResult);
begin
  if Res and $80000000 <> 0 then
    RaiseAfterCheck(Caller, Context, Res);
end;

class procedure E7Zip.CheckOk(Caller: TObject; const Context: shortstring;
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

  T7zArchive = class;

  T7zStream = class(TInterfacedObject,
    ISequentialInStream, IInStream, IStreamGetSize,
    ISequentialOutStream, IOutStream, IOutStreamFlush)
  private
    fStream: TStream;
    fIndex: integer;
    fOwnedStream: boolean;
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
    constructor Create(Stream: TStream; OwnedStream: boolean;
      ArchiveIndex: integer = -1);
    constructor CreateFromFile(const Name: TFileName; Mode: cardinal;
      ArchiveIndex: integer = -1);
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

  T7zArchive = class(T7zParent,
    I7zArchive, IProgress, ICryptoGetTextPassword, ICryptoGetTextPassword2)
  protected
    fFileName: TFileName;
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
    fPasswordCallback: T7zPasswordCallback;
    fPasswordIsDefined: boolean;
    fPasswordUtf16: SynUnicode;
    function GetLibStringProperty(index: T7zHandlerPropID): string;
    function GetLibGUIDProperty(index: T7zHandlerPropID): TGuid;
    function GetInstance: TObject;
    // I7zArchive methods
    function FileName: TFileName;
    function Format: T7zFormatHandler;
    function FormatExt: string;
    function FormatExts: string;
    function ClassId: TGuid;
    procedure SetProgressCallback(const callback: T7zProgressCallback);
    procedure SetPasswordCallback(const callback: T7zPasswordCallback);
    procedure SetPassword(const password: RawUtf8);
    function Count: integer; virtual; abstract;
    function NameToIndex(const zipname: RawUtf8): integer; virtual; abstract;
    function GetZipName(index: integer): RawUtf8; virtual; abstract;
    function GetName(index: integer): RawUtf8; virtual; abstract;
    function GetMethod(index: integer): RawUtf8; virtual; abstract;
    function GetSize(index: integer): Int64; virtual; abstract;
    function GetIsFolder(index: integer): boolean; virtual; abstract;
    function GetPackSize(index: integer): Int64; virtual; abstract;
    function GetCrc(index: integer): cardinal; virtual; abstract;
    function GetComment(index: integer): RawUtf8; virtual; abstract;
    function GetModDate(index: integer): TDateTime; virtual; abstract;
    function GetCreateDate(index: integer): TDateTime; virtual; abstract;
    function GetAttributes(index: integer): cardinal; virtual; abstract;
    // IProgress methods
    function SetTotal(total: Int64): HRESULT; overload; stdcall;
    function SetCompleted(completeValue: PInt64): HRESULT; overload; stdcall;
    // ICryptoGetTextPassword
    function CryptoGetTextPassword(var password: TBStr): HRESULT; stdcall;
    // ICryptoGetTextPassword2 methods
    function CryptoGetTextPassword2(passwordIsDefined: PInteger;
      var password: TBStr): HRESULT;  stdcall;
  public
    constructor Create(lib: T7zLib; fmt: T7zFormatHandler;
      libowned: boolean); reintroduce; overload;
    function HandlerProperty(propID: T7zHandlerPropID): T7zVariant;
    property Name: string
      index hpiName read GetLibStringProperty;
    property Extension: string
      index hpiExtension read GetLibStringProperty;
  end;

  T7zReader = class(T7zArchive,
    I7zReader, IArchiveOpenCallback,
    IArchiveExtractCallback, IArchiveOpenVolumeCallback,
    IArchiveOpenSetSubArchiveName)
  private
    fInArchive: IInArchive;
    fStream: TStream;
    fSubArchiveMode: boolean;
    fExtractPathNoSubFolder: boolean;
    fSubArchiveName: RawUtf8;
    fExtractCallback: T7zGetStreamCallBack;
    fExtractPath: TFileName;
    fExtractCurrent: record
      FileName: TFileName;
      Created, Accessed, Written: Int64;
    end;
    function GetProp(item: cardinal; prop: TPropID): T7zVariant;
    procedure GetPropUtf8(item: cardinal; prop: TPropID; out dest: RawUtf8);
    function GetPropDateTime(item: cardinal; prop: TPropID): TDateTime;
    function GetPropFileTime(item: cardinal; prop: TPropID): Int64;
    procedure EnsureOpened;
  protected
    // I7zArchive methods
    function Count: integer; override;
    function NameToIndex(const zipname: RawUtf8): integer; override;
    function GetZipName(index: integer): RawUtf8; override;
    function GetName(index: integer): RawUtf8; override;
    function GetMethod(index: integer): RawUtf8; override;
    function GetSize(index: integer): Int64; override;
    function GetIsFolder(index: integer): boolean; override;
    function GetPackSize(index: integer): Int64; override;
    function GetCrc(index: integer): cardinal; override;
    function GetComment(index: integer): RawUtf8; override;
    function GetModDate(index: integer): TDateTime; override;
    function GetCreateDate(index: integer): TDateTime; override;
    function GetAttributes(index: integer): cardinal; override;
    // I7zReader methods
    procedure OpenFile(const name: TFileName);
    procedure OpenStream(stream: IInStream);
    procedure Close;
    function InArchive: IInArchive;
    procedure Extract(item: cardinal; Stream: TStream); overload;
    procedure Extract(item: cardinal; const path: TFileName;
      nosubfolder: boolean); overload;
    function Extract(const zipname: RawUtf8; Stream: TStream): boolean; overload;
    function Extract(const zipname: RawUtf8): RawByteString; overload;
    function Extract(const zipname: RawUtf8; const path: TFileName;
      nosubfolder: boolean): boolean; overload;
    function Extract(const zipnames: array of RawUtf8; const path: TFileName;
      nosubfolder: boolean): TRawUtf8DynArray; overload;
    procedure Extract(const items: array of integer;
      const callback: T7zGetStreamCallBack); overload;
    procedure ExtractAll(const callback: T7zGetStreamCallBack); overload;
    procedure ExtractAll(const path: TFileName; nosubfolder: boolean); overload;
    // IArchiveOpenCallback
    function SetTotal(files, bytes: PInt64): HRESULT; overload; stdcall;
    function SetCompleted(files, bytes: PInt64): HRESULT; overload; stdcall;
    // IArchiveExtractCallback
    function GetStream(index: cardinal; var outStream: ISequentialOutStream;
      askExtractMode: T7zExtractAskMode): HRESULT; overload; stdcall;
    function PrepareOperation(askExtractMode: T7zExtractAskMode): HRESULT; stdcall;
    function SetOperationResult(
      opResult: T7zExtractOperationResult): HRESULT; overload; stdcall;
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
    smFile,
    smUpdate);

  T7zItem = class
  public
    ZipName: RawUtf8;
    Stream: TStream;
    Attributes: cardinal;
    SourceMode: T7zItemSourceMode;
    Ownership: TStreamOwnership;
    IsFolder, IsAnti: boolean;
    UpdateItemIndex: integer;
    CreationTime, LastWriteTime: TFileTime;
    FileName: TFileName;
    Size: Int64;
    destructor Destroy; override;
  end;

  T7zWriter = class(T7zArchive,
    I7zWriter, IArchiveUpdateCallback, IArchiveUpdateCallback2)
  private
    fOutArchive: IOutArchive;
    fEntries: array of T7zItem;
    fCurrentItem: T7zItem;
    fUpdateReader: I7zReader;
  protected
    procedure SetCardinalProperty(const zipname: RawUtf8; card: cardinal);
    procedure SetTextProperty(const zipname, text: RawUtf8);
    procedure AddOrReplace(item: T7zItem);
    function Get(index: PtrUInt): T7zItem;
    // I7zArchive methods
    function Count: integer; override;
    function NameToIndex(const zipname: RawUtf8): integer; override;
    function GetZipName(index: integer): RawUtf8; override;
    function GetName(index: integer): RawUtf8; override;
    function GetMethod(index: integer): RawUtf8; override;
    function GetSize(index: integer): Int64; override;
    function GetIsFolder(index: integer): boolean; override;
    function GetPackSize(index: integer): Int64; override;
    function GetCrc(index: integer): cardinal; override;
    function GetComment(index: integer): RawUtf8; override;
    function GetModDate(index: integer): TDateTime; override;
    function GetCreateDate(index: integer): TDateTime; override;
    function GetAttributes(index: integer): cardinal; override;
    // I7zWriter methods
    function AddFile(const FileName: TFileName; const ZipName: RawUtf8): boolean;
    procedure AddFiles(const Dir, Path, Wildcard: TFileName; recurse: boolean);
    procedure AddBuffer(const ZipName: RawUtf8; const Data: RawByteString);
    procedure AddStream(Stream: TStream; Ownership: TStreamOwnership;
      Attributes: cardinal; CreationTime, LastWriteTime: TUnixTime;
      const ZipName: RawUtf8; IsFolder, IsAnti: boolean);
    function Delete(index: integer): boolean; overload;
    function Delete(const ZipName: RawUtf8): boolean; overload;
    procedure SaveToFile(const DestName: TFileName);
    procedure SaveToStream(stream: TStream);
    procedure Clear;
    procedure SetProperty(const zipname: RawUtf8; const value: T7zVariant);
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
    // IArchiveUpdateCallback2 methods
    function GetVolumeSize(index: cardinal; size: PInt64): HRESULT; stdcall;
    function GetVolumeStream(index: cardinal;
      var volumeStream: ISequentialOutStream): HRESULT; stdcall;
  public
    constructor Create(lib: T7zLib; const rd: I7zReader;
      libowned: boolean); reintroduce; overload;
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
    'ZIP', 'JAR', 'DOCX', 'PPTX', 'XLSX', 'XPI', 'ODT', 'ODS',
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
  // internal FormatFileExtension(s) cache
  FILE_EXT_NAME, FILE_EXT_NAMES: array[T7zFormatHandler] of string;

class function T7zLib.FormatFileExtensions(fmt: T7zFormatHandler): string;
var
  i: PtrInt;
begin
  result := FILE_EXT_NAMES[fmt];
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
      result := FormatString('%*.%;', [result, FILE_EXT[i]]);
  if result = '' then
    result := ';'
  else
    result := SysUtils.LowerCase(copy(result, 1, length(result) - 1));
  FILE_EXT_NAMES[fmt] := result;
end;

class function T7zLib.FormatFileExtension(fmt: T7zFormatHandler): string;
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
  result := ';';
  for i := 0 to high(FILE_EXT_TYPE) do
    if FILE_EXT_TYPE[i] = fmt then
    begin
      result := SysUtils.LowerCase(string(FILE_EXT[i]));
      break;
    end;
  FILE_EXT_NAME[fmt] := result;
end;

function T7zLib.TryLoad(const libname: TFileName): boolean;
begin
  result := false;
  if libname = '' then
    exit;
  fHandle := LibraryOpen(libname);
  if not ValidHandle(fHandle) then
    exit;
  fCreateObject := LibraryResolve(fHandle, 'CreateObject');
  if Assigned(fCreateObject) then
  begin
    fFileName := libname;
    result := true;
  end
  else
    LibraryClose(fHandle);
end;

var
  LastFoundDll: TFileName; // do the folders ressearch once if possible
  
constructor T7zLib.Create(lib: TFileName);
begin
  if lib <> '' then
    TryLoad(lib)
  else
    // search in exe and 7-Zip folder, trying any possible .dll file name
    if TryLoad(LastFoundDll) or
       TryLoad(Executable.ProgramFilePath + '7z.dll') or
       TryLoad(Executable.ProgramFilePath + '7za.dll') or
       TryLoad(Executable.ProgramFilePath + '7zxa.dll') or
       TryLoad('c:\Program Files\7-Zip\7z.dll') or
       {$ifdef CPU32}
       TryLoad('c:\Program Files (x86)\7-Zip\7z.dll') or
       {$endif CPU32}
       TryLoad('7z.dll') then
      LastFoundDll := fFileName
    else
      lib := '7z.dll';
  if Assigned(fCreateObject) then
    exit;
  LastFoundDll := '';
  raise E7Zip.CreateUtf8('% is not a Win' +
    {$ifdef CPU32} '32' {$else} '64'  {$endif CPU32} + ' 7-Zip library', [lib]);
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
  E7Zip.Check(self, 'CreateObject', fCreateObject(clsid, iid, obj));
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

function T7zLib.NewWriter(const name: TFileName): I7zWriter;
begin
  result := T7zWriter.Create(self, NewReader(name), {libowned=}false);
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

function New7zWriter(const name: TFileName; const lib: TFileName): I7zWriter;
begin
  result := T7zWriter.Create(
    T7zLib.Create(lib), New7zReader(name, lib), {libowned=}true);
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
    raise E7Zip.CreateUtf8('% is not a codec library', [lib]);
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
  E7Zip.Check(self, 'GetMethodProperty',
    fGetMethodProperty(index, propID, result));
end;

function T7zCodec.GetName(index: integer): string;
begin
  result := MethodProperty[index, kName];
end;

function T7zCodec.GetNumberOfMethods: cardinal;
begin
  E7Zip.Check(self, 'GetNumberOfMethods', fGetNumberOfMethods(@result));
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
    raise E7Zip.CreateUtF8('%.Create(fhUndefined)', [self]);
  fClassId := lib.FormatGuid(fmt);
  fFormat := fmt;
  fGetHandlerProperty := LibraryResolve(lib.fHandle, 'GetHandlerProperty');
  if not Assigned(fGetHandlerProperty) then
    raise E7Zip.CreateUtf8('% is not an archive library', [lib]);
end;

function T7zArchive.ClassId: TGuid;
begin
  result := fClassId;
end;

function T7zArchive.Format: T7zFormatHandler;
begin
  result := fFormat;
end;

function T7zArchive.FormatExt: string;
begin
  result := T7zLib.FormatFileExtension(fFormat);
end;

function T7zArchive.FormatExts: string;
begin
  result := T7zLib.FormatFileExtensions(fFormat);
end;

function T7zArchive.HandlerProperty(propID: T7zHandlerPropID): T7zVariant;
begin
  E7Zip.Check(self, 'HandlerProperty',
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

procedure T7zArchive.SetPasswordCallback(const callback: T7zPasswordCallback);
begin
  fPasswordCallback := callback;
  fPasswordIsDefined := false;
end;

procedure T7zArchive.SetPassword(const password: RawUtf8);
begin
  Utf8ToSynUnicode(password, fPasswordUtf16);
  fPasswordIsDefined := true;
end;

function T7zArchive.CryptoGetTextPassword(var password: TBStr): HRESULT;
var
  fromuser: RawUtf8;
begin
  if not fPasswordIsDefined then
    if Assigned(fPasswordCallback) then
      if fPasswordCallback(self, fromuser) then
        SetPassword(fromuser);
  if fPasswordIsDefined then
  begin
    password := SysAllocString(pointer(fPasswordUtf16));
    result := S_OK;
  end
  else
    result := S_FALSE;
end;

function T7zArchive.CryptoGetTextPassword2(passwordIsDefined: PInteger;
  var password: TBStr): HRESULT;
begin
  result := CryptoGetTextPassword(password);
  passwordIsDefined^ := ord(result = S_OK);
  result := S_OK;
end;

function T7zArchive.SetTotal(total: Int64): HRESULT;
begin
  fProgressTotal := total;
  result := S_OK;
  // SetCompleted(0) will be called just after this
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

function T7zArchive.FileName: TFileName;
begin
  result := fFileName;
end;

function T7zArchive.GetInstance: TObject;
begin
  result := self;
end;



{ T7zReader }

procedure T7zReader.Close;
begin
  SetPasswordCallback(nil);
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
    raise E7Zip.CreateUtf8('% missing OpenFile/OpenStream', [self]);
end;

const
  KPID_VTYPE: array[kpidPath .. kpidVa] of word = (
    VT_BSTR,        // kpidPath
    VT_BSTR,        // kpidName
    VT_BSTR,        // kpidExtension
    VT_BOOL,        // kpidIsFolder
    VT_UI8,         // kpidSize
    VT_UI8,         // kpidPackedSize
    VT_UI4,         // kpidAttributes
    varEmpty,       // kpidCreationTime - VT_FILETIME is not RTL compatible
    varEmpty,       // kpidLastAccessTime
    varEmpty,       // kpidLastWriteTime
    VT_BOOL,        // kpidSolid
    VT_BOOL,        // kpidCommented
    VT_BOOL,        // kpidEncrypted
    VT_BOOL,        // kpidSplitBefore
    VT_BOOL,        // kpidSplitAfter
    VT_UI4,         // kpidDictionarySize
    VT_UI4,         // kpidCRC
    VT_BSTR,        // kpidType
    VT_BOOL,        // kpidIsAnti
    VT_BSTR,        // kpidMethod
    VT_BSTR,        // kpidHostOS
    VT_BSTR,        // kpidFileSystem
    VT_BSTR,        // kpidUser
    VT_BSTR,        // kpidGroup
    VT_UI4,         // kpidBlock
    VT_BSTR,        // kpidComment
    VT_UI4,         // kpidPosition
    VT_BSTR,        // kpidPrefix
    VT_UI4,         // kpidNumSubDirs
    VT_UI4,         // kpidNumSubFiles
    VT_UI1,         // kpidUnpackVer
    VT_UI4,         // kpidVolume
    VT_BOOL,        // kpidIsVolume
    VT_UI8,         // kpidOffset
    VT_UI4,         // kpidLinks
    VT_UI4,         // kpidNumBlocks
    VT_UI4,         // kpidNumVolumes
    VT_UI4,         // kpidTimeType
    VT_BOOL,        // kpidBit64
    VT_BOOL,        // kpidBigEndian
    VT_BSTR,        // kpidCpu
    VT_UI8,         // kpidPhySize
    VT_UI8,         // kpidHeadersSize
    VT_UI4,         // kpidChecksum
    VT_BSTR,        // kpidCharacts
    VT_UI8);        // kpidVa

function T7zReader.GetProp(item: cardinal; prop: TPropID): T7zVariant;
begin
  EnsureOpened;
  VarClear(result);
  E7Zip.CheckOK(self, 'GetProp',
    fInArchive.GetProperty(Item, prop, result));
  with TVarData(result) do
    if VType = VT_FILETIME then
      raise E7Zip.Create('GetProperty: VT_FILETIME is unsupported - ' +
        'use GetPropDateTime instead')
    else if (VType <> varEmpty) and
            (prop >= low(KPID_VTYPE)) and
            (prop <= high(KPID_VTYPE)) and
            (VType <> KPID_VTYPE[prop]) then
      raise E7Zip.CreateUtf8('GetProperty(%): expected %, returned %',
        [prop, KPID_VTYPE[prop], VType])
end;

procedure T7zReader.GetPropUtf8(item: cardinal; prop: TPropID; out dest: RawUtf8);
var
  v: T7zVariant;
begin
  v := GetProp(item, prop);
  if not VarIsEmptyOrNull(v) then
    VariantToUtf8(v, dest);
end;

function T7zReader.GetPropDateTime(item: cardinal;
  prop: TPropID): TDateTime;
var
  v: TVarData; // VT_FILETIME/varOleFileTime is not handled by the RTL
begin
  v.VType := 0;
  E7Zip.CheckOK(self, 'GetPropDateTime',
    fInArchive.GetProperty(Item, prop, variant(v)));
  if not (v.VType in [varEmpty, VT_FILETIME]) then
    raise E7Zip.CreateUtf8('T7zReader.GetPropDateTime=%', [v.VType]);
  VariantToDateTime(variant(v), result);
end;

function T7zReader.GetPropFileTime(item: cardinal; prop: TPropID): Int64;
var
  v: TVarData; // not handled by the RTL
begin
  v.VType := 0;
  E7Zip.CheckOK(self, 'GetPropFileTime',
    fInArchive.GetProperty(Item, prop, variant(v)));
  result := 0;
  if v.VType = VT_FILETIME then
    result := v.VInt64;
end;

function T7zReader.GetIsFolder(index: integer): boolean;
begin
  result := boolean(GetProp(index, kpidIsFolder));
end;

function T7zReader.GetZipName(index: integer): RawUtf8;
begin
  GetPropUtf8(index, kpidPath, result);
end;

function T7zReader.GetModDate(index: integer): TDateTime;
begin
  result := GetPropDateTime(index, kpidLastWriteTime);
end;

function T7zReader.GetCreateDate(index: integer): TDateTime;
begin
  result := GetPropDateTime(index, kpidCreationTime);
end;

function T7zReader.GetAttributes(index: integer): cardinal;
begin
  result := VariantToInt64Def(GetProp(index, kpidAttributes), 0);
end;

function T7zReader.GetPackSize(index: integer): Int64;
begin
  result := VariantToInt64Def(GetProp(index, kpidPackedSize), -1);
end;

function T7zReader.GetComment(index: integer): RawUtf8;
begin
  GetPropUtf8(index, kpidComment, result);
end;

function T7zReader.GetCrc(index: integer): cardinal;
begin
  result := VariantToInt64Def(GetProp(index, kpidCRC), -1);
end;

function T7zReader.GetName(index: integer): RawUtf8;
begin
  GetPropUtf8(index, kpidName, result);
end;

function T7zReader.GetMethod(index: integer): RawUtf8;
begin
  GetPropUtf8(index, kpidMethod, result);
end;

function T7zReader.GetSize(index: integer): Int64;
begin
  result := VariantToInt64Def(GetProp(index, kpidSize), -1);
end;

function T7zReader.Count: integer;
begin
  if fInArchive = nil then
    result := 0
  else
    E7Zip.CheckOk(self, 'GetNumberOfItems',
      fInArchive.GetNumberOfItems(PCardinal(@result)^));
end;

function T7zReader.NameToIndex(const zipname: RawUtf8): integer;
begin
  if fInArchive <> nil then
    for result := 0 to Count - 1 do
      if IdemPropNameU(zipname, GetZipName(result)) then
        exit;
  result := -1;
end;

const
  MAXCHECK: Int64 = 1 shl 20;

procedure T7zReader.OpenFile(const name: TFileName);
var
  strm: IInStream;
begin
  strm := T7zStream.CreateFromFile(name, fmOpenReadDenyNone);
  E7Zip.CheckOk(self, 'OpenFile',
    InArchive.Open(strm, @MAXCHECK, self as IArchiveOpenCallBack));
  fFileName := name;
end;

procedure T7zReader.OpenStream(stream: IInStream);
begin
  E7Zip.CheckOk(self, 'OpenStream',
    InArchive.Open(stream, @MAXCHECK, self as IArchiveOpenCallBack));
end;

procedure T7zReader.Extract(item: cardinal; Stream: TStream);
begin
  EnsureOpened;
  fStream := Stream;
  try
    E7Zip.CheckOk(self, 'Extract',
      fInArchive.Extract(
        @item, 1, ord(Stream = nil), self as IArchiveExtractCallback));
  finally
    fStream := nil;
  end;
end;

procedure T7zReader.Extract(item: cardinal; const path: TFileName;
  nosubfolder: boolean);
begin
  EnsureOpened;
  fExtractPath := EnsureDirectoryExists(path);
  fExtractPathNoSubFolder := nosubfolder;
  try
    E7Zip.CheckOk(self, 'Extract',
      fInArchive.Extract(
        @item, 1, {test=}0, self as IArchiveExtractCallback));
  finally
    fExtractPath := '';
  end;
end;

function T7zReader.Extract(const zipname: RawUtf8; Stream: TStream): boolean;
var
  i: integer;
begin
  result := false;
  i := NameToIndex(zipname);
  if i >= 0 then
    try
      Extract(i, Stream);
      result := true;
    except
      result := false;
    end;
end;

function T7zReader.Extract(const zipname: RawUtf8): RawByteString;
var
  s: TRawByteStringStream;
begin
  result := '';
  s := TRawByteStringStream.Create;
  try
    if Extract(zipname, s) then
      result := s.DataString
  finally
    s.Free;
  end;
end;

function T7zReader.Extract(const zipname: RawUtf8; const path: TFileName;
  nosubfolder: boolean): boolean;
var
  i: integer;
begin
  result := false;
  i := NameToIndex(zipname);
  if i >= 0 then
    try
      Extract(i, path, nosubfolder);
      result := true;
    except
      result := false;
    end;
end;

function T7zReader.Extract(const zipnames: array of RawUtf8;
  const path: TFileName; nosubfolder: boolean): TRawUtf8DynArray;
var
  i: integer;
begin
  result := nil;
  for i := 0 to high(zipnames) do
    if not Extract(zipnames[i], path, nosubfolder) then
      AddRawUtf8(result, zipnames[i]);
end;

function T7zReader.GetStream(index: cardinal;
  var outStream: ISequentialOutStream;
  askExtractMode: T7zExtractAskMode): HRESULT;
var
  path: TFileName;
begin
  case askExtractMode of
    eamExtract:
      if fStream <> nil then
        outStream := T7zStream.Create(fStream, false, index)
      else if assigned(fExtractCallback) then
      begin
        result := fExtractCallback(self, index, outStream);
        exit;
      end
      else if fExtractPath <> '' then
        if not GetIsFolder(index) then
        begin
          path := Utf8ToString(GetZipName(index));
          if fExtractPathNoSubFolder then
            path := ExtractFileName(path);
          path := fExtractPath + path;
          ForceDirectories(ExtractFilePath(path));
          outStream := T7zStream.CreateFromFile(path, fmCreate, index);
          with fExtractCurrent do
          begin
            FileName := path;
            Created := GetPropFileTime(index, kpidCreationTime);
            Accessed := GetPropFileTime(index, kpidLastAccessTime);
            Written := GetPropFileTime(index, kpidLastWriteTime);
          end;
        end;
  end;
  result := S_OK; // S_FALSE would skip this file
end;

function T7zReader.PrepareOperation(
  askExtractMode: T7zExtractAskMode): HRESULT;
begin
  result := S_OK;
end;

function T7zReader.SetCompleted(files, bytes: PInt64): HRESULT;
begin
  result := S_OK;
end;

function T7zReader.SetOperationResult(
  opResult: T7zExtractOperationResult): HRESULT;
begin
  case opResult of
    eorOK:
      with fExtractCurrent do
        if (FileName <> '') and
           ((Written or Created or Accessed) <> 0) then
          FileSetTime(FileName, Created, Accessed, Written);
  end;
  fExtractCurrent.FileName := '';
  result := S_OK;
end;

function T7zReader.SetTotal(files, bytes: PInt64): HRESULT;
begin
  result := S_OK;
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

function T7zReader.SetSubArchiveName(name: PWideChar): HRESULT;
begin
  fSubArchiveMode := true;
  fSubArchiveName := UnicodeBufferToUtf8(name);
  result := S_OK;
end;

procedure T7zReader.Extract(const items: array of integer;
  const callback: T7zGetStreamCallBack);
var
  n: PtrInt;
  sorted: TIntegerDynArray;
begin
  EnsureOpened;
  n := length(items);
  if n = 0 then
    exit;
  SetLength(sorted, n);
  MoveFast(items[0], sorted[0], n shl 2);
  QuickSortInteger(sorted); // indexes should be sorted
  fExtractCallback := callback;
  try
    E7Zip.CheckOk(self, 'Extract', fInArchive.Extract(pointer(sorted), n,
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
    E7Zip.CheckOk(self, 'ExtractAll', fInArchive.Extract(
      nil, $FFFFFFFF, ord(Assigned(callback)), self as IArchiveExtractCallback));
  finally
    fExtractCallback := nil;
  end;
end;

procedure T7zReader.ExtractAll(const path: TFileName; nosubfolder: boolean);
begin
  EnsureOpened;
  fExtractPath := EnsureDirectoryExists(path);
  fExtractPathNoSubFolder := nosubfolder;
  try
    E7Zip.CheckOk(self, 'ExtractAll', fInArchive.Extract(
      nil, $FFFFFFFF, 0, self as IArchiveExtractCallback));
  finally
    fExtractPath := '';
  end;
end;


{ T7zStream }

constructor T7zStream.Create(Stream: TStream; OwnedStream: boolean;
  ArchiveIndex: integer);
begin
  inherited Create;
  fStream := Stream;
  fOwnedStream := OwnedStream;
  fIndex := ArchiveIndex;
end;

constructor T7zStream.CreateFromFile(const Name: TFileName; Mode: cardinal;
  ArchiveIndex: integer);
begin
  Create(TFileStreamEx.Create(Name, Mode), {owned=}true, ArchiveIndex);
end;

destructor T7zStream.Destroy;
begin
  inherited;
  if fOwnedStream then
    FreeAndNil(fStream);
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


{ T7zItem }

destructor T7zItem.Destroy;
begin
  if Ownership = soOwned then
    Stream.Free;
  inherited;
end;


{ T7zWriter }

constructor T7zWriter.Create(lib: T7zLib; const rd: I7zReader; libowned: boolean);
var
  i: PtrInt;
  item: T7zItem;
begin
  inherited Create(lib, rd.Format, libowned);
  fFileName := rd.FileName; 
  fUpdateReader := rd;
  for i := 0 to rd.Count - 1 do
  begin
    item := T7zItem.Create;
    item.SourceMode := smUpdate;
    item.ZipName := rd.ZipName[i];
    item.Size := rd.Size[i];
    item.Attributes := rd.Attributes[i];
    item.IsFolder := rd.IsFolder[i];
    item.UpdateItemIndex := i;
    GetSystemTimeAsFileTime(item.LastWriteTime);
    ObjArrayAdd(fEntries, item);
  end;
end;

procedure T7zWriter.AddOrReplace(item: T7zItem);
var
  i: PtrInt;
begin
  i := NameToIndex(item.ZipName);
  if i >= 0 then
  begin
    fEntries[i].Free; // replace
    fEntries[i] := item;
  end
  else
    ObjArrayAdd(fEntries, item);
end;

function T7zWriter.AddFile(const Filename: TFileName; const ZipName: RawUtf8): boolean;
var
  item: T7zItem;
  Handle: THandle;
begin
  Handle := FileOpen(Filename, fmOpenReadDenyNone);
  result := ValidHandle(Handle);
  if not result then
    exit;
  item := T7zItem.Create;
  item.SourceMode := smFile;
  item.FileName := Filename;
  item.ZipName := ZipName;
  GetFileTime(Handle, @item.CreationTime, nil, @item.LastWriteTime);
  item.Size := FileSize(Handle);
  CloseHandle(Handle);
  item.Attributes := GetFileAttributes(pointer(Filename));
  item.IsFolder := false;
  item.IsAnti := false;
  item.Ownership := soOwned;
  item.UpdateItemIndex := -1;
  AddOrReplace(item);
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
      FindClose(f);
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
        StringToUtf8(fn, item.ZipName);
        item.CreationTime := f.FindData.ftCreationTime;
        item.LastWriteTime := f.FindData.ftLastWriteTime;
        item.Attributes := f.FindData.dwFileAttributes;
        item.Size := f.Size;
        item.IsFolder := false;
        item.IsAnti := false;
        item.Ownership := soOwned;
        item.UpdateItemIndex := -1;
        AddOrReplace(item);
      until FindNext(f) <> 0;
      FindClose(f);
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

procedure T7zWriter.AddBuffer(const ZipName: RawUtf8;
  const Data: RawByteString);
begin
  AddStream(TRawByteStringStream.Create(Data), soReference,
    faArchive, 0, 0, ZipName, false, false);
end;

procedure T7zWriter.AddStream(Stream: TStream; Ownership: TStreamOwnership;
  Attributes: cardinal; CreationTime, LastWriteTime: TUnixTime;
  const ZipName: RawUtf8; IsFolder, IsAnti: boolean);
var
  item: T7zItem;
begin
  item := T7zItem.Create;
  Item.SourceMode := smStream;
  item.Attributes := Attributes;
  if CreationTime <> 0 then
    UnixTimeToFileTime(CreationTime, item.CreationTime);
  if LastWriteTime <> 0 then
    UnixTimeToFileTime(LastWriteTime, item.LastWriteTime)
  else
    GetSystemTimeAsFileTime(item.LastWriteTime);
  item.ZipName := ZipName;
  item.IsFolder := IsFolder;
  item.IsAnti := IsAnti;
  item.Stream := Stream;
  item.Size := Stream.Size;
  item.Ownership := Ownership;
  item.UpdateItemIndex := -1;
  AddOrReplace(item);
end;

function T7zWriter.Delete(index: integer): boolean;
begin
  if cardinal(index) < cardinal(length(fEntries)) then
  begin
    ObjArrayDelete(fEntries, index);
    result := true;
  end
  else
    result := false;
end;

function T7zWriter.Delete(const ZipName: RawUtf8): boolean;
begin
  result := Delete(NameToIndex(ZipName));
end;

procedure T7zWriter.Clear;
begin
  ObjArrayClear(fEntries);
  SetPasswordCallback(nil);
end;

destructor T7zWriter.Destroy;
begin
  Clear;
  fUpdateReader := nil;
  fOutArchive := nil;
  inherited;
end;

function T7zWriter.OutArchive: IOutArchive;
begin
  if fOutArchive = nil then
    if not Assigned(fUpdateReader) then
      fOwner.CreateObject(fClassID, IOutArchive, fOutArchive)
    else if not Supports(fUpdateReader.InArchive, IOutArchive, fOutArchive) then
      raise E7Zip.CreateUtf8('%.OutArchive: % format can not be updated',
        [self, fUpdateReader.FormatExt]);
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
      kpidNoProperty:
        VType := varNull;
      kpidAttributes:
        begin
          VType := VT_UI4; // = varLongWord
          VLongWord := item.Attributes;
        end;
      kpidLastWriteTime:
        if Int64(item.LastWriteTime) <> 0 then
        begin
          VType := VT_FILETIME; // = varOleFileTime
          VInt64 := Int64(item.LastWriteTime);
        end;
      kpidCreationTime:
        if Int64(item.CreationTime) <> 0 then
        begin
          VType := VT_FILETIME;
          VInt64 := Int64(item.CreationTime);
        end;
      kpidTimeType:
        begin
          VType := VT_UI4;
          VLongWord := ord(fttWindows);
        end;
      kpidPath:
        if item.ZipName <> '' then
          value := Utf8ToWideString(item.ZipName);
      kpidIsFolder:
        Value := item.IsFolder;
      kpidIsAnti:
        value := item.IsAnti;
      kpidSize:
        begin
          VType := VT_UI8; // = varWord64
          VInt64 := item.Size;
        end;
    end;
  result := S_OK;
end;

function T7zWriter.GetStream(index: cardinal;
  var inStream: ISequentialInStream): HRESULT;
begin
  if index >= cardinal(length(fEntries)) then
  begin
    result := ERROR_INVALID_PARAMETER;
    exit;
  end;
  result := S_OK;
  fCurrentItem := fEntries[index];
  case fCurrentItem.SourceMode of
    smFile:
      inStream := T7zStream.CreateFromFile(
        fCurrentItem.FileName, fmOpenReadDenyNone, index);
    smStream:
      begin
        fCurrentItem.Stream.Seek(0, soFromBeginning);
        inStream := T7zStream.Create(fCurrentItem.Stream, {owned=}true, index);
      end;
  else
    result := ERROR_INVALID_PARAMETER;
  end;
end;

function T7zWriter.GetUpdateItemInfo(index: cardinal;
  newData, newProperties: PInteger; indexInArchive: PCardinal): HRESULT;
var
  d, p, a: integer;
begin
  if index >= cardinal(length(fEntries)) then
  begin
    result := ERROR_INVALID_PARAMETER;
    exit;
  end;
  {
   newData^ newProperties^
   0        0      Copy data and properties from archive
   0        1      Copy data from archive, request new properties
   1        0      that combination is unused now
   1        1      Request new data and new properties. Can be a folder.
  indexInArchive=-1 if there is no item in archive, or if it doesn't matter
  }
  with fEntries[index] do
  begin
    a := UpdateItemIndex;
    p := ord(a < 0);
    d := ord(a < 0);
  end;
  if newData <> nil then
    newData^ := d;
  if newProperties <> nil then
    newProperties^ := p;
  if indexInArchive <> nil then
    indexInArchive^ := a;
  result := S_OK;
end;

procedure T7zWriter.SaveToFile(const DestName: TFileName);
var
  f: TFileStreamEx;
begin
  fFileName := DestName;
  f := TFileStreamEx.Create(DestName, fmCreate);
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
  strm := T7zStream.Create(stream, {owned=}false);
  try
    E7Zip.CheckOk(self, 'SaveToStream',
      OutArchive.UpdateItems(strm, length(fEntries), self as IArchiveUpdateCallback));
  finally
    strm := nil;
  end;
end;

function T7zWriter.SetOperationResult(operationResult: integer): HRESULT;
begin
  fCurrentItem := nil;
  result := S_OK;
end;

function T7zWriter.GetVolumeSize(index: cardinal; size: PInt64): HRESULT;
begin
  result := S_FALSE;
end;

function T7zWriter.GetVolumeStream(index: cardinal;
  var volumeStream: ISequentialOutStream): HRESULT;
begin
  result := S_FALSE;
end;

procedure T7zWriter.SetProperty(const zipname: RawUtf8;
  const value: T7zVariant);
var
  intf: ISetProperties;
  p: PWideChar;
begin
  p := pointer(Utf8DecodeToUnicodeRawByteString(zipname));
  if Supports(OutArchive, ISetProperties, intf) then
    E7Zip.CheckOk(self, 'SetProperty', intf.SetProperties(@p, @value, 1));
end;

procedure T7zWriter.SetCardinalProperty(const zipname: RawUtf8; card: cardinal);
var
  v: TVarData;
begin
  v.VType := VT_UI4;
  v.VLongWord := card;
  SetProperty(zipname, T7zVariant(v));
end;

procedure T7zWriter.SetTextProperty(const zipname, text: RawUtf8);
var
  v: T7zVariant;
begin
  v := Utf8ToWideString(text);
  SetProperty(zipname, v);
end;

function T7zWriter.Count: integer;
begin
  result := length(fEntries);
end;

function T7zWriter.NameToIndex(const zipname: RawUtf8): integer;
begin
  for result := 0 to length(fEntries) - 1 do
    if IdemPropNameU(fEntries[result].ZipName, zipname) then
      exit;
  result := -1;
end;

function T7zWriter.Get(index: PtrUInt): T7zItem;
begin
  if index < PtrUInt(length(fEntries)) then
    result := fEntries[index]
  else
    raise E7Zip.CreateUtf8('Out of range %.Get(%)', [self, index]);
end;

function T7zWriter.GetZipName(index: integer): RawUtf8;
begin
  result := Get(index).ZipName;
end;

function T7zWriter.GetName(index: integer): RawUtf8;
begin
  index := Get(index).UpdateItemIndex;
  if index < 0 then
    result := ''
  else
    result := fUpdateReader.GetName(index);
end;

function T7zWriter.GetMethod(index: integer): RawUtf8;
begin
  index := Get(index).UpdateItemIndex;
  if index < 0 then
    result := ''
  else
    result := fUpdateReader.GetMethod(index);
end;

function T7zWriter.GetSize(index: integer): Int64;
begin
  result := Get(index).Size;
end;

function T7zWriter.GetIsFolder(index: integer): boolean;
begin
  result := Get(index).IsFolder;
end;

function T7zWriter.GetPackSize(index: integer): Int64;
begin
  index := Get(index).UpdateItemIndex;
  if index < 0 then
    result := -1
  else
    result := fUpdateReader.GetPackSize(index);
end;

function T7zWriter.GetCrc(index: integer): cardinal;
begin
  index := Get(index).UpdateItemIndex;
  if index < 0 then
    result := 0
  else
    result := fUpdateReader.GetCrc(index);
end;

function T7zWriter.GetComment(index: integer): RawUtf8;
begin
  index := Get(index).UpdateItemIndex;
  if index < 0 then
    result := ''
  else
    result := fUpdateReader.GetComment(index);
end;

function T7zWriter.GetModDate(index: integer): TDateTime;
var
  item: T7zItem;
begin
  item := Get(index);
  index := item.UpdateItemIndex;
  if index < 0 then
    result := FileTimeToDateTime(item.LastWriteTime)
  else
    result := fUpdateReader.GetModDate(index);
end;

function T7zWriter.GetCreateDate(index: integer): TDateTime;
begin
  index := Get(index).UpdateItemIndex;
  if index < 0 then
    result := 0
  else
    result := fUpdateReader.GetCreateDate(index);
end;

function T7zWriter.GetAttributes(index: integer): cardinal;
begin
  result := Get(index).Attributes;
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



{$endif OSPOSIX}

end.

