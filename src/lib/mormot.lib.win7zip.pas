/// access to the 7-Zip library on Windows
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.lib.win7zip;

{
  *****************************************************************************

   Access to the 7-Zip Compression/Decompression DLL on Windows 
   - Low-Level 7-Zip API Definitions
   - I7zInArchive/I7zOutArchive High-Level Wrappers

  *****************************************************************************

}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  types,
  ActiveX,
  mormot.core.base,
  mormot.core.os,      // for TSynLibrary
  mormot.core.unicode,
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
  E7Zip = class(ExceptionWithProps)
  public
    class procedure Check(Caller: TObject; const Context: shortstring; Res: HResult);
    class procedure CheckOk(Caller: TObject; const Context: shortstring; Res: HResult);
  end;

type
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
    function CryptoGetTextPassword2(passwordIsDefined: PInteger; var password: TBStr): HRESULT; stdcall;
  end;

  ISequentialInStream = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000300010000}']
    function Read(data: Pointer; size: cardinal; processedSize: PCardinal): HRESULT; stdcall;
  end;

  ISequentialOutStream = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000300020000}']
    function Write(data: Pointer; size: cardinal; processedSize: PCardinal): HRESULT; stdcall;
  end;

  IInStream = interface(ISequentialInStream)
    ['{23170F69-40C1-278A-0000-000300030000}']
    function Seek(offset: Int64; seekOrigin: cardinal; newPosition: PInt64): HRESULT; stdcall;
  end;

  IOutStream = interface(ISequentialOutStream)
    ['{23170F69-40C1-278A-0000-000300040000}']
    function Seek(offset: Int64; seekOrigin: cardinal; newPosition: PInt64): HRESULT; stdcall;
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
    function PrepareOperation(askExtractMode: T7zExtractAskMode): HRESULT; stdcall;
    function SetOperationResult(resultEOperationResult: T7zExtractOperationResult): HRESULT; stdcall;
  end;

  IArchiveOpenVolumeCallback = interface
    ['{23170F69-40C1-278A-0000-000600300000}']
    function GetProperty(propID: TPropID; var value: variant): HRESULT; stdcall;
    function GetStream(const name: PWideChar; var inStream: IInStream): HRESULT; stdcall;
  end;

  IInArchiveGetStream = interface
    ['{23170F69-40C1-278A-0000-000600400000}']
    function GetStream(index: cardinal; var stream: ISequentialInStream ): HRESULT; stdcall;
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
    function GetProperty(index: cardinal; propID: TPropID; var value: variant): HRESULT; stdcall;
    function Extract(indices: PCardinalArray; numItems: cardinal;
      testMode: Integer; extractCallback: IArchiveExtractCallback): HRESULT; stdcall;
    function GetArchiveProperty(propID: TPropID; var value: variant): HRESULT; stdcall;
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
    function GetProperty(index: cardinal; propID: TPropID; var value: variant): HRESULT; stdcall;
    function GetStream(index: cardinal; var inStream: ISequentialInStream): HRESULT; stdcall;
    function SetOperationResult(operationResult: Integer): HRESULT; stdcall;
  end;

  IArchiveUpdateCallback2 = interface(IArchiveUpdateCallback)
    ['{23170F69-40C1-278A-0000-000600820000}']
    function GetVolumeSize(index: cardinal; size: PInt64): HRESULT; stdcall;
    function GetVolumeStream(index: cardinal; var volumeStream: ISequentialOutStream): HRESULT; stdcall;
  end;

  IOutArchive = interface
    ['{23170F69-40C1-278A-0000-000600A00000}']
    function UpdateItems(outStream: ISequentialOutStream; numItems: cardinal;
      updateCallback: IArchiveUpdateCallback): HRESULT; stdcall;
    function GetFileTimeType(type_: PCardinal): HRESULT; stdcall;
  end;

  ISetProperties = interface
    ['{23170F69-40C1-278A-0000-000600030000}']
    function SetProperties(names: PPWideChar; values: PPropVariant;
    numProperties: Integer): HRESULT; stdcall;
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
    function Code(var inStreams: ISequentialInStream;
      var inSizes: PInt64; numInStreams: cardinal; var outStreams: ISequentialOutStream;
      var outSizes: PInt64; numOutStreams: cardinal;
      progress: ICompressProgressInfo): HRESULT; stdcall;
  end;

  ICompressSetCoderProperties = interface
    ['{23170F69-40C1-278A-0000-000400200000}']
    function SetCoderProperties(propIDs: PPropID;
      properties: PPropVariant; numProperties: cardinal): HRESULT; stdcall;
  end;

  ICompressSetDecoderProperties2 = interface
    ['{23170F69-40C1-278A-0000-000400220000}']
    function SetDecoderProperties2(data: PByte; size: cardinal): HRESULT; stdcall;
  end;

  ICompressWriteCoderProperties = interface
    ['{23170F69-40C1-278A-0000-000400230000}']
    function WriteCoderProperties(outStreams: ISequentialOutStream): HRESULT; stdcall;
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


{ ****************** I7zInArchive/I7zOutArchive High-Level Wrappers }

type
  T7zPasswordCallback = function(sender: Pointer;
    var password: UnicodeString): HRESULT; stdcall;
  T7zGetStreamCallBack = function(sender: Pointer; index: Cardinal;
    var outStream: ISequentialOutStream): HRESULT; stdcall;
  T7zProgressCallback = function(sender: Pointer; total: boolean;
    value: int64): HRESULT; stdcall;

  I7zInArchive = interface
    ['{9C0C0C8C-883A-49ED-B608-A6D66D36D53A}']
    // some internal methods used as property getters / setters
    function GetItemCRC(const index: integer): AnsiString; //domasz
    function GetItemPackSize(const index: integer): Cardinal; //domasz
    function GetItemComment(const index: integer): UnicodeString; //domasz
    function GetItemModDate(const index: integer): TDateTime; //domasz
    function GetItemDate(const index: integer): TDateTime; //domasz
    function GetItemPath(const index: integer): UnicodeString; stdcall;
    function GetItemName(const index: integer): UnicodeString; stdcall;
    function GetItemSize(const index: integer): Cardinal; stdcall;
    function GetItemIsFolder(const index: integer): boolean; stdcall;
    function GetClassId: TGUID;
    procedure SetClassId(const classid: TGUID);
    // main high-level methods
    procedure OpenFile(const filename: string); stdcall;
    procedure OpenStream(stream: IInStream); stdcall;
    procedure Close; stdcall;
    procedure ExtractItem(const item: Cardinal; Stream: TStream; test: boolean); stdcall;
    procedure ExtractItems(items: PCardinalArray; count: cardinal; test: boolean;
      sender: pointer; callback: T7zGetStreamCallBack); stdcall;
    procedure ExtractAll(test: boolean; sender: pointer; callback: T7zGetStreamCallBack); stdcall;
    procedure ExtractTo(const path: string); stdcall;
    procedure SetPasswordCallback(sender: Pointer; callback: T7zPasswordCallback); stdcall;
    procedure SetPassword(const password: UnicodeString); stdcall;
    procedure SetProgressCallback(sender: Pointer; callback: T7zProgressCallback); stdcall;
    function InArchive: IInArchive; stdcall;
    function NumberOfItems: Cardinal; stdcall;
    property ClassId: TGUID
      read GetClassId write SetClassId;
    property ItemPath[const index: integer]: UnicodeString
      read GetItemPath;
    property ItemName[const index: integer]: UnicodeString
      read GetItemName;
    property ItemSize[const index: integer]: Cardinal
      read GetItemSize;
    property ItemIsFolder[const index: integer]: boolean
      read GetItemIsFolder;
    property ItemComment[const index: integer]: UnicodeString
      read GetItemComment; //domasz
    property ItemCRC[const index: integer]: AnsiString
      read GetItemCRC; //domasz
    property ItemDate[const index: integer]: TDateTime
      read GetItemDate; //domasz
    property ItemModDate[const index: integer]: TDateTime
      read GetItemModDate;  //domasz
    property ItemPackSize[const index: integer]: Cardinal
      read GetItemPackSize; //domasz
  end;

  I7zOutArchive = interface
    ['{9C0C0C8C-883A-49ED-B608-A6D66D36D53B}']
    // some internal methods used as property getters / setters
    procedure SetClassId(const classid: TGUID);
    function GetClassId: TGUID;
    // main high-level methods
    procedure AddStream(Stream: TStream; Ownership: TStreamOwnership; Attributes: Cardinal;
      CreationTime, LastWriteTime: TFileTime; const Path: UnicodeString;
      IsFolder, IsAnti: boolean); stdcall;
    procedure AddFile(const Filename: TFileName; const Path: UnicodeString); stdcall;
    procedure AddFiles(const Dir, Path, Wildcard: string; recurse: boolean); stdcall;
    procedure SaveToFile(const FileName: TFileName); stdcall;
    procedure SaveToStream(stream: TStream); stdcall;
    procedure SetProgressCallback(sender: Pointer; callback: T7zProgressCallback); stdcall;
    procedure ClearBatch; stdcall;
    procedure SetPassword(const password: UnicodeString); stdcall;
    procedure SetProperty(const name: UnicodeString; value: variant); stdcall;
    function OutArchive: IOutArchive; stdcall;
    property ClassId: TGUID
      read GetClassId write SetClassId;
  end;

  I7zCodec = interface
    ['{9C0C0C8C-883A-49ED-B608-A6D66D36D53C}']
  end;

type
  /// the supported archive formats
  // - use HandlerGuid() to retrieve the TGuid of a given archive format
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

/// return the GUID of a given archive format
// - in the form '{23170F69-40C1-278A-1000-000110xx0000}'
function HandlerGuid(h: T7zFormatHandler): TGuid;

/// try to detect the proper archive format of a file
function DetectFormat(const FileName: TFileName): T7zFormatHandler;

/// factory of the main I7ZInArchive high-level archive decompressor
function New7ZInArchive(fmt: T7zFormatHandler;
  const lib: TFileName = '7z.dll'): I7ZInArchive;

/// factory of the main I7ZOutArchive high-level archive compressor
function New7ZOutArchive(fmt: T7zFormatHandler;
  const lib: TFileName = '7z.dll'): I7ZOutArchive;


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

function HandlerGuid(h: T7zFormatHandler): TGuid;
begin
  result := HANDLER_GUID;
  result.D4[5] := HANDLER_TO_GUID[h];
end;


{ E7Zip }

class procedure E7Zip.Check(Caller: TObject; const Context: shortstring; Res: HResult);
begin
  if Res and $80000000 <> 0 then
    raise CreateFmt('%s.%s error %x (%s)',
      [ClassNameShort(Caller)^, Context, Res, string(WinErrorText(Res, nil))])
end;

class procedure E7Zip.CheckOk(Caller: TObject; const Context: shortstring;
  Res: HResult);
begin
  if Res <> S_OK then
    raise CreateFmt('%s.%s error %x (%s)',
      [ClassNameShort(Caller)^, Context, Res, string(WinErrorText(Res, nil))])
end;



{ ****************** I7zInArchive/I7zOutArchive High-Level Wrappers }

type
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

  T7zStream = class(TInterfacedObject, IInStream, IStreamGetSize,
    ISequentialOutStream, ISequentialInStream, IOutStream, IOutStreamFlush)
  private
    FStream: TStream;
    FOwnership: TStreamOwnership;
  protected
    function Read(data: Pointer; size: Cardinal; processedSize: PCardinal): HRESULT; stdcall;
    function Seek(offset: Int64; seekOrigin: Cardinal; newPosition: Pint64): HRESULT; stdcall;
    function GetSize(size: PInt64): HRESULT; stdcall;
    function SetSize(newSize: Int64): HRESULT; stdcall;
    function Write(data: Pointer; size: Cardinal; processedSize: PCardinal): HRESULT; stdcall;
    function Flush: HRESULT; stdcall;
  public
    constructor Create(Stream: TStream; Ownership: TStreamOwnership = soReference);
    destructor Destroy; override;
  end;

  T7zPlugin = class(TInterfacedObject)
  private
    FHandle: THandle;
    // the raw function from 7Zip.dll
    FCreateObject: function(const clsid, iid: TGUID; var outObject): HRESULT; stdcall;
  public
    constructor Create(const lib: TFileName); virtual;
    destructor Destroy; override;
    procedure CreateObject(const clsid, iid: TGUID; var obj);
  end;

  T7zCodec = class(T7zPlugin, I7zCodec, ICompressProgressInfo)
  private
    // the raw functions from 7Zip.dll
    FGetMethodProperty: function(index: Cardinal; propID: T7zMethodPropID;
      var value: variant): HRESULT; stdcall;
    FGetNumberOfMethods: function(numMethods: PCardinal): HRESULT; stdcall;
  protected
    function GetNumberOfMethods: Cardinal;
    function GetMethodProperty(index: Cardinal; propID: T7zMethodPropID): variant;
    function GetName(const index: integer): string;
    function SetRatioInfo(inSize, outSize: PInt64): HRESULT; stdcall;
  public
    function GetDecoder(const index: integer): ICompressCoder;
    function GetEncoder(const index: integer): ICompressCoder;
    constructor Create(const lib: TFileName); override;
    property MethodProperty[index: Cardinal; propID: T7zMethodPropID]: variant
      read GetMethodProperty;
    property NumberOfMethods: Cardinal read GetNumberOfMethods;
    property Name[const index: integer]: string read GetName;
  end;

  T7zArchive = class(T7zPlugin)
  private
    // the raw function from 7Zip.dll
    FGetHandlerProperty: function(propID: T7zHandlerPropID;
      var value: variant): HRESULT; stdcall;
  protected
    FClassId: TGUID;
    procedure SetClassId(const classid: TGUID);
    function GetClassId: TGUID;
  public
    function GetHandlerProperty(const propID: T7zHandlerPropID): variant;
    function GetLibStringProperty(const Index: T7zHandlerPropID): string;
    function GetLibGUIDProperty(const Index: T7zHandlerPropID): TGUID;
    constructor Create(const lib: TFileName); override;
    property HandlerProperty[const propID: T7zHandlerPropID]: variant
      read GetHandlerProperty;
    property Name: string
      index hpiName read GetLibStringProperty;
    property ClassID: TGUID
      read GetClassId write SetClassId;
    property Extension: string
      index hpiExtension read GetLibStringProperty;
  end;

  T7zInArchive = class(T7zArchive, I7zInArchive, IProgress, IArchiveOpenCallback,
    IArchiveExtractCallback, ICryptoGetTextPassword, IArchiveOpenVolumeCallback,
    IArchiveOpenSetSubArchiveName)
  private
    FInArchive: IInArchive;
    FPasswordCallback: T7zPasswordCallback;
    FPasswordSender: Pointer;
    FProgressCallback: T7zProgressCallback;
    FProgressSender: Pointer;
    FStream: TStream;
    FPasswordIsDefined: Boolean;
    FPassword: UnicodeString;
    FSubArchiveMode: Boolean;
    FSubArchiveName: UnicodeString;
    FExtractCallBack: T7zGetStreamCallBack;
    FExtractSender: Pointer;
    FExtractPath: string;
    function GetItemProp(const Item: Cardinal; prop: TPropID): variant;
  protected
    // I7zInArchive methods
    procedure OpenFile(const filename: string); stdcall;
    procedure OpenStream(stream: IInStream); stdcall;
    procedure Close; stdcall;
    function InArchive: IInArchive; stdcall;
    function NumberOfItems: Cardinal; stdcall;
    function GetItemPath(const index: integer): UnicodeString; stdcall;
    function GetItemName(const index: integer): UnicodeString; stdcall;
    function GetItemSize(const index: integer): Cardinal; stdcall;
    function GetItemIsFolder(const index: integer): boolean; stdcall;
    procedure ExtractItem(const item: Cardinal; Stream: TStream; test: boolean); stdcall;
    procedure ExtractItems(items: PCardinalArray; count: cardinal; test: boolean;
      sender: pointer; callback: T7zGetStreamCallBack); stdcall;
    procedure SetPasswordCallback(sender: Pointer; callback: T7zPasswordCallback); stdcall;
    procedure SetProgressCallback(sender: Pointer; callback: T7zProgressCallback); stdcall;
    procedure ExtractAll(test: boolean; sender: pointer; callback: T7zGetStreamCallBack); stdcall;
    procedure ExtractTo(const path: string); stdcall;
    procedure SetPassword(const password: UnicodeString); stdcall;
    // IArchiveOpenCallback
    function SetTotal(files, bytes: PInt64): HRESULT; overload; stdcall;
    function SetCompleted(files, bytes: PInt64): HRESULT; overload; stdcall;
    // IProgress
    function SetTotal(total: Int64): HRESULT;  overload; stdcall;
    function SetCompleted(completeValue: PInt64): HRESULT; overload; stdcall;
    // IArchiveExtractCallback
    function GetStream(index: Cardinal; var outStream: ISequentialOutStream;
      askExtractMode: T7zExtractAskMode): HRESULT; overload; stdcall;
    function PrepareOperation(askExtractMode: T7zExtractAskMode): HRESULT; stdcall;
    function SetOperationResult(
      resultEOperationResult: T7zExtractOperationResult): HRESULT; overload; stdcall;
    // ICryptoGetTextPassword
    function CryptoGetTextPassword(var password: TBStr): HRESULT; stdcall;
    // IArchiveOpenVolumeCallback
    function GetProperty(propID: TPropID; var value: variant): HRESULT; overload; stdcall;
    function GetStream(const name: PWideChar; var inStream: IInStream): HRESULT; overload; stdcall;
    // IArchiveOpenSetSubArchiveName
    function SetSubArchiveName(name: PWideChar): HRESULT; stdcall;
    function GetItemPackSize(const index: integer): Cardinal; //domasz
    function GetItemCRC(const index: integer): AnsiString;
    function GetItemComment(const index: integer): UnicodeString;
    function GetItemModDate(const index: integer): TDateTime;
    function GetItemDate(const index: integer): TDateTime;
  public
    constructor Create(const lib: TFileName); override;
    destructor Destroy; override;
  end;

  T7zBatchSourceMode = (
    smStream,
    smFile);

  T7zBatchItem = class
  public
    SourceMode: T7zBatchSourceMode;
    Stream: TStream;
    Attributes: Cardinal;
    CreationTime, LastWriteTime: TFileTime;
    Path: UnicodeString;
    IsFolder, IsAnti: boolean;
    FileName: TFileName;
    Ownership: TStreamOwnership;
    Size: Int64;
    destructor Destroy; override;
  end;

  T7zOutArchive = class(T7zArchive, I7zOutArchive, IArchiveUpdateCallback, ICryptoGetTextPassword2)
  private
    FOutArchive: IOutArchive;
    FBatchList: array of T7zBatchItem;
    FProgressCallback: T7zProgressCallback;
    FProgressSender: Pointer;
    FPassword: UnicodeString;
  protected
    // I7zOutArchive methods
    procedure AddStream(Stream: TStream; Ownership: TStreamOwnership;
      Attributes: Cardinal; CreationTime, LastWriteTime: TFileTime;
      const Path: UnicodeString; IsFolder, IsAnti: boolean); stdcall;
    procedure AddFile(const Filename: TFileName; const Path: UnicodeString); stdcall;
    procedure AddFiles(const Dir, Path, Wildcard: string; recurse: boolean); stdcall;
    procedure SaveToFile(const FileName: TFileName); stdcall;
    procedure SaveToStream(stream: TStream); stdcall;
    procedure SetProgressCallback(sender: Pointer; callback: T7zProgressCallback); stdcall;
    procedure ClearBatch; stdcall;
    procedure SetPassword(const password: UnicodeString); stdcall;
    procedure SetProperty(const name: UnicodeString; value: variant); stdcall;
    function OutArchive: IOutArchive; stdcall;
    // IProgress methods
    function SetTotal(total: Int64): HRESULT; stdcall;
    function SetCompleted(completeValue: PInt64): HRESULT; stdcall;
    // IArchiveUpdateCallback methods
    function GetUpdateItemInfo(index: cardinal;
      newData, newProperties: PInteger; indexInArchive: PCardinal): HRESULT; stdcall;
    function GetProperty(index: Cardinal; propID: TPropID; var value: variant): HRESULT; stdcall;
    function GetStream(index: Cardinal; var inStream: ISequentialInStream): HRESULT; stdcall;
    function SetOperationResult(operationResult: Integer): HRESULT; stdcall;
    // ICryptoGetTextPassword2 methods
    function CryptoGetTextPassword2(passwordIsDefined: PInteger; var password: TBStr): HRESULT; stdcall;
  public
    constructor Create(const lib: TFileName); override;
    destructor Destroy; override;
  end;


function DetectFormat(const FileName: TFileName): T7zFormatHandler;
begin
  result := fhUndefined; // TODO: implement DetectFormat()
end;

function New7ZInArchive(fmt: T7zFormatHandler; const lib: TFileName): I7ZInArchive;
begin

end;

function New7ZOutArchive(fmt: T7zFormatHandler; const lib: TFileName): I7ZOutArchive;
begin

end;


{ T7zPlugin }

constructor T7zPlugin.Create(const lib: TFileName);
begin
  FHandle := LibraryOpen(lib);
  if not ValidHandle(FHandle) then
    raise E7Zip.CreateFmt('Error loading library %s', [lib]);
  FCreateObject := LibraryResolve(FHandle, 'CreateObject');
  if not Assigned(FCreateObject) then
    raise E7Zip.CreateFmt('%s is not a 7z library', [lib]);
end;

destructor T7zPlugin.Destroy;
begin
  if fHandle <> 0 then
    LibraryClose(FHandle);
  inherited;
end;

procedure T7zPlugin.CreateObject(const clsid, iid: TGUID; var obj);
begin
  E7Zip.Check(self, 'CreateObject', FCreateObject(clsid, iid, obj));
end;


{ T7zCodec }

function VariantToGuid(const v: variant): TGuid;
begin
  with TVarData(v) do
    if VType = VT_CLSID then
      result := PGuid(VAny)^
    else
      FillZero(result);
end;

constructor T7zCodec.Create(const lib: TFileName);
begin
  inherited Create(lib);
  FGetMethodProperty := LibraryResolve(FHandle, 'GetMethodProperty');
  FGetNumberOfMethods := LibraryResolve(FHandle, 'GetNumberOfMethods');
  if not (Assigned(FGetMethodProperty) and
          Assigned(FGetNumberOfMethods)) then
    raise E7Zip.CreateFmt('%s is not a codec library', [lib]);
end;

function T7zCodec.GetDecoder(const index: integer): ICompressCoder;
var
  v: variant;
begin
  v := MethodProperty[index, kDecoder];
  CreateObject(VariantToGuid(v), ICompressCoder, result);
end;

function T7zCodec.GetEncoder(const index: integer): ICompressCoder;
var
  v: variant;
begin
  v := MethodProperty[index, kEncoder];
  CreateObject(VariantToGuid(v), ICompressCoder, result);
end;

function T7zCodec.GetMethodProperty(index: Cardinal;
  propID: T7zMethodPropID): variant;
begin
  E7Zip.Check(self, 'GetMethodProperty',
    FGetMethodProperty(index, propID, result));
end;

function T7zCodec.GetName(const index: integer): string;
begin
  result := MethodProperty[index, kName];
end;

function T7zCodec.GetNumberOfMethods: Cardinal;
begin
  E7Zip.Check(self, 'GetNumberOfMethods', FGetNumberOfMethods(@result));
end;


function T7zCodec.SetRatioInfo(inSize, outSize: PInt64): HRESULT;
begin
  result := S_OK;
end;


{ T7zInArchive }

procedure T7zInArchive.Close;
begin
  FPasswordIsDefined := false;
  FSubArchiveMode := false;
  FInArchive.Close;
  FInArchive := nil;
end;

constructor T7zInArchive.Create(const lib: TFileName);
begin
  inherited;
  FPasswordCallback := nil;
  FPasswordSender := nil;
  FPasswordIsDefined := false;
  FSubArchiveMode := false;
  FExtractCallBack := nil;
  FExtractSender := nil;
end;

destructor T7zInArchive.Destroy;
begin
  FInArchive := nil;
  inherited;
end;

function T7zInArchive.InArchive: IInArchive;
begin
  if FInArchive = nil then
    CreateObject(ClassID, IInArchive, FInArchive);
  result := FInArchive;
end;

function T7zInArchive.GetItemPath(const index: integer): UnicodeString; stdcall;
begin
  result := UnicodeString(GetItemProp(index, kpidPath));
end;

function T7zInArchive.GetItemModDate(const index: integer): TDateTime; //domasz
var
  Val: variant;
begin
  Val := GetItemProp(index, kpidLastWriteTime);
  if not VariantToDateTime(Val, result) then
    result := 0;
end;

function T7zInArchive.GetItemDate(const index: integer): TDateTime; //domasz
var
  Val: variant;
begin
  Val := GetItemProp(index, kpidCreationTime);
  if not VariantToDateTime(Val, result) then
    result := 0;
end;

function T7zInArchive.GetItemPackSize(const index: integer): Cardinal; //domasz
begin
  result := GetItemProp(index, kpidPackedSize);
end;

function T7zInArchive.GetItemComment(const index: integer): UnicodeString; //domasz
begin
  result := UnicodeString(GetItemProp(index, kpidComment));
end;

function T7zInArchive.GetItemCRC(const index: integer): AnsiString; //domasz
var CRC: Cardinal;
begin
  CRC := GetItemProp(index, kpidCRC);
  result := IntToHex(CRC, 8);
end;

function T7zInArchive.NumberOfItems: Cardinal; stdcall;
begin
  E7Zip.CheckOk(self, 'NumberOfItems', FInArchive.GetNumberOfItems(result));
end;

const
  MAXCHECK: Int64 = 1 shl 20;

procedure T7zInArchive.OpenFile(const filename: string); stdcall;
var
  strm: IInStream;
begin
  strm := T7zStream.Create(TFileStream.Create(filename, fmOpenRead or fmShareDenyNone), soOwned);
  try
    E7Zip.CheckOk(self, 'OpenFile',
      InArchive.Open(
        strm,
          @MAXCHECK, self as IArchiveOpenCallBack
        )
      );
  finally
    strm := nil;
  end;
end;

procedure T7zInArchive.OpenStream(stream: IInStream); stdcall;
begin
  E7Zip.CheckOk(self, 'OpenStream',
    InArchive.Open(stream, @MAXCHECK, self as IArchiveOpenCallBack));
end;

function T7zInArchive.GetItemIsFolder(const index: integer): boolean; stdcall;
begin
  result := Boolean(GetItemProp(index, kpidIsFolder));
end;

function T7zInArchive.GetItemProp(const Item: Cardinal;
  prop: TPropID): variant;
begin
  VarClear(result);
  FInArchive.GetProperty(Item, prop, result);
end;

procedure T7zInArchive.ExtractItem(const item: Cardinal; Stream: TStream;
  test: boolean); stdcall;
begin
  FStream := Stream;
  try
    E7Zip.CheckOk(self, 'ExtractItem',
      FInArchive.Extract(@item, 1, ord(test), self as IArchiveExtractCallback));
  finally
    FStream := nil;
  end;
end;

function T7zInArchive.GetStream(index: Cardinal;
  var outStream: ISequentialOutStream; askExtractMode: T7zExtractAskMode): HRESULT;
var
  path: string;
begin
  if askExtractMode = eamExtract then
    if FStream <> nil then
      outStream := T7zStream.Create(FStream, soReference) as ISequentialOutStream else
    if assigned(FExtractCallback) then
    begin
      result := FExtractCallBack(FExtractSender, index, outStream);
      Exit;
    end else
    if FExtractPath <> '' then
    begin
      if not GetItemIsFolder(index) then
      begin
        path := FExtractPath + GetItemPath(index);
        ForceDirectories(ExtractFilePath(path));
        outStream := T7zStream.Create(TFileStream.Create(path, fmCreate), soOwned);
      end;
    end;
  result := S_OK;
end;

function T7zInArchive.PrepareOperation(askExtractMode: T7zExtractAskMode): HRESULT;
begin
  result := S_OK;
end;

function T7zInArchive.SetCompleted(completeValue: PInt64): HRESULT;
begin
  if Assigned(FProgressCallback) and (completeValue <> nil) then
    result := FProgressCallback(FProgressSender, false, completeValue^) else
    result := S_OK;
end;

function T7zInArchive.SetCompleted(files, bytes: PInt64): HRESULT;
begin
  result := S_OK;
end;

function T7zInArchive.SetOperationResult(
  resultEOperationResult: T7zExtractOperationResult): HRESULT;
begin
  result := S_OK;
end;

function T7zInArchive.SetTotal(total: Int64): HRESULT;
begin
  if Assigned(FProgressCallback) then
    result := FProgressCallback(FProgressSender, true, total) else
    result := S_OK;
end;

function T7zInArchive.SetTotal(files, bytes: PInt64): HRESULT;
begin
  result := S_OK;
end;

function T7zInArchive.CryptoGetTextPassword(var password: TBStr): HRESULT;
var
  wpass: UnicodeString;
begin
  if FPasswordIsDefined then
  begin
    password := SysAllocString(PWideChar(FPassword));
    result := S_OK;
  end else
  if Assigned(FPasswordCallback) then
  begin
    result := FPasswordCallBack(FPasswordSender, wpass);
    if result = S_OK then
    begin
      password := SysAllocString(PWideChar(wpass));
      FPasswordIsDefined := True;
      FPassword := wpass;
    end;
  end else
    result := S_FALSE;
end;

function T7zInArchive.GetProperty(propID: TPropID;
  var value: variant): HRESULT;
begin
  result := S_OK;
end;

function T7zInArchive.GetStream(const name: PWideChar;
  var inStream: IInStream): HRESULT;
begin
  result := S_OK;
end;

procedure T7zInArchive.SetPasswordCallback(sender: Pointer;
  callback: T7zPasswordCallback); stdcall;
begin
  FPasswordSender := sender;
  FPasswordCallback := callback;
end;

function T7zInArchive.SetSubArchiveName(name: PWideChar): HRESULT;
begin
  FSubArchiveMode := true;
  FSubArchiveName := name;
  result := S_OK;
end;

function T7zInArchive.GetItemName(const index: integer): UnicodeString; stdcall;
begin
  result := UnicodeString(GetItemProp(index, kpidName));
end;

function T7zInArchive.GetItemSize(const index: integer): Cardinal; stdcall;
begin
  result := Cardinal(GetItemProp(index, kpidSize));
end;

procedure T7zInArchive.ExtractItems(items: PCardinalArray; count: cardinal;
  test: boolean; sender: pointer; callback: T7zGetStreamCallBack); stdcall;
begin
  FExtractCallBack := callback;
  FExtractSender := sender;
  try
    E7Zip.CheckOk(self, 'ExtractItems',
      FInArchive.Extract(items, count, ord(test), self as IArchiveExtractCallback));
  finally
    FExtractCallBack := nil;
    FExtractSender := nil;
  end;
end;

procedure T7zInArchive.SetProgressCallback(sender: Pointer;
  callback: T7zProgressCallback); stdcall;
begin
  FProgressSender := sender;
  FProgressCallback := callback;
end;

procedure T7zInArchive.ExtractAll(test: boolean; sender: pointer;
  callback: T7zGetStreamCallBack);
begin
  FExtractCallBack := callback;
  FExtractSender := sender;
  try
    E7Zip.CheckOk(self, 'ExtractAll',
      FInArchive.Extract(nil, $FFFFFFFF, ord(test), self as IArchiveExtractCallback));
  finally
    FExtractCallBack := nil;
    FExtractSender := nil;
  end;
end;

procedure T7zInArchive.ExtractTo(const path: string);
begin
  FExtractPath := IncludeTrailingPathDelimiter(path);
  try
    E7Zip.CheckOk(self, 'ExtractTo',
      FInArchive.Extract(nil, $FFFFFFFF, 0, self as IArchiveExtractCallback));
  finally
    FExtractPath := '';
  end;
end;

procedure T7zInArchive.SetPassword(const password: UnicodeString);
begin
  FPassword := password;
  FPasswordIsDefined :=  FPassword <> '';
end;


{ T7zArchive }

constructor T7zArchive.Create(const lib: TFileName);
begin
  inherited;
  FGetHandlerProperty := LibraryResolve(FHandle, 'GetHandlerProperty');
  if not Assigned(FGetHandlerProperty) then
    raise E7Zip.CreateFmt('%s is not a Format library', [lib]);
end;

function T7zArchive.GetClassId: TGUID;
begin
  result := FClassId;
end;

function T7zArchive.GetHandlerProperty(const propID: T7zHandlerPropID): variant;
begin
  E7Zip.Check(self, 'GetHandlerProperty',
    FGetHandlerProperty(propID, result));
end;

function T7zArchive.GetLibGUIDProperty(const Index: T7zHandlerPropID): TGUID;
begin
  result := VariantToGuid(HandlerProperty[index]);
end;

function T7zArchive.GetLibStringProperty(const Index: T7zHandlerPropID): string;
begin
  result := HandlerProperty[Index];
end;

procedure T7zArchive.SetClassId(const classid: TGUID);
begin
  FClassId := classid;
end;


{ T7zStream }

constructor T7zStream.Create(Stream: TStream; Ownership: TStreamOwnership);
begin
  inherited Create;
  FStream := Stream;
  FOwnership := Ownership;
end;

destructor T7zStream.destroy;
begin
  if FOwnership = soOwned then
  begin
    FStream.Free;
    FStream := nil;
  end;
  inherited;
end;

function T7zStream.Flush: HRESULT;
begin
  result := S_OK;
end;

function T7zStream.GetSize(size: PInt64): HRESULT;
begin
  if size <> nil then
    size^ := FStream.Size;
  result := S_OK;
end;

function T7zStream.Read(data: Pointer; size: Cardinal;
  processedSize: PCardinal): HRESULT;
var
  len: integer;
begin
  len := FStream.Read(data^, size);
  if processedSize <> nil then
    processedSize^ := len;
  result := S_OK;
end;

function T7zStream.Seek(offset: Int64; seekOrigin: Cardinal;
  newPosition: PInt64): HRESULT;
begin
  FStream.Seek(offset, TSeekOrigin(seekOrigin));
  if newPosition <> nil then
    newPosition^ := FStream.Position;
  result := S_OK;
end;

function T7zStream.SetSize(newSize: Int64): HRESULT;
begin
  FStream.Size := newSize;
  result := S_OK;
end;

function T7zStream.Write(data: Pointer; size: Cardinal;
  processedSize: PCardinal): HRESULT;
var
  len: integer;
begin
  len := FStream.Write(data^, size);
  if processedSize <> nil then
    processedSize^ := len;
  result := S_OK;
end;

destructor T7zBatchItem.Destroy;
begin
  if (Ownership = soOwned) and (Stream <> nil) then
    Stream.Free;
  inherited;
end;


{ T7zOutArchive }

function GetFileTime(hFile: THandle;
  lpCreationTime, lpLastAccessTime, lpLastWriteTime: PFileTime): LongBool;
    external 'kernel32' name 'GetFileTime';
function GetFileAttributes(lpFileName: PChar): cardinal;
    external 'kernel32' name 'GetFileAttributes' + _AW;

procedure T7zOutArchive.AddFile(const Filename: TFileName; const Path: UnicodeString);
var
  item: T7zBatchItem;
  Handle: THandle;
begin
  if not FileExists(Filename) then exit;
  item := T7zBatchItem.Create;
  Item.SourceMode := smFile;
  item.Stream := nil;
  item.FileName := Filename;
  item.Path := Path;
  Handle := FileOpen(Filename, fmOpenRead or fmShareDenyNone);
  GetFileTime(Handle, @item.CreationTime, nil, @item.LastWriteTime);
  item.Size := FileSize(Handle);
  CloseHandle(Handle);
  item.Attributes := GetFileAttributes(PChar(Filename));
  item.IsFolder := false;
  item.IsAnti := False;
  item.Ownership := soOwned;
  ObjArrayAdd(FBatchList, item);
end;

procedure T7zOutArchive.AddFiles(const Dir, Path, Wildcard: string; recurse: boolean);
var
  lencut: integer;
  willlist: TStringList;
  zedir: string;
  procedure Traverse(p: string);
  var
    f: TSearchRec;
    i: integer;
    item: T7zBatchItem;
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

    for i := 0 to willlist.Count - 1 do
    begin
      if FindFirst(p + willlist[i], faReadOnly or faHidden or faSysFile or faArchive, f) = 0 then
      repeat
        item := T7zBatchItem.Create;
        Item.SourceMode := smFile;
        item.Stream := nil;
        item.FileName := p + f.Name;
        item.Path := copy(item.FileName, lencut, length(item.FileName) - lencut + 1);
        if path <> '' then
          item.Path := IncludeTrailingPathDelimiter(path) + item.Path;
        item.CreationTime := f.FindData.ftCreationTime;
        item.LastWriteTime := f.FindData.ftLastWriteTime;
        item.Attributes := f.FindData.dwFileAttributes;
        item.Size := f.Size;
        item.IsFolder := false;
        item.IsAnti := False;
        item.Ownership := soOwned;
        ObjArrayAdd(FBatchList, item);
      until FindNext(f) <> 0;
      SysUtils.FindClose(f);
    end;
  end;
begin
  willlist := TStringList.Create;
  try
    willlist.Delimiter := ';';
    willlist.DelimitedText := Wildcard;
    zedir := IncludeTrailingPathDelimiter(Dir);
    lencut := Length(zedir) + 1;
    Traverse(zedir);
  finally
    willlist.Free;
  end;
end;

procedure T7zOutArchive.AddStream(Stream: TStream; Ownership: TStreamOwnership;
  Attributes: Cardinal; CreationTime, LastWriteTime: TFileTime;
  const Path: UnicodeString; IsFolder, IsAnti: boolean); stdcall;
var
  item: T7zBatchItem;
begin
  item := T7zBatchItem.Create;
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
  ObjArrayAdd(FBatchList, item);
end;

procedure T7zOutArchive.ClearBatch;
begin
  ObjArrayClear(FBatchList);
end;

constructor T7zOutArchive.Create(const lib: TFileName);
begin
  inherited Create(lib);
  FProgressCallback := nil;
  FProgressSender := nil;
end;

function T7zOutArchive.CryptoGetTextPassword2(passwordIsDefined: PInteger;
  var password: TBStr): HRESULT;
begin
  if FPassword <> '' then
  begin
   passwordIsDefined^ := 1;
   password := SysAllocString(PWideChar(FPassword));
  end else
    passwordIsDefined^ := 0;
  result := S_OK;
end;

destructor T7zOutArchive.Destroy;
begin
  FOutArchive := nil;
  ClearBatch;
  inherited;
end;

function T7zOutArchive.OutArchive: IOutArchive;
begin
  if FOutArchive = nil then
    CreateObject(ClassID, IOutArchive, FOutArchive);
  result := FOutArchive;
end;

function T7zOutArchive.GetProperty(index: Cardinal; propID: PROPID;
  var value: variant): HRESULT;
var
  item: T7zBatchItem;
begin
  VarClear(value);
  item := T7zBatchItem(FBatchList[index]);
  with TVarData(Value) do
  case propID of
    kpidAttributes:
      begin
        VType := VT_UI4;
        VLongWord := item.Attributes;
      end;
    kpidLastWriteTime:
      begin
        VType := VT_FILETIME;
        VInt64 := Int64(item.LastWriteTime);
      end;
    kpidPath:
      begin
        if item.Path <> '' then
          value := item.Path;
      end;
    kpidIsFolder: Value := item.IsFolder;
    kpidSize:
      begin
        VType := VT_UI8;
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

function T7zOutArchive.GetStream(index: Cardinal;
  var inStream: ISequentialInStream): HRESULT;
var
  item: T7zBatchItem;
begin
  item := T7zBatchItem(FBatchList[index]);
  case item.SourceMode of
    smFile: inStream := T7zStream.Create(TFileStream.Create(item.FileName, fmOpenRead or fmShareDenyNone), soOwned);
    smStream:
      begin
        item.Stream.Seek(0, soFromBeginning);
        inStream := T7zStream.Create(item.Stream);
      end;
  end;
  result := S_OK;
end;

function T7zOutArchive.GetUpdateItemInfo(index: Cardinal; newData,
  newProperties: PInteger; indexInArchive: PCardinal): HRESULT;
begin
  newData^ := 1;
  newProperties^ := 1;
  indexInArchive^ := CArdinal(-1);
  result := S_OK;
end;

procedure T7zOutArchive.SaveToFile(const FileName: TFileName);
var
  f: TFileStream;
begin
  f := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(f);
  finally
    f.free;
  end;
end;

procedure T7zOutArchive.SaveToStream(stream: TStream);
var
  strm: ISequentialOutStream;
begin
  strm := T7zStream.Create(stream);
  try
    E7Zip.CheckOk(self, 'SaveToStream',
      OutArchive.UpdateItems(strm, length(FBatchList), self as IArchiveUpdateCallback));
  finally
    strm := nil;
  end;
end;

function T7zOutArchive.SetCompleted(completeValue: PInt64): HRESULT;
begin
  if Assigned(FProgressCallback) and (completeValue <> nil) then
    result := FProgressCallback(FProgressSender, false, completeValue^) else
    result := S_OK;
end;

function T7zOutArchive.SetOperationResult(
  operationResult: Integer): HRESULT;
begin
  result := S_OK;
end;

procedure T7zOutArchive.SetPassword(const password: UnicodeString);
begin
  FPassword := password;
end;

procedure T7zOutArchive.SetProgressCallback(sender: Pointer;
  callback: T7zProgressCallback);
begin
  FProgressCallback := callback;
  FProgressSender := sender;
end;

procedure T7zOutArchive.SetProperty(const name: UnicodeString;
  value: variant);
var
  intf: ISetProperties;
  p: PWideChar;
begin
  intf := OutArchive as ISetProperties;
  p := PWideChar(name);
  E7Zip.CheckOk(self, 'SetProperty',
    intf.SetProperties(@p, @TPropVariant(value), 1));
end;

function T7zOutArchive.SetTotal(total: Int64): HRESULT;
begin
  if Assigned(FProgressCallback) then
    result := FProgressCallback(FProgressSender, true, total)
  else
    result := S_OK;
end;


initialization
  assert(HANDLER_GUID.D4[5] = $ff);
  assert(GUIDToString(HandlerGuid(fhGZip)) = '{23170F69-40C1-278A-1000-000110EF0000}');

end.

