/// access to the 7-Zip library on Windows
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.lib.win7zip;

{
  *****************************************************************************

   Access to the 7-Zip Compression/Decompression DLL on Windows 
   - Low-Level 7-Zip API Process
   - T7Zip High-Level Class

  *****************************************************************************

}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,      // for TSynLibrary
  mormot.core.unicode,
  mormot.core.buffers; // for TAlgoCompress

{ ****************** Low-Level 7-Zip API Process }

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

  /// the supported archive formats
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


{ ****************** T7Zip High-Level Class }




implementation


{ ****************** Low-Level 7-Zip API Process }

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



{ ****************** T7Zip High-Level Class }


initialization
  assert(HANDLER_GUID.D4[5] = $ff);
  assert(GUIDToString(HandlerGuid(fhGZip)) = '{23170F69-40C1-278A-1000-000110EF0000}');

end.

