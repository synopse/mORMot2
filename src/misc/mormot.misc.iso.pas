/// ISO 9660 CD/DVD File System Reader
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.misc.iso;

{
  *****************************************************************************

   ISO 9660 File System Reader, as used for optical disc media
    - Low-Level ISO 9660 Encoding Structures
    - Low-Level UDF Encoding Structures
    - High-Level .iso File Reader

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os;
  

{ ************ Low-Level ISO 9660 Encoding Structures }

type
  /// Date/Time encoded as 7 bytes
  TVolumeDateTime = packed record
    /// Number of years since 1900
    Year: byte;
    /// Month of the year from 1 to 12
    Month: byte;
    /// Day of the Month from 1 to 31
    Day: byte;
    /// Hour of the day from 0 to 23
    Hour: byte;
    /// Minute of the hour from 0 to 59
    Minute: byte;
    /// second of the minute from 0 to 59
    Second: byte;
    /// Offset from Greenwich Mean Time
    // - number of 15 minute intervals from -48(West) to +52(East)
    Zone: shortint;
  end;
  PVolumeDateTime = ^TVolumeDateTime;

  TFileFlag = (
    ffHidden,
    ffDirectory,
    ffAssociated,
    ffInformationAvailable,
    ffOwnerAvailable,
    ffReserved5,
    ffReserved6,
    ffMoreThanOneDirectoryRecord);
  TFileFlags = set of TFileFlag;

  /// defines one directory/folder record
  TDirectoryRecord = packed record
    /// size of this Directory entry, in bytes
    LengthOfDirectoryRecord: byte;
    /// optional Extended Attribute Record size, in bytes
    ExtendedAttributeRecordLength: byte;
    /// logical extended block number, in bytes
    LocationOfExtent: int64;
    /// file section length, in bytes
    DataLength: int64;
    /// when the file was stored
    RecordingDateAndTime: TVolumeDateTime;
    /// define some file attriutes
    FileFlags: TFileFlags;
    /// is usually 0 - unless interleave mode is used
    FileUnitSize: byte;
    /// is usually 0 - unless interleave mode is used
    InterleaveGapSize: byte;
    /// ordinal number of the volume in the Volume
    VolumeSequenceNumber: integer;
    /// file identifier used size, in bytes
    LengthOfFileIdentifier: byte;
    /// file identifier content
    FileIdentifier: array[0..0] of byte;
  end;
  PDirectoryRecord = ^TDirectoryRecord;

  /// define one entry in the Path Table
  TPathTableRecord = packed record
    /// size of this Directory Identifier entry, in bytes
    LengthOfDirectoryIdentifier: byte;
    /// optional Extended Attribute Record size, in bytes
    ExtendedAttributeRecordLength: byte;
    /// logical extended block number, in bytes
    LocationOfExtent: cardinal;
    /// record number of the parent directory of this entry
    ParentDirectoryNumber: word;
    /// directory identifier content
    DirectoryIdentifier: array[0..0] of byte;
  end;
  PPathTableRecord = ^TPathTableRecord;

  TXBoxFileFlag = (
    xfReadOnly,
    xfHidden,
    xfSystem,
    xfReserved4,
    xfDirectory,
    xfArchive,
    xfReserved7,
    xfNormal);
  TXBoxFileFlags = set of TXBoxFileFlag;

  /// directory definition in XBOX format
  TXBoxDirectoryRecord = packed record
    /// offset to the left sub-tree entry
    LeftSubTree: smallint;
    /// offset to the right sub-tree entry
    RightSubTree: smallint;
    /// starting sector of the file
    LocationOfExtent: cardinal;
    /// total file size
    DataLength: cardinal;
    /// define some XBOX file attriutes
    FileFlags: TXBoxFileFlags;
    /// file identifier used size, in bytes
    LengthOfFileIdentifier: byte;
    /// file identifier content
    FileIdentifier: array[0..0] of byte;
  end;
  PXBoxDirectoryRecord = ^TXBoxDirectoryRecord;

  TMagicID  = array[0..4] of AnsiChar;
  TXBoxID   = array[0..19] of AnsiChar;
  TToritoID = array[0..31] of AnsiChar;

  TAscDateTime   = array[0..16] of AnsiChar;
  TIdentifier8   = array[0..7] of AnsiChar;
  TIdentifier23  = array[0..22] of AnsiChar;
  TIdentifier24  = array[0..23] of AnsiChar;
  TIdentifier28  = array[0..27] of AnsiChar;
  TIdentifier32  = array[0..31] of AnsiChar;
  TIdentifier36  = array[0..35] of AnsiChar;
  TIdentifier37  = array[0..36] of AnsiChar;
  TIdentifier62  = array[0..61] of AnsiChar;
  TIdentifier128 = array[0..127] of AnsiChar;

const
  CDSIGNATURE: TMagicID = 'CD001';
  UDFBEA:      TMagicID = 'BEA01';
  UDFNSR02:    TMagicID = 'NSR02';
  UDFNSR03:    TMagicID = 'NSR03';
  UDFTEA:      TMagicID = 'TEA01';
  XBOXSIGNATURE: TXBoxID = 'MICROSOFT*XBOX*MEDIA';
  TORITO: TToritoID = 'EL TORITO SPECIFICATION'; // padded with zeros

type
  /// defines a Volume
  TPrimaryVolumeDescriptor = packed record
    VolumeDescriptorType: byte;
    StandardIdentifier: TMagicID;
    VolumeDescriptorVersion: byte;
    Ununsed: byte;
    SystemIdentifier: TIdentifier32;
    VolumeIdentifier: TIdentifier32;
    Unused2: array[0..7] of byte;
    /// number of logical blocks in the Volume
    VolumeSpaceSize: int64;
    Unused3: array[0..31] of byte;
    /// assigned Volum Set size
    VolumeSetSize: cardinal;
    /// ordinal number in the Volume Set
    VolumeSequenceNumber: cardinal;
    /// logical block size, in bytes
    LogicalBlockSize: cardinal;
    /// length of the Path Table, in bytes
    PathTableSize: int64;
    LocationOfTypeLPathTable: cardinal;
    LocationOfOptionalTypeLPathTable: cardinal;
    LocationOfTypeMPathTable: cardinal;
    LocationOfOptionalTypeMPathTable: cardinal;
    /// actual directory record the for root folder of this volume
    DirectoryRecordForRootDirectory: TDirectoryRecord;
    VolumeSetIdentifier: TIdentifier128;
    PublisherIdentifier: TIdentifier128;
    DataPreparerIdentifier: TIdentifier128;
    ApplicationIdentifier: TIdentifier128;
    CopyrightFileIdentifier: TIdentifier37;
    AbstractFileIdentifier: TIdentifier37;
    BibliographicFileIdentifier: TIdentifier37;
    VolumeCreationDateAndTime: TAscDateTime;
    VolumeModificationDateAndTime: TAscDateTime;
    VolumeExpirationDateAndTime: TAscDateTime;
    VolumeEffectiveDateAndTime: TAscDateTime;
    FileStructureVersion: byte;
    ReservedForFutureStandardization: byte;
    ApplicationUse: array[0..511] of byte;
    ReservedForFutureStandardization2: array[0..652] of byte;
  end;
  PPrimaryVolumeDescriptor = ^TPrimaryVolumeDescriptor;

  TXBoxPrimaryVolumeDescriptor = packed record
    /// should contain XBOXSIGNATURE constant
    StandardIdentifier: TXBoxID;
    LocationOfExtent: cardinal;
    DataLength: cardinal;
    /// Windows FILETIME 64-bit structure
    FileTime: QWord;
    Unused: array[0..1991] of byte;
    /// should contain XBOXSIGNATURE constant
    StandardIdentifier2: TXBoxID;
  end;
  PXBoxPrimaryVolumeDescriptor = ^TXBoxPrimaryVolumeDescriptor;
  TXBoxVolumeDescriptor = TXBoxPrimaryVolumeDescriptor;

  /// Volume with El Torito extension
  TBootRecordVolumeDescriptor = packed record
    /// must be 0
    BootRecordIndicator: byte;
    /// must equal CDSIGNATURE
    StandardIdentifier: TMagicID;
    /// must be 1
    VersionOfDescriptor: byte;
    /// must equal TORITO with zero padding
    BootSystemIdentifier: TToritoID;
    Unused: array[0..31] of byte;
    /// absolute position to the Boot Catalog first sector
    BootCatalogPointer: cardinal;
    Unused2: array[0..1972] of byte;
  end;
  PBootRecordVolumeDescriptor = ^TBootRecordVolumeDescriptor;

  TPlatformID = (
    pi80x86,
    piPowerPC,
    piMac);

  TValidationEntry = packed record
    HeaderID: byte;
    PlatformID: TPlatformID;
    Reserved: word;
    /// manufacturer of the CD-ROM
    ID: TIdentifier24;
    Checksum: word;
    /// must be AA55
    KeyWord: word;
  end;
  PValidationEntry = ^TValidationEntry;

  TBootMediaType = (
    bmtNone,
    bmt12M,
    bmt144M,
    bmt288M,
    bmtHardDisk);

  /// define the initial entry of this media
  TInitialEntry = packed record
    /// Boot Indicator: 88 = Bootable, 00 = Not Bootable
    Bootable: byte;
    BootMediaType: TBootMediaType;
    /// load segment
    // - if 0 the system will use the traditional segment of 7C0
    LoadSegment: word;
    /// copy of byte 5 (System Type) from the Partition Table
    SystemType: byte;
    Reserved: byte;
    /// number of virtual/emulated sectors loaded at boot
    SectorCount: word;
    /// start address of the virtual disk
    LoadRBA: cardinal;
    Reserved2: array[0..19] of byte;
  end;
  PInitialEntry = ^TInitialEntry;

  TSectionHeaderEntry = packed record
    /// flag 90 = more headers follow, 91 = final header
    HeaderIndicator: byte;
    PlatformID: TPlatformID;
    NumberOfSectionEntries: word;
    /// BIOS will check this identifier at startup
    ID: TIdentifier28;
  end;
  PSectionHeaderEntry = ^TSectionHeaderEntry;

  TSectionEntry = packed record
    /// Boot Indicator: 88 = Bootable, 00 = Not Bootable
    BootIndicator: byte;
    /// low 0-3 bits = TBootMediaType
    BootMediaType: byte;
    LoadSegment: word;
    SystemType: byte;
    Reserved: byte;
    SectorCount: word;
    LoadRBA: cardinal;
    SelectionCriteriaType: byte;
    SelectionCriteria: array[0..18] of byte;
  end;

  TSectionEntryExtension = packed record
    /// must be 44
    ExtensionIndicator: byte;
    /// bit 5 set = Extension Record follows, unset: this is final Extension
    Bits: byte;
    /// Vendor unique selection criteria
    SelectionCriteria: array[0..29] of byte;
  end;

  TCatalogEntry = packed record
  case integer of
    0: (
      Initial: TInitialEntry );
    1: (
      Header: TSectionHeaderEntry );
    2: (
      Entry: TSectionEntry );
    3: (
      Extension: TSectionEntryExtension );
  end;
  PCatalogEntry = ^TCatalogEntry;

  TBootCatalog = packed record
    Validation: TValidationEntry;
    Entry: array[0..1] of TCatalogEntry;
  end;
  PBootCatalog = ^TBootCatalog;

  TPartition = packed record
    /// 0x80 - active
    boot_ind: byte;
    /// starting head
    head: byte;
    /// starting sector
    sector: byte;
    /// starting cylinder
    cyl: byte;
    /// What partition type
    sys_ind: byte;
    /// end head
    end_head: byte;
    /// end sector
    end_sector: byte;
    /// end cylinder
    end_cyl: byte;
    /// starting sector counting from 0
    start_sect: cardinal;
    /// nr of sectors in partition
    nr_sects: cardinal;
  end;
  PPartition = ^TPartition;

 /// map the Master Boot Loader on this Media
 TMBR = packed record
    Loader: array[0..445] of byte;
    Partition: array[0..3] of TPartition;
    Signature: word;
  end;
  PMBT = ^TMBR;


{ ************ Low-Level UDF Encoding Structures }

type
  TTag = packed record
    TagIdentifier: word;
    DescriptionVersion: word;
    TagChecksum: byte;
    Reserved: byte;
    TagSerialNumber: word;
    DescriptorCRC: word;
    DescriptorCRCLength: word;
    TagLocation: cardinal;
  end;

  TCharSpec = packed record
    CharacterSetType: byte;
    /// should be "OSTA Compressed Unicode"
    SharacterSetInfo: array[0..62] of byte;
  end;

  /// ECMA 167 1/7.3
  TTimestamp = packed record
    TypeAndTimezone: word;
    Year: smallint;
    Month: byte;
    Day: byte;
    Hour: byte;
    Minute: byte;
    Second: byte;
    Centiseconds: byte;
    HundredsofMicroseconds: byte;
    Microseconds: byte;
  end;

  /// ECMA 167 1/7.4
  TEntityID = packed record
    Flags: byte;
    Identifier: TIdentifier23;
    IdentifierSuffix: TIdentifier8;
  end;

  TExtentAd = packed record
    extLength: cardinal;
    extLocation: cardinal;
  end;

  TShortAd = packed record
    extLength: cardinal;
    extPosition: cardinal;
  end;

  TLbAddr = packed record
    logicalBlockNum: cardinal;
    partitionReferenceNum: word;
  end;

  TLongAd = packed record
    extLength: cardinal;
    extLocation: TLbAddr;
    impUse: array[0..5] of byte;
  end;

  TLVInformation = packed record
    LVICharset: TCharSpec;
    LogicalVolumeIdentifier: TIdentifier128;
    LVInfo1: TIdentifier36;
    LVInfo2: TIdentifier36;
    LVInfo3: TIdentifier36;
    ImplementationID: TEntityID;
    ImplementationUse: array[0..127] of byte;
  end;

  TImplementationUse = packed record
    ImplementationID: TEntityID;
    NumberOfFiles: cardinal;
    NumberOfDirectories: cardinal;
    MinimumUDFReadRevision: word;
    MinimumUDFWriteRevision: word;
    MaximumUDFWriteRevision: word;
  end;

  /// ECMA 167 3/10.4
  TImpUseVolumeDescriptor = packed record
    DescriptorTag: TTag;
    VolumeDescriptorSequenceNumber: cardinal;
    ImplementationIdentifier: TEntityID;
    case integer of
      0: (
        reserved: array[0..459] of byte);
      1: (
        Use: TImplementationUse);
  end;

  TPrimaryVolumeDescriptor_UDF = packed record
    DescriptorTag: TTag;
    VolumeDescriptorSequenceNumber: cardinal;
    PrimaryVolumeDescriptorNumber: cardinal;
    VolumeIdentifier: TIdentifier32;
    VolumeSequenceNumber: word;
    MaximumVolumeSequenceNumber: word;
    InterchangeLevel: word;
    MaximumInterchangeLevel: word;
    CharacterSetList: cardinal;
    MaximumCharacterSetList: cardinal;
    VolumeSetIdentifier: TIdentifier128;
    DescriptorCharacterSet: TCharSpec;
    ExplanatoryCharacterSet: TCharSpec;
    VolumeAbstract: TExtentAd;
    VolumeCopyrightNotice: TExtentAd;
    ApplicationIdentifier: TEntityID;
    RecordingDateandTime: TTimestamp;
    ImplementationIdentifier: TEntityID;
    ImplementationUse: array[0..63] of byte;
    PredecessorVolumeDescriptorSequenceLocation: cardinal;
    Flags: word;
    Reserved: array[0..21] of byte;
  end;
  PPrimaryVolumeDescriptor_UDF = ^TPrimaryVolumeDescriptor_UDF;

  /// Generic Partition Map (ECMA 167r3 3/10.7.1)
  TPartitionMap = packed record
    MapType: byte;
    MapLength: byte;
    Mapping: array[0..1] of byte;
  end;

  /// Type 1 Partition Map (ECMA 167r3 3/10.7.2)
  TPartitionMap1 = packed record
    MapType: byte;
    MapLength: byte;
    VolumeSequenceNumber: word;
    PartitionNumber: word;
  end;

  /// Type 2 Partition Map (ECMA 167r3 3/10.7.3)
  TPartitionMap2 = packed record
    MapType: byte;
    MapLength: byte;
    Ident: TIdentifier62;
  end;

  /// ECMA 167 3/10.6
  TLogicalVolumeDescriptor = packed record
    DescriptorTag: TTag;
    VolumeDescriptorSequenceNumber: cardinal;
    DescriptorCharacterSet: TCharSpec;
    LogicalVolumeIdentifier: TIdentifier128;
    LogicalBlockSize: cardinal;
    DomainIdentifier: TEntityID;
    LogicalVolumeContentsUse: TLongAd;
    MapTableLength: cardinal;
    NumberofPartitionMaps: cardinal;
    ImplementationIdentifier: TEntityID;
    ImplementationUse: array[0..127] of byte;
    IntegritySequenceExtent: TExtentAd;
    PartitionMaps: TPartitionMap;
  end;
  PLogicalVolumeDescriptor = ^TLogicalVolumeDescriptor;

  /// ECMA 167 4/14.1
  TFileSetDescriptor = packed record
    DescriptorTag: TTag;
    RecordingDateandTime: TTimestamp;
    InterchangeLevel: word;
    MaximumInterchangeLevel: word;
    CharacterSetList: cardinal;
    MaximumCharacterSetList: cardinal;
    FileSetNumber: cardinal;
    FileSetDescriptorNumber: cardinal;
    LogicalVolumeIdentifierCharacterSet: TCharSpec;
    LogicalVolumeIdentifier: TIdentifier128;
    FileSetCharacterSet: TCharSpec;
    FileSetIdentifier: TIdentifier32;
    CopyrightFileIdentifier: TIdentifier32;
    AbstractFileIdentifier: TIdentifier32;
    RootDirectoryICB: TLongAd;
    DomainIdentifier: TEntityID;
    NextExtent: TLongAd;
    SystemStreamDirectoryICB: TLongAd;
    Reserved: array[0..31] of byte;
  end;

  /// ECMA 167 3/10.2
  /// - this structure must be located at sector 256
  TAnchorVolumeDescriptor = packed record
    DescriptorTag: TTag;
    MainVolumeDescriptorSequenceExtent: TExtentAd;
    ReserveVolumeDescriptorSequenceExtent: TExtentAd;
    Reserved: array[0..479] of byte;
  end;

  /// ECMA 167 4/14.3
  TPartitionHeaderDescriptor = packed record
    UnallocatedSpaceTable: TShortAd;
    UnallocatedSpaceBitmap: TShortAd;
    PartitionIntegrityTable: TShortAd;
    FreedSpaceTable: TShortAd;
    FreedSpaceBitmap: TShortAd;
    Reserved: array[0..87] of byte;
  end;

  /// ECMA 167 3/10.5
  TPartitionDescriptor = packed record
    DescriptorTag: TTag;
    VolumeDescriptorSequenceNumber: cardinal;
    PartitionFlags: word;
    PartitionNumber: word;
    PartitionContents: TEntityID;
    PartitionContentsUse: TPartitionHeaderDescriptor;
    AccessType: cardinal;
    PartitionStartingLocation: cardinal;
    PartitionLength: cardinal;
    ImplementationIdentifier: TEntityID;
    ImplementationUse: array[0..127] of byte;
    Reserved: array[0..155] of byte;
  end;
  PPartitionDescriptor = ^TPartitionDescriptor;

  /// ECMA 167 3/10.10
  TLogicalVolumeIntegrityDesc = packed record
    DescriptorTag: TTag;
    RecordingDateAndTime: TTimestamp;
    IntegrityType: cardinal;
    NextIntegrityExtent: TExtentAd;
    LogicalVolumeContentsUse: array[0..31] of byte;
    NumberOfPartitions: cardinal;
    LengthOfImplementationUse: cardinal;
    FreeSpaceTable: array[0..0] of cardinal;
    SizeTable: array[0..0] of cardinal;
    ImplementationUse: array[0..0] of byte;
  end;

  /// ECMA 167 3/10.8
  TUnallocatedSpaceDesc = packed record
    DescriptorTag: TTag;
    VolumeDescriptorSequenceNumber: cardinal;
    NumberofAllocationDescriptors: cardinal;
    AllocationDescriptors: array[0..0] of TExtentAd;
  end;

  /// ECMA 167 4/14.6
  TIcbTag = packed record
    PriorRecordedNumberofDirectEntries: cardinal;
    StrategyType: word;
    StrategyParameter: array[0..1] of byte;
    MaximumNumberofEntries: word;
    Reserved: byte;
    FileType: byte;
    ParentICBLocation: TLbAddr;
    Flags: word;
  end;

  /// ECMA 167 4/14.9
  TFileEntry = packed record
    DescriptorTag: TTag;
    ICBTag: TIcbTag;
    Uid: cardinal;
    Gid: cardinal;
    Permissions: cardinal;
    FileLinkCount: word;
    RecordFormat: byte;
    RecordDisplayAttributes: byte;
    RecordLength: cardinal;
    InformationLength: QWord;
    LogicalBlocksRecorded: QWord;
    AccessTime: TTimeStamp;
    ModificationTime: TTimeStamp;
    AttributeTime: TTimeStamp;
    Checkpoint: cardinal;
    ExtendedAttributeICB: TLongAd;
    ImplementationIdentifier: TEntityID;
    UniqueID: QWord;
    LengthOfExtendedAttributes: cardinal;
    LengthOfAllocationDescriptors: cardinal;
    case integer of
      0: (
        ExtendedAttributes: array[0..0] of byte);
      1: (
        AllocationDescriptor: array[0..0] of byte);
  end;

  /// ECMA 167 4/14.4
  TFileIdentifierDescriptor = packed record
    DescriptorTag: TTag;
    FileVersionNumber: word;
    FileCharacteristics: byte;
    LengthOfFileIdentifier: byte;
    ICB: TLongAd;
    LengthOfImplementationUse: word;
    case integer of
      0: (
        ImplementationUse: array[0..0] of byte);
      1: (
        FileIdentifier: array[0..0] of AnsiChar);
  end;

  TFileSetDescriptorEx = packed record
    LogicalVolumeDescriptor: PLogicalVolumeDescriptor;
    FileSetDescriptor: TFileSetDescriptor;
  end;
  PFileSetDescriptorEx = ^TFileSetDescriptorEx;


const
  /// Tag Identifier (ECMA 167r3 3/7.2.1)
  TAG_IDENT_PVD  = $0001;
  TAG_IDENT_AVDP = $0002;
  TAG_IDENT_VDP  = $0003;
  TAG_IDENT_IUVD = $0004;
  TAG_IDENT_PD   = $0005;
  TAG_IDENT_LVD  = $0006;
  TAG_IDENT_USD  = $0007;
  TAG_IDENT_TD   = $0008;
  TAG_IDENT_LVID = $0009;

  /// Tag Identifier (ECMA 167r3 4/7.2.1)
  TAG_IDENT_FSD  = $0100;
  TAG_IDENT_FID  = $0101;
  TAG_IDENT_AED  = $0102;
  TAG_IDENT_IE   = $0103;
  TAG_IDENT_TE   = $0104;
  TAG_IDENT_FE   = $0105;
  TAG_IDENT_EAHD = $0106;
  TAG_IDENT_USE  = $0107;
  TAG_IDENT_SBD  = $0108;
  TAG_IDENT_PIE  = $0109;
  TAG_IDENT_EFE  = $010A;

  FATTR_HIDDEN    = 1 shl ord(ffHidden);
  FATTR_DIRECTORY = 1 shl ord(ffDirectory);


{ ************ High-Level .iso File Reader }

type
  TUdfDetectedStage = (
    no_udf,
    udf_bea,
    udf_nsr,
    udf_tea);

  TIsoImage = packed record
    hFile: THandle;
    //VolumeDescriptors: array of TPrimaryVolumeDescriptorEx;
    DescriptorNum: cardinal;
    DataOffset: cardinal;
    HeaderSize: cardinal;
    RealBlockSize: cardinal;
    //DirectoryList: array of TDirectory;
    DirectoryCount: cardinal;
    Index: cardinal;
    UdfStage: TUdfDetectedStage;
    UdfOffset: cardinal;
    UdfBlockSize: cardinal;
  end;

  TUdfImage = packed record
    PrimaryVolumeDescriptor_UDF: PPrimaryVolumeDescriptor_UDF;
    LogicalVolumeDescriptor: PLogicalVolumeDescriptor;
    PartitionDescriptor: PPartitionDescriptor;
    FileSetDescriptorEx: PFileSetDescriptorEx;
    primary_num: integer;
    logic_num: integer;
    partition_desc_num: integer;
    file_set_desc_num: integer;
  end;


implementation

{ ************ Low-Level ISO 9660 Encoding Structures }

{ ************ High-Level .iso File Reader }

initialization

end.

