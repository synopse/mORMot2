unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.unicode,
  mormot.core.rtti,
  mormot.core.interfaces;

type
  /// Version information record
  TVersionInfo = packed record
    ServerName: RawUtf8;
    Version: RawUtf8;
    BuildDate: RawUtf8;
    mORMot2Version: RawUtf8;
  end;

  /// Processing result record
  TProcessingResult = packed record
    Success: Boolean;
    ProcessedData: RawUtf8;
    ProcessingTimeMs: Integer;
    Message: RawUtf8;
  end;

  /// Article summary record
  TArticleSummary = packed record
    ID: TID;
    Title: RawUtf8;
    Author: RawUtf8;
    ViewCount: Integer;
    IsPublished: Boolean;
  end;
  TArticleSummaryDynArray = array of TArticleSummary;

  /// Batch creation result
  TBatchResult = packed record
    Success: Boolean;
    CreatedCount: Integer;
    CreatedIDs: TIDDynArray;
    Message: RawUtf8;
  end;

  /// Database statistics
  TDatabaseStats = packed record
    TotalArticles: Integer;
    PublishedArticles: Integer;
    TotalAuthors: Integer;
    TotalViewCount: Int64;
    LastUpdateTime: TDateTime;
  end;

  /// Complete API interface - demonstrates all mORMot2 capabilities
  ICompleteApi = interface(IInvokable)
    ['{51A1B2C3-D4E5-F678-90AB-CDEF12345001}']

    /// Get server version information
    function GetVersion: TVersionInfo;

    /// Process data with validation and transformation
    function ProcessData(const aData: RawUtf8): TProcessingResult;

    /// Get articles by author - demonstrates filtering
    function GetArticlesByAuthor(const aAuthor: RawUtf8): TArticleSummaryDynArray;

    /// Batch create articles - demonstrates transactions
    function BatchCreateArticles(aCount: Integer; const aPrefix: RawUtf8): TBatchResult;

    /// Get database statistics - demonstrates aggregation
    function GetStatistics: TDatabaseStats;

    /// Increment article view count - demonstrates updates
    function IncrementViewCount(aArticleID: TID): Boolean;

    /// Search articles by keyword - demonstrates full-text search
    function SearchArticles(const aKeyword: RawUtf8): TArticleSummaryDynArray;
  end;

const
  SERVICE_CONTRACT_COMPLETE = 'CompleteApi';

implementation

initialization
  // Register records for JSON serialization
  Rtti.RegisterFromText(TypeInfo(TVersionInfo),
    'servername:RawUtf8 version:RawUtf8 builddate:RawUtf8 mormot2version:RawUtf8');
  Rtti.RegisterFromText(TypeInfo(TProcessingResult),
    'success:Boolean processeddata:RawUtf8 processingtimems:Integer message:RawUtf8');
  Rtti.RegisterFromText(TypeInfo(TArticleSummary),
    'id:Int64 title:RawUtf8 author:RawUtf8 viewcount:Integer ispublished:Boolean');
  Rtti.RegisterFromText(TypeInfo(TBatchResult),
    'success:Boolean createdcount:Integer createdids:TIDDynArray message:RawUtf8');
  Rtti.RegisterFromText(TypeInfo(TDatabaseStats),
    'totalarticles:Integer publishedarticles:Integer totalauthors:Integer totalviewcount:Int64 lastupdatetime:TDateTime');

  // Register interface for TypeInfo(ICompleteApi)
  TInterfaceFactory.RegisterInterfaces([TypeInfo(ICompleteApi)]);

end.
