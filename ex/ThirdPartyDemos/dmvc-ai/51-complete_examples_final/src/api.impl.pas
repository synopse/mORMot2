unit api.impl;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  Classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.rtti,
  mormot.core.interfaces,
  mormot.core.perf,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.rest.server,
  mormot.soa.server,
  entities,
  api.interfaces;

type
  /// Implementation of complete API demonstrating all features
  TCompleteApiService = class(TInjectableObjectRest, ICompleteApi)
  private
    fServer: TRestServer;
  public
    constructor Create(aServer: TRestServer); reintroduce;

    // ICompleteApi implementation
    function GetVersion: TVersionInfo;
    function ProcessData(const aData: RawUtf8): TProcessingResult;
    function GetArticlesByAuthor(const aAuthor: RawUtf8): TArticleSummaryDynArray;
    function BatchCreateArticles(aCount: Integer; const aPrefix: RawUtf8): TBatchResult;
    function GetStatistics: TDatabaseStats;
    function IncrementViewCount(aArticleID: TID): Boolean;
    function SearchArticles(const aKeyword: RawUtf8): TArticleSummaryDynArray;
  end;

implementation

uses
  mormot.rest.core;

{ TCompleteApiService }

constructor TCompleteApiService.Create(aServer: TRestServer);
begin
  inherited Create;
  fServer := aServer;
end;

function TCompleteApiService.GetVersion: TVersionInfo;
begin
  TSynLog.Add.Log(sllTrace, 'GetVersion called');

  Result.ServerName := 'mORMot2 Complete Example Server';
  Result.Version := '1.0.0';
  Result.BuildDate := StringToUtf8(DateTimeToStr(Now));
  Result.mORMot2Version := SYNOPSE_FRAMEWORK_VERSION;

  TSynLog.Add.Log(sllDebug, 'GetVersion: % v%',
    [Result.ServerName, Result.Version]);
end;

function TCompleteApiService.ProcessData(const aData: RawUtf8): TProcessingResult;
var
  timer: TPrecisionTimer;
begin
  TSynLog.Add.Log(sllTrace, 'ProcessData called with: %', [aData]);

  timer.Start;

  // Validation
  if aData = '' then
  begin
    Result.Success := False;
    Result.Message := 'Data cannot be empty';
    Result.ProcessedData := '';
    Result.ProcessingTimeMs := timer.TimeInMicroSec div 1000;
    TSynLog.Add.Log(sllWarning, 'ProcessData: validation failed - empty data');
    Exit;
  end;

  // Processing simulation
  Result.Success := True;
  Result.ProcessedData := UpperCaseU(aData) + ' (PROCESSED)';
  Result.Message := FormatUtf8('Successfully processed % bytes', [Length(aData)]);
  Result.ProcessingTimeMs := timer.TimeInMicroSec div 1000;

  TSynLog.Add.Log(sllDebug, 'ProcessData: success, % bytes, % ms',
    [Length(aData), Result.ProcessingTimeMs]);
end;

function TCompleteApiService.GetArticlesByAuthor(
  const aAuthor: RawUtf8): TArticleSummaryDynArray;
var
  articles: array of TArticle;
  i: PtrInt;
begin
  TSynLog.Add.Log(sllTrace, 'GetArticlesByAuthor: %', [aAuthor]);

  // Query articles by author
  if fServer.Orm.RetrieveListObjArray(articles, TArticle, 'Author=?', [aAuthor]) then
  begin
    SetLength(Result, Length(articles));
    for i := 0 to High(articles) do
    begin
      Result[i].ID := articles[i].ID;
      Result[i].Title := articles[i].Title;
      Result[i].Author := articles[i].Author;
      Result[i].ViewCount := articles[i].ViewCount;
      Result[i].IsPublished := articles[i].Published;
    end;

    TSynLog.Add.Log(sllDebug, 'GetArticlesByAuthor: found % articles for %',
      [Length(articles), aAuthor]);
  end
  else
  begin
    SetLength(Result, 0);
    TSynLog.Add.Log(sllDebug, 'GetArticlesByAuthor: no articles found for %',
      [aAuthor]);
  end;

  ObjArrayClear(articles);
end;

function TCompleteApiService.BatchCreateArticles(aCount: Integer;
  const aPrefix: RawUtf8): TBatchResult;
var
  i: Integer;
  article: TArticle;
  batch: TRestBatch;
begin
  TSynLog.Add.Log(sllTrace, 'BatchCreateArticles: % articles with prefix "%"',
    [aCount, aPrefix]);

  Result.Success := False;
  Result.CreatedCount := 0;
  SetLength(Result.CreatedIDs, 0);

  // Validation
  if (aCount <= 0) or (aCount > 100) then
  begin
    Result.Message := 'Count must be between 1 and 100';
    TSynLog.Add.Log(sllWarning, 'BatchCreateArticles: invalid count %', [aCount]);
    Exit;
  end;

  // Use batch for transaction
  batch := TRestBatch.Create(fServer.Orm, TArticle, {transactionrows=}1000);
  try
    SetLength(Result.CreatedIDs, aCount);

    for i := 0 to aCount - 1 do
    begin
      article := TArticle.Create;
      try
        article.Title := FormatUtf8('% Article #%', [aPrefix, i + 1]);
        article.Author := FormatUtf8('Author %', [(i mod 5) + 1]);
        article.Content := FormatUtf8('Content for % article number %',
          [aPrefix, i + 1]);
        article.CreatedAt := NowUtc;
        article.UpdatedAt := NowUtc;
        article.ViewCount := 0;
        article.Published := True;

        Result.CreatedIDs[i] := batch.Add(article, true);
      finally
        article.Free;
      end;
    end;

    // Execute batch
    if fServer.Orm.BatchSend(batch) = HTTP_SUCCESS then
    begin
      Result.Success := True;
      Result.CreatedCount := aCount;
      Result.Message := FormatUtf8('Successfully created % articles', [aCount]);

      TSynLog.Add.Log(sllInfo, 'BatchCreateArticles: created % articles',
        [aCount]);
    end
    else
    begin
      Result.Message := 'Batch operation failed';
      TSynLog.Add.Log(sllError, 'BatchCreateArticles: batch send failed');
    end;

  finally
    batch.Free;
  end;
end;

function TCompleteApiService.GetStatistics: TDatabaseStats;
var
  articles: array of TArticle;
  i: PtrInt;
  authors: TRawUtf8DynArray;
begin
  TSynLog.Add.Log(sllTrace, 'GetStatistics called');

  FillCharFast(Result, SizeOf(Result), 0);

  // Retrieve all articles
  if fServer.Orm.RetrieveListObjArray(articles, TArticle, '', []) then
  begin
    Result.TotalArticles := Length(articles);

    for i := 0 to High(articles) do
    begin
      if articles[i].Published then
        Inc(Result.PublishedArticles);

      Inc(Result.TotalViewCount, articles[i].ViewCount);

      // Count unique authors
      if FindRawUtf8(authors, articles[i].Author) < 0 then
        AddRawUtf8(authors, articles[i].Author);
    end;

    Result.TotalAuthors := Length(authors);
    Result.LastUpdateTime := NowUtc;

    TSynLog.Add.Log(sllDebug,
      'GetStatistics: % total, % published, % authors, % views',
      [Result.TotalArticles, Result.PublishedArticles, Result.TotalAuthors,
       Result.TotalViewCount]);
  end;

  ObjArrayClear(articles);
end;

function TCompleteApiService.IncrementViewCount(aArticleID: TID): Boolean;
var
  article: TArticle;
begin
  TSynLog.Add.Log(sllTrace, 'IncrementViewCount: article %', [aArticleID]);

  article := TArticle.Create(fServer.Orm, aArticleID);
  try
    if article.ID <> 0 then
    begin
      article.ViewCount := article.ViewCount + 1;
      article.UpdatedAt := NowUtc;

      Result := fServer.Orm.Update(article);

      if Result then
        TSynLog.Add.Log(sllDebug, 'IncrementViewCount: article % now has % views',
          [aArticleID, article.ViewCount])
      else
        TSynLog.Add.Log(sllError, 'IncrementViewCount: update failed for %',
          [aArticleID]);
    end
    else
    begin
      Result := False;
      TSynLog.Add.Log(sllWarning, 'IncrementViewCount: article % not found',
        [aArticleID]);
    end;
  finally
    article.Free;
  end;
end;

function TCompleteApiService.SearchArticles(
  const aKeyword: RawUtf8): TArticleSummaryDynArray;
var
  articles: array of TArticle;
  i: PtrInt;
  keyword: RawUtf8;
begin
  TSynLog.Add.Log(sllTrace, 'SearchArticles: "%"', [aKeyword]);

  keyword := LowerCaseU(aKeyword);

  // Retrieve all articles for search (in production, use SQL LIKE)
  if fServer.Orm.RetrieveListObjArray(articles, TArticle, '', []) then
  begin
    SetLength(Result, 0);

    for i := 0 to High(articles) do
    begin
      // Simple search in title and content
      if (PosEx(keyword, LowerCaseU(articles[i].Title)) > 0) or
         (PosEx(keyword, LowerCaseU(articles[i].Content)) > 0) then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)].ID := articles[i].ID;
        Result[High(Result)].Title := articles[i].Title;
        Result[High(Result)].Author := articles[i].Author;
        Result[High(Result)].ViewCount := articles[i].ViewCount;
        Result[High(Result)].IsPublished := articles[i].Published;
      end;
    end;

    TSynLog.Add.Log(sllDebug, 'SearchArticles: found % results for "%"',
      [Length(Result), aKeyword]);
  end;

  ObjArrayClear(articles);
end;

end.
