unit api.impl;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  RegularExpressions,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.log,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.data,
  mormot.core.variants,
  mormot.core.interfaces,
  mormot.orm.core,
  mormot.soa.core,
  api.interfaces,
  entities;

type
  /// Articles CRUD API implementation
  // Ports DMVC TArticlesService and business logic from TArticle to mORMot2
  TArticlesApi = class(TInterfacedObject, IArticlesApi)
  protected
    fRest: IRestOrm;
    /// Validates article code format (must be C followed by 2-4 digits)
    procedure ValidateArticleCode(const code: RawUtf8);
    /// Validates price for update (must be > 2)
    procedure ValidatePriceForUpdate(price: Currency);
    /// Validates price for delete (must be > 5)
    procedure ValidatePriceForDelete(price: Currency);
    /// Converts TOrmArticle to TArticleDto
    function OrmToDto(const orm: TOrmArticle): TArticleDto;
    /// Converts TArticleDto to TOrmArticle
    procedure DtoToOrm(const dto: TArticleDto; orm: TOrmArticle);
  public
    constructor CreateApi(const aRest: IRestOrm);
    // IArticlesApi methods
    function GetAll: TArticleDtos;
    function Search(const query: RawUtf8): TArticleDtos;
    function GetMeta: TArticleMetaResponse;
    function GetById(id: TID): TArticleDto;
    function Create(const article: TArticleDto): TID; overload;
    function CreateBulk(const articles: TArticleDtos): Integer;
    procedure Update(id: TID; const article: TArticleDto);
    procedure Delete(id: TID);
  end;


implementation


{ TArticlesApi }

constructor TArticlesApi.CreateApi(const aRest: IRestOrm);
begin
  inherited Create;
  fRest := aRest;
end;

procedure TArticlesApi.ValidateArticleCode(const code: RawUtf8);
var
  pattern: string;
begin
  // Port of TArticle.OnBeforeInsertOrUpdate validation
  // Code must match format "CXX" or "CXXX" or "CXXXX"
  pattern := '^C[0-9]{2,4}$';
  if not TRegEx.IsMatch(Utf8ToString(code), pattern) then
    raise EServiceException.CreateUtf8(
      'Article code must be in the format "CXX or CXXX or CXXXX"', []);
end;

procedure TArticlesApi.ValidatePriceForUpdate(price: Currency);
begin
  // Port of TArticle.OnBeforeUpdate validation
  if price <= 2 then
    raise EServiceException.Create(
      'We cannot sell so low cost pizzas!');
end;

procedure TArticlesApi.ValidatePriceForDelete(price: Currency);
begin
  // Port of TArticle.OnBeforeDelete validation
  if price <= 5 then
    raise EServiceException.Create(
      'Cannot delete an article with a price below 5 euros (yes, it is a silly check)');
end;

function TArticlesApi.OrmToDto(const orm: TOrmArticle): TArticleDto;
begin
  result.ID := orm.ID;
  result.Code := orm.Code;
  result.Description := orm.Description;
  result.Price := orm.Price;
  result.CreatedAt := orm.CreatedAt;
  result.UpdatedAt := orm.UpdatedAt;
end;

procedure TArticlesApi.DtoToOrm(const dto: TArticleDto; orm: TOrmArticle);
begin
  orm.Code := dto.Code;
  orm.Description := dto.Description;
  orm.Price := dto.Price;
  // Don't set ID or timestamps - handled by ORM
end;

function TArticlesApi.GetAll: TArticleDtos;
var
  articles: TOrmArticles;
  i: PtrInt;
begin
  // Port of TArticlesService.GetAll
  // Original: TMVCActiveRecord.SelectRQL<TArticle>('sort(+id)', 1000)
  TSynLog.Add.Log(sllInfo, 'GetAll: Fetching all articles');

  if not fRest.RetrieveListObjArray(articles, TOrmArticle, '', []) then
    articles := nil;
  try
    SetLength(result, Length(articles));
    for i := 0 to High(articles) do
      result[i] := OrmToDto(articles[i]);

    TSynLog.Add.Log(sllInfo, 'GetAll: Returned % articles', [Length(result)]);
  finally
    ObjArrayClear(articles);
  end;
end;

function TArticlesApi.Search(const query: RawUtf8): TArticleDtos;
var
  articles: TOrmArticles;
  i: PtrInt;
  whereClause: RawUtf8;
begin
  // Port of TArticlesService.GetArticles with search query
  // Original: TMVCActiveRecord.SelectByNamedQuery<TArticle>('search_by_text', [aTextSearch], [ftString])
  // Named query was: 'SELECT * FROM ARTICOLI WHERE DESCRIZIONE CONTAINING ? ORDER BY ID'
  TSynLog.Add.Log(sllInfo, 'Search: query=%', [query]);

  if query = '' then
  begin
    result := GetAll;
    Exit;
  end;

  // SQLite uses LIKE instead of CONTAINING
  whereClause := FormatUtf8('Description LIKE ? ORDER BY ID', []);
  if not fRest.RetrieveListObjArray(articles, TOrmArticle, whereClause, ['%' + query + '%']) then
    articles := nil;
  try
    SetLength(result, Length(articles));
    for i := 0 to High(articles) do
      result[i] := OrmToDto(articles[i]);

    TSynLog.Add.Log(sllInfo, 'Search: Found % articles', [Length(result)]);
  finally
    ObjArrayClear(articles);
  end;
end;

function TArticlesApi.GetMeta: TArticleMetaResponse;
var
  doc: TDocVariantData;
begin
  // Port of TArticlesService.GetMeta
  // Returns field metadata as JSON
  TSynLog.Add.Log(sllInfo, 'GetMeta: Returning article metadata');

  doc.InitFast;
  doc.AddValue('id', 'Int64');
  doc.AddValue('code', 'String(20)');
  doc.AddValue('description', 'String(200)');
  doc.AddValue('price', 'Currency');
  doc.AddValue('createdat', 'DateTime');
  doc.AddValue('updatedat', 'DateTime');

  result.Fields := doc.ToJson;
end;

function TArticlesApi.GetById(id: TID): TArticleDto;
var
  article: TOrmArticle;
begin
  // Port of TArticlesService.GetByID
  // Original: TMVCActiveRecord.GetByPK<TArticle>(AID)
  TSynLog.Add.Log(sllInfo, 'GetById: id=%', [id]);

  article := TOrmArticle.Create(fRest, id);
  try
    if article.ID = 0 then
      raise EServiceException.CreateUtf8('Article % not found', [id]);

    result := OrmToDto(article);
    TSynLog.Add.Log(sllInfo, 'GetById: Found article code=%', [result.Code]);
  finally
    article.Free;
  end;
end;

function TArticlesApi.Create(const article: TArticleDto): TID;
var
  orm: TOrmArticle;
begin
  // Port of TArticlesService.Add
  // Original: AArticolo.Insert
  TSynLog.Add.Log(sllInfo, 'Create: code=%, price=%', [article.Code, article.Price]);

  // Validate code format
  ValidateArticleCode(article.Code);

  orm := TOrmArticle.Create;
  try
    DtoToOrm(article, orm);
    orm.CreatedAt := Now;
    orm.UpdatedAt := Now;

    result := fRest.Add(orm, true);
    if result = 0 then
      raise EServiceException.Create('Failed to create article');

    TSynLog.Add.Log(sllInfo, 'Create: Created article id=%', [result]);
  finally
    orm.Free;
  end;
end;

function TArticlesApi.CreateBulk(const articles: TArticleDtos): Integer;
var
  i: PtrInt;
  batch: TRestBatch;
begin
  // Port of TArticlesService.CreateArticles
  // Original: Transaction with multiple Insert calls
  TSynLog.Add.Log(sllInfo, 'CreateBulk: Creating % articles', [Length(articles)]);

  result := 0;
  batch := TRestBatch.Create(fRest, TOrmArticle, {transactionrow=}1000);
  try
    for i := 0 to High(articles) do
    begin
      var orm := TOrmArticle.Create;
      try
        ValidateArticleCode(articles[i].Code);
        DtoToOrm(articles[i], orm);
        orm.CreatedAt := Now;
        orm.UpdatedAt := Now;
        batch.Add(orm, true);
      finally
        orm.Free;
      end;
    end;

    result := fRest.BatchSend(batch);
    TSynLog.Add.Log(sllInfo, 'CreateBulk: Created % articles', [result]);
  finally
    batch.Free;
  end;
end;

procedure TArticlesApi.Update(id: TID; const article: TArticleDto);
var
  orm: TOrmArticle;
begin
  // Port of TArticlesService.Update
  // Original: AArticolo.Update()
  TSynLog.Add.Log(sllInfo, 'Update: id=%, code=%, price=%',
    [id, article.Code, article.Price]);

  // Validate code format and price
  ValidateArticleCode(article.Code);
  ValidatePriceForUpdate(article.Price);

  orm := TOrmArticle.Create(fRest, id);
  try
    if orm.ID = 0 then
      raise EServiceException.CreateUtf8('Article % not found', [id]);

    DtoToOrm(article, orm);
    orm.UpdatedAt := Now;

    if not fRest.Update(orm) then
      raise EServiceException.CreateUtf8('Failed to update article %', [id]);

    TSynLog.Add.Log(sllInfo, 'Update: Updated article id=%', [id]);
  finally
    orm.Free;
  end;
end;

procedure TArticlesApi.Delete(id: TID);
var
  article: TOrmArticle;
begin
  // Port of TArticlesService.Delete
  // Original: AArticolo.Delete()
  TSynLog.Add.Log(sllInfo, 'Delete: id=%', [id]);

  article := TOrmArticle.Create(fRest, id);
  try
    if article.ID = 0 then
      raise EServiceException.CreateUtf8('Article % not found', [id]);

    // Validate price for delete
    ValidatePriceForDelete(article.Price);

    if not fRest.Delete(TOrmArticle, id) then
      raise EServiceException.CreateUtf8('Failed to delete article %', [id]);

    TSynLog.Add.Log(sllInfo, 'Delete: Deleted article id=%', [id]);
  finally
    article.Free;
  end;
end;


end.
