unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.text,
  mormot.core.rtti,
  mormot.core.interfaces;

type
  /// Article DTO (Data Transfer Object)
  // Port of DMVC TArticle business object to mORMot2 record
  TArticleDto = packed record
    ID: TID;
    Code: RawUtf8;
    Description: RawUtf8;
    Price: Currency;
    CreatedAt: TDateTime;
    UpdatedAt: TDateTime;
  end;
  TArticleDtos = array of TArticleDto;

  /// Article metadata response
  TArticleMetaResponse = packed record
    Fields: RawUtf8;  // JSON with field metadata
  end;

  /// Articles CRUD API interface
  // Ports DMVC IArticlesService and TArticlesController to mORMot2
  IArticlesApi = interface(IInvokable)
    ['{F1A2B3C4-D5E6-7890-ABCD-EF1234567890}']

    /// GET /articles - Returns list of all articles
    function GetAll: TArticleDtos;

    /// GET /articles/searches?q=text - Search articles by description
    function Search(const query: RawUtf8): TArticleDtos;

    /// GET /articles/meta - Returns article field metadata
    function GetMeta: TArticleMetaResponse;

    /// GET /articles/{id} - Returns article by ID
    function GetById(id: TID): TArticleDto;

    /// POST /articles - Creates a new article
    // Returns the ID of the created article
    function Create(const article: TArticleDto): TID;

    /// POST /articles/bulk - Creates multiple articles
    // Returns count of created articles
    function CreateBulk(const articles: TArticleDtos): Integer;

    /// PUT /articles/{id} - Updates an article
    procedure Update(id: TID; const article: TArticleDto);

    /// DELETE /articles/{id} - Deletes an article
    procedure Delete(id: TID);
  end;


implementation


initialization
  // Customize DTO field names (lowercase for JSON)
  Rtti.RegisterFromText(TypeInfo(TArticleDto),
    'id:Int64 code:RawUtf8 description:RawUtf8 price:Currency ' +
    'createdat:TDateTime updatedat:TDateTime');

  Rtti.RegisterFromText(TypeInfo(TArticleMetaResponse),
    'fields:RawUtf8');

  // Register interface for TypeInfo(IArticlesApi)
  TInterfaceFactory.RegisterInterfaces([TypeInfo(IArticlesApi)]);

end.
