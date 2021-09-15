/// ViewModel/Control interfaces for the MVCServer BLOG sample
unit MVCViewModel;

interface

{$I mormot.defines.inc}

{$ifdef FPC}
  {$WARN 5024 off : Parameter not used}
  {$WARN 5027 off : Local variable is assigned but never used}
  {$WARN 5037 off : Variable does not seem to be initialized}
  {$WARN 5057 off : variable does not seem to be initialized}
  {$WARN 5091 off : variable of a managed type does not seem to be initialized}
  {$WARN 5092 off : variable of a managed type does not seem to be initialized}
{$endif FPC}

uses
  sysutils,
  contnrs,
  variants,
  mormot.core.base,
  mormot.core.data,
  mormot.core.text,
  mormot.core.json,
  mormot.core.variants,
  mormot.core.os,
  mormot.core.test,
  mormot.core.rtti,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.mvc,
  mormot.rest.sqlite3,
  mormot.orm.core,
  mormot.orm.base,
  MVCModel;

type
  /// defines the main ViewModel/Controller commands of the BLOG web site
  // - typical URI are:
  // ! blog/main/articleView?id=12 -> view one article
  // ! blog/main/authorView?id=12 -> information about one author
  // ! blog/main/login?name=...&plainpassword=... -> log as author
  // ! blog/main/articlecommit -> article edition commit (ID=0 for new)
  IBlogApplication = interface(IMvcApplication)
    ['{73B27C06-9DB9-45A2-BEDD-2013CFB609D0}']
    procedure ArticleView(
      ID: TID;
      var WithComments: boolean;
      Direction: integer;
      var Scope: variant;
      var Article: TOrmArticle;
      var Author: variant;
      var Comments: TObjectList);
    procedure AuthorView(
      var ID: TID;
      var Author: TOrmAuthor;
      out Articles: variant);
    function Login(
      const LogonName, PlainPassword: RawUtf8): TMvcAction;
    function Logout: TMvcAction;
    function ArticleComment(
      ID: TID;
      const Title, Comment: RawUtf8): TMvcAction;
    function ArticleMatch(
      const Match: RawUtf8): TMvcAction;
    procedure ArticleEdit(
      var ID: TID;
      const Title, Content: RawUtf8;
      const ValidationError: variant;
      out Article: TOrmArticle);
    function ArticleCommit(
      ID: TID;
      const Title, Content: RawUtf8): TMvcAction;
  end;

  /// session information which will be stored on client side within a cookie
  // - TMvcSessionWithCookies is able to store any record on the client side,
  // as optimized base64 encoded binary data, without any storage on the server
  // - before Delphi 2010, Rtti.RegisterFromText() is called in initialization
  // block  below, to allow proper JSON serialization as needed for fields
  // injection into the Mustache rendering data context
  TCookieData = packed record
    AuthorName: RawUtf8;
    AuthorID: cardinal;
    AuthorRights: TOrmAuthorRights;
  end;

  /// implements the ViewModel/Controller of this BLOG web site
  TBlogApplication = class(TMvcApplication, IBlogApplication)
  protected
    fBlogMainInfo: variant;
    fTagsLookup: TOrmTags;
    fDefaultData: ILockedDocVariant;
    fDefaultLastID: TID;
    fHasFTS: boolean;
    procedure ComputeMinimalData; virtual;
    procedure FlushAnyCache; override;
    procedure GetViewInfo(MethodIndex: integer; out info: variant); override;
    function GetLoggedAuthorID(Right: TOrmAuthorRight;
      ContentToFillAuthor: TOrmContent): TID;
    procedure MonthToText(const Value: variant; out result: variant);
    procedure TagToText(const Value: variant; out result: variant);
  public
    procedure Start(aServer: TRest); reintroduce;
  public
    procedure Default(
      var Scope: variant);
    procedure ArticleView(
      ID: TID;
      var WithComments: boolean;
      Direction: integer;
      var Scope: variant;
      var Article: TOrmArticle;
      var Author: variant;
      var Comments: TObjectList);
    procedure AuthorView(
      var ID: TID;
      var Author: TOrmAuthor;
      out Articles: variant);
    function Login(
      const LogonName, PlainPassword: RawUtf8): TMvcAction;
    function Logout: TMvcAction;
    function ArticleComment(
      ID: TID;
      const Title, Comment: RawUtf8): TMvcAction;
    function ArticleMatch(
      const Match: RawUtf8): TMvcAction;
    procedure ArticleEdit(
      var ID: TID;
      const Title, Content: RawUtf8; const
      ValidationError: variant;
      out Article: TOrmArticle);
    function ArticleCommit(
      ID: TID;
      const Title, Content: RawUtf8): TMvcAction;
    property HasFts: boolean read fHasFts write fHasFts;
  end;

implementation

resourcestring
  sErrorInvalidLogin = 'Wrong logging information';
  sErrorNeedValidAuthorSession =
    'You need to be logged as a valid Author to perform this action';
  sErrorWriting = 'An error occured during saving the information to the database';


{ TBlogApplication }

procedure TBlogApplication.Start(aServer: TRest);
begin
  fDefaultData := TLockedDocVariant.Create;
  inherited Start(aServer, TypeInfo(IBlogApplication));
  fHasFTS := true;
  // TRestOrmServer(TRestServer(aServer).Server).StaticVirtualTable[TOrmArticle]=nil;
  fTagsLookup.Init(RestModel.Orm);
  // publish IBlogApplication using Mustache Views (TMvcRunOnRestServer default)
  fMainRunner := TMvcRunOnRestServer.Create(Self).
    SetCache('Default', cacheRootIfNoSession, 15).
    SetCache('ArticleView', cacheWithParametersIfNoSession, 60).
    SetCache('AuthorView', cacheWithParametersIgnoringSession, 60);
  with TMvcRunOnRestServer(fMainRunner) do
  begin
    PublishOptions := PublishOptions - [cacheStatic];
    StaticCacheControlMaxAge := 60 * 30; // 30 minutes
  end;
  (TMvcRunOnRestServer(fMainRunner).Views as TMvcViewsMustache).
    RegisterExpressionHelpers(['MonthToText'], [MonthToText]).
    RegisterExpressionHelpers(['TagToText'],   [TagToText]);
  // data setup
  ComputeMinimalData;
  aServer.Orm.Cache.SetCache(TOrmAuthor);
  aServer.Orm.Cache.SetCache(TOrmArticle);
  aServer.Orm.Cache.SetCache(TOrmComment);
  aServer.Orm.Cache.SetTimeOut(TOrmArticle, 60000);
  aServer.Orm.Cache.SetTimeOut(TOrmComment, 60000);
  with TOrmBlogInfo.Create(RestModel.Orm, '') do
  try
    fBlogMainInfo := GetSimpleFieldsAsDocVariant(false);
  finally
    Free;
  end;
end;

procedure TBlogApplication.MonthToText(const Value: variant; out result: variant);
const
  MONTHS: array[0..11] of RawUtf8 = (
    'January', 'February', 'March', 'April', 'May', 'June',
    'July', 'August', 'September', 'October', 'November', 'December');
var
  month: integer;
begin
  if VariantToInteger(Value, month) and
     (month > 0) then
    RawUtf8ToVariant(MONTHS[month mod 12] + ' ' + UInt32ToUTF8(month div 12), result)
  else
    SetVariantNull(result);
end;

procedure TBlogApplication.TagToText(const Value: variant; out result: variant);
var
  tag: integer;
begin
  if VariantToInteger(Value, tag) then
    RawUtf8ToVariant(fTagsLookup.Get(tag), result)
  else
    SetVariantNull(result);
end;

const
  // just try with 100000 - and let your WordPress blog engine start to cry...
  // note that it includes FullText indexation if you use SQLite3 as database!
  FAKEDATA_ARTICLESCOUNT = 10000;

procedure TBlogApplication.ComputeMinimalData;
var
  info: TOrmBlogInfo;
  article: TOrmArticle;
  comment: TOrmComment;
  tag: TOrmTag;
  batch: TRestBatch;
  n, t: integer;
  articles, tags, comments: TIDDynArray;
  tmp: RawUtf8;
  auto: IAutoFree; // mandatory only for FPC
begin
  auto := TOrm.AutoFree([ // avoid several try..finally
    @info, TOrmBlogInfo,
    @article, TOrmArticle,
    @comment, TOrmComment,
    @tag, TOrmTag]);
  if not RestModel.Orm.Retrieve('', info) then
  begin // retrieve first item
    tmp := StringFromFile(
      '/home/ab/Downloads/2020-06-16-a8003957c2ae6bde5be6ea279c9c9ce4-backup.txt');
    info.Language := 'en';
    if tmp <> '' then
    begin
      info.Title := 'Synopse Blog';
      info.Description := 'Articles, announcements, news, updates and more ' +
        'about our Open Source projects';
      info.About := 'Latest information about Synopse Open Source librairies, ' +
        'mainly the mORMot ORM/SOA/MVC framework, and SynPDF.';
    end
    else
    begin
      info.Title := 'mORMot BLOG';
      info.Description :=
        'Sample Blog Web Application using Synopse mORMot 2 MVC';
      info.About := TSynTestCase.RandomTextParagraph(10, '!');
    end;
    info.About := info.About + #13#10'Website powered by mORMot MVC ' +
      SYNOPSE_FRAMEWORK_VERSION + ', compiled with ' + COMPILER_VERSION +
      ', running on ' + ToText(OSVersion32) + '.';
    info.Copyright := '&copy;' + ToUTF8(CurrentYear) +
      '<a href=https://synopse.info>Synopse Informatique</a>';
    RestModel.Orm.Add(info, true);
  end;
  if RestModel.Orm.TableHasRows(TOrmArticle) then
    exit;
  if tmp <> '' then
  begin
    DotClearFlatImport(RestModel.Orm, tmp, fTagsLookup, 'http://blog.synopse.info',
      (TMvcRunOnRestServer(fMainRunner).Views as TMvcViewsMustache).ViewStaticFolder);
    exit;
  end;
  SetLength(tags, 32);
  for n := 1 to length(tags) do
  begin
    tag.Ident := 'Tag' + UInt32ToUtf8(n);
    tag.IDValue := n * 2; // force test TSQLTags layout
    tags[n - 1] := RestModel.Orm.Add(tag, true, true);
  end;
  fTagsLookup.Init(RestModel.Orm); // reload after initial fill
  batch := TRestBatch.Create(RestModel.Orm, TOrmArticle, 20000);
  try
    article.Author := TOrmAuthor(1);
    article.AuthorName := 'synopse';
    for n := 1 to FAKEDATA_ARTICLESCOUNT do
    begin
      article.PublishedMonth := 2014 * 12 + (n div 100);
      article.Title := TSynTestCase.RandomTextParagraph(5, ' ');
      article.abstract := TSynTestCase.RandomTextParagraph(30, '!');
      article.Content := TSynTestCase.RandomTextParagraph(
        200, '.', 'https://synopse.info');
      article.Tags := nil;
      for t := 1 to Random32(6) do
        article.TagsAddOrdered(tags[random(length(tags))], fTagsLookup);
      batch.Add(article, true);
    end;
    if RestModel.Orm.BatchSend(batch, articles) = HTTP_SUCCESS then
    begin
      fTagsLookup.SaveOccurence(RestModel.Orm);
      comment.Author := article.Author;
      comment.AuthorName := article.AuthorName;
      batch.Reset(TOrmComment, 20000);
      for n := 1 to FAKEDATA_ARTICLESCOUNT * 2 do
      begin
        comment.Article := CastID(articles[random(length(articles))]);
        comment.Title := TSynTestCase.RandomTextParagraph(5, ' ');
        comment.Content :=
          TSynTestCase.RandomTextParagraph(30, '.', 'http://mormot.net');
        batch.Add(comment, true);
      end;
      RestModel.Orm.BatchSend(batch, comments)
    end;
  finally
    batch.Free;
  end;
end;

function TBlogApplication.GetLoggedAuthorID(Right: TOrmAuthorRight;
  ContentToFillAuthor: TOrmContent): TID;
var
  SessionInfo: TCookieData;
  author: TOrmAuthor;
begin
  result := 0;
  if (CurrentSession.CheckAndRetrieve(@SessionInfo, TypeInfo(TCookieData)) > 0) and
     (Right in SessionInfo.AuthorRights) then
    with TOrmAuthor.AutoFree(author, RestModel.Orm, SessionInfo.AuthorID) do
      if Right in author.Rights then
      begin
        result := SessionInfo.AuthorID;
        if ContentToFillAuthor <> nil then
        begin
          ContentToFillAuthor.Author := CastID(result);
          ContentToFillAuthor.AuthorName := author.LogonName;
        end;
      end;
end;

procedure TBlogApplication.GetViewInfo(MethodIndex: integer; out info: variant);
var
  archives: variant; // needed to circumvent memory leak bug on FPC
begin
  inherited GetViewInfo(MethodIndex, info);
  _ObjAddProps(['blog', fBlogMainInfo,
                'session',
                  CurrentSession.CheckAndRetrieveInfo(TypeInfo(TCookieData))],
               info);
  if not fDefaultData.AddExistingProp('archives', info) then
  begin
    archives := RestModel.Orm.RetrieveDocVariantArray(TOrmArticle, '',
      'group by PublishedMonth order by PublishedMonth desc limit 100', [],
      'distinct(PublishedMonth),max(RowID)+1 as FirstID');
    fDefaultData.AddNewProp('archives', archives, info);
  end;
  if not fDefaultData.AddExistingProp('tags', info) then
    fDefaultData.AddNewProp('tags', fTagsLookup.GetAsDocVariantArray, info);
end;

procedure TBlogApplication.FlushAnyCache;
begin
  inherited FlushAnyCache; // call fMainRunner.NotifyContentChanged
  fDefaultData.Clear;
end;


{ TBlogApplication - Commands }

const
  ARTICLE_FIELDS =
    'RowID,Title,Tags,Abstract,ContentHtml,Author,AuthorName,CreatedAt';
  ARTICLE_DEFAULT_LIMIT =
    ' limit 20';
  ARTICLE_DEFAULT_ORDER: RawUtf8 =
    'order by RowID desc' + ARTICLE_DEFAULT_LIMIT;

procedure TBlogApplication.Default(var Scope: variant);
var
  scop: PDocVariantData;
  lastID: TID;
  tag: integer;
  whereClause, match: RawUtf8;
  articles: variant;
  rank: double;
begin
  lastID := 0;
  tag := 0;
  rank := 0;
  scop := _Safe(Scope);
  if scop^.GetAsRawUtf8('match', match) and
     fHasFTS then
  begin
    if scop^.GetAsDouble('lastrank', rank) then
      whereClause := 'and rank<? ';
    whereClause :=
      'join (select docid,rank(matchinfo(ArticleSearch),1.0,0.7,0.5) as rank ' +
      'from ArticleSearch where ArticleSearch match ? ' + whereClause +
      'order by rank desc' + ARTICLE_DEFAULT_LIMIT + ')as r on (r.docid=Article.id)';
    articles := RestModel.Orm.RetrieveDocVariantArray(
      TOrmArticle, '', whereClause, [match, rank],
      'id,title,tags,author,authorname,createdat,abstract,contenthtml,rank');
    with _Safe(articles)^ do
      if IsArray and
         (Count > 0) then
        rank := Values{%H-}[Count - 1].rank
      else
        rank := 0;
    Scope := _ObjFast(['Articles', articles,
                       'lastrank', rank,
                       'match', match]);
    exit;
  end
  else
  begin
    if scop^.GetAsInt64('lastID', Int64(lastID)) then
      whereClause := 'RowID<?'
    else
      whereClause := 'RowID>?'; // will search ID>0 so always true
    if scop^.GetAsInteger('tag', tag) and
       (tag > 0) then
      // uses custom function to search in BLOB
      whereClause := whereClause + ' and IntegerDynArrayContains(Tags,?)';
  end;
  SetVariantNull(Scope);
  if (lastID = 0) and
     (tag = 0) then
  begin // use simple cache if no parameters
    if not fDefaultData.AddExistingProp('Articles', Scope) then
    begin
      articles := RestModel.Orm.RetrieveDocVariantArray(TOrmArticle, '',
        ARTICLE_DEFAULT_ORDER, [], ARTICLE_FIELDS, nil, @fDefaultLastID);
      fDefaultData.AddNewProp('Articles', articles, Scope);
    end;
    lastID := fDefaultLastID;
  end
  else
  begin
    // use more complex request using lastID + tag parameters
    articles := RestModel.Orm.RetrieveDocVariantArray(TOrmArticle, '',
      whereClause + ARTICLE_DEFAULT_ORDER, [lastID, tag], ARTICLE_FIELDS, nil, @lastID);
    Scope := _ObjFast(['Articles', articles]);
  end;
  if lastID > 1 then
    _ObjAddProps(['lastID', lastID, 'tag', tag], Scope);
end;

procedure TBlogApplication.ArticleView(ID: TID; var WithComments: boolean;
  Direction: integer; var Scope: variant; var Article: TOrmArticle;
  var Author: variant; var Comments: TObjectList);
var
  newID: Int64;
const
  WHERE: array[1..2] of PUtf8Char = (
    'RowID<? order by id desc',
    'RowID>? order by id');
begin
  if Direction in [1, 2] then
    // allows fast paging using index on ID
    if RestModel.Orm.OneFieldValue(TOrmArticle, 'RowID',
         WHERE[Direction], [], [ID], newID) and
       (newID <> 0) then
      ID := newID;
  RestModel.Orm.Retrieve(ID, Article);
  if Article.ID <> 0 then
  begin
    Author := RestModel.Orm.RetrieveDocVariant(
      TOrmAuthor, 'RowID=?', [Article.Author.ID], 'FirstName,FamilyName');
    if WithComments then
    begin
      Comments.Free; // we will override the TObjectList created at input
      Comments := RestModel.Orm.RetrieveList(
        TOrmComment, 'Article=?', [Article.ID]);
    end;
  end
  else
    raise EMvcApplication.CreateGotoError(HTTP_NOTFOUND);
end;

procedure TBlogApplication.AuthorView(var ID: TID; var Author: TOrmAuthor;
  out Articles: variant);
begin
  RestModel.Orm.Retrieve(ID, Author);
  Author.HashedPassword := ''; // no need to publish it
  if Author.ID <> 0 then
    Articles := RestModel.Orm.RetrieveDocVariantArray(TOrmArticle, '',
      'Author=? order by RowId desc limit 50', [ID], ARTICLE_FIELDS)
  else
    raise EMvcApplication.CreateGotoError(HTTP_NOTFOUND);
end;

function TBlogApplication.Login(const LogonName, PlainPassword: RawUtf8): TMvcAction;
var
  Author: TOrmAuthor;
  SessionInfo: TCookieData;
begin
  if CurrentSession.CheckAndRetrieve <> 0 then
  begin
    GotoError(result, HTTP_BADREQUEST);
    exit;
  end;
  Author := TOrmAuthor.Create(RestModel.Orm, 'LogonName=?', [LogonName]);
  try
    if (Author.ID <> 0) and
       Author.CheckPlainPassword(PlainPassword) then
    begin
      SessionInfo.AuthorName := Author.LogonName;
      SessionInfo.AuthorID := Author.ID;
      SessionInfo.AuthorRights := Author.Rights;
      CurrentSession.Initialize(@SessionInfo, TypeInfo(TCookieData));
      GotoDefault(result);
    end
    else
      GotoError(result, sErrorInvalidLogin);
  finally
    Author.Free;
  end;
end;

function TBlogApplication.Logout: TMvcAction;
begin
  CurrentSession.Finalize;
  GotoDefault(result);
end;

function TBlogApplication.ArticleComment(
  ID: TID; const Title, Comment: RawUtf8): TMvcAction;
var
  comm: TOrmComment;
  AuthorID: TID;
  error: string;
begin
  with TOrmComment.AutoFree(comm) do
  begin
    AuthorID := GetLoggedAuthorID(canComment, comm);
    if AuthorID = 0 then
    begin
      GotoError(result, sErrorNeedValidAuthorSession);
      exit;
    end;
    if not RestModel.Orm.MemberExists(TOrmArticle, ID) then
    begin
      GotoError(result, HTTP_UNAVAILABLE);
      exit;
    end;
    comm.Title := Title;
    comm.Content := Comment;
    comm.Article := TOrmArticle(ID);
    if comm.FilterAndValidate(RestModel.Orm, error) and
       (RestModel.Orm.Add(comm, true) <> 0) then
      GotoView(result, 'ArticleView',
        ['ID', ID,
         'withComments', true])
    else
      GotoView(result, 'ArticleView',
        ['ID', ID,
         'withComments', true,
         'Scope', _ObjFast(['CommentError', error, 'CommentTitle', comm.Title,
          'CommentContent', comm.Content])], HTTP_BADREQUEST);
  end;
end;

function TBlogApplication.ArticleMatch(const Match: RawUtf8): TMvcAction;
begin
  if Match = '' then
    GotoError(result, HTTP_NOTMODIFIED)
  else
    GotoView(result, 'Default', ['scope', _ObjFast(['match', Match])]);
end;

procedure TBlogApplication.ArticleEdit(var ID: TID; const Title, Content: RawUtf8;
  const ValidationError: variant; out Article: TOrmArticle);
var
  AuthorID: PtrUInt;
begin
  AuthorID := GetLoggedAuthorID(canPost, Article);
  if AuthorID = 0 then
    raise EMvcApplication.CreateGotoError(sErrorNeedValidAuthorSession);
  if ID <> 0 then
    if not RestModel.Orm.Retrieve(ID, Article) then
      raise EMvcApplication.CreateGotoError(HTTP_UNAVAILABLE)
    else if Article.Author <> CastID(AuthorID) then
      raise EMvcApplication.CreateGotoError(sErrorNeedValidAuthorSession);
  if Title <> '' then
    Article.Title := Title;
  if Content <> '' then
    Article.Content := Content;
end;

function TBlogApplication.ArticleCommit(
  ID: TID; const Title, Content: RawUtf8): TMvcAction;
var
  Article: TOrmArticle;
  AuthorID: TID;
  error: string;
begin
  with TOrmArticle.AutoFree(Article, RestModel.Orm, ID) do
  begin
    AuthorID := GetLoggedAuthorID(canPost, Article);
    if AuthorID = 0 then
    begin
      GotoError(result, sErrorNeedValidAuthorSession);
      exit;
    end;
    FlushAnyCache;
    Article.Title := Title;
    Article.Content := Content;
    if not Article.FilterAndValidate(RestModel.Orm, error) then
      GotoView(result, 'ArticleEdit',
        ['ValidationError', error,
         'ID', ID,
         'Title', Article.Title,
         'Content', Article.Content], HTTP_BADREQUEST)
    else if Article.ID = 0 then
    begin
      Article.PublishedMonth := TOrmArticle.CurrentPublishedMonth;
      if RestModel.Orm.Add(Article, true) <> 0 then
        GotoView(result, 'ArticleView',
          ['ID', Article.ID], HTTP_SUCCESS)
      else
        GotoError(result, sErrorWriting);
    end
    else
      RestModel.Orm.Update(Article);
  end;
end;

initialization
  {$ifndef DELPHI2010}
  // manual definition mandatory only if Delphi 2010 RTTI is not available
  Rtti.RegisterType(TypeInfo(TOrmAuthorRights));
  Rtti.RegisterFromText(TypeInfo(TCookieData),
    'AuthorName:RawUtf8 AuthorID:cardinal AuthorRights:TOrmAuthorRights');
  {$endif DELPHI2010}

end.

