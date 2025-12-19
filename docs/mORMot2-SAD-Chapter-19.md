# 19. MVC/MVVM Web Applications

*Building Complete Web Applications with mORMot*

This chapter provides a practical guide to building MVC/MVVM web applications using mORMot 2. We'll walk through the blog sample application structure, demonstrating patterns you can apply to your own projects.

---

## 19.1. Application Architecture

### 19.1.1. MVVM in mORMot

mORMot uses a hybrid MVC/MVVM pattern:

| Component | File | mORMot Class |
|-----------|------|--------------|
| **Model** | `MVCModel.pas` | `TOrm` classes, `TOrmModel` |
| **View** | `*.html` | Mustache templates |
| **ViewModel** | `MVCViewModel.pas` | `IMvcApplication` interface |

The ViewModel acts as both Controller (handling requests) and ViewModel (preparing data for views).

### 19.1.2. Project Structure

```
BlogServer/
├── BlogServer.dpr          # Main program
├── MVCModel.pas            # ORM Model definitions
├── MVCViewModel.pas        # Controller/ViewModel
└── Views/
    ├── Default.html        # Home page
    ├── Error.html          # Error page
    ├── ArticleView.html    # Single article
    ├── AuthorView.html     # Author profile
    ├── Login.html          # Login form
    └── .static/
        ├── blog.css        # Stylesheets
        └── script.js       # Client scripts
```

---

## 19.2. Defining the Model

### 19.2.1. Entity Classes

```pascal
unit MVCModel;

interface

uses
  mormot.core.base,
  mormot.orm.core;

type
  /// Base class with common content fields
  TOrmContent = class(TOrm)  // Use TCreateTime/TModTime properties for timestamps
  private
    fTitle: RawUtf8;
    fContent: RawUtf8;
    fAuthor: TOrmAuthor;
    fAuthorName: RawUtf8;  // Denormalized for performance
  published
    property Title: RawUtf8 index 80 read fTitle write fTitle;
    property Content: RawUtf8 read fContent write fContent;
    property Author: TOrmAuthor read fAuthor write fAuthor;
    property AuthorName: RawUtf8 index 50 read fAuthorName write fAuthorName;
  end;

  /// Blog article
  TOrmArticle = class(TOrmContent)
  private
    fAbstract: RawUtf8;
    fPublishedMonth: Integer;
    fTags: TIntegerDynArray;
  public
    class function CurrentPublishedMonth: Integer;
    class procedure InitializeTable(const Server: IRestOrmServer;
      const FieldName: RawUtf8; Options: TOrmInitializeTableOptions); override;
  published
    property Abstract: RawUtf8 index 1024 read fAbstract write fAbstract;
    property PublishedMonth: Integer read fPublishedMonth write fPublishedMonth;
    property Tags: TIntegerDynArray index 1 read fTags write fTags;
  end;

  /// Comment on an article
  TOrmComment = class(TOrmContent)
  private
    fArticle: TOrmArticle;
  published
    property Article: TOrmArticle read fArticle write fArticle;
  end;

  /// Blog author
  TOrmAuthor = class(TOrm)
  private
    fLogonName: RawUtf8;
    fPasswordHash: RawUtf8;
    fFirstName: RawUtf8;
    fFamilyName: RawUtf8;
    fEmail: RawUtf8;
  published
    property LogonName: RawUtf8 index 30 read fLogonName write fLogonName;
    property PasswordHash: RawUtf8 index 64 read fPasswordHash write fPasswordHash;
    property FirstName: RawUtf8 index 50 read fFirstName write fFirstName;
    property FamilyName: RawUtf8 index 50 read fFamilyName write fFamilyName;
    property Email: RawUtf8 index 100 read fEmail write fEmail;
  end;

  /// Tag for categorizing articles
  TOrmTag = class(TOrm)
  private
    fIdent: RawUtf8;
  published
    property Ident: RawUtf8 index 30 read fIdent write fIdent;
  end;

  /// Full-text search index
  TOrmArticleSearch = class(TOrmFts5)
  private
    fTitle: RawUtf8;
    fAbstract: RawUtf8;
    fContent: RawUtf8;
  published
    property Title: RawUtf8 read fTitle write fTitle;
    property Abstract: RawUtf8 read fAbstract write fAbstract;
    property Content: RawUtf8 read fContent write fContent;
  end;

function CreateBlogModel: TOrmModel;

implementation

class procedure TOrmArticle.InitializeTable(const Server: IRestOrmServer;
  const FieldName: RawUtf8; Options: TOrmInitializeTableOptions);
begin
  inherited;
  if (FieldName = '') or (FieldName = 'PublishedMonth') then
    Server.CreateSqlIndex(TOrmArticle, 'PublishedMonth', False);
end;

class function TOrmArticle.CurrentPublishedMonth: Integer;
var
  Y, M, D: Word;
begin
  DecodeDate(Date, Y, M, D);
  Result := Y * 12 + M;
end;

function CreateBlogModel: TOrmModel;
begin
  Result := TOrmModel.Create(
    [TOrmAuthor, TOrmTag, TOrmArticle, TOrmComment, TOrmArticleSearch],
    'blog');
  // Validation filters
  TOrmArticle.AddFilterNotVoidText(['Title', 'Content']);
  TOrmComment.AddFilterNotVoidText(['Title', 'Content']);
  TOrmTag.AddFilterNotVoidText(['Ident']);
  // FTS without content (external content table)
  Result.Props[TOrmArticleSearch].Fts5WithoutContent(TOrmArticle);
end;

end.
```

### 19.2.2. Model Design Decisions

Key patterns demonstrated:

| Pattern | Example | Benefit |
|---------|---------|---------|
| **Inheritance** | `TOrmContent` base class | Shared fields |
| **Denormalization** | `AuthorName` in content | Avoid joins |
| **Indexed fields** | `PublishedMonth` | Fast queries |
| **Dynamic arrays** | `Tags: TIntegerDynArray` | No pivot table |
| **FTS5 integration** | `TOrmArticleSearch` | Full-text search |

---

## 19.3. Defining the ViewModel/Controller

### 19.3.1. Interface Definition

```pascal
unit MVCViewModel;

interface

uses
  mormot.core.base,
  mormot.core.variants,
  mormot.core.mvc,
  mormot.rest.mvc,
  mormot.orm.core,
  contnrs,
  MVCModel;

type
  IBlogApplication = interface(IMvcApplication)
    ['{73B27C06-9DB9-45A0-BFCD-D23E5C62C113}']
    /// View single article with optional comments
    procedure ArticleView(ID: Integer; var WithComments: Boolean;
      Direction: Integer; out Article: TOrmArticle; out Author: variant;
      out Comments: TObjectList);
    /// View author profile
    procedure AuthorView(var ID: Integer; out Author: TOrmAuthor;
      out Articles: variant);
    /// Login page
    procedure LoginForm(out Msg: RawUtf8);
    /// Process login
    function Login(const LogonName, PlainPassword: RawUtf8): TMvcAction;
    /// Logout
    function Logout: TMvcAction;
    /// Article editor
    procedure ArticleEdit(var ID: Integer; const Title, Content: RawUtf8;
      const ValidationError: variant; out Article: TOrmArticle);
    /// Save article
    function ArticleCommit(ID: Integer;
      const Title, Content: RawUtf8): TMvcAction;
  end;
```

### 19.3.2. Controller Implementation

```pascal
type
  TBlogApplication = class(TMvcApplicationRest, IBlogApplication)
  protected
    fBlogInfo: variant;
    fTagsLookup: variant;
    procedure FlushCache;
    function GetLoggedAuthor: TOrmAuthor;
  public
    procedure Start(aRestModel: TRest; aInterface: PRttiInfo); override;
    // IMvcApplication
    procedure Default(var Scope: variant);
    procedure Error(var Msg: RawUtf8; var Scope: variant);
    // IBlogApplication
    procedure ArticleView(ID: Integer; var WithComments: Boolean;
      Direction: Integer; out Article: TOrmArticle; out Author: variant;
      out Comments: TObjectList);
    procedure AuthorView(var ID: Integer; out Author: TOrmAuthor;
      out Articles: variant);
    procedure LoginForm(out Msg: RawUtf8);
    function Login(const LogonName, PlainPassword: RawUtf8): TMvcAction;
    function Logout: TMvcAction;
    procedure ArticleEdit(var ID: Integer; const Title, Content: RawUtf8;
      const ValidationError: variant; out Article: TOrmArticle);
    function ArticleCommit(ID: Integer;
      const Title, Content: RawUtf8): TMvcAction;
  end;
```

### 19.3.3. Method Implementations

```pascal
procedure TBlogApplication.Start(aRestModel: TRest; aInterface: PRttiInfo);
begin
  inherited Start(aRestModel, TypeInfo(IBlogApplication));
  FlushCache;
end;

procedure TBlogApplication.Default(var Scope: variant);
var
  lastID: TID;
begin
  // Cache blog info
  if VarIsEmpty(fBlogInfo) then
    fBlogInfo := RestModel.Orm.RetrieveDocVariant(
      TOrmBlogInfo, 'ID=?', [1], 'Title,Language,Description');

  // Get recent articles
  lastID := RestModel.Orm.TableMaxID(TOrmArticle);
  Scope := _ObjFast([
    'Info', fBlogInfo,
    'Articles', RestModel.Orm.RetrieveDocVariantArray(
      TOrmArticle, '', 'ID>? order by ID desc limit 20',
      [lastID - 100], 'ID,Title,Abstract,AuthorName,CreatedAt')
  ]);
end;

procedure TBlogApplication.ArticleView(ID: Integer; var WithComments: Boolean;
  Direction: Integer; out Article: TOrmArticle; out Author: variant;
  out Comments: TObjectList);
var
  newID: TID;
const
  WHERE: array[1..2] of RawUtf8 = (
    'ID<? order by ID desc',
    'ID>? order by ID'
  );
begin
  // Navigate to previous/next article
  if Direction in [1, 2] then
    if RestModel.Orm.OneFieldValue(TOrmArticle, 'ID', WHERE[Direction],
         [], [ID], newID) and (newID <> 0) then
      ID := newID;

  // Load article
  RestModel.Orm.Retrieve(ID, Article);
  if Article.ID = 0 then
    raise EMvcApplication.CreateGotoError(HTTP_NOTFOUND);

  // Load author (as variant for flexibility)
  Author := RestModel.Orm.RetrieveDocVariant(
    TOrmAuthor, 'ID=?', [Article.Author.ID], 'FirstName,FamilyName');

  // Optionally load comments
  if WithComments then
  begin
    Comments.Free;
    Comments := RestModel.Orm.RetrieveList(
      TOrmComment, 'Article=? order by ID', [Article.ID]);
  end;
end;

function TBlogApplication.Login(const LogonName, PlainPassword: RawUtf8): TMvcAction;
var
  Author: TOrmAuthor;
begin
  Author := TOrmAuthor.Create(RestModel.Orm, 'LogonName=?', [LogonName]);
  try
    if (Author.ID = 0) or
       not PasswordHashMatch(PlainPassword, Author.PasswordHash) then
      raise EMvcApplication.CreateGotoError('Invalid credentials');

    // Store in session
    CurrentSession['AuthorID'] := Author.ID;
    CurrentSession['LogonName'] := Author.LogonName;

    Result.RedirectToMethodName := 'Default';
  finally
    Author.Free;
  end;
end;

function TBlogApplication.Logout: TMvcAction;
begin
  CurrentSession.Clear;
  Result.RedirectToMethodName := 'Default';
end;
```

---

## 19.4. View Templates

### 19.4.1. Main Layout

```html
<!-- Default.html -->
<!DOCTYPE html>
<html lang="{{Info.Language}}">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>{{Info.Title}}</title>
  <link rel="stylesheet" href="/.static/blog.css">
</head>
<body>
  <header>
    <h1><a href="/blog/default">{{Info.Title}}</a></h1>
    <nav>
      {{#Main.LogonName}}
      <span>Welcome, {{.}}</span>
      <a href="/blog/articleEdit">New Article</a>
      <a href="/blog/logout">Logout</a>
      {{/Main.LogonName}}
      {{^Main.LogonName}}
      <a href="/blog/loginForm">Login</a>
      {{/Main.LogonName}}
    </nav>
  </header>

  <main>
    <p>{{Info.Description}}</p>

    {{#Articles}}
    <article class="summary">
      <h2><a href="/blog/articleView?id={{ID}}">{{Title}}</a></h2>
      <p class="meta">By {{AuthorName}} on {{DateTimeToText CreatedAt}}</p>
      <p>{{Abstract}}</p>
    </article>
    {{/Articles}}

    {{^Articles}}
    <p>No articles published yet.</p>
    {{/Articles}}
  </main>

  <footer>
    <p>&copy; {{Info.Title}}</p>
  </footer>
</body>
</html>
```

### 19.4.2. Article View

```html
<!-- ArticleView.html -->
<!DOCTYPE html>
<html>
<head>
  <title>{{Article.Title}}</title>
  <link rel="stylesheet" href="/.static/blog.css">
</head>
<body>
  <header>
    <nav>
      <a href="/blog/default">&larr; Back to Home</a>
    </nav>
  </header>

  <main>
    <article>
      <h1>{{Article.Title}}</h1>
      <p class="meta">
        By <a href="/blog/authorView?id={{Article.Author}}">
          {{Author.FirstName}} {{Author.FamilyName}}
        </a>
        on {{DateTimeToText Article.CreatedAt}}
      </p>

      <div class="content">
        {{{Article.Content}}}
      </div>

      <nav class="pagination">
        <a href="/blog/articleView?id={{Article.ID}}&direction=1&withComments={{WithComments}}">
          &larr; Previous
        </a>
        <a href="/blog/articleView?id={{Article.ID}}&direction=2&withComments={{WithComments}}">
          Next &rarr;
        </a>
      </nav>
    </article>

    <section class="comments">
      {{#WithComments}}
      <h2>Comments</h2>
      {{#Comments}}
      <div class="comment {{#-odd}}odd{{/-odd}}">
        <h3>{{Title}}</h3>
        <p class="meta">By {{AuthorName}} on {{DateTimeToText CreatedAt}}</p>
        <p>{{Content}}</p>
      </div>
      {{/Comments}}
      {{^Comments}}
      <p>No comments yet.</p>
      {{/Comments}}
      <a href="/blog/articleView?id={{Article.ID}}&withComments=false" class="btn">
        Hide Comments
      </a>
      {{/WithComments}}

      {{^WithComments}}
      <a href="/blog/articleView?id={{Article.ID}}&withComments=true#comments"
         class="btn">
        Show Comments
      </a>
      {{/WithComments}}
    </section>
  </main>
</body>
</html>
```

### 19.4.3. Login Form

```html
<!-- LoginForm.html -->
<!DOCTYPE html>
<html>
<head>
  <title>Login</title>
  <link rel="stylesheet" href="/.static/blog.css">
</head>
<body>
  <main class="login-page">
    <h1>Login</h1>

    {{#Msg}}
    <div class="error">{{.}}</div>
    {{/Msg}}

    <form method="post" action="/blog/login">
      <div class="field">
        <label for="logonName">Username</label>
        <input type="text" id="logonName" name="logonName" required autofocus>
      </div>
      <div class="field">
        <label for="plainPassword">Password</label>
        <input type="password" id="plainPassword" name="plainPassword" required>
      </div>
      <button type="submit">Login</button>
    </form>

    <p><a href="/blog/default">Back to Home</a></p>
  </main>
</body>
</html>
```

---

## 19.5. Hosting the Application

### 19.5.1. Main Server Program

```pascal
program BlogServer;

{$APPTYPE CONSOLE}

uses
  mormot.core.base,
  mormot.core.os,
  mormot.orm.core,
  mormot.rest.sqlite3,
  mormot.rest.http.server,
  mormot.rest.mvc,
  MVCModel,
  MVCViewModel;

var
  Model: TOrmModel;
  Server: TRestServerDB;
  Application: TBlogApplication;
  HttpServer: TRestHttpServer;
begin
  Model := CreateBlogModel;
  try
    Server := TRestServerDB.Create(Model,
      Executable.ProgramFilePath + 'blog.db3');
    try
      Server.DB.Synchronous := smNormal;
      Server.DB.LockingMode := lmExclusive;
      Server.CreateMissingTables;

      Application := TBlogApplication.Create;
      try
        Application.Start(Server, TypeInfo(IBlogApplication));

        // Publish MVC on /blog/*
        TMvcRunOnRestServer.Create(Application,
          Executable.ProgramFilePath + 'Views', Server, '',
          nil, [publishMvcInfo, publishStatic, bypassAuthentication]);

        HttpServer := TRestHttpServer.Create('8092', [Server], '+', useHttpAsync);
        try
          HttpServer.RootRedirectToUri('blog/default');

          WriteLn('Blog server running on http://localhost:8092');
          WriteLn('Press Enter to stop...');
          ReadLn;
        finally
          HttpServer.Free;
        end;
      finally
        Application.Free;
      end;
    finally
      Server.Free;
    end;
  finally
    Model.Free;
  end;
end.
```

### 19.5.2. Integration Options

| Option | Configuration |
|--------|--------------|
| **Same server** | MVC + REST API on same port |
| **Sub-URI** | `/api/*` for REST, `/blog/*` for MVC |
| **Sub-domain** | `api.example.com`, `www.example.com` |

Sub-domain configuration:
```pascal
HttpServer.DomainHostRedirect('api.example.com', 'root');
HttpServer.DomainHostRedirect('www.example.com', 'root/blog');
HttpServer.DomainHostRedirect('example.com', 'root/blog');
```

---

## 19.6. Advanced Features

### 19.6.1. CSRF Protection

mORMot includes built-in CSRF protection:

```html
<form method="post" action="/blog/articleCommit">
  <input type="hidden" name="__csrf" value="{{Main.CsrfToken}}">
  <!-- form fields -->
</form>
```

### 19.6.2. Full-Text Search

```pascal
procedure TBlogApplication.Search(const Query: RawUtf8;
  out Results: variant);
begin
  Results := RestModel.Orm.FtsMatch(
    TOrmArticleSearch, 'Title,Abstract,Content', Query,
    'ID,Title,Abstract', 20);
end;
```

### 19.6.3. Caching

```pascal
procedure TBlogApplication.Default(var Scope: variant);
begin
  // Cache expensive queries
  if VarIsEmpty(fCachedInfo) then
  begin
    fCachedInfo := RestModel.Orm.RetrieveDocVariant(...);
    fCacheTime := GetTickCount64;
  end
  else if GetTickCount64 - fCacheTime > 60000 then  // 1 minute
    FlushCache;

  Scope := fCachedInfo;
end;
```

### 19.6.4. Responsive Design

Use Bootstrap or similar in templates:

```html
<link rel="stylesheet"
      href="https://cdn.jsdelivr.net/npm/bootstrap@5/dist/css/bootstrap.min.css">

<div class="container">
  <div class="row">
    <main class="col-md-8">
      {{#Articles}}...{{/Articles}}
    </main>
    <aside class="col-md-4">
      {{#Tags}}...{{/Tags}}
    </aside>
  </div>
</div>
```

---

## 19.7. Testing MVC Applications

### 19.7.1. Unit Testing Controllers

```pascal
procedure TBlogTest.TestArticleView;
var
  App: TBlogApplication;
  Article: TOrmArticle;
  Author: variant;
  Comments: TObjectList;
  WithComments: Boolean;
begin
  App := TBlogApplication.Create;
  try
    App.Start(TestServer, TypeInfo(IBlogApplication));

    Article := nil;
    Comments := TObjectList.Create;
    try
      WithComments := True;
      App.ArticleView(1, WithComments, 0, Article, Author, Comments);

      Check(Article.ID = 1);
      Check(Article.Title <> '');
      Check(not VarIsEmpty(Author));
    finally
      Article.Free;
      Comments.Free;
    end;
  finally
    App.Free;
  end;
end;
```

### 19.7.2. Integration Testing

```pascal
procedure TBlogTest.TestWebPages;
var
  Client: THttpClientSocket;
  Response: RawUtf8;
begin
  Client := THttpClientSocket.Create('localhost', '8092');
  try
    Check(Client.Get('/blog/default', Response) = HTTP_SUCCESS);
    Check(PosEx('<article', Response) > 0);

    Check(Client.Get('/blog/articleView?id=1', Response) = HTTP_SUCCESS);
    Check(PosEx('</article>', Response) > 0);
  finally
    Client.Free;
  end;
end;
```

---

## Summary

Building MVC web applications with mORMot 2:

1. **Model**: Define `TOrm` classes with proper relationships and indexes
2. **ViewModel**: Create interface with methods for each page/action
3. **Views**: Write Mustache templates with proper data binding
4. **Hosting**: Use `TMvcRunOnRestServer` to publish on HTTP

Key benefits:
- **Type safety**: Interface parameters checked at compile time
- **Separation**: Clean split between data, logic, and presentation
- **Performance**: Efficient ORM queries, caching support
- **Testability**: Controllers testable without HTTP
- **Flexibility**: Same data model for MVC and REST API

---

## Navigation

| Previous | Index | Next |
|----------|-------|------|
| [Chapter 18: The MVC Pattern](mORMot2-SAD-Chapter-18.md) | [Index](mORMot2-SAD-Index.md) | [Chapter 20: Hosting and Deployment](mORMot2-SAD-Chapter-20.md) |
