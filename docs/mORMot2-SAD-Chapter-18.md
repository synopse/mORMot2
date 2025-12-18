# 18. The MVC Pattern

*Separating Concerns for Better Architecture*

The Model-View-Controller (MVC) pattern separates application concerns into three interconnected components. mORMot 2 provides a complete MVC implementation for both desktop and web applications, with the Mustache template engine as its primary view technology.

---

## 18.1. MVC Architecture Overview

### 18.1.1. The Three Components

| Component | Responsibility | mORMot Implementation |
|-----------|---------------|----------------------|
| **Model** | Data and business logic | `TOrm` classes, `TOrmModel`, `TRest` |
| **View** | Presentation layer | Mustache templates (`.html`), Desktop UI |
| **Controller** | User input handling | `IMvcApplication` interface methods |

### 18.1.2. Benefits of MVC

- **Separation of concerns**: Each component has a single responsibility
- **Testability**: Logic can be tested without UI
- **Reusability**: Same model for different views
- **Maintainability**: Changes in one layer don't affect others
- **Team collaboration**: Designers work on views, developers on logic

### 18.1.3. mORMot's MVC Stack

```
┌──────────────────────────────────────────────┐
│                    View                      │
│  Mustache templates (.html) or Desktop UI    │
├──────────────────────────────────────────────┤
│                 Controller                   │
│  IMvcApplication interface implementation    │
├──────────────────────────────────────────────┤
│                   Model                      │
│     TOrm classes + TRest ORM/SOA             │
└──────────────────────────────────────────────┘
```

---

## 18.2. The Model Layer

### 18.2.1. ORM as the Model

In mORMot, the Model is your ORM classes and the `TOrmModel`:

```pascal
type
  TOrmAuthor = class(TOrm)
  private
    fName: RawUtf8;
    fEmail: RawUtf8;
  published
    property Name: RawUtf8 index 100 read fName write fName;
    property Email: RawUtf8 index 100 read fEmail write fEmail;
  end;

  TOrmArticle = class(TOrm)  // Use TCreateTime/TModTime properties for timestamps
  private
    fTitle: RawUtf8;
    fContent: RawUtf8;
    fAuthor: TOrmAuthor;
    fPublished: Boolean;
  published
    property Title: RawUtf8 index 200 read fTitle write fTitle;
    property Content: RawUtf8 read fContent write fContent;
    property Author: TOrmAuthor read fAuthor write fAuthor;
    property Published: Boolean read fPublished write fPublished;
  end;

function CreateBlogModel: TOrmModel;
begin
  Result := TOrmModel.Create([TOrmAuthor, TOrmArticle], 'blog');
end;
```

### 18.2.2. Model Best Practices

1. **Keep models focused**: Each `TOrm` class represents one entity
2. **Use class hierarchy**: Share common properties via inheritance
3. **Define indexes**: Override `InitializeTable` for performance
4. **Add validation**: Use `AddFilterNotVoidText` and similar methods
5. **Document relationships**: One-to-many, many-to-many via properties

```pascal
class procedure TOrmArticle.InitializeTable(const Server: IRestOrmServer;
  const FieldName: RawUtf8; Options: TOrmInitializeTableOptions);
begin
  inherited;
  if (FieldName = '') or (FieldName = 'Author') then
    Server.CreateSqlIndex(TOrmArticle, 'Author', False);
end;
```

---

## 18.3. The View Layer

mORMot provides two view technologies:

### 18.3.1. Desktop Views (RTTI-based)

For desktop applications, UI can be generated from RTTI:

```pascal
type
  TBlogAction = (
    baNewArticle,
    baEditArticle,
    baDeleteArticle,
    baQuit
  );

const
  ToolbarActions: array[0..1] of set of TBlogAction = (
    [baNewArticle, baEditArticle, baDeleteArticle],
    [baQuit]
  );
```

Benefits:
- **Camel Case to labels**: `baNewArticle` becomes "New Article"
- **Automatic i18n**: Labels can be translated
- **Type safety**: Compiler checks action names
- **Code-first**: UI defined in code, not visual designer

### 18.3.2. Web Views (Mustache Templates)

For web applications, Mustache templates provide logic-less rendering:

```html
<!-- ArticleView.html -->
<article>
  <h1>{{Article.Title}}</h1>
  <p class="meta">By {{Author.Name}} on {{Article.CreatedAt}}</p>
  <div class="content">
    {{{Article.Content}}}
  </div>

  {{#WithComments}}
  <section class="comments">
    <h2>Comments</h2>
    {{#Comments}}
    <div class="comment">
      <strong>{{Title}}</strong>
      <p>{{Content}}</p>
    </div>
    {{/Comments}}
    {{^Comments}}
    <p>No comments yet.</p>
    {{/Comments}}
  </section>
  {{/WithComments}}
</article>
```

---

## 18.4. The Mustache Template Engine

### 18.4.1. Why Mustache?

Mustache is a **logic-less** template system:
- No `if` statements or loops in templates
- Data context determines what renders
- Clean separation of logic and presentation
- Same templates work client-side (JavaScript) and server-side

### 18.4.2. Basic Syntax

| Tag | Purpose |
|-----|---------|
| `{{variable}}` | HTML-escaped output |
| `{{{variable}}}` | Unescaped output (raw HTML) |
| `{{#section}}...{{/section}}` | Section (conditional/loop) |
| `{{^section}}...{{/section}}` | Inverted section (if false/empty) |
| `{{! comment }}` | Comment (not rendered) |
| `{{> partial}}` | Include partial template |

### 18.4.3. Variables

```mustache
Hello {{name}}!
You have {{count}} messages.
```

With context:
```json
{"name": "John", "count": 5}
```

Renders:
```
Hello John!
You have 5 messages.
```

### 18.4.4. Sections

Sections render based on the truthiness of the key:

```mustache
{{#hasMessages}}
  <ul>
  {{#messages}}
    <li>{{subject}}</li>
  {{/messages}}
  </ul>
{{/hasMessages}}

{{^hasMessages}}
  <p>No messages.</p>
{{/hasMessages}}
```

**Section behavior:**
- `false` or empty array `[]`: Block not rendered
- Non-false value: Block rendered once (context switches to value)
- Non-empty array: Block rendered for each item

### 18.4.5. mORMot Extensions

mORMot adds useful features to standard Mustache:

| Tag | Description |
|-----|-------------|
| `{{.}}` | Current context value |
| `{{-index}}` | Current iteration index (1-based) |
| `{{-index0}}` | Current iteration index (0-based) |
| `{{#-first}}...{{/-first}}` | First item in loop |
| `{{#-last}}...{{/-last}}` | Last item in loop |
| `{{#-odd}}...{{/-odd}}` | Odd items (for zebra striping) |
| `{{<partial}}...{{/partial}}` | Inline partial definition |
| `{{"text}}` | Translatable text |
| `{{helperName value}}` | Expression helpers |

### 18.4.6. Expression Helpers

Register helpers for common transformations:

```pascal
// Built-in helpers
{{DateTimeToText Article.CreatedAt}}
{{TimeLogToText Article.ModifiedAt}}
{{EnumTrim Article.Status}}  // Removes type prefix (e.g., "osActive" → "Active")
{{BlobToBase64 Article.Image}}

// Custom helper registration
TSynMustache.HelpersGetStandardList.Add(
  'FormatCurrency',
  procedure(const Value: variant; out Result: variant)
  begin
    Result := FormatFloat('$#,##0.00', Value);
  end
);
```

### 18.4.7. ORM Integration

Register ORM classes as expression helpers:

```pascal
RegisterExpressionHelpersForTables(Views, RestServer, [TOrmAuthor, TOrmArticle]);
```

Then in templates:
```mustache
{{#TOrmAuthor Article.AuthorID}}
  Author: {{Name}} ({{Email}})
{{/TOrmAuthor Article.AuthorID}}
```

The `TOrmAuthor` helper automatically loads the record by ID.

---

## 18.5. The Controller Layer

### 18.5.1. Defining Commands

The Controller is defined as an interface:

```pascal
type
  IMvcApplication = interface(IInvokable)
    ['{C48718BF-861B-448A-B593-8012DB51E15D}']
    procedure Default(var Scope: variant);
    procedure Error(var Msg: RawUtf8; var Scope: variant);
  end;

  IBlogApplication = interface(IMvcApplication)
    ['{12345678-1234-1234-1234-123456789ABC}']
    procedure ArticleView(ID: Integer; var WithComments: Boolean;
      out Article: TOrmArticle; out Author: variant; out Comments: TObjectList);
    procedure AuthorView(var ID: Integer; out Author: TOrmAuthor;
      out Articles: variant);
    function Login(const LogonName, Password: RawUtf8): TMvcAction;
    function Logout: TMvcAction;
  end;
```

### 18.5.2. Parameter Directions

| Direction | Meaning |
|-----------|---------|
| `const` | Input only (from URL/form) |
| `var` | Input and output |
| `out` | Output only (to view) |
| `function: TMvcAction` | Redirect action |

### 18.5.3. URI Mapping

Methods map to URIs automatically:

| Method | URI |
|--------|-----|
| `Default` | `/blog/default` |
| `Error` | `/blog/error` |
| `ArticleView` | `/blog/articleView?id=123&withComments=true` |
| `Login` | `/blog/login?logonName=john&password=xxx` |

### 18.5.4. Implementing the Controller

```pascal
type
  TBlogApplication = class(TMvcApplicationRest, IBlogApplication)
  public
    procedure Start(aRestModel: TRest; aInterface: PRttiInfo); override;
    procedure Default(var Scope: variant);
    procedure ArticleView(ID: Integer; var WithComments: Boolean;
      out Article: TOrmArticle; out Author: variant; out Comments: TObjectList);
    function Login(const LogonName, Password: RawUtf8): TMvcAction;
  end;

procedure TBlogApplication.ArticleView(ID: Integer; var WithComments: Boolean;
  out Article: TOrmArticle; out Author: variant; out Comments: TObjectList);
begin
  RestModel.Orm.Retrieve(ID, Article);
  if Article.ID = 0 then
    raise EMvcApplication.CreateGotoError(HTTP_NOTFOUND);

  Author := RestModel.Orm.RetrieveDocVariant(
    TOrmAuthor, 'ID=?', [Article.Author.ID], 'Name,Email');

  if WithComments then
  begin
    Comments.Free;
    Comments := RestModel.Orm.RetrieveList(TOrmComment, 'Article=?', [ID]);
  end;
end;

function TBlogApplication.Login(const LogonName, Password: RawUtf8): TMvcAction;
begin
  if ValidateCredentials(LogonName, Password) then
  begin
    CurrentSession.User := LogonName;
    Result.RedirectToMethodName := 'Default';
  end
  else
    raise EMvcApplication.CreateGotoError('Invalid credentials');
end;
```

### 18.5.5. TMvcAction for Redirects

Methods returning `TMvcAction` don't render a view directly:

```pascal
type
  TMvcAction = record
    RedirectToMethodName: RawUtf8;
    RedirectToMethodParameters: RawUtf8;
    ReturnedStatus: Cardinal;
  end;

function TBlogApplication.Logout: TMvcAction;
begin
  CurrentSession.Clear;
  Result.RedirectToMethodName := 'Default';
  // or: Result.ReturnedStatus := HTTP_TEMPORARYREDIRECT;
end;
```

### 18.5.6. Error Handling

Raise `EMvcApplication` to redirect to error page:

```pascal
// Redirect to error page with message
raise EMvcApplication.CreateGotoError('Article not found');

// Redirect to error page with HTTP status
raise EMvcApplication.CreateGotoError(HTTP_NOTFOUND);

// Redirect to another method
raise EMvcApplication.CreateGotoMethod('Login');
```

---

## 18.6. View Templates

### 18.6.1. Template Location

By default, templates are in a `Views` subfolder:

```
MyApp/
├── MyApp.exe
└── Views/
    ├── Default.html
    ├── Error.html
    ├── ArticleView.html
    ├── AuthorView.html
    └── _partials/
        ├── header.html
        └── footer.html
```

### 18.6.2. Template Structure

```html
<!-- Default.html -->
{{>header}}

<main>
  <h1>{{Scope.Title}}</h1>

  {{#Scope.Articles}}
  <article>
    <h2><a href="/blog/articleView?id={{ID}}">{{Title}}</a></h2>
    <p>{{Abstract}}</p>
    <small>{{DateTimeToText CreatedAt}}</small>
  </article>
  {{/Scope.Articles}}

  {{^Scope.Articles}}
  <p>No articles published yet.</p>
  {{/Scope.Articles}}
</main>

{{>footer}}
```

### 18.6.3. Partials

Define reusable template fragments:

```html
<!-- _partials/header.html -->
<!DOCTYPE html>
<html>
<head>
  <title>{{Main.Title}} - My Blog</title>
  <link rel="stylesheet" href="/static/style.css">
</head>
<body>
  <nav>
    <a href="/blog/default">Home</a>
    {{#Main.User}}
    <a href="/blog/logout">Logout ({{.}})</a>
    {{/Main.User}}
    {{^Main.User}}
    <a href="/blog/login">Login</a>
    {{/Main.User}}
  </nav>
```

### 18.6.4. Static Files

Serve static content alongside templates:

```pascal
// Enable static file serving
fMainRunner := TMvcRunOnRestServer.Create(Self, '', Server, 'blog', nil,
  [publishMvcInfo, publishStatic, bypassAuthentication]);
```

Static files served from `.static` subfolder at `/blog/.static/*`.

---

## 18.7. Session Management

### 18.7.1. Cookie-Based Sessions

MVC applications use cookie sessions:

```pascal
type
  TMvcSessionWithRestServer = class(TMvcSessionWithCookies)
  protected
    function GetCookie(out Value: PUtf8Char): Integer; override;
    procedure SetCookie(const Value: RawUtf8); override;
  end;
```

### 18.7.2. Session Data

Access session data in controllers:

```pascal
procedure TBlogApplication.Default(var Scope: variant);
begin
  // Read session data
  if CurrentSession['LoggedIn'] then
    Scope := _ObjFast(['User', CurrentSession['UserName']]);
end;

function TBlogApplication.Login(const LogonName, Password: RawUtf8): TMvcAction;
begin
  if ValidateUser(LogonName, Password) then
  begin
    CurrentSession['LoggedIn'] := True;
    CurrentSession['UserName'] := LogonName;
    CurrentSession['UserID'] := GetUserID(LogonName);
    Result.RedirectToMethodName := 'Default';
  end;
end;
```

---

## 18.8. Debugging MVC Applications

### 18.8.1. The mvc-info Page

Access `/blog/mvc-info` to see:
- All registered commands
- Parameter types and directions
- View data context structure

```
/blog/Default?Scope=..[variant]..
/blog/Error?Msg=..[string]..&Scope=..[variant]..
/blog/ArticleView?ID=..[integer]..&WithComments=..[boolean]..

ArticleView context:
{{WithComments}}: boolean
{{Article}}: TOrmArticle
{{Author}}: variant
{{Comments}}: TObjectList
```

### 18.8.2. JSON Context View

Append `/json` to any URL to see the raw data context:

```
/blog/articleView?id=123           → HTML page
/blog/articleView/json?id=123      → JSON data context
```

---

## Summary

The MVC pattern in mORMot 2 provides:

- **Model**: `TOrm` classes with full ORM capabilities
- **View**: Mustache templates for logic-less rendering
- **Controller**: Interface methods mapped to URIs

Key features:
- **Automatic routing**: Methods become URIs
- **Type-safe parameters**: Compile-time checking
- **Expression helpers**: Transform data in templates
- **Session management**: Cookie-based state
- **Debug tools**: mvc-info and JSON context views

This architecture cleanly separates concerns while providing the power of mORMot's ORM and SOA capabilities.

---

## Navigation

| Previous | Index | Next |
|----------|-------|------|
| [Chapter 17: Cross-Platform Clients](mORMot2-SAD-Chapter-17.md) | [Index](mORMot2-SAD-Index.md) | [Chapter 19: MVC/MVVM Web Applications](mORMot2-SAD-Chapter-19.md) |
