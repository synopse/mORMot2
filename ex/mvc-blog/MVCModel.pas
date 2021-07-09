/// database Model for the MVCServer BLOG sample
unit MVCModel;

interface

{$I mormot.defines.inc}

{$ifdef FPC}
  {$WARN 5057 off : variable does not seem to be initialized}
  {$WARN 5091 off : variable of a managed type does not seem to be initialized}
{$endif FPC}

uses
  sysutils,
  mormot.core.base,
  mormot.core.data,
  mormot.core.os,
  mormot.core.datetime,
  mormot.core.variants,
  mormot.core.unicode,
  mormot.core.text,
  mormot.crypt.core,
  mormot.orm.core,
  mormot.rest.core,
  mormot.rest.server;

type
  TOrmBlogInfo = class(TOrm)
  private
    fCopyright: RawUtf8;
    fDescription: RawUtf8;
    fTitle: RawUtf8;
    fLanguage: RawUtf8;
    fAbout: RawUtf8;
  published
    property Title: RawUtf8
      index 80 read fTitle write fTitle;
    property Language: RawUtf8
      index 3 read fLanguage write fLanguage;
    property Description: RawUtf8
      index 120 read fDescription write fDescription;
    property Copyright: RawUtf8
      index 80 read fCopyright write fCopyright;
    property About: RawUtf8
      read fAbout write fAbout;
  end;

  TOrmTimeStamped = class(TOrm)
  private
    fCreatedAt: TCreateTime;
    fModifiedAt: TModTime;
  published
    property CreatedAt: TCreateTime
      read fCreatedAt write fCreatedAt;
    property ModifiedAt: TModTime
      read fModifiedAt write fModifiedAt;
  end;

  TOrmSomeone = class(TOrmTimeStamped)
  private
    fFirstName: RawUtf8;
    fFamilyName: RawUtf8;
    fBirthDate: TDateTime;
    fEmail: RawUtf8;
    fVerified: boolean;
    fHashedPassword: RawUtf8;
    fLogonName: RawUtf8;
  public
    procedure SetPlainPassword(const PlainPassword: RawUtf8);
    function CheckPlainPassword(const PlainPassword: RawUtf8): boolean;
    function Name: RawUtf8;
  published
    property LogonName: RawUtf8
      index 30 read fLogonName write fLogonName stored AS_UNIQUE;
    property FirstName: RawUtf8
      index 50 read fFirstName write fFirstName;
    property FamilyName: RawUtf8
      index 50 read fFamilyName write fFamilyName;
    property BirthDate: TDateTime
      read fBirthDate write fBirthDate;
    property Email: RawUtf8
      index 40 read fEmail write fEmail;
    property HashedPassword: RawUtf8
      index 64 read fHashedPassword write fHashedPassword;
    property Verified: boolean
      read fVerified write fVerified;
  end;

  TOrmAuthorRight = (
    canComment,
    canPost,
    canDelete,
    canAdministrate);

  TOrmAuthorRights = set of TOrmAuthorRight;

  TOrmAuthor = class(TOrmSomeone)
  private
    fRights: TOrmAuthorRights;
  public
    class procedure InitializeTable(const Server: IRestOrmServer;
      const FieldName: RawUtf8; Options: TOrmInitializeTableOptions); override;
  published
    property Rights: TOrmAuthorRights
      read fRights write fRights;
  end;

  TOrmContent = class(TOrmTimeStamped)
  private
    fContent: RawUtf8;
    fTitle: RawUtf8;
    fAuthor: TOrmAuthor;
    fAuthorName: RawUtf8;
    fContentHtml: boolean;
  published
    property Title: RawUtf8
      index 120 read fTitle write fTitle;
    property Content: RawUtf8
      read fContent write fContent;
    property ContentHtml: boolean
      read fContentHtml write fContentHtml;
    property Author: TOrmAuthor
      read fAuthor write fAuthor;
    property AuthorName: RawUtf8
      index 50 read fAuthorName write fAuthorName;
  end;

  TOrmTags = object
    Lock: IAutoLocker;
    Lookup: array of record
      Ident: RawUtf8;
      Occurence: integer;
    end;
    OrderID: TIntegerDynArray;
    procedure Init(const aRest: IRestOrm);
    function Get(tagID: integer): RawUtf8;
    procedure SaveOccurence(const aRest: IRestOrm);
    procedure SortTagsByIdent(var Tags: TIntegerDynArray);
    function GetAsDocVariantArray: Variant;
  end;

  TOrmArticle = class(TOrmContent)
  private
    fAbstract: RawUtf8;
    fPublishedMonth: Integer;
    fTags: TIntegerDynArray;
  public
    class function CurrentPublishedMonth: Integer;
    class procedure InitializeTable(const Server: IRestOrmServer;
      const FieldName: RawUtf8; Options: TOrmInitializeTableOptions); override;
    procedure SetPublishedMonth(FromTime: TTimeLog);
    // note: caller should call Tags.SaveOccurence() to update the DB
    procedure TagsAddOrdered(aTagID: Integer; var aTags: TOrmTags);
  published
    property PublishedMonth: Integer
      read fPublishedMonth write fPublishedMonth;
    property abstract: RawUtf8
      read fAbstract write fAbstract;
    // "index 1" below to allow writing e.g. aArticle.DynArray(1).Delete(aIndex)
    property Tags: TIntegerDynArray
      index 1 read fTags write fTags;
  end;

  TOrmArticleSearch = class(TOrmFTS4Porter)
  private
    fContent: RawUtf8;
    fTitle: RawUtf8;
    fAbstract: RawUtf8;
  published
    property Title: RawUtf8
      read fTitle write fTitle;
    property abstract: RawUtf8
      read fAbstract write fAbstract;
    property Content: RawUtf8
      read fContent write fContent;
  end;

  TOrmComment = class(TOrmContent)
  private
    fArticle: TOrmArticle;
  published
    property Article: TOrmArticle
      read fArticle write fArticle;
  end;

  TOrmTag = class(TOrm)
  private
    fIdent: RawUtf8;
    fOccurence: integer;
    fCreatedAt: TCreateTime;
  published
    property Ident: RawUtf8
      index 80 read fIdent write fIdent;
    property Occurence: Integer
      read fOccurence write fOccurence;
    property CreatedAt: TCreateTime
      read fCreatedAt write fCreatedAt;
  end;

function CreateModel: TOrmModel;

procedure DotClearFlatImport(const Rest: IRestOrm; const aFlatFile: RawUtf8;
  var aTagsLookup: TOrmTags; const aDotClearRoot: RawUtf8;
  const aStaticFolder: TFileName);


implementation

uses
  mormot.core.buffers,
  mormot.net.client;


function CreateModel: TOrmModel;
begin
  result := TOrmModel.Create([
    TOrmBlogInfo,
    TOrmAuthor,
    TOrmTag,
    TOrmArticle,
    TOrmComment,
    TOrmArticleSearch],
    'blog');
  TOrmArticle.AddFilterNotVoidText(['Title', 'Content']);
  TOrmComment.AddFilterNotVoidText(['Title', 'Content']);
  TOrmTag.AddFilterNotVoidText(['Ident']);
  result.Props[TOrmArticleSearch].FTS4WithoutContent(TOrmArticle);
end;


{ TOrmSomeone }

const
  SALT = 'mORMot';

function TOrmSomeone.CheckPlainPassword(const PlainPassword: RawUtf8): boolean;
begin
  result := fHashedPassword = Sha256(SALT + LogonName + PlainPassword);
end;

function TOrmSomeone.Name: RawUtf8;
begin
  result := FirstName + ' ' + FamilyName;
end;

procedure TOrmSomeone.SetPlainPassword(const PlainPassword: RawUtf8);
begin
  fHashedPassword := Sha256(SALT + LogonName + PlainPassword);
end;


{ TOrmAuthor }

class procedure TOrmAuthor.InitializeTable(const Server: IRestOrmServer;
  const FieldName: RawUtf8; Options: TOrmInitializeTableOptions);
var
  Auth: TOrmAuthor;
begin
  inherited InitializeTable(Server, FieldName, Options);
  if FieldName = '' then
  begin // new table -> create default Author
    Auth := TOrmAuthor.Create;
    try
      Auth.LogonName := 'synopse';
      Auth.SetPlainPassword('synopse'); // please customize it
      Auth.FamilyName := 'Synopse';
      Auth.Verified := true;
      Auth.Rights := [Low(TOrmAuthorRight)..High(TOrmAuthorRight)];
      Server.Add(Auth, true);
    finally
      Auth.Free;
    end;
  end;
end;


{ TOrmArticle }

class function TOrmArticle.CurrentPublishedMonth: Integer;
var
  Y, M, D: word;
begin
  DecodeDate(NowUTC, Y, M, D);
  result := integer(Y) * 12 + integer(M) - 1;
end;

class procedure TOrmArticle.InitializeTable(const Server: IRestOrmServer;
  const FieldName: RawUtf8; Options: TOrmInitializeTableOptions);
begin
  inherited;
  if (FieldName = '') or
     (FieldName = 'PublishedMonth') then
    Server.CreateSQLIndex(TOrmArticle, 'PublishedMonth', false);
end;

procedure TOrmArticle.SetPublishedMonth(FromTime: TTimeLog);
begin
  fPublishedMonth :=
    TTimeLogBits(FromTime).Year * 12 + TTimeLogBits(FromTime).Month - 1;
end;

procedure TOrmArticle.TagsAddOrdered(aTagID: Integer; var aTags: TOrmTags);
begin
  if (aTagID < length(aTags.Lookup)) and
     AddInteger(fTags, aTagID, true) then
    with aTags.Lock.ProtectMethod do
    begin
      inc(aTags.Lookup[aTagID - 1].Occurence);
      aTags.SortTagsByIdent(fTags);
    end;
end;


{ TOrmTags }

function TOrmTags.Get(tagID: integer): RawUtf8;
begin
  if (tagID > 0) and
     (tagID <= Length(Lookup)) then
    result := Lookup[tagID - 1].Ident
  else
    result := '';
end;

function TOrmTags.GetAsDocVariantArray: Variant;
var
  i, ndx: Integer;
begin
  TDocVariant.NewFast(result);
  with Lock.ProtectMethod do
    for i := 0 to length(OrderID) - 1 do
    begin
      ndx := OrderID[i] - 1;
      with Lookup[ndx] do
        if Occurence > 0 then
          TDocVariantData(result).AddItem(
            _ObjFast(['tagID', ndx + 1,
                      'ident', Ident,
                      'occurence', Occurence]));
    end;
end;

procedure TOrmTags.Init(const aRest: IRestOrm);
var
  tag: TOrmTag;
  ID, count, maxID: integer;
begin
  Finalize(Lookup);
  if Lock = nil then
    Lock := TAutoLocker.Create;
  with Lock.ProtectMethod,
    TAutoFree.One(tag, TOrmTag.CreateAndFillPrepare(
      aRest, 'order by Ident', 'RowID,Ident,Occurence')) do
  begin
    count := tag.FillTable.RowCount;
    if count = 0 then
      exit;
    SetLength(OrderID, count);
    count := 0;
    maxID := 0;
    while tag.FillOne do
    begin
      ID := tag.ID;
      OrderID[count] := ID;
      inc(count);
      if ID > maxID then
        maxID := ID;
    end;
    SetLength(Lookup, maxID);
    tag.FillRewind;
    while tag.FillOne do
      with Lookup[tag.ID - 1] do
      begin
        Ident := tag.Ident;
        Occurence := tag.Occurence;
      end;
  end;
end;

procedure TOrmTags.SaveOccurence(const aRest: IRestOrm);
var
  tag: TOrmTag;
  batch: TRestBatch;
begin
  with TAutoFree.Several([
         @tag,   TOrmTag.CreateAndFillPrepare(aRest, '', 'RowID,Occurence'),
         @batch, TRestBatch.Create(aRest, TOrmTag, 1000)]),
       Lock.ProtectMethod do
  begin
    while tag.FillOne do
    begin
      if tag.ID <= length(Lookup) then
        if Lookup[tag.ID - 1].Occurence <> tag.Occurence then
        begin
          tag.Occurence := Lookup[tag.ID - 1].Occurence;
          batch.Update(tag); // will update only Occurence field
        end;
    end;
    aRest.BatchSend(batch);
  end;
end;

procedure TOrmTags.SortTagsByIdent(var Tags: TIntegerDynArray);
var
  new: TIntegerDynArray;
  i, n: integer;
begin // Lock.ProtectMethod made by caller
  n := length(Tags);
  if n = 1 then
    exit;
  SetLength(new, n);
  QuickSortInteger(pointer(Tags), 0, n - 1);
  n := 0;
  for i := 0 to length(OrderID) - 1 do
    if FastFindIntegerSorted(Tags, OrderID[i]) >= 0 then
    begin
      new[n] := OrderID[i];
      inc(n);
    end;
  assert(n = length(Tags));
  Tags := new;
end;

type
  /// used to store a DotClear flat export data section
  TDotClearTable = class(TOrmTable)
  protected
    fText: RawUtf8;
    fFields: TRawUtf8DynArray;
    fJsonData: TOrmTableJsonDataArray;
    fName: RawUtf8;
  public
    /// compute a section content
    constructor Create(var Text: PUtf8Char);
    /// parse a DotClear flat export text file, and create a list of sections
    // - you can later on use aList.GetObjectByName('post') as TDotClearTable
    // to access a given section
    class function Parse(const aFlatExport: RawUtf8): TRawUtf8List;
    /// the name of the section, e.g. 'category' or 'post'
    property Name: RawUtf8 read fName;
  end;

constructor TDotClearTable.Create(var Text: PUtf8Char);
var
  P, D: PUtf8Char;
  f, r: integer;
begin
  fName := GetNextItem(Text, ' ');
  CSVToRawUtf8DynArray(Pointer(GetNextItem(Text, ']')), fFields);
  fFieldCount := length(fFields);
  Text := GotoNextLine(Text);
  P := pointer(Text);
  while (Text <> nil) and (Text^ = '"') do
  begin
    Text := GotoNextLine(Text);
    inc(fRowCount);
  end;
  if Text = nil then
    fText := P
  else
    SetString(fText, PAnsiChar(P), Text - P);
  SetLength(fJsonData, fFieldCount * (fRowCount + 1));
  fData := pointer(fJsonData);
  for f := 0 to fFieldCount - 1 do
  begin
    SetResultsSafe(f, pointer(fFields[f]));
    SetFieldType(f, sftUTF8Text);
  end;
  for r := 1 to fRowCount do
  begin
    assert(P^ = '"');
    inc(P);
    for f := 0 to fFieldCount - 1 do
    begin
      SetResultsSafe(r * fFieldCount + f, P);
      D := P;
      while P^ <> '"' do
        if P^ = #0 then
          exit
        else
        begin
          if P^ = '\' then
          begin
            inc(P);
            case P^ of
              'r':
                D^ := #13;
              'n':
                D^ := #10;
              '\':
                D^ := '\';
              '"':
                D^ := '"';
            else
              begin
                D^ := '\';
                inc(D);
                D^ := P^;
              end;
            end;
          end
          else
            D^ := P^;
          inc(P);
          inc(D);
        end;
      D^ := #0;
      inc(P);
      if (P[0] = ',') and
         (P[1] = '"') then
        inc(P, 2);
    end;
    P := GotoNextLine(P);
  end;
end;

class function TDotClearTable.Parse(const aFlatExport: RawUtf8): TRawUtf8List;
var
  P: PUtf8Char;
  T: TDotClearTable;
begin
  result := TRawUtf8List.Create(true);
  P := pointer(aFlatExport);
  repeat
    while (P <> nil) and
          (P^ <> '[') do
      P := GotoNextLine(P);
    if P = nil then
      exit;
    inc(P);
    T := TDotClearTable.Create(P);
    result.AddObject(T.Name, T);
    //FileFromString(T.GetODSDocument,TFileName(T.Name)+'.ods');
  until P = nil;
end;

procedure DotClearFlatImport(const Rest: IRestOrm; const aFlatFile: RawUtf8;
  var aTagsLookup: TOrmTags; const aDotClearRoot: RawUtf8;
  const aStaticFolder: TFileName);
var
  T, tagTable, postTable: TDotClearTable;
  data, urls: TRawUtf8List;
  info: TOrmBlogInfo;
  article: TOrmArticle;
  comment: TOrmComment;
  tag: TOrmTag;
  tags: TRawUtf8DynArray;
  tagID: TIDDynArray;
  tagsCount: integer;
  batch: TRestBatch;
  PublicFolder: TFileName;
  notfound: TRawUtf8DynArray;
  r, ndx, post_url, meta_id, meta_type, tag_post_id, postID, post_id: integer;

  function FixLinks(P: PUtf8Char): RawUtf8;
  var
    url, urlnoparam: RawUtf8;
    urlLen: integer;

    procedure GetUrl(H: PUtf8Char);
    var
      i: integer;
    begin
      url := GetNextItem(H, '"');
      urlLen := length(url);
      url := UrlDecode(url);
      i := PosExChar('?', url);
      if i > 0 then
        urlnoparam := copy(url, 1, i - 1)
      else
        urlnoparam := url;
    end;

  var
    PB, H: PUtf8Char;
    i, status: integer;
    pic: RawByteString;
    FN: TFileName;
    tag: (href, src);
    tmp: TTextWriterStackBuffer;
  begin
    tag := href;
    with TBaseWriter.CreateOwnedStream(tmp) do
    try
      PB := P;
      while P <> nil do
      begin
        while P^ <> ' ' do
          if P^ = #0 then
            break
          else
            inc(P);
        if P^ = #0 then
          break;
        inc(P);
        H := P; // makes compiler happy
        if IdemPChar(P, 'HREF="') then
        begin
          tag := href;
          inc(H, 6);
        end
        else if IdemPChar(P, 'SRC="') then
        begin
          tag := src;
          inc(H, 5);
        end
        else
          continue;
        AddNoJSONEscape(PB, H - PB);
        P := H;
        if IdemPChar(P, 'HTTP://SYNOPSE.INFO') then
        begin
          AddShort('https://synopse.info');
          inc(P, 19);
        end
        else if P^ = '/' then
        begin
          if IdemPChar(P + 1, 'POST/') then
          begin
            GetUrl(P + 6);
            i := urls.IndexOf(urlnoparam);
            if i >= 0 then
            begin
              AddShort('articleView?id=');
              Add(i + 1);
              inc(P, urlLen + 6);
            end
            else
              AddString(aDotClearRoot);
          end
          else if IdemPChar(P + 1, 'PUBLIC/') then
          begin
            if PublicFolder <> '' then
            begin
              GetUrl(P + 8);
              FN := PublicFolder + UTF8ToString(StringReplaceChars(url, '/', PathDelim));
              EnsureDirectoryExists(ExtractFilePath(FN));
              if not FileExists(FN) then
                FileFromString(HttpGet(aDotClearRoot + '/public/' + url, nil,
                  {forceNotSocket=}true), FN);
              AddShort('.static/public/'); // will append 'fullfilename">...'
              inc(P, 8);
            end
            else
              AddString(aDotClearRoot);
          end;
        end
        else if (tag = src) and IdemPChar(P, 'HTTP') then
        begin
          GetUrl(P);
          if IdemFileExts(pointer(urlnoparam), ['.JP', '.PNG', '.GIF', '.SVG']) >= 0 then
          begin
            if FindRawUtf8(notfound, url) < 0 then
            begin
              FN := 'ext-' + Ansi7ToString(MD5(url)) + SysUtils.lowercase(ExtractFileExt
                (UTF8ToString(urlnoparam)));
              if not FileExists(PublicFolder + FN) then
              begin
                write(urlnoparam);
                pic := HttpGet(url, nil, {forceNotSocket=}true, @status);
                if (status <> 200) or (pic = '') or (PosExChar(#0, pic) = 0) or
                  {%H-}IdemPChar(pointer(pic), '<!DOCTYPE') then
                begin
                  if {%H-}IdemPChar(pointer(url), 'HTTP:') then
                  begin
                    pic := url;
                    insert('s', pic, 5);
                    write(' https? ');
                    pic := HttpGet(pic, nil, {forceNotSocket=}true, @status);
                    if (status <> 200) or (pic = '') or (PosExChar(#0, pic) = 0)
                      or                       {%H-}IdemPChar(pointer(pic),
                      '<!DOCTYPE') then
                      pic := '';
                  end;
                end;
                if pic = '' then
                begin
                  AddRawUtf8(notfound, url);
                  writeln(': KO (', status, ')');
                end
                else
                begin
                  writeln(': ', status, ' = ', FN);
                  FileFromString(pic, PublicFolder + FN);
                end;
              end;
              AddShort('.static/public/');
              AddNoJSONEscapeString(FN);
              inc(P, urlLen);
            end;
          end;
        end;
        PB := P;
      end;
      AddNoJSONEscape(PB);
      SetText(result);
    finally
      Free;
    end;
  end;

var
  {%H-}auto1, {%H-}auto2: IAutoFree; // mandatory only for FPC
begin
  if aStaticFolder <> '' then
  begin
    PublicFolder :=
      IncludeTrailingPathDelimiter(aStaticFolder) + 'public' + PathDelim;
    EnsureDirectoryExists(PublicFolder);
    HTTP_DEFAULT_RESOLVETIMEOUT := 1000; // don't wait forever
    HTTP_DEFAULT_CONNECTTIMEOUT := 1000;
    HTTP_DEFAULT_RECEIVETIMEOUT := 2000;
  end;
  auto1 := TAutoFree.Several([
    @data,    TDotClearTable.Parse(aFlatFile),
    @urls,    TRawUtf8ListHashed.Create,
    @batch,   TRestBatch.Create(Rest, TOrmTag, 5000)]);
  auto2 := TOrm.AutoFree([ // avoid several try..finally
    @info,    TOrmBlogInfo,
    @article, TOrmArticle,
    @comment, TOrmComment,
    @tag,     TOrmTag]);
  T := data.GetObjectFrom('setting');
  Rest.Retrieve('', info);
  info.Copyright := VariantToUTF8(
    T.GetValue('setting_id', 'copyright_notice', 'setting_value'));
  if info.ID = 0 then
    Rest.Add(info, true)
  else
    Rest.Update(info);
  tagTable := data.GetObjectFrom('meta');
  tagsCount := 0;
  meta_id := tagTable.FieldIndexExisting('meta_id');
  meta_type := tagTable.FieldIndexExisting('meta_type');
  for r := 1 to tagTable.RowCount do
    if tagTable.GetU(r, meta_type) = 'tag' then
      AddSortedRawUtf8(tags, tagsCount,
        tagTable.GetU(r, meta_id), nil, -1, @StrIComp);
  for r := 0 to tagsCount - 1 do
  begin
    tag.Ident := tags[r];
    batch.Add(tag, true);
  end;
  Rest.BatchSend(batch, tagID);
  aTagsLookup.Init(Rest); // reload after initial fill
  batch.Reset(TOrmArticle, 5000);
  tag_post_id := tagTable.FieldIndexExisting('post_id');
  T.SortFields(tag_post_id, true, nil, sftInteger);
  postTable := data.GetObjectFrom('post');
  postTable.SortFields('post_creadt', true, nil, sftDateTime);
  post_id := postTable.FieldIndexExisting('post_id');
  post_url := postTable.FieldIndexExisting('post_url');
  if postTable.Step(true) then
    repeat
      urls.Add(postTable.FieldBuffer(post_url));
    until not postTable.Step;
  article.Author := TOrmAuthor(1);
  article.AuthorName := 'synopse';
  article.ContentHtml := true;
  for r := 1 to postTable.RowCount do
  begin
    article.Title := postTable.GetU(r, 'post_title');
    article.abstract := FixLinks(postTable.Get(r, 'post_excerpt_xhtml'));
    article.Content := FixLinks(postTable.Get(r, 'post_content_xhtml'));
    if article.abstract = '' then
    begin
      article.abstract := article.Content;
      article.Content := '';
    end;
    article.CreatedAt := Iso8601ToTimeLog(postTable.GetU(r, 'post_creadt'));
    article.ModifiedAt := Iso8601ToTimeLog(postTable.GetU(r, 'post_upddt'));
    article.SetPublishedMonth(article.CreatedAt);
    postID := postTable.GetAsInteger(r, post_id);
    article.Tags := nil;
    if tagTable.Step(true) then
      repeat
        if tagTable.FieldAsInteger(tag_post_id) = postID then
        begin
          ndx := FastFindPUtf8CharSorted(
            pointer(tags), high(tags), tagTable.FieldBuffer(meta_id), @StrIComp);
          if ndx >= 0 then
            article.TagsAddOrdered(tagID[ndx], aTagsLookup);
        end;
      until not tagTable.Step;
    batch.Add(article, true, false, [], true);
  end;
  Rest.BatchSend(batch);
  aTagsLookup.SaveOccurence(Rest);
  writeln(#13#10'-- import finished!');
end;

end.

