/// MVC Web Server over mORMot's REST and Mustache
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.rest.mvc;

{
  *****************************************************************************

   Model-View-Controller (MVC) pattern and Mustache over TRestServer and TOrm
   - MVC/MVVM Application over a TRestServer
   - Expression Helpers for the ORM Data Model

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  variants,
  contnrs,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.core.variants,
  mormot.core.log,
  mormot.core.data,
  mormot.core.interfaces,
  mormot.core.mustache,
  mormot.core.mvc,
  mormot.orm.base,
  mormot.orm.core,
  mormot.rest.core,
  mormot.rest.server;


{ ******************************** MVC/MVVM application over a TRestServer }

type
  /// parent class to implement a MVC/MVVM application over a TRestServer
  // - your inherited class should also implement an interface inheriting from
  // IMvcApplication to define the various commands/uri of the application
  // - here the Model would be a TRest instance, Views will be defined by
  // TMvcViewsAbstract (e.g. TMvcViewsMustache), and the ViewModel/Controller
  // will be implemented with IMvcApplication methods of the inherited class
  TMvcApplicationRest = class(TMvcApplication)
  protected
    fRestModel: TRest;
    fRestServer: TRestServer;
    /// compute the data context e.g. for the /mvc-info URI
    procedure GetMvcInfo(out info: variant); override;
  public
    /// initialize the instance of the MVC/MVVM application
    // - define the associated REST instance, and the interface definition for
    // application commands
    // - is not defined as constructor, since this TInjectableObject may
    // expect injection using the CreateInjected() constructor
    procedure Start(aRestModel: TRest; aInterface: PRttiInfo); virtual;
    /// read-only access to the associated mORMot REST instance implementing the
    // MVC data Model of the application
    // - you could use RestModel.Orm to access the associated database
    // - is a TRestServer instance e.g. for TMvcRunOnRestServer
    property RestModel: TRest
      read fRestModel;
  end;

  /// implement ViewModel/Controller sessions over TRestServer cookies
  TMvcSessionWithRestServer = class(TMvcSessionWithCookies)
  protected
    function GetCookie(out Value: PUtf8Char): integer; override;
    procedure SetCookie(const Value: RawUtf8); override;
  end;

  /// run TMvcApplication directly within a TRestServer method-based service
  // - this is the easiest way to host and publish a MVC Application, optionally
  // in conjunction with REST/AJAX client access
  TMvcRunOnRestServer = class(TMvcRunWithViews)
  protected
    fRestServer: TRestServer;
    /// method-based services used for the actual rendering on the TRestServer
    procedure RunOnRestServerRoot(Ctxt: TRestServerUriContext);
    procedure RunOnRestServerSub(Ctxt: TRestServerUriContext);
    procedure InternalRunOnRestServer(Ctxt: TRestServerUriContext;
      const MethodName: RawUtf8);
  public
    /// this constructor will publish some views to a TRestServer instance
    // - the associated RestModel can match the supplied TRestServer, or be
    // another instance (if the data model is not part of the publishing server)
    // - all TMvcApplication methods would be registered to the TRestServer,
    // as /root/methodName if aSubURI is '', or as /root/aSubURI/methodName
    // - if aApplication has no Views instance associated, this constructor will
    // initialize a Mustache renderer in its default folder, with '.html' void
    // template generation
    // - will also create a TMvcSessionWithRestServer for simple cookie sessions
    // - aPublishOptions could be used to specify integration with the server
    // - aAllowedMethods will render standard GET/POST by default
    constructor Create(aApplication: TMvcApplicationRest;
      const aTemplatesFolder: TFileName = ''; aRestServer: TRestServer = nil;
      const aSubURI: RawUtf8 = ''; aViews: TMvcViewsAbstract = nil;
      aPublishOptions: TMvcPublishOptions =
        [low(TMvcPublishOption) .. high(TMvcPublishOption)];
      aAllowedMethods: TUriMethods = [mGET, mPOST]); reintroduce;
  end;


var
  /// the pseudo-method name for the MVC information html page
  MVCINFO_URI: RawUtf8 = 'mvc-info';


{ ************ Expression Helpers for the ORM Data Model }

/// define Expression Helpers for some ORM tables
// - e.g. to read a TMyOrm from its ID value and put its fields
// in the current rendering data context, you can write:
// ! aView.RegisterExpressionHelpersForTables(aServer,[TMyOrm]);
// then use the following Mustache tag
// ! {{#TMyOrm MyRecordID}} ... {{/TMyOrm MyRecordID}}
// - use Bootstap CSS by default, but you can supply your aHtmlTableStyle
// - returns self so that may be called in a fluent interface
procedure RegisterExpressionHelpersForTables(aViews: TMvcViewsAbstract;
  aRest: TRest; const aTables: array of TOrmClass;
  aHtmlTableStyle: TExpressionHtmlTableStyleClass = nil); overload;

/// define Expression Helpers for all ORM tables of the supplied model
// - e.g. to read a TMyOrm from its ID value and put its fields
// in the current rendering data context, you can write:
// ! aView.RegisterExpressionHelpersForTables(aServer);
// then use the following Mustache tag
// ! {{#TMyOrm MyRecordID}} ... {{/TMyOrm MyRecordID}}
// - returns self so that may be called in a fluent interface
procedure RegisterExpressionHelpersForTables(aViews: TMvcViewsAbstract;
  aRest: TRest; aHtmlTableStyle: TExpressionHtmlTableStyleClass = nil); overload;


implementation


{ ******************************** MVC/MVVM application over a TRestServer }

{ TMvcRunOnRestServer }

constructor TMvcRunOnRestServer.Create(aApplication: TMvcApplicationRest;
  const aTemplatesFolder: TFileName; aRestServer: TRestServer;
  const aSubURI: RawUtf8; aViews: TMvcViewsAbstract;
  aPublishOptions: TMvcPublishOptions; aAllowedMethods: TUriMethods);
var
  m: PtrInt;
  bypass: boolean;
begin
  // initialize the TMvcRunWithViews parent context
  inherited Create(aApplication, aTemplatesFolder, aViews,
    aPublishOptions, aAllowedMethods);
  // setup the associated TRestServer
  if aRestServer = nil then
    fRestServer := aApplication.RestModel as TRestServer
  else
    fRestServer := aRestServer;
  if registerOrmTableAsExpressions in fPublishOptions then
    RegisterExpressionHelpersForTables(fViews, fRestServer);
  // no remote ORM access via REST, and route method_name as method/name too
  fRestServer.Options := fRestServer.Options +
    [rsoNoTableURI, rsoNoInternalState, rsoMethodUnderscoreAsSlashUri];
  // register the URI routes for this TRestServer
  bypass := bypassAuthentication in fPublishOptions;
  if aSubURI <> '' then
    fRestServer.ServiceMethodRegister(
      aSubURI, RunOnRestServerSub, bypass, fAllowedMethods)
  else
  begin
    for m := 0 to high(fApplication.MethodUri) do
      fRestServer.ServiceMethodRegister(
        fApplication.MethodUri[m], RunOnRestServerRoot, bypass, fAllowedMethods);
    if publishMvcInfo in fPublishOptions then
      fRestServer.ServiceMethodRegister(
        MVCINFO_URI, RunOnRestServerRoot, bypass, fAllowedMethods);
    if publishStatic in fPublishOptions then
      fRestServer.ServiceMethodRegister(
        STATIC_URI, RunOnRestServerRoot, bypass, fAllowedMethods);
  end;
  // setup the process session via TRestServer cookies
  aApplication.SetSession(TMvcSessionWithRestServer.Create(aApplication));
end;

procedure TMvcRunOnRestServer.InternalRunOnRestServer(
  Ctxt: TRestServerUriContext; const MethodName: RawUtf8);
var
  p: PUtf8Char;
  inputContext: variant;
  mainMethod, subMethod, body: RawUtf8;
  cache: TMvcRunCacheStatic;
  meth: PInterfaceMethod;
  r: TMvcRendererReturningData;
  start: Int64;
begin
  // 1. parse URI
  p := pointer(MethodName);
  if GetNextItemMultiple(p, '/?', mainMethod) = '/' then
    GetNextItem(p, '?', subMethod);
  // 2. implement mvc-info endpoint
  if (publishMvcInfo in fPublishOptions) and
     PropNameEquals(mainMethod, MVCINFO_URI) then
  begin
    if fMvcInfoCache = '' then
      ComputeMvcInfoCache;
    Ctxt.Returns(fMvcInfoCache, HTTP_SUCCESS, HTML_CONTENT_TYPE_HEADER, true);
    exit;
  end;
  // 3. serve static resources, with proper caching
  if (publishStatic in fPublishOptions) and
     PropNameEquals(mainMethod, STATIC_URI) then
  begin
    // this method uses a local in-memory cache, but would do the same as:
    // Ctxt.ReturnFileFromFolder(fViews.ViewStaticFolder);
    ProcessStatic(subMethod, cache);
    if cache.FileName <> '' then
      Ctxt.ReturnFile(cache.FileName, {304=}true, '', '', '', fStaticCacheControlMaxAge)
    else if cache.Body = '' then
      Ctxt.Error('', HTTP_NOTFOUND, fStaticCacheControlMaxAge)
    else
      Ctxt.Returns(cache.Body, HTTP_SUCCESS, cache.Header,
        {handle304=}true, false, fStaticCacheControlMaxAge, cache.Etag);
    exit;
  end;
  // 4. check basic context: HTTP method, URI and input parameters
  if not (Ctxt.Method in fAllowedMethods) then
  begin
    Ctxt.Error('', HTTP_NOTALLOWED); // we have an HTTP status code for that
    exit;
  end;
  meth := fApplication.Factory.FindMethod(mainMethod);
  if meth = nil then
  begin
    Ctxt.Error('', HTTP_NOTFOUND);
    exit;
  end;
  // 5. render regular page using proper viewer
  QueryPerformanceMicroSeconds(start);
  inputContext := Ctxt.GetInputAsTDocVariant(JSON_MVC, meth);
  if subMethod <> '' then
    if (allowJsonFormat in fPublishOptions) and
       PropNameEquals(subMethod, 'json') then
      r := TMvcRendererJson.Create
    else
    begin
      Ctxt.Error('', HTTP_NOTFOUND);
      exit;
    end
  else
    r := TMvcRendererFromViews.Create;
  try
    // _CurrentRenderer := r;
    r.Prepare(self, Ctxt.Call^.LowLevelRemoteIP, Ctxt.Call^.LowLevelUserAgent,
      Ctxt.TickCount64 div MilliSecsPerSec, @inputContext,
      pointer(Ctxt.Call^.InHead), meth);
    // handle OnBeforeRender custom callback
    if Assigned(fApplication.OnBeforeRender) then
      if not fApplication.OnBeforeRender(r) then
        exit; // aborted by this event handler
    // serialize r.InputContext^ into r.Input and execute into r.Output
    r.ExecuteCommand; // with proper caching
    // handle OnAfterRender custom callback
    if Assigned(fApplication.OnAfterRender) then
      if not fApplication.OnAfterRender(r) then
        exit; // processed by this event handler
    // return the response with proper ViewGenerationTimeTag replacement
    body := r.Output.Content;
    if viewHasGenerationTimeTag in r.OutputFlags then
      body := StringReplaceAll(body, fViews.ViewGenerationTimeTag,
        ShortStringToAnsi7String(MicroSecFrom(start)));
    Ctxt.Returns(body, r.Output.Status, r.Output.Header,
      {handle304=}true, {noerrorprocess=}true, {cachecontrol=}0,
      {hashwithouttime=}crc32cUtf8ToHex(r.Output.Content));
    if r.OutputCookieName <> '' then
      Ctxt.OutCookie[r.OutputCookieName] := r.OutputCookieValue;
  finally
    r.Free;
  end;
end;

procedure TMvcRunOnRestServer.RunOnRestServerRoot(Ctxt: TRestServerUriContext);
begin
  InternalRunOnRestServer(Ctxt, Ctxt.UriWithoutRoot);
end;

procedure TMvcRunOnRestServer.RunOnRestServerSub(Ctxt: TRestServerUriContext);
begin
  if Ctxt.UriMethodPath = '' then
    Ctxt.Redirect(Ctxt.UriWithoutSignature + '/default')
  else
    InternalRunOnRestServer(Ctxt, Ctxt.UriMethodPath);
end;


{ TMvcSessionWithRestServer }

function TMvcSessionWithRestServer.GetCookie(out Value: PUtf8Char): integer;
var
  ctxt: TRestServerUriContext;
  cookie: PHttpCookie;
  valueend: PUtf8Char;
begin
  result := 0;
  ctxt := ServiceRunningContext.Request;
  if ctxt = nil then
    exit;
  if ctxt.OutSetCookie <> '' then // 'name=value; path=...'
  begin
    Value := PosCharU(ctxt.OutSetCookie, '='); // assume name=CookieName
    if Value <> nil then
    begin
      inc(Value);
      valueend := PosChar(Value, ';');
      if valueend <> nil then
        result := valueend - Value
      else
        result := StrLen(Value);
      exit;
    end;
  end;
  cookie := ctxt.InCookieSearch(fContext.CookieName);
  if cookie = nil then
    exit;
  result := cookie^.ValueLen;
  Value := cookie^.ValueStart;
end;

procedure TMvcSessionWithRestServer.SetCookie(const Value: RawUtf8);
var
  ctxt: TRestServerUriContext;
begin
  ctxt := ServiceRunningContext.Request;
  if ctxt <> nil then
    ctxt.OutCookie[fContext.CookieName] := Value;
end;


{ TMvcApplicationRest }

procedure TMvcApplicationRest.Start(aRestModel: TRest; aInterface: PRttiInfo);
begin
  fLogClass := aRestModel.LogClass;
  SetInterface(aInterface);
  fRestModel := aRestModel;
end;

procedure TMvcApplicationRest.GetMvcInfo(out info: variant);
begin
  inherited GetMvcInfo(info); // name/mORMot/methods
  _ObjAddProp('root', RestModel.Model.Root, info);
end;


{ ************ Expression Helpers for the ORM Data Model }

type
  TExpressionHelperForTable = class
  public
    Rest: TRest;
    Table: TOrmClass;
    TableProps: TOrmProperties;
    HtmlTableStyle: TExpressionHtmlTableStyleClass;
    constructor Create(aRest: TRest; aTable: TOrmClass;
      var aHelpers: TSynMustacheHelpers;
      aHtmlTableStyle: TExpressionHtmlTableStyleClass);
    procedure ExpressionGet(const Value: variant; out Result: variant);
    procedure ExpressionHtmlTable(const Value: variant; out Result: variant);
  end;

{ TExpressionHelperForTable }

constructor TExpressionHelperForTable.Create(aRest: TRest; aTable: TOrmClass;
  var aHelpers: TSynMustacheHelpers;
  aHtmlTableStyle: TExpressionHtmlTableStyleClass);
var
  HelperName: RawUtf8;
begin
  aRest.PrivateGarbageCollector.Add(self);
  Rest := aRest;
  HelperName := RawUtf8(aTable.ClassName);
  Table := aTable;
  TableProps := aTable.OrmProps;
  TSynMustache.HelperAdd(aHelpers, HelperName, ExpressionGet);
  if aHtmlTableStyle = nil then
    aHtmlTableStyle := TExpressionHtmlTableStyleBootstrap;
  HtmlTableStyle := aHtmlTableStyle;
  TSynMustache.HelperAdd(aHelpers,
    HelperName + '.HtmlTable', ExpressionHtmlTable);
end;

procedure TExpressionHelperForTable.ExpressionHtmlTable(const Value: variant;
  out Result: variant);
var
  Rec: PDocVariantData;
  f, i, j: PtrInt;
  int: integer;
  Field: TOrmPropInfo;
  timelog: TTimeLogBits;
  caption: string;
  sets: TStringList;
  u: RawUtf8;
  W: TTextWriter;
  tmp: TTextWriterStackBuffer; // 8KB work buffer on stack
const
  ONOFF: array[boolean] of THtmlTableStyleLabel = (
    labelOff, labelOn);
  ENUM: array[boolean, boolean] of THtmlTableStyleLabel = (
   (labelValue, labelValue),
   (labelFalse, labelTrue));
begin
  if _SafeObject(Value, Rec) then
  begin
    W := TTextWriter.CreateOwnedStream(tmp);
    try
      HtmlTableStyle.StartTable(W);
      for f := 0 to TableProps.Fields.Count - 1 do
      begin
        Field := TableProps.Fields.List[f];
        i := Rec^.GetValueIndex(Field.Name);
        if i < 0 then
          continue;
        if not (Field.OrmFieldType in
            [oftAnsiText,
             oftUtf8Text,
             oftInteger,
             oftFloat,
             oftCurrency,
             oftTimeLog,
             oftModTime,
             oftCreateTime,
             oftDateTime,
             oftDateTimeMS,
             oftUnixTime,
             oftUnixMSTime,
             oftBoolean,
             oftEnumerate,
             oftSet]) then
          // we support only most obvious types in 'case OrmFieldType of" below
          continue;
        HtmlTableStyle.BeforeFieldName(W);
        GetCaptionFromPCharLen(TrimLeftLowerCase(Field.Name), caption);
        W.AddHtmlEscapeString(caption);
        HtmlTableStyle.BeforeValue(W);
        VariantToUtf8(Rec^.Values[i], u);
        case Field.OrmFieldType of
          oftAnsiText,
          oftUtf8Text,
          oftInteger,
          oftFloat,
          oftCurrency:
            W.AddHtmlEscape(pointer(u));
          oftTimeLog,
          oftModTime,
          oftCreateTime:
            if VariantToInt64(Rec^.Values[i], timelog.Value) then
              W.AddHtmlEscapeString(timelog.i18nText);
          oftDateTime,
          oftDateTimeMS:
            begin
              timelog.From(u);
              W.AddHtmlEscapeString(timelog.i18nText);
            end;
          oftUnixTime,
          oftUnixMSTime:
            if VariantToInt64(Rec^.Values[i], timelog.Value) then
            begin
              if Field.OrmFieldType = oftUnixTime then
                timelog.FromUnixTime(timelog.Value)
              else
                timelog.FromUnixMSTime(timelog.Value);
              W.AddHtmlEscapeString(timelog.i18nText);
            end;
          oftBoolean,
          oftEnumerate:
            if Field.InheritsFrom(TOrmPropInfoRttiEnum) then
            begin
              caption := TOrmPropInfoRttiEnum(Field).GetCaption(u, int);
              HtmlTableStyle.AddLabel(W, caption,
                ENUM[Field.OrmFieldType = oftBoolean, int <> 0]);
            end;
          oftSet:
            if Field.InheritsFrom(TOrmPropInfoRttiSet) and
               VariantToInteger(Rec^.Values[i], int) then
            begin
              sets := TStringList.Create;
              try
                TOrmPropInfoRttiSet(Field).SetEnumType^.AddCaptionStrings(sets);
                for j := 0 to sets.Count - 1 do
                begin
                  HtmlTableStyle.AddLabel(W, sets[j], ONOFF[GetBit(int, j)]);
                  W.AddShorter('<br/>');
                end;
              finally
                sets.Free;
              end;
            end;
        end;
        HtmlTableStyle.AfterValue(W);
      end;
      HtmlTableStyle.EndTable(W);
      RawUtf8ToVariant(W.Text, Result);
    finally
      W.Free;
    end;
  end
  else
    // not an object -> return input value as is
    Result := Value;
end;

procedure TExpressionHelperForTable.ExpressionGet(const Value: variant;
  out Result: variant);
var
  Rec: TOrm;
  ID: integer;
begin
  SetVariantNull(Result);
  if not VariantToInteger(Value, ID) then
    exit;
  Rec := Table.Create;
  try
    if Rest.ORM.Retrieve(ID, Rec) then
      Result := Rec.GetSimpleFieldsAsDocVariant(true);
  finally
    Rec.Free;
  end;
end;

type
  _TMvcViewsMustache = class(TMvcViewsMustache); // to access fViewHelpers var

procedure RegisterExpressionHelpersForTables(aViews: TMvcViewsAbstract;
  aRest: TRest; const aTables: array of TOrmClass;
  aHtmlTableStyle: TExpressionHtmlTableStyleClass);
var
  t: PtrInt;
begin
  if (aRest <> nil) and
     (aViews <> nil) and
     aViews.InheritsFrom(TMvcViewsMustache) then
    for t := 0 to high(aTables) do
      if aRest.Model.GetTableIndex(aTables[t]) >= 0 then
        TExpressionHelperForTable.Create(aRest, aTables[t],
          _TMvcViewsMustache(aViews).fViewHelpers, aHtmlTableStyle);
end;

procedure RegisterExpressionHelpersForTables(aViews: TMvcViewsAbstract;
  aRest: TRest; aHtmlTableStyle: TExpressionHtmlTableStyleClass);
var
  t: PtrInt;
begin
  if (aRest <> nil) and
     (aViews <> nil) and
     aViews.InheritsFrom(TMvcViewsMustache) then
    for t := 0 to aRest.Model.TablesMax do
      TExpressionHelperForTable.Create(aRest, aRest.Model.Tables[t],
        _TMvcViewsMustache(aViews).fViewHelpers, aHtmlTableStyle);
end;


end.

