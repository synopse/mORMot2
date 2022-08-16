// Author: Thomas Bogenrieder
// The example is a proof of concept for working with the mORMot library. Source code is neither tested nor optimized.

unit u_FileServer;

{$I mormot.defines.inc}

interface

uses
  System.SysUtils,
  mormot.core.base,
  mormot.core.data,
  mormot.core.os,
  mormot.core.log,
  mormot.core.zip,
  mormot.core.text,
  mormot.core.json,
  mormot.core.rtti,
  mormot.core.perf,
  mormot.core.search,
  mormot.core.buffers,
  mormot.core.unicode,
  mormot.core.variants,
  mormot.core.mustache,
  mormot.core.collections,
  mormot.soa.core,
  mormot.orm.base,
  mormot.orm.core,
  mormot.net.sock,
  mormot.net.http,
  mormot.net.client,
  mormot.net.server;

type
  TProjectData = record
    Mustache: RawUtf8;
    InlineJavaScript: RawUtf8;
    MustacheDataFiles: TRawUtf8DynArray;
    ExternalCssJsFiles: TRawUtf8DynArray;
    function LoadFromFile(const pmcFileName: TFileName): Boolean;
    procedure SaveToFile(const pmcFileName: TFileName);
    procedure Clear;
  end;

type
  THttpServerMode = (
    hsmHttp10,      // HTTP 1.0
    hsmHttp11,      // HTTP 1.1
    hsmHttpsSelf,   // HTTPS - self-signed
    hsmHttpsCert);  // HTTPS - with own Certificate

  TOnStartNavigate = procedure(const pmcUri: String) of Object;
  TOnPrepareFinished = procedure(const pmElapsedTime: TSynMonitorOneMicroSec) of Object;

  TFileServer = class(TObject)
  strict private
    FTimer: TPrecisionTimer;
    FHttpServer: THttpServer;
    FMustacheHelpers: TSynMustacheHelpers;
    FMustachePartials: TSynMustachePartials;
    FFileContentCache: IKeyValue<RawUtf8, RawByteString>;
  private
    const
      ZIPFILE_SEPERATOR = '.ZIP/';
      DEFAULTHTML_FILENAME = 'INDEX.HTML';
      PARTIALFILE_SEARCHMASK = '*.partial.html';  // Editors select the correct schema with .html extension
      CONTEXTFILE_SEARCHMASK = '*.json';
  private
    FUrlScheme: RawUtf8;
    FServerPort: RawUtf8;
    FRawIndexHtml: RawUtf8;
    FLastCleanupRun: Int64;
    FLastAccessToken: Cardinal;
    FOnStartNavigate: TOnStartNavigate;
    FOnPrepareFinished: TOnPrepareFinished;
    function DoRequest(pmCtxt: THttpServerRequestAbstract): Cardinal;
    function DoFileRequest(pmCtxt: THttpServerRequestAbstract; const pmcUriPath: RawUtf8): Cardinal;
    function DoZipFileRequest(pmCtxt: THttpServerRequestAbstract; const pmcUriPath: RawUtf8): Cardinal;
    function DoDefaultFileRequest(pmCtxt: THttpServerRequestAbstract; const pmcUriPath: RawUtf8): Cardinal;
    function SplitUrlSchemePathAndQuery(const pmcUrl: RawUtf8; out pmoUrlPath, pmoUrlQuery: RawUtf8): Boolean;
    function PrepareHtmlOutput(const pmcProjectData: TProjectData): RawUtf8;
    procedure PrepareMustacheContextData(const pmDataFiles: TRawUtf8DynArray; out pmoContextData: TDocVariantData);
    procedure PrepareMustachePartials;
    procedure CleanFileContentCache;
    procedure InitMustacheHelpers;
    // Mustache ExpressionHelpers
    procedure CurrToText(const pmcValue: Variant; out pmoResult: Variant);
    procedure IsVariable(const pmcValue: Variant; out pmoResult: Variant);
    procedure IsVarNotVoid(const pmcValue: Variant; out pmoResult: Variant);
  public
    const
      ASSETS_FOLDER = 'Assets';
      PARTIALS_FOLDER = 'Partials';
  public
    constructor Create;
    destructor Destroy; override;
    function StartServer(const pmcPort: RawUtf8; pmMode: THttpServerMode = hsmHttp10;
      const pmcCertFileName: TFileName = '';
      const pmcPrivKeyFileName: TFileName = '';
      const pmcPrivKeyPassword: RawUtf8 = ''): Boolean;
    procedure PrepareAndExecute(const pmcProjectData: TProjectData);
    property OnStartNavigate: TOnStartNavigate
      read FOnStartNavigate write FOnStartNavigate;
    property OnPrepareFinished: TOnPrepareFinished
      read FOnPrepareFinished write FOnPrepareFinished;
  end;


implementation

uses
  System.Math, Dialogs,
  u_Utilities;

//==============================================================================
// TProjectData
//==============================================================================

function TProjectData.LoadFromFile(const pmcFileName: TFileName): Boolean;
begin
  Result := False;
  if FileExists(pmcFileName) then
  begin
    var json: RawJson := StringFromFile(pmcFileName);
    Result := RecordLoadJson(Self, json, TypeInfo(TProjectData));
  end
end;


procedure TProjectData.SaveToFile(const pmcFileName: TFileName);
begin
  FileFromString(RecordSaveJson(Self, TypeInfo(TProjectData)), pmcFileName);
end;


procedure TProjectData.Clear;
begin
  Mustache := '';
  InlineJavaScript := '';
  SetLength(MustacheDataFiles, 0);
  SetLength(ExternalCssJsFiles, 0);
end;


//==============================================================================
// TFileServer
//==============================================================================

constructor TFileServer.Create;
begin
  inherited Create;
  FMustachePartials := TSynMustachePartials.Create;
  FFileContentCache := Collections.NewKeyValue<RawUtf8, RawByteString>([kvoKeyCaseInsensitive, kvoThreadSafe], (60 * 15));
  FLastCleanupRun := GetTickCount64;
  InitMustacheHelpers;
end;


destructor TFileServer.Destroy;
begin
  FreeAndNil(FHttpServer);
  FFileContentCache.Clear;
  FMustachePartials.Free;
  inherited Destroy;
end;


function TFileServer.DoRequest(pmCtxt: THttpServerRequestAbstract): Cardinal;
var
  p: PUtf8Char;
  urlPath: RawUtf8;
  urlQuery: RawUtf8;
  accessToken: Int64;
begin
  if not SplitUrlSchemePathAndQuery(pmCtxt.Url, urlPath, urlQuery) then Exit(HTTP_BADREQUEST); //=>

  accessToken := 0;
  if Length(urlQuery) > 0 then
  begin
    p := PUtf8Char(Pointer(urlQuery));
    if UrlDecodeNeedParameters(p, 'ACCESSTOKEN') then
      UrlDecodeInt64(p, 'ACCESSTOKEN=', accessToken);
  end;

  if (accessToken = 0)
    or (accessToken = FLastAccessToken) then
  begin
    if IdemPChar(PUtf8Char(Pointer(urlPath)), DEFAULTHTML_FILENAME) then
      Result := DoDefaultFileRequest(pmCtxt, urlPath)
    else if PosI(ZIPFILE_SEPERATOR, urlPath) > 0 then
      Result := DoZipFileRequest(pmCtxt, urlPath)
    else
      Result := DoFileRequest(pmCtxt, urlPath);
  end
  else
    Result := HTTP_NOTALLOWED;
end;


function TFileServer.DoFileRequest(pmCtxt: THttpServerRequestAbstract; const pmcUriPath: RawUtf8): Cardinal;
var
  fileName: TFileName;
begin
  Utf8ToFileName(pmcUriPath, fileName);
  if CheckFileName(fileName, [fnvFileName], @fileName) then
  begin
    fileName := MakePath([Executable.ProgramFilePath, ASSETS_FOLDER, fileName]);
    pmCtxt.OutContent := StringToUtf8(fileName);
    pmCtxt.OutContentType := STATICFILE_CONTENT_TYPE;
    pmCtxt.OutCustomHeaders := STATICFILE_CONTENT_TYPE_HEADER + #13#10 + GetMimeContentTypeHeader('', fileName);
    Result := HTTP_SUCCESS;
  end
  else
    Result :=  HTTP_NOTFOUND;
end;


function TFileServer.DoZipFileRequest(pmCtxt: THttpServerRequestAbstract; const pmcUriPath: RawUtf8): Cardinal;
var
  p: PUtf8Char;
  idx: Integer;
  zipFile, contentFile: RawUtf8;
  rawFileContent: RawByteString;
begin
  Result := HTTP_NOTFOUND;
  p := PUtf8Char(Pointer(pmcUriPath));
  idx := PosI(ZIPFILE_SEPERATOR, pmcUriPath);

  Inc(idx, Length(ZIPFILE_SEPERATOR) - 2);
  FastSetString(zipFile, p, idx);

  Inc(idx, 1);
  FastSetString(contentFile,  p + idx, Length(pmcUriPath) - idx);

  if contentFile <> '' then
  begin
    if not FFileContentCache.TryGetValue(pmcUriPath, rawFileContent) then
    begin
      var zipFileName: TFileName := Utf8ToString(zipFile);
      if CheckFileName(zipFileName, [fnvFileName], @zipFileName) then
      begin
        zipFileName := MakePath([Executable.ProgramFilePath, ASSETS_FOLDER, zipFileName]);
        var zipRead: TZipRead := TZipRead.Create(zipFileName);
        try
          idx := zipRead.NameToIndex(Utf8ToString(contentFile));
          if idx >= 0 then
          begin
            var contentStream: TRawByteStringStream := TRawByteStringStream.Create;
            try
              if zipRead.UnZip(idx, contentStream) then
              begin
                rawFileContent := contentStream.DataString;
                if Length(rawFileContent) < (2 shl 20) then
                  FFileContentCache.Add(pmcUriPath, rawFileContent);
              end;
            finally
              contentStream.Free;
            end;
          end;
        finally
          zipRead.Free;
        end;
      end;
    end;

    if Length(rawFileContent) > 0 then
    begin
      pmCtxt.OutContent := rawFileContent;
      pmCtxt.OutContentType := GetMimeContentType(Pointer(rawFileContent), Length(rawFileContent), Utf8ToString(contentFile));
      pmCtxt.OutCustomHeaders := HEADER_CONTENT_TYPE + pmCtxt.OutContentType;
      Result := HTTP_SUCCESS;
    end;
  end;
end;


function TFileServer.DoDefaultFileRequest(pmCtxt: THttpServerRequestAbstract; const pmcUriPath: RawUtf8): Cardinal;
begin
  if FRawIndexHtml <> '' then
  begin
    pmCtxt.OutContent := FRawIndexHtml;
    pmCtxt.OutContentType := HTML_CONTENT_TYPE;
    pmCtxt.OutCustomHeaders := HTML_CONTENT_TYPE_HEADER;
    Result := HTTP_SUCCESS;
  end
  else
    Result := HTTP_NOCONTENT;
end;


function TFileServer.SplitUrlSchemePathAndQuery(const pmcUrl: RawUtf8; out pmoUrlPath, pmoUrlQuery: RawUtf8): Boolean;
var
  p: PUtf8Char;
  urlQuery: PUtf8Char;
  urlPathLen: Integer;
begin
  p := PUtf8Char(Pointer(pmcUrl));
  if (p <> Nil) and (p^ = '/') then
    Inc(p)
  else
    Exit(False); //=>

  urlQuery := PosChar(p, '?');
  if urlQuery <> Nil then
  begin
    Inc(urlQuery);
    urlPathLen := urlQuery - p - 1;
    FastSetString(pmoUrlQuery, urlQuery, Length(p) - (urlQuery - p));
  end
  else
    urlPathLen := Length(p);

  FastSetString(pmoUrlPath, p, urlPathLen);
  Result := True;
end;


function TFileServer.PrepareHtmlOutput(const pmcProjectData: TProjectData): RawUtf8;

  //-------- local functions ------------------------------------------

  procedure InsertCSSLinks(_pmWriter: TTextWriter; const _pmcFiles: TRawUtf8DynArray);
  var
    idx: Integer;
    linkArray: TRawUtf8DynArray;
  begin
    SetLength(linkArray, 1);
    linkArray[0] := 'bootstrap.zip/bootstrap.min.css';
    for var i: Integer := 0 to High(_pmcFiles) do
    begin
      if _pmcFiles[i] = '' then Continue; //->

      if IdemFileExt(Pointer(_pmcFiles[i]), '.CSS') then
      begin
        idx := Length(linkArray);
        SetLength(linkArray, idx + 1);
        if IdemPChar(Pointer(_pmcFiles[i]), '*.') then
        begin
          linkArray[idx] := linkArray[0];
          linkArray[0] := '';
        end
        else
          linkArray[idx] := _pmcFiles[i];
      end;
    end;

    idx := IfThen((linkArray[0] <> ''), 0, 1);
    repeat
      _pmWriter.AddString(FormatUtf8('<link href="%" rel="stylesheet">', [linkArray[idx]]));
      Inc(idx);
    until (idx = Length(linkArray));
  end;


  procedure InsertJSScripts(_pmWriter: TTextWriter; const _pmcFiles: TRawUtf8DynArray);
  var
    p: PUtf8Char;
    idx: Integer;
    fileName: RawUtf8;
    linkArray: TRawUtf8DynArray;
  begin
    SetLength(linkArray, 1);
    linkArray[0] := '<script src="bootstrap.zip/bootstrap.bundle.min.js" defer></script>';
    for var i: Integer := 0 to High(_pmcFiles) do
    begin
      if _pmcFiles[i] = '' then Continue; //->

      if IdemFileExt(Pointer(_pmcFiles[i]), '.JS') then
      begin
        idx := Length(linkArray);
        SetLength(linkArray, idx + 1);
        if IdemPChar(Pointer(_pmcFiles[i]), '*.') then
        begin
          linkArray[idx] := linkArray[0];
          linkArray[0] := '';
        end
        else
        begin
          p := PUtf8Char(Pointer(_pmcFiles[i]));
          case p[0] of
            '!':
              begin
                FastSetString(fileName, (p + 1), (Length(p) -1));
                linkArray[idx] := FormatUtf8('<script src="%"></script>', [fileName]);
              end;
            '?':
              begin
                FastSetString(fileName, (p + 1), (Length(p) -1));
                linkArray[idx] := FormatUtf8('<script src="%" async></script>', [fileName]);
              end;
            else
              linkArray[idx] := FormatUtf8('<script src="%" defer></script>', [p]);
          end;
        end;
      end;
    end;

    idx := IfThen((linkArray[0] <> ''), 0, 1);
    repeat
      _pmWriter.AddString(linkArray[idx]);
      Inc(idx);
    until (idx = Length(linkArray));
  end;

  //-------- end local functions --------------------------------------

var
  writer: TTextWriter;
  buffer: TTextWriterStackBuffer;
  contextData: TDocVariantData;
begin
  writer := TTextWriter.CreateOwnedStream(buffer);
  try
    writer.AddStrings(['<!DOCTYPE html>', '<html lang="en">', '<head>']);
    writer.AddString('<meta charset="utf-8">');
    writer.AddString('<meta name="viewport" content="width=device-width, initial-scale=1">');
    InsertCSSLinks(writer, pmcProjectData.ExternalCssJsFiles);
    InsertJSScripts(writer, pmcProjectData.ExternalCssJsFiles);
    if Length(pmcProjectData.InlineJavaScript) > 0 then
      writer.AddStrings(['<script>', pmcProjectData.InlineJavaScript, '</script>']);
    writer.AddStrings(['</head>', '<body>']);

    // That's a disaster for performance, but here in the demo you can immediately see
    // what's going on. A much better implementation can be seen in unit mormot.rest.mvc.
    PrepareMustachePartials;
    PrepareMustacheContextData(pmcProjectData.MustacheDataFiles, contextData);
    writer.AddString(TSynMustache.Parse(pmcProjectData.Mustache).Render(Variant(contextData), FMustachePartials, FMustacheHelpers));

    writer.AddStrings(['</body>', '</html>']);
    writer.SetText(Result);
  finally
    writer.Free;
  end;
end;


procedure TFileServer.PrepareMustacheContextData(const pmDataFiles: TRawUtf8DynArray; out pmoContextData: TDocVariantData);
var
  context: TDocVariantData;
  contextName: RawUtf8;
begin
  pmoContextData.Init;
  var assetsFolder: TFileName := MakePath([Executable.ProgramFilePath, ASSETS_FOLDER], True);
  var contextFiles: TFileNameDynArray := FileNames(assetsFolder, CONTEXTFILE_SEARCHMASK, [ffoExcludesDir]);
  for var i: Integer := 0 to High(contextFiles) do
  begin
    if FindPropName(pmDataFiles, StringToUtf8(contextFiles[i])) < 0 then Continue; //->

    context.InitJsonFromFile(assetsFolder + contextFiles[i], JSON_FAST_FLOAT);
    contextName := StringToUtf8(GetFileNameWithoutExt(contextFiles[i]));
    pmoContextData.AddValue(contextName, Variant(context));
    context.Clear;
  end;
end;


procedure TFileServer.PrepareMustachePartials;
var
  partialName: TFileName;
begin
  FMustachePartials.List.Clear;
  var partialsFolder: TFileName := MakePath([Executable.ProgramFilePath, PARTIALS_FOLDER], True);
  var partialFiles: TFileNameDynArray := FileNames(partialsFolder, PARTIALFILE_SEARCHMASK, [ffoExcludesDir]);
  for var i: Integer := 0 to High(partialFiles) do
  begin
    partialName := GetFileNameWithoutExt(GetFileNameWithoutExt(partialFiles[i]));
    FMustachePartials.Add(StringToUtf8(partialName), AnyTextFileToRawUtf8(partialsFolder + partialFiles[i]));
  end;
end;


procedure TFileServer.CleanFileContentCache;
const
  INTERVAL = (1000 * 60 * 3);
var
  tix: Int64;
begin
  tix := GetTickCount64;
  if tix > FLastCleanupRun + INTERVAL then
  begin
    FLastCleanupRun := tix;
    FFileContentCache.DeleteDeprecated;
  end;
end;


procedure TFileServer.InitMustacheHelpers;
begin
  FMustacheHelpers := TSynMustache.HelpersGetStandardList(
    ['CurrToText', 'IsVariable', 'IsVarNotVoid'], [CurrToText, IsVariable, IsVarNotVoid]);
end;


//--- Mustache ExpressionHelpers -----------------------------------------------

procedure TFileServer.CurrToText(const pmcValue: Variant; out pmoResult: Variant);
begin
  pmoResult := CurrToStrF(pmcValue, ffCurrency, 2);
end;


procedure TFileServer.IsVariable(const pmcValue: Variant; out pmoResult: Variant);
begin
  pmoResult := not VarIsEmptyOrNull(pmcValue);
end;


procedure TFileServer.IsVarNotVoid(const pmcValue: Variant; out pmoResult: Variant);
begin
  pmoResult := not VarIsVoid(pmcValue);
end;


function TFileServer.StartServer(const pmcPort: RawUtf8; pmMode: THttpServerMode;
  const pmcCertFileName, pmcPrivKeyFileName: TFileName; const pmcPrivKeyPassword: RawUtf8): Boolean;
const
  WAIT_SECONDS = 10;
var
  tls: TNetTlsContext;
  keepAliveMs: Integer;
  serverOptions: THttpServerOptions;
begin
  Result := False;
  if FHttpServer <> Nil then Exit; //=>
  if (pmMode = hsmHttpsCert)
    and not (FileExists(pmcCertFileName) and FileExists(pmcPrivKeyFileName) and (pmcPrivKeyPassword <> '')) then Exit; //=>

  keepAliveMs := 30000;
  if pmMode = hsmHttp10 then
    keepAliveMs := 0;  // HTTP/1.0: KeepAliveTimeOut = 0

  serverOptions := [];
  if (pmMode = hsmHttpsSelf) or (pmMode = hsmHttpsCert) then
    serverOptions := [hsoEnableTls];

  FHttpServer := THttpServer.Create(pmcPort, Nil, Nil, '', {ServerThreadPoolCount=} 2, keepAliveMs, serverOptions);
  if (pmMode = hsmHttp10) or (pmMode = hsmHttp11) then
    FHttpServer.WaitStarted(WAIT_SECONDS)
  else
  begin
    if pmMode = hsmHttpsSelf then
    begin
      InitNetTlsContextSelfSignedServer(tls);
      try
        FHttpServer.WaitStarted(WAIT_SECONDS, @tls);
      finally
        DeleteFile(Utf8ToString(tls.CertificateFile));
        DeleteFile(Utf8ToString(tls.PrivateKeyFile));
      end;
    end
    else
    begin
      InitNetTlsContext(tls, True, pmcCertFileName, pmcPrivKeyFileName, pmcPrivKeyPassword);
      FHttpServer.WaitStarted(WAIT_SECONDS, @tls);
    end;
  end;

  Result := FHttpServer.Started;
  if Result then
  begin
    FServerPort := pmcPort;
    if pmMode in [hsmHttpsSelf, hsmHttpsCert] then
      FUrlScheme := 'https'
    else
      FUrlScheme := 'http';

    FHttpServer.OnRequest := DoRequest;
  end;
end;


procedure TFileServer.PrepareAndExecute(const pmcProjectData: TProjectData);
const
  URL_TEMPLATE = '%://localhost:%/index.html?accessToken=%';
begin
  if not Assigned(FOnStartNavigate) then Exit; //=>

  FTimer.Start;
  FRawIndexHtml := PrepareHtmlOutput(pmcProjectData);
  if FRawIndexHtml <> '' then
  begin
    FLastAccessToken := Random32;
    FOnStartNavigate(Utf8ToString(FormatUtf8(URL_TEMPLATE, [FUrlScheme, FServerPort, Int64(FLastAccessToken)])));  // Cardinal values should be type-casted to Int64
    FTimer.Pause;
    if Assigned(FOnPrepareFinished) then
      FOnPrepareFinished(FTimer.LastTimeInMicroSec);
  end;

  CleanFileContentCache;
end;

end.
