// Author: Thomas Bogenrieder
// The example is a proof of concept for creating method-based services with mORMot, the source code is neither tested nor optimized.

program TestRestServer;

{$I mormot.defines.inc}

{$IFDEF MSWINDOWS}
  {$APPTYPE CONSOLE}
{$ENDIF MSWINDOWS}

uses
  {$I mormot.uses.inc}
  System.SysUtils,
  mormot.core.base,
  mormot.core.data,
  mormot.core.os,
  mormot.core.log,
  mormot.core.text,
  mormot.core.json,
  mormot.core.search,
  mormot.core.buffers,
  mormot.core.unicode,
  mormot.orm.base,
  mormot.orm.core,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.http.server,
  mormot.rest.memserver,
  u_SharedTypes;

type
  TTestServerLog = class(TSynLog)
  protected
    procedure ComputeFileName; override;
  end;

  TCustomerItem = record
    CustomerNum: Integer;
    LoginUserName: RawUtf8;
    LoginPassword: RawUtf8;
  end;

  TCustomerItemArray = array of TCustomerItem;

  TFileAuthUser = class(TAuthUser)
  strict private
    FCustomerNum: Integer;
  published
    property CustomerNum: Integer
      read FCustomerNum write FCustomerNum;
  end;

  TFileRestServer = class(TRestServerFullMemory)
  strict private
    FDataFolder: TFileName;
  protected
    const
      DEFAULT_FILE_EXT = '._';
  protected
    function GetSessionUserDirName(pmCtxt: TRestServerUriContext; out pmoDirName: TFileName): Boolean;
    property DataFolder: TFileName
      read FDataFolder;
  public
    constructor Create(const pmcRootName: RawUtf8; const pmcDataFolder: TFileName); reintroduce;
    function InitAuthForAllCustomers(const pmcFileName: TFileName): Boolean;
  published
    procedure LoadFile(pmCtxt: TRestServerUriContext);
    procedure SaveFile(pmCtxt: TRestServerUriContext);
    procedure GetAllFileNames(pmCtxt: TRestServerUriContext);
  end;

  TTestServerMain = class(TSynPersistent)
  strict private
    FCustomerConfigFile: TFileName;
  private
    FHttpServer: TRestHttpServer;
    FRestServer: TFileRestServer;
  public
    constructor Create(const pmcCustomerConfigFileName: TFileName; const pmcDataFolder: TFileName); reintroduce;
    destructor Destroy; override;
    function RunServer(const pmcPort: RawUtf8): Boolean;
  end;


resourcestring
  SErrHttpForbidden =
      'This action is forbidden for you.';

//==============================================================================
// TTestServerLog
//==============================================================================

procedure TTestServerLog.ComputeFileName;
begin
  inherited ComputeFileName;
  FFileName := StringReplace(FFileName, ' ', '_', [rfReplaceAll]);
end;


//==============================================================================
// TFileRestServer
//==============================================================================

constructor TFileRestServer.Create(const pmcRootName: RawUtf8; const pmcDataFolder: TFileName);
begin
  if not DirectoryExists(pmcDataFolder) then
    raise Exception.Create('A data directory must be specified.');

  CreateWithOwnModel([TAuthGroup, TFileAuthUser], {HandleUserAuthentication=} True, pmcRootName);

  // Logging class initialization
  SetLogClass(TTestServerLog);
  LogFamily.Level := LOG_VERBOSE;
  LogFamily.PerThreadLog := ptIdentifiedInOneFile;
  LogFamily.HighResolutionTimestamp := True;

  Server.CreateMissingTables(0, [itoNoAutoCreateUsers]);  // You should always create your own users
  FDataFolder := pmcDataFolder;
end;


function TFileRestServer.GetSessionUserDirName(pmCtxt: TRestServerUriContext; out pmoDirName: TFileName): Boolean;
begin
  Result := False;
  if pmCtxt = Nil then Exit; //=>

  var authUser: TFileAuthUser := TFileAuthUser(pmCtxt.Server.SessionGetUser(pmCtxt.Session));
  if authUser <> Nil then
  try
    pmoDirName := Format('C%.5d', [authUser.CustomerNum]);
    Result := True;
  finally
    authUser.Free;
  end;
end;


function TFileRestServer.InitAuthForAllCustomers(const pmcFileName: TFileName): Boolean;
var
  json: RawUtf8;
  customers: TCustomerItemArray;
begin
  Result := False;
  if not FileExists(pmcFileName) then Exit; //=>

  json := AnyTextFileToRawUtf8(pmcFileName, {AssumeUtf8IfNoBom=} True);
  if IsValidJson(json)
    and (DynArrayLoadJson(customers, Pointer(json), TypeInfo(TCustomerItemArray)) <> Nil) then
  begin
    var authUser: TFileAuthUser := TFileAuthUser.Create;
    try
      for var i: Integer := 0 to High(customers) do
      begin
        if (customers[i].LoginUserName <> '')
          and (customers[i].LoginPassword <> '') then
        begin
          authUser.CustomerNum := customers[i].CustomerNum;
          authUser.LogonName := customers[i].LoginUserName;
          // Never work with passwords in plain text, this also applies to saving! Therefore, this is not a good example.
          authUser.PasswordHashHexa := TAuthUser.ComputeHashedPassword(customers[i].LoginPassword);
          authUser.DisplayName := StringToUtf8(Format('Customer number: %d', [customers[i].CustomerNum]));
          authUser.GroupRights := TAuthGroup(3);  // AuthGroup: User
          Server.Add(authUser, True);
        end;
      end;
    finally
      authUser.Free;
    end;

    Result := (Server.TableRowCount(TFileAuthUser) > 0);
  end;
end;


procedure TFileRestServer.LoadFile(pmCtxt: TRestServerUriContext);
resourcestring
  SErrFileNotFound =
      'File not found.';
var
  dirName, imageName, fileName: TFileName;
begin
  if (pmCtxt.Method = mGET)
    and GetSessionUserDirName(pmCtxt, dirName)
    and UrlDecodeNeedParameters(pmCtxt.Parameters, TFileServiceFunction.Param.LoadFile_ImageName) then
  begin
    Utf8ToFileName(pmCtxt.InputUtf8[TFileServiceFunction.Param.LoadFile_ImageName], imageName);
    if CheckFileName(imageName, [fnvFileName], @imageName) then  // Never trust data that comes from outside!
    begin
      if ExtractFileExt(imageName) = '' then
        imageName := ChangeFileExt(imageName, DEFAULT_FILE_EXT);

      fileName := MakePath([DataFolder, dirName, imageName]);
      if FileExists(fileName) then
      begin
        pmCtxt.ReturnFile(fileName);
        Exit; //=>
      end;
    end;

    pmCtxt.Error(StringToUtf8(SErrFileNotFound), HTTP_NOTFOUND);
  end
  else
    pmCtxt.Error(StringToUtf8(SErrHttpForbidden), HTTP_FORBIDDEN);
end;


procedure TFileRestServer.SaveFile(pmCtxt: TRestServerUriContext);
resourcestring
  SErrFileNotCreated =
      'File could not be created.';
var
  dirName, imageName, fileName: TFileName;
begin
  if (pmCtxt.Method = mPUT)
    and GetSessionUserDirName(pmCtxt, dirName)
    and UrlDecodeNeedParameters(pmCtxt.Parameters, TFileServiceFunction.Param.SaveFile_ImageName) then
  begin
    Utf8ToFileName(pmCtxt.InputUtf8[TFileServiceFunction.Param.SaveFile_ImageName], imageName);
    if CheckFileName(imageName, [fnvFileName], @imageName) then  // Never trust data that comes from outside!
    begin
      if ExtractFileExt(imageName) = '' then
        imageName := ChangeFileExt(imageName, DEFAULT_FILE_EXT);

      fileName := MakePath([DataFolder, dirName, imageName]);
      if (EnsureDirectoryExists(ExtractFilePath(fileName)) <> '')
        and FileFromString(pmCtxt.Call.InBody, fileName) then
      begin
        pmCtxt.Success;
        Exit; //=>
      end;
    end;

    pmCtxt.Error(StringToUtf8(SErrFileNotCreated), HTTP_BADREQUEST);
  end
  else
    pmCtxt.Error(StringToUtf8(SErrHttpForbidden), HTTP_FORBIDDEN);
end;


procedure TFileRestServer.GetAllFileNames(pmCtxt: TRestServerUriContext);
var
  dirName: TFileName;
  dirFiles: TFileNameDynArray;
begin
  if (pmCtxt.Method = mGET)
    and GetSessionUserDirName(pmCtxt, dirName) then
  begin
    dirFiles := FileNames([DataFolder, dirName], FILES_ALL, [ffoSortByName, ffoExcludesDir]);
    for var i: Integer := Low(dirFiles) to High(dirFiles) do
    begin
      if ExtractFileExt(dirFiles[i]) = DEFAULT_FILE_EXT then
        dirFiles[i] := GetFileNameWithoutExt(dirFiles[i]);
    end;

    pmCtxt.Returns(DynArraySaveJson(dirFiles, TypeInfo(TFileNameDynArray)), HTTP_SUCCESS);
  end
  else
    pmCtxt.Error(StringToUtf8(SErrHttpForbidden), HTTP_FORBIDDEN);
end;


//==============================================================================
// TTestServerMain
//==============================================================================

constructor TTestServerMain.Create(const pmcCustomerConfigFileName: TFileName; const pmcDataFolder: TFileName);
begin
  inherited Create;
  FRestServer := TFileRestServer.Create(ROOT_NAME_FILE, pmcDataFolder);
  FCustomerConfigFile := pmcCustomerConfigFileName;
  if ExtractFilePath(FCustomerConfigFile) = '' then
    FCustomerConfigFile := MakePath([Executable.ProgramFilePath, FCustomerConfigFile]);
end;


destructor TTestServerMain.Destroy;
begin
  FreeAndNil(FHttpServer);
  FRestServer.Free;
  inherited Destroy;
end;


function TTestServerMain.RunServer(const pmcPort: RawUtf8): Boolean;
begin
  Result := False;
  if (FHttpServer = Nil)
    and FRestServer.InitAuthForAllCustomers(FCustomerConfigFile) then
  begin
    FHttpServer := TRestHttpServer.Create(pmcPort, [FRestServer], '+' {DomainName}, useHttpSocket {or useHttpAsync});
    FHttpServer.AccessControlAllowOrigin := '*';
    Result := True;
  end;
end;


//==============================================================================

var
  MainServer: TTestServerMain;

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  AuthUserGroupDefaultTimeout := 60 * 10;  // 10 minutes

  MainServer := TTestServerMain.Create('Customer.config', Executable.ProgramFilePath);
  try
    try
      if MainServer.RunServer('8080') then
        WriteLn('Press [Enter] to quit server...')
      else
        WriteLn('Something went wrong...');
    except
      on E: Exception do
      begin
        ConsoleShowFatalException(E, True);
        ExitCode := 1;
      end;
    end;

    ConsoleWaitForEnterKey;
  finally
    MainServer.Free;
  end;

end.
