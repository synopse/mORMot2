// Author: Thomas Bogenrieder
// The example is a proof of concept for creating interface-based services with mORMot, the source code is neither tested nor optimized.

unit u_ServiceServer;

{$I mormot.defines.inc}

interface

uses
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
  mormot.core.interfaces,
  mormot.orm.base,
  mormot.orm.core,
  mormot.soa.core,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.memserver;

type
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

  TRestServerLog = class(TSynLog)
  protected
    procedure ComputeFileName; override;
  end;

  TFileRestServer = class(TRestServerFullMemory)
  strict private
    FDataFolder: TFileName;
  public
    const
      DEFAULT_FILE_EXT = '._';
  public
    constructor Create(const pmcDataFolder: TFileName); reintroduce;
    function InitAuthForAllCustomers(const pmcFileName: TFileName): Boolean;
    property DataFolder: TFileName
      read FDataFolder;
  end;


implementation

uses
  u_ModelServices,
  u_ServiceInterfaces;

//==============================================================================
// TRestServerLog
//==============================================================================

procedure TRestServerLog.ComputeFileName;
begin
  inherited ComputeFileName;
  FFileName := StringReplace(FFileName, ' ', '_', [rfReplaceAll]);
end;


//==============================================================================
// TFileRestServer
//==============================================================================

constructor TFileRestServer.Create(const pmcDataFolder: TFileName);
begin
  if not DirectoryExists(pmcDataFolder) then
    raise Exception.Create('A data directory must be specified.');

  CreateWithOwnModel([TAuthGroup, TFileAuthUser], {HandleUserAuthentication=} True, ROOT_NAME_FILE);

  // Logging class initialization
  SetLogClass(TRestServerLog);
  LogFamily.Level := LOG_VERBOSE;
  LogFamily.PerThreadLog := ptIdentifiedInOneFile;
  LogFamily.HighResolutionTimestamp := True;

  // Service registration
  ServiceDefine(TDocumentService, [IDocument], sicShared);

  Server.CreateMissingTables(0, [itoNoAutoCreateUsers]);  // You should always create your own users
  FDataFolder := pmcDataFolder;
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

end.
