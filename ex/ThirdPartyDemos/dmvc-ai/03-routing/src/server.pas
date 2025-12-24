unit server;

{$I mormot.defines.inc}

interface

uses
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.log,
  mormot.core.json,
  mormot.core.rtti,
  mormot.core.variants,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.http.server,
  mormot.rest.memserver,
  mormot.soa.core,
  mormot.net.server,
  mormot.net.http,
  api.interfaces,
  api.impl;

type
  /// Routing Sample Server
  /// Demonstrates different routing patterns in mORMot2:
  /// 1. Interface-based RPC services (all POST with JSON body)
  /// 2. TUriRouter-based RESTful endpoints (proper HTTP verbs and URL parameters)
  TRoutingSampleServer = class
  protected
    fRestServer: TRestServer;
    fHttpServer: TRestHttpServer;
    fApiImpl: TRoutingApi; // Shared implementation for both routing styles
    /// RESTful endpoint handlers (TUriRouter)
    function DoGetUser(Ctxt: THttpServerRequestAbstract): cardinal;
    function DoListUsers(Ctxt: THttpServerRequestAbstract): cardinal;
    function DoCreateUser(Ctxt: THttpServerRequestAbstract): cardinal;
    function DoUpdateUser(Ctxt: THttpServerRequestAbstract): cardinal;
    function DoDeleteUser(Ctxt: THttpServerRequestAbstract): cardinal;
    function DoSearchUsers(Ctxt: THttpServerRequestAbstract): cardinal;
    function DoGetUsersByStatus(Ctxt: THttpServerRequestAbstract): cardinal;
    /// Setup RESTful routes using TUriRouter
    procedure SetupRestfulRoutes;
  public
    constructor Create(const aPort: RawUtf8 = '8080'); reintroduce;
    destructor Destroy; override;
    /// start the HTTP server
    procedure Start;
    /// stop the HTTP server
    procedure Stop;
    /// get the server URL
    function GetServerUrl: RawUtf8;
  end;


implementation


{ TRoutingSampleServer }

constructor TRoutingSampleServer.Create(const aPort: RawUtf8);
begin
  // Create shared implementation instance (used by both routing styles)
  fApiImpl := TRoutingApi.Create;

  // Create REST server (no database needed for this demo)
  fRestServer := TRestServerFullMemory.Create(nil, 'root', false);

  // Register the Routing API implementation (will be created by ServiceDefine)
  fRestServer.ServiceDefine(TRoutingApi, [IRoutingApi], sicShared);

  // Create HTTP server
  fHttpServer := TRestHttpServer.Create(aPort, [fRestServer],
    '+', HTTP_DEFAULT_MODE, {threaded=}32);

  // Enable CORS for all origins (for testing with web clients)
  fHttpServer.AccessControlAllowOrigin := '*';

  // Setup RESTful routes using TUriRouter
  SetupRestfulRoutes;

  TSynLog.Add.Log(sllInfo, 'Routing Sample Server created on port %', [aPort]);
  TSynLog.Add.Log(sllInfo, 'Server URL: %', [GetServerUrl]);
  TSynLog.Add.Log(sllInfo, '');
  TSynLog.Add.Log(sllInfo, '=== RPC-Style Endpoints (Interface-based) ===');
  TSynLog.Add.Log(sllInfo, 'All use POST with JSON body:');
  TSynLog.Add.Log(sllInfo, '  POST %/RoutingApi.GetUser {"id":1}', [GetServerUrl]);
  TSynLog.Add.Log(sllInfo, '  POST %/RoutingApi.GetUserDetails {"id":1,"includeStats":true}', [GetServerUrl]);
  TSynLog.Add.Log(sllInfo, '  POST %/RoutingApi.ListUsers {"filter":"active","limit":10}', [GetServerUrl]);
  TSynLog.Add.Log(sllInfo, '  POST %/RoutingApi.Search {"term":"user"}', [GetServerUrl]);
  TSynLog.Add.Log(sllInfo, '  POST %/RoutingApi.CreateUser {"name":"John","email":"john@example.com"}', [GetServerUrl]);
  TSynLog.Add.Log(sllInfo, '  POST %/RoutingApi.UpdateUser {"id":1,"name":"Jane","email":"jane@example.com"}', [GetServerUrl]);
  TSynLog.Add.Log(sllInfo, '  POST %/RoutingApi.DeleteUser {"id":1}', [GetServerUrl]);
  TSynLog.Add.Log(sllInfo, '  POST %/RoutingApi.GetUsersByStatus {"status":"active","page":1,"pageSize":10}', [GetServerUrl]);
  TSynLog.Add.Log(sllInfo, '  POST %/RoutingApi.BatchDeleteUsers {"ids":[1,2,3]}', [GetServerUrl]);
  TSynLog.Add.Log(sllInfo, '');
  TSynLog.Add.Log(sllInfo, '=== RESTful Endpoints (TUriRouter-based) ===');
  TSynLog.Add.Log(sllInfo, 'Use proper HTTP verbs and URL parameters:');
  TSynLog.Add.Log(sllInfo, '  GET    http://localhost:%/api/users/<id>', [fHttpServer.Port]);
  TSynLog.Add.Log(sllInfo, '  GET    http://localhost:%/api/users?filter=active&limit=10', [fHttpServer.Port]);
  TSynLog.Add.Log(sllInfo, '  GET    http://localhost:%/api/search/<term>', [fHttpServer.Port]);
  TSynLog.Add.Log(sllInfo, '  POST   http://localhost:%/api/users (body: {"name":"John","email":"..."})', [fHttpServer.Port]);
  TSynLog.Add.Log(sllInfo, '  PUT    http://localhost:%/api/users/<id> (body: {"name":"Jane","email":"..."})', [fHttpServer.Port]);
  TSynLog.Add.Log(sllInfo, '  DELETE http://localhost:%/api/users/<id>', [fHttpServer.Port]);
  TSynLog.Add.Log(sllInfo, '  GET    http://localhost:%/api/users/status/<status>?page=1&pageSize=10', [fHttpServer.Port]);
  TSynLog.Add.Log(sllInfo, '');
end;

destructor TRoutingSampleServer.Destroy;
begin
  Stop;
  fHttpServer.Free;
  fRestServer.Free;
  fApiImpl.Free;
  inherited Destroy;
end;

function TRoutingSampleServer.GetServerUrl: RawUtf8;
begin
  result := FormatUtf8('http://localhost:%/root', [fHttpServer.Port]);
end;

procedure TRoutingSampleServer.Start;
begin
  TSynLog.Add.Log(sllInfo, 'Starting Routing Sample Server...');
  TSynLog.Add.Log(sllInfo, 'Press Ctrl+C to stop');
end;

procedure TRoutingSampleServer.Stop;
begin
  TSynLog.Add.Log(sllInfo, 'Stopping Routing Sample Server...');
end;

procedure TRoutingSampleServer.SetupRestfulRoutes;
begin
  // Setup RESTful routes using TUriRouter
  // These demonstrate proper REST conventions with HTTP verbs and URL parameters

  // GET /api/users/<id> - Get user by ID
  fHttpServer.Route.Get('/api/users/<int:id>', DoGetUser);

  // GET /api/users - List users (with optional query params)
  fHttpServer.Route.Get('/api/users', DoListUsers);

  // GET /api/search/<term> - Search users
  fHttpServer.Route.Get('/api/search/<term>', DoSearchUsers);

  // POST /api/users - Create new user
  fHttpServer.Route.Post('/api/users', DoCreateUser);

  // PUT /api/users/<id> - Update user
  fHttpServer.Route.Put('/api/users/<int:id>', DoUpdateUser);

  // DELETE /api/users/<id> - Delete user
  fHttpServer.Route.Delete('/api/users/<int:id>', DoDeleteUser);

  // GET /api/users/status/<status> - Get users by status (with pagination)
  fHttpServer.Route.Get('/api/users/status/<status>', DoGetUsersByStatus);
end;

function TRoutingSampleServer.DoGetUser(Ctxt: THttpServerRequestAbstract): cardinal;
var
  userId: Int64;
  user: TUserDTO;
  json: RawUtf8;
begin
  // Extract ID from URL path: /api/users/123
  if not Ctxt.RouteInt64('id', userId) then
    Exit(HTTP_BADREQUEST);

  try
    // Use shared implementation
    user := fApiImpl.GetUser(userId);
    json := RecordSaveJson(user, TypeInfo(TUserDTO));
    Ctxt.OutContent := json;
    Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
    result := HTTP_SUCCESS;
  except
    on E: EServiceException do
    begin
      Ctxt.OutContent := FormatUtf8('{"error":"%"}', [E.Message]);
      Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
      result := HTTP_NOTFOUND;
    end;
  end;
end;

function TRoutingSampleServer.DoListUsers(Ctxt: THttpServerRequestAbstract): cardinal;
var
  filter: RawUtf8;
  limit: cardinal;
  users: TUserDTOs;
  json: RawUtf8;
begin
  // Extract query parameters: /api/users?filter=active&limit=10
  Ctxt.UrlParam('FILTER=', filter);
  if not Ctxt.UrlParam('LIMIT=', limit) then
    limit := 0;

  // Use shared implementation
  users := fApiImpl.ListUsers(filter, integer(limit));
  json := DynArraySaveJson(users, TypeInfo(TUserDTOs));
  Ctxt.OutContent := json;
  Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
  result := HTTP_SUCCESS;
end;

function TRoutingSampleServer.DoSearchUsers(Ctxt: THttpServerRequestAbstract): cardinal;
var
  term: RawUtf8;
  searchResult: TSearchResultDTO;
  json: RawUtf8;
begin
  // Extract term from URL path: /api/search/john
  term := Ctxt['term'];
  if term = '' then
    Exit(HTTP_BADREQUEST);

  // Use shared implementation
  searchResult := fApiImpl.Search(term);
  json := RecordSaveJson(searchResult, TypeInfo(TSearchResultDTO));
  Ctxt.OutContent := json;
  Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
  result := HTTP_SUCCESS;
end;

function TRoutingSampleServer.DoCreateUser(Ctxt: THttpServerRequestAbstract): cardinal;
var
  doc: TDocVariantData;
  name, email: RawUtf8;
  newUserId: TUserID;
begin
  // Parse JSON body
  if not doc.InitJson(Ctxt.InContent, JSON_FAST) then
    Exit(HTTP_BADREQUEST);

  name := doc.U['name'];
  email := doc.U['email'];

  if (name = '') or (email = '') then
    Exit(HTTP_BADREQUEST);

  try
    // Use shared implementation
    newUserId := fApiImpl.CreateUser(name, email);
    Ctxt.OutContent := FormatUtf8('{"id":%}', [newUserId]);
    Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
    result := HTTP_CREATED;
  except
    on E: EServiceException do
    begin
      Ctxt.OutContent := FormatUtf8('{"error":"%"}', [E.Message]);
      Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
      result := HTTP_BADREQUEST;
    end;
  end;
end;

function TRoutingSampleServer.DoUpdateUser(Ctxt: THttpServerRequestAbstract): cardinal;
var
  userId: Int64;
  doc: TDocVariantData;
  name, email: RawUtf8;
  success: boolean;
begin
  // Extract ID from URL path
  if not Ctxt.RouteInt64('id', userId) then
    Exit(HTTP_BADREQUEST);

  // Parse JSON body
  if not doc.InitJson(Ctxt.InContent, JSON_FAST) then
    Exit(HTTP_BADREQUEST);

  name := doc.U['name'];
  email := doc.U['email'];

  try
    // Use shared implementation
    success := fApiImpl.UpdateUser(userId, name, email);
    if success then
    begin
      Ctxt.OutContent := '{"success":true}';
      Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
      result := HTTP_SUCCESS;
    end
    else
    begin
      Ctxt.OutContent := '{"success":false}';
      Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
      result := HTTP_NOTFOUND;
    end;
  except
    on E: EServiceException do
    begin
      Ctxt.OutContent := FormatUtf8('{"error":"%"}', [E.Message]);
      Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
      result := HTTP_NOTFOUND;
    end;
  end;
end;

function TRoutingSampleServer.DoDeleteUser(Ctxt: THttpServerRequestAbstract): cardinal;
var
  userId: Int64;
  success: boolean;
begin
  // Extract ID from URL path
  if not Ctxt.RouteInt64('id', userId) then
    Exit(HTTP_BADREQUEST);

  // Use shared implementation
  success := fApiImpl.DeleteUser(userId);
  if success then
  begin
    Ctxt.OutContent := '{"success":true}';
    Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
    result := HTTP_SUCCESS;
  end
  else
  begin
    Ctxt.OutContent := '{"success":false}';
    Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
    result := HTTP_NOTFOUND;
  end;
end;

function TRoutingSampleServer.DoGetUsersByStatus(Ctxt: THttpServerRequestAbstract): cardinal;
var
  status: RawUtf8;
  page, pageSize: cardinal;
  users: TUserDTOs;
  json: RawUtf8;
begin
  // Extract status from URL path: /api/users/status/active
  status := Ctxt['status'];
  if status = '' then
    Exit(HTTP_BADREQUEST);

  // Extract pagination from query params
  if not Ctxt.UrlParam('PAGE=', page) then
    page := 1;
  if not Ctxt.UrlParam('PAGESIZE=', pageSize) then
    pageSize := 10;

  // Use shared implementation
  users := fApiImpl.GetUsersByStatus(status, integer(page), integer(pageSize));
  json := DynArraySaveJson(users, TypeInfo(TUserDTOs));
  Ctxt.OutContent := json;
  Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
  result := HTTP_SUCCESS;
end;


end.
