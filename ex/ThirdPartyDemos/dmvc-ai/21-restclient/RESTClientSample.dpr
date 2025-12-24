program RESTClientSample;

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$APPTYPE CONSOLE}
{$endif OSWINDOWS}

uses
  {$I mormot.uses.inc}
  SysUtils,
  Classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.json,
  mormot.core.data,
  mormot.core.variants,
  mormot.core.rtti,
  mormot.net.http,
  mormot.net.client,
  dto.types in 'src\dto.types.pas';

type
  /// Helper class for organized REST client examples
  TRestClientExamples = class
  private
    FBaseUrl: RawUtf8;
    FClient: THttpClientSocket;
    FUserAgent: RawUtf8;
    FTimeout: Integer;
    procedure SetupClient(const aBaseUrl: RawUtf8);
    function DoRequest(const aMethod, aUrl: RawUtf8; const aBody: RawUtf8 = '';
      const aHeaders: RawUtf8 = ''): RawUtf8;
  public
    constructor Create;
    destructor Destroy; override;

    // Configuration
    procedure BasicConfiguration;

    // HTTP Methods
    procedure SimpleGETRequest;
    procedure GETWithDeserialization;
    procedure GETUsersList;
    procedure POSTWithJSONBody;
    procedure PUTRequest;
    procedure DELETERequest;

    // Advanced Features
    procedure CustomHeaders;
    procedure AuthenticationExamples;
    procedure URLEncodedBody;
    procedure ErrorHandling;
    procedure HTTPBinExamples;
  end;

{ TRestClientExamples }

constructor TRestClientExamples.Create;
begin
  inherited Create;
  FUserAgent := 'mORMot2 REST Client Example/1.0';
  FTimeout := 5000; // 5 second timeout
  FClient := THttpClientSocket.Create(FTimeout);
  FClient.UserAgent := FUserAgent;
  SetupClient('https://jsonplaceholder.typicode.com');
end;

destructor TRestClientExamples.Destroy;
begin
  FClient.Free;
  inherited;
end;

procedure TRestClientExamples.SetupClient(const aBaseUrl: RawUtf8);
begin
  FBaseUrl := aBaseUrl;
  FClient.Close; // Close any existing connection
end;

function TRestClientExamples.DoRequest(const aMethod, aUrl: RawUtf8;
  const aBody: RawUtf8 = ''; const aHeaders: RawUtf8 = ''): RawUtf8;
var
  fullUrl: RawUtf8;
  status: Integer;
  contentType: RawUtf8;
begin
  // Construct full URL
  if (aUrl <> '') and (aUrl[1] = '/') then
    fullUrl := FBaseUrl + aUrl
  else
    fullUrl := aUrl;

  // Determine content type
  if aBody <> '' then
    contentType := JSON_CONTENT_TYPE
  else
    contentType := '';

  // Perform request
  status := FClient.Request(fullUrl, aMethod, 0, aHeaders, aBody, contentType);

  if status = 0 then
    raise Exception.CreateFmt('Connection failed to %s', [fullUrl]);

  Result := FClient.Content;
end;

procedure TRestClientExamples.BasicConfiguration;
begin
  ConsoleWrite('=== BASIC CONFIGURATION ===');
  ConsoleWrite('Base URL: %', [FBaseUrl]);
  ConsoleWrite('User Agent: %', [FUserAgent]);
  ConsoleWrite('Timeout: % ms', [FTimeout]);
  ConsoleWrite('');
end;

procedure TRestClientExamples.SimpleGETRequest;
var
  response: RawUtf8;
  doc: TDocVariantData;
begin
  ConsoleWrite('=== SIMPLE GET REQUEST ===');
  try
    response := DoRequest('GET', '/posts/1');

    ConsoleWrite('Status: OK');
    ConsoleWrite('Response Length: % bytes', [Length(response)]);
    ConsoleWrite('Response Body:');

    // Parse and pretty-print JSON
    if doc.InitJson(response) then
      ConsoleWrite(doc.ToJson('', '', jsonHumanReadable))
    else
      ConsoleWrite(response);

    ConsoleWrite('');
  except
    on E: Exception do
      ConsoleWrite('Error: %', [E.Message]);
  end;
end;

procedure TRestClientExamples.GETWithDeserialization;
var
  response: RawUtf8;
  post: TPostDto;
begin
  ConsoleWrite('=== GET WITH DESERIALIZATION ===');
  try
    response := DoRequest('GET', '/posts/1');

    // Deserialize JSON to record
    if RecordLoadJson(post, response, TypeInfo(TPostDto)) then
    begin
      ConsoleWrite('Deserialized Post:');
      ConsoleWrite('  ID: %', [post.Id]);
      ConsoleWrite('  User ID: %', [post.UserId]);
      ConsoleWrite('  Title: %', [Utf8ToString(post.Title)]);
      ConsoleWrite('  Body: %...', [Copy(Utf8ToString(post.Body), 1, 50)]);
    end
    else
      ConsoleWrite('Failed to deserialize response');

    ConsoleWrite('');
  except
    on E: Exception do
      ConsoleWrite('Error: %', [E.Message]);
  end;
end;

procedure TRestClientExamples.GETUsersList;
var
  response: RawUtf8;
  users: TUserDtos;
  i: Integer;
begin
  ConsoleWrite('=== GET USERS LIST ===');
  try
    response := DoRequest('GET', '/users');

    // Deserialize JSON array to dynamic array
    DynArrayLoadJson(users, response, TypeInfo(TUserDtos));

    ConsoleWrite('Found % users:', [Length(users)]);
    ConsoleWrite('');
    for i := 0 to High(users) do
    begin
      ConsoleWrite('  %) % (@%) - %', [
        users[i].Id,
        Utf8ToString(users[i].Name),
        Utf8ToString(users[i].Username),
        Utf8ToString(users[i].Email)
      ]);
    end;

    ConsoleWrite('');
  except
    on E: Exception do
      ConsoleWrite('Error: %', [E.Message]);
  end;
end;

procedure TRestClientExamples.POSTWithJSONBody;
var
  post: TPostDto;
  requestBody, response: RawUtf8;
  responsePost: TPostDto;
begin
  ConsoleWrite('=== POST WITH JSON BODY ===');
  try
    // Create post data
    post.UserId := 1;
    post.Title := 'Example Post from mORMot2';
    post.Body := 'This is an example post created with mORMot2 HTTP client';

    // Serialize to JSON
    requestBody := RecordSaveJson(post, TypeInfo(TPostDto));

    // Send POST request
    response := DoRequest('POST', '/posts', requestBody,
      'Content-Type: application/json');

    ConsoleWrite('POST Response:');
    if RecordLoadJson(responsePost, response, TypeInfo(TPostDto)) then
    begin
      ConsoleWrite('  Created ID: %', [responsePost.Id]);
      ConsoleWrite('  Title: %', [Utf8ToString(responsePost.Title)]);
    end
    else
      ConsoleWrite(response);

    ConsoleWrite('');
  except
    on E: Exception do
      ConsoleWrite('Error: %', [E.Message]);
  end;
end;

procedure TRestClientExamples.PUTRequest;
var
  post: TPostDto;
  requestBody, response: RawUtf8;
  responsePost: TPostDto;
begin
  ConsoleWrite('=== PUT REQUEST ===');
  try
    // Update post data
    post.Id := 1;
    post.UserId := 1;
    post.Title := 'Updated Post';
    post.Body := 'Content updated via PUT with mORMot2';

    // Serialize to JSON
    requestBody := RecordSaveJson(post, TypeInfo(TPostDto));

    // Send PUT request
    response := DoRequest('PUT', '/posts/1', requestBody,
      'Content-Type: application/json');

    ConsoleWrite('PUT Response:');
    if RecordLoadJson(responsePost, response, TypeInfo(TPostDto)) then
    begin
      ConsoleWrite('  ID: %', [responsePost.Id]);
      ConsoleWrite('  Title: %', [Utf8ToString(responsePost.Title)]);
      ConsoleWrite('  Body: %', [Utf8ToString(responsePost.Body)]);
    end
    else
      ConsoleWrite(response);

    ConsoleWrite('');
  except
    on E: Exception do
      ConsoleWrite('Error: %', [E.Message]);
  end;
end;

procedure TRestClientExamples.DELETERequest;
var
  response: RawUtf8;
begin
  ConsoleWrite('=== DELETE REQUEST ===');
  try
    response := DoRequest('DELETE', '/posts/1');

    ConsoleWrite('DELETE Response:');
    if response = '{}' then
      ConsoleWrite('  Success (empty response)')
    else
      ConsoleWrite('  %', [response]);

    ConsoleWrite('');
  except
    on E: Exception do
      ConsoleWrite('Error: %', [E.Message]);
  end;
end;

procedure TRestClientExamples.CustomHeaders;
var
  response: RawUtf8;
  headers: RawUtf8;
begin
  ConsoleWrite('=== CUSTOM HEADERS ===');
  try
    headers := 'X-Custom-Header: mORMot2'#13#10 +
               'X-Request-ID: 12345';

    response := DoRequest('GET', '/posts/1', '', headers);

    ConsoleWrite('Request with custom headers completed');
    ConsoleWrite('Response Length: % bytes', [Length(response)]);
    ConsoleWrite('');
  except
    on E: Exception do
      ConsoleWrite('Error: %', [E.Message]);
  end;
end;

procedure TRestClientExamples.AuthenticationExamples;
var
  response: RawUtf8;
  headers: RawUtf8;
  auth: RawUtf8;
begin
  ConsoleWrite('=== AUTHENTICATION EXAMPLES ===');

  // Basic Authentication
  auth := BinToBase64('testuser:testpass');
  ConsoleWrite('Basic Auth Header: Basic %', [auth]);

  // Bearer Token
  ConsoleWrite('Bearer Auth Header: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.test.token');

  // Test with httpbin
  SetupClient('https://httpbin.org');
  try
    headers := 'Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.test.token';
    response := DoRequest('GET', '/bearer', '', headers);
    ConsoleWrite('Bearer test completed: %', [response]);
  except
    on E: Exception do
      ConsoleWrite('Bearer test failed: %', [E.Message]);
  end;

  // Restore base URL
  SetupClient('https://jsonplaceholder.typicode.com');
  ConsoleWrite('');
end;

procedure TRestClientExamples.URLEncodedBody;
var
  response: RawUtf8;
  body: RawUtf8;
begin
  ConsoleWrite('=== URL ENCODED BODY ===');
  SetupClient('https://httpbin.org');
  try
    body := 'field1=value1&field2=Daniele+Teti&framework=mORMot2';

    response := DoRequest('POST', '/post', body,
      'Content-Type: application/x-www-form-urlencoded');

    ConsoleWrite('URL Encoded Response received');
    ConsoleWrite('Response Length: % bytes', [Length(response)]);
    ConsoleWrite('');
  except
    on E: Exception do
      ConsoleWrite('Error: %', [E.Message]);
  end;

  // Restore base URL
  SetupClient('https://jsonplaceholder.typicode.com');
end;

procedure TRestClientExamples.ErrorHandling;
var
  response: RawUtf8;
begin
  ConsoleWrite('=== ERROR HANDLING ===');

  // Test 404
  try
    response := DoRequest('GET', '/posts/999999');
    ConsoleWrite('404 Test - Response: %', [response]);
  except
    on E: Exception do
      ConsoleWrite('404 Exception: %', [E.Message]);
  end;

  // Test timeout
  SetupClient('https://httpbin.org');
  try
    FTimeout := 100; // 100ms timeout
    FClient.ReceiveTimeout := FTimeout;
    response := DoRequest('GET', '/delay/5'); // 5 seconds delay
    FTimeout := 5000; // Restore
    FClient.ReceiveTimeout := FTimeout;
  except
    on E: Exception do
    begin
      ConsoleWrite('Timeout Exception: %', [E.Message]);
      FTimeout := 5000; // Restore
      FClient.ReceiveTimeout := FTimeout;
    end;
  end;

  // Restore base URL
  SetupClient('https://jsonplaceholder.typicode.com');
  ConsoleWrite('');
end;

procedure TRestClientExamples.HTTPBinExamples;
var
  response: RawUtf8;
  doc: TDocVariantData;
  headers: RawUtf8;
begin
  ConsoleWrite('=== HTTPBIN EXAMPLES ===');
  SetupClient('https://httpbin.org');

  // Test headers
  try
    headers := 'X-Test-Header: mORMot2';
    response := DoRequest('GET', '/headers', '', headers);

    ConsoleWrite('Headers Test Status: OK');

    // Parse JSON response
    if doc.InitJson(response) then
    begin
      ConsoleWrite('User-Agent sent: %', [doc.GetValueOrDefault('headers.User-Agent', '')]);
      ConsoleWrite('Custom Header sent: %', [doc.GetValueOrDefault('headers.X-Test-Header', '')]);
    end;
  except
    on E: Exception do
      ConsoleWrite('Headers test failed: %', [E.Message]);
  end;

  // Test IP
  try
    response := DoRequest('GET', '/ip');
    ConsoleWrite('IP Response: %', [response]);
  except
    on E: Exception do
      ConsoleWrite('IP test failed: %', [E.Message]);
  end;

  // Restore base URL
  SetupClient('https://jsonplaceholder.typicode.com');
  ConsoleWrite('');
end;

// Main Program
procedure RunExamples;
var
  examples: TRestClientExamples;
begin
  ConsoleWrite('mORMot2 REST Client - Complete Examples');
  ConsoleWrite('========================================');
  ConsoleWrite('');

  examples := TRestClientExamples.Create;
  try
    examples.BasicConfiguration;
    examples.SimpleGETRequest;
    examples.GETWithDeserialization;
    examples.GETUsersList;
    examples.POSTWithJSONBody;
    examples.PUTRequest;
    examples.DELETERequest;
    examples.CustomHeaders;
    examples.AuthenticationExamples;
    examples.URLEncodedBody;
    examples.HTTPBinExamples;
    examples.ErrorHandling;
  finally
    examples.Free;
  end;

  ConsoleWrite('========================================');
  ConsoleWrite('Examples completed. Press ENTER to exit...');
end;

begin
  try
    RunExamples;
    {$ifdef OSWINDOWS}
    ReadLn;
    {$endif OSWINDOWS}
  except
    on E: Exception do
    begin
      ConsoleWrite('Error: %: %', [E.ClassName, E.Message]);
      {$ifdef OSWINDOWS}
      ReadLn;
      {$endif OSWINDOWS}
      ExitCode := 1;
    end;
  end;
end.
