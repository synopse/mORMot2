unit auth.handler;

interface

uses
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.log,
  mormot.orm.core,
  mormot.rest.core,
  mormot.rest.server;

type
  /// Custom authentication handler with file-based session data persistence
  // Port of DMVC file-based session concept
  //
  // Key design decisions:
  // - Session DATA is persisted by USER ID (not session ID)
  // - This allows data to survive server restarts
  // - User re-authenticates and their previous data is restored
  // - This matches how most real-world systems work
  //
  // Why not persist by session ID?
  // - mORMot sessions use cryptographic signatures with a private salt
  // - The salt is lost on restart, making old session IDs invalid
  // - Forcing users to keep old session IDs would be a security issue
  TFileBasedAuthHandler = class(TRestServerAuthenticationDefault)
  private
    fSessionFilePath: RawUtf8;
    /// Save user's session data to file (keyed by user ID)
    procedure SaveUserDataToFile(user: TAuthUser);
    /// Load user's session data from file (keyed by user ID)
    procedure LoadUserDataFromFile(user: TAuthUser);
    /// Get the filename for a user's session data
    function GetUserDataFileName(userID: TID): RawUtf8;
  protected
    /// Called after a new session is created - loads persisted data
    procedure SessionCreate(Ctxt: TRestServerUriContext;
      var User: TAuthUser); override;
  public
    /// Called on each request - saves data if modified
    function RetrieveSession(Ctxt: TRestServerUriContext): TAuthSession; override;
    constructor Create(aServer: TRestServer); override;
    destructor Destroy; override;
    /// Scan and report existing session files on startup
    procedure ReportPersistedSessions;
    property SessionFilePath: RawUtf8 read fSessionFilePath write fSessionFilePath;
  end;

implementation

uses
  SysUtils,
  Classes,
  mormot.core.data,
  mormot.core.variants,
  mormot.core.datetime,
  mormot.core.text;

{ TFileBasedAuthHandler }

constructor TFileBasedAuthHandler.Create(aServer: TRestServer);
begin
  inherited Create(aServer);

  // Use 'sessions' subfolder in program directory
  fSessionFilePath := IncludeTrailingPathDelimiter(
    Executable.ProgramFilePath + 'sessions');

  // Create sessions directory if it doesn't exist
  if not DirectoryExists(fSessionFilePath) then
    CreateDir(fSessionFilePath);

  TSynLog.Add.Log(sllDebug, 'File-based session storage initialized: %',
    [fSessionFilePath]);
end;

destructor TFileBasedAuthHandler.Destroy;
begin
  inherited;
end;

function TFileBasedAuthHandler.GetUserDataFileName(userID: TID): RawUtf8;
begin
  // Key by user ID, not session ID - survives server restarts
  Result := fSessionFilePath + 'user_' + IntToStr(userID) + '_session.json';
end;

procedure TFileBasedAuthHandler.SaveUserDataToFile(user: TAuthUser);
var
  filename: RawUtf8;
  json: RawUtf8;
  doc: TDocVariantData;
begin
  if user = nil then
    exit;

  filename := GetUserDataFileName(user.ID);

  // Create JSON document with user session data
  doc.InitFast;
  doc.AddValue('user_id', user.ID);
  doc.AddValue('user_name', user.LogonName);
  doc.AddValue('data', user.Data);
  doc.AddValue('saved_at', NowToString(true));

  json := doc.ToJson;

  // Save to file
  FileFromString(json, filename);

  TSynLog.Add.Log(sllTrace, 'User % session data saved to: %',
    [user.LogonName, filename]);
end;

procedure TFileBasedAuthHandler.LoadUserDataFromFile(user: TAuthUser);
var
  filename: RawUtf8;
  json: RawUtf8;
  doc: TDocVariantData;
  savedData: RawUtf8;
begin
  if user = nil then
    exit;

  filename := GetUserDataFileName(user.ID);

  // Check if file exists
  if not FileExists(filename) then
  begin
    TSynLog.Add.Log(sllTrace, 'No persisted data for user %: %',
      [user.LogonName, filename]);
    exit;
  end;

  // Load from file
  json := StringFromFile(filename);
  if json = '' then
    exit;

  // Parse JSON
  doc.InitJson(json, JSON_FAST);

  // Verify user ID matches (security check)
  if doc.I['user_id'] <> user.ID then
  begin
    TSynLog.Add.Log(sllWarning, 'User ID mismatch in session file % (expected %, found %)',
      [filename, user.ID, doc.I['user_id']]);
    exit;
  end;

  // Restore session data
  savedData := doc.U['data'];
  if savedData <> '' then
  begin
    user.Data := savedData;
    TSynLog.Add.Log(sllInfo, 'User % session data restored from: % (saved at %)',
      [user.LogonName, filename, doc.U['saved_at']]);
  end;
end;

procedure TFileBasedAuthHandler.SessionCreate(Ctxt: TRestServerUriContext;
  var User: TAuthUser);
begin
  // CRITICAL: Load persisted data BEFORE calling inherited
  // This restores previous session data when user re-authenticates
  LoadUserDataFromFile(User);

  // Call inherited to create the actual session
  inherited SessionCreate(Ctxt, User);

  TSynLog.Add.Log(sllDebug, 'Session created for user % with restored data',
    [User.LogonName]);
end;

function TFileBasedAuthHandler.RetrieveSession(
  Ctxt: TRestServerUriContext): TAuthSession;
var
  previousData: RawUtf8;
begin
  // Call inherited to get session from memory
  Result := inherited RetrieveSession(Ctxt);

  if Result <> nil then
  begin
    // Remember current data to detect changes
    previousData := Result.User.Data;

    // Check if data was modified during this request cycle
    // (we'll save on the NEXT request to capture any changes)
    // For immediate persistence, we save every time
    if Result.User.Data <> '' then
      SaveUserDataToFile(Result.User);
  end;
end;

procedure TFileBasedAuthHandler.ReportPersistedSessions;
var
  searchRec: TSearchRec;
  pattern: RawUtf8;
  count: integer;
begin
  pattern := fSessionFilePath + 'user_*_session.json';
  count := 0;

  WriteLn('Persisted session files:');
  if FindFirst(pattern, faAnyFile, searchRec) = 0 then
  begin
    repeat
      Inc(count);
      WriteLn('  - ', searchRec.Name);
    until FindNext(searchRec) <> 0;
    FindClose(searchRec);
  end;

  if count = 0 then
    WriteLn('  (none)')
  else
    WriteLn('  Total: ', count, ' user session file(s)');
  WriteLn;
end;

end.
