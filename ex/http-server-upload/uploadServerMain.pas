unit uploadServerMain;

{$I mormot.defines.inc}

interface

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.unicode,
  mormot.net.http,
  mormot.net.server,
  mormot.net.async,
  mormot.net.client; // for THttpMultiPartDecoder

type
  /// a HTTP server receiving file uploads with a constant memory usage
  // - the incoming request body never reaches the RAM: OnBodyDownload spools
  // it into a temporary file, then THttpMultiPartDecoder walks that file
  // section by section - see https://github.com/synopse/mORMot2/issues/292
  TUploadServer = class
  protected
    fHttp: THttpAsyncServer;
    fDestFolder, fSpoolFolder, fStagingFolder: TFileName;
    // the two halves of the process
    function DoBodyDownload(const aUrl, aMethod, aInHeaders, aInContentType,
      aRemoteIP: RawUtf8; aContentLength: Int64): TStream;
    function DoRequest(Ctxt: THttpServerRequestAbstract): cardinal;
    // POST /upload: decode the spooled body, and store its file sections
    function DoUpload(Ctxt: THttpServerRequestAbstract): cardinal;
    // turn the client supplied file name into a name within fDestFolder
    function SafeDestFile(const aClientName: RawUtf8; out aDest: TFileName): boolean;
    // publish one staging file, working around the platform rename semantics
    function Publish(const aStaging, aDest: TFileName): boolean;
  public
    /// start the HTTP server on the supplied port
    constructor Create(const aPort: RawUtf8; const aDestFolder, aSpoolFolder: TFileName;
      aMaxBodyMB: integer);
    /// release the HTTP server
    destructor Destroy; override;
  end;

var
  silent: boolean;

procedure Main;

implementation

const
  UPLOAD_URI = '/upload';

  /// sub-folder of the destination, holding the files being written
  // - must be on the SAME file system as the destination, otherwise the final
  // rename would fail (POSIX EXDEV, and MoveFileW without MOVEFILE_COPY_ALLOWED)
  STAGING_SUBFOLDER = '.staging';

  /// refuse a body made of an unreasonable number of sections
  // - each file section costs one staging file, so an otherwise legitimate
  // 64 MB body made of tiny parts could exhaust inodes and CPU
  MAX_PARTS = 64;

  // Win32 reserved device names: creating one of these opens the DEVICE, not a
  // file - THttpMultiPartDecoder documentation asks callers to reject them
  DOS_DEVICES: array[0 .. 21] of RawUtf8 = (
    'CON', 'PRN', 'AUX', 'NUL',
    'COM1', 'COM2', 'COM3', 'COM4', 'COM5', 'COM6', 'COM7', 'COM8', 'COM9',
    'LPT1', 'LPT2', 'LPT3', 'LPT4', 'LPT5', 'LPT6', 'LPT7', 'LPT8', 'LPT9');

  // the browser form served at the root URI, to try the sample by hand
  UPLOAD_FORM: RawUtf8 =
    '<!doctype html><html><head><meta charset="utf-8">' +
    '<title>mORMot upload sample</title></head><body>' +
    '<h1>mORMot streamed upload</h1>' +
    '<form method="post" action="/upload" enctype="multipart/form-data">' +
    '<p><input type="file" name="file" multiple></p>' +
    '<p><input type="text" name="comment" placeholder="a plain form field"></p>' +
    '<p><button type="submit">Upload</button></p>' +
    '</form></body></html>';


{ TUploadServer }

constructor TUploadServer.Create(const aPort: RawUtf8;
  const aDestFolder, aSpoolFolder: TFileName; aMaxBodyMB: integer);
begin
  fDestFolder := EnsureDirectoryExists(aDestFolder, EOSException);
  fStagingFolder := EnsureDirectoryExists(
    fDestFolder + STAGING_SUBFOLDER, EOSException);
  fSpoolFolder := EnsureDirectoryExists(aSpoolFolder, EOSException);
  // a process killed mid-upload leaves its staging and spool files behind, so
  // sweep them at startup - they are useless once their request is gone
  DirectoryDelete(fStagingFolder, FILES_ALL, {filesonly=}true);
  fHttp := THttpAsyncServer.Create(aPort, nil, nil, 'upload', {threads=}4);
  // a body larger than this is refused with a 413 while it is received, so
  // neither the RAM nor the spool disk can be filled by a single client - this
  // applies to a Content-Length body AND to a chunked one, the latter only
  // because DoBodyDownload below returns a stream
  // - note: mORMot reads a value <= 0 as "no limit at all", so the caller is
  // responsible for never passing one (see Main below)
  fHttp.MaximumAllowedContentLength := Int64(aMaxBodyMB) shl 20;
  fHttp.OnBodyDownload := DoBodyDownload;
  fHttp.OnRequest := DoRequest;
  fHttp.WaitStarted;
end;

destructor TUploadServer.Destroy;
begin
  fHttp.Free;
  inherited Destroy;
end;

function TUploadServer.DoBodyDownload(const aUrl, aMethod, aInHeaders,
  aInContentType, aRemoteIP: RawUtf8; aContentLength: Int64): TStream;
begin
  // this event is called once the headers have been parsed and a body is
  // actually following - returning nil falls back to the default in-memory
  // buffering, which is what every other URI of this server wants
  result := nil;
  if not IdemPropNameU(aMethod, 'POST') or
     not IdemPropNameU(aUrl, UPLOAD_URI) then
    exit;
  // one spool file per request - TemporaryFileName() is thread-safe, which
  // matters because this event runs on the server event loop threads, and it
  // returns a name which does not exist yet
  // - TFileStreamEventuallyDelete removes that file when the server releases
  // the stream, i.e. once the request has been processed: nothing to track,
  // and nothing to clean up on error
  // - fmShareRead is what allows the file to be read back by its name while
  // this stream is still open - mandatory on Windows, harmless elsewhere
  result := TFileStreamEventuallyDelete.Create(
    TemporaryFileName(fSpoolFolder, 'upload'), fmCreate or fmShareRead);
end;

function TUploadServer.DoRequest(Ctxt: THttpServerRequestAbstract): cardinal;
begin
  if IdemPropNameU(Ctxt.Method, 'POST') and
     IdemPropNameU(Ctxt.Url, UPLOAD_URI) then
    result := DoUpload(Ctxt)
  else if IdemPropNameU(Ctxt.Method, 'GET') and
          (IdemPropNameU(Ctxt.Url, '/') or
           (Ctxt.Url = '')) then
  begin
    Ctxt.OutContent := UPLOAD_FORM;
    Ctxt.OutContentType := HTML_CONTENT_TYPE;
    result := HTTP_SUCCESS;
  end
  else
    result := HTTP_NOTFOUND;
end;

function TUploadServer.SafeDestFile(const aClientName: RawUtf8;
  out aDest: TFileName): boolean;
var
  fn, base: TFileName;
  last: char;
  dot: PtrInt;
begin
  result := false;
  // the "filename" parameter is raw client input: it may hold a path, a '..'
  // sequence, or the delimiter of the OTHER operating system - so any path is
  // stripped here and only the last name kept, i.e. 'a/../../b.txt' is stored
  // as 'b.txt' rather than being rejected, which keeps browsers sending a full
  // local path usable
  fn := NormalizeFileName(Utf8ToString(aClientName)); // unify \ and /
  fn := ExtractFileName(fn);                          // drop any path
  // what is left can still be unusable, e.g. '..' has no path to strip
  if (fn = '') or
     (fn = '.') or
     (fn = '..') or
     not SafeFileName(fn) then
    exit;
  // Win32 silently drops a trailing '.' or ' ', so the stored file would not
  // have the name we validated - rejected on every platform, for one behaviour
  last := fn[length(fn)];
  if (last = '.') or
     (last = ' ') then
    exit;
  // same reasoning for the DOS device names: 'CON' or 'NUL.txt' would write to
  // a device on Windows, so they are refused everywhere rather than turning
  // this sample into a platform-dependent one
  // - the name to check is what comes before the FIRST dot, with any trailing
  // space removed, which is how Win32 resolves a device - not the file name
  // without its last extension, otherwise 'con.txt.txt' would slip through
  dot := Pos('.', fn);
  if dot = 0 then
    base := fn
  else
    base := copy(fn, 1, dot - 1);
  while (base <> '') and
        (base[length(base)] = ' ') do
    SetLength(base, length(base) - 1); // Win32 ignores trailing spaces, too
  if FindPropName(DOS_DEVICES, StringToUtf8(base)) >= 0 then
    exit;
  aDest := fDestFolder + fn;
  result := true;
end;

function TUploadServer.Publish(const aStaging, aDest: TFileName): boolean;
begin
  // try the plain rename first: on POSIX it atomically replaces any existing
  // target, so nothing is ever missing in between
  result := RenameFile(aStaging, aDest);
  if result then
    exit;
  // it failed - most likely because the target exists and this is Windows,
  // where RenameFile() is MoveFileW() without MOVEFILE_REPLACE_EXISTING
  // - deleting the target first is what makes both platforms behave alike, but
  // it is deliberately the SECOND attempt: doing it upfront would destroy the
  // previous file even when the new one cannot be put in place
  // - a real service would rather not replace at all, e.g. by versioning the
  // name; this sample settles on last-wins and keeps the window as small as
  // the platform allows
  if not DeleteFile(aDest) then
    exit;
  result := RenameFile(aStaging, aDest);
end;

function TUploadServer.DoUpload(Ctxt: THttpServerRequestAbstract): cardinal;
var
  mp: THttpMultiPartDecoder;
  ct, fields: RawUtf8;
  dest: TFileName;
  // each section is written to a staging file of its own, and renamed to its
  // final name only once the whole body proved valid
  staging, final: array of TFileName;
  part: TFileStreamEx;
  files, parts, i: integer;
begin
  result := HTTP_BADREQUEST;
  Ctxt.OutContentType := TEXT_CONTENT_TYPE;
  files := 0;
  parts := 0;
  fields := '';
  final := nil;   // silence the compiler: managed types are nil anyway
  staging := nil;
  // the spooled body is supplied as InContentStream, rewinded and still open
  // - with a TFileStreamEx (as here), Ctxt.InContent also holds its local file
  // name and InContentType is STATICFILE_CONTENT_TYPE, mirroring the response
  // process - but reading the stream directly needs no file name at all
  if (Ctxt.InContentStream = nil) or
     not FindNameValue(Ctxt.InHeaders, 'CONTENT-TYPE:', ct) or
     // CreateFromContentType() only looks for a "boundary" parameter, so the
     // media type itself has to be checked here - otherwise 'text/plain;
     // boundary=x' would be decoded as multipart and stored
     not IdemPChar(pointer(ct), 'MULTIPART/FORM-DATA') then
  begin
    Ctxt.OutContent := 'expected a multipart/form-data body';
    exit;
  end;
  try
    try
      mp := THttpMultiPartDecoder.CreateFromContentType(Ctxt.InContentStream, ct);
      try
        while mp.NextPart do
        begin
          inc(parts);
          if parts > MAX_PARTS then
          begin
            Ctxt.OutContent := 'too many sections in this body';
            exit; // the outer finally removes what was written so far
          end;
          if mp.FileName = '' then
            // a plain form field - this sample only reports the field names, so
            // its value is left for NextPart to drain from the spool file
            fields := FormatUtf8('% %', [fields, mp.Name])
          else
          begin
            // a file section: stream it out, never buffering the whole content
            if not SafeDestFile(mp.FileName, dest) then
            begin
              Ctxt.OutContent := 'unusable file name';
              exit;
            end;
            if files = length(final) then
            begin
              SetLength(final, NextGrow(files));
              SetLength(staging, length(final));
            end;
            final[files] := dest;
            // the staging name is unique, and lives in a sub-folder of the
            // destination: same file system, so the rename below stays cheap,
            // but out of the folder being served - a process killed mid-upload
            // would otherwise leave a partial file among the published ones
            // - a shared '<name>.part' would not do either: two sections of one
            // request, or two concurrent requests, carrying the same file name
            // would write over each other and publish a corrupted file that
            // still looks complete
            staging[files] := TemporaryFileName(fStagingFolder, 'part');
            inc(files);
            part := TFileStreamEx.Create(staging[files - 1], fmCreate);
            try
              // Content is incremental and its size is unknown in advance, so
              // TStream.CopyFrom(src, 0) would silently copy nothing here
              StreamCopyUntilEnd(mp.Content, part);
            finally
              part.Free;
            end;
          end;
        end;
        // only a final --boundary-- delimiter proves the body was complete: a
        // connection cut in the middle would otherwise leave truncated files
        // looking exactly like good ones
        if not mp.Close then
        begin
          Ctxt.OutContent := 'truncated or malformed multipart body';
          exit;
        end;
      finally
        mp.Free;
      end;
      // the upload is valid: publish each file under its final name
      for i := 0 to files - 1 do
        if not Publish(staging[i], final[i]) then
        begin
          // report no success for a file which was not stored - note that the
          // files published by the previous iterations do stay: there is no
          // atomic way of publishing a whole set, so this is a partial result
          // - only the client supplied name is echoed back, never our path
          Ctxt.OutContent := FormatUtf8('could not store %',
            [ExtractFileName(final[i])]);
          result := HTTP_SERVERERROR;
          exit;
        end;
      Ctxt.OutContent := FormatUtf8('% file(s) stored, fields:%', [files, fields]);
      result := HTTP_SUCCESS;
    except
      on EHttpMultiPart do
        // malformed input is a client error, not a server failure: Read() does
        // raise on truncated data, rather than returning it as a short section
        // - the exception text names our internal classes, so it is not echoed
        Ctxt.OutContent := 'invalid multipart body';
    end;
  finally
    // whatever went wrong, no staging file survives the request - a staging
    // file already renamed above simply is not there any more
    for i := 0 to files - 1 do
      DeleteFile(staging[i]);
  end;
end;


procedure Main;
var
  cmd: TExecutableCommandLine;
  port: RawUtf8;
  dest, spool: TFileName;
  maxmb: integer;
  server: TUploadServer;
begin
  cmd := Executable.Command;
  port  := cmd.Param('&port', 'the HTTP #port to listen on', '8888');
  dest  := cmd.ParamS('&folder', 'the #foldername where uploads are stored',
    Executable.ProgramFilePath + 'uploads');
  spool := cmd.ParamS('&spool', 'the #foldername for temporary body files',
    GetSystemPath(spTemp));
  // a bounded value on purpose: MaximumAllowedContentLength <= 0 means "no
  // limit" to mORMot, so '--maxsize 0' would silently remove the cap
  if not cmd.Get('&maxsize', 1, 65536, maxmb, 'the maximum body #size in MB',
       {default=}64) then
    maxmb := 64;
  silent := cmd.Option('silent', 'no output to the console');
  if cmd.ConsoleHelpFailed(
    'mORMot 2.' + SYNOPSE_FRAMEWORK_BRANCH + ' Streamed Upload Server') then
  begin
    ExitCode := 1;
    exit;
  end;
  server := TUploadServer.Create(port, dest, spool, maxmb);
  try
    if not silent then
    begin
      // this sample has no authentication whatsoever, and the server listens on
      // every network interface, not just the loopback one
      ConsoleWrite('Listening on port % (ALL interfaces, no authentication)',
        [port], ccLightRed);
      ConsoleWrite('Try http://localhost:%/ - uploads go to %',
        [port, dest], ccLightGreen);
      ConsoleWrite('Press [Enter] to quit', ccLightGray);
    end;
    // wait even when silent: this is what keeps the server running
    ConsoleWaitForEnterKey;
  finally
    server.Free;
  end;
end;

end.
