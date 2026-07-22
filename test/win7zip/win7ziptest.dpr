/// standalone console tests for the two mormot.lib.win7zip.pas changes
// - Change 1: T7zLib.NewWriter() gained an optional password, forwarded to the
//   seed reader, so an existing 7z archive with an ENCRYPTED HEADER (7z -mhe=on)
//   can be opened for update - previously the update failed because the seed
//   reader was created without a password and could not even list the archive.
// - Change 2: T7zReader.SetOperationResult() now records the first non-eorOK
//   result (e.g. eorCRCError from a wrong ZipCrypto password or a corrupt entry)
//   and the Delphi-side RaiseIfExtractFailed() turns it into an E7Zip once the
//   7z.dll call has safely returned - previously any such failure was silently
//   swallowed and a garbage file was written while success was reported.
//
// This program deliberately uses ONLY the public mormot.lib.win7zip interface
// (I7zLib / I7zReader / I7zWriter) so it exercises exactly the code that was
// changed. It needs 7z.dll reachable (exe folder or C:\Program Files\7-Zip).
//
// This program is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
program win7ziptest;

{$I ..\..\src\mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$apptype console}
{$endif OSWINDOWS}

uses
  {$I ..\..\src\mormot.uses.inc}
  classes,
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.lib.win7zip;

const
  ZIP_PW  = 'correct-horse';           // password for the ZipCrypto .zip
  ZIP_BAD = 'battery-staple';          // a deliberately WRONG password
  HDR_PW  = 'header-secret';           // password for the -mhe=on .7z

var
  fLib:      I7zLib;
  fWorkDir:  TFileName;
  fChecks:   integer;
  fFailures: integer;

// ------------------------------------------------------------------ helpers

procedure Check(condition: boolean; const context: RawUtf8);
begin
  inc(fChecks);
  if condition then
    writeln('  [ok]   ', context)
  else
  begin
    inc(fFailures);
    writeln('  [FAIL] ', context);
  end;
end;

function WorkFile(const name: TFileName): TFileName;
begin
  result := fWorkDir + name;
  DeleteFile(result); // start each sub-test from a clean slate
end;

// ------------------------------------------------------- Change 2 (extract)

// Build a classic .zip whose single entry is ZipCrypto-encrypted with ZIP_PW,
// then prove that (a) the right password round-trips, and (b) a WRONG password
// is now reported as a failure instead of silently yielding a garbage file.
procedure Test_ExtractFailureIsReported;
var
  zip:     TFileName;
  writer:  I7zWriter;
  reader:  I7zReader;
  payload, got: RawByteString;
  stream:  TMemoryStream;
  raised:  boolean;
begin
  writeln('Change 2 - extraction failures are no longer swallowed');
  zip := WorkFile('secret.zip');
  payload := 'The quick brown fox jumps over the lazy dog.'#13#10 +
             'Line two, so the CRC has something to disagree with.';

  // create a ZipCrypto-protected .zip via the public writer API
  writer := fLib.NewWriter(fhZip);
  writer.SetPassword(ZIP_PW);
  writer.SetEncryptionMethod(emZipCrypto);
  writer.AddBuffer('secret.txt', payload);
  writer.SaveToFile(zip);
  Check(FileExists(zip), 'ZipCrypto archive was created');

  // (a) correct password -> content round-trips exactly
  reader := fLib.NewReader(zip, fhZip, ZIP_PW);
  Check(reader.Count = 1, 'archive lists exactly one entry');
  got := reader.Extract('secret.txt');
  Check(got = payload, 'correct password extracts the original bytes');
  reader := nil;

  // (b) wrong password via the boolean-returning helper: before the fix this
  // returned True with a garbage file on disk; now the recorded eorCRCError
  // makes the item extract raise, so the helper returns False.
  reader := fLib.NewReader(zip, fhZip, ZIP_BAD);
  Check(reader.Extract('secret.txt', fWorkDir, {nosubfolder=}true) = false,
    'wrong password no longer reports success');
  reader := nil;

  // (c) same wrong password via the raising item overload: must raise E7Zip
  //     (this is the exact code path RaiseIfExtractFailed guards)
  reader := fLib.NewReader(zip, fhZip, ZIP_BAD);
  raised := false;
  try
    reader.Extract({item=}0, fWorkDir, {nosubfolder=}true);
  except
    on E7Zip do
      raised := true;
  end;
  Check(raised, 'wrong password raises E7Zip instead of writing garbage');
  reader := nil;

  // (d) wrong password via the STREAM/RawByteString path: this overload used to
  // be uncovered, so it silently returned garbage bytes. It now raises inside
  // Extract(item, Stream); the RawByteString helper swallows that and returns ''.
  reader := fLib.NewReader(zip, fhZip, ZIP_BAD);
  got := reader.Extract('secret.txt'); // -> RawByteString
  Check(got = '', 'wrong password yields no bytes (not garbage) to RawByteString');
  reader := nil;

  // (e) same wrong password via the raising item-to-stream overload: must raise
  reader := fLib.NewReader(zip, fhZip, ZIP_BAD);
  stream := TMemoryStream.Create;
  try
    raised := false;
    try
      reader.Extract({item=}0, stream);
    except
      on E7Zip do
        raised := true;
    end;
    Check(raised, 'wrong password raises E7Zip on stream extraction too');
  finally
    stream.Free;
  end;
  reader := nil;
  writeln;
end;

// --------------------------------------------------- Change 1 (NewWriter pw)

// Build a .7z with an ENCRYPTED HEADER (-mhe=on), then update it. The update
// can only seed itself from the existing archive if NewWriter is given the
// password - which is exactly what Change 1 added.
procedure Test_UpdateEncryptedHeaderArchive;
var
  src:     TFileName;
  writer:  I7zWriter;
  reader:  I7zReader;
  first, second: RawByteString;
  raised:  boolean;
begin
  writeln('Change 1 - NewWriter(pw) can update an encrypted-header .7z');
  src := WorkFile('mhe.7z');
  first  := 'FIRST entry - present before the update.';
  second := 'SECOND entry - added by the password-seeded update.';

  // create a 7z whose *header* (the file list) is encrypted
  writer := fLib.NewWriter(fh7z);
  writer.SetPassword(HDR_PW);
  writer.EncryptHeaders7z(true);        // -mhe=on : the entry list itself is AES
  writer.AddBuffer('first.txt', first);
  writer.SaveToFile(src);
  writer := nil;
  Check(FileExists(src), 'encrypted-header .7z was created');

  // the header really is encrypted: opening without a password must fail,
  // it cannot even be listed
  raised := false;
  try
    reader := fLib.NewReader(src, fh7z);  // no password
    reader.Count;                         // force the open/list to happen
  except
    on E7Zip do
      raised := true;
  end;
  reader := nil;
  Check(raised, 'header is truly encrypted (no-password open fails)');

  // negative control: updating WITHOUT the password fails, because the seed
  // reader inside NewWriter cannot open the encrypted header
  raised := false;
  try
    writer := fLib.NewWriter(src, fh7z);  // old-style: no password
    writer.AddBuffer('second.txt', second);
    writer.SaveToFile(WorkFile('mhe-nopw.7z'));
  except
    on E7Zip do
      raised := true;
  end;
  writer := nil;
  Check(raised, 'update without password fails (the bug Change 1 fixes)');

  // the fix: pass the password to NewWriter so the seed reader can open the
  // existing archive, then add a second entry and save an updated copy
  raised := false;
  try
    writer := fLib.NewWriter(src, fh7z, HDR_PW);
    writer.SetPassword(HDR_PW);          // keep the OUTPUT header encrypted too
    writer.EncryptHeaders7z(true);
    writer.AddBuffer('second.txt', second);
    writer.SaveToFile(src);              // update in place
  except
    on E: E7Zip do
    begin
      raised := true;
      writeln('  (unexpected) ', E.Message);
    end;
  end;
  writer := nil;
  Check(not raised, 'update WITH password succeeds');

  // verify the updated archive: both entries present, contents intact
  reader := fLib.NewReader(src, fh7z, HDR_PW);
  Check(reader.Count = 2, 'updated archive holds both entries');
  Check(reader.Extract('first.txt') = first,
    'the original entry survived the update');
  Check(reader.Extract('second.txt') = second,
    'the newly added entry is correct');
  reader := nil;
  writeln;
end;

// ------------------------------------------------------------------- main

begin
  fWorkDir := Executable.ProgramFilePath + 'win7ziptest_work\';
  ForceDirectories(fWorkDir);
  writeln('mormot.lib.win7zip change tests');
  writeln('work folder: ', fWorkDir);
  try
    fLib := T7zLib.Create; // auto-locate 7z.dll
  except
    on E: Exception do
    begin
      writeln('cannot load 7z.dll: ', E.Message);
      writeln('place 7z.dll next to this exe or install 7-Zip, then retry.');
      ExitCode := 2;
      Halt;
    end;
  end;
  writeln('using 7z.dll: ', fLib.FileName);
  writeln;
  try
    Test_ExtractFailureIsReported;
    Test_UpdateEncryptedHeaderArchive;
  except
    on E: Exception do
    begin
      inc(fFailures);
      writeln('  [FAIL] unhandled ', E.ClassName, ': ', E.Message);
    end;
  end;
  fLib := nil;
  writeln('-----------------------------------------------------------');
  writeln(Format('%d checks, %d failure(s)', [fChecks, fFailures]));
  if fFailures = 0 then
    writeln('ALL PASSED')
  else
    ExitCode := 1;
end.
