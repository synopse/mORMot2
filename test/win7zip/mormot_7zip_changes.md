# Changes to `mormot.lib.win7zip.pas`

This document describes the modifications made to mORMot's 7-Zip wrapper unit
(`mormot.lib.win7zip.pas`) for the Compare project, why they were needed, and
what would go wrong without them.

**File modified:** `C:\....\mORMot2\src\lib\mormot.lib.win7zip.pas`
**Original backup:** `mormot.lib.win7zip-bak.pas` (in the same folder — the pristine
file before any change)

There are two independent changes. Both are backward-compatible: existing code
that used this unit keeps working unchanged.

---

## Change 1 — Let `NewWriter` open a password-protected archive for update

### The problem

When you sync *into* an existing archive, mORMot has to first read the archive's
current contents so it can add or replace files without losing the rest. It does
this by opening a "writer" that is seeded from the existing file:

```pascal
function T7zLib.NewWriter(const name: TFileName; fmt: T7zFormatHandler): I7zWriter;
begin
  result := T7zWriter.Create(self, NewReader(name, fmt), {libowned=}false);
end;
```

Notice `NewReader(name, fmt)` is called with **no password**. That is fine for a
normal archive, but it breaks for a 7z archive whose *header* (the list of files)
is encrypted — created with 7-Zip's `-mhe=on` option. Such an archive cannot be
opened at all, not even to read its file list, without the password. So the
seed-reader fails, and the whole update fails.

`NewReader` already had a password-capable overload
(`NewReader(name, fmt, pw)`), but `NewWriter` had no way to pass a password
through to it, and `T7zWriter` lives in the unit's `implementation` section, so
a caller outside the unit cannot build the writer themselves.

### What would go wrong

Take a 7z archive with an encrypted header and try to sync a new file into it.
Every attempt to *write* to the archive fails, because mORMot cannot open the
existing archive to seed the update.

### The fix

`NewWriter` gained an optional password parameter, which it forwards to
`NewReader` — mirroring the existing `NewReader(name, fmt, pw)` overload exactly:

```pascal
function T7zLib.NewWriter(const name: TFileName; fmt: T7zFormatHandler;
  const pw: RawUtf8): I7zWriter;
begin
  // pw is needed for the update reader below to open an archive with encrypted
  // headers (7z -mhe=on), whose entry list is unreadable without it
  result := T7zWriter.Create(self, NewReader(name, fmt, pw), {libowned=}false);
end;
```

The parameter defaults to `''`, so every existing caller compiles and behaves as
before. The declaration was updated in both the `I7zLib` interface and the
`T7zLib` class.

---

## Change 2 — Stop silently ignoring extraction failures

### The problem

When 7z.dll extracts each file, it reports the outcome back to mORMot through a
callback, `SetOperationResult`. The possible outcomes include success, "wrong
password", "CRC error" (the extracted data does not match the stored checksum),
"data error", and others.

The original code only did something for the **success** case and threw every
other outcome away:

```pascal
function T7zReader.SetOperationResult(
  opResult: T7zExtractOperationResult): HRESULT;
begin
  case opResult of
    eorOK:
      with fExtractCurrent do
        if (FileName <> '') and
           ((Written or Created or Accessed) <> 0) then
          FileSetTime(FileName, Created, Accessed, Written);
  end;
  fExtractCurrent.FileName := '';
  result := S_OK;
end;
```

A `case` with only an `eorOK` branch means "wrong password", "CRC error" and
every other failure were **ignored**. mORMot returned success to the caller, and
the caller had no way to know the extraction had actually failed.

This is worse than just a password issue. It means **any** corrupt or
undecryptable entry was written to disk as garbage, silently, with no error
reported.

### What would go wrong

Two concrete examples:

1. **Wrong password, no error.** Extract a password-protected `.zip` (classic
   ZipCrypto encryption) using the *wrong* password. 7z.dll decrypts to garbage,
   notices the CRC does not match, and reports `eorCRCError`. The old code
   ignored it, so mORMot happily wrote a garbage file and reported success. The
   application could not tell the password was wrong — the sync would appear to
   succeed while producing corrupt output.

   (Note: password-protected *7z* archives use AES, which fails earlier when the
   archive is opened, so those *were* detected. It was specifically the ZipCrypto
   case — a `.zip` handled by mORMot — that slipped through.)

2. **Corrupt archive, no error.** Extract a file from an archive whose data is
   damaged. The CRC check fails, 7z.dll reports `eorCRCError`, the old code
   ignored it, and a corrupt file landed on disk with no warning.

### Why the fix is not simply "raise an exception here"

`SetOperationResult` is a callback invoked by 7z.dll from inside its own C++
code. Raising a Delphi exception at that point would try to unwind the stack
*through* the 7z.dll native frames, which is undefined behaviour and can crash
the process. So the failure must be **recorded** inside the callback and turned
into an exception later, on the Delphi side, after control has safely returned
from the DLL.

### The fix

The callback now records the first failure it sees (and still returns `S_OK`, as
7z.dll expects):

```pascal
function T7zReader.SetOperationResult(
  opResult: T7zExtractOperationResult): HRESULT;
begin
  case opResult of
    eorOK:
      with fExtractCurrent do
        if (FileName <> '') and
           ((Written or Created or Accessed) <> 0) then
          FileSetTime(FileName, Created, Accessed, Written);
  else
    // record the first failure (e.g. eorWrongPassword / eorCRCError); do NOT
    // raise here - this runs inside a 7z.dll stdcall callback. The Delphi-side
    // Extract*/ RaiseIfExtractFailed turns it into an exception once the DLL
    // call returns. Without this, a wrong password or corrupt entry was silently
    // ignored and produced a garbage output file.
    if fExtractOpResult = eorOK then
      fExtractOpResult := opResult;
  end;
  fExtractCurrent.FileName := '';
  result := S_OK;
end;
```

A new private field `fExtractOpResult` holds the recorded result, and a new
helper raises if a failure was recorded:

```pascal
procedure T7zReader.RaiseIfExtractFailed(const Context: ShortString);
begin
  if fExtractOpResult <> eorOK then
    E7Zip.RaiseUtf8('%.%: extract failed (operation result %)',
      [self, Context, ord(fExtractOpResult)]);
end;
```

Every extract entry point resets the recorded result before the DLL call and
checks it afterwards, once control is safely back on the Delphi side. For example,
`ExtractAll(path)`:

```pascal
  fExtractOpResult := eorOK;
  try
    E7Zip.CheckOk(self, 'ExtractAll', fInArchive.Extract(
      nil, $ffffffff, 0, self as IArchiveExtractCallback));
    RaiseIfExtractFailed('ExtractAll');
  finally
    fExtractPath := '';
  end;
```

All the other extract overloads were updated the same way, so the check is not
tied to one code path:

- `Extract(item, path, nosubfolder)` — to a local folder;
- `Extract(item, Stream)` — to a stream, which also backs
  `Extract(zipname): RawByteString` and `Extract(zipname, Stream): boolean`
  (without this, a wrong ZipCrypto password still returned garbage *bytes*, even
  though the to-disk path was already guarded);
- `Extract(items, callback)` and `ExtractAll(callback)` — the callback-driven
  overloads.

A genuine failure now raises `E7Zip` instead of being swallowed, on whichever
overload the caller uses. (The `boolean`/`RawByteString` helpers still swallow
that exception by design, so they now return `False` / `''` respectively instead
of reporting success with a garbage payload.)

### Effect on other callers

This changes global behaviour: extraction now **raises** on a bad-CRC or
wrong-password entry where it previously returned silently. That is the correct
behaviour — it stops corrupt files being written unnoticed — but any other
project sharing this mORMot copy that relied on the old silent behaviour will now
see an exception on genuinely corrupt archives instead of quietly receiving
garbage files.

---

## Summary

| Change | Problem | Fix |
|--------|---------|-----|
| `NewWriter` password | Could not update a 7z archive with an encrypted header (`-mhe`); writes failed. | Added an optional password, forwarded to the seed reader. |
| `SetOperationResult` | Extraction failures (wrong password, bad CRC) were silently ignored; garbage files written with no error. | Record the failure in the callback, raise on the Delphi side after the DLL call returns. |

Both changes are backward-compatible with existing callers. To revert, replace
the unit with `mormot.lib.win7zip-bak.pas`.
