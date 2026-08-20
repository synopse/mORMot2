# Streamed File Upload Server

A stand-alone HTTP server which receives `multipart/form-data` uploads without
ever holding the request body in memory - the answer to
[issue #292](https://github.com/synopse/mORMot2/issues/292).

## Running it

Build `httpServerUpload.dpr` (or open `httpServerUpload.lpi` in Lazarus), then:

    ./httpServerUpload --port 8888 --folder ./uploads --maxsize 64

Open `http://localhost:8888/` in a browser for a small upload form, or use any
HTTP client:

    curl -F 'file=@somefile.bin' -F 'comment=hello' http://localhost:8888/upload

Options: `--port`, `--folder` (where uploads are stored), `--spool` (where the
temporary body files go, defaults to the system temporary folder), `--maxsize`
(the maximum accepted body size in MB), `--silent`, `--help`.

## How it works

Two halves, each doing one job:

1. **`THttpServerGeneric.OnBodyDownload`** is asked, once the headers are parsed
   and a body is known to follow, where that body should go. Returning a
   `TFileStreamEventuallyDelete` makes the server write the incoming bytes
   straight into a temporary file instead of the in-memory `Content` buffer. The
   stream is handed to the request as `InContentStream`, then released - and the
   spool file deleted with it - once the request has been processed.

   The event returns `nil` for every other URI, so the rest of the server keeps
   the default in-memory path, which is the right one for small JSON bodies.

2. **`THttpMultiPartDecoder`** walks that stream section by section. Each file
   section exposes its content as an incremental `TStream`, so a section is
   copied to its destination without being assembled in memory first.

Each file section is written to a uniquely named staging file inside a
`.staging` sub-folder of the destination, and renamed to its final name only
once `Close()` confirmed the final `--boundary--` delimiter was received. A
connection cut in the middle of an upload otherwise leaves truncated files that
look exactly like complete ones - and a staging file that outlives a crash stays
out of the folder being served.

## What it costs

Measured on Linux/FPC with a single 400 MB upload, reading `VmHWM` (peak
resident memory) of the server process:

| body path | peak memory |
| --- | --- |
| `POST /upload` - `OnBodyDownload` returns a spool stream | 89 MB |
| `POST /other` - `OnBodyDownload` returns `nil`, i.e. the default | 406 MB |

The in-memory path grows with the body: the payload *is* the footprint. With a
spool stream it does not.

`MaximumAllowedContentLength` is still what bounds a single request: above it,
the server answers `413` while the body is being received. It applies to a
`Content-Length` body and - because this sample supplies a stream - to a chunked
one as well, on its cumulated size.

## Things worth knowing

- **This sample has no authentication, and listens on every interface.** It
  writes whatever it is sent into a folder. Do not run it on a reachable
  network without putting something in front of it.
- **The file name of a section is raw client input.** This sample normalizes
  the delimiters, strips any path, then rejects what is left if it is empty,
  `.`, `..`, ends with `.` or a space (Win32 drops those silently, so the stored
  name would differ from the validated one), or resolves to a DOS device name
  such as `CON` or `NUL.txt`. The device check looks at the part before the
  *first* dot, which is how Win32 resolves a device: checking the name without
  its last extension would let `con.txt.txt` through. The rejections apply on
  every platform, so the sample behaves the same everywhere.
- **Staging files live in `<destination>/.staging`.** They must share the file
  system with the destination, otherwise the final rename fails outright (POSIX
  `EXDEV`; `MoveFileW` without `MOVEFILE_COPY_ALLOWED`), which rules out putting
  them next to the spool files. The folder is swept at startup, since a process
  killed mid-upload leaves its staging file behind.
- **Replacing an existing file is last-wins, and not atomic on Windows.** The
  rename is tried first - on POSIX that already replaces the target atomically.
  Only if it fails is the target deleted and the rename retried, which is what
  Windows needs, since `RenameFile()` is `MoveFileW()` without
  `MOVEFILE_REPLACE_EXISTING`. Doing the delete upfront would be simpler but
  would destroy the previous file even when the new one cannot be put in place.
- **A failing rename gives `500`, but the result can still be partial**: files
  published by earlier iterations of the loop stay. There is no atomic way to
  publish a whole set; a real service would rather version names than replace.
- The spool file is still open while the handler runs. On Windows it therefore
  needs a sharing mode to be read back by name (`fmCreate or fmShareRead`, as
  used here), and it cannot be renamed or moved from the handler, since
  `FileShare()` never includes `FILE_SHARE_DELETE`. Its default location is the
  system temporary folder, which on POSIX is world-readable: point `--spool`
  somewhere private if the uploads are sensitive.
- `THttpMultiPartDecoder.CreateFromContentType()` only looks for a `boundary`
  parameter, so the media type is checked separately - otherwise `text/plain;
  boundary=x` would be decoded as multipart and stored.
- The number of sections is capped (`MAX_PARTS`): each file section costs one
  staging file, so a body made of many tiny parts would otherwise be a cheap way
  to burn inodes and CPU.
- `MaximumAllowedContentLength` of `0` or less means *no limit* to mORMot, which
  is why `--maxsize` is read with a bounded overload: a typo would otherwise
  remove the very protection the option advertises.
- A body with a `Content-Encoding` matching a registered compression algorithm
  cannot be streamed, and is rejected as `415`. The deprecated HTTP/1.0
  close-delimited body is not streamed either.
- **The handler still occupies one server thread for the whole decode.**
  Spooling the body keeps it out of memory, not out of the thread pool:
  `StreamCopyUntilEnd` copies each section synchronously on one of the (four,
  here) `THttpAsyncServer` threads. For multi-gigabyte uploads, hand the work to
  dedicated workers, or use a server model made for blocking handlers. A
  `TPipeStream` to a consumer thread would avoid the intermediate file
  altogether, but its `Write()` blocks until the consumer drains it, holding an
  event loop thread for the whole upload rather than just the decode.
