/// JSON Oriented Preprocessor Reusable Processing Functions
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit jop.main;

{
  *****************************************************************************

   JSON, YAML, CSV, Markdown and Text Processing
    - Formats, Options, Errors and Statistics
    - In-memory and Writer Processing Functions
    - File and Batch Processing
    - Random Documents and Compact JSON Comparison

  *****************************************************************************
}

interface

{$I ..\..\mormot.defines.inc}

uses
  classes,
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.json,
  mormot.core.fmt;


{ ************ Formats, Options, Errors and Statistics }

type
  /// supported input and output formats
  // - jfAuto uses file extensions, or jfJson for in-memory input
  // - jfAuto output is jfText for text input, otherwise jfJson
  // - JSON input uses the native relaxed parser; jfHjson/jfMorml also allow
  // an implicit root object - these names do not imply RFC validation
  // - jfMd and jfMdPipe are output only: native Markdown with fenced CSV
  // sections or pipe tables; the .md extension selects jfMd
  TJopFormat = (
    jfAuto,
    jfJson,
    jfJsonC,
    jfJson5,
    jfHjson,
    jfMorml,
    jfYaml,
    jfCsv,
    jfText,
    jfMd,
    jfMdPipe);

  /// optional processing features, with no validation enabled by default
  // - input is opaque 8-bit text: jofValidateUtf8 explicitly checks its UTF-8
  // encoding, including active defines and includes; BOM-less bytes stay as-is
  // - file and include loading uses the BOM support of RawUtf8FromFile()
  // - embedded #0 is rejected because the core parsers are zero-terminated
  // - jofValidateJson checks the expanded compact JSON with native IsValidJson;
  // this is a structural check, not full RFC 8259 validation
  // - jofNoExpand bypasses preprocessing, including defines and includes
  // - jofNoComments omits JSON comments, keeping the selected output layout
  // - jofNoNamespaces disables external namespaces, preserving local defines
  // - jofPreprocessInput explicitly expands YAML/CSV source text before parsing
  // - jofDebugComments requires preprocessing and a comment-capable output
  // - jofAllowMissingIncludes skips includes not found by FileExists(); a file
  // found but empty, BOM-only or unreadable is still an include error
  // - jofCompact selects compact jfJson output, instead of indented JSON
  // - jofRecursive and jofDryRun apply to file processing only
  TJopFlag = (
    jofValidateUtf8,
    jofNoExpand,
    jofNoComments,
    jofNoNamespaces,
    jofPreprocessInput,
    jofDebugComments,
    jofAbsolutePaths,
    jofAllowMissingIncludes,
    jofCompact,
    jofRecursive,
    jofDryRun,
    jofValidateJson);
  TJopFlags = set of TJopFlag;

  /// parameters shared by all processing functions
  // - initialize with JopOptions() or a zeroed/default record
  // - Defines are literal name=value pairs; the last wins, then source defines
  // may override them - their contents are never appended as DSL source
  // - CsvDelimiter applies to CSV input only: #0 means comma, #9 means tab
  // - CSV input has a unique nonempty header and equally wide rows, all strings
  // - CSV/Markdown output follows native relational table semantics, including
  // __rowid, flattened objects and linked child arrays with __parentid
  // - TableOptions is the core formatter's set; TableRoot='' means 'root'
  // and labels follow the native exporter's escaping rules
  // - CSV output always uses comma and LF, as VariantToCsv() does
  TJopOptions = record
    InputFormat: TJopFormat;
    OutputFormat: TJopFormat;
    Flags: TJopFlags;
    IncludeRoot: TFileName;
    Defines: TRawUtf8DynArray;
    CsvDelimiter: AnsiChar;
    TableOptions: TTableOptions;
    TableRoot: RawUtf8;
  end;

  /// result of a processing call, without a required diagnostic allocation
  // - jeSuccess also covers completed table output with native remarks
  // - the optional Info pointer receives remarks or contextual error details;
  // use JOP_ERROR[result] for the static description of an error
  TJopError = (
    jeSuccess,
    jeInvalidOptions,
    jeUnsupportedInput,
    jeEmbeddedNul,
    jeInvalidUtf8,
    jeInvalidJson,
    jeInvalidYaml,
    jeInvalidCsv,
    jePreprocess,
    jeTableData,
    jeTableDepth,
    jeMultipleTables,
    jeNoSource,
    jeRead,
    jeWrite,
    jeSameFile,
    jeInvalidPath,
    jeNoFiles,
    jeCollision,
    jeException);

  /// accumulating counters, initialized to zero before the first call
  // - FilesSucceeded/Failed count documents for in-memory calls
  // - InputBytes counts processed input bytes after any file BOM decoding;
  // IncludeBytes counts each successful include/insert load separately,
  // including repeated loads
  // - OutputBytes counts successful output only; failed writer/stream calls
  // may already have appended bytes which the caller must discard
  // - elapsed file time includes reading, writing and final replacement
  // - dry runs count skipped files without reading their contents
  TJopStatistics = record
    FilesSucceeded: Int64;
    FilesFailed: Int64;
    FilesSkipped: Int64;
    IncludeFiles: Int64;
    InsertFiles: Int64;
    InputBytes: Int64;
    IncludeBytes: Int64;
    OutputBytes: Int64;
    ElapsedMicroSeconds: Int64;
  end;
  PJopStatistics = ^TJopStatistics;

  /// raised only by the convenient string-returning JopProcess() overload
  EJopException = class(ESynException);

const
  JOP_JSON_FORMATS = [jfJson, jfJsonC, jfJson5, jfHjson, jfMorml];
  JOP_INPUT_FORMATS = [jfJson, jfJsonC, jfJson5, jfHjson, jfMorml,
    jfYaml, jfCsv, jfText];
  JOP_OUTPUT_FORMATS = JOP_INPUT_FORMATS + [jfMd, jfMdPipe];
  JOP_FORMAT_NAMES: array[TJopFormat] of RawUtf8 = (
    'auto', 'json', 'jsonc', 'json5', 'hjson', 'morml', 'yaml', 'csv',
    'text', 'md', 'md-pipe');
  JOP_FORMAT_EXTENSIONS: array[TJopFormat] of TFileName = (
    '', '.json', '.jsonc', '.json5', '.hjson', '.morml', '.yaml', '.csv',
    '.txt', '.md', '.md');
  JOP_ERROR: array[TJopError] of RawUtf8 = (
    '',
    'Invalid processing options',
    'Unsupported input format',
    'Embedded NUL in input',
    'Invalid UTF-8 input',
    'Invalid JSON',
    'Invalid YAML',
    'Invalid CSV',
    'Preprocessing failed',
    'Incompatible relational table data',
    'Relational table nesting is too deep',
    'CSV has several tables: use JopProcessToCsv',
    'Input file does not exist',
    'Could not read input file',
    'Could not write output',
    'Source and destination must differ',
    'Invalid input or output path',
    'No matching input files',
    'Input/output collision or duplicate destination',
    'Unexpected processing exception');

/// initialize processing parameters, without allocating default strings
function JopOptions(InputFormat: TJopFormat = jfAuto;
  OutputFormat: TJopFormat = jfAuto; Flags: TJopFlags = []): TJopOptions;

/// resolve a case-insensitive format name, including yml/txt/jop/md-csv
function JopFormatFromName(const Name: RawUtf8; out Format: TJopFormat): boolean;

/// recognize format extensions, including .ini/.cnf/.jop as plain text
// - recognizing .md/.markdown does not make Markdown an input format
function JopFormatFromFileName(const FileName: TFileName;
  out Format: TJopFormat): boolean;

/// compute decimal MB/s from main plus included bytes and elapsed time
function JopThroughput(const Statistics: TJopStatistics): double;


{ ************ In-memory and Writer Processing Functions }

/// process a document, clearing Dest on failure
// - Source is a private writable buffer, consumed by this call: conversion may
// replace it, then decode JSON objects/arrays in-place through InitJsonInPlace()
// - no hidden copy is made: load Source from a file or create a private buffer;
// assigning a shared string or literal to a variable does not make it writable
// - Source and Dest must be distinct variables
// - Info is optional and cleared on entry; it may contain native table remarks
// on success, or parser/exception details on failure
function JopProcess(var Source: RawUtf8; const Options: TJopOptions;
  out Dest: RawUtf8; Statistics: PJopStatistics = nil;
  Info: PRawUtf8 = nil): TJopError; overload;

/// convenient string result, raising EJopException on a processing error
// - Source has the same writable, consumed-buffer contract as the other overload
function JopProcess(var Source: RawUtf8; InputFormat: TJopFormat = jfJson;
  OutputFormat: TJopFormat = jfJson; Flags: TJopFlags = []): RawUtf8; overload;

/// append to an existing JSON-capable text writer, without flushing or owning it
// - default JSON-to-JSON processing writes directly to W in one pass
// - Source has the same writable, consumed-buffer contract as JopProcess()
// - dialect conversion, expansion and explicit validation may replace Source;
// ordinary JSON is decoded in-place for YAML/CSV/Markdown without a buffer copy
// - failed calls may have appended partial bytes; discard that transaction
// - jfCsv requires exactly one table; use JopProcessToCsv() for nested arrays
function JopProcessToWriter(var Source: RawUtf8; W: TJsonWriter;
  const Options: TJopOptions; Statistics: PJopStatistics = nil;
  Info: PRawUtf8 = nil): TJopError;

/// append at the stream's current position and flush completed output
// - Source is a private writable buffer, as for JopProcess()
// - does not own, close, truncate or reposition the caller's stream
function JopProcessToStream(var Source: RawUtf8; Dest: TStream;
  const Options: TJopOptions; Statistics: PJopStatistics = nil;
  Info: PRawUtf8 = nil): TJopError;

/// process into all named relational CSV tables, without creating files
// - Source is a private writable buffer, as for JopProcess()
// - returns native TTextPairs, preserving every parent and child table
// - Options.OutputFormat is ignored; each Value uses comma separators and LF
// - Tables is nil on failure; OutputBytes sums the completed CSV values
function JopProcessToCsv(var Source: RawUtf8; const Options: TJopOptions;
  out Tables: TTextPairs; Statistics: PJopStatistics = nil;
  Info: PRawUtf8 = nil): TJopError;


{ ************ File and Batch Processing }

type
  /// outcome of one planned file conversion
  TJopFileStatus = (
    jfsPending,
    jfsSucceeded,
    jfsFailed,
    jfsSkipped);
  TJopFileResult = record
    Source: TFileName;
    Dest: TFileName;
    Status: TJopFileStatus;
    Error: TJopError;
    Info: RawUtf8;
  end;
  TJopFileResults = array of TJopFileResult;

/// process one file, replacing Dest from a temporary sibling after success
// - RawUtf8FromFile handles BOMs; BOMless bytes are left unchanged
// - empty/BOM-only input or a read failure returns jeRead, leaving Dest intact
// - Source and Dest must differ; existing output survives processing failure
// - explicit formats override extensions; dry runs check paths and options
// without reading input contents or creating directories
function JopProcessFile(const Source, Dest: TFileName;
  const Options: TJopOptions; Statistics: PJopStatistics = nil;
  Info: PRawUtf8 = nil): TJopError;

/// plan and process a file mask or directory, retaining relative subdirectories
// - only the filename may contain wildcards; OutputFolder is required
// - all destination collisions are rejected before the first output is made
// - a recursive input enumeration excludes its output subtree
// - directory input skips unknown/output-only formats; an explicit mask reports
// them as per-file errors; processing continues after individual file errors
// - returns the first error, with the full plan and results in Files
function JopProcessFiles(const InputMask, OutputFolder: TFileName;
  const Options: TJopOptions; out Files: TJopFileResults;
  Statistics: PJopStatistics = nil; Info: PRawUtf8 = nil): TJopError;


{ ************ Random Documents and Compact JSON Comparison }

/// append a reproducible random JSON document without allocating per value
// - Depth is the maximum number of nested containers, in the range 0..64:
// 0 writes one scalar; otherwise the root is an array containing Items values
// - each nested container has one child container and up to two scalar siblings,
// so output size is proportional to Items * Depth, not exponential
// - Seed initializes a local TLecuyer, without changing the process random state
// - W is neither owned nor flushed; discard partial output on failure
function JopGenerate(W: TJsonWriter; Depth, Items: integer;
  Seed: cardinal): TJopError;

/// compare two files after native JsonReformat(jsonCompact) normalization
// - uses the syntax understood by JsonReformat(); file extensions are ignored
// and no format inference, preprocessing or validation is performed
// - equality is compact-text equality: comments and formatting are ignored,
// but property order remains significant; there is no semantic comparison pass
// - only normalized documents are retained together; no variant tree is built
// - Equal is false on I/O failure, with the affected filename in optional Info
function JopCompareFiles(const Left, Right: TFileName; out Equal: boolean;
  Info: PRawUtf8 = nil): TJopError;


implementation

uses
  mormot.core.variants,
  mormot.core.data,
  mormot.core.search;


{ ************ Formats, Options, Errors and Statistics }

function JopOptions(InputFormat, OutputFormat: TJopFormat;
  Flags: TJopFlags): TJopOptions;
begin
  result.InputFormat := InputFormat;
  result.OutputFormat := OutputFormat;
  result.Flags := Flags;
  result.IncludeRoot := '';
  result.Defines := nil;
  result.CsvDelimiter := #0;
  result.TableOptions := TABLE_DEFAULT;
  result.TableRoot := '';
end;

function JopFormatFromName(const Name: RawUtf8; out Format: TJopFormat): boolean;
var
  f: TJopFormat;
  p: PUtf8Char;
  n: PtrInt;
begin
  Format := jfAuto;
  p := pointer(Name);
  n := length(Name);
  result := true;
  for f := low(f) to high(f) do
    if IdemPropNameU(JOP_FORMAT_NAMES[f], p, n) then
    begin
      Format := f;
      exit;
    end;
  if IdemPropName('yml', p, n) then
    Format := jfYaml
  else if IdemPropName('txt', p, n) or
          IdemPropName('jop', p, n) then
    Format := jfText
  else if IdemPropName('markdown', p, n) or
          IdemPropName('md-csv', p, n) then
    Format := jfMd
  else if IdemPropName('markdown-pipe', p, n) then
    Format := jfMdPipe
  else
    result := false;
end;

function JopFormatFromFileName(const FileName: TFileName;
  out Format: TJopFormat): boolean;
begin
  Format := jfAuto;
  case SameExt(FileName, ['json', 'jsonc', 'json5', 'hjson', 'morml',
        'yaml', 'yml', 'csv', 'txt', 'jop', 'ini', 'cnf', 'md', 'markdown'], true) of
    0: Format := jfJson;
    1: Format := jfJsonC;
    2: Format := jfJson5;
    3: Format := jfHjson;
    4: Format := jfMorml;
    5, 6: Format := jfYaml;
    7: Format := jfCsv;
    8 .. 11: Format := jfText;
    12, 13: Format := jfMd;
  end;
  result := Format <> jfAuto;
end;

function JopThroughput(const Statistics: TJopStatistics): double;
begin
  result := 0;
  if Statistics.ElapsedMicroSeconds > 0 then
    result := (Statistics.InputBytes + Statistics.IncludeBytes) /
              Statistics.ElapsedMicroSeconds; // decimal MB per second
end;

function JopCheckOptions(var Options: TJopOptions): TJopError;
begin
  if Options.InputFormat = jfAuto then
    Options.InputFormat := jfJson;
  if Options.OutputFormat = jfAuto then
    if Options.InputFormat = jfText then
      Options.OutputFormat := jfText
    else
      Options.OutputFormat := jfJson;
  result := jeUnsupportedInput;
  if not (Options.InputFormat in JOP_INPUT_FORMATS) then
    exit;
  result := jeInvalidOptions;
  if (Options.InputFormat = jfText) <> (Options.OutputFormat = jfText) then
    exit;
  if jofDebugComments in Options.Flags then
    if not (Options.OutputFormat in [jfJsonC, jfJson5, jfHjson, jfText]) or
       (Options.Flags * [jofNoExpand, jofNoComments] <> []) or
       (Options.InputFormat in [jfYaml, jfCsv]) then
      exit;
  if Options.CsvDelimiter = #0 then
    Options.CsvDelimiter := ',';
  if Options.InputFormat = jfCsv then
    if (Options.CsvDelimiter <> #9) and
       ((Options.CsvDelimiter < ' ') or
        (Options.CsvDelimiter >= #127) or
        (Options.CsvDelimiter = '"')) then
      exit;
  if Options.TableRoot = '' then
    Options.TableRoot := 'root';
  result := jeSuccess;
end;

procedure JopAddStatistics(Dest: PJopStatistics; const Added: TJopStatistics);
begin
  if Dest = nil then
    exit;
  inc(Dest^.FilesSucceeded, Added.FilesSucceeded);
  inc(Dest^.FilesFailed, Added.FilesFailed);
  inc(Dest^.FilesSkipped, Added.FilesSkipped);
  inc(Dest^.IncludeFiles, Added.IncludeFiles);
  inc(Dest^.InsertFiles, Added.InsertFiles);
  inc(Dest^.InputBytes, Added.InputBytes);
  inc(Dest^.IncludeBytes, Added.IncludeBytes);
  inc(Dest^.OutputBytes, Added.OutputBytes);
  inc(Dest^.ElapsedMicroSeconds, Added.ElapsedMicroSeconds);
end;

function JopStart(Statistics: PJopStatistics; out Run: TJopStatistics): Int64;
begin
  FillCharFast(Run, SizeOf(Run), 0);
  result := 0;
  if Statistics <> nil then
    QueryPerformanceMicroSeconds(result);
end;

procedure JopFinish(var Run: TJopStatistics; Started: Int64; Error: TJopError;
  Statistics: PJopStatistics);
var
  stopped: Int64;
begin
  if Statistics = nil then
    exit;
  QueryPerformanceMicroSeconds(stopped);
  Run.ElapsedMicroSeconds := stopped - Started;
  if Error = jeSuccess then
    Run.FilesSucceeded := 1
  else
  begin
    Run.FilesFailed := 1;
    Run.OutputBytes := 0;
  end;
  JopAddStatistics(Statistics, Run);
end;

function JopException(E: Exception; Info: PRawUtf8): TJopError;
begin
  if Info <> nil then
    Info^ := StringToUtf8(E.Message);
  if E.InheritsFrom(EYamlException) then
    result := jeInvalidYaml
  else if E.InheritsFrom(EPreprocException) then
    result := jePreprocess
  else if E.InheritsFrom(EStreamError) then
    result := jeWrite
  else
    result := jeException;
end;


{ ************ In-memory and Writer Processing Functions }

type
  PJopCsv = ^TTextPairs;

const
  JOP_EMPTY: array[0..7] of AnsiChar = (#0, #0, #0, #0, #0, #0, #0, #0);
  JOP_TABLE_ERROR: array[TTableResult] of TJopError = (
    jeSuccess, jeSuccess, jeInvalidJson, jeTableData, jeTableDepth, jeWrite);

function JopJsonFormat(Format: TJopFormat; Flags: TJopFlags): TTextWriterJsonFormat;
begin
  case Format of
    jfJsonC: result := jsonC;
    jfJson5: result := json5;
    jfHjson: result := jsonH;
    jfMorml: result := jsonMorml;
  else
    if jofCompact in Flags then
      result := jsonCompact
    else
      result := jsonHumanReadable;
  end;
end;

function JopCheckBytes(P: PUtf8Char; Len: PtrInt; ValidateUtf8: boolean): TJopError;
begin
  result := jeEmbeddedNul;
  if ByteScanIndex(pointer(P), Len, 0) >= 0 then
    exit;
  result := jeInvalidUtf8;
  if ValidateUtf8 and
     (Len <> 0) and
     not IsValidUtf8Buffer(P, Len) then
    exit;
  result := jeSuccess;
end;

function JopCheckJson(P: PUtf8Char; Len: PtrInt; Comments: boolean): boolean;
var
  w: TJsonWriter;
  temp: TTextWriterStackBuffer;
  settings: TPreprocSettings;
begin
  if not Comments then
  begin
    result := IsValidJson(P, Len, {strict=}true);
    exit;
  end;
  // Only explicit validation of comment-preserving output needs this copy.
  // Keep the original comment buffer intact for final output.
  settings.Flags := [jppNoExpand];
  settings.IncludeRoot := '';
  settings.Defines := nil;
  settings.Statistics := nil;
  w := TJsonWriter.CreateOwnedStream(temp);
  try
    result := JsonPreprocessToWriter(P, w, jsonCompact, settings);
    if result then
    begin
      Len := w.TextLength;
      result := IsValidJson(w.GetTextAsBuffer, Len, {strict=}true);
    end;
  finally
    w.Free;
  end;
end;

function JopNeedsPrepare(P: PUtf8Char; Expand: boolean): boolean;
begin
  result := true;
  while P^ <> #0 do
  begin
    case P^ of
      '"':
        begin
          P := GotoEndOfJsonString(P);
          if P^ <> '"' then
            exit; // leave malformed strings to the native formatter
        end;
      '/', '''':
        exit;
      '$':
        if Expand then
          exit;
    end;
    inc(P);
  end;
  result := false;
end;

function JopConvert(var Source: RawUtf8; W: TJsonWriter;
  const Options: TJopOptions; var Run: TJopStatistics;
  Tables: PJopCsv; Info: PRawUtf8): TJopError;
var
  opt: TJopOptions;
  settings: TPreprocSettings;
  included: TPreprocStatistics;
  stage, target: TTextWriterJsonFormat;
  p: PUtf8Char;
  len, i: PtrInt;
  before: Int64;
  comments: boolean;
  csv: TTextPairs;
  doc: variant;
  field: TGetJsonField;
  docOptions: TDocVariantOptions;
  mdFormat: TTableTextFormat;
  text, detail: RawUtf8;

  function Prepare(Input: TJopFormat;
    Format: TTextWriterJsonFormat): TJopError;
  var
    writer: TJsonWriter;
    temp: TTextWriterStackBuffer;
    error: TCsvError;
  begin
    writer := TJsonWriter.CreateOwnedStream(temp);
    try
      case Input of
        jfText:
          begin
            result := jePreprocess;
            if not TextPreprocessToWriter(p, writer, settings) then
              exit;
          end;
        jfCsv:
          begin
            result := jeInvalidCsv;
            error := CsvToJson(p, writer, opt.CsvDelimiter);
            if error <> ceNone then
            begin
              if Info <> nil then
                Info^ := RawUtf8(CSV_ERROR[error]);
              exit;
            end;
          end;
      else
        result := jeInvalidJson;
        if not JsonPreprocessToWriter(p, writer, Format, settings) then
          exit;
      end;
      writer.SetText(Source);
      p := pointer(Source);
      len := length(Source);
      if p = nil then
        p := @JOP_EMPTY;
      result := jeSuccess;
    finally
      writer.Free;
    end;
  end;

begin
  Run.InputBytes := length(Source);
  if Info <> nil then
    Info^ := '';
  result := jeInvalidOptions;
  if (W = nil) and
     (Tables = nil) then
    exit;
  opt := Options;
  result := JopCheckOptions(opt);
  if result <> jeSuccess then
    exit;
  p := pointer(Source);
  len := length(Source);
  result := JopCheckBytes(p, len, jofValidateUtf8 in opt.Flags);
  if result <> jeSuccess then
    exit;
  if (len >= 3) and
     (p[0] = #$ef) and
     (p[1] = #$bb) and
     (p[2] = #$bf) then
  begin
    Delete(Source, 1, 3); // the supplied buffer is writable, including its BOM
    p := pointer(Source);
    len := length(Source);
  end;
  if p = nil then
    p := @JOP_EMPTY;
  settings.Flags := [jppStrictInclude];
  if jofNoExpand in opt.Flags then
    include(settings.Flags, jppNoExpand);
  if jofNoComments in opt.Flags then
    include(settings.Flags, jppNoComments);
  if (jofValidateUtf8 in opt.Flags) and
     not (jofNoExpand in opt.Flags) then
    include(settings.Flags, jppValidateUtf8);
  if jofNoNamespaces in opt.Flags then
    include(settings.Flags, jppNoNamespaces);
  if jofDebugComments in opt.Flags then
    include(settings.Flags, jppDebugComment);
  if jofAbsolutePaths in opt.Flags then
    include(settings.Flags, jppIncludeAbsolute);
  if jofAllowMissingIncludes in opt.Flags then
    include(settings.Flags, jppAllowMissingInclude);
  if opt.InputFormat in [jfHjson, jfMorml] then
    include(settings.Flags, jppJsonImplicitObject);
  settings.IncludeRoot := opt.IncludeRoot;
  settings.Defines := opt.Defines;
  FillCharFast(included, SizeOf(included), 0);
  settings.Statistics := @included;
  before := 0;
  if W <> nil then
    before := W.TextLength;
  result := jeInvalidJson;
  try
    if opt.InputFormat = jfText then
    begin
      if jofNoExpand in opt.Flags then
      begin
        W.AddNoJsonEscape(p, len);
        result := jeSuccess;
      end
      else if TextPreprocessToWriter(p, W, settings) then
        result := jeSuccess
      else
        result := jePreprocess;
      exit;
    end;
    target := JopJsonFormat(opt.OutputFormat, opt.Flags);
    if (opt.InputFormat in JOP_JSON_FORMATS) and
       (opt.OutputFormat in JOP_JSON_FORMATS) and
       not (jofValidateJson in opt.Flags) then
    begin
      // The common path: one native parser, one writer, no intermediate JSON.
      if JsonPreprocessToWriter(p, W, target, settings) then
        result := jeSuccess;
      exit;
    end;
    if (opt.InputFormat in [jfYaml, jfCsv]) and
       (jofPreprocessInput in opt.Flags) and
       not (jofNoExpand in opt.Flags) then
    begin
      result := Prepare(jfText, jsonCompact);
      if result <> jeSuccess then
        exit;
    end;
    comments := (opt.OutputFormat in [jfJsonC, jfJson5, jfHjson]) and
                (opt.InputFormat in JOP_JSON_FORMATS) and
                not (jofNoComments in opt.Flags);
    stage := jsonCompact;
    if comments then
      stage := jsonC;
    case opt.InputFormat of
      jfYaml:
        begin
          Source := YamlToJson(Source);
          p := pointer(Source);
          len := length(Source);
        end;
      jfCsv:
        begin
          result := Prepare(jfCsv, stage);
          if result <> jeSuccess then
            exit;
        end;
    else
      // Native document decoding needs no copy for ordinary JSON. Normalize
      // dialects, comments or DSL first; these scans do not validate the input.
      if (opt.InputFormat <> jfJson) or
         (jofValidateJson in opt.Flags) or
         (not (jofNoExpand in opt.Flags) and
          (opt.Defines <> nil)) or
         JopNeedsPrepare(p, not (jofNoExpand in opt.Flags)) then
      begin
        result := Prepare(opt.InputFormat, stage);
        if result <> jeSuccess then
          exit;
      end;
    end;
    result := jeInvalidJson;
    if len = 0 then
      exit;
    if (jofValidateJson in opt.Flags) and
       not JopCheckJson(p, len, comments) then
      exit;
    if opt.OutputFormat in JOP_JSON_FORMATS then
    begin
      if target = stage then
        W.AddNoJsonEscape(p, len)
      else
      begin
        // Converted or expanded values are data, never a second DSL input.
        settings.Flags := [jppNoExpand];
        if jofNoComments in opt.Flags then
          include(settings.Flags, jppNoComments);
        if not JsonPreprocessToWriter(p, W, target, settings) then
          exit;
      end;
      result := jeSuccess;
      exit;
    end;
    if opt.OutputFormat = jfYaml then
      docOptions := JSON_YAML
    else
      docOptions := JSON_TABLE;
    p := GotoNextNotSpace(p);
    if p^ in ['{', '['] then
    begin
      if TDocVariantData(doc).InitJsonInPlace(p, docOptions) = nil then
        exit;
    end
    else
    begin
      // InitJsonInPlace handles containers; retain native scalar support.
      field.Json := p;
      JsonToAnyVariant(doc, field, @docOptions, {AllowDouble=}true);
      if field.Json = nil then
        exit;
    end;
    case opt.OutputFormat of
      jfYaml:
        begin
          text := VariantToYaml(doc);
          W.AddNoJsonEscape(pointer(text), length(text));
          result := jeSuccess;
        end;
      jfMd, jfMdPipe:
        begin
          mdFormat := ttfMarkdownCsv;
          if opt.OutputFormat = jfMdPipe then
            mdFormat := ttfMarkdownPipe;
          result := JOP_TABLE_ERROR[VariantToMd(doc, text, detail,
            opt.TableOptions, opt.TableRoot, mdFormat)];
          if result = jeSuccess then
            W.AddNoJsonEscape(pointer(text), length(text));
        end;
      jfCsv:
        begin
          result := JOP_TABLE_ERROR[VariantToCsv(doc, csv, detail,
            opt.TableOptions, opt.TableRoot)];
          if result = jeSuccess then
            if Tables <> nil then
            begin
              Tables^ := csv;
              for i := 0 to high(csv) do
                inc(Run.OutputBytes, length(csv[i].Value));
            end
            else if length(csv) = 1 then
              W.AddNoJsonEscape(pointer(csv[0].Value), length(csv[0].Value))
            else
              result := jeMultipleTables;
        end;
    end;
    if Info <> nil then
      Info^ := detail;
  finally
    Run.IncludeFiles := included.IncludeFiles;
    Run.InsertFiles := included.InsertFiles;
    Run.IncludeBytes := included.IncludeBytes;
    if (result = jeSuccess) and
       (W <> nil) then
      Run.OutputBytes := W.TextLength - before;
  end;
end;

function JopProcessToWriter(var Source: RawUtf8; W: TJsonWriter;
  const Options: TJopOptions; Statistics: PJopStatistics;
  Info: PRawUtf8): TJopError;
var
  run: TJopStatistics;
  started: Int64;
begin
  started := JopStart(Statistics, run);
  try
    result := JopConvert(Source, W, Options, run, nil, Info);
  except
    on E: Exception do
      result := JopException(E, Info);
  end;
  JopFinish(run, started, result, Statistics);
end;

function JopProcess(var Source: RawUtf8; const Options: TJopOptions;
  out Dest: RawUtf8; Statistics: PJopStatistics; Info: PRawUtf8): TJopError;
var
  w: TJsonWriter;
  temp: TTextWriterStackBuffer;
  run: TJopStatistics;
  started: Int64;
begin
  Dest := '';
  if Info <> nil then
    Info^ := '';
  started := JopStart(Statistics, run);
  w := nil;
  try
    try
      w := TJsonWriter.CreateOwnedStream(temp);
      result := JopConvert(Source, w, Options, run, nil, Info);
      if result = jeSuccess then
        w.SetText(Dest);
    except
      on E: Exception do
      begin
        Dest := '';
        result := JopException(E, Info);
      end;
    end;
  finally
    w.Free;
  end;
  JopFinish(run, started, result, Statistics);
end;

function JopProcess(var Source: RawUtf8; InputFormat, OutputFormat: TJopFormat;
  Flags: TJopFlags): RawUtf8;
var
  error: TJopError;
  info: RawUtf8;
begin
  error := JopProcess(Source, JopOptions(InputFormat, OutputFormat, Flags),
    result, nil, @info);
  if error <> jeSuccess then
    if info = '' then
      EJopException.RaiseUtf8('JOP: %', [JOP_ERROR[error]])
    else
      EJopException.RaiseUtf8('JOP: % (%)', [JOP_ERROR[error], info]);
end;

function JopProcessToStream(var Source: RawUtf8; Dest: TStream;
  const Options: TJopOptions; Statistics: PJopStatistics;
  Info: PRawUtf8): TJopError;
var
  w: TJsonWriter;
  temp: TTextWriterStackBuffer;
  run: TJopStatistics;
  started: Int64;
begin
  if Info <> nil then
    Info^ := '';
  started := JopStart(Statistics, run);
  result := jeInvalidOptions;
  w := nil;
  try
    try
      if Dest <> nil then
      begin
        w := TJsonWriter.Create(Dest, @temp, SizeOf(temp));
        result := JopConvert(Source, w, Options, run, nil, Info);
        if result = jeSuccess then
          w.FlushFinal;
      end;
    except
      on E: Exception do
        result := JopException(E, Info);
    end;
  finally
    w.Free;
  end;
  JopFinish(run, started, result, Statistics);
end;

function JopProcessToCsv(var Source: RawUtf8; const Options: TJopOptions;
  out Tables: TTextPairs; Statistics: PJopStatistics;
  Info: PRawUtf8): TJopError;
var
  opt: TJopOptions;
  run: TJopStatistics;
  started: Int64;
begin
  Tables := nil;
  started := JopStart(Statistics, run);
  opt := Options;
  opt.OutputFormat := jfCsv;
  try
    result := JopConvert(Source, nil, opt, run, @Tables, Info);
  except
    on E: Exception do
    begin
      Tables := nil;
      result := JopException(E, Info);
    end;
  end;
  JopFinish(run, started, result, Statistics);
end;


{ ************ File and Batch Processing }

function JopFullFileName(const Name: TFileName): TFileName;
begin
  result := ExpandFileName(NormalizeFileName(Name));
end;

function JopSameFileName(const A, B: TFileName): boolean;
begin
  result := AnsiCompareFileName(A, B) = 0;
end;

function JopWithinFolder(const Name, Folder: TFileName): boolean;
begin
  // Folder is absolute and ends with PathDelim: out/ does not match out2/.
  result := (length(Name) >= length(Folder)) and
            JopSameFileName(copy(Name, 1, length(Folder)), Folder);
end;

function JopDefaultOutput(InputFormat: TJopFormat): TJopFormat;
begin
  if InputFormat = jfText then
    result := jfText
  else
    result := jfJson;
end;

function JopProcessFile(const Source, Dest: TFileName;
  const Options: TJopOptions; Statistics: PJopStatistics;
  Info: PRawUtf8): TJopError;
var
  src, dst, tmp, folder, context: TFileName;
  opt: TJopOptions;
  data: RawUtf8;
  f: TFileStreamEx;
  started, finished: Int64;
  inputBytes: PtrInt;
  stats: TJopStatistics;
  failure: TJopError;
begin
  result := jeInvalidPath;
  failure := result;
  if Info <> nil then
    Info^ := '';
  FillCharFast(stats, SizeOf(stats), 0);
  QueryPerformanceMicroSeconds(started);
  context := Source;
  inputBytes := 0;
  try
    try
      if (Source = '') or
         (Dest = '') then
        exit;
      src := JopFullFileName(Source);
      dst := JopFullFileName(Dest);
      result := jeSameFile;
      if JopSameFileName(src, dst) then
        exit;
      context := src;
      result := jeNoSource;
      if not FileExists(src) then
        exit;
      context := dst;
      result := jeInvalidPath;
      if DirectoryExists(dst) then
        exit;
      opt := Options;
      result := jeUnsupportedInput;
      if (opt.InputFormat = jfAuto) and
         not JopFormatFromFileName(src, opt.InputFormat) then
        exit;
      if (opt.OutputFormat = jfAuto) and
         not JopFormatFromFileName(dst, opt.OutputFormat) then
        opt.OutputFormat := JopDefaultOutput(opt.InputFormat);
      result := JopCheckOptions(opt);
      if result <> jeSuccess then
        exit;
      if jofDryRun in opt.Flags then
      begin
        stats.FilesSkipped := 1;
        exit;
      end;
      context := src;
      failure := jeRead;
      result := failure;
      data := RawUtf8FromFile(src);
      inputBytes := length(data);
      if data = '' then
        exit; // the native helper returns empty on no input or read failure
      context := dst;
      failure := jeWrite;
      result := failure;
      folder := EnsureDirectoryExists(ExtractPath(dst));
      if folder = '' then
        exit;
      tmp := TemporaryFileName(folder, 'jop');
      f := TFileStreamEx.Create(tmp, fmCreate);
      try
        result := JopProcessToStream(data, f, opt, @stats, Info);
      finally
        f.Free; // close before replacement, including on Windows
      end;
      if result <> jeSuccess then
        exit;
      result := jeWrite;
      if not mormot.core.os.ReplaceFile(tmp, dst) then
      begin
        if Info <> nil then
          Info^ := StringToUtf8(dst);
        exit;
      end;
      tmp := '';
      result := jeSuccess;
    except
      on E: Exception do
      begin
        result := failure;
        if Info <> nil then
          Info^ := StringToUtf8(E.Message);
      end;
    end;
  finally
    if tmp <> '' then
      DeleteFile(tmp);
    stats.InputBytes := inputBytes;
    if result <> jeSuccess then
    begin
      stats.FilesSucceeded := 0;
      stats.FilesFailed := 1;
      stats.OutputBytes := 0;
      if (Info <> nil) and
         (Info^ = '') then
        Info^ := StringToUtf8(context);
    end;
    QueryPerformanceMicroSeconds(finished);
    stats.ElapsedMicroSeconds := finished - started;
    JopAddStatistics(Statistics, stats);
  end;
end;

function JopProcessFiles(const InputMask, OutputFolder: TFileName;
  const Options: TJopOptions; out Files: TJopFileResults;
  Statistics: PJopStatistics; Info: PRawUtf8): TJopError;
var
  mask, root, output, name, relative, context: TFileName;
  matches, names: TFileNameDynArray;
  find: TFindFilesOptions;
  infmt, outfmt: TJopFormat;
  allfiles, excludeoutput: boolean;
  i, j, n, c: PtrInt;
  da: TDynArray;
  stats: TJopStatistics;
  started, finished: Int64;
begin
  result := jeInvalidPath;
  Files := nil;
  if Info <> nil then
    Info^ := '';
  FillCharFast(stats, SizeOf(stats), 0);
  QueryPerformanceMicroSeconds(started);
  context := InputMask;
  try
    try
      if (InputMask = '') or
         (OutputFolder = '') then
        exit;
      context := OutputFolder;
      output := IncludeTrailingPathDelimiter(JopFullFileName(OutputFolder));
      if FileExists(ExcludeTrailingPathDelimiter(output)) then
        exit;
      context := InputMask;
      result := jeInvalidPath;
      mask := NormalizeFileName(InputMask);
      allfiles := DirectoryExists(mask);
      if allfiles then
      begin
        root := IncludeTrailingPathDelimiter(JopFullFileName(mask));
        mask := '*';
      end
      else
      begin
        root := ExtractPath(mask);
        mask := ExtractFileName(mask);
        if (Pos('*', root) <> 0) or
           (Pos('?', root) <> 0) then
          exit;
        if root = '' then
          root := '.';
        root := IncludeTrailingPathDelimiter(JopFullFileName(root));
      end;
      result := jeNoSource;
      if not DirectoryExists(root) then
        exit;
      find := [ffoSortByFullName];
      if jofRecursive in Options.Flags then
        include(find, ffoSubFolder);
      matches := FileNames(root, mask, find);
      excludeoutput := JopWithinFolder(output, root) and
                       not JopSameFileName(output, root);
      SetLength(Files, length(matches));
      n := 0;
      for i := 0 to high(matches) do
      begin
        name := JopFullFileName(matches[i]);
        if excludeoutput and
           JopWithinFolder(name, output) then
          continue;
        Files[n].Source := name;
        infmt := Options.InputFormat;
        if infmt = jfAuto then
          JopFormatFromFileName(name, infmt);
        if not (infmt in JOP_INPUT_FORMATS) then
        begin
          if allfiles then
            Files[n].Status := jfsSkipped
          else
          begin
            Files[n].Status := jfsFailed;
            Files[n].Error := jeUnsupportedInput;
          end;
          inc(n);
          continue;
        end;
        outfmt := Options.OutputFormat;
        if outfmt = jfAuto then
          outfmt := JopDefaultOutput(infmt);
        relative := copy(name, length(root) + 1, MaxInt);
        Files[n].Dest := ChangeFileExt(output + relative,
          JOP_FORMAT_EXTENSIONS[outfmt]);
        inc(n);
      end;
      SetLength(Files, n);
      result := jeNoFiles;
      if n = 0 then
        exit;
      // Include skipped sources in collision checks before writing any output.
      SetLength(names, n * 2);
      c := 0;
      for i := 0 to n - 1 do
      begin
        names[c] := Files[i].Source;
        inc(c);
        if Files[i].Dest <> '' then
        begin
          names[c] := Files[i].Dest;
          inc(c);
        end;
      end;
      SetLength(names, c);
      da.Init(TypeInfo(TFileNameDynArray), names);
      da.Sort(@SortDynArrayFileName);
      result := jeCollision;
      for i := 1 to c - 1 do
        if JopSameFileName(names[i - 1], names[i]) then
        begin
          context := names[i];
          for j := 0 to n - 1 do
            if JopSameFileName(Files[j].Dest, names[i]) then
            begin
              Files[j].Status := jfsFailed;
              Files[j].Error := result;
            end;
          exit;
        end;
      result := jeSuccess;
      for i := 0 to n - 1 do
      begin
        if Files[i].Status = jfsPending then
        begin
          Files[i].Error := JopProcessFile(Files[i].Source, Files[i].Dest,
            Options, @stats, @Files[i].Info);
          if Files[i].Error = jeSuccess then
            if jofDryRun in Options.Flags then
              Files[i].Status := jfsSkipped
            else
              Files[i].Status := jfsSucceeded
          else
            Files[i].Status := jfsFailed;
        end;
        if (Files[i].Status = jfsFailed) and
           (result = jeSuccess) then
        begin
          result := Files[i].Error;
          context := Files[i].Source;
          if Info <> nil then
            Info^ := Files[i].Info;
        end;
      end;
    except
      on E: Exception do
      begin
        result := jeException;
        if Info <> nil then
          Info^ := StringToUtf8(E.Message);
      end;
    end;
  finally
    if (result <> jeSuccess) and
       (Info <> nil) and
       (Info^ = '') then
      Info^ := StringToUtf8(context);
    // Recount the final plan once, including errors found before any file opens.
    stats.FilesSucceeded := 0;
    stats.FilesFailed := 0;
    stats.FilesSkipped := 0;
    for i := 0 to high(Files) do
      case Files[i].Status of
        jfsSucceeded:
          inc(stats.FilesSucceeded);
        jfsFailed:
          inc(stats.FilesFailed);
        jfsSkipped:
          inc(stats.FilesSkipped);
      end;
    QueryPerformanceMicroSeconds(finished);
    stats.ElapsedMicroSeconds := finished - started;
    JopAddStatistics(Statistics, stats);
  end;
end;

{ ************ Random Documents and Compact JSON Comparison }

function JopGenerate(W: TJsonWriter; Depth, Items: integer;
  Seed: cardinal): TJopError;
var
  random: TLecuyer;
  i: integer;

  procedure Scalar;
  var
    text: TShort31;
  begin
    case random.Next(6) of
      0:
        W.AddShort('null');
      1:
        W.Add(random.Next(2) <> 0);
      2:
        W.Add(integer(random.Next(2000001)) - 1000000);
      3:
        begin
          W.Add(integer(random.Next(2000001)) - 1000000);
          W.Add('.');
          W.AddU(random.Next(1000));
        end;
    else
      begin
        // FillShort31 XORs its buffer: clear it for deterministic seeded output.
        FillCharFast(text, SizeOf(text), 0);
        random.FillShort31(text);
        W.Add('"');
        if text[0] <> #0 then
          W.AddJsonEscape(@text[1], length(text));
        W.Add('"');
      end;
    end;
  end;

  procedure Value(Level: integer);
  var
    i, n: integer;
    arr: boolean;
  begin
    if Level = 0 then
    begin
      Scalar;
      exit;
    end;
    arr := random.Next(2) = 0;
    if arr then
      W.Add('[')
    else
      W.Add('{');
    n := 1 + random.Next(3);
    for i := 0 to n - 1 do
    begin
      if i <> 0 then
        W.Add(',');
      if not arr then
      begin
        W.AddShort('"v');
        W.AddU(i);
        W.AddShort('":');
      end;
      if i = 0 then
        Value(Level - 1)
      else
        Scalar;
    end;
    if arr then
      W.Add(']')
    else
      W.Add('}');
  end;

begin
  result := jeInvalidOptions;
  if (W = nil) or
     (Depth < 0) or
     (Depth > 64) or
     (Items < 0) then
    exit;
  random.SeedGenerator(Seed);
  try
    if Depth = 0 then
      Scalar
    else
    begin
      W.Add('[');
      for i := 0 to Items - 1 do
      begin
        if i <> 0 then
          W.Add(',');
        Value(Depth - 1);
      end;
      W.Add(']');
    end;
    result := jeSuccess;
  except
    on Exception do
      result := jeWrite;
  end;
end;

function JopCompareFiles(const Left, Right: TFileName; out Equal: boolean;
  Info: PRawUtf8): TJopError;
var
  a, b: RawUtf8;

  function Normalize(const Name: TFileName; out Json: RawUtf8): TJopError;
  var
    source: RawUtf8;
  begin
    result := jeNoSource;
    if FileExists(Name) then
    begin
      source := RawUtf8FromFile(Name);
      result := jeRead;
      if source <> '' then
      begin
        Json := JsonReformat(source, jsonCompact);
        result := jeSuccess;
      end;
    end;
    if (result <> jeSuccess) and
       (Info <> nil) then
      Info^ := StringToUtf8(Name);
  end;

begin
  Equal := false;
  if Info <> nil then
    Info^ := '';
  try
    result := Normalize(Left, a);
    if result = jeSuccess then
      result := Normalize(Right, b);
    if result = jeSuccess then
      Equal := a = b;
  except
    on E: Exception do
    begin
      result := jeException;
      if Info <> nil then
        Info^ := StringToUtf8(E.Message);
    end;
  end;
end;

end.
