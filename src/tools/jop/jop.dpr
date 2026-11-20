program jop;

{
  JSON and Text Processing Command Line Tool

  This file is a part of the Open Source Synopse mORMot framework 2,
  licensed under a MPL/GPL/LGPL three license - see LICENSE.md
}

{$I ..\..\mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$apptype console}
  {$R ..\..\mormot.win.default.manifest.res}
{$endif OSWINDOWS}

uses
  {$I ..\..\mormot.uses.inc}
  classes,
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.json,
  mormot.core.fmt,
  mormot.tools.jop in 'mormot.tools.jop.pas';


type
  TJopCommand = (
    jcConvert,
    jcBatch,
    jcGenerate,
    jcCompare);

const
  COMMAND_NAMES: array[TJopCommand] of RawUtf8 = (
    'convert', 'batch', 'generate', 'compare');

  DESCRIPTION: RawUtf8 =
    'JOP - JSON and text processing with mORMot 2'#13#10 +
    '  jop convert source destination [options]'#13#10 +
    '  jop source destination [options]'#13#10 +
    '  jop batch "input/*.json" output-folder [options]'#13#10 +
    '  jop generate output.json [--depth 3 --items 10 --seed 42]'#13#10 +
    '  jop compare left.json right.json5'#13#10 +
    'Use <command> --help for its parameters. Put filenames before switches.'#13#10 +
    'Formats: json, jsonc, json5, hjson, morml, yaml, csv, text.'#13#10 +
    'Output also accepts md/md-csv (fenced CSV) and md-pipe (pipe tables).'#13#10 +
    'Compare uses native compact JSON text, without validation or expansion;'#13#10 +
    'comments/formatting are ignored, object property order is significant.'#13#10 +
    'Exit codes: 0 success/equal, 1 different, 2 usage or processing error.';

  FLAG_NAMES: array[TJopFlag] of RawUtf8 = (
    'validate-utf8',
    'no-expand',
    'no-comments',
    'no-namespaces',
    'preprocess-input',
    'debug-comments',
    'absolute-paths',
    'allow-missing-includes',
    'compact',
    'recursive',
    'dry-run',
    'validate-json');
  FLAG_HELP: array[TJopFlag] of RawUtf8 = (
    'explicitly validate UTF-8; input is otherwise opaque 8-bit text',
    'bypass preprocessing, including defines and includes',
    'omit JSON comments',
    'disable external namespaces',
    'preprocess YAML/CSV source text before parsing',
    'add preprocessing diagnostics to comment-capable output',
    'allow absolute paths in include/insert directives',
    'ignore missing includes',
    'write compact JSON',
    'recurse into subfolders in batch mode',
    'show planned file processing without reading or writing documents',
    'explicitly apply the native JSON structural check');

  TABLE_FLAG_NAMES: array[TTableOption] of RawUtf8 = (
    'first-row-fields', 'strict-fields', 'strict-types',
    'mixed-as-json', 'null-as-text', 'keep-array-index');
  TABLE_FLAG_HELP: array[TTableOption] of RawUtf8 = (
    'use the first row to establish table fields',
    'require a consistent table field set',
    'require consistent table field types',
    'encode mixed table values as JSON',
    'write null table values as text',
    'retain native child-array indexes');


function ProcessingOptions(C: TExecutableCommandLine;
  out Options: TJopOptions; out Info: RawUtf8): TJopError;
var
  f: TJopFlag;
  t: TTableOption;
  value: RawUtf8;
begin
  result := jeSuccess;
  Info := '';
  Options := JopOptions;
  value := C.Param('from', 'input #format, or auto from the file extension', 'auto');
  if not JopFormatFromName(value, Options.InputFormat) then
  begin
    result := jeInvalidOptions;
    Info := 'Unknown --from format: ' + value;
  end;
  value := C.Param('to', 'output #format, or auto from the file extension', 'auto');
  if not JopFormatFromName(value, Options.OutputFormat) then
  begin
    result := jeInvalidOptions;
    Info := 'Unknown --to format: ' + value;
  end;
  for f := low(f) to high(f) do
    if C.Option(FLAG_NAMES[f], FLAG_HELP[f]) then
      include(Options.Flags, f);
  Options.IncludeRoot := C.ParamS('include-root',
    'base #folder for include/insert directives');
  C.Get(['D', 'define'], Options.Defines,
    'literal #name=value definition; may be repeated', {includeempty=}true);
  value := C.Param('delimiter', 'CSV input #character, or tab; default comma');
  if IdemPropNameU(value, 'tab') then
    Options.CsvDelimiter := #9
  else if length(value) = 1 then
    Options.CsvDelimiter := value[1]
  else if value <> '' then
  begin
    result := jeInvalidOptions;
    Info := '--delimiter expects one 8-bit character or tab';
  end;
  for t := low(t) to high(t) do
    if C.Option(TABLE_FLAG_NAMES[t], TABLE_FLAG_HELP[t]) then
      include(Options.TableOptions, t);
  Options.TableRoot := C.Param('table-root',
    'root table #name for CSV/Markdown output', 'root');
end;

function GenerateFile(const Dest: TFileName; Depth, Items: integer;
  Seed: cardinal; out Info: RawUtf8): TJopError;
var
  target, temp, folder: TFileName;
  w: TJsonWriter;
begin
  result := jeWrite;
  Info := '';
  temp := '';
  try
    try
      target := ExpandFileName(Dest);
      folder := EnsureDirectoryExists(ExtractPath(target));
      if folder = '' then
        exit;
      temp := TemporaryFileName(folder, 'jop');
      w := TJsonWriter.CreateOwnedFileStream(temp);
      try
        result := JopGenerate(w, Depth, Items, Seed);
        if result = jeSuccess then
          w.FlushFinal;
      finally
        w.Free;
      end;
      if result <> jeSuccess then
        exit;
      result := jeWrite;
      if not mormot.core.os.ReplaceFile(temp, target) then
        exit;
      temp := '';
      result := jeSuccess;
    except
      on E: Exception do
      begin
        result := jeWrite;
        Info := StringToUtf8(E.Message);
      end;
    end;
  finally
    if temp <> '' then
      DeleteFile(temp);
    if (result <> jeSuccess) and
       (Info = '') then
      Info := StringToUtf8(Dest);
  end;
end;

procedure ErrorToConsole(Error: TJopError; const Info: RawUtf8);
begin
  if Info = '' then
    ConsoleWrite('JOP: %', [JOP_ERROR[Error]], ccLightRed)
  else
    ConsoleWrite('JOP: % (%)', [JOP_ERROR[Error], Info], ccLightRed);
end;

procedure Run;
var
  c: TExecutableCommandLine;
  command, candidate: TJopCommand;
  options: TJopOptions;
  statistics: TJopStatistics;
  files: TJopFileResults;
  error: TJopError;
  first, info, unknown, value: RawUtf8;
  source, dest: TFileName;
  offset, depth, items, i: integer;
  seed: cardinal;
  number: Int64;
  help, stats, equal: boolean;
begin
  ExitCode := 2;
  c := Executable.Command;
  c.ExeDescription := DESCRIPTION;
  command := jcConvert;
  offset := 0;
  first := '';
  if length(c.Args) <> 0 then
    first := c.Args[0];
  for candidate := low(candidate) to high(candidate) do
    if IdemPropNameU(first, COMMAND_NAMES[candidate]) then
    begin
      command := candidate;
      offset := 1;
      c.Arg(0, '#command convert, batch, generate or compare');
      break;
    end;
  help := c.Option(['h', 'help', '?'], 'display help for this command');
  if IdemPropNameU(first, 'help') then
  begin
    help := true;
    c.Arg(0, '#command convert, batch, generate or compare');
  end;
  error := jeSuccess;
  stats := false;
  FillCharFast(statistics, SizeOf(statistics), 0);
  case command of
    jcConvert,
    jcBatch:
      begin
        source := c.ArgString(offset, 'source #filename or batch mask', false);
        dest := c.ArgString(offset + 1,
          'destination #filename or batch folder', false);
        error := ProcessingOptions(c, options, info);
        stats := c.Option('stats', 'display processing counters and throughput');
      end;
    jcGenerate:
      begin
        dest := c.ArgString(offset, 'destination JSON #filename', false);
        if not c.Get('depth', 0, 64, depth,
          'maximum container #depth; 0 scalar, 1 flat array', 3) and
           c.Has('depth') then
        begin
          error := jeInvalidOptions;
          info := '--depth expects an integer in 0..64';
        end;
        if not c.Get('items', 0, maxInt, items,
          'root array #count; ignored for depth 0', 10) and
           c.Has('items') then
        begin
          error := jeInvalidOptions;
          info := '--items expects a nonnegative integer';
        end;
        seed := Random32;
        if c.Get('seed', value, 'unsigned 32-bit #seed; default random') then
          if ToInt64(value, number) and
             (number >= 0) and
             (number <= high(cardinal)) then
            seed := number
          else
          begin
            error := jeInvalidOptions;
            info := '--seed expects an integer in 0..4294967295';
          end;
      end;
    jcCompare:
      begin
        source := c.ArgString(offset, 'left JSON #filename', false);
        dest := c.ArgString(offset + 1, 'right JSON #filename', false);
      end;
  end;
  if help or
     (length(c.Args) = 0) and
     (length(c.Options) = 0) and
     (length(c.Names) = 0) then
  begin
    ConsoleWrite(c.FullDescription);
    ExitCode := 0;
    exit;
  end;
  unknown := c.DetectUnknown;
  if unknown <> '' then
  begin
    ConsoleWrite(c.FullDescription);
    ConsoleWrite(unknown, ccLightRed);
    exit;
  end;
  if error <> jeSuccess then
  begin
    ErrorToConsole(error, info);
    exit;
  end;
  case command of
    jcConvert:
      begin
        error := JopProcessFile(source, dest, options, @statistics, @info);
        if error = jeSuccess then
          if jofDryRun in options.Flags then
            ConsoleWrite('Planned: % -> %', [source, dest])
          else
            ConsoleWrite('Written: %', [dest]);
      end;
    jcBatch:
      begin
        error := JopProcessFiles(source, dest, options, files, @statistics, @info);
        for i := 0 to high(files) do
          if files[i].Status = jfsFailed then
            ConsoleWrite('%: % (%)', [files[i].Source,
              JOP_ERROR[files[i].Error], files[i].Info], ccLightRed)
          else if files[i].Status = jfsSkipped then
            ConsoleWrite('Planned: % -> %', [files[i].Source, files[i].Dest]);
        ConsoleWrite('Files: % succeeded, % failed, % skipped',
          [statistics.FilesSucceeded, statistics.FilesFailed,
           statistics.FilesSkipped]);
      end;
    jcGenerate:
      begin
        error := GenerateFile(dest, depth, items, seed, info);
        if error = jeSuccess then
          ConsoleWrite('Generated: % (depth %, items %, seed %)',
            [dest, depth, items, Int64(seed)]);
      end;
    jcCompare:
      begin
        error := JopCompareFiles(source, dest, equal, @info);
        if error = jeSuccess then
          if equal then
            ConsoleWrite('Equal: % / %', [source, dest])
          else
          begin
            ConsoleWrite('Different: % / %', [source, dest]);
            ExitCode := 1;
            exit;
          end;
      end;
  end;
  if stats then
    ConsoleWrite('Bytes: % input + % included -> % output; % us; % MB/s',
      [statistics.InputBytes, statistics.IncludeBytes, statistics.OutputBytes,
       statistics.ElapsedMicroSeconds, JopThroughput(statistics)]);
  if error <> jeSuccess then
    ErrorToConsole(error, info)
  else
    ExitCode := 0;
end;

begin
  try
    Run;
  except
    on E: Exception do
    begin
      ConsoleWrite('JOP: %', [E.Message], ccLightRed);
      ExitCode := 2;
    end;
  end;
end.
