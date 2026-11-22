/// small SQL command line client for our DuckDB implementations
// - this program is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
// - can connect to:
//   - the embedded engine, via duckdb.dll / libduckdb.so (64-bit only)
//   - a mormot.db.proxy server (HTTP), which may publish any database
//   - a remote DuckDB Quack server, with no DuckDB library needed
program duckdbcli;

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$apptype console}
  {$R ..\..\..\..\src\mormot.win.default.manifest.res}
{$endif OSWINDOWS}

uses
  {$I mormot.uses.inc}
  {$ifdef OSWINDOWS}
  windows,
  {$endif OSWINDOWS}
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.json,
  mormot.core.log,
  mormot.core.perf,
  mormot.db.core,
  mormot.db.sql,
  mormot.db.proxy,
  mormot.db.raw.duckdb, // also the Quack client, which works on 32-bit
  mormot.db.sql.duckdb;

type
  TOutputMode = (omTable, omCsv, omJson, omLine);

const
  MODE_NAME: array[TOutputMode] of RawUtf8 = (
    'table', 'csv', 'json', 'line');

  DOT_HELP =
    'Statements end with ";" and may span several lines. Dot commands:'#10 +
    '  .tables  .schema [table]  .mode <mode>  .timer on|off  .version  ' +
      '.help  .quit';

  SQL_TABLES =
    'select table_name from information_schema.tables ' +
    'where table_schema not in (''information_schema'', ''pg_catalog'') ' +
    'order by 1';

  SQL_COLUMNS =
    'select column_name, data_type, is_nullable from information_schema.columns ' +
    'where table_name = ? order by ordinal_position';

var
  Props: TSqlDBConnectionProperties;
  Mode: TOutputMode = omTable;
  Header: boolean = true;
  Timer: boolean = false;
  MaxRows: integer = 10000;

var
  OutStream: THandleStream; // direct and buffered redirection of stdout
  OutText: TTextWriter;

procedure FlushOut;
begin
  OutText.FlushToStream;
end;

{$ifdef OSWINDOWS}
function StdOutIsTTY: boolean; // false if redirected to a file or a pipe
var
  mode: cardinal;
begin
  result := GetConsoleMode(GetStdHandle(STD_OUTPUT_HANDLE), mode);
end;
{$endif OSWINDOWS}

procedure Say(const Text: RawUtf8);
begin
  OutText.AddString(Text);
  OutText.AddDirect(#10);
end;

procedure SayError(const Text: RawUtf8);
begin
  FlushOut;
  ConsoleWrite(Text, ccLightRed);
end;

// number of displayed characters of a UTF-8 text
function CharCount(const Text: RawUtf8): integer;
var
  i: PtrInt;
begin
  result := 0;
  for i := 1 to length(Text) do
    if ord(Text[i]) and $c0 <> $80 then
      inc(result);
end;

function Pad(const Text: RawUtf8; Width: integer): RawUtf8;
var
  n: integer;
begin
  n := Width - CharCount(Text);
  if n > 0 then
    result := Text + RawUtf8(StringOfChar(' ', n))
  else
    result := Text;
end;

function CsvEscape(const Text: RawUtf8): RawUtf8;
begin
  if (PosExChar(',', Text) = 0) and
     (PosExChar('"', Text) = 0) and
     (PosExChar(#10, Text) = 0) and
     (PosExChar(#13, Text) = 0) then
    result := Text
  else
    result := QuotedStr(Text, '"');
end;

// one line of text to display from a possibly multi-line value
function OneLine(const Text: RawUtf8): RawUtf8;
begin
  result := StringReplaceAll(StringReplaceAll(Text, #13, ''), #10, '\n');
end;

function ColumnText(Rows: ISqlDBRows; Col: integer): RawUtf8;
begin
  if Rows.ColumnNull(Col) then
    result := 'NULL'
  else if Rows.ColumnType(Col) = ftBlob then
    result := BinToHexLower(Rows.ColumnBlob(Col)) // never binary on screen
  else
    result := Rows.ColumnUtf8(Col);
end;

/// display the result of an executed statement
procedure DisplayRows(Stmt: ISqlDBStatement);
var
  col, n, row, w, shown: integer;
  names: TRawUtf8DynArray;
  cells: TRawUtf8DynArray;
  widths: array of integer;
  line, sep: RawUtf8;
  numeric: array of boolean;
  more: boolean;
  s: RawUtf8;
begin
  n := Stmt.ColumnCount;
  if n = 0 then
  begin
    Say(FormatUtf8('OK, % row(s) affected', [Stmt.UpdateCount]));
    exit;
  end;
  if Mode = omJson then
  begin
    // the JSON output is not limited
    Say(StringReplaceAll(Stmt.FetchAllAsJson(true), #10, ''));
    exit;
  end;
  SetLength(names, n);
  SetLength(widths, n);
  SetLength(numeric, n);
  for col := 0 to n - 1 do
  begin
    names[col] := Stmt.ColumnName(col);
    widths[col] := CharCount(names[col]);
    try
      numeric[col] := Stmt.ColumnType(col) in [ftInt64, ftDouble, ftCurrency];
    except
      numeric[col] := false; // e.g. mormot.db.proxy with no row
    end;
  end;
  row := 0;
  case Mode of
    omCsv:
      begin
        if Header then
        begin
          line := '';
          for col := 0 to n - 1 do
            line := line + CsvEscape(names[col]) + ',';
          SetLength(line, length(line) - 1);
          Say(line);
        end;
        while Stmt.Step do
        begin
          for col := 0 to n - 1 do
          begin
            if col > 0 then
              OutText.AddDirect(',');
            OutText.AddString(CsvEscape(ColumnText(Stmt, col))); // NULL as DuckDB
          end;
          OutText.AddDirect(#10);
          inc(row);
        end;
      end;
    omLine:
      begin
        w := 0;
        for col := 0 to n - 1 do
          if widths[col] > w then
            w := widths[col];
        while Stmt.Step do
        begin
          if row > 0 then
            Say('');
          for col := 0 to n - 1 do
            Say(Pad(names[col], w) + ' = ' + ColumnText(Stmt, col));
          inc(row);
        end;
      end;
  else
    begin
      // omTable: all displayed rows are needed to compute the columns width
      SetLength(cells, 0);
      shown := 0;
      more := false;
      while Stmt.Step do
      begin
        inc(row);
        if shown >= MaxRows then
        begin
          more := true;
          continue; // just count the remaining rows
        end;
        inc(shown);
        SetLength(cells, shown * n);
        for col := 0 to n - 1 do
        begin
          s := OneLine(ColumnText(Stmt, col));
          cells[(shown - 1) * n + col] := s;
          w := CharCount(s);
          if w > widths[col] then
            widths[col] := w;
        end;
      end;
      sep := '+';
      for col := 0 to n - 1 do
        sep := sep + RawUtf8(StringOfChar('-', widths[col] + 2)) + '+';
      if Header then
      begin
        Say(sep);
        line := '|';
        for col := 0 to n - 1 do
          line := line + ' ' + Pad(names[col], widths[col]) + ' |';
        Say(line);
      end;
      Say(sep);
      for w := 0 to shown - 1 do
      begin
        line := '|';
        for col := 0 to n - 1 do
        begin
          s := cells[w * n + col];
          if numeric[col] and
             (s <> 'NULL') then
            line := line + ' ' + RawUtf8(StringOfChar(' ',
              widths[col] - CharCount(s))) + s + ' |'
          else
            line := line + ' ' + Pad(s, widths[col]) + ' |';
        end;
        Say(line);
      end;
      Say(sep);
      if more then
        Say(FormatUtf8('(only the first % rows are displayed)', [MaxRows]));
    end;
  end;
  if Mode in [omTable] then
    Say(FormatUtf8('(% row(s))', [row]));
end;

/// execute one SQL statement and display its result
procedure Execute(const Sql: RawUtf8);
var
  stmt: ISqlDBStatement;
  tm: TPrecisionTimer;
begin
  tm.Start;
  stmt := Props.ThreadSafeConnection.NewStatementPrepared(Sql, true, true);
  stmt.ExecutePrepared;
  DisplayRows(stmt);
  stmt.ReleaseRows;
  if Timer then
    Say(FormatUtf8('Time: %', [tm.Stop]));
  FlushOut; // every statement result is available at once
end;

function ParseMode(const Text: RawUtf8; out Value: TOutputMode): boolean;
var
  m: TOutputMode;
begin
  result := false;
  for m := low(m) to high(m) do
    if IdemPropNameU(Text, MODE_NAME[m]) then
    begin
      Value := m;
      result := true;
      exit;
    end;
end;

/// process a ".xxx" command - returns false for .quit
function DotCommand(const Line: RawUtf8): boolean;
var
  cmd, arg: RawUtf8;
  m: TOutputMode;
  stmt: ISqlDBStatement;
begin
  result := true;
  Split(TrimU(Line), ' ', cmd, arg);
  arg := TrimU(arg);
  if IdemPropNameU(cmd, '.quit') or
     IdemPropNameU(cmd, '.exit') then
    result := false
  else if IdemPropNameU(cmd, '.help') then
    Say(Executable.Command.FullDescription + #10 + DOT_HELP)
  else if IdemPropNameU(cmd, '.tables') then
    Execute(SQL_TABLES)
  else if IdemPropNameU(cmd, '.schema') then
  begin
    if arg = '' then
      Execute('select sql from duckdb_tables() order by table_name')
    else
    begin
      stmt := Props.ThreadSafeConnection.NewStatementPrepared(
        SQL_COLUMNS, true, true);
      stmt.BindTextU(1, arg);
      stmt.ExecutePrepared;
      DisplayRows(stmt);
    end;
  end
  else if IdemPropNameU(cmd, '.mode') then
  begin
    if ParseMode(arg, m) then
      Mode := m
    else
      SayError('Unknown mode, use table, csv, json or line');
  end
  else if IdemPropNameU(cmd, '.timer') then
    Timer := IdemPropNameU(arg, 'on')
  else if IdemPropNameU(cmd, '.version') then
    Say(FormatUtf8('% - mORMot %', [Props.EngineName,
      SYNOPSE_FRAMEWORK_VERSION]))
  else
    SayError(FormatUtf8('Unknown command %, try .help', [cmd]));
end;

/// true if the SQL text is complete, i.e. ends with ; outside of any quote
function IsComplete(const Sql: RawUtf8): boolean;
var
  i: PtrInt;
  q: AnsiChar;
  last: AnsiChar;
begin
  q := #0;
  last := #0;
  for i := 1 to length(Sql) do
    if q <> #0 then
    begin
      if Sql[i] = q then
        q := #0;
    end
    else if Sql[i] in ['''', '"'] then
    begin
      q := Sql[i];
      last := Sql[i];
    end
    else if Sql[i] > ' ' then
      last := Sql[i];
  result := (q = #0) and
            (last = ';');
end;

var
  Pending: RawUtf8; // the statement being typed

/// run the pending statement, displaying any error
procedure RunPending(Interactive: boolean);
begin
  try
    Execute(Pending);
  except
    on E: Exception do
    begin
      SayError(StringReplaceAll(E.Message, #13, ''));
      if not Interactive then
        ExitCode := 1;
    end;
  end;
  Pending := '';
end;

/// process one input line, from the console or from a script
// - returns false when .quit was requested
function Feed(const Line: RawUtf8; Interactive: boolean): boolean;
begin
  result := true;
  if Pending = '' then
  begin
    if TrimU(Line) = '' then
      exit;
    if TrimU(Line)[1] = '.' then
    begin
      result := DotCommand(Line);
      exit;
    end;
    Pending := Line;
  end
  else
    Pending := Pending + #10 + Line;
  if IsComplete(Pending) then
    RunPending(Interactive);
end;

procedure Interact;
var
  s: string;
begin
  if StdOutIsTTY then // no prompt when the output is redirected
    Say('mORMot DuckDB client - ' + Props.EngineName +
      ' - type .help for help, .quit to exit');
  repeat
    if StdOutIsTTY then
    begin
      FlushOut;
      if Pending = '' then
        write('duckdb> ')
      else
        write('   ...> ');
    end;
    if eof(input) then
      break;
    readln(s);
  until not Feed(StringToUtf8(s), {interactive=}true);
  if TrimU(Pending) <> '' then
    RunPending(true); // a last statement without the ending ; before EOF
end;

procedure RunScript(const Lines: TRawUtf8DynArray);
var
  i: PtrInt;
begin
  for i := 0 to high(Lines) do
    if not Feed(Lines[i], false) then
      exit;
  if TrimU(Pending) <> '' then
    RunPending(false); // a last statement without the ending ;
end;

function ReadScript(const FileName: TFileName): TRawUtf8DynArray;
var
  txt: RawUtf8;
begin
  txt := StringFromFile(FileName);
  if txt = '' then
    raise Exception.CreateFmt('Unable to read %s', [FileName]);
  result := CsvToRawUtf8DynArray(StringReplaceAll(txt, #13, ''), #10);
end;

var
  duckfile, proxy, root, quack, user, pass, command, script, modetxt: RawUtf8;
  {$ifdef CPU64}
  readonly: boolean;
  {$endif CPU64}
  lines: TRawUtf8DynArray;

begin
  with Executable.Command do
  begin
    ExeDescription := 'mORMot DuckDB SQL command line client';
    // the connection: embedded engine by default
    {$ifdef CPU64}
    duckfile := Param(['d', 'duckdb'], 'embedded engine #file ' +
      '(duckdb.dll/libduckdb.so, 64-bit), :memory: by default');
    if duckfile = '' then
      duckfile := ArgU(0, 'the embedded DuckDB database #file');
    readonly := Option(['r', 'readonly'],
      'open the embedded database file in read-only mode');
    {$endif CPU64}
    proxy := Param(['p', 'proxy'], 'mormot.db.proxy #address as host:port over HTTP');
    root := Param('root', 'the proxy URI #name as set on the server');
    quack := Param(['q', 'quack'],
      'Quack server #uri like quack:localhost or quack:host:9494');
    user := Param(['u', 'user'], 'the proxy #user or the Quack client identifier');
    pass := Param(['w', 'password'], 'the proxy #password or the Quack token - ' +
      'DUCKDB_TOKEN environment variable is used for Quack by default');
    // the execution
    command := Param(['c', 'command'], 'execute the #sql statement(s) and exit');
    script := Param(['f', 'file'], 'execute a #script file and exit');
    modetxt := Param(['m', 'mode'], 'output #mode as table, csv, json or line',
      'table');
    Header := not Option('noheader', 'do not display the column names');
    MaxRows := Param('maxrows', 10000, 'rows displayed in table mode');
    if ConsoleHelpFailed('mORMot DuckDB SQL command line client') then
      exit;
  end;
  if not ParseMode(modetxt, Mode) then
  begin
    SayError('Unknown mode ' + modetxt);
    halt(2);
  end;
  if ord(proxy <> '') + ord(quack <> '') + ord(duckfile <> '') > 1 then
  begin
    SayError('Please select only one of duckdb, proxy and quack');
    halt(2);
  end;
  Props := nil;
  AllocConsole; // initialize StdOut before using it
  OutStream := THandleStream.Create(StdOut);
  OutText := TTextWriter.Create(OutStream, 65536);
  try
    try
      if quack <> '' then
      begin
        if pass = '' then
          pass := StringToUtf8(GetEnvironmentVariable('DUCKDB_TOKEN'));
        Props := TSqlDBQuackConnectionProperties.Create(quack, '', user, pass);
      end
      else if proxy <> '' then
        Props := TSqlDBSocketConnectionProperties.Create(
          proxy, root, user, pass)
      else
      begin
        {$ifdef CPU64}
        if duckfile = '' then
          duckfile := ':memory:';
        Props := TSqlDBDuckDBConnectionProperties.Create(duckfile, '', '', '');
        (Props as TSqlDBDuckDBConnectionProperties).ReadOnly := readonly;
        {$else}
        SayError('The embedded DuckDB engine is only available for 64-bit');
        halt(1);
        {$endif CPU64}
      end;
      // test the connection now, to display any error early
      Props.ThreadSafeConnection.Connect;
      if command <> '' then
      begin
        SetLength(lines, 1);
        lines[0] := command;
        RunScript(lines);
      end
      else if script <> '' then
        RunScript(ReadScript(UTF8ToString(script)))
      else
        Interact;
    except
      on E: Exception do
      begin
        SayError(StringReplaceAll(E.Message, #13, ''));
        ExitCode := 1;
      end;
    end;
  finally
    OutText.FlushFinal;
    OutText.Free;
    OutStream.Free;
    Props.Free;
  end;
end.
