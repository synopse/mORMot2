/// Console Applications Support
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.app.console;

{
  *****************************************************************************

   Some Features Dedicated to Console Apps
    - ICommandLine for Parsing Command Line Arguments

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  variants,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.buffers,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.core.data,
  mormot.core.variants;



{ ************ ICommandLine for Parsing Command Line Arguments }

type
  /// an interface to process the command line switches over a console
  // - as implemented e.g. by TCommandLine class
  // - can implement any process, optionally with console interactivity
  ICommandLine = interface
    ['{77AB427C-1025-488B-8E04-3E62C8100E62}']
    /// returns a command line switch value as UTF-8 text
    // - you can specify a prompt text, when asking for any missing switch
    function AsUtf8(const Switch, Default: RawUtf8;
      const Prompt: string): RawUtf8;
    /// returns a command line switch value as VCL string text
    // - you can specify a prompt text, when asking for any missing switch
    function AsString(const Switch: RawUtf8; const Default: string;
      const Prompt: string): string;
    /// returns a command line switch value as integer
    // - you can specify a prompt text, when asking for any missing switch
    function AsInt(const Switch: RawUtf8; Default: Int64;
      const Prompt: string): Int64;
    /// returns a command line switch ISO-8601 value as date value
    // - here dates are expected to be encoded with ISO-8601, i.e. YYYY-MM-DD
    // - you can specify a prompt text, when asking for any missing switch
    function AsDate(const Switch: RawUtf8; Default: TDateTime;
      const Prompt: string): TDateTime;
    /// returns a command line switch value as enumeration ordinal
    // - RTTI will be used to check for the enumeration text, or plain integer
    // value will be returned as ordinal value
    // - you can specify a prompt text, when asking for any missing switch
    function AsEnum(const Switch, Default: RawUtf8; TypeInfo: pointer;
      const Prompt: string): integer;
    /// returns all command line values as an array of UTF-8 text
    // - i.e. won't interpret the various switches in the input parameters
    // - as created e.g. by TCommandLine.CreateAsArray constructor
    function AsArray: TRawUtf8DynArray;
    /// serialize all recognized switches as UTF-8 JSON text
    function AsJson(Format: TTextWriterJSONFormat): RawUtf8;
    /// equals TRUE if the -noprompt switch has been supplied
    // - may be used to force pure execution without console interaction,
    // e.g. when run from another process
    function NoPrompt: boolean;
    /// change the console text color
    // - do nothing if NoPrompt is TRUE
    procedure TextColor(Color: TConsoleColor);
    /// write some console text, with an optional color
    // - will output the text even if NoPrompt is TRUE
    procedure Text(const Fmt: RawUtf8; const Args: array of const;
      Color: TConsoleColor = ccLightGray);
  end;

  /// a class to process the command line switches, with console interactivity
  // - is able to redirect all Text() output to an internal UTF-8 storage,
  // in addition or instead of the console (to be used e.g. from a GUI)
  // - implements ICommandLine interface
  TCommandLine = class(TInterfacedObjectWithCustomCreate, ICommandLine)
  private
    fValues: TDocVariantData;
    fNoPrompt: boolean;
    fNoConsole: boolean;
    fLines: TRawUtf8DynArray;
    procedure SetNoConsole(value: boolean);
  public
    /// initialize the internal storage from the command line
    // - will parse "-switch1 value1 -switch2 value2" layout
    // - stand-alone "-switch1 -switch2 value2" will a create switch1=true value
    constructor Create; overload; override;
    /// initialize the internal storage from the command line
    // - will set paramstr(firstParam)..paramstr(paramcount) in fValues as array
    // - may be used e.g. for "val1 val2 val3" command line layout
    constructor CreateAsArray(firstParam: integer);
    /// initialize the internal storage with some ready-to-use switches
    // - will also set the NoPrompt option, and set the supplied NoConsole value
    // - may be used e.g. from a graphical interface instead of console mode
    constructor Create(const switches: variant; aNoConsole: boolean = true);
      reintroduce; overload;
    /// initialize the internal storage with some ready-to-use name/value pairs
    // - will also set the NoPrompt option, and set the supplied NoConsole value
    // - may be used e.g. from a graphical interface instead of console mode
    constructor Create(const NameValuePairs: array of const;
        aNoConsole: boolean = true); reintroduce; overload;
    /// returns a command line switch value as UTF-8 text
    // - you can specify a prompt text, when asking for any missing switch
    function AsUtf8(const Switch, Default: RawUtf8;
      const Prompt: string): RawUtf8;
    /// returns a command line switch value as VCL string text
    // - you can specify a prompt text, when asking for any missing switch
    function AsString(const Switch: RawUtf8; const Default: string;
      const Prompt: string): string;
    /// returns a command line switch value as integer
    // - you can specify a prompt text, when asking for any missing switch
    function AsInt(const Switch: RawUtf8; Default: Int64;
      const Prompt: string): Int64;
    /// returns a command line switch ISO-8601 value as date value
    // - here dates are expected to be encoded with ISO-8601, i.e. YYYY-MM-DD
    // - you can specify a prompt text, when asking for any missing switch
    function AsDate(const Switch: RawUtf8; Default: TDateTime;
      const Prompt: string): TDateTime;
    /// returns a command line switch value as enumeration ordinal
    // - RTTI will be used to check for the enumeration text, or plain integer
    // value will be returned as ordinal value
    // - you can specify a prompt text, when asking for any missing switch
    function AsEnum(const Switch, Default: RawUtf8; TypeInfo: pointer;
      const Prompt: string): integer;
    /// returns all command line values as an array of UTF-8 text
    // - i.e. won't interpret the various switches in the input parameters
    // - as created e.g. by TCommandLine.CreateAsArray constructor
    function AsArray: TRawUtf8DynArray;
    /// serialize all recognized switches as UTF-8 JSON text
    function AsJson(Format: TTextWriterJSONFormat): RawUtf8;
    /// equals TRUE if the -noprompt switch has been supplied
    // - may be used to force pure execution without console interaction,
    // e.g. when run from another process
    function NoPrompt: boolean;
    /// change the console text color
    // - do nothing if NoPrompt is TRUE
    procedure TextColor(Color: TConsoleColor);
    /// write some console text, with an optional color
    // - will output the text even if NoPrompt=TRUE, but not if NoConsole=TRUE
    // - will append the text to the internal storage, available from ConsoleText
    procedure Text(const Fmt: RawUtf8; const Args: array of const;
      Color: TConsoleColor = ccLightGray);
    /// returns the UTF-8 text as inserted by Text() calls
    // - line feeds will be included to the ConsoleLines[] values
    function ConsoleText(const LineFeed: RawUtf8 = sLineBreak): RawUtf8;
    /// low-level access to the internal switches storage
    property Values: TDocVariantData read fValues;
    /// if Text() should be redirected to ConsoleText internal storage
    // - and don't write anything to the console
    // - should be associated with NoProperty = TRUE property
    property NoConsole: boolean
      read fNoConsole write SetNoConsole;
    /// low-level access to the internal UTF-8 console lines storage
    property ConsoleLines: TRawUtf8DynArray
      read fLines;
  end;



implementation


{ ************ ICommandLine for Parsing Command Line Arguments }

{ TCommandLine }

constructor TCommandLine.Create;
var
  i: integer;
  p, sw: RawUtf8;
begin
  inherited Create;
  fValues.InitFast(ParamCount shr 1, dvObject);
  for i := 1 to ParamCount do
  begin
    p := StringToUtf8(ParamStr(i));
    if p <> '' then
      if p[1] in ['-', '/'] then
      begin
        if {%H-}sw <> '' then
          fValues.AddValue(sw, true); // -flag -switch value -> flag=true
        sw := LowerCase(copy(p, 2, 100));
        if sw = 'noprompt' then
        begin
          fNoPrompt := true;
          sw := '';
        end;
      end
      else if sw <> '' then
      begin
        fValues.AddValueFromText(sw, p, true);
        sw := '';
      end;
  end;
  if sw <> '' then
    fValues.AddValue(sw, true); // trailing -flag
end;

constructor TCommandLine.Create(const switches: variant;
  aNoConsole: boolean);
begin
  inherited Create;
  fValues.InitCopy(switches, JSON_OPTIONS_FAST);
  fNoPrompt := true;
  fNoConsole := aNoConsole;
end;

constructor TCommandLine.Create(const NameValuePairs: array of const;
  aNoConsole: boolean);
begin
  inherited Create;
  fValues.InitObject(NameValuePairs, JSON_OPTIONS_FAST);
  fNoPrompt := true;
  fNoConsole := aNoConsole;
end;

constructor TCommandLine.CreateAsArray(firstParam: integer);
var
  i: integer;
begin
  inherited Create;
  fValues.InitFast(ParamCount, dvArray);
  for i := firstParam to ParamCount do
    fValues.AddItem(ParamStr(i));
end;

function TCommandLine.NoPrompt: boolean;
begin
  result := fNoPrompt;
end;

function TCommandLine.ConsoleText(const LineFeed: RawUtf8): RawUtf8;
begin
  result := RawUtf8ArrayToCsv(fLines, LineFeed);
end;

procedure TCommandLine.SetNoConsole(value: boolean);
begin
  if value = fNoConsole then
    exit;
  if value then
    fNoPrompt := true;
  fNoConsole := false;
end;

procedure TCommandLine.TextColor(Color: TConsoleColor);
begin
  if not fNoPrompt then
    mormot.core.os.TextColor(Color);
end;

procedure TCommandLine.Text(const Fmt: RawUtf8; const Args: array of const;
  Color: TConsoleColor);
var
  msg: RawUtf8;
begin
  FormatUtf8(Fmt, Args, msg);
  {$I-}
  if msg <> '' then
  begin
    TextColor(Color);
    AddRawUtf8(fLines, msg);
    if not fNoConsole then
      write(Utf8ToConsole(msg));
  end;
  if not fNoConsole then
  begin
    writeln;
    ioresult;
  end;
  {$I+}
end;

function TCommandLine.AsUtf8(const Switch, Default: RawUtf8;
  const Prompt: string): RawUtf8;
var
  i: integer;
begin
  i := fValues.GetValueIndex(Switch);
  if i >= 0 then
  begin // found
    VariantToUtf8(fValues.Values[i], result);
    fValues.Delete(i);
    exit;
  end;
  result := Default;
  if fNoPrompt or (Prompt = '') then
    exit;
  TextColor(ccLightGray);
  {$I-}
  writeln(Prompt);
  if ioresult <> 0 then
    exit; // no console -> no prompt
  TextColor(ccCyan);
  write(Switch);
  if Default <> '' then
    write(' [', Default, '] ');
  write(': ');
  TextColor(ccWhite);
  readln(result);
  writeln;
  ioresult;
  {$I+}
  TextColor(ccLightGray);
  result := TrimU(result);
  if result = '' then
    result := Default;
end;

function TCommandLine.AsInt(const Switch: RawUtf8; Default: Int64;
  const Prompt: string): Int64;
var
  res: RawUtf8;
begin
  res := AsUtf8(Switch, Int64ToUtf8(Default), Prompt);
  result := GetInt64Def(pointer(res), Default);
end;

function TCommandLine.AsDate(const Switch: RawUtf8; Default: TDateTime;
  const Prompt: string): TDateTime;
var
  res: RawUtf8;
begin
  res := AsUtf8(Switch, DateTimeToIso8601Text(Default), Prompt);
  if res = '0' then
  begin
    result := 0;
    exit;
  end;
  result := Iso8601ToDateTime(res);
  if result = 0 then
    result := Default;
end;

function TCommandLine.AsEnum(const Switch, Default: RawUtf8; TypeInfo: pointer;
  const Prompt: string): integer;
var
  res: RawUtf8;
begin
  res := AsUtf8(Switch, Default, Prompt);
  if not ToInteger(res, result) then
    result := GetEnumNameValue(TypeInfo, pointer(res), length(res), true);
end;

function TCommandLine.AsArray: TRawUtf8DynArray;
begin
  fValues.ToRawUtf8DynArray(result);
end;

function TCommandLine.AsJson(Format: TTextWriterJSONFormat): RawUtf8;
begin
  result := fValues.ToJson('', '', Format);
end;

function TCommandLine.AsString(const Switch: RawUtf8;
  const Default, Prompt: string): string;
begin
  result := Utf8ToString(AsUtf8(Switch, StringToUtf8(Default), Prompt));
end;


end.

