/// Framework Core High-Level JSON and Text Processing
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.fmt;

{
  *****************************************************************************

   JSON and Text Advanced Formatting Functions
    -  JSON and Text Preprocessor

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  classes,
  contnrs,
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.core.buffers,
  mormot.core.data,
  mormot.core.json;

{ ********** JSON and Text Preprocessor }

type
  /// tune JsonPreprocess() pre-processor features
  // - jppIncludeAbsolute enables unsafe "include <file>" outside the root folder
  // - jppDebugComment will emit verbose debugging comments during pre-processing
  TPreprocFlags = set of (
    jppIncludeAbsolute,
    jppDebugComment);

/// pre-process and format JSON with $".." and $(ident) expansion
function JsonPreprocess(const Json: RawUtf8;
  Format: TTextWriterJsonFormat = jsonHumanReadable;
  Flags: TPreprocFlags = []; const IncludeRoot: TFileName = ''): RawUtf8;

/// pre-process and format JSON with $".." and $(ident) expansion into a file
function JsonPreprocessToFile(const Json: RawUtf8; const Dest: TFileName;
  Format: TTextWriterJsonFormat = jsonHumanReadable;
  Flags: TPreprocFlags = []; const IncludeRoot: TFileName = ''): boolean;


implementation


{ ********** JSON and Text Preprocessor }

const
  DSL_INCLUDE_DEPTH = 4; // avoid infinite include <filename>

type
  TPreprocIfLevel = 0 .. 15; // 0=outer level, 1..15=nested levels
  TPreprocIf = (piNone, piIf, piIfDef, piElse, piEnd);

  /// implement pre-processing to JSON or text input
  TPreproc = class(TPreprocAbstract)
  protected
    Vars: TBinDictionary;
    IfLevel: TPreprocIfLevel;
    IfSkip: set of TPreprocIfLevel;
    IncludeDepth: byte;
    Options: TPreprocFlags;
    IncludeFolder: TFileName;
    procedure DoSkip(var P: PUtf8Char);
    function DoFind(Key: pointer; KeyLen: PtrInt; var ValueLen: PtrInt): pointer;
    function DoIf(P: PUtf8Char): PUtf8Char;
    procedure DoEndif(var P: PUtf8Char);
      {$ifdef HASINLINE} inline; {$endif}
    procedure DoRegister(m: TPreprocMarker; k, v, ve: PUtf8Char; kl: PtrInt);
    procedure DoInclude(P: PUtf8Char; const Append: TOnPreprocAppend);
  public
    /// initialize this pre-processor engine
    constructor Create(flags: TPreprocFlags; const folder: TFileName); reintroduce;
    /// finalize this engine and all transient variables
    destructor Destroy; override;
    /// called with P^=$$ to integrate DSL sections
    function ParseSection(P: PUtf8Char): PUtf8Char; override;
    /// called with P^=$ for conditional process
    function WasIf(var P: PUtf8Char): boolean; override;
    /// called with P^=$ to recognize $(ident) into Value/Len
    function Expand(P: PUtf8Char; var Value: PUtf8Char; var Len: PtrInt;
      KeepMarker: boolean): PUtf8Char; override;
    /// called with P^=$ to recognize $(ident) and append as plain unescaped text
    function ExpandTo(P: PUtf8Char; W: TTextWriter): PUtf8Char; override;
  end;

constructor TPreproc.Create(flags: TPreprocFlags; const folder: TFileName);
begin
  inherited Create;
  Options := flags;
  if DirectoryExists(folder) then
    IncludeFolder := IncludeTrailingPathDelimiter(folder);
  Vars := TBinDictionary.Create;
end;

destructor TPreproc.Destroy;
begin
  inherited Destroy;
  Vars.Free;
end;

function TPreproc.DoFind(Key: pointer; KeyLen: PtrInt; var ValueLen: PtrInt): pointer;
begin
  result := Vars.Find(Key, KeyLen, @ValueLen); // from known variables/templates
  if result = nil then
    result := GlobalInfoFind(Key, KeyLen, ValueLen); // global macros
end;

function TPreproc.Expand(P: PUtf8Char; var Value: PUtf8Char; var Len: PtrInt;
  KeepMarker: boolean): PUtf8Char;
var
  key: PUtf8Char;
  keylen: PtrInt;
  ending: AnsiChar;
begin
  result := P;
  inc(result); // called with P^ = '$'
  ending := '$';
  if result^ in ['(', '{'] then // $(ident) ${ident} format, not $ident$
  begin
    if result^ = '(' then
      ending := ')'
    else
      ending := '}';
    inc(result);
  end;
  key := result;
  while (result^ <> ending) and not (result^ in [#0 .. ' ', '|']) do
    inc(result);
  Value := nil;
  if result^ <= ' ' then
    exit;
  Value := DoFind(key, result - key, Len); // resolve
  if result^ = '|' then   // $(ident|default) or $ident|default$
  begin
    inc(result);
    if result^ = '$' then // $ident|$default$$ or $(ident|$(def1)|$(def2))}
    begin
      result := Expand(result, key, keylen, KeepMarker); // cascaded defaults
      if Value = nil then // fallback to the nested $default$
      begin
        Value := key;
        Len := keylen;
      end;
      while (result^ <> ending) and (result^ in [#0 .. #31]) do
        inc(result);
      inc(result); // skip trailing $ or }
      exit;
    end;
    key := result;
    while (result^ <> ending) and not (result^ in [#0 .. #31, '|']) do
      inc(result);
    if result^ < ' ' then
      exit;
    if Value = nil then // fallback to the specified default
    begin
      Value := key;
      Len := result - key;
    end;
  end;
  inc(result); // skip trailing $ } )
  if (Value = nil) or
     KeepMarker or
     (TPreprocMarker(Value^) > high(TPreprocMarker)) then
    exit;
  inc(Value); // trim marker
  dec(Len);
end;

function TPreproc.ExpandTo(P: PUtf8Char; W: TTextWriter): PUtf8Char;
var
  v: PUtf8Char;
  l: PtrInt;
  m: TPreprocMarker;
begin
  result := Expand(P, v, l, {KeepMarker=}true);
  if v = nil then // this $(ident) was not found
    exit;
  m := TPreprocMarker(v^);
  if m = pmTemplate then
    exit;// { } [] not expanded in "string"
  if m <= high(m) then // skip marker
  begin
    inc(v);
    dec(l);
  end;
  W.AddShort(v, l);
end;

procedure TPreproc.DoEndif(var P: PUtf8Char);
begin
  if IfLevel <> 0 then
  begin
    exclude(IfSkip, IfLevel); // cleanup for debugging
    dec(IfLevel);
    if (IfLevel <> 0) and
       (IfLevel in IfSkip) then
      DoSkip(P); // continue skip as in DoIf()
  end
  else if Assigned(OnAddDebugComment) then
    OnAddDebugComment('$endif$ with no prior $if')
end;

function ParseIfDollar(var P: PUtf8Char): TPreprocIf; // caller ensured P^='$'
begin
  result := piNone;
  case PCardinal(P)^ of
    ord('$') + ord('i') shl 8 + ord('f') shl 16 + ord(' ') shl 24:  // '$if '
      begin
        result := piIf;
        inc(P, 4);
      end;
    ord('$') + ord('i') shl 8 + ord('f') shl 16 + ord('d') shl 24:
      if PCardinal(P + 4)^ and $ffffff =
           ord('e') + ord('f') shl 8 + ord(' ') shl 16 then         // '$ifdef '
      begin
        inc(P, 7);
        result := piIfDef;
      end;
    ord('$') + ord('e') shl 8 + ord('l') shl 16 + ord('s') shl 24:
      if cardinal(PWord(P + 4)^) = ord('e') + ord('$') shl 8 then   // '$else$'
      begin
        inc(P, 6);
        result := piElse;
      end;
    ord('$') + ord('e') shl 8 + ord('n') shl 16 + ord('d') shl 24:
      if PCardinal(P + 4)^ and $ffffff =
           ord('i') + ord('f') shl 8 + ord('$') shl 16 then         // '$endif$'
      begin
        inc(P, 7);
        result := piEnd;
      end;
  end;
end;

procedure TPreproc.DoSkip(var P: PUtf8Char);
begin
  while true do
    if P^ = #0 then
      exit
    else if P^ <> '$' then
      inc(P) // most common case
    else
      case ParseIfDollar(P) of
        piNone:
          inc(P);
        piIf:
          begin
            dec(P, 4); // caller should detect and execute DslIf()
            exit;
          end;
        piIfDef:
          begin
            dec(P, 7);
            exit;
          end;
        piEnd:
          begin
            DoEndif(P);
            exit;
          end;
        piElse:
          if IfLevel = 0 then // paranoid
            exit
          else if IfLevel in IfSkip then
          begin
            exclude(IfSkip, IfLevel); // include until $end$
            exit;
          end
          else
            include(IfSkip, IfLevel);  // and continue skip loop
      end;
end;

function TPreproc.DoIf(P: PUtf8Char): PUtf8Char;
var
  exp: TTextExpression;
  len, level: PtrInt;
  match: boolean;
begin // P^ = 'id$' or 'id = val$' from '$ifdef id$' or '$if id = val$'
  result := ParseTextExpression(P, exp, {altstopchar=}'$');
  if result = nil then
  begin
    result := @NULCHAR; // force <> nil but #0
    exit;
  end;
  while (exp.ValueLen > 0) and
        (exp.ValueStart[exp.ValueLen - 1] = ' ') do
    dec(exp.ValueLen);
  if exp.ValueLen = 0 then
    if result^ = '$' then // resolve $val$
    begin
      result := Expand(result, exp.ValueStart, len, {keepmarker=}false);
      if exp.ValueStart <> nil then
        exp.ValueLen := len;
      result := GotoNextNotSpace(result);
      if result^ <> '$' then
        exit;
    end
    else
      exit; // '$if id =$' is invalid syntax (ValueLen=-1 for $ifdef id$)
  if IfLevel < high(IfLevel) then
    if (IfLevel > 0) and
       (IfLevel in IfSkip) then // quickly skip all nested $if$ $endif$
    begin
      level := 0;
      while true do
        if result^ = #0 then
          exit
        else if result^ <> '$' then
          inc(result)
        else
          case ParseIfDollar(result) of
            piNone:
              inc(result);
            piIf, piIfDef:
              inc(level);
            piEnd:
              if level = 0 then
                break
              else
                dec(level);
          end;
      DoSkip(result);
      exit; // no inc(result) after DslSkip()
    end
    else
    begin
      len := 0; // need a PtrInt, not an integer
      exp.NameStart := DoFind(exp.NameStart, exp.NameLen, len);
      exp.NameLen := len;
      if (exp.NameStart <> nil) and
         (TPreprocMarker(exp.NameStart^) <= high(TPreprocMarker)) then
      begin
       inc(exp.NameStart); // trim marker after raw DoFind()
       dec(exp.NameLen);
      end;
      if exp.ValueLen < 0 then
        match := exp.NameStart <> nil // $ifdef id$ just need DoFind() <> nil
      else if (exp.ValueStart = nil) or (exp.ValueLen = 0) then
        match := (exp.NameStart = nil) or (exp.NameLen = 0) // both void
      else
        match := EvaluateTextExpression(exp); // non-void = < > <= >= ~ ~~ * **
      inc(IfLevel);
      if match then // $if$ include [$else$ skip] $endif$
        exclude(IfSkip, IfLevel)
      else
      begin
        include(IfSkip, IfLevel);
        DoSkip(result);
        exit; // no inc(result) after DslSkip()
      end;
    end
  else if Assigned(OnAddDebugComment) then
    OnAddDebugComment('too many nested $if$ (15 allowed)');
  if result^ = '$' then
    inc(result);
end;

function TPreproc.WasIf(var P: PUtf8Char): boolean;
begin
  result := true;
  case ParseIfDollar(P) of // called with P^ = '$'
    piIf, piIfDef: // complex $if $ifdef evaluation
      P := DoIf(P);
    piElse:        // $else$ just toggles skip flag
      if IfLevel > 0 then
        if IfLevel in IfSkip then
          exclude(IfSkip, IfLevel) // include until $end$
        else
        begin
          include(IfSkip, IfLevel); // skip until $end$
          DoSkip(P);
        end
      else if Assigned(OnAddDebugComment) then
        OnAddDebugComment('$else$ with no prior $if');
    piEnd:         // $endif$
      DoEndif(P);
  else
    result := false;
  end;
end;

function GotoTemplateEnding(P: PUtf8Char): PUtf8Char;
var
  level: integer;
begin
  result := P + 1; // skip { [
  level := 0;
  repeat
    case result^ of
      #0:
        exit;
      '{', '[':
        inc(level);
      '}', ']':
        if level = 0 then
          break
        else
          dec(level);
      '"':
        result := GotoEndOfJsonString(result);
    end;
    inc(result);
  until false;
end;

procedure TPreproc.DoRegister(m: TPreprocMarker; k, v, ve: PUtf8Char; kl: PtrInt);
var
  beg, val: PUtf8Char;
  l, vallen: PtrInt; // @vallen = PPtrInt
  comment: PShortString;
  tmp: TSynTempAdder;
begin
  tmp.Init;
  tmp.AddDirect(AnsiChar(m)); // type: #0=template #1="string" #2=const/num
  repeat
    beg := v;
    while (v < ve) and
          (cardinal(PWord(v)^) <> SLASH_16) and
          not (v^ in ['$', '#']) do
      inc(v);
    tmp.Add(beg, v - beg);
    if v >= ve then
      break;
    if v^ = '$' then
      if (m = pmTemplate) and
         WasIf(v) then // $if$ $endif$
        continue
      else // $ident$ ${ident} $env:NAME$ ${env:NAME}
      begin
        v := Expand(v, val, vallen, {KeepMarker=}false);
        if val <> nil then
          tmp.Add(val, vallen);
      end
    else
    begin // # comment or // comment
      l := v - beg;
      while (l <> 0) and
            (v[l - 1] = ' ') do
        dec(l); // trim right
      if l > 0 then
      begin
        tmp.Add(beg, l);
        tmp.AddDirect(#10);
      end;
      v := GotoNextLineSmall(v);
    end;
  until v >= ve;
  Vars.Update(k, tmp.Buffer, kl, tmp.Size);
  if Assigned(OnAddDebugComment) then
  begin
    comment := tmp.Buffer;
    if tmp.Size > 100 then
    begin
      comment^[0] := #100; // truncate to 100 chars seems enough for a comment
      AppendShort('... size=', comment^);
      AppendShortCardinal(tmp.Size, comment^);
    end
    else
      comment^[0] := AnsiChar(tmp.Size - 1); // we can append a small value
    AppendShort(' as $(', comment^);
    AppendShortBuffer(pointer(k), kl, high(comment^), pointer(comment));
    AppendShortCharSafe(')', comment^);
    OnAddDebugComment(comment^);
  end;
  tmp.Store.Done; // free memory - unlikely from heap
end;

function TPreproc.ParseSection(P: PUtf8Char): PUtf8Char;
var
  key, value: PUtf8Char;
  keylen: PtrInt;
  marker: TPreprocMarker;
label
  ok;
begin // handle P = '$$'
  result := P + 2; // allow '$$' or '$$$' or '$$ some text' markers
  repeat
    result := GotoNextLineSmall(result);
ok: result := GotoNextNotSpace(result);
    case result^ of
      #0:
        exit;
      '$':
        if result[1] = '$' then
          break     // end of DSL section
        else if WasIf(result) then
          goto ok   // $if$ $else$ $endif$ conditional logic
        else
          continue; // $ is not allowed in identifiers anyway
      '#', '/':
        continue;   // comment line
      'i', 'I':
        if IdemPChar(result + 1, 'NCLUDE ') then
        begin
          if IncludeFolder <> '' then
            DoInclude(GotoNextNotSpace(result + 8), OnAppend);
          continue; // this is a reserved keyword, never an identifier
        end;
    end;
    key := result;
    repeat
      inc(result);
    until result^ in [#0 .. ' ', '=', ':', '$', '|'];
    keylen := result - key;
    result := GotoNextNotSpace(result);
    if not (result^ in ['=', ':']) then
      continue; // $ and | are not allowed in identifiers
    while result^ in ['=', ':', ' '] do
      inc(result); // allow := or == syntax
    marker := pmEscapedString;
    value := result + 1; // early to make Delphi happy - exclude starting { [ "
    case result^ of
      #0:
        exit;
      '{', '[': // value = whole {..}/[..] text block
        begin
          result := GotoTemplateEnding(result);
          DoRegister(pmTemplate, key, value, result - 1, keylen);
          continue; // has been expanded and processed for $if$
        end;
      '"', '''':
        while (result^ >= ' ') and
              (result^ <> value^) do // quoted value, not supporting \" nor ''
          inc(result);
    else
      begin
        value := result;
        while not (result^ in [#0 .. #31, '#', '/']) do
        begin
          // unquoted value in $$ DSL section = till end of line or comment
          if result^ in ['\', '"'] then
            marker := pmPlainString; // would need W.AddJsonEscape()
          inc(result);
        end;
        while result[-1] = ' ' do
          dec(result); // trim right
        if IsConstantOrNumberJson(value, result - value) then
          marker := pmConstNum;
      end;
    end;
    DoRegister(marker, key, value, result, keylen);
  until false;
  result := GotoNextLineSmall(result); // ignore ending '$$...' marker line
end;

procedure TPreproc.DoInclude(P: PUtf8Char; const Append: TOnPreprocAppend);
var
  tmp: ShortString; // to compute the expanded filename
  v: PUtf8Char;
  vlen: PtrInt;     // @vlen = PPtrInt
  c: AnsiChar;
  txt: RawUtf8;     // UTF-8 file name
  fn: TFileName;    // RTL file name
begin
  tmp[0] := #0;
  if P^ = '"' then
    inc(P);
  while not (P^ in [#0, '"', #13, #10]) do
  begin
    if P^ = '$' then
    begin
      P := Expand(P, v, vlen, {KeepMarker=}false);
      if v <> nil then // include "config/general-$mode$.json"
        AppendShortBuffer(pointer(v), vlen, high(tmp), @tmp);
    end
    else
    begin
      c := P^;
      if c = InvertedPathDelim then
        c := PathDelim; // normalize for direct cross-platform support
      AppendShortCharSafe(c, tmp);
    end;
  end;
  if IncludeDepth >= DSL_INCLUDE_DEPTH then
  begin
    if Assigned(OnAddDebugComment) then
      OnAddDebugComment('include too deep: rejected');
    exit;
  end;
  fn := MakeString([tmp]);
  if not (jppIncludeAbsolute in Options) and
     not SafeFileName(fn) then
    exit;
  txt := RawUtf8FromFile(fn);
  if Assigned(OnAddDebugComment) then
  begin
    if txt = '' then
      AppendShort(' not found: skipped', tmp)
    else
    begin
      AppendShort(' included: size=', tmp);
      AppendShortCardinal(length(txt), tmp);
    end;
    OnAddDebugComment(tmp);
  end;
  if txt = '' then
    exit;
  inc(IncludeDepth);
  Append(pointer(txt));
  dec(IncludeDepth);
end;

function JsonPreprocess(const Json: RawUtf8; Format: TTextWriterJsonFormat;
  Flags: TPreprocFlags; const IncludeRoot: TFileName): RawUtf8;
var
  preproc: TPreproc;
begin
  preproc := TPreProc.Create(Flags, IncludeRoot);
  try
    JsonBufferReformat(pointer(Json), result, Format, preproc);
  finally
    preproc.Free;
  end;
end;

function JsonPreprocessToFile(const Json: RawUtf8; const Dest: TFileName;
  Format: TTextWriterJsonFormat; Flags: TPreprocFlags; const IncludeRoot: TFileName): boolean;
var
  preproc: TPreproc;
begin
  preproc := TPreProc.Create(Flags, IncludeRoot);
  try
    result := JsonBufferReformatToFile(pointer(Json), Dest, Format, preproc);
  finally
    preproc.Free;
  end;
end;



end.
