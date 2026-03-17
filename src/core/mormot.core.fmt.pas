/// Framework Core High-Level Content Processing
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.fmt;

{
  *****************************************************************************

   Binary, JSON and Text Advanced Formatting Functions
    - Markup (e.g. HTML or Emoji) process
    - JSON and Text Preprocessor

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


{ ************* Markup (e.g. HTML or Emoji) process }

type
  /// tune AddHtmlEscapeWiki/AddHtmlEscapeMarkdown wrapper functions process
  // - heHtmlEscape will escape any HTML special chars, e.g. & into &amp;
  // - heEmojiToUtf8 will convert any Emoji text into UTF-8 Unicode character,
  // recognizing e.g. :joy: or :) in the text
  TTextWriterHtmlEscape = set of (
    heHtmlEscape,
    heEmojiToUtf8);

/// convert some wiki-like text into proper HTML
// - convert all #13#10 into <p>...</p>, *..* into <em>..</em>, +..+ into
// <strong>..</strong>, `..` into <code>..</code>, and http://... as
// <a href=http://...>
// - escape any HTML special chars, and Emoji tags as specified with esc
procedure AddHtmlEscapeWiki(W: TTextWriter; P: PUtf8Char;
  esc: TTextWriterHtmlEscape = [heHtmlEscape, heEmojiToUtf8]);

/// convert minimal Markdown text into proper HTML
// - see https://enterprise.github.com/downloads/en/markdown-cheatsheet.pdf
// - convert all #13#10 into <p>...</p>, *..* into <em>..</em>, **..** into
// <strong>..</strong>, `...` into <code>...</code>, backslash espaces \\
// \* \_ and so on, [title](http://...) and detect plain http:// as
// <a href=...>
// - create unordered lists from trailing * + - chars, blockquotes from
// trailing > char, and code line from 4 initial spaces
// - as with default Markdown, won't escape HTML special chars (i.e. you can
// write plain HTML in the supplied text) unless esc is set otherwise
// - only inline-style links and images are supported yet (not reference-style);
// tables aren't supported either
procedure AddHtmlEscapeMarkdown(W: TTextWriter; P: PUtf8Char;
  esc: TTextWriterHtmlEscape = [heEmojiToUtf8]);

/// escape some wiki-marked text into HTML
// - just a wrapper around AddHtmlEscapeWiki() process
function HtmlEscapeWiki(const wiki: RawUtf8;
  esc: TTextWriterHtmlEscape = [heHtmlEscape, heEmojiToUtf8]): RawUtf8;

/// escape some Markdown-marked text into HTML
// - just a wrapper around AddHtmlEscapeMarkdown() process
function HtmlEscapeMarkdown(const md: RawUtf8;
  esc: TTextWriterHtmlEscape = [heEmojiToUtf8]): RawUtf8;

type
  /// map the first Unicode page of Emojis, from U+1F600 to U+1F64F
  // - naming comes from github/Markdown :identifiers:
  TEmoji = (
    eNone,
    eGrinning,
    eGrin,
    eJoy,
    eSmiley,
    eSmile,
    eSweat_smile,
    eLaughing,
    eInnocent,
    eSmiling_imp,
    eWink,
    eBlush,
    eYum,
    eRelieved,
    eHeart_eyes,
    eSunglasses,
    eSmirk,
    eNeutral_face,
    eExpressionless,
    eUnamused,
    eSweat,
    ePensive,
    eConfused,
    eConfounded,
    eKissing,
    eKissing_heart,
    eKissing_smiling_eyes,
    eKissing_closed_eyes,
    eStuck_out_tongue,
    eStuck_out_tongue_winking_eye,
    eStuck_out_tongue_closed_eyes,
    eDisappointed,
    eWorried,
    eAngry,
    ePout,
    eCry,
    ePersevere,
    eTriumph,
    eDisappointed_relieved,
    eFrowning,
    eAnguished,
    eFearful,
    eWeary,
    eSleepy,
    eTired_face,
    eGrimacing,
    eSob,
    eOpen_mouth,
    eHushed,
    eCold_sweat,
    eScream,
    eAstonished,
    eFlushed,
    eSleeping,
    eDizzy_face,
    eNo_mouth,
    eMask,
    eSmile_cat,
    eJoy_cat,
    eSmiley_cat,
    eHeart_eyes_cat,
    eSmirk_cat,
    eKissing_cat,
    ePouting_cat,
    eCrying_cat_face,
    eScream_cat,
    eSlightly_frowning_face,
    eSlightly_smiling_face,
    eUpside_down_face,
    eRoll_eyes,
    eNo_good,
    oOk_woman,
    eBow,
    eSee_no_evil,
    eHear_no_evil,
    eSpeak_no_evil,
    eRaising_hand,
    eRaised_hands,
    eFrowning_woman,
    ePerson_with_pouting_face,
    ePray);

var
  /// github/Markdown compatible text of Emojis
  // - e.g. 'grinning' or 'person_with_pouting_face'
  EMOJI_TEXT: array[TEmoji] of RawUtf8;

  /// github/Markdown compatible tag of Emojis, including trailing and ending :
  // - e.g. ':grinning:' or ':person_with_pouting_face:'
  EMOJI_TAG: array[TEmoji] of RawUtf8;

  /// the Unicode character matching a given Emoji, after UTF-8 encoding
  EMOJI_UTF8: array[TEmoji] of RawUtf8;

  /// low-level access to TEmoji RTTI - used when inlining EmojiFromText()
  EMOJI_RTTI: PShortString;

  /// to recognize simple :) :( :| :/ :D :o :p :s characters as smilleys
  EMOJI_AFTERDOTS: array['('..'|'] of TEmoji;

/// recognize github/Markdown compatible text of Emojis
// - for instance 'sunglasses' text buffer will return eSunglasses
// - returns eNone if no case-insensitive match was found
function EmojiFromText(P: PUtf8Char; len: PtrInt): TEmoji;
  {$ifdef HASINLINE}inline;{$endif}

/// low-level parser of github/Markdown compatible text of Emojis
// - supplied P^ should point to ':'
// - will append the recognized UTF-8 Emoji if P contains e.g. :joy: or :)
// - will append ':' if no Emoji text is recognized, and return eNone
// - will try both EMOJI_AFTERDOTS[] and EMOJI_RTTI[] reference set
// - if W is nil, won't append anything, but just return the recognized TEmoji
function EmojiParseDots(var P: PUtf8Char; W: TTextWriter = nil): TEmoji;

/// low-level conversion of UTF-8 Emoji sequences into github/Markdown :identifiers:
procedure EmojiToDots(P: PUtf8Char; W: TTextWriter); overload;

/// conversion of UTF-8 Emoji sequences into github/Markdown :identifiers:
function EmojiToDots(const text: RawUtf8): RawUtf8; overload;

/// low-level conversion of github/Markdown :identifiers: into UTF-8 Emoji sequences
procedure EmojiFromDots(P: PUtf8Char; W: TTextWriter); overload;

/// conversion of github/Markdown :identifiers: into UTF-8 Emoji sequences
function EmojiFromDots(const text: RawUtf8): RawUtf8; overload;


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

{ ************* Markup (e.g. HTML or Emoji) process }

{ internal TTextWriterEscape class }

type
  TTextWriterEscapeStyle = (
    tweBold,
    tweItalic,
    tweCode);

  TTextWriterEscapeLineStyle = (
    twlNone,
    twlParagraph,
    twlOrderedList,
    twlUnorderedList,
    twlBlockquote,
    twlCode4,
    twlCode3);

  {$ifdef USERECORDWITHMETHODS}
  TTextWriterEscape = record
  {$else}
  TTextWriterEscape = object
  {$endif USERECORDWITHMETHODS}
  public
    P, B, P2, B2: PUtf8Char;
    W: TTextWriter;
    st: set of TTextWriterEscapeStyle;
    fmt: TTextWriterHtmlFormat;
    esc: TTextWriterHtmlEscape;
    lst: TTextWriterEscapeLineStyle;
    procedure Start(dest: TTextWriter; src: PUtf8Char; escape: TTextWriterHtmlEscape);
    function ProcessText(const stopchars: TSynByteSet): AnsiChar;
    procedure ProcessHRef;
    function ProcessLink: boolean;
    procedure ProcessEmoji;
      {$ifdef HASINLINE}inline;{$endif}
    procedure Toggle(style: TTextWriterEscapeStyle);
    procedure SetLine(style: TTextWriterEscapeLineStyle);
    procedure EndOfParagraph;
    procedure NewMarkdownLine;
    procedure AddHtmlEscapeWiki(dest: TTextWriter; src: PUtf8Char;
      escape: TTextWriterHtmlEscape);
    procedure AddHtmlEscapeMarkdown(dest: TTextWriter; src: PUtf8Char;
      escape: TTextWriterHtmlEscape);
  end;

procedure TTextWriterEscape.Start(dest: TTextWriter; src: PUtf8Char;
  escape: TTextWriterHtmlEscape);
begin
  P := src;
  W := dest;
  st := [];
  if heHtmlEscape in escape then
    fmt := hfOutsideAttributes
  else
    fmt := hfNone;
  esc := escape;
  lst := twlNone;
end;

function IsHttpOrHttps(P: PUtf8Char): boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
  result := (PCardinal(P)^ = HTTP__32) and
            ((PCardinal(P + 4)^ and $ffffff = HTTP__24) or
             (PCardinal(P + 4)^ =
             ord('s') + ord(':') shl 8 + ord('/') shl 16 + ord('/') shl 24));
end;

function TTextWriterEscape.ProcessText(const stopchars: TSynByteSet): AnsiChar;
begin
  if P = nil then
  begin
    result := #0;
    exit;
  end;
  B := P;
  while not (ord(P^) in stopchars) and
        not IsHttpOrHttps(P) do
    inc(P);
  W.AddHtmlEscape(B, P - B, fmt);
  result := P^;
end;

procedure TTextWriterEscape.ProcessHRef;
begin
  B := P;
  while P^ > ' ' do
    inc(P);
  W.AddShort('<a href="');
  W.AddHtmlEscape(B, P - B, hfWithinAttributes);
  W.AddShort('" rel="nofollow">');
  W.AddHtmlEscape(B, P - B);
  W.AddDirect('<', '/', 'a', '>');
end;

function TTextWriterEscape.ProcessLink: boolean;
begin
  inc(P);
  B2 := P;
  while not (P^ in [#0, ']']) do
    inc(P);
  P2 := P;
  if PWord(P)^ = ord(']') + ord('(') shl 8 then
  begin
    inc(P, 2);
    B := P;
    while not (P^ in [#0, ')']) do
      inc(P);
    if P^ = ')' then
    begin
      // [GitHub](https://github.com)
      result := true;
      exit;
    end;
  end;
  P := B2; // rollback
  result := false;
end;

procedure TTextWriterEscape.ProcessEmoji;
begin
  if heEmojiToUtf8 in esc then
    EmojiParseDots(P, W)
  else
  begin
    W.Add(':');
    inc(P);
  end;
end;

procedure TTextWriterEscape.Toggle(style: TTextWriterEscapeStyle);
const
  HTML: array[tweBold..tweCode] of TShort7 = (
    'strong>', 'em>', 'code>');
begin
  W.Add('<');
  if style in st then
  begin
    W.Add('/');
    exclude(st, style);
  end
  else
    include(st, style);
  W.AddShorter(HTML[style]);
end;

procedure TTextWriterEscape.EndOfParagraph;
begin
  if tweBold in st then
    Toggle(tweBold);
  if tweItalic in st then
    Toggle(tweItalic);
  if P <> nil then
    if PWord(P)^ = EOLW then
      inc(P, 2)
    else
      inc(P);
end;

procedure TTextWriterEscape.SetLine(style: TTextWriterEscapeLineStyle);
const
  HTML: array[twlParagraph..twlCode3] of TShort7 = (
    'p>', 'li>', 'li>', 'p>', 'code>', 'code>');
  HTML2: array[twlOrderedList..twlCode3] of TShort15 = (
    'ol>', 'ul>', 'blockquote>', 'pre>', 'pre>');
begin
  if lst >= low(HTML) then
  begin
    if (lst < twlCode4) or
       (lst <> style) then
    begin
      W.Add('<', '/');
      W.AddShorter(HTML[lst]);
    end;
    if (lst >= low(HTML2)) and
       (lst <> style) then
    begin
      W.Add('<', '/');
      W.AddShort(HTML2[lst]);
    end;
  end;
  if style >= low(HTML) then
  begin
    if (style >= low(HTML2)) and
       (lst <> style) then
    begin
      W.Add('<');
      W.AddShort(HTML2[style]);
    end;
    if (style < twlCode4) or
       (lst <> style) then
    begin
      W.Add('<');
      W.AddShorter(HTML[style]);
    end;
  end;
  lst := style;
end;

procedure TTextWriterEscape.NewMarkdownLine;
label
  none;
var
  c: cardinal;
begin
  if P = nil then
    exit;
  c := PCardinal(P)^;
  if c and $ffffff = ord('`') + ord('`') shl 8 + ord('`') shl 16 then
  begin
    inc(P, 3);
    if lst = twlCode3 then
    begin
      lst := twlCode4; // to close </code></pre>
      NewMarkdownLine;
      exit;
    end;
    SetLine(twlCode3);
  end;
  if lst = twlCode3 then
    exit; // no prefix process within ``` code blocks
  if c = $20202020 then
  begin
    SetLine(twlCode4);
    inc(P, 4);
    exit;
  end;
  P := GotoNextNotSpaceSameLine(P); // don't implement nested levels yet
  case P^ of
    '*',
    '+',
    '-':
      if P[1] = ' ' then
        SetLine(twlUnorderedList)
      else
        goto none;
    '1'..'9':
      begin
        // first should be 1. then any ##. number to continue
        B := P;
        repeat
          inc(P)
        until not (P^ in ['0'..'9']);
        if (P^ = '.') and
           ((lst = twlOrderedList) or
            (PWord(B)^ = ord('1') + ord('.') shl 8)) then
          SetLine(twlOrderedList)
        else
        begin
          P := B;
none:     if lst = twlParagraph then
          begin
            c := PWord(P)^; // detect blank line to separate paragraphs
            if c = EOLW then
              inc(P, 2)
            else if c and $ff = $0a then
              inc(P)
            else
            begin
              W.AddOnce(' ');
              exit;
            end;
          end;
          SetLine(twlParagraph);
          exit;
        end;
      end;
    '>':
      if P[1] = ' ' then
        SetLine(twlBlockquote)
      else
        goto none;
  else
    goto none;
  end;
  P := GotoNextNotSpaceSameLine(P + 1);
end;

procedure TTextWriterEscape.AddHtmlEscapeWiki(dest: TTextWriter;
  src: PUtf8Char; escape: TTextWriterHtmlEscape);
begin
  Start(dest, src, escape);
  SetLine(twlParagraph);
  repeat
    case ProcessText([0, 10, 13,
                      ord('*'), ord('+'), ord('`'), ord('\'), ord(':')]) of
      #0:
        break;
      #10,
      #13:
        begin
          EndOfParagraph;
          SetLine(twlParagraph);
          continue;
        end;
      '\':
        if P[1] in ['\', '`', '*', '+'] then
        begin
          inc(P);
          W.Add(P^);
        end
        else
          W.Add('\');
      '*':
        Toggle(tweItalic);
      '+':
        Toggle(tweBold);
      '`':
        Toggle(tweCode);
      'h':
        begin
          ProcessHRef;
          continue;
        end;
      ':':
        begin
          ProcessEmoji;
          continue;
        end;
    end;
    inc(P);
  until false;
  EndOfParagraph;
  SetLine(twlNone);
end;

procedure TTextWriterEscape.AddHtmlEscapeMarkdown(dest: TTextWriter;
  src: PUtf8Char; escape: TTextWriterHtmlEscape);
begin
  Start(dest, src, escape);
  NewMarkDownLine;
  repeat
    if lst >= twlCode4 then // no Markdown tags within code blocks
      if ProcessText([0, 10, 13]) = #0 then
        break
      else
      begin
        if PWord(P)^ = EOLW then
          inc(P, 2)
        else
          inc(P);
        W.AddCR; // keep LF within <pre>
        NewMarkdownLine;
        continue;
      end
    else
      case ProcessText([0, 10, 13, ord('*'), ord('_'), ord('`'),
                        ord('\'), ord('['), ord('!'), ord(':')]) of
        #0:
          break;
        #10,
        #13:
          begin
            EndOfParagraph;
            NewMarkdownLine;
            continue;
          end;
        '\':
          if P[1] in ['\', '`', '*', '_', '[', ']', '{', '}',
                      '(', ')', '#', '+', '-', '.', '!'] then
          begin
            // backslash escape
            inc(P);
            W.Add(P^);
          end
          else
            W.Add('\');
        '*',
        '_':
          if P[1] = P[0] then
          begin
            // **This text will be bold** or __This text will be bold__
            inc(P);
            Toggle(tweBold);
          end
          else
            // *This text will be italic* or _This text will be italic_
            Toggle(tweItalic);
        '`':
          // `This text will be code`
          Toggle(tweCode);
        '[':
          if ProcessLink then
          begin
            // [GitHub](https://github.com)
            W.AddShort('<a href="');
            W.AddHtmlEscape(B, P - B, hfWithinAttributes);
            if IsHttpOrHttps(B) then
              W.AddShort('" rel="nofollow">')
            else
              W.Add('"', '>');
            W.AddHtmlEscape(B2, P2 - B2, fmt);
            W.AddDirect('<', '/', 'a', '>'); // no inc(P) needed here
          end
          else
            // not a true link -> just append
            W.Add('[');
        '!':
          begin
            if P[1] = '[' then
            begin
              inc(P);
              if ProcessLink then
              begin
                W.AddShort('<img alt="');
                W.AddHtmlEscape(B2, P2 - B2, hfWithinAttributes);
                W.AddShorter('" src="');
                W.AddNoJsonEscape(B, P - B);
                W.AddDirect('"', '>');
                inc(P);
                continue;
              end;
              dec(P);
            end;
            W.Add('!'); // not a true image
          end;
        'h':
          begin
            ProcessHRef;
            continue;
          end;
        ':':
          begin
            ProcessEmoji;
            continue;
          end;
      end;
    inc(P);
  until false;
  EndOfParagraph;
  SetLine(twlNone);
end;

function HtmlEscapeWiki(const wiki: RawUtf8; esc: TTextWriterHtmlEscape): RawUtf8;
var
  temp: TTextWriterStackBuffer; // 8KB work buffer on stack
  W: TTextWriter;
begin
  W := TTextWriter.CreateOwnedStream(temp);
  try
    AddHtmlEscapeWiki(W, pointer(wiki), esc);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

function HtmlEscapeMarkdown(const md: RawUtf8; esc: TTextWriterHtmlEscape): RawUtf8;
var
  temp: TTextWriterStackBuffer;
  W: TTextWriter;
begin
  W := TTextWriter.CreateOwnedStream(temp);
  try
    AddHtmlEscapeMarkdown(W, pointer(md), esc);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

procedure AddHtmlEscapeWiki(W: TTextWriter; P: PUtf8Char; esc: TTextWriterHtmlEscape);
var
  doesc: TTextWriterEscape;
begin
  doesc.AddHtmlEscapeWiki(W, P, esc);
end;

procedure AddHtmlEscapeMarkdown(W: TTextWriter; P: PUtf8Char; esc: TTextWriterHtmlEscape);
var
  doesc: TTextWriterEscape;
begin
  doesc.AddHtmlEscapeMarkdown(W, P, esc);
end;

function EmojiFromText(P: PUtf8Char; len: PtrInt): TEmoji;
begin
  // RTTI has shortstrings in adjacent L1 cache lines -> faster than EMOJI_TEXT[]
  result := TEmoji(FindShortStringListTrimLowerCase(
                     EMOJI_RTTI, ord(high(TEmoji)) - 1, P, len) + 1);
  // note: we may enhance performance by using FastFindPUtf8CharSorted()
end;

function EmojiParseDots(var P: PUtf8Char; W: TTextWriter): TEmoji;
var
  c: PUtf8Char;
begin
  result := eNone;
  inc(P); // ignore trailing ':'
  c := P;
  if c[-2] <= ' ' then
  begin
    if (c[1] <= ' ') and
       (c^ in ['('..'|']) then
      result := EMOJI_AFTERDOTS[c^]; // e.g. :)
    if result = eNone then
    begin
      while c^ in ['a'..'z', 'A'..'Z', '_'] do
        inc(c);
      if (c^ = ':') and
         (c[1] <= ' ') then // try e.g. :joy_cat:
        result := EmojiFromText(P, c - P);
    end;
    if result <> eNone then
    begin
      P := c + 1; // continue parsing after the Emoji text
      if W <> nil then
        W.AddShort(pointer(EMOJI_UTF8[result]), 4);
      exit;
    end;
  end;
  if W <> nil then
    W.Add(':');
end;

procedure EmojiToDots(P: PUtf8Char; W: TTextWriter);
var
  B: PUtf8Char;
  c: cardinal;
begin
  if (P <> nil) and
     (W <> nil) then
    repeat
      B := P;
      while (P^ <> #0) and
            (PWord(P)^ <> $9ff0) do
        inc(P);
      W.AddNoJsonEscape(B, P - B);
      if P^ = #0 then
        break;
      B := P;
      c := NextUtf8Ucs4(P) - $1f5ff;
      if c <= cardinal(high(TEmoji)) then
        W.AddString(EMOJI_TAG[TEmoji(c)])
      else
        W.AddNoJsonEscape(B, P - B);
    until P^ = #0;
end;

function EmojiToDots(const text: RawUtf8): RawUtf8;
var
  W: TTextWriter;
  tmp: TTextWriterStackBuffer;
begin
  if PosExChar(#$f0, text) = 0 then
  begin
    result := text; // no UTF-8 smiley for sure
    exit;
  end;
  W := TTextWriter.CreateOwnedStream(tmp);
  try
    EmojiToDots(pointer(text), W);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

procedure EmojiFromDots(P: PUtf8Char; W: TTextWriter);
var
  B: PUtf8Char;
begin
  if (P <> nil) and
     (W <> nil) then
    repeat
      B := P;
      while not (P^ in [#0, ':']) do
        inc(P);
      W.AddNoJsonEscape(B, P - B);
      if P^ = #0 then
        break;
      EmojiParseDots(P, W);
    until P^ = #0;
end;

function EmojiFromDots(const text: RawUtf8): RawUtf8;
var
  W: TTextWriter;
  tmp: TTextWriterStackBuffer;
begin
  W := TTextWriter.CreateOwnedStream(tmp);
  try
    EmojiFromDots(pointer(text), W);
    W.SetText(result);
  finally
    W.Free;
  end;
end;


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


procedure InitializeUnit;
var
  e: TEmoji;
begin
  // HTML/Emoji Efficient Parsing
  Assert(ord(high(TEmoji)) = $4f + 1);
  EMOJI_RTTI := GetEnumName(TypeInfo(TEmoji), 1); // ignore eNone=0
  GetEnumTrimmedNames(TypeInfo(TEmoji), @EMOJI_TEXT, scLowerCase);
  FastAssignNew(EMOJI_TEXT[eNone]);
  for e := succ(low(e)) to high(e) do
  begin
    Join([':', EMOJI_TEXT[e], ':'], EMOJI_TAG[e]);
    // order matches U+1F600 to U+1F64F codepoints
    Ucs4ToUtf8(ord(e) + $1f5ff, FastSetString(EMOJI_UTF8[e], 4));
  end;
  EMOJI_AFTERDOTS[')'] := eSmiley;
  EMOJI_AFTERDOTS['('] := eFrowning;
  EMOJI_AFTERDOTS['|'] := eExpressionless;
  EMOJI_AFTERDOTS['/'] := eConfused;
  EMOJI_AFTERDOTS['D'] := eLaughing;
  EMOJI_AFTERDOTS['o'] := eOpen_mouth;
  EMOJI_AFTERDOTS['O'] := eOpen_mouth;
  EMOJI_AFTERDOTS['p'] := eYum;
  EMOJI_AFTERDOTS['P'] := eYum;
  EMOJI_AFTERDOTS['s'] := eScream;
  EMOJI_AFTERDOTS['S'] := eScream;
end;


initialization
  InitializeUnit;

end.
