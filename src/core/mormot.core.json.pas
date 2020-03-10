/// Framework Core Low-Level JSON Processing
// - this unit is a part of the freeware Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.json;

{
  *****************************************************************************

   JSON functions shared by all framework units
    -

  *****************************************************************************
}


(*

/// convert UTF-8 content into a JSON string
// - with proper escaping of the content, and surounding " characters
procedure QuotedStrJSON(const aText: RawUTF8; var result: RawUTF8;
  const aPrefix: RawUTF8 = ''; const aSuffix: RawUTF8 = ''); overload;
  {$ifdef HASINLINE} inline; {$endif}

/// convert UTF-8 buffer into a JSON string
// - with proper escaping of the content, and surounding " characters
procedure QuotedStrJSON(P: PUTF8Char; PLen: PtrInt; var result: RawUTF8;
  const aPrefix: RawUTF8 = ''; const aSuffix: RawUTF8 = ''); overload;

/// convert UTF-8 content into a JSON string
// - with proper escaping of the content, and surounding " characters
function QuotedStrJSON(const aText: RawUTF8): RawUTF8; overload;
  {$ifdef HASINLINE} inline; {$endif}

procedure QuotedStrJSON(P: PUTF8Char; PLen: PtrInt; var result: RawUTF8;
  const aPrefix, aSuffix: RawUTF8);
var
  temp: TTextWriterStackBuffer;
  Lp, Ls: PtrInt;
  D: PUTF8Char;
begin
  if (pointer(result) = pointer(P)) or NeedsJsonEscape(P) then
    with TTextWriter.CreateOwnedStream(temp) do
    try
      AddString(aPrefix);
      Add('"');
      AddJSONEscape(P, PLen);
      Add('"');
      AddString(aSuffix);
      SetText(result);
      exit;
    finally
      Free;
    end
  else
  begin
    Lp := length(aPrefix);
    Ls := length(aSuffix);
    FastSetString(result, nil, PLen + Lp + Ls + 2);
    D := pointer(result); // we checked dest result <> source P above
    if Lp > 0 then
    begin
      MoveSmall(pointer(aPrefix), D, Lp);
      inc(D, Lp);
    end;
    D^ := '"';
    MoveFast(P^, D[1], PLen);
    inc(D, PLen);
    D[1] := '"';
    if Ls > 0 then
      MoveSmall(pointer(aSuffix), D + 2, Ls);
  end;
end;

procedure QuotedStrJSON(const aText: RawUTF8; var result: RawUTF8; const aPrefix, aSuffix: RawUTF8);
begin
  QuotedStrJSON(pointer(aText), Length(aText), result, aPrefix, aSuffix);
end;

function QuotedStrJSON(const aText: RawUTF8): RawUTF8;
begin
  QuotedStrJSON(pointer(aText), Length(aText), result, '', '');
end;

*)

interface

{$I ..\mormot.defines.inc}

uses
  Classes, Contnrs, Types, SysUtils, mormot.core.base;

implementation

end.






