{:
---------------------------------------------------(C) martindoyle 2017-2026 --
 Project : mdComponents

  Module : mdDates.pas

  Last modified
    Date : 07.02.2026
    Author : Martin Doyle
    Email : martin-doyle@online.de

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to
    deal in the Software without restriction, including without limitation the
    rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
    sell copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in
    all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
    FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
    IN THE SOFTWARE.
--------------------------------------------------------------------------------
}
unit mdDates;

interface

{ Date formatting helpers - use system locale, never hardcoded formats }
function AppDateToStr(const ADate: TDateTime): string;
function AppTryStrToDate(const AText: string; out ADate: TDateTime): Boolean;
function AppDateFormatHint: string;

implementation

uses
  SysUtils;

function AppDateToStr(const ADate: TDateTime): string;
begin
  Result := SysUtils.DateToStr(ADate);
end;

function AppTryStrToDate(const AText: string; out ADate: TDateTime): Boolean;
begin
  Result := SysUtils.TryStrToDate(Trim(AText), ADate);
end;

function AppDateFormatHint: string;
begin
  Result := {$IFDEF FPC}FormatSettings.{$ENDIF}ShortDateFormat;
end;

end.
