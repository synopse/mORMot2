{:
———————————————————————————————————————————————— © martindoyle 2017-2025 ——
 Project : mdComponents

  Module : mdSystemProcs.pas

  Last modified
    Date : 17.08.2024
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
————————————————————————————————————————————————————————————————————————————
}
unit mdSystemProcs;

interface
{$IFDEF MSWINDOWS} uses Windows;   {$ENDIF}
{$IFDEF UNIX} uses Windows;   {$ENDIF}

function GetMenuFont: HFONT;
function GetStatusFont: HFONT;
function GetMessageFont: HFONT;
procedure ReleaseFont(AFont: HFONT);
function GetControlHeight(AFont: HFont; IsStdControl: Boolean): Integer;
function GetScrollBarWidth: Integer;
function GetAppDataPath : string;
{function GetCommonAppDataPath : string;
function GetCommonDocumentsPath : string;}
function GetPersonalPath : string;
function GetProgramFilesPath : string;
{function GetProgramFilesCommonPath : string;
function GetSystemPath : string;
function GetWindowsPath : string;}

implementation
uses SHFolder;

function GetMenuFont: HFONT;
var
  NonClientMetrics: TNonClientMetrics;
begin
  NonClientMetrics.cbSize := sizeof(NonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    Result := CreateFontIndirect(NonClientMetrics.lfMenuFont)
  else
    Result := GetStockObject(SYSTEM_FONT);
end;

function GetStatusFont: HFONT;
var
  NonClientMetrics: TNonClientMetrics;
begin
  NonClientMetrics.cbSize := sizeof(NonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    Result := CreateFontIndirect(NonClientMetrics.lfStatusFont)
  else
    Result := GetStockObject(SYSTEM_FONT);
end;

function GetMessageFont: HFONT;
var
  NonClientMetrics: TNonClientMetrics;
begin
  NonClientMetrics.cbSize := sizeof(NonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    Result := CreateFontIndirect(NonClientMetrics.lfMessageFont)
  else
    Result := GetStockObject(SYSTEM_FONT);
end;

procedure ReleaseFont(AFont: HFONT);
begin
  if AFont <> 0 then
    DeleteObject(AFont);
end;

function GetControlHeight(AFont: HFont; IsStdControl: Boolean): Integer;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, AFont);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  if IsStdControl then
    I:=6
  else
    I:=8;
  I := GetSystemMetrics(SM_CYBORDER) * I;
  GetControlHeight := Metrics.tmHeight + I;
end;

function GetScrollbarWidth: Integer;
begin
  GetScrollbarWidth:= GetSystemMetrics(SM_CYHSCROLL);
end;

function GetSystemFolder(Folder : integer) : string;
var
  PP : array[1..1024] of Char;
  PPP : PChar;
begin
  SHGetFolderPath(0, Folder, 0, 0, @PP);
  PPP := @PP[1];
  GetSystemFolder := PPP;
end;

function GetAppDataPath : string;
begin
  GetAppDataPath := GetSystemFolder(CSIDL_APPDATA);
end;

function GetCommonAppDataPath : string;
begin
  GetCommonAppDataPath := GetSystemFolder(CSIDL_COMMON_APPDATA);
end;

function GetCommonDocumentsPath : string;
begin
  GetCommonDocumentsPath := GetSystemFolder(CSIDL_COMMON_DOCUMENTS);
end;

function GetPersonalPath : string;
begin
  GetPersonalPath := GetSystemFolder(CSIDL_PERSONAL);
end;

function GetProgramFilesPath : string;
begin
  GetProgramFilesPath := GetSystemFolder(CSIDL_PROGRAM_FILES);
end;

function GetProgramFilesCommonPath : string;
begin
  GetProgramFilesCommonPath := GetSystemFolder(CSIDL_PROGRAM_FILES_COMMON);
end;
function GetSystemPath : string;
begin
  GetSystemPath := GetSystemFolder(CSIDL_SYSTEM);
end;

function GetWindowsPath : string;
begin
  GetWindowsPath := GetSystemFolder(CSIDL_WINDOWS);
end;

end.

