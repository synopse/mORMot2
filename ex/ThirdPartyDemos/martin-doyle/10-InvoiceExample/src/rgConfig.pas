{:
———————————————————————————————————————————————— © martindoyle 2017-2026 ——
 Project : Rechnung

 Using mORMot2
     Synopse mORMot2 framework. Copyright (C) 2025 Arnaud Bouchez
     Synopse Informatique - http://synopse.info

  Module : rgConfig.pas

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
————————————————————————————————————————————————————————————————————————————
}
unit rgConfig;

interface

{$I mormot.defines.inc}

uses
  SysUtils,
  mormot.core.base,
  mormot.core.json,
  mormot.core.os,
  rgConst;

type
  TRgMode = (rmLocal, rmService);

  TRgConfig = class(TSynJsonFileSettings)
  private
    fMode: TRgMode;
    fHost: RawUtf8;
    fPort: RawUtf8;
  public
    constructor Create; override;
  published
    property Mode: TRgMode read fMode write fMode;
    property Host: RawUtf8 read fHost write fHost;
    property Port: RawUtf8 read fPort write fPort;
  end;

function RgConfig: TRgConfig;

implementation

var
  GRgConfig: TRgConfig;

function RgConfig: TRgConfig;
begin
  if GRgConfig = nil then
  begin
    GRgConfig := TRgConfig.Create;
    GRgConfig.LoadFromFile(
      IncludeTrailingPathDelimiter(Executable.ProgramFilePath) +
      ConfigFileName);
    GRgConfig.SaveIfNeeded;
  end;
  Result := GRgConfig;
end;

{ TRgConfig }

constructor TRgConfig.Create;
begin
  inherited Create;
  fMode := rmLocal;
  fHost := 'localhost';
  fPort := HttpPort;
end;

initialization

finalization
  FreeAndNil(GRgConfig);

end.
