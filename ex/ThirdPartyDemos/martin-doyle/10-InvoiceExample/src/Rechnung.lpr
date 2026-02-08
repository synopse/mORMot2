{:
———————————————————————————————————————————————— © martindoyle 2017-2025 ——
 Project : Rechnung

 Using mORMot2
     Synopse mORMot2 framework. Copyright (C) 2025 Arnaud Bouchez
     Synopse Informatique - http://synopse.info

  Module : Rechnung.dpr

  Last modified
    Date : 26.12.2025
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

program Rechnung;
{$I mormot.defines.inc}
{$define PUREMORMOT2}
{$define NEWRTTINOTUSED}

uses
  {$I mormot.uses.inc}// follow FPC_X64MM or FPC_LIBCMM conditionals
  {$ifdef UNIX}
  cwstring, // needed as fallback if ICU is not available
  {$endif UNIX}
  Classes,
  {$IFDEF FPC}
    Interfaces, // this includes the LCL widgetset
  {$ENDIF FPC}
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.os.mac,
  mormot.core.unicode,
  mormot.core.Text,
  mormot.core.datetime,
  mormot.core.log,
  mormot.db.raw.sqlite3, // for the SQLite3 version below

  Forms,
  rgConst,
  rgMain;

  {$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
