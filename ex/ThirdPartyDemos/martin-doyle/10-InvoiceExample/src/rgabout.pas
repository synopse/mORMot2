{:
———————————————————————————————————————————————— © martindoyle 2017-2025 ——
 Project : Rechnung

 Using mORMot2
     Synopse mORMot2 framework. Copyright (C) 2025 Arnaud Bouchez
     Synopse Informatique - http://synopse.info

  Module : rgAbout.pas

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
    ----------------------------------------------------------------------------
}
unit rgAbout;

interface

{$I mormot.defines.inc}

uses
  Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    AppVersion: TLabel;
    CPU: TLabel;
    BIOS: TLabel;
    OKButton: TButton;
    Copyright: TLabel;
    Line: TBevel;
    SKUName: TLabel;
    Memory: TLabel;
    OS: TLabel;
    ImageMain: TImage;
    LabelmORMot: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

procedure ShowAboutBox;

implementation

uses
  Math,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mdLayout,
  rgConst;

{$R *.dfm}

var
  AboutBox: TAboutForm;

procedure ShowAboutBox;
begin
  AboutBox := TAboutForm.Create(Application);
  try
    AboutBox.ShowModal;
  finally
    AboutBox.Free;
  end;
end;

procedure TAboutForm.FormShow(Sender: TObject);
var
  Layout: TLayoutHelper;
  Margins: TLayoutMargins;
  MS: TMemoryInfo;
  MaxLabelWidth: Integer;
begin
  // Set Label text
  AppVersion.Caption := FormatString('Version %.%.% (Build: %)',
    [AppVersionMajor, AppVersionMinor, AppVersionBuild, AppBuildDate]);
  SKUName.Caption := 'Generated with: ' + COMPILER_VERSION;
  Copyright.Caption := 'Using mORMot' + SYNOPSE_FRAMEWORK_FULLVERSION;
  OS.Caption := OSVersionText;
  CPU.Caption := CpuInfoText;
  BIOS.Caption := BiosInfoText;
  GetMemoryInfo(MS, True);
  Memory.Caption := GetMemoryInfoText;

  // Calculate margins based on label height
  Margins := TLayoutMargins.Create(SKUName.Height);
  Layout := TLayoutHelper.Create(Self, Margins);
  try
    // Apply platform-specific fixes (macOS transparency, button heights)
    Layout.AdjustForPlatform;

    // Place image
    ImageMain.SetBounds(Margins.Left, Margins.Top,
      ImageMain.Width, ImageMain.Height);

    // Place header label with larger font
    LabelmORMot.Font.Size := Round(1.7 * SKUName.Height);
    LabelmORMot.Font.Style := [fsBold];
    LabelmORMot.SetBounds(
      Margins.Left + ImageMain.Width + Margins.Middle,
      Margins.Top,
      LabelmORMot.Width,
      LabelmORMot.Height
    );

    // Place version below header
    Layout.Place(LabelmORMot, AppVersion, ldBelow, 0.2);

    // Place info labels in column below version
    Layout.Place(AppVersion, SKUName, ldBelow, 0.5);
    Layout.Place(SKUName, Copyright, ldBelow, 0.5);

    // Calculate and set line width
    MaxLabelWidth := Layout.CalculateMaxWidth([AppVersion, SKUName, Copyright, OS, CPU, BIOS, Memory]);
    Line.Width := MaxLabelWidth;

    // Place separator line with asymmetric spacing (small before, larger after)
    Layout.PlaceSeparator(Copyright, Line, OS);

    // Place remaining system info labels
    Layout.PlaceColumn(OS, [CPU, BIOS, Memory], TLayoutSpacing.Proportional(0.5));

    // Auto-size form based on content
    Layout.AutoSizeForm;

    // Place OK button at bottom-right
    OKButton.SetBounds(
      ClientWidth - Margins.Right - OKButton.Width,
      ClientHeight - Margins.Bottom - OKButton.Height,
      OKButton.Width,
      OKButton.Height
    );

    Position := poDesktopCenter;
  finally
    Layout.Free;
  end;
end;



procedure TAboutForm.FormCreate(Sender: TObject);
begin
 {$IFDEF FPC}
 {$ELSE FPC}
 Font.Handle := Screen.MenuFont.Handle;
 {$ENDIF FPC}
end;

end.
