{:
---------------------------------------------------(C) martindoyle 2017-2026 --
 Project : mdComponents

  Module : mdLayout.pas - Adaptive Layout Manager

  Description:
    Generic layout manager for cross-platform adaptive UI design.
    Provides relative positioning, margin management, and platform-specific
    adjustments without using anchors or RAD-style constraints.

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
--------------------------------------------------------------------------------
}
unit mdLayout;

interface

uses
  Classes, SysUtils, Controls, Forms, StdCtrls, ExtCtrls, Graphics;

type
  // Direction for relative placement
  TLayoutDirection = (ldRight, ldBelow, ldAbove, ldLeft);

  // Alignment options
  TLayoutAlign = (laLeft, laCenter, laRight, laTop, laBottom);

  // Spacing mode
  TSpacingMode = (smFixed, smProportional);

  { TLayoutMargins - Manages form margins }
  TLayoutMargins = record
    Left, Top, Right, Bottom: Integer;
    Middle: Integer; // Spacing between major sections
    BaseHeight: Integer; // Base control height for proportional calculations
  end;

  { TLayoutSpacing - Defines spacing between controls }
  TLayoutSpacing = record
    Distance: Single;      // Multiplier for proportional, pixels for fixed
    Mode: TSpacingMode;
  end;

// Record factory functions (Delphi 7 compatible)
function LayoutMargins(ABaseHeight: Integer): TLayoutMargins;
function LayoutMarginsCustom(ALeft, ATop, ARight, ABottom, AMiddle: Integer): TLayoutMargins;
function LayoutSectionSpacing(const AMargins: TLayoutMargins; AMultiplier: Single): Integer;
function LayoutSpacingProportional(ADistance: Single): TLayoutSpacing;
function LayoutSpacingFixed(APixels: Integer): TLayoutSpacing;

type
  { TLayoutHelper - Main layout helper class }
  TLayoutHelper = class
  private
    FForm: TForm;
    FMargins: TLayoutMargins;
    FPlatformAdjusted: Boolean;
    procedure ApplyPlatformFixes;
  public
    constructor Create(AForm: TForm; AMargins: TLayoutMargins);

    // Basic placement
    procedure Place(const AReference: TControl; const ATarget: TControl;
      ADirection: TLayoutDirection; ASpacing: TLayoutSpacing); overload;
    procedure Place(const AReference: TControl; const ATarget: TControl;
      ADirection: TLayoutDirection; ADistance: Single = 0.5); overload;

    // Alignment helpers
    procedure AlignTo(const AReference: TControl; const ATarget: TControl;
      AHorzAlign: TLayoutAlign; AVertAlign: TLayoutAlign);

    // Group placement (place multiple controls in sequence)
    procedure PlaceColumn(const AReference: TControl;
      const AControls: array of TControl; ASpacing: TLayoutSpacing);
    procedure PlaceRow(const AReference: TControl;
      const AControls: array of TControl; ASpacing: TLayoutSpacing);

    // Separator placement with asymmetric spacing (small before, larger after)
    procedure PlaceSeparator(const AReference: TControl; const ASeparator: TControl;
      const ANextControl: TControl; ASpacingBefore: Single = 0.6; ASpacingAfter: Single = 1.5);

    // Size calculations
    function CalculateMaxWidth(const AControls: array of TControl): Integer;
    function CalculateMaxHeight(const AControls: array of TControl): Integer;
    function CalculateTotalHeight(const AControls: array of TControl): Integer;
    function CalculateTotalWidth(const AControls: array of TControl): Integer;

    // Form sizing
    procedure AutoSizeForm(AMinWidth: Integer = 0; AMinHeight: Integer = 0);
    procedure CenterControl(const AControl: TControl; AHorizontal: Boolean = True; AVertical: Boolean = True);

    // Platform-specific adjustments
    procedure AdjustForPlatform;

    property Margins: TLayoutMargins read FMargins write FMargins;
  end;

  { TLayoutBuilder - Fluent API for complex layouts }
  TLayoutBuilder = class
  private
    FHelper: TLayoutHelper;
    FLastControl: TControl;
    FReferenceControl: TControl;
    FNextDirection: TLayoutDirection;
    FNextSpacing: Single;
  public
    constructor Create(AHelper: TLayoutHelper; AStartControl: TControl);

    // Fluent methods
    function Below(ASpacing: Single = 0.5): TLayoutBuilder;
    function Right(ASpacing: Single = 0.5): TLayoutBuilder;
    function Add(AControl: TControl): TLayoutBuilder;
    function AlignLeft: TLayoutBuilder;
    function AlignCenter: TLayoutBuilder;
    function Skip(ADistance: Single): TLayoutBuilder;

    // Finalize
    procedure Done;
  end;

// Helper functions for common patterns
procedure LayoutAboutDialog(AForm: TForm; AImageCtrl, AHeaderLabel: TControl;
  const AInfoLabels: array of TLabel; const ASystemLabels: array of TLabel;
  ALine: TBevel; AOKButton: TButton);

implementation

uses
  Math;

{ Record factory functions }

function LayoutMargins(ABaseHeight: Integer): TLayoutMargins;
begin
  Result.BaseHeight := ABaseHeight;
  Result.Left := ABaseHeight;
  Result.Top := ABaseHeight;
  Result.Bottom := ABaseHeight;
  Result.Right := 2 * ABaseHeight;
  Result.Middle := ABaseHeight;
end;

function LayoutMarginsCustom(ALeft, ATop, ARight, ABottom, AMiddle: Integer): TLayoutMargins;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Right := ARight;
  Result.Bottom := ABottom;
  Result.Middle := AMiddle;
end;

function LayoutSectionSpacing(const AMargins: TLayoutMargins; AMultiplier: Single): Integer;
begin
  Result := Round(AMultiplier * AMargins.BaseHeight);
end;

function LayoutSpacingProportional(ADistance: Single): TLayoutSpacing;
begin
  Result.Distance := ADistance;
  Result.Mode := smProportional;
end;

function LayoutSpacingFixed(APixels: Integer): TLayoutSpacing;
begin
  Result.Distance := APixels;
  Result.Mode := smFixed;
end;

{ TLayoutHelper }

constructor TLayoutHelper.Create(AForm: TForm; AMargins: TLayoutMargins);
begin
  inherited Create;
  FForm := AForm;
  FMargins := AMargins;
  FPlatformAdjusted := False;
end;

procedure TLayoutHelper.ApplyPlatformFixes;
{$IFDEF DARWIN}
var
  i: Integer;
{$ENDIF}
begin
  if FPlatformAdjusted then Exit;

  {$IFDEF DARWIN}
  // Fix macOS button heights and image transparency
  for i := 0 to FForm.ControlCount - 1 do
  begin
    if FForm.Controls[i] is TButton then
      FForm.Controls[i].Height := 22;
    if FForm.Controls[i] is TImage then
      TImage(FForm.Controls[i]).Transparent := False;
  end;
  {$ENDIF}

  FPlatformAdjusted := True;
end;

procedure TLayoutHelper.Place(const AReference: TControl; const ATarget: TControl;
  ADirection: TLayoutDirection; ASpacing: TLayoutSpacing);
var
  Left, Top: Integer;
  SpacingPixels: Integer;
begin
  {$IFDEF DEBUG}
  Assert(AReference <> nil, 'Reference control is nil');
  Assert(ATarget <> nil, 'Target control is nil');
  {$ENDIF}

  // Calculate spacing
  if ASpacing.Mode = smProportional then
    SpacingPixels := Round(ASpacing.Distance * AReference.Height)
  else
    SpacingPixels := Round(ASpacing.Distance);

  // Calculate position based on direction
  Left := 0;
  Top := 0;
  case ADirection of
    ldRight:
      begin
        Left := AReference.Left + AReference.Width + SpacingPixels;
        Top := AReference.Top;
      end;
    ldBelow:
      begin
        Left := AReference.Left;
        Top := AReference.Top + AReference.Height + SpacingPixels;
      end;
    ldAbove:
      begin
        Left := AReference.Left;
        Top := AReference.Top - ATarget.Height - SpacingPixels;
      end;
    ldLeft:
      begin
        Left := AReference.Left - ATarget.Width - SpacingPixels;
        Top := AReference.Top;
      end;
  end;

  ATarget.SetBounds(Left, Top, ATarget.Width, ATarget.Height);
end;

procedure TLayoutHelper.Place(const AReference: TControl; const ATarget: TControl;
  ADirection: TLayoutDirection; ADistance: Single = 0.5);
begin
  Place(AReference, ATarget, ADirection, LayoutSpacingProportional(ADistance));
end;

procedure TLayoutHelper.AlignTo(const AReference: TControl; const ATarget: TControl;
  AHorzAlign: TLayoutAlign; AVertAlign: TLayoutAlign);
var
  Left, Top: Integer;
begin
  Left := ATarget.Left;
  Top := ATarget.Top;

  // Horizontal alignment
  case AHorzAlign of
    laLeft: Left := AReference.Left;
    laCenter: Left := AReference.Left + (AReference.Width - ATarget.Width) div 2;
    laRight: Left := AReference.Left + AReference.Width - ATarget.Width;
  end;

  // Vertical alignment
  case AVertAlign of
    laTop: Top := AReference.Top;
    laCenter: Top := AReference.Top + (AReference.Height - ATarget.Height) div 2;
    laBottom: Top := AReference.Top + AReference.Height - ATarget.Height;
  end;

  ATarget.SetBounds(Left, Top, ATarget.Width, ATarget.Height);
end;

procedure TLayoutHelper.PlaceColumn(const AReference: TControl;
  const AControls: array of TControl; ASpacing: TLayoutSpacing);
var
  i: Integer;
  CurrentRef: TControl;
begin
  if Length(AControls) = 0 then Exit;

  CurrentRef := AReference;
  for i := Low(AControls) to High(AControls) do
  begin
    Place(CurrentRef, AControls[i], ldBelow, ASpacing);
    CurrentRef := AControls[i];
  end;
end;

procedure TLayoutHelper.PlaceRow(const AReference: TControl;
  const AControls: array of TControl; ASpacing: TLayoutSpacing);
var
  i: Integer;
  CurrentRef: TControl;
begin
  if Length(AControls) = 0 then Exit;

  CurrentRef := AReference;
  for i := Low(AControls) to High(AControls) do
  begin
    Place(CurrentRef, AControls[i], ldRight, ASpacing);
    CurrentRef := AControls[i];
  end;
end;

procedure TLayoutHelper.PlaceSeparator(const AReference: TControl;
  const ASeparator: TControl; const ANextControl: TControl;
  ASpacingBefore: Single = 0.6; ASpacingAfter: Single = 1.5);
begin
  {$IFDEF DEBUG}
  Assert(AReference <> nil, 'Reference control is nil');
  Assert(ASeparator <> nil, 'Separator control is nil');
  Assert(ANextControl <> nil, 'Next control is nil');
  {$ENDIF}

  // Place separator below reference with smaller spacing
  Place(AReference, ASeparator, ldBelow, ASpacingBefore);

  // Place next control below separator with larger spacing
  Place(ASeparator, ANextControl, ldBelow, ASpacingAfter);
end;

function TLayoutHelper.CalculateMaxWidth(const AControls: array of TControl): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := Low(AControls) to High(AControls) do
    if AControls[i].Width > Result then
      Result := AControls[i].Width;
end;

function TLayoutHelper.CalculateMaxHeight(const AControls: array of TControl): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := Low(AControls) to High(AControls) do
    if AControls[i].Height > Result then
      Result := AControls[i].Height;
end;

function TLayoutHelper.CalculateTotalHeight(const AControls: array of TControl): Integer;
var
  i: Integer;
  MaxBottom: Integer;
begin
  MaxBottom := 0;
  for i := Low(AControls) to High(AControls) do
    MaxBottom := Max(MaxBottom, AControls[i].Top + AControls[i].Height);

  Result := MaxBottom;
end;

function TLayoutHelper.CalculateTotalWidth(const AControls: array of TControl): Integer;
var
  i: Integer;
  MaxRight: Integer;
begin
  MaxRight := 0;
  for i := Low(AControls) to High(AControls) do
    MaxRight := Max(MaxRight, AControls[i].Left + AControls[i].Width);

  Result := MaxRight;
end;

procedure TLayoutHelper.AutoSizeForm(AMinWidth: Integer = 0; AMinHeight: Integer = 0);
var
  MaxRight, MaxBottom: Integer;
  i: Integer;
begin
  MaxRight := 0;
  MaxBottom := 0;

  for i := 0 to FForm.ControlCount - 1 do
  begin
    MaxRight := Max(MaxRight, FForm.Controls[i].Left + FForm.Controls[i].Width);
    MaxBottom := Max(MaxBottom, FForm.Controls[i].Top + FForm.Controls[i].Height);
  end;

  FForm.ClientWidth := Max(AMinWidth, MaxRight + FMargins.Right);
  FForm.ClientHeight := Max(AMinHeight, MaxBottom + FMargins.Bottom);
end;

procedure TLayoutHelper.CenterControl(const AControl: TControl; AHorizontal: Boolean = True; AVertical: Boolean = True);
begin
  if AHorizontal then
    AControl.Left := (FForm.ClientWidth - AControl.Width) div 2;
  if AVertical then
    AControl.Top := (FForm.ClientHeight - AControl.Height) div 2;
end;

procedure TLayoutHelper.AdjustForPlatform;
begin
  ApplyPlatformFixes;
end;

{ TLayoutBuilder }

constructor TLayoutBuilder.Create(AHelper: TLayoutHelper; AStartControl: TControl);
begin
  inherited Create;
  FHelper := AHelper;
  FReferenceControl := AStartControl;
  FLastControl := AStartControl;
  FNextDirection := ldBelow;
  FNextSpacing := 0.5;
end;

function TLayoutBuilder.Below(ASpacing: Single = 0.5): TLayoutBuilder;
begin
  FNextDirection := ldBelow;
  FNextSpacing := ASpacing;
  Result := Self;
end;

function TLayoutBuilder.Right(ASpacing: Single = 0.5): TLayoutBuilder;
begin
  FNextDirection := ldRight;
  FNextSpacing := ASpacing;
  Result := Self;
end;

function TLayoutBuilder.Add(AControl: TControl): TLayoutBuilder;
begin
  FHelper.Place(FLastControl, AControl, FNextDirection, FNextSpacing);
  FLastControl := AControl;
  // Reset to default
  FNextDirection := ldBelow;
  FNextSpacing := 0.5;
  Result := Self;
end;

function TLayoutBuilder.AlignLeft: TLayoutBuilder;
begin
  FLastControl.Left := FReferenceControl.Left;
  Result := Self;
end;

function TLayoutBuilder.AlignCenter: TLayoutBuilder;
begin
  FLastControl.Left := FReferenceControl.Left +
    (FReferenceControl.Width - FLastControl.Width) div 2;
  Result := Self;
end;

function TLayoutBuilder.Skip(ADistance: Single): TLayoutBuilder;
var
  SpacingPixels: Integer;
begin
  SpacingPixels := Round(ADistance * FLastControl.Height);
  // Move virtual position down
  FLastControl.Top := FLastControl.Top + SpacingPixels;
  Result := Self;
end;

procedure TLayoutBuilder.Done;
begin
  // Cleanup if needed
end;

{ Helper Functions }

procedure LayoutAboutDialog(AForm: TForm; AImageCtrl, AHeaderLabel: TControl;
  const AInfoLabels: array of TLabel; const ASystemLabels: array of TLabel;
  ALine: TBevel; AOKButton: TButton);
var
  Layout: TLayoutHelper;
  Margins: TLayoutMargins;
  MaxLabelWidth: Integer;
  i: Integer;
  AllLabels: array of TControl;
begin
  // Calculate margins based on first info label height
  if Length(AInfoLabels) > 0 then
    Margins := LayoutMargins(AInfoLabels[0].Height)
  else
    Margins := LayoutMargins(16); // Default

  Layout := TLayoutHelper.Create(AForm, Margins);
  try
    // Apply platform fixes first
    Layout.AdjustForPlatform;

    // Place image
    AImageCtrl.SetBounds(Margins.Left, Margins.Top,
      AImageCtrl.Width, AImageCtrl.Height);

    // Place header next to image
    AHeaderLabel.SetBounds(
      Margins.Left + AImageCtrl.Width + Margins.Middle,
      Margins.Top,
      AHeaderLabel.Width,
      AHeaderLabel.Height
    );

    // Place info labels in column below header
    if Length(AInfoLabels) > 0 then
    begin
      Layout.Place(AHeaderLabel, AInfoLabels[0], ldBelow, 0.3);
      for i := 1 to High(AInfoLabels) do
        Layout.Place(AInfoLabels[i-1], AInfoLabels[i], ldBelow, 0.5);
    end;

    // Place line
    if (ALine <> nil) and (Length(AInfoLabels) > 0) then
    begin
      // Calculate max label width
      SetLength(AllLabels, Length(AInfoLabels) + Length(ASystemLabels));
      for i := 0 to High(AInfoLabels) do
        AllLabels[i] := AInfoLabels[i];
      for i := 0 to High(ASystemLabels) do
        AllLabels[Length(AInfoLabels) + i] := ASystemLabels[i];

      MaxLabelWidth := Layout.CalculateMaxWidth(AllLabels);
      ALine.Width := MaxLabelWidth;
      Layout.Place(AInfoLabels[High(AInfoLabels)], ALine, ldBelow, 0.8);
    end;

    // Place system info labels
    if Length(ASystemLabels) > 0 then
    begin
      if Length(AInfoLabels) > 0 then
        Layout.Place(AInfoLabels[0], ASystemLabels[0], ldBelow, 3.0)
      else
        Layout.Place(AHeaderLabel, ASystemLabels[0], ldBelow, 3.0);

      for i := 1 to High(ASystemLabels) do
        Layout.Place(ASystemLabels[i-1], ASystemLabels[i], ldBelow, 0.5);
    end;

    // Auto-size form
    Layout.AutoSizeForm;

    // Place OK button at bottom-right
    AOKButton.SetBounds(
      AForm.ClientWidth - Margins.Right - AOKButton.Width,
      AForm.ClientHeight - Margins.Bottom - AOKButton.Height,
      AOKButton.Width,
      AOKButton.Height
    );

    AForm.Position := poDesktopCenter;

  finally
    Layout.Free;
  end;
end;

end.
