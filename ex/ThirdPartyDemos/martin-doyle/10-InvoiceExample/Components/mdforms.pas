{:
---------------------------------------------------(C) martindoyle 2017-2026 --
 Project : mdComponents

  Module : mdForms.pas

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
unit MdForms;

interface

uses
  Classes, SysUtils, Controls, Forms, Menus,
  {$IFDEF FPC}
  LCLType
  {$ELSE FPC}
  Windows
  {$ENDIF FPC},
  mdLayout;

type
  TFormMode = (fmBrowse, fmInsert, fmEdit);

  { MDChildForm }

  TMDChildForm = class(TForm)
  private
    FAsChild: boolean;
    FTempParent: TWinControl;
    FLayout: TLayoutHelper;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; AParent: TWinControl); reintroduce; overload;
    destructor Destroy; override;
    procedure InitLayout(ABaseHeight: Integer);
    function GetFormMenu: TMainMenu; virtual; abstract;
    function CanChange: boolean; virtual;
    property Layout: TLayoutHelper read FLayout;
  end;

  { TMDDBModeForm }

  TMDDBModeForm = class(TMDChildForm)
  private
    FFormMode: TFormMode;
    FOnSetFormMode: TNotifyEvent;
  protected
    procedure SetFormMode(AValue: TFormMode); virtual;
    function GetFormMode: TFormMode; virtual;
  public
    property FormMode: TFormMode read GetFormMode write SetFormMode;
  published
    property OnSetFormMode: TNotifyEvent read FOnSetFormMode write FOnSetFormMode;
  end;

  { TMDDBNavStatForm }

  TMDDBNavStatForm = class(TMDDBModeForm)
  private
  protected
    procedure SetButtons; virtual;
    procedure SetStatusBar; virtual;
    procedure SetFormMode(AValue: TFormMode); override;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; AParent: TWinControl); overload;
    procedure SetToolBarParent(AParent: TWinControl);
    procedure SetStatusBarParent(AParent: TWinControl);
  end;

implementation

{ TMDChildForm }

procedure TMDChildForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if FAsChild then
    Params.Style := Params.Style or WS_CHILD;
end;

procedure TMDChildForm.Loaded;
begin
  inherited Loaded;
  if FAsChild then
  begin
    align := alClient;
    BorderStyle := bsNone;
    BorderIcons := [];
    Parent := FTempParent;
    Position := poDefault;
  end;
end;

constructor TMDChildForm.Create(AOwner: TComponent);
begin
  FAsChild := False;
  inherited Create(AOwner);
end;

constructor TMDChildForm.Create(AOwner: TComponent; AParent: TWinControl);
begin
  FAsChild := True;
  FTempParent := aParent;
  inherited Create(AOwner);
end;

destructor TMDChildForm.Destroy;
begin
  FLayout.Free;
  inherited Destroy;
end;

procedure TMDChildForm.InitLayout(ABaseHeight: Integer);
begin
  FreeAndNil(FLayout);
  FLayout := TLayoutHelper.Create(Self, LayoutMargins(ABaseHeight));
  FLayout.AdjustForPlatform;
end;

function TMDChildForm.CanChange: boolean;
begin
  Result := True;
end;

{ TMDDBModeForm }

procedure TMDDBModeForm.SetFormMode(AValue: TFormMode);
begin
  FFormMode := AValue;
  if Assigned(FOnSetFormMode) then
    FOnSetFormMode(self);
end;

function TMDDBModeForm.GetFormMode: TFormMode;
begin
  Result := FFormMode;
end;

{ TMDDBNavStatForm }

procedure TMDDBNavStatForm.SetButtons;

  procedure SetBrowseButtons;
  begin
  end;

  procedure SetInsertButtons;
  begin
  end;

  procedure SetEditButtons;
  begin
  end;

begin
  case FormMode of
    fmBrowse: SetBrowseButtons;
    fmInsert: SetInsertButtons;
    fmEdit: SetEditButtons;
  end;
end;

procedure TMDDBNavStatForm.SetStatusBar;
begin

end;

procedure TMDDBNavStatForm.SetFormMode(AValue: TFormMode);
begin
  inherited SetFormMode(AValue);
  SetButtons;
  SetStatusBar;
end;

constructor TMDDBNavStatForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FormMode := fmBrowse;
end;

constructor TMDDBNavStatForm.Create(AOwner: TComponent; AParent: TWinControl);
begin
  inherited Create(AOwner, AParent);
  FormMode := fmBrowse;
end;

procedure TMDDBNavStatForm.SetToolBarParent(AParent: TWinControl);
begin

end;

procedure TMDDBNavStatForm.SetStatusBarParent(AParent: TWinControl);
begin

end;

end.
