{:
———————————————————————————————————————————————— © martindoyle 2017-2025 ——
 Project : mdComponents

  Module : mdButtons.pas

  Last modified
    Date : 18.08.2024
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
unit mdButtons;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Buttons, mdMessages;

type
  TLTSpeedButton = class(TSpeedButton)
  private
    procedure CMParentFontChanged(var Message: TMessage); message
            CM_PARENTFONTCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
  published
  end;

  TLTBitButton = class(TBitBtn)
  private
    procedure CMParentFontChanged(var Message: TMessage); message
            CM_PARENTFONTCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
  published
  end;

  TLTBigButton = class(TLTSpeedButton)
  private
    procedure CMParentFontChanged(var Message: TMessage); message
            CM_PARENTFONTCHANGED;
  end;

  TLTSmallButton = class(TLTBitButton)
  private
    procedure CMParentFontChanged(var Message: TMessage); message
            CM_PARENTFONTCHANGED;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Lutz', [TLTBigButton]);
  RegisterComponents('Lutz', [TLTSmallButton]);
end;

{ TLTSpeedButton }

{
******************************** TLTSpeedButton ********************************
}
constructor TLTSpeedButton.Create(AOwner: TComponent);
begin
  inherited;
  Height:=150;
  Width:=150;
  Cursor:=crHandPoint;
end;

procedure TLTSpeedButton.CMParentFontChanged(var Message: TMessage);
begin
  inherited;
  ParentFont:=True;
  Font.Style:=[fsBold];
end;

{ TLTBigButton }

{
********************************* TLTBigButton *********************************
}
procedure TLTBigButton.CMParentFontChanged(var Message: TMessage);
var
  intLength: Integer;
begin
  inherited;
  intLength:=Abs(Font.Height) * 9;
  if intLength < 121 then
    intLength:=121;
  Height:=intLength;
  Width:=intLength;
end;

{ TLTSmallButton }

{
******************************** TLTSmallButton ********************************
}
procedure TLTSmallButton.CMParentFontChanged(var Message: TMessage);
var
  intHeight: Integer;
  FFontHeight: Integer;
begin
  inherited;
  FFontHeight := Abs(Font.Height);
  intHeight:=FFontHeight * 3;
  if intHeight < 43 then
    Height:=43
  else
    Height:=intHeight;
  Width:=FFontHeight * 10;
end;

{ TLTBitButton }

{
********************************* TLTBitButton *********************************
}
constructor TLTBitButton.Create(AOwner: TComponent);
begin
  inherited;
  Height:=150;
  Width:=150;
  Cursor:=crHandPoint;
  if FStdControl then
  else
end;

procedure TLTBitButton.CMParentFontChanged(var Message: TMessage);
begin
  inherited;
  ParentFont:=True;
  Font.Style:=[fsBold];
end;


end.

