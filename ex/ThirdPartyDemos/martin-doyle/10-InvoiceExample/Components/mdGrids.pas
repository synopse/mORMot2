{:
———————————————————————————————————————————————— © martindoyle 2017-2026 ——
 Project : mdComponents

  Module : mdGrids.pas - ListView-compatible Grid Component

  Description:
    Custom grid component using TDrawGrid internally to replace TListView.
    Fixes macOS Cocoa crash (BUG-004) while providing ListView-compatible API.
    Inspired by mORMot2 TOrmTableToGrid pattern.

  Last modified
    Date : 01.02.2026
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
unit mdGrids;

interface

uses
  Classes, SysUtils, Controls, Graphics, Grids, Forms, LCLType, LCLIntf;

type
  TMDListGrid = class;
  TMDListItem = class;

  //==========================================================================
  // TMDListColumn - Column definition
  //==========================================================================
  TMDListColumn = class(TCollectionItem)
  private
    FCaption: string;
    FWidth: Integer;
    FAlignment: TAlignment;
    FMinWidth: Integer;
    FMaxWidth: Integer;
    procedure SetCaption(const Value: string);
    procedure SetWidth(Value: Integer);
    procedure SetAlignment(Value: TAlignment);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: string read FCaption write SetCaption;
    property Width: Integer read FWidth write SetWidth default 100;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property MinWidth: Integer read FMinWidth write FMinWidth default 20;
    property MaxWidth: Integer read FMaxWidth write FMaxWidth default 1000;
  end;

  //==========================================================================
  // TMDListColumns - Column collection
  //==========================================================================
  TMDListColumns = class(TCollection)
  private
    FOwner: TMDListGrid;
    function GetItem(Index: Integer): TMDListColumn;
    procedure SetItem(Index: Integer; Value: TMDListColumn);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TMDListGrid);
    function Add: TMDListColumn;
    function Owner: TMDListGrid;
    property Items[Index: Integer]: TMDListColumn read GetItem write SetItem; default;
  end;

  //==========================================================================
  // TMDListItem - Row item (similar to TListItem)
  //==========================================================================
  TMDListItem = class
  private
    FOwner: TMDListGrid;
    FIndex: Integer;
    FCaption: string;
    FSubItems: TStringList;
    FData: Pointer;
    procedure SetCaption(const Value: string);
    function GetSelected: Boolean;
  public
    constructor Create(AOwner: TMDListGrid; AIndex: Integer);
    destructor Destroy; override;
    property Caption: string read FCaption write SetCaption;
    property SubItems: TStringList read FSubItems;
    property Data: Pointer read FData write FData;
    property Index: Integer read FIndex;
    property Selected: Boolean read GetSelected;
  end;

  //==========================================================================
  // TMDListItems - Items collection
  //==========================================================================
  TMDListItems = class
  private
    FOwner: TMDListGrid;
    FList: TList;
    FUpdateCount: Integer;
    function GetCount: Integer;
    function GetItem(Index: Integer): TMDListItem;
  public
    constructor Create(AOwner: TMDListGrid);
    destructor Destroy; override;
    function Add: TMDListItem;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure BeginUpdate;
    procedure EndUpdate;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TMDListItem read GetItem; default;
  end;

  //==========================================================================
  // Event types (ListView-compatible)
  //==========================================================================
  TMDListSelectItemEvent = procedure(Sender: TObject; Item: TMDListItem;
    Selected: Boolean) of object;

  //==========================================================================
  // TMDListGrid - Main component
  //==========================================================================
  TMDListGrid = class(TCustomControl)
  private
    // Internal components
    FGrid: TDrawGrid;

    // Data
    FColumns: TMDListColumns;
    FItems: TMDListItems;
    FItemIndex: Integer;
    FPreviousItemIndex: Integer;

    // Options
    FRowSelect: Boolean;
    FShowHeader: Boolean;

    // Events
    FOnSelectItem: TMDListSelectItemEvent;
    FOnDblClick: TNotifyEvent;

    // Internal methods (inspired by TOrmTableToGrid)
    function NotDefined: Boolean;
    procedure UpdateGridSize;
    procedure SyncGridSelection;

    // Grid event handlers
    procedure GridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure GridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure GridDblClick(Sender: TObject);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    // Drawing
    procedure DrawHeaderCell(ACol: Integer; const ARect: TRect);
    procedure DrawDataCell(ACol, ARow: Integer; const ARect: TRect;
      State: TGridDrawState);
    procedure DrawCellText(const ARect: TRect; const AText: string;
      AAlignment: TAlignment);

    // Property accessors
    function GetSelected: TMDListItem;
    procedure SetItemIndex(Value: Integer);
    procedure SetRowSelect(Value: Boolean);
    procedure SetShowHeader(Value: Boolean);

  protected
    procedure Resize; override;
    procedure Loaded; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Invalidate; override;
    function ItemAtPos(X, Y: Integer): TMDListItem;

    property Selected: TMDListItem read GetSelected;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;

  published
    property Columns: TMDListColumns read FColumns;
    property Items: TMDListItems read FItems;
    property RowSelect: Boolean read FRowSelect write SetRowSelect default True;
    property ShowHeader: Boolean read FShowHeader write SetShowHeader default True;
    property OnSelectItem: TMDListSelectItemEvent read FOnSelectItem write FOnSelectItem;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;

    // Inherited properties
    property Align;
    property Anchors;
    property BorderSpacing;
    property Color;
    property Constraints;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
  end;

implementation

//============================================================================
// TMDListColumn
//============================================================================

constructor TMDListColumn.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FCaption := '';
  FWidth := 100;
  FAlignment := taLeftJustify;
  FMinWidth := 20;
  FMaxWidth := 1000;
end;

procedure TMDListColumn.Assign(Source: TPersistent);
var
  Src: TMDListColumn;
begin
  if Source is TMDListColumn then
  begin
    Src := TMDListColumn(Source);
    FCaption := Src.Caption;
    FWidth := Src.Width;
    FAlignment := Src.Alignment;
    FMinWidth := Src.MinWidth;
    FMaxWidth := Src.MaxWidth;
    Changed(False);
  end
  else
    inherited Assign(Source);
end;

function TMDListColumn.GetDisplayName: string;
begin
  if FCaption <> '' then
    Result := FCaption
  else
    Result := inherited GetDisplayName;
end;

procedure TMDListColumn.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

procedure TMDListColumn.SetWidth(Value: Integer);
begin
  if Value < FMinWidth then
    Value := FMinWidth;
  if Value > FMaxWidth then
    Value := FMaxWidth;
  if FWidth <> Value then
  begin
    FWidth := Value;
    Changed(False);
  end;
end;

procedure TMDListColumn.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Changed(False);
  end;
end;

//============================================================================
// TMDListColumns
//============================================================================

constructor TMDListColumns.Create(AOwner: TMDListGrid);
begin
  inherited Create(TMDListColumn);
  FOwner := AOwner;
end;

function TMDListColumns.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TMDListColumns.GetItem(Index: Integer): TMDListColumn;
begin
  Result := TMDListColumn(inherited GetItem(Index));
end;

procedure TMDListColumns.SetItem(Index: Integer; Value: TMDListColumn);
begin
  inherited SetItem(Index, Value);
end;

function TMDListColumns.Add: TMDListColumn;
begin
  Result := TMDListColumn(inherited Add);
end;

function TMDListColumns.Owner: TMDListGrid;
begin
  Result := FOwner;
end;

procedure TMDListColumns.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if FOwner <> nil then
    FOwner.UpdateGridSize;
end;

//============================================================================
// TMDListItem
//============================================================================

constructor TMDListItem.Create(AOwner: TMDListGrid; AIndex: Integer);
begin
  inherited Create;
  FOwner := AOwner;
  FIndex := AIndex;
  FCaption := '';
  FSubItems := TStringList.Create;
  FData := nil;
end;

destructor TMDListItem.Destroy;
begin
  FreeAndNil(FSubItems);
  inherited Destroy;
end;

procedure TMDListItem.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    if (FOwner <> nil) and (FOwner.FItems.FUpdateCount = 0) then
      FOwner.Invalidate;
  end;
end;

function TMDListItem.GetSelected: Boolean;
begin
  Result := (FOwner <> nil) and (FOwner.FItemIndex = FIndex);
end;

//============================================================================
// TMDListItems
//============================================================================

constructor TMDListItems.Create(AOwner: TMDListGrid);
begin
  inherited Create;
  FOwner := AOwner;
  FList := TList.Create;
  FUpdateCount := 0;
end;

destructor TMDListItems.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

function TMDListItems.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TMDListItems.GetItem(Index: Integer): TMDListItem;
begin
  if (Index >= 0) and (Index < FList.Count) then
    Result := TMDListItem(FList[Index])
  else
    Result := nil;
end;

function TMDListItems.Add: TMDListItem;
begin
  Result := TMDListItem.Create(FOwner, FList.Count);
  FList.Add(Result);
  if FUpdateCount = 0 then
    FOwner.UpdateGridSize;
end;

procedure TMDListItems.Clear;
var
  i: Integer;
begin
  for i := FList.Count - 1 downto 0 do
    TMDListItem(FList[i]).Free;
  FList.Clear;
  if (FOwner <> nil) then
  begin
    FOwner.FItemIndex := -1;
    if FUpdateCount = 0 then
      FOwner.UpdateGridSize;
  end;
end;

procedure TMDListItems.Delete(Index: Integer);
var
  i: Integer;
begin
  if (Index >= 0) and (Index < FList.Count) then
  begin
    TMDListItem(FList[Index]).Free;
    FList.Delete(Index);
    // Update indices for remaining items
    for i := Index to FList.Count - 1 do
      TMDListItem(FList[i]).FIndex := i;
    // Adjust selection
    if FOwner.FItemIndex >= FList.Count then
      FOwner.FItemIndex := FList.Count - 1;
    if FUpdateCount = 0 then
      FOwner.UpdateGridSize;
  end;
end;

procedure TMDListItems.BeginUpdate;
begin
  Inc(FUpdateCount);
  if (FOwner <> nil) and (FOwner.FGrid <> nil) then
    FOwner.FGrid.BeginUpdate;
end;

procedure TMDListItems.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
    begin
      if (FOwner <> nil) then
      begin
        FOwner.UpdateGridSize;
        if FOwner.FGrid <> nil then
          FOwner.FGrid.EndUpdate;
      end;
    end;
  end;
end;

//============================================================================
// TMDListGrid
//============================================================================

constructor TMDListGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Control defaults
  Width := 250;
  Height := 150;
  TabStop := True;

  // Options
  FRowSelect := True;
  FShowHeader := True;
  FItemIndex := -1;
  FPreviousItemIndex := -1;

  // Create collections
  FColumns := TMDListColumns.Create(Self);
  FItems := TMDListItems.Create(Self);

  // Create internal grid
  FGrid := TDrawGrid.Create(Self);
  FGrid.Parent := Self;
  FGrid.Align := alClient;
  FGrid.BorderStyle := bsSingle;
  FGrid.DefaultDrawing := False;
  FGrid.FixedCols := 0;
  FGrid.FixedRows := 1;
  FGrid.Options := [goFixedHorzLine, goFixedVertLine, goVertLine,
                    goHorzLine, goRowSelect, goThumbTracking];
  FGrid.RowCount := 1;
  FGrid.ColCount := 1;

  // Connect events
  FGrid.OnDrawCell := GridDrawCell;
  FGrid.OnSelectCell := GridSelectCell;
  FGrid.OnDblClick := GridDblClick;
  FGrid.OnKeyDown := GridKeyDown;
end;

destructor TMDListGrid.Destroy;
begin
  // CRITICAL: Reset event handlers BEFORE freeing grid
  // This prevents GPF in Cocoa Autorelease Pool on macOS (BUG-004 fix)
  // Pattern from mORMot2 TOrmTableToGrid (lines 475-492)
  if FGrid <> nil then
  begin
    FGrid.OnDrawCell := nil;
    FGrid.OnSelectCell := nil;
    FGrid.OnDblClick := nil;
    FGrid.OnKeyDown := nil;
    FGrid.OnKeyPress := nil;
    FGrid.OnMouseDown := nil;
    FGrid.OnMouseUp := nil;
    FGrid.OnMouseMove := nil;
  end;

  FreeAndNil(FItems);
  FreeAndNil(FColumns);
  FreeAndNil(FGrid);
  inherited Destroy;
end;

// NotDefined Guard (from TOrmTableToGrid pattern)
function TMDListGrid.NotDefined: Boolean;
begin
  Result := (Self = nil) or
            (FGrid = nil) or
            (FColumns = nil) or
            (FItems = nil);
end;

procedure TMDListGrid.UpdateGridSize;
var
  i: Integer;
  RowCount: Integer;
begin
  if NotDefined then
    Exit;

  // Update column count and widths
  if FColumns.Count > 0 then
  begin
    FGrid.ColCount := FColumns.Count;
    for i := 0 to FColumns.Count - 1 do
      FGrid.ColWidths[i] := FColumns[i].Width;
  end
  else
  begin
    FGrid.ColCount := 1;
    FGrid.ColWidths[0] := FGrid.ClientWidth;
  end;

  // Update row count
  if FShowHeader then
  begin
    FGrid.FixedRows := 1;
    RowCount := FItems.Count + 1;
  end
  else
  begin
    FGrid.FixedRows := 0;
    RowCount := FItems.Count;
  end;

  if RowCount < 1 then
    RowCount := 1;
  FGrid.RowCount := RowCount;

  // Sync selection
  SyncGridSelection;

  FGrid.Invalidate;
end;

procedure TMDListGrid.SyncGridSelection;
var
  GridRow: Integer;
begin
  if NotDefined then
    Exit;

  if FShowHeader then
    GridRow := FItemIndex + 1
  else
    GridRow := FItemIndex;

  if (GridRow >= FGrid.FixedRows) and (GridRow < FGrid.RowCount) then
    FGrid.Row := GridRow
  else if FGrid.RowCount > FGrid.FixedRows then
    FGrid.Row := FGrid.FixedRows;
end;

procedure TMDListGrid.GridDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  DataRow: Integer;
begin
  if NotDefined then
    Exit;
  if (ACol < 0) or (ACol >= FColumns.Count) then
    Exit;

  if FShowHeader and (ARow = 0) then
    DrawHeaderCell(ACol, Rect)
  else
  begin
    if FShowHeader then
      DataRow := ARow - 1
    else
      DataRow := ARow;

    if (DataRow >= 0) and (DataRow < FItems.Count) then
      DrawDataCell(ACol, DataRow, Rect, State);
  end;
end;

procedure TMDListGrid.DrawHeaderCell(ACol: Integer; const ARect: TRect);
var
  Col: TMDListColumn;
begin
  if (ACol < 0) or (ACol >= FColumns.Count) then
    Exit;

  Col := FColumns[ACol];

  // Background
  FGrid.Canvas.Brush.Color := clBtnFace;
  FGrid.Canvas.FillRect(ARect);

  // Border
  FGrid.Canvas.Pen.Color := clBtnShadow;
  FGrid.Canvas.MoveTo(ARect.Right - 1, ARect.Top);
  FGrid.Canvas.LineTo(ARect.Right - 1, ARect.Bottom);
  FGrid.Canvas.MoveTo(ARect.Left, ARect.Bottom - 1);
  FGrid.Canvas.LineTo(ARect.Right, ARect.Bottom - 1);

  // Text (bold, centered)
  FGrid.Canvas.Font := Self.Font;
  FGrid.Canvas.Font.Style := [fsBold];
  DrawCellText(ARect, Col.Caption, taCenter);
  FGrid.Canvas.Font.Style := [];
end;

procedure TMDListGrid.DrawDataCell(ACol, ARow: Integer; const ARect: TRect;
  State: TGridDrawState);
var
  Item: TMDListItem;
  Col: TMDListColumn;
  Text: string;
begin
  if (ARow < 0) or (ARow >= FItems.Count) then
    Exit;
  if (ACol < 0) or (ACol >= FColumns.Count) then
    Exit;

  Item := FItems[ARow];
  Col := FColumns[ACol];

  // Background
  if gdSelected in State then
    FGrid.Canvas.Brush.Color := clHighlight
  else
    FGrid.Canvas.Brush.Color := Self.Color;
  FGrid.Canvas.FillRect(ARect);

  // Text color
  FGrid.Canvas.Font := Self.Font;
  if gdSelected in State then
    FGrid.Canvas.Font.Color := clHighlightText
  else
    FGrid.Canvas.Font.Color := Self.Font.Color;

  // Get text
  if ACol = 0 then
    Text := Item.Caption
  else if (ACol - 1) < Item.SubItems.Count then
    Text := Item.SubItems[ACol - 1]
  else
    Text := '';

  DrawCellText(ARect, Text, Col.Alignment);
end;

procedure TMDListGrid.DrawCellText(const ARect: TRect; const AText: string;
  AAlignment: TAlignment);
var
  X, Y: Integer;
  TextWidth, TextHeight: Integer;
  DrawRect: TRect;
begin
  TextHeight := FGrid.Canvas.TextHeight('Ag');
  TextWidth := FGrid.Canvas.TextWidth(AText);
  Y := ARect.Top + (ARect.Bottom - ARect.Top - TextHeight) div 2;

  case AAlignment of
    taLeftJustify:
      X := ARect.Left + 4;
    taRightJustify:
      X := ARect.Right - TextWidth - 4;
    taCenter:
      X := ARect.Left + (ARect.Right - ARect.Left - TextWidth) div 2;
  else
    X := ARect.Left + 4;
  end;

  // Clip text to cell
  DrawRect := ARect;
  InflateRect(DrawRect, -2, 0);
  FGrid.Canvas.TextRect(DrawRect, X, Y, AText);
end;

procedure TMDListGrid.GridSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
var
  DataRow: Integer;
  Item: TMDListItem;
begin
  if NotDefined then
    Exit;

  CanSelect := True;

  // Calculate data row
  if FShowHeader then
    DataRow := ARow - 1
  else
    DataRow := ARow;

  // Update selection
  if (DataRow >= 0) and (DataRow < FItems.Count) then
  begin
    FPreviousItemIndex := FItemIndex;
    FItemIndex := DataRow;

    // Fire event
    if Assigned(FOnSelectItem) then
    begin
      Item := FItems[DataRow];
      // Deselect previous if different
      if (FPreviousItemIndex >= 0) and (FPreviousItemIndex < FItems.Count) and
         (FPreviousItemIndex <> FItemIndex) then
        FOnSelectItem(Self, FItems[FPreviousItemIndex], False);
      // Select new
      FOnSelectItem(Self, Item, True);
    end;
  end
  else
  begin
    // Invalid row - deselect
    if (FItemIndex >= 0) and (FItemIndex < FItems.Count) and
       Assigned(FOnSelectItem) then
      FOnSelectItem(Self, FItems[FItemIndex], False);
    FItemIndex := -1;
  end;
end;

procedure TMDListGrid.GridDblClick(Sender: TObject);
begin
  if NotDefined then
    Exit;
  if Assigned(FOnDblClick) then
    FOnDblClick(Self);
end;

procedure TMDListGrid.GridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if NotDefined then
    Exit;

  // Enter key triggers double-click action
  if (Key = VK_RETURN) and (Shift = []) then
  begin
    if Assigned(FOnDblClick) then
      FOnDblClick(Self);
    Key := 0;
  end;
end;

function TMDListGrid.GetSelected: TMDListItem;
begin
  if (FItemIndex >= 0) and (FItemIndex < FItems.Count) then
    Result := FItems[FItemIndex]
  else
    Result := nil;
end;

procedure TMDListGrid.SetItemIndex(Value: Integer);
begin
  if Value < -1 then
    Value := -1;
  if Value >= FItems.Count then
    Value := FItems.Count - 1;

  if FItemIndex <> Value then
  begin
    FItemIndex := Value;
    SyncGridSelection;
  end;
end;

procedure TMDListGrid.SetRowSelect(Value: Boolean);
begin
  if FRowSelect <> Value then
  begin
    FRowSelect := Value;
    if FGrid <> nil then
    begin
      if Value then
        FGrid.Options := FGrid.Options + [goRowSelect]
      else
        FGrid.Options := FGrid.Options - [goRowSelect];
    end;
  end;
end;

procedure TMDListGrid.SetShowHeader(Value: Boolean);
begin
  if FShowHeader <> Value then
  begin
    FShowHeader := Value;
    UpdateGridSize;
  end;
end;

procedure TMDListGrid.Resize;
begin
  inherited Resize;
  // Grid handles resize via alClient
end;

procedure TMDListGrid.Loaded;
begin
  inherited Loaded;
  UpdateGridSize;
end;

procedure TMDListGrid.Invalidate;
begin
  inherited Invalidate;
  if FGrid <> nil then
    FGrid.Invalidate;
end;

function TMDListGrid.ItemAtPos(X, Y: Integer): TMDListItem;
var
  ACol, ARow: Integer;
  DataRow: Integer;
begin
  Result := nil;
  if NotDefined then
    Exit;

  FGrid.MouseToCell(X, Y, ACol, ARow);

  if FShowHeader then
    DataRow := ARow - 1
  else
    DataRow := ARow;

  if (DataRow >= 0) and (DataRow < FItems.Count) then
    Result := FItems[DataRow];
end;

end.
