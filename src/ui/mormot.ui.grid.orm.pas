/// Display ORM and database content in grids
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.ui.grid.orm;

{
  *****************************************************************************

   Fill a Read/Only TDrawGrid
    - TOrmTableToGrid wrapper to manage a TDrawGrid from a TOrmTable
    - Fill a TStringGrid from ORM results

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  {$ifdef FPC}
  LCLType,
  LCLProc,
  LCLIntf,
  LMessages,
  {$else}
  Windows,
  Types,
  {$endif FPC}
  {$ifdef NEEDVCLPREFIX}
  vcl.graphics,
  vcl.controls,
  vcl.grids,
  {$else}
  graphics,
  controls,
  grids,
  {$endif NEEDVCLPREFIX}
  sysutils,
  classes,
  variants,
  dateutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.buffers,
  mormot.core.rtti,
  mormot.core.data,
  mormot.core.json,
  mormot.db.core,
  mormot.orm.base,
  mormot.orm.core,
  mormot.orm.client,
  mormot.rest.client,
  mormot.ui.controls;



{************ TOrmTableToGrid wrapper to manage a TDrawGrid from a TOrmTable }

type
  EOrmTableToGrid = class(ESynException);

  /// standard actions for User Interface generation
  // - actNoAction for not defined action
  // - actMark (standard action) to Mark rows, i.e. display sub-menu with
  // actmarkAllEntries..actmarkOlderThanOneYear items
  // - actUnmarkAll (standard action) to UnMark all rows
  // - actmarkAllEntries to Mark all rows
  // - actmarkToday to Mark rows for today
  // - actmarkThisWeek to Mark rows for this Week
  // - actmarkThisMonth to Mark rows for this month
  // - actmarkYesterday to Mark rows for today
  // - actmarkLastWeek to Mark rows for Last Week
  // - actmarkLastMonth to Mark rows for Last month
  // - actmarkOlderThanOneDay to Mark rows After one day
  // - actmarkOlderThanOneWeek to Mark rows older than one week
  // - actmarkOlderThanOneMonth to Mark rows older than one month
  // - actmarkOlderThanSixMonths to Mark rows older than one half year
  // - actmarkOlderThanOneYear to Mark rows older than one year
  // - actmarkInverse to Inverse Mark values (ON->OFF, OFF->ON)
  TGridAction = (
    actNoAction,
    actMark,
    actUnmarkAll,
    actmarkAllEntries,
    actmarkToday,
    actmarkThisWeek,
    actmarkThisMonth,
    actmarkYesterday,
    actmarkLastWeek,
    actmarkLastMonth,
    actmarkOlderThanOneDay,
    actmarkOlderThanOneWeek,
    actmarkOlderThanOneMonth,
    actmarkOlderThanSixMonths,
    actmarkOlderThanOneYear,
    actmarkInverse);

  /// set of standard actions for User Interface generation
  TGridActions = set of TGridAction;

  /// kind of event used to change some text on the fly for grid display
  // - expect generic string Text, i.e. UnicodeString for Delphi 2009/2010,
  // ready to be used with the VCL for all Delphi compiler versions
  // - if the cell at FiieldIndex/RowIndex is to have a custom content,
  // shall set the Text variable content and return TRUE
  // - if returns FALSE, the default content will be displayed
  TValueTextEvent = function(Sender: TOrmTable; FieldIndex, RowIndex: Integer;
    var Text: string): boolean of object;

  /// kind of event used to change some text on the fly for popup hint
  // - expect generic string Text, i.e. UnicodeString for Delphi 2009/2010,
  // ready to be used with the VCL for all Delphi compiler versions
  THintTextEvent = function(Sender: TOrmTable; FieldIndex, RowIndex: Integer;
    var Text: string): boolean of object;

  /// kind of event used to display a menu on a cell right click
  TRightClickCellEvent = procedure(Sender: TOrmTable;
    ACol, ARow, MouseX, MouseY: Integer) of object;

  /// the available alignments of a TOrmTableToGrid cell
  TOrmTableToGridAlign = (
   galLeft,
   galCenter,
   galRight);

  /// a hidden component, used for displaying a TOrmTable in a TDrawGrid
  // - just call TOrmTableToGrid.Create(Grid,Table)  to initiate the association
  // - the Table will be released when no longer necessary
  // - any former association by TOrmTableToGrid.Create() will be overridden
  // - handle unicode, column size, field sort, incremental key lookup, hide ID
  // - Ctrl + click on a cell to display its full unicode content
  TOrmTableToGrid = class(TComponent)
  private
    fOnSelectCell: TSelectCellEvent;
    fOnRightClickCell: TRightClickCellEvent;
    fClient: TRestClientURI;
    fModel: TOrmModel;
    fOnDrawCellBackground: TDrawCellEvent;
    fMarked: TByteDynArray;
    fMarkAllowed: boolean;
    fMouseDownMarkedValue: (markNone, markOn, markOff);
    fTruncAsHint: boolean;
    fHeaderCheckboxSelectsInsteadOfSort: boolean;
    fOnSelectCellProcessing: boolean;
    fFieldIndexTimeLogForMark: integer;
    function GetMarked(RowIndex: PtrInt): boolean;
    procedure SetMarked(RowIndex: PtrInt; const Value: boolean);
    function GetMarkAvailable: boolean;
    function GetDrawGrid: TDrawGrid;
    function GetMarkedIsOnlyCurrrent: boolean;
    function GetMarkedTotalCount: integer;
    // function because field information may be set manually after Create
    function GetFieldIndexTimeLogForMark: integer;
    function GetAlign(aCol: cardinal): TOrmTableToGridAlign;
    procedure SetAlign(aCol: cardinal; Value: TOrmTableToGridAlign);
    function GetCustomFormat(aCol: cardinal): string;
    procedure SetCustomFormat(aCol: cardinal; const Value: string);
    function GetGridColumnWidths: RawUtf8;
    procedure SetGridColumnWidths(const Value: RawUtf8);
  protected
    fOnValueText: TValueTextEvent;
    fOnHintText: THintTextEvent;
    fOnSort: TNotifyEvent;
    /// associated TOrmTable result to be displayed
    fTable: TOrmTable;
    /// true if the specific field is in Ascending order
    fFieldOrder: array of boolean;
    /// current field number used for field
    fCurrentFieldOrder: integer;
    /// avoid resizing columns on height change only
    fLastWidth: cardinal;
    /// contain the key to be searched
    fIncrementalSearch: RawUtf8;
    /// true if row is changed by incremental key lookup
    fIncrementalSearchMove: boolean;
    /// used to display some hint text
    fHint: THintWindowDelayed;
    /// text of this field must be aligned as set by Aligned[] property
    fAligned: array of TOrmTableToGridAlign;
    /// custom formats as set by CustomFormat[] property
    fCustomFormat: array of string;
    /// text of this column/field name has been truncated
    fFieldNameTruncated: Int64;
    /// used by OnTableUpdate() event
    fOnTableUpdateID: TIDDynArray;
    /// return true if a GPF may occur
    function NotDefined: boolean;
  public
    /// fill a TDrawGrid with the results contained in a TOrmTable
    constructor Create(aGrid: TDrawGrid; aTable: TOrmTable;
      aClient: TRestClientURI); reintroduce;
    /// release the hidden object
    // - will be called by the parent Grid when it is destroyed
    // - will be called by any future TOrmTableToGrid.Create() association
    // - free the associated TOrmTable and its memory content
    // - will reset the Grid overridden events to avoid GPF
    destructor Destroy; override;
    /// called by the owner TDrawGrid to draw a Cell from the TOrmTable data
    // - the cell is drawn using direct Win32 Unicode API
    // - the first row (fixed) is drawn as field name (centered bold text with
    //  sorting order displayed with a triangular arrow)
    procedure DrawCell(Sender: TObject; ACol, ARow: Longint; Rect: TRect;
      State: TGridDrawState);
    /// called by the owner TDrawGrid when a Cell is selected
    procedure DrawGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: boolean);
    /// called by the owner TDrawGrid when a Cell is clicked by the mouse
    // - check if the first (fixed) row is clicked: then change sort order
    // - Ctrl + click to display its full unicode content (see HintText to customize it)
    procedure DrawGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    /// called by the owner TDrawGrid when the mouse is unclicked over a Cell
    procedure DrawGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    /// called by the owner TDrawGrid when the mouse is over a Cell
    procedure DrawGridMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    /// called by the owner TDrawGrid when the user presses a key
    // - used for incremental key lookup
    procedure DrawGridKeyPress(Sender: TObject; var Key: Char);
    /// called by the owner TDrawGrid when the user presses a key
    // - used for LEFT/RIGHT ARROW column order change
    procedure DrawGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    /// call this procedure to automaticaly resize the TDrawString columns
    // - can be used as TOrmTableToGrid.From(DrawGrid).Resize();
    procedure Resize(Sender: TObject);
    /// display a popup Hint window at a specified Cell position
    // - expect generic string Text, i.e. UnicodeString for Delphi 2009/2010,
    // ready to be used with the VCL for all Delphi compiler versions
    procedure ShowHintString(const Text: string; ACol, ARow, Time: integer;
      FontColor: TColor = clBlack);
    /// if the ID column is available, hides it from the grid
    procedure IDColumnHide;
    /// toggle the sort order of a specified column
    procedure SortChange(ACol: integer);
    /// set a specified column for sorting
    // - if ACol=-1, then the Marked[] rows are shown first, in current sort
    procedure SortForce(ACol: integer; Ascending: boolean; ARow: integer = -1);
    /// get the ID of the first selected row, 0 on error (no ID field e.g.)
    // - useful even if ID column was hidden with IDColumnHide
    function SelectedID: TID;
    /// retrieve the record content of the first selected row, nil on error
    // - record type is retrieved via Table.QueryTables[0] (if defined)
    // - warning: it's up to the caller to Free the created instance after use
    // (you should e.g. embedd the process in a try...finally block):
    // ! Rec := Grid.SelectedRecordCreate;
    // ! if Rec<>nil then
    // ! try
    // !   DoSomethingWith(Rec);
    // ! finally
    // !   Rec.Free;
    // ! end;
    // - useful even if ID column was hidden with IDColumnHide
    function SelectedRecordCreate: TOrm;
    /// set individual column alignment
    property Aligned[aCol: cardinal]: TOrmTableToGridAlign
      read GetAlign write SetAlign;
    /// set columns number which must be aligned to non default left layout
    // - a faster overload to Aligned[] property
    procedure SetAligned(const aCols: array of cardinal;
      aAlign: TOrmTableToGridAlign);
    /// set column alignment for a given type
    // - a faster overload to Aligned[] property
    procedure SetAlignedByType(aFieldType: TOrmFieldType;
      aAlign: TOrmTableToGridAlign);
    /// set individual column custom format
    // - as handled by TOrmTable.ExpandAsString() method, i.e. Format() or
    // FormatFloat()/FormatCurrency() mask for oftFloat or oftCurrency, or
    // FormatDateTime() mask for oftDateTime, oftDateTimeMS, oftTimeLog, oftModTime,
    // oftCreateTime, oftUnixTime, oftUnixMSTime)
    property CustomFormat[aCol: cardinal]: string
      read GetCustomFormat write SetCustomFormat;
    /// set a custom format for all columns of a given type
    // - a faster overload to CustomFormat[] property
    // - only support the field types and formats handled by CustomFormat[] property
    procedure SetCustomFormatByType(aFieldType: TOrmFieldType;
      const aCustomFormat: string);
    /// force the mean of characters length for every field
    // - supply a string with every character value is proportionate to
    // the corresponding column width
    // - if the character is lowercase, the column is set as centered
    // - if aMarkAllowed is set, a first checkbox column is added, for
    // reflecting and updating the Marked[] field values e.g.
    // - if Lengths='', will set some uniform width, left aligned
    procedure SetFieldLengthMean(const Lengths: RawUtf8; aMarkAllowed: boolean);
    /// force all columns to have a specified width, in pixels
    procedure SetFieldFixedWidth(aColumnWidth: integer);
    /// force refresh paint of Grid from Table data
    // - return true if Table data has been successfully retrieved from Client
    // and if data was refreshed because changed since last time
    // - if ForceRefresh is TRUE, the Client is not used to retrieve the data,
    // which must be already refreshed before this call
    // - if AutoResizeColumns is TRUE, the column visual width will be re-computed
    // from the actual content - set it to FALSE to avoid it
    function Refresh(ForceRefresh: boolean = false;
      AutoResizeColumns: boolean = true): boolean;
    /// call this procedure after a refresh of the data
    // - current Row will be set back to aID
    // - called internal by Refresh function above
    procedure AfterRefresh(const aID: TID; AutoResizeColumns: boolean);
    /// you can call this method when the list is no more on the screen
    // - it will hide any pending popup Hint windows, for example
    procedure PageChanged;
    /// perform the corresponding Mark/Unmark[All] Action
    procedure SetMark(aAction: TGridAction);
    /// retrieve the Marked[] bits array
    function GetMarkedBits: pointer;
    /// read-only access to a particular row values, as VCL text
    // - Model is one TOrmModel instance (used to display TRecordReference)
    // - returns the text as generic string, ready to be displayed via the VCL
    // after translation, for oftEnumerate, oftTimeLog, oftRecord and all other
    // properties
    // - uses OnValueText property Event if defined by caller
    function ExpandRowAsString(Row: PtrInt; Model: TOrmModel = nil): string;
    /// retrieve the associated TOrmTableToGrid from a specific TDrawGrid
    class function From(Grid: TDrawGrid): TOrmTableToGrid;
    /// used by TRestClientURI.UpdateFromServer() to let the client
    // perform the rows update (for Marked[])
    procedure OnTableUpdate(State: TOnTableUpdateState);

    /// associated TDrawGrid
    // - just typecast the Owner as TDrawGrid
    property DrawGrid: TDrawGrid
      read GetDrawGrid;
    /// associated TOrmTable to be displayed
    property Table: TOrmTable
      read fTable;
    /// associated Client used to retrieved the Table data
    property Client: TRestClientURI
      read fClient;
    /// associated Client Model used to retrieved the Table data
    property Model: TOrmModel
      read fModel;
    /// used to display some hint text
    property Hint: THintWindowDelayed
      read fHint;
    /// assign an event here to customize the background drawing of a cell
    property OnDrawCellBackground: TDrawCellEvent
      read fOnDrawCellBackground write fOnDrawCellBackground;
    /// true if Marked[] is available (add checkboxes at the left side of every row)
    property MarkAllowed: boolean
      read fMarkAllowed;
    /// true if any Marked[] is checked
    property MarkAvailable: boolean
      read GetMarkAvailable;
    /// true if only one entry is in Marked[], and it is the current one
    property MarkedIsOnlyCurrrent: boolean
      read GetMarkedIsOnlyCurrrent;
    /// returns the number of item marked or selected
    // - if no item is marked, it return 0 even if a row is currently selected
    property MarkedTotalCount: integer
      read GetMarkedTotalCount;
    /// retrieves if a row was previously marked
    // - first data row index is 1
    property Marked[RowIndex: PtrInt]: boolean
      read GetMarked write SetMarked;
    /// retrieve or define the column widths of this grid, as text
    // - as a CSV list of the associated DrawGrid.ColWidths[] values
    property GridColumnWidths: RawUtf8
      read GetGridColumnWidths write SetGridColumnWidths;
    /// retrieves the index of the oftTimeLog first field
    // - i.e. the field index which can be used for Marked actions
    // - equals -1 if not such field exists
    property FieldIndexTimeLogForMark: integer
      read GetFieldIndexTimeLogForMark write fFieldIndexTimeLogForMark;
    /// current field number used for current table sorting
    property CurrentFieldOrder: integer
      read fCurrentFieldOrder;
    /// set to FALSE to display the column title as hint when truncated on screen
    property FieldTitleTruncatedNotShownAsHint: boolean
      read fTruncAsHint write fTruncAsHint;
    /// set to TRUE to let the header check box select/unselect all rows
    // instead of sorting them
    // - may be more conventional use of this header check box
    property HeaderCheckboxSelectsInsteadOfSort: boolean
      read fHeaderCheckboxSelectsInsteadOfSort write fHeaderCheckboxSelectsInsteadOfSort;
    /// override this event to customize the text display in the table
    property OnValueText: TValueTextEvent
      read fOnValueText write fOnValueText;
    /// override this event to customize the Ctrl+Mouse click popup text
    property OnHintText: THintTextEvent
      read fOnHintText write fOnHintText;
    /// override this event to customize the Mouse click on a data cell
    property OnSelectCell: TSelectCellEvent
      read fOnSelectCell write fOnSelectCell;
    /// override this event to customize the Mouse right click on a data cell
    property OnRightClickCell: TRightClickCellEvent
      read fOnRightClickCell write fOnRightClickCell;
    /// override this event to be notified when the content is sorted
    property OnSort: TNotifyEvent
      read fOnsort write fOnsort;
  end;


{************ Fill a TStringGrid from ORM results }

/// fill TStringGrid.Cells[] with the supplied data
// - will be slower than the TOrmTableToGrid wrapper (which works in virtual
// mode), but will work on a non standard TDrawGrid component
// - it will display date & time and enumerates as plain text, and handle
// the header properly (using the current i18n language settings, if any)
// - the Model optional parameter will be used to display any RecordRef column
// - all data will be stored within the TStringGrid: you can safely release the
// Source data after having called this procedure
procedure FillStringGrid(Source: TOrmTable; Dest: TStringGrid;
  Model: TOrmModel = nil);



implementation



{************ TOrmTableToGrid wrapper to manage a TDrawGrid from a TOrmTable }

{ TOrmTableToGrid }

resourcestring
  sErrorTOrmTableToGridNoData = '%s didn''t receive any data for %s';

constructor TOrmTableToGrid.Create(aGrid: TDrawGrid; aTable: TOrmTable;
  aClient: TRestClientURI);
begin
  // setup this instance
  if aTable = nil then
    raise EOrmTableToGrid.CreateFmt(
      sErrorTOrmTableToGridNoData, [ClassName, aGrid.Name]);
  From(aGrid).Free; // any old association will be overridden by this instance
  inherited Create({owner=}aGrid); // this instance will be owned by the grid
  fTable := aTable;
  fClient := aClient;
  if aClient <> nil then
    fModel := aClient.Model;
  fHint := THintWindowDelayed.Create(self);
  SetLength(fFieldOrder, Table.FieldCount);   // auto filled to false
  fCurrentFieldOrder := Table.FieldIndex(
    pointer(SqlGetOrder(Table.QuerySQL)));    // get 'ORDER BY' field index
  if fCurrentFieldOrder >= 0 then
    fFieldOrder[fCurrentFieldOrder] := true;  // mark 'ORDER BY' field ascending
  fFieldIndexTimeLogForMark := -2; // so GetFieldIndexTimeLogForMark will get it
  // setup the associated TDrawGrid
  aGrid.RowCount := 2; // first reset row count, to avoid flicking
  aGrid.FixedRows := 1;
  aGrid.Canvas.Font := aGrid.Font;
  with aGrid.Canvas.TextExtent('jQH'#$00B0';') do
    aGrid.DefaultRowHeight := cy + 4;
  aGrid.Options := [
    goFixedHorzLine,
    goFixedVertLine,
    goVertLine,
    goHorzLine,
    goColSizing,
    goRowSelect,
    goThumbTracking]; // no goRangeSelect
  aGrid.FixedCols := 0;
  aGrid.ColCount := Table.FieldCount;
  aGrid.OnDrawCell := DrawCell;
  aGrid.OnMouseMove := DrawGridMouseMove;
  aGrid.OnMouseDown := DrawGridMouseDown;
  aGrid.OnMouseUp := DrawGridMouseUp;
  aGrid.OnSelectCell := DrawGridSelectCell;
  aGrid.OnKeyPress := DrawGridKeyPress;
  aGrid.OnKeyDown := DrawGridKeyDown;
  if Table.RowCount > 0 then
    aGrid.RowCount := Table.RowCount + 1; // set the virtual raw count
end;

destructor TOrmTableToGrid.Destroy;
begin
  if (Owner <> nil) and
     Owner.InheritsFrom(TDrawGrid) then
    with TDrawGrid(Owner) do
    begin
      // reset the Grid overridden events to avoid GPF
      OnDrawCell := nil;
      OnMouseMove := nil;
      OnMouseDown := nil;
      OnMouseUp := nil;
      OnSelectCell := nil;
      OnKeyPress := nil;
      OnKeyDown := nil;
    end;
  FreeAndNil(fTable);
  inherited;
end;

class function TOrmTableToGrid.From(Grid: TDrawGrid): TOrmTableToGrid;
var
  i: PtrInt;
begin
  if Grid <> nil then
    for i := 0 to Grid.ComponentCount - 1 do
    begin
      result := pointer(Grid.Components[i]);
      if result.InheritsFrom(TOrmTableToGrid) and
         (result.Owner = Grid) then
        exit;
    end;
  result := nil;
end;

procedure TOrmTableToGrid.DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  c: TCanvas;
  x, l: integer;
  s: string;
  withmark: boolean;
  al: TAlignment;
  gal: TOrmTableToGridAlign;
begin
  // default cell draw
  if NotDefined then
    exit;
  if Assigned(OnDrawCellBackground) then
    OnDrawCellBackground(Owner, ACol, ARow, Rect, State);
  if (cardinal(ARow) > cardinal(Table.RowCount)) or
     (cardinal(ACol) >= cardinal(Table.FieldCount)) then // avoid any GPF
    exit;
  al := taLeftJustify;
  c := TDrawGrid(Owner).Canvas;
  withmark := fMarkAllowed and (ACol = 0);
  if ARow = 0 then
  begin
    // 1st row = header: bold + centered translated text, with sort indicator
    if not Assigned(OnValueText) or
       not OnValueText(Table, ACol, 0, s) then
      s := Table.GetCaption(0, ACol); // auto translated
    if not Assigned(OnDrawCellBackground) then
      c.Font.Style := [fsBold];
    l := Rect.Right - Rect.Left;
    if withmark then
      dec(l, CheckBoxWidth + 4);
    if c.TextWidth(s) < l then
      gal := galCenter
    else
      gal := galLeft;
    if gal = galCenter then
    begin
      UnSetBit64(fFieldNameTruncated, ACol);
      x := l shr 1;
      al := taCenter;
    end
    else
    begin
      SetBit64(fFieldNameTruncated, ACol);
      x := 2;
    end;
    if withmark then
      inc(x, CheckBoxWidth + 4);
    TextRectString(Rect, c, x, 2, s, al);
    c.Font.Style := [];
    if fCurrentFieldOrder = ACol then
      // sorted field: draw sort indicator
      DrawSortArrow(c, Rect.Right - 8, Rect.Bottom - 9, fFieldOrder[ACol]);
  end
  else
  begin
    // 2. field value rows
    l := Rect.Right - Rect.Left;
    if withmark then
      dec(l, CheckBoxWidth + 4);
    gal := self.Aligned[ACol];
    case gal of
      galCenter:
        begin
          al := taCenter;
          x := l shr 1;
        end;
      galRight:
        begin
          al := taRightJustify;
          x := l - 4;
        end
    else
      x := 4;
    end;
    if withmark then
      inc(x, CheckBoxWidth + 4);
    if Assigned(OnValueText) and
       OnValueText(Table, ACol, ARow, s) then
    begin
      if length(s) > 255 then
        SetLength(s, 255); // avoid blank cell drawing for huge content
      TextRectString(Rect, c, x, 2, s, al);
    end
    else
      case Table.ExpandAsString(ARow, ACol, fModel, s, GetCustomFormat(ACol)) of
        // very fast response (type is evaluated once)
        oftBoolean:
          // display boolean as checkbox
          DrawCheckBox(TDrawGrid(Owner).Handle, c.Handle, Rect, Table.GetB(ARow, ACol));
        oftInteger,
        oftFloat,
        oftCurrency,
        oftEnumerate,
        oftTimeLog,
        oftRecord,
        oftDateTime,
        oftDateTimeMS,
        oftUnixTime,
        oftUnixMSTime:
          // simple values have been translated as text
          TextRectString(Rect, c, x, 2, s, al);
    //oftID,oftTID:
    // proposal: display ID as TOrm content? better compute it in SELECT
      else
        // normal field value: unicode text, left aligned
        TextRectUtf8(Rect, c, x, 2, Table.GetU(ARow, ACol), al, {noctrl=}true);
      end;
  end;
  if withmark then
  begin
    // draw left side checkbox with Marked[] value
    inc(Rect.Left, 2);
    Rect.Right := Rect.Left + CheckBoxWidth;
    if ARow = 0 then
      withmark := HeaderCheckboxSelectsInsteadOfSort and
                  (MarkedTotalCount + 1 = TDrawGrid(Owner).RowCount)
    else
      withmark := Marked[ARow];
    DrawCheckBox(TDrawGrid(Owner).Handle, c.Handle, Rect, withmark);
  end;
end;

procedure TOrmTableToGrid.SortForce(ACol: integer; Ascending: boolean; ARow: integer);
var
  ids: TIDDynArray;
  id: TID;
begin
  if NotDefined or
     (ACol >= Table.FieldCount) then // we allow ACol<0 (see below)
    exit;
  if ARow < 0 then
    ARow := TDrawGrid(Owner).Row; // keep current row selected if none specified
  if ACol < 0 then
  begin
    // if ACol=-1, then the Marked[] rows are shown first, in current sort
    if fMarked = nil then
      exit; // no Marked[] rows to put in first place
    id := Table.GetID(ARow); // current row
    Table.SortBitsFirst(fMarked[0]);
    TDrawGrid(Owner).Row := Table.RowFromID(id);
    // no PageChanged here
  end
  else
  begin
    if fMarked <> nil then
      Table.IDArrayFromBits(fMarked[0], ids); // save current marked entries
    fFieldOrder[ACol] := Ascending;
    fCurrentFieldOrder := ACol;
    Table.SortFields(ACol, fFieldOrder[ACol], @ARow);
    if ids <> nil then
      Table.IDArrayToBits(fMarked[0], ids); // restore marked entries
    TDrawGrid(Owner).Row := ARow; // reselect row after sort (+ Invalidate)
    PageChanged; // hide any pending popup Hint e.g.
  end;
  if Assigned(OnSort) then
    OnSort(TDrawGrid(Owner));
end;

procedure TOrmTableToGrid.DrawGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  c, r: integer;
  s: string; // generic string type, for VCL
begin
  if NotDefined then // avoid any possible GPF
    exit;
  fMouseDownMarkedValue := markNone;
  TDrawGrid(Owner).MouseToCell(X, Y, c, r);
  if cardinal(c) < cardinal(Table.FieldCount) then
    if r = 0 then
      // click within header
      if (ssCtrl in Shift) or
         (Button <> mbLeft) then
      begin
        // Ctrl or right button pressed -> display first row as s
        ShowHintString(Table.GetString(r, c), c, r, 4000);
      end
      else
      begin
        // first row -> sort fields
        if fMarkAllowed and
           (X < CheckBoxWidth + 4) then
          if HeaderCheckboxSelectsInsteadOfSort then
            // toggle selection
            if MarkAvailable then
              SetMark(actUnmarkAll)
            else
              SetMark(actmarkAllEntries)
          else
            // sort Marked[] first
            SortForce(-1, true)
        else if fCurrentFieldOrder = c then
          // same column -> toggle sorting order
          SortForce(c, not fFieldOrder[c])
        else
          // column changed -> sort ascending first
          SortForce(c, true);
      end
    // click on data cell
    else if (Button = mbRight) and
            (ssRight in Shift) and
            Assigned(OnRightClickCell) then
      OnRightClickCell(Table, c, r, X, Y)
    else if (ssCtrl in Shift) or
            (Button <> mbLeft) then
    begin
      if not Assigned(OnHintText) or
         not OnHintText(Table, c, r, s) then
        Table.ExpandAsString(r, c, fModel, s);
      // s := IntToStr(SelectedID);
      ShowHintString(s, c, r, 4000);
    end
    else if (Button = mbLeft) and
            (c = 0) and
            fMarkAllowed and
            (X < CheckBoxWidth + 4) then
    begin
      // on click: invert current Marked[] checkbox state
      if Marked[r] then
        fMouseDownMarkedValue := markOff
      else
        fMouseDownMarkedValue := markOn;
      Marked[r] := (fMouseDownMarkedValue = markOn);
    end;
  TDrawGrid(Owner).Invalidate;
end;

resourcestring
  sPutMarkedRowFirst = 'Sort marked rows first';

procedure TOrmTableToGrid.DrawGridMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  c, r: integer;
begin
  if NotDefined then // avoid any possible GPF
    exit;
  TDrawGrid(Owner).MouseToCell(X, Y, c, r);
  if cardinal(c) >= cardinal(Table.FieldCount) then
    exit;
  if r = 0 then
  begin
    // over the checkbox left of the first row: show appropriate hint
    if (c = 0) and
       fMarkAllowed and
       (fMarked <> nil) and
       (X < CheckBoxWidth + 4) and
       ((Hint = nil) or
        (Hint.Col <> -1) or
        (Hint.Row <> 0)) then
    begin
      if not HeaderCheckboxSelectsInsteadOfSort then
      begin
        ShowHintString(sPutMarkedRowFirst, 0, 0, 1000);
        Hint.Col := -1; // column = -1 for checkbox
      end;
    end
    // over the first row, i.e. column name: show hint if name was truncated
    else if (not FieldTitleTruncatedNotShownAsHint) and
            GetBit64(fFieldNameTruncated, c) and
            ((Hint = nil) or
             (Hint.Col <> c) or
             (Hint.Row <> 0)) then
      ShowHintString(Table.GetCaption(0, c), c, 0, 1000);
  end
  // select/unselect checkbox left of data rows
  else if (c = 0) and
          fMarkAllowed and
          (fMouseDownMarkedValue <> markNone) and
          (X <= CheckBoxWidth + 4) then
    Marked[r] := (fMouseDownMarkedValue = markOn);
end;

procedure TOrmTableToGrid.DrawGridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  fMouseDownMarkedValue := markNone; // reset Marked[] checkbox state
end;

procedure TOrmTableToGrid.DrawGridSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: boolean);
begin
  if NotDefined then // avoid any possible GPF
    exit;
  if not fIncrementalSearchMove then
    fIncrementalSearch := ''; // reset incremental key lookup
  if Assigned(OnSelectCell) and
     not fOnSelectCellProcessing then
  try
    fOnSelectCellProcessing := true; // avoid endless loop or GPF
    OnSelectCell(Sender, ACol, ARow, CanSelect);
  finally
    fOnSelectCellProcessing := false;
  end;
end;

procedure TOrmTableToGrid.Resize(Sender: TObject);
var
  i: PtrInt;
  width, tot: cardinal;
begin
  if NotDefined then // avoid any possible GPF
    exit;
  width := TDrawGrid(Owner).ClientWidth - GetSystemMetrics(SM_CXBORDER) * 4;
  if width = fLastWidth then
    exit; // draw if necessary
  fLastWidth := width;
  tot := Table.FieldLengthMeanSum;
  for i := 0 to Table.FieldCount - 1 do
    TDrawGrid(Owner).ColWidths[i] := (width * Table.FieldLengthMean(i)) div tot;
//  with TDrawGrid(Owner) do SetScrollVPage(Handle,ClientHeight div DefaultRowHeight,RowCount-FixedRows);
end;

function TOrmTableToGrid.NotDefined: boolean;
begin
  result := (self = nil) or
            (Owner = nil) or
            (Table = nil) or
            not Owner.InheritsFrom(TDrawGrid);
end;

procedure TOrmTableToGrid.DrawGridKeyPress(Sender: TObject; var Key: Char);
var
  f, r: integer;
begin
  // incremental key lookup
  if NotDefined then // avoid any possible GPF
    exit;
  if Key = #27 then
    // ESC key reset the lookup string
    fIncrementalSearch := ''
  else if Key = #8 then
  begin
    // BACKDEL key delete last lookup char
    if fIncrementalSearch <> '' then
      SetLength(fIncrementalSearch, length(fIncrementalSearch) - 1);
  end
  else if Key >= ' ' then
    if (Key = ' ') and
       (fIncrementalSearch = '') then
    begin
      // space with no lookup key -> allow mark/unmark current one
      r := TDrawGrid(Owner).Row;
      if fMarkAllowed and
         (r > 0) then
      begin
        Marked[r] := not Marked[r];
        inc(r);
        if r <= Table.RowCount then
          TDrawGrid(Owner).Row := r
        else // and go to next row
          TDrawGrid(Owner).Invalidate;
      end;
      exit;
    end
    else
      fIncrementalSearch :=
        fIncrementalSearch + StringToUtf8(sysutils.UpperCase(string(Key)));
  if fIncrementalSearch = '' then
  begin
    if fHint <> nil then
      fHint.Hide;
    exit; // nothing to search
  end;
  // search from the next row
  f := fCurrentFieldOrder;
  r := Table.SearchValue(fIncrementalSearch, TDrawGrid(Owner).Row + 1,
    fCurrentFieldOrder, Client.Orm);
  if r = 0 then
  begin
    // not found: search from the beginning
    r := Table.SearchValue(fIncrementalSearch, 1, fCurrentFieldOrder, Client.Orm);
    if r = 0 then
    begin
      // not found in this field: search in all fields
      r := Table.SearchValue(fIncrementalSearch,
        TDrawGrid(Owner).Row + 1, @f, Client.Orm);
      if r = 0 then
        // still not found: search in all fields from the beginning
        r := Table.SearchValue(fIncrementalSearch, 1, @f, Client.Orm);
    end;
  end;
  if r > 0 then
  begin
    fIncrementalSearchMove := true; // DrawGridSelectCell() won't reset fIncremental
    TDrawGrid(Owner).Row := r;
    fIncrementalSearchMove := false;
    ShowHintString(Utf8ToString(fIncrementalSearch), f, r, 2000, clNavy);
  end
  else
  // not found: display searched string in red
    ShowHintString(Utf8ToString(fIncrementalSearch) + '?', fCurrentFieldOrder,
      TDrawGrid(Owner).Row, 2000, clRed);
end;

procedure TOrmTableToGrid.ShowHintString(const Text: string;
  ACol, ARow, Time: integer; FontColor: TColor);
begin
  if NotDefined then // avoid any possible GPF
    exit;
  if Text = '' then
  begin
    if fHint <> nil then
      fHint.Hide;
    exit;
  end;
  fHint.Col := ACol;
  fHint.Row := ARow;
  with TDrawGrid(Owner).CellRect(ACol, ARow) do
    fHint.ShowDelayedString(Text, TDrawGrid(Owner), Right, Top + 2, Time, FontColor);
end;

procedure TOrmTableToGrid.DrawGridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  f: integer;
begin
  if NotDefined or
     (Shift <> []) then // avoid any possible GPF
    exit;
  case Key of
    VK_LEFT: // LEFT ARROW key sort previous column
      if fCurrentFieldOrder > 0 then
        f := fCurrentFieldOrder - 1
      else
        f := Table.FieldCount - 1;
    VK_RIGHT: // RIGHT ARROW key sort next column
      if fCurrentFieldOrder >= Table.FieldCount - 1 then
        f := 0
      else
        f := fCurrentFieldOrder + 1;
  else
    exit;
  end;
  SortChange(f);
  Key := 0; // we proceed this key -> caller will ignore it
end;

procedure TOrmTableToGrid.SortChange(ACol: integer);
begin
  if not NotDefined and
     (cardinal(ACol) < cardinal(Table.FieldCount)) then
    if fCurrentFieldOrder = ACol then
      // same column -> toggle sorting order
      SortForce(ACol, not fFieldOrder[ACol])
    else      // column changed -> sort ascending first
      SortForce(ACol, true);
end;

procedure TOrmTableToGrid.IDColumnHide; { TODO: IDColumnHide }
begin
  raise EOrmTableToGrid.CreateUtf8('%.IDColumnHide is unimplemented', [self]);
  if NotDefined {or not Table.IDColumnHide} then
    exit;
  TDrawGrid(Owner).ColCount := Table.FieldCount; // we loose one column
  fCurrentFieldOrder := -1; // force no previous column -> always ascending order
  SortChange(0);
end;

function TOrmTableToGrid.SelectedID: TID;
begin
  if NotDefined then
    result := 0
  else
    result := Table.GetID(TDrawGrid(Owner).Row);
end;

function TOrmTableToGrid.SelectedRecordCreate: TOrm;
var
  id: TID;
  c: TOrmClass;
begin
  id := SelectedID;
  if (id <= 0) or (fClient = nil) then
    result := nil
  else
  begin
    c := TOrmClass(Table.QueryRecordType);
    if (c = nil) or
       not c.InheritsFrom(TOrm) then
      result := nil
    else
      result := c.Create(fClient.Orm, id);
  end;
end;

function TOrmTableToGrid.GetAlign(aCol: cardinal): TOrmTableToGridAlign;
begin
  if (self = nil) or
     (Table = nil) or
     (aCol >= cardinal(length(fAligned))) or
     (aCol >= cardinal(Table.FieldCount)) then
    result := galLeft
  else
    result := fAligned[aCol];
end;

procedure TOrmTableToGrid.SetAlign(aCol: cardinal; Value: TOrmTableToGridAlign);
begin
  if (self = nil) or
     (Table = nil) or
     (aCol >= cardinal(Table.FieldCount)) then
    exit;
  if length(fAligned) < Table.FieldCount then
    SetLength(fAligned, Table.FieldCount);
  fAligned[aCol] := Value;
end;

function TOrmTableToGrid.GetCustomFormat(aCol: cardinal): string;
begin
  if (self = nil) or
     (Table = nil) or
     (aCol >= cardinal(length(fCustomFormat))) or
     (aCol >= cardinal(Table.FieldCount)) then
    result := ''
  else
    result := fCustomFormat[aCol];
end;

procedure TOrmTableToGrid.SetCustomFormat(aCol: cardinal; const Value: string);
begin
  if (self = nil) or
     (Table = nil) or
     (aCol >= cardinal(Table.FieldCount)) then
    exit;
  if length(fCustomFormat) < Table.FieldCount then
    SetLength(fCustomFormat, Table.FieldCount);
  fCustomFormat[aCol] := Value;
end;

procedure TOrmTableToGrid.SetAligned(const aCols: array of cardinal;
  aAlign: TOrmTableToGridAlign);
var
  i: PtrInt;
begin
  if Table <> nil then
    for i := 0 to high(aCols) do
      SetAlign(aCols[i], aAlign);
end;

procedure TOrmTableToGrid.SetAlignedByType(aFieldType: TOrmFieldType;
  aAlign: TOrmTableToGridAlign);
var
  i: PtrInt;
begin
  if (self = nil) or
     (Table = nil) then
    exit;
  for i := 0 to Table.FieldCount - 1 do
    if Table.FieldType(i) = aFieldType then
      SetAlign(i, aAlign);
end;

procedure TOrmTableToGrid.SetCustomFormatByType(aFieldType: TOrmFieldType;
  const aCustomFormat: string);
var
  i: PtrInt;
begin
  if (self = nil) or
     (Table = nil) then
    exit;
  for i := 0 to Table.FieldCount - 1 do
    if Table.FieldType(i) = aFieldType then
      SetCustomFormat(i, aCustomFormat);
end;

procedure TOrmTableToGrid.PageChanged;
begin
  if (Self <> nil) and
     (Hint <> nil) then
    Hint.Hide;
end;

function TOrmTableToGrid.Refresh(ForceRefresh, AutoResizeColumns: boolean): boolean;
var
  refreshed: boolean;
  id: TID;
begin
  if self = nil then
    result := false
  else
  begin
    id := Table.GetID(TDrawGrid(Owner).Row);
    if id = 0 then
      result := false
    else if ForceRefresh then
      result := true
    else
      result := Client.Client.UpdateFromServer([Table], refreshed) and
                refreshed;
    if result then
      AfterRefresh(id, AutoResizeColumns);
  end;
end;

procedure TOrmTableToGrid.AfterRefresh(const aID: TID; AutoResizeColumns: boolean);
var
  r: integer;
  dummy: boolean;
begin
  with TDrawGrid(Owner) do
  begin
    if Table.RowCount = 0 then
      RowCount := 2
    else
      RowCount := Table.RowCount + 1;
    if Table.FieldCount <> ColCount then
    begin
      // we get results from a void table for the first time
      ColCount := Table.FieldCount;
      SetLength(fFieldOrder, Table.FieldCount);
    end;
    r := Table.RowFromID(aID);
    if r = 0 then
      r := 1;
    Row := r;
    TopRow := 1;
    Invalidate;
  end;
  if AutoResizeColumns then
    Resize(nil);
  if Assigned(OnSelectCell) then
    OnSelectCell(Owner, 0, r, dummy); // refresh details
end;

procedure TOrmTableToGrid.SetFieldLengthMean(const Lengths: RawUtf8;
  aMarkAllowed: boolean);
var
  l, i: PtrInt;
  c: AnsiChar;
  means: array of cardinal;
begin
  if self = nil then
    Exit;
  fMarkAllowed := aMarkAllowed;
  l := length(Lengths);
  if l = 0 then
  begin
    SetLength(means, Table.FieldCount);
    for i := 0 to Table.FieldCount - 1 do
      means[i] := 10; // some fixed width
  end
  else if Table.FieldCount = l then
  begin
    SetLength(means, l);
    for i := 0 to l - 1 do
    begin
      c := Lengths[i + 1];
      if c in ['a'..'z'] then
      begin
        Aligned[i] := galCenter;
        dec(c, 32);
      end;
      means[i] := ord(c) + (1 - ord('A'));
    end;
    Table.SetFieldLengthMean(means);
  end;
  if aMarkAllowed then
    Table.FieldLengthMeanIncrease(0, 2); // space for Marked[] checkbox e.g.
end;

procedure TOrmTableToGrid.SetFieldFixedWidth(aColumnWidth: integer);
var
  i: PtrInt;
begin
  with TDrawGrid(Owner) do
    for i := 0 to ColCount - 1 do
      ColWidths[i] := aColumnWidth;
end;

function TOrmTableToGrid.GetGridColumnWidths: RawUtf8;
var
  i: PtrInt;
  w: RawUtf8;
begin
  result := '';
  if self <> nil then
    with TDrawGrid(Owner) do
      for i := 0 to ColCount - 1 do
      begin
        Int32ToUtf8(ColWidths[i], w);
        if i = 0 then
          result := w
        else
          result := result + ',' + w;
      end;
end;

procedure TOrmTableToGrid.SetGridColumnWidths(const Value: RawUtf8);
var
  P: PUtf8Char;
  i: PtrInt;
  w: cardinal;
begin
  if self = nil then
    exit;
  P := pointer(Value);
  with TDrawGrid(Owner) do
    for i := 0 to ColCount - 1 do
    begin
      w := GetNextItemCardinal(P);
      if w = 0 then
        w := 100;
      ColWidths[i] := w;
    end;
end;

function TOrmTableToGrid.GetMarked(RowIndex: PtrInt): boolean;
begin
  dec(RowIndex);
  if (self = nil) or
     (fMarked = nil) or
     (PtrUInt(RowIndex) >= PtrUInt(length(fMarked) shl 3)) then
    result := false
  else
    result := GetBitPtr(pointer(fMarked), RowIndex);
end;

procedure TOrmTableToGrid.SetMarked(RowIndex: PtrInt; const Value: boolean);
var
  n: PtrInt;
begin
  dec(RowIndex);
  if (self = nil) or
     (PtrUInt(RowIndex) >= PtrUInt(Table.RowCount)) then
    exit;
  n := (Table.RowCount shr 3) + 1;
  if length(fMarked) < n then // need to allocate/expand fMarked[] array?
    SetLength(fMarked, n);
  if Value then
    SetBitPtr(pointer(fMarked), RowIndex)
  else
    UnSetBitPtr(pointer(fMarked), RowIndex)
end;

procedure TOrmTableToGrid.SetMark(aAction: TGridAction);
var
  i: PtrInt;
  v: Int64;
  current: TDateTime;
  min, max: TTimeLogBits;
const
  DIFFTIME: array[actMarkOlderThanOneDay..actMarkOlderThanOneYear] of double = (1,
    7, 31, 183, 365); // 183 = more or less half a year
begin
  if NotDefined then
    exit;
  with TDrawGrid(Owner) do
    case aAction of
      actMarkAllEntries:
        for i := 1 to RowCount do
          Marked[i] := true;
      actUnMarkAll:
        if fMarked <> nil then
          Finalize(fMarked);
      actmarkInverse:
        for i := 1 to RowCount do
          Marked[i] := not Marked[i];
    else
      if FieldIndexTimeLogForMark >= 0 then
      begin
        // use TDateTime calculation because TTimeLog is not duration compatible
        current := Trunc(NowUTC);
        case aAction of
          actMarkToday:
            begin
              min.From(current, true);
              max.From(current + 1, true);
            end;
          actMarkThisWeek:
            begin
              min.From(StartOfTheWeek(current), true);
              max.From(EndOfTheWeek(current) + 1, true);
            end;
          actMarkThisMonth:
            begin
              min.From(StartOfTheMonth(current), true);
              max.From(EndOfTheMonth(current) + 1, true);
            end;
          actMarkYesterday:
            begin
              min.From(current - 1, true);
              max.From(current, true);
            end;
          actMarkLastWeek:
            begin
              min.From(IncWeek(StartOfTheWeek(current), -1), true);
              max.From(StartOfTheWeek(current), true);
            end;
          actMarkLastMonth:
            begin
              min.From(IncMonth(StartOfTheMonth(current), -1), true);
              max.From(StartOfTheMonth(current), true);
            end;
          actMarkOlderThanOneDay..actMarkOlderThanOneYear:
            begin
              min.Value := 1; // = 1 second after Jesus' birth = not <> 0
              max.From(NowUTC - DIFFTIME[aAction], true);
            end;
        else
          exit;
        end;
        for i := 1 to RowCount do
        begin
          v := Table.GetAsInt64(i, fFieldIndexTimeLogForMark);
          if (v >= min.Value) and
             (v <= max.Value) then
            Marked[i] := true;
        end;
      end;
    end;
  TDrawGrid(Owner).Invalidate; // refresh screen
end;

function TOrmTableToGrid.GetMarkAvailable: boolean;
var
  i: PtrInt;
begin
  result := fMarkAllowed and
            (fMarked <> nil);
  if not result then
    exit;
  for i := 0 to Table.RowCount - 1 do // very any bit is realy set
    if GetBitPtr(pointer(fMarked), i) then
      exit;
  result := false;
end;

function TOrmTableToGrid.GetMarkedIsOnlyCurrrent: boolean;
begin
  with TDrawGrid(Owner) do
    result := fMarkAllowed and
              (fMarked <> nil) and
              Marked[Row] and
              (GetBitsCount(fMarked[0], RowCount) = 1);
end;

function TOrmTableToGrid.GetMarkedTotalCount: integer;
begin
  with TDrawGrid(Owner) do
    if not fMarkAllowed or
       (fMarked = nil) then
      result := 0
    else
      result := GetBitsCount(fMarked[0], RowCount);
end;

function TOrmTableToGrid.ExpandRowAsString(Row: PtrInt; Model: TOrmModel): string;
var
  f, i: PtrInt;
  s: string; // generic VCL-ready string
begin
  result := '';
  if (self = nil) or
     (PtrUInt(Row) > PtrUInt(Table.RowCount)) or
     (Table.FieldCount <= 0) then
    exit;
  for f := 0 to Table.FieldCount - 1 do
  begin
    if (not Assigned(OnValueText)) or
       (not OnValueText(Table, f, Row, s)) then
      Table.ExpandAsString(Row, f, Model, s);
    i := pos(#13, s); // trim multi-line s to first line
    if i > 0 then
      SetLength(s, i - 1);
    if (f > 0) and
       (s <> '') then
      s := ' ' + s;
    result := result + s;
  end;
end;

procedure TOrmTableToGrid.OnTableUpdate(State: TOnTableUpdateState);
begin
  if (self = nil) or
     (fMarked = nil) then
    exit; // wrong parameters
  case State of
    tusPrepare:      // save current marked entries
      if fMarked <> nil then
      begin
        Table.IDArrayFromBits(fMarked[0], fOnTableUpdateID);
        exit; // don't Finalize(fOnTableUpdateID)
      end;
    tusChanged:      // restore marked entries
      if fOnTableUpdateID <> nil then
        Table.IDArrayToBits(fMarked[0], fOnTableUpdateID);
  end;
  // tusNoChange or tusChanged: release IDs memory
  if fOnTableUpdateID <> nil then
    Finalize(fOnTableUpdateID);
end;

function TOrmTableToGrid.GetMarkedBits: pointer;
begin
  result := fMarked;
end;

function TOrmTableToGrid.GetDrawGrid: TDrawGrid;
begin
  if self = nil then
    result := nil
  else
    result := TDrawGrid(Owner);
end;

function TOrmTableToGrid.GetFieldIndexTimeLogForMark: integer;
var
  f: PtrInt;
begin
  if Self = nil then
  begin
    result := -1;
    exit;
  end;
  if fFieldIndexTimeLogForMark = -2 then
  begin
    // first call: initialize the cached value
    fFieldIndexTimeLogForMark := -1;
    for f := 0 to Table.FieldCount - 1 do
      if Table.FieldType(f) = oftTimeLog then
      begin
        fFieldIndexTimeLogForMark := f;
        break;
      end;
  end;
  result := fFieldIndexTimeLogForMark;
end;


{************ Fill a TStringGrid from ORM results }

procedure FillStringGrid(Source: TOrmTable; Dest: TStringGrid; Model: TOrmModel);
var
  col, row: PtrInt;
  s: string;
begin
  if (Source = nil) or
     (Dest = nil) then
    exit; // avoid GPF
  Dest.ColCount := Source.FieldCount;
  Dest.RowCount := Source.RowCount + 1;
  for row := 0 to Source.RowCount + 1 do
    for col := 0 to Source.FieldCount-1 do
    begin
      Source.ExpandAsString(row, col, Model, s); // will do all the magic
      Dest.Cells[col, row] := s;
    end;
end;


end.

