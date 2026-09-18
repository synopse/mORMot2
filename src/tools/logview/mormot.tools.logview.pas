/// Main Form of the mORMot LogView Visual Tool
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.tools.logview;

{
  *****************************************************************************

   Main Form of the TSynLog .log Files Visualizer
    - Browse, Filter and Search Events, per Thread and per Day
    - Methods Profiler with Merged or per-Call Timing
    - HTTP Server Mode Displaying Remote Logs Echoed from mORMot Clients

   Ported from the mORMot 1 LogView sample - FPC/Lazarus LCL only for now

  *****************************************************************************
}

interface

{$I ..\..\mormot.defines.inc}

uses
  sysutils,
  classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  CheckLst,
  Menus,
  ExtCtrls,
  Grids,
  ComCtrls,
  ShellCtrls,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.core.log,
  mormot.core.perf,
  mormot.rest.core,
  mormot.rest.http.server,
  mormot.tools.logview.remote;

type
  /// a row received by the HTTP server thread, with its sender
  TReceivedRow = record
    Text: RawUtf8;
    Received: RawUtf8; // TRemoteLogNormalizer.NowStamp at reception
    RemoteIP: RawUtf8;
    Connection: TRestConnectionID;
  end;
  TReceivedRowDynArray = array of TReceivedRow;

  { TMainLogView }

  TMainLogView = class(TForm)
    PanelLeft: TPanel;
    PanelThread: TPanel;
    PanelBottom: TPanel;
    BtnBrowse: TButton;
    EventsList: TCheckListBox;
    FilterMenu: TPopupMenu;
    EditSearch: TEdit;
    BtnSearchNext: TButton;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    BtnStats: TButton;
    BtnMapSearch: TButton;
    MergedProfile: TCheckBox;
    ProfileGroup: TRadioGroup;
    ImageLogo: TImage;
    List: TDrawGrid;
    ProfileList: TDrawGrid;
    ThreadGroup: TGroupBox;
    BtnThreadNext: TButton;
    ThreadListBox: TCheckListBox;
    Splitter1: TSplitter;
    BtnThreadShow: TButton;
    PanelBrowse: TPanel;
    Directory: TShellTreeView;
    Files: TShellListView;
    Splitter4: TSplitter;
    ListMenu: TPopupMenu;
    ListMenuCopy: TMenuItem;
    BtnSearchPrevious: TButton;
    btnServerLaunch: TButton;
    lblServerRoot: TLabel;
    edtServerRoot: TEdit;
    lblServerPort: TLabel;
    edtServerPort: TEdit;
    tmrRefresh: TTimer;
    btnListClear: TButton;
    btnListSave: TButton;
    dlgSaveList: TSaveDialog;
    pnlThreadBottom: TPanel;
    lblThreadName: TLabel;
    btnThread0: TButton;
    btnThread1: TButton;
    btnThreadAll: TButton;
    btnThreadDown: TButton;
    btnThreadUp: TButton;
    lstDays: TListBox;
    MemoBottom: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure EventsListClickCheck(Sender: TObject);
    procedure BtnSearchNextClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListClick(Sender: TObject);
    procedure ProfileListClick(Sender: TObject);
    procedure ListDblClick(Sender: TObject);
    procedure BtnStatsClick(Sender: TObject);
    procedure BtnMapSearchClick(Sender: TObject);
    procedure MergedProfileClick(Sender: TObject);
    procedure ProfileGroupClick(Sender: TObject);
    procedure ImageLogoClick(Sender: TObject);
    procedure EventsListDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ListDrawCell(Sender: TObject; ACol, ARow: integer;
      ARect: TRect; State: TGridDrawState);
    procedure ProfileListDrawCell(Sender: TObject; ACol, ARow: integer;
      ARect: TRect; State: TGridDrawState);
    procedure EventsListDblClick(Sender: TObject);
    procedure BtnThreadNextClick(Sender: TObject);
    procedure BtnThreadShowClick(Sender: TObject);
    procedure ThreadListBoxDblClick(Sender: TObject);
    procedure BtnThreadClick(Sender: TObject);
    procedure ThreadListBoxClickCheck(Sender: TObject);
    procedure BtnBrowseClick(Sender: TObject);
    procedure FilesClick(Sender: TObject);
    procedure ListMenuCopyClick(Sender: TObject);
    procedure btnServerLaunchClick(Sender: TObject);
    procedure tmrRefreshTimer(Sender: TObject);
    procedure btnListClearClick(Sender: TObject);
    procedure btnListSaveClick(Sender: TObject);
    procedure ThreadListBoxClick(Sender: TObject);
    procedure lstDaysDblClick(Sender: TObject);
    procedure PanelLeftResize(Sender: TObject);
    procedure btnThreadDownClick(Sender: TObject);
    procedure btnThreadUpClick(Sender: TObject);
  protected
    fLog: TSynLogFileView;
    fMainCaption: string;
    fMenuFilterAll: TMenuItem;
    fLogUncompressed: TMemoryStream;
    fThreadNames: TRawUtf8DynArray;
    fDays: TDateTimeDynArray;
    fRemoteLogService: TRestHttpRemoteLogServer;
    fPanelThreadVisible: boolean;
    // rows received by the HTTP server thread, applied by the UI thread
    fReceivedSafe: TLightLock;
    fReceived: TReceivedRowDynArray;
    fReceivedCount: integer;
    fNormalizer: TRemoteLogNormalizer; // used by the UI thread only
    fHiddenThreadRows: boolean; // ApplyReceived() appended a filtered thread row
    fRemoteStarted: RawUtf8; // UTC 'yyyy-mm-dd hh:mm:ss' of the current capture
    procedure SetLogFileName(const Value: TFileName);
    procedure SetListColumns(aLogFormat: boolean);
    procedure LayoutLeftPanel;
    procedure SetListItem(Index: integer; const search: RawUtf8 = '');
    procedure BtnFilterMenu(Sender: TObject);
    procedure ThreadListCheckRefresh;
    procedure ThreadListNameRefresh(Index: integer);
    procedure ReceivedOne(const Text: RawUtf8;
      ConnectionID: TRestConnectionID; const RemoteIP: RawUtf8);
    function ApplyReceived: boolean;
    procedure RefreshRemoteThreads;
    procedure ClearReceived;
    procedure StopRemoteLog;
    function RowToIndex(aRow: integer): integer;
    function VisibleRows: integer;
  public
    property LogFileName: TFileName
      write SetLogFileName;
  end;


var
  MainLogView: TMainLogView;


implementation

{$R *.lfm}

uses
  LCLIntf,
  LCLType,
  LazUTF8,
  Themes,
  Clipbrd;

resourcestring
  sEnterAddress = 'Enter a relative hexadecimal address (RVA):';
  sAddressNotFound = 'No symbol found at this address';
  sMabOnly = 'Please select a .mab file - use the mab tool to convert a .map or .dbg';
  sStopRemote = 'Stop the remote logging server, and discard the received rows?';
  sCompressFailed = 'Unable to create %s'#13#10'The uncompressed log is kept as %s';
  sStats = #13#10 +
    '%s'#13#10'%s'#13#10#13#10 +
    'Started: %s'#13#10'Closed:  %s'#13#10'Time elapsed: %d.%s'#13#10 +
    'Events: %d'#13#10'Methods: %d'#13#10'Threads: %d'#13#10'Size: %s'#13#10#13#10 +
    'Executable'#13#10'----------'#13#10#13#10'Name: %s%s'#13#10 +
    'Version: %s'#13#10'Build Date: %s'#13#10'Framework: %s'#13#10#13#10 +
    'Host'#13#10'----'#13#10#13#10'Computer: %s'#13#10 +
    'User: %s'#13#10'CPU: %s%s'#13#10'OS: %s'#13#10#13#10 +
    'Events'#13#10'------'#13#10#13#10;
  sNoFile = 'No File';
  sInvalidFile = 'Invalid File';
  sRemoteLog = 'Remote Log';
  sWindowsStats = 'Windows %s (service pack %d)'#13#10'Wow64: %s';
  sTimeInfo = '%d lines - time elapsed: %s';

const
  TIME_FORMAT = 'hh:mm:ss.zzz';

function MonospaceFontName: string;
const
  NAMES: array[0..4] of string = (
    'Consolas', 'Menlo', 'DejaVu Sans Mono', 'Liberation Mono', 'Courier New');
var
  i: PtrInt;
begin
  for i := 0 to high(NAMES) do
    if Screen.Fonts.IndexOf(NAMES[i]) >= 0 then
    begin
      result := NAMES[i];
      exit;
    end;
  result := 'Monospace';
end;


{ TMainLogView }

procedure TMainLogView.SetLogFileName(const Value: TFileName);
var
  e: TSynLogLevel;
  i: integer;
begin
  if (Value <> '') and
     (GetFileNameExtIndex(Value, 'log,synlz,txt') < 0) then
    exit;
  StopRemoteLog; // don't append remote rows to a log file or an empty view
  FreeAndNil(fLog);
  FreeAndNil(fLogUncompressed);
  ThreadListBox.Clear;
  List.RowCount := 0;
  EventsList.Items.Clear;
  if FileExists(Value) then
  begin
    Screen.Cursor := crHourGlass;
    EventsList.Items.BeginUpdate;
    try
      if SameText(ExtractFileExt(Value), '.synlz') then
      begin
        fLogUncompressed := AlgoSynLZ.StreamUnCompress(Value, LOG_MAGIC, {hash32=}true);
        if fLogUncompressed <> nil then
        begin
          fLog := TSynLogFileView.Create(
            fLogUncompressed.Memory, fLogUncompressed.Size);
          fLog.FileName := Value;
        end;
      end
      else
        fLog := TSynLogFileView.Create(Value);
      if fLog = nil then
        // invalid .synlz content: continue with the empty view state below
        Caption := fMainCaption + sInvalidFile + ' ' + ExpandFileName(Value)
      else
      begin
        Caption := fMainCaption + ExpandFileName(Value);
        SetListColumns(fLog.EventLevel <> nil); // plain text if not a TSynLog
      end;
      if (fLog <> nil) and
         (fLog.EventLevel <> nil) then
      begin
        for e := succ(sllNone) to high(e) do
          if e in fLog.EventLevelUsed then
            EventsList.Items.AddObject(ToCaption(e), pointer(PtrInt(ord(e))));
        for i := 1 to FilterMenu.Items.Count - 1 do
          FilterMenu.Items[i].Visible :=
            LOG_FILTER[TSynLogFilter(FilterMenu.Items[i].Tag)] *
              fLog.EventLevelUsed <> [];
      end;
    finally
      EventsList.Items.EndUpdate;
      Screen.Cursor := crDefault;
    end;
  end
  else
    Caption := fMainCaption + sNoFile;
  ThreadGroup.Visible := (fLog <> nil) and
                         (fLog.EventThread <> nil);
  lblThreadName.Caption := '';
  if fPanelThreadVisible and
     not ThreadGroup.Visible then
    BtnThreadShowClick(nil); // close the thread panel of the previous log
  if ThreadGroup.Visible then
  begin
    fThreadNames := fLog.ThreadNames(-1);
    ThreadListBox.Items.BeginUpdate;
    try
      ThreadListBox.Items.Clear;
      for i := 0 to fLog.ThreadsCount - 1 do
      begin
        ThreadListBox.Items.Add(Utf8ToString(fThreadNames[i]));
        ThreadListBox.Checked[i] := true;
      end;
    finally
      ThreadListBox.Items.EndUpdate;
    end;
  end;
  lstDays.Visible := (fLog <> nil) and
                     (fLog.DayChangeIndex <> nil) and
                     (fLog.DayCount <> nil);
  if lstDays.Visible then
  begin
    fLog.GetDays(fDays);
    lstDays.Items.BeginUpdate;
    try
      lstDays.Items.Clear;
      for i := 0 to high(fDays) do
        lstDays.Items.Add(Format('%s (%d rows)',
          [DateToStr(fDays[i]), fLog.DayCount[i]]));
    finally
      lstDays.Items.EndUpdate;
    end;
    lstDays.ItemIndex := 0;
  end;
  ProfileGroup.ItemIndex := 0;
  MergedProfile.Checked := false;
  BtnFilterMenu(fMenuFilterAll);
  EventsList.Visible := (fLog <> nil) and
                        (fLog.EventLevel <> nil); // no event for plain text
  ProfileGroup.Visible := (fLog <> nil) and
                          (fLog.LogProcCount <> 0);
  MergedProfile.Visible := ProfileGroup.Visible;
  BtnStats.Visible := (fLog <> nil) and
                      (fLog.EventLevel <> nil);
  BtnMapSearch.Visible := fLog <> nil;
  EditSearch.Visible := fLog <> nil;
  if EditSearch.Visible and
     EditSearch.CanFocus then
    EditSearch.SetFocus;
  BtnSearchNext.Visible := fLog <> nil;
  BtnSearchPrevious.Visible := fLog <> nil;
  lblServerRoot.Visible := fLog = nil;
  lblServerPort.Visible := fLog = nil;
  edtServerRoot.Visible := fLog = nil;
  edtServerPort.Visible := fLog = nil;
  btnServerLaunch.Visible := fLog = nil;
  btnListClear.Hide;
  btnListSave.Hide;
  List.Visible := fLog <> nil;
  LayoutLeftPanel;
  EventsListClickCheck(nil);
end;

procedure TMainLogView.LayoutLeftPanel;
var
  y, gap, h, w: integer;
begin
  // mORMot 1 stacked controls with fixed pixel offsets, which overflowed the
  // panel with current fonts and DPI: now stack by actual heights, and let
  // the events list scroll instead of pushing everything out of sight
  if csLoading in ComponentState then
    exit;
  gap := Scale96ToForm(8);
  // fixed pixel widths did clip the captions e.g. with the GTK fonts
  w := PanelLeft.ClientWidth - 2 * EventsList.Left;
  BtnBrowse.SetBounds(EventsList.Left, BtnBrowse.Top, w, BtnBrowse.Height);
  btnServerLaunch.SetBounds(EventsList.Left, btnServerLaunch.Top, w,
    btnServerLaunch.Height);
  edtServerRoot.Width := w;
  edtServerPort.Width := w;
  EventsList.Width := w;
  ProfileGroup.Width := w;
  MergedProfile.Left := EventsList.Left;
  MergedProfile.Width := w;
  BtnStats.Width := w div 2 - 2;
  BtnMapSearch.Left := BtnStats.Left + BtnStats.Width + 4;
  BtnMapSearch.Width := BtnStats.Width;
  ThreadGroup.Width := w;
  lstDays.Width := w;
  y := EventsList.Top;
  if EventsList.Visible then
  begin
    h := EventsList.ItemHeight;
    if EventsList.Count > 0 then // a widgetset may use taller rows, e.g. GTK
      with EventsList.ItemRect(0) do
        h := MaxPtrInt(h, Bottom - Top);
    h := EventsList.Count * h + gap;
    if h > PanelLeft.ClientHeight div 3 then
      h := PanelLeft.ClientHeight div 3;
    EventsList.Height := h;
    inc(y, h + gap);
  end;
  if btnListClear.Visible then
  begin
    btnListClear.Top := y;
    btnListClear.Width := EventsList.Width div 2 - 2;
    btnListSave.Top := y;
    btnListSave.Width := btnListClear.Width;
    btnListSave.Left := btnListClear.Left + btnListClear.Width + 4;
    inc(y, btnListClear.Height + gap);
  end;
  if ProfileGroup.Visible then
  begin
    ProfileGroup.Top := y;
    inc(y, ProfileGroup.Height + 2);
    MergedProfile.Top := y;
    inc(y, MergedProfile.Height + gap);
  end;
  if BtnStats.Visible or
     BtnMapSearch.Visible then
  begin
    BtnStats.Top := y;
    BtnMapSearch.Top := y;
    inc(y, BtnStats.Height + gap);
  end;
  if ThreadGroup.Visible then
  begin
    ThreadGroup.Top := y;
    inc(y, ThreadGroup.Height + gap);
  end;
  ImageLogo.Visible := y < ImageLogo.Top;
  if lstDays.Visible then
  begin
    lstDays.Top := y;
    if ImageLogo.Visible then
      h := ImageLogo.Top - gap - y
    else
      h := PanelLeft.ClientHeight - gap - y;
    lstDays.Height := MaxPtrInt(h, EventsList.ItemHeight * 2);
  end;
end;

procedure TMainLogView.SetListColumns(aLogFormat: boolean);
var
  e: TSynLogLevel;
  w: integer;
begin
  if not aLogFormat then
  begin
    List.ColCount := 1;
    List.ColWidths[0] := 2000;
    exit;
  end;
  List.ColCount := 4;
  List.ColWidths[0] := Canvas.TextWidth('00:00:00.000') + 12; // GetCell() format
  w := 0;
  for e := succ(sllNone) to high(e) do
    w := MaxPtrInt(w, Canvas.TextWidth(ToCaption(e)));
  List.ColWidths[1] := w + 12;
  List.ColWidths[2] := Canvas.TextWidth('00000') + 12;
  List.ColWidths[3] := 2000;
end;

procedure TMainLogView.FormCreate(Sender: TObject);
var
  f: TSynLogFilter;
  o: TLogProcSortOrder;
  m: TMenuItem;
begin
  fMainCaption := Format(Caption, [SYNOPSE_FRAMEWORK_VERSION]) + ' ';
  for f := low(f) to high(f) do
  begin
    m := TMenuItem.Create(self);
    m.Caption := ToCaption(f);
    m.Tag := ord(f);
    m.OnClick := BtnFilterMenu;
    if f = lfAll then
      fMenuFilterAll := m;
    FilterMenu.Items.Add(m);
  end;
  for o := low(o) to high(o) do
    ProfileGroup.Items.AddObject(GetCaptionFromEnum(
      TypeInfo(TLogProcSortOrder), ord(o)), TObject(PtrInt(ord(o))));
  ProfileList.Hide;
  MemoBottom.Font.Name := MonospaceFontName;
  MemoBottom.Text := '';
end;

procedure TMainLogView.FormDestroy(Sender: TObject);
begin
  // stop receiving before releasing what ReceivedOne() may still reach
  FreeAndNil(fRemoteLogService);
  FreeAndNil(fLog);
  FreeAndNil(fLogUncompressed);
  FreeAndNil(fNormalizer);
end;

procedure TMainLogView.FormShow(Sender: TObject);
var
  cmdline: TFileName;
  h: integer;
begin
  // mORMot 1 fixed pixel sizes did clip the text with current fonts and DPI
  h := Canvas.TextHeight('Wg') + 4;
  List.DefaultRowHeight := h;
  ProfileList.DefaultRowHeight := h;
  EventsList.ItemHeight := h;
  // set here, since the LCL DPI scaling resets widths assigned in FormCreate
  ProfileList.ColWidths[0] := Canvas.TextWidth('000.000.000') + 12;
  ProfileList.ColWidths[1] := 2000;
  PanelThread.Width := 300;
  // the widest caption of the left panel, with its check mark and margins
  h := Canvas.TextWidth(MergedProfile.Caption) + Scale96ToForm(32) +
       2 * EventsList.Left;
  if h > PanelLeft.Width then
  begin
    PanelLeft.Constraints.MinWidth := h;
    PanelLeft.Width := h;
  end;
  // always set the empty view state first: the .lfm shows e.g. the profiler,
  // and SetLogFileName() ignores a directory or an unsupported file extension
  Directory.Path := Executable.ProgramFilePath;
  LogFileName := '';
  if ParamCount > 0 then
  begin
    cmdline := ParamStr(1);
    if DirectoryExists(cmdline) then
    begin
      BtnBrowseClick(nil);
      Directory.Path := cmdline;
    end
    else
      LogFileName := cmdline;
  end;
  WindowState := wsMaximized;
end;

procedure TMainLogView.BtnFilterMenu(Sender: TObject);
var
  f: TSynLogFilter;
  i: integer;
begin
  if not Sender.InheritsFrom(TMenuItem) then
    exit;
  f := TSynLogFilter(TMenuItem(Sender).Tag);
  for i := 0 to EventsList.Count - 1 do
    EventsList.Checked[i] :=
      TSynLogLevel(PtrInt(EventsList.Items.Objects[i])) in LOG_FILTER[f];
  EventsListClickCheck(nil);
end;

procedure TMainLogView.EventsListDrawItem(Control: TWinControl;
  Index: integer; ARect: TRect; State: TOwnerDrawState);
var
  e: TSynLogLevel;
  {$ifdef OSWINDOWS}
  box: TRect;
  details: TThemedElementDetails;
  {$endif OSWINDOWS}
begin
  if Index < 0 then
    exit;
  e := TSynLogLevel(PtrInt(EventsList.Items.Objects[Index]));
  with EventsList.Canvas do
  begin
    Brush.Color := LOG_LEVEL_COLORS[false, e];
    Font.Color := LOG_LEVEL_COLORS[true, e];
    FillRect(ARect);
    {$ifdef OSWINDOWS}
    // the LCL does not draw the check box of an owner drawn item on Windows
    box := ARect;
    inc(box.Left);
    box.Right := ARect.Left + (ARect.Bottom - ARect.Top) - 2;
    if EventsList.Checked[Index] then
      details := ThemeServices.GetElementDetails(tbCheckBoxCheckedNormal)
    else
      details := ThemeServices.GetElementDetails(tbCheckBoxUncheckedNormal);
    ThemeServices.DrawElement(Handle, details, box);
    ARect.Left := box.Right;
    {$endif OSWINDOWS}
    TextRect(ARect, ARect.Left + 4, ARect.Top, ToCaption(e));
  end;
end;

procedure TMainLogView.EventsListClickCheck(Sender: TObject);
var
  i, n: integer;
  levels: TSynLogLevels;
begin
  if (fLog <> nil) and
     (fLog.EventLevel <> nil) then
  begin
    levels := [];
    for i := 0 to EventsList.Count - 1 do
      if EventsList.Checked[i] then
        include(levels, TSynLogLevel(PtrInt(EventsList.Items.Objects[i])));
    fLog.Events := levels;
  end;
  i := -1;
  if (fLog <> nil) and
     (fLog.EventLevel <> nil) and
     (fLog.Events <> []) then // Select() would keep its previous selection
    i := fLog.Select(List.Row);
  n := VisibleRows;
  if (n > 0) and
     (List.Row >= n) then
    List.Row := 0; // avoid "Grid Out Of Range" when shrinking RowCount
  List.RowCount := n;
  if cardinal(i) < cardinal(n) then
    List.Row := i;
  SetListItem(List.Row);
  if List.Visible then
  begin
    List.Repaint;
    ListClick(nil);
  end;
end;

procedure TMainLogView.EventsListDblClick(Sender: TObject);
var
  i: integer;
begin
  if (fLog = nil) or
     (fLog.EventLevel = nil) then // plain text file does not handle this
    exit;
  i := EventsList.ItemIndex;
  if i >= 0 then
    i := fLog.SearchNextEvent(
      TSynLogLevel(PtrInt(EventsList.Items.Objects[i])), List.Row);
  if i >= 0 then
    SetListItem(i);
end;

procedure TMainLogView.ListDrawCell(Sender: TObject; ACol, ARow: integer;
  ARect: TRect; State: TGridDrawState);
var
  txt: string;
  inverted: boolean;
  level: TSynLogLevel;
begin
  with List.Canvas do
  begin
    if fLog = nil then
    begin
      FillRect(ARect);
      exit;
    end;
    txt := fLog.GetCell(ACol, ARow, level);
    if fLog.EventLevel <> nil then
    begin
      Brush.Style := bsClear;
      if cardinal(ARow) < cardinal(fLog.SelectedCount) then
      begin
        inverted := (gdFocused in State) or
                    (gdSelected in State);
        if inverted then
          Brush.Color := clBlack
        else
          Brush.Color := LOG_LEVEL_COLORS[inverted, level];
        Font.Color := LOG_LEVEL_COLORS[not inverted, level];
      end
      else
      begin
        Brush.Color := clLtGray;
        FillRect(ARect);
        exit;
      end;
    end;
    FillRect(ARect);
    TextRect(ARect, ARect.Left + 4, ARect.Top, txt);
  end;
end;

procedure TMainLogView.BtnSearchNextClick(Sender: TObject);
var
  ndx: integer;
  s: RawUtf8;
begin
  if (fLog = nil) or
     (List.RowCount = 0) then
    exit;
  s := UpperCase(StringToUtf8(EditSearch.Text));
  Screen.Cursor := crHourGlass;
  try
    if Sender = BtnSearchPrevious then
      ndx := fLog.SearchPreviousText(s, List.Row)
    else if Sender = EditSearch then
      ndx := fLog.SearchNextText(s, List.Row, 0)
    else
      ndx := fLog.SearchNextText(s, List.Row, 1); // e.g. BtnSearchNext
    if ndx >= 0 then
      SetListItem(ndx, s);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainLogView.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F3 then
  begin
    if Shift = [] then
      BtnSearchNextClick(nil)
    else if ssShift in Shift then
      BtnSearchNextClick(BtnSearchPrevious)
    else
      exit;
    if List.CanFocus then
      List.SetFocus;
  end
  else if (Shift = [ssCtrl]) and
          (Key = ord('F')) and
          EditSearch.CanFocus then
    EditSearch.SetFocus;
end;

procedure TMainLogView.ProfileListClick(Sender: TObject);
var
  ndx, i: integer;
begin
  i := ProfileList.Row;
  if (fLog <> nil) and
     (cardinal(i) < cardinal(fLog.LogProcCount)) then
  begin
    ndx := fLog.LogProc[i].Index;
    i := IntegerScanIndex(pointer(fLog.Selected), fLog.SelectedCount, ndx);
    if i >= 0 then
    begin
      SetListItem(i);
      if List.CanFocus then
        List.SetFocus;
    end;
  end;
end;

procedure TMainLogView.ProfileListDrawCell(Sender: TObject; ACol,
  ARow: integer; ARect: TRect; State: TGridDrawState);
var
  tim: cardinal; // as TSynLogFileProc.Time/ProperTime, > 35 minutes as integer
  s: string;
begin
  if (fLog <> nil) and
     (cardinal(ARow) < cardinal(fLog.LogProcCount)) then
    with fLog.LogProc[ARow] do
    begin
      if ACol = 0 then
      begin
        if fLog.LogProcOrder = soByProperTime then
          tim := ProperTime
        else
          tim := Time;
        s := string(MicroSecToString(tim));
      end
      else
        s := fLog.EventString(Index);
      ProfileList.Canvas.TextRect(ARect, ARect.Left + 4, ARect.Top, s);
    end;
end;

procedure TMainLogView.ListClick(Sender: TObject);
var
  i, ndx, found: integer;
  selection: TGridRect;
  elapsed: TDateTime;
  s, tim: string;
begin
  if fLog = nil then
  begin
    MemoBottom.Text := '';
    exit;
  end;
  i := List.Row;
  if cardinal(i) < cardinal(fLog.SelectedCount) then
  begin
    i := fLog.Selected[i];
    s := fLog.EventString(i, '', 0, true);
    if fPanelThreadVisible and
       (fLog.EventThread <> nil) then
    begin
      ThreadListNameRefresh(i);
      ndx := fLog.EventThread[i] - 1;
      if ndx <> ThreadListBox.ItemIndex then
      begin
        btnThread1.Caption := IntToStr(ndx + 1);
        ThreadListBox.ItemIndex := ndx;
        ThreadListBoxClick(nil);
      end;
    end;
    if lstDays.Visible then
    begin
      ndx := lstDays.ItemIndex;
      if (cardinal(ndx) < cardinal(length(fDays))) and
         (Trunc(fLog.EventDateTime(i)) <> fDays[ndx]) then
      begin
        found := high(fLog.DayChangeIndex);
        for ndx := 1 to found do
          if fLog.DayChangeIndex[ndx] > i then
          begin
            found := ndx - 1;
            break;
          end;
        lstDays.ItemIndex := found;
      end;
    end;
  end
  else
    s := fLog.EventString(i, '', 0, true);
  selection := List.Selection;
  if selection.Bottom > selection.Top then
  begin
    elapsed := fLog.EventDateTime(RowToIndex(selection.Bottom)) -
               fLog.EventDateTime(RowToIndex(selection.Top));
    if fLog.Freq = 0 then
    begin
      DateTimeToString(tim, TIME_FORMAT, elapsed);
      s := tim + #13#10 + s;
    end
    else
    begin
      tim := IntToStr(trunc(elapsed * MSecsPerDay * 1000) mod 1000);
      s := StringOfChar('0', 3 - length(tim)) + tim + #13#10 + s;
      DateTimeToString(tim, TIME_FORMAT, elapsed);
      s := tim + '.' + s;
    end;
    s := Format(sTimeInfo, [selection.Bottom - selection.Top + 1, s]);
  end;
  if Pos(#9, s) > 0 then
    s := StringReplace(s, #9, ' ', [rfReplaceAll]);
  MemoBottom.Text := s;
end;

procedure TMainLogView.ListDblClick(Sender: TObject);
var
  ndx: integer;
begin
  if fLog = nil then
    exit;
  ndx := fLog.SearchEnterLeave(List.Row);
  if ndx >= 0 then
    SetListItem(ndx);
end;

procedure TMainLogView.BtnThreadNextClick(Sender: TObject);
begin
  if fLog <> nil then
    SetListItem(fLog.SearchNextThread(List.Row));
end;

procedure TMainLogView.btnThreadDownClick(Sender: TObject);
begin
  if fLog <> nil then
    SetListItem(fLog.SearchNextSameThread(List.Row));
end;

procedure TMainLogView.btnThreadUpClick(Sender: TObject);
begin
  if fLog <> nil then
    SetListItem(fLog.SearchPreviousSameThread(List.Row));
end;

procedure TMainLogView.ThreadListBoxDblClick(Sender: TObject);
var
  id: cardinal;
begin
  id := ThreadListBox.ItemIndex;
  if (fLog <> nil) and
     (id < fLog.ThreadsCount) then
    SetListItem(fLog.SearchThread(id + 1, List.Row));
end;

procedure TMainLogView.BtnThreadShowClick(Sender: TObject);
begin
  fPanelThreadVisible := not fPanelThreadVisible;
  PanelThread.Visible := fPanelThreadVisible;
  Splitter3.Visible := fPanelThreadVisible;
  if fPanelThreadVisible then
  begin
    PanelThread.Left := ProfileList.Left + ProfileList.Width;
    Splitter3.Left := PanelThread.Left + PanelThread.Width;
    ListClick(nil);
  end
  else
    btnThread1.Caption := '1';
  btnThread0.Enabled := fPanelThreadVisible;
  btnThread1.Enabled := fPanelThreadVisible;
  btnThreadAll.Enabled := fPanelThreadVisible;
end;

procedure TMainLogView.BtnStatsClick(Sender: TObject);
var
  m: TMemo;
  f: TForm;
  s, ostext: string;
  counts: array[TSynLogLevel] of integer;
  i: integer;
  p: PUtf8Char;
  feat, line, name, value: RawUtf8;
  intel: TIntelCpuFeatures;
  arm32: TArm32HwCaps;
  arm64: TArm64HwCaps;
  closed, elapsed: TDateTime;
begin
  if (fLog = nil) or
     (fLog.EventLevel = nil) then
    exit;
  f := TForm.Create(Application);
  try
    f.Caption := fMainCaption + BtnStats.Caption;
    f.Font.Name := MonospaceFontName;
    f.Position := poScreenCenter;
    f.Width := 700;
    f.Height := 600;
    m := TMemo.Create(f);
    m.Parent := f;
    m.Align := alClient;
    m.ScrollBars := ssVertical;
    m.WordWrap := true;
    m.ReadOnly := true;
    with fLog do
    begin
      if InstanceName <> '' then
        s := ' / ' + Utf8ToString(InstanceName);
      if OS <> wUnknown then
        ostext := Format(sWindowsStats, [string(WINDOWS_NAME[OS]),
          ServicePack, string(BOOL_STR[Wow64])])
      else
        ostext := Utf8ToString(DetailedOS);
      // mORMot 2 logs the Intel/AMD or ARM 32-bit/64-bit CPU features
      intel := IntelCPU;
      arm32 := Arm32CPU;
      arm64 := Arm64CPU;
      feat := ToText(intel, ' ');
      if feat = '' then
        feat := ToText(arm64, ' ');
      if feat = '' then
        feat := ToText(arm32, ' ');
      if feat <> '' then
        feat := '  ' + LowerCase(feat);
      closed := EventDateTime(Count - 1);
      elapsed := closed - StartDateTime;
      s := Format(sStats, [FileName, StringOfChar('-', length(FileName)),
        DateTimeToStr(StartDateTime), DateTimeToStr(closed),
        trunc(elapsed), FormatDateTime('hh:mm:ss', elapsed),
        Count, LogProcCount, ThreadsCount, string(KB(Map.Size)),
        Utf8ToString(ExecutableName), s, Utf8ToString(ExecutableVersion),
        DateTimeToStr(ExecutableDate), Utf8ToString(Framework),
        Utf8ToString(ComputerHost), Utf8ToString(RunningUser),
        Utf8ToString(CPU), Utf8ToString(feat), ostext]);
      FillCharFast(counts, SizeOf(counts), 0);
      for i := 0 to Count - 1 do
        inc(counts[EventLevel[i]]);
      for i := 0 to EventsList.Count - 1 do
        s := s + EventsList.Items[i] + ': ' + IntToStr(counts[TSynLogLevel(
          PtrInt(EventsList.Items.Objects[i]))]) + #13#10;
      p := pointer(Headers);
      while (p <> nil) and
            (p^ <> #0) do
      begin
        line := GetNextLine(p, p);
        Split(line, '=', name, value);
        if value <> '' then
          s := s + #13#10 + Utf8ToString(name) + #13#10 +
            StringOfChar('-', length(name)) + #13#10#13#10 +
            Utf8ToString(StringReplaceAll(value, #9, #13#10)) + #13#10;
      end;
    end;
    m.Text := s;
    f.ShowModal;
  finally
    f.Free;
  end;
end;

procedure TMainLogView.SetListItem(Index: integer; const search: RawUtf8);
var
  i: integer;
  s, ss: string;
begin
  if (Index < 0) or
     (fLog = nil) or
     (Index >= List.RowCount) then
    MemoBottom.Text := ''
  else
  begin
    List.Row := Index;
    if (search = '') and
       List.Visible and
       List.CanFocus then
      List.SetFocus;
    if fLog.EventLevel <> nil then
      Index := fLog.Selected[Index];
    s := fLog.EventString(Index, '', 0, true);
    if Pos(#9, s) > 0 then
      s := StringReplace(s, #9, ' ', [rfReplaceAll]);
    MemoBottom.Text := s;
    if search <> '' then
    begin
      ss := Utf8ToString(search);
      // UpperCase() only changes ASCII chars, so keeps the UTF-8 byte positions
      i := Pos(ss, SysUtils.UpperCase(s));
      if i > 0 then
      begin
        {$if defined(LCLgtk2) or defined(LCLgtk3)}
        // GTK text buffers count characters
        MemoBottom.SelStart := UTF8Length(copy(s, 1, i - 1));
        MemoBottom.SelLength := UTF8Length(ss);
        {$else}
        // Win32 EM_SETSEL, Cocoa NSRange and Qt cursors count UTF-16 code units
        MemoBottom.SelStart := length(UTF8ToUTF16(copy(s, 1, i - 1)));
        MemoBottom.SelLength := length(UTF8ToUTF16(ss));
        {$ifend}
      end;
    end;
  end;
end;

procedure TMainLogView.BtnMapSearchClick(Sender: TObject);
var
  fn: TFileName;
  addr: string;
  addrint: Int64;
  err: integer;
  loc: RawUtf8;
  dbg: TDebugFile;
  dlg: TOpenDialog;
begin
  fn := '';
  if (fLog <> nil) and
     (fLog.ExecutableName <> '') then
  begin
    // only .mab: TDebugFile would parse DWARF but not Delphi .map text with
    // FPC, and probes the folder of a .dbg - convert them with the mab tool
    fn := ChangeFileExt(ExtractFileName(Utf8ToString(fLog.ExecutableName)), '.mab');
  end;
  dlg := TOpenDialog.Create(self);
  try
    dlg.DefaultExt := '.mab';
    dlg.Filter := '*.mab|*.mab';
    if fn <> '' then
      dlg.Filter := fn + '|' + fn + '|' + dlg.Filter;
    dlg.Options := [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing];
    if not dlg.Execute then
      exit;
    if not SameText(ExtractFileExt(dlg.FileName), '.mab') then
    begin
      ShowMessage(sMabOnly); // the dialog filter does not prevent typing it
      exit;
    end;
    // TDebugFile.Create maintains a .mab cache for an executable: with an
    // existing .mab as file name, these flags make the constructor read-only
    dbg := TDebugFile.Create(dlg.FileName,
      [dfsNoMabExternalCheck, dfsNoMabSaveAtCreate]);
    try
      addr := '';
      repeat
        if not InputQuery(BtnMapSearch.Hint, sEnterAddress, addr) then
          exit;
        addr := SysUtils.Trim(addr);
        if addr = '' then
          continue;
        if addr[1] <> '$' then
          addr := '$' + addr;
        val(addr, addrint, err);
        if (err <> 0) or
           (addrint < 0) or
           (addrint > high(TDebugAddress)) then
          continue;
        loc := dbg.Lookup(addrint);
        if loc = '' then
          ShowMessage(addr + #13#10 + sAddressNotFound)
        else
          ShowMessage(addr + #13#10 + Utf8ToString(loc));
      until false;
    finally
      dbg.Free;
    end;
  finally
    dlg.Free;
  end;
end;

procedure TMainLogView.MergedProfileClick(Sender: TObject);
begin
  if (fLog = nil) or
     (fLog.LogProcCount = 0) then // TSynLogFile.SetLogProcMerged() would GPF
    exit;
  Screen.Cursor := crHourGlass;
  try
    fLog.LogProcMerged := MergedProfile.Checked;
    ProfileGroupClick(nil);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainLogView.ProfileGroupClick(Sender: TObject);
var
  o: TLogProcSortOrder;
begin
  if ProfileGroup.ItemIndex < 0 then
    o := low(o)
  else
    o := TLogProcSortOrder(ProfileGroup.ItemIndex);
  if (fLog = nil) or
     (fLog.LogProcCount = 0) or
     (o = soNone) then
  begin
    Splitter1.Hide;
    ProfileList.Hide;
  end
  else
  begin
    Screen.Cursor := crHourGlass;
    try
      fLog.LogProcSort(o);
    finally
      Screen.Cursor := crDefault;
    end;
    ProfileList.RowCount := fLog.LogProcCount;
    ProfileList.Show;
    Splitter1.Left := ProfileList.Left + ProfileList.Width;
    Splitter1.Show;
    ProfileList.Repaint;
  end;
end;

procedure TMainLogView.ImageLogoClick(Sender: TObject);
begin
  OpenURL('https://synopse.info');
end;

procedure TMainLogView.BtnThreadClick(Sender: TObject);
begin
  if fLog = nil then
    exit;
  fLog.SetAllThreads(Sender = btnThreadAll);
  if Sender = btnThread1 then
    fLog.Threads[ThreadListBox.ItemIndex + 1] := true;
  ThreadListCheckRefresh;
  EventsListClickCheck(nil);
end;

procedure TMainLogView.ThreadListCheckRefresh;
var
  i: integer;
begin
  for i := 0 to ThreadListBox.Count - 1 do
    ThreadListBox.Checked[i] := fLog.Threads[i + 1];
end;

procedure TMainLogView.ThreadListBoxClickCheck(Sender: TObject);
var
  i: integer;
begin
  i := ThreadListBox.ItemIndex;
  if (fLog <> nil) and
     (i >= 0) then
  begin
    fLog.Threads[i + 1] := ThreadListBox.Checked[i];
    EventsListClickCheck(nil);
  end;
end;

procedure TMainLogView.ThreadListNameRefresh(Index: integer);
var
  names: TRawUtf8DynArray;
  i: integer;
begin
  if fRemoteLogService <> nil then
    exit; // remote mode: RefreshRemoteThreads() maintains the names
  names := fLog.ThreadNames(Index);
  if names = nil then
    exit;
  for i := 0 to MinPtrInt(high(names),
                 MinPtrInt(high(fThreadNames), ThreadListBox.Count - 1)) do
    if names[i] <> fThreadNames[i] then
      ThreadListBox.Items[i] := Utf8ToString(names[i]);
  fThreadNames := names;
end;

procedure TMainLogView.ThreadListBoxClick(Sender: TObject);
var
  i: integer;
begin
  i := ThreadListBox.ItemIndex;
  if cardinal(i) < cardinal(length(fThreadNames)) then
  begin
    lblThreadName.Caption := Utf8ToString(fThreadNames[i]);
    btnThread1.Caption := IntToStr(i + 1);
  end;
end;

procedure TMainLogView.BtnBrowseClick(Sender: TObject);
begin
  PanelBrowse.Visible := (Sender = nil) or
                         not PanelBrowse.Visible;
  Splitter4.Visible := PanelBrowse.Visible;
  if PanelBrowse.Visible then
    Splitter4.Left := PanelBrowse.Left + PanelBrowse.Width;
end;

procedure TMainLogView.FilesClick(Sender: TObject);
var
  fn: TFileName;
begin
  if Files.Selected = nil then
    exit;
  fn := Files.GetPathFromItem(Files.Selected);
  if (fLog <> nil) and
     (fLog.FileName = fn) then
    exit;
  // opening a file stops the remote server and drops its unsaved rows
  if fRemoteLogService <> nil then
    tmrRefreshTimer(nil); // also show the rows still waiting in the queue
  if (fRemoteLogService <> nil) and
     (fLog <> nil) and
     (fLog.Count > 1) and
     (MessageDlg(sStopRemote, mtConfirmation, [mbYes, mbNo], 0) <> mrYes) then
    exit;
  LogFileName := fn;
end;

procedure TMainLogView.ListMenuCopyClick(Sender: TObject);
var
  selection: TGridRect;
  i: integer;
  s: string;
begin
  if (fLog = nil) or
     (List.RowCount = 0) then
    exit;
  selection := List.Selection;
  for i := selection.Top to selection.Bottom do
    if fLog.EventLevel = nil then
      // GetLineForClipboard() expects the EventLevel[] of a TSynLog file
      s := s + fLog.Strings[i] + sLineBreak
    else
      s := s + fLog.GetLineForClipboard(i) + sLineBreak;
  Clipboard.AsText := s;
end;

procedure TMainLogView.btnServerLaunchClick(Sender: TObject);
var
  e: TSynLogLevel;
  i: integer;
begin
  if fRemoteLogService = nil then
  try
    fRemoteLogService := TRestHttpRemoteLogServer.CreateWithSender(
      StringToUtf8(edtServerRoot.Text), StrToInt(edtServerPort.Text), ReceivedOne);
    Caption := fMainCaption + sRemoteLog;
  except
    on E: Exception do
    begin
      ShowMessage(E.Message);
      exit;
    end;
  end;
  if fLog = nil then
    fLog := TSynLogFileView.Create;
  List.DoubleBuffered := true;
  SetListColumns(true);
  fLog.Events := LOG_VERBOSE; // filtered remote log view support
  EventsList.Items.BeginUpdate;
  try
    EventsList.Items.Clear;
    for e := succ(sllNone) to high(e) do
    begin
      EventsList.Items.AddObject(ToCaption(e), pointer(PtrInt(ord(e))));
      EventsList.Checked[EventsList.Count - 1] := true;
    end;
    for i := 1 to FilterMenu.Items.Count - 1 do
      FilterMenu.Items[i].Visible := true;
  finally
    EventsList.Items.EndUpdate;
  end;
  EventsList.Show;
  lblServerRoot.Hide;
  lblServerPort.Hide;
  edtServerRoot.Hide;
  edtServerPort.Hide;
  btnServerLaunch.Hide;
  btnListClear.Show;
  btnListSave.Show;
  EditSearch.Show;
  if EditSearch.CanFocus then
    EditSearch.SetFocus;
  BtnSearchNext.Show;
  BtnSearchPrevious.Show;
  LayoutLeftPanel;
  if fNormalizer = nil then
    fNormalizer := TRemoteLogNormalizer.Create;
  if fLog.Count = 0 then
  begin
    fRemoteStarted := NowToString({expanded=}true, ' ', {utc=}true);
    // the first row fixes the layout of the view - see TRemoteLogNormalizer
    fLog.AddInMemoryLine(TRemoteLogNormalizer.Banner(FormatUtf8(
      'Remote Logging Server started on port % with root name "%"',
      [fRemoteLogService.Port, fRemoteLogService.Server.Model.Root])));
  end;
  List.RowCount := VisibleRows;
  List.Show;
  tmrRefresh.Enabled := true;
end;

procedure TMainLogView.ReceivedOne(const Text: RawUtf8;
  ConnectionID: TRestConnectionID; const RemoteIP: RawUtf8);
var
  p: PUtf8Char;
  line, stamp: RawUtf8;
begin
  // called from the HTTP server thread: just queue the rows, since
  // TSynLogFileView.AddInMemoryLine() reallocates what the UI thread draws
  stamp := TRemoteLogNormalizer.NowStamp;
  p := pointer(Text);
  fReceivedSafe.Lock;
  try
    while p <> nil do // handle multiple log rows in the incoming text
    begin
      line := GetNextLine(p, p);
      if line = '' then
        continue; // any other row is kept, even if not a TSynLog row
      if fReceivedCount = length(fReceived) then
        SetLength(fReceived, NextGrow(fReceivedCount));
      // no "with": the record fields would hide the Text/RemoteIP parameters
      fReceived[fReceivedCount].Text := line;
      fReceived[fReceivedCount].Received := stamp;
      fReceived[fReceivedCount].RemoteIP := RemoteIP;
      fReceived[fReceivedCount].Connection := ConnectionID;
      inc(fReceivedCount);
    end;
  finally
    fReceivedSafe.UnLock;
  end;
end;

function TMainLogView.ApplyReceived: boolean;
var
  received: TReceivedRowDynArray;
  rows: TRawUtf8DynArray;
  n, i, count: integer;
begin
  // called from the UI thread only
  fReceivedSafe.Lock;
  try
    n := fReceivedCount;
    received := fReceived;
    fReceived := nil;
    fReceivedCount := 0;
  finally
    fReceivedSafe.UnLock;
  end;
  result := (n <> 0) and
            (fLog <> nil) and
            (fNormalizer <> nil);
  if not result then
    exit;
  count := 0;
  for i := 0 to n - 1 do
    fNormalizer.Normalize(received[i].Text, received[i].Received,
      received[i].Connection, received[i].RemoteIP, rows, count);
  for i := 0 to count - 1 do
  begin
    fLog.AddInMemoryLine(rows[i]);
    // normalized rows have their thread column at 17: see TRemoteLogNormalizer
    if (fLog.EventThread <> nil) and
       not fLog.Threads[Chars3ToInt18(PUtf8Char(pointer(rows[i])) + 17)] then
      fHiddenThreadRows := true;
  end;
end;

procedure TMainLogView.RefreshRemoteThreads;
var
  i, n: integer;
  caption: string;
begin
  if (fLog = nil) or
     (fLog.EventThread = nil) or
     (fNormalizer = nil) then
    exit;
  // TSynLogFile.ThreadNames() would compare addresses of appended rows, so
  // use the latest names as seen by the normalizer, with their row counts
  n := fLog.ThreadsCount;
  SetLength(fThreadNames, n);
  ThreadListBox.Items.BeginUpdate;
  try
    for i := 0 to n - 1 do
    begin
      FormatUtf8('% % (% rows)', [i + 1, fNormalizer.ThreadName(i + 1),
        fLog.ThreadRows(i + 1)], fThreadNames[i]);
      caption := Utf8ToString(fThreadNames[i]);
      if i >= ThreadListBox.Count then
      begin
        ThreadListBox.Items.Add(caption);
        ThreadListBox.Checked[i] := fLog.Threads[i + 1];
      end
      else if ThreadListBox.Items[i] <> caption then
        ThreadListBox.Items[i] := caption; // only the threads with new rows
    end;
  finally
    ThreadListBox.Items.EndUpdate;
  end;
  if not ThreadGroup.Visible then
  begin
    ThreadGroup.Show;
    LayoutLeftPanel;
  end;
  // TSynLogFileView.AddInMemoryLine() only checks the event level: re-select
  // only when rows of a filtered thread were appended since the last tick
  if fHiddenThreadRows and
     (fLog.Events <> []) then
    fLog.Select(List.Row);
  fHiddenThreadRows := false;
end;

procedure TMainLogView.ClearReceived;
begin
  if fNormalizer <> nil then
    fNormalizer.Clear; // new connections and threads numbering
  fReceivedSafe.Lock;
  try
    fReceived := nil;
    fReceivedCount := 0;
  finally
    fReceivedSafe.UnLock;
  end;
end;

procedure TMainLogView.StopRemoteLog;
begin
  if fRemoteLogService = nil then
    exit;
  tmrRefresh.Enabled := false;
  FreeAndNil(fRemoteLogService); // stop receiving before dropping the queue
  ClearReceived;
  FreeAndNil(fNormalizer);
end;

function TMainLogView.VisibleRows: integer;
begin
  if fLog = nil then
    result := 0
  else if fLog.EventLevel = nil then
    result := fLog.Count // plain text
  else if fLog.Events = [] then
    result := 0 // TSynLogFileView.Select() keeps its previous selection
  else
    result := fLog.SelectedCount;
end;

function TMainLogView.RowToIndex(aRow: integer): integer;
begin
  if fLog.EventLevel <> nil then
    result := fLog.Selected[aRow]
  else
    result := aRow;
end;

procedure TMainLogView.tmrRefreshTimer(Sender: TObject);
var
  following: boolean;
  n: integer;
begin
  if not ApplyReceived then
    exit; // no new row since the last refresh
  RefreshRemoteThreads;
  // VisibleRowCount is not the viewport capacity once the rows overflow it
  following := (List.RowCount = 0) or // LeftCol: may be scrolled horizontally
               List.IsCellVisible(List.LeftCol, List.RowCount - 1);
  n := VisibleRows;
  if n <> List.RowCount then
  begin
    List.RowCount := n;
    if following and
       (n > List.VisibleRowCount) then // don't scroll a user reading above
      List.TopRow := n - List.VisibleRowCount;
  end;
  List.Invalidate;
end;

procedure TMainLogView.btnListClearClick(Sender: TObject);
begin
  ClearReceived; // rows received before the Clear click
  List.RowCount := 0;
  MemoBottom.Text := ''; // no OnClick is triggered when RowCount shrinks
  if fPanelThreadVisible then
    BtnThreadShowClick(nil);
  ThreadGroup.Hide;
  lblThreadName.Caption := '';
  ThreadListBox.Clear;
  fThreadNames := nil;
  fHiddenThreadRows := false;
  FreeAndNil(fLog);
  btnServerLaunchClick(nil);
end;

procedure TMainLogView.btnListSaveClick(Sender: TObject);
var
  fn, tmp: TFileName;
  header: RawUtf8;
begin
  if fLog = nil then
    exit;
  dlgSaveList.FileName := 'Remote ' + Utf8ToString(DateTimeToIso8601(Now, false, ' '));
  if not dlgSaveList.Execute then
    exit;
  fn := dlgSaveList.FileName;
  // StartDateTime is read from the 3rd line: UTC as the normalized rows
  header := StringToUtf8(Executable.ProgramFileName) + ' 0.0.0.0 (' + fRemoteStarted + ')'#13 +
    'Host=Remote User=Unknown CPU=Unknown OS=0.0=0.0.0 Wow64=0 Freq=1'#13 +
    'LogView ' + SYNOPSE_FRAMEWORK_VERSION + ' Remote ' + fRemoteStarted + #13#13;
  // the dialog keeps a typed extension, whatever the selected filter is
  if not SameText(ExtractFileExt(fn), '.synlz') then
  begin
    fLog.SaveToFile(fn, header); // overwriting was confirmed by the dialog
    exit;
  end;
  // .synlz: save as plain text into an unused sibling file, then compress it
  repeat
    tmp := fn + '.' + IntToHex(Random32, 8) + '.tmp';
  until not FileExists(tmp);
  try
    fLog.SaveToFile(tmp, header);
  except
    DeleteFile(tmp); // e.g. disk full: don't leave a partial file behind
    raise;
  end;
  if AlgoSynLZ.FileCompress(tmp, fn, LOG_MAGIC, {hash32=}true) then
    DeleteFile(tmp)
  else
    ShowMessage(Format(sCompressFailed, [fn, tmp]));
end;

procedure TMainLogView.lstDaysDblClick(Sender: TObject);
var
  ndx: integer;
begin
  if fLog = nil then
    exit;
  ndx := lstDays.ItemIndex;
  if cardinal(ndx) < cardinal(length(fLog.DayChangeIndex)) then
    SetListItem(fLog.SearchNextSelected(fLog.DayChangeIndex[ndx]));
end;

procedure TMainLogView.PanelLeftResize(Sender: TObject);
begin
  LayoutLeftPanel;
end;


end.

