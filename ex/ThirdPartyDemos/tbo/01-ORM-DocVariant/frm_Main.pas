// Author: Thomas Bogenrieder
// The example is a proof of concept for using SQLite with mORMot, the source code is neither tested nor optimized.

unit frm_Main;

{$I mormot.defines.inc}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  mormot.core.base,
  mormot.core.data,
  mormot.core.text,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.unicode,
  mormot.core.variants,
  mormot.core.datetime,
  mormot.core.perf,
  mormot.core.os,
  mormot.orm.base,
  mormot.orm.core,
  mormot.orm.sqlite3,
  mormot.rest.core,
  mormot.rest.sqlite3,
  mormot.db.raw.sqlite3,
  mormot.db.raw.sqlite3.static,
  mormot.ui.gdiplus;

type
  TOrmFile = class(TOrm)
  private
    FTitle: RawUtf8;
    FComment: RawUtf8;
    FMetaData: Variant;
    FImageData: RawBlob;
    FCreatedAt: TCreateTime;
    FModifiedAt: TModTime;
  published
    property Title: RawUTF8
      read FTitle write FTitle;
    property Comment: RawUtf8
      read FComment write FComment;
    property MetaData: Variant
      read FMetaData write FMetaData;
    property ImageData: RawBlob
      read FImageData write FImageData;
    property CreatedAt: TCreateTime
      read FCreatedAt;
    property ModifiedAt: TModTime
      read FModifiedAt;
  end;

  // https://www.sqlite.org/fts5.html
  TOrmFileSearch = class(TOrmFts5Unicode61)
  private
    FTitle: RawUtf8;
    FComment: RawUtf8;
  published
    // the properties used for searching must match those in TOrmFile
    property Title: RawUTF8
      read FTitle write FTitle;
    property Comment: RawUtf8
      read FComment write FComment;
  end;

  TImageResourceDB = class(TObject)
  private
    FRestServer: TRestServerDB;
    function CreateModel: TOrmModel;
  protected
    function LoadData(pmStream: TStream; const pmcRowID: TID): Boolean;
    function SaveData(const pmcImageData: RawBlob; const pmcTitle, pmcComment: String; const pmcMetaData: Variant; var pmvRowID: TID): Boolean;
  public
    constructor Create(const pmcFileName: TFileName; const pmcPassword: RawUtf8 = '');
    destructor Destroy; override;
    class function InitDefaultMetaData(const pmcCreator, pmcLocation: RawUtf8; pmLatitude, pmLongitude: Double; pmDate: TDate; pmTime: TTime): Variant;
    function LoadImage(pmImage: TImage; const pmcRowID: TID): Boolean; overload;
    function LoadImage(pmImage: TImage; const pmcSearchPhrase: String; pmResultIDs: PIDDynArray = Nil): Boolean; overload;
    function LoadImage(pmImage: TImage; const pmcMetaFieldName: String; const pmcMetaFieldValue: Variant; pmResultIDs: PIDDynArray = Nil): Boolean; overload;
    function SaveImage(pmImage: TImage; const pmcTitle, pmcComment: String; const pmcMetaData: Variant; pmRowID: PID = Nil): Boolean; overload;
    function SaveImage(pmStream: TStream; const pmcTitle, pmcComment: String; const pmcMetaData: Variant; pmRowID: PID = Nil): Boolean; overload;
  end;

type
  TfrmMain = class(TForm)
    ScrollBox: TScrollBox;
    Image: TImage;
    btnNewImage: TButton;
    btnAddImage: TButton;
    btnLoadImage: TButton;
    edtSearchPhrase: TEdit;
    btnLoadSearchFTS: TButton;
    btnLoadSearchMetaData: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAddImageClick(Sender: TObject);
    procedure btnLoadImageClick(Sender: TObject);
    procedure btnLoadSearchFTSClick(Sender: TObject);
    procedure btnLoadSearchMetaDataClick(Sender: TObject);
    procedure btnNewImageClick(Sender: TObject);
  private
    FLastInsertRowID: TID;
    FImageResourceDB: TImageResourceDB;
    FTimer: TPrecisionTimer;
  end;

var
  frmMain: TfrmMain;


implementation

{$R *.dfm}

//==============================================================================
// TImageResourceDB
//==============================================================================

constructor TImageResourceDB.Create(const pmcFileName: TFileName; const pmcPassword: RawUtf8);
begin
  inherited Create;
  FRestServer := TRestServerDB.Create(CreateModel, pmcFileName, False, pmcPassword);
  FRestServer.Model.Owner := FRestServer;
  FRestServer.DB.Synchronous := smFull;
  FRestServer.DB.LockingMode := lmExclusive;
  FRestServer.Server.CreateMissingTables(0, [itoNoAutoCreateGroups, itoNoAutoCreateUsers]);
end;

destructor TImageResourceDB.Destroy;
begin
  FRestServer.Free;
  inherited Destroy;
end;

function TImageResourceDB.CreateModel: TOrmModel;
begin
  Result := TOrmModel.Create([TOrmFile, TOrmFileSearch]);
  Result.Props[TOrmFileSearch].FTS4WithoutContent(TOrmFile);
end;

function TImageResourceDB.LoadData(pmStream: TStream; const pmcRowID: TID): Boolean;
var
  tmpData: RawBlob;
begin
  Result := False;
  if pmcRowID <= 0 then Exit; //=>

  if FRestServer.Server.RetrieveBlob(TOrmFile, pmcRowID, 'ImageData', tmpData) then
    Result := (pmStream.Write(Pointer(tmpData)^, Length(tmpData)) = Length(tmpData));
end;

function TImageResourceDB.SaveData(const pmcImageData: RawBlob; const pmcTitle, pmcComment: String; const pmcMetaData: Variant; var pmvRowID: TID): Boolean;
var
  ormFile: TOrmFile;
begin
  Result := False;
  ormFile := TOrmFile.Create;
  try
    ormFile.Title := StringToUtf8(pmcTitle);
    ormFile.Comment := StringToUtf8(pmcComment);
    ormFile.MetaData := pmcMetaData;
    ormFile.ImageData := pmcImageData;
    pmvRowID := FRestServer.Server.Add(ormFile, True);
    if pmvRowID > 0 then
      Result := FRestServer.Server.UpdateBlobFields(ormFile);
  finally
    ormFile.Free;
  end;
end;

class function TImageResourceDB.InitDefaultMetaData(const pmcCreator, pmcLocation: RawUtf8; pmLatitude, pmLongitude: Double; pmDate: TDate; pmTime: TTime): Variant;
var
  doc: TDocVariantData;
begin
  doc.InitFast(dvObject);
  doc.AddValue('Creator', pmcCreator);
  doc.AddValue('Location', pmcLocation);
  doc.AddValue('Latitude', pmLatitude);
  doc.AddValue('Longitude', pmLongitude);
  doc.AddValue('Date', DateToIso8601(pmDate, True));
  doc.AddValue('Time', TimeToIso8601(pmTime, True));
  Result := Variant(doc);
end;

function TImageResourceDB.LoadImage(pmImage: TImage; const pmcRowID: TID): Boolean;
var
  tmpStream: TMemoryStream;
begin
  Result := False;
  if pmImage = Nil then Exit; //=>

  tmpStream := TMemoryStream.Create;
  try
    if LoadData(tmpStream, pmcRowID) then
    begin
      tmpStream.Position := 0;
      try
        pmImage.Picture.LoadFromStream(tmpStream);
        Result := True;
      except
      end;
    end;
  finally
    tmpStream.Free;
  end;
end;

function TImageResourceDB.LoadImage(pmImage: TImage; const pmcSearchPhrase: String; pmResultIDs: PIDDynArray): Boolean;
var
  sqlWhere: RawUtf8;
  searchIDs: TIDDynArray;
begin
  Result := False;
  if pmImage = Nil then Exit; //=>
  if pmcSearchPhrase = '' then Exit; //=>

  sqlWhere := FormatUtf8('% MATCH ? ORDER BY rank DESC', [TOrmFileSearch.OrmProps.SqlTableName], [StringToUtf8(pmcSearchPhrase)]);
  if FRestServer.Server.FTSMatch(TOrmFileSearch, sqlWhere, searchIDs) then
  begin
    Result := LoadImage(pmImage, searchIDs[0]);
    if Result and (pmResultIDs <> Nil) then
      DynArrayCopy(Pointer(pmResultIDs), @searchIDs, TypeInfo(TIDDynArray), Nil);
  end;
end;

function TImageResourceDB.LoadImage(pmImage: TImage; const pmcMetaFieldName: String; const pmcMetaFieldValue: Variant; pmResultIDs: PIDDynArray = Nil): Boolean;
const
  // Schema: SELECT * FROM File WHERE MetaData->>'$.Creator'='Thomas' ORDER BY ModifiedAt DESC
  _SQL_WHERE: RawUtf8 = 'MetaData->>''$.%''=? ORDER BY ModifiedAt DESC';
var
  sqlWhere: RawUtf8;
  searchIDs: TIDDynArray;
begin
  Result := False;
  if pmcMetaFieldName = '' then Exit; //=>
  if VarIsEmptyOrNull(pmcMetaFieldValue) then Exit; //=>

  sqlWhere := FormatUtf8(_SQL_WHERE, [pmcMetaFieldName], [pmcMetaFieldValue]);
  if FRestServer.Server.OneFieldValues(TOrmFile, 'RowID', sqlWhere, TInt64DynArray(searchIDs)) then
  begin
    Result := LoadImage(pmImage, searchIDs[0]);
    if Result and (pmResultIDs <> Nil) then
      DynArrayCopy(Pointer(pmResultIDs), @searchIDs, TypeInfo(TIDDynArray), Nil);
  end;
end;

function TImageResourceDB.SaveImage(pmImage: TImage; const pmcTitle, pmcComment: String; const pmcMetaData: Variant; pmRowID: PID): Boolean;
var
  rowID: TID;
  tmpStream: TRawByteStringStream;
begin
  Result := False;
  if pmImage = Nil then Exit; //=>
  if pmcTitle = '' then Exit; //=>

  tmpStream := TRawByteStringStream.Create;
  try
    pmImage.Picture.SaveToStream(tmpStream);
    Result := SaveData(tmpStream.DataString, pmcTitle, pmcComment, pmcMetaData, rowID);
    if Result and (pmRowID <> Nil) then
      pmRowID^ := rowID;
  finally
    tmpStream.Free;
  end;
end;

function TImageResourceDB.SaveImage(pmStream: TStream; const pmcTitle, pmcComment: String; const pmcMetaData: Variant; pmRowID: PID): Boolean;
var
  rowID: TID;
  tmpData: RawBlob;
begin
  Result := False;
  if pmStream = Nil then Exit; //=>
  if pmcTitle = '' then Exit; //=>

  pmStream.Position := 0;
  Result := (pmStream.Read(Pointer(tmpData)^, pmStream.Size) = pmStream.Size);
  if Result then
  begin
    Result := SaveData(tmpData, pmcTitle, pmcComment, pmcMetaData, rowID);
    if Result and (pmRowID <> Nil) then
      pmRowID^ := rowID;
  end;
end;


//==============================================================================
// TfrmMain
//==============================================================================

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FImageResourceDB := TImageResourceDB.Create(ChangeFileExt(Application.ExeName, '.dat'));
  Image.Picture.LoadFromFile('TestImage.png');
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FImageResourceDB.Free;
end;

procedure TfrmMain.btnNewImageClick(Sender: TObject);
begin
  with TOpenDialog.Create(Nil) do
  try
    Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];
    InitialDir := Executable.ProgramFilePath;
    DefaultExt := GraphicExtension(TGraphic);
    Filter := GraphicFilter(TGraphic);
    if Execute then
      Image.Picture.LoadFromFile(FileName);
  finally
    Free;
  end;
end;

procedure TfrmMain.btnAddImageClick(Sender: TObject);
var
  metaData: Variant;
begin
  FTimer.Start;
  metaData := TImageResourceDB.InitDefaultMetaData('Thomas', 'Würzburg', 49.78, 9.94, Date, Time);
  metaData.ShutterSpeed := 2;
  metaData.ISOSensitivity := 200;
  FImageResourceDB.SaveImage(Image,
    'Würzburg main station at night',
    'Würzburg is a beautiful city in the state of Bavaria. It is known for its baroque and rococo style buildings.',
    metaData,
    @FLastInsertRowID);

  ShowMessage(Format('Total time: %s', [FTimer.Stop]));
end;

procedure TfrmMain.btnLoadImageClick(Sender: TObject);
begin
  FTimer.Start;
  if FImageResourceDB.LoadImage(Image, FLastInsertRowID) then
    ShowMessage(Format('Total time: %s', [FTimer.Stop]))
  else
    Image.Picture.Assign(Nil);
end;

procedure TfrmMain.btnLoadSearchFTSClick(Sender: TObject);
var
  resultIDs: TIDDynArray;
begin
  FTimer.Start;
  if FImageResourceDB.LoadImage(Image, edtSearchPhrase.Text, @resultIDs) then
    ShowMessage(Format('Total time: %s'#10#13'ResultIDs: %s', [FTimer.Stop, Int64DynArrayToCsv(TInt64DynArray(resultIDs))]))
  else
    Image.Picture.Assign(Nil);
end;

procedure TfrmMain.btnLoadSearchMetaDataClick(Sender: TObject);
var
  resultIDs: TIDDynArray;
begin
  FTimer.Start;
  // if FImageResourceDB.LoadImage(Image, 'Creator', 'Thomas', @resultIDs) then
  if FImageResourceDB.LoadImage(Image, 'ShutterSpeed', 2, @resultIDs) then
    ShowMessage(Format('Total time: %s'#10#13'ResultIDs: %s', [FTimer.Stop, Int64DynArrayToCsv(TInt64DynArray(resultIDs))]))
  else
    Image.Picture.Assign(Nil);
end;


initialization
  RegisterSynPictures;

end.
