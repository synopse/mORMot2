// Author: Thomas Bogenrieder
// The example is a proof of concept for using mORMots TZipRead/TZipWrite and TAesPkcs7Reader/TAesPkcs7Writer, the source code is neither tested nor optimized.

unit frm_Main;

{$I mormot.defines.inc}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  mormot.core.base,
  mormot.core.data,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.os,
  mormot.core.zip,
  mormot.core.perf,
  mormot.crypt.core,
  mormot.crypt.secure,
  mormot.ui.gdiplus;

type
  TImageResourceFile = class(TObject)
  private
    FPassword: RawUtf8;
    FFileName: TFileName;
    FIsWritable: Boolean;
  protected
    function LoadStream(pmStream: TCustomMemoryStream; const pmcResName: TFileName; const pmcPassword: RawUtf8): Boolean;
    procedure SaveStream(pmStream: TCustomMemoryStream; const pmcResName: TFileName; const pmcPassword: RawUtf8; pmIsCompressed: Boolean = True);
  public
    constructor Create(const pmcFileName: TFileName; const pmcPassword: RawUtf8);
    destructor Destroy; override;
    function LoadImage(pmImage: TImage; const pmcImageName: String): Boolean;
    procedure SaveImage(pmImage: TImage; const pmcImageName: String); overload;
    procedure SaveImage(pmImageData: TCustomMemoryStream; const pmcImageName: String); overload;
  end;

  TCustomZipProgressGuiHelper = class abstract(TComponent)
  protected
    procedure DoOnInfoProgress(pmSender: TObject; pmInfo: PProgressInfo); virtual; abstract;
  public
    procedure Prepare(pmZip: TZipAbstract); virtual; abstract;
  end;

type
  TfrmMain = class(TForm)
    Image: TImage;
    ScrollBox: TScrollBox;
    ProgressBar: TProgressBar;
    btnNewImage: TButton;
    btnSaveImage: TButton;
    btnLoadImage: TButton;
    btnCryptSaveLoad: TButton;
    btnZipInfoProgress: TButton;
    btnWriteSpeedChallenge: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnNewImageClick(Sender: TObject);
    procedure btnSaveImageClick(Sender: TObject);
    procedure btnLoadImageClick(Sender: TObject);
    procedure btnCryptSaveLoadClick(Sender: TObject);
    procedure btnZipInfoProgressClick(Sender: TObject);
    procedure btnWriteSpeedChallengeClick(Sender: TObject);
  private
    FTimer: TPrecisionTimer;
    FLastInsertName: String;
    FProgressHelper: TCustomZipProgressGuiHelper;
  public
  end;

var
  frmMain: TfrmMain;


implementation

uses
  System.StrUtils,
  System.Zip;

{$R *.dfm}

//==============================================================================
// TImageResourceFile
//==============================================================================

constructor TImageResourceFile.Create(const pmcFileName: TFileName; const pmcPassword: RawUtf8);
begin
  inherited Create;
  FFileName := pmcFileName;
  FPassword := pmcPassword;
  FIsWritable := IsDirectoryWritable(ExtractFilePath(FFileName));
end;

destructor TImageResourceFile.Destroy;
begin
  FillZero(FPassword);
  inherited Destroy;
end;

function TImageResourceFile.LoadStream(pmStream: TCustomMemoryStream; const pmcResName: TFileName; const pmcPassword: RawUtf8): Boolean;
var
  idx: Integer;
  zipRead: TZipRead;
  encStream: TMemoryStream;
  aesReader: TAesPkcs7Reader;
begin
  Result := False;
  if pmStream = Nil then Exit; //=>
  if pmcResName = '' then Exit; //=>
  if not FileExists(FFileName) then Exit; //=>

  zipRead := TZipRead.Create(FFileName);
  try
    idx := zipRead.NameToIndex(pmcResName);
    if idx < 0 then Exit; //=>

    if pmcPassword = '' then
      Result := zipRead.UnZip(idx, pmStream)
    else
    begin
      encStream := TMemoryStream.Create;
      try
        if zipRead.UnZip(idx, encStream) then
        begin
          encStream.Position := 0;
          aesReader := TAesPkcs7Reader.Create(encStream, pmcPassword);
          try
            Result := (StreamCopyUntilEnd(aesReader, pmStream) > 0);
          finally
            aesReader.Free;
          end;
        end;
      finally
        encStream.Free;
      end;
    end;
  finally
    zipRead.Free;
  end;
end;

procedure TImageResourceFile.SaveStream(pmStream: TCustomMemoryStream; const pmcResName: TFileName; const pmcPassword: RawUtf8; pmIsCompressed: Boolean);
const
  IS_COMPRESSED: array[Boolean] of Integer = (0, 6);
var
  zipWrite: TZipWrite;
  encStream: TMemoryStream;
  aesWriter: TAesPkcs7Writer;
begin
  if pmStream = Nil then Exit; //=>
  if pmcResName = '' then Exit; //=>
  if not FIsWritable then Exit; //=>

  if FileExists(FFileName) then
    zipWrite := TZipWrite.CreateFromIgnore(FFileName, [pmcResName])
  else
    zipWrite := TZipWrite.Create(FFileName);

  try
    if pmcPassword = '' then
      zipWrite.AddDeflated(pmcResName, pmStream.Memory, pmStream.Size, IS_COMPRESSED[pmIsCompressed], DateTimeToFileDate(Now))
    else
    begin
      encStream := TMemoryStream.Create;
      try
        aesWriter := TAesPkcs7Writer.Create(encStream, pmcPassword);
        try
          pmStream.Position := 0;
          if StreamCopyUntilEnd(pmStream, aesWriter) = pmStream.Size then
            aesWriter.Finish
          else
            encStream.Position := 0;
        finally
          aesWriter.Free;
        end;

        zipWrite.AddDeflated(pmcResName, encStream.Memory, encStream.Position, IS_COMPRESSED[pmIsCompressed], DateTimeToFileDate(Now));
      finally
        encStream.Free;
      end;
    end;
  finally
    zipWrite.Free;
  end;
end;

function TImageResourceFile.LoadImage(pmImage: TImage; const pmcImageName: String): Boolean;
var
  tmpStream: TMemoryStream;
begin
  Result := False;
  if pmImage = Nil then Exit; //=>
  if pmcImageName = '' then Exit; //=>

  tmpStream := TMemoryStream.Create;
  try
    if LoadStream(tmpStream, pmcImageName, FPassword) then
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

procedure TImageResourceFile.SaveImage(pmImage: TImage; const pmcImageName: String);
var
  tmpStream: TMemoryStream;
begin
  if pmImage = Nil then Exit; //=>
  if pmcImageName = '' then Exit; //=>

  tmpStream := TMemoryStream.Create;
  try
    pmImage.Picture.SaveToStream(tmpStream);
    SaveImage(tmpStream, pmcImageName);
  finally
    tmpStream.Free;
  end;
end;

procedure TImageResourceFile.SaveImage(pmImageData: TCustomMemoryStream; const pmcImageName: String);
begin
  if pmImageData = Nil then Exit; //=>
  if pmcImageName = '' then Exit; //=>

  SaveStream(pmImageData, pmcImageName, FPassword, False);
end;


//==============================================================================
// TZipProgressBarGuiHelper
//==============================================================================

type
  TZipProgressBarGuiHelper = class(TCustomZipProgressGuiHelper)
  private
    FProgressBar: TProgressBar;
  protected
    procedure DoOnInfoProgress(pmSender: TObject; pmInfo: PProgressInfo); override;
  public
    constructor Create(pmProgressBar: TProgressBar); reintroduce;
    procedure Prepare(pmZip: TZipAbstract); override;
  end;

constructor TZipProgressBarGuiHelper.Create(pmProgressBar: TProgressBar);
begin
  inherited Create(pmProgressBar);

  Assert(pmProgressBar <> Nil);
  FProgressBar := pmProgressBar;
end;

procedure TZipProgressBarGuiHelper.DoOnInfoProgress(pmSender: TObject; pmInfo: PProgressInfo);
begin
  if pmInfo.CurrentSize = 0 then
  begin
    FProgressBar.Min := 0;
    FProgressBar.Max := 100;
    FProgressBar.Position := 0
  end
  else if pmInfo.CurrentSize >= pmInfo.ExpectedSize then
    FProgressBar.Position := 0
  else
    FProgressBar.Position := pmInfo.Percent;
end;

procedure TZipProgressBarGuiHelper.Prepare(pmZip: TZipAbstract);
begin
  if pmZip = Nil then Exit; //=>

  pmZip.ReportDelay := 50;
  pmZip.OnProgress := DoOnInfoProgress;
end;


//==============================================================================
// TfrmMain
//==============================================================================

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FProgressHelper := TZipProgressBarGuiHelper.Create(ProgressBar);
  Image.Picture.LoadFromFile(MakePath([Executable.ProgramFilePath, 'TestImage.png']));
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

procedure TfrmMain.btnSaveImageClick(Sender: TObject);
var
  imgResFile: TImageResourceFile;
begin
  FTimer.Start;
  imgResFile := TImageResourceFile.Create(ChangeFileExt(Executable.ProgramFileName, '.dat'), 'Thomas');
  try
    FLastInsertName := Random32(65536).ToString;
    imgResFile.SaveImage(Image, FLastInsertName);
    ShowMessage(Format('Total time: %s', [FTimer.Stop]));
  finally
    imgResFile.Free;
  end;
end;

procedure TfrmMain.btnLoadImageClick(Sender: TObject);
var
  imgResFile: TImageResourceFile;
begin
  FTimer.Start;
  imgResFile := TImageResourceFile.Create(ChangeFileExt(Executable.ProgramFileName, '.dat'), 'Thomas');
  try
    if imgResFile.LoadImage(Image, FLastInsertName) then
      ShowMessage(Format('Total time: %s', [FTimer.Stop]))
    else
      Image.Picture.Assign(Nil);
  finally
    imgResFile.Free;
  end;
end;

procedure TfrmMain.btnCryptSaveLoadClick(Sender: TObject);
var
  fileName: TFileName;
  fileText: RawByteString;
begin
  fileName := MakePath([Executable.ProgramFilePath, 'TestCryptFile.dat']);
  fileText := TAesPrng.Main.FillRandom(2 shl 20);
  FileFromString(fileText, fileName);

  FTimer.Start;
  AesPkcs7File(fileName, ChangeFileExt(fileName, '.tmp'), True, 'Thomas');
  AesPkcs7File(ChangeFileExt(fileName, '.tmp'), fileName + '2', False, 'Thomas');
  FTimer.Pause;

  ShowMessage(Format('Total time: %s', [IfThen(HashFileMd5(fileName) = HashFileMd5(fileName + '2'), String(FTimer.Time), 'broken')]));
end;

procedure TfrmMain.btnZipInfoProgressClick(Sender: TObject);
var
  zipWrite: TZipWrite;
  fileName: TFileName;
  fileData: RawByteString;
begin
  fileName := MakePath([Executable.ProgramFilePath, 'TestProgressFile.dat']);
  fileData := TAesPrng.Main.FillRandom(80 shl 20);
  FileFromString(fileData, fileName);

  FTimer.Start;
  zipWrite := TZipWrite.Create(MakePath([Executable.ProgramFilePath, 'TestProgressFile.zip']));
  try
    FProgressHelper.Prepare(zipWrite);
    zipWrite.AddDeflated(fileName);
    FTimer.Pause;
  finally
    zipWrite.Free;
  end;

  DeleteFile(fileName);
  ShowMessage(Format('Total time: %s', [FTimer.Time]));
end;

procedure TfrmMain.btnWriteSpeedChallengeClick(Sender: TObject);
var
  fileName: TFileName;
  fileData: RawByteString;
begin
  fileName := MakePath([Executable.ProgramFilePath, 'TestZIPSpeedFile.dat']);
  fileData := TAesPrng.Main.FillRandom(10 shl 20);
  FileFromString(fileData, fileName);

  FTimer.Start;
  var zipFile: TZipFile := TZipFile.Create;
  try
    zipFile.Open(MakePath([Executable.ProgramFilePath, 'TestZIPSpeedDelphi.zip']), zmWrite);
    zipFile.Add(fileName);
    FTimer.Pause;
  finally
    zipFile.Free;
  end;

  var timeDelphi: TShort16 := FTimer.Time;

  FTimer.Start;
  var zipWrite: TZipWrite := TZipWrite.Create(MakePath([Executable.ProgramFilePath, 'TestZIPSpeedMormot.zip']));
  try
    zipWrite.AddDeflated(fileName);
    FTimer.Pause;
  finally
    zipWrite.Free;
  end;

  DeleteFile(fileName);
  ShowMessage(Format('Total time:'#10#13'- Delphi %s'#10#13'- Mormot %s', [timeDelphi, FTimer.Time]));
end;


initialization
  RegisterSynPictures;

end.
