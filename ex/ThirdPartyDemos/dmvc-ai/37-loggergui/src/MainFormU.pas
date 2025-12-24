unit MainFormU;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  mormot.core.base,
  mormot.core.os,
  mormot.core.rtti,
  mormot.core.log,
  mormot.core.text;

type
  TMainForm = class(TForm)
    btnLoggerTest: TButton;
    memoLog: TMemo;
    btnClearLog: TButton;
    btnViewLogFile: TButton;
    lblLogFile: TLabel;
    procedure btnLoggerTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnClearLogClick(Sender: TObject);
    procedure btnViewLogFileClick(Sender: TObject);
  private
    procedure AddLog(const aLevel, aMessage: string);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Configure logging
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;
  TSynLog.Family.AutoFlushTimeOut := 1;

  lblLogFile.Caption := 'Log file: ' + TSynLog.Add.FileName;

  memoLog.Lines.Add('mORMot2 Logger GUI Sample');
  memoLog.Lines.Add('=========================');
  memoLog.Lines.Add('Click "Test Logger" to generate sample log entries');
  memoLog.Lines.Add('Port of: DMVCFramework samples/LoggerGUI');
  memoLog.Lines.Add('');
end;

procedure TMainForm.AddLog(const aLevel, aMessage: string);
begin
  memoLog.Lines.Add(Format('[%s] %s: %s',
    [FormatDateTime('hh:nn:ss', Now), aLevel, aMessage]));
end;

procedure TMainForm.btnLoggerTestClick(Sender: TObject);
begin
  // DMVC: Log.Info('This is an info log', 'log1');
  TSynLog.Add.Log(sllInfo, 'This is an info log [tag:log1]');
  AddLog('INFO', 'This is an info log [tag:log1]');

  // DMVC: Log.Warn('This is a warn log', 'log1');
  TSynLog.Add.Log(sllWarning, 'This is a warn log [tag:log1]');
  AddLog('WARN', 'This is a warn log [tag:log1]');

  // DMVC: Log.Debug('This is a debug log', 'log2');
  TSynLog.Add.Log(sllDebug, 'This is a debug log [tag:log2]');
  AddLog('DEBUG', 'This is a debug log [tag:log2]');

  // DMVC: Log.Error('This is an error log', 'log2');
  TSynLog.Add.Log(sllError, 'This is an error log [tag:log2]');
  AddLog('ERROR', 'This is an error log [tag:log2]');

  // DMVC: Log.Fatal('This is a fatal log', 'log3');
  TSynLog.Add.Log(sllFail, 'This is a fatal log [tag:log3]');
  AddLog('FAIL', 'This is a fatal log [tag:log3]');

  memoLog.Lines.Add('');
  memoLog.Lines.Add('All logs written to: ' + TSynLog.Add.FileName);
  memoLog.Lines.Add('');
end;

procedure TMainForm.btnClearLogClick(Sender: TObject);
begin
  memoLog.Clear;
  memoLog.Lines.Add('Log display cleared');
  memoLog.Lines.Add('');
end;

procedure TMainForm.btnViewLogFileClick(Sender: TObject);
var
  logContent: RawUtf8;
  logFile: string;
begin
  logFile := TSynLog.Add.FileName;

  if not FileExists(logFile) then
  begin
    ShowMessage('Log file not found: ' + logFile);
    Exit;
  end;

  logContent := StringFromFile(logFile);

  memoLog.Clear;
  memoLog.Lines.Add('=== Log File Contents ===');
  memoLog.Lines.Add('File: ' + logFile);
  memoLog.Lines.Add('');
  memoLog.Lines.Add(Utf8ToString(logContent));
end;

end.
