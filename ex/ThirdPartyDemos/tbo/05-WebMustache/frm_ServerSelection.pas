// Author: Thomas Bogenrieder
// The example is a proof of concept for working with the mORMot library. Source code is neither tested nor optimized.

unit frm_ServerSelection;

{$I mormot.defines.inc}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  u_FileServer;

type
  TfrmServerSelection = class(TForm)
    btnStartServer: TButton;
    btnTerminate: TButton;
    rbHttp10: TRadioButton;
    rbHttp11: TRadioButton;
    rbHttpsSelf: TRadioButton;
    rbHttpsCert: TRadioButton;
    edtCertFileName: TEdit;
    edtPrivKeyFileName: TEdit;
    edtPrivKeyPassword: TEdit;
    lblCertFileName: TLabel;
    lblPrivKeyFileName: TLabel;
    lblPrivKeyPassword: TLabel;
    procedure FormShow(Sender: TObject);
    procedure btnStartServerClick(Sender: TObject);
  protected
    procedure UpdateActions; override;
  public
    class function Execute(out pmoMode: THttpServerMode;
      out pmoCertFileName, pmoPrivKeyFileName: TFileName; out pmoPrivKeyPassword: String): Boolean;
  end;


implementation

uses
  mormot.core.os,
  mormot.core.text,
  u_Utilities;

{$R *.dfm}

//==============================================================================
// TfrmServerSelection
//==============================================================================

class function TfrmServerSelection.Execute(out pmoMode: THttpServerMode;
  out pmoCertFileName, pmoPrivKeyFileName: TFileName; out pmoPrivKeyPassword: String): Boolean;
begin
  with Self.Create(Nil) do
  try
    Result := (ShowModal = mrOk);
    if Result then
    begin
      if rbHttp10.Checked then
        pmoMode := hsmHttp10
      else if rbHttp11.Checked then
        pmoMode := hsmHttp11
      else if rbHttpsSelf.Checked then
        pmoMode := hsmHttpsSelf
      else if rbHttpsCert.Checked then
      begin
        pmoMode := hsmHttpsCert;
        pmoCertFileName := edtCertFileName.Text;
        pmoPrivKeyFileName := edtPrivKeyFileName.Text;
        pmoPrivKeyPassword := edtPrivKeyPassword.Text;
      end;
    end;
  finally
    Free;
  end;
end;


procedure TfrmServerSelection.FormShow(Sender: TObject);
var
  certFileName, privKeyFileName: TFileName;
begin
  TFormStorage.Create(Self).Load([
    TCompPropStorage.PropPath(edtCertFileName, 'Text'),
    TCompPropStorage.PropPath(edtPrivKeyFileName, 'Text')]);

  certFileName := edtCertFileName.Text;
  if ExtractFilePath(certFileName) = '' then
    certFileName := MakePath([Executable.ProgramFilePath, certFileName]);

  privKeyFileName := edtPrivKeyFileName.Text;
  if ExtractFilePath(privKeyFileName) = '' then
    privKeyFileName := MakePath([Executable.ProgramFilePath, privKeyFileName]);

  if (FileExists(certFileName)
    and FileExists(privKeyFileName)) then
  begin
    rbHttpsCert.Checked := True;
    edtPrivKeyPassword.Enabled := True;
    ActiveControl := edtPrivKeyPassword;
    PostMessage(Handle, WM_SETFOCUS, 0, 0);
  end;
end;


procedure TfrmServerSelection.UpdateActions;
var
  edtEnabled: Boolean;
begin
  inherited UpdateActions;
  edtEnabled := edtCertFileName.Enabled;
  edtCertFileName.Enabled := rbHttpsCert.Checked;
  if (edtEnabled <> edtCertFileName.Enabled) and not edtCertFileName.Enabled then
  begin
    edtCertFileName.Text := '';
    edtPrivKeyFileName.Text := '';
    edtPrivKeyPassword.Text := '';
  end;

  edtPrivKeyFileName.Enabled := rbHttpsCert.Checked;
  edtPrivKeyPassword.Enabled := rbHttpsCert.Checked;
end;


procedure TfrmServerSelection.btnStartServerClick(Sender: TObject);
var
  certFileName, privKeyFileName: TFileName;
begin
  ModalResult := mrOk;
  if not rbHttpsCert.Checked then Exit; //=>

  certFileName := edtCertFileName.Text;
  if ExtractFilePath(certFileName) = '' then
    certFileName := MakePath([Executable.ProgramFilePath, certFileName]);

  privKeyFileName := edtPrivKeyFileName.Text;
  if ExtractFilePath(privKeyFileName) = '' then
    privKeyFileName := MakePath([Executable.ProgramFilePath, privKeyFileName]);

  if not (FileExists(certFileName)
    and FileExists(privKeyFileName)
    and (edtPrivKeyPassword.Text <> '')) then
  begin
    ModalResult := mrNone;
  end;
end;

end.
