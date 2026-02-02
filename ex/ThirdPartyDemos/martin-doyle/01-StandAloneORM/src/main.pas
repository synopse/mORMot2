unit main;

interface

{$I mormot.defines.inc}
uses
  {$ifdef MSWINDOWS}
  Windows,
  {$endif MSWINDOWS}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  mormot.core.base, mormot.core.os, mormot.core.unicode, mormot.orm.core, mormot.rest.sqlite3, mormot.db.raw.sqlite3.static,
  client, data;

type
  TMainForm = class(TForm)
    ButtonDelete: TButton;
    ButtonFind: TButton;
    ButtonNew: TButton;
    ButtonQuit: TButton;
    ButtonSave: TButton;
    LabelEdit: TLabel;
    LabelList: TLabel;
    NameEdit: TEdit;
    NamesList: TListBox;
    QuestionMemo: TMemo;
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ButtonFindClick(Sender: TObject);
    procedure ButtonNewClick(Sender: TObject);
    procedure ButtonQuitClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure NamesListClick(Sender: TObject);
  private
    procedure RefreshNamesList;
  public
    Client: TSampleClient;
    Model: TOrmModel;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

{
********************************** TMainForm ***********************************
}
procedure TMainForm.RefreshNamesList;
var
  Rec: TOrmSample;
begin
  NamesList.Items.Clear;
  Rec := TOrmSample.CreateAndFillPrepare(Client.Orm, '', []);
  try
    while Rec.FillOne do
      NamesList.Items.AddObject(UTF8ToString(Rec.Name), TObject(Rec.ID));
  finally
    Rec.Free;
  end;
end;

procedure TMainForm.ButtonNewClick(Sender: TObject);
begin
  NameEdit.Text := '';
  QuestionMemo.Text := '';
  NamesList.ItemIndex := -1;
  NameEdit.SetFocus;
end;

procedure TMainForm.ButtonSaveClick(Sender: TObject);
var
  Rec: TOrmSample;
  RecID: TID;
begin
  if NamesList.ItemIndex >= 0 then
  begin
    // Update existing record
    RecID := TID(NamesList.Items.Objects[NamesList.ItemIndex]);
    Rec := TOrmSample.Create(Client.Orm, RecID);
    try
      Rec.Name := StringToUTF8(NameEdit.Text);
      Rec.Question := StringToUTF8(QuestionMemo.Text);
      if not Client.Orm.Update(Rec) then
        ShowMessage('Error updating the data');
    finally
      Rec.Free;
    end;
  end
  else
  begin
    // Add new record
    Rec := TOrmSample.Create;
    try
      Rec.Name := StringToUTF8(NameEdit.Text);
      Rec.Question := StringToUTF8(QuestionMemo.Text);
      if Client.Orm.Add(Rec, true) = 0 then
        ShowMessage('Error adding the data');
    finally
      Rec.Free;
    end;
  end;
  RefreshNamesList;
end;

procedure TMainForm.ButtonDeleteClick(Sender: TObject);
var
  RecID: TID;
begin
  if NamesList.ItemIndex < 0 then
  begin
    ShowMessage('No record selected');
    Exit;
  end;
  RecID := TID(NamesList.Items.Objects[NamesList.ItemIndex]);
  if not Client.Orm.Delete(TOrmSample, RecID) then
    ShowMessage('Error deleting the data')
  else
  begin
    NameEdit.Text := '';
    QuestionMemo.Text := '';
    RefreshNamesList;
  end;
end;

procedure TMainForm.ButtonFindClick(Sender: TObject);
var
  i: Integer;
  SearchName: string;
begin
  SearchName := NameEdit.Text;
  for i := 0 to NamesList.Items.Count - 1 do
    if SameText(NamesList.Items[i], SearchName) then
    begin
      NamesList.ItemIndex := i;
      NamesListClick(nil);
      Exit;
    end;
  ShowMessage('Not found');
end;

procedure TMainForm.ButtonQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Model := CreateSampleModel;
  Client := TSampleClient.Create(Model, nil, ChangeFileExt(Executable.ProgramFileName,'.db'), TRestServerDB, false, '');
  Client.Server.Server.CreateMissingTables;
  RefreshNamesList;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Client.Free;
  Model.Free;
end;

procedure TMainForm.NamesListClick(Sender: TObject);
var
  Rec: TOrmSample;
  RecID: TID;
begin
  if NamesList.ItemIndex < 0 then
    Exit;
  RecID := TID(NamesList.Items.Objects[NamesList.ItemIndex]);
  Rec := TOrmSample.Create(Client.Orm, RecID);
  try
    if Rec.ID > 0 then
    begin
      NameEdit.Text := UTF8ToString(Rec.Name);
      QuestionMemo.Text := UTF8ToString(Rec.Question);
    end;
  finally
    Rec.Free;
  end;
end;

end.
