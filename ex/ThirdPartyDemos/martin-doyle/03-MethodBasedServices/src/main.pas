unit main;

interface

{$I mormot.defines.inc}
uses
  {$ifdef MSWINDOWS}
  Windows,
  {$endif MSWINDOWS}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  mormot.core.base,
  mormot.core.data,
  mormot.core.json,
  mormot.core.os,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.variants,
  mormot.orm.core,
  mormot.rest.core,
  mormot.rest.http.client,
  data;

type
  TMainForm = class(TForm)
    ButtonDelete: TButton;
    ButtonFind: TButton;
    ButtonNew: TButton;
    ButtonQuit: TButton;
    ButtonSave: TButton;
    LabelName: TLabel;
    LabelNames: TLabel;
    EditName: TEdit;
    ListNames: TListBox;
    MemoQuestion: TMemo;
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ButtonFindClick(Sender: TObject);
    procedure ButtonNewClick(Sender: TObject);
    procedure ButtonQuitClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListNamesClick(Sender: TObject);
  private
    function GetSelectedID: TID;
    procedure RefreshNamesList;
  public
    HttpClient: TRestHttpClient;
    Model: TOrmModel;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

{
********************************** TMainForm ***********************************
}
function TMainForm.GetSelectedID: TID;
begin
  if ListNames.ItemIndex >= 0 then
    Result := TID(ListNames.Items.Objects[ListNames.ItemIndex])
  else
    Result := 0;
end;

procedure TMainForm.RefreshNamesList;
var
  Response: RawUTF8;
  Doc: TDocVariantData;
  i: Integer;
  ID: Int64;
  Name: RawUTF8;
begin
  ListNames.Items.Clear;
  if HttpClient.CallBackGet('examplelist', [], Response) = HTTP_SUCCESS then
  begin
    Doc.InitJSON(Response);
    for i := 0 to Doc.Count - 1 do
    begin
      VariantToInt64(Doc.Values[i].ID, ID);
      Name := VariantToUTF8(Doc.Values[i].Name);
      ListNames.Items.AddObject(UTF8ToString(Name), TObject(ID));
    end;
  end;
end;

procedure TMainForm.ButtonNewClick(Sender: TObject);
begin
  EditName.Text := '';
  MemoQuestion.Text := '';
  ListNames.ItemIndex := -1;
  EditName.SetFocus;
end;

procedure TMainForm.ButtonSaveClick(Sender: TObject);
var
  Response: RawUTF8;
  Sample: TSample;
begin
  Sample.Name := StringToUTF8(EditName.Text);
  Sample.Question := StringToUTF8(MemoQuestion.Text);
  if HttpClient.CallBackPut('example', RecordSave(Sample, TypeInfo(TSample)), Response) = HTTP_CREATED then
    RefreshNamesList
  else
    ShowMessage('Error saving the data');
end;

procedure TMainForm.ButtonDeleteClick(Sender: TObject);
var
  Response: RawUTF8;
  RecID: TID;
begin
  if ListNames.ItemIndex < 0 then
  begin
    ShowMessage('No record selected');
    Exit;
  end;
  if MessageDlg('Delete this record?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;
  RecID := GetSelectedID;
  if HttpClient.CallBack(mDELETE, 'example?ID=' + Int64ToUtf8(RecID), '', Response) = HTTP_SUCCESS then
  begin
    EditName.Text := '';
    MemoQuestion.Text := '';
    RefreshNamesList;
  end
  else
    ShowMessage('Error deleting the data');
end;

procedure TMainForm.ButtonFindClick(Sender: TObject);
var
  i: Integer;
  SearchName: string;
begin
  SearchName := EditName.Text;
  for i := 0 to ListNames.Items.Count - 1 do
    if SameText(ListNames.Items[i], SearchName) then
    begin
      ListNames.ItemIndex := i;
      ListNamesClick(nil);
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
  HttpClient := TRestHttpClient.Create('localhost', HttpPort, Model);
  RefreshNamesList;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  HttpClient.Free;
  Model.Free;
end;

procedure TMainForm.ListNamesClick(Sender: TObject);
var
  Response, Question: RawUTF8;
  RecID: TID;
  Name: RawUTF8;
begin
  RecID := GetSelectedID;
  if RecID = 0 then
    Exit;
  // Find the name for this ID from the list
  Name := StringToUTF8(ListNames.Items[ListNames.ItemIndex]);
  if HttpClient.CallBackGet('example', ['Name', Name], Response) = HTTP_SUCCESS then
  begin
    Question := JSONDecode(Response, 'Result', nil, false);
    EditName.Text := UTF8ToString(Name);
    MemoQuestion.Text := UTF8ToString(Question);
  end;
end;

end.
