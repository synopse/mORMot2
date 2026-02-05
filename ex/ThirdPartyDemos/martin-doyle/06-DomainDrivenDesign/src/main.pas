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
  mormot.core.unicode,
  mormot.orm.core,
  mormot.soa.core,
  mormot.rest.http.client,
  DomTypes,
  DomServiceInterfaces,
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
    ExampleService: ISampleService;
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
  Samples: TSampleInfoDynArray;
  i: Integer;
begin
  ListNames.Items.Clear;
  if ExampleService.ListSamples(Samples) = sSuccess then
    for i := 0 to Length(Samples) - 1 do
      ListNames.Items.AddObject(UTF8ToString(Samples[i].Name), TObject(Samples[i].ID));
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
  Sample: TSample;
  RecID: TID;
begin
  Sample.Name := StringToUTF8(EditName.Text);
  Sample.Question := StringToUTF8(MemoQuestion.Text);
  if ListNames.ItemIndex >= 0 then
  begin
    // Update existing record
    RecID := GetSelectedID;
    if ExampleService.UpdateSample(RecID, Sample) = sSuccess then
      RefreshNamesList
    else
      ShowMessage('Error updating the data');
  end
  else
  begin
    // Add new record
    if ExampleService.AddSample(Sample) = sSuccess then
      RefreshNamesList
    else
      ShowMessage('Error adding the data');
  end;
end;

procedure TMainForm.ButtonDeleteClick(Sender: TObject);
var
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
  if ExampleService.DeleteSample(RecID) = sSuccess then
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
  Model := TOrmModel.Create([]);
  HttpClient := TRestHttpClient.Create('localhost', HTTP_PORT, Model);
  HttpClient.ServiceDefine([ISampleService], sicShared, EXAMPLE_CONTRACT);
  HttpClient.Services[EXAMPLE_CONTRACT].Get(ExampleService);
  RefreshNamesList;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ExampleService := nil;
  HttpClient.Free;
  Model.Free;
end;

procedure TMainForm.ListNamesClick(Sender: TObject);
var
  Sample: TSample;
begin
  if ListNames.ItemIndex < 0 then
    Exit;
  Sample.Name := StringToUTF8(ListNames.Items[ListNames.ItemIndex]);
  if ExampleService.FindSample(Sample) = sSuccess then
  begin
    EditName.Text := UTF8ToString(Sample.Name);
    MemoQuestion.Text := UTF8ToString(Sample.Question);
  end;
end;

end.
