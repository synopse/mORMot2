unit MainFormU;

{$I mormot.defines.inc}

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  Messages,
  {$endif}
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Grids,
  ExtCtrls,
  mormot.core.base,
  mormot.core.text,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.datetime,
  mormot.core.interfaces,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.data,
  mormot.core.variants,
  mormot.orm.core,
  mormot.rest.client,
  mormot.rest.http.client,
  api.interfaces;

type
  TMainForm = class(TForm)
    pnlTop: TPanel;
    Label1: TLabel;
    lblStatus: TLabel;
    pnlButtons: TPanel;
    btnRefresh: TButton;
    btnNew: TButton;
    btnEdit: TButton;
    btnDelete: TButton;
    btnSearch: TButton;
    gridArticles: TStringGrid;
    pnlSearch: TPanel;
    Label2: TLabel;
    edtSearch: TEdit;
    btnSearchExecute: TButton;
    btnSearchClear: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnSearchExecuteClick(Sender: TObject);
    procedure btnSearchClearClick(Sender: TObject);
    procedure gridArticlesDblClick(Sender: TObject);
  private
    fClient: TRestHttpClientGeneric;
    fArticles: TArticleDtos;
    procedure InitializeGrid;
    procedure LoadArticles;
    procedure DisplayArticles;
    procedure ShowStatus(const aText: string; aError: Boolean = False);
    procedure ShowError(const aError: string);
    function GetSelectedArticleId: TID;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  ArticleEditFormU;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Create HTTP client pointing to the articles_crud_server
  fClient := TRestHttpClientGeneric.Create('localhost', '8080', TOrmModel.Create([]));

  InitializeGrid;
  pnlSearch.Visible := False;
  ShowStatus('Ready');
  LoadArticles;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  fClient.Free;
end;

procedure TMainForm.InitializeGrid;
begin
  // Setup grid columns
  gridArticles.ColCount := 6;
  gridArticles.RowCount := 1;
  gridArticles.FixedRows := 1;
  gridArticles.DefaultRowHeight := 20;

  // Column headers
  gridArticles.Cells[0, 0] := 'ID';
  gridArticles.Cells[1, 0] := 'Code';
  gridArticles.Cells[2, 0] := 'Description';
  gridArticles.Cells[3, 0] := 'Price';
  gridArticles.Cells[4, 0] := 'Created At';
  gridArticles.Cells[5, 0] := 'Updated At';

  // Column widths
  gridArticles.ColWidths[0] := 50;   // ID
  gridArticles.ColWidths[1] := 80;   // Code
  gridArticles.ColWidths[2] := 250;  // Description
  gridArticles.ColWidths[3] := 80;   // Price
  gridArticles.ColWidths[4] := 140;  // Created At
  gridArticles.ColWidths[5] := 140;  // Updated At
end;

procedure TMainForm.LoadArticles;
var
  response: RawUtf8;
  status: Integer;
  doc: TDocVariantData;
  item: PDocVariantData;
  i: Integer;
begin
  ShowStatus('Loading articles...');
  Screen.Cursor := crHourGlass;
  try
    try
      // Call POST ArticlesApi.GetAll (JSON-RPC style)
      status := fClient.CallBack(mPOST, 'ArticlesApi.GetAll', '', response);

      if status = 200 then
      begin
        // Parse JSON array response
        doc.InitJson(response, JSON_FAST);
        SetLength(fArticles, doc.Count);
        for i := 0 to doc.Count - 1 do
        begin
          item := _Safe(doc.Value[i]);
          fArticles[i].ID := item^.I['id'];
          fArticles[i].Code := item^.U['code'];
          fArticles[i].Description := item^.U['description'];
          fArticles[i].Price := StrToCurr(Utf8ToString(item^.U['price']));
          fArticles[i].CreatedAt := Iso8601ToDateTime(item^.U['createdat']);
          fArticles[i].UpdatedAt := Iso8601ToDateTime(item^.U['updatedat']);
        end;
        DisplayArticles;
        ShowStatus(Format('Loaded %d articles', [Length(fArticles)]));
      end
      else
      begin
        ShowError(Format('Failed to load articles: HTTP %d - %s', [status, response]));
        SetLength(fArticles, 0);
        DisplayArticles;
      end;
    except
      on E: Exception do
      begin
        ShowError('Failed to load articles: ' + E.Message);
        SetLength(fArticles, 0);
        DisplayArticles;
      end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.DisplayArticles;
var
  i: Integer;
begin
  // Resize grid
  if Length(fArticles) > 0 then
    gridArticles.RowCount := Length(fArticles) + 1
  else
    gridArticles.RowCount := 2;

  // Clear existing data
  for i := 1 to gridArticles.RowCount - 1 do
  begin
    gridArticles.Cells[0, i] := '';
    gridArticles.Cells[1, i] := '';
    gridArticles.Cells[2, i] := '';
    gridArticles.Cells[3, i] := '';
    gridArticles.Cells[4, i] := '';
    gridArticles.Cells[5, i] := '';
  end;

  // Fill with article data
  for i := 0 to High(fArticles) do
  begin
    gridArticles.Cells[0, i + 1] := IntToStr(fArticles[i].ID);
    gridArticles.Cells[1, i + 1] := Utf8ToString(fArticles[i].Code);
    gridArticles.Cells[2, i + 1] := Utf8ToString(fArticles[i].Description);
    gridArticles.Cells[3, i + 1] := CurrToStr(fArticles[i].Price);
    if fArticles[i].CreatedAt <> 0 then
      gridArticles.Cells[4, i + 1] := DateTimeToStr(fArticles[i].CreatedAt);
    if fArticles[i].UpdatedAt <> 0 then
      gridArticles.Cells[5, i + 1] := DateTimeToStr(fArticles[i].UpdatedAt);
  end;
end;

procedure TMainForm.ShowStatus(const aText: string; aError: Boolean = False);
begin
  lblStatus.Caption := aText;
  if aError then
    lblStatus.Font.Color := clRed
  else
    lblStatus.Font.Color := clWindowText;
  Application.ProcessMessages;
end;

procedure TMainForm.ShowError(const aError: string);
begin
  ShowStatus(aError, True);
  MessageDlg(aError, mtError, [mbOK], 0);
end;

function TMainForm.GetSelectedArticleId: TID;
var
  row: Integer;
begin
  Result := 0;
  row := gridArticles.Row;
  if (row > 0) and (row <= Length(fArticles)) then
    Result := fArticles[row - 1].ID;
end;

procedure TMainForm.btnRefreshClick(Sender: TObject);
begin
  LoadArticles;
end;

procedure TMainForm.btnNewClick(Sender: TObject);
var
  editForm: TArticleEditForm;
  newArticle: TArticleDto;
  postData, response: RawUtf8;
  status: Integer;
  doc: TDocVariantData;
begin
  editForm := TArticleEditForm.Create(Self);
  try
    editForm.IsNewArticle := True;
    if editForm.ShowModal = mrOk then
    begin
      newArticle := editForm.Article;
      ShowStatus('Creating article...');
      Screen.Cursor := crHourGlass;
      try
        try
          // Build JSON body: {"article":{...}}
          doc.InitObject(['article', _ObjFast([
            'code', newArticle.Code,
            'description', newArticle.Description,
            'price', newArticle.Price
          ])]);
          postData := doc.ToJson;

          // Call POST ArticlesApi.Create
          status := fClient.CallBack(mPOST, 'ArticlesApi.Create', postData, response);

          if status = 200 then
          begin
            ShowStatus('Article created successfully');
            LoadArticles; // Refresh list
          end
          else
            ShowError(Format('Failed to create article: HTTP %d - %s', [status, response]));
        except
          on E: Exception do
            ShowError('Failed to create article: ' + E.Message);
        end;
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  finally
    editForm.Free;
  end;
end;

procedure TMainForm.btnEditClick(Sender: TObject);
var
  editForm: TArticleEditForm;
  selectedId: TID;
  article: TArticleDto;
  updatedArticle: TArticleDto;
  row: Integer;
  postData, response: RawUtf8;
  status: Integer;
  doc: TDocVariantData;
begin
  selectedId := GetSelectedArticleId;
  if selectedId = 0 then
  begin
    ShowMessage('Please select an article to edit');
    Exit;
  end;

  // Find selected article in array
  row := gridArticles.Row;
  if (row < 1) or (row > Length(fArticles)) then
    Exit;
  article := fArticles[row - 1];

  editForm := TArticleEditForm.Create(Self);
  try
    editForm.IsNewArticle := False;
    editForm.Article := article;
    if editForm.ShowModal = mrOk then
    begin
      updatedArticle := editForm.Article;
      ShowStatus('Updating article...');
      Screen.Cursor := crHourGlass;
      try
        try
          // Build JSON body: {"id":X,"article":{...}}
          doc.InitObject(['id', selectedId, 'article', _ObjFast([
            'code', updatedArticle.Code,
            'description', updatedArticle.Description,
            'price', updatedArticle.Price
          ])]);
          postData := doc.ToJson;

          // Call POST ArticlesApi.Update
          status := fClient.CallBack(mPOST, 'ArticlesApi.Update', postData, response);

          if status = 200 then
          begin
            ShowStatus('Article updated successfully');
            LoadArticles; // Refresh list
          end
          else
            ShowError(Format('Failed to update article: HTTP %d - %s', [status, response]));
        except
          on E: Exception do
            ShowError('Failed to update article: ' + E.Message);
        end;
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  finally
    editForm.Free;
  end;
end;

procedure TMainForm.btnDeleteClick(Sender: TObject);
var
  selectedId: TID;
  row: Integer;
  article: TArticleDto;
  postData, response: RawUtf8;
  status: Integer;
  doc: TDocVariantData;
begin
  selectedId := GetSelectedArticleId;
  if selectedId = 0 then
  begin
    ShowMessage('Please select an article to delete');
    Exit;
  end;

  // Find selected article in array for display
  row := gridArticles.Row;
  if (row < 1) or (row > Length(fArticles)) then
    Exit;
  article := fArticles[row - 1];

  if MessageDlg(Format('Delete article "%s" (ID %d)?',
    [Utf8ToString(article.Description), selectedId]),
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    ShowStatus('Deleting article...');
    Screen.Cursor := crHourGlass;
    try
      try
        // Build JSON body: {"id":X}
        doc.InitObject(['id', selectedId]);
        postData := doc.ToJson;

        // Call POST ArticlesApi.Delete
        status := fClient.CallBack(mPOST, 'ArticlesApi.Delete', postData, response);

        if status = 200 then
        begin
          ShowStatus('Article deleted successfully');
          LoadArticles; // Refresh list
        end
        else
          ShowError(Format('Failed to delete article: HTTP %d - %s', [status, response]));
      except
        on E: Exception do
          ShowError('Failed to delete article: ' + E.Message);
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TMainForm.btnSearchClick(Sender: TObject);
begin
  pnlSearch.Visible := not pnlSearch.Visible;
  if pnlSearch.Visible then
    edtSearch.SetFocus;
end;

procedure TMainForm.btnSearchExecuteClick(Sender: TObject);
var
  query: RawUtf8;
  postData, response: RawUtf8;
  status: Integer;
  doc, queryDoc: TDocVariantData;
  item: PDocVariantData;
  i: Integer;
begin
  query := Trim(StringToUtf8(edtSearch.Text));
  if query = '' then
  begin
    ShowMessage('Please enter a search term');
    Exit;
  end;

  ShowStatus('Searching articles...');
  Screen.Cursor := crHourGlass;
  try
    try
      // Build JSON body: {"query":"..."}
      queryDoc.InitObject(['query', query]);
      postData := queryDoc.ToJson;

      // Call POST ArticlesApi.Search
      status := fClient.CallBack(mPOST, 'ArticlesApi.Search', postData, response);

      if status = 200 then
      begin
        // Parse JSON array response
        doc.InitJson(response, JSON_FAST);
        SetLength(fArticles, doc.Count);
        for i := 0 to doc.Count - 1 do
        begin
          item := _Safe(doc.Value[i]);
          fArticles[i].ID := item^.I['id'];
          fArticles[i].Code := item^.U['code'];
          fArticles[i].Description := item^.U['description'];
          fArticles[i].Price := StrToCurr(Utf8ToString(item^.U['price']));
          fArticles[i].CreatedAt := Iso8601ToDateTime(item^.U['createdat']);
          fArticles[i].UpdatedAt := Iso8601ToDateTime(item^.U['updatedat']);
        end;
        DisplayArticles;
        ShowStatus(Format('Found %d articles matching "%s"',
          [Length(fArticles), Utf8ToString(query)]));
      end
      else
      begin
        ShowError(Format('Failed to search articles: HTTP %d - %s', [status, response]));
        SetLength(fArticles, 0);
        DisplayArticles;
      end;
    except
      on E: Exception do
      begin
        ShowError('Failed to search articles: ' + E.Message);
        SetLength(fArticles, 0);
        DisplayArticles;
      end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.btnSearchClearClick(Sender: TObject);
begin
  edtSearch.Clear;
  pnlSearch.Visible := False;
  LoadArticles; // Reload all articles
end;

procedure TMainForm.gridArticlesDblClick(Sender: TObject);
begin
  // Double-click opens edit form
  if GetSelectedArticleId <> 0 then
    btnEditClick(Sender);
end;

end.
