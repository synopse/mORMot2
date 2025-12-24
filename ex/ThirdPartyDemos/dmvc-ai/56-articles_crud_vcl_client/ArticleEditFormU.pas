unit ArticleEditFormU;

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
  mormot.core.base,
  mormot.core.text,
  mormot.core.unicode,
  api.interfaces;

type
  TArticleEditForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    edtCode: TEdit;
    edtDescription: TEdit;
    edtPrice: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    lblValidation: TLabel;
    lblInfo: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fIsNewArticle: Boolean;
    fArticle: TArticleDto;
    function ValidateArticle: Boolean;
  public
    property IsNewArticle: Boolean read fIsNewArticle write fIsNewArticle;
    property Article: TArticleDto read fArticle write fArticle;
  end;

var
  ArticleEditForm: TArticleEditForm;

implementation

{$R *.dfm}

procedure TArticleEditForm.FormCreate(Sender: TObject);
begin
  lblValidation.Caption := '';
  fIsNewArticle := True;
end;

function TArticleEditForm.ValidateArticle: Boolean;
var
  code: RawUtf8;
  price: Currency;
  codeLen: Integer;
  i: Integer;
begin
  Result := False;
  lblValidation.Caption := '';

  // Validate code format: CXX, CXXX, or CXXXX
  code := Trim(StringToUtf8(edtCode.Text));
  if code = '' then
  begin
    lblValidation.Caption := 'Code is required';
    edtCode.SetFocus;
    Exit;
  end;

  codeLen := Length(code);
  if (codeLen < 3) or (codeLen > 5) then
  begin
    lblValidation.Caption := 'Code must be 3-5 characters (CXX, CXXX, or CXXXX)';
    edtCode.SetFocus;
    Exit;
  end;

  if code[1] <> 'C' then
  begin
    lblValidation.Caption := 'Code must start with "C"';
    edtCode.SetFocus;
    Exit;
  end;

  for i := 2 to codeLen do
  begin
    if not (code[i] in ['0'..'9']) then
    begin
      lblValidation.Caption := 'Code must be in format CXX, CXXX, or CXXXX (digits only after C)';
      edtCode.SetFocus;
      Exit;
    end;
  end;

  // Validate description
  if Trim(edtDescription.Text) = '' then
  begin
    lblValidation.Caption := 'Description is required';
    edtDescription.SetFocus;
    Exit;
  end;

  // Validate price
  if not TryStrToCurr(edtPrice.Text, price) then
  begin
    lblValidation.Caption := 'Invalid price format';
    edtPrice.SetFocus;
    Exit;
  end;

  if price <= 0 then
  begin
    lblValidation.Caption := 'Price must be greater than 0';
    edtPrice.SetFocus;
    Exit;
  end;

  // Additional validation for updates (price > 2)
  if not fIsNewArticle and (price <= 2) then
  begin
    lblValidation.Caption := 'Price must be greater than 2 for updates';
    edtPrice.SetFocus;
    Exit;
  end;

  Result := True;
end;

procedure TArticleEditForm.btnOKClick(Sender: TObject);
begin
  if not ValidateArticle then
  begin
    ModalResult := mrNone;
    Exit;
  end;

  // Build article DTO
  fArticle.Code := StringToUtf8(Trim(edtCode.Text));
  fArticle.Description := StringToUtf8(Trim(edtDescription.Text));
  fArticle.Price := StrToCurr(edtPrice.Text);

  // Note: ID, CreatedAt, UpdatedAt are managed by server
  ModalResult := mrOk;
end;

end.
