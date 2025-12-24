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
  mormot.core.base,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.data,
  mormot.core.variants,
  mormot.core.os,
  mormot.core.json,
  mormot.core.buffers,
  mormot.net.sock,
  mormot.net.client,
  mormot.net.http;

type
  TMainForm = class(TForm)
    lblTitle: TLabel;
    edtUrl: TEdit;
    btnGet: TButton;
    memoResponse: TMemo;
    lblResult: TLabel;
    chkIgnoreCert: TCheckBox;
    lblUrl: TLabel;
    btnPost: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnGetClick(Sender: TObject);
    procedure btnPostClick(Sender: TObject);
  private
    fClient: TSimpleHttpClient;
    procedure ShowResponse(const aText: RawUtf8);
    procedure ShowError(const aError: string);
    procedure ConfigureClient;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Create HTTP client with SSL/TLS support
  fClient := TSimpleHttpClient.Create;
  memoResponse.Lines.Clear;
  lblResult.Caption := 'Ready - Enter HTTPS URL and click GET or POST';

  // Note: For production, you should properly configure certificates
  // This example shows how to handle both trusted and self-signed certificates
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  fClient.Free;
end;

procedure TMainForm.ConfigureClient;
begin
  // Configure TLS/SSL settings
  if chkIgnoreCert.Checked then
  begin
    // For development/testing with self-signed certificates
    // WARNING: Don't use this in production!
    fClient.Options^.TLS.IgnoreCertificateErrors := True;
    memoResponse.Lines.Add('*** WARNING: TLS certificate validation is DISABLED ***');
    memoResponse.Lines.Add('*** This should only be used in development environments ***');
    memoResponse.Lines.Add('');
  end
  else
  begin
    // For production with proper certificates
    fClient.Options^.TLS.IgnoreCertificateErrors := False;
    // Optional: Configure custom CA certificates
    // fClient.Options^.TLS.CACertificatesFile := 'path/to/ca-bundle.crt';
  end;

  // Optional: Configure other TLS settings
  // fClient.Options^.TLS.AllowDeprecatedTls := False;
  // fClient.Options^.TLS.CertificateFile := 'client-cert.pem';
  // fClient.Options^.TLS.PrivateKeyFile := 'client-key.pem';

  // Set reasonable connection timeout (in milliseconds)
  fClient.Options^.CreateTimeoutMS := 30000; // 30 seconds
end;

procedure TMainForm.ShowResponse(const aText: RawUtf8);
var
  formatted: RawUtf8;
begin
  // Try to format JSON response for better readability
  if (aText <> '') and (aText[1] in ['{', '[']) then
  begin
    formatted := JsonReformat(aText, jsonHumanReadable);
    if formatted <> '' then
      memoResponse.Lines.Add(Utf8ToString(formatted))
    else
      memoResponse.Lines.Add(Utf8ToString(aText));
  end
  else
    memoResponse.Lines.Add(Utf8ToString(aText));

  lblResult.Caption := 'Success - Response received';
end;

procedure TMainForm.ShowError(const aError: string);
begin
  memoResponse.Lines.Add('*** ERROR ***');
  memoResponse.Lines.Add(aError);
  memoResponse.Lines.Add('');
  lblResult.Caption := 'Error occurred - see details above';
  ShowMessage(aError);
end;

procedure TMainForm.btnGetClick(Sender: TObject);
var
  uri: TUri;
  status: integer;
begin
  memoResponse.Lines.Clear;
  lblResult.Caption := 'Calling HTTPS endpoint...';
  Application.ProcessMessages;

  try
    // Parse and validate URL
    if not uri.From(StringToUtf8(edtUrl.Text)) then
    begin
      ShowError('Invalid URL format');
      Exit;
    end;

    if not uri.Https then
    begin
      ShowError('URL must use HTTPS protocol (https://)');
      Exit;
    end;

    // Configure TLS settings
    ConfigureClient;

    // Display request information
    memoResponse.Lines.Add('=== REQUEST ===');
    memoResponse.Lines.Add('Method: GET');
    memoResponse.Lines.Add('URL: ' + edtUrl.Text);
    memoResponse.Lines.Add('Protocol: HTTPS (TLS)');
    memoResponse.Lines.Add('');

    // Perform HTTPS GET request
    status := fClient.Request(uri, 'GET', '', '', '', 10000);

    // Display response
    memoResponse.Lines.Add('=== RESPONSE ===');
    memoResponse.Lines.Add(Format('HTTP Status: %d', [status]));
    memoResponse.Lines.Add('');

    if fClient.Headers <> '' then
    begin
      memoResponse.Lines.Add('--- Response Headers ---');
      memoResponse.Lines.Add(Utf8ToString(fClient.Headers));
      memoResponse.Lines.Add('');
    end;

    if fClient.Body <> '' then
    begin
      memoResponse.Lines.Add('--- Response Body ---');
      ShowResponse(fClient.Body);
    end
    else
      memoResponse.Lines.Add('(empty response)');

    if (status >= 200) and (status < 300) then
      lblResult.Caption := Format('Success - HTTP %d', [status])
    else
      lblResult.Caption := Format('HTTP Error %d', [status]);

  except
    on E: Exception do
    begin
      ShowError('Exception: ' + E.ClassName + ' - ' + E.Message);
    end;
  end;
end;

procedure TMainForm.btnPostClick(Sender: TObject);
var
  uri: TUri;
  status: integer;
  postData: RawUtf8;
  doc: TDocVariantData;
begin
  memoResponse.Lines.Clear;
  lblResult.Caption := 'Calling HTTPS endpoint...';
  Application.ProcessMessages;

  try
    // Parse and validate URL
    if not uri.From(StringToUtf8(edtUrl.Text)) then
    begin
      ShowError('Invalid URL format');
      Exit;
    end;

    if not uri.Https then
    begin
      ShowError('URL must use HTTPS protocol (https://)');
      Exit;
    end;

    // Configure TLS settings
    ConfigureClient;

    // Create sample JSON POST data
    doc.Init;
    doc.AddValue('name', 'John Doe');
    doc.AddValue('email', 'john.doe@example.com');
    doc.AddValue('timestamp', NowUtc);
    postData := doc.ToJson;

    // Display request information
    memoResponse.Lines.Add('=== REQUEST ===');
    memoResponse.Lines.Add('Method: POST');
    memoResponse.Lines.Add('URL: ' + edtUrl.Text);
    memoResponse.Lines.Add('Protocol: HTTPS (TLS)');
    memoResponse.Lines.Add('Content-Type: application/json');
    memoResponse.Lines.Add('');
    memoResponse.Lines.Add('--- Request Body ---');
    memoResponse.Lines.Add(Utf8ToString(JsonReformat(postData, jsonHumanReadable)));
    memoResponse.Lines.Add('');

    // Perform HTTPS POST request
    status := fClient.Request(uri, 'POST', 'Content-Type: application/json'#13#10,
      postData, '', 10000);

    // Display response
    memoResponse.Lines.Add('=== RESPONSE ===');
    memoResponse.Lines.Add(Format('HTTP Status: %d', [status]));
    memoResponse.Lines.Add('');

    if fClient.Headers <> '' then
    begin
      memoResponse.Lines.Add('--- Response Headers ---');
      memoResponse.Lines.Add(Utf8ToString(fClient.Headers));
      memoResponse.Lines.Add('');
    end;

    if fClient.Body <> '' then
    begin
      memoResponse.Lines.Add('--- Response Body ---');
      ShowResponse(fClient.Body);
    end
    else
      memoResponse.Lines.Add('(empty response)');

    if (status >= 200) and (status < 300) then
      lblResult.Caption := Format('Success - HTTP %d', [status])
    else
      lblResult.Caption := Format('HTTP Error %d', [status]);

  except
    on E: Exception do
    begin
      ShowError('Exception: ' + E.ClassName + ' - ' + E.Message);
    end;
  end;
end;

end.
