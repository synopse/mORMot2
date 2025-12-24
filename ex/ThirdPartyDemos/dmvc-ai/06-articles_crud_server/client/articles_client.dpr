program ArticlesClient;

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$APPTYPE CONSOLE}
{$endif OSWINDOWS}

uses
  {$I mormot.uses.inc}
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.unicode,
  mormot.core.text,
  mormot.orm.core,
  mormot.rest.client,
  mormot.rest.http.client,
  api.interfaces in '..\src\api.interfaces.pas';

var
  Client: TRestHttpClient;
  ArticlesApi: IArticlesApi;
  Articles: TArticleDtos;
  Article: TArticleDto;
  NewId: TID;
  i: Integer;

procedure ShowArticle(const art: TArticleDto);
begin
  WriteLn('  ID: ', art.ID);
  WriteLn('  Code: ', Utf8ToString(art.Code));
  WriteLn('  Description: ', Utf8ToString(art.Description));
  WriteLn('  Price: €', FormatFloat('0.00', art.Price));
  WriteLn('  Created: ', DateTimeToStr(art.CreatedAt));
  WriteLn('  Updated: ', DateTimeToStr(art.UpdatedAt));
  WriteLn;
end;

begin
  WriteLn('mORMot2 Articles CRUD Client');
  WriteLn('============================');
  WriteLn;

  // Enable logging
  TSynLog.Family.Level := LOG_VERBOSE;

  try
    // Create HTTP client
    WriteLn('Connecting to http://localhost:8080...');
    Client := TRestHttpClientWinHttp.Create('localhost', '8080', TOrmModel.Create([]));
    try
      // Register and resolve service
      Client.ServiceDefine([IArticlesApi], sicShared);
      if not Client.Services.Resolve(IArticlesApi, ArticlesApi) then
        raise Exception.Create('Failed to resolve IArticlesApi');

      WriteLn('Connected successfully!');
      WriteLn;

      // 1. Get all articles
      WriteLn('=== GET ALL ARTICLES ===');
      Articles := ArticlesApi.GetAll;
      WriteLn('Found ', Length(Articles), ' articles:');
      WriteLn;
      for i := 0 to High(Articles) do
      begin
        WriteLn('Article #', i + 1, ':');
        ShowArticle(Articles[i]);
      end;

      // 2. Search articles
      WriteLn('=== SEARCH ARTICLES (query="Margherita") ===');
      Articles := ArticlesApi.Search('Margherita');
      WriteLn('Found ', Length(Articles), ' matching articles:');
      WriteLn;
      for i := 0 to High(Articles) do
        ShowArticle(Articles[i]);

      // 3. Get article by ID
      WriteLn('=== GET ARTICLE BY ID (id=1) ===');
      try
        Article := ArticlesApi.GetById(1);
        ShowArticle(Article);
      except
        on E: Exception do
          WriteLn('Error: ', E.Message);
      end;

      // 4. Create new article
      WriteLn('=== CREATE NEW ARTICLE ===');
      Article.Code := 'C777';
      Article.Description := 'Test Pizza from Client';
      Article.Price := 15.50;
      try
        NewId := ArticlesApi.Create(Article);
        WriteLn('Created article with ID: ', NewId);
        WriteLn;

        // Get the created article
        Article := ArticlesApi.GetById(NewId);
        WriteLn('Verified created article:');
        ShowArticle(Article);
      except
        on E: Exception do
          WriteLn('Error creating article: ', E.Message);
      end;

      // 5. Update article
      if NewId > 0 then
      begin
        WriteLn('=== UPDATE ARTICLE (id=', NewId, ') ===');
        Article.Description := 'Updated Test Pizza';
        Article.Price := 16.00;
        try
          ArticlesApi.Update(NewId, Article);
          WriteLn('Article updated successfully');
          WriteLn;

          // Verify update
          Article := ArticlesApi.GetById(NewId);
          WriteLn('Verified updated article:');
          ShowArticle(Article);
        except
          on E: Exception do
            WriteLn('Error updating article: ', E.Message);
        end;
      end;

      // 6. Test validation - try invalid code
      WriteLn('=== TEST VALIDATION (invalid code) ===');
      Article.Code := 'INVALID';
      Article.Description := 'Should Fail';
      Article.Price := 10.00;
      try
        ArticlesApi.Create(Article);
        WriteLn('ERROR: Should have failed validation!');
      except
        on E: Exception do
          WriteLn('Validation worked correctly: ', E.Message);
      end;
      WriteLn;

      // 7. Test validation - try low price update
      if NewId > 0 then
      begin
        WriteLn('=== TEST VALIDATION (price too low for update) ===');
        Article.Code := 'C777';
        Article.Description := 'Test';
        Article.Price := 1.50;  // Below 2
        try
          ArticlesApi.Update(NewId, Article);
          WriteLn('ERROR: Should have failed validation!');
        except
          on E: Exception do
            WriteLn('Validation worked correctly: ', E.Message);
        end;
        WriteLn;
      end;

      // 8. Delete article
      if NewId > 0 then
      begin
        WriteLn('=== DELETE ARTICLE (id=', NewId, ') ===');
        try
          ArticlesApi.Delete(NewId);
          WriteLn('Article deleted successfully');
        except
          on E: Exception do
            WriteLn('Error deleting article: ', E.Message);
        end;
        WriteLn;

        // Verify deletion
        WriteLn('Verifying deletion...');
        try
          Article := ArticlesApi.GetById(NewId);
          WriteLn('ERROR: Article still exists!');
        except
          on E: Exception do
            WriteLn('Deletion verified: ', E.Message);
        end;
      end;

      // 9. Get metadata
      WriteLn;
      WriteLn('=== GET ARTICLE METADATA ===');
      var meta := ArticlesApi.GetMeta;
      WriteLn('Field definitions:');
      WriteLn(Utf8ToString(meta.Fields));
      WriteLn;

      // 10. Bulk create
      WriteLn('=== BULK CREATE ARTICLES ===');
      SetLength(Articles, 3);
      Articles[0].Code := 'C888';
      Articles[0].Description := 'Bulk Pizza 1';
      Articles[0].Price := 10.00;
      Articles[1].Code := 'C889';
      Articles[1].Description := 'Bulk Pizza 2';
      Articles[1].Price := 11.00;
      Articles[2].Code := 'C890';
      Articles[2].Description := 'Bulk Pizza 3';
      Articles[2].Price := 12.00;
      try
        var count := ArticlesApi.CreateBulk(Articles);
        WriteLn('Created ', count, ' articles in bulk');
      except
        on E: Exception do
          WriteLn('Error in bulk create: ', E.Message);
      end;

      WriteLn;
      WriteLn('All tests completed successfully!');

    finally
      Client.Free;
    end;

  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      TSynLog.Add.Log(sllError, 'Client error: %', [E.Message]);
      ExitCode := 1;
    end;
  end;

  WriteLn;
  WriteLn('Press [Enter] to exit');
  ReadLn;
end.
