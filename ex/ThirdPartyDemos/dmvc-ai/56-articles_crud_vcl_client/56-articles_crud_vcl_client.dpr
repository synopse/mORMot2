{
  mORMot2 DMVC Port - Sample 56: Articles CRUD VCL Client

  Purpose:
  - VCL client application that consumes the REST API from 06-articles_crud_server
  - Demonstrates using mORMot2 interface-based REST client for CRUD operations
  - Full CRUD interface with grid display and detail editing
  - Implements all article API operations (GetAll, Search, GetById, Create, Update, Delete)

  Features:
  - Interface-based REST client (IArticlesApi)
  - DBGrid display of articles list
  - Detail form for Create/Edit operations
  - Search functionality
  - Business rule validation (code format, price constraints)
  - Automatic timestamp display (CreatedAt, UpdatedAt)

  Ported from DMVCframework articles_crud_vcl_client - December 2024
}

program ArticlesCrudVCLClient;

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$APPTYPE GUI}
{$endif OSWINDOWS}

uses
  {$ifdef MSWINDOWS}
  Windows,
  {$endif}
  SysUtils,
  Forms,
  MainFormU in 'MainFormU.pas' {MainForm},
  ArticleEditFormU in 'ArticleEditFormU.pas' {ArticleEditForm},
  api.interfaces in '..\06-articles_crud_server\src\api.interfaces.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
