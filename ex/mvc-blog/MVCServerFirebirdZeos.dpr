// MVC sample web application, publishing a simple BLOG
// - this program is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
program MVCServerFirebirdZeos;

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$apptype console}
  {$R mormot.win.default.manifest.res}
{$endif OSWINDOWS}

uses
  {$I mormot.uses.inc}

  MVCModel     in 'MVCModel.pas',
  MVCViewModel in 'MVCViewModel.pas',

  sysutils,
  mormot.core.base,
  mormot.core.rtti,
  mormot.core.os,
  mormot.db.raw.sqlite3,
  mormot.db.raw.sqlite3.static,
  mormot.db.sql.zeos,
  mormot.orm.core,
  mormot.orm.sql,
  mormot.rest.http.server,
  mormot.rest.sqlite3,
  mormot.core.log, zdbc;


var
  aModel: TOrmModel;
  aExternalDB: TSqlDBZeosConnectionProperties;
  aServer: TRestServerDB;
  aApplication: TBlogApplication;
  aHTTPServer: TRestHttpServer;
  LogFamily: TSynLogFamily;

begin
  LogFamily := TSynLog.Family;
  LogFamily.Level := LOG_VERBOSE;
  LogFamily.PerThreadLog := ptIdentifiedInOnFile;
  //LogFamily.EchoToConsole := LOG_VERBOSE;
  aModel := CreateModel;
  try
    aExternalDB := TSqlDBZeosConnectionProperties.Create(
      'zdbc:firebird://localhost:3033/c:\temp\mvc_blog.fdb?'+
      'username=SYSDBA;'+
      'password=masterkey;'+
      'LibLocation=C:\Firebird\firebird3_32\fbclient.dll;'+
      'hard_commit=true', '', '', '' );
    try
      VirtualTableExternalRegisterAll(aModel,aExternalDB, [regMapAutoKeywordFields]);
      aServer := TRestServerDB.Create(aModel, SQLITE_MEMORY_DATABASE_NAME);
      try
        aServer.Server.CreateMissingTables;
        aApplication := TBlogApplication.Create;
        try
          aApplication.Start(aServer);
          aApplication.HasFts:=False;
          aHTTPServer := TRestHttpServer.Create('8092', aServer
            {$ifdef USEHTTPSYS}, '+', useHttpApiRegisteringURI{$endif});
          try
            aHTTPServer.RootRedirectToURI('blog/default'); // redirect / to blog/default
            aServer.RootRedirectGet := 'blog/default';     // redirect blog to blog/default
            writeln('"MVC Blog Server" launched on port 8092 using ',
              aHttpServer.HttpServer.ClassName);
            writeln(#10'You can check http://localhost:8092/blog/mvc-info for information');
            writeln('or point to http://localhost:8092 to access the web app.');
            writeln(#10'Press [Enter] to close the server.'#10);
            readln;
            writeln('HTTP server shutdown...');
          finally
            aHTTPServer.Free;
          end;
        finally
          aApplication.Free;
        end;
      finally
        aServer.Free;
      end;
    finally
      aExternalDB.Free;
    end;
  finally
    aModel.Free;
  end;
  writeln('HTTP server finalized. Bye!');
  {$ifdef FPC_X64MM}
  WriteHeapStatus(' ', 16, 8, {compileflags=}true);
  {$endif FPC_X64MM}
end.

