unit entities;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.unicode,
  mormot.orm.base,
  mormot.orm.core;

type
  /// Article entity - demonstrates ORM capabilities
  TArticle = class(TOrm)
  private
    fTitle: RawUtf8;
    fAuthor: RawUtf8;
    fContent: RawUtf8;
    fCreatedAt: TDateTime;
    fUpdatedAt: TDateTime;
    fViewCount: Integer;
    fPublished: Boolean;
  published
    property Title: RawUtf8 read fTitle write fTitle;
    property Author: RawUtf8 read fAuthor write fAuthor;
    property Content: RawUtf8 read fContent write fContent;
    property CreatedAt: TDateTime read fCreatedAt write fCreatedAt;
    property UpdatedAt: TDateTime read fUpdatedAt write fUpdatedAt;
    property ViewCount: Integer read fViewCount write fViewCount;
    property Published: Boolean read fPublished write fPublished;
  end;

implementation

initialization
  // Register entity for ORM
  TArticle.AutoFree([TArticle]);

end.
