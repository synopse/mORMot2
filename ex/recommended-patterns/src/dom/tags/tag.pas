
unit tag;

{ Tag aggregate (Recommended Patterns B.3 / B.6.1).

  The TOrm doubling as the domain object lives in dom/ and depends only on the
  ORM *base type* (mormot.orm.core), never the ORM *runtime*. The IRestOrm-based
  lazy migration is a persistence concern and lives in infra/tag_repository_orm. }

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.data,
  mormot.core.rtti,
  mormot.orm.core;

const
  TAG_CURRENT_VERSION = 1;

type
  TTag = class;
  /// Object-owning dynamic array of TTag; registered as a T*ObjArray so
  /// `IList<TTag>` storage frees its items automatically on release.
  TTagObjArray = array of TTag;

  /// Tag aggregate — standalone entity referenced by Task.TagIDs
  TTag = class(TOrm)
  private
    fName: RawUtf8;
    fColor: RawUtf8;
    fCreatedAt: TDateTime;
    fSchemaVersion: integer;
  published
    property Name: RawUtf8 read fName write fName;
    property Color: RawUtf8 read fColor write fColor;
    property CreatedAt: TDateTime read fCreatedAt write fCreatedAt;
    property SchemaVersion: integer read fSchemaVersion write fSchemaVersion;
  end;

function IsValidColor(const Color: RawUtf8): boolean;
function GetDefaultColor: RawUtf8;

implementation

function IsValidColor(const Color: RawUtf8): boolean;
begin
  Result := (Length(Color) > 0) and
            ((Color[1] = '#') or (Pos('#', Color) = 0));
end;

function GetDefaultColor: RawUtf8;
begin
  Result := '#3498db';
end;

initialization
  Rtti.RegisterObjArray(TypeInfo(TTagObjArray), TTag);

end.
