
unit tag_repository;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.collections,
  mormot.core.interfaces,
  mormot.orm.core,
  tag;

type
  /// Persistence-port for the Tag aggregate.
  /// Hides the IRestOrm / SQL / migration choices: reads return aggregates
  /// already migrated to the current schema, so the app layer never has to
  /// remember to migrate.
  /// Returns `IList<TTag>`; items are freed when the interface ref-count
  /// drops (TTagObjArray is registered as a T*ObjArray).
  /// Caller still owns TTag instances returned by GetByID.
  ITagRepository = interface(IInvokable)
    ['{D2E3F4A5-1111-2222-3333-444455556666}']
    /// Load one tag by ID (migrated to the current schema), nil if missing.
    function GetByID(aID: TID): TTag;
    function Add(aTag: TTag): TID;
    function Update(aTag: TTag): boolean;
    function Delete(aID: TID): boolean;
    /// List all tags, each migrated to the current schema.
    function List: IList<TTag>;
    function FindByName(const aName: RawUtf8): IList<TTag>;
    function FindByNameExcludingID(const aName: RawUtf8; aExcludeID: TID): IList<TTag>;
    /// Name search, each result migrated to the current schema.
    function SearchByLike(const aPattern: RawUtf8): IList<TTag>;
  end;

implementation

end.
