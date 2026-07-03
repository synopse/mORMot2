
unit tag_query;

{$mode objfpc}{$H+}

interface

uses
  mormot.core.base,
  mormot.core.interfaces,
  tag_dtos;

type
  /// CQRS Query interface — read-optimized operations for Tag context
  ITagQuery = interface(IInvokable)
    ['{B2C3D4E5-1111-2222-3333-444455556677}']
    function GetTagView(aTagID: TID): TTagViewDTO;
    function ListTags: TTagViewDTODynArray;
    function SearchTags(const aSearchTerm: RawUtf8): TTagViewDTODynArray;
  end;

implementation

end.
