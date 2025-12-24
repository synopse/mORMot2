unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.interfaces,
  mormot.core.variants,
  entities;

type
  /// RESTful CRUD service interface for ActiveRecord pattern
  // Demonstrates mORMot2 ORM operations similar to DMVC ActiveRecord
  IActiveRecordApi = interface(IInvokable)
    ['{8F2A3B4C-5D6E-7F8A-9B0C-1D2E3F4A5B6C}']

    // Person CRUD operations
    function GetPerson(id: TID): Variant;
    function GetAllPeople: TVariantDynArray;
    function CreatePerson(const lastName, firstName, note: RawUtf8;
      dob: TDateTime; isMale: Boolean): TID;
    function UpdatePerson(id: TID; const lastName, firstName, note: RawUtf8;
      dob: TDateTime; isMale: Boolean): Boolean;
    function DeletePerson(id: TID): Boolean;

    // Article CRUD operations
    function GetArticle(id: TID): Variant;
    function GetAllArticles: TVariantDynArray;
    function CreateArticle(const description: RawUtf8; price: Currency): TID;
    function UpdateArticle(id: TID; const description: RawUtf8;
      price: Currency): Boolean;
    function DeleteArticle(id: TID): Boolean;

    // Phone CRUD operations
    function GetPhone(id: TID): Variant;
    function GetPhonesByPerson(personId: TID): TVariantDynArray;
    function CreatePhone(personId: TID; const phoneNumber,
      numberType: RawUtf8): TID;
    function DeletePhone(id: TID): Boolean;
  end;

implementation

end.
