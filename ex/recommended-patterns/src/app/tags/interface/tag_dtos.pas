
unit tag_dtos;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.rtti,
  shared_types;

type
  /// DTO for displaying a single tag (read side)
  TTagViewDTO = packed record
    ID: TID;
    Name: RawUtf8;
    Color: RawUtf8;
    CreatedAt: TDateTime;
  end;
  TTagViewDTODynArray = array of TTagViewDTO;

  /// DTO for creating a tag (write side)
  TTagCreateDTO = packed record
    Name: RawUtf8;
    Color: RawUtf8;
    /// Unused padding — DO NOT remove.
    // Works around a mORMot SOA quirk: a service method whose sole input is a
    // record with *exactly two* fields deserializes to all-empty fields over
    // HTTP (the parser still reports success). A third field of any type
    // restores correct parsing; clients may omit it on the wire.
    Reserved: RawUtf8;
  end;

  /// DTO for updating a tag (write side)
  TTagUpdateDTO = packed record
    TagID: TID;
    Name: RawUtf8;
    Color: RawUtf8;
  end;

implementation

initialization
  Rtti.RegisterFromText(TypeInfo(TTagViewDTO),
    'ID Int64 Name RawUtf8 Color RawUtf8 CreatedAt TDateTime');
  Rtti.RegisterFromText(TypeInfo(TTagCreateDTO),
    'Name RawUtf8 Color RawUtf8 Reserved RawUtf8');
  Rtti.RegisterFromText(TypeInfo(TTagUpdateDTO),
    'TagID Int64 Name RawUtf8 Color RawUtf8');

  // camelCase the wire contract on the DTOs only (Recommended Patterns A.7,
  // Gotcha 3) — after RegisterFromText built the fields these calls rename.
  Rtti[TypeInfo(TTagViewDTO)].Props.NameChangeCase(DtoJsonCase);
  Rtti[TypeInfo(TTagCreateDTO)].Props.NameChangeCase(DtoJsonCase);
  Rtti[TypeInfo(TTagUpdateDTO)].Props.NameChangeCase(DtoJsonCase);

end.
