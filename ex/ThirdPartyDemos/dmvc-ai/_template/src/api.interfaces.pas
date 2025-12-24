unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.os,
  mormot.core.rtti,
  mormot.core.interfaces;

type
  TSampleID = Int64;

  /// sample DTO (Data Transfer Object)
  TSampleDTO = packed record
    ID: TSampleID;
    Name: RawUtf8;
    Description: RawUtf8;
    CreatedAt: TUnixMSTime;
  end;
  TSampleDTOs = array of TSampleDTO;

  /// Template Sample API interface
  ITemplateSample = interface(IInvokable)
    ['{12345678-1234-1234-1234-123456789ABC}']
    /// create a new sample entry
    function CreateSample(const name, description: RawUtf8): TSampleID;
    /// retrieve a sample by ID
    function GetSample(id: TSampleID): TSampleDTO;
    /// retrieve all samples
    function GetAllSamples: TSampleDTOs;
    /// update an existing sample
    function UpdateSample(id: TSampleID; const name, description: RawUtf8): boolean;
    /// delete a sample by ID
    function DeleteSample(id: TSampleID): boolean;
  end;


implementation


initialization
  // customize the DTO fields (as shorter lowercase)
  Rtti.RegisterFromText(TypeInfo(TSampleDTO),
    'id:Int64 name:RawUtf8 desc:RawUtf8 created:Int64');
  // allow to use directly ITemplateSample type/guid for TypeInfo(ITemplateSample)
  TInterfaceFactory.RegisterInterfaces([TypeInfo(ITemplateSample)]);

end.
