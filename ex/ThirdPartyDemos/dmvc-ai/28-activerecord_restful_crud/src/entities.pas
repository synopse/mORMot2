unit entities;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.data,
  mormot.core.variants,
  mormot.orm.core;

type
  /// ORM class for People table
  // Maps to DMVC's TPerson (TMVCActiveRecord)
  // In mORMot2, TOrm provides similar functionality to ActiveRecord
  TPersonOrm = class(TOrm)
  private
    fLastName: RawUtf8;
    fFirstName: RawUtf8;
    fDOB: TDateTime;
    fIsMale: Boolean;
    fNote: RawUtf8;
  published
    property LastName: RawUtf8 read fLastName write fLastName;
    property FirstName: RawUtf8 read fFirstName write fFirstName;
    property DOB: TDateTime read fDOB write fDOB;
    property IsMale: Boolean read fIsMale write fIsMale;
    property Note: RawUtf8 read fNote write fNote;
  end;

  /// ORM class for Articles table
  // Maps to DMVC's TArticle (TMVCActiveRecord)
  TArticleOrm = class(TOrm)
  private
    fDescription: RawUtf8;
    fPrice: Currency;
  published
    property Description: RawUtf8 read fDescription write fDescription;
    property Price: Currency read fPrice write fPrice;
  end;

  /// ORM class for Phones table
  // Maps to DMVC's TPhone (TMVCActiveRecord)
  TPhoneOrm = class(TOrm)
  private
    fPhoneNumber: RawUtf8;
    fNumberType: RawUtf8;
    fPersonID: TID; // Foreign key to TPersonOrm
  published
    property PhoneNumber: RawUtf8 read fPhoneNumber write fPhoneNumber;
    property NumberType: RawUtf8 read fNumberType write fNumberType;
    property PersonID: TID read fPersonID write fPersonID;
  end;

  // Dynamic array types for ORM objects
  TPersonOrmObjArray = array of TPersonOrm;
  TArticleOrmObjArray = array of TArticleOrm;
  TPhoneOrmObjArray = array of TPhoneOrm;

implementation

// Note: In mORMot2, ORM classes are registered when creating the TOrmModel
// See server.pas: TOrmModel.Create([TPersonOrm, TArticleOrm, TPhoneOrm], 'activerecord')
// This is different from DMVC's ActiveRecord which requires explicit registration

end.
