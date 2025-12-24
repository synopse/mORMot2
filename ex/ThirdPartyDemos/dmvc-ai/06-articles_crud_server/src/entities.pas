unit entities;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.text,
  mormot.orm.core;

type
  /// Article entity
  // Port of DMVC TArticle (BusinessObjects.pas) to mORMot2 ORM
  // Maps to ARTICOLI table with fields: ID, CODICE, DESCRIZIONE, PREZZO, CREATED_AT, UPDATED_AT
  TOrmArticle = class(TOrm)
  private
    fCode: RawUtf8;
    fDescription: RawUtf8;
    fPrice: Currency;
    fCreatedAt: TDateTime;
    fUpdatedAt: TDateTime;
  published
    /// Article code (must match format "CXX" or "CXXX" or "CXXXX")
    property Code: RawUtf8 index 20 read fCode write fCode;
    /// Article description
    property Description: RawUtf8 index 200 read fDescription write fDescription;
    /// Article price (must be > 2 for updates, > 5 for deletes)
    property Price: Currency read fPrice write fPrice;
    /// Creation timestamp (auto-set on insert)
    property CreatedAt: TDateTime read fCreatedAt write fCreatedAt;
    /// Update timestamp (auto-set on update)
    property UpdatedAt: TDateTime read fUpdatedAt write fUpdatedAt;
  end;

  /// Array of TOrmArticle objects
  TOrmArticles = array of TOrmArticle;


implementation


end.
