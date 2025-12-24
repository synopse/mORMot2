unit BusinessObjectsU;

interface

uses
  System.Classes,
  mormot.core.base;

type
  TPerson = class(TPersistent)
  private
    FLastName: RawUtf8;
    FDOB: TDateTime;
    FFirstName: RawUtf8;
    FMarried: Boolean;
  published
    property FirstName: RawUtf8 read FFirstName write FFirstName;
    property LastName: RawUtf8 read FLastName write FLastName;
    property DOB: TDateTime read FDOB write FDOB;
    property Married: Boolean read FMarried write FMarried;
  end;

implementation

end.
