unit entities;

interface

type
  TPerson = class
  private
    FFirstName: string;
    FLastName: string;
    FDOB: TDate;
    FMarried: Boolean;
  public
    property FirstName: string read FFirstName write FFirstName;
    property LastName: string read FLastName write FLastName;
    property DOB: TDate read FDOB write FDOB;
    property Married: Boolean read FMarried write FMarried;
  end;

implementation

end.
