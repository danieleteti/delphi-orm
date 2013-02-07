unit BObjectsU;

interface

uses
  dorm, dorm.Commons, dorm.Mappings, dorm.ObjectStatus;

type
  [Entity('PEOPLE')]
  TPerson = class
  private
    FLastName: String;
    FID: Integer;
    FFirstName: String;
    FObjStatus: TdormObjectStatus;
    FDateOfBirth: TDate;
    procedure SetFirstName(const Value: String);
    procedure SetID(const Value: Integer);
    procedure SetLastName(const Value: String);
    procedure SetObjStatus(const Value: TdormObjectStatus);
    procedure SetDateOfBirth(const Value: TDate);
  public
    property ID: Integer read FID write SetID;
    [Transient]
    property ObjStatus: TdormObjectStatus read FObjStatus write SetObjStatus;
    property FirstName: String read FFirstName write SetFirstName;
    property LastName: String read FLastName write SetLastName;
    [Column('DOB')]
    property DateOfBirth: TDate read FDateOfBirth write SetDateOfBirth;
  end;

implementation

{ TPerson }

procedure TPerson.SetDateOfBirth(const Value: TDate);
begin
  FDateOfBirth := Value;
end;

procedure TPerson.SetFirstName(const Value: String);
begin
  FFirstName := Value;
end;

procedure TPerson.SetID(const Value: Integer);
begin
  FID := Value;
end;

procedure TPerson.SetLastName(const Value: String);
begin
  FLastName := Value;
end;

procedure TPerson.SetObjStatus(const Value: TdormObjectStatus);
begin
  FObjStatus := Value;
end;

end.
