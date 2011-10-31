unit BusinessObjects;

interface

uses
  Generics.Collections;

type
  TPerson = class
  private
    FID: Integer;
    FAge: Integer;
    FLastName: String;
    FFirstName: String;
    procedure SetAge(const Value: Integer);
    procedure SetFirstName(const Value: String);
    procedure SetLastName(const Value: String);
    function GetIsAdult: Boolean;
  public
    constructor Create; overload;
    constructor Create(FirstName, LastName: String; Age: Integer); overload;
    property ID: Integer read FID write FID;
    property FirstName: String read FFirstName write SetFirstName;
    property LastName: String read FLastName write SetLastName;
    property Age: Integer read FAge write SetAge;
    property IsAdult: Boolean read GetIsAdult;
  end;

  TPeople = class(TObjectList<TPerson>)
  end;

implementation

uses
  System.Bindings.Helper;

{ TPerson }

constructor TPerson.Create;
begin
  inherited;
  FirstName := 'Daniele';
  LastName := 'Teti';
  Age := 32;
end;

constructor TPerson.Create(FirstName, LastName: String; Age: Integer);
begin
  inherited Create;
  FFirstName := FirstName;
  FLastName := LastName;
  FAge := Age;
end;

function TPerson.GetIsAdult: Boolean;
begin
  Result := FAge >= 18;
end;

procedure TPerson.SetAge(const Value: Integer);
begin
  FAge := Value;
  TBindings.Notify(self, '');
end;

procedure TPerson.SetFirstName(const Value: String);
begin
  FFirstName := Value;
end;

procedure TPerson.SetLastName(const Value: String);
begin
  FLastName := Value;
end;

end.
