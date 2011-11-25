unit BusinessObjects;

interface

uses
  Generics.Collections, dorm.Collections;

type
  TPerson = class;

  TLaptop = class
  private
    FRAM: Integer;
    FCores: Integer;
    FModel: String;
    FPersonID: Integer;
    FID: Integer;
    FOwner: TPerson;
    procedure SetCores(const Value: Integer);
    procedure SetModel(const Value: String);
    procedure SetRAM(const Value: Integer);
    procedure SetID(const Value: Integer);
    procedure SetOwner(const Value: TPerson);
    property PersonID: Integer read FPersonID write FPersonID;
  protected
    class procedure Register;
  public
    constructor Create(AModel: String; ARAM: Integer; ACores: Integer);
      overload;
    property Owner: TPerson read FOwner write SetOwner;
    property ID: Integer read FID write SetID;
    property Model: String read FModel write SetModel;
    property RAM: Integer read FRAM write SetRAM;
    property Cores: Integer read FCores write SetCores;
  end;

  TPerson = class
  private
    FID: Integer;
    FAge: Integer;
    FLastName: String;
    FFirstName: String;
    FLaptops: TdormCollection;
    procedure SetAge(const Value: Integer);
    procedure SetFirstName(const Value: String);
    procedure SetLastName(const Value: String);
    function GetIsAdult: Boolean;
    procedure SetLaptops(const Value: TdormCollection);
  public
    constructor Create; overload;
    constructor Create(FirstName, LastName: String; Age: Integer); overload;
    destructor Destroy; override;
    property ID: Integer read FID write FID;
    property FirstName: String read FFirstName write SetFirstName;
    property LastName: String read FLastName write SetLastName;
    property Age: Integer read FAge write SetAge;
    property IsAdult: Boolean read GetIsAdult;
    property Laptops: TdormCollection read FLaptops write SetLaptops;
  end;

implementation

uses
  System.Bindings.Helper, System.SysUtils;

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

destructor TPerson.Destroy;
begin
  FreeAndNil(FLaptops);
  inherited;
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

procedure TPerson.SetLaptops(const Value: TdormCollection);
begin
  FreeAndNil(FLaptops);
  FLaptops := Value;
end;

procedure TPerson.SetLastName(const Value: String);
begin
  FLastName := Value;
end;

{ TLaptop }

constructor TLaptop.Create(AModel: String; ARAM, ACores: Integer);
begin
  inherited Create;
  FModel := AModel;
  FRAM := ARAM;
  FCores := ACores;
end;

class procedure TLaptop.Register;
begin
  //
end;

procedure TLaptop.SetCores(const Value: Integer);
begin
  FCores := Value;
end;

procedure TLaptop.SetID(const Value: Integer);
begin
  FID := Value;
end;

procedure TLaptop.SetModel(const Value: String);
begin
  FModel := Value;
end;

procedure TLaptop.SetOwner(const Value: TPerson);
begin
  FOwner := Value;
  if Value <> nil then
    PersonID := Value.ID
  else
    PersonID := 0;
end;

procedure TLaptop.SetRAM(const Value: Integer);
begin
  FRAM := Value;
end;

initialization

TLaptop.Register;

end.
