unit BusinessObjects;

interface

uses
  Generics.Collections,
  dorm.Collections,
  SysUtils;

type
  TPerson = class;

  TPeople = class(TObjectList<TPerson>)
  public
    function GetElement(const index: Integer): TPerson;
  end;

  TLaptop = class
  private
    FRAM: Integer;
    FCores: Integer;
    FModel: string;
    FPersonID: Integer;
    FID: Integer;
    FOwner: TPerson;
    procedure SetCores(const Value: Integer);
    procedure SetModel(const Value: string);
    procedure SetRAM(const Value: Integer);
    procedure SetID(const Value: Integer);
    procedure SetOwner(const Value: TPerson);
    property PersonID: Integer read FPersonID write FPersonID;
  protected
    class procedure register;
  public
    constructor Create(AModel: string; ARAM: Integer; ACores: Integer);
      overload;
    property Owner: TPerson read FOwner write SetOwner;
    property ID: Integer read FID write SetID;
    property Model: string read FModel write SetModel;
    property RAM: Integer read FRAM write SetRAM;
    property Cores: Integer read FCores write SetCores;
  end;

  TLaptops = class(TObjectList<TLaptop>)
  public
    function GetElement(const index: Integer): TLaptop;
  end;

  TPerson = class
  private
    FID: Integer;
    FAge: Integer;
    FLastName: string;
    FFirstName: string;
    FLaptops: TLaptops;
    procedure SetAge(const Value: Integer);
    procedure SetFirstName(const Value: string);
    procedure SetLastName(const Value: string);
    function GetIsAdult: Boolean;
    procedure Init;
  public
    constructor Create; overload;
    constructor Create(FirstName, LastName: string; Age: Integer); overload;
    destructor Destroy; override;
    property ID: Integer read FID write FID;
    property FirstName: string read FFirstName write SetFirstName;
    property LastName: string read FLastName write SetLastName;
    property Age: Integer read FAge write SetAge;
    property IsAdult: Boolean read GetIsAdult;
    property Laptops: TLaptops read FLaptops;
  end;

implementation

{ TPerson }

constructor TPerson.Create;
begin
  inherited;
  Init;
  FirstName := 'Daniele';
  LastName := 'Teti';
  Age := 32;
end;

constructor TPerson.Create(FirstName, LastName: string; Age: Integer);
begin
  inherited Create;
  Init;
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

procedure TPerson.Init;
begin
  FLaptops := TLaptops.Create;
end;

procedure TPerson.SetAge(const Value: Integer);
begin
  FAge := Value;
end;

procedure TPerson.SetFirstName(const Value: string);
begin
  FFirstName := Value;
end;

procedure TPerson.SetLastName(const Value: string);
begin
  FLastName := Value;
end;

{ TLaptop }

constructor TLaptop.Create(AModel: string; ARAM, ACores: Integer);
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

procedure TLaptop.SetModel(const Value: string);
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

{ TPeople }

function TPeople.GetElement(const index: Integer): TPerson;
begin
  Result := Self.Items[index];
end;

{ TLaptops }

function TLaptops.GetElement(const index: Integer): TLaptop;
begin
  Result := Self.Items[index];
end;

initialization

TLaptop.Register;

end.
