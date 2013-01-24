unit BusinessObjects;

interface

uses
  Generics.Collections,
  dorm.Mappings,
  dorm.Collections,
  dorm.Commons,
  dorm.ObjectStatus;

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
    FObjStatus: TdormObjectStatus;
    procedure SetCores(const Value: Integer);
    procedure SetModel(const Value: String);
    procedure SetRAM(const Value: Integer);
    procedure SetID(const Value: Integer);
    procedure SetOwner(const Value: TPerson);
    procedure SetObjStatus(const Value: TdormObjectStatus);
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
    [Transient]
    property ObjStatus: TdormObjectStatus read FObjStatus write SetObjStatus;
  end;

  TPerson = class
  private
    FID: Integer;
    FAge: Integer;
    FLastName: String;
    FFirstName: String;
    FLaptops: TObjectList<TLaptop>;
    FObjStatus: TdormObjectStatus;
    procedure SetAge(const Value: Integer);
    procedure SetFirstName(const Value: String);
    procedure SetLastName(const Value: String);
    function GetIsAdult: Boolean;
    procedure Init;
    procedure SetObjStatus(const Value: TdormObjectStatus);

  public
    constructor Create; overload;
    constructor Create(FirstName, LastName: String; Age: Integer); overload;
    destructor Destroy; override;
    property ID: Integer read FID write FID;
    property FirstName: String read FFirstName write SetFirstName;
    property LastName: String read FLastName write SetLastName;
    property Age: Integer read FAge write SetAge;
    [Transient]
    property IsAdult: Boolean read GetIsAdult;
    property Laptops: TObjectList<TLaptop> read FLaptops;
    [Transient]
    property ObjStatus: TdormObjectStatus read FObjStatus write SetObjStatus;
  end;

implementation

uses
  System.Bindings.Helper,
  System.SysUtils;

{ TPerson }

constructor TPerson.Create;
begin
  inherited;
  Init;
  FirstName := 'Daniele';
  LastName := 'Teti';
  Age := 32;
end;

constructor TPerson.Create(FirstName, LastName: String; Age: Integer);
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
  FLaptops := TObjectList<TLaptop>.Create(true);
end;

procedure TPerson.SetAge(const Value: Integer);
begin
  FAge := Value;
  TBindings.Notify(self, '');
  ObjStatus := osDirty;
end;

procedure TPerson.SetFirstName(const Value: String);
begin
  FFirstName := Value;
  ObjStatus := osDirty;
end;

procedure TPerson.SetLastName(const Value: String);
begin
  FLastName := Value;
  ObjStatus := osDirty;
end;

procedure TPerson.SetObjStatus(const Value: TdormObjectStatus);
begin
  FObjStatus := Value;
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
  ObjStatus := osDirty;
end;

procedure TLaptop.SetID(const Value: Integer);
begin
  FID := Value;
  ObjStatus := osDirty;
end;

procedure TLaptop.SetModel(const Value: String);
begin
  FModel := Value;
  ObjStatus := osDirty;
end;

procedure TLaptop.SetObjStatus(const Value: TdormObjectStatus);
begin
  FObjStatus := Value;
end;

procedure TLaptop.SetOwner(const Value: TPerson);
begin
  FOwner := Value;
  if Value <> nil then
    PersonID := Value.ID
  else
    PersonID := 0;
  ObjStatus := osDirty;
end;

procedure TLaptop.SetRAM(const Value: Integer);
begin
  FRAM := Value;
  ObjStatus := osDirty;
end;

initialization

TLaptop.Register;

end.
