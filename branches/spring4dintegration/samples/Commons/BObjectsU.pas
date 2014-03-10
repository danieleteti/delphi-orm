unit BObjectsU;

interface

uses
  dorm.mappings, dorm.ObjectStatus, dorm, System.Generics.Collections,
  System.Classes;

type

  [Entity('CUSTOMERS')]
  TCustomer = class
  private
    FName: String;
    FEMail: String;
    FID: Integer;
    FCreatedAt: TDate;
    FADDRESS: String;
    procedure SetADDRESS(const Value: String);
    procedure SetCreatedAt(const Value: TDate);
    procedure SetEMail(const Value: String);
    procedure SetID(const Value: Integer);
    procedure SetName(const Value: String);
  public
    property ID: Integer read FID write SetID;
    property Name: String read FName write SetName;
    property Address: String read FADDRESS write SetADDRESS;
    property EMail: String read FEMail write SetEMail;
    [Column('CREATED_AT')]
    property CreatedAt: TDate read FCreatedAt write SetCreatedAt;
  end;

  [Entity('CUSTOMERS_OS')]
  TCustomerOS = class(TCustomer)
  private
    FObjStatus: TdormObjectStatus;
    procedure SetObjStatus(const Value: TdormObjectStatus);
  public
    // this property change completely the way you works with DORM
    [Transient]
    property ObjStatus: TdormObjectStatus read FObjStatus write SetObjStatus;
  end;

  [Entity('CUSTOMERS_OS')]
  TCustomerVal = class(TCustomerOS)
  protected
    procedure OnBeforePersist;
    procedure Validate;
  end;

  [Entity('CUSTOMERS_VERSIONED')]
  TCustomerV = class(TCustomerOS)
  private
    FObjVersion: Integer;
    procedure SetObjVersion(const Value: Integer);
  public
    property ObjVersion: Integer read FObjVersion write SetObjVersion;
  end;

  [Entity('PHONES')]
  TPhone = class
  private
    FNumber: string;
    FModel: string;
    FID: Integer;
    FPersonID: Integer;
    FObjStatus: TdormObjectStatus;
    procedure SetNumber(const Value: string);
    procedure SetModel(const Value: string);
    procedure SetID(const Value: Integer);
    procedure SetPersonID(const Value: Integer);
    function GetIsItalianNumber: Boolean;
    procedure SetObjStatus(const Value: TdormObjectStatus);
  public
    constructor Create;
    property ID: Integer read FID write SetID;
    property Number: string read FNumber write SetNumber;
    property Model: string read FModel write SetModel;
    [Column('ID_PERSON')]
    property PersonID: Integer read FPersonID write SetPersonID;
    [NoAutomapping]
    property IsItalianNumber: Boolean read GetIsItalianNumber;
    [Transient]
    property ObjStatus: TdormObjectStatus read FObjStatus write SetObjStatus;
    function ToString: String; override;
  end;

  [ListOf('BObjectsU.TPhone')]
  TPhones = class(TObjectList<TPhone>)
  end;

  [Entity('PEOPLE')]
  TPerson = class
  private
    FLastName: string;
    FFirstName: string;
    FID: Integer;
    FBornDate: TDate;
    FPhones: TPhones;
    FPhoto: TStream;
    FIsMale: Boolean;
    FObjStatus: TdormObjectStatus;
    procedure SetLastName(const Value: string);
    procedure SetFirstName(const Value: string);
    procedure SetID(const Value: Integer);
    procedure SetBornDate(const Value: TDate);
    procedure SetPhoto(const Value: TStream);
    function GetFullName: string;
    procedure SetIsMale(const Value: Boolean);
    function GetAge: Int32;
    procedure SetObjStatus(const Value: TdormObjectStatus);

  public
    constructor Create; virtual;
    destructor Destroy; override;
    function ToString: string; override;
    class function NewPerson: TPerson;
    property ID: Integer read FID write SetID;
    [Column('FIRST_NAME')]
    property FirstName: string read FFirstName write SetFirstName;
    [Column('LAST_NAME')]
    property LastName: string read FLastName write SetLastName;
    // [Transient]
    // property Age: Int32 read GetAge write FAge;
    [Column('BORN_DATE')]
    property BornDate: TDate read FBornDate write SetBornDate;
    [HasMany('PersonID')]
    property Phones: TPhones read FPhones;

    // [Transient] {if you dont want save AGE}
    property Age: Integer read GetAge; // Saved but not retrieved

    property Photo: TStream read FPhoto write SetPhoto;
    [Column('IS_MALE')]
    property IsMale: Boolean read FIsMale write SetIsMale;
    [Transient]
    property ObjStatus: TdormObjectStatus read FObjStatus write SetObjStatus;
    [Transient]
    property FullName: string read GetFullName;
  end;

procedure CreateCustomers(Session: TSession; HowMany: Integer);

implementation

uses RandomUtilsU, System.SysUtils, System.DateUtils;

function IsValidEmail(const Value: string): Boolean;

  function CheckAllowed(const s: string): Boolean;
  var
    i: Integer;
  begin
    Result := false;
    for i := 1 to Length(s) do
      if not(CharInSet(s[i], ['a' .. 'z', 'A' .. 'Z', '0' .. '9', '_', '-',
        '.'])) then
        Exit;
    Result := True;
  end;

var
  i: Integer;
  NamePart, ServerPart: string;
begin
  Result := false;
  i := Pos('@', Value);
  if i = 0 then
    Exit;
  NamePart := Copy(Value, 1, i - 1);
  ServerPart := Copy(Value, i + 1, Length(Value));
  if (Length(NamePart) = 0) or ((Length(ServerPart) < 5)) then
    Exit;
  i := Pos('.', ServerPart);
  if (i = 0) or (i > (Length(ServerPart) - 2)) then
    Exit;
  Result := CheckAllowed(NamePart) and CheckAllowed(ServerPart);
end;

procedure CreateCustomers(Session: TSession; HowMany: Integer);
var
  Customer: TCustomerOS;
  i: Integer;
begin
  Session.DeleteAll(TCustomerOS);
  for i := 1 to HowMany do
  begin
    Customer := TCustomerOS.Create;
    Customer.Name := RandomFirstName + ' ' + RandomLastName;
    Customer.Address := RandomCountry + ' Street, ' +
      (1 + Random(1000)).ToString;
    Customer.EMail := RandomFirstName + '@' + RandomLastName + '.com';
    Customer.CreatedAt := Date - Random(365);
    Session.Persist(Customer);
    Customer.Free;
  end;
end;

{ TCustomer }

procedure TCustomer.SetADDRESS(const Value: String);
begin
  FADDRESS := Value;
end;

procedure TCustomer.SetCreatedAt(const Value: TDate);
begin
  FCreatedAt := Value;
end;

procedure TCustomer.SetEMail(const Value: String);
begin
  FEMail := Value;
end;

procedure TCustomer.SetID(const Value: Integer);
begin
  FID := Value;
end;

procedure TCustomer.SetName(const Value: String);
begin
  FName := Value;
end;

{ TCustomerOS }

procedure TCustomerOS.SetObjStatus(const Value: TdormObjectStatus);
begin
  FObjStatus := Value;
end;

{ TPerson }

constructor TPerson.Create;
begin
  inherited;
  FPhoto := nil;
  FPhones := TPhones.Create(True);
end;

destructor TPerson.Destroy;
begin
  FreeAndNil(FPhoto);
  FreeAndNil(FPhones);
  inherited;
end;

function TPerson.GetAge: Int32;
begin
  Result := YearsBetween(now, FBornDate);
end;

function TPerson.GetFullName: string;
begin
  Result := FirstName + ' ' + LastName;
end;

class function TPerson.NewPerson: TPerson;
begin
  Result := TPerson.Create;
  Result.FirstName := 'Daniele';
  Result.LastName := 'Teti';
  Result.BornDate := EncodeDate(1979, 11, 4);
end;

procedure TPerson.SetBornDate(const Value: TDate);
begin
  FBornDate := Value;
end;

procedure TPerson.SetFirstName(const Value: string);
begin
  FFirstName := Value;
end;

procedure TPerson.SetID(const Value: Integer);
begin
  FID := Value;
end;

procedure TPerson.SetIsMale(const Value: Boolean);
begin
  FIsMale := Value;
end;

procedure TPerson.SetLastName(const Value: string);
begin
  FLastName := Value;
end;

procedure TPerson.SetObjStatus(const Value: TdormObjectStatus);
begin
  FObjStatus := Value;
end;

procedure TPerson.SetPhoto(const Value: TStream);
begin

end;

function TPerson.ToString: string;
begin
  Result := Format('ID: %d, Nome: %s, Cognome: %s, Età: %d nato il %s',
    [Self.ID, Self.FirstName, Self.LastName, Self.Age,
    datetostr(Self.BornDate)]);
end;

{ TPhone }

constructor TPhone.Create;
begin
  inherited;
end;

function TPhone.GetIsItalianNumber: Boolean;
begin
  Result := (Copy(Self.Number, 1, 3) = '+39') or
    (Copy(Self.Number, 1, 4) = '0039');
end;

procedure TPhone.SetID(const Value: Integer);
begin
  FID := Value;
end;

procedure TPhone.SetModel(const Value: string);
begin
  FModel := Value;
  ObjStatus := osDirty;
end;

procedure TPhone.SetNumber(const Value: string);
begin
  FNumber := Value;
  ObjStatus := osDirty;
end;

procedure TPhone.SetObjStatus(const Value: TdormObjectStatus);
begin
  FObjStatus := Value;
end;

procedure TPhone.SetPersonID(const Value: Integer);
begin
  FPersonID := Value;
end;

function TPhone.ToString: String;
begin
  Result := Format(' %-15s (%s)', [Number, Model]);
end;

{ TCustomerV }

procedure TCustomerV.SetObjVersion(const Value: Integer);
begin
  FObjVersion := Value;
end;

{ TCustomerVal }

procedure TCustomerVal.OnBeforePersist;
begin
  FEMail := EMail.ToUpper;
end;

procedure TCustomerVal.Validate;
begin
  if not IsValidEmail(EMail) then
    raise Exception.Create('Email is not valid');
end;

end.
